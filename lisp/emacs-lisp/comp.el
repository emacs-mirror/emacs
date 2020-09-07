;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

;; Author: Andrea Corallo <akrl@sdf.com>

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Keywords: lisp
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This code is an attempt to make the pig fly.
;; Or, to put it another way to make a 911 out of a turbocharged VW Bug.

;;; Code:

(require 'bytecomp)
(require 'cl-extra)
(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'gv)
(require 'rx)
(require 'subr-x)
(require 'warnings)

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defcustom comp-speed 2
  "Compiler optimization level.  From -1 to 3.
- -1 functions are kept in bytecode form and no native compilation is performed.
- 0 native compilation is performed with no optimizations.
- 1 lite optimizations.
- 2 max optimization level fully adherent to the language semantic.
- 3 max optimization level, to be used only when necessary.
    Warning: the compiler is free to perform dangerous optimizations."
  :type 'number
  :group 'comp)

(defcustom comp-debug 0
  "Compiler debug level.  From 0 to 3.
This intended for debugging the compiler itself.
- 0 no debug facility.
    This is the recommended value unless you are debugging the compiler itself.
- 1 emit debug symbols and dump pseudo C code.
- 2 dump gcc passes and libgccjit log file.
- 3 dump libgccjit reproducers."
  :type 'number
  :group 'comp)

(defcustom comp-verbose 0
  "Compiler verbosity.  From 0 to 3.
This intended for debugging the compiler itself.
- 0 no logging.
- 1 final limple is logged.
- 2 LAP and final limple and some pass info are logged.
- 3 max verbosity."
  :type 'number
  :group 'comp)

(defcustom comp-always-compile nil
  "Unconditionally (re-)compile all files."
  :type 'boolean
  :group 'comp)

(defcustom comp-deferred-compilation-black-list
  '()
  "List of regexps to exclude files from deferred native compilation.
Skip if any is matching."
  :type 'list
  :group 'comp)

(defcustom comp-bootstrap-black-list
  '()
  "List of regexps to exclude files from native compilation during bootstrap.
Skip if any is matching."
  :type 'list
  :group 'comp)

(defcustom comp-never-optimize-functions
  '(;; Mandatory for Emacs to be working correctly
    macroexpand scroll-down scroll-up narrow-to-region widen rename-buffer
    make-indirect-buffer delete-file top-level abort-recursive-edit
    ;; For user convenience
    yes-or-no-p
    ;; Make the Evil happy :/
    read-key-sequence select-window set-window-buffer split-window-internal
    use-global-map use-local-map)
  "Primitive functions for which we do not perform trampoline optimization.
This is especially useful for primitives known to be advised or
redefined when compilation is performed at `comp-speed' > 0."
  :type 'list
  :group 'comp)

(defcustom comp-async-jobs-number 0
  "Default number of processes used for async compilation.
When zero use half of the CPUs or at least one."
  :type 'number
  :group 'comp)

(defcustom comp-async-cu-done-hook nil
  "This hook is run whenever an asyncronous native compilation
finishes compiling a single compilation unit.
The argument FILE passed to the function is the filename used as
compilation input."
  :type 'hook
  :group 'comp)

(defcustom comp-async-all-done-hook nil
  "This hook is run whenever the asyncronous native compilation
finishes compiling all input files."
  :type 'hook
  :group 'comp)

(defcustom comp-async-env-modifier-form nil
  "Form to be evaluated by each asyncronous compilation worker
before compilation.  Usable to modify the compiler environment."
  :type 'list
  :group 'comp)

(defcustom comp-native-driver-options nil
  "Options passed verbatim to the native compiler's backend driver.
Note that not all options are meaningful; typically only the options
affecting the assembler and linker are likely to be useful.

Passing these options is only available in libgccjit version 9
and above."
  :type 'list
  :group 'comp)

(defvar comp-dry-run nil
  "When non nil run everything but the C back-end.")

(defconst comp-valid-source-re (rx ".el" (? ".gz") eos)
  "Regexp to match filename of valid input source files.")

(defconst comp-log-buffer-name "*Native-compile-Log*"
  "Name of the native-compiler log buffer.")

(defconst comp-async-buffer-name "*Async-native-compile-log*"
  "Name of the async compilation buffer log.")

(defvar comp-native-compiling nil
  "This gets bound to t while native compilation.
Can be used by code that wants to expand differently in this case.")

(defvar comp-pass nil
  "Every pass has the right to bind what it likes here.")

(defvar comp-curr-allocation-class 'd-default
  "Current allocation class.
Can be one of: 'd-default', 'd-impure' or 'd-ephemeral'.  See `comp-ctxt'.")

(defconst comp-passes '(comp-spill-lap
                        comp-limplify
                        comp-fwprop
                        comp-call-optim
                        comp-ipa-pure
                        comp-fwprop
                        comp-dead-code
                        comp-tco
                        comp-fwprop
                        comp-remove-type-hints
                        comp-final)
  "Passes to be executed in order.")

(defvar comp-disabled-passes '()
  "List of disabled passes.
For internal use only by the testsuite.")

(defvar comp-post-pass-hooks ()
  "Alist PASS FUNCTIONS.
Each function in FUNCTIONS is run after PASS.
Useful to hook into pass checkers.")

(defconst comp-known-ret-types '((cons . cons)
                                 (1+ . number)
                                 (1- . number)
                                 (+ . number)
                                 (- . number)
                                 (* . number)
                                 (/ . number)
                                 (% . number)
                                 ;; Type hints
                                 (comp-hint-fixnum . fixnum)
                                 (comp-hint-cons . cons))
  "Alist used for type propagation.")

(defconst comp-symbol-values-optimizable '(most-positive-fixnum
                                           most-negative-fixnum)
  "Symbol values we can resolve in the compile-time.")

(defconst comp-type-hints '(comp-hint-fixnum
                            comp-hint-cons)
  "List of fake functions used to give compiler hints.")

(defconst comp-limple-sets '(set
                             setimm
                             set-par-to-local
                             set-args-to-local
                             set-rest-args-to-local)
  "Limple set operators.")

(defconst comp-limple-assignments `(fetch-handler
                                    ,@comp-limple-sets)
  "Limple operators that clobbers the first m-var argument.")

(defconst comp-limple-calls '(call
                              callref
                              direct-call
                              direct-callref)
  "Limple operators use to call subrs.")

(define-error 'native-compiler-error-dyn-func
  "can't native compile a non lexical scoped function"
  'native-compiler-error)
(define-error 'native-compiler-error-empty-byte
  "empty byte compiler output"
  'native-compiler-error)

(eval-when-compile
  (defconst comp-op-stack-info
    (cl-loop with h = (make-hash-table)
	     for k across byte-code-vector
	     for v across byte-stack+-info
	     when k
	     do (puthash k v h)
	     finally return h)
    "Hash table lap-op -> stack adjustment."))

(cl-defstruct comp-data-container
  "Data relocation container structure."
  (l () :type list
     :documentation "Constant objects used by functions.")
  (idx (make-hash-table :test #'equal) :type hash-table
       :documentation "Obj -> position into the previous field."))

(cl-defstruct comp-ctxt
  "Lisp side of the compiler context."
  (output nil :type string
          :documentation "Target output file-name for the compilation.")
  (top-level-forms () :type list
                   :documentation "List of spilled top level forms.")
  (funcs-h (make-hash-table :test #'equal) :type hash-table
           :documentation "c-name -> comp-func.")
  (sym-to-c-name-h (make-hash-table :test #'eq) :type hash-table
                   :documentation "symbol-function -> c-name.
This is only for optimizing intra CU calls at speed 3.")
  (byte-func-to-func-h (make-hash-table :test #'equal) :type hash-table
                     :documentation "byte-function -> comp-func.
Needed to replace immediate byte-compiled lambdas with the compiled reference.")
  (lambda-fixups-h (make-hash-table :test #'equal) :type hash-table
                   :documentation  "Hash table byte-func -> mvar to fixup.")
  (function-docs (make-hash-table :test #'eql) :type (or hash-table vector)
               :documentation "Documentation index -> documentation")
  (d-default (make-comp-data-container) :type comp-data-container
             :documentation "Standard data relocated in use by functions.")
  (d-impure (make-comp-data-container) :type comp-data-container
            :documentation "Relocated data that cannot be moved into pure space.
This is tipically for top-level forms other than defun.")
  (d-ephemeral (make-comp-data-container) :type comp-data-container
               :documentation "Relocated data not necessary after load.")
  (with-late-load nil :type boolean
                  :documentation "When non nil support late load."))

(cl-defstruct comp-args-base
  (min nil :type number
       :documentation "Minimum number of arguments allowed."))

(cl-defstruct (comp-args (:include comp-args-base))
  (max nil :type number
       :documentation "Maximum number of arguments allowed.
To be used when ncall-conv is nil."))

(cl-defstruct (comp-nargs (:include comp-args-base))
  "Describe args when the function signature is of kind:
(ptrdiff_t nargs, Lisp_Object *args)."
  (nonrest nil :type number
           :documentation "Number of non rest arguments.")
  (rest nil :type boolean
        :documentation "t if rest argument is present."))

(cl-defstruct (comp-block (:copier nil)
                          (:constructor nil))
  "A base class for basic blocks."
  (name nil :type symbol)
  (insns () :type list
         :documentation "List of instructions.")
  (closed nil :type boolean
          :documentation "t if closed.")
  ;; All the followings are for SSA and CGF analysis.
  ;; Keep in sync with `comp-clean-ssa'!!
  (in-edges () :type list
            :documentation "List of incoming edges.")
  (out-edges () :type list
             :documentation "List of out-coming edges.")
  (dom nil :type comp-block
        :documentation "Immediate dominator.")
  (df (make-hash-table) :type hash-table
      :documentation "Dominance frontier set. Block-name -> block")
  (post-num nil :type number
            :documentation "Post order number.")
  (final-frame nil :type vector
             :documentation "This is a copy of the frame when leaving the block.
Is in use to help the SSA rename pass."))

(cl-defstruct (comp-block-lap (:copier nil)
                              (:include comp-block)
                              (:constructor make--comp-block-lap
                                            (addr sp name))) ; Positional
  "A basic block created from lap."
  ;; These two slots are used during limplification.
  (sp nil :type number
      :documentation "When non nil indicates the sp value while entering
into it.")
  (addr nil :type number
        :documentation "Start block LAP address."))

(cl-defstruct (comp-latch (:copier nil)
                          (:include comp-block))
  "A basic block for a latch loop.")

(cl-defstruct (comp-edge (:copier nil) (:constructor make--comp-edge))
  "An edge connecting two basic blocks."
  (src nil :type comp-block)
  (dst nil :type comp-block)
  (number nil :type number
          :documentation "The index number corresponding to this edge in the
 edge vector."))

(defun comp-block-preds (basic-block)
  "Given BASIC-BLOCK return the list of its predecessors."
  (mapcar #'comp-edge-src (comp-block-in-edges basic-block)))

(defun comp-gen-counter ()
  "Return a sequential number generator."
  (let ((n -1))
    (lambda ()
      (cl-incf n))))

(cl-defstruct (comp-func (:copier nil))
  "LIMPLE representation of a function."
  (name nil :type symbol
        :documentation "Function symbol name. Nil indicates anonymous.")
  (c-name nil :type string
          :documentation "The function name in the native world.")
  (byte-func nil
             :documentation "Byte compiled version.")
  (doc nil :type string
       :documentation "Doc string.")
  (int-spec nil :type list
            :documentation "Interactive form.")
  (lap () :type list
       :documentation "LAP assembly representation.")
  (ssa-status nil :type symbol
       :documentation "SSA status either: 'nil', 'dirty' or 't'.
Once in SSA form this *must* be set to 'dirty' every time the topology of the
CFG is mutated by a pass.")
  (frame-size nil :type number)
  (blocks (make-hash-table) :type hash-table
          :documentation "Key is the basic block symbol value is a comp-block
structure.")
  (lap-block (make-hash-table :test #'equal) :type hash-table
             :documentation "LAP lable -> LIMPLE basic block name.")
  (edges () :type list
         :documentation "List of edges connecting basic blocks.")
  (block-cnt-gen (funcall #'comp-gen-counter) :type function
                 :documentation "Generates block numbers.")
  (edge-cnt-gen (funcall #'comp-gen-counter) :type function
                :documentation "Generates edges numbers.")
  (has-non-local nil :type boolean
                 :documentation "t if non local jumps are present.")
  (array-h (make-hash-table) :type hash-table
           :documentation "array idx -> array length.")
  (speed nil :type number
         :documentation "Optimization level (see `comp-speed').")
  (pure nil :type boolean
        :documentation "t if pure nil otherwise."))

(cl-defstruct (comp-func-l (:include comp-func))
  "Lexical scoped function."
  (args nil :type comp-args-base
        :documentation "Argument specification of the function"))

(cl-defstruct (comp-func-d (:include comp-func))
  "Dynamic scoped function."
  (lambda-list nil :type list
        :documentation "Original lambda-list."))

(cl-defstruct (comp-mvar (:constructor make--comp-mvar))
  "A meta-variable being a slot in the meta-stack."
  (id nil :type (or null number)
      :documentation "Unique id when in SSA form.")
  (slot nil :type (or fixnum symbol)
        :documentation "Slot number in the array if a number or
        'scratch' for scratch slot.")
  (const-vld nil :type boolean
             :documentation "Valid signal for the following slot.")
  (constant nil
            :documentation "When const-vld non nil this is used for holding
 a value known at compile time.")
  (type nil :type symbol
        :documentation "When non nil indicates the type when known at compile
 time."))

;; Special vars used by some passes
(defvar comp-func)



(defun comp-ensure-native-compiler ()
  "Make sure Emacs has native compiler support and libgccjit is laodable.
Raise and error otherwise.
To be used by all entry points."
  (cond
   ((null (boundp 'comp-ctxt))
    (error "Emacs not compiled with native compiler support (--with-nativecomp)"))
   ((null (native-comp-available-p))
    (error "Cannot find libgccjit"))))

(defsubst comp-set-op-p (op)
  "Assignment predicate for OP."
  (when (memq op comp-limple-sets) t))

(defsubst comp-assign-op-p (op)
  "Assignment predicate for OP."
  (when (memq op comp-limple-assignments) t))

(defsubst comp-call-op-p (op)
  "Call predicate for OP."
  (when (memq op comp-limple-calls) t))

(defsubst comp-limple-insn-call-p (insn)
  "Limple INSN call predicate."
  (comp-call-op-p (car-safe insn)))

(defsubst comp-type-hint-p (func)
  "Type hint predicate for function name FUNC."
  (when (memq func comp-type-hints) t))

(defun comp-func-unique-in-cu-p (func)
  "Return t if FUNC is know to be unique in the current compilation unit."
  (if (symbolp func)
      (cl-loop with h = (make-hash-table :test #'eq)
               for f being the hash-value in (comp-ctxt-funcs-h comp-ctxt)
               for name = (comp-func-name f)
               when (gethash name h)
                 return nil
               do (puthash name t h)
               finally return t)
    t))

(defsubst comp-symbol-func-to-fun (symbol-funcion)
  "Given a function called SYMBOL-FUNCION return its `comp-func'."
  (gethash (gethash symbol-funcion (comp-ctxt-sym-to-c-name-h
                                    comp-ctxt))
           (comp-ctxt-funcs-h comp-ctxt)))

(defsubst comp-function-pure-p (f)
  "Return t if F is pure."
  (or (get f 'pure)
      (when-let ((func (comp-symbol-func-to-fun f)))
        (comp-func-pure func))))

(defsubst comp-alloc-class-to-container (alloc-class)
  "Given ALLOC-CLASS return the data container for the current context.
Assume allocaiton class 'd-default as default."
  (cl-struct-slot-value 'comp-ctxt (or alloc-class 'd-default) comp-ctxt))

(defsubst comp-add-const-to-relocs (obj)
  "Keep track of OBJ into the ctxt relocations."
  (puthash obj t (comp-data-container-idx (comp-alloc-class-to-container
                                           comp-curr-allocation-class))))


;;; Log rountines.

(defconst comp-limple-lock-keywords
  `((,(rx bol "(comment" (1+ not-newline)) . font-lock-comment-face)
    (,(rx "#s(" (group-n 1 "comp-mvar"))
     (1 font-lock-function-name-face))
    (,(rx bol "(" (group-n 1 "phi"))
     (1 font-lock-variable-name-face))
    (,(rx (group-n 1 (or "entry"
                         (seq (or "entry_" "entry_fallback_" "bb_")
                              (1+ num)))))
     (1 font-lock-constant-face))
    (,(rx "(" (group-n 1 (1+ (or word "-"))))
     (1 font-lock-keyword-face)))
  "Highlights used by comp-limple-mode.")

(define-derived-mode comp-limple-mode fundamental-mode "LIMPLE"
  "Syntax highlight LIMPLE IR."
  (setf font-lock-defaults '(comp-limple-lock-keywords)))

(cl-defun comp-log (data &optional (level 1))
  "Log DATA at LEVEL.
LEVEL is a number from 1-3; if it is less than `comp-verbose', do
nothing.  If `noninteractive', log with `message'.  Otherwise,
log with `comp-log-to-buffer'."
  (when (>= comp-verbose level)
    (if noninteractive
        (cl-typecase data
          (atom (message "%s" data))
          (t (dolist (elem data)
               (message "%s" elem))))
      (comp-log-to-buffer data))))

(cl-defun comp-log-to-buffer (data)
  "Log DATA to `comp-log-buffer-name'."
  (let* ((log-buffer
          (or (get-buffer comp-log-buffer-name)
              (with-current-buffer (get-buffer-create comp-log-buffer-name)
                (setf buffer-read-only t)
                (current-buffer))))
         (log-window (get-buffer-window log-buffer))
         (inhibit-read-only t)
         at-end-p)
    (with-current-buffer log-buffer
      (unless (eq major-mode 'comp-limple-mode)
        (comp-limple-mode))
      (when (= (point) (point-max))
        (setf at-end-p t))
      (save-excursion
        (goto-char (point-max))
        (cl-typecase data
          (atom (princ data log-buffer))
          (t (dolist (elem data)
               (princ elem log-buffer)
               (insert "\n"))))
        (insert "\n"))
      (when (and at-end-p log-window)
        ;; When log window's point is at the end, follow the tail.
        (with-selected-window log-window
          (goto-char (point-max)))))))

(defun comp-log-func (func verbosity)
  "Log function FUNC.
VERBOSITY is a number between 0 and 3."
  (when (>= comp-verbose verbosity)
    (comp-log (format "\nFunction: %s\n" (comp-func-name func)) verbosity)
    (cl-loop for block-name being each hash-keys of (comp-func-blocks func)
             using (hash-value bb)
             do (comp-log (concat "<" (symbol-name block-name) ">") verbosity)
                (comp-log (comp-block-insns bb) verbosity))))

(defun comp-log-edges (func)
  "Log edges in FUNC."
  (let ((edges (comp-func-edges func)))
    (comp-log (format "\nEdges in function: %s\n"
                      (comp-func-name func))
              2)
    (mapc (lambda (e)
            (comp-log (format "n: %d src: %s dst: %s\n"
                              (comp-edge-number e)
                              (comp-block-name (comp-edge-src e))
                              (comp-block-name (comp-edge-dst e)))
                      2))
          edges)))



(defmacro comp-loop-insn-in-block (basic-block &rest body)
  "Loop over all insns in BASIC-BLOCK executning BODY.
Inside BODY `insn' can be used to read or set the current
instruction."
  (declare (debug (form body))
           (indent defun))
  (let ((sym-cell (gensym "cell-")))
    `(cl-symbol-macrolet ((insn (car ,sym-cell)))
       (cl-loop for ,sym-cell on (comp-block-insns ,basic-block)
	        do ,@body))))

;;; spill-lap pass specific code.

(defsubst comp-lex-byte-func-p (f)
  "Return t if F is a lexical scoped byte compiled function."
  (and (byte-code-function-p f)
       (fixnump (aref f 0))))

(defun comp-spill-decl-spec (function-name spec)
  "Return the declared specifier SPEC for FUNCTION-NAME."
  (plist-get (cdr (assq function-name byte-to-native-plist-environment))
             spec))

(defun comp-spill-speed (function-name)
  "Return the speed for FUNCTION-NAME."
  (or (comp-spill-decl-spec function-name 'speed)
      comp-speed))

(defun comp-c-func-name (name prefix &optional first)
  "Given NAME return a name suitable for the native code.
Add PREFIX in front of it.  If FIRST is not nil pick the first
available name ignoring compilation context and potential name
clashes."
  ;; Unfortunatelly not all symbol names are valid as C function names...
  ;; Nassi's algorithm here:
  (let* ((orig-name (if (symbolp name) (symbol-name name) name))
         (crypted (cl-loop with str = (make-string (* 2 (length orig-name)) 0)
	                   for j from 0 by 2
	                   for i across orig-name
	                   for byte = (format "%x" i)
	                   do (aset str j (aref byte 0))
	                      (aset str (1+ j) (aref byte 1))
	                   finally return str))
         (human-readable (replace-regexp-in-string
                          "-" "_" orig-name))
         (human-readable (replace-regexp-in-string
                          (rx (not (any "0-9a-z_"))) "" human-readable)))
    (if (null first)
        ;; Prevent C namespace conflicts.
        (cl-loop
         with h = (comp-ctxt-funcs-h comp-ctxt)
         for i from 0
         for c-sym = (concat prefix crypted "_" human-readable "_"
                             (number-to-string i))
         unless (gethash c-sym h)
         return c-sym)
      ;; When called out of a compilation context (ex disassembling)
      ;; pick the first one.
      (concat prefix crypted "_" human-readable "_0"))))

(defun comp-decrypt-arg-list (x function-name)
  "Decript argument list X for FUNCTION-NAME."
  (unless (fixnump x)
    (signal 'native-compiler-error-dyn-func function-name))
  (let ((rest (not (= (logand x 128) 0)))
        (mandatory (logand x 127))
        (nonrest (ash x -8)))
    (if (and (null rest)
             (< nonrest 9)) ;; SUBR_MAX_ARGS
        (make-comp-args :min mandatory
                        :max nonrest)
      (make-comp-nargs :min mandatory
                       :nonrest nonrest
                       :rest rest))))

(defsubst comp-byte-frame-size (byte-compiled-func)
  "Given BYTE-COMPILED-FUNC return the frame size to be allocated."
  (aref byte-compiled-func 3))

(defun comp-add-func-to-ctxt (func)
  "Add FUNC to the current compiler contex."
  (let ((name (comp-func-name func))
        (c-name (comp-func-c-name func)))
    (puthash name c-name (comp-ctxt-sym-to-c-name-h comp-ctxt))
    (puthash c-name func (comp-ctxt-funcs-h comp-ctxt))))

(cl-defgeneric comp-spill-lap-function (input)
  "Byte compile INPUT and spill lap for further stages.")

(cl-defgeneric comp-spill-lap-function ((function-name symbol))
  "Byte compile FUNCTION-NAME spilling data from the byte compiler."
  (let* ((f (symbol-function function-name))
         (c-name (comp-c-func-name function-name "F"))
         (func (make-comp-func-l :name function-name
                                 :c-name c-name
                                 :doc (documentation f t)
                                 :int-spec (interactive-form f)
                                 :speed (comp-spill-speed function-name)
                                 :pure (comp-spill-decl-spec function-name
                                                             'pure))))
      (when (byte-code-function-p f)
        (signal 'native-compiler-error
                "can't native compile an already bytecompiled function"))
      (setf (comp-func-byte-func func)
            (byte-compile (comp-func-name func)))
      (let ((lap (byte-to-native-lambda-lap
                  (gethash (aref (comp-func-byte-func func) 1)
                           byte-to-native-lambdas-h))))
        (cl-assert lap)
        (comp-log lap 2)
        (let ((arg-list (aref (comp-func-byte-func func) 0)))
          (setf (comp-func-l-args func)
                (comp-decrypt-arg-list arg-list function-name)
                (comp-func-lap func)
                lap
                (comp-func-frame-size func)
                (comp-byte-frame-size (comp-func-byte-func func))))
        (setf (comp-ctxt-top-level-forms comp-ctxt)
              (list (make-byte-to-native-func-def :name function-name
                                                  :c-name c-name)))
        ;; Create the default array.
        (puthash 0 (comp-func-frame-size func) (comp-func-array-h func))
        (comp-add-func-to-ctxt func))))

(defun comp-intern-func-in-ctxt (_ obj)
  "Given OBJ of type `byte-to-native-lambda' create a function in `comp-ctxt'."
  (when-let ((byte-func (byte-to-native-lambda-byte-func obj)))
    (let* ((lap (byte-to-native-lambda-lap obj))
           (top-l-form (cl-loop
                        for form in (comp-ctxt-top-level-forms comp-ctxt)
                        when (and (byte-to-native-func-def-p form)
                                  (eq (byte-to-native-func-def-byte-func form)
                                      byte-func))
                        return form))
           (name (when top-l-form
                   (byte-to-native-func-def-name top-l-form)))
           (c-name (comp-c-func-name (or name "anonymous-lambda") "F"))
           (func (if (comp-lex-byte-func-p byte-func)
                     (make-comp-func-l
                      :args (comp-decrypt-arg-list (aref byte-func 0)
                                                   name))
                   (make-comp-func-d :lambda-list (aref byte-func 0)))))
      (setf (comp-func-name func) name
            (comp-func-byte-func func) byte-func
            (comp-func-doc func) (documentation byte-func t)
            (comp-func-int-spec func) (interactive-form byte-func)
            (comp-func-c-name func) c-name
            (comp-func-lap func) lap
            (comp-func-frame-size func) (comp-byte-frame-size byte-func)
            (comp-func-speed func) (comp-spill-speed name)
            (comp-func-pure func) (comp-spill-decl-spec name 'pure))

      ;; Store the c-name to have it retrivable from
      ;; `comp-ctxt-top-level-forms'.
      (when top-l-form
        (setf (byte-to-native-func-def-c-name top-l-form) c-name))
      (unless name
        (puthash byte-func func (comp-ctxt-byte-func-to-func-h comp-ctxt)))
      ;; Create the default array.
      (puthash 0 (comp-func-frame-size func) (comp-func-array-h func))
      (comp-add-func-to-ctxt func)
      (comp-log (format "Function %s:\n" name) 1)
      (comp-log lap 1))))

(cl-defgeneric comp-spill-lap-function ((filename string))
  "Byte compile FILENAME spilling data from the byte compiler."
  (byte-compile-file filename)
  (unless byte-to-native-top-level-forms
    (signal 'native-compiler-error-empty-byte filename))
  (setf (comp-ctxt-top-level-forms comp-ctxt)
        (cl-loop
         for form in (reverse byte-to-native-top-level-forms)
         collect
         (if (and (byte-to-native-func-def-p form)
                  (eq -1
                      (comp-spill-speed (byte-to-native-func-def-name form))))
             (let ((byte-code (byte-to-native-func-def-byte-func form)))
               (remhash byte-code byte-to-native-lambdas-h)
               (make-byte-to-native-top-level
                :form `(defalias
                         ',(byte-to-native-func-def-name form)
                         ,byte-code
                         nil)
                :lexical (comp-lex-byte-func-p byte-code)))
           form)))
  (maphash #'comp-intern-func-in-ctxt byte-to-native-lambdas-h))

(defun comp-spill-lap (input)
  "Byte compile and spill the LAP representation for INPUT.
If INPUT is a symbol this is the function-name to be compiled.
If INPUT is a string this is the file path to be compiled."
  (let ((byte-native-compiling t)
        (byte-to-native-lambdas-h (make-hash-table :test #'eq))
        (byte-to-native-top-level-forms ())
        (byte-to-native-plist-environment ()))
    (comp-spill-lap-function input)))


;;; Limplification pass specific code.

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during function limplification."
  (frame nil :type vector
         :documentation "Meta-stack used to flat LAP.")
  (curr-block nil :type comp-block
              :documentation "Current block being limplified.")
  (sp -1 :type number
      :documentation "Current stack pointer while walking LAP.
Points to the next slot to be filled.")
  (pc 0 :type number
      :documentation "Current program counter while walking LAP.")
  (label-to-addr nil :type hash-table
                 :documentation "LAP hash table -> address.")
  (pending-blocks () :type list
                  :documentation "List of blocks waiting for limplification."))

(defconst comp-lap-eob-ops
  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
              byte-goto-if-not-nil-else-pop byte-return byte-pushcatch
              byte-switch byte-pushconditioncase)
  "LAP end of basic blocks op codes.")

(defsubst comp-lap-eob-p (inst)
  "Return t if INST closes the current basic blocks, nil otherwise."
  (when (memq (car inst) comp-lap-eob-ops)
    t))

(defsubst comp-lap-fall-through-p (inst)
  "Return t if INST fall through, nil otherwise."
  (when (not (memq (car inst) '(byte-goto byte-return)))
    t))

(defsubst comp-sp ()
  "Current stack pointer."
  (comp-limplify-sp comp-pass))
(gv-define-setter comp-sp (value)
  `(setf (comp-limplify-sp comp-pass) ,value))

(defmacro comp-with-sp (sp &rest body)
  "Execute BODY setting the stack pointer to SP.
Restore the original value afterwards."
  (declare (debug (form body))
           (indent defun))
  (let ((sym (gensym)))
    `(let ((,sym (comp-sp)))
       (setf (comp-sp) ,sp)
       (progn ,@body)
       (setf (comp-sp) ,sym))))

(defsubst comp-slot-n (n)
  "Slot N into the meta-stack."
  (aref (comp-limplify-frame comp-pass) n))

(defsubst comp-slot ()
  "Current slot into the meta-stack pointed by sp."
  (comp-slot-n (comp-sp)))

(defsubst comp-slot+1 ()
  "Slot into the meta-stack pointed by sp + 1."
  (comp-slot-n (1+ (comp-sp))))

(defsubst comp-label-to-addr (label)
  "Find the address of LABEL."
  (or (gethash label (comp-limplify-label-to-addr comp-pass))
      (signal 'native-ice (list "label not found" label))))

(defsubst comp-mark-curr-bb-closed ()
  "Mark the current basic block as closed."
  (setf (comp-block-closed (comp-limplify-curr-block comp-pass)) t))

(defun comp-bb-maybe-add (lap-addr &optional sp)
  "If necessary create a pending basic block for LAP-ADDR with stack depth SP.
The basic block is returned regardless it was already declared or not."
  (let ((bb (or (cl-loop  ; See if the block was already limplified.
                 for bb being the hash-value in (comp-func-blocks comp-func)
                 when (and (comp-block-lap-p bb)
                           (equal (comp-block-lap-addr bb) lap-addr))
                   return bb)
                (cl-find-if (lambda (bb) ; Look within the pendings blocks.
                              (and (comp-block-lap-p bb)
                                   (= (comp-block-lap-addr bb) lap-addr)))
                            (comp-limplify-pending-blocks comp-pass)))))
    (if bb
        (progn
          (unless (or (null sp) (= sp (comp-block-lap-sp bb)))
            (signal 'native-ice (list "incoherent stack pointers"
                                      sp (comp-block-lap-sp bb))))
          bb)
      (car (push (make--comp-block-lap lap-addr sp (comp-new-block-sym))
                 (comp-limplify-pending-blocks comp-pass))))))

(defsubst comp-call (func &rest args)
  "Emit a call for function FUNC with ARGS."
  `(call ,func ,@args))

(defun comp-callref (func nargs stack-off)
  "Emit a call using narg abi for FUNC.
NARGS is the number of arguments.
STACK-OFF is the index of the first slot frame involved."
  `(callref ,func ,@(cl-loop repeat nargs
                             for sp from stack-off
                             collect (comp-slot-n sp))))

(cl-defun make-comp-mvar (&key slot (constant nil const-vld) type)
  (when const-vld
    (comp-add-const-to-relocs constant))
  (make--comp-mvar :slot slot :const-vld const-vld :constant constant
                   :type type))

(defun comp-new-frame (size &optional ssa)
  "Return a clean frame of meta variables of size SIZE.
If SSA non nil populate it of m-var in ssa form."
  (cl-loop with v = (make-vector size nil)
           for i below size
           for mvar = (if ssa
                          (make-comp-ssa-mvar :slot i)
                        (make-comp-mvar :slot i))
           do (aset v i mvar)
           finally return v))

(defsubst comp-emit (insn)
  "Emit INSN into basic block BB."
  (let ((bb (comp-limplify-curr-block comp-pass)))
    (cl-assert (not (comp-block-closed bb)))
    (push insn (comp-block-insns bb))))

(defsubst comp-emit-set-call (call)
  "Emit CALL assigning the result the the current slot frame.
If the callee function is known to have a return type propagate it."
  (cl-assert call)
  (comp-emit (list 'set (comp-slot) call)))

(defun comp-copy-slot (src-n &optional dst-n)
  "Set slot number DST-N to slot number SRC-N as source.
If DST-N is specified use it otherwise assume it to be the current slot."
  (comp-with-sp (or dst-n (comp-sp))
    (let ((src-slot (comp-slot-n src-n)))
      (cl-assert src-slot)
      (comp-emit `(set ,(comp-slot) ,src-slot)))))

(defsubst comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defsubst comp-emit-setimm (val)
  "Set constant VAL to current slot."
  (comp-add-const-to-relocs val)
  ;; Leave relocation index nil on purpose, will be fixed-up in final
  ;; by `comp-finalize-relocs'.
  (comp-emit `(setimm ,(comp-slot) ,val)))

(defun comp-make-curr-block (block-name entry-sp &optional addr)
  "Create a basic block with BLOCK-NAME and set it as current block.
ENTRY-SP is the sp value when entering.
The block is added to the current function.
The block is returned."
  (let ((bb (make--comp-block-lap addr entry-sp block-name)))
    (setf (comp-limplify-curr-block comp-pass) bb
          (comp-limplify-pc comp-pass) addr
          (comp-limplify-sp comp-pass) (when (comp-block-lap-p bb)
                                         (comp-block-lap-sp bb)))
    (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
    bb))

(defun comp-latch-make-fill (target)
  "Create a latch pointing to TARGET and fill it.
Return the created latch"
  (let ((latch (make-comp-latch :name (comp-new-block-sym "latch")))
        (curr-bb (comp-limplify-curr-block comp-pass)))
    ;; See `comp-make-curr-block'.
    (setf (comp-limplify-curr-block comp-pass) latch)
    (when (< (comp-func-speed comp-func) 3)
      ;; At speed 3 the programmer is responsible to manually
      ;; place `comp-maybe-gc-or-quit'.
      (comp-emit '(call comp-maybe-gc-or-quit)))
    ;; See `comp-emit-uncond-jump'.
    (comp-emit `(jump ,(comp-block-name target)))
    (comp-mark-curr-bb-closed)
    (puthash (comp-block-name latch) latch (comp-func-blocks comp-func))
    (setf (comp-limplify-curr-block comp-pass) curr-bb)
    latch))

(defun comp-emit-uncond-jump (lap-label)
  "Emit an unconditional branch to LAP-LABEL."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (when stack-depth
      (cl-assert (= (1- stack-depth) (comp-sp))))
    (let* ((target-addr (comp-label-to-addr label-num))
           (target (comp-bb-maybe-add target-addr
                                      (comp-sp)))
           (latch (when (< target-addr (comp-limplify-pc comp-pass))
                    (comp-latch-make-fill target)))
           (eff-target-name (comp-block-name (or latch target))))
      (comp-emit `(jump ,eff-target-name))
      (comp-mark-curr-bb-closed))))

(defun comp-emit-cond-jump (a b target-offset lap-label negated)
  "Emit a conditional jump to LAP-LABEL when A and B satisfy EQ.
TARGET-OFFSET is the positive offset on the SP when branching to the target
block.
If NEGATED non null negate the tested condition.
Return value is the fall through block name."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (let* ((bb (comp-block-name (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                                   (comp-sp)))) ; Fall through block.
           (target-sp (+ target-offset (comp-sp)))
           (target-addr (comp-label-to-addr label-num))
           (target (comp-bb-maybe-add target-addr target-sp))
           (latch (when (< target-addr (comp-limplify-pc comp-pass))
                    (comp-latch-make-fill target)))
           (eff-target-name (comp-block-name (or latch target))))
      (when label-sp
        (cl-assert (= (1- label-sp) (+ target-offset (comp-sp)))))
      (comp-emit (if negated
		     (list 'cond-jump a b eff-target-name bb)
		   (list 'cond-jump a b bb eff-target-name)))
      (comp-mark-curr-bb-closed)
      bb)))

(defun comp-emit-handler (lap-label handler-type)
  "Emit a non local exit handler to LAP-LABEL of type HANDLER-TYPE."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (cl-assert (= (- label-sp 2) (comp-sp)))
    (setf (comp-func-has-non-local comp-func) t)
    (let* ((guarded-bb (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                          (comp-sp)))
           (handler-bb (comp-bb-maybe-add (comp-label-to-addr label-num)
                                          (1+ (comp-sp))))
           (pop-bb (make--comp-block-lap nil (comp-sp) (comp-new-block-sym))))
      (comp-emit (list 'push-handler
                       handler-type
                       (comp-slot+1)
                       (comp-block-name pop-bb)
                       (comp-block-name guarded-bb)))
      (comp-mark-curr-bb-closed)
      ;; Emit the basic block to pop the handler if we got the non local.
      (puthash (comp-block-name pop-bb) pop-bb (comp-func-blocks comp-func))
      (setf (comp-limplify-curr-block comp-pass) pop-bb)
      (comp-emit `(fetch-handler ,(comp-slot+1)))
      (comp-emit `(jump ,(comp-block-name handler-bb)))
      (comp-mark-curr-bb-closed))))

(defun comp-limplify-listn (n)
  "Limplify list N."
  (comp-with-sp (+ (comp-sp) n -1)
    (comp-emit-set-call (comp-call 'cons
                                   (comp-slot)
                                   (make-comp-mvar :constant nil))))
  (cl-loop for sp from (+ (comp-sp) n -2) downto (comp-sp)
           do (comp-with-sp sp
                (comp-emit-set-call (comp-call 'cons
                                               (comp-slot)
                                               (comp-slot+1))))))

(defun comp-new-block-sym (&optional postfix)
  "Return a unique symbol postfixing POSTFIX naming the next new basic block."
  (intern (format (if postfix "bb_%s_%s" "bb_%s")
                  (funcall (comp-func-block-cnt-gen comp-func))
                  postfix)))

(defun comp-fill-label-h ()
  "Fill label-to-addr hash table for the current function."
  (setf (comp-limplify-label-to-addr comp-pass) (make-hash-table :test 'eql))
  (cl-loop for insn in (comp-func-lap comp-func)
           for addr from 0
           do (pcase insn
                (`(TAG ,label . ,_)
                 (puthash label addr (comp-limplify-label-to-addr comp-pass))))))

(defun comp-jump-table-optimizable (jmp-table)
  "Return t if JMP-TABLE can be optimized out."
  (cl-loop
   with labels = (cl-loop for target-label being each hash-value of jmp-table
                          collect target-label)
   with x = (car labels)
   for l in (cdr-safe labels)
   unless (= l x)
     return nil
   finally return t))

(defun comp-emit-switch (var last-insn)
  "Emit a limple for a lap jump table given VAR and LAST-INSN."
  ;; FIXME this not efficient for big jump tables. We should have a second
  ;; strategy for this case.
  (pcase last-insn
    (`(setimm ,_ ,jmp-table)
     (unless (comp-jump-table-optimizable jmp-table)
       (cl-loop
        for test being each hash-keys of jmp-table
        using (hash-value target-label)
        with len = (hash-table-count jmp-table)
        with test-func = (hash-table-test jmp-table)
        for n from 1
        for last = (= n len)
        for m-test = (make-comp-mvar :constant test)
        for target-name = (comp-block-name (comp-bb-maybe-add (comp-label-to-addr target-label)
                                                              (comp-sp)))
        for ff-bb = (if last
                        (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                           (comp-sp))
                      (make--comp-block-lap nil
                                            (comp-sp)
                                            (comp-new-block-sym)))
        for ff-bb-name = (comp-block-name ff-bb)
        if (eq test-func 'eq)
          do (comp-emit (list 'cond-jump var m-test ff-bb-name target-name))
        else
        ;; Store the result of the comparison into the scratch slot before
        ;; emitting the conditional jump.
          do (comp-emit (list 'set (make-comp-mvar :slot 'scratch)
                              (comp-call test-func var m-test)))
             (comp-emit (list 'cond-jump
                              (make-comp-mvar :slot 'scratch)
                              (make-comp-mvar :constant nil)
                              target-name ff-bb-name))
        unless last
        ;; All fall through are artificially created here except the last one.
          do (puthash ff-bb-name ff-bb (comp-func-blocks comp-func))
             (setf (comp-limplify-curr-block comp-pass) ff-bb))))
    (_ (signal 'native-ice
               "missing previous setimm while creating a switch"))))

(defun comp-emit-set-call-subr (subr-name sp-delta)
    "Emit a call for SUBR-NAME.
SP-DELTA is the stack adjustment."
    (let ((subr (symbol-function subr-name))
          (nargs (1+ (- sp-delta))))
      (unless (subrp subr)
        (signal 'native-ice (list "not a subr" subr)))
      (let* ((arity (subr-arity subr))
             (minarg (car arity))
             (maxarg (cdr arity)))
        (when (eq maxarg 'unevalled)
          (signal 'native-ice (list "subr contains  unevalled args" subr-name)))
        (if (eq maxarg 'many)
            ;; callref case.
            (comp-emit-set-call (comp-callref subr-name nargs (comp-sp)))
          ;; Normal call.
          (unless (and (>= maxarg nargs) (<= minarg nargs))
            (signal 'native-ice
                    (list "incoherent stack adjustment" nargs maxarg minarg)))
          (let* ((subr-name subr-name)
                 (slots (cl-loop for i from 0 below maxarg
                                 collect (comp-slot-n (+ i (comp-sp))))))
            (comp-emit-set-call (apply #'comp-call (cons subr-name slots))))))))

(eval-when-compile
  (defun comp-op-to-fun (x)
    "Given the LAP op strip \"byte-\" to have the subr name."
    (intern (replace-regexp-in-string "byte-" "" x)))

  (defun comp-body-eff (body op-name sp-delta)
    "Given the original body BODY compute the effective one.
When BODY is auto guess function name form the LAP byte-code
name.  Otherwise expect lname fnname."
    (pcase (car body)
      ('auto
       (list `(comp-emit-set-call-subr
               ',(comp-op-to-fun op-name)
               ,sp-delta)))
      ((pred symbolp)
       (list `(comp-emit-set-call-subr
               ',(car body)
               ,sp-delta)))
      (_ body))))

(defmacro comp-op-case (&rest cases)
  "Expand CASES into the corresponding `pcase' expansion.
This is responsible for generating the proper stack adjustment when known and
the annotation emission."
  (declare (debug (body))
           (indent defun))
  `(pcase op
     ,@(cl-loop for (op . body) in cases
		for sp-delta = (gethash op comp-op-stack-info)
                for op-name = (symbol-name op)
		if body
		collect `(',op
                          ;; Log all LAP ops except the TAG one.
                          ,(unless (eq op 'TAG)
                             `(comp-emit-annotation
                               ,(concat "LAP op " op-name)))
                          ;; Emit the stack adjustment if present.
                          ,(when (and sp-delta (not (eq 0 sp-delta)))
			     `(cl-incf (comp-sp) ,sp-delta))
                          ,@(comp-body-eff body op-name sp-delta))
                else
		collect `(',op (signal 'native-ice
                                       (list "unsupported LAP op" ',op-name))))
     (_ (signal 'native-ice (list "unexpected LAP op" (symbol-name op))))))

(defun comp-limplify-lap-inst (insn)
  "Limplify LAP instruction INSN pushing it in the proper basic block."
  (let ((op (car insn))
        (arg (if (consp (cdr insn))
                 (cadr insn)
               (cdr insn))))
    (comp-op-case
      (TAG
       (cl-destructuring-bind (_TAG label-num . label-sp) insn
         ;; Paranoid?
         (when label-sp
           (cl-assert (= (1- label-sp) (comp-limplify-sp comp-pass))))
         (comp-emit-annotation (format "LAP TAG %d" label-num))))
      (byte-stack-ref
       (comp-copy-slot (- (comp-sp) arg 1)))
      (byte-varref
       (comp-emit-set-call (comp-call 'symbol-value (make-comp-mvar
                                                     :constant arg))))
      (byte-varset
       (comp-emit (comp-call 'set_internal
                             (make-comp-mvar :constant arg)
                             (comp-slot+1))))
      (byte-varbind ;; Verify
       (comp-emit (comp-call 'specbind
                             (make-comp-mvar :constant arg)
                             (comp-slot+1))))
      (byte-call
       (cl-incf (comp-sp) (- arg))
       (comp-emit-set-call (comp-callref 'funcall (1+ arg) (comp-sp))))
      (byte-unbind
       (comp-emit (comp-call 'helper_unbind_n
                             (make-comp-mvar :constant arg))))
      (byte-pophandler
       (comp-emit '(pop-handler)))
      (byte-pushconditioncase
       (comp-emit-handler (cddr insn) 'condition-case))
      (byte-pushcatch
       (comp-emit-handler (cddr insn) 'catcher))
      (byte-nth auto)
      (byte-symbolp auto)
      (byte-consp auto)
      (byte-stringp auto)
      (byte-listp auto)
      (byte-eq auto)
      (byte-memq auto)
      (byte-not null)
      (byte-car auto)
      (byte-cdr auto)
      (byte-cons auto)
      (byte-list1
       (comp-limplify-listn 1))
      (byte-list2
       (comp-limplify-listn 2))
      (byte-list3
       (comp-limplify-listn 3))
      (byte-list4
       (comp-limplify-listn 4))
      (byte-length auto)
      (byte-aref auto)
      (byte-aset auto)
      (byte-symbol-value auto)
      (byte-symbol-function auto)
      (byte-set auto)
      (byte-fset auto)
      (byte-get auto)
      (byte-substring auto)
      (byte-concat2
       (comp-emit-set-call (comp-callref 'concat 2 (comp-sp))))
      (byte-concat3
       (comp-emit-set-call (comp-callref 'concat 3 (comp-sp))))
      (byte-concat4
       (comp-emit-set-call (comp-callref 'concat 4 (comp-sp))))
      (byte-sub1 1-)
      (byte-add1 1+)
      (byte-eqlsign =)
      (byte-gtr >)
      (byte-lss <)
      (byte-leq <=)
      (byte-geq >=)
      (byte-diff -)
      (byte-negate
       (comp-emit-set-call (comp-call 'negate (comp-slot))))
      (byte-plus +)
      (byte-max auto)
      (byte-min auto)
      (byte-mult *)
      (byte-point auto)
      (byte-goto-char auto)
      (byte-insert auto)
      (byte-point-max auto)
      (byte-point-min auto)
      (byte-char-after auto)
      (byte-following-char auto)
      (byte-preceding-char preceding-char)
      (byte-current-column auto)
      (byte-indent-to
       (comp-emit-set-call (comp-call 'indent-to
                                      (comp-slot)
                                      (make-comp-mvar :constant nil))))
      (byte-scan-buffer-OBSOLETE)
      (byte-eolp auto)
      (byte-eobp auto)
      (byte-bolp auto)
      (byte-bobp auto)
      (byte-current-buffer auto)
      (byte-set-buffer auto)
      (byte-save-current-buffer
       (comp-emit (comp-call 'record_unwind_current_buffer)))
      (byte-set-mark-OBSOLETE)
      (byte-interactive-p-OBSOLETE)
      (byte-forward-char auto)
      (byte-forward-word auto)
      (byte-skip-chars-forward auto)
      (byte-skip-chars-backward auto)
      (byte-forward-line auto)
      (byte-char-syntax auto)
      (byte-buffer-substring auto)
      (byte-delete-region auto)
      (byte-narrow-to-region
       (comp-emit-set-call (comp-call 'narrow-to-region
                                      (comp-slot)
                                      (comp-slot+1))))
      (byte-widen
       (comp-emit-set-call (comp-call 'widen)))
      (byte-end-of-line auto)
      (byte-constant2) ; TODO
      ;; Branches.
      (byte-goto
       (comp-emit-uncond-jump (cddr insn)))
      (byte-goto-if-nil
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 0
                            (cddr insn) nil))
      (byte-goto-if-not-nil
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 0
                            (cddr insn) t))
      (byte-goto-if-nil-else-pop
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 1
                            (cddr insn) nil))
      (byte-goto-if-not-nil-else-pop
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 1
                            (cddr insn) t))
      (byte-return
       (comp-emit `(return ,(comp-slot+1))))
      (byte-discard 'pass)
      (byte-dup
       (comp-copy-slot (1- (comp-sp))))
      (byte-save-excursion
       (comp-emit (comp-call 'record_unwind_protect_excursion)))
      (byte-save-window-excursion-OBSOLETE)
      (byte-save-restriction
       (comp-emit (comp-call 'helper_save_restriction)))
      (byte-catch) ;; Obsolete
      (byte-unwind-protect
       (comp-emit (comp-call 'helper_unwind_protect (comp-slot+1))))
      (byte-condition-case) ;; Obsolete
      (byte-temp-output-buffer-setup-OBSOLETE)
      (byte-temp-output-buffer-show-OBSOLETE)
      (byte-unbind-all) ;; Obsolete
      (byte-set-marker auto)
      (byte-match-beginning auto)
      (byte-match-end auto)
      (byte-upcase auto)
      (byte-downcase auto)
      (byte-string= string-equal)
      (byte-string< string-lessp)
      (byte-equal auto)
      (byte-nthcdr auto)
      (byte-elt auto)
      (byte-member auto)
      (byte-assq auto)
      (byte-nreverse auto)
      (byte-setcar auto)
      (byte-setcdr auto)
      (byte-car-safe auto)
      (byte-cdr-safe auto)
      (byte-nconc auto)
      (byte-quo /)
      (byte-rem %)
      (byte-numberp auto)
      (byte-integerp auto)
      (byte-listN
       (cl-incf (comp-sp) (- 1 arg))
       (comp-emit-set-call (comp-callref 'list arg (comp-sp))))
      (byte-concatN
       (cl-incf (comp-sp) (- 1 arg))
       (comp-emit-set-call (comp-callref 'concat arg (comp-sp))))
      (byte-insertN
       (cl-incf (comp-sp) (- 1 arg))
       (comp-emit-set-call (comp-callref 'insert arg (comp-sp))))
      (byte-stack-set
       (comp-copy-slot (1+ (comp-sp)) (- (comp-sp) arg -1)))
      (byte-stack-set2 (cl-assert nil)) ;; TODO
      (byte-discardN
       (cl-incf (comp-sp) (- arg)))
      (byte-switch
       ;; Assume to follow the emission of a setimm.
       ;; This is checked into comp-emit-switch.
       (comp-emit-switch (comp-slot+1)
                         (cl-second (comp-block-insns
                                     (comp-limplify-curr-block comp-pass)))))
      (byte-constant
       (comp-emit-setimm arg))
      (byte-discardN-preserve-tos
       (cl-incf (comp-sp) (- arg))
       (comp-copy-slot (+ arg (comp-sp)))))))

(defun comp-emit-narg-prologue (minarg nonrest rest)
  "Emit the prologue for a narg function."
  (cl-loop for i below minarg
           do (comp-emit `(set-args-to-local ,(comp-slot-n i)))
              (comp-emit '(inc-args)))
  (cl-loop for i from minarg below nonrest
           for bb = (intern (format "entry_%s" i))
           for fallback = (intern (format "entry_fallback_%s" i))
           do (comp-emit `(cond-jump-narg-leq ,i ,bb ,fallback))
              (comp-make-curr-block bb (comp-sp))
              (comp-emit `(set-args-to-local ,(comp-slot-n i)))
              (comp-emit '(inc-args))
              finally (comp-emit '(jump entry_rest_args)))
  (when (not (= minarg nonrest))
    (cl-loop for i from minarg below nonrest
             for bb = (intern (format "entry_fallback_%s" i))
             for next-bb = (if (= (1+ i) nonrest)
                               'entry_rest_args
                             (intern (format "entry_fallback_%s" (1+ i))))
             do (comp-with-sp i
                  (comp-make-curr-block bb (comp-sp))
                  (comp-emit-setimm nil)
                  (comp-emit `(jump ,next-bb)))))
  (comp-make-curr-block 'entry_rest_args (comp-sp))
  (comp-emit `(set-rest-args-to-local ,(comp-slot-n nonrest)))
  (setf (comp-sp) nonrest)
  (when (and (> nonrest 8) (null rest))
    (cl-decf (comp-sp))))

(defun comp-limplify-finalize-function (func)
  "Reverse insns into all basic blocks of FUNC."
  (cl-loop for bb being the hash-value in (comp-func-blocks func)
           do (setf (comp-block-insns bb)
                    (nreverse (comp-block-insns bb))))
  (comp-log-func func 2)
  func)

(cl-defgeneric comp-prepare-args-for-top-level (function)
  "Given FUNCTION return the two args arguments for comp--register-...")

(cl-defmethod comp-prepare-args-for-top-level ((function comp-func-l))
  "Lexical scoped FUNCTION."
  (let ((args (comp-func-l-args function)))
    (cons (make-comp-mvar :constant (comp-args-base-min args))
          (make-comp-mvar :constant (if (comp-args-p args)
                                        (comp-args-max args)
                                      'many)))))

(cl-defmethod comp-prepare-args-for-top-level ((function comp-func-d))
  "Dynamic scoped FUNCTION."
  (cons (make-comp-mvar :constant (func-arity (comp-func-byte-func function)))
        (let ((comp-curr-allocation-class 'd-default))
          ;; Lambda-lists must stay in the same relocation class of
          ;; the object referenced by code to respect uninterned
          ;; symbols.
          (make-comp-mvar :constant (comp-func-d-lambda-list function)))))

(cl-defgeneric comp-emit-for-top-level (form for-late-load)
  "Emit the limple code for top level FORM.")

(cl-defmethod comp-emit-for-top-level ((form byte-to-native-func-def)
                                       for-late-load)
  (let* ((name (byte-to-native-func-def-name form))
         (c-name (byte-to-native-func-def-c-name form))
         (f (gethash c-name (comp-ctxt-funcs-h comp-ctxt)))
         (args (comp-prepare-args-for-top-level f)))
    (cl-assert (and name f))
    (comp-emit (comp-call (if for-late-load
                              'comp--late-register-subr
                            'comp--register-subr)
                          (make-comp-mvar :constant name)
                          (car args)
                          (cdr args)
                          (make-comp-mvar :constant c-name)
                          (make-comp-mvar
                           :constant
                           (let* ((h (comp-ctxt-function-docs comp-ctxt))
                                  (i (hash-table-count h)))
                             (puthash i (comp-func-doc f) h)
                             i))
                          (make-comp-mvar :constant
                                          (comp-func-int-spec f))
                          ;; This is the compilation unit it-self passed as
                          ;; parameter.
                          (make-comp-mvar :slot 0)))))

(cl-defmethod comp-emit-for-top-level ((form byte-to-native-top-level)
                                       for-late-load)
  (unless for-late-load
    (comp-emit
     (comp-call 'eval
                (let ((comp-curr-allocation-class 'd-impure))
                  (make-comp-mvar :constant
                                  (byte-to-native-top-level-form form)))
                (make-comp-mvar :constant
                                (byte-to-native-top-level-lexical form))))))

(defun comp-emit-lambda-for-top-level (func)
  "Emit the creation of subrs for lambda FUNC.
These are stored in the reloc data array."
  (let ((args (comp-prepare-args-for-top-level func)))
    (let ((comp-curr-allocation-class 'd-impure))
      (comp-add-const-to-relocs (comp-func-byte-func func)))
    (comp-emit
     (comp-call 'comp--register-lambda
                ;; mvar to be fixed-up when containers are
                ;; finalized.
                (or (gethash (comp-func-byte-func func)
                             (comp-ctxt-lambda-fixups-h comp-ctxt))
                    (puthash (comp-func-byte-func func)
                             (make-comp-mvar :constant nil)
                             (comp-ctxt-lambda-fixups-h comp-ctxt)))
                (car args)
                (cdr args)
                (make-comp-mvar :constant (comp-func-c-name func))
                (make-comp-mvar
                 :constant (let* ((h (comp-ctxt-function-docs comp-ctxt))
                                  (i (hash-table-count h)))
                             (puthash i (comp-func-doc func) h)
                             i))
                (make-comp-mvar :constant (comp-func-int-spec func))
                ;; This is the compilation unit it-self passed as
                ;; parameter.
                (make-comp-mvar :slot 0)))))

(defun comp-limplify-top-level (for-late-load)
  "Create a limple function to modify the global environment at load.
When FOR-LATE-LOAD is non nil the emitted function modifies only
function definition.

Synthesize a function called 'top_level_run' that gets one single
parameter (the compilation unit it-self).  To define native
functions 'top_level_run' will call back `comp--register-subr'
into the C code forwarding the compilation unit."
  ;; Once an .eln is loaded and Emacs is dumped 'top_level_run' has no
  ;; reasons to be execute ever again.  Therefore all objects can be
  ;; just ephemeral.
  (let* ((comp-curr-allocation-class 'd-ephemeral)
         (func (make-comp-func-l :name (if for-late-load
                                           'late-top-level-run
                                         'top-level-run)
                                 :c-name (if for-late-load
                                             "late_top_level_run"
                                           "top_level_run")
                                 :args (make-comp-args :min 1 :max 1)
                                 :frame-size 1
                                 :speed comp-speed))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :curr-block (make--comp-block-lap -1 0 'top-level)
                     :frame (comp-new-frame 1))))
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation (if for-late-load
                              "Late top level"
                            "Top level"))
    ;; Assign the compilation unit incoming as parameter to the slot frame 0.
    (comp-emit `(set-par-to-local ,(comp-slot-n 0) 0))
    (maphash (lambda (_ func)
               (comp-emit-lambda-for-top-level func))
             (comp-ctxt-byte-func-to-func-h comp-ctxt))
    (mapc (lambda (x) (comp-emit-for-top-level x for-late-load))
          (comp-ctxt-top-level-forms comp-ctxt))
    (comp-emit `(return ,(make-comp-mvar :constant t)))
    (puthash 0 (comp-func-frame-size func) (comp-func-array-h func))
    (comp-limplify-finalize-function func)))

(defun comp-addr-to-bb-name (addr)
  "Search for a block starting at ADDR into pending or limplified blocks."
  ;; FIXME Actually we could have another hash for this.
  (cl-flet ((pred (bb)
              (equal (comp-block-lap-addr bb) addr)))
    (if-let ((pending (cl-find-if #'pred
                                  (comp-limplify-pending-blocks comp-pass))))
        (comp-block-name pending)
      (cl-loop for bb being the hash-value in (comp-func-blocks comp-func)
               when (pred bb)
                 return (comp-block-name bb)))))

(defun comp-limplify-block (bb)
  "Limplify basic-block BB and add it to the current function."
  (setf (comp-limplify-curr-block comp-pass) bb
        (comp-limplify-sp comp-pass) (comp-block-lap-sp bb)
        (comp-limplify-pc comp-pass) (comp-block-lap-addr bb))
  (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
  (cl-loop
   for inst-cell on (nthcdr (comp-limplify-pc comp-pass)
                            (comp-func-lap comp-func))
   for inst = (car inst-cell)
   for next-inst = (car-safe (cdr inst-cell))
   do (comp-limplify-lap-inst inst)
      (cl-incf (comp-limplify-pc comp-pass))
   when (comp-lap-fall-through-p inst)
   do (pcase next-inst
        (`(TAG ,_label . ,label-sp)
         (when label-sp
           (cl-assert (= (1- label-sp) (comp-sp))))
         (let* ((stack-depth (if label-sp
                                 (1- label-sp)
                               (comp-sp)))
                (next-bb (comp-block-name (comp-bb-maybe-add (comp-limplify-pc comp-pass) stack-depth))))
           (unless (comp-block-closed bb)
             (comp-emit `(jump ,next-bb))))
         (cl-return)))
   until (comp-lap-eob-p inst)))

(defun comp-limplify-function (func)
  "Limplify a single function FUNC."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :frame (comp-new-frame frame-size))))
    (comp-fill-label-h)
    ;; Prologue
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-name func))))
    ;; Dynamic functions have parameters bound by the trampoline.
    (when (comp-func-l-p func)
      (let ((args (comp-func-l-args func)))
        (if (comp-args-p args)
            (cl-loop for i below (comp-args-max args)
                     do (cl-incf (comp-sp))
                        (comp-emit `(set-par-to-local ,(comp-slot) ,i)))
          (comp-emit-narg-prologue (comp-args-base-min args)
                                   (comp-nargs-nonrest args)
                                   (comp-nargs-rest args)))))
    (comp-emit '(jump bb_0))
    ;; Body
    (comp-bb-maybe-add 0 (comp-sp))
    (cl-loop for next-bb = (pop (comp-limplify-pending-blocks comp-pass))
             while next-bb
             do (comp-limplify-block next-bb))
    ;; Sanity check against block duplication.
    (cl-loop with addr-h = (make-hash-table)
             for bb being the hash-value in (comp-func-blocks func)
             for addr = (when (comp-block-lap-p bb)
                          (comp-block-lap-addr bb))
             when addr
               do (cl-assert (null (gethash addr addr-h)))
                  (puthash addr t addr-h))
    (comp-limplify-finalize-function func)))

(defun comp-limplify (_)
  "Compute LIMPLE IR for forms in `comp-ctxt'."
  (maphash (lambda (_ f) (comp-limplify-function f))
           (comp-ctxt-funcs-h comp-ctxt))
  (comp-add-func-to-ctxt (comp-limplify-top-level nil))
  (when (comp-ctxt-with-late-load comp-ctxt)
    (comp-add-func-to-ctxt (comp-limplify-top-level t))))


;;; pure-func pass specific code.

;; Simple IPA pass to infer function purity of functions not
;; explicitly declared as such.  This is effective only at speed 3 to
;; avoid optimizing-out functions and preventing their redefinition
;; being effective.

(defun comp-collect-calls (f)
  "Return a list with all the functions called by F."
  (cl-loop
   with h = (make-hash-table :test #'eq)
   for b being each hash-value of (comp-func-blocks f)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(set ,_lval (,(pred comp-call-op-p) ,f . ,_rest))
             (puthash f t h))
            (`(,(pred comp-call-op-p) ,f . ,_rest)
             (puthash f t h))))
   finally return (cl-loop
                   for f being each hash-key of h
                   collect (if (stringp f)
                               (comp-func-name
                                (gethash f
                                         (comp-ctxt-funcs-h comp-ctxt)))
                             f))))

(defun comp-pure-infer-func (f)
  "If all funtions called by F are pure then F is pure too."
  (when (and (cl-every (lambda (x)
                         (or (comp-function-pure-p x)
                             (eq x (comp-func-name f))))
                       (comp-collect-calls f))
             (not (eq (comp-func-pure f) t)))
    (comp-log (format "%s inferred to be pure" (comp-func-name f)))
    (setf (comp-func-pure f) t)))

(defun comp-ipa-pure (_)
  "Infer function purity."
  (cl-loop
   with pure-n = 0
   for n from 1
   while
   (/= pure-n
       (setf pure-n
             (cl-loop
              for f being each hash-value of (comp-ctxt-funcs-h comp-ctxt)
              when (and (>= (comp-func-speed f) 3)
                        (comp-func-l-p f)
                        (not (comp-func-pure f)))
              do (comp-pure-infer-func f)
              count (comp-func-pure f))))
   finally (comp-log (format "ipa-pure iterated %d times" n))))


;;; SSA pass specific code.
;; After limplification no edges are present between basic blocks and an
;; implicit phi is present for every slot at the beginning of every basic block.
;; This pass is responsible for building all the edges and replace all m-vars
;; plus placing the needed phis.
;; Because the number of phis placed is (supposed) to be the minimum necessary
;; this form is called 'minimal SSA form'.
;; This pass should be run every time basic blocks or m-var are shuffled.

(cl-defun make-comp-ssa-mvar (&key slot (constant nil const-vld) type)
  (let ((mvar (make--comp-mvar :slot slot
                               :const-vld const-vld
                               :constant constant
                               :type type)))
    (setf (comp-mvar-id mvar) (sxhash-eq mvar))
    mvar))

(defun comp-clean-ssa (f)
  "Clean-up SSA for funtion F."
  (setf (comp-func-edges f) ())
  (cl-loop
   for b being each hash-value of (comp-func-blocks f)
   do (setf (comp-block-in-edges b) ()
            (comp-block-out-edges b) ()
            (comp-block-dom b) nil
            (comp-block-df b) (make-hash-table)
            (comp-block-post-num b) nil
            (comp-block-final-frame b) nil
            ;; Prune all phis.
            (comp-block-insns b) (cl-loop for insn in (comp-block-insns b)
                                          unless (eq 'phi (car insn))
                                            collect insn))))

(defun comp-compute-edges ()
  "Compute the basic block edges for the current function."
  (cl-flet ((edge-add (&rest args)
              (push
               (apply #'make--comp-edge
                      :number (funcall (comp-func-edge-cnt-gen comp-func))
                      args)
               (comp-func-edges comp-func))))

    (cl-loop with blocks = (comp-func-blocks comp-func)
             for bb being each hash-value of blocks
             for last-insn = (car (last (comp-block-insns bb)))
             for (op first second third forth) = last-insn
             do (cl-case op
                  (jump
                   (edge-add :src bb :dst (gethash first blocks)))
                  (cond-jump
                   (edge-add :src bb :dst (gethash third blocks))
                   (edge-add :src bb :dst (gethash forth blocks)))
                  (cond-jump-narg-leq
                   (edge-add :src bb :dst (gethash second blocks))
                   (edge-add :src bb :dst (gethash third blocks)))
                  (push-handler
                   (edge-add :src bb :dst (gethash third blocks))
                   (edge-add :src bb :dst (gethash forth blocks)))
                  (return)
                  (otherwise
                   (signal 'native-ice
                           (list "block does not end with a branch"
                                 bb
                                 (comp-func-name comp-func)))))
             finally (setf (comp-func-edges comp-func)
                           (nreverse (comp-func-edges comp-func)))
                     ;; Update edge refs into blocks.
                     (cl-loop for edge in (comp-func-edges comp-func)
                              do (push edge
                                       (comp-block-out-edges (comp-edge-src edge)))
                              (push edge
                                    (comp-block-in-edges (comp-edge-dst edge))))
                     (comp-log-edges comp-func))))

(defun comp-collect-rev-post-order (basic-block)
  "Walk BASIC-BLOCK children and return their name in reversed post-order."
  (let ((visited (make-hash-table))
        (acc ()))
    (cl-labels ((collect-rec (bb)
                  (let ((name (comp-block-name bb)))
                    (unless (gethash name visited)
                      (puthash name t visited)
                      (cl-loop for e in (comp-block-out-edges bb)
                               for dst-block = (comp-edge-dst e)
                               do (collect-rec dst-block))
                      (push name acc)))))
      (collect-rec basic-block)
      acc)))

(defun comp-compute-dominator-tree ()
  "Compute immediate dominators for each basic block in current function."
  ;; Originally based on: "A Simple, Fast Dominance Algorithm"
  ;; Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001).
  (cl-flet ((intersect (b1 b2)
              (let ((finger1 (comp-block-post-num b1))
                    (finger2 (comp-block-post-num b2)))
                (while (not (= finger1 finger2))
                  (while (< finger1 finger2)
                    (setf b1 (comp-block-dom b1)
                          finger1 (comp-block-post-num b1)))
                  (while (< finger2 finger1)
                    (setf b2 (comp-block-dom b2)
                          finger2 (comp-block-post-num b2))))
                b1))
            (first-processed (l)
              (if-let ((p (cl-find-if (lambda (p) (comp-block-dom p)) l)))
                  p
                (signal 'native-ice "cant't find first preprocessed"))))

    (when-let ((blocks (comp-func-blocks comp-func))
               (entry (gethash 'entry blocks))
               ;; No point to go on if the only bb is 'entry'.
               (bb1 (gethash 'bb_1 blocks)))
      (cl-loop with rev-bb-list = (comp-collect-rev-post-order entry)
               with changed = t
               while changed
               initially (progn
                           (comp-log "Computing dominator tree...\n" 2)
                           (setf (comp-block-dom entry) entry)
                           ;; Set the post order number.
                           (cl-loop for name in (reverse rev-bb-list)
                                    for b = (gethash name blocks)
                                    for i from 0
                                    do (setf (comp-block-post-num b) i)))
               do (cl-loop
                   for name in (cdr rev-bb-list)
                   for b = (gethash name blocks)
                   for preds = (comp-block-preds b)
                   for new-idom = (first-processed preds)
                   initially (setf changed nil)
                   do (cl-loop for p in (delq new-idom preds)
                               when (comp-block-dom p)
                                 do (setf new-idom (intersect p new-idom)))
                   unless (eq (comp-block-dom b) new-idom)
                   do (setf (comp-block-dom b) new-idom
                            changed t))))))

(defun comp-compute-dominator-frontiers ()
  "Compute the dominator frontier for each basic block in `comp-func'."
  ;; Originally based on: "A Simple, Fast Dominance Algorithm"
  ;; Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001).
  (cl-loop with blocks = (comp-func-blocks comp-func)
           for b-name being each hash-keys of blocks
           using (hash-value b)
           for preds = (comp-block-preds b)
           when (>= (length preds) 2) ; All joins
           do (cl-loop for p in preds
                       for runner = p
                       do (while (not (eq runner (comp-block-dom b)))
                            (puthash b-name b (comp-block-df runner))
                            (setf runner (comp-block-dom runner))))))

(defun comp-log-block-info ()
  "Log basic blocks info for the current function."
  (maphash (lambda (name bb)
             (let ((dom (comp-block-dom bb))
                   (df (comp-block-df bb)))
               (comp-log (format "block: %s idom: %s DF %s\n"
                                 name
                                 (when dom (comp-block-name dom))
                                 (cl-loop for b being each hash-keys of df
                                          collect b))
                         3)))
           (comp-func-blocks comp-func)))

(defun comp-place-phis ()
  "Place phi insns into the current function."
  ;; Originally based on: Static Single Assignment Book
  ;; Algorithm 3.1: Standard algorithm for inserting phi-functions
  (cl-flet ((add-phi (slot-n bb)
             ;; Add a phi func for slot SLOT-N at the top of BB.
             (push `(phi ,slot-n) (comp-block-insns bb)))
            (slot-assigned-p (slot-n bb)
             ;; Return t if a SLOT-N was assigned within BB.
             (cl-loop for insn in (comp-block-insns bb)
                      for op = (car insn)
                      when (or (and (comp-assign-op-p op)
                                    (eql slot-n (comp-mvar-slot (cadr insn))))
                               ;; fetch-handler is after a non local
                               ;; therefore clobbers all frame!!!
                               (eq op 'fetch-handler))
                        return t)))

    (cl-loop for i from 0 below (comp-func-frame-size comp-func)
             ;; List of blocks with a definition of mvar i
             for defs-v = (cl-loop with blocks = (comp-func-blocks comp-func)
                                    for b being each hash-value of blocks
                                    when (slot-assigned-p i b)
                                    collect b)
             ;; Set of basic blocks where phi is added.
             for f = ()
             ;; Worklist, set of basic blocks that contain definitions of v.
             for w = defs-v
             do
             (while w
               (let ((x (pop w)))
                 (cl-loop for y being each hash-value of (comp-block-df x)
                          unless (cl-find y f)
                          do (add-phi i y)
                             (push y f)
                             ;; Adding a phi implies mentioning the
                             ;; corresponding slot so in case adjust w.
                             (unless (cl-find y defs-v)
                               (push y w))))))))

(defun comp-dom-tree-walker (bb pre-lambda post-lambda)
  "Dominator tree walker function starting from basic block BB.
PRE-LAMBDA and POST-LAMBDA are called in pre or post-order if non nil."
  (when pre-lambda
    (funcall pre-lambda bb))
  (when-let ((out-edges (comp-block-out-edges bb)))
    (cl-loop for ed in out-edges
             for child = (comp-edge-dst ed)
             when (eq bb (comp-block-dom child))
             ;; Current block is the immediate dominator then recur.
             do (comp-dom-tree-walker child pre-lambda post-lambda)))
  (when post-lambda
    (funcall post-lambda bb)))

(cl-defstruct (comp-ssa (:copier nil))
  "Support structure used while SSA renaming."
  (frame (comp-new-frame (comp-func-frame-size comp-func) t) :type vector
         :documentation "Vector of m-vars."))

(defun comp-ssa-rename-insn (insn frame)
  (dotimes (slot-n (comp-func-frame-size comp-func))
    (cl-flet ((targetp (x)
                ;; Ret t if x is an mvar and target the correct slot number.
                (and (comp-mvar-p x)
                     (eql slot-n (comp-mvar-slot x))))
              (new-lvalue ()
                ;; If is an assignment make a new mvar and put it as l-value.
                (let ((mvar (make-comp-ssa-mvar :slot slot-n)))
                  (setf (aref frame slot-n) mvar
                        (cadr insn) mvar))))
      (pcase insn
        (`(,(pred comp-assign-op-p) ,(pred targetp) . ,_)
         (let ((mvar (aref frame slot-n)))
           (setcdr insn (cl-nsubst-if mvar #'targetp (cdr insn))))
         (new-lvalue))
        (`(fetch-handler . ,_)
         ;; Clobber all no matter what!
         (setf (aref frame slot-n) (make-comp-ssa-mvar :slot slot-n)))
        (`(phi  ,n)
         (when (equal n slot-n)
           (new-lvalue)))
        (_
         (let ((mvar (aref frame slot-n)))
           (setcdr insn (cl-nsubst-if mvar #'targetp (cdr insn)))))))))

(defun comp-ssa-rename ()
  "Entry point to rename into SSA within the current function."
  (comp-log "Renaming\n" 2)
  (let ((frame-size (comp-func-frame-size comp-func))
        (visited (make-hash-table)))
    (cl-labels ((ssa-rename-rec (bb in-frame)
                  (unless (gethash bb visited)
                    (puthash bb t visited)
                    (cl-loop for insn in (comp-block-insns bb)
                             do (comp-ssa-rename-insn insn in-frame))
                    (setf (comp-block-final-frame bb)
                          (copy-sequence in-frame))
                    (when-let ((out-edges (comp-block-out-edges bb)))
                      (cl-loop for ed in out-edges
                               for child = (comp-edge-dst ed)
                               ;; Provide a copy of the same frame to all childs.
                               do (ssa-rename-rec child (copy-sequence in-frame)))))))

      (ssa-rename-rec (gethash 'entry (comp-func-blocks comp-func))
                      (comp-new-frame frame-size t)))))

(defun comp-finalize-phis ()
  "Fixup r-values into phis in all basic blocks."
  (cl-flet ((finalize-phi (args b)
              ;; Concatenate into args all incoming m-vars for this phi.
              (setcdr args
                      (cl-loop with slot-n = (comp-mvar-slot (car args))
                               for e in (comp-block-in-edges b)
                               for b = (comp-edge-src e)
                               for in-frame = (comp-block-final-frame b)
                               collect (aref in-frame slot-n)))))

    (cl-loop for b being each hash-value of (comp-func-blocks comp-func)
             do (cl-loop for (op . args) in (comp-block-insns b)
                         when (eq op 'phi)
                           do (finalize-phi args b)))))

(defun comp-ssa ()
  "Port all functions into mininal SSA form."
  (maphash (lambda (_ f)
             (let* ((comp-func f)
                    (ssa-status (comp-func-ssa-status f)))
               (unless (eq ssa-status t)
                 (when (eq ssa-status 'dirty)
                   (comp-clean-ssa f))
                 (comp-compute-edges)
                 (comp-compute-dominator-tree)
                 (comp-compute-dominator-frontiers)
                 (comp-log-block-info)
                 (comp-place-phis)
                 (comp-ssa-rename)
                 (comp-finalize-phis)
                 (comp-log-func comp-func 3)
                 (setf (comp-func-ssa-status f) t))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; propagate pass specific code.
;; A very basic propagation pass follows.
;; This propagates values and types plus ref property in the control flow graph.
;; This is also responsible for removing function calls to pure functions if
;; possible.

(defsubst comp-strict-type-of (obj)
  "Given OBJ return its type understanding fixnums."
  ;; Should be certainly smarter but now we take advantages just from fixnums.
  (if (fixnump obj)
      'fixnum
    (type-of obj)))

(defun comp-copy-insn (insn)
  "Deep copy INSN."
  ;; Adapted from `copy-tree'.
  (if (consp insn)
      (let (result)
	(while (consp insn)
	  (let ((newcar (car insn)))
	    (if (or (consp (car insn)) (comp-mvar-p (car insn)))
		(setf newcar (comp-copy-insn (car insn))))
	    (push newcar result))
	  (setf insn (cdr insn)))
	(nconc (nreverse result)
               (if (comp-mvar-p insn) (comp-copy-insn insn) insn)))
    (if (comp-mvar-p insn)
        (copy-comp-mvar insn)
      insn)))

(defmacro comp-apply-in-env (func &rest args)
  "Apply FUNC to ARGS in the current compilation environment."
  `(let ((env (cl-loop
               for f being the hash-value in (comp-ctxt-funcs-h comp-ctxt)
               for func-name = (comp-func-name f)
               for byte-code = (comp-func-byte-func f)
               when func-name
               collect `(,func-name . ,(symbol-function func-name))
               and do
               (setf (symbol-function func-name) byte-code))))
     (unwind-protect
         (apply ,func ,@args)
       (cl-loop
        for (func-name . def) in env
        do (setf (symbol-function func-name) def)))))

(defun comp-fwprop-prologue ()
  "Prologue for the propagate pass.
Here goes everything that can be done not iteratively (read once).
Forward propagate immediate involed in assignments."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(setimm ,lval ,v)
             (setf (comp-mvar-const-vld lval) t
                   (comp-mvar-constant lval) v
                   (comp-mvar-type lval) (comp-strict-type-of v)))))))

(defsubst comp-mvar-propagate (lval rval)
  "Propagate into LVAL properties of RVAL."
  (setf (comp-mvar-const-vld lval) (comp-mvar-const-vld rval)
        (comp-mvar-constant lval) (comp-mvar-constant rval)
        (comp-mvar-type lval) (comp-mvar-type rval)))

(defsubst comp-function-optimizable-p (f args)
  "Given function F called with ARGS return non nil when optimizable."
  (and (cl-every #'comp-mvar-const-vld args)
       (comp-function-pure-p f)))

(defsubst comp-function-call-maybe-remove (insn f args)
  "Given INSN when F is pure if all ARGS are known remove the function call."
  (cl-flet ((rewrite-insn-as-setimm (insn value)
               ;; See `comp-emit-setimm'.
               (comp-add-const-to-relocs value)
               (setf (car insn) 'setimm
                     (cddr insn) `(,value))))
    (cond
     ((eq f 'symbol-value)
      (when-let* ((arg0 (car args))
                  (const (comp-mvar-const-vld arg0))
                  (ok-to-optim (member (comp-mvar-constant arg0)
                                       comp-symbol-values-optimizable)))
        (rewrite-insn-as-setimm insn (symbol-value (comp-mvar-constant
                                                    (car args))))))
     ((comp-function-optimizable-p f args)
      (ignore-errors
        ;; No point to complain here in case of error because we
        ;; should do basic block pruning in order to be sure that this
        ;; is not dead-code.  This is now left to gcc, to be
        ;; implemented only if we want a reliable diagnostic here.
        (let* ((f (if-let (f-in-ctxt (comp-symbol-func-to-fun f))
                      ;; If the function is IN the compilation ctxt
                      ;; and know to be pure.
                      (comp-func-byte-func f-in-ctxt)
                    f))
               (value (comp-apply-in-env f (mapcar #'comp-mvar-constant args))))
          (rewrite-insn-as-setimm insn value)))))))

(defun comp-fwprop-insn (insn)
  "Propagate within INSN."
  (pcase insn
    (`(set ,lval ,rval)
     (pcase rval
       (`(,(or 'call 'callref) ,f . ,args)
        (setf (comp-mvar-type lval)
              (alist-get f comp-known-ret-types))
        (comp-function-call-maybe-remove insn f args))
       (`(,(or 'direct-call 'direct-callref) ,f . ,args)
        (let ((f (comp-func-name (gethash f (comp-ctxt-funcs-h comp-ctxt)))))
          (setf (comp-mvar-type lval)
                (alist-get f comp-known-ret-types))
          (comp-function-call-maybe-remove insn f args)))
       (_
        (comp-mvar-propagate lval rval))))
    (`(setimm ,lval ,v)
     (setf (comp-mvar-const-vld lval) t
           (comp-mvar-constant lval) v
           (comp-mvar-type lval) (comp-strict-type-of v)))
    (`(phi ,lval . ,rest)
     ;; Forward const prop here.
     (when-let* ((vld (cl-every #'comp-mvar-const-vld rest))
                 (consts (mapcar #'comp-mvar-constant rest))
                 (x (car consts))
                 (equals (cl-every (lambda (y) (equal x y)) consts)))
       (setf (comp-mvar-constant lval) x))
     ;; Forward type propagation.
     ;; FIXME: checking for type equality is not sufficient cause does not
     ;; account type hierarchy!
     (when-let* ((types (mapcar #'comp-mvar-type rest))
                 (non-empty (cl-notany #'null types))
                 (x (car types))
                 (eqs (cl-every (lambda (y) (eq x y)) types)))
       (setf (comp-mvar-type lval) x)))))

(defun comp-fwprop* ()
  "Propagate for set* and phi operands.
Return t if something was changed."
  (cl-loop with modified = nil
           for b being each hash-value of (comp-func-blocks comp-func)
           do (cl-loop for insn in (comp-block-insns b)
                       for orig-insn = (unless modified ; Save consing after 1th change.
                                         (comp-copy-insn insn))
                       do (comp-fwprop-insn insn)
                       when (and (null modified) (not (equal insn orig-insn)))
                         do (setf modified t))
           finally return modified))

(defun comp-fwprop (_)
  "Forward propagate types and consts within the lattice."
  (comp-ssa)
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        ;; FIXME remove the following condition when tested.
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f))
                 (comp-fwprop-prologue)
                 (cl-loop
                  for i from 1
                  while (comp-fwprop*)
                  finally (comp-log (format "Propagation run %d times\n" i) 2))
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Call optimizer pass specific code.
;; This pass is responsible for the following optimizations:
;; - Call to subrs that are in defined in the C source and are passing through
;;   funcall trampoline gets optimized into normal indirect calls.
;;   This makes effectively this calls equivalent to all the subrs that got
;;   dedicated byte-code ops.
;;   Triggered at comp-speed >= 2.
;; - Recursive calls gets optimized into direct calls.
;;   Triggered at comp-speed >= 2.
;; - Intra compilation unit procedure calls gets optimized into direct calls.
;;   This can be a big win and even allow gcc to inline but does not make
;;   function in the compilation unit re-definable safely without recompiling
;;   the full compilation unit.
;;   For this reason this is triggered only at comp-speed == 3.

(defun comp-func-in-unit (func)
  "Given FUNC return the `comp-fun' definition in the current context.
FUNCTION can be a function-name or byte compiled function."
  (if (symbolp func)
      (comp-symbol-func-to-fun func)
    (cl-assert (byte-code-function-p func))
    (gethash func (comp-ctxt-byte-func-to-func-h comp-ctxt))))

(defun comp-call-optim-form-call (callee args)
  ""
  (cl-flet ((fill-args (args total)
              ;; Fill missing args to reach TOTAL
              (append args (cl-loop repeat (- total (length args))
                                    collect (make-comp-mvar :constant nil)))))
    (when (and callee
               (or (symbolp callee)
                   (gethash callee (comp-ctxt-byte-func-to-func-h comp-ctxt)))
               (not (memq callee comp-never-optimize-functions)))
      (let* ((f (if (symbolp callee)
                    (symbol-function callee)
                  (cl-assert (byte-code-function-p callee))
                  callee))
             (subrp (subrp f))
             (comp-func-callee (comp-func-in-unit callee)))
        (cond
         ((and subrp (not (subr-native-elisp-p f)))
          ;; Trampoline removal.
          (let* ((callee (intern (subr-name f))) ; Fix aliased names.
                 (maxarg (cdr (subr-arity f)))
                 (call-type (if (if subrp
                                    (not (numberp maxarg))
                                  (comp-nargs-p comp-func-callee))
                                'callref
                              'call))
                 (args (if (eq call-type 'callref)
                           args
                         (fill-args args maxarg))))
            `(,call-type ,callee ,@args)))
         ;; Intra compilation unit procedure call optimization.
         ;; Attention speed 3 triggers this for non self calls too!!
         ((and comp-func-callee
               (or (and (>= (comp-func-speed comp-func) 3)
                        (comp-func-unique-in-cu-p callee))
                   (and (>= (comp-func-speed comp-func) 2)
                        ;; Anonymous lambdas can't be redefined so are
                        ;; always safe to optimize.
                        (byte-code-function-p callee))))
          (let* ((func-args (comp-func-l-args comp-func-callee))
                 (nargs (comp-nargs-p func-args))
                 (call-type (if nargs 'direct-callref 'direct-call))
                 (args (if (eq call-type 'direct-callref)
                           args
                         (fill-args args (comp-args-max func-args)))))
            `(,call-type ,(comp-func-c-name comp-func-callee) ,@args)))
         ((comp-type-hint-p callee)
          `(call ,callee ,@args)))))))

(defun comp-call-optim-func ()
  "Perform the trampoline call optimization for the current function."
  (cl-loop
   with self = (comp-func-name comp-func)
   for b being each hash-value of (comp-func-blocks comp-func)
   when self ;; FIXME add proper anonymous lambda support.
   do (comp-loop-insn-in-block b
        (pcase insn
          (`(set ,lval (callref funcall ,f . ,rest))
           (when-let ((new-form (comp-call-optim-form-call
                                 (comp-mvar-constant f) rest)))
             (setf insn `(set ,lval ,new-form))))
          (`(callref funcall ,f . ,rest)
           (when-let ((new-form (comp-call-optim-form-call
                                 (comp-mvar-constant f) rest)))
             (setf insn new-form)))))))

(defun comp-call-optim (_)
  "Try to optimize out funcall trampoline usage when possible."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        (comp-func-l-p f))
               (let ((comp-func f))
                 (comp-call-optim-func))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Dead code elimination pass specific code.
;; This simple pass try to eliminate insns became useful after propagation.
;; Even if gcc would take care of this is good to perform this here
;; in the hope of removing memory references.
;;
;; This pass can be run as last optim.

(defun comp-collect-mvar-ids (insn)
  "Collect the m-var unique identifiers into INSN."
  (cl-loop for x in insn
           if (consp x)
             append (comp-collect-mvar-ids x)
           else
             when (comp-mvar-p x)
               collect (comp-mvar-id x)))

(defun comp-dead-assignments-func ()
  "Clean-up dead assignments into current function.
Return the list of m-var ids nuked."
  (let ((l-vals ())
        (r-vals ()))
    ;; Collect used r and l-values.
    (cl-loop
     for b being each hash-value of (comp-func-blocks comp-func)
     do (cl-loop
         for insn in (comp-block-insns b)
         for (op arg0 . rest) = insn
         if (comp-set-op-p op)
           do (push (comp-mvar-id arg0) l-vals)
              (setf r-vals (nconc (comp-collect-mvar-ids rest) r-vals))
         else
           do (setf r-vals (nconc (comp-collect-mvar-ids insn) r-vals))))
    ;; Every l-value appearing that does not appear as r-value has no right to
    ;; exist and gets nuked.
    (let ((nuke-list (cl-set-difference l-vals r-vals)))
      (comp-log (format "Function %s\nl-vals %s\nr-vals %s\nNuking ids: %s\n"
                        (comp-func-name comp-func)
                        l-vals
                        r-vals
                        nuke-list)
                3)
      (cl-loop
       for b being each hash-value of (comp-func-blocks comp-func)
       do (comp-loop-insn-in-block b
            (cl-destructuring-bind (op &optional arg0 arg1 &rest rest) insn
              (when (and (comp-set-op-p op)
                         (memq (comp-mvar-id arg0) nuke-list))
                (setf insn
                      (if (comp-limple-insn-call-p arg1)
                          arg1
                        `(comment ,(format "optimized out: %s"
                                           insn))))))))
      nuke-list)))

(defun comp-dead-code (_)
  "Dead code elimination."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        ;; FIXME remove the following condition when tested.
                        (not (comp-func-has-non-local f)))
               (cl-loop
                for comp-func = f
                for i from 1
                while (comp-dead-assignments-func)
                finally (comp-log (format "dead code rm run %d times\n" i) 2)
                (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Tail Call Optimization pass specific code.

(defun comp-form-tco-call-seq (args)
  "Generate a tco sequence for ARGS."
  `(,@(cl-loop for arg in args
               for i from 0
               collect `(set ,(make-comp-mvar :slot i) ,arg))
    (jump bb_0)))

(defun comp-tco-func ()
  "Try to pattern match and perform TCO within the current function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       named in-the-basic-block
       for insns-seq on (comp-block-insns b)
       do (pcase insns-seq
            (`((set ,l-val (direct-call ,func . ,args))
               (comment ,_comment)
               (return ,ret-val))
             (when (and (string= func (comp-func-c-name comp-func))
                        (eq l-val ret-val))
               (let ((tco-seq (comp-form-tco-call-seq args)))
                 (setf (car insns-seq) (car tco-seq)
                       (cdr insns-seq) (cdr tco-seq)
                       (comp-func-ssa-status comp-func) 'dirty)
                 (cl-return-from in-the-basic-block))))))))

(defun comp-tco (_)
  "Simple peephole pass performing self TCO."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 3)
                        (comp-func-l-p f)
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f))
                 (comp-tco-func)
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Type hint removal pass specific code.

;; This must run after all SSA prop not to have the type hint
;; information overwritten.

(defun comp-remove-type-hints-func ()
  "Remove type hints from the current function.
These are substituted with a normal 'set' op."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (comp-loop-insn-in-block b
        (pcase insn
          (`(set ,l-val (call ,(pred comp-type-hint-p) ,r-val))
           (setf insn `(set ,l-val ,r-val)))))))

(defun comp-remove-type-hints (_)
  "Dead code elimination."
  (maphash (lambda (_ f)
             (when (>= (comp-func-speed f) 2)
               (let ((comp-func f))
                 (comp-remove-type-hints-func)
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Final pass specific code.

(defun comp-finalize-container (cont)
  "Finalize data container CONT."
  (setf (comp-data-container-l cont)
        (cl-loop with h = (comp-data-container-idx cont)
                 for obj each hash-keys of h
                 for i from 0
                 do (puthash obj i h)
                 ;; Prune byte-code objects coming from lambdas.
                 ;; These are not anymore necessary as they will be
                 ;; replaced at load time by native-elisp-subrs.
                 ;; Note: we leave the objects in the idx hash table
                 ;; to still be able to retrieve the correct index
                 ;; from the corresponding m-var.
                 collect (if (gethash obj
                                      (comp-ctxt-byte-func-to-func-h comp-ctxt))
                             'lambda-fixup
                           obj))))

(defun comp-finalize-relocs ()
  "Finalize data containers for each relocation class.
Remove immediate duplicates within relocation classes.
Update all insn accordingly."
  ;; Symbols imported by C inlined functions.  We do this here because
  ;; is better to add all objs to the relocation containers before we
  ;; compacting them.
  (mapc #'comp-add-const-to-relocs '(nil t consp listp))

  (let* ((d-default (comp-ctxt-d-default comp-ctxt))
         (d-default-idx (comp-data-container-idx d-default))
         (d-impure (comp-ctxt-d-impure comp-ctxt))
         (d-impure-idx (comp-data-container-idx d-impure))
         (d-ephemeral (comp-ctxt-d-ephemeral comp-ctxt))
         (d-ephemeral-idx (comp-data-container-idx d-ephemeral)))
    ;; We never want compiled lambdas ending up in pure space.  A copy must
    ;; be already present in impure (see `comp-emit-lambda-for-top-level').
    (cl-loop for obj being each hash-keys of d-default-idx
             when (gethash obj (comp-ctxt-lambda-fixups-h comp-ctxt))
               do (cl-assert (gethash obj d-impure-idx))
                  (remhash obj d-default-idx))
    ;; Remove entries in d-impure already present in d-default.
    (cl-loop for obj being each hash-keys of d-impure-idx
             when (gethash obj d-default-idx)
               do (remhash obj d-impure-idx))
    ;; Remove entries in d-ephemeral already present in d-default or
    ;; d-impure.
    (cl-loop for obj being each hash-keys of d-ephemeral-idx
             when (or (gethash obj d-default-idx) (gethash obj d-impure-idx))
               do (remhash obj d-ephemeral-idx))
    ;; Fix-up indexes in each relocation class and fill corresponding
    ;; reloc lists.
    (mapc #'comp-finalize-container (list d-default d-impure d-ephemeral))
    ;; Make a vector from the function documentation hash table.
    (cl-loop with h = (comp-ctxt-function-docs comp-ctxt)
             with v = (make-vector (hash-table-count h) nil)
             for idx being each hash-keys of h
             for doc = (gethash idx h)
             do (setf (aref v idx) doc)
             finally
             do (setf (comp-ctxt-function-docs comp-ctxt) v))
    ;; And now we conclude with the following: We need to pass to
    ;; `comp--register-lambda' the index in the impure relocation
    ;; array to store revived lambdas, but given we know it only now
    ;; we fix it up as last.
    (cl-loop for f being each hash-keys of (comp-ctxt-lambda-fixups-h comp-ctxt)
             using (hash-value mvar)
             with reverse-h = (make-hash-table) ;; Make sure idx is unique.
             for idx = (gethash f d-impure-idx)
             do
             (cl-assert (null (gethash idx reverse-h)))
             (cl-assert (fixnump idx))
             (setf (comp-mvar-constant mvar) idx)
             (puthash idx t reverse-h))))

(defun comp-compile-ctxt-to-file (name)
  "Compile as native code the current context naming it NAME.
Prepare every function for final compilation and drive the C back-end."
  (let ((dir (file-name-directory name)))
    (comp-finalize-relocs)
    (maphash (lambda (_ f)
               (comp-log-func f 1))
             (comp-ctxt-funcs-h comp-ctxt))
    (unless (file-exists-p dir)
      ;; In case it's created in the meanwhile.
      (ignore-error 'file-already-exists
        (make-directory dir t)))
    (unless comp-dry-run
      (comp--compile-ctxt-to-file name))))

(defun comp-final (_)
  "Final pass driving the C back-end for code emission."
  (let (compile-result)
    (comp--init-ctxt)
    (unwind-protect
        (setf compile-result
              (comp-compile-ctxt-to-file (comp-ctxt-output comp-ctxt)))
      (and (comp--release-ctxt)
           compile-result))))


;;; Compiler type hints.
;; These are public entry points be used in user code to give comp suggestion
;; about types.
;; These can be used to implement CL style 'the', 'declare' or something like.
;; Note: types will propagates.
;; WARNING: At speed >= 2 type checking is not performed anymore and suggestions
;; are assumed just to be true. Use with extreme caution...

(defun comp-hint-fixnum (x)
  (unless (fixnump x)
    (signal 'wrong-type-argument x)))

(defun comp-hint-cons (x)
  (unless (consp x)
    (signal 'wrong-type-argument x)))


;; Some entry point support code.

;;;###autoload
(defun comp-clean-up-stale-eln (file)
  "Given FILE remove all the .eln files in `comp-eln-load-path'
sharing the original source filename (including FILE)."
  (string-match (rx "-" (group-n 1 (1+ hex)) "-" (1+ hex) ".eln" eos) file)
  (cl-loop
   with filename-hash = (match-string 1 file)
   with regexp = (rx-to-string
                  `(seq "-" ,filename-hash "-" (1+ hex) ".eln" eos))
   for dir in (butlast comp-eln-load-path) ; Skip last dir.
   do (cl-loop
       with full-dir = (concat dir comp-native-version-dir)
       for f in (when (file-exists-p full-dir)
		  (directory-files full-dir t regexp t))
       do (comp-delete-or-replace-file f))))

(defun comp-delete-or-replace-file (oldfile &optional newfile)
  "Replace OLDFILE with NEWFILE.
When NEWFILE is nil just delete OLDFILE.
Takes the necessary steps when dealing with OLDFILE being a
shared libraries that may be currently loaded by a running Emacs
session."
  (cond ((eq 'windows-nt system-type)
         (ignore-errors (delete-file oldfile))
         (while
             (condition-case _
                 (progn
                   ;; oldfile maybe recreated by another Emacs in
                   ;; between the following two rename-file calls
                   (if (file-exists-p oldfile)
                       (rename-file oldfile (make-temp-file-internal
                                             (file-name-sans-extension oldfile)
                                             nil ".eln.old" nil)
                                    t))
                   (when newfile
                     (rename-file newfile oldfile nil))
                   ;; Keep on trying.
                   nil)
               (file-already-exists
                ;; Done
                t))))
        ;; Remove the old eln instead of copying the new one into it
        ;; to get a new inode and prevent crashes in case the old one
        ;; is currently loaded.
        (t (delete-file oldfile)
           (when newfile
             (rename-file newfile oldfile)))))

(defvar comp-files-queue ()
  "List of Elisp files to be compiled.")

(defvar comp-async-compilations (make-hash-table :test #'equal)
  "Hash table file-name -> async compilation process.")

(defun comp-async-runnings ()
  "Return the number of async compilations currently running.
This function has the side effect of cleaning-up finished
processes from `comp-async-compilations'"
  (cl-loop
   for file-name in (cl-loop
                     for file-name being each hash-key of comp-async-compilations
                     for prc = (gethash file-name comp-async-compilations)
                     unless (process-live-p prc)
                       collect file-name)
   do (remhash file-name comp-async-compilations))
  (hash-table-count comp-async-compilations))

(defvar comp-num-cpus nil)
(defun comp-effective-async-max-jobs ()
  "Compute the effective number of async jobs."
  (if (zerop comp-async-jobs-number)
      (or comp-num-cpus
          (setf comp-num-cpus
                ;; FIXME: we already have a function to determine
                ;; the number of processors, see get_native_system_info in w32.c.
                ;; The result needs to be exported to Lisp.
                (max 1 (/ (cond ((eq 'windows-nt system-type)
                                 (string-to-number (getenv "NUMBER_OF_PROCESSORS")))
                                ((executable-find "nproc")
                                 (string-to-number (shell-command-to-string "nproc")))
                                (t 1))
                          2))))
    comp-async-jobs-number))

(defun comp-run-async-workers ()
  "Start compiling files from `comp-files-queue' asynchronously.
When compilation is finished, run `comp-async-all-done-hook' and
display a message."
  (if (or comp-files-queue
          (> (comp-async-runnings) 0))
      (unless (>= (comp-async-runnings) (comp-effective-async-max-jobs))
        (cl-loop
         for (source-file . load) = (pop comp-files-queue)
         while source-file
         do (cl-assert (string-match-p comp-valid-source-re source-file) nil
                       "`comp-files-queue' should be \".el\" files: %s"
                       source-file)
         when (or comp-always-compile
                  load ; Always compile when the compilation is
                       ; commanded for late load.
                  (file-newer-than-file-p source-file
                                          (comp-el-to-eln-filename source-file)))
         do (let* ((expr `(progn
                            (require 'comp)
                            (setf comp-speed ,comp-speed
                                  comp-debug ,comp-debug
                                  comp-verbose ,comp-verbose
                                  comp-eln-load-path ',comp-eln-load-path
                                  comp-native-driver-options
                                  ',comp-native-driver-options
                                  load-path ',load-path)
                            ,comp-async-env-modifier-form
                            (message "Compiling %s..." ,source-file)
                            (native-compile ,source-file ,(and load t))))
                   (source-file1 source-file) ;; Make the closure works :/
                   (temp-file (make-temp-file
                               (concat "emacs-async-comp-"
                                       (file-name-base source-file) "-")
                               nil ".el"))
                   (expr-string (prin1-to-string expr))
                   (_ (progn
                        (with-temp-file temp-file
                          (insert expr-string))
                        (comp-log "\n")
                        (comp-log expr-string)))
                   (load1 load)
                   (process (make-process
                             :name (concat "Compiling: " source-file)
                             :buffer (get-buffer-create comp-async-buffer-name)
                             :command (list
                                       (expand-file-name invocation-name
                                                         invocation-directory)
                                       "--batch" "-l" temp-file)
                             :sentinel
                             (lambda (process _event)
                               (run-hook-with-args
                                'comp-async-cu-done-hook
                                source-file)
                               (accept-process-output process)
                               (ignore-errors (delete-file temp-file))
                               (when (and load1
                                          (zerop (process-exit-status process)))
                                 (native-elisp-load
                                  (comp-el-to-eln-filename source-file1)
                                  (eq load1 'late)))
                               (comp-run-async-workers)))))
              (puthash source-file process comp-async-compilations))
         when (>= (comp-async-runnings) (comp-effective-async-max-jobs))
           do (cl-return)))
    ;; No files left to compile and all processes finished.
    (let ((msg "Compilation finished."))
      (run-hooks 'comp-async-all-done-hook)
      (with-current-buffer (get-buffer-create comp-async-buffer-name)
        (save-excursion
          (goto-char (point-max))
          (insert msg "\n")))
      ;; `comp-deferred-pending-h' should be empty at this stage.
      ;; Reset it anyway.
      (clrhash comp-deferred-pending-h)
      (message msg))))


;;; Compiler entry points.

;;;###autoload
(defun native-compile (function-or-file &optional with-late-load)
  "Compile FUNCTION-OR-FILE into native code.
This is the entry-point for the Emacs Lisp native compiler.
FUNCTION-OR-FILE is a function symbol or a path to an Elisp file.
When WITH-LATE-LOAD non Nil mark the compilation unit for late load
once finished compiling (internal use only).
Return the compilation unit file name."
  (comp-ensure-native-compiler)
  (unless (or (functionp function-or-file)
              (stringp function-or-file))
    (signal 'native-compiler-error
            (list "Not a function symbol or file" function-or-file)))
  (let* ((data function-or-file)
         (comp-native-compiling t)
         ;; Have byte compiler signal an error when compilation fails.
         (byte-compile-debug t)
         (comp-ctxt
          (make-comp-ctxt
           :output (if (symbolp function-or-file)
                       (make-temp-file (symbol-name function-or-file) nil ".eln")
                     (comp-el-to-eln-filename function-or-file
                                              (when byte-native-for-bootstrap
                                                (car (last comp-eln-load-path)))))
           :with-late-load with-late-load)))
    (comp-log "\n\n" 1)
    (condition-case err
        (mapc (lambda (pass)
                (unless (memq pass comp-disabled-passes)
                  (comp-log (format "(%s) Running pass %s:\n"
                                    function-or-file pass)
                            2)
                  (setf data (funcall pass data))
                  (cl-loop for f in (alist-get pass comp-post-pass-hooks)
                           do (funcall f data))))
              comp-passes)
      (native-compiler-error
       ;; Add source input.
       (let ((err-val (cdr err)))
	 (signal (car err) (if (consp err-val)
			       (cons function-or-file err-val)
			     (list function-or-file err-val))))))
    data))

;;;###autoload
(defun batch-native-compile ()
  "Run `native-compile' on remaining command-line arguments.
Ultra cheap impersonation of `batch-byte-compile'."
  (comp-ensure-native-compiler)
  (cl-loop for file in command-line-args-left
           if (or (null byte-native-for-bootstrap)
                  (cl-notany (lambda (re) (string-match re file))
                             comp-bootstrap-black-list))
           do (native-compile file)
           else
           do (byte-compile-file file)))

;;;###autoload
(defun batch-byte-native-compile-for-bootstrap ()
  "As `batch-byte-compile' but used for booststrap.
Always generate elc files too and handle native compiler expected errors."
  (comp-ensure-native-compiler)
  (if (equal (getenv "NATIVE_DISABLE") "1")
      (batch-byte-compile)
    (cl-assert (= 1 (length command-line-args-left)))
    (let ((byte-native-for-bootstrap t)
          (byte-to-native-output-file nil))
      (unwind-protect
          (condition-case _
              (batch-native-compile)
            (native-compiler-error-dyn-func)
            (native-compiler-error-empty-byte))
        (pcase byte-to-native-output-file
          (`(,tempfile . ,target-file)
           (rename-file tempfile target-file t)))))))

;;;###autoload
(defun native-compile-async (paths &optional recursively load)
  "Compile PATHS asynchronously.
PATHS is one path or a list of paths to files or directories.
`comp-async-jobs-number' specifies the number of (commands) to
run simultaneously.  If RECURSIVELY, recurse into subdirectories
of given directories.
LOAD can be nil t or 'late."
  (comp-ensure-native-compiler)
  (unless (member load '(nil t late))
    (error "LOAD must be nil t or 'late"))
  (unless (listp paths)
    (setf paths (list paths)))
  (let (files)
    (dolist (path paths)
      (cond ((file-directory-p path)
             (dolist (file (if recursively
                               (directory-files-recursively path comp-valid-source-re)
                             (directory-files path t comp-valid-source-re)))
               (push file files)))
            ((file-exists-p path) (push path files))
            (t (signal 'native-compiler-error
                       (list "Path not a file nor directory" path)))))
    (dolist (file files)
      (if-let ((entry (cl-find file comp-files-queue :key #'car :test #'string=)))
          ;; When no load is specified (plain async compilation) we
          ;; consider valid the one previously queued, otherwise we
          ;; check for coherence (bug#40602).
          (cl-assert (or (null load)
                         (eq load (cdr entry)))
                     nil "Trying to queue %s with LOAD %s but this is already \
queued with LOAD %"
                     file load (cdr entry))
        ;; Make sure we are not already compiling `file' (bug#40838).
        (unless (or (gethash file comp-async-compilations)
                    ;; Also exclude files from deferred compilation if
                    ;; any of the regexps in
                    ;; `comp-deferred-compilation-black-list' matches.
                    (and (eq load 'late)
                         (cl-some (lambda (re) (string-match re file))
                                  comp-deferred-compilation-black-list)))
          (let* ((out-filename (comp-el-to-eln-filename file))
                 (out-dir (file-name-directory out-filename)))
            (unless (file-exists-p out-dir)
              (make-directory out-dir t))
            (if (file-writable-p out-filename)
                (setf comp-files-queue
                      (append comp-files-queue `((,file . ,load))))
              (display-warning 'comp
                               (format "No write access for %s skipping."
                                       out-filename)))))))
    (when (zerop (comp-async-runnings))
      (comp-run-async-workers)
      (message "Compilation started."))))

(provide 'comp)

;;; comp.el ends here
