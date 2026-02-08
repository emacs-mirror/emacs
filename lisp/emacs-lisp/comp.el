;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>
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
(require 'cl-lib)
(require 'gv)
(require 'rx)
(require 'subr-x)
(require 'warnings)
(require 'comp-common)
(require 'comp-cstr)

;; These variables and functions are defined in comp.c
(defvar comp-native-version-dir)
(defvar comp-subr-arities-h)
(defvar native-comp-eln-load-path)
(defvar native-comp-enable-subr-trampolines)
(defvar comp--\#$)

(declare-function comp--compile-ctxt-to-file0 "comp.c")
(declare-function comp--init-ctxt "comp.c")
(declare-function comp--release-ctxt "comp.c")
(declare-function comp-el-to-eln-filename "comp.c")
(declare-function comp-el-to-eln-rel-filename "comp.c")
(declare-function native-elisp-load "comp.c")

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defcustom native-comp-speed 2
  "Optimization level for native compilation, a number between -1 and 3.
 -1 functions are kept in bytecode form and no native compilation is performed
    (but *.eln files are still produced, and include the compiled code in
    bytecode form).
  0 native compilation is performed with no optimizations.
  1 light optimizations.
  2 max optimization level fully adherent to the language semantic.
  3 max optimization level, to be used only when necessary.
    Warning: with 3, the compiler is free to perform dangerous optimizations."
  :type 'integer
  :safe #'integerp
  :version "28.1")

(defcustom native-comp-debug 0
  "Debug level for native compilation, a number between 0 and 3.
This is intended for debugging the compiler itself.
  0 no debug output.
  1 emit debug symbols.
  2 emit debug symbols and dump pseudo C code.
  3 emit debug symbols and dump: pseudo C code, GCC intermediate
  passes and libgccjit log file.
When generated, the pseudo C code is deposited in the same directory
as the corresponding .eln file."
  :type 'natnum
  :safe #'natnump
  :version "29.1")

(defcustom native-comp-bootstrap-deny-list
  '()
  "List of regexps to exclude files from native compilation during bootstrap.
Files whose names match any regexp are excluded from native compilation
during bootstrap."
  :type '(repeat regexp)
  :version "28.1")

(defcustom native-comp-compiler-options nil
  "Command line options passed verbatim to GCC compiler.
Note that not all options are meaningful and some options might even
break your Emacs.  Use at your own risk.

Passing these options is only available in libgccjit version 9
and above."
  :type '(repeat string)
  :version "28.1")

(defcustom native-comp-driver-options
  (cond ((eq system-type 'darwin) '("-Wl,-w"))
        ((eq system-type 'cygwin) '("-Wl,-dynamicbase")))
  "Options passed verbatim to the native compiler's back-end driver.
Note that not all options are meaningful; typically only the options
affecting the assembler and linker are likely to be useful.

Passing these options is only available in libgccjit version 9
and above."
  :type '(repeat string)
  :version "28.1")

(defcustom comp-libgccjit-reproducer nil
  "When non-nil produce a libgccjit reproducer.
The reproducer is a file ELNFILENAME_libgccjit_repro.c deposed in
the .eln output directory."
  :type 'boolean
  :version "28.1")

(defcustom native-comp-warning-on-missing-source t
  "Emit a warning if a byte-code file being loaded has no corresponding source.
The source file is necessary for native code file look-up and deferred
compilation mechanism."
  :type 'boolean
  :version "28.1")

(defvar no-native-compile nil
  "Non-nil to prevent native-compiling of Emacs Lisp code.
Note that when `no-byte-compile' is set to non-nil it overrides the value of
`no-native-compile'.
This is normally set in local file variables at the end of the
Emacs Lisp file:

\;; Local Variables:\n;; no-native-compile: t\n;; End:")
;;;###autoload(put 'no-native-compile 'safe-local-variable 'booleanp)

(defvar native-compile-target-directory nil
  "When non-nil force the target directory for the eln files being compiled.")

(defvar comp-log-time-report nil
  "If non-nil, log a time report for each pass.")

(defvar comp-dry-run nil
  "If non-nil, run everything but the C back-end.")

(defvar comp-native-compiling nil
  "This gets bound to t during native compilation.
Intended to be used by code that needs to work differently when
native compilation runs.")

(defvar comp-pass nil
  "Every native-compilation pass can bind this to whatever it likes.")

(defvar comp-curr-allocation-class 'd-default
  "Current allocation class.
Can be one of: `d-default' or `d-ephemeral'.  See `comp-ctxt'.")

(defconst comp-passes '(comp--spill-lap
                        comp--limplify
                        comp--fwprop
                        comp--call-optim
                        comp--ipa-pure
                        comp--add-cstrs
                        comp--fwprop
                        comp--type-check-optim
                        comp--tco
                        comp--fwprop
                        comp--remove-type-hints
                        comp--sanitizer
                        comp--compute-function-types
                        comp--final)
  "Passes to be executed in order.")

(defvar comp-disabled-passes '()
  "List of disabled passes.
For internal use by the test suite only.")

(defvar comp-post-pass-hooks '()
  "Alist whose elements are of the form (PASS FUNCTIONS...).
Each function in FUNCTIONS is run after PASS.
Useful to hook into pass checkers.")

(defconst comp-primitive-func-cstr-h
  (cl-loop
   with comp-ctxt = (make-comp-cstr-ctxt)
   with h = (make-hash-table :test #'eq)
   for (f type-spec) in comp-primitive-type-specifiers
   for cstr = (comp-type-spec-to-cstr type-spec)
   do (puthash f cstr h)
   finally return h)
  "Hash table function -> `comp-constraint'.")

(defsubst comp--symbol-func-to-fun (symbol-func)
  "Given a function called SYMBOL-FUNC return its `comp-func'."
  (gethash (gethash symbol-func (comp-ctxt-sym-to-c-name-h comp-ctxt))
           (comp-ctxt-funcs-h comp-ctxt)))

(defun comp--get-function-cstr (function)
  "Given FUNCTION return the corresponding `comp-constraint'."
  (when (symbolp function)
    (or (gethash function comp-primitive-func-cstr-h)
        (when-let* ((type (or (when-let* ((f (comp--symbol-func-to-fun function)))
                                (comp-func-declared-type f))
                              (function-get function 'function-type))))
          (comp-type-spec-to-cstr type)))))

;; Keep it in sync with the `cl-deftype-satisfies' property set in
;; cl-macs.el. We can't use `cl-deftype-satisfies' directly as the
;; relation type <-> predicate is not bijective (bug#45576).
(defconst comp-known-predicates
  ;; FIXME: Auto-generate (most of) it from `cl-deftype-satisfies'?
  '((arrayp              array)
    (atom		 atom)
    (bool-vector-p       bool-vector)
    (booleanp            boolean)
    (bufferp             buffer)
    (char-table-p	 char-table)
    (characterp          fixnum t)
    (consp               cons)
    (floatp              float)
    (framep              frame)
    (functionp           (or function symbol cons) (not function))
    (hash-table-p	 hash-table)
    (integer-or-marker-p integer-or-marker)
    (integerp            integer)
    (keywordp            symbol t)
    (listp               list)
    (markerp             marker)
    (natnump             (integer 0 *))
    (null		 null)
    (number-or-marker-p  number-or-marker)
    (numberp             number)
    (obarrayp            obarray)
    (overlayp            overlay)
    (processp            process)
    (sequencep           sequence)
    (stringp             string)
    (subrp               subr)
    (symbol-with-pos-p   symbol-with-pos)
    (symbolp             symbol)
    (vectorp             vector)
    (windowp             window))
  "(PREDICATE TYPE-IF-SATISFIED ?TYPE-IF-NOT-SATISFIED).")

(defconst comp-known-predicates-h
  (cl-loop
   with comp-ctxt = (make-comp-cstr-ctxt)
   with h = (make-hash-table :test #'eq)
   for (pred . type-specs) in comp-known-predicates
   for pos-cstr = (comp-type-spec-to-cstr (car type-specs))
   for neg-cstr = (if (length> type-specs 1)
                      (comp-type-spec-to-cstr (cl-second type-specs))
                    (comp-cstr-negation-make pos-cstr))
   do (puthash pred (cons pos-cstr neg-cstr) h)
   finally return h)
  "Hash table FUNCTION -> (POS-CSTR . NEG-CSTR).")

(defun comp--known-predicate-p (predicate)
  "Return t if PREDICATE is known."
  (when (or (gethash predicate comp-known-predicates-h)
            (gethash predicate (comp-cstr-ctxt-pred-type-h comp-ctxt)))
    t))

(defun comp--pred-to-pos-cstr (predicate)
  "Given PREDICATE, return the corresponding positive constraint."
  (or (car-safe (gethash predicate comp-known-predicates-h))
      (gethash predicate (comp-cstr-ctxt-pred-type-h comp-ctxt))))

(defun comp--pred-to-neg-cstr (predicate)
  "Given PREDICATE, return the corresponding negative constraint."
  (or (cdr-safe (gethash predicate comp-known-predicates-h))
      (gethash predicate (comp-cstr-ctxt-pred-type-h comp-ctxt))))

(defconst comp-symbol-values-optimizable '(most-positive-fixnum
                                           most-negative-fixnum)
  "Symbol values we can resolve at compile-time.")

(defconst comp-type-hints '(comp-hint-fixnum
                            comp-hint-cons)
  "List of fake functions used to give compiler hints.")

(defvar comp-func nil
  "Bound to the current function by most passes.")

(defvar comp-block nil
  "Bound to the current basic block by some passes.")

(define-error 'native-compiler-error-dyn-func
  "can't native compile a non-lexically-scoped function"
  'native-compiler-error)
(define-error 'native-compiler-error-empty-byte
  "empty byte compiler output"
  'native-compiler-error)


(defvar comp-no-spawn nil
  "Non-nil don't spawn native compilation processes.")


(cl-defstruct (comp-vec (:copier nil))
  "A re-sizable vector like object."
  (data (make-hash-table :test #'eql) :type hash-table
        :documentation "Payload data.")
  (beg 0 :type integer)
  (end 0 :type natnum))

(defsubst comp-vec-copy (vec)
  "Return a copy of VEC."
  (make-comp-vec :data (copy-hash-table (comp-vec-data vec))
                 :beg (comp-vec-beg vec)
                 :end (comp-vec-end vec)))

(defsubst comp-vec-length (vec)
  "Return the number of elements of VEC."
  (- (comp-vec-end vec) (comp-vec-beg vec)))

(defsubst comp-vec--verify-idx (vec idx)
  "Check whether IDX is in bounds for VEC."
  (cl-assert (and (< idx (comp-vec-end vec))
                  (>= idx (comp-vec-beg vec)))))

(defsubst comp-vec-aref (vec idx)
  "Return the element of VEC whose index is IDX."
  (declare (gv-setter (lambda (val)
                        `(comp-vec--verify-idx ,vec ,idx)
                        `(puthash ,idx ,val (comp-vec-data ,vec)))))
  (comp-vec--verify-idx vec idx)
  (gethash idx (comp-vec-data vec)))

(defsubst comp-vec-append (vec elt)
  "Append ELT into VEC.
Returns ELT."
  (puthash (comp-vec-end vec) elt (comp-vec-data vec))
  (incf (comp-vec-end vec))
  elt)

(defsubst comp-vec-prepend (vec elt)
  "Prepend ELT into VEC.
Returns ELT."
  (puthash (1- (comp-vec-beg vec)) elt (comp-vec-data vec))
  (decf (comp-vec-beg vec))
  elt)



(eval-when-compile
  (defconst comp-op-stack-info
    (cl-loop with h = (make-hash-table)
	     for k across byte-code-vector
	     for v across byte-stack+-info
	     when k
	     do (puthash k v h)
	     finally return h)
    "Hash table lap-op -> stack adjustment."))

(define-hash-table-test 'comp-imm-equal-test #'equal-including-properties
  #'sxhash-equal-including-properties)

(cl-defstruct comp-data-container
  "Data relocation container structure."
  (l () :type list
     :documentation "Constant objects used by functions.")
  (idx (make-hash-table :test 'comp-imm-equal-test) :type hash-table
       :documentation "Obj -> position into the previous field."))

(cl-defstruct (comp-ctxt (:include comp-cstr-ctxt))
  "Lisp side of the compiler context."
  (output nil :type string
          :documentation "Target output file-name for the compilation.")
  (speed native-comp-speed :type number
         :documentation "Default speed for this compilation unit.")
  (safety compilation-safety :type number
         :documentation "Default safety level for this compilation unit.")
  (debug native-comp-debug :type number
         :documentation "Default debug level for this compilation unit.")
  (compiler-options native-comp-compiler-options :type list
                    :documentation "Options for the GCC compiler.")
  (driver-options native-comp-driver-options :type list
         :documentation "Options for the GCC driver.")
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
  (d-ephemeral (make-comp-data-container) :type comp-data-container
               :documentation "Relocated data not necessary after load.")
  (non-materializable-objs-h (make-hash-table :test #'equal) :type hash-table
               :documentation "Objects produced by the propagation engine which can't be materialized.
Typically floating points (which are not cons-hashed).")
  (with-late-load nil :type boolean
                  :documentation "When non-nil support late load."))

(cl-defstruct comp-args-base
  (min nil :type integer
       :documentation "Minimum number of arguments allowed."))

(cl-defstruct (comp-args (:include comp-args-base))
  (max nil :type integer
       :documentation "Maximum number of arguments allowed."))

(cl-defstruct (comp-nargs (:include comp-args-base))
  "Describe args when the function signature is of kind:
(ptrdiff_t nargs, Lisp_Object *args)."
  (nonrest nil :type integer
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
  ;; All the following are for SSA and CGF analysis.
  ;; Keep in sync with `comp--clean-ssa'!!
  (in-edges () :type list
            :documentation "List of incoming edges.")
  (out-edges () :type list
             :documentation "List of out-coming edges.")
  (idom nil :type (or null comp-block)
        :documentation "Immediate dominator.")
  (df (make-hash-table) :type (or null hash-table)
      :documentation "Dominance frontier set. Block-name -> block")
  (post-num nil :type (or null number)
            :documentation "Post order number.")
  (final-frame nil :type (or null comp-vec)
             :documentation "This is a copy of the frame when leaving the block.
Is in use to help the SSA rename pass."))

(cl-defstruct (comp-block-lap (:copier nil)
                              (:include comp-block)
                              (:constructor make--comp-block-lap
                                            (addr sp name))) ; Positional
  "A basic block created from lap (real code)."
  ;; These two slots are used during limplification.
  (sp nil :type number
      :documentation "When non-nil indicates the sp value while entering
into it.")
  (addr nil :type number
        :documentation "Start block LAP address.")
  (non-ret-insn nil :type list
                :documentation "Insn known to perform a non local exit.
`comp--fwprop' may identify and store here basic blocks performing
non local exits and mark it rewrite it later.")
  (no-ret nil :type boolean
         :documentation "t when the block is known to perform a
non local exit (ends with an `unreachable' insn)."))

(cl-defstruct (comp-latch (:copier nil)
                          (:include comp-block))
  "A basic block for a latch loop.")

(cl-defstruct (comp-block-cstr (:copier nil)
                               (:include comp-block))
  "A basic block holding only constraints.")

(cl-defstruct (comp-edge (:copier nil) (:constructor comp--edge-make0))
  "An edge connecting two basic blocks."
  (src nil :type (or null comp-block))
  (dst nil :type (or null comp-block))
  (number nil :type number
          :documentation "The index number corresponding to this edge in the
 edge hash."))

(defun comp--edge-make (&rest args)
  "Create a `comp-edge' with basic blocks SRC and DST."
  (let ((n (funcall (comp-func-edge-cnt-gen comp-func))))
    (puthash
     n
     (apply #'comp--edge-make0 :number n args)
     (comp-func-edges-h comp-func))))

(defun comp--block-preds (basic-block)
  "Return the list of predecessors of BASIC-BLOCK."
  (mapcar #'comp-edge-src (comp-block-in-edges basic-block)))

(defun comp--gen-counter ()
  "Return a sequential number generator."
  (let ((n -1))
    (lambda ()
      (incf n))))

(cl-defstruct (comp-func (:copier nil))
  "LIMPLE representation of a function."
  (name nil :type symbol
        :documentation "Function symbol name. Nil indicates anonymous.")
  (c-name nil :type string
          :documentation "The function name in the native world.")
  (byte-func nil
             :documentation "Byte-compiled version.")
  (doc nil :type string
       :documentation "Doc string.")
  (int-spec nil :type list
            :documentation "Interactive form.")
  (command-modes nil :type list
                 :documentation "Command modes.")
  (lap () :type list
       :documentation "LAP assembly representation.")
  (ssa-status nil :type symbol
       :documentation "SSA status either: nil, `dirty' or t.
Once in SSA form this *must* be set to `dirty' every time the topology of the
CFG is mutated by a pass.")
  (frame-size nil :type integer)
  (vframe-size 0 :type integer)
  (blocks (make-hash-table :test #'eq) :type hash-table
          :documentation "Basic block symbol -> basic block.")
  (lap-block (make-hash-table :test #'equal) :type hash-table
             :documentation "LAP label -> LIMPLE basic block name.")
  (edges-h (make-hash-table) :type hash-table
         :documentation "Hash edge-num -> edge connecting basic two blocks.")
  (block-cnt-gen (funcall #'comp--gen-counter) :type function
                 :documentation "Generates block numbers.")
  (edge-cnt-gen (funcall #'comp--gen-counter) :type function
                :documentation "Generates edges numbers.")
  (has-non-local nil :type boolean
                 :documentation "t if non local jumps are present.")
  (speed nil :type number
         :documentation "Optimization level (see `native-comp-speed').")
  (safety nil :type number
         :documentation "Safety level (see `safety').")
  (pure nil :type boolean
        :documentation "t if pure nil otherwise.")
  (declared-type nil :type list
        :documentation "Declared function type.")
  (type nil :type (or null comp-mvar)
        :documentation "Mvar holding the derived return type."))

(cl-defstruct (comp-func-l (:include comp-func))
  "Lexically-scoped function."
  (args nil :type comp-args-base
        :documentation "Argument specification of the function"))

(cl-defstruct (comp-func-d (:include comp-func))
  "Dynamically-scoped function."
  (lambda-list nil :type list
        :documentation "Original lambda-list."))

(cl-defstruct (comp-mvar (:constructor make--comp-mvar0)
                         (:include comp-cstr))
  "A meta-variable being a slot in the meta-stack."
  (id nil :type (or null number)
      :documentation "Unique id when in SSA form.")
  (slot nil :type (or fixnum symbol)
        :documentation "Slot number in the array if a number or
        `scratch' for scratch slot."))

;; In use by comp.c.
(defun comp-mvar-type-hint-match-p (mvar type-hint)
  "Match MVAR against TYPE-HINT.
In use by the back-end."
  (cl-ecase type-hint
    (cons (comp-cstr-cons-p mvar))
    (fixnum (comp-cstr-fixnum-p mvar))))



(defun comp--equality-fun-p (function)
  "Equality functions predicate for FUNCTION."
  (when (memq function '(eq eql equal)) t))

(defun comp--arithm-cmp-fun-p (function)
  "Predicate for arithmetic comparison functions."
  (when (memq function '(= > < >= <=)) t))

(defun comp--set-op-p (op)
  "Assignment predicate for OP."
  (when (memq op comp-limple-sets) t))

(defun comp--assign-op-p (op)
  "Assignment predicate for OP."
  (when (memq op comp-limple-assignments) t))

(defun comp--call-op-p (op)
  "Call predicate for OP."
  (when (memq op comp-limple-calls) t))

(defun comp--branch-op-p (op)
  "Branch predicate for OP."
  (when (memq op comp-limple-branches) t))

(defsubst comp--limple-insn-call-p (insn)
  "Limple INSN call predicate."
  (comp--call-op-p (car-safe insn)))

(defun comp--type-hint-p (func)
  "Type-hint predicate for function name FUNC."
  (when (memq func comp-type-hints) t))

(defun comp--func-unique-in-cu-p (func)
  "Return t if FUNC is known to be unique in the current compilation unit."
  (if (symbolp func)
      (cl-loop with h = (make-hash-table :test #'eq)
               for f being the hash-value in (comp-ctxt-funcs-h comp-ctxt)
               for name = (comp-func-name f)
               when (gethash name h)
                 return nil
               do (puthash name t h)
               finally return t)
    t))

(defun comp--function-pure-p (f)
  "Return t if F is pure."
  (or (get f 'pure)
      (when-let* ((func (comp--symbol-func-to-fun f)))
        (comp-func-pure func))))

(defun comp--alloc-class-to-container (alloc-class)
  "Given ALLOC-CLASS, return the data container for the current context.
Assume allocation class `d-default' as default."
  (cl-struct-slot-value 'comp-ctxt (or alloc-class 'd-default) comp-ctxt))

(defsubst comp--add-const-to-relocs (obj)
  "Keep track of OBJ into the ctxt relocations."
  (puthash obj t (comp-data-container-idx (comp--alloc-class-to-container
                                           comp-curr-allocation-class))))


;;; Log routines.

(defun comp--prettyformat-mvar (mvar)
  (format "#(mvar %s %s %S)"
          (comp-mvar-id mvar)
          (comp-mvar-slot mvar)
          (comp-cstr-to-type-spec mvar)))

(defun comp--prettyformat-insn (insn)
  (cond
   ((comp-mvar-p insn)
    (comp--prettyformat-mvar insn))
   ((proper-list-p insn)
    (concat "(" (mapconcat #'comp--prettyformat-insn insn " ") ")"))
   (t (prin1-to-string insn))))

(defun comp--log-func (func verbosity)
  "Log function FUNC at VERBOSITY.
VERBOSITY is a number between 0 and 3."
  (when (>= native-comp-verbose verbosity)
    (comp-log (format "\nFunction: %s\n" (comp-func-name func)) verbosity)
    (cl-loop
     for block-name being each hash-keys of (comp-func-blocks func)
     using (hash-value bb)
     do (comp-log (concat "<" (symbol-name block-name) ">") verbosity)
        (cl-loop
         for insn in (comp-block-insns bb)
         do (comp-log (comp--prettyformat-insn insn) verbosity)))))

(defun comp--log-edges (func)
  "Log edges in FUNC."
  (let ((edges (comp-func-edges-h func)))
    (comp-log (format "\nEdges in function: %s\n"
                      (comp-func-name func))
              2)
    (maphash (lambda (_ e)
               (comp-log (format "n: %d src: %s dst: %s\n"
                                 (comp-edge-number e)
                                 (comp-block-name (comp-edge-src e))
                                 (comp-block-name (comp-edge-dst e)))
                         2))
          edges)))



(defmacro comp--loop-insn-in-block (basic-block &rest body)
  "Loop over all insns in BASIC-BLOCK executing BODY.
Inside BODY, `insn' and `insn-cell'can be used to read or set the
current instruction or its cell."
  (declare (debug (form body))
           (indent defun))
  `(cl-symbol-macrolet ((insn (car insn-cell)))
     (let ((insn-cell (comp-block-insns ,basic-block)))
       (while insn-cell
         ,@body
         (setf insn-cell (cdr insn-cell))))))

;;; spill-lap pass specific code.

(defun comp--lex-byte-func-p (f)
  "Return t if F is a lexically-scoped byte compiled function."
  (and (byte-code-function-p f)
       (fixnump (aref f 0))))

(defun comp--spill-decl-spec (function-name spec)
  "Return the declared specifier SPEC for FUNCTION-NAME."
  (plist-get (cdr (assq function-name byte-to-native-plist-environment))
             spec))

(defun comp--spill-speed (function-name)
  "Return the speed for FUNCTION-NAME."
  (or (comp--spill-decl-spec function-name 'speed)
      (comp-ctxt-speed comp-ctxt)))

(defun comp--spill-safety (function-name)
  "Return the safety level for FUNCTION-NAME."
  (or (comp--spill-decl-spec function-name 'safety)
      (comp-ctxt-safety comp-ctxt)))

;; Autoloaded as might be used by `disassemble-internal'.
;;;###autoload
(defun comp-c-func-name (name prefix &optional first)
  "Given NAME, return a name suitable for the native code.
Add PREFIX in front of it.  If FIRST is not nil, pick the first
available name ignoring compilation context and potential name
clashes."
  ;; Unfortunately not all symbol names are valid as C function names...
  ;; Nassi's algorithm here:
  (let* ((orig-name (if (symbolp name) (symbol-name name) name))
         (crypted (cl-loop with str = (make-string (* 2 (length orig-name)) 0)
	                   for j from 0 by 2
	                   for i across orig-name
	                   for byte = (format "%x" i)
	                   do (aset str j (aref byte 0))
			      (aset str (1+ j) (if (length> byte 1)
						   (aref byte 1)
						 ?\_))
	                   finally return str))
         (human-readable (string-replace
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

(defun comp--decrypt-arg-list (x function-name)
  "Decrypt argument list X for FUNCTION-NAME."
  (unless (fixnump x)
    (signal 'native-compiler-error-dyn-func (list function-name)))
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

(defsubst comp--byte-frame-size (byte-compiled-func)
  "Return the frame size to be allocated for BYTE-COMPILED-FUNC."
  (aref byte-compiled-func 3))

(defun comp--add-func-to-ctxt (func)
  "Add FUNC to the current compiler context."
  (let ((name (comp-func-name func))
        (c-name (comp-func-c-name func)))
    (puthash name c-name (comp-ctxt-sym-to-c-name-h comp-ctxt))
    (puthash c-name func (comp-ctxt-funcs-h comp-ctxt))))

(cl-defgeneric comp--spill-lap-function (input)
  "Byte-compile INPUT and spill lap for further stages.")

(cl-defmethod comp--spill-lap-function ((function-name symbol))
  "Byte-compile FUNCTION-NAME, spilling data from the byte compiler."
  (unless (comp-ctxt-output comp-ctxt)
    (setf (comp-ctxt-output comp-ctxt)
          (make-temp-file (comp-c-func-name function-name "freefn-")
                          nil ".eln")))
  (let* ((f (symbol-function function-name))
         (byte-code (byte-compile function-name))
         (c-name (comp-c-func-name function-name "F")))
      (when (byte-code-function-p f)
        (signal 'native-compiler-error
                '("can't native compile an already byte-compiled function")))
        (setf (comp-ctxt-top-level-forms comp-ctxt)
              (list (make-byte-to-native-func-def :name function-name
                                                  :c-name c-name
                                                  :byte-func byte-code)))
      (maphash #'comp--intern-func-in-ctxt byte-to-native-lambdas-h)))

(defun comp--spill-lap-single-function (function)
  "Byte-compile FUNCTION, spilling data from the byte compiler."
  (unless (comp-ctxt-output comp-ctxt)
    (setf (comp-ctxt-output comp-ctxt)
          (make-temp-file "comp-lambda-" nil ".eln")))
  (let* ((byte-code (byte-compile function))
         (c-name (comp-c-func-name "anonymous-lambda" "F")))
    (setf (comp-ctxt-top-level-forms comp-ctxt)
          (list (make-byte-to-native-func-def :name '--anonymous-lambda
                                              :c-name c-name
                                              :byte-func byte-code)))
    (maphash #'comp--intern-func-in-ctxt byte-to-native-lambdas-h)))

(cl-defmethod comp--spill-lap-function ((form list))
  "Byte-compile FORM, spilling data from the byte compiler."
  (unless (eq (car-safe form) 'lambda)
    (signal 'native-compiler-error
            '("Cannot native-compile, form is not a lambda")))
  (comp--spill-lap-single-function form))

(cl-defmethod comp--spill-lap-function ((fun interpreted-function))
  "Spill data from the byte compiler for the interpreted-function FUN."
  (comp--spill-lap-single-function fun))

(defun comp--intern-func-in-ctxt (_ obj)
  "Given OBJ of type `byte-to-native-lambda', create a function in `comp-ctxt'."
  (when-let* ((byte-func (byte-to-native-lambda-byte-func obj)))
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
           (func (if (comp--lex-byte-func-p byte-func)
                     (make-comp-func-l
                      :args (comp--decrypt-arg-list (aref byte-func 0)
                                                   name))
                   (make-comp-func-d :lambda-list (aref byte-func 0)))))
      (setf (comp-func-name func) name
            (comp-func-byte-func func) byte-func
            (comp-func-doc func) (documentation byte-func t)
            (comp-func-int-spec func) (interactive-form byte-func)
            (comp-func-command-modes func) (command-modes byte-func)
            (comp-func-c-name func) c-name
            (comp-func-lap func) lap
            (comp-func-frame-size func) (comp--byte-frame-size byte-func)
            (comp-func-speed func) (comp--spill-speed name)
            (comp-func-safety func) (comp--spill-safety name)
            (comp-func-declared-type func) (comp--spill-decl-spec name 'function-type)
            (comp-func-pure func) (comp--spill-decl-spec name 'pure))

      ;; Store the c-name to have it retrievable from
      ;; `comp-ctxt-top-level-forms'.
      (when top-l-form
        (setf (byte-to-native-func-def-c-name top-l-form) c-name))
      (unless name
        (puthash byte-func func (comp-ctxt-byte-func-to-func-h comp-ctxt)))
      (comp--add-func-to-ctxt func)
      (comp-log (format "Function %s:\n" name) 1)
      (comp-log lap 1 t))))

(cl-defmethod comp--spill-lap-function ((filename string))
  "Byte-compile FILENAME, spilling data from the byte compiler."
  (byte-compile-file filename)
  (when (or (null byte-native-qualities)
            (alist-get 'no-native-compile byte-native-qualities))
    (throw 'no-native-compile nil))
  (unless byte-to-native-top-level-forms
    (signal 'native-compiler-error-empty-byte (list filename)))
  (unless (comp-ctxt-output comp-ctxt)
    (setf (comp-ctxt-output comp-ctxt)
          (comp-el-to-eln-filename filename native-compile-target-directory)))
  (setf (comp-ctxt-speed comp-ctxt) (alist-get 'native-comp-speed
                                               byte-native-qualities)
        (comp-ctxt-safety comp-ctxt) (alist-get 'compilation-safety
                                                byte-native-qualities)
        (comp-ctxt-debug comp-ctxt) (alist-get 'native-comp-debug
                                               byte-native-qualities)
        (comp-ctxt-compiler-options comp-ctxt) (alist-get 'native-comp-compiler-options
                                                        byte-native-qualities)
        (comp-ctxt-driver-options comp-ctxt) (alist-get 'native-comp-driver-options
                                                        byte-native-qualities)
        (comp-ctxt-top-level-forms comp-ctxt)
        (cl-loop
         for form in (reverse byte-to-native-top-level-forms)
         collect
         (if (and (byte-to-native-func-def-p form)
                  (eq -1
                      (comp--spill-speed (byte-to-native-func-def-name form))))
             (let ((byte-code (byte-to-native-func-def-byte-func form)))
               (remhash byte-code byte-to-native-lambdas-h)
               (make-byte-to-native-top-level
                :form `(defalias
                         ',(byte-to-native-func-def-name form)
                         ,byte-code
                         nil)
                :lexical (comp--lex-byte-func-p byte-code)))
           form)))
  (maphash #'comp--intern-func-in-ctxt byte-to-native-lambdas-h))

(defun comp--spill-lap (input)
  "Byte-compile and spill the LAP representation for INPUT.
If INPUT is a symbol, it is the function-name to be compiled.
If INPUT is a string, it is the filename to be compiled."
  (let* ((byte-native-compiling t)
         (byte-to-native-lambdas-h (make-hash-table :test #'eq))
         (byte-to-native-top-level-forms ())
         (byte-to-native-plist-environment ())
         (res (comp--spill-lap-function input)))
    (comp-cstr-ctxt-update-type-slots comp-ctxt)
    res))


;;; Limplification pass specific code.

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during function limplification."
  (frame nil :type (or null comp-vec)
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

(defun comp--lap-eob-p (inst)
  "Return t if INST closes the current basic blocks, nil otherwise."
  (when (memq (car inst) comp-lap-eob-ops)
    t))

(defun comp--lap-fall-through-p (inst)
  "Return t if INST falls through, nil otherwise."
  (when (not (memq (car inst) '(byte-goto byte-return)))
    t))

(defsubst comp--sp ()
  "Current stack pointer."
  (declare (gv-setter (lambda (val)
                        `(setf (comp-limplify-sp comp-pass) ,val))))
  (comp-limplify-sp comp-pass))

(defmacro comp--with-sp (sp &rest body)
  "Execute BODY setting the stack pointer to SP.
Restore the original value afterwards."
  (declare (debug (form body))
           (indent defun))
  (cl-with-gensyms (sym)
    `(let ((,sym (comp--sp)))
       (setf (comp--sp) ,sp)
       (progn ,@body)
       (setf (comp--sp) ,sym))))

(defsubst comp--slot-n (n)
  "Slot N into the meta-stack."
  (comp-vec-aref (comp-limplify-frame comp-pass) n))

(defsubst comp--slot ()
  "Current slot into the meta-stack pointed by sp."
  (comp--slot-n (comp--sp)))

(defsubst comp--slot+1 ()
  "Slot into the meta-stack pointed by sp + 1."
  (comp--slot-n (1+ (comp--sp))))

(defsubst comp--label-to-addr (label)
  "Find the address of LABEL."
  (or (gethash label (comp-limplify-label-to-addr comp-pass))
      (signal 'native-ice (list "label not found" label))))

(defsubst comp--mark-curr-bb-closed ()
  "Mark the current basic block as closed."
  (setf (comp-block-closed (comp-limplify-curr-block comp-pass)) t))

(defun comp--bb-maybe-add (lap-addr &optional sp)
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
      (car (push (make--comp-block-lap lap-addr sp (comp--new-block-sym))
                 (comp-limplify-pending-blocks comp-pass))))))

(defsubst comp--call (func &rest args)
  "Emit a call for function FUNC with ARGS."
  `(call ,func ,@args))

(defun comp--callref (func nargs stack-off)
  "Emit a call using narg abi for FUNC.
NARGS is the number of arguments.
STACK-OFF is the index of the first slot frame involved."
  `(callref ,func ,@(cl-loop repeat nargs
                             for sp from stack-off
                             collect (comp--slot-n sp))))

(cl-defun make--comp-mvar (&key slot (constant nil const-vld) type neg)
  "`comp-mvar' initializer."
  (let ((mvar (make--comp-mvar0 :slot slot)))
    (when const-vld
      (comp--add-const-to-relocs constant)
      (setf (comp-cstr-imm mvar) constant))
    (when type
      (setf (comp-mvar-typeset mvar) (list type)))
    (when neg
      (setf (comp-mvar-neg mvar) t))
    mvar))

(defun comp--new-frame (size vsize &optional ssa)
  "Return a clean frame of meta variables of size SIZE and VSIZE.
If SSA is non-nil, populate it with m-var in ssa form."
  (cl-loop with v = (make-comp-vec :beg (- vsize) :end size)
           for i from (- vsize) below size
           for mvar = (if ssa
                          (make--comp--ssa-mvar :slot i)
                        (make--comp-mvar :slot i))
           do (setf (comp-vec-aref v i) mvar)
           finally return v))

(defun comp--emit (insn)
  "Emit INSN into basic block BB."
  (let ((bb (comp-limplify-curr-block comp-pass)))
    (cl-assert (not (comp-block-closed bb)))
    (push insn (comp-block-insns bb))))

(defun comp--emit-set-call (call)
  "Emit CALL assigning the result to the current slot frame.
If the callee function is known to have a return type, propagate it."
  (cl-assert call)
  (comp--emit (list 'set (comp--slot) call)))

(defun comp--copy-slot (src-n &optional dst-n)
  "Set slot number DST-N to slot number SRC-N as source.
If DST-N is specified, use it; otherwise assume it to be the current slot."
  (comp--with-sp (or dst-n (comp--sp))
    (let ((src-slot (comp--slot-n src-n)))
      (cl-assert src-slot)
      (comp--emit `(set ,(comp--slot) ,src-slot)))))

(defsubst comp--emit-annotation (str)
  "Emit annotation STR."
  (comp--emit `(comment ,str)))

(defsubst comp--emit-setimm (val)
  "Set constant VAL to current slot."
  (comp--add-const-to-relocs val)
  ;; Leave relocation index nil on purpose, will be fixed-up in final
  ;; by `comp--finalize-relocs'.
  (comp--emit `(setimm ,(comp--slot) ,val)))

(defun comp--make-curr-block (block-name entry-sp &optional addr)
  "Create a basic block with BLOCK-NAME and set it as current block.
ENTRY-SP is the sp value when entering.
Add block to the current function and return it."
  (let ((bb (make--comp-block-lap addr entry-sp block-name)))
    (setf (comp-limplify-curr-block comp-pass) bb
          (comp-limplify-pc comp-pass) addr
          (comp-limplify-sp comp-pass) (when (comp-block-lap-p bb)
                                         (comp-block-lap-sp bb)))
    (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
    bb))

(defun comp--latch-make-fill (target)
  "Create a latch pointing to TARGET and fill it.
Return the created latch."
  (let ((latch (make-comp-latch :name (comp--new-block-sym "latch")))
        (curr-bb (comp-limplify-curr-block comp-pass)))
    ;; See `comp--make-curr-block'.
    (setf (comp-limplify-curr-block comp-pass) latch)
    (when (< (comp-func-speed comp-func) 3)
      ;; At speed 3 the programmer is responsible to manually
      ;; place `comp-maybe-gc-or-quit'.
      (comp--emit '(call comp-maybe-gc-or-quit)))
    ;; See `comp--emit-uncond-jump'.
    (comp--emit `(jump ,(comp-block-name target)))
    (comp--mark-curr-bb-closed)
    (puthash (comp-block-name latch) latch (comp-func-blocks comp-func))
    (setf (comp-limplify-curr-block comp-pass) curr-bb)
    latch))

(defun comp--emit-uncond-jump (lap-label)
  "Emit an unconditional branch to LAP-LABEL."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (when stack-depth
      (cl-assert (= (1- stack-depth) (comp--sp))))
    (let* ((target-addr (comp--label-to-addr label-num))
           (target (comp--bb-maybe-add target-addr
                                      (comp--sp)))
           (latch (when (< target-addr (comp-limplify-pc comp-pass))
                    (comp--latch-make-fill target)))
           (eff-target-name (comp-block-name (or latch target))))
      (comp--emit `(jump ,eff-target-name))
      (comp--mark-curr-bb-closed))))

(defun comp--emit-cond-jump (a b target-offset lap-label negated)
  "Emit a conditional jump to LAP-LABEL when A and B satisfy EQ.
TARGET-OFFSET is the positive offset on the SP when branching to the target
block.
If NEGATED is non null, negate the tested condition.
Return value is the fall-through block name."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (let* ((bb (comp-block-name (comp--bb-maybe-add
                                 (1+ (comp-limplify-pc comp-pass))
                                 (comp--sp)))) ; Fall through block.
           (target-sp (+ target-offset (comp--sp)))
           (target-addr (comp--label-to-addr label-num))
           (target (comp--bb-maybe-add target-addr target-sp))
           (latch (when (< target-addr (comp-limplify-pc comp-pass))
                    (comp--latch-make-fill target)))
           (eff-target-name (comp-block-name (or latch target))))
      (when label-sp
        (cl-assert (= (1- label-sp) (+ target-offset (comp--sp)))))
      (comp--emit (if negated
                     (list 'cond-jump a b bb eff-target-name)
		   (list 'cond-jump a b eff-target-name bb)))
      (comp--mark-curr-bb-closed)
      bb)))

(defun comp--emit-handler (lap-label handler-type)
  "Emit a nonlocal-exit handler to LAP-LABEL of type HANDLER-TYPE."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (cl-assert (= (- label-sp 2) (comp--sp)))
    (setf (comp-func-has-non-local comp-func) t)
    (let* ((guarded-bb (comp--bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                          (comp--sp)))
           (handler-bb (comp--bb-maybe-add (comp--label-to-addr label-num)
                                          (1+ (comp--sp))))
           (pop-bb (make--comp-block-lap nil (comp--sp) (comp--new-block-sym))))
      (comp--emit (list 'push-handler
                       handler-type
                       (comp--slot+1)
                       (comp-block-name pop-bb)
                       (comp-block-name guarded-bb)))
      (comp--mark-curr-bb-closed)
      ;; Emit the basic block to pop the handler if we got the non local.
      (puthash (comp-block-name pop-bb) pop-bb (comp-func-blocks comp-func))
      (setf (comp-limplify-curr-block comp-pass) pop-bb)
      (comp--emit `(fetch-handler ,(comp--slot+1)))
      (comp--emit `(jump ,(comp-block-name handler-bb)))
      (comp--mark-curr-bb-closed))))

(defun comp--limplify-listn (n)
  "Limplify list N."
  (comp--with-sp (+ (comp--sp) n -1)
    (comp--emit-set-call (comp--call 'cons
                                   (comp--slot)
                                   (make--comp-mvar :constant nil))))
  (cl-loop for sp from (+ (comp--sp) n -2) downto (comp--sp)
           do (comp--with-sp sp
                (comp--emit-set-call (comp--call 'cons
                                               (comp--slot)
                                               (comp--slot+1))))))

(defun comp--new-block-sym (&optional postfix)
  "Return a unique symbol postfixing POSTFIX naming the next new basic block."
  (intern (format (if postfix "bb_%s_%s" "bb_%s")
                  (funcall (comp-func-block-cnt-gen comp-func))
                  postfix)))

(defun comp--fill-label-h ()
  "Fill label-to-addr hash table for the current function."
  (setf (comp-limplify-label-to-addr comp-pass) (make-hash-table :test 'eql))
  (cl-loop for insn in (comp-func-lap comp-func)
           for addr from 0
           do (pcase insn
                (`(TAG ,label . ,_)
                 (puthash label addr (comp-limplify-label-to-addr comp-pass))))))

(defun comp--jump-table-optimizable (jmp-table)
  "Return t if JMP-TABLE can be optimized out."
  ;; Identify LAP sequences like:
  ;; (byte-constant #s(hash-table test eq data (created 126 deleted 126 changed 126)) . 24)
  ;; (byte-switch)
  ;; (TAG 126 . 10)
  (let ((targets (hash-table-values jmp-table)))
    (when (apply #'= targets)
      (pcase (nth (1+ (comp-limplify-pc comp-pass)) (comp-func-lap comp-func))
        (`(TAG ,target . ,_label-sp)
         (= target (car targets)))))))

(defun comp--emit-switch (var last-insn)
  "Emit a Limple for a lap jump table given VAR and LAST-INSN."
  ;; FIXME this not efficient for big jump tables. We should have a second
  ;; strategy for this case.
  (pcase last-insn
    (`(setimm ,_ ,jmp-table)
     (unless (comp--jump-table-optimizable jmp-table)
       (cl-loop
        for test being each hash-keys of jmp-table
        using (hash-value target-label)
        with len = (hash-table-count jmp-table)
        with test-func = (hash-table-test jmp-table)
        for n from 1
        for last = (= n len)
        for m-test = (make--comp-mvar :constant test)
        for target-name = (comp-block-name (comp--bb-maybe-add
                                            (comp--label-to-addr target-label)
                                            (comp--sp)))
        for ff-bb = (if last
                        (comp--bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                           (comp--sp))
                      (make--comp-block-lap nil
                                            (comp--sp)
                                            (comp--new-block-sym)))
        for ff-bb-name = (comp-block-name ff-bb)
        if (eq test-func 'eq)
          do (comp--emit (list 'cond-jump var m-test target-name ff-bb-name))
        else
        ;; Store the result of the comparison into the scratch slot before
        ;; emitting the conditional jump.
          do (comp--emit (list 'set (make--comp-mvar :slot 'scratch)
                              (comp--call test-func var m-test)))
             (comp--emit (list 'cond-jump
                              (make--comp-mvar :slot 'scratch)
                              (make--comp-mvar :constant nil)
                              ff-bb-name target-name))
        unless last
        ;; All fall through are artificially created here except the last one.
          do (puthash ff-bb-name ff-bb (comp-func-blocks comp-func))
             (setf (comp-limplify-curr-block comp-pass) ff-bb))))
    (_ (signal 'native-ice
               '("missing previous setimm while creating a switch")))))

(defun comp--func-arity (subr-name)
  "Like `func-arity' but invariant against primitive redefinitions.
SUBR-NAME is the name of function."
  (or (gethash subr-name comp-subr-arities-h)
      (func-arity subr-name)))

(defun comp--emit-set-call-subr (subr-name sp-delta)
    "Emit a call for SUBR-NAME.
SP-DELTA is the stack adjustment."
    (let* ((nargs (1+ (- sp-delta)))
           (arity (comp--func-arity subr-name))
           (minarg (car arity))
           (maxarg (cdr arity)))
      (when (eq maxarg 'unevalled)
        (signal 'native-ice (list "subr contains unevalled args" subr-name)))
      (if (eq maxarg 'many)
          ;; callref case.
          (comp--emit-set-call (comp--callref subr-name nargs (comp--sp)))
        ;; Normal call.
        (unless (and (>= maxarg nargs) (<= minarg nargs))
          (signal 'native-ice
                  (list "incoherent stack adjustment" nargs maxarg minarg)))
        (let* ((subr-name subr-name)
               (slots (cl-loop for i from 0 below maxarg
                               collect (comp--slot-n (+ i (comp--sp))))))
          (comp--emit-set-call (apply #'comp--call (cons subr-name slots)))))))

(eval-when-compile
  (defun comp--op-to-fun (x)
    "Given the LAP op strip \"byte-\" to have the subr name."
    (intern (string-replace "byte-" "" x)))

  (defun comp--body-eff (body op-name sp-delta)
    "Given the original BODY, compute the effective one.
When BODY is `auto', guess function name from the LAP byte-code
name.  Otherwise expect lname fnname."
    (pcase (car body)
      ('auto
       `((comp--emit-set-call-subr ',(comp--op-to-fun op-name) ,sp-delta)))
      ((pred symbolp)
       `((comp--emit-set-call-subr ',(car body) ,sp-delta)))
      (_ body))))

(defmacro comp--op-case (&rest cases)
  "Expand CASES into the corresponding `pcase' expansion.
This is responsible for generating the proper stack adjustment, when known,
and the annotation emission."
  (declare (debug (body))
           (indent defun))
  (declare-function comp--body-eff nil (body op-name sp-delta))
  `(pcase op
     ,@(cl-loop for (op . body) in cases
		for sp-delta = (gethash op comp-op-stack-info)
                for op-name = (symbol-name op)
		if body
		collect `(',op
                          ;; Log all LAP ops except the TAG one.
                          ;; ,(unless (eq op 'TAG)
                          ;;    `(comp--emit-annotation
                          ;;      ,(concat "LAP op " op-name)))
                          ;; Emit the stack adjustment if present.
                          ,(when (and sp-delta (not (eq 0 sp-delta)))
                             `(incf (comp--sp) ,sp-delta))
                          ,@(comp--body-eff body op-name sp-delta))
                else
		collect `(',op (signal 'native-ice
                                       (list "unsupported LAP op" ',op-name))))
     (_ (signal 'native-ice (list "unexpected LAP op" (symbol-name op))))))

(defun comp--limplify-lap-inst (insn)
  "Limplify LAP instruction INSN pushing it in the proper basic block."
  (let ((op (car insn))
        (arg (if (consp (cdr insn))
                 (cadr insn)
               (cdr insn))))
    (comp--op-case
      (TAG
       (cl-destructuring-bind (_TAG label-num . label-sp) insn
         ;; Paranoid?
         (when label-sp
           (cl-assert (= (1- label-sp) (comp-limplify-sp comp-pass))))
         (comp--emit-annotation (format "LAP TAG %d" label-num))))
      (byte-stack-ref
       (comp--copy-slot (- (comp--sp) arg 1)))
      (byte-varref
       (comp--emit-set-call (comp--call 'symbol-value (make--comp-mvar
                                                     :constant arg))))
      (byte-varset
       (comp--emit (comp--call 'set_internal
                             (make--comp-mvar :constant arg)
                             (comp--slot+1))))
      (byte-varbind ;; Verify
       (comp--emit (comp--call 'specbind
                             (make--comp-mvar :constant arg)
                             (comp--slot+1))))
      (byte-call
       (incf (comp--sp) (- arg))
       (comp--emit-set-call (comp--callref 'funcall (1+ arg) (comp--sp))))
      (byte-unbind
       (comp--emit (comp--call 'helper_unbind_n
                             (make--comp-mvar :constant arg))))
      (byte-pophandler
       (comp--emit '(pop-handler)))
      (byte-pushconditioncase
       (comp--emit-handler (cddr insn) 'condition-case))
      (byte-pushcatch
       (comp--emit-handler (cddr insn) 'catcher))
      (byte-nth auto)
      (byte-symbolp auto)
      (byte-consp auto)
      (byte-stringp auto)
      (byte-listp auto)
      (byte-eq auto)
      (byte-memq auto)
      (byte-not
       (comp--emit-set-call (comp--call 'eq (comp--slot-n (comp--sp))
                                      (make--comp-mvar :constant nil))))
      (byte-car auto)
      (byte-cdr auto)
      (byte-cons auto)
      (byte-list1
       (comp--limplify-listn 1))
      (byte-list2
       (comp--limplify-listn 2))
      (byte-list3
       (comp--limplify-listn 3))
      (byte-list4
       (comp--limplify-listn 4))
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
       (comp--emit-set-call (comp--callref 'concat 2 (comp--sp))))
      (byte-concat3
       (comp--emit-set-call (comp--callref 'concat 3 (comp--sp))))
      (byte-concat4
       (comp--emit-set-call (comp--callref 'concat 4 (comp--sp))))
      (byte-sub1 1-)
      (byte-add1 1+)
      (byte-eqlsign =)
      (byte-gtr >)
      (byte-lss <)
      (byte-leq <=)
      (byte-geq >=)
      (byte-diff -)
      (byte-negate
       (comp--emit-set-call (comp--call 'negate (comp--slot))))
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
       (comp--emit-set-call (comp--call 'indent-to
                                      (comp--slot)
                                      (make--comp-mvar :constant nil))))
      (byte-scan-buffer-OBSOLETE)
      (byte-eolp auto)
      (byte-eobp auto)
      (byte-bolp auto)
      (byte-bobp auto)
      (byte-current-buffer auto)
      (byte-set-buffer auto)
      (byte-save-current-buffer
       (comp--emit (comp--call 'record_unwind_current_buffer)))
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
       (comp--emit-set-call (comp--call 'narrow-to-region
                                      (comp--slot)
                                      (comp--slot+1))))
      (byte-widen
       (comp--emit-set-call (comp--call 'widen)))
      (byte-end-of-line auto)
      (byte-constant2) ; TODO
      ;; Branches.
      (byte-goto
       (comp--emit-uncond-jump (cddr insn)))
      (byte-goto-if-nil
       (comp--emit-cond-jump (comp--slot+1) (make--comp-mvar :constant nil) 0
                            (cddr insn) nil))
      (byte-goto-if-not-nil
       (comp--emit-cond-jump (comp--slot+1) (make--comp-mvar :constant nil) 0
                            (cddr insn) t))
      (byte-goto-if-nil-else-pop
       (comp--emit-cond-jump (comp--slot+1) (make--comp-mvar :constant nil) 1
                            (cddr insn) nil))
      (byte-goto-if-not-nil-else-pop
       (comp--emit-cond-jump (comp--slot+1) (make--comp-mvar :constant nil) 1
                            (cddr insn) t))
      (byte-return
       (comp--emit `(return ,(comp--slot+1))))
      (byte-discard 'pass)
      (byte-dup
       (comp--copy-slot (1- (comp--sp))))
      (byte-save-excursion
       (comp--emit (comp--call 'record_unwind_protect_excursion)))
      (byte-save-window-excursion-OBSOLETE)
      (byte-save-restriction
       (comp--emit (comp--call 'helper_save_restriction)))
      (byte-catch) ;; Obsolete
      (byte-unwind-protect
       (comp--emit (comp--call 'helper_unwind_protect (comp--slot+1))))
      (byte-condition-case) ;; Obsolete
      (byte-temp-output-buffer-setup-OBSOLETE)
      (byte-temp-output-buffer-show-OBSOLETE)
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
       (incf (comp--sp) (- 1 arg))
       (comp--emit-set-call (comp--callref 'list arg (comp--sp))))
      (byte-concatN
       (incf (comp--sp) (- 1 arg))
       (comp--emit-set-call (comp--callref 'concat arg (comp--sp))))
      (byte-insertN
       (incf (comp--sp) (- 1 arg))
       (comp--emit-set-call (comp--callref 'insert arg (comp--sp))))
      (byte-stack-set
       (comp--copy-slot (1+ (comp--sp)) (- (comp--sp) arg -1)))
      (byte-stack-set2 (cl-assert nil)) ;; TODO
      (byte-discardN
       (incf (comp--sp) (- arg)))
      (byte-switch
       ;; Assume to follow the emission of a setimm.
       ;; This is checked into comp--emit-switch.
       (comp--emit-switch (comp--slot+1)
                         (cl-first (comp-block-insns
                                    (comp-limplify-curr-block comp-pass)))))
      (byte-constant
       (comp--emit-setimm arg))
      (byte-discardN-preserve-tos
       (incf (comp--sp) (- arg))
       (comp--copy-slot (+ arg (comp--sp)))))))

(defun comp--emit-narg-prologue (minarg nonrest rest)
  "Emit the prologue for a narg function."
  (cl-loop for i below minarg
           do (comp--emit `(set-args-to-local ,(comp--slot-n i)))
              (comp--emit '(inc-args)))
  (cl-loop for i from minarg below nonrest
           for bb = (intern (format "entry_%s" i))
           for fallback = (intern (format "entry_fallback_%s" i))
           do (comp--emit `(cond-jump-narg-leq ,i ,fallback ,bb))
              (comp--make-curr-block bb (comp--sp))
              (comp--emit `(set-args-to-local ,(comp--slot-n i)))
              (comp--emit '(inc-args))
              finally (comp--emit '(jump entry_rest_args)))
  (when (/= minarg nonrest)
    (cl-loop for i from minarg below nonrest
             for bb = (intern (format "entry_fallback_%s" i))
             for next-bb = (if (= (1+ i) nonrest)
                               'entry_rest_args
                             (intern (format "entry_fallback_%s" (1+ i))))
             do (comp--with-sp i
                  (comp--make-curr-block bb (comp--sp))
                  (comp--emit-setimm nil)
                  (comp--emit `(jump ,next-bb)))))
  (comp--make-curr-block 'entry_rest_args (comp--sp))
  (comp--emit `(set-rest-args-to-local ,(comp--slot-n nonrest)))
  (setf (comp--sp) nonrest)
  (when (and (> nonrest 8) (null rest))
    (decf (comp--sp))))

(defun comp--limplify-finalize-function (func)
  "Reverse insns into all basic blocks of FUNC."
  (cl-loop for bb being the hash-value in (comp-func-blocks func)
           do (setf (comp-block-insns bb)
                    (nreverse (comp-block-insns bb))))
  (comp--log-func func 2)
  func)

(cl-defgeneric comp--prepare-args-for-top-level (function)
  "Given FUNCTION, return the two arguments for comp--register-...")

(cl-defmethod comp--prepare-args-for-top-level ((function comp-func-l))
  "Lexically-scoped FUNCTION."
  (let ((args (comp-func-l-args function)))
    (cons (make--comp-mvar :constant (comp-args-base-min args))
          (make--comp-mvar :constant (cond
                                     ((comp-args-p args) (comp-args-max args))
                                     ((comp-nargs-rest args) 'many)
                                     (t (comp-nargs-nonrest args)))))))

(cl-defmethod comp--prepare-args-for-top-level ((function comp-func-d))
  "Dynamically scoped FUNCTION."
  (cons (make--comp-mvar :constant (func-arity (comp-func-byte-func function)))
        (let ((comp-curr-allocation-class 'd-default))
          ;; Lambda-lists must stay in the same relocation class of
          ;; the object referenced by code to respect uninterned
          ;; symbols.
          (make--comp-mvar :constant (comp-func-d-lambda-list function)))))

(cl-defgeneric comp--emit-for-top-level (form for-late-load)
  "Emit the Limple code for top level FORM.")

(cl-defmethod comp--emit-for-top-level ((form byte-to-native-func-def)
                                       for-late-load)
  (let* ((name (byte-to-native-func-def-name form))
         (c-name (byte-to-native-func-def-c-name form))
         (f (gethash c-name (comp-ctxt-funcs-h comp-ctxt)))
         (args (comp--prepare-args-for-top-level f)))
    (cl-assert (and name f))
    (comp--emit
     `(set ,(make--comp-mvar :slot 1)
           ,(comp--call (if for-late-load
                           'comp--late-register-subr
                         'comp--register-subr)
                       (make--comp-mvar :constant name)
                       (make--comp-mvar :constant c-name)
                       (car args)
                       (cdr args)
                       (setf (comp-func-type f)
                             (make--comp-mvar :constant nil))
                       (make--comp-mvar
                        :constant
                        (list
                         (let* ((h (comp-ctxt-function-docs comp-ctxt))
                                (i (hash-table-count h)))
                           (puthash i (comp-func-doc f) h)
                           i)
                         (comp-func-int-spec f)
                         (comp-func-command-modes f)))
                       ;; This is the compilation unit it-self passed as
                       ;; parameter.
                       (make--comp-mvar :slot 0))))))

(cl-defmethod comp--emit-for-top-level ((form byte-to-native-top-level)
                                       for-late-load)
  (unless for-late-load
    (comp--emit
     (comp--call 'eval
                (let ((comp-curr-allocation-class 'd-default))
                  (make--comp-mvar :constant
                                  (byte-to-native-top-level-form form)))
                (make--comp-mvar :constant
                                (byte-to-native-top-level-lexical form))))))

(defun comp--emit-lambda-for-top-level (func)
  "Emit the creation of subrs for lambda FUNC.
These are stored in the reloc data array."
  (let ((args (comp--prepare-args-for-top-level func)))
    (let ((comp-curr-allocation-class 'd-default))
      (comp--add-const-to-relocs (comp-func-byte-func func)))
    (comp--emit
     (comp--call 'comp--register-lambda
                ;; mvar to be fixed-up when containers are
                ;; finalized.
                (or (gethash (comp-func-byte-func func)
                             (comp-ctxt-lambda-fixups-h comp-ctxt))
                    (puthash (comp-func-byte-func func)
                             (make--comp-mvar :constant nil)
                             (comp-ctxt-lambda-fixups-h comp-ctxt)))
                (make--comp-mvar :constant (comp-func-c-name func))
                (car args)
                (cdr args)
                (setf (comp-func-type func)
                      (make--comp-mvar :constant nil))
                (make--comp-mvar
                 :constant
                 (list
                  (let* ((h (comp-ctxt-function-docs comp-ctxt))
                         (i (hash-table-count h)))
                    (puthash i (comp-func-doc func) h)
                    i)
                  (comp-func-int-spec func)
                  (comp-func-command-modes func)))
                ;; This is the compilation unit it-self passed as
                ;; parameter.
                (make--comp-mvar :slot 0)))))

(defun comp--limplify-top-level (for-late-load)
  "Create a Limple function to modify the global environment at load.
When FOR-LATE-LOAD is non-nil, the emitted function modifies only
function definition.

Synthesize a function called `top_level_run' that gets one single
parameter (the compilation unit itself).  To define native
functions, `top_level_run' will call back `comp--register-subr'
into the C code forwarding the compilation unit."
  ;; Once an .eln is loaded and Emacs is dumped 'top_level_run' has no
  ;; reasons to be executed ever again.  Therefore all objects can be
  ;; just ephemeral.
  (let* ((comp-curr-allocation-class 'd-ephemeral)
         (func (make-comp-func-l :name (if for-late-load
                                           'late-top-level-run
                                         'top-level-run)
                                 :c-name (if for-late-load
                                             "late_top_level_run"
                                           "top_level_run")
                                 :args (make-comp-args :min 1 :max 1)
                                 ;; Frame is 2 wide: Slot 0 is the
                                 ;; compilation unit being loaded
                                 ;; (incoming parameter).  Slot 1 is
                                 ;; the last function being
                                 ;; registered.
                                 :frame-size 2
                                 :speed (comp-ctxt-speed comp-ctxt)
                                 :safety (comp-ctxt-safety comp-ctxt)))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :curr-block (make--comp-block-lap -1 0 'top-level)
                     :frame (comp--new-frame 1 0))))
    (comp--make-curr-block 'entry (comp--sp))
    (comp--emit-annotation (if for-late-load
                              "Late top level"
                            "Top level"))
    ;; Assign the compilation unit incoming as parameter to the slot frame 0.
    (comp--emit `(set-par-to-local ,(comp--slot-n 0) 0))
    (maphash (lambda (_ func)
               (comp--emit-lambda-for-top-level func))
             (comp-ctxt-byte-func-to-func-h comp-ctxt))
    (mapc (lambda (x) (comp--emit-for-top-level x for-late-load))
          (comp-ctxt-top-level-forms comp-ctxt))
    (comp--emit `(return ,(make--comp-mvar :slot 1)))
    (comp--limplify-finalize-function func)))

(defun comp--addr-to-bb-name (addr)
  "Search for a block starting at ADDR into pending or limplified blocks."
  ;; FIXME Actually we could have another hash for this.
  (cl-flet ((pred (bb)
              (equal (comp-block-lap-addr bb) addr)))
    (if-let* ((pending (cl-find-if #'pred
                                  (comp-limplify-pending-blocks comp-pass))))
        (comp-block-name pending)
      (cl-loop for bb being the hash-value in (comp-func-blocks comp-func)
               when (pred bb)
                 return (comp-block-name bb)))))

(defun comp--limplify-block (bb)
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
   do (comp--limplify-lap-inst inst)
      (incf (comp-limplify-pc comp-pass))
   when (comp--lap-fall-through-p inst)
   do (pcase next-inst
        (`(TAG ,_label . ,label-sp)
         (when label-sp
           (cl-assert (= (1- label-sp) (comp--sp))))
         (let* ((stack-depth (if label-sp
                                 (1- label-sp)
                               (comp--sp)))
                (next-bb (comp-block-name (comp--bb-maybe-add
                                           (comp-limplify-pc comp-pass)
                                           stack-depth))))
           (unless (comp-block-closed bb)
             (comp--emit `(jump ,next-bb))))
         (cl-return)))
   until (comp--lap-eob-p inst)))

(defun comp--limplify-function (func)
  "Limplify a single function FUNC."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :frame (comp--new-frame frame-size 0))))
    (comp--fill-label-h)
    ;; Prologue
    (comp--make-curr-block 'entry (comp--sp))
    (comp--emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-name func))))
    ;; Dynamic functions have parameters bound by the trampoline.
    (when (comp-func-l-p func)
      (let ((args (comp-func-l-args func)))
        (if (comp-args-p args)
            (cl-loop for i below (comp-args-max args)
                     do (incf (comp--sp))
                        (comp--emit `(set-par-to-local ,(comp--slot) ,i)))
          (comp--emit-narg-prologue (comp-args-base-min args)
                                   (comp-nargs-nonrest args)
                                   (comp-nargs-rest args)))))
    (comp--emit '(jump bb_0))
    ;; Body
    (comp--bb-maybe-add 0 (comp--sp))
    (cl-loop for next-bb = (pop (comp-limplify-pending-blocks comp-pass))
             while next-bb
             do (comp--limplify-block next-bb))
    ;; Sanity check against block duplication.
    (cl-loop with addr-h = (make-hash-table)
             for bb being the hash-value in (comp-func-blocks func)
             for addr = (when (comp-block-lap-p bb)
                          (comp-block-lap-addr bb))
             when addr
               do (cl-assert (null (gethash addr addr-h)))
                  (puthash addr t addr-h))
    (comp--limplify-finalize-function func)))

(defun comp--limplify (_)
  "Compute LIMPLE IR for forms in `comp-ctxt'."
  (maphash (lambda (_ f) (comp--limplify-function f))
           (comp-ctxt-funcs-h comp-ctxt))
  (comp--add-func-to-ctxt (comp--limplify-top-level nil))
  (when (comp-ctxt-with-late-load comp-ctxt)
    (comp--add-func-to-ctxt (comp--limplify-top-level t))))


;;; add-cstrs pass specific code.

;; This pass is responsible for adding constraints, these are
;; generated from:
;;
;;  - Conditional branches: each branch taken or non taken can be used
;;    in the CFG to infer information on the tested variables.
;;
;;  - Range propagation under test and branch (when the test is an
;;    arithmetic comparison).
;;
;;  - Type constraint under test and branch (when the test is a
;;    known predicate).
;;
;;  - Function calls: function calls to function assumed to be not
;;    redefinable can be used to add constrains on the function
;;    arguments.  Ex: if we execute successfully (= x y) we know that
;;    afterwards both x and y must satisfy the (or number marker)
;;    type specifier.


(defsubst comp--mvar-used-p (mvar)
  "Non-nil when MVAR is used as lhs in the current function."
  (declare (gv-setter (lambda (val)
			`(puthash ,mvar ,val comp-pass))))
  (gethash mvar comp-pass))

(defun comp--collect-mvars (form)
  "Add rhs m-var present in FORM into `comp-pass'."
  (cl-loop for x in form
           if (consp x)
             do (comp--collect-mvars x)
           else
             when (comp-mvar-p x)
               do (setf (comp--mvar-used-p x) t)))

(defun comp--collect-rhs ()
  "Collect all lhs mvars into `comp-pass'."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn in (comp-block-insns b)
       for (op . args) = insn
       if (comp--assign-op-p op)
         do (comp--collect-mvars (if (eq op 'setimm)
                                     (cl-first args)
                                   (cdr args)))
       else
         do (comp--collect-mvars args))))

(defun comp--negate-arithm-cmp-fun (function)
  "Negate FUNCTION.
Return nil if we don't want to emit constraints for its negation."
  (cl-ecase function
    (= nil)
    (> '<=)
    (< '>=)
    (>= '<)
    (<= '>)))

(defun comp--reverse-arithm-fun (function)
  "Reverse FUNCTION."
  (cl-case function
    (= '=)
    (> '<)
    (< '>)
    (>= '<=)
    (<= '>=)
    (t function)))

(defun comp--emit-assume (kind lhs rhs bb negated)
  "Emit an assume of kind KIND for mvar LHS being RHS.
When NEGATED is non-nil, the assumption is negated.
The assume is emitted at the beginning of the block BB."
  (let ((lhs-slot (comp-mvar-slot lhs)))
    (cl-assert lhs-slot)
    (pcase kind
      ((or 'and 'and-nhc)
       (if (comp-mvar-p rhs)
           (let ((tmp-mvar (if negated
                               (make--comp-mvar :slot (comp-mvar-slot rhs))
                             rhs)))
             (push `(assume ,(make--comp-mvar :slot lhs-slot)
                            (,kind ,lhs ,tmp-mvar))
	           (comp-block-insns bb))
             (if negated
                 (push `(assume ,tmp-mvar (not ,rhs))
	               (comp-block-insns bb))))
         ;; If is only a constraint we can negate it directly.
         (push `(assume ,(make--comp-mvar :slot lhs-slot)
                        (,kind ,lhs ,(if negated
                                       (comp-cstr-negation-make rhs)
                                     rhs)))
	       (comp-block-insns bb))))
      ((pred comp--arithm-cmp-fun-p)
       (when-let* ((kind (if negated
                             (comp--negate-arithm-cmp-fun kind)
                           kind)))
         (push `(assume ,(make--comp-mvar :slot lhs-slot)
                        (,kind ,lhs
                               ,(if-let* ((vld (comp-cstr-imm-vld-p rhs))
                                          (val (comp-cstr-imm rhs))
                                          (ok (and (integerp val)
                                                   (not (memq kind '(= !=))))))
                                    val
                                  (make--comp-mvar :slot (comp-mvar-slot rhs)))))
	       (comp-block-insns bb))))
      (_ (cl-assert nil)))
    (setf (comp-func-ssa-status comp-func) 'dirty)))

(defun comp--maybe-add-vmvar (op cmp-res insns-seq)
  "If CMP-RES is clobbering OP emit a new constrained mvar and return it.
Return OP otherwise."
  (if-let* ((match (eql (comp-mvar-slot op) (comp-mvar-slot cmp-res)))
            (new-mvar (make--comp-mvar
                       :slot
                       (- (incf (comp-func-vframe-size comp-func))))))
      (progn
        (push `(assume ,new-mvar ,op) (cdr insns-seq))
        new-mvar)
    op))

(defun comp--add-new-block-between (bb-symbol bb-a bb-b)
  "Create a new basic-block named BB-SYMBOL and add it between BB-A and BB-B."
  (cl-loop
   with new-bb = (make-comp-block-cstr :name bb-symbol
                                       :insns `((jump ,(comp-block-name bb-b))))
   with new-edge = (comp--edge-make :src bb-a :dst new-bb)
   for ed in (comp-block-in-edges bb-b)
   when (eq (comp-edge-src ed) bb-a)
   do
   ;; Connect `ed' to `new-bb' and disconnect it from `bb-a'.
   (cl-assert (memq ed (comp-block-out-edges bb-a)))
   (setf (comp-edge-src ed) new-bb
         (comp-block-out-edges bb-a) (delq ed (comp-block-out-edges bb-a)))
   (push ed (comp-block-out-edges new-bb))
   ;; Connect `bb-a' `new-bb' with `new-edge'.
   (push new-edge (comp-block-out-edges bb-a))
   (push new-edge (comp-block-in-edges new-bb))
   (setf (comp-func-ssa-status comp-func) 'dirty)
   ;; Add `new-edge' to the current function and return it.
   (cl-return (puthash bb-symbol new-bb (comp-func-blocks comp-func)))
   finally (cl-assert nil)))

;; Cheap substitute to a copy propagation pass...
(defun comp--cond-cstrs-target-mvar (mvar exit-insn bb)
  "Given MVAR, search in BB the original mvar MVAR got assigned from.
Keep on searching till EXIT-INSN is encountered."
  (cl-flet ((targetp (x)
              ;; Ret t if x is an mvar and target the correct slot number.
              (and (comp-mvar-p x)
                   (eql (comp-mvar-slot mvar) (comp-mvar-slot x)))))
    (cl-loop
     with res = nil
     for insn in (comp-block-insns bb)
     when (eq insn exit-insn)
     do (cl-return (and (comp-mvar-p res) res))
     do (pcase insn
          (`(,(pred comp--assign-op-p) ,(pred targetp) ,rhs)
           (setf res rhs)))
     finally (cl-assert nil))))

(defun comp--add-cond-cstrs-target-block (curr-bb target-bb-sym)
  "Return the appropriate basic block to add constraint assumptions into.
CURR-BB is the current basic block.
TARGET-BB-SYM is the symbol name of the target block."
  (let* ((target-bb (gethash target-bb-sym
                             (comp-func-blocks comp-func)))
         (target-bb-in-edges (comp-block-in-edges target-bb)))
    (cl-assert target-bb-in-edges)
    (if (length= target-bb-in-edges 1)
        ;; If block has only one predecessor is already suitable for
        ;; adding constraint assumptions.
        target-bb
      (cl-loop
       ;; Search for the first suitable basic block name.
       for i from 0
       for new-name = (intern (format "%s_cstrs_%d" (symbol-name target-bb-sym)
                                      i))
       until (null (gethash new-name (comp-func-blocks comp-func)))
       finally
       ;; Add it.
       (cl-return (comp--add-new-block-between new-name curr-bb target-bb))))))

(defun comp--add-cond-cstrs-simple ()
  "`comp--add-cstrs' worker function for each selected function."
  (cl-loop
   ;; Don't iterate over hash values directly as
   ;; `comp--add-cond-cstrs-target-block' can modify the hash table
   ;; content.
   for b in (cl-loop for b being each hash-value of (comp-func-blocks comp-func)
                     collect b)
   do
   (cl-loop
    named in-the-basic-block
    for insn-seq on (comp-block-insns b)
    do
    (pcase insn-seq
      (`((set ,(and (pred comp-mvar-p) tmp-mvar) ,(pred comp-mvar-p))
         ;; (comment ,_comment-str)
         (cond-jump ,tmp-mvar ,obj2 . ,blocks))
       (cl-loop
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(nil t)
	when (comp--mvar-used-p tmp-mvar)
        do
	(let ((block-target (comp--add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp--emit-assume 'and tmp-mvar obj2 block-target negated))
        finally (cl-return-from in-the-basic-block)))
      (`((cond-jump ,obj1 ,obj2 . ,blocks))
       (cl-loop
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(nil t)
	when (comp--mvar-used-p obj1)
        do
	(let ((block-target (comp--add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp--emit-assume 'and obj1 obj2 block-target negated))
        finally (cl-return-from in-the-basic-block)))))))

(defun comp--add-cond-cstrs ()
  "`comp--add-cstrs' worker function for each selected function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do
   (cl-loop
    named in-the-basic-block
    with prev-insns-seq
    for insns-seq on (comp-block-insns b)
    do
    (pcase insns-seq
      (`((set ,(and (pred comp-mvar-p) mvar-tested-copy)
              ,(and (pred comp-mvar-p) mvar-tested))
         (set ,(and (pred comp-mvar-p) mvar-1)
              (call type-of ,(and (pred comp-mvar-p) mvar-tested-copy)))
         (set ,(and (pred comp-mvar-p) mvar-2)
              (call symbol-value ,(and (pred comp-cstr-cl-tag-p) mvar-tag)))
         (set ,(and (pred comp-mvar-p) mvar-3)
              (call memq ,(and (pred comp-mvar-p) mvar-1) ,(and (pred comp-mvar-p) mvar-2)))
         (cond-jump ,(and (pred comp-mvar-p) mvar-3) ,(pred comp-mvar-p) ,_bb1 ,bb2))
       (comp--emit-assume 'and mvar-tested
                          (make--comp-mvar :type (comp-cstr-cl-tag mvar-tag))
                          (comp--add-cond-cstrs-target-block b bb2)
                          nil))
      (`((set ,(and (pred comp-mvar-p) cmp-res)
              (,(pred comp--call-op-p)
               ,(and (or (pred comp--equality-fun-p)
                         (pred comp--arithm-cmp-fun-p))
                     fun)
               ,op1 ,op2))
	 ;; (comment ,_comment-str)
	 (cond-jump ,cmp-res ,(pred comp-mvar-p) . ,blocks))
       (cl-loop
        with target-mvar1 = (comp--cond-cstrs-target-mvar op1 (car insns-seq) b)
        with target-mvar2 = (comp--cond-cstrs-target-mvar op2 (car insns-seq) b)
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(t nil)
        for kind = (cl-case fun
                     (equal 'and-nhc)
                     (eql 'and-nhc)
                     (eq 'and)
                     (t fun))
        when (or (comp--mvar-used-p target-mvar1)
                 (comp--mvar-used-p target-mvar2))
        do
        (let ((block-target (comp--add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (when (comp--mvar-used-p target-mvar1)
            (comp--emit-assume kind target-mvar1
                              (comp--maybe-add-vmvar op2 cmp-res prev-insns-seq)
                              block-target negated))
          (when (comp--mvar-used-p target-mvar2)
            (comp--emit-assume (comp--reverse-arithm-fun kind)
                              target-mvar2
                              (comp--maybe-add-vmvar op1 cmp-res prev-insns-seq)
                              block-target negated)))
        finally (cl-return-from in-the-basic-block)))
      (`((set ,(and (pred comp-mvar-p) cmp-res)
              (,(pred comp--call-op-p)
               ,(and (pred comp--known-predicate-p) fun)
               ,op))
         . ,(or
	     ;; (comment ,_comment-str)
	     (and `((cond-jump ,cmp-res ,(pred comp-mvar-p) . ,blocks))
	          (let negated-branch nil))
             (and `((set ,neg-cmp-res
	                 (call eq ,cmp-res ,(pred comp-cstr-null-p)))
	            (cond-jump ,neg-cmp-res ,(pred comp-mvar-p) . ,blocks))
	          (let negated-branch t))))
       (cl-loop
        with target-mvar = (comp--cond-cstrs-target-mvar op (car insns-seq) b)
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in (if negated-branch '(nil t) '(t nil))
        when (comp--mvar-used-p target-mvar)
        do
        (let ((block-target (comp--add-cond-cstrs-target-block
                             b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp--emit-assume 'and target-mvar (if negated
                                                  (comp--pred-to-neg-cstr fun)
                                                (comp--pred-to-pos-cstr fun))
                             block-target nil))
        finally (cl-return-from in-the-basic-block))))
    (setf prev-insns-seq insns-seq))))

(defsubst comp--insert-insn (insn insn-cell)
  "Insert INSN as second insn of INSN-CELL."
  (let ((next-cell (cdr insn-cell))
        (new-cell `(,insn)))
    (setf (cdr insn-cell) new-cell
          (cdr new-cell) next-cell
          (comp-func-ssa-status comp-func) 'dirty)))

(defun comp--emit-call-cstr (mvar call-cell cstr)
  "Emit a constraint CSTR for MVAR after CALL-CELL."
  (let* ((new-mvar (make--comp-mvar :slot (comp-mvar-slot mvar)))
         ;; Have new-mvar as LHS *and* RHS to ensure monotonicity and
         ;; fwprop convergence!!
         (insn `(assume ,new-mvar (and ,new-mvar ,mvar ,cstr))))
    (comp--insert-insn insn call-cell)))

(defun comp--lambda-list-gen (lambda-list)
  "Return a generator to iterate over LAMBDA-LIST."
  (lambda ()
    (cl-case (car lambda-list)
      (&optional
       (setf lambda-list (cdr lambda-list))
       (prog1
           (car lambda-list)
         (setf lambda-list (cdr lambda-list))))
      (&rest
       (cadr lambda-list))
      (t
       (prog1
           (car lambda-list)
         (setf lambda-list (cdr lambda-list)))))))

(defun comp--add-call-cstr ()
  "Add args assumptions for each function of which the type specifier is known."
  (cl-loop
   for bb being each hash-value of (comp-func-blocks comp-func)
   do
   (comp--loop-insn-in-block bb
     (when-let* ((match
                  (pcase insn
                    (`(set ,lhs (,(pred comp--call-op-p) ,f . ,args))
                     (when-let* ((cstr-f (comp--get-function-cstr f)))
                       (cl-values f cstr-f lhs args)))
                    (`(,(pred comp--call-op-p) ,f . ,args)
                     (when-let* ((cstr-f (comp--get-function-cstr f)))
                       (cl-values f cstr-f nil args))))))
       (cl-multiple-value-bind (f cstr-f lhs args) match
         (cl-loop
          with gen = (comp--lambda-list-gen (comp-cstr-f-args cstr-f))
          for arg in args
          for cstr = (funcall gen)
          for target = (comp--cond-cstrs-target-mvar arg insn bb)
          unless (comp-cstr-p cstr)
            do (signal 'native-ice
                       (list "Incoherent type specifier for function" f))
          when (and target
                    ;; No need to add call constraints if this is t
                    ;; (bug#45812 bug#45705 bug#45751).
                    (not (equal comp-cstr-t cstr))
                    (or (null lhs)
                        (not (eql (comp-mvar-slot lhs)
                                  (comp-mvar-slot target)))))
            do (comp--emit-call-cstr target insn-cell cstr)))))))

(defun comp--add-cstrs (_)
  "Rewrite conditional branches adding appropriate `assume' insns.
This is introducing and placing `assume' insns in use by fwprop
to propagate conditional branch test information on target basic
blocks."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 1)
                        ;; No point to run this on dynamic scope as
                        ;; this pass is effective only on local
                        ;; variables.
			(comp-func-l-p f)
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f)
                     (comp-pass (make-hash-table :test #'eq)))
                 (comp--collect-rhs)
		 (comp--add-cond-cstrs-simple)
                 (comp--add-cond-cstrs)
                 (comp--add-call-cstr)
                 (comp--log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; pure-func pass specific code.

;; Simple IPA pass to infer function purity of functions not
;; explicitly declared as such.  This is effective only at speed 3 to
;; avoid optimizing-out functions and preventing their redefinition
;; being effective.

(defun comp--collect-calls (f)
  "Return a list with all the functions called by F."
  (cl-loop
   with h = (make-hash-table :test #'eq)
   for b being each hash-value of (comp-func-blocks f)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(set ,_lval (,(pred comp--call-op-p) ,f . ,_rest))
             (puthash f t h))
            (`(,(pred comp--call-op-p) ,f . ,_rest)
             (puthash f t h))))
   finally return (cl-loop
                   for f being each hash-key of h
                   collect (if (stringp f)
                               (comp-func-name
                                (gethash f
                                         (comp-ctxt-funcs-h comp-ctxt)))
                             f))))

(defun comp--pure-infer-func (f)
  "If all functions called by F are pure then F is pure too."
  (when (and (cl-every (lambda (x)
                         (or (comp--function-pure-p x)
                             (eq x (comp-func-name f))))
                       (comp--collect-calls f))
             (not (eq (comp-func-pure f) t)))
    (comp-log (format "%s inferred to be pure" (comp-func-name f)))
    (setf (comp-func-pure f) t)))

(defun comp--ipa-pure (_)
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
              do (comp--pure-infer-func f)
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

(cl-defun make--comp--ssa-mvar (&rest rest &key _slot _constant _type)
  "Same as `make--comp-mvar' but set the `id' slot."
  (let ((mvar (apply #'make--comp-mvar rest)))
    (setf (comp-mvar-id mvar) (sxhash-eq mvar))
    mvar))

(defun comp--clean-ssa (f)
  "Clean-up SSA for function F."
  (setf (comp-func-edges-h f) (make-hash-table))
  (cl-loop
   for b being each hash-value of (comp-func-blocks f)
   do (setf (comp-block-in-edges b) ()
            (comp-block-out-edges b) ()
            (comp-block-idom b) nil
            (comp-block-df b) (make-hash-table)
            (comp-block-post-num b) nil
            (comp-block-final-frame b) nil
            ;; Prune all phis.
            (comp-block-insns b) (cl-loop for insn in (comp-block-insns b)
                                          unless (eq 'phi (car insn))
                                            collect insn))))

(defun comp--compute-edges ()
  "Compute the basic block edges for the current function."
  (cl-loop with blocks = (comp-func-blocks comp-func)
           for bb being each hash-value of blocks
           for last-insn = (car (last (comp-block-insns bb)))
           for (op first second third forth) = last-insn
           do (cl-case op
                (jump
                 (comp--edge-make :src bb :dst (gethash first blocks)))
                (cond-jump
                 (comp--edge-make :src bb :dst (gethash third blocks))
                 (comp--edge-make :src bb :dst (gethash forth blocks)))
                (cond-jump-narg-leq
                 (comp--edge-make :src bb :dst (gethash second blocks))
                 (comp--edge-make :src bb :dst (gethash third blocks)))
                (push-handler
                 (comp--edge-make :src bb :dst (gethash third blocks))
                 (comp--edge-make :src bb :dst (gethash forth blocks)))
                (return)
                (unreachable)
                (otherwise
                 (signal 'native-ice
                         (list "block does not end with a branch"
                               bb
                               (comp-func-name comp-func)))))
           ;; Update edge refs into blocks.
           finally
           (cl-loop
            for edge being the hash-value in (comp-func-edges-h comp-func)
            do
            (push edge
                  (comp-block-out-edges (comp-edge-src edge)))
            (push edge
                  (comp-block-in-edges (comp-edge-dst edge))))
           (comp--log-edges comp-func)))

(defun comp--collect-rev-post-order (basic-block)
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

(defun comp--compute-dominator-tree ()
  "Compute immediate dominators for each basic block in current function."
  ;; Originally based on: "A Simple, Fast Dominance Algorithm"
  ;; Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001).
  (cl-flet ((intersect (b1 b2)
              (let ((finger1 (comp-block-post-num b1))
                    (finger2 (comp-block-post-num b2)))
                (while (not (= finger1 finger2))
                  (while (< finger1 finger2)
                    (setf b1 (comp-block-idom b1)
                          finger1 (comp-block-post-num b1)))
                  (while (< finger2 finger1)
                    (setf b2 (comp-block-idom b2)
                          finger2 (comp-block-post-num b2))))
                b1))
            (first-processed (l)
              (if-let* ((p (cl-find-if #'comp-block-idom l)))
                  p
                (signal 'native-ice '("can't find first preprocessed")))))

    (when-let* ((blocks (comp-func-blocks comp-func))
                (entry (gethash 'entry blocks))
                ;; No point to go on if the only bb is 'entry'.
                (bb0 (gethash 'bb_0 blocks)))
      (cl-loop
       with rev-bb-list = (comp--collect-rev-post-order entry)
       with changed = t
       while changed
       initially (progn
                   (comp-log "Computing dominator tree...\n" 2)
                   (setf (comp-block-idom entry) entry)
                   ;; Set the post order number.
                   (cl-loop for name in (reverse rev-bb-list)
                            for b = (gethash name blocks)
                            for i from 0
                            do (setf (comp-block-post-num b) i)))
       do (cl-loop
           for name in (cdr rev-bb-list)
           for b = (gethash name blocks)
           for preds = (comp--block-preds b)
           for new-idom = (first-processed preds)
           initially (setf changed nil)
           do (cl-loop for p in (delq new-idom preds)
                       when (comp-block-idom p)
                       do (setf new-idom (intersect p new-idom)))
           unless (eq (comp-block-idom b) new-idom)
           do (setf (comp-block-idom b) (unless (and (comp-block-lap-p new-idom)
                                                    (comp-block-lap-no-ret
                                                     new-idom))
                                         new-idom)
                    changed t))))))

(defun comp--compute-dominator-frontiers ()
  "Compute the dominator frontier for each basic block in `comp-func'."
  ;; Originally based on: "A Simple, Fast Dominance Algorithm"
  ;; Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001).
  (cl-loop with blocks = (comp-func-blocks comp-func)
           for b-name being each hash-keys of blocks
           using (hash-value b)
           for preds = (comp--block-preds b)
           when (length> preds 1) ; All joins
           do (cl-loop for p in preds
                       for runner = p
                       do (while (not (eq runner (comp-block-idom b)))
                            (puthash b-name b (comp-block-df runner))
                            (setf runner (comp-block-idom runner))))))

(defun comp--log-block-info ()
  "Log basic blocks info for the current function."
  (maphash (lambda (name bb)
             (let ((dom (comp-block-idom bb))
                   (df (comp-block-df bb)))
               (comp-log (format "block: %s idom: %s DF %s\n"
                                 name
                                 (when dom (comp-block-name dom))
                                 (cl-loop for b being each hash-keys of df
                                          collect b))
                         3)))
           (comp-func-blocks comp-func)))

(defun comp--place-phis ()
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
                      when (or (and (comp--assign-op-p op)
                                    (eql slot-n (comp-mvar-slot (cadr insn))))
                               ;; fetch-handler is after a non local
                               ;; therefore clobbers all frame!!!
                               (eq op 'fetch-handler))
                        return t)))

    (cl-loop for i from (- (comp-func-vframe-size comp-func))
                   below (comp-func-frame-size comp-func)
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

(defun comp--dom-tree-walker (bb pre-lambda post-lambda)
  "Dominator tree walker function starting from basic block BB.
PRE-LAMBDA and POST-LAMBDA are called in pre or post-order if non-nil."
  (when pre-lambda
    (funcall pre-lambda bb))
  (when-let* ((out-edges (comp-block-out-edges bb)))
    (cl-loop for ed in out-edges
             for child = (comp-edge-dst ed)
             when (eq bb (comp-block-idom child))
             ;; Current block is the immediate dominator then recur.
             do (comp--dom-tree-walker child pre-lambda post-lambda)))
  (when post-lambda
    (funcall post-lambda bb)))

(cl-defstruct (comp--ssa (:copier nil))
  "Support structure used while SSA renaming."
  (frame (comp--new-frame (comp-func-frame-size comp-func)
                         (comp-func-vframe-size comp-func) t)
         :type comp-vec
         :documentation "`comp-vec' of m-vars."))

(defun comp--ssa-rename-insn (insn frame)
  (cl-loop
   for slot-n from (- (comp-func-vframe-size comp-func))
              below (comp-func-frame-size comp-func)
   do
   (cl-flet ((targetp (x)
               ;; Ret t if x is an mvar and target the correct slot number.
               (and (comp-mvar-p x)
                    (eql slot-n (comp-mvar-slot x))))
             (new-lvalue ()
               ;; If is an assignment make a new mvar and put it as l-value.
               (let ((mvar (make--comp--ssa-mvar :slot slot-n)))
                 (setf (comp-vec-aref frame slot-n) mvar
                       (cadr insn) mvar))))
     (pcase insn
       (`(setimm ,lval ,_imm)
        (when (targetp lval)
          (new-lvalue)))
       (`(,(pred comp--assign-op-p) ,(pred targetp) . ,_)
        (let ((mvar (comp-vec-aref frame slot-n)))
          (setf (cddr insn) (cl-nsubst-if mvar #'targetp (cddr insn))))
        (new-lvalue))
       (`(fetch-handler . ,_)
        ;; Clobber all no matter what!
        (setf (comp-vec-aref frame slot-n) (make--comp--ssa-mvar :slot slot-n)))
       (`(phi ,n)
        (when (equal n slot-n)
          (new-lvalue)))
       (_
        (let ((mvar (comp-vec-aref frame slot-n)))
          (setcdr insn (cl-nsubst-if mvar #'targetp (cdr insn)))))))))

(defun comp--ssa-rename ()
  "Entry point to rename into SSA within the current function."
  (comp-log "Renaming\n" 2)
  (let ((visited (make-hash-table)))
    (cl-labels ((ssa-rename-rec (bb in-frame)
                  (unless (gethash bb visited)
                    (puthash bb t visited)
                    (cl-loop for insn in (comp-block-insns bb)
                             do (comp--ssa-rename-insn insn in-frame))
                    (setf (comp-block-final-frame bb)
                          (copy-sequence in-frame))
                    (when-let* ((out-edges (comp-block-out-edges bb)))
                      (cl-loop
                       for ed in out-edges
                       for child = (comp-edge-dst ed)
                       ;; Provide a copy of the same frame to all children.
                       do (ssa-rename-rec child (comp-vec-copy in-frame)))))))

      (ssa-rename-rec (gethash 'entry (comp-func-blocks comp-func))
                      (comp--new-frame (comp-func-frame-size comp-func)
                                      (comp-func-vframe-size comp-func)
                                      t)))))

(defun comp--finalize-phis ()
  "Fixup r-values into phis in all basic blocks."
  (cl-flet ((finalize-phi (args b)
              ;; Concatenate into args all incoming m-vars for this phi.
              (setcdr args
                      (cl-loop with slot-n = (comp-mvar-slot (car args))
                               for e in (comp-block-in-edges b)
                               for b = (comp-edge-src e)
                               for in-frame = (comp-block-final-frame b)
                               collect (list (comp-vec-aref in-frame slot-n)
                                             (comp-block-name b))))))

    (cl-loop for b being each hash-value of (comp-func-blocks comp-func)
             do (cl-loop for (op . args) in (comp-block-insns b)
                         when (eq op 'phi)
                           do (finalize-phi args b)))))

(defun comp--remove-unreachable-blocks ()
  "Remove unreachable basic blocks.
Return t when one or more block was removed, nil otherwise."
  (cl-loop
   with ret
   for bb being each hash-value of (comp-func-blocks comp-func)
   for bb-name = (comp-block-name bb)
   when (and (not (eq 'entry bb-name))
             (null (comp-block-idom bb)))
   do
   (comp-log (format "Removing block: %s" bb-name) 1)
   (remhash bb-name (comp-func-blocks comp-func))
   (setf (comp-func-ssa-status comp-func) t
              ret t)
   finally return ret))

(defun comp--ssa-function (function)
  "Port into minimal SSA FUNCTION."
  (let* ((comp-func function)
         (ssa-status (comp-func-ssa-status function)))
    (unless (eq ssa-status t)
      (cl-loop
       when (eq ssa-status 'dirty)
         do (comp--clean-ssa function)
       do (comp--compute-edges)
          (comp--compute-dominator-tree)
       until (null (comp--remove-unreachable-blocks)))
      (comp--compute-dominator-frontiers)
      (comp--log-block-info)
      (comp--place-phis)
      (comp--ssa-rename)
      (comp--finalize-phis)
      (comp--log-func comp-func 3)
      (setf (comp-func-ssa-status function) t))))

(defun comp--ssa ()
  "Port all functions into minimal SSA all functions."
  (cl-loop for f being the hash-value in (comp-ctxt-funcs-h comp-ctxt)
           do (comp--ssa-function f)))


;;; propagate pass specific code.
;; A very basic propagation pass follows.
;; This propagates values and types plus ref property in the control flow graph.
;; This is also responsible for removing function calls to pure functions if
;; possible.

(defconst comp--fwprop-max-insns-scan 4500
  ;; Chosen as ~ the greatest required value for full convergence
  ;; native compiling all Emacs code-base.
  "Max number of scanned insn before giving-up.")

(defun comp--copy-insn-rec (insn)
  "Deep copy INSN."
  ;; Adapted from `copy-tree'.
  (if (consp insn)
      (let (result)
	(while (consp insn)
	  (let ((newcar (car insn)))
	    (if (or (consp (car insn)) (comp-mvar-p (car insn)))
		(setf newcar (comp--copy-insn (car insn))))
	    (push newcar result))
	  (setf insn (cdr insn)))
	(nconc (nreverse result)
               (if (comp-mvar-p insn) (comp--copy-insn insn) insn)))
    (if (comp-mvar-p insn)
        (copy-comp-mvar insn)
      insn)))

(defun comp--copy-insn (insn)
  "Deep copy INSN."
  (pcase insn
    (`(setimm ,mvar ,imm)
     `(setimm ,(copy-comp-mvar mvar) ,imm))
    (_ (comp--copy-insn-rec insn))))

(defmacro comp--apply-in-env (func &rest args)
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

(defun comp--fwprop-prologue ()
  "Prologue for the propagate pass.
Here goes everything that can be done not iteratively (read once).
Forward propagate immediate involed in assignments." ; FIXME: Typo.  Involved or invoked?
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(setimm ,lval ,v)
             (setf (comp-cstr-imm lval) v))))))

(defun comp--function-foldable-p (f args)
  "Given function F called with ARGS, return non-nil when optimizable."
  (and (comp--function-pure-p f)
       (cl-every #'comp-cstr-imm-vld-p args)))

(defun comp--function-call-maybe-fold (insn f args)
  "Given INSN, when F is pure if all ARGS are known, remove the function call.
Return non-nil if the function is folded successfully."
  (cl-flet ((rewrite-insn-as-setimm (insn value)
               ;; See `comp--emit-setimm'.
               (comp--add-const-to-relocs value)
               (setf (car insn) 'setimm
                     (cddr insn) `(,value))))
    (cond
     ((eq f 'symbol-value)
      (when-let* ((arg0 (car args))
                  (const (comp-cstr-imm-vld-p arg0))
                  (ok-to-optim (member (comp-cstr-imm arg0)
                                       comp-symbol-values-optimizable)))
        (rewrite-insn-as-setimm insn (symbol-value (comp-cstr-imm
                                                    (car args))))))
     ((comp--function-foldable-p f args)
      (ignore-errors
        ;; No point to complain here in case of error because we
        ;; should do basic block pruning in order to be sure that this
        ;; is not dead-code.  This is now left to gcc, to be
        ;; implemented only if we want a reliable diagnostic here.
        (let* ((f (if-let* ((f-in-ctxt (comp--symbol-func-to-fun f)))
                      ;; If the function is IN the compilation ctxt
                      ;; and know to be pure.
                      (comp-func-byte-func f-in-ctxt)
                    f))
               (value (comp--apply-in-env f (mapcar #'comp-cstr-imm args))))
          (rewrite-insn-as-setimm insn value)))))))

(defun comp--fwprop-call (insn lval f args)
  "Propagate on a call INSN into LVAL.
F is the function being called with arguments ARGS.
Fold the call in case."
  (unless (comp--function-call-maybe-fold insn f args)
    (when (and (eq 'funcall f)
               (comp-cstr-imm-vld-p (car args)))
      (setf f (comp-cstr-imm (car args))
            args (cdr args)))
    (when-let* ((cstr-f (comp--get-function-cstr f)))
      (let ((cstr (comp-cstr-f-ret cstr-f)))
        (when (comp-cstr-empty-p cstr)
          ;; Store it to be rewritten as non local exit.
          (setf (comp-block-lap-non-ret-insn comp-block) insn))
        (comp-cstr-shallow-copy lval cstr)))
    (cl-case f
      (+ (comp-cstr-add lval args))
      (- (comp-cstr-sub lval args))
      (1+ (comp-cstr-add lval `(,(car args) ,comp-cstr-one)))
      (1- (comp-cstr-sub lval `(,(car args) ,comp-cstr-one)))
      (record (when (comp-cstr-imm-vld-p (car args))
                (comp-cstr-shallow-copy lval
                                        (comp-type-spec-to-cstr
                                         (comp-cstr-imm (car args)))))))))

(defun comp--fwprop-insn (insn)
  "Propagate within INSN."
  (pcase insn
    (`(set ,lval ,rval)
     (pcase rval
       (`(,(or 'call 'callref) ,f . ,args)
        (comp--fwprop-call insn lval f args))
       (`(,(or 'direct-call 'direct-callref) ,f . ,args)
        (let ((f (comp-func-name (gethash f (comp-ctxt-funcs-h comp-ctxt)))))
          (comp--fwprop-call insn lval f args)))
       (_
        (comp-cstr-shallow-copy lval rval))))
    (`(assume ,lval ,(and (pred comp-mvar-p) rval))
     ;; NOTE we should probably assert this case in the future when
     ;; will be possible.
     (comp-cstr-shallow-copy lval rval))
    (`(assume ,lval (,kind . ,operands))
     (cl-case kind
       (and
        (apply #'comp-cstr-intersection lval operands))
       (and-nhc
        (apply #'comp-cstr-intersection-no-hashcons lval operands))
       (not
        ;; Prevent double negation!
        (unless (comp-cstr-neg (car operands))
          (comp-cstr-value-negation lval (car operands))))
       (>
        (comp-cstr-> lval (car operands) (cadr operands)))
       (>=
        (comp-cstr->= lval (car operands) (cadr operands)))
       (<
        (comp-cstr-< lval (car operands) (cadr operands)))
       (<=
        (comp-cstr-<= lval (car operands) (cadr operands)))
       (=
        (comp-cstr-= lval (car operands) (cadr operands)
                     (comp-ctxt-non-materializable-objs-h comp-ctxt)))))
    (`(setimm ,lval ,v)
     (setf (comp-cstr-imm lval) v))
    (`(phi ,lval . ,rest)
     (let* ((from-latch (cl-some
                         (lambda (x)
                           (let* ((bb-name (cadr x))
                                  (bb (gethash bb-name
                                               (comp-func-blocks comp-func))))
                             (or (comp-latch-p bb)
                                 (when (comp-block-cstr-p bb)
                                   (comp-latch-p (car (comp--block-preds bb)))))))
                         rest))
            (prop-fn (if from-latch
                         #'comp-cstr-union-no-range
                       #'comp-cstr-union))
            (rvals (mapcar #'car rest)))
       (apply prop-fn lval rvals)))))

(defun comp--fwprop* ()
  "Propagate for set* and phi operands.
Return t if something was changed."
  (cl-loop named outer
           with modified = nil
           with i = 0
           for b being each hash-value of (comp-func-blocks comp-func)
           do (cl-loop
               with comp-block = b
               for insn in (comp-block-insns b)
               for orig-insn = (unless modified
                                 ;; Save consing after 1st change.
                                 (comp--copy-insn insn))
               do
               (comp--fwprop-insn insn)
               (incf i)
               when (and (null modified) (not (equal insn orig-insn)))
                 do (setf modified t))
               when (> i comp--fwprop-max-insns-scan)
                 do (cl-return-from outer nil)
           finally return modified))

(defun comp--rewrite-non-locals ()
  "Make explicit in LIMPLE non-local exits if identified."
  (cl-loop
   for bb being each hash-value of (comp-func-blocks comp-func)
   for non-local-insn = (and (comp-block-lap-p bb)
                             (comp-block-lap-non-ret-insn bb))
   when non-local-insn
   do
   ;; Rework the current block.
   (let* ((insn-seq (memq non-local-insn (comp-block-insns bb))))
     (setf (comp-block-lap-non-ret-insn bb) ()
           (comp-block-lap-no-ret bb) t
           (comp-block-out-edges bb) ()
           ;; Prune unnecessary insns!
           (cdr insn-seq) '((unreachable))
           (comp-func-ssa-status comp-func) 'dirty))))

(defun comp--fwprop (_)
  "Forward propagate types and consts within the lattice."
  (comp--ssa)
  (comp--dead-code)
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        ;; FIXME remove the following condition when tested.
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f))
                 (comp--fwprop-prologue)
                 (cl-loop
                  for i from 1 to 100
                  while (comp--fwprop*)
                  finally
                  (when (= i 100)
                    (display-warning
                     'native-compiler
                     (format "fwprop pass jammed into %s?" (comp-func-name f))))
                  (comp-log (format "Propagation run %d times\n" i) 2))
                 (comp--rewrite-non-locals)
                 (comp--log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Type check optimizer pass specific code.

;; This pass optimize-out unnecessary type checks, that is calls to
;; `type-of' and corresponding conditional branches.
;;
;; This is often advantageous in cases where a function manipulates an
;; object with several slot accesses like:
;;
;; (cl-defstruct foo a b c)
;; (defun bar (x)
;;   (setf (foo-a x) 3)
;;   (+ (foo-b x) (foo-c x)))
;;
;; After x is accessed and type checked once, it's proved to be of type
;; foo, and no other type checks are required.

;; At present running this pass over the whole Emacs codebase triggers
;; the optimization of 1972 type checks.

(defun comp--type-check-optim-block (block)
  "Optimize conditional branches in BLOCK when possible."
  (cl-loop
   named in-the-basic-block
   for insns-seq on (comp-block-insns block)
   do (pcase insns-seq
        (`((set ,(and (pred comp-mvar-p) mvar-tested-copy)
                ,(and (pred comp-mvar-p) mvar-tested))
           (set ,(and (pred comp-mvar-p) mvar-1)
                (call type-of ,(and (pred comp-mvar-p) mvar-tested-copy)))
           (set ,(and (pred comp-mvar-p) mvar-2)
                (call symbol-value ,(and (pred comp-cstr-cl-tag-p) mvar-tag)))
           (set ,(and (pred comp-mvar-p) mvar-3)
                (call memq ,(and (pred comp-mvar-p) mvar-1) ,(and (pred comp-mvar-p) mvar-2)))
           (cond-jump ,(and (pred comp-mvar-p) mvar-3) ,(pred comp-mvar-p) ,bb1 ,bb2))
         (cl-assert (comp-cstr-imm-vld-p mvar-tag))
         (when (comp-cstr-type-p mvar-tested (comp-cstr-cl-tag mvar-tag))
           (comp-log (format "Optimizing conditional branch %s in function: %s"
                             bb1
                             (comp-func-name comp-func))
                     3)
           (setf (car insns-seq) '(comment "optimized by comp--type-check-optim")
                 (cdr insns-seq) `((jump ,bb2))
                 ;; Set the SSA status as dirty so
                 ;; `comp--ssa-function' will remove the unreachable
                 ;; branches later.
                 (comp-func-ssa-status comp-func) 'dirty))))))

(defun comp--type-check-optim (_)
  "Optimize conditional branches when possible."
  (cl-loop
   for f being each hash-value of (comp-ctxt-funcs-h comp-ctxt)
   for comp-func = f
   when (>= (comp-func-speed f) 2)
   do (cl-loop
       for b being each hash-value of (comp-func-blocks f)
       do (comp--type-check-optim-block b)
       finally
       (progn
         (when (eq (comp-func-ssa-status f) 'dirty)
           (comp--ssa-function f))
         (comp--log-func comp-func 3)))))


;;; Call optimizer pass specific code.
;; This pass is responsible for the following optimizations:
;; - Call to subrs that are in defined in the C source and are passing through
;;   funcall trampoline gets optimized into normal indirect calls.
;;   This makes effectively this calls equivalent to all the subrs that got
;;   dedicated byte-code ops.
;;   Triggered at native-comp-speed >= 2.
;; - Recursive calls gets optimized into direct calls.
;;   Triggered at native-comp-speed >= 2.
;; - Intra compilation unit procedure calls gets optimized into direct calls.
;;   This can be a big win and even allow gcc to inline but does not make
;;   function in the compilation unit re-definable safely without recompiling
;;   the full compilation unit.
;;   For this reason this is triggered only at native-comp-speed == 3.

(defun comp--func-in-unit (func)
  "Given FUNC return the `comp-fun' definition in the current context.
FUNCTION can be a function-name or byte compiled function."
  (if (symbolp func)
      (comp--symbol-func-to-fun func)
    (cl-assert (byte-code-function-p func))
    (gethash func (comp-ctxt-byte-func-to-func-h comp-ctxt))))

(defun comp--call-optim-form-call (callee args)
  (cl-flet ((fill-args (args total)
              ;; Fill missing args to reach TOTAL
              (append args (cl-loop repeat (- total (length args))
                                    collect (make--comp-mvar :constant nil)))))
    (when (and callee
               (or (symbolp callee)
                   (gethash callee (comp-ctxt-byte-func-to-func-h comp-ctxt)))
               (not (memq callee native-comp-never-optimize-functions)))
      (let* ((f (if (symbolp callee)
                    (symbol-function callee)
                  (cl-assert (byte-code-function-p callee))
                  callee))
             ;; Below call to `subrp' returns nil on an advised
             ;; primitive F, so that we do not optimize calls to F
             ;; with the funcall trampoline removal below.  But if F
             ;; is advised while we compile its call, it is very
             ;; likely to be advised also when that call is executed.
             ;; And in that case an "unoptimized" call to F is
             ;; actually cheaper since it avoids the call to the
             ;; intermediate native trampoline (bug#67005).
             (subrp (subrp f))
             (comp-func-callee (comp--func-in-unit callee)))
        (cond
         ((and subrp (not (native-comp-function-p f)))
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
               (comp-func-c-name comp-func-callee)
               (or (and (>= (comp-func-speed comp-func) 3)
                        (comp--func-unique-in-cu-p callee))
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
         ((comp--type-hint-p callee)
          `(call ,callee ,@args)))))))

(defun comp--call-optim-func ()
  "Perform the trampoline call optimization for the current function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (comp--loop-insn-in-block b
        (pcase insn
          (`(set ,lval (callref funcall ,f . ,rest))
           (when-let* ((ok (comp-cstr-imm-vld-p f))
                       (new-form (comp--call-optim-form-call
                                  (comp-cstr-imm f) rest)))
             (setf insn `(set ,lval ,new-form))))
          (`(callref funcall ,f . ,rest)
           (when-let* ((ok (comp-cstr-imm-vld-p f))
                       (new-form (comp--call-optim-form-call
                                  (comp-cstr-imm f) rest)))
             (setf insn new-form)))))))

(defun comp--call-optim (_)
  "Try to optimize out funcall trampoline usage when possible."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        (comp-func-l-p f))
               (let ((comp-func f))
                 (comp--call-optim-func))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Dead code elimination pass specific code.
;; This simple pass try to eliminate insns became useful after propagation.
;; Even if gcc would take care of this is good to perform this here
;; in the hope of removing memory references.
;;
;; This pass can be run as last optim.

(defun comp--collect-mvar-ids (insn)
  "Collect the m-var unique identifiers into INSN."
  (cl-loop for x in insn
           if (consp x)
             append (comp--collect-mvar-ids x)
           else
             when (comp-mvar-p x)
               collect (comp-mvar-id x)))

(defun comp--dead-assignments-func ()
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
         if (comp--assign-op-p op)
           do (push (comp-mvar-id arg0) l-vals)
              (unless (eq op 'setimm)
                (setf r-vals (nconc (comp--collect-mvar-ids rest) r-vals)))
         else
           do (setf r-vals (nconc (comp--collect-mvar-ids insn) r-vals))))
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
       do (comp--loop-insn-in-block b
            (cl-destructuring-bind (op &optional arg0 arg1 &rest rest) insn
              (when (and (comp--assign-op-p op)
                         (memq (comp-mvar-id arg0) nuke-list))
                (setf insn
                      (if (comp--limple-insn-call-p arg1)
                          arg1
                        `(comment ,(format "optimized out: %s"
                                           insn))))))))
      nuke-list)))

(defun comp--dead-code ()
  "Dead code elimination."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        ;; FIXME remove the following condition when tested.
                        (not (comp-func-has-non-local f)))
               (cl-loop
                for comp-func = f
                for i from 1
                while (comp--dead-assignments-func)
                finally (comp-log (format "dead code rm run %d times\n" i) 2)
                (comp--log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Tail Call Optimization pass specific code.

(defun comp--form-tco-call-seq (args)
  "Generate a TCO sequence for ARGS."
  `(,@(cl-loop for arg in args
               for i from 0
               collect `(set ,(make--comp-mvar :slot i) ,arg))
    (jump bb_0)))

(defun comp--tco-func ()
  "Try to pattern match and perform TCO within the current function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       named in-the-basic-block
       for insns-seq on (comp-block-insns b)
       do (pcase insns-seq
            (`((set ,l-val (direct-call ,func . ,args))
               ;; (comment ,_comment)
               (return ,ret-val))
             (when (and (string= func (comp-func-c-name comp-func))
                        (eq l-val ret-val))
               (let ((tco-seq (comp--form-tco-call-seq args)))
                 (setf (car insns-seq) (car tco-seq)
                       (cdr insns-seq) (cdr tco-seq)
                       (comp-func-ssa-status comp-func) 'dirty)
                 (cl-return-from in-the-basic-block))))))))

(defun comp--tco (_)
  "Simple peephole pass performing self TCO."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 3)
                        (comp-func-l-p f)
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f))
                 (comp--tco-func)
                 (comp--log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Type hint removal pass specific code.

;; This must run after all SSA prop not to have the type hint
;; information overwritten.

(defun comp--remove-type-hints-func ()
  "Remove type hints from the current function.
These are substituted with a normal `set' op."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (comp--loop-insn-in-block b
        (pcase insn
          (`(set ,l-val (call ,(pred comp--type-hint-p) ,r-val))
           (setf insn `(set ,l-val ,r-val)))))))

(defun comp--remove-type-hints (_)
  "Dead code elimination."
  (maphash (lambda (_ f)
             (when (>= (comp-func-speed f) 2)
               (let ((comp-func f))
                 (comp--remove-type-hints-func)
                 (comp--log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Sanitizer pass specific code.

;; This pass aims to verify compile-time value-type predictions during
;; execution of the code.
;; The sanitizer pass injects a call to 'helper_sanitizer_assert' before
;; each conditional branch.  'helper_sanitizer_assert' will verify that
;; the variable tested by the conditional branch is of the predicted
;; value type, or signal an error otherwise.

;;; Example:

;; Assume we want to compile 'test.el' and test the function `foo'
;; defined in it.  Then:

;;  - Native-compile 'test.el' instrumenting it for sanitizer usage:
;;      (let ((comp-sanitizer-emit t))
;;        (load (native-compile "test.el")))

;;  - Run `foo' with the sanitizer active:
;;      (let ((comp-sanitizer-active t))
;;        (foo))

(defvar comp-sanitizer-emit nil
  "Gates the sanitizer pass.
This is intended to be used only for development and verification of
the native compiler.")

(defun comp--sanitizer (_)
  (when comp-sanitizer-emit
    (cl-loop
     for f being each hash-value of (comp-ctxt-funcs-h comp-ctxt)
     for comp-func = f
     unless (comp-func-has-non-local comp-func)
     do
     (cl-loop
      for b being each hash-value of (comp-func-blocks f)
      do
      (cl-loop
       named in-the-basic-block
       for insns-seq on (comp-block-insns b)
       do (pcase insns-seq
            (`((cond-jump ,(and (pred comp-mvar-p) mvar-tested)
                          ,(pred comp-mvar-p) ,_bb1 ,_bb2))
             (let ((type (comp-cstr-to-type-spec mvar-tested))
                   (insn (car insns-seq)))
               ;; No need to check if type is t.
               (unless (eq type t)
                 (comp--add-const-to-relocs type)
                 (setcar
                  insns-seq
                  (comp--call 'helper_sanitizer_assert
                              mvar-tested
                              (make--comp-mvar :constant type)))
                 (setcdr insns-seq (list insn)))
               ;; (setf (comp-func-ssa-status comp-func) 'dirty)
               (cl-return-from in-the-basic-block))))))
     do (comp--log-func comp-func 3))))


;;; Function types pass specific code.

(defun comp--compute-function-type (_ func)
  "Compute type specifier for `comp-func' FUNC.
Set it into the `type' slot."
  (when (and (comp-func-l-p func)
             (comp-mvar-p (comp-func-type func)))
    (let* ((comp-func (make-comp-func))
           (res-mvar (apply #'comp-cstr-union
                            (make-comp-cstr)
                            (cl-loop
                             with res = nil
                             for bb being the hash-value in (comp-func-blocks
                                                             func)
                             do (cl-loop
                                 for insn in (comp-block-insns bb)
                                 ;; Collect over every exit point the returned
                                 ;; mvars and union results.
                                 do (pcase insn
                                      (`(return ,mvar)
                                       (push mvar res))))
                             finally return res)))
           (type `(function ,(comp--args-to-lambda-list (comp-func-l-args func))
                            ,(comp-cstr-to-type-spec res-mvar))))
      (comp--add-const-to-relocs type)
      ;; Fix it up.
      (setf (comp-cstr-imm (comp-func-type func)) type))))

(defun comp--compute-function-types (_)
  "Compute and store the type specifier for all functions."
  (maphash #'comp--compute-function-type (comp-ctxt-funcs-h comp-ctxt)))


;;; Final pass specific code.

(defun comp--args-to-lambda-list (args)
  "Return a lambda list for ARGS."
  (cl-loop
   with res
   repeat (comp-args-base-min args)
   do (push t res)
   finally
   (if (comp-args-p args)
       (cl-loop
        with n = (- (comp-args-max args) (comp-args-min args))
        initially (unless (zerop n)
                    (push '&optional res))
        repeat n
        do (push t res))
     (cl-loop
      with n = (- (comp-nargs-nonrest args) (comp-nargs-min args))
      initially (unless (zerop n)
                  (push '&optional res))
      repeat n
      do (push t res)
      finally (when (comp-nargs-rest args)
                (push '&rest res)
                (push 't res))))
   (cl-return (reverse res))))

(defun comp--finalize-container (cont)
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
                             ;; This prints as #$, so we can assert this
                             ;; value does not remain in the data vector
                             comp--\#$
                           obj))))

(defun comp--finalize-relocs ()
  "Finalize data containers for each relocation class.
Remove immediate duplicates within relocation classes.
Update all insn accordingly."
  ;; Symbols imported by C inlined functions.  We do this here because
  ;; is better to add all objs to the relocation containers before we
  ;; compacting them.
  (mapc #'comp--add-const-to-relocs '(nil t consp listp symbol-with-pos-p))

  (let* ((d-default (comp-ctxt-d-default comp-ctxt))
         (d-default-idx (comp-data-container-idx d-default))
         (d-ephemeral (comp-ctxt-d-ephemeral comp-ctxt))
         (d-ephemeral-idx (comp-data-container-idx d-ephemeral)))
    ;; Remove entries in d-ephemeral already present in d-default
    (cl-loop for obj being each hash-keys of d-ephemeral-idx
             when (gethash obj d-default-idx)
               do (remhash obj d-ephemeral-idx))
    ;; Fix-up indexes in each relocation class and fill corresponding
    ;; reloc lists.
    (mapc #'comp--finalize-container (list d-default d-ephemeral))
    ;; Make a vector from the function documentation hash table.
    (cl-loop with h = (comp-ctxt-function-docs comp-ctxt)
             with v = (make-vector (hash-table-count h) nil)
             for idx being each hash-keys of h
             for doc = (gethash idx h)
             do (setf (aref v idx) doc)
             finally
             do (setf (comp-ctxt-function-docs comp-ctxt) v))
    ;; And now we conclude with the following: We need to pass to
    ;; `comp--register-lambda' the index in the relocation array to
    ;; store revived lambdas, but given we know it only now we fix it up
    ;; as last.
    (cl-loop for f being each hash-keys of (comp-ctxt-lambda-fixups-h comp-ctxt)
             using (hash-value mvar)
             with reverse-h = (make-hash-table) ;; Make sure idx is unique.
             for idx = (gethash f d-default-idx)
             do
             (cl-assert (null (gethash idx reverse-h)))
             (cl-assert (fixnump idx))
             (setf (comp-mvar-valset mvar) ()
                   (comp-mvar-range mvar) (list (cons idx idx)))
             (puthash idx t reverse-h))))

(defun comp--compile-ctxt-to-file (name)
  "Compile as native code the current context naming it NAME.
Prepare every function for final compilation and drive the C back-end."
  (let ((dir (file-name-directory name)))
    (comp--finalize-relocs)
    (maphash (lambda (_ f)
               (comp--log-func f 1))
             (comp-ctxt-funcs-h comp-ctxt))
    (unless (file-exists-p dir)
      ;; In case it's created in the meanwhile.
      (ignore-error file-already-exists
        (make-directory dir t)))
    (comp--compile-ctxt-to-file0 name)))

(defun comp--final1 ()
  (comp--init-ctxt)
  (unwind-protect
      (comp--compile-ctxt-to-file (comp-ctxt-output comp-ctxt))
    (comp--release-ctxt)))

(defvar comp-async-compilation nil
  "Non-nil while executing an asynchronous native compilation.")

(defvar comp-running-batch-compilation nil
  "Non-nil when compilation is driven by any `batch-*-compile' function.")

(defun comp--final (_)
  "Final pass driving the C back-end for code emission."
  (unless comp-dry-run
    ;; Always run the C side of the compilation as a sub-process
    ;; unless during bootstrap or async compilation (bug#45056).  GCC
    ;; leaks memory but also interfere with the ability of Emacs to
    ;; detect when a sub-process completes (TODO understand why).
    (if (or comp-running-batch-compilation comp-async-compilation)
	(comp--final1)
      ;; Call comp--final1 in a child process.
      (let* ((output (comp-ctxt-output comp-ctxt))
             (print-escape-newlines t)
             (print-length nil)
             (print-level nil)
             (print-quoted t)
             (print-gensym t)
             (print-circle t)
             (print-escape-multibyte t)
             (expr `((require 'comp)
                     (setf native-comp-verbose ,native-comp-verbose
                           comp-libgccjit-reproducer ,comp-libgccjit-reproducer
                           comp-ctxt ,comp-ctxt
                           native-comp-eln-load-path ',native-comp-eln-load-path
                           native-comp-compiler-options
                           ',native-comp-compiler-options
                           native-comp-driver-options
                           ',native-comp-driver-options
                           byte-compile-warnings ',byte-compile-warnings
                           load-path ',load-path)
                     ,native-comp-async-env-modifier-form
                     (message "Compiling %s..." ',output)
                     (comp--final1)))
             (temp-file (make-temp-file
			 (concat "emacs-int-comp-"
				 (file-name-base output) "-")
			 nil ".el"))
             (default-directory invocation-directory))
	(with-temp-file temp-file
          (insert ";; -*- coding: utf-8-emacs-unix; lexical-binding: t -*-\n")
          (mapc (lambda (e)
                  (insert (prin1-to-string e)))
                expr))
	(with-temp-buffer
          (unwind-protect
              (if (zerop
                   (call-process (expand-file-name invocation-name
                                                   invocation-directory)
				 nil t t "-no-comp-spawn" "-Q" "--batch" "-l"
                                 temp-file))
                  (progn
                    (delete-file temp-file)
                    output)
		(signal 'native-compiler-error (list (buffer-string))))
            (comp-log-to-buffer (buffer-string))))))))


;;; Compiler type hints.
;; Public entry points to be used by user code to give comp
;; suggestions about types.  These are used to implement CL style
;; `cl-the' and hopefully parameter type declaration.
;; Note: types will propagates.
;; WARNING: At speed >= 2 type checking is not performed anymore and suggestions
;; are assumed just to be true. Use with extreme caution...

(defun comp-hint-fixnum (x)
  (declare (ftype (function (t) fixnum))
           (gv-setter (lambda (val) `(setf ,x ,val))))
  x)

(defun comp-hint-cons (x)
  (declare (ftype (function (t) cons))
           (gv-setter (lambda (val) `(setf ,x ,val))))
  x)


;; Primitive function advice machinery

(defun comp--make-lambda-list-from-subr (subr)
  "Given SUBR return the equivalent lambda-list."
  (pcase-let ((`(,min . ,max) (subr-arity subr))
              (lambda-list '()))
    (cl-loop repeat min
             do (push (gensym "arg") lambda-list))
    (if (numberp max)
        (cl-loop
         initially (push '&optional lambda-list)
         repeat (- max min)
         do (push (gensym "arg") lambda-list))
      (push '&rest lambda-list)
      (push (gensym "arg") lambda-list))
    (reverse lambda-list)))

(defun comp--trampoline-abs-filename (subr-name)
  "Return the absolute filename for a trampoline for SUBR-NAME."
  (cl-loop
   with dirs = (if (stringp native-comp-enable-subr-trampolines)
                   (list (expand-file-name native-comp-enable-subr-trampolines
                                           invocation-directory))
                 (if native-compile-target-directory
                     (list (expand-file-name comp-native-version-dir
                                             native-compile-target-directory))
                   (comp-eln-load-path-eff)))
   with rel-filename = (comp-trampoline-filename subr-name)
   for dir in dirs
   for abs-filename = (expand-file-name rel-filename dir)
   unless (file-exists-p dir)
     do (ignore-errors
          (make-directory dir t)
          (cl-return abs-filename))
   when (file-writable-p abs-filename)
     do (cl-return abs-filename)
   ;; Default to some temporary directory if no better option was
   ;; found.
   finally (cl-return
            (make-temp-file (file-name-sans-extension rel-filename) nil ".eln"
                            nil))))

;; Called from comp-run.el
;;;###autoload
(defun comp-trampoline-compile (subr-name)
  "Synthesize compile and return a trampoline for SUBR-NAME."
  (let* ((lambda-list (comp--make-lambda-list-from-subr
                       (symbol-function subr-name)))
         ;; The synthesized trampoline must expose the exact same ABI of
         ;; the primitive we are replacing in the function reloc table.
         (form `(lambda ,lambda-list
                  (let ((f #',subr-name))
                    (,(if (memq '&rest lambda-list) #'apply 'funcall)
                     f
                     ,@(cl-loop
                        for arg in lambda-list
                        unless (memq arg '(&optional &rest))
                        collect arg)))))
         ;; Use speed 1 for compilation speed and not to optimize away
         ;; funcall calls!
         (byte-optimize nil)
         (native-comp-speed 1)
         (lexical-binding t))
    (comp--native-compile
     form nil
     (comp--trampoline-abs-filename subr-name))))


;; Some entry point support code.

;;;###autoload
(defun comp-clean-up-stale-eln (file)
  "Remove all FILE*.eln* files found in `native-comp-eln-load-path'.
The files to be removed are those produced from the original source
filename (including FILE)."
  (when (string-match (rx "-" (group-n 1 (1+ hex)) "-" (1+ hex) ".eln" eos)
                      file)
    (cl-loop
     with filename-hash = (match-string 1 file)
     with regexp = (rx-to-string
                    `(seq "-" ,filename-hash "-" (1+ hex) ".eln" eos))
     for dir in (comp-eln-load-path-eff)
     do (cl-loop
         for f in (when (file-exists-p dir)
		    (directory-files dir t regexp t))
         ;; We may not be able to delete the file if we have no write
         ;; permission.
         do (ignore-error file-error
              (comp-delete-or-replace-file f))))))

;; In use by comp.c.
(defun comp-delete-or-replace-file (oldfile &optional newfile)
  "Replace OLDFILE with NEWFILE.
When NEWFILE is nil just delete OLDFILE.
Takes the necessary steps when dealing with OLDFILE being a
shared library that might be currently loaded into a running Emacs
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
        (t (if newfile
               (rename-file newfile oldfile t)
             (delete-file oldfile)))))

(defun comp--native-compile (function-or-file &optional with-late-load output)
  "Compile FUNCTION-OR-FILE into native code.
When WITH-LATE-LOAD is non-nil, mark the compilation unit for late
load once it finishes compiling.
This serves as internal implementation of `native-compile' but
allowing for WITH-LATE-LOAD to be controlled is in use also for
the deferred compilation mechanism."
  (comp-ensure-native-compiler)
  (unless (or (functionp function-or-file)
              (stringp function-or-file))
    (signal 'native-compiler-error
            (list "Not a function symbol or file" function-or-file)))
  (when (or (null comp-no-spawn) comp-async-compilation)
    (catch 'no-native-compile
      (let* ((print-symbols-bare t)
             (data function-or-file)
             (comp-native-compiling t)
             (byte-native-qualities nil)
             (symbols-with-pos-enabled t)
             ;; Have byte compiler signal an error when compilation fails.
             (byte-compile-debug t)
             (comp-ctxt (make-comp-ctxt :output (when output
                                                  (expand-file-name output))
                                        :with-late-load with-late-load)))
        (comp-log "\n\n" 1)
        (unwind-protect
            (progn
              (condition-case-unless-debug err
                  (cl-loop
                   with report = nil
                   for t0 = (current-time)
                   for pass in comp-passes
                   unless (memq pass comp-disabled-passes)
                   do
                   (comp-log (format "\n(%S) Running pass %s:\n"
                                     function-or-file pass)
                             2)
                   (setf data (funcall pass data))
                   (push (cons pass (float-time (time-since t0))) report)
                   (cl-loop for f in (alist-get pass comp-post-pass-hooks)
                            do (funcall f data))
                   finally
                   (when comp-log-time-report
                     (comp-log (format "Done compiling %S" data) 0)
                     (cl-loop for (pass . time) in (reverse report)
                              do (comp-log (format "Pass %s took: %fs."
                                                   pass time)
                                           0))))
                (t
                 (let ((err-val (cdr err)))
                   ;; If we are doing an async native compilation print the
                   ;; error in the correct format so is parsable and abort.
                   (if (and comp-async-compilation
                            (not (eq (car err) 'native-compiler-error)))
                       (progn
                         (message "%S: Error %s"
                                  function-or-file
                                  (error-message-string err))
                         (kill-emacs -1))
                     ;; Otherwise re-signal it adding the compilation input.
                     ;; FIXME: We can't just insert arbitrary info in the
                     ;; error-data part of an error: the handler may expect
                     ;; specific data at specific positions!
	             (signal (car err) (if (consp err-val)
			                   (cons function-or-file err-val)
			                 ;; FIXME: `err-val' is supposed to be
			                 ;; a list, so it can only be nil here!
			                 (list function-or-file err-val)))))))
              (if (stringp function-or-file)
                  data
                ;; So we return the compiled function.
                (native-elisp-load data)))
          (when (and (not (stringp function-or-file))
                     (not output)
                     comp-ctxt
                     (comp-ctxt-output comp-ctxt)
                     (file-exists-p (comp-ctxt-output comp-ctxt)))
            ;; NOTE: Not sure if we want to remove this or being cautious.
            (cond ((eq 'windows-nt system-type)
                   ;; We may still be using the temporary .eln file.
                   (ignore-errors (delete-file (comp-ctxt-output comp-ctxt))))
                  (t (delete-file (comp-ctxt-output comp-ctxt))))))))))


;;; Compiler entry points.

(defun comp-compile-all-trampolines ()
  "Pre-compile AOT all trampolines."
  (let ((comp-running-batch-compilation t)
        ;; We want to target only the 'native-lisp' directory.
        (native-compile-target-directory
         (car (last native-comp-eln-load-path))))
    (mapatoms (lambda (f)
                (when (subr-primitive-p (symbol-function f))
                  (message "Compiling trampoline for: %s" f)
                  (comp-trampoline-compile f))))))

;;;###autoload
(defun comp-lookup-eln (filename)
  "Given a Lisp source FILENAME return the corresponding .eln file if found.
Search happens in `native-comp-eln-load-path'."
  (cl-loop
   with eln-filename = (comp-el-to-eln-rel-filename filename)
   for dir in (comp-eln-load-path-eff)
   for f = (expand-file-name eln-filename dir)
   when (file-exists-p f)
     do (cl-return f)))

;;;###autoload
(defun native-compile (function-or-file &optional output)
  "Compile FUNCTION-OR-FILE into native code.
This is the synchronous entry-point for the Emacs Lisp native compiler.
FUNCTION-OR-FILE is a function symbol, a form, an interpreted-function,
or the filename of an Emacs Lisp source file.  If OUTPUT is non-nil, use
it as the filename for the compiled object.  If FUNCTION-OR-FILE is a
filename, if the compilation was successful return the filename of the
compiled object.  If FUNCTION-OR-FILE is a function symbol or a form, if
the compilation was successful return the compiled function."
  (declare (ftype (function ((or string symbol) &optional string)
                            (or native-comp-function string))))
  (comp--native-compile function-or-file nil output))

;;;###autoload
(defun native-compile-directory (directory)
  "Native compile if necessary all the .el files present in DIRECTORY.
Each .el file is native-compiled if the corresponding .eln file is not
found in any directory mentioned in `native-comp-eln-load-path'.
The search within DIRECTORY is performed recursively."
  (mapc (lambda (file)
	  (unless (comp-lookup-eln file)
	    (native-compile file)))
	(directory-files-recursively directory ".+\\.el\\'")))

;;;###autoload
(defun batch-native-compile (&optional for-tarball)
  "Perform batch native compilation of remaining command-line arguments.

Native compilation equivalent of `batch-byte-compile'.
Use this from the command line, with `-batch'; it won't work
in an interactive Emacs session.
Optional argument FOR-TARBALL non-nil means the file being compiled
as part of building the source tarball, in which case the .eln file
will be placed under the native-lisp/ directory (actually, in the
last directory in `native-comp-eln-load-path')."
  (comp-ensure-native-compiler)
  (let ((comp-running-batch-compilation t)
        (native-compile-target-directory
         (if for-tarball
             (car (last native-comp-eln-load-path))
           native-compile-target-directory)))
    (cl-loop for file in command-line-args-left
             if (or (null byte+native-compile)
                    (cl-notany (lambda (re) (string-match re file))
                               native-comp-bootstrap-deny-list))
             collect (comp--native-compile file)
             else
             collect (byte-compile-file file))))

;; In use by elisp-mode.el
(defun comp--write-bytecode-file (eln-file)
  "After native compilation write the bytecode file for ELN-FILE.
Make sure that eln file is younger than byte-compiled one and
return the filename of this last.

This function can be used only in conjunction with
`byte+native-compile' `byte-to-native-output-buffer-file' (see
`batch-byte+native-compile')."
  (pcase byte-to-native-output-buffer-file
    (`(,temp-buffer . ,target-file)
     (unwind-protect
         (progn
           (byte-write-target-file temp-buffer target-file)
           ;; Touch the .eln in order to have it older than the
           ;; corresponding .elc.
           (when (stringp eln-file)
             (set-file-times eln-file)))
       (kill-buffer temp-buffer))
     target-file)))

;;;###autoload
(defun batch-byte+native-compile ()
  "Like `batch-native-compile', but used for bootstrap.
Generate .elc files in addition to the .eln files.
Force the produced .eln to be outputted in the eln system
directory (the last entry in `native-comp-eln-load-path') unless
`native-compile-target-directory' is non-nil.  If the environment
variable \"NATIVE_DISABLED\" is set, only byte compile."
  (comp-ensure-native-compiler)
  (if (equal (getenv "NATIVE_DISABLED") "1")
      (batch-byte-compile)
    (cl-assert (length= command-line-args-left 1))
    (let* ((byte+native-compile t)
           (native-compile-target-directory
            (car (last native-comp-eln-load-path)))
           (byte-to-native-output-buffer-file nil)
           (eln-file (car (batch-native-compile))))
      (comp--write-bytecode-file eln-file)
      (setq command-line-args-left (cdr command-line-args-left)))))

;;;###autoload
(defun native-compile-prune-cache ()
  "Remove *.eln files that aren't usable by the current Emacs build.

This command removes all the *.eln files in `native-comp-eln-load-path'
which are incompatible with the Emacs session in which you invoke this
command.  This includes the *.eln files compiled by all the Emacs
sessions where `comp-native-version-dir' had a value different from the
current session.

Note that this command does not prune the *.eln files in the last
directory in `native-comp-eln-load-path', which holds *.eln files
compiled during the Emacs build process."
  (interactive)
  (unless (featurep 'native-compile)
    (user-error "This Emacs isn't built with native-compile support"))
  ;; The last directory in 'native-comp-eln-load-path' is assumed to be a
  ;; system directory, so don't try to delete anything there (bug#59658).
  (dolist (dir (butlast native-comp-eln-load-path))
    ;; If a directory is non absolute it is assumed to be relative to
    ;; `invocation-directory'.
    (setq dir (expand-file-name dir invocation-directory))
    (when (file-exists-p dir)
      (dolist (subdir (seq-filter
                       (lambda (f) (not (string-match (rx "/." (? ".") eos) f)))
                       (directory-files dir t)))
        (when (and (file-directory-p subdir)
                   (file-writable-p subdir)
                   (not (equal (file-name-nondirectory
                                (directory-file-name subdir))
                               comp-native-version-dir)))
          (message "Deleting `%s'..." subdir)
          ;; We're being overly cautious here -- there shouldn't be
          ;; anything but .eln files in these directories.
          (dolist (eln (directory-files subdir t "\\.eln\\(\\.tmp\\)?\\'"))
            (when (file-writable-p eln)
              (delete-file eln)))
          (when (directory-empty-p subdir)
            (delete-directory subdir))))))
  (message "Cache cleared"))

(provide 'comp)

;; LocalWords: limplified limplification limplify Limple LIMPLE libgccjit elc eln

;;; comp.el ends here
