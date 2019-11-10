;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

;; Author: Andrea Corallo <akrl@sdf.com>

;; Copyright (C) 2019 Free Software Foundation, Inc.

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
(require 'gv)
(require 'cl-lib)
(require 'cl-extra)
(require 'subr-x)

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defcustom comp-speed 0
  "Compiler optimization level.  From 0 to 3.
- 0 no otimizations are performed, compile time is favored.
- 1 lite optimizations.
- 2 heavy optimizations.
- 3 max optimization level, to be used only when necessary.
    The compiler can inline within the compilation unit..."
  :type 'number
  :group 'comp)

(defcustom comp-debug 0
  "Compiler debug level.  From 0 to 3.
- 0 no debug facility.
- 1 emit debug symbols and dump pseudo C code.
- 2 dump gcc passes and libgccjit log file.
- 3 dump libgccjit reproducers."
  :type 'number
  :group 'comp)

(defcustom comp-verbose 0
  "Compiler verbosity.  From 0 to 3.
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

(defconst native-compile-log-buffer "*Native-compile-Log*"
  "Name of the native-compiler log buffer.")

(defvar comp-native-compiling nil
  "This gets bound to t while native compilation.
Can be used by code that wants to expand differently in this case.")

(defvar comp-pass nil
  "Every pass has the right to bind what it likes here.")

(defconst comp-passes '(comp-spill-lap
                        comp-limplify
                        comp-ssa
                        comp-propagate
                        comp-call-optim
                        comp-propagate
                        comp-dead-code
                        comp-final)
  "Passes to be executed in order.")

;; TODO hash here.
(defconst comp-known-ret-types '((cons . cons)
                                 (1+ . number)
                                 (1- . number)
                                 (+ . number)
                                 (- . number)
                                 (* . number)
                                 (/ . number)
                                 (% . number)
                                 ;; Type hint
                                 (comp-hint-fixnum . fixnum)
                                 (comp-hint-cons . cons))
  "Alist used for type propagation.")

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
  "Limple operators that clobbers the first mvar argument.")

(defconst comp-limple-calls '(call
                              callref
                              direct-call
                              direct-callref)
  "Limple operators use to call subrs.")

(eval-when-compile
  (defconst comp-op-stack-info
    (cl-loop with h = (make-hash-table)
	     for k across byte-code-vector
	     for v across byte-stack+-info
	     when k
	     do (puthash k v h)
	     finally return h)
    "Hash table lap-op -> stack adjustment."))

(cl-defstruct comp-ctxt
  "Lisp side of the compiler context."
  (output nil :type string
          :documentation "Target output filename for the compilation.")
  (top-level-forms () :type list
                   :documentation "List of spilled top level forms.")
  (funcs-h (make-hash-table) :type hash-table
           :documentation "lisp-func-name -> comp-func.
This is to build the prev field.")
  (data-relocs-l () :type list
               :documentation "Constant objects used by functions.")
  (data-relocs-idx (make-hash-table :test #'equal) :type hash-table
                   :documentation "Obj -> position into data-relocs.")
  (func-relocs-l () :type list
               :documentation "Native functions imported.")
  (func-relocs-idx (make-hash-table :test #'equal) :type hash-table
                   :documentation "Obj -> position into func-relocs."))

(cl-defstruct comp-args-base
  (min nil :type number
       :documentation "Minimum number of arguments allowed."))

(cl-defstruct (comp-args (:include comp-args-base))
  (max nil :type number
       :documentation "Maximum number of arguments allowed.
To be used when ncall-conv is nil."))

(cl-defstruct (comp-nargs (:include comp-args-base))
  "Describe args when the functin signature is of kind:
(ptrdiff_t nargs, Lisp_Object *args)."
  (nonrest nil :type number
           :documentation "Number of non rest arguments.")
  (rest nil :type boolean
        :documentation "t if rest argument is present."))

(cl-defstruct (comp-block (:copier nil)
                          (:constructor make--comp-block
                                        (addr sp name))) ; Positional
  "A basic block."
  (name nil :type symbol)
  ;; These two slots are used during limplification.
  (sp nil :type number
      :documentation "When non nil indicates the sp value while entering
into it.")
  (addr nil :type number
        :documentation "Start block LAP address.")
  (insns () :type list
         :documentation "List of instructions.")
  (closed nil :type boolean
          :documentation "t if closed.")
  ;; All the followings are for SSA and CGF analysis.
  (in-edges () :type list
            :documentation "List of incoming edges.")
  (out-edges () :type list
             :documentation "List of outcoming edges.")
  (dom nil :type comp-block
        :documentation "Immediate dominator.")
  (df (make-hash-table) :type hash-table
      :documentation "Dominance frontier set. Block-name -> block")
  (post-num nil :type number
            :documentation "Post order number.")
  (final-frame nil :type vector
             :documentation "This is a copy of the frame when leaving the block.
Is in use to help the SSA rename pass."))

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
  (symbol-name nil
               :documentation "Function symbol's name.")
  (c-func-name nil :type string
               :documentation "The function name in the native world.")
  (byte-func nil
             :documentation "Byte compiled version.")
  (doc nil :type string
       :documentation "Doc string.")
  (lap () :type list
       :documentation "LAP assembly representation.")
  (args nil :type comp-args-base)
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
  (ssa-cnt-gen (funcall #'comp-gen-counter) :type function
               :documentation "Counter to create ssa limple vars."))

(defun comp-func-reset-generators (func)
  "Reset unique id generators for FUNC."
  (setf (comp-func-edge-cnt-gen func) (comp-gen-counter))
  (setf (comp-func-ssa-cnt-gen func) (comp-gen-counter)))

(cl-defstruct (comp-mvar (:constructor make--comp-mvar))
  "A meta-variable being a slot in the meta-stack."
  (slot nil :type fixnum
        :documentation "Slot number.")
  (id nil :type (or null number)
     :documentation "SSA number.")
  (const-vld nil :type boolean
             :documentation "Valid signal for the following slot.")
  (constant nil
            :documentation "When const-vld non nil this is used for constant
 propagation.")
  (type nil
        :documentation "When non nil is used for type propagation.")
  (ref nil :type boolean
       :documentation "When t this is used by reference."))

;; Special vars used by some passes
(defvar comp-func)



(defun comp-set-op-p (op)
  "Assignment predicate for OP."
  (cl-find op comp-limple-sets))

(defun comp-assign-op-p (op)
  "Assignment predicate for OP."
  (cl-find op comp-limple-assignments))

(defun comp-limple-insn-call-p (insn)
  "Limple INSN call predicate."
  (when (member (car-safe insn) comp-limple-calls)
    t))

(defun comp-type-hint-p (func)
  "Type hint predicate for function name FUNC."
  (member func comp-type-hints))

(defun comp-add-const-to-relocs (obj)
  "Keep track of OBJ into the ctxt relocations.
The corresponding index is returned."
  (let ((data-relocs-idx (comp-ctxt-data-relocs-idx comp-ctxt)))
    (if-let ((idx (gethash obj data-relocs-idx)))
        idx
      (push obj (comp-ctxt-data-relocs-l comp-ctxt))
      (puthash obj (hash-table-count data-relocs-idx) data-relocs-idx))))

(defun comp-add-subr-to-relocs (subr-name)
  "Keep track of SUBR-NAME into the ctxt relocations.
The corresponding index is returned."
  (let ((func-relocs-idx (comp-ctxt-func-relocs-idx comp-ctxt)))
    (if-let ((idx (gethash subr-name func-relocs-idx)))
        idx
      (push subr-name (comp-ctxt-func-relocs-l comp-ctxt))
      (puthash subr-name (hash-table-count func-relocs-idx) func-relocs-idx))))

(defmacro comp-within-log-buff (&rest body)
  "Execute BODY while at the end the log-buffer.
BODY is evaluate only if `comp-verbose' is > 0."
  (declare (debug (form body))
           (indent defun))
  `(when (> comp-verbose 0)
     (with-current-buffer (get-buffer-create native-compile-log-buffer)
       (setq buffer-read-only t)
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         ,@body))))

(defun comp-log (data verbosity)
  "Log DATA given VERBOSITY."
  (when (>= comp-verbose verbosity)
    (if noninteractive
        (if (atom data)
            (message "%s" data)
	  (mapc (lambda (x)
                  (message "%s"(prin1-to-string x)))
                data))
      (comp-within-log-buff
        (if (and data (atom data))
            (insert data)
          (mapc (lambda (x)
                  (insert (prin1-to-string x) "\n"))
                data)
          (insert "\n"))))))

(defun comp-log-func (func verbosity)
  "Log function FUNC."
  (when (>= comp-verbose verbosity)
    (comp-log (format "\nFunction: %s\n" (comp-func-symbol-name func)) verbosity)
    (cl-loop for block-name being each hash-keys of (comp-func-blocks func)
             using (hash-value bb)
             do (comp-log (concat "<" (symbol-name block-name) ">\n") verbosity)
                (comp-log (comp-block-insns bb) verbosity))))

(defun comp-log-edges (func)
  "Log edges in FUNC."
  (let ((edges (comp-func-edges func)))
    (when (> comp-verbose 2)
      (comp-log (format "\nEdges in function: %s\n"
                        (comp-func-symbol-name func))
                0))
    (mapc (lambda (e)
            (when (> comp-verbose 2)
              (comp-log (format "n: %d src: %s dst: %s\n"
                                (comp-edge-number e)
                                (comp-block-name (comp-edge-src e))
                                (comp-block-name (comp-edge-dst e)))
                        0)))
          edges)))


;;; spill-lap pass specific code.

(defun comp-c-func-name (symbol prefix)
  "Given SYMBOL return a name suitable for the native code.
Put PREFIX in front of it."
  ;; Unfortunatelly not all symbol names are valid as C function names...
  ;; Nassi's algorithm here:
  (let* ((orig-name (symbol-name symbol))
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
    (concat prefix crypted "_" human-readable)))

(defun comp-decrypt-lambda-list (x)
  "Decript lambda list X."
  (unless (fixnump x)
    (error "Can't native compile a non lexical scoped function"))
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

(defun comp-byte-frame-size (byte-compiled-func)
  "Given BYTE-COMPILED-FUNC return the frame size to be allocated."
  ;; Is this really correct?
  ;; For the 1+ see bytecode.c:365 (finger crossed).
  (1+ (aref byte-compiled-func 3)))

(defun comp-spill-lap-function (_function-name)
  "Byte compile FUNCTION-NAME spilling data from the byte compiler."
  (error "To be reimplemented")
  ;; (let* ((f (symbol-function function-name))
  ;;        (func (make-comp-func :symbol-name function-name
  ;;                              :c-func-name (comp-c-func-name
  ;;                                            function-name
  ;;                                            "F"))))
  ;;     (when (byte-code-function-p f)
  ;;       (error "Can't native compile an already bytecompiled function"))
  ;;     (setf (comp-func-byte-func func)
  ;;           (byte-compile (comp-func-symbol-name func)))
  ;;     (let ((lap (alist-get function-name (reverse byte-to-native-bytecode))))
  ;;       (cl-assert lap)
  ;;       (comp-log lap)
  ;;       (let ((lambda-list (aref (comp-func-byte-func func) 0)))
  ;;         (setf (comp-func-args func)
  ;;               (comp-decrypt-lambda-list lambda-list)))
  ;;       (setf (comp-func-lap func) lap)
  ;;       (setf (comp-func-frame-size func)
  ;;             (comp-byte-frame-size (comp-func-byte-func func)))
  ;;       func))
  )

(defun comp-spill-lap-functions-file (filename)
  "Byte compile FILENAME spilling data from the byte compiler."
  (byte-compile-file filename)
  (setf (comp-ctxt-top-level-forms comp-ctxt) (reverse byte-to-native-top-level-forms))
  (cl-loop
   for f in (cl-loop for x in byte-to-native-top-level-forms ; All non anonymous.
                     when (and (byte-to-native-function-p x)
                               (byte-to-native-function-name x))
                       collect x)
   for name = (byte-to-native-function-name f)
   for data = (byte-to-native-function-data f)
   for doc = (when (>= (length data) 5) (aref data 4))
   for lap = (alist-get name byte-to-native-lap)
   for lambda-list = (aref data 0)
   for func = (make-comp-func :symbol-name name
                              :byte-func data
                              :doc doc
                              :c-func-name (comp-c-func-name
                                            name
                                            "F")
                              :args (comp-decrypt-lambda-list lambda-list)
                              :lap lap
                              :frame-size (comp-byte-frame-size data))
   do (comp-log (format "Function %s:\n" name) 1)
      (comp-log lap 1)
   collect func))

(defun comp-spill-lap (input)
  "Byte compile and spill the LAP rapresentation for INPUT.
If INPUT is a symbol this is the function-name to be compiled.
If INPUT is a string this is the file path to be compiled."
  (let ((byte-native-compiling t)
        (byte-to-native-lap ())
        (byte-to-native-top-level-forms ()))
    (cl-typecase input
      (symbol (list (comp-spill-lap-function input)))
      (string (comp-spill-lap-functions-file input)))))


;;; Limplification pass specific code.

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during function limplification."
  (frame nil :type vector
         :documentation "Meta-stack used to flat LAP.")
  (curr-block nil :type comp-block
              :documentation "Current block baing limplified.")
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
  (when (member (car inst) comp-lap-eob-ops)
    t))

(defsubst comp-lap-fall-through-p (inst)
  "Return t if INST fall through, nil otherwise."
  (when (not (member (car inst) '(byte-goto byte-return)))
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
      (error "Can't find label %d" label)))

(defsubst comp-mark-curr-bb-closed ()
  "Mark the current basic block as closed."
  (setf (comp-block-closed (comp-limplify-curr-block comp-pass)) t))

(defun comp-bb-maybe-add (lap-addr &optional sp)
  "If necessary create a pending basic block for LAP-ADDR with stack depth SP.
The basic block is returned regardless it was already declared or not."
  (let ((bb (or (cl-loop  ; See if the block was already liplified.
                 for bb being the hash-value in (comp-func-blocks comp-func)
                 when (equal (comp-block-addr bb) lap-addr)
                   return bb)
                (cl-find-if (lambda (bb) ; Look within the pendings blocks.
                              (= (comp-block-addr bb) lap-addr))
                            (comp-limplify-pending-blocks comp-pass)))))
    (if bb
        (progn
          (cl-assert (or (null sp) (= sp (comp-block-sp bb)))
                     (sp (comp-block-sp bb)) "sp %d %d differs")
          bb)
      (car (push (make--comp-block lap-addr sp (comp-new-block-sym))
                 (comp-limplify-pending-blocks comp-pass))))))

(defun comp-call (func &rest args)
  "Emit a call for function FUNC with ARGS."
  (comp-add-subr-to-relocs func)
  `(call ,func ,@args))

(defun comp-callref (func &rest args)
  "Emit a call usign narg abi for FUNC with ARGS."
  (comp-add-subr-to-relocs func)
  `(callref ,func ,@(cl-loop with (nargs off) = args
                             repeat nargs
                             for sp from off
                             collect (comp-slot-n sp))))

(cl-defun make-comp-mvar (&key slot (constant nil const-vld) type)
  (when const-vld
    (comp-add-const-to-relocs constant))
  (make--comp-mvar :slot slot :const-vld const-vld :constant constant
                   :type type))

(defun comp-new-frame (size &optional ssa)
  "Return a clean frame of meta variables of size SIZE."
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
  (comp-with-sp (if dst-n dst-n (comp-sp))
    (let ((src-slot (comp-slot-n src-n)))
      (cl-assert src-slot)
      (comp-emit `(set ,(comp-slot) ,src-slot)))))

(defsubst comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defun comp-emit-set-const (val)
  "Set constant VAL to current slot."
  (let ((rel-idx (comp-add-const-to-relocs val)))
    (cl-assert (numberp rel-idx))
    (comp-emit `(setimm ,(comp-slot) ,rel-idx ,val))))

(defun comp-make-curr-block (block-name entry-sp &optional addr)
  "Create a basic block with BLOCK-NAME and set it as current block.
ENTRY-SP is the sp value when entering.
The block is added to the current function.
The block is returned."
  (let ((bb (make--comp-block addr entry-sp block-name)))
    (setf (comp-limplify-curr-block comp-pass) bb)
    (setf (comp-limplify-pc comp-pass) addr)
    (setf (comp-limplify-sp comp-pass) (comp-block-sp bb))
    (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
    bb))

(defun comp-emit-uncond-jump (lap-label)
  "Emit an unconditional branch to LAP-LABEL."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (when stack-depth
      (cl-assert (= (1- stack-depth) (comp-sp))))
    (let ((target (comp-bb-maybe-add (comp-label-to-addr label-num)
                                     (comp-sp))))
      (comp-emit `(jump ,(comp-block-name target)))
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
           (target (comp-block-name (comp-bb-maybe-add (comp-label-to-addr label-num)
                                                       target-sp))))
      (when label-sp
        (cl-assert (= (1- label-sp) (+ target-offset (comp-sp)))))
      (comp-emit (if negated
		     (list 'cond-jump a b target bb)
		   (list 'cond-jump a b bb target)))
      (comp-mark-curr-bb-closed)
      bb)))

(defun comp-emit-handler (lap-label handler-type)
  "Emit a non local exit handler to LAP-LABEL of type HANDLER-TYPE."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (cl-assert (= (- label-sp 2) (comp-sp)))
    (let* ((guarded-bb (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                          (comp-sp)))
           (handler-bb (comp-bb-maybe-add (comp-label-to-addr label-num)
                                          (1+ (comp-sp))))
           (pop-bb (make--comp-block nil (comp-sp) (comp-new-block-sym))))
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

(defun comp-new-block-sym ()
  "Return a unique symbol naming the next new basic block."
  (intern (format "bb_%s" (funcall (comp-func-block-cnt-gen comp-func)))))

(defun comp-fill-label-h ()
  "Fill label-to-addr hash table for the current function."
  (setf (comp-limplify-label-to-addr comp-pass) (make-hash-table :test 'eql))
  (cl-loop for insn in (comp-func-lap comp-func)
           for addr from 0
           do (pcase insn
                (`(TAG ,label . ,_)
                 (puthash label addr (comp-limplify-label-to-addr comp-pass))))))

(defun comp-emit-switch (var last-insn)
  "Emit a limple for a lap jump table given VAR and LAST-INSN."
  (pcase last-insn
    (`(setimm ,_ ,_ ,const)
     (cl-loop
      for test being each hash-keys of const
      using (hash-value target-label)
      with len = (hash-table-count const)
      for n from 1
      for last = (= n len)
      for m-test = (make-comp-mvar :constant test)
      for target-name = (comp-block-name (comp-bb-maybe-add (comp-label-to-addr target-label)
                                                            (comp-sp)))
      for ff-bb = (if last
                      (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                         (comp-sp))
                    (make--comp-block nil
                                      (comp-sp)
                                      (comp-new-block-sym)))
      for ff-bb-name = (comp-block-name ff-bb)
      do
      (comp-emit (list 'cond-jump var m-test ff-bb-name target-name))
      (unless last
        ;; All fall through are artificially created here except the last one.
        (puthash ff-bb-name ff-bb (comp-func-blocks comp-func))
        (setf (comp-limplify-curr-block comp-pass) ff-bb))))
    (_ (error "Missing previous setimm while creating a switch"))))

(defun comp-emit-set-call-subr (subr-name sp-delta)
    "Emit a call for SUBR-NAME.
SP-DELTA is the stack adjustment."
    (let ((subr (symbol-function subr-name))
          (subr-str (symbol-name subr-name))
          (nargs (1+ (- sp-delta))))
      (cl-assert (subrp subr) nil
                 "%s not a subr" subr-str)
      (let* ((arity (subr-arity subr))
             (minarg (car arity))
             (maxarg (cdr arity)))
        (cl-assert (not (eq maxarg 'unevalled)) nil
                   "%s contains unevalled arg" subr-name)
        (if (eq maxarg 'many)
            ;; callref case.
            (comp-emit-set-call (comp-callref subr-name nargs (comp-sp)))
          ;; Normal call.
          (cl-assert (and (>= maxarg nargs) (<= minarg nargs))
                     (nargs maxarg minarg)
                     "Incoherent stack adjustment %d, maxarg %d minarg %d")
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
When BODY is auto guess function name form the LAP bytecode
name. Othewise expect lname fnname."
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
  "Expand CASES into the corresponding pcase.
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
		collect `(',op (error ,(concat "Unsupported LAP op "
                                               op-name))))
     (_ (error "Unexpected LAP op %s" (symbol-name op)))))

(defun comp-limplify-lap-inst (insn)
  "Limplify LAP instruction INSN pushng it in the proper basic block."
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
       (comp-with-sp (1+ (comp-sp)) ;; FIXME!!
         (comp-copy-slot (comp-sp) (- (comp-sp) arg))))
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
       (comp-emit-set-const arg))
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
                  (comp-emit-set-const nil)
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

(cl-defgeneric comp-emit-for-top-level (form)
  "Emit the limple code for top level FORM.")

(cl-defmethod comp-emit-for-top-level ((form byte-to-native-function))
  (let* ((name (byte-to-native-function-name form))
         (f (gethash name (comp-ctxt-funcs-h comp-ctxt)))
         (args (comp-func-args f))
         (c-name (comp-func-c-func-name f))
         (doc (comp-func-doc f)))
    (cl-assert (and name f))
    (comp-emit (comp-call 'comp--register-subr
                          (make-comp-mvar :constant name)
                          (make-comp-mvar :constant (comp-args-base-min args))
                          (make-comp-mvar :constant (if (comp-args-p args)
                                                        (comp-args-max args)
                                                      'many))
                          (make-comp-mvar :constant c-name)
                          (make-comp-mvar :constant doc)))))

(cl-defmethod comp-emit-for-top-level ((form byte-to-native-top-level))
  (let ((form (byte-to-native-top-level-form form)))
    (comp-emit (comp-call 'eval
                          (make-comp-mvar :constant form)
                          (make-comp-mvar :constant t)))))

(defun comp-limplify-top-level ()
  "Create a limple function doing the business for top level forms.
This will be called at load-time."
  (let* ((func (make-comp-func :symbol-name 'top-level-run
                  :c-func-name "top_level_run"
                  :args (make-comp-args :min 0 :max 0)
                  :frame-size 0))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :curr-block (make--comp-block -1 0 'top-level)
                     :frame (comp-new-frame 0))))
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation "Top level")
    (mapc #'comp-emit-for-top-level (comp-ctxt-top-level-forms comp-ctxt))
    (comp-emit `(return ,(make-comp-mvar :constant t)))
    (comp-limplify-finalize-function func)))

(defun comp-addr-to-bb-name (addr)
  "Search for a block starting at ADDR into pending or limplified blocks."
  ;; FIXME: Actually we could have another hash for this.
  (cl-flet ((pred (bb)
              (equal (comp-block-addr bb) addr)))
    (if-let ((pending (cl-find-if #'pred
                                  (comp-limplify-pending-blocks comp-pass))))
        (comp-block-name pending)
      (cl-loop for bb being the hash-value in (comp-func-blocks comp-func)
               when (pred bb)
                 return (comp-block-name bb)))))

(defun comp-limplify-block (bb)
  "Limplify basic-block BB and add it to the current function."
  (setf (comp-limplify-curr-block comp-pass) bb)
  (setf (comp-limplify-sp comp-pass) (comp-block-sp bb))
  (setf (comp-limplify-pc comp-pass) (comp-block-addr bb))
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
                     :frame (comp-new-frame frame-size)))
         (args (comp-func-args func)))
    (comp-fill-label-h)
    ;; Prologue
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-symbol-name func))))
    (if (comp-args-p args)
        (cl-loop for i below (comp-args-max args)
                 do (cl-incf (comp-sp))
                    (comp-emit `(set-par-to-local ,(comp-slot) ,i)))
      (comp-emit-narg-prologue (comp-args-base-min args)
                               (comp-nargs-nonrest args)
                               (comp-nargs-rest args)))
    (comp-emit '(jump bb_0))
    ;; Body
    (comp-bb-maybe-add 0 (comp-sp))
    (cl-loop for next-bb = (pop (comp-limplify-pending-blocks comp-pass))
             while next-bb
             do (comp-limplify-block next-bb))
    ;; Sanity check against block duplication.
    (cl-loop with addr-h = (make-hash-table)
             for bb being the hash-value in (comp-func-blocks func)
             for addr = (comp-block-addr bb)
             when addr
               do (cl-assert (null (gethash addr addr-h)))
                  (puthash addr t addr-h))
    (comp-limplify-finalize-function func)))

(defun comp-add-func-to-ctxt (func)
  "Add FUNC to the current compiler contex."
  (puthash (comp-func-symbol-name func)
           func
           (comp-ctxt-funcs-h comp-ctxt)))

(defun comp-limplify (lap-funcs)
  "Compute the LIMPLE ir for LAP-FUNCS.
Top level forms for the current context are rendered too."
  (mapc #'comp-add-func-to-ctxt (mapcar #'comp-limplify-function lap-funcs))
  (comp-add-func-to-ctxt (comp-limplify-top-level)))


;;; SSA pass specific code.
;; After limplification no edges are present between basic blocks and an
;; implicit phi is present for every slot at the beginning of every basic block.
;; This pass is responsible for building all the edges and replace all m-vars
;; plus placing the needed phis.
;; Because the number of phis placed is (supposed) to be the minimum necessary
;; this form is called 'minimal SSA form'.
;; This pass should be run every time basic blocks or mvar are shuffled.

(cl-defun make-comp-ssa-mvar (&key slot (constant nil const-vld) type)
  (make--comp-mvar  :id (funcall (comp-func-ssa-cnt-gen comp-func))
                    :slot slot :const-vld const-vld :constant constant
                    :type type))

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
                   (error "Block %s does not end with a branch in func %s"
                          bb (comp-func-symbol-name comp-func))))
             finally (progn
                       (setf (comp-func-edges comp-func)
                             (nreverse (comp-func-edges comp-func)))
                       ;; Update edge refs into blocks.
                       (cl-loop for edge in (comp-func-edges comp-func)
                                do (push edge
                                         (comp-block-out-edges (comp-edge-src edge)))
                                   (push edge
                                         (comp-block-in-edges (comp-edge-dst edge))))
                       (comp-log-edges comp-func)))))

(defun comp-collect-rev-post-order (basic-block)
  "Walk BASIC-BLOCK childs and return their name in reversed post-oder."
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
                    (setf b1 (comp-block-dom b1))
                    (setf finger1 (comp-block-post-num b1)))
                  (while (< finger2 finger1)
                    (setf b2 (comp-block-dom b2))
                    (setf finger2 (comp-block-post-num b2))))
                b1))
            (first-processed (l)
              (if-let ((p (cl-find-if (lambda (p) (comp-block-dom p)) l)))
                  p
                (error "Cant't find first preprocessed"))))

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
                   do (setf (comp-block-dom b) new-idom)
                      (setf changed t))))))

(defun comp-compute-dominator-frontiers ()
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
                      when (and (comp-assign-op-p (car insn))
                                (= slot-n (comp-mvar-slot (cadr insn))))
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
         :documentation "Vector of mvars."))

(defun comp-ssa-rename-insn (insn frame)
  (dotimes (slot-n (comp-func-frame-size comp-func))
    (cl-flet ((targetp (x)
                ;; Ret t if x is an mvar and target the correct slot number.
                (and (comp-mvar-p x)
                     (eql slot-n (comp-mvar-slot x))))
              (new-lvalue ()
                ;; If is an assignment make a new mvar and put it as l-value.
                (let ((mvar (make-comp-ssa-mvar :slot slot-n)))
                  (setf (aref frame slot-n) mvar)
                  (setf (cadr insn) mvar))))
      (pcase insn
        (`(,(pred comp-assign-op-p) ,(pred targetp) . ,_)
         (let ((mvar (aref frame slot-n)))
           (setcdr insn (cl-nsubst-if mvar #'targetp (cdr insn))))
         (new-lvalue))
        (`(phi  ,n)
         (when (equal n slot-n)
           (new-lvalue)))
        (_
         (let ((mvar (aref frame slot-n)))
           (setcdr insn (cl-nsubst-if mvar #'targetp (cdr insn)))))))))

(defun comp-ssa-rename ()
  "Entry point to rename SSA within the current function."
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
              ;; Concatenate into args all incoming mvars for this phi.
              (setcdr args
                      (cl-loop with slot-n = (comp-mvar-slot (car args))
                               for e in (comp-block-in-edges b)
                               for b = (comp-edge-src e)
                               for in-frame = (comp-block-final-frame b)
                               collect (aref in-frame slot-n))) ))

    (cl-loop for b being each hash-value of (comp-func-blocks comp-func)
             do (cl-loop for (op . args) in (comp-block-insns b)
                         when (eq op 'phi)
                         do (finalize-phi args b)))))

(defun comp-ssa (_)
  "Port FUNCS into mininal SSA form."
  (maphash (lambda (_ f)
             (let ((comp-func f))
               ;; TODO: if this is run more than once we should clean all CFG
               ;; data including phis here.
               (comp-func-reset-generators comp-func)
               (comp-compute-edges)
               (comp-compute-dominator-tree)
               (comp-compute-dominator-frontiers)
               (comp-log-block-info)
               (comp-place-phis)
               (comp-ssa-rename)
               (comp-finalize-phis)
               (comp-log-func comp-func 3)))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; propagate pass specific code.
;; A very basic propagation pass follows.

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
		(setq newcar (comp-copy-insn (car insn))))
	    (push newcar result))
	  (setq insn (cdr insn)))
	(nconc (nreverse result)
               (if (comp-mvar-p insn) (comp-copy-insn insn) insn)))
    (if (comp-mvar-p insn)
        (copy-comp-mvar insn)
      insn)))

(defun comp-basic-const-propagate ()
  "Propagate simple constants for setimm operands.
This can run just once."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(setimm ,lval ,_ ,v)
             (setf (comp-mvar-const-vld lval) t)
             (setf (comp-mvar-constant lval) v)
             (setf (comp-mvar-type lval) (comp-strict-type-of v)))))))

(defsubst comp-mvar-propagate (lval rval)
  "Propagate into LVAL properties of RVAL."
  (setf (comp-mvar-const-vld lval) (comp-mvar-const-vld rval))
  (setf (comp-mvar-constant lval) (comp-mvar-constant rval))
  (setf (comp-mvar-type lval) (comp-mvar-type rval)))

(defun comp-propagate-insn (insn)
  "Propagate within INSN."
  (pcase insn
    (`(set ,lval ,rval)
     (pcase rval
       (`(,(or 'call 'direct-call) ,f . ,_)
        (setf (comp-mvar-type lval)
              (alist-get f comp-known-ret-types)))
       (`(,(or 'callref 'direct-callref) ,f . ,args)
        (cl-loop for v in args
                 do (setf (comp-mvar-ref v) t))
        (setf (comp-mvar-type lval)
              (alist-get f comp-known-ret-types)))
       (_
        (comp-mvar-propagate lval rval))))
    (`(phi ,lval . ,rest)
     ;; Const prop here.
     (when (and (cl-every #'comp-mvar-const-vld rest)
                (cl-reduce #'equal (mapcar #'comp-mvar-constant rest)))
       (setf (comp-mvar-constant lval) (comp-mvar-constant (car rest))))
     ;; Type propagation.
     ;; FIXME: checking for type equality is not sufficient cause does not
     ;; account type hierarchy!!
     (when (cl-reduce #'eq (mapcar #'comp-mvar-type rest))
       (setf (comp-mvar-type lval) (comp-mvar-type (car rest))))
     ;; Reference propagation.
     (let ((operands (cons lval rest)))
       (when (cl-some #'comp-mvar-ref operands)
         (mapc (lambda (x) (setf (comp-mvar-ref x) t)) operands))))))

(defun comp-propagate* ()
  "Propagate for set* and phi operands.
Return t if something was changed."
  (cl-loop with modified = nil
           for b being each hash-value of (comp-func-blocks comp-func)
           do (cl-loop for insn in (comp-block-insns b)
                       for orig-insn = (unless modified ; Save consing after 1th change.
                                         (comp-copy-insn insn))
                       do (comp-propagate-insn insn)
                       when (and (null modified) (not (equal insn orig-insn)))
                         do (setf modified t))
           finally (cl-return modified)))

(defun comp-propagate (_)
  (maphash (lambda (_ f)
             (let ((comp-func f))
               (comp-basic-const-propagate)
               (cl-loop
                for i from 1
                while (comp-propagate*)
                finally (comp-log (format "Propagation run %d times\n" i) 2))
                        (comp-log-func comp-func 3)))
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

(defun comp-call-optim-form-call (callee args self)
  ""
  (cl-flet ((fill-args (args total)
              ;; Fill missing args to reach TOTAL
              (append args (cl-loop repeat (- total (length args))
                                    collect (make-comp-mvar :constant nil))))
            (clean-args-ref (args)
              ;; Clean-up the ref slot in all args
              (mapc (lambda (arg)
                      (setf (comp-mvar-ref arg) nil))
                    args)
              args))
    (when (symbolp callee) ; Do nothing if callee is a byte compiled func.
      (let* ((f (symbol-function callee))
             (subrp (subrp f))
             (callee-in-unit (gethash callee
                                      (comp-ctxt-funcs-h comp-ctxt))))
        (cond
         ((and subrp (not (subr-native-elisp-p f)))
          ;; Trampoline removal.
          (let* ((maxarg (cdr (subr-arity f)))
                 (call-type (if (if subrp
                                    (not (numberp maxarg))
                                  (comp-nargs-p callee-in-unit))
                                'callref
                              'call))
                 (args (if (eq call-type 'callref)
                           args
                         (fill-args args maxarg))))
            (comp-add-subr-to-relocs callee)
            `(,call-type ,callee ,@(clean-args-ref args))))
         ;; Intra compilation unit procedure call optimization.
         ;; Attention speed 3 triggers that for non self calls too!!
         ((or (eq callee self)
              (and (>= comp-speed 3)
                   callee-in-unit))
          (let* ((func-args (comp-func-args callee-in-unit))
                 (nargs (comp-nargs-p func-args))
                 (call-type (if nargs 'direct-callref 'direct-call))
                 (args (if (eq call-type 'direct-callref)
                           args
                         (fill-args args (comp-args-max func-args)))))
            `(,call-type ,callee ,@(clean-args-ref args))))
         ((comp-type-hint-p callee)
          `(call ,callee ,@args)))))))

(defun comp-call-optim-func ()
  "Perform trampoline call optimization for the current function."
  (cl-loop
   with self = (comp-func-symbol-name comp-func)
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn-cell on (comp-block-insns b)
       for insn = (car insn-cell)
       do (pcase insn
            (`(set ,lval (callref funcall ,f . ,rest))
             (when-let ((new-form (comp-call-optim-form-call
                                   (comp-mvar-constant f) rest self)))
               (setcar insn-cell `(set ,lval ,new-form))))
            (`(callref funcall ,f . ,rest)
             (when-let ((new-form (comp-call-optim-form-call
                                   (comp-mvar-constant f) rest self)))
               (setcar insn-cell new-form)))))))

(defun comp-call-optim (_)
  "Given FUNCS try to avoid funcall trampoline usage when possible."
  (when (>= comp-speed 2)
    (maphash (lambda (_ f)
               (let ((comp-func f))
                 (comp-call-optim-func)))
             (comp-ctxt-funcs-h comp-ctxt))))


;;; Dead code elimination pass specific code.
;; This simple pass try to eliminate insns became useful after propagation.
;; Even if gcc would take care of this is good to perform this here
;; in the hope of removing memory references (remember that most lisp
;; objects are loaded from the reloc array).
;;
;; This pass can be run as last optim.

(defun comp-collect-mvar-ids (insn)
  "Collect the mvar unique identifiers into INSN."
  (cl-loop for x in insn
           if (consp x)
           append (comp-collect-mvar-ids x)
           else
           when (comp-mvar-p x)
           collect (comp-mvar-id x)))

(defun comp-dead-assignments-func ()
  "Clean-up dead assignments into current function."
  (let ((l-vals ())
        (r-vals ()))
    ;; Collect used r and l values.
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
                        (comp-func-symbol-name comp-func)
                        l-vals
                        r-vals
                        nuke-list)
                3)
      (cl-loop
       for b being each hash-value of (comp-func-blocks comp-func)
       do (cl-loop
           for insn-cell on (comp-block-insns b)
           for insn = (car insn-cell)
           for (op arg0 rest) = insn
           when (and (comp-set-op-p op)
                     (member (comp-mvar-id arg0) nuke-list))
             do (setcar insn-cell
                        (if (comp-limple-insn-call-p rest)
                            rest
                          `(comment ,(format "optimized out: %s"
                                             insn)))))))))

(defun comp-remove-type-hints-func ()
  "Remove type hints from the current function.
These are substituted with normals 'set'."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn-cell on (comp-block-insns b)
       for insn = (car insn-cell)
       do (pcase insn
            (`(set ,l-val (call ,(pred comp-type-hint-p) ,r-val))
             (setcar insn-cell `(set ,l-val ,r-val)))))))

(defun comp-dead-code (_)
  "Dead code elimination."
  (when (>= comp-speed 2)
    (maphash (lambda (_ f)
               (let ((comp-func f))
                 (comp-dead-assignments-func)
                 (comp-remove-type-hints-func)
                 (comp-log-func comp-func 3)))
             (comp-ctxt-funcs-h comp-ctxt))))


;;; Final pass specific code.

(defun comp-compile-ctxt-to-file (name)
  "Compile as native code the current context naming it NAME.
Prepare every function for final compilation and drive the C back-end."
  (cl-assert (= (length (comp-ctxt-data-relocs-l comp-ctxt))
                (hash-table-count (comp-ctxt-data-relocs-idx comp-ctxt))))
  (comp--compile-ctxt-to-file name))

(defun comp-final (_)
  "Final pass driving DATA into the C back-end for code emission."
  (let (compile-result)
    (comp--init-ctxt)
    (unwind-protect
        (setq compile-result
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
  (cl-assert (fixnump x)))

(defun comp-hint-cons (x)
  (cl-assert (consp x)))


;; Some entry point support code.

(defvar comp-src-pool ()
  "List containing the files to be compiled.")

(defvar comp-src-pool-mutex (make-mutex)
  "Mutex for `comp-src-pool'.")

(defun comp-to-file-p (file)
  "Return t if FILE has to be compiled."
  (let ((compiled-f (concat file "n")))
    (or comp-always-compile
        (not (and (file-exists-p compiled-f)
                  (file-newer-than-file-p compiled-f file))))))

(defun comp-start-async-worker ()
  "Start an async compiler worker."
  (make-thread
   (lambda ()
     (let (f)
       (while (setf f (with-mutex comp-src-pool-mutex
                        (pop comp-src-pool)))
         (when (comp-to-file-p f)
           (let* ((cmd (concat "emacs --batch --eval="
                               "'(native-compile \"" f "\")'"))
                  (prc (start-process-shell-command (concat "async compilation: " f)
                                                    "async-compile-buffer"
                                                    cmd)))
             (while (accept-process-output prc)
               (thread-yield)))))))))

;;; Compiler entry points.

;;;###autoload
(defun native-compile (input)
  "Compile INPUT into native code.
This is the entrypoint for the Emacs Lisp native compiler.
If INPUT is a symbol, native compile its function definition.
If INPUT is a string, use it as the file path to be native compiled.
Return the compilation unit filename."
  (unless (or (symbolp input)
              (stringp input))
    (error "Trying to native compile something not a symbol function or file"))
  (let ((data input)
        (comp-native-compiling t)
        (comp-ctxt (make-comp-ctxt
                    :output (if (symbolp input)
                                (symbol-name input)
                              (file-name-sans-extension (expand-file-name input))))))
    (comp-log "\n\n" 1)
    (mapc (lambda (pass)
            (comp-log (format "Running pass %s:\n" pass) 2)
            (setq data (funcall pass data)))
          comp-passes)
    data))

;;;###autoload
(defun native-compile-async (input &optional jobs recursively)
  "Compile INPUT asyncronosly.
INPUT can be either a folder or a file.
JOBS specifies the number of jobs (commands) to run simultaneously (1 default).
Follow folders RECURSIVELY if non nil."
  (let ((jobs (or jobs 1))
        (files (if (file-directory-p input)
                   (if recursively
                       (directory-files-recursively input "\\.el$")
                     (directory-files input t "\\.el$"))
                 (if (file-exists-p input)
                     (list input)
                   (error "Input not a file nor directory")))))
    (with-mutex comp-src-pool-mutex
      (setf comp-src-pool (nconc files comp-src-pool)))
    (cl-loop repeat jobs
             do (comp-start-async-worker))))

(provide 'comp)

;;; comp.el ends here
