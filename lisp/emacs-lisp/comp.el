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

(defcustom comp-verbose 0
  "Compiler verbosity. From 0 to 3.
- 0 no logging
- 1 final limple is logged
- 2 LAP and final limple are logged
- 3 all passes are dumping"
  :type 'number
  :group 'comp)

(defconst native-compile-log-buffer "*Native-compile-Log*"
  "Name of the native-compiler's log buffer.")

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

(defconst comp-limple-assignments `(push-handler
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
  (top-level-defvars nil :type list
                   :documentation "List of top level form to be exp.")
  (exp-funcs () :type list
             :documentation "Exported functions list.")
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
           :documentation "Number of non rest arguments."))

(cl-defstruct (comp-block (:copier nil) (:constructor make--comp-block))
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

(cl-defstruct (comp-mvar (:copier nil) (:constructor make--comp-mvar))
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

(defvar comp-ctxt) ;; FIXME (to be removed)

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

(defun comp-log (data)
  "Log DATA."
  (if (and noninteractive
           (> comp-verbose 0))
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
        (insert "\n")))))

(defun comp-log-func (func)
  "Log function FUNC."
  (comp-log (format "\nFunction: %s" (comp-func-symbol-name func)))
  (cl-loop for block-name being each hash-keys of (comp-func-blocks func)
           using (hash-value bb)
           do (comp-log (concat "<" (symbol-name block-name) ">\n"))
              (comp-log (comp-block-insns bb))))

(defun comp-log-edges (func)
  "Log edges in FUNC."
  (let ((edges (comp-func-edges func)))
    (when (> comp-verbose 2)
      (comp-log (format "\nEdges in function: %s\n"
                        (comp-func-symbol-name func))))
    (mapc (lambda (e)
            (when (> comp-verbose 2)
              (comp-log (format "n: %d src: %s dst: %s\n"
                                (comp-edge-number e)
                                (comp-block-name (comp-edge-src e))
                                (comp-block-name (comp-edge-dst e))))))
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
                       :nonrest nonrest))))

(defun comp-byte-frame-size (byte-compiled-func)
  "Given BYTE-COMPILED-FUNC return the frame size to be allocated."
  ;; Is this really correct?
  ;; For the 1+ see bytecode.c:365 (finger crossed).
  (1+ (aref byte-compiled-func 3)))

(defun comp-spill-lap-function (function-name)
  "Byte compile FUNCTION-NAME spilling data from the byte compiler."
  (let* ((f (symbol-function function-name))
         (func (make-comp-func :symbol-name function-name
                               :c-func-name (comp-c-func-name
                                             function-name
                                             "F"))))
      (when (byte-code-function-p f)
        (error "Can't native compile an already bytecompiled function"))
      (setf (comp-func-byte-func func)
            (byte-compile (comp-func-symbol-name func)))
      (let ((lap (alist-get function-name (reverse byte-to-native-bytecode))))
        (cl-assert lap)
        (comp-log lap)
        (let ((lambda-list (aref (comp-func-byte-func func) 0)))
          (setf (comp-func-args func)
                (comp-decrypt-lambda-list lambda-list)))
        (setf (comp-func-lap func) lap)
        (setf (comp-func-frame-size func)
              (comp-byte-frame-size (comp-func-byte-func func)))
        func)))

(defun comp-spill-lap-functions-file (filename)
  "Byte compile FILENAME spilling data from the byte compiler."
  (byte-compile-file filename)
  (setf (comp-ctxt-top-level-defvars comp-ctxt)
        (reverse (mapcar (lambda (x)
                           (cl-ecase (car x)
                             ('defvar (cdr x))
                             ('defconst (cdr x))))
                         byte-to-native-top-level-forms)))
  (cl-loop for (name . bytecode) in byte-to-native-bytecode
           for lap = (alist-get name byte-to-native-lap)
           for lambda-list = (aref bytecode 0)
           for func = (make-comp-func :symbol-name name
                                      :byte-func bytecode
                                      :c-func-name (comp-c-func-name
                                                    name
                                                    "F")
                                      :args (comp-decrypt-lambda-list lambda-list)
                                      :lap lap
                                      :frame-size (comp-byte-frame-size
                                                   bytecode))
           do (when (> comp-verbose 1)
                (comp-log (format "Function %s:\n" name))
                (comp-log lap))
           collect func))

(defun comp-spill-lap (input)
  "Byte compile and spill the LAP rapresentation for INPUT.
If INPUT is a symbol this is the function-name to be compiled.
If INPUT is a string this is the file path to be compiled."
  (let ((byte-native-compiling t)
        (byte-to-native-lap ())
        (byte-to-native-bytecode ())
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
  (sp 0 :type number
      :documentation "Current stack pointer while walking LAP.")
  (pc 0 :type number
      :documentation "Current program counter while walking LAP.")
  (label-to-addr nil :type hash-table
                 :documentation "LAP hash table -> address.")
  (pending-blocks () :type list
              :documentation "List of blocks waiting for limplification."))

(defconst comp-lap-eob-ops
  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
              byte-goto-if-not-nil-else-pop byte-return byte-pushcatch)
  "LAP end of basic blocks op codes.")

(defsubst comp-lap-eob-p (inst)
  "Return t if INST closes the current basic blocks, nil otherwise."
  (when (member (car inst) comp-lap-eob-ops)
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

(cl-defun comp-block-maybe-mark-pending (&rest args &key name sp &allow-other-keys)
  "Create a basic block and mark it as pending."
  (if-let ((bb (gethash name (comp-func-blocks comp-func))))
      ;; If was already limplified sanity check sp.
      (cl-assert (or (null sp) (= sp (comp-block-sp bb)))
                 (sp (comp-block-sp bb)) "sp %d %d differs")
    ;; Mark it pending in case is not already.
    (unless (cl-find-if (lambda (bb)
                          (eq (comp-block-name bb) name))
                        (comp-limplify-pending-blocks comp-pass))
      (push (apply #'make--comp-block args)
            (comp-limplify-pending-blocks comp-pass)))))

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
           for mvar = (if ssa (make-comp-ssa-mvar :slot i)
                          (make-comp-mvar :slot i))
           do (aset v i mvar)
           finally (return v)))

(defsubst comp-emit (insn)
  "Emit INSN into current basic block."
  (push insn (comp-block-insns (comp-limplify-curr-block comp-pass))))

(defun comp-emit-set-call (call)
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

(defun comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defun comp-emit-set-const (val)
  "Set constant VAL to current slot."
  (let ((rel-idx (comp-add-const-to-relocs val)))
    (cl-assert (numberp rel-idx))
    (comp-emit `(setimm ,(comp-slot) ,rel-idx ,val))))

(defun comp-make-curr-block (block-name entry-sp)
  "Create a basic block with BLOCK-NAME and set it as current block.
ENTRY-SP is the sp value when entering.
The block is added to the current function.
The block is returned."
  (let ((bb (make--comp-block :name block-name :sp entry-sp)))
    (setf (comp-limplify-curr-block comp-pass) bb)
    (setf (comp-limplify-sp comp-pass) (comp-block-sp bb))
    (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
    bb))

(defun comp-emit-uncond-jump (lap-label)
  "Emit an unconditional branch to LAP-LABEL."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (cl-assert (= stack-depth (comp-sp)))
    (let ((target (comp-lap-to-limple-bb label-num)))
      (comp-block-maybe-mark-pending :name target
                                     :sp stack-depth
                                     :addr (comp-label-to-addr label-num))
      (comp-emit `(jump ,target)))))

(defun comp-emit-cond-jump (a b target-offset lap-label negated)
  "Emit a conditional jump to LAP-LABEL when A and B satisfy EQ.
TARGET-OFFSET is the positive offset on the SP when branching to the target
block.
If NEGATED non null negate the tested condition."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (cl-assert (= stack-depth (+ target-offset (comp-sp))))
    (let ((bb (comp-new-block-sym)) ; Fall through block.
          (target (comp-lap-to-limple-bb label-num)))
      (comp-block-maybe-mark-pending :name bb
                                     :sp stack-depth
                                     :addr (1+ (comp-limplify-pc comp-pass)))
      (comp-block-maybe-mark-pending :name target
                                     :sp (+ target-offset stack-depth)
                                     :addr (comp-label-to-addr label-num))
      (comp-emit (if negated
		     (list 'cond-jump a b target bb)
		   (list 'cond-jump a b bb target))))))

(defun comp-emit-handler (lap-label handler-type)
  "Emit a non local exit handler to LAP-LABEL of type HANDLER-TYPE."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (cl-assert (= stack-depth (comp-sp)))
    (let ((guarded-bb (comp-new-block-sym))
          (handler-bb (comp-lap-to-limple-bb label-num)))
      (comp-block-maybe-mark-pending :name guarded-bb
                                     :sp stack-depth
                                     :addr (1+ (comp-limplify-pc comp-pass)))
      (comp-block-maybe-mark-pending :name handler-bb
                                     :sp (1+ stack-depth)
                                     :addr (comp-label-to-addr label-num))
      (comp-emit (list 'push-handler
                       (comp-slot+1)
                       (comp-slot+1)
                       handler-type
                       handler-bb
                       guarded-bb)))))

(defun comp-stack-adjust (n)
  "Move sp by N."
  (cl-incf (comp-sp) n))

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

(defun comp-lap-to-limple-bb (n)
  "Given the LAP label N return the limple basic block name."
  (let ((hash (comp-func-lap-block comp-func)))
    (if-let ((bb (gethash n hash)))
        ;; If was already created return it.
        bb
      (let ((name (comp-new-block-sym)))
        (puthash n name hash)
        name))))

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
     (cl-loop for test being each hash-keys of const
              using (hash-value target-label)
              for m-test = (make-comp-mvar :constant test)
              do (comp-emit-cond-jump var m-test 0 target-label nil)))
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
			     `(comp-stack-adjust ,sp-delta))
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
       ;; Paranoically sanity check stack depth.
       (cl-assert (= (cddr insn) (comp-limplify-sp comp-pass))))
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
       (comp-stack-adjust (- arg))
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
       (comp-emit-set-call (comp-call 'indent_to
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
       (comp-emit-set-call (comp-call 'narrow_to_region
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
       (comp-call 'helper-save-restriction))
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
       (comp-stack-adjust (- 1 arg))
       (comp-emit-set-call (comp-callref 'list arg (comp-sp))))
      (byte-concatN
       (comp-stack-adjust (- 1 arg))
       (comp-emit-set-call (comp-callref 'concat arg (comp-sp))))
      (byte-insertN
       (comp-stack-adjust (- 1 arg))
       (comp-emit-set-call (comp-callref 'insert arg (comp-sp))))
      (byte-stack-set
       (comp-with-sp (1+ (comp-sp)) ;; FIXME!!
         (comp-copy-slot (comp-sp) (- (comp-sp) arg))))
      (byte-stack-set2 (cl-assert nil)) ;; TODO
      (byte-discardN
       (comp-stack-adjust (- arg)))
      (byte-switch
       ;; Assume to follow the emission of a setimm.
       ;; This is checked into comp-emit-switch.
       (comp-emit-switch (comp-slot+1)
                         (cl-second (comp-block-insns
                                     (comp-limplify-curr-block comp-pass)))))
      (byte-constant
       (comp-emit-set-const arg))
      (byte-discardN-preserve-tos
       (comp-stack-adjust (- arg))
       (comp-copy-slot (+ arg (comp-sp)))))))

(defun comp-emit-narg-prologue (minarg nonrest)
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
  (cl-loop for i from minarg below nonrest
           do (comp-with-sp i
                (comp-make-curr-block (intern (format "entry_fallback_%s" i))
                                      (comp-sp))
                (comp-emit-set-const nil)))
  (comp-make-curr-block 'entry_rest_args (comp-sp))
  (comp-emit `(set-rest-args-to-local ,(comp-slot-n nonrest))))

(defun comp-limplify-finalize-function (func)
  "Reverse insns into all basic blocks of FUNC."
  (cl-loop for bb being the hash-value in (comp-func-blocks func)
           do (setf (comp-block-insns bb)
                    (nreverse (comp-block-insns bb))))
  (when (> comp-verbose 2)
    (comp-log-func func))
  func)

(defun comp-limplify-top-level ()
  "Create a limple function doing the business for top level forms.
This will be called at load-time."
  (let* ((func (make-comp-func :symbol-name 'top-level-run
                  :c-func-name "top_level_run"
                  :args (make-comp-args :min 0 :max 0)
                  :frame-size 0))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :curr-block (make--comp-block)
                     :frame (comp-new-frame 0))))
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation "Top level")
    (cl-loop for args in (comp-ctxt-top-level-defvars comp-ctxt)
             do (comp-emit (comp-call 'defvar (make-comp-mvar :constant args))))
    (comp-emit `(return ,(make-comp-mvar :constant nil)))
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
                 do (return (comp-block-name bb))))))

(defun comp-limplify-block (bb)
  "Limplify basic-block BB and add it to the current function."
  (setf (comp-limplify-curr-block comp-pass) bb)
  (setf (comp-limplify-sp comp-pass) (comp-block-sp bb))
  (setf (comp-limplify-pc comp-pass) (comp-block-addr bb))
  (cl-loop
   for inst-cell on (nthcdr (comp-limplify-pc comp-pass)
                            (comp-func-lap comp-func))
   for inst = (car inst-cell)
   for next-inst = (car-safe (cdr inst-cell))
   do (comp-limplify-lap-inst inst)
      (cl-incf (comp-limplify-pc comp-pass))
   when (eq (car next-inst) 'TAG)
     do ; That's a fall through.
     (let ((bb (or (comp-addr-to-bb-name (comp-limplify-pc comp-pass))
                   (comp-new-block-sym))))
       (comp-block-maybe-mark-pending :name bb
                                      :sp (comp-sp)
                                      :addr (comp-limplify-pc comp-pass))
       (comp-emit `(jump ,bb)))
     and return nil
   until (comp-lap-eob-p inst))
  (puthash (comp-block-name bb) bb (comp-func-blocks comp-func)))

(defun comp-limplify-function (func)
  "Limplify a single function FUNC."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :frame (comp-new-frame frame-size)))
         (args (comp-func-args func))
         (args-min (comp-args-base-min args)))
    (comp-fill-label-h)
    ;; Prologue
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-symbol-name func))))
    (if (comp-args-p args)
        (cl-loop for i below (comp-args-max args)
                 do (cl-incf (comp-sp))
                    (comp-emit `(set-par-to-local ,(comp-slot) ,i)))
      (let ((nonrest (comp-nargs-nonrest args)))
        (comp-emit-narg-prologue args-min nonrest)
        (cl-incf (comp-sp) (1+ nonrest))))
    (comp-emit '(jump bb_0))
    ;; Body
    (comp-block-maybe-mark-pending :name (comp-new-block-sym)
                                   :sp (comp-sp)
                                   :addr 0)
    (cl-loop for next-bb = (pop (comp-limplify-pending-blocks comp-pass))
             while next-bb
             do (comp-limplify-block next-bb))
    (comp-limplify-finalize-function func)))

(defun comp-add-func-to-ctxt (func)
  "Add FUNC to the current compiler contex."
  (puthash (comp-func-symbol-name func)
           func
           (comp-ctxt-funcs-h comp-ctxt)))

(defun comp-limplify (lap-funcs)
  "Compute the LIMPLE ir for LAP-FUNCS.
Top level forms for the current context are rendered too."
  (mapc #'comp-add-func-to-ctxt
        (cons (comp-limplify-top-level)
              (mapcar #'comp-limplify-function lap-funcs))))


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
             for (op first second third forth fifth) = last-insn
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
                   (edge-add :src bb :dst (gethash forth blocks))
                   (edge-add :src bb :dst (gethash fifth blocks)))
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
                           (when (> comp-verbose 2)
                             (comp-log "Computing dominator tree...\n"))
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
               (when (> comp-verbose 2)
                 (comp-log (format "block: %s idom: %s DF %s\n"
                                   name
                                   (when dom (comp-block-name dom))
                                   (cl-loop for b being each hash-keys of df
                                            collect b))))))
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
  (when (> comp-verbose 2)
    (comp-log "Renaming\n"))
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
               (when (> comp-verbose 2)
                 (comp-log-func comp-func))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; propagate pass specific code.
;; A very basic propagation pass follows.

(defsubst comp-strict-type-of (obj)
  "Given OBJ return its type understanding fixnums."
  ;; Should be certainly smarter but now we take advantages just from fixnums.
  (if (fixnump obj)
      'fixnum
    (type-of obj)))

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
     (setf (comp-mvar-ref lval) (cl-every #'comp-mvar-ref rest)))))

(defun comp-propagate* ()
  "Propagate for set and phi operands."
  (cl-loop for b being each hash-value of (comp-func-blocks comp-func)
           do (cl-loop for insn in (comp-block-insns b)
                       do (comp-propagate-insn insn))))

(defun comp-propagate (_)
  (maphash (lambda (_ f)
             (let ((comp-func f))
               (comp-basic-const-propagate)
               ;; FIXME: unbelievably dumb...
               (cl-loop repeat 10
                        do (comp-propagate*))
               (when (> comp-verbose 2)
                 (comp-log-func comp-func))))
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
      (when (> comp-verbose 2)
        (comp-log (format "Function %s\n" (comp-func-symbol-name comp-func)))
        (comp-log (format "l-vals %s\n" l-vals))
        (comp-log (format "r-vals %s\n" r-vals))
        (comp-log (format "Nuking ids: %s\n" nuke-list)))
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
                 (comp-log-func comp-func)))
             (comp-ctxt-funcs-h comp-ctxt))))


;;; Final pass specific code.

(defun comp-compile-ctxt-to-file (name)
  "Compile as native code the current context naming it NAME.
Prepare every function for final compilation and drive the C back-end."
  (cl-assert (= (length (comp-ctxt-data-relocs-l comp-ctxt))
                (hash-table-count (comp-ctxt-data-relocs-idx comp-ctxt))))
  (setf (comp-ctxt-exp-funcs comp-ctxt)
        (cl-loop with h = (comp-ctxt-funcs-h comp-ctxt)
                 for f being each hash-value of h
                 for args = (comp-func-args f)
                 for doc = (when (> (length (comp-func-byte-func f)) 4)
                             (aref (comp-func-byte-func f) 4))
                 collect (vector (comp-func-symbol-name f)
                                 (comp-func-c-func-name f)
                                 (cons (comp-args-base-min args)
                                       (if (comp-args-p args)
                                           (comp-args-max args)
                                         'many))
                                 doc)))
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


;;; Compiler entry points.

(defun native-compile (input)
  "Compile INPUT into native code.
This is the entrypoint for the Emacs Lisp native compiler.
If INPUT is a symbol, native-compile its function definition.
If INPUT is a string, use it as the file path to be native compiled."
  (unless (or (symbolp input)
              (stringp input))
    (error "Trying to native compile something not a symbol function or file"))
  (let ((data input)
        (comp-native-compiling t)
        (comp-ctxt (make-comp-ctxt
                    :output (if (symbolp input)
                                (symbol-name input)
                              (file-name-sans-extension (expand-file-name input))))))
    (mapc (lambda (pass)
            (comp-log (format "Running pass %s:\n" pass))
            (setq data (funcall pass data)))
          comp-passes)))

(provide 'comp)

;;; comp.el ends here
