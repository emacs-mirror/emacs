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
;; Or, to put it another way to make a Carrera out of a turbocharged VW Bug.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(require 'cl-extra)
(require 'subr-x)

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defcustom comp-debug t
  "Log compilation process."
  :type 'boolean
  :group 'comp)

(defconst native-compile-log-buffer "*Native-compile-Log*"
  "Name of the native-compiler's log buffer.")

;; FIXME these has to be removed
(defvar comp-speed 2)
(defvar comp-verbose nil)

(defvar comp-pass nil
  "Every pass has the right to bind what it likes here.")

(defconst comp-passes '(comp-spill-lap
                        comp-limplify
                        comp-ssa
                        comp-final)
  "Passes to be executed in order.")

(defconst comp-known-ret-types '((cons . cons))
  "Alist used for type propagation.")

(defconst comp-limple-assignments '(set setimm set-par-to-local)
  "Limple operators used to assign to mvars.")

(defconst comp-mostly-pure-funcs
  '(% * + - / /= 1+ 1- < <= = > >= cons list % concat logand logcount logior
      lognot logxor regexp-opt regexp-quote string-to-char string-to-syntax
      symbol-name)
  "Functions on witch we do constant propagation."
  ;;  Is it acceptable to move into the compile time functions that are
  ;; allocating memory? (these are technically not side effect free)
)

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
  (output nil :type 'string
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
  (sp nil
      :documentation "When non nil indicates the sp value while entering
into it.")
  (closed nil :type boolean
          :documentation "If the block was already closed.")
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
            :documentation "Post order number."))

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
  (func nil
        :documentation "Original form.")
  (byte-func nil
             :documentation "Byte compiled version.")
  (lap () :type list
       :documentation "Lap assembly representation.")
  (args nil :type comp-args-base)
  (frame-size nil :type number)
  (blocks (make-hash-table) :type hash-table
          :documentation "Key is the basic block symbol value is a comp-block
structure.")
  (lap-block (make-hash-table :test #'equal) :type hash-table
             :documentation "Key value to convert from LAP label number to
LIMPLE basic block.")
  (edges () :type list
         :documentation "List of edges connecting basic blocks.")
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
  (id nil :type number
     :documentation "SSA number.")
  (slot nil :type fixnum
        :documentation "Slot position.")
  (const-vld nil
             :documentation "Valid signal for the following slot.")
  (constant nil
            :documentation "When const-vld non nil this is used for constant
 propagation.")
  (type nil
        :documentation "When non nil is used for type propagation."))

(defvar comp-ctxt) ;; FIXME (to be removed)

;; Special vars used by some passes
(defvar comp-block)
(defvar comp-func)


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
BODY is evaluate only if `comp-debug' is non nil."
  (declare (debug (form body))
           (indent defun))
  `(when comp-debug
     (with-current-buffer (get-buffer-create native-compile-log-buffer)
       (setq buffer-read-only t)
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         ,@body))))

(defun comp-log (data)
  "Log DATA."
  (if (and noninteractive
           comp-verbose)
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
              data)))))

(defun comp-log-func (func)
  "Log function FUNC."
  (comp-log (format "\n\n Function: %s" (comp-func-symbol-name func)))
  (cl-loop for block-name being each hash-keys of (comp-func-blocks func)
           using (hash-value bb)
           do (progn
                (comp-log (concat "\n<" (symbol-name block-name) ">\n"))
                (comp-log (comp-block-insns bb)))))

(defun comp-log-edges (func)
  "Log edges in FUNC."
  (let ((edges (comp-func-edges func)))
    (comp-log (format "\nEdges in function: %s\n" (comp-func-symbol-name func)))
    (mapc (lambda (e)
            (comp-log (format "n: %d src: %s dst: %s\n"
                              (comp-edge-number e)
                              (comp-block-name (comp-edge-src e))
                              (comp-block-name (comp-edge-dst e)))))
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
	                   do (aset str (1+ j) (aref byte 1))
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

(defun comp-spill-lap-function (function-name)
  "Byte compile FUNCTION-NAME spilling data from the byte compiler."
  (let* ((f (symbol-function function-name))
         (func (make-comp-func :symbol-name function-name
                               :func f
                               :c-func-name (comp-c-func-name
                                             function-name
                                             "F"))))
      (when (byte-code-function-p f)
        (error "Can't native compile an already bytecompiled function"))
      (setf (comp-func-byte-func func)
            (byte-compile (comp-func-symbol-name func)))
      (comp-log byte-to-native-last-lap)
      (let ((lambda-list (aref (comp-func-byte-func func) 0)))
        (setf (comp-func-args func)
              (comp-decrypt-lambda-list lambda-list)))
      (setf (comp-func-lap func) byte-to-native-last-lap)
      (setf (comp-func-frame-size func) (aref (comp-func-byte-func func) 3))
      func))

(defun comp-spill-lap-functions-file (filename)
  "Byte compile FILENAME spilling data from the byte compiler."
  (byte-compile-file filename)
  (setf (comp-ctxt-top-level-defvars comp-ctxt)
        (reverse (mapcar (lambda (x)
                           (cl-ecase (car x)
                             ('defvar (cdr x))
                             ('defconst (cdr x))))
                         byte-to-native-top-level-forms)))
  (cl-loop for (name lap bytecode) in byte-to-native-output
           for lambda-list = (aref bytecode 0)
           for func = (make-comp-func :symbol-name name
                                      :byte-func bytecode
                                      :c-func-name (comp-c-func-name
                                                    name
                                                    "F")
                                      :args (comp-decrypt-lambda-list lambda-list)
                                      :lap lap
                                      :frame-size (aref bytecode 3))
           do (comp-log lap)
           collect func))

(defun comp-spill-lap (input)
  "Byte compile and spill the LAP rapresentation for INPUT.
If INPUT is a symbol this is the function-name to be compiled.
If INPUT is a string this is the file path to be compiled."
  (let ((byte-native-compiling t)
        (byte-to-native-last-lap nil)
        (byte-to-native-output ())
        (byte-to-native-top-level-forms ()))
    (cl-typecase input
      (symbol (list (comp-spill-lap-function input)))
      (string (comp-spill-lap-functions-file input)))))


;;; Limplification pass specific code.

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during function limplification."
  (sp 0 :type fixnum
      :documentation "Current stack pointer while walking LAP.")
  (frame nil :type vector
         :documentation "Meta-stack used to flat LAP.")
  (block-name nil :type symbol
    :documentation "Current basic block name."))

(cl-defun comp-block-maybe-add (&rest args &key name &allow-other-keys)
  (let ((blocks (comp-func-blocks comp-func)))
    (unless (gethash name blocks)
      (puthash name (apply #'make--comp-block args) blocks))))

;; (defun comp-opt-call (inst)
;;   "Optimize if possible a side-effect-free call in INST."
;;   (cl-destructuring-bind (_ f &rest args) inst
;;     (when (and (member f comp-mostly-pure-funcs)
;;                (cl-every #'identity (mapcar #'comp-mvar-const-vld args)))
;;       (apply f (mapcar #'comp-mvar-constant args)))))

(defun comp-call (func &rest args)
  "Emit a call for function FUNC with ARGS."
  (comp-add-subr-to-relocs func)
  `(call ,func ,@args))

(defun comp-callref (func &rest args)
  "Emit a call usign narg abi for FUNC with ARGS."
  (comp-add-subr-to-relocs func)
  `(callref ,func ,@args))

(defun comp-new-frame (size)
  "Return a clean frame of meta variables of size SIZE."
  (cl-loop with v = (make-vector size nil)
           for i below size
           do (aset v i (make-comp-mvar :slot i))
           finally (return v)))

(cl-defun make-comp-mvar (&key slot (constant nil const-vld) type)
  (when const-vld
    (comp-add-const-to-relocs constant))
  (make--comp-mvar :id (funcall (comp-func-ssa-cnt-gen comp-func))
                   :slot slot :const-vld const-vld :constant constant
                   :type type))

(defmacro comp-sp ()
  "Current stack pointer."
  '(comp-limplify-sp comp-pass))

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

(defmacro comp-slot-n (n)
  "Slot N into the meta-stack."
  (declare (debug (form)))
  `(aref (comp-limplify-frame comp-pass) ,n))

(defmacro comp-slot ()
  "Current slot into the meta-stack pointed by sp."
  '(comp-slot-n (comp-sp)))

(defmacro comp-slot+1 ()
  "Slot into the meta-stack pointed by sp + 1."
  '(comp-slot-n (1+ (comp-sp))))

(defun comp-emit (insn)
  "Emit INSN into current basic block."
  (push insn (comp-block-insns comp-block)))

(defun comp-emit-set-call (call)
  "Emit CALL assigning the result the the current slot frame.
If the callee function is known to have a return type propagate it."
  (cl-assert call)
  (setf (comp-slot)
        (make-comp-mvar :slot (comp-sp)
                        :type (when (> comp-speed 0)
                                (alist-get (cadr call)
                                           comp-known-ret-types))))
  (comp-emit (list 'set (comp-slot) call)))

(defmacro comp-emit-set-call-subr (subr-name sp-delta)
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
            `(comp-emit-set-call (comp-callref ',subr-name ,nargs (comp-sp)))
          ;; Normal call.
          (cl-assert (and (>= maxarg nargs) (<= minarg nargs))
                     (nargs maxarg minarg)
                     "Incoherent stack adjustment %d, maxarg %d minarg %d")
          `(let* ((subr-name ',subr-name)
                  (slots (cl-loop for i from 0 below ,maxarg
                                  collect (comp-slot-n (+ i (comp-sp))))))
             (comp-emit-set-call (apply #'comp-call (cons subr-name slots))))))))

(defun comp-copy-slot (src-n &optional dst-n)
  "Set slot number DST-N to slot number SRC-N as source.
If DST-N is specified use it otherwise assume it to be the current slot."
  (comp-with-sp (if dst-n dst-n (comp-sp))
    (let ((src-slot (comp-slot-n src-n)))
      (cl-assert src-slot)
      ;; FIXME id should encrease here.
      (setf (comp-slot)
            (copy-sequence src-slot))
      (setf (comp-mvar-slot (comp-slot)) (comp-sp))
      (comp-emit `(set ,(comp-slot) ,src-slot)))))

(defun comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defun comp-emit-set-const (val)
  "Set constant VAL to current slot."
  (let ((rel-idx (comp-add-const-to-relocs val)))
    (cl-assert (numberp rel-idx))
    (setf (comp-slot) (make-comp-mvar :slot (comp-sp)
                                      :constant val))
    (comp-emit `(setimm ,(comp-slot) ,rel-idx ,val))))

(defun comp-mark-block-closed ()
  "Mark current basic block as closed."
  (setf (comp-block-closed (gethash (comp-limplify-block-name comp-pass)
                                    (comp-func-blocks comp-func)))
        t))

(defun comp-emit-jump (target)
  "Emit an unconditional branch to block TARGET."
  (comp-emit (list 'jump target))
  (comp-mark-block-closed))

(defun comp-emit-block (block-name)
  "Emit basic block BLOCK-NAME."
  (let ((blocks (comp-func-blocks comp-func)))
    ;; In case does not exist register it into comp-func-blocks.
    (comp-block-maybe-add :name block-name
                          :sp (comp-sp))
    ;; If we are abandoning an non closed basic block close it with a fall
    ;; through.
    (when (and (not (eq block-name 'entry))
               (not (comp-block-closed
                     (gethash (comp-limplify-block-name comp-pass)
                              blocks))))
      (comp-emit-jump block-name))
    ;; Set this a currently compiled block.
    (setf comp-block (gethash block-name blocks))
    ;; Every new block we are forced to wipe out all the frame.
    ;; This will be optimized by proper flow analysis.
    (setf (comp-limplify-frame comp-pass)
          (comp-new-frame (comp-func-frame-size comp-func)))
    ;; If we are landing here form a recorded branch adjust sp accordingly.
    (setf (comp-sp)
          (comp-block-sp (gethash block-name blocks)))
    (setf (comp-limplify-block-name comp-pass) block-name)))

(defun comp-emit-cond-jump (a b target-offset lap-label negated)
  "Emit a conditional jump to LAP-LABEL when A and B satisfy EQ.
TARGET-OFFSET is the positive offset on the SP when branching to the target
block.
If NEGATED non nil negate the tested condition."
  (let ((bb (comp-new-block-sym))) ;; Fall through block
    (comp-block-maybe-add :name bb :sp (comp-sp))
    (let ((target (comp-lap-to-limple-bb lap-label)))
      (comp-emit (if negated
		     (list 'cond-jump a b target bb)
		   (list 'cond-jump a b bb target)))
      (comp-block-maybe-add :name target :sp (+ target-offset (comp-sp)))
      (comp-mark-block-closed))
    (comp-emit-block bb)))

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
  "Return a symbol naming the next new basic block."
  (intern (format "bb-%s" (hash-table-count (comp-func-blocks comp-func)))))

(defun comp-lap-to-limple-bb (n)
  "Given the LAP label N return the limple basic block."
  (let ((hash (comp-func-lap-block comp-func)))
    (if-let ((bb (gethash n hash)))
        ;; If was already created return it.
        bb
      (let ((name (comp-new-block-sym)))
        (puthash n name hash)
        name))))

(defun comp-emit-handler (guarded-label handler-type)
  "Emit a non local exit handler for GUARDED-LABEL of type HANDLER-TYPE."
  (let ((guarded-bb (comp-new-block-sym)))
    (comp-block-maybe-add :name guarded-bb :sp (comp-sp))
    (let ((handler-bb (comp-lap-to-limple-bb guarded-label)))
      (comp-emit (list 'push-handler (comp-slot+1)
                       handler-type
                       handler-bb
                       guarded-bb))
      (comp-block-maybe-add :name handler-bb :sp (1+ (comp-sp)))
      (comp-mark-block-closed)
      (comp-emit-block guarded-bb))))

(defun comp-emit-switch (var m-hash)
  "Emit a limple for a lap jump table given VAR and M-HASH."
  (cl-assert (comp-mvar-const-vld m-hash))
  (cl-loop for test being each hash-keys of (comp-mvar-constant m-hash)
           using (hash-value target-label)
           for m-test = (make-comp-mvar :constant test)
           do (comp-emit-cond-jump var m-test 0 target-label nil)))

(defun comp-emit-funcall (narg)
  "Avoid Ffuncall trampoline if possibile.
NARG is the number of Ffuncall arguments."
  (comp-stack-adjust (- narg))
  (let* ((callee (comp-slot))
         (callee-sym-name (comp-mvar-constant callee))
         (optimize nil)
         (callref nil))
    (and (comp-mvar-const-vld callee)
         (or (and (>= comp-speed 2)
                  (eq callee-sym-name (comp-func-symbol-name comp-func))
                  (setq optimize t)
                  (setq callref (comp-nargs-p (comp-func-args comp-func))))
             ;; (and (>= comp-speed 3)
             ;;      (symbol-function callee-sym-name)
             ;;      (subrp (symbol-function callee-sym-name))
             ;;      (setq optimize t)
             ;;      (setq callref (eq 'many
             ;;                        (cdr (subr-arity
             ;;                              (symbol-function callee-sym-name)))))
             ;;      (setf callee-sym-name ))
             ))
    (if optimize
        (if callref
            (comp-emit-set-call (comp-callref callee-sym-name
                                              narg (1+ (comp-sp))))
          (comp-emit-set-call `(call ,callee-sym-name
                                     ,@(cl-loop for i from (1+ (comp-sp))
                                                repeat narg
                                                collect (comp-slot-n i)))))
      (comp-emit-set-call (comp-callref 'funcall (1+ narg) (comp-sp))))))

(defmacro comp-op-case (&rest cases)
  "Expand CASES into the corresponding pcase.
This is responsible for generating the proper stack adjustment when known and
the annotation emission."
  (declare (debug (body))
           (indent defun))
  (cl-labels ((op-to-fun (x)
                 ;; Given the LAP op strip "byte-" to have the subr name.
                 (intern (replace-regexp-in-string "byte-" "" x)))
              (body-eff (body op-name sp-delta)
                 ;; Given the original body BODY compute the effective one.
                 ;; When BODY is auto guess function name form the LAP bytecode
                 ;; name. Othewise expect lname fnname.
                 (pcase (car body)
                   ('auto
                    (list `(comp-emit-set-call-subr
                            ,(op-to-fun op-name)
                            ,sp-delta)))
                   ((pred symbolp)
                    (list `(comp-emit-set-call-subr
                            ,(car body)
                            ,sp-delta)))
                   (_ body))))
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
                              ,@(body-eff body op-name sp-delta))
                  else
		    collect `(',op (error ,(concat "Unsupported LAP op "
                                                 op-name))))
       (_ (error "Unexpected LAP op %s" (symbol-name op))))))

(defun comp-limplify-lap-inst (insn)
  "Limplify LAP instruction INSN pushng it in the proper basic block."
  (let ((op (car insn))
        (arg (if (consp (cdr insn))
                 (cadr insn)
               (cdr insn))))
    (comp-op-case
      (TAG
       (comp-emit-block (comp-lap-to-limple-bb arg)))
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
       (comp-emit-funcall arg))
      (byte-unbind
       (comp-emit (comp-call 'helper_unbind_n
                             (make-comp-mvar :constant arg))))
      (byte-pophandler
       (comp-emit '(pop-handler)))
      (byte-pushconditioncase
       (comp-emit-handler (cl-third insn) 'condition-case))
      (byte-pushcatch
       (comp-emit-handler (cl-third insn) 'catcher))
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
      (byte-constant2) ;; TODO
      (byte-goto
       (comp-emit-jump (comp-lap-to-limple-bb (cl-third insn))))
      (byte-goto-if-nil
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 0
                            (cl-third insn) nil))
      (byte-goto-if-not-nil
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 0
                            (cl-third insn) t))
      (byte-goto-if-nil-else-pop
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 1
                            (cl-third insn) nil))
      (byte-goto-if-not-nil-else-pop
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 1
                            (cl-third insn) t))
      (byte-return
       (comp-emit `(return ,(comp-slot+1)))
       (comp-mark-block-closed))
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
       (comp-with-sp (1+ (comp-sp))
         (comp-copy-slot (comp-sp) (- (comp-sp) arg))))
      (byte-stack-set2 (cl-assert nil)) ;; TODO
      (byte-discardN
       (comp-stack-adjust (- arg)))
      (byte-switch
       (comp-emit-switch (comp-slot+1) (comp-slot-n (+ 2 (comp-sp)))))
      (byte-constant
       (comp-emit-set-const arg))
      (byte-discardN-preserve-tos
       (comp-stack-adjust (- arg))
       (comp-copy-slot (+ arg (comp-sp)))))))

(defun comp-emit-narg-prologue (minarg nonrest)
  "Emit the prologue for a narg function."
  (cl-loop for i below minarg
           do (progn
                (comp-emit `(set-args-to-local ,i))
                (comp-emit '(inc-args))))
  (cl-loop for i from minarg below nonrest
           for bb = (intern (format "entry_%s" i))
           for fallback = (intern (format "entry_fallback_%s" i))
           do (progn
                (comp-emit `(cond-jump-narg-leq ,i ,bb ,fallback))
                (comp-mark-block-closed)
                (comp-emit-block bb)
                (comp-emit `(set-args-to-local ,i))
                (comp-emit '(inc-args)))
           finally (comp-emit-jump 'entry_rest_args))
  (cl-loop for i from minarg below nonrest
           do (comp-with-sp i
                (comp-emit-block (intern (format "entry_fallback_%s" i)))
                (comp-emit-set-const nil)))
  (comp-emit-block 'entry_rest_args)
  (comp-emit `(set-rest-args-to-local ,nonrest)))

(defun comp-limplify-finalize-function (func)
  "Reverse insns into all basic blocks of FUNC."
  (cl-loop for bb being the hash-value in (comp-func-blocks func)
           do (setf (comp-block-insns bb)
                    (nreverse (comp-block-insns bb))))
  (comp-log-func func)
  func)

(defun comp-limplify-top-level ()
  "Create a limple function doing the business for top level forms.
This will be called at runtime."
  (let* ((func (make-comp-func :symbol-name 'top-level-run
                  :c-func-name "top_level_run"
                  :args (make-comp-args :min 0 :max 0)
                  :frame-size 0))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :sp -1
                     :frame (comp-new-frame 0)))
         (comp-block ()))
    (comp-emit-block 'entry)
    (comp-emit-annotation "Top level")
    (cl-loop for args in (comp-ctxt-top-level-defvars comp-ctxt)
             do (comp-emit (comp-call 'defvar (make-comp-mvar :constant args))))
    (comp-emit `(return ,(make-comp-mvar :constant nil)))
    (comp-limplify-finalize-function func)))

(defun comp-limplify-function (func)
  "Limplify a single function FUNC."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :sp -1
                     :frame (comp-new-frame frame-size)))
         (args (comp-func-args func))
         (args-min (comp-args-base-min args))
         (comp-block ()))
    ;; Prologue
    (comp-emit-block 'entry)
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-symbol-name func))))
    (if (comp-args-p args)
        (cl-loop for i below (comp-args-max args)
                 do (cl-incf (comp-sp))
                 do (comp-emit `(set-par-to-local ,(comp-slot) ,i)))
      (let ((nonrest (comp-nargs-nonrest args)))
        (comp-emit-narg-prologue args-min nonrest)
        (cl-incf (comp-sp) (1+ nonrest))))
    ;; Body
    (comp-emit-block 'bb-1)
    (mapc #'comp-limplify-lap-inst (comp-func-lap func))
    (comp-limplify-finalize-function func)))

(defun comp-limplify (funcs)
  "Compute the LIMPLE ir for FUNCS.
Top level forms for the current context are rendered too."
  (cons (comp-limplify-top-level)
        (mapcar #'comp-limplify-function funcs)))


;;; SSA pass specific code.
;; After limplification no edges are present between basic blocks and an
;; implicit phi is present for every slot at the beginning of every basic block.
;; This pass is responsible for building all the edges and replace all m-vars
;; plus placing the needed phis.
;; Becase the number of phis placed is (supposed) to be the minimum necessary
;; this form is called 'minimal SSA form'.
;; This pass should be run every time basic blocks or mvar are shuffled.

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
             for (op first _ third forth) = last-insn
             do (cl-ecase op
                  (jump
                   (edge-add :src bb :dst (gethash first
                                                   blocks)))
                  (cond-jump
                   (edge-add :src bb :dst (gethash third
                                                   blocks))
                   (edge-add :src bb :dst (gethash forth
                                                   blocks)))
                  (return))
             finally (progn
                       (setf (comp-func-edges comp-func)
                             (nreverse (comp-func-edges comp-func)))
                       ;; Update edge refs into blocks.
                       (cl-loop for edge in (comp-func-edges comp-func)
                                do (push edge
                                         (comp-block-out-edges (comp-edge-src edge)))
                                do (push edge
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
               (bb1 (gethash 'bb-1 blocks)))
      (cl-loop with rev-bb-list = (comp-collect-rev-post-order entry)
               with changed = t
               while changed
               initially (progn
                           (comp-log "Computing dominator tree...\n")
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
                   do (progn
                        (setf (comp-block-dom b) new-idom)
                        (setf changed t)))))))

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
                                          collect b)))))
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
                      when (and (cl-find op comp-limple-assignments)
                                (= slot-n (comp-mvar-slot (cadr insn))))
                      do (return t))))

    (cl-loop for i from 0 below (comp-func-frame-size comp-func)
             ;; List of blocks with a definition of mvar i
             with defs-v = (cl-loop with blocks = (comp-func-blocks comp-func)
                                    for b being each hash-value of blocks
                                    when (slot-assigned-p i b)
                                    collect b)
             ;; Set of basic blocks where phi is added.
             with f = ()
             ;; Worklist, set of basic blocks that contain definitions of v.
             with w = defs-v
             do
             (while w
               (let ((x (pop w)))
                 (cl-loop for y being each hash-value of (comp-block-df x)
                          unless (cl-find y f)
                          do (progn
                               (add-phi i y)
                               (push y f)
                               ;; Adding a phi implies mentioning the
                               ;; correspondig slot so in case adjust w.
                               (unless (cl-find y defs-v)
                                 (push y w)))))))))

(defun comp-dominator-tree-walker (bb pre-lambda post-lambda)
  "Dominator tree walker function starting from basic block BB.
PRE-LAMBDA and POST-LAMBDA are called in pre or post-order if non nil."
  (when pre-lambda
    (funcall pre-lambda bb))
  (when-let ((out-edges (comp-block-out-edges bb)))
    (cl-loop for ed in out-edges
             for child = (comp-edge-dst ed)
             when (eq bb (comp-block-dom child))
             ;; Current block is the immediate dominator the recur.
             do (comp-dominator-tree-walker child pre-lambda post-lambda)))
  (when post-lambda
    (funcall post-lambda bb)))

(defun comp-rename-mvars ()
  "Rename all mvar accoring to the new SSA rapresentation."
  ;; Originally based on: Static Single Assignment Book
  ;; Algorithm 3.3: Renaming algorithm
  (comp-dominator-tree-walker (gethash 'entry (comp-func-blocks comp-func)) nil
                              (lambda (bb) (comp-log (format "\n%s" (comp-block-name bb))))))

(defun comp-ssa (funcs)
  "Port FUNCS into mininal SSA form."
  (cl-loop for comp-func in funcs
           do (progn
                ;; TODO: if this is run more than once we should clean all CFG
                ;; data including phis here.
                (comp-func-reset-generators comp-func)
                (comp-compute-edges)
                (comp-compute-dominator-tree)
                (comp-compute-dominator-frontiers)
                (comp-log-block-info)
                (comp-place-phis)
                (comp-log-func comp-func)
                (comp-rename-mvars))))


;;; Final pass specific code.

(defun comp-compile-ctxt-to-file (name)
  "Compile as native code the current context naming it NAME.
Prepare every functions for final compilation and drive the C side."
  (cl-assert (= (length (comp-ctxt-data-relocs-l comp-ctxt))
                (hash-table-count (comp-ctxt-data-relocs-idx comp-ctxt))))
  (setf (comp-ctxt-exp-funcs comp-ctxt)
        (cl-loop with h = (comp-ctxt-funcs-h comp-ctxt)
                 for f being each hash-value of h
                 for args = (comp-func-args f)
                 for doc = (when (> (length (comp-func-byte-func f))
                                    4)
                             (aref (comp-func-byte-func f) 4))
                 collect (vector (comp-func-symbol-name f)
                                 (comp-func-c-func-name f)
                                 (cons (comp-args-base-min args)
                                       (if (comp-args-p args)
                                           (comp-args-max args)
                                         'many))
                                 doc)))
  (comp--compile-ctxt-to-file name))

(defun comp-add-func-to-ctxt (func)
  "Add FUNC to the current compiler contex."
  (puthash (comp-func-symbol-name func)
           func
           (comp-ctxt-funcs-h comp-ctxt)))

(defun comp-final (data)
  "Final pass driving DATA into the C side for code emission."
  (let (compile-result)
    (comp--init-ctxt)
    (unwind-protect
        (progn
          (mapc #'comp-add-func-to-ctxt data)
          (setq compile-result
                (comp-compile-ctxt-to-file (comp-ctxt-output comp-ctxt))))
      (and (comp--release-ctxt)
           compile-result))))


;;; Entry points.

(defun native-compile (input)
  "Compile INPUT into native code.
This is the entrypoint for the Emacs Lisp native compiler.
If INPUT is a symbol, native-compile its function definition.
If INPUT is a string, use it as the file path to be native compiled."
  (unless (or (symbolp input)
              (stringp input))
    (error "Trying to native compile something not a symbol function or file"))
  (let ((data input)
        (comp-ctxt (make-comp-ctxt :output (if (symbolp input)
                                               (symbol-name input)
                                             (file-name-sans-extension input)))))
    (mapc (lambda (pass)
            (setq data (funcall pass data)))
          comp-passes)))

(provide 'comp)

;;; comp.el ends here
