;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

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

(defconst comp-debug t)

(defvar comp-speed 2)

(defconst comp-passes '(comp-recuparate-lap
                        comp-limplify)
  "Passes to be executed in order.")

(defconst comp-known-ret-types '((Fcons . cons)))

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

(cl-defstruct comp-args
  (min nil :type number
       :documentation "Minimum number of arguments allowed")
  (max nil
       :documentation "Maximum number of arguments allowed
To be used when ncall-conv is nil.")
  (ncall-conv nil :type boolean
              :documentation "If t the signature is:
(ptrdiff_t nargs, Lisp_Object *args)"))

(cl-defstruct (comp-block (:copier nil))
  "A basic block."
  (sp nil
      :documentation "When non nil indicates its the sp value")
  (closed nil :type 'boolean
          :documentation "If the block was already closed"))

(cl-defstruct (comp-func (:copier nil))
  "Internal rapresentation for a function."
  (symbol-name nil
               :documentation "Function symbol's name")
  (c-func-name nil :type 'string
               :documentation "The function name in the native world")
  (func nil
        :documentation "Original form")
  (byte-func nil
             :documentation "Byte compiled version")
  (ir nil
      :documentation "Current intermediate rappresentation")
  (args nil :type 'comp-args)
  (frame-size nil :type 'number)
  (blocks (make-hash-table) :type 'hash-table
          :documentation "Key is the basic block symbol value is a comp-block
structure")
  (lap-block (make-hash-table :test #'equal) :type 'hash-table
             :documentation "Key value to convert from LAP label number to
LIMPLE basic block")
  (limple-cnt -1 :type 'number
              :documentation "Counter to create ssa limple vars"))

(cl-defstruct (comp-mvar (:copier nil) (:constructor make--comp-mvar))
  "A meta-variable being a slot in the meta-stack."
  (id nil :type number
     :documentation "SSA number")
  (slot nil :type fixnum
        :documentation "Slot position")
  (const-vld nil
             :documentation "Valid signal for the following slot")
  (constant nil
            :documentation "When const-vld non nil this is used for constant
 propagation")
  (type nil
        :documentation "When non nil is used for type propagation"))

(cl-defstruct (comp-limple-frame (:copier nil))
  "This structure is used during the limplify pass."
  (sp 0 :type 'fixnum
      :documentation "Current stack pointer")
  (frame nil :type 'vector
         :documentation "Meta-stack used to flat LAP")
  (block-sp (make-hash-table) :type 'hash-table
            :documentation "Key is the basic block value is the stack pointer"))

(defun comp-limple-frame-new-frame (size)
  "Return a clean frame of meta variables of size SIZE."
  (let ((v (make-vector size nil)))
    (cl-loop for i below size
             do (aset v i (make-comp-mvar :slot i)))
    v))

(defun comp-c-func-name (symbol-function)
  "Given SYMBOL-FUNCTION return a name suitable for the native code."
  ;; Unfortunatelly not all symbol names are valid as C function names...
  ;; Nassi's algorithm.
  (let* ((orig-name (symbol-name symbol-function))
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
                          (rx (not (any "a-z_"))) "" human-readable)))
    (concat "F" crypted "_" human-readable)))

(defun comp-decrypt-lambda-list (x)
  "Decript lambda list X."
  (let ((rest (not (= (logand x 128) 0)))
        (mandatory (logand x 127))
        (nonrest (ash x -8)))
    (if (and (null rest)
             (< nonrest 9)) ;; SUBR_MAX_ARGS
        (make-comp-args :min mandatory
                        :max nonrest)
      (make-comp-args :min mandatory
                      :ncall-conv t))))

(defun comp-recuparate-lap (func)
  "Byte compile and recuparate LAP rapresentation for FUNC."
  ;; FIXME block timers here, otherwise we could spill the wrong LAP.
  (setf (comp-func-byte-func func)
        (byte-compile (comp-func-symbol-name func)))
  (when comp-debug
    (cl-prettyprint byte-compile-lap-output))
  (let ((lambda-list (aref (comp-func-byte-func func) 0)))
    (if (fixnump lambda-list)
        (setf (comp-func-args func)
              (comp-decrypt-lambda-list (aref (comp-func-byte-func func) 0)))
      (error "Can't native compile a non lexical scoped function")))
  (setf (comp-func-ir func) byte-compile-lap-output)
  (setf (comp-func-frame-size func) (aref (comp-func-byte-func func) 3))
  func)

(declare-function comp-init-ctxt "comp.c")
(declare-function comp-release-ctxt "comp.c")
(declare-function comp-add-func-to-ctxt "comp.c")
(declare-function comp-compile-and-load-ctxt "comp.c")

;; (defun comp-opt-call (inst)
;;   "Optimize if possible a side-effect-free call in INST."
;;   (cl-destructuring-bind (_ f &rest args) inst
;;     (when (and (member f comp-mostly-pure-funcs)
;;                (cl-every #'identity (mapcar #'comp-mvar-const-vld args)))
;;       (apply f (mapcar #'comp-mvar-constant args)))))

;; Special vars used during limplifications
(defvar comp-frame)
(defvar comp-limple)
(defvar comp-func)

(cl-defun make-comp-mvar (&key slot const-vld constant type)
  (make--comp-mvar :id (cl-incf (comp-func-limple-cnt comp-func))
                   :slot slot :const-vld const-vld :constant constant
                   :type type))

(defmacro comp-sp ()
  "Current stack pointer."
  '(comp-limple-frame-sp comp-frame))

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
  `(aref (comp-limple-frame-frame comp-frame) ,n))

(defmacro comp-slot ()
  "Current slot into the meta-stack pointed by sp."
  '(comp-slot-n (comp-sp)))

(defmacro comp-slot-next ()
  "Slot into the meta-stack pointed by sp + 1."
  '(comp-slot-n (1+ (comp-sp))))

(defun comp-emit (x)
  "Emit X into current LIMPLE ir.."
  (push x comp-limple))

(defun comp-emit-set-call (call)
  "Emit CALL assigning the result the the current slot frame.
If the calle function is known to have a return type propagate it."
  (cl-assert call)
  (setf (comp-slot)
        (make-comp-mvar :slot (comp-sp)
                        :type (when (> comp-speed 0)
                                (alist-get (cadr call)
                                           comp-known-ret-types))))
  (comp-emit (list 'set (comp-slot) call)))

(defun comp-copy-slot-n (n)
  "Set current slot with slot number N as source."
  (let ((src-slot (comp-slot-n n)))
    (cl-assert src-slot)
    ;; FIXME should the id increase?
    (setf (comp-slot)
          (copy-sequence src-slot))
    (setf (comp-mvar-slot (comp-slot)) (comp-sp))
    (comp-emit (list 'set (comp-slot) src-slot))))

(defun comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defun comp-set-const (val)
  "Set constant VAL to current slot."
  (setf (comp-slot) (make-comp-mvar :slot (comp-sp)
                                    :const-vld t
                                    :constant val))
  (comp-emit (list 'setimm (comp-slot) val)))

(defun comp-emit-block (block-name)
  "Emit basic block BLOCK-NAME."
  (unless (gethash block-name (comp-func-blocks comp-func))
    (puthash block-name
             (make-comp-block :sp (comp-sp))
             (comp-func-blocks comp-func)))
  ;; Every new block we are forced to wipe out all the frame.
  ;; This will be optimized by proper flow analysis.
  (setf (comp-limple-frame-frame comp-frame)
        (comp-limple-frame-new-frame (comp-func-frame-size comp-func)))
  ;; If we are landing here form a recorded branch adjust sp accordingly.
  (setf (comp-sp)
        (comp-block-sp (gethash block-name (comp-func-blocks comp-func))))
  (comp-emit `(block ,block-name)))

(defmacro comp-with-fall-through-block (bb &rest body)
  "Create a basic block BB that is used to fall through after executing BODY."
  (declare (debug (form body))
           (indent defun))
  `(let ((,bb (comp-new-block-sym)))
     (puthash ,bb
              (make-comp-block :sp (comp-sp))
              (comp-func-blocks comp-func))
     (progn ,@body)
     (comp-emit-block ,bb)))

(defun comp-stack-adjust (n)
  "Move sp by N."
  (cl-incf (comp-sp) n))

(defun comp-limplify-listn (n)
  "Limplify list N."
  (comp-with-sp (1- n)
    (comp-emit-set-call `(call Fcons
                               ,(comp-slot)
                               ,(make-comp-mvar :const-vld t
                                                :constant nil))))
  (cl-loop for sp from (+ (comp-sp) n -2) downto (comp-sp)
           do (comp-with-sp sp
                (comp-emit-set-call `(call Fcons
                                           ,(comp-slot)
                                           ,(comp-slot-next))))))

(defun comp-new-block-sym ()
  "Return a symbol naming the next new basic block."
  (intern (format "bb_%s" (hash-table-count (comp-func-blocks comp-func)))))

(defun comp-lap-to-limple-bb (n)
  "Given the LAP label N return the limple basic block."
  (let ((hash (comp-func-lap-block comp-func)))
    (if-let ((bb (gethash n hash)))
        ;; If was already created return it.
        bb
      (let ((name (comp-new-block-sym)))
        (puthash n name hash)
        name))))

(defmacro comp-op-case (&rest cases)
  "Expand CASES into the corresponding pcase."
  (declare (debug (body))
           (indent defun))
  `(pcase op
     ,@(cl-loop for (op . body) in cases
		for sp-delta = (gethash op comp-op-stack-info)
                for op-name = (symbol-name op)
		if body
		  collect `(',op
                            ,(unless (eq op 'TAG)
                               `(comp-emit-annotation
                                 ,(concat "LAP op " op-name)))
                            ,(when sp-delta
			       `(comp-stack-adjust ,sp-delta))
			    (progn ,@body))
                else
		  collect `(',op (error ,(concat "Unsupported LAP op "
                                                op-name))))
     (_ (error "Unexpected LAP op %s" (symbol-name op)))))

(defun comp-limplify-lap-inst (inst)
  "Limplify LAP instruction INST accumulating in `comp-limple'."
  (let ((op (car inst))
        (arg (if (consp (cdr inst))
                 (cadr inst)
               (cdr inst))))
    (comp-op-case
      (TAG
       (comp-emit-block (comp-lap-to-limple-bb arg)))
      (byte-stack-ref
       (comp-copy-slot-n (- (comp-sp) (cdr inst) 1)))
      (byte-varref
       (comp-emit-set-call `(call Fsymbol_value ,(make-comp-mvar
                                                  :const-vld t
                                                  :constant arg))))
      (byte-varset
       (comp-emit `(call set_internal
                         ,(make-comp-mvar :const-vld t
                                          :constant arg)
                         ,(comp-slot))))
      (byte-varbind)
      (byte-call
       (comp-stack-adjust (- arg))
       (comp-emit-set-call `(callref Ffuncall ,(1+ arg) ,(comp-sp))))
      (byte-unbind)
      (byte-pophandler)
      (byte-pushconditioncase)
      (byte-pushcatch)
      (byte-nth)
      (byte-symbolp)
      (byte-consp)
      (byte-stringp)
      (byte-listp)
      (byte-eq)
      (byte-memq)
      (byte-not)
      (byte-car
       (comp-emit-set-call `(call Fcar ,(comp-slot))))
      (byte-cdr
       (comp-emit-set-call `(call Fcdr ,(comp-slot))))
      (byte-cons
       (comp-emit-set-call `(call Fcons ,(comp-slot) ,(comp-slot-next))))
      (byte-list1
       (comp-limplify-listn 1))
      (byte-list2
       (comp-limplify-listn 2))
      (byte-list3
       (comp-limplify-listn 3))
      (byte-list4
       (comp-limplify-listn 4))
      (byte-length
       (comp-emit-set-call `(call Flength ,(comp-slot))))
      (byte-aref
       (comp-emit-set-call `(call Faref
                                  ,(comp-slot)
                                  ,(comp-slot-next))))
      (byte-aset
       (comp-emit-set-call `(call Faset
                                  ,(comp-slot)
                                  ,(comp-slot-next)
                                  ,(comp-slot-n (+ 2 (comp-sp))))))
      (byte-symbol-value
       (comp-emit-set-call `(call Fsymbol_value ,(comp-slot))))
      (byte-symbol-function)
      (byte-set)
      (byte-fset)
      (byte-get)
      (byte-substring)
      (byte-concat2
       (comp-emit-set-call `(callref Fconcat 2 ,(comp-sp))))
      (byte-concat3
       (comp-emit-set-call `(callref Fconcat 3 ,(comp-sp))))
      (byte-concat4
       (comp-emit-set-call `(callref Fconcat 4 ,(comp-sp))))
      (byte-sub1)
      (byte-add1)
      (byte-eqlsign)
      (byte-gtr)
      (byte-lss)
      (byte-leq)
      (byte-geq)
      (byte-diff)
      (byte-negate)
      (byte-plus
       (comp-emit-set-call `(callref Fplus 2 ,(comp-sp))))
      (byte-max)
      (byte-min)
      (byte-mult)
      (byte-point)
      (byte-goto-char)
      (byte-insert)
      (byte-point-max)
      (byte-point-min)
      (byte-char-after)
      (byte-following-char)
      (byte-preceding-char)
      (byte-current-column)
      (byte-indent-to)
      (byte-scan-buffer-OBSOLETE)
      (byte-eolp)
      (byte-eobp)
      (byte-bolp)
      (byte-bobp)
      (byte-current-buffer)
      (byte-set-buffer)
      (byte-save-current-buffer)
      (byte-set-mark-OBSOLETE)
      (byte-interactive-p-OBSOLETE)
      (byte-forward-char)
      (byte-forward-word)
      (byte-skip-chars-forward)
      (byte-skip-chars-backward)
      (byte-forward-line)
      (byte-char-syntax)
      (byte-buffer-substring)
      (byte-delete-region)
      (byte-narrow-to-region)
      (byte-widen)
      (byte-end-of-line)
      (byte-constant2)
      (byte-goto
       (comp-with-fall-through-block bb
         (let ((target (comp-lap-to-limple-bb (cl-third inst))))
           (comp-emit (list 'jump target))
           (puthash target (comp-sp) (comp-limple-frame-block-sp comp-frame)))
         ))
      (byte-goto-if-nil
       (comp-with-fall-through-block bb
         (let ((target (comp-lap-to-limple-bb (cl-third inst))))
           (comp-emit (list 'cond-jump
                            (comp-slot)
                            bb
                            target))
           (puthash target (comp-sp) (comp-limple-frame-block-sp comp-frame)))))
      (byte-goto-if-not-nil
       (comp-with-fall-through-block bb
         (let ((target (comp-lap-to-limple-bb (cl-third inst))))
           (comp-emit (list 'cond-jump
                            (comp-slot)
                            target
                            bb))
           (puthash target (comp-sp) (comp-limple-frame-block-sp comp-frame)))))
      (byte-goto-if-nil-else-pop
       (comp-with-fall-through-block bb
         (let ((target (comp-lap-to-limple-bb (cl-third inst))))
           (comp-emit (list 'cond-jump
                            (comp-slot)
                            bb
                            target))
           (puthash target (comp-sp) (comp-limple-frame-block-sp comp-frame))
           (comp-stack-adjust -1))))
      (byte-goto-if-not-nil-else-pop
       (comp-with-fall-through-block bb
         (let ((target (comp-lap-to-limple-bb (cl-third inst))))
           (comp-emit (list 'cond-jump
                            (comp-slot)
                            target
                            bb))
           (puthash target (comp-sp) (comp-limple-frame-block-sp comp-frame))
           (comp-stack-adjust -1))))
      (byte-return
       (comp-emit (list 'return (comp-slot-next))))
      (byte-discard t)
      (byte-dup
       (comp-copy-slot-n (1- (comp-sp))))
      (byte-save-excursion)
      (byte-save-window-excursion-OBSOLETE)
      (byte-save-restriction)
      (byte-catch)
      (byte-unwind-protect)
      (byte-condition-case)
      (byte-temp-output-buffer-setup-OBSOLETE)
      (byte-temp-output-buffer-show-OBSOLETE)
      (byte-unbind-all)
      (byte-set-marker)
      (byte-match-beginning)
      (byte-match-end)
      (byte-upcase)
      (byte-downcase)
      (byte-string=)
      (byte-string<)
      (byte-equal)
      (byte-nthcdr)
      (byte-elt)
      (byte-member)
      (byte-assq)
      (byte-nreverse)
      (byte-setcar)
      (byte-setcdr)
      (byte-car-safe
       (comp-emit-set-call `(call Fcar_safe ,(comp-slot))))
      (byte-cdr-safe
       (comp-emit-set-call `(call Fcdr_safe ,(comp-slot))))
      (byte-nconc)
      (byte-quo)
      (byte-rem)
      (byte-numberp)
      (byte-integerp)
      (byte-listN)
      (byte-concatN
       (comp-stack-adjust (- (1- arg)))
       (comp-emit-set-call `(callref Fconcat ,arg ,(comp-sp))))
      (byte-insertN)
      (byte-stack-set)
      (byte-stack-set2)
      (byte-discardN)
      (byte-switch)
      (byte-constant
       (comp-set-const arg)))))

(defun comp-limplify (func)
  "Given FUNC and return compute its LIMPLE ir."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-frame (make-comp-limple-frame
                      :sp -1
                      :frame (comp-limple-frame-new-frame frame-size)))
         (comp-limple ()))
    ;; Prologue
    (comp-emit-block 'entry)
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-symbol-name func))))
    (cl-loop for i below (comp-args-mandatory (comp-func-args func))
             do (progn
                  (cl-incf (comp-sp))
                  (push `(setpar ,(comp-slot) ,i) comp-limple)))
    (push '(jump body) comp-limple)
    ;; Body
    (comp-emit-block 'body)
    (mapc #'comp-limplify-lap-inst (comp-func-ir func))
    (setf (comp-func-ir func) (reverse comp-limple))
    (when comp-debug
      (cl-prettyprint (comp-func-ir func)))
    func))

(defun native-compile (fun)
  "FUN is the function definition to be compiled into native code."
  (if-let ((f (symbol-function fun)))
      (progn
        (when (byte-code-function-p f)
          (error "Can't native compile an already bytecompiled function"))
        (let ((func (make-comp-func :symbol-name fun
                                    :func f
                                    :c-func-name (comp-c-func-name fun))))
          (mapc (lambda (pass)
                  (funcall pass func))
                comp-passes)
          ;; Once we have the final LIMPLE we jump into C.
          (comp-init-ctxt)
          (comp-add-func-to-ctxt func)
          (comp-compile-and-load-ctxt)
          (comp-release-ctxt)))
    (error "Trying to native compile something not a function")))

(provide 'comp)

;;; comp.el ends here
