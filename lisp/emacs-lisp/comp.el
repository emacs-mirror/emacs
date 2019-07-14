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

;; FIXME these has to be removed
(defvar comp-speed 2)
(defvar byte-compile-lap-output)

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
       :documentation "Minimum number of arguments allowed.")
  (max nil
       :documentation "Maximum number of arguments allowed
To be used when ncall-conv is nil..")
  (ncall-conv nil :type boolean
              :documentation "If t the signature is:
(ptrdiff_t nargs, Lisp_Object *args)."))

(cl-defstruct (comp-block (:copier nil))
  "A basic block."
  (sp nil
      :documentation "When non nil indicates its the sp value while entering
into it.")
  (closed nil :type 'boolean
          :documentation "If the block was already closed."))

(cl-defstruct (comp-func (:copier nil))
  "Internal rapresentation for a function."
  (symbol-name nil
               :documentation "Function symbol's name.")
  (c-func-name nil :type 'string
               :documentation "The function name in the native world.")
  (func nil
        :documentation "Original form.")
  (byte-func nil
             :documentation "Byte compiled version.")
  (ir nil
      :documentation "Current intermediate rappresentation.")
  (args nil :type 'comp-args)
  (frame-size nil :type 'number)
  (blocks (make-hash-table) :type 'hash-table
          :documentation "Key is the basic block symbol value is a comp-block
structure.")
  (lap-block (make-hash-table :test #'equal) :type 'hash-table
             :documentation "Key value to convert from LAP label number to
LIMPLE basic block.")
  (limple-cnt -1 :type 'number
              :documentation "Counter to create ssa limple vars."))

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

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during limplification."
  (sp 0 :type 'fixnum
      :documentation "Current stack pointer while walking LAP.")
  (frame nil :type 'vector
         :documentation "Meta-stack used to flat LAP.")
  (block-name nil :type 'symbol
    :documentation "Current basic block name."))

(defun comp-new-frame (size)
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
                          (rx (not (any "0-9a-z_"))) "" human-readable)))
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
(defvar comp-pass)
(defvar comp-limple)
(defvar comp-func)

(cl-defun make-comp-mvar (&key slot const-vld constant type)
  (make--comp-mvar :id (cl-incf (comp-func-limple-cnt comp-func))
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

(defmacro comp-emit-set-call-subr (subr-name &optional c-fun-name)
  "Emit a call for SUBR-NAME using C-FUN-NAME.
If C-FUN-NAME is nil will be guessed from SUBR-NAME."
  (let ((subr (symbol-function subr-name))
        (subr-str (symbol-name subr-name)))
    (cl-assert (subrp subr) nil
               "%s not a subr" subr-str)
      (let* ((arity (subr-arity subr))
             (minarg (car arity))
             (maxarg (cdr arity)))
        (unless c-fun-name
          (setq c-fun-name
                (intern (concat "F"
                                (replace-regexp-in-string
                                 "-" "_"
                                 subr-str)))))
        (cl-assert (not (or (eq maxarg 'many) (eq maxarg 'unevalled))) nil
                   "%s contains %s arg" subr-name maxarg )
        (cl-assert (= minarg maxarg) (minarg maxarg)
                   "args %d %d differs for %s" subr-name)
        `(let ((c-fun-name ',c-fun-name)
               (slots (cl-loop for i from 0 below ,maxarg
                               collect (comp-slot-n (+ i (comp-sp))))))
           (comp-emit-set-call `(call ,c-fun-name ,@slots))))))

(defun comp-copy-slot-n (n)
  "Set current slot with slot number N as source."
  (let ((src-slot (comp-slot-n n)))
    (cl-assert src-slot)
    ;; Should the id increased here?
    (setf (comp-slot)
          (copy-sequence src-slot))
    (setf (comp-mvar-slot (comp-slot)) (comp-sp))
    (comp-emit (list 'set (comp-slot) src-slot))))

(defun comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defun comp-emit-set-const (val)
  "Set constant VAL to current slot."
  (setf (comp-slot) (make-comp-mvar :slot (comp-sp)
                                    :const-vld t
                                    :constant val))
  (comp-emit (list 'setimm (comp-slot) val)))

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
    (unless (gethash block-name blocks)
      (puthash block-name
               (make-comp-block :sp (comp-sp))
               blocks))
    ;; If we are abandoning an non closed basic block close it with a fall
    ;; through.
    (when (and (not (eq block-name 'entry))
               (not (comp-block-closed (gethash (comp-limplify-block-name comp-pass)
                                                blocks))))
      (comp-emit-jump block-name))
    ;; Every new block we are forced to wipe out all the frame.
    ;; This will be optimized by proper flow analysis.
    (setf (comp-limplify-frame comp-pass)
          (comp-new-frame (comp-func-frame-size comp-func)))
    ;; If we are landing here form a recorded branch adjust sp accordingly.
    (setf (comp-sp)
          (comp-block-sp (gethash block-name blocks)))
    (comp-emit `(block ,block-name))
    (setf (comp-limplify-block-name comp-pass) block-name)))

(defun comp-emit-cond-jump (discard-n lap-label negated)
  "Emit a conditional jump to LAP-LABEL.
Discard DISCARD-N slots afterward.
If NEGATED non nil negate the test condition."
  (let ((bb (comp-new-block-sym))
        (blocks (comp-func-blocks comp-func)))
    (puthash bb
	     (make-comp-block :sp (- (comp-sp) discard-n))
	     blocks)
    (progn
      (let ((target (comp-lap-to-limple-bb lap-label)))
        (comp-emit (if negated
		       (list 'cond-jump (comp-slot-next) target bb)
		     (list 'cond-jump (comp-slot-next) bb target)))
        (puthash target
	         (make-comp-block :sp (comp-sp))
	         blocks)
        (comp-mark-block-closed)))
    (comp-emit-block bb)))

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
  "Expand CASES into the corresponding pcase.
This is responsible for generating the proper stack adjustment when known and
the annotation emission."
  (declare (debug (body))
           (indent defun))
  (cl-flet ((op-to-fun (x)
               ;; Given the LAP op strip "byte-" to have the subr name.
               (intern (replace-regexp-in-string "byte-" "" x))))
    `(pcase op
       ,@(cl-loop for (op . body) in cases
		  for sp-delta = (gethash op comp-op-stack-info)
                  for op-name = (symbol-name op)
                  for body-eff = (if (eq (car body) 'auto)
                                     (list `(comp-emit-set-call-subr
                                             ,(op-to-fun op-name)))
                                   body)
		  if body
		    collect `(',op
                              ,(unless (eq op 'TAG)
                                 `(comp-emit-annotation
                                   ,(concat "LAP op " op-name)))
                              ,(when sp-delta
			         `(comp-stack-adjust ,sp-delta))
			      (progn ,@body-eff))
                  else
		    collect `(',op (error ,(concat "Unsupported LAP op "
                                                 op-name))))
       (_ (error "Unexpected LAP op %s" (symbol-name op))))))

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
      (byte-varbind
       (comp-emit `(call specbind
                         ,(make-comp-mvar :const-vld t
                                          :constant arg)
                         ,(comp-slot-next))))
      (byte-call
       (comp-stack-adjust (- arg))
       (comp-emit-set-call `(callref Ffuncall ,(1+ arg) ,(comp-sp))))
      (byte-unbind
       (comp-emit `(call unbind_to
                         ,(make-comp-mvar :const-vld t
                                          :constant arg)
                         ,(make-comp-mvar :const-vld t
                                          :constant nil))))
      (byte-pophandler)
      (byte-pushconditioncase)
      (byte-pushcatch)
      (byte-nth auto)
      (byte-symbolp auto)
      (byte-consp auto)
      (byte-stringp auto)
      (byte-listp auto)
      (byte-eq auto)
      (byte-memq auto)
      (byte-not)
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
      (byte-substring)
      (byte-concat2
       (comp-emit-set-call `(callref Fconcat 2 ,(comp-sp))))
      (byte-concat3
       (comp-emit-set-call `(callref Fconcat 3 ,(comp-sp))))
      (byte-concat4
       (comp-emit-set-call `(callref Fconcat 4 ,(comp-sp))))
      (byte-sub1)
      (byte-add1)
      (byte-eqlsign
       (comp-emit-set-call `(call Fstring_equal
                                  ,(comp-slot)
                                  ,(comp-slot-next))))
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
      (byte-point auto)
      (byte-goto-char auto)
      (byte-insert)
      (byte-point-max auto)
      (byte-point-min auto)
      (byte-char-after)
      (byte-following-char auto)
      (byte-preceding-char auto)
      (byte-current-column auto)
      (byte-indent-to)
      (byte-scan-buffer-OBSOLETE)
      (byte-eolp auto)
      (byte-eobp auto)
      (byte-bolp auto)
      (byte-bobp auto)
      (byte-current-buffer auto)
      (byte-set-buffer auto)
      (byte-save-current-buffer)
      (byte-set-mark-OBSOLETE)
      (byte-interactive-p-OBSOLETE)
      (byte-forward-char)
      (byte-forward-word)
      (byte-skip-chars-forward)
      (byte-skip-chars-backward)
      (byte-forward-line)
      (byte-char-syntax auto)
      (byte-buffer-substring auto)
      (byte-delete-region auto)
      (byte-narrow-to-region)
      (byte-widen)
      (byte-end-of-line)
      (byte-constant2)
      (byte-goto
       (let ((bb (comp-new-block-sym))
             (blocks (comp-func-blocks comp-func))
             (target (comp-lap-to-limple-bb (cl-third inst))))
         (puthash bb (make-comp-block :sp (comp-sp)) blocks)
         (comp-emit-jump target)
         (puthash target
	          (make-comp-block :sp (comp-sp))
	          blocks)
         (comp-emit-block bb)))
      (byte-goto-if-nil
       (comp-emit-cond-jump 0 (cl-third inst) nil))
      (byte-goto-if-not-nil
       (comp-emit-cond-jump 0 (cl-third inst) t))
      (byte-goto-if-nil-else-pop
       (comp-emit-cond-jump 1 (cl-third inst) nil))
      (byte-goto-if-not-nil-else-pop
       (comp-emit-cond-jump 1 (cl-third inst) t))
      (byte-return
       (comp-emit (list 'return (comp-slot-next)))
       (comp-mark-block-closed))
      (byte-discard 'pass)
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
      (byte-nthcdr auto)
      (byte-elt auto)
      (byte-member auto)
      (byte-assq auto)
      (byte-nreverse auto)
      (byte-setcar auto)
      (byte-setcdr auto)
      (byte-car-safe
       (comp-emit-set-call `(call Fcar_safe ,(comp-slot))))
      (byte-cdr-safe
       (comp-emit-set-call `(call Fcdr_safe ,(comp-slot))))
      (byte-nconc)
      (byte-quo)
      (byte-rem)
      (byte-numberp auto)
      (byte-integerp auto)
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
       (comp-emit-set-const arg)))))

(defun comp-limplify (func)
  "Given FUNC compute its LIMPLE ir."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :sp -1
                     :frame (comp-new-frame frame-size)))
         (comp-limple ()))
    ;; Prologue
    (comp-emit-block 'entry)
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-symbol-name func))))
    (cl-loop for i below (comp-args-min (comp-func-args func))
             do (progn
                  (cl-incf (comp-sp))
                  (push `(setpar ,(comp-slot) ,i) comp-limple)))
    (comp-emit-jump 'body)
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
