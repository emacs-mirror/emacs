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

;;; Code:

(require 'bytecomp)
(eval-when-compile (require 'cl-lib))

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defconst comp-debug t)

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

(cl-defstruct comp-args
  mandatory nonrest rest)

(cl-defstruct (comp-func (:copier nil))
  "Internal rapresentation for a function."
  (symbol-name nil
   :documentation "Function symbol's name")
  (func nil
   :documentation "Original form")
  (byte-func nil
   :documentation "Byte compiled version")
  (ir nil
      :documentation "Current intermediate rappresentation")
  (args nil :type 'comp-args)
  (limple-cnt -1 :type 'number
              :documentation "Counter to create ssa limple vars"))

(cl-defstruct (comp-mvar (:copier nil) (:constructor make--comp-mvar))
  "A meta-variable being a slot in the meta-stack."
  (n nil :type number
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
  "A LIMPLE func."
  (sp 0 :type 'fixnum
      :documentation "Current stack pointer")
  (frame nil :type 'vector
         :documentation "Meta-stack used to flat LAP"))

(defun comp-decrypt-lambda-list (x)
  "Decript lambda list X."
  (make-comp-args :rest (not (= (logand x 128) 0))
                  :mandatory (logand x 127)
                  :nonrest (ash x -8)))

(defun comp-recuparate-lap (func)
  "Byte compile and recuparate LAP rapresentation for FUNC."
  ;; FIXME block timers here, otherwise we could spill the wrong LAP.
  (setf (comp-func-byte-func func)
        (byte-compile (comp-func-symbol-name func)))
  (when comp-debug
    (cl-prettyprint byte-compile-lap-output))
  (setf (comp-func-args func)
        (comp-decrypt-lambda-list (aref (comp-func-byte-func func) 0)))
  (setf (comp-func-ir func) byte-compile-lap-output)
  func)

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
  (make--comp-mvar :n (cl-incf (comp-func-limple-cnt comp-func))
                   :slot slot :const-vld const-vld :constant constant
                   :type type))

(defmacro comp-sp ()
  "Current stack pointer."
  '(comp-limple-frame-sp comp-frame))

(defmacro comp-slot-n (n)
  "Slot N into the meta-stack."
  `(aref (comp-limple-frame-frame comp-frame) ,n))

(defmacro comp-slot ()
  "Current slot into the meta-stack pointed by sp."
  '(comp-slot-n (comp-sp)))

(defmacro comp-slot-next ()
  "Slot into the meta-stack pointed by sp + 1."
  '(comp-slot-n (1+ (comp-sp))))

(defun comp-push-call (src-slot)
  "Push call X into frame."
  (cl-incf (comp-sp))
  (setf (comp-slot)
        (make-comp-mvar :slot (comp-sp)
                        :type (alist-get (second src-slot)
                                         comp-known-ret-types)))
  (push (list '=call (comp-slot) src-slot) comp-limple))

(defun comp-push-slot-n (n)
  "Push slot number N into frame."
  (let ((src-slot (comp-slot-n n)))
    (cl-incf (comp-sp))
    (setf (comp-slot)
          (copy-sequence src-slot))
    (setf (comp-mvar-slot (comp-slot)) (comp-sp))
    (push (list '=slot (comp-slot) src-slot) comp-limple)))

(defun comp-push-const (val)
  "Push VAL into frame.
VAL is known at compile time."
  (cl-incf (comp-sp))
  (setf (comp-slot) (make-comp-mvar :slot (comp-sp)
                                    :const-vld t
                                    :constant val))
  (push (list '=const (comp-slot) val) comp-limple))

(defun comp-pop (n)
  "Pop N elements from the meta-stack."
  (cl-decf (comp-sp) n))

(defun comp-limplify-listn (n)
  "Limplify list N."
  (comp-pop 1)
  (comp-push-call `(call Fcons ,(comp-slot-next)
                         ,(make-comp-mvar :const-vld t
                                          :constant nil)))
  (dotimes (_ (1- n))
    (comp-pop 2)
    (comp-push-call `(call Fcons
                           ,(comp-slot-next)
                           ,(comp-slot-n (+ 2 (comp-sp)))))))

(defun comp-limplify-lap-inst (inst)
  "Limplify LAP instruction INST accumulating in `comp-limple'."
  (let ((op (car inst)))
    (pcase op
      ('byte-dup
       (comp-push-slot-n (comp-sp)))
      ('byte-varref
       (comp-push-call `(call Fsymbol_value ,(second inst))))
      ('byte-constant
       (comp-push-const (second inst)))
      ('byte-stack-ref
       (comp-push-slot-n (- (comp-sp) (cdr inst))))
      ('byte-plus
       (comp-pop 2)
       (comp-push-call `(callref Fplus 2 ,(comp-sp))))
      ('byte-car
       (comp-pop 1)
       (comp-push-call `(call Fcar ,(comp-sp))))
      ('byte-cdr
       (comp-pop 1)
       (comp-push-call `(call Fcdr ,(comp-sp))))
      ('byte-car-safe
       (comp-pop 1)
       (comp-push-call `(call Fcar-safe ,(comp-sp))))
      ('byte-cdr-safe
       (comp-pop 1)
       (comp-push-call `(call Fcdr-safe ,(comp-sp))))
      ('byte-list1
       (comp-limplify-listn 1))
      ('byte-list2
       (comp-limplify-listn 2))
      ('byte-list3
       (comp-limplify-listn 3))
      ('byte-list4
       (comp-limplify-listn 4))
      ('byte-return
       `(return ,(comp-slot)))
      (_ (error "Unexpected LAP op %s" (symbol-name op))))))

(defun comp-limplify (func)
  "Given FUNC and return LIMPLE."
  (let* ((frame-size (aref (comp-func-byte-func func) 3))
         (comp-func func)
         (comp-frame (make-comp-limple-frame
                      :sp -1
                      :frame (let ((v (make-vector frame-size nil)))
                               (cl-loop for i below frame-size
                                        do (aset v i (make-comp-mvar :slot i)))
                               v)))
         (comp-limple ()))
    ;; Prologue
    (push '(BLOCK prologue) comp-limple)
    (cl-loop for i below (comp-args-mandatory (comp-func-args func))
             do (progn
                  (cl-incf (comp-sp))
                  (push `(=par ,(comp-slot) ,i) comp-limple)))
    (push '(BLOCK body) comp-limple)
    (mapc #'comp-limplify-lap-inst (comp-func-ir func))
    (setf (comp-func-ir func) (reverse comp-limple))
    (when comp-debug
      (cl-prettyprint (comp-func-ir func)))
    func))

(defun native-compile (fun)
  "FUN is the function definition to be compiled to native code."
  (unless lexical-binding
    (error "Can't compile a non lexical binded function"))
  (if-let ((f (symbol-function fun)))
      (progn
        (when (byte-code-function-p f)
          (error "Can't native compile an already bytecompiled function"))
        (cl-loop with func = (make-comp-func :symbol-name fun
                                             :func f)
                 for pass in comp-passes
                 do (funcall pass func)
                 finally return func))
    (error "Trying to native compile not a function")))

(provide 'comp)

;;; comp.el ends here
