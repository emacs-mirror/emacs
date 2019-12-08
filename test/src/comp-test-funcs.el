;;; comp-test-funcs.el --- compilation unit tested by comp-tests.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.org>

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

;;; Code:

(defvar comp-tests-var1 3)

(defun comp-tests-varref-f ()
  comp-tests-var1)

(defun comp-tests-list-f ()
  (list 1 2 3))
(defun comp-tests-list2-f (a b c)
  (list a b c))
(defun comp-tests-car-f (x)
  ;; Bcar
  (car x))
(defun comp-tests-cdr-f (x)
  ;; Bcdr
  (cdr x))
(defun comp-tests-car-safe-f (x)
  ;; Bcar_safe
  (car-safe x))
(defun comp-tests-cdr-safe-f (x)
  ;; Bcdr_safe
  (cdr-safe x))

(defun comp-tests-cons-car-f ()
  (car (cons 1 2)))
(defun comp-tests-cons-cdr-f (x)
  (cdr (cons 'foo x)))

(defun comp-tests-varset0-f ()
  (setq comp-tests-var1 55))
(defun comp-tests-varset1-f ()
  (setq comp-tests-var1 66)
  4)

(defun comp-tests-length-f ()
  (length '(1 2 3)))

(defun comp-tests-aref-aset-f ()
  (let ((vec [1 2 3]))
    (aset vec 2 100)
    (aref vec 2)))

(defvar comp-tests-var2 3)
(defun comp-tests-symbol-value-f ()
  (symbol-value 'comp-tests-var2))

(defun comp-tests-concat-f (x)
  (concat "a" "b" "c" "d"
          (concat "a" "b" "c" (concat "a" "b" (concat "foo" x)))))

(defun comp-tests-ffuncall-callee-f (x y z)
  (list x y z))

(defun comp-tests-ffuncall-callee-optional-f (a b &optional c d)
  (list a b c d))

(defun comp-tests-ffuncall-callee-rest-f (a b &rest c)
  (list a b c))

(defun comp-tests-ffuncall-callee-more8-f (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
  ;; More then 8 args.
  (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))

(defun comp-tests-ffuncall-callee-more8-rest-f (p1 p2 p3 p4 p5 p6 p7 p8 p9 &rest p10)
  ;; More then 8 args.
  (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))

(defun comp-tests-ffuncall-native-f ()
  "Call a primitive with no dedicate op."
  (make-vector 1 nil))

(defun comp-tests-ffuncall-native-rest-f ()
  "Call a primitive with no dedicate op with &rest."
  (vector 1 2 3))

(defun comp-tests-ffuncall-apply-many-f (x)
  (apply #'list x))

(defun comp-tests-ffuncall-lambda-f (x)
  (let ((fun (lambda (x)
               (1+ x))))
    (funcall fun x)))

(defun comp-tests-jump-table-1-f (x)
  (pcase x
    ('x 'a)
    ('y 'b)
    (_ 'c)))

(defun comp-tests-jump-table-2-f (x)
  (pcase x
    ("aaa" 'a)
    ("bbb" 'b)))

(defun comp-tests-conditionals-1-f (x)
  ;; Generate goto-if-nil
  (if x 1 2))
(defun comp-tests-conditionals-2-f (x)
  ;; Generate goto-if-nil-else-pop
  (when x
    1340))

(defun comp-tests-fixnum-1-minus-f (x)
  ;; Bsub1
  (1- x))
(defun comp-tests-fixnum-1-plus-f (x)
  ;; Badd1
  (1+ x))
(defun comp-tests-fixnum-minus-f (x)
  ;; Bnegate
  (- x))

(defun comp-tests-eqlsign-f (x y)
  ;; Beqlsign
  (= x y))
(defun comp-tests-gtr-f (x y)
  ;; Bgtr
  (> x y))
(defun comp-tests-lss-f (x y)
  ;; Blss
  (< x y))
(defun comp-tests-les-f (x y)
  ;; Bleq
  (<= x y))
(defun comp-tests-geq-f (x y)
  ;; Bgeq
  (>= x y))

(defun comp-tests-setcar-f (x y)
  (setcar x y)
  x)
(defun comp-tests-setcdr-f (x y)
  (setcdr x y)
  x)

(defun comp-bubble-sort-f (list)
  (let ((i (length list)))
    (while (> i 1)
      (let ((b list))
        (while (cdr b)
          (when (< (cadr b) (car b))
            (setcar b (prog1 (cadr b)
                        (setcdr b (cons (car b) (cddr b))))))
          (setq b (cdr b))))
      (setq i (1- i)))
    list))

(defun comp-tests-consp-f (x)
  ;; Bconsp
  (consp x))
(defun comp-tests-setcar2-f (x)
  ;; Bsetcar
  (setcar x 3))

(defun comp-tests-integerp-f (x)
  ;; Bintegerp
  (integerp x))
(defun comp-tests-numberp-f (x)
  ;; Bnumberp
  (numberp x))

(defun comp-tests-discardn-f (x)
  ;; BdiscardN
  (1+ (let ((a 1)
            (_b)
            (_c))
        a)))
(defun comp-tests-insertn-f (a b c d)
  ;; Binsert
  (insert a b c d))

(defun comp-tests-err-arith-f ()
  (/ 1 0))
(defun comp-tests-err-foo-f ()
  (error "foo"))

(defun comp-tests-condition-case-0-f ()
  ;; Bpushhandler Bpophandler
  (condition-case
      err
      (comp-tests-err-arith-f)
    (arith-error (concat "arith-error "
                         (error-message-string err)
                         " catched"))
    (error (concat "error "
                   (error-message-string err)
                   " catched"))))
(defun comp-tests-condition-case-1-f ()
  ;; Bpushhandler Bpophandler
  (condition-case
      err
      (comp-tests-err-foo-f)
    (arith-error (concat "arith-error "
                         (error-message-string err)
                         " catched"))
    (error (concat "error "
                   (error-message-string err)
                   " catched"))))
(defun comp-tests-catch-f (f)
  (catch 'foo
    (funcall f)))
(defun comp-tests-throw-f (x)
  (throw 'foo x))

(defun comp-tests-buff0-f ()
  (with-temp-buffer
    (insert "foo")
    (buffer-string)))

(defun comp-tests-lambda-return-f ()
  (lambda (x) (1+ x)))

(defun comp-tests-fib-f (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (comp-tests-fib-f (- n 1))
	      (comp-tests-fib-f (- n 2))))))

(defmacro comp-tests-macro-m (x)
  x)

(defun comp-tests-string-trim-f (url)
  (string-trim url))

(defun comp-tests-trampoline-removal-f ()
  (make-hash-table))

(defun comp-tests-signal-f ()
  (signal 'foo t))

(defun comp-tests-func-call-removal-f ()
  (let ((a 10)
	(b 3))
    (% a b)))

(defun comp-tests-doc-f ()
  "A nice docstring"
  t)

(defun comp-test-interactive-form0-f (dir)
  (interactive "D")
  dir)

(defun comp-test-interactive-form1-f (x y)
  (interactive '(1 2))
  (+ x y))

(defun comp-test-interactive-form2-f ()
  (interactive))


;;;;;;;;;;;;;;;;;;;;
;; Tromey's tests ;;
;;;;;;;;;;;;;;;;;;;;

;; Test Bconsp.
(defun comp-test-consp (x) (consp x))

;; Test Blistp.
(defun comp-test-listp (x) (listp x))

;; Test Bstringp.
(defun comp-test-stringp (x) (stringp x))

;; Test Bsymbolp.
(defun comp-test-symbolp (x) (symbolp x))

;; Test Bintegerp.
(defun comp-test-integerp (x) (integerp x))

;; Test Bnumberp.
(defun comp-test-numberp (x) (numberp x))

;; Test Badd1.
(defun comp-test-add1 (x) (1+ x))

;; Test Bsub1.
(defun comp-test-sub1 (x) (1- x))

;; Test Bneg.
(defun comp-test-negate (x) (- x))

;; Test Bnot.
(defun comp-test-not (x) (not x))

;; Test Bbobp, Beobp, Bpoint, Bpoint_min, Bpoint_max.
(defun comp-test-bobp () (bobp))
(defun comp-test-eobp () (eobp))
(defun comp-test-point () (point))
(defun comp-test-point-min () (point-min))
(defun comp-test-point-max () (point-max))

;; Test Bcar and Bcdr.
(defun comp-test-car (x) (car x))
(defun comp-test-cdr (x) (cdr x))

;; Test Bcar_safe and Bcdr_safe.
(defun comp-test-car-safe (x) (car-safe x))
(defun comp-test-cdr-safe (x) (cdr-safe x))

;; Test Beq.
(defun comp-test-eq (x y) (eq x y))

;; Test Bgotoifnil.
(defun comp-test-if (x y) (if x x y))

;; Test Bgotoifnilelsepop.
(defun comp-test-and (x y) (and x y))

;; Test Bgotoifnonnilelsepop.
(defun comp-test-or (x y) (or x y))

;; Test Bsave_excursion.
(defun comp-test-save-excursion ()
  (save-excursion
    (insert "XYZ")))

;; Test Bcurrent_buffer.
(defun comp-test-current-buffer () (current-buffer))

;; Test Bgtr.
(defun comp-test-> (a b)
  (> a b))

;; Test Bpushcatch.
(defun comp-test-catch (&rest l)
  (catch 'done
    (dolist (v l)
      (when (> v 23)
        (throw 'done v)))))

;; Test Bmemq.
(defun comp-test-memq (val list)
  (memq val list))

;; Test BlistN.
(defun comp-test-listN (x)
  (list x x x x x x x x x x x x x x x x))

;; Test BconcatN.
(defun comp-test-concatN (x)
  (concat x x x x x x))

;; Test optional and rest arguments.
(defun comp-test-opt-rest (a &optional b &rest c)
  (list a b c))

;; Test for too many arguments.
(defun comp-test-opt (a &optional b)
  (cons a b))

;; Test for unwind-protect.
(defvar comp-test-up-val nil)
(defun comp-test-unwind-protect (fun)
  (setq comp-test-up-val nil)
  (unwind-protect
      (progn
        (setq comp-test-up-val 23)
        (funcall fun)
        (setq comp-test-up-val 24))
    (setq comp-test-up-val 999)))

;; Non tested functions that proved just to be difficult to compile.

(defun comp-test-callee (_ __) t)
(defun comp-test-silly-frame1 (x)
  ;; Check robustness against dead code.
  (cl-case x
    (0 (comp-test-callee
        (pcase comp-tests-var1
          (1 1)
          (2 2))
        3))))

(defun comp-test-silly-frame2 (token)
  ;; Check robustness against dead code.
  (while c
    (cl-case c
      (?< 1)
      (?> 2))))

(provide 'comp-test-funcs)

;;; comp-test-funcs.el ends here
