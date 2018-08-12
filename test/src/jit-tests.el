;;; jit-tests.el --- unit tests for src/jijt.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

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

;; Unit tests for src/jit.c.

;;; Code:

(require 'ert)

(defun jit-test-apply (func &rest args)
  (unless (byte-code-function-p (symbol-function func))
    (byte-compile func))
  (apply func args))

;; Test Bconsp.
(defun jit-test-consp (x) (consp x))

(ert-deftest jit-consp ()
  (should-not (jit-test-apply 'jit-test-consp 23))
  (should-not (jit-test-apply 'jit-test-consp nil))
  (should (jit-test-apply 'jit-test-consp '(1 . 2))))

;; Test Blistp.
(defun jit-test-listp (x) (listp x))

(ert-deftest jit-listp ()
  (should-not (jit-test-apply 'jit-test-listp 23))
  (should (jit-test-apply 'jit-test-listp nil))
  (should (jit-test-apply 'jit-test-listp '(1 . 2))))

;; Test Bstringp.
(defun jit-test-stringp (x) (stringp x))

(ert-deftest jit-stringp ()
  (should-not (jit-test-apply 'jit-test-stringp 23))
  (should-not (jit-test-apply 'jit-test-stringp nil))
  (should (jit-test-apply 'jit-test-stringp "hi")))

;; Test Bsymbolp.
(defun jit-test-symbolp (x) (symbolp x))

(ert-deftest jit-symbolp ()
  (should-not (jit-test-apply 'jit-test-symbolp 23))
  (should-not (jit-test-apply 'jit-test-symbolp "hi"))
  (should (jit-test-apply 'jit-test-symbolp 'whatever)))

;; Test Bintegerp.
(defun jit-test-integerp (x) (integerp x))

(ert-deftest jit-integerp ()
  (should (jit-test-apply 'jit-test-integerp 23))
  (should-not (jit-test-apply 'jit-test-integerp 57.5))
  (should-not (jit-test-apply 'jit-test-integerp "hi"))
  (should-not (jit-test-apply 'jit-test-integerp 'whatever)))

;; Test Bnumberp.
(defun jit-test-numberp (x) (numberp x))

(ert-deftest jit-numberp ()
  (should (jit-test-apply 'jit-test-numberp 23))
  (should (jit-test-apply 'jit-test-numberp 57.5))
  (should-not (jit-test-apply 'jit-test-numberp "hi"))
  (should-not (jit-test-apply 'jit-test-numberp 'whatever)))

;; Test Badd1.
(defun jit-test-add1 (x) (1+ x))

(ert-deftest jit-add1 ()
  (should (eq (jit-test-apply 'jit-test-add1 23) 24))
  (should (eq (jit-test-apply 'jit-test-add1 -17) -16))
  (should (eql (jit-test-apply 'jit-test-add1 1.0) 2.0))
  (should-error (jit-test-apply 'jit-test-add1 nil)
                :type 'wrong-type-argument))

;; Test Bsub1.
(defun jit-test-sub1 (x) (1- x))

(ert-deftest jit-sub1 ()
  (should (eq (jit-test-apply 'jit-test-sub1 23) 22))
  (should (eq (jit-test-apply 'jit-test-sub1 -17) -18))
  (should (eql (jit-test-apply 'jit-test-sub1 1.0) 0.0))
  (should-error (jit-test-apply 'jit-test-sub1 nil)
                :type 'wrong-type-argument))

;; Test Bneg.
(defun jit-test-negate (x) (- x))

(ert-deftest jit-negate ()
  (should (eq (jit-test-apply 'jit-test-negate 23) -23))
  (should (eq (jit-test-apply 'jit-test-negate -17) 17))
  (should (eql (jit-test-apply 'jit-test-negate 1.0) -1.0))
  (should-error (jit-test-apply 'jit-test-negate nil)
                :type 'wrong-type-argument))

;; Test Bnot.
(defun jit-test-not (x) (not x))

(ert-deftest jit-not ()
  (should (eq (jit-test-apply 'jit-test-not 23) nil))
  (should (eq (jit-test-apply 'jit-test-not nil) t))
  (should (eq (jit-test-apply 'jit-test-not t) nil)))

;; Test Bbobp, Beobp, Bpoint, Bpoint_min, Bpoint_max.
(defun jit-test-bobp () (bobp))
(defun jit-test-eobp () (eobp))
(defun jit-test-point () (point))
(defun jit-test-point-min () (point-min))
(defun jit-test-point-max () (point-max))

(ert-deftest jit-bobp-and-eobp ()
  (with-temp-buffer
    (should (jit-test-apply 'jit-test-bobp))
    (should (jit-test-apply 'jit-test-eobp))
    (insert "hi")
    (goto-char (point-min))
    (should (eq (jit-test-apply 'jit-test-point-min) (point-min)))
    (should (eq (jit-test-apply 'jit-test-point) (point-min)))
    (should (jit-test-apply 'jit-test-bobp))
    (should-not (jit-test-apply 'jit-test-eobp))
    (goto-char (point-max))
    (should (eq (jit-test-apply 'jit-test-point-max) (point-max)))
    (should (eq (jit-test-apply 'jit-test-point) (point-max)))
    (should-not (jit-test-apply 'jit-test-bobp))
    (should (jit-test-apply 'jit-test-eobp))))

;; Test Bcar and Bcdr.
(defun jit-test-car (x) (car x))
(defun jit-test-cdr (x) (cdr x))

(ert-deftest jit-car-cdr ()
  (let ((pair '(1 . b)))
    (should (eq (jit-test-apply 'jit-test-car pair) 1))
    (should (eq (jit-test-apply 'jit-test-car nil) nil))
    (should-error (jit-test-apply 'jit-test-car 23)
                  :type 'wrong-type-argument)
    (should (eq (jit-test-apply 'jit-test-cdr pair) 'b))
    (should (eq (jit-test-apply 'jit-test-cdr nil) nil))
    (should-error (jit-test-apply 'jit-test-cdr 23)
                  :type 'wrong-type-argument)))

;; Test Bcar_safe and Bcdr_safe.
(defun jit-test-car-safe (x) (car-safe x))
(defun jit-test-cdr-safe (x) (cdr-safe x))

(ert-deftest jit-car-cdr-safe ()
  (let ((pair '(1 . b)))
    (should (eq (jit-test-apply 'jit-test-car-safe pair) 1))
    (should (eq (jit-test-apply 'jit-test-car-safe nil) nil))
    (should (eq (jit-test-apply 'jit-test-car-safe 23) nil))
    (should (eq (jit-test-apply 'jit-test-cdr-safe pair) 'b))
    (should (eq (jit-test-apply 'jit-test-cdr-safe nil) nil))
    (should (eq (jit-test-apply 'jit-test-cdr-safe 23) nil))))

;; Test Beq.
(defun jit-test-eq (x y) (eq x y))

(ert-deftest jit-eq ()
  (should (jit-test-apply 'jit-test-eq 'a 'a))
  (should (jit-test-apply 'jit-test-eq 5 5))
  (should-not (jit-test-apply 'jit-test-eq 'a 'b))
  (should-not (jit-test-apply 'jit-test-eq "x" "x")))

;; Test Bgotoifnil.
(defun jit-test-if (x y) (if x x y))

(ert-deftest jit-if ()
  (should (eq (jit-test-apply 'jit-test-if 'a 'b) 'a))
  (should (eq (jit-test-apply 'jit-test-if 0 23) 0))
  (should (eq (jit-test-apply 'jit-test-if nil 'b) 'b)))

;; Test Bgotoifnilelsepop.
(defun jit-test-and (x y) (and x y))

(ert-deftest jit-and ()
  (should (eq (jit-test-apply 'jit-test-and 'a 'b) 'b))
  (should (eq (jit-test-apply 'jit-test-and 0 23) 23))
  (should (eq (jit-test-apply 'jit-test-and nil 'b) nil)))

;; Test Bgotoifnonnilelsepop.
(defun jit-test-or (x y) (or x y))

(ert-deftest jit-or ()
  (should (eq (jit-test-apply 'jit-test-or 'a 'b) 'a))
  (should (eq (jit-test-apply 'jit-test-or 0 23) 0))
  (should (eq (jit-test-apply 'jit-test-or nil 'b) 'b)))

;; Test Bsave_excursion.
(defun jit-test-save-excursion ()
  (save-excursion
    (insert "XYZ")))

;; Test Bcurrent_buffer.
(defun jit-test-current-buffer () (current-buffer))

(ert-deftest jit-save-excursion ()
  (with-temp-buffer
    (jit-test-apply 'jit-test-save-excursion)
    (should (eq (point) (point-min)))
    (should (eq (jit-test-apply 'jit-test-current-buffer) (current-buffer)))))

;; Test Bgtr.
(defun jit-test-> (a b)
  (> a b))

(ert-deftest jit-> ()
  (should (eq (jit-test-apply 'jit-test-> 0 23) nil))
  (should (eq (jit-test-apply 'jit-test-> 23 0) t)))

;; Test Bpushcatch.
(defun jit-test-catch (&rest l)
  (catch 'done
    (dolist (v l)
      (when (> v 23)
        (throw 'done v)))))

(ert-deftest jit-catch ()
  (should (eq (jit-test-apply 'jit-test-catch 0 1 2 3 4) nil))
  (should (eq (jit-test-apply 'jit-test-catch 20 21 22 23 24 25 26 27 28) 24)))

;; Test Bmemq.
(defun jit-test-memq (val list)
  (memq val list))

(ert-deftest jit-memq ()
  (should (equal (jit-test-apply 'jit-test-memq 0 '(5 4 3 2 1 0)) '(0)))
  (should (eq (jit-test-apply 'jit-test-memq 72 '(5 4 3 2 1 0)) nil)))

;; Test BlistN.
(defun jit-test-listN (x)
  (list x x x x x x x x x x x x x x x x))

(ert-deftest jit-listN ()
  (should (equal (jit-test-apply 'jit-test-listN 57)
                 '(57 57 57 57 57 57 57 57 57 57 57 57 57 57 57 57))))

;; Test BconcatN.
(defun jit-test-concatN (x)
  (concat x x x x x x))

(ert-deftest jit-concatN ()
  (should (equal (jit-test-apply 'jit-test-concatN "x") "xxxxxx")))

;; Test optional and rest arguments.
(defun jit-test-opt-rest (a &optional b &rest c)
  (list a b c))

(ert-deftest jit-opt-rest ()
  (should (equal (jit-test-apply 'jit-test-opt-rest 1) '(1 nil nil)))
  (should (equal (jit-test-apply 'jit-test-opt-rest 1 2) '(1 2 nil)))
  (should (equal (jit-test-apply 'jit-test-opt-rest 1 2 3) '(1 2 (3))))
  (should (equal (jit-test-apply 'jit-test-opt-rest 1 2 56 57 58)
                 '(1 2 (56 57 58)))))

;; Test for too many arguments.
(defun jit-test-opt (a &optional b)
  (cons a b))

(ert-deftest jit-opt ()
  (should (equal (jit-test-apply 'jit-test-opt 23) '(23)))
  (should (equal (jit-test-apply 'jit-test-opt 23 24) '(23 . 24)))
  (should-error (jit-test-apply 'jit-test-opt)
                :type 'wrong-number-of-arguments)
  (should-error (jit-test-apply 'jit-test-opt nil 24 97)
                :type 'wrong-number-of-arguments))

;; Test for unwind-protect.
(defvar jit-test-up-val nil)
(defun jit-test-unwind-protect (fun)
  (setq jit-test-up-val nil)
  (unwind-protect
      (progn
        (setq jit-test-up-val 23)
        (funcall fun)
        (setq jit-test-up-val 24))
    (setq jit-test-up-val 999)))

(ert-deftest jit-unwind-protect ()
  (jit-test-unwind-protect 'ignore)
  (should (eq jit-test-up-val 999))
  (condition-case nil
      (jit-test-unwind-protect (lambda () (error "HI")))
    (error
     nil))
  (should (eq jit-test-up-val 999)))

;;; jit-tests.el ends here
