;;; comp-tests.el --- unit tests for src/comp.c      -*- lexical-binding: t; -*-

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

;; Unit tests for src/comp.c.

;;; Code:

(require 'ert)
(require 'comp)
;; (require 'cl-lib)

(defun comp-test-apply (func &rest args)
  (unless (subrp (symbol-function func))
    (native-compile func))
  (apply func args))

(defun comp-mashup (&rest args)
  "Mash-up ARGS and return a symbol."
  (intern (apply #'concat
                 (mapcar (lambda (x)
                           (cl-etypecase x
                             (symbol (symbol-name x))
                             (string x)))
                         args))))

;; (setq garbage-collection-messages t)

(defvar comp-tests-var1 3)

;; (defmacro comp-ert-deftest (name &rest body)
;;   (declare (indent defun))
;;   `(progn
;;      ,@(cl-loop for speed from 0 to 3
;;                 for test-name = (comp-mashup name "-speed-"
;;                                              (number-to-string speed))
;;                 collect `(ert-deftest ,test-name ()
;;                            (let ((comp-speed ,speed))
;;                              ,body)))))

(ert-deftest comp-tests-varref ()
  "Testing varref."
  (defun comp-tests-varref-f ()
    comp-tests-var1)

  (should (= (comp-test-apply #'comp-tests-varref-f) 3)))

(ert-deftest comp-tests-list ()
  "Testing cons car cdr."
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

  (should (equal (comp-test-apply #'comp-tests-list-f) '(1 2 3)))
  (should (equal (comp-test-apply #'comp-tests-list2-f 1 2 3) '(1 2 3)))
  (should (= (comp-test-apply #'comp-tests-car-f '(1 . 2)) 1))
  (should (null (comp-test-apply #'comp-tests-car-f nil)))
  (should (= (condition-case err
                 (comp-test-apply #'comp-tests-car-f 3)
               (error 10))
             10))
  (should (= (comp-test-apply #'comp-tests-cdr-f '(1 . 2)) 2))
  (should (null (comp-test-apply #'comp-tests-cdr-f nil)))
  (should (= (condition-case err
                 (comp-test-apply #'comp-tests-cdr-f 3)
               (error 10))
             10))
  (should (= (comp-test-apply #'comp-tests-car-safe-f '(1 . 2)) 1))
  (should (null (comp-test-apply #'comp-tests-car-safe-f 'a)))
  (should (= (comp-test-apply #'comp-tests-cdr-safe-f '(1 . 2)) 2))
  (should (null (comp-test-apply #'comp-tests-cdr-safe-f 'a))))

(ert-deftest  comp-tests-cons-car-cdr ()
  "Testing cons car cdr."
  (defun comp-tests-cons-car-f ()
    (car (cons 1 2)))

  (defun comp-tests-cons-cdr-f (x)
    (cdr (cons 'foo x)))

  (should (= (comp-test-apply #'comp-tests-cons-car-f) 1))
  (should (= (comp-test-apply #'comp-tests-cons-cdr-f 3) 3)))

(ert-deftest comp-tests-varset ()
  "Testing varset."
  (defun comp-tests-varset-f ()
      (setq comp-tests-var1 55))

  (comp-test-apply #'comp-tests-varset-f)

  (should (= comp-tests-var1 55)))

(ert-deftest comp-tests-length ()
  "Testing length."
  (defun comp-tests-length-f ()
      (length '(1 2 3)))

  (should (= (comp-test-apply #'comp-tests-length-f) 3)))

(ert-deftest comp-tests-aref-aset ()
  "Testing aref and aset."
  (defun comp-tests-aref-aset-f ()
    (let ((vec [1 2 3]))
      (aset vec 2 100)
      (aref vec 2)))

  (should (= (comp-test-apply #'comp-tests-aref-aset-f) 100)))

(ert-deftest comp-tests-symbol-value ()
  "Testing aref and aset."
  (defvar comp-tests-var2 3)
  (defun comp-tests-symbol-value-f ()
    (symbol-value 'comp-tests-var2))

  (should (= (comp-test-apply #'comp-tests-symbol-value-f) 3)))

(ert-deftest comp-tests-concat ()
  "Testing concatX opcodes."
  (defun comp-tests-concat-f (x)
    (concat "a" "b" "c" "d"
            (concat "a" "b" "c" (concat "a" "b" (concat "foo" x)))))

  (should (string= (comp-test-apply #'comp-tests-concat-f "bar") "abcdabcabfoobar")))

(defun comp-tests-ffuncall-callee-f (x y z)
    (list x y z))

(ert-deftest comp-tests-ffuncall ()
  "Test calling conventions."
  (native-compile #'comp-tests-ffuncall-calle-f)
  (defun comp-tests-ffuncall-caller-f ()
    (comp-tests-ffuncall-callee-f 1 2 3))

  (should (equal (comp-test-apply #'comp-tests-ffuncall-caller-f) '(1 2 3)))

  (defun comp-tests-ffuncall-callee-optional-f (a b &optional c d)
    (list a b c d))

  (should (equal (comp-test-apply #'comp-tests-ffuncall-callee-optional-f 1 2 3 4)
                 '(1 2 3 4)))
  (should (equal (comp-test-apply #'comp-tests-ffuncall-callee-optional-f 1 2 3)
                 '(1 2 3 nil)))
  (should (equal (comp-test-apply #'comp-tests-ffuncall-callee-optional-f 1 2)
                 '(1 2 nil nil)))

  (defun comp-tests-ffuncall-callee-rest-f (a b &rest c)
    (list a b c))

  (should (equal (comp-test-apply #'comp-tests-ffuncall-callee-rest-f 1 2)
                 '(1 2 nil)))
  (should (equal (comp-test-apply #'comp-tests-ffuncall-callee-rest-f 1 2 3)
                 '(1 2 (3))))
  (should (equal (comp-test-apply #'comp-tests-ffuncall-callee-rest-f 1 2 3 4)
                 '(1 2 (3 4))))

  (defun comp-tests-ffuncall-native-f ()
    "Call a primitive with no dedicate op."
    (make-vector 1 nil))

  (should (equal (comp-test-apply #'comp-tests-ffuncall-native-f) [nil]))

  (defun comp-tests-ffuncall-native-rest-f ()
    "Call a primitive with no dedicate op with &rest."
    (vector 1 2 3))

  (should (equal (comp-test-apply #'comp-tests-ffuncall-native-rest-f) [1 2 3]))

  (defun comp-tests-ffuncall-apply-many-f (x)
    (apply #'list x))

  (should (equal (comp-test-apply #'comp-tests-ffuncall-apply-many-f '(1 2 3))
                 '(1 2 3)))

  (defun comp-tests-ffuncall-lambda-f (x)
    (let ((fun (lambda (x)
                 (1+ x))))
      (funcall fun x)))

  (should (= (comp-test-apply #'comp-tests-ffuncall-lambda-f 1) 2)))

(ert-deftest comp-tests-jump-table ()
  "Testing jump tables"
  (defun comp-tests-jump-table-1-f (x)
    (pcase x
      ('x 'a)
      ('y 'b)
      (_ 'c)))

  (should (eq (comp-test-apply #'comp-tests-jump-table-1-f 'x) 'a))
  (should (eq (comp-test-apply #'comp-tests-jump-table-1-f 'y) 'b))
  (should (eq (comp-test-apply #'comp-tests-jump-table-1-f 'xxx) 'c)))

(ert-deftest comp-tests-conditionals ()
  "Testing conditionals."
  (defun comp-tests-conditionals-1-f (x)
    ;; Generate goto-if-nil
    (if x 1 2))
  (defun comp-tests-conditionals-2-f (x)
    ;; Generate goto-if-nil-else-pop
    (when x
      1340))

  (should (= (comp-test-apply #'comp-tests-conditionals-1-f t) 1))
  (should (= (comp-test-apply #'comp-tests-conditionals-1-f nil) 2))
  (should (= (comp-test-apply #'comp-tests-conditionals-2-f t) 1340))
  (should (eq (comp-test-apply #'comp-tests-conditionals-2-f nil) nil)))

(ert-deftest comp-tests-fixnum ()
  "Testing some fixnum inline operation."
  (defun comp-tests-fixnum-1-minus-f (x)
    ;; Bsub1
    (1- x))
  (defun comp-tests-fixnum-1-plus-f (x)
    ;; Badd1
    (1+ x))
  (defun comp-tests-fixnum-minus-f (x)
    ;; Bnegate
    (- x))

  (should (= (comp-test-apply #'comp-tests-fixnum-1-minus-f 10) 9))
  (should (= (comp-test-apply #'comp-tests-fixnum-1-minus-f most-negative-fixnum)
             (1- most-negative-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-1-minus-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a)))
  (should (= (comp-test-apply #'comp-tests-fixnum-1-plus-f 10) 11))
  (should (= (comp-test-apply #'comp-tests-fixnum-1-plus-f most-positive-fixnum)
             (1+ most-positive-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-1-plus-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a)))
  (should (= (comp-test-apply #'comp-tests-fixnum-minus-f 10) -10))
  (should (= (comp-test-apply #'comp-tests-fixnum-minus-f most-negative-fixnum)
             (- most-negative-fixnum)))
  (should (equal (condition-case err
                     (comp-tests-fixnum-minus-f 'a)
                   (error err))
                 '(wrong-type-argument number-or-marker-p a))))

(ert-deftest comp-tests-arith-comp ()
  "Testing arithmetic comparisons."
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

  (should (eq (comp-test-apply #'comp-tests-eqlsign-f 4 3) nil))
  (should (eq (comp-test-apply #'comp-tests-eqlsign-f 3 3) t))
  (should (eq (comp-test-apply #'comp-tests-eqlsign-f 2 3) nil))
  (should (eq (comp-test-apply #'comp-tests-gtr-f 4 3) t))
  (should (eq (comp-test-apply #'comp-tests-gtr-f 3 3) nil))
  (should (eq (comp-test-apply #'comp-tests-gtr-f 2 3) nil))
  (should (eq (comp-test-apply #'comp-tests-lss-f 4 3) nil))
  (should (eq (comp-test-apply #'comp-tests-lss-f 3 3) nil))
  (should (eq (comp-test-apply #'comp-tests-lss-f 2 3) t))
  (should (eq (comp-test-apply #'comp-tests-les-f 4 3) nil))
  (should (eq (comp-test-apply #'comp-tests-les-f 3 3) t))
  (should (eq (comp-test-apply #'comp-tests-les-f 2 3) t))
  (should (eq (comp-test-apply #'comp-tests-geq-f 4 3) t))
  (should (eq (comp-test-apply #'comp-tests-geq-f 3 3) t))
  (should (eq (comp-test-apply #'comp-tests-geq-f 2 3) nil)))

(ert-deftest comp-tests-setcarcdr ()
  "Testing setcar setcdr."
  (defun comp-tests-setcar-f (x y)
    (setcar x y)
    x)
  (defun comp-tests-setcdr-f (x y)
    (setcdr x y)
    x)

  (should (equal (comp-test-apply #'comp-tests-setcar-f '(10 . 10) 3) '(3 . 10)))
  (should (equal (comp-test-apply #'comp-tests-setcdr-f '(10 . 10) 3) '(10 . 3)))
  (should (equal (condition-case
                     err
                     (comp-tests-setcar-f 3 10)
                   (error err))
                 '(wrong-type-argument consp 3)))
  (should (equal (condition-case
                     err
                     (comp-test-apply #'comp-tests-setcdr-f 3 10)
                   (error err))
                 '(wrong-type-argument consp 3))))

(ert-deftest comp-tests-bubble-sort ()
  "Run bubble sort."
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

  (let* ((list1 (mapcar 'random (make-list 1000 most-positive-fixnum)))
         (list2 (copy-sequence list1)))
    (should (equal (comp-bubble-sort-f list1)
                   (sort list2 #'<)))))

(ert-deftest comp-test-apply ()
  "Test some inlined list functions."
  (defun comp-tests-consp-f (x)
    ;; Bconsp
    (consp x))
  (defun comp-tests-car-f (x)
    ;; Bsetcar
    (setcar x 3))

  (should (eq (comp-test-apply #'comp-tests-consp-f '(1)) t))
  (should (eq (comp-test-apply #'comp-tests-consp-f 1) nil))
  (let ((x (cons 1 2)))
    (should (= (comp-test-apply #'comp-tests-car-f x) 3))
    (should (equal x '(3 . 2)))))

(ert-deftest comp-tests-num-inline ()
  "Test some inlined number functions."
  (defun comp-tests-integerp-f (x)
    ;; Bintegerp
    (integerp x))
  (defun comp-tests-numberp-f (x)
    ;; Bnumberp
    (numberp x))

  (should (eq (comp-test-apply #'comp-tests-integerp-f 1) t))
  (should (eq (comp-test-apply #'comp-tests-integerp-f '(1)) nil))
  (should (eq (comp-test-apply #'comp-tests-integerp-f 3.5) nil))
  (should (eq (comp-test-apply #'comp-tests-integerp-f (1+ most-negative-fixnum)) t))

  (should (eq (comp-test-apply #'comp-tests-numberp-f 1) t))
  (should (eq (comp-test-apply #'comp-tests-numberp-f 'a) nil))
  (should (eq (comp-test-apply #'comp-tests-numberp-f 3.5) t)))

(ert-deftest comp-tests-stack ()
  "Test some stack operation."
  (defun comp-tests-discardn-f (x)
    ;; BdiscardN
    (1+ (let ((a 1)
            (_b)
            (_c))
          a)))
  (defun comp-tests-insertn-f (a b c d)
    ;; Binsert
    (insert a b c d))

  (should (= (comp-test-apply #'comp-tests-discardn-f 10) 2))
  (should (string= (with-temp-buffer
                      (comp-tests-insertn-f "a" "b" "c" "d")
                      (buffer-string))
                   "abcd")))

(ert-deftest comp-tests-non-locals ()
  "Test non locals."
  (let ((gc-cons-threshold most-positive-fixnum)) ;; FIXME!!
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

    (should (string= (comp-test-apply #'comp-tests-condition-case-0-f)
                     "arith-error Arithmetic error catched"))
    (should (string= (comp-test-apply #'comp-tests-condition-case-1-f)
                     "error foo catched"))
    (should (= (comp-test-apply #'comp-tests-catch-f
                                (lambda () (throw 'foo 3)))
               3))
    (should (= (catch 'foo
                 (comp-tests-throw-f 3))))))

(ert-deftest comp-tests-gc ()
  "Try to do some longer computation to let the gc kick in."
  (dotimes (_ 100000)
    (comp-tests-cons-cdr-f 3))

  (should (= (comp-test-apply #'comp-tests-cons-cdr-f 3) 3)))

(ert-deftest comp-tests-buffer ()
  (defun comp-tests-buff0-f ()
    (with-temp-buffer
      (insert "foo")
      (buffer-string)))

  (should (string= (comp-test-apply #'comp-tests-buff0-f) "foo")))

;;;;;;;;;;;;;;;;;;;;
;; Tromey's tests ;;
;;;;;;;;;;;;;;;;;;;;

;; Test Bconsp.
(defun comp-test-consp (x) (consp x))

(ert-deftest comp-consp ()
  (should-not (comp-test-apply 'comp-test-consp 23))
  (should-not (comp-test-apply 'comp-test-consp nil))
  (should (comp-test-apply 'comp-test-consp '(1 . 2))))

;; Test Blistp.
(defun comp-test-listp (x) (listp x))

(ert-deftest comp-listp ()
  (should-not (comp-test-apply 'comp-test-listp 23))
  (should (comp-test-apply 'comp-test-listp nil))
  (should (comp-test-apply 'comp-test-listp '(1 . 2))))

;; Test Bstringp.
(defun comp-test-stringp (x) (stringp x))

(ert-deftest comp-stringp ()
  (should-not (comp-test-apply 'comp-test-stringp 23))
  (should-not (comp-test-apply 'comp-test-stringp nil))
  (should (comp-test-apply 'comp-test-stringp "hi")))

;; Test Bsymbolp.
(defun comp-test-symbolp (x) (symbolp x))

(ert-deftest comp-symbolp ()
  (should-not (comp-test-apply 'comp-test-symbolp 23))
  (should-not (comp-test-apply 'comp-test-symbolp "hi"))
  (should (comp-test-apply 'comp-test-symbolp 'whatever)))

;; Test Bintegerp.
(defun comp-test-integerp (x) (integerp x))

(ert-deftest comp-integerp ()
  (should (comp-test-apply 'comp-test-integerp 23))
  (should-not (comp-test-apply 'comp-test-integerp 57.5))
  (should-not (comp-test-apply 'comp-test-integerp "hi"))
  (should-not (comp-test-apply 'comp-test-integerp 'whatever)))

;; Test Bnumberp.
(defun comp-test-numberp (x) (numberp x))

(ert-deftest comp-numberp ()
  (should (comp-test-apply 'comp-test-numberp 23))
  (should (comp-test-apply 'comp-test-numberp 57.5))
  (should-not (comp-test-apply 'comp-test-numberp "hi"))
  (should-not (comp-test-apply 'comp-test-numberp 'whatever)))

;; Test Badd1.
(defun comp-test-add1 (x) (1+ x))

(ert-deftest comp-add1 ()
  (should (eq (comp-test-apply 'comp-test-add1 23) 24))
  (should (eq (comp-test-apply 'comp-test-add1 -17) -16))
  (should (eql (comp-test-apply 'comp-test-add1 1.0) 2.0))
  (should-error (comp-test-apply 'comp-test-add1 nil)
                :type 'wrong-type-argument))

;; Test Bsub1.
(defun comp-test-sub1 (x) (1- x))

(ert-deftest comp-sub1 ()
  (should (eq (comp-test-apply 'comp-test-sub1 23) 22))
  (should (eq (comp-test-apply 'comp-test-sub1 -17) -18))
  (should (eql (comp-test-apply 'comp-test-sub1 1.0) 0.0))
  (should-error (comp-test-apply 'comp-test-sub1 nil)
                :type 'wrong-type-argument))

;; Test Bneg.
(defun comp-test-negate (x) (- x))

(ert-deftest comp-negate ()
  (should (eq (comp-test-apply 'comp-test-negate 23) -23))
  (should (eq (comp-test-apply 'comp-test-negate -17) 17))
  (should (eql (comp-test-apply 'comp-test-negate 1.0) -1.0))
  (should-error (comp-test-apply 'comp-test-negate nil)
                :type 'wrong-type-argument))

;; Test Bnot.
(defun comp-test-not (x) (not x))

(ert-deftest comp-not ()
  (should (eq (comp-test-apply 'comp-test-not 23) nil))
  (should (eq (comp-test-apply 'comp-test-not nil) t))
  (should (eq (comp-test-apply 'comp-test-not t) nil)))

;; Test Bbobp, Beobp, Bpoint, Bpoint_min, Bpoint_max.
(defun comp-test-bobp () (bobp))
(defun comp-test-eobp () (eobp))
(defun comp-test-point () (point))
(defun comp-test-point-min () (point-min))
(defun comp-test-point-max () (point-max))

(ert-deftest comp-bobp-and-eobp ()
  (with-temp-buffer
    (should (comp-test-apply 'comp-test-bobp))
    (should (comp-test-apply 'comp-test-eobp))
    (insert "hi")
    (goto-char (point-min))
    (should (eq (comp-test-apply 'comp-test-point-min) (point-min)))
    (should (eq (comp-test-apply 'comp-test-point) (point-min)))
    (should (comp-test-apply 'comp-test-bobp))
    (should-not (comp-test-apply 'comp-test-eobp))
    (goto-char (point-max))
    (should (eq (comp-test-apply 'comp-test-point-max) (point-max)))
    (should (eq (comp-test-apply 'comp-test-point) (point-max)))
    (should-not (comp-test-apply 'comp-test-bobp))
    (should (comp-test-apply 'comp-test-eobp))))

;; Test Bcar and Bcdr.
(defun comp-test-car (x) (car x))
(defun comp-test-cdr (x) (cdr x))

(ert-deftest comp-car-cdr ()
  (let ((pair '(1 . b)))
    (should (eq (comp-test-apply 'comp-test-car pair) 1))
    (should (eq (comp-test-apply 'comp-test-car nil) nil))
    (should-error (comp-test-apply 'comp-test-car 23)
                  :type 'wrong-type-argument)
    (should (eq (comp-test-apply 'comp-test-cdr pair) 'b))
    (should (eq (comp-test-apply 'comp-test-cdr nil) nil))
    (should-error (comp-test-apply 'comp-test-cdr 23)
                  :type 'wrong-type-argument)))

;; Test Bcar_safe and Bcdr_safe.
(defun comp-test-car-safe (x) (car-safe x))
(defun comp-test-cdr-safe (x) (cdr-safe x))

(ert-deftest comp-car-cdr-safe ()
  (let ((pair '(1 . b)))
    (should (eq (comp-test-apply 'comp-test-car-safe pair) 1))
    (should (eq (comp-test-apply 'comp-test-car-safe nil) nil))
    (should (eq (comp-test-apply 'comp-test-car-safe 23) nil))
    (should (eq (comp-test-apply 'comp-test-cdr-safe pair) 'b))
    (should (eq (comp-test-apply 'comp-test-cdr-safe nil) nil))
    (should (eq (comp-test-apply 'comp-test-cdr-safe 23) nil))))

;; Test Beq.
(defun comp-test-eq (x y) (eq x y))

(ert-deftest comp-eq ()
  (should (comp-test-apply 'comp-test-eq 'a 'a))
  (should (comp-test-apply 'comp-test-eq 5 5))
  (should-not (comp-test-apply 'comp-test-eq 'a 'b))
  (should-not (comp-test-apply 'comp-test-eq "x" "x")))

;; Test Bgotoifnil.
(defun comp-test-if (x y) (if x x y))

(ert-deftest comp-if ()
  (should (eq (comp-test-apply 'comp-test-if 'a 'b) 'a))
  (should (eq (comp-test-apply 'comp-test-if 0 23) 0))
  (should (eq (comp-test-apply 'comp-test-if nil 'b) 'b)))

;; Test Bgotoifnilelsepop.
(defun comp-test-and (x y) (and x y))

(ert-deftest comp-and ()
  (should (eq (comp-test-apply 'comp-test-and 'a 'b) 'b))
  (should (eq (comp-test-apply 'comp-test-and 0 23) 23))
  (should (eq (comp-test-apply 'comp-test-and nil 'b) nil)))

;; Test Bgotoifnonnilelsepop.
(defun comp-test-or (x y) (or x y))

(ert-deftest comp-or ()
  (should (eq (comp-test-apply 'comp-test-or 'a 'b) 'a))
  (should (eq (comp-test-apply 'comp-test-or 0 23) 0))
  (should (eq (comp-test-apply 'comp-test-or nil 'b) 'b)))

;; Test Bsave_excursion.
(defun comp-test-save-excursion ()
  (save-excursion
    (insert "XYZ")))

;; Test Bcurrent_buffer.
(defun comp-test-current-buffer () (current-buffer))

(ert-deftest comp-save-excursion ()
  (with-temp-buffer
    (comp-test-apply 'comp-test-save-excursion)
    (should (eq (point) (point-min)))
    (should (eq (comp-test-apply 'comp-test-current-buffer) (current-buffer)))))

;; Test Bgtr.
(defun comp-test-> (a b)
  (> a b))

(ert-deftest comp-> ()
  (should (eq (comp-test-apply 'comp-test-> 0 23) nil))
  (should (eq (comp-test-apply 'comp-test-> 23 0) t)))

;; Test Bpushcatch.
(defun comp-test-catch (&rest l)
  (catch 'done
    (dolist (v l)
      (when (> v 23)
        (throw 'done v)))))

(ert-deftest comp-catch ()
  (should (eq (comp-test-apply 'comp-test-catch 0 1 2 3 4) nil))
  (should (eq (comp-test-apply 'comp-test-catch 20 21 22 23 24 25 26 27 28) 24)))

;; Test Bmemq.
(defun comp-test-memq (val list)
  (memq val list))

(ert-deftest comp-memq ()
  (should (equal (comp-test-apply 'comp-test-memq 0 '(5 4 3 2 1 0)) '(0)))
  (should (eq (comp-test-apply 'comp-test-memq 72 '(5 4 3 2 1 0)) nil)))

;; Test BlistN.
(defun comp-test-listN (x)
  (list x x x x x x x x x x x x x x x x))

(ert-deftest comp-listN ()
  (should (equal (comp-test-apply 'comp-test-listN 57)
                 '(57 57 57 57 57 57 57 57 57 57 57 57 57 57 57 57))))

;; Test BconcatN.
(defun comp-test-concatN (x)
  (concat x x x x x x))

(ert-deftest comp-concatN ()
  (should (equal (comp-test-apply 'comp-test-concatN "x") "xxxxxx")))

;; Test optional and rest arguments.
(defun comp-test-opt-rest (a &optional b &rest c)
  (list a b c))

(ert-deftest comp-opt-rest ()
  (should (equal (comp-test-apply 'comp-test-opt-rest 1) '(1 nil nil)))
  (should (equal (comp-test-apply 'comp-test-opt-rest 1 2) '(1 2 nil)))
  (should (equal (comp-test-apply 'comp-test-opt-rest 1 2 3) '(1 2 (3))))
  (should (equal (comp-test-apply 'comp-test-opt-rest 1 2 56 57 58)
                 '(1 2 (56 57 58)))))

;; Test for too many arguments.
(defun comp-test-opt (a &optional b)
  (cons a b))

(ert-deftest comp-opt ()
  (should (equal (comp-test-apply 'comp-test-opt 23) '(23)))
  (should (equal (comp-test-apply 'comp-test-opt 23 24) '(23 . 24)))
  (should-error (comp-test-apply 'comp-test-opt)
                :type 'wrong-number-of-arguments)
  (should-error (comp-test-apply 'comp-test-opt nil 24 97)
                :type 'wrong-number-of-arguments))

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

(ert-deftest comp-unwind-protect ()
  (comp-test-unwind-protect 'ignore)
  (should (eq comp-test-up-val 999))
  (condition-case nil
      (comp-test-unwind-protect (lambda () (error "HI")))
    (error
     nil))
  (should (eq comp-test-up-val 999)))

;;; comp-tests.el ends here
