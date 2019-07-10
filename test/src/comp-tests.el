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

(setq garbage-collection-messages t)

(defvar comp-tests-var1 3)

(ert-deftest  comp-tests-varref ()
  "Testing varref."
  (defun comp-tests-varref-f ()
    comp-tests-var1)

  (native-compile #'comp-tests-varref-f)

  (should (= (comp-tests-varref-f) 3)))

(ert-deftest comp-tests-list ()
  "Testing cons car cdr."
  ;; (defun comp-tests-list-f ()
  ;;   (list 1 2 3))
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

  ;; (native-compile #'comp-tests-list-f)
  (native-compile #'comp-tests-car-f)
  (native-compile #'comp-tests-cdr-f)
  (native-compile #'comp-tests-car-safe-f)
  (native-compile #'comp-tests-cdr-safe-f)

  ;; (should (equal (comp-tests-list-f) '(1 2 3)))
  (should (= (comp-tests-car-f '(1 . 2)) 1))
  (should (null (comp-tests-car-f nil)))
  (should (= (condition-case err
                 (comp-tests-car-f 3)
               (error 10))
             10))
  (should (= (comp-tests-cdr-f '(1 . 2)) 2))
  (should (null (comp-tests-cdr-f nil)))
  (should (= (condition-case err
                 (comp-tests-cdr-f 3)
               (error 10))
             10))
  (should (= (comp-tests-car-safe-f '(1 . 2)) 1))
  (should (null (comp-tests-car-safe-f 'a)))
  (should (= (comp-tests-cdr-safe-f '(1 . 2)) 2))
  (should (null (comp-tests-cdr-safe-f 'a))))

;; (ert-deftest  comp-tests-cons-car-cdr ()
;;   "Testing cons car cdr."
;;   (defun comp-tests-cons-car-f ()
;;     (car (cons 1 2)))
;;   (native-compile #'comp-tests-cons-car-f)

;;   (defun comp-tests-cons-cdr-f (x)
;;     (cdr (cons 'foo x)))
;;   (native-compile #'comp-tests-cons-cdr-f)

;;   (should (= (comp-tests-cons-car-f) 1))
;;   (should (= (comp-tests-cons-cdr-f 3) 3)))

;; (ert-deftest  comp-tests-varset ()
;;   "Testing varset."
;;   (defun comp-tests-varset-f ()
;;       (setq comp-tests-var1 55))
;;   (native-compile #'comp-tests-varset-f)

;;   (comp-tests-varset-f)

;;   (should (= comp-tests-var1 55)))

;; (ert-deftest  comp-tests-length ()
;;   "Testing length."
;;   (defun comp-tests-length-f ()
;;       (length '(1 2 3)))
;;   (native-compile #'comp-tests-length-f)

;;   (should (= (comp-tests-length-f) 3)))

;; (ert-deftest  comp-tests-aref-aset ()
;;   "Testing aref and aset."
;;   (defun comp-tests-aref-aset-f ()
;;     (let ((vec [1 2 3]))
;;       (aset vec 2 100)
;;       (aref vec 2)))
;;   (native-compile #'comp-tests-aref-aset-f)

;;   (should (= (comp-tests-aref-aset-f) 100)))

;; (ert-deftest  comp-tests-symbol-value ()
;;   "Testing aref and aset."
;;   (defvar comp-tests-var2 3)
;;   (defun comp-tests-symbol-value-f ()
;;     (symbol-value 'comp-tests-var2))
;;   (native-compile #'comp-tests-symbol-value-f)

;;   (should (= (comp-tests-symbol-value-f) 3)))

;; (ert-deftest  comp-tests-concat ()
;;   "Testing concatX opcodes."
;;   (defun comp-tests-concat-f (x)
;;     (concat "a" "b" "c" "d"
;;             (concat "a" "b" "c" (concat "a" "b" (concat "foo" x)))))
;;   (native-compile #'comp-tests-concat-f)

;;   (should (string= (comp-tests-concat-f "bar") "abcdabcabfoobar")))

;; (ert-deftest  comp-tests-ffuncall ()
;;   "Test calling conventions."
;;   (defun comp-tests-ffuncall-callee-f (x y z)
;;     (list x y z))
;;   (defun comp-tests-ffuncall-caller-f ()
;;     (comp-tests-ffuncall-callee-f 1 2 3))

;;   (native-compile #'comp-tests-ffuncall-caller-f)

;;   (should (equal (comp-tests-ffuncall-caller-f) '(1 2 3)))

;;   (defun comp-tests-ffuncall-callee-optional-f (a b &optional c d)
;;     (list a b c d))
;;   (native-compile #'comp-tests-ffuncall-callee-optional-f)

;;   (should (equal (comp-tests-ffuncall-callee-optional-f 1 2 3 4) '(1 2 3 4)))
;;   (should (equal (comp-tests-ffuncall-callee-optional-f 1 2 3) '(1 2 3 nil)))
;;   (should (equal (comp-tests-ffuncall-callee-optional-f 1 2) '(1 2 nil nil)))

;;   (defun comp-tests-ffuncall-callee-rest-f (a b &rest c)
;;     (list a b c))
;;   (native-compile #'comp-tests-ffuncall-callee-rest-f)

;;   (should (equal (comp-tests-ffuncall-callee-rest-f 1 2) '(1 2 nil)))
;;   (should (equal (comp-tests-ffuncall-callee-rest-f 1 2 3) '(1 2 (3))))
;;   (should (equal (comp-tests-ffuncall-callee-rest-f 1 2 3 4) '(1 2 (3 4))))

;;   (defun comp-tests-ffuncall-native-f ()
;;     "Call a primitive with no dedicate op."
;;     (make-vector 1 nil))

;;   (native-compile #'comp-tests-ffuncall-native-f)

;;   (should (equal (comp-tests-ffuncall-native-f) [nil]))

;;   (defun comp-tests-ffuncall-native-rest-f ()
;;     "Call a primitive with no dedicate op with &rest."
;;     (vector 1 2 3))

;;   (native-compile #'comp-tests-ffuncall-native-rest-f)

;;   (should (equal (comp-tests-ffuncall-native-rest-f) [1 2 3]))

;;   (defun comp-tests-ffuncall-apply-many-f (x)
;;     (apply #'list x))

;;   (native-compile #'comp-tests-ffuncall-apply-many-f)

;;   (should (equal (comp-tests-ffuncall-apply-many-f '(1 2 3)) '(1 2 3)))

;;   (defun comp-tests-ffuncall-lambda-f (x)
;;     (let ((fun (lambda (x)
;;                  (1+ x))))
;;       (funcall fun x)))

;;   (native-compile #'comp-tests-ffuncall-lambda-f)

;;   (should (= (comp-tests-ffuncall-lambda-f 1) 2)))

;; (ert-deftest  comp-tests-jump-table ()
;;   "Testing jump tables"
;;   (defun comp-tests-jump-table-1-f (x)
;;     (pcase x
;;       ('x 'a)
;;       ('y 'b)
;;       (_ 'c)))


;;   (should (eq (comp-tests-jump-table-1-f 'x) 'a))
;;   (should (eq (comp-tests-jump-table-1-f 'y) 'b))
;;   (should (eq (comp-tests-jump-table-1-f 'xxx) 'c)))

;; (ert-deftest  comp-tests-conditionals ()
;;   "Testing conditionals."
;;   (defun comp-tests-conditionals-1-f (x)
;;     ;; Generate goto-if-nil
;;     (if x 1 2))
;;   (defun comp-tests-conditionals-2-f (x)
;;     ;; Generate goto-if-nil-else-pop
;;     (when x
;;         1340))
;;   (native-compile #'comp-tests-conditionals-1-f)
;;   (native-compile #'comp-tests-conditionals-2-f)

;;   (should (= (comp-tests-conditionals-1-f t) 1))
;;   (should (= (comp-tests-conditionals-1-f nil) 2))
;;   (should (= (comp-tests-conditionals-2-f t) 1340))
;;   (should (eq (comp-tests-conditionals-2-f nil) nil)))

;; (ert-deftest  comp-tests-fixnum ()
;;   "Testing some fixnum inline operation."
;;   (defun comp-tests-fixnum-1-minus-f (x)
;;     ;; Bsub1
;;     (1- x))
;;   (defun comp-tests-fixnum-1-plus-f (x)
;;     ;; Badd1
;;     (1+ x))
;;   (defun comp-tests-fixnum-minus-f (x)
;;     ;; Bnegate
;;     (- x))

;;   (native-compile #'comp-tests-fixnum-1-minus-f)
;;   (native-compile #'comp-tests-fixnum-1-plus-f)
;;   (native-compile #'comp-tests-fixnum-minus-f)

;;   (should (= (comp-tests-fixnum-1-minus-f 10) 9))
;;   (should (= (comp-tests-fixnum-1-minus-f most-negative-fixnum)
;;              (1- most-negative-fixnum)))
;;   (should (equal (condition-case err
;;                      (comp-tests-fixnum-1-minus-f 'a)
;;                    (error err))
;;                  '(wrong-type-argument number-or-marker-p a)))
;;   (should (= (comp-tests-fixnum-1-plus-f 10) 11))
;;   (should (= (comp-tests-fixnum-1-plus-f most-positive-fixnum)
;;              (1+ most-positive-fixnum)))
;;   (should (equal (condition-case err
;;                      (comp-tests-fixnum-1-plus-f 'a)
;;                    (error err))
;;                  '(wrong-type-argument number-or-marker-p a)))
;;   (should (= (comp-tests-fixnum-minus-f 10) -10))
;;   (should (= (comp-tests-fixnum-minus-f most-negative-fixnum)
;;              (- most-negative-fixnum)))
;;   (should (equal (condition-case err
;;                      (comp-tests-fixnum-minus-f 'a)
;;                    (error err))
;;                  '(wrong-type-argument number-or-marker-p a))))

;; (ert-deftest  comp-tests-arith-comp ()
;;   "Testing arithmetic comparisons."
;;   (defun comp-tests-eqlsign-f (x y)
;;     ;; Beqlsign
;;     (= x y))
;;   (defun comp-tests-gtr-f (x y)
;;     ;; Bgtr
;;     (> x y))
;;   (defun comp-tests-lss-f (x y)
;;     ;; Blss
;;     (< x y))
;;   (defun comp-tests-les-f (x y)
;;     ;; Bleq
;;     (<= x y))
;;   (defun comp-tests-geq-f (x y)
;;     ;; Bgeq
;;     (>= x y))


;;   (native-compile #'comp-tests-eqlsign-f)
;;   (native-compile #'comp-tests-gtr-f)
;;   (native-compile #'comp-tests-lss-f)
;;   (native-compile #'comp-tests-les-f)
;;   (native-compile #'comp-tests-geq-f)

;;   (should (eq (comp-tests-eqlsign-f 4 3) nil))
;;   (should (eq (comp-tests-eqlsign-f 3 3) t))
;;   (should (eq (comp-tests-eqlsign-f 2 3) nil))
;;   (should (eq (comp-tests-gtr-f 4 3) t))
;;   (should (eq (comp-tests-gtr-f 3 3) nil))
;;   (should (eq (comp-tests-gtr-f 2 3) nil))
;;   (should (eq (comp-tests-lss-f 4 3) nil))
;;   (should (eq (comp-tests-lss-f 3 3) nil))
;;   (should (eq (comp-tests-lss-f 2 3) t))
;;   (should (eq (comp-tests-les-f 4 3) nil))
;;   (should (eq (comp-tests-les-f 3 3) t))
;;   (should (eq (comp-tests-les-f 2 3) t))
;;   (should (eq (comp-tests-geq-f 4 3) t))
;;   (should (eq (comp-tests-geq-f 3 3) t))
;;   (should (eq (comp-tests-geq-f 2 3) nil)))

;; (ert-deftest comp-tests-setcarcdr ()
;;   "Testing setcar setcdr."
;;   (defun comp-tests-setcar-f (x y)
;;     (setcar x y)
;;     x)
;;   (defun comp-tests-setcdr-f (x y)
;;     (setcdr x y)
;;     x)

;;   (native-compile #'comp-tests-setcar-f)
;;   (native-compile #'comp-tests-setcdr-f)

;;   (should (equal (comp-tests-setcar-f '(10 . 10) 3) '(3 . 10)))
;;   (should (equal (comp-tests-setcdr-f '(10 . 10) 3) '(10 . 3)))
;;   (should (equal (condition-case
;;                      err
;;                      (comp-tests-setcar-f 3 10)
;;                    (error err))
;;                  '(wrong-type-argument consp 3)))
;;   (should (equal (condition-case
;;                      err
;;                      (comp-tests-setcdr-f 3 10)
;;                    (error err))
;;                  '(wrong-type-argument consp 3))))

;; (ert-deftest comp-tests-bubble-sort ()
;;   "Run bubble sort."
;;   (defun comp-bubble-sort-f (list)
;;     (let ((i (length list)))
;;       (while (> i 1)
;;         (let ((b list))
;;           (while (cdr b)
;;             (when (< (cadr b) (car b))
;;               (setcar b (prog1 (cadr b)
;;                           (setcdr b (cons (car b) (cddr b))))))
;;             (setq b (cdr b))))
;;         (setq i (1- i)))
;;       list))

;;   (native-compile #'comp-bubble-sort-f)

;;   (let* ((list1 (mapcar 'random (make-list 1000 most-positive-fixnum)))
;;          (list2 (copy-sequence list1)))
;;     (should (equal (comp-bubble-sort-f list1)
;;                    (sort list2 #'<)))))

;; (ert-deftest comp-tests-list-inline ()
;;   "Test some inlined list functions."
;;   (defun comp-tests-consp-f (x)
;;     ;; Bconsp
;;     (consp x))
;;   (defun comp-tests-car-f (x)
;;     ;; Bsetcar
;;     (setcar x 3))

;;   (native-compile #'comp-tests-consp-f)
;;   (native-compile #'comp-tests-car-f)

;;   (should (eq (comp-tests-consp-f '(1)) t))
;;   (should (eq (comp-tests-consp-f 1) nil))
;;   (let ((x (cons 1 2)))
;;     (should (= (comp-tests-car-f x) 3))
;;     (should (equal x '(3 . 2)))))

;; (ert-deftest comp-tests-num-inline ()
;;   "Test some inlined number functions."
;;   (defun comp-tests-integerp-f (x)
;;     ;; Bintegerp
;;     (integerp x))
;;   (defun comp-tests-numberp-f (x)
;;     ;; Bnumberp
;;     (numberp x))

;;   (native-compile #'comp-tests-integerp-f)
;;   (native-compile #'comp-tests-numberp-f)

;;   (should (eq (comp-tests-integerp-f 1) t))
;;   (should (eq (comp-tests-integerp-f '(1)) nil))
;;   (should (eq (comp-tests-integerp-f 3.5) nil))
;;   (should (eq (comp-tests-integerp-f (1+ most-negative-fixnum)) t))

;;   (should (eq (comp-tests-numberp-f 1) t))
;;   (should (eq (comp-tests-numberp-f 'a) nil))
;;   (should (eq (comp-tests-numberp-f 3.5) t)))

;; (ert-deftest comp-tests-stack ()
;;   "Test some stack operation."
;;   (defun comp-tests-discardn-f (x)
;;     ;; BdiscardN
;;     (1+ (let ((a 1)
;;             (_b)
;;             (_c))
;;           a)))
;;   (defun comp-tests-insertn-f (a b c d)
;;     ;; Binsert
;;     (insert a b c d))

;;   (native-compile #'comp-tests-discardn-f)
;;   (native-compile #'comp-tests-insertn-f)

;;   (should (= (comp-tests-discardn-f 10) 2))

;;   (should (string= (with-temp-buffer
;;                       (comp-tests-insertn-f "a" "b" "c" "d")
;;                       (buffer-string))
;;                    "abcd")))

;; (ert-deftest comp-tests-non-locals ()
;;   "Test non locals."
;;   (defun comp-tests-err-arith-f ()
;;     (/ 1 0))
;;   (defun comp-tests-err-foo-f ()
;;     (error "foo"))

;;   (defun comp-tests-condition-case-0-f ()
;;     ;; Bpushhandler Bpophandler
;;     (condition-case
;;         err
;;         (comp-tests-err-arith-f)
;;       (arith-error (concat "arith-error "
;;                            (error-message-string err)
;;                            " catched"))
;;       (error (concat "error "
;;                      (error-message-string err)
;;                      " catched"))))

;;   (defun comp-tests-condition-case-1-f ()
;;     ;; Bpushhandler Bpophandler
;;     (condition-case
;;         err
;;         (comp-tests-err-foo-f)
;;       (arith-error (concat "arith-error "
;;                            (error-message-string err)
;;                            " catched"))
;;       (error (concat "error "
;;                      (error-message-string err)
;;                      " catched"))))

;;   (defun comp-tests-catch-f (f)
;;     (catch 'foo
;;       (funcall f)))

;;   (defun comp-tests-throw-f (x)
;;     (throw 'foo x))

;;   (native-compile #'comp-tests-condition-case-0-f)
;;   (native-compile #'comp-tests-condition-case-1-f)
;;   (native-compile #'comp-tests-catch-f)
;;   (native-compile #'comp-tests-throw-f)

;;   (should (string= (comp-tests-condition-case-0-f)
;;                    "arith-error Arithmetic error catched"))
;;   (should (string= (comp-tests-condition-case-1-f)
;;                    "error foo catched"))
;;   (should (= (comp-tests-catch-f (lambda () (throw 'foo 3))) 3))
;;   (should (= (catch 'foo
;;                (comp-tests-throw-f 3)))))

;; (ert-deftest comp-tests-gc ()
;;   "Try to do some longer computation to let the gc kick in."
;;   (dotimes (_ 100000)
;;     (comp-tests-cons-cdr-f 3))

;;   (should (= (comp-tests-cons-cdr-f 3) 3)))

;;; comp-tests.el ends here
