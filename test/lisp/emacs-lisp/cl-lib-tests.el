;;; cl-lib-tests.el --- tests for emacs-lisp/cl-lib.el  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

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

;; Some of these tests were extracted from ert-tests.el, back when ert
;; used to reimplement some cl functions.

;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest cl-lib-test-pushnew ()
  (let ((list '(1 2 3)))
    (cl-pushnew 0 list)
    (should (equal list '(0 1 2 3))))
  (let ((list '((1 2) (3 4))))
    (cl-pushnew '(3 7) list :key #'cdr)
    (should (equal list '((3 7) (1 2) (3 4)) )))
  (let ((list '((1 2) (3 4))))
    (cl-pushnew '(3 7) list :key #'car)
    (should (equal list '((1 2) (3 4)))))
  (let ((list '((1 2) (3 4))))
    (cl-pushnew '(3 4) list :test #'equal)
    (should (equal list '((1 2) (3 4)))))
  (let ((list '((1 2) (3 4))))
    (cl-pushnew '(3 5) list :test #'equal)
    (should (equal list '((3 5) (1 2) (3 4)))))
  (let ((list '((1 2) (3 4))))
    (cl-pushnew '(3 4) list :test-not #'equal)
    (should (equal list '((1 2) (3 4)))))
  (let ((list '((1 2) (3 4))))
    (cl-pushnew '(3 5) list :test-not #'equal)
    (should (equal list '((1 2) (3 4))))))

(ert-deftest cl-lib-test-values-list ()
  (let ((list '(:a :b :c)))
    (should (equal (cl-values-list list) '(:a :b :c))))
  (let ((not-a-list :a))
    (should-error (cl-values-list not-a-list) :type 'wrong-type-argument)))

(ert-deftest cl-lib-multiple-value-list ()
  (should (equal (cl-multiple-value-list 1) 1))
  (should (equal (cl-multiple-value-list '(1 2 3)) '(1 2 3)))
  (should (equal (cl-multiple-value-list "string") "string"))
  (should (equal (cl-multiple-value-list nil) nil))
  (should (equal (cl-multiple-value-list (list 1 2 3)) '(1 2 3))))

(ert-deftest cl-digit-char-p ()
  (should (eql 3 (cl-digit-char-p ?3)))
  (should (eql 10 (cl-digit-char-p ?a 11)))
  (should (eql 10 (cl-digit-char-p ?A 11)))
  (should-not (cl-digit-char-p ?a))
  (should (eql 32 (cl-digit-char-p ?w 36)))
  (should-error (cl-digit-char-p ?a 37) :type 'args-out-of-range)
  (should-error (cl-digit-char-p ?a 1) :type 'args-out-of-range))

(ert-deftest cl-lib-test-first ()
  (should (null (cl-first '())))
  (should (= 4 (cl-first '(4))))
  (should (= 4 (cl-first '(4 2))))
  (should-error (cl-first "42") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-second ()
  (should (null (cl-second '())))
  (should (null (cl-second '(4))))
  (should (= 2 (cl-second '(1 2))))
  (should (= 2 (cl-second '(1 2 3))))
  (should-error (cl-second "1 2 3") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-third ()
  (should (null (cl-third '())))
  (should (null (cl-third '(1 2))))
  (should (= 3 (cl-third '(1 2 3))))
  (should (= 3 (cl-third '(1 2 3 4))))
  (should-error (cl-third "123") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-fourth ()
  (should (null (cl-fourth '())))
  (should (null (cl-fourth '(1 2 3))))
  (should (= 4 (cl-fourth '(1 2 3 4))))
  (should (= 4 (cl-fourth '(1 2 3 4 5))))
  (should-error (cl-fourth "1234") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-fifth ()
  (should (null (cl-fifth '())))
  (should (null (cl-fifth '(1 2 3 4))))
  (should (= 5 (cl-fifth '(1 2 3 4 5))))
  (should (= 5 (cl-fifth '(1 2 3 4 5 6))))
  (should-error (cl-fifth "12345") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-sixth ()
  (should (null (cl-sixth '())))
  (should (null (cl-sixth '(1 2 3 4 5))))
  (should (= 6 (cl-sixth '(1 2 3 4 5 6))))
  (should (= 6 (cl-sixth '(1 2 3 4 5 6 7))))
  (should-error (cl-sixth "123456") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-seventh ()
  (should (null (cl-seventh '())))
  (should (null (cl-seventh '(1 2 3 4 5 6))))
  (should (= 7 (cl-seventh '(1 2 3 4 5 6 7))))
  (should (= 7 (cl-seventh '(1 2 3 4 5 6 7 8))))
  (should-error (cl-seventh "1234567") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-eighth ()
  (should (null (cl-eighth '())))
  (should (null (cl-eighth '(1 2 3 4 5 6 7))))
  (should (= 8 (cl-eighth '(1 2 3 4 5 6 7 8))))
  (should (= 8 (cl-eighth '(1 2 3 4 5 6 7 8 9))))
  (should-error (cl-eighth "12345678") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-ninth ()
  (should (null (cl-ninth '())))
  (should (null (cl-ninth '(1 2 3 4 5 6 7 8))))
  (should (= 9 (cl-ninth '(1 2 3 4 5 6 7 8 9))))
  (should (= 9 (cl-ninth '(1 2 3 4 5 6 7 8 9 10))))
  (should-error (cl-ninth "123456789") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-tenth ()
  (should (null (cl-tenth '())))
  (should (null (cl-tenth '(1 2 3 4 5 6 7 8 9))))
  (should (= 10 (cl-tenth '(1 2 3 4 5 6 7 8 9 10))))
  (should (= 10 (cl-tenth '(1 2 3 4 5 6 7 8 9 10 11))))
  (should-error (cl-tenth "1234567890") :type 'wrong-type-argument))

(ert-deftest cl-lib-test-mapcar ()
  (should (equal (cl-mapcar #'1+ '(1 2 3)) '(2 3 4)))
  (should (equal (cl-mapcar #'+ '(1 2 3) '(4 5 6)) '(5 7 9)))
  (should (equal (cl-mapcar #'+ '(1 2 3) '(4 5)) '(5 7)))
  (should (equal (cl-mapcar #'+ '() '()) '()))
  (should-error (cl-mapcar #'+ 1 '(4 5 6)))
  (should-error (cl-mapcar #'+ '(1 2 3) 4)))

(ert-deftest cl-lib-test-list* ()
  (should (equal (cl-list* 'a) 'a))
  (should (equal (cl-list* 'a 'b) '(a . b)))
  (should (equal (cl-list* 'a 'b 'c 'd) '(a b c . d)))
  (should (equal (cl-list* 'a 'b '(c d)) '(a b c d))))

(ert-deftest cl-lib-test-copy-list ()
  (let ((original '(1 2 . 3))
        (result (cl-copy-list '(1 2 . 3))))
    (and (should (equal original result))
         (not (eq original result)))))

(ert-deftest cl-lib-test-subst ()
  (should (equal (cl-subst 'x 'a '(a b c)) '(x b c)))
  (should (equal (cl-subst 'x 'a '(a b a c)) '(x b x c)))
  (should (equal (cl-subst 'x 'a '(b c d)) '(b c d)))
  (should (equal (cl-subst 'x 'a '(a b (a c) d)) '(x b (x c) d)))
  (should (equal (cl-subst "a" "A" '("a" "b" "c" "a") :test #'equal) '("a" "b" "c" "a"))))

(ert-deftest cl-lib-test-acons ()
  (should (equal (cl-acons 'key 'value '()) '((key . value))))
  (should (equal (cl-acons 'key 'value '((a . 1) (b . 2))) '((key . value) (a . 1) (b . 2))))
  (should (equal (cl-acons 'a 1 '((a . 1) (b . 2))) '((a . 1) (a . 1) (b . 2))))
  (should (equal (cl-acons nil 'value '((a . 1) (b . 2))) '((nil . value) (a . 1) (b . 2))))
  (should (equal (cl-acons 'key nil '((a . 1) (b . 2))) '((key . nil) (a . 1) (b . 2)))))

(ert-deftest cl-lib-test-pairlis ()
  (should (equal (cl-pairlis '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3))))
  (should (equal (cl-pairlis '(a b c d) '(1 2 3)) '((a . 1) (b . 2) (c . 3))))
  (should (equal (cl-pairlis '(a b c) '(1 2 3 4)) '((a . 1) (b . 2) (c . 3))))
  (should (equal (cl-pairlis '(a b c) '(1 2 3) '((d . 4) (e . 5))) '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))))
  (should (equal (cl-pairlis '() '(1 2 3)) '()))
  (should (equal (cl-pairlis '(a b c) '()) '()))
  (should (equal (cl-pairlis '(a nil c) '(1 2 3)) '((a . 1) (nil . 2) (c . 3))))
  (should (equal (cl-pairlis '(a b c) '(1 nil 3)) '((a . 1) (b) (c . 3)))))

(ert-deftest cl-lib-test-nth-value ()
  (let ((vals (cl-values 2 3)))
    (should (= (cl-nth-value 0 vals) 2))
    (should (= (cl-nth-value 1 vals) 3))
    (should (null (cl-nth-value 2 vals)))
    (should-error (cl-nth-value 0.0 vals) :type 'wrong-type-argument)))

(ert-deftest cl-lib-nth-value-test-multiple-values ()
  "While CL multiple values are an alias to list, these won't work."
  :expected-result :failed
  (should (equal (cl-nth-value 0 '(2 3)) '(2 3)))
  (should (= (cl-nth-value 0 1) 1))
  (should (null (cl-nth-value 1 1)))
  (should-error (cl-nth-value -1 (cl-values 2 3)) :type 'args-out-of-range)
  (should (string= (cl-nth-value 0 "only lists") "only lists")))

(ert-deftest cl-test-ldiff ()
  (let ((l '(1 2 3)))
    (should (null (cl-ldiff '() '())))
    (should (null (cl-ldiff '() l)))
    (should (null (cl-ldiff l l)))
    (should (equal l (cl-ldiff l '())))
    ;; must be part of the list
    (should (equal l (cl-ldiff l (list 2 3))))
    (should (equal '(1) (cl-ldiff l (nthcdr 1 l))))
    ;; should return a copy
    (should-not (eq (cl-ldiff l '()) l))))

(ert-deftest cl-lib-adjoin-test ()
  (let ((nums '(1 2))
        (myfn-p '=))
    ;; add non-existing item to the front
    (should (equal '(3 1 2) (cl-adjoin 3 nums)))
    ;; just add - don't copy rest
    (should (eq nums (cdr (cl-adjoin 3 nums))))
    ;; add only when not already there
    (should (eq nums (cl-adjoin 2 nums)))
    (with-suppressed-warnings ((suspicious memql))
      (should (equal '(2 1 (2)) (cl-adjoin 2 '(1 (2))))))
    ;; default test function is eql
    (should (equal '(1.0 1 2) (cl-adjoin 1.0 nums)))
    ;; own :test function - returns true if match
    (should (equal '(1.0 1 2) (cl-adjoin 1.0 nums :test nil))) ;defaults to eql
    (should (eq nums (cl-adjoin 2 nums :test myfn-p))) ;match
    (should (equal '(3 1 2) (cl-adjoin 3 nums :test myfn-p))) ;no match
    ;; own :test-not function - returns false if match
    (should (equal '(1.0 1 2) (cl-adjoin 1.0 nums :test-not nil))) ;defaults to eql
    (should (equal '(2 2) (cl-adjoin 2 '(2) :test-not myfn-p))) ; no match
    (should (eq nums (cl-adjoin 2 nums :test-not myfn-p))) ; 1 matches
    (should (eq nums (cl-adjoin 3 nums :test-not myfn-p))) ; 1 and 2 matches

    ;; according to CLtL2 passing both :test and :test-not should signal error
    ;;(should-error (cl-adjoin 3 nums :test 'myfn-p :test-not myfn-p))

    ;; own :key fn
    (should (eq nums (cl-adjoin 3 nums :key (lambda (x) (if (evenp x) (1+ x) x)))))
    (should (equal '(3 1 2) (cl-adjoin 3 nums :key (lambda (x) (if (evenp x) (+ 2 x) x)))))

    ;; convert using :key, then compare with :test
    (should (eq nums (cl-adjoin 1 nums :key 'int-to-string :test 'string=)))
    (should (equal '(3 1 2) (cl-adjoin 3 nums :key 'int-to-string :test 'string=)))
    (should-error (cl-adjoin 3 nums :key 'int-to-string :test myfn-p)
                  :type 'wrong-type-argument)

    ;; convert using :key, then compare with :test-not
    (should (eq nums (cl-adjoin 3 nums :key 'int-to-string :test-not 'string=)))
    (should (equal '(1 1) (cl-adjoin 1 '(1) :key 'int-to-string :test-not 'string=)))
    (should-error (cl-adjoin 1 nums :key 'int-to-string :test-not myfn-p)
                  :type 'wrong-type-argument)))

(ert-deftest old-struct ()
  (cl-defstruct foo x)
  (with-suppressed-warnings ((obsolete cl-old-struct-compat-mode))
    (let ((x (vector 'cl-struct-foo))
          (saved cl-old-struct-compat-mode))
      (cl-old-struct-compat-mode -1)
      (should (eq (type-of x) 'vector))

      (cl-old-struct-compat-mode 1)
      (defvar cl-struct-foo)
      (let ((cl-struct-foo (cl--struct-get-class 'foo)))
        (setf (symbol-function 'cl-struct-foo) :quick-object-witness-check)
        (should (eq (type-of x) 'foo))
        (should (eq (type-of (vector 'foo)) 'vector)))

      (cl-old-struct-compat-mode (if saved 1 -1)))))

(ert-deftest cl-lib-old-struct ()
  (with-suppressed-warnings ((obsolete cl-old-struct-compat-mode))
    (let ((saved cl-old-struct-compat-mode))
      (cl-old-struct-compat-mode -1)
      (cl-struct-define 'foo "" 'cl-structure-object nil nil nil
                        'cl-struct-foo-tags 'cl-struct-foo t)
      (should cl-old-struct-compat-mode)
      (cl-old-struct-compat-mode (if saved 1 -1)))))

(ert-deftest cl-constantly ()
  (should (equal (mapcar (cl-constantly 3) '(a b c d))
                 '(3 3 3 3))))

;;; cl-lib-tests.el ends here
