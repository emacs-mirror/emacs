;;; cl-seq-tests.el --- Tests for cl-seq.el functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Nicolas Richard <youngfrog@members.fsf.org>

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

(require 'ert)
(require 'cl-seq)

(ert-deftest cl-union-test-00 ()
  "Test for https://debbugs.gnu.org/22729 ."
  (let ((str1 "foo")
        (str2 (make-string 3 ?o)))
    ;; Emacs may make two string literals eql when reading.
    (aset str2 0 ?f)
    (should (not (eql str1 str2)))
    (should (equal str1 str2))
    (should (equal (cl-union (list str1) (list str2))
                   (list str2)))
    (should (equal (cl-union (list str1) (list str2) :test 'eql)
                   (list str1 str2)))))

(defvar cl-seq--test-list nil
  "List used on `cl-seq' tests with side effects.")
(defvar cl-seq--test-list2 nil
  "List used on `cl-seq' tests with side effects.")

(defmacro cl-seq--with-side-effects (list list2 &rest body)
  "Run a test with side effects on lists; after the test restore the lists.
LIST is the value of `cl-seq--test-list' before the test.
LIST2, if non-nil, is the value of `cl-seq--test-list2' before the test.
Body are forms defining the test."
  (declare (indent 2) (debug t))
  (let ((orig (make-symbol "orig"))
        (orig2 (make-symbol "orig2")))
    `(let ((,orig (copy-sequence ,list))
           (,orig2 (copy-sequence ,list2)))
       (unwind-protect (progn ,@body)
         (setq cl-seq--test-list ,orig)
         (when ,list2
           (setq cl-seq--test-list2 ,orig2))))))

;; keywords supported:  :start :end
(ert-deftest cl-seq-fill-test ()
  (let* ((cl-seq--test-list '(1 2 3 4 5 2 6))
         (orig (copy-sequence cl-seq--test-list))
         (tests '((should (equal '(b b b b b b b) (cl-fill _list 'b)))
                  (should (equal '(1 2 3 4 b b b) (cl-fill _list 'b :start 4)))
                  (should (equal '(b b b b 5 2 6) (cl-fill _list 'b :end 4)))
                  (should (equal '(1 2 b b 5 2 6) (cl-fill _list 'b :start 2 :end 4)))
                  (should (equal orig (cl-fill _list 'b :end 0))))))
    (dolist (test tests)
      (let ((_list cl-seq--test-list))
        (cl-seq--with-side-effects orig nil
          test)))))

;; keywords supported:  :start1 :end1 :start2 :end2
(ert-deftest cl-seq-replace-test ()
  (let* ((cl-seq--test-list '(1 2 3 4 5 2 6))
         (cl-seq--test-list2 (make-list 6 'a))
         (orig  (copy-sequence cl-seq--test-list))
         (orig2 (copy-sequence cl-seq--test-list2))
         (tests '((should (equal '(a a a a a a 6) (cl-replace _list _list2)))
                  (should (equal '(a a a a a a 6) (cl-replace _list _list2 :start1 0)))
                  (should (equal '(a a a a a a 6) (cl-replace _list _list2 :start2 0)))
                  (should (equal orig (cl-replace _list _list2 :start1 (length _list))))
                  (should (equal orig (cl-replace _list _list2 :start2 (length _list2))))
                  (should (equal orig (cl-replace _list _list2 :end1 0)))
                  (should (equal orig (cl-replace _list _list2 :end2 0)))
                  (should (equal '(1 2 3 4 a a a) (cl-replace _list _list2 :start1 4)))
                  (should (equal '(a a a a 5 2 6) (cl-replace _list _list2 :end1 4)))
                  (should (equal '(a a 3 4 5 2 6) (cl-replace _list _list2 :start2 4)))
                  (should (equal '(a a a a 5 2 6) (cl-replace _list _list2 :end2 4)))
                  (should (equal '(1 2 a a 5 2 6) (cl-replace _list _list2 :start1 2 :end1 4)))
                  (should (equal '(a a 3 4 5 2 6) (cl-replace _list _list2 :start2 2 :end2 4))))))
    (dolist (test tests)
      (let ((_list cl-seq--test-list)
            (_list2 cl-seq--test-list2))
        (cl-seq--with-side-effects orig orig2
          test)))))

;; keywords supported:  :test :test-not :key :count :start :end :from-end
(ert-deftest cl-seq-remove-test ()
  (let ((list '(1 2 3 4 5 2 6)))
    (should (equal list (cl-remove 'foo list)))
    (should (equal '(1 3 4 5 6) (cl-remove 2 list)))
    (should (equal '(1 3 4 5 6) (cl-remove 2 list
                                           :key #'identity
                                           :test (lambda (a b) (eql a b)))))
    (should (equal '(1 2 3 4 2)   (cl-remove 4 list :test (lambda (a b) (> b a)))))
    (should (equal '(5 6)         (cl-remove 4 list :test-not (lambda (a b) (> b a)))))
    (should (equal '(1 3 5)       (cl-remove 'foo list :if #'cl-evenp)))
    (should (equal '(2 4 2 6)     (cl-remove 'foo list :if-not #'cl-evenp)))
    (should (equal '(1 2 3 4 5)   (cl-remove 'foo list :if #'cl-evenp :start 4)))
    (should (equal '(1 2 3 4 5 6) (cl-remove 2 list :start 5 :end 6)))
    (should (equal '(1 3 4 5 2 6) (cl-remove 2 list :count 1)))
    (should (equal '(1 3 4 5 2 6) (cl-remove 2 list :from-end nil :count 1)))
    (should (equal '(1 2 3 4 5 6) (cl-remove 2 list :from-end t :count 1)))))

;; keywords supported:  :test :test-not :key :count :start :end :from-end
(ert-deftest cl-seq-delete-test ()
  (let* ((cl-seq--test-list '(1 2 3 4 5 2 6))
         (orig (copy-sequence cl-seq--test-list))
         (tests '((should (equal orig         (cl-delete 'foo _list)))
                  (should (equal '(1 3 4 5 6) (cl-delete 2 _list)))
                  (should (equal '(1 3 4 5 6) (cl-delete 2 _list
                                                         :key #'identity
                                                         :test (lambda (a b) (eql a b)))))
                  (should (equal '(1 2 3 4 2)   (cl-delete 4 _list :test (lambda (a b) (> b a)))))
                  (should (equal '(5 6)         (cl-delete 4 _list :test-not (lambda (a b) (> b a)))))
                  (should (equal '(1 3 5)       (cl-delete 'foo _list :if #'cl-evenp)))
                  (should (equal '(2 4 2 6)     (cl-delete 'foo _list :if-not #'cl-evenp)))
                  (should (equal '(1 2 3 4 5)   (cl-delete 'foo _list :if #'cl-evenp :start 4)))
                  (should (equal '(1 2 3 4 5 6) (cl-delete 2 _list :start 5 :end 6)))
                  (should (equal '(1 3 4 5 2 6) (cl-delete 2 _list :count 1)))
                  (should (equal '(1 3 4 5 2 6) (cl-delete 2 _list :from-end nil :count 1)))
                  (should (equal '(1 2 3 4 5 6) (cl-delete 2 _list :from-end t :count 1))))))
    (dolist (test tests)
      (let ((_list cl-seq--test-list))
        (cl-seq--with-side-effects orig nil
          test)))))

;; keywords supported:  :test :test-not :key :start :end :from-end
(ert-deftest cl-seq-remove-duplicates-test ()
  (let ((list '(1 2 3 4 5 2 6)))
    (should (equal '(1 3 4 5 2 6) (cl-remove-duplicates list)))
    (should (equal '(1 2 3 4 5 6) (cl-remove-duplicates list :from-end t)))
    (should (equal list           (cl-remove-duplicates list :start 2)))
    (should (equal list           (cl-remove-duplicates list :start 2 :from-end t)))
    (should (equal list           (cl-remove-duplicates list :end 4)))
    (should (equal '(6)           (cl-remove-duplicates list :test (lambda (a b) (< a b)))))
    (should (equal '(1 2 6)       (cl-remove-duplicates list :test (lambda (a b) (>= a b)))))
    (should (equal (cl-remove-duplicates list :test (lambda (a b) (>= a b)))
                   (cl-remove-duplicates list :test-not (lambda (a b) (< a b)))))
    (should (equal (cl-remove-duplicates list)
                   (cl-remove-duplicates list :key #'number-to-string :test #'string=)))
    (should (equal list
                   (cl-remove-duplicates list :key #'number-to-string :test #'eq)))))

;; keywords supported:  :test :test-not :key :count :start :end :from-end
(ert-deftest cl-seq-substitute-test ()
  (let ((list '(1 2 3 4 5 2 6)))
    (should (equal '(1 b 3 4 5 b 6) (cl-substitute 'b 2 list)))
    (should (equal list (cl-substitute 'b 2 list :start (length list))))
    (should (equal list (cl-substitute 'b 2 list :end 0)))
    (should (equal '(1 2 3 4 5 b 6) (cl-substitute 'b 2 list :start 2)))
    (should (equal '(1 b 3 4 5 2 6) (cl-substitute 'b 2 list :end 2)))
    (should (equal list (cl-substitute 'b 2 list :start 2 :end 4)))
    (should (equal '(1 b 3 4 5 2 6) (cl-substitute 'b 2 list :count 1)))
    (should (equal '(1 2 3 4 5 b 6) (cl-substitute 'b 2 list :count 1 :from-end t)))
    (should (equal list             (cl-substitute 'b 2 list :count -1)))
    (should (equal '(1 b 3 4 5 b 6) (cl-substitute 'b "2" list :key #'number-to-string
                                                   :test #'string=)))
    (should (equal (cl-substitute 'b 2 list)
                   (cl-substitute 'b 2 list :test #'eq)))
    (should (equal '(1 2 b b b 2 b) (cl-substitute 'b 2 list :test (lambda (a b) (< a b)))))
    (should (equal '(b b 3 4 5 b 6) (cl-substitute 'b 2 list :test (lambda (a b) (>= a b)))))
    (should (equal list             (cl-substitute 'b 99 list :test (lambda (a b) (< a b)))))
    (should (equal (cl-substitute 'b 2 list :test (lambda (a b) (>= a b)))
                   (cl-substitute 'b 2 list :test-not (lambda (a b) (< a b)))))
    (should (equal '(1 2 b b b 2 b) (cl-substitute 'b nil list :if (lambda (x) (> (cl-position x list) 1)))))
    (should (equal '(1 b b b b b b) (cl-substitute 'b nil list :if (lambda (x) (> (cl-position x list :from-end t) 1)))))

    (should (equal '(b b 3 4 5 b 6) (cl-substitute 'b nil list
                                                   :if-not (lambda (x) (> (cl-position x list) 1)))))
    (should (equal '(b 2 3 4 5 2 6) (cl-substitute 'b nil list
                                                   :if-not (lambda (x) (> (cl-position x list :from-end t) 1)))))))


;; keywords supported:  :test :test-not :key :count :start :end :from-end
(ert-deftest cl-seq-nsubstitute-test ()
  (let ((cl-seq--test-list '(1 2 3 4 5 2 6))
        (orig (copy-sequence cl-seq--test-list))
        (tests '((should (equal '(1 b 3 4 5 b 6) (cl-nsubstitute 'b 2 _list)))
                 (should (equal _list (cl-substitute 'b 2 _list :start (length _list))))
                 (should (equal _list (cl-substitute 'b 2 _list :end 0)))
                 (should (equal '(1 2 3 4 5 b 6) (cl-substitute 'b 2 _list :start 2)))
                 (should (equal '(1 b 3 4 5 2 6) (cl-substitute 'b 2 _list :end 2)))
                 (should (equal _list (cl-substitute 'b 2 _list :start 2 :end 4)))
                 (should (equal '(1 b 3 4 5 2 6) (cl-nsubstitute 'b 2 _list :count 1)))
                 (should (equal '(1 2 3 4 5 b 6) (cl-nsubstitute 'b 2 _list :count 1 :from-end t)))
                 (should (equal _list            (cl-nsubstitute 'b 2 _list :count -1)))
                 (should (equal '(1 b 3 4 5 b 6) (cl-nsubstitute 'b "2" _list :key #'number-to-string
                                                                 :test #'string=)))
                 (should (equal (cl-nsubstitute 'b 2 _list)
                                (cl-nsubstitute 'b 2 _list :test #'eq)))
                 (should (equal '(1 2 b b b 2 b) (cl-nsubstitute 'b 2 _list :test (lambda (a b) (< a b)))))
                 (should (equal '(b b 3 4 5 b 6) (cl-nsubstitute 'b 2 _list :test (lambda (a b) (>= a b)))))
                 (should (equal _list            (cl-nsubstitute 'b 99 _list :test (lambda (a b) (< a b)))))
                 (should (equal (cl-nsubstitute 'b 2 _list :test (lambda (a b) (>= a b)))
                                (cl-nsubstitute 'b 2 _list :test-not (lambda (a b) (< a b)))))
                 (should (equal '(1 2 b b b 2 b)
                                (cl-nsubstitute 'b nil _list :if (lambda (x) (> (cl-position x _list) 1)))))
                 (should (equal '(1 b b b b b b)
                                (cl-nsubstitute 'b nil _list :if (lambda (x) (> (cl-position x _list :from-end t) 1)))))
                 (should (equal '(b b 3 4 5 b 6)
                                (cl-nsubstitute 'b nil _list
                                                :if-not (lambda (x) (> (cl-position x _list) 1)))))
                 (should (equal '(b 2 3 4 5 2 6)
                                (cl-nsubstitute 'b nil _list
                                                :if-not (lambda (x) (> (cl-position x _list :from-end t) 1))))))))
    (dolist (test tests)
      (let ((_list cl-seq--test-list))
        (cl-seq--with-side-effects orig nil
                                   test)))))

;; keywords supported:  :test :test-not :key :start :end :from-end
(ert-deftest cl-seq-position-test ()
  (let ((list '(1 2 3 4 5 2 6)))
    (should-not (cl-position 'foo list))
    (should (= 1 (cl-position 2 list)))
    (should (= 5 (cl-position 2 list :start 5 :end 6)))
    (should (= 1 (cl-position 2 list :from-end nil)))
    (should (= 5 (cl-position 2 list :from-end t)))
    (should (cl-position 2 list :key #'identity
                         :test (lambda (a b) (eql a b))))
    (should (= 1 (cl-position "2" list :key #'number-to-string :test #'string=)))
    (should (= 5 (cl-position "2" list :key #'number-to-string :test #'string= :from-end t)))
    (should-not (cl-position "2" list :key #'number-to-string))
    (should (cl-position 5 list :key (lambda (x) (1+ (* 1.0 x x))) :test #'=))
    (should-not (cl-position 5 list :key (lambda (x) (1+ (* 1.0 x x)))))
    (should (= 1 (cl-position 5 list :key (lambda (x) (1+ (* x x))))))
    (should (= 5 (cl-position 5 list :key (lambda (x) (1+ (* x x))) :from-end t)))))

;; keywords supported:  :test :test-not :key :start :end
(ert-deftest cl-seq-count-test ()
  (let ((list '(1 2 3 4 5 2 6)))
    (should (= 2 (cl-count 2 list)))
    (should (= 1 (cl-count 2 list :start 2)))
    (should (= 1 (cl-count 2 list :end 4)))
    (should (= 0 (cl-count -5 list)))
    (should (= 0 (cl-count 2 list :start 2 :end 4)))
    (should (= 4 (cl-count 'foo list :key (lambda (x) (and (cl-evenp x) 'foo)))))
    (should (= 4 (cl-count 'foo list :test (lambda (_a b) (cl-evenp b)))))
    (should (equal (cl-count 'foo list :test (lambda (_a b) (cl-oddp b)))
                   (cl-count 'foo list :test-not (lambda (_a b) (cl-evenp b)))))))

;; keywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
(ert-deftest cl-seq-mismatch-test ()
  (let ((list '(1 2 3 4 5 2 6))
        (list2 '(1 999 2 3 4 5 2 6)))
    (should-not (cl-mismatch list list))
    (should-not (cl-mismatch list (remove 999 list2)))
    (should (= 0 (cl-mismatch list list :key #'number-to-string)))
    (should-not (cl-mismatch list list :key #'number-to-string :test #'string=))
    (should (= 1 (cl-mismatch list list2)))
    (should (= 0 (cl-mismatch list list2 :from-end t)))
    (should (= 3 (cl-mismatch '(1 2 3) list)))
    (should-not (cl-mismatch list list2 :end1 1 :end2 1))
    (should-not (cl-mismatch list list2 :start1 1 :start2 2))
    (should (= 1 (cl-mismatch list list2 :start1 1 :end1 2 :start2 4 :end2 4)))
    (should (= -1 (cl-mismatch list list2 :key #'number-to-string
                               :test (lambda (a b)
                                       (and (stringp a) (stringp b))) :from-end t)))
    (should (= 7 (cl-mismatch list list2 :key #'number-to-string
                              :test (lambda (a b)
                                      (and (stringp a) (stringp b))))))))

;; keywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
(ert-deftest cl-seq-search-test ()
  (let ((list '(1 2 3 4 5 2 6))
        (list2 '(1 999 2 3 4 5 2 6)))
    (should-not (cl-search list list2))
    (should (= 2 (cl-search list list2 :start1 1 :start2 2)))
    (should (= 4 (cl-search list list2 :start1 3)))
    (should (= 6 (cl-search list list2 :start1 5)))
    (should (= 0 (cl-search list list2 :end1 1)))
    (should (= 0 (cl-search nil list2)))
    (should (= 2 (cl-search list list2 :start1 1 :end1 2 :end2 3)))
    (should (= 0 (cl-search list list2 :test (lambda (a b) (and (numberp a) (numberp b))))))
    (should (= 0 (cl-search list list2 :key (lambda (x) (and (numberp x) 'foo))
                            :test (lambda (a b) (and (eq a 'foo) (eq b 'foo))))))
    (should (= 1 (cl-search (nthcdr 2 list) (nthcdr 2 list2))))
    (should (= 3 (cl-search (nthcdr 2 list) list2)))))

(ert-deftest cl-seq-test-bug24264 ()
  "Test for https://debbugs.gnu.org/24264 ."
  (let ((list  (append (make-list 8000005 1) '(8)))
        (list2 (make-list 8000005 2)))
    (should (cl-position 8 list))
    (should-not (equal '(8) (last (cl-remove 8 list))))
    (should (equal '(2 8) (last (cl-substitute 2 1 list) 2)))
    (should (equal '(2 8) (last (cl-replace list list2) 2)))
    (should (equal '(1 1) (last (cl-fill list 1) 2)))))


(provide 'cl-seq-tests)
;;; cl-seq-tests.el ends here
