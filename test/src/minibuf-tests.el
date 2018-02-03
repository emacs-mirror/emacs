;;; minibuf-tests.el --- tests for minibuf.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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

(require 'ert)


;;; Support functions for `try-completion', `all-completion', and
;;; `test-completion' tests.

(defun minibuf-tests--strings-to-symbol-list (list)
  (mapcar #'intern list))
(defun minibuf-tests--strings-to-symbol-alist (list)
  (let ((num 0))
    (mapcar (lambda (str) (cons (intern str) (cl-incf num))) list)))
(defun minibuf-tests--strings-to-string-alist (list)
  (let ((num 0))
    (mapcar (lambda (str) (cons str (cl-incf num))) list)))
(defun minibuf-tests--strings-to-obarray (list)
  (let ((ob (make-vector 7 0)))
    (mapc (lambda (str) (intern str ob)) list)
    ob))
(defun minibuf-tests--strings-to-string-hashtable (list)
  (let ((ht (make-hash-table :test #'equal))
        (num 0))
    (mapc (lambda (str) (puthash str (cl-incf num) ht)) list)
    ht))
(defun minibuf-tests--strings-to-symbol-hashtable (list)
  (let ((ht (make-hash-table :test #'equal))
        (num 0))
    (mapc (lambda (str) (puthash (intern str) (cl-incf num) ht)) list)
    ht))

;;; Functions that produce a predicate (for *-completion functions)
;;; which always returns non-nil for a given collection.

(defun minibuf-tests--memq-of-collection (collection)
  (lambda (elt) (memq elt collection)))
(defun minibuf-tests--part-of-obarray (ob)
  (lambda (sym) (eq (intern-soft (symbol-name sym) ob) sym)))
(defun minibuf-tests--part-of-hashtable (table)
  (lambda (k v) (equal (gethash k table) v)))


;;; Testing functions that are agnostic to type of COLLECTION.

(defun minibuf-tests--try-completion (xform-collection)
  (let* ((abcdef (funcall xform-collection '("abc" "def")))
         (+abba  (funcall xform-collection '("abc" "abba" "def"))))
    (should (equal (try-completion "a" abcdef) "abc"))
    (should (equal (try-completion "a" +abba) "ab"))
    (should (equal (try-completion "abc" +abba) t))
    (should (equal (try-completion "abcd" +abba) nil))))

(defun minibuf-tests--try-completion-pred (xform-collection collection-member)
  (let* ((abcdef (funcall xform-collection '("abc" "def")))
         (abcdef-member (funcall collection-member abcdef))
         (+abba  (funcall xform-collection '("abc" "abba" "def")))
         (+abba-member (funcall collection-member +abba)))
    (should (equal (try-completion "a" abcdef abcdef-member) "abc"))
    (should (equal (try-completion "a" +abba +abba-member) "ab"))
    (should (equal (try-completion "abc" +abba +abba-member) t))
    (should (equal (try-completion "abcd" +abba +abba-member) nil))
    (should-not (try-completion "a" abcdef #'ignore))
    (should-not (try-completion "a" +abba #'ignore))
    (should-not (try-completion "abc" +abba #'ignore))
    (should-not (try-completion "abcd" +abba #'ignore))))

(defun minibuf-tests--try-completion-regexp (xform-collection)
  (let ((abcdef (funcall xform-collection '("abc" "def")))
        (+abba  (funcall xform-collection '("abc" "abba" "def"))))
    (let ((completion-regexp-list '(".")))
      (should (equal (try-completion "a" abcdef) "abc"))
      (should (equal (try-completion "a" +abba) "ab"))
      (should (equal (try-completion "abc" +abba) t))
      (should (equal (try-completion "abcd" +abba) nil)))
    (let ((completion-regexp-list '("X")))
      (should-not (try-completion "a" abcdef))
      (should-not (try-completion "a" +abba))
      (should-not (try-completion "abc" +abba))
      (should-not (try-completion "abcd" +abba)))))

(defun minibuf-tests--all-completions (xform-collection)
  (let* ((abcdef (funcall xform-collection '("abc" "def")))
         (+abba  (funcall xform-collection '("abc" "abba" "def"))))
    (should (equal (all-completions "a" abcdef) '("abc")))
    (should (equal (all-completions "a" +abba) '("abc" "abba")))
    (should (equal (all-completions "abc" +abba) '("abc")))
    (should (equal (all-completions "abcd" +abba) nil))))

(defun minibuf-tests--all-completions-pred (xform-collection collection-member)
  (let* ((abcdef (funcall xform-collection '("abc" "def")))
         (abcdef-member (funcall collection-member abcdef))
         (+abba  (funcall xform-collection '("abc" "abba" "def")))
         (+abba-member (funcall collection-member +abba)))
    (should (equal (all-completions "a" abcdef abcdef-member) '("abc")))
    (should (equal (all-completions "a" +abba +abba-member) '("abc" "abba")))
    (should (equal (all-completions "abc" +abba +abba-member) '("abc")))
    (should (equal (all-completions "abcd" +abba +abba-member) nil))
    (should-not (all-completions "a" abcdef #'ignore))
    (should-not (all-completions "a" +abba #'ignore))
    (should-not (all-completions "abc" +abba #'ignore))
    (should-not (all-completions "abcd" +abba #'ignore))))

(defun minibuf-tests--all-completions-regexp (xform-collection)
  (let ((abcdef (funcall xform-collection '("abc" "def")))
        (+abba  (funcall xform-collection '("abc" "abba" "def"))))
    (let ((completion-regexp-list '(".")))
      (should (equal (all-completions "a" abcdef) '("abc")))
      (should (equal (all-completions "a" +abba) '("abc" "abba")))
      (should (equal (all-completions "abc" +abba) '("abc")))
      (should (equal (all-completions "abcd" +abba) nil)))
    (let ((completion-regexp-list '("X")))
      (should-not (all-completions "a" abcdef))
      (should-not (all-completions "a" +abba))
      (should-not (all-completions "abc" +abba))
      (should-not (all-completions "abcd" +abba)))))

(defun minibuf-tests--test-completion (xform-collection)
  (let* ((abcdef (funcall xform-collection '("abc" "def")))
         (+abba  (funcall xform-collection '("abc" "abba" "def"))))
    (should (test-completion "abc" abcdef))
    (should (test-completion "def" +abba))
    (should (test-completion "abba" +abba))
    (should-not (test-completion "abcd" +abba))))

(defun minibuf-tests--test-completion-pred (xform-collection collection-member)
  (let* ((abcdef (funcall xform-collection '("abc" "def")))
         (abcdef-member (funcall collection-member abcdef))
         (+abba  (funcall xform-collection '("abc" "abba" "def")))
         (+abba-member (funcall collection-member +abba)))
    (should (test-completion "abc" abcdef abcdef-member))
    (should (test-completion "def" +abba +abba-member))
    (should (test-completion "abba" +abba +abba-member))
    (should-not (test-completion "abcd" +abba +abba-member))
    (should-not (test-completion "abc" abcdef #'ignore))
    (should-not (test-completion "def" +abba #'ignore))
    (should-not (test-completion "abba" +abba #'ignore))
    (should-not (test-completion "abcd" +abba #'ignore))))

(defun minibuf-tests--test-completion-regexp (xform-collection)
  (let ((abcdef (funcall xform-collection '("abc" "def")))
        (+abba  (funcall xform-collection '("abc" "abba" "def"))))
    (let ((completion-regexp-list '(".")))
      (should (test-completion "abc" abcdef))
      (should (test-completion "def" +abba))
      (should (test-completion "abba" +abba))
      (should-not (test-completion "abcd" +abba)))
    (let ((completion-regexp-list '("X")))
      (should-not (test-completion "abc" abcdef))
      (should-not (test-completion "def" +abba))
      (should-not (test-completion "abba" +abba))
      (should-not (test-completion "abcd" +abba)))))


;;; Tests for `try-completion'.
(ert-deftest try-completion-string-list ()
  (minibuf-tests--try-completion #'identity))
(ert-deftest try-completion-string-list-predicate ()
  (minibuf-tests--try-completion-pred
   #'identity #'minibuf-tests--memq-of-collection))
(ert-deftest try-completion-string-list-completion-regexp ()
  (minibuf-tests--try-completion-regexp #'identity))

(ert-deftest try-completion-symbol-list ()
  (minibuf-tests--try-completion
   #'minibuf-tests--strings-to-symbol-list))
(ert-deftest try-completion-symbol-list-predicate ()
  (minibuf-tests--try-completion-pred
   #'minibuf-tests--strings-to-symbol-list
   #'minibuf-tests--memq-of-collection))
(ert-deftest try-completion-symbol-list-completion-regexp ()
  (minibuf-tests--try-completion-regexp
   #'minibuf-tests--strings-to-symbol-list))

(ert-deftest try-completion-symbol-alist ()
  (minibuf-tests--try-completion
   #'minibuf-tests--strings-to-symbol-alist))
(ert-deftest try-completion-symbol-alist-predicate ()
  (minibuf-tests--try-completion-pred
   #'minibuf-tests--strings-to-symbol-alist
   #'minibuf-tests--memq-of-collection))
(ert-deftest try-completion-symbol-alist-completion-regexp ()
  (minibuf-tests--try-completion-regexp
   #'minibuf-tests--strings-to-symbol-alist))

(ert-deftest try-completion-string-alist ()
  (minibuf-tests--try-completion
   #'minibuf-tests--strings-to-string-alist))
(ert-deftest try-completion-string-alist-predicate ()
  (minibuf-tests--try-completion-pred
   #'minibuf-tests--strings-to-string-alist
   #'minibuf-tests--memq-of-collection))
(ert-deftest try-completion-string-alist-completion-regexp ()
  (minibuf-tests--try-completion-regexp
   #'minibuf-tests--strings-to-string-alist))

(ert-deftest try-completion-obarray ()
  (minibuf-tests--try-completion
   #'minibuf-tests--strings-to-obarray))
(ert-deftest try-completion-obarray-predicate ()
  (minibuf-tests--try-completion-pred
   #'minibuf-tests--strings-to-obarray
   #'minibuf-tests--part-of-obarray))
(ert-deftest try-completion-obarray-completion-regexp ()
  (minibuf-tests--try-completion-regexp
   #'minibuf-tests--strings-to-obarray))

(ert-deftest try-completion-string-hashtable ()
  (minibuf-tests--try-completion
   #'minibuf-tests--strings-to-string-hashtable))
(ert-deftest try-completion-string-hashtable-predicate ()
  (minibuf-tests--try-completion-pred
   #'minibuf-tests--strings-to-string-hashtable
   #'minibuf-tests--part-of-hashtable))
(ert-deftest try-completion-string-hashtable-completion-regexp ()
  (minibuf-tests--try-completion-regexp
   #'minibuf-tests--strings-to-string-hashtable))

(ert-deftest try-completion-symbol-hashtable ()
  (minibuf-tests--try-completion
   #'minibuf-tests--strings-to-symbol-hashtable))
(ert-deftest try-completion-symbol-hashtable-predicate ()
  (minibuf-tests--try-completion-pred
   #'minibuf-tests--strings-to-symbol-hashtable
   #'minibuf-tests--part-of-hashtable))
(ert-deftest try-completion-symbol-hashtable-completion-regexp ()
  (minibuf-tests--try-completion-regexp
   #'minibuf-tests--strings-to-symbol-hashtable))


;;; Tests for `all-completions'.

(ert-deftest all-completions-string-list ()
  (minibuf-tests--all-completions #'identity))
(ert-deftest all-completions-string-list-predicate ()
  (minibuf-tests--all-completions-pred
   #'identity #'minibuf-tests--memq-of-collection))
(ert-deftest all-completions-string-list-completion-regexp ()
  (minibuf-tests--all-completions-regexp #'identity))

(ert-deftest all-completions-symbol-list ()
  (minibuf-tests--all-completions
   #'minibuf-tests--strings-to-symbol-list))
(ert-deftest all-completions-symbol-list-predicate ()
  (minibuf-tests--all-completions-pred
   #'minibuf-tests--strings-to-symbol-list
   #'minibuf-tests--memq-of-collection))
(ert-deftest all-completions-symbol-list-completion-regexp ()
  (minibuf-tests--all-completions-regexp
   #'minibuf-tests--strings-to-symbol-list))

(ert-deftest all-completions-symbol-alist ()
  (minibuf-tests--all-completions
   #'minibuf-tests--strings-to-symbol-alist))
(ert-deftest all-completions-symbol-alist-predicate ()
  (minibuf-tests--all-completions-pred
   #'minibuf-tests--strings-to-symbol-alist
   #'minibuf-tests--memq-of-collection))
(ert-deftest all-completions-symbol-alist-completion-regexp ()
  (minibuf-tests--all-completions-regexp
   #'minibuf-tests--strings-to-symbol-alist))

(ert-deftest all-completions-string-alist ()
  (minibuf-tests--all-completions
   #'minibuf-tests--strings-to-string-alist))
(ert-deftest all-completions-string-alist-predicate ()
  (minibuf-tests--all-completions-pred
   #'minibuf-tests--strings-to-string-alist
   #'minibuf-tests--memq-of-collection))
(ert-deftest all-completions-string-alist-completion-regexp ()
  (minibuf-tests--all-completions-regexp
   #'minibuf-tests--strings-to-string-alist))

(ert-deftest all-completions-obarray ()
  (minibuf-tests--all-completions
   #'minibuf-tests--strings-to-obarray))
(ert-deftest all-completions-obarray-predicate ()
  (minibuf-tests--all-completions-pred
   #'minibuf-tests--strings-to-obarray
   #'minibuf-tests--part-of-obarray))
(ert-deftest all-completions-obarray-completion-regexp ()
  (minibuf-tests--all-completions-regexp
   #'minibuf-tests--strings-to-obarray))

(ert-deftest all-completions-string-hashtable ()
  (minibuf-tests--all-completions
   #'minibuf-tests--strings-to-string-hashtable))
(ert-deftest all-completions-string-hashtable-predicate ()
  (minibuf-tests--all-completions-pred
   #'minibuf-tests--strings-to-string-hashtable
   #'minibuf-tests--part-of-hashtable))
(ert-deftest all-completions-string-hashtable-completion-regexp ()
  (minibuf-tests--all-completions-regexp
   #'minibuf-tests--strings-to-string-hashtable))

(ert-deftest all-completions-symbol-hashtable ()
  (minibuf-tests--all-completions
   #'minibuf-tests--strings-to-symbol-hashtable))
(ert-deftest all-completions-symbol-hashtable-predicate ()
  (minibuf-tests--all-completions-pred
   #'minibuf-tests--strings-to-symbol-hashtable
   #'minibuf-tests--part-of-hashtable))
(ert-deftest all-completions-symbol-hashtable-completion-regexp ()
  (minibuf-tests--all-completions-regexp
   #'minibuf-tests--strings-to-symbol-hashtable))


;;; Tests for `test-completion'.

(ert-deftest test-completion-string-list ()
  (minibuf-tests--test-completion #'identity))
(ert-deftest test-completion-string-list-predicate ()
  (minibuf-tests--test-completion-pred
   #'identity #'minibuf-tests--memq-of-collection))
(ert-deftest test-completion-string-list-completion-regexp ()
  (minibuf-tests--test-completion-regexp #'identity))

(ert-deftest test-completion-symbol-list ()
  (minibuf-tests--test-completion
   #'minibuf-tests--strings-to-symbol-list))
(ert-deftest test-completion-symbol-list-predicate ()
  (minibuf-tests--test-completion-pred
   #'minibuf-tests--strings-to-symbol-list
   #'minibuf-tests--memq-of-collection))
(ert-deftest test-completion-symbol-list-completion-regexp ()
  (minibuf-tests--test-completion-regexp
   #'minibuf-tests--strings-to-symbol-list))

(ert-deftest test-completion-symbol-alist ()
  (minibuf-tests--test-completion
   #'minibuf-tests--strings-to-symbol-alist))
(ert-deftest test-completion-symbol-alist-predicate ()
  (minibuf-tests--test-completion-pred
   #'minibuf-tests--strings-to-symbol-alist
   #'minibuf-tests--memq-of-collection))
(ert-deftest test-completion-symbol-alist-completion-regexp ()
  (minibuf-tests--test-completion-regexp
   #'minibuf-tests--strings-to-symbol-alist))

(ert-deftest test-completion-string-alist ()
  (minibuf-tests--test-completion
   #'minibuf-tests--strings-to-string-alist))
(ert-deftest test-completion-string-alist-predicate ()
  (minibuf-tests--test-completion-pred
   #'minibuf-tests--strings-to-string-alist
   #'minibuf-tests--memq-of-collection))
(ert-deftest test-completion-string-alist-completion-regexp ()
  (minibuf-tests--test-completion-regexp
   #'minibuf-tests--strings-to-string-alist))

(ert-deftest test-completion-obarray ()
  (minibuf-tests--test-completion
   #'minibuf-tests--strings-to-obarray))
(ert-deftest test-completion-obarray-predicate ()
  (minibuf-tests--test-completion-pred
   #'minibuf-tests--strings-to-obarray
   #'minibuf-tests--part-of-obarray))
(ert-deftest test-completion-obarray-completion-regexp ()
  (minibuf-tests--test-completion-regexp
   #'minibuf-tests--strings-to-obarray))

(ert-deftest test-completion-string-hashtable ()
  (minibuf-tests--test-completion
   #'minibuf-tests--strings-to-string-hashtable))
(ert-deftest test-completion-string-hashtable-predicate ()
  (minibuf-tests--test-completion-pred
   #'minibuf-tests--strings-to-string-hashtable
   #'minibuf-tests--part-of-hashtable))
(ert-deftest test-completion-string-hashtable-completion-regexp ()
  (minibuf-tests--test-completion-regexp
   #'minibuf-tests--strings-to-string-hashtable))

(ert-deftest test-completion-symbol-hashtable ()
  (minibuf-tests--test-completion
   #'minibuf-tests--strings-to-symbol-hashtable))
(ert-deftest test-completion-symbol-hashtable-predicate ()
  (minibuf-tests--test-completion-pred
   #'minibuf-tests--strings-to-symbol-hashtable
   #'minibuf-tests--part-of-hashtable))
(ert-deftest test-completion-symbol-hashtable-completion-regexp ()
  (minibuf-tests--test-completion-regexp
   #'minibuf-tests--strings-to-symbol-hashtable))


;;; minibuf-tests.el ends here
