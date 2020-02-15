;;; sort-tests.el --- Tests for sort.el              -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'sort)

(defun sort-tests-random-word (n)
  (mapconcat (lambda (_) (string (let ((c (random 52)))
                              (+ (if (> c 25) 71 65)
                                 c))))
             (make-list n nil) ""))

(defun sort-tests--insert-words-sort-and-compare (words separator function reverse less-predicate)
  (with-temp-buffer
    (let ((aux words))
      (while aux
        (insert (pop aux))
        (when aux
          (insert separator))))
    ;; Final newline.
    (insert "\n")
    (funcall function reverse (point-min) (point-max))
    (let ((sorted-words
           (mapconcat #'identity
                      (sort (copy-sequence words)
                            (if reverse
                                (lambda (a b) (funcall less-predicate b a))
                              less-predicate))
                      separator)))
      (should (string= (substring (buffer-string) 0 -1) sorted-words)))))

;;; This function uses randomly generated tests and should satisfy
;;; most needs for this lib.
(cl-defun sort-tests-test-sorter-function (separator function &key generator less-pred noreverse)
  "Check that FUNCTION correctly sorts words separated by SEPARATOR.
This checks whether it is equivalent to sorting a list of such
words via LESS-PREDICATE, and then inserting them separated by
SEPARATOR.
LESS-PREDICATE defaults to `string-lessp'.
GENERATOR is a function called with one argument that returns a
word, it defaults to `sort-tests-random-word'.
NOREVERSE means that the first arg of FUNCTION is not used for
reversing the sort."
  (dotimes (n 20)
    ;; Sort n words of length n.
    (let ((words (mapcar (or generator #'sort-tests-random-word) (make-list n n)))
          (sort-fold-case nil)
          (less-pred (or less-pred #'string<)))
      (sort-tests--insert-words-sort-and-compare words separator function nil less-pred)
      (unless noreverse
        (sort-tests--insert-words-sort-and-compare
         words separator function 'reverse less-pred))
      (let ((less-pred-case (lambda (a b) (funcall less-pred (downcase a) (downcase b))))
            (sort-fold-case t))
        (sort-tests--insert-words-sort-and-compare words separator function nil less-pred-case)
        (unless noreverse
          (sort-tests--insert-words-sort-and-compare
           words separator function 'reverse less-pred-case))))))

(ert-deftest sort-tests--lines ()
  (sort-tests-test-sorter-function "\n" #'sort-lines))

(ert-deftest sort-tests--paragraphs ()
  (let ((paragraph-separate "[\s\t\f]*$"))
    (sort-tests-test-sorter-function "\n\n" #'sort-paragraphs)))

(ert-deftest sort-tests--numeric-fields ()
  (cl-labels ((field-to-number (f) (string-to-number (car (split-string f)))))
    (sort-tests-test-sorter-function "\n" (lambda (_ l r) (sort-numeric-fields 1 l (1- r)))
                           :noreverse t
                           :generator (lambda (_) (format "%s %s" (random) (sort-tests-random-word 20)))
                           :less-pred (lambda (a b) (< (field-to-number a)
                                                  (field-to-number b))))))

(ert-deftest sort-tests--fields-1 ()
  (cl-labels ((field-n (f n) (elt (split-string f) (1- n))))
    (sort-tests-test-sorter-function "\n" (lambda (_ l r) (sort-fields 1 l (1- r)))
                           :noreverse t
                           :generator (lambda (n) (concat (sort-tests-random-word n) " " (sort-tests-random-word n)))
                           :less-pred (lambda (a b) (string< (field-n a 1) (field-n b 1))))))

(ert-deftest sort-tests--fields-2 ()
  (cl-labels ((field-n (f n) (elt (split-string f) (1- n))))
    (sort-tests-test-sorter-function "\n" (lambda (_ l r) (sort-fields 2 l (1- r)))
                           :noreverse t
                           :generator (lambda (n) (concat (sort-tests-random-word n) " " (sort-tests-random-word n)))
                           :less-pred (lambda (a b) (string< (field-n a 2) (field-n b 2))))))

(provide 'sort-tests)
;;; sort-tests.el ends here
