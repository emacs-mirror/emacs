;;; cl-extra-tests.el --- tests for emacs-lisp/cl-extra.el  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest cl-get ()
  (put 'cl-get-test 'x 1)
  (put 'cl-get-test 'y nil)
  (should (eq (cl-get 'cl-get-test 'x) 1))
  (should (eq (cl-get 'cl-get-test 'y :none) nil))
  (should (eq (cl-get 'cl-get-test 'z :none) :none)))

(ert-deftest cl-getf ()
  (let ((plist '(x 1 y nil)))
    (should (eq (cl-getf plist 'x) 1))
    (should (eq (cl-getf plist 'y :none) nil))
    (should (eq (cl-getf plist 'z :none) :none))))

(ert-deftest cl-extra-test-mapc ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (_x) nil))
        (fn2 (lambda (_x _y) nil))
        (fn3 (lambda (_x _y _z) nil)))
    (should (equal lst (cl-mapc fn1 lst)))
    (should (equal lst (cl-mapc fn2 lst lst2)))
    (should (equal lst (cl-mapc fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-mapl ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) (should (consp x))))
        (fn2 (lambda (x y) (should (and (consp x) (consp y)))))
        (fn3 (lambda (x y z) (should (and (consp x) (consp y) (consp z))))))
    (should (equal lst (cl-mapl fn1 lst)))
    (should (equal lst (cl-mapl fn2 lst lst2)))
    (should (equal lst (cl-mapl fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-mapcar ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) x))
        (fn2 (lambda (_x y) y))
        (fn3 (lambda (_x _y z) z)))
    (should (equal lst (cl-mapcar fn1 lst)))
    (should (equal lst2 (cl-mapcar fn2 lst lst2)))
    (should (equal lst3 (cl-mapcar fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-map ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) x))
        (fn2 (lambda (_x y) y))
        (fn3 (lambda (x _y _z) (string-to-char (format "%S" x)))))
    (should (equal lst (cl-map 'list fn1 lst)))
    (should (equal (vconcat lst2) (cl-map 'vector fn2 lst lst2)))
    (should (equal (mapconcat (lambda (x) (format "%S" x)) lst "")
                   (cl-map 'string fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-maplist ()
  (let ((lst '(a b c))
        (lst2 '(d e f))
        (lst3 '(1 2 3))
        (fn1 (lambda (x) (should (consp x)) x))
        (fn2 (lambda (x y) (should (and (consp x) (consp y))) y))
        (fn3 (lambda (x y z) (should (and (consp x) (consp y) (consp z))) z)))
    (should (equal (list lst (cdr lst) (cddr lst))
                   (cl-maplist fn1 lst)))
    (should (equal (list lst2 (cdr lst2) (cddr lst2))
                   (cl-maplist fn2 lst lst2)))
    (should (equal (list lst3 (cdr lst3) (cddr lst3))
                   (cl-maplist fn3 lst lst2 lst3)))))

(ert-deftest cl-extra-test-cl-make-random-state ()
  (let ((s (cl-make-random-state)))
    ;; Test for Bug#33731.
    (should-not (eq s (cl-make-random-state s)))))

(ert-deftest cl-concatenate ()
  (should (equal (cl-concatenate 'list '(1 2 3) '(4 5 6))
                 '(1 2 3 4 5 6)))
  (should (equal (cl-concatenate 'vector [1 2 3] [4 5 6])
                 [1 2 3 4 5 6]))
  (should (equal (cl-concatenate 'string "123" "456")
                 "123456")))

;;; cl-extra-tests.el ends here
