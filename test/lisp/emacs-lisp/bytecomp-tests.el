;;; bytecomp-tests.el --- Tests for bytecomp.el  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 Free Software Foundation, Inc.

;; Author: Shigeru Fukaya <shigeru.fukaya@gmail.com>
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Created:        November 2008
;; Keywords:       internal
;; Human-Keywords: internal

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

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

;;; Code:

;; Replacement for `byte-compile--log-warning-for-byte-compile'
;; that doesn't call `display-warning' to avoid warnings being printed
;; to the test log when running noninteractively.
(defun bytecomp-tests--log-warning-for-byte-compile (string _pos _fill level)
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (byte-compile-warning-prefix level nil)
        (insert
         (format "%s%s\n"
                 (cond ((eq level :warning) "Warning: ")
                       ((eq level :error) "Error: "))
                 string))))))

(defmacro bytecomp-tests--with-warnings (&rest body)
  "Run BODY, compiler warnings going to `byte-compile-log-buffer' only."
  `(cl-letf (((symbol-function 'byte-compile--log-warning-for-byte-compile)
              #'bytecomp-tests--log-warning-for-byte-compile))
     ,@body))

(defvar bytecomp-test-var nil)

(defun bytecomp-test-get-var ()
  bytecomp-test-var)

(defun bytecomp-test-identity (x)
  "Identity, but hidden from some optimizations."
  x)

(defmacro bytecomp-test-loop (outer1 outer2 inner1 inner2)
  "Exercise constant propagation inside `while' loops.
OUTER1, OUTER2, INNER1 and INNER2 are forms placed in the outer and
inner loops respectively."
  `(let ((x 1) (i 3) (res nil))
     (while (> i 0)
       (let ((y 2) (j 2))
         (setq res (cons (list 'outer x y) res))
         (while (> j 0)
           (setq res (cons (list 'inner x y) res))
           ,inner1
           ,inner2
           (setq j (1- j)))
         ,outer1
         ,outer2)
       (setq i (1- i)))
     res))

(defvar bytecomp-tests--xx nil)

(defconst bytecomp-tests--test-cases
  '(
    ;; some functional tests
    (let ((a most-positive-fixnum) (b 1) (c 1.0))  (+ a b c))
    (let ((a most-positive-fixnum) (b -2) (c 1.0)) (- a b c))
    (let ((a most-positive-fixnum) (b 2) (c 1.0))  (* a b c))
    (let ((a 3) (b 2) (c 1.0))                     (/ a b c))
    (let ((a (+ 1 (expt 2 -64))) (b (expt 2 -65))) (+ a -1 b))
    (let ((a (+ 1 (expt 2 -64))) (b (expt 2 -65))) (- a 1 (- b)))
    (let ((a (expt 2 -1074)) (b 0.125))		   (* a 8 b))
    (let ((a 1.0))				   (* a 0))
    (let ((a 1.0))				   (* a 2.0 0))
    (let ((a 1.0))				   (/ 0 a))
    (let ((a 1.0))				   (/ 3 a 2))
    (let ((a most-positive-fixnum) (b 2.0))	   (* a 2 b))
    (let ((a 3) (b 2))				   (/ a b 1.0))
    (let ((a -0.0)) (+ a))
    (let ((a -0.0)) (- a))
    (let ((a -0.0)) (* a))
    (let ((a -0.0)) (min a))
    (let ((a -0.0)) (max a))
    (/ 3 -1)
    (+ 4 3 2 1)
    (+ 4 3 2.0 1)
    (- 4 3 2 1)				; not new, for reference
    (- 4 3 2.0 1)			; not new, for reference
    (* 4 3 2 1)
    (* 4 3 2.0 1)
    (/ 4 3 2 1)
    (/ 4 3 2.0 1)
    (let ((a 3) (b 2))				   (+ a b 1))
    (let ((a 3) (b 2))				   (+ a b -1))
    (let ((a 3) (b 2))				   (- a b 1))
    (let ((a 3) (b 2))				   (- a b -1))
    (let ((a 3) (b 2))				   (+ a b a 1))
    (let ((a 3) (b 2))				   (+ a b a -1))
    (let ((a 3) (b 2))				   (- a b a 1))
    (let ((a 3) (b 2))				   (- a b a -1))
    (let ((a 3) (b 2))				   (* a b -1))
    (let ((a 3) (b 2))				   (* a -1))
    (let ((a 3) (b 2))				   (/ a b 1))
    (let ((a 3) (b 2))				   (/ (+ a b) 1))

    ;; coverage test
    (let ((a 3) (b 2) (c 1.0)) (+))
    (let ((a 3) (b 2) (c 1.0)) (+ 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 2 0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (+ 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (+ a))
    (let ((a 3) (b 2) (c 1.0)) (+ a 0))
    (let ((a 3) (b 2) (c 1.0)) (+ a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (+ c 0))
    (let ((a 3) (b 2) (c 1.0)) (+ c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 c))
    (let ((a 3) (b 2) (c 1.0)) (+ 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (+ 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (+ 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (+ 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (+ a 1))
    (let ((a 3) (b 2) (c 1.0)) (+ a -1))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 a))
    (let ((a 3) (b 2) (c 1.0)) (+ -1 a))
    (let ((a 3) (b 2) (c 1.0)) (+ c 1))
    (let ((a 3) (b 2) (c 1.0)) (+ c -1))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 c))
    (let ((a 3) (b 2) (c 1.0)) (+ -1 c))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 0))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 1))
    (let ((a 3) (b 2) (c 1.0)) (+ a b -1))
    (let ((a 3) (b 2) (c 1.0)) (+ a b 2))
    (let ((a 3) (b 2) (c 1.0)) (+ 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (+ a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (+ a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (+ a b c -1))

    (let ((a 3) (b 2) (c 1.0)) (-))
    (let ((a 3) (b 2) (c 1.0)) (- 2))
    (let ((a 3) (b 2) (c 1.0)) (- 2 0))
    (let ((a 3) (b 2) (c 1.0)) (- 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 2.0))
    (let ((a 3) (b 2) (c 1.0)) (- 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (- 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 2))
    (let ((a 3) (b 2) (c 1.0)) (- 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (- a))
    (let ((a 3) (b 2) (c 1.0)) (- a 0))
    (let ((a 3) (b 2) (c 1.0)) (- a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (- c 0))
    (let ((a 3) (b 2) (c 1.0)) (- c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 c))
    (let ((a 3) (b 2) (c 1.0)) (- 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (- a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (- 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (- 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (- 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (- 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (- a 1))
    (let ((a 3) (b 2) (c 1.0)) (- a -1))
    (let ((a 3) (b 2) (c 1.0)) (- 1 a))
    (let ((a 3) (b 2) (c 1.0)) (- -1 a))
    (let ((a 3) (b 2) (c 1.0)) (- c 1))
    (let ((a 3) (b 2) (c 1.0)) (- c -1))
    (let ((a 3) (b 2) (c 1.0)) (- 1 c))
    (let ((a 3) (b 2) (c 1.0)) (- -1 c))
    (let ((a 3) (b 2) (c 1.0)) (- a b 0))
    (let ((a 3) (b 2) (c 1.0)) (- a b 1))
    (let ((a 3) (b 2) (c 1.0)) (- a b -1))
    (let ((a 3) (b 2) (c 1.0)) (- a b 2))
    (let ((a 3) (b 2) (c 1.0)) (- 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (- a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (- a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (- a b c -1))

    (let ((a 3) (b 2) (c 1.0)) (*))
    (let ((a 3) (b 2) (c 1.0)) (* 2))
    (let ((a 3) (b 2) (c 1.0)) (* 2 0))
    (let ((a 3) (b 2) (c 1.0)) (* 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 2.0))
    (let ((a 3) (b 2) (c 1.0)) (* 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (* 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 2))
    (let ((a 3) (b 2) (c 1.0)) (* 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (* a))
    (let ((a 3) (b 2) (c 1.0)) (* a 0))
    (let ((a 3) (b 2) (c 1.0)) (* a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (* c 0))
    (let ((a 3) (b 2) (c 1.0)) (* c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 c))
    (let ((a 3) (b 2) (c 1.0)) (* 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (* a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (* 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (* 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (* 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (* 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (* a 1))
    (let ((a 3) (b 2) (c 1.0)) (* a -1))
    (let ((a 3) (b 2) (c 1.0)) (* 1 a))
    (let ((a 3) (b 2) (c 1.0)) (* -1 a))
    (let ((a 3) (b 2) (c 1.0)) (* c 1))
    (let ((a 3) (b 2) (c 1.0)) (* c -1))
    (let ((a 3) (b 2) (c 1.0)) (* 1 c))
    (let ((a 3) (b 2) (c 1.0)) (* -1 c))
    (let ((a 3) (b 2) (c 1.0)) (* a b 0))
    (let ((a 3) (b 2) (c 1.0)) (* a b 1))
    (let ((a 3) (b 2) (c 1.0)) (* a b -1))
    (let ((a 3) (b 2) (c 1.0)) (* a b 2))
    (let ((a 3) (b 2) (c 1.0)) (* 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (* a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (* a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (* a b c -1))

    (let ((a 3) (b 2) (c 1.0)) (/))
    (let ((a 3) (b 2) (c 1.0)) (/ 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 2 0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2.0 0))
    (let ((a 3) (b 2) (c 1.0)) (/ 2.0 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 2.0))
    (let ((a 3) (b 2) (c 1.0)) (/ a))
    (let ((a 3) (b 2) (c 1.0)) (/ a 0))
    (let ((a 3) (b 2) (c 1.0)) (/ a 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 a))
    (let ((a 3) (b 2) (c 1.0)) (/ c 0))
    (let ((a 3) (b 2) (c 1.0)) (/ c 0.0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 c))
    (let ((a 3) (b 2) (c 1.0)) (/ 0.0 c))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 0 c 0))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a b))
    (let ((a 3) (b 2) (c 1.0)) (/ 0 a b c))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 2 3))
    (let ((a 3) (b 2) (c 1.0)) (/ 3.0 2.0 1))
    (let ((a 3) (b 2) (c 1.0)) (/ 3.0 2.0 1 4))
    (let ((a 3) (b 2) (c 1.0)) (/ a 1))
    (let ((a 3) (b 2) (c 1.0)) (/ a -1))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 a))
    (let ((a 3) (b 2) (c 1.0)) (/ -1 a))
    (let ((a 3) (b 2) (c 1.0)) (/ c 1))
    (let ((a 3) (b 2) (c 1.0)) (/ c -1))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 c))
    (let ((a 3) (b 2) (c 1.0)) (/ -1 c))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 0))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 1))
    (let ((a 3) (b 2) (c 1.0)) (/ a b -1))
    (let ((a 3) (b 2) (c 1.0)) (/ a b 2))
    (let ((a 3) (b 2) (c 1.0)) (/ 1 a b c))
    (let ((a 3) (b 2) (c 1.0)) (/ a b c 0))
    (let ((a 3) (b 2) (c 1.0)) (/ a b c 1))
    (let ((a 3) (b 2) (c 1.0)) (/ a b c -1))

    (let ((a t)) (logand 0 a))

    ;; Test switch bytecode
    (let ((a 3)) (cond ((eq a 1) 'one) ((eq a 2) 'two) ((eq a 3) 'three) (t t)))
    (let ((a 'three)) (cond ((eq a 'one) 1) ((eq a 2) 'two) ((eq a 'three) 3)
                            (t t)))
    (let ((a 2)) (cond ((eq a 'one) 1) ((eq a 1) 'one) ((eq a 2) 'two)
                       (t nil)))
    (let ((a 2.0)) (cond ((eql a 2) 'incorrect) ((eql a 2.00) 'correct)))
    (let ((a "foobar")) (cond ((equal "notfoobar" a) 'incorrect)
                              ((equal 1 a) 'incorrect)
                              ((equal a "foobar") 'correct)
                              (t 'incorrect)))
    (let ((a "foobar") (l t)) (pcase a
                                ("bar" 'incorrect)
                                ("foobar" (while l
                                            a (setq l nil))
                                 'correct)))
    (let ((a 'foobar) (l t)) (cl-case a
                         ('foo 'incorrect)
                         ('bar 'incorrect)
                         ('foobar (while l
                                    a (setq l nil))
                                  'correct)))
    (let ((a 'foobar) (l t)) (cond
                        ((eq a 'bar) 'incorrect)
                        ((eq a 'foo) 'incorrect)
                        ((eq a 'bar) 'incorrect)
                        (t (while l
                             a (setq l nil))
                           'correct)))
    (let ((a 'foobar) (l t)) (cond
                        ((eq a 'bar) 'incorrect)
                        ((eq a 'foo) 'incorrect)
                        ((eq a 'foobar)
                         (while l
                           a (setq l nil))
                         'correct)
                        (t 'incorrect)))
    (let ((a))
      (cond ((eq a 'foo) 'incorrect)
            (t)))
    (let ((a))
      (cond ((eq a 'foo) 'incorrect)
            ('correct)))
    ;; Bug#31734
    (let ((variable 0))
      (cond
       ((eq variable 'default)
	(message "equal"))
       (t
	(message "not equal"))))
    ;; Bug#35770
    (let ((x 'a)) (cond ((eq x 'a) 'correct)
                        ((eq x 'b) 'incorrect)
                        ((eq x 'a) 'incorrect)
                        ((eq x 'c) 'incorrect)))
    (let ((x #x10000000000000000))
      (cond ((eql x #x10000000000000000) 'correct)
            ((eql x #x10000000000000001) 'incorrect)
            ((eql x #x10000000000000000) 'incorrect)
            ((eql x #x10000000000000002) 'incorrect)))
    (let ((x "a")) (cond ((equal x "a") 'correct)
                         ((equal x "b") 'incorrect)
                         ((equal x "a") 'incorrect)
                         ((equal x "c") 'incorrect)))
    ;; Multi-value clauses
    (mapcar (lambda (x) (cond ((eq x 'a) 11)
                              ((memq x '(b a c d)) 22)
                              ((eq x 'c) 33)
                              ((eq x 'e) 44)
                              ((memq x '(d f g)) 55)
                              (t 99)))
            '(a b c d e f g h))
    (mapcar (lambda (x) (cond ((eql x 1) 11)
                              ((memq x '(a b c)) 22)
                              ((memql x '(2 1 4 1e-3)) 33)
                              ((eq x 'd) 44)
                              ((eql x #x10000000000000000))))
            '(1 2 4 1e-3 a b c d 1.0 #x10000000000000000))
    (mapcar (lambda (x) (cond ((eq x 'a) 11)
                              ((memq x '(b d)) 22)
                              ((equal x '(a . b)) 33)
                              ((member x '(b c 1.5 2.5 "X" (d))) 44)
                              ((eql x 3.14) 55)
                              ((memql x '(9 0.5 1.5 q)) 66)
                              (t 99)))
            '(a b c d (d) (a . b) "X" 0.5 1.5 3.14 9 9.0))
    ;; Multi-switch cond form
    (mapcar (lambda (p) (let ((x (car p)) (y (cadr p)))
                          (cond ((consp x) 11)
                                ((eq x 'a) 22)
                                ((memql x '(b 7 a -3)) 33)
                                ((equal y "a") 44)
                                ((memq y '(c d e)) 55)
                                ((booleanp x) 66)
                                ((eq x 'q) 77)
                                ((memq x '(r s)) 88)
                                ((eq x 't) 99)
                                (t 999))))
            '((a c) (b c) (7 c) (-3 c) (nil nil) (t c) (q c) (r c) (s c)
              (t c) (x "a") (x "c") (x c) (x d) (x e)))

    (mapcar (lambda (x) (ignore-errors (cond ((member '(a . b) x) 1)
                                             ((equal x '(c)) 2))))
            '(((a . b)) a b (c) (d)))
    (mapcar (lambda (x) (ignore-errors (cond ((memq '(a . b) x) 1)
                                             ((equal x '(c)) 2))))
            '(((a . b)) a b (c) (d)))
    (mapcar (lambda (x) (ignore-errors (cond ((member '(a b) x) 1)
                                             ((equal x '(c)) 2))))
            '(((a b)) a b (c) (d)))
    (mapcar (lambda (x) (ignore-errors (cond ((memq '(a b) x) 1)
                                             ((equal x '(c)) 2))))
            '(((a b)) a b (c) (d)))

    (assoc 'b '((a 1) (b 2) (c 3)))
    (assoc "b" '(("a" 1) ("b" 2) ("c" 3)))
    (let ((x '((a 1) (b 2) (c 3)))) (assoc 'c x))
    (assoc 'a '((a 1) (b 2) (c 3)) (lambda (u v) (not (equal u v))))

    ;; Constprop test cases
    (let ((a 'alpha) (b (concat "be" "ta")) (c nil) (d t) (e :gamma)
          (f '(delta epsilon)))
      (list a b c d e f))

    (let ((x 1) (y (+ 3 4)))
      (list
       (let (q (y x) (z y))
         (if q x (list x y z)))))

    (let* ((x 3) (y (* x 2)) (x (1+ y)))
      x)

    (let ((x 1) (bytecomp-test-var 2) (y 3))
      (list x bytecomp-test-var (bytecomp-test-get-var) y))

    (progn
      (defvar d)
      (let ((x 'a) (y 'b)) (list x y)))

    (let ((x 2))
      (list x (setq x 13) (setq x (* x 2)) x))

    (let ((x 'a) (y 'b))
      (setq y x
            x (cons 'c y)
            y x)
      (list x y))

    (let ((x 3))
      (let ((y x) z)
        (setq x 5)
        (setq y (+ y 8))
        (setq z (if (bytecomp-test-identity t)
                    (progn
                      (setq x (+ x 1))
                      (list x y))
                  (setq x (+ x 2))
                  (list x y)))
        (list x y z)))

    (let ((i 1) (s 0) (x 13))
      (while (< i 5)
        (setq s (+ s i))
        (setq i (1+ i)))
      (list s x i))

    (let ((x 2))
      (list (or (bytecomp-test-identity 'a) (setq x 3)) x))

    (mapcar (lambda (b)
              (let ((a nil))
                (+ 0
                   (progn
                     (setq a b)
                     (setq b 1)
                     a))))
            '(10))

    (let* ((x 1)
           (y (condition-case x
                  (/ 1 0)
                (arith-error x))))
      (list x y))

    (funcall
     (condition-case x
         (/ 1 0)
       (arith-error (prog1 (lambda (y) (+ y x))
                      (setq x 10))))
     4)

    ;; Loop constprop: set the inner and outer variables in the inner
    ;; and outer loops, all combinations.
    (bytecomp-test-loop nil        nil        nil        nil       )
    (bytecomp-test-loop nil        nil        nil        (setq x 6))
    (bytecomp-test-loop nil        nil        (setq x 5) nil       )
    (bytecomp-test-loop nil        nil        (setq x 5) (setq x 6))
    (bytecomp-test-loop nil        (setq x 4) nil        nil       )
    (bytecomp-test-loop nil        (setq x 4) nil        (setq x 6))
    (bytecomp-test-loop nil        (setq x 4) (setq x 5) nil       )
    (bytecomp-test-loop nil        (setq x 4) (setq x 5) (setq x 6))
    (bytecomp-test-loop (setq x 3) nil        nil        nil       )
    (bytecomp-test-loop (setq x 3) nil        nil        (setq x 6))
    (bytecomp-test-loop (setq x 3) nil        (setq x 5) nil       )
    (bytecomp-test-loop (setq x 3) nil        (setq x 5) (setq x 6))
    (bytecomp-test-loop (setq x 3) (setq x 4) nil        nil       )
    (bytecomp-test-loop (setq x 3) (setq x 4) nil        (setq x 6))
    (bytecomp-test-loop (setq x 3) (setq x 4) (setq x 5) nil       )
    (bytecomp-test-loop (setq x 3) (setq x 4) (setq x 5) (setq x 6))

    ;; No error, no success handler.
    (condition-case x
        (list 42)
      (error (cons 'bad x)))
    ;; Error, no success handler.
    (condition-case x
        (/ 1 0)
      (error (cons 'bad x)))
    ;; No error, success handler.
    (condition-case x
        (list 42)
      (error (cons 'bad x))
      (:success (cons 'good x)))
    ;; Error, success handler.
    (condition-case x
        (/ 1 0)
      (error (cons 'bad x))
      (:success (cons 'good x)))
    ;; Verify that the success code is not subject to the error handlers.
    (condition-case x
        (list 42)
      (error (cons 'bad x))
      (:success (/ (car x) 0)))
    ;; Check variable scoping on success.
    (let ((x 2))
      (condition-case x
          (list x)
        (error (list 'bad x))
        (:success (list 'good x))))
    ;; Check variable scoping on failure.
    (let ((x 2))
      (condition-case x
          (/ 1 0)
        (error (list 'bad x))
        (:success (list 'good x))))
    ;; Check capture of mutated result variable.
    (funcall
     (condition-case x
         3
       (:success (prog1 (lambda (y) (+ y x))
                   (setq x 10))))
     4)
    ;; Check for-effect context, on error.
    (let ((f (lambda (x)
               (condition-case nil
                   (/ 1 0)
                 (error 'bad)
                 (:success 'good))
               (1+ x))))
      (funcall f 3))
    ;; Check for-effect context, on success.
    (let ((f (lambda (x)
               (condition-case nil
                   nil
                 (error 'bad)
                 (:success 'good))
               (1+ x))))
      (funcall f 3))

    ;; Check `not' in cond switch (bug#49746).
    (mapcar (lambda (x) (cond ((equal x "a") 1)
                              ((member x '("b" "c")) 2)
                              ((not x) 3)))
            '("a" "b" "c" "d" nil))

    ;; `let' and `let*' optimizations with body being constant or variable
    (let* (a
           (b (progn (setq a (cons 1 a)) 2))
           (c (1+ b))
           (d (list a c)))
      d)
    (let ((a nil))
      (let ((b (progn (setq a (cons 1 a)) 2))
            (c (progn (setq a (cons 3 a))))
            (d (list a)))
        d))
    (let* ((_a 1)
           (_b 2))
      'z)
    (let ((_a 1)
          (_b 2))
      'z)
    (let (x y)
      y)
    (let* (x y)
      y)
    (let (x y)
      'a)
    (let* (x y)
      'a)

    ;; Check empty-list optimizations.
    (mapcar (lambda (x) (member x nil)) '("a" 2 nil))
    (mapcar (lambda (x) (memql x nil)) '(a 2 nil))
    (mapcar (lambda (x) (memq x nil)) '(a nil))
    (let ((n 0))
      (list (mapcar (lambda (x) (member (setq n (1+ n)) nil)) '(a "nil"))
            n))
    (mapcar (lambda (x) (assoc x nil)) '("a" nil))
    (mapcar (lambda (x) (assq x nil)) '(a nil))
    (mapcar (lambda (x) (rassoc x nil)) '("a" nil))
    (mapcar (lambda (x) (rassq x nil)) '(a nil))
    (let ((n 0))
      (list (mapcar (lambda (x) (assoc (setq n (1+ n)) nil)) '(a "nil"))
            n))

    ;; Exercise variable-aliasing optimizations.
    (let ((a (list 1)))
      (let ((b a))
        (let ((a (list 2)))
          (list a b))))

    (let ((a (list 1)))
      (let ((a (list 2))
            (b a))
        (list a b)))

    (let* ((a (list 1))
           (b a)
           (a (list 2)))
      (condition-case a
          (list a b)
        (error (list 'error a b))))

    (let* ((a (list 1))
           (b a)
           (a (list 2)))
      (condition-case a
          (/ 0)
        (error (list 'error a b))))

    (let* ((a (list 1))
           (b a)
           (a (list 2))
           (f (list (lambda (x) (list x a)))))
      (funcall (car f) 3))

    (let* ((a (list 1))
           (b a)
           (f (list (lambda (x) (setq a x)))))
      (funcall (car f) 3)
      (list a b))

    (let* ((a (list 1))
           (b a)
           (a (list 2))
           (f (list (lambda (x) (setq a x)))))
      (funcall (car f) 3)
      (list a b))

    (let ((x (list 1)))
      (let ((y x)
            (z (setq x (vector x))))
        (list x y z)))

    (let ((x (list 1)))
      (let* ((y x)
             (z (setq x (vector x))))
        (list x y z)))

    (cond)
    (mapcar (lambda (x) (cond ((= x 0)))) '(0 1))

    ;; These expressions give different results in lexbind and dynbind modes,
    ;; but in each the compiler and interpreter should agree!
    ;; (They look much the same but come in pairs exercising both the
    ;; `let' and `let*' paths.)
    (let ((f (lambda (x)
               (lambda ()
                 (let ((g (lambda () x)))
                   (let ((x 'a))
                     (list x (funcall g))))))))
      (funcall (funcall f 'b)))
    (let ((f (lambda (x)
               (lambda ()
                 (let ((g (lambda () x)))
                   (let* ((x 'a))
                     (list x (funcall g))))))))
      (funcall (funcall f 'b)))
    (let ((f (lambda (x)
               (lambda ()
                 (let ((g (lambda () x)))
                   (setq x (list x x))
                   (let ((x 'a))
                     (list x (funcall g))))))))
      (funcall (funcall f 'b)))
    (let ((f (lambda (x)
               (lambda ()
                 (let ((g (lambda () x)))
                   (setq x (list x x))
                   (let* ((x 'a))
                     (list x (funcall g))))))))
      (funcall (funcall f 'b)))
    (let ((f (lambda (x)
               (lambda ()
                 (let ((g (lambda () x))
                       (h (lambda () (setq x (list x x)))))
                   (let ((x 'a))
                     (list x (funcall g) (funcall h))))))))
      (funcall (funcall f 'b)))
    (let ((f (lambda (x)
               (lambda ()
                 (let ((g (lambda () x))
                       (h (lambda () (setq x (list x x)))))
                   (let* ((x 'a))
                     (list x (funcall g) (funcall h))))))))
      (funcall (funcall f 'b)))

    ;; Test constant-propagation of access to captured variables.
    (let* ((x 2)
           (f (lambda ()
                (let ((y x)) (list y 3 y)))))
      (funcall f))

    ;; Test rewriting of `set' to `setq' (only done on dynamic variables).
    (let ((xx 1)) (set 'xx 2) xx)
    (let ((bytecomp-tests--xx 1))
      (set 'bytecomp-tests--xx 2)
      bytecomp-tests--xx)
    (let ((aaa 1)) (set (make-local-variable 'aaa) 2) aaa)
    (let ((bytecomp-tests--xx 1))
      (set (make-local-variable 'bytecomp-tests--xx) 2)
      bytecomp-tests--xx)

    ;; Check for-effect optimization of `condition-case' body form.
    ;; With `condition-case' in for-effect context:
    (let ((x (bytecomp-test-identity ?A))
          (r nil))
      (condition-case e
          (characterp x)                ; value (:success, var)
        (error (setq r 'bad))
        (:success (setq r (list 'good e))))
      r)
    (let ((x (bytecomp-test-identity ?B))
          (r nil))
      (condition-case nil
          (characterp x)               ; for-effect (:success, no var)
        (error (setq r 'bad))
        (:success (setq r 'good)))
      r)
    (let ((x (bytecomp-test-identity ?C))
          (r nil))
      (condition-case e
          (characterp x)               ; for-effect (no :success, var)
        (error (setq r (list 'bad e))))
      r)
    (let ((x (bytecomp-test-identity ?D))
          (r nil))
      (condition-case nil
          (characterp x)               ; for-effect (no :success, no var)
        (error (setq r 'bad)))
      r)
    ;; With `condition-case' in value context:
    (let ((x (bytecomp-test-identity ?E)))
      (condition-case e
          (characterp x)               ; for-effect (:success, var)
        (error (list 'bad e))
        (:success (list 'good e))))
    (let ((x (bytecomp-test-identity ?F)))
      (condition-case nil
          (characterp x)               ; for-effect (:success, no var)
        (error 'bad)
        (:success 'good)))
    (let ((x (bytecomp-test-identity ?G)))
      (condition-case e
          (characterp x)               ; value (no :success, var)
        (error (list 'bad e))))
    (let ((x (bytecomp-test-identity ?H)))
      (condition-case nil
          (characterp x)               ; value (no :success, no var)
        (error 'bad)))

    (condition-case nil
        (bytecomp-test-identity 3)
      (error 'bad)
      (:success))                       ; empty handler

    ;; `cond' miscompilation bug
    (let ((fn (lambda (x)
                (let ((y nil))
                  (cond ((progn (setq x (1+ x)) (> x 10)) (setq y 'a))
                        ((eq x 1) (setq y 'b))
                        ((eq x 2) (setq y 'c)))
                  (list x y)))))
      (mapcar fn (bytecomp-test-identity '(0 1 2 3 10 11))))

    ;; `nconc' nil arg elimination
    (nconc (list 1 2 3 4) nil)
    (nconc (list 1 2 3 4) nil nil)
    (let ((x (cons 1 (cons 2 (cons 3 4)))))
      (nconc x nil))
    (let ((x (cons 1 (cons 2 (cons 3 4)))))
      (nconc x nil nil))
    (let ((x (cons 1 (cons 2 (cons 3 4)))))
      (nconc nil x nil (list 5 6) nil))

    ;; (+ 0 -0.0) etc
    (let ((x (bytecomp-test-identity -0.0)))
      (list x (+ x) (+ 0 x) (+ x 0) (+ 1 2 -3 x) (+ 0 x 0)))

    ;; Unary comparisons: keep side-effect, return t
    (let ((x 0))
      (list (= (setq x 1))
            x))
    ;; Aristotelian identity optimization
    (let ((x (bytecomp-test-identity 1)))
      (list (eq x x) (eql x x) (equal x x)))

    ;; Legacy single-arg `apply' call
    (apply '(* 2 3))
    )
  "List of expressions for cross-testing interpreted and compiled code.")

(defconst bytecomp-tests--test-cases-lexbind-only
  `(
    ;; This would infloop (and exhaust stack) with dynamic binding.
    (let ((f #'car))
      (let ((f (lambda (x) (cons (funcall f x) (cdr x)))))
        (funcall f '(1 . 2))))
    )
  "List of expressions for cross-testing interpreted and compiled code.
These are only tested with lexical binding.")

(defun bytecomp-tests--eval-interpreted (form)
  "Evaluate FORM using the Lisp interpreter, returning errors as a
special value."
  (condition-case err
      (let ((inhibit-message t))
        (eval form lexical-binding))
    (error (list 'bytecomp-check-error (car err)))))

(defun bytecomp-tests--eval-compiled (form)
  "Evaluate FORM using the Lisp byte-code compiler, returning errors as a
special value."
  (let ((warning-minimum-log-level :emergency)
        (byte-compile-warnings nil))
     (condition-case err
	 (funcall (bytecomp-tests--with-warnings
                   (byte-compile (list 'lambda nil form))))
       (error (list 'bytecomp-check-error (car err))))))

(ert-deftest bytecomp-tests-lexbind ()
  "Check that various expressions behave the same when interpreted and
byte-compiled.  Run with lexical binding."
  (let ((lexical-binding t))
    (dolist (form (append bytecomp-tests--test-cases-lexbind-only
                          bytecomp-tests--test-cases))
      (ert-info ((prin1-to-string form) :prefix "form: ")
        (should (equal (bytecomp-tests--eval-interpreted form)
                       (bytecomp-tests--eval-compiled form)))))))

(ert-deftest bytecomp-tests-dynbind ()
  "Check that various expressions behave the same when interpreted and
byte-compiled.  Run with dynamic binding."
  (let ((lexical-binding nil))
    (dolist (form bytecomp-tests--test-cases)
      (ert-info ((prin1-to-string form) :prefix "form: ")
        (should (equal (bytecomp-tests--eval-interpreted form)
                       (bytecomp-tests--eval-compiled form)))))))

(ert-deftest bytecomp--fun-value-as-head ()
  ;; Check that (FUN-VALUE ...) is a valid call, for compatibility (bug#68931).
  ;; (There is also a warning but this test does not check that.)
  (dolist (lb '(nil t))
    (ert-info ((prin1-to-string lb) :prefix "lexical-binding: ")
      (let* ((lexical-binding lb)
             (s-int '(lambda (x) (1+ x)))
             (s-comp (byte-compile s-int))
             (v-int (lambda (x) (1+ x)))
             (v-comp (byte-compile v-int))
             (comp (lambda (f)
                     (funcall (bytecomp-tests--with-warnings
                               (byte-compile `(lambda () (,f 3))))))))
        (should (equal (funcall comp s-int) 4))
        (should (equal (funcall comp s-comp) 4))
        (should (equal (funcall comp v-int) 4))
        (should (equal (funcall comp v-comp) 4))))))

(defmacro bytecomp-tests--with-fresh-warnings (&rest body)
  `(let ((macroexp--warned            ; oh dear
          (make-hash-table :test #'equal :weakness 'key)))
     (bytecomp-tests--with-warnings
      ,@body)))

(defun test-byte-comp-compile-and-load (compile &rest forms)
  (declare (indent 1))
  (ert-with-temp-file elfile
    :suffix ".el"
    (ert-with-temp-file elcfile
      :suffix ".elc"
      (with-temp-buffer
        (insert ";;; -*- lexical-binding: t -*-\n")
        (dolist (form forms)
          (print form (current-buffer)))
        (write-region (point-min) (point-max) elfile nil 'silent))
      (if compile
          (let ((byte-compile-dest-file-function
                 (lambda (e) elcfile)))
            (bytecomp-tests--with-fresh-warnings
             (byte-compile-file elfile))))
      (load elfile nil 'nomessage))))

(ert-deftest test-byte-comp-macro-expansion ()
  (test-byte-comp-compile-and-load t
    '(progn (defmacro abc (arg) 1) (defun def () (abc 2))))
  (should (equal (funcall 'def) 1)))

(ert-deftest test-byte-comp-macro-expansion-eval-and-compile ()
  (test-byte-comp-compile-and-load t
    '(eval-and-compile (defmacro abc (arg) -1) (defun def () (abc 2))))
  (should (equal (funcall 'def) -1)))

(ert-deftest test-byte-comp-macro-expansion-eval-when-compile ()
  ;; Make sure we interpret eval-when-compile forms properly.  CLISP
  ;; and SBCL interpreter eval-when-compile (well, the CL equivalent)
  ;; in the same way.
  (test-byte-comp-compile-and-load t
    '(eval-when-compile
      (defmacro abc (arg) -10)
      (defun abc-1 () (abc 2)))
    '(defmacro abc-2 () (abc-1))
    '(defun def () (abc-2)))
  (should (equal (funcall 'def) -10)))

(ert-deftest test-byte-comp-macro-expand-lexical-override ()
  ;; Intuitively, one might expect the defmacro to override the
  ;; macrolet since macrolet's is explicitly called out as being
  ;; equivalent to toplevel, but CLISP and SBCL both evaluate the form
  ;; this way, so we should too.
  (test-byte-comp-compile-and-load t
    '(require 'cl-lib)
    '(cl-macrolet ((m () 4))
      (defmacro m () 5)
      (defun def () (m))))
  (should (equal (funcall 'def) 4)))


;;;; Warnings.

(ert-deftest bytecomp-tests--warnings ()
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (let ((inhibit-read-only t)) (erase-buffer)))
  (mapc #'fmakunbound '(my-test0 my--test11 my--test12 my--test2))
  (test-byte-comp-compile-and-load t
    '(progn
       (defun my-test0 ()
         (my--test11 3)
         (my--test12 3)
         (my--test2 5))
       (defmacro my--test11 (arg) (+ arg 1))
       (eval-and-compile
         (defmacro my--test12 (arg) (+ arg 1))
         (defun my--test2 (arg) (+ arg 1)))))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (goto-char (point-min))
    ;; Should warn that mt--test1[12] are first used as functions.
    ;; The second alternative is for when the file name is so long
    ;; that pretty-printing starts the message on the next line.
    (should (or (re-search-forward "my--test11:\n.*macro" nil t)
                (re-search-forward "my--test11:\n.*:\n.*macro" nil t)))
    (should (or (re-search-forward "my--test12:\n.*macro" nil t)
                (re-search-forward "my--test12:\n.*:\n.*macro" nil t)))
    (goto-char (point-min))
    ;; Should not warn that mt--test2 is not known to be defined.
    (should-not (re-search-forward "my--test2" nil t))))

(defun bytecomp--with-warning-test (re-warning form)
  (declare (indent 1))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
     (let ((inhibit-read-only t)) (erase-buffer))
       (ert-info ((prin1-to-string form) :prefix "form: ")
         (let ((text-quoting-style 'grave))
           (bytecomp-tests--with-fresh-warnings
            (byte-compile form)))
         (ert-info ((prin1-to-string (buffer-string)) :prefix "buffer: ")
           (should (re-search-forward
                    (string-replace " " "[ \n]+" re-warning)))))))

(defun bytecomp--without-warning-test (form)
  (bytecomp--with-warning-test "\\`\\'" form))

(ert-deftest bytecomp-warn--ignore ()
  (bytecomp--with-warning-test "unused"
    '(lambda (y) 6))
  (bytecomp--without-warning-test
    '(lambda (y) (ignore y) 6))
  (bytecomp--with-warning-test "assq"
    '(lambda (x y) (progn (assq x y) 5)))
  (bytecomp--without-warning-test
    '(lambda (x y) (progn (ignore (assq x y)) 5))))

(ert-deftest bytecomp-warn-wrong-args ()
  (bytecomp--with-warning-test "remq.*3.*2"
    '(remq 1 2 3)))

(ert-deftest bytecomp-warn-wrong-args-subr ()
  (bytecomp--with-warning-test "safe-length.*3.*1"
    '(safe-length 1 2 3)))

(ert-deftest bytecomp-warn-variable-lacks-prefix ()
  (bytecomp--with-warning-test "foo.*lacks a prefix"
    '(defvar foo nil)))

(defvar bytecomp-tests--docstring (make-string 100 ?x))

(ert-deftest bytecomp-warn-wide-docstring/defconst ()
  (bytecomp--with-warning-test "defconst.*foo.*wider than.*characters"
    `(defconst foo t ,bytecomp-tests--docstring)))

(ert-deftest bytecomp-warn-wide-docstring/defvar ()
  (bytecomp--with-warning-test "defvar.*foo.*wider than.*characters"
    `(defvar foo t ,bytecomp-tests--docstring)))

(ert-deftest bytecomp-warn-wide-docstring/cl-defsubst ()
  (bytecomp--without-warning-test
   `(cl-defsubst short-name ()
      "Do something."))
  (bytecomp--without-warning-test
   `(cl-defsubst long-name-with-less-80-characters-but-still-quite-a-bit ()
      "Do something."))
  (bytecomp--with-warning-test "wider than.*characters"
   `(cl-defsubst long-name-with-more-than-80-characters-yes-this-is-a-very-long-name-but-why-not!! ()
      "Do something.")))

(ert-deftest bytecomp-warn-wide-docstring/cl-defstruct ()
  (bytecomp--without-warning-test
   `(cl-defstruct short-name
      field))
  (bytecomp--without-warning-test
   `(cl-defstruct short-name
      long-name-with-less-80-characters-but-still-quite-a-bit))
  (bytecomp--without-warning-test
   `(cl-defstruct long-name-with-less-80-characters-but-still-quite-a-bit
      field))
  (bytecomp--with-warning-test "wider than.*characters"
    `(cl-defstruct short-name
       long-name-with-more-than-80-characters-yes-this-is-a-very-long-name-but-why-not!!))
  (bytecomp--with-warning-test "wider than.*characters"
    `(cl-defstruct long-name-with-more-than-80-characters-yes-this-is-a-very-long-name-but-why-not!!
       field)))

(ert-deftest bytecomp-warn-quoted-condition ()
  (bytecomp--with-warning-test
      "Warning: `condition-case' condition should not be quoted: 'arith-error"
    '(condition-case nil
         (abc)
       ('arith-error "ugh")))
  (bytecomp--with-warning-test
      "Warning: `ignore-error' condition argument should not be quoted: 'error"
    '(ignore-error 'error (abc))))

(ert-deftest bytecomp-warn-dodgy-args-eq ()
  (dolist (fn '(eq eql))
    (cl-flet ((msg (type arg)
                (format
                 "`%s' called with literal %s that may never match (arg %d)"
                 fn type arg)))
      (bytecomp--with-warning-test (msg "list" 1)   `(,fn '(a) 'x))
      (bytecomp--with-warning-test (msg "string" 2) `(,fn 'x "a"))
      (bytecomp--with-warning-test (msg "vector" 2) `(,fn 'x [a]))
      (bytecomp--with-warning-test (msg "function" 2) `(,fn 'x (lambda () 1)))
      (bytecomp--with-warning-test (msg "function" 2) `(,fn 'x #'(lambda () 1)))
      (unless (eq fn 'eql)
        (bytecomp--with-warning-test (msg "integer" 2) `(,fn 'x #x10000000000))
        (bytecomp--with-warning-test (msg "float" 2)   `(,fn 'x 1.0))))))

(ert-deftest bytecomp-warn-dodgy-args-memq ()
  (dolist (fn '(memq memql remq delq assq rassq))
    (cl-labels
        ((msg1 (type)
           (format
            "`%s' called with literal %s that may never match (arg 1)"
            fn type))
         (msg2 (type)
           (format
            "`%s' called with literal %s that may never match (element 2 of arg 2)"
            fn type))
         (lst (elt)
           (cond ((eq fn 'assq)  `((a . 1) (,elt . 2) (c . 3)))
                 ((eq fn 'rassq) `((1 . a) (2 . ,elt) (3 . c)))
                 (t              `(a       ,elt       c))))
         (form2 (elt)
           `(,fn 'x ',(lst elt))))

    (bytecomp--with-warning-test (msg1 "list")   `(,fn '(a) '(x)))
    (bytecomp--with-warning-test (msg1 "string") `(,fn "a" '(x)))
    (bytecomp--with-warning-test (msg1 "vector") `(,fn [a] '(x)))
    (bytecomp--with-warning-test (msg1 "function") `(,fn (lambda () 1) '(x)))
    (bytecomp--with-warning-test (msg1 "function") `(,fn #'(lambda () 1) '(x)))
    (unless (eq fn 'memql)
      (bytecomp--with-warning-test (msg1 "integer") `(,fn #x10000000000 '(x)))
      (bytecomp--with-warning-test (msg1 "float")   `(,fn 1.0 '(x))))

    (bytecomp--with-warning-test (msg2 "list")   (form2 '(b)))
    (bytecomp--with-warning-test (msg2 "list")   (form2 ''b))
    (bytecomp--with-warning-test (msg2 "string") (form2 "b"))
    (bytecomp--with-warning-test (msg2 "vector") (form2 [b]))
    (unless (eq fn 'memql)
      (bytecomp--with-warning-test (msg2 "integer") (form2 #x10000000000))
      (bytecomp--with-warning-test (msg2 "float")   (form2 1.0))))))

(defmacro bytecomp--define-warning-file-test (file re-warning &optional reverse)
  `(ert-deftest ,(intern (format "bytecomp/%s" file)) ()
     (with-current-buffer (get-buffer-create "*Compile-Log*")
       (let ((inhibit-read-only t)) (erase-buffer))
       (bytecomp-tests--with-warnings
        (byte-compile-file ,(ert-resource-file file)))
       (ert-info ((buffer-string) :prefix "buffer: ")
         (,(if reverse 'should-not 'should)
          (re-search-forward ,re-warning nil t))))))

(bytecomp--define-warning-file-test "error-lexical-var-with-add-hook.el"
                            "add-hook.*lexical var")

(bytecomp--define-warning-file-test "error-lexical-var-with-remove-hook.el"
                            "remove-hook.*lexical var")

(bytecomp--define-warning-file-test "error-lexical-var-with-run-hook-with-args-until-failure.el"
                            "args-until-failure.*lexical var")

(bytecomp--define-warning-file-test "error-lexical-var-with-run-hook-with-args-until-success.el"
                            "args-until-success.*lexical var")

(bytecomp--define-warning-file-test "error-lexical-var-with-run-hook-with-args.el"
                            "args.*lexical var")

(bytecomp--define-warning-file-test "error-lexical-var-with-symbol-value.el"
                            "symbol-value.*lexical var")

(bytecomp--define-warning-file-test "warn-autoload-not-on-top-level.el"
                            "compiler ignores.*autoload.*")

(bytecomp--define-warning-file-test "warn-callargs.el"
                            "with 2 arguments, but accepts only 1")

(bytecomp--define-warning-file-test "warn-callargs-defsubst.el"
                            "with 2 arguments, but accepts only 1")

(bytecomp--define-warning-file-test "warn-defcustom-nogroup.el"
                            "fails to specify containing group")

(bytecomp--define-warning-file-test "warn-defcustom-notype.el"
                            "missing :type keyword parameter")

(bytecomp--define-warning-file-test "warn-defvar-lacks-prefix.el"
                            "var.*foo.*lacks a prefix")

(bytecomp--define-warning-file-test "warn-format.el"
                            "called with 2 arguments to fill 1 format field")

(bytecomp--define-warning-file-test "warn-free-setq.el"
                            "free.*foo")

(bytecomp--define-warning-file-test "warn-free-variable-reference.el"
                            "free variable .bar")

(bytecomp--define-warning-file-test "warn-make-variable-buffer-local.el"
                            "make-variable-buffer-local. not called at toplevel")

(bytecomp--define-warning-file-test "warn-interactive-only.el"
                            "next-line.*interactive use only.*forward-line")

(bytecomp--define-warning-file-test "warn-lambda-malformed-interactive-spec.el"
                            "malformed .interactive. specification")

(bytecomp--define-warning-file-test "warn-obsolete-defun.el"
  "foo-obsolete. is an obsolete function (as of 99.99)")

(defvar bytecomp--tests-obsolete-var nil)
(make-obsolete-variable 'bytecomp--tests-obsolete-var nil "99.99")

(bytecomp--define-warning-file-test "warn-obsolete-hook.el"
  "bytecomp--tests-obsolete-var. is an obsolete variable (as of 99.99)")

(bytecomp--define-warning-file-test "warn-obsolete-variable-same-file.el"
                            "foo-obs.*obsolete.*99.99" t)

(bytecomp--define-warning-file-test "warn-obsolete-variable.el"
  "bytecomp--tests-obsolete-var. is an obsolete variable (as of 99.99)")

(bytecomp--define-warning-file-test "warn-obsolete-variable-bound.el"
                            "bytecomp--tests-obs.*obsolete.*99.99" t)

(bytecomp--define-warning-file-test "warn-redefine-defun-as-macro.el"
                            "as both function and macro")

(bytecomp--define-warning-file-test "warn-redefine-macro-as-defun.el"
                            "as both function and macro")

(bytecomp--define-warning-file-test "warn-redefine-defun.el"
                            "defined multiple")

(bytecomp--define-warning-file-test "warn-save-excursion.el"
                            "with-current.*rather than save-excursion")

(bytecomp--define-warning-file-test "warn-variable-let-bind-constant.el"
                            "let-bind constant")

(bytecomp--define-warning-file-test "warn-variable-let-bind-nonvariable.el"
                            "let-bind nonvariable")

(bytecomp--define-warning-file-test "warn-variable-set-constant.el"
                            "attempt to set constant")

(bytecomp--define-warning-file-test "warn-variable-setq-nonvariable.el"
                            "attempt to set non-variable")

(bytecomp--define-warning-file-test "warn-variable-setq-odd.el"
                            "odd number of arguments")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-autoload.el"
 "autoload .foox. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-custom-declare-variable.el"
 "custom-declare-variable .foo. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-defalias.el"
 "defalias .foo. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-defconst.el"
 "defconst .foo-bar. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-define-abbrev-table.el"
 "define-abbrev-table .foo. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-define-obsolete-function-alias.el"
 "defalias .foo. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-define-obsolete-variable-alias.el"
 "defvaralias .foo. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-defun.el"
 "Warning: docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-defvar.el"
 "defvar .foo-bar. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-defvaralias.el"
 "defvaralias .foo-bar. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-ignore-fill-column.el"
 "defvar .foo-bar. docstring wider than .* characters" 'reverse)

(bytecomp--define-warning-file-test
 "warn-wide-docstring-ignore-function-signature.el"
 "defvar .foo-bar. docstring wider than .* characters" 'reverse)

(bytecomp--define-warning-file-test
 "warn-wide-docstring-ignore-override.el"
 "defvar .foo-bar. docstring wider than .* characters" 'reverse)

(bytecomp--define-warning-file-test
 "warn-wide-docstring-ignore-substitutions.el"
 "defvar .foo-bar. docstring wider than .* characters" 'reverse)

(bytecomp--define-warning-file-test
 "warn-wide-docstring-ignore.el"
 "defvar .foo-bar. docstring wider than .* characters" 'reverse)

(bytecomp--define-warning-file-test
 "warn-wide-docstring-multiline-first.el"
 "defvar .foo-bar. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "warn-wide-docstring-multiline.el"
 "defvar .foo-bar. docstring wider than .* characters")

(bytecomp--define-warning-file-test
 "nowarn-inline-after-defvar.el"
 "Lexical argument shadows" 'reverse)

(bytecomp--define-warning-file-test
 "warn-make-process-missing-keyword-arg.el"
 "called without required keyword argument :command")

(bytecomp--define-warning-file-test
 "warn-make-process-unknown-keyword-arg.el"
 "called with unknown keyword argument :coding-system")

(bytecomp--define-warning-file-test
 "warn-make-process-repeated-keyword-arg.el"
 "called with repeated keyword argument :name")

(bytecomp--define-warning-file-test
 "warn-make-process-missing-keyword-value.el"
 "missing value for keyword argument :command")

;;;; NEW STUFF, 2025-07-13
(bytecomp--define-warning-file-test "macro-warning-position.el" ":18:8:")

(bytecomp--define-warning-file-test "macro-warning-position-2.el" ":18:8:")
;;;; END OF NEW STUFF

;;;; Macro expansion.

(ert-deftest test-eager-load-macro-expansion ()
  (test-byte-comp-compile-and-load nil
    '(progn (defmacro abc (arg) 1) (defun def () (abc 2))))
  (should (equal (funcall 'def) 1)))

(ert-deftest test-eager-load-macro-expansion-eval-and-compile ()
  (test-byte-comp-compile-and-load nil
    '(eval-and-compile (defmacro abc (arg) -1) (defun def () (abc 2))))
  (should (equal (funcall 'def) -1)))

(ert-deftest test-eager-load-macro-expansion-eval-when-compile ()
  ;; Make sure we interpret eval-when-compile forms properly.  CLISP
  ;; and SBCL interpreter eval-when-compile (well, the CL equivalent)
  ;; in the same way.
  (test-byte-comp-compile-and-load nil
    '(eval-when-compile
      (defmacro abc (arg) -10)
      (defun abc-1 () (abc 2)))
    '(defmacro abc-2 () (abc-1))
    '(defun def () (abc-2)))
  (should (equal (funcall 'def) -10)))

(ert-deftest test-eager-load-macro-expand-lexical-override ()
  ;; Intuitively, one might expect the defmacro to override the
  ;; macrolet since macrolet's is explicitly called out as being
  ;; equivalent to toplevel, but CLISP and SBCL both evaluate the form
  ;; this way, so we should too.
  (test-byte-comp-compile-and-load nil
    '(require 'cl-lib)
    '(cl-macrolet ((m () 4))
      (defmacro m () 5)
      (defun def () (m))))
  (should (equal (funcall 'def) 4)))

(ert-deftest test-eager-load-macro-expand-defalias ()
  (ert-with-temp-file elfile
    :suffix ".el"
    (write-region
     (concat ";;; -*- lexical-binding: t -*-\n"
             (mapconcat #'prin1-to-string
                        '((defalias 'nothing '(macro . ignore))
                          (defalias 'something (cons 'macro #'identity))
                          (defalias 'five (cons 'macro (lambda (&rest _) 5)))
                          (eval-when-compile
                            (defun def () (or (nothing t) (something (five nil))))))
                        "\n"))
     nil elfile)
    (let* ((byte-compile-debug t)
           (byte-compile-dest-file-function #'ignore))
      (bytecomp-tests--with-warnings
       (byte-compile-file elfile))
      (should (equal (funcall 'def) 5)))))

(defmacro bytecomp-tests--with-temp-file (file-name-var &rest body)
  (declare (indent 1))
  (cl-check-type file-name-var symbol)
  `(ert-with-temp-file ,file-name-var
     (unwind-protect
         (progn ,@body)
       (let ((elc (concat ,file-name-var ".elc")))
         (if (file-exists-p elc) (delete-file elc))))))

(defun bytecomp-tests--log-from-compilation (source)
  "Compile the string SOURCE and return the compilation log output."
  (let ((text-quoting-style 'grave)
        (byte-compile-log-buffer (generate-new-buffer " *Compile-Log*")))
    (with-current-buffer byte-compile-log-buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (bytecomp-tests--with-temp-file el-file
      (write-region source nil el-file)
      (bytecomp-tests--with-warnings
       (byte-compile-file el-file)))
    (with-current-buffer byte-compile-log-buffer
      (buffer-string))))

(ert-deftest bytecomp-tests--lexical-binding-cookie ()
  (cl-flet ((cookie-warning (source)
              (string-search
               "file has no `lexical-binding' directive on its first line"
               (bytecomp-tests--log-from-compilation source))))
    (dolist (lb '(t nil))
      (let ((lexical-binding lb)
            (some-code "(defun my-fun () 12)\n"))
        (should-not (cookie-warning
                     (concat ";;; -*-lexical-binding:t-*-\n" some-code)))
        (should-not (cookie-warning
                     (concat ";;; -*-lexical-binding:nil-*-\n" some-code)))
        (should (cookie-warning some-code))))))

(defun bytecomp-tests--f (x y &optional u v) (list x y u v))

(ert-deftest bytecomp-tests--warn-arity-non-compiled-callee ()
  "Check that calls to non-compiled functions are arity-checked (bug#78685)"
  (should (not (compiled-function-p (symbol-function 'bytecomp-tests--f))))
  (let* ((source (concat ";;; -*-lexical-binding:t-*-\n"
                         "(defun my-fun () (bytecomp-tests--f 11))\n"))
         (lexical-binding t)
         (log (bytecomp-tests--log-from-compilation source)))
    (should (string-search
             (concat "Warning: `bytecomp-tests--f' called with 1 argument,"
                     " but requires 2-4")
             log))))

(ert-deftest bytecomp-tests--unescaped-char-literals ()
  "Check that byte compiling warns about unescaped character
literals (Bug#20852)."
  (should (boundp 'lread--unescaped-character-literals))
  (let ((byte-compile-error-on-warn t)
        (byte-compile-debug t)
        (text-quoting-style 'grave))
    (bytecomp-tests--with-temp-file source
      (write-region (concat ";;; -*-lexical-binding:t-*-\n"
                            "(list ?) ?( ?; ?\" ?[ ?])")
                    nil source)
      (bytecomp-tests--with-temp-file destination
        (let* ((byte-compile-dest-file-function (lambda (_) destination))
               (err (should-error (byte-compile-file source))))
          (should (equal (cdr err)
                         `(,(concat "unescaped character literals "
                                    "`?\"', `?(', `?)', `?;', `?[', `?]' "
                                    "detected, "
                                    "`?\\\"', `?\\(', `?\\)', `?\\;', `?\\[', "
                                    "`?\\]' expected!")))))))
    ;; But don't warn in subsequent compilations (Bug#36068).
    (bytecomp-tests--with-temp-file source
      (write-region (concat ";;; -*-lexical-binding:t-*-\n"
                            "(list 1 2 3)")
                    nil source)
      (bytecomp-tests--with-temp-file destination
        (let ((byte-compile-dest-file-function (lambda (_) destination)))
          (should (byte-compile-file source)))))))

(ert-deftest bytecomp-tests-function-put ()
  "Check `function-put' operates during compilation."
  (bytecomp-tests--with-temp-file source
    (insert  ";;; -*-lexical-binding:t-*-\n")
    (dolist (form '((function-put 'bytecomp-tests--foo 'foo 1)
                    (function-put 'bytecomp-tests--foo 'bar 2)
                    (defmacro bytecomp-tests--foobar ()
                      `(cons ,(function-get 'bytecomp-tests--foo 'foo)
                             ,(function-get 'bytecomp-tests--foo 'bar)))
                    (defvar bytecomp-tests--foobar 1)
                    (setq bytecomp-tests--foobar (bytecomp-tests--foobar))))
      (print form (current-buffer)))
    (write-region (point-min) (point-max) source nil 'silent)
    (byte-compile-file source)
    (load source nil t)
    (should (equal bytecomp-tests--foobar (cons 1 2)))))

(ert-deftest bytecomp-tests--test-no-warnings-with-advice ()
  (defun f ())
  (define-advice f (:around (oldfun &rest args) test)
    (apply oldfun args))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (let ((inhibit-read-only t)) (erase-buffer)))
  (test-byte-comp-compile-and-load t '(defun f ()))
  (with-current-buffer (get-buffer-create "*Compile-Log*")
    (goto-char (point-min))
    (should-not (search-forward "Warning" nil t))))

(ert-deftest bytecomp-test-featurep-warnings ()
  (let ((byte-compile-log-buffer (generate-new-buffer " *Compile-Log*")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "\
\(defun foo ()
  (an-undefined-function))

\(defun foo1 ()
  (if (featurep 'xemacs)
      (some-undefined-function-if)))

\(defun foo2 ()
  (and (featurep 'xemacs)
      (some-undefined-function-and)))

\(defun foo3 ()
  (if (not (featurep 'emacs))
      (some-undefined-function-not)))

\(defun foo4 ()
  (or (featurep 'emacs)
      (some-undefined-function-or)))
")
            (bytecomp-tests--with-warnings
             (byte-compile-from-buffer (current-buffer))))
          (with-current-buffer byte-compile-log-buffer
            (should (search-forward "an-undefined-function" nil t))
            (should-not (search-forward "some-undefined-function" nil t))))
      (if (buffer-live-p byte-compile-log-buffer)
          (kill-buffer byte-compile-log-buffer)))))

(ert-deftest bytecomp-test--switch-duplicates ()
  "Check that duplicates in switches are eliminated correctly (bug#35770)."
  :expected-result (if byte-compile-cond-use-jump-table :passed :failed)
  (dolist (params
           '(((lambda (x)
                (cond ((eq x 'a) 111)
                      ((eq x 'b) 222)
                      ((eq x 'a) 333)
                      ((eq x 'c) 444)))
              (a b c)
              string<)
             ((lambda (x)
                (cond ((eql x #x10000000000000000) 111)
                      ((eql x #x10000000000000001) 222)
                      ((eql x #x10000000000000000) 333)
                      ((eql x #x10000000000000002) 444)))
              (#x10000000000000000 #x10000000000000001 #x10000000000000002)
              <)
             ((lambda (x)
                (cond ((equal x "a") 111)
                      ((equal x "b") 222)
                      ((equal x "a") 333)
                      ((equal x "c") 444)))
              ("a" "b" "c")
              string<)))
    (let* ((lisp (nth 0 params))
           (keys (nth 1 params))
           (lessp (nth 2 params))
           (bc (byte-compile lisp))
           (lap (byte-decompile-bytecode (aref bc 1) (aref bc 2)))
           ;; Assume the first constant is the switch table.
           (table (cadr (assq 'byte-constant lap))))
      (should (hash-table-p table))
      (should (equal (sort (hash-table-keys table) lessp) keys))
      (should (member '(byte-constant 111) lap))
      (should (member '(byte-constant 222) lap))
      (should-not (member '(byte-constant 333) lap))
      (should (member '(byte-constant 444) lap)))))

(defun test-suppression (form suppress match)
  (let ((lexical-binding t)
        (text-quoting-style 'grave)
        (byte-compile-log-buffer (generate-new-buffer " *Compile-Log*")))
    ;; Check that we get a warning without suppression.
    (with-current-buffer byte-compile-log-buffer
      (setq-local fill-column 9999)
      (setq-local warning-fill-column fill-column)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (test-byte-comp-compile-and-load t form)
    (with-current-buffer byte-compile-log-buffer
      (unless match
        (error "%s" (buffer-string)))
      (goto-char (point-min))
      (should (string-match match (buffer-string))))
    ;; And that it's gone now.
    (with-current-buffer byte-compile-log-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (test-byte-comp-compile-and-load t
     `(with-suppressed-warnings ,suppress
        ,form))
    (with-current-buffer byte-compile-log-buffer
      (goto-char (point-min))
      (should-not (string-match match (buffer-string))))
    ;; Also check that byte compiled forms are identical.
    (let ((normal (bytecomp-tests--with-warnings (byte-compile form)))
          (nowarn (bytecomp-tests--with-warnings
                   (byte-compile `(with-suppressed-warnings ,suppress ,form)))))
      (should (equal normal nowarn)))))

(ert-deftest bytecomp-test--with-suppressed-warnings ()
  (test-suppression
   '(defvar prefixless)
   '((lexical prefixless))
   "global/dynamic var .prefixless. lacks")

  ;; FIXME: These messages cannot be suppressed reliably right now,
  ;; but attempting mutate `nil' or `5' is a rather daft thing to do
  ;; in the first place.  Preventing mutation of constants such as
  ;; `most-positive-fixnum' makes more sense but the compiler doesn't
  ;; warn about that at all right now (it's caught at runtime, and we
  ;; allow writing the same value).
  ;;
  ;; (test-suppression
  ;;  '(defun foo()
  ;;     (let ((nil t))
  ;;       (message-mail)))
  ;;  '((constants nil))
  ;;  "Warning: attempt to let-bind constant .nil.")

  (test-suppression
   '(progn
      (defun obsolete ()
        (declare (obsolete foo "22.1")))
      (defun zot ()
        (obsolete)))
   '((obsolete obsolete))
   "Warning: .obsolete. is an obsolete function")

  (test-suppression
   '(progn
      (defun wrong-params (foo &optional unused)
        (ignore unused)
        foo)
      (defun zot ()
        (wrong-params 1 2 3)))
   '((callargs wrong-params))
   "Warning: .wrong-params. called with")

  (test-byte-comp-compile-and-load nil
    (defvar obsolete-variable nil)
    (make-obsolete-variable 'obsolete-variable nil "24.1"))
  (test-suppression
   '(defun zot ()
      obsolete-variable)
   '((obsolete obsolete-variable))
   "obsolete")

  (test-suppression
   '(defun zot ()
      (next-line))
   '((interactive-only next-line))
   "interactive use only")

  (test-suppression
   '(defun zot ()
      (mapcar #'list '(1 2 3))
      nil)
   '((ignored-return-value mapcar))
   "Warning: value from call to `mapcar' is unused; use `mapc' or `dolist' instead")

  (test-suppression
   '(defun zot ()
      free-variable)
   '((free-vars free-variable))
   "Warning: reference to free variable")

  (test-suppression
   '(defun zot ()
      (save-excursion
        (set-buffer (get-buffer-create "foo"))
        nil))
   '((suspicious set-buffer))
   "Warning: Use .with-current-buffer. rather than")

  (test-suppression
   '(defun zot (x)
      (condition-case nil (list x)))
   '((suspicious condition-case))
   "Warning: `condition-case' without handlers")

  (test-suppression
   '(defun zot (x)
      (unwind-protect (print x)))
   '((suspicious unwind-protect))
   "Warning: `unwind-protect' without unwind forms")

  (test-suppression
   '(defun zot (x)
      (cond
       ((zerop x) 'zero)
       (t 'nonzero)
       (happy puppy)))
   '((suspicious cond))
   "Warning: Useless clause following default `cond' clause")

  (test-suppression
   '(defun zot ()
      (let ((_ 1))
        ))
   '((empty-body let))
   "Warning: `let' with empty body")

  (test-suppression
   '(defun zot ()
      (let* ((_ 1))
        ))
   '((empty-body let*))
   "Warning: `let\\*' with empty body")

  (test-suppression
   '(defun zot (x)
      (when x
        ))
   '((empty-body when))
   "Warning: `when' with empty body")

  (test-suppression
   '(defun zot (x)
      (unless x
        ))
   '((empty-body unless))
   "Warning: `unless' with empty body")

  (test-suppression
   '(defun zot (x)
      (ignore-error arith-error
        ))
   '((empty-body ignore-error))
   "Warning: `ignore-error' with empty body")

  (test-suppression
   '(defun zot (x)
      (with-suppressed-warnings ((suspicious eq))
        ))
   '((empty-body with-suppressed-warnings))
   "Warning: `with-suppressed-warnings' with empty body")

  (test-suppression
   '(defun zot ()
      (setcar '(1 2) 3))
   '((mutate-constant setcar))
   "Warning: `setcar' on constant list (arg 1)")

  (test-suppression
   '(defun zot ()
      (aset [1 2] 1 3))
   '((mutate-constant aset))
   "Warning: `aset' on constant vector (arg 1)")

  (test-suppression
   '(defun zot ()
      (aset "abc" 1 ?d))
   '((mutate-constant aset))
   "Warning: `aset' on constant string (arg 1)")

  (test-suppression
   '(defun zot (x y)
      (nconc x y '(1 2) '(3 4)))
   '((mutate-constant nconc))
   "Warning: `nconc' on constant list (arg 3)")

  (test-suppression
   '(defun zot ()
      (put-text-property 0 2 'prop 'val "abc"))
   '((mutate-constant put-text-property))
   "Warning: `put-text-property' on constant string (arg 5)")
  )

(ert-deftest bytecomp-tests--not-writable-directory ()
  "Test that byte compilation works if the output directory isn't
writable (Bug#44631)."
  (ert-with-temp-directory directory
    (let* ((input-file (expand-file-name "test.el" directory))
           (output-file (expand-file-name "test.elc" directory))
           (byte-compile-dest-file-function
            (lambda (_) output-file))
           (byte-compile-error-on-warn t))
      (unwind-protect
          (progn
            (write-region ";;; -*-lexical-binding:t-*-\n"
                          nil input-file nil nil nil 'excl)
            (write-region "" nil output-file nil nil nil 'excl)
            (set-file-modes input-file #o400)
            (set-file-modes output-file #o200)
            (set-file-modes directory #o500)
            (should (byte-compile-file input-file))
            (should (file-regular-p output-file))
            (should (plusp (file-attribute-size
                            (file-attributes output-file)))))
        ;; Allow the directory to be deleted.
        (set-file-modes directory #o777)))))

(ert-deftest bytecomp-tests--dest-mountpoint ()
  "Test that byte compilation works if the destination file is a
mountpoint (Bug#44631)."
  (let ((bwrap (executable-find "bwrap"))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless bwrap)
    (skip-unless (file-executable-p bwrap))
    (skip-unless (not (file-remote-p bwrap)))
    (skip-unless (file-executable-p emacs))
    (skip-unless (not (file-remote-p emacs)))
    (ert-with-temp-directory directory
      (let* ((input-file (expand-file-name "test.el" directory))
             (output-file (expand-file-name "test.elc" directory))
             (unquoted-file (file-name-unquote output-file))
             (byte-compile-dest-file-function
              (lambda (_) output-file))
             (byte-compile-error-on-warn t))
        (should-not (file-remote-p input-file))
        (should-not (file-remote-p output-file))
        (write-region ";;; -*-lexical-binding:t-*-\n"
                      nil input-file nil nil nil 'excl)
        (write-region "" nil output-file nil nil nil 'excl)
        (unwind-protect
            (progn
              (set-file-modes input-file #o400)
              (set-file-modes output-file #o200)
              (set-file-modes directory #o500)
              (skip-unless
               (zerop (call-process
                       bwrap nil nil nil
                       "--ro-bind" "/" "/"
                       "--bind" unquoted-file unquoted-file
                       "true")))
              (with-temp-buffer
                (let ((status (call-process
                               bwrap nil t nil
                               "--ro-bind" "/" "/"
                               "--bind" unquoted-file unquoted-file
                               emacs "--quick" "--batch" "--load=bytecomp"
                               (format "--eval=%S"
                                       `(setq byte-compile-dest-file-function
                                              (lambda (_) ,output-file)
                                              byte-compile-error-on-warn t))
                               "--funcall=batch-byte-compile" input-file)))
                  (unless (eql status 0)
                    (ert-fail `((status . ,status)
                                (output . ,(buffer-string)))))))
              (should (file-regular-p output-file))
              (should (plusp (file-attribute-size
                              (file-attributes output-file)))))
          ;; Allow the directory to be deleted.
          (set-file-modes directory #o777))))))

(ert-deftest bytecomp-tests--target-file-no-directory ()
  "Check that Bug#45287 is fixed."
  (ert-with-temp-directory directory
    (let* ((default-directory directory)
           (byte-compile-dest-file-function (lambda (_) "test.elc"))
           (byte-compile-error-on-warn t))
      (write-region  ";;; -*-lexical-binding:t-*-\n"
                     nil "test.el" nil nil nil 'excl)
      (should (byte-compile-file "test.el"))
      (should (file-regular-p "test.elc"))
      (should (plusp (file-attribute-size
                      (file-attributes "test.elc")))))))

(defun bytecomp-tests--get-vars ()
  (list (ignore-errors (symbol-value 'bytecomp-tests--var1))
        (ignore-errors (symbol-value 'bytecomp-tests--var2))))

(ert-deftest bytecomp-local-defvar ()
  "Check that local `defvar' declarations work correctly, both
interpreted and compiled."
  (let ((lexical-binding t))
    (let ((fun '(lambda ()
                  (defvar bytecomp-tests--var1)
                  (let ((bytecomp-tests--var1 'a)    ; dynamic
                        (bytecomp-tests--var2 'b))   ; still lexical
                    (ignore bytecomp-tests--var2)    ; avoid warning
                    (bytecomp-tests--get-vars)))))
      (should (listp fun))      ; Guard against overzealous refactoring!
      (should (equal (funcall (eval fun t)) '(a nil)))
      (should (equal (funcall (byte-compile fun)) '(a nil)))
      )

    ;; `progn' does not constitute a lexical scope for `defvar' (bug#46387).
    (let ((fun '(lambda ()
                  (progn
                    (defvar bytecomp-tests--var1)
                    (defvar bytecomp-tests--var2))
                  (let ((bytecomp-tests--var1 'c)
                        (bytecomp-tests--var2 'd))
                    (bytecomp-tests--get-vars)))))
      (should (listp fun))
      (should (equal (funcall (eval fun t)) '(c d)))
      (should (equal (funcall (byte-compile fun)) '(c d))))))

(ert-deftest bytecomp-reify-function ()
  "Check that closures that modify their bound variables are
compiled correctly."
  (cl-letf ((lexical-binding t)
            ((symbol-function 'counter) nil))
    (let ((x 0))
      (defun counter () (incf x))
      (should (equal (counter) 1))
      (should (equal (counter) 2))
      ;; byte compiling should not cause counter to always return the
      ;; same value (bug#46834)
      (byte-compile 'counter)
      (should (equal (counter) 3))
      (should (equal (counter) 4)))
    (let ((x 0))
      (let ((x 1))
        (defun counter () x)
        (should (equal (counter) 1))
        ;; byte compiling should not cause the outer binding to shadow
        ;; the inner one (bug#46834)
        (byte-compile 'counter)
        (should (equal (counter) 1))))))

(ert-deftest bytecomp-string-vs-docstring ()
  ;; Don't confuse a string return value for a docstring.
  (let ((lexical-binding t))
    (should (equal (funcall (bytecomp-tests--with-warnings
                             (byte-compile '(lambda (x) "foo")))
                            'dummy)
                   "foo"))))

(ert-deftest bytecomp-condition-case-success ()
  ;; No error, no success handler.
  (should (equal (condition-case x
                     (list 42)
                   (error (cons 'bad x)))
                 '(42)))
  ;; Error, no success handler.
  (should (equal (condition-case x
                     (/ 1 0)
                   (error (cons 'bad x)))
                 '(bad arith-error)))
  ;; No error, success handler.
  (should (equal (condition-case x
                     (list 42)
                   (error (cons 'bad x))
                   (:success (cons 'good x)))
                 '(good 42)))
  ;; Error, success handler.
  (should (equal (condition-case x
                     (/ 1 0)
                   (error (cons 'bad x))
                   (:success (cons 'good x)))
                 '(bad arith-error)))
  ;; Verify that the success code is not subject to the error handlers.
  (should-error (condition-case x
                    (list 42)
                  (error (cons 'bad x))
                  (:success (/ (car x) 0)))
                :type 'arith-error)
  ;; Check variable scoping.
  (let ((x 2))
    (should (equal (condition-case x
                       (list x)
                     (error (list 'bad x))
                     (:success (list 'good x)))
                   '(good (2))))
    (should (equal (condition-case x
                       (/ 1 0)
                     (error (list 'bad x))
                     (:success (list 'good x)))
                   '(bad (arith-error)))))
  ;; Check capture of mutated result variable.
  (should (equal (funcall
                  (condition-case x
                      3
                    (:success (prog1 (lambda (y) (+ y x))
                                (setq x 10))))
                  4)
                 14))
    ;; Check for-effect context, on error.
  (should (equal (let ((f (lambda (x)
                            (condition-case nil
                                (/ 1 0)
                              (error 'bad)
                              (:success 'good))
                            (1+ x))))
                   (funcall f 3))
                 4))
  ;; Check for-effect context, on success.
  (should (equal (let ((f (lambda (x)
                            (condition-case nil
                                nil
                              (error 'bad)
                              (:success 'good))
                            (1+ x))))
                   (funcall f 3))
                 4)))

(declare-function bc-test-alpha-f (ert-resource-file "bc-test-alpha.el"))

(ert-deftest bytecomp-defsubst ()
  ;; Check that lexical variables don't leak into inlined code.  See
  ;; https://lists.gnu.org/archive/html/emacs-devel/2021-05/msg01227.html

  ;; First, remove any trace of the functions and package defined:
  (fmakunbound 'bc-test-alpha-f)
  (fmakunbound 'bc-test-beta-f)
  (setq features (delq 'bc-test-beta features))
  ;; Byte-compile one file that uses a function from another file that isn't
  ;; compiled.
  (let ((file (ert-resource-file "bc-test-alpha.el"))
        (load-path (cons (ert-resource-directory) load-path)))
    (byte-compile-file file)
    (load (concat file "c") nil t t)
    (should (equal (bc-test-alpha-f 'a) '(nil a)))))

(ert-deftest bytecomp-tests-byte-compile--wide-docstring-p/func-arg-list ()
  (should-not (byte-compile--wide-docstring-p "\
\(dbus-register-property BUS SERVICE PATH INTERFACE PROPERTY ACCESS \
[TYPE] VALUE &optional EMITS-SIGNAL DONT-REGISTER-SERVICE)" fill-column))
  (should-not (byte-compile--wide-docstring-p "\
(fn CMD FLAGS FIS &key (BUF (cvs-temp-buffer)) DONT-CHANGE-DISC CVSARGS \
POSTPROC)" fill-column))
  ;; Bug#49007
  (should-not (byte-compile--wide-docstring-p "\
(fn (THIS rudel-protocol-backend) TRANSPORT \
INFO INFO-CALLBACK &optional PROGRESS-CALLBACK)" fill-column))
  (should-not (byte-compile--wide-docstring-p "\
\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] BODY...)" fill-column))
  (should-not (byte-compile--wide-docstring-p "\
(make-soap-xs-element &key NAME NAMESPACE-TAG ID TYPE^ OPTIONAL? MULTIPLE? \
REFERENCE SUBSTITUTION-GROUP ALTERNATIVES IS-GROUP)" fill-column))
  (should-not (byte-compile--wide-docstring-p "\
(fn NAME FIXTURE INPUT &key SKIP-PAIR-STRING EXPECTED-STRING \
EXPECTED-POINT BINDINGS (MODES \\='\\='(ruby-mode js-mode python-mode)) \
(TEST-IN-COMMENTS t) (TEST-IN-STRINGS t) (TEST-IN-CODE t) \
(FIXTURE-FN \\='#\\='electric-pair-mode))" fill-column)))

(ert-deftest bytecomp-test-defcustom-type ()
  (cl-flet ((dc (type) `(defcustom mytest nil "doc" :type ',type :group 'test)))
    (bytecomp--with-warning-test
     (rx "type should not be quoted") (dc ''integer))
    (bytecomp--with-warning-test
     (rx "type should not be quoted") (dc '(choice '(repeat boolean))))
    (bytecomp--with-warning-test
     (rx "misplaced :tag keyword") (dc '(choice (const b :tag "a"))))
    (bytecomp--with-warning-test
     (rx "`choice' without any types inside") (dc '(choice :tag "a")))
    (bytecomp--with-warning-test
     (rx "`other' not last in `choice'")
     (dc '(choice (const a) (other b) (const c))))
    (bytecomp--with-warning-test
     (rx "duplicated value in `choice': `a'")
     (dc '(choice (const a) (const b) (const a))))
    (bytecomp--with-warning-test
     (rx "duplicated :tag string in `choice': \"X\"")
     (dc '(choice (const :tag "X" a) (const :tag "Y" b) (other :tag "X" c))))
    (bytecomp--with-warning-test
     (rx "`cons' requires 2 type specs, found 1")
     (dc '(cons :tag "a" integer)))
    (bytecomp--with-warning-test
     (rx "`repeat' without type specs")
     (dc '(repeat :tag "a")))
    (bytecomp--with-warning-test
     (rx "`const' with too many values")
     (dc '(const :tag "a" x y)))
    (bytecomp--with-warning-test
     (rx "`const' with quoted value")
     (dc '(const :tag "a" 'x)))
    (bytecomp--with-warning-test
     (rx "`bool' is not a valid type")
     (dc '(bool :tag "a")))
    (bytecomp--with-warning-test
     (rx "irregular type `:tag'")
     (dc '(:tag "a")))
    (bytecomp--with-warning-test
     (rx "irregular type `\"string\"'")
     (dc '(list "string")))
    (bytecomp--with-warning-test
     (rx "`list' without arguments")
     (dc 'list))
    (bytecomp--with-warning-test
     (rx "`integerp' is not a valid type")
     (dc 'integerp))
    ))

(ert-deftest bytecomp-test-defcustom-local ()
  (cl-flet ((dc (local) `(defcustom mytest nil "doc" :type 'sexp :local ',local :group 'test)))
    (bytecomp--with-warning-test
     (rx ":local keyword does not accept 'symbol") (dc 'symbol))
    (bytecomp--with-warning-test
     (rx ":local keyword does not accept \"string\"") (dc "string"))
    (bytecomp--without-warning-test (dc t))
    (bytecomp--without-warning-test (dc 'permanent))
    (bytecomp--without-warning-test (dc 'permanent-only))
    ))

(ert-deftest bytecomp-test-defface-spec ()
  (cl-flet ((df (spec) `(defface mytest ',spec "doc" :group 'test)))
    (bytecomp--with-warning-test
     (rx "Bad face display condition `max-colors'")
     (df '((((class color grayscale) (max-colors 75) (background light))
            :foreground "cyan"))))
    (bytecomp--with-warning-test
     (rx "Bad face display `bad-default'")
     (df '((bad-default :foreground "cyan"))))
    (bytecomp--with-warning-test
     (rx "`:inverse' is not a valid face attribute keyword")
     (df '((t :background "blue" :inverse t))))
    (bytecomp--with-warning-test
     (rx "`:inverse' is not a valid face attribute keyword")
     (df '((t (:background "blue" :inverse t)))))  ; old attr list syntax
    (bytecomp--with-warning-test
     (rx "Face attribute `:reverse-video' has been removed;"
         " use `:inverse-video' instead")
     (df '((t :background "red" :reverse-video t))))
    (bytecomp--with-warning-test
     (rx "Value for face attribute `:inherit' should not be quoted")
     (df '((t :inherit 'other))))
    (bytecomp--with-warning-test
     (rx "Missing face attribute `:extend' value")
     (df '((t :foundry "abc" :extend))))
    (bytecomp--with-warning-test
     (rx "Non-keyword in face attribute list: `\"green\"'")
     (df '((t :foreground "white" "green"))))
    ))

(ert-deftest bytecomp-function-attributes ()
  ;; Check that `byte-compile' keeps the declarations, interactive spec and
  ;; doc string of the function (bug#55830).
  (let ((fname 'bytecomp-test-fun))
    (fset fname nil)
    (put fname 'pure nil)
    (put fname 'lisp-indent-function nil)
    (eval `(defun ,fname (x)
             "tata"
             (declare (pure t) (indent 1))
             (interactive "P")
             (list 'toto x))
          t)
    (let ((bc (byte-compile fname)))
      (should (byte-code-function-p bc))
      (should (equal (funcall bc 'titi) '(toto titi)))
      (should (equal (aref bc 5) "P"))
      (should (equal (get fname 'pure) t))
      (should (equal (get fname 'lisp-indent-function) 1))
      (should (equal (aref bc 4) "tata\n\n(fn X)")))))

(ert-deftest bytecomp-fun-attr-warn ()
  ;; Check that warnings are emitted when doc strings, `declare' and
  ;; `interactive' forms don't come in the proper order, or more than once.
  (let* ((filename "fun-attr-warn.el")
         (el (ert-resource-file filename))
         (elc (concat el "c"))
         (text-quoting-style 'grave))
    (with-current-buffer (get-buffer-create "*Compile-Log*")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (bytecomp-tests--with-warnings
       (byte-compile-file el))
      (let ((expected
             '("70:4: Warning: `declare' after `interactive'"
               "74:4: Warning: Doc string after `interactive'"
               "79:4: Warning: Doc string after `interactive'"
               "84:4: Warning: Doc string after `declare'"
               "89:4: Warning: Doc string after `declare'"
               "96:4: Warning: `declare' after `interactive'"
               "102:4: Warning: `declare' after `interactive'"
               "108:4: Warning: `declare' after `interactive'"
               "106:4: Warning: Doc string after `interactive'"
               "114:4: Warning: `declare' after `interactive'"
               "112:4: Warning: Doc string after `interactive'"
               "118:4: Warning: Doc string after `interactive'"
               "119:4: Warning: `declare' after `interactive'"
               "124:4: Warning: Doc string after `interactive'"
               "125:4: Warning: `declare' after `interactive'"
               "130:4: Warning: Doc string after `declare'"
               "136:4: Warning: Doc string after `declare'"
               "142:4: Warning: Doc string after `declare'"
               "148:4: Warning: Doc string after `declare'"
               "159:4: Warning: More than one doc string"
               "165:4: Warning: More than one doc string"
               "171:4: Warning: More than one doc string"
               "178:4: Warning: More than one doc string"
               "186:4: Warning: More than one doc string"
               "192:4: Warning: More than one doc string"
               "200:4: Warning: More than one doc string"
               "206:4: Warning: More than one doc string"
               "215:4: Warning: More than one `declare' form"
               "222:4: Warning: More than one `declare' form"
               "230:4: Warning: More than one `declare' form"
               "237:4: Warning: More than one `declare' form"
               "244:4: Warning: More than one `interactive' form"
               "251:4: Warning: More than one `interactive' form"
               "258:4: Warning: More than one `interactive' form"
               "257:4: Warning: `declare' after `interactive'"
               "265:4: Warning: More than one `interactive' form"
               "264:4: Warning: `declare' after `interactive'")))
        (goto-char (point-min))
        (let ((actual nil))
          (while (re-search-forward
                  (rx bol (* (not ":")) ":"
                      (group (+ digit) ":" (+ digit) ": Warning: "
                             (or "More than one " (+ nonl) " form"
                                 (: (+ nonl) " after " (+ nonl))))
                      eol)
                  nil t)
            (push (match-string 1) actual))
          (setq actual (nreverse actual))
          (should (equal actual expected)))))))

(ert-deftest byte-compile-file/no-byte-compile ()
  (let* ((src-file (ert-resource-file "no-byte-compile.el"))
         (dest-file (make-temp-file "bytecomp-tests-" nil ".elc"))
         (byte-compile-dest-file-function (lambda (_) dest-file)))
    (should (eq (byte-compile-file src-file) 'no-byte-compile))
    (should-not (file-exists-p dest-file))))

(ert-deftest bytecomp--copy-tree ()
  (should (null (bytecomp--copy-tree nil)))
  (let ((print-circle t))
    (let* ((x '(1 2 (3 4)))
           (y (bytecomp--copy-tree x)))
      (should (equal (prin1-to-string (list x y))
                     "((1 2 (3 4)) (1 2 (3 4)))")))
    (let* ((x '#1=(a #1#))
           (y (bytecomp--copy-tree x)))
      (should (equal (prin1-to-string (list x y))
                     "(#1=(a #1#) #2=(a #2#))")))
    (let* ((x '#1=(#1# a))
           (y (bytecomp--copy-tree x)))
      (should (equal (prin1-to-string (list x y))
                     "(#1=(#1# a) #2=(#2# a))")))
    (let* ((x '((a . #1=(b)) #1#))
           (y (bytecomp--copy-tree x)))
      (should (equal (prin1-to-string (list x y))
                     "(((a . #1=(b)) #1#) ((a . #2=(b)) #2#))")))
    (let* ((x '#1=(a #2=(#1# b . #3=(#2# c . #1#)) (#3# d)))
           (y (bytecomp--copy-tree x)))
      (should (equal (prin1-to-string (list x y))
                     (concat
                      "("
                      "#1=(a #2=(#1# b . #3=(#2# c . #1#)) (#3# d))"
                      " "
                      "#4=(a #5=(#4# b . #6=(#5# c . #4#)) (#6# d))"
                      ")"))))))

(require 'backtrace)

(defun bytecomp-tests--error-frame (fun args)
  "Call FUN with ARGS.  Return result or (ERROR . BACKTRACE-FRAME)."
  (letrec ((handler (lambda (e)
                      (throw 'bytecomp-tests--backtrace
                             (cons e (cadr (backtrace-get-frames handler)))))))
    (catch 'bytecomp-tests--backtrace
      (handler-bind ((error handler))
        (apply fun args)))))

(defconst bytecomp-tests--byte-op-error-cases
  '(((car a) (wrong-type-argument listp a))
    ((cdr 3) (wrong-type-argument listp 3))
    ((setcar 4 b) (wrong-type-argument consp 4))
    ((setcdr c 5) (wrong-type-argument consp c))
    ((nth 2 "abcd") (wrong-type-argument listp "abcd"))
    ((elt (x y . z) 2) (wrong-type-argument listp z))
    ((aref [2 3 5] p) (wrong-type-argument fixnump p))
    ((aref #s(a b c) p) (wrong-type-argument fixnump p))
    ((aref "abc" p) (wrong-type-argument fixnump p))
    ((aref [2 3 5] 3) (args-out-of-range [2 3 5] 3))
    ((aref #s(a b c) 3) (args-out-of-range #s(a b c) 3))
    ((aset [2 3 5] q 1) (wrong-type-argument fixnump q))
    ((aset #s(a b c) q 1) (wrong-type-argument fixnump q))
    ((aset [2 3 5] -1 1) (args-out-of-range [2 3 5] -1))
    ((aset #s(a b c) -1 1) (args-out-of-range #s(a b c) -1))
    ;; Many more to add
    ))

(ert-deftest bytecomp--byte-op-error-backtrace ()
  "Check that signaling byte ops show up in the backtrace."
  (dolist (case bytecomp-tests--byte-op-error-cases)
    (ert-info ((prin1-to-string case) :prefix "case: ")
      (let* ((call (nth 0 case))
             (expected-error (nth 1 case))
             (fun-sym (car call))
             (actuals (cdr call)))
        ;; Test both calling the function directly, and calling
        ;; a byte-compiled η-expansion (lambda (ARGS...) (FUN ARGS...))
        ;; which should turn the function call into a byte-op.
        (dolist (mode '(funcall byte-op))
          (ert-info ((symbol-name mode) :prefix "mode: ")
            (let* ((fun (pcase-exhaustive mode
                          ('funcall fun-sym)
                          ('byte-op
                           (let* ((nargs (length (cdr call)))
                                  (formals (mapcar (lambda (i)
                                                     (intern (format "x%d" i)))
                                                   (number-sequence 1 nargs))))
                             (byte-compile
                              `(lambda ,formals (,fun-sym ,@formals)))))))
                   (error-frame (bytecomp-tests--error-frame fun actuals)))
              (should (consp error-frame))
              (should (equal (car error-frame) expected-error))
              (let ((frame (cdr error-frame)))
                (should (equal (type-of frame) 'backtrace-frame))
                (should (equal (cons (backtrace-frame-fun frame)
                                     (backtrace-frame-args frame))
                               call))))))))))

(ert-deftest bytecomp--eq-symbols-with-pos-enabled ()
  ;; Verify that we don't optimize away a binding of
  ;; `symbols-with-pos-enabled' around an application of `eq' (bug#65017).
  (let* ((sym-with-pos1 (read-positioning-symbols "sym"))
         (sym-with-pos2 (read-positioning-symbols " sym"))  ; <- space!
         (without-pos-eq (lambda (a b)
                           (let ((symbols-with-pos-enabled nil))
                             (eq a b))))
         (without-pos-eq-compiled (byte-compile without-pos-eq))
         (with-pos-eq (lambda (a b)
                        (let ((symbols-with-pos-enabled t))
                          (eq a b))))
         (with-pos-eq-compiled (byte-compile with-pos-eq)))
    (dolist (mode '(interpreted compiled))
      (ert-info ((symbol-name mode) :prefix "mode: ")
        (ert-info ("disabled" :prefix "symbol-pos: ")
          (let ((eq-fn (pcase-exhaustive mode
                         ('interpreted without-pos-eq)
                         ('compiled    without-pos-eq-compiled))))
            (should (equal (funcall eq-fn 'sym 'sym) t))
            (should (equal (funcall eq-fn sym-with-pos1 'sym) nil))
            (should (equal (funcall eq-fn 'sym sym-with-pos1) nil))
            (should (equal (funcall eq-fn sym-with-pos1 sym-with-pos1) t))
            (should (equal (funcall eq-fn sym-with-pos1 sym-with-pos2) nil))))
        (ert-info ("enabled" :prefix "symbol-pos: ")
          (let ((eq-fn (pcase-exhaustive mode
                         ('interpreted with-pos-eq)
                         ('compiled    with-pos-eq-compiled))))
            (should (equal (funcall eq-fn 'sym 'sym) t))
            (should (equal (funcall eq-fn sym-with-pos1 'sym) t))
            (should (equal (funcall eq-fn 'sym sym-with-pos1) t))
            (should (equal (funcall eq-fn sym-with-pos1 sym-with-pos1) t))
            (should (equal (funcall eq-fn sym-with-pos1 sym-with-pos2) t))))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'bytecomp-tests)
;;; bytecomp-tests.el ends here
