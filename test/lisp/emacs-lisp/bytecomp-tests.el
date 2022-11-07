;;; bytecomp-tests.el --- Tests for bytecomp.el  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2022 Free Software Foundation, Inc.

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
      (eval form lexical-binding)
    (error (list 'bytecomp-check-error (car err)))))

(defun bytecomp-tests--eval-compiled (form)
  "Evaluate FORM using the Lisp byte-code compiler, returning errors as a
special value."
  (let ((warning-minimum-log-level :emergency)
        (byte-compile-warnings nil))
    (condition-case err
	(funcall (byte-compile (list 'lambda nil form)))
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

(defun test-byte-comp-compile-and-load (compile &rest forms)
  (declare (indent 1))
  (let ((elfile nil)
        (elcfile nil))
    (unwind-protect
         (progn
           (setf elfile (make-temp-file "test-bytecomp" nil ".el"))
           (when compile
             (setf elcfile (make-temp-file "test-bytecomp" nil ".elc")))
           (with-temp-buffer
             (dolist (form forms)
               (print form (current-buffer)))
             (write-region (point-min) (point-max) elfile nil 'silent))
           (if compile
               (let ((byte-compile-dest-file-function
                      (lambda (e) elcfile)))
                 (byte-compile-file elfile)))
           (load elfile nil 'nomessage))
      (when elfile (delete-file elfile))
      (when elcfile (delete-file elcfile)))))

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

(defmacro bytecomp--with-warning-test (re-warning &rest form)
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create "*Compile-Log*")
     (let ((inhibit-read-only t)) (erase-buffer))
     (byte-compile ,@form)
     (ert-info ((prin1-to-string (buffer-string)) :prefix "buffer: ")
       (should (re-search-forward ,(string-replace " " "[ \n]+" re-warning))))))

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

(defmacro bytecomp--define-warning-file-test (file re-warning &optional reverse)
  `(ert-deftest ,(intern (format "bytecomp/%s" file)) ()
     (with-current-buffer (get-buffer-create "*Compile-Log*")
       (let ((inhibit-read-only t)) (erase-buffer))
       (byte-compile-file ,(ert-resource-file file))
       (ert-info ((buffer-string) :prefix "buffer: ")
         (,(if reverse 'should-not 'should)
          (re-search-forward ,(string-replace " " "[ \n]+" re-warning)
                             nil t))))))

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
                            "fails to specify type")

(bytecomp--define-warning-file-test "warn-defvar-lacks-prefix.el"
                            "var.*foo.*lacks a prefix")

(bytecomp--define-warning-file-test "warn-format.el"
                            "called with 2 args to fill 1 format field")

(bytecomp--define-warning-file-test "warn-free-setq.el"
                            "free.*foo")

(bytecomp--define-warning-file-test "warn-free-variable-reference.el"
                            "free variable .bar")

(bytecomp--define-warning-file-test "warn-make-variable-buffer-local.el"
                            "make-variable-buffer-local. not called at toplevel")

(bytecomp--define-warning-file-test "warn-interactive-only.el"
                            "next-line.*interactive use only.*forward-line")

(bytecomp--define-warning-file-test "warn-lambda-malformed-interactive-spec.el"
                            "malformed interactive spec")

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
                            "variable reference to constant")

(bytecomp--define-warning-file-test "warn-variable-set-nonvariable.el"
                            "variable reference to nonvariable")

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
 "wider than .* characters")

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
 "warn-wide-docstring-ignore-override.el"
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

(defmacro bytecomp-tests--with-temp-file (file-name-var &rest body)
  (declare (indent 1))
  (cl-check-type file-name-var symbol)
  `(let ((,file-name-var (make-temp-file "emacs")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,file-name-var)
       (let ((elc (concat ,file-name-var ".elc")))
         (if (file-exists-p elc) (delete-file elc))))))

(ert-deftest bytecomp-tests--unescaped-char-literals ()
  "Check that byte compiling warns about unescaped character
literals (Bug#20852)."
  (should (boundp 'lread--unescaped-character-literals))
  (let ((byte-compile-error-on-warn t)
        (byte-compile-debug t))
    (bytecomp-tests--with-temp-file source
      (write-region "(list ?) ?( ?; ?\" ?[ ?])" nil source)
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
      (write-region "(list 1 2 3)" nil source)
      (bytecomp-tests--with-temp-file destination
        (let ((byte-compile-dest-file-function (lambda (_) destination)))
          (should (byte-compile-file source)))))))

(ert-deftest bytecomp-tests-function-put ()
  "Check `function-put' operates during compilation."
  (bytecomp-tests--with-temp-file source
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
    (load source)
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
            (byte-compile-from-buffer (current-buffer)))
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
    (should (equal (byte-compile form)
                   (byte-compile
                    `(with-suppressed-warnings ,suppress ,form))))))

(ert-deftest bytecomp-test--with-suppressed-warnings ()
  (test-suppression
   '(defvar prefixless)
   '((lexical prefixless))
   "global/dynamic var .prefixless. lacks")

  (test-suppression
   '(defun foo()
      (let ((nil t))
        (message-mail)))
   '((constants nil))
   "Warning: attempt to let-bind constant .nil.")

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
   "Warning: wrong-params called with")

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
   '((mapcar mapcar))
   "Warning: .mapcar. called for effect")

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
   "Warning: Use .with-current-buffer. rather than"))

(ert-deftest bytecomp-tests--not-writable-directory ()
  "Test that byte compilation works if the output directory isn't
writable (Bug#44631)."
  (let ((directory (make-temp-file "bytecomp-tests-" :directory)))
    (unwind-protect
        (let* ((input-file (expand-file-name "test.el" directory))
               (output-file (expand-file-name "test.elc" directory))
               (byte-compile-dest-file-function
                (lambda (_) output-file))
               (byte-compile-error-on-warn t))
          (write-region "" nil input-file nil nil nil 'excl)
          (write-region "" nil output-file nil nil nil 'excl)
          (set-file-modes input-file #o400)
          (set-file-modes output-file #o200)
          (set-file-modes directory #o500)
          (should (byte-compile-file input-file))
          (should (file-regular-p output-file))
          (should (cl-plusp (file-attribute-size
                             (file-attributes output-file)))))
      (with-demoted-errors "Error cleaning up directory: %s"
        (set-file-modes directory #o700)
        (delete-directory directory :recursive)))))

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
    (let ((directory (make-temp-file "bytecomp-tests-" :directory)))
      (unwind-protect
          (let* ((input-file (expand-file-name "test.el" directory))
                 (output-file (expand-file-name "test.elc" directory))
                 (unquoted-file (file-name-unquote output-file))
                 (byte-compile-dest-file-function
                  (lambda (_) output-file))
                 (byte-compile-error-on-warn t))
            (should-not (file-remote-p input-file))
            (should-not (file-remote-p output-file))
            (write-region "" nil input-file nil nil nil 'excl)
            (write-region "" nil output-file nil nil nil 'excl)
            (set-file-modes input-file #o400)
            (set-file-modes output-file #o200)
            (set-file-modes directory #o500)
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
            (should (cl-plusp (file-attribute-size
                               (file-attributes output-file)))))
        (with-demoted-errors "Error cleaning up directory: %s"
          (set-file-modes directory #o700)
          (delete-directory directory :recursive))))))

(ert-deftest bytecomp-tests--target-file-no-directory ()
  "Check that Bug#45287 is fixed."
  (let ((directory (make-temp-file "bytecomp-tests-" :directory)))
    (unwind-protect
        (let* ((default-directory directory)
               (byte-compile-dest-file-function (lambda (_) "test.elc"))
               (byte-compile-error-on-warn t))
          (write-region "" nil "test.el" nil nil nil 'excl)
          (should (byte-compile-file "test.el"))
          (should (file-regular-p "test.elc"))
          (should (cl-plusp (file-attribute-size
                             (file-attributes "test.elc")))))
      (with-demoted-errors "Error cleaning up directory: %s"
        (delete-directory directory :recursive)))))

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
      (defun counter () (cl-incf x))
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
    (should (equal (funcall (byte-compile '(lambda (x) "foo")) 'dummy) "foo"))))

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
    (load-file (concat file "c"))
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


;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'bytecomp-tests)
;;; bytecomp-tests.el ends here
