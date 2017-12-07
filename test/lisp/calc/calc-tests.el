;;; calc-tests.el --- tests for calc                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: maint

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

(require 'cl-lib)
(require 'ert)
(require 'calc)
(require 'calc-ext)
(require 'calc-units)

;; XXX The order in which calc libraries (in particular calc-units)
;; are loaded influences whether a calc integer in an expression
;; involving units is represented as a lisp integer or a calc float,
;; see bug#19582.  Until this will be fixed the following function can
;; be used to compare such calc expressions.
(defun calc-tests-equal (a b)
  "Like `equal' but allow for different representations of numbers.
For example: (calc-tests-equal 10 '(float 1 1)) => t.
A and B should be calc expressions."
  (cond ((math-numberp a)
	 (and (math-numberp b)
	      (math-equal a b)))
	((atom a)
	 (equal a b))
	((consp b)
	 ;; Can't be dotted or circular.
	 (and (= (length a) (length b))
	      (equal (car a) (car b))
	      (cl-every #'calc-tests-equal (cdr a) (cdr b))))))

(defun calc-tests-simple (fun string &rest args)
  "Push STRING on the calc stack, then call FUN and return the new top.
The result is a calc (i.e., lisp) expression, not its string representation.
Also pop the entire stack afterwards.
An existing calc stack is reused, otherwise a new one is created."
  (calc-eval string 'push)
  (prog1
      (ignore-errors
	(apply fun args)
	(calc-top-n 1))
    (calc-pop 0)))

(ert-deftest test-math-bignum ()
  ;; bug#17556
  (let ((n (math-bignum most-negative-fixnum)))
    (should (math-negp n))
    (should (cl-notany #'cl-minusp (cdr n)))))

(ert-deftest test-calc-remove-units ()
  (should (calc-tests-equal (calc-tests-simple #'calc-remove-units "-1 m") -1)))

(ert-deftest test-calc-extract-units ()
  (should (calc-tests-equal (calc-tests-simple #'calc-extract-units "-1 m")
			    '(var m var-m)))
  (should (calc-tests-equal (calc-tests-simple #'calc-extract-units "-1 m*cm")
			    '(* (float 1 -2) (^ (var m var-m) 2)))))

(ert-deftest test-calc-convert-units ()
  ;; Used to ask for `(The expression is unitless when simplified) Old Units: '.
  (should (calc-tests-equal (calc-tests-simple #'calc-convert-units "-1 m" nil "cm")
			    '(* -100 (var cm var-cm))))
  ;; Gave wrong result.
  (should (calc-tests-equal (calc-tests-simple #'calc-convert-units "-1 m"
					       (math-read-expr "1m") "cm")
			    '(* -100 (var cm var-cm)))))

(ert-deftest test-calc-23889 ()
  "Test for https://debbugs.gnu.org/23889 and 25652."
  (skip-unless (>= math-bignum-digit-length 9))
  (dolist (mode '(deg rad))
    (let ((calc-angle-mode mode))
      ;; If user inputs angle units, then should ignore `calc-angle-mode'.
      (should (string= "5253"
                       (substring
                        (number-to-string
                         (nth 1
                              (math-simplify-units
                               '(calcFunc-cos (* 45 (var rad var-rad))))))
                        0 4)))
      (should (string= "7071"
                       (substring
                        (number-to-string
                         (nth 1
                              (math-simplify-units
                               '(calcFunc-cos (* 45 (var deg var-deg))))))
                        0 4)))
      (should (string= "8939"
                       (substring
                        (number-to-string
                         (nth 1
                              (math-simplify-units
                               '(+ (calcFunc-sin (* 90 (var rad var-rad)))
                                   (calcFunc-cos (* 90 (var deg var-deg)))))))
                        0 4)))
      (should (string= "5519"
                       (substring
                        (number-to-string
                         (nth 1
                              (math-simplify-units
                               '(+ (calcFunc-sin (* 90 (var deg var-deg)))
                                   (calcFunc-cos (* 90 (var rad var-rad)))))))
                        0 4)))
      ;; If user doesn't input units, then must use `calc-angle-mode'.
      (should (string= (if (eq calc-angle-mode 'deg)
                           "9998"
                         "5403")
                       (substring
                        (number-to-string
                         (nth 1 (calcFunc-cos 1)))
                        0 4))))))

(provide 'calc-tests)
;;; calc-tests.el ends here

;; Local Variables:
;; bug-reference-url-format: "https://debbugs.gnu.org/%s"
;; End:
