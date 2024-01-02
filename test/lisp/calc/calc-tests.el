;;; calc-tests.el --- tests for calc                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2024 Free Software Foundation, Inc.

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
(require 'calc-forms)

;; XXX The order in which calc libraries (in particular calc-units)
;; are loaded influences whether a calc integer in an expression
;; involving units is represented as a lisp integer or a calc float,
;; see bug#19582.  Until this will be fixed the following function can
;; be used to compare such calc expressions.
(defun calc-tests-equal (a b)
  "Like `equal' but allow for different representations of numbers.
For example: (calc-tests-equal 10 \\='(float 1 1)) => t.
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
The result is a calc (i.e., Lisp) expression, not its string representation.
Also pop the entire stack afterwards.
An existing calc stack is reused, otherwise a new one is created."
  (calc-eval string 'push)
  (prog1
      (ignore-errors
	(apply fun args)
	(calc-top-n 1))
    (calc-pop 0)))

(ert-deftest calc-remove-units ()
  (should (calc-tests-equal (calc-tests-simple #'calc-remove-units "-1 m") -1)))

(ert-deftest calc-extract-units ()
  (let ((calc-display-working-message nil))
    (should (calc-tests-equal (calc-tests-simple #'calc-extract-units "-1 m")
			      '(var m var-m)))
    (should (calc-tests-equal (calc-tests-simple #'calc-extract-units "-1 m*cm")
			      '(* (float 1 -2) (^ (var m var-m) 2))))))

(ert-deftest calc-convert-units ()
  (let ((calc-display-working-message nil))
    ;; Used to ask `(The expression is unitless when simplified) Old Units: '.
    (should (calc-tests-equal (calc-tests-simple #'calc-convert-units "-1 m"
                                                 nil "cm")
			      '(* -100 (var cm var-cm))))
    ;; Gave wrong result.
    (should (calc-tests-equal (calc-tests-simple #'calc-convert-units "-1 m"
					         (math-read-expr "1m") "cm")
			      '(* -100 (var cm var-cm))))))

(ert-deftest calc-imaginary-i ()
  "Test `math-imaginary-i' for non-special-const values."
  (let ((var-i (calcFunc-polar (calcFunc-sqrt -1))))
    (should (math-imaginary-i)))
  (let ((var-i (calcFunc-sqrt -1)))
    (should (math-imaginary-i))))

(ert-deftest calc-bug-23889 ()
  "Test for https://debbugs.gnu.org/23889 and 25652."
  (skip-unless t) ;; (>= math-bignum-digit-length 9))
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

(ert-deftest calc-trig ()
  "Trigonometric simplification; bug#33052."
  (let ((calc-angle-mode 'rad))
    (let ((calc-symbolic-mode t))
      (should (equal (math-simplify '(calcFunc-sin (/ (var pi var-pi) 4)))
                     '(/ (calcFunc-sqrt 2) 2)))
      (should (equal (math-simplify '(calcFunc-cos (/ (var pi var-pi) 4)))
                     '(/ (calcFunc-sqrt 2) 2)))
      (should (equal (math-simplify '(calcFunc-sec (/ (var pi var-pi) 4)))
                     '(calcFunc-sqrt 2)))
      (should (equal (math-simplify '(calcFunc-csc (/ (var pi var-pi) 4)))
                     '(calcFunc-sqrt 2)))
      (should (equal (math-simplify '(calcFunc-tan (/ (var pi var-pi) 3)))
                     '(calcFunc-sqrt 3)))
      (should (equal (math-simplify '(calcFunc-cot (/ (var pi var-pi) 3)))
                     '(/ (calcFunc-sqrt 3) 3))))
    (let ((calc-symbolic-mode nil))
      (should (equal (math-simplify '(calcFunc-sin (/ (var pi var-pi) 4)))
                     '(calcFunc-sin (/ (var pi var-pi) 4))))
      (should (equal (math-simplify '(calcFunc-cos (/ (var pi var-pi) 4)))
                     '(calcFunc-cos (/ (var pi var-pi) 4))))
      (should (equal (math-simplify '(calcFunc-sec (/ (var pi var-pi) 4)))
                     '(calcFunc-sec (/ (var pi var-pi) 4))))
      (should (equal (math-simplify '(calcFunc-csc (/ (var pi var-pi) 4)))
                     '(calcFunc-csc (/ (var pi var-pi) 4))))
      (should (equal (math-simplify '(calcFunc-tan (/ (var pi var-pi) 3)))
                     '(calcFunc-tan (/ (var pi var-pi) 3))))
      (should (equal (math-simplify '(calcFunc-cot (/ (var pi var-pi) 3)))
                     '(calcFunc-cot (/ (var pi var-pi) 3)))))))

(ert-deftest calc-format-radix ()
  "Test integer formatting (bug#36689)."
  (let ((calc-group-digits nil))
    (let ((calc-number-radix 10))
      (should (equal (math-format-number 12345678901) "12345678901")))
    (let ((calc-number-radix 2))
      (should (equal (math-format-number 12345) "2#11000000111001")))
    (let ((calc-number-radix 8))
      (should (equal (math-format-number 12345678901) "8#133767016065")))
    (let ((calc-number-radix 16))
      (should (equal (math-format-number 12345678901) "16#2DFDC1C35")))
    (let ((calc-number-radix 36))
      (should (equal (math-format-number 12345678901) "36#5O6AQT1"))))
  (let ((calc-group-digits t))
    (let ((calc-number-radix 10))
      (should (equal (math-format-number 12345678901) "12,345,678,901")))
    (let ((calc-number-radix 2))
      (should (equal (math-format-number 12345) "2#11,0000,0011,1001")))
    (let ((calc-number-radix 8))
      (should (equal (math-format-number 12345678901) "8#133,767,016,065")))
    (let ((calc-number-radix 16))
      (should (equal (math-format-number 12345678901) "16#2,DFDC,1C35")))
    (let ((calc-number-radix 36))
      (should (equal (math-format-number 12345678901) "36#5,O6A,QT1")))))

(ert-deftest calc-digit-after-point ()
  "Test display of trailing 0 after decimal point (bug#47302)."
  (let ((calc-digit-after-point nil))
    ;; Integral floats have no digits after the decimal point (default).
    (should (equal (math-format-number '(float 0 0)) "0."))
    (should (equal (math-format-number '(float 5 0)) "5."))
    (should (equal (math-format-number '(float 3 1)) "30."))
    (should (equal (math-format-number '(float 23 0)) "23."))
    (should (equal (math-format-number '(float 123 0)) "123."))
    (should (equal (math-format-number '(float 1 -1)) "0.1"))
    (should (equal (math-format-number '(float 54 -1)) "5.4"))
    (should (equal (math-format-number '(float 1 -4)) "1e-4"))
    (should (equal (math-format-number '(float 1 14)) "1e14"))
    (should (equal (math-format-number 12) "12")))
  (let ((calc-digit-after-point t))
    ;; Integral floats have at least one digit after the decimal point.
    (should (equal (math-format-number '(float 0 0)) "0.0"))
    (should (equal (math-format-number '(float 5 0)) "5.0"))
    (should (equal (math-format-number '(float 3 1)) "30.0"))
    (should (equal (math-format-number '(float 23 0)) "23.0"))
    (should (equal (math-format-number '(float 123 0)) "123.0"))
    (should (equal (math-format-number '(float 1 -1)) "0.1"))
    (should (equal (math-format-number '(float 54 -1)) "5.4"))
    (should (equal (math-format-number '(float 1 -4)) "1e-4"))
    (should (equal (math-format-number '(float 1 14)) "1e14"))
    (should (equal (math-format-number 12) "12"))))

(ert-deftest calc-calendar ()
  "Test calendar conversions (bug#36822)."
  (should (equal (calcFunc-julian (math-parse-date "2019-07-27")) 2458692))
  (should (equal (math-parse-date "2019-07-27") '(date 737267)))
  (should (equal (calcFunc-julian '(date 0)) 1721425))
  (should (equal (math-date-to-gregorian-dt 1) '(1 1 1)))
  (should (equal (math-date-to-gregorian-dt 0) '(-1 12 31)))
  (should (equal (math-date-to-gregorian-dt -1721425) '(-4714 11 24)))
  (should (equal (math-absolute-from-gregorian-dt 2019 7 27) 737267))
  (should (equal (math-absolute-from-gregorian-dt 1 1 1) 1))
  (should (equal (math-absolute-from-gregorian-dt -1 12 31) 0))
  (should (equal (math-absolute-from-gregorian-dt -99 12 31) -35795))
  (should (equal (math-absolute-from-gregorian-dt -4714 11 24) -1721425))
  (should (equal (calcFunc-julian '(date -1721425)) 0))
  (should (equal (math-date-to-julian-dt 1) '(1 1 3)))
  (should (equal (math-date-to-julian-dt -1721425) '(-4713 1 1)))
  (should (equal (math-absolute-from-julian-dt 2019 1 1) 737073))
  (should (equal (math-absolute-from-julian-dt 1 1 3) 1))
  (should (equal (math-absolute-from-julian-dt -101 1 1) -36892))
  (should (equal (math-absolute-from-julian-dt -101 3 1) -36832))
  (should (equal (math-absolute-from-julian-dt -4713 1 1) -1721425)))

(ert-deftest calc-solve-linear-system ()
  "Test linear system solving (bug#35374)."
  ;;   x + y =   3
  ;;  2x - 3y = -4
  ;; with the unique solution x=1, y=2
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 3)
              (calcFunc-eq (- (* 2 (var x var-x)) (* 3 (var y var-y))) -4))
            '(vec (var x var-x) (var y var-y)))
           '(vec (calcFunc-eq (var x var-x) 1)
                 (calcFunc-eq (var y var-y) 2))))

  ;;  x + y = 1
  ;;  x + y = 2
  ;; has no solution
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 1)
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 2))
            '(vec (var x var-x) (var y var-y)))
           '(calcFunc-solve
             (vec
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 1)
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 2))
             (vec (var x var-x) (var y var-y)))))
  ;;   x - y = 1
  ;;   x + y = 1
  ;; with the unique solution x=1, y=0
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq (- (var x var-x) (var y var-y)) 1)
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 1))
            '(vec (var x var-x) (var y var-y)))
           '(vec (calcFunc-eq (var x var-x) 1)
                 (calcFunc-eq (var y var-y) 0))))
  ;;  2x - 3y +  z =  5
  ;;   x +  y - 2z =  0
  ;;  -x + 2y + 3z = -3
  ;; with the unique solution x=1, y=-1, z=0
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq
               (+ (- (* 2 (var x var-x)) (* 3 (var y var-y))) (var z var-z))
               5)
              (calcFunc-eq
               (- (+ (var x var-x) (var y var-y)) (* 2 (var z var-z)))
               0)
              (calcFunc-eq
               (+ (- (* 2 (var y var-y)) (var x var-x)) (* 3 (var z var-z)))
               -3))
            '(vec (var x var-x) (var y var-y) (var z var-z)))
           ;; The `float' forms in the result are just artifacts of Calc's
           ;; current solver; it should be fixed to produce exact (integral)
           ;; results in this case.
           '(vec (calcFunc-eq (var x var-x) (float 1 0))
                 (calcFunc-eq (var y var-y) (float -1 0))
                 (calcFunc-eq (var z var-z) 0))))
  ;;   x = y + 1
  ;;   x = y
  ;; has no solution
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq (var x var-x) (+ (var y var-y) 1))
              (calcFunc-eq (var x var-x) (var y var-y)))
            '(vec (var x var-x) (var y var-y)))
           '(calcFunc-solve
             (vec
              (calcFunc-eq (var x var-x) (+ (var y var-y) 1))
              (calcFunc-eq (var x var-x) (var y var-y)))
             (vec (var x var-x) (var y var-y)))))
  ;;  x + y + z = 6
  ;;  x + y     = 3
  ;;  x - y     = 1
  ;; with the unique solution x=2, y=1, z=3
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq (+ (+ (var x var-x) (var y var-y)) (var z var-z)) 6)
              (calcFunc-eq (+ (var x var-x) (var y var-y)) 3)
              (calcFunc-eq (- (var x var-x) (var y var-y)) 1))
            '(vec (var x var-x) (var y var-y) (var z var-z)))
           '(vec
             (calcFunc-eq (var x var-x) 2)
             (calcFunc-eq (var y var-y) 1)
             (calcFunc-eq (var z var-z) 3))))
  ;; x = 3
  ;; x + 4y^2 = 3                   (ok, so this one isn't linear)
  ;; with the unique (double) solution x=3, y=0
  (should (equal
           (calcFunc-solve
            '(vec
              (calcFunc-eq (var x var-x) 3)
              (calcFunc-eq (+ (var x var-x) (* 4 (^ (var y var-y) 2))) 3))
            '(vec (var x var-x) (var y var-y)))
           '(vec (calcFunc-eq (var x var-x) 3)
                 (calcFunc-eq (var y var-y) 0)))))

(ert-deftest calc-poly-div ()
  "Test polynomial division, and that the remainder is recorded in the trail."
  (with-current-buffer (calc-trail-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)

      (calc-eval "2x**3+1" 'push)
      (calc-eval "x**2+2x" 'push)
      (calc-poly-div nil)
      (let ((tos (calc-top-n 1))
            (trail (buffer-string)))
        (calc-pop 0)
        (should (equal tos '(- (* 2 (var x var-x)) 4)))
        (should (equal trail "pdiv 2 * x - 4\nprem 8 * x + 1\n"))))))

(ert-deftest calc-Math-integerp ()
  (should (Math-integerp -7))
  (should (Math-integerp (ash 1 65)))
  (should-not (Math-integerp '(float 1 0)))
  (should-not (Math-integerp nil))

  (should (Math-num-integerp -7))
  (should (Math-num-integerp (ash 1 65)))
  (should (Math-num-integerp '(float 1 0)))
  (should-not (Math-num-integerp nil)))

(ert-deftest calc-matrix-determinant ()
  (let ((calc-display-working-message nil))
    (should (equal (calcFunc-det '(vec (vec 3)))
                   3))
    (should (equal (calcFunc-det '(vec (vec 2 3) (vec 6 7)))
                   -4))
    (should (equal (calcFunc-det '(vec (vec 1 2 3) (vec 4 5 7) (vec 9 6 2)))
                   15))
    (should (equal (calcFunc-det '(vec (vec 0 5 7 3)
                                       (vec 0 0 2 0)
                                       (vec 1 2 3 4)
                                       (vec 0 0 0 3)))
                   30))
    (should (equal (calcFunc-det '(vec (vec (var a var-a))))
                   '(var a var-a)))
    (should (equal (calcFunc-det '(vec (vec 2 (var a var-a))
                                       (vec 7 (var a var-a))))
                   '(* -5 (var a var-a))))
    (should (equal (calcFunc-det '(vec (vec 1 0 0 0)
                                       (vec 0 1 0 0)
                                       (vec 0 0 0 1)
                                       (vec 0 0 (var a var-a) 0)))
                   '(neg (var a var-a))))))

(ert-deftest calc-gcd ()
  (should (equal (calcFunc-gcd 3 4) 1))
  (should (equal (calcFunc-gcd 12 15) 3))
  (should (equal (calcFunc-gcd -12 15) 3))
  (should (equal (calcFunc-gcd 12 -15) 3))
  (should (equal (calcFunc-gcd -12 -15) 3))
  (should (equal (calcFunc-gcd 0 5) 5))
  (should (equal (calcFunc-gcd 5 0) 5))
  (should (equal (calcFunc-gcd 0 -5) 5))
  (should (equal (calcFunc-gcd -5 0) 5))
  (should (equal (calcFunc-gcd 0 0) 0))
  (should (equal (calcFunc-gcd 0 '(var x var-x))
                 '(calcFunc-abs (var x var-x))))
  (should (equal (calcFunc-gcd '(var x var-x) 0)
                 '(calcFunc-abs (var x var-x)))))

(ert-deftest calc-sum-gcd ()
  ;; sum(gcd(0,n),n,-1,-1)
  (should (equal (math-simplify '(calcFunc-sum (calcFunc-gcd 0 (var n var-n))
                                               (var n var-n) -1 -1))
                 1))
  ;; sum(sum(gcd(n,k),k,-1,1),n,-1,1)
  (should (equal (math-simplify
                  '(calcFunc-sum
                    (calcFunc-sum (calcFunc-gcd (var n var-n) (var k var-k))
                                  (var k var-k) -1 1)
                    (var n var-n) -1 1))
                 8)))

(defun calc-tests--fac (n)
  (apply #'* (number-sequence 1 n)))

(defun calc-tests--choose (n k)
  "N choose K, reference implementation."
  (cond
   ((and (integerp n) (integerp k))
    (if (<= 0 n)
        (if (<= 0 k n)
            (/ (calc-tests--fac n)
               (* (calc-tests--fac k) (calc-tests--fac (- n k))))
          0)    ; 0≤n<k
      ;; n<0, n and k integers: use extension from M. J. Kronenburg
      (cond
       ((<= 0 k)
        (* (expt -1 k)
           (calc-tests--choose (+ (- n) k -1) k)))
       ((<= k n)
        (* (expt -1 (- n k))
           (calc-tests--choose (+ (- k) -1) (- n k))))
       (t  ; n<k<0
        0))))
   ((natnump k)
    ;; Generalization for any n, integral k≥0: use falling product
    (/ (apply '* (number-sequence n (- n (1- k)) -1))
       (calc-tests--fac k)))
   (t (error "Case not covered"))))

(defun calc-tests--calc-to-number (x)
  "Convert a Calc object to a Lisp number."
  (pcase x
    ((pred numberp) x)
    (`(frac ,p ,q) (/ (float p) q))
    (`(float ,m ,e) (* m (expt 10 e)))
    (_ (error "calc object not converted: %S" x))))

(ert-deftest calc-choose ()
  "Test computation of binomial coefficients (bug#16999)."
  (let ((calc-display-working-message nil))
    ;; Integral arguments
    (dolist (n (number-sequence -6 6))
      (dolist (k (number-sequence -6 6))
        (should (equal (calcFunc-choose n k)
                       (calc-tests--choose n k)))))

    ;; Fractional n, natural k
    (should (equal (calc-tests--calc-to-number
                    (calcFunc-choose '(frac 15 2) 3))
                   (calc-tests--choose 7.5 3)))

    (should (equal (calc-tests--calc-to-number
                    (calcFunc-choose '(frac 1 2) 2))
                   (calc-tests--choose 0.5 2)))

    (should (equal (calc-tests--calc-to-number
                    (calcFunc-choose '(frac -15 2) 3))
                   (calc-tests--choose -7.5 3)))))

(ert-deftest calc-business-days ()
  (cl-flet ((m (s) (math-parse-date s))
            (b+ (a b) (calcFunc-badd a b))
            (b- (a b) (calcFunc-bsub a b)))
    ;; Sanity check.
    (should (equal (m "2020-09-07") '(date 737675)))

    ;; Test with standard business days (Mon-Fri):
    (should (equal (b+ (m "2020-09-07") 1) (m "2020-09-08"))) ; Mon->Tue
    (should (equal (b+ (m "2020-09-08") 1) (m "2020-09-09"))) ; Tue->Wed
    (should (equal (b+ (m "2020-09-09") 1) (m "2020-09-10"))) ; Wed->Thu
    (should (equal (b+ (m "2020-09-10") 1) (m "2020-09-11"))) ; Thu->Fri
    (should (equal (b+ (m "2020-09-11") 1) (m "2020-09-14"))) ; Fri->Mon

    (should (equal (b+ (m "2020-09-07") 4) (m "2020-09-11"))) ; Mon->Fri
    (should (equal (b+ (m "2020-09-07") 6) (m "2020-09-15"))) ; Mon->Tue

    (should (equal (b+ (m "2020-09-12") 1) (m "2020-09-14"))) ; Sat->Mon
    (should (equal (b+ (m "2020-09-13") 1) (m "2020-09-14"))) ; Sun->Mon

    (should (equal (b- (m "2020-09-11") 1) (m "2020-09-10"))) ; Fri->Thu
    (should (equal (b- (m "2020-09-10") 1) (m "2020-09-09"))) ; Thu->Wed
    (should (equal (b- (m "2020-09-09") 1) (m "2020-09-08"))) ; Wed->Tue
    (should (equal (b- (m "2020-09-08") 1) (m "2020-09-07"))) ; Tue->Mon
    (should (equal (b- (m "2020-09-07") 1) (m "2020-09-04"))) ; Mon->Fri

    (should (equal (b- (m "2020-09-11") 4) (m "2020-09-07"))) ; Fri->Mon
    (should (equal (b- (m "2020-09-15") 6) (m "2020-09-07"))) ; Tue->Mon

    (should (equal (b- (m "2020-09-12") 1) (m "2020-09-11"))) ; Sat->Fri
    (should (equal (b- (m "2020-09-13") 1) (m "2020-09-11"))) ; Sun->Fri

    ;; Stepping fractional days
    (should (equal (b+ (m "2020-09-08 21:00") '(frac 1 2))
                   (m "2020-09-09 09:00")))
    (should (equal (b+ (m "2020-09-11 21:00") '(frac 1 2))
                   (m "2020-09-14 09:00")))
    (should (equal (b- (m "2020-09-08 21:00") '(frac 1 2))
                   (m "2020-09-08 09:00")))
    (should (equal (b- (m "2020-09-14 06:00") '(frac 1 2))
                   (m "2020-09-11 18:00")))

    ;; Test with a couple of extra days off:
    (let ((var-Holidays (list 'vec
                              '(var sat var-sat) '(var sun var-sun)
                              (m "2020-09-09") (m "2020-09-11"))))

      (should (equal (b+ (m "2020-09-07") 1) (m "2020-09-08"))) ; Mon->Tue
      (should (equal (b+ (m "2020-09-08") 1) (m "2020-09-10"))) ; Tue->Thu
      (should (equal (b+ (m "2020-09-10") 1) (m "2020-09-14"))) ; Thu->Mon
      (should (equal (b+ (m "2020-09-14") 1) (m "2020-09-15"))) ; Mon->Tue
      (should (equal (b+ (m "2020-09-15") 1) (m "2020-09-16"))) ; Tue->Wed

      (should (equal (b- (m "2020-09-16") 1) (m "2020-09-15"))) ; Wed->Tue
      (should (equal (b- (m "2020-09-15") 1) (m "2020-09-14"))) ; Tue->Mon
      (should (equal (b- (m "2020-09-14") 1) (m "2020-09-10"))) ; Mon->Thu
      (should (equal (b- (m "2020-09-10") 1) (m "2020-09-08"))) ; Thu->Tue
      (should (equal (b- (m "2020-09-08") 1) (m "2020-09-07"))) ; Tue->Mon
      )

    ;; Test with odd non-business weekdays (Tue, Wed, Sat):
    (let ((var-Holidays '(vec (var tue var-tue)
                              (var wed var-wed)
                              (var sat var-sat))))
      (should (equal (b+ (m "2020-09-07") 1) (m "2020-09-10"))) ; Mon->Thu
      (should (equal (b+ (m "2020-09-10") 1) (m "2020-09-11"))) ; Thu->Fri
      (should (equal (b+ (m "2020-09-11") 1) (m "2020-09-13"))) ; Fri->Sun
      (should (equal (b+ (m "2020-09-13") 1) (m "2020-09-14"))) ; Sun->Mon

      (should (equal (b- (m "2020-09-14") 1) (m "2020-09-13"))) ; Mon->Sun
      (should (equal (b- (m "2020-09-13") 1) (m "2020-09-11"))) ; Sun->Fri
      (should (equal (b- (m "2020-09-11") 1) (m "2020-09-10"))) ; Fri->Thu
      (should (equal (b- (m "2020-09-10") 1) (m "2020-09-07"))) ; Thu->Mon
      )
  ))

(ert-deftest calc-unix-date ()
  (let* ((d-1970-01-01 (math-parse-date "1970-01-01"))
         (d-2020-09-07 (math-parse-date "2020-09-07"))
         (d-1991-01-09-0600 (math-parse-date "1991-01-09 06:00")))
    ;; calcFunc-unixtime (command "t U") converts a date value to Unix time,
    ;; and a number to a date.
    (should (equal d-1970-01-01 '(date 719163)))
    (should (equal (calcFunc-unixtime d-1970-01-01 0) 0))
    (should (equal (calc-tests--calc-to-number (cadr (calcFunc-unixtime 0 0)))
                   (cadr d-1970-01-01)))
    (should (equal (calcFunc-unixtime d-2020-09-07 0)
                   (* (- (cadr d-2020-09-07)
                         (cadr d-1970-01-01))
                      86400)))
    (should (equal (calcFunc-unixtime d-1991-01-09-0600 0)
                   663400800))
    (should (equal (calc-tests--calc-to-number
                    (cadr (calcFunc-unixtime 663400800 0)))
                   726841.25))

    (let ((calc-date-format '(U)))
      ;; Test parsing Unix time.
      (should (equal (calc-tests--calc-to-number
                      (cadr (math-parse-date "0")))
                     719163))
      (should (equal (calc-tests--calc-to-number
                      (cadr (math-parse-date "469324800")))
                     (+ 719163 (/ 469324800 86400))))
      (should (equal (calc-tests--calc-to-number
                      (cadr (math-parse-date "663400800")))
                     726841.25))

      ;; Test formatting Unix time.
      (should (equal (math-format-date d-1970-01-01) "0"))
      (should (equal (math-format-date d-2020-09-07)
                     (number-to-string (* (- (cadr d-2020-09-07)
                                             (cadr d-1970-01-01))
                                          86400))))
      (should (equal (math-format-date d-1991-01-09-0600) "663400800")))))

;; Reference implementations of bit operations:

(defun calc-tests--clip (x w)
  "Clip X to W bits, signed if W is negative, otherwise unsigned."
  (cond ((zerop w) x)
        ((> w 0) (logand x (- (ash 1 w) 1)))
        (t (let ((y (calc-tests--clip x (- w)))
                 (msb (ash 1 (- (- w) 1))))
             (- y (ash (logand y msb) 1))))))

(defun calc-tests--not (x w)
  "Bitwise complement of X, word size W."
  (calc-tests--clip (lognot x) w))

(defun calc-tests--and (x y w)
  "Bitwise AND of X and W, word size W."
  (calc-tests--clip (logand x y) w))

(defun calc-tests--or (x y w)
  "Bitwise OR of X and Y, word size W."
  (calc-tests--clip (logior x y) w))

(defun calc-tests--xor (x y w)
  "Bitwise XOR of X and Y, word size W."
  (calc-tests--clip (logxor x y) w))

(defun calc-tests--diff (x y w)
  "Bitwise AND of X and NOT Y, word size W."
  (calc-tests--clip (logand x (lognot y)) w))

(defun calc-tests--lsh (x n w)
  "Logical shift left X by N steps, word size W."
  (if (< n 0)
      (calc-tests--rsh x (- n) w)
    (calc-tests--clip (ash x n) w)))

(defun calc-tests--rsh (x n w)
  "Logical shift right X by N steps, word size W."
  (if (< n 0)
      (calc-tests--lsh x (- n) w)
    ;; First zero-extend, then shift.
    (calc-tests--clip
     (ash (calc-tests--clip x (abs w)) (- n))
     w)))

(defun calc-tests--ash (x n w)
  "Arithmetic shift left X by N steps, word size W."
  (if (< n 0)
      (calc-tests--rash x (- n) w)
    (calc-tests--clip (ash x n) w)))

(defun calc-tests--rash (x n w)
  "Arithmetic shift right X by N steps, word size W."
  (if (< n 0)
      (calc-tests--ash x (- n) w)
    ;; First sign-extend, then shift.
    (calc-tests--clip
     (ash (calc-tests--clip x (- (abs w))) (- n))
     w)))

(defun calc-tests--rot (x n w)
  "Rotate X left by N steps, word size W."
  (when (zerop w)
    (error "Undefined"))
  (let* ((aw (abs w))
         (y (calc-tests--clip x aw))
         (steps (mod n aw)))
    (calc-tests--clip (logior (ash y steps) (ash y (- steps aw)))
                      w)))

(ert-deftest calc-shift-binary ()
  (dolist (w '(16 32 -16 -32 0))
    (dolist (x '(0 1 #x1234 #x8000 #xabcd #xffff
                 #x12345678 #xabcdef12 #x80000000 #xffffffff
                 #x1234567890ab #x1234967890ab
                 -1 -14 #x-8000 #x-ffff #x-8001 #x-10000
                 #x-80000000 #x-ffffffff #x-80000001 #x-100000000))
      (dolist (n '(0 1 4 16 32 -1 -4 -16 -32))
        (should (equal (calcFunc-lsh x n w)
                       (calc-tests--lsh x n w)))
        (should (equal (calcFunc-rsh x n w)
                       (calc-tests--rsh x n w)))
        (should (equal (calcFunc-ash x n w)
                       (calc-tests--ash x n w)))
        (should (equal (calcFunc-rash x n w)
                       (calc-tests--rash x n w)))
        (unless (zerop w)
          (should (equal (calcFunc-rot x n w)
                         (calc-tests--rot x n w)))))))
  (should-error (calcFunc-rot 1 1 0)))

(ert-deftest calc-bit-ops ()
  (dolist (w '(16 32 -16 -32 0))
    (dolist (x '(0 1 #x1234 #x8000 #xabcd #xffff
                 #x12345678 #xabcdef12 #x80000000 #xffffffff
                 #x1234567890ab #x1234967890ab
                 -1 -14 #x-8000 #x-ffff #x-8001 #x-10000
                 #x-80000000 #x-ffffffff #x-80000001 #x-100000000))
      (should (equal (calcFunc-not x w)
                     (calc-tests--not x w)))

      (dolist (n '(0 1 4 16 32 -1 -4 -16 -32))
        (equal (calcFunc-clip x n)
               (calc-tests--clip x n)))

      (dolist (y '(0 1 #x1234 #x8000 #xabcd #xffff
                     #x12345678 #xabcdef12 #x80000000 #xffffffff
                     #x1234567890ab #x1234967890ab
                     -1 -14 #x-8000 #x-ffff #x-8001 #x-10000
                     #x-80000000 #x-ffffffff #x-80000001 #x-100000000))
        (should (equal (calcFunc-and x y w)
                       (calc-tests--and x y w)))
        (should (equal (calcFunc-or x y w)
                       (calc-tests--or x y w)))
        (should (equal (calcFunc-xor x y w)
                       (calc-tests--xor x y w)))
        (should (equal (calcFunc-diff x y w)
                       (calc-tests--diff x y w)))))))

(ert-deftest calc-latex-input ()
  ;; Check precedence of "/" in LaTeX input mode.
  (should (equal (math-read-exprs "a+b/c*d")
                 '((+ (var a var-a) (/ (var b var-b)
                                       (* (var c var-c) (var d var-d)))))))
  (unwind-protect
      (progn
        (calc-set-language 'latex)
        (should (equal (math-read-exprs "a+b/c*d")
                 '((+ (var a var-a) (/ (var b var-b)
                                       (* (var c var-c) (var d var-d)))))))
        (should (equal (math-read-exprs "a+b\\over c*d")
                       '((/ (+ (var a var-a) (var b var-b))
                            (* (var c var-c) (var d var-d))))))
        (should (equal (math-read-exprs "a/b/c")
                       '((/ (/ (var a var-a) (var b var-b))
                            (var c var-c))))))
    (calc-set-language nil)))

(defvar var-g)

;; Test `let'.
(defmath test1 (x)
  (let ((x (+ x 1))
        (y (+ x 3)))
    (let ((z (+ y 6)))
      (* x y z g))))

;; Test `let*'.
(defmath test2 (x)
  (let* ((y (+ x 1))
         (z (+ y 3)))
    (let* ((u (+ z 6)))
      (* x y z u g))))

;; Test `for'.
(defmath test3 (x)
  (let ((s 0))
    (for ((ii 1 x)
          (jj 1 ii))
      (setq s (+ s (* ii jj))))
    s))

;; Test `for' with non-unit stride.
(defmath test4 (x)
  (let ((l nil))
    (for ((ii 1 x 1)
          (jj 1 10 ii))
      (setq l ('cons jj l)))       ; Use Lisp `cons', not `calcFunc-cons'.
    (reverse l)))

;; Test `foreach'.
(defmath test5 (x)
  (let ((s 0))
    (foreach ((a x)
              (b a))
      (setq s (+ s b)))
    s))

;; Test `break'.
(defmath test6 (x)
  (let ((a (for ((ii 1 10))
             (when (= ii x)
               (break (* ii 2)))))
        (b (foreach ((e '(9 3 6)))
             (when (= e x)
               (break (- e 1))))))
    (* a b)))

;; Test `return' from `for'.
(defmath test7 (x)
  (for ((ii 1 10))
    (when (= ii x)
      (return (* ii 2))))
  5)

(ert-deftest calc-defmath ()
  (let ((var-g 17))
    (should (equal (calcFunc-test1 2) (* 3 5 11 17)))
    (should (equal (calcFunc-test2 2) (* 2 3 6 12 17))))
  (should (equal (calcFunc-test3 3)
                 (+ (* 1 1)
                    (* 2 1) (* 2 2)
                    (* 3 1) (* 3 2) (* 3 3))))
  (should (equal (calcFunc-test4 5)
                 '( 1 2 3 4 5 6 7 8 9 10
                    1 3 5 7 9
                    1 4 7 10
                    1 5 9
                    1 6)))
  (should (equal (calcFunc-test5 '((2 3) (5) (7 11 13)))
                 (+ 2 3 5 7 11 13)))
  (should (equal (calcFunc-test6 3) (* (* 3 2) (- 3 1))))
  (should (equal (calcFunc-test7 3) (* 3 2))))

(ert-deftest calc-nth-root ()
  ;; bug#51209
  (let* ((calc-display-working-message nil)
         (x (calc-tests--calc-to-number (math-pow 8 '(frac 1 6)))))
    (should (< (abs (- x (sqrt 2.0))) 1.0e-10))))

(provide 'calc-tests)
;;; calc-tests.el ends here
