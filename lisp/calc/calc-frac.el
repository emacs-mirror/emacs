;;; calc-frac.el --- fraction functions for Calc

;; Copyright (C) 1990, 1991, 1992, 1993, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <belanger@truman.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;;; Code:

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

(defun calc-fdiv (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op ":" 'calcFunc-fdiv arg 1)))


(defun calc-fraction (arg)
  (interactive "P")
  (calc-slow-wrapper
   (let ((func (if (calc-is-hyperbolic) 'calcFunc-frac 'calcFunc-pfrac)))
     (if (eq arg 0)
	 (calc-enter-result 2 "frac" (list func
					   (calc-top-n 2)
					   (calc-top-n 1)))
       (calc-enter-result 1 "frac" (list func
					 (calc-top-n 1)
					 (prefix-numeric-value (or arg 0))))))))


(defun calc-over-notation (fmt)
  (interactive "sFraction separator: ")
  (calc-wrapper
   (if (string-match "\\`\\([^ 0-9][^ 0-9]?\\)[0-9]*\\'" fmt)
       (let ((n nil))
	 (if (/= (match-end 0) (match-end 1))
	     (setq n (string-to-number (substring fmt (match-end 1)))
		   fmt (math-match-substring fmt 1)))
	 (if (eq n 0) (error "Bad denominator"))
	 (calc-change-mode 'calc-frac-format (list fmt n) t))
     (error "Bad fraction separator format"))))

(defun calc-slash-notation (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-frac-format (if n '("//" nil) '("/" nil)) t)))


(defun calc-frac-mode (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-prefer-frac n nil t)
   (message (if calc-prefer-frac
		"Integer division will now generate fractions"
	      "Integer division will now generate floating-point results"))))


;;;; Fractions.

;;; Build a normalized fraction.  [R I I]
;;; (This could probably be implemented more efficiently than using
;;;  the plain gcd algorithm.)
(defun math-make-frac (num den)
  (if (Math-integer-negp den)
      (setq num (math-neg num)
	    den (math-neg den)))
  (let ((gcd (math-gcd num den)))
    (if (eq gcd 1)
	(if (eq den 1)
	    num
	  (list 'frac num den))
      (if (equal gcd den)
	  (math-quotient num gcd)
	(list 'frac (math-quotient num gcd) (math-quotient den gcd))))))

(defun calc-add-fractions (a b)
  (if (eq (car-safe a) 'frac)
      (if (eq (car-safe b) 'frac)
	  (math-make-frac (math-add (math-mul (nth 1 a) (nth 2 b))
				    (math-mul (nth 2 a) (nth 1 b)))
			  (math-mul (nth 2 a) (nth 2 b)))
	(math-make-frac (math-add (nth 1 a)
				  (math-mul (nth 2 a) b))
			(nth 2 a)))
    (math-make-frac (math-add (math-mul a (nth 2 b))
			      (nth 1 b))
		    (nth 2 b))))

(defun calc-mul-fractions (a b)
  (if (eq (car-safe a) 'frac)
      (if (eq (car-safe b) 'frac)
	  (math-make-frac (math-mul (nth 1 a) (nth 1 b))
			  (math-mul (nth 2 a) (nth 2 b)))
	(math-make-frac (math-mul (nth 1 a) b)
			(nth 2 a)))
    (math-make-frac (math-mul a (nth 1 b))
		    (nth 2 b))))

(defun calc-div-fractions (a b)
  (if (eq (car-safe a) 'frac)
      (if (eq (car-safe b) 'frac)
	  (math-make-frac (math-mul (nth 1 a) (nth 2 b))
			  (math-mul (nth 2 a) (nth 1 b)))
	(math-make-frac (nth 1 a)
			(math-mul (nth 2 a) b)))
    (math-make-frac (math-mul a (nth 2 b))
		    (nth 1 b))))


;;; Convert a real value to fractional form.  [T R I; T R F] [Public]
(defun calcFunc-frac (a &optional tol)
  (or tol (setq tol 0))
  (cond ((Math-ratp a)
	 a)
	((memq (car a) '(cplx polar vec hms date sdev intv mod))
	 (cons (car a) (mapcar (function
				(lambda (x)
				  (calcFunc-frac x tol)))
			       (cdr a))))
	((Math-messy-integerp a)
	 (math-trunc a))
	((Math-negp a)
	 (math-neg (calcFunc-frac (math-neg a) tol)))
	((not (eq (car a) 'float))
	 (if (math-infinitep a)
	     a
	   (if (math-provably-integerp a)
	       a
	     (math-reject-arg a 'numberp))))
	((integerp tol)
	 (if (<= tol 0)
	     (setq tol (+ tol calc-internal-prec)))
	 (calcFunc-frac a (list 'float 5
				(- (+ (math-numdigs (nth 1 a))
				      (nth 2 a))
				   (1+ tol)))))
	((not (eq (car tol) 'float))
	 (if (Math-realp tol)
	     (calcFunc-frac a (math-float tol))
	   (math-reject-arg tol 'realp)))
	((Math-negp tol)
	 (calcFunc-frac a (math-neg tol)))
	((Math-zerop tol)
	 (calcFunc-frac a 0))
	((not (math-lessp-float tol '(float 1 0)))
	 (math-trunc a))
	((Math-zerop a)
	 0)
	(t
	 (let ((cfrac (math-continued-fraction a tol))
	       (calc-prefer-frac t))
	   (math-eval-continued-fraction cfrac)))))

(defun math-continued-fraction (a tol)
  (let ((calc-internal-prec (+ calc-internal-prec 2)))
    (let ((cfrac nil)
	  (aa a)
	  (calc-prefer-frac nil)
	  int)
      (while (or (null cfrac)
		 (and (not (Math-zerop aa))
		      (not (math-lessp-float
			    (math-abs
			     (math-sub a
				       (let ((f (math-eval-continued-fraction
						 cfrac)))
					 (math-working "Fractionalize" f)
					 f)))
			    tol))))
	(setq int (math-trunc aa)
	      aa (math-sub aa int)
	      cfrac (cons int cfrac))
	(or (Math-zerop aa)
	    (setq aa (math-div 1 aa))))
      cfrac)))

(defun math-eval-continued-fraction (cf)
  (let ((n (car cf))
	(d 1)
	temp)
    (while (setq cf (cdr cf))
      (setq temp (math-add (math-mul (car cf) n) d)
	    d n
	    n temp))
    (math-div n d)))



(defun calcFunc-fdiv (a b)   ; [R I I] [Public]
  (if (Math-num-integerp a)
      (if (Math-num-integerp b)
	  (if (Math-zerop b)
	      (math-reject-arg a "*Division by zero")
	    (math-make-frac (math-trunc a) (math-trunc b)))
	(math-reject-arg b 'integerp))
    (math-reject-arg a 'integerp)))

(provide 'calc-frac)

;;; arch-tag: 89d65274-0b3b-42d8-aacd-eaf86da5b4ea
;;; calc-frac.el ends here
