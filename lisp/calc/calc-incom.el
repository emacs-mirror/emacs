;;; calc-incom.el --- complex data type input functions for Calc

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

;;; Incomplete forms.

(defun calc-begin-complex ()
  (interactive)
  (calc-wrapper
   (if (or calc-algebraic-mode calc-incomplete-algebraic-mode)
       (calc-alg-entry "(")
     (calc-push (list 'incomplete calc-complex-mode)))))

(defun calc-end-complex ()
  (interactive)
  (calc-comma t)
  (calc-wrapper
   (let ((top (calc-top 1)))
     (if (and (eq (car-safe top) 'incomplete)
	      (eq (nth 1 top) 'intv))
	 (progn
	   (if (< (length top) 4)
	       (setq top (append top '((neg (var inf var-inf))))))
	   (if (< (length top) 5)
	       (setq top (append top '((var inf var-inf)))))
	   (calc-enter-result 1 "..)" (cdr top)))
       (if (not (and (eq (car-safe top) 'incomplete)
		     (memq (nth 1 top) '(cplx polar))))
	   (error "Not entering a complex number"))
       (while (< (length top) 4)
	 (setq top (append top '(0))))
       (if (not (and (math-realp (nth 2 top))
		     (math-anglep (nth 3 top))))
	   (error "Components must be real"))
       (calc-enter-result 1 "()" (cdr top))))))

(defun calc-begin-vector ()
  (interactive)
  (calc-wrapper
   (if (or calc-algebraic-mode calc-incomplete-algebraic-mode)
       (calc-alg-entry "[")
     (calc-push '(incomplete vec)))))

(defun calc-end-vector ()
  (interactive)
  (calc-comma t)
  (calc-wrapper
   (let ((top (calc-top 1)))
     (if (and (eq (car-safe top) 'incomplete)
	      (eq (nth 1 top) 'intv))
	 (progn
	   (if (< (length top) 4)
	       (setq top (append top '((neg (var inf var-inf))))))
	   (if (< (length top) 5)
	       (setq top (append top '((var inf var-inf)))))
	   (setcar (cdr (cdr top)) (1+ (nth 2 top)))
	   (calc-enter-result 1 "..]" (cdr top)))
       (if (not (and (eq (car-safe top) 'incomplete)
		     (eq (nth 1 top) 'vec)))
	   (error "Not entering a vector"))
       (calc-pop-push-record 1 "[]" (cdr top))))))

(defun calc-comma (&optional allow-polar)
  (interactive)
  (calc-wrapper
   (let ((num (calc-find-first-incomplete
	       (nthcdr calc-stack-top calc-stack) 1)))
     (if (= num 0)
	 (error "Not entering a vector or complex number"))
     (let* ((inc (calc-top num))
	    (stuff (calc-top-list (1- num)))
	    (new (append inc stuff)))
       (if (and (null stuff)
		(not allow-polar)
		(or (eq (nth 1 inc) 'vec)
		    (< (length new) 4)))
	   (setq new (append new
			     (if (= (length new) 2)
				 '(0)
			       (nthcdr (1- (length new)) new)))))
       (or allow-polar
	   (if (eq (nth 1 new) 'polar)
	       (setq new (append '(incomplete cplx) (cdr (cdr new))))
	     (if (eq (nth 1 new) 'intv)
		 (setq new (append '(incomplete cplx)
				   (cdr (cdr (cdr new))))))))
       (if (and (memq (nth 1 new) '(cplx polar))
		(> (length new) 4))
	   (error "Too many components in complex number"))
       (if (and (eq (nth 1 new) 'intv)
		(> (length new) 5))
	   (error "Too many components in interval form"))
       (calc-pop-push num new)))))

(defun calc-semi ()
  (interactive)
  (calc-wrapper
   (let ((num (calc-find-first-incomplete
	       (nthcdr calc-stack-top calc-stack) 1)))
     (if (= num 0)
	 (error "Not entering a vector or complex number"))
     (let ((inc (calc-top num))
	   (stuff (calc-top-list (1- num))))
       (if (eq (nth 1 inc) 'cplx)
	   (setq inc (append '(incomplete polar) (cdr (cdr inc))))
	 (if (eq (nth 1 inc) 'intv)
	     (setq inc (append '(incomplete polar) (cdr (cdr (cdr inc)))))))
       (cond ((eq (nth 1 inc) 'polar)
	      (let ((new (append inc stuff)))
		(if (> (length new) 4)
		    (error "Too many components in complex number")
		  (if (= (length new) 2)
		      (setq new (append new '(1)))))
		(calc-pop-push num new)))
	     ((null stuff)
	      (if (> (length inc) 2)
		  (if (math-vectorp (nth 2 inc))
		      (calc-comma)
		    (calc-pop-push 1
				   (list 'incomplete 'vec (cdr (cdr inc)))
				   (list 'incomplete 'vec)))))
	     ((math-vectorp (car stuff))
	      (calc-comma))
	     ((eq (car-safe (car-safe (nth (+ num calc-stack-top)
					   calc-stack))) 'incomplete)
	      (calc-end-vector)
	      (calc-comma)
	      (let ((calc-algebraic-mode nil)
		    (calc-incomplete-algebraic-mode nil))
		(calc-begin-vector)))
	     ((or (= (length inc) 2)
		  (math-vectorp (nth 2 inc)))
	      (calc-pop-push num
			     (append inc (list (cons 'vec stuff)))
			     (list 'incomplete 'vec)))
	     (t
	      (calc-pop-push num
			     (list 'incomplete 'vec
				   (cons 'vec (append (cdr (cdr inc)) stuff)))
			     (list 'incomplete 'vec))))))))

;; The following variables are initially declared in calc.el,
;; but are used by calc-digit-dots.
(defvar calc-prev-char)
(defvar calc-prev-prev-char)
(defvar calc-digit-value)

(defun calc-digit-dots ()
  (if (eq calc-prev-char ?.)
      (progn
	(delete-backward-char 1)
	(if (calc-minibuffer-contains ".*\\.\\'")
	    (delete-backward-char 1))
	(setq calc-prev-char 'dots
	      last-command-char 32)
	(if calc-prev-prev-char
	    (calcDigit-nondigit)
	  (setq calc-digit-value nil)
          (let ((inhibit-read-only t))
            (erase-buffer))
	  (exit-minibuffer)))
    ;; just ignore extra decimal point, anticipating ".."
    (delete-backward-char 1)))

(defun calc-dots ()
  (interactive)
  (calc-wrapper
   (let ((num (calc-find-first-incomplete
	       (nthcdr calc-stack-top calc-stack) 1)))
     (if (= num 0)
	 (error "Not entering an interval form"))
     (let* ((inc (calc-top num))
	    (stuff (calc-top-list (1- num)))
	    (new (append inc stuff)))
       (if (not (eq (nth 1 new) 'intv))
	   (setq new (append '(incomplete intv)
			     (if (eq (nth 1 new) 'vec) '(2) '(0))
			     (cdr (cdr new)))))
       (if (and (null stuff)
		(= (length new) 3))
	   (setq new (append new '((neg (var inf var-inf))))))
       (if (> (length new) 5)
	   (error "Too many components in interval form"))
       (calc-pop-push num new)))))

(defun calc-find-first-incomplete (stack n)
  (cond ((null stack)
	 0)
	((eq (car-safe (car-safe (car stack))) 'incomplete)
	 n)
	(t
	 (calc-find-first-incomplete (cdr stack) (1+ n)))))

(defun calc-incomplete-error (a)
  (cond ((memq (nth 1 a) '(cplx polar))
	 (error "Complex number is incomplete"))
	((eq (nth 1 a) 'vec)
	 (error "Vector is incomplete"))
	((eq (nth 1 a) 'intv)
	 (error "Interval form is incomplete"))
	(t (error "Object is incomplete"))))

(provide 'calc-incom)

;;; arch-tag: b8001270-4dc7-481b-a3e3-a952e19b390d
;;; calc-incom.el ends here
