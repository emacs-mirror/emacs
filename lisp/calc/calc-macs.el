;; Calculator for GNU Emacs, part I [calc-macs.el]
;; Copyright (C) 1990, 1991, 1992, 1993 Free Software Foundation, Inc.
;; Written by Dave Gillespie, daveg@synaptics.com.

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


(provide 'calc-macs)

(defun calc-need-macros () nil)


(defmacro calc-record-compilation-date-macro ()
  `(setq calc-installed-date ,(concat (current-time-string)
				      " by "
				      (user-full-name))))


(defmacro calc-wrapper (&rest body)
  (list 'calc-do (list 'function (append (list 'lambda ()) body)))
)

;; We use "point" here to generate slightly smaller byte-code than "t".
(defmacro calc-slow-wrapper (&rest body)
  (list 'calc-do (list 'function (append (list 'lambda ()) body)) (point))
)


(defmacro math-showing-full-precision (body)
  (list 'let
	'((calc-float-format calc-full-float-format))
	body)
)


(defmacro math-with-extra-prec (delta &rest body)
  (` (math-normalize
      (let ((calc-internal-prec (+ calc-internal-prec (, delta))))
	(,@ body))))
)


;;; Faster in-line version zerop, normalized values only.
(defmacro Math-zerop (a)   ; [P N]
  (` (if (consp (, a))
	 (and (not (memq (car (, a)) '(bigpos bigneg)))
	      (if (eq (car (, a)) 'float)
		  (eq (nth 1 (, a)) 0)
		(math-zerop (, a))))
       (eq (, a) 0)))
)

(defmacro Math-integer-negp (a)
  (` (if (consp (, a))
	 (eq (car (, a)) 'bigneg)
       (< (, a) 0)))
)

(defmacro Math-integer-posp (a)
  (` (if (consp (, a))
	 (eq (car (, a)) 'bigpos)
       (> (, a) 0)))
)


(defmacro Math-negp (a)
  (` (if (consp (, a))
	 (or (eq (car (, a)) 'bigneg)
	     (and (not (eq (car (, a)) 'bigpos))
		  (if (memq (car (, a)) '(frac float))
		      (Math-integer-negp (nth 1 (, a)))
		    (math-negp (, a)))))
       (< (, a) 0)))
)


(defmacro Math-looks-negp (a)   ; [P x] [Public]
  (` (or (Math-negp (, a))
	 (and (consp (, a)) (or (eq (car (, a)) 'neg)
				(and (memq (car (, a)) '(* /))
				     (or (math-looks-negp (nth 1 (, a)))
					 (math-looks-negp (nth 2 (, a)))))))))
)


(defmacro Math-posp (a)
  (` (if (consp (, a))
	 (or (eq (car (, a)) 'bigpos)
	     (and (not (eq (car (, a)) 'bigneg))
		  (if (memq (car (, a)) '(frac float))
		      (Math-integer-posp (nth 1 (, a)))
		    (math-posp (, a)))))
       (> (, a) 0)))
)


(defmacro Math-integerp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg))))
)


(defmacro Math-natnump (a)
  (` (if (consp (, a))
	 (eq (car (, a)) 'bigpos)
       (>= (, a) 0)))
)

(defmacro Math-ratp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg frac))))
)

(defmacro Math-realp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg frac float))))
)

(defmacro Math-anglep (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg frac float hms))))
)

(defmacro Math-numberp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg frac float cplx polar))))
)

(defmacro Math-scalarp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg frac float cplx polar hms))))
)

(defmacro Math-vectorp (a)
  (` (and (consp (, a)) (eq (car (, a)) 'vec)))
)

(defmacro Math-messy-integerp (a)
  (` (and (consp (, a))
	  (eq (car (, a)) 'float)
	  (>= (nth 2 (, a)) 0)))
)

(defmacro Math-objectp (a)    ;  [Public]
  (` (or (not (consp (, a)))
	 (memq (car (, a))
	       '(bigpos bigneg frac float cplx polar hms date sdev intv mod))))
)

(defmacro Math-objvecp (a)    ;  [Public]
  (` (or (not (consp (, a)))
	 (memq (car (, a))
	       '(bigpos bigneg frac float cplx polar hms date
			sdev intv mod vec))))
)


;;; Compute the negative of A.  [O O; o o] [Public]
(defmacro Math-integer-neg (a)
  (` (if (consp (, a))
	 (if (eq (car (, a)) 'bigpos)
	     (cons 'bigneg (cdr (, a)))
	   (cons 'bigpos (cdr (, a))))
       (- (, a))))
)


(defmacro Math-equal (a b)
  (` (= (math-compare (, a) (, b)) 0))
)

(defmacro Math-lessp (a b)
  (` (= (math-compare (, a) (, b)) -1))
)


(defmacro math-working (msg arg)    ; [Public]
  (` (if (eq calc-display-working-message 'lots)
	 (math-do-working (, msg) (, arg))))
)


(defmacro calc-with-default-simplification (body)
  (list 'let
	'((calc-simplify-mode (and (not (memq calc-simplify-mode '(none num)))
				   calc-simplify-mode)))
	body)
)


(defmacro Math-primp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg frac float cplx polar
				    hms date mod var))))
)


(defmacro calc-with-trail-buffer (&rest body)
  (` (let ((save-buf (current-buffer))
	   (calc-command-flags nil))
       (unwind-protect
	   (, (append '(progn
			 (set-buffer (calc-trail-display t))
			 (goto-char calc-trail-pointer))
		      body))
	 (set-buffer save-buf))))
)


(defmacro Math-num-integerp (a)
  (` (or (not (consp (, a)))
	 (memq (car (, a)) '(bigpos bigneg))
	 (and (eq (car (, a)) 'float)
	      (>= (nth 2 (, a)) 0))))
)


(defmacro Math-bignum-test (a)   ; [B N; B s; b b]
  (` (if (consp (, a))
	 (, a)
       (math-bignum (, a))))
)


(defmacro Math-equal-int (a b)
  (` (or (eq (, a) (, b))
	 (and (consp (, a))
	      (eq (car (, a)) 'float)
	      (eq (nth 1 (, a)) (, b))
	      (= (nth 2 (, a)) 0))))
)

(defmacro Math-natnum-lessp (a b)
  (` (if (consp (, a))
	 (and (consp (, b))
	      (= (math-compare-bignum (cdr (, a)) (cdr (, b))) -1))
       (or (consp (, b))
	   (< (, a) (, b)))))
)


(defmacro math-format-radix-digit (a)   ; [X D]
  (` (aref math-radix-digits (, a)))
)


