;;; calc-bin.el --- binary functions for Calc

;; Copyright (C) 1990, 1991, 1992, 1993, 2001 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Colin Walters <walters@debian.org>

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

(defun calc-Need-calc-bin () nil)


;;; b-prefix binary commands.

(defun calc-and (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 2 "and"
		      (append '(calcFunc-and)
			      (calc-top-list-n 2)
			      (and n (list (prefix-numeric-value n)))))))

(defun calc-or (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 2 "or"
		      (append '(calcFunc-or)
			      (calc-top-list-n 2)
			      (and n (list (prefix-numeric-value n)))))))

(defun calc-xor (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 2 "xor"
		      (append '(calcFunc-xor)
			      (calc-top-list-n 2)
			      (and n (list (prefix-numeric-value n)))))))

(defun calc-diff (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 2 "diff"
		      (append '(calcFunc-diff)
			      (calc-top-list-n 2)
			      (and n (list (prefix-numeric-value n)))))))

(defun calc-not (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 1 "not"
		      (append '(calcFunc-not)
			      (calc-top-list-n 1)
			      (and n (list (prefix-numeric-value n)))))))

(defun calc-lshift-binary (n)
  (interactive "P")
  (calc-slow-wrapper
   (let ((hyp (if (calc-is-hyperbolic) 2 1)))
     (calc-enter-result hyp "lsh"
			(append '(calcFunc-lsh)
				(calc-top-list-n hyp)
				(and n (list (prefix-numeric-value n))))))))

(defun calc-rshift-binary (n)
  (interactive "P")
  (calc-slow-wrapper
   (let ((hyp (if (calc-is-hyperbolic) 2 1)))
     (calc-enter-result hyp "rsh"
			(append '(calcFunc-rsh)
				(calc-top-list-n hyp)
				(and n (list (prefix-numeric-value n))))))))

(defun calc-lshift-arith (n)
  (interactive "P")
  (calc-slow-wrapper
   (let ((hyp (if (calc-is-hyperbolic) 2 1)))
     (calc-enter-result hyp "ash"
			(append '(calcFunc-ash)
				(calc-top-list-n hyp)
				(and n (list (prefix-numeric-value n))))))))

(defun calc-rshift-arith (n)
  (interactive "P")
  (calc-slow-wrapper
   (let ((hyp (if (calc-is-hyperbolic) 2 1)))
     (calc-enter-result hyp "rash"
			(append '(calcFunc-rash)
				(calc-top-list-n hyp)
				(and n (list (prefix-numeric-value n))))))))

(defun calc-rotate-binary (n)
  (interactive "P")
  (calc-slow-wrapper
   (let ((hyp (if (calc-is-hyperbolic) 2 1)))
     (calc-enter-result hyp "rot"
			(append '(calcFunc-rot)
				(calc-top-list-n hyp)
				(and n (list (prefix-numeric-value n))))))))

(defun calc-clip (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 1 "clip"
		      (append '(calcFunc-clip)
			      (calc-top-list-n 1)
			      (and n (list (prefix-numeric-value n)))))))

(defun calc-word-size (n)
  (interactive "P")
  (calc-wrapper
   (or n (setq n (read-string (format "Binary word size: (default %d) "
				      calc-word-size))))
   (setq n (if (stringp n)
	       (if (equal n "")
		   calc-word-size
		 (if (string-match "\\`[-+]?[0-9]+\\'" n)
		     (string-to-int n)
		   (error "Expected an integer")))
	     (prefix-numeric-value n)))
   (or (= n calc-word-size)
       (if (> (math-abs n) 100)
	   (calc-change-mode 'calc-word-size n calc-leading-zeros)
	 (calc-change-mode '(calc-word-size calc-previous-modulo)
			   (list n (math-power-of-2 (math-abs n)))
			   calc-leading-zeros)))
   (if (< n 0)
       (message "Binary word size is %d bits (2's complement)" (- n))
     (message "Binary word size is %d bits" n))))





;;; d-prefix mode commands.

(defun calc-radix (n)
  (interactive "NDisplay radix (2-36): ")
  (calc-wrapper
   (if (and (>= n 2) (<= n 36))
       (progn
	 (calc-change-mode 'calc-number-radix n t)
	 ;; also change global value so minibuffer sees it
	 (setq-default calc-number-radix calc-number-radix))
     (setq n calc-number-radix))
   (message "Number radix is %d" n)))

(defun calc-decimal-radix ()
  (interactive)
  (calc-radix 10))

(defun calc-binary-radix ()
  (interactive)
  (calc-radix 2))

(defun calc-octal-radix ()
  (interactive)
  (calc-radix 8))

(defun calc-hex-radix ()
  (interactive)
  (calc-radix 16))

(defun calc-leading-zeros (n)
  (interactive "P")
  (calc-wrapper
   (if (calc-change-mode 'calc-leading-zeros n t t)
       (message "Zero-padding integers to %d digits (assuming radix %d)"
		(let* ((calc-internal-prec 6))
		  (math-compute-max-digits (math-abs calc-word-size)
					   calc-number-radix))
		calc-number-radix)
     (message "Omitting leading zeros on integers"))))


(defvar math-power-of-2-cache (list 1 2 4 8 16 32 64 128 256 512 1024))
(defvar math-big-power-of-2-cache nil)
(defun math-power-of-2 (n)    ;  [I I] [Public]
  (if (and (natnump n) (<= n 100))
      (or (nth n math-power-of-2-cache)
	  (let* ((i (length math-power-of-2-cache))
		 (val (nth (1- i) math-power-of-2-cache)))
	    (while (<= i n)
	      (setq val (math-mul val 2)
		    math-power-of-2-cache (nconc math-power-of-2-cache
						 (list val))
		    i (1+ i)))
	    val))
    (let ((found (assq n math-big-power-of-2-cache)))
      (if found
	  (cdr found)
	(let ((po2 (math-ipow 2 n)))
	  (setq math-big-power-of-2-cache
		(cons (cons n po2) math-big-power-of-2-cache))
	  po2)))))

(defun math-integer-log2 (n)    ; [I I] [Public]
  (let ((i 0)
	(p math-power-of-2-cache)
	val)
    (while (and p (Math-natnum-lessp (setq val (car p)) n))
      (setq p (cdr p)
	    i (1+ i)))
    (if p
	(and (equal val n)
	     i)
      (while (Math-natnum-lessp
	      (prog1
		  (setq val (math-mul val 2))
		(setq math-power-of-2-cache (nconc math-power-of-2-cache
						   (list val))))
	      n)
	(setq i (1+ i)))
      (and (equal val n)
	   i))))




;;; Bitwise operations.

(defun calcFunc-and (a b &optional w)   ; [I I I] [Public]
  (cond ((Math-messy-integerp w)
	 (calcFunc-and a b (math-trunc w)))
	((and w (not (integerp w)))
	 (math-reject-arg w 'fixnump))
	((and (integerp a) (integerp b))
	 (math-clip (logand a b) w))
	((or (eq (car-safe a) 'mod) (eq (car-safe b) 'mod))
	 (math-binary-modulo-args 'calcFunc-and a b w))
	((not (Math-num-integerp a))
	 (math-reject-arg a 'integerp))
	((not (Math-num-integerp b))
	 (math-reject-arg b 'integerp))
	(t (math-clip (cons 'bigpos
			    (math-and-bignum (math-binary-arg a w)
					     (math-binary-arg b w)))
		      w))))

(defun math-binary-arg (a w)
  (if (not (Math-integerp a))
      (setq a (math-trunc a)))
  (if (Math-integer-negp a)
      (math-not-bignum (cdr (math-bignum-test (math-sub -1 a)))
		       (math-abs (if w (math-trunc w) calc-word-size)))
    (cdr (Math-bignum-test a))))

(defun math-binary-modulo-args (f a b w)
  (let (mod)
    (if (eq (car-safe a) 'mod)
	(progn
	  (setq mod (nth 2 a)
		a (nth 1 a))
	  (if (eq (car-safe b) 'mod)
	      (if (equal mod (nth 2 b))
		  (setq b (nth 1 b))
		(math-reject-arg b "*Inconsistent modulos"))))
      (setq mod (nth 2 b)
	    b (nth 1 b)))
    (if (Math-messy-integerp mod)
	(setq mod (math-trunc mod))
      (or (Math-integerp mod)
	  (math-reject-arg mod 'integerp)))
    (let ((bits (math-integer-log2 mod)))
      (if bits
	  (if w
	      (if (/= w bits)
		  (calc-record-why
		   "*Warning: Modulo inconsistent with word size"))
	    (setq w bits))
	(calc-record-why "*Warning: Modulo is not a power of 2"))
      (math-make-mod (if b
			 (funcall f a b w)
		       (funcall f a w))
		     mod))))

(defun math-and-bignum (a b)   ; [l l l]
  (and a b
       (let ((qa (math-div-bignum-digit a 512))
	     (qb (math-div-bignum-digit b 512)))
	 (math-mul-bignum-digit (math-and-bignum (math-norm-bignum (car qa))
						  (math-norm-bignum (car qb)))
				 512
				 (logand (cdr qa) (cdr qb))))))

(defun calcFunc-or (a b &optional w)   ; [I I I] [Public]
  (cond ((Math-messy-integerp w)
	 (calcFunc-or a b (math-trunc w)))
	((and w (not (integerp w)))
	 (math-reject-arg w 'fixnump))
	((and (integerp a) (integerp b))
	 (math-clip (logior a b) w))
	((or (eq (car-safe a) 'mod) (eq (car-safe b) 'mod))
	 (math-binary-modulo-args 'calcFunc-or a b w))
	((not (Math-num-integerp a))
	 (math-reject-arg a 'integerp))
	((not (Math-num-integerp b))
	 (math-reject-arg b 'integerp))
	(t (math-clip (cons 'bigpos
			    (math-or-bignum (math-binary-arg a w)
					    (math-binary-arg b w)))
		      w))))

(defun math-or-bignum (a b)   ; [l l l]
  (and (or a b)
       (let ((qa (math-div-bignum-digit a 512))
	     (qb (math-div-bignum-digit b 512)))
	 (math-mul-bignum-digit (math-or-bignum (math-norm-bignum (car qa))
						 (math-norm-bignum (car qb)))
				 512
				 (logior (cdr qa) (cdr qb))))))

(defun calcFunc-xor (a b &optional w)   ; [I I I] [Public]
  (cond ((Math-messy-integerp w)
	 (calcFunc-xor a b (math-trunc w)))
	((and w (not (integerp w)))
	 (math-reject-arg w 'fixnump))
	((and (integerp a) (integerp b))
	 (math-clip (logxor a b) w))
	((or (eq (car-safe a) 'mod) (eq (car-safe b) 'mod))
	 (math-binary-modulo-args 'calcFunc-xor a b w))
	((not (Math-num-integerp a))
	 (math-reject-arg a 'integerp))
	((not (Math-num-integerp b))
	 (math-reject-arg b 'integerp))
	(t (math-clip (cons 'bigpos
			    (math-xor-bignum (math-binary-arg a w)
					     (math-binary-arg b w)))
		      w))))

(defun math-xor-bignum (a b)   ; [l l l]
  (and (or a b)
       (let ((qa (math-div-bignum-digit a 512))
	     (qb (math-div-bignum-digit b 512)))
	 (math-mul-bignum-digit (math-xor-bignum (math-norm-bignum (car qa))
						  (math-norm-bignum (car qb)))
				 512
				 (logxor (cdr qa) (cdr qb))))))

(defun calcFunc-diff (a b &optional w)   ; [I I I] [Public]
  (cond ((Math-messy-integerp w)
	 (calcFunc-diff a b (math-trunc w)))
	((and w (not (integerp w)))
	 (math-reject-arg w 'fixnump))
	((and (integerp a) (integerp b))
	 (math-clip (logand a (lognot b)) w))
	((or (eq (car-safe a) 'mod) (eq (car-safe b) 'mod))
	 (math-binary-modulo-args 'calcFunc-diff a b w))
	((not (Math-num-integerp a))
	 (math-reject-arg a 'integerp))
	((not (Math-num-integerp b))
	 (math-reject-arg b 'integerp))
	(t (math-clip (cons 'bigpos
			    (math-diff-bignum (math-binary-arg a w)
					      (math-binary-arg b w)))
		      w))))

(defun math-diff-bignum (a b)   ; [l l l]
  (and a
       (let ((qa (math-div-bignum-digit a 512))
	     (qb (math-div-bignum-digit b 512)))
	 (math-mul-bignum-digit (math-diff-bignum (math-norm-bignum (car qa))
						   (math-norm-bignum (car qb)))
				 512
				 (logand (cdr qa) (lognot (cdr qb)))))))

(defun calcFunc-not (a &optional w)   ; [I I] [Public]
  (cond ((Math-messy-integerp w)
	 (calcFunc-not a (math-trunc w)))
	((eq (car-safe a) 'mod)
	 (math-binary-modulo-args 'calcFunc-not a nil w))
	((and w (not (integerp w)))
	 (math-reject-arg w 'fixnump))
	((not (Math-num-integerp a))
	 (math-reject-arg a 'integerp))
	((< (or w (setq w calc-word-size)) 0)
	 (math-clip (calcFunc-not a (- w)) w))
	(t (math-normalize
	    (cons 'bigpos
		  (math-not-bignum (math-binary-arg a w)
				   w))))))

(defun math-not-bignum (a w)   ; [l l]
  (let ((q (math-div-bignum-digit a 512)))
    (if (<= w 9)
	(list (logand (lognot (cdr q))
		      (1- (lsh 1 w))))
      (math-mul-bignum-digit (math-not-bignum (math-norm-bignum (car q))
					       (- w 9))
			      512
			      (logxor (cdr q) 511)))))

(defun calcFunc-lsh (a &optional n w)   ; [I I] [Public]
  (setq a (math-trunc a)
	n (if n (math-trunc n) 1))
  (if (eq (car-safe a) 'mod)
      (math-binary-modulo-args 'calcFunc-lsh a n w)
    (setq w (if w (math-trunc w) calc-word-size))
    (or (integerp w)
	(math-reject-arg w 'fixnump))
    (or (Math-integerp a)
	(math-reject-arg a 'integerp))
    (or (Math-integerp n)
	(math-reject-arg n 'integerp))
    (if (< w 0)
	(math-clip (calcFunc-lsh a n (- w)) w)
      (if (Math-integer-negp a)
	  (setq a (math-clip a w)))
      (cond ((or (Math-lessp n (- w))
		 (Math-lessp w n))
	     0)
	    ((< n 0)
	     (math-quotient (math-clip a w) (math-power-of-2 (- n))))
	    (t
	     (math-clip (math-mul a (math-power-of-2 n)) w))))))

(defun calcFunc-rsh (a &optional n w)   ; [I I] [Public]
  (calcFunc-lsh a (math-neg (or n 1)) w))

(defun calcFunc-ash (a &optional n w)   ; [I I] [Public]
  (if (or (null n)
	  (not (Math-negp n)))
      (calcFunc-lsh a n w)
    (setq a (math-trunc a)
	  n (if n (math-trunc n) 1))
    (if (eq (car-safe a) 'mod)
	(math-binary-modulo-args 'calcFunc-ash a n w)
      (setq w (if w (math-trunc w) calc-word-size))
      (or (integerp w)
	  (math-reject-arg w 'fixnump))
      (or (Math-integerp a)
	  (math-reject-arg a 'integerp))
      (or (Math-integerp n)
	  (math-reject-arg n 'integerp))
      (if (< w 0)
	  (math-clip (calcFunc-ash a n (- w)) w)
	(if (Math-integer-negp a)
	    (setq a (math-clip a w)))
	(let ((two-to-sizem1 (math-power-of-2 (1- w)))
	      (sh (calcFunc-lsh a n w)))
	  (cond ((Math-natnum-lessp a two-to-sizem1)
		 sh)
		((Math-lessp n (- 1 w))
		 (math-add (math-mul two-to-sizem1 2) -1))
		(t (let ((two-to-n (math-power-of-2 (- n))))
		     (math-add (calcFunc-lsh (math-add two-to-n -1)
					     (+ w n) w)
			       sh)))))))))

(defun calcFunc-rash (a &optional n w)   ; [I I] [Public]
  (calcFunc-ash a (math-neg (or n 1)) w))

(defun calcFunc-rot (a &optional n w)   ; [I I] [Public]
  (setq a (math-trunc a)
	n (if n (math-trunc n) 1))
  (if (eq (car-safe a) 'mod)
      (math-binary-modulo-args 'calcFunc-rot a n w)
    (setq w (if w (math-trunc w) calc-word-size))
    (or (integerp w)
	(math-reject-arg w 'fixnump))
    (or (Math-integerp a)
	(math-reject-arg a 'integerp))
    (or (Math-integerp n)
	(math-reject-arg n 'integerp))
    (if (< w 0)
	(math-clip (calcFunc-rot a n (- w)) w)
      (if (Math-integer-negp a)
	  (setq a (math-clip a w)))
      (cond ((or (Math-integer-negp n)
		 (not (Math-natnum-lessp n w)))
	     (calcFunc-rot a (math-mod n w) w))
	    (t
	     (math-add (calcFunc-lsh a (- n w) w)
		       (calcFunc-lsh a n w)))))))

(defun math-clip (a &optional w)   ; [I I] [Public]
  (cond ((Math-messy-integerp w)
	 (math-clip a (math-trunc w)))
	((eq (car-safe a) 'mod)
	 (math-binary-modulo-args 'math-clip a nil w))
	((and w (not (integerp w)))
	 (math-reject-arg w 'fixnump))
	((not (Math-num-integerp a))
	 (math-reject-arg a 'integerp))
	((< (or w (setq w calc-word-size)) 0)
	 (setq a (math-clip a (- w)))
	 (if (Math-natnum-lessp a (math-power-of-2 (- -1 w)))
	     a
	   (math-sub a (math-power-of-2 (- w)))))
	((Math-negp a)
	 (math-normalize (cons 'bigpos (math-binary-arg a w))))
	((and (integerp a) (< a 1000000))
	 (if (>= w 20)
	     a
	   (logand a (1- (lsh 1 w)))))
	(t
	 (math-normalize
	  (cons 'bigpos
		(math-clip-bignum (cdr (math-bignum-test (math-trunc a)))
				  w))))))

(defalias 'calcFunc-clip 'math-clip)

(defun math-clip-bignum (a w)   ; [l l]
  (let ((q (math-div-bignum-digit a 512)))
    (if (<= w 9)
	(list (logand (cdr q)
		      (1- (lsh 1 w))))
      (math-mul-bignum-digit (math-clip-bignum (math-norm-bignum (car q))
						(- w 9))
			      512
			      (cdr q)))))

(defvar math-max-digits-cache nil)
(defun math-compute-max-digits (w r)
  (let* ((pair (+ (* r 100000) w))
	 (res (assq pair math-max-digits-cache)))
    (if res
	(cdr res)
      (let* ((calc-command-flags nil)
	     (digs (math-ceiling (math-div w (math-real-log2 r)))))
	(setq math-max-digits-cache (cons (cons pair digs)
					  math-max-digits-cache))
	digs))))

(defvar math-log2-cache (list '(2 . 1)
			      '(4 . 2)
			      '(8 . 3)
			      '(10 . (float 332193 -5))
			      '(16 . 4)
			      '(32 . 5)))
(defun math-real-log2 (x)   ;;; calc-internal-prec must be 6
  (let ((res (assq x math-log2-cache)))
    (if res
	(cdr res)
      (let* ((calc-symbolic-mode nil)
	     (calc-display-working-message nil)
	     (log (calcFunc-log x 2)))
	(setq math-log2-cache (cons (cons x log) math-log2-cache))
	log))))

(defconst math-radix-digits ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
			     "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
			     "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
			     "U" "V" "W" "X" "Y" "Z"])

(defsubst math-format-radix-digit (a)   ; [X D]
  (aref math-radix-digits a))

(defun math-format-radix (a)   ; [X S]
  (if (< a calc-number-radix)
      (if (< a 0)
	  (concat "-" (math-format-radix (- a)))
	(math-format-radix-digit a))
    (let ((s ""))
      (while (> a 0)
	(setq s (concat (math-format-radix-digit (% a calc-number-radix)) s)
	      a (/ a calc-number-radix)))
      s)))

(defconst math-binary-digits ["000" "001" "010" "011"
			      "100" "101" "110" "111"])
(defun math-format-binary (a)   ; [X S]
  (if (< a 8)
      (if (< a 0)
	  (concat "-" (math-format-binary (- a)))
	(math-format-radix a))
    (let ((s ""))
      (while (> a 7)
	(setq s (concat (aref math-binary-digits (% a 8)) s)
	      a (/ a 8)))
      (concat (math-format-radix a) s))))

(defun math-format-bignum-radix (a)   ; [X L]
  (cond ((null a) "0")
	((and (null (cdr a))
	      (< (car a) calc-number-radix))
	 (math-format-radix-digit (car a)))
	(t
	 (let ((q (math-div-bignum-digit a calc-number-radix)))
	   (concat (math-format-bignum-radix (math-norm-bignum (car q)))
		   (math-format-radix-digit (cdr q)))))))

(defun math-format-bignum-binary (a)   ; [X L]
  (cond ((null a) "0")
	((null (cdr a))
	 (math-format-binary (car a)))
	(t
	 (let ((q (math-div-bignum-digit a 512)))
	   (concat (math-format-bignum-binary (math-norm-bignum (car q)))
		   (aref math-binary-digits (/ (cdr q) 64))
		   (aref math-binary-digits (% (/ (cdr q) 8) 8))
		   (aref math-binary-digits (% (cdr q) 8)))))))

(defun math-format-bignum-octal (a)   ; [X L]
  (cond ((null a) "0")
	((null (cdr a))
	 (math-format-radix (car a)))
	(t
	 (let ((q (math-div-bignum-digit a 512)))
	   (concat (math-format-bignum-octal (math-norm-bignum (car q)))
		   (math-format-radix-digit (/ (cdr q) 64))
		   (math-format-radix-digit (% (/ (cdr q) 8) 8))
		   (math-format-radix-digit (% (cdr q) 8)))))))

(defun math-format-bignum-hex (a)   ; [X L]
  (cond ((null a) "0")
	((null (cdr a))
	 (math-format-radix (car a)))
	(t
	 (let ((q (math-div-bignum-digit a 256)))
	   (concat (math-format-bignum-hex (math-norm-bignum (car q)))
		   (math-format-radix-digit (/ (cdr q) 16))
		   (math-format-radix-digit (% (cdr q) 16)))))))

;;; Decompose into integer and fractional parts, without depending
;;; on calc-internal-prec.
(defun math-float-parts (a need-frac)    ; returns ( int frac fracdigs )
  (if (>= (nth 2 a) 0)
      (list (math-scale-rounding (nth 1 a) (nth 2 a)) '(float 0 0) 0)
    (let* ((d (math-numdigs (nth 1 a)))
	   (n (- (nth 2 a))))
      (if need-frac
	  (if (>= n d)
	      (list 0 a n)
	    (let ((qr (math-idivmod (nth 1 a) (math-scale-int 1 n))))
	      (list (car qr) (math-make-float (cdr qr) (- n)) n)))
	(list (math-scale-rounding (nth 1 a) (nth 2 a))
	      '(float 0 0) 0)))))

(defun math-format-radix-float (a prec)
  (let ((fmt (car calc-float-format))
	(figs (nth 1 calc-float-format))
	(point calc-point-char)
	(str nil))
    (if (eq fmt 'fix)
	(let* ((afigs (math-abs figs))
	       (fp (math-float-parts a (> afigs 0)))
	       (calc-internal-prec (+ 3 (max (nth 2 fp)
					     (math-convert-radix-digits
					      afigs t))))
	       (int (car fp))
	       (frac (math-round (math-mul (math-normalize (nth 1 fp))
					   (math-radix-float-power afigs)))))
	  (if (not (and (math-zerop frac) (math-zerop int) (< figs 0)))
	      (let ((math-radix-explicit-format nil))
		(let ((calc-group-digits nil))
		  (setq str (if (> afigs 0) (math-format-number frac) ""))
		  (if (< (length str) afigs)
		      (setq str (concat (make-string (- afigs (length str)) ?0)
					str))
		    (if (> (length str) afigs)
			(setq str (substring str 1)
			      int (math-add int 1))))
		  (setq str (concat (math-format-number int) point str)))
		(when calc-group-digits
		  (setq str (math-group-float str))))
	    (setq figs 0))))
    (or str
	(let* ((prec calc-internal-prec)
	       (afigs (if (> figs 0)
			  figs
			(max 1 (+ figs
				  (1- (math-convert-radix-digits
				       (max prec
					    (math-numdigs (nth 1 a)))))))))
	       (calc-internal-prec (+ 3 (math-convert-radix-digits afigs t)))
	       (explo -1) (vlo (math-radix-float-power explo))
	       (exphi 1) (vhi (math-radix-float-power exphi))
	       expmid vmid eadj)
	  (setq a (math-normalize a))
	  (if (Math-zerop a)
	      (setq explo 0)
	    (if (math-lessp-float '(float 1 0) a)
		(while (not (math-lessp-float a vhi))
		  (setq explo exphi vlo vhi
			exphi (math-mul exphi 2)
			vhi (math-radix-float-power exphi)))
	      (while (math-lessp-float a vlo)
		(setq exphi explo vhi vlo
		      explo (math-mul explo 2)
		      vlo (math-radix-float-power explo))))
	    (while (not (eq (math-sub exphi explo) 1))
	      (setq expmid (math-div2 (math-add explo exphi))
		    vmid (math-radix-float-power expmid))
	      (if (math-lessp-float a vmid)
		  (setq exphi expmid vhi vmid)
		(setq explo expmid vlo vmid)))
	    (setq a (math-div-float a vlo)))
	  (let* ((sc (math-round (math-mul a (math-radix-float-power
					      (1- afigs)))))
		 (math-radix-explicit-format nil))
	    (let ((calc-group-digits nil))
	      (setq str (math-format-number sc))))
	  (if (> (length str) afigs)
	      (setq str (substring str 0 -1)
		    explo (1+ explo)))
	  (if (and (eq fmt 'float)
		   (math-lessp explo (+ (if (= figs 0)
					    (1- (math-convert-radix-digits
						 prec))
					  afigs)
					calc-display-sci-high 1))
		   (math-lessp calc-display-sci-low explo))
	      (let ((dpos (1+ explo)))
		(cond ((<= dpos 0)
		       (setq str (concat "0" point (make-string (- dpos) ?0)
					 str)))
		      ((> dpos (length str))
		       (setq str (concat str (make-string (- dpos (length str))
							  ?0) point)))
		      (t
		       (setq str (concat (substring str 0 dpos) point
					 (substring str dpos)))))
		(setq explo nil))
	    (setq eadj (if (eq fmt 'eng)
			   (min (math-mod explo 3) (length str))
			 0)
		  str (concat (substring str 0 (1+ eadj)) point
			      (substring str (1+ eadj)))))
	  (setq pos (length str))
	  (while (eq (aref str (1- pos)) ?0) (setq pos (1- pos)))
	  (and explo (eq (aref str (1- pos)) ?.) (setq pos (1- pos)))
	  (setq str (substring str 0 pos))
	  (when calc-group-digits
	    (setq str (math-group-float str)))
	  (if explo
	      (let ((estr (let ((calc-number-radix 10)
				(calc-group-digits nil))
			    (setq estr (math-format-number
					(math-sub explo eadj))))))
		(setq str (if (or (memq calc-language '(math maple))
				  (> calc-number-radix 14))
			      (format "%s*%d.^%s" str calc-number-radix estr)
			    (format "%se%s" str estr)))))))
    str))

(defvar math-radix-digits-cache nil)

(defun math-convert-radix-digits (n &optional to-dec)
  (let ((key (cons n (cons to-dec calc-number-radix))))
    (or (cdr (assoc key math-radix-digits-cache))
	(let* ((calc-internal-prec 6)
	       (log (math-div (math-real-log2 calc-number-radix)
			      '(float 332193 -5))))
	  (cdr (car (setq math-radix-digits-cache
			  (cons (cons key (math-ceiling (if to-dec
							    (math-mul n log)
							  (math-div n log))))
				math-radix-digits-cache))))))))

(defvar math-radix-float-cache-tag nil)

(defun math-radix-float-power (n)
  (if (eq n 0)
      '(float 1 0)
    (or (and (eq calc-number-radix (car math-radix-float-cache-tag))
	     (<= calc-internal-prec (cdr math-radix-float-cache-tag)))
	(setq math-radix-float-cache-tag (cons calc-number-radix
					       calc-internal-prec)
	      math-radix-float-cache nil))
    (math-normalize
     (or (cdr (assoc n math-radix-float-cache))
	 (cdr (car (setq math-radix-float-cache
			 (cons (cons
				n
				(let ((calc-internal-prec
				       (cdr math-radix-float-cache-tag)))
				  (if (math-negp n)
				      (math-div-float '(float 1 0)
						      (math-radix-float-power
						       (math-neg n)))
				    (math-mul-float (math-sqr-float
						     (math-radix-float-power
						      (math-div2 n)))
						    (if (math-evenp n)
							'(float 1 0)
						      (math-float
						       calc-number-radix))))))
			       math-radix-float-cache))))))))


;;; calc-bin.el ends here
