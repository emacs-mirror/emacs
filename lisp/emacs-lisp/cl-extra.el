;;; cl-extra.el --- Common Lisp features, part 2  -*- lexical-binding: t -*-

;; Copyright (C) 1993, 2000-2018 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Keywords: extensions
;; Package: emacs

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

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains portions of the Common Lisp extensions
;; package which are autoloaded since they are relatively obscure.

;;; Code:

(require 'cl-lib)

;;; Type coercion.

;;;###autoload
(defun cl-coerce (x type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier.
\n(fn OBJECT TYPE)"
  (cond ((eq type 'list) (if (listp x) x (append x nil)))
	((eq type 'vector) (if (vectorp x) x (vconcat x)))
	((eq type 'string) (if (stringp x) x (concat x)))
	((eq type 'array) (if (arrayp x) x (vconcat x)))
	((and (eq type 'character) (stringp x) (= (length x) 1)) (aref x 0))
	((and (eq type 'character) (symbolp x))
         (cl-coerce (symbol-name x) type))
	((eq type 'float) (float x))
	((cl-typep x type) x)
	(t (error "Can't coerce %s to type %s" x type))))


;;; Predicates.

;;;###autoload
(defun cl-equalp (x y)
  "Return t if two Lisp objects have similar structures and contents.
This is like `equal', except that it accepts numerically equal
numbers of different types (float vs. integer), and also compares
strings case-insensitively."
  (cond ((eq x y) t)
	((stringp x)
	 (and (stringp y) (= (length x) (length y))
	      (or (string-equal x y)
		  (string-equal (downcase x) (downcase y))))) ;Lazy but simple!
	((numberp x)
	 (and (numberp y) (= x y)))
	((consp x)
	 (while (and (consp x) (consp y) (cl-equalp (car x) (car y)))
	   (setq x (cdr x) y (cdr y)))
	 (and (not (consp x)) (cl-equalp x y)))
	((vectorp x)
	 (and (vectorp y) (= (length x) (length y))
	      (let ((i (length x)))
		(while (and (>= (setq i (1- i)) 0)
			    (cl-equalp (aref x i) (aref y i))))
		(< i 0))))
	(t (equal x y))))


;;; Control structures.

;;;###autoload
(defun cl--mapcar-many (cl-func cl-seqs &optional acc)
  (if (cdr (cdr cl-seqs))
      (let* ((cl-res nil)
	     (cl-n (apply 'min (mapcar 'length cl-seqs)))
	     (cl-i 0)
	     (cl-args (copy-sequence cl-seqs))
	     cl-p1 cl-p2)
	(setq cl-seqs (copy-sequence cl-seqs))
	(while (< cl-i cl-n)
	  (setq cl-p1 cl-seqs cl-p2 cl-args)
	  (while cl-p1
	    (setcar cl-p2
		    (if (consp (car cl-p1))
			(prog1 (car (car cl-p1))
			  (setcar cl-p1 (cdr (car cl-p1))))
		      (aref (car cl-p1) cl-i)))
	    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)))
	  (if acc
	      (push (apply cl-func cl-args) cl-res)
	    (apply cl-func cl-args))
	  (setq cl-i (1+ cl-i)))
	(and acc (nreverse cl-res)))
    (let ((cl-res nil)
	  (cl-x (car cl-seqs))
	  (cl-y (nth 1 cl-seqs)))
      (let ((cl-n (min (length cl-x) (length cl-y)))
	    (cl-i -1))
	(while (< (setq cl-i (1+ cl-i)) cl-n)
	  (let ((val (funcall cl-func
			      (if (consp cl-x) (pop cl-x) (aref cl-x cl-i))
			      (if (consp cl-y) (pop cl-y) (aref cl-y cl-i)))))
	    (when acc
	      (push val cl-res)))))
	(and acc (nreverse cl-res)))))

;;;###autoload
(defun cl-map (cl-type cl-func cl-seq &rest cl-rest)
  "Map a FUNCTION across one or more SEQUENCEs, returning a sequence.
TYPE is the sequence type to return.
\n(fn TYPE FUNCTION SEQUENCE...)"
  (let ((cl-res (apply 'cl-mapcar cl-func cl-seq cl-rest)))
    (and cl-type (cl-coerce cl-res cl-type))))

;;;###autoload
(defun cl-maplist (cl-func cl-list &rest cl-rest)
  "Map FUNCTION to each sublist of LIST or LISTs.
Like `cl-mapcar', except applies to lists and their cdr's rather than to
the elements themselves.
\n(fn FUNCTION LIST...)"
  (if cl-rest
      (let ((cl-res nil)
	    (cl-args (cons cl-list (copy-sequence cl-rest)))
	    cl-p)
	(while (not (memq nil cl-args))
	  (push (apply cl-func cl-args) cl-res)
	  (setq cl-p cl-args)
	  (while cl-p (setcar cl-p (cdr (pop cl-p)))))
	(nreverse cl-res))
    (let ((cl-res nil))
      (while cl-list
	(push (funcall cl-func cl-list) cl-res)
	(setq cl-list (cdr cl-list)))
      (nreverse cl-res))))

;;;###autoload
(defun cl-mapc (cl-func cl-seq &rest cl-rest)
  "Like `cl-mapcar', but does not accumulate values returned by the function.
\n(fn FUNCTION SEQUENCE...)"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-seq) (nlistp (car cl-rest)))
          (progn
            (cl--mapcar-many cl-func (cons cl-seq cl-rest))
            cl-seq)
        (let ((cl-x cl-seq) (cl-y (car cl-rest)))
          (while (and cl-x cl-y)
            (funcall cl-func (pop cl-x) (pop cl-y)))
          cl-seq))
    (mapc cl-func cl-seq)))

;;;###autoload
(defun cl-mapl (cl-func cl-list &rest cl-rest)
  "Like `cl-maplist', but does not accumulate values returned by the function.
\n(fn FUNCTION LIST...)"
  (if cl-rest
      (let ((cl-args (cons cl-list (copy-sequence cl-rest)))
	    cl-p)
	(while (not (memq nil cl-args))
          (apply cl-func cl-args)
	  (setq cl-p cl-args)
	  (while cl-p (setcar cl-p (cdr (pop cl-p))))))
    (let ((cl-p cl-list))
      (while cl-p (funcall cl-func cl-p) (setq cl-p (cdr cl-p)))))
  cl-list)

;;;###autoload
(defun cl-mapcan (cl-func cl-seq &rest cl-rest)
  "Like `cl-mapcar', but nconc's together the values returned by the function.
\n(fn FUNCTION SEQUENCE...)"
  (if cl-rest
      (apply 'nconc (apply 'cl-mapcar cl-func cl-seq cl-rest))
    (mapcan cl-func cl-seq)))

;;;###autoload
(defun cl-mapcon (cl-func cl-list &rest cl-rest)
  "Like `cl-maplist', but nconc's together the values returned by the function.
\n(fn FUNCTION LIST...)"
  (apply 'nconc (apply 'cl-maplist cl-func cl-list cl-rest)))

;;;###autoload
(defun cl-some (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of any element of SEQ or SEQs.
If so, return the true (non-nil) value returned by PREDICATE.
\n(fn PREDICATE SEQ...)"
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-some
	(apply 'cl-map nil
	       (function (lambda (&rest cl-x)
			   (let ((cl-res (apply cl-pred cl-x)))
			     (if cl-res (throw 'cl-some cl-res)))))
	       cl-seq cl-rest) nil)
    (let ((cl-x nil))
      (while (and cl-seq (not (setq cl-x (funcall cl-pred (pop cl-seq))))))
      cl-x)))

;;;###autoload
(defun cl-every (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-every
	(apply 'cl-map nil
	       (function (lambda (&rest cl-x)
			   (or (apply cl-pred cl-x) (throw 'cl-every nil))))
	       cl-seq cl-rest) t)
    (while (and cl-seq (funcall cl-pred (car cl-seq)))
      (setq cl-seq (cdr cl-seq)))
    (null cl-seq)))

;;;###autoload
(defun cl-notany (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (not (apply 'cl-some cl-pred cl-seq cl-rest)))

;;;###autoload
(defun cl-notevery (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of some element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (not (apply 'cl-every cl-pred cl-seq cl-rest)))

;;;###autoload
(defun cl--map-keymap-recursively (cl-func-rec cl-map &optional cl-base)
  (or cl-base
      (setq cl-base (copy-sequence [0])))
  (map-keymap
   (function
    (lambda (cl-key cl-bind)
      (aset cl-base (1- (length cl-base)) cl-key)
      (if (keymapp cl-bind)
	  (cl--map-keymap-recursively
	   cl-func-rec cl-bind
	   (vconcat cl-base (list 0)))
	(funcall cl-func-rec cl-base cl-bind))))
   cl-map))

;;;###autoload
(defun cl--map-intervals (cl-func &optional cl-what cl-prop cl-start cl-end)
  (or cl-what (setq cl-what (current-buffer)))
  (if (bufferp cl-what)
      (let (cl-mark cl-mark2 (cl-next t) cl-next2)
	(with-current-buffer cl-what
	  (setq cl-mark (copy-marker (or cl-start (point-min))))
	  (setq cl-mark2 (and cl-end (copy-marker cl-end))))
	(while (and cl-next (or (not cl-mark2) (< cl-mark cl-mark2)))
	  (setq cl-next (if cl-prop (next-single-property-change
				     cl-mark cl-prop cl-what)
			  (next-property-change cl-mark cl-what))
		cl-next2 (or cl-next (with-current-buffer cl-what
				       (point-max))))
	  (funcall cl-func (prog1 (marker-position cl-mark)
			     (set-marker cl-mark cl-next2))
		   (if cl-mark2 (min cl-next2 cl-mark2) cl-next2)))
	(set-marker cl-mark nil) (if cl-mark2 (set-marker cl-mark2 nil)))
    (or cl-start (setq cl-start 0))
    (or cl-end (setq cl-end (length cl-what)))
    (while (< cl-start cl-end)
      (let ((cl-next (or (if cl-prop (next-single-property-change
				      cl-start cl-prop cl-what)
			   (next-property-change cl-start cl-what))
			 cl-end)))
	(funcall cl-func cl-start (min cl-next cl-end))
	(setq cl-start cl-next)))))

;;;###autoload
(defun cl--map-overlays (cl-func &optional cl-buffer cl-start cl-end cl-arg)
  (or cl-buffer (setq cl-buffer (current-buffer)))
  (let (cl-ovl)
    (with-current-buffer cl-buffer
      (setq cl-ovl (overlay-lists))
      (if cl-start (setq cl-start (copy-marker cl-start)))
      (if cl-end (setq cl-end (copy-marker cl-end))))
    (setq cl-ovl (nconc (car cl-ovl) (cdr cl-ovl)))
    (while (and cl-ovl
		(or (not (overlay-start (car cl-ovl)))
		    (and cl-end (>= (overlay-start (car cl-ovl)) cl-end))
		    (and cl-start (<= (overlay-end (car cl-ovl)) cl-start))
		    (not (funcall cl-func (car cl-ovl) cl-arg))))
      (setq cl-ovl (cdr cl-ovl)))
    (if cl-start (set-marker cl-start nil))
    (if cl-end (set-marker cl-end nil))))

;;; Support for `setf'.
;;;###autoload
(defun cl--set-frame-visible-p (frame val)
  (cond ((null val) (make-frame-invisible frame))
	((eq val 'icon) (iconify-frame frame))
	(t (make-frame-visible frame)))
  val)


;;; Numbers.

;;;###autoload
(defun cl-gcd (&rest args)
  "Return the greatest common divisor of the arguments."
  (let ((a (or (pop args) 0)))
    (dolist (b args)
      (while (/= b 0)
        (setq b (% a (setq a b)))))
    (abs a)))

;;;###autoload
(defun cl-lcm (&rest args)
  "Return the least common multiple of the arguments."
  (if (memq 0 args)
      0
    (let ((a (or (pop args) 1)))
      (dolist (b args)
        (setq a (* (/ a (cl-gcd a b)) b)))
      (abs a))))

;;;###autoload
(defun cl-isqrt (x)
  "Return the integer square root of the argument."
  (if (and (integerp x) (> x 0))
      (let ((g (cond ((<= x 100) 10) ((<= x 10000) 100)
		     ((<= x 1000000) 1000) (t x)))
	    g2)
	(while (< (setq g2 (/ (+ g (/ x g)) 2)) g)
	  (setq g g2))
	g)
    (if (eq x 0) 0 (signal 'arith-error nil))))

;;;###autoload
(defun cl-floor (x &optional y)
  "Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient."
  (let ((q (floor x y)))
    (list q (- x (if y (* y q) q)))))

;;;###autoload
(defun cl-ceiling (x &optional y)
  "Return a list of the ceiling of X and the fractional part of X.
With two arguments, return ceiling and remainder of their quotient."
  (let ((res (cl-floor x y)))
    (if (= (car (cdr res)) 0) res
      (list (1+ (car res)) (- (car (cdr res)) (or y 1))))))

;;;###autoload
(defun cl-truncate (x &optional y)
  "Return a list of the integer part of X and the fractional part of X.
With two arguments, return truncation and remainder of their quotient."
  (if (eq (>= x 0) (or (null y) (>= y 0)))
      (cl-floor x y) (cl-ceiling x y)))

;;;###autoload
(defun cl-round (x &optional y)
  "Return a list of X rounded to the nearest integer and the remainder.
With two arguments, return rounding and remainder of their quotient."
  (if y
      (if (and (integerp x) (integerp y))
	  (let* ((hy (/ y 2))
		 (res (cl-floor (+ x hy) y)))
	    (if (and (= (car (cdr res)) 0)
		     (= (+ hy hy) y)
		     (/= (% (car res) 2) 0))
		(list (1- (car res)) hy)
	      (list (car res) (- (car (cdr res)) hy))))
	(let ((q (round (/ x y))))
	  (list q (- x (* q y)))))
    (if (integerp x) (list x 0)
      (let ((q (round x)))
	(list q (- x q))))))

;;;###autoload
(defun cl-mod (x y)
  "The remainder of X divided by Y, with the same sign as Y."
  (nth 1 (cl-floor x y)))

;;;###autoload
(defun cl-rem (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (nth 1 (cl-truncate x y)))

;;;###autoload
(defun cl-signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

;;;###autoload
(cl-defun cl-parse-integer (string &key start end radix junk-allowed)
  "Parse integer from the substring of STRING from START to END.
STRING may be surrounded by whitespace chars (chars with syntax ` ').
Other non-digit chars are considered junk.
RADIX is an integer between 2 and 36, the default is 10.  Signal
an error if the substring between START and END cannot be parsed
as an integer unless JUNK-ALLOWED is non-nil."
  (cl-check-type string string)
  (let* ((start (or start 0))
	 (len	(length string))
	 (end   (or end len))
	 (radix (or radix 10)))
    (or (<= start end len)
	(error "Bad interval: [%d, %d)" start end))
    (cl-flet ((skip-whitespace ()
		(while (and (< start end)
			    (= 32 (char-syntax (aref string start))))
		  (setq start (1+ start)))))
      (skip-whitespace)
      (let ((sign (cl-case (and (< start end) (aref string start))
		    (?+ (cl-incf start) +1)
		    (?- (cl-incf start) -1)
		    (t  +1)))
	    digit sum)
	(while (and (< start end)
		    (setq digit (cl-digit-char-p (aref string start) radix)))
	  (setq sum (+ (* (or sum 0) radix) digit)
		start (1+ start)))
	(skip-whitespace)
	(cond ((and junk-allowed (null sum)) sum)
	      (junk-allowed (* sign sum))
	      ((or (/= start end) (null sum))
	       (error "Not an integer string: `%s'" string))
	      (t (* sign sum)))))))


;; Random numbers.

(defun cl--random-time ()
  (let* ((time (copy-sequence (current-time-string))) (i (length time)) (v 0))
    (while (>= (cl-decf i) 0) (setq v (+ (* v 3) (aref time i))))
    v))

;;;###autoload (autoload 'cl-random-state-p "cl-extra")
(cl-defstruct (cl--random-state
               (:copier nil)
               (:predicate cl-random-state-p)
               (:constructor nil)
               (:constructor cl--make-random-state (vec)))
  (i -1) (j 30) vec)

(defvar cl--random-state (cl--make-random-state (cl--random-time)))

;;;###autoload
(defun cl-random (lim &optional state)
  "Return a random nonnegative number less than LIM, an integer or float.
Optional second arg STATE is a random-state object."
  (or state (setq state cl--random-state))
  ;; Inspired by "ran3" from Numerical Recipes.  Additive congruential method.
  (let ((vec (cl--random-state-vec state)))
    (if (integerp vec)
	(let ((i 0) (j (- 1357335 (abs (% vec 1357333)))) (k 1))
	  (setf (cl--random-state-vec state)
                (setq vec (make-vector 55 nil)))
	  (aset vec 0 j)
	  (while (> (setq i (% (+ i 21) 55)) 0)
	    (aset vec i (setq j (prog1 k (setq k (- j k))))))
	  (while (< (setq i (1+ i)) 200) (cl-random 2 state))))
    (let* ((i (cl-callf (lambda (x) (% (1+ x) 55)) (cl--random-state-i state)))
	   (j (cl-callf (lambda (x) (% (1+ x) 55)) (cl--random-state-j state)))
	   (n (logand 8388607 (aset vec i (- (aref vec i) (aref vec j))))))
      (if (integerp lim)
	  (if (<= lim 512) (% n lim)
	    (if (> lim 8388607) (setq n (+ (lsh n 9) (cl-random 512 state))))
	    (let ((mask 1023))
	      (while (< mask (1- lim)) (setq mask (1+ (+ mask mask))))
	      (if (< (setq n (logand n mask)) lim) n (cl-random lim state))))
	(* (/ n '8388608e0) lim)))))

;;;###autoload
(defun cl-make-random-state (&optional state)
  "Return a copy of random-state STATE, or of the internal state if omitted.
If STATE is t, return a new state object seeded from the time of day."
  (unless state (setq state cl--random-state))
  (if (cl-random-state-p state)
      (copy-tree state t)
    (cl--make-random-state (if (integerp state) state (cl--random-time)))))

;; Implementation limits.

(defun cl--finite-do (func a b)
  (condition-case _
      (let ((res (funcall func a b)))   ; check for IEEE infinity
	(and (numberp res) (/= res (/ res 2)) res))
    (arith-error nil)))

;;;###autoload
(defun cl-float-limits ()
  "Initialize the Common Lisp floating-point parameters.
This sets the values of: `cl-most-positive-float', `cl-most-negative-float',
`cl-least-positive-float', `cl-least-negative-float', `cl-float-epsilon',
`cl-float-negative-epsilon', `cl-least-positive-normalized-float', and
`cl-least-negative-normalized-float'."
  (or cl-most-positive-float (not (numberp '2e1))
      (let ((x '2e0) y z)
	;; Find maximum exponent (first two loops are optimizations)
	(while (cl--finite-do '* x x) (setq x (* x x)))
	(while (cl--finite-do '* x (/ x 2)) (setq x (* x (/ x 2))))
	(while (cl--finite-do '+ x x) (setq x (+ x x)))
	(setq z x y (/ x 2))
	;; Now cl-fill in 1's in the mantissa.
	(while (and (cl--finite-do '+ x y) (/= (+ x y) x))
	  (setq x (+ x y) y (/ y 2)))
	(setq cl-most-positive-float x
	      cl-most-negative-float (- x))
	;; Divide down until mantissa starts rounding.
	(setq x (/ x z) y (/ 16 z) x (* x y))
	(while (condition-case _ (and (= x (* (/ x 2) 2)) (> (/ y 2) 0))
		 (arith-error nil))
	  (setq x (/ x 2) y (/ y 2)))
	(setq cl-least-positive-normalized-float y
	      cl-least-negative-normalized-float (- y))
	;; Divide down until value underflows to zero.
	(setq x (/ z) y x)
	(while (condition-case _ (> (/ x 2) 0) (arith-error nil))
	  (setq x (/ x 2)))
	(setq cl-least-positive-float x
	      cl-least-negative-float (- x))
	(setq x '1e0)
	(while (/= (+ '1e0 x) '1e0) (setq x (/ x 2)))
	(setq cl-float-epsilon (* x 2))
	(setq x '1e0)
	(while (/= (- '1e0 x) '1e0) (setq x (/ x 2)))
	(setq cl-float-negative-epsilon (* x 2))))
  nil)


;;; Sequence functions.

;;;###autoload
(defun cl-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end.
Signal an error if START or END are outside of the sequence (i.e
too large if positive or too small if negative)."
  (declare (gv-setter
            (lambda (new)
              (macroexp-let2 nil new new
		`(progn (cl-replace ,seq ,new :start1 ,start :end1 ,end)
			,new)))))
  (cond ((or (stringp seq) (vectorp seq)) (substring seq start end))
        ((listp seq)
         (let (len
               (errtext (format "Bad bounding indices: %s, %s" start end)))
           (and end (< end 0) (setq end (+ end (setq len (length seq)))))
           (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
           (unless (>= start 0)
             (error "%s" errtext))
           (when (> start 0)
             (setq seq (nthcdr (1- start) seq))
             (or seq (error "%s" errtext))
             (setq seq (cdr seq)))
           (if end
               (let ((res nil))
                 (while (and (>= (setq end (1- end)) start) seq)
                   (push (pop seq) res))
                 (or (= (1+ end) start) (error "%s" errtext))
                 (nreverse res))
             (copy-sequence seq))))
        (t (error "Unsupported sequence: %s" seq))))

;;;###autoload
(defun cl-concatenate (type &rest sequences)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCEs.
\n(fn TYPE SEQUENCE...)"
  (pcase type
    (`vector (apply #'vconcat sequences))
    (`string (apply #'concat sequences))
    (`list (apply #'append (append sequences '(nil))))
    (_ (error "Not a sequence type name: %S" type))))

;;; List functions.

;;;###autoload
(defun cl-revappend (x y)
  "Equivalent to (append (reverse X) Y)."
  (nconc (reverse x) y))

;;;###autoload
(defun cl-nreconc (x y)
  "Equivalent to (nconc (nreverse X) Y)."
  (nconc (nreverse x) y))

;;;###autoload
(defun cl-list-length (x)
  "Return the length of list X.  Return nil if list is circular."
  (let ((n 0) (fast x) (slow x))
    (while (and (cdr fast) (not (and (eq fast slow) (> n 0))))
      (setq n (+ n 2) fast (cdr (cdr fast)) slow (cdr slow)))
    (if fast (if (cdr fast) nil (1+ n)) n)))

;;;###autoload
(defun cl-tailp (sublist list)
  "Return true if SUBLIST is a tail of LIST."
  (while (and (consp list) (not (eq sublist list)))
    (setq list (cdr list)))
  (if (numberp sublist) (equal sublist list) (eq sublist list)))

;;; Property lists.

;;;###autoload
(defun cl-get (sym tag &optional def)
  "Return the value of SYMBOL's PROPNAME property, or DEFAULT if none.
\n(fn SYMBOL PROPNAME &optional DEFAULT)"
  (declare (compiler-macro cl--compiler-macro-get)
           (gv-setter (lambda (store) (ignore def) `(put ,sym ,tag ,store))))
  (cl-getf (symbol-plist sym) tag def))
(autoload 'cl--compiler-macro-get "cl-macs")

;;;###autoload
(defun cl-getf (plist tag &optional def)
  "Search PROPLIST for property PROPNAME; return its value or DEFAULT.
PROPLIST is a list of the sort returned by `symbol-plist'.
\n(fn PROPLIST PROPNAME &optional DEFAULT)"
  (declare (gv-expander
            (lambda (do)
              (gv-letplace (getter setter) plist
                (macroexp-let2* nil ((k tag) (d def))
                  (funcall do `(cl-getf ,getter ,k ,d)
			   (lambda (v)
			     (macroexp-let2 nil val v
			       `(progn
				  ,(funcall setter
					    `(cl--set-getf ,getter ,k ,val))
				  ,val)))))))))
  (let ((val-tail (cdr-safe (plist-member plist tag))))
    (if val-tail (car val-tail) def)))

;;;###autoload
(defun cl--set-getf (plist tag val)
  (let ((val-tail (cdr-safe (plist-member plist tag))))
    (if val-tail (progn (setcar val-tail val) plist)
      (cl-list* tag val plist))))

;;;###autoload
(defun cl--do-remf (plist tag)
  (let ((p (cdr plist)))
    ;; Can't use `plist-member' here because it goes to the cons-cell
    ;; of TAG and we need the one before.
    (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
    (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t))))

;;;###autoload
(defun cl-remprop (sym tag)
  "Remove from SYMBOL's plist the property PROPNAME and its value.
\n(fn SYMBOL PROPNAME)"
  (let ((plist (symbol-plist sym)))
    (if (and plist (eq tag (car plist)))
	(progn (setplist sym (cdr (cdr plist))) t)
      (cl--do-remf plist tag))))

;;; Streams.

;;;###autoload
(defun cl-fresh-line (&optional stream)
  "Output a newline unless already at the beginning of a line."
  (terpri stream 'ensure))

;;; Some debugging aids.

(defun cl-prettyprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  (let ((pt (point)) last)
    (insert "\n" (prin1-to-string form) "\n")
    (setq last (point))
    (goto-char (1+ pt))
    (while (search-forward "(quote " last t)
      (delete-char -7)
      (insert "'")
      (forward-sexp)
      (delete-char 1))
    (goto-char (1+ pt))
    (cl--do-prettyprint)))

(defun cl--do-prettyprint ()
  (skip-chars-forward " ")
  (if (looking-at "(")
      (let ((skip (or (looking-at "((") (looking-at "(prog")
		      (looking-at "(unwind-protect ")
		      (looking-at "(function (")
		      (looking-at "(cl--block-wrapper ")))
	    (two (or (looking-at "(defun ") (looking-at "(defmacro ")))
	    (let (or (looking-at "(let\\*? ") (looking-at "(while ")))
	    (set (looking-at "(p?set[qf] ")))
	(if (or skip let
		(progn
		  (forward-sexp)
		  (and (>= (current-column) 78) (progn (backward-sexp) t))))
	    (let ((nl t))
	      (forward-char 1)
	      (cl--do-prettyprint)
	      (or skip (looking-at ")") (cl--do-prettyprint))
	      (or (not two) (looking-at ")") (cl--do-prettyprint))
	      (while (not (looking-at ")"))
		(if set (setq nl (not nl)))
		(if nl (insert "\n"))
		(lisp-indent-line)
		(cl--do-prettyprint))
	      (forward-char 1))))
    (forward-sexp)))

;;;###autoload
(defun cl-prettyexpand (form &optional full)
  "Expand macros in FORM and insert the pretty-printed result.
Optional argument FULL non-nil means to expand all macros,
including `cl-block' and `cl-eval-when'."
  (message "Expanding...")
  (let ((cl--compiling-file full)
	(byte-compile-macro-environment nil))
    (setq form (macroexpand-all form
                                (and (not full) '((cl-block) (cl-eval-when)))))
    (message "Formatting...")
    (prog1 (cl-prettyprint form)
      (message ""))))

;;; Integration into the online help system.

(eval-when-compile (require 'cl-macs))  ;Explicitly, for cl--find-class.
(require 'help-mode)

;; FIXME: We could go crazy and add another entry so describe-symbol can be
;; used with the slot names of CL structs (and/or EIEIO objects).
(add-to-list 'describe-symbol-backends
             `(nil ,#'cl-find-class ,(lambda (s _b _f) (cl-describe-type s))))

(defconst cl--typedef-regexp
  (concat "(" (regexp-opt '("defclass" "defstruct" "cl-defstruct"
                            "cl-deftype" "deftype"))
          "[ \t\r\n]+%s[ \t\r\n]+"))
(with-eval-after-load 'find-func
  (defvar find-function-regexp-alist)
  (add-to-list 'find-function-regexp-alist
               `(define-type . cl--typedef-regexp)))

(define-button-type 'cl-help-type
  :supertype 'help-function-def
  'help-function #'cl-describe-type
  'help-echo (purecopy "mouse-2, RET: describe this type"))

(define-button-type 'cl-type-definition
  :supertype 'help-function-def
  'help-echo (purecopy "mouse-2, RET: find type definition"))

(declare-function help-fns-short-filename "help-fns" (filename))

;;;###autoload
(defun cl-find-class (type) (cl--find-class type))

;;;###autoload
(defun cl-describe-type (type)
  "Display the documentation for type TYPE (a symbol)."
  (interactive
   (let ((str (completing-read "Describe type: " obarray #'cl-find-class t)))
     (if (<= (length str) 0)
         (user-error "Abort!")
       (list (intern str)))))
  (help-setup-xref (list #'cl-describe-type type)
                   (called-interactively-p 'interactive))
  (save-excursion
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (let ((class (cl-find-class type)))
          (if class
              (cl--describe-class type class)
            ;; FIXME: Describe other types (the built-in ones, or those from
            ;; cl-deftype).
            (user-error "Unknown type %S" type))))
      (with-current-buffer standard-output
        ;; Return the text we displayed.
        (buffer-string)))))

(defun cl--describe-class (type &optional class)
  (unless class (setq class (cl--find-class type)))
  (let ((location (find-lisp-object-file-name type 'define-type))
        (metatype (type-of class)))
    (insert (symbol-name type)
            (substitute-command-keys " is a type (of kind `"))
    (help-insert-xref-button (symbol-name metatype)
                             'cl-help-type metatype)
    (insert (substitute-command-keys "')"))
    (when location
      (insert (substitute-command-keys " in `"))
      (help-insert-xref-button
       (help-fns-short-filename location)
       'cl-type-definition type location 'define-type)
      (insert (substitute-command-keys "'")))
    (insert ".\n")

    ;; Parents.
    (let ((pl (cl--class-parents class))
          cur)
      (when pl
        (insert " Inherits from ")
        (while (setq cur (pop pl))
          (setq cur (cl--class-name cur))
          (insert (substitute-command-keys "`"))
          (help-insert-xref-button (symbol-name cur)
                                   'cl-help-type cur)
          (insert (substitute-command-keys (if pl "', " "'"))))
        (insert ".\n")))

    ;; Children, if available.  ¡For EIEIO!
    (let ((ch (condition-case nil
                  (cl-struct-slot-value metatype 'children class)
                (cl-struct-unknown-slot nil)))
          cur)
      (when ch
        (insert " Children ")
        (while (setq cur (pop ch))
          (insert (substitute-command-keys "`"))
          (help-insert-xref-button (symbol-name cur)
                                   'cl-help-type cur)
          (insert (substitute-command-keys (if ch "', " "'"))))
        (insert ".\n")))

    ;; Type's documentation.
    (let ((doc (cl--class-docstring class)))
      (when doc
        (insert "\n" doc "\n\n")))

    ;; Describe all the slots in this class.
    (cl--describe-class-slots class)

    ;; Describe all the methods specific to this class.
    (let ((generics (cl-generic-all-functions type)))
      (when generics
        (insert (propertize "Specialized Methods:\n\n" 'face 'bold))
        (dolist (generic generics)
          (insert (substitute-command-keys "`"))
          (help-insert-xref-button (symbol-name generic)
                                   'help-function generic)
          (insert (substitute-command-keys "'"))
          (pcase-dolist (`(,qualifiers ,args ,doc)
                         (cl--generic-method-documentation generic type))
            (insert (format " %s%S\n" qualifiers args)
                    (or doc "")))
          (insert "\n\n"))))))

(defun cl--describe-class-slot (slot)
  (insert
   (concat
    (propertize "Slot: " 'face 'bold)
    (prin1-to-string (cl--slot-descriptor-name slot))
    (unless (eq (cl--slot-descriptor-type slot) t)
      (concat "    type = "
              (prin1-to-string (cl--slot-descriptor-type slot))))
    ;; FIXME: The default init form is treated differently for structs and for
    ;; eieio objects: for structs, the default is nil, for eieio-objects
    ;; it's a special "unbound" value.
    (unless nil ;; (eq (cl--slot-descriptor-initform slot) eieio-unbound)
      (concat "    default = "
              (prin1-to-string (cl--slot-descriptor-initform slot))))
    (when (alist-get :printer (cl--slot-descriptor-props slot))
      (concat "    printer = "
              (prin1-to-string
               (alist-get :printer (cl--slot-descriptor-props slot)))))
    (when (alist-get :documentation (cl--slot-descriptor-props slot))
      (concat "\n  "
              (substitute-command-keys
               (alist-get :documentation (cl--slot-descriptor-props slot)))
              "\n")))
   "\n"))

(defun cl--print-table (header rows)
  ;; FIXME: Isn't this functionality already implemented elsewhere?
  (let ((cols (apply #'vector (mapcar #'string-width header)))
        (col-space 2))
    (dolist (row rows)
      (dotimes (i (length cols))
        (let* ((x (pop row))
               (curwidth (aref cols i))
               (newwidth (if x (string-width x) 0)))
          (if (> newwidth curwidth)
              (setf (aref cols i) newwidth)))))
    (let ((formats '())
          (col 0))
      (dotimes (i (length cols))
        (push (concat (propertize "	"
                                  'display
                                  `(space :align-to ,(+ col col-space)))
                      "%s")
              formats)
        (cl-incf col (+ col-space (aref cols i))))
      (let ((format (mapconcat #'identity (nreverse formats) "")))
        (insert (apply #'format format
                       (mapcar (lambda (str) (propertize str 'face 'italic))
                               header))
                "\n")
        (insert (apply #'format format
                       (mapcar (lambda (str) (make-string (string-width str) ?—))
                               header))
                "\n")
        (dolist (row rows)
          (insert (apply #'format format row) "\n"))))))

(defun cl--describe-class-slots (class)
  "Print help description for the slots in CLASS.
Outputs to the current buffer."
  (let* ((slots (cl--class-slots class))
         (metatype (type-of class))
         ;; ¡For EIEIO!
         (cslots (condition-case nil
                     (cl-struct-slot-value metatype 'class-slots class)
                   (cl-struct-unknown-slot nil))))
    (insert (propertize "Instance Allocated Slots:\n\n"
			'face 'bold))
    (let* ((has-doc nil)
           (slots-strings
            (mapcar
             (lambda (slot)
               (list (cl-prin1-to-string (cl--slot-descriptor-name slot))
                     (cl-prin1-to-string (cl--slot-descriptor-type slot))
                     (cl-prin1-to-string (cl--slot-descriptor-initform slot))
                     (let ((doc (alist-get :documentation
                                           (cl--slot-descriptor-props slot))))
                       (if (not doc) ""
                         (setq has-doc t)
                         (substitute-command-keys doc)))))
             slots)))
      (cl--print-table `("Name" "Type" "Default" . ,(if has-doc '("Doc")))
                       slots-strings))
    (insert "\n")
    (when (> (length cslots) 0)
      (insert (propertize "\nClass Allocated Slots:\n\n" 'face 'bold))
      (mapc #'cl--describe-class-slot cslots))))


(run-hooks 'cl-extra-load-hook)

;; Local variables:
;; byte-compile-dynamic: t
;; generated-autoload-file: "cl-loaddefs.el"
;; End:

(provide 'cl-extra)
;;; cl-extra.el ends here
