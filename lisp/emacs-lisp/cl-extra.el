;;; cl-extra.el --- Common Lisp features, part 2 -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1993,2000  Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Keywords: extensions

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

(or (memq 'cl-19 features)
    (error "Tried to load `cl-extra' before `cl'!"))


;;; We define these here so that this file can compile without having
;;; loaded the cl.el file already.

(defmacro cl-push (x place) (list 'setq place (list 'cons x place)))
(defmacro cl-pop (place)
  (list 'car (list 'prog1 place (list 'setq place (list 'cdr place)))))

;;; Type coercion.

(defun coerce (x type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier."
  (cond ((eq type 'list) (if (listp x) x (append x nil)))
	((eq type 'vector) (if (vectorp x) x (vconcat x)))
	((eq type 'string) (if (stringp x) x (concat x)))
	((eq type 'array) (if (arrayp x) x (vconcat x)))
	((and (eq type 'character) (stringp x) (= (length x) 1)) (aref x 0))
	((and (eq type 'character) (symbolp x)) (coerce (symbol-name x) type))
	((eq type 'float) (float x))
	((typep x type) x)
	(t (error "Can't coerce %s to type %s" x type))))


;;; Predicates.

(defun equalp (x y)
  "T if two Lisp objects have similar structures and contents.
This is like `equal', except that it accepts numerically equal
numbers of different types (float vs. integer), and also compares
strings case-insensitively."
  (cond ((eq x y) t)
	((stringp x)
	 (and (stringp y) (= (length x) (length y))
	      (or (string-equal x y)
		  (string-equal (downcase x) (downcase y)))))   ; lazy but simple!
	((numberp x)
	 (and (numberp y) (= x y)))
	((consp x)
	 (while (and (consp x) (consp y) (equalp (car x) (car y)))
	   (setq x (cdr x) y (cdr y)))
	 (and (not (consp x)) (equalp x y)))
	((vectorp x)
	 (and (vectorp y) (= (length x) (length y))
	      (let ((i (length x)))
		(while (and (>= (setq i (1- i)) 0)
			    (equalp (aref x i) (aref y i))))
		(< i 0))))
	(t (equal x y))))


;;; Control structures.

(defun cl-mapcar-many (cl-func cl-seqs)
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
	  (cl-push (apply cl-func cl-args) cl-res)
	  (setq cl-i (1+ cl-i)))
	(nreverse cl-res))
    (let ((cl-res nil)
	  (cl-x (car cl-seqs))
	  (cl-y (nth 1 cl-seqs)))
      (let ((cl-n (min (length cl-x) (length cl-y)))
	    (cl-i -1))
	(while (< (setq cl-i (1+ cl-i)) cl-n)
	  (cl-push (funcall cl-func
			    (if (consp cl-x) (cl-pop cl-x) (aref cl-x cl-i))
			    (if (consp cl-y) (cl-pop cl-y) (aref cl-y cl-i)))
		   cl-res)))
      (nreverse cl-res))))

(defun map (cl-type cl-func cl-seq &rest cl-rest)
  "Map a function across one or more sequences, returning a sequence.
TYPE is the sequence type to return, FUNC is the function, and SEQS
are the argument sequences."
  (let ((cl-res (apply 'mapcar* cl-func cl-seq cl-rest)))
    (and cl-type (coerce cl-res cl-type))))

(defun maplist (cl-func cl-list &rest cl-rest)
  "Map FUNC to each sublist of LIST or LISTS.
Like `mapcar', except applies to lists and their cdr's rather than to
the elements themselves."
  (if cl-rest
      (let ((cl-res nil)
	    (cl-args (cons cl-list (copy-sequence cl-rest)))
	    cl-p)
	(while (not (memq nil cl-args))
	  (cl-push (apply cl-func cl-args) cl-res)
	  (setq cl-p cl-args)
	  (while cl-p (setcar cl-p (cdr (cl-pop cl-p)) )))
	(nreverse cl-res))
    (let ((cl-res nil))
      (while cl-list
	(cl-push (funcall cl-func cl-list) cl-res)
	(setq cl-list (cdr cl-list)))
      (nreverse cl-res))))

(defun cl-mapc (cl-func cl-seq &rest cl-rest)
  "Like `mapcar', but does not accumulate values returned by the function."
  (if cl-rest
      (progn (apply 'map nil cl-func cl-seq cl-rest)
	     cl-seq)
    (mapc cl-func cl-seq)))

(defun mapl (cl-func cl-list &rest cl-rest)
  "Like `maplist', but does not accumulate values returned by the function."
  (if cl-rest
      (apply 'maplist cl-func cl-list cl-rest)
    (let ((cl-p cl-list))
      (while cl-p (funcall cl-func cl-p) (setq cl-p (cdr cl-p)))))
  cl-list)

(defun mapcan (cl-func cl-seq &rest cl-rest)
  "Like `mapcar', but nconc's together the values returned by the function."
  (apply 'nconc (apply 'mapcar* cl-func cl-seq cl-rest)))

(defun mapcon (cl-func cl-list &rest cl-rest)
  "Like `maplist', but nconc's together the values returned by the function."
  (apply 'nconc (apply 'maplist cl-func cl-list cl-rest)))

(defun some (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of any element of SEQ or SEQs.
If so, return the true (non-nil) value returned by PREDICATE."
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-some
	(apply 'map nil
	       (function (lambda (&rest cl-x)
			   (let ((cl-res (apply cl-pred cl-x)))
			     (if cl-res (throw 'cl-some cl-res)))))
	       cl-seq cl-rest) nil)
    (let ((cl-x nil))
      (while (and cl-seq (not (setq cl-x (funcall cl-pred (cl-pop cl-seq))))))
      cl-x)))

(defun every (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of every element of SEQ or SEQs."
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-every
	(apply 'map nil
	       (function (lambda (&rest cl-x)
			   (or (apply cl-pred cl-x) (throw 'cl-every nil))))
	       cl-seq cl-rest) t)
    (while (and cl-seq (funcall cl-pred (car cl-seq)))
      (setq cl-seq (cdr cl-seq)))
    (null cl-seq)))

(defun notany (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of every element of SEQ or SEQs."
  (not (apply 'some cl-pred cl-seq cl-rest)))

(defun notevery (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of some element of SEQ or SEQs."
  (not (apply 'every cl-pred cl-seq cl-rest)))

;;; Support for `loop'.
(defun cl-map-keymap (cl-func cl-map)
  (while (symbolp cl-map) (setq cl-map (symbol-function cl-map)))
  (if (listp cl-map)
      (let ((cl-p cl-map))
	(while (consp (setq cl-p (cdr cl-p)))
	  (cond ((consp (car cl-p))
		 (funcall cl-func (car (car cl-p)) (cdr (car cl-p))))
		((or (vectorp (car cl-p)) (char-table-p (car cl-p)))
		 (cl-map-keymap cl-func (car cl-p)))
		((eq (car cl-p) 'keymap)
		 (setq cl-p nil)))))
    (let ((cl-i -1))
      (while (< (setq cl-i (1+ cl-i)) (length cl-map))
	(if (aref cl-map cl-i)
	    (funcall cl-func cl-i (aref cl-map cl-i)))))))

(defun cl-map-keymap-recursively (cl-func-rec cl-map &optional cl-base)
  (or cl-base
      (setq cl-base (copy-sequence [0])))
  (cl-map-keymap
   (function
    (lambda (cl-key cl-bind)
      (aset cl-base (1- (length cl-base)) cl-key)
      (if (keymapp cl-bind)
	  (cl-map-keymap-recursively
	   cl-func-rec cl-bind
	   (vconcat cl-base (list 0)))
	(funcall cl-func-rec cl-base cl-bind))))
   cl-map))

(defun cl-map-intervals (cl-func &optional cl-what cl-prop cl-start cl-end)
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

(defun cl-map-overlays (cl-func &optional cl-buffer cl-start cl-end cl-arg)
  (or cl-buffer (setq cl-buffer (current-buffer)))
  (if (fboundp 'overlay-lists)

      ;; This is the preferred algorithm, though overlay-lists is undocumented.
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
	(if cl-end (set-marker cl-end nil)))

    ;; This alternate algorithm fails to find zero-length overlays.
    (let ((cl-mark (with-current-buffer cl-buffer
		     (copy-marker (or cl-start (point-min)))))
	  (cl-mark2 (and cl-end (with-current-buffer cl-buffer
				  (copy-marker cl-end))))
	  cl-pos cl-ovl)
      (while (save-excursion
	       (and (setq cl-pos (marker-position cl-mark))
		    (< cl-pos (or cl-mark2 (point-max)))
		    (progn
		      (set-buffer cl-buffer)
		      (setq cl-ovl (overlays-at cl-pos))
		      (set-marker cl-mark (next-overlay-change cl-pos)))))
	(while (and cl-ovl
		    (or (/= (overlay-start (car cl-ovl)) cl-pos)
			(not (and (funcall cl-func (car cl-ovl) cl-arg)
				  (set-marker cl-mark nil)))))
	  (setq cl-ovl (cdr cl-ovl))))
      (set-marker cl-mark nil) (if cl-mark2 (set-marker cl-mark2 nil)))))

;;; Support for `setf'.
(defun cl-set-frame-visible-p (frame val)
  (cond ((null val) (make-frame-invisible frame))
	((eq val 'icon) (iconify-frame frame))
	(t (make-frame-visible frame)))
  val)

;;; Support for `progv'.
(defvar cl-progv-save)
(defun cl-progv-before (syms values)
  (while syms
    (cl-push (if (boundp (car syms))
		 (cons (car syms) (symbol-value (car syms)))
	       (car syms)) cl-progv-save)
    (if values
	(set (cl-pop syms) (cl-pop values))
      (makunbound (cl-pop syms)))))

(defun cl-progv-after ()
  (while cl-progv-save
    (if (consp (car cl-progv-save))
	(set (car (car cl-progv-save)) (cdr (car cl-progv-save)))
      (makunbound (car cl-progv-save)))
    (cl-pop cl-progv-save)))


;;; Numbers.

(defun gcd (&rest args)
  "Return the greatest common divisor of the arguments."
  (let ((a (abs (or (cl-pop args) 0))))
    (while args
      (let ((b (abs (cl-pop args))))
	(while (> b 0) (setq b (% a (setq a b))))))
    a))

(defun lcm (&rest args)
  "Return the least common multiple of the arguments."
  (if (memq 0 args)
      0
    (let ((a (abs (or (cl-pop args) 1))))
      (while args
	(let ((b (abs (cl-pop args))))
	  (setq a (* (/ a (gcd a b)) b))))
      a)))

(defun isqrt (a)
  "Return the integer square root of the argument."
  (if (and (integerp a) (> a 0))
      (let ((g (cond ((<= a 100) 10) ((<= a 10000) 100)
		     ((<= a 1000000) 1000) (t a)))
	    g2)
	(while (< (setq g2 (/ (+ g (/ a g)) 2)) g)
	  (setq g g2))
	g)
    (if (eq a 0) 0 (signal 'arith-error nil))))

(defun floor* (x &optional y)
  "Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient."
  (let ((q (floor x y)))
    (list q (- x (if y (* y q) q)))))

(defun ceiling* (x &optional y)
  "Return a list of the ceiling of X and the fractional part of X.
With two arguments, return ceiling and remainder of their quotient."
  (let ((res (floor* x y)))
    (if (= (car (cdr res)) 0) res
      (list (1+ (car res)) (- (car (cdr res)) (or y 1))))))

(defun truncate* (x &optional y)
  "Return a list of the integer part of X and the fractional part of X.
With two arguments, return truncation and remainder of their quotient."
  (if (eq (>= x 0) (or (null y) (>= y 0)))
      (floor* x y) (ceiling* x y)))

(defun round* (x &optional y)
  "Return a list of X rounded to the nearest integer and the remainder.
With two arguments, return rounding and remainder of their quotient."
  (if y
      (if (and (integerp x) (integerp y))
	  (let* ((hy (/ y 2))
		 (res (floor* (+ x hy) y)))
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

(defun mod* (x y)
  "The remainder of X divided by Y, with the same sign as Y."
  (nth 1 (floor* x y)))

(defun rem* (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (nth 1 (truncate* x y)))

(defun signum (a)
  "Return 1 if A is positive, -1 if negative, 0 if zero."
  (cond ((> a 0) 1) ((< a 0) -1) (t 0)))


;; Random numbers.

(defvar *random-state*)
(defun random* (lim &optional state)
  "Return a random nonnegative number less than LIM, an integer or float.
Optional second arg STATE is a random-state object."
  (or state (setq state *random-state*))
  ;; Inspired by "ran3" from Numerical Recipes.  Additive congruential method.
  (let ((vec (aref state 3)))
    (if (integerp vec)
	(let ((i 0) (j (- 1357335 (% (abs vec) 1357333))) (k 1) ii)
	  (aset state 3 (setq vec (make-vector 55 nil)))
	  (aset vec 0 j)
	  (while (> (setq i (% (+ i 21) 55)) 0)
	    (aset vec i (setq j (prog1 k (setq k (- j k))))))
	  (while (< (setq i (1+ i)) 200) (random* 2 state))))
    (let* ((i (aset state 1 (% (1+ (aref state 1)) 55)))
	   (j (aset state 2 (% (1+ (aref state 2)) 55)))
	   (n (logand 8388607 (aset vec i (- (aref vec i) (aref vec j))))))
      (if (integerp lim)
	  (if (<= lim 512) (% n lim)
	    (if (> lim 8388607) (setq n (+ (lsh n 9) (random* 512 state))))
	    (let ((mask 1023))
	      (while (< mask (1- lim)) (setq mask (1+ (+ mask mask))))
	      (if (< (setq n (logand n mask)) lim) n (random* lim state))))
	(* (/ n '8388608e0) lim)))))

(defun make-random-state (&optional state)
  "Return a copy of random-state STATE, or of `*random-state*' if omitted.
If STATE is t, return a new state object seeded from the time of day."
  (cond ((null state) (make-random-state *random-state*))
	((vectorp state) (cl-copy-tree state t))
	((integerp state) (vector 'cl-random-state-tag -1 30 state))
	(t (make-random-state (cl-random-time)))))

(defun random-state-p (object)
  "Return t if OBJECT is a random-state object."
  (and (vectorp object) (= (length object) 4)
       (eq (aref object 0) 'cl-random-state-tag)))


;; Implementation limits.

(defun cl-finite-do (func a b)
  (condition-case err
      (let ((res (funcall func a b)))   ; check for IEEE infinity
	(and (numberp res) (/= res (/ res 2)) res))
    (arith-error nil)))

(defvar most-positive-float)
(defvar most-negative-float)
(defvar least-positive-float)
(defvar least-negative-float)
(defvar least-positive-normalized-float)
(defvar least-negative-normalized-float)
(defvar float-epsilon)
(defvar float-negative-epsilon)

(defun cl-float-limits ()
  (or most-positive-float (not (numberp '2e1))
      (let ((x '2e0) y z)
	;; Find maximum exponent (first two loops are optimizations)
	(while (cl-finite-do '* x x) (setq x (* x x)))
	(while (cl-finite-do '* x (/ x 2)) (setq x (* x (/ x 2))))
	(while (cl-finite-do '+ x x) (setq x (+ x x)))
	(setq z x y (/ x 2))
	;; Now fill in 1's in the mantissa.
	(while (and (cl-finite-do '+ x y) (/= (+ x y) x))
	  (setq x (+ x y) y (/ y 2)))
	(setq most-positive-float x
	      most-negative-float (- x))
	;; Divide down until mantissa starts rounding.
	(setq x (/ x z) y (/ 16 z) x (* x y))
	(while (condition-case err (and (= x (* (/ x 2) 2)) (> (/ y 2) 0))
		 (arith-error nil))
	  (setq x (/ x 2) y (/ y 2)))
	(setq least-positive-normalized-float y
	      least-negative-normalized-float (- y))
	;; Divide down until value underflows to zero.
	(setq x (/ 1 z) y x)
	(while (condition-case err (> (/ x 2) 0) (arith-error nil))
	  (setq x (/ x 2)))
	(setq least-positive-float x
	      least-negative-float (- x))
	(setq x '1e0)
	(while (/= (+ '1e0 x) '1e0) (setq x (/ x 2)))
	(setq float-epsilon (* x 2))
	(setq x '1e0)
	(while (/= (- '1e0 x) '1e0) (setq x (/ x 2)))
	(setq float-negative-epsilon (* x 2))))
  nil)


;;; Sequence functions.

(defun subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (cl-push (cl-pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))

(defun concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCES."
  (cond ((eq type 'vector) (apply 'vconcat seqs))
	((eq type 'string) (apply 'concat seqs))
	((eq type 'list) (apply 'append (append seqs '(nil))))
	(t (error "Not a sequence type name: %s" type))))


;;; List functions.

(defun revappend (x y)
  "Equivalent to (append (reverse X) Y)."
  (nconc (reverse x) y))

(defun nreconc (x y)
  "Equivalent to (nconc (nreverse X) Y)."
  (nconc (nreverse x) y))

(defun list-length (x)
  "Return the length of a list.  Return nil if list is circular."
  (let ((n 0) (fast x) (slow x))
    (while (and (cdr fast) (not (and (eq fast slow) (> n 0))))
      (setq n (+ n 2) fast (cdr (cdr fast)) slow (cdr slow)))
    (if fast (if (cdr fast) nil (1+ n)) n)))

(defun tailp (sublist list)
  "Return true if SUBLIST is a tail of LIST."
  (while (and (consp list) (not (eq sublist list)))
    (setq list (cdr list)))
  (if (numberp sublist) (equal sublist list) (eq sublist list)))

(defalias 'cl-copy-tree 'copy-tree)


;;; Property lists.

(defun get* (sym tag &optional def)    ; See compiler macro in cl-macs.el
  "Return the value of SYMBOL's PROPNAME property, or DEFAULT if none."
  (or (get sym tag)
      (and def
	   (let ((plist (symbol-plist sym)))
	     (while (and plist (not (eq (car plist) tag)))
	       (setq plist (cdr (cdr plist))))
	     (if plist (car (cdr plist)) def)))))

(defun getf (plist tag &optional def)
  "Search PROPLIST for property PROPNAME; return its value or DEFAULT.
PROPLIST is a list of the sort returned by `symbol-plist'."
  (setplist '--cl-getf-symbol-- plist)
  (or (get '--cl-getf-symbol-- tag)
      ;; Originally we called get* here,
      ;; but that fails, because get* has a compiler macro
      ;; definition that uses getf!
      (when def
	(while (and plist (not (eq (car plist) tag)))
	  (setq plist (cdr (cdr plist))))
	(if plist (car (cdr plist)) def))))

(defun cl-set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

(defun cl-do-remf (plist tag)
  (let ((p (cdr plist)))
    (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
    (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t))))

(defun cl-remprop (sym tag)
  "Remove from SYMBOL's plist the property PROP and its value."
  (let ((plist (symbol-plist sym)))
    (if (and plist (eq tag (car plist)))
	(progn (setplist sym (cdr (cdr plist))) t)
      (cl-do-remf plist tag))))
(defalias 'remprop 'cl-remprop)



;;; Hash tables.

(defun cl-make-hash-table (&rest cl-keys)
  "Make an empty Common Lisp-style hash-table.
Keywords supported:  :test :size
The Common Lisp keywords :rehash-size and :rehash-threshold are ignored."
  (let ((cl-test (or (car (cdr (memq :test cl-keys))) 'eql))
	(cl-size (or (car (cdr (memq :size cl-keys))) 20)))
    (make-hash-table :size cl-size :test cl-size)))

(defun cl-hash-table-p (x)
  "Return t if OBJECT is a hash table."
  (or (hash-table-p x)
      (eq (car-safe x) 'cl-hash-table-tag)))

(defun cl-not-hash-table (x &optional y &rest z)
  (signal 'wrong-type-argument (list 'cl-hash-table-p (or y x))))

(defun cl-hash-lookup (key table)
  (or (eq (car-safe table) 'cl-hash-table-tag) (cl-not-hash-table table))
  (let* ((array (nth 2 table)) (test (car (cdr table))) (str key) sym)
    (if (symbolp array) (setq str nil sym (symbol-value array))
      (while (or (consp str) (and (vectorp str) (> (length str) 0)))
	(setq str (elt str 0)))
      (cond ((stringp str) (if (eq test 'equalp) (setq str (downcase str))))
	    ((symbolp str) (setq str (symbol-name str)))
	    ((and (numberp str) (> str -8000000) (< str 8000000))
	     (or (integerp str) (setq str (truncate str)))
	     (setq str (aref ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"
			      "11" "12" "13" "14" "15"] (logand str 15))))
	    (t (setq str "*")))
      (setq sym (symbol-value (intern-soft str array))))
    (list (and sym (cond ((or (eq test 'eq)
			      (and (eq test 'eql) (not (numberp key))))
			  (assq key sym))
			 ((memq test '(eql equal)) (assoc key sym))
			 (t (assoc* key sym :test test))))
	  sym str)))

;; These variables are just kept for compatibility with code
;; byte-compiled by Emacs-20.
(defvar cl-builtin-gethash (symbol-function 'gethash))
(defvar cl-builtin-remhash (symbol-function 'remhash))
(defvar cl-builtin-clrhash (symbol-function 'clrhash))
(defvar cl-builtin-maphash (symbol-function 'maphash))

(defun cl-gethash (key table &optional def)
  "Look up KEY in HASH-TABLE; return corresponding value, or DEFAULT."
  (if (consp table)
      (let ((found (cl-hash-lookup key table)))
	(if (car found) (cdr (car found)) def))
    (gethash key table def)))

(defun cl-puthash (key val table)
  (if (consp table)
      (let ((found (cl-hash-lookup key table)))
	(if (car found) (setcdr (car found) val)
	  (if (nth 2 found)
	      (progn
		(if (> (nth 3 table) (* (length (nth 2 table)) 3))
		    (let ((new-table (make-vector (nth 3 table) 0)))
		      (mapatoms (function
				 (lambda (sym)
				   (set (intern (symbol-name sym) new-table)
					(symbol-value sym))))
				(nth 2 table))
		      (setcar (cdr (cdr table)) new-table)))
		(set (intern (nth 2 found) (nth 2 table))
		     (cons (cons key val) (nth 1 found))))
	    (set (nth 2 table) (cons (cons key val) (nth 1 found))))
	  (setcar (cdr (cdr (cdr table))) (1+ (nth 3 table)))))
    (funcall 'puthash key val table)) val)

(defun cl-remhash (key table)
  "Remove KEY from HASH-TABLE."
  (if (consp table)
      (let ((found (cl-hash-lookup key table)))
	(and (car found)
	     (let ((del (delq (car found) (nth 1 found))))
	       (setcar (cdr (cdr (cdr table))) (1- (nth 3 table)))
	       (if (nth 2 found) (set (intern (nth 2 found) (nth 2 table)) del)
		 (set (nth 2 table) del)) t)))
    (prog1 (not (eq (gethash key table '--cl--) '--cl--))
      (remhash key table))))

(defun cl-clrhash (table)
  "Clear HASH-TABLE."
  (if (consp table)
      (progn
	(or (cl-hash-table-p table) (cl-not-hash-table table))
	(if (symbolp (nth 2 table)) (set (nth 2 table) nil)
	  (setcar (cdr (cdr table)) (make-vector (length (nth 2 table)) 0)))
	(setcar (cdr (cdr (cdr table))) 0))
    (clrhash table))
  nil)

(defun cl-maphash (cl-func cl-table)
  "Call FUNCTION on keys and values from HASH-TABLE."
  (or (cl-hash-table-p cl-table) (cl-not-hash-table cl-table))
  (if (consp cl-table)
      (mapatoms (function (lambda (cl-x)
			    (setq cl-x (symbol-value cl-x))
			    (while cl-x
			      (funcall cl-func (car (car cl-x))
				       (cdr (car cl-x)))
			      (setq cl-x (cdr cl-x)))))
		(if (symbolp (nth 2 cl-table))
		    (vector (nth 2 cl-table)) (nth 2 cl-table)))
    (maphash cl-func cl-table)))

(defun cl-hash-table-count (table)
  "Return the number of entries in HASH-TABLE."
  (or (cl-hash-table-p table) (cl-not-hash-table table))
  (if (consp table)
      (nth 3 table)
    (hash-table-count table)))


;;; Some debugging aids.

(defun cl-prettyprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  (let ((pt (point)) last)
    (insert "\n" (prin1-to-string form) "\n")
    (setq last (point))
    (goto-char (1+ pt))
    (while (search-forward "(quote " last t)
      (delete-backward-char 7)
      (insert "'")
      (forward-sexp)
      (delete-char 1))
    (goto-char (1+ pt))
    (cl-do-prettyprint)))

(defun cl-do-prettyprint ()
  (skip-chars-forward " ")
  (if (looking-at "(")
      (let ((skip (or (looking-at "((") (looking-at "(prog")
		      (looking-at "(unwind-protect ")
		      (looking-at "(function (")
		      (looking-at "(cl-block-wrapper ")))
	    (two (or (looking-at "(defun ") (looking-at "(defmacro ")))
	    (let (or (looking-at "(let\\*? ") (looking-at "(while ")))
	    (set (looking-at "(p?set[qf] ")))
	(if (or skip let
		(progn
		  (forward-sexp)
		  (and (>= (current-column) 78) (progn (backward-sexp) t))))
	    (let ((nl t))
	      (forward-char 1)
	      (cl-do-prettyprint)
	      (or skip (looking-at ")") (cl-do-prettyprint))
	      (or (not two) (looking-at ")") (cl-do-prettyprint))
	      (while (not (looking-at ")"))
		(if set (setq nl (not nl)))
		(if nl (insert "\n"))
		(lisp-indent-line)
		(cl-do-prettyprint))
	      (forward-char 1))))
    (forward-sexp)))

(defvar cl-macroexpand-cmacs nil)
(defvar cl-closure-vars nil)

(defun cl-macroexpand-all (form &optional env)
  "Expand all macro calls through a Lisp FORM.
This also does some trivial optimizations to make the form prettier."
  (while (or (not (eq form (setq form (macroexpand form env))))
	     (and cl-macroexpand-cmacs
		  (not (eq form (setq form (compiler-macroexpand form)))))))
  (cond ((not (consp form)) form)
	((memq (car form) '(let let*))
	 (if (null (nth 1 form))
	     (cl-macroexpand-all (cons 'progn (cddr form)) env)
	   (let ((letf nil) (res nil) (lets (cadr form)))
	     (while lets
	       (cl-push (if (consp (car lets))
			    (let ((exp (cl-macroexpand-all (caar lets) env)))
			      (or (symbolp exp) (setq letf t))
			      (cons exp (cl-macroexpand-body (cdar lets) env)))
			  (let ((exp (cl-macroexpand-all (car lets) env)))
			    (if (symbolp exp) exp
			      (setq letf t) (list exp nil)))) res)
	       (setq lets (cdr lets)))
	     (list* (if letf (if (eq (car form) 'let) 'letf 'letf*) (car form))
		    (nreverse res) (cl-macroexpand-body (cddr form) env)))))
	((eq (car form) 'cond)
	 (cons (car form)
	       (mapcar (function (lambda (x) (cl-macroexpand-body x env)))
		       (cdr form))))
	((eq (car form) 'condition-case)
	 (list* (car form) (nth 1 form) (cl-macroexpand-all (nth 2 form) env)
		(mapcar (function
			 (lambda (x)
			   (cons (car x) (cl-macroexpand-body (cdr x) env))))
			(cdddr form))))
	((memq (car form) '(quote function))
	 (if (eq (car-safe (nth 1 form)) 'lambda)
	     (let ((body (cl-macroexpand-body (cddadr form) env)))
	       (if (and cl-closure-vars (eq (car form) 'function)
			(cl-expr-contains-any body cl-closure-vars))
		   (let* ((new (mapcar 'gensym cl-closure-vars))
			  (sub (pairlis cl-closure-vars new)) (decls nil))
		     (while (or (stringp (car body))
				(eq (car-safe (car body)) 'interactive))
		       (cl-push (list 'quote (cl-pop body)) decls))
		     (put (car (last cl-closure-vars)) 'used t)
		     (append
		      (list 'list '(quote lambda) '(quote (&rest --cl-rest--)))
		      (sublis sub (nreverse decls))
		      (list
		       (list* 'list '(quote apply)
			      (list 'list '(quote quote)
				    (list 'function
					  (list* 'lambda
						 (append new (cadadr form))
						 (sublis sub body))))
			      (nconc (mapcar (function
					      (lambda (x)
						(list 'list '(quote quote) x)))
					     cl-closure-vars)
				     '((quote --cl-rest--)))))))
		 (list (car form) (list* 'lambda (cadadr form) body))))
	   (let ((found (assq (cadr form) env)))
	     (if (eq (cadr (caddr found)) 'cl-labels-args)
		 (cl-macroexpand-all (cadr (caddr (cadddr found))) env)
	       form))))
	((memq (car form) '(defun defmacro))
	 (list* (car form) (nth 1 form) (cl-macroexpand-body (cddr form) env)))
	((and (eq (car form) 'progn) (not (cddr form)))
	 (cl-macroexpand-all (nth 1 form) env))
	((eq (car form) 'setq)
	 (let* ((args (cl-macroexpand-body (cdr form) env)) (p args))
	   (while (and p (symbolp (car p))) (setq p (cddr p)))
	   (if p (cl-macroexpand-all (cons 'setf args)) (cons 'setq args))))
	(t (cons (car form) (cl-macroexpand-body (cdr form) env)))))

(defun cl-macroexpand-body (body &optional env)
  (mapcar (function (lambda (x) (cl-macroexpand-all x env))) body))

(defun cl-prettyexpand (form &optional full)
  (message "Expanding...")
  (let ((cl-macroexpand-cmacs full) (cl-compiling-file full)
	(byte-compile-macro-environment nil))
    (setq form (cl-macroexpand-all form
				   (and (not full) '((block) (eval-when)))))
    (message "Formatting...")
    (prog1 (cl-prettyprint form)
      (message ""))))



(run-hooks 'cl-extra-load-hook)

;;; cl-extra.el ends here
