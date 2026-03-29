;;; cl-extra.el --- Common Lisp features, part 2  -*- lexical-binding: t -*-

;; Copyright (C) 1993, 2000-2026 Free Software Foundation, Inc.

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
(require 'seq)

;;; Type coercion.

;;;###autoload
(defun cl-coerce (x type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier.
\n(fn OBJECT TYPE)"
  (cond ((eq type 'list) (if (listp x) x (append x nil)))
	((eq type 'vector) (if (vectorp x) x (vconcat x)))
	((eq type 'bool-vector)
         (if (bool-vector-p x) x (apply #'bool-vector (cl-coerce x 'list))))
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
  (declare (side-effect-free error-free))
  (cond ((eq x y) t)
	((stringp x)
	 (and (stringp y) (string-equal-ignore-case x y)))
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
(defun cl--mapcar-many (func seqs &optional acc)
  (if (cdr (cdr seqs))
      (let* ((res nil)
             (n (apply #'min (mapcar #'length seqs)))
             (i 0)
             (args (copy-sequence seqs))
             p1 p2)
        (setq seqs (copy-sequence seqs))
        (while (< i n)
          (setq p1 seqs p2 args)
          (while p1
            (setcar p2
                    (if (consp (car p1))
                        (prog1 (car (car p1))
                          (setcar p1 (cdr (car p1))))
                      (aref (car p1) i)))
            (setq p1 (cdr p1) p2 (cdr p2)))
	  (if acc
              (push (apply func args) res)
            (apply func args))
          (setq i (1+ i)))
        (and acc (nreverse res)))
    (let ((res nil)
          (x (car seqs))
          (y (nth 1 seqs)))
      (let ((n (min (length x) (length y)))
            (i -1))
        (while (< (setq i (1+ i)) n)
          (let ((val (funcall func
                              (if (consp x) (pop x) (aref x i))
                              (if (consp y) (pop y) (aref y i)))))
	    (when acc
              (push val res)))))
      (and acc (nreverse res)))))

;;;###autoload
(defsubst cl-map (type func seq &rest rest)
  "Map a FUNCTION across one or more SEQUENCEs, returning a sequence.
TYPE is the sequence type to return.
\n(fn TYPE FUNCTION SEQUENCE...)"
  (declare (important-return-value t))
  (let ((res (apply 'cl-mapcar func seq rest)))
    (and type (cl-coerce res type))))

;;;###autoload
(defun cl-maplist (func list &rest rest)
  "Map FUNCTION to each sublist of LIST or LISTs.
Like `cl-mapcar', except applies to lists and their cdr's rather than to
the elements themselves.
\n(fn FUNCTION LIST...)"
  (declare (important-return-value t))
  (if rest
      (let ((res nil)
            (args (cons list (copy-sequence rest)))
            p)
        (while (not (memq nil args))
          (push (apply func args) res)
          (setq p args)
          (while p (setcar p (cdr (pop p)))))
        (nreverse res))
    (let ((res nil))
      (while list
        (push (funcall func list) res)
        (setq list (cdr list)))
      (nreverse res))))

;;;###autoload
(defun cl-mapc (func seq &rest rest)
  "Like `cl-mapcar', but does not accumulate values returned by the function.
\n(fn FUNCTION SEQUENCE...)"
  (if rest
      (if (or (cdr rest) (nlistp seq) (nlistp (car rest)))
          (progn
            (cl--mapcar-many func (cons seq rest))
            seq)
        (let ((x seq) (y (car rest)))
          (while (and x y)
            (funcall func (pop x) (pop y)))
          seq))
    (mapc func seq)))

;;;###autoload
(defun cl-mapl (func list &rest rest)
  "Like `cl-maplist', but does not accumulate values returned by the function.
\n(fn FUNCTION LIST...)"
  (if rest
      (let ((args (cons list (copy-sequence rest)))
            p)
        (while (not (memq nil args))
          (apply func args)
          (setq p args)
          (while p (setcar p (cdr (pop p))))))
    (let ((p list))
      (while p (funcall func p) (setq p (cdr p)))))
  list)

;;;###autoload
(defun cl-mapcan (func seq &rest rest)
  "Like `cl-mapcar', but nconc's together the values returned by the function.
\n(fn FUNCTION SEQUENCE...)"
  (declare (important-return-value t))
  (if rest
      (apply #'nconc (apply #'cl-mapcar func seq rest))
    (mapcan func seq)))

;;;###autoload
(defun cl-mapcon (func list &rest rest)
  "Like `cl-maplist', but nconc's together the values returned by the function.
\n(fn FUNCTION LIST...)"
  (declare (important-return-value t))
  (apply #'nconc (apply #'cl-maplist func list rest)))

;;;###autoload
(defun cl-some (pred seq &rest rest)
  "Say whether PREDICATE is true for any element in the SEQ sequences.
More specifically, the return value of this function will be the
same as the first return value of PREDICATE where PREDICATE has a
non-nil value.

\n(fn PREDICATE SEQ...)"
  (declare (important-return-value t))
  (if (or rest (nlistp seq))
      (catch 'cl-some
        (apply #'cl-map nil
               (lambda (&rest x)
                 (let ((res (apply pred x)))
                   (if res (throw 'cl-some res))))
               seq rest) nil)
    (let ((x nil))
      (while (and seq (not (setq x (funcall pred (pop seq))))))
      x)))

;;;###autoload
(defun cl-every (pred seq &rest rest)
  "Return true if PREDICATE is true of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (declare (important-return-value t))
  (if (or rest (nlistp seq))
      (catch 'cl-every
        (apply #'cl-map nil
               (lambda (&rest x)
                 (or (apply pred x) (throw 'cl-every nil)))
               seq rest) t)
    (while (and seq (funcall pred (car seq)))
      (setq seq (cdr seq)))
    (null seq)))

;;;###autoload
(defsubst cl-notany (pred seq &rest rest)
  "Return true if PREDICATE is false of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (declare (important-return-value t))
  (not (apply #'cl-some pred seq rest)))

;;;###autoload
(defsubst cl-notevery (pred seq &rest rest)
  "Return true if PREDICATE is false of some element of SEQ or SEQs.
\n(fn PREDICATE SEQ...)"
  (declare (important-return-value t))
  (not (apply #'cl-every pred seq rest)))

;;;###autoload
(defun cl--map-keymap-recursively (func-rec map &optional base)
  (or base
      (setq base (copy-sequence [0])))
  (map-keymap
   (lambda (key bind)
     (aset base (1- (length base)) key)
     (if (keymapp bind)
         (cl--map-keymap-recursively
          func-rec bind
          (vconcat base (list 0)))
       (funcall func-rec base bind)))
   map))

;;;###autoload
(defun cl--map-intervals (func &optional what prop start end)
  (or what (setq what (current-buffer)))
  (if (bufferp what)
      (let (mark mark2 (next t) next2)
        (with-current-buffer what
          (setq mark (copy-marker (or start (point-min))))
          (setq mark2 (and end (copy-marker end))))
        (while (and next (or (not mark2) (< mark mark2)))
          (setq next (if prop (next-single-property-change
                               mark prop what)
                       (next-property-change mark what))
                next2 (or next (with-current-buffer what
                                 (point-max))))
          (funcall func (prog1 (marker-position mark)
                          (set-marker mark next2))
                   (if mark2 (min next2 mark2) next2)))
        (set-marker mark nil) (if mark2 (set-marker mark2 nil)))
    (or start (setq start 0))
    (or end (setq end (length what)))
    (while (< start end)
      (let ((next (or (if prop (next-single-property-change
                                start prop what)
                        (next-property-change start what))
                      end)))
        (funcall func start (min next end))
        (setq start next)))))

;;;###autoload
(defun cl--map-overlays (func &optional buffer start end arg)
  (or buffer (setq buffer (current-buffer)))
  (let (ovl)
    (with-current-buffer buffer
      (setq ovl (overlay-lists))
      (if start (setq start (copy-marker start)))
      (if end (setq end (copy-marker end))))
    (setq ovl (nconc (car ovl) (cdr ovl)))
    (while (and ovl
                (or (not (overlay-start (car ovl)))
                    (and end (>= (overlay-start (car ovl)) end))
                    (and start (<= (overlay-end (car ovl)) start))
                    (not (funcall func (car ovl) arg))))
      (setq ovl (cdr ovl)))
    (if start (set-marker start nil))
    (if end (set-marker end nil))))

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
  (declare (side-effect-free t))
  (let ((a (or (pop args) 0)))
    (dolist (b args)
      (while (/= b 0)
        (setq b (% a (setq a b)))))
    (abs a)))

;;;###autoload
(defun cl-lcm (&rest args)
  "Return the least common multiple of the arguments."
  (declare (side-effect-free t))
  (if (memq 0 args)
      0
    (let ((a (or (pop args) 1)))
      (dolist (b args)
        (setq a (* (/ a (cl-gcd a b)) b)))
      (abs a))))

;;;###autoload
(defun cl-isqrt (x)
  "Return the integer square root of the (integer) argument X."
  (declare (side-effect-free t))
  (if (and (integerp x) (> x 0))
      (let ((g (ash 2 (/ (logb x) 2)))
	    g2)
	(while (< (setq g2 (/ (+ g (/ x g)) 2)) g)
	  (setq g g2))
	g)
    (if (eq x 0) 0 (signal 'arith-error nil))))

;;;###autoload
(defun cl-floor (x &optional y)
  "Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient."
  (declare (side-effect-free t))
  (let ((q (floor x y)))
    (list q (- x (if y (* y q) q)))))

;;;###autoload
(defun cl-ceiling (x &optional y)
  "Return a list of the ceiling of X and the fractional part of X.
With two arguments, return ceiling and remainder of their quotient."
  (declare (side-effect-free t))
  (let ((res (cl-floor x y)))
    (if (= (car (cdr res)) 0) res
      (list (1+ (car res)) (- (car (cdr res)) (or y 1))))))

;;;###autoload
(defun cl-truncate (x &optional y)
  "Return a list of the integer part of X and the fractional part of X.
With two arguments, return truncation and remainder of their quotient."
  (declare (side-effect-free t))
  (if (eq (>= x 0) (or (null y) (>= y 0)))
      (cl-floor x y) (cl-ceiling x y)))

;;;###autoload
(defun cl-round (x &optional y)
  "Return a list of X rounded to the nearest integer and the remainder.
With two arguments, return rounding and remainder of their quotient."
  (declare (side-effect-free t))
  (if y
      (if (and (integerp x) (integerp y))
	  (let* ((hy (/ y 2))
		 (res (cl-floor (+ x hy) y)))
	    (if (and (= (car (cdr res)) 0)
		     (= (+ hy hy) y)
		     (oddp (car res)))
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
  (declare (side-effect-free t))
  (nth 1 (cl-floor x y)))

;;;###autoload
(defun cl-rem (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (declare (side-effect-free t))
  (nth 1 (cl-truncate x y)))

;;;###autoload
(defun cl-signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (declare (side-effect-free t))
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

;;;###autoload
(cl-defun cl-parse-integer (string &key start end radix junk-allowed)
  "Parse integer from the substring of STRING from START to END.
STRING may be surrounded by whitespace chars (chars with syntax ` ').
Other non-digit chars are considered junk.
RADIX is an integer between 2 and 36, the default is 10.  Signal
an error if the substring between START and END cannot be parsed
as an integer unless JUNK-ALLOWED is non-nil."
  (declare (side-effect-free t))
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
                    (?+ (incf start) +1)
                    (?- (incf start) -1)
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
  "Return high-precision timestamp from `time-convert'.

For example, suitable for use as seed by `cl-make-random-state'."
  (car (time-convert nil t)))

;;;###autoload (autoload 'cl-random-state-p "cl-extra")
;;;###autoload (function-put 'cl-random-state-p 'side-effect-free 'error-free)
(cl-defstruct (cl--random-state
               (:copier nil)
               (:predicate cl-random-state-p)
               (:constructor nil)
               (:constructor cl--make-random-state (vec)))
  (i -1) (j 30) vec)

(defvar cl--random-state (cl--make-random-state (cl--random-time)))

;;;###autoload
(defun cl-random (lim &optional state)
  "Return a pseudo-random nonnegative number less than LIM, an integer or float.
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
	   (n (aset vec i (logand 8388607 (- (aref vec i) (aref vec j))))))
      (cond
       ((natnump lim)
	(if (<= lim 512) (% n lim)
	  (if (> lim 8388607) (setq n (+ (ash n 9) (cl-random 512 state))))
	  (let ((mask 1023))
	    (while (< mask (1- lim)) (setq mask (1+ (+ mask mask))))
	    (if (< (setq n (logand n mask)) lim) n (cl-random lim state)))))
       ((< 0 lim 1.0e+INF)
        (* (/ n '8388608e0) lim))
       (t
        (error "Limit %S not supported by cl-random" lim))))))

;;;###autoload
(defun cl-make-random-state (&optional state)
  "Return a copy of random-state STATE, or of the internal state if omitted.
If STATE is t, return a new state object seeded from the time of day."
  (unless state (setq state cl--random-state))
  (if (cl-random-state-p state)
      (copy-sequence state)
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
  (declare (side-effect-free t)
           (gv-setter
            (lambda (new)
              (macroexp-let2 nil new new
		`(progn (cl-replace ,seq ,new :start1 ,start :end1 ,end)
			,new)))))
  (seq-subseq seq start end))

;;; This isn't a defalias because autoloading defaliases doesn't work
;;; very well.

;;;###autoload
(defun cl-concatenate (type &rest sequences)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCEs.
\n(fn TYPE SEQUENCE...)"
  (apply #'seq-concatenate type sequences))

;;; List functions.

;;;###autoload
(defsubst cl-revappend (x y)
  "Equivalent to (append (reverse X) Y)."
  (declare (side-effect-free t))
  (nconc (reverse x) y))

;;;###autoload
(defsubst cl-nreconc (x y)
  "Equivalent to (nconc (nreverse X) Y)."
  (declare (important-return-value t))
  (nconc (nreverse x) y))

;;;###autoload
(defun cl-list-length (x)
  "Return the length of list X.  Return nil if list is circular."
  (declare (side-effect-free t))
  (cl-check-type x list)
  (condition-case nil
      (length x)
    (circular-list)))

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
  (declare (side-effect-free t)
           (compiler-macro cl--compiler-macro-get)
           (gv-setter (lambda (store) (ignore def) `(put ,sym ,tag ,store))))
  (cl-getf (symbol-plist sym) tag def))
(autoload 'cl--compiler-macro-get "cl-macs")

;;;###autoload
(defun cl-getf (plist tag &optional def)
  "Search PROPLIST for property PROPNAME; return its value or DEFAULT.
PROPLIST is a list of the sort returned by `symbol-plist'.
\n(fn PROPLIST PROPNAME &optional DEFAULT)"
  (declare (side-effect-free t)
           (gv-expander
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
  (let ((val-tail (cdr (plist-member plist tag))))
    (if val-tail (car val-tail) def)))

;;;###autoload
(defun cl--set-getf (plist tag val)
  (let ((val-tail (cdr (plist-member plist tag))))
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
(defun cl-remprop (symbol propname)
  "Remove from SYMBOL's plist the property PROPNAME and its value."
  (let ((plist (symbol-plist symbol)))
    (if (and plist (eq propname (car plist)))
	(progn (setplist symbol (cdr (cdr plist))) t)
      (cl--do-remf plist propname))))

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
(defun cl-prettyexpand (form &optional _full)
  "Expand macros in FORM and insert the pretty-printed result."
  (declare (advertised-calling-convention (form) "27.1"))
  (message "Expanding...")
  (setq form (macroexpand-all form))
  (message "Formatting...")
  (prog1
      (cl-prettyprint form)
    (message "")))

;;; Integration into the online help system.

(eval-when-compile (require 'cl-macs))  ;Explicitly, for cl--find-class.
(require 'help-mode)

(defconst cl--typedef-regexp
  (concat "(" (regexp-opt '("defclass" "defstruct" "cl-defstruct"
                            "cl-deftype" "deftype"))
          "[ \t\r\n]+%s[ \t\r\n]+"))
(with-eval-after-load 'find-func
  (defvar find-function-regexp-alist)
  (add-to-list 'find-function-regexp-alist
               '(define-type . cl--typedef-regexp)))

(define-button-type 'cl-type-definition
  :supertype 'help-function-def
  'help-echo "mouse-2, RET: find type definition")

(declare-function help-fns-short-filename "help-fns" (filename))

;;;###autoload
(defun cl-find-class (type)
    "Return CL class of TYPE.

Call `cl--find-class' to get TYPE's propname `cl--class'"
  (cl--find-class type))

(declare-function help-fns--setup-xref-backend "help-fns" ())

;;;###autoload
(defun cl-describe-type (type &optional _buf _frame)
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
        (help-fns--setup-xref-backend)
        ;; Return the text we displayed.
        (buffer-string)))))

(defun cl--class-children (class)
  (let ((children '()))
    (mapatoms
     (lambda (sym)
       (let ((sym-class (cl--find-class sym)))
         (and sym-class (memq class (cl--class-parents sym-class))
          (push sym children)))))
    children))

(defun cl--describe-class (type &optional class)
  (unless class (setq class (cl--find-class type)))
  (let ((location (find-lisp-object-file-name type 'define-type))
        (metatype (type-of class)))
    (insert (symbol-name type)
            (substitute-command-keys " is a type (of kind `"))
    (help-insert-xref-button (symbol-name metatype)
                             'help-type metatype)
    (insert (substitute-command-keys "')"))
    (when location
      (insert (substitute-command-keys " in `"))
      (help-insert-xref-button
       (help-fns-short-filename location)
       'cl-type-definition type location 'define-type)
      (insert (substitute-quotes "'")))
    (insert ".\n")

    ;; Parents.
    (let ((pl (cl--class-parents class))
          cur)
      (when pl
        (insert " Inherits from ")
        (while (setq cur (pop pl))
          (setq cur (cl--class-name cur))
          (insert (substitute-quotes "`"))
          (help-insert-xref-button (symbol-name cur)
                                   'help-type cur)
          (insert (substitute-command-keys (if pl "', " "'"))))
        (insert ".\n")))

    ;; Children.
    (let ((ch (cl--class-children class))
          cur)
      (when ch
        (insert " Children ")
        (while (setq cur (pop ch))
          (insert (substitute-quotes "`"))
          (help-insert-xref-button (symbol-name cur)
                                   'help-type cur)
          (insert (substitute-command-keys (if ch "', " "'"))))
        (insert ".\n")))

    ;; Describe all the slots in this class.
    ;; Put it before the docstring, since the docstring may want
    ;; to refer to the slots.
    (cl--describe-class-slots class)

    ;; Type's documentation.
    (let ((doc (cl--class-docstring class)))
      (when doc
        (insert (if (save-excursion
                      (or (< (skip-chars-backward "\n") -1) (bobp)))
                    ""
                  "\n")
                doc "\n\n")))

    ;; Describe all the methods specific to this class.
    (let ((generics (cl-generic-all-functions type)))
      (when generics
        (insert (propertize "Specialized Methods:\n\n" 'face 'bold))
        (dolist (generic generics)
          (insert (substitute-quotes "`"))
          (help-insert-xref-button (symbol-name generic)
                                   'help-function generic)
          (insert (substitute-quotes "'"))
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

(defun cl--print-table (header rows &optional last-slot-on-next-line)
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
        (incf col (+ col-space (aref cols i))))
      (let ((format (mapconcat #'identity (nreverse formats))))
        (insert (apply #'format format
                       (mapcar (lambda (str) (propertize str 'face 'italic))
                               header))
                "\n")
        (insert (apply #'format format
                       (mapcar (lambda (str) (make-string (string-width str) ?—))
                               header))
                "\n")
        (dolist (row rows)
          (insert (apply #'format format row) "\n")
          (when last-slot-on-next-line
            (dolist (line (string-lines (car (last row))))
              (insert "    " line "\n"))
            (insert "\n")))))))

(defun cl--describe-class-slots (class)
  "Print help description for the slots in CLASS.
Outputs to the current buffer."
  (let* ((slots (cl--class-slots class))
         (metatype (type-of class))
         ;; ¡For EIEIO!
         (cslots (condition-case nil
                     (cl-struct-slot-value metatype 'class-slots class)
                   (cl-struct-unknown-slot nil))))
    (if (and (null slots) (eq metatype 'built-in-class))
        (insert "This is a built-in type.\n")

      (insert (propertize "Instance Allocated Slots:\n\n"
			  'face 'bold))
      (let* ((has-doc nil)
             (slots-strings
              (mapcar
               (lambda (slot)
                 (list (cl-prin1-to-string (cl--slot-descriptor-name slot))
                       (let ((type (cl--slot-descriptor-type slot)))
                         (cond
                          ((eq type t) "")
                          ((and type (symbolp type) (cl--find-class type))
                           (make-text-button (symbol-name type) nil
		                             'type 'help-type
		                             'help-args (list type)))
                          (t (cl-prin1-to-string type))))
                       (cl-prin1-to-string (cl--slot-descriptor-initform slot))
                       (let ((doc (alist-get :documentation
                                             (cl--slot-descriptor-props slot))))
                         (if (not doc) ""
                           (setq has-doc t)
                           (substitute-command-keys doc)))))
               slots)))
        (cl--print-table `("Name" "Type" "Default") slots-strings has-doc)))
    (insert "\n")
    (when (> (length cslots) 0)
      (insert (propertize "\nClass Allocated Slots:\n\n" 'face 'bold))
      (mapc #'cl--describe-class-slot cslots))))

;;;; Method dispatch on `cl-deftype' types (a.k.a "derived types").

;; Extend `cl-deftype' to define data types which are also valid
;; argument types for dispatching generic function methods (see also
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=77725>).

;; Ensure each type satisfies `eql'.
(defvar cl--derived-type-specializers-memo (make-hash-table :test 'equal)
  "Memoization table used in `cl--derived-type-specializers'.")

;; The usual dispatch is
;;
;;   (lambda (arg &rest args)
;;     (let ((f (gethash (cl-typeof arg) precomputed-methods-table)))
;;       (if f
;;           (apply f arg args)
;;         ;; Slow case when encountering a new type
;;         ...)))
;;
;; where often the most expensive part is `&rest' (which has to
;; allocate a list for those remaining arguments),
;;
;; So we're talking about replacing
;;
;;   &rest + cl-type-of + gethash + if + apply
;;
;; with a function that loops over N types, calling `cl-typep' on each
;; one of them (`cl-typep' itself being a recursive function that
;; basically interprets the type language).  This is going to slow
;; down dispatch very significantly for those generic functions that
;; have a method that dispatches on a derived type, compared to
;; those that don't.
;;
;; As a simple optimization, the method dispatch tests only those
;; derived types which have been used as a specialize in a method.
;;
;; A possible further improvement:
;;
;; - based on the PARENTS declaration, create a map from builtin-type
;;   to the set of cl-types that have that builtin-type among their
;;   parents.  That presumes some PARENTS include some builtin-types,
;;   obviously otherwise the map will be trivial with all cl-types
;;   associated with the `t' "dummy parent".  [ We could even go crazy
;;   and try and guess PARENTS when not provided, by analyzing the
;;   type's definition. ]
;; - in `cl--derived-type-specializers' start by calling `cl-type-of',
;;   then use the map to find which cl-types may need to be checked.
;;
(defun cl--derived-type-specializers (object types)
  "Return the list of specializers for OBJECT, derived from TYPES.
Return an unique (eq) list of atomic types OBJECT belongs to, ordered
from the most specific type to the most general.
TYPES is a list of types that OBJECT can potentially belong to."
  ;; This function is speed critical for the dispatch on CL's derived types.
  ;; Currently TYPES is just the set of types we're interested in.
  ;; TODO: We could speed this up by replacing TYPES with anything that can
  ;; be precomputed from it.
  (let* ((found (list (cl-type-of object))))
    ;; Build a list of all types OBJECT belongs to.
    (dolist (type types)
      ;; If OBJECT is of type, add type to the matching list.
      (if (funcall (get type 'cl-deftype-satisfies) object)
          (push type found)))
    ;; This memoization has two purposes:
    ;; - Speed up computation.
    ;; - Make sure we always return the same (eq) object, so that the
    ;;   method dispatch's own caching works as it should.
    (with-memoization (gethash found cl--derived-type-specializers-memo)
      ;; Compute an ordered list of types from the DAG.
      (merge-ordered-lists
       (mapcar (lambda (type)
                 (cl--class-allparents (cl--find-class type)))
               found)))))

(cl-generic-define-generalizer cl--derived-type-generalizer
  ;; FIXME: This priority can't be always right.  :-(
  ;; E.g. a method dispatching on a type like (or number function),
  ;; should take precedence over a method on `t' but not over a method
  ;; on `number'.  Similarly a method dispatching on a type like
  ;; (satisfies (lambda (x) (equal x '(A . B)))) should take precedence
  ;; over a method on (head 'A).
  ;; Fixing this 100% is impossible so this generalizer is condemned to
  ;; suffer from "undefined method ordering" problems, unless/until we
  ;; restrict it somehow to a subset that we can handle reliably.
  20 ;; "typeof" < "derived-types" < "head" priority
  (lambda (obj &optional types &rest _)
    (if (not types)
        :need-specializers
      `(nil ;; Extra bindings, if any.
        . (cl--derived-type-specializers ,obj ,types))))
  (lambda (tag &rest _) (if (consp tag) tag)))

;;;###autoload
(defun cl--derived-type-generalizers (type)
  ;; Make sure this derived type can be used without arguments.
  (funcall (or (get type 'cl-deftype-handler)
               (error "Type %S lacks cl-deftype-handler" type)))
  ;; Check that we have a precomputed predicate since that's what
  ;; `cl--derived-type-specializers' uses.
  (or (get type 'cl-deftype-satisfies)
      (error "Type %S lacks cl-deftype-satisfies" type))
  (list cl--derived-type-generalizer))

;;;; Trailer

(make-obsolete-variable 'cl-extra-load-hook
                        "use `with-eval-after-load' instead." "28.1")
(run-hooks 'cl-extra-load-hook)

;; Local variables:
;; generated-autoload-file: "cl-loaddefs.el"
;; End:

(provide 'cl-extra)
;;; cl-extra.el ends here
