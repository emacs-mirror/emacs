;;; cl.el --- Common Lisp extensions for Emacs -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
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

;; This file contains the portions of the Common Lisp extensions
;; package which should always be present.


;;; Future notes:

;; Once Emacs 19 becomes standard, many things in this package which are
;; messy for reasons of compatibility can be greatly simplified.  For now,
;; I prefer to maintain one unified version.


;;; Change Log:

;; Version 2.02 (30 Jul 93):
;;  * Added "cl-compat.el" file, extra compatibility with old package.
;;  * Added `lexical-let' and `lexical-let*'.
;;  * Added `define-modify-macro', `callf', and `callf2'.
;;  * Added `ignore-errors'.
;;  * Changed `(setf (nthcdr N PLACE) X)' to work when N is zero.
;;  * Merged `*gentemp-counter*' into `*gensym-counter*'.
;;  * Extended `subseq' to allow negative START and END like `substring'.
;;  * Added `in-ref', `across-ref', `elements of-ref' loop clauses.
;;  * Added `concat', `vconcat' loop clauses.
;;  * Cleaned up a number of compiler warnings.

;; Version 2.01 (7 Jul 93):
;;  * Added support for FSF version of Emacs 19.
;;  * Added `add-hook' for Emacs 18 users.
;;  * Added `defsubst*' and `symbol-macrolet'.
;;  * Added `maplist', `mapc', `mapl', `mapcan', `mapcon'.
;;  * Added `map', `concatenate', `reduce', `merge'.
;;  * Added `revappend', `nreconc', `tailp', `tree-equal'.
;;  * Added `assert', `check-type', `typecase', `typep', and `deftype'.
;;  * Added destructuring and `&environment' support to `defmacro*'.
;;  * Added destructuring to `loop', and added the following clauses:
;;      `elements', `frames', `overlays', `intervals', `buffers', `key-seqs'.
;;  * Renamed `delete' to `delete*' and `remove' to `remove*'.
;;  * Completed support for all keywords in `remove*', `substitute', etc.
;;  * Added `most-positive-float' and company.
;;  * Fixed hash tables to work with latest Lucid Emacs.
;;  * `proclaim' forms are no longer compile-time-evaluating; use `declaim'.
;;  * Syntax for `warn' declarations has changed.
;;  * Improved implementation of `random*'.
;;  * Moved most sequence functions to a new file, cl-seq.el.
;;  * Moved `eval-when' into cl-macs.el.
;;  * Moved `pushnew' and `adjoin' to cl.el for most common cases.
;;  * Moved `provide' forms down to ends of files.
;;  * Changed expansion of `pop' to something that compiles to better code.
;;  * Changed so that no patch is required for Emacs 19 byte compiler.
;;  * Made more things dependent on `optimize' declarations.
;;  * Added a partial implementation of struct print functions.
;;  * Miscellaneous minor changes.

;; Version 2.00:
;;  * First public release of this package.


;;; Code:

(defvar cl-optimize-speed 1)
(defvar cl-optimize-safety 1)


;;;###autoload
(defvar custom-print-functions nil
  "This is a list of functions that format user objects for printing.
Each function is called in turn with three arguments: the object, the
stream, and the print level (currently ignored).  If it is able to
print the object it returns true; otherwise it returns nil and the
printer proceeds to the next function on the list.

This variable is not used at present, but it is defined in hopes that
a future Emacs interpreter will be able to use it.")


;;; Predicates.

(defun eql (a b)    ; See compiler macro in cl-macs.el
  "T if the two args are the same Lisp object.
Floating-point numbers of equal value are `eql', but they may not be `eq'."
  (if (numberp a)
      (equal a b)
    (eq a b)))


;;; Generalized variables.  These macros are defined here so that they
;;; can safely be used in .emacs files.

(defmacro incf (place &optional x)
  "Increment PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the incremented value of PLACE."
  (if (symbolp place)
      (list 'setq place (if x (list '+ place x) (list '1+ place)))
    (list 'callf '+ place (or x 1))))

(defmacro decf (place &optional x)
  "Decrement PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the decremented value of PLACE."
  (if (symbolp place)
      (list 'setq place (if x (list '- place x) (list '1- place)))
    (list 'callf '- place (or x 1))))

(defmacro pop (place)
  "Remove and return the head of the list stored in PLACE.
Analogous to (prog1 (car PLACE) (setf PLACE (cdr PLACE))), though more
careful about evaluating each argument only once and in the right order.
PLACE may be a symbol, or any generalized variable allowed by `setf'."
  (if (symbolp place)
      (list 'car (list 'prog1 place (list 'setq place (list 'cdr place))))
    (cl-do-pop place)))

(defmacro push (x place)
  "Insert X at the head of the list stored in PLACE.
Analogous to (setf PLACE (cons X PLACE)), though more careful about
evaluating each argument only once and in the right order.  PLACE may
be a symbol, or any generalized variable allowed by `setf'."
  (if (symbolp place) (list 'setq place (list 'cons x place))
    (list 'callf2 'cons x place)))

(defmacro pushnew (x place &rest keys)
  "(pushnew X PLACE): insert X at the head of the list if not already there.
Like (push X PLACE), except that the list is unmodified if X is `eql' to
an element already on the list.
Keywords supported:  :test :test-not :key"
  (if (symbolp place) (list 'setq place (list* 'adjoin x place keys))
    (list* 'callf2 'adjoin x place keys)))

(defun cl-set-elt (seq n val)
  (if (listp seq) (setcar (nthcdr n seq) val) (aset seq n val)))

(defun cl-set-nthcdr (n list x)
  (if (<= n 0) x (setcdr (nthcdr (1- n) list) x) list))

(defun cl-set-buffer-substring (start end val)
  (save-excursion (delete-region start end)
		  (goto-char start)
		  (insert val)
		  val))

(defun cl-set-substring (str start end val)
  (if end (if (< end 0) (incf end (length str)))
    (setq end (length str)))
  (if (< start 0) (incf start str))
  (concat (and (> start 0) (substring str 0 start))
	  val
	  (and (< end (length str)) (substring str end))))


;;; Control structures.

;;; These macros are so simple and so often-used that it's better to have
;;; them all the time than to load them from cl-macs.el.

(defun cl-map-extents (&rest cl-args)
  (apply 'cl-map-overlays cl-args))


;;; Blocks and exits.

(defalias 'cl-block-wrapper 'identity)
(defalias 'cl-block-throw 'throw)


;;; Multiple values.  True multiple values are not supported, or even
;;; simulated.  Instead, multiple-value-bind and friends simply expect
;;; the target form to return the values as a list.

(defsubst values (&rest values)
  "Return multiple values, Common Lisp style.
The arguments of `values' are the values
that the containing function should return."
  (apply 'list values))

(defsubst values-list (list)
  "Return multiple values, Common Lisp style, taken from a list.
LIST specifies the list of values
that the containing function should return."
  list)

(defsubst multiple-value-list (expression)
  "Return a list of the multiple values produced by EXPRESSION.
This handles multiple values in Common Lisp style, but it does not
work right when EXPRESSION calls an ordinary Emacs Lisp function
that returns just one value."
  expression)

(defsubst multiple-value-apply (function expression)
  "Evaluate EXPRESSION to get multiple values and apply FUNCTION to them.
This handles multiple values in Common Lisp style, but it does not work
right when EXPRESSION calls an ordinary Emacs Lisp function that returns just
one value."
  (apply function expression))

(defsubst nth-value (n expression)
  "Evaluate EXPRESSION to get multiple values and return the Nth one.
This handles multiple values in Common Lisp style, but it does not work
right when EXPRESSION calls an ordinary Emacs Lisp function that returns just
one value."
  (nth n expression))

;;; Macros.

(defvar cl-macro-environment nil)
(defvar cl-old-macroexpand (prog1 (symbol-function 'macroexpand)
			     (defalias 'macroexpand 'cl-macroexpand)))

(defun cl-macroexpand (cl-macro &optional cl-env)
  "Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((cl-macro-environment cl-env))
    (while (progn (setq cl-macro (funcall cl-old-macroexpand cl-macro cl-env))
		  (and (symbolp cl-macro)
		       (cdr (assq (symbol-name cl-macro) cl-env))))
      (setq cl-macro (cadr (assq (symbol-name cl-macro) cl-env))))
    cl-macro))


;;; Declarations.

(defvar cl-compiling-file nil)
(defun cl-compiling-file ()
  (or cl-compiling-file
      (and (boundp 'outbuffer) (bufferp (symbol-value 'outbuffer))
	   (equal (buffer-name (symbol-value 'outbuffer))
		  " *Compiler Output*"))))

(defvar cl-proclaims-deferred nil)

(defun proclaim (spec)
  (if (fboundp 'cl-do-proclaim) (cl-do-proclaim spec t)
    (push spec cl-proclaims-deferred))
  nil)

(defmacro declaim (&rest specs)
  (let ((body (mapcar (function (lambda (x) (list 'proclaim (list 'quote x))))
		      specs)))
    (if (cl-compiling-file) (list* 'eval-when '(compile load eval) body)
      (cons 'progn body))))   ; avoid loading cl-macs.el for eval-when


;;; Symbols.

(defun cl-random-time ()
  (let* ((time (copy-sequence (current-time-string))) (i (length time)) (v 0))
    (while (>= (decf i) 0) (setq v (+ (* v 3) (aref time i))))
    v))

(defvar *gensym-counter* (* (logand (cl-random-time) 1023) 100))


;;; Numbers.

(defun floatp-safe (x)
  "T if OBJECT is a floating point number.
On Emacs versions that lack floating-point support, this function
always returns nil."
  (and (numberp x) (not (integerp x))))

(defun plusp (x)
  "T if NUMBER is positive."
  (> x 0))

(defun minusp (x)
  "T if NUMBER is negative."
  (< x 0))

(defun oddp (x)
  "T if INTEGER is odd."
  (eq (logand x 1) 1))

(defun evenp (x)
  "T if INTEGER is even."
  (eq (logand x 1) 0))

(defvar *random-state* (vector 'cl-random-state-tag -1 30 (cl-random-time)))

;;; The following are actually set by cl-float-limits.
(defconst most-positive-float nil)
(defconst most-negative-float nil)
(defconst least-positive-float nil)
(defconst least-negative-float nil)
(defconst least-positive-normalized-float nil)
(defconst least-negative-normalized-float nil)
(defconst float-epsilon nil)
(defconst float-negative-epsilon nil)


;;; Sequence functions.

(defalias 'copy-seq 'copy-sequence)

(defun mapcar* (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types."
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (cl-mapcar-many cl-func (cons cl-x cl-rest))
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x)))

(defalias 'svref 'aref)

;;; List functions.

(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'rest 'cdr)
(defalias 'endp 'null)

(defun third (x)
  "Return the third element of the list X."
  (car (cdr (cdr x))))

(defun fourth (x)
  "Return the fourth element of the list X."
  (nth 3 x))

(defun fifth (x)
  "Return the fifth element of the list X."
  (nth 4 x))

(defun sixth (x)
  "Return the sixth element of the list X."
  (nth 5 x))

(defun seventh (x)
  "Return the seventh element of the list X."
  (nth 6 x))

(defun eighth (x)
  "Return the eighth element of the list X."
  (nth 7 x))

(defun ninth (x)
  "Return the ninth element of the list X."
  (nth 8 x))

(defun tenth (x)
  "Return the tenth element of the list X."
  (nth 9 x))

(defun caaar (x)
  "Return the `car' of the `car' of the `car' of X."
  (car (car (car x))))

(defun caadr (x)
  "Return the `car' of the `car' of the `cdr' of X."
  (car (car (cdr x))))

(defun cadar (x)
  "Return the `car' of the `cdr' of the `car' of X."
  (car (cdr (car x))))

(defun caddr (x)
  "Return the `car' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr x))))

(defun cdaar (x)
  "Return the `cdr' of the `car' of the `car' of X."
  (cdr (car (car x))))

(defun cdadr (x)
  "Return the `cdr' of the `car' of the `cdr' of X."
  (cdr (car (cdr x))))

(defun cddar (x)
  "Return the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (car x))))

(defun cdddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr x))))

(defun caaaar (x)
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (car (car (car (car x)))))

(defun caaadr (x)
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (car (car (car (cdr x)))))

(defun caadar (x)
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (car (car (cdr (car x)))))

(defun caaddr (x)
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (car (car (cdr (cdr x)))))

(defun cadaar (x)
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (car (cdr (car (car x)))))

(defun cadadr (x)
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (car (cdr (car (cdr x)))))

(defun caddar (x)
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (car (cdr (cdr (car x)))))

(defun cadddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr (cdr x)))))

(defun cdaaar (x)
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (cdr (car (car (car x)))))

(defun cdaadr (x)
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (cdr (car (car (cdr x)))))

(defun cdadar (x)
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (cdr (car (cdr (car x)))))

(defun cdaddr (x)
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (cdr (car (cdr (cdr x)))))

(defun cddaar (x)
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (cdr (cdr (car (car x)))))

(defun cddadr (x)
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (cdr (cdr (car (cdr x)))))

(defun cdddar (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (cdr (car x)))))

(defun cddddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr (cdr x)))))

;;(defun last* (x &optional n)
;;  "Returns the last link in the list LIST.
;;With optional argument N, returns Nth-to-last link (default 1)."
;;  (if n
;;      (let ((m 0) (p x))
;;	(while (consp p) (incf m) (pop p))
;;	(if (<= n 0) p
;;	  (if (< n m) (nthcdr (- m n) x) x)))
;;    (while (consp (cdr x)) (pop x))
;;    x))

(defun list* (arg &rest rest)   ; See compiler macro in cl-macs.el
  "Return a new list with specified args as elements, cons'd to last arg.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'."
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defun ldiff (list sublist)
  "Return a copy of LIST with the tail SUBLIST removed."
  (let ((res nil))
    (while (and (consp list) (not (eq list sublist)))
      (push (pop list) res))
    (nreverse res)))

(defun cl-maclisp-member (item list)
  (while (and list (not (equal item (car list)))) (setq list (cdr list)))
  list)

(defalias 'cl-member 'memq)   ; for compatibility with old CL package
(defalias 'cl-floor 'floor*)
(defalias 'cl-ceiling 'ceiling*)
(defalias 'cl-truncate 'truncate*)
(defalias 'cl-round 'round*)
(defalias 'cl-mod 'mod*)

(defun adjoin (cl-item cl-list &rest cl-keys)  ; See compiler macro in cl-macs
  "Return ITEM consed onto the front of LIST only if it's not already there.
Otherwise, return LIST unmodified.
Keywords supported:  :test :test-not :key"
  (cond ((or (equal cl-keys '(:test eq))
	     (and (null cl-keys) (not (numberp cl-item))))
	 (if (memq cl-item cl-list) cl-list (cons cl-item cl-list)))
	((or (equal cl-keys '(:test equal)) (null cl-keys))
	 (if (member cl-item cl-list) cl-list (cons cl-item cl-list)))
	(t (apply 'cl-adjoin cl-item cl-list cl-keys))))

(defun subst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (non-destructively).
Return a copy of TREE with all elements `eql' to OLD replaced by NEW.
Keywords supported:  :test :test-not :key"
  (if (or cl-keys (and (numberp cl-old) (not (integerp cl-old))))
      (apply 'sublis (list (cons cl-old cl-new)) cl-tree cl-keys)
    (cl-do-subst cl-new cl-old cl-tree)))

(defun cl-do-subst (cl-new cl-old cl-tree)
  (cond ((eq cl-tree cl-old) cl-new)
	((consp cl-tree)
	 (let ((a (cl-do-subst cl-new cl-old (car cl-tree)))
	       (d (cl-do-subst cl-new cl-old (cdr cl-tree))))
	   (if (and (eq a (car cl-tree)) (eq d (cdr cl-tree)))
	       cl-tree (cons a d))))
	(t cl-tree)))

(defun acons (a b c) (cons (cons a b) c))
(defun pairlis (a b &optional c) (nconc (mapcar* 'cons a b) c))


;;; Miscellaneous.

(put 'cl-assertion-failed 'error-conditions '(error))
(put 'cl-assertion-failed 'error-message "Assertion failed")

(defvar cl-fake-autoloads nil
  "Non-nil means don't make CL functions autoload.")

;;; Autoload the other portions of the package.
;; We want to replace the basic versions of dolist, dotimes below.
(fmakunbound 'dolist)
(fmakunbound 'dotimes)
(mapcar (function
	 (lambda (set)
	   (let ((file (if cl-fake-autoloads "<none>" (car set))))
	     (mapcar (function
		      (lambda (func)
			(autoload func (car set) nil nil (nth 1 set))))
		     (cddr set)))))
	'(("cl-extra" nil
	   coerce equalp cl-map-keymap maplist mapc mapl mapcan mapcon
	   cl-map-keymap cl-map-keymap-recursively cl-map-intervals
	   cl-map-overlays cl-set-frame-visible-p cl-float-limits
	   gcd lcm isqrt floor* ceiling* truncate* round*
	   mod* rem* signum random* make-random-state random-state-p
	   subseq concatenate cl-mapcar-many map some every notany
	   notevery revappend nreconc list-length tailp copy-tree get* getf
	   cl-set-getf cl-do-remf remprop cl-make-hash-table cl-hash-lookup
	   cl-gethash cl-puthash cl-remhash cl-clrhash cl-maphash cl-hash-table-p
	   cl-hash-table-count cl-progv-before cl-prettyexpand
	   cl-macroexpand-all)
	  ("cl-seq" nil
	   reduce fill replace remove* remove-if remove-if-not
	   delete* delete-if delete-if-not remove-duplicates
	   delete-duplicates substitute substitute-if substitute-if-not
	   nsubstitute nsubstitute-if nsubstitute-if-not find find-if
	   find-if-not position position-if position-if-not count count-if
	   count-if-not mismatch search sort* stable-sort merge member*
	   member-if member-if-not cl-adjoin assoc* assoc-if assoc-if-not
	   rassoc* rassoc-if rassoc-if-not union nunion intersection
	   nintersection set-difference nset-difference set-exclusive-or
	   nset-exclusive-or subsetp subst-if subst-if-not nsubst nsubst-if
	   nsubst-if-not sublis nsublis tree-equal)
	  ("cl-macs" nil
	   gensym gentemp typep cl-do-pop get-setf-method
	   cl-struct-setf-expander compiler-macroexpand cl-compile-time-init)
	  ("cl-macs" t
	   defun* defmacro* function* destructuring-bind eval-when
	   load-time-value case ecase typecase etypecase
	   block return return-from loop do do* dolist dotimes do-symbols
	   do-all-symbols psetq progv flet labels macrolet symbol-macrolet
	   lexical-let lexical-let* multiple-value-bind multiple-value-setq
	   locally the declare define-setf-method defsetf define-modify-macro
	   setf psetf remf shiftf rotatef letf letf* callf callf2 defstruct
	   check-type assert ignore-errors define-compiler-macro)))

;;; Define data for indentation and edebug.
(mapcar (function
	 (lambda (entry)
	   (mapcar (function
		    (lambda (func)
		      (put func 'lisp-indent-function (nth 1 entry))
		      (put func 'lisp-indent-hook (nth 1 entry))
		      (or (get func 'edebug-form-spec)
			  (put func 'edebug-form-spec (nth 2 entry)))))
		   (car entry))))
	'(((defun* defmacro*) 2)
	  ((function*) nil
	   (&or symbolp ([&optional 'macro] 'lambda (&rest sexp) &rest form)))
	  ((eval-when) 1 (sexp &rest form))
	  ((declare) nil (&rest sexp))
	  ((the) 1 (sexp &rest form))
	  ((case ecase typecase etypecase) 1 (form &rest (sexp &rest form)))
	  ((block return-from) 1 (sexp &rest form))
	  ((return) nil (&optional form))
	  ((do do*) 2 ((&rest &or symbolp (symbolp &optional form form))
		       (form &rest form)
		       &rest form))
	  ((do-symbols) 1 ((symbolp form &optional form form) &rest form))
	  ((do-all-symbols) 1 ((symbolp form &optional form) &rest form))
	  ((psetq setf psetf) nil edebug-setq-form)
	  ((progv) 2 (&rest form))
	  ((flet labels macrolet) 1
	   ((&rest (sexp sexp &rest form)) &rest form))
	  ((symbol-macrolet lexical-let lexical-let*) 1
	   ((&rest &or symbolp (symbolp form)) &rest form))
	  ((multiple-value-bind) 2 ((&rest symbolp) &rest form))
	  ((multiple-value-setq) 1 ((&rest symbolp) &rest form))
	  ((incf decf remf pushnew shiftf rotatef) nil (&rest form))
	  ((letf letf*) 1 ((&rest (&rest form)) &rest form))
	  ((callf destructuring-bind) 2 (sexp form &rest form))
	  ((callf2) 3 (sexp form form &rest form))
	  ((loop) nil (&rest &or symbolp form))
	  ((ignore-errors) 0 (&rest form))))


;;; This goes here so that cl-macs can find it if it loads right now.
(provide 'cl-19)     ; usage: (require 'cl-19 "cl")


;;; Things to do after byte-compiler is loaded.
;;; As a side effect, we cause cl-macs to be loaded when compiling, so
;;; that the compiler-macros defined there will be present.

(defvar cl-hacked-flag nil)
(defun cl-hack-byte-compiler ()
  (if (and (not cl-hacked-flag) (fboundp 'byte-compile-file-form))
      (progn
	(cl-compile-time-init)   ; in cl-macs.el
	(setq cl-hacked-flag t))))

;;; Try it now in case the compiler has already been loaded.
(cl-hack-byte-compiler)

;;; Also make a hook in case compiler is loaded after this file.
(add-hook 'bytecomp-load-hook 'cl-hack-byte-compiler)


;;; The following ensures that packages which expect the old-style cl.el
;;; will be happy with this one.

(provide 'cl)

(run-hooks 'cl-load-hook)

;;; cl.el ends here
