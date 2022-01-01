;;; cl-lib.el --- Common Lisp extensions for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1993, 2001-2022 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 1.0
;; Keywords: extensions

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

;; This file contains the portions of the Common Lisp extensions
;; package which should always be present.


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

(require 'macroexp)

(defvar cl--optimize-speed 1)
(defvar cl--optimize-safety 1)

;;;###autoload
(define-obsolete-variable-alias
  ;; This alias is needed for compatibility with .elc files that use defstruct
  ;; and were compiled with Emacs<24.3.
  'custom-print-functions 'cl-custom-print-functions "24.3")

;;;###autoload
(defvar cl-custom-print-functions nil
  "This is a list of functions that format user objects for printing.
Each function is called in turn with three arguments: the object, the
stream, and the print level (currently ignored).  If it is able to
print the object it returns true; otherwise it returns nil and the
printer proceeds to the next function on the list.

This variable is not used at present, but it is defined in hopes that
a future Emacs interpreter will be able to use it.")

;;; Generalized variables.
;; These macros are defined here so that they
;; can safely be used in init files.

;;;###autoload
(defmacro cl-incf (place &optional x)
  "Increment PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the incremented value of PLACE."
  (declare (debug (place &optional form)))
  (if (symbolp place)
      (list 'setq place (if x (list '+ place x) (list '1+ place)))
    (list 'cl-callf '+ place (or x 1))))

(defmacro cl-decf (place &optional x)
  "Decrement PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the decremented value of PLACE."
  (declare (debug cl-incf))
  (if (symbolp place)
      (list 'setq place (if x (list '- place x) (list '1- place)))
    (list 'cl-callf '- place (or x 1))))

(defmacro cl-pushnew (x place &rest keys)
  "Add X to the list stored in PLACE unless X is already in the list.
PLACE is a generalized variable that stores a list.

Like (push X PLACE), except that PLACE is unmodified if X is `eql'
to an element already in the list stored in PLACE.

\nKeywords supported:  :test :test-not :key
\n(fn X PLACE [KEYWORD VALUE]...)"
  (declare (debug
            (form place &rest
                  &or [[&or ":test" ":test-not" ":key"] form]
                  [keywordp form])))
  (if (symbolp place)
      (if (null keys)
          (macroexp-let2 nil var x
            `(if (memql ,var ,place)
                 ;; This symbol may later on expand to actual code which then
                 ;; trigger warnings like "value unused" since cl-pushnew's
                 ;; return value is rarely used.  It should not matter that
                 ;; other warnings may be silenced, since `place' is used
                 ;; earlier and should have triggered them already.
                 (with-no-warnings ,place)
               (setq ,place (cons ,var ,place))))
	`(setq ,place (cl-adjoin ,x ,place ,@keys)))
    `(cl-callf2 cl-adjoin ,x ,place ,@keys)))

(defun cl--set-buffer-substring (start end val)
  (save-excursion (delete-region start end)
		  (goto-char start)
		  (insert val)
		  val))

(defun cl--set-substring (str start end val)
  (if end (if (< end 0) (cl-incf end (length str)))
    (setq end (length str)))
  (if (< start 0) (cl-incf start (length str)))
  (concat (and (> start 0) (substring str 0 start))
	  val
	  (and (< end (length str)) (substring str end))))


;;; Blocks and exits.

(defalias 'cl--block-wrapper 'identity)
(defalias 'cl--block-throw 'throw)


;;; Multiple values.
;; True multiple values are not supported, or even
;; simulated.  Instead, cl-multiple-value-bind and friends simply expect
;; the target form to return the values as a list.

(defun cl--defalias (cl-f el-f &optional doc)
  (defalias cl-f el-f doc)
  (put cl-f 'byte-optimizer 'byte-compile-inline-expand))

(cl--defalias 'cl-values #'list
  "Return multiple values, Common Lisp style.
The arguments of `cl-values' are the values
that the containing function should return.

\(fn &rest VALUES)")

(defun cl-values-list (list)
  "Return multiple values, Common Lisp style, taken from a list.
LIST specifies the list of values that the containing function
should return.

Note that Emacs Lisp doesn't really support multiple values, so
all this function does is return LIST."
  (unless (listp list)
    (signal 'wrong-type-argument list))
  list)

(defsubst cl-multiple-value-list (expression)
  "Return a list of the multiple values produced by EXPRESSION.
This handles multiple values in Common Lisp style, but it does not
work right when EXPRESSION calls an ordinary Emacs Lisp function
that returns just one value."
  expression)

(defsubst cl-multiple-value-apply (function expression)
  "Evaluate EXPRESSION to get multiple values and apply FUNCTION to them.
This handles multiple values in Common Lisp style, but it does not work
right when EXPRESSION calls an ordinary Emacs Lisp function that returns just
one value."
  (apply function expression))

(defalias 'cl-multiple-value-call 'apply
  "Apply FUNCTION to ARGUMENTS, taking multiple values into account.
This implementation only handles the case where there is only one argument.")

(cl--defalias 'cl-nth-value #'nth
  "Evaluate EXPRESSION to get multiple values and return the Nth one.
This handles multiple values in Common Lisp style, but it does not work
right when EXPRESSION calls an ordinary Emacs Lisp function that returns just
one value.

\(fn N EXPRESSION)")

;;; Declarations.

(define-obsolete-function-alias 'cl--compiling-file
  #'macroexp-compiling-p "28.1")

(defvar cl--proclaims-deferred nil)

(defun cl-proclaim (spec)
  "Record a global declaration specified by SPEC."
  (if (fboundp 'cl--do-proclaim) (cl--do-proclaim spec t)
    (push spec cl--proclaims-deferred))
  nil)

(defmacro cl-declaim (&rest specs)
  "Like `cl-proclaim', but takes any number of unevaluated, unquoted arguments.
Puts `(cl-eval-when (compile load eval) ...)' around the declarations
so that they are registered at compile-time as well as run-time."
  (let ((body (mapcar (lambda (x) `(cl-proclaim ',x)) specs)))
    (if (macroexp-compiling-p) `(cl-eval-when (compile load eval) ,@body)
      `(progn ,@body))))           ; Avoid loading cl-macs.el for cl-eval-when.


;;; Numbers.

(define-obsolete-function-alias 'cl-floatp-safe 'floatp "24.4")

(defsubst cl-plusp (number)
  "Return t if NUMBER is positive."
  (> number 0))

(defsubst cl-minusp (number)
  "Return t if NUMBER is negative."
  (< number 0))

(defun cl-oddp (integer)
  "Return t if INTEGER is odd."
  (eq (logand integer 1) 1))

(defun cl-evenp (integer)
  "Return t if INTEGER is even."
  (eq (logand integer 1) 0))

(defconst cl-digit-char-table
  (let* ((digits (make-vector 256 nil))
         (populate (lambda (start end base)
                     (mapc (lambda (i)
                             (aset digits i (+ base (- i start))))
                           (number-sequence start end)))))
    (funcall populate ?0 ?9 0)
    (funcall populate ?A ?Z 10)
    (funcall populate ?a ?z 10)
    digits))

(defun cl-digit-char-p (char &optional radix)
  "Test if CHAR is a digit in the specified RADIX (default 10).
If true return the decimal value of digit CHAR in RADIX."
  (or (<= 2 (or radix 10) 36)
      (signal 'args-out-of-range (list 'radix radix '(2 36))))
  (let ((n (aref cl-digit-char-table char)))
    (and n (< n (or radix 10)) n)))

(defconst cl-most-positive-float nil
  "The largest value that a Lisp float can hold.
If your system supports infinities, this is the largest finite value.
For Emacs, this equals 1.7976931348623157e+308.
Call `cl-float-limits' to set this.")

(defconst cl-most-negative-float nil
  "The largest negative value that a Lisp float can hold.
This is simply -`cl-most-positive-float'.
Call `cl-float-limits' to set this.")

(defconst cl-least-positive-float nil
  "The smallest value greater than zero that a Lisp float can hold.
For Emacs, this equals 5e-324 if subnormal numbers are supported,
`cl-least-positive-normalized-float' if they are not.
Call `cl-float-limits' to set this.")

(defconst cl-least-negative-float nil
  "The smallest value less than zero that a Lisp float can hold.
This is simply -`cl-least-positive-float'.
Call `cl-float-limits' to set this.")

(defconst cl-least-positive-normalized-float nil
  "The smallest normalized Lisp float greater than zero.
This is the smallest value that has full precision.
For Emacs, this equals 2.2250738585072014e-308.
Call `cl-float-limits' to set this.")

(defconst cl-least-negative-normalized-float nil
  "The smallest normalized Lisp float less than zero.
This is simply -`cl-least-positive-normalized-float'.
Call `cl-float-limits' to set this.")

(defconst cl-float-epsilon nil
  "The smallest positive float that adds to 1.0 to give a distinct value.
Adding a number less than this to 1.0 returns 1.0 due to roundoff.
For Emacs, this equals 2.220446049250313e-16.
Call `cl-float-limits' to set this.")

(defconst cl-float-negative-epsilon nil
  "The smallest positive float that subtracts from 1.0 to give a distinct value.
For Emacs, this equals 1.1102230246251565e-16.
Call `cl-float-limits' to set this.")


;;; Sequence functions.

(cl--defalias 'cl-copy-seq 'copy-sequence)

(declare-function cl--mapcar-many "cl-extra" (cl-func cl-seqs &optional acc))

(defun cl-mapcar (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (cl--mapcar-many cl-func (cons cl-x cl-rest) 'accumulate)
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x)))

(cl--defalias 'cl-svref 'aref)

;;; List functions.

(cl--defalias 'cl-first 'car)
(cl--defalias 'cl-second 'cadr)
(cl--defalias 'cl-rest 'cdr)

(cl--defalias 'cl-third 'cl-caddr "Return the third element of the list X.")
(cl--defalias 'cl-fourth 'cl-cadddr "Return the fourth element of the list X.")

(defsubst cl-fifth (x)
  "Return the fifth element of the list X."
  (declare (gv-setter (lambda (store) `(setcar (nthcdr 4 ,x) ,store))))
  (nth 4 x))

(defsubst cl-sixth (x)
  "Return the sixth element of the list X."
  (declare (gv-setter (lambda (store) `(setcar (nthcdr 5 ,x) ,store))))
  (nth 5 x))

(defsubst cl-seventh (x)
  "Return the seventh element of the list X."
  (declare (gv-setter (lambda (store) `(setcar (nthcdr 6 ,x) ,store))))
  (nth 6 x))

(defsubst cl-eighth (x)
  "Return the eighth element of the list X."
  (declare (gv-setter (lambda (store) `(setcar (nthcdr 7 ,x) ,store))))
  (nth 7 x))

(defsubst cl-ninth (x)
  "Return the ninth element of the list X."
  (declare (gv-setter (lambda (store) `(setcar (nthcdr 8 ,x) ,store))))
  (nth 8 x))

(defsubst cl-tenth (x)
  "Return the tenth element of the list X."
  (declare (gv-setter (lambda (store) `(setcar (nthcdr 9 ,x) ,store))))
  (nth 9 x))

(defalias 'cl-caaar 'caaar)
(defalias 'cl-caadr 'caadr)
(defalias 'cl-cadar 'cadar)
(defalias 'cl-caddr 'caddr)
(defalias 'cl-cdaar 'cdaar)
(defalias 'cl-cdadr 'cdadr)
(defalias 'cl-cddar 'cddar)
(defalias 'cl-cdddr 'cdddr)
(defalias 'cl-caaaar 'caaaar)
(defalias 'cl-caaadr 'caaadr)
(defalias 'cl-caadar 'caadar)
(defalias 'cl-caaddr 'caaddr)
(defalias 'cl-cadaar 'cadaar)
(defalias 'cl-cadadr 'cadadr)
(defalias 'cl-caddar 'caddar)
(defalias 'cl-cadddr 'cadddr)
(defalias 'cl-cdaaar 'cdaaar)
(defalias 'cl-cdaadr 'cdaadr)
(defalias 'cl-cdadar 'cdadar)
(defalias 'cl-cdaddr 'cdaddr)
(defalias 'cl-cddaar 'cddaar)
(defalias 'cl-cddadr 'cddadr)
(defalias 'cl-cdddar 'cdddar)
(defalias 'cl-cddddr 'cddddr)

;;(defun last* (x &optional n)
;;  "Returns the last link in the list LIST.
;;With optional argument N, returns Nth-to-last link (default 1)."
;;  (if n
;;      (let ((m 0) (p x))
;;	(while (consp p) (cl-incf m) (pop p))
;;	(if (<= n 0) p
;;	  (if (< n m) (nthcdr (- m n) x) x)))
;;    (while (consp (cdr x)) (pop x))
;;    x))

(defun cl-list* (arg &rest rest)
  "Return a new list with specified ARGs as elements, consed to last ARG.
Thus, `(cl-list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'.
\n(fn ARG...)"
  (declare (compiler-macro cl--compiler-macro-list*))
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defun cl-ldiff (list sublist)
  "Return a copy of LIST with the tail SUBLIST removed."
  (let ((res nil))
    (while (and (consp list) (not (eq list sublist)))
      (push (pop list) res))
    (nreverse res)))

(defun cl-copy-list (list)
  "Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))

;; Autoloaded, but we have not loaded cl-loaddefs yet.
(declare-function cl-floor "cl-extra" (x &optional y))
(declare-function cl-ceiling "cl-extra" (x &optional y))
(declare-function cl-truncate "cl-extra" (x &optional y))
(declare-function cl-round "cl-extra" (x &optional y))
(declare-function cl-mod "cl-extra" (x y))

(defun cl-adjoin (cl-item cl-list &rest cl-keys)
  "Return ITEM consed onto the front of LIST only if it's not already there.
Otherwise, return LIST unmodified.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (declare (compiler-macro cl--compiler-macro-adjoin))
  (cond ((or (equal cl-keys '(:test eq))
	     (and (null cl-keys) (not (numberp cl-item))))
	 (if (memq cl-item cl-list) cl-list (cons cl-item cl-list)))
	((or (equal cl-keys '(:test equal)) (null cl-keys))
	 (if (member cl-item cl-list) cl-list (cons cl-item cl-list)))
	(t (apply 'cl--adjoin cl-item cl-list cl-keys))))

(defun cl-subst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (non-destructively).
Return a copy of TREE with all elements `eql' to OLD replaced by NEW.
\nKeywords supported:  :test :test-not :key
\n(fn NEW OLD TREE [KEYWORD VALUE]...)"
  (if (or cl-keys (and (numberp cl-old) (not (integerp cl-old))))
      (apply 'cl-sublis (list (cons cl-old cl-new)) cl-tree cl-keys)
    (cl--do-subst cl-new cl-old cl-tree)))

(defun cl--do-subst (cl-new cl-old cl-tree)
  (cond ((eq cl-tree cl-old) cl-new)
	((consp cl-tree)
	 (let ((a (cl--do-subst cl-new cl-old (car cl-tree)))
	       (d (cl--do-subst cl-new cl-old (cdr cl-tree))))
	   (if (and (eq a (car cl-tree)) (eq d (cdr cl-tree)))
	       cl-tree (cons a d))))
	(t cl-tree)))

(defun cl-acons (key value alist)
  "Add KEY and VALUE to ALIST.
Return a new list with (cons KEY VALUE) as car and ALIST as cdr."
  (cons (cons key value) alist))

(defun cl-pairlis (keys values &optional alist)
  "Make an alist from KEYS and VALUES.
Return a new alist composed by associating KEYS to corresponding VALUES;
the process stops as soon as KEYS or VALUES run out.
If ALIST is non-nil, the new pairs are prepended to it."
  (nconc (cl-mapcar 'cons keys values) alist))

;;; Miscellaneous.

(provide 'cl-lib)
(unless (load "cl-loaddefs" 'noerror 'quiet)
  ;; When bootstrapping, cl-loaddefs hasn't been built yet!
  (require 'cl-macs)
  (require 'cl-seq))

(defun cl--old-struct-type-of (orig-fun object)
  (or (and (vectorp object) (> (length object) 0)
           (let ((tag (aref object 0)))
             (when (and (symbolp tag)
                        (string-prefix-p "cl-struct-" (symbol-name tag)))
               (unless (eq (symbol-function tag)
                           :quick-object-witness-check)
                 ;; Old-style old-style struct:
                 ;; Convert to new-style old-style struct!
                 (let* ((type (intern (substring (symbol-name tag)
                                                 (length "cl-struct-"))))
                        (class (cl--struct-get-class type)))
                   ;; If the `cl-defstruct' was recompiled after the code
                   ;; which constructed `object', `cl--struct-get-class' may
                   ;; not have called `cl-struct-define' and setup the tag
                   ;; symbol for us.
                   (unless (eq (symbol-function tag)
                               :quick-object-witness-check)
                     (set tag class)
                     (fset tag :quick-object-witness-check))))
               (cl--class-name (symbol-value tag)))))
      (funcall orig-fun object)))

;;;###autoload
(define-minor-mode cl-old-struct-compat-mode
  "Enable backward compatibility with old-style structs.
This can be needed when using code byte-compiled using the old
macro-expansion of `cl-defstruct' that used vectors objects instead
of record objects."
  :global t
  :group 'tools
  (cond
   (cl-old-struct-compat-mode
    (advice-add 'type-of :around #'cl--old-struct-type-of))
   (t
    (advice-remove 'type-of #'cl--old-struct-type-of))))

;;; cl-lib.el ends here
