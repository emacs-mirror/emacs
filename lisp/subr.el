;;; subr.el --- basic lisp subroutines for Emacs

;; Copyright (C) 1985, 1986, 1992, 1994, 1995, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(defvar custom-declare-variable-list nil
  "Record `defcustom' calls made before `custom.el' is loaded to handle them.
Each element of this list holds the arguments to one call to `defcustom'.")

;; Use this, rather than defcustom, in subr.el and other files loaded
;; before custom.el.
(defun custom-declare-variable-early (&rest arguments)
  (setq custom-declare-variable-list
	(cons arguments custom-declare-variable-list)))


;;;; Lisp language features.

(defalias 'not 'null)

(defmacro noreturn (form)
  "Evaluates FORM, with the expectation that the evaluation will signal an error
instead of returning to its caller.  If FORM does return, an error is
signaled."
  `(prog1 ,form
     (error "Form marked with `noreturn' did return")))

(defmacro 1value (form)
  "Evaluates FORM, with the expectation that the same value will be returned
from all evaluations of FORM.  This is the global do-nothing
version of `1value'.  There is also `testcover-1value' that
complains if FORM ever does return differing values."
  form)

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
`funcall' or `mapcar', etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of Lisp expressions.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

(defmacro push (newelt listname)
  "Add NEWELT to the list stored in the symbol LISTNAME.
This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
LISTNAME must be a symbol."
  (declare (debug (form sexp)))
  (list 'setq listname
	(list 'cons newelt listname)))

(defmacro pop (listname)
  "Return the first element of LISTNAME's value, and remove it from the list.
LISTNAME must be a symbol whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list."
  (declare (debug (sexp)))
  (list 'car
	(list 'prog1 listname
	      (list 'setq listname (list 'cdr listname)))))

(defmacro when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil."
  (declare (indent 1) (debug t))
  (list 'if cond (cons 'progn body)))

(defmacro unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil."
  (declare (indent 1) (debug t))
  (cons 'if (cons cond (cons nil body))))

(defmacro dolist (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (let ((temp (make-symbol "--dolist-temp--")))
    `(let ((,temp ,(nth 1 spec))
	   ,(car spec))
       (while ,temp
	 (setq ,(car spec) (car ,temp))
	 (setq ,temp (cdr ,temp))
	 ,@body)
       ,@(if (cdr (cdr spec))
	     `((setq ,(car spec) nil) ,@(cdr (cdr spec)))))))

(defmacro dotimes (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY with VAR bound to successive integers running from 0,
inclusive, to COUNT, exclusive.  Then evaluate RESULT to get
the return value (nil if RESULT is omitted).

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent 1) (debug dolist))
  (let ((temp (make-symbol "--dotimes-temp--"))
	(start 0)
	(end (nth 1 spec)))
    `(let ((,temp ,end)
	   (,(car spec) ,start))
       (while (< ,(car spec) ,temp)
	 ,@body
	 (setq ,(car spec) (1+ ,(car spec))))
       ,@(cdr (cdr spec)))))

(defmacro declare (&rest specs)
  "Do not evaluate any arguments and return nil.
Treated as a declaration when used at the right place in a
`defmacro' form.  \(See Info anchor `(elisp)Definition of declare'.)"
  nil)

(defsubst caar (x)
  "Return the car of the car of X."
  (car (car x)))

(defsubst cadr (x)
  "Return the car of the cdr of X."
  (car (cdr x)))

(defsubst cdar (x)
  "Return the cdr of the car of X."
  (cdr (car x)))

(defsubst cddr (x)
  "Return the cdr of the cdr of X."
  (cdr (cdr x)))

(defun last (list &optional n)
  "Return the last link of LIST.  Its car is the last element.
If LIST is nil, return nil.
If N is non-nil, return the Nth-to-last link of LIST.
If N is bigger than the length of LIST, return LIST."
  (if n
      (let ((m 0) (p list))
	(while (consp p)
	  (setq m (1+ m) p (cdr p)))
	(if (<= n 0) p
	  (if (< n m) (nthcdr (- m n) list) list)))
    (while (consp (cdr list))
      (setq list (cdr list)))
    list))

(defun butlast (list &optional n)
  "Return a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) list
    (nbutlast (copy-sequence list) n)))

(defun nbutlast (list &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length list)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) list) nil))
	   list))))

(defun delete-dups (list)
  "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
  (let ((tail list))
    (while tail
      (setcdr tail (delete (car tail) (cdr tail)))
      (setq tail (cdr tail))))
  list)

(defun number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))

(defun remove (elt seq)
  "Return a copy of SEQ with all occurrences of ELT removed.
SEQ must be a list, vector, or string.  The comparison is done with `equal'."
  (if (nlistp seq)
      ;; If SEQ isn't a list, there's no need to copy SEQ because
      ;; `delete' will return a new object.
      (delete elt seq)
    (delete elt (copy-sequence seq))))

(defun remq (elt list)
  "Return LIST with all occurrences of ELT removed.
The comparison is done with `eq'.  Contrary to `delq', this does not use
side-effects, and the argument LIST is not modified."
  (if (memq elt list)
      (delq elt (copy-sequence list))
    list))

(defun copy-tree (tree &optional vecp)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and its cdr.
Contrast to `copy-sequence', which copies only along the cdrs.  With second
argument VECP, this copies vectors as well as conses."
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree)) (and vecp (vectorp (car tree))))
		(setq newcar (copy-tree (car tree) vecp)))
	    (push newcar result))
	  (setq tree (cdr tree)))
	(nconc (nreverse result) tree))
    (if (and vecp (vectorp tree))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (copy-tree (aref tree i) vecp)))
	  tree)
      tree)))

(defun assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

(make-obsolete 'assoc-ignore-case 'assoc-string)
(defun assoc-ignore-case (key alist)
  "Like `assoc', but ignores differences in case and text representation.
KEY must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
  (assoc-string key alist t))

(make-obsolete 'assoc-ignore-representation 'assoc-string)
(defun assoc-ignore-representation (key alist)
  "Like `assoc', but ignores differences in text representation.
KEY must be a string.
Unibyte strings are converted to multibyte for comparison."
  (assoc-string key alist nil))

(defun member-ignore-case (elt list)
  "Like `member', but ignores differences in case and text representation.
ELT must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison.
Non-strings in LIST are ignored."
  (while (and list
	      (not (and (stringp (car list))
			(eq t (compare-strings elt 0 nil (car list) 0 nil t)))))
    (setq list (cdr list)))
  list)

(defmacro with-lexical-binding (&rest body)
  "Execute the statements in BODY using lexical binding."
  `(let ((internal-interpreter-environment internal-interpreter-environment))
     (setq internal-interpreter-environment '(t))
     ,@body))


;;;; Keymap support.

(defun undefined ()
  (interactive)
  (ding))

;Prevent the \{...} documentation construct
;from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (define-key map [remap self-insert-command] 'undefined)
  (or nodigits
      (let (loop)
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (define-key map (char-to-string loop) 'digit-argument)
	  (setq loop (1+ loop))))))

(defvar key-substitution-in-progress nil
 "Used internally by `substitute-key-definition'.")

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys which are defined as OLDDEF in OLDMAP.

For most uses, it is simpler and safer to use command remappping like this:
  \(define-key KEYMAP [remap OLDDEF] NEWDEF)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (prefix1 (vconcat prefix [nil]))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(defun substitute-key-definition-key (defn olddef newdef prefix keymap)
  (let (inner-def skipped menu-item)
    ;; Find the actual command name within the binding.
    (if (eq (car-safe defn) 'menu-item)
	(setq menu-item defn defn (nth 2 defn))
      ;; Skip past menu-prompt.
      (while (stringp (car-safe defn))
	(push (pop defn) skipped))
      ;; Skip past cached key-equivalence data for menu items.
      (if (consp (car-safe defn))
	  (setq defn (cdr defn))))
    (if (or (eq defn olddef)
	    ;; Compare with equal if definition is a key sequence.
	    ;; That is useful for operating on function-key-map.
	    (and (or (stringp defn) (vectorp defn))
		 (equal defn olddef)))
	(define-key keymap prefix
	  (if menu-item
	      (let ((copy (copy-sequence menu-item)))
		(setcar (nthcdr 2 copy) newdef)
		copy)
	    (nconc (nreverse skipped) newdef)))
      ;; Look past a symbol that names a keymap.
      (setq inner-def
	    (and defn
		 (condition-case nil (indirect-function defn) (error defn))))
      ;; For nested keymaps, we use `inner-def' rather than `defn' so as to
      ;; avoid autoloading a keymap.  This is mostly done to preserve the
      ;; original non-autoloading behavior of pre-map-keymap times.
      (if (and (keymapp inner-def)
	       ;; Avoid recursively scanning
	       ;; where KEYMAP does not have a submap.
	       (let ((elt (lookup-key keymap prefix)))
		 (or (null elt) (natnump elt) (keymapp elt)))
	       ;; Avoid recursively rescanning keymap being scanned.
	       (not (memq inner-def key-substitution-in-progress)))
	  ;; If this one isn't being scanned already, scan it now.
	  (substitute-key-definition olddef newdef keymap inner-def prefix)))))

(defun define-key-after (keymap key definition &optional after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `define-key' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap matters when it is used as a menu."
  (unless after (setq after t))
  (or (keymapp keymap)
      (signal 'wrong-type-argument (list 'keymapp keymap)))
  (setq key
	(if (<= (length key) 1) (aref key 0)
	  (setq keymap (lookup-key keymap
				   (apply 'vector
					  (butlast (mapcar 'identity key)))))
	  (aref key (1- (length key)))))
  (let ((tail keymap) done inserted)
    (while (and (not done) tail)
      ;; Delete any earlier bindings for the same key.
      (if (eq (car-safe (car (cdr tail))) key)
	  (setcdr tail (cdr (cdr tail))))
      ;; If we hit an included map, go down that one.
      (if (keymapp (car tail)) (setq tail (car tail)))
      ;; When we reach AFTER's binding, insert the new binding after.
      ;; If we reach an inherited keymap, insert just before that.
      ;; If we reach the end of this keymap, insert at the end.
      (if (or (and (eq (car-safe (car tail)) after)
		   (not (eq after t)))
	      (eq (car (cdr tail)) 'keymap)
	      (null (cdr tail)))
	  (progn
	    ;; Stop the scan only if we find a parent keymap.
	    ;; Keep going past the inserted element
	    ;; so we can delete any duplications that come later.
	    (if (eq (car (cdr tail)) 'keymap)
		(setq done t))
	    ;; Don't insert more than once.
	    (or inserted
		(setcdr tail (cons (cons key definition) (cdr tail))))
	    (setq inserted t)))
      (setq tail (cdr tail)))))

(defun map-keymap-internal (function keymap &optional sort-first)
  "Implement `map-keymap' with sorting.
Don't call this function; it is for internal use only."
  (if sort-first
      (let (list)
	(map-keymap (lambda (a b) (push (cons a b) list))
		    keymap)
	(setq list (sort list
			 (lambda (a b)
			   (setq a (car a) b (car b))
			   (if (integerp a)
			       (if (integerp b) (< a b)
				 t)
			     (if (integerp b) t
			       (string< a b))))))
	(dolist (p list)
	  (funcall function (car p) (cdr p))))
    (map-keymap function keymap)))

(defmacro kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `edmacro-mode')."
  (read-kbd-macro keys))

(put 'keyboard-translate-table 'char-table-extra-slots 0)

(defun keyboard-translate (from to)
  "Translate character FROM to TO at a low level.
This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it."
  (or (char-table-p keyboard-translate-table)
      (setq keyboard-translate-table
	    (make-char-table 'keyboard-translate-table nil)))
  (aset keyboard-translate-table from to))


;;;; The global keymap tree.

;;; global-map, esc-map, and ctl-x-map have their values set up in
;;; keymap.c; we just give them docstrings here.

(defvar global-map nil
  "Default global keymap mapping Emacs keyboard input into commands.
The value is a keymap which is usually (but not necessarily) Emacs's
global map.")

(defvar esc-map nil
  "Default keymap for ESC (meta) commands.
The normal global definition of the character ESC indirects to this keymap.")

(defvar ctl-x-map nil
  "Default keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")

(defvar ctl-x-4-map (make-sparse-keymap)
  "Keymap for subcommands of C-x 4.")
(defalias 'ctl-x-4-prefix ctl-x-4-map)
(define-key ctl-x-map "4" 'ctl-x-4-prefix)

(defvar ctl-x-5-map (make-sparse-keymap)
  "Keymap for frame commands.")
(defalias 'ctl-x-5-prefix ctl-x-5-map)
(define-key ctl-x-map "5" 'ctl-x-5-prefix)


;;;; Event manipulation functions.

;; The call to `read' is to ensure that the value is computed at load time
;; and not compiled into the .elc file.  The value is negative on most
;; machines, but not on all!
(defconst listify-key-sequence-1 (logior 128 (read "?\\M-\\^@")))

(defun listify-key-sequence (key)
  "Convert a key sequence to a list of events."
  (if (vectorp key)
      (append key nil)
    (mapcar (function (lambda (c)
			(if (> c 127)
			    (logxor c listify-key-sequence-1)
			  c)))
	    key)))

(defsubst eventp (obj)
  "True if the argument is an event object."
  (or (and (integerp obj)
	   ;; Filter out integers too large to be events.
	   ;; M is the biggest modifier.
	   (zerop (logand obj (lognot (1- (lsh ?\M-\^@ 1)))))
	   (char-valid-p (event-basic-type obj)))
      (and (symbolp obj)
	   (get obj 'event-symbol-elements))
      (and (consp obj)
	   (symbolp (car obj))
	   (get (car obj) 'event-symbol-elements))))

(defun event-modifiers (event)
  "Return a list of symbols representing the modifier keys in event EVENT.
The elements of the list may include `meta', `control',
`shift', `hyper', `super', `alt', `click', `double', `triple', `drag',
and `down'.
EVENT may be an event or an event type.  If EVENT is a symbol
that has never been used in an event that has been read as input
in the current Emacs session, then this function can return nil,
even when EVENT actually has modifiers."
  (let ((type event))
    (if (listp type)
	(setq type (car type)))
    (if (symbolp type)
	(cdr (get type 'event-symbol-elements))
      (let ((list nil)
	    (char (logand type (lognot (logior ?\M-\^@ ?\C-\^@ ?\S-\^@
					       ?\H-\^@ ?\s-\^@ ?\A-\^@)))))
	(if (not (zerop (logand type ?\M-\^@)))
	    (push 'meta list))
	(if (or (not (zerop (logand type ?\C-\^@)))
		(< char 32))
	    (push 'control list))
	(if (or (not (zerop (logand type ?\S-\^@)))
		(/= char (downcase char)))
	    (push 'shift list))
	(or (zerop (logand type ?\H-\^@))
	    (push 'hyper list))
	(or (zerop (logand type ?\s-\^@))
	    (push 'super list))
	(or (zerop (logand type ?\A-\^@))
	    (push 'alt list))
	list))))

(defun event-basic-type (event)
  "Return the basic type of the given event (all modifiers removed).
The value is a printing character (not upper case) or a symbol.
EVENT may be an event or an event type.  If EVENT is a symbol
that has never been used in an event that has been read as input
in the current Emacs session, then this function may return nil."
  (if (consp event)
      (setq event (car event)))
  (if (symbolp event)
      (car (get event 'event-symbol-elements))
    (let* ((base (logand event (1- ?\A-\^@)))
	   (uncontrolled (if (< base 32) (logior base 64) base)))
      ;; There are some numbers that are invalid characters and
      ;; cause `downcase' to get an error.
      (condition-case ()
	  (downcase uncontrolled)
	(error uncontrolled)))))

(defsubst mouse-movement-p (object)
  "Return non-nil if OBJECT is a mouse movement event."
  (eq (car-safe object) 'mouse-movement))

(defsubst event-start (event)
  "Return the starting position of EVENT.
If EVENT is a mouse or key press or a mouse click, this returns the location
of the event.
If EVENT is a drag, this returns the drag's starting position.
The return value is of the form
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The `posn-' functions access elements of such lists."
  (if (consp event) (nth 1 event)
    (list (selected-window) (point) '(0 . 0) 0)))

(defsubst event-end (event)
  "Return the ending location of EVENT.
EVENT should be a click, drag, or key press event.
If EVENT is a click event, this function is the same as `event-start'.
The return value is of the form
   (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
    IMAGE (DX . DY) (WIDTH . HEIGHT))
The `posn-' functions access elements of such lists."
  (if (consp event) (nth (if (consp (nth 2 event)) 2 1) event)
    (list (selected-window) (point) '(0 . 0) 0)))

(defsubst event-click-count (event)
  "Return the multi-click count of EVENT, a click or drag event.
The return value is a positive integer."
  (if (and (consp event) (integerp (nth 2 event))) (nth 2 event) 1))

(defsubst posn-window (position)
  "Return the window in POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 0 position))

(defsubst posn-area (position)
  "Return the window area recorded in POSITION, or nil for the text area.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (let ((area (if (consp (nth 1 position))
		  (car (nth 1 position))
		(nth 1 position))))
    (and (symbolp area) area)))

(defsubst posn-point (position)
  "Return the buffer location in POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (or (nth 5 position)
      (if (consp (nth 1 position))
	  (car (nth 1 position))
	(nth 1 position))))

(defun posn-set-point (position)
  "Move point to POSITION.
Select the corresponding window as well."
    (if (not (windowp (posn-window position)))
	(error "Position not in text area of window"))
    (select-window (posn-window position))
    (if (numberp (posn-point position))
	(goto-char (posn-point position))))

(defsubst posn-x-y (position)
  "Return the x and y coordinates in POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 2 position))

(defun posn-col-row (position)
  "Return the nominal column and row in POSITION, measured in characters.
The column and row values are approximations calculated from the x
and y coordinates in POSITION and the frame's default character width
and height.
For a scroll-bar event, the result column is 0, and the row
corresponds to the vertical position of the click in the scroll bar.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (let* ((pair   (posn-x-y position))
	 (window (posn-window position))
	 (area   (posn-area position)))
    (cond
     ((null window)
      '(0 . 0))
     ((eq area 'vertical-scroll-bar)
      (cons 0 (scroll-bar-scale pair (1- (window-height window)))))
     ((eq area 'horizontal-scroll-bar)
      (cons (scroll-bar-scale pair (window-width window)) 0))
     (t
      (let* ((frame (if (framep window) window (window-frame window)))
	     (x (/ (car pair) (frame-char-width frame)))
	     (y (/ (cdr pair) (+ (frame-char-height frame)
				 (or (frame-parameter frame 'line-spacing)
				     default-line-spacing
				     0)))))
	(cons x y))))))

(defun posn-actual-col-row (position)
  "Return the actual column and row in POSITION, measured in characters.
These are the actual row number in the window and character number in that row.
Return nil if POSITION does not contain the actual position; in that case
`posn-col-row' can be used to get approximate values.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 6 position))

(defsubst posn-timestamp (position)
  "Return the timestamp of POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 3 position))

(defsubst posn-string (position)
  "Return the string object of POSITION, or nil if a buffer position.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 4 position))

(defsubst posn-image (position)
  "Return the image object of POSITION, or nil if a not an image.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 7 position))

(defsubst posn-object (position)
  "Return the object (image or string) of POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (or (posn-image position) (posn-string position)))

(defsubst posn-object-x-y (position)
  "Return the x and y coordinates relative to the object of POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 8 position))

(defsubst posn-object-width-height (position)
  "Return the pixel width and height of the object of POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (nth 9 position))


;;;; Obsolescent names for functions.

(define-obsolete-function-alias 'window-dot 'window-point "22.1")
(define-obsolete-function-alias 'set-window-dot 'set-window-point "22.1")
(define-obsolete-function-alias 'read-input 'read-string "22.1")
(define-obsolete-function-alias 'show-buffer 'set-window-buffer "22.1")
(define-obsolete-function-alias 'eval-current-buffer 'eval-buffer "22.1")
(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

(make-obsolete 'char-bytes "now always returns 1." "20.4")

(defun insert-string (&rest args)
  "Mocklisp-compatibility insert function.
Like the function `insert' except that any argument that is a number
is converted into a string by expressing it in decimal."
  (dolist (el args)
    (insert (if (integerp el) (number-to-string el) el))))
(make-obsolete 'insert-string 'insert "22.1")

(defun makehash (&optional test) (make-hash-table :test (or test 'eql)))
(make-obsolete 'makehash 'make-hash-table "22.1")

;; Some programs still use this as a function.
(defun baud-rate ()
  "Return the value of the `baud-rate' variable."
  baud-rate)
(make-obsolete 'baud-rate "use the `baud-rate' variable instead." "before 19.15")

;; These are used by VM and some old programs
(defalias 'focus-frame 'ignore "")
(make-obsolete 'focus-frame "it does nothing." "22.1")
(defalias 'unfocus-frame 'ignore "")
(make-obsolete 'unfocus-frame "it does nothing." "22.1")


;;;; Obsolescence declarations for variables, and aliases.

(make-obsolete-variable 'directory-sep-char "do not use it." "21.1")
(make-obsolete-variable 'mode-line-inverse-video "use the appropriate faces instead." "21.1")
(make-obsolete-variable 'unread-command-char
  "use `unread-command-events' instead.  That variable is a list of events to reread, so it now uses nil to mean `no event', instead of -1."
  "before 19.15")

;; Lisp manual only updated in 22.1.
(define-obsolete-variable-alias 'executing-macro 'executing-kbd-macro
			      "before 19.34")

(defvaralias 'x-lost-selection-hooks 'x-lost-selection-functions)
(make-obsolete-variable 'x-lost-selection-hooks 'x-lost-selection-functions "22.1")
(defvaralias 'x-sent-selection-hooks 'x-sent-selection-functions)
(make-obsolete-variable 'x-sent-selection-hooks 'x-sent-selection-functions "22.1")

(defvaralias 'messages-buffer-max-lines 'message-log-max)

;;;; Alternate names for functions - these are not being phased out.

(defalias 'send-string 'process-send-string)
(defalias 'send-region 'process-send-region)
(defalias 'string= 'string-equal)
(defalias 'string< 'string-lessp)
(defalias 'move-marker 'set-marker)
(defalias 'rplaca 'setcar)
(defalias 'rplacd 'setcdr)
(defalias 'beep 'ding) ;preserve lingual purity
(defalias 'indent-to-column 'indent-to)
(defalias 'backward-delete-char 'delete-backward-char)
(defalias 'search-forward-regexp (symbol-function 're-search-forward))
(defalias 'search-backward-regexp (symbol-function 're-search-backward))
(defalias 'int-to-string 'number-to-string)
(defalias 'store-match-data 'set-match-data)
(defalias 'make-variable-frame-localizable 'make-variable-frame-local)
;; These are the XEmacs names:
(defalias 'point-at-eol 'line-end-position)
(defalias 'point-at-bol 'line-beginning-position)


;;;; Hook manipulation functions.

(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
The return value is HOOK.

You never need to call this function now that `add-hook' does it for you
if its LOCAL argument is non-nil.

When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function works by making t a member of the buffer-local value,
which acts as a flag to run the hook functions in the default value as
well.  This works for all normal hooks, but does not work for most
non-normal hooks yet.  We will be changing the callers of non-normal
hooks so that they can handle localness; this has to be done one by
one.

This function does nothing if HOOK is already local in the current
buffer.

Do not use `make-local-variable' to make a hook variable buffer-local."
  (if (local-variable-p hook)
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)
(make-obsolete 'make-local-hook "not necessary any more." "21.1")

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  (if local (unless (local-variable-if-set-p hook)
	      (set (make-local-variable hook) (list t)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
      (setq local t)))
  (let ((hook-value (if local (symbol-value hook) (default-value hook))))
    ;; If the hook value is a single function, turn it into a list.
    (when (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
      (setq hook-value (list hook-value)))
    ;; Do the actual addition if necessary
    (unless (member function hook-value)
      (setq hook-value
	    (if append
		(append hook-value (list function))
	      (cons function hook-value))))
    ;; Set the actual variable
    (if local (set hook hook-value) (set-default hook hook-value))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; Do nothing if LOCAL is t but this hook has no local binding.
  (unless (and local (not (local-variable-p hook)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (when (and (local-variable-p hook)
	       (not (and (consp (symbol-value hook))
			 (memq t (symbol-value hook)))))
      (setq local t))
    (let ((hook-value (if local (symbol-value hook) (default-value hook))))
      ;; Remove the function, for both the list and the non-list cases.
      (if (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
	  (if (equal hook-value function) (setq hook-value nil))
	(setq hook-value (delete function (copy-sequence hook-value))))
      ;; If the function is on the global hook, we need to shadow it locally
      ;;(when (and local (member function (default-value hook))
      ;;	       (not (member (cons 'not function) hook-value)))
      ;;  (push (cons 'not function) hook-value))
      ;; Set the actual variable
      (if (not local)
	  (set-default hook hook-value)
	(if (equal hook-value '(t))
	    (kill-local-variable hook)
	  (set hook hook-value))))))

(defun add-to-list (list-var element &optional append)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

The return value is the new value of LIST-VAR.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))


(defun add-to-ordered-list (list-var element &optional order)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `eq'.

The resulting list is reordered so that the elements are in the
order given by each element's numeric list order.  Elements
without a numeric list order are placed at the end of the list.

If the third optional argument ORDER is a number (integer or
float), set the element's list order to the given value.  If
ORDER is nil or omitted, do not change the numeric order of
ELEMENT.  If ORDER has any other value, remove the numeric order
of ELEMENT if it has one.

The list order for each element is stored in LIST-VAR's
`list-order' property.

The return value is the new value of LIST-VAR."
  (let ((ordering (get list-var 'list-order)))
    (unless ordering
      (put list-var 'list-order
           (setq ordering (make-hash-table :weakness 'key :test 'eq))))
    (when order
      (puthash element (and (numberp order) order) ordering))
    (unless (memq element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var))))
    (set list-var (sort (symbol-value list-var)
			(lambda (a b)
			  (let ((oa (gethash a ordering))
				(ob (gethash b ordering)))
			    (if (and oa ob)
				(< oa ob)
			      oa)))))))


;;; Load history

;;; (defvar symbol-file-load-history-loaded nil
;;;   "Non-nil means we have loaded the file `fns-VERSION.el' in `exec-directory'.
;;; That file records the part of `load-history' for preloaded files,
;;; which is cleared out before dumping to make Emacs smaller.")

;;; (defun load-symbol-file-load-history ()
;;;   "Load the file `fns-VERSION.el' in `exec-directory' if not already done.
;;; That file records the part of `load-history' for preloaded files,
;;; which is cleared out before dumping to make Emacs smaller."
;;;   (unless symbol-file-load-history-loaded
;;;     (load (expand-file-name
;;; 	   ;; fns-XX.YY.ZZ.el does not work on DOS filesystem.
;;; 	   (if (eq system-type 'ms-dos)
;;; 	       "fns.el"
;;; 	     (format "fns-%s.el" emacs-version))
;;; 	   exec-directory)
;;; 	  ;; The file name fns-%s.el already has a .el extension.
;;; 	  nil nil t)
;;;     (setq symbol-file-load-history-loaded t)))

(defun symbol-file (symbol &optional type)
  "Return the input source in which SYMBOL was defined.
The value is normally a string that was passed to `load':
either an absolute file name, or a library name
\(with no directory name and no `.el' or `.elc' at the end).
It can also be nil, if the definition is not associated with any file.

If TYPE is nil, then any kind of definition is acceptable.
If TYPE is `defun' or `defvar', that specifies function
definition only or variable definition only.
`defface' specifies a face definition only."
  (if (and (or (null type) (eq type 'defun))
	   (symbolp symbol) (fboundp symbol)
	   (eq 'autoload (car-safe (symbol-function symbol))))
      (nth 1 (symbol-function symbol))
    (let ((files load-history)
	  file)
      (while files
	(if (if type
		(if (eq type 'defvar)
		    ;; Variables are present just as their names.
		    (member symbol (cdr (car files)))
		  ;; Other types are represented as (TYPE . NAME).
		  (member (cons type symbol) (cdr (car files))))
	      ;; We accept all types, so look for variable def
	      ;; and then for any other kind.
	      (or (member symbol (cdr (car files)))
		  (rassq symbol (cdr (car files)))))
	    (setq file (car (car files)) files nil))
	(setq files (cdr files)))
      file)))


;;;; Specifying things to do after certain files are loaded.

(defun eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE must match exactly.  Normally FILE is the name of a library,
with no directory or extension specified, since that is how `load'
is normally called.
FILE can also be a feature (i.e. a symbol), in which case FORM is
evaluated whenever that feature is `provide'd."
  (let ((elt (assoc file after-load-alist)))
    ;; Make sure there is an element for FILE.
    (unless elt (setq elt (list file)) (push elt after-load-alist))
    ;; Add FORM to the element if it isn't there.
    (unless (member form (cdr elt))
      (nconc elt (list form))
      ;; If the file has been loaded already, run FORM right away.
      (if (if (symbolp file)
	      (featurep file)
	    ;; Make sure `load-history' contains the files dumped with
	    ;; Emacs for the case that FILE is one of them.
	    ;; (load-symbol-file-load-history)
	    (assoc file load-history))
	  (eval form))))
  form)

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))

;;; open-network-stream is a wrapper around make-network-process.

(when (featurep 'make-network-process)
  (defun open-network-stream (name buffer host service)
  "Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.

Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
HOST is name of the host to connect to, or its IP address.
SERVICE is name of the service desired, or an integer specifying
 a port number to connect to."
  (make-network-process :name name :buffer buffer
			:host host :service service)))

;; compatibility

(make-obsolete 'process-kill-without-query
               "use `process-query-on-exit-flag' or `set-process-query-on-exit-flag'."
               "22.1")
(defun process-kill-without-query (process &optional flag)
  "Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required."
  (let ((old (process-query-on-exit-flag process)))
    (set-process-query-on-exit-flag process nil)
    old))

;; process plist management

(defun process-get (process propname)
  "Return the value of PROCESS' PROPNAME property.
This is the last value stored with `(process-put PROCESS PROPNAME VALUE)'."
  (plist-get (process-plist process) propname))

(defun process-put (process propname value)
  "Change PROCESS' PROPNAME property to VALUE.
It can be retrieved with `(process-get PROCESS PROPNAME)'."
  (set-process-plist process
		     (plist-put (process-plist process) propname value)))


;;;; Input and display facilities.

(defvar read-quoted-char-radix 8
  "*Radix for \\[quoted-insert] and other uses of `read-quoted-char'.
Legitimate radix values are 8, 10 and 16.")

(custom-declare-variable-early
 'read-quoted-char-radix 8
 "*Radix for \\[quoted-insert] and other uses of `read-quoted-char'.
Legitimate radix values are 8, 10 and 16."
  :type '(choice (const 8) (const 10) (const 16))
  :group 'editing-basics)

(defun read-quoted-char (&optional prompt)
  "Like `read-char', but do not allow quitting.
Also, if the first character read is an octal digit,
we read any number of octal digits and return the
specified character code.  Any nondigit terminates the sequence.
If the terminator is RET, it is discarded;
any other terminator is used itself as input.

The optional argument PROMPT specifies a string to use to prompt the user.
The variable `read-quoted-char-radix' controls which radix to use
for numeric input."
  (let ((message-log-max nil) done (first t) (code 0) char translated)
    (while (not done)
      (let ((inhibit-quit first)
	    ;; Don't let C-h get the help message--only help function keys.
	    (help-char nil)
	    (help-form
	     "Type the special character you want to use,
or the octal character code.
RET terminates the character code and is discarded;
any other non-digit terminates the character code and is then used as input."))
	(setq char (read-event (and prompt (format "%s-" prompt)) t))
	(if inhibit-quit (setq quit-flag nil)))
      ;; Translate TAB key into control-I ASCII character, and so on.
      ;; Note: `read-char' does it using the `ascii-character' property.
      ;; We could try and use read-key-sequence instead, but then C-q ESC
      ;; or C-q C-x might not return immediately since ESC or C-x might be
      ;; bound to some prefix in function-key-map or key-translation-map.
      (setq translated char)
      (let ((translation (lookup-key function-key-map (vector char))))
	(if (arrayp translation)
	    (setq translated (aref translation 0))))
      (cond ((null translated))
	    ((not (integerp translated))
	     (setq unread-command-events (list char)
		   done t))
	    ((/= (logand translated ?\M-\^@) 0)
	     ;; Turn a meta-character into a character with the 0200 bit set.
	     (setq code (logior (logand translated (lognot ?\M-\^@)) 128)
		   done t))
	    ((and (<= ?0 translated) (< translated (+ ?0 (min 10 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix) (- translated ?0)))
	     (and prompt (setq prompt (message "%s %c" prompt translated))))
	    ((and (<= ?a (downcase translated))
		  (< (downcase translated) (+ ?a -10 (min 36 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix)
			   (+ 10 (- (downcase translated) ?a))))
	     (and prompt (setq prompt (message "%s %c" prompt translated))))
	    ((and (not first) (eq translated ?\C-m))
	     (setq done t))
	    ((not first)
	     (setq unread-command-events (list char)
		   done t))
	    (t (setq code translated
		     done t)))
      (setq first nil))
    code))

(defun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `.' for each character that the user types.
The user ends with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
C-g quits; if `inhibit-quit' was non-nil around this function,
then it returns nil if the user types C-g.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (with-local-quit
    (if confirm
	(let (success)
	  (while (not success)
	    (let ((first (read-passwd prompt nil default))
		  (second (read-passwd "Confirm password: " nil default)))
	      (if (equal first second)
		  (progn
		    (and (arrayp second) (clear-string second))
		    (setq success first))
		(and (arrayp first) (clear-string first))
		(and (arrayp second) (clear-string second))
		(message "Password not repeated accurately; please start over")
		(sit-for 1))))
	  success)
      (let ((pass nil)
	    (c 0)
	    (echo-keystrokes 0)
	    (cursor-in-echo-area t))
	(while (progn (message "%s%s"
			       prompt
			       (make-string (length pass) ?.))
		      (setq c (read-char-exclusive nil t))
		      (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
	  (clear-this-command-keys)
	  (if (= c ?\C-u)
	      (progn
		(and (arrayp pass) (clear-string pass))
		(setq pass ""))
	    (if (and (/= c ?\b) (/= c ?\177))
		(let* ((new-char (char-to-string c))
		       (new-pass (concat pass new-char)))
		  (and (arrayp pass) (clear-string pass))
		  (clear-string new-char)
		  (setq c ?\0)
		  (setq pass new-pass))
	      (if (> (length pass) 0)
		  (let ((new-pass (substring pass 0 -1)))
		    (and (arrayp pass) (clear-string pass))
		    (setq pass new-pass))))))
	(message nil)
	(or pass default "")))))

;; This should be used by `call-interactively' for `n' specs.
(defun read-number (prompt &optional default)
  (let ((n nil))
    (when default
      (setq prompt
	    (if (string-match "\\(\\):[ \t]*\\'" prompt)
		(replace-match (format " (default %s)" default) t t prompt 1)
	      (replace-regexp-in-string "[ \t]*\\'"
					(format " (default %s) " default)
					prompt t t))))
    (while
	(progn
	  (let ((str (read-from-minibuffer prompt nil nil nil nil
					   (and default
						(number-to-string default)))))
	    (setq n (cond
		     ((zerop (length str)) default)
		     ((stringp str) (read str)))))
	  (unless (numberp n)
	    (message "Please enter a number.")
	    (sit-for 1)
	    t)))
    n))

;;; Atomic change groups.

(defmacro atomic-change-group (&rest body)
  "Perform BODY as an atomic change group.
This means that if BODY exits abnormally,
all of its changes to the current buffer are undone.
This works regardless of whether undo is enabled in the buffer.

This mechanism is transparent to ordinary use of undo;
if undo is enabled in the buffer and BODY succeeds, the
user can undo the change normally."
  (let ((handle (make-symbol "--change-group-handle--"))
	(success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
	   (,success nil))
       (unwind-protect
	   (progn
	     ;; This is inside the unwind-protect because
	     ;; it enables undo if that was disabled; we need
	     ;; to make sure that it gets disabled again.
	     (activate-change-group ,handle)
	     ,@body
	     (setq ,success t))
	 ;; Either of these functions will disable undo
	 ;; if it was disabled before.
	 (if ,success
	     (accept-change-group ,handle)
	   (cancel-change-group ,handle))))))

(defun prepare-change-group (&optional buffer)
  "Return a handle for the current buffer's state, for a change group.
If you specify BUFFER, make a handle for BUFFER's state instead.

Pass the handle to `activate-change-group' afterward to initiate
the actual changes of the change group.

To finish the change group, call either `accept-change-group' or
`cancel-change-group' passing the same handle as argument.  Call
`accept-change-group' to accept the changes in the group as final;
call `cancel-change-group' to undo them all.  You should use
`unwind-protect' to make sure the group is always finished.  The call
to `activate-change-group' should be inside the `unwind-protect'.
Once you finish the group, don't use the handle again--don't try to
finish the same group twice.  For a simple example of correct use, see
the source code of `atomic-change-group'.

The handle records only the specified buffer.  To make a multibuffer
change group, call this function once for each buffer you want to
cover, then use `nconc' to combine the returned values, like this:

  (nconc (prepare-change-group buffer-1)
         (prepare-change-group buffer-2))

You can then activate that multibuffer change group with a single
call to `activate-change-group' and finish it with a single call
to `accept-change-group' or `cancel-change-group'."

  (if buffer
      (list (cons buffer (with-current-buffer buffer buffer-undo-list)))
    (list (cons (current-buffer) buffer-undo-list))))

(defun activate-change-group (handle)
  "Activate a change group made with `prepare-change-group' (which see)."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (if (eq buffer-undo-list t)
	  (setq buffer-undo-list nil)))))

(defun accept-change-group (handle)
  "Finish a change group made with `prepare-change-group' (which see).
This finishes the change group by accepting its changes as final."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (if (eq elt t)
	  (setq buffer-undo-list t)))))

(defun cancel-change-group (handle)
  "Finish a change group made with `prepare-change-group' (which see).
This finishes the change group by reverting all of its changes."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (setq elt (cdr elt))
      (let ((old-car
	     (if (consp elt) (car elt)))
	    (old-cdr
	     (if (consp elt) (cdr elt))))
	;; Temporarily truncate the undo log at ELT.
	(when (consp elt)
	  (setcar elt nil) (setcdr elt nil))
	(unless (eq last-command 'undo) (undo-start))
	;; Make sure there's no confusion.
	(when (and (consp elt) (not (eq elt (last pending-undo-list))))
	  (error "Undoing to some unrelated state"))
	;; Undo it all.
	(while pending-undo-list (undo-more 1))
	;; Reset the modified cons cell ELT to its original content.
	(when (consp elt)
	  (setcar elt old-car)
	  (setcdr elt old-cdr))
	;; Revert the undo info to what it was when we grabbed the state.
	(setq buffer-undo-list elt)))))

;; For compatibility.
(defalias 'redraw-modeline 'force-mode-line-update)

(defun force-mode-line-update (&optional all)
  "Force redisplay of the current buffer's mode line and header line.
With optional non-nil ALL, force redisplay of all mode lines and
header lines.  This function also forces recomputation of the
menu bar menus and the frame title."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(defun momentary-string-display (string pos &optional exit-char message)
  "Momentarily display STRING in the buffer at POS.
Display remains until next event is input.
Optional third arg EXIT-CHAR can be a character, event or event
description list.  EXIT-CHAR defaults to SPC.  If the input is
EXIT-CHAR it is swallowed; otherwise it is then available as
input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\ ))
  (let ((inhibit-read-only t)
	;; Don't modify the undo list at all.
	(buffer-undo-list t)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-end)
    (unwind-protect
	(progn
	  (save-excursion
	    (goto-char pos)
	    ;; defeat file locking... don't try this at home, kids!
	    (setq buffer-file-name nil)
	    (insert-before-markers string)
	    (setq insert-end (point))
	    ;; If the message end is off screen, recenter now.
	    (if (< (window-end nil t) insert-end)
		(recenter (/ (window-height) 2)))
	    ;; If that pushed message start off the screen,
	    ;; scroll to start it at the top of the screen.
	    (move-to-window-line 0)
	    (if (> (point) pos)
		(progn
		  (goto-char pos)
		  (recenter 0))))
	  (message (or message "Type %s to continue editing.")
		   (single-key-description exit-char))
	  (let (char)
	    (if (integerp exit-char)
		(condition-case nil
		    (progn
		      (setq char (read-char))
		      (or (eq char exit-char)
			  (setq unread-command-events (list char))))
		  (error
		   ;; `exit-char' is a character, hence it differs
		   ;; from char, which is an event.
		   (setq unread-command-events (list char))))
	      ;; `exit-char' can be an event, or an event description
	      ;; list.
	      (setq char (read-event))
	      (or (eq char exit-char)
		  (eq char (event-convert-list exit-char))
		  (setq unread-command-events (list char))))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))


;;;; Overlay operations

(defun copy-overlay (o)
  "Return a copy of overlay O."
  (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
			  ;; FIXME: there's no easy way to find the
			  ;; insertion-type of the two markers.
			  (overlay-buffer o)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (pop props) (pop props)))
    o1))

(defun remove-overlays (&optional beg end name val)
  "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o name) val)
	;; Either push this overlay outside beg...end
	;; or split it to exclude beg...end
	;; or delete it entirely (if it is contained in beg...end).
	(if (< (overlay-start o) beg)
	    (if (> (overlay-end o) end)
		(progn
		  (move-overlay (copy-overlay o)
				(overlay-start o) beg)
		  (move-overlay o end (overlay-end o)))
	      (move-overlay o (overlay-start o) beg))
	  (if (> (overlay-end o) end)
	      (move-overlay o end (overlay-end o))
	    (delete-overlay o)))))))

;;;; Miscellanea.

(defvar suspend-hook nil
  "Normal hook run by `suspend-emacs', before suspending.")

(defvar suspend-resume-hook nil
  "Normal hook run by `suspend-emacs', after Emacs is continued.")

(defvar temp-buffer-show-hook nil
  "Normal hook run by `with-output-to-temp-buffer' after displaying the buffer.
When the hook runs, the temporary buffer is current, and the window it
was displayed in is selected.  This hook is normally set up with a
function to make the buffer read only, and find function names and
variable names in it, provided the major mode is still Help mode.")

(defvar temp-buffer-setup-hook nil
  "Normal hook run by `with-output-to-temp-buffer' at the start.
When the hook runs, the temporary buffer is current.
This hook is normally set up with a function to put the buffer in Help
mode.")

;; Avoid compiler warnings about this variable,
;; which has a special meaning on certain system types.
(defvar buffer-file-type nil
  "Non-nil if the visited file is a binary file.
This variable is meaningful on MS-DOG and Windows NT.
On those systems, it is automatically local in every buffer.
On other systems, this variable is normally always nil.")

;; This should probably be written in C (i.e., without using `walk-windows').
(defun get-buffer-window-list (buffer &optional minibuf frame)
  "Return list of all windows displaying BUFFER, or nil if none.
BUFFER can be a buffer or a buffer name.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (walk-windows (function (lambda (window)
			      (if (eq (window-buffer window) buffer)
				  (setq windows (cons window windows)))))
		  minibuf frame)
    windows))

(defun ignore (&rest ignore)
  "Do nothing and return nil.
This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'.
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency."
  (while t
    (signal 'error (list (apply 'format args)))))

(defalias 'user-original-login-name 'user-login-name)

(defvar yank-excluded-properties)

(defun remove-yank-excluded-properties (start end)
  "Remove `yank-excluded-properties' between START and END positions.
Replaces `category' properties with their defined properties."
  (let ((inhibit-read-only t))
    ;; Replace any `category' property with the properties it stands for.
    (unless (memq yank-excluded-properties '(t nil))
      (save-excursion
	(goto-char start)
	(while (< (point) end)
	  (let ((cat (get-text-property (point) 'category))
		run-end)
	    (setq run-end
		  (next-single-property-change (point) 'category nil end))
	    (when cat
	      (let (run-end2 original)
		(remove-list-of-text-properties (point) run-end '(category))
		(while (< (point) run-end)
		  (setq run-end2 (next-property-change (point) nil run-end))
		  (setq original (text-properties-at (point)))
		  (set-text-properties (point) run-end2 (symbol-plist cat))
		  (add-text-properties (point) run-end2 original)
		  (goto-char run-end2))))
	    (goto-char run-end)))))
    (if (eq yank-excluded-properties t)
	(set-text-properties start end nil)
      (remove-list-of-text-properties start end yank-excluded-properties))))

(defvar yank-undo-function)

(defun insert-for-yank (string)
  "Calls `insert-for-yank-1' repetitively for each `yank-handler' segment.

See `insert-for-yank-1' for more details."
  (let (to)
    (while (setq to (next-single-property-change 0 'yank-handler string))
      (insert-for-yank-1 (substring string 0 to))
      (setq string (substring string to))))
  (insert-for-yank-1 string))

(defun insert-for-yank-1 (string)
  "Insert STRING at point, stripping some text properties.

Strip text properties from the inserted text according to
`yank-excluded-properties'.  Otherwise just like (insert STRING).

If STRING has a non-nil `yank-handler' property on the first character,
the normal insert behavior is modified in various ways.  The value of
the yank-handler property must be a list with one to four elements
with the following format:  (FUNCTION PARAM NOEXCLUDE UNDO).
When FUNCTION is present and non-nil, it is called instead of `insert'
 to insert the string.  FUNCTION takes one argument--the object to insert.
If PARAM is present and non-nil, it replaces STRING as the object
 passed to FUNCTION (or `insert'); for example, if FUNCTION is
 `yank-rectangle', PARAM may be a list of strings to insert as a
 rectangle.
If NOEXCLUDE is present and non-nil, the normal removal of the
 yank-excluded-properties is not performed; instead FUNCTION is
 responsible for removing those properties.  This may be necessary
 if FUNCTION adjusts point before or after inserting the object.
If UNDO is present and non-nil, it is a function that will be called
 by `yank-pop' to undo the insertion of the current object.  It is
 called with two arguments, the start and end of the current region.
 FUNCTION may set `yank-undo-function' to override the UNDO value."
  (let* ((handler (and (stringp string)
		       (get-text-property 0 'yank-handler string)))
	 (param (or (nth 1 handler) string))
	 (opoint (point)))
    (setq yank-undo-function t)
    (if (nth 0 handler) ;; FUNCTION
	(funcall (car handler) param)
      (insert param))
    (unless (nth 2 handler) ;; NOEXCLUDE
      (remove-yank-excluded-properties opoint (point)))
    (if (eq yank-undo-function t)  ;; not set by FUNCTION
	(setq yank-undo-function (nth 3 handler))) ;; UNDO
    (if (nth 4 handler) ;; COMMAND
	(setq this-command (nth 4 handler)))))

(defun insert-buffer-substring-no-properties (buffer &optional start end)
  "Insert before point a substring of BUFFER, without text properties.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER."
  (let ((opoint (point)))
    (insert-buffer-substring buffer start end)
    (let ((inhibit-read-only t))
      (set-text-properties opoint (point) nil))))

(defun insert-buffer-substring-as-yank (buffer &optional start end)
  "Insert before point a part of BUFFER, stripping some text properties.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER.
Strip text properties from the inserted text according to
`yank-excluded-properties'."
  ;; Since the buffer text should not normally have yank-handler properties,
  ;; there is no need to handle them here.
  (let ((opoint (point)))
    (insert-buffer-substring buffer start end)
    (remove-yank-excluded-properties opoint (point))))


;; Synchronous shell commands.

(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
COMMAND is the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handled as usual in the shell.

\(fn NAME BUFFER COMMAND &rest COMMAND-ARGS)"
  (cond
   ((eq system-type 'vax-vms)
    (apply 'start-process name buffer args))
   ;; We used to use `exec' to replace the shell with the command,
   ;; but that failed to handle (...) and semicolon, etc.
   (t
    (start-process name buffer shell-file-name shell-command-switch
		   (mapconcat 'identity args " ")))))

(defun call-process-shell-command (command &optional infile buffer display
					   &rest args)
  "Execute the shell command COMMAND synchronously in separate process.
The remaining arguments are optional.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as additional arguments for COMMAND.
Wildcards and redirection are handled as usual in the shell.

If BUFFER is 0, `call-process-shell-command' returns immediately with value nil.
Otherwise it waits for COMMAND to terminate and returns a numeric exit
status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again."
  (cond
   ((eq system-type 'vax-vms)
    (apply 'call-process command infile buffer display args))
   ;; We used to use `exec' to replace the shell with the command,
   ;; but that failed to handle (...) and semicolon, etc.
   (t
    (call-process shell-file-name
		  infile buffer display
		  shell-command-switch
		  (mapconcat 'identity (cons command args) " ")))))

(defmacro with-current-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer ,buffer)
     ,@body))

(defmacro with-selected-window (window &rest body)
  "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the current buffer, since otherwise
its normal operation could potentially make a different
buffer current.  It does not alter the buffer list ordering.

This macro saves and restores the selected window, as well as
the selected window in each frame.  If the previously selected
window of some frame is no longer live at the end of BODY, that
frame's selected window is left alone.  If the selected window is
no longer live, then whatever window is selected at the end of
BODY remains selected.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  ;; Most of this code is a copy of save-selected-window.
  `(let ((save-selected-window-window (selected-window))
	 ;; It is necessary to save all of these, because calling
	 ;; select-window changes frame-selected-window for whatever
	 ;; frame that window is in.
	 (save-selected-window-alist
	  (mapcar (lambda (frame) (list frame (frame-selected-window frame)))
		  (frame-list))))
     (save-current-buffer
       (unwind-protect
	   (progn (select-window ,window 'norecord)
		  ,@body)
	 (dolist (elt save-selected-window-alist)
	   (and (frame-live-p (car elt))
		(window-live-p (cadr elt))
		(set-frame-selected-window (car elt) (cadr elt))))
	 (if (window-live-p save-selected-window-window)
	     (select-window save-selected-window-window 'norecord))))))

(defmacro with-temp-file (file &rest body)
  "Create a new buffer, evaluate BODY there, and write the buffer to FILE.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (declare (debug t))
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-file ,file)
	   (,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp file*"))))
       (unwind-protect
	   (prog1
	       (with-current-buffer ,temp-buffer
		 ,@body)
	     (with-current-buffer ,temp-buffer
	       (widen)
	       (write-region (point-min) (point-max) ,temp-file nil 0)))
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-temp-message (message &rest body)
  "Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area."
  (declare (debug t))
  (let ((current-message (make-symbol "current-message"))
	(temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
	   (,current-message))
       (unwind-protect
	   (progn
	     (when ,temp-message
	       (setq ,current-message (current-message))
	       (message "%s" ,temp-message))
	     ,@body)
	 (and ,temp-message
	      (if ,current-message
		  (message "%s" ,current-message)
		(message nil)))))))

(defmacro with-temp-buffer (&rest body)
  "Create a temporary buffer, and evaluate BODY there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       (unwind-protect
	   (with-current-buffer ,temp-buffer
	     ,@body)
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  (declare (indent 0) (debug t))
  `(let ((standard-output
	  (get-buffer-create (generate-new-buffer-name " *string-output*"))))
     (let ((standard-output standard-output))
       ,@body)
     (with-current-buffer standard-output
       (prog1
	   (buffer-string)
	 (kill-buffer nil)))))

(defmacro with-local-quit (&rest body)
  "Execute BODY, allowing quits to terminate BODY but not escape further.
When a quit terminates BODY, `with-local-quit' returns nil but
requests another quit.  That quit will be processed, the next time quitting
is allowed once again."
  (declare (debug t) (indent 0))
  `(condition-case nil
       (let ((inhibit-quit nil))
	 ,@body)
     (quit (setq quit-flag t) nil)))

(defmacro while-no-input (&rest body)
  "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym))
	   (or (not (sit-for 0 0 t))
	     ,@body))))))

(defmacro combine-after-change-calls (&rest body)
  "Execute BODY, but don't call the after-change functions till the end.
If BODY makes changes in the buffer, they are recorded
and the functions on `after-change-functions' are called several times
when BODY is finished.
The return value is the value of the last form in BODY.

If `before-change-functions' is non-nil, then calls to the after-change
functions can't be deferred, so in that case this macro has no effect.

Do not alter `after-change-functions' or `before-change-functions'
in BODY."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (let ((combine-after-change-calls t))
	 . ,body)
     (combine-after-change-execute)))


(defvar delay-mode-hooks nil
  "If non-nil, `run-mode-hooks' should delay running the hooks.")
(defvar delayed-mode-hooks nil
  "List of delayed mode hooks waiting to be run.")
(make-variable-buffer-local 'delayed-mode-hooks)
(put 'delay-mode-hooks 'permanent-local t)

(defvar after-change-major-mode-hook nil
  "Normal hook run at the very end of major mode functions.")

(defun run-mode-hooks (&rest hooks)
  "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Execution is delayed if `delay-mode-hooks' is non-nil.
If `delay-mode-hooks' is nil, run `after-change-major-mode-hook'
after running the mode hooks.
Major mode functions should use this."
  (if delay-mode-hooks
      ;; Delaying case.
      (dolist (hook hooks)
	(push hook delayed-mode-hooks))
    ;; Normal case, just run the hook as before plus any delayed hooks.
    (setq hooks (nconc (nreverse delayed-mode-hooks) hooks))
    (setq delayed-mode-hooks nil)
    (apply 'run-hooks hooks)
    (run-hooks 'after-change-major-mode-hook)))

(defmacro delay-mode-hooks (&rest body)
  "Execute BODY, but delay any `run-mode-hooks'.
These hooks will be executed by the first following call to
`run-mode-hooks' that occurs outside any `delayed-mode-hooks' form.
Only affects hooks run in the current buffer."
  (declare (debug t) (indent 0))
  `(progn
     (make-local-variable 'delay-mode-hooks)
     (let ((delay-mode-hooks t))
       ,@body)))

;; PUBLIC: find if the current mode derives from another.

(defun derived-mode-p (&rest modes)
  "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
  (let ((parent major-mode))
    (while (and (not (memq parent modes))
		(setq parent (get parent 'derived-mode-parent))))
    parent))

(defun find-tag-default ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn
	  (goto-char (match-end 0))
	  (condition-case nil
	      (buffer-substring-no-properties
	       (point)
	       (progn (forward-sexp -1)
		      (while (looking-at "\\s'")
			(forward-char 1))
		      (point)))
	    (error nil)))
      nil)))

(defmacro with-syntax-table (table &rest body)
  "Evaluate BODY with syntax table of current buffer set to TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (declare (debug t))
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table ,table)
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

(defmacro dynamic-completion-table (fun)
  "Use function FUN as a dynamic completion table.
FUN is called with one argument, the string for which completion is required,
and it should return an alist containing all the intended possible
completions.  This alist may be a full list of possible completions so that FUN
can ignore the value of its argument.  If completion is performed in the
minibuffer, FUN will be called in the buffer from which the minibuffer was
entered.

The result of the `dynamic-completion-table' form is a function
that can be used as the ALIST argument to `try-completion' and
`all-completion'.  See Info node `(elisp)Programmed Completion'."
  (declare (debug (lambda-expr)))
  (let ((win (make-symbol "window"))
        (string (make-symbol "string"))
        (predicate (make-symbol "predicate"))
        (mode (make-symbol "mode")))
    `(lambda (,string ,predicate ,mode)
       (with-current-buffer (let ((,win (minibuffer-selected-window)))
                              (if (window-live-p ,win) (window-buffer ,win)
                                (current-buffer)))
         (cond
          ((eq ,mode t) (all-completions ,string (,fun ,string) ,predicate))
          ((not ,mode) (try-completion ,string (,fun ,string) ,predicate))
          (t (test-completion ,string (,fun ,string) ,predicate)))))))

(defmacro lazy-completion-table (var fun &rest args)
  "Initialize variable VAR as a lazy completion table.
If the completion table VAR is used for the first time (e.g., by passing VAR
as an argument to `try-completion'), the function FUN is called with arguments
ARGS.  FUN must return the completion table that will be stored in VAR.
If completion is requested in the minibuffer, FUN will be called in the buffer
from which the minibuffer was entered.  The return value of
`lazy-completion-table' must be used to initialize the value of VAR."
  (declare (debug (symbol lambda-expr def-body)))
  (let ((str (make-symbol "string")))
    `(dynamic-completion-table
      (lambda (,str)
        (unless (listp ,var)
          (setq ,var (,fun ,@args)))
        ,var))))

(defmacro complete-in-turn (a b)
  "Create a completion table that first tries completion in A and then in B.
A and B should not be costly (or side-effecting) expressions."
  (declare (debug (def-form def-form)))
  `(lambda (string predicate mode)
     (cond
      ((eq mode t)
       (or (all-completions string ,a predicate)
	   (all-completions string ,b predicate)))
      ((eq mode nil)
       (or (try-completion string ,a predicate)
	   (try-completion string ,b predicate)))
      (t
       (or (test-completion string ,a predicate)
	   (test-completion string ,b predicate))))))

;;; Matching and substitution

(defvar save-match-data-internal)

;; We use save-match-data-internal as the local variable because
;; that works ok in practice (people should not use that variable elsewhere).
;; We used to use an uninterned symbol; the compiler handles that properly
;; now, but it generates slower code.
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data.
The value returned is the value of the last form in BODY."
  ;; It is better not to use backquote here,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (declare (indent 0) (debug t))
  (list 'let
	'((save-match-data-internal (match-data)))
	(list 'unwind-protect
	      (cons 'progn body)
	      ;; It is safe to free (evaporate) markers immediately here,
	      ;; as Lisp programs should not copy from save-match-data-internal.
	      '(set-match-data save-match-data-internal 'evaporate))))

(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

(defun looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying how far back the
match can start.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
  (let ((start (point))
	(pos
	 (save-excursion
	   (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
		(point)))))
    (if (and greedy pos)
	(save-restriction
	  (narrow-to-region (point-min) start)
	  (while (and (> pos (point-min))
		      (save-excursion
			(goto-char pos)
			(backward-char 1)
			(looking-at (concat "\\(?:"  regexp "\\)\\'"))))
	    (setq pos (1- pos)))
	  (save-excursion
	    (goto-char pos)
	    (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null pos))))


(defconst split-string-default-separators "[ \f\t\n\r\v]+"
  "The default value of separators for `split-string'.

A regexp matching strings of whitespace.  May be locale-dependent
\(as yet unimplemented).  Should not match non-breaking spaces.

Warning: binding this to a different value and using it as default is
likely to have undesired semantics.")

;; The specification says that if both SEPARATORS and OMIT-NULLS are
;; defaulted, OMIT-NULLS should be treated as t.  Simplifying the logical
;; expression leads to the equivalent implementation that if SEPARATORS
;; is defaulted, OMIT-NULLS is treated as t.
(defun split-string (string &optional separators omit-nulls)
  "Split STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-nil, it should be a regular expression matching text
which separates, but is not part of, the substrings.  If nil it defaults to
`split-string-default-separators', normally \"[ \\f\\t\\n\\r\\v]+\", and
OMIT-NULLS is forced to t.

If OMIT-NULLS is t, zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained,
which correctly parses CSV format, for example.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)').  In the rare
case that you wish to retain zero-length substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (let ((keep-nulls (not (if separators omit-nulls t)))
	(rexp (or separators split-string-default-separators))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

(defun replace-regexp-in-string (regexp rep string &optional
                                 fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function, it is called with the actual text of each
match, and its value is used as the replacement text.  When REP is called,
the match-data are the result of matching REGEXP against a substring
of STRING.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb)       ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defun subregexp-context-p (regexp pos &optional start)
  "Return non-nil if POS is in a normal subregexp context in REGEXP.
A subregexp context is one where a sub-regexp can appear.
A non-subregexp context is for example within brackets, or within a
repetition bounds operator `\\=\\{...\\}', or right after a `\\'.
If START is non-nil, it should be a position in REGEXP, smaller
than POS, and known to be in a subregexp context."
  ;; Here's one possible implementation, with the great benefit that it
  ;; reuses the regexp-matcher's own parser, so it understands all the
  ;; details of the syntax.  A disadvantage is that it needs to match the
  ;; error string.
  (condition-case err
      (progn
        (string-match (substring regexp (or start 0) pos) "")
        t)
    (invalid-regexp
     (not (member (cadr err) '("Unmatched [ or [^"
                               "Unmatched \\{"
                               "Trailing backslash")))))
  ;; An alternative implementation:
  ;; (defconst re-context-re
  ;;   (let* ((harmless-ch "[^\\[]")
  ;;          (harmless-esc "\\\\[^{]")
  ;;          (class-harmless-ch "[^][]")
  ;;          (class-lb-harmless "[^]:]")
  ;;          (class-lb-colon-maybe-charclass ":\\([a-z]+:]\\)?")
  ;;          (class-lb (concat "\\[\\(" class-lb-harmless
  ;;                            "\\|" class-lb-colon-maybe-charclass "\\)"))
  ;;          (class
  ;;           (concat "\\[^?]?"
  ;;                   "\\(" class-harmless-ch
  ;;                   "\\|" class-lb "\\)*"
  ;;                   "\\[?]"))     ; special handling for bare [ at end of re
  ;;          (braces "\\\\{[0-9,]+\\\\}"))
  ;;     (concat "\\`\\(" harmless-ch "\\|" harmless-esc
  ;;             "\\|" class "\\|" braces "\\)*\\'"))
  ;;   "Matches any prefix that corresponds to a normal subregexp context.")
  ;; (string-match re-context-re (substring regexp (or start 0) pos))
  )

(defun shell-quote-argument (argument)
  "Quote an argument for passing as argument to an inferior shell."
  (if (eq system-type 'ms-dos)
      ;; Quote using double quotes, but escape any existing quotes in
      ;; the argument with backslashes.
      (let ((result "")
	    (start 0)
	    end)
	(if (or (null (string-match "[^\"]" argument))
		(< (match-end 0) (length argument)))
	    (while (string-match "[\"]" argument start)
	      (setq end (match-beginning 0)
		    result (concat result (substring argument start end)
				   "\\" (substring argument end (1+ end)))
		    start (1+ end))))
	(concat "\"" result (substring argument start) "\""))
    (if (eq system-type 'windows-nt)
	(concat "\"" argument "\"")
      (if (equal argument "")
	  "''"
	;; Quote everything except POSIX filename characters.
	;; This should be safe enough even for really weird shells.
	(let ((result "") (start 0) end)
	  (while (string-match "[^-0-9a-zA-Z_./]" argument start)
	    (setq end (match-beginning 0)
		  result (concat result (substring argument start end)
				 "\\" (substring argument end (1+ end)))
		  start (1+ end)))
	  (concat result (substring argument start)))))))

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
Create a syntax table which inherits from OLDTABLE (if non-nil) or
from `standard-syntax-table' otherwise."
  (let ((table (make-char-table 'syntax-table nil)))
    (set-char-table-parent table (or oldtable (standard-syntax-table)))
    table))

(defun syntax-after (pos)
  "Return the raw syntax of the char after POS.
If POS is outside the buffer's accessible portion, return nil."
  (unless (or (< pos (point-min)) (>= pos (point-max)))
    (let ((st (if parse-sexp-lookup-properties
		  (get-char-property pos 'syntax-table))))
      (if (consp st) st
	(aref (or st (syntax-table)) (char-after pos))))))

(defun syntax-class (syntax)
  "Return the syntax class part of the syntax descriptor SYNTAX.
If SYNTAX is nil, return nil."
  (and syntax (logand (car syntax) 65535)))

(defun add-to-invisibility-spec (element)
  "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec (list t)))
  (setq buffer-invisibility-spec
	(cons element buffer-invisibility-spec)))

(defun remove-from-invisibility-spec (element)
  "Remove ELEMENT from `buffer-invisibility-spec'."
  (if (consp buffer-invisibility-spec)
    (setq buffer-invisibility-spec (delete element buffer-invisibility-spec))))

(defun global-set-key (key command)
  "Give KEY a global binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key (current-global-map) key command))

(defun local-set-key (key command)
  "Give KEY a local binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "KSet key locally: \nCSet key %s locally to command: ")
  (let ((map (current-local-map)))
    (or map
	(use-local-map (setq map (make-sparse-keymap))))
    (or (vectorp key) (stringp key)
	(signal 'wrong-type-argument (list 'arrayp key)))
    (define-key map key command)))

(defun global-unset-key (key)
  "Remove global binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key globally: ")
  (global-set-key key nil))

(defun local-unset-key (key)
  "Remove local binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (local-set-key key nil))
  nil)

;; We put this here instead of in frame.el so that it's defined even on
;; systems where frame.el isn't loaded.
(defun frame-configuration-p (object)
  "Return non-nil if OBJECT seems to be a frame configuration.
Any list whose car is `frame-configuration' is assumed to be a frame
configuration."
  (and (consp object)
       (eq (car object) 'frame-configuration)))

(defun assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (eq (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (eq (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun rassq-delete-all (value alist)
  "Delete from ALIST all elements whose cdr is `eq' to VALUE.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (eq (cdr (car alist)) value))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (eq (cdr (car tail-cdr)) value))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun make-temp-file (prefix &optional dir-flag suffix)
  "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary),
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file.

If SUFFIX is non-nil, add that at the end of the file name."
  (let ((umask (default-file-modes))
	file)
    (unwind-protect
	(progn
	  ;; Create temp files with strict access rights.  It's easy to
	  ;; loosen them later, whereas it's impossible to close the
	  ;; time-window of loose permissions otherwise.
	  (set-default-file-modes ?\700)
	  (while (condition-case ()
		     (progn
		       (setq file
			     (make-temp-name
			      (expand-file-name prefix temporary-file-directory)))
		       (if suffix
			   (setq file (concat file suffix)))
		       (if dir-flag
			   (make-directory file)
			 (write-region "" nil file nil 'silent nil 'excl))
		       nil)
		   (file-already-exists t))
	    ;; the file was somehow created by someone else between
	    ;; `make-temp-name' and `write-region', let's try again.
	    nil)
	  file)
      ;; Reset the umask.
      (set-default-file-modes umask))))


;; If a minor mode is not defined with define-minor-mode,
;; add it here explicitly.
;; isearch-mode is deliberately excluded, since you should
;; not call it yourself.
(defvar minor-mode-list '(auto-save-mode auto-fill-mode abbrev-mode
					 overwrite-mode view-mode
                                         hs-minor-mode)
  "List of all minor mode functions.")

(defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Register a new minor mode.

This is an XEmacs-compatibility function.  Use `define-minor-mode' instead.

TOGGLE is a symbol which is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string.

Optional KEYMAP is the keymap for the minor mode that will be added
to `minor-mode-map-alist'.

Optional AFTER specifies that TOGGLE should be added after AFTER
in `minor-mode-alist'.

Optional TOGGLE-FUN is an interactive function to toggle the mode.
It defaults to (and should by convention be) TOGGLE.

If TOGGLE has a non-nil `:included' property, an entry for the mode is
included in the mode-line minor mode menu.
If TOGGLE has a `:menu-tag', that is used for the menu item's label."
  (unless (memq toggle minor-mode-list)
    (push toggle minor-mode-list))

  (unless toggle-fun (setq toggle-fun toggle))
  (unless (eq toggle-fun toggle)
    (put toggle :minor-mode-function toggle-fun))
  ;; Add the name to the minor-mode-alist.
  (when name
    (let ((existing (assq toggle minor-mode-alist)))
      (if existing
	  (setcdr existing (list name))
	(let ((tail minor-mode-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (list toggle name)) rest))
	    (setq minor-mode-alist (cons (list toggle name)
					 minor-mode-alist)))))))
  ;; Add the toggle to the minor-modes menu if requested.
  (when (get toggle :included)
    (define-key mode-line-mode-menu
      (vector toggle)
      (list 'menu-item
	    (concat
	     (or (get toggle :menu-tag)
		 (if (stringp name) name (symbol-name toggle)))
	     (let ((mode-name (if (symbolp name) (symbol-value name))))
	       (if (and (stringp mode-name) (string-match "[^ ]+" mode-name))
		   (concat " (" (match-string 0 mode-name) ")"))))
	    toggle-fun
	    :button (cons :toggle toggle))))

  ;; Add the map to the minor-mode-map-alist.
  (when keymap
    (let ((existing (assq toggle minor-mode-map-alist)))
      (if existing
	  (setcdr existing keymap)
	(let ((tail minor-mode-map-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (cons toggle keymap)) rest))
	    (setq minor-mode-map-alist (cons (cons toggle keymap)
					     minor-mode-map-alist))))))))

;; Clones ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun text-clone-maintain (ol1 after beg end &optional len)
  "Propagate the changes made under the overlay OL1 to the other clones.
This is used on the `modification-hooks' property of text clones."
  (when (and after (not undo-in-progress) (overlay-start ol1))
    (let ((margin (if (overlay-get ol1 'text-clone-spreadp) 1 0)))
      (setq beg (max beg (+ (overlay-start ol1) margin)))
      (setq end (min end (- (overlay-end ol1) margin)))
      (when (<= beg end)
	(save-excursion
	  (when (overlay-get ol1 'text-clone-syntax)
	    ;; Check content of the clone's text.
	    (let ((cbeg (+ (overlay-start ol1) margin))
		  (cend (- (overlay-end ol1) margin)))
	      (goto-char cbeg)
	      (save-match-data
		(if (not (re-search-forward
			  (overlay-get ol1 'text-clone-syntax) cend t))
		    ;; Mark the overlay for deletion.
		    (overlay-put ol1 'text-clones nil)
		  (when (< (match-end 0) cend)
		    ;; Shrink the clone at its end.
		    (setq end (min end (match-end 0)))
		    (move-overlay ol1 (overlay-start ol1)
				  (+ (match-end 0) margin)))
		  (when (> (match-beginning 0) cbeg)
		    ;; Shrink the clone at its beginning.
		    (setq beg (max (match-beginning 0) beg))
		    (move-overlay ol1 (- (match-beginning 0) margin)
				  (overlay-end ol1)))))))
	  ;; Now go ahead and update the clones.
	  (let ((head (- beg (overlay-start ol1)))
		(tail (- (overlay-end ol1) end))
		(str (buffer-substring beg end))
		(nothing-left t)
		(inhibit-modification-hooks t))
	    (dolist (ol2 (overlay-get ol1 'text-clones))
	      (let ((oe (overlay-end ol2)))
		(unless (or (eq ol1 ol2) (null oe))
		  (setq nothing-left nil)
		  (let ((mod-beg (+ (overlay-start ol2) head)))
		    ;;(overlay-put ol2 'modification-hooks nil)
		    (goto-char (- (overlay-end ol2) tail))
		    (unless (> mod-beg (point))
		      (save-excursion (insert str))
		      (delete-region mod-beg (point)))
		    ;;(overlay-put ol2 'modification-hooks '(text-clone-maintain))
		    ))))
	    (if nothing-left (delete-overlay ol1))))))))

(defun text-clone-create (start end &optional spreadp syntax)
  "Create a text clone of START...END at point.
Text clones are chunks of text that are automatically kept identical:
changes done to one of the clones will be immediately propagated to the other.

The buffer's content at point is assumed to be already identical to
the one between START and END.
If SYNTAX is provided it's a regexp that describes the possible text of
the clones; the clone will be shrunk or killed if necessary to ensure that
its text matches the regexp.
If SPREADP is non-nil it indicates that text inserted before/after the
clone should be incorporated in the clone."
  ;; To deal with SPREADP we can either use an overlay with `nil t' along
  ;; with insert-(behind|in-front-of)-hooks or use a slightly larger overlay
  ;; (with a one-char margin at each end) with `t nil'.
  ;; We opted for a larger overlay because it behaves better in the case
  ;; where the clone is reduced to the empty string (we want the overlay to
  ;; stay when the clone's content is the empty string and we want to use
  ;; `evaporate' to make sure those overlays get deleted when needed).
  ;;
  (let* ((pt-end (+ (point) (- end start)))
  	 (start-margin (if (or (not spreadp) (bobp) (<= start (point-min)))
			   0 1))
  	 (end-margin (if (or (not spreadp)
			     (>= pt-end (point-max))
  			     (>= start (point-max)))
  			 0 1))
  	 (ol1 (make-overlay (- start start-margin) (+ end end-margin) nil t))
  	 (ol2 (make-overlay (- (point) start-margin) (+ pt-end end-margin) nil t))
	 (dups (list ol1 ol2)))
    (overlay-put ol1 'modification-hooks '(text-clone-maintain))
    (when spreadp (overlay-put ol1 'text-clone-spreadp t))
    (when syntax (overlay-put ol1 'text-clone-syntax syntax))
    ;;(overlay-put ol1 'face 'underline)
    (overlay-put ol1 'evaporate t)
    (overlay-put ol1 'text-clones dups)
    ;;
    (overlay-put ol2 'modification-hooks '(text-clone-maintain))
    (when spreadp (overlay-put ol2 'text-clone-spreadp t))
    (when syntax (overlay-put ol2 'text-clone-syntax syntax))
    ;;(overlay-put ol2 'face 'underline)
    (overlay-put ol2 'evaporate t)
    (overlay-put ol2 'text-clones dups)))

(defun play-sound (sound)
  "SOUND is a list of the form `(sound KEYWORD VALUE...)'.
The following keywords are recognized:

  :file FILE - read sound data from FILE.  If FILE isn't an
absolute file name, it is searched in `data-directory'.

  :data DATA - read sound data from string DATA.

Exactly one of :file or :data must be present.

  :volume VOL - set volume to VOL.  VOL must an integer in the
range 0..100 or a float in the range 0..1.0.  If not specified,
don't change the volume setting of the sound device.

  :device DEVICE - play sound on DEVICE.  If not specified,
a system-dependent default device name is used."
  (if (fboundp 'play-sound-internal)
      (play-sound-internal sound)
    (error "This Emacs binary lacks sound support")))

(defun define-mail-user-agent (symbol composefunc sendfunc
				      &optional abortfunc hookvar)
  "Define a symbol to identify a mail-sending package for `mail-user-agent'.

SYMBOL can be any Lisp symbol.  Its function definition and/or
value as a variable do not matter for this usage; we use only certain
properties on its property list, to encode the rest of the arguments.

COMPOSEFUNC is program callable function that composes an outgoing
mail message buffer.  This function should set up the basics of the
buffer without requiring user interaction.  It should populate the
standard mail headers, leaving the `to:' and `subject:' headers blank
by default.

COMPOSEFUNC should accept several optional arguments--the same
arguments that `compose-mail' takes.  See that function's documentation.

SENDFUNC is the command a user would run to send the message.

Optional ABORTFUNC is the command a user would run to abort the
message.  For mail packages that don't have a separate abort function,
this can be `kill-buffer' (the equivalent of omitting this argument).

Optional HOOKVAR is a hook variable that gets run before the message
is actually sent.  Callers that use the `mail-user-agent' may
install a hook function temporarily on this hook variable.
If HOOKVAR is nil, `mail-send-hook' is used.

The properties used on SYMBOL are `composefunc', `sendfunc',
`abortfunc', and `hookvar'."
  (put symbol 'composefunc composefunc)
  (put symbol 'sendfunc sendfunc)
  (put symbol 'abortfunc (or abortfunc 'kill-buffer))
  (put symbol 'hookvar (or hookvar 'mail-send-hook)))

;; Standardized progress reporting

;; Progress reporter has the following structure:
;;
;;	(NEXT-UPDATE-VALUE . [NEXT-UPDATE-TIME
;;			      MIN-VALUE
;;			      MAX-VALUE
;;			      MESSAGE
;;			      MIN-CHANGE
;;			      MIN-TIME])
;;
;; This weirdeness is for optimization reasons: we want
;; `progress-reporter-update' to be as fast as possible, so
;; `(car reporter)' is better than `(aref reporter 0)'.
;;
;; NEXT-UPDATE-TIME is a float.  While `float-time' loses a couple
;; digits of precision, it doesn't really matter here.  On the other
;; hand, it greatly simplifies the code.

(defsubst progress-reporter-update (reporter value)
  "Report progress of an operation in the echo area.
However, if the change since last echo area update is too small
or not enough time has passed, then do nothing (see
`make-progress-reporter' for details).

First parameter, REPORTER, should be the result of a call to
`make-progress-reporter'.  Second, VALUE, determines the actual
progress of operation; it must be between MIN-VALUE and MAX-VALUE
as passed to `make-progress-reporter'.

This function is very inexpensive, you may not bother how often
you call it."
  (when (>= value (car reporter))
    (progress-reporter-do-update reporter value)))

(defun make-progress-reporter (message min-value max-value
				       &optional current-value
				       min-change min-time)
  "Return progress reporter object to be used with `progress-reporter-update'.

MESSAGE is shown in the echo area.  When at least 1% of operation
is complete, the exact percentage will be appended to the
MESSAGE.  When you call `progress-reporter-done', word \"done\"
is printed after the MESSAGE.  You can change MESSAGE of an
existing progress reporter with `progress-reporter-force-update'.

MIN-VALUE and MAX-VALUE designate starting (0% complete) and
final (100% complete) states of operation.  The latter should be
larger; if this is not the case, then simply negate all values.
Optional CURRENT-VALUE specifies the progress by the moment you
call this function.  You should omit it or set it to nil in most
cases since it defaults to MIN-VALUE.

Optional MIN-CHANGE determines the minimal change in percents to
report (default is 1%.)  Optional MIN-TIME specifies the minimal
time before echo area updates (default is 0.2 seconds.)  If
`float-time' function is not present, then time is not tracked
at all.  If OS is not capable of measuring fractions of seconds,
then this parameter is effectively rounded up."

  (unless min-time
    (setq min-time 0.2))
  (let ((reporter
	 (cons min-value ;; Force a call to `message' now
	       (vector (if (and (fboundp 'float-time)
				(>= min-time 0.02))
			   (float-time) nil)
		       min-value
		       max-value
		       message
		       (if min-change (max (min min-change 50) 1) 1)
		       min-time))))
    (progress-reporter-update reporter (or current-value min-value))
    reporter))

(defun progress-reporter-force-update (reporter value &optional new-message)
  "Report progress of an operation in the echo area unconditionally.

First two parameters are the same as for
`progress-reporter-update'.  Optional NEW-MESSAGE allows you to
change the displayed message."
  (let ((parameters (cdr reporter)))
    (when new-message
      (aset parameters 3 new-message))
    (when (aref parameters 0)
      (aset parameters 0 (float-time)))
    (progress-reporter-do-update reporter value)))

(defun progress-reporter-do-update (reporter value)
  (let* ((parameters   (cdr reporter))
	 (min-value    (aref parameters 1))
	 (max-value    (aref parameters 2))
	 (one-percent  (/ (- max-value min-value) 100.0))
	 (percentage   (if (= max-value min-value)
			   0
			 (truncate (/ (- value min-value) one-percent))))
	 (update-time  (aref parameters 0))
	 (current-time (float-time))
	 (enough-time-passed
	  ;; See if enough time has passed since the last update.
	  (or (not update-time)
	      (when (>= current-time update-time)
		;; Calculate time for the next update
		(aset parameters 0 (+ update-time (aref parameters 5)))))))
    ;;
    ;; Calculate NEXT-UPDATE-VALUE.  If we are not going to print
    ;; message this time because not enough time has passed, then use
    ;; 1 instead of MIN-CHANGE.  This makes delays between echo area
    ;; updates closer to MIN-TIME.
    (setcar reporter
	    (min (+ min-value (* (+ percentage
				    (if enough-time-passed
					(aref parameters 4) ;; MIN-CHANGE
				      1))
				 one-percent))
		 max-value))
    (when (integerp value)
      (setcar reporter (ceiling (car reporter))))
    ;;
    ;; Only print message if enough time has passed
    (when enough-time-passed
      (if (> percentage 0)
	  (message "%s%d%%" (aref parameters 3) percentage)
	(message "%s" (aref parameters 3))))))

(defun progress-reporter-done (reporter)
  "Print reporter's message followed by word \"done\" in echo area."
  (message "%sdone" (aref (cdr reporter) 3)))

(defmacro dotimes-with-progress-reporter (spec message &rest body)
  "Loop a certain number of times and report progress in the echo area.
Evaluate BODY with VAR bound to successive integers running from
0, inclusive, to COUNT, exclusive.  Then evaluate RESULT to get
the return value (nil if RESULT is omitted).

At each iteration MESSAGE followed by progress percentage is
printed in the echo area.  After the loop is finished, MESSAGE
followed by word \"done\" is printed.  This macro is a
convenience wrapper around `make-progress-reporter' and friends.

\(fn (VAR COUNT [RESULT]) MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((temp (make-symbol "--dotimes-temp--"))
	(temp2 (make-symbol "--dotimes-temp2--"))
	(start 0)
	(end (nth 1 spec)))
    `(let ((,temp ,end)
	   (,(car spec) ,start)
	   (,temp2 (make-progress-reporter ,message ,start ,end)))
       (while (< ,(car spec) ,temp)
	 ,@body
	 (progress-reporter-update ,temp2
				   (setq ,(car spec) (1+ ,(car spec)))))
       (progress-reporter-done ,temp2)
       nil ,@(cdr (cdr spec)))))

;;;; Integer list & Version funs.

(defvar version-separator "."
  "*Specify the string used to separate the version elements.

Usually the separator is \".\", but it can be any other string.")


(defvar version-regexp-alist
  '(("^a\\(lpha\\)?$"   . -3)
    ("^b\\(eta\\)?$"    . -2)
    ("^\\(pre\\|rc\\)$" . -1))
  "*Specify association between non-numeric version part and a priority.

This association is used to handle version string like \"1.0pre2\",
\"0.9alpha1\", etc.  It's used by `version-to-list' (which see) to convert the
non-numeric part to an integer.  For example:

   String Version    Integer List Version
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8Beta3\"       (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9alpha\"        (0  9 -3)

Each element has the following form:

   (REGEXP . PRIORITY)

Where:

REGEXP		regexp used to match non-numeric part of a version string.

PRIORITY	negative integer which indicate the non-numeric priority.")


(defun version-to-list (ver)
  "Convert version string VER into an integer list.

The version syntax is given by the following EBNF:

   VERSION ::= NUMBER ( SEPARATOR NUMBER )*.

   NUMBER ::= (0|1|2|3|4|5|6|7|8|9)+.

   SEPARATOR ::= `version-separator' (which see)
	       | `version-regexp-alist' (which see).

As an example of valid version syntax:

   1.0pre2   1.0.7.5   22.8beta3   0.9alpha1

As an example of invalid version syntax:

   1.0prepre2   1.0..7.5   22.8X3   alpha3.2   .5

As an example of version convertion:

   String Version    Integer List Version
   \"1.0.7.5\"         (1  0  7 5)
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8Beta3\"       (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9alpha\"        (0  9 -3)

See documentation for `version-separator' and `version-regexp-alist'."
  (or (and (stringp ver) (not (string= ver "")))
      (error "Invalid version string: '%s'" ver))
  (save-match-data
    (let ((i 0)
	  case-fold-search		; ignore case in matching
	  lst s al)
      (while (and (setq s (string-match "[0-9]+" ver i))
		  (= s i))
	;; handle numeric part
	(setq lst (cons (string-to-number (substring ver i (match-end 0)))
			lst)
	      i   (match-end 0))
	;; handle non-numeric part
	(when (and (setq s (string-match "[^0-9]+" ver i))
		   (= s i))
	  (setq s (substring ver i (match-end 0))
		i (match-end 0))
	  ;; handle alpha, beta, pre, etc. separator
	  (unless (string= s version-separator)
	    (setq al version-regexp-alist)
	    (while (and al (not (string-match (caar al) s)))
	      (setq al (cdr al)))
	    (or al (error "Invalid version syntax: '%s'" ver))
	    (setq lst (cons (cdar al) lst)))))
      (if (null lst)
	  (error "Invalid version syntax: '%s'" ver)
	(nreverse lst)))))


(defun integer-list-< (l1 l2)
  "Return t if integer list L1 is lesser than L2.

Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
etc.  That is, the trailing zeroes are irrelevant.  Also, integer
list (1) is greater than (1 -1) which is greater than (1 -2)
which is greater than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null         ==> l1 length = l2 length
   ((and (null l1) (null l2)) nil)
   ;; l1 not null and l2 null     ==> l1 length > l2 length
   (l1 (< (integer-list-not-zero l1) 0))
   ;; l1 null and l2 not null     ==> l2 length > l1 length
   (t  (< 0 (integer-list-not-zero l2)))))


(defun integer-list-= (l1 l2)
  "Return t if integer list L1 is equal to L2.

Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
etc.  That is, the trailing zeroes are irrelevant.  Also, integer
list (1) is greater than (1 -1) which is greater than (1 -2)
which is greater than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) nil)
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (zerop (integer-list-not-zero l1)))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (zerop (integer-list-not-zero l2)))))


(defun integer-list-<= (l1 l2)
  "Return t if integer list L1 is lesser than or equal to L2.

Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
etc.  That is, the trailing zeroes are irrelevant.  Also, integer
list (1) is greater than (1 -1) which is greater than (1 -2)
which is greater than (1 -3)."
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (<= (integer-list-not-zero l1) 0))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (<= 0 (integer-list-not-zero l2)))))


(defalias 'version= 'string-equal
  "Return t if version V1 is equal to V2.

Compare version string using `string-equal'.")


(defun version< (v1 v2)
  "Return t if version V1 is lesser than V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are irrelevant.  Also, version string \"1\"
is greater than \"1pre\" which is greater than \"1beta\" which is greater than
\"1alpha\"."
  (integer-list-< (version-to-list v1) (version-to-list v2)))


(defun version<= (v1 v2)
  "Return t if version V1 is lesser than or equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are irrelevant.  Also, version string \"1\"
is greater than \"1pre\" which is greater than \"1beta\" which is greater than
\"1alpha\"."
  (integer-list-<= (version-to-list v1) (version-to-list v2)))


(defun integer-list-not-zero (lst)
  "Return the first non-zero element of integer list LST.

If all LST elements are zeroes or LST is nil, return zero."
  (while (zerop (car lst))
    (setq lst (cdr lst)))
  (if lst
      (car lst)
    ;; there is no element different of zero
    0))


;; arch-tag: f7e0e6e5-70aa-4897-ae72-7a3511ec40bc
;;; subr.el ends here
