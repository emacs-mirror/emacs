;;; subr.el --- basic lisp subroutines for Emacs

;; Copyright (C) 1985, 1986, 1992, 1994, 1995 Free Software Foundation, Inc.

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

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

(defmacro push (newelt listname)
  "Add NEWELT to the list which is the value of LISTNAME.
This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
LISTNAME must be a symbol."
  (list 'setq list
	(list 'cons newelt list)))

(defmacro pop (listname)
  "Return the first element of LISTNAME's value, and remove it from the list.
LISTNAME must be a symbol whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list."
  (list 'prog1 (list 'car listname)
	(list 'setq listname (list 'cdr listname))))

(defmacro when (cond &rest body)
  "(when COND BODY...): if COND yields non-nil, do BODY, else return nil."
  (list 'if cond (cons 'progn body)))
(put 'when 'lisp-indent-function 1)
(put 'when 'edebug-form-spec '(&rest form))

(defmacro unless (cond &rest body)
  "(unless COND BODY...): if COND yields nil, do BODY, else return nil."
  (cons 'if (cons cond (cons nil body))))
(put 'unless 'lisp-indent-function 1)
(put 'unless 'edebug-form-spec '(&rest form))

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

(defun last (x &optional n)
  "Return the last link of the list X.  Its car is the last element.
If X is nil, return nil.
If N is non-nil, return the Nth-to-last link of X.
If N is bigger than the length of X, return X."
  (if n
      (let ((m 0) (p x))
	(while (consp p)
	  (setq m (1+ m) p (cdr p)))
	(if (<= n 0) p
	  (if (< n m) (nthcdr (- m n) x) x)))
    (while (cdr x)
      (setq x (cdr x)))
    x))

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

(defun assoc-ignore-case (key alist)
  "Like `assoc', but ignores differences in case and text representation.
KEY must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
  (let (element)
    (while (and alist (not element))
      (if (eq t (compare-strings key 0 nil (car (car alist)) 0 nil t))
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))

(defun assoc-ignore-representation (key alist)
  "Like `assoc', but ignores differences in text representation.
KEY must be a string.  
Unibyte strings are converted to multibyte for comparison."
  (let (element)
    (while (and alist (not element))
      (if (eq t (compare-strings key 0 nil (car (car alist)) 0 nil))
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))

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
  (substitute-key-definition 'self-insert-command 'undefined map global-map)
  (or nodigits
      (let (loop)
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (define-key map (char-to-string loop) 'digit-argument)
	  (setq loop (1+ loop))))))

;Moved to keymap.c
;(defun copy-keymap (keymap)
;  "Return a copy of KEYMAP"  
;  (while (not (keymapp keymap))
;    (setq keymap (signal 'wrong-type-argument (list 'keymapp keymap))))
;  (if (vectorp keymap)
;      (copy-sequence keymap)
;      (copy-alist keymap)))

(defvar key-substitution-in-progress nil
 "Used internally by substitute-key-definition.")

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
If optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those chars which are defined as OLDDEF in OLDMAP."
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (vec1 (vector nil))
	 (prefix1 (vconcat prefix vec1))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (while (consp scan)
      (if (consp (car scan))
	  (let ((char (car (car scan)))
		(defn (cdr (car scan))))
	    ;; The inside of this let duplicates exactly
	    ;; the inside of the following let that handles array elements.
	    (aset vec1 0 char)
	    (aset prefix1 (length prefix) char)
	    (let (inner-def skipped)
	      ;; Skip past menu-prompt.
	      (while (stringp (car-safe defn))
		(setq skipped (cons (car defn) skipped))
		(setq defn (cdr defn)))
	      ;; Skip past cached key-equivalence data for menu items.
	      (and (consp defn) (consp (car defn))
		   (setq defn (cdr defn)))
	      (setq inner-def defn)
	      ;; Look past a symbol that names a keymap.
	      (while (and (symbolp inner-def)
			  (fboundp inner-def))
		(setq inner-def (symbol-function inner-def)))
	      (if (or (eq defn olddef)
		      ;; Compare with equal if definition is a key sequence.
		      ;; That is useful for operating on function-key-map.
		      (and (or (stringp defn) (vectorp defn))
			   (equal defn olddef)))
		  (define-key keymap prefix1 (nconc (nreverse skipped) newdef))
		(if (and (keymapp defn)
			 ;; Avoid recursively scanning
			 ;; where KEYMAP does not have a submap.
			 (let ((elt (lookup-key keymap prefix1)))
			   (or (null elt)
			       (keymapp elt)))
			 ;; Avoid recursively rescanning keymap being scanned.
			 (not (memq inner-def
				    key-substitution-in-progress)))
		    ;; If this one isn't being scanned already,
		    ;; scan it now.
		    (substitute-key-definition olddef newdef keymap
					       inner-def
					       prefix1)))))
	(if (vectorp (car scan))
	    (let* ((array (car scan))
		   (len (length array))
		   (i 0))
	      (while (< i len)
		(let ((char i) (defn (aref array i)))
		  ;; The inside of this let duplicates exactly
		  ;; the inside of the previous let.
		  (aset vec1 0 char)
		  (aset prefix1 (length prefix) char)
		  (let (inner-def skipped)
		    ;; Skip past menu-prompt.
		    (while (stringp (car-safe defn))
		      (setq skipped (cons (car defn) skipped))
		      (setq defn (cdr defn)))
		    (and (consp defn) (consp (car defn))
			 (setq defn (cdr defn)))
		    (setq inner-def defn)
		    (while (and (symbolp inner-def)
				(fboundp inner-def))
		      (setq inner-def (symbol-function inner-def)))
		    (if (or (eq defn olddef)
			    (and (or (stringp defn) (vectorp defn))
				 (equal defn olddef)))
			(define-key keymap prefix1
			  (nconc (nreverse skipped) newdef))
		      (if (and (keymapp defn)
			       (let ((elt (lookup-key keymap prefix1)))
				 (or (null elt)
				     (keymapp elt)))
			       (not (memq inner-def
					  key-substitution-in-progress)))
			  (substitute-key-definition olddef newdef keymap
						     inner-def
						     prefix1)))))
		(setq i (1+ i))))
	  (if (char-table-p (car scan))
	      (map-char-table
	       (function (lambda (char defn)
			   (let ()
			     ;; The inside of this let duplicates exactly
			     ;; the inside of the previous let,
			     ;; except that it uses set-char-table-range
			     ;; instead of define-key.
			     (aset vec1 0 char)
			     (aset prefix1 (length prefix) char)
			     (let (inner-def skipped)
			       ;; Skip past menu-prompt.
			       (while (stringp (car-safe defn))
				 (setq skipped (cons (car defn) skipped))
				 (setq defn (cdr defn)))
			       (and (consp defn) (consp (car defn))
				    (setq defn (cdr defn)))
			       (setq inner-def defn)
			       (while (and (symbolp inner-def)
					   (fboundp inner-def))
				 (setq inner-def (symbol-function inner-def)))
			       (if (or (eq defn olddef)
				       (and (or (stringp defn) (vectorp defn))
					    (equal defn olddef)))
				   (define-key keymap prefix1
				     (nconc (nreverse skipped) newdef))
				 (if (and (keymapp defn)
					  (let ((elt (lookup-key keymap prefix1)))
					    (or (null elt)
						(keymapp elt)))
					  (not (memq inner-def
						     key-substitution-in-progress)))
				     (substitute-key-definition olddef newdef keymap
								inner-def
								prefix1)))))))
	       (car scan)))))
      (setq scan (cdr scan)))))

(defun define-key-after (keymap key definition after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `define-key' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t, the new binding goes at the end of the keymap.

KEY must contain just one event type--that is to say, it must be
a string or vector of length 1.

The order of bindings in a keymap matters when it is used as a menu."

  (or (keymapp keymap)
      (signal 'wrong-type-argument (list 'keymapp keymap)))
  (if (> (length key) 1)
      (error "multi-event key specified in `define-key-after'"))
  (let ((tail keymap) done inserted
	(first (aref key 0)))
    (while (and (not done) tail)
      ;; Delete any earlier bindings for the same key.
      (if (eq (car-safe (car (cdr tail))) first)
	  (setcdr tail (cdr (cdr tail))))
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
		(setcdr tail (cons (cons (aref key 0) definition) (cdr tail))))
	    (setq inserted t)))
      (setq tail (cdr tail)))))

(defmacro kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `insert-kbd-macro')."
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
  "Keymap for subcommands of C-x 4")
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
	    (append key nil))))

(defsubst eventp (obj)
  "True if the argument is an event object."
  (or (integerp obj)
      (and (symbolp obj)
	   (get obj 'event-symbol-elements))
      (and (consp obj)
	   (symbolp (car obj))
	   (get (car obj) 'event-symbol-elements))))

(defun event-modifiers (event)
  "Returns a list of symbols representing the modifier keys in event EVENT.
The elements of the list may include `meta', `control',
`shift', `hyper', `super', `alt', `click', `double', `triple', `drag',
and `down'."
  (let ((type event))
    (if (listp type)
	(setq type (car type)))
    (if (symbolp type)
	(cdr (get type 'event-symbol-elements))
      (let ((list nil))
	(or (zerop (logand type ?\M-\^@))
	    (setq list (cons 'meta list)))
	(or (and (zerop (logand type ?\C-\^@))
		 (>= (logand type 127) 32))
	    (setq list (cons 'control list)))
	(or (and (zerop (logand type ?\S-\^@))
		 (= (logand type 255) (downcase (logand type 255))))
	    (setq list (cons 'shift list)))
	(or (zerop (logand type ?\H-\^@))
	    (setq list (cons 'hyper list)))
	(or (zerop (logand type ?\s-\^@))
	    (setq list (cons 'super list)))
	(or (zerop (logand type ?\A-\^@))
	    (setq list (cons 'alt list)))
	list))))

(defun event-basic-type (event)
  "Returns the basic type of the given event (all modifiers removed).
The value is an ASCII printing character (not upper case) or a symbol."
  (if (consp event)
      (setq event (car event)))
  (if (symbolp event)
      (car (get event 'event-symbol-elements))
    (let ((base (logand event (1- (lsh 1 18)))))
      (downcase (if (< base 32) (logior base 64) base)))))

(defsubst mouse-movement-p (object)
  "Return non-nil if OBJECT is a mouse movement event."
  (and (consp object)
       (eq (car object) 'mouse-movement)))

(defsubst event-start (event)
  "Return the starting position of EVENT.
If EVENT is a mouse press or a mouse click, this returns the location
of the event.
If EVENT is a drag, this returns the drag's starting position.
The return value is of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
The `posn-' functions access elements of such lists."
  (nth 1 event))

(defsubst event-end (event)
  "Return the ending location of EVENT.  EVENT should be a click or drag event.
If EVENT is a click event, this function is the same as `event-start'.
The return value is of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
The `posn-' functions access elements of such lists."
  (nth (if (consp (nth 2 event)) 2 1) event))

(defsubst event-click-count (event)
  "Return the multi-click count of EVENT, a click or drag event.
The return value is a positive integer."
  (if (integerp (nth 2 event)) (nth 2 event) 1))

(defsubst posn-window (position)
  "Return the window in POSITION.
POSITION should be a list of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
as returned by the `event-start' and `event-end' functions."
  (nth 0 position))

(defsubst posn-point (position)
  "Return the buffer location in POSITION.
POSITION should be a list of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
as returned by the `event-start' and `event-end' functions."
  (if (consp (nth 1 position))
      (car (nth 1 position))
    (nth 1 position)))

(defsubst posn-x-y (position)
  "Return the x and y coordinates in POSITION.
POSITION should be a list of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
as returned by the `event-start' and `event-end' functions."
  (nth 2 position))

(defun posn-col-row (position)
  "Return the column and row in POSITION, measured in characters.
POSITION should be a list of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
as returned by the `event-start' and `event-end' functions.
For a scroll-bar event, the result column is 0, and the row
corresponds to the vertical position of the click in the scroll bar."
  (let ((pair   (nth 2 position))
	(window (posn-window position)))
    (if (eq (if (consp (nth 1 position))
		(car (nth 1 position))
	      (nth 1 position))
	    'vertical-scroll-bar)
	(cons 0 (scroll-bar-scale pair (1- (window-height window))))
      (if (eq (if (consp (nth 1 position))
		  (car (nth 1 position))
		(nth 1 position))
	      'horizontal-scroll-bar)
	  (cons (scroll-bar-scale pair (window-width window)) 0)
	(let* ((frame (if (framep window) window (window-frame window)))
	       (x (/ (car pair) (frame-char-width frame)))
	       (y (/ (cdr pair) (frame-char-height frame))))
	  (cons x y))))))

(defsubst posn-timestamp (position)
  "Return the timestamp of POSITION.
POSITION should be a list of the form
   (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
as returned by the `event-start' and `event-end' functions."
  (nth 3 position))


;;;; Obsolescent names for functions.

(defalias 'dot 'point)
(defalias 'dot-marker 'point-marker)
(defalias 'dot-min 'point-min)
(defalias 'dot-max 'point-max)
(defalias 'window-dot 'window-point)
(defalias 'set-window-dot 'set-window-point)
(defalias 'read-input 'read-string)
(defalias 'send-string 'process-send-string)
(defalias 'send-region 'process-send-region)
(defalias 'show-buffer 'set-window-buffer)
(defalias 'buffer-flush-undo 'buffer-disable-undo)
(defalias 'eval-current-buffer 'eval-buffer)
(defalias 'compiled-function-p 'byte-code-function-p)
(defalias 'define-function 'defalias)

(defalias 'sref 'aref)
(make-obsolete 'sref 'aref)
(make-obsolete 'char-bytes "Now this function always returns 1")

;; Some programs still use this as a function.
(defun baud-rate ()
  "Obsolete function returning the value of the `baud-rate' variable.
Please convert your programs to use the variable `baud-rate' directly."
  baud-rate)

(defalias 'focus-frame 'ignore)
(defalias 'unfocus-frame 'ignore)

;;;; Alternate names for functions - these are not being phased out.

(defalias 'string= 'string-equal)
(defalias 'string< 'string-lessp)
(defalias 'move-marker 'set-marker)
(defalias 'not 'null)
(defalias 'rplaca 'setcar)
(defalias 'rplacd 'setcdr)
(defalias 'beep 'ding) ;preserve lingual purity
(defalias 'indent-to-column 'indent-to)
(defalias 'backward-delete-char 'delete-backward-char)
(defalias 'search-forward-regexp (symbol-function 're-search-forward))
(defalias 'search-backward-regexp (symbol-function 're-search-backward))
(defalias 'int-to-string 'number-to-string)
(defalias 'store-match-data 'set-match-data)
(defalias 'point-at-eol 'line-end-position)
(defalias 'point-at-bol 'line-beginning-position)

;;; Should this be an obsolete name?  If you decide it should, you get
;;; to go through all the sources and change them.
(defalias 'string-to-int 'string-to-number)

;;;; Hook manipulation functions.

(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
The return value is HOOK.

When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function works by making `t' a member of the buffer-local value,
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

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (if (or local
	  ;; Detect the case where make-local-variable was used on a hook
	  ;; and do what we used to do.
	  (and (local-variable-if-set-p hook)
	       (not (memq t (symbol-value hook)))))
      ;; Alter the local value only.
      (or (if (or (consp function) (byte-code-function-p function))
	      (member function (symbol-value hook))
	    (memq function (symbol-value hook)))
	  (set hook 
	       (if append
		   (append (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))
    ;; Alter the global value (which is also the only value,
    ;; if the hook doesn't have a local value).
    (or (if (or (consp function) (byte-code-function-p function))
	    (member function (default-value hook))
	  (memq function (default-value hook)))
	(set-default hook 
		     (if append
			 (append (default-value hook) (list function))
		       (cons function (default-value hook)))))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (not (default-boundp hook))
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (if (or local
	    ;; Detect the case where make-local-variable was used on a hook
	    ;; and do what we used to do.
	    (and (local-variable-p hook)
		  (consp (symbol-value hook))
		  (not (memq t (symbol-value hook)))))
	(let ((hook-value (symbol-value hook)))
	  (if (consp hook-value)
	      (if (member function hook-value)
		  (setq hook-value (delete function (copy-sequence hook-value))))
	    (if (equal hook-value function)
		(setq hook-value nil)))
	  (set hook hook-value))
      (let ((hook-value (default-value hook)))
	(if (and (consp hook-value) (not (functionp hook-value)))
	    (if (member function hook-value)
		(setq hook-value (delete function (copy-sequence hook-value))))
	  (if (equal hook-value function)
	      (setq hook-value nil)))
	(set-default hook hook-value)))))

(defun add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var (cons element (symbol-value list-var)))))

;;;; Specifying things to do after certain files are loaded.

(defun eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
  ;; Make sure there is an element for FILE.
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  ;; Add FORM to the element if it isn't there.
  (let ((elt (assoc file after-load-alist)))
    (or (member form (cdr elt))
	(progn
	  (nconc elt (list form))
	  ;; If the file has been loaded already, run FORM right away.
	  (and (assoc file load-history)
	       (eval form)))))
  form)

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))


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
  (let ((message-log-max nil) done (first t) (code 0) char)
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
      (and char
	   (let ((translated (lookup-key function-key-map (vector char))))
	     (if (arrayp translated)
		 (setq char (aref translated 0)))))
      (cond ((null char))
	    ((not (integerp char))
	     (setq unread-command-events (list char)
		   done t))
	    ((/= (logand char ?\M-\^@) 0)
	     ;; Turn a meta-character into a character with the 0200 bit set.
	     (setq code (logior (logand char (lognot ?\M-\^@)) 128)
		   done t))
	    ((and (<= ?0 char) (< char (+ ?0 (min 10 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix) (- char ?0)))
	     (and prompt (setq prompt (message "%s %c" prompt char))))
	    ((and (<= ?a (downcase char))
		  (< (downcase char) (+ ?a -10 (min 26 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix)
			   (+ 10 (- (downcase char) ?a))))
	     (and prompt (setq prompt (message "%s %c" prompt char))))
	    ((and (not first) (eq char ?\C-m))
	     (setq done t))
	    ((not first)
	     (setq unread-command-events (list char)
		   done t))
	    (t (setq code char
		     done t)))
      (setq first nil))
    code))

(defun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT.  Echo `.' for each character typed.
End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
Optional argument CONFIRM, if non-nil, then read it twice to make sure.
Optional DEFAULT is a default password to use instead of empty input."
  (if confirm
      (let (success)
	(while (not success)
	  (let ((first (read-passwd prompt nil default))
		(second (read-passwd "Confirm password: " nil default)))
	    (if (equal first second)
		(setq success first)
	      (message "Password not repeated accurately; please start over")
	      (sit-for 1))))
	success)
    (clear-this-command-keys)
    (let ((pass nil)
	  (c 0)
	  (echo-keystrokes 0)
	  (cursor-in-echo-area t))
      (while (progn (message "%s%s"
			     prompt
			     (make-string (length pass) ?.))
		    (setq c (read-char nil t))
		    (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
	(if (= c ?\C-u)
	    (setq pass "")
	  (if (and (/= c ?\b) (/= c ?\177))
	      (setq pass (concat pass (char-to-string c)))
	    (if (> (length pass) 0)
		(setq pass (substring pass 0 -1))))))
      (message nil)
      (or pass default ""))))

(defun force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL, force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(defun momentary-string-display (string pos &optional exit-char message) 
  "Momentarily display STRING in the buffer at POS.
Display remains until next character is typed.
If the char is EXIT-CHAR (optional third arg, default is SPC) it is swallowed;
otherwise it is then available as input (as a command if nothing else).
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
	  (let ((char (read-event)))
	    (or (eq char exit-char)
		(setq unread-command-events (list char)))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))


;;;; Miscellanea.

;; A number of major modes set this locally.
;; Give it a global value to avoid compiler warnings.
(defvar font-lock-defaults nil)

(defvar suspend-hook nil
  "Normal hook run by `suspend-emacs', before suspending.")

(defvar suspend-resume-hook nil
  "Normal hook run by `suspend-emacs', after Emacs is continued.")

;; Avoid compiler warnings about this variable,
;; which has a special meaning on certain system types.
(defvar buffer-file-type nil
  "Non-nil if the visited file is a binary file.
This variable is meaningful on MS-DOG and Windows NT.
On those systems, it is automatically local in every buffer.
On other systems, this variable is normally always nil.")

;; This should probably be written in C (i.e., without using `walk-windows').
(defun get-buffer-window-list (buffer &optional minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
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

(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER COMMAND &rest COMMAND-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is command name, the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handled as usual in the shell."
  (cond
   ((eq system-type 'vax-vms)
    (apply 'start-process name buffer args))
   ;; We used to use `exec' to replace the shell with the command,
   ;; but that failed to handle (...) and semicolon, etc.
   (t
    (start-process name buffer shell-file-name shell-command-switch
		   (mapconcat 'identity args " ")))))

(defmacro with-current-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  `(save-current-buffer
    (set-buffer ,buffer)
    ,@body))

(defmacro with-temp-file (file &rest body)
  "Create a new buffer, evaluate BODY there, and write the buffer to FILE.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
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
	 (and ,temp-message ,current-message
	      (message "%s" ,current-message))))))

(defmacro with-temp-buffer (&rest body)
  "Create a temporary buffer, and evaluate BODY there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp*"))))
       (unwind-protect
	   (with-current-buffer ,temp-buffer
	     ,@body)
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  `(let ((standard-output
	  (get-buffer-create (generate-new-buffer-name " *string-output*"))))
     (let ((standard-output standard-output))
       ,@body)
     (with-current-buffer standard-output
       (prog1
	   (buffer-string)
	 (kill-buffer nil)))))

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
  `(unwind-protect
       (let ((combine-after-change-calls t))
	 . ,body)
     (combine-after-change-execute)))


(defvar save-match-data-internal)

;; We use save-match-data-internal as the local variable because
;; that works ok in practice (people should not use that variable elsewhere).
;; We used to use an uninterned symbol; the compiler handles that properly
;; now, but it generates slower code.
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  `(let ((save-match-data-internal (match-data)))
       (unwind-protect
	   (progn ,@body)
	 (set-match-data save-match-data-internal))))

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
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

(defun split-string (string &optional separators)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
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

(defun shell-quote-argument (argument)
  "Quote an argument for passing as argument to an inferior shell."
  (if (eq system-type 'ms-dos)
      ;; MS-DOS shells don't have quoting, so don't do any.
      argument
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
If OLDTABLE is non-nil, copy OLDTABLE.
Otherwise, create a syntax table which inherits
all letters and control characters from the standard syntax table;
other characters are copied from the standard syntax table."
  (if oldtable
      (copy-syntax-table oldtable)
    (let ((table (copy-syntax-table))
	  i)
      (setq i 0)
      (while (<= i 31)
	(aset table i nil)
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(aset table i nil)
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(aset table i nil)
	(setq i (1+ i)))
      (setq i 128)
      (while (<= i 255)
	(aset table i nil)
	(setq i (1+ i)))
      table)))

(defun add-to-invisibility-spec (arg)
  "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (cond
   ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
	(setq buffer-invisibility-spec (list arg)))
   (t
    (setq buffer-invisibility-spec
	  (cons arg buffer-invisibility-spec)))))

(defun remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (consp buffer-invisibility-spec)
    (setq buffer-invisibility-spec (delete arg buffer-invisibility-spec))))

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
KEY is a string representing a sequence of keystrokes."
  (interactive "kUnset key globally: ")
  (global-set-key key nil))

(defun local-unset-key (key)
  "Remove local binding of KEY.
KEY is a string representing a sequence of keystrokes."
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

(defun functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

;; now in fns.c
;(defun nth (n list)
;  "Returns the Nth element of LIST.
;N counts from zero.  If LIST is not that long, nil is returned."
;  (car (nthcdr n list)))
;
;(defun copy-alist (alist)
;  "Return a copy of ALIST.
;This is a new alist which represents the same mapping
;from objects to objects, but does not share the alist structure with ALIST.
;The objects mapped (cars and cdrs of elements of the alist)
;are shared, however."
;  (setq alist (copy-sequence alist))
;  (let ((tail alist))
;    (while tail
;      (if (consp (car tail))
;	  (setcar tail (cons (car (car tail)) (cdr (car tail)))))
;      (setq tail (cdr tail))))
;  alist)

(defun assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  (setq alist (copy-sequence alist))
  (let ((tail alist))
    (while tail
      (if (eq (car (car tail)) key)
	  (setq alist (delq (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

;;; subr.el ends here
