;;; subr.el --- basic lisp subroutines for Emacs

;;; Copyright (C) 1985, 1986, 1992, 1994, 1995 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:


;;;; Lisp language features.

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING should be a string, as described for `defun'.  It may be omitted.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

;;(defmacro defun-inline (name args &rest body)
;;  "Create an \"inline defun\" (actually a macro).
;;Use just like `defun'."
;;  (nconc (list 'defmacro name '(&rest args))
;;	 (if (stringp (car body))
;;	     (prog1 (list (car body))
;;	       (setq body (or (cdr body) body))))
;;	 (list (list 'cons (list 'quote
;;				 (cons 'lambda (cons args body)))
;;		     'args))))


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
	      (if (eq defn olddef)
		  (define-key keymap prefix1 (nconc (nreverse skipped) newdef))
		(if (and (keymapp defn)
			 ;; Avoid recursively scanning
			 ;; where KEYMAP does not have a submap.
			 (keymapp (lookup-key keymap prefix1))
			 ;; Avoid recursively rescanning keymap being scanned.
			 (not (memq inner-def
				    key-substitution-in-progress)))
		    ;; If this one isn't being scanned already,
		    ;; scan it now.
		    (substitute-key-definition olddef newdef keymap
					       inner-def
					       prefix1)))))
	(if (arrayp (car scan))
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
		    (if (eq defn olddef)
			(define-key keymap prefix1
			  (nconc (nreverse skipped) newdef))
		      (if (and (keymapp defn)
			       (keymapp (lookup-key keymap prefix1))
			       (not (memq inner-def
					  key-substitution-in-progress)))
			  (substitute-key-definition olddef newdef keymap
						     inner-def
						     prefix1)))))
		(setq i (1+ i))))))
      (setq scan (cdr scan)))))

(defun define-key-after (keymap key definition after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `define-key' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.
The order matters when the keymap is used as a menu.
KEY must contain just one event type--that is to say, it must be
a string or vector of length 1."
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
      (if (or (eq (car-safe (car tail)) after)
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

(defun keyboard-translate (from to)
  "Translate character FROM to TO at a low level.
This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it."
  (or (arrayp keyboard-translate-table)
      (setq keyboard-translate-table ""))
  (if (or (> from (length keyboard-translate-table))
	  (> to   (length keyboard-translate-table)))
      (progn
	(let* ((i (length keyboard-translate-table))
	       (table (concat keyboard-translate-table
			      (make-string (- 256 i) 0))))
	  (while (< i 256)
	    (aset table i i)
	    (setq i (1+ i)))
	  (setq keyboard-translate-table table))))
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

;; Some programs still use this as a function.
(defun baud-rate ()
  "Obsolete function returning the value of the `baud-rate' variable.
Please convert your programs to use the variable `baud-rate' directly."
  baud-rate)


;;;; Alternate names for functions - these are not being phased out.

(defalias 'string= 'string-equal)
(defalias 'string< 'string-lessp)
(defalias 'move-marker 'set-marker)
(defalias 'eql 'eq)
(defalias 'not 'null)
(defalias 'rplaca 'setcar)
(defalias 'rplacd 'setcdr)
(defalias 'beep 'ding) ;preserve lingual purity
(defalias 'indent-to-column 'indent-to)
(defalias 'backward-delete-char 'delete-backward-char)
(defalias 'search-forward-regexp (symbol-function 're-search-forward))
(defalias 'search-backward-regexp (symbol-function 're-search-backward))
(defalias 'int-to-string 'number-to-string)
(defalias 'set-match-data 'store-match-data)

;;; Should this be an obsolete name?  If you decide it should, you get
;;; to go through all the sources and change them.
(defalias 'string-to-int 'string-to-number)

;;;; Hook manipulation functions.

(defun run-hooks (&rest hooklist)
  "Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

To make a hook variable buffer-local, use `make-local-hook', not
`make-local-variable'."
  (while hooklist
    (let ((sym (car hooklist)))
      (and (boundp sym)
	   (symbol-value sym)
	   (let ((value (symbol-value sym)))
	     (if (and (listp value) (not (eq (car value) 'lambda)))
		 (while value
		   (if (eq (car value) t)
		       ;; t indicates this hook has a local binding;
		       ;; it means to run the global binding too.
		       (let ((functions (default-value sym)))
			 (while functions
			   (funcall (car functions))
			   (setq functions (cdr functions))))
		     (funcall (car value)))
		   (setq value (cdr value)))
	       (funcall value)))))
    (setq hooklist (cdr hooklist))))

(defun run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value return by `run-hook-with-args',
as that may change.

To make a hook variable buffer-local, use `make-local-hook', not
`make-local-variable'."
  (and (boundp hook)
       (symbol-value hook)
       (let ((value (symbol-value hook)))
	 (if (and (listp value) (not (eq (car value) 'lambda)))
	     (while value
	       (if (eq (car value) t)
		   ;; t indicates this hook has a local binding;
		   ;; it means to run the global binding too.
		   (let ((functions (default-value hook)))
		     (while functions
		       (apply (car functions) args)
		       (setq functions (cdr functions))))
		 (apply (car value) args))
	       (setq value (cdr value)))
	   (apply value args)))))

(defun run-hook-with-args-until-success (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns a non-nil value.  Then we return that value.
If all the functions return nil, we return nil.

To make a hook variable buffer-local, use `make-local-hook', not
`make-local-variable'."
  (and (boundp hook)
       (symbol-value hook)
       (let ((value (symbol-value hook))
	     success)
	 (while (and value (not success))
	   (if (eq (car value) t)
	       ;; t indicates this hook has a local binding;
	       ;; it means to run the global binding too.
	       (let ((functions (default-value hook)))
		 (while (and functions (not success))
		   (setq success (apply (car functions) args))
		   (setq functions (cdr functions))))
	     (setq success (apply (car value) args)))
	   (setq value (cdr value)))
	 success)))

(defun run-hook-with-args-until-failure (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns nil.  Then we return nil.
If all the functions return non-nil, we return non-nil.

To make a hook variable buffer-local, use `make-local-hook', not
`make-local-variable'."
  ;; We must return non-nil if there are no hook functions!
  (or (not (boundp hook))
      (not (symbol-value hook))
      (let ((value (symbol-value hook))
	    (success t))
	(while (and value success)
	  (if (eq (car value) t)
	      ;; t indicates this hook has a local binding;
	      ;; it means to run the global binding too.
	      (let ((functions (default-value hook)))
		(while (and functions success)
		  (setq success (apply (car functions) args))
		  (setq functions (cdr functions))))
	    (setq success (apply (car value) args)))
	  (setq value (cdr value)))
	success)))

;; Tell C code how to call this function.
(defconst run-hooks 'run-hooks
  "Variable by which C primitives find the function `run-hooks'.
Don't change it.")

(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function does nothing if HOOK is already local in the current buffer.

Do not use `make-local-variable' to make a hook variable buffer-local."
  (if (local-variable-p hook)
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t))))

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
	  (and (local-variable-p hook)
	       (not (memq t (symbol-value hook)))))
      ;; Alter the local value only.
      (or (if (consp function)
	      (member function (symbol-value hook))
	    (memq function (symbol-value hook)))
	  (set hook 
	       (if append
		   (append (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))
    ;; Alter the global value (which is also the only value,
    ;; if the hook doesn't have a local value).
    (or (if (consp function)
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
	  (not (default-boundp 'hook))
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (if (or local
	    ;; Detect the case where make-local-variable was used on a hook
	    ;; and do what we used to do.
	    (and (local-variable-p hook)
		 (not (memq t (symbol-value hook)))))
	(let ((hook-value (symbol-value hook)))
	  (if (consp hook-value)
	      (if (member function hook-value)
		  (setq hook-value (delete function (copy-sequence hook-value))))
	    (if (equal hook-value function)
		(setq hook-value nil)))
	  (set hook hook-value))
      (let ((hook-value (default-value hook)))
	(if (consp hook-value)
	    (if (member function hook-value)
		(setq hook-value (delete function (copy-sequence hook-value))))
	  (if (equal hook-value function)
	      (setq hook-value nil)))
	(set-default hook hook-value)))))

(defun add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (or (member element (symbol-value list-var))
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

(defun read-quoted-char (&optional prompt)
  "Like `read-char', except that if the first character read is an octal
digit, we read up to two more octal digits and return the character
represented by the octal number consisting of those digits.
Optional argument PROMPT specifies a string to use to prompt the user."
  (let ((message-log-max nil) (count 0) (code 0) char)
    (while (< count 3)
      (let ((inhibit-quit (zerop count))
	    (help-form nil))
	(and prompt (message "%s-" prompt))
	(setq char (read-char))
	(if inhibit-quit (setq quit-flag nil)))
      (cond ((null char))
	    ((and (<= ?0 char) (<= char ?7))
	     (setq code (+ (* code 8) (- char ?0))
		   count (1+ count))
	     (and prompt (message (setq prompt
					(format "%s %c" prompt char)))))
	    ((> count 0)
	     (setq unread-command-events (list char) count 259))
	    (t (setq code char count 259))))
    ;; Turn a meta-character into a character with the 0200 bit set.
    (logior (if (/= (logand code ?\M-\^@) 0) 128 0)
	    (logand 255 code))))

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
  (let ((buffer-read-only nil)
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
	    (if (> (window-end) insert-end)
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

;; Avoid compiler warnings about this variable,
;; which has a special meaning on certain system types.
(defvar buffer-file-type nil
  "Non-nil if the visited file is a binary file.
This variable is meaningful on MS-DOG and Windows NT.
On those systems, it is automatically local in every buffer.
On other systems, this variable is normally always nil.")

(defun ignore (&rest ignore)
  "Do nothing and return nil.
This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'."
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

(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
	  (list 'unwind-protect
		(cons 'progn body)
		(list 'store-match-data original)))))

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

(defun buffer-substring-no-properties (beg end)
  "Return the text from BEG to END, without text properties, as a string."
  (let ((string (buffer-substring beg end)))
    (set-text-properties 0 (length string) nil string)
    string))

(defun shell-quote-argument (argument)
  "Quote an argument for passing as argument to an inferior shell."
  ;; Quote everything except POSIX filename characters.
  ;; This should be safe enough even for really weird shells.
  (let ((result "") (start 0) end)
    (while (string-match "[^-0-9a-zA-Z_./]" argument start)
      (setq end (match-beginning 0)
	    result (concat result (substring argument start end)
			   "\\" (substring argument end (1+ end)))
	    start (1+ end)))
    (concat result (substring argument start))))

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
It inherits all letters and control characters from the standard
syntax table; other characters are copied from the standard syntax table."
  (if oldtable
      (copy-syntax-table oldtable)
    (let ((table (copy-syntax-table))
	  i)
      (setq i 0)
      (while (<= i 31)
	(aset table i 13)
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(aset table i 13)
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(aset table i 13)
	(setq i (1+ i)))
      (setq i 128)
      (while (<= i 255)
	(aset table i 13)
	(setq i (1+ i)))
      table)))

(defun global-set-key (key command)
  "Give KEY a global binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEY is a key sequence (a string or vector of characters or event types).
Non-ASCII characters with codes above 127 (such as ISO Latin-1)
can be included if you use a vector.
Note that if KEY has a local binding in the current buffer
that local binding will continue to shadow any global binding."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key (current-global-map) key command)
  nil)

(defun local-set-key (key command)
  "Give KEY a local binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEY is a key sequence (a string or vector of characters or event types).
Non-ASCII characters with codes above 127 (such as ISO Latin-1)
can be included if you use a vector.
The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "KSet key locally: \nCSet key %s locally to command: ")
  (let ((map (current-local-map)))
    (or map
	(use-local-map (setq map (make-sparse-keymap))))
    (or (vectorp key) (stringp key)
	(signal 'wrong-type-argument (list 'arrayp key)))
    (define-key map key command))
  nil)

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

;;; subr.el ends here

