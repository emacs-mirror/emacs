;; Emulate the Lucid event data type and associated functions.
;; Copyright (C) 1993 Free Software Foundation, Inc.

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

;;; Notes:

;; Things we cannot emulate in Lisp:
;; It is not possible to emulate current-mouse-event as a variable,
;; though it is not hard to obtain the data from (this-command-keys).

;; We don't have variables last-command-event and last-input-event;
;; instead, we made last-...-char have these values.

;; We do not have a variable unread-command-event;
;; instead, we have the more general unread-command-events.

;; We could support those variables with C code as part of a merge.

;;current-mouse-event

;;The mouse-button event which invoked this command, or nil.
;;This is what (interactive "e") returns.

;;------------------------------
;;last-command-event

;;Last keyboard or mouse button event that was part of a command.  This
;;variable is off limits: you may not set its value or modify the event that
;;is its value, as it is destructively modified by read-key-sequence.  If
;;you want to keep a pointer to this value, you must use copy-event.

;;------------------------------
;;last-input-event

;;Last keyboard or mouse button event recieved.  This variable is off
;;limits: you may not set its value or modify the event that is its value, as
;;it is destructively modified by next-event.  If you want to keep a pointer
;;to this value, you must use copy-event.

;;------------------------------
;;unread-command-event

;;Set this to an event object to simulate the reciept of an event from
;;the user.  Normally this is nil.

;;[The variable unread-command-char no longer exists, because with the new event
;; model, it is incorrect for code to do (setq unread-command-char (read-char)),
;; because all user-input can't be represented as ASCII characters.  

;; A compatibility hack could be added to check unread-command-char as well as 
;; unread-command-event; or to only use unread-command-char and allow it to be
;; an ASCII code or an event, but I think that's a bad idea because it would 
;; allow incorrect code to work so long as someone didn't type a character 
;; without an ASCII equivalent, making it likely that such code would not get
;; fixed.]


;;Other related functions:
;;==============================

;;read-char ()

;;Read a character from the command input (keyboard or macro).
;;If a mouse click is detected, an error is signalled.  The character typed
;;is returned as an ASCII value.  This is most likely the wrong thing for you
;;to be using: consider using the `next-command-event' function instead.

;;------------------------------
;;read-key-sequence (prompt)

;;Read a sequence of keystrokes or mouse clicks and return a vector of the
;;event objects read.  The vector is newly created, but the event objects are
;;reused: if you want to hold a pointer to them beyond the next call to this
;;function, you must copy them first.

;;The sequence read is sufficient to specify a non-prefix command starting
;;from the current local and global keymaps.  A C-g typed while in this
;;function is treated like any other character, and quit-flag is not set.
;;One arg, PROMPT, is a prompt string, or nil meaning do not prompt specially.

;;If the user selects a menu item while we are prompting for a key-sequence,
;;the returned value will be a vector of a single menu-selection event.
;;An error will be signalled if you pass this value to lookup-key or a
;;related function.

;;------------------------------
;;recent-keys ()

;;Return vector of last 100 keyboard or mouse button events read.
;;This copies 100 event objects and a vector; it is safe to keep and modify
;;them.
;;------------------------------


;;Other related variables:
;;==============================

;;executing-kbd-macro

;;Currently executing keyboard macro (a vector of events);
;;nil if none executing.

;;------------------------------
;;executing-macro

;;Currently executing keyboard macro (a vector of events);
;;nil if none executing.

;;------------------------------
;;last-command-char

;;If the value of last-command-event is a keyboard event, then
;;this is the nearest ASCII equivalent to it.  This the the value that
;;self-insert-command will put in the buffer.  Remember that there is
;;NOT a 1:1 mapping between keyboard events and ASCII characters: the set
;;of keyboard events is much larger, so writing code that examines this
;;variable to determine what key has been typed is bad practice, unless
;;you are certain that it will be one of a small set of characters.

;;------------------------------
;;last-input-char

;;If the value of last-input-event is a keyboard event, then
;;this is the nearest ASCII equivalent to it.  Remember that there is
;;NOT a 1:1 mapping between keyboard events and ASCII characters: the set
;;of keyboard events is much larger, so writing code that examines this
;;variable to determine what key has been typed is bad practice, unless
;;you are certain that it will be one of a small set of characters.


;;; Code:

;; Make events of type eval, menu and timeout
;; execute properly.

(define-key global-map [menu] 'execute-eval-event)
(define-key global-map [timeout] 'execute-eval-event)
(define-key global-map [eval] 'execute-eval-event)

(defun execute-eval-event (event)
  (interactive "e")
  (funcall (nth 1 event) (nth 2 event)))

(put 'eval 'event-symbol-elements '(eval))
(put 'menu 'event-symbol-elements '(eval))
(put 'timeout 'event-symbol-elements '(eval))

(defsubst eventp (obj)
  "True if the argument is an event object."
  (or (integerp obj)
      (and (symbolp obj)
	   (get obj 'event-symbol-elements))
      (and (consp obj)
	   (symbolp (car obj))
	   (get (car obj) 'event-symbol-elements))))

(defun allocate-event ()
  "Returns an empty event structure.
In this emulation, it returns nil."
  nil)

(defun button-press-event-p (obj)
  "True if the argument is a mouse-button-press event object."
  (and (consp obj) (symbolp (car obj))
       (memq 'down (get (car obj) 'event-symbol-elements))))

(defun button-release-event-p (obj)
  "True if the argument is a mouse-button-release event object."
  (and (consp obj) (symbolp (car obj))
       (or (memq 'click (get (car obj) 'event-symbol-elements))
	   (memq 'drag (get (car obj) 'event-symbol-elements)))))

(defun character-to-event (ch &optional event)
  "Converts a numeric ASCII value to an event structure, replete with
bucky bits.  The character is the first argument, and the event to fill
in is the second.  This function contains knowledge about what the codes
mean -- for example, the number 9 is converted to the character Tab,
not the distinct character Control-I.

Beware that character-to-event and event-to-character are not strictly 
inverse functions, since events contain much more information than the 
ASCII character set can encode."
  ch)

(defun copy-event (event1 &optional event2)
  "Make a copy of the given event object.
In this emulation, `copy-event' just returns its argument."
  event1)

(defun deallocate-event (event)
  "Allow the given event structure to be reused.
In actual Lucid Emacs, you MUST NOT use this event object after
calling this function with it.  You will lose.  It is not necessary to
call this function, as event objects are garbage- collected like all
other objects; however, it may be more efficient to explicitly
deallocate events when you are sure that that is safe.

This emulation does not actually deallocate or reuse events
except via garbage collection and `cons'."
  nil)

(defun dispatch-event (event)
  "Given an event object returned by next-event, execute it."
  (let ((type (car-safe event)))
    (cond ((eq type 'eval)
	   (funcall (nth 1 event) (nth 2 event)))
	  ((eq type 'menu)
	   (funcall (nth 1 event) (nth 2 event)))
	  ((eq type 'switch-frame)
	   (internal-select-frame (nth 1 event)))
	  (t (error "keyboard and mouse events not allowed in `dispatch-event'")))))

(defun enqueue-eval-event: (function object)
  "Add an eval event to the back of the queue.
It will be the next event read after all pending events."
  (setq unread-command-events
	(nconc unread-command-events
	       (list (list 'eval function object)))))

(defun eval-event-p (obj)
  "True if the argument is an eval or menu event object."
  (eq (car-safe obj) 'eval))

(defun event-button (event)
  "Return the button-number of the given mouse-button-press event."
  (let ((sym (car (get (car event) 'event-symbol-elements))))
    (cdr (assq sym '((mouse-1 . 1) (mouse-2 . 2) (mouse-3 . 3)
		     (mouse-4 . 4) (mouse-5 . 5))))))

(defun event-function (event)
  "Return the callback function of the given timeout, menu, or eval event."
  (nth 1 event))

(defun event-key (event)
  "Returns the KeySym of the given key-press event.
The value is an ASCII printing character (not upper case) or a symbol."
  (if (symbolp event)
      (car (get event 'event-symbol-elements))
    (let ((base (logand event (1- (lsh 1 18)))))
      (downcase (if (< base 32) (logior base 64) base)))))

(defun event-modifiers (event)
  "Returns a list of symbols representing the modifier keys in event EVENT.
The elements of the list may include `meta', `control',
`shift', `hyper', `super', `alt'.
See also the function `event-modifier-bits'."
  (let ((type event))
    (if (listp type)
	(setq type (car type)))
    (if (symbolp type)
	(cdr (get type 'event-symbol-elements))
      (let ((list nil))
	(or (zerop (logand type (lsh 1 23)))
	    (setq list (cons 'meta list)))
	(or (and (zerop (logand type (lsh 1 22)))
		 (>= (logand type 127) 32))
	    (setq list (cons 'control list)))
	(or (and (zerop (logand type (lsh 1 21)))
		 (= (logand type 255) (downcase (logand type 255))))
	    (setq list (cons 'shift list)))
	(or (zerop (logand type (lsh 1 20)))
	    (setq list (cons 'hyper list)))
	(or (zerop (logand type (lsh 1 19)))
	    (setq list (cons 'super list)))
	(or (zerop (logand type (lsh 1 18)))
	    (setq list (cons 'alt list)))
	list))))

(defun event-modifier-bits (event)
  "Returns a number representing the modifier keys in event EVENT.
See also the function `event-modifiers'."
  (let ((type event))
    (if (listp type)
	(setq type (car type)))
    (if (symbolp type)
	(logand (lsh 63 18)
		(nth 1 (get type 'event-symbol-element-mask)))
      (let ((bits (logand type (lsh 63 18)))
	    (base (logand type 127)))
	;; Put in Control and Shift bits
	;; in the cases where the basic code expresses them.
	(if (< base 32)
	    (setq bits (logior (lsh 1 22) bits)))
	(if (/= base (downcase base))
	    (setq bits (logior (lsh 1 21) bits)))
	bits))))

(defun event-object (event)
  "Returns the function argument of the given timeout, menu, or eval event."
  (nth 2 event))

(defun event-point (event)
  "Returns the character position of the given mouse-related event.
If the event did not occur over a window, or did
not occur over text, then this returns nil.  Otherwise, it returns an index
into the buffer visible in the event's window."
  (posn-point (event-end event)))

(defun event-process (event)
  "Returns the process of the given process-output event."
  (nth 1 event))

(defun event-timestamp (event)
  "Returns the timestamp of the given event object.
In Lucid Emacs, this works for any kind of event.
In this emulation, it returns nil for non-mouse-related events."
  (and (listp event)
       (posn-timestamp (event-end event))))

(defun event-to-character (event &optional lenient)
  "Returns the closest ASCII approximation to the given event object.
If the event isn't a keypress, this returns nil.
If the second argument is non-nil, then this is lenient in its 
translation; it will ignore modifier keys other than control and meta,
and will ignore the shift modifier on those characters which have no 
shifted ASCII equivalent (Control-Shift-A for example, will be mapped to 
the same ASCII code as Control-A.)  If the second arg is nil, then nil 
will be returned for events which have no direct ASCII equivalent."
  (if (symbolp event)
      (and lenient
	   (cdr (assq event '((backspace . 8) (delete . 127) (tab . 9)
			      (return . 10) (enter . 10)))))
    ;; Our interpretation is, ASCII means anything a number can represent.
    (if (integerp event)
	event nil)))

(defun event-window (event)
  "Returns the window of the given mouse-related event object."
  (posn-window (event-end event)))

(defun event-x (event)
  "Returns the X position in characters of the given mouse-related event."
  (/ (car (posn-col-row (event-end event)))
     (character-width (window-frame (event-window event)))))

(defun event-x-pixel (event)
  "Returns the X position in pixels of the given mouse-related event."
  (car (posn-col-row (event-end event))))

(defun event-y (event)
  "Returns the Y position in characters of the given mouse-related event."
  (/ (cdr (posn-col-row (event-end event)))
     (character-width (window-frame (event-window event)))))

(defun event-y-pixel (event)
  "Returns the Y position in pixels of the given mouse-related event."
  (cdr (posn-col-row (event-end event))))

(defun key-press-event-p (obj)
  "True if the argument is a keyboard event object."
  (or (integerp obj)
      (and (symbolp obj)
	   (get obj 'event-symbol-elements))))

(defun menu-event-p (obj)
  "True if the argument is a menu event object."
  (eq (car-safe obj) 'menu))

(defun motion-event-p (obj)
  "True if the argument is a mouse-motion event object."
  (eq (car-safe obj) 'mouse-movement))

(defun next-command-event (event)
  "Given an event structure, fills it in with the next keyboard, mouse
press, or mouse release event available from the user.  If there are
non-command events available (mouse motion, sub-process output, etc) then
these will be executed (with dispatch-event) and discarded."
  (while (progn
	  (next-event event)
	  (not (or (key-press-event-p event)
		   (button-press-event-p event)
		   (button-release-event-p event)
		   (menu-event-p event))))
    (dispatch-event event)))

(defun next-event (event &optional ignore)
  "Given an event structure, fills it in with the next event available
from the window system or terminal driver.  Pass this object to
`dispatch-event' to handle it.

See also the function `next-command-event'.

If the second optional argument is non-nil, then this will never return
key-press and mouse-click events, but will delay them until later.  You
should probably never need to use this option; it is used for implementing
the `wait-reading-process-input' function."
  (read-event))

(defun process-event-p (obj)
  "True if the argument is a process-output event object.
GNU Emacs 19 does not currently generate process-output events."
  (eq (car-safe obj) 'process))

(defun timeout-event-p (obj)
  "True if the argument is a timeout event object.
GNU Emacs 19 does not currently generate timeout events."
  (eq (car-safe obj) 'timeout))

;;; levents.el ends here
