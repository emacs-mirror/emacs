;;; mwheel.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, 2000, 2001, Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: mouse

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

;; This code will enable the use of the infamous 'wheel' on the new
;; crop of mice.  Under XFree86 and the XSuSE X Servers, the wheel
;; events are sent as button4/button5 events.

;; I for one would prefer some way of converting the button4/button5
;; events into different event types, like 'mwheel-up' or
;; 'mwheel-down', but I cannot find a way to do this very easily (or
;; portably), so for now I just live with it.

;; To enable this code, simply put this at the top of your .emacs
;; file:
;;
;; (mwheel-install)

;;; Code:

(require 'custom)

;; Setter function for mouse-button user-options.  Switch Mouse Wheel
;; mode off and on again so that the old button is unbound and
;; new button is bound to mwheel-scroll.

(defun mouse-wheel-change-button (var button)
  (set-default var button)
  (when mouse-wheel-mode
    (mouse-wheel-mode 0)
    (mouse-wheel-mode 1)))

(defcustom mouse-wheel-down-button 4
  "Mouse button number for scrolling down."
  :group 'mouse
  :type 'integer
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-up-button 5
  "Mouse button number for scrolling up."
  :group 'mouse
  :type 'integer
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-scroll-amount '(1 5 nil)
  "Amount to scroll windows by when spinning the mouse wheel.
This is actually a list, where the first element is the amount to
scroll slowly (normally invoked with the Shift key depressed) the
second is the amount to scroll on a normal wheel event, and the third
is the amount to scroll fast (normally with the Control key depressed).

Each item should be the number of lines to scroll, or `nil' for near
full screen.
A near full screen is `next-screen-context-lines' less than a full screen."
  :group 'mouse
  :type '(list
	  (choice :tag "Slow (Shift key)"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "Normal (no keys)"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "Fast (Ctrl key)"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))))

(defcustom mouse-wheel-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

(defun mouse-wheel-event-window ()
  "Return the window associated with this mouse command."
  ;; If the command was a mouse event, the window is stored in the event.
  (if (listp last-command-event)
      (if (fboundp 'event-window)
	  (event-window last-command-event)
	(posn-window (event-start last-command-event)))
    ;; If not a mouse event, use the window the mouse is over now.
    (let* ((coordinates (mouse-position))
	   (x (car (cdr coordinates)))
	   (y (cdr (cdr coordinates))))
      (and (numberp x)
	   (numberp y)
	   (window-at x y (car coordinates))))))

;; Interpret mouse-wheel-scroll-amount
;; If the scroll-amount is a cons cell instead of a list,
;; then the car is the normal speed, the cdr is the slow
;; speed, and the fast speed is nil.  This is for pre-21.1
;; backward compatibility.
(defun mouse-wheel-amount (speed)
  (cond ((not (consp mouse-wheel-scroll-amount))
	 ;; illegal value
	 mouse-wheel-scroll-amount)
	((not (consp (cdr mouse-wheel-scroll-amount)))
	 ;; old-style value: a cons
	 (cond ((eq speed 'normal)
		(car mouse-wheel-scroll-amount))
	       ((eq speed 'slow)
		(cdr mouse-wheel-scroll-amount))
	       (t
		nil)))
	(t
	 (cond ((eq speed 'slow)
		(nth 0 mouse-wheel-scroll-amount))
	       ((eq speed 'normal)
		(nth 1 mouse-wheel-scroll-amount))
	       (t			;fast
		(nth 2 mouse-wheel-scroll-amount))))))

(defun mouse-wheel-scroll-internal (direction speed)
  "Scroll DIRECTION (up or down) SPEED (slow, normal, or fast).
`mouse-wheel-scroll-amount' defines the speeds."
  (let* ((scrollwin (if mouse-wheel-follow-mouse
			(mouse-wheel-event-window)))
	 (curwin (if scrollwin
		     (selected-window)))
	 (amt (mouse-wheel-amount speed)))
    (unwind-protect
	(progn
	  (if scrollwin (select-window scrollwin))
	  (if (eq direction 'down)
	      (scroll-down amt)
	    (scroll-up amt)))
      (if curwin (select-window curwin)))))


(defun mouse-wheel-scroll-up-fast ()
  "Scroll text of current window upward a full screen.
`mouse-wheel-follow-mouse' controls how the current window is determined.
`mouse-wheel-scroll-amount' controls the amount of scroll."
  (interactive)
  (mouse-wheel-scroll-internal 'up 'fast))

(defun mouse-wheel-scroll-down-fast ()
  "Scroll text of current window down a full screen.
`mouse-wheel-follow-mouse' controls how the current window is determined.
`mouse-wheel-scroll-amount' controls the amount of scroll."
  (interactive)
  (mouse-wheel-scroll-internal 'down 'fast))

(defun mouse-wheel-scroll-up-normal ()
  "Scroll text of current window upward a few lines.
`mouse-wheel-follow-mouse' controls how the current window is determined.
`mouse-wheel-scroll-amount' controls the amount of scroll."
  (interactive)
  (mouse-wheel-scroll-internal 'up 'normal))

(defun mouse-wheel-scroll-down-normal ()
  "Scroll text of current window down a few lines.
`mouse-wheel-follow-mouse' controls how the current window is determined.
`mouse-wheel-scroll-amount' controls the amount of scroll."
  (interactive)
  (mouse-wheel-scroll-internal 'down 'normal))

(defun mouse-wheel-scroll-up-slow ()
  "Scroll text of current window upward a line.
`mouse-wheel-follow-mouse' controls how the current window is determined.
`mouse-wheel-scroll-amount' controls the amount of scroll."
  (interactive)
  (mouse-wheel-scroll-internal 'up 'slow))

(defun mouse-wheel-scroll-down-slow ()
  "Scroll text of current window down a line.
`mouse-wheel-follow-mouse' controls how the current window is determined.
`mouse-wheel-scroll-amount' controls the amount of scroll."
  (interactive)
  (mouse-wheel-scroll-internal 'down 'slow))


;;; helper functions for minor mode mouse-wheel-mode.

(defun mouse-wheel-button-definer (button-pair down-function up-function)
  (mouse-wheel-key-definer button-pair 'dn down-function)
  (mouse-wheel-key-definer button-pair 'up up-function))

(defun mouse-wheel-key-definer (button-pair up-or-dn function)
  (let ((key (if (featurep 'xemacs)
		 (mouse-wheel-xemacs-key-formatter (car button-pair) up-or-dn)
	       (mouse-wheel-intern-vector (cdr button-pair) up-or-dn))))
    (cond (mouse-wheel-mode
	   (define-key global-map key function))
	  ((eq (lookup-key global-map key) 'function)
	   (define-key global-map key nil)))))

(defun mouse-wheel-xemacs-key-formatter (key-format-list up-or-dn)
  (cond ((listp key-format-list)		;e.g., (shift "button%d")
	 (list (car key-format-list)
	       (mouse-wheel-xemacs-intern  (car (cdr key-format-list)) up-or-dn)))
	(t
	 (mouse-wheel-xemacs-intern key-format-list up-or-dn))))

(defun mouse-wheel-xemacs-intern (key-format-string up-or-dn)
  (intern (format key-format-string
		  (if (eq up-or-dn 'up)
		      mouse-wheel-up-button
		    mouse-wheel-down-button))))

(defun mouse-wheel-intern-vector (key-format-string up-or-dn)
  "Turns \"mouse-%d\" into [mouse-4]."
  (vector (intern (format key-format-string
			  (if (eq up-or-dn 'up)
			      mouse-wheel-up-button
			    mouse-wheel-down-button)))))

;;; Note this definition must be at the end of the file, because
;;; `define-minor-mode' actually calls the mode-function if the
;;; associated variable is non-nil, which requires that all needed
;;; functions be already defined.
;;;###autoload
(define-minor-mode mouse-wheel-mode
  "Toggle mouse wheel support.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'mouse
  ;; This condition-case is here because Emacs 19 will throw an error
  ;; if you try to define a key that it does not know about.  I for one
  ;; prefer to just unconditionally do a mwheel-install in my .emacs, so
  ;; that if the wheeled-mouse is there, it just works, and this way it
  ;; doesn't yell at me if I'm on my laptop or another machine, etc.
  (condition-case ()
      (progn
	;; In the latest versions of XEmacs, we could just use
	;; (S-)*mouse-[45], since those are aliases for the button
	;; equivalents in XEmacs, but I want this to work in as many
	;; versions of XEmacs as it can.
	(mouse-wheel-button-definer '("button%d" .  "mouse-%d")
	 'mouse-wheel-scroll-down-normal 'mouse-wheel-scroll-up-normal)
	(mouse-wheel-button-definer '((shift "button%d") . "S-mouse-%d")
	 'mouse-wheel-scroll-down-slow 'mouse-wheel-scroll-up-slow)
	(mouse-wheel-button-definer '((control  "button%d") . "C-mouse-%d")
	 'mouse-wheel-scroll-down-fast 'mouse-wheel-scroll-up-fast))
    (error nil)))

;;; Compatibility entry point
;;;###autoload
(defun mwheel-install (&optional uninstall)
  "Enable mouse wheel support."
  (mouse-wheel-mode t))


(provide 'mwheel)

;;; mwheel.el ends here
