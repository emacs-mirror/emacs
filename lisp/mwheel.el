;;; mwheel.el --- Wheel mouse support

;; Copyright (C) 1998, 2000, 2001, 2002, 2002, 2004,
;;   2005 Free Software Foundation, Inc.
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;; (mouse-wheel-mode 1)

;;; Code:

(require 'custom)
(require 'timer)

;; Setter function for mouse-button user-options.  Switch Mouse Wheel
;; mode off and on again so that the old button is unbound and
;; new button is bound to mwheel-scroll.

(defun mouse-wheel-change-button (var button)
  (let ((active mouse-wheel-mode))
    ;; Deactivate before changing the setting.
    (when active (mouse-wheel-mode -1))
    (set-default var button)
    (when active (mouse-wheel-mode 1))))

(defvar mouse-wheel-down-button 4)
(make-obsolete-variable 'mouse-wheel-down-button
                        'mouse-wheel-down-event)
(defcustom mouse-wheel-down-event
  ;; In the latest versions of XEmacs, we could just use mouse-%s as well.
  (if (memq window-system '(w32 mac))
      'wheel-up
    (intern (format (if (featurep 'xemacs) "button%s" "mouse-%s")
		    mouse-wheel-down-button)))
  "Event used for scrolling down."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defvar mouse-wheel-up-button 5)
(make-obsolete-variable 'mouse-wheel-up-button
                        'mouse-wheel-up-event)
(defcustom mouse-wheel-up-event
  ;; In the latest versions of XEmacs, we could just use mouse-%s as well.
  (if (memq window-system '(w32 mac))
      'wheel-down
    (intern (format (if (featurep 'xemacs) "button%s" "mouse-%s")
		    mouse-wheel-up-button)))
  "Event used for scrolling down."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defvar mouse-wheel-click-button 2)
(make-obsolete-variable 'mouse-wheel-click-button
                        'mouse-wheel-click-event)
(defcustom mouse-wheel-click-event
  ;; In the latest versions of XEmacs, we could just use mouse-%s as well.
  (intern (format (if (featurep 'xemacs) "button%s" "mouse-%s")
		  mouse-wheel-click-button))
  "Event that should be temporarily inhibited after mouse scrolling.
The mouse wheel is typically on the mouse-2 button, so it may easily
happen that text is accidentially yanked into the buffer when
scrolling with the mouse wheel.  To prevent that, this variable can be
set to the event sent when clicking on the mouse wheel button."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-inhibit-click-time 0.35
  "Time in seconds to inhibit clicking on mouse wheel button after scroll."
  :group 'mouse
  :type 'number)

(defcustom mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control) . nil))
  "Amount to scroll windows by when spinning the mouse wheel.
This is an alist mapping the modifier key to the amount to scroll when
the wheel is moved with the modifier key depressed.
Elements of the list have the form (MODIFIERS . AMOUNT) or just AMOUNT if
MODIFIERS is nil.

AMOUNT should be the number of lines to scroll, or nil for near full
screen.  It can also be a floating point number, specifying the fraction of
a full screen to scroll.  A near full screen is `next-screen-context-lines'
less than a full screen."
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines")
		  (float :tag "Fraction of window")
		  (cons
		   (repeat (choice :tag "modifier"
				   (const alt) (const control) (const hyper)
				   (const meta) (const shift) (const super)))
		   (choice :tag "scroll amount"
			   (const :tag "Full screen" :value nil)
			   (integer :tag "Specific # of lines")
			   (float :tag "Fraction of window"))))
          (repeat
           (cons
            (repeat (choice :tag "modifier"
			    (const alt) (const control) (const hyper)
                            (const meta) (const shift) (const super)))
            (choice :tag "scroll amount"
                    (const :tag "Full screen" :value nil)
                    (integer :tag "Specific # of lines")
                    (float :tag "Fraction of window"))))))

(defcustom mouse-wheel-progressive-speed t
  "If non-nil, the faster the user moves the wheel, the faster the scrolling.
Note that this has no effect when `mouse-wheel-scroll-amount' specifies
a \"near full screen\" scroll or when the mouse wheel sends key instead
of button events."
  :group 'mouse
  :type 'boolean)

(defcustom mouse-wheel-follow-mouse t
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people prefer it."
  :group 'mouse
  :type 'boolean)

(if (not (fboundp 'event-button))
    (defun mwheel-event-button (event)
      (let ((x (event-basic-type event)))
	;; Map mouse-wheel events to appropriate buttons
	(if (eq 'mouse-wheel x)
	    (let ((amount (car (cdr (cdr (cdr event))))))
	      (if (< amount 0)
		  mouse-wheel-up-event
		mouse-wheel-down-event))
	  x)))
  (fset 'mwheel-event-button 'event-button))

(if (not (fboundp 'event-window))
    (defun mwheel-event-window (event)
      (posn-window (event-start event)))
  (fset 'mwheel-event-window 'event-window))

(defvar mwheel-inhibit-click-event-timer nil
  "Timer running while mouse wheel click event is inhibited.")

(defun mwheel-inhibit-click-timeout ()
  "Handler for `mwheel-inhibit-click-event-timer'."
  (setq mwheel-inhibit-click-event-timer nil)
  (remove-hook 'pre-command-hook 'mwheel-filter-click-events))

(defun mwheel-filter-click-events ()
  "Discard `mouse-wheel-click-event' while scrolling the mouse."
  (if (eq (event-basic-type last-input-event) mouse-wheel-click-event)
      (setq this-command 'ignore)))

(defun mwheel-scroll (event)
  "Scroll up or down according to the EVENT.
This should only be bound to mouse buttons 4 and 5."
  (interactive (list last-input-event))
  (let* ((curwin (if mouse-wheel-follow-mouse
                     (prog1
                         (selected-window)
                       (select-window (mwheel-event-window event)))))
         (mods
	  (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
         (amt (assoc mods mouse-wheel-scroll-amount)))
    ;; Extract the actual amount or find the element that has no modifiers.
    (if amt (setq amt (cdr amt))
      (let ((list-elt mouse-wheel-scroll-amount))
	(while (consp (setq amt (pop list-elt))))))
    (if (floatp amt) (setq amt (1+ (truncate (* amt (window-height))))))
    (when (and mouse-wheel-progressive-speed (numberp amt))
      ;; When the double-mouse-N comes in, a mouse-N has been executed already,
      ;; So by adding things up we get a squaring up (1, 3, 6, 10, 15, ...).
      (setq amt (* amt (event-click-count event))))
    (unwind-protect
	(let ((button (mwheel-event-button event)))
	  (cond ((eq button mouse-wheel-down-event) (scroll-down amt))
		((eq button mouse-wheel-up-event) (scroll-up amt))
		(t (error "Bad binding in mwheel-scroll"))))
      (if curwin (select-window curwin))))
  (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
    (if mwheel-inhibit-click-event-timer
	(cancel-timer mwheel-inhibit-click-event-timer)
      (add-hook 'pre-command-hook 'mwheel-filter-click-events))
    (setq mwheel-inhibit-click-event-timer
	  (run-with-timer mouse-wheel-inhibit-click-time nil
			  'mwheel-inhibit-click-timeout))))

;;;###autoload
(define-minor-mode mouse-wheel-mode
  "Toggle mouse wheel support.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'mouse
  (let* ((dn mouse-wheel-down-event)
         (up mouse-wheel-up-event)
         (keys
          (nconc (mapcar (lambda (amt) `[(,@(if (consp amt) (car amt)) ,up)])
			 mouse-wheel-scroll-amount)
                 (mapcar (lambda (amt) `[(,@(if (consp amt) (car amt)) ,dn)])
			 mouse-wheel-scroll-amount))))
    ;; This condition-case is here because Emacs 19 will throw an error
    ;; if you try to define a key that it does not know about.  I for one
    ;; prefer to just unconditionally do a mwheel-install in my .emacs, so
    ;; that if the wheeled-mouse is there, it just works, and this way it
    ;; doesn't yell at me if I'm on my laptop or another machine, etc.
    (condition-case ()
	(dolist (key keys)
	  (cond (mouse-wheel-mode
		 (global-set-key key 'mwheel-scroll))
		((eq (lookup-key (current-global-map) key) 'mwheel-scroll)
		 (global-unset-key key))))
      (error nil))))

;;; Compatibility entry point
;;;###autoload
(defun mwheel-install (&optional uninstall)
  "Enable mouse wheel support."
  (mouse-wheel-mode (if uninstall -1 1)))

(provide 'mwheel)

;; arch-tag: 50ed00e7-3686-4b7a-8037-fb31aa5c237f
;;; mwheel.el ends here
