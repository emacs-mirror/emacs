;;; mwheel.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, 2000, 2001 Free Software Foundation, Inc.
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

(defcustom mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control) . nil))
  "Amount to scroll windows by when spinning the mouse wheel.
This is actually a cons cell, where the first item is the amount to scroll
on a normal wheel event, and the rest is an alist mapping the modifier key
to the amount to scroll when the wheel is moved with the modifier key depressed.

Each item should be the number of lines to scroll, or `nil' for near
full screen.  It can also be a floating point number, specifying
the fraction of the window to scroll.
A near full screen is `next-screen-context-lines' less than a full screen."
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines")
		  (float :tag "Fraction of window"))
          (repeat
           (cons
            (repeat (choice :tag "modifier" (const alt) (const control) (const hyper)
                            (const meta) (const shift) (const super)))
            (choice :tag "scroll amount"
                    (const :tag "Full screen" :value nil)
                    (integer :tag "Specific # of lines")
                    (float :tag "Fraction of window"))))))

(defcustom mouse-wheel-progessive-speed t
  "If non-nil, the faster the user moves the wheel, the faster the scrolling.
Note that this has no effect when `mouse-wheel-scroll-amount' specifies
a \"near full screen\" scroll."
  :group 'mouse
  :type 'boolean)

(defcustom mouse-wheel-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

(if (not (fboundp 'event-button))
    (defun mwheel-event-button (event)
      (let ((x (symbol-name (event-basic-type event))))
	(if (not (string-match "^mouse-\\([0-9]+\\)" x))
	    (error "Not a button event: %S" event))
	(string-to-int (substring x (match-beginning 1) (match-end 1)))))
  (fset 'mwheel-event-button 'event-button))

(if (not (fboundp 'event-window))
    (defun mwheel-event-window (event)
      (posn-window (event-start event)))
  (fset 'mwheel-event-window 'event-window))

(defun mwheel-scroll (event)
  "Scroll up or down according to the EVENT.
This should only be bound to mouse buttons 4 and 5."
  (interactive "e")
  (let* ((curwin (if mouse-wheel-follow-mouse
                     (prog1
                         (selected-window)
                       (select-window (mwheel-event-window event)))))
         (mods
	  (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
         (amt
	  (or (and mods
		   (cdr (assoc mods (cdr mouse-wheel-scroll-amount))))
	      (car mouse-wheel-scroll-amount))))
    (if (floatp amt) (setq amt (1+ (truncate (* amt (window-height))))))
    (when (and mouse-wheel-progessive-speed (numberp amt))
      ;; When the double-mouse-N comes in, a mouse-N has been executed already,
      ;; So by adding things up we get a squaring up (1, 3, 6, 10, 16, ...).
      (setq amt (* amt (event-click-count event))))
    (unwind-protect
	(let ((button (mwheel-event-button event)))
	  (cond ((= button mouse-wheel-down-button) (scroll-down amt))
		((= button mouse-wheel-up-button) (scroll-up amt))
		(t (error "Bad binding in mwheel-scroll"))))
      (if curwin (select-window curwin)))))


;;;###autoload
(define-minor-mode mouse-wheel-mode
  "Toggle mouse wheel support.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'mouse
  ;; In the latest versions of XEmacs, we could just use
  ;; (S-)*mouse-[45], since those are aliases for the button
  ;; equivalents in XEmacs, but I want this to work in as many
  ;; versions of XEmacs as it can.
  (let* ((prefix (if (featurep 'xemacs) "button%d" "mouse-%d"))
         (dn (intern (format prefix mouse-wheel-down-button)))
         (up (intern (format prefix mouse-wheel-up-button)))
         (keys
          (nconc (list (vector dn) (vector up))
                 (mapcar (lambda (amt) `[(,@(car amt) ,up)])
                         (cdr mouse-wheel-scroll-amount))
                 (mapcar (lambda (amt) `[(,@(car amt) ,dn)])
                         (cdr mouse-wheel-scroll-amount)))))
    ;; This condition-case is here because Emacs 19 will throw an error
    ;; if you try to define a key that it does not know about.  I for one
    ;; prefer to just unconditionally do a mwheel-install in my .emacs, so
    ;; that if the wheeled-mouse is there, it just works, and this way it
    ;; doesn't yell at me if I'm on my laptop or another machine, etc.
    (condition-case ()
	(dolist (key keys)
	  (cond (mouse-wheel-mode
		 (define-key global-map key 'mwheel-scroll))
		((eq (lookup-key global-map key) 'mwheel-scroll)
		 (define-key global-map key nil))))
      (error nil))))

;;; Compatibility entry point
;;;###autoload
(defun mwheel-install (&optional uninstall)
  "Enable mouse wheel support."
  (mouse-wheel-mode t))

(provide 'mwheel)

;;; mwheel.el ends here
