;;; mwheel.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: mouse

;; This file is part of Emacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

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
;; (autoload 'mwheel-install "mwheel" "Enable mouse wheel support.")
;; (mwheel-install)

;;; Code:

(require 'custom)
(require 'cl)

(defcustom mwheel-scroll-amount '(5 . 1)
  "Amount to scroll windows by when spinning the mouse wheel.
This is actually a cons cell, where the first item is the amount to scroll
on a normal wheel event, and the second is the amount to scroll when the
wheel is moved with the shift key depressed.

Each item should be the number of lines to scroll, or `nil' for near
full screen.
A near full screen is `next-screen-context-lines' less than a full screen."
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "Shifted"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))))

(defcustom mwheel-follow-mouse nil
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
  (interactive "e")
  (let ((curwin (if mwheel-follow-mouse
		    (prog1
			(selected-window)
		      (select-window (mwheel-event-window event)))))
	(amt (if (memq 'shift (event-modifiers event))
		 (cdr mwheel-scroll-amount)
	       (car mwheel-scroll-amount))))
    (unwind-protect
	(case (mwheel-event-button event)
	  (4 (scroll-down amt))
	  (5 (scroll-up amt))
	  (otherwise (error "Bad binding in mwheel-scroll")))
      (if curwin (select-window curwin)))))

;;;###autoload
(defun mwheel-install ()
  "Enable mouse wheel support."
  ;; In the latest versions of XEmacs, we could just use
  ;; (S-)*mouse-[45], since those are aliases for the button
  ;; equivalents in XEmacs, but I want this to work in as many
  ;; versions of XEmacs as it can.
  (let* ((mwheel-running-xemacs (string-match "XEmacs" (emacs-version)))
	 (keys (if mwheel-running-xemacs
		   '(button4 [(shift button4)] button5 [(shift button5)])
		 '([mouse-4] [S-mouse-4] [mouse-5] [S-mouse-5]))))
    ;; This condition-case is here because Emacs 19 will throw an error
    ;; if you try to define a key that it does not know about.  I for one
    ;; prefer to just unconditionally do a mwheel-install in my .emacs, so
    ;; that if the wheeled-mouse is there, it just works, and this way it
    ;; doesn't yell at me if I'm on my laptop or another machine, etc.
    (condition-case ()
	(while keys
	  (define-key global-map (car keys) 'mwheel-scroll)
	  (setq keys (cdr keys)))
      (error nil))))
    
(provide 'mwheel)

;;; mwheel.el ends here
