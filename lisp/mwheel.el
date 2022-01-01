;;; mwheel.el --- Mouse wheel support  -*- lexical-binding:t -*-

;; Copyright (C) 1998, 2000-2022 Free Software Foundation, Inc.
;; Keywords: mouse
;; Package: emacs

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

;; This enables the use of the mouse wheel (or scroll wheel) in Emacs.
;; Under X11/X.Org, the wheel events are sent as button4/button5
;; events.

;; Mouse wheel support is already enabled by default on most graphical
;; displays.  You can toggle it using `M-x mouse-wheel-mode'.

;;; Code:

;; Implementation note:
;;
;; I for one would prefer some way of converting the button4/button5
;; events into different event types, like 'mwheel-up' or
;; 'mwheel-down', but I cannot find a way to do this very easily (or
;; portably), so for now I just live with it.

(require 'timer)

(defvar mouse-wheel-mode)
(defvar mouse-wheel--installed-bindings-alist nil
  "Alist of all installed mouse wheel key bindings.")

;; Setter function for mouse-button user-options.  Switch Mouse Wheel
;; mode off and on again so that the old button is unbound and
;; new button is bound to mwheel-scroll.

(defun mouse-wheel-change-button (var button)
  (set-default var button)
  ;; Sync the bindings if they're already setup.
  (when (and mouse-wheel--installed-bindings-alist
             (bound-and-true-p mouse-wheel-mode))
    (mouse-wheel-mode 1)))

(defcustom mouse-wheel-down-event
  (if (or (featurep 'w32-win) (featurep 'ns-win))
      'wheel-up
    'mouse-4)
  "Event used for scrolling down."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-up-event
  (if (or (featurep 'w32-win) (featurep 'ns-win))
      'wheel-down
    'mouse-5)
  "Event used for scrolling up."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-click-event 'mouse-2
  "Event that should be temporarily inhibited after mouse scrolling.
The mouse wheel is typically on the mouse-2 button, so it may easily
happen that text is accidentally yanked into the buffer when
scrolling with the mouse wheel.  To prevent that, this variable can be
set to the event sent when clicking on the mouse wheel button."
  :group 'mouse
  :type 'symbol
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-inhibit-click-time 0.35
  "Time in seconds to inhibit clicking on mouse wheel button after scroll."
  :group 'mouse
  :type 'number)

(defcustom mouse-wheel-scroll-amount
  '(1 ((shift) . hscroll) ((meta) . nil) ((control) . text-scale))
  "Amount to scroll windows by when spinning the mouse wheel.
This is an alist mapping the modifier key to the amount to scroll when
the wheel is moved with the modifier key depressed.
Elements of the list have the form (MODIFIER . AMOUNT) or just AMOUNT if
MODIFIER is nil.

AMOUNT should be the number of lines to scroll, or nil for near full
screen.  It can also be a floating point number, specifying the fraction of
a full screen to scroll.  A near full screen is `next-screen-context-lines'
less than a full screen.

If AMOUNT is the symbol 'hscroll', this means that with MODIFIER,
the mouse wheel will scroll horizontally instead of vertically.

If AMOUNT is the symbol 'text-scale', this means that with
MODIFIER, the mouse wheel will change the face height instead of
scrolling."
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
		   (choice :tag "action"
			   (const :tag "Scroll full screen" :value nil)
			   (integer :tag "Scroll specific # of lines")
			   (float :tag "Scroll fraction of window"))))
          (repeat
           (cons
            (repeat (choice :tag "modifier"
			    (const alt) (const control) (const hyper)
                            (const meta) (const shift) (const super)))
            (choice :tag "action"
                    (const :tag "Scroll full screen" :value nil)
                    (integer :tag "Scroll specific # of lines")
                    (float :tag "Scroll fraction of window")
                    (const :tag "Scroll horizontally" :value hscroll)
                    (const :tag "Change face size" :value text-scale)))))
  :set 'mouse-wheel-change-button
  :version "28.1")

(defcustom mouse-wheel-progressive-speed t
  "If nil, scrolling speed is proportional to the wheel speed.
If non-nil, moving the wheel faster will make scrolling
progressively faster.

Note that this has no effect when `mouse-wheel-scroll-amount' specifies
a \"near full screen\" scroll or when the mouse wheel sends key instead
of button events."
  :group 'mouse
  :type 'boolean)

(defcustom mouse-wheel-follow-mouse t
  "Whether the mouse wheel should scroll the window that the mouse is over.
This affects both the commands for scrolling and changing the
face height."
  :group 'mouse
  :type 'boolean)

(defcustom mouse-wheel-scroll-amount-horizontal 1
  "Amount to scroll windows horizontally.
Its value can be changed dynamically by using a numeric prefix argument
before starting horizontal scrolling.
It has effect when `mouse-wheel-scroll-amount' binds the value `hscroll'
to one of modifiers (`Shift' by default)."
  :group 'mouse
  :type 'number
  :version "28.1")

;;; For tilt-scroll
;;;
(defcustom mouse-wheel-tilt-scroll nil
  "Enable horizontal scrolling by tilting mouse wheel or via touchpad.
Also see `mouse-wheel-flip-direction'."
  :group 'mouse
  :type 'boolean
  :version "26.1")

(defcustom mouse-wheel-flip-direction nil
  "Swap direction of `wheel-right' and `wheel-left'.
By default, `wheel-right' scrolls the text to the right,
and `wheel-left' scrolls in the other direction.
If this variable is non-nil, it inverts the direction of
horizontal scrolling by tilting the mouse wheel.
Also see `mouse-wheel-tilt-scroll'."
  :group 'mouse
  :type 'boolean
  :version "26.1")

(defun mwheel-event-button (event)
  (let ((x (event-basic-type event)))
    ;; Map mouse-wheel events to appropriate buttons
    (if (eq 'mouse-wheel x)
        (let ((amount (car (cdr (cdr (cdr event))))))
          (if (< amount 0)
              mouse-wheel-up-event
            mouse-wheel-down-event))
      x)))

(defun mwheel-event-window (event)
  (posn-window (event-start event)))

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

(defvar mwheel-scroll-up-function 'scroll-up
  "Function that does the job of scrolling upward.")

(defvar mwheel-scroll-down-function 'scroll-down
  "Function that does the job of scrolling downward.")

(defvar mwheel-scroll-left-function 'scroll-left
  "Function that does the job of scrolling left.")

(defvar mwheel-scroll-right-function 'scroll-right
  "Function that does the job of scrolling right.")

(defvar mouse-wheel-left-event
  (if (or (featurep 'w32-win) (featurep 'ns-win))
      'wheel-left
    'mouse-6)
  "Event used for scrolling left.")

(defvar mouse-wheel-right-event
  (if (or (featurep 'w32-win) (featurep 'ns-win))
      'wheel-right
    'mouse-7)
  "Event used for scrolling right.")

(defun mouse-wheel--get-scroll-window (event)
  "Return window for mouse wheel event EVENT.
If `mouse-wheel-follow-mouse' is non-nil, return the window that
the mouse pointer is over.  Otherwise, return the currently
active window."
  (or (catch 'found
        (let* ((window (if mouse-wheel-follow-mouse
                           (mwheel-event-window event)
                         (selected-window)))
               (frame (when (window-live-p window)
                        (frame-parameter
                         (window-frame window) 'mouse-wheel-frame))))
          (when (frame-live-p frame)
            (let* ((pos (mouse-absolute-pixel-position))
                   (pos-x (car pos))
                   (pos-y (cdr pos)))
              (walk-window-tree
               (lambda (window-1)
                 (let ((edges (window-edges window-1 nil t t)))
                   (when (and (<= (nth 0 edges) pos-x)
                              (<= pos-x (nth 2 edges))
                              (<= (nth 1 edges) pos-y)
                              (<= pos-y (nth 3 edges)))
                     (throw 'found window-1))))
               frame nil t)))))
      (mwheel-event-window event)))

(defun mwheel-scroll (event &optional arg)
  "Scroll up or down according to the EVENT.
This should be bound only to mouse buttons 4, 5, 6, and 7 on
non-Windows systems.

Optional argument ARG (interactively, prefix numeric argument) controls
the step of horizontal scrolling.

The variable `mouse-wheel-scroll-amount-horizontal' records the last
value of ARG, and the command uses it in subsequent scrolls."
  (interactive (list last-input-event current-prefix-arg))
  (let* ((selected-window (selected-window))
         (scroll-window (mouse-wheel--get-scroll-window event))
	 (old-point
          (and (eq scroll-window selected-window)
	       (eq (car-safe transient-mark-mode) 'only)
	       (window-point)))
         (mods
	  (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
         (amt (assoc mods mouse-wheel-scroll-amount))
         saw-error)
    (unless (eq scroll-window selected-window)
      ;; Mark window to be scrolled for redisplay.
      (select-window scroll-window 'mark-for-redisplay))
    ;; Extract the actual amount or find the element that has no modifiers.
    (if amt (setq amt (cdr amt))
      (let ((list-elt mouse-wheel-scroll-amount))
	(while (consp (setq amt (pop list-elt))))))
    (if (floatp amt) (setq amt (1+ (truncate (* amt (window-height))))))
    (when (and mouse-wheel-progressive-speed (numberp amt))
      ;; When the double-mouse-N comes in, a mouse-N has been executed already,
      ;; So by adding things up we get a squaring up (1, 3, 6, 10, 15, ...).
      (setq amt (* amt (event-click-count event))))
    (when (numberp amt) (setq amt (* amt (event-line-count event))))
    (condition-case nil
        (unwind-protect
	    (let ((button (mwheel-event-button event)))
              (cond ((and (eq amt 'hscroll) (eq button mouse-wheel-down-event))
                     (when (and (natnump arg) (> arg 0))
                       (setq mouse-wheel-scroll-amount-horizontal arg))
                     (funcall (if mouse-wheel-flip-direction
                                  mwheel-scroll-left-function
                                mwheel-scroll-right-function)
                              mouse-wheel-scroll-amount-horizontal))
                    ((eq button mouse-wheel-down-event)
                     (condition-case nil (funcall mwheel-scroll-down-function amt)
                       ;; Make sure we do indeed scroll to the beginning of
                       ;; the buffer.
                       (beginning-of-buffer
                        (unwind-protect
                            (funcall mwheel-scroll-down-function)
                          ;; If the first scroll succeeded, then some scrolling
                          ;; is possible: keep scrolling til the beginning but
                          ;; do not signal an error.  For some reason, we have
                          ;; to do it even if the first scroll signaled an
                          ;; error, because otherwise the window is recentered
                          ;; for a reason that escapes me.  This problem seems
                          ;; to only affect scroll-down.  --Stef
                          (set-window-start (selected-window) (point-min))))))
                    ((and (eq amt 'hscroll) (eq button mouse-wheel-up-event))
                     (when (and (natnump arg) (> arg 0))
                       (setq mouse-wheel-scroll-amount-horizontal arg))
                     (funcall (if mouse-wheel-flip-direction
                                  mwheel-scroll-right-function
                                mwheel-scroll-left-function)
                              mouse-wheel-scroll-amount-horizontal))
                    ((eq button mouse-wheel-up-event)
                     (condition-case nil (funcall mwheel-scroll-up-function amt)
                       ;; Make sure we do indeed scroll to the end of the buffer.
                       (end-of-buffer (while t (funcall mwheel-scroll-up-function)))))
                    ((eq button mouse-wheel-left-event) ; for tilt scroll
                     (when mouse-wheel-tilt-scroll
                       (funcall (if mouse-wheel-flip-direction
                                    mwheel-scroll-right-function
                                  mwheel-scroll-left-function) amt)))
                    ((eq button mouse-wheel-right-event) ; for tilt scroll
                     (when mouse-wheel-tilt-scroll
                       (funcall (if mouse-wheel-flip-direction
                                    mwheel-scroll-left-function
                                  mwheel-scroll-right-function) amt)))
		    (t (error "Bad binding in mwheel-scroll"))))
          (if (eq scroll-window selected-window)
              ;; If there is a temporarily active region, deactivate it if
              ;; scrolling moved point.
	      (when (and old-point (/= old-point (window-point)))
                ;; Call `deactivate-mark' at the original position, so that
                ;; the original region is saved to the X selection.
	        (let ((new-point (window-point)))
	          (goto-char old-point)
	          (deactivate-mark)
	          (goto-char new-point)))
	    (select-window selected-window t)))
      ;; Do not ding at buffer limits.  Show a message instead.
      (beginning-of-buffer
       (message (error-message-string '(beginning-of-buffer)))
       (setq saw-error t))
      (end-of-buffer
       (message (error-message-string '(end-of-buffer)))
       (setq saw-error t)))

    (when (and (not saw-error)
               mouse-wheel-click-event mouse-wheel-inhibit-click-time)
      (if mwheel-inhibit-click-event-timer
          (cancel-timer mwheel-inhibit-click-event-timer)
        (add-hook 'pre-command-hook 'mwheel-filter-click-events))
      (setq mwheel-inhibit-click-event-timer
            (run-with-timer mouse-wheel-inhibit-click-time nil
                            'mwheel-inhibit-click-timeout)))))

(put 'mwheel-scroll 'scroll-command t)

(defun mouse-wheel-text-scale (event)
  "Increase or decrease the height of the default face according to the EVENT."
  (interactive (list last-input-event))
  (let ((selected-window (selected-window))
        (scroll-window (mouse-wheel--get-scroll-window event))
        (button (mwheel-event-button event)))
    (select-window scroll-window 'mark-for-redisplay)
    (unwind-protect
        (cond ((eq button mouse-wheel-down-event)
               (text-scale-increase 1))
              ((eq button mouse-wheel-up-event)
               (text-scale-decrease 1)))
      (select-window selected-window))))

(defun mouse-wheel--add-binding (key fun)
  "Bind mouse wheel button KEY to function FUN.
Save it for later removal by `mouse-wheel--remove-bindings'."
  (global-set-key key fun)
  (push (cons key fun) mouse-wheel--installed-bindings-alist))

(defun mouse-wheel--remove-bindings ()
  "Remove all mouse wheel key bindings.
This is a helper function for `mouse-wheel-mode'."
  (dolist (binding mouse-wheel--installed-bindings-alist)
    (let ((key (car binding))
          (fun (cdr binding)))
     (when (eq (lookup-key (current-global-map) key) fun)
       (global-unset-key key))))
  (setq mouse-wheel--installed-bindings-alist nil))

(defun mouse-wheel--create-scroll-keys (binding event)
  "Return list of key vectors for BINDING and EVENT.
BINDING is an element in `mouse-wheel-scroll-amount'.  EVENT is
an event used for scrolling, such as `mouse-wheel-down-event'."
  (let ((prefixes (list 'left-margin 'right-margin
                        'left-fringe 'right-fringe
                        'vertical-scroll-bar 'horizontal-scroll-bar
                        'mode-line 'header-line)))
    (if (consp binding)
        ;; With modifiers, bind only the buffer area (no prefix).
        (list `[(,@(car binding) ,event)])
      ;; No modifier: bind also some non-buffer areas of the screen.
      (cons (vector event)
            (mapcar (lambda (prefix) (vector prefix event)) prefixes)))))

;;;###autoload
(define-minor-mode mouse-wheel-mode
  "Toggle mouse wheel support (Mouse Wheel mode)."
  :init-value t
  :global t
  :group 'mouse
  ;; Remove previous bindings, if any.
  (mouse-wheel--remove-bindings)
  ;; Setup bindings as needed.
  (when mouse-wheel-mode
    (mouse-wheel--setup-bindings)))

(defun mouse-wheel--setup-bindings ()
  (dolist (binding mouse-wheel-scroll-amount)
    (cond
     ;; Bindings for changing font size.
     ((and (consp binding) (eq (cdr binding) 'text-scale))
      (dolist (event (list mouse-wheel-down-event mouse-wheel-up-event))
        (mouse-wheel--add-binding `[,(list (caar binding) event)]
                                  'mouse-wheel-text-scale)))
     ;; Bindings for scrolling.
     (t
      (dolist (event (list mouse-wheel-down-event mouse-wheel-up-event
                           mouse-wheel-left-event mouse-wheel-right-event))
        (dolist (key (mouse-wheel--create-scroll-keys binding event))
          (mouse-wheel--add-binding key 'mwheel-scroll)))))))

(when mouse-wheel-mode
  (mouse-wheel--setup-bindings))

;;; Obsolete.

;;; Compatibility entry point
;; preloaded ;;;###autoload
(defun mwheel-install (&optional uninstall)
  "Enable mouse wheel support."
  (declare (obsolete mouse-wheel-mode "27.1"))
  (mouse-wheel-mode (if uninstall -1 1)))

(defvar mwheel-installed-bindings nil)
(make-obsolete-variable 'mwheel-installed-bindings nil "28.1")

(defvar mwheel-installed-text-scale-bindings nil)
(make-obsolete-variable 'mwheel-installed-text-scale-bindings nil "28.1")

(provide 'mwheel)

;;; mwheel.el ends here
