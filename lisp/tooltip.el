;;; tooltip.el --- show tooltip windows

;; Copyright (C) 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;;        Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@acm.org>
;; Keywords: help c mouse tools

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

;;; Customizable settings

(defgroup tooltip nil
  "Customization group for the `tooltip' package."
  :group 'help
  :group 'gud
  :group 'mouse
  :group 'tools
  :version "21.1"
  :tag "Tool Tips")

(defcustom tooltip-delay 0.7
  "Seconds to wait before displaying a tooltip the first time."
  :tag "Delay"
  :type 'number
  :group 'tooltip)

(defcustom tooltip-short-delay 0.1
  "Seconds to wait between subsequent tooltips on different items."
  :tag "Short delay"
  :type 'number
  :group 'tooltip)

(defcustom tooltip-recent-seconds 1
  "Display tooltips if changing tip items within this many seconds.
Do so after `tooltip-short-delay'."
  :tag "Recent seconds"
  :type 'number
  :group 'tooltip)

(defcustom tooltip-hide-delay 10
  "Hide tooltips automatically after this many seconds."
  :tag "Hide delay"
  :type 'number
  :group 'tooltip)

(defcustom tooltip-x-offset nil
  "X offset, in pixels, for the display of tooltips.
The offset is relative to the position of the mouse.  It must
be chosen so that the tooltip window doesn't contain the mouse
when it pops up.  If the value is nil, the default offset is 5
pixels.

If `tooltip-frame-parameters' includes the `left' parameter,
the value of `tooltip-x-offset' is ignored."
  :tag "X offset"
  :type '(choice (const :tag "Default" nil)
		 (integer :tag "Offset" :value 1))
  :group 'tooltip)

(defcustom tooltip-y-offset nil
  "Y offset, in pixels, for the display of tooltips.
The offset is relative to the position of the mouse.  It must
be chosen so that the tooltip window doesn't contain the mouse
when it pops up.  If the value is nil, the default offset is -10
pixels.

If `tooltip-frame-parameters' includes the `top' parameter,
the value of `tooltip-y-offset' is ignored."
  :tag "Y offset"
  :type '(choice (const :tag "Default" nil)
		 (integer :tag "Offset" :value 1))
  :group 'tooltip)

(defcustom tooltip-frame-parameters
  '((name . "tooltip")
    (internal-border-width . 5)
    (border-width . 1))
  "Frame parameters used for tooltips.

If `left' or `top' parameters are included, they specify the absolute
position to pop up the tooltip."
  :type 'sexp
  :tag "Frame Parameters"
  :group 'tooltip)

(defface tooltip
  '((((class color))
     :background "lightyellow"
     :foreground "black"
     :inherit variable-pitch)
    (t
     :inherit variable-pitch))
  "Face for tooltips."
  :group 'tooltip)

(defcustom tooltip-use-echo-area nil
  "Use the echo area instead of tooltip frames for help and GUD tooltips."
  :type 'boolean
  :tag "Use echo area"
  :group 'tooltip)


;;; Variables that are not customizable.

(defvar tooltip-hook nil
  "Functions to call to display tooltips.
Each function is called with one argument EVENT which is a copy of
the last mouse movement event that occurred.")

(defvar tooltip-timeout-id nil
  "The id of the timeout started when Emacs becomes idle.")

(defvar tooltip-last-mouse-motion-event nil
  "A copy of the last mouse motion event seen.")

(defvar tooltip-hide-time nil
  "Time when the last tooltip was hidden.")

(defvar gud-tooltip-mode) ;; Prevent warning.

;;; Event accessors

(defun tooltip-event-buffer (event)
  "Return the buffer over which event EVENT occurred.
This might return nil if the event did not occur over a buffer."
  (let ((window (posn-window (event-end event))))
    (and window (window-buffer window))))

;;; Switching tooltips on/off

;; We don't set track-mouse globally because this is a big redisplay
;; problem in buffers having a pre-command-hook or such installed,
;; which does a set-buffer, like the summary buffer of Gnus.  Calling
;; set-buffer prevents redisplay optimizations, so every mouse motion
;; would be accompanied by a full redisplay.

;;;###autoload
(define-minor-mode tooltip-mode
  "Toggle Tooltip display.
With ARG, turn tooltip mode on if and only if ARG is positive."
  :global t
  ;; If you change the :init-value below, you also need to change the
  ;; corresponding code in startup.el.
  :init-value (not (or noninteractive
		       (and (boundp 'emacs-quick-startup) emacs-quick-startup)
		       (not (and (fboundp 'display-graphic-p)
				 (display-graphic-p)))
		       (not (fboundp 'x-show-tip))))
  :group 'tooltip
  (unless (or (null tooltip-mode) (fboundp 'x-show-tip))
    (error "Sorry, tooltips are not yet available on this system"))
  (if tooltip-mode
      (progn
	(add-hook 'pre-command-hook 'tooltip-hide)
	(add-hook 'tooltip-hook 'tooltip-help-tips))
    (unless (and (boundp 'gud-tooltip-mode) gud-tooltip-mode)
      (remove-hook 'pre-command-hook 'tooltip-hide))
    (remove-hook 'tooltip-hook 'tooltip-help-tips))
  (setq show-help-function
	(if tooltip-mode 'tooltip-show-help nil)))


;;; Timeout for tooltip display

(defun tooltip-delay ()
  "Return the delay in seconds for the next tooltip."
  (let ((delay tooltip-delay)
	(now (float-time)))
    (when (and tooltip-hide-time
	       (< (- now tooltip-hide-time) tooltip-recent-seconds))
      (setq delay tooltip-short-delay))
    delay))

(defun tooltip-cancel-delayed-tip ()
  "Disable the tooltip timeout."
  (when tooltip-timeout-id
    (disable-timeout tooltip-timeout-id)
    (setq tooltip-timeout-id nil)))

(defun tooltip-start-delayed-tip ()
  "Add a one-shot timeout to call function `tooltip-timeout'."
  (setq tooltip-timeout-id
	(add-timeout (tooltip-delay) 'tooltip-timeout nil)))

(defun tooltip-timeout (object)
  "Function called when timer with id `tooltip-timeout-id' fires."
  (run-hook-with-args-until-success 'tooltip-hook
				    tooltip-last-mouse-motion-event))


;;; Displaying tips

(defun tooltip-set-param (alist key value)
  "Change the value of KEY in alist ALIST to VALUE.
If there's no association for KEY in ALIST, add one, otherwise
change the existing association.  Value is the resulting alist."
  (let ((param (assq key alist)))
    (if (consp param)
	(setcdr param value)
      (push (cons key value) alist))
    alist))

(defun tooltip-show (text &optional use-echo-area)
  "Show a tooltip window displaying TEXT.

Text larger than `x-max-tooltip-size' is clipped.

If the alist in `tooltip-frame-parameters' includes `left' and `top'
parameters, they determine the x and y position where the tooltip
is displayed.  Otherwise, the tooltip pops at offsets specified by
`tooltip-x-offset' and `tooltip-y-offset' from the current mouse
position.

Optional second arg USE-ECHO-AREA non-nil means to show tooltip
in echo area."
  (if use-echo-area
      (message "%s" text)
    (condition-case error
	(let ((params (copy-sequence tooltip-frame-parameters))
	      (fg (face-attribute 'tooltip :foreground))
	      (bg (face-attribute 'tooltip :background)))
	  (when (stringp fg)
	    (setq params (tooltip-set-param params 'foreground-color fg))
	    (setq params (tooltip-set-param params 'border-color fg)))
	  (when (stringp bg)
	    (setq params (tooltip-set-param params 'background-color bg)))
	  (x-show-tip (propertize text 'face 'tooltip)
		      (selected-frame)
		      params
		      tooltip-hide-delay
		      tooltip-x-offset
		      tooltip-y-offset))
      (error
       (message "Error while displaying tooltip: %s" error)
       (sit-for 1)
       (message "%s" text)))))

(defun tooltip-hide (&optional ignored-arg)
  "Hide a tooltip, if one is displayed.
Value is non-nil if tooltip was open."
  (tooltip-cancel-delayed-tip)
  (when (x-hide-tip)
    (setq tooltip-hide-time (float-time))))


;;; Debugger-related functions

(defun tooltip-identifier-from-point (point)
  "Extract the identifier at POINT, if any.
Value is nil if no identifier exists at point.  Identifier extraction
is based on the current syntax table."
  (save-excursion
    (goto-char point)
    (let ((start (progn (skip-syntax-backward "w_") (point))))
      (unless (looking-at "[0-9]")
	(skip-syntax-forward "w_")
	(when (> (point) start)
	  (buffer-substring start (point)))))))

(defmacro tooltip-region-active-p ()
  "Value is non-nil if the region is currently active."
  (if (string-match "^GNU" (emacs-version))
      `(and transient-mark-mode mark-active)
    `(region-active-p)))

(defun tooltip-expr-to-print (event)
  "Return an expression that should be printed for EVENT.
If a region is active and the mouse is inside the region, print
the region.  Otherwise, figure out the identifier around the point
where the mouse is."
  (save-excursion
    (set-buffer (tooltip-event-buffer event))
    (let ((point (posn-point (event-end event))))
      (if (tooltip-region-active-p)
	  (when (and (<= (region-beginning) point) (<= point (region-end)))
	    (buffer-substring (region-beginning) (region-end)))
	(tooltip-identifier-from-point point)))))

(defun tooltip-process-prompt-regexp (process)
  "Return regexp matching the prompt of PROCESS at the end of a string.
The prompt is taken from the value of `comint-prompt-regexp' in
the buffer of PROCESS."
  (let ((prompt-regexp (save-excursion
			 (set-buffer (process-buffer process))
			 comint-prompt-regexp)))
    ;; Most start with `^' but the one for `sdb' cannot be easily
    ;; stripped.  Code the prompt for `sdb' fixed here.
    (if (= (aref prompt-regexp 0) ?^)
	(setq prompt-regexp (substring prompt-regexp 1))
      (setq prompt-regexp "\\*"))
    (concat "\n*" prompt-regexp "$")))

(defun tooltip-strip-prompt (process output)
  "Return OUTPUT with any prompt of PROCESS stripped from its end."
  (let ((prompt-regexp (tooltip-process-prompt-regexp process)))
    (save-match-data
      (when (string-match prompt-regexp output)
	(setq output (substring output 0 (match-beginning 0)))))
    output))


;;; Tooltip help.

(defvar tooltip-help-message nil
  "The last help message received via `tooltip-show-help'.")

(defun tooltip-show-help (msg)
  "Function installed as `show-help-function'.
MSG is either a help string to display, or nil to cancel the display."
  (let ((previous-help tooltip-help-message))
    (setq tooltip-help-message msg)
    (cond ((null msg)
	   ;; Cancel display.  This also cancels a delayed tip, if
	   ;; there is one.
	   (tooltip-hide))
	  ((equal previous-help msg)
	   ;; Same help as before (but possibly the mouse has moved).
	   ;; Keep what we have.
	   )
	  (t
	   ;; A different help.  Remove a previous tooltip, and
	   ;; display a new one, with some delay.
	   (tooltip-hide)
	   (tooltip-start-delayed-tip)))))

(defun tooltip-help-tips (event)
  "Hook function to display a help tooltip.
This is installed on the hook `tooltip-hook', which is run when
the timer with id `tooltip-timeout-id' fires.
Value is non-nil if this function handled the tip."
  (when (stringp tooltip-help-message)
    (tooltip-show tooltip-help-message tooltip-use-echo-area)
    t))

(provide 'tooltip)

;; arch-tag: 3d61135e-4618-4a78-af28-183f6df5636f
;;; tooltip.el ends here
