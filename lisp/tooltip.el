;;; tooltip.el --- Show tooltip windows

;; Copyright (C) 1997, 1999, 2000, 2001 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'comint)
  (require 'gud))

(provide 'tooltip)


;;; Customizable settings

(defgroup tooltip nil
  "Customization group for the `tooltip' package."
  :group 'help
  :group 'gud
  :group 'mouse
  :group 'tools
  :version "21.1"
  :tag "Tool Tips")

(defvar tooltip-mode)

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
  "Specify an X offset, in pixels, for the display of tooltips.
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
  "Specify a Y offset, in pixels, for the display of tooltips.
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
     (:background "lightyellow" :foreground "black"))
    (t ()))
  "Face for tooltips."
  :group 'tooltip)


(defcustom tooltip-gud-tips-p nil
  "*Non-nil means show tooltips in GUD sessions."
  :type 'boolean
  :tag "GUD"
  :set #'(lambda (symbol on)
	   (setq tooltip-gud-tips-p on)
	   (if on (tooltip-gud-tips-setup)))
  :group 'tooltip)


(defcustom tooltip-gud-modes '(gud-mode c-mode c++-mode)
  "List of modes for which to enable GUD tips."
  :type 'sexp
  :tag "GUD modes"
  :group 'tooltip)


(defcustom tooltip-gud-display
  '((eq (tooltip-event-buffer tooltip-gud-event)
	(marker-buffer overlay-arrow-position)))
  "List of forms determining where GUD tooltips are displayed.

Forms in the list are combined with AND.  The default is to display
only tooltips in the buffer containing the overlay arrow."
  :type 'sexp
  :tag "GUD buffers predicate"
  :group 'tooltip)


(defcustom tooltip-use-echo-area nil
  "Use the echo area instead of tooltip frames.
This is only relevant GUD display, since otherwise it is equivalent to
turning off Tooltip mode."
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


(defvar tooltip-gud-debugger nil
  "The debugger for which we show tooltips.")



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
(defun tooltip-mode (&optional arg)
  "Mode for tooltip display.
With ARG, turn tooltip mode on if and only if ARG is positive."
  (interactive "P")
  (unless (fboundp 'x-show-tip)
    (error "Sorry, tooltips are not yet available on this system"))
  (let* ((on (if arg
		 (> (prefix-numeric-value arg) 0)
	       (not tooltip-mode)))
	 (hook-fn (if on 'add-hook 'remove-hook)))
    (setq tooltip-mode on)
    (funcall hook-fn 'change-major-mode-hook 'tooltip-change-major-mode)
    (tooltip-activate-mouse-motions-if-enabled)
    (funcall hook-fn 'pre-command-hook 'tooltip-hide)
    (funcall hook-fn 'tooltip-hook 'tooltip-gud-tips)
    (funcall hook-fn 'tooltip-hook 'tooltip-help-tips)
    (setq show-help-function (if on 'tooltip-show-help-function nil))
    ;; `ignore' is the default binding for mouse movements.
    (define-key global-map [mouse-movement]
      (if on 'tooltip-mouse-motion 'ignore))
    (tooltip-gud-tips-setup)))

(defun tooltip-gud-tips-setup ()
  "Setup debugger mode-hooks for tooltips."
  (when (and tooltip-mode tooltip-gud-tips-p)
    (global-set-key [S-mouse-3] 'tooltip-gud-toggle-dereference)
    (add-hook 'gdb-mode-hook
	      #'(lambda () (setq tooltip-gud-debugger 'gdb)))
    (add-hook 'sdb-mode-hook
	      #'(lambda () (setq tooltip-gud-debugger 'sdb)))
    (add-hook 'dbx-mode-hook
	      #'(lambda () (setq tooltip-gud-debugger 'dbx)))
    (add-hook 'xdb-mode-hook
	      #'(lambda () (setq tooltip-gud-debugger 'xdb)))
    (add-hook 'perldb-mode-hook
	      #'(lambda () (setq tooltip-gud-debugger 'perldb)))))

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
  "Add a one-shot timeout to call function tooltip-timeout."
  (setq tooltip-timeout-id
	(add-timeout (tooltip-delay) 'tooltip-timeout nil)))


(defun tooltip-timeout (object)
  "Function called when timer with id tooltip-timeout-id fires."
  (run-hook-with-args-until-success 'tooltip-hook
				    tooltip-last-mouse-motion-event))



;;; Reacting on mouse movements

(defun tooltip-change-major-mode ()
  "Function added to `change-major-mode-hook' when tooltip mode is on."
  (add-hook 'post-command-hook 'tooltip-activate-mouse-motions-if-enabled))


(defun tooltip-activate-mouse-motions-if-enabled ()
  "Reconsider for all buffers whether mouse motion events are desired."
  (remove-hook 'post-command-hook 'tooltip-activate-mouse-motions-if-enabled)
  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(if (and tooltip-mode
		 tooltip-gud-tips-p
		 (memq major-mode tooltip-gud-modes))
	    (tooltip-activate-mouse-motions t)
	  (tooltip-activate-mouse-motions nil))
	(setq buffers (cdr buffers))))))


(defun tooltip-activate-mouse-motions (activatep)
  "Activate/deactivate mouse motion events for the current buffer.
ACTIVATEP non-nil means activate mouse motion events."
  (if activatep
      (progn
	(make-local-variable 'track-mouse)
	(setq track-mouse t))
    (kill-local-variable 'track-mouse)))


(defun tooltip-mouse-motion (event)
  "Command handler for mouse movement events in `global-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))



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


(defun tooltip-show (text)
  "Show a tooltip window displaying TEXT.

Text larger than `x-max-tooltip-size' (which see) is clipped.

If the alist in `tooltip-frame-parameters' includes `left' and `top'
parameters, they determine the x and y position where the tooltip
is displayed.  Otherwise, the tooltip pops at offsets specified by
`tooltip-x-offset' and `tooltip-y-offset' from the current mouse
position."
  (if tooltip-use-echo-area
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
The prompt is taken from the value of COMINT-PROMPT-REGEXP in the buffer
of PROCESS."
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



;;; Tips for `gud'

(defvar tooltip-gud-original-filter nil
  "Process filter to restore after GUD output has been received.")


(defvar tooltip-gud-dereference nil
  "Non-nil means print expressions with a `*' in front of them.
For C this would dereference a pointer expression.")


(defvar tooltip-gud-event nil
  "The mouse movement event that led to a tooltip display.
This event can be examined by forms in TOOLTIP-GUD-DISPLAY.")


(defvar tooltip-gud-debugger nil
  "A symbol describing the debugger running under GUD.")


(defun tooltip-gud-toggle-dereference ()
  "Toggle whether tooltips should show `* expr' or `expr'."
  (interactive)
  (setq tooltip-gud-dereference (not tooltip-gud-dereference))
  (when (interactive-p)
    (message "Dereferencing is now %s."
	     (if tooltip-gud-dereference "on" "off"))))


(defun tooltip-gud-process-output (process output)
  "Process debugger output and show it in a tooltip window."
  (set-process-filter process tooltip-gud-original-filter)
  (tooltip-show (tooltip-strip-prompt process output)))


(defun tooltip-gud-print-command (expr)
  "Return a suitable command to print the expression EXPR.
If TOOLTIP-GUD-DEREFERENCE is t, also prepend a `*' to EXPR."
  (when tooltip-gud-dereference
    (setq expr (concat "*" expr)))
  (case tooltip-gud-debugger
    ((gdb dbx) (concat "print " expr))
    (xdb (concat "p " expr))
    (sdb (concat expr "/"))
    (perldb expr)))


(defun tooltip-gud-tips (event)
  "Show tip for identifier or selection under the mouse.
The mouse must either point at an identifier or inside a selected
region for the tip window to be shown.  If tooltip-gud-dereference is t,
add a `*' in front of the printed expression.

This function must return nil if it doesn't handle EVENT."
  (let (gud-buffer process)
    (when (and (eventp event)
	       tooltip-gud-tips-p
	       (boundp 'gud-comint-buffer)
	       (setq gud-buffer gud-comint-buffer)
	       (setq process (get-buffer-process gud-buffer))
	       (posn-point (event-end event))
	       (progn (setq tooltip-gud-event event)
		      (eval (cons 'and tooltip-gud-display))))
      (let ((expr (tooltip-expr-to-print event)))
	(when expr
	  (let ((cmd (tooltip-gud-print-command expr)))
	    (unless (null cmd)	       ; CMD can be nil if unknown debugger
	      (setq tooltip-gud-original-filter (process-filter process))
	      (set-process-filter process 'tooltip-gud-process-output)
	      (gud-basic-call cmd)
	      expr)))))))


;;; Tooltip help.

(defvar tooltip-help-message nil
  "The last help message received via `tooltip-show-help-function'.")


(defun tooltip-show-help-function (msg)
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
the timer with ID `tooltip-timeout-id' fires.
Value is non-nil if this function handled the tip."
  (when (stringp tooltip-help-message)
    (tooltip-show tooltip-help-message)
    t))



;;; Do this after all functions have been defined that are called from
;;; `tooltip-mode'.  The actual default value of `tooltip-mode' is set
;;; in startup.el.

;;;###autoload
(defcustom tooltip-mode nil
  "Toggle tooltip-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `tooltip-mode'."
  :set (lambda (symbol value)
	 (tooltip-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :require 'tooltip
  :group 'tooltip)


;;; tooltip.el ends here
