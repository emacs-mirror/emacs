;;; tooltip.el --- Show tooltip windows

;; Copyright (C) 1997, 1999 Free Software Foundation, Inc.

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

;; Put into your `.emacs'

;; (require 'tooltip)
;; (tooltip-mode 1)



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
  :group 'c
  :group 'mouse
  :group 'tools
  :version "21.1"
  :tag "Tool Tips")

(defvar tooltip-mode)

(defcustom tooltip-delay 1.0
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


(defcustom tooltip-frame-parameters
  '((name . "tooltip")
    (foreground-color . "black")
    (background-color . "lightyellow")
    (internal-border-width . 5)
    (border-color . "lightyellow")
    (border-width . 1))
  "Frame parameters used for tooltips."
  :type 'sexp
  :tag "Frame Parameters"
  :group 'tooltip)


(defcustom tooltip-gud-tips-p nil
  "Non-nil means show tooltips in GUD sessions."
  :type 'boolean
  :tag "GUD"
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
    (when (and on tooltip-gud-tips-p)
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
		#'(lambda () (setq tooltip-gud-debugger 'perldb))))))



;;; Timeout for tooltip display

(defun tooltip-float-time ()
  "Return the values of `current-time' as a float."
  (let ((now (current-time)))
    (+ (* 65536.0 (nth 0 now))
       (nth 1 now)
       (/ (nth 2 now) 1000000.0))))


(defun tooltip-delay ()
  "Return the delay in seconds for the next tooltip."
  (let ((delay tooltip-delay)
	(now (tooltip-float-time)))
    (when (and tooltip-hide-time
	       (< (- now tooltip-hide-time) tooltip-recent-seconds))
      (setq delay tooltip-short-delay))
    delay))


(defun tooltip-disable-timeout ()
  "Disable the tooltip timeout."
  (when tooltip-timeout-id
    (disable-timeout tooltip-timeout-id)
    (setq tooltip-timeout-id nil)))


(defun tooltip-add-timeout ()
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
    (tooltip-add-timeout)))



;;; Displaying tips

(defun tooltip-show (text)
  "Show a tooltip window at the current mouse position displaying TEXT."
  (x-show-tip text (selected-frame) tooltip-frame-parameters))


(defun tooltip-hide (&optional ignored-arg)
  "Hide a tooltip, if one is displayed.
Value is non-nil if tooltip was open."
  (tooltip-disable-timeout)
  (when (x-hide-tip)
    (setq tooltip-hide-time (tooltip-float-time))))



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
  "Toggle whether tooltips should show `* exor' or `expr'."
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
	  (setq tooltip-gud-original-filter (process-filter process))
	  (set-process-filter process 'tooltip-gud-process-output)
	  (process-send-string
	   process (concat (tooltip-gud-print-command expr) "\n"))
	  expr)))))



;;; Tooltip help.

(defvar tooltip-help-message nil
  "The last help message received via `tooltip-show-help-function'.")


(defun tooltip-show-help-function (msg)
  "Function installed as `show-help-function'.
MSG is either a help string to display, or nil to cancel the display."
  (let ((previous-help tooltip-help-message))
    (setq tooltip-help-message msg)
    (cond ((null msg)
	   (tooltip-hide))
	  ((or (not (stringp previous-help))
	       (not (string= msg previous-help)))
	   (tooltip-hide)
	   (tooltip-add-timeout))
	  (t
	   (tooltip-disable-timeout)
	   (tooltip-add-timeout)))))


(defun tooltip-help-tips (event)
  "Hook function to display a help tooltip.
Value is non-nil if this function handled the tip."
  (when (stringp tooltip-help-message)
    (tooltip-show tooltip-help-message)
    (setq tooltip-help-message nil)
    t))



;;; Do this after all functions have been defined that are called
;;; from `tooltip-mode'.


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
