;;; jit-lock.el --- just-in-time fontification.

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Keywords: faces files
;; Version: 1.0

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

;; Just-in-time fontification, triggered by C redisplay code.

;;; Code:


(require 'font-lock)

(eval-when-compile
  (defmacro with-buffer-unmodified (&rest body)
    "Eval BODY, preserving the current buffer's modified state."
    (let ((modified (make-symbol "modified")))
      `(let ((,modified (buffer-modified-p)))
	 ,@body
	 (unless ,modified)
	   (restore-buffer-modified-p nil))))
  
  (defmacro with-buffer-prepared-for-font-lock (&rest body)
    "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
    `(with-buffer-unmodified
      (let ((buffer-undo-list t)
	    (inhibit-read-only t)
	    (inhibit-point-motion-hooks t)
	    before-change-functions
	    after-change-functions
	    deactivate-mark
	    buffer-file-name
	    buffer-file-truename)
	,@body))))

  

;;; Customization.

(defcustom jit-lock-chunk-size 500
  "*Font-lock chunks of this many characters, or smaller."
  :type 'integer
  :group 'jit-lock)


(defcustom jit-lock-stealth-time 3
  "*Time in seconds to wait before beginning stealth fontification.
Stealth fontification occurs if there is no input within this time.
If nil, means stealth fontification is never performed.

The value of this variable is used when JIT Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))
  :group 'jit-lock)


(defcustom jit-lock-stealth-nice 0.125
  "*Time in seconds to pause between chunks of stealth fontification.
Each iteration of stealth fontification is separated by this amount of time,
thus reducing the demand that stealth fontification makes on the system.
If nil, means stealth fontification is never paused.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could increase the value of this variable.
See also `jit-lock-stealth-load'."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))	  
  :group 'jit-lock)
 

(defcustom jit-lock-stealth-load
  (if (condition-case nil (load-average) (error)) 200)
  "*Load in percentage above which stealth fontification is suspended.
Stealth fontification pauses when the system short-term load average (as
returned by the function `load-average' if supported) goes above this level,
thus reducing the demand that stealth fontification makes on the system.
If nil, means stealth fontification is never suspended.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable.
See also `jit-lock-stealth-nice'."
  :type (if (condition-case nil (load-average) (error))
	    '(choice (const :tag "never" nil)
		     (integer :tag "load"))
	  '(const :format "%t: unsupported\n" nil))
  :group 'jit-lock)


(defcustom jit-lock-stealth-verbose nil
  "*If non-nil, means stealth fontification should show status messages."
  :type 'boolean
  :group 'jit-lock)


(defcustom jit-lock-defer-contextually 'syntax-driven
  "*If non-nil, means deferred fontification should be syntactically true.
If nil, means deferred fontification occurs only on those lines modified.  This
means where modification on a line causes syntactic change on subsequent lines,
those subsequent lines are not refontified to reflect their new context.
If t, means deferred fontification occurs on those lines modified and all
subsequent lines.  This means those subsequent lines are refontified to reflect
their new syntactic context, either immediately or when scrolling into them.
If any other value, e.g., `syntax-driven', means deferred syntactically true
fontification occurs only if syntactic fontification is performed using the
buffer mode's syntax table, i.e., only if `font-lock-keywords-only' is nil.

The value of this variable is used when JIT Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (other :tag "syntax-driven" syntax-driven))
  :group 'jit-lock)



;;; Variables that are not customizable.

(defvar jit-lock-mode nil
  "Non-nil means Just-in-time Lock mode is active.")
(make-variable-buffer-local 'jit-lock-mode)


(defvar jit-lock-first-unfontify-pos nil
  "Consider text after this position as unfontified.")
(make-variable-buffer-local 'jit-lock-first-unfontify-pos)


(defvar jit-lock-stealth-timer nil
  "Timer for stealth fontification in Just-in-time Lock mode.")



;;; JIT lock mode

;;;###autoload
(defun jit-lock-mode (arg)
  "Toggle Just-in-time Lock mode.
With arg, turn Just-in-time Lock mode on if and only if arg is positive.
Enable it automatically by customizing group `font-lock'.

When Just-in-time Lock mode is enabled, fontification is different in the
following ways:

- Demand-driven buffer fontification triggered by Emacs C code.
  This means initial fontification of the whole buffer does not occur.
  Instead, fontification occurs when necessary, such as when scrolling
  through the buffer would otherwise reveal unfontified areas.  This is
  useful if buffer fontification is too slow for large buffers.

- Stealthy buffer fontification if `jit-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `jit-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

- Deferred context fontification if `jit-lock-defer-contextually' is
  non-nil.  This means fontification updates the buffer corresponding to
  true syntactic context, after `jit-lock-stealth-time' seconds of Emacs
  idle time, while Emacs remains idle.  Otherwise, fontification occurs
  on modified lines only, and subsequent lines can remain fontified
  corresponding to previous syntactic contexts.  This is useful where
  strings or comments span lines.

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `jit-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `jit-lock-stealth-nice' and `jit-lock-stealth-lines'."
  (interactive "P")
  (setq jit-lock-mode (if arg
			  (> (prefix-numeric-value arg) 0)
			(not jit-lock-mode)))
  (cond ((and jit-lock-mode
	      (or (not (boundp 'font-lock-mode))
		  (not font-lock-mode)))
	 ;; If font-lock is not on, turn it on, with Just-in-time
	 ;; Lock mode as support mode; font-lock will call us again.
	 (let ((font-lock-support-mode 'jit-lock-mode))
	   (font-lock-mode t)))

	;; Turn Just-in-time Lock mode on.
	(jit-lock-mode
	 ;; Setting `font-lock-fontified' makes font-lock believe the
	 ;; buffer is already fontified, so that it won't highlight
	 ;; the whole buffer.
	 (make-local-variable 'font-lock-fontified)
	 (setq font-lock-fontified t)

	 (setq jit-lock-first-unfontify-pos nil)
	 
	 ;; Install an idle timer for stealth fontification.
	 (when (and jit-lock-stealth-time
		    (null jit-lock-stealth-timer))
	   (setq jit-lock-stealth-timer 
		 (run-with-idle-timer jit-lock-stealth-time
				      jit-lock-stealth-time
				      'jit-lock-stealth-fontify)))

	 ;; Add a hook for deferred contectual fontification.
	 (when (or (eq jit-lock-defer-contextually 'always)
		   (and (not (eq jit-lock-defer-contextually 'never))
			(null font-lock-keywords-only)))
	   (add-hook 'after-change-functions 'jit-lock-after-change))
	 
	 ;; Install the fontification hook.
	 (add-hook 'fontification-functions 'jit-lock-function))

	;; Turn Just-in-time Lock mode off.
	(t
	 ;; Cancel our idle timer.
	 (when jit-lock-stealth-timer
	   (cancel-timer jit-lock-stealth-timer)
	   (setq jit-lock-stealth-timer nil))

	 ;; Remove hooks.
	 (remove-hook 'after-change-functions 'jit-lock-after-change)
	 (remove-hook 'fontification-functions 'jit-lock-function))))


;;;###autoload
(defun turn-on-jit-lock ()
  "Unconditionally turn on Just-in-time Lock mode."
  (jit-lock-mode 1))



;;; On demand fontification.

(defun jit-lock-function (start)
  "Fontify current buffer starting at position START.
This function is added to `fontification-functions' when `jit-lock-mode'
is active."
  (when jit-lock-mode
    (jit-lock-function-1 start)))
     
  
(defun jit-lock-function-1 (start)
  "Fontify current buffer starting at position START.
This function is added to `fontification-functions' when `jit-lock-mode'
is active."
  (with-buffer-prepared-for-font-lock
   (save-excursion
     (save-restriction
       (widen)
       (let ((end (min (point-max) (+ start jit-lock-chunk-size)))
	     (parse-sexp-lookup-properties font-lock-syntactic-keywords)
	     (font-lock-beginning-of-syntax-function nil)
	     (old-syntax-table (syntax-table))
	     next font-lock-start font-lock-end)
	 (when font-lock-syntax-table
	   (set-syntax-table font-lock-syntax-table))
	 (save-match-data
	   (condition-case error
	       ;; Fontify chunks beginning at START.  The end of a
	       ;; chunk is either `end', or the start of a region
	       ;; before `end' that has already been fontified.
	       (while start
		 ;; Determine the end of this chunk.
		 (setq next (or (text-property-any start end 'fontified t)
				end))

		 ;; Decide which range of text should be fontified.
		 ;; The problem is that START and NEXT may be in the
		 ;; middle of something matched by a font-lock regexp.
		 ;; Until someone has a better idea, let's start
		 ;; at the start of the line containing START and
		 ;; stop at the start of the line following NEXT.
		 (goto-char next)
		 (setq font-lock-end (line-beginning-position 2))
		 (goto-char start)
		 (setq font-lock-start (line-beginning-position))
		   
		 ;; Fontify the chunk, and mark it as fontified.
		 (font-lock-fontify-region font-lock-start font-lock-end nil)
		 (add-text-properties start next '(fontified t))
		   
		 ;; Find the start of the next chunk, if any.
		 (setq start (text-property-any next end 'fontified nil)))
	       
	     ((error quit)
	      (message "Fontifying region...%s" error))))
       
	 ;; Restore previous buffer settings.
	 (set-syntax-table old-syntax-table))))))


(defun jit-lock-after-fontify-buffer ()
  "Mark the current buffer as fontified.
Called from `font-lock-after-fontify-buffer."
  (with-buffer-prepared-for-font-lock
   (add-text-properties (point-min) (point-max) '(fontified t))))


(defun jit-lock-after-unfontify-buffer ()
  "Mark the current buffer as unfontified.
Called from `font-lock-after-fontify-buffer."
  (with-buffer-prepared-for-font-lock
   (remove-text-properties (point-min) (point-max) '(fontified nil))))



;;; Stealth fontification.

(defsubst jit-lock-stealth-chunk-start (around)
  "Return the start of the next chunk to fontify around position AROUND..
Value is nil if there is nothing more to fontify."
  (if (zerop (buffer-size))
      nil
    (save-restriction
      (widen)
      (let* ((next (text-property-any around (point-max) 'fontified nil))
	     (prev (previous-single-property-change around 'fontified))
	     (prop (get-text-property (max (point-min) (1- around))
				      'fontified))
	     (start (cond
		     ((null prev)
		      ;; There is no property change between AROUND
		      ;; and the start of the buffer.  If PROP is
		      ;; non-nil, everything in front of AROUND is
		      ;; fontified, otherwise nothing is fontified.
		      (if prop
			  nil
			(max (point-min)
			     (- around (/ jit-lock-chunk-size 2)))))
		     (prop
		      ;; PREV is the start of a region of fontified
		      ;; text containing AROUND.  Start fontfifying a
		      ;; chunk size before the end of the unfontified
		      ;; region in front of that.
		      (max (or (previous-single-property-change prev 'fontified)
			       (point-min))
			   (- prev jit-lock-chunk-size)))
		     (t
		      ;; PREV is the start of a region of unfontified
		      ;; text containing AROUND.  Start at PREV or
		      ;; chunk size in front of AROUND, whichever is
		      ;; nearer.
		      (max prev (- around jit-lock-chunk-size)))))
	     (result (cond ((null start) next)
			   ((null next) start)
			   ((< (- around start) (- next around)) start)
			   (t next))))
	result))))
	

(defun jit-lock-stealth-fontify ()
  "Fontify buffers stealthily.
This functions is called after Emacs has been idle for
`jit-lock-stealth-time' seconds."
  (unless (or executing-kbd-macro
	      (window-minibuffer-p (selected-window)))
    (let ((buffers (buffer-list))
	  minibuffer-auto-raise
	  message-log-max)
      (while (and buffers (not (input-pending-p)))
	(let ((buffer (car buffers)))
	  (setq buffers (cdr buffers))
	  
	  (with-current-buffer buffer
	    (when jit-lock-mode
	      ;; This is funny.  Calling sit-for with 3rd arg non-nil
	      ;; so that it doesn't redisplay, internally calls
	      ;; wait_reading_process_input also with a parameter
	      ;; saying "don't redisplay."  Since this function here
	      ;; is called periodically, this effectively leads to
	      ;; process output not being redisplayed at all because
	      ;; redisplay_internal is never called.  (That didn't
	      ;; work in the old redisplay either.)  So, we learn that
	      ;; we mustn't call sit-for that way here.  But then, we
	      ;; have to be cautious not to call sit-for in a widened
	      ;; buffer, since this could display hidden parts of that
	      ;; buffer.  This explains the seemingly weird use of
	      ;; save-restriction/widen here.

	      (with-temp-message (if jit-lock-stealth-verbose
				     (concat "JIT stealth lock "
					     (buffer-name)))

		;; Perform deferred unfontification, if any.
		(when jit-lock-first-unfontify-pos
		  (save-restriction
		    (widen)
		    (when (and (>= jit-lock-first-unfontify-pos (point-min))
			       (< jit-lock-first-unfontify-pos (point-max)))
		      (with-buffer-prepared-for-font-lock
		       (put-text-property jit-lock-first-unfontify-pos
					  (point-max) 'fontified nil))
		      (setq jit-lock-first-unfontify-pos nil))))

		;; In the following code, the `sit-for' calls cause a
		;; redisplay, so it's required that the
		;; buffer-modified flag of a buffer that is displayed
		;; has the right value---otherwise the mode line of
		;; an unmodified buffer would show a `*'.
		(let (start
		      (nice (or jit-lock-stealth-nice 0))
		      (point (point)))
		  (while (and (setq start
				    (jit-lock-stealth-chunk-start point))
			      (sit-for nice))
		    
		    ;; Wait a little if load is too high.
		    (when (and jit-lock-stealth-load
			       (> (car (load-average)) jit-lock-stealth-load))
		      (sit-for (or jit-lock-stealth-time 30)))
		    
		    ;; Unless there's input pending now, fontify.
		    (unless (input-pending-p)
		      (jit-lock-function-1 start))))))))))))



;;; Deferred fontification.

(defun jit-lock-after-change (start end old-len)
  "Mark the rest of the buffer as not fontified after a change.
Installed on `after-change-functions'.
START and END are the start and end of the changed text.  OLD-LEN
is the pre-change length.
This function ensures that lines following the change will be refontified
in case the syntax of those lines has changed.  Refontification
will take place when text is fontified stealthily."
  ;; Don't do much here---removing text properties is too slow for
  ;; fast typers, giving them the impression of Emacs not being
  ;; very responsive.
  (when jit-lock-mode
    (setq jit-lock-first-unfontify-pos
	  (if jit-lock-first-unfontify-pos
	      (min jit-lock-first-unfontify-pos start)
	    start))))
  

(provide 'jit-lock)

;; jit-lock.el ends here
