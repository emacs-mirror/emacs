;;; This file is in the public domain.

;;; Keywords: scroll display minor-mode
;;; Author: Pete Ware <ware@cis.ohio-state.edu>
;;; Maintainer: FSF

;;; Commentary:

;;; This file provides functions that
;;; automatically scroll the window horizontally when the point moves
;;; off the left or right side of the window.

;;; Once this library is loaded, automatic horizontal scrolling
;;; occurs whenever long lines are being truncated.
;;; To request truncation of long lines, set the variable
;;; Setting the variable `truncate-lines' to non-nil.
;;; You can do this for all buffers as follows:
;;;
;;; (set-default 'truncate-lines t)

;;; Here is how to do it for C mode only:
;;;
;;; (set-default 'truncate-lines nil)	; this is the original value
;;; (defun my-c-mode-hook ()
;;;   "Run when C-mode starts up.  Changes ..."
;;;   ... set various personal preferences ...
;;;   (setq truncate-lines t))
;;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;;;
;;;
;;; As a finer level of control, you can still have truncated lines but
;;; without the automatic horizontal scrolling by setting the buffer
;;; local variable `auto-show-mode' to nil.  The default value is t.
;;; The command `auto-show-mode' toggles the value of the variable
;;; `auto-show-mode'.

;;; Code:

(defvar auto-show-mode t
  "*Non-nil enables automatic horizontal scrolling, when lines are truncated.
The default value is t.  To change the default, do this:
	(set-default 'auto-show-mode nil)
See also command `auto-show-mode'.
This variable has no effect when lines are not being truncated.")

(make-variable-buffer-local 'auto-show-mode)

(defvar auto-show-shift-amount 8 
  "*Extra columns to scroll. for automatic horizontal scrolling.")

(defvar auto-show-show-left-margin-threshold 50
  "*Threshold column for automatic horizontal scrolling to the right.
If point is before this column, we try to scroll to make the left margin
visible.  Setting this to 0 disables this feature.")

(defun auto-show-truncationp ()
  "True if line truncation is enabled for the selected window."
  (or truncate-lines 
      (and truncate-partial-width-windows
	   (< (window-width) (frame-width)))))

;;;###autoload
(defun auto-show-mode (arg)
  "Turn automatic horizontal scroll mode on or off.
With arg, turn auto scrolling on if arg is positive, off otherwise."
  (interactive "P")
  (setq auto-show-mode
	(if (null arg)
	    (not auto-show-mode)
	  (> (prefix-numeric-value arg) 0))))
  
(defun auto-show-make-point-visible (&optional ignore-arg)
  "Scroll horizontally to make point visible, if that is enabled.
This function only does something if `auto-show-mode' is non-nil
and longlines are being truncated in the selected window.
See also the command `auto-show-toggle'."
  (interactive)
  (if (and auto-show-mode (auto-show-truncationp)
	   (equal (window-buffer) (current-buffer)))
      (let* ((col (current-column))	;column on line point is at
	     (scroll (window-hscroll))	;how far window is scrolled
	     (w-width (- (window-width) 
			 (if (> scroll 0)
			     2 1)))	;how wide window is on the screen
	     (right-col (+ scroll w-width)))
	(if (and (< col auto-show-show-left-margin-threshold)
		 (< col (window-width))
		 (> scroll 0))
	    (scroll-right scroll)
	  (if (< col scroll)		;to the left of the screen
	      (scroll-right (+ (- scroll col) auto-show-shift-amount))
	    (if (or (> col right-col)	;to the right of the screen
		    (and (= col right-col)
			 (not (eolp))))
		(scroll-left (+ auto-show-shift-amount 
				(- col (+ scroll w-width))))
	      )
	    )
	  )
	)
    )
  )

;; Do auto-scrolling after commands.
(add-hook 'post-command-hook 'auto-show-make-point-visible)

;; Do auto-scrolling in comint buffers after process output also.
(add-hook 'comint-output-filter-functions 'auto-show-make-point-visible t)

(provide 'auto-show)

;; auto-show.el ends here

