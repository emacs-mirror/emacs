;;  LCD Archive Entry:
;;  auto-show|Pete Ware|ware@cis.ohio-state.edu|
;;  Automatically scroll horizontally|
;;  95-02-24|1.9|~/misc/auto-show.el|
;;
;;;
;;; Author:
;;;
;;;	Pete Ware					ware@cis.ohio-state.edu
;;;	CIS Dept, Ohio State University			w/ (614) 292-8501
;;;	774 Dreese, 2015 Neil Ave.			h/ (614) 791-1347
;;;	Columbus, OH 43210		http://www.cis.ohio-state.edu/~ware
;;;
;;;
;;; Modification history:
;;; 02/24/95	Added auto-show-show-left-margin-threshold so that if
;;;		there is anyway for the left margin to be displayed it is.
;;; 02/24/95	Only scroll window if it matches current buffer.  Added
;;;		function for enabeling scrolling in comint buffers on output.
;;; 02/13/95	Aded Kevin Broadey <KevinB@bartley.demon.co.uk> fix so that
;;;		it doesn't scroll if we are at window border and at the
;;;		end of the line (i.e. newline character)
;;; 02/10/95	jeff.dwork@amd.com added auto-show-toggle function.
;;; 02/08/95	Added auto-show-enable as per
;;;		jeff.dwork@amd.com (Jeff Dwork)'s suggestion.  Cleaned up
;;;		documentation.
;;; 02/07/95	Rewrote for emacs 19: much, much cleaner.  Renamed auto-show
;;; 8/6/90	Make next-line/previous-line do better job following movement.
;;; 3/21/90         better calculation of w-width in e-make-point-visible
;;;                 test for truncated windows
;;;                 added substitute-in-keymap
;;;                 renamed to auto-horizontal
;;;                 added backward-delete-char
;;; 8/13/88	Created

;;;
;;; This is a rewrite of auto-horizontal.  It is comparable in
;;; functionality to hscroll.el except it is not a minor mode and does
;;; not use any timers.  This file provides functions that
;;; automatically scroll the window horizontally when the point moves
;;; off the left or right side of the window.  To load it just add:
;;; 	(require 'auto-show)
;;; to your .emacs.
;;;
;;; Setting the variable ``truncate-lines'' to non-nil causes long
;;; lines to disappear off the end of the screen instead of wrapping
;;; to the beginning of the next line.  To make this the default for
;;; all buffers add the following line to your .emacs (sans ;;;):
;;;
;;; (set-default 'truncate-lines t)
;;;

;;; However, I've found that I only want this when I'm editing C code.
;;; Accordingly I have something like the following in my .emacs:
;;;
;;; (set-default 'truncate-lines nil)	; this is the original value
;;; (defun my-c-mode-hook ()
;;;   "Run when C-mode starts up.  Changes ..."
;;;   ... set various personal preferences ...
;;;   (setq truncate-lines t))
;;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;;;
;;;
;;; As a finer level of control, one can still have truncated lines but
;;; without the automatic left and right scrolling by setting the buffer
;;; local variable ``auto-show-enable'' to nil.  The default value is t.
;;; The command ``auto-show-toggle'' will toggle the value of
;;; ``auto-show-enable''.
;;;
;;;
;;; I also like the output from my shell's (and other comint.el based commands)
;;; to scroll on output.  One can call:
;;;
;;;	(auto-show-comint-make-point-visible)
;;;
;;; which adds auto-show-make-point-visible to comint-output-filter-functions.

(provide 'auto-show)

;;;************************************************************
;;;*
;;;*  Define Automatic Horizontal Scrolling Functions
;;;*
;;;************************************************************

(add-hook 'post-command-hook 'auto-show-make-point-visible)

(defvar auto-show-enable t
  "*Allows one to turn off automatic horizontal scrolling on a per buffer
basis independent of whether truncate-lines is t.  The default value is t.
To change the default:
	(set-default 'auto-show-enable nil)
Any time auto-show-enable is changed it is only in the current buffer:
	(setq auto-show-enable nil)
turns it on for this buffer.
See also command `auto-show-toggle'.")

(make-variable-buffer-local 'auto-show-enable)

(defvar auto-show-shift-amount 8 
  "*Extra amount to shift a line when point is not visible.")

(defvar auto-show-show-left-margin-threshold 50
  "*Point must be before this column for us to try and make the left margin
visible.  Setting this to 0 disables this feature.")

(defun auto-show-truncationp ()
  "True if truncation is on for current window."
  (or truncate-lines 
      (and truncate-partial-width-windows
	   (< (window-width) (frame-width)))))

(defun auto-show-toggle ()
  "Toggle value of auto-show-enable."
  (interactive)
  (setq auto-show-enable (not auto-show-enable)))
  
(defun auto-show-make-point-visible (&optional ignore-arg)
  "Scrolls the screen horizontally to make point visible but only if
auto-show-enable is non-nil and lines are truncated.  See also variable
`auto-show-enable' and command `auto-show-toggle'."
  (interactive)
  (if (and auto-show-enable (auto-show-truncationp)
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

(defun auto-show-comint-make-point-visible ()
  "Add a function to comint-output-filter-functions that auto-scrolls
left or right on output to the buffer.

NOTE: you should load comint mode before this as comint.el uses a
defvar to initialize comint-output-filter-functions to the default value."
  (interactive)
  (add-hook 'comint-output-filter-functions 'auto-show-make-point-visible t)
  )

;; end of auto-show.el

