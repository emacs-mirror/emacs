;;; frame.el --- multi-frame management independent of window systems.

;;;; Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(defvar frame-creation-function nil
  "Window-system dependent function to call to create a new frame.
The window system startup file should set this to its frame creation
function, which should take an alist of parameters as its argument.")

;;; The default value for this must ask for a minibuffer.  There must
;;; always exist a frame with a minibuffer, and after we delete the
;;; terminal frame, this will be the only frame.
(defvar initial-frame-alist '((minibuffer . t))
  "Alist of values used when creating the initial emacs text frame.
These may be set in your init file, like this:
 (setq initial-frame-alist '((top . 1) (left . 1) (width . 80) (height . 55)))
These supercede the values given in frame-default-alist.")

(defvar minibuffer-frame-alist nil
  "Alist of values to apply to a minibuffer frame.
These may be set in your init file, like this:
 (setq minibuffer-frame-alist
   '((top . 1) (left . 1) (width . 80) (height . 1)))
These supercede the values given in default-frame-alist.")

(defvar pop-up-frame-alist nil
  "Alist of values used when creating pop-up frames.
Pop-up frames are used for completions, help, and the like.
This variable can be set in your init file, like this:
  (setq pop-up-frame-alist '((width . 80) (height . 20)))
These supercede the values given in default-frame-alist.")

(setq pop-up-frame-function
      (function (lambda ()
		  (new-frame pop-up-frame-alist))))


;;;; Arrangement of frames at startup

;;; 1) Load the window system startup file from the lisp library and read the
;;; high-priority arguments (-q and the like).  The window system startup
;;; file should create any frames specified in the window system defaults.
;;; 
;;; 2) If no frames have been opened, we open an initial text frame.
;;;
;;; 3) Once the init file is done, we apply any newly set parameters
;;; in initial-frame-alist to the frame.

(add-hook 'before-init-hook 'frame-initialize)
(add-hook 'window-setup-hook 'frame-notice-user-settings)

;;; If we create the initial frame, this is it.
(defvar frame-initial-frame nil)

;;; startup.el calls this function before loading the user's init
;;; file - if there is no frame with a minibuffer open now, create
;;; one to display messages while loading the init file.
(defun frame-initialize ()
  
  ;; Are we actually running under a window system at all?
  (if (and window-system (not noninteractive))
      (let ((frames (frame-list)))
    
	;; Look for a frame that has a minibuffer.
	(while (and frames
		    (or (eq (car frames) terminal-frame)
			(not (cdr (assq 'minibuffer
					(frame-parameters
					 (car frames)))))))
	  (setq frames (cdr frames)))

	;; If there was none, then we need to create the opening frame.
	(or frames
	    (setq default-minibuffer-frame
		  (setq frame-initial-frame
			(new-frame initial-frame-alist))))
    
	;; At this point, we know that we have a frame open, so we 
	;; can delete the terminal frame.
	(delete-frame terminal-frame)
	(setq terminal-frame nil))
    
    ;; No, we're not running a window system.  Arrange to cause errors.
    (setq frame-creation-function
	  (function
	   (lambda (parameters)
	     (error
	      "Can't create multiple frames without a window system."))))))
					
;;; startup.el calls this function after loading the user's init file.
;;; If we created a minibuffer before knowing if we had permission, we
;;; need to see if it should go away or change.  Create a text frame
;;; here.
(defun frame-notice-user-settings ()
  (if (frame-live-p frame-initial-frame)
      (progn
	;; If the user wants a minibuffer-only frame, we'll have to
	;; make a new one; you can't remove or add a root window to/from
	;; an existing frame.
	;; NOTE: default-frame-alist was nil when we created the
	;; existing frame.  We need to explicitly include
	;; default-frame-alist in the parameters of the screen we
	;; create here, so that its new value, gleaned from the user's
	;; .emacs file, will be applied to the existing screen.
	(if (eq (cdr (or (assq 'minibuffer initial-frame-alist)
			 '(minibuffer . t)))
		     'only)
	    (progn
	      (setq default-minibuffer-frame
		    (new-frame
		     (append initial-frame-alist
			     default-frame-alist
			     (frame-parameters frame-initial-frame))))

	      ;; Redirect events enqueued at this frame to the new frame.
	      ;; Is this a good idea?
	      (redirect-frame-focus frame-initial-frame
				    default-minibuffer-frame)

	      (delete-frame frame-initial-frame))
	  (modify-frame-parameters frame-initial-frame
				   (append initial-frame-alist
					   default-frame-alist)))))

  ;; Make sure the initial frame can be GC'd if it is ever deleted.
  (makunbound 'frame-initial-frame))


;;;; Creation of additional frames

;;; Return some frame other than the current frame,
;;; creating one if neccessary.  Note that the minibuffer frame, if
;;; separate, is not considered (see next-frame).
(defun get-other-frame ()
  (let ((s (if (equal (next-frame (selected-frame)) (selected-frame))
	       (new-frame)
	     (next-frame (selected-frame)))))
    s))

(defun next-multiframe-window ()
  "Select the next window, regardless of which frame it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      t)))

(defun previous-multiframe-window ()
  "Select the previous window, regardless of which frame it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  t)))

(defun new-frame (&optional parameters)
  "Create a new frame, displaying the current buffer.

Optional argument PARAMETERS is an alist of parameters for the new
frame.  Specifically, PARAMETERS is a list of pairs, each having one
of the following forms:

(name . STRING)	- The frame should be named STRING.

(height . NUMBER) - The frame should be NUMBER text lines high.  If
	this parameter is present, the width parameter must also be
	given.

(width . NUMBER) - The frame should be NUMBER characters in width.
	If this parameter is present, the height parameter must also
	be given.

(minibuffer . t) - the frame should have a minibuffer
(minibuffer . none) - the frame should have no minibuffer
(minibuffer . only) - the frame should contain only a minibuffer
(minibuffer . WINDOW) - the frame should use WINDOW as its minibuffer window.

(NAME . VALUE), specifying the parameter and the value it should have.
NAME should be one of the following symbols:
  name		VALUE 

The documentation for the function x-create-frame describes
additional frame parameters that Emacs will recognize when running
under the X Window System."
  (interactive)
  (funcall frame-creation-function parameters))


;;;; Frame configurations

(defun current-frame-configuration ()
  "Return a list describing the positions and states of all frames.
Each element is a list of the form (FRAME ALIST WINDOW-CONFIG), where
FRAME is a frame object, ALIST is an association list specifying
some of FRAME's parameters, and WINDOW-CONFIG is a window
configuration object for FRAME."
  (mapcar (function
	   (lambda (frame)
	     (list frame
		   (frame-parameters frame)
		   (current-window-configuration frame))))
	  (frame-list)))

(defun set-frame-configuration (configuration)
  "Restore the frames to the state described by CONFIGURATION.
Each frame listed in CONFIGURATION has its position, size, window
configuration, and other parameters set as specified in CONFIGURATION."
  (let (frames-to-delete)
    (mapcar (function
	     (lambda (frame)
	       (let ((parameters (assq frame configuration)))
		 (if parameters
		     (progn
		       (modify-frame-parameters frame (nth 1 parameters))
		       (set-window-configuration (nth 2 parameters)))
		   (setq frames-to-delete (cons frame frames-to-delete))))))
	    (frame-list))
    (mapcar 'delete-frame frames-to-delete)))


;;;; Convenience functions for accessing and interactively changing
;;;; frame parameters.

(defun frame-height (&optional frame)
  "Return number of lines available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (cdr (assq 'height (frame-parameters frame))))

(defun frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (cdr (assq 'width (frame-parameters frame))))

(defun set-default-font (font-name)
  (interactive "sFont name: ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'font font-name))))

(defun set-frame-background (color-name)
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'background-color color-name))))

(defun set-frame-foreground (color-name)
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'foreground-color color-name))))

(defun set-cursor-color (color-name)
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'cursor-color color-name))))

(defun set-pointer-color (color-name)
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'mouse-color color-name))))

(defun set-auto-raise (toggle)
  (interactive "xt or nil? ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'auto-raise toggle))))

(defun set-auto-lower (toggle)
  (interactive "xt or nil? ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'auto-lower toggle))))

(defun set-vertical-bar (toggle)
  (interactive "xt or nil? ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'vertical-scroll-bar toggle))))

(defun set-horizontal-bar (toggle)
  (interactive "xt or nil? ")
  (modify-frame-parameters (selected-frame)
			    (list (cons 'horizontal-scroll-bar toggle))))

;;;; Aliases for backward compatibility with Emacs 18.
(fset 'screen-height 'frame-height)
(fset 'screen-width 'frame-width)

(defun set-screen-width (cols &optional pretend)
  "Obsolete function to change the size of the screen to COLS columns.\n\
Optional second arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the frame should not be changed.\n\
This function is provided only for compatibility with Emacs 18; new code\n\
should use `set-frame-width instead'."
  (set-frame-width (selected-frame) cols pretend))

(defun set-screen-height (lines &optional pretend)
  "Obsolete function to change the height of the screen to LINES lines.\n\
Optional second arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the screen should not be changed.\n\
This function is provided only for compatibility with Emacs 18; new code\n\
should use `set-frame-width' instead."
  (set-frame-height (selected-frame) lines pretend))

(make-obsolete 'screen-height 'frame-height)
(make-obsolete 'screen-width  'frame-width)
(make-obsolete 'set-screen-width 'set-frame-width)
(make-obsolete 'set-screen-height 'set-frame-height)


;;;; Key bindings
(defvar ctl-x-5-map (make-sparse-keymap)
  "Keymap for frame commands.")
(fset 'ctl-x-5-prefix ctl-x-5-map)
(define-key ctl-x-map "5" 'ctl-x-5-prefix)

(define-key ctl-x-5-map "2" 'new-frame)
(define-key ctl-x-5-map "0" 'delete-frame)

(provide 'frame)

;;; frame.el ends here
