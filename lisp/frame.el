;;; frame.el --- multi-frame management independent of window systems.

;;;; Copyright (C) 1993 Free Software Foundation, Inc.

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

;;; The initial value given here for this must ask for a minibuffer.
;;; There must always exist a frame with a minibuffer, and after we
;;; delete the terminal frame, this will be the only frame.
(defvar initial-frame-alist '((minibuffer . t))
  "Alist of frame parameters for creating the initial X window frame.
You can set this in your `.emacs' file; for example,
 (setq initial-frame-alist '((top . 1) (left . 1) (width . 80) (height . 55)))
If the value calls for a frame without a minibuffer, and you do not create a
minibuffer frame on your own, one is created according to
`minibuffer-frame-alist'.
Parameters specified here supersede the values given in
`default-frame-alist'.")

(defvar minibuffer-frame-alist '((width . 80) (height . 2))
  "Alist of frame parameters for initially creating a minibuffer frame.
You can set this in your `.emacs' file; for example,
 (setq minibuffer-frame-alist
   '((top . 1) (left . 1) (width . 80) (height . 2)))
Parameters specified here supersede the values given in
`default-frame-alist'.")

(defvar pop-up-frame-alist nil
  "Alist of frame parameters used when creating pop-up frames.
Pop-up frames are used for completions, help, and the like.
This variable can be set in your init file, like this:
  (setq pop-up-frame-alist '((width . 80) (height . 20)))
These supersede the values given in `default-frame-alist'.")

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

;; These are now called explicitly at the proper times, 
;; since that is easier to understand.
;; Actually using hooks within Emacs is bad for future maintenance. --rms.
;; (add-hook 'before-init-hook 'frame-initialize)
;; (add-hook 'window-setup-hook 'frame-notice-user-settings)

;;; If we create the initial frame, this is it.
(defvar frame-initial-frame nil)

;; Record the parameters used in frame-initialize to make the initial frame.
(defvar frame-initial-frame-alist)

;;; startup.el calls this function before loading the user's init
;;; file - if there is no frame with a minibuffer open now, create
;;; one to display messages while loading the init file.
(defun frame-initialize ()
  
  ;; Are we actually running under a window system at all?
  (if (and window-system (not noninteractive))
      (progn
	;; If there is no frame with a minibuffer besides the terminal
	;; frame, then we need to create the opening frame.  Make sure
	;; it has a minibuffer, but let initial-frame-alist omit the
	;; minibuffer spec.
	(or (delq terminal-frame (minibuffer-frame-list))
	    (progn
	      (setq frame-initial-frame-alist
		    (append initial-frame-alist default-frame-alist))
	      (setq default-minibuffer-frame
		    (setq frame-initial-frame
			  (new-frame initial-frame-alist)))
	      ;; Delete any specifications for window geometry parameters
	      ;; so that we won't reapply them in frame-notice-user-settings.
	      ;; It would be wrong to reapply them then,
	      ;; because that would override explicit user resizing.
	      (setq initial-frame-alist
		    (frame-remove-geometry-params initial-frame-alist))
	      ;; Handle `reverse' as a parameter.
	      (if (cdr (or (assq 'reverse initial-frame-alist)
			   (assq 'reverse default-frame-alist)
			   (cons nil
				 (member (x-get-resource "reverseVideo" "ReverseVideo")
					 '("on" "true")))))
		  (let ((params (frame-parameters frame-initial-frame)))
		    (modify-frame-parameters
		     frame-initial-frame
		     ;; Must set cursor-color after background color.
		     ;; So put it first.
		     (list (cons 'cursor-color
				 (cdr (assq 'background-color params)))
			   (cons 'foreground-color
				 (cdr (assq 'background-color params)))
			   (cons 'background-color
				 (cdr (assq 'foreground-color params)))
			   (cons 'mouse-color
				 (cdr (assq 'background-color params)))
			   (cons 'border-color
				 (cdr (assq 'background-color params)))))))))

	;; At this point, we know that we have a frame open, so we 
	;; can delete the terminal frame.
	(delete-frame terminal-frame)
	(setq terminal-frame nil))
    
    ;; No, we're not running a window system.  Arrange to cause errors.
    (setq frame-creation-function
	  (function
	   (lambda (parameters)
	     (error
	      "Can't create multiple frames without a window system"))))))
					
;;; startup.el calls this function after loading the user's init
;;; file.  Now default-frame-alist and initial-frame-alist contain
;;; information to which we must react; do what needs to be done.
(defun frame-notice-user-settings ()

  ;; Creating and deleting frames may shift the selected frame around,
  ;; and thus the current buffer.  Protect against that.  We don't
  ;; want to use save-excursion here, because that may also try to set
  ;; the buffer of the selected window, which fails when the selected
  ;; window is the minibuffer.
  (let ((old-buffer (current-buffer)))

    ;; If the initial frame is still around, apply initial-frame-alist
    ;; and default-frame-alist to it.
    (if (frame-live-p frame-initial-frame)

	;; The initial frame we create above always has a minibuffer.
	;; If the user wants to remove it, or make it a minibuffer-only
	;; frame, then we'll have to delete the current frame and make a
	;; new one; you can't remove or add a root window to/from an
	;; existing frame.
	;;
	;; NOTE: default-frame-alist was nil when we created the
	;; existing frame.  We need to explicitly include
	;; default-frame-alist in the parameters of the screen we
	;; create here, so that its new value, gleaned from the user's
	;; .emacs file, will be applied to the existing screen.
	(if (not (eq (cdr (or (assq 'minibuffer initial-frame-alist)
			      (assq 'minibuffer default-frame-alist)
			      '(minibuffer . t)))
		     t))
	    ;; Create the new frame.
	    (let ((new
		   (new-frame
		    (append initial-frame-alist
			    default-frame-alist
			    (frame-parameters frame-initial-frame)))))

	      ;; The initial frame, which we are about to delete, may be
	      ;; the only frame with a minibuffer.  If it is, create a
	      ;; new one.
	      (or (delq frame-initial-frame (minibuffer-frame-list))
		  (new-frame (append minibuffer-frame-alist
				     '((minibuffer . only)))))

	      ;; If the initial frame is serving as a surrogate
	      ;; minibuffer frame for any frames, we need to wean them
	      ;; onto a new frame.  The default-minibuffer-frame
	      ;; variable must be handled similarly.
	      (let ((users-of-initial
		     (filtered-frame-list
		      (function (lambda (frame)
				  (and (not (eq frame frame-initial-frame))
				       (eq (window-frame
					    (minibuffer-window frame))
					   frame-initial-frame)))))))
		(if (or users-of-initial
			(eq default-minibuffer-frame frame-initial-frame))

		    ;; Choose an appropriate frame.  Prefer frames which
		    ;; are only minibuffers.
		    (let* ((new-surrogate
			    (car
			     (or (filtered-frame-list
				  (function
				   (lambda (frame)
				     (eq (cdr (assq 'minibuffer
						    (frame-parameters frame)))
					 'only))))
				 (minibuffer-frame-list))))
			   (new-minibuffer (minibuffer-window new-surrogate)))

		      (if (eq default-minibuffer-frame frame-initial-frame)
			  (setq default-minibuffer-frame new-surrogate))

		      ;; Wean the frames using frame-initial-frame as
		      ;; their minibuffer frame.
		      (mapcar
		       (function
			(lambda (frame)
			  (modify-frame-parameters
			   frame (list (cons 'minibuffer new-minibuffer)))))
		       users-of-initial))))

	      ;; Redirect events enqueued at this frame to the new frame.
	      ;; Is this a good idea?
	      (redirect-frame-focus frame-initial-frame new)

	      ;; Finally, get rid of the old frame.
	      (delete-frame frame-initial-frame t))

	  ;; Otherwise, we don't need all that rigamarole; just apply
	  ;; the new parameters.
	  (let (newparms allparms tail)
	    (setq allparms (append initial-frame-alist
				   default-frame-alist))
	    (setq tail allparms)
	    ;; Find just the parms that have changed since we first
	    ;; made this frame.  Those are the ones actually set by
	    ;; the init file.  For those parms whose values we already knew
	    ;; (such as those spec'd by command line options)
	    ;; it is undesirable to specify the parm again
	    ;; once the user has seen the frame and been able to alter it
	    ;; manually.
	    (while tail
	      (let (newval oldval)
		(setq oldval (cdr (assq (car (car tail))
					frame-initial-frame-alist)))
		(setq newval (cdr (assq (car (car tail)) allparms)))
		(or (eq oldval newval)
		    (setq newparms
			  (cons (cons (car (car tail)) newval) newparms))))
	      (setq tail (cdr tail)))
	    (modify-frame-parameters frame-initial-frame
				     (nreverse newparms)))))

    ;; Restore the original buffer.
    (set-buffer old-buffer)

    ;; Make sure the initial frame can be GC'd if it is ever deleted.
    ;; Make sure frame-notice-user-settings does nothing if called twice.
    (setq frame-initial-frame nil)))


;;;; Creation of additional frames, and other frame miscellanea

;;; Return some frame other than the current frame, creating one if
;;; necessary.  Note that the minibuffer frame, if separate, is not
;;; considered (see next-frame).
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

;; Alias, kept temporarily.
(defalias 'new-frame 'make-frame)
(defun make-frame (&optional parameters)
  "Create a new frame, displaying the current buffer.

Optional argument PARAMETERS is an alist of parameters for the new
frame.  Specifically, PARAMETERS is a list of pairs, each having one
of the following forms:

\(name . STRING)	- The frame should be named STRING.

\(height . NUMBER) - The frame should be NUMBER text lines high.  If
	this parameter is present, the width parameter must also be
	given.

\(width . NUMBER) - The frame should be NUMBER characters in width.
	If this parameter is present, the height parameter must also
	be given.

\(minibuffer . t) - the frame should have a minibuffer
\(minibuffer . nil) - the frame should have no minibuffer
\(minibuffer . only) - the frame should contain only a minibuffer
\(minibuffer . WINDOW) - the frame should use WINDOW as its minibuffer window.

The documentation for the function `x-create-frame' describes
additional frame parameters that Emacs recognizes for X window frames."
  (interactive)
  (let ((nframe))
    (run-hooks 'before-make-frame-hook)
    (setq nframe (funcall frame-creation-function parameters))
    (run-hooks 'after-make-frame-hook)
    nframe))

(defun filtered-frame-list (predicate)
  "Return a list of all live frames which satisfy PREDICATE."
  (let ((frames (frame-list))
	good-frames)
    (while (consp frames)
      (if (funcall predicate (car frames))
	  (setq good-frames (cons (car frames) good-frames)))
      (setq frames (cdr frames)))
    good-frames))

(defun minibuffer-frame-list ()
  "Return a list of all frames with their own minibuffers."
  (filtered-frame-list
   (function (lambda (frame)
	       (eq frame (window-frame (minibuffer-window frame)))))))

(defun frame-remove-geometry-params (param-list)
  "Return the parameter list PARAM-LIST, but with geometry specs removed.
This deletes all bindings in PARAM-LIST for `top', `left', `width',
and `height' parameters.
Emacs uses this to avoid overriding explicit moves and resizings from
the user during startup."
  (setq param-list (cons nil param-list))
  (let ((tail param-list))
    (while (consp (cdr tail))
      (if (and (consp (car (cdr tail)))
	       (memq (car (car (cdr tail))) '(height width top left)))
	  (setcdr tail (cdr (cdr tail)))
	(setq tail (cdr tail)))))
  (cdr param-list))


(defun other-frame (arg)
  "Select the ARG'th different visible frame, and raise it.
All frames are arranged in a cyclic order.
This command selects the frame ARG steps away in that order.
A negative ARG moves in the opposite order."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (while (not (eq (frame-visible-p frame) t))
	(setq frame (next-frame frame)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (while (not (eq (frame-visible-p frame) t))
	(setq frame (previous-frame frame)))
      (setq arg (1+ arg)))
    (raise-frame frame)
    (select-frame frame)
    (set-mouse-position (selected-frame) (1- (frame-width)) 0)
    (unfocus-frame)))

;;;; Frame configurations

(defun current-frame-configuration ()
  "Return a list describing the positions and states of all frames.
Its car is `frame-configuration'.
Each element of the cdr is a list of the form (FRAME ALIST WINDOW-CONFIG),
where
  FRAME is a frame object,
  ALIST is an association list specifying some of FRAME's parameters, and
  WINDOW-CONFIG is a window configuration object for FRAME."
  (cons 'frame-configuration
	(mapcar (function
		 (lambda (frame)
		   (list frame
			 (frame-parameters frame)
			 (current-window-configuration frame))))
		(frame-list))))

(defun set-frame-configuration (configuration &optional nodelete)
  "Restore the frames to the state described by CONFIGURATION.
Each frame listed in CONFIGURATION has its position, size, window
configuration, and other parameters set as specified in CONFIGURATION.
Ordinarily, this function deletes all existing frames not
listed in CONFIGURATION.  But if optional second argument NODELETE
is given and non-nil, the unwanted frames are iconified instead."
  (or (frame-configuration-p configuration)
      (signal 'wrong-type-argument
	      (list 'frame-configuration-p configuration)))
  (let ((config-alist (cdr configuration))
	frames-to-delete)
    (mapcar (function
	     (lambda (frame)
	       (let ((parameters (assq frame config-alist)))
		 (if parameters
		     (progn
		       (modify-frame-parameters
			frame
			;; Since we can't set a frame's minibuffer status, 
			;; we might as well omit the parameter altogether.
			(let* ((parms (nth 1 parameters))
			       (mini (assq 'minibuffer parms)))
			  (if mini (setq parms (delq mini parms)))
			  parms))
		       (set-window-configuration (nth 2 parameters)))
		   (setq frames-to-delete (cons frame frames-to-delete))))))
	    (frame-list))
    (if nodelete
	;; Note: making frames invisible here was tried
	;; but led to some strange behavior--each time the frame
	;; was made visible again, the window manager asked afresh
	;; for where to put it.
	(mapcar 'iconify-frame frames-to-delete)
      (mapcar 'delete-frame frames-to-delete))))

(defun frame-configuration-p (object)
  "Return non-nil if OBJECT seems to be a frame configuration.
Any list whose car is `frame-configuration' is assumed to be a frame
configuration."
  (and (consp object)
       (eq (car object) 'frame-configuration)))


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
  "Set the font of the selected frame to FONT.
When called interactively, prompt for the name of the font to use."
  (interactive "sFont name: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'font font-name))))

(defun set-background-color (color-name)
  "Set the background color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'background-color color-name))))

(defun set-foreground-color (color-name)
  "Set the foreground color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'foreground-color color-name))))

(defun set-cursor-color (color-name)
  "Set the text cursor color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'cursor-color color-name))))

(defun set-mouse-color (color-name)
  "Set the color of the mouse pointer of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'mouse-color color-name))))

(defun set-border-color (color-name)
  "Set the color of the border of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'border-color color-name))))

(defun auto-raise-mode (arg)
  "Toggle whether or not the selected frame should auto-raise.
With arg, turn auto-raise mode on if and only if arg is positive."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (cdr (assq 'auto-raise (frame-parameters (selected-frame))))
		-1 1)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'auto-raise (> arg 0)))))

(defun auto-lower-mode (arg)
  "Toggle whether or not the selected frame should auto-lower.
With arg, turn auto-lower mode on if and only if arg is positive."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (cdr (assq 'auto-lower (frame-parameters (selected-frame))))
		-1 1)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'auto-lower (> arg 0)))))

(defun toggle-scroll-bar (arg)
  "Toggle whether or not the selected frame has vertical scroll bars.
With arg, turn vertical scroll bars on if and only if arg is positive."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (cdr (assq 'vertical-scroll-bars
			   (frame-parameters (selected-frame))))
		-1 1)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'vertical-scroll-bars (> arg 0)))))

(defun toggle-horizontal-scroll-bar (arg)
  "Toggle whether or not the selected frame has horizontal scroll bars.
With arg, turn horizontal scroll bars on if and only if arg is positive.
Horizontal scroll bars aren't implemented yet."
  (interactive "P")
  (error "Horizontal scroll bars aren't implemented yet"))


;;;; Aliases for backward compatibility with Emacs 18.
(defalias 'screen-height 'frame-height)
(defalias 'screen-width 'frame-width)

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
(defalias 'ctl-x-5-prefix ctl-x-5-map)
(define-key ctl-x-map "5" 'ctl-x-5-prefix)

(define-key ctl-x-5-map "2" 'new-frame)
(define-key ctl-x-5-map "0" 'delete-frame)
(define-key ctl-x-5-map "o" 'other-frame)

(provide 'frame)

;;; frame.el ends here
