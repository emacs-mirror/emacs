;;; frame.el --- multi-frame management independent of window systems

;; Copyright (C) 1993, 1994, 1996, 1997, 2000, 2001
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

(defvar frame-creation-function nil
  "Window-system dependent function to call to create a new frame.
The window system startup file should set this to its frame creation
function, which should take an alist of parameters as its argument.")

;; The initial value given here used to ask for a minibuffer.
;; But that's not necessary, because the default is to have one.
;; By not specifying it here, we let an X resource specify it.
(defcustom initial-frame-alist nil
  "*Alist of frame parameters for creating the initial X window frame.
You can set this in your `.emacs' file; for example,
 (setq initial-frame-alist '((top . 1) (left . 1) (width . 80) (height . 55)))
Parameters specified here supersede the values given in `default-frame-alist'.

If the value calls for a frame without a minibuffer, and you have not created
a minibuffer frame on your own, one is created according to
`minibuffer-frame-alist'.

You can specify geometry-related options for just the initial frame
by setting this variable in your `.emacs' file; however, they won't
take effect until Emacs reads `.emacs', which happens after first creating
the frame.  If you want the frame to have the proper geometry as soon
as it appears, you need to use this three-step process:
* Specify X resources to give the geometry you want.
* Set `default-frame-alist' to override these options so that they
  don't affect subsequent frames.
* Set `initial-frame-alist' in a way that matches the X resources,
  to override what you put in `default-frame-alist'."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'frames)

(defcustom minibuffer-frame-alist '((width . 80) (height . 2))
  "*Alist of frame parameters for initially creating a minibuffer frame.
You can set this in your `.emacs' file; for example,
 (setq minibuffer-frame-alist
   '((top . 1) (left . 1) (width . 80) (height . 2)))
Parameters specified here supersede the values given in
`default-frame-alist', for a minibuffer frame."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'frames)

(defcustom pop-up-frame-alist nil
  "*Alist of frame parameters used when creating pop-up frames.
Pop-up frames are used for completions, help, and the like.
This variable can be set in your init file, like this:
  (setq pop-up-frame-alist '((width . 80) (height . 20)))
These supersede the values given in `default-frame-alist',
for pop-up frames."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'frames)

(setq pop-up-frame-function
      (function (lambda ()
		  (make-frame pop-up-frame-alist))))

(defcustom special-display-frame-alist
  '((height . 14) (width . 80) (unsplittable . t))
  "*Alist of frame parameters used when creating special frames.
Special frames are used for buffers whose names are in
`special-display-buffer-names' and for buffers whose names match
one of the regular expressions in `special-display-regexps'.
This variable can be set in your init file, like this:
  (setq special-display-frame-alist '((width . 80) (height . 20)))
These supersede the values given in `default-frame-alist'."
  :type '(repeat (cons :format "%v"
			 (symbol :tag "Parameter")
			 (sexp :tag "Value")))
  :group 'frames)

(defun special-display-popup-frame (buffer &optional args)
  "Display BUFFER in its own frame, reusing an existing window if any.
Return the window chosen.
Currently we do not insist on selecting the window within its frame.
If ARGS is an alist, use it as a list of frame parameter specs.
If ARGS is a list whose car is a symbol,
use (car ARGS) as a function to do the work.
Pass it BUFFER as first arg, and (cdr ARGS) gives the rest of the args."
  (if (and args (symbolp (car args)))
      (apply (car args) buffer (cdr args))
    (let ((window (get-buffer-window buffer t)))
      (or
       ;; If we have a window already, make it visible.
       (when window
	 (let ((frame (window-frame window)))
	   (make-frame-visible frame)
	   (raise-frame frame)
	   window))
       ;; Reuse the current window if the user requested it.
       (when (cdr (assq 'same-window args))
	 (condition-case nil
	     (progn (switch-to-buffer buffer) (selected-window))
	   (error nil)))
       ;; Stay on the same frame if requested.
       (when (or (cdr (assq 'same-frame args)) (cdr (assq 'same-window args)))
	 (let* ((pop-up-frames nil) (pop-up-windows t)
		special-display-regexps special-display-buffer-names
		(window (display-buffer buffer)))
	   ;; (set-window-dedicated-p window t)
	   window))
       ;; If no window yet, make one in a new frame.
       (let ((frame (make-frame (append args special-display-frame-alist))))
	 (set-window-buffer (frame-selected-window frame) buffer)
	 (set-window-dedicated-p (frame-selected-window frame) t)
	 (frame-selected-window frame))))))

(defun handle-delete-frame (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
	(i 0)
	(tail (frame-list)))
    (while tail
      (and (frame-visible-p (car tail))
	   (not (eq (car tail) frame))
	  (setq i (1+ i)))
      (setq tail (cdr tail)))
    (if (> i 0)
	(delete-frame frame t)
      ;; Gildea@x.org says it is ok to ask questions before terminating.
      (save-buffers-kill-emacs))))

;;;; Arrangement of frames at startup

;; 1) Load the window system startup file from the lisp library and read the
;; high-priority arguments (-q and the like).  The window system startup
;; file should create any frames specified in the window system defaults.
;;
;; 2) If no frames have been opened, we open an initial text frame.
;;
;; 3) Once the init file is done, we apply any newly set parameters
;; in initial-frame-alist to the frame.

;; These are now called explicitly at the proper times,
;; since that is easier to understand.
;; Actually using hooks within Emacs is bad for future maintenance. --rms.
;; (add-hook 'before-init-hook 'frame-initialize)
;; (add-hook 'window-setup-hook 'frame-notice-user-settings)

;; If we create the initial frame, this is it.
(defvar frame-initial-frame nil)

;; Record the parameters used in frame-initialize to make the initial frame.
(defvar frame-initial-frame-alist)

(defvar frame-initial-geometry-arguments nil)

;; startup.el calls this function before loading the user's init
;; file - if there is no frame with a minibuffer open now, create
;; one to display messages while loading the init file.
(defun frame-initialize ()
  "Create an initial frame if necessary."
  ;; Are we actually running under a window system at all?
  (if (and window-system (not noninteractive) (not (eq window-system 'pc)))
      (progn
	;; Turn on special-display processing only if there's a window system.
	(setq special-display-function 'special-display-popup-frame)

	;; If there is no frame with a minibuffer besides the terminal
	;; frame, then we need to create the opening frame.  Make sure
	;; it has a minibuffer, but let initial-frame-alist omit the
	;; minibuffer spec.
	(or (delq terminal-frame (minibuffer-frame-list))
	    (progn
	      (setq frame-initial-frame-alist
		    (append initial-frame-alist default-frame-alist nil))
	      (or (assq 'horizontal-scroll-bars frame-initial-frame-alist)
		  (setq frame-initial-frame-alist
			(cons '(horizontal-scroll-bars . t)
			      frame-initial-frame-alist)))
	      (setq default-minibuffer-frame
		    (setq frame-initial-frame
			  (make-frame frame-initial-frame-alist)))
	      ;; Delete any specifications for window geometry parameters
	      ;; so that we won't reapply them in frame-notice-user-settings.
	      ;; It would be wrong to reapply them then,
	      ;; because that would override explicit user resizing.
	      (setq initial-frame-alist
		    (frame-remove-geometry-params initial-frame-alist))))
	;; At this point, we know that we have a frame open, so we
	;; can delete the terminal frame.
	(delete-frame terminal-frame)
	(setq terminal-frame nil))

    ;; No, we're not running a window system.  Use make-terminal-frame if
    ;; we support that feature, otherwise arrange to cause errors.
    (or (eq window-system 'pc)
	(setq frame-creation-function
	      (if (fboundp 'tty-create-frame-with-faces)
		  'tty-create-frame-with-faces
		(function
		 (lambda (parameters)
		   (error
		    "Can't create multiple frames without a window system"))))))))

(defvar frame-notice-user-settings t
  "Non-nil means function `frame-notice-user-settings' wasn't run yet.")

;; startup.el calls this function after loading the user's init
;; file.  Now default-frame-alist and initial-frame-alist contain
;; information to which we must react; do what needs to be done.
(defun frame-notice-user-settings ()
  "Act on user's init file settings of frame parameters.
React to settings of `default-frame-alist', `initial-frame-alist' there."
  ;; Make menu-bar-mode and default-frame-alist consistent.
  (when (boundp 'menu-bar-mode)
    (let ((default (assq 'menu-bar-lines default-frame-alist)))
      (if default
	  (setq menu-bar-mode (not (eq (cdr default) 0)))
	(setq default-frame-alist
	      (cons (cons 'menu-bar-lines (if menu-bar-mode 1 0))
		    default-frame-alist)))))

  ;; Make tool-bar-mode and default-frame-alist consistent.  Don't do
  ;; it in batch mode since that would leave a tool-bar-lines
  ;; parameter in default-frame-alist in a dumped Emacs, which is not
  ;; what we want.
  (when (and (boundp 'tool-bar-mode)
	     (not noninteractive))
    (let ((default (assq 'tool-bar-lines default-frame-alist)))
      (if default
	  (setq tool-bar-mode (not (eq (cdr default) 0)))
	(setq default-frame-alist
	      (cons (cons 'tool-bar-lines (if tool-bar-mode 1 0))
		    default-frame-alist)))))

  ;; Creating and deleting frames may shift the selected frame around,
  ;; and thus the current buffer.  Protect against that.  We don't
  ;; want to use save-excursion here, because that may also try to set
  ;; the buffer of the selected window, which fails when the selected
  ;; window is the minibuffer.
  (let ((old-buffer (current-buffer)))

    (when (and frame-notice-user-settings
	       (null frame-initial-frame))
      ;; This case happens when we don't have a window system, and
      ;; also for MS-DOS frames.
      (let ((parms (frame-parameters frame-initial-frame)))
	;; Don't change the frame names.
	(setq parms (delq (assq 'name parms) parms))
	;; Can't modify the minibuffer parameter, so don't try.
	(setq parms (delq (assq 'minibuffer parms) parms))
	(modify-frame-parameters nil
				 (if (null window-system)
				     (append initial-frame-alist
					     default-frame-alist
					     parms
					     nil)
				   ;; initial-frame-alist and
				   ;; default-frame-alist were already
				   ;; applied in pc-win.el.
				   parms))
	(if (null window-system) ;; MS-DOS does this differently in pc-win.el
	    (let ((newparms (frame-parameters))
		  (frame (selected-frame)))
	      (tty-handle-reverse-video frame newparms)
	      ;; If we changed the background color, we need to update
	      ;; the background-mode parameter, and maybe some faces,
	      ;; too.
	      (when (assq 'background-color newparms)
		(unless (or (assq 'background-mode initial-frame-alist)
			    (assq 'background-mode default-frame-alist))
		  (frame-set-background-mode frame))
		(face-set-after-frame-default frame))))))

    ;; If the initial frame is still around, apply initial-frame-alist
    ;; and default-frame-alist to it.
    (when (frame-live-p frame-initial-frame)

      ;; When tool-bar has been switched off, correct the frame size
      ;; by the lines added in x-create-frame for the tool-bar and
      ;; switch `tool-bar-mode' off.
      (when (display-graphic-p)
	(let ((tool-bar-lines (or (assq 'tool-bar-lines initial-frame-alist)
				  (assq 'tool-bar-lines default-frame-alist))))
	  (when (and tool-bar-originally-present
                     (or (null tool-bar-lines)
                         (null (cdr tool-bar-lines))
                         (eq 0 (cdr tool-bar-lines))))
	    (let* ((char-height (frame-char-height frame-initial-frame))
		   (image-height tool-bar-images-pixel-height)
		   (margin (cond ((and (consp tool-bar-button-margin)
				       (integerp (cdr tool-bar-button-margin))
				       (> tool-bar-button-margin 0))
				  (cdr tool-bar-button-margin))
				 ((and (integerp tool-bar-button-margin)
				       (> tool-bar-button-margin 0))
				  tool-bar-button-margin)
				 (t 0)))
		   (relief (if (and (integerp tool-bar-button-relief)
				    (> tool-bar-button-relief 0))
			       tool-bar-button-relief 3))
		   (lines (/ (+ image-height
				(* 2 margin)
				(* 2 relief)
				(1- char-height))
			     char-height))
		   (height (frame-parameter frame-initial-frame 'height))
		   (newparms (list (cons 'height (- height lines))))
		   (initial-top (cdr (assq 'top
					   frame-initial-geometry-arguments)))
		   (top (frame-parameter frame-initial-frame 'top)))
	      (when (and (consp initial-top) (eq '- (car initial-top)))
		(setq newparms
		      (append newparms
			      `((top . ,(+ top (* lines char-height))))
			      nil)))
	      (modify-frame-parameters frame-initial-frame newparms)
	      (tool-bar-mode -1)))))

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
	  (let (parms new)
	    ;; If the frame isn't visible yet, wait till it is.
	    ;; If the user has to position the window,
	    ;; Emacs doesn't know its real position until
	    ;; the frame is seen to be visible.
	    (while (not (cdr (assq 'visibility
				   (frame-parameters frame-initial-frame))))
	      (sleep-for 1))
	    (setq parms (frame-parameters frame-initial-frame))

            ;; Get rid of `name' unless it was specified explicitly before.
	    (or (assq 'name frame-initial-frame-alist)
		(setq parms (delq (assq 'name parms) parms)))

	    (setq parms (append initial-frame-alist
				default-frame-alist
				parms
				nil))

	    ;; Get rid of `reverse', because that was handled
	    ;; when we first made the frame.
	    (setq parms (cons '(reverse) (delq (assq 'reverse parms) parms)))

	    (if (assq 'height frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'height parms)))
	    (if (assq 'width frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'width parms)))
	    (if (assq 'left frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'left parms)))
	    (if (assq 'top frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'top parms)))
	    (setq new
		  (make-frame
		   ;; Use the geometry args that created the existing
		   ;; frame, rather than the parms we get for it.
		   (append frame-initial-geometry-arguments
			   '((user-size . t) (user-position . t))
			   parms)))
	    ;; The initial frame, which we are about to delete, may be
	    ;; the only frame with a minibuffer.  If it is, create a
	    ;; new one.
	    (or (delq frame-initial-frame (minibuffer-frame-list))
		(make-initial-minibuffer-frame nil))

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
				 default-frame-alist nil))
	  (if (assq 'height frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'height allparms)))
	  (if (assq 'width frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'width allparms)))
	  (if (assq 'left frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'left allparms)))
	  (if (assq 'top frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'top allparms)))
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
	      (setq oldval (assq (car (car tail))
				 frame-initial-frame-alist))
	      (setq newval (cdr (assq (car (car tail)) allparms)))
	      (or (and oldval (eq (cdr oldval) newval))
		  (setq newparms
			(cons (cons (car (car tail)) newval) newparms))))
	    (setq tail (cdr tail)))
	  (setq newparms (nreverse newparms))
	  (modify-frame-parameters frame-initial-frame
				   newparms)
	  ;; If we changed the background color,
	  ;; we need to update the background-mode parameter
	  ;; and maybe some faces too.
	  (when (assq 'background-color newparms)
	    (unless (assq 'background-mode newparms)
	      (frame-set-background-mode frame-initial-frame))
	    (face-set-after-frame-default frame-initial-frame)))))

    ;; Restore the original buffer.
    (set-buffer old-buffer)

    ;; Make sure the initial frame can be GC'd if it is ever deleted.
    ;; Make sure frame-notice-user-settings does nothing if called twice.
    (setq frame-notice-user-settings nil)
    (setq frame-initial-frame nil)))

(defun make-initial-minibuffer-frame (display)
  (let ((parms (append minibuffer-frame-alist '((minibuffer . only)))))
    (if display
	(make-frame-on-display display parms)
      (make-frame parms))))

;;;; Creation of additional frames, and other frame miscellanea

(defun get-other-frame ()
  "Return some frame other than the current frame.
Create one if necessary.  Note that the minibuffer frame, if separate,
is not considered (see `next-frame')."
  (let ((s (if (equal (next-frame (selected-frame)) (selected-frame))
	       (make-frame)
	     (next-frame (selected-frame)))))
    s))

(defun next-multiframe-window ()
  "Select the next window, regardless of which frame it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      t))
  (select-frame-set-input-focus (selected-frame)))

(defun previous-multiframe-window ()
  "Select the previous window, regardless of which frame it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  t))
  (select-frame-set-input-focus (selected-frame)))

(defun make-frame-on-display (display &optional parameters)
  "Make a frame on display DISPLAY.
The optional second argument PARAMETERS specifies additional frame parameters."
  (interactive "sMake frame on display: ")
  (or (string-match "\\`[^:]*:[0-9]+\\(\\.[0-9]+\\)?\\'" display)
      (error "Invalid display, not HOST:SERVER or HOST:SERVER.SCREEN"))
  (make-frame (cons (cons 'display display) parameters)))

(defun make-frame-command ()
  "Make a new frame, and select it if the terminal displays only one frame."
  (interactive)
  (if (and window-system (not (eq window-system 'pc)))
      (make-frame)
    (select-frame (make-frame))))

(defvar before-make-frame-hook nil
  "Functions to run before a frame is created.")

(defvar after-make-frame-functions nil
  "Functions to run after a frame is created.
The functions are run with one arg, the newly created frame.")

(defvar after-setting-font-hook nil
  "Functions to run after a frame's font has been changed.")

;; Alias, kept temporarily.
(defalias 'new-frame 'make-frame)

(defun make-frame (&optional parameters)
  "Return a newly created frame displaying the current buffer.
Optional argument PARAMETERS is an alist of parameters for the new frame.
Each element of PARAMETERS should have the form (NAME . VALUE), for example:

 (name . STRING)	The frame should be named STRING.

 (width . NUMBER)	The frame should be NUMBER characters in width.
 (height . NUMBER)	The frame should be NUMBER text lines high.

You cannot specify either `width' or `height', you must use neither or both.

 (minibuffer . t)	The frame should have a minibuffer.
 (minibuffer . nil)	The frame should have no minibuffer.
 (minibuffer . only)	The frame should contain only a minibuffer.
 (minibuffer . WINDOW)	The frame should use WINDOW as its minibuffer window.

Before the frame is created (via `frame-creation-function'), functions on the
hook `before-make-frame-hook' are run.  After the frame is created, functions
on `after-make-frame-functions' are run with one arg, the newly created frame."
  (interactive)
  (run-hooks 'before-make-frame-hook)
  (let ((frame (funcall frame-creation-function parameters)))
    (run-hook-with-args 'after-make-frame-functions frame)
    frame))

(defun filtered-frame-list (predicate)
  "Return a list of all live frames which satisfy PREDICATE."
  (let* ((frames (frame-list))
	 (list frames))
    (while (consp frames)
      (unless (funcall predicate (car frames))
	(setcar frames nil))
      (setq frames (cdr frames)))
    (delq nil list)))

(defun minibuffer-frame-list ()
  "Return a list of all frames with their own minibuffers."
  (filtered-frame-list
   (function (lambda (frame)
	       (eq frame (window-frame (minibuffer-window frame)))))))

(defun frames-on-display-list (&optional display)
  "Return a list of all frames on DISPLAY.
DISPLAY is a name of a display, a string of the form HOST:SERVER.SCREEN.
If DISPLAY is omitted or nil, it defaults to the selected frame's display."
  (let* ((display (or display (frame-parameter nil 'display)))
	 (func #'(lambda (frame)
		   (eq (frame-parameter frame 'display) display))))
    (filtered-frame-list func)))

(defun framep-on-display (&optional display)
  "Return the type of frames on DISPLAY.
DISPLAY may be a display name or a frame.  If it is a frame, its type is
returned.
If DISPLAY is omitted or nil, it defaults to the selected frame's display.
All frames on a given display are of the same type."
  (or (framep display)
      (framep (car (frames-on-display-list display)))))

(defun frame-remove-geometry-params (param-list)
  "Return the parameter list PARAM-LIST, but with geometry specs removed.
This deletes all bindings in PARAM-LIST for `top', `left', `width',
`height', `user-size' and `user-position' parameters.
Emacs uses this to avoid overriding explicit moves and resizings from
the user during startup."
  (setq param-list (cons nil param-list))
  (let ((tail param-list))
    (while (consp (cdr tail))
      (if (and (consp (car (cdr tail)))
	       (memq (car (car (cdr tail)))
		     '(height width top left user-position user-size)))
	  (progn
	    (setq frame-initial-geometry-arguments
		  (cons (car (cdr tail)) frame-initial-geometry-arguments))
	    (setcdr tail (cdr (cdr tail))))
	(setq tail (cdr tail)))))
  (setq frame-initial-geometry-arguments
	(nreverse frame-initial-geometry-arguments))
  (cdr param-list))


(defcustom focus-follows-mouse t
  "*Non-nil if window system changes focus when you move the mouse.
You should set this variable to tell Emacs how your window manager
handles focus, since there is no way in general for Emacs to find out
automatically."
  :type 'boolean
  :group 'frames
  :version "20.3")

(defun select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (when (eq window-system 'w32)
      (w32-focus-frame frame))
    (cond (focus-follows-mouse
	   (unless (eq window-system 'w32)
	     (set-mouse-position (selected-frame) (1- (frame-width)) 0)))
	  (t
	   (when (eq window-system 'x)
	     (x-focus-frame frame)))))

(defun other-frame (arg)
  "Select the ARG'th different visible frame on current display, and raise it.
All frames are arranged in a cyclic order.
This command selects the frame ARG steps away in that order.
A negative ARG moves in the opposite order.

To make this command work properly, you must tell Emacs
how the system (or the window manager) generally handles
focus-switching between windows.  If moving the mouse onto a window
selects it (gives it focus), set `focus-follows-mouse' to t.
Otherwise, that variable should be nil."
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
    (select-frame-set-input-focus frame)))

(defun make-frame-names-alist ()
  (let* ((current-frame (selected-frame))
	 (falist
	  (cons
	   (cons (frame-parameter current-frame 'name) current-frame) nil))
	 (frame (next-frame nil t)))
    (while (not (eq frame current-frame))
      (progn
	(setq falist (cons (cons (frame-parameter frame 'name) frame) falist))
	(setq frame (next-frame frame t))))
    falist))

(defvar frame-name-history nil)
(defun select-frame-by-name (name)
  "Select the frame on the current terminal whose name is NAME and raise it.
If there is no frame by that name, signal an error."
  (interactive
   (let* ((frame-names-alist (make-frame-names-alist))
	   (default (car (car frame-names-alist)))
	   (input (completing-read
		   (format "Select Frame (default %s): " default)
		   frame-names-alist nil t nil 'frame-name-history)))
     (if (= (length input) 0)
	 (list default)
       (list input))))
  (let* ((frame-names-alist (make-frame-names-alist))
	 (frame (cdr (assoc name frame-names-alist))))
    (or frame
	(error "There is no frame named `%s'" name))
    (make-frame-visible frame)
    (raise-frame frame)
    (select-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (if (eq window-system 'w32)
	(w32-focus-frame frame)
      (when focus-follows-mouse
	(set-mouse-position (selected-frame) (1- (frame-width)) 0)))))

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

(defalias 'set-default-font 'set-frame-font)
(defun set-frame-font (font-name)
  "Set the font of the selected frame to FONT-NAME.
When called interactively, prompt for the name of the font to use.
To get the frame's current default font, use `frame-parameters'."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Font name: "
		       (mapcar #'list
			       ;; x-list-fonts will fail with an error
			       ;; if this frame doesn't support fonts.
			       (x-list-fonts "*" nil (selected-frame)))))))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'font font-name)))
  (run-hooks 'after-setting-font-hook 'after-setting-font-hooks))

(defun set-frame-parameter (frame parameter value)
  (modify-frame-parameters frame (list (cons parameter value))))

(defun set-background-color (color-name)
  "Set the background color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current background color, use `frame-parameters'."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'background-color color-name)))
  (or window-system
      (face-set-after-frame-default (selected-frame))))

(defun set-foreground-color (color-name)
  "Set the foreground color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current foreground color, use `frame-parameters'."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'foreground-color color-name)))
  (or window-system
      (face-set-after-frame-default (selected-frame))))

(defun set-cursor-color (color-name)
  "Set the text cursor color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current cursor color, use `frame-parameters'."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'cursor-color color-name))))

(defun set-mouse-color (color-name)
  "Set the color of the mouse pointer of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current mouse color, use `frame-parameters'."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'mouse-color
				       (or color-name
					   (cdr (assq 'mouse-color
						      (frame-parameters))))))))

(defun set-border-color (color-name)
  "Set the color of the border of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current border color, use `frame-parameters'."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'border-color color-name))))

(defun auto-raise-mode (arg)
  "Toggle whether or not the selected frame should auto-raise.
With arg, turn auto-raise mode on if and only if arg is positive.
Note that this controls Emacs's own auto-raise feature.
Some window managers allow you to enable auto-raise for certain windows.
You can use that for Emacs windows if you wish, but if you do,
that is beyond the control of Emacs and this command has no effect on it."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (cdr (assq 'auto-raise (frame-parameters (selected-frame))))
		-1 1)))
  (if (> arg 0)
      (raise-frame (selected-frame)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'auto-raise (> arg 0)))))

(defun auto-lower-mode (arg)
  "Toggle whether or not the selected frame should auto-lower.
With arg, turn auto-lower mode on if and only if arg is positive.
Note that this controls Emacs's own auto-lower feature.
Some window managers allow you to enable auto-lower for certain windows.
You can use that for Emacs windows if you wish, but if you do,
that is beyond the control of Emacs and this command has no effect on it."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (cdr (assq 'auto-lower (frame-parameters (selected-frame))))
		-1 1)))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'auto-lower (> arg 0)))))
(defun set-frame-name (name)
  "Set the name of the selected frame to NAME.
When called interactively, prompt for the name of the frame.
The frame name is displayed on the modeline if the terminal displays only
one frame, otherwise the name is displayed on the frame's caption bar."
  (interactive "sFrame name: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'name name))))

;;;; Frame/display capabilities.
(defun display-mouse-p (&optional display)
  "Return non-nil if DISPLAY has a mouse available.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((eq frame-type 'pc)
      (msdos-mouse-p))
     ((eq system-type 'windows-nt)
      (> w32-num-mouse-buttons 0))
     ((memq frame-type '(x mac))
      t)    ;; We assume X and Mac *always* have a pointing device
     (t
      (or (and (featurep 'xt-mouse)
	       xterm-mouse-mode)
	  ;; t-mouse is distributed with the GPM package.  It doesn't have
	  ;; a toggle.
	  (featurep 't-mouse))))))

(defun display-popup-menus-p (&optional display)
  "Return non-nil if popup menus are supported on DISPLAY.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display).
Support for popup menus requires that the mouse be available."
  (and
   (let ((frame-type (framep-on-display display)))
     (memq frame-type '(x w32 pc mac)))
   (display-mouse-p display)))

(defun display-graphic-p (&optional display)
  "Return non-nil if DISPLAY is a graphic display.
Graphical displays are those which are capable of displaying several
frames and several different fonts at once.  This is true for displays
that use a window system such as X, and false for text-only terminals.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (not (null (memq (framep-on-display display) '(x w32 mac)))))

(defun display-images-p (&optional display)
  "Return non-nil if DISPLAY can display images.

DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (and (display-graphic-p display)
       (fboundp 'image-mask-p)
       (fboundp 'image-size)
       ;; FIXME: this will need to be revisited when the Windows port
       ;; supports images.
       (not (eq (framep-on-display display) 'w32))))

(defalias 'display-multi-frame-p 'display-graphic-p)
(defalias 'display-multi-font-p 'display-graphic-p)

(defun display-selections-p (&optional display)
  "Return non-nil if DISPLAY supports selections.
A selection is a way to transfer text or other data between programs
via special system buffers called `selection' or `cut buffer' or
`clipboard'.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((eq frame-type 'pc)
      ;; MS-DOG frames support selections when Emacs runs inside
      ;; the Windows' DOS Box.
      (not (null dos-windows-version)))
     ((memq frame-type '(x w32 mac))
      t)    ;; FIXME?
     (t
      nil))))

(defun display-screens (&optional display)
  "Return the number of screens associated with DISPLAY."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32))
      (x-display-screens display))
     (t	;; FIXME: is this correct for the Mac?
      1))))

(defun display-pixel-height (&optional display)
  "Return the height of DISPLAY's screen in pixels.
For character terminals, each character counts as a single pixel."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-pixel-height display))
     (t
      (frame-height (if (framep display) display (selected-frame)))))))

(defun display-pixel-width (&optional display)
  "Return the width of DISPLAY's screen in pixels.
For character terminals, each character counts as a single pixel."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-pixel-width display))
     (t
      (frame-width (if (framep display) display (selected-frame)))))))

(defun display-mm-height (&optional display)
  "Return the height of DISPLAY's screen in millimeters.
If the information is unavailable, value is nil."
  (and (memq (framep-on-display display) '(x w32 mac))
       (x-display-mm-height display)))

(defun display-mm-width (&optional display)
  "Return the width of DISPLAY's screen in millimeters.
If the information is unavailable, value is nil."
  (and (memq (framep-on-display display) '(x w32 mac))
       (x-display-mm-width display)))

(defun display-backing-store (&optional display)
  "Return the backing store capability of DISPLAY's screen.
The value may be `always', `when-mapped', `not-useful', or nil if
the question is inapplicable to a certain kind of display."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-backing-store display))
     (t
      'not-useful))))

(defun display-save-under (&optional display)
  "Return non-nil if DISPLAY's screen supports the SaveUnder feature."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-save-under display))
     (t
      'not-useful))))

(defun display-planes (&optional display)
  "Return the number of planes supported by DISPLAY."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-planes display))
     ((eq frame-type 'pc)
      4)
     (t
      (truncate (log (length (tty-color-alist)) 2))))))

(defun display-color-cells (&optional display)
  "Return the number of color cells supported by DISPLAY."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-color-cells display))
     ((eq frame-type 'pc)
      16)
     (t
      (tty-display-color-cells)))))

(defun display-visual-class (&optional display)
  "Returns the visual class of DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-visual-class display))
     ((and (memq frame-type '(pc t))
	   (tty-display-color-p display))
      'static-color)
     (t
      'static-gray))))


;;;; Aliases for backward compatibility with Emacs 18.
(defalias 'screen-height 'frame-height)
(defalias 'screen-width 'frame-width)

(defun set-screen-width (cols &optional pretend)
  "Obsolete function to change the size of the screen to COLS columns.
Optional second arg non-nil means that redisplay should use COLS columns
but that the idea of the actual width of the frame should not be changed.
This function is provided only for compatibility with Emacs 18; new code
should use `set-frame-width instead'."
  (set-frame-width (selected-frame) cols pretend))

(defun set-screen-height (lines &optional pretend)
  "Obsolete function to change the height of the screen to LINES lines.
Optional second arg non-nil means that redisplay should use LINES lines
but that the idea of the actual height of the screen should not be changed.
This function is provided only for compatibility with Emacs 18; new code
should use `set-frame-height' instead."
  (set-frame-height (selected-frame) lines pretend))

(defun delete-other-frames (&optional frame)
  "Delete all frames except FRAME.
If FRAME uses another frame's minibuffer, the minibuffer frame is
left untouched.  FRAME nil or omitted means use the selected frame."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (let* ((mini-frame (window-frame (minibuffer-window frame)))
	 (frames (delq mini-frame (delq frame (frame-list)))))
    ;; Delete mon-minibuffer-only frames first, because `delete-frame'
    ;; signals an error when trying to delete a mini-frame that's
    ;; still in use by another frame.
    (dolist (frame frames)
      (unless (eq (frame-parameter frame 'minibuffer) 'only)
	(delete-frame frame)))
    ;; Delete minibuffer-only frames.
    (dolist (frame frames)
      (when (eq (frame-parameter frame 'minibuffer) 'only)
	(delete-frame frame)))))


(make-obsolete 'screen-height 'frame-height) ;before 19.15
(make-obsolete 'screen-width  'frame-width) ;before 19.15
(make-obsolete 'set-screen-width 'set-frame-width) ;before 19.15
(make-obsolete 'set-screen-height 'set-frame-height) ;before 19.15


;; Highlighting trailing whitespace.

(make-variable-buffer-local 'show-trailing-whitespace)

(defcustom show-trailing-whitespace nil
  "*Non-nil means highlight trailing whitespace in face `trailing-whitespace'.

Setting this variable makes it local to the current buffer."
  :tag "Highlight trailing whitespace."
  :type 'boolean
  :group 'font-lock)



;; Scrolling

(defgroup scrolling nil
  "Scrolling windows."
  :version "21.1"
  :group 'frames)

(defcustom auto-hscroll-mode t
  "*Allow or disallow automatic scrolling windows horizontally.
If non-nil, windows are automatically scrolled horizontally to make
point visible."
  :version "21.1"
  :type 'boolean
  :group 'scrolling)
(defvaralias 'automatic-hscrolling 'auto-hscroll-mode)


;; Blinking cursor

(defgroup cursor nil
  "Displaying text cursors."
  :version "21.1"
  :group 'frames)

(defcustom blink-cursor-delay 0.5
  "*Seconds of idle time after which cursor starts to blink."
  :tag "Delay in seconds."
  :type 'number
  :group 'cursor)

(defcustom blink-cursor-interval 0.5
  "*Length of cursor blink interval in seconds."
  :tag "Blink interval in seconds."
  :type 'number
  :group 'cursor)

(defvar blink-cursor-idle-timer nil
  "Timer started after `blink-cursor-delay' seconds of Emacs idle time.
The function `blink-cursor-start' is called when the timer fires.")

(defvar blink-cursor-timer nil
  "Timer started from `blink-cursor-start'.
This timer calls `blink-cursor' every `blink-cursor-interval' seconds.")

(defvar blink-cursor-mode nil
  "Non-nil means blinking cursor is active.")

(defun blink-cursor-mode (arg)
  "Toggle blinking cursor mode.
With a numeric argument, turn blinking cursor mode on iff ARG is positive.
When blinking cursor mode is enabled, the cursor of the selected
window blinks.

Note that this command is effective only when Emacs
displays through a window system, because then Emacs does its own
cursor display.  On a text-only terminal, this is not implemented."
  (interactive "P")
  (let ((on-p (if (null arg)
		  (not blink-cursor-mode)
		(> (prefix-numeric-value arg) 0))))
    (if blink-cursor-idle-timer
	(cancel-timer blink-cursor-idle-timer))
    (if blink-cursor-timer
	(cancel-timer blink-cursor-timer))
    (setq blink-cursor-idle-timer nil
	  blink-cursor-timer nil
	  blink-cursor-mode nil)
    (if on-p
	(progn
	  ;; Hide the cursor.
	  ;(internal-show-cursor nil nil)
	  (setq blink-cursor-idle-timer
		(run-with-idle-timer blink-cursor-delay
				     blink-cursor-delay
				     'blink-cursor-start))
	  (setq blink-cursor-mode t))
      (internal-show-cursor nil t))))

;; Note that this is really initialized from startup.el before
;; the init-file is read.

(defcustom blink-cursor nil
  "*Non-nil means blinking cursor mode is active."
  :group 'cursor
  :tag "Blinking cursor"
  :type 'boolean
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (blink-cursor-mode (or value 0))))

(defun blink-cursor-start ()
  "Timer function called from the timer `blink-cursor-idle-timer'.
This starts the timer `blink-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null blink-cursor-timer)
    (add-hook 'pre-command-hook 'blink-cursor-end)
    (setq blink-cursor-timer
	  (run-with-timer blink-cursor-interval blink-cursor-interval
			  'blink-cursor-timer-function))))

(defun blink-cursor-timer-function ()
  "Timer function of timer `blink-cursor-timer'."
  (internal-show-cursor nil (not (internal-show-cursor-p))))

(defun blink-cursor-end ()
  "Stop cursor blinking.
This is installed as a pre-command hook by `blink-cursor-start'.
When run, it cancels the timer `blink-cursor-timer' and removes
itself as a pre-command hook."
  (remove-hook 'pre-command-hook 'blink-cursor-end)
  (internal-show-cursor nil t)
  (cancel-timer blink-cursor-timer)
  (setq blink-cursor-timer nil))



;; Hourglass pointer

(defcustom display-hourglass t
  "*Non-nil means show an hourglass pointer when running under a window system."
  :tag "Hourglass pointer"
  :type 'boolean
  :group 'cursor)

(defcustom hourglass-delay 1
  "*Seconds to wait before displaying an hourglass pointer."
  :tag "Hourglass delay"
  :type 'number
  :group 'cursor)


(defcustom cursor-in-non-selected-windows t
  "*Non-nil means show a hollow box cursor in non-selected-windows.
If nil, don't show a cursor except in the selected window.
Use Custom to set this variable to get the display updated."
  :tag "Cursor in non-selected windows"
  :type 'boolean
  :group 'cursor
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (force-mode-line-update t)))


;;;; Key bindings

(define-key ctl-x-5-map "2" 'make-frame-command)
(define-key ctl-x-5-map "1" 'delete-other-frames)
(define-key ctl-x-5-map "0" 'delete-frame)
(define-key ctl-x-5-map "o" 'other-frame)

(provide 'frame)

;;; frame.el ends here
