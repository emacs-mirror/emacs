;;; screen.el --- multi-screen management independent of window systems.

;;;; Copyright (C) 1990, 1992 Free Software Foundation, Inc.

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
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

(defvar screen-creation-function nil
  "Window-system dependent function to call to create a new screen.
The window system startup file should set this to its screen creation
function, which should take an alist of parameters as its argument.")

;;; The default value for this must ask for a minibuffer.  There must
;;; always exist a screen with a minibuffer, and after we delete the
;;; terminal screen, this will be the only screen.
(defvar initial-screen-alist '((minibuffer . nil))
  "Alist of values used when creating the initial emacs text screen.
These may be set in your init file, like this:
 (setq initial-screen-alist '((top . 1) (left . 1) (width . 80) (height . 55)))
These supercede the values given in screen-default-alist.")

(defvar minibuffer-screen-alist nil
  "Alist of values to apply to a minibuffer screen.
These may be set in your init file, like this:
 (setq minibuffer-screen-alist
   '((top . 1) (left . 1) (width . 80) (height . 1)))
These supercede the values given in default-screen-alist.")

(defvar pop-up-screen-alist nil
  "Alist of values used when creating pop-up screens.
Pop-up screens are used for completions, help, and the like.
This variable can be set in your init file, like this:
  (setq pop-up-screen-alist '((width . 80) (height . 20)))
These supercede the values given in default-screen-alist.")

(setq pop-up-screen-function
      (function (lambda ()
		  (new-screen pop-up-screen-alist))))


;;;; Arrangement of screens at startup

;;; 1) Load the window system startup file from the lisp library and read the
;;; high-priority arguments (-q and the like).  The window system startup
;;; file should create any screens specified in the window system defaults.
;;; 
;;; 2) If no screens have been opened, we open an initial text screen.
;;;
;;; 3) Once the init file is done, we apply any newly set parameters
;;; in initial-screen-alist to the screen.

(add-hook 'before-init-hook 'screen-initialize)
(add-hook 'window-setup-hook 'screen-notice-user-settings)

;;; If we create the initial screen, this is it.
(defvar screen-initial-screen nil)

;;; startup.el calls this function before loading the user's init
;;; file - if there is no screen with a minibuffer open now, create
;;; one to display messages while loading the init file.
(defun screen-initialize ()
  
  ;; Are we actually running under a window system at all?
  (if (and window-system (not noninteractive))
      (let ((screens (screen-list)))
    
	;; Look for a screen that has a minibuffer.
	(while (and screens
		    (or (eq (car screens) terminal-screen)
			(not (cdr (assq 'minibuffer
					(screen-parameters
					 (car screens)))))))
	  (setq screens (cdr screens)))

	;; If there was none, then we need to create the opening screen.
	(or screens
	    (setq default-minibuffer-screen
		  (setq screen-initial-screen
			(new-screen initial-screen-alist))))
    
	;; At this point, we know that we have a screen open, so we 
	;; can delete the terminal screen.
	(delete-screen terminal-screen)
	(setq terminal-screen nil))
    
    ;; No, we're not running a window system.  Arrange to cause errors.
    (setq screen-creation-function
	  (function
	   (lambda (parameters)
	     (error
	      "Can't create multiple screens without a window system."))))))
					
;;; startup.el calls this function after loading the user's init file.
;;; If we created a minibuffer before knowing if we had permission, we
;;; need to see if it should go away or change.  Create a text screen
;;; here.
(defun screen-notice-user-settings ()
  (if screen-initial-screen
      (progn
	
	;; If the user wants a minibuffer-only screen, we'll have to
	;; make a new one; you can't remove or add a root window to/from
	;; an existing screen.
	(if (eq (cdr (or (assq 'minibuffer initial-screen-alist)
			 '(minibuffer . t)))
		     'only)
	    (progn
	      (setq default-minibuffer-screen
		    (new-screen
		     (append initial-screen-alist
			     (screen-parameters screen-initial-screen))))
	      (delete-screen screen-initial-screen))
	  (modify-screen-parameters screen-initial-screen
				    initial-screen-alist))))

  ;; Make sure the initial screen can be GC'd if it is ever deleted.
  (makunbound 'screen-initial-screen))


;;;; Creation of additional screens

;;; Return some screen other than the current screen,
;;; creating one if neccessary.  Note that the minibuffer screen, if
;;; separate, is not considered (see next-screen).
(defun get-screen ()
  (let ((s (if (equal (next-screen (selected-screen)) (selected-screen))
	       (new-screen)
	     (next-screen (selected-screen)))))
    s))

(defun next-multiscreen-window ()
  "Select the next window, regardless of which screen it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      t)))

(defun previous-multiscreen-window ()
  "Select the previous window, regardless of which screen it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  t)))

(defun new-screen (&optional parameters)
  "Create a new screen, displaying the current buffer.

Optional argument PARAMETERS is an alist of parameters for the new
screen.  Specifically, PARAMETERS is a list of pairs, each having one
of the following forms:

(name . STRING)	- The screen should be named STRING.

(height . NUMBER) - The screen should be NUMBER text lines high.  If
	this parameter is present, the width parameter must also be
	given.

(width . NUMBER) - The screen should be NUMBER characters in width.
	If this parameter is present, the height parameter must also
	be given.

(minibuffer . t) - the screen should have a minibuffer
(minibuffer . none) - the screen should have no minibuffer
(minibuffer . only) - the screen should contain only a minibuffer
(minibuffer . WINDOW) - the screen should use WINDOW as its minibuffer window.

(NAME . VALUE), specifying the parameter and the value it should have.
NAME should be one of the following symbols:
  name		VALUE 

The documentation for the function x-create-screen describes
additional screen parameters that Emacs will recognize when running
under the X Window System."
  (interactive)
  (funcall screen-creation-function parameters))


;;;; Iconification

;;; A possible enhancement for the below: if you iconify a surrogate
;;; minibuffer screen, iconify all of its minibuffer's users too; 
;;; de-iconify them as a group.  This will need to wait until screens
;;; have mapping and unmapping hooks.

(defun iconify ()
  "Iconify or deiconify the selected screen."
  (interactive)
  (let ((screen (selected-screen)))
    (if (eq (screen-visible-p screen) t)
	(iconify-screen screen)
      (deiconify-screen screen))))


;;;; Convenience functions for dynamically changing screen parameters

(defun set-screen-height (h)
  (interactive "NHeight: ")
  (let* ((screen (selected-screen))
	 (width (cdr (assoc 'width (screen-parameters (selected-screen))))))
    (set-screen-size (selected-screen) width h)))

(defun set-screen-width (w)
  (interactive "NWidth: ")
  (let* ((screen (selected-screen))
	 (height (cdr (assoc 'height (screen-parameters (selected-screen))))))
    (set-screen-size (selected-screen) w height)))

(defun set-default-font (font-name)
  (interactive "sFont name: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'font font-name))))

(defun set-screen-background (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'background-color color-name))))

(defun set-screen-foreground (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'foreground-color color-name))))

(defun set-cursor-color (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'cursor-color color-name))))

(defun set-pointer-color (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'mouse-color color-name))))

(defun set-auto-raise (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'auto-raise toggle))))

(defun set-auto-lower (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'auto-lower toggle))))

(defun set-vertical-bar (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'vertical-scroll-bar toggle))))

(defun set-horizontal-bar (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'horizontal-scroll-bar toggle))))

;;;; Key bindings
(define-prefix-command 'ctl-x-3-map)
(define-key ctl-x-map "3" 'ctl-x-3-map)

(define-key ctl-x-3-map "2" 'new-screen)
(define-key ctl-x-3-map "0" 'delete-screen)

(provide 'screen)

;;; screen.el ends here
