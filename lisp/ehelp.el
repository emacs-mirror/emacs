;;; ehelp.el --- bindings for electric-help mode

;; Copyright (C) 1986, 1995 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, extensions

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides a pre-packaged `Electric Help Mode' for
;; browsing on-line help screens.  There is one entry point,
;; `with-electric-help'; All you have to give it is a no-argument
;; function that generates the actual text of the help into the current
;; buffer.

;;; Code:

(require 'electric)
(defvar electric-help-map ()
  "Keymap defining commands available in `electric-help-mode'.")

(put 'electric-help-undefined 'suppress-keymap t)
(if electric-help-map
    ()
  (let ((map (make-keymap)))
    ;; allow all non-self-inserting keys - search, scroll, etc
    (suppress-keymap map)
    (define-key map (char-to-string help-char) 'electric-help-help)
    (define-key map "?" 'electric-help-help)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    ;(define-key map "\C-g" 'electric-help-exit)
    (define-key map "q" 'electric-help-exit)
    (define-key map "Q" 'electric-help-exit)
    ;;a better key than this?
    (define-key map "r" 'electric-help-retain)
    (define-key map "R" 'electric-help-retain)

    (setq electric-help-map map)))
   
(defun electric-help-mode ()
  "`with-electric-help' temporarily places its buffer in this mode.
\(On exit from `with-electric-help', the buffer is put in `default-major-mode'.)"
  (setq buffer-read-only t)
  (setq mode-name "Help")
  (setq major-mode 'help)
  (setq mode-line-buffer-identification '(" Help:  %b"))
  (use-local-map electric-help-map)
  ;; this is done below in with-electric-help
  ;(run-hooks 'electric-help-mode-hook)
  )

(defun with-electric-help (thunk &optional buffer noerase)
  "Arguments are THUNK &optional BUFFER NOERASE.  BUFFER defaults to \"*Help*\"
THUNK is a function of no arguments which is called to initialize
the contents of BUFFER.  BUFFER will be erased before THUNK is called unless
NOERASE is non-nil.  THUNK will be called with `standard-output' bound to
the buffer specified by BUFFER

After THUNK has been called, this function \"electrically\" pops up a window
in which BUFFER is displayed and allows the user to scroll through that buffer
in electric-help-mode.
When the user exits (with `electric-help-exit', or otherwise) the help
buffer's window disappears (i.e., we use `save-window-excursion')
BUFFER is put into `default-major-mode' (or `fundamental-mode') when we exit"
  (setq buffer (get-buffer-create (or buffer "*Help*")))
  (let ((one (one-window-p t))
	(config (current-window-configuration))
        (bury nil))
    (unwind-protect
         (save-excursion
           (if one (goto-char (window-start (selected-window))))
           (let ((pop-up-windows t))
             (pop-to-buffer buffer))
           (save-excursion
             (set-buffer buffer)
             (electric-help-mode)
             (setq buffer-read-only nil)
             (or noerase (erase-buffer)))
           (let ((standard-output buffer))
             (if (not (funcall thunk))
                 (progn
                   (set-buffer buffer)
                   (set-buffer-modified-p nil)
                   (goto-char (point-min))
                   (if one (shrink-window-if-larger-than-buffer (selected-window))))))
           (set-buffer buffer)
           (run-hooks 'electric-help-mode-hook)
           (if (eq (car-safe (electric-help-command-loop))
                   'retain)
               (setq config (current-window-configuration))
               (setq bury t)))
      (message "")
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (condition-case ()
          (funcall (or default-major-mode 'fundamental-mode))
        (error nil))
      (set-window-configuration config)
      (if bury
          (progn
            ;;>> Perhaps this shouldn't be done.
            ;; so that when we say "Press space to bury" we mean it
            (replace-buffer-in-windows buffer)
            ;; must do this outside of save-window-excursion
            (bury-buffer buffer))))))

(defun electric-help-command-loop ()
  (catch 'exit
    (if (pos-visible-in-window-p (point-max))
	(progn (message (substitute-command-keys "<<< Press Space to bury the help buffer, Press \\[electric-help-retain] to retain it >>>"))
	       (if (equal (setq unread-command-events (list (read-event)))
			  '(?\ ))
		   (progn (setq unread-command-events nil)
			  (throw 'exit t)))))
    (let (up down both neither
	  (standard (and (eq (key-binding " ")
			     'scroll-up)
			 (eq (key-binding "\^?")
			     'scroll-down)
			 (eq (key-binding "q")
			     'electric-help-exit)
			 (eq (key-binding "r")
			     'electric-help-retain))))
      (Electric-command-loop
        'exit
	(function (lambda ()
	  (let ((min (pos-visible-in-window-p (point-min)))
		(max (pos-visible-in-window-p (point-max))))
	    (cond ((and min max)
		   (cond (standard "Press q to exit, r to retain ")
			 (neither)
			 (t (setq neither (substitute-command-keys "Press \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (min
		   (cond (standard "Press SPC to scroll, q to exit, r to retain ")
			 (up)
			 (t (setq up (substitute-command-keys "Press \\[scroll-up] to scroll, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (max
		   (cond (standard "Press DEL to scroll back, q to exit ")
			 (down)
			 (t (setq down (substitute-command-keys "Press \\[scroll-down] to scroll back, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (t
		   (cond (standard "Press SPC to scroll, DEL to scroll back, q to exit ")
			 (both)
			 (t (setq both (substitute-command-keys "Press \\[scroll-up] to scroll, \\[scroll-down] to scroll back, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))))))
		    t))))



;(defun electric-help-scroll-up (arg)
;  ">>>Doc"
;  (interactive "P")
;  (if (and (null arg) (pos-visible-in-window-p (point-max)))
;      (electric-help-exit)
;    (scroll-up arg)))

(defun electric-help-exit ()
  ">>>Doc"
  (interactive)
  (throw 'exit t))

(defun electric-help-retain ()
  "Exit `electric-help', retaining the current window/buffer configuration.
\(The *Help* buffer will not be selected, but \\[switch-to-buffer-other-window] RET
will select it.)"
  (interactive)
  (throw 'exit '(retain)))


(defun electric-help-undefined ()
  (interactive)
  (error "%s is undefined -- Press %s to exit"
	 (mapconcat 'single-key-description (this-command-keys) " ")
	 (if (eq (key-binding "Q") 'electric-help-exit)
	     "Q"
	   (substitute-command-keys "\\[electric-help-exit]"))))


;>>> this needs to be hairified (recursive help, anybody?)
(defun electric-help-help ()
  (interactive)
  (if (and (eq (key-binding "q") 'electric-help-exit)
	   (eq (key-binding " ") 'scroll-up)
	   (eq (key-binding "\^?") 'scroll-down)
	   (eq (key-binding "r") 'electric-help-retain))
      (message "SPC scrolls up, DEL scrolls down, q exits burying help buffer, r exits")
    (message "%s" (substitute-command-keys "\\[scroll-up] scrolls up, \\[scroll-down] scrolls down, \\[electric-help-exit] exits burying help buffer, \\[electric-help-retain] exits")))
  (sit-for 2))


(defun electric-helpify (fun)
  (let ((name "*Help*"))
    (if (save-window-excursion
	  ;; kludge-o-rama
	  (let* ((p (symbol-function 'print-help-return-message))
		 (b (get-buffer name))
		 (m (buffer-modified-p b)))
	    (and b (not (get-buffer-window b))
		 (setq b nil))
	    (unwind-protect
		(progn
		  (message "%s..." (capitalize (symbol-name fun)))
		  ;; with-output-to-temp-buffer marks the buffer as unmodified.
		  ;; kludging excessively and relying on that as some sort
		  ;;  of indication leads to the following abomination...
		  ;;>> This would be doable without such icky kludges if either
		  ;;>> (a) there were a function to read the interactive
		  ;;>>     args for a command and return a list of those args.
		  ;;>>     (To which one would then just apply the command)
		  ;;>>     (The only problem with this is that interactive-p
		  ;;>>      would break, but that is such a misfeature in
		  ;;>>      any case that I don't care)
		  ;;>>     It is easy to do this for emacs-lisp functions;
		  ;;>>     the only problem is getting the interactive spec
		  ;;>>     for subrs
		  ;;>> (b) there were a function which returned a
		  ;;>>     modification-tick for a buffer.  One could tell
		  ;;>>     whether a buffer had changed by whether the
		  ;;>>     modification-tick were different.
		  ;;>>     (Presumably there would have to be a way to either
		  ;;>>      restore the tick to some previous value, or to
		  ;;>>      suspend updating of the tick in order to allow
		  ;;>>      things like momentary-string-display)
		  (and b
		       (save-excursion
			 (set-buffer b)
			 (set-buffer-modified-p t)))
		  (fset 'print-help-return-message 'ignore)
		  (call-interactively fun)
		  (and (get-buffer name)
		       (get-buffer-window (get-buffer name))
		       (or (not b)
			   (not (eq b (get-buffer name)))
			   (not (buffer-modified-p b)))))
	      (fset 'print-help-return-message p)
	      (and b (buffer-name b)
		   (save-excursion
		     (set-buffer b)
		     (set-buffer-modified-p m))))))
	(with-electric-help 'ignore name t))))


(defun electric-describe-key ()
  (interactive)
  (electric-helpify 'describe-key))

(defun electric-describe-mode ()
  (interactive)
  (electric-helpify 'describe-mode))

(defun electric-view-lossage ()
  (interactive)
  (electric-helpify 'view-lossage))

;(defun electric-help-for-help ()
;  "See help-for-help"
;  (interactive)
;  )

(defun electric-describe-function ()
  (interactive)
  (electric-helpify 'describe-function))

(defun electric-describe-variable ()
  (interactive)
  (electric-helpify 'describe-variable))

(defun electric-describe-bindings ()
  (interactive)
  (electric-helpify 'describe-bindings))

(defun electric-describe-syntax ()
  (interactive)
  (electric-helpify 'describe-syntax))

(defun electric-command-apropos ()
  (interactive)
  (electric-helpify 'command-apropos))

;(define-key help-map "a" 'electric-command-apropos)

(defun electric-apropos ()
  (interactive)
  (electric-helpify 'apropos))


;;;; ehelp-map

(defvar ehelp-map ())
(if ehelp-map
    nil
  (let ((map (copy-keymap help-map))) 
    (substitute-key-definition 'command-apropos 'electric-command-apropos map)
    (substitute-key-definition 'describe-key 'electric-describe-key map)
    (substitute-key-definition 'describe-mode 'electric-describe-mode map)
    (substitute-key-definition 'view-lossage 'electric-view-lossage map)
    (substitute-key-definition 'describe-function 'electric-describe-function map)
    (substitute-key-definition 'describe-variable 'electric-describe-variable map)
    (substitute-key-definition 'describe-bindings 'electric-describe-bindings map)
    (substitute-key-definition 'describe-syntax 'electric-describe-syntax map)

    (setq ehelp-map map)
    (fset 'ehelp-command map)))

;; Do (define-key global-map "\C-h" 'ehelp-command) if you want to win

(provide 'ehelp) 

;;; ehelp.el ends here
