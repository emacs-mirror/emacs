;;; help-macro.el --- Makes command line help such as help-for-help

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Lynn Slater <lrs@indetech.com>
;; Created: : Mon Oct  1 11:42:39 1990
;; Adapted-By: ESR

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
;;
;; 	This file supplies the macro make-help-screen which constructs
;; single character dispatching with browsable help such as that provided
;; by help-for-help. This can be used to make many modes easier to use; for
;; example, the Gnu Emacs Empire Tool uses this for every "nested" mode map
;; called from the main mode map.

;;       The name of this package was changed from help-screen.el to
;; help-macro.el in order to fit in a 14-character limit.

;;-> ***********************  Example of use *********************************

;;->(make-help-screen help-for-empire-redistribute-map
;;->		  "c:civ m:mil p:population f:food ?"
;;->		  "You have discovered the GEET redistribution commands
;;->   From here, you can use the following options:
;;->
;;->c	Redistribute civs from overfull sectors into connected underfull ones
;;->	  The functions typically named by empire-ideal-civ-fcn control
;;->          based in part on empire-sector-civ-threshold  
;;->m	Redistribute military using levels given by empire-ideal-mil-fcn
;;->p	Redistribute excess population to highways for max pop growth
;;->	  Excess is any sector so full babies will not be born.
;;->f	Even out food on highways to highway min and leave levels
;;->	  This is good to pump max food to all warehouses/dist pts
;;->
;;->
;;->Use \\[help-for-empire-redistribute-map] for help on redistribution.
;;->Use \\[help-for-empire-extract-map] for help on data extraction.
;;->Please use \\[describe-key] to find out more about any of the other keys."
;;->		  empire-shell-redistribute-map)

;;->  (define-key c-mp "\C-h" 'help-for-empire-redistribute-map)
;;->  (define-key c-mp help-character 'help-for-empire-redistribute-map)

;;; Change Log:
;;
;; 22-Jan-1991		Lynn Slater x2048	
;;    Last Modified: Mon Oct  1 11:43:52 1990 #3 (Lynn Slater)
;;    documented better

;;; Code:

(provide 'help-macro)
(require 'backquote)

;;;###autoload
(defvar three-step-help nil
  "*Non-nil means give more info about Help command in three steps.
The three steps are simple prompt, prompt with all options,
and window listing and describing the options.
A value of nil means skip the middle step, so that
\\[help-command] \\[help-command] gives the window that lists the options.")

(defmacro make-help-screen (fname help-line help-text helped-map)
  "Construct help-menu function name FNAME.
When invoked, FNAME shows HELP-LINE and reads a command using HELPED-MAP.
If the command is the help character, FNAME displays HELP-TEXT
and continues trying to read a command using HELPED-MAP.
When FNAME finally does get a command, it executes that command
and then returns."
  (` (defun (, fname) ()
	   (, help-text)
	   (interactive)
	   (let ((line-prompt
		  (substitute-command-keys (, help-line))))
	     (if three-step-help
		 (message line-prompt))
	     (let* ((help-screen (documentation (quote (, fname))))
		    ;; We bind overriding-local-map for very small
		    ;; sections, *excluding* where we switch buffers
		    ;; and where we execute the chosen help command.
		    (local-map (make-sparse-keymap))
		    (minor-mode-map-alist nil)
		    (prev-frame (selected-frame))
		    config new-frame key char)
	       (unwind-protect
		   (progn
		     (setcdr local-map (, helped-map))
		     (define-key local-map [t] 'undefined)
		     (if three-step-help
			 (progn
			   (setq key (let ((overriding-local-map local-map))
				     (read-key-sequence nil)))
			   ;; Make the HELP key translate to C-h.
			   (if (lookup-key function-key-map key)
			       (setq key (lookup-key function-key-map key)))
			   (setq char (aref key 0)))
		       (setq char ??))
		     (if (or (eq char ??) (eq char help-char))
			 (progn
			   (setq config (current-window-configuration))
			   (switch-to-buffer-other-window "*Help*")
			   (and (fboundp 'make-frame)
				(not (eq (window-frame (selected-window))
					 prev-frame))
				(setq new-frame (window-frame (selected-window))
				      config nil))
			   (erase-buffer)
			   (insert help-screen)
			   (help-mode)
			   (goto-char (point-min))
			   (while (or (memq char (cons help-char '(?? ?\C-v ?\ ?\177 delete ?\M-v)))
				      (eq (car-safe char) 'switch-frame)
				      (equal key "\M-v"))
			     (condition-case nil
				 (progn
				   (if (eq (car-safe char) 'switch-frame)
				       (handle-switch-frame char))
				   (if (memq char '(?\C-v ?\ ))
				       (scroll-up))
				   (if (or (memq char '(?\177 ?\M-v delete))
					   (equal key "\M-v"))
				       (scroll-down)))
			       (error nil))
			     (let ((cursor-in-echo-area t)
				   (overriding-local-map local-map))
			       (setq key (read-key-sequence
					  (format "Type one of the options listed%s: "
						  (if (pos-visible-in-window-p
						       (point-max))
						      "" " or Space to scroll")))
				     char (aref key 0))))))
		     ;; Mouse clicks are not part of the help feature,
		     ;; so reexecute them in the standard environment.
		     (if (listp char)
			 (setq unread-command-events
			       (cons char unread-command-events)
			       config nil)
		       (let ((defn (lookup-key local-map key)))
			 (if defn
			     (progn
			       (if config
				   (progn
				     (set-window-configuration config)
				     (setq config nil)))
			       (if new-frame
				   (progn (iconify-frame new-frame)
					  (setq new-frame nil)))
			       (call-interactively defn))
			   (ding)))))
		 (if new-frame (iconify-frame new-frame))
		 (if config
		     (set-window-configuration config))))))
     ))

;;; help-macro.el

