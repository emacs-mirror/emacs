;;; erc-sound.el --- CTCP SOUND support for ERC

;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This used to be in erc.el, I (Jorgen) just extracted it from there
;; and put it in this file.  Bugs and features are those of the
;; original author.

;;; Code:

(require 'erc)

;;;###autoload (autoload 'erc-sound-mode "erc-sound")
(define-erc-module sound ctcp-sound
  "In ERC sound mode, the client will respond to CTCP SOUND requests
and play sound files as requested."
  ;; Enable:
  ((define-key erc-mode-map "\C-c\C-s" 'erc-toggle-sound))
  ;; Disable:
  ((define-key erc-mode-map "\C-c\C-s" 'undefined)))

(erc-define-catalog-entry 'english 'CTCP-SOUND "%n (%u@%h) plays %s:%m")

(defgroup erc-sound nil
  "Make ERC play bells and whistles while chatting with people."
  :group 'erc)

(defcustom erc-play-sound t
  "*Play sound on SOUND ctcp requests (used in ICQ chat)."
  :group 'erc-sound
  :type 'boolean)

(defcustom erc-sound-path nil
  "List of directories that contain sound samples to play on SOUND events."
  :group 'erc-sound
  :type '(repeat directory))

(defcustom erc-default-sound nil
  "Play this sound if the requested file was not found."
  :group 'erc-sound
  :type '(choice (const nil)
		 file))

(defcustom erc-play-command "play"
  "Command for playing sound samples."
  :group 'erc-sound
  :type 'string)

(defun erc-cmd-SOUND (line &optional force)
  "Play the sound given in LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*\\)?$" line)
    (let ((file (match-string 1 line))
	  (msg (match-string 2 line))
	  (tgt (erc-default-target)))
      (if (null msg)
	  (setq msg "")
	;; remove the first white space
	(setq msg (substring msg 1)))
      (if tgt
	  (progn
	    (erc-send-ctcp-message tgt (format "SOUND %s %s" file msg) force)
	    (if erc-play-sound (erc-play-sound file)))
	(erc-display-message nil 'error (current-buffer) 'no-target))
      t))
   (t nil)))

(defvar erc-ctcp-query-SOUND-hook '(erc-ctcp-query-SOUND))
(defun erc-ctcp-query-SOUND (proc nick login host to msg)
  (when (string-match "^SOUND\\s-+\\(\\S-+\\)\\(\\(\\s-+.*\\)\\|\\(\\s-*\\)\\)$" msg)
    (let ((sound (match-string 1 msg))
	  (comment (match-string 2 msg)))
      (when erc-play-sound (erc-play-sound sound))
      (erc-display-message
       nil 'notice nil
       'CTCP-SOUND ?n nick ?u login ?h host ?s sound ?m comment)))
  nil)

(defun erc-play-sound (file)
  "Plays a sound file located in one of the directories in `erc-sound-path'
with a command `erc-play-command'."
  (let ((filepath (erc-find-file file erc-sound-path)))
    (if (and (not filepath) erc-default-sound)
	(setq filepath erc-default-sound))
    (cond ((and filepath (file-exists-p filepath))
	   (if (and (fboundp 'device-sound-enabled-p)
		    (device-sound-enabled-p))
	       ; For XEmacs
	       (play-sound-file filepath)
;	     (start-process "erc-sound" nil erc-play-command filepath)
	     (start-process "erc-sound" nil "/bin/tcsh"  "-c"
			    (concat erc-play-command " " filepath))))
	  (t (beep)))
    (erc-log (format "Playing sound file %S" filepath))))

;(defun erc-play-sound (file)
;  "Plays a sound file located in one of the directories in `erc-sound-path'
;   with a command `erc-play-command'."
;  (let ((filepath nil)
;	(paths erc-sound-path))
;    (while (and paths
;		(progn (setq filepath (expand-file-name file (car paths)))
;		       (not (file-exists-p filepath))))
;      (setq paths (cdr paths)))
;    (if (and (not (and filepath (file-exists-p filepath)))
;	     erc-default-sound)
;	(setq filepath erc-default-sound))
;    (cond ((and filepath (file-exists-p filepath))
;;	   (start-process "erc-sound" nil erc-play-command filepath)
;	   (start-process "erc-sound" nil "/bin/tcsh"  "-c"
;			  (concat erc-play-command " " filepath))
;	   )
;	  (t (beep)))
;    (erc-log (format "Playing sound file %S" filepath))))

(defun erc-toggle-sound (&optional arg)
  "Toggles playing sounds on and off.  With positive argument,
  turns them on.  With any other argument turns sounds off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-play-sound t))
	(arg (setq erc-play-sound nil))
	(t (setq erc-play-sound (not erc-play-sound))))
  (message "ERC sound is %s" (if erc-play-sound "ON" "OFF")))


(provide 'erc-sound)

;; arch-tag: 53657d1d-007f-4a20-91c1-588e71cf0cee
;;; erc-sound.el ends here
