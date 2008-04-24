;;; gnus-audio.el --- Sound effects for Gnus

;; Copyright (C) 1996, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Steven L. Baur <steve@miranova.com>
;; Keywords: news, mail, multimedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; This file provides access to sound effects in Gnus.
;; This file is partially stripped to support earcons.el.

;;; Code:

(require 'nnheader)

(defgroup gnus-audio nil
  "Playing sound in Gnus."
  :version "21.1"
  :group 'gnus-visual
  :group 'multimedia)

(defvar gnus-audio-inline-sound
  (or (if (fboundp 'device-sound-enabled-p)
	  (device-sound-enabled-p))	; XEmacs
      (fboundp 'play-sound))		; Emacs 21
  "Non-nil means try to play sounds without using an external program.")

(defcustom gnus-audio-directory (nnheader-find-etc-directory "sounds")
  "The directory containing the Sound Files."
  :type '(choice directory (const nil))
  :group 'gnus-audio)

(defcustom gnus-audio-au-player (executable-find "play")
  "Executable program for playing sun AU format sound files."
  :group 'gnus-audio
  :type '(choice file (const nil)))

(defcustom gnus-audio-wav-player (executable-find "play")
  "Executable program for playing WAV files."
  :group 'gnus-audio
  :type '(choice file (const nil)))

;;; The following isn't implemented yet.  Wait for Millennium Gnus.
;;(defvar gnus-audio-effects-enabled t
;;  "When t, Gnus will use sound effects.")
;;(defvar gnus-audio-enable-hooks nil
;;  "Functions run when enabling sound effects.")
;;(defvar gnus-audio-disable-hooks nil
;;  "Functions run when disabling sound effects.")
;;(defvar gnus-audio-theme-song nil
;;  "Theme song for Gnus.")
;;(defvar gnus-audio-enter-group nil
;;  "Sound effect played when selecting a group.")
;;(defvar gnus-audio-exit-group nil
;;  "Sound effect played when exiting a group.")
;;(defvar gnus-audio-score-group nil
;;  "Sound effect played when scoring a group.")
;;(defvar gnus-audio-busy-sound nil
;;  "Sound effect played when going into a ... sequence.")


;;;###autoload
;;(defun gnus-audio-enable-sound ()
;;  "Enable Sound Effects for Gnus."
;;  (interactive)
;;  (setq gnus-audio-effects-enabled t)
;;  (gnus-run-hooks gnus-audio-enable-hooks))

;;;###autoload
					;(defun gnus-audio-disable-sound ()
;;  "Disable Sound Effects for Gnus."
;;  (interactive)
;;  (setq gnus-audio-effects-enabled nil)
;;  (gnus-run-hooks gnus-audio-disable-hooks))

;;;###autoload
(defun gnus-audio-play (file)
  "Play a sound FILE through the speaker."
  (interactive "fSound file name: ")
  (let ((sound-file (if (file-exists-p file)
			file
		      (expand-file-name file gnus-audio-directory))))
    (when (file-exists-p sound-file)
      (cond ((and gnus-audio-inline-sound
		 (condition-case nil
		     ;; Even if we have audio, we may fail with the
		     ;; wrong sort of sound file.
		     (progn (play-sound-file sound-file)
			    t)
		   (error nil))))
	    ;; If we don't have built-in sound, or playing it failed,
	    ;; try with external program.
	    ((equal "wav" (file-name-extension sound-file))
	     (call-process gnus-audio-wav-player
			   sound-file
			   0
			   nil
			   sound-file))
	    ((equal "au" (file-name-extension sound-file))
	     (call-process gnus-audio-au-player
			   sound-file
			   0
			   nil
			   sound-file))))))


;;; The following isn't implemented yet, wait for Red Gnus
;;(defun gnus-audio-startrek-sounds ()
;;  "Enable sounds from Star Trek the original series."
;;  (interactive)
;;  (setq gnus-audio-busy-sound "working.au")
;;  (setq gnus-audio-enter-group "bulkhead_door.au")
;;  (setq gnus-audio-exit-group "bulkhead_door.au")
;;  (setq gnus-audio-score-group "ST_laser.au")
;;  (setq gnus-audio-theme-song "startrek.au")
;;  (add-hook 'gnus-select-group-hook 'gnus-audio-startrek-select-group)
;;  (add-hook 'gnus-exit-group-hook 'gnus-audio-startrek-exit-group))
;;;***

(defvar gnus-startup-jingle "Tuxedomoon.Jingle4.au"
  "Name of the Gnus startup jingle file.")

(defun gnus-play-jingle ()
  "Play the Gnus startup jingle, unless that's inhibited."
  (interactive)
  (gnus-audio-play gnus-startup-jingle))

(provide 'gnus-audio)

(run-hooks 'gnus-audio-load-hook)

;; arch-tag: 6f129e78-3416-4fc9-973f-6cf5ac8d654b
;;; gnus-audio.el ends here
