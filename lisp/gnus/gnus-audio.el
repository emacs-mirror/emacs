;;; gnus-audio.el --- Sound effects for Gnus
;; Copyright (C) 1996 Free Software Foundation

;; Author: Steven L. Baur <steve@miranova.com>

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
;; This file provides access to sound effects in Gnus.
;; Prerelease:  This file is partially stripped to support earcons.el
;; You can safely ignore most of it until Red Gnus.  **Evil Laugh**
;;; Code:

(when (null (boundp 'running-xemacs))
  (defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)))

(require 'nnheader)
(eval-when-compile (require 'cl))

(defvar gnus-audio-inline-sound
  (and (fboundp 'device-sound-enabled-p)
       (device-sound-enabled-p))
  "When t, we will not spawn a subprocess to play sounds.")

(defvar gnus-audio-directory (nnheader-find-etc-directory "sounds")
  "The directory containing the Sound Files.")

(defvar gnus-audio-au-player "/usr/bin/showaudio"
  "Executable program for playing sun AU format sound files.")

(defvar gnus-audio-wav-player "/usr/local/bin/play"
  "Executable program for playing WAV files.")

;;; The following isn't implemented yet.  Wait for Millennium Gnus.
;(defvar gnus-audio-effects-enabled t
;  "When t, Gnus will use sound effects.")
;(defvar gnus-audio-enable-hooks nil
;  "Functions run when enabling sound effects.")
;(defvar gnus-audio-disable-hooks nil
;  "Functions run when disabling sound effects.")
;(defvar gnus-audio-theme-song nil
;  "Theme song for Gnus.")
;(defvar gnus-audio-enter-group nil
;  "Sound effect played when selecting a group.")
;(defvar gnus-audio-exit-group nil
;  "Sound effect played when exiting a group.")
;(defvar gnus-audio-score-group nil
;  "Sound effect played when scoring a group.")
;(defvar gnus-audio-busy-sound nil
;  "Sound effect played when going into a ... sequence.")


;;;###autoload
					;(defun gnus-audio-enable-sound ()
;  "Enable Sound Effects for Gnus."
;  (interactive)
;  (setq gnus-audio-effects-enabled t)
;  (gnus-run-hooks gnus-audio-enable-hooks))

;;;###autoload
					;(defun gnus-audio-disable-sound ()
;  "Disable Sound Effects for Gnus."
;  (interactive)
;  (setq gnus-audio-effects-enabled nil)
;  (gnus-run-hooks gnus-audio-disable-hooks))

;;;###autoload
(defun gnus-audio-play (file)
  "Play a sound through the speaker."
  (interactive)
  (let ((sound-file (if (file-exists-p file)
			file
		      (concat gnus-audio-directory file))))
    (when (file-exists-p sound-file)
      (if gnus-audio-inline-sound
	  (play-sound-file sound-file)
	(cond ((string-match "\\.wav$" sound-file)
	       (call-process gnus-audio-wav-player
			     sound-file
			     0
			     nil
			     sound-file))
	      ((string-match "\\.au$" sound-file)
	       (call-process gnus-audio-au-player
			     sound-file
			     0
			     nil
			     sound-file)))))))


;;; The following isn't implemented yet, wait for Red Gnus
					;(defun gnus-audio-startrek-sounds ()
;  "Enable sounds from Star Trek the original series."
;  (interactive)
;  (setq gnus-audio-busy-sound "working.au")
;  (setq gnus-audio-enter-group "bulkhead_door.au")
;  (setq gnus-audio-exit-group "bulkhead_door.au")
;  (setq gnus-audio-score-group "ST_laser.au")
;  (setq gnus-audio-theme-song "startrek.au")
;  (add-hook 'gnus-select-group-hook 'gnus-audio-startrek-select-group)
;  (add-hook 'gnus-exit-group-hook 'gnus-audio-startrek-exit-group))
;;;***

(defvar gnus-startup-jingle "Tuxedomoon.Jingle4.au"
  "Name of the Gnus startup jingle file.")

(defun gnus-play-jingle ()
  "Play the Gnus startup jingle, unless that's inhibited."
  (interactive)
  (gnus-audio-play gnus-startup-jingle))

(provide 'gnus-audio)

(run-hooks 'gnus-audio-load-hook)

;;; gnus-audio.el ends here
