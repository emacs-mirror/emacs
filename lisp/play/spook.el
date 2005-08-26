;;; spook.el --- spook phrase utility for overloading the NSA line eater

;; Copyright (C) 1988, 1993 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: games
;; Created: May 1987

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

;; Steve Strassmann <straz@media-lab.media.mit.edu> didn't write
;; this, and even if he did, he really didn't mean for you to use it
;; in an anarchistic way.
;;
;; To use this:
;;  Just before sending mail, do M-x spook.
;;  A number of phrases will be inserted into your buffer, to help
;;  give your message that extra bit of attractiveness for automated
;;  keyword scanners.  Help defeat the NSA trunk trawler!

;;; Code:

(require 'cookie1)

; Variables
(defgroup spook nil
  "Spook phrase utility for overloading the NSA line eater."
  :prefix "spook-"
  :group 'games)

(defcustom spook-phrases-file (expand-file-name "spook.lines" data-directory)
  "Keep your favorite phrases here."
  :type 'file
  :group 'spook)

(defcustom spook-phrase-default-count 15
  "Default number of phrases to insert."
  :type 'integer
  :group 'spook)

;;;###autoload
(defun spook ()
  "Adds that special touch of class to your outgoing mail."
  (interactive)
  (cookie-insert spook-phrases-file
		 spook-phrase-default-count
		 "Checking authorization..."
		 "Checking authorization...Approved"))

;;;###autoload
(defun snarf-spooks ()
  "Return a vector containing the lines from `spook-phrases-file'."
  (cookie-snarf spook-phrases-file
		"Checking authorization..."
		"Checking authorization...Approved"))

;; Note: the implementation that used to take up most of this file has been
;; cleaned up, generalized, gratuitously broken by esr, and now resides in
;; cookie1.el.

(provide 'spook)

;;; arch-tag: c682b61f-92b6-4492-9c0d-2367e562449c
;;; spook.el ends here
