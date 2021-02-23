;;; yow.el --- quote random zippyisms  -*- lexical-binding: t; -*-

;; Copyright (C) 1993-1995, 2000-2021 Free Software Foundation, Inc.

;; Author: Richard Mlynarik
;; Maintainer: emacs-devel@gnu.org
;; Keywords: games
;; Obsolete-since: 24.4

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Important pinheadery for GNU Emacs.
;; This file is obsolete.  For similar functionality, see
;; fortune.el and cookie1.el.

;;; Code:

(require 'cookie1)

(defgroup yow nil
  "Quote random zippyisms."
  :prefix "yow-"
  :group 'games)

(defcustom yow-file (expand-file-name "yow.lines" data-directory)
   "File containing pertinent pinhead phrases."
  :type 'file)

(defconst yow-load-message "Am I CONSING yet?...")
(defconst yow-after-load-message "I have SEEN the CONSING!!")

;;;###autoload
(defun yow (&optional insert display)
  "Return or display a random Zippy quotation.  With prefix arg, insert it."
  (interactive "P\np")
  (let ((yow (cookie yow-file yow-load-message yow-after-load-message)))
    (cond (insert
	   (insert yow))
	  ((not display)
	   yow)
	  (t
	   (message "%s" yow)))))

(defsubst read-zippyism (prompt &optional require-match)
  "Read a Zippyism from the minibuffer with completion, prompting with PROMPT.
If optional second arg is non-nil, require input to match a completion."
  (cookie-read prompt yow-file yow-load-message yow-after-load-message
	       require-match))

;;;###autoload
(defun insert-zippyism (&optional zippyism)
  "Prompt with completion for a known Zippy quotation, and insert it at point."
  (interactive (list (read-zippyism "Pinhead wisdom: " t)))
  (insert zippyism))

;;;###autoload
(defun apropos-zippy (regexp)
  "Return a list of all Zippy quotes matching REGEXP.
If called interactively, display a list of matches."
  (interactive "sApropos Zippy (regexp): ")
  (cookie-apropos regexp yow-file (called-interactively-p 'interactive)))


;; Yowza!! Feed zippy quotes to the doctor. Watch results.
;; fun, fun, fun. Entertainment for hours...
;;
;; written by Kayvan Aghaiepour

(declare-function doctor-ret-or-read "doctor" (arg))

;;;###autoload
(defun psychoanalyze-pinhead ()
  "Zippy goes to the analyst."
  (interactive)
  (cookie-doctor yow-file))

(provide 'yow)

;;; yow.el ends here
