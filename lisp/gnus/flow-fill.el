;;; flow-fill.el --- interprete RFC2646 "flowed" text

;; Copyright (C) 2000, 2002 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: mail

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

;; This implement decoding of RFC2646 formatted text, including the
;; quoted-depth wins rules.

;; Theory of operation: search for lines ending with SPC, save quote
;; length of line, remove SPC and concatenate line with the following
;; line if quote length of following line matches current line.

;; When no further concatenations are possible, we've found a
;; paragraph and we let `fill-region' fill the long line into several
;; lines with the quote prefix as `fill-prefix'.

;; Todo: encoding, implement basic `fill-region' (Emacs and XEmacs
;;       implementations differ..)

;; History:

;; 2000-02-17  posted on ding mailing list
;; 2000-02-19  use `point-at-{b,e}ol' in XEmacs
;; 2000-03-11  no compile warnings for point-at-bol stuff
;; 2000-03-26  committed to gnus cvs
;; 2000-10-23  don't flow "-- " lines, make "quote-depth wins" rule
;;             work when first line is at level 0.

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  (defalias 'fill-flowed-point-at-bol
	(if (fboundp 'point-at-bol)
	    'point-at-bol
	  'line-beginning-position))

   (defalias 'fill-flowed-point-at-eol
	(if (fboundp 'point-at-eol)
	    'point-at-eol
	  'line-end-position)))

(defun fill-flowed (&optional buffer)
  (save-excursion
    (set-buffer (or (current-buffer) buffer))
    (goto-char (point-min))
    (while (re-search-forward " $" nil t)
      (when (save-excursion
	      (beginning-of-line)
	      (looking-at "^\\(>*\\)\\( ?\\)"))
	(let ((quote (match-string 1)) sig)
	  (if (string= quote "")
	      (setq quote nil))
	  (when (and quote (string= (match-string 2) ""))
	    (save-excursion
	      ;; insert SP after quote for pleasant reading of quoted lines
	      (beginning-of-line)
	      (when (> (skip-chars-forward ">") 0)
		(insert " "))))
	  (while (and (save-excursion
			(ignore-errors (backward-char 3))
			(setq sig (looking-at "-- "))
			(looking-at "[^-][^-] "))
		      (save-excursion
			(unless (eobp)
			  (forward-char 1)
			  (looking-at (format "^\\(%s\\)\\([^>]\\)" (or quote " ?"))))))
	    (save-excursion
	      (replace-match (if (string= (match-string 2) " ")
				 "" "\\2")))
	    (backward-delete-char -1)
	    (end-of-line))
	  (unless sig
	    (let ((fill-prefix (when quote (concat quote " "))))
	      (fill-region (fill-flowed-point-at-bol)
			   (fill-flowed-point-at-eol)
			   'left 'nosqueeze))))))))

(provide 'flow-fill)

;;; flow-fill.el ends here
