;;; misc.el --- some nonstandard basic editing commands for Emacs

;; Copyright (C) 1989 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: convenience

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

(defun copy-from-above-command (&optional arg)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point."
  (interactive "P")
  (let ((cc (current-column))
	n
	(string ""))
    (save-excursion
      (beginning-of-line)
      (backward-char 1)
      (skip-chars-backward "\ \t\n")
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (setq n (if arg (prefix-numeric-value arg) (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (= (preceding-char) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\ ))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (save-excursion (end-of-line) (point))
				 (+ n (point)))))))
    (insert string)))

(provide 'misc)

;;; misc.el ends here
