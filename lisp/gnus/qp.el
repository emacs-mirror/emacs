;;; qp.el --- Quoted-Printable functions
;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'mm-util)

(defvar quoted-printable-encoding-characters
  (mapcar 'identity "0123456789ABCDEFabcdef"))

(defun quoted-printable-decode-region (from to &optional charset)
  "Decode quoted-printable in the region between FROM and TO.
If CHARSET is non-nil, decode the region with charset."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let (start)
	(narrow-to-region from to)
	(goto-char from)
	(while (not (eobp))
	  (cond 
	   ((eq (char-after) ?=)
	    (delete-char 1)
	    (unless start
	      (setq start (point)))
	    (cond
	     ;; End of the line.
	     ((eq (char-after) ?\n)
	      (delete-char 1))
	     ;; Encoded character.
	     ((and
	       (memq (char-after) quoted-printable-encoding-characters)
	       (memq (char-after (1+ (point)))
		     quoted-printable-encoding-characters))
	      (insert
	       (string-to-number
		(buffer-substring (point) (+ 2 (point)))
		16))
	      (delete-char 2))
	     ;; Quoted equal sign.
	     ((eq (char-after) ?=)
	      (forward-char 1))
	     ;; End of buffer.
	     ((eobp))
	     ;; Invalid.
	     (t
	      (message "Malformed MIME quoted-printable message"))))
	   ((and charset start (not (eq (mm-charset-after) 'ascii)))
	    (mm-decode-coding-region start (point) charset)
	    (setq start nil)
	    (forward-char 1))
	   (t
	    (forward-char 1))))
	(if (and charset start)
	    (mm-decode-coding-region start (point) charset))))))

(defun quoted-printable-decode-string (string &optional charset)
  "Decode the quoted-printable-encoded STRING and return the results.
If CHARSET is non-nil, decode the region with charset."
  (with-temp-buffer
    (insert string)
    (quoted-printable-decode-region (point-min) (point-max) charset)
    (buffer-string)))

(defun quoted-printable-encode-region (from to &optional fold class)
  "QP-encode the region between FROM and TO.

If FOLD fold long lines.  If CLASS, translate the characters 
matched by that regexp.

If `mm-use-ultra-safe-encoding' is set, fold unconditionally and
encode lines starting with \"From\"."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      ;;      (mm-encode-body)
      ;; Encode all the non-ascii and control characters.
      (goto-char (point-min))
      (while (and (skip-chars-forward
		   (or class "^\000-\007\013\015-\037\200-\377="))
		  (not (eobp)))
	(insert
	 (prog1
	     (upcase (format "=%02x" (char-after)))
	   (delete-char 1))))
      ;; Encode white space at the end of lines.
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
	(goto-char (match-beginning 0))
	(while (not (eolp))
	  (insert
	   (prog1
	       (upcase (format "=%02x" (char-after)))
	     (delete-char 1)))))
      (when (or fold mm-use-ultra-safe-encoding)
	;; Fold long lines.
	(let ((tab-width 1)) ;; HTAB is one character.
	  (goto-char (point-min))
	  (while (not (eobp))
	    ;; In ultra-safe mode, encode "From " at the beginning of a
	    ;; line.
	    (when mm-use-ultra-safe-encoding
	      (beginning-of-line)
	      (when (looking-at "From ")
		(replace-match "From=20" nil t)))
	    (end-of-line)
	    (while (> (current-column) 76) ;; tab-width must be 1.
	      (beginning-of-line)
	      (forward-char 75);; 75 chars plus an "="
	      (search-backward "=" (- (point) 2) t)
	      (insert "=\n")
	      (end-of-line))
	    (unless (eobp)
	      (forward-line))))))))

(defun quoted-printable-encode-string (string)
  "QP-encode STRING and return the results."
  (mm-with-unibyte-buffer
    (insert string)
    (quoted-printable-encode-region (point-min) (point-max))
    (buffer-string)))

(provide 'qp)

;; qp.el ends here
