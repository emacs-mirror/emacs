;;; zone-mode.el --- major mode for editing DNS zone files

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: John Heidemann <johnh@isi.edu>
;; Keywords: DNS, languages

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

;;;
;;; See the comments in ``define-derived-mode zone-mode''
;;; (the last function in this file)
;;; for what this mode is and how to use it automatically.
;;;

;;;
;;; Credits:
;;; Zone-mode was written by John Heidemann <johnh@isi.edu>,
;;; with bug fixes from Simon Leinen <simon@limmat.switch.ch>.
;;;

;;; Code:

(defun zone-mode-update-serial ()
  "Update the serial number in a zone."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\b\\([0-9]+\\)\\([0-9][0-9]\\)\\([ \t]+;[ \t]+[Ss]erial\\)" (point-max) t)
      (let* ((old-date (match-string 1))
	     (old-seq (match-string 2))
	     (old-seq-num (string-to-number (match-string 2)))
	     (old-flag (match-string 3))
	     (cur-date (format-time-string "%Y%m%d"))
	     (new-seq
	      (cond
	       ((not (string= old-date cur-date))
		"00") ;; reset sequence number
	       ((>= old-seq-num 99)
		(error "Serial number's sequence cannot increment beyond 99"))
	       (t
		(format "%02d" (1+ old-seq-num)))))
	     (old-serial (concat old-date old-seq))
	     (new-serial (concat cur-date new-seq)))
	(if (string-lessp new-serial old-serial)
	    (error (format "Serial numbers want to move backwards from %s to %s" old-serial new-serial))
	  (replace-match (concat cur-date new-seq old-flag) t t))))))
  
;;;###autoload
(defun zone-mode-update-serial-hook ()
  "Update the serial number in a zone if the file was modified."
  (interactive)
  (if (buffer-modified-p (current-buffer))
      (zone-mode-update-serial))
  nil ;; so we can run from write-file-hooks
  )

(defvar zone-mode-syntax-table nil
  "Zone-mode's syntax table.")

(defun zone-mode-load-time-setup ()
  "Initialise `zone-mode' stuff."
  (setq zone-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "<" zone-mode-syntax-table)
  (modify-syntax-entry ?\n ">" zone-mode-syntax-table))

;;;###autoload
(define-derived-mode zone-mode fundamental-mode "zone"
  "A mode for editing DNS zone files.

Zone-mode does two things:

	- automatically update the serial number for a zone
		when saving the file

	- fontification"

  (add-hook 'write-file-hooks 'zone-mode-update-serial-hook nil t)

  (if (null zone-mode-syntax-table)
      (zone-mode-load-time-setup)) ;; should have been run at load-time

  ;; font-lock support:
  (set-syntax-table zone-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(nil nil nil nil beginning-of-line)))

(zone-mode-load-time-setup)

(provide 'zone-mode)

;;; zone-mode.el ends here
