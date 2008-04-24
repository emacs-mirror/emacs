;;; spam-wash.el --- wash spam before analysis

;; Copyright (C) 2004, 2007, 2008  Free Software Foundation, Inc.

;; Author: Andrew Cohen <cohen@andy.bu.edu>
;; Keywords: mail

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

;; This library decodes MIME encodings such as base64 and
;; quoted-printable to allow for better spam analysis.
;;
;; `spam-wash' should be called in a buffer containing the message.

;;; Code:

(require 'gnus-art)

(defun spam-wash ()
  "Treat the current buffer prior to spam analysis."
  (interactive)
  (run-hooks 'gnus-article-decode-hook)
  (save-excursion
    (save-restriction
      (let* ((buffer-read-only  nil)
	     (gnus-inhibit-treatment t)
	     (gnus-article-buffer (current-buffer))
	     (handles (or (mm-dissect-buffer nil gnus-article-loose-mime)
			  (and gnus-article-emulate-mime
			       (mm-uu-dissect))))
	     handle)
	  (when gnus-article-mime-handles
	    (mm-destroy-parts gnus-article-mime-handles)
	    (setq gnus-article-mime-handle-alist nil))
	  (setq gnus-article-mime-handles handles)
	  (when (and handles
		   (or (not (stringp (car handles)))
		       (cdr handles)))
		(article-goto-body)
		(delete-region (point) (point-max))
		(spam-treat-parts handles))))))

(defun spam-treat-parts (handle)
  (if (stringp (car handle))
      (mapcar 'spam-treat-parts (cdr handle))
    (if (bufferp (car handle))
	(save-restriction
	  (narrow-to-region (point) (point))
	(when (let ((case-fold-search t))
		(string-match "text" (car (mm-handle-type handle))))
	  (mm-insert-part handle))
	  (goto-char (point-max)))
      (mapcar 'spam-treat-parts handle))))

(provide 'spam-wash)

;; arch-tag: 3c7f94a7-c96d-4c77-bb59-950df12bc85f
;;; spam-wash.el ends here
