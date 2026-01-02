;;; mh-gnus.el --- make MH-E compatible with various versions of Gnus  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2004, 2006-2026 Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;;; Code:

(require 'mh-e)

(require 'gnus-util)
(require 'mm-bodies)
(require 'mm-decode)
(require 'mm-view)
(require 'mml)

(defun mh-gnus-local-map-property (map)
  "Return a list suitable for a text property list specifying keymap MAP."
  (declare (obsolete nil "29.1"))
  (list 'keymap map))

(define-obsolete-function-alias 'mh-mm-merge-handles
  #'mm-merge-handles "29.1")

(define-obsolete-function-alias 'mh-mm-set-handle-multipart-parameter
  #'mm-set-handle-multipart-parameter "29.1")

(define-obsolete-function-alias 'mh-mm-inline-text-vcard
  #'mm-inline-text-vcard "29.1")

(define-obsolete-function-alias 'mh-mm-possibly-verify-or-decrypt
  #'mm-possibly-verify-or-decrypt "29.1")

(define-obsolete-function-alias 'mh-mm-handle-multipart-ctl-parameter
  #'mm-handle-multipart-ctl-parameter "29.1")

(define-obsolete-function-alias 'mh-mm-readable-p
  #'mm-readable-p "29.1")

(define-obsolete-function-alias 'mh-mm-long-lines-p
  #'mm-long-lines-p "29.1")

(define-obsolete-function-alias 'mh-mm-keep-viewer-alive-p
  #'mm-keep-viewer-alive-p "29.1")

(define-obsolete-function-alias 'mh-mm-destroy-parts
  #'mm-destroy-parts "29.1")

(define-obsolete-function-alias 'mh-mm-uu-dissect-text-parts
  #'mm-uu-dissect-text-parts "29.1")

(define-obsolete-function-alias 'mh-mml-minibuffer-read-disposition
  #'mml-minibuffer-read-disposition "29.1")

;; This is mm-save-part from Gnus 5.11 since that function in Emacs
;; 21.2 is buggy (the args to read-file-name are incorrect) and the
;; version in Emacs 22 is not consistent with C-x C-w in that you
;; can't just specify a directory and have the right thing happen.
(defun mh-mm-save-part (handle &optional prompt)
  "Write HANDLE to a file.
PROMPT overrides the default one used to ask user for a file name."
  (let ((filename (or (mail-content-type-get
		       (mm-handle-disposition handle) 'filename)
		      (mail-content-type-get
		       (mm-handle-type handle) 'name)))
	file)
    (when filename
      (setq filename (gnus-map-function mm-file-name-rewrite-functions
					(file-name-nondirectory filename))))
    (setq file
          (read-file-name (or prompt "Save MIME part to: ")
                          (or mm-default-directory default-directory)
                          nil nil (or filename "")))
    (setq mm-default-directory (file-name-directory file))
    (and (or (not (file-exists-p file))
	     (yes-or-no-p (format "File %s already exists; overwrite? "
				  file)))
	 (progn
	   (mm-save-part-to-file handle file)
	   file))))

(defun mh-mm-text-html-renderer ()
  "Find the renderer Gnus is using to display text/html MIME parts."
  (declare (obsolete mm-text-html-renderer "29.1"))
  mm-text-html-renderer)

(provide 'mh-gnus)

;; Local Variables:
;; no-update-autoloads: t
;; sentence-end-double-space: nil
;; End:

;;; mh-gnus.el ends here
