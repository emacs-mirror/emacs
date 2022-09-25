;;; gnus-dup.el --- suppression of duplicate articles in Gnus  -*- lexical-binding: t -*-

;; Copyright (C) 1996-2022 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;; This package tries to mark articles as read the second time the
;; user reads a copy.  This is useful if the server doesn't support
;; Xref properly, or if the user reads the same group from several
;; servers.

;;; Code:

(require 'gnus)
(require 'gnus-art)

(defgroup gnus-duplicate nil
  "Suppression of duplicate articles."
  :group 'gnus)

(defcustom gnus-save-duplicate-list nil
  "If non-nil, save the duplicate list when shutting down Gnus.
If nil, duplicate suppression will only work on duplicates
seen in the same session."
  :type 'boolean)

(defcustom gnus-duplicate-list-length 10000
  "The maximum number of duplicate Message-IDs to keep track of."
  :type 'integer)

(defcustom gnus-duplicate-file (nnheader-concat gnus-directory "suppression")
  "The name of the file to store the duplicate suppression list."
  :type 'file)

;;; Internal variables

(defvar gnus-dup-list nil
  "List of seen message IDs, as strings.")

(defvar gnus-dup-hashtb nil
  "Hash table of seen message IDs, for fast lookup.")

(defvar gnus-dup-list-dirty nil
  "Non-nil if `gnus-dup-list' needs to be saved.")

;;;
;;; Starting and stopping
;;;

(gnus-add-shutdown 'gnus-dup-close 'gnus)

(defun gnus-dup-close ()
  "Possibly save the duplicate suppression list and shut down the subsystem."
  (gnus-dup-save)
  (setq gnus-dup-list nil
	gnus-dup-hashtb nil
	gnus-dup-list-dirty nil))

(defun gnus-dup-open ()
  "Possibly read the duplicate suppression list and start the subsystem."
  (if gnus-save-duplicate-list
      (gnus-dup-read)
    (setq gnus-dup-list nil))
  (setq gnus-dup-hashtb (gnus-make-hashtable))
  ;; Enter all Message-IDs into the hash table.
  (dolist (g gnus-dup-list)
    (puthash g t gnus-dup-hashtb)))

(defun gnus-dup-read ()
  "Read the duplicate suppression list."
  (setq gnus-dup-list nil)
  (when (file-exists-p gnus-duplicate-file)
    (load gnus-duplicate-file t t t)))

(defun gnus-dup-save ()
  "Save the duplicate suppression list."
  (when (and gnus-save-duplicate-list
	     gnus-dup-list-dirty)
    (with-temp-file gnus-duplicate-file
      (gnus-prin1 `(setq gnus-dup-list ',gnus-dup-list))))
  (setq gnus-dup-list-dirty nil))

;;;
;;; Interface functions
;;;

(defun gnus-dup-enter-articles ()
  "Enter articles from the current group for future duplicate suppression."
  (unless gnus-dup-hashtb
    (gnus-dup-open))
  (setq gnus-dup-list-dirty t)		; mark list for saving
  (let (msgid)
    ;; Enter the Message-IDs of all read articles into the list
    ;; and hash table.
    (dolist (datum gnus-newsgroup-data)
      (when (and (not (gnus-data-pseudo-p datum))
		 (> (gnus-data-number datum) 0)
		 (not (memq (gnus-data-number datum) gnus-newsgroup-unreads))
		 (not (= (gnus-data-mark datum) gnus-canceled-mark))
		 (setq msgid (mail-header-id (gnus-data-header datum)))
		 (not (nnheader-fake-message-id-p msgid))
		 (not (gethash msgid gnus-dup-hashtb)))
	(push msgid gnus-dup-list)
	(puthash msgid t gnus-dup-hashtb))))
  ;; Remove excess Message-IDs from the list and hash table.
  (let* ((dups (cons nil gnus-dup-list))
         (end  (nthcdr gnus-duplicate-list-length dups)))
    (when end
      (mapc (lambda (id) (remhash id gnus-dup-hashtb)) (cdr end))
      (setcdr end nil))
    (setq gnus-dup-list (cdr dups))))

(defun gnus-dup-suppress-articles ()
  "Mark duplicate articles as read."
  (unless gnus-dup-hashtb
    (gnus-dup-open))
  (gnus-message 8 "Suppressing duplicates...")
  (let ((auto (and gnus-newsgroup-auto-expire
		   (memq gnus-duplicate-mark gnus-auto-expirable-marks)))
	number)
    (dolist (header gnus-newsgroup-headers)
      (when (and (gethash (mail-header-id header) gnus-dup-hashtb)
                 (setq number (mail-header-number header))
                 (gnus-summary-article-unread-p number))
        (setq gnus-newsgroup-unreads (delq number gnus-newsgroup-unreads))
	(if (not auto)
	    (push (cons number gnus-duplicate-mark) gnus-newsgroup-reads)
	  (push number gnus-newsgroup-expirable)
	  (push (cons number gnus-expirable-mark) gnus-newsgroup-reads)))))
  (gnus-message 8 "Suppressing duplicates...done"))

(defun gnus-dup-unsuppress-article (article)
  "Stop suppression of ARTICLE."
  (let (header id)
    (when (and gnus-dup-hashtb
               (setq header (gnus-data-header (gnus-data-find article)))
               (setq id (mail-header-id header)))
      (setq gnus-dup-list-dirty t)
      (setq gnus-dup-list (delete id gnus-dup-list))
      (remhash id gnus-dup-hashtb))))

(provide 'gnus-dup)

;;; gnus-dup.el ends here
