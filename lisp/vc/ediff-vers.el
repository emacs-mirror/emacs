;;; ediff-vers.el --- version control interface to Ediff  -*- lexical-binding:t -*-

;; Copyright (C) 1995-1997, 2001-2022 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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

(eval-when-compile (require 'ediff-init))

(defvar rcs-default-co-switches)

(defcustom ediff-keep-tmp-versions nil
  "If t, do not delete temporary previous versions for the files on which
comparison or merge operations are being performed."
  :type 'boolean
  :group 'ediff-vers
  )

(define-obsolete-function-alias 'ediff-vc-revision-other-window
  #'vc-revision-other-window "28.1")
(define-obsolete-function-alias 'ediff-vc-working-revision
  #'vc-working-revision "28.1")

;; VC.el support

(eval-when-compile
  (require 'vc-hooks)) ;; for vc-call macro


(defun ediff-vc-latest-version (file)
  "Return the version level of the latest version of FILE in repository."
  (if (fboundp 'vc-latest-version)
      (vc-latest-version file)
    (or (vc-file-getprop file 'vc-latest-revision)
	(cond ((vc-backend file)
	       (vc-call state file)
	       (vc-file-getprop file 'vc-latest-revision))
	      (t (error "File %s is not under version control" file))))
    ))


(defun ediff-vc-internal (rev1 rev2 &optional startup-hooks)
  ;; Run Ediff on versions of the current buffer.
  ;; If REV1 is "", use the latest version of the current buffer's file.
  ;; If REV2 is "" then compare current buffer with REV1.
  ;; If the current buffer is named `F', the version is named `F.~REV~'.
  ;; If `F.~REV~' already exists, it is used instead of being re-created.
  (let (file1 file2 rev1buf rev2buf)
    (if (string= rev1 "")
	(setq rev1 (ediff-vc-latest-version (buffer-file-name))))
    (save-window-excursion
      (save-excursion
	(vc-revision-other-window rev1)
	(setq rev1buf (current-buffer)
	      file1 (buffer-file-name)))
      (save-excursion
	(or (string= rev2 "") 		; use current buffer
	    (vc-revision-other-window rev2))
	(setq rev2buf (current-buffer)
	      file2 (buffer-file-name)))
      (push (lambda ()
	      (ediff-delete-version-file file1)
	      (or (string= rev2 "") (ediff-delete-version-file file2)))
	    startup-hooks))
    (ediff-buffers
     rev1buf rev2buf
     startup-hooks
     'ediff-revision)))

;; RCS.el support
(defun rcs-ediff-view-revision (&optional rev)
  "View previous RCS revision of current file.
With prefix argument, prompts for a revision name."
  (interactive (list (if current-prefix-arg
			 (read-string "Revision: "))))
  (let* ((filename (buffer-file-name (current-buffer)))
	 (switches (append '("-p")
			   (if rev (list (concat "-r" rev)) nil)))
	 (buff (concat (file-name-nondirectory filename) ".~" rev "~")))
    (message "Working ...")
    (setq filename (expand-file-name filename))
    (with-output-to-temp-buffer buff
      (ediff-with-current-buffer standard-output
	(fundamental-mode))
      (let ((output-buffer (ediff-rcs-get-output-buffer filename buff)))
	(delete-windows-on output-buffer)
	(with-current-buffer output-buffer
	  (apply #'call-process "co" nil t nil
		 ;; -q: quiet (no diagnostics)
		 (append switches rcs-default-co-switches
			 (list "-q" filename)))))
      (message "")
      buff)))

(defun ediff-rcs-get-output-buffer (file name)
  ;; Get a buffer for RCS output for FILE, make it writable and clean it up.
  ;; Optional NAME is name to use instead of `*RCS-output*'.
  ;; This is a modified version from rcs.el v1.1.  I use it here to make
  ;; Ediff immune to changes in rcs.el
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil
	    default-directory (file-name-directory (expand-file-name file)))
      (erase-buffer))
    buf))

(defun ediff-rcs-internal (rev1 rev2 &optional startup-hooks)
;; Run Ediff on versions of the current buffer.
;; If REV2 is "" then use current buffer.
  (let (rev2buf rev1buf)
    (save-window-excursion
      (setq rev2buf (if (string= rev2 "")
			(current-buffer)
		      (rcs-ediff-view-revision rev2))
	    rev1buf (rcs-ediff-view-revision rev1)))

    ;; rcs.el doesn't create temp version files, so we don't have to delete
    ;; anything in startup hooks to ediff-buffers
    (ediff-buffers rev1buf rev2buf startup-hooks 'ediff-revision)
    ))

;;; Merge with Version Control

(defun ediff-vc-merge-internal (rev1 rev2 ancestor-rev
				     &optional startup-hooks merge-buffer-file)
;; If ANCESTOR-REV non-nil, merge with ancestor
  (let (buf1 buf2 ancestor-buf)
    (save-window-excursion
      (save-excursion
	(vc-revision-other-window rev1)
	(setq buf1 (current-buffer)))
      (save-excursion
	(or (string= rev2 "")
	    (vc-revision-other-window rev2))
	(setq buf2 (current-buffer)))
      (if ancestor-rev
	  (save-excursion
	    (if (string= ancestor-rev "")
		(setq ancestor-rev (vc-working-revision
                                    buffer-file-name)))
	    (vc-revision-other-window ancestor-rev)
	    (setq ancestor-buf (current-buffer))))
      (push (let ((f1 (buffer-file-name buf1))
                  (f2 (unless (string= rev2 "") (buffer-file-name buf2)))
                  (fa (unless (or (string= ancestor-rev "")
		                  (not ancestor-rev))
		        (buffer-file-name ancestor-buf))))
              (lambda ()
	        (ediff-delete-version-file f1)
	        (if f2 (ediff-delete-version-file f2))
	        (if fa (ediff-delete-version-file fa))))
	    startup-hooks))
    (if ancestor-rev
	(ediff-merge-buffers-with-ancestor
	 buf1 buf2 ancestor-buf
	 startup-hooks 'ediff-merge-revisions-with-ancestor merge-buffer-file)
      (ediff-merge-buffers
       buf1 buf2 startup-hooks 'ediff-merge-revisions merge-buffer-file))
    ))

(defun ediff-rcs-merge-internal (rev1 rev2 ancestor-rev
				      &optional
				      startup-hooks merge-buffer-file)
  ;; If ANCESTOR-REV non-nil, merge with ancestor
  (let (buf1 buf2 ancestor-buf)
    (save-window-excursion
      (setq buf1 (rcs-ediff-view-revision rev1)
	    buf2 (if (string= rev2 "")
		     (current-buffer)
		   (rcs-ediff-view-revision rev2))
	    ancestor-buf (if ancestor-rev
			     (if (string= ancestor-rev "")
				 (current-buffer)
			       (rcs-ediff-view-revision ancestor-rev)))))
    ;; rcs.el doesn't create temp version files, so we don't have to delete
    ;; anything in startup hooks to ediff-buffers
    (if ancestor-rev
	(ediff-merge-buffers-with-ancestor
	 buf1 buf2 ancestor-buf
	 startup-hooks 'ediff-merge-revisions-with-ancestor merge-buffer-file)
      (ediff-merge-buffers
       buf1 buf2 startup-hooks 'ediff-merge-revisions merge-buffer-file))))


;; delete version file on exit unless ediff-keep-tmp-versions is true
(defun ediff-delete-version-file (file)
  (or ediff-keep-tmp-versions (delete-file file)))


(provide 'ediff-vers)
;;; ediff-vers.el ends here
