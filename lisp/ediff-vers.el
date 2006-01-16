;;; ediff-vers.el --- version control interface to Ediff

;; Copyright (C) 1995, 1996, 1997, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>

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

;;; Code:

;; Compiler pacifier
(defvar rcs-default-co-switches)
(defvar sc-mode)
(defvar cvs-shell)
(defvar cvs-program)
(defvar cvs-cookie-handle)
(defvar ediff-temp-file-prefix)

(and noninteractive
     (eval-when-compile
       (let ((load-path (cons (expand-file-name ".") load-path)))
	 (load "pcl-cvs" 'noerror)
	 (load "rcs" 'noerror)
	 ;; On 8+3 MS-DOS filesystems, generic-x.el is loaded
	 ;; instead of (the missing) generic-sc.el.  Since the
	 ;; version of Emacs which supports MS-DOS doesn't have
	 ;; generic-sc, we simply avoid loading it.
	 (or (and (fboundp 'msdos-long-file-names)
		  (not (msdos-long-file-names)))
	     (load "generic-sc" 'noerror))
	 ;; (load "vc" 'noerror) ; this sometimes causes compiler error
	 (or (featurep 'ediff-init)
	     (load "ediff-init.el" nil nil 'nosuffix))
	 )))
;; end pacifier

;; VC.el support

(defun ediff-vc-latest-version (file)
  "Return the version level of the latest version of FILE in repository."
  (if (fboundp 'vc-latest-version)
      (vc-latest-version file)
    (or (vc-file-getprop file 'vc-latest-version)
	(cond ((vc-backend file)
	       (vc-call state file)
	       (vc-file-getprop file 'vc-latest-version))
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
	(vc-version-other-window rev1)
	(setq rev1buf (current-buffer)
	      file1 (buffer-file-name)))
      (save-excursion
	(or (string= rev2 "") 		; use current buffer
	    (vc-version-other-window rev2))
	(setq rev2buf (current-buffer)
	      file2 (buffer-file-name)))
      (setq startup-hooks
	    (cons `(lambda ()
		     (delete-file ,file1)
		     (or ,(string= rev2 "") (delete-file ,file2)))
		  startup-hooks)))
    (ediff-buffers
     rev1buf rev2buf
     startup-hooks
     'ediff-revision)))

;; RCS.el support
(defun rcs-ediff-view-revision (&optional rev)
;; View previous RCS revision of current file.
;; With prefix argument, prompts for a revision name.
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
	(save-excursion
	  (set-buffer output-buffer)
	  (apply 'call-process "co" nil t nil
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
  (let* ((default-major-mode 'fundamental-mode) ; no frills!
	 (buf (get-buffer-create name)))
    (save-excursion
      (set-buffer buf)
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


;; GENERIC-SC.el support

(defun generic-sc-get-latest-rev ()
  (cond ((eq sc-mode 'CCASE)
	 (eval "main/LATEST"))
	(t (eval ""))))

(defun ediff-generic-sc-internal (rev1 rev2 &optional startup-hooks)
;; Run Ediff on versions of the current buffer.
;; If REV2 is "" then compare current buffer with REV1.
;; If the current buffer is named `F', the version is named `F.~REV~'.
;; If `F.~REV~' already exists, it is used instead of being re-created.
  (let (rev1buf rev2buf)
    (save-excursion
      (if (or (not rev1) (string= rev1 ""))
	  (setq rev1 (generic-sc-get-latest-rev)))
      (sc-visit-previous-revision rev1)
      (setq rev1buf (current-buffer)))
    (save-excursion
      (or (string= rev2 "") 		; use current buffer
	  (sc-visit-previous-revision rev2))
      (setq rev2buf (current-buffer)))
    (ediff-buffers rev1buf rev2buf startup-hooks 'ediff-revision)))


;;; Merge with Version Control

(defun ediff-vc-merge-internal (rev1 rev2 ancestor-rev
				     &optional startup-hooks merge-buffer-file)
;; If ANCESTOR-REV non-nil, merge with ancestor
  (let (buf1 buf2 ancestor-buf)
    (save-window-excursion
      (save-excursion
	(vc-version-other-window rev1)
	(setq buf1 (current-buffer)))
      (save-excursion
	(or (string= rev2 "")
	    (vc-version-other-window rev2))
	(setq buf2 (current-buffer)))
      (if ancestor-rev
	  (save-excursion
	    (if (string= ancestor-rev "")
		(setq ancestor-rev (vc-workfile-version buffer-file-name)))
	    (vc-version-other-window ancestor-rev)
	    (setq ancestor-buf (current-buffer))))
      (setq startup-hooks
	    (cons
	     `(lambda ()
		(delete-file ,(buffer-file-name buf1))
		(or ,(string= rev2 "")
		    (delete-file ,(buffer-file-name buf2)))
		(or ,(string= ancestor-rev "")
		    ,(not ancestor-rev)
		    (delete-file ,(buffer-file-name ancestor-buf)))
		)
	     startup-hooks)))
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

(defun ediff-generic-sc-merge-internal (rev1 rev2 ancestor-rev
					     &optional
					     startup-hooks merge-buffer-file)
  ;; If ANCESTOR-REV non-nil, merge with ancestor
  (let (buf1 buf2 ancestor-buf)
    (save-excursion
      (if (string= rev1 "")
	  (setq rev1 (generic-sc-get-latest-rev)))
      (sc-visit-previous-revision rev1)
      (setq buf1 (current-buffer)))
    (save-excursion
      (or (string= rev2 "")
	  (sc-visit-previous-revision rev2))
      (setq buf2 (current-buffer)))
    (if ancestor-rev
	(save-excursion
	  (or (string= ancestor-rev "")
	      (sc-visit-previous-revision ancestor-rev))
	  (setq ancestor-buf (current-buffer))))
    (if ancestor-rev
	(ediff-merge-buffers-with-ancestor
	 buf1 buf2 ancestor-buf
	 startup-hooks 'ediff-merge-revisions-with-ancestor merge-buffer-file)
      (ediff-merge-buffers
       buf1 buf2 startup-hooks 'ediff-merge-revisions merge-buffer-file))))


;; PCL-CVS.el support

;; MK: Check. This function doesn't seem to be used any more by pcvs or pcl-cvs
(defun cvs-run-ediff-on-file-descriptor (tin)
;; This is a replacement for cvs-emerge-mode
;; Runs after cvs-update.
;; Ediff-merge appropriate revisions of the selected file.
  (let* ((fileinfo (tin-cookie cvs-cookie-handle tin))
	 (type (cvs-fileinfo->type fileinfo))
	 (tmp-file
	  (cvs-retrieve-revision-to-tmpfile fileinfo))
	 (default-directory
	   (file-name-as-directory (cvs-fileinfo->dir fileinfo)))
	 ancestor-file)

    (or (memq type '(MERGED CONFLICT MODIFIED))
	(error
	 "Can only merge `Modified', `Merged' or `Conflict' files"))

    (cond ((memq type '(MERGED CONFLICT))
	   (setq ancestor-file
		 (cvs-retrieve-revision-to-tmpfile
		  fileinfo
		  ;; revision
		  (cvs-fileinfo->base-revision fileinfo)))
	   (ediff-merge-buffers-with-ancestor
	    (find-file-noselect tmp-file)
	    (find-file-noselect (cvs-fileinfo->backup-file fileinfo))
	    (find-file-noselect ancestor-file)
	    nil ; startup-hooks
	    'ediff-merge-revisions-with-ancestor))
	  ((eq type 'MODIFIED)
	   (ediff-buffers
	    (find-file-noselect tmp-file)
	    (if (featurep 'xemacs)
		;; XEmacs doesn't seem to have cvs-fileinfo->full-name
		(find-file-noselect (cvs-fileinfo->full-path fileinfo))
	      (find-file-noselect (cvs-fileinfo->full-name fileinfo)))
	    nil ; startup-hooks
	    'ediff-revisions)))
    (if (stringp tmp-file) (delete-file tmp-file))
    (if (stringp ancestor-file) (delete-file ancestor-file))))


(provide 'ediff-vers)

;;; Local Variables:
;;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;;; End:

;;; arch-tag: bbb34f0c-2a90-426a-a77a-c75f479ebbbf
;;; ediff-vers.el ends here
