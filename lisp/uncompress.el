;;; uncompress.el --- auto-decompression hook for visiting .Z files

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Maintainer: FSF

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package can be used to arrange for automatic uncompress of
;; files packed with the UNIX compress(1) utility when they are visited.
;; All that's necessary is to load it.  This can conveniently be done from
;; your .emacs file.

;;; Code:

;; When we are about to make a backup file,
;; uncompress the file we visited
;; so that making the backup can work properly.
;; This is used as a write-file-hook.

(defun uncompress-backup-file ()
  (and buffer-file-name make-backup-files (not buffer-backed-up)
       (not (file-exists-p buffer-file-name))
       (call-process "uncompress" nil nil nil buffer-file-name))
  nil)

(or (assoc "\\.Z$" auto-mode-alist)
    (setq auto-mode-alist
	  (cons '("\\.Z$" . uncompress-while-visiting) auto-mode-alist)))

(defun uncompress-while-visiting ()
  "Temporary \"major mode\" used for .Z files, to uncompress the contents.
It then selects a major mode from the uncompressed file name and contents."
  (if (and (not (null buffer-file-name))
	   (string-match "\\.Z$" buffer-file-name))
      (set-visited-file-name
       (substring buffer-file-name 0 (match-beginning 0))))
  (message "Uncompressing...")
  (let ((buffer-read-only nil))
    (shell-command-on-region (point-min) (point-max) "uncompress" t))
  (message "Uncompressing...done")
  (set-buffer-modified-p nil)
  (make-local-variable 'write-file-hooks)
  (or (memq 'uncompress-backup-file write-file-hooks)
      (setq write-file-hooks (cons 'uncompress-backup-file write-file-hooks)))
  (normal-mode))

(or (memq 'find-compressed-version find-file-not-found-hooks)
    (setq find-file-not-found-hooks
	  (cons 'find-compressed-version find-file-not-found-hooks)))

(defun find-compressed-version ()
  "Hook to read and uncompress the compressed version of a file."
  ;; Just pretend we had visited the compressed file,
  ;; and uncompress-while-visiting will do the rest.
  (if (file-exists-p (concat buffer-file-name ".Z"))
      (progn
	(setq buffer-file-name (concat buffer-file-name ".Z"))
	(insert-file-contents buffer-file-name t)
	(goto-char (point-min))
	(setq error nil)
	t)))

(provide 'uncompress)

;;; uncompress.el ends here
