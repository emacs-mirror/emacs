;;; uncompress.el --- auto-decompression hook for visiting .Z files

;; Copyright (C) 1992, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: files

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

;; This file has been obsolete since Emacs 21.1.

;; This package can be used to arrange for automatic uncompress of
;; compressed files when they are visited.
;; All that's necessary is to load it.  This can conveniently be done from
;; your .emacs file.

;; M-x auto-compression-mode is a more modern replacement for this package.

;;; Code:

;; When we are about to make a backup file,
;; uncompress the file we visited
;; so that making the backup can work properly.
;; This is used as a write-file-hook.

(defvar uncompress-program "gunzip"
  "Program to use for uncompression.")

(defun uncompress-backup-file ()
  (and buffer-file-name make-backup-files (not buffer-backed-up)
       (not (file-exists-p buffer-file-name))
       (call-process uncompress-program nil nil nil buffer-file-name))
  nil)

(or (assoc "\\.Z$" auto-mode-alist)
    (setq auto-mode-alist
	  (cons '("\\.Z$" . uncompress-while-visiting) auto-mode-alist)))
(or (assoc "\\.gz$" auto-mode-alist)
    (setq auto-mode-alist
	  (cons '("\\.gz$" . uncompress-while-visiting) auto-mode-alist)))
(or (assoc "\\.tgz$" auto-mode-alist)
    (setq auto-mode-alist
	  (cons '("\\.tgz$" . uncompress-while-visiting) auto-mode-alist)))

(defun uncompress-while-visiting ()
  "Temporary \"major mode\" used for .Z and .gz files, to uncompress them.
It then selects a major mode from the uncompressed file name and contents."
  (if (and (not (null buffer-file-name))
	   (string-match "\\.Z$" buffer-file-name))
      (set-visited-file-name
       (substring buffer-file-name 0 (match-beginning 0)))
    (if (and (not (null buffer-file-name))
	     (string-match "\\.gz$" buffer-file-name))
	(set-visited-file-name
	 (substring buffer-file-name 0 (match-beginning 0)))
      (if (and (not (null buffer-file-name))
               (string-match "\\.tgz$" buffer-file-name))
          (set-visited-file-name
           (concat (substring buffer-file-name 0 (match-beginning 0)) ".tar")))))
  (message "Uncompressing...")
  (let ((buffer-read-only nil)
	(coding-system-for-write 'no-conversion)
	(coding-system-for-read
	 (car (find-operation-coding-system
	       'insert-file-contents
	       buffer-file-name t))))
    (shell-command-on-region (point-min) (point-max) uncompress-program t))
  (goto-char (point-min))
  (message "Uncompressing...done")
  (set-buffer-modified-p nil)
  (add-hook 'write-file-functions 'uncompress-backup-file nil t)
  (normal-mode))

(add-hook 'find-file-not-found-functions 'find-compressed-version)

(defun find-compressed-version ()
  "Hook to read and uncompress the compressed version of a file."
  ;; Just pretend we had visited the compressed file,
  ;; and uncompress-while-visiting will do the rest.
  (let (name)
    (if (file-exists-p (setq name (concat buffer-file-name ".Z")))
	(setq buffer-file-name name)
      (if (file-exists-p (setq name (concat buffer-file-name ".gz")))
	  (setq buffer-file-name name)))
    (if (eq name buffer-file-name)
	(progn
	  (insert-file-contents buffer-file-name t)
	  (goto-char (point-min))
	  ;; No need for this, because error won't be set to t
	  ;; if this function returns t.
	  ;; (setq error nil)
	  t))))

(message "The uncompress package is obsolete; use M-x auto-compression-mode")

(provide 'uncompress)

;; arch-tag: 626658d4-fcce-499a-990d-d165f2ed7da3
;;; uncompress.el ends here
