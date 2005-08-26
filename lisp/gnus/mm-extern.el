;;; mm-extern.el --- showing message/external-body

;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: message external-body

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'mm-util)
(require 'mm-decode)
(require 'mm-url)

(defvar mm-extern-function-alist
  '((local-file . mm-extern-local-file)
    (url . mm-extern-url)
    (anon-ftp . mm-extern-anon-ftp)
    (ftp . mm-extern-ftp)
;;;     (tftp . mm-extern-tftp)
    (mail-server . mm-extern-mail-server)
;;;     (afs . mm-extern-afs))
    ))

(defvar mm-extern-anonymous "anonymous")

(defun mm-extern-local-file (handle)
  (erase-buffer)
  (let ((name (cdr (assq 'name (cdr (mm-handle-type handle)))))
	(coding-system-for-read mm-binary-coding-system))
    (unless name
      (error "The filename is not specified"))
    (mm-disable-multibyte)
    (if (file-exists-p name)
	(mm-insert-file-contents name nil nil nil nil t)
      (error "File %s is gone" name))))

(defun mm-extern-url (handle)
  (erase-buffer)
  (let ((url (cdr (assq 'url (cdr (mm-handle-type handle)))))
	(name buffer-file-name)
	(coding-system-for-read mm-binary-coding-system))
    (unless url
      (error "URL is not specified"))
    (mm-with-unibyte-current-buffer
      (mm-url-insert-file-contents url))
    (mm-disable-multibyte)
    (setq buffer-file-name name)))

(defun mm-extern-anon-ftp (handle)
  (erase-buffer)
  (let* ((params (cdr (mm-handle-type handle)))
	 (name (cdr (assq 'name params)))
	 (site (cdr (assq 'site params)))
	 (directory (cdr (assq 'directory params)))
	 (mode (cdr (assq 'mode params)))
	 (path (concat "/" (or mm-extern-anonymous
			       (read-string (format "ID for %s: " site)))
		       "@" site ":" directory "/" name))
	 (coding-system-for-read mm-binary-coding-system))
    (unless name
      (error "The filename is not specified"))
    (mm-disable-multibyte)
    (mm-insert-file-contents path nil nil nil nil t)))

(defun mm-extern-ftp (handle)
  (let (mm-extern-anonymous)
    (mm-extern-anon-ftp handle)))

(defun mm-extern-mail-server (handle)
  (require 'message)
  (let* ((params (cdr (mm-handle-type handle)))
	 (server (cdr (assq 'server params)))
	 (subject (or (cdr (assq 'subject params)) "none"))
	 (buf (current-buffer))
	 info)
    (if (y-or-n-p (format "Send a request message to %s?" server))
	(save-window-excursion
	  (message-mail server subject)
	  (message-goto-body)
	  (delete-region (point) (point-max))
	  (insert-buffer-substring buf)
	  (message "Requesting external body...")
	  (message-send-and-exit)
	  (setq info "Request is sent.")
	  (message info))
      (setq info "Request is not sent."))
    (goto-char (point-min))
    (insert "[" info "]\n\n")))

;;;###autoload
(defun mm-inline-external-body (handle &optional no-display)
  "Show the external-body part of HANDLE.
This function replaces the buffer of HANDLE with a buffer contains
the entire message.
If NO-DISPLAY is nil, display it. Otherwise, do nothing after replacing."
  (let* ((access-type (cdr (assq 'access-type
				 (cdr (mm-handle-type handle)))))
	 (func (cdr (assq (intern
			   (downcase
			    (or access-type
				(error "Couldn't find access type"))))
			  mm-extern-function-alist)))
	 gnus-displaying-mime buf
	 handles)
    (unless (mm-handle-cache handle)
      (unless func
	(error "Access type (%s) is not supported" access-type))
      (with-temp-buffer
	(mm-insert-part handle)
	(goto-char (point-max))
	(insert "\n\n")
	(setq handles (mm-dissect-buffer t)))
      (unless (bufferp (car handles))
	(mm-destroy-parts handles)
	(error "Multipart external body is not supported"))
      (save-excursion ;; single part
	(set-buffer (setq buf (mm-handle-buffer handles)))
	(let (good)
	  (unwind-protect
	      (progn
		(funcall func handle)
		(setq good t))
	    (unless good
	      (mm-destroy-parts handles))))
	(mm-handle-set-cache handle handles))
      (setq gnus-article-mime-handles
	    (mm-merge-handles gnus-article-mime-handles handles)))
    (unless no-display
      (save-excursion
	(save-restriction
	  (narrow-to-region (point) (point))
	  (gnus-display-mime (mm-handle-cache handle))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(condition-case nil
		    ;; This is only valid on XEmacs.
		    (mapcar (lambda (prop)
			    (remove-specifier
			     (face-property 'default prop) (current-buffer)))
			    '(background background-pixmap foreground))
		  (error nil))
		(delete-region ,(point-min-marker) ,(point-max-marker))))))))))

(provide 'mm-extern)

;;; arch-tag: 9653808e-14d9-4172-86e6-adceaa05378e
;;; mm-extern.el ends here
