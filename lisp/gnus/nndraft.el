;;; nndraft.el --- draft article access for Gnus
;; Copyright (C) 1995,96,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

;;; Code:

(require 'nnheader)
(require 'nnmh)
(require 'nnoo)
(eval-and-compile (require 'cl))

(nnoo-declare nndraft)

(eval-and-compile
  (autoload 'mail-send-and-exit "sendmail"))

(defvoo nndraft-directory nil
  "Where nndraft will store its directory.")



(defconst nndraft-version "nndraft 1.0")
(defvoo nndraft-status-string "")



;;; Interface functions.

(nnoo-define-basics nndraft)

(deffoo nndraft-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let* ((buf (get-buffer-create " *draft headers*"))
	   article)
      (set-buffer buf)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;; We don't support fetching by Message-ID.
      (if (stringp (car articles))
	  'headers
	(while articles
	  (set-buffer buf)
	  (when (nndraft-request-article
		 (setq article (pop articles)) group server (current-buffer))
	    (goto-char (point-min))
	    (if (search-forward "\n\n" nil t)
		(forward-line -1)
	      (goto-char (point-max)))
	    (delete-region (point) (point-max))
	    (set-buffer nntp-server-buffer)
	    (goto-char (point-max))
	    (insert (format "221 %d Article retrieved.\n" article))
	    (insert-buffer-substring buf)
	    (insert ".\n")))

	(nnheader-fold-continuation-lines)
	'headers))))

(deffoo nndraft-open-server (server &optional defs)
  (nnoo-change-server 'nndraft server defs)
  (unless (assq 'nndraft-directory defs)
    (setq nndraft-directory server))
  (cond
   ((not (file-exists-p nndraft-directory))
    (nndraft-close-server)
    (nnheader-report 'nndraft "No such file or directory: %s"
		     nndraft-directory))
   ((not (file-directory-p (file-truename nndraft-directory)))
    (nndraft-close-server)
    (nnheader-report 'nndraft "Not a directory: %s" nndraft-directory))
   (t
    (nnheader-report 'nndraft "Opened server %s using directory %s"
		     server nndraft-directory)
    t)))

(deffoo nndraft-request-article (id &optional group server buffer)
  (when (numberp id)
    ;; We get the newest file of the auto-saved file and the
    ;; "real" file.
    (let* ((file (nndraft-article-filename id))
	   (auto (nndraft-auto-save-file-name file))
	   (newest (if (file-newer-than-file-p file auto) file auto))
	   (nntp-server-buffer (or buffer nntp-server-buffer)))
      (when (and (file-exists-p newest)
		 (nnmail-find-file newest))
	(save-excursion
	  (set-buffer nntp-server-buffer)
	  (goto-char (point-min))
	  ;; If there's a mail header separator in this file,
	  ;; we remove it.
	  (when (re-search-forward
		 (concat "^" mail-header-separator "$") nil t)
	    (replace-match "" t t)))
	t))))

(deffoo nndraft-request-restore-buffer (article &optional group server)
  "Request a new buffer that is restored to the state of ARTICLE."
  (let ((file (nndraft-article-filename article ".state"))
	nndraft-point nndraft-mode nndraft-buffer-name)
    (when (file-exists-p file)
      (load file t t t)
      (when nndraft-buffer-name
	(set-buffer (get-buffer-create
		     (generate-new-buffer-name nndraft-buffer-name)))
	(nndraft-request-article article group server (current-buffer))
	(funcall nndraft-mode)
	(let ((gnus-verbose-backends nil))
	  (nndraft-request-expire-articles (list article) group server t))
	(goto-char nndraft-point))
      nndraft-buffer-name)))

(deffoo nndraft-request-update-info (group info &optional server)
  (setcar (cddr info) nil)
  (when (nth 3 info)
    (setcar (nthcdr 3 info) nil))
  t)

(deffoo nndraft-request-associate-buffer (group)
  "Associate the current buffer with some article in the draft group."
  (let* ((gnus-verbose-backends nil)
	 (article (cdr (nndraft-request-accept-article
			group (nnoo-current-server 'nndraft) t 'noinsert)))
	 (file (nndraft-article-filename article)))
    (setq buffer-file-name file)
    (setq buffer-auto-save-file-name (make-auto-save-file-name))
    (clear-visited-file-modtime)
    article))

(deffoo nndraft-request-group (group &optional server dont-check)
  (prog1
      (nndraft-execute-nnmh-command
       `(nnmh-request-group group "" ,dont-check))
    (nnheader-report 'nndraft nnmh-status-string)))

(deffoo nndraft-request-list (&optional server dir)
  (nndraft-execute-nnmh-command
   `(nnmh-request-list nil ,dir)))

(deffoo nndraft-request-newgroups (date &optional server)
  (nndraft-execute-nnmh-command
   `(nnmh-request-newgroups ,date ,server)))

(deffoo nndraft-request-expire-articles
  (articles group &optional server force)
  (let ((res (nndraft-execute-nnmh-command
	      `(nnmh-request-expire-articles
		',articles group ,server ,force)))
	article)
    ;; Delete all the "state" files of articles that have been expired.
    (while articles
      (unless (memq (setq article (pop articles)) res)
	(let ((file (nndraft-article-filename article ".state"))
	      (auto (nndraft-auto-save-file-name
		     (nndraft-article-filename article))))
	  (when (file-exists-p file)
	    (funcall nnmail-delete-file-function file))
	  (when (file-exists-p auto)
	    (funcall nnmail-delete-file-function auto)))))
    res))

(deffoo nndraft-request-accept-article (group &optional server last noinsert)
  (let* ((point (point))
	 (mode major-mode)
	 (name (buffer-name))
	 (gnus-verbose-backends nil)
	 (gart (nndraft-execute-nnmh-command
		`(nnmh-request-accept-article group ,server ,last noinsert)))
	 (state
	  (nndraft-article-filename (cdr gart) ".state")))
    ;; Write the "state" file.
    (save-excursion
      (nnheader-set-temp-buffer " *draft state*")
      (insert (format "%S\n" `(setq nndraft-mode (quote ,mode)
				    nndraft-point ,point
				    nndraft-buffer-name ,name)))
      (write-region (point-min) (point-max) state nil 'silent)
      (kill-buffer (current-buffer)))
    gart))

(deffoo nndraft-close-group (group &optional server)
  t)

(deffoo nndraft-request-create-group (group &optional server args)
  (if (file-exists-p nndraft-directory)
      (if (file-directory-p nndraft-directory)
	  t
	nil)
    (condition-case ()
	(progn
	  (gnus-make-directory nndraft-directory)
	  t)
      (file-error nil))))


;;; Low-Level Interface

(defun nndraft-execute-nnmh-command (command)
  (let ((dir (expand-file-name nndraft-directory)))
    (when (string-match "/$" dir)
      (setq dir (substring dir 0 (match-beginning 0))))
    (string-match "/[^/]+$" dir)
    (let ((group (substring dir (1+ (match-beginning 0))))
          (nnmh-directory (substring dir 0 (1+ (match-beginning 0))))
	  (nnmail-keep-last-article nil)
	  (nnmh-get-new-mail nil))
      (eval command))))

(defun nndraft-article-filename (article &rest args)
  (apply 'concat
	 (file-name-as-directory nndraft-directory)
	 (int-to-string article)
	 args))

(defun nndraft-auto-save-file-name (file)
  (save-excursion
    (prog1
	(progn
	  (set-buffer (get-buffer-create " *draft tmp*"))
	  (setq buffer-file-name file)
	  (make-auto-save-file-name))
      (kill-buffer (current-buffer)))))

(provide 'nndraft)

;;; nndraft.el ends here
