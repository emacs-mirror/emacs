;;; nndir.el --- single directory newsgroup access for Gnus

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
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
(require 'nnml)

(eval-and-compile
  (autoload 'mail-send-and-exit "sendmail"))



(defconst nndir-version "nndir 1.0")

(defvar nndir-current-directory nil
  "Current news group directory.")

(defvar nndir-status-string "")

(defvar nndir-nov-is-evil nil
  "*Non-nil means that nndir will never retrieve NOV headers.")



;;; Interface functions.


(defun nndir-retrieve-headers (sequence &optional newsgroup server)
  (nndir-execute-nnml-command
   '(nnml-retrieve-headers sequence group server) server))

(defun nndir-open-server (host &optional service)
  "Open nndir backend."
  (setq nndir-status-string "")
  (nnheader-init-server-buffer))

(defun nndir-close-server (&optional server)
  "Close news server."
  t)

(defun nndir-server-opened (&optional server)
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nndir-status-message (&optional server)
  "Return server status response as string."
  nndir-status-string)

(defun nndir-request-article (id &optional newsgroup server buffer)
  (nndir-execute-nnmh-command
   '(nnmh-request-article id group server buffer) server))

(defun nndir-request-group (group &optional server dont-check)
  "Select news GROUP."
  (nndir-execute-nnmh-command
   '(nnmh-request-group group "" dont-check) server))

(defun nndir-request-list (&optional server dir)
  "Get list of active articles in all newsgroups."
  (nndir-execute-nnmh-command
   '(nnmh-request-list nil dir) server))

(defun nndir-request-newgroups (date &optional server)
  (nndir-execute-nnmh-command
   '(nnmh-request-newgroups date server) server))

(defun nndir-request-post (&optional server)
  "Post a new news in current buffer."
  (mail-send-and-exit nil))

(defalias 'nndir-request-post-buffer 'nnmail-request-post-buffer)

(defun nndir-request-expire-articles (articles newsgroup &optional server force)
  "Expire all articles in the ARTICLES list in group GROUP."
  (setq nndir-status-string "nndir: expire not possible")
  nil)

(defun nndir-close-group (group &optional server)
  t)

(defun nndir-request-move-article (article group server accept-form)
  (setq nndir-status-string "nndir: move not possible")
  nil)

(defun nndir-request-accept-article (group)
  (setq nndir-status-string "nndir: accept not possible")
  nil)


;;; Low-Level Interface

(defun nndir-execute-nnmh-command (command server)
  (let ((dir (expand-file-name server)))
    (and (string-match "/$" dir)
	 (setq dir (substring dir 0 (match-beginning 0))))
    (string-match "/[^/]+$" dir)
    (let ((group (substring dir (1+ (match-beginning 0))))
	  (nnmh-directory (substring dir 0 (1+ (match-beginning 0))))
	  (nnmh-get-new-mail nil))
      (eval command))))

(defun nndir-execute-nnml-command (command server)
  (let ((dir (expand-file-name server)))
    (and (string-match "/$" dir)
	 (setq dir (substring dir 0 (match-beginning 0))))
    (string-match "/[^/]+$" dir)
    (let ((group (substring dir (1+ (match-beginning 0))))
	  (nnml-directory (substring dir 0 (1+ (match-beginning 0))))
	  (nnml-nov-is-evil nndir-nov-is-evil)
	  (nnml-get-new-mail nil))
      (eval command))))

(provide 'nndir)

;;; nndir.el ends here
