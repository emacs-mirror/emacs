;;; nnagent.el --- offline backend for Gnus
;; Copyright (C) 1997,98 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

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
(require 'nnoo)
(eval-when-compile (require 'cl))
(require 'gnus-agent)
(require 'nnml)

(nnoo-declare nnagent
  nnml)



(defconst nnagent-version "nnagent 1.0")

(defvoo nnagent-directory nil
  "Internal variable."
  nnml-directory)

(defvoo nnagent-active-file nil
  "Internal variable."
  nnml-active-file)

(defvoo nnagent-newsgroups-file nil
  "Internal variable."
  nnml-newsgroups-file)

(defvoo nnagent-get-new-mail nil
  "Internal variable."
  nnml-get-new-mail)

;;; Interface functions.

(nnoo-define-basics nnagent)

(deffoo nnagent-open-server (server &optional defs)
  (setq defs
	`((nnagent-directory ,(gnus-agent-directory))
	  (nnagent-active-file ,(gnus-agent-lib-file "active"))
	  (nnagent-newsgroups-file ,(gnus-agent-lib-file "newsgroups"))
	  (nnagent-get-new-mail nil)))
  (nnoo-change-server 'nnagent server defs)
  (let ((dir (gnus-agent-directory))
	err)
    (cond
     ((not (condition-case arg
	       (file-exists-p dir)
	     (ftp-error (setq err (format "%s" arg)))))
      (nnagent-close-server)
      (nnheader-report
       'nnagent (or err
		    (format "No such file or directory: %s" dir))))
     ((not (file-directory-p (file-truename dir)))
      (nnagent-close-server)
      (nnheader-report 'nnagent "Not a directory: %s" dir))
     (t
      (nnheader-report 'nnagent "Opened server %s using directory %s"
		       server dir)
      t))))

(deffoo nnagent-retrieve-groups (groups &optional server)
  (save-excursion
    (cond
     ((file-exists-p (gnus-agent-lib-file "groups"))
      (nnmail-find-file (gnus-agent-lib-file "groups"))
      'groups)
     ((file-exists-p (gnus-agent-lib-file "active"))
      (nnmail-find-file (gnus-agent-lib-file "active"))
      'active)
     (t nil))))

(defun nnagent-request-type (group article)
  (unless (stringp article)
    (let ((gnus-plugged t))
      (if (not (gnus-check-backend-function
		'request-type (car gnus-command-method)))
	  'unknown
	(funcall (gnus-get-function gnus-command-method 'request-type)
		 (gnus-group-real-name group) article)))))

(deffoo nnagent-request-newgroups (date server)
  nil)

(deffoo nnagent-request-update-info (group info &optional server)
  nil)

(deffoo nnagent-request-post (&optional server)
  (gnus-agent-insert-meta-information 'news gnus-command-method)
  (gnus-request-accept-article "nndraft:queue"))

;; Use nnml functions for just about everything.
(nnoo-import nnagent
  (nnml))


;;; Internal functions.

(provide 'nnagent)

;;; nnagent.el ends here
