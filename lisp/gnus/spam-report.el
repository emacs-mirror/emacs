;;; spam-report.el --- Reporting spam
;; Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: network

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; This module addresses a few aspects of spam reporting under Gnus.  Page
;;; breaks are used for grouping declarations and documentation relating to
;;; each particular aspect.

;;; Code:
(require 'gnus)
(require 'gnus-sum)

(eval-and-compile
  (autoload 'mm-url-insert "mm-url"))

(defgroup spam-report nil
  "Spam reporting configuration.")

(defcustom spam-report-gmane-regex nil
  "Regexp matching Gmane newsgroups, e.g. \"^nntp\\+.*:gmane\\.\"
If you are using spam.el, consider setting gnus-spam-process-newsgroups
or the gnus-group-spam-exit-processor-report-gmane group/topic parameter
instead."
  :type '(radio (const nil)
		(regexp :value "^nntp\+.*:gmane\."))
  :group 'spam-report)

(defcustom spam-report-gmane-spam-header
  "^X-Report-Spam: http://\\([^/]+\\)\\(.*\\)$"
  "String matching Gmane spam-reporting header.  Two match groups are needed."
  :type 'regexp
  :group 'spam-report)

(defcustom spam-report-gmane-use-article-number t
  "Whether the article number (faster!) or the header should be used."
  :type 'boolean
  :group 'spam-report)

(defcustom spam-report-url-ping-function
  'spam-report-url-ping-plain
  "Function to use for url ping spam reporting."
  :type '(choice
	  (const :tag "Connect directly"
		 spam-report-url-ping-plain)
	  (const :tag "Use the external program specified in `mm-url-program'"
		 spam-report-url-ping-mm-url))
  :group 'spam-report)

(defun spam-report-gmane (&rest articles)
  "Report an article as spam through Gmane"
  (dolist (article articles)
    (when (and gnus-newsgroup-name
	       (or (null spam-report-gmane-regex)
		   (string-match spam-report-gmane-regex gnus-newsgroup-name)))
      (gnus-message 6 "Reporting spam article %d to spam.gmane.org..." article)
      (if spam-report-gmane-use-article-number
	  (spam-report-url-ping "spam.gmane.org"
				(format "/%s:%d"
					(gnus-group-real-name gnus-newsgroup-name)
					article))
	(with-current-buffer nntp-server-buffer
	  (gnus-request-head article gnus-newsgroup-name)
	  (goto-char (point-min))
	  (if (re-search-forward spam-report-gmane-spam-header nil t)
	      (let* ((host (match-string 1))
		     (report (match-string 2))
		     (url (format "http://%s%s" host report)))
		(gnus-message 7 "Reporting spam through URL %s..." url)
		(spam-report-url-ping host report))
	    (gnus-message 3 "Could not find X-Report-Spam in article %d..."
			  article)))))))

(defun spam-report-url-ping (host report)
  "Ping a host through HTTP, addressing a specific GET resource using
the function specified by `spam-report-url-ping-function'."
  (funcall spam-report-url-ping-function host report))

(defun spam-report-url-ping-plain (host report)
  "Ping a host through HTTP, addressing a specific GET resource."
  (let ((tcp-connection))
    (with-temp-buffer
      (or (setq tcp-connection
		(open-network-stream
		 "URL ping"
		 (buffer-name)
		 host
		 80))
	  (error "Could not open connection to %s" host))
      (set-marker (process-mark tcp-connection) (point-min))
      (process-send-string
       tcp-connection
       (format "GET %s HTTP/1.1\nUser-Agent: %s (spam-report.el)\nHost: %s\n\n"
	       report (gnus-emacs-version) host)))))

(defun spam-report-url-ping-mm-url (host report)
  "Ping a host through HTTP, addressing a specific GET resource. Use
the external program specified in `mm-url-program' to connect to
server."
  (with-temp-buffer
    (let ((url (concat "http://" host "/" report)))
      (mm-url-insert url t))))

(provide 'spam-report)

;;; arch-tag: f6683295-ec89-4ab5-8803-8cc842293022
;;; spam-report.el ends here.
