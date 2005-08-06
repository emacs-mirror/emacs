;;; spam-report.el --- Reporting spam

;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
  "Spam reporting configuration."
  :group 'mail
  :group 'news)

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
  "Function to use for url ping spam reporting.
The function must accept the arguments `host' and `report'."
  :type '(choice
	  (const :tag "Connect directly"
		 spam-report-url-ping-plain)
	  (const :tag "Use the external program specified in `mm-url-program'"
		 spam-report-url-ping-mm-url)
	  (const :tag "Store request URLs in `spam-report-requests-file'"
		 spam-report-url-to-file)
	  (function :tag "User defined function" nil))
  :group 'spam-report)

(defcustom spam-report-requests-file
  (nnheader-concat gnus-directory "spam/" "spam-report-requests.url")
  ;; Is there a convention for the extension of such a file?
  ;; Should we use `spam-directory'?
  "File where spam report request are stored."
  :type 'file
  :group 'spam-report)

(defvar spam-report-url-ping-temp-agent-function nil
  "Internal variable for `spam-report-agentize' and `spam-report-deagentize'.
This variable will store the value of `spam-report-url-ping-function' from
before `spam-report-agentize' was run, so that `spam-report-deagentize' can
undo that change.")

(defun spam-report-gmane (&rest articles)
  "Report an article as spam through Gmane"
  (dolist (article articles)
    (when (and gnus-newsgroup-name
	       (or (null spam-report-gmane-regex)
		   (string-match spam-report-gmane-regex gnus-newsgroup-name)))
      (gnus-message 6 "Reporting spam article %d to spam.gmane.org..." article)
      (if spam-report-gmane-use-article-number
	  (spam-report-url-ping
	   "spam.gmane.org"
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

;;;###autoload
(defun spam-report-process-queue (&optional file keep)
  "Report all queued requests from `spam-report-requests-file'.

If FILE is given, use it instead of `spam-report-requests-file'.
If KEEP is t, leave old requests in the file.  If KEEP is the
symbol `ask', query before flushing the queue file."
  (interactive
   (list (read-file-name
	  "File: "
	  (file-name-directory spam-report-requests-file)
	  spam-report-requests-file
	  nil
	  (file-name-nondirectory spam-report-requests-file))
	 current-prefix-arg))
  (if (eq spam-report-url-ping-function 'spam-report-url-to-file)
      (error (concat "Cannot process requests when "
		     "`spam-report-url-ping-function' is "
		     "`spam-report-url-to-file'."))
    (gnus-message 7 "Processing requests using `%s'."
		  spam-report-url-ping-function))
  (or file (setq file spam-report-requests-file))
  (save-excursion
    (set-buffer (find-file-noselect file))
    (goto-char (point-min))
    (while (and (not (eobp))
		(re-search-forward
		 "http://\\([^/]+\\)\\(/.*\\) *$" (gnus-point-at-eol) t))
      (funcall spam-report-url-ping-function (match-string 1) (match-string 2))
      (forward-line 1))
    (if (or (eq keep nil)
	    (and (eq keep 'ask)
		 (y-or-n-p
		  (format
		   "Flush requests from `%s'? " (current-buffer)))))
	(progn
	  (gnus-message 7 "Flushing request file `%s'"
			spam-report-requests-file)
	  (erase-buffer)
	  (save-buffer)
	  (kill-buffer (current-buffer)))
      (gnus-message 7 "Keeping requests in `%s'" spam-report-requests-file))))

;;;###autoload
(defun spam-report-url-ping-mm-url (host report)
  "Ping a host through HTTP, addressing a specific GET resource. Use
the external program specified in `mm-url-program' to connect to
server."
  (with-temp-buffer
    (let ((url (format "http://%s%s" host report)))
      (mm-url-insert url t))))

;;;###autoload
(defun spam-report-url-to-file (host report)
  "Collect spam report requests in `spam-report-requests-file'.
Customize `spam-report-url-ping-function' to use this function."
  (let ((url (format "http://%s%s" host report))
	(file spam-report-requests-file))
    (gnus-make-directory (file-name-directory file))
    (gnus-message 9 "Writing URL `%s' to file `%s'" url file)
    (with-temp-buffer
      (insert url)
      (newline)
      (append-to-file (point-min) (point-max) file))))

;;;###autoload
(defun spam-report-agentize ()
  "Add spam-report support to the Agent.
Spam reports will be queued with \\[spam-report-url-to-file] when
the Agent is unplugged, and will be submitted in a batch when the
Agent is plugged."
  (interactive)
  (add-hook 'gnus-agent-plugged-hook 'spam-report-plug-agent)
  (add-hook 'gnus-agent-unplugged-hook 'spam-report-unplug-agent))

;;;###autoload
(defun spam-report-deagentize ()
  "Remove spam-report support from the Agent.
Spam reports will be queued with the method used when
\\[spam-report-agentize] was run."
  (interactive)
  (remove-hook 'gnus-agent-plugged-hook 'spam-report-plug-agent)
  (remove-hook 'gnus-agent-unplugged-hook 'spam-report-unplug-agent))

(defun spam-report-plug-agent ()
  "Adjust spam report settings for plugged state.
Process queued spam reports."
  ;; Process the queue, unless the user only wanted to report to a file
  ;; anyway.
  (unless (equal spam-report-url-ping-temp-agent-function
		 'spam-report-url-to-file)
    (spam-report-process-queue))
  ;; Set the reporting function, if we have memorized something otherwise,
  ;; stick with plain URL reporting.
  (setq spam-report-url-ping-function
	(or spam-report-url-ping-temp-agent-function
	    'spam-report-url-ping-plain)))

(defun spam-report-unplug-agent ()
  "Restore spam report settings for unplugged state."
  ;; save the old value
  (setq spam-report-url-ping-temp-agent-function
	spam-report-url-ping-function)
  ;; store all reports to file
  (setq spam-report-url-ping-function
	'spam-report-url-to-file))

(provide 'spam-report)

;;; arch-tag: f6683295-ec89-4ab5-8803-8cc842293022
;;; spam-report.el ends here.
