;;; url-queue.el --- Fetching web pages in parallel   -*- lexical-binding: t -*-

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: comm

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

;; The point of this package is to allow fetching web pages in
;; parallel -- but control the level of parallelism to avoid DoS-ing
;; web servers and Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'browse-url)
(require 'url-parse)
(require 'with-url)

(defcustom url-queue-parallel-processes 6
  "The number of concurrent processes."
  :version "24.1"
  :type 'integer
  :group 'url)

(defcustom url-queue-timeout 10
  "How long to let a job live once it's started (in seconds)."
  :version "26.1"
  :type 'integer
  :group 'url)

;;; Internal variables.

(defvar url-queue nil)
(defvar url-queue-progress-timer nil)

(cl-defstruct url-queue
  url callback cbargs silentp
  buffer start-time pre-triggered
  inhibit-cookiesp context-buffer)

;;;###autoload
(defun url-queue-retrieve (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
This is like `url-retrieve' (which see for details of the arguments),
but with limits on the degree of parallelism.  The variable
`url-queue-parallel-processes' sets the number of concurrent processes.
The variable `url-queue-timeout' sets a timeout."
  (setq url-queue
	(append url-queue
		(list (make-url-queue :url url
				      :callback callback
				      :cbargs cbargs
				      :silentp silent
				      :inhibit-cookiesp inhibit-cookies
                                      :context-buffer (current-buffer)))))
  (url-queue-setup-runners))

;; To ensure asynch behavior, we start the required number of queue
;; runners from `run-with-idle-timer'.  So we're basically going
;; through the queue in two ways: 1) synchronously when a program
;; calls `url-queue-retrieve' (which will then start the required
;; number of queue runners), and 2) at the exit of each job, which
;; will then not start any further threads, but just reuse the
;; previous "slot".

(defun url-queue-setup-runners ()
  (let ((running 0)
	waiting)
    (dolist (entry url-queue)
      (cond
       ((or (url-queue-start-time entry)
	    (url-queue-pre-triggered entry))
	(cl-incf running))
       ((not waiting)
	(setq waiting entry))))
    (when (and waiting
	       (< running url-queue-parallel-processes))
      (setf (url-queue-pre-triggered waiting) t)
      ;; We start fetching from this idle timer...
      (run-with-idle-timer 0.01 nil #'url-queue-run-queue)
      ;; And then we set up a separate timer to ensure progress when a
      ;; web server is unresponsive.
      (unless url-queue-progress-timer
        (setq url-queue-progress-timer
              (run-with-idle-timer 1 1 #'url-queue-check-progress))))))

(defun url-queue-run-queue ()
  (let ((running 0)
	waiting)
    (dolist (entry url-queue)
      (cond
       ((url-queue-start-time entry)
	(cl-incf running))
       ((not waiting)
	(setq waiting entry))))
    (when (and waiting
	       (< running url-queue-parallel-processes))
      (setf (url-queue-start-time waiting) (float-time))
      (url-queue-start-retrieve waiting))))

(defun url-queue-check-progress ()
  (when url-queue-progress-timer
    (if url-queue
        (url-queue-run-queue)
      (cancel-timer url-queue-progress-timer)
      (setq url-queue-progress-timer nil))))

(defun url-queue-callback-function (job)
  (setq url-queue (delq job url-queue))
  (when (and (url-errorp)
             ;; FIXME: Push the connection failed status to the status
	     (eq (url-status 'response) 500))
    ;; If we get a connection error, then flush all other jobs from
    ;; the host from the queue.  This particularly makes sense if the
    ;; error really is a DNS resolver issue, which happens
    ;; synchronously and totally halts Emacs.
    (url-queue-remove-jobs-from-host (url-host
                                      (url-generic-parse-url
                                       (url-queue-url job)))))
  (url-queue-run-queue)
  (apply (url-queue-callback job) (url-queue-cbargs job)))

(defun url-queue-remove-jobs-from-host (host)
  (dolist (job url-queue)
    (when (equal (url-host (url-generic-parse-url (url-queue-url job)))
                 host)
      (setq url-queue (delq job url-queue)))))

(defun url-queue-start-retrieve (job)
  (with-fetched-url ((url-queue-url job)
                     :verbose (if (url-queue-silentp job)
                                  0 5)
                     :cookies (not (url-queue-inhibit-cookiesp job))
                     :read-timeout url-queue-timeout)
    (url-queue-callback-function job)))

(provide 'url-queue)

;;; url-queue.el ends here
