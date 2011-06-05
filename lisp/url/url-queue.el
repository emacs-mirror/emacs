;;; url-queue.el --- Fetching web pages in parallel

;; Copyright (C) 2011 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The point of this package is to allow fetching web pages in
;; parallel -- but control the level of parallelism to avoid DoS-ing
;; web servers and Emacs.

;;; Code:

(eval-when-compile (require 'cl))
(require 'browse-url)

(defcustom url-queue-parallel-processes 6
  "The number of concurrent processes."
  :version "24.1"
  :type 'integer
  :group 'url)

(defcustom url-queue-timeout 5
  "How long to let a job live once it's started (in seconds)."
  :version "24.1"
  :type 'integer
  :group 'url)

;;; Internal variables.

(defvar url-queue nil)

(defstruct url-queue
  url callback cbargs silentp
  buffer start-time)

;;;###autoload
(defun url-queue-retrieve (url callback &optional cbargs silent)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
Like `url-retrieve' (which see for details of the arguments), but
controls the level of parallelism via the
`url-queue-parallel-processes' variable."
  (setq url-queue
	(append url-queue
		(list (make-url-queue :url url
				      :callback callback
				      :cbargs cbargs
				      :silentp silent))))
  (url-queue-run-queue))

(defun url-queue-run-queue ()
  (url-queue-prune-old-entries)
  (let ((running 0)
	waiting)
    (dolist (entry url-queue)
      (cond
       ((url-queue-start-time entry)
	(incf running))
       ((not waiting)
	(setq waiting entry))))
    (when (and waiting
	       (< running url-queue-parallel-processes))
      (setf (url-queue-start-time waiting) (float-time))
      (url-queue-start-retrieve waiting))))

(defun url-queue-callback-function (status job)
  (setq url-queue (delq job url-queue))
  (url-queue-run-queue)
  (apply (url-queue-callback job) (cons status (url-queue-cbargs job))))

(defun url-queue-start-retrieve (job)
  (setf (url-queue-buffer job)
	(ignore-errors
	  (url-retrieve (url-queue-url job)
			#'url-queue-callback-function (list job)
			(url-queue-silentp job)))))

(defun url-queue-prune-old-entries ()
  (let (dead-jobs)
    (dolist (job url-queue)
      ;; Kill jobs that have lasted longer than the timeout.
      (when (and (url-queue-start-time job)
		 (> (- (float-time) (url-queue-start-time job))
		    url-queue-timeout))
	(push job dead-jobs)))
    (dolist (job dead-jobs)
      (when (bufferp (url-queue-buffer job))
	(while (get-buffer-process (url-queue-buffer job))
	  (ignore-errors
	    (delete-process (get-buffer-process (url-queue-buffer job)))))
	(ignore-errors
	  (kill-buffer (url-queue-buffer job))))
      (setq url-queue (delq job url-queue)))))

(provide 'url-queue)

;;; url-queue.el ends here
