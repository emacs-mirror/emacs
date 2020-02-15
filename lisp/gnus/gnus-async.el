;;; gnus-async.el --- asynchronous support for Gnus  -*- lexical-binding:t -*-

;; Copyright (C) 1996-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gnus)
(require 'gnus-sum)
(require 'nntp)

(defgroup gnus-asynchronous nil
  "Support for asynchronous operations."
  :group 'gnus)

(defcustom gnus-use-article-prefetch 30
  "If non-nil, prefetch articles in groups that allow this.
If a number, prefetch only that many articles forward;
if t, prefetch as many articles as possible."
  :type '(choice (const :tag "off" nil)
		 (const :tag "all" t)
		 (integer :tag "some" 0)))

(defcustom gnus-asynchronous nil
  "If nil, inhibit all Gnus asynchronicity.
If non-nil, let the other asynch variables be heeded."
  :type 'boolean)

(defcustom gnus-prefetched-article-deletion-strategy '(read exit)
  "List of symbols that say when to remove articles from the prefetch buffer.
Possible values in this list are `read', which means that
articles are removed as they are read, and `exit', which means
that all articles belonging to a group are removed on exit
from that group."
  :type '(set (const read) (const exit)))

(defcustom gnus-use-header-prefetch nil
  "If non-nil, prefetch the headers to the next group."
  :type 'boolean)

(defcustom gnus-async-prefetch-article-p #'gnus-async-unread-p
  "Function called to say whether an article should be prefetched or not.
The function is called with one parameter -- the article data.
It should return non-nil if the article is to be prefetched."
  :type 'function)

(defcustom gnus-async-post-fetch-function #'ignore
  "Function called after an article has been prefetched.
The function will be called narrowed to the region of the article
that was fetched."
  :version "27.1"
  :type 'function)

;;; Internal variables.

(defvar gnus-async-prefetch-article-buffer " *Async Prefetch Article*")
(defvar gnus-async-article-alist nil)
(defvar gnus-async-article-semaphore '(nil))
(defvar gnus-async-fetch-list nil)
(defvar gnus-async-current-prefetch-group nil)
(defvar gnus-async-current-prefetch-article nil)
(defvar gnus-async-timer nil)

(defvar gnus-async-prefetch-headers-buffer " *Async Prefetch Headers*")
(defvar gnus-async-header-prefetched nil)

;;; Utility functions.

(defun gnus-group-asynchronous-p (group)
  "Say whether GROUP is fetched from a server that supports asynchronicity."
  (gnus-asynchronous-p (gnus-find-method-for-group group)))

;;; Somewhat bogus semaphores.

(defun gnus-async-get-semaphore (semaphore)
  "Wait until SEMAPHORE is released."
  (while (/= (length (nconc (symbol-value semaphore) (list nil))) 2)
    (sleep-for 1)))

(defun gnus-async-release-semaphore (semaphore)
  "Release SEMAPHORE."
  (setcdr (symbol-value semaphore) nil))

(defmacro gnus-async-with-semaphore (&rest forms)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
	 (gnus-async-get-semaphore 'gnus-async-article-semaphore)
	 ,@forms)
     (gnus-async-release-semaphore 'gnus-async-article-semaphore)))

;;;
;;; Article prefetch
;;;

(gnus-add-shutdown 'gnus-async-close 'gnus)
(defun gnus-async-close ()
  (gnus-kill-buffer gnus-async-prefetch-article-buffer)
  (gnus-kill-buffer gnus-async-prefetch-headers-buffer)
  (setq gnus-async-article-alist nil
	gnus-async-header-prefetched nil))

(defun gnus-async-set-buffer ()
  (nnheader-set-temp-buffer gnus-async-prefetch-article-buffer t))

(defun gnus-async-halt-prefetch ()
  "Stop prefetching."
  (setq gnus-async-fetch-list nil))

(defun gnus-async-prefetch-next (group article summary)
  "Possibly prefetch several articles starting with the article after ARTICLE."
  (when (and (gnus-buffer-live-p summary)
	     gnus-asynchronous
	     (gnus-group-asynchronous-p group))
    (with-current-buffer gnus-summary-buffer
      (let ((next (cadr (gnus-data-find-list article))))
	(when next
	  (when gnus-async-timer
	    (ignore-errors
	      (cancel-timer 'gnus-async-timer)))
	  (setq gnus-async-timer
		(run-with-idle-timer
		 0.1 nil
		 (lambda ()
		   ;; When running from a timer, `C-g' is inhibited.
		   ;; But the prefetch action may (when there's a
		   ;; network problem or the like) hang (or take a
		   ;; long time), so allow quitting anyway.
		   (let ((inhibit-quit nil))
		     (gnus-async-prefetch-article
		      group (gnus-data-number next) summary))))))))))

(defun gnus-async-prefetch-article (group article summary &optional next)
  "Possibly prefetch several articles starting with ARTICLE."
  (if (not (gnus-buffer-live-p summary))
      (gnus-async-with-semaphore
	(setq gnus-async-fetch-list nil))
    (when (and gnus-asynchronous
	       (gnus-alive-p))
      (when next
	(gnus-async-with-semaphore
	  (pop gnus-async-fetch-list)))
      (let ((do-fetch next)
	    (do-message t))		;(eq major-mode 'gnus-summary-mode)))
	(when (and (gnus-group-asynchronous-p group)
		   (gnus-buffer-live-p summary)
		   (or (not next)
		       gnus-async-fetch-list))
	  (gnus-async-with-semaphore
	    (unless next
	      (setq do-fetch (not gnus-async-fetch-list))
	      ;; Nix out any outstanding requests.
	      (setq gnus-async-fetch-list nil)
	      ;; Fill in the new list.
	      (let ((n gnus-use-article-prefetch)
		    (data (gnus-data-find-list article))
		    d)
		(while (and (setq d (pop data))
			    (if (numberp n)
				(natnump (cl-decf n))
			      n))
		  (unless (or (gnus-async-prefetched-article-entry
			       group (setq article (gnus-data-number d)))
			      (not (natnump article))
			      (not (funcall gnus-async-prefetch-article-p d)))
		    ;; Not already fetched -- so we add it to the list.
		    (push article gnus-async-fetch-list)))
		(setq gnus-async-fetch-list
		      (nreverse gnus-async-fetch-list))))

	    (when do-fetch
	      (setq article (car gnus-async-fetch-list))))

	  (when (and do-fetch article)
	    ;; We want to fetch some more articles.
	    (with-current-buffer summary
	      (let (mark)
		(gnus-async-set-buffer)
		(goto-char (point-max))
		(setq mark (point-marker))
		(let ((nnheader-callback-function
		       (gnus-make-async-article-function
			group article mark summary next))
		      (nntp-server-buffer
		       (get-buffer gnus-async-prefetch-article-buffer)))
		  (when do-message
		    (gnus-message 9 "Prefetching article %d in group %s"
				  article group))
		  (setq gnus-async-current-prefetch-group group)
		  (setq gnus-async-current-prefetch-article article)
		  (gnus-request-article article group))))))))))

(defun gnus-make-async-article-function (group article mark summary next)
  "Return a callback function."
  (lambda (arg)
    (gnus-async-article-callback arg group article mark summary next)))

(defun gnus-async-article-callback (arg group article mark summary next)
  "Function called when an async article is done being fetched."
  (save-excursion
    (setq gnus-async-current-prefetch-article nil)
    (when arg
      (gnus-async-set-buffer)
      (save-excursion
	(save-restriction
	  (narrow-to-region mark (point-max))
	  ;; Put the articles into the agent, if they aren't already.
	  (when (and gnus-agent
		     (gnus-agent-group-covered-p group))
	    (save-restriction
	      (narrow-to-region mark (point-max))
	      (gnus-agent-store-article article group)))
	  ;; Prefetch images for the groups that want that.
	  (when (fboundp 'gnus-html-prefetch-images)
	    (gnus-html-prefetch-images summary))
	  (when gnus-async-post-fetch-function
	    (funcall gnus-async-post-fetch-function summary))))
      (gnus-async-with-semaphore
	(push (list (format "%s-%d" group article)
		    mark (point-max-marker)
		    group article)
	      gnus-async-article-alist)))
    (if (not (gnus-buffer-live-p summary))
	(gnus-async-with-semaphore
	  (setq gnus-async-fetch-list nil))
      (gnus-async-prefetch-article group next summary t))))

(defun gnus-async-unread-p (data)
  "Return non-nil if DATA represents an unread article."
  (gnus-data-unread-p data))

(defun gnus-async-request-fetched-article (group article buffer)
  "See whether we have ARTICLE from GROUP and put it in BUFFER."
  (when (numberp article)
    (when (and (equal group gnus-async-current-prefetch-group)
	       (eq article gnus-async-current-prefetch-article))
      (gnus-async-wait-for-article article))
    (let ((entry (gnus-async-prefetched-article-entry group article)))
      (when entry
	(save-excursion
	  (gnus-async-set-buffer)
	  (copy-to-buffer buffer (cadr entry) (caddr entry))
	  ;; Remove the read article from the prefetch buffer.
	  (when (memq 'read gnus-prefetched-article-deletion-strategy)
	    (gnus-async-delete-prefetched-entry entry))
	  t)))))

(defun gnus-async-wait-for-article (article)
  "Wait until ARTICLE is no longer the currently-being-fetched article."
  (save-excursion
    (gnus-async-set-buffer)
    (let ((proc (nntp-find-connection (current-buffer)))
	  (nntp-server-buffer (current-buffer))
	  (nntp-have-messaged nil)
	  (tries 0))
      (when proc
	(condition-case nil
	    ;; FIXME: we could stop waiting after some
	    ;; timeout, but this is the wrong place to do it.
	    ;; rather than checking time-spent-waiting, we
	    ;; should check time-since-last-output, which
	    ;; needs to be done in nntp.el.
	    (while (eq article gnus-async-current-prefetch-article)
	      (cl-incf tries)
	      (when (nntp-accept-process-output proc)
		(setq tries 0))
	      (when (and (not nntp-have-messaged)
			 (= tries 3))
		(gnus-message 5 "Waiting for async article...")
		(setq nntp-have-messaged t)))
	  (quit
	   ;; if the user interrupted on a slow/hung connection,
	   ;; do something friendly.
	   (when (> tries 3)
	     (setq gnus-async-current-prefetch-article nil))
	   (signal 'quit nil)))
	(when nntp-have-messaged
	  (gnus-message 5 ""))))))

(defun gnus-async-delete-prefetched-entry (entry)
  "Delete ENTRY from buffer and alist."
  (ignore-errors
    (delete-region (cadr entry) (caddr entry))
    (set-marker (cadr entry) nil)
    (set-marker (caddr entry) nil))
  (gnus-async-with-semaphore
    (setq gnus-async-article-alist
	  (delete entry gnus-async-article-alist))))

(defun gnus-async-prefetch-remove-group (group)
  "Remove all articles belonging to GROUP from the prefetch buffer."
  (when (and (gnus-group-asynchronous-p group)
	     (memq 'exit gnus-prefetched-article-deletion-strategy))
    (save-excursion
      (gnus-async-set-buffer)
      (dolist (entry gnus-async-article-alist)
	(when (equal group (nth 3 entry))
	  (gnus-async-delete-prefetched-entry entry))))))

(defun gnus-async-prefetched-article-entry (group article)
  "Return the entry for ARTICLE in GROUP if it has been prefetched."
  (let ((entry (save-excursion
		 (gnus-async-set-buffer)
		 (assoc (format "%s-%d" group article)
			gnus-async-article-alist))))
    ;; Perhaps something has emptied the buffer?
    (if (and entry
	     (= (cadr entry) (caddr entry)))
	(progn
	  (ignore-errors
	    (set-marker (cadr entry) nil)
	    (set-marker (caddr entry) nil))
	  (setq gnus-async-article-alist
		(delete entry gnus-async-article-alist))
	  nil)
      entry)))

;;;
;;; Header prefetch
;;;

(defun gnus-async-prefetch-headers (group)
  "Prefetch the headers for group GROUP."
  (save-excursion
    (let (unread)
      (when (and gnus-use-header-prefetch
		 gnus-asynchronous
		 (gnus-group-asynchronous-p group)
		 (listp gnus-async-header-prefetched)
		 (setq unread (gnus-list-of-unread-articles group)))
	;; Mark that a fetch is in progress.
	(setq gnus-async-header-prefetched t)
	(nnheader-set-temp-buffer gnus-async-prefetch-headers-buffer t)
	(erase-buffer)
	(let ((nntp-server-buffer (current-buffer))
	      (nnheader-callback-function
	       (lambda (_arg)
		  (setq gnus-async-header-prefetched
			(cons group unread)))))
	  (gnus-retrieve-headers unread group gnus-fetch-old-headers))))))

(defun gnus-async-retrieve-fetched-headers (articles group)
  "See whether we have prefetched headers."
  (when (and gnus-use-header-prefetch
	     (gnus-group-asynchronous-p group)
	     (listp gnus-async-header-prefetched)
	     (equal group (car gnus-async-header-prefetched))
	     (equal articles (cdr gnus-async-header-prefetched)))
    (save-excursion
      (nnheader-set-temp-buffer gnus-async-prefetch-headers-buffer t)
      (nntp-decode-text)
      (copy-to-buffer nntp-server-buffer (point-min) (point-max))
      (erase-buffer)
      (setq gnus-async-header-prefetched nil)
      t)))

(provide 'gnus-async)

;;; gnus-async.el ends here
