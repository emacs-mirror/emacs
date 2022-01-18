;;; url.el --- Uniform Resource Locator retrieval tool  -*- lexical-binding: t -*-

;; Copyright (C) 1996-1999, 2001, 2004-2022 Free Software Foundation,
;; Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
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

;; Registered URI schemes: https://www.iana.org/assignments/uri-schemes

;;; Code:


(require 'mailcap)

(eval-when-compile
  (require 'mm-decode)
  (require 'mm-view))

(require 'url-vars)
(require 'url-cookie)
(require 'url-history)
(require 'url-expand)
(require 'url-privacy)
(require 'url-methods)
(require 'url-proxy)
(require 'url-parse)
(require 'url-util)


(defcustom url-configuration-directory
  (locate-user-emacs-file "url/" ".url/")
  "Directory used by the URL package for cookies, history, etc."
  :type 'directory
  :group 'url)

(defun url-do-setup ()
  "Setup the URL package.
This is to avoid conflict with user settings if URL is dumped with
Emacs."
  (unless url-setup-done

    (mailcap-parse-mailcaps)
    (mailcap-parse-mimetypes)

    ;; Register all the authentication schemes we can handle
    (url-register-auth-scheme "basic" nil 4)
    (url-register-auth-scheme "digest" nil 7)

    (setq url-cookie-file
	  (or url-cookie-file
	      (expand-file-name "cookies" url-configuration-directory)))

    (setq url-history-file
	  (or url-history-file
	      (expand-file-name "history" url-configuration-directory)))

    ;; Parse the global history file if it exists, so that it can be used
    ;; for URL completion, etc.
    (url-history-parse-history)
    (url-history-setup-save-timer)

    ;; Ditto for cookies
    (url-cookie-setup-save-timer)
    (url-cookie-parse-file url-cookie-file)

    ;; Read in proxy gateways
    (let ((noproxy (and (not (assoc "no_proxy" url-proxy-services))
			(or (getenv "NO_PROXY")
			    (getenv "no_PROXY")
			    (getenv "no_proxy")))))
      (if noproxy
	  (setq url-proxy-services
		(cons (cons "no_proxy"
			    (concat "\\("
				    (mapconcat
				     (lambda (x)
				       (cond
					((= x ?,) "\\|")
					((= x ? ) "")
					((= x ?.) (regexp-quote "."))
					((= x ?*) ".*")
					((= x ??) ".")
					(t (char-to-string x))))
				     noproxy "") "\\)"))
		      url-proxy-services))))

    (url-setup-privacy-info)
    (run-hooks 'url-load-hook)
    (setq url-setup-done t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieval functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar url-redirect-buffer nil
  "New buffer into which the retrieval will take place.
Sometimes while retrieving a URL, the URL library needs to use another buffer
than the one returned initially by `url-retrieve'.  In this case, it sets this
variable in the original buffer as a forwarding pointer.")

(defvar url-retrieve-number-of-calls 0)
(autoload 'url-cache-prune-cache "url-cache")
(defvar url-asynchronous t
  "Bind to nil before calling `url-retrieve' to signal :nowait connections.")

;;;###autoload
(defun url-retrieve (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
URL is either a string or a parsed URL.  If it is a string
containing characters that are not valid in a URI, those
characters are percent-encoded; see `url-encode-url'.

CALLBACK is called when the object has been completely retrieved, with
the current buffer containing the object, and any MIME headers associated
with it.  It is called as (apply CALLBACK STATUS CBARGS).
STATUS is a plist representing what happened during the request,
with most recent events first, or an empty list if no events have
occurred.  Each pair is one of:

\(:redirect REDIRECTED-TO) - the request was redirected to this URL.

\(:error (error type . DATA)) - an error occurred.  TYPE is a
symbol that says something about where the error occurred, and
DATA is a list (possibly nil) that describes the error further.

Return the buffer URL will load into, or nil if the process has
already completed (i.e. URL was a mailto URL or similar; in this case
the callback is not called).

The variables `url-request-data', `url-request-method' and
`url-request-extra-headers' can be dynamically bound around the
request; dynamic binding of other variables doesn't necessarily
take effect.

If SILENT, then don't message progress reports and the like.
If INHIBIT-COOKIES, cookies will neither be stored nor sent to
the server.
If URL is a multibyte string, it will be encoded as utf-8 and
URL-encoded before it's used."
  ;; XXX: There is code in Emacs that does dynamic binding
  ;; of the following variables around url-retrieve:
  ;; url-standalone-mode, url-gateway-unplugged, w3-honor-stylesheets,
  ;; url-confirmation-func, url-cookie-multiple-line,
  ;; url-cookie-{{,secure-}storage,confirmation}
  ;; url-standalone-mode and url-gateway-unplugged should work as
  ;; usual.  url-confirmation-func is only used in nnwarchive.el and
  ;; webmail.el; the latter should be updated.  Is
  ;; url-cookie-multiple-line needed anymore?  The other url-cookie-*
  ;; are (for now) only used in synchronous retrievals.
  (url-retrieve-internal url callback (cons nil cbargs) silent
			 inhibit-cookies))

(defun url-retrieve-internal (url callback cbargs &optional silent
				  inhibit-cookies)
  "Internal function; external interface is `url-retrieve'.
The callback function will receive an updated value of CBARGS as
arguments; its first element should be a plist specifying what has
happened so far during the request, as described in the docstring
of `url-retrieve' (if in doubt, specify nil).

If SILENT, don't message progress reports and the like.
If INHIBIT-COOKIES, cookies will neither be stored nor sent to
the server.
If URL is a multibyte string, it will be encoded as utf-8 and
URL-encoded before it's used."
  (url-do-setup)
  (url-gc-dead-buffers)
  (when (stringp url)
    (set-text-properties 0 (length url) nil url)
    (setq url (url-encode-url url)))
  (if (not (url-p url))
      (setq url (url-generic-parse-url url)))
  (if (not (functionp callback))
      (error "Must provide a callback function to url-retrieve"))
  (unless (url-type url)
    (error "Bad url: %s" (url-recreate-url url)))
  (setf (url-silent url) silent)
  (setf (url-asynchronous url) url-asynchronous)
  (setf (url-use-cookies url) (not inhibit-cookies))
  ;; Once in a while, remove old entries from the URL cache.
  (when (zerop (% url-retrieve-number-of-calls 1000))
    (condition-case error
	(url-cache-prune-cache)
      (file-error
       (message "Error when expiring the cache: %s" error))))
  (setq url-retrieve-number-of-calls (1+ url-retrieve-number-of-calls))
  (let ((loader (url-scheme-get-property (url-type url) 'loader))
	(url-using-proxy (if (url-host url)
			     (url-find-proxy-for-url url (url-host url))))
	(buffer nil)
	(asynch (url-scheme-get-property (url-type url) 'asynchronous-p)))
    (when url-using-proxy
      (setf asynch t
	    loader #'url-proxy
            (url-asynchronous url) t))
    (if asynch
	(let ((url-current-object url))
	  (setq buffer (funcall loader url callback cbargs)))
      (setq buffer (funcall loader url))
      (if buffer
	  (with-current-buffer buffer
	    (apply callback cbargs))))
    (if url-history-track
	(url-history-update-url url (current-time)))
    buffer))

;;;###autoload
(defun url-retrieve-synchronously (url &optional silent inhibit-cookies timeout)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL.

If SILENT is non-nil, don't do any messaging while retrieving.
If INHIBIT-COOKIES is non-nil, refuse to store cookies.  If
TIMEOUT is passed, it should be a number that says (in seconds)
how long to wait for a response before giving up."
  (url-do-setup)
  (let* (url-asynchronous
         data-buffer
         (callback (lambda (&rest _args)
                     (setq data-buffer (current-buffer))
                     (url-debug 'retrieval
                                "Synchronous fetching done (%S)"
                                data-buffer)))
         (start-time (current-time))
         (proc-buffer (url-retrieve url callback nil silent
                                    inhibit-cookies)))
    (if (not proc-buffer)
        (url-debug 'retrieval "Synchronous fetching unnecessary %s" url)
      (unwind-protect
          (catch 'done
            (while (not data-buffer)
              (when (and timeout (time-less-p timeout
                                              (time-since start-time)))
                (url-debug 'retrieval "Timed out %s (after %ss)" url
                           (float-time (time-since start-time)))
                (throw 'done 'timeout))
	      (url-debug 'retrieval
		         "Spinning in url-retrieve-synchronously: nil (%S)"
		         proc-buffer)
              (when-let ((redirect-buffer
                          (buffer-local-value 'url-redirect-buffer
                                              proc-buffer)))
                (unless (eq redirect-buffer proc-buffer)
                  (url-debug
                   'retrieval "Redirect in url-retrieve-synchronously: %S -> %S"
		   proc-buffer redirect-buffer)
                  (let (kill-buffer-query-functions)
                    (kill-buffer proc-buffer))
                  ;; Accommodate hack in commit 55d1d8b.
                  (setq proc-buffer redirect-buffer)))
              (when-let ((proc (get-buffer-process proc-buffer)))
                (when (memq (process-status proc)
                            '(closed exit signal failed))
                  ;; Process sentinel vagaries occasionally cause
                  ;; url-retrieve to fail calling callback.
                  (unless data-buffer
                    (url-debug 'retrieval "Dead process %s" url)
		    (throw 'done 'exception))))
              ;; Querying over consumer internet in the US takes 100
              ;; ms, so split the difference.
              (accept-process-output nil 0.05)))
        (unless (eq data-buffer proc-buffer)
          (let (kill-buffer-query-functions)
            (kill-buffer proc-buffer)))))
    data-buffer))

;; url-mm-callback called from url-mm, which requires mm-decode.
(declare-function mm-dissect-buffer "mm-decode"
		  (&optional no-strict-mime loose-mime from))
(declare-function mm-display-part "mm-decode"
		  (handle &optional no-default force))

(defun url-mm-callback (&rest ignored)
  (let ((handle (mm-dissect-buffer t)))
    (url-mark-buffer-as-dead (current-buffer))
    (with-current-buffer
        (generate-new-buffer (url-recreate-url url-current-object))
      (if (eq (mm-display-part handle) 'external)
	  (progn
	    (set-process-sentinel
	     ;; Fixme: this shouldn't have to know the form of the
	     ;; undisplayer produced by `mm-display-part'.
	     (get-buffer-process (cdr (mm-handle-undisplayer handle)))
	     `(lambda (proc event)
		(mm-destroy-parts (quote ,handle))))
	    (message "Viewing externally")
	    (kill-buffer (current-buffer)))
	(display-buffer (current-buffer))
	(add-hook 'kill-buffer-hook
		  `(lambda () (mm-destroy-parts ',handle))
		  nil
		  t)))))

(defun url-mm-url (url)
  "Retrieve URL and pass to the appropriate viewing application."
  ;; These requires could advantageously be moved to url-mm-callback or
  ;; turned into autoloads, but I suspect that it would introduce some bugs
  ;; because loading those files from a process sentinel or filter may
  ;; result in some undesirable corner cases.
  (require 'mm-decode)
  (require 'mm-view)
  (url-retrieve url 'url-mm-callback nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-dead-buffer-list nil)

(defun url-mark-buffer-as-dead (buff)
  (push buff url-dead-buffer-list))

(defun url-gc-dead-buffers ()
  (let ((buff))
    (while (setq buff (pop url-dead-buffer-list))
      (if (buffer-live-p buff)
	  (kill-buffer buff)))))

(define-obsolete-function-alias 'url-warn #'display-warning "28.1")

(provide 'url)

;;; url.el ends here
