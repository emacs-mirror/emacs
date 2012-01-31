;;; imap-hash.el --- Hashtable-like interface to an IMAP mailbox

;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides hashtable-like functions on top of imap.el
;; functionality.  All the authentication is handled by auth-source so
;; there are no authentication options here, only the server and
;; mailbox names are needed.

;; Create a IHT (imap-hash table) object with `imap-hash-make'.  Then
;; use it with `imap-hash-map' to map a function across all the
;; messages.  Use `imap-hash-get' and `imap-hash-rem' to operate on
;; individual messages.  See the tramp-imap.el library in Tramp if you
;; need to see practical examples.

;; This only works with IMAP4r1.  Sorry to everyone without it, but
;; the compatibility code is too annoying and it's 2009.

;; TODO: Use SEARCH instead of FETCH when a test is specified.  List
;; available mailboxes.  Don't select an invalid mailbox.

;;; Code:

(require 'assoc)
(require 'imap)
(require 'sendmail)			; for mail-header-separator
(require 'message)
(autoload 'auth-source-user-or-password "auth-source")

;; retrieve these headers
(defvar imap-hash-headers
  (append '(Subject From Date Message-Id References In-Reply-To Xref)))

;; from nnheader.el
(defsubst imap-hash-remove-cr-followed-by-lf ()
  (goto-char (point-max))
  (while (search-backward "\r\n" nil t)
    (delete-char 1)))

;; from nnheader.el
(defun imap-hash-ms-strip-cr (&optional string)
  "Strip ^M from the end of all lines in current buffer or STRING."
  (if string
    (with-temp-buffer
      (insert string)
      (imap-hash-remove-cr-followed-by-lf)
      (buffer-string))
    (save-excursion
      (imap-hash-remove-cr-followed-by-lf))))

(defun imap-hash-make (server port mailbox &optional user password ssl)
  "Make a new imap-hash object using SERVER, PORT, and MAILBOX.
USER, PASSWORD and SSL are optional.
The test is set to t, meaning all messages are considered."
  (when (and server port mailbox)
    (list :server server :port port :mailbox mailbox
	  :ssl ssl :user user :password password
	  :test t)))

(defun imap-hash-p (iht)
  "Check whether IHT is a valid imap-hash."
  (and
   (imap-hash-server iht)
   (imap-hash-port iht)
   (imap-hash-mailbox iht)
   (imap-hash-test iht)))

(defmacro imap-hash-gather (uid)
  `(imap-message-get ,uid 'BODYDETAIL))

(defmacro imap-hash-data-body (details)
  `(nth 2 (nth 1 ,details)))

(defmacro imap-hash-data-headers (details)
  `(nth 2 (nth 0 ,details)))

(defun imap-hash-get (key iht &optional refetch)
  "Get the value for KEY in the imap-hash IHT.
Requires either `imap-hash-fetch' to be called beforehand
\(e.g. by `imap-hash-map'), or REFETCH to be t.
Returns a list of the headers (an alist, see `imap-hash-map') and
the body of the message as a string.
Also see `imap-hash-test'."
  (with-current-buffer (imap-hash-get-buffer iht)
    (when refetch
      (imap-hash-fetch iht nil key))
  (let ((details (imap-hash-gather key)))
      (list
       (imap-hash-get-headers
	(imap-hash-data-headers details))
       (imap-hash-get-body
	(imap-hash-data-body details))))))

(defun imap-hash-put (value iht &optional key)
  "Put VALUE in the imap-hash IHT.  Return the new key.
If KEY is given, removes it.
VALUE can be a list of the headers (an alist, see `imap-hash-map')
and the body of the message as a string.  It can also be a uid,
in which case `imap-hash-get' will be called to get the value.
Also see `imap-hash-test'."
  (let ((server-buffer (imap-hash-get-buffer iht))
	(value (if (listp value) value (imap-hash-get value iht)))
	newuid)
      (when value
	(with-temp-buffer
	  (funcall 'imap-hash-make-message
		   (nth 0 value)
		   (nth 1 value)
		   nil)
	  (setq newuid (nth 1 (imap-message-append
			       (imap-hash-mailbox iht)
			       (current-buffer) nil nil server-buffer)))
	  (when key (imap-hash-rem key iht))))
      newuid))

(defun imap-hash-make-message (headers body &optional overrides)
  "Make a message with HEADERS and BODY suitable for `imap-append',
using `message-setup'.
Look in the alist OVERRIDES for header overrides as per `imap-hash-headers'."
  ;; don't insert a signature no matter what
  (let (message-signature)
    (message-setup
     (append overrides headers))
    (message-generate-headers message-required-mail-headers)
    (message-remove-header "X-Draft-From")
    (message-goto-body)
    (insert (or (aget overrides 'body)
		body
		""))
    (goto-char (point-min))
    ;; TODO: make this search better
    (if (search-forward mail-header-separator nil t)
	(delete-region (line-beginning-position) (line-end-position))
      (error "Could not find the body separator in the encoded message!"))))

(defun imap-hash-rem (key iht)
  "Remove KEY in the imap-hash IHT.
Also see `imap-hash-test'.  Requires `imap-hash-fetch' to have
been called and the imap-hash server buffer to be current,
so it's best to use it inside `imap-hash-map'.
The key will not be found on the next `imap-hash-map' call."
  (with-current-buffer (imap-hash-get-buffer iht)
    (imap-message-flags-add
     (imap-range-to-message-set (list key))
     "\\Deleted" 'silent)
    (imap-mailbox-expunge t)))

(defun imap-hash-clear (iht)
  "Remove all keys in the imap-hash IHT.
Also see `imap-hash-test'."
  (imap-hash-map (lambda (uid b c) (imap-hash-rem uid iht)) iht))

(defun imap-hash-get-headers (text-headers)
  (with-temp-buffer
    (insert (or text-headers ""))
    (imap-hash-remove-cr-followed-by-lf)
    (mapcar (lambda (header)
	      (cons header
		    (message-fetch-field (format "%s" header))))
	    imap-hash-headers)))

(defun imap-hash-get-body (text)
  (with-temp-buffer
    (insert (or text ""))
    (imap-hash-remove-cr-followed-by-lf)
    (buffer-string)))

(defun imap-hash-map (function iht &optional headers-only &rest messages)
  "Call FUNCTION for all entries in IHT and pass it the message uid,
the headers (an alist, see `imap-hash-headers'), and the body
contents as a string.  If HEADERS-ONLY is not nil, the body will be nil.
Returns results of evaluating, as would `mapcar'.
If MESSAGES are given, iterate only over those UIDs.
Also see `imap-hash-test'."
  (imap-hash-fetch iht headers-only)
  (let ((test (imap-hash-test iht)))
    (with-current-buffer (imap-hash-get-buffer iht)
      (delq nil
	    (imap-message-map (lambda (message ignored-parameter)
				(let* ((details (imap-hash-gather message))
				       (headers (imap-hash-data-headers details))
				       (hlist (imap-hash-get-headers headers))
				       (runit (cond
					       ((stringp test)
						(string-match
						 test
						 (format "%s" (aget hlist 'Subject))))
					       ((functionp test)
						(funcall test hlist))
					       ;; otherwise, return test itself
					       (t test))))
				  ;;(debug message headers)
				  (when runit
				    (funcall function
					     message
					     (imap-hash-get-headers
					      headers)
					     (imap-hash-get-body
					      (imap-hash-data-body details))))))
			      "UID")))))

(defun imap-hash-count (iht)
  "Count the number of messages in the imap-hash IHT.
Also see `imap-hash-test'.  It uses `imap-hash-map' so just use that
function if you want to do more than count the elements."
  (length (imap-hash-map (lambda (a b c)) iht t)))

(defalias 'imap-hash-size 'imap-hash-count)

(defun imap-hash-test (iht)
  "Return the test used by `imap-hash-map' for IHT.
When the test is t, any key will be a candidate.
When the test is a string, messages will be filtered on that string as a
regexp against the subject.
When the test is a function, messages will be filtered with it.
The function is passed the message headers (see `imap-hash-get-headers')."
  (plist-get iht :test))

(defun imap-hash-server (iht)
  "Return the server used by the imap-hash IHT."
  (plist-get iht :server))

(defun imap-hash-port (iht)
  "Return the port used by the imap-hash IHT."
  (plist-get iht :port))

(defun imap-hash-ssl (iht)
  "Return the SSL need for the imap-hash IHT."
  (plist-get iht :ssl))

(defun imap-hash-mailbox (iht)
  "Return the mailbox used by the imap-hash IHT."
  (plist-get iht :mailbox))

(defun imap-hash-user (iht)
  "Return the username used by the imap-hash IHT."
  (plist-get iht :user))

(defun imap-hash-password (iht)
  "Return the password used by the imap-hash IHT."
  (plist-get iht :password))

(defun imap-hash-open-connection (iht)
  "Open the connection used for IMAP interactions with the imap-hash IHT."
  (let* ((server (imap-hash-server iht))
	 (port (imap-hash-port iht))
	 (ssl-need (imap-hash-ssl iht))
	 (auth-need (not (and (imap-hash-user iht)
			      (imap-hash-password iht))))
	 ;; this will not be needed if auth-need is t
	 (auth-info (when auth-need
		      (auth-source-user-or-password
		       '("login" "password")
		       server port)))
	 (auth-user (or (imap-hash-user iht)
			(nth 0 auth-info)))
	 (auth-passwd (or (imap-hash-password iht)
			  (nth 1 auth-info)))
	 (imap-logout-timeout nil))

	;; (debug "opening server: opened+state" (imap-opened) imap-state)
	;; this is the only place where IMAP vs IMAPS matters
	(if (imap-open server port (if ssl-need 'ssl nil) nil (current-buffer))
	    (progn
	      ;; (debug "after opening server: opened+state" (imap-opened (current-buffer)) imap-state)
	      ;; (debug "authenticating" auth-user auth-passwd)
	      (if (not (imap-capability 'IMAP4rev1))
		  (error "IMAP server does not support IMAP4r1, it won't work, sorry")
		(imap-authenticate auth-user auth-passwd)
		(imap-id)
		;; (debug "after authenticating: opened+state" (imap-opened (current-buffer)) imap-state)
		(imap-opened (current-buffer))))
	  (error "Could not open the IMAP buffer"))))

(defun imap-hash-get-buffer (iht)
  "Get or create the connection buffer to be used for the imap-hash IHT."
  (let* ((name (imap-hash-buffer-name iht))
	 (buffer (get-buffer name)))
  (if (and buffer (imap-opened buffer))
      buffer
    (when buffer (kill-buffer buffer))
    (with-current-buffer (get-buffer-create name)
      (setq buffer-undo-list t)
      (when (imap-hash-open-connection iht)
	(current-buffer))))))

(defun imap-hash-buffer-name (iht)
  "Get the connection buffer to be used for the imap-hash IHT."
  (when (imap-hash-p iht)
    (let ((server (imap-hash-server iht))
	  (port (imap-hash-port iht))
	  (ssl-text (if (imap-hash-ssl iht) "SSL" "NoSSL")))
      (format "*imap-hash/%s:%s:%s*" server port ssl-text))))

(defun imap-hash-fetch (iht &optional headers-only &rest messages)
  "Fetch all the messages for imap-hash IHT.
Get only the headers if HEADERS-ONLY is not nil."
  (with-current-buffer (imap-hash-get-buffer iht)
    (let ((range (if messages
		     (list
		      (imap-range-to-message-set messages)
		      (imap-range-to-message-set messages))
		   '("1:*" . "1,*:*"))))

      ;; (with-current-buffer "*imap-debug*"
      ;;   (erase-buffer))
      (imap-mailbox-unselect)
      (imap-mailbox-select (imap-hash-mailbox iht))
      ;; (debug "after selecting mailbox: opened+state" (imap-opened) imap-state)
	  ;; (setq imap-message-data (make-vector imap-message-prime 0)
      (imap-fetch-safe range
		       (concat (format "(UID RFC822.SIZE BODY %s "
				       (if headers-only "" "BODY.PEEK[TEXT]"))
			       (format "BODY.PEEK[HEADER.FIELDS %s])"
				     imap-hash-headers))))))

(provide 'imap-hash)
;;; imap-hash.el ends here

;; ignore, for testing only

;;; (setq iht (imap-hash-make "yourhosthere.com" "imap" "INBOX.test"))
;;; (setq iht (imap-hash-make "yourhosthere.com" "imap" "test"))
;;; (imap-hash-make "server1" "INBOX.mailbox2")
;;; (imap-hash-p iht)
;;; (imap-hash-get 35 iht)
;;; (imap-hash-get 38 iht)
;;; (imap-hash-get 37 iht t)
;;; (mapc (lambda (buffer) (with-current-buffer buffer (erase-buffer))) '("*imap-debug*" "*imap-log*"))
;;; (imap-hash-put (imap-hash-get 5 iht) iht)
;;; (with-current-buffer (imap-hash-get-buffer iht) (let ((uid (imap-hash-put (imap-hash-get 5 iht) iht))) (imap-hash-put uid iht uid)))
;;; (imap-hash-put (imap-hash-get 35 iht) iht)
;;; (imap-hash-make-message '((Subject . "normal")) "normal body")
;;; (imap-hash-make-message '((Subject . "old")) "old body" '((Subject . "new")))
;;; (imap-hash-make-message '((Subject . "old")) "old body" '((body . "new body")) (lambda (subject) (concat "overwrite-" subject)))
;;; (imap-hash-make-message '((Subject . "old")) "old body" '((Subject . "change this")) (lambda (subject) (concat "overwrite-" subject)))
;;; (imap-hash-make-message '((Subject . "Twelcome")) "body here" nil)
;; (with-current-buffer (imap-hash-get-buffer iht) (imap-hash-rem (imap-hash-put (imap-hash-get 5 iht) iht) iht))
;;; (kill-buffer (imap-hash-buffer-name iht))
;;; (imap-hash-map 'debug iht)
;;; (imap-hash-map 'debug iht t)
;;;(tramp-imap-handle-file-inode "/imap:yourhosthere.com:/test/welcome")
;;;(imap-hash-count iht)
;;; (mapc (lambda (buffer) (with-current-buffer buffer (erase-buffer))) '("*imap-debug*" "*imap-log*"))
;;; (kill-buffer (imap-hash-buffer-name iht))
;;; this should always return t if the server is up, automatically reopening if needed
;;; (imap-opened (imap-hash-get-buffer iht))
;;; (imap-hash-buffer-name iht)
;;; (with-current-buffer (imap-hash-get-buffer iht) (debug "mailbox data, auth and state" imap-mailbox-data imap-auth imap-state))
;;;(tramp-imap-handle-file-inode "/imap:yourhosthere.com:/test/welcome")
;;; (imap-hash-fetch iht nil)
;;; (imap-hash-fetch iht t)
;;; (imap-hash-fetch iht nil 1 2 3)
;;; (imap-hash-fetch iht t 1 2 3)

;; arch-tag: 071410ac-91dc-4e36-b892-18e057d639c5
