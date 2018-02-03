;;; tramp-cache.el --- file information caching for Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2000, 2005-2018 Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@inanna.danann.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Maintainer: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

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

;; An implementation of information caching for remote files.

;; Each connection, identified by a `tramp-file-name' structure or by
;; a process, has a unique cache. We distinguish 3 kind of caches,
;; depending on the key:
;;
;; - localname is NIL.  This are reusable properties.  Examples:
;;   "remote-shell" identifies the POSIX shell to be called on the
;;   remote host, or "perl" is the command to be called on the remote
;;   host when starting a Perl script.  These properties are saved in
;;   the file `tramp-persistency-file-name'.
;;
;; - localname is a string.  This are temporary properties, which are
;;   related to the file localname is referring to.  Examples:
;;   "file-exists-p" is t or nil, depending on the file existence, or
;;   "file-attributes" caches the result of the function
;;   `file-attributes'.  These entries have a timestamp, and they
;;   expire after `remote-file-name-inhibit-cache' seconds if this
;;   variable is set.
;;
;; - The key is a process.  This are temporary properties related to
;;   an open connection.  Examples: "scripts" keeps shell script
;;   definitions already sent to the remote shell, "last-cmd-time" is
;;   the time stamp a command has been sent to the remote process.

;;; Code:

(require 'tramp)
(autoload 'time-stamp-string "time-stamp")

;;; -- Cache --

;;;###tramp-autoload
(defvar tramp-cache-data (make-hash-table :test 'equal)
  "Hash table for remote files properties.")

;;;###tramp-autoload
(defcustom tramp-connection-properties nil
  "List of static connection properties.
Every entry has the form (REGEXP PROPERTY VALUE).  The regexp
matches remote file names.  It can be nil.  PROPERTY is a string,
and VALUE the corresponding value.  They are used, if there is no
matching entry for PROPERTY in `tramp-cache-data'.  For more
details see the info pages."
  :group 'tramp
  :version "24.4"
  :type '(repeat (list (choice :tag "File Name regexp" regexp (const nil))
		       (choice :tag "        Property" string)
		       (choice :tag "           Value" sexp)))
  :require 'tramp)

;;;###tramp-autoload
(defcustom tramp-persistency-file-name
  (expand-file-name (locate-user-emacs-file "tramp"))
  "File which keeps connection history for Tramp connections."
  :group 'tramp
  :type 'file
  :require 'tramp)

(defvar tramp-cache-data-changed nil
  "Whether persistent cache data have been changed.")

(defun tramp-get-hash-table (key)
  "Returns the hash table for KEY.
If it doesn't exist yet, it is created and initialized with
matching entries of `tramp-connection-properties'."
  (or (gethash key tramp-cache-data)
      (let ((hash
	     (puthash key (make-hash-table :test 'equal) tramp-cache-data)))
	(when (tramp-file-name-p key)
	  (dolist (elt tramp-connection-properties)
	    (when (string-match
		   (or (nth 0 elt) "")
		   (tramp-make-tramp-file-name key 'noloc 'nohop))
	      (tramp-set-connection-property key (nth 1 elt) (nth 2 elt)))))
	hash)))

;;;###tramp-autoload
(defun tramp-get-file-property (key file property default)
  "Get the PROPERTY of FILE from the cache context of KEY.
Returns DEFAULT if not set."
  ;; Unify localname.  Remove hop from `tramp-file-name' structure.
  (setq file (tramp-compat-file-name-unquote file)
	key (copy-tramp-file-name key))
  (setf (tramp-file-name-localname key)
	(tramp-run-real-handler 'directory-file-name (list file))
	(tramp-file-name-hop key) nil)
  (let* ((hash (tramp-get-hash-table key))
	 (value (when (hash-table-p hash) (gethash property hash))))
    (if ;; We take the value only if there is any, and
	;; `remote-file-name-inhibit-cache' indicates that it is still
	;; valid.  Otherwise, DEFAULT is set.
	(and (consp value)
	     (or (null remote-file-name-inhibit-cache)
		 (and (integerp remote-file-name-inhibit-cache)
		      (<=
		       (tramp-time-diff (current-time) (car value))
		       remote-file-name-inhibit-cache))
		 (and (consp remote-file-name-inhibit-cache)
		      (time-less-p
		       remote-file-name-inhibit-cache (car value)))))
	(setq value (cdr value))
      (setq value default))

    (tramp-message key 8 "%s %s %s" file property value)
    (when (>= tramp-verbose 10)
      (let* ((var (intern (concat "tramp-cache-get-count-" property)))
	     (val (or (bound-and-true-p var)
		      (progn
			(add-hook 'tramp-cache-unload-hook
				  (lambda () (makunbound var)))
			0))))
	(set var (1+ val))))
    value))

;;;###tramp-autoload
(defun tramp-set-file-property (key file property value)
  "Set the PROPERTY of FILE to VALUE, in the cache context of KEY.
Returns VALUE."
  ;; Unify localname.  Remove hop from `tramp-file-name' structure.
  (setq file (tramp-compat-file-name-unquote file)
	key (copy-tramp-file-name key))
  (setf (tramp-file-name-localname key)
	(tramp-run-real-handler 'directory-file-name (list file))
	(tramp-file-name-hop key) nil)
  (let ((hash (tramp-get-hash-table key)))
    ;; We put the timestamp there.
    (puthash property (cons (current-time) value) hash)
    (tramp-message key 8 "%s %s %s" file property value)
    (when (>= tramp-verbose 10)
      (let* ((var (intern (concat "tramp-cache-set-count-" property)))
	     (val (or (bound-and-true-p var)
		      (progn
			(add-hook 'tramp-cache-unload-hook
				  (lambda () (makunbound var)))
			0))))
	(set var (1+ val))))
    value))

;;;###tramp-autoload
(defun tramp-flush-file-property (key file property)
  "Remove PROPERTY of FILE in the cache context of KEY."
  ;; Unify localname.  Remove hop from `tramp-file-name' structure.
  (setq file (tramp-compat-file-name-unquote file)
	key (copy-tramp-file-name key))
  (setf (tramp-file-name-localname key)
	(tramp-run-real-handler 'directory-file-name (list file))
	(tramp-file-name-hop key) nil)
  (remhash property (tramp-get-hash-table key))
  (tramp-message key 8 "%s %s" file property)
  (when (>= tramp-verbose 10)
    (let ((var (intern (concat "tramp-cache-set-count-" property))))
      (makunbound var))))

;;;###tramp-autoload
(defun tramp-flush-file-properties (key file)
  "Remove all properties of FILE in the cache context of KEY."
  (let* ((file (tramp-run-real-handler
		'directory-file-name (list file)))
	 (truename (tramp-get-file-property key file "file-truename" nil)))
    ;; Unify localname.  Remove hop from `tramp-file-name' structure.
    (setq file (tramp-compat-file-name-unquote file)
	  key (copy-tramp-file-name key))
    (setf (tramp-file-name-localname key) file
	  (tramp-file-name-hop key) nil)
    (tramp-message key 8 "%s" file)
    (remhash key tramp-cache-data)
    ;; Remove file properties of symlinks.
    (when (and (stringp truename)
	       (not (string-equal file (directory-file-name truename))))
      (tramp-flush-file-properties key truename))))

;;;###tramp-autoload
(defun tramp-flush-directory-properties (key directory)
  "Remove all properties of DIRECTORY in the cache context of KEY.
Remove also properties of all files in subdirectories."
  (setq directory (tramp-compat-file-name-unquote directory))
  (let* ((directory (tramp-run-real-handler
		    'directory-file-name (list directory)))
	 (truename (tramp-get-file-property key directory "file-truename" nil)))
    (tramp-message key 8 "%s" directory)
    (maphash
     (lambda (key _value)
       (when (and (tramp-file-name-p key)
		  (stringp (tramp-file-name-localname key))
		  (string-match (regexp-quote directory)
				(tramp-file-name-localname key)))
	 (remhash key tramp-cache-data)))
     tramp-cache-data)
    ;; Remove file properties of symlinks.
    (when (and (stringp truename)
	       (not (string-equal directory (directory-file-name truename))))
      (tramp-flush-directory-properties key truename))))

;; Reverting or killing a buffer should also flush file properties.
;; They could have been changed outside Tramp.  In eshell, "ls" would
;; not show proper directory contents when a file has been copied or
;; deleted before.  We must apply `save-match-data', because it would
;; corrupt other packages otherwise (reported from org).
(defun tramp-flush-file-function ()
  "Flush all Tramp cache properties from `buffer-file-name'.
This is suppressed for temporary buffers."
  (save-match-data
    (unless (or (null (buffer-name))
		(string-match "^\\( \\|\\*\\)" (buffer-name)))
      (let ((bfn (if (stringp (buffer-file-name))
		     (buffer-file-name)
		   default-directory))
	    (tramp-verbose 0))
	(when (tramp-tramp-file-p bfn)
	  (with-parsed-tramp-file-name bfn nil
	    (tramp-flush-file-properties v localname)))))))

(add-hook 'before-revert-hook 'tramp-flush-file-function)
(add-hook 'eshell-pre-command-hook 'tramp-flush-file-function)
(add-hook 'kill-buffer-hook 'tramp-flush-file-function)
(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (remove-hook 'before-revert-hook
			 'tramp-flush-file-function)
	    (remove-hook 'eshell-pre-command-hook
			 'tramp-flush-file-function)
	    (remove-hook 'kill-buffer-hook
			 'tramp-flush-file-function)))

;;; -- Properties --

;;;###tramp-autoload
(defun tramp-get-connection-property (key property default)
  "Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.  If the
value is not set for the connection, returns DEFAULT."
  ;; Unify key by removing localname and hop from `tramp-file-name'
  ;; structure.  Work with a copy in order to avoid side effects.
  (when (tramp-file-name-p key)
    (setq key (copy-tramp-file-name key))
    (setf (tramp-file-name-localname key) nil
	  (tramp-file-name-hop key) nil))
  (let* ((hash (tramp-get-hash-table key))
	 (value
	  ;; If the key is an auxiliary process object, check whether
	  ;; the process is still alive.
	  (if (and (processp key) (not (process-live-p key)))
	      default
	    (if (hash-table-p hash)
		(gethash property hash default)
	      default))))
    (tramp-message key 7 "%s %s" property value)
    value))

;;;###tramp-autoload
(defun tramp-set-connection-property (key property value)
  "Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure."
  ;; Unify key by removing localname and hop from `tramp-file-name'
  ;; structure.  Work with a copy in order to avoid side effects.
  (when (tramp-file-name-p key)
    (setq key (copy-tramp-file-name key))
    (setf (tramp-file-name-localname key) nil
	  (tramp-file-name-hop key) nil))
  (let ((hash (tramp-get-hash-table key)))
    (puthash property value hash)
    (setq tramp-cache-data-changed t)
    (tramp-message key 7 "%s %s" property value)
    value))

;;;###tramp-autoload
(defun tramp-connection-property-p (key property)
  "Check whether named PROPERTY of a connection is defined.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine."
  (not (eq (tramp-get-connection-property key property 'undef) 'undef)))

;;;###tramp-autoload
(defun tramp-flush-connection-property (key property)
  "Remove the named PROPERTY of a connection identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure."
  ;; Unify key by removing localname and hop from `tramp-file-name'
  ;; structure.  Work with a copy in order to avoid side effects.
  (when (tramp-file-name-p key)
    (setq key (copy-tramp-file-name key))
    (setf (tramp-file-name-localname key) nil
	  (tramp-file-name-hop key) nil))
  (remhash property (tramp-get-hash-table key))
  (setq tramp-cache-data-changed t)
  (tramp-message key 7 "%s" property))

;;;###tramp-autoload
(defun tramp-flush-connection-properties (key)
  "Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine."
  ;; Unify key by removing localname and hop from `tramp-file-name'
  ;; structure.  Work with a copy in order to avoid side effects.
  (when (tramp-file-name-p key)
    (setq key (copy-tramp-file-name key))
    (setf (tramp-file-name-localname key) nil
	  (tramp-file-name-hop key) nil))
  (tramp-message
   key 7 "%s %s" key
   (let ((hash (gethash key tramp-cache-data))
	 properties)
     (when (hash-table-p hash)
       (maphash (lambda (x _y) (add-to-list 'properties x 'append)) hash))
     properties))
  (setq tramp-cache-data-changed t)
  (remhash key tramp-cache-data))

;;;###tramp-autoload
(defun tramp-cache-print (table)
  "Print hash table TABLE."
  (when (hash-table-p table)
    (let (result)
      (maphash
       (lambda (key value)
	 ;; Remove text properties from KEY and VALUE.
	 ;; `cl-struct-slot-*' functions exist since Emacs 25 only; we
	 ;; ignore errors.
	 (when (tramp-file-name-p key)
	   ;; (dolist
	   ;;     (slot
	   ;; 	(mapcar 'car (cdr (cl-struct-slot-info 'tramp-file-name))))
	   ;;   (when (stringp (cl-struct-slot-value 'tramp-file-name slot key))
	   ;;     (setf (cl-struct-slot-value 'tramp-file-name slot key)
	   ;; 	     (substring-no-properties
	   ;; 	      (cl-struct-slot-value 'tramp-file-name slot key))))))
	   (dotimes (i (length key))
	     (when (stringp (elt key i))
	       (setf (elt key i) (substring-no-properties (elt key i))))))
	 (when (stringp key)
	   (setq key (substring-no-properties key)))
	 (when (stringp value)
	   (setq value (substring-no-properties value)))
	 ;; Dump.
	 (let ((tmp (format
		     "(%s %s)"
		     (if (processp key)
			 (prin1-to-string (prin1-to-string key))
		       (prin1-to-string key))
		     (if (hash-table-p value)
			 (tramp-cache-print value)
		       (if (bufferp value)
			   (prin1-to-string (prin1-to-string value))
			 (prin1-to-string value))))))
	   (setq result (if result (concat result " " tmp) tmp))))
       table)
      result)))

;;;###tramp-autoload
(defun tramp-list-connections ()
  "Return all known `tramp-file-name' structs according to `tramp-cache'."
    (let (result tramp-verbose)
      (maphash
       (lambda (key _value)
	 (when (and (tramp-file-name-p key)
		    (null (tramp-file-name-localname key))
		    (tramp-connection-property-p key "process-buffer"))
	   (add-to-list 'result key)))
       tramp-cache-data)
      result))

(defun tramp-dump-connection-properties ()
  "Write persistent connection properties into file `tramp-persistency-file-name'."
  ;; We shouldn't fail, otherwise Emacs might not be able to be closed.
  (ignore-errors
    (when (and (hash-table-p tramp-cache-data)
	       (not (zerop (hash-table-count tramp-cache-data)))
	       tramp-cache-data-changed
	       (stringp tramp-persistency-file-name))
      (let ((cache (copy-hash-table tramp-cache-data))
	    print-length print-level)
	;; Remove temporary data.  If there is the key "login-as", we
	;; don't save either, because all other properties might
	;; depend on the login name, and we want to give the
	;; possibility to use another login name later on.  Key
	;; "started" exists for the "ftp" method only, which must be
	;; be kept persistent.
	(maphash
	 (lambda (key value)
	   (if (and (tramp-file-name-p key) value
		    (not (string-equal
			  (tramp-file-name-method key) tramp-archive-method))
		    (not (tramp-file-name-localname key))
		    (not (gethash "login-as" value))
		    (not (gethash "started" value)))
	       (progn
		 (remhash "process-name" value)
		 (remhash "process-buffer" value)
		 (remhash "first-password-request" value))
	     (remhash key cache)))
	 cache)
	;; Dump it.
	(with-temp-file tramp-persistency-file-name
	  (insert
	   ";; -*- emacs-lisp -*-"
	   ;; `time-stamp-string' might not exist in all Emacs flavors.
	   (condition-case nil
	       (progn
		 (format
		  " <%s %s>\n"
		  (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
		  tramp-persistency-file-name))
	     (error "\n"))
	   ";; Tramp connection history.  Don't change this file.\n"
	   ";; You can delete it, forcing Tramp to reapply the checks.\n\n"
	   (with-output-to-string
	     (pp (read (format "(%s)" (tramp-cache-print cache)))))))))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'tramp-dump-connection-properties))
(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (remove-hook 'kill-emacs-hook
			 'tramp-dump-connection-properties)))

;;;###tramp-autoload
(defun tramp-parse-connection-properties (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection history."
  (let (res)
    (maphash
     (lambda (key _value)
       (if (and (tramp-file-name-p key)
		(string-equal method (tramp-file-name-method key))
		(not (tramp-file-name-localname key)))
	   (push (list (tramp-file-name-user key)
		       (tramp-file-name-host key))
		 res)))
     tramp-cache-data)
    res))

;; When "emacs -Q" has been called, both variables are nil.  We do not
;; load the persistency file then, in order to have a clean test environment.
;;;###tramp-autoload
(defvar tramp-cache-read-persistent-data (or init-file-user site-run-file)
  "Whether to read persistent data at startup time.")

;; Read persistent connection history.
(when (and (stringp tramp-persistency-file-name)
	   (zerop (hash-table-count tramp-cache-data))
	   tramp-cache-read-persistent-data)
  (condition-case err
      (with-temp-buffer
	(insert-file-contents tramp-persistency-file-name)
	(let ((list (read (current-buffer)))
	      (tramp-verbose 0)
	      element key item)
	  (while (setq element (pop list))
	    (setq key (pop element))
	    (when (tramp-file-name-p key)
	      (while (setq item (pop element))
		;; We set only values which are not contained in
		;; `tramp-connection-properties'.  The cache is
		;; initialized properly by side effect.
		(unless (tramp-connection-property-p key (car item))
		  (tramp-set-connection-property key (pop item) (car item)))))))
	(setq tramp-cache-data-changed nil))
    (file-error
     ;; Most likely because the file doesn't exist yet.  No message.
     (clrhash tramp-cache-data))
    (error
     ;; File is corrupted.
     (message "Tramp persistency file `%s' is corrupted: %s"
	      tramp-persistency-file-name (error-message-string err))
     (clrhash tramp-cache-data))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-cache 'force)))

(provide 'tramp-cache)

;;; tramp-cache.el ends here
