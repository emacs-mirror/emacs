;;; tramp-cache.el --- file information caching for Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2000, 2005-2024 Free Software Foundation, Inc.

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
;; a process, has a unique cache.  We distinguish several kinds of
;; caches, depending on the key:
;;
;; - localname is nil.  These are reusable properties.  Examples:
;;   "remote-shell" identifies the POSIX shell to be called on the
;;   remote host, or "perl" is the command to be called on the remote
;;   host when starting a Perl script.  These properties are saved in
;;   the file `tramp-persistency-file-name'.
;;
;; - localname is an absolute file name.  These are temporary
;;   properties, which are related to the file localname is referring
;;   to.  Examples: "file-exists-p" is t or nil, depending on the file
;;   existence, or "file-attributes" caches the result of the function
;;   `file-attributes'.  These entries have a timestamp, and they
;;   expire after `remote-file-name-inhibit-cache' seconds if this
;;   variable is set.
;;
;; - The key is a process.  These are temporary properties related to
;;   an open connection.  Examples: "scripts" keeps shell script
;;   definitions already sent to the remote shell, "last-cmd-time" is
;;   the timestamp a command has been sent to the remote process.
;;
;; - The key is `tramp-null-hop' or nil.  These are temporary
;;   properties related to the local machine.  If the key is nil, it
;;   is silently converted into `tramp-null-hop'.
;;   Examples: "parse-passwd" and "parse-group" keep the results of
;;   parsing "/etc/passwd" and "/etc/group",
;;   "{uid,gid}-{integer,string}" are the local uid and gid, and
;;   "locale" is the used shell locale.  "user-host-completions" keeps
;;   the reachable hosts for the commands in tramp-container.el.
;;
;; - The key is `tramp-cache-version'.  It keeps the Tramp version the
;;   cache data was produced with.  If the cache is read by another
;;   Tramp version, it is flushed.
;;
;; - The key is `tramp-cache-undefined'.  All functions return the
;;   expected values, but nothing is cached.

;; Some properties are handled special:
;;
;; - Properties which start with a space, like " process-name", are
;;   not saved in the file `tramp-persistency-file-name', although
;;   being connection properties related to a `tramp-file-name'
;;   structure.
;;
;; - Reusable properties, which should not be saved, are kept in the
;;   process key retrieved by `tramp-get-process' (the main connection
;;   process).  Other processes could reuse these properties, avoiding
;;   recomputation when a new asynchronous process is created by
;;   `make-process'.  Examples are "unsafe-temporary-file",
;;   "remote-path", "device" (tramp-adb.el) or "share" (tramp-gvfs.el).

;;; Code:

(require 'tramp-compat)
(require 'time-stamp)

;;; -- Cache --

;;;###tramp-autoload
(defvar tramp-cache-data (make-hash-table :test #'equal)
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
  :link '(info-link :tag "Tramp manual"
		    "(tramp) Predefined connection information"))

;;;###tramp-autoload
(defcustom tramp-persistency-file-name (locate-user-emacs-file "tramp")
  "File which keeps connection history for Tramp connections."
  :group 'tramp
  :type 'file
  :link '(info-link :tag "Tramp manual" "(tramp) Connection caching"))

;;;###tramp-autoload
(defconst tramp-cache-version (make-tramp-file-name :method "cache")
"Virtual connection vector for Tramp version.")

(defvar tramp-cache-data-changed nil
  "Whether persistent cache data have been changed.")

;;;###tramp-autoload
(defconst tramp-cache-undefined 'undef
  "The symbol marking undefined hash keys and values.")

;;;###tramp-autoload
(defun tramp-get-hash-table (key)
  "Return the hash table for KEY.
If it doesn't exist yet, it is created and initialized with
matching entries of `tramp-connection-properties'.
If KEY is `tramp-cache-undefined', don't create anything, and return nil."
  ;; (declare (tramp-suppress-trace t))
  (unless (eq key tramp-cache-undefined)
    (or (gethash key tramp-cache-data)
	(let ((hash
	       (puthash key (make-hash-table :test #'equal) tramp-cache-data)))
	  (when (tramp-file-name-p key)
	    (dolist (elt tramp-connection-properties)
	      (when (string-match-p
		     (or (nth 0 elt) "")
		     (tramp-make-tramp-file-name key 'noloc))
		(tramp-set-connection-property key (nth 1 elt) (nth 2 elt)))))
	  hash))))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-get-hash-table 'tramp-suppress-trace t)

;;;###tramp-autoload
(defun tramp-get-file-property (key file property &optional default)
  "Get the PROPERTY of FILE from the cache context of KEY.
Return DEFAULT if not set."
  (setq key (tramp-file-name-unify key file))
  (if (eq key tramp-cache-undefined) default
    (let* ((hash (tramp-get-hash-table key))
	   (cached (and (hash-table-p hash) (gethash property hash)))
	   (cached-at
	    (and (consp cached) (format-time-string "%T" (car cached))))
	   (value default)
	   cache-used)

      (when ;; We take the value only if there is any, and
	    ;; `remote-file-name-inhibit-cache' indicates that it is
	    ;; still valid.  Otherwise, DEFAULT is set.
	  (and (consp cached)
	       (or (null remote-file-name-inhibit-cache)
		   (and (integerp remote-file-name-inhibit-cache)
			(time-less-p
			 nil
			 (time-add (car cached) remote-file-name-inhibit-cache)))
		   (and (consp remote-file-name-inhibit-cache)
			(time-less-p
			 remote-file-name-inhibit-cache (car cached)))))
	(setq value (cdr cached)
	      cache-used t))

      (tramp-message
       key 8 "%s %s %s; inhibit: %s; cache used: %s; cached at: %s"
       (tramp-file-name-localname key)
       property value remote-file-name-inhibit-cache cache-used cached-at)
      ;; For analysis purposes, count the number of getting this file attribute.
      (when (>= tramp-verbose 10)
	(let* ((var (intern (concat "tramp-cache-get-count-" property)))
	       (val (or (and (boundp var) (numberp (symbol-value var))
			     (symbol-value var))
			0)))
	  (set var (1+ val))))
      value)))

(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (dolist (var (all-completions "tramp-cache-get-count-" obarray))
	      (unintern var obarray))))

;;;###tramp-autoload
(defun tramp-set-file-property (key file property value)
  "Set the PROPERTY of FILE to VALUE, in the cache context of KEY.
Return VALUE."
  (setq key (tramp-file-name-unify key file))
  (if (eq key tramp-cache-undefined) value
    (let ((hash (tramp-get-hash-table key)))
      ;; We put the timestamp there.
      (puthash property (cons (current-time) value) hash)
      (tramp-message
       key 8 "%s %s %s" (tramp-file-name-localname key) property value)
      ;; For analysis purposes, count the number of setting this file attribute.
      (when (>= tramp-verbose 10)
	(let* ((var (intern (concat "tramp-cache-set-count-" property)))
	       (val (or (and (boundp var) (numberp (symbol-value var))
			     (symbol-value var))
			0)))
	  (set var (1+ val))))
      value)))

(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (dolist (var (all-completions "tramp-cache-set-count-" obarray))
	      (unintern var obarray))))

;;;###tramp-autoload
(defun tramp-file-property-p (key file property)
  "Check whether PROPERTY of FILE is defined in the cache context of KEY."
  (and
   (not (eq key tramp-cache-undefined))
   (not (eq (tramp-get-file-property key file property tramp-cache-undefined)
	    tramp-cache-undefined))))

;;;###tramp-autoload
(defun tramp-flush-file-property (key file property)
  "Remove PROPERTY of FILE in the cache context of KEY."
  (setq key (tramp-file-name-unify key file))
  (unless (eq key tramp-cache-undefined)
    (remhash property (tramp-get-hash-table key))
    (tramp-message key 8 "%s %s" (tramp-file-name-localname key) property)
    (when (>= tramp-verbose 10)
      (let ((var (intern (concat "tramp-cache-set-count-" property))))
	(makunbound var)))))

(defun tramp-flush-file-upper-properties (key file)
  "Remove some properties of FILE's upper directory."
  (when (file-name-absolute-p file)
    ;; `file-name-directory' can return nil, for example for "~".
    (when-let* ((file (file-name-directory file))
		(file (directory-file-name file)))
      (setq key (tramp-file-name-unify key file))
      (unless (eq key tramp-cache-undefined)
	(dolist (property (hash-table-keys (tramp-get-hash-table key)))
	  (when (string-match-p
		 (rx
		  bos (| "directory-" "file-name-all-completions"
			 "file-entries"))
		 property)
	    (tramp-flush-file-property key file property)))))))

;;;###tramp-autoload
(defun tramp-flush-file-properties (key file)
  "Remove all properties of FILE in the cache context of KEY."
  (let ((truename (tramp-get-file-property key file "file-truename")))
    (setq key (tramp-file-name-unify key file))
    (unless (eq key tramp-cache-undefined)
      (tramp-message key 8 "%s" (tramp-file-name-localname key))
      (remhash key tramp-cache-data)
      ;; Remove file properties of symlinks.
      (when (and (stringp truename)
		 (not (string-equal file (directory-file-name truename))))
	(tramp-flush-file-properties key truename))
      ;; Remove selected properties of upper directory.
      (tramp-flush-file-upper-properties key file))))

;;;###tramp-autoload
(defun tramp-flush-directory-properties (key directory)
  "Remove all properties of DIRECTORY in the cache context of KEY.
Remove also properties of all files in subdirectories."
  (let* ((directory (directory-file-name (file-name-unquote directory)))
	 (truename (tramp-get-file-property key directory "file-truename")))
    (tramp-message key 8 "%s" directory)
    (dolist (key (hash-table-keys tramp-cache-data))
      (when (and (tramp-file-name-p key)
		 (stringp (tramp-file-name-localname key))
		 (string-search directory (tramp-file-name-localname key)))
	(remhash key tramp-cache-data)))
    ;; Remove file properties of symlinks.
    (when (and (stringp truename)
	       (not (string-equal directory (directory-file-name truename))))
      (tramp-flush-directory-properties key truename))
    ;; Remove selected properties of upper directory.
    (tramp-flush-file-upper-properties key directory)))

;; Reverting or killing a buffer should also flush file properties.
;; They could have been changed outside Tramp.  In eshell, "ls" would
;; not show proper directory contents when a file has been copied or
;; deleted before.  We must apply `save-match-data', because it would
;; corrupt other packages otherwise (reported from org).
;;;###tramp-autoload
(defun tramp-flush-file-function ()
  "Flush all Tramp cache properties from `buffer-file-name'.
This is suppressed for temporary buffers."
  (save-match-data
    (unless (or (null (buffer-name))
		(string-match-p (rx bos (| blank "*")) (buffer-name)))
      (let ((bfn (if (stringp (buffer-file-name))
		     (buffer-file-name)
		   default-directory))
	    (tramp-verbose 0))
	(when (tramp-tramp-file-p bfn)
	  (tramp-flush-file-properties
	   (tramp-dissect-file-name bfn) (tramp-file-local-name bfn)))))))

(add-hook 'before-revert-hook #'tramp-flush-file-function)
(add-hook 'eshell-pre-command-hook #'tramp-flush-file-function)
(add-hook 'kill-buffer-hook #'tramp-flush-file-function)
(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (remove-hook 'before-revert-hook
			 #'tramp-flush-file-function)
	    (remove-hook 'eshell-pre-command-hook
			 #'tramp-flush-file-function)
	    (remove-hook 'kill-buffer-hook
			 #'tramp-flush-file-function)))

;;;###tramp-autoload
(defmacro with-tramp-file-property (key file property &rest body)
  "Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via KEY."
  (declare (indent 3) (debug t))
  `(let ((value (tramp-get-file-property
		 ,key ,file ,property tramp-cache-undefined)))
     (when (eq value tramp-cache-undefined)
       ;; We cannot pass @body as parameter to
       ;; `tramp-set-file-property' because it mangles our debug
       ;; messages.
       (setq value (progn ,@body))
       (tramp-set-file-property ,key ,file ,property value))
     value))

;;;###tramp-autoload
(defmacro with-tramp-saved-file-property (key file property &rest body)
  "Save PROPERTY, run BODY, reset PROPERTY.
Preserve timestamps."
  (declare (indent 3) (debug t))
  `(let* ((key (tramp-file-name-unify ,key ,file))
	  (hash (tramp-get-hash-table key))
	  (cached (and (hash-table-p hash) (gethash ,property hash))))
     (unwind-protect (progn ,@body)
       ;; Reset PROPERTY.  Recompute hash, it could have been flushed.
       (setq hash (tramp-get-hash-table key))
       (if (consp cached)
	   (puthash ,property cached hash)
	 (remhash ,property hash)))))

;;;###tramp-autoload
(defmacro with-tramp-saved-file-properties (key file properties &rest body)
  "Save PROPERTIES, run BODY, reset PROPERTIES.
PROPERTIES is a list of file properties (strings).
Preserve timestamps."
  (declare (indent 3) (debug t))
  `(let* ((key (tramp-file-name-unify ,key ,file))
	  (hash (tramp-get-hash-table key))
	  (values
	   (and (hash-table-p hash)
		(mapcar
		 (lambda (property) (cons property (gethash property hash)))
		 ,properties))))
     (unwind-protect (progn ,@body)
       ;; Reset PROPERTIES.  Recompute hash, it could have been flushed.
       (setq hash (tramp-get-hash-table key))
       (dolist (value values)
	 (if (consp (cdr value))
	     (puthash (car value) (cdr value) hash)
	   (remhash (car value) hash))))))

;;; -- Properties --

;;;###tramp-autoload
(defun tramp-get-connection-property (key property &optional default)
  "Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
If KEY is `tramp-cache-undefined', or if the value is not set for
the connection, return DEFAULT."
  (setq key (tramp-file-name-unify key))
  (let* ((hash (tramp-get-hash-table key))
	 (cached (if (hash-table-p hash)
		     (gethash property hash tramp-cache-undefined)
		   tramp-cache-undefined))
	 (value default)
	 cache-used)

    (when (and (not (eq cached tramp-cache-undefined))
	       ;; If the key is an auxiliary process object, check
	       ;; whether the process is still alive.
	       (not (and (processp key) (not (process-live-p key)))))
      (setq value cached
	    cache-used t))
    (unless (eq key tramp-cache-version)
      (tramp-message key 7 "%s %s; cache used: %s" property value cache-used))
    value))

;;;###tramp-autoload
(defun tramp-set-connection-property (key property value)
  "Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.  If KEY
is `tramp-cache-undefined', nothing is set.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure.
Return VALUE."
  (setq key (tramp-file-name-unify key))
  (when-let* ((hash (tramp-get-hash-table key)))
    (puthash property value hash))
  (setq tramp-cache-data-changed
	(or tramp-cache-data-changed (tramp-file-name-p key)))
  (unless (eq key tramp-cache-version)
    (tramp-message key 7 "%s %s" property value))
  value)

;;;###tramp-autoload
(defun tramp-connection-property-p (key property)
  "Check whether named PROPERTY of a connection is defined.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine."
  (not (eq (tramp-get-connection-property key property tramp-cache-undefined)
	   tramp-cache-undefined)))

;;;###tramp-autoload
(defun tramp-flush-connection-property (key property)
  "Remove the named PROPERTY of a connection identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure."
  (setq key (tramp-file-name-unify key))
  (when-let* ((hash (tramp-get-hash-table key)))
    (remhash property hash))
  (setq tramp-cache-data-changed
	(or tramp-cache-data-changed (tramp-file-name-p key)))
  (tramp-message key 7 "%s" property))

;;;###tramp-autoload
(defun tramp-flush-connection-properties (key)
  "Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine."
  (setq key (tramp-file-name-unify key))
  (tramp-message
   key 7 "%s %s" key
   (when-let* ((hash (gethash key tramp-cache-data)))
     (hash-table-keys hash)))
  (setq tramp-cache-data-changed
	(or tramp-cache-data-changed (tramp-file-name-p key)))
  (remhash key tramp-cache-data))

;;;###tramp-autoload
(defmacro with-tramp-connection-property (key property &rest body)
  "Check in Tramp for property PROPERTY, otherwise execute BODY and set."
  (declare (indent 2) (debug t))
  `(let ((value (tramp-get-connection-property
		 ,key ,property tramp-cache-undefined)))
     (when (eq value tramp-cache-undefined)
       ;; We cannot pass ,@body as parameter to
       ;; `tramp-set-connection-property' because it mangles our debug
       ;; messages.
       (setq value (progn ,@body))
       (tramp-set-connection-property ,key ,property value))
     value))

;;;###tramp-autoload
(defmacro with-tramp-saved-connection-property (key property &rest body)
  "Save PROPERTY, run BODY, reset PROPERTY."
  (declare (indent 2) (debug t))
  `(let* ((key (tramp-file-name-unify ,key))
	  (hash (tramp-get-hash-table key))
	  (cached (and (hash-table-p hash)
		       (gethash ,property hash tramp-cache-undefined))))
     (unwind-protect (progn ,@body)
       ;; Reset PROPERTY.  Recompute hash, it could have been flushed.
       (setq hash (tramp-get-hash-table key))
       (if (not (eq cached tramp-cache-undefined))
	   (puthash ,property cached hash)
	 (remhash ,property hash)))))

;;;###tramp-autoload
(defmacro with-tramp-saved-connection-properties (key properties &rest body)
  "Save PROPERTIES, run BODY, reset PROPERTIES.
PROPERTIES is a list of file properties (strings)."
  (declare (indent 2) (debug t))
  `(let* ((key (tramp-file-name-unify ,key))
	  (hash (tramp-get-hash-table key))
	  (values
	   (mapcar
	    (lambda (property)
	      (cons property (gethash property hash tramp-cache-undefined)))
	    ,properties)))
     (unwind-protect (progn ,@body)
       ;; Reset PROPERTIES.  Recompute hash, it could have been flushed.
       (setq hash (tramp-get-hash-table key))
       (dolist (value values)
	 (if (not (eq (cdr value) tramp-cache-undefined))
	     (puthash (car value) (cdr value) hash)
	   (remhash (car value) hash))))))

;;;###tramp-autoload
(defun tramp-cache-print (table)
  "Print hash table TABLE."
  ;; (declare (tramp-suppress-trace t))
  (when (hash-table-p table)
    (let (result)
      (maphash
       (lambda (key value)
	 ;; Remove text properties from KEY and VALUE.
	 (when (tramp-file-name-p key)
           (dolist
               (slot
                (mapcar #'car (cdr (cl-struct-slot-info 'tramp-file-name))))
             (when (stringp (cl-struct-slot-value 'tramp-file-name slot key))
               (setf (cl-struct-slot-value 'tramp-file-name slot key)
                     (substring-no-properties
                      (cl-struct-slot-value 'tramp-file-name slot key))))))
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

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-cache-print 'tramp-suppress-trace t)

;;;###tramp-autoload
(defun tramp-list-connections ()
  "Return all active `tramp-file-name' structs according to `tramp-cache-data'."
  (let ((tramp-verbose 0))
    (tramp-compat-seq-keep
     (lambda (key)
       (and (tramp-file-name-p key)
	    (null (tramp-file-name-localname key))
	    (tramp-connection-property-p key " process-buffer")
	    key))
     (hash-table-keys tramp-cache-data))))

(defun tramp-dump-connection-properties ()
  "Write persistent connection properties into file \
`tramp-persistency-file-name'."
  (declare (tramp-suppress-trace t))
  ;; We shouldn't fail, otherwise Emacs might not be able to be closed.
  (ignore-errors
    (when (and (hash-table-p tramp-cache-data)
	       (not (zerop (hash-table-count tramp-cache-data)))
	       tramp-cache-data-changed
	       (stringp tramp-persistency-file-name))
      (let ((cache (copy-hash-table tramp-cache-data))
	    print-length print-level)
	;; Remove `tramp-null-hop'.
	(remhash tramp-null-hop cache)
	;; Remove temporary data.  If there is the key "login-as", we
	;; don't save either, because all other properties might
	;; depend on the login name, and we want to give the
	;; possibility to use another login name later on.  Key
	;; "started" exists for the "ftp" method only, which must not
	;; be kept persistent.
	(maphash
	 (lambda (key value)
	   (if (and (tramp-file-name-p key) (hash-table-p value)
		    (not (string-equal
			  (tramp-file-name-method key) tramp-archive-method))
		    (not (tramp-file-name-localname key))
		    (not (gethash "login-as" value))
		    (not (gethash "started" value)))
	       (dolist (k (hash-table-keys value))
		 (when (string-prefix-p " " k)
		   (remhash k value)))
	     (remhash key cache)))
	 cache)
	;; Dump it.
	(with-temp-file tramp-persistency-file-name
	  (insert
	   (format ";; -*- lisp-data -*- <%s %s>\n"
		   (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
		   tramp-persistency-file-name)
	   ";; Tramp connection history.  Don't change this file.\n"
	   ";; Run `M-x tramp-cleanup-all-connections' instead.\n\n"
	   (with-output-to-string
	     (pp (read (format "(%s)" (tramp-cache-print cache)))))))))))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'tramp-dump-connection-properties))
(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (remove-hook 'kill-emacs-hook
			 #'tramp-dump-connection-properties)))

;;;###tramp-autoload
(defcustom tramp-completion-use-cache t
  "Whether to use the Tramp cache for completion of user and host names.
Set it to nil if there are invalid entries in the cache, for
example if the host configuration changes often, or if you plug
your laptop to different networks frequently."
  :group 'tramp
  :version "29.1"
  :type 'boolean
  :link '(tramp-info-link :tag "Tramp manual" tramp-completion-use-cache))

;;;###tramp-autoload
(defun tramp-parse-connection-properties (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection history."
  (and tramp-completion-use-cache
       (mapcar
	(lambda (key)
	  (and (tramp-file-name-p key)
	       (string-equal method (tramp-file-name-method key))
	       (not (tramp-file-name-localname key))
	       (list (tramp-file-name-user key)
		     (tramp-file-name-host key))))
	(hash-table-keys tramp-cache-data))))

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
	(insert-file-contents-literally tramp-persistency-file-name)
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
	;; Check Tramp version.  Clear cache in case of mismatch.
	(unless (string-equal
		 (tramp-get-connection-property
		  tramp-cache-version "tramp-version" "")
		 tramp-version)
	  (signal 'file-error nil))
	(setq tramp-cache-data-changed nil))
    (file-error
     ;; Most likely because the file doesn't exist yet, or the Tramp
     ;; version doesn't match.  No message.
     (clrhash tramp-cache-data))
    (error
     ;; File is corrupted.
     (message "Tramp persistency file `%s' is corrupted: %s"
	      tramp-persistency-file-name (error-message-string err))
     (clrhash tramp-cache-data))))

;; Initialize the cache version.
(tramp-set-connection-property tramp-cache-version "tramp-version" tramp-version)

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-cache 'force)))

(provide 'tramp-cache)

;;; TODO:
;;
;; * Use multisession.el, starting with Emacs 29.1.

;;; tramp-cache.el ends here
