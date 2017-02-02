;;; tramp-compat.el --- Tramp compatibility functions

;; Copyright (C) 2007-2017 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tramp's main Emacs version for development is Emacs 26.  This
;; package provides compatibility functions for Emacs 23, Emacs 24 and
;; Emacs 25.

;;; Code:

;; Pacify byte-compiler.
(eval-when-compile
  (require 'cl))

(require 'auth-source)
(require 'advice)
(require 'custom)
(require 'format-spec)
(require 'parse-time)
(require 'password-cache)
(require 'shell)
(require 'timer)
(require 'ucs-normalize)

(require 'trampver)
(require 'tramp-loaddefs)

;; `remote-file-name-inhibit-cache' has been introduced with Emacs
;; 24.1.  Besides t, nil, and integer, we use also timestamps (as
;; returned by `current-time') internally.
(unless (boundp 'remote-file-name-inhibit-cache)
  (defvar remote-file-name-inhibit-cache nil))

;; For not existing functions, obsolete functions, or functions with a
;; changed argument list, there are compiler warnings.  We want to
;; avoid them in cases we know what we do.
(defmacro tramp-compat-funcall (function &rest arguments)
  "Call FUNCTION if it exists.  Do not raise compiler warnings."
  `(when (functionp ,function)
     (with-no-warnings (funcall ,function ,@arguments))))

;; We currently use "[" and "]" in the filename format for IPv6 hosts
;; of GNU Emacs.  This means that Emacs wants to expand wildcards if
;; `find-file-wildcards' is non-nil, and then barfs because no
;; expansion could be found.  We detect this situation and do
;; something really awful: we have `file-expand-wildcards' return the
;; original filename if it can't expand anything.  Let's just hope
;; that this doesn't break anything else.  It is not needed anymore
;; since GNU Emacs 23.2.
(unless (featurep 'files 'remote-wildcards)
  (defadvice file-expand-wildcards
      (around tramp-advice-file-expand-wildcards activate)
    (let ((name (ad-get-arg 0)))
      ;; If it's a Tramp file, look if wildcards need to be expanded
      ;; at all.
      (if (and
	   (tramp-tramp-file-p name)
	   (not (string-match "[[*?]" (file-remote-p name 'localname))))
	  (setq ad-return-value (list name))
	;; Otherwise, just run the original function.
	ad-do-it)))
  (add-hook
   'tramp-unload-hook
   (lambda ()
     (ad-remove-advice
      'file-expand-wildcards 'around 'tramp-advice-file-expand-wildcards)
     (ad-activate 'file-expand-wildcards))))

;; `condition-case-unless-debug' is introduced with Emacs 24.
(if (fboundp 'condition-case-unless-debug)
    (defalias 'tramp-compat-condition-case-unless-debug
      'condition-case-unless-debug)
  (defmacro tramp-compat-condition-case-unless-debug
    (var bodyform &rest handlers)
  "Like `condition-case' except that it does not catch anything when debugging."
    (declare (debug condition-case) (indent 2))
    (let ((bodysym (make-symbol "body")))
      `(let ((,bodysym (lambda () ,bodyform)))
	 (if debug-on-error
	     (funcall ,bodysym)
	   (condition-case ,var
	       (funcall ,bodysym)
	     ,@handlers))))))

(defsubst tramp-compat-temporary-file-directory ()
  "Return name of directory for temporary files.
It is the default value of `temporary-file-directory'."
  ;; We must return a local directory.  If it is remote, we could run
  ;; into an infloop.
  (eval (car (get 'temporary-file-directory 'standard-value))))

(defsubst tramp-compat-make-temp-file (f &optional dir-flag)
  "Create a local temporary file (compat function).
Add the extension of F, if existing."
  (let* (file-name-handler-alist
	 (prefix (expand-file-name
		  (symbol-value 'tramp-temp-name-prefix)
		  (tramp-compat-temporary-file-directory)))
	 (extension (file-name-extension f t)))
    (make-temp-file prefix dir-flag extension)))

;; `temporary-file-directory' as function is introduced with Emacs 26.1.
(defalias 'tramp-compat-temporary-file-directory-function
  (if (fboundp 'temporary-file-directory)
      'temporary-file-directory
    'tramp-handle-temporary-file-directory))

;; PRESERVE-EXTENDED-ATTRIBUTES has been introduced with Emacs 24.1
;; (as PRESERVE-SELINUX-CONTEXT), and renamed in Emacs 24.3.
(defun tramp-compat-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files (compat function)."
  (cond
   (preserve-extended-attributes
    (condition-case nil
	(tramp-compat-funcall
	 'copy-file filename newname ok-if-already-exists keep-date
	 preserve-uid-gid preserve-extended-attributes)
      (wrong-number-of-arguments
       (copy-file
	filename newname ok-if-already-exists keep-date preserve-uid-gid))))
   (t
    (copy-file
     filename newname ok-if-already-exists keep-date preserve-uid-gid))))

;; COPY-CONTENTS has been introduced with Emacs 24.1.
(defun tramp-compat-copy-directory
  (directory newname &optional keep-time parents copy-contents)
  "Make a copy of DIRECTORY (compat function)."
  (condition-case nil
      (tramp-compat-funcall
       'copy-directory directory newname keep-time parents copy-contents)

    ;; `copy-directory' is either not implemented, or it does not
    ;; support the the COPY-CONTENTS flag.  For the time being, we
    ;; ignore COPY-CONTENTS as well.

    (error
     ;; If `default-directory' is a remote directory, make sure we
     ;; find its `copy-directory' handler.
     (let ((handler (or (find-file-name-handler directory 'copy-directory)
			(find-file-name-handler newname 'copy-directory))))
       (if handler
	   (funcall handler 'copy-directory directory newname keep-time parents)

	 ;; Compute target name.
	 (setq directory (directory-file-name (expand-file-name directory))
	       newname   (directory-file-name (expand-file-name newname)))
	 (if (and (file-directory-p newname)
		  (not (string-equal (file-name-nondirectory directory)
				     (file-name-nondirectory newname))))
	     (setq newname
		   (expand-file-name
		    (file-name-nondirectory directory) newname)))
	 (if (not (file-directory-p newname)) (make-directory newname parents))

	 ;; Copy recursively.
	 (mapc
	  (lambda (file)
	    (if (file-directory-p file)
		(tramp-compat-copy-directory file newname keep-time parents)
	      (copy-file file newname t keep-time)))
	  ;; We do not want to delete "." and "..".
	  (directory-files directory 'full directory-files-no-dot-files-regexp))

	 ;; Set directory attributes.
	 (set-file-modes newname (file-modes directory))
	 (if keep-time
	     (set-file-times newname (nth 5 (file-attributes directory)))))))))

;; TRASH has been introduced with Emacs 24.1.
(defun tramp-compat-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files (compat function)."
  (condition-case nil
      (tramp-compat-funcall 'delete-file filename trash)
    ;; This Emacs version does not support the TRASH flag.
    (wrong-number-of-arguments
     (let ((delete-by-moving-to-trash
	    (and (boundp 'delete-by-moving-to-trash)
		 (symbol-value 'delete-by-moving-to-trash)
		 trash)))
       (delete-file filename)))))

;; RECURSIVE has been introduced with Emacs 23.2.  TRASH has been
;; introduced with Emacs 24.1.
(defun tramp-compat-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files (compat function)."
  (condition-case nil
      (cond
       (trash
	(tramp-compat-funcall 'delete-directory directory recursive trash))
       (t
	(delete-directory directory recursive)))
    ;; This Emacs version does not support the TRASH flag.  We use the
    ;; implementation from Emacs 23.2.
    (wrong-number-of-arguments
     (setq directory (directory-file-name (expand-file-name directory)))
     (when (not (file-symlink-p directory))
       (mapc (lambda (file)
	       (if (eq t (car (file-attributes file)))
		   (tramp-compat-delete-directory file recursive trash)
		 (tramp-compat-delete-file file trash)))
	     (directory-files
	      directory 'full directory-files-no-dot-files-regexp)))
     (delete-directory directory))))

(defun tramp-compat-process-running-p (process-name)
  "Returns t if system process PROCESS-NAME is running for `user-login-name'."
  (when (stringp process-name)
    (cond
     ;; GNU Emacs 22 on w32.
     ((fboundp 'w32-window-exists-p)
      (tramp-compat-funcall 'w32-window-exists-p process-name process-name))

     ;; GNU Emacs 23.
     ((and (fboundp 'list-system-processes) (fboundp 'process-attributes))
      (let (result)
	(dolist (pid (tramp-compat-funcall 'list-system-processes) result)
	  (let ((attributes (process-attributes pid)))
	    (when (and (string-equal
                        (cdr (assoc 'user attributes)) (user-login-name))
                       (let ((comm (cdr (assoc 'comm attributes))))
                         ;; The returned command name could be truncated
                         ;; to 15 characters.  Therefore, we cannot check
                         ;; for `string-equal'.
                         (and comm (string-match
                                    (concat "^" (regexp-quote comm))
                                    process-name))))
	      (setq result t)))))))))

;; `process-running-live-p' is introduced in Emacs 24.
(defalias 'tramp-compat-process-live-p
  (if (fboundp 'process-running-live-p)
      'process-running-live-p
    (lambda (process)
      "Returns non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'.  Value is nil if PROCESS is not a
process."
      (and (processp process)
	   (memq (process-status process)
		 '(run open listen connect stop))))))

;; `user-error' has appeared in Emacs 24.3.
(defsubst tramp-compat-user-error (vec-or-proc format &rest args)
  "Signal a pilot error."
  (apply
   'tramp-error vec-or-proc
   (if (fboundp 'user-error) 'user-error 'error) format args))

;; `file-attribute-*' are introduced in Emacs 25.1.

(if (fboundp 'file-attribute-type)
    (defalias 'tramp-compat-file-attribute-type 'file-attribute-type)
  (defsubst tramp-compat-file-attribute-type (attributes)
    "The type field in ATTRIBUTES returned by `file-attributes'.
The value is either t for directory, string (name linked to) for
symbolic link, or nil."
    (nth 0 attributes)))

(if (fboundp 'file-attribute-link-number)
    (defalias 'tramp-compat-file-attribute-link-number
      'file-attribute-link-number)
  (defsubst tramp-compat-file-attribute-link-number (attributes)
    "Return the number of links in ATTRIBUTES returned by `file-attributes'."
    (nth 1 attributes)))

(if (fboundp 'file-attribute-user-id)
    (defalias 'tramp-compat-file-attribute-user-id 'file-attribute-user-id)
  (defsubst tramp-compat-file-attribute-user-id (attributes)
    "The UID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
    (nth 2 attributes)))

(if (fboundp 'file-attribute-group-id)
    (defalias 'tramp-compat-file-attribute-group-id 'file-attribute-group-id)
  (defsubst tramp-compat-file-attribute-group-id (attributes)
    "The GID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
    (nth 3 attributes)))

(if (fboundp 'file-attribute-modification-time)
    (defalias 'tramp-compat-file-attribute-modification-time
      'file-attribute-modification-time)
  (defsubst tramp-compat-file-attribute-modification-time (attributes)
    "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a list of integers (HIGH LOW USEC PSEC) in the same style
as (current-time)."
    (nth 5 attributes)))

(if (fboundp 'file-attribute-size)
    (defalias 'tramp-compat-file-attribute-size 'file-attribute-size)
  (defsubst tramp-compat-file-attribute-size (attributes)
    "The size (in bytes) in ATTRIBUTES returned by `file-attributes'.
This is a floating point number if the size is too large for an integer."
    (nth 7 attributes)))

(if (fboundp 'file-attribute-modes)
    (defalias 'tramp-compat-file-attribute-modes 'file-attribute-modes)
  (defsubst tramp-compat-file-attribute-modes (attributes)
    "The file modes in ATTRIBUTES returned by `file-attributes'.
This is a string of ten letters or dashes as in ls -l."
    (nth 8 attributes)))

;; `default-toplevel-value' has been declared in Emacs 24.
(unless (fboundp 'default-toplevel-value)
  (defalias 'default-toplevel-value 'symbol-value))

;; `format-message' is new in Emacs 25.
(unless (fboundp 'format-message)
  (defalias 'format-message 'format))

;; `file-missing' is introduced in Emacs 26.
(defconst tramp-file-missing
  (if (get 'file-missing 'error-conditions) 'file-missing 'file-error)
  "The error symbol for the `file-missing' error.")

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-loaddefs 'force)
	    (unload-feature 'tramp-compat 'force)))

;; `file-name-quoted-p', `file-name-quote' and `file-name-unquote' are
;; introduced in Emacs 26.
(eval-and-compile
  (if (fboundp 'file-name-quoted-p)
      (defalias 'tramp-compat-file-name-quoted-p 'file-name-quoted-p)
    (defsubst tramp-compat-file-name-quoted-p (name)
      "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name, check the local part of NAME."
      (string-match "^/:" (or (file-remote-p name 'localname) name))))

  (if (fboundp 'file-name-quote)
      (defalias 'tramp-compat-file-name-quote 'file-name-quote)
    (defsubst tramp-compat-file-name-quote (name)
      "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name, the local part of NAME is quoted."
      (concat
       (file-remote-p name) "/:" (or (file-remote-p name 'localname) name))))

  (if (fboundp 'file-name-unquote)
      (defalias 'tramp-compat-file-name-unquote 'file-name-unquote)
    (defsubst tramp-compat-file-name-unquote (name)
      "Remove quotation prefix \"/:\" from file NAME.
If NAME is a remote file name, the local part of NAME is unquoted."
      (save-match-data
	(let ((localname (or (file-remote-p name 'localname) name)))
	  (when (tramp-compat-file-name-quoted-p localname)
	    (setq
	     localname
	     (replace-match
	      (if (= (length localname) 2) "/" "") nil t localname)))
	  (concat (file-remote-p name) localname))))))

(provide 'tramp-compat)

;;; TODO:

;;; tramp-compat.el ends here
