;;; tramp-compat.el --- Tramp compatibility functions

;; Copyright (C) 2007-2016 Free Software Foundation, Inc.

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

;; Tramp's main Emacs version for development is Emacs 25.  This
;; package provides compatibility functions for Emacs 23 and Emacs 24.

;;; Code:

;; Pacify byte-compiler.
(eval-when-compile
  (require 'cl))

(require 'auth-source)
(require 'advice)
(require 'custom)
(require 'format-spec)
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

;; For not existing functions, or functions with a changed argument
;; list, there are compiler warnings.  We want to avoid them in cases
;; we know what we do.
(defmacro tramp-compat-funcall (function &rest arguments)
  `(when (or (subrp ,function) (functionp ,function))
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
	  (directory-files
	   directory 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))

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
     (if (not (file-symlink-p directory))
	 (mapc (lambda (file)
		 (if (eq t (car (file-attributes file)))
		     (tramp-compat-delete-directory file recursive trash)
		   (tramp-compat-delete-file file trash)))
	       (directory-files
		directory 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))
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

;; `default-toplevel-value' has been declared in Emacs 24.
(unless (fboundp 'default-toplevel-value)
  (defalias 'default-toplevel-value 'symbol-value))

;; `format-message' is new in Emacs 25.
(unless (fboundp 'format-message)
  (defalias 'format-message 'format))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-loaddefs 'force)
	    (unload-feature 'tramp-compat 'force)))

(provide 'tramp-compat)

;;; TODO:

;;; tramp-compat.el ends here
