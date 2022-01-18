;;; tramp-sudoedit.el --- Functions for accessing under root permissions  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The "sudoedit" Tramp method allows to edit a file as a different
;; user on the local host.  Contrary to the "sudo" method, all magic
;; file name functions are implemented by single "sudo ..."  commands.
;; The purpose is to make editing such a file as secure as possible;
;; there must be no session running in the Emacs background which
;; could be attacked from inside Emacs.

;; Consequently, external processes are not implemented.

;;; Code:

(require 'tramp)

;;;###tramp-autoload
(defconst tramp-sudoedit-method "sudoedit"
  "When this method name is used, call sudoedit for editing a file.")

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
              `(,tramp-sudoedit-method
                (tramp-sudo-login (("sudo") ("-u" "%u") ("-S") ("-H")
			           ("-p" "Password:") ("--")))))

 (add-to-list 'tramp-default-user-alist '("\\`sudoedit\\'" nil "root"))

 (tramp-set-completion-function
  tramp-sudoedit-method tramp-completion-function-alist-su))

(defconst tramp-sudoedit-sudo-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-sudoedit-action-sudo))
  "List of pattern/action pairs.
This list is used for sudo calls.

See `tramp-actions-before-shell' for more info.")

;;;###tramp-autoload
(defconst tramp-sudoedit-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-sudoedit-handle-add-name-to-file)
    (byte-compiler-base-file-name . ignore)
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-sudoedit-handle-copy-file)
    (delete-directory . tramp-sudoedit-handle-delete-directory)
    (delete-file . tramp-sudoedit-handle-delete-file)
    (diff-latest-backup-file . ignore)
    ;; `directory-file-name' performed by default handler.
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . ignore)
    (expand-file-name . tramp-sudoedit-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . tramp-sudoedit-handle-file-acl)
    (file-attributes . tramp-sudoedit-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-sudoedit-handle-file-executable-p)
    (file-exists-p . tramp-sudoedit-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions
     . tramp-sudoedit-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-notify-valid-p . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-sudoedit-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-sudoedit-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-sudoedit-handle-file-system-info)
    (file-truename . tramp-sudoedit-handle-file-truename)
    (file-writable-p . tramp-sudoedit-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-sudoedit-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-sudoedit-handle-make-symbolic-link)
    (process-file . ignore)
    (rename-file . tramp-sudoedit-handle-rename-file)
    (set-file-acl . tramp-sudoedit-handle-set-file-acl)
    (set-file-modes . tramp-sudoedit-handle-set-file-modes)
    (set-file-selinux-context . tramp-sudoedit-handle-set-file-selinux-context)
    (set-file-times . tramp-sudoedit-handle-set-file-times)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . ignore)
    (start-file-process . ignore)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-remote-gid . tramp-sudoedit-handle-get-remote-gid)
    (tramp-get-remote-uid . tramp-sudoedit-handle-get-remote-uid)
    (tramp-set-file-uid-gid . tramp-sudoedit-handle-set-file-uid-gid)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-sudoedit-handle-write-region))
  "Alist of handler functions for Tramp SUDOEDIT method.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-sudoedit-file-name-p (filename)
  "Check if it's a FILENAME for SUDOEDIT."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
		tramp-sudoedit-method)))

;;;###tramp-autoload
(defun tramp-sudoedit-file-name-handler (operation &rest args)
  "Invoke the SUDOEDIT handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let ((fn (assoc operation tramp-sudoedit-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-sudoedit-file-name-p #'tramp-sudoedit-file-name-handler))


;; File name primitives.

(defun tramp-sudoedit-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for Tramp files."
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
	(if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       v 'file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
	;; Do the 'confirm if exists' thing.
	(when (file-exists-p newname)
	  ;; What to do?
	  (if (or (null ok-if-already-exists) ; not allowed to exist
		  (and (numberp ok-if-already-exists)
		       (not (yes-or-no-p
			     (format
			      "File %s already exists; make it a link anyway?"
			      v2-localname)))))
	      (tramp-error v2 'file-already-exists newname)
	    (delete-file newname)))
	(tramp-flush-file-properties v2 v2-localname)
	(unless
	    (tramp-sudoedit-send-command
	     v1 "ln"
	     (tramp-compat-file-name-unquote v1-localname)
	     (tramp-compat-file-name-unquote v2-localname))
	  (tramp-error
	   v1 'file-error
	   "error with add-name-to-file, see buffer `%s' for details"
	   (buffer-name))))))

(defun tramp-sudoedit-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid if both files are on the same host.
PRESERVE-EXTENDED-ATTRIBUTES activates selinux and acl commands.

This function is invoked by `tramp-sudoedit-handle-copy-file' and
`tramp-sudoedit-handle-rename-file'.  It is an error if OP is
neither of `copy' and `rename'.  FILENAME and NEWNAME must be
absolute file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  (setq filename (file-truename filename))
  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))

    (let ((t1 (tramp-sudoedit-file-name-p filename))
	  (t2 (tramp-sudoedit-file-name-p newname))
	  (file-times (tramp-compat-file-attribute-modification-time
		       (file-attributes filename)))
	  (file-modes (tramp-default-file-modes filename))
	  (attributes (and preserve-extended-attributes
			   (file-extended-attributes filename)))
	  (sudoedit-operation
	   (cond
	    ((and (eq op 'copy) preserve-uid-gid) '("cp" "-f" "-p"))
	    ((eq op 'copy) '("cp" "-f"))
	    ((eq op 'rename) '("mv" "-f"))))
	  (msg-operation (if (eq op 'copy) "Copying" "Renaming")))

      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(unless (file-exists-p filename)
	  (tramp-compat-file-missing v filename))
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))
	(when (and (file-directory-p newname)
		   (not (directory-name-p newname)))
	  (tramp-error v 'file-error "File is a directory %s" newname))

	(if (or (and (file-remote-p filename) (not t1))
		(and (file-remote-p newname)  (not t2)))
	    ;; We cannot copy or rename directly.
	    (let ((tmpfile (tramp-compat-make-temp-file filename)))
	      (if (eq op 'copy)
		  (copy-file filename tmpfile t)
		(rename-file filename tmpfile t))
	      (rename-file tmpfile newname ok-if-already-exists))

	  ;; Direct action.
	  (with-tramp-progress-reporter
	      v 0 (format "%s %s to %s" msg-operation filename newname)
	    (unless (tramp-sudoedit-send-command
		     v sudoedit-operation
		     (tramp-unquote-file-local-name filename)
		     (tramp-unquote-file-local-name newname))
	      (tramp-error
	       v 'file-error
	       "Error %s `%s' `%s'" msg-operation filename newname))))

	;; When `newname' is local, we must change the ownership to
	;; the local user.
	(unless (file-remote-p newname)
	  (tramp-set-file-uid-gid
	   (concat (file-remote-p filename) newname)
	   (tramp-get-local-uid 'integer)
	   (tramp-get-local-gid 'integer)))

	;; Set the time and mode. Mask possible errors.
	(when keep-date
	  (ignore-errors
	    (tramp-compat-set-file-times
	     newname file-times (unless ok-if-already-exists 'nofollow))
	    (set-file-modes newname file-modes)))

	;; Handle `preserve-extended-attributes'.  We ignore possible
	;; errors, because ACL strings could be incompatible.
	(when attributes
	  (ignore-errors
	    (set-file-extended-attributes newname attributes)))

	(when (and t1 (eq op 'rename))
	  (with-parsed-tramp-file-name filename v1
	    (tramp-flush-file-properties v1 v1-localname)))

	(when t2
	  (with-parsed-tramp-file-name newname v2
	    (tramp-flush-file-properties v2 v2-localname)))))))

(defun tramp-sudoedit-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
	  (tramp-tramp-file-p newname))
      (tramp-sudoedit-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date
       preserve-uid-gid preserve-extended-attributes)
    (tramp-run-real-handler
     #'copy-file
     (list filename newname ok-if-already-exists keep-date
	   preserve-uid-gid preserve-extended-attributes))))

(defun tramp-sudoedit-handle-delete-directory
    (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (tramp-skeleton-delete-directory directory recursive trash
    (unless (tramp-sudoedit-send-command
	     v (if recursive '("rm" "-rf") "rmdir")
	     (tramp-compat-file-name-unquote localname))
      (tramp-error v 'file-error "Couldn't delete %s" directory))))

(defun tramp-sudoedit-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v localname)
    (if (and delete-by-moving-to-trash trash)
	(move-file-to-trash filename)
      (unless (tramp-sudoedit-send-command
	       v "rm" (tramp-compat-file-name-unquote localname))
	;; Propagate the error.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (tramp-error-with-buffer
	   nil v 'file-error "Couldn't delete %s" filename))))))

(defun tramp-sudoedit-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files.
If the localname part of the given file name starts with \"/../\" then
the result will be a local, non-Tramp, file name."
  ;; If DIR is not given, use `default-directory' or "/".
  (setq dir (or dir default-directory "/"))
  ;; Handle empty NAME.
  (when (zerop (length name)) (setq name "."))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (tramp-compat-file-name-concat dir name)))
  (with-parsed-tramp-file-name name nil
    ;; Tilde expansion if necessary.  We cannot accept "~/", because
    ;; under sudo "~/" is expanded to the local user home directory
    ;; but to the root home directory.
    (when (zerop (length localname))
      (setq localname "~"))
    (unless (file-name-absolute-p localname)
      (setq localname (format "~%s/%s" user localname)))
    (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" localname)
      (let ((uname (match-string 1 localname))
	    (fname (match-string 2 localname)))
	(when (string-equal uname "~")
	  (setq uname (concat uname user)))
	(setq localname (concat uname fname))))
     ;; Do not keep "/..".
      (when (string-match-p "^/\\.\\.?$" localname)
	(setq localname "/"))
    ;; Do normal `expand-file-name' (this does "~user/", "/./" and "/../").
    (tramp-make-tramp-file-name v (expand-file-name localname))))

(defun tramp-sudoedit-remote-acl-p (vec)
  "Check, whether ACL is enabled on the remote host."
  (with-tramp-connection-property (tramp-get-process vec) "acl-p"
    (zerop (tramp-call-process vec "getfacl" nil nil nil "/"))))

(defun tramp-sudoedit-handle-file-acl (filename)
  "Like `file-acl' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-acl"
      (let ((result (and (tramp-sudoedit-remote-acl-p v)
			 (tramp-sudoedit-send-command-string
			  v "getfacl" "-acp"
			  (tramp-compat-file-name-unquote localname)))))
	;; The acl string must have a trailing \n, which is not
	;; provided by `tramp-sudoedit-send-command-string'.  Add it.
	(and (stringp result) (concat result "\n"))))))

(defun tramp-sudoedit-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
	v localname (format "file-attributes-%s" id-format)
      (tramp-message v 5 "file attributes: %s" localname)
      (ignore-errors
	(tramp-convert-file-attributes
	 v
	 (tramp-sudoedit-send-command-and-read
	  v "env" "QUOTING_STYLE=locale" "stat" "-c"
	  (format
	   ;; Apostrophes in the stat output are masked as
	   ;; `tramp-stat-marker', in order to make a proper shell
	   ;; escape of them in file names.
	   "((%s%%N%s) %%h %s %s %%X %%Y %%Z %%s %s%%A%s t %%i -1)"
	   tramp-stat-marker tramp-stat-marker
	   (if (eq id-format 'integer)
	       "%u"
	     (eval-when-compile
	       (concat tramp-stat-marker "%U" tramp-stat-marker)))
	   (if (eq id-format 'integer)
	       "%g"
	     (eval-when-compile
	       (concat tramp-stat-marker "%G" tramp-stat-marker)))
	   tramp-stat-marker tramp-stat-marker)
	  (tramp-compat-file-name-unquote localname)))))))

(defun tramp-sudoedit-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-executable-p"
      (tramp-sudoedit-send-command
       v "test" "-x" (tramp-compat-file-name-unquote localname)))))

(defun tramp-sudoedit-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  ;; `file-exists-p' is used as predicate in file name completion.
  ;; We don't want to run it when `non-essential' is t, or there is
  ;; no connection process yet.
  (when (tramp-connectable-p filename)
    (with-parsed-tramp-file-name filename nil
      (with-tramp-file-property v localname "file-exists-p"
	(tramp-sudoedit-send-command
	 v "test" "-e" (tramp-compat-file-name-unquote localname))))))

(defun tramp-sudoedit-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name (expand-file-name directory) nil
     (with-tramp-file-property v localname "file-name-all-completions"
       (tramp-sudoedit-send-command
	v "ls" "-a1" "--quoting-style=literal" "--show-control-chars"
	(if (zerop (length localname))
	    "" (tramp-compat-file-name-unquote localname)))
       (mapcar
	(lambda (f)
	  (if (file-directory-p (expand-file-name f directory))
	      (file-name-as-directory f)
	    f))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (delq
	   nil
	   (mapcar
	    (lambda (l) (and (not (string-match-p "^[[:space:]]*$" l)) l))
	    (split-string (buffer-string) "\n" 'omit)))))))))

(defun tramp-sudoedit-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-readable-p"
      (or (tramp-handle-file-readable-p filename)
	  (tramp-sudoedit-send-command
	   v "test" "-r" (tramp-compat-file-name-unquote localname))))))

(defun tramp-sudoedit-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; It is unlikely that "chmod -h" works.
    (unless (and (eq flag 'nofollow) (file-symlink-p filename))
      (tramp-flush-file-properties v localname)
      (unless (tramp-sudoedit-send-command
	       v "chmod" (format "%o" mode)
	       (tramp-compat-file-name-unquote localname))
	(tramp-error
	 v 'file-error "Error while changing file's mode %s" filename)))))

(defun tramp-sudoedit-remote-selinux-p (vec)
  "Check, whether SELINUX is enabled on the remote host."
  (with-tramp-connection-property (tramp-get-process vec) "selinux-p"
    (zerop (tramp-call-process vec "selinuxenabled"))))

(defun tramp-sudoedit-handle-file-selinux-context (filename)
  "Like `file-selinux-context' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-selinux-context"
      (let ((context '(nil nil nil nil))
	    (regexp (concat "\\([[:alnum:]_]+\\):" "\\([[:alnum:]_]+\\):"
			    "\\([[:alnum:]_]+\\):" "\\([[:alnum:]_]+\\)")))
	(when (and (tramp-sudoedit-remote-selinux-p v)
		   (tramp-sudoedit-send-command
		    v "ls" "-d" "-Z"
		    (tramp-compat-file-name-unquote localname)))
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (when (re-search-forward regexp (point-at-eol) t)
	      (setq context (list (match-string 1) (match-string 2)
				  (match-string 3) (match-string 4))))))
	;; Return the context.
	context))))

(defun tramp-sudoedit-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  (ignore-errors
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (tramp-message v 5 "file system info: %s" localname)
      (when (tramp-sudoedit-send-command
	     v "df" "--block-size=1" "--output=size,used,avail"
	     (tramp-compat-file-name-unquote localname))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (forward-line)
	  (when (looking-at
		 (concat "[[:space:]]*\\([[:digit:]]+\\)"
			 "[[:space:]]+\\([[:digit:]]+\\)"
			 "[[:space:]]+\\([[:digit:]]+\\)"))
	    (list (string-to-number (match-string 1))
		  ;; The second value is the used size.  We need the
		  ;; free size.
		  (- (string-to-number (match-string 1))
		     (string-to-number (match-string 2)))
		  (string-to-number (match-string 3)))))))))

(defun tramp-sudoedit-handle-set-file-times (filename &optional time flag)
  "Like `set-file-times' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v localname)
    (let ((time
	   (if (or (null time)
		   (tramp-compat-time-equal-p time tramp-time-doesnt-exist)
		   (tramp-compat-time-equal-p time tramp-time-dont-know))
	       (current-time)
	     time)))
      (tramp-sudoedit-send-command
       v "env" "TZ=UTC" "touch" "-t"
       (format-time-string "%Y%m%d%H%M.%S" time t)
       (if (eq flag 'nofollow) "-h" "")
       (tramp-compat-file-name-unquote localname)))))

(defun tramp-sudoedit-handle-file-truename (filename)
  "Like `file-truename' for Tramp files."
  ;; Preserve trailing "/".
  (funcall
   (if (directory-name-p filename) #'file-name-as-directory #'identity)
   ;; Quote properly.
   (funcall
    (if (tramp-compat-file-name-quoted-p filename)
	#'tramp-compat-file-name-quote #'identity)
    (with-parsed-tramp-file-name
	(tramp-compat-file-name-unquote (expand-file-name filename)) nil
      (tramp-make-tramp-file-name
       v
       (with-tramp-file-property v localname "file-truename"
	 (let (result)
	   (tramp-message v 4 "Finding true name for `%s'" filename)
	   (setq result (tramp-sudoedit-send-command-string
			 v "readlink" "--canonicalize-missing" localname))
	   ;; Detect cycle.
	   (when (and (file-symlink-p filename)
		      (string-equal result localname))
	     (tramp-error
	      v 'file-error
	      "Apparent cycle of symbolic links for %s" filename))
	   ;; If the resulting localname looks remote, we must quote it
	   ;; for security reasons.
	   (when (file-remote-p result)
	     (setq result (tramp-compat-file-name-quote result 'top)))
	   (tramp-message v 4 "True name of `%s' is `%s'" localname result)
	   result))
       'nohop)))))

(defun tramp-sudoedit-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  (tramp-sudoedit-send-command
	   v "test" "-w" (tramp-compat-file-name-unquote localname))
	(let ((dir (file-name-directory filename)))
	  (and (file-exists-p dir)
	       (file-writable-p dir)))))))

(defun tramp-sudoedit-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (when (and (null parents) (file-exists-p dir))
      (tramp-error v 'file-already-exists "Directory already exists %s" dir))
    ;; When PARENTS is non-nil, DIR could be a chain of non-existent
    ;; directories a/b/c/...  Instead of checking, we simply flush the
    ;; whole cache.
    (tramp-flush-directory-properties
     v (if parents "/" (file-name-directory localname)))
    (unless (tramp-sudoedit-send-command
	     v (if parents '("mkdir" "-p") "mkdir")
	     "-m" (format "%#o" (default-file-modes))
	     (tramp-compat-file-name-unquote localname))
      (tramp-error v 'file-error "Couldn't make directory %s" dir))))

(defun tramp-sudoedit-handle-make-symbolic-link
    (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
If TARGET is a non-Tramp file, it is used verbatim as the target
of the symlink.  If TARGET is a Tramp file, only the localname
component is used as the target of the symlink."
  (if (not (tramp-tramp-file-p (expand-file-name linkname)))
      (tramp-run-real-handler
       #'make-symbolic-link (list target linkname ok-if-already-exists))

    (with-parsed-tramp-file-name linkname nil
      ;; If TARGET is a Tramp name, use just the localname component.
      ;; Don't check for a proper method.
      (let ((non-essential t))
	(when (and (tramp-tramp-file-p target)
		   (tramp-file-name-equal-p v (tramp-dissect-file-name target)))
	  (setq target (tramp-file-local-name (expand-file-name target)))))

      ;; If TARGET is still remote, quote it.
      (if (tramp-tramp-file-p target)
	  (make-symbolic-link (tramp-compat-file-name-quote target 'top)
	   linkname ok-if-already-exists)

	;; Do the 'confirm if exists' thing.
	(when (file-exists-p linkname)
	  ;; What to do?
	  (if (or (null ok-if-already-exists) ; not allowed to exist
		  (and (numberp ok-if-already-exists)
		       (not
			(yes-or-no-p
			 (format
			  "File %s already exists; make it a link anyway?"
			  localname)))))
	      (tramp-error v 'file-already-exists localname)
	    (delete-file linkname)))

	(tramp-flush-file-properties v localname)
        (tramp-sudoedit-send-command
	 v "ln" "-sf"
	 (tramp-compat-file-name-unquote target)
	 (tramp-compat-file-name-unquote localname))))))

(defun tramp-sudoedit-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-sudoedit-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists
       'keep-date 'preserve-uid-gid)
    (tramp-run-real-handler
     #'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-sudoedit-handle-set-file-acl (filename acl-string)
  "Like `set-file-acl' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (when (and (stringp acl-string) (tramp-sudoedit-remote-acl-p v))
      ;; Massage `acl-string'.
      (setq acl-string (string-join (split-string acl-string "\n" 'omit) ","))
      (prog1
	  (tramp-sudoedit-send-command
	   v "setfacl" "-m"
	   acl-string (tramp-compat-file-name-unquote localname))
	(tramp-flush-file-property v localname "file-acl")))))

(defun tramp-sudoedit-handle-set-file-selinux-context (filename context)
  "Like `set-file-selinux-context' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (and (consp context)
	       (tramp-sudoedit-remote-selinux-p v))
      (let ((user (and (stringp (nth 0 context)) (nth 0 context)))
	    (role (and (stringp (nth 1 context)) (nth 1 context)))
	    (type (and (stringp (nth 2 context)) (nth 2 context)))
	    (range (and (stringp (nth 3 context)) (nth 3 context))))
	(when (tramp-sudoedit-send-command
	       v "chcon"
	       (when user (format "--user=%s" user))
	       (when role (format "--role=%s" role))
	       (when type (format "--type=%s" type))
	       (when range (format "--range=%s" range))
	       (tramp-compat-file-name-unquote localname))
	  (if (and user role type range)
	      (tramp-set-file-property
	       v localname "file-selinux-context" context)
	    (tramp-flush-file-property v localname "file-selinux-context"))
	  t)))))

(defun tramp-sudoedit-handle-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (if (equal id-format 'integer)
      (tramp-sudoedit-send-command-and-read vec "id" "-u")
    (tramp-sudoedit-send-command-string vec "id" "-un")))

(defun tramp-sudoedit-handle-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (if (equal id-format 'integer)
      (tramp-sudoedit-send-command-and-read vec "id" "-g")
    (tramp-sudoedit-send-command-string vec "id" "-gn")))

(defun tramp-sudoedit-handle-set-file-uid-gid (filename &optional uid gid)
  "Like `tramp-set-file-uid-gid' for Tramp files."
    (with-parsed-tramp-file-name filename nil
      (tramp-sudoedit-send-command
       v "chown"
       (format "%d:%d"
	       (or uid (tramp-get-remote-uid v 'integer))
	       (or gid (tramp-get-remote-gid v 'integer)))
       (tramp-unquote-file-local-name filename))))

(defun tramp-sudoedit-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (let* ((uid (or (tramp-compat-file-attribute-user-id
		     (file-attributes filename 'integer))
		    (tramp-get-remote-uid v 'integer)))
	   (gid (or (tramp-compat-file-attribute-group-id
		     (file-attributes filename 'integer))
		    (tramp-get-remote-gid v 'integer)))
	   (flag (and (eq mustbenew 'excl) 'nofollow))
	   (modes (tramp-default-file-modes filename flag))
	   (attributes (file-extended-attributes filename)))
      (prog1
	  (tramp-handle-write-region
	   start end filename append visit lockname mustbenew)

	;; Set the ownership, modes and extended attributes.  This is
	;; not performed in `tramp-handle-write-region'.
	(unless (and (= (tramp-compat-file-attribute-user-id
			 (file-attributes filename 'integer))
			uid)
                     (= (tramp-compat-file-attribute-group-id
			 (file-attributes filename 'integer))
			gid))
          (tramp-set-file-uid-gid filename uid gid))
	(tramp-compat-set-file-modes filename modes flag)
	;; We ignore possible errors, because ACL strings could be
	;; incompatible.
	(when attributes
	  (ignore-errors
	    (set-file-extended-attributes filename attributes)))))))


;; Internal functions.

;; Used in `tramp-sudoedit-sudo-actions'.
(defun tramp-sudoedit-action-sudo (proc vec)
  "Check, whether a sudo process has finished.  Remove unneeded output."
  ;; There might be pending output for the exit status.
  (unless (process-live-p proc)
    (while (tramp-accept-process-output proc 0))
    ;; Delete narrowed region, it would be in the way reading a Lisp form.
    (goto-char (point-min))
    (widen)
    (delete-region (point-min) (point))
    ;; Delete empty lines.
    (goto-char (point-min))
    (while (and (not (eobp)) (= (point) (point-at-eol)))
      (forward-line))
    (delete-region (point-min) (point))
    (tramp-message vec 3 "Process has finished.")
    (throw 'tramp-action 'ok)))

(defun tramp-sudoedit-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  ;; We need a process bound to the connection buffer.  Therefore, we
  ;; create a dummy process.  Maybe there is a better solution?
  (unless (tramp-get-connection-process vec)
    (let ((p (make-network-process
	      :name (tramp-get-connection-name vec)
	      :buffer (tramp-get-connection-buffer vec)
	      :server t :host 'local :service t :noquery t)))
      (process-put p 'vector vec)
      (set-process-query-on-exit-flag p nil)

      ;; Mark process for filelock.
      (tramp-set-connection-property p "lock-pid" (truncate (time-to-seconds)))

      ;; Set connection-local variables.
      (tramp-set-connection-local-variables vec)

      ;; Mark it as connected.
      (tramp-set-connection-property p "connected" t))))

(defun tramp-sudoedit-send-command (vec &rest args)
  "Send commands ARGS to connection VEC.
If an element of ARGS is a list, it will be flattened.  If an
element of ARGS is nil, it will be deleted.
Erases temporary buffer before sending the command.  Returns nil
in case of error, t otherwise."
  (tramp-sudoedit-maybe-open-connection vec)
  (with-current-buffer (tramp-get-connection-buffer vec)
    (erase-buffer)
    (let* ((delete-exited-processes t)
	   (process-connection-type tramp-process-connection-type)
	   (p (apply #'start-process
		     (tramp-get-connection-name vec) (current-buffer)
		     (append
		      (tramp-expand-args
		       vec 'tramp-sudo-login
		       ?h (or (tramp-file-name-host vec) "")
		       ?u (or (tramp-file-name-user vec) ""))
		      (tramp-compat-flatten-tree args))))
	   ;; We suppress the messages `Waiting for prompts from remote shell'.
	   (tramp-verbose (if (= tramp-verbose 3) 2 tramp-verbose))
	   ;; The password shall be cached also in case of "emacs -Q".
	   ;; See `tramp-process-actions'.
	   (tramp-cache-read-persistent-data t)
	   ;; We do not want to save the password.
	   auth-source-save-behavior)
      (tramp-message vec 6 "%s" (string-join (process-command p) " "))
      ;; Avoid process status message in output buffer.
      (set-process-sentinel p #'ignore)
      (process-put p 'vector vec)
      (process-put p 'adjust-window-size-function #'ignore)
      (set-process-query-on-exit-flag p nil)
      (tramp-process-actions p vec nil tramp-sudoedit-sudo-actions)
      (tramp-message vec 6 "%s\n%s" (process-exit-status p) (buffer-string))
      (prog1
	  (zerop (process-exit-status p))
	(delete-process p)))))

(defun tramp-sudoedit-send-command-and-read (vec &rest args)
  "Run command ARGS and return the output, which must be a Lisp expression.
In case there is no valid Lisp expression, it raises an error."
  (when (apply #'tramp-sudoedit-send-command vec args)
    (with-current-buffer (tramp-get-connection-buffer vec)
      ;; Replace stat marker.
      (goto-char (point-min))
      (when (search-forward tramp-stat-marker nil t)
	(goto-char (point-min))
	(while (search-forward "\"" nil t)
	  (replace-match "\\\"" nil 'literal))
	(goto-char (point-min))
	(while (search-forward tramp-stat-marker nil t)
	  (replace-match "\"")))
      ;; Read the expression.
      (tramp-message vec 6 "\n%s" (buffer-string))
      (goto-char (point-min))
      (condition-case nil
	  (prog1 (read (current-buffer))
	    ;; Error handling.
	    (when (re-search-forward "\\S-" (point-at-eol) t)
	      (error nil)))
	(error (tramp-error
		vec 'file-error
		"`%s' does not return a valid Lisp expression: `%s'"
		(car args) (buffer-string)))))))

(defun tramp-sudoedit-send-command-string (vec &rest args)
  "Run command ARGS and return the output as a string."
  (when (apply #'tramp-sudoedit-send-command vec args)
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 6 "\n%s" (buffer-string))
      (goto-char (point-max))
      ;(delete-blank-lines)
      (while (looking-back "[ \t\n]+" nil 'greedy)
	(delete-region (match-beginning 0) (point)))
      (when (> (point-max) (point-min))
	(substring-no-properties (buffer-string))))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-sudoedit 'force)))

(provide 'tramp-sudoedit)

;;; TODO:

;; * Fix *-selinux functions.  Likely, this is due to wrong file
;;   ownership after `write-region' and/or `copy-file'.

;;; tramp-sudoedit.el ends here
