;;; tramp-sudoedit.el --- Functions for accessing under root permissions  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Free Software Foundation, Inc.

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

;; The "sudoedit" Tramp method enables editing a file as a different
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
			           ("-p" "Password:") ("--")))
		(tramp-password-previous-hop t)))

 (add-to-list 'tramp-default-user-alist
	      `(,(rx bos (literal tramp-sudoedit-method) eos)
		nil ,tramp-root-id-string))

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
  '((abbreviate-file-name . tramp-handle-abbreviate-file-name)
    (access-file . tramp-handle-access-file)
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
    (file-group-gid . tramp-handle-file-group-gid)
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
    (file-notify-add-watch . tramp-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-sudoedit-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-sudoedit-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-sudoedit-handle-file-system-info)
    (file-truename . tramp-sudoedit-handle-file-truename)
    (file-user-uid . tramp-handle-file-user-uid)
    (file-writable-p . tramp-sudoedit-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (list-system-processes . ignore)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-sudoedit-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-sudoedit-handle-make-symbolic-link)
    (memory-info . ignore)
    (process-attributes . ignore)
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
    (tramp-get-home-directory . tramp-sudoedit-handle-get-home-directory)
    (tramp-get-remote-gid . tramp-sudoedit-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-sudoedit-handle-get-remote-groups)
    (tramp-get-remote-uid . tramp-sudoedit-handle-get-remote-uid)
    (tramp-set-file-uid-gid . tramp-sudoedit-handle-set-file-uid-gid)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-handle-write-region))
  "Alist of handler functions for Tramp SUDOEDIT method.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-sudoedit-file-name-p (vec-or-filename)
  "Check if it's a VEC-OR-FILENAME for SUDOEDIT."
  (and-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))
	     ((string= (tramp-file-name-method vec) tramp-sudoedit-method)))))

;;;###tramp-autoload
(defun tramp-sudoedit-file-name-handler (operation &rest args)
  "Invoke the SUDOEDIT handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let* ((fn (assoc operation tramp-sudoedit-file-name-handler-alist)))
      (prog1 (save-match-data (apply (cdr fn) args))
	(setq tramp-debug-message-fnh-function (cdr fn)))
    (prog1 (tramp-run-real-handler operation args)
      (setq tramp-debug-message-fnh-function operation))))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-sudoedit-file-name-p #'tramp-sudoedit-file-name-handler))

;; Needed for `tramp-read-passwd'.
(defconst tramp-sudoedit-null-hop
  (make-tramp-file-name
   :method tramp-sudoedit-method :user (user-login-name) :host tramp-system-name)
"Connection hop which identifies the virtual hop before the first one.")


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
  (with-parsed-tramp-file-name (expand-file-name filename) v1
    (with-parsed-tramp-file-name (expand-file-name newname) v2
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
	     (file-name-unquote v1-localname)
	     (file-name-unquote v2-localname))
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
PRESERVE-EXTENDED-ATTRIBUTES activates SELinux and ACL commands.

This function is invoked by `tramp-sudoedit-handle-copy-file' and
`tramp-sudoedit-handle-rename-file'.  It is an error if OP is
neither of `copy' and `rename'.  FILENAME and NEWNAME must be
absolute file names."
  ;; FILENAME and NEWNAME are already expanded.
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))
    (if (file-symlink-p filename)
	(progn
	  (make-symbolic-link
	   (file-symlink-p filename) newname ok-if-already-exists)
	  (when (eq op 'rename) (delete-file filename)))

      ;; FIXME: This should be optimized.  Computing `file-attributes'
      ;; checks already, whether the file exists.
      (let ((t1 (tramp-sudoedit-file-name-p filename))
	    (t2 (tramp-sudoedit-file-name-p newname))
	    (file-times (file-attribute-modification-time
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
	  (tramp-barf-if-file-missing v filename
	    (when (and (not ok-if-already-exists) (file-exists-p newname))
	      (tramp-error v 'file-already-exists newname))
	    (when (and (file-directory-p newname)
		       (not (directory-name-p newname)))
	      (tramp-error v 'file-error "File is a directory %s" newname))

	    (if (or (and (tramp-tramp-file-p filename) (not t1))
		    (and (tramp-tramp-file-p newname)  (not t2)))
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

	    ;; When `newname' is local, we must change the ownership
	    ;; to the local user.
	    (unless (tramp-tramp-file-p newname)
	      (tramp-set-file-uid-gid
	       (concat (file-remote-p filename) newname)
	       (tramp-get-local-uid 'integer)
	       (tramp-get-local-gid 'integer)))

	    ;; Set the time and mode. Mask possible errors.
	    (when keep-date
	      (ignore-errors
		(set-file-times
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
		(tramp-flush-file-properties v2 v2-localname)))))))))

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
	     (file-name-unquote localname))
      (tramp-error v 'file-error "Couldn't delete %s" directory))))

(defun tramp-sudoedit-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (tramp-skeleton-delete-file filename trash
    (unless (tramp-sudoedit-send-command
	     v "rm" "-f" (file-name-unquote localname))
      ;; Propagate the error.
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(tramp-error-with-buffer
	 nil v 'file-error "Couldn't delete %s" filename)))))

(defun tramp-sudoedit-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files.
If the localname part of the given file name starts with \"/../\" then
the result will be a local, non-Tramp, file name."
  ;; If DIR is not given, use `default-directory' or "/".
  (setq dir (or dir default-directory "/"))
  ;; Handle empty NAME.
  (when (string-empty-p name)
    (setq name "."))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (file-name-concat dir name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler #'expand-file-name (list name))
    (with-parsed-tramp-file-name name nil
      ;; Tilde expansion if necessary.  We cannot accept "~/", because
      ;; under sudo "~/" is expanded to the local user home directory
      ;; but to the root home directory.
      (when (tramp-string-empty-or-nil-p localname)
	(setq localname "~"))
      ;; Tilde expansion shall be possible also for quoted localname.
      (when (string-prefix-p "~" (file-name-unquote localname))
	(setq localname (file-name-unquote localname)))
      (unless (file-name-absolute-p localname)
	(setq localname (format "~%s/%s" user localname)))
      (when (string-match
	     (rx bos "~" (group (* (not "/"))) (group (* nonl)) eos) localname)
	(let ((uname (match-string 1 localname))
	      (fname (match-string 2 localname))
	      hname)
	  (when (tramp-string-empty-or-nil-p uname)
	    (setq uname user))
	  (when (setq hname (tramp-get-home-directory v uname))
	    (setq localname (concat hname fname)))))
      ;; Do not keep "/..".
      (when (string-match-p (rx bos "/" (** 1 2 ".") eos) localname)
	(setq localname "/"))
      ;; Do normal `expand-file-name' (this does "~user/", "/./" and "/../").
      (tramp-make-tramp-file-name
       v (if (string-prefix-p "~" localname)
	     localname
	   (tramp-run-real-handler
	    #'expand-file-name (list localname)))))))

(defun tramp-sudoedit-remote-acl-p (vec)
  "Check, whether ACL is enabled on the remote host."
  (with-tramp-connection-property (tramp-get-process vec) "acl-p"
    (zerop (tramp-call-process vec "getfacl" nil nil nil "/"))))

(defun tramp-sudoedit-handle-file-acl (filename)
  "Like `file-acl' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-acl"
      (let ((result (and (tramp-sudoedit-remote-acl-p v)
			 (tramp-sudoedit-send-command-string
			  v "getfacl" "-acp"
			  (file-name-unquote localname)))))
	;; The acl string must have a trailing \n, which is not
	;; provided by `tramp-sudoedit-send-command-string'.  Add it.
	(and (stringp result) (concat result "\n"))))))

(defconst tramp-sudoedit-file-attributes
  (format
   ;; Apostrophes in the stat output are masked as
   ;; `tramp-stat-marker', in order to make a proper shell escape of
   ;; them in file names.  They are replaced in
   ;; `tramp-sudoedit-send-command-and-read'.
   (concat "((%s%%N%s) %%h (%s%%U%s . %%u) (%s%%G%s . %%g)"
	   " %%X %%Y %%Z %%s %s%%A%s t %%i -1)")
   tramp-stat-marker tramp-stat-marker  ; %%N
   tramp-stat-marker tramp-stat-marker  ; %%U
   tramp-stat-marker tramp-stat-marker  ; %%G
   tramp-stat-marker tramp-stat-marker) ; %%A
  "stat format string to produce output suitable for use with
`file-attributes' on the remote file system.")

(defconst tramp-sudoedit-file-attributes-with-selinux
  (format
   ;; Apostrophes in the stat output are masked as
   ;; `tramp-stat-marker', in order to make a proper shell escape of
   ;; them in file names.  They are replaced in
   ;; `tramp-sudoedit-send-command-and-read'.
   (concat "((%s%%N%s) %%h (%s%%U%s . %%u) (%s%%G%s . %%g)"
	   " %%X %%Y %%Z %%s %s%%A%s t %%i -1 %s%%C%s)")
   tramp-stat-marker tramp-stat-marker  ; %%N
   tramp-stat-marker tramp-stat-marker  ; %%U
   tramp-stat-marker tramp-stat-marker  ; %%G
   tramp-stat-marker tramp-stat-marker  ; %%A
   tramp-stat-marker tramp-stat-marker) ; %%C
  "stat format string to produce output suitable for use with
`file-attributes' on the remote file system, including SELinux context.")

(defun tramp-sudoedit-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  ;; The result is cached in `tramp-convert-file-attributes'.
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (tramp-convert-file-attributes v localname id-format
      (cond
       ((tramp-sudoedit-remote-selinux-p v)
	(tramp-sudoedit-send-command-and-read
	 v "env" "QUOTING_STYLE=locale" "stat" "-c"
	 tramp-sudoedit-file-attributes-with-selinux
	 (file-name-unquote localname)))
       (t
	(tramp-sudoedit-send-command-and-read
	 v "env" "QUOTING_STYLE=locale" "stat" "-c"
	 tramp-sudoedit-file-attributes (file-name-unquote localname)))))))

(defun tramp-sudoedit-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      ;; Examine `file-attributes' cache to see if request can be
      ;; satisfied without remote operation.
      (or (tramp-check-cached-permissions v ?x)
	  (tramp-check-cached-permissions v ?s)
	  (tramp-check-cached-permissions v ?t)
          (tramp-sudoedit-send-command
	   v "test" "-x" (file-name-unquote localname))))))

(defun tramp-sudoedit-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (tramp-skeleton-file-exists-p filename
    (tramp-sudoedit-send-command
     v "test" "-e" (file-name-unquote localname))))

(defun tramp-sudoedit-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (tramp-skeleton-file-name-all-completions filename directory
    (all-completions
     filename
     (with-parsed-tramp-file-name (expand-file-name directory) nil
       (with-tramp-file-property v localname "file-name-all-completions"
	 (tramp-sudoedit-send-command
	  v "ls" "-a1" "--quoting-style=literal" "--show-control-chars"
	  (if (tramp-string-empty-or-nil-p localname)
	      "" (file-name-unquote localname)))
	 (mapcar
	  (lambda (f)
	    (if (ignore-errors (file-directory-p (expand-file-name f directory)))
		(file-name-as-directory f)
	      f))
	  (mapcar
	   (lambda (l) (and (not (string-match-p (rx bol (* blank) eol) l)) l))
	   (split-string
	    (tramp-get-buffer-string (tramp-get-connection-buffer v))
	    "\n" 'omit))))))))

(defun tramp-sudoedit-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-readable-p"
      ;; Examine `file-attributes' cache to see if request can be
      ;; satisfied without remote operation.
      (or (tramp-handle-file-readable-p filename)
	  (tramp-sudoedit-send-command
	   v "test" "-r" (file-name-unquote localname))))))

(defun tramp-sudoedit-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  ;; It is unlikely that "chmod -h" works.
  (unless (and (eq flag 'nofollow) (file-symlink-p filename))
    (tramp-skeleton-set-file-modes-times-uid-gid filename
      (unless (tramp-sudoedit-send-command
	       v "chmod" (format "%o" mode) (file-name-unquote localname))
	(tramp-error
	 v 'file-error "Error while changing file's mode %s" filename)))))

(defun tramp-sudoedit-remote-selinux-p (vec)
  "Check, whether SELinux is enabled on the remote host."
  (with-tramp-connection-property (tramp-get-process vec) "selinux-p"
    (zerop (tramp-call-process vec "selinuxenabled"))))

(defun tramp-sudoedit-handle-file-selinux-context (filename)
  "Like `file-selinux-context' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-selinux-context"
      (let ((context '(nil nil nil nil))
	    (regexp (rx
		     (group (+ (any "_" alnum))) ":"
		     (group (+ (any "_" alnum))) ":"
		     (group (+ (any "_" alnum))) ":"
		     (group (+ (any "_" alnum))))))
	(when (and (tramp-sudoedit-remote-selinux-p v)
		   (tramp-sudoedit-send-command
		    v "ls" "-d" "-Z" (file-name-unquote localname)))
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (when (search-forward-regexp regexp (line-end-position) t)
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
	     (file-name-unquote localname))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (forward-line)
	  (when (looking-at
		 (rx (* blank) (group (+ digit))
		     (+ blank) (group (+ digit))
		     (+ blank) (group (+ digit))))
	    (list (string-to-number (match-string 1))
		  ;; The second value is the used size.  We need the
		  ;; free size.
		  (- (string-to-number (match-string 1))
		     (string-to-number (match-string 2)))
		  (string-to-number (match-string 3)))))))))

(defun tramp-sudoedit-handle-set-file-times (filename &optional time flag)
  "Like `set-file-times' for Tramp files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (tramp-sudoedit-send-command
     v "env" "TZ=UTC0" "touch" "-t"
     (format-time-string "%Y%m%d%H%M.%S" (tramp-defined-time time) t)
     (if (eq flag 'nofollow) "-h" "")
     (file-name-unquote localname))))

(defun tramp-sudoedit-handle-file-truename (filename)
  "Like `file-truename' for Tramp files."
  (tramp-skeleton-file-truename filename
    (tramp-sudoedit-send-command-string
     v "readlink" "--canonicalize-missing" localname)))

(defun tramp-sudoedit-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  ;; Examine `file-attributes' cache to see if request can be
	  ;; satisfied without remote operation.
	  (or (tramp-check-cached-permissions v ?w)
	      (tramp-sudoedit-send-command
	       v "test" "-w" (file-name-unquote localname)))
	;; If file doesn't exist, check if directory is writable.
	(and
	 (file-directory-p (file-name-directory filename))
	 (file-writable-p (file-name-directory filename)))))))

(defun tramp-sudoedit-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (tramp-skeleton-make-directory dir parents
    (unless (tramp-sudoedit-send-command
	     v "mkdir" "-m" (format "%#o" (default-file-modes))
	     (file-name-unquote localname))
      (tramp-error v 'file-error "Couldn't make directory %s" dir))))

(defun tramp-sudoedit-handle-make-symbolic-link
    (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files."
  (tramp-skeleton-make-symbolic-link target linkname ok-if-already-exists
    (tramp-sudoedit-send-command
     v "ln" "-sf"
     (file-name-unquote target)
     (file-name-unquote localname))))

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
	   v "setfacl" "-m" acl-string (file-name-unquote localname))
	(tramp-flush-file-property v localname "file-acl")))))

(defun tramp-sudoedit-handle-set-file-selinux-context (filename context)
  "Like `set-file-selinux-context' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
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
	       (file-name-unquote localname))
	  (if (and user role type range)
	      (tramp-set-file-property
	       v localname "file-selinux-context" context)
	    (tramp-flush-file-property v localname "file-selinux-context"))
	  t)))))

(defun tramp-sudoedit-handle-get-home-directory (vec &optional user)
  "The remote home directory for connection VEC as local file name.
If USER is a string, return its home directory instead of the
user identified by VEC.  If there is no user specified in either
VEC or USER, or if there is no home directory, return nil."
  (expand-file-name (concat "~" (or user (tramp-file-name-user vec)))))

(defun tramp-sudoedit-handle-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (tramp-sudoedit-send-command vec "id")
  (tramp-read-id-output vec)
  (tramp-get-connection-property vec (format "uid-%s" id-format)))

(defun tramp-sudoedit-handle-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (tramp-sudoedit-send-command vec "id")
  (tramp-read-id-output vec)
  (tramp-get-connection-property vec (format "gid-%s" id-format)))

(defun tramp-sudoedit-handle-get-remote-groups (vec id-format)
  "Like `tramp-get-remote-groups' for Tramp files.
ID-FORMAT valid values are `string' and `integer'."
  (tramp-sudoedit-send-command vec "id")
  (tramp-read-id-output vec)
  (tramp-get-connection-property vec (format "groups-%s" id-format)))

(defun tramp-sudoedit-handle-set-file-uid-gid (filename &optional uid gid)
  "Like `tramp-set-file-uid-gid' for Tramp files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (tramp-sudoedit-send-command
     v "chown"
     (format "%d:%d"
	     (or uid (tramp-get-remote-uid v 'integer))
	     (or gid (tramp-get-remote-gid v 'integer)))
     (tramp-unquote-file-local-name filename))))


;; Internal functions.

;; Used in `tramp-sudoedit-sudo-actions'.
(defun tramp-sudoedit-action-sudo (proc vec)
  "Check, whether a sudo process has finished.  Remove unneeded output."
  ;; There might be pending output for the exit status.
  (unless (process-live-p proc)
    (while (tramp-accept-process-output proc))
    ;; Delete narrowed region, it would be in the way reading a Lisp form.
    (goto-char (point-min))
    (widen)
    (delete-region (point-min) (point))
    ;; Delete empty lines.
    (goto-char (point-min))
    (while (and (not (eobp)) (= (point) (line-end-position)))
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

  (with-tramp-debug-message vec "Opening connection"
    ;; We need a process bound to the connection buffer.  Therefore,
    ;; we create a dummy process.  Maybe there is a better solution?
    (unless (tramp-get-connection-process vec)
      (let ((p (make-network-process
		:name (tramp-get-connection-name vec)
		:buffer (tramp-get-connection-buffer vec)
		:server t :host 'local :service t :noquery t)))
	(tramp-post-process-creation p vec)

	;; Set connection-local variables.
	(tramp-set-connection-local-variables vec)

	;; Mark it as connected.
	(tramp-set-connection-property p "connected" t)))))

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
	   (p (apply
	       #'tramp-start-process vec
	       (tramp-get-connection-name vec) (current-buffer)
	       (append
		(tramp-expand-args
		 vec 'tramp-sudo-login nil
		 ?h (or (tramp-file-name-host vec) "")
		 ?u (or (tramp-file-name-user vec) ""))
		(flatten-tree args))))
	   ;; We suppress the messages `Waiting for prompts from remote shell'.
	   (tramp-verbose (if (= tramp-verbose 3) 2 tramp-verbose))
	   ;; The password shall be cached also in case of "emacs -Q".
	   ;; See `tramp-process-actions'.
	   (tramp-cache-read-persistent-data t)
	   ;; We do not want to save the password.
	   auth-source-save-behavior)
      ;; Avoid process status message in output buffer.
      (set-process-sentinel p #'ignore)
      (tramp-set-connection-property p "pw-vector" tramp-sudoedit-null-hop)
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
	    (when (search-forward-regexp (rx (not blank)) (line-end-position) t)
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
      (while (looking-back (rx (+ (any " \t\n"))) nil 'greedy)
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
