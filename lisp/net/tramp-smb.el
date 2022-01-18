;;; tramp-smb.el --- Tramp access functions for SMB servers  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2022 Free Software Foundation, Inc.

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

;; Access functions for SMB servers like SAMBA or M$ Windows from Tramp.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'tramp)

;; Define SMB method ...
;;;###tramp-autoload
(defconst tramp-smb-method "smb"
  "Method to connect SAMBA and M$ SMB servers.")

;; ... and add it to the method list.
;;;###tramp-autoload
(unless (memq system-type '(cygwin windows-nt))
  (tramp--with-startup
   (add-to-list 'tramp-methods
                `(,tramp-smb-method
                  ;; This is just a guess.  We don't know whether the share "C$"
                  ;; is available for public use, and whether the user has write
                  ;; access.
                  (tramp-tmpdir "/C$/Temp")
                  ;; Another guess.  We might implement a better check later on.
                  (tramp-case-insensitive t)))))

;; Add a default for `tramp-default-user-alist'.  Rule: For the SMB method,
;; the anonymous user is chosen.
;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-default-user-alist
	      `(,(concat "\\`" tramp-smb-method "\\'") nil nil))

 ;; Add completion function for SMB method.
 (tramp-set-completion-function
  tramp-smb-method
  '((tramp-parse-netrc "~/.netrc"))))

(defcustom tramp-smb-program "smbclient"
  "Name of SMB client to run."
  :group 'tramp
  :type 'string)

(defcustom tramp-smb-acl-program "smbcacls"
  "Name of SMB acls to run."
  :group 'tramp
  :type 'string
  :version "24.4")

(defcustom tramp-smb-conf null-device
  "Path of the \"smb.conf\" file.
If it is nil, no \"smb.conf\" will be added to the `tramp-smb-program'
call, letting the SMB client use the default one."
  :group 'tramp
  :type '(choice (const nil) (file :must-match t)))

(defcustom tramp-smb-options nil
  "List of additional options.
They are added to the `tramp-smb-program' call via \"--option '...'\".

For example, if the deprecated SMB1 protocol shall be used, add to
this variable \"client min protocol=NT1\"."
  :group 'tramp
  :type '(repeat string)
  :version "28.1")

(defvar tramp-smb-version nil
  "Version string of the SMB client.")

(defconst tramp-smb-server-version
  "Domain=\\[[^]]*\\] OS=\\[[^]]*\\] Server=\\[[^]]*\\]"
  "Regexp of SMB server identification.")

(defconst tramp-smb-prompt "^\\(smb:\\|PS\\) .+> \\|^\\s-+Server\\s-+Comment$"
  "Regexp used as prompt in smbclient or powershell.")

(defconst tramp-smb-wrong-passwd-regexp
  (regexp-opt
   '("NT_STATUS_LOGON_FAILURE"
     "NT_STATUS_WRONG_PASSWORD"))
  "Regexp for login error strings of SMB servers.")

(defconst tramp-smb-errors
  (mapconcat
   #'identity
   `(;; Connection error / timeout / unknown command.
     "Connection\\( to \\S-+\\)? failed"
     "Read from server failed, maybe it closed the connection"
     "Call timed out: server did not respond"
     "\\S-+: command not found"
     "Server doesn't support UNIX CIFS calls"
     ,(regexp-opt
       '(;; Samba.
	 "ERRDOS"
	 "ERRHRD"
	 "ERRSRV"
	 "ERRbadfile"
	 "ERRbadpw"
	 "ERRfilexists"
	 "ERRnoaccess"
	 "ERRnomem"
	 "ERRnosuchshare"
	 ;; See /usr/include/samba-4.0/core/ntstatus.h.
	 ;; Windows 4.0 (Windows NT), Windows 5.0 (Windows 2000),
	 ;; Windows 5.1 (Windows XP), Windows 5.2 (Windows Server 2003),
	 ;; Windows 6.0 (Windows Vista), Windows 6.1 (Windows 7),
	 ;; Windows 6.3 (Windows Server 2012, Windows 10).
	 "NT_STATUS_ACCESS_DENIED"
	 "NT_STATUS_ACCOUNT_LOCKED_OUT"
	 "NT_STATUS_BAD_NETWORK_NAME"
	 "NT_STATUS_CANNOT_DELETE"
	 "NT_STATUS_CONNECTION_DISCONNECTED"
	 "NT_STATUS_CONNECTION_REFUSED"
	 "NT_STATUS_CONNECTION_RESET"
	 "NT_STATUS_DIRECTORY_NOT_EMPTY"
	 "NT_STATUS_DUPLICATE_NAME"
	 "NT_STATUS_FILE_IS_A_DIRECTORY"
	 "NT_STATUS_HOST_UNREACHABLE"
	 "NT_STATUS_IMAGE_ALREADY_LOADED"
	 "NT_STATUS_INVALID_LEVEL"
	 "NT_STATUS_INVALID_PARAMETER"
	 "NT_STATUS_INVALID_PARAMETER_MIX"
	 "NT_STATUS_IO_TIMEOUT"
	 "NT_STATUS_LOGON_FAILURE"
	 "NT_STATUS_NETWORK_ACCESS_DENIED"
	 "NT_STATUS_NOT_IMPLEMENTED"
	 "NT_STATUS_NO_LOGON_SERVERS"
	 "NT_STATUS_NO_SUCH_FILE"
	 "NT_STATUS_NO_SUCH_USER"
	 "NT_STATUS_NOT_A_DIRECTORY"
	 "NT_STATUS_NOT_SUPPORTED"
	 "NT_STATUS_OBJECT_NAME_COLLISION"
	 "NT_STATUS_OBJECT_NAME_INVALID"
	 "NT_STATUS_OBJECT_NAME_NOT_FOUND"
	 "NT_STATUS_OBJECT_PATH_SYNTAX_BAD"
	 "NT_STATUS_PASSWORD_MUST_CHANGE"
	 "NT_STATUS_RESOURCE_NAME_NOT_FOUND"
	 "NT_STATUS_REVISION_MISMATCH"
	 "NT_STATUS_SHARING_VIOLATION"
	 "NT_STATUS_TRUSTED_RELATIONSHIP_FAILURE"
	 "NT_STATUS_UNSUCCESSFUL"
	 "NT_STATUS_WRONG_PASSWORD")))
   "\\|")
  "Regexp for possible error strings of SMB servers.
Used instead of analyzing error codes of commands.")

(defconst tramp-smb-actions-with-share
  '((tramp-smb-prompt tramp-action-succeed)
    (tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
This list is used for login to SMB servers.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-smb-actions-without-share
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for login to SMB servers.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-smb-actions-with-tar
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-smb-action-with-tar))
  "List of pattern/action pairs.
This list is used for tar-like copy of directories.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-smb-actions-get-acl
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-smb-action-get-acl))
  "List of pattern/action pairs.
This list is used for smbcacls actions.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-smb-actions-set-acl
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-smb-action-set-acl))
  "List of pattern/action pairs.
This list is used for smbcacls actions.

See `tramp-actions-before-shell' for more info.")

;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-smb-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-smb-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-smb-handle-copy-directory)
    (copy-file . tramp-smb-handle-copy-file)
    (delete-directory . tramp-smb-handle-delete-directory)
    (delete-file . tramp-smb-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-smb-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . ignore)
    (expand-file-name . tramp-smb-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . tramp-smb-handle-file-acl)
    (file-attributes . tramp-smb-handle-file-attributes)
    (file-directory-p .  tramp-handle-file-directory-p)
    (file-file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-handle-file-exists-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-smb-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-smb-handle-file-name-all-completions)
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
    (file-readable-p . tramp-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-smb-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . tramp-smb-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-smb-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-smb-handle-make-directory)
    (make-directory-internal . tramp-smb-handle-make-directory-internal)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-smb-handle-make-symbolic-link)
    (process-file . tramp-smb-handle-process-file)
    (rename-file . tramp-smb-handle-rename-file)
    (set-file-acl . tramp-smb-handle-set-file-acl)
    (set-file-modes . tramp-smb-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-smb-handle-start-file-process)
    (substitute-in-file-name . tramp-smb-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-remote-gid . ignore)
    (tramp-get-remote-uid . ignore)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-smb-handle-write-region))
  "Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;; Options for remote processes via winexe.
(defcustom tramp-smb-winexe-program "winexe"
  "Name of winexe client to run.
If it isn't found in the local $PATH, the absolute path of winexe
shall be given.  This is needed for remote processes."
  :group 'tramp
  :type 'string
  :version "24.3")

(defcustom tramp-smb-winexe-shell-command "powershell.exe"
  "Shell to be used for processes on remote machines.
This must be Powershell V2 compatible."
  :group 'tramp
  :type 'string
  :version "24.3")

(defcustom tramp-smb-winexe-shell-command-switch "-file -"
  "Command switch used together with `tramp-smb-winexe-shell-command'.
This can be used to disable echo etc."
  :group 'tramp
  :type 'string
  :version "24.3")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-smb-file-name-p (filename)
  "Check if it's a FILENAME for SMB servers."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
		tramp-smb-method)))

;;;###tramp-autoload
(defun tramp-smb-file-name-handler (operation &rest args)
  "Invoke the SMB related OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let ((fn (assoc operation tramp-smb-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

;;;###tramp-autoload
(unless (memq system-type '(cygwin windows-nt))
  (tramp--with-startup
   (tramp-register-foreign-file-name-handler
    #'tramp-smb-file-name-p #'tramp-smb-file-name-handler)))

;; File name primitives.

(defun tramp-smb-handle-add-name-to-file
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
      (when (file-directory-p filename)
	(tramp-error
	 v2 'file-error
	 "add-name-to-file: %s must not be a directory" filename))
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
      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-properties v2 v2-localname)
      (unless
	  (tramp-smb-send-command
	   v1
	   (format
	    "%s \"%s\" \"%s\""
	    (if (tramp-smb-get-cifs-capabilities v1) "link" "hardlink")
	    (tramp-smb-get-localname v1)
	    (tramp-smb-get-localname v2)))
	(tramp-error
	 v2 'file-error
	 "error with add-name-to-file, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-smb-action-with-tar (proc vec)
  "Untar from connection buffer."
  (if (not (process-live-p proc))
      (throw 'tramp-action 'process-died)

    (with-current-buffer (tramp-get-connection-buffer vec)
      (goto-char (point-min))
      (when (search-forward-regexp tramp-smb-server-version nil t)
	;; There might be a hidden password prompt.
	(widen)
	(forward-line)
	(tramp-message vec 6 (buffer-substring (point-min) (point)))
	(delete-region (point-min) (point))
	(throw 'tramp-action 'ok)))))

(defun tramp-smb-handle-copy-directory
  (dirname newname &optional keep-date parents copy-contents)
  "Like `copy-directory' for Tramp files."
  (let ((t1 (tramp-tramp-file-p dirname))
	(t2 (tramp-tramp-file-p newname))
	target)
    (with-parsed-tramp-file-name (if t1 dirname newname) nil
      (unless (file-exists-p dirname)
	(tramp-compat-file-missing v dirname))

      ;; `copy-directory-create-symlink' exists since Emacs 28.1.
      (if (and (bound-and-true-p copy-directory-create-symlink)
	       (setq target (file-symlink-p dirname))
	       (tramp-equal-remote dirname newname))
	  (make-symbolic-link
	   target
	   (if (directory-name-p newname)
	       (concat newname (file-name-nondirectory dirname)) newname)
	   t)

	(if copy-contents
	    ;; We must do it file-wise.
	    (tramp-run-real-handler
	     #'copy-directory
	     (list dirname newname keep-date parents copy-contents))

	  (setq dirname (expand-file-name dirname)
		newname (expand-file-name newname))
	  (with-tramp-progress-reporter
	      v 0 (format "Copying %s to %s" dirname newname)
	    (unless (file-exists-p dirname)
	      (tramp-compat-file-missing v dirname))
	    (when (and (file-directory-p newname)
		       (not (directory-name-p newname)))
	      (tramp-error v 'file-already-exists newname))
	    (cond
	     ;; We must use a local temporary directory.
	     ((and t1 t2)
	      (let ((tmpdir (tramp-compat-make-temp-name)))
		(unwind-protect
		    (progn
		      (make-directory tmpdir)
		      (copy-directory
		       dirname (file-name-as-directory tmpdir)
		       keep-date 'parents)
		      (copy-directory
		       (expand-file-name (file-name-nondirectory dirname) tmpdir)
		       newname keep-date parents))
		  (delete-directory tmpdir 'recursive))))

	     ;; We can copy recursively.
	     ;; TODO: Does not work reliably.
	     (nil ;(and (or t1 t2) (tramp-smb-get-cifs-capabilities v))
	      (when (and (file-directory-p newname)
			 (not (string-equal (file-name-nondirectory dirname)
					    (file-name-nondirectory newname))))
		(setq newname
		      (expand-file-name
		       (file-name-nondirectory dirname) newname))
		(if t2 (setq v (tramp-dissect-file-name newname))))
	      (if (not (file-directory-p newname))
		  (make-directory newname parents))

	      (let* ((share (tramp-smb-get-share v))
		     (localname (file-name-as-directory
				 (tramp-compat-string-replace
				  "\\" "/" (tramp-smb-get-localname v))))
		     (tmpdir    (tramp-compat-make-temp-name))
		     (args      (list (concat "//" host "/" share) "-E"))
		     (options   tramp-smb-options))

		(if (not (zerop (length user)))
		    (setq args (append args (list "-U" user)))
		  (setq args (append args (list "-N"))))

		(when domain (setq args (append args (list "-W" domain))))
		(when port   (setq args (append args (list "-p" port))))
		(when tramp-smb-conf
		  (setq args (append args (list "-s" tramp-smb-conf))))
		(while options
		  (setq args
			(append args `("--option" ,(format "%s" (car options))))
			options (cdr options)))
		(setq args
		      (if t1
			  ;; Source is remote.
			  (append args
				  (list "-D" (tramp-unquote-shell-quote-argument
					      localname)
					"-c" (tramp-unquote-shell-quote-argument
					      "tar qc - *")
					"|" "tar" "xfC" "-"
					(tramp-unquote-shell-quote-argument
					 tmpdir)))
			;; Target is remote.
			(append (list
				 "tar" "cfC" "-"
				 (tramp-unquote-shell-quote-argument dirname)
				 "." "|")
				args
				(list "-D" (tramp-unquote-shell-quote-argument
					    localname)
				      "-c" (tramp-unquote-shell-quote-argument
					    "tar qx -")))))

		(unwind-protect
		    (with-temp-buffer
		      ;; Set the transfer process properties.
		      (tramp-set-connection-property
		       v "process-name" (buffer-name (current-buffer)))
		      (tramp-set-connection-property
		       v "process-buffer" (current-buffer))

		      (when t1
			;; The smbclient tar command creates always
			;; complete paths.  We must emulate the
			;; directory structure, and symlink to the
			;; real target.
			(make-directory
			 (expand-file-name
			  ".." (concat tmpdir localname))
			 'parents)
			(make-symbolic-link
			 newname
			 (directory-file-name (concat tmpdir localname))))

		      ;; Use an asynchronous processes.  By this,
		      ;; password can be handled.
		      (let* ((default-directory tmpdir)
			     (p (apply
				 #'start-process
				 (tramp-get-connection-name v)
				 (tramp-get-connection-buffer v)
				 tramp-smb-program args)))

			(tramp-message
			 v 6 "%s" (string-join (process-command p) " "))
			(process-put p 'vector v)
			(process-put p 'adjust-window-size-function #'ignore)
			(set-process-query-on-exit-flag p nil)
			(tramp-process-actions
			 p v nil tramp-smb-actions-with-tar)

			(while (process-live-p p)
			  (sleep-for 0.1))
			(tramp-message v 6 "\n%s" (buffer-string))))

		  ;; Reset the transfer process properties.
		  (tramp-flush-connection-property v "process-name")
		  (tramp-flush-connection-property v "process-buffer")
		  (when t1 (delete-directory tmpdir 'recursive))))

	      ;; Handle KEEP-DATE argument.
	      (when keep-date
		(tramp-compat-set-file-times
		 newname
		 (tramp-compat-file-attribute-modification-time
		  (file-attributes dirname))
		 (unless ok-if-already-exists 'nofollow)))

	      ;; Set the mode.
	      (unless keep-date
		(set-file-modes newname (tramp-default-file-modes dirname)))

	      ;; When newname did exist, we have wrong cached values.
	      (when t2
		(with-parsed-tramp-file-name newname nil
		  (tramp-flush-file-properties v localname))))

	     ;; We must do it file-wise.
	     (t
	      (tramp-run-real-handler
	       #'copy-directory (list dirname newname keep-date parents))))))))))

(defun tramp-smb-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   _preserve-uid-gid _preserve-extended-attributes)
  "Like `copy-file' for Tramp files.
KEEP-DATE has no effect in case NEWNAME resides on an SMB server.
PRESERVE-UID-GID and PRESERVE-EXTENDED-ATTRIBUTES are completely ignored."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (with-tramp-progress-reporter
      (tramp-dissect-file-name
       (if (tramp-tramp-file-p filename) filename newname))
      0 (format "Copying %s to %s" filename newname)

    (if (file-directory-p filename)
	(copy-directory filename newname keep-date 'parents 'copy-contents)

      (unless (file-exists-p filename)
	(tramp-compat-file-missing
	 (tramp-dissect-file-name
	  (if (tramp-tramp-file-p filename) filename newname))
	 filename))

      (if-let ((tmpfile (file-local-copy filename)))
	  ;; Remote filename.
	  (condition-case err
	      (rename-file tmpfile newname ok-if-already-exists)
	    ((error quit)
	     (delete-file tmpfile)
	     (signal (car err) (cdr err))))

	;; Remote newname.
	(when (and (file-directory-p newname)
		   (directory-name-p newname))
	  (setq newname
		(expand-file-name (file-name-nondirectory filename) newname)))

	(with-parsed-tramp-file-name newname nil
	  (when (and (not ok-if-already-exists) (file-exists-p newname))
	    (tramp-error v 'file-already-exists newname))
	  (when (and (file-directory-p newname)
		     (not (directory-name-p newname)))
	    (tramp-error v 'file-error "File is a directory %s" newname))

	  ;; We must also flush the cache of the directory, because
	  ;; `file-attributes' reads the values from there.
	  (tramp-flush-file-properties v localname)
	  (unless (tramp-smb-get-share v)
	    (tramp-error
	     v 'file-error "Target `%s' must contain a share name" newname))
	  (unless (tramp-smb-send-command
		   v (format "put \"%s\" \"%s\""
			     (tramp-compat-file-name-unquote filename)
			     (tramp-smb-get-localname v)))
	    (tramp-error
	     v 'file-error "Cannot copy `%s' to `%s'" filename newname)))))

    ;; KEEP-DATE handling.
    (when keep-date
      (tramp-compat-set-file-times
       newname
       (tramp-compat-file-attribute-modification-time
	(file-attributes filename))
       (unless ok-if-already-exists 'nofollow)))))

(defun tramp-smb-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (tramp-skeleton-delete-directory directory recursive trash
    (when (file-exists-p directory)
      (when recursive
	(mapc
	 (lambda (file)
	   (if (file-directory-p file)
	       (delete-directory file recursive)
	     (delete-file file)))
	 ;; We do not want to delete "." and "..".
	 (directory-files directory 'full directory-files-no-dot-files-regexp)))

      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-directory-properties v localname)
      (unless (tramp-smb-send-command
	       v (format
		  "%s \"%s\""
		  (if (tramp-smb-get-cifs-capabilities v)
		      "posix_rmdir" "rmdir")
		  (tramp-smb-get-localname v)))
	;; Error.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (search-forward-regexp tramp-smb-errors nil t)
	  (tramp-error v 'file-error "%s `%s'" (match-string 0) directory)))

      ;; "rmdir" does not report an error.  So we check ourselves.
      (when (file-exists-p directory)
	(tramp-error v 'file-error "`%s' not removed." directory)))))

(defun tramp-smb-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (when (file-exists-p filename)
    (with-parsed-tramp-file-name filename nil
      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-properties v localname)
      (if (and delete-by-moving-to-trash trash)
	  (move-file-to-trash filename)
	(unless (tramp-smb-send-command
		 v (format
		    "%s \"%s\""
		    (if (tramp-smb-get-cifs-capabilities v) "posix_unlink" "rm")
		    (tramp-smb-get-localname v)))
	  ;; Error.
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (search-forward-regexp tramp-smb-errors nil t)
	    (tramp-error v 'file-error "%s `%s'" (match-string 0) filename)))))))

(defun tramp-smb-handle-directory-files
  (directory &optional full match nosort count)
  "Like `directory-files' for Tramp files."
  (unless (file-exists-p directory)
    (tramp-compat-file-missing (tramp-dissect-file-name directory) directory))
  (let ((result (mapcar #'directory-file-name
			(file-name-all-completions "" directory))))
    ;; Discriminate with regexp.
    (when match
      (setq result
	    (delete nil
		    (mapcar (lambda (x) (when (string-match-p match x) x))
			    result))))

    ;; Sort them if necessary.
    (unless nosort
      (setq result (sort result #'string-lessp)))

    ;; Return count number of results.
    (when (and (natnump count) (> count 0))
      (setq result (nbutlast result (- (length result) count))))

    ;; Prepend directory.
    (when full
      (setq result
	    (mapcar
	     (lambda (x) (format "%s/%s" (directory-file-name directory) x))
	     result)))

    result))

(defun tramp-smb-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Handle empty NAME.
  (when (zerop (length name)) (setq name "."))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (tramp-compat-file-name-concat dir name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler #'expand-file-name (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      ;; Tilde expansion if necessary.  We use the user name as share,
      ;; which is often the case in domains.
      (when (string-match "\\`/?~\\([^/]*\\)" localname)
	(setq localname
	      (replace-match
	       (if (zerop (length (match-string 1 localname)))
		   user
		 (match-string 1 localname))
	       nil nil localname)))
      ;; Make the file name absolute.
      (unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
     ;; Do not keep "/..".
      (when (string-match-p "^/\\.\\.?$" localname)
	(setq localname "/"))
      ;; No tilde characters in file name, do normal
      ;; `expand-file-name' (this does "/./" and "/../").
      (tramp-make-tramp-file-name
       v (tramp-run-real-handler #'expand-file-name (list localname))))))

(defun tramp-smb-action-get-acl (proc vec)
  "Read ACL data from connection buffer."
  (unless (process-live-p proc)
    ;; Accept pending output.
    (while (tramp-accept-process-output proc))
    (with-current-buffer (tramp-get-connection-buffer vec)
      ;; There might be a hidden password prompt.
      (widen)
      (tramp-message vec 10 "\n%s" (buffer-string))
      (goto-char (point-min))
      (while (and (not (eobp)) (not (looking-at-p "^REVISION:")))
	(forward-line)
	(delete-region (point-min) (point)))
      (while (and (not (eobp)) (looking-at-p "^.+:.+"))
 	(forward-line))
      (delete-region (point) (point-max))
      (throw 'tramp-action 'ok))))

(defun tramp-smb-handle-file-acl (filename)
  "Like `file-acl' for Tramp files."
  (ignore-errors
    (with-parsed-tramp-file-name filename nil
      (with-tramp-file-property v localname "file-acl"
	(when (executable-find tramp-smb-acl-program)
	  (let* ((share     (tramp-smb-get-share v))
		 (localname (tramp-compat-string-replace
			     "\\" "/" (tramp-smb-get-localname v)))
		 (args      (list (concat "//" host "/" share) "-E"))
		 (options   tramp-smb-options))

	    (if (not (zerop (length user)))
		(setq args (append args (list "-U" user)))
	      (setq args (append args (list "-N"))))

	    (when domain (setq args (append args (list "-W" domain))))
	    (when port   (setq args (append args (list "-p" port))))
	    (when tramp-smb-conf
	      (setq args (append args (list "-s" tramp-smb-conf))))
	    (while options
	      (setq args
		    (append args `("--option" ,(format "%s" (car options))))
		    options (cdr options)))
	    (setq
	     args
	     (append args (list (tramp-unquote-shell-quote-argument localname)
				(concat "2>" (tramp-get-remote-null-device v)))))

	    (unwind-protect
		(with-temp-buffer
		  ;; Set the transfer process properties.
		  (tramp-set-connection-property
		   v "process-name" (buffer-name (current-buffer)))
		  (tramp-set-connection-property
		   v "process-buffer" (current-buffer))

		  ;; Use an asynchronous process.  By this, password can
		  ;; be handled.
		  (let ((p (apply
			    #'start-process
			    (tramp-get-connection-name v)
			    (tramp-get-connection-buffer v)
			    tramp-smb-acl-program args)))

		    (tramp-message
		     v 6 "%s" (string-join (process-command p) " "))
		    (process-put p 'vector v)
		    (process-put p 'adjust-window-size-function #'ignore)
		    (set-process-query-on-exit-flag p nil)
		    (tramp-process-actions p v nil tramp-smb-actions-get-acl)
		    (when (> (point-max) (point-min))
		      (substring-no-properties (buffer-string)))))

	      ;; Reset the transfer process properties.
	      (tramp-flush-connection-property v "process-name")
	      (tramp-flush-connection-property v "process-buffer"))))))))

(defun tramp-smb-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    (with-parsed-tramp-file-name filename nil
      (with-tramp-file-property
	  v localname (format "file-attributes-%s" id-format)
	(if (tramp-smb-get-stat-capability v)
	    (tramp-smb-do-file-attributes-with-stat v id-format)
	  ;; Reading just the filename entry via "dir localname" is not
	  ;; possible, because when filename is a directory, some
	  ;; smbclient versions return the content of the directory, and
	  ;; other versions don't.  Therefore, the whole content of the
	  ;; upper directory is retrieved, and the entry of the filename
	  ;; is extracted from.
	  (let* ((entries (tramp-smb-get-file-entries
			   (file-name-directory filename)))
		 (entry (assoc (file-name-nondirectory filename) entries))
		 (uid (if (equal id-format 'string) "nobody" -1))
		 (gid (if (equal id-format 'string) "nogroup" -1))
		 (inode (tramp-get-inode v))
		 (device (tramp-get-device v)))

	    ;; Check result.
	    (when entry
	      (list (and (tramp-compat-string-search "d" (nth 1 entry))
			 t)              ;0 file type
		    -1	                 ;1 link count
		    uid	                 ;2 uid
		    gid	                 ;3 gid
		    tramp-time-dont-know ;4 atime
		    (nth 3 entry)        ;5 mtime
		    tramp-time-dont-know ;6 ctime
		    (nth 2 entry)        ;7 size
		    (nth 1 entry)        ;8 mode
		    nil	                 ;9 gid weird
		    inode	         ;10 inode number
		    device))))))))       ;11 file system number

(defun tramp-smb-do-file-attributes-with-stat (vec &optional id-format)
  "Implement `file-attributes' for Tramp files using `stat' command."
  (tramp-message
   vec 5 "file attributes with stat: %s" (tramp-file-name-localname vec))
  (let* (size id link uid gid atime mtime ctime mode inode)
    (when (tramp-smb-send-command
	   vec (format "stat \"%s\"" (tramp-smb-get-localname vec)))

      ;; Loop the listing.
      (with-current-buffer (tramp-get-connection-buffer vec)
	(goto-char (point-min))
	(unless (re-search-forward tramp-smb-errors nil t)
	  (while (not (eobp))
	    (cond
	     ((looking-at
	       (concat
		"Size:\\s-+\\([[:digit:]]+\\)\\s-+"
		"Blocks:\\s-+[[:digit:]]+\\s-+\\(\\w+\\)"))
	      (setq size (string-to-number (match-string 1))
		    id (if (string-equal "directory" (match-string 2)) t
			 (if (string-equal "symbolic" (match-string 2)) ""))))
	     ((looking-at
	       "Inode:\\s-+\\([[:digit:]]+\\)\\s-+Links:\\s-+\\([[:digit:]]+\\)")
	      (setq inode (string-to-number (match-string 1))
		    link (string-to-number (match-string 2))))
	     ((looking-at
	       (concat
		"Access:\\s-+([[:digit:]]+/\\(\\S-+\\))\\s-+"
		"Uid:\\s-+\\([[:digit:]]+\\)\\s-+"
		"Gid:\\s-+\\([[:digit:]]+\\)"))
	      (setq mode (match-string 1)
		    uid (if (equal id-format 'string) (match-string 2)
			  (string-to-number (match-string 2)))
		    gid (if (equal id-format 'string) (match-string 3)
			  (string-to-number (match-string 3)))))
	     ((looking-at
	       (concat
		"Access:\\s-+"
		"\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\)\\s-+"
		"\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"))
	      (setq atime
		    (encode-time
		     (string-to-number (match-string 6)) ;; sec
		     (string-to-number (match-string 5)) ;; min
		     (string-to-number (match-string 4)) ;; hour
		     (string-to-number (match-string 3)) ;; day
		     (string-to-number (match-string 2)) ;; month
		     (string-to-number (match-string 1))))) ;; year
	     ((looking-at
	       (concat
		"Modify:\\s-+"
		"\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\)\\s-+"
		"\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"))
	      (setq mtime
		    (encode-time
		     (string-to-number (match-string 6)) ;; sec
		     (string-to-number (match-string 5)) ;; min
		     (string-to-number (match-string 4)) ;; hour
		     (string-to-number (match-string 3)) ;; day
		     (string-to-number (match-string 2)) ;; month
		     (string-to-number (match-string 1))))) ;; year
	     ((looking-at
	       (concat
		"Change:\\s-+"
		"\\([[:digit:]]+\\)-\\([[:digit:]]+\\)-\\([[:digit:]]+\\)\\s-+"
		"\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"))
	      (setq ctime
		    (encode-time
		     (string-to-number (match-string 6)) ;; sec
		     (string-to-number (match-string 5)) ;; min
		     (string-to-number (match-string 4)) ;; hour
		     (string-to-number (match-string 3)) ;; day
		     (string-to-number (match-string 2)) ;; month
		     (string-to-number (match-string 1)))))) ;; year
	    (forward-line))

	  ;; Resolve symlink.
	  (when (and (stringp id)
		     (tramp-smb-send-command
		      vec
		      (format "readlink \"%s\"" (tramp-smb-get-localname vec))))
	    (goto-char (point-min))
	    (and (looking-at ".+ -> \\(.+\\)")
		 (setq id (match-string 1))))

	  ;; Return the result.
	  (when (or id link uid gid atime mtime ctime size mode inode)
	    (list id link uid gid atime mtime ctime size mode nil inode
		  (tramp-get-device vec))))))))

(defun tramp-smb-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name (file-truename filename) nil
    (unless (file-exists-p (file-truename filename))
      (tramp-compat-file-missing v filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (with-tramp-progress-reporter
	  v 3 (format "Fetching %s to tmp file %s" filename tmpfile)
	(unless (tramp-smb-send-command
		 v (format "get \"%s\" \"%s\""
			   (tramp-smb-get-localname v) tmpfile))
	  ;; Oops, an error.  We shall cleanup.
	  (delete-file tmpfile)
	  (tramp-error
	   v 'file-error "Cannot make local copy of file `%s'" filename)))
      tmpfile)))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-smb-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name (expand-file-name directory) nil
     (with-tramp-file-property v localname "file-name-all-completions"
       (delete-dups
	(mapcar
	 (lambda (x)
	   (list
	    (if (tramp-compat-string-search "d" (nth 1 x))
		(file-name-as-directory (nth 0 x))
	      (nth 0 x))))
	 (tramp-smb-get-file-entries directory)))))))

(defun tramp-smb-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  (ignore-errors
    (unless (file-directory-p filename)
      (setq filename (file-name-directory filename)))
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (when (tramp-smb-get-share v)
	(tramp-message v 5 "file system info: %s" localname)
	(tramp-smb-send-command
	 v (format "du %s/*" (tramp-smb-get-localname v)))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (let (total avail blocksize)
	    (goto-char (point-min))
	    (forward-line)
	    (when (looking-at
		   (concat "[[:space:]]*\\([[:digit:]]+\\)"
			   " blocks of size \\([[:digit:]]+\\)"
			   "\\. \\([[:digit:]]+\\) blocks available"))
	      (setq blocksize (string-to-number (match-string 2))
		    total (* blocksize (string-to-number (match-string 1)))
		    avail (* blocksize (string-to-number (match-string 3)))))
	    (forward-line)
	    (when (looking-at "Total number of bytes: \\([[:digit:]]+\\)")
	      ;; The used number of bytes is not part of the result.
	      ;; As side effect, we store it as file property.
	      (tramp-set-file-property
	       v localname "used-bytes" (string-to-number (match-string 1))))
	    ;; Result.
	    (when (and total avail)
	      (list total (- total avail) avail))))))))

(defun tramp-smb-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (if (file-exists-p filename)
      (tramp-compat-string-search
       "w"
       (or (tramp-compat-file-attribute-modes (file-attributes filename)) ""))
    (let ((dir (file-name-directory filename)))
      (and (file-exists-p dir)
	   (file-writable-p dir)))))

(defun tramp-smb-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (setq filename (expand-file-name filename))
  (unless switches (setq switches ""))
  ;; Mark trailing "/".
  (when (and (directory-name-p filename)
	     (not full-directory-p))
    (setq switches (concat switches "F")))
  (if full-directory-p
      ;; Called from `dired-add-entry'.
      (setq filename (file-name-as-directory filename))
    (setq filename (directory-file-name filename)))
  ;; Check, whether directory is accessible.
  (unless wildcard
    (access-file filename "Reading directory"))
  (with-parsed-tramp-file-name filename nil
    (with-tramp-progress-reporter v 0 (format "Opening directory %s" filename)
      (save-match-data
	(let ((base (file-name-nondirectory filename))
	      ;; We should not destroy the cache entry.
	      (entries (copy-tree
			(tramp-smb-get-file-entries
			 (file-name-directory filename))))
	      (avail (get-free-disk-space filename))
	      ;; `get-free-disk-space' calls `file-system-info', which
	      ;; sets file property "used-bytes" as side effect.
	      (used
	       (format
		"%.0f"
		(/ (tramp-get-file-property v localname "used-bytes" 0) 1024))))

	  (when wildcard
	    (string-match "\\." base)
	    (setq base (replace-match "\\\\." nil nil base))
	    (string-match "\\*" base)
	    (setq base (replace-match ".*" nil nil base))
	    (string-match "\\?" base)
	    (setq base (replace-match ".?" nil nil base)))

	  ;; Filter entries.
	  (setq entries
		(delq
		 nil
		 (if (or wildcard (zerop (length base)))
		     ;; Check for matching entries.
		     (mapcar
		      (lambda (x)
			(when (string-match-p (format "^%s" base) (nth 0 x)) x))
		      entries)
		   ;; We just need the only and only entry FILENAME.
		   (list (assoc base entries)))))

	  ;; Sort entries.
	  (setq entries
		(sort
		 entries
		 (lambda (x y)
		   (if (tramp-compat-string-search "t" switches)
		       ;; Sort by date.
		       (time-less-p (nth 3 y) (nth 3 x))
		     ;; Sort by name.
		     (string-lessp (nth 0 x) (nth 0 y))))))

	  ;; Handle "-F" switch.
	  (when (tramp-compat-string-search "F" switches)
	    (mapc
	     (lambda (x)
	       (unless (zerop (length (car x)))
		 (cond
		  ((char-equal ?d (string-to-char (nth 1 x)))
		   (setcar x (concat (car x) "/")))
		  ((char-equal ?x (string-to-char (nth 1 x)))
		   (setcar x (concat (car x) "*"))))))
	     entries))

	  ;; Insert size information.
	  (when full-directory-p
	    (insert
	     (if avail
		 (format "total used in directory %s available %s\n" used avail)
	       (format "total %s\n" used))))

	  ;; Print entries.
	  (mapc
	   (lambda (x)
	     (unless (zerop (length (nth 0 x)))
	       (let ((attr
		      (when (tramp-smb-get-stat-capability v)
			(ignore-errors
			  (file-attributes
			   (expand-file-name
			    (nth 0 x) (file-name-directory filename))
			   'string)))))
		 (when (tramp-compat-string-search "l" switches)
		   (insert
		    (format
		     "%10s %3d %-8s %-8s %8s %s "
		     (or (tramp-compat-file-attribute-modes attr) (nth 1 x))
		     (or (tramp-compat-file-attribute-link-number attr) 1)
		     (or (tramp-compat-file-attribute-user-id attr) "nobody")
		     (or (tramp-compat-file-attribute-group-id attr) "nogroup")
		     (or (tramp-compat-file-attribute-size attr) (nth 2 x))
		     (format-time-string
		      (if (time-less-p
			   ;; Half a year.
			   (time-since (nth 3 x)) (days-to-time 183))
			  "%b %e %R"
			"%b %e %Y")
		      (nth 3 x))))) ; date

		 ;; We mark the file name.  The inserted name could be
		 ;; from somewhere else, so we use the relative file name
		 ;; of `default-directory'.
		 (let ((start (point)))
		   (insert
		    (file-relative-name
		     (expand-file-name
		      (nth 0 x) (file-name-directory filename))
		     (when full-directory-p (file-name-directory filename))))
		   (put-text-property start (point) 'dired-filename t))

		 ;; Insert symlink.
		 (when (and (tramp-compat-string-search "l" switches)
			    (stringp (tramp-compat-file-attribute-type attr)))
		   (insert " -> " (tramp-compat-file-attribute-type attr))))

	       (insert "\n")
	       (beginning-of-line)))
	   entries))))))

(defun tramp-smb-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (directory-file-name (expand-file-name dir)))
  (unless (file-name-absolute-p dir)
    (setq dir (expand-file-name dir default-directory)))
  (with-parsed-tramp-file-name dir nil
    (when (and (null parents) (file-exists-p dir))
      (tramp-error v 'file-already-exists dir))
    (let* ((ldir (file-name-directory dir)))
      ;; Make missing directory parts.
      (when (and parents
		 (tramp-smb-get-share v)
		 (not (file-directory-p ldir)))
	(make-directory ldir parents))
      ;; Just do it.
      (when (file-directory-p ldir)
	(make-directory-internal dir))
      (unless (file-directory-p dir)
	(tramp-error v 'file-error "Couldn't make directory %s" dir)))))

(defun tramp-smb-handle-make-directory-internal (directory)
  "Like `make-directory-internal' for Tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-name-absolute-p directory)
    (setq directory (expand-file-name directory default-directory)))
  (with-parsed-tramp-file-name directory nil
    (let* ((file (tramp-smb-get-localname v)))
      (when (file-directory-p (file-name-directory directory))
	(tramp-smb-send-command
	 v
	 (if (tramp-smb-get-cifs-capabilities v)
	     (format "posix_mkdir \"%s\" %o" file (default-file-modes))
	   (format "mkdir \"%s\"" file)))
	;; We must also flush the cache of the directory, because
	;; `file-attributes' reads the values from there.
	(tramp-flush-file-properties v localname))
      (unless (file-directory-p directory)
	(tramp-error v 'file-error "Couldn't make directory %s" directory)))))

(defun tramp-smb-handle-make-symbolic-link
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
		       (not (yes-or-no-p
			     (format
			      "File %s already exists; make it a link anyway?"
			      localname)))))
	      (tramp-error v 'file-already-exists localname)
	    (delete-file linkname)))

	(unless (tramp-smb-get-cifs-capabilities v)
	  (tramp-error v 'file-error "make-symbolic-link not supported"))

	;; We must also flush the cache of the directory, because
	;; `file-attributes' reads the values from there.
	(tramp-flush-file-properties v localname)

	(unless
	    (tramp-smb-send-command
	     v (format "symlink \"%s\" \"%s\""
		       (tramp-compat-file-name-unquote target)
		       (tramp-smb-get-localname v)))
	  (tramp-error
	   v 'file-error
	   "error with make-symbolic-link, see buffer `%s' for details"
	   (tramp-get-connection-buffer v)))))))

(defun tramp-smb-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name (expand-file-name default-directory) nil
    (let* ((name (file-name-nondirectory program))
	   (name1 name)
	   (i 0)
	   input tmpinput outbuf command ret)

      ;; Determine input.
      (when infile
	(setq infile (expand-file-name infile))
	(if (tramp-equal-remote default-directory infile)
	    ;; INFILE is on the same remote host.
	    (setq input (tramp-file-local-name infile))
	  ;; INFILE must be copied to remote host.
	  (setq input (tramp-make-tramp-temp-file v)
		tmpinput (tramp-make-tramp-file-name v input))
	  (copy-file infile tmpinput t))
	;; Transform input into a filename powershell does understand.
	(setq input (format "//%s%s" host input)))

      ;; Determine output.
      (cond
       ;; Just a buffer.
       ((bufferp destination)
	(setq outbuf destination))
       ;; A buffer name.
       ((stringp destination)
	(setq outbuf (get-buffer-create destination)))
       ;; (REAL-DESTINATION ERROR-DESTINATION)
       ((consp destination)
	;; output.
	(cond
	 ((bufferp (car destination))
	  (setq outbuf (car destination)))
	 ((stringp (car destination))
	  (setq outbuf (get-buffer-create (car destination))))
	 ((car destination)
	  (setq outbuf (current-buffer))))
	;; stderr.
	(tramp-message v 2 "%s" "STDERR not supported"))
       ;; 't
       (destination
	(setq outbuf (current-buffer))))

      ;; Construct command.
      (setq command (string-join (cons program args) " ")
	    command (if input
			(format
			 "get-content %s | & %s"
			 (tramp-smb-shell-quote-argument input) command)
		      (format "& %s" command)))

      (while (get-process name1)
	;; NAME must be unique as process name.
	(setq i (1+ i)
	      name1 (format "%s<%d>" name i)))

      ;; Set the new process properties.
      (tramp-set-connection-property v "process-name" name1)
      (tramp-set-connection-property
       v "process-buffer"
       (or outbuf (generate-new-buffer tramp-temp-buffer-name)))

      ;; Call it.
      (condition-case nil
	  (with-current-buffer (tramp-get-connection-buffer v)
	    ;; Preserve buffer contents.
	    (narrow-to-region (point-max) (point-max))
	    (tramp-smb-call-winexe v)
	    (when (tramp-smb-get-share v)
	      (tramp-smb-send-command
	       v (format "cd \"//%s%s\"" host (file-name-directory localname))))
	    (tramp-smb-send-command v command)
	    ;; Preserve command output.
	    (narrow-to-region (point-max) (point-max))
	    (let ((p (tramp-get-connection-process v)))
	      (tramp-smb-send-command v "exit $lasterrorcode")
	      (while (process-live-p p)
		(sleep-for 0.1)
		(setq ret (process-exit-status p))))
	    (delete-region (point-min) (point-max))
	    (widen))

	;; When the user did interrupt, we should do it also.  We use
	;; return code -1 as marker.
	(quit
	 (setq ret -1))
	;; Handle errors.
	(error
	 (setq ret 1)))

      ;; We should redisplay the output.
      (when (and display outbuf (get-buffer-window outbuf t)) (redisplay))

      ;; Cleanup.  We remove all file cache values for the connection,
      ;; because the remote process could have changed them.
      (tramp-flush-connection-property v "process-name")
      (tramp-flush-connection-property v "process-buffer")
      (when tmpinput (delete-file tmpinput))
      (unless outbuf
	(kill-buffer (tramp-get-connection-property v "process-buffer" nil)))

      (unless process-file-side-effects
	(tramp-flush-directory-properties v ""))

      ;; Return exit status.
      (if (equal ret -1)
	  (keyboard-quit)
	ret))))

(defun tramp-smb-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (with-parsed-tramp-file-name
      (if (tramp-tramp-file-p filename) filename newname) nil
    (unless (file-exists-p filename)
      (tramp-compat-file-missing v filename))
    (when (and (not ok-if-already-exists) (file-exists-p newname))
      (tramp-error v 'file-already-exists newname))
    (when (and (file-directory-p newname)
	       (not (directory-name-p newname)))
      (tramp-error v 'file-error "File is a directory %s" newname))

    (with-tramp-progress-reporter
	v 0 (format "Renaming %s to %s" filename newname)

      (if (and (not (file-exists-p newname))
	       (tramp-equal-remote filename newname)
	       (string-equal
		(tramp-smb-get-share (tramp-dissect-file-name filename))
		(tramp-smb-get-share (tramp-dissect-file-name newname))))
	  ;; We can rename directly.
	  (with-parsed-tramp-file-name filename v1
	    (with-parsed-tramp-file-name newname v2

	      ;; We must also flush the cache of the directory, because
	      ;; `file-attributes' reads the values from there.
	      (tramp-flush-file-properties v1 v1-localname)
	      (tramp-flush-file-properties v2 v2-localname)
	      (unless (tramp-smb-get-share v2)
		(tramp-error
		 v2 'file-error
		 "Target `%s' must contain a share name" newname))
	      (unless (tramp-smb-send-command
		       v2 (format "rename \"%s\" \"%s\""
				  (tramp-smb-get-localname v1)
				  (tramp-smb-get-localname v2)))
		(tramp-error v2 'file-error "Cannot rename `%s'" filename))))

	;; We must rename via copy.
	(copy-file
	 filename newname ok-if-already-exists 'keep-time 'preserve-uid-gid)
	(if (file-directory-p filename)
	    (delete-directory filename 'recursive)
	  (delete-file filename))))))

(defun tramp-smb-action-set-acl (proc vec)
  "Set ACL data."
  (unless (process-live-p proc)
    ;; Accept pending output.
    (while (tramp-accept-process-output proc))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 10 "\n%s" (buffer-string))
      (throw 'tramp-action 'ok))))

(defun tramp-smb-handle-set-file-acl (filename acl-string)
  "Like `set-file-acl' for Tramp files."
  (ignore-errors
    (with-parsed-tramp-file-name filename nil
      (tramp-flush-file-property v localname "file-acl")

      (when (and (stringp acl-string) (executable-find tramp-smb-acl-program))
	(let* ((share     (tramp-smb-get-share v))
	       (localname (tramp-compat-string-replace
			   "\\" "/" (tramp-smb-get-localname v)))
	       (args      (list (concat "//" host "/" share) "-E" "-S"
				(tramp-compat-string-replace
				 "\n" "," acl-string)))
	       (options   tramp-smb-options))

	  (if (not (zerop (length user)))
	      (setq args (append args (list "-U" user)))
	    (setq args (append args (list "-N"))))

	  (when domain (setq args (append args (list "-W" domain))))
	  (when port   (setq args (append args (list "-p" port))))
	  (when tramp-smb-conf
	    (setq args (append args (list "-s" tramp-smb-conf))))
	  (while options
	    (setq args
		  (append args `("--option" ,(format "%s" (car options))))
		  options (cdr options)))
	  (setq
	   args
	   (append args (list (tramp-unquote-shell-quote-argument localname)
			      "&&" "echo" "tramp_exit_status" "0"
			      "||" "echo" "tramp_exit_status" "1")))

	  (unwind-protect
	      (with-temp-buffer
		;; Set the transfer process properties.
		(tramp-set-connection-property
		 v "process-name" (buffer-name (current-buffer)))
		(tramp-set-connection-property
		 v "process-buffer" (current-buffer))

		;; Use an asynchronous process.  By this, password can
		;; be handled.
		(let ((p (apply
			  #'start-process
			  (tramp-get-connection-name v)
			  (tramp-get-connection-buffer v)
			  tramp-smb-acl-program args)))

		  (tramp-message v 6 "%s" (string-join (process-command p) " "))
		  (process-put p 'vector v)
		  (process-put p 'adjust-window-size-function #'ignore)
		  (set-process-query-on-exit-flag p nil)
		  (tramp-process-actions p v nil tramp-smb-actions-set-acl)
		  ;; This is meant for traces, and returning from the
		  ;; function.  No error is propagated outside, due to
		  ;; the `ignore-errors' closure.
		  (unless (tramp-search-regexp "tramp_exit_status [[:digit:]]+")
		    (tramp-error
		     v 'file-error
		     "Couldn't find exit status of `%s'" tramp-smb-acl-program))
		  (skip-chars-forward "^ ")
		  (when (zerop (read (current-buffer)))
		    ;; Success.
		    (tramp-set-file-property v localname "file-acl" acl-string)
		    t)))

	    ;; Reset the transfer process properties.
	    (tramp-flush-connection-property v "process-name")
	    (tramp-flush-connection-property v "process-buffer")))))))

(defun tramp-smb-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; smbclient chmod does not support nofollow.
    (unless (and (eq flag 'nofollow) (file-symlink-p filename))
      (when (tramp-smb-get-cifs-capabilities v)
	(tramp-flush-file-properties v localname)
	(unless (tramp-smb-send-command
		 v (format "chmod \"%s\" %o" (tramp-smb-get-localname v) mode))
	  (tramp-error
	   v 'file-error "Error while changing file's mode %s" filename))))))

;; We use BUFFER also as connection buffer during setup.  Because of
;; this, its original contents must be saved, and restored once
;; connection has been setup.
(defun tramp-smb-handle-start-file-process (name buffer program &rest args)
  "Like `start-file-process' for Tramp files."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((buffer
	    (if buffer
		(get-buffer-create buffer)
	      ;; BUFFER can be nil.  We use a temporary buffer.
	      (generate-new-buffer tramp-temp-buffer-name)))
	   (command (string-join (cons program args) " "))
	   (bmp (and (buffer-live-p buffer) (buffer-modified-p buffer)))
	   (name1 name)
	   (i 0))
      (unwind-protect
	  (save-excursion
	    (save-restriction
	      (while (get-process name1)
		;; NAME must be unique as process name.
		(setq i (1+ i)
		      name1 (format "%s<%d>" name i)))
	      ;; Set the new process properties.
	      (tramp-set-connection-property v "process-name" name1)
	      (tramp-set-connection-property v "process-buffer" buffer)
	      ;; Activate narrowing in order to save BUFFER contents.
	      (with-current-buffer (tramp-get-connection-buffer v)
		(let ((buffer-undo-list t))
		  (narrow-to-region (point-max) (point-max))
		  (tramp-smb-call-winexe v)
		  (when (tramp-smb-get-share v)
		    (tramp-smb-send-command
		     v (format
			"cd \"//%s%s\""
			host (file-name-directory localname))))
		  (tramp-message v 6 "(%s); exit" command)
		  (tramp-send-string v command)))
	      ;; Return value.
	      (tramp-get-connection-process v)))

	;; Save exit.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (if (tramp-compat-string-search tramp-temp-buffer-name (buffer-name))
	      (progn
		(set-process-buffer (tramp-get-connection-process v) nil)
		(kill-buffer (current-buffer)))
	    (set-buffer-modified-p bmp)))
	(tramp-flush-connection-property v "process-name")
	(tramp-flush-connection-property v "process-buffer")))))

(defun tramp-smb-handle-substitute-in-file-name (filename)
  "Like `substitute-in-file-name' for Tramp files.
\"//\" substitutes only in the local filename part.  Catches
errors for shares like \"C$/\", which are common in Microsoft Windows."
  ;; Check, whether the local part is a quoted file name.
  (if (tramp-compat-file-name-quoted-p filename)
      filename
    (with-parsed-tramp-file-name filename nil
      ;; Ignore in LOCALNAME everything before "//".
      (when (and (stringp localname) (string-match ".+?/\\(/\\|~\\)" localname))
	(setq filename
	      (concat (file-remote-p filename)
		      (replace-match "\\1" nil nil localname)))))
    (condition-case nil
	(tramp-run-real-handler #'substitute-in-file-name (list filename))
      (error filename))))

(defun tramp-smb-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename)
	lockname (file-truename (or lockname filename)))
  (with-parsed-tramp-file-name filename nil
    (when (and mustbenew (file-exists-p filename)
	       (or (eq mustbenew 'excl)
		   (not
		    (y-or-n-p
		     (format "File %s exists; overwrite anyway?" filename)))))
      (tramp-error v 'file-already-exists filename))

    (let ((file-locked (eq (file-locked-p lockname) t))
	  (curbuf (current-buffer))
	  (tmpfile (tramp-compat-make-temp-file filename)))

      ;; Lock file.
      (when (and (not (auto-save-file-name-p (file-name-nondirectory filename)))
		 (file-remote-p lockname)
		 (not file-locked))
	(setq file-locked t)
	;; `lock-file' exists since Emacs 28.1.
	(tramp-compat-funcall 'lock-file lockname))

      (when (and append (file-exists-p filename))
	(copy-file filename tmpfile 'ok))
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (let (create-lockfiles)
        (write-region start end tmpfile append 'no-message))

      (with-tramp-progress-reporter
	  v 3 (format "Moving tmp file %s to %s" tmpfile filename)
	(unwind-protect
	    (unless (tramp-smb-send-command
		     v (format "put %s \"%s\""
			       tmpfile (tramp-smb-get-localname v)))
	      (tramp-error v 'file-error "Cannot write `%s'" filename))
	  (delete-file tmpfile)))

      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-properties v localname)

      (unless (equal curbuf (current-buffer))
	(tramp-error
	 v 'file-error
	 "Buffer has changed from `%s' to `%s'" curbuf (current-buffer)))

      ;; Set file modification time.
      (when (or (eq visit t) (stringp visit))
	(set-visited-file-modtime
	 (or (tramp-compat-file-attribute-modification-time
	      (file-attributes filename))
	     (current-time))))

      ;; Unlock file.
      (when file-locked
	;; `unlock-file' exists since Emacs 28.1.
	(tramp-compat-funcall 'unlock-file lockname))

      ;; The end.
      (when (and (null noninteractive)
		 (or (eq visit t) (string-or-null-p visit)))
	(tramp-message v 0 "Wrote %s" filename))
      (run-hooks 'tramp-handle-write-region-hook))))

;; Internal file name functions.

(defun tramp-smb-get-share (vec)
  "Return the share name of LOCALNAME."
  (save-match-data
    (let ((localname (tramp-file-name-unquote-localname vec)))
      (when (string-match "^/?\\([^/]+\\)/" localname)
	(match-string 1 localname)))))

(defun tramp-smb-get-localname (vec)
  "Return the file name of LOCALNAME.
If VEC has no cifs capabilities, exchange \"/\" by \"\\\\\"."
  (save-match-data
    (let ((localname (tramp-file-name-unquote-localname vec)))
      (setq
       localname
       (if (string-match "^/?[^/]+\\(/.*\\)" localname)
	   ;; There is a share, separated by "/".
	   (if (not (tramp-smb-get-cifs-capabilities vec))
	       (mapconcat
		(lambda (x) (if (equal x ?/) "\\" (char-to-string x)))
		(match-string 1 localname) "")
	     (match-string 1 localname))
	 ;; There is just a share.
	 (if (string-match "^/?\\([^/]+\\)$" localname)
	     (match-string 1 localname)
	   "")))

      ;; Sometimes we have discarded `substitute-in-file-name'.
      (when (string-match "\\(\\$\\$\\)\\(/\\|$\\)" localname)
	(setq localname (replace-match "$" nil nil localname 1)))

      ;; A period followed by a space, or trailing periods and spaces,
      ;; are not supported.
      (when (string-match-p "\\. \\|\\.$\\| $" localname)
	(tramp-error
	 vec 'file-error
	 "Invalid file name %s" (tramp-make-tramp-file-name vec localname)))

      localname)))

;; Share names of a host are cached.  It is very unlikely that the
;; shares do change during connection.
(defun tramp-smb-get-file-entries (directory)
  "Read entries which match DIRECTORY.
Either the shares are listed, or the `dir' command is executed.
Result is a list of (LOCALNAME MODE SIZE MONTH DAY TIME YEAR)."
  ;; If CIFS capabilities are enabled, symlinks are not listed
  ;; by `dir'.  This is a consequence of
  ;; <https://www.samba.org/samba/news/symlink_attack.html>.  See also
  ;; <https://bugzilla.samba.org/show_bug.cgi?id=5116>.
  (with-parsed-tramp-file-name (file-name-as-directory directory) nil
    (setq localname (or localname "/"))
    (with-tramp-file-property v localname "file-entries"
      (let* ((share (tramp-smb-get-share v))
	     (cache (tramp-get-connection-property v "share-cache" nil))
	     res entry)

	(if (and (not share) cache)
	    ;; Return cached shares.
	    (setq res cache)

	  ;; Read entries.
	  (if share
	      (tramp-smb-send-command
	       v (format "dir \"%s*\"" (tramp-smb-get-localname v)))
	    ;; `tramp-smb-maybe-open-connection' lists also the share names.
	    (tramp-smb-maybe-open-connection v))

	  ;; Loop the listing.
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (if (re-search-forward tramp-smb-errors nil t)
		(tramp-error v 'file-error "%s `%s'" (match-string 0) directory)
	      (while (not (eobp))
		(setq entry (tramp-smb-read-file-entry share))
		(forward-line)
		(when entry (push entry res)))))

	  ;; Cache share entries.
	  (unless share
	    (tramp-set-connection-property v "share-cache" res)))

	;; Add directory itself.
	(push '("" "drwxrwxrwx" 0 (0 0)) res)

	;; Return entries.
	(delq nil res)))))

;; Return either a share name (if SHARE is nil), or a file name.
;;
;; If shares are listed, the following format is expected:
;;
;; Disk|                                  - leading spaces
;; [^|]+|                                 - share name, 14 char
;; .*                                     - comment
;;
;; Entries provided by smbclient DIR aren't fully regular.
;; They should have the format
;;
;; \s-\{2,2\}                             - leading spaces
;; \S-\(.*\S-\)\s-*                       - file name, 30 chars, left bound
;; \s-+[ADHRSV]*                          - permissions, 7 chars, right bound
;; \s-                                    - space delimiter
;; \s-+[[:digit:]]+                       - size, 8 chars, right bound
;; \s-\{2,2\}                             - space delimiter
;; \w\{3,3\}                              - weekday
;; \s-                                    - space delimiter
;; \w\{3,3\}                              - month
;; \s-                                    - space delimiter
;; [ 12][[:digit:]]                       - day
;; \s-                                    - space delimiter
;; [[:digit:]]\{2,2\}:[[:digit:]]\{2,2\}:[[:digit:]]\{2,2\} - time
;; \s-                                    - space delimiter
;; [[:digit:]]\{4,4\}                     - year
;;
;; samba/src/client.c (http://samba.org/doxygen/samba/client_8c-source.html)
;; has function display_finfo:
;;
;;   d_printf("  %-30s%7.7s %8.0f  %s",
;;            finfo->name,
;;            attrib_string(finfo->mode),
;;            (double)finfo->size,
;;            asctime(LocalTime(&t)));
;;
;; in Samba 1.9, there's the following code:
;;
;;   DEBUG(0,("  %-30s%7.7s%10d  %s",
;;  	   CNV_LANG(finfo->name),
;;	   attrib_string(finfo->mode),
;;	   finfo->size,
;;	   asctime(LocalTime(&t))));
;;
;; Problems:
;; * Modern regexp constructs, like spy groups and counted repetitions, aren't
;;   available in older Emacsen.
;; * The length of constructs (file name, size) might exceed the default.
;; * File names might contain spaces.
;; * Permissions might be empty.
;;
;; So we try to analyze backwards.
(defun tramp-smb-read-file-entry (share)
  "Parse entry in SMB output buffer.
If SHARE is result, entries are of type dir.  Otherwise, shares
are listed.  Result is the list (LOCALNAME MODE SIZE MTIME)."
;; We are called from `tramp-smb-get-file-entries', which sets the
;; current buffer.
  (let ((line (buffer-substring (point) (point-at-eol)))
	localname mode size month day hour min sec year mtime)

    (if (not share)

	;; Read share entries.
	(when (string-match "^Disk|\\([^|]+\\)|" line)
	  (setq localname (match-string 1 line)
		mode "dr-xr-xr-x"
		size 0))

      ;; Real listing.
      (cl-block nil

	;; year.
	(if (string-match "\\([[:digit:]]+\\)$" line)
	    (setq year (string-to-number (match-string 1 line))
		  line (substring line 0 -5))
	  (cl-return))

	;; time.
	(if (string-match
	     "\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" line)
	    (setq hour (string-to-number (match-string 1 line))
		  min  (string-to-number (match-string 2 line))
		  sec  (string-to-number (match-string 3 line))
		  line (substring line 0 -9))
	  (cl-return))

	;; day.
	(if (string-match "\\([[:digit:]]+\\)$" line)
	    (setq day  (string-to-number (match-string 1 line))
		  line (substring line 0 -3))
	  (cl-return))

	;; month.
	(if (string-match "\\(\\w+\\)$" line)
	    (setq month (match-string 1 line)
		  line  (substring line 0 -4))
	  (cl-return))

	;; weekday.
	(if (string-match-p "\\(\\w+\\)$" line)
	    (setq line (substring line 0 -5))
	  (cl-return))

	;; size.
	(if (string-match "\\([[:digit:]]+\\)$" line)
	    (let ((length (- (max 10 (1+ (length (match-string 1 line)))))))
	      (setq size (string-to-number (match-string 1 line)))
	      (when (string-match
		     "\\([ACDEHNORrsSTV]+\\)" (substring line length))
		(setq length (+ length (match-end 0))))
	      (setq line (substring line 0 length)))
	  (cl-return))

	;; mode: ARCHIVE, COMPRESSED, DIRECTORY, ENCRYPTED, HIDDEN,
	;;       NONINDEXED, NORMAL, OFFLINE, READONLY,
	;;       REPARSE_POINT, SPARSE, SYSTEM, TEMPORARY, VOLID.

	(if (string-match "\\([ACDEHNORrsSTV]+\\)?$" line)
	    (setq
	     mode (or (match-string 1 line) "")
	     mode (format
		    "%s%s"
		    (if (tramp-compat-string-search "D" mode) "d" "-")
		    (mapconcat
		     (lambda (_x) "") "    "
		     (format
		      "r%sx"
		      (if (tramp-compat-string-search "R" mode) "-" "w"))))
	     line (substring line 0 -6))
	  (cl-return))

	;; localname.
	(if (string-match "^\\s-+\\(\\S-\\(.*\\S-\\)?\\)\\s-*$" line)
	    (setq localname (match-string 1 line))
	  (cl-return))))

    (when (and localname mode size)
      (setq mtime
	    (if (and sec min hour day month year)
		(encode-time
		 sec min hour day
		 ;; `parse-time-months' could be customized by the
		 ;; user, so we take its default value.
		 (cdr
		  (assoc
		   (downcase month)
		   (default-toplevel-value 'parse-time-months)))
		 year)
	      tramp-time-dont-know))
      (list localname mode size mtime))))

(defun tramp-smb-get-cifs-capabilities (vec)
  "Check whether the SMB server supports POSIX commands."
  ;; When we are not logged in yet, we return nil.  The
  ;; connection-local property "posix" is not set explicitly; it is
  ;; just checked in order to let a user configure it to nil on hosts
  ;; which return cifs properties, but lack a proper implementation.
  ;; Very old Samba implementations, for example.
  (if (and (process-live-p (tramp-get-connection-process vec))
	   (tramp-get-connection-property vec "posix" t))
      (with-tramp-connection-property
	  (tramp-get-process vec) "cifs-capabilities"
	(save-match-data
	  (when (tramp-smb-send-command vec "posix")
	    (with-current-buffer (tramp-get-connection-buffer vec)
	      (goto-char (point-min))
	      (when
		  (re-search-forward "Server supports CIFS capabilities" nil t)
		(member
		 "pathnames"
		 (split-string
		  (buffer-substring (point) (point-at-eol)) nil 'omit)))))))))

(defun tramp-smb-get-stat-capability (vec)
  "Check whether the SMB server supports the `stat' command."
  ;; When we are not logged in yet, we return nil.
  (if (and (tramp-smb-get-share vec)
	   (process-live-p (tramp-get-connection-process vec)))
      (with-tramp-connection-property (tramp-get-process vec) "stat-capability"
	(tramp-smb-send-command vec "stat \"/\""))))


;; Connection functions.

(defun tramp-smb-send-command (vec command)
  "Send the COMMAND to connection VEC.
Returns nil if there has been an error message from smbclient."
  (tramp-smb-maybe-open-connection vec)
  (tramp-message vec 6 "%s" command)
  (tramp-send-string vec command)
  (tramp-smb-wait-for-output vec))

(defun tramp-smb-maybe-open-connection (vec &optional argument)
  "Maybe open a connection to HOST, log in as USER, using `tramp-smb-program'.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason.
If ARGUMENT is non-nil, use it as argument for
`tramp-smb-winexe-program', and suppress any checks."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  (let* ((share (tramp-smb-get-share vec))
	 (buf (tramp-get-connection-buffer vec))
	 (p (get-buffer-process buf)))

    ;; Check whether we still have the same smbclient version.
    ;; Otherwise, we must delete the connection cache, because
    ;; capabilities might have changed.
    (unless (or argument (processp p))
      (let ((default-directory tramp-compat-temporary-file-directory)
	    (command (concat tramp-smb-program " -V")))

	(unless tramp-smb-version
	  (unless (executable-find tramp-smb-program)
	    (tramp-error
	     vec 'file-error
	     "Cannot find command %s in %s" tramp-smb-program exec-path))
	  (setq tramp-smb-version (shell-command-to-string command))
	  (tramp-message vec 6 command)
	  (tramp-message vec 6 "\n%s" tramp-smb-version)
	  (if (string-match "[ \t\n\r]+\\'" tramp-smb-version)
	      (setq tramp-smb-version
		    (replace-match "" nil nil tramp-smb-version))))

	(unless (string-equal
		 tramp-smb-version
		 (tramp-get-connection-property
		  vec "smbclient-version" tramp-smb-version))
	  (tramp-flush-directory-properties vec "")
	  (tramp-flush-connection-properties vec))

	(tramp-set-connection-property
	 vec "smbclient-version" tramp-smb-version)))

    ;; If too much time has passed since last command was sent, look
    ;; whether there has been an error message; maybe due to
    ;; connection timeout.
    (with-current-buffer buf
      (goto-char (point-min))
      (when (and (time-less-p
		  60 (time-since
		      (tramp-get-connection-property p "last-cmd-time" 0)))
		 (process-live-p p)
		 (re-search-forward tramp-smb-errors nil t))
	(delete-process p)
	(setq p nil)))

    ;; Check whether it is still the same share.
    (unless (and (process-live-p p)
		 (or argument
		     (string-equal
		      share
		      (tramp-get-connection-property p "smb-share" ""))))
      (save-match-data
	;; There might be unread output from checking for share names.
	(when buf (with-current-buffer buf (erase-buffer)))
	(when (and p (processp p)) (delete-process p))

	(let* ((user   (tramp-file-name-user vec))
	       (host   (tramp-file-name-host vec))
	       (domain (tramp-file-name-domain vec))
	       (port   (tramp-file-name-port vec))
	       (options tramp-smb-options)
	       args)

	  (cond
	   (argument
	    (setq args (list (concat "//" host))))
	   (share
	    (setq args (list (concat "//" host "/" share))))
	   (t
	    (setq args (list "-g" "-L" host ))))

	  (if (not (zerop (length user)))
	      (setq args (append args (list "-U" user)))
	    (setq args (append args (list "-N"))))

	  (when domain (setq args (append args (list "-W" domain))))
	  (when port   (setq args (append args (list "-p" port))))
	  (when tramp-smb-conf
	    (setq args (append args (list "-s" tramp-smb-conf))))
	  (dolist (option options)
	    (setq args (append args (list "--option" option))))
	  (when argument
	    (setq args (append args (list argument))))

	  ;; OK, let's go.
	  (with-tramp-progress-reporter
	      vec 3
	      (format "Opening connection for //%s%s/%s"
		      (if (not (zerop (length user))) (concat user "@") "")
		      host (or share ""))

	    (let* ((coding-system-for-read nil)
		   (process-connection-type tramp-process-connection-type)
		   (p (let ((default-directory
			      tramp-compat-temporary-file-directory)
			    (process-environment
			     (cons (concat "TERM=" tramp-terminal-type)
				   process-environment)))
			(apply #'start-process
			       (tramp-get-connection-name vec)
			       (tramp-get-connection-buffer vec)
			       (if argument
				   tramp-smb-winexe-program tramp-smb-program)
			       args))))

	      (tramp-message vec 6 "%s" (string-join (process-command p) " "))
	      (process-put p 'vector vec)
	      (process-put p 'adjust-window-size-function #'ignore)
	      (set-process-query-on-exit-flag p nil)

	      ;; Set connection-local variables.
	      (tramp-set-connection-local-variables vec)

	      (condition-case err
		  (let ((inhibit-message t))
		    ;; Play login scenario.
		    (tramp-process-actions
		     p vec nil
		     (if (or argument share)
			 tramp-smb-actions-with-share
		       tramp-smb-actions-without-share))

		    ;; Check server version.
		    ;; FIXME: With recent smbclient versions, this
		    ;; information isn't printed anymore.
		    ;; (unless argument
		    ;;   (with-current-buffer (tramp-get-connection-buffer vec)
		    ;; 	(goto-char (point-min))
		    ;; 	(search-forward-regexp tramp-smb-server-version nil t)
		    ;; 	(let ((smbserver-version (match-string 0)))
		    ;; 	  (unless
		    ;; 	      (string-equal
		    ;; 	       smbserver-version
		    ;; 	       (tramp-get-connection-property
		    ;; 		vec "smbserver-version" smbserver-version))
		    ;; 	    (tramp-flush-directory-properties vec "")
		    ;; 	    (tramp-flush-connection-properties vec))
		    ;; 	  (tramp-set-connection-property
		    ;; 	   vec "smbserver-version" smbserver-version))))

		    ;; Set chunksize to 1.  smbclient reads its input
		    ;; character by character; if we send the string
		    ;; at once, it is read painfully slow.
		    (tramp-set-connection-property p "smb-share" share)
		    (tramp-set-connection-property p "chunksize" 1)

		    ;; Mark it as connected.
		    (tramp-set-connection-property p "connected" t))

		;; Check for the error reason.  If it was due to wrong
		;; password, reestablish the connection.  We cannot
		;; handle this in `tramp-process-actions', because
		;; smbclient does not ask for the password, again.
		(error
		 (with-current-buffer (tramp-get-connection-buffer vec)
		   (goto-char (point-min))
		   (if (and (bound-and-true-p auth-sources)
			    (search-forward-regexp
			     tramp-smb-wrong-passwd-regexp nil t))
		       ;; Disable `auth-source' and `password-cache'.
		       (let (auth-sources)
			 (tramp-message
			  vec 3 "Retry connection with new password")
			 (tramp-cleanup-connection vec t)
			 (tramp-smb-maybe-open-connection vec argument))
		     ;; Propagate the error.
		     (signal (car err) (cdr err)))))))))))))

;; We don't use timeouts.  If needed, the caller shall wrap around.
(defun tramp-smb-wait-for-output (vec)
  "Wait for output from smbclient command.
Removes smb prompt.  Returns nil if an error message has appeared."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (let ((p (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))

      ;; Read pending output.
      (while (not (re-search-forward tramp-smb-prompt nil t))
	(while (tramp-accept-process-output p 0))
	(goto-char (point-min)))
      (tramp-message vec 6 "\n%s" (buffer-string))

      ;; Remove prompt.
      (goto-char (point-min))
      (when (re-search-forward tramp-smb-prompt nil t)
	(goto-char (point-max))
	(re-search-backward tramp-smb-prompt nil t)
	(delete-region (point) (point-max)))

      ;; Return value is whether no error message has appeared.
      (goto-char (point-min))
      (not (re-search-forward tramp-smb-errors nil t)))))

(defun tramp-smb-kill-winexe-function ()
  "Send SIGKILL to the winexe process."
  (ignore-errors
    (let ((p (get-buffer-process (current-buffer))))
      (when (process-live-p p)
	(signal-process (process-id p) 'SIGINT)))))

(defun tramp-smb-call-winexe (vec)
  "Apply a remote command, if possible, using `tramp-smb-winexe-program'."
  ;; Check for program.
  (unless (executable-find tramp-smb-winexe-program)
    (tramp-error
     vec 'file-error "Cannot find program: %s" tramp-smb-winexe-program))

  ;; winexe does not supports ports.
  (when (tramp-file-name-port vec)
    (tramp-error vec 'file-error "Port not supported for remote processes"))

  (tramp-smb-maybe-open-connection
   vec
   (format
    "%s %s"
    tramp-smb-winexe-shell-command tramp-smb-winexe-shell-command-switch))

  (add-hook 'kill-buffer-hook #'tramp-smb-kill-winexe-function nil t)

  ;; Suppress "^M".  Shouldn't we specify utf8?
  (set-process-coding-system (tramp-get-connection-process vec) 'raw-text-dos)

  ;; Set width to 128.  This avoids mixing prompt and long error messages.
  (tramp-smb-send-command vec "$rawui = (Get-Host).UI.RawUI")
  (tramp-smb-send-command vec "$bufsize = $rawui.BufferSize")
  (tramp-smb-send-command vec "$winsize = $rawui.WindowSize")
  (tramp-smb-send-command vec "$bufsize.Width = 128")
  (tramp-smb-send-command vec "$winsize.Width = 128")
  (tramp-smb-send-command vec "$rawui.BufferSize = $bufsize")
  (tramp-smb-send-command vec "$rawui.WindowSize = $winsize"))

(defun tramp-smb-shell-quote-argument (s)
  "Similar to `shell-quote-argument', but uses Windows cmd syntax."
  (let ((system-type 'ms-dos))
    (tramp-unquote-shell-quote-argument s)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-smb 'force)))

(provide 'tramp-smb)

;;; TODO:

;; * Return more comprehensive file permission string.
;;
;; * Try to remove the inclusion of dummy "" directory.  Seems to be at
;;   several places, especially in `tramp-smb-handle-insert-directory'.
;;
;; * Keep a separate connection process per share.

;;; tramp-smb.el ends here
