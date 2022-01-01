;;; tramp-sshfs.el --- Tramp access functions via sshfs  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; sshfs is a program to mount a virtual file system, based on an sftp
;; connection.  Tramp uses its mount utility to access files and
;; directories there.

;; A remote file under sshfs control has the form
;; "/sshfs:user@host#port:/path/to/file".  User name and port number
;; are optional.

;;; Code:

(require 'tramp)
(require 'tramp-fuse)

;;;###tramp-autoload
(defconst tramp-sshfs-method "sshfs"
  "Tramp method for sshfs mounts.")

(defcustom tramp-sshfs-program "sshfs"
  "The sshfs mount command."
  :group 'tramp
  :version "28.1"
  :type 'string)

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
	      `(,tramp-sshfs-method
		(tramp-mount-args            (("-C") ("-p" "%p")
					      ("-o" "idmap=user,reconnect")))
		;; These are for remote processes.
                (tramp-login-program        "ssh")
                (tramp-login-args           (("-q")("-l" "%u") ("-p" "%p")
				             ("-e" "none") ("%h") ("%l")))
                (tramp-direct-async         t)
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))

 (add-to-list 'tramp-connection-properties
	      `(,(format "/%s:" tramp-sshfs-method) "direct-async-process" t))

 (tramp-set-completion-function
  tramp-sshfs-method tramp-completion-function-alist-ssh))


;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-sshfs-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-sshfs-handle-copy-file)
    (delete-directory . tramp-fuse-handle-delete-directory)
    (delete-file . tramp-fuse-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-fuse-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-sshfs-handle-exec-path)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-fuse-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-fuse-handle-file-executable-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-fuse-handle-file-name-all-completions)
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
    (file-readable-p . tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-sshfs-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . tramp-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-sshfs-handle-insert-file-contents)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-fuse-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-handle-make-process)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (process-file . tramp-sshfs-handle-process-file)
    (rename-file . tramp-sshfs-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . tramp-sshfs-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-remote-gid . ignore)
    (tramp-get-remote-uid . ignore)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-sshfs-handle-write-region))
"Alist of handler functions for Tramp SSHFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-sshfs-file-name-p (filename)
  "Check if it's a FILENAME for sshfs."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
	        tramp-sshfs-method)))

;;;###tramp-autoload
(defun tramp-sshfs-file-name-handler (operation &rest args)
  "Invoke the sshfs handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let ((fn (assoc operation tramp-sshfs-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-sshfs-file-name-p #'tramp-sshfs-file-name-handler))


;; File name primitives.

(defun tramp-sshfs-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-date
     preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (if (file-directory-p filename)
      (copy-directory filename newname keep-date t)
    (copy-file
     (if (tramp-sshfs-file-name-p filename)
	 (tramp-fuse-local-file-name filename) filename)
     (if (tramp-sshfs-file-name-p newname)
	 (tramp-fuse-local-file-name newname) newname)
     ok-if-already-exists keep-date
     preserve-uid-gid preserve-extended-attributes)
    (when (tramp-sshfs-file-name-p newname)
      (with-parsed-tramp-file-name newname nil
	(tramp-flush-file-properties v localname)))))

(defun tramp-sshfs-handle-exec-path ()
  "Like `exec-path' for Tramp files."
  (append
   (with-parsed-tramp-file-name default-directory nil
     (with-tramp-connection-property (tramp-get-process v) "remote-path"
       (with-temp-buffer
	 (process-file "getconf" nil t nil "PATH")
	 (split-string
	  (progn
	    ;; Read the expression.
	    (goto-char (point-min))
	    (buffer-substring (point) (point-at-eol)))
	  ":" 'omit))))
   ;; The equivalent to `exec-directory'.
   `(,(tramp-file-local-name (expand-file-name default-directory)))))

(defun tramp-sshfs-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  ;;`file-system-info' exists since Emacs 27.1.
  (tramp-compat-funcall 'file-system-info (tramp-fuse-local-file-name filename)))

(defun tramp-sshfs-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (setq filename (expand-file-name filename))
  (let (signal-hook-function result)
    (unwind-protect
        (setq result
	      (insert-file-contents
	       (tramp-fuse-local-file-name filename) visit beg end replace))
      (when visit (setq buffer-file-name filename))
      (cons filename (cdr result)))))

(defun tramp-sshfs-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name (expand-file-name default-directory) nil
    (let ((command
	   (format
	    "cd %s && exec %s"
	    localname
	    (mapconcat #'tramp-shell-quote-argument (cons program args) " "))))
      (unwind-protect
	  (apply
	   #'tramp-call-process
	   v (tramp-get-method-parameter v 'tramp-login-program)
	   infile destination display
	   (tramp-expand-args
	    v 'tramp-login-args
	    ?h (or (tramp-file-name-host v) "")
	    ?u (or (tramp-file-name-user v) "")
	    ?p (or (tramp-file-name-port v) "")
	    ?l command))

	(unless process-file-side-effects
          (tramp-flush-directory-properties v ""))))))

(defun tramp-sshfs-handle-rename-file
    (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (rename-file
   (if (tramp-sshfs-file-name-p filename)
       (tramp-fuse-local-file-name filename) filename)
   (if (tramp-sshfs-file-name-p newname)
       (tramp-fuse-local-file-name newname) newname)
   ok-if-already-exists)
  (when (tramp-sshfs-file-name-p filename)
    (with-parsed-tramp-file-name filename nil
      (tramp-flush-file-properties v localname)))
  (when (tramp-sshfs-file-name-p newname)
    (with-parsed-tramp-file-name newname nil
      (tramp-flush-file-properties v localname))))

(defun tramp-sshfs-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (unless (and (eq flag 'nofollow) (file-symlink-p filename))
      (tramp-flush-file-properties v localname)
      (tramp-compat-set-file-modes
       (tramp-fuse-local-file-name filename) mode flag))))

(defun tramp-sshfs-handle-write-region
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

    (let ((file-locked (eq (file-locked-p lockname) t)))

      ;; Lock file.
      (when (and (not (auto-save-file-name-p (file-name-nondirectory filename)))
		 (file-remote-p lockname)
		 (not file-locked))
	(setq file-locked t)
	;; `lock-file' exists since Emacs 28.1.
	(tramp-compat-funcall 'lock-file lockname))

      (let (create-lockfiles)
	(write-region
	 start end (tramp-fuse-local-file-name filename) append 'nomessage)
	(tramp-flush-file-properties v localname))

      ;; Unlock file.
      (when file-locked
	;; `unlock-file' exists since Emacs 28.1.
	(tramp-compat-funcall 'unlock-file lockname))

      ;; The end.
      (when (and (null noninteractive)
		 (or (eq visit t) (string-or-null-p visit)))
	(tramp-message v 0 "Wrote %s" filename))
      (run-hooks 'tramp-handle-write-region-hook))))


;; File name conversions.

(defun tramp-sshfs-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  ;; We need a process bound to the connection buffer.  Therefore, we
  ;; create a dummy process.  Maybe there is a better solution?
  (unless (get-buffer-process (tramp-get-connection-buffer vec))
    (let ((p (make-network-process
	      :name (tramp-get-connection-name vec)
	      :buffer (tramp-get-connection-buffer vec)
	      :server t :host 'local :service t :noquery t)))
      (process-put p 'vector vec)
      (set-process-query-on-exit-flag p nil)

      ;; Mark process for filelock.
      (tramp-set-connection-property p "lock-pid" (truncate (time-to-seconds)))

      ;; Set connection-local variables.
      (tramp-set-connection-local-variables vec)))

  ;; Create directory.
  (unless (file-directory-p (tramp-fuse-mount-point vec))
    (make-directory (tramp-fuse-mount-point vec) 'parents))

  (unless
      (or (tramp-fuse-mounted-p vec)
	  (with-temp-buffer
	    (zerop
	     (apply
	      #'tramp-call-process
	      vec tramp-sshfs-program nil t nil
	      (tramp-fuse-mount-spec vec)
	      (tramp-fuse-mount-point vec)
	      (tramp-expand-args
	       vec 'tramp-mount-args
	       ?p (or (tramp-file-name-port vec) ""))))))
    (tramp-error
     vec 'file-error "Error mounting %s" (tramp-fuse-mount-spec vec)))

  ;; Mark it as connected.
  (add-to-list 'tramp-fuse-mount-points (tramp-file-name-unify vec))
  (tramp-set-connection-property
   (tramp-get-connection-process vec) "connected" t)

  ;; In `tramp-check-cached-permissions', the connection properties
  ;; "{uid,gid}-{integer,string}" are used.  We set them to proper values.
  (with-tramp-connection-property
      vec "uid-integer" (tramp-get-local-uid 'integer))
  (with-tramp-connection-property
      vec "gid-integer" (tramp-get-local-gid 'integer))
  (with-tramp-connection-property
      vec "uid-string" (tramp-get-local-uid 'string))
  (with-tramp-connection-property
      vec "gid-string" (tramp-get-local-gid 'string)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-sshfs 'force)))

(provide 'tramp-sshfs)

;;; tramp-sshfs.el ends here
