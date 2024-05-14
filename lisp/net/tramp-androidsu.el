;;; tramp-androidsu.el --- Tramp method for Android superuser shells  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Po Lu
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

;; `su' method implementation for Android.
;;
;; The `su' method struggles (as do other shell-based methods) with the
;; crippled versions of many Unix utilities installed on Android,
;; workarounds for which are implemented in the `adb' method.  This
;; method defines a shell-based method that is identical in function to
;; and replaces if connecting to a local Android machine `su', but
;; reuses such code from the `adb' method where applicable and also
;; provides for certain mannerisms of popular Android `su'
;; implementations.

;;; Code:

(require 'tramp)
(require 'tramp-adb)
(require 'tramp-sh)

;;;###tramp-autoload
(defconst tramp-androidsu-method "androidsu"
  "When this method name is used, forward all calls to su.")

;;;###tramp-autoload
(defcustom tramp-androidsu-mount-global-namespace t
  "When non-nil, browse files from within the global mount namespace.
On systems that assign each application a unique view of the
filesystem by executing them within individual mount namespaces
and thus conceal each application's data directories from
others, invoke `su' with the option `-mm' in order for the shell
launched to run within the global mount namespace, so that Tramp
may edit files belonging to any and all applications."
  :group 'tramp
  :version "30.1"
  :type 'boolean)

;;;###tramp-autoload
(defcustom tramp-androidsu-remote-path '("/system/bin"
                                         "/system/xbin")
  "Directories in which to search for transfer programs and the like."
  :group 'tramp
  :version "30.1"
  :type '(repeat string))

(defvar tramp-androidsu-su-mm-supported 'unknown
  "Whether `su -mm' is supported on this system.")

;;;###tramp-autoload
(defconst tramp-androidsu-local-shell-name "/system/bin/sh"
  "Name of the local shell on Android.")

;;;###tramp-autoload
(defconst tramp-androidsu-local-tmp-directory "/data/local/tmp"
  "Name of the local temporary directory on Android.")

;;;###tramp-autoload
(defun tramp-enable-androidsu-method ()
  "Enable \"androidsu\" method."
  (add-to-list 'tramp-methods
	       `(,tramp-androidsu-method
                 (tramp-login-program       "su")
                 (tramp-login-args          (("-") ("%u")))
                 (tramp-remote-shell        ,tramp-androidsu-local-shell-name)
                 (tramp-remote-shell-login  ("-l"))
                 (tramp-remote-shell-args   ("-c"))
                 (tramp-tmpdir              ,tramp-androidsu-local-tmp-directory)
                 (tramp-connection-timeout  10)
                 (tramp-shell-name	   ,tramp-androidsu-local-shell-name)))

  (add-to-list 'tramp-default-user-alist
	       `(,(rx bos (literal tramp-androidsu-method) eos)
		 nil ,tramp-root-id-string)))

;;;###tramp-autoload
(tramp--with-startup
 (when (eq system-type 'android)
   (tramp-enable-androidsu-method)))

(defvar android-use-exec-loader) ; androidfns.c.

(defun tramp-androidsu-maybe-open-connection (vec)
  "Open a connection VEC if not already open.
Mostly identical to `tramp-adb-maybe-open-connection', but also disables
multibyte mode and waits for the shell prompt to appear."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  (with-tramp-debug-message vec "Opening connection"
    (let ((p (tramp-get-connection-process vec))
	  (process-name (tramp-get-connection-property vec "process-name"))
	  (process-environment (copy-sequence process-environment)))
      ;; Open a new connection.
      (condition-case err
	  (unless (process-live-p p)
	    (with-tramp-progress-reporter
		vec 3
		(if (tramp-string-empty-or-nil-p (tramp-file-name-user vec))
		    (format "Opening connection %s for %s using %s"
			    process-name
			    (tramp-file-name-host vec)
			    (tramp-file-name-method vec))
		  (format "Opening connection %s for %s@%s using %s"
			  process-name
			  (tramp-file-name-user vec)
			  (tramp-file-name-host vec)
			  (tramp-file-name-method vec)))
              (let* ((coding-system-for-read 'utf-8-unix)
                     (process-connection-type tramp-process-connection-type)
                     ;; The executable loader cannot execute setuid
                     ;; binaries, such as su.
                     (android-use-exec-loader nil)
		     (p (start-process (tramp-get-connection-name vec)
			               (tramp-get-connection-buffer vec)
                                       ;; Disregard
                                       ;; `tramp-encoding-shell', as
                                       ;; there's no guarantee that it's
                                       ;; possible to execute it with
                                       ;; `android-use-exec-loader' off.
			               tramp-androidsu-local-shell-name "-i"))
		     (user (tramp-file-name-user vec))
                     su-binary path command)
                ;; Set sentinel.  Initialize variables.
	        (set-process-sentinel p #'tramp-process-sentinel)
	        (tramp-post-process-creation p vec)
                ;; Replace `login-args' place holders.  `PATH' must be
                ;; set to `tramp-androidsu-remote-path', as some `su'
                ;; implementations propagate their callers' environments
                ;; to the root session, which might be contaminated with
                ;; incompatible `ls' binaries or similar.
		(setq path (tramp-shell-quote-argument
                            (string-join tramp-androidsu-remote-path ":"))
                      su-binary
                      (shell-quote-argument
                       (or (executable-find "su")
                           (user-error
                            "No su binary is available in any of `exec-path'")))
                      command (format "PATH=%s exec %s - %s || exit"
                                      path su-binary user))
                ;; Attempt to execute the shell inside the global mount
                ;; namespace if requested.
                (when tramp-androidsu-mount-global-namespace
                  (progn
                    (when (eq tramp-androidsu-su-mm-supported 'unknown)
                      ;; Change the prompt in advance so that
                      ;; `tramp-adb-send-command-and-check' can call
                      ;; `tramp-search-regexp'.
	              (tramp-adb-send-command
		       vec (format "PS1=%s PS2=''"
			           (tramp-shell-quote-argument
                                    tramp-end-of-output)))
                      (setq tramp-androidsu-su-mm-supported
                            ;; Detect support for `su -mm'.
                            (tramp-adb-send-command-and-check
                             vec (format "%s -mm -c 'exit 24'" su-binary)
                             24)))
                    (when tramp-androidsu-su-mm-supported
                      (tramp-set-connection-property
                       vec "remote-namespace" t)
		      (setq command (format "PATH=%s exec %s -mm - %s || exit"
				            path su-binary user)))))
	        ;; Send the command.
		(tramp-message vec 3 "Sending command `%s'" command)
		(tramp-adb-send-command vec command t t)
		;; Android su binaries contact a background service to
		;; obtain authentication; during this process, input
		;; received is discarded, so input cannot be
		;; guaranteed to reach the root shell until its prompt
		;; is displayed.
		(with-current-buffer (process-buffer p)
		  (tramp-wait-for-regexp p tramp-connection-timeout
					 "#[[:space:]]*$"))
	        ;; Set connection-local variables.
	        (tramp-set-connection-local-variables vec)
	        ;; Change prompt.
	        (tramp-adb-send-command
		 vec (format "PS1=%s PS2=''"
			     (tramp-shell-quote-argument tramp-end-of-output)))
	        ;; Disable line editing.
	        (tramp-adb-send-command
	         vec "set +o vi +o vi-esccomplete +o vi-tabcomplete +o emacs")
                ;; Disable Unicode, for otherwise Unicode filenames will
                ;; not be decoded correctly.
                (tramp-adb-send-command vec "set +U")
	        ;; Dump option settings in the traces.
	        (when (>= tramp-verbose 9)
		  (tramp-adb-send-command vec "set -o"))
		;; Disable echo expansion.
		(tramp-adb-send-command
		 vec "stty -inlcr -onlcr -echo kill '^U' erase '^H'" t)
		;; Check whether the echo has really been disabled.
		;; Some implementations, like busybox, don't support
		;; disabling.
		(tramp-adb-send-command vec "echo foo" t)
		(with-current-buffer (process-buffer p)
		  (goto-char (point-min))
		  (when (looking-at-p "echo foo")
		    (tramp-set-connection-property p "remote-echo" t)
		    (tramp-message vec 5 "Remote echo still on. Ok.")
		    ;; Make sure backspaces and their echo are enabled
		    ;; and no line width magic interferes with them.
		    (tramp-adb-send-command
		     vec "stty icanon erase ^H cols 32767" t)))
		;; Mark it as connected.
		(tramp-set-connection-property p "connected" t))))
	;; Cleanup, and propagate the signal.
	((error quit)
	 (tramp-cleanup-connection vec t)
	 (signal (car err) (cdr err)))))))

(defun tramp-androidsu-generate-wrapper (function)
  "Return connection wrapper function for FUNCTION.
Return a function which temporarily substitutes local replacements for
the `adb' method's connection management functions around a call to
FUNCTION."
  (lambda (&rest args)
    (let ((tramp-adb-wait-for-output
           (symbol-function #'tramp-adb-wait-for-output))
          (tramp-adb-maybe-open-connection
           (symbol-function #'tramp-adb-maybe-open-connection)))
      (unwind-protect
          (progn
            ;; `tramp-adb-wait-for-output' addresses problems introduced
            ;; by the adb utility itself, not Android utilities, so
            ;; replace it with the regular Tramp function.
            (fset 'tramp-adb-wait-for-output #'tramp-wait-for-output)
            ;; Likewise, except some special treatment is necessary on
            ;; account of flaws in Android's su implementation.
            (fset 'tramp-adb-maybe-open-connection
                  #'tramp-androidsu-maybe-open-connection)
            (apply function args))
        ;; Restore the original definitions of the functions overridden
        ;; above.
        (fset 'tramp-adb-wait-for-output tramp-adb-wait-for-output)
        (fset 'tramp-adb-maybe-open-connection
              tramp-adb-maybe-open-connection)))))

(defalias 'tramp-androidsu-handle-copy-file #'tramp-sh-handle-copy-file)

(defalias 'tramp-androidsu-handle-delete-directory
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-delete-directory))

(defalias 'tramp-androidsu-handle-delete-file
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-delete-file))

(defalias 'tramp-androidsu-handle-directory-files-and-attributes
  (tramp-androidsu-generate-wrapper
   #'tramp-adb-handle-directory-files-and-attributes))

(defalias 'tramp-androidsu-handle-exec-path
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-exec-path))

(defalias 'tramp-androidsu-handle-file-attributes
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-attributes))

(defalias 'tramp-androidsu-handle-file-executable-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-executable-p))

(defalias 'tramp-androidsu-handle-file-exists-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-exists-p))

(defalias 'tramp-androidsu-handle-file-local-copy
  #'tramp-sh-handle-file-local-copy)

(defalias 'tramp-androidsu-handle-file-name-all-completions
  (tramp-androidsu-generate-wrapper
   #'tramp-adb-handle-file-name-all-completions))

(defalias 'tramp-androidsu-handle-file-readable-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-readable-p))

(defalias 'tramp-androidsu-handle-file-system-info
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-system-info))

(defalias 'tramp-androidsu-handle-file-writable-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-writable-p))

(defalias 'tramp-androidsu-handle-make-directory
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-make-directory))

(defun tramp-androidsu-handle-make-process (&rest args)
  "Like `tramp-handle-make-process', but modified for Android."
  (when args
    (with-parsed-tramp-file-name (expand-file-name default-directory) nil
      (let ((default-directory tramp-compat-temporary-file-directory)
	    (name (plist-get args :name))
	    (buffer (plist-get args :buffer))
	    (command (plist-get args :command))
	    (coding (plist-get args :coding))
	    (noquery (plist-get args :noquery))
	    (connection-type
	     (or (plist-get args :connection-type) process-connection-type))
	    (filter (plist-get args :filter))
	    (sentinel (plist-get args :sentinel))
	    (stderr (plist-get args :stderr)))
	(unless (stringp name)
	  (signal 'wrong-type-argument (list #'stringp name)))
	(unless (or (bufferp buffer) (string-or-null-p buffer))
	  (signal 'wrong-type-argument (list #'bufferp buffer)))
	(unless (consp command)
	  (signal 'wrong-type-argument (list #'consp command)))
	(unless (or (null coding)
		    (and (symbolp coding) (memq coding coding-system-list))
		    (and (consp coding)
			 (memq (car coding) coding-system-list)
			 (memq (cdr coding) coding-system-list)))
	  (signal 'wrong-type-argument (list #'symbolp coding)))
	(when (eq connection-type t)
	  (setq connection-type 'pty))
	(unless (or (and (consp connection-type)
			 (memq (car connection-type) '(nil pipe pty))
			 (memq (cdr connection-type) '(nil pipe pty)))
		    (memq connection-type '(nil pipe pty)))
	  (signal 'wrong-type-argument (list #'symbolp connection-type)))
	(unless (or (null filter) (eq filter t) (functionp filter))
	  (signal 'wrong-type-argument (list #'functionp filter)))
	(unless (or (null sentinel) (functionp sentinel))
	  (signal 'wrong-type-argument (list #'functionp sentinel)))
	(unless (or (null stderr) (bufferp stderr))
	  (signal 'wrong-type-argument (list #'bufferp stderr)))
	(let* ((buffer
		(if buffer
		    (get-buffer-create buffer)
		  ;; BUFFER can be nil.  We use a temporary buffer.
		  (generate-new-buffer tramp-temp-buffer-name)))
	       (orig-command command)
	       (env (mapcar
		     (lambda (elt)
		       (when (tramp-compat-string-search "=" elt) elt))
		     tramp-remote-process-environment))
	       ;; We use as environment the difference to toplevel
	       ;; `process-environment'.
	       (env (dolist (elt process-environment env)
		      (when
			  (and
			   (tramp-compat-string-search "=" elt)
			   (not
			    (member
			     elt (default-toplevel-value 'process-environment))))
			(setq env (cons elt env)))))
	       ;; Add remote path if exists.
	       (env (let ((remote-path
			   (string-join (tramp-get-remote-path v) ":")))
		      (setenv-internal env "PATH" remote-path 'keep)))
	       (env (setenv-internal
		     env "INSIDE_EMACS" (tramp-inside-emacs) 'keep))
	       (env (mapcar #'tramp-shell-quote-argument (delq nil env)))
	       ;; Quote command.
	       (command (mapconcat #'tramp-shell-quote-argument command " "))
	       ;; Set cwd and environment variables.
	       (command
	        (append
		 `("cd" ,(tramp-shell-quote-argument localname) "&&" "(" "env")
		 env `(,command ")")))
	       ;; Add remote shell if needed.
	       (command
		(if (consp (tramp-get-method-parameter v 'tramp-direct-async))
		    (append
		     (tramp-get-method-parameter v 'tramp-direct-async)
                     `(,(string-join command " ")))
		  command))
               p)
          ;; Generate a command to start the process using `su' with
          ;; suitable options for specifying the mount namespace and
          ;; suchlike.
	  (setq
	   p (let ((android-use-exec-loader nil))
               (make-process
	        :name name
                :buffer buffer
	        :command
                (if (equal user "root")
                    ;; Invoke su in the simplest manner possible, that
                    ;; is to say, without specifying the user, which
                    ;; certain implementations cannot parse when a
                    ;; command is also present, if it may be omitted, so
                    ;; that starting inferior shells on systems with
                    ;; such implementations does not needlessly fail.
                    (if (tramp-get-connection-property v "remote-namespace")
                        (append (list "su" "-mm" user "-c") command)
                      (append (list "su" "-c") command))
                  (if (tramp-get-connection-property v "remote-namespace")
                      (append (list "su" "-mm" "-" user "-c") command)
                    (append (list "su" "-" user "-c") command)))
	        :coding coding
                :noquery noquery
                :connection-type connection-type
	        :sentinel sentinel
                :stderr stderr)))
	  ;; Set filter.  Prior Emacs 29.1, it doesn't work reliably
	  ;; to provide it as `make-process' argument when filter is
	  ;; t.  See Bug#51177.
	  (when filter
	    (set-process-filter p filter))
	  (tramp-post-process-creation p v)
	  ;; Query flag is overwritten in `tramp-post-process-creation',
	  ;; so we reset it.
	  (set-process-query-on-exit-flag p (null noquery))
	  ;; This is needed for ssh or PuTTY based processes, and
	  ;; only if the respective options are set.  Perhaps, the
	  ;; setting could be more fine-grained.
	  ;; (process-put p 'tramp-shared-socket t)
	  (process-put p 'remote-command orig-command)
	  (tramp-set-connection-property p "remote-command" orig-command)
	  (when (bufferp stderr)
	    (tramp-taint-remote-process-buffer stderr))
	  p)))))

(defalias 'tramp-androidsu-handle-make-symbolic-link
  #'tramp-sh-handle-make-symbolic-link)

(defalias 'tramp-androidsu-handle-process-file
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-process-file))

(defalias 'tramp-androidsu-handle-rename-file #'tramp-sh-handle-rename-file)

(defalias 'tramp-androidsu-handle-set-file-modes
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-set-file-modes))

(defalias 'tramp-androidsu-handle-set-file-times
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-set-file-times))

(defalias 'tramp-androidsu-handle-get-remote-gid
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-get-remote-gid))

(defalias 'tramp-androidsu-handle-get-remote-groups
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-get-remote-groups))

(defalias 'tramp-androidsu-handle-get-remote-uid
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-get-remote-uid))

(defalias 'tramp-androidsu-handle-write-region #'tramp-sh-handle-write-region)

;;;###tramp-autoload
(defconst tramp-androidsu-file-name-handler-alist
  '(;; `abbreviate-file-name' performed by default handler.
    (access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-androidsu-handle-copy-file)
    (delete-directory . tramp-androidsu-handle-delete-directory)
    (delete-file . tramp-androidsu-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-androidsu-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-androidsu-handle-exec-path)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-androidsu-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-androidsu-handle-file-executable-p)
    (file-exists-p . tramp-androidsu-handle-file-exists-p)
    (file-group-gid . tramp-handle-file-group-gid)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-androidsu-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions
     . tramp-androidsu-handle-file-name-all-completions)
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
    (file-readable-p . tramp-androidsu-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-androidsu-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-user-uid . tramp-handle-file-user-uid)
    (file-writable-p . tramp-androidsu-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (list-system-processes . tramp-handle-list-system-processes)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-androidsu-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-androidsu-handle-make-process)
    (make-symbolic-link . tramp-androidsu-handle-make-symbolic-link)
    (memory-info . tramp-handle-memory-info)
    (process-attributes . tramp-handle-process-attributes)
    (process-file . tramp-androidsu-handle-process-file)
    (rename-file . tramp-androidsu-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . tramp-androidsu-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . tramp-androidsu-handle-set-file-times)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . ignore)
    (tramp-get-remote-gid . tramp-androidsu-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-androidsu-handle-get-remote-groups)
    (tramp-get-remote-uid . tramp-androidsu-handle-get-remote-uid)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-androidsu-handle-write-region))
  "Alist of Tramp handler functions for superuser sessions on Android.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-androidsu-file-name-p (vec-or-filename)
  "Check whether VEC-OR-FILENAME is for the `androidsu' method."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (equal (tramp-file-name-method vec) tramp-androidsu-method)))

;;;###tramp-autoload
(defun tramp-androidsu-file-name-handler (operation &rest args)
  "Invoke the `androidsu' handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let ((fn (assoc operation tramp-androidsu-file-name-handler-alist)))
      (prog1 (save-match-data (apply (cdr fn) args))
	(setq tramp-debug-message-fnh-function (cdr fn)))
    (prog1 (tramp-run-real-handler operation args)
      (setq tramp-debug-message-fnh-function operation))))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-androidsu-file-name-p #'tramp-androidsu-file-name-handler))

;;; Default connection-local variables for Tramp.

(defconst tramp-androidsu-connection-local-default-variables
  `((tramp-remote-path . ,tramp-androidsu-remote-path))
  "Default connection-local variables for remote androidsu connections.")

(connection-local-set-profile-variables
 'tramp-androidsu-connection-local-default-profile
 tramp-androidsu-connection-local-default-variables)

(connection-local-set-profiles
 `(:application tramp :protocol ,tramp-androidsu-method)
 'tramp-androidsu-connection-local-default-profile)

(with-eval-after-load 'shell
  (connection-local-set-profiles
   `(:application tramp :protocol ,tramp-androidsu-method)
   'tramp-adb-connection-local-default-shell-profile
   'tramp-adb-connection-local-default-ps-profile))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-androidsu 'force)))

(provide 'tramp-androidsu)
;;; tramp-androidsu.el ends here
