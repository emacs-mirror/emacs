;;; tramp-androidsu.el --- TRAMP method for Android superuser shells  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

;; The `su' method struggles (as do other shell-based methods) with the
;; crippled versions of many Unix utilities installed on Android,
;; workarounds for which are implemented in the `adb' method.  This
;; method defines a shell-based method that is identical in function to
;; `su', but reuses such code from the `adb' method where applicable and
;; also provides for certain mannerisms of popular Android `su'
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
On systems that assign each application a unique view of the filesystem
by executing them within individual mount namespaces and thus conceal
each application's data directories from others, invoke `su' with the
option `-mm' in order for the shell launched to run within the global
mount namespace, so that TRAMP may edit files belonging to any and all
applications."
  :group 'tramp
  :version "30.1"
  :type 'boolean)

(defvar tramp-androidsu-su-mm-supported 'unknown
  "Whether `su -mm' is supported on this system.")

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
	      `(,tramp-androidsu-method
                (tramp-login-program        "su")
                (tramp-login-args           (("-") ("%u")))
                (tramp-remote-shell         "/system/bin/sh")
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-tmpdir               "/data/local/tmp")
                (tramp-connection-timeout   10)))

 (add-to-list 'tramp-default-host-alist
              `(,tramp-androidsu-method nil "localhost")))

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
                                       ;; tramp-encoding-shell, as
                                       ;; there's no guarantee that it's
                                       ;; possible to execute it with
                                       ;; `android-use-exec-loader' off.
			               "/system/bin/sh" "-i"))
		     (user (tramp-file-name-user vec))
                     command)
                ;; Set sentinel.  Initialize variables.
	        (set-process-sentinel p #'tramp-process-sentinel)
	        (tramp-post-process-creation p vec)

                ;; Replace `login-args' place holders.
		(setq command (format "exec su - %s || exit"
				      (or user "root")))

                ;; Attempt to execute the shell inside the global mount
                ;; namespace if requested.
                (when tramp-androidsu-mount-global-namespace
                  (progn
                    (when (eq tramp-androidsu-su-mm-supported 'unknown)
                      ;; Change the prompt in advance so that
                      ;; tramp-adb-send-command-and-check can call
                      ;; tramp-search-regexp.
	              (tramp-adb-send-command
		       vec (format "PS1=%s"
			           (tramp-shell-quote-argument
                                    tramp-end-of-output)))
                      (setq tramp-androidsu-su-mm-supported
                            ;; Detect support for `su -mm'.
                            (tramp-adb-send-command-and-check
                             vec "su -mm -c 'exit 24'" 24)))
                    (when tramp-androidsu-su-mm-supported
		      (setq command (format "exec su -mm - %s || exit"
				            (or user "root"))))))
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
		 vec (format "PS1=%s"
			     (tramp-shell-quote-argument tramp-end-of-output)))

	        ;; Disable line editing.
	        (tramp-adb-send-command
	         vec "set +o vi +o vi-esccomplete +o vi-tabcomplete +o emacs")

	        ;; Dump option settings in the traces.
	        (when (>= tramp-verbose 9)
		  (tramp-adb-send-command vec "set -o"))

                ;; Disable Unicode.
                (tramp-adb-send-command vec "set +U")

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
		    (tramp-adb-send-command vec
					    "stty icanon erase ^H cols 32767"
					    t)))

		;; Set the remote PATH to a suitable value.
		(tramp-set-connection-property vec "remote-path"
					       "/system/bin:/system/xbin")

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
            ;; tramp-adb-wait-for-output addresses problems introduced
            ;; by the adb utility itself, not Android utilities, so
            ;; replace it with the regular TRAMP function.
            (fset 'tramp-adb-wait-for-output #'tramp-wait-for-output)
            ;; Likewise, except some special treatment is necessary on
            ;; account of flaws in Android's su implementation.
            (fset 'tramp-adb-maybe-open-connection
                  #'tramp-androidsu-maybe-open-connection)
            (apply function args))
        ;; Restore the original definitions of the functions overridden
        ;; above.
        (fset 'tramp-adb-wait-for-output tramp-adb-wait-for-output)
        (fset 'tramp-adb-maybe-open-connection tramp-adb-maybe-open-connection)))))

(defalias 'tramp-androidsu-handle-access-file
  (tramp-androidsu-generate-wrapper #'tramp-handle-access-file))

(defalias 'tramp-androidsu-handle-add-name-to-file
  (tramp-androidsu-generate-wrapper #'tramp-handle-add-name-to-file))

(defalias 'tramp-androidsu-handle-copy-directory
  (tramp-androidsu-generate-wrapper #'tramp-handle-copy-directory))

(defalias 'tramp-androidsu-sh-handle-copy-file
  (tramp-androidsu-generate-wrapper #'tramp-sh-handle-copy-file))

(defalias 'tramp-androidsu-adb-handle-delete-directory
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-delete-directory))

(defalias 'tramp-androidsu-adb-handle-delete-file
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-delete-file))

(defalias 'tramp-androidsu-handle-directory-file-name
  (tramp-androidsu-generate-wrapper #'tramp-handle-directory-file-name))

(defalias 'tramp-androidsu-handle-directory-files
  (tramp-androidsu-generate-wrapper #'tramp-handle-directory-files))

(defalias 'tramp-androidsu-adb-handle-directory-files-and-attributes
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-directory-files-and-attributes))

(defalias 'tramp-androidsu-handle-dired-uncache
  (tramp-androidsu-generate-wrapper #'tramp-handle-dired-uncache))

(defalias 'tramp-androidsu-adb-handle-exec-path
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-exec-path))

(defalias 'tramp-androidsu-handle-expand-file-name
  (tramp-androidsu-generate-wrapper #'tramp-handle-expand-file-name))

(defalias 'tramp-androidsu-handle-file-accessible-directory-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-accessible-directory-p))

(defalias 'tramp-androidsu-adb-handle-file-attributes
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-attributes))

(defalias 'tramp-androidsu-handle-file-directory-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-directory-p))

(defalias 'tramp-androidsu-handle-file-equal-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-equal-p))

(defalias 'tramp-androidsu-adb-handle-file-executable-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-executable-p))

(defalias 'tramp-androidsu-adb-handle-file-exists-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-exists-p))

(defalias 'tramp-androidsu-handle-file-group-gid
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-group-gid))

(defalias 'tramp-androidsu-handle-file-in-directory-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-in-directory-p))

(defalias 'tramp-androidsu-sh-handle-file-local-copy
  (tramp-androidsu-generate-wrapper #'tramp-sh-handle-file-local-copy))

(defalias 'tramp-androidsu-handle-file-locked-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-locked-p))

(defalias 'tramp-androidsu-handle-file-modes
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-modes))

(defalias 'tramp-androidsu-adb-handle-file-name-all-completions
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-name-all-completions))

(defalias 'tramp-androidsu-handle-file-name-as-directory
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-name-as-directory))

(defalias 'tramp-androidsu-handle-file-name-case-insensitive-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-name-case-insensitive-p))

(defalias 'tramp-androidsu-handle-file-name-completion
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-name-completion))

(defalias 'tramp-androidsu-handle-file-name-directory
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-name-directory))

(defalias 'tramp-androidsu-handle-file-name-nondirectory
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-name-nondirectory))

(defalias 'tramp-androidsu-handle-file-newer-than-file-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-newer-than-file-p))

(defalias 'tramp-androidsu-handle-file-notify-add-watch
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-notify-add-watch))

(defalias 'tramp-androidsu-handle-file-notify-rm-watch
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-notify-rm-watch))

(defalias 'tramp-androidsu-handle-file-notify-valid-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-notify-valid-p))

(defalias 'tramp-androidsu-adb-handle-file-readable-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-readable-p))

(defalias 'tramp-androidsu-handle-file-regular-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-regular-p))

(defalias 'tramp-androidsu-handle-file-remote-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-remote-p))

(defalias 'tramp-androidsu-handle-file-selinux-context
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-selinux-context))

(defalias 'tramp-androidsu-handle-file-symlink-p
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-symlink-p))

(defalias 'tramp-androidsu-adb-handle-file-system-info
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-system-info))

(defalias 'tramp-androidsu-handle-file-truename
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-truename))

(defalias 'tramp-androidsu-handle-file-user-uid
  (tramp-androidsu-generate-wrapper #'tramp-handle-file-user-uid))

(defalias 'tramp-androidsu-adb-handle-file-writable-p
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-file-writable-p))

(defalias 'tramp-androidsu-handle-find-backup-file-name
  (tramp-androidsu-generate-wrapper #'tramp-handle-find-backup-file-name))

(defalias 'tramp-androidsu-handle-insert-directory
  (tramp-androidsu-generate-wrapper #'tramp-handle-insert-directory))

(defalias 'tramp-androidsu-handle-insert-file-contents
  (tramp-androidsu-generate-wrapper #'tramp-handle-insert-file-contents))

(defalias 'tramp-androidsu-handle-list-system-processes
  (tramp-androidsu-generate-wrapper #'tramp-handle-list-system-processes))

(defalias 'tramp-androidsu-handle-load
  (tramp-androidsu-generate-wrapper #'tramp-handle-load))

(defalias 'tramp-androidsu-handle-lock-file
  (tramp-androidsu-generate-wrapper #'tramp-handle-lock-file))

(defalias 'tramp-androidsu-handle-make-auto-save-file-name
  (tramp-androidsu-generate-wrapper #'tramp-handle-make-auto-save-file-name))

(defalias 'tramp-androidsu-adb-handle-make-directory
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-make-directory))

(defalias 'tramp-androidsu-handle-make-lock-file-name
  (tramp-androidsu-generate-wrapper #'tramp-handle-make-lock-file-name))

(defalias 'tramp-androidsu-handle-make-nearby-temp-file
  (tramp-androidsu-generate-wrapper #'tramp-handle-make-nearby-temp-file))

(defalias 'tramp-androidsu-adb-handle-make-process
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-make-process))

(defalias 'tramp-androidsu-sh-handle-make-symbolic-link
  (tramp-androidsu-generate-wrapper
   #'tramp-sh-handle-make-symbolic-link))

(defalias 'tramp-androidsu-handle-memory-info
  (tramp-androidsu-generate-wrapper #'tramp-handle-memory-info))

(defalias 'tramp-androidsu-handle-process-attributes
  (tramp-androidsu-generate-wrapper #'tramp-handle-process-attributes))

(defalias 'tramp-androidsu-adb-handle-process-file
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-process-file))

(defalias 'tramp-androidsu-sh-handle-rename-file
  (tramp-androidsu-generate-wrapper #'tramp-sh-handle-rename-file))

(defalias 'tramp-androidsu-adb-handle-set-file-modes
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-set-file-modes))

(defalias 'tramp-androidsu-adb-handle-set-file-times
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-set-file-times))

(defalias 'tramp-androidsu-handle-set-visited-file-modtime
  (tramp-androidsu-generate-wrapper #'tramp-handle-set-visited-file-modtime))

(defalias 'tramp-androidsu-handle-shell-command
  (tramp-androidsu-generate-wrapper #'tramp-handle-shell-command))

(defalias 'tramp-androidsu-handle-start-file-process
  (tramp-androidsu-generate-wrapper #'tramp-handle-start-file-process))

(defalias 'tramp-androidsu-handle-substitute-in-file-name
  (tramp-androidsu-generate-wrapper #'tramp-handle-substitute-in-file-name))

(defalias 'tramp-androidsu-handle-temporary-file-directory
  (tramp-androidsu-generate-wrapper #'tramp-handle-temporary-file-directory))

(defalias 'tramp-androidsu-adb-handle-get-remote-gid
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-get-remote-gid))

(defalias 'tramp-androidsu-adb-handle-get-remote-groups
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-get-remote-groups))

(defalias 'tramp-androidsu-adb-handle-get-remote-uid
  (tramp-androidsu-generate-wrapper #'tramp-adb-handle-get-remote-uid))

(defalias 'tramp-androidsu-handle-unlock-file
  (tramp-androidsu-generate-wrapper #'tramp-handle-unlock-file))

(defalias 'tramp-androidsu-handle-verify-visited-file-modtime
  (tramp-androidsu-generate-wrapper #'tramp-handle-verify-visited-file-modtime))

(defalias 'tramp-androidsu-sh-handle-write-region
  (tramp-androidsu-generate-wrapper #'tramp-sh-handle-write-region))

;;;###tramp-autoload
(defconst tramp-androidsu-file-name-handler-alist
  '(;; `abbreviate-file-name' performed by default handler.
    (access-file . tramp-androidsu-handle-access-file)
    (add-name-to-file . tramp-androidsu-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-androidsu-handle-copy-directory)
    (copy-file . tramp-androidsu-sh-handle-copy-file)
    (delete-directory . tramp-androidsu-adb-handle-delete-directory)
    (delete-file . tramp-androidsu-adb-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-androidsu-handle-directory-file-name)
    (directory-files . tramp-androidsu-handle-directory-files)
    (directory-files-and-attributes
     . tramp-androidsu-adb-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-androidsu-handle-dired-uncache)
    (exec-path . tramp-androidsu-adb-handle-exec-path)
    (expand-file-name . tramp-androidsu-handle-expand-file-name)
    (file-accessible-directory-p . tramp-androidsu-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-androidsu-adb-handle-file-attributes)
    (file-directory-p . tramp-androidsu-handle-file-directory-p)
    (file-equal-p . tramp-androidsu-handle-file-equal-p)
    (file-executable-p . tramp-androidsu-adb-handle-file-executable-p)
    (file-exists-p . tramp-androidsu-adb-handle-file-exists-p)
    (file-group-gid . tramp-androidsu-handle-file-group-gid)
    (file-in-directory-p . tramp-androidsu-handle-file-in-directory-p)
    (file-local-copy . tramp-androidsu-sh-handle-file-local-copy)
    (file-locked-p . tramp-androidsu-handle-file-locked-p)
    (file-modes . tramp-androidsu-handle-file-modes)
    (file-name-all-completions . tramp-androidsu-adb-handle-file-name-all-completions)
    (file-name-as-directory . tramp-androidsu-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-androidsu-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-androidsu-handle-file-name-completion)
    (file-name-directory . tramp-androidsu-handle-file-name-directory)
    (file-name-nondirectory . tramp-androidsu-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-androidsu-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-androidsu-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-androidsu-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-androidsu-handle-file-notify-valid-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-androidsu-adb-handle-file-readable-p)
    (file-regular-p . tramp-androidsu-handle-file-regular-p)
    (file-remote-p . tramp-androidsu-handle-file-remote-p)
    (file-selinux-context . tramp-androidsu-handle-file-selinux-context)
    (file-symlink-p . tramp-androidsu-handle-file-symlink-p)
    (file-system-info . tramp-androidsu-adb-handle-file-system-info)
    (file-truename . tramp-androidsu-handle-file-truename)
    (file-user-uid . tramp-androidsu-handle-file-user-uid)
    (file-writable-p . tramp-androidsu-adb-handle-file-writable-p)
    (find-backup-file-name . tramp-androidsu-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-androidsu-handle-insert-directory)
    (insert-file-contents . tramp-androidsu-handle-insert-file-contents)
    (list-system-processes . tramp-androidsu-handle-list-system-processes)
    (load . tramp-androidsu-handle-load)
    (lock-file . tramp-androidsu-handle-lock-file)
    (make-auto-save-file-name . tramp-androidsu-handle-make-auto-save-file-name)
    (make-directory . tramp-androidsu-adb-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-androidsu-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-androidsu-handle-make-nearby-temp-file)
    (make-process . tramp-androidsu-adb-handle-make-process)
    (make-symbolic-link . tramp-androidsu-sh-handle-make-symbolic-link)
    (memory-info . tramp-androidsu-handle-memory-info)
    (process-attributes . tramp-androidsu-handle-process-attributes)
    (process-file . tramp-androidsu-adb-handle-process-file)
    (rename-file . tramp-androidsu-sh-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . tramp-androidsu-adb-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . tramp-androidsu-adb-handle-set-file-times)
    (set-visited-file-modtime . tramp-androidsu-handle-set-visited-file-modtime)
    (shell-command . tramp-androidsu-handle-shell-command)
    (start-file-process . tramp-androidsu-handle-start-file-process)
    (substitute-in-file-name . tramp-androidsu-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-androidsu-handle-temporary-file-directory)
    (tramp-get-home-directory . ignore)
    (tramp-get-remote-gid . tramp-androidsu-adb-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-androidsu-adb-handle-get-remote-groups)
    (tramp-get-remote-uid . tramp-androidsu-adb-handle-get-remote-uid)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-androidsu-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-androidsu-handle-verify-visited-file-modtime)
    (write-region . tramp-androidsu-sh-handle-write-region))
  "Alist of TRAMP handler functions for superuser sessions on Android.")

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

(connection-local-set-profile-variables
 'tramp-adb-connection-local-default-ps-profile
 tramp-adb-connection-local-default-ps-variables)

(with-eval-after-load 'shell
  (connection-local-set-profiles
   `(:application tramp :protocol ,tramp-adb-method)
   'tramp-adb-connection-local-default-shell-profile
   'tramp-adb-connection-local-default-ps-profile))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-androidsu 'force)))

(provide 'tramp-androidsu)
;;; tramp-androidsu.el ends here
