;;; tramp-rclone.el --- Tramp access functions to cloud storages  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

;; rclone is a command line program to sync files and directories to
;; and from cloud storages.  Tramp uses its mount utility to access
;; files and directories there.  The configuration of rclone for
;; different storage systems is performed outside Tramp, see rclone(1).

;; A remote file under rclone control has the form
;; "/rclone:<remote>:/path/to/file".  <remote> is the name of a
;; storage system in rclone's configuration.  Therefore, such a remote
;; file name does not know of any user or port specification.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'tramp)

;;;###tramp-autoload
(defconst tramp-rclone-method "rclone"
  "When this method name is used, forward all calls to rclone mounts.")

;;;###tramp-autoload
(defcustom tramp-rclone-program "rclone"
  "Name of the rclone program."
  :group 'tramp
  :version "27.1"
  :type 'string)

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
	      `(,tramp-rclone-method
		(tramp-mount-args nil)
		(tramp-copyto-args nil)
		(tramp-moveto-args nil)
		(tramp-about-args ("--full"))))

 (add-to-list 'tramp-default-host-alist `(,tramp-rclone-method nil ""))

 (tramp-set-completion-function
  tramp-rclone-method '((tramp-rclone-parse-device-names ""))))


;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-rclone-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-rclone-handle-copy-file)
    (delete-directory . tramp-rclone-handle-delete-directory)
    (delete-file . tramp-rclone-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-rclone-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . ignore)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-rclone-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-rclone-handle-file-executable-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-rclone-handle-file-name-all-completions)
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
    (file-readable-p . tramp-rclone-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-rclone-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . tramp-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-rclone-handle-make-directory)
    (make-directory-internal . ignore)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (process-file . ignore)
    (rename-file . tramp-rclone-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . ignore)
    (start-file-process . ignore)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-handle-write-region))
  "Alist of handler functions for Tramp RCLONE method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-rclone-file-name-p (filename)
  "Check if it's a FILENAME for rclone."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
		tramp-rclone-method)))

;;;###tramp-autoload
(defun tramp-rclone-file-name-handler (operation &rest args)
  "Invoke the rclone handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-rclone-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-rclone-file-name-p #'tramp-rclone-file-name-handler))

;;;###tramp-autoload
(defun tramp-rclone-parse-device-names (_ignore)
  "Return a list of (nil host) tuples allowed to access."
  (with-tramp-connection-property nil "rclone-device-names"
    (delq nil
	  (mapcar
	   (lambda (line)
	     (when (string-match "^\\(\\S-+\\):$" line)
	       `(nil ,(match-string 1 line))))
	   (tramp-process-lines nil tramp-rclone-program "listremotes")))))


;; File name primitives.

(defun tramp-rclone-do-copy-or-rename-file
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
PRESERVE-EXTENDED-ATTRIBUTES is ignored.

This function is invoked by `tramp-rclone-handle-copy-file' and
`tramp-rclone-handle-rename-file'.  It is an error if OP is neither
of `copy' and `rename'.  FILENAME and NEWNAME must be absolute
file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  (setq filename (file-truename filename))
  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname))
	  (rclone-operation (if (eq op 'copy) "copyto" "moveto"))
	  (msg-operation (if (eq op 'copy) "Copying" "Renaming")))

      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(unless (file-exists-p filename)
	  (tramp-error
	   v tramp-file-missing
	   "%s file" msg-operation "No such file or directory" filename))
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))
	(when (and (file-directory-p newname)
		   (not (tramp-compat-directory-name-p newname)))
	  (tramp-error v 'file-error "File is a directory %s" newname))

	(if (or (and t1 (not (tramp-rclone-file-name-p filename)))
		(and t2 (not (tramp-rclone-file-name-p newname))))

	    ;; We cannot copy or rename directly.
	    (let ((tmpfile (tramp-compat-make-temp-file filename)))
	      (if (eq op 'copy)
		  (copy-file
		   filename tmpfile t keep-date preserve-uid-gid
		   preserve-extended-attributes)
		(rename-file filename tmpfile t))
	      (rename-file tmpfile newname ok-if-already-exists))

	  ;; Direct action.
	  (with-tramp-progress-reporter
	      v 0 (format "%s %s to %s" msg-operation filename newname)
	    (unless (zerop
		     (tramp-rclone-send-command
		      v rclone-operation
		      (tramp-rclone-remote-file-name filename)
		      (tramp-rclone-remote-file-name newname)))
	      (tramp-error
	       v 'file-error
	       "Error %s `%s' `%s'" msg-operation filename newname)))

	  (when (and t1 (eq op 'rename))
	    (with-parsed-tramp-file-name filename v1
	      (tramp-flush-file-properties v1 v1-localname)
	      (when (tramp-rclone-file-name-p filename)
		(tramp-rclone-flush-directory-cache v1)
		;; The mount point's directory cache might need time
		;; to flush.
		(while (file-exists-p filename)
		  (tramp-flush-file-properties v1 v1-localname)))))

	  (when t2
	    (with-parsed-tramp-file-name newname v2
	      (tramp-flush-file-properties v2 v2-localname)
	      (when (tramp-rclone-file-name-p newname)
		(tramp-rclone-flush-directory-cache v2)
		;; The mount point's directory cache might need time
		;; to flush.
		(while (not (file-exists-p newname))
		  (tramp-flush-file-properties v2 v2-localname))))))))))

(defun tramp-rclone-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
	  (tramp-tramp-file-p newname))
      (tramp-rclone-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date
       preserve-uid-gid preserve-extended-attributes)
    (tramp-run-real-handler
     #'copy-file
     (list filename newname ok-if-already-exists keep-date
	   preserve-uid-gid preserve-extended-attributes))))

(defun tramp-rclone-handle-delete-directory
    (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (delete-directory (tramp-rclone-local-file-name directory) recursive trash)
    (tramp-flush-directory-properties v localname)
    (tramp-rclone-flush-directory-cache v)))

(defun tramp-rclone-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (delete-file (tramp-rclone-local-file-name filename) trash)
    (tramp-flush-file-properties v localname)
    (tramp-rclone-flush-directory-cache v)))

(defun tramp-rclone-handle-directory-files
    (directory &optional full match nosort)
  "Like `directory-files' for Tramp files."
  (unless (file-exists-p directory)
    (tramp-error
     (tramp-dissect-file-name directory) tramp-file-missing
     "No such file or directory" directory))
  (when (file-directory-p directory)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (with-parsed-tramp-file-name directory nil
      (let ((result
	     (directory-files
	      (tramp-rclone-local-file-name directory) full match)))
	;; Massage the result.
	(when full
	  (let ((local (concat "^" (regexp-quote (tramp-rclone-mount-point v))))
		(remote (funcall (if (tramp-compat-file-name-quoted-p directory)
				     #'tramp-compat-file-name-quote #'identity)
				 (file-remote-p directory))))
	    (setq result
		  (mapcar
		   (lambda (x) (replace-regexp-in-string local remote x))
		   result))))
	;; Some storage systems do not return "." and "..".
	(dolist (item '(".." "."))
	  (when (and (string-match-p (or match (regexp-quote item)) item)
		     (not
		      (member (if full (setq item (concat directory item)) item)
			      result)))
	    (setq result (cons item result))))
	;; Return result.
	(if nosort result (sort result #'string<))))))

(defun tramp-rclone-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
	v localname (format "file-attributes-%s" id-format)
      (file-attributes (tramp-rclone-local-file-name filename) id-format))))

(defun tramp-rclone-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      (file-executable-p (tramp-rclone-local-file-name filename)))))

(defun tramp-rclone-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (delete-dups
    (append
     (file-name-all-completions
      filename (tramp-rclone-local-file-name directory))
     ;; Some storage systems do not return "." and "..".
     (let (result)
       (dolist (item '(".." ".") result)
	 (when (string-prefix-p filename item)
	   (catch 'match
	     (dolist (elt completion-regexp-list)
	       (unless (string-match-p elt item) (throw 'match nil)))
	     (setq result (cons (concat item "/") result))))))))))

(defun tramp-rclone-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-readable-p"
      (file-readable-p (tramp-rclone-local-file-name filename)))))

(defun tramp-rclone-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  (ignore-errors
    (unless (file-directory-p filename)
      (setq filename (file-name-directory filename)))
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (tramp-message v 5 "file system info: %s" localname)
      (tramp-rclone-send-command v "about" (concat host ":"))
      (with-current-buffer (tramp-get-connection-buffer v)
	(let (total used free)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when (looking-at "Total: [[:space:]]+\\([[:digit:]]+\\)")
	      (setq total (string-to-number (match-string 1))))
	    (when (looking-at "Used: [[:space:]]+\\([[:digit:]]+\\)")
	      (setq used (string-to-number (match-string 1))))
	    (when (looking-at "Free: [[:space:]]+\\([[:digit:]]+\\)")
	      (setq free (string-to-number (match-string 1))))
	    (forward-line))
	  (when used
	    ;; The used number of bytes is not part of the result.  As
	    ;; side effect, we store it as file property.
	    (tramp-set-file-property v localname "used-bytes" used))
	  ;; Result.
	  (when (and total free)
	    (list total free (- total free))))))))

(defun tramp-rclone-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (insert-directory
   (tramp-rclone-local-file-name filename) switches wildcard full-directory-p)
  (goto-char (point-min))
  (while (search-forward (tramp-rclone-local-file-name filename) nil 'noerror)
    (replace-match filename)))

(defun tramp-rclone-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (let ((result
	 (insert-file-contents
	  (tramp-rclone-local-file-name filename) visit beg end replace)))
    (prog1
	(list (expand-file-name filename) (cadr result))
      (when visit (setq buffer-file-name filename)))))

(defun tramp-rclone-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name dir) nil
    (make-directory (tramp-rclone-local-file-name dir) parents)
    ;; When PARENTS is non-nil, DIR could be a chain of non-existent
    ;; directories a/b/c/...  Instead of checking, we simply flush the
    ;; whole file cache.
    (tramp-flush-file-properties v localname)
    (tramp-flush-directory-properties
     v (if parents "/" (file-name-directory localname)))
    (tramp-rclone-flush-directory-cache v)))

(defun tramp-rclone-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-rclone-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists
       'keep-date 'preserve-uid-gid)
    (tramp-run-real-handler
     #'rename-file (list filename newname ok-if-already-exists))))


;; File name conversions.

(defun tramp-rclone-mount-point (vec)
  "Return local mount point of VEC."
  (expand-file-name
   (concat
    tramp-temp-name-prefix (tramp-file-name-method vec)
    "." (tramp-file-name-host vec))
   (tramp-compat-temporary-file-directory)))

(defun tramp-rclone-mounted-p (vec)
  "Check, whether storage system determined by VEC is mounted."
  (when (tramp-get-connection-process vec)
    ;; We cannot use `with-connection-property', because we don't want
    ;; to cache a nil result.
    (or (tramp-get-connection-property
	 (tramp-get-connection-process vec) "mounted" nil)
	(let* ((default-directory temporary-file-directory)
	       (mount (shell-command-to-string "mount -t fuse.rclone")))
	  (tramp-message vec 6 "%s" "mount -t fuse.rclone")
	  (tramp-message vec 6 "\n%s" mount)
	  (tramp-set-connection-property
	   (tramp-get-connection-process vec) "mounted"
	   (when (string-match
		  (format
		   "^\\(%s:\\S-*\\)" (regexp-quote (tramp-file-name-host vec)))
		  mount)
	     (match-string 1 mount)))))))

(defun tramp-rclone-flush-directory-cache (vec)
  "Flush directory cache of VEC mount."
  (let ((rclone-pid
	 ;; Identify rclone process.
	 (when (tramp-get-connection-process vec)
	   (with-tramp-connection-property
	       (tramp-get-connection-process vec) "rclone-pid"
	     (catch 'pid
	       (dolist (pid (list-system-processes)) ;; "pidof rclone" ?
		 (and (string-match-p
		       (regexp-quote
			(format "rclone mount %s:" (tramp-file-name-host vec)))
		       (or (cdr (assoc 'args (process-attributes pid))) ""))
		      (throw 'pid pid))))))))
    ;; Send a SIGHUP in order to flush directory cache.
    (when rclone-pid
      (tramp-message
       vec 6 "Send SIGHUP %d: %s"
       rclone-pid (cdr (assoc 'args (process-attributes rclone-pid))))
      (signal-process rclone-pid 'SIGHUP))))

(defun tramp-rclone-local-file-name (filename)
  "Return local mount name of FILENAME."
  (setq filename (tramp-compat-file-name-unquote (expand-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    ;; As long as we call `tramp-rclone-maybe-open-connection' here,
    ;; we cache the result.
    (with-tramp-file-property v localname "local-file-name"
      (tramp-rclone-maybe-open-connection v)
      (let ((quoted (tramp-compat-file-name-quoted-p localname))
	    (localname (tramp-compat-file-name-unquote localname)))
	(funcall
	 (if quoted #'tramp-compat-file-name-quote #'identity)
	 (expand-file-name
	  (if (file-name-absolute-p localname)
	      (substring localname 1) localname)
	  (tramp-rclone-mount-point v)))))))

(defun tramp-rclone-remote-file-name (filename)
  "Return FILENAME as used in the `rclone' command."
  (setq filename (tramp-compat-file-name-unquote (expand-file-name filename)))
  (if (tramp-rclone-file-name-p filename)
      (with-parsed-tramp-file-name filename nil
	;; As long as we call `tramp-rclone-maybe-open-connection' here,
	;; we cache the result.
	(with-tramp-file-property v localname "remote-file-name"
	  (tramp-rclone-maybe-open-connection v)
	  ;; TODO: This shall be handled by `expand-file-name'.
	  (setq localname
		(replace-regexp-in-string "^\\." "" (or localname "")))
	  (format "%s%s" (tramp-rclone-mounted-p v) localname)))
    ;; It is a local file name.
    filename))

(defun tramp-rclone-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  (let ((host (tramp-file-name-host vec)))
    (when (rassoc `(,host) (tramp-rclone-parse-device-names nil))
      (if (zerop (length host))
	  (tramp-error vec 'file-error "Storage %s not connected" host))
      ;; We need a process bound to the connection buffer.  Therefore,
      ;; we create a dummy process.  Maybe there is a better solution?
      (unless (get-buffer-process (tramp-get-connection-buffer vec))
	(let ((p (make-network-process
		  :name (tramp-get-connection-name vec)
		  :buffer (tramp-get-connection-buffer vec)
		  :server t :host 'local :service t :noquery t)))
	  (process-put p 'vector vec)
	  (set-process-query-on-exit-flag p nil)

	  ;; Set connection-local variables.
	  (tramp-set-connection-local-variables vec)))

      ;; Create directory.
      (unless (file-directory-p (tramp-rclone-mount-point vec))
	(make-directory (tramp-rclone-mount-point vec) 'parents))

      ;; Mount.  This command does not return, so we use 0 as
      ;; DESTINATION of `tramp-call-process'.
      (unless (tramp-rclone-mounted-p vec)
	(apply
	 #'tramp-call-process
	 vec tramp-rclone-program nil 0 nil
	 (delq nil
	       `("mount" ,(concat host ":/")
		 ,(tramp-rclone-mount-point vec)
		 ;; This could be nil.
		 ,(tramp-get-method-parameter vec 'tramp-mount-args))))
	(while (not (file-exists-p (tramp-make-tramp-file-name vec 'localname)))
	  (tramp-cleanup-connection vec 'keep-debug 'keep-password))

	;; Mark it as connected.
	(tramp-set-connection-property
	 (tramp-get-connection-process vec) "connected" t))))

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

(defun tramp-rclone-send-command (vec &rest args)
  "Send a command to connection VEC.
The command is the list of strings ARGS."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (erase-buffer)
    (let ((flags (tramp-get-method-parameter
		  vec (intern (format "tramp-%s-args" (car args))))))
      (apply #'tramp-call-process
	     vec tramp-rclone-program nil t nil (append args flags)))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-rclone 'force)))

(provide 'tramp-rclone)

;;; TODO:

;; * If possible, get rid of "rclone mount".  Maybe it is more
;;   performant then.

;;; tramp-rclone.el ends here
