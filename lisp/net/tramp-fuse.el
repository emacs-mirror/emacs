;;; tramp-fuse.el --- Tramp access functions for FUSE mounts  -*- lexical-binding:t -*-

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

;; These are helper functions for FUSE file systems.

;;; Code:

(require 'tramp)

;; File name primitives.

(defun tramp-fuse-handle-delete-directory
    (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (tramp-flush-directory-properties v localname)
    (delete-directory (tramp-fuse-local-file-name directory) recursive trash)))

(defun tramp-fuse-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (delete-file (tramp-fuse-local-file-name filename) trash)
    (tramp-flush-file-properties v localname)))

(defun tramp-fuse-handle-directory-files
    (directory &optional full match nosort count)
  "Like `directory-files' for Tramp files."
  (unless (file-exists-p directory)
    (tramp-compat-file-missing (tramp-dissect-file-name directory) directory))
  (when (file-directory-p directory)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (with-parsed-tramp-file-name directory nil
      (let ((result
	     (tramp-compat-directory-files
	      (tramp-fuse-local-file-name directory) full match nosort count)))
	;; Massage the result.
	(when full
	  (let ((local (concat "^" (regexp-quote (tramp-fuse-mount-point v))))
		(remote (directory-file-name
			 (funcall
			  (if (tramp-compat-file-name-quoted-p directory)
			      #'tramp-compat-file-name-quote #'identity)
			  (file-remote-p directory)))))
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

(defun tramp-fuse-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
        v localname (format "file-attributes-%s" id-format)
      (file-attributes (tramp-fuse-local-file-name filename) id-format))))

(defun tramp-fuse-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      (file-executable-p (tramp-fuse-local-file-name filename)))))

(defun tramp-fuse-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (delete-dups
    (append
     (file-name-all-completions
      filename (tramp-fuse-local-file-name directory))
     ;; Some storage systems do not return "." and "..".
     (let (result)
       (dolist (item '(".." ".") result)
	 (when (string-prefix-p filename item)
	   (catch 'match
	     (dolist (elt completion-regexp-list)
	       (unless (string-match-p elt item) (throw 'match nil)))
	     (setq result (cons (concat item "/") result))))))))))

(defun tramp-fuse-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-readable-p"
      (file-readable-p (tramp-fuse-local-file-name filename)))))

;; This function isn't used.
(defun tramp-fuse-handle-insert-directory
    (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (insert-directory
   (tramp-fuse-local-file-name filename) switches wildcard full-directory-p)
  (goto-char (point-min))
  (while (search-forward (tramp-fuse-local-file-name filename) nil 'noerror)
    (replace-match filename)))

(defun tramp-fuse-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name dir) nil
    (make-directory (tramp-fuse-local-file-name dir) parents)
    ;; When PARENTS is non-nil, DIR could be a chain of non-existent
    ;; directories a/b/c/...  Instead of checking, we simply flush the
    ;; whole file cache.
    (tramp-flush-file-properties v localname)
    (tramp-flush-directory-properties
     v (if parents "/" (file-name-directory localname)))))


;; File name helper functions.

(defun tramp-fuse-mount-spec (vec)
  "Return local mount spec of VEC."
  (if-let ((host (tramp-file-name-host vec))
	   (user (tramp-file-name-user vec)))
      (format "%s@%s:/" user host)
    (format "%s:/" host)))

(defun tramp-fuse-mount-point (vec)
  "Return local mount point of VEC."
  (or (tramp-get-connection-property vec "mount-point" nil)
      (expand-file-name
       (concat
	tramp-temp-name-prefix
	(tramp-file-name-method vec) "."
	(when (tramp-file-name-user vec)
	  (concat (tramp-file-name-user-domain vec) "@"))
	(tramp-file-name-host-port vec))
       tramp-compat-temporary-file-directory)))

(defconst tramp-fuse-mount-timeout
  (eval (car (get 'remote-file-name-inhibit-cache 'standard-value)) t)
  "Time period to check whether the mount point still exists.
It has the same meaning as `remote-file-name-inhibit-cache'.")

(defun tramp-fuse-mounted-p (vec)
  "Check, whether fuse volume determined by VEC is mounted."
  ;; Remember the mount status by using a file property on "/",
  ;; instead of using a connection property, because a file property
  ;; has a timeout.  Having a timeout lets us regularly recheck the
  ;; mount status, as requested by `tramp-fuse-mount-timeout'.  We
  ;; cannot use `with-tramp-file-property', because we don't want to
  ;; cache a nil result.
  (let ((remote-file-name-inhibit-cache tramp-fuse-mount-timeout))
    (or (tramp-get-file-property vec "/" "mounted" nil)
        (let* ((default-directory tramp-compat-temporary-file-directory)
               (command (format "mount -t fuse.%s" (tramp-file-name-method vec)))
	       (mount (shell-command-to-string command)))
          (tramp-message vec 6 "%s\n%s" command mount)
          (tramp-set-file-property
	   vec "/" "mounted"
           (when (string-match
	          (format
                   "^\\(%s\\)\\s-" (regexp-quote (tramp-fuse-mount-spec vec)))
	          mount)
             (match-string 1 mount)))))))

(defun tramp-fuse-get-fusermount ()
  "Determine the local `fusermount' command."
  ;; We use key nil for local connection properties.
  (with-tramp-connection-property nil "fusermount"
    (or (executable-find "fusermount3")
	(executable-find "fusermount"))))

(defvar tramp-fuse-mount-points nil
  "List of fuse volume determined by a VEC.")

(defun tramp-fuse-unmount (vec)
  "Unmount fuse volume determined by VEC."
  (let* ((default-directory tramp-compat-temporary-file-directory)
	 (mount-point (tramp-fuse-mount-point vec))
         (command (format "%s -u %s" (tramp-fuse-get-fusermount) mount-point)))
    (tramp-message vec 6 "%s\n%s" command (shell-command-to-string command))
    (tramp-flush-file-property vec "/" "mounted")
    (setq tramp-fuse-mount-points
	  (delete (tramp-file-name-unify vec) tramp-fuse-mount-points))
    ;; Give the caches a chance to expire.
    (sleep-for 1)
    (when (tramp-compat-directory-empty-p mount-point)
      (delete-directory mount-point))))

(defun tramp-fuse-local-file-name (filename)
  "Return local mount name of FILENAME."
  (setq filename (tramp-compat-file-name-unquote (expand-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    ;; As long as we call `tramp-*-maybe-open-connection' here,
    ;; we cache the result.
    (with-tramp-file-property v localname "local-file-name"
      (funcall
       (intern
	(format "tramp-%s-maybe-open-connection" (tramp-file-name-method v)))
       v)
      (let ((quoted (tramp-compat-file-name-quoted-p localname))
	    (localname (tramp-compat-file-name-unquote localname)))
	(funcall
	 (if quoted #'tramp-compat-file-name-quote #'identity)
	 (expand-file-name
	  (if (file-name-absolute-p localname)
	      (substring localname 1) localname)
	  (tramp-fuse-mount-point v)))))))

(defcustom tramp-fuse-unmount-on-cleanup nil
  "Whether fuse volumes shall be unmounted on cleanup."
  :group 'tramp
  :version "28.1"
  :type 'boolean)

(defun tramp-fuse-cleanup (vec)
  "Cleanup fuse volume determined by VEC."
  (and tramp-fuse-unmount-on-cleanup
       (member (tramp-file-name-unify vec) tramp-fuse-mount-points)
       (tramp-fuse-unmount vec)))

(defun tramp-fuse-cleanup-all ()
  "Unmount all fuse volumes used by Tramp."
  (and tramp-fuse-unmount-on-cleanup
       (mapc #'tramp-fuse-unmount tramp-fuse-mount-points)))

;; Add cleanup hooks.
(add-hook 'tramp-cleanup-connection-hook #'tramp-fuse-cleanup)
(add-hook 'tramp-cleanup-all-connections-hook #'tramp-fuse-cleanup-all)
(add-hook 'kill-emacs-hook #'tramp-fuse-cleanup-all)
(add-hook 'tramp-fuse-unload-hook
	  (lambda ()
	    (remove-hook 'tramp-cleanup-connection-hook
			 #'tramp-fuse-cleanup)
	    (remove-hook 'tramp-cleanup-all-connections-hook
			 #'tramp-fuse-cleanup-all)
	    (remove-hook 'kill-emacs-hook
			 #'tramp-fuse-cleanup-all)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-fuse 'force)))

(provide 'tramp-fuse)

;;; tramp-fuse.el ends here
