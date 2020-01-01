;;; tramp-adb.el --- Functions for calling Android Debug Bridge from Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Jürgen Hötzel <juergen@archlinux.org>
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

;; The Android Debug Bridge "adb" must be installed on your local
;; machine.  If it is not in your $PATH, add the following form into
;; your .emacs:
;;
;;   (setq tramp-adb-program "/path/to/adb")
;;
;; Due to security it is not possible to access non-root devices.

;;; Code:

(require 'tramp)

;;;###tramp-autoload
(defcustom tramp-adb-program "adb"
  "Name of the Android Debug Bridge program."
  :group 'tramp
  :version "24.4"
  :type 'string)

;;;###tramp-autoload
(defcustom tramp-adb-connect-if-not-connected nil
  "Try to run `adb connect' if provided device is not connected currently.
It is used for TCP/IP devices."
  :group 'tramp
  :version "25.1"
  :type 'boolean)

;;;###tramp-autoload
(defconst tramp-adb-method "adb"
  "When this method name is used, forward all calls to Android Debug Bridge.")

;;;###tramp-autoload
(defcustom tramp-adb-prompt
  "^[[:digit:]]*|?[[:alnum:]\e;[]*@?[[:alnum:]]*[^#\\$]*[#\\$][[:space:]]"
  "Regexp used as prompt in almquist shell."
  :type 'regexp
  :version "24.4"
  :group 'tramp)

(defconst tramp-adb-ls-date-regexp
  "[[:space:]][0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9][[:space:]][0-9][0-9]:[0-9][0-9][[:space:]]"
  "Regexp for date format in ls output.")

(defconst tramp-adb-ls-toolbox-regexp
  (concat
   "^[[:space:]]*\\([-.[:alpha:]]+\\)"	; \1 permissions
   "\\(?:[[:space:]]+[[:digit:]]+\\)?"	; links (Android 7/toybox)
   "[[:space:]]*\\([^[:space:]]+\\)"	; \2 username
   "[[:space:]]+\\([^[:space:]]+\\)"	; \3 group
   "[[:space:]]+\\([[:digit:]]+\\)"	; \4 size
   "[[:space:]]+\\([-[:digit:]]+[[:space:]][:[:digit:]]+\\)" ; \5 date
   "[[:space:]]\\(.*\\)$")		; \6 filename
  "Regexp for ls output.")

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
	      `(,tramp-adb-method
	        (tramp-tmpdir            "/data/local/tmp")
                (tramp-default-port      5555)))

 (add-to-list 'tramp-default-host-alist `(,tramp-adb-method nil ""))

 (tramp-set-completion-function
  tramp-adb-method '((tramp-adb-parse-device-names ""))))

;;;###tramp-autoload
(defconst tramp-adb-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-adb-handle-copy-file)
    (delete-directory . tramp-adb-handle-delete-directory)
    (delete-file . tramp-adb-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-adb-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-adb-handle-exec-path)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-adb-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    ;; FIXME: This is too sloppy.
    (file-executable-p . tramp-handle-file-exists-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-adb-handle-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-adb-handle-file-name-all-completions)
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
    (file-system-info . tramp-adb-handle-file-system-info)
    (file-truename . tramp-adb-handle-file-truename)
    (file-writable-p . tramp-adb-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-adb-handle-make-directory)
    (make-directory-internal . ignore)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-adb-handle-make-process)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (process-file . tramp-adb-handle-process-file)
    (rename-file . tramp-adb-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . tramp-adb-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . tramp-adb-handle-set-file-times)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-adb-handle-write-region))
  "Alist of handler functions for Tramp ADB method.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-adb-file-name-p (filename)
  "Check if it's a FILENAME for ADB."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
		tramp-adb-method)))

;;;###tramp-autoload
(defun tramp-adb-file-name-handler (operation &rest arguments)
  "Invoke the ADB handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of
ARGUMENTS to pass to the OPERATION."
  (let ((fn (assoc operation tramp-adb-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) arguments))
      (tramp-run-real-handler operation arguments))))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-adb-file-name-p #'tramp-adb-file-name-handler))

;;;###tramp-autoload
(defun tramp-adb-parse-device-names (_ignore)
  "Return a list of (nil host) tuples allowed to access."
  (delq nil
	(mapcar
	 (lambda (line)
	   (when (string-match "^\\(\\S-+\\)[[:space:]]+device$" line)
	     ;; Replace ":" by "#".
	     `(nil ,(replace-regexp-in-string
		     ":" tramp-prefix-port-format (match-string 1 line)))))
	 (tramp-process-lines nil tramp-adb-program "devices"))))

(defun tramp-adb-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  (ignore-errors
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (tramp-message v 5 "file system info: %s" localname)
      (tramp-adb-send-command
       v (format "df -k %s" (tramp-shell-quote-argument localname)))
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(forward-line)
	(when (looking-at
	       (eval-when-compile
		 (concat "[[:space:]]*[^[:space:]]+"
			 "[[:space:]]+\\([[:digit:]]+\\)"
			 "[[:space:]]+\\([[:digit:]]+\\)"
			 "[[:space:]]+\\([[:digit:]]+\\)")))
	  ;; The values are given as 1k numbers, so we must change
	  ;; them to number of bytes.
	  (list (* 1024 (string-to-number (match-string 1)))
		;; The second value is the used size.  We need the
		;; free size.
		(* 1024 (- (string-to-number (match-string 1))
			   (string-to-number (match-string 2))))
		(* 1024 (string-to-number (match-string 3)))))))))

;; This is derived from `tramp-sh-handle-file-truename'.  Maybe the
;; code could be shared?
(defun tramp-adb-handle-file-truename (filename)
  "Like `file-truename' for Tramp files."
  ;; Preserve trailing "/".
  (funcall
   (if (tramp-compat-directory-name-p filename)
       #'file-name-as-directory #'identity)
   ;; Quote properly.
   (funcall
    (if (tramp-compat-file-name-quoted-p filename)
	#'tramp-compat-file-name-quote #'identity)
    (with-parsed-tramp-file-name
	(tramp-compat-file-name-unquote (expand-file-name filename)) nil
      (tramp-make-tramp-file-name
       v
       (with-tramp-file-property v localname "file-truename"
	 (let (result)			; result steps in reverse order
	   (tramp-message v 4 "Finding true name for `%s'" filename)
	   (let* ((steps (split-string localname "/" 'omit))
		  (localnamedir (tramp-run-real-handler
				 'file-name-as-directory (list localname)))
		  (is-dir (string= localname localnamedir))
		  (thisstep nil)
		  (numchase 0)
		  ;; Don't make the following value larger than
		  ;; necessary.  People expect an error message in a
		  ;; timely fashion when something is wrong; otherwise
		  ;; they might think that Emacs is hung.  Of course,
		  ;; correctness has to come first.
		  (numchase-limit 20)
		  symlink-target)
	     (while (and steps (< numchase numchase-limit))
	       (setq thisstep (pop steps))
	       (tramp-message
		v 5 "Check %s"
		(string-join
		 (append '("") (reverse result) (list thisstep)) "/"))
	       (setq symlink-target
		     (tramp-compat-file-attribute-type
		      (file-attributes
		       (tramp-make-tramp-file-name
			v
			(string-join
			 (append
			  '("") (reverse result) (list thisstep)) "/")))))
	       (cond ((string= "." thisstep)
		      (tramp-message v 5 "Ignoring step `.'"))
		     ((string= ".." thisstep)
		      (tramp-message v 5 "Processing step `..'")
		      (pop result))
		     ((stringp symlink-target)
		      ;; It's a symlink, follow it.
		      (tramp-message v 5 "Follow symlink to %s" symlink-target)
		      (setq numchase (1+ numchase))
		      (when (file-name-absolute-p symlink-target)
			(setq result nil))
		      ;; If the symlink was absolute, we'll get a string
		      ;; like "/user@host:/some/target"; extract the
		      ;; "/some/target" part from it.
		      (when (tramp-tramp-file-p symlink-target)
			(unless (tramp-equal-remote filename symlink-target)
			  (tramp-error
			   v 'file-error
			   "Symlink target `%s' on wrong host" symlink-target))
			(setq symlink-target localname))
		      (setq steps
			    (append (split-string symlink-target "/" 'omit)
				    steps)))
		     (t
		      ;; It's a file.
		      (setq result (cons thisstep result)))))
	     (when (>= numchase numchase-limit)
	       (tramp-error
		v 'file-error
		"Maximum number (%d) of symlinks exceeded" numchase-limit))
	     (setq result (reverse result))
	     ;; Combine list to form string.
	     (setq result
		   (if result
		       (string-join (cons "" result) "/")
		     "/"))
	     (when (and is-dir (or (string-empty-p result)
				   (not (string= (substring result -1) "/"))))
	       (setq result (concat result "/"))))

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
	   result)))))))

(defun tramp-adb-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    (with-parsed-tramp-file-name filename nil
      (with-tramp-file-property
	  v localname (format "file-attributes-%s" id-format)
	(and
	 (tramp-adb-send-command-and-check
	  v (format "%s -d -l %s"
		    (tramp-adb-get-ls-command v)
		    (tramp-shell-quote-argument localname)))
	 (with-current-buffer (tramp-get-buffer v)
	   (tramp-adb-sh-fix-ls-output)
	   (cdar (tramp-do-parse-file-attributes-with-ls v id-format))))))))

(defun tramp-do-parse-file-attributes-with-ls (vec &optional id-format)
  "Parse `file-attributes' for Tramp files using the ls(1) command."
  (with-current-buffer (tramp-get-buffer vec)
    (goto-char (point-min))
    (let ((file-properties nil))
      (while (re-search-forward tramp-adb-ls-toolbox-regexp nil t)
	(let* ((mod-string (match-string 1))
	       (is-dir (eq ?d (aref mod-string 0)))
	       (is-symlink (eq ?l (aref mod-string 0)))
	       (uid (match-string 2))
	       (gid (match-string 3))
	       (size (string-to-number (match-string 4)))
	       (date (match-string 5))
	       (name (match-string 6))
	       (symlink-target
		(and is-symlink
		     (cadr (split-string name "\\( -> \\|\n\\)")))))
	  (push (list
		 (if is-symlink
		     (car (split-string name "\\( -> \\|\n\\)"))
		   name)
		 (or is-dir symlink-target)
		 1     ;link-count
		 ;; no way to handle numeric ids in Androids ash
		 (if (eq id-format 'integer) 0 uid)
		 (if (eq id-format 'integer) 0 gid)
		 tramp-time-dont-know   ; atime
		 (date-to-time date)	; mtime
		 tramp-time-dont-know   ; ctime
		 size
		 mod-string
		 ;; fake
		 t 1
		 (tramp-get-device vec))
		file-properties)))
      file-properties)))

(defun tramp-adb-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for Tramp files."
  (unless (file-exists-p directory)
    (tramp-error
     (tramp-dissect-file-name directory) tramp-file-missing
     "No such file or directory" directory))
  (when (file-directory-p directory)
    (with-parsed-tramp-file-name (expand-file-name directory) nil
      (copy-tree
       (with-tramp-file-property
	   v localname (format "directory-files-and-attributes-%s-%s-%s-%s"
			       full match id-format nosort)
	 (with-current-buffer (tramp-get-buffer v)
	   (when (tramp-adb-send-command-and-check
		  v (format "%s -a -l %s"
			    (tramp-adb-get-ls-command v)
			    (tramp-shell-quote-argument localname)))
	     ;; We insert also filename/. and filename/.., because "ls" doesn't.
	     ;; Looks like it does include them in toybox, since Android 6.
	     (unless (re-search-backward "\\.$" nil t)
	       (narrow-to-region (point-max) (point-max))
	       (tramp-adb-send-command
		v (format "%s -d -a -l %s %s"
			  (tramp-adb-get-ls-command v)
			  (tramp-shell-quote-argument
			   (concat (file-name-as-directory localname) "."))
			  (tramp-shell-quote-argument
			   (concat (file-name-as-directory localname) ".."))))
	       (widen)))
	   (tramp-adb-sh-fix-ls-output)
	   (let ((result (tramp-do-parse-file-attributes-with-ls
			  v (or id-format 'integer))))
	     (when full
	       (setq result
		     (mapcar
		      (lambda (x)
			(cons (expand-file-name (car x) directory) (cdr x)))
		      result)))
	     (unless nosort
	       (setq result
		     (sort result (lambda (x y) (string< (car x) (car y))))))
	     (delq nil
		   (mapcar (lambda (x)
			     (if (or (not match) (string-match-p match (car x)))
				 x))
			   result)))))))))

(defun tramp-adb-get-ls-command (vec)
  "Determine `ls' command and its arguments."
  (with-tramp-connection-property vec "ls"
    (tramp-message vec 5 "Finding a suitable `ls' command")
    (cond
     ;; Support Android derived systems where "ls" command is provided
     ;; by GNU Coreutils. Force "ls" to print one column and set
     ;; time-style to imitate other "ls" flavors.
     ((tramp-adb-send-command-and-check
       vec "ls --time-style=long-iso /dev/null")
      "ls -1 --time-style=long-iso")
     ;; Can't disable coloring explicitly for toybox ls command.  We
     ;; also must force "ls" to print just one column.
     ((tramp-adb-send-command-and-check vec "toybox") "ls -1")
     ;; On CyanogenMod based system BusyBox is used and "ls" output
     ;; coloring is enabled by default.  So we try to disable it when
     ;; possible.
     ((tramp-adb-send-command-and-check vec "ls --color=never -al /dev/null")
      "ls --color=never")
     (t "ls"))))

(defun tramp-adb--gnu-switches-to-ash (switches)
  "Almquist shell can't handle multiple arguments.
Convert (\"-al\") to (\"-a\" \"-l\").  Remove arguments like \"--dired\"."
  (split-string
   (apply #'concat
	  (mapcar (lambda (s)
		    (replace-regexp-in-string
		     "\\(.\\)" " -\\1" (replace-regexp-in-string "^-" "" s)))
		  ;; FIXME: Warning about removed switches (long and non-dash).
		  (delq nil
			(mapcar
			 (lambda (s)
			   (and (not (string-match-p "\\(^--\\|^[^-]\\)" s)) s))
			 switches))))))

(defun tramp-adb-sh-fix-ls-output (&optional sort-by-time)
  "Insert dummy 0 in empty size columns.
Android's \"ls\" command doesn't insert size column for directories:
Emacs dired can't find files."
  (save-excursion
    ;; Insert missing size.
    (goto-char (point-min))
    (while
	(search-forward-regexp
	 "[[:space:]]\\([[:space:]][0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9][[:space:]]\\)" nil t)
      (replace-match "0\\1" "\\1" nil)
      ;; Insert missing "/".
      (when (looking-at-p "[0-9][0-9]:[0-9][0-9][[:space:]]+$")
	(end-of-line)
	(insert "/")))
    ;; Sort entries.
    (let* ((lines (split-string (buffer-string) "\n" t))
	   (sorted-lines
	    (sort
	     lines
	     (if sort-by-time
		 #'tramp-adb-ls-output-time-less-p
	       #'tramp-adb-ls-output-name-less-p))))
      (delete-region (point-min) (point-max))
      (insert "  " (string-join sorted-lines "\n  ")))
    ;; Add final newline.
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))))

(defun tramp-adb-ls-output-time-less-p (a b)
  "Sort \"ls\" output by time, descending."
  (let (time-a time-b)
    (string-match tramp-adb-ls-date-regexp a)
    (setq time-a (apply #'encode-time (parse-time-string (match-string 0 a))))
    (string-match tramp-adb-ls-date-regexp b)
    (setq time-b (apply #'encode-time (parse-time-string (match-string 0 b))))
    (time-less-p time-b time-a)))

(defun tramp-adb-ls-output-name-less-p (a b)
  "Sort \"ls\" output by name, ascending."
  (if (string-match directory-listing-before-filename-regexp a)
      (let ((posa (match-end 0)))
	(if (string-match directory-listing-before-filename-regexp b)
	    (let ((posb (match-end 0)))
	      (string-lessp (substring a posa) (substring b posb)))))))

(defun tramp-adb-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (when (and (null parents) (file-exists-p dir))
      (tramp-error v 'file-already-exists "Directory already exists %s" dir))
    (when parents
      (let ((par (expand-file-name ".." dir)))
	(unless (file-directory-p par)
	  (make-directory par parents))))
    (tramp-flush-directory-properties v localname)
    (unless (or (tramp-adb-send-command-and-check
		 v (format "mkdir %s" (tramp-shell-quote-argument localname)))
		(and parents (file-directory-p dir)))
      (tramp-error v 'file-error "Couldn't make directory %s" dir))))

(defun tramp-adb-handle-delete-directory (directory &optional recursive _trash)
  "Like `delete-directory' for Tramp files."
  (setq directory (expand-file-name directory))
  (with-parsed-tramp-file-name (file-truename directory) nil
    (tramp-flush-directory-properties v localname))
  (with-parsed-tramp-file-name directory nil
    (tramp-flush-directory-properties v localname)
    (tramp-adb-barf-unless-okay
     v (format "%s %s"
	       (if recursive "rm -r" "rmdir")
	       (tramp-shell-quote-argument localname))
     "Couldn't delete %s" directory)))

(defun tramp-adb-handle-delete-file (filename &optional _trash)
  "Like `delete-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v localname)
    (tramp-adb-barf-unless-okay
     v (format "rm %s" (tramp-shell-quote-argument localname))
     "Couldn't delete %s" filename)))

(defun tramp-adb-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name (expand-file-name directory) nil
     (with-tramp-file-property v localname "file-name-all-completions"
       (tramp-adb-send-command
	v (format "%s -a %s"
		  (tramp-adb-get-ls-command v)
		  (tramp-shell-quote-argument localname)))
       (mapcar
	(lambda (f)
	  (if (file-directory-p (expand-file-name f directory))
	      (file-name-as-directory f)
	    f))
	(with-current-buffer (tramp-get-buffer v)
	  (delete-dups
	   (append
	    ;; In older Android versions, "." and ".." are not
	    ;; included.  In newer versions (toybox, since Android 6)
	    ;; they are.  We fix this by `delete-dups'.
	    '("." "..")
	    (delq
	     nil
	     (mapcar
	      (lambda (l) (and (not (string-match-p "^[[:space:]]*$" l)) l))
	      (split-string (buffer-string) "\n")))))))))))

(defun tramp-adb-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (unless (file-exists-p (file-truename filename))
      (tramp-error
       v tramp-file-missing
       "Cannot make local copy of non-existing file `%s'" filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (with-tramp-progress-reporter
	  v 3 (format "Fetching %s to tmp file %s" filename tmpfile)
	;; "adb pull ..." does not always return an error code.
	(when (or (tramp-adb-execute-adb-command
		   v "pull" (tramp-compat-file-name-unquote localname) tmpfile)
		  (not (file-exists-p tmpfile)))
	  (ignore-errors (delete-file tmpfile))
	  (tramp-error
	   v 'file-error "Cannot make local copy of file `%s'" filename))
	(set-file-modes tmpfile (logior (or (file-modes filename) 0) #o0400)))
      tmpfile)))

(defun tramp-adb-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files.
But handle the case, if the \"test\" command is not available."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (tramp-adb-find-test-command v)
	  (if (file-exists-p filename)
	      (tramp-adb-send-command-and-check
	       v (format "test -w %s" (tramp-shell-quote-argument localname)))
	    (and
	     (file-directory-p (file-name-directory filename))
	     (file-writable-p (file-name-directory filename))))

	;; Missing "test" command on Android < 4.
       (let ((rw-path "/data/data"))
	 (tramp-message
	  v 5
	  "Not implemented yet (assuming \"/data/data\" is writable): %s"
	  localname)
	 (and (>= (length localname) (length rw-path))
	      (string= (substring localname 0 (length rw-path))
		       rw-path)))))))

(defun tramp-adb-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (when (and mustbenew (file-exists-p filename)
	       (or (eq mustbenew 'excl)
		   (not
		    (y-or-n-p
		     (format "File %s exists; overwrite anyway? " filename)))))
      (tramp-error v 'file-already-exists filename))

    ;; We must also flush the cache of the directory, because
    ;; `file-attributes' reads the values from there.
    (tramp-flush-file-properties v localname)
    (let* ((curbuf (current-buffer))
	   (tmpfile (tramp-compat-make-temp-file filename)))
      (when (and append (file-exists-p filename))
	(copy-file filename tmpfile 'ok)
	(set-file-modes tmpfile (logior (or (file-modes tmpfile) 0) #o0600)))
      (tramp-run-real-handler
       #'write-region (list start end tmpfile append 'no-message lockname))
      (with-tramp-progress-reporter
        v 3 (format-message
             "Moving tmp file `%s' to `%s'" tmpfile filename)
	(unwind-protect
	    (when (tramp-adb-execute-adb-command
		   v "push" tmpfile (tramp-compat-file-name-unquote localname))
	      (tramp-error v 'file-error "Cannot write: `%s'" filename))
	  (delete-file tmpfile)))

      (unless (equal curbuf (current-buffer))
	(tramp-error
	 v 'file-error
	 "Buffer has changed from `%s' to `%s'" curbuf (current-buffer)))

      ;; Set file modification time.
      (when (or (eq visit t) (stringp visit))
	(set-visited-file-modtime
	 (tramp-compat-file-attribute-modification-time
	  (file-attributes filename))))

      ;; The end.
      (when (and (null noninteractive)
		 (or (eq visit t) (null visit) (stringp visit)))
	(tramp-message v 0 "Wrote %s" filename))
      (run-hooks 'tramp-handle-write-region-hook))))

(defun tramp-adb-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v localname)
    (tramp-adb-send-command-and-check v (format "chmod %o %s" mode localname))))

(defun tramp-adb-handle-set-file-times (filename &optional time)
  "Like `set-file-times' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v localname)
    (let ((time (if (or (null time)
			(tramp-compat-time-equal-p time tramp-time-doesnt-exist)
			(tramp-compat-time-equal-p time tramp-time-dont-know))
		    (current-time)
		  time))
	  (quoted-name (tramp-shell-quote-argument localname)))
      ;; Older versions of toybox 'touch' mishandle nanoseconds and/or
      ;; trailing "Z", so fall back on plain seconds if nanoseconds+Z
      ;; fails.  Also, fall back on old POSIX 'touch -t' if 'touch -d'
      ;; (introduced in POSIX.1-2008) fails.
      (tramp-adb-send-command-and-check
       v (format (concat "touch -d %s %s 2>/dev/null || "
			 "touch -d %s %s 2>/dev/null || "
			 "touch -t %s %s")
		 (format-time-string "%Y-%m-%dT%H:%M:%S.%NZ" time t)
		 quoted-name
		 (format-time-string "%Y-%m-%dT%H:%M:%S" time t)
		 quoted-name
		 (format-time-string "%Y%m%d%H%M.%S" time t)
		 quoted-name)))))

(defun tramp-adb-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   _preserve-uid-gid _preserve-extended-attributes)
  "Like `copy-file' for Tramp files.
PRESERVE-UID-GID and PRESERVE-EXTENDED-ATTRIBUTES are completely ignored."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (if (file-directory-p filename)
      (copy-directory filename newname keep-date t)

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname)))
      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(unless (file-exists-p filename)
	  (tramp-error
	   v tramp-file-missing
	   "Copying file" "No such file or directory" filename))
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))
	(when (and (file-directory-p newname)
		   (not (tramp-compat-directory-name-p newname)))
	  (tramp-error v 'file-error "File is a directory %s" newname))

	(with-tramp-progress-reporter
	    v 0 (format "Copying %s to %s" filename newname)
	  (if (and t1 t2 (tramp-equal-remote filename newname))
	      (let ((l1 (tramp-compat-file-local-name filename))
		    (l2 (tramp-compat-file-local-name newname)))
		;; We must also flush the cache of the directory,
		;; because `file-attributes' reads the values from
		;; there.
		(tramp-flush-file-properties v l2)
		;; Short track.
		(tramp-adb-barf-unless-okay
		 v (format
		    "cp -f %s %s"
		    (tramp-shell-quote-argument l1)
		    (tramp-shell-quote-argument l2))
		 "Error copying %s to %s" filename newname))

	    (let ((tmpfile (file-local-copy filename)))

	      (if tmpfile
		  ;; Remote filename.
		  (condition-case err
		      (rename-file tmpfile newname ok-if-already-exists)
		    ((error quit)
		     (delete-file tmpfile)
		     (signal (car err) (cdr err))))

		;; Remote newname.
		(when (and (file-directory-p newname)
			   (tramp-compat-directory-name-p newname))
		  (setq newname
			(expand-file-name
			 (file-name-nondirectory filename) newname)))

		(with-parsed-tramp-file-name newname nil
		  (when (and (not ok-if-already-exists)
			     (file-exists-p newname))
		    (tramp-error v 'file-already-exists newname))

		  ;; We must also flush the cache of the directory,
		  ;; because `file-attributes' reads the values from
		  ;; there.
		  (tramp-flush-file-properties v localname)
		  (when (tramp-adb-execute-adb-command
			 v "push"
			 (tramp-compat-file-name-unquote filename)
			 (tramp-compat-file-name-unquote localname))
		    (tramp-error
		     v 'file-error
		     "Cannot copy `%s' `%s'" filename newname)))))))))

    ;; KEEP-DATE handling.
    (when keep-date
      (set-file-times
       newname
       (tramp-compat-file-attribute-modification-time
	(file-attributes filename))))))

(defun tramp-adb-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname t t)
	(delete-directory filename 'recursive))

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname)))
      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(unless (file-exists-p filename)
	  (tramp-error
	   v tramp-file-missing
	   "Renaming file" "No such file or directory" filename))
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))
	(when (and (file-directory-p newname)
		   (not (tramp-compat-directory-name-p newname)))
	  (tramp-error v 'file-error "File is a directory %s" newname))

	(with-tramp-progress-reporter
	    v 0 (format "Renaming %s to %s" filename newname)
	  (if (and t1 t2
		   (tramp-equal-remote filename newname)
		   (not (file-directory-p filename)))
	      (let ((l1 (tramp-compat-file-local-name filename))
		    (l2 (tramp-compat-file-local-name newname)))
		;; We must also flush the cache of the directory, because
		;; `file-attributes' reads the values from there.
		(tramp-flush-file-properties v l1)
		(tramp-flush-file-properties v l2)
		;; Short track.
		(tramp-adb-barf-unless-okay
		 v (format
		    "mv -f %s %s"
		    (tramp-shell-quote-argument l1)
		    (tramp-shell-quote-argument l2))
		 "Error renaming %s to %s" filename newname))

	    ;; Rename by copy.
	    (copy-file
	     filename newname ok-if-already-exists 'keep-time 'preserve-uid-gid)
	    (delete-file filename)))))))

(defun tramp-adb-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name default-directory nil
    (let (command input tmpinput stderr tmpstderr outbuf ret)
      ;; Compute command.
      (setq command (mapconcat #'tramp-shell-quote-argument
			       (cons program args) " "))
      ;; Determine input.
      (if (null infile)
	  (setq input "/dev/null")
	(setq infile (expand-file-name infile))
	(if (tramp-equal-remote default-directory infile)
	    ;; INFILE is on the same remote host.
	    (setq input (with-parsed-tramp-file-name infile nil localname))
	  ;; INFILE must be copied to remote host.
	  (setq input (tramp-make-tramp-temp-file v)
		tmpinput (tramp-make-tramp-file-name v input))
	  (copy-file infile tmpinput t)))
      (when input (setq command (format "%s <%s" command input)))

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
	(cond
	 ((stringp (cadr destination))
	  (setcar (cdr destination) (expand-file-name (cadr destination)))
	  (if (tramp-equal-remote default-directory (cadr destination))
	      ;; stderr is on the same remote host.
	      (setq stderr (with-parsed-tramp-file-name
			       (cadr destination) nil localname))
	    ;; stderr must be copied to remote host.  The temporary
	    ;; file must be deleted after execution.
	    (setq stderr (tramp-make-tramp-temp-file v)
		  tmpstderr (tramp-make-tramp-file-name v stderr))))
	 ;; stderr to be discarded.
	 ((null (cadr destination))
	  (setq stderr "/dev/null"))))
       ;; 't
       (destination
	(setq outbuf (current-buffer))))
      (when stderr (setq command (format "%s 2>%s" command stderr)))

      ;; Send the command.  It might not return in time, so we protect
      ;; it.  Call it in a subshell, in order to preserve working
      ;; directory.
      (condition-case nil
	  (progn
	    (setq ret
		  (if (tramp-adb-send-command-and-check
		       v
		       (format "(cd %s; %s)"
			       (tramp-shell-quote-argument localname) command))
		      ;; Set return status accordingly.
		      0 1))
	    ;; We should add the output anyway.
	    (when outbuf
	      (with-current-buffer outbuf
		(insert-buffer-substring (tramp-get-connection-buffer v)))
	      (when (and display (get-buffer-window outbuf t)) (redisplay))))
	;; When the user did interrupt, we should do it also.  We use
	;; return code -1 as marker.
	(quit
	 (kill-buffer (tramp-get-connection-buffer v))
	 (setq ret -1))
	;; Handle errors.
	(error
	 (kill-buffer (tramp-get-connection-buffer v))
	 (setq ret 1)))

      ;; Provide error file.
      (when tmpstderr (rename-file tmpstderr (cadr destination) t))

      ;; Cleanup.  We remove all file cache values for the connection,
      ;; because the remote process could have changed them.
      (when tmpinput (delete-file tmpinput))

      (unless process-file-side-effects
        (tramp-flush-directory-properties v ""))

      ;; Return exit status.
      (if (equal ret -1)
	  (keyboard-quit)
	ret))))

;; We use BUFFER also as connection buffer during setup.  Because of
;; this, its original contents must be saved, and restored once
;; connection has been setup.
(defun tramp-adb-handle-make-process (&rest args)
  "Like `make-process' for Tramp files."
  (when args
    (with-parsed-tramp-file-name (expand-file-name default-directory) nil
      (let ((name (plist-get args :name))
	    (buffer (plist-get args :buffer))
	    (command (plist-get args :command))
	    (coding (plist-get args :coding))
	    (noquery (plist-get args :noquery))
	    (connection-type (plist-get args :connection-type))
	    (filter (plist-get args :filter))
	    (sentinel (plist-get args :sentinel))
	    (stderr (plist-get args :stderr)))
	(unless (stringp name)
	  (signal 'wrong-type-argument (list #'stringp name)))
	(unless (or (null buffer) (bufferp buffer) (stringp buffer))
	  (signal 'wrong-type-argument (list #'stringp buffer)))
	(unless (consp command)
	  (signal 'wrong-type-argument (list #'consp command)))
	(unless (or (null coding)
		    (and (symbolp coding) (memq coding coding-system-list))
		    (and (consp coding)
			 (memq (car coding) coding-system-list)
			 (memq (cdr coding) coding-system-list)))
	  (signal 'wrong-type-argument (list #'symbolp coding)))
	(unless (or (null connection-type) (memq connection-type '(pipe pty)))
	  (signal 'wrong-type-argument (list #'symbolp connection-type)))
	(unless (or (null filter) (functionp filter))
	  (signal 'wrong-type-argument (list #'functionp filter)))
	(unless (or (null sentinel) (functionp sentinel))
	  (signal 'wrong-type-argument (list #'functionp sentinel)))
	(unless (or (null stderr) (bufferp stderr) (stringp stderr))
	  (signal 'wrong-type-argument (list #'stringp stderr)))

	(let* ((buffer
		(if buffer
		    (get-buffer-create buffer)
		  ;; BUFFER can be nil.  We use a temporary buffer.
		  (generate-new-buffer tramp-temp-buffer-name)))
	       (program (car command))
	       (args (cdr command))
	       (command
		(format "cd %s && exec %s"
			(tramp-shell-quote-argument localname)
			(mapconcat #'tramp-shell-quote-argument
				   (cons program args) " ")))
	       (tramp-process-connection-type
		(or (null program) tramp-process-connection-type))
	       (bmp (and (buffer-live-p buffer) (buffer-modified-p buffer)))
	       (name1 name)
	       (i 0))

	  (while (get-process name1)
	    ;; NAME must be unique as process name.
	    (setq i (1+ i)
		  name1 (format "%s<%d>" name i)))
	  (setq name name1)
	  ;; Set the new process properties.
	  (tramp-set-connection-property v "process-name" name)
	  (tramp-set-connection-property v "process-buffer" buffer)

	  (with-current-buffer (tramp-get-connection-buffer v)
	    (unwind-protect
		;; We catch this event.  Otherwise, `make-process'
		;; could be called on the local host.
		(save-excursion
		  (save-restriction
		    ;; Activate narrowing in order to save BUFFER
		    ;; contents.  Clear also the modification time;
		    ;; otherwise we might be interrupted by
		    ;; `verify-visited-file-modtime'.
		    (let ((buffer-undo-list t)
			  (inhibit-read-only t))
		      (clear-visited-file-modtime)
		      (narrow-to-region (point-max) (point-max))
		      ;; We call `tramp-adb-maybe-open-connection', in
		      ;; order to cleanup the prompt afterwards.
		      (tramp-adb-maybe-open-connection v)
		      (delete-region (point-min) (point-max))
		      ;; Send the command.
		      (let* ((p (tramp-get-connection-process v)))
                        (tramp-adb-send-command v command nil t) ; nooutput
			;; Set sentinel and filter.
			(when sentinel
			  (set-process-sentinel p sentinel))
			(when filter
			  (set-process-filter p filter))
			;; Set query flag and process marker for this
			;; process.  We ignore errors, because the
			;; process could have finished already.
			(ignore-errors
			  (set-process-query-on-exit-flag p (null noquery))
			  (set-marker (process-mark p) (point)))
			;; Read initial output.  Remove the first line,
			;; which is the command echo.
			(while
			    (progn
			      (goto-char (point-min))
			      (not (re-search-forward "[\n]" nil t)))
			  (tramp-accept-process-output p 0))
			(delete-region (point-min) (point))
			;; Return process.
			p))))

	      ;; Save exit.
	      (if (string-match-p tramp-temp-buffer-name (buffer-name))
		  (ignore-errors
		    (set-process-buffer (tramp-get-connection-process v) nil)
		    (kill-buffer (current-buffer)))
		(set-buffer-modified-p bmp))
	      (tramp-flush-connection-property v "process-name")
	      (tramp-flush-connection-property v "process-buffer"))))))))

(defun tramp-adb-handle-exec-path ()
  "Like `exec-path' for Tramp files."
  (append
   (with-parsed-tramp-file-name default-directory nil
     (with-tramp-connection-property v "remote-path"
       (tramp-adb-send-command v "echo \\\"$PATH\\\"")
       (split-string
	(with-current-buffer (tramp-get-connection-buffer v)
	  ;; Read the expression.
	  (goto-char (point-min))
	  (read (current-buffer)))
	":" 'omit)))
   ;; The equivalent to `exec-directory'.
   `(,(tramp-compat-file-local-name default-directory))))

(defun tramp-adb-get-device (vec)
  "Return full host name from VEC to be used in shell execution.
E.g. a host name \"192.168.1.1#5555\" returns \"192.168.1.1:5555\"
     a host name \"R38273882DE\" returns \"R38273882DE\"."
  ;; Sometimes this is called before there is a connection process
  ;; yet.  In order to work with the connection cache, we flush all
  ;; unwanted entries first.
  (tramp-flush-connection-properties nil)
  (with-tramp-connection-property (tramp-get-connection-process vec) "device"
    (let* ((host (tramp-file-name-host vec))
	   (port (tramp-file-name-port-or-default vec))
	   (devices (mapcar #'cadr (tramp-adb-parse-device-names nil))))
      (replace-regexp-in-string
       tramp-prefix-port-format ":"
       (cond ((member host devices) host)
	     ;; This is the case when the host is connected to the default port.
	     ((member (format "%s%s%d" host tramp-prefix-port-format port)
		      devices)
	      (format "%s:%d" host port))
	     ;; An empty host name shall be mapped as well, when there
	     ;; is exactly one entry in `devices'.
	     ((and (zerop (length host)) (= (length devices) 1))
	      (car devices))
	     ;; Try to connect device.
	     ((and tramp-adb-connect-if-not-connected
		   (not (zerop (length host)))
		   (not (tramp-adb-execute-adb-command
                         vec "connect"
                         (replace-regexp-in-string
                          tramp-prefix-port-format ":" host))))
	      ;; When new device connected, running other adb command (e.g.
	      ;; adb shell) immediately will fail.  To get around this
	      ;; problem, add sleep 0.1 second here.
	      (sleep-for 0.1)
	      host)
	     (t (tramp-error
		 vec 'file-error "Could not find device %s" host)))))))

(defun tramp-adb-execute-adb-command (vec &rest args)
  "Return nil on success error-output on failure."
  (when (and (> (length (tramp-file-name-host vec)) 0)
	     ;; The -s switch is only available for ADB device commands.
	     (not (member (car args) '("connect" "disconnect"))))
    (setq args (append (list "-s" (tramp-adb-get-device vec)) args)))
  (with-temp-buffer
    (prog1
	(unless
	    (zerop
	     (apply #'tramp-call-process vec tramp-adb-program nil t nil args))
	  (buffer-string))
      (tramp-message vec 6 "%s" (buffer-string)))))

(defun tramp-adb-find-test-command (vec)
  "Check whether the ash has a builtin \"test\" command.
This happens for Android >= 4.0."
  (with-tramp-connection-property vec "test"
    (tramp-adb-send-command-and-check vec "type test")))

;; Connection functions

(defun tramp-adb-send-command (vec command &optional neveropen nooutput)
  "Send the COMMAND to connection VEC."
  (unless neveropen (tramp-adb-maybe-open-connection vec))
  (tramp-message vec 6 "%s" command)
  (tramp-send-string vec command)
  (unless nooutput
    ;; FIXME: Race condition.
    (tramp-adb-wait-for-output (tramp-get-connection-process vec))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (save-excursion
	(goto-char (point-min))
	;; We can't use stty to disable echo of command.  stty is said
	;; to be added to toybox 0.7.6.  busybox shall have it, but this
	;; isn't used any longer for Android.
	(delete-matching-lines (regexp-quote command))
	;; When the local machine is W32, there are still trailing ^M.
	;; There must be a better solution by setting the correct coding
	;; system, but this requires changes in core Tramp.
	(goto-char (point-min))
	(while (re-search-forward "\r+$" nil t)
	  (replace-match "" nil nil))))))

(defun tramp-adb-send-command-and-check (vec command)
  "Run COMMAND and check its exit status.
Sends `echo $?' along with the COMMAND for checking the exit
status.  If COMMAND is nil, just sends `echo $?'.  Returns nil if
the exit status is not equal 0, and t otherwise."
  (tramp-adb-send-command
   vec (if command
	   (format "%s; echo tramp_exit_status $?" command)
	 "echo tramp_exit_status $?"))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (unless (tramp-search-regexp "tramp_exit_status [0-9]+")
      (tramp-error
       vec 'file-error "Couldn't find exit status of `%s'" command))
    (skip-chars-forward "^ ")
    (prog1
	(zerop (read (current-buffer)))
      (let ((inhibit-read-only t))
	(delete-region (match-beginning 0) (point-max))))))

(defun tramp-adb-barf-unless-okay (vec command fmt &rest args)
  "Run COMMAND, check exit status, throw error if exit status not okay.
FMT and ARGS are passed to `error'."
  (unless (tramp-adb-send-command-and-check vec command)
    (apply #'tramp-error vec 'file-error fmt args)))

(defun tramp-adb-wait-for-output (proc &optional timeout)
  "Wait for output from remote command."
  (unless (buffer-live-p (process-buffer proc))
    (delete-process proc)
    (tramp-error proc 'file-error "Process `%s' not available, try again" proc))
  (let ((prompt (tramp-get-connection-property proc "prompt" tramp-adb-prompt)))
    (with-current-buffer (process-buffer proc)
      (if (tramp-wait-for-regexp proc timeout prompt)
	  (let ((inhibit-read-only t))
	    (goto-char (point-min))
	    ;; ADB terminal sends "^H" sequences.
	    (when (re-search-forward "<\b+" (point-at-eol) t)
	      (forward-line 1)
	      (delete-region (point-min) (point)))
	    ;; Delete the prompt.
            (goto-char (point-min))
            (when (re-search-forward prompt (point-at-eol) t)
              (forward-line 1)
              (delete-region (point-min) (point)))
	    (when (tramp-search-regexp prompt)
	      (delete-region (point) (point-max))))
	(if timeout
	    (tramp-error
	     proc 'file-error
	     "[[Remote prompt `%s' not found in %d secs]]" prompt timeout)
	  (tramp-error
	   proc 'file-error "[[Remote prompt `%s' not found]]" prompt))))))

(defun tramp-adb-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  (let* ((buf (tramp-get-connection-buffer vec))
	 (p (get-buffer-process buf))
	 (host (tramp-file-name-host vec))
	 (user (tramp-file-name-user vec))
         (device (tramp-adb-get-device vec)))

    ;; Maybe we know already that "su" is not supported.  We cannot
    ;; use a connection property, because we have not checked yet
    ;; whether it is still the same device.
    (when (and user (not (tramp-get-file-property vec "" "su-command-p" t)))
      (tramp-error vec 'file-error "Cannot switch to user `%s'" user))

    (unless (process-live-p p)
      (save-match-data
	(when (and p (processp p)) (delete-process p))
	(if (zerop (length device))
	    (tramp-error vec 'file-error "Device %s not connected" host))
	(with-tramp-progress-reporter vec 3 "Opening adb shell connection"
	  (let* ((coding-system-for-read 'utf-8-dos) ;is this correct?
		 (process-connection-type tramp-process-connection-type)
		 (args (if (> (length host) 0)
			   (list "-s" device "shell")
			 (list "shell")))
		 (p (let ((default-directory
			    (tramp-compat-temporary-file-directory)))
		      (apply #'start-process (tramp-get-connection-name vec) buf
			     tramp-adb-program args)))
		 (prompt (md5 (concat (prin1-to-string process-environment)
				      (current-time-string)))))
	    (tramp-message
	     vec 6 "%s" (string-join (process-command p) " "))
	    ;; Wait for initial prompt.  On some devices, it needs an
	    ;; initial RET, in order to get it.
            (sleep-for 0.1)
	    (tramp-send-string vec tramp-rsh-end-of-line)
	    (tramp-adb-wait-for-output p 30)
	    (unless (process-live-p p)
	      (tramp-error vec 'file-error "Terminated!"))

	    ;; Set sentinel and query flag.  Initialize variables.
	    (set-process-sentinel p #'tramp-process-sentinel)
	    (process-put p 'vector vec)
	    (process-put p 'adjust-window-size-function #'ignore)
	    (set-process-query-on-exit-flag p nil)

	    ;; Change prompt.
	    (tramp-set-connection-property
	     p "prompt" (regexp-quote (format "///%s#$" prompt)))
	    (tramp-adb-send-command
	     vec (format "PS1=\"///\"\"%s\"\"#$\"" prompt))

	    ;; Check whether the properties have been changed.  If
	    ;; yes, this is a strong indication that we must expire all
	    ;; connection properties.  We start again.
	    (tramp-message vec 5 "Checking system information")
	    (tramp-adb-send-command
	     vec "echo \\\"`getprop ro.product.model` `getprop ro.product.version` `getprop ro.build.version.release`\\\"")
	    (let ((old-getprop
		   (tramp-get-connection-property vec "getprop" nil))
		  (new-getprop
		   (tramp-set-connection-property
		    vec "getprop"
		    (with-current-buffer (tramp-get-connection-buffer vec)
		      ;; Read the expression.
		      (goto-char (point-min))
		      (read (current-buffer))))))
	      (when (and (stringp old-getprop)
			 (not (string-equal old-getprop new-getprop)))
		(tramp-message
		 vec 3
		 "Connection reset, because remote host changed from `%s' to `%s'"
		 old-getprop new-getprop)
		(tramp-cleanup-connection vec t)
		(tramp-adb-maybe-open-connection vec)))

	    ;; Change user if indicated.
	    (when user
	      (tramp-adb-send-command vec (format "su %s" user))
	      (unless (tramp-adb-send-command-and-check vec nil)
		(delete-process p)
		(tramp-flush-file-property vec "" "su-command-p")
		(tramp-error
		 vec 'file-error "Cannot switch to user `%s'" user)))

	    ;; Set connection-local variables.
	    (tramp-set-connection-local-variables vec)

	    ;; Mark it as connected.
	    (tramp-set-connection-property p "connected" t)))))))

;; Default settings for connection-local variables.
(defconst tramp-adb-connection-local-default-profile
  '((shell-file-name . "/system/bin/sh")
    (shell-command-switch . "-c"))
  "Default connection-local variables for remote adb connections.")

;; `connection-local-set-profile-variables' and
;; `connection-local-set-profiles' exists since Emacs 26.1.
(with-eval-after-load 'shell
  (tramp-compat-funcall
   'connection-local-set-profile-variables
   'tramp-adb-connection-local-default-profile
   tramp-adb-connection-local-default-profile)
  (tramp-compat-funcall
   'connection-local-set-profiles
   `(:application tramp :protocol ,tramp-adb-method)
   'tramp-adb-connection-local-default-profile))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-adb 'force)))

(provide 'tramp-adb)

;;; tramp-adb.el ends here
