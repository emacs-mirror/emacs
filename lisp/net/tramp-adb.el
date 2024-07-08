;;; tramp-adb.el --- Functions for calling Android Debug Bridge from Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2024 Free Software Foundation, Inc.

;; Author: Jürgen Hötzel <juergen@archlinux.org>
;; Maintainer: Michael Albinus <michael.albinus@gmx.de>
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

(defvar process-file-return-signal-string)

;;;###tramp-autoload
(defcustom tramp-adb-program "adb"
  "Name of the Android Debug Bridge program."
  :group 'tramp
  :version "24.4"
  :type 'string)

(defcustom tramp-adb-connect-if-not-connected nil
  "Try to run `adb connect' if provided device is not connected currently.
It is used for TCP/IP devices."
  :group 'tramp
  :version "25.1"
  :type 'boolean)

;;;###tramp-autoload
(defconst tramp-adb-method "adb"
  "When this method name is used, forward all calls to Android Debug Bridge.")

(defcustom tramp-adb-prompt (rx bol (* (not (any "#$\r\n"))) (any "#$") blank)
  "Regexp used as prompt in almquist shell."
  :type 'regexp
  :version "28.1"
  :group 'tramp)

(eval-and-compile
  (defconst tramp-adb-ls-date-year-regexp
    (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
    "Regexp for date year format in ls output."))

(eval-and-compile
  (defconst tramp-adb-ls-date-time-regexp (rx (= 2 digit) ":" (= 2 digit))
  "Regexp for date time format in ls output."))

(defconst tramp-adb-ls-date-regexp
  (rx
   blank (regexp tramp-adb-ls-date-year-regexp)
   blank (regexp tramp-adb-ls-date-time-regexp)
   blank)
  "Regexp for date format in ls output.")

(defconst tramp-adb-ls-toolbox-regexp
  (rx
   bol (* blank) (group (+ (any ".-" alpha)))			; \1 permissions
   (? (+ blank) (+ digit))			      ; links (Android 7/toybox)
   (* blank) (group (+ (not blank)))				; \2 username
   (+ blank) (group (+ (not blank)))				; \3 group
   (+ blank) (group (+ digit))					; \4 size
   (+ blank) (group (regexp tramp-adb-ls-date-year-regexp)
	      blank (regexp tramp-adb-ls-date-time-regexp))	; \5 date
   blank (group (* nonl)) eol)					; \6 filename
  "Regexp for ls output.")

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
	      `(,tramp-adb-method
                (tramp-login-program ,tramp-adb-program)
                (tramp-login-args    (("-s" "%d") ("shell")))
                (tramp-direct-async  t)
	        (tramp-tmpdir        "/data/local/tmp")
                (tramp-default-port  5555)))

 (add-to-list 'tramp-default-host-alist `(,tramp-adb-method nil ""))

 (tramp-set-completion-function
  tramp-adb-method '((tramp-adb-parse-device-names ""))))

;;;###tramp-autoload
(defconst tramp-adb-file-name-handler-alist
  '(;; `abbreviate-file-name' performed by default handler.
    (access-file . tramp-handle-access-file)
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
    (file-executable-p . tramp-adb-handle-file-executable-p)
    (file-exists-p . tramp-adb-handle-file-exists-p)
    (file-group-gid . tramp-handle-file-group-gid)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-adb-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
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
    (file-readable-p . tramp-adb-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-adb-handle-file-system-info)
    (file-truename . tramp-handle-file-truename)
    (file-user-uid . tramp-handle-file-user-uid)
    (file-writable-p . tramp-adb-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (list-system-processes . tramp-handle-list-system-processes)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-adb-handle-make-directory)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-adb-handle-make-process)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (memory-info . tramp-handle-memory-info)
    (process-attributes . tramp-handle-process-attributes)
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
    (tramp-get-home-directory . ignore)
    (tramp-get-remote-gid . tramp-adb-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-adb-handle-get-remote-groups)
    (tramp-get-remote-uid . tramp-adb-handle-get-remote-uid)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-adb-handle-write-region))
  "Alist of handler functions for Tramp ADB method.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-adb-file-name-p (vec-or-filename)
  "Check if it's a VEC-OR-FILENAME for ADB."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) tramp-adb-method)))

;;;###tramp-autoload
(defun tramp-adb-file-name-handler (operation &rest args)
  "Invoke the ADB handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION."
  (if-let ((fn (assoc operation tramp-adb-file-name-handler-alist)))
      (prog1 (save-match-data (apply (cdr fn) args))
	(setq tramp-debug-message-fnh-function (cdr fn)))
    (prog1 (tramp-run-real-handler operation args)
      (setq tramp-debug-message-fnh-function operation))))

;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'tramp-adb-file-name-p #'tramp-adb-file-name-handler))

;;;###tramp-autoload
(defun tramp-adb-parse-device-names (_ignore)
  "Return a list of (nil host) tuples allowed to access."
  (tramp-compat-seq-keep
   (lambda (line)
     (when (string-match
	    (rx bol (group (+ (not blank))) (+ blank) "device" eol) line)
       ;; Replace ":" by "#".
       `(nil ,(tramp-compat-string-replace
	       ":" tramp-prefix-port-format (match-string 1 line)))))
   (tramp-process-lines nil tramp-adb-program "devices")))

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
	       (rx (* blank) (+ (not blank))
		   (+ blank) (group (+ digit))
		   (+ blank) (group (+ digit))
		   (+ blank) (group (+ digit))))
	  ;; The values are given as 1k numbers, so we must change
	  ;; them to number of bytes.
	  (list (* 1024 (string-to-number (match-string 1)))
		;; The second value is the used size.  We need the
		;; free size.
		(* 1024 (- (string-to-number (match-string 1))
			   (string-to-number (match-string 2))))
		(* 1024 (string-to-number (match-string 3)))))))))

(defun tramp-adb-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  ;; The result is cached in `tramp-convert-file-attributes'.
  (with-parsed-tramp-file-name filename nil
    (tramp-convert-file-attributes v localname id-format
      (and
       (tramp-adb-send-command-and-check
	v (format "(%s -d -l %s; echo tramp_exit_status $?) | cat"
		  (tramp-adb-get-ls-command v)
		  (tramp-shell-quote-argument localname))
        nil t)
       (with-current-buffer (tramp-get-buffer v)
	 (tramp-adb-sh-fix-ls-output)
	 (cdar (tramp-do-parse-file-attributes-with-ls v)))))))

(defun tramp-do-parse-file-attributes-with-ls (vec)
  "Parse `file-attributes' for Tramp files using the ls(1) command."
  (with-current-buffer (tramp-get-buffer vec)
    (goto-char (point-min))
    (let (file-properties)
      (while (search-forward-regexp tramp-adb-ls-toolbox-regexp nil t)
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
		     (cadr (split-string name (rx (| " -> " "\n")))))))
	  (push (list
		 (if is-symlink
		     (car (split-string name (rx (| " -> " "\n"))))
		   name)
		 (or is-dir symlink-target)
		 1     ;link-count
		 ;; no way to handle numeric ids in Androids ash
		 (cons uid tramp-unknown-id-integer)
		 (cons gid tramp-unknown-id-integer)
		 tramp-time-dont-know   ; atime
		 ;; `date-to-time' checks `iso8601-parse', which might fail.
		 (let (signal-hook-function)
		   (date-to-time date))	; mtime
		 tramp-time-dont-know   ; ctime
		 size
		 mod-string
		 ;; fake
		 t 1
		 (tramp-get-device vec))
		file-properties)))
      file-properties)))

(defun tramp-adb-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for Tramp files."
  (tramp-skeleton-directory-files-and-attributes
      directory full match nosort id-format count
    (with-current-buffer (tramp-get-buffer v)
      (when (tramp-adb-send-command-and-check
	     v (format "(%s -a -l %s; echo tramp_exit_status $?) | cat"
		       (tramp-adb-get-ls-command v)
		       (tramp-shell-quote-argument localname))
             nil t)
	;; We insert also filename/. and filename/.., because "ls"
	;; doesn't on some file systems, like "sdcard".
	(unless (search-backward-regexp (rx "." eol) nil t)
	  (narrow-to-region (point-max) (point-max))
	  (tramp-adb-send-command
	   v (format "%s -d -a -l %s %s | cat"
		     (tramp-adb-get-ls-command v)
		     (tramp-shell-quote-argument
		      (tramp-compat-file-name-concat localname "."))
		     (tramp-shell-quote-argument
		      (tramp-compat-file-name-concat localname ".."))))
	  (tramp-compat-replace-regexp-in-region
	   (rx (literal (file-name-unquote (file-name-as-directory localname))))
	   "" (point-min))
	  (widen)))
      (tramp-adb-sh-fix-ls-output)
      (tramp-do-parse-file-attributes-with-ls v))))

(defun tramp-adb-get-ls-command (vec)
  "Determine `ls' command and its arguments."
  (with-tramp-connection-property vec "ls"
    (tramp-message vec 5 "Finding a suitable `ls' command")
    (cond
     ;; Support Android derived systems where "ls" command is provided
     ;; by GNU Coreutils.  Force "ls" to print one column and set
     ;; time-style to imitate other "ls" flavors.
     ((tramp-adb-send-command-and-check
       vec (concat "ls --time-style=long-iso "
                   (tramp-get-remote-null-device vec)))
      "ls -1 --time-style=long-iso")
     ;; Can't disable coloring explicitly for toybox ls command.  We
     ;; also must force "ls" to print just one column.
     ((tramp-adb-send-command-and-check vec "toybox") "ls -1")
     ;; On CyanogenMod based system BusyBox is used and "ls" output
     ;; coloring is enabled by default.  So we try to disable it when
     ;; possible.
     ((tramp-adb-send-command-and-check
       vec (concat "ls --color=never -al " (tramp-get-remote-null-device vec)))
      "ls --color=never")
     (t "ls"))))

(defun tramp-adb-sh-fix-ls-output (&optional sort-by-time)
  "Insert dummy 0 in empty size columns.
Android's \"ls\" command doesn't insert size column for directories:
Emacs dired can't find files."
  (save-excursion
    ;; Insert missing size.
    (goto-char (point-min))
    (while
	(search-forward-regexp
	 (rx blank (group blank (regexp tramp-adb-ls-date-year-regexp) blank))
	 nil t)
      (replace-match "0\\1" "\\1" nil)
      ;; Insert missing "/".
      (when (looking-at-p
	     (rx (regexp tramp-adb-ls-date-time-regexp) (+ blank) eol))
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
    (setq time-a (encode-time (parse-time-string (match-string 0 a))))
    (string-match tramp-adb-ls-date-regexp b)
    (setq time-b (encode-time (parse-time-string (match-string 0 b))))
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
  (tramp-skeleton-make-directory dir parents
    (unless (tramp-adb-send-command-and-check
	     v (format "mkdir -m %#o %s"
		       (default-file-modes)
		       (tramp-shell-quote-argument localname)))
      (tramp-error v 'file-error "Couldn't make directory %s" dir))))

(defun tramp-adb-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-adb-barf-unless-okay
     v (format "%s %s"
	       (if recursive "rm -r" "rmdir")
	       (tramp-shell-quote-argument localname))
     "Couldn't delete %s" directory)))

(defun tramp-adb-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (tramp-skeleton-delete-file filename trash
    (tramp-adb-barf-unless-okay
     v (format "rm %s" (tramp-shell-quote-argument localname))
     "Couldn't delete %s" filename)))

(defun tramp-adb-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (tramp-skeleton-file-name-all-completions filename directory
    (all-completions
     filename
     (with-parsed-tramp-file-name (expand-file-name directory) nil
       (with-tramp-file-property v localname "file-name-all-completions"
	 (unless (tramp-adb-send-command-and-check
		  v (format "(%s -a %s; echo tramp_exit_status $?) | cat"
			    (tramp-adb-get-ls-command v)
			    (tramp-shell-quote-argument localname))
                  nil t)
	   (erase-buffer))
	 (mapcar
	  (lambda (f)
	    (if (file-directory-p (expand-file-name f directory))
		(file-name-as-directory f)
	      f))
	  (with-current-buffer (tramp-get-buffer v)
	    (mapcar
	     (lambda (l)
	       (and (not (string-match-p (rx bol (* blank) eol) l)) l))
	     (split-string (buffer-string) "\n" 'omit)))))))))

(defun tramp-adb-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (tramp-skeleton-file-local-copy filename
    (with-tramp-progress-reporter
	v 3 (format "Fetching %s to tmp file %s" filename tmpfile)
      ;; "adb pull ..." does not always return an error code.
      (unless
	  (and (tramp-adb-execute-adb-command
		v "pull" (file-name-unquote localname) tmpfile)
	       (file-exists-p tmpfile))
	(ignore-errors (delete-file tmpfile))
	(tramp-error
	 v 'file-error "Cannot make local copy of file `%s'" filename))
      (set-file-modes tmpfile (logior (or (file-modes filename) 0) #o0400)))))

(defun tramp-adb-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      ;; Examine `file-attributes' cache to see if request can be
      ;; satisfied without remote operation.
      (if (tramp-use-file-attributes v)
	  (or (tramp-check-cached-permissions v ?x)
	      (tramp-check-cached-permissions v ?s))
	(tramp-adb-send-command-and-check
	 v (format "test -x %s" (tramp-shell-quote-argument localname)))))))

(defun tramp-adb-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (tramp-skeleton-file-exists-p filename
    (tramp-adb-send-command-and-check
     v (format "test -e %s" (tramp-shell-quote-argument localname)))))

(defun tramp-adb-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-readable-p"
      ;; Examine `file-attributes' cache to see if request can be
      ;; satisfied without remote operation.
      (if (tramp-use-file-attributes v)
	  (tramp-handle-file-readable-p filename)
	(tramp-adb-send-command-and-check
	 v (format "test -r %s" (tramp-shell-quote-argument localname)))))))

(defun tramp-adb-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
          ;; The file-attributes cache is unreliable since its
          ;; information does not take partition writability into
          ;; account, so a call to test must never be skipped.
	  (tramp-adb-send-command-and-check
	   v (format "test -w %s" (tramp-shell-quote-argument localname)))
	;; If file doesn't exist, check if directory is writable.
	(and
	 (file-directory-p (file-name-directory filename))
	 (file-writable-p (file-name-directory filename)))))))

(defun tramp-adb-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (tramp-skeleton-write-region start end filename append visit lockname mustbenew
    ;; If `start' is the empty string, it is likely that a temporary
    ;; file is created.  Do it directly.
    (if (and (stringp start) (string-empty-p start))
	(tramp-adb-send-command-and-check
	 v (format "echo -n \"\" >%s" (tramp-shell-quote-argument localname)))

      (let ((tmpfile (tramp-compat-make-temp-file filename)))
	(when (and append (file-exists-p filename))
	  (copy-file filename tmpfile 'ok)
	  (set-file-modes tmpfile (logior (or (file-modes tmpfile) 0) #o0600)))
	(let (create-lockfiles)
          (write-region start end tmpfile append 'no-message))
	;; Now, `last-coding-system-used' has the right value.  Remember it.
	(setq coding-system-used last-coding-system-used)
	(with-tramp-progress-reporter
	    v 3 (format-message
		 "Moving tmp file `%s' to `%s'" tmpfile filename)
	  (unwind-protect
	      (unless (tramp-adb-execute-adb-command
		       v "push" tmpfile (file-name-unquote localname))
		(tramp-error v 'file-error "Cannot write: `%s'" filename))
	    (delete-file tmpfile)))))))

(defun tramp-adb-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  ;; ADB shell does not support "chmod -h".
  (unless (and (eq flag 'nofollow) (file-symlink-p filename))
    (tramp-skeleton-set-file-modes-times-uid-gid filename
      (tramp-adb-send-command-and-check
       v (format "chmod %o %s" mode (tramp-shell-quote-argument localname))))))

(defun tramp-adb-handle-set-file-times (filename &optional time flag)
  "Like `set-file-times' for Tramp files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (let ((time (tramp-defined-time time))
	  (nofollow (if (eq flag 'nofollow) "-h" ""))
	  (quoted-name (tramp-shell-quote-argument localname)))
      ;; Older versions of toybox 'touch' mishandle nanoseconds and/or
      ;; trailing "Z", so fall back on plain seconds if nanoseconds+Z
      ;; fails.  Also, fall back on old POSIX 'touch -t' if 'touch -d'
      ;; (introduced in POSIX.1-2008) fails.
      (tramp-adb-send-command-and-check
       v (format
	  (concat "touch -d %s %s %s 2>%s || "
		  "touch -d %s %s %s 2>%s || "
		  "touch -t %s %s %s")
	  (format-time-string "%Y-%m-%dT%H:%M:%S.%NZ" time t)
	  nofollow quoted-name (tramp-get-remote-null-device v)
	  (format-time-string "%Y-%m-%dT%H:%M:%S" time t)
	  nofollow quoted-name (tramp-get-remote-null-device v)
	  (format-time-string "%Y%m%d%H%M.%S" time t)
	  nofollow quoted-name)))))

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
	  (t2 (tramp-tramp-file-p newname))
	  ;; We don't want the target file to be compressed, so we
	  ;; let-bind `jka-compr-inhibit' to t.
	  (jka-compr-inhibit t))
      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(tramp-barf-if-file-missing v filename
	  (when (and (not ok-if-already-exists) (file-exists-p newname))
	    (tramp-error v 'file-already-exists newname))
	  (when (and (file-directory-p newname)
		     (not (directory-name-p newname)))
	    (tramp-error v 'file-error "File is a directory %s" newname))

	  (with-tramp-progress-reporter
	      v 0 (format "Copying %s to %s" filename newname)
	    (if (and t1 t2 (tramp-equal-remote filename newname))
		(let ((l1 (tramp-file-local-name filename))
		      (l2 (tramp-file-local-name newname)))
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
		  (unless (tramp-adb-execute-adb-command
			   v "push"
			   (file-name-unquote filename)
			   (file-name-unquote localname))
		    (tramp-error
		     v 'file-error
		     "Cannot copy `%s' `%s'" filename newname)))))))))

    ;; KEEP-DATE handling.
    (when keep-date
      (tramp-compat-set-file-times
       newname
       (file-attribute-modification-time (file-attributes filename))
       (unless ok-if-already-exists 'nofollow)))))

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
	  (t2 (tramp-tramp-file-p newname))
	  ;; We don't want the target file to be compressed, so we
	  ;; let-bind `jka-compr-inhibit' to t.
	  (jka-compr-inhibit t))
      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(tramp-barf-if-file-missing v filename
	  (when (and (not ok-if-already-exists) (file-exists-p newname))
	    (tramp-error v 'file-already-exists newname))
	  (when (and (file-directory-p newname)
		     (not (directory-name-p newname)))
	    (tramp-error v 'file-error "File is a directory %s" newname))

	  (with-tramp-progress-reporter
	      v 0 (format "Renaming %s to %s" filename newname)
	    (if (and t1 t2
		     (tramp-equal-remote filename newname)
		     (not (file-directory-p filename)))
		(let ((l1 (tramp-file-local-name filename))
		      (l2 (tramp-file-local-name newname)))
		  ;; We must also flush the cache of the directory,
		  ;; because `file-attributes' reads the values from
		  ;; there.
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
	       filename newname ok-if-already-exists
	       'keep-time 'preserve-uid-gid)
	      (delete-file filename))))))))

(defun tramp-adb-get-signal-strings (vec)
  "Strings to return by `process-file' in case of signals."
  (with-tramp-connection-property vec "signal-strings"
    (let ((default-directory (tramp-make-tramp-file-name vec 'noloc))
	  process-file-return-signal-string signals result)
      (dotimes (i 128) (push (format "Signal %d" i) result))
      (setq result (reverse result)
	    signals (split-string
		     (shell-command-to-string "COLUMNS=40 kill -l") "\n" 'omit))
      (setcar result 0)
      (dolist (line signals)
	(when (string-match
	       (rx bol (* blank) (group (+ digit))
		   (+ blank) (+ (not blank))
		   (+ blank) (group alpha (* nonl)) eol)
	       line)
	  (setcar
	   (nthcdr (string-to-number (match-string 1 line)) result)
	   (match-string 2 line))))
      result)))

(defun tramp-adb-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  (tramp-skeleton-process-file program infile destination display args
      ;; Compute command.
      (setq command (mapconcat #'tramp-shell-quote-argument
			       (cons program args) " "))
      (when input (setq command (format "%s <%s" command input)))
      (when stderr (setq command (format "%s 2>%s" command stderr)))

      ;; Send the command.  It might not return in time, so we protect
      ;; it.  Call it in a subshell, in order to preserve working
      ;; directory.
      (condition-case nil
	  (unwind-protect
	      (setq ret (tramp-adb-send-command-and-check
			 v (format
			    "(cd %s; %s)"
			    (tramp-unquote-shell-quote-argument localname)
			    command)
			 t))
	    (unless (natnump ret) (setq ret 1))
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

      ;; Handle signals.  `process-file-return-signal-string' exists
      ;; since Emacs 28.1.
      (when (and (bound-and-true-p process-file-return-signal-string)
		 (natnump ret) (> ret 128))
	(setq ret (nth (- ret 128) (tramp-adb-get-signal-strings v))))))

;; We use BUFFER also as connection buffer during setup.  Because of
;; this, its original contents must be saved, and restored once
;; connection has been setup.
;; The complete STDERR buffer is available only when the process has
;; terminated.
(defun tramp-adb-handle-make-process (&rest args)
  "Like `make-process' for Tramp files.
STDERR can also be a remote file name.  If method parameter
`tramp-direct-async' and connection-local variable
`tramp-direct-async-process' are non-nil, an alternative implementation
will be used."
  (if (tramp-direct-async-process-p args)
      (apply #'tramp-handle-make-process args)
    (tramp-skeleton-make-process args nil t
      (let* ((program (car command))
	     (args (cdr command))
	     ;; STDERR can also be a file name.
	     (tmpstderr
	      (and stderr
		   (if (and (stringp stderr) (tramp-tramp-file-p stderr))
		       (tramp-unquote-file-local-name stderr)
		     (tramp-make-tramp-temp-file v))))
	     (remote-tmpstderr
	      (and tmpstderr (tramp-make-tramp-file-name v tmpstderr)))
	     (command
	      (format "cd %s && exec %s %s"
		      (tramp-shell-quote-argument localname)
		      (if tmpstderr (format "2>'%s'" tmpstderr) "")
		      (mapconcat #'tramp-shell-quote-argument
				 (cons program args) " ")))
	     (tramp-process-connection-type
	      (or (null program) tramp-process-connection-type))
	     (bmp (and (buffer-live-p buffer) (buffer-modified-p buffer)))
	     p)

	(when (string-match-p (rx multibyte) command)
	  (tramp-error
	   v 'file-error "Cannot apply multibyte command `%s'" command))

	(with-tramp-saved-connection-properties
	    v '("process-name" "process-buffer")
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
			  (inhibit-read-only t)
			  (coding-system-for-write
			   (if (symbolp coding) coding (car coding)))
			  (coding-system-for-read
			   (if (symbolp coding) coding (cdr coding))))
		      (clear-visited-file-modtime)
		      (narrow-to-region (point-max) (point-max))
		      ;; We call `tramp-adb-maybe-open-connection', in
		      ;; order to cleanup the prompt afterwards.
		      (tramp-adb-maybe-open-connection v)
		      (delete-region (point-min) (point-max))
		      ;; Send the command.
		      (setq p (tramp-get-connection-process v))
                      (tramp-adb-send-command v command nil t) ; nooutput
		      ;; Set sentinel and filter.
		      (when sentinel
			(set-process-sentinel p sentinel))
		      (when filter
			(set-process-filter p filter))
		      (process-put p 'remote-command orig-command)
		      (tramp-set-connection-property
		       p "remote-command" orig-command)
		      ;; Set query flag and process marker for this
		      ;; process.  We ignore errors, because the
		      ;; process could have finished already.
		      (ignore-errors
			(set-process-query-on-exit-flag p (null noquery))
			(set-marker (process-mark p) (point))
			;; We must flush them here already;
			;; otherwise `rename-file', `delete-file'
			;; or `insert-file-contents' will fail.
			(tramp-flush-connection-property v "process-name")
			(tramp-flush-connection-property v "process-buffer")
			;; Copy tmpstderr file.
			(when (and (stringp stderr)
				   (not (tramp-tramp-file-p stderr)))
			  (add-function
			   :after (process-sentinel p)
			   (lambda (_proc _msg)
			     (rename-file remote-tmpstderr stderr))))
			;; Read initial output.  Remove the first
			;; line, which is the command echo.
			(unless (eq filter t)
			  (while (progn
				   (goto-char (point-min))
				   (not (search-forward "\n" nil t)))
			    (tramp-accept-process-output p))
			  (delete-region (point-min) (point)))
			;; Provide error buffer.  This shows only
			;; initial error messages; messages arriving
			;; later on will be inserted when the process
			;; is deleted.  The temporary file will exist
			;; until the process is deleted.
			(when (bufferp stderr)
			  (ignore-errors
			    (tramp-taint-remote-process-buffer stderr)
			    (with-current-buffer stderr
			      (insert-file-contents-literally
			       remote-tmpstderr 'visit)))
			  ;; Delete tmpstderr file.
			  (add-function
			   :after (process-sentinel p)
			   (lambda (_proc _msg)
			     (ignore-errors
			       (with-current-buffer stderr
				 (insert-file-contents-literally
				  remote-tmpstderr 'visit nil nil 'replace))
			       (delete-file remote-tmpstderr)))))
			;; Return process.
			p))))

	      ;; Save exit.
	      (if (string-prefix-p tramp-temp-buffer-name (buffer-name))
		  (ignore-errors
		    (set-process-buffer p nil)
		    (kill-buffer (current-buffer)))
		(set-buffer-modified-p bmp)))))))))

(defun tramp-adb-handle-exec-path ()
  "Like `exec-path' for Tramp files."
  (append
   (with-parsed-tramp-file-name default-directory nil
     (with-tramp-connection-property (tramp-get-process v) "remote-path"
       (tramp-adb-send-command v "echo \\\"$PATH\\\"")
       (split-string
	(with-current-buffer (tramp-get-connection-buffer v)
	  ;; Read the expression.
	  (goto-char (point-min))
	  (read (current-buffer)))
	":" 'omit)))
   ;; The equivalent to `exec-directory'.
   `(,(tramp-file-local-name (expand-file-name default-directory)))))

(defun tramp-adb-handle-get-remote-uid (vec id-format)
  "Like `tramp-get-remote-uid' for Tramp files.
ID-FORMAT valid values are `string' and `integer'."
  (tramp-adb-send-command vec "id")
  (tramp-read-id-output vec)
  (tramp-get-connection-property vec (format "uid-%s" id-format)))

(defun tramp-adb-handle-get-remote-gid (vec id-format)
  "Like `tramp-get-remote-gid' for Tramp files.
ID-FORMAT valid values are `string' and `integer'."
  (tramp-adb-send-command vec "id")
  (tramp-read-id-output vec)
  (tramp-get-connection-property vec (format "gid-%s" id-format)))

(defun tramp-adb-handle-get-remote-groups (vec id-format)
  "Like `tramp-get-remote-groups' for Tramp files.
ID-FORMAT valid values are `string' and `integer'."
  (tramp-adb-send-command vec "id")
  (tramp-read-id-output vec)
  (tramp-get-connection-property vec (format "groups-%s" id-format)))

(defun tramp-adb-get-device (vec)
  "Return full host name from VEC to be used in shell execution.
E.g. a host name \"192.168.1.1#5555\" returns \"192.168.1.1:5555\"
     a host name \"R38273882DE\" returns \"R38273882DE\"."
  (with-tramp-connection-property (tramp-get-process vec) "device"
    (let* ((host (tramp-file-name-host vec))
	   (port (tramp-file-name-port-or-default vec))
	   (devices (mapcar #'cadr (tramp-adb-parse-device-names nil))))
      (tramp-compat-string-replace
       tramp-prefix-port-format ":"
       (cond ((member host devices) host)
	     ;; This is the case when the host is connected to the default port.
	     ((member (format "%s%s%s" host tramp-prefix-port-format port)
		      devices)
	      (format "%s:%s" host port))
	     ;; An empty host name shall be mapped as well, when there
	     ;; is exactly one entry in `devices'.
	     ((and (tramp-string-empty-or-nil-p host)
		   (tramp-compat-length= devices 1))
	      (car devices))
	     ;; Try to connect device.
	     ((and tramp-adb-connect-if-not-connected
		   (tramp-compat-length> host 0)
		   (tramp-adb-execute-adb-command
                    vec "connect"
                    (tramp-compat-string-replace
		     tramp-prefix-port-format ":" host)))
	      ;; When new device connected, running other adb command (e.g.
	      ;; adb shell) immediately will fail.  To get around this
	      ;; problem, add sleep 0.1 second here.
	      (sleep-for 0.1)
	      host)
	     (t (tramp-error
		 vec 'file-error "Could not find device %s" host)))))))

(defun tramp-adb-execute-adb-command (vec &rest args)
  "Execute an adb command.
Insert the result into the connection buffer.  Return nil on
error and non-nil on success."
  (when (and (tramp-compat-length> (tramp-file-name-host vec) 0)
	     ;; The -s switch is only available for ADB device commands.
	     (not (member (car args) '("connect" "disconnect"))))
    (setq args (append (list "-s" (tramp-adb-get-device vec)) args)))
  (with-current-buffer (tramp-get-connection-buffer vec)
    ;; Clean up the buffer.  We cannot call `erase-buffer' because
    ;; narrowing might be in effect.
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (zerop (apply #'tramp-call-process vec tramp-adb-program nil t nil args))))

;; Connection functions

(defun tramp-adb-send-command (vec command &optional neveropen nooutput)
  "Send the COMMAND to connection VEC."
  (if (string-match-p (rx multibyte) command)
      ;; Multibyte codepoints with four bytes are not supported at
      ;; least by toybox.

      ;; <https://android.stackexchange.com/questions/226638/how-to-use-multibyte-file-names-in-adb-shell/232379#232379>
      ;; mksh uses UTF-8 internally, but is currently limited to the
      ;; BMP (basic multilingua plane), which means U+0000 to
      ;; U+FFFD. If you want to use SMP codepoints (U-00010000 to
      ;; U-0010FFFD) on the input line, you currently have to disable
      ;; the UTF-8 mode (sorry).
      (tramp-adb-execute-adb-command vec "shell" command)

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
	  (delete-matching-lines (rx bol (literal command) eol))
	  ;; When the local machine is W32, there are still trailing ^M.
	  ;; There must be a better solution by setting the correct coding
	  ;; system, but this requires changes in core Tramp.
	  (goto-char (point-min))
	  (while (search-forward-regexp (rx (+ "\r") eol) nil t)
	    (replace-match "" nil nil)))))))

(defun tramp-adb-send-command-and-check
    (vec command &optional exit-status command-augmented-p)
  "Run COMMAND and check its exit status.
Sends `echo $?' along with the COMMAND for checking the exit
status.  If COMMAND is nil, just sends `echo $?'.  Returns nil if
the exit status is not equal 0, and t otherwise.

If COMMAND-AUGMENTED-P, COMMAND is already configured to print exit
status upon completion and need not be modified.

Optional argument EXIT-STATUS, if non-nil, triggers the return of
the exit status."
  (tramp-adb-send-command
   vec (if command
	   (if command-augmented-p
               command
             (format "%s; echo tramp_exit_status $?" command))
	 "echo tramp_exit_status $?"))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (unless (tramp-search-regexp (rx "tramp_exit_status " (+ digit)))
      (tramp-error
       vec 'file-error "Couldn't find exit status of `%s'" command))
    (skip-chars-forward "^ ")
    (prog1
	(if exit-status
	    (read (current-buffer))
	  (zerop (read (current-buffer))))
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
	    (when (search-forward-regexp (rx "<" (+ "\b")) (line-end-position) t)
	      (forward-line 1)
	      (delete-region (point-min) (point)))
	    ;; Delete the prompt.
            (goto-char (point-min))
            (when (search-forward-regexp prompt (line-end-position) t)
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

  (with-tramp-debug-message vec "Opening connection"
    (let* ((buf (tramp-get-connection-buffer vec))
	   (p (get-buffer-process buf))
	   (host (tramp-file-name-host vec))
	   (user (tramp-file-name-user vec))
           (device (tramp-adb-get-device vec)))

      ;; Maybe we know already that "su" is not supported.  We cannot
      ;; use a connection property, because we have not checked yet
      ;; whether it is still the same device.
      (when (and user (not (tramp-get-file-property vec "/" "su-command-p" t)))
	(tramp-error vec 'file-error "Cannot switch to user `%s'" user))

      (unless (process-live-p p)
	(save-match-data
	  (when (and p (processp p)) (delete-process p))
	  (if (tramp-string-empty-or-nil-p device)
	      (tramp-error vec 'file-error "Device %s not connected" host))
	  (with-tramp-progress-reporter vec 3 "Opening adb shell connection"
	    (let* ((coding-system-for-read 'utf-8-dos) ; Is this correct?
		   (process-connection-type tramp-process-connection-type)
		   (args (tramp-expand-args
			  vec 'tramp-login-args nil ?d (or device "")))
		   (p (let ((default-directory
			     tramp-compat-temporary-file-directory))
			(apply
			 #'start-process (tramp-get-connection-name vec) buf
			 tramp-adb-program args)))
		   (prompt (md5 (concat (prin1-to-string process-environment)
					(current-time-string)))))
	      ;; Wait for initial prompt.  On some devices, it needs
	      ;; an initial RET, in order to get it.
              (sleep-for 0.1)
	      (tramp-send-string vec tramp-rsh-end-of-line)
	      (tramp-adb-wait-for-output p 30)
	      (unless (process-live-p p)
		(tramp-error vec 'file-error "Terminated!"))

	      ;; Set sentinel.  Initialize variables.
	      (set-process-sentinel p #'tramp-process-sentinel)
	      (tramp-post-process-creation p vec)

	      ;; Set connection-local variables.
	      (tramp-set-connection-local-variables vec)

	      ;; Change prompt.
	      (tramp-set-connection-property
	       p "prompt" (rx "///" (literal prompt) "#$"))
	      (tramp-adb-send-command
	       vec (format "PS1=\"///\"\"%s\"\"#$\" PS2=''" prompt))

	      ;; Disable line editing.
	      (tramp-adb-send-command
	       vec "set +o vi +o vi-esccomplete +o vi-tabcomplete +o emacs")

	      ;; Dump option settings in the traces.
	      (when (>= tramp-verbose 9)
		(tramp-adb-send-command vec "set -o"))

	      ;; Check whether the properties have been changed.  If
	      ;; yes, this is a strong indication that we must expire
	      ;; all connection properties.  We start again.
	      (tramp-message vec 5 "Checking system information")
	      (tramp-adb-send-command
	       vec
	       (concat
		"echo \\\"`getprop ro.product.model` "
		"`getprop ro.product.version` "
		"`getprop ro.build.version.release`\\\""))
	      (let ((old-getprop (tramp-get-connection-property vec "getprop"))
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
		   (concat
		    "Connection reset, because remote host changed "
		    "from `%s' to `%s'")
		   old-getprop new-getprop)
		  (tramp-cleanup-connection vec t)
		  (tramp-adb-maybe-open-connection vec)))

	      ;; Change user if indicated.
	      (when user
		(tramp-adb-send-command vec (format "su %s" user))
		(unless (tramp-adb-send-command-and-check vec nil)
		  (delete-process p)
		  ;; Do not flush, we need the nil value.
		  (tramp-set-file-property vec "/" "su-command-p" nil)
		  (tramp-error
		   vec 'file-error "Cannot switch to user `%s'" user)))

	      ;; Mark it as connected.
	      (tramp-set-connection-property p "connected" t))))))))

;;; Default connection-local variables for Tramp.

(defconst tramp-adb-connection-local-default-shell-variables
  '((shell-file-name . "/system/bin/sh")
    (shell-command-switch . "-c"))
  "Default connection-local shell variables for remote adb connections.")

(connection-local-set-profile-variables
 'tramp-adb-connection-local-default-shell-profile
 tramp-adb-connection-local-default-shell-variables)

(defconst tramp-adb-connection-local-default-ps-variables
  '((tramp-process-attributes-ps-args)
    (tramp-process-attributes-ps-format
     . ((user . string)
        (pid . number)
        (ppid . number)
        (vsize . number)
        (rss . number)
        (wchan . string) ; ??
        (pc . string) ; ??
        (state . string)
        (args . nil))))
  "Default connection-local ps variables for remote adb connections.")

(connection-local-set-profile-variables
 'tramp-adb-connection-local-default-ps-profile
 tramp-adb-connection-local-default-ps-variables)

(with-eval-after-load 'shell
  (connection-local-set-profiles
   `(:application tramp :protocol ,tramp-adb-method)
   'tramp-adb-connection-local-default-shell-profile
   'tramp-adb-connection-local-default-ps-profile))

;; `shell-mode' tries to open remote files like "/adb::~/.history".
;; This fails, because the tilde cannot be expanded.  Tell
;; `tramp-handle-expand-file-name' to tolerate this.
(defun tramp-adb-tolerate-tilde (orig-fun)
  "Advice for `shell-mode' to tolerate tilde in remote file names."
  (let ((tramp-tolerate-tilde
	 (or tramp-tolerate-tilde
	     (equal (file-remote-p default-directory 'method)
		    tramp-adb-method))))
    (funcall orig-fun)))

(add-function
 :around (symbol-function #'shell-mode) #'tramp-adb-tolerate-tilde)
(add-hook 'tramp-adb-unload-hook
	  (lambda ()
	    (remove-function
	     (symbol-function #'shell-mode) #'tramp-adb-tolerate-tilde)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-adb 'force)))

(provide 'tramp-adb)

;;; TODO:
;;
;; * Support file names with multibyte codepoints.  Use as fallback
;;   "adb shell COMMAND".
;;
;;; tramp-adb.el ends here
