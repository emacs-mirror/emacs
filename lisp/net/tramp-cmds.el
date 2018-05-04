;;; tramp-cmds.el --- Interactive commands for Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2018 Free Software Foundation, Inc.

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

;; This package provides all interactive commands which are related
;; to Tramp.

;;; Code:

(require 'tramp)

;; Pacify byte-compiler.
(declare-function mml-mode "mml")
(declare-function mml-insert-empty-tag "mml")
(declare-function reporter-dump-variable "reporter")
(defvar reporter-eval-buffer)
(defvar reporter-prompt-for-summary-p)

;;;###tramp-autoload
(defun tramp-change-syntax (&optional syntax)
  "Change Tramp syntax.
SYNTAX can be one of the symbols `default' (default),
`simplified' (ange-ftp like) or `separate' (XEmacs like)."
  (interactive
   (let ((input (completing-read
		 "Enter Tramp syntax: " (tramp-syntax-values) nil t
		 (symbol-name tramp-syntax))))
     (unless (string-equal input "")
       (list (intern input)))))
  (when syntax
    (customize-set-variable 'tramp-syntax syntax)))

(defun tramp-list-tramp-buffers ()
  "Return a list of all Tramp connection buffers."
  (append
   (all-completions
    "*tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))
   (all-completions
    "*debug tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))))

(defun tramp-list-remote-buffers ()
  "Return a list of all buffers with remote default-directory."
  (delq
   nil
   (mapcar
    (lambda (x)
      (with-current-buffer x (when (tramp-tramp-file-p default-directory) x)))
    (buffer-list))))

;;;###tramp-autoload
(defun tramp-cleanup-connection (vec &optional keep-debug keep-password)
  "Flush all connection related objects.
This includes password cache, file cache, connection cache,
buffers.  KEEP-DEBUG non-nil preserves the debug buffer.
KEEP-PASSWORD non-nil preserves the password cache.
When called interactively, a Tramp connection has to be selected."
  (interactive
   ;; When interactive, select the Tramp remote identification.
   ;; Return nil when there is no Tramp connection.
   (list
    (let ((connections
	   (mapcar 'tramp-make-tramp-file-name (tramp-list-connections)))
	  name)

      (when connections
	(setq name
	      (completing-read
	       "Enter Tramp connection: " connections nil t
	       (try-completion "" connections)))
	(and (tramp-tramp-file-p name) (tramp-dissect-file-name name))))
    nil nil))

  (if (not vec)
      ;; Nothing to do.
      (message "No Tramp connection found.")

    ;; Flush password cache.
    (unless keep-password (tramp-clear-passwd vec))

    ;; Cleanup `tramp-current-connection'.  Otherwise, we would be
    ;; suppressed in the test suite.  We use `keep-password' as
    ;; indicator; it is not worth to add a new argument.
    (when keep-password (setq tramp-current-connection nil))

    ;; Flush file cache.
    (tramp-flush-directory-properties vec "")

    ;; Flush connection cache.
    (when (processp (tramp-get-connection-process vec))
      (tramp-flush-connection-properties (tramp-get-connection-process vec))
      (delete-process (tramp-get-connection-process vec)))
    (tramp-flush-connection-properties vec)

    ;; Remove buffers.
    (dolist
	(buf (list (get-buffer (tramp-buffer-name vec))
		   (unless keep-debug
		     (get-buffer (tramp-debug-buffer-name vec)))
		   (tramp-get-connection-property vec "process-buffer" nil)))
      (when (bufferp buf) (kill-buffer buf)))))

;;;###tramp-autoload
(defun tramp-cleanup-this-connection ()
  "Flush all connection related objects of the current buffer's connection."
  (interactive)
  (and (tramp-tramp-file-p default-directory)
       (tramp-cleanup-connection
	(tramp-dissect-file-name default-directory 'noexpand))))

;;;###tramp-autoload
(defun tramp-cleanup-all-connections ()
  "Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers."
  (interactive)

  ;; Unlock Tramp.
  (setq tramp-locked nil)

  ;; Flush password cache.
  (password-reset)

  ;; Flush file and connection cache.
  (clrhash tramp-cache-data)

  ;; Cleanup local copies of archives.
  (when (bound-and-true-p tramp-archive-enabled)
    (tramp-archive-cleanup-hash))

  ;; Remove buffers.
  (dolist (name (tramp-list-tramp-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

;;;###tramp-autoload
(defun tramp-cleanup-all-buffers ()
  "Kill all remote buffers."
  (interactive)

  ;; Remove all Tramp related connections.
  (tramp-cleanup-all-connections)

  ;; Remove all buffers with a remote default-directory.
  (dolist (name (tramp-list-remote-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

;; Tramp version is useful in a number of situations.

;;;###tramp-autoload
(defun tramp-version (arg)
  "Print version number of tramp.el in minibuffer or current buffer."
  (interactive "P")
  (if arg (insert tramp-version) (message tramp-version)))

;; Make the "reporter" functionality available for making bug reports about
;; the package.  A most useful piece of code.

(autoload 'reporter-submit-bug-report "reporter")

;;;###tramp-autoload
(defun tramp-bug ()
  "Submit a bug report to the Tramp developers."
  (interactive)
  (catch 'dont-send
    (let ((reporter-prompt-for-summary-p t)
	  ;; In rare cases, it could contain the password.  So we make it nil.
	  tramp-password-save-function)
      (reporter-submit-bug-report
       tramp-bug-report-address		; to-address
       (format "tramp (%s)" tramp-version) ; package name and version
       (sort
	(delq nil (mapcar
	  (lambda (x)
	    (and x (boundp x) (cons x 'tramp-reporter-dump-variable)))
	  (append
	   (mapcar 'intern (all-completions "tramp-" obarray 'boundp))
	   ;; Non-tramp variables of interest.
	   '(shell-prompt-pattern
	     backup-by-copying
	     backup-by-copying-when-linked
	     backup-by-copying-when-mismatch
	     backup-by-copying-when-privileged-mismatch
	     backup-directory-alist
	     password-cache
	     password-cache-expiry
	     remote-file-name-inhibit-cache
	     connection-local-profile-alist
	     connection-local-criteria-alist
	     file-name-handler-alist))))
	(lambda (x y) (string< (symbol-name (car x)) (symbol-name (car y)))))

       'tramp-load-report-modules	; pre-hook
       'tramp-append-tramp-buffers	; post-hook
       (propertize
	"\n" 'display "\
Enter your bug report in this message, including as much detail
as you possibly can about the problem, what you did to cause it
and what the local and remote machines are.

If you can give a simple set of instructions to make this bug
happen reliably, please include those.  Thank you for helping
kill bugs in Tramp.

Before reproducing the bug, you might apply

  M-x tramp-cleanup-all-connections

This allows us to investigate from a clean environment.  Another
useful thing to do is to put

  (setq tramp-verbose 9)

in your init file and to repeat the bug.  Then, include the
contents of the *tramp/foo* buffer and the *debug tramp/foo*
buffer in your bug report.

--bug report follows this line--
")))))

(defun tramp-reporter-dump-variable (varsym mailbuf)
  "Pretty-print the value of the variable in symbol VARSYM."
  (let* ((reporter-eval-buffer (symbol-value 'reporter-eval-buffer))
	 (val (with-current-buffer reporter-eval-buffer
		(symbol-value varsym))))

    (if (hash-table-p val)
	;; Pretty print the cache.
	(set varsym (read (format "(%s)" (tramp-cache-print val))))
      ;; There are non-7bit characters to be masked.
      (when (and (stringp val)
		 (string-match
		  (concat "[^" (bound-and-true-p mm-7bit-chars) "]") val))
	(with-current-buffer reporter-eval-buffer
	  (set
	   varsym
	   (format
	    "(decode-coding-string (base64-decode-string \"%s\") 'raw-text)"
	    (base64-encode-string (encode-coding-string val 'raw-text)))))))

    ;; Dump variable.
    (reporter-dump-variable varsym mailbuf)

    (unless (hash-table-p val)
      ;; Remove string quotation.
      (forward-line -1)
      (when (looking-at
	     (concat "\\(^.*\\)" "\""                       ;; \1 "
		     "\\((base64-decode-string \\)" "\\\\"  ;; \2 \
		     "\\(\".*\\)" "\\\\"                    ;; \3 \
		     "\\(\")\\)" "\"$"))                    ;; \4 "
	(replace-match "\\1\\2\\3\\4")
	(beginning-of-line)
	(insert " ;; Variable encoded due to non-printable characters.\n"))
      (forward-line 1))

    ;; Reset VARSYM to old value.
    (with-current-buffer reporter-eval-buffer
      (set varsym val))))

(defun tramp-load-report-modules ()
  "Load needed modules for reporting."
  (message-mode)
  (mml-mode t))

(defun tramp-append-tramp-buffers ()
  "Append Tramp buffers and buffer local variables into the bug report."
  (goto-char (point-max))

  ;; Dump buffer local variables.
  (insert "\nlocal variables:\n================")
  (dolist (buffer
	   (delq nil
		 (mapcar
		  (lambda (b)
                    (when (string-match "\\*tramp/" (buffer-name b)) b))
		  (buffer-list))))
    (let ((reporter-eval-buffer buffer)
	  (elbuf (get-buffer-create " *tmp-reporter-buffer*")))
      (with-current-buffer elbuf
	(emacs-lisp-mode)
	(erase-buffer)
	(insert (format "\n;; %s\n(setq-local\n" (buffer-name buffer)))
	(lisp-indent-line)
	(dolist
	    (varsym
	     (sort
	      (append
	       (mapcar
		'intern
		(all-completions "tramp-" (buffer-local-variables buffer)))
	       ;; Non-tramp variables of interest.
	       '(connection-local-variables-alist default-directory))
	      'string<))
	    (reporter-dump-variable varsym elbuf))
	(lisp-indent-line)
	(insert ")\n"))
      (insert-buffer-substring elbuf)))

  ;; Dump load-path shadows.
  (insert "\nload-path shadows:\n==================\n")
  (ignore-errors
    (mapc
     (lambda (x) (when (string-match "tramp" x) (insert x "\n")))
     (split-string (list-load-path-shadows t) "\n")))

  ;; Append buffers only when we are in message mode.
  (when (and
	 (eq major-mode 'message-mode)
	 (bound-and-true-p mml-mode))

    (let ((tramp-buf-regexp "\\*\\(debug \\)?tramp/")
	  (buffer-list (tramp-list-tramp-buffers))
	  (curbuf (current-buffer)))

      ;; There is at least one Tramp buffer.
      (when buffer-list
	(switch-to-buffer (list-buffers-noselect nil))
	(delete-other-windows)
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (re-search-forward tramp-buf-regexp (point-at-eol) t)
	      (forward-line 1)
	    (forward-line 0)
	    (let ((start (point)))
	      (forward-line 1)
	      (kill-region start (point)))))
	(insert "
The buffer(s) above will be appended to this message.  If you
don't want to append a buffer because it contains sensitive data,
or because the buffer is too large, you should delete the
respective buffer.  The buffer(s) will contain user and host
names.  Passwords will never be included there.")

	(when (>= tramp-verbose 6)
	  (insert "\n\n")
	  (let ((start (point)))
	    (insert "\
Please note that you have set `tramp-verbose' to a value of at
least 6.  Therefore, the contents of files might be included in
the debug buffer(s).")
	    (add-text-properties start (point) '(face italic))))

	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(goto-char (point-min))

	(if (y-or-n-p "Do you want to append the buffer(s)? ")
	    ;; OK, let's send.  First we delete the buffer list.
	    (progn
	      (kill-buffer nil)
	      (switch-to-buffer curbuf)
	      (goto-char (point-max))
	      (insert (propertize "\n" 'display "\n\
This is a special notion of the `gnus/message' package.  If you
use another mail agent (by copying the contents of this buffer)
please ensure that the buffers are attached to your email.\n\n"))
	      (dolist (buffer buffer-list)
		(mml-insert-empty-tag
		 'part 'type "text/plain"
		 'encoding "base64" 'disposition "attachment" 'buffer buffer
		 'description buffer))
	      (set-buffer-modified-p nil))

	  ;; Don't send.  Delete the message buffer.
	  (set-buffer curbuf)
	  (set-buffer-modified-p nil)
	  (kill-buffer nil)
	  (throw 'dont-send nil))))))

(defalias 'tramp-submit-bug 'tramp-bug)

(add-hook 'tramp-unload-hook
	  (lambda () (unload-feature 'tramp-cmds 'force)))

(provide 'tramp-cmds)

;;; TODO:

;; * Clean up unused *tramp/foo* buffers after a while.  (Pete Forman)
;;
;; * WIBNI there was an interactive command prompting for Tramp
;;   method, hostname, username and filename and translates the user
;;   input into the correct filename syntax (depending on the Emacs
;;   flavor)  (Reiner Steib)
;;
;; * Let the user edit the connection properties interactively.
;;   Something like `gnus-server-edit-server' in Gnus' *Server* buffer.

;;; tramp-cmds.el ends here
