;;; tramp-cmds.el --- Interactive commands for Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2020 Free Software Foundation, Inc.

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
    "*tramp" (mapcar #'list (mapcar #'buffer-name (buffer-list))))
   (all-completions
    "*debug tramp" (mapcar #'list (mapcar #'buffer-name (buffer-list))))))

(defun tramp-list-remote-buffers ()
  "Return a list of all buffers with remote `default-directory'."
  (delq
   nil
   (mapcar
    (lambda (x)
      (with-current-buffer x (when (tramp-tramp-file-p default-directory) x)))
    (buffer-list))))

;;;###tramp-autoload
(defvar tramp-cleanup-connection-hook nil
  "List of functions to be called after Tramp connection is cleaned up.
Each function is called with the current vector as argument.")

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
	   (mapcar #'tramp-make-tramp-file-name (tramp-list-connections)))
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
    ;; suppressed.
    (setq tramp-current-connection nil)

    ;; Flush file cache.
    (tramp-flush-directory-properties vec "")

    ;; Flush connection cache.
    (when (processp (tramp-get-connection-process vec))
      (tramp-flush-connection-properties (tramp-get-connection-process vec))
      (delete-process (tramp-get-connection-process vec)))
    (tramp-flush-connection-properties vec)

    ;; Cancel timer.
    (dolist (timer timer-list)
      (when (and (eq (timer--function timer) 'tramp-timeout-session)
		 (tramp-file-name-equal-p vec (car (timer--args timer))))
	(cancel-timer timer)))

    ;; Remove buffers.
    (dolist
	(buf (list (get-buffer (tramp-buffer-name vec))
		   (unless keep-debug
		     (get-buffer (tramp-debug-buffer-name vec)))
		   (tramp-get-connection-property vec "process-buffer" nil)))
      (when (bufferp buf) (kill-buffer buf)))

    ;; The end.
    (run-hook-with-args 'tramp-cleanup-connection-hook vec)))

;;;###tramp-autoload
(defun tramp-cleanup-this-connection ()
  "Flush all connection related objects of the current buffer's connection."
  (interactive)
  (and (tramp-tramp-file-p default-directory)
       (tramp-cleanup-connection
	(tramp-dissect-file-name default-directory 'noexpand))))

;;;###tramp-autoload
(defvar tramp-cleanup-all-connections-hook nil
  "List of functions to be called after all Tramp connections are cleaned up.")

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

  ;; Remove ad-hoc proxies.
  (let ((proxies tramp-default-proxies-alist))
    (while proxies
      (if (ignore-errors
	    (get-text-property 0 'tramp-ad-hoc (nth 2 (car proxies))))
	  (setq tramp-default-proxies-alist
		(delete (car proxies) tramp-default-proxies-alist)
		proxies tramp-default-proxies-alist)
	(setq proxies (cdr proxies)))))
  (when (and tramp-default-proxies-alist tramp-save-ad-hoc-proxies)
    (customize-save-variable
     'tramp-default-proxies-alist tramp-default-proxies-alist))

  ;; Cancel timers.
  (cancel-function-timers 'tramp-timeout-session)

  ;; Remove buffers.
  (dolist (name (tramp-list-tramp-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name)))

  ;; The end.
  (run-hooks 'tramp-cleanup-all-connections-hook))

;;;###tramp-autoload
(defun tramp-cleanup-all-buffers ()
  "Kill all remote buffers."
  (interactive)

  ;; Remove all Tramp related connections.
  (tramp-cleanup-all-connections)

  ;; Remove all buffers with a remote default-directory.
  (dolist (name (tramp-list-remote-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

;;;###tramp-autoload
(defcustom tramp-default-rename-alist nil
  "Default target for renaming remote buffer file names.
This is an alist of cons cells (SOURCE . TARGET).  The first
matching item specifies the target to be applied for renaming
buffer file names from source via `tramp-rename-files'.  SOURCE
is a regular expressions, which matches a remote file name.
TARGET must be a directory name, which could be remote (including
remote directories Tramp infers by default, such as
\"/method:user@host:\").

TARGET can contain the patterns %m, %u or %h, which are replaced
by the method name, user name or host name of SOURCE when calling
`tramp-rename-files'.

SOURCE could also be a Lisp form, which will be evaluated.  The
result must be a string or nil, which is interpreted as a regular
expression which always matches."
  :group 'tramp
  :version "27.1"
  :type '(repeat (cons (choice :tag "Source regexp" regexp sexp)
		       (choice :tag "Target   name" string (const nil)))))

;;;###tramp-autoload
(defcustom tramp-confirm-rename-file-names t
  "Whether renaming a buffer file name must be confirmed."
  :group 'tramp
  :version "27.1"
  :type 'boolean)

(defun tramp-default-rename-file (string)
  "Determine default file name for renaming according to STRING.
The user option `tramp-default-rename-alist' is consulted,
finding the default mapping.  If there is no matching entry, the
function returns nil"
  (when (tramp-tramp-file-p string)
    (let ((tdra tramp-default-rename-alist)
	  (method (or (file-remote-p string 'method) ""))
	  (user (or (file-remote-p string 'user) ""))
	  (host (or (file-remote-p string 'host) ""))
	  item result)
      (while (setq item (pop tdra))
	(when (string-match-p (or (eval (car item)) "") string)
	  (setq tdra nil
		result
		(format-spec
		 (cdr item) (format-spec-make ?m method ?u user ?h host)))))
      result)))

(defsubst tramp-rename-read-file-name-dir (string)
  "Return the DIR entry to be applied in `read-file-name', based on STRING."
  (when (tramp-tramp-file-p string)
    (substring (file-remote-p string) 0 -1)))

(defsubst tramp-rename-read-file-name-init (string)
  "Return the INIT entry to be applied in `read-file-name', based on STRING."
  (when (tramp-tramp-file-p string)
    (string-remove-prefix (tramp-rename-read-file-name-dir string) string)))

;;;###tramp-autoload
(defun tramp-rename-files (source target)
  "Replace in all buffers the visiting file name from SOURCE to TARGET.
SOURCE is a remote directory name, which could contain also a
localname part.  TARGET is the directory name SOURCE is replaced
with.  Often, TARGET is a remote directory name on another host,
but it can also be a local directory name.  If TARGET has no
local part, the local part from SOURCE is used.

If TARGET is nil, it is selected according to the first match in
`tramp-default-rename-alist'.  If called interactively, this
match is offered as initial value for selection.

On all buffers, which have a `buffer-file-name' matching SOURCE,
this name is modified by replacing SOURCE with TARGET.  This is
applied by calling `set-visited-file-name'.  The new
`buffer-file-name' is prompted for modification in the
minibuffer.  The buffers are marked modified, and must be saved
explicitly.

If user option `tramp-confirm-rename-file-names' is nil, changing
the file name happens without confirmation.  This requires a
matching entry in `tramp-default-rename-alist'.

Remote buffers related to the remote connection identified by
SOURCE, which are not visiting files, or which are visiting files
not matching SOURCE, are not modified.

Interactively, TARGET is selected from `tramp-default-rename-alist'
without confirmation if the prefix argument is non-nil.

The remote connection identified by SOURCE is flushed by
`tramp-cleanup-connection'."
  (interactive
   (let ((connections
	  (mapcar #'tramp-make-tramp-file-name (tramp-list-connections)))
	 ;; Completion packages do their voodoo in `completing-read'
	 ;; and `read-file-name', which is often incompatible with
	 ;; Tramp.  Ignore them.
	 (completing-read-function #'completing-read-default)
	 (read-file-name-function #'read-file-name-default)
	  source target)
     (if (null connections)
	 (tramp-user-error nil "There are no remote connections.")
       (setq source
	     ;; Likely, the source remote connection is broken. So we
	     ;; shall avoid any action on it.
	     (let (non-essential)
	       (completing-read-default
		"Enter old Tramp connection: "
		;; Completion function.
		(completion-table-dynamic
		 (lambda (string)
		   (cond
		    ;; Initially, show existing remote connections.
		    ((not (tramp-tramp-file-p string))
		     (all-completions string connections))
		    ;; There is a selected remote connection.  Show
		    ;; its longest common directory path of respective
		    ;; buffers.
		    (t (mapcar
			(lambda (buffer)
			  (let ((bfn (buffer-file-name buffer)))
			    (and (buffer-live-p buffer)
				 (tramp-equal-remote string bfn)
				 (stringp bfn) (file-name-directory bfn))))
			(tramp-list-remote-buffers))))))
		#'tramp-tramp-file-p t
		;; If the current buffer is a remote one, it is likely
		;; that this connection is meant.  So we offer it as
		;; initial value.  Otherwise, use the longest remote
		;; connection path as initial value.
		(or (file-remote-p default-directory)
		    (try-completion "" connections))))

	     target
	     (when (null current-prefix-arg)
	       ;; The source remote connection shall not trigger any action.
	       ;; FIXME: Better error prompt when trying to access source host.
	       (let* ((default (or (tramp-default-rename-file source) source))
		      (dir (tramp-rename-read-file-name-dir default))
		      (init (tramp-rename-read-file-name-init default))
		      (tramp-ignored-file-name-regexp
		       (regexp-quote (file-remote-p source))))
		 (read-file-name-default
		  "Enter new Tramp connection: "
		  dir default 'confirm init #'file-directory-p)))))

     (list source target)))

  (unless (tramp-tramp-file-p source)
    (tramp-user-error nil "Source %s must be remote." source))
  (when (null target)
    (or (setq target (tramp-default-rename-file source))
	(tramp-user-error
	 nil
	 (eval-when-compile
	   (concat "There is no target specified.  "
		   "Check `tramp-default-rename-alist' for a proper entry.")))))
  (when (tramp-equal-remote source target)
    (tramp-user-error nil "Source and target must have different remote."))

  ;; Append local file name if none is specified.
  (when (string-equal (file-remote-p target) target)
    (setq target (concat target (file-remote-p source 'localname))))
  ;; Make them directory names.
  (setq source (directory-file-name source)
	target (directory-file-name target))

  ;; Rename visited file names of source buffers.
  (save-window-excursion
    (save-current-buffer
      (let ((help-form "\
Type SPC or `y' to set visited file name,
DEL or `n' to skip to next,
`e' to edit the visited file name,
ESC or `q' to quit without changing further buffers,
`!' to change all remaining buffers with no more questions.")
	    (query-choices '(?y ?\s ?n ?\177 ?! ?e ?q ?\e))
	    (query (unless tramp-confirm-rename-file-names ?!))
	    changed-buffers)
	(dolist (buffer (tramp-list-remote-buffers))
          (switch-to-buffer buffer)
	  (let* ((bfn (buffer-file-name))
		 (new-bfn (and (stringp bfn)
			       (replace-regexp-in-string
				(regexp-quote source) target bfn)))
		 (prompt (format-message
			  "Set visited file name to `%s' [Type yn!eq or %s] "
			  new-bfn (key-description (vector help-char)))))
	    (when (and (buffer-live-p buffer) (stringp bfn)
		       (string-prefix-p source bfn)
		       ;; Skip, and don't ask again.
		       (not (memq query '(?q ?\e))))
	      ;; Read prompt.
	      (unless (eq query ?!)
		(setq query (read-char-choice prompt query-choices)))
	      ;; Edit the new buffer file name.
	      (when (eq query ?e)
		(setq new-bfn
		      (read-file-name
		       "New visited file name: "
		       (file-name-directory new-bfn) new-bfn)))
	      ;; Set buffer file name.  Remember the change.
	      (when (memq query '(?y ?\s ?! ?e))
		(setq changed-buffers
		      (cons (list buffer bfn (buffer-modified-p))
			    changed-buffers))
                (set-visited-file-name new-bfn))
	      ;; Quit.  Revert changes if prompted by user.
	      (when (and (memq query '(?q ?\e)) changed-buffers
			 (y-or-n-p "Do you want to revert applied changes?"))
		(dolist (item changed-buffers)
		  (with-current-buffer (car item)
		    (set-visited-file-name (nth 1 item))
		    (set-buffer-modified-p (nth 2 item)))))
	      ;; Cleanup echo area.
	      (message nil)))))))

  ;; Cleanup.
  (tramp-cleanup-connection (tramp-dissect-file-name source)))

;;;###tramp-autoload
(defun tramp-rename-these-files (target)
  "Replace visiting file names to TARGET.
The current buffer must be related to a remote connection.  In
all buffers, which are visiting a file with the same directory
name, the buffer file name is changed.

Interactively, TARGET is selected from `tramp-default-rename-alist'
without confirmation if the prefix argument is non-nil.

For details, see `tramp-rename-files'."
  (interactive
   (let ((source default-directory)
	 target
	 ;; Completion packages do their voodoo in `completing-read'
	 ;; and `read-file-name', which is often incompatible with
	 ;; Tramp.  Ignore them.
	 (completing-read-function #'completing-read-default)
	 (read-file-name-function #'read-file-name-default))
     (if (not (tramp-tramp-file-p source))
	 (tramp-user-error
	  nil
	  (substitute-command-keys
	   (concat "Current buffer is not remote.  "
		   "Consider `\\[tramp-rename-files]' instead.")))
       (setq target
	     (when (null current-prefix-arg)
	       ;; The source remote connection shall not trigger any action.
	       ;; FIXME: Better error prompt when trying to access source host.
	       (let* ((default (or (tramp-default-rename-file source) source))
		      (dir (tramp-rename-read-file-name-dir default))
		      (init (tramp-rename-read-file-name-init default))
		      (tramp-ignored-file-name-regexp
		       (regexp-quote (file-remote-p source))))
		 (read-file-name-default
		  (format "Change Tramp connection `%s': " source)
		  dir default 'confirm init #'file-directory-p)))))
     (list target)))

  (tramp-rename-files default-directory target))

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
  (let ((reporter-prompt-for-summary-p t)
	;; In rare cases, it could contain the password.  So we make it nil.
	tramp-password-save-function)
    (reporter-submit-bug-report
     tramp-bug-report-address	  ; to-address
     (format "tramp (%s %s/%s)" ; package name and version
	     tramp-version tramp-repository-branch tramp-repository-version)
     (sort
      (delq nil (mapcar
	(lambda (x)
	  (and x (boundp x) (cons x 'tramp-reporter-dump-variable)))
	(append
	 (mapcar #'intern (all-completions "tramp-" obarray #'boundp))
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
"))))

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
		 (string-match-p
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
	     (eval-when-compile
	       (concat "\\(^.*\\)" "\""                       ;; \1 "
		       "\\((base64-decode-string \\)" "\\\\"  ;; \2 \
		       "\\(\".*\\)" "\\\\"                    ;; \3 \
		       "\\(\")\\)" "\"$")))                   ;; \4 "
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
                    (when (string-match-p "\\*tramp/" (buffer-name b)) b))
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
		#'intern
		(all-completions "tramp-" (buffer-local-variables buffer)))
	       ;; Non-tramp variables of interest.
	       '(connection-local-variables-alist default-directory))
	      #'string<))
	    (reporter-dump-variable varsym elbuf))
	(lisp-indent-line)
	(insert ")\n"))
      (insert-buffer-substring elbuf)))

  ;; Dump load-path shadows.
  (insert "\nload-path shadows:\n==================\n")
  (ignore-errors
    (mapc
     (lambda (x) (when (string-match-p "tramp" x) (insert x "\n")))
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

	(when (y-or-n-p "Do you want to append the buffer(s)? ")
	  ;; OK, let's send.  First we delete the buffer list.
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
	  (set-buffer-modified-p nil))))))

(defalias 'tramp-submit-bug #'tramp-bug)

(add-hook 'tramp-unload-hook
	  (lambda () (unload-feature 'tramp-cmds 'force)))

(provide 'tramp-cmds)

;;; TODO:

;; * Clean up unused *tramp/foo* buffers after a while.  (Pete Forman)
;;
;; * Let the user edit the connection properties interactively.
;;   Something like `gnus-server-edit-server' in Gnus' *Server* buffer.

;;; tramp-cmds.el ends here
