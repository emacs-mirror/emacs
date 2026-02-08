;;; tramp-cmds.el --- Interactive commands for Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.

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
(declare-function dired-advertise "dired")
(declare-function dired-get-file-for-visit "dired")
(declare-function dired-unadvertise "dired")
(declare-function mml-mode "mml")
(declare-function mml-insert-empty-tag "mml")
(declare-function reporter-dump-variable "reporter")
(defvar mm-7bit-chars)
(defvar reporter-eval-buffer)
(defvar reporter-prompt-for-summary-p)
(defvar tramp-repository-branch)
(defvar tramp-repository-version)

;;;###tramp-autoload
(defun tramp-change-syntax (&optional syntax)
  "Change Tramp syntax.
SYNTAX can be one of the symbols `default' (default),
`simplified' (ange-ftp like) or `separate' (XEmacs like)."
  (interactive
   (let ((input (completing-read
		 "Enter Tramp syntax: " (tramp-syntax-values) nil t
		 (symbol-name tramp-syntax))))
     (unless (string-empty-p input)
       (list (intern input)))))
  (when syntax
    (customize-set-variable 'tramp-syntax syntax)))

;;;###tramp-autoload
(defun tramp-enable-method (method)
  "Enable optional METHOD if possible."
  (interactive
   (list
    (completing-read
     "method: "
     (tramp-compat-seq-keep
      (lambda (x)
	(when-let* ((name (symbol-name x))
		    ;; It must match `tramp-enable-METHOD-method'.
		    ((string-match
		      (rx "tramp-enable-"
			  (group (regexp tramp-method-regexp))
			  "-method")
		      name))
		    (method (match-string 1 name))
		    ;; It must not be enabled yet.
		    ((not (assoc method tramp-methods))))
	  method))
      ;; All method enabling functions.
      (apropos-internal (rx bos "tramp-enable-") #'functionp)))))

  (when-let* (((not (assoc method tramp-methods)))
	      (fn (intern (format "tramp-enable-%s-method" method)))
	      ((functionp fn)))
    (funcall fn)
    (message "Tramp method \"%s\" enabled" method)))

;; Use `match-buffers' starting with Emacs 29.1.
;;;###tramp-autoload
(defun tramp-list-tramp-buffers ()
  "Return a list of all Tramp connection buffers."
  (append
   (all-completions
    "*tramp" (mapcar #'list (mapcar #'buffer-name (buffer-list))))
   (all-completions
    "*debug tramp" (mapcar #'list (mapcar #'buffer-name (buffer-list))))
   (all-completions
    "*trace tramp" (mapcar #'list (mapcar #'buffer-name (buffer-list))))))

;; Use `match-buffers' starting with Emacs 29.1.
;;;###tramp-autoload
(defun tramp-list-remote-buffers ()
  "Return a list of remote buffers, excluding internal Tramp buffers.
A buffer is considered remote if either its `default-directory' or
`buffer-file-name' is a remote file name."
  (tramp-compat-seq-keep
   (lambda (buffer)
     (when (tramp-tramp-file-p
            (or (buffer-file-name buffer)
                (tramp-get-default-directory buffer)))
       buffer))
   (buffer-list)))

;;;###tramp-autoload
(defun tramp-list-remote-buffer-connections ()
  "Return a list of all remote buffer connections.
A buffer is considered remote if either its `default-directory' or
`buffer-file-name' is a remote file name."
  (seq-uniq
   (mapcar (lambda (buffer)
             (or
              (when (buffer-file-name buffer)
                (file-remote-p (buffer-file-name buffer)))
              (when (tramp-get-default-directory buffer)
                (file-remote-p (tramp-get-default-directory buffer)))))
           ;; Eliminate false positives from internal Tramp buffers.
           (seq-remove
            (lambda (buffer)
              (member (buffer-name buffer) (tramp-list-tramp-buffers)))
            (tramp-list-remote-buffers)))))

;;; Cleanup

;;;###tramp-autoload
(defvar tramp-cleanup-connection-hook nil
  "List of functions to be called after Tramp connection is cleaned up.
Each function is called with the current vector as argument.")

;;;###tramp-autoload
(defun tramp-cleanup-connection
    (vec &optional keep-debug keep-password keep-processes)
  "Flush all connection related objects.
This includes password cache, file cache, connection cache, buffers,
processes.  KEEP-DEBUG non-nil preserves the debug and trace buffer.
KEEP-PASSWORD non-nil preserves the password cache.  KEEP-PROCESSES
non-nil preserves the asynchronous processes.  When called
interactively, a Tramp connection has to be selected."
  (declare (completion tramp-active-command-completion-p))
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

    ;; Cancel timer.
    (dolist (timer timer-list)
      (when (and (eq (timer--function timer) #'tramp-timeout-session)
		 (tramp-file-name-equal-p vec (car (timer--args timer))))
	(cancel-timer timer)))

    ;; Delete processes.
    (dolist (key (hash-table-keys tramp-cache-data))
      (when (and (processp key)
		 (tramp-file-name-equal-p (process-get key 'tramp-vector) vec)
		 (or (not keep-processes)
		     (eq key (tramp-get-process vec))))
	(tramp-flush-connection-properties key)
	(ignore-errors (delete-process key))))

    ;; Remove buffers.
    (dolist
	(buf (list (get-buffer (tramp-buffer-name vec))
		   (unless keep-debug
		     (get-buffer (tramp-debug-buffer-name vec)))
		   (unless keep-debug
		     (get-buffer (tramp-trace-buffer-name vec)))
		   (tramp-get-connection-property vec " connected")))
      (when (bufferp buf) (kill-buffer buf)))

    ;; Flush file cache.
    (tramp-flush-directory-properties vec "/")

    ;; Flush connection cache.
    (tramp-flush-connection-properties vec)

    ;; The end.
    (run-hook-with-args 'tramp-cleanup-connection-hook vec)))

;;;###tramp-autoload
(defun tramp-cleanup-this-connection ()
  "Flush all connection related objects of the current buffer's connection."
  (declare (completion tramp-command-completion-p))
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
  (declare (completion tramp-active-command-completion-p))
  (interactive)

  ;; Flush password cache.
  (password-reset)

  ;; Flush file and connection cache.
  (clrhash tramp-cache-data)

  ;; Initialize the cache version.
  (tramp-set-connection-property
   tramp-cache-version "tramp-version" tramp-version)

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

  ;; Remove processes and buffers.
  (dolist (name (tramp-list-tramp-buffers))
    (when (processp (get-buffer-process name)) (delete-process name))
    (when (bufferp (get-buffer name)) (kill-buffer name)))

  ;; The end.
  (run-hooks 'tramp-cleanup-all-connections-hook))

(defcustom tramp-cleanup-some-buffers-hook nil
  "Hook for `tramp-cleanup-some-buffers'.
The functions determine which buffers shall be killed.  This
happens when at least one of the functions returns non-nil.  The
functions are called with `current-buffer' set."
  :group 'tramp
  :version "30.1"
  :type 'hook
  :link '(info-link :tag "Tramp manual" "(tramp) Cleanup remote connections"))

(add-hook 'tramp-cleanup-some-buffers-hook
	  #'buffer-file-name)

(defun tramp-dired-buffer-p ()
  "Return t if current buffer runs `dired-mode'."
  (declare (tramp-suppress-trace t))
  (derived-mode-p 'dired-mode))

(add-hook 'tramp-cleanup-some-buffers-hook
	  #'tramp-dired-buffer-p)

(defvar tramp-tainted-remote-process-buffers nil
  "List of process buffers to be cleaned up.")

(defun tramp-delete-tainted-remote-process-buffer-function ()
  "Delete current buffer from `tramp-tainted-remote-process-buffers'."
  (declare (tramp-suppress-trace t))
  (setq tramp-tainted-remote-process-buffers
	(delete (current-buffer) tramp-tainted-remote-process-buffers)))

;;;###tramp-autoload
(defun tramp-taint-remote-process-buffer (buffer)
  "Mark buffer as related to remote processes."
  ;; (declare (tramp-suppress-trace t))
  (add-to-list 'tramp-tainted-remote-process-buffers buffer))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-taint-remote-process-buffer 'tramp-suppress-trace t)

(add-hook 'kill-buffer-hook
	  #'tramp-delete-tainted-remote-process-buffer-function)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'kill-buffer-hook
			 #'tramp-delete-tainted-remote-process-buffer-function)))

(defun tramp-remote-process-p ()
  "Return t if current buffer belongs to a remote process."
  (memq (current-buffer) tramp-tainted-remote-process-buffers))

(add-hook 'tramp-cleanup-some-buffers-hook
	  #'tramp-remote-process-p)

;;;###tramp-autoload
(defun tramp-cleanup-some-buffers ()
  "Kill some remote buffers.
A buffer is killed when it has a remote `default-directory', and
one of the functions in `tramp-cleanup-some-buffers-hook' returns
non-nil."
  (declare (completion tramp-active-command-completion-p))
  (interactive)

  ;; Remove all Tramp related connections.
  (tramp-cleanup-all-connections)

  ;; Remove all buffers with a remote default-directory which fit the hook.
  (dolist (name (tramp-list-remote-buffers))
    (and (buffer-live-p (get-buffer name))
	 (with-current-buffer name
	   (run-hook-with-args-until-success 'tramp-cleanup-some-buffers-hook))
	 (kill-buffer name))))

;;;###tramp-autoload
(defun tramp-cleanup-all-buffers ()
  "Kill all remote buffers."
  (declare (completion tramp-active-command-completion-p))
  (interactive)
  (let ((tramp-cleanup-some-buffers-hook '(always)))
    (tramp-cleanup-some-buffers)))

;;;###tramp-autoload
(defun tramp-cleanup-bufferless-connections ()
  "Flush connection-related objects for which no buffer exists.
A bufferless connection is one for which no live buffer's
`buffer-file-name' or `default-directory' is associated with that
connection, except for Tramp internal buffers.
Display a message of cleaned-up connections."
  (interactive)
  (when-let* ((bufferless-connections
               (seq-difference
                (mapcar #'tramp-make-tramp-file-name (tramp-list-connections))
                (tramp-list-remote-buffer-connections))))
    (message "Cleaning up %s" (string-join bufferless-connections ", "))
    (dolist (connection bufferless-connections)
      (tramp-cleanup-connection
       (tramp-dissect-file-name connection 'noexpand)))))

;;; Rename

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
		       (choice :tag "Target   name" string (const nil))))
  :link '(info-link :tag "Tramp manual" "(tramp) Renaming remote files"))

(defcustom tramp-confirm-rename-file-names t
  "Whether renaming a buffer file name must be confirmed."
  :group 'tramp
  :version "27.1"
  :type 'boolean
  :link '(info-link :tag "Tramp manual" "(tramp) Renaming remote files"))

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
	(when (string-match-p (or (eval (car item) t) "") string)
	  (setq tdra nil
		result
		(tramp-format-spec
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
  (declare (completion tramp-active-command-completion-p))
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
	 (tramp-user-error nil "There are no remote connections")
       (setq source
	     ;; Likely, the source remote connection is broken.  So we
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
		       (rx (literal (file-remote-p source)))))
		 (read-file-name-default
		  "Enter new Tramp connection: "
		  dir default 'confirm init #'file-directory-p)))))

     (list source target)))

  (unless (tramp-tramp-file-p source)
    (tramp-user-error nil "Source %s must be remote" source))
  (when (null target)
    (or (setq target (tramp-default-rename-file source))
	(tramp-user-error
	 nil
	 (concat "There is no target specified.  "
		 "Check `tramp-default-rename-alist' for a proper entry"))))
  (when (tramp-equal-remote source target)
    (tramp-user-error nil "Source and target must have different remote"))

  ;; Append local file name if none is specified.
  (when (string-equal (file-remote-p target) target)
    (setq target (concat target (tramp-file-local-name source))))
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
		 (new-bfn (and (stringp bfn) (string-replace source target bfn)))
		 (prompt (format-message
			  "Set visited file name to `%s' [Type yn!eq or %s] "
                          new-bfn (if (fboundp 'help-key) (help-key) ; 29.1
                                    (key-description (vector help-char))))))
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
  (declare (completion tramp-command-completion-p))
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
		   "Consider `\\[tramp-rename-files]' instead")))
       (setq target
	     (when (null current-prefix-arg)
	       ;; The source remote connection shall not trigger any action.
	       ;; FIXME: Better error prompt when trying to access source host.
	       (let* ((default (or (tramp-default-rename-file source) source))
		      (dir (tramp-rename-read-file-name-dir default))
		      (init (tramp-rename-read-file-name-init default))
		      (tramp-ignored-file-name-regexp
		       (rx (literal (file-remote-p source)))))
		 (read-file-name-default
		  (format "Change Tramp connection `%s': " source)
		  dir default 'confirm init #'file-directory-p)))))
     (list target)))

  (tramp-rename-files default-directory target))

;;; Run as sudo

(defcustom tramp-file-name-with-method "sudo"
  "Which method to be used in `tramp-file-name-with-sudo'."
  :group 'tramp
  :version "31.1"
  ;; It should be a choice of constant strings.  See
  ;; `with-tramp-file-name-with-method'.
  :type '(choice (const "su") (const "surs")
		 (const "sudo") (const "sudors")
		 (const "doas")
		 (const "run0")
		 (const "ksu"))
  :initialize #'custom-initialize-default
  :set #'tramp-set-file-name-with-method
  :link '(tramp-info-link :tag "Tramp manual" tramp-file-name-with-method))

(defun tramp-set-file-name-with-method (symbol value)
  "Set SYMBOL to value VALUE.
Used in user option `tramp-file-name-with-method'.  If VALUE is an
optional method, enable it."
  (unless (string-equal (symbol-value symbol) value)
    ;; Enable optional method.
    (tramp-enable-method value)
    ;; Set the value.
    (when (assoc value tramp-methods)
      (set-default symbol value))))

(defun tramp-get-file-name-with-method ()
  "Return connection-local value of `tramp-file-name-with-method'."
  (tramp-compat-connection-local-value tramp-file-name-with-method))

(defmacro with-tramp-file-name-with-method (&rest body)
  "Ask user for `tramp-file-name-with-method' if needed.
Run BODY."
  (declare (indent 0) (debug t))
  `(let ((tramp-file-name-with-method
          (if current-prefix-arg
	      (completing-read
	       "Tramp method: "
	       ;; Filter out enabled methods.
	       (seq-intersection
		(mapcar #'car tramp-methods)
		(mapcar
		 #'cadr (cdr (get 'tramp-file-name-with-method 'custom-type))))
               nil t (tramp-get-file-name-with-method))
            (tramp-get-file-name-with-method))))
     ,@body))

(defun tramp-file-name-with-sudo (filename)
  "Convert FILENAME into a multi-hop file name with \"sudo\".
An alternative method could be chosen with `tramp-file-name-with-method'."
  (setq filename (expand-file-name filename))
  (let ((default-method (tramp-get-file-name-with-method)))
    (if (tramp-tramp-file-p filename)
	(with-parsed-tramp-file-name filename nil
	  (cond
	   ;; Remote file with proper method.
	   ((string-equal method default-method)
	    filename)
	   ;; Remote file on the local host.
	   ((and
	     (stringp tramp-local-host-regexp) (stringp host)
	     (string-match-p tramp-local-host-regexp host))
	    (tramp-make-tramp-file-name
	     (make-tramp-file-name
	      :method default-method :localname localname)))
	   ;; Remote file with multi-hop capable method.
	   ((tramp-multi-hop-p v)
	    (tramp-make-tramp-file-name
	     (make-tramp-file-name
	      :method (tramp-find-method default-method nil host)
	      :user (tramp-find-user default-method nil host)
	      :host (tramp-find-host default-method nil host)
	      :localname localname :hop (tramp-make-tramp-hop-name v))))
	   ;; Other remote file.
	   (t (tramp-user-error v "Multi-hop with `%s' not applicable" method))))
      ;; Local file.
      (tramp-make-tramp-file-name
       (make-tramp-file-name :method default-method :localname filename)))))

;; FIXME: We would like to rename this for Emacs 31.1 to a name that
;; does not encode the default method.  It is intended as a generic
;; privilege-elevation command.  Some ideas from bug#76974:
;; `tramp-revert-buffer-obtain-root',
;; `tramp-revert-buffer-as-superuser'.

;;;###autoload
(defun tramp-revert-buffer-with-sudo ()
  "Visit the current file again with superuser, or root, permissions.

By default this is done using the \"sudo\" Tramp method.
You can customize `tramp-file-name-with-method' to change this.

Interactively, with a prefix argument, prompt for a different method.

If the buffer visits a file, the file is replaced.
If the buffer runs `dired', the buffer is reverted."
  (interactive)
  (with-tramp-file-name-with-method
    (cond
     ((buffer-file-name)
      (let ((pos (point)))
        (find-alternate-file (tramp-file-name-with-sudo (buffer-file-name)))
        (goto-char pos)))
     ((tramp-dired-buffer-p)
      (dired-unadvertise (expand-file-name default-directory))
      (setq default-directory (tramp-file-name-with-sudo default-directory)
	    list-buffers-directory
	    (tramp-file-name-with-sudo list-buffers-directory))
      (if (consp dired-directory)
	  (setcar
	   dired-directory (tramp-file-name-with-sudo (car dired-directory)))
        (setq dired-directory (tramp-file-name-with-sudo dired-directory)))
      (dired-advertise)
      (revert-buffer)))))

;; This function takes action, when `read-extended-command-predicate'
;; is set to `command-completion-default-include-p'.
(defun tramp-dired-buffer-command-completion-p (_symbol buffer)
  "A predicate for Tramp interactive commands.
They are completed by `M-x TAB' only in Dired buffers."
  (declare (tramp-suppress-trace t))
  (with-current-buffer buffer
    (tramp-dired-buffer-p)))

;; FIXME: See FIXME above about renaming this before Emacs 31.1.

;;;###autoload
(defun tramp-dired-find-file-with-sudo ()
  "Visit the file or directory named on this line as the superuser.

By default this is done using the \"sudo\" Tramp method.
You can customize `tramp-file-name-with-method' to change this.

Interactively, with a prefix argument, prompt for a different method."
  ;; (declare (completion tramp-dired-buffer-command-completion-p))
  (interactive)
  (with-tramp-file-name-with-method
    (find-file (tramp-file-name-with-sudo (dired-get-file-for-visit)))))

;; `tramp-dired-buffer-command-completion-p' is not autoloaded, and this
;; setting isn't either.
(function-put
 #'tramp-dired-find-file-with-sudo 'completion-predicate
 #'tramp-dired-buffer-command-completion-p)

;;; Recompile on ELPA

;; This function takes action, when `read-extended-command-predicate'
;; is set to `command-completion-default-include-p'.
;;;###tramp-autoload
(defun tramp-recompile-elpa-command-completion-p (_symbol _buffer)
  "A predicate for `tramp-recompile-elpa'.
It is completed by `M-x TAB' only if package.el is loaded, and
Tramp is an installed ELPA package."
  ;; We cannot apply `package-installed-p', this would also return the
  ;; builtin package.
  (and (assq 'tramp (bound-and-true-p package-alist))
       (tramp-compat-funcall 'package--user-installed-p 'tramp)))

;;;###tramp-autoload
(defun tramp-recompile-elpa ()
  "Recompile the installed Tramp ELPA package.
This is needed if there are compatibility problems."
  (declare (completion tramp-recompile-elpa-command-completion-p))
  (interactive)
  ;; We expect just one Tramp package is installed.
  (when-let*
      ((dir (tramp-compat-funcall
	     'package-desc-dir
	     (car (alist-get 'tramp (bound-and-true-p package-alist))))))
    (dolist (elc (directory-files dir 'full (rx ".elc" eos)))
      (delete-file elc))
    (with-current-buffer (get-buffer-create byte-compile-log-buffer)
      (let ((inhibit-read-only t))
	(compilation-mode)
	(goto-char (point-max))
	(insert "\f\n")
	(call-process
	 (expand-file-name invocation-name invocation-directory) nil t t
	 "-Q" "-batch" "-L" dir
	 "--eval" (format "(byte-recompile-directory %S 0 t)" dir))
	(message "Package `tramp' recompiled.")))))

;; Tramp version is useful in a number of situations.

;;;###tramp-autoload
(defun tramp-version (arg)
  "Print version number of tramp.el in echo area or current buffer."
  (interactive "P")
  (if arg (insert tramp-version) (message tramp-version)))

;; Make the "reporter" functionality available for making bug reports about
;; the package.  A most useful piece of code.

(autoload 'reporter-submit-bug-report "reporter")

;;;###tramp-autoload
(defun tramp-bug ()
  "Submit a bug report to the Tramp developers."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     tramp-bug-report-address	  ; to-address
     (format "tramp (%s %s/%s)" ; package name and version
	     tramp-version tramp-repository-branch tramp-repository-version)
     (sort
      (tramp-compat-seq-keep
       (lambda (x)
	 (and x (boundp x) (not (get x 'tramp-suppress-trace))
	      (cons x 'tramp-reporter-dump-variable)))
       (append
	(apropos-internal (rx bos "tramp-") #'boundp)
	;; Non-Tramp variables of interest.
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
	  file-name-handler-alist)))
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
  (when-let* ((reporter-eval-buffer reporter-eval-buffer)
	      (val (buffer-local-value varsym reporter-eval-buffer)))

    (if (hash-table-p val)
	;; Pretty print the cache.
	(set varsym (read (format "(%s)" (tramp-cache-print val))))
      ;; There are non-7bit characters to be masked.
      (when (and (stringp val)
		 (string-match-p
		  (rx-to-string `(not (any ,mm-7bit-chars))) val))
	(with-current-buffer reporter-eval-buffer
	  (set varsym
	       `(decode-coding-string
		 (base64-decode-string
		  ,(base64-encode-string (encode-coding-string val 'raw-text)))
		 'raw-text)))))

    ;; Dump variable.
    (goto-char (point-max))
    (save-excursion
      (reporter-dump-variable varsym mailbuf))

    (unless (hash-table-p val)
      ;; Remove string quotation.
      (when (looking-at
	     (rx
	      bol (group (* anychar)) "\""          ;; \1 "
	      (group "(base64-decode-string ") "\\" ;; \2 \
	      (group "\"" (* anychar)) "\\"         ;; \3 \
	      (group "\")") "\"" eol))              ;; \4 "
	(replace-match "\\1\\2\\3\\4")
	(beginning-of-line)
	(insert " ;; Variable encoded due to non-printable characters.\n")))
    (goto-char (point-max))

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
  (dolist (buffer (tramp-compat-seq-keep
		   (lambda (b)
		     (when (string-match-p "\\*tramp/" (buffer-name b)) b))
		   (buffer-list)))
    (let ((reporter-eval-buffer buffer)
	  (elbuf (get-buffer-create " *tmp-reporter-buffer*")))
      (with-current-buffer elbuf
	(emacs-lisp-mode)
	(erase-buffer)
	(insert (format "\n;; %s\n(setq-local\n" (buffer-name buffer)))
	(lisp-indent-line)
	(dolist (varsym
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

  ;; Beautify encoded values.
  (goto-char (point-min))
  (while (search-forward-regexp
	  (rx "'" (group "(decode-coding-string")) nil 'noerror)
    (replace-match "\\1"))
  (goto-char (point-max))

  ;; Dump load-path shadows.
  (insert "\nload-path shadows:\n==================\n")
  (ignore-errors
    (mapc
     (lambda (x) (when (string-search "tramp" x) (insert x "\n")))
     (split-string (list-load-path-shadows t) "\n")))

  ;; Append buffers only when we are in message mode.
  (when (and
	 (eq major-mode 'message-mode)
	 (bound-and-true-p mml-mode))

    (let ((tramp-buf-regexp (rx "*" (? "debug ") "tramp/"))
	  (buffer-list (tramp-list-tramp-buffers))
	  (curbuf (current-buffer)))

      ;; There is at least one Tramp buffer.
      (when buffer-list
	(switch-to-buffer (list-buffers-noselect nil))
	(delete-other-windows)
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (search-forward-regexp tramp-buf-regexp (line-end-position) t)
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

	(when (y-or-n-p "Do you want to append the buffer(s)?")
	  ;; OK, let's send.  First we delete the buffer list.
	  (kill-buffer)
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
