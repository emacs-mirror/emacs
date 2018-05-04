;;; autorevert.el --- revert buffers when files on disk change  -*- lexical-binding:t -*-

;; Copyright (C) 1997-1999, 2001-2018 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Keywords: convenience
;; Created: 1997-06-01
;; Date: 1999-11-30

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

;; Introduction:
;;
;; Whenever a file that Emacs is editing has been changed by another
;; program the user normally has to execute the command `revert-buffer'
;; to load the new content of the file into Emacs.
;;
;; This package contains two minor modes: Global Auto-Revert Mode and
;; Auto-Revert Mode.  Both modes automatically revert buffers
;; whenever the corresponding files have been changed on disk and the
;; buffer contains no unsaved changes.
;;
;; Auto-Revert Mode can be activated for individual buffers.  Global
;; Auto-Revert Mode applies to all file buffers. (If the user option
;; `global-auto-revert-non-file-buffers' is non-nil, it also applies
;; to some non-file buffers.  This option is disabled by default.)
;;
;; Since checking a remote file is slow, these modes check or revert
;; remote files only if the user option `auto-revert-remote-files' is
;; non-nil.  It is recommended to disable version control for remote
;; files.
;;
;; Both modes operate by checking the time stamp of all files at
;; intervals of `auto-revert-interval'.  The default is every five
;; seconds.  The check is aborted whenever the user actually uses
;; Emacs.  You should never even notice that this package is active
;; (except that your buffers will be reverted, of course).
;;
;; If the file exists, Auto-Revert Mode updates the buffer based on
;; its (possibly empty) contents.  If the file no longer exists, then
;; there is nothing to revert, so it does not modify the buffer.  Once
;; a deleted file corresponding to a buffer in Auto-Revert Mode
;; reappears, Auto-Revert Mode continues to work.
;;
;; If Emacs is compiled with file notification support, notifications
;; are used instead of checking the time stamp of the files.  You can
;; disable this by setting the user option `auto-revert-use-notify' to
;; nil.  Alternatively, a regular expression of directories to be
;; excluded from file notifications can be specified by
;; `auto-revert-notify-exclude-dir-regexp'.
;;
;; After reverting a file buffer, Auto-Revert Mode normally puts point
;; at the same position that a regular manual revert would.  However,
;; there is one exception to this rule.  If point is at the end of the
;; buffer before reverting, it stays at the end.  Similarly if point
;; is displayed at the end of a file buffer in any window, it will stay
;; at the end of the buffer in that window, even if the window is not
;; selected.  This way, you can use Auto-Revert Mode to `tail' a file.
;; Just put point at the end of the buffer and it will stay there.
;; These rules apply to file buffers. For non-file buffers, the
;; behavior may be mode dependent.
;;
;; While you can use Auto-Revert Mode to tail a file, this package
;; contains a third minor mode, Auto-Revert Tail Mode, which does so
;; more efficiently, as long as you are sure that the file will only
;; change by growing at the end.  It only appends the new output,
;; instead of reverting the entire buffer.  It does so even if the
;; buffer contains unsaved changes.  (Because they will not be lost.)

;; Usage:
;;
;; Go to the appropriate buffer and press either of:
;;   M-x auto-revert-mode RET
;;   M-x auto-revert-tail-mode RET
;;
;; To activate Global Auto-Revert Mode, press:
;;   M-x global-auto-revert-mode RET
;;
;; To activate Global Auto-Revert Mode every time Emacs is started
;; customize the option `global-auto-revert-mode' or the following
;; line could be added to your ~/.emacs:
;;   (global-auto-revert-mode 1)
;;
;; The function `turn-on-auto-revert-mode' could be added to any major
;; mode hook to activate Auto-Revert Mode for all buffers in that
;; mode.  For example, the following line will activate Auto-Revert
;; Mode in all C mode buffers:
;;
;; (add-hook 'c-mode-hook #'turn-on-auto-revert-mode)

;;; Code:

;; Dependencies:

(eval-when-compile (require 'cl-lib))
(require 'timer)
(require 'filenotify)

;; Custom Group:
;;
;; The two modes will be placed next to Auto Save Mode under the
;; Files group under Emacs.

(defgroup auto-revert nil
  "Revert individual buffers when files on disk change.
Auto-Revert Mode enables auto-revert in individual buffers.
Global Auto-Revert Mode does so in all buffers."
  :group 'files
  :group 'convenience)


;; Variables:

;;; What's this?: ;; Autoload for the benefit of `make-mode-line-mouse-sensitive'.
;;; What's this?: ;;;###autoload
(defvar auto-revert-mode nil
  "Non-nil when Auto-Revert Mode is active.
Never set this variable directly, use the command `auto-revert-mode' instead.")
(put 'auto-revert-mode 'permanent-local t)

(defvar auto-revert-tail-mode nil
  "Non-nil when Auto-Revert Tail Mode is active.
Never set this variable directly, use the command
`auto-revert-tail-mode' instead.")
(put 'auto-revert-tail-mode 'permanent-local t)

(defvar auto-revert-timer nil
  "Timer used by Auto-Revert Mode.")

(defcustom auto-revert-interval 5
  "Time, in seconds, between Auto-Revert Mode file checks.
The value may be an integer or floating point number.

If a timer is already active, there are two ways to make sure
that the new value will take effect immediately.  You can set
this variable through Custom or you can call the command
`auto-revert-set-timer' after setting the variable.  Otherwise,
the new value will take effect the first time Auto-Revert Mode
calls `auto-revert-set-timer' for internal reasons or in your
next editing session."
  :group 'auto-revert
  :type 'number
  :set (lambda (variable value)
	 (set-default variable value)
	 (and (boundp 'auto-revert-timer)
	      auto-revert-timer
	      (auto-revert-set-timer))))

(defcustom auto-revert-stop-on-user-input t
  "When non-nil, user input temporarily interrupts Auto-Revert Mode.
With this setting, Auto-Revert Mode checks for user input after
handling each buffer and does not process any further buffers
\(until the next run of the timer) if user input is available.
When nil, Auto-Revert Mode checks files and reverts buffers,
with quitting disabled, without paying attention to user input.
Thus, with this setting, Emacs might be non-responsive at times."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-verbose t
  "When nil, Auto-Revert Mode does not generate any messages.
When non-nil, a message is generated whenever a buffer is reverted."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-mode-text " ARev"
  "String to display in the mode line when Auto-Revert Mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "Auto-Revert Mode Text"		; To separate it from `global-...'
  :group 'auto-revert
  :type 'string)

(defcustom auto-revert-tail-mode-text " Tail"
  "String to display in the mode line when Auto-Revert Tail Mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :group 'auto-revert
  :type 'string
  :version "22.1")

(defcustom auto-revert-mode-hook nil
  "Functions to run when Auto-Revert Mode is activated."
  :tag "Auto-Revert Mode Hook"		; To separate it from `global-...'
  :group 'auto-revert
  :type 'hook)

(defcustom global-auto-revert-mode-text ""
  "String to display when Global Auto-Revert Mode is active.

The default is nothing since when this mode is active this text doesn't
vary over time, or between buffers.  Hence mode line text
would only waste precious space."
  :group 'auto-revert
  :type 'string)

(defcustom global-auto-revert-mode-hook nil
  "Hook called when Global Auto-Revert Mode is activated."
  :group 'auto-revert
  :type 'hook)

(defcustom global-auto-revert-non-file-buffers nil
  "When nil, Global Auto-Revert Mode operates only on file-visiting buffers.

When non-nil, both file buffers and buffers with a custom
`revert-buffer-function' and a `buffer-stale-function' are
reverted by Global Auto-Revert Mode.  These include the Buffer
List buffer displayed by `buffer-menu', and Dired buffers showing
complete local directories.  The Buffer List buffer reverts every
`auto-revert-interval' seconds; Dired buffers when the file list of
the main directory changes.  Dired buffers do not auto-revert as
a result of changes in subdirectories, or in the contents, size,
modes, etc., of files.  You may still sometimes want to revert
them manually.

Use this option with care since it could lead to excessive auto-reverts.
For more information, see Info node `(emacs)Autorevert'."
  :group 'auto-revert
  :type 'boolean
  :link '(info-link "(emacs)Autorevert"))

(defcustom global-auto-revert-ignore-modes ()
  "List of major modes Global Auto-Revert Mode should not check."
  :group 'auto-revert
  :type '(repeat sexp))

(defcustom auto-revert-load-hook nil
  "Functions to run when Auto-Revert Mode is first loaded."
  :tag "Load Hook"
  :group 'auto-revert
  :type 'hook)

(defcustom auto-revert-check-vc-info nil
  "If non-nil Auto-Revert Mode reliably updates version control info.
Auto-Revert Mode updates version control info whenever the buffer
needs reverting, regardless of the value of this variable.
However, the version control state can change without changes to
the work file.  If the change is made from the current Emacs
session, all info is updated.  But if, for instance, a new
version is checked in from outside the current Emacs session, the
version control number in the mode line, as well as other version
control related information, may not be properly updated.  If you
are worried about this, set this variable to a non-nil value.

This currently works by automatically updating the version
control info every `auto-revert-interval' seconds.  Nevertheless,
it should not cause excessive CPU usage on a reasonably fast
machine, if it does not apply to too many version controlled
buffers.  CPU usage depends on the version control system."
  :group 'auto-revert
  :type 'boolean
  :version "22.1")

(defvar-local global-auto-revert-ignore-buffer nil
  "When non-nil, Global Auto-Revert Mode will not revert this buffer.
This variable becomes buffer local when set in any fashion.")

(defcustom auto-revert-remote-files nil
  "If non-nil remote files are also reverted."
  :group 'auto-revert
  :type 'boolean
  :version "24.4")

(defcustom auto-revert-use-notify t
  "If non-nil Auto-Revert Mode uses file notification functions.
You should set this variable through Custom."
  :group 'auto-revert
  :type 'boolean
  :set (lambda (variable value)
	 (set-default variable value)
	 (unless (symbol-value variable)
	   (dolist (buf (buffer-list))
	     (with-current-buffer buf
	       (when (symbol-value 'auto-revert-notify-watch-descriptor)
		 (auto-revert-notify-rm-watch))))))
  :initialize 'custom-initialize-default
  :version "24.4")

(defcustom auto-revert-notify-exclude-dir-regexp
  (concat
   ;; No mounted file systems.
   "^" (regexp-opt '("/afs/" "/media/" "/mnt" "/net/" "/tmp_mnt/"))
   ;; No remote files.
   (unless auto-revert-remote-files "\\|^/[^/|:][^/|]+:"))
  "Regular expression of directories to be excluded from file notifications."
  :group 'auto-revert
  :type 'regexp
  :version "24.4")

;; Internal variables:

(defvar auto-revert-buffer-list ()
  "List of buffers in Auto-Revert Mode.

Note that only Auto-Revert Mode, never Global Auto-Revert Mode, adds
buffers to this list.

The timer function `auto-revert-buffers' is responsible for purging
the list of old buffers.")

(defvar auto-revert-remaining-buffers ()
  "Buffers not checked when user input stopped execution.")

(defvar auto-revert-tail-pos 0
  "Position of last known end of file.")

(defun auto-revert-find-file-function ()
  (setq-local auto-revert-tail-pos
              (nth 7 (file-attributes buffer-file-name))))

(add-hook 'find-file-hook
	  #'auto-revert-find-file-function)

(defvar auto-revert-notify-watch-descriptor-hash-list
  (make-hash-table :test 'equal)
  "A hash table collecting all file watch descriptors.
Hash key is a watch descriptor, hash value is a list of buffers
which are related to files being watched and carrying the same
default directory.")

(defvar-local auto-revert-notify-watch-descriptor nil
  "The file watch descriptor active for the current buffer.")
(put 'auto-revert-notify-watch-descriptor 'permanent-local t)

(defvar-local auto-revert-notify-modified-p nil
  "Non-nil when file has been modified on the file system.
This has been reported by a file notification event.")

;; Functions:

(defun auto-revert-remove-current-buffer ()
  "Remove dead buffer from `auto-revert-buffer-list'."
  (setq auto-revert-buffer-list
        (delq (current-buffer) auto-revert-buffer-list)))

;;;###autoload
(define-minor-mode auto-revert-mode
  "Toggle reverting buffer when the file changes (Auto-Revert Mode).
With a prefix argument ARG, enable Auto-Revert Mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Auto-Revert Mode is a minor mode that affects only the current
buffer.  When enabled, it reverts the buffer when the file on
disk changes.

When a buffer is reverted, a message is generated.  This can be
suppressed by setting `auto-revert-verbose' to nil.

Use `global-auto-revert-mode' to automatically revert all buffers.
Use `auto-revert-tail-mode' if you know that the file will only grow
without being changed in the part that is already in the buffer."
  :group 'auto-revert :lighter auto-revert-mode-text
  (if auto-revert-mode
      (when (not (memq (current-buffer) auto-revert-buffer-list))
        (push (current-buffer) auto-revert-buffer-list)
        (add-hook
         'kill-buffer-hook
         #'auto-revert-remove-current-buffer
         nil t))
    (when auto-revert-notify-watch-descriptor (auto-revert-notify-rm-watch))
    (auto-revert-remove-current-buffer))
  (auto-revert-set-timer)
  (when auto-revert-mode
    (auto-revert-buffers)
    (setq auto-revert-tail-mode nil)))


;;;###autoload
(defun turn-on-auto-revert-mode ()
  "Turn on Auto-Revert Mode.

This function is designed to be added to hooks, for example:
  (add-hook \\='c-mode-hook #\\='turn-on-auto-revert-mode)"
  (auto-revert-mode 1))


;;;###autoload
(define-minor-mode auto-revert-tail-mode
  "Toggle reverting tail of buffer when the file grows.
With a prefix argument ARG, enable Auto-Revert Tail Mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When Auto-Revert Tail Mode is enabled, the tail of the file is
constantly followed, as with the shell command `tail -f'.  This
means that whenever the file grows on disk (presumably because
some background process is appending to it from time to time),
this is reflected in the current buffer.

You can edit the buffer and turn this mode off and on again as
you please.  But make sure the background process has stopped
writing before you save the file!

When a buffer is reverted, a message is generated.  This can be
suppressed by setting `auto-revert-verbose' to nil.

Use `auto-revert-mode' for changes other than appends!"
  :group 'find-file :lighter auto-revert-tail-mode-text
  (when auto-revert-tail-mode
    (unless buffer-file-name
      (auto-revert-tail-mode 0)
      (error "This buffer is not visiting a file"))
    (if (and (buffer-modified-p)
	     (zerop auto-revert-tail-pos) ; library was loaded only after finding file
	     (not (y-or-n-p "Buffer is modified, so tail offset may be wrong.  Proceed? ")))
	(auto-revert-tail-mode 0)
      ;; a-r-tail-pos stores the size of the file at the time of the
      ;; last revert. After this package loads, it adds a
      ;; find-file-hook to set this variable every time a file is
      ;; loaded.  If the package is loaded only _after_ visiting the
      ;; file to be reverted, then we have no idea what the value of
      ;; a-r-tail-pos should have been when the file was visited.  If
      ;; the file has changed on disk in the meantime, all we can do
      ;; is offer to revert the whole thing. If you choose not to
      ;; revert, then you might miss some output then happened
      ;; between visiting the file and activating a-r-t-mode.
      (and (zerop auto-revert-tail-pos)
           (not (verify-visited-file-modtime (current-buffer)))
           (y-or-n-p "File changed on disk, content may be missing.  \
Perform a full revert? ")
           ;; Use this (not just revert-buffer) for point-preservation.
           (auto-revert-buffers))
      ;; else we might reappend our own end when we save
      (add-hook 'before-save-hook (lambda () (auto-revert-tail-mode 0)) nil t)
      (or (local-variable-p 'auto-revert-tail-pos) ; don't lose prior position
	  (setq-local auto-revert-tail-pos
                      (nth 7 (file-attributes buffer-file-name))))
      ;; let auto-revert-mode set up the mechanism for us if it isn't already
      (or auto-revert-mode
	  (let ((auto-revert-tail-mode t))
	    (auto-revert-mode 1)))
      (setq auto-revert-mode nil))))


;;;###autoload
(defun turn-on-auto-revert-tail-mode ()
  "Turn on Auto-Revert Tail Mode.

This function is designed to be added to hooks, for example:
  (add-hook \\='my-logfile-mode-hook #\\='turn-on-auto-revert-tail-mode)"
  (auto-revert-tail-mode 1))


;;;###autoload
(define-minor-mode global-auto-revert-mode
  "Toggle Global Auto-Revert Mode.
With a prefix argument ARG, enable Global Auto-Revert Mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Global Auto-Revert Mode is a global minor mode that reverts any
buffer associated with a file when the file changes on disk.  Use
`auto-revert-mode' to revert a particular buffer.

If `global-auto-revert-non-file-buffers' is non-nil, this mode
may also revert some non-file buffers, as described in the
documentation of that variable.  It ignores buffers with modes
matching `global-auto-revert-ignore-modes', and buffers with a
non-nil vale of `global-auto-revert-ignore-buffer'.

When a buffer is reverted, a message is generated.  This can be
suppressed by setting `auto-revert-verbose' to nil.

This function calls the hook `global-auto-revert-mode-hook'.
It displays the text that `global-auto-revert-mode-text'
specifies in the mode line."
  :global t :group 'auto-revert :lighter global-auto-revert-mode-text
  (auto-revert-set-timer)
  (if global-auto-revert-mode
      (auto-revert-buffers)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when auto-revert-notify-watch-descriptor
	  (auto-revert-notify-rm-watch))))))

(defun auto-revert-set-timer ()
  "Restart or cancel the timer used by Auto-Revert Mode.
If such a timer is active, cancel it.  Start a new timer if
Global Auto-Revert Mode is active or if Auto-Revert Mode is active
in some buffer.  Restarting the timer ensures that Auto-Revert Mode
will use an up-to-date value of `auto-revert-interval'"
  (interactive)
  (if (timerp auto-revert-timer)
      (cancel-timer auto-revert-timer))
  (setq auto-revert-timer
	(if (or global-auto-revert-mode auto-revert-buffer-list)
	    (run-with-timer auto-revert-interval
			    auto-revert-interval
			    'auto-revert-buffers))))

(defun auto-revert-notify-rm-watch ()
  "Disable file notification for current buffer's associated file."
  (when auto-revert-notify-watch-descriptor
    (maphash
     (lambda (key value)
       (when (equal key auto-revert-notify-watch-descriptor)
	 (setq value (delete (current-buffer) value))
	 (if value
	     (puthash key value auto-revert-notify-watch-descriptor-hash-list)
	   (remhash key auto-revert-notify-watch-descriptor-hash-list)
	   (ignore-errors
	     (file-notify-rm-watch auto-revert-notify-watch-descriptor)))))
     auto-revert-notify-watch-descriptor-hash-list)
    (remove-hook 'kill-buffer-hook #'auto-revert-notify-rm-watch))
  (setq auto-revert-notify-watch-descriptor nil
	auto-revert-notify-modified-p nil))

(defun auto-revert-notify-add-watch ()
  "Enable file notification for current buffer's associated file."
  ;; We can assume that `buffer-file-name' and
  ;; `auto-revert-notify-watch-descriptor' are non-nil.
  (unless (or auto-revert-notify-watch-descriptor
              (string-match auto-revert-notify-exclude-dir-regexp
			    (expand-file-name default-directory))
	      (file-symlink-p (or buffer-file-name default-directory)))
    (setq auto-revert-notify-watch-descriptor
	  (ignore-errors
	    (if buffer-file-name
		(file-notify-add-watch
		 (expand-file-name buffer-file-name default-directory)
		 '(change attribute-change)
		 'auto-revert-notify-handler)
	      (file-notify-add-watch
	       (expand-file-name default-directory)
	       '(change)
	       'auto-revert-notify-handler))))
    (when auto-revert-notify-watch-descriptor
      (setq auto-revert-notify-modified-p t)
      (puthash
      auto-revert-notify-watch-descriptor
       (cons (current-buffer)
	     (gethash auto-revert-notify-watch-descriptor
		      auto-revert-notify-watch-descriptor-hash-list))
       auto-revert-notify-watch-descriptor-hash-list)
      (add-hook 'kill-buffer-hook #'auto-revert-notify-rm-watch nil t))))

;; If we have file notifications, we want to update the auto-revert buffers
;; immediately when a notification occurs. Since file updates can happen very
;; often, we want to skip some revert operations so that we don't spend all our
;; time reverting the buffer.
;;
;; We do this by reverting immediately in response to the first in a flurry of
;; notifications. We suppress subsequent notifications until the next time
;; `auto-revert-buffers' is called (this happens on a timer with a period set by
;; `auto-revert-interval').
(defvar auto-revert-buffers-counter 1
  "Incremented each time `auto-revert-buffers' is called")
(defvar-local auto-revert-buffers-counter-lockedout 0
  "Buffer-local value to indicate whether we should immediately
update the buffer on a notification event or not. If

  (= auto-revert-buffers-counter-lockedout
     auto-revert-buffers-counter)

then the updates are locked out, and we wait until the next call
of `auto-revert-buffers' to revert the buffer. If no lockout is
present, then we revert immediately and set the lockout, so that
no more reverts are possible until the next call of
`auto-revert-buffers'")

(defun auto-revert-notify-handler (event)
  "Handle an EVENT returned from file notification."
  (with-demoted-errors
    (let* ((descriptor (car event))
	   (action (nth 1 event))
	   (file (nth 2 event))
	   (file1 (nth 3 event)) ;; Target of `renamed'.
	   (buffers (gethash descriptor
			     auto-revert-notify-watch-descriptor-hash-list)))
      ;; Check, that event is meant for us.
      (cl-assert descriptor)
      ;; Since we watch a directory, a file name must be returned.
      (cl-assert (stringp file))
      (when (eq action 'renamed) (cl-assert (stringp file1)))

      (if (eq action 'stopped)
          ;; File notification has stopped.  Continue with polling.
          (cl-dolist (buffer
                      (if global-auto-revert-mode
                          (buffer-list) auto-revert-buffer-list))
            (with-current-buffer buffer
              (when (and (equal descriptor auto-revert-notify-watch-descriptor)
                         (or
                          ;; A buffer associated with a file.
                          (and (stringp buffer-file-name)
                               (string-equal
                                (file-name-nondirectory file)
                                (file-name-nondirectory buffer-file-name)))
                          ;; A buffer w/o a file, like dired.
                          (null buffer-file-name)))
                (auto-revert-notify-rm-watch))))

        ;; Loop over all buffers, in order to find the intended one.
        (cl-dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (or
                     ;; A buffer associated with a file.
                     (and (stringp buffer-file-name)
                          (or
                           (and (memq
                                 action '(attribute-changed changed created))
                                (string-equal
                                 (file-name-nondirectory file)
                                 (file-name-nondirectory buffer-file-name)))
                           (and (eq action 'renamed)
                                (string-equal
                                 (file-name-nondirectory file1)
                                 (file-name-nondirectory buffer-file-name)))))
                     ;; A buffer w/o a file, like dired.
                     (and (null buffer-file-name)
                          (memq action '(created renamed deleted))))
                ;; Mark buffer modified.
                (setq auto-revert-notify-modified-p t)

                ;; Revert the buffer now if we're not locked out.
                (when (/= auto-revert-buffers-counter-lockedout
                          auto-revert-buffers-counter)
                  (auto-revert-handler)
                  (setq auto-revert-buffers-counter-lockedout
                        auto-revert-buffers-counter))

                ;; No need to check other buffers.
                (cl-return)))))))))

(defun auto-revert-active-p ()
  "Check if auto-revert is active (in current buffer or globally)."
  (or auto-revert-mode
      auto-revert-tail-mode
      (and global-auto-revert-mode
           (not global-auto-revert-ignore-buffer)
           (not (memq major-mode global-auto-revert-ignore-modes)))))

(defun auto-revert-handler ()
  "Revert current buffer, if appropriate.
This is an internal function used by Auto-Revert Mode."
  (let* ((buffer (current-buffer)) size
         ;; Tramp caches the file attributes.  Setting
         ;; `remote-file-name-inhibit-cache' forces Tramp to reread
         ;; the values.
         (remote-file-name-inhibit-cache t)
         (revert
          (if buffer-file-name
              (and (or auto-revert-remote-files
                       (not (file-remote-p buffer-file-name)))
                   (or (not auto-revert-notify-watch-descriptor)
                       auto-revert-notify-modified-p)
                   (if auto-revert-tail-mode
                       (and (file-readable-p buffer-file-name)
                            (/= auto-revert-tail-pos
                                (setq size
                                      (nth 7 (file-attributes
                                              buffer-file-name)))))
                     (funcall (or buffer-stale-function
                                  #'buffer-stale--default-function)
                              t)))
            (and (or auto-revert-mode
                     global-auto-revert-non-file-buffers)
                 (funcall (or buffer-stale-function
                              #'buffer-stale--default-function)
                          t))))
         eob eoblist)
    (setq auto-revert-notify-modified-p nil)
    (when revert
      (when (and auto-revert-verbose
                 (not (eq revert 'fast)))
        (message "Reverting buffer `%s'." (buffer-name)))
      ;; If point (or a window point) is at the end of the buffer, we
      ;; want to keep it at the end after reverting.  This allows one
      ;; to tail a file.
      (when buffer-file-name
        (setq eob (eobp))
        (walk-windows
         (lambda (window)
           (and (eq (window-buffer window) buffer)
                (= (window-point window) (point-max))
                (push window eoblist)))
         'no-mini t))
      (if auto-revert-tail-mode
          (auto-revert-tail-handler size)
        ;; Bind buffer-read-only in case user has done C-x C-q, so as
        ;; not to forget that.  This gives undesirable results when
        ;; the file's mode changes, but that is less common.
        (let ((buffer-read-only buffer-read-only))
          ;; Bug#23276: When the file has been deleted, keep the
          ;; buffer unchanged.
          (ignore-errors
            (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes))))
      (when buffer-file-name
        (when eob (goto-char (point-max)))
        (dolist (window eoblist)
          (set-window-point window (point-max)))))
    ;; `preserve-modes' avoids changing the (minor) modes.  But we do
    ;; want to reset the mode for VC, so we do it manually.
    (when (or revert auto-revert-check-vc-info)
      (let ((revert-buffer-in-progress-p t))
        (vc-refresh-state)))))

(defun auto-revert-tail-handler (size)
  (let ((modified (buffer-modified-p))
	(inhibit-read-only t)		; Ignore.
	(file buffer-file-name)
	(buffer-file-name nil))		; Ignore that file has changed.
    (when (/= auto-revert-tail-pos size)
      (run-hooks 'before-revert-hook)
      (undo-boundary)
      (save-restriction
	(widen)
	(save-excursion
	  (goto-char (point-max))
	  (insert-file-contents file nil
				(and (< auto-revert-tail-pos size)
				     auto-revert-tail-pos)
				size)))
      (run-hooks 'after-revert-hook)
      (undo-boundary)
      (setq auto-revert-tail-pos size)
      (restore-buffer-modified-p modified)))
  (set-visited-file-modtime))

(defun auto-revert-buffers ()
  "Revert buffers as specified by Auto-Revert and Global Auto-Revert Mode.

Should `global-auto-revert-mode' be active all file buffers are checked.

Should `auto-revert-mode' be active in some buffers, those buffers
are checked.

Non-file buffers that have a custom `revert-buffer-function' and
`buffer-stale-function' are reverted either when Auto-Revert
Mode is active in that buffer, or when the variable
`global-auto-revert-non-file-buffers' is non-nil and Global
Auto-Revert Mode is active.

This function stops whenever there is user input.  The buffers not
checked are stored in the variable `auto-revert-remaining-buffers'.

To avoid starvation, the buffers in `auto-revert-remaining-buffers'
are checked first the next time this function is called.

This function is also responsible for removing buffers no longer in
Auto-Revert Mode from `auto-revert-buffer-list', and for canceling
the timer when no buffers need to be checked."

  (setq auto-revert-buffers-counter
        (1+ auto-revert-buffers-counter))

  (save-match-data
    (let ((bufs (if global-auto-revert-mode
		    (buffer-list)
		  auto-revert-buffer-list))
	  remaining new)
      ;; Partition `bufs' into two halves depending on whether or not
      ;; the buffers are in `auto-revert-remaining-buffers'.  The two
      ;; halves are then re-joined with the "remaining" buffers at the
      ;; head of the list.
      (dolist (buf auto-revert-remaining-buffers)
	(if (memq buf bufs)
	    (push buf remaining)))
      (dolist (buf bufs)
	(if (not (memq buf remaining))
	    (push buf new)))
      (setq bufs (nreverse (nconc new remaining)))
      (while (and bufs
		  (not (and auto-revert-stop-on-user-input
			    (input-pending-p))))
	(let ((buf (car bufs)))
          (with-current-buffer buf
            (if (buffer-live-p buf)
                (progn
                  ;; Test if someone has turned off Auto-Revert Mode
                  ;; in a non-standard way, for example by changing
                  ;; major mode.
                  (if (and (not auto-revert-mode)
                           (not auto-revert-tail-mode)
                           (memq buf auto-revert-buffer-list))
                      (auto-revert-remove-current-buffer))
                  (when (auto-revert-active-p)
                    ;; Enable file notification.
                    (when (and auto-revert-use-notify
                               (not auto-revert-notify-watch-descriptor))
                      (auto-revert-notify-add-watch))
                    (auto-revert-handler)))
              ;; Remove dead buffer from `auto-revert-buffer-list'.
              (auto-revert-remove-current-buffer))))
	(setq bufs (cdr bufs)))
      (setq auto-revert-remaining-buffers bufs)
      ;; Check if we should cancel the timer.
      (when (and (not global-auto-revert-mode)
		 (null auto-revert-buffer-list))
        (if (timerp auto-revert-timer)
	    (cancel-timer auto-revert-timer))
	(setq auto-revert-timer nil)))))


;; The end:
(provide 'autorevert)

(run-hooks 'auto-revert-load-hook)

;;; autorevert.el ends here
