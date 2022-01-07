;;; autorevert.el --- revert buffers when files on disk change  -*- lexical-binding:t -*-

;; Copyright (C) 1997-1999, 2001-2022 Free Software Foundation, Inc.

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
;; Auto-Revert Mode applies to all file buffers.  (If the user option
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
;; These rules apply to file buffers.  For non-file buffers, the
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

(require 'cl-lib)
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
For more information, see Info node `(emacs)Auto Revert'."
  :group 'auto-revert
  :type 'boolean
  :link '(info-link "(emacs)Auto Revert"))

(defcustom global-auto-revert-ignore-modes ()
  "List of major modes Global Auto-Revert Mode should not check."
  :group 'auto-revert
  :type '(repeat sexp))

(defcustom auto-revert-load-hook nil
  "Functions to run when Auto-Revert Mode is first loaded."
  :tag "Load Hook"
  :group 'auto-revert
  :type 'hook)
(make-obsolete-variable 'auto-revert-load-hook
                        "use `with-eval-after-load' instead." "28.1")

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
This variable can also be a predicate function, in which case
it'll be called with one parameter (the buffer in question), and
it should return non-nil to make Global Auto-Revert Mode not
revert this buffer.")

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

(defcustom auto-revert-avoid-polling nil
  "Non-nil to avoid polling files when notification is available.

Set this variable to a non-nil value to save power by avoiding
polling when possible.  Files on file-systems that do not support
change notifications must match `auto-revert-notify-exclude-dir-regexp'
for Auto-Revert to work properly in this case.  This typically
includes files on network file systems on Unix-like machines,
when those files are modified from another computer.

When nil, buffers in Auto-Revert Mode will always be polled for
changes to their files on disk every `auto-revert-interval'
seconds, in addition to using notification for those files."
  :group 'auto-revert
  :type 'boolean
  :set (lambda (variable value)
         (set-default variable value)
	 (when (fboundp 'auto-revert-set-timer)
           (auto-revert-set-timer)))
  :version "27.1")

;; Internal variables:

(defvar auto-revert-buffer-list ()
  "List of buffers in Auto-Revert Mode.

Note that only Auto-Revert Mode, never Global Auto-Revert Mode, adds
buffers to this list.

The timer function `auto-revert-buffers' is responsible for purging
the list of old buffers.")

(defvar-local auto-revert--global-mode nil
  "Non-nil if buffer is handled by Global Auto-Revert mode.")

(defvar auto-revert-remaining-buffers ()
  "Buffers not checked when user input stopped execution.")

(defvar auto-revert-tail-pos 0
  "Position of last known end of file.")

(defun auto-revert-find-file-function ()
  (setq-local auto-revert-tail-pos
              (file-attribute-size (file-attributes buffer-file-name))))

(add-hook 'find-file-hook
	  #'auto-revert-find-file-function)
(add-hook 'after-set-visited-file-name-hook
          #'auto-revert-set-visited-file-name)

(defvar auto-revert--buffer-by-watch-descriptor nil
  "An association list mapping notification descriptors to buffers.
The buffer uses that descriptor for auto-revert notifications.
The key is equal to `auto-revert-notify-watch-descriptor' in each
buffer.")

(defvar-local auto-revert-notify-watch-descriptor nil
  "The file watch descriptor active for the current buffer.")
(put 'auto-revert-notify-watch-descriptor 'permanent-local t)

(defvar-local auto-revert-notify-modified-p nil
  "Non-nil when file has been modified on the file system.
This has been reported by a file notification event.")

(defvar auto-revert-debug nil
  "Use for debug messages.")

;; Functions:

(defun auto-revert-remove-current-buffer (&optional buffer)
  "Remove BUFFER from `auto-revert-buffer-list'.
BUFFER defaults to `current-buffer'."
  (setq auto-revert-buffer-list
        (delq (or buffer (current-buffer)) auto-revert-buffer-list)))

;;;###autoload
(define-minor-mode auto-revert-mode
  "Toggle reverting buffer when the file changes (Auto-Revert Mode).

Auto-Revert Mode is a minor mode that affects only the current
buffer.  When enabled, it reverts the buffer when the file on
disk changes.

When a buffer is reverted, a message is generated.  This can be
suppressed by setting `auto-revert-verbose' to nil.

Reverting can sometimes fail to preserve all the markers in the buffer.
To avoid that, set `revert-buffer-insert-file-contents-function' to
the slower function `revert-buffer-insert-file-contents-delicately'.

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
                      (file-attribute-size
                       (file-attributes buffer-file-name))))
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

Global Auto-Revert Mode is a global minor mode that reverts any
buffer associated with a file when the file changes on disk.  Use
`auto-revert-mode' to revert a particular buffer.

If `global-auto-revert-non-file-buffers' is non-nil, this mode
may also revert some non-file buffers, as described in the
documentation of that variable.  It ignores buffers with modes
matching `global-auto-revert-ignore-modes', and buffers with a
non-nil value of `global-auto-revert-ignore-buffer'.

When a buffer is reverted, a message is generated.  This can be
suppressed by setting `auto-revert-verbose' to nil.

This function calls the hook `global-auto-revert-mode-hook'.
It displays the text that `global-auto-revert-mode-text'
specifies in the mode line."
  :global t :group 'auto-revert :lighter global-auto-revert-mode-text
  (auto-revert-set-timer)
  (if global-auto-revert-mode
      ;; Turn global-auto-revert-mode ON.
      (progn
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (auto-revert--global-add-current-buffer)))
        ;; Make sure future buffers are added as well.
        (add-hook 'find-file-hook #'auto-revert--global-adopt-current-buffer)
        ;; To track non-file buffers, we need to listen in to buffer
        ;; creation in general.  Listening to major-mode changes is
        ;; suitable, since we then know whether it's a mode that is tracked.
        (add-hook 'after-change-major-mode-hook
                  #'auto-revert--global-possibly-adopt-current-buffer)
        (auto-revert-buffers))
    ;; Turn global-auto-revert-mode OFF.
    (remove-hook 'after-change-major-mode-hook
                 #'auto-revert--global-possibly-adopt-current-buffer)
    (remove-hook 'find-file-hook #'auto-revert--global-adopt-current-buffer)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when auto-revert--global-mode
          (setq auto-revert--global-mode nil)
          (when (and auto-revert-notify-watch-descriptor
                     (not (or auto-revert-mode auto-revert-tail-mode)))
	    (auto-revert-notify-rm-watch)))))))

(defun auto-revert--global-add-current-buffer ()
  "Set current buffer to be tracked by Global Auto-Revert if appropriate."
  (when (and (not auto-revert--global-mode)
             (or buffer-file-name
                 (and global-auto-revert-non-file-buffers
                      (not (string-prefix-p " " (buffer-name)))
                      ;; Any non-file buffer must have a custom
                      ;; `buffer-stale-function' to be tracked, since
                      ;; we wouldn't know when to revert it otherwise.
                      (not (eq buffer-stale-function
                               #'buffer-stale--default-function))))
             (not (memq major-mode global-auto-revert-ignore-modes))
             (or (null global-auto-revert-ignore-buffer)
                 (if (functionp global-auto-revert-ignore-buffer)
                     (not (funcall global-auto-revert-ignore-buffer
                                   (current-buffer)))
                   nil)))
    (setq auto-revert--global-mode t)))

(defun auto-revert--global-possibly-adopt-current-buffer ()
  "Consider tracking current buffer in a running Global Auto-Revert mode.
This tracks buffers if `global-auto-revert-non-file-buffers' is non-nil."
  (when global-auto-revert-non-file-buffers
    (auto-revert--global-adopt-current-buffer)))

(defun auto-revert--global-adopt-current-buffer ()
  "Consider tracking current buffer in a running Global Auto-Revert mode."
  (auto-revert--global-add-current-buffer)
  (auto-revert-set-timer))

(defun auto-revert-set-visited-file-name ()
  "Update Auto-Revert management of the current buffer.
Called after `set-visited-file-name'."
  (when auto-revert-notify-watch-descriptor
    ;; Remove any existing notifier so that we don't track the wrong
    ;; file in case the file name was changed.
    (auto-revert-notify-rm-watch))
    (cond (global-auto-revert-mode
           (auto-revert--global-adopt-current-buffer))
          ((or auto-revert-mode auto-revert-tail-mode)
           (auto-revert-set-timer))))

(defun auto-revert--polled-buffers ()
  "List of buffers that need to be polled."
  (cond (global-auto-revert-mode
         (mapcan (lambda (buffer)
                   (and (not (and auto-revert-avoid-polling
                                  (buffer-local-value
                                   'auto-revert-notify-watch-descriptor
                                   buffer)))
                        (or (buffer-local-value
                             'auto-revert--global-mode buffer)
                            (buffer-local-value 'auto-revert-mode buffer)
                            (buffer-local-value 'auto-revert-tail-mode buffer))
                        (list buffer)))
                 (buffer-list)))
        (auto-revert-avoid-polling
         (mapcan (lambda (buffer)
                   (and (not (buffer-local-value
                              'auto-revert-notify-watch-descriptor buffer))
                        (list buffer)))
                 auto-revert-buffer-list))
        (t auto-revert-buffer-list)))

;; Same as above in a boolean context, but cheaper.
(defun auto-revert--need-polling-p ()
  "Whether periodic polling is required."
  (cond (global-auto-revert-mode
         (or (not auto-revert-avoid-polling)
             (cl-some
              (lambda (buffer)
                (and (not (buffer-local-value
                           'auto-revert-notify-watch-descriptor buffer))
                     (or (buffer-local-value 'auto-revert--global-mode buffer)
                         (buffer-local-value 'auto-revert-mode buffer)
                         (buffer-local-value 'auto-revert-tail-mode buffer))))
              (buffer-list))))
        (auto-revert-avoid-polling
         (not (cl-every
               (lambda (buffer)
                 (buffer-local-value
                  'auto-revert-notify-watch-descriptor buffer))
               auto-revert-buffer-list)))
        (t auto-revert-buffer-list)))

(defun auto-revert-set-timer ()
  "Restart or cancel the timer used by Auto-Revert Mode.
If such a timer is active, cancel it.  Start a new timer if
Global Auto-Revert Mode is active or if Auto-Revert Mode is active
in some buffer.  Restarting the timer ensures that Auto-Revert Mode
will use an up-to-date value of `auto-revert-interval'."
  (interactive)
  (if (timerp auto-revert-timer)
      (cancel-timer auto-revert-timer))
  (setq auto-revert-timer
	(and (auto-revert--need-polling-p)
	     (run-with-timer auto-revert-interval
			     auto-revert-interval
			     'auto-revert-buffers))))

(defun auto-revert-notify-rm-watch ()
  "Disable file notification for current buffer's associated file."
  (when-let ((desc auto-revert-notify-watch-descriptor))
    (setq auto-revert--buffer-by-watch-descriptor
          (assoc-delete-all desc auto-revert--buffer-by-watch-descriptor))
    (ignore-errors
      (file-notify-rm-watch desc))
    (remove-hook 'kill-buffer-hook #'auto-revert-notify-rm-watch t))
  (setq auto-revert-notify-watch-descriptor nil
	auto-revert-notify-modified-p nil))

(defun auto-revert-notify-add-watch ()
  "Enable file notification for current buffer's associated file."
  ;; We can assume that `auto-revert-notify-watch-descriptor' is nil.
  (unless (or auto-revert-notify-watch-descriptor
              (string-match auto-revert-notify-exclude-dir-regexp
			    (expand-file-name default-directory))
	      (file-symlink-p (or buffer-file-name default-directory)))
    (let ((file (if buffer-file-name
		    (expand-file-name buffer-file-name default-directory)
	          (expand-file-name default-directory))))
      (setq auto-revert-notify-watch-descriptor
	    (ignore-errors
	      (file-notify-add-watch
	       file
               (if buffer-file-name '(change attribute-change) '(change))
               'auto-revert-notify-handler))))
      (when auto-revert-notify-watch-descriptor
        (setq auto-revert-notify-modified-p t
              auto-revert--buffer-by-watch-descriptor
              (cons (cons auto-revert-notify-watch-descriptor (current-buffer))
                    auto-revert--buffer-by-watch-descriptor))
        (add-hook 'kill-buffer-hook #'auto-revert-notify-rm-watch nil t))))

;; If we have file notifications, we want to update the auto-revert buffers
;; immediately when a notification occurs. Since file updates can happen very
;; often, we want to skip some revert operations so that we don't spend all our
;; time reverting the buffer.
;;
;; We do this by reverting immediately in response to the first in a
;; flurry of notifications. Any notifications during the following
;; `auto-revert-lockout-interval' seconds are noted but not acted upon
;; until the end of that interval.

(defconst auto-revert--lockout-interval 2.5
  "Duration, in seconds, of the Auto-Revert Mode notification lockout.
This is the quiescence after each notification of a file being
changed during which no automatic reverting takes place, to
prevent many updates in rapid succession from overwhelming the
system.")

(defvar-local auto-revert--lockout-timer nil
  "Timer awaiting the end of the notification lockout interval, or nil.")

(defun auto-revert-notify-handler (event)
  "Handle an EVENT returned from file notification."
  (with-demoted-errors
    (let* ((descriptor (car event))
	   (action (nth 1 event))
	   (file (nth 2 event))
	   (file1 (nth 3 event)) ;; Target of `renamed'.
	   (buffer (alist-get descriptor auto-revert--buffer-by-watch-descriptor
                              nil nil #'equal)))
      ;; Check, that event is meant for us.
      (cl-assert descriptor)
      ;; Since we watch a directory, a file name must be returned.
      (cl-assert (stringp file))
      (when (eq action 'renamed) (cl-assert (stringp file1)))
      (when auto-revert-debug
        (message "auto-revert-notify-handler %S" event))

      (when (buffer-live-p buffer)
        (if (eq action 'stopped)
            ;; File notification has stopped.  Continue with polling.
            (with-current-buffer buffer
              (when (or
                     ;; A buffer associated with a file.
                     (and (stringp buffer-file-name)
                          (string-equal
                           (file-name-nondirectory file)
                           (file-name-nondirectory buffer-file-name)))
                     ;; A buffer w/o a file, like dired.
                     (null buffer-file-name))
                (auto-revert-notify-rm-watch)
                ;; Restart the timer if it wasn't running.
                (unless auto-revert-timer
                  (auto-revert-set-timer))))

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
              (unless auto-revert--lockout-timer
                (auto-revert-handler)
                (setq auto-revert--lockout-timer
                      (run-with-timer
                       auto-revert--lockout-interval nil
                       #'auto-revert--end-lockout buffer))))))))))

(defun auto-revert--end-lockout (buffer)
  "End the lockout period after a notification.
If the buffer needs to be reverted, do it now."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq auto-revert--lockout-timer nil)
      (when auto-revert-notify-modified-p
        (auto-revert-handler)))))

(defun auto-revert-active-p ()
  "Check if auto-revert is active in current buffer."
  (or auto-revert-mode
      auto-revert-tail-mode
      auto-revert--global-mode))

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
                                      (file-attribute-size
                                       (file-attributes buffer-file-name)))))
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

(defun auto-revert--buffer-candidates ()
  "Return a prioritized list of buffers to maybe auto-revert.
The differences between this return value and the reference
variable `auto-revert-buffer-list' include: 1) this has more
entries when in `global-auto-revert-mode'; 2) this prioritizes
buffers not reverted last time due to user interruption."
  (let ((bufs (delq nil
                    ;; Buffers with remote contents shall be reverted only
                    ;; if the connection is established already.
                    (mapcar
                     (lambda (buf)
                       (and (buffer-live-p buf)
                            (with-current-buffer buf
                              (and
                               (or (not (file-remote-p default-directory))
                                   (file-remote-p default-directory nil t))
                               buf))))
                     (auto-revert--polled-buffers))))
        remaining new)
    ;; Partition `bufs' into two halves depending on whether or not
    ;; the buffers are in `auto-revert-remaining-buffers'.  The two
    ;; halves are then re-joined with the "remaining" buffers at the
    ;; head of the list.
    (dolist (buf auto-revert-remaining-buffers)
      (when (memq buf bufs)
        (push buf remaining)))
    (dolist (buf bufs)
      (unless (memq buf remaining)
        (push buf new)))
    (nreverse (nconc new remaining))))

(defun auto-revert-buffer (buf)
  "Revert a single buffer BUF.

This is performed as specified by Auto-Revert and Global
Auto-Revert Modes."
  (if (not (buffer-live-p buf))
      (auto-revert-remove-current-buffer buf)
    (with-current-buffer buf
      ;; Test if someone has turned off Auto-Revert Mode
      ;; in a non-standard way, for example by changing
      ;; major mode.
      (when (and (not auto-revert-mode)
                 (not auto-revert-tail-mode))
        (auto-revert-remove-current-buffer))
      (when (auto-revert-active-p)
        ;; Enable file notification.
        ;; Don't bother creating a notifier for non-file buffers
        ;; unless it explicitly indicates that this works.
        (when (and auto-revert-use-notify
                   (not auto-revert-notify-watch-descriptor)
                   (or buffer-file-name
                       buffer-auto-revert-by-notification))
          (auto-revert-notify-add-watch))
        (auto-revert-handler)))))

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
  (save-match-data
    (let ((bufs (auto-revert--buffer-candidates)))
      (while (and bufs
		  (not (and auto-revert-stop-on-user-input
			    (input-pending-p))))
        (auto-revert-buffer (pop bufs)))
      (setq auto-revert-remaining-buffers bufs)
      ;; Check if we should cancel the timer.
      (unless (auto-revert--need-polling-p)
        (when (timerp auto-revert-timer)
          (cancel-timer auto-revert-timer))
	(setq auto-revert-timer nil)))))

(provide 'autorevert)

(run-hooks 'auto-revert-load-hook)

;;; autorevert.el ends here
