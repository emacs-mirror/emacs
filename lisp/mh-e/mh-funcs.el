;;; mh-funcs.el --- MH-E functions not everyone will use right away

;; Copyright (C) 1993, 1995, 2001, 02, 2003 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Internal support for MH-E package.
;; Putting these functions in a separate file lets MH-E start up faster,
;; since less Lisp code needs to be loaded all at once.

;;; Change Log:

;;; Code:

(require 'mh-e)

;;; Customization

(defvar mh-sortm-args nil
  "Extra arguments to have \\[mh-sort-folder] pass to the \"sortm\" command.
The arguments are passed to sortm if \\[mh-sort-folder] is given a
prefix argument.  Normally default arguments to sortm are specified in the
MH profile.
For example, '(\"-nolimit\" \"-textfield\" \"subject\") is a useful setting.")

(defvar mh-note-copied "C"
  "String whose first character is used to notate copied messages.")

(defvar mh-note-printed "P"
  "String whose first character is used to notate printed messages.")

;;; Functions

;;;###mh-autoload
(defun mh-burst-digest ()
  "Burst apart the current message, which should be a digest.
The message is replaced by its table of contents and the messages from the
digest are inserted into the folder after that message."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (mh-process-or-undo-commands mh-current-folder)
    (mh-set-folder-modified-p t)        ; lock folder while bursting
    (message "Bursting digest...")
    (mh-exec-cmd "burst" mh-current-folder digest "-inplace")
    (with-mh-folder-updating (t)
      (beginning-of-line)
      (delete-region (point) (point-max)))
    (mh-regenerate-headers (format "%d-last" digest) t)
    (mh-goto-cur-msg)
    (message "Bursting digest...done")))

;;;###mh-autoload
(defun mh-copy-msg (msg-or-seq folder)
  "Copy the specified MSG-OR-SEQ to another FOLDER without deleting them.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is copied.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence."
  (interactive (list (mh-interactive-msg-or-seq "Copy")
                     (mh-prompt-for-folder "Copy to" "" t)))
  (let ((msg-list (let ((result ()))
                    (mh-iterate-on-msg-or-seq msg msg-or-seq
                      (mh-notate nil mh-note-copied mh-cmd-note)
                      (push msg result))
                    result)))
    (mh-exec-cmd "refile" (mh-coalesce-msg-list msg-list)
                 "-link" "-src" mh-current-folder folder)))

;;;###mh-autoload
(defun mh-kill-folder ()
  "Remove the current folder and all included messages.
Removes all of the messages (files) within the specified current folder,
and then removes the folder (directory) itself."
  (interactive)
  (if (or mh-index-data
          (yes-or-no-p (format "Remove folder %s (and all included messages)? "
                               mh-current-folder)))
      (let ((folder mh-current-folder)
            (window-config mh-previous-window-config))
        (mh-set-folder-modified-p t)    ; lock folder to kill it
        (mh-exec-cmd-daemon "rmf" 'mh-rmf-daemon folder)
        (when (boundp 'mh-speed-folder-map)
          (mh-speed-invalidate-map folder))
        (mh-remove-from-sub-folders-cache folder)
        (mh-set-folder-modified-p nil)  ; so kill-buffer doesn't complain
        (if (and mh-show-buffer (get-buffer mh-show-buffer))
            (kill-buffer mh-show-buffer))
        (if (get-buffer folder)
            (kill-buffer folder))
        (when window-config
          (set-window-configuration window-config))
        (message "Folder %s removed" folder))
    (message "Folder not removed")))

(defun mh-rmf-daemon (process output)
  "The rmf PROCESS puts OUTPUT in temporary buffer.
Display the results only if something went wrong."
  (set-buffer (get-buffer-create mh-temp-buffer))
  (insert-before-markers output)
  (when (save-excursion
          (beginning-of-buffer)
          (re-search-forward "^rmf: " (point-max) t))
    (display-buffer mh-temp-buffer)))

;; Avoid compiler warning...
(defvar view-exit-action)

;;;###mh-autoload
(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (let ((temp-buffer mh-folders-buffer))
    (with-output-to-temp-buffer temp-buffer
      (save-excursion
        (set-buffer temp-buffer)
        (erase-buffer)
        (message "Listing folders...")
        (mh-exec-cmd-output "folders" t (if mh-recursive-folders-flag
                                            "-recurse"
                                          "-norecurse"))
        (goto-char (point-min))
        (view-mode 1)
        (setq view-exit-action 'kill-buffer)
        (message "Listing folders...done")))))

;;;###mh-autoload
(defun mh-pack-folder (range)
  "Renumber the messages of a folder to be 1..n.
First, offer to execute any outstanding commands for the current folder. If
optional prefix argument provided, prompt for the RANGE of messages to display
after packing. Otherwise, show the entire folder."
  (interactive (list (if current-prefix-arg
                         (mh-read-msg-range mh-current-folder t)
                       '("all"))))
  (let ((threaded-flag (memq 'unthread mh-view-ops)))
    (mh-pack-folder-1 range)
    (mh-goto-cur-msg)
    (when mh-index-data
      (mh-index-update-maps mh-current-folder))
    (cond (threaded-flag (mh-toggle-threads))
          (mh-index-data (mh-index-insert-folder-headers))))
  (message "Packing folder...done"))

(defun mh-pack-folder-1 (range)
  "Close and pack the current folder.
Display the given RANGE of messages after packing. If RANGE is nil, show the
entire folder."
  (mh-process-or-undo-commands mh-current-folder)
  (message "Packing folder...")
  (mh-set-folder-modified-p t)          ; lock folder while packing
  (save-excursion
    (mh-exec-cmd-quiet t "folder" mh-current-folder "-pack"
                       "-norecurse" "-fast"))
  (mh-reset-threads-and-narrowing)
  (mh-regenerate-headers range))

;;;###mh-autoload
(defun mh-pipe-msg (command include-headers)
  "Pipe the current message through the given shell COMMAND.
If INCLUDE-HEADERS (prefix argument) is provided, send the entire message.
Otherwise just send the message's body without the headers."
  (interactive
   (list (read-string "Shell command on message: ") current-prefix-arg))
  (let ((msg-file-to-pipe (mh-msg-filename (mh-get-msg-num t)))
        (message-directory default-directory))
    (save-excursion
      (set-buffer (get-buffer-create mh-temp-buffer))
      (erase-buffer)
      (insert-file-contents msg-file-to-pipe)
      (goto-char (point-min))
      (if (not include-headers) (search-forward "\n\n"))
      (let ((default-directory message-directory))
        (shell-command-on-region (point) (point-max) command nil)))))

;;;###mh-autoload
(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (mh-in-show-buffer (mh-show-buffer)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      ;; Search for blank line and then for From:
      (or (and (search-forward "\n\n" nil t)
               (re-search-forward "^From:" nil t))
          (error "No more messages in digest")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (mh-recenter 0)))

;;;###mh-autoload
(defun mh-page-digest-backwards ()
  "Back up displayed message to previous digested message."
  (interactive)
  (mh-in-show-buffer (mh-show-buffer)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      (beginning-of-line)
      (or (and (search-backward "\n\n" nil t)
               (re-search-backward "^From:" nil t))
          (error "No previous message in digest")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (if (search-backward "\n\n" nil t)
        (forward-line 2))
    (mh-recenter 0)))

;;;###mh-autoload
(defun mh-print-msg (msg-or-seq)
  "Print MSG-OR-SEQ on printer.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is printed.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence.

The variable `mh-lpr-command-format' is used to generate the print command.
The messages are formatted by mhl. See the variable `mhl-formfile'."
  (interactive (list (mh-interactive-msg-or-seq "Print")))
  (message "Printing...")
  (let (msgs)
    ;; Gather message numbers and add them to "printed" sequence.
    (mh-iterate-on-msg-or-seq msg msg-or-seq
      (mh-add-msgs-to-seq msg 'printed t)
      (mh-notate nil mh-note-printed mh-cmd-note)
      (push msg msgs))
    (setq msgs (nreverse msgs))
    ;; Print scan listing if we have more than one message.
    (if (> (length msgs) 1)
        (let* ((msgs-string
                (mapconcat 'identity (mh-list-to-string
                                      (mh-coalesce-msg-list msgs)) " "))
               (lpr-command
                (format mh-lpr-command-format
                        (cond ((listp msg-or-seq)
                               (format "Folder: %s, Messages: %s"
                                       mh-current-folder msgs-string))
                              ((symbolp msg-or-seq)
                               (format "Folder: %s, Sequence: %s"
                                       mh-current-folder msg-or-seq)))))
               (scan-command
                (format "scan %s | %s" msgs-string lpr-command)))
          (if mh-print-background-flag
              (mh-exec-cmd-daemon shell-file-name nil "-c" scan-command)
            (call-process shell-file-name nil nil nil "-c" scan-command))))
    ;; Print the messages
    (dolist (msg msgs)
      (let* ((mhl-command (format "%s %s %s"
                                  (expand-file-name "mhl" mh-lib-progs)
                                  (if mhl-formfile
                                      (format " -form %s" mhl-formfile)
                                    "")
                                  (mh-msg-filename msg)))
             (lpr-command
              (format mh-lpr-command-format
                      (format "%s/%s" mh-current-folder msg)))
             (print-command
              (format "%s | %s" mhl-command lpr-command)))
        (if mh-print-background-flag
            (mh-exec-cmd-daemon shell-file-name nil "-c" print-command)
          (call-process shell-file-name nil nil nil "-c" print-command)))))
  (message "Printing...done"))

;;;###mh-autoload
(defun mh-sort-folder (&optional extra-args)
  "Sort the messages in the current folder by date.
Calls the MH program sortm to do the work.
The arguments in the list `mh-sortm-args' are passed to sortm if the optional
argument EXTRA-ARGS is given."
  (interactive "P")
  (mh-process-or-undo-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (mh-set-folder-modified-p t)          ; lock folder while sorting
  (message "Sorting folder...")
  (let ((threaded-flag (memq 'unthread mh-view-ops)))
    (mh-exec-cmd "sortm" mh-current-folder (if extra-args mh-sortm-args))
    (when mh-index-data
      (mh-index-update-maps mh-current-folder))
    (message "Sorting folder...done")
    (mh-scan-folder mh-current-folder "all")
    (cond (threaded-flag (mh-toggle-threads))
          (mh-index-data (mh-index-insert-folder-headers)))))

;;;###mh-autoload
(defun mh-undo-folder (&rest ignore)
  "Undo all pending deletes and refiles in current folder.
Argument IGNORE is deprecated."
  (interactive)
  (cond ((or mh-do-not-confirm-flag
             (yes-or-no-p "Undo all commands in folder? "))
         (setq mh-delete-list nil
               mh-refile-list nil
               mh-seq-list nil
               mh-next-direction 'forward)
         (with-mh-folder-updating (nil)
           (mh-unmark-all-headers t)))
        (t
         (message "Commands not undone.")
         ;; Remove by 2003-06-30 if nothing seems amiss. XXX
         ;; (sit-for 2)
         )))

;;;###mh-autoload
(defun mh-store-msg (directory)
  "Store the file(s) contained in the current message into DIRECTORY.
The message can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
`mh-store-default-directory' or the current directory."
  (interactive (list (let ((udir (or mh-store-default-directory
                                     default-directory)))
                       (read-file-name "Store message in directory: "
                                       udir udir nil))))
  (let ((msg-file-to-store (mh-msg-filename (mh-get-msg-num t))))
    (save-excursion
      (set-buffer (get-buffer-create mh-temp-buffer))
      (erase-buffer)
      (insert-file-contents msg-file-to-store)
      (mh-store-buffer directory))))

;;;###mh-autoload
(defun mh-store-buffer (directory)
  "Store the file(s) contained in the current buffer into DIRECTORY.
The buffer can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
`mh-store-default-directory' or the current directory."
  (interactive (list (let ((udir (or mh-store-default-directory
                                     default-directory)))
                       (read-file-name "Store buffer in directory: "
                                       udir udir nil))))
  (let ((store-directory (expand-file-name directory))
        (sh-start (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward
                         "^#![ \t]*/bin/sh\\|^#\\|^: " nil t)
                        (progn
                          ;; The "cut here" pattern was removed from above
                          ;; because it seemed to hurt more than help.
                          ;; But keep this to make it easier to put it back.
                          (if (looking-at "^[^a-z0-9\"]*cut here\\b")
                              (forward-line 1))
                          (beginning-of-line)
                          (if (looking-at "^[#:]....+\n\\( ?\n\\)?end$")
                              nil       ;most likely end of a uuencode
                            (point))))))
        (command "sh")
        (uudecode-filename "(unknown filename)")
        log-begin)
    (if (not sh-start)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^begin [0-7]+ " nil t)
              (setq uudecode-filename
                    (buffer-substring (point)
                                      (progn (end-of-line) (point)))))))
    (save-excursion
      (set-buffer (get-buffer-create mh-log-buffer))
      (setq log-begin (mh-truncate-log-buffer))
      (if (not (file-directory-p store-directory))
          (progn
            (insert "mkdir " directory "\n")
            (call-process "mkdir" nil mh-log-buffer t store-directory)))
      (insert "cd " directory "\n")
      (setq mh-store-default-directory directory)
      (if (not sh-start)
          (progn
            (setq command "uudecode")
            (insert uudecode-filename " being uudecoded...\n"))))
    (set-window-start (display-buffer mh-log-buffer) log-begin) ;watch progress
    (let ((default-directory (file-name-as-directory store-directory)))
      (if (equal (call-process-region sh-start (point-max) command
                                      nil mh-log-buffer t)
                 0)
          (save-excursion
            (set-buffer mh-log-buffer)
            (insert "\n(mh-store finished)\n"))
        (error "Error occurred during execution of %s" command)))))



;;; Help Functions

;;;###mh-autoload
(defun mh-ephem-message (string)
  "Display STRING in the minibuffer momentarily."
  (message "%s" string)
  (sit-for 5)
  (message ""))

;;;###mh-autoload
(defun mh-help ()
  "Display cheat sheet for the MH-Folder commands in minibuffer."
  (interactive)
  (mh-ephem-message
   (substitute-command-keys
    (mapconcat 'identity (cdr (assoc nil mh-help-messages)) ""))))

;;;###mh-autoload
(defun mh-prefix-help ()
  "Display cheat sheet for the commands of the current prefix in minibuffer."
  (interactive)
  ;; We got here because the user pressed a `?', but he pressed a prefix key
  ;; before that. Since the the key vector starts at index 0, the index of the
  ;; last keystroke is length-1 and thus the second to last keystroke is at
  ;; length-2. We use that information to obtain a suitable prefix character
  ;; from the recent keys.
  (let* ((keys (recent-keys))
         (prefix-char (elt keys (- (length keys) 2))))
    (mh-ephem-message
     (substitute-command-keys
      (mapconcat 'identity (cdr (assoc prefix-char mh-help-messages)) "")))))

(provide 'mh-funcs)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 1936c4f1-4843-438e-bc4b-a63bb75a7762
;;; mh-funcs.el ends here
