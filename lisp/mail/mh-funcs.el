;;; mh-funcs --- mh-e functions not everyone will use right away
;; Time-stamp: <94/03/08 16:00:54 gildea>

;; Copyright 1993 Free Software Foundation, Inc.

;; This file is part of mh-e.

;; mh-e is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mh-e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mh-e; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Internal support for mh-e package.
;;; Putting these functions in a separate file lets mh-e start up faster,
;;; since less Lisp code needs to be loaded all at once.

;;; Code:

(provide 'mh-funcs)
(require 'mh-e)

(defvar mh-sortm-args nil
  "Extra arguments to have \\[mh-sort-folder] pass to the \"sortm\" command.
For example, '(\"-nolimit\" \"-textfield\" \"subject\") is a useful setting.")

(defun mh-burst-digest ()
  "Burst apart the current message, which should be a digest.
The message is replaced by its table of contents and the letters from the
digest are inserted into the folder after that message."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (mh-process-or-undo-commands mh-current-folder)
    (mh-set-folder-modified-p t)		; lock folder while bursting
    (message "Bursting digest...")
    (mh-exec-cmd "burst" mh-current-folder digest "-inplace")
    (mh-scan-folder mh-current-folder (format "%d-last" mh-first-msg-num))
    (message "Bursting digest...done")))


(defun mh-copy-msg (dest msg-or-seq)
  "Copy to another FOLDER the specified MESSAGE(s) without deleting them.
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence."
  (interactive (list (mh-prompt-for-folder "Copy to" "" t)
		     (if current-prefix-arg
			 (mh-read-seq-default "Copy" t)
			 (mh-get-msg-num t))))
  (mh-exec-cmd "refile" msg-or-seq "-link" "-src" mh-current-folder dest)
  (if (numberp msg-or-seq)
      (mh-notate msg-or-seq ?C mh-cmd-note)
      (mh-notate-seq msg-or-seq ?C mh-cmd-note)))

(defun mh-kill-folder ()
  "Remove the current folder."
  (interactive)
  (if (or mh-do-not-confirm
	  (yes-or-no-p (format "Remove folder %s? " mh-current-folder)))
      (let ((folder mh-current-folder))
	(if (null mh-folder-list)
	    (mh-set-folder-list))
	(mh-set-folder-modified-p t)	; lock folder to kill it
	(mh-exec-cmd-daemon "rmf" folder)
	(setq mh-folder-list
	      (delq (assoc folder mh-folder-list) mh-folder-list))
	(message "Folder %s removed" folder)
	(mh-set-folder-modified-p nil)	; so kill-buffer doesn't complain
	(if (get-buffer mh-show-buffer)
	    (kill-buffer mh-show-buffer))
	(kill-buffer folder))
      (message "Folder not removed")))


(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (with-output-to-temp-buffer " *mh-temp*"
    (save-excursion
      (switch-to-buffer " *mh-temp*")
      (erase-buffer)
      (message "Listing folders...")
      (mh-exec-cmd-output "folders" t (if mh-recursive-folders
					  "-recurse"
					  "-norecurse"))
      (goto-char (point-min))
      (message "Listing folders...done"))))


(defun mh-pack-folder (range)
  "Renumber the messages of a folder to be 1..n.
First, offer to execute any outstanding commands for the current folder.
If optional prefix argument provided, prompt for the range of messages
to display after packing.  Otherwise, show the entire folder."
  (interactive (list (if current-prefix-arg
			 (mh-read-msg-range
			  "Range to scan after packing [all]? ")
		       "all")))
  (mh-pack-folder-1 range)
  (mh-goto-cur-msg)
  (message "Packing folder...done"))


(defun mh-pack-folder-1 (range)
  ;; Close and pack the current folder.
  (mh-process-or-undo-commands mh-current-folder)
  (message "Packing folder...")
  (mh-set-folder-modified-p t)		; lock folder while packing
  (save-excursion
    (mh-exec-cmd-quiet t "folder" mh-current-folder "-pack"))
  (mh-regenerate-headers range))


(defun mh-pipe-msg (command include-headers)
  "Pipe the current message through the given shell COMMAND.
If INCLUDE-HEADERS (prefix argument) is provided, send the entire message.
Otherwise just send the message's body without the headers."
  (interactive
   (list (read-string "Shell command on message: ") current-prefix-arg))
  (let ((file-name (mh-msg-filename (mh-get-msg-num t))))
    (save-excursion
      (set-buffer (get-buffer-create " *mh-temp*"))
      (erase-buffer)
      (insert-file-contents file-name)
      (goto-char (point-min))
      (if (not include-headers) (search-forward "\n\n"))
      (shell-command-on-region (point) (point-max) command nil))))


(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (mh-in-show-buffer (mh-show-buffer)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      ;; Search for blank line and then for From:
      (or (and (search-forward "\n\n" nil t)
	       (search-forward "From:" nil t))
	  (error "No more messages in digest")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (mh-recenter 0)))


(defun mh-page-digest-backwards ()
  "Back up displayed message to previous digested message."
  (interactive)
  (mh-in-show-buffer (mh-show-buffer)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      (beginning-of-line)
      (or (and (search-backward "\n\n" nil t)
	       (search-backward "From:" nil t))
	  (error "No previous message in digest")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (if (search-backward "\n\n" nil t)
	(forward-line 2))
    (mh-recenter 0)))


(defun mh-print-msg (msg-or-seq)
  "Print MESSAGE(s) (default: displayed message) on printer.
If optional prefix argument provided, then prompt for the message sequence.
The variable mh-lpr-command-format is used to generate the print command.
The messages are formatted by mhl.  See the variable mhl-formfile."
  (interactive (list (if current-prefix-arg
			 (reverse (mh-seq-to-msgs
				   (mh-read-seq-default "Print" t)))
		       (mh-get-msg-num t))))
  (if (numberp msg-or-seq)
      (message "Printing message...")
      (message "Printing sequence..."))
  (let ((print-command
	 (if (numberp msg-or-seq)
	     (format "%s -nobell -clear %s %s | %s"
		     (expand-file-name "mhl" mh-lib)
		     (mh-msg-filename msg-or-seq)
		     (if (stringp mhl-formfile)
			 (format "-form %s" mhl-formfile)
		       "")
		     (format mh-lpr-command-format
			     (if (numberp msg-or-seq)
				 (format "%s/%d" mh-current-folder
				       msg-or-seq)
			         (format "Sequence from %s" mh-current-folder))))
	     (format "(scan -clear %s ; %s -nobell -clear %s %s) | %s"
		     (mapconcat (function (lambda (msg) msg)) msg-or-seq " ")
		     (expand-file-name "mhl" mh-lib)
		     (if (stringp mhl-formfile)
			 (format "-form %s" mhl-formfile)
		       "")
		     (mh-msg-filenames msg-or-seq)
		     (format mh-lpr-command-format
			     (if (numberp msg-or-seq)
				 (format "%s/%d" mh-current-folder
				       msg-or-seq)
			         (format "Sequence from %s"
					 mh-current-folder)))))))
    (if mh-print-background
	(mh-exec-cmd-daemon shell-file-name "-c" print-command)
      (call-process shell-file-name nil nil nil "-c" print-command))
    (if (numberp msg-or-seq)
	(mh-notate msg-or-seq ?P mh-cmd-note)
        (mh-notate-seq msg-or-seq ?P mh-cmd-note))
    (mh-add-msgs-to-seq msg-or-seq 'printed t)
    (if (numberp msg-or-seq)
	(message "Printing message...done")
        (message "Printing sequence...done"))))


(defun mh-msg-filenames (msgs &optional folder)
  ;; Return a list of file names for MSGS in FOLDER (default current folder).
  (mapconcat (function (lambda (msg) (mh-msg-filename msg folder))) msgs " "))


(defun mh-sort-folder (&optional no-args)
  "Sort the messages in the current folder by date.
Calls the MH program sortm to do the work.
The arguments in the list  mh-sortm-args  are passed to sortm
unless this function is passed an argument."
  (interactive "P")
  (mh-process-or-undo-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (mh-set-folder-modified-p t)		; lock folder while sorting
  (message "Sorting folder...")
  (mh-exec-cmd "sortm" mh-current-folder (if (not no-args) mh-sortm-args))
  (message "Sorting folder...done")
  (mh-scan-folder mh-current-folder "all"))


(defun mh-undo-folder (&rest ignore)
  "Undo all commands in current folder."
  (interactive)
  (cond ((or mh-do-not-confirm
	     (yes-or-no-p "Undo all commands in folder? "))
	 (setq mh-delete-list nil
	       mh-refile-list nil
	       mh-seq-list nil
	       mh-next-direction 'forward)
	 (with-mh-folder-updating (nil)
	   (mh-unmark-all-headers t)))
	(t
	 (message "Commands not undone.")
	 (sit-for 2))))


(defun mh-store-msg (dir)
  "Store the file(s) contained in the current message into DIRECTORY.
The message can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
mh-store-default-directory  or the current directory."
  (interactive (list (let ((udir (or mh-store-default-directory default-directory)))
				 (read-file-name "Store message in directory: "
						 udir udir nil))))
  (let ((file-name (mh-msg-filename (mh-get-msg-num t))))
    (save-excursion
      (set-buffer (get-buffer-create " *mh-temp*"))
      (erase-buffer)
      (insert-file-contents file-name)
      (mh-store-buffer dir))))

(defun mh-store-buffer (dir)
  "Store the file(s) contained in the current buffer into DIRECTORY.
The buffer can contain a shar file or uuencoded file.
Default directory is the last directory used, or initially the value of
`mh-store-default-directory' or the current directory."
  (interactive (list (let ((udir (or mh-store-default-directory default-directory)))
				 (read-file-name "Store buffer in directory: "
						 udir udir nil))))
  (let ((store-directory (expand-file-name dir))
	(start (save-excursion
		 (goto-char (point-min))
		 (if (or (re-search-forward "^#![ \t]*/bin/sh" nil t)
			 (and (re-search-forward "^[^a-z0-9\"]*cut here\\b" nil t)
			      (forward-line 1))
			 (re-search-forward "^#" nil t)
			 (re-search-forward "^: " nil t))
		     (progn (beginning-of-line) (point)))))
	(log-buffer (get-buffer-create "*Store Output*"))
	(command "sh"))
    (save-excursion
      (set-buffer log-buffer)
      (erase-buffer)
      (if (not (file-directory-p store-directory))
	  (progn
	    (insert "mkdir " dir "\n")
	    (call-process "mkdir" nil log-buffer t store-directory)))
      (insert "cd " dir "\n")
      (if (not start)
	  (progn
	    (setq command "uudecode")
	    (insert "uudecoding...\n"))))
    (set-window-start (display-buffer log-buffer) 0) ;watch progress
    (let ((default-directory (file-name-as-directory store-directory)))
      (call-process-region start (point-max) command nil log-buffer t))
    (setq mh-store-default-directory dir)
    (set-buffer log-buffer)
    (insert "\n(mh-store finished)\n")))
	
