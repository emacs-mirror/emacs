;;; mh-comp --- mh-e functions for composing messages
;; Time-stamp: <95/04/20 19:16:23 gildea>

;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Internal support for mh-e package.

;;; Change Log:

;; $Id: mh-comp.el,v 1.4 1995/04/10 00:19:27 kwzh Exp kwzh $

;;; Code:

(provide 'mh-comp)
(require 'mh-utils)

;;; Site customization (see also mh-utils.el):

(defvar mh-send-prog "send"
  "Name of the MH send program.
Some sites need to change this because of a name conflict.")

(defvar mh-redist-full-contents nil
  "Non-nil if the `dist' command needs whole letter for redistribution.
This is the case only when `send' is compiled with the BERK option.
If MH will not allow you to redist a previously redist'd msg, set to nil.")


(defvar mh-note-repl "-"
  "String whose first character is used to notate replied to messages.")

(defvar mh-note-forw "F"
  "String whose first character is used to notate forwarded messages.")

(defvar mh-note-dist "R"
  "String whose first character is used to notate redistributed messages.")

(defvar mh-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the citation
text as modified.

This is a normal hook, misnamed for historical reasons.
It is semi-obsolete and is only used if mail-citation-hook is nil.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), the text of the message is inserted
with mh-ins-buf-prefix prefixed to each line.

See also the variable mh-yank-from-start-of-msg, which controls how
much of the message passed to the hook.")

;;; Copied from sendmail.el for Hyperbole
(defvar mail-header-separator "--------"
  "*Line used by MH to separate headers from text in messages being composed.")

;;; Personal preferences:

(defvar mh-delete-yanked-msg-window nil
  "*Controls window display when a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If non-nil, yanking the current message into a draft letter deletes any
windows displaying the message.")

(defvar mh-yank-from-start-of-msg t
  "*Controls which part of a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If non-nil, include the entire message.  If the symbol `body', then yank the
message minus the header.  If nil, yank only the portion of the message
following the point.  If the show buffer has a region, this variable is
ignored.")

(defvar mh-ins-buf-prefix "> "
  "*String to put before each non-blank line of a yanked or inserted message.
\\<mh-letter-mode-map>Used when the message is inserted into an outgoing letter
by \\[mh-insert-letter] or \\[mh-yank-cur-msg].")

(defvar mh-reply-default-reply-to nil
  "*Sets the person or persons to whom a reply will be sent.
If nil, prompt for recipient.  If non-nil, then \\<mh-folder-mode-map>`\\[mh-reply]' will use this
value and it should be one of \"from\", \"to\", \"cc\", or \"all\".
The values \"cc\" and \"all\" do the same thing.")

(defvar mh-signature-file-name "~/.signature"
  "*Name of file containing the user's signature.
Inserted into message by \\<mh-letter-mode-map>\\[mh-insert-signature].")

(defvar mh-forward-subject-format "%s: %s"
  "*Format to generate the Subject: line contents for a forwarded message.
The two string arguments to the format are the sender of the original
message and the original subject line.")

(defvar mh-comp-formfile "components"
  "Name of file to be used as a skeleton for composing messages.
Default is \"components\".  If not a complete path name, the file
is searched for first in the user's MH directory, then in the
system MH lib directory.")

(defvar mh-repl-formfile "replcomps"
  "Name of file to be used as a skeleton for replying to messages.
Default is \"replcomps\".  If not a complete path name, the file
is searched for first in the user's MH directory, then in the
system MH lib directory.")

;;; Hooks:

(defvar mh-letter-mode-hook nil
  "Invoked in `mh-letter-mode' on a new letter.")

(defvar mh-compose-letter-function nil
  "Invoked when setting up a letter draft.
It is passed three arguments: TO recipients, SUBJECT, and CC recipients.")

(defvar mh-before-send-letter-hook nil
  "Invoked at the beginning of the \\<mh-letter-mode-map>\\[mh-send-letter] command.")


(defvar mh-rejected-letter-start
  (concat "^   ----- Unsent message follows -----$"	;from sendmail V5
	  "\\|^   ----- Original message follows -----$" ;from sendmail V8
	  "\\|^------- Unsent Draft$"			;from MH itself
	  "\\|^----------  Original Message  ----------$" ;from zmailer
	  "\\|^  --- The unsent message follows ---$"	;from AIX mail system
	  "\\|^    Your message follows:$"		;from MMDF-II
	  "\\|^Content-Description: Returned Content$"	;1993 KJ sendmail
	  )
  "Regexp specifying the beginning of the wrapper around a returned letter.
This wrapper is generated by the mail system when rejecting a letter.")

(defvar mh-new-draft-cleaned-headers
  "^Date:\\|^Received:\\|^Message-Id:\\|^From:\\|^Sender:\\|^Errors-To:\\|^Delivery-Date:\\|^Return-Path:"
  "Regexp of header lines to remove before offering a message as a new draft.
Used by the \\<mh-folder-mode-map>`\\[mh-edit-again]' and `\\[mh-extract-rejected-mail]' commands.")

(defvar mh-to-field-choices '(("t" . "To:") ("s" . "Subject:") ("c" . "Cc:")
			      ("b" . "Bcc:") ("f" . "Fcc:") ("r" . "From:")
			      ("d" . "Dcc:"))
  "Alist of (final-character . field-name) choices for mh-to-field.")

(defvar mh-letter-mode-map (copy-keymap text-mode-map)
  "Keymap for composing mail.")

(defvar mh-letter-mode-syntax-table nil
  "Syntax table used by mh-e while in MH-Letter mode.")

(if mh-letter-mode-syntax-table
    ()
    (setq mh-letter-mode-syntax-table
	  (make-syntax-table text-mode-syntax-table))
    (modify-syntax-entry ?% "." mh-letter-mode-syntax-table))


;;;###autoload
(defun mh-smail ()
  "Compose and send mail with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))


(defun mh-edit-again (msg)
  "Clean-up a draft or a message previously sent and make it resendable.
Default is the current message.
The variable mh-new-draft-cleaned-headers specifies the headers to remove.
See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-get-msg-num t)))
  (let* ((from-folder mh-current-folder)
	 (config (current-window-configuration))
	 (draft
	  (cond ((and mh-draft-folder (equal from-folder mh-draft-folder))
		 (pop-to-buffer (find-file-noselect (mh-msg-filename msg)) t)
		 (rename-buffer (format "draft-%d" msg))
		 (buffer-name))
		(t
		 (mh-read-draft "clean-up" (mh-msg-filename msg) nil)))))
    (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder nil nil nil nil nil nil
			      config)))


(defun mh-extract-rejected-mail (msg)
  "Extract a letter returned by the mail system and make it resendable.
Default is the current message.  The variable mh-new-draft-cleaned-headers
gives the headers to clean out of the original message.
See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-get-msg-num t)))
  (let ((from-folder mh-current-folder)
	(config (current-window-configuration))
	(draft (mh-read-draft "extraction" (mh-msg-filename msg) nil)))
    (goto-char (point-min))
    (cond ((re-search-forward mh-rejected-letter-start nil t)
	   (skip-chars-forward " \t\n")
	   (delete-region (point-min) (point))
	   (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil))
	  (t
	   (message "Does not appear to be a rejected letter.")))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder msg
			      (mh-get-header-field "To:")
			      (mh-get-header-field "From:")
			      (mh-get-header-field "Cc:")
			      nil nil config)))


(defun mh-forward (to cc &optional msg-or-seq)
  "Forward a message or message sequence.  Defaults to displayed message.
If optional prefix argument provided, then prompt for the message sequence.
See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-read-address "To: ")
		     (mh-read-address "Cc: ")
		     (if current-prefix-arg
			 (mh-read-seq-default "Forward" t)
		       (mh-get-msg-num t))))
  (or msg-or-seq
      (setq msg-or-seq (mh-get-msg-num t)))
  (let* ((folder mh-current-folder)
	 (config (current-window-configuration))
	 ;; forw always leaves file in "draft" since it doesn't have -draft
	 (draft-name (expand-file-name "draft" mh-user-path))
	 (draft (cond ((or (not (file-exists-p draft-name))
			   (y-or-n-p "The file 'draft' exists.  Discard it? "))
		       (mh-exec-cmd "forw" "-build"
				    mh-current-folder msg-or-seq)
		       (prog1
			   (mh-read-draft "" draft-name t)
			 (mh-insert-fields "To:" to "Cc:" cc)
			 (set-buffer-modified-p nil)))
		      (t
		       (mh-read-draft "" draft-name nil)))))
    (let (orig-from
	  orig-subject)
      (goto-char (point-min))
      (re-search-forward "^------- Forwarded Message")
      (forward-line 1)
      (skip-chars-forward " \t\n")
      (save-restriction
	(narrow-to-region (point) (point-max))
	(setq orig-from (mh-get-header-field "From:"))
	(setq orig-subject (mh-get-header-field "Subject:")))
      (let ((forw-subject
	     (mh-forwarded-letter-subject orig-from orig-subject)))
	(mh-insert-fields "Subject:" forw-subject)
	(goto-char (point-min))
	(re-search-forward "^------- Forwarded Message")
	(forward-line -1)
	(delete-other-windows)
	(if (numberp msg-or-seq)
	    (mh-add-msgs-to-seq msg-or-seq 'forwarded t)
	  (mh-add-msgs-to-seq (mh-seq-to-msgs msg-or-seq) 'forwarded t))
	(mh-compose-and-send-mail draft "" folder msg-or-seq
				  to forw-subject cc
				  mh-note-forw "Forwarded:"
				  config)))))

(defun mh-forwarded-letter-subject (from subject)
  ;; Return a Subject suitable for a forwarded message.
  ;; Original message has headers FROM and SUBJECT.
  (let ((addr-start (string-match "<" from))
	(comment (string-match "(" from)))
    (cond ((and addr-start (> addr-start 0))
	   ;; Full Name <luser@host>
	   (setq from (substring from 0 (1- addr-start))))
	  (comment
	   ;; luser@host (Full Name)
	   (setq from (substring from (1+ comment) (1- (length from)))))))
  (format mh-forward-subject-format from subject))


;;;###autoload
(defun mh-smail-other-window ()
  "Compose and send mail in other window with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))


(defun mh-redistribute (to cc &optional msg)
  "Redistribute a letter.
Depending on how your copy of MH was compiled, you may need to change the
setting of the variable mh-redist-full-contents.  See its documentation."
  (interactive (list (mh-read-address "Redist-To: ")
		     (mh-read-address "Redist-Cc: ")
		     (mh-get-msg-num t)))
  (or msg
      (setq msg (mh-get-msg-num t)))
  (save-window-excursion
    (let ((folder mh-current-folder)
	  (draft (mh-read-draft "redistribution"
				(if mh-redist-full-contents
				    (mh-msg-filename msg)
				  nil)
				nil)))
      (mh-goto-header-end 0)
      (insert "Resent-To: " to "\n")
      (if (not (equal cc "")) (insert "Resent-cc: " cc "\n"))
      (mh-clean-msg-header (point-min)
			   "^Message-Id:\\|^Received:\\|^Return-Path:\\|^Sender:\\|^Date:\\|^From:"
			   nil)
      (save-buffer)
      (message "Redistributing...")
      (if mh-redist-full-contents
	  (call-process "/bin/sh" nil 0 nil "-c"
			(format "mhdist=1 mhaltmsg=%s %s -push %s"
				buffer-file-name
				(expand-file-name mh-send-prog mh-progs)
				buffer-file-name))
	(call-process "/bin/sh" nil 0 nil "-c"
		      (format "mhdist=1 mhaltmsg=%s mhannotate=1 %s -push %s"
			      (mh-msg-filename msg folder)
			      (expand-file-name mh-send-prog mh-progs)
			      buffer-file-name)))
      (mh-annotate-msg msg folder mh-note-dist
		       "-component" "Resent:"
		       "-text" (format "\"%s %s\"" to cc))
      (kill-buffer draft)
      (message "Redistributing...done"))))


(defun mh-reply (message &optional includep)
  "Reply to MESSAGE (default: current message).
If optional prefix argument INCLUDEP provided, then include the message
in the reply using filter mhl.reply in your MH directory.
Prompts for type of addresses to reply to:
   from    sender only,
   to      sender and primary recipients,
   cc/all  sender and all recipients.
If the file named by `mh-repl-formfile' exists, it is used as a skeleton
for the reply.  See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-get-msg-num t) current-prefix-arg))
  (let ((minibuffer-help-form
	 "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
    (let ((reply-to (or mh-reply-default-reply-to
			(completing-read "Reply to whom: "
					 '(("from") ("to") ("cc") ("all"))
					 nil
					 t)))
	  (folder mh-current-folder)
	  (show-buffer mh-show-buffer)
	  (config (current-window-configuration)))
      (message "Composing a reply...")
      (mh-exec-cmd "repl" "-build" "-noquery" "-nodraftfolder"
	     (if (stringp mh-repl-formfile) ;must be string, but we're paranoid
		 (list "-form" mh-repl-formfile))
	     mh-current-folder message
	     (cond ((or (equal reply-to "from") (equal reply-to ""))
		    '("-nocc" "all"))
		   ((equal reply-to "to")
		    '("-cc" "to"))
		   ((or (equal reply-to "cc") (equal reply-to "all"))
		    '("-cc" "all" "-nocc" "me")))
	     (if includep
		 '("-filter" "mhl.reply")))
      (let ((draft (mh-read-draft "reply"
				  (expand-file-name "reply" mh-user-path)
				  t)))
	(delete-other-windows)
	(set-buffer-modified-p nil)
	
	(let ((to (mh-get-header-field "To:"))
	      (subject (mh-get-header-field "Subject:"))
	      (cc (mh-get-header-field "Cc:")))
	  (goto-char (point-min))
	  (mh-goto-header-end 1)
	  (or includep
	      (mh-in-show-buffer (show-buffer)
		(mh-display-msg message folder)))
	  (mh-add-msgs-to-seq message 'answered t)
	  (message "Composing a reply...done")
	  (mh-compose-and-send-mail draft "" folder message to subject cc
				    mh-note-repl "Replied:" config))))))


(defun mh-send (to cc subject)
  "Compose and send a letter.
The file named by `mh-comp-formfile' will be used as the form.
Do not call this function from outside mh-e; use \\[mh-smail] instead.

The letter is composed in mh-letter-mode; see its documentation for more
details.  If `mh-compose-letter-function' is defined, it is called on the
draft and passed three arguments: to, subject, and cc."
  (interactive (list
		(mh-read-address "To: ")
		(mh-read-address "Cc: ")
		(read-string "Subject: ")))
  (let ((config (current-window-configuration)))
    (delete-other-windows)
    (mh-send-sub to cc subject config)))


(defun mh-send-other-window (to cc subject)
  "Compose and send a letter in another window.
Do not call this function from outside mh-e;
use \\[mh-smail-other-window] instead.
See also documentation for `\\[mh-send]' function."
  (interactive (list
		(mh-read-address "To: ")
		(mh-read-address "Cc: ")
		(read-string "Subject: ")))
  (let ((pop-up-windows t))
    (mh-send-sub to cc subject (current-window-configuration))))


(defun mh-send-sub (to cc subject config)
  ;; Do the real work of composing and sending a letter.
  ;; Expects the TO, CC, and SUBJECT fields as arguments.
  ;; CONFIG is the window configuration before sending mail.
  (let ((folder mh-current-folder)
	(msg-num (mh-get-msg-num nil)))
    (message "Composing a message...")
    (let ((draft (mh-read-draft
		  "message"
		  (let (components)
		    (cond
		     ((file-exists-p
		       (setq components
			     (expand-file-name mh-comp-formfile mh-user-path)))
		      components)
		     ((file-exists-p
		       (setq components
			     (expand-file-name mh-comp-formfile mh-lib)))
		      components)
		     (t
		      (error (format "Can't find components file \"%s\""
				     components)))))
		  nil)))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (goto-char (point-max))
      (message "Composing a message...done")
      (mh-compose-and-send-mail draft "" folder msg-num
				to subject cc
				nil nil config))))


(defun mh-read-draft (use initial-contents delete-contents-file)
  ;; Read draft file into a draft buffer and make that buffer the current one.
  ;; USE is a message used for prompting about the intended use of the message.
  ;; INITIAL-CONTENTS is filename that is read into an empty buffer, or NIL
  ;; if buffer should not be modified.  Delete the initial-contents file if
  ;; DELETE-CONTENTS-FILE flag is set.
  ;; Returns the draft folder's name.
  ;; If the draft folder facility is enabled in ~/.mh_profile, a new buffer is
  ;; used each time and saved in the draft folder.  The draft file can then be
  ;; reused.
  (cond (mh-draft-folder
	 (let ((orig-default-dir default-directory)
	       (draft-file-name (mh-new-draft-name)))
	   (pop-to-buffer (generate-new-buffer
			   (format "draft-%s"
				   (file-name-nondirectory draft-file-name))))
	   (condition-case ()
	       (insert-file-contents draft-file-name t)
	     (file-error))
	   (setq default-directory orig-default-dir)))
	(t
	 (let ((draft-name (expand-file-name "draft" mh-user-path)))
	   (pop-to-buffer "draft")	; Create if necessary
	   (if (buffer-modified-p)
	       (if (y-or-n-p "Draft has been modified; kill anyway? ")
		   (set-buffer-modified-p nil)
		   (error "Draft preserved")))
	   (setq buffer-file-name draft-name)
	   (clear-visited-file-modtime)
	   (unlock-buffer)
	   (cond ((and (file-exists-p draft-name)
		       (not (equal draft-name initial-contents)))
		  (insert-file-contents draft-name)
		  (delete-file draft-name))))))
  (cond ((and initial-contents
	      (or (zerop (buffer-size))
		  (not (y-or-n-p
			(format "A draft exists.  Use for %s? " use)))))
	 (erase-buffer)
	 (insert-file-contents initial-contents)
	 (if delete-contents-file (delete-file initial-contents))))
  (auto-save-mode 1)
  (if mh-draft-folder
      (save-buffer))			; Do not reuse draft name
  (buffer-name))


(defun mh-new-draft-name ()
  ;; Returns the pathname of folder for draft messages.
  (save-excursion
    (mh-exec-cmd-quiet t "mhpath" mh-draft-folder "new")
    (buffer-substring (point-min) (1- (point-max)))))


(defun mh-annotate-msg (msg buffer note &rest args)
  ;; Mark the MESSAGE in BUFFER listing with the character NOTE and annotate
  ;; the saved message with ARGS.
  (apply 'mh-exec-cmd "anno" buffer msg args)
  (save-excursion
    (cond ((get-buffer buffer)		; Buffer may be deleted
	   (set-buffer buffer)
	   (if (symbolp msg)
	       (mh-notate-seq msg note (1+ mh-cmd-note))
	       (mh-notate msg note (1+ mh-cmd-note)))))))


(defun mh-insert-fields (&rest name-values)
  ;; Insert the NAME-VALUE pairs in the current buffer.
  ;; If field NAME exists, append VALUE to it.
  ;; Do not insert any pairs whose value is the empty string.
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (car (cdr name-values))))
	(cond ((equal value "")
	       nil)
	      ((mh-position-on-field field-name)
	       (insert " " value))
	      (t
	       (insert field-name " " value "\n")))
	(setq name-values (cdr (cdr name-values)))))))


(defun mh-position-on-field (field &optional ignore)
  ;; Move to the end of the FIELD in the header.
  ;; Move to end of entire header if FIELD not found.
  ;; Returns non-nil iff FIELD was found.
  ;; The optional second arg is for pre-version 4 compatibility.
  (if (mh-goto-header-field field)
      (progn
	(mh-header-field-end)
	t)))


(defun mh-get-header-field (field)
  ;; Find and return the body of FIELD in the mail header.
  ;; Returns the empty string if the field is not in the header of the
  ;; current buffer.
  (if (mh-goto-header-field field)
      (progn
	(skip-chars-forward " \t")	;strip leading white space in body
	(let ((start (point)))
	  (mh-header-field-end)
	  (buffer-substring start (point))))
    ""))

(fset 'mh-get-field 'mh-get-header-field) ;mh-e 4 compatibility

(defun mh-goto-header-field (field)
  ;; Move to FIELD in the message header.
  ;; Move to the end of the FIELD name, which should end in a colon.
  ;; Returns T if found, NIL if not.
  (goto-char (point-min))
  (let ((case-fold-search t)
	(headers-end (save-excursion
		      (mh-goto-header-end 0)
		      (point))))
    (re-search-forward (format "^%s" field) headers-end t)))

(defun mh-header-field-end ()
  ;; Move to the end of the current header field.
  ;; Handles RFC 822 continuation lines.
  (forward-line 1)
  (while (looking-at "^[ \t]")
    (forward-line 1))
  (backward-char 1))		;to end of previous line
  

(defun mh-goto-header-end (arg)
  ;; Find the end of the message header in the current buffer and position
  ;; the cursor at the ARG'th newline after the header.
  (if (re-search-forward "^-*$" nil nil)
      (forward-line arg)))


(defun mh-read-address (prompt)
  ;; Read a To: or Cc: address, prompting in the minibuffer with PROMPT.
  ;; May someday do completion on aliases.
  (read-string prompt))



;;; Mode for composing and sending a draft message.

(defvar mh-sent-from-folder nil)	;Folder of msg assoc with this letter.

(defvar mh-sent-from-msg nil)		;Number of msg assoc with this letter.

(defvar mh-send-args nil)		;Extra args to pass to "send" command.

(defvar mh-annotate-char nil)		;Character to use to annotate mh-sent-from-msg.

(defvar mh-annotate-field nil)		;Field name for message annotation.

(put 'mh-letter-mode 'mode-class 'special)

;;;###autoload
(defun mh-letter-mode ()
  "Mode for composing letters in mh-e.\\<mh-letter-mode-map>
When you have finished composing, type \\[mh-send-letter] to send the message
using the MH mail handling system.
See the documentation for \\[mh-edit-mhn] for information on composing MIME
messages.

\\{mh-letter-mode-map}

Variables controlling this mode (defaults in parentheses):

 mh-delete-yanked-msg-window (nil)
    If non-nil, \\[mh-yank-cur-msg] will delete any windows displaying
    the yanked message.

 mh-yank-from-start-of-msg (t)
    If non-nil, \\[mh-yank-cur-msg] will include the entire message.
    If `body', just yank the body (no header).
    If nil, only the portion of the message following the point will be yanked.
    If there is a region, this variable is ignored.

 mh-ins-buf-prefix (\"> \")
    String to insert before each non-blank line of a message as it is
    inserted in a draft letter.

 mh-signature-file-name (\"~/.signature\")
    File to be inserted into message by \\[mh-insert-signature].

Upon invoking mh-letter-mode, text-mode-hook and mh-letter-mode-hook are
invoked with no args, if those values are non-nil."

  (interactive)
  (or mh-user-path (mh-find-path))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-separate))
  (make-local-variable 'mh-send-args)
  (make-local-variable 'mh-annotate-char)
  (make-local-variable 'mh-annotate-field)
  (make-local-variable 'mh-previous-window-config)
  (make-local-variable 'mh-sent-from-folder)
  (make-local-variable 'mh-sent-from-msg)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator "--------") ;for Hyperbole
  (use-local-map mh-letter-mode-map)
  (setq major-mode 'mh-letter-mode)
  (mh-set-mode-name "MH-Letter")
  (set-syntax-table mh-letter-mode-syntax-table)
  (run-hooks 'text-mode-hook)
  ;; if text-mode-hook turned on auto-fill, tune it for messages
  (cond ((and (boundp 'auto-fill-hook) auto-fill-hook) ;emacs 18
	 (make-local-variable 'auto-fill-hook)
	 (setq auto-fill-hook 'mh-auto-fill-for-letter)))
  (cond ((and (boundp 'auto-fill-function) auto-fill-function) ;emacs 19
	 (make-local-variable 'auto-fill-function)
	 (setq auto-fill-function 'mh-auto-fill-for-letter)))
  (run-hooks 'mh-letter-mode-hook))


(defun mh-auto-fill-for-letter ()
  ;; Auto-fill in letters treats the header specially by inserting a tab
  ;; before continuation line.
  (if (mh-in-header-p)
      (let ((fill-prefix "\t"))
	(do-auto-fill))
    (do-auto-fill)))


(defun mh-in-header-p ()
  ;; Return non-nil if the point is in the header of a draft message.
  (save-excursion
    (let ((cur-point (point)))
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      (< cur-point (point)))))


(defun mh-to-field ()
  "Move point to the end of a specified header field.
The field is indicated by the previous keystroke (the last keystroke
of the command) according to the list in the variable mh-to-field-choices.
Create the field if it does not exist.  Set the mark to point before moving."
  (interactive)
  (expand-abbrev)
  (let ((target (cdr (or (assoc (char-to-string (logior last-input-char ?`))
				mh-to-field-choices)
			 ;; also look for a char for version 4 compat
			 (assoc (logior last-input-char ?`) mh-to-field-choices))))
	(case-fold-search t))
    (push-mark)
    (cond ((mh-position-on-field target)
	   (let ((eol (point)))
	     (skip-chars-backward " \t")
	     (delete-region (point) eol))
	   (if (and (not (eq (logior last-input-char ?`) ?s))
		    (save-excursion
		      (backward-char 1)
		      (not (looking-at "[:,]"))))
	       (insert ", ")
	       (insert " ")))
	  (t
	   (if (mh-position-on-field "To:")
	       (forward-line 1))
	   (insert (format "%s \n" target))
	   (backward-char 1)))))


(defun mh-to-fcc (&optional folder)
  "Insert an Fcc: FOLDER field in the current message.
Prompt for the field name with a completion list of the current folders."
  (interactive)
  (or folder
      (setq folder (mh-prompt-for-folder
		    "Fcc"
		    (or (and mh-default-folder-for-message-function
			     (save-excursion
			       (goto-char (point-min))
			       (funcall mh-default-folder-for-message-function)))
			"")
		    t)))
  (let ((last-input-char ?\C-f))
    (expand-abbrev)
    (save-excursion
      (mh-to-field)
      (insert (if (mh-folder-name-p folder)
		  (substring folder 1)
		folder)))))


(defun mh-insert-signature ()
  "Insert the file named by mh-signature-file-name at the current point."
  (interactive)
  (insert-file-contents mh-signature-file-name)
  (set-buffer-modified-p (buffer-modified-p))) ; force mode line update


(defun mh-check-whom ()
  "Verify recipients of the current letter, showing expansion of any aliases."
  (interactive)
  (let ((file-name buffer-file-name))
    (save-buffer)
    (message "Checking recipients...")
    (mh-in-show-buffer ("*Recipients*")
      (bury-buffer (current-buffer))
      (erase-buffer)
      (mh-exec-cmd-output "whom" t file-name))
    (message "Checking recipients...done")))



;;; Routines to compose and send a letter.

(defun mh-compose-and-send-mail (draft send-args
				       sent-from-folder sent-from-msg
				       to subject cc
				       annotate-char annotate-field
				       config)
  ;; Edit and compose a draft message in buffer DRAFT and send or save it.
  ;; SENT-FROM-FOLDER is buffer containing scan listing of current folder, or
  ;; nil if none exists.
  ;; SENT-FROM-MSG is the message number or sequence name or nil.
  ;; SEND-ARGS is an optional argument passed to the send command.
  ;; The TO, SUBJECT, and CC fields are passed to the
  ;; mh-compose-letter-function.
  ;; If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of the
  ;; message.  In that case, the ANNOTATE-FIELD is used to build a string
  ;; for mh-annotate-msg.
  ;; CONFIG is the window configuration to restore after sending the letter.
  (pop-to-buffer draft)
  (mh-letter-mode)
  (setq mh-sent-from-folder sent-from-folder)
  (setq mh-sent-from-msg sent-from-msg)
  (setq mh-send-args send-args)
  (setq mh-annotate-char annotate-char)
  (setq mh-annotate-field annotate-field)
  (setq mh-previous-window-config config)
  (setq mode-line-buffer-identification (list "{%b}"))
  (if (and (boundp 'mh-compose-letter-function)
	   mh-compose-letter-function)
      ;; run-hooks will not pass arguments.
      (let ((value mh-compose-letter-function))
	(if (and (listp value) (not (eq (car value) 'lambda)))
	    (while value
	      (funcall (car value) to subject cc)
	      (setq value (cdr value)))
	    (funcall mh-compose-letter-function to subject cc)))))


(defun mh-send-letter (&optional arg)
  "Send the draft letter in the current buffer.
If optional prefix argument is provided, monitor delivery.
Run mh-before-send-letter-hook before doing anything."
  (interactive "P")
  (run-hooks 'mh-before-send-letter-hook)
  (set-buffer-modified-p t)		; Make sure buffer is written
  (save-buffer)
  (message "Sending...")
  (let ((draft-buffer (current-buffer))
	(file-name buffer-file-name)
	(config mh-previous-window-config))
    (cond (arg
	   (pop-to-buffer "MH mail delivery")
	   (erase-buffer)
	   (mh-exec-cmd-output mh-send-prog t "-watch" "-nopush"
			       "-nodraftfolder" mh-send-args file-name)
	   (goto-char (point-max))	; show the interesting part
	   (recenter -1)
	   (set-buffer draft-buffer))	; for annotation below
	  (t
	   (mh-exec-cmd-daemon mh-send-prog "-nodraftfolder" "-noverbose"
			       mh-send-args file-name)))
    (if mh-annotate-char
	(mh-annotate-msg mh-sent-from-msg
			 mh-sent-from-folder
			 mh-annotate-char
			 "-component" mh-annotate-field
			 "-text" (format "\"%s %s\""
					 (mh-get-header-field "To:")
					 (mh-get-header-field "Cc:"))))

    (cond ((or (not arg)
	       (y-or-n-p "Kill draft buffer? "))
	   (kill-buffer draft-buffer)
	   (if config
	       (set-window-configuration config))))
    (if arg
	(message "Sending...done")
      (message "Sending...backgrounded"))))


(defun mh-insert-letter (folder message verbatim)
  "Insert a message into the current letter.
Removes the message's headers using mh-invisible-headers.  Prefixes
each non-blank line with mh-ins-buf-prefix.  Prompts for FOLDER and
MESSAGE.  If prefix argument VERBATIM provided, do not indent and do
not delete headers.  Leaves the mark before the letter and point after it."
  (interactive
   (list (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
	 (read-input (format "Message number%s: "
			     (if mh-sent-from-msg
				 (format " [%d]" mh-sent-from-msg)
			       "")))
	 current-prefix-arg))
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((start (point-min)))
      (if (equal message "") (setq message (int-to-string mh-sent-from-msg)))
      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
			      (expand-file-name message
						(mh-expand-file-name folder)))
      (cond ((not verbatim)
	     (mh-clean-msg-header start mh-invisible-headers mh-visible-headers)
	     (set-mark start)		; since mh-clean-msg-header moves it
	     (mh-insert-prefix-string mh-ins-buf-prefix))))))


(defun mh-yank-cur-msg ()
  "Insert the current message into the draft buffer.
Prefix each non-blank line in the message with the string in
`mh-ins-buf-prefix'.  If a region is set in the message's buffer, then
only the region will be inserted.  Otherwise, the entire message will
be inserted if `mh-yank-from-start-of-msg' is non-nil.  If this variable
is nil, the portion of the message following the point will be yanked.
If `mh-delete-yanked-msg-window' is non-nil, any window displaying the
yanked message will be deleted."
  (interactive)
  (if (and mh-sent-from-folder mh-sent-from-msg)
      (let ((to-point (point))
	    (to-buffer (current-buffer)))
	(set-buffer mh-sent-from-folder)
	(if mh-delete-yanked-msg-window
	    (delete-windows-on mh-show-buffer))
	(set-buffer mh-show-buffer)	; Find displayed message
	(let ((mh-ins-str (cond ((if (boundp 'mark-active)
				     mark-active ;Emacs 19
				   (mark)) ;Emacs 18
				 (buffer-substring (region-beginning)
						   (region-end)))
				((eq 'body mh-yank-from-start-of-msg)
				 (buffer-substring
				  (save-excursion
				    (goto-char (point-min))
				    (mh-goto-header-end 1)
				    (point))
				  (point-max)))
				(mh-yank-from-start-of-msg
				 (buffer-substring (point-min) (point-max)))
				(t
				 (buffer-substring (point) (point-max))))))
	  (set-buffer to-buffer)
	  (save-restriction
	    (narrow-to-region to-point to-point)
	    (push-mark)
	    (insert mh-ins-str)
	    (mh-insert-prefix-string mh-ins-buf-prefix)
	    (insert "\n"))))
    (error "There is no current message")))


(defun mh-insert-prefix-string (mh-ins-string)
  ;; Run mail-citation-hook to insert a prefix string before each line
  ;; in the buffer.  Generality for supercite users.
  (set-mark (point-max))
  (goto-char (point-min))
  (cond (mail-citation-hook
	 (run-hooks 'mail-citation-hook))
	(mh-yank-hooks			;old hook name
	 (run-hooks 'mh-yank-hooks))
	(t
	 (or (bolp) (forward-line 1))
	 (let ((zmacs-regions nil))	;so "(mark)" works in XEmacs
	   (while (< (point) (mark))
	     (insert mh-ins-string)
	     (forward-line 1))))))


(defun mh-fully-kill-draft ()
  "Kill the draft message file and the draft message buffer.
Use \\[kill-buffer] if you don't want to delete the draft message file."
  (interactive)
  (if (y-or-n-p "Kill draft message? ")
      (let ((config mh-previous-window-config))
	(if (file-exists-p buffer-file-name)
	    (delete-file buffer-file-name))
	(set-buffer-modified-p nil)
	(kill-buffer (buffer-name))
	(message "")
	(if config
	    (set-window-configuration config)))
    (error "Message not killed")))


;;; Build the letter-mode keymap:

(define-key mh-letter-mode-map "\C-c\C-f\C-b" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-c" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-d" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-f" 'mh-to-fcc)
(define-key mh-letter-mode-map "\C-c\C-f\C-r" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-s" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-t" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fb" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fc" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fd" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-ff" 'mh-to-fcc)
(define-key mh-letter-mode-map "\C-c\C-fr" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fs" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-ft" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-i" 'mh-insert-letter)
(define-key mh-letter-mode-map "\C-c\C-q" 'mh-fully-kill-draft)
(define-key mh-letter-mode-map "\C-c\C-\\" 'mh-fully-kill-draft) ;if no C-q
(define-key mh-letter-mode-map "\C-c\C-s" 'mh-insert-signature)
(define-key mh-letter-mode-map "\C-c\C-^" 'mh-insert-signature) ;if no C-s
(define-key mh-letter-mode-map "\C-c\C-w" 'mh-check-whom)
(define-key mh-letter-mode-map "\C-c\C-y" 'mh-yank-cur-msg)
(define-key mh-letter-mode-map "\C-c\C-c" 'mh-send-letter)
(define-key mh-letter-mode-map "\C-c\C-m\C-f" 'mh-mhn-compose-forw)
(define-key mh-letter-mode-map "\C-c\C-m\C-e" 'mh-mhn-compose-anon-ftp)
(define-key mh-letter-mode-map "\C-c\C-m\C-t" 'mh-mhn-compose-external-compressed-tar)
(define-key mh-letter-mode-map "\C-c\C-m\C-i" 'mh-mhn-compose-insertion)
(define-key mh-letter-mode-map "\C-c\C-e" 'mh-edit-mhn)
(define-key mh-letter-mode-map "\C-c\C-m\C-u" 'mh-revert-mhn-edit)


;;; autoloads from mh-mime

(autoload 'mh-mhn-compose-insertion "mh-mime"
  "Add a directive to insert a MIME message part from a file.
This is the typical way to insert non-text parts in a message.
See also \\[mh-edit-mhn]." t)

(autoload 'mh-mhn-compose-anon-ftp "mh-mime"
  "Add a directive for a MIME anonymous ftp external body part.
This directive tells MH to include a reference to a
message/external-body part retrievable by anonymous FTP.
See also \\[mh-edit-mhn]." t)

(autoload 'mh-mhn-compose-external-compressed-tar "mh-mime"
  "Add a directive to include a MIME reference to a compressed tar file.
The file should be available via anonymous ftp.  This directive
tells MH to include a reference to a message/external-body part.
See also \\[mh-edit-mhn]." t)

(autoload 'mh-mhn-compose-forw "mh-mime"
  "Add a forw directive to this message, to forward a message with MIME.
This directive tells MH to include another message in this one.
See also \\[mh-edit-mhn]." t)

(autoload 'mh-edit-mhn "mh-mime"
  "Format the current draft for MIME, expanding any mhn directives.
Process the current draft with the mhn program, which,
using directives already inserted in the draft, fills in
all the MIME components and header fields.
This step should be done last just before sending the message.
The mhn program is part of MH version 6.8 or later.
The `\\[mh-revert-mhn-edit]' command undoes this command.
For assistance with creating mhn directives to insert
various types of components in a message, see
\\[mh-mhn-compose-insertion] (generic insertion from a file),
\\[mh-mhn-compose-anon-ftp] (external reference to file via anonymous ftp),
\\[mh-mhn-compose-external-compressed-tar] \
\(reference to compressed tar file via anonymous ftp), and
\\[mh-mhn-compose-forw] (forward message)." t)

(autoload 'mh-revert-mhn-edit "mh-mime"
  "Undoes the effect of \\[mh-edit-mhn] by reverting to the backup file.
Optional non-nil argument means don't ask for confirmation." t)
