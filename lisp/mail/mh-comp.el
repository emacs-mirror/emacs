;;; mh-comp.el --- MH-E functions for composing messages

;; Copyright (C) 1993,1995,1997,2000,2001,2002 Free Software Foundation, Inc.

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

;;; Change Log:

;; $Id: mh-comp.el,v 1.145 2002/11/29 16:49:43 wohler Exp $

;;; Code:

(require 'mh-e)
(require 'mh-utils)
(require 'gnus-util)
(require 'easymenu)
(require 'cl)

;; Shush the byte-compiler
(defvar adaptive-fill-first-line-regexp)
(defvar font-lock-defaults)
(defvar mark-active)
(defvar sendmail-coding-system)
(defvar tool-bar-mode)

;;; autoloads from mh-mime
(autoload 'mh-press-button "mh-mime")

;;; autoloads for mh-seq
(autoload 'mh-notate-seq "mh-seq")

(autoload 'mh-compose-insertion "mh-mime"
  "Add a MIME directive to insert a file, using mhn or gnus.
If the variable mh-compose-insertion is set to 'mhn, then that will be used.
If it is set to 'gnus, then that will be used instead.")

(autoload 'mh-compose-forward "mh-mime"
  "Add a MIME directive to forward a message, using mhn or gnus.
If the variable mh-compose-insertion is set to 'mhn, then that will be used.
If it is set to 'gnus, then that will be used instead.")

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
The \\[mh-revert-mhn-edit] command undoes this command.
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

(autoload 'mh-mml-to-mime "mh-mime"
  "Compose MIME message from mml directives.")

(autoload 'mh-mml-forward-message "mh-mime"
  "Forward a message as attachment.
The function will prompt the user for a description, a folder and message
number.")

(autoload 'mh-mml-attach-file "mh-mime"
  "Attach a file to the outgoing MIME message.
The file is not inserted or encoded until you send the message with
`\\[message-send-and-exit]' or `\\[message-send]'.

Message dispostion is \"inline\" is INLINE is non-nil, else the default is
\"attachment\".
FILE is the name of the file to attach.  TYPE is its content-type, a
string of the form \"type/subtype\".  DESCRIPTION is a one-line
description of the attachment.")

(autoload 'mh-mml-secure-message-sign-pgpmime "mh-mime"
  "Add MML tag to encrypt/sign the entire message.")

(autoload 'mh-mml-secure-message-encrypt-pgpmime "mh-mime"
  "Add MML tag to encrypt and sign the entire message.
If called with a prefix argument, only encrypt (do NOT sign).")

;;; Other Autoloads.

(autoload 'Info-goto-node "info")
(autoload 'mail-mode-fill-paragraph "sendmail")
(autoload 'mm-handle-displayed-p "mm-decode")

(autoload 'sc-cite-original "sc"
  "Workhorse citing function which performs the initial citation.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard.  See `\\[sc-describe]'
for more details.  `sc-cite-original' does not do any yanking of the
original message but it does require a few things:

     1) The reply buffer is the current buffer.

     2) The original message has been yanked and inserted into the
        reply buffer.

     3) Verbose mail headers from the original message have been
        inserted into the reply buffer directly before the text of the
        original message.

     4) Point is at the beginning of the verbose headers.

     5) Mark is at the end of the body of text to be cited.

For Emacs 19's, the region need not be active (and typically isn't
when this function is called.  Also, the hook `sc-pre-hook' is run
before, and `sc-post-hook' is run after the guts of this function.")

;;; Site customization (see also mh-utils.el):

(defgroup mh-compose nil
  "MH-E functions for composing messages."
  :prefix "mh-"
  :group 'mh)

(defvar mh-send-prog "send"
  "Name of the MH send program.
Some sites need to change this because of a name conflict.")

(defvar mh-redist-full-contents nil
  "Non-nil if the `dist' command needs whole letter for redistribution.
This is the case only when `send' is compiled with the BERK option.
If MH will not allow you to redist a previously redist'd msg, set to nil.")

(defvar mh-redist-background nil
  "If non-nil redist will be done in background like send.
This allows transaction log to be visible if -watch, -verbose or -snoop are
used.")

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
It is semi-obsolete and is only used if `mail-citation-hook' is nil.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), the text of the message is inserted
with `mh-ins-buf-prefix' prefixed to each line.

See also the variable `mh-yank-from-start-of-msg', which controls how
much of the message passed to the hook.

This hook was historically provided to set up supercite.  You may now leave
this nil and set up supercite by setting the variable
`mh-yank-from-start-of-msg' to 'supercite or, for more automatic insertion,
to 'autosupercite.")

;;; Personal preferences:

(defcustom mh-compose-insertion (if (locate-library "mml") 'gnus 'mhn)
  "Use either 'gnus or 'mhn to insert MIME message directives in messages."
  :type '(choice (const :tag "Use gnus" gnus)
                 (const :tag "Use mhn"  mhn))
  :group 'mh-compose)

(defcustom mh-x-face-file "~/.face"
  "*File name containing the encoded X-Face string to insert in outgoing mail.
If nil, or the file does not exist, nothing is added to message headers."
  :type 'file
  :group 'mh-compose)

(defcustom mh-insert-x-mailer-flag t
  "*Non-nil means append an X-Mailer field to the header."
  :type 'boolean
  :group 'mh-compose)

(defvar mh-x-mailer-string nil
  "*String containing the contents of the X-Mailer header field.
If nil, this variable is initialized to show the version of MH-E, Emacs, and
MH the first time a message is composed.")

(defcustom mh-insert-mail-followup-to-flag t
  "Non-nil means maybe append a Mail-Followup-To field to the header.
The insertion is done if the To: or Cc: fields matches an entry in
`mh-insert-mail-followup-to-list'."
  :type 'boolean
  :group 'mh-compose)

(defcustom mh-insert-mail-followup-to-list nil
  "Alist of addresses for which a Mail-Followup-To field is inserted.
Each element has the form (REGEXP ADDRESS).
When the REGEXP appears in the To or cc fields of a message, the corresponding
ADDRESS is inserted in a Mail-Followup-To field.

Here's a customization example:

  regexp: mh-e-users@lists.s\\\\(ourceforge\\\\|f\\\\).net
 address: mh-e-users@lists.sourceforge.net

This corresponds to:

  (setq mh-insert-mail-followup-to-list
	'((\"mh-e-users@lists.s\\\\(ourceforge\\\\|f\\\\).net\"
	   \"mh-e-users@lists.sourceforge.net\")))

While it might be tempting to add a descriptive name to the mailing list
address, consider that this field will appear in other people's outgoing
mail in their To: field.  It might be best to keep it simple."
  :type '(repeat (list (string :tag "regexp") (string :tag "address")))
  :group 'mh-compose)

(defcustom mh-delete-yanked-msg-window-flag nil
  "*Non-nil means delete any window displaying the message.
Controls window display when a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If non-nil, yanking the current message into a draft letter deletes any
windows displaying the message."
  :type 'boolean
  :group 'mh-compose)

(defcustom mh-yank-from-start-of-msg 'attribution
  "*Controls which part of a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If t, include the entire message, with full headers.  This is historically
here for use with supercite, but is now deprecated in favor of the setting
`supercite' below.

If the symbol `body', then yank the message minus the header.

If the symbol `supercite', include the entire message, with full headers.
This also causes the invocation of `sc-cite-original' without the setting
of `mail-citation-hook', now deprecated practice.

If the symbol `autosupercite', do as for `supercite' automatically when
show buffer matches the message being replied-to.  When this option is used,
the -noformat switch is passed to the repl program to override a -filter or
-format switch.

If the symbol `attribution', then yank the message minus the header and add
a simple attribution line at the top.

If the symbol `autoattrib', do as for `attribution' automatically when show
buffer matches the message being replied-to.  You can make sure this is
always the case by setting `mh-reply-show-message-flag' to t (which is the
default) and optionally `mh-delete-yanked-msg-window-flag' to t as well such
that the show window is never displayed.  When the `autoattrib' option is
used, the -noformat switch is passed to the repl program to override a
-filter or -format switch.

If nil, yank only the portion of the message following the point.

If the show buffer has a region, this variable is ignored unless its value is
one of `attribution' or `autoattrib' in which case the attribution is added
to the yanked region."
  :type '(choice (const :tag "Below point" nil)
		 (const :tag "Without header" body)
		 (const :tag "Invoke supercite" supercite)
		 (const :tag "Invoke supercite, automatically" autosupercite)
		 (const :tag "Without header, with attribution" attribution)
		 (const :tag "Without header, with attribution, automatically"
                        autoattrib)
		 (const :tag "Entire message with headers" t))
  :group 'mh-compose)

(defcustom mh-extract-from-attribution-verb "wrote:"
  "*Verb to use for attribution when a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg]."
  :type '(choice (const "wrote:")
		 (const "a écrit :")
		 (string :tag "Custom string"))
  :group 'mh-compose)

(defcustom mh-ins-buf-prefix "> "
  "*String to put before each non-blank line of a yanked or inserted message.
\\<mh-letter-mode-map>Used when the message is inserted into an outgoing letter
by \\[mh-insert-letter] or \\[mh-yank-cur-msg]."
  :type 'string
  :group 'mh-compose)

(defcustom mh-reply-default-reply-to nil
  "*Sets the person or persons to whom a reply will be sent.
If nil, prompt for recipient.  If non-nil, then \\<mh-folder-mode-map>`\\[mh-reply]' will use this
value and it should be one of \"from\", \"to\", \"cc\", or \"all\".
The values \"cc\" and \"all\" do the same thing."
  :type '(choice (const :tag "Prompt" nil)
		 (const "from") (const "to")
		 (const "cc") (const "all"))
  :group 'mh-compose)

(defcustom mh-signature-file-name "~/.signature"
  "*Name of file containing the user's signature.
Inserted into message by \\<mh-letter-mode-map>\\[mh-insert-signature]."
  :type 'file
  :group 'mh-compose)

(defcustom mh-forward-subject-format "%s: %s"
  "*Format to generate the Subject: line contents for a forwarded message.
The two string arguments to the format are the sender of the original
message and the original subject line."
  :type 'string
  :group 'mh-compose)

(defvar mh-comp-formfile "components"
  "Name of file to be used as a skeleton for composing messages.
Default is \"components\".  If not an absolute file name, the file
is searched for first in the user's MH directory, then in the
system MH lib directory.")

(defvar mh-repl-formfile "replcomps"
  "Name of file to be used as a skeleton for replying to messages.
Default is \"replcomps\".  If not an absolute file name, the file
is searched for first in the user's MH directory, then in the
system MH lib directory.")

(defvar mh-repl-group-formfile "replgroupcomps"
  "Name of file to be used as a skeleton for replying to messages.
This file is used to form replies to the sender and all recipients of a
message. Only used if `mh-nmh-flag' is non-nil. Default is \"replgroupcomps\".
If not an absolute file name, the file is searched for first in the user's MH
directory, then in the system MH lib directory.")

(defcustom mh-reply-show-message-flag t
  "*Non-nil means the show buffer is displayed using \\<mh-letter-mode-map>\\[mh-reply].

The setting of this variable determines whether the MH `show-buffer' is
displayed with the current message when using `mh-reply' without a prefix
argument.  Set it to nil if you already include the message automatically
in your draft using
 repl: -filter repl.filter
in your ~/.mh_profile file."
  :type 'boolean
  :group 'mh-compose)

(defcustom mh-letter-fill-column 72
  "*Fill column to use in `mh-letter-mode'.
This is usually less than in other text modes because email messages get
quoted by some prefix (sometimes many times) when they are replied to,
and it's best to avoid quoted lines that span more than 80 columns."
  :type 'integer
  :group 'mh-compose)

;;; Hooks:

(defcustom mh-letter-mode-hook nil
  "Invoked in `mh-letter-mode' on a new letter."
  :type 'hook
  :group 'mh-compose)

(defcustom mh-compose-letter-function nil
  "Invoked when setting up a letter draft.
It is passed three arguments: TO recipients, SUBJECT, and CC recipients."
  :type '(choice (const nil) function)
  :group 'mh-compose)

(defcustom mh-before-send-letter-hook nil
  "Invoked at the beginning of the \\<mh-letter-mode-map>\\[mh-send-letter] command."
  :type 'hook
  :group 'mh-compose)

(defcustom mh-letter-insert-signature-hook nil
  "Invoked at the beginning of the \\<mh-letter-mode-map>\\[mh-insert-signature] command.
Can be used to determine which signature file to use based on message content.
On return, if `mh-signature-file-name' is non-nil that file will be inserted at
the current point in the buffer."
  :type 'hook
  :group 'mh-compose)

(defvar mh-rejected-letter-start
  (format "^%s$"
	  (regexp-opt
	   '("Content-Type: message/rfc822"	;MIME MDN
	     "   ----- Unsent message follows -----" ;from sendmail V5
	     " --------Unsent Message below:" ; from sendmail at BU
	     "   ----- Original message follows -----" ;from sendmail V8
	     "------- Unsent Draft"		;from MH itself
	     "----------  Original Message  ----------" ;from zmailer
	     "  --- The unsent message follows ---" ;from AIX mail system
	     "    Your message follows:"	;from MMDF-II
	     "Content-Description: Returned Content" ;1993 KJ sendmail
	     ))))

(defvar mh-new-draft-cleaned-headers
  "^Date:\\|^Received:\\|^Message-Id:\\|^From:\\|^Sender:\\|^Errors-To:\\|^Delivery-Date:\\|^Return-Path:"
  "Regexp of header lines to remove before offering a message as a new draft.
Used by the \\<mh-folder-mode-map>`\\[mh-edit-again]' and `\\[mh-extract-rejected-mail]' commands.")

(defvar mh-to-field-choices '(("t" . "To:") ("s" . "Subject:") ("c" . "Cc:")
			      ("b" . "Bcc:") ("f" . "Fcc:") ("r" . "From:")
			      ("d" . "Dcc:"))
  "Alist of (final-character . field-name) choices for `mh-to-field'.")

(defvar mh-letter-mode-map (copy-keymap text-mode-map)
  "Keymap for composing mail.")

(defvar mh-letter-mode-syntax-table nil
  "Syntax table used by MH-E while in MH-Letter mode.")

(if mh-letter-mode-syntax-table
    ()
    (setq mh-letter-mode-syntax-table
	  (make-syntax-table text-mode-syntax-table))
    (modify-syntax-entry ?% "." mh-letter-mode-syntax-table))

(defvar mh-sent-from-folder nil
  "Folder of msg assoc with this letter.")

(defvar mh-sent-from-msg nil
  "Number of msg assoc with this letter.")

(defvar mh-send-args nil
  "Extra args to pass to \"send\" command.")

(defvar mh-annotate-char nil
  "Character to use to annotate `mh-sent-from-msg'.")

(defvar mh-annotate-field nil
  "Field name for message annotation.")

;;;###autoload
(defun mh-smail ()
  "Compose and send mail with the MH mail system.
This function is an entry point to MH-E, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))

(defvar mh-error-if-no-draft nil)	;raise error over using old draft

;;;###autoload
(defun mh-smail-batch (&optional to subject other-headers &rest ignored)
  "Set up a mail composition draft with the MH mail system.
This function is an entry point to MH-E, the Emacs front end
to the MH mail system.  This function does not prompt the user
for any header fields, and thus is suitable for use by programs
that want to create a mail buffer.
Users should use `\\[mh-smail]' to compose mail.
Optional arguments for setting certain fields include TO, SUBJECT, and
OTHER-HEADERS. Additional arguments are IGNORED."
  (mh-find-path)
  (let ((mh-error-if-no-draft t))
    (mh-send (or to "") "" (or subject ""))))

;; XEmacs needs this:
;;;###autoload
(defun mh-user-agent-compose (&optional to subject other-headers continue
                                             switch-function yank-action
                                             send-actions)
  "Set up mail composition draft with the MH mail system.
This is `mail-user-agent' entry point to MH-E.

The optional arguments TO and SUBJECT specify recipients and the
initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

CONTINUE, SWITCH-FUNCTION, YANK-ACTION and SEND-ACTIONS are ignored."
  (mh-find-path)
  (let ((mh-error-if-no-draft t))
    (mh-send to "" subject)
    (while other-headers
      (mh-insert-fields (concat (car (car other-headers)) ":")
                       (cdr (car other-headers)))
      (setq other-headers (cdr other-headers)))))

(defun mh-edit-again (msg)
  "Clean up a draft or a message MSG previously sent and make it resendable.
Default is the current message.
The variable `mh-new-draft-cleaned-headers' specifies the headers to remove.
See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-get-msg-num t)))
  (let* ((from-folder mh-current-folder)
	 (config (current-window-configuration))
	 (draft
	  (cond ((and mh-draft-folder (equal from-folder mh-draft-folder))
		 (pop-to-buffer (find-file-noselect (mh-msg-filename msg)) t)
		 (rename-buffer (format "draft-%d" msg))
                 ;; Make buffer writable...
                 (setq buffer-read-only nil)
                 ;; If buffer was being used to display the message reinsert
                 ;; from file...
                 (when (eq major-mode 'mh-show-mode)
                   (erase-buffer)
                   (insert-file-contents buffer-file-name))
		 (buffer-name))
		(t
		 (mh-read-draft "clean-up" (mh-msg-filename msg) nil)))))
    (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil)
    (mh-insert-header-separator)
    (goto-char (point-min))
    (save-buffer)
    (mh-compose-and-send-mail draft "" from-folder nil nil nil nil nil nil
			      config)
    (mh-letter-mode-message)))

(defun mh-extract-rejected-mail (msg)
  "Extract message MSG returned by the mail system and make it resendable.
Default is the current message.  The variable `mh-new-draft-cleaned-headers'
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
    (mh-insert-header-separator)
    (goto-char (point-min))
    (save-buffer)
    (mh-compose-and-send-mail draft "" from-folder msg
			      (mh-get-header-field "To:")
			      (mh-get-header-field "From:")
			      (mh-get-header-field "Cc:")
			      nil nil config)
    (mh-letter-mode-message)))

(defun mh-forward (to cc &optional msg-or-seq)
"Forward one or more messages to the recipients TO and CC.

Use the optional MSG-OR-SEQ to specify a message or sequence to forward.

Default is the displayed message.  If optional prefix argument is given then
prompt for the message sequence.  If variable `transient-mark-mode' is non-nil
and the mark is active, then the selected region is forwarded.
See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-read-address "To: ")
		     (mh-read-address "Cc: ")
		     (cond
                      ((mh-mark-active-p t)
                       (mh-region-to-sequence (region-beginning) (region-end))
                       'region)
                      (current-prefix-arg
                       (mh-read-seq-default "Forward" t))
                      (t
                       (mh-get-msg-num t)))))
  (let* ((folder mh-current-folder)
	 (msgs (if (numberp msg-or-seq)
		   (list msg-or-seq)
		 (mh-seq-to-msgs msg-or-seq)))
	 (config (current-window-configuration))
	 (fwd-msg-file (mh-msg-filename (car msgs) folder))
	 ;; forw always leaves file in "draft" since it doesn't have -draft
	 (draft-name (expand-file-name "draft" mh-user-path))
	 (draft (cond ((or (not (file-exists-p draft-name))
			   (y-or-n-p "The file 'draft' exists.  Discard it? "))
		       (mh-exec-cmd "forw" "-build" (if mh-nmh-flag "-mime")
				    mh-current-folder msgs)
		       (prog1
			   (mh-read-draft "" draft-name t)
			 (mh-insert-fields "To:" to "Cc:" cc)
			 (save-buffer)))
		      (t
		       (mh-read-draft "" draft-name nil)))))
    (let (orig-from
	  orig-subject)
      (save-excursion
	(set-buffer (get-buffer-create mh-temp-buffer))
	(erase-buffer)
	(insert-file-contents fwd-msg-file)
	(setq orig-from (mh-get-header-field "From:"))
	(setq orig-subject (mh-get-header-field "Subject:")))
      (let ((forw-subject
	     (mh-forwarded-letter-subject orig-from orig-subject))
	    (mail-header-separator mh-mail-header-separator)
	    (compose))
	(mh-insert-fields "Subject:" forw-subject)
	(goto-char (point-min))
	;; If using MML, translate mhn
	(if (equal mh-compose-insertion 'gnus)
	    (save-excursion
	      (setq compose t)
	      (re-search-forward (format "^\\(%s\\)?$" mail-header-separator))
	      (while
		  (re-search-forward "^#forw \\[\\([^]]+\\)\\] \\(+\\S-+\\) \\(.*\\)$" (point-max) t)
		(let ((description (if (equal (match-string 1) "forwarded messages")
				       "forwarded message %d"
				     (match-string 1)))
		      (msgs (split-string (match-string 3)))
		      (i 0))
		  (beginning-of-line)
                  (delete-region (point)(progn (forward-line 1)(point)))
		  (dolist (msg msgs)
		    (setq i (1+ i))
		    (mh-mml-forward-message (format description i) folder msg))))))
	;; Postition just before forwarded message
	(if (re-search-forward "^------- Forwarded Message" nil t)
	    (forward-line -1)
	  (re-search-forward (format "^\\(%s\\)?$" mail-header-separator))
	  (forward-line 1))
	(delete-other-windows)
	(mh-add-msgs-to-seq msgs 'forwarded t)
	(mh-compose-and-send-mail draft "" folder msg-or-seq
				  to forw-subject cc
				  mh-note-forw "Forwarded:"
				  config)
	(if compose
	    (setq mh-mml-compose-insert-flag t))
	(mh-letter-mode-message)))))

(defun mh-forwarded-letter-subject (from subject)
  "Return a Subject suitable for a forwarded message.
Original message has headers FROM and SUBJECT."
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
This function is an entry point to MH-E, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))

(defun mh-redistribute (to cc &optional msg)
  "Redistribute displayed message to recipients TO and CC.
Use optional argument MSG to redistribute another message.
Depending on how your copy of MH was compiled, you may need to change the
setting of the variable `mh-redist-full-contents'.  See its documentation."
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
      (if (not mh-redist-background)
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
				  buffer-file-name))))
      (mh-annotate-msg msg folder mh-note-dist
		       "-component" "Resent:"
		       "-text" (format "\"%s %s\"" to cc))
      (if mh-redist-background
	  (mh-exec-cmd-daemon "/bin/sh" "-c"
			      (format "mhdist=1 mhaltmsg=%s %s %s %s"
				      (if mh-redist-full-contents
					  buffer-file-name
					(mh-msg-filename msg folder))
				      (if mh-redist-full-contents
					  ""
					"mhannotate=1")
				      (mh-expand-file-name "send" mh-progs)
				      buffer-file-name)))
      (kill-buffer draft)
      (message "Redistributing...done"))))

(defun mh-show-buffer-message-number (&optional buffer)
  "Message number of displayed message in corresponding show buffer.
Return nil if show buffer not displayed.
If in `mh-letter-mode', don't display the message number being replied to,
but rather the message number of the show buffer associated with our
originating folder buffer.
Optional argument BUFFER can be used to specify the buffer."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (cond ((eq major-mode 'mh-show-mode)
           (let ((number-start (search "/" buffer-file-name :from-end t)))
             (car (read-from-string (subseq buffer-file-name
                                            (1+ number-start))))))
          ((and (eq major-mode 'mh-folder-mode)
                mh-show-buffer
                (get-buffer mh-show-buffer))
           (mh-show-buffer-message-number mh-show-buffer))
          ((and (eq major-mode 'mh-letter-mode)
                mh-sent-from-folder
                (get-buffer mh-sent-from-folder))
           (mh-show-buffer-message-number mh-sent-from-folder))
          (t
           nil))))

(defun mh-reply (message &optional reply-to includep)
  "Reply to MESSAGE (default: current message).
If the optional argument REPLY-TO is not given, prompts for type of addresses
to reply to:
   from    sender only,
   to      sender and primary recipients,
   cc/all  sender and all recipients.
If optional prefix argument INCLUDEP provided, then include the message
in the reply using filter `mhl.reply' in your MH directory.
If the file named by `mh-repl-formfile' exists, it is used as a skeleton
for the reply.  See also documentation for `\\[mh-send]' function."
  (interactive (list
                (mh-get-msg-num t)
                (let ((minibuffer-help-form
                       "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
                  (or mh-reply-default-reply-to
                      (completing-read "Reply to whom? (from, to, all) [from]: "
                                       '(("from") ("to") ("cc") ("all"))
                                       nil
                                       t)))
                current-prefix-arg))
  (let* ((folder mh-current-folder)
         (show-buffer mh-show-buffer)
         (config (current-window-configuration))
         (group-reply (or (equal reply-to "cc") (equal reply-to "all")))
         (form-file (cond ((and mh-nmh-flag group-reply
                                (stringp mh-repl-group-formfile))
                           mh-repl-group-formfile)
                          ((stringp mh-repl-formfile) mh-repl-formfile)
                          (t nil))))
    (message "Composing a reply...")
    (mh-exec-cmd "repl" "-build" "-noquery" "-nodraftfolder"
                 (if form-file
                     (list "-form" form-file))
                 mh-current-folder message
                 (cond ((or (equal reply-to "from") (equal reply-to ""))
                        '("-nocc" "all"))
                       ((equal reply-to "to")
                        '("-cc" "to"))
                       (group-reply (if mh-nmh-flag
                                        '("-group" "-nocc" "me")
                                      '("-cc" "all" "-nocc" "me"))))
		 (cond ((or (eq mh-yank-from-start-of-msg 'autosupercite)
			    (eq mh-yank-from-start-of-msg 'autoattrib))
			'("-noformat"))
		       (includep '("-filter" "mhl.reply"))
		       (t '())))
    (let ((draft (mh-read-draft "reply"
                                (expand-file-name "reply" mh-user-path)
                                t)))
      (delete-other-windows)
      (save-buffer)
      
      (let ((to (mh-get-header-field "To:"))
            (subject (mh-get-header-field "Subject:"))
            (cc (mh-get-header-field "Cc:")))
        (goto-char (point-min))
        (mh-goto-header-end 1)
        (or includep
            (not mh-reply-show-message-flag)
            (mh-in-show-buffer (show-buffer)
              (mh-display-msg message folder)))
        (mh-add-msgs-to-seq message 'answered t)
        (message "Composing a reply...done")
        (mh-compose-and-send-mail draft "" folder message to subject cc
                                  mh-note-repl "Replied:" config))
      (when (and (or (eq 'autosupercite mh-yank-from-start-of-msg)
                     (eq 'autoattrib mh-yank-from-start-of-msg))
                 (eq (mh-show-buffer-message-number) mh-sent-from-msg))
        (undo-boundary)
        (mh-yank-cur-msg))
      (mh-letter-mode-message))))

(defun mh-send (to cc subject)
  "Compose and send a letter.

Do not call this function from outside MH-E; use \\[mh-smail] instead.

The file named by `mh-comp-formfile' will be used as the form.
The letter is composed in `mh-letter-mode'; see its documentation for more
details.
If `mh-compose-letter-function' is defined, it is called on the draft and
passed three arguments: TO, CC, and SUBJECT."
  (interactive (list
		(mh-read-address "To: ")
		(mh-read-address "Cc: ")
		(read-string "Subject: ")))
  (let ((config (current-window-configuration)))
    (delete-other-windows)
    (mh-send-sub to cc subject config)))

(defun mh-send-other-window (to cc subject)
  "Compose and send a letter in another window.

Do not call this function from outside MH-E; use \\[mh-smail-other-window]
instead.

The file named by `mh-comp-formfile' will be used as the form.
The letter is composed in `mh-letter-mode'; see its documentation for more
details.
If `mh-compose-letter-function' is defined, it is called on the draft and
passed three arguments: TO, CC, and SUBJECT."
  (interactive (list
		(mh-read-address "To: ")
		(mh-read-address "Cc: ")
		(read-string "Subject: ")))
  (let ((pop-up-windows t))
    (mh-send-sub to cc subject (current-window-configuration))))

(defun mh-send-sub (to cc subject config)
  "Do the real work of composing and sending a letter.
Expects the TO, CC, and SUBJECT fields as arguments.
CONFIG is the window configuration before sending mail."
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
		     ((file-exists-p
		       (setq components
			     (expand-file-name mh-comp-formfile
					       ;; What is this mh-etc ??  -sm
                                               ;; This is dead code, so
                                               ;; remove it.
					       ;(and (boundp 'mh-etc) mh-etc)
                                               )))
		      components)
		     (t
		      (error (format "Can't find components file \"%s\""
				     components)))))
		  nil)))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (goto-char (point-max))
      (mh-compose-and-send-mail draft "" folder msg-num
				to subject cc
				nil nil config)
      (mh-letter-mode-message))))

(defun mh-read-draft (use initial-contents delete-contents-file)
  "Read draft file into a draft buffer and make that buffer the current one.
USE is a message used for prompting about the intended use of the message.
INITIAL-CONTENTS is filename that is read into an empty buffer, or nil
if buffer should not be modified.  Delete the initial-contents file if
DELETE-CONTENTS-FILE flag is set.
Returns the draft folder's name.
If the draft folder facility is enabled in ~/.mh_profile, a new buffer is
used each time and saved in the draft folder.  The draft file can then be
reused."
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
		  (if (y-or-n-p
			(format "A draft exists.  Use for %s? " use))
		      (if mh-error-if-no-draft
			  (error "A prior draft exists"))
		    t)))
	 (erase-buffer)
	 (insert-file-contents initial-contents)
	 (if delete-contents-file (delete-file initial-contents))))
  (auto-save-mode 1)
  (if mh-draft-folder
      (save-buffer))			; Do not reuse draft name
  (buffer-name))

(defun mh-new-draft-name ()
  "Return the pathname of folder for draft messages."
  (save-excursion
    (mh-exec-cmd-quiet t "mhpath" mh-draft-folder "new")
    (buffer-substring (point-min) (1- (point-max)))))

(defun mh-annotate-msg (msg buffer note &rest args)
  "Mark MSG in BUFFER with character NOTE and annotate message with ARGS."
  (apply 'mh-exec-cmd "anno" buffer msg args)
  (save-excursion
    (cond ((get-buffer buffer)		; Buffer may be deleted
	   (set-buffer buffer)
	   (if (symbolp msg)
	       (mh-notate-seq msg note (1+ mh-cmd-note))
	       (mh-notate msg note (1+ mh-cmd-note)))))))

(defun mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUES pairs in the current buffer.
If the field exists, append the value to it.
Do not insert any pairs whose value is the empty string."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (car (cdr name-values))))
	(cond ((equal value "")
	       nil)
	      ((mh-position-on-field field-name)
	       (insert " " (or value "")))
	      (t
	       (insert field-name " " value "\n")))
	(setq name-values (cdr (cdr name-values)))))))

(defun mh-position-on-field (field &optional ignored)
  "Move to the end of the FIELD in the header.
Move to end of entire header if FIELD not found.
Returns non-nil iff FIELD was found.
The optional second arg is for pre-version 4 compatibility and is IGNORED."
  (cond ((mh-goto-header-field field)
	 (mh-header-field-end)
	 t)
	((mh-goto-header-end 0)
	 nil)))

(defun mh-get-header-field (field)
  "Find and return the body of FIELD in the mail header.
Returns the empty string if the field is not in the header of the
current buffer."
  (if (mh-goto-header-field field)
      (progn
	(skip-chars-forward " \t")	;strip leading white space in body
	(let ((start (point)))
	  (mh-header-field-end)
	  (buffer-substring start (point))))
    ""))

(fset 'mh-get-field 'mh-get-header-field) ;MH-E 4 compatibility

(defun mh-goto-header-field (field)
  "Move to FIELD in the message header.
Move to the end of the FIELD name, which should end in a colon.
Returns t if found, nil if not."
  (goto-char (point-min))
  (let ((case-fold-search t)
	(headers-end (save-excursion
		      (mh-goto-header-end 0)
		      (point))))
    (re-search-forward (format "^%s" field) headers-end t)))

(defun mh-goto-header-end (arg)
  "Move the cursor ARG lines after the header."
  (if (re-search-forward "^-*$" nil nil)
      (forward-line arg)))


(defun mh-read-address (prompt)
  "Read a To: or Cc: address, prompting in the minibuffer with PROMPT.
May someday do completion on aliases."
  (read-string prompt))



;;; Mode for composing and sending a draft message.

(put 'mh-letter-mode 'mode-class 'special)

;;; Support for emacs21 toolbar using gnus/message.el icons (and code).
(eval-when-compile (defvar tool-bar-map))
(defvar mh-letter-tool-bar-map nil)
(when (and (fboundp 'tool-bar-add-item)
           tool-bar-mode)
  (setq mh-letter-tool-bar-map
    (let ((tool-bar-map (make-sparse-keymap)))
      (tool-bar-add-item "mail_send" 'mh-send-letter 'mh-lettertoolbar-send
                         :help "Send this letter")
      (tool-bar-add-item "attach" 'mh-compose-insertion
                         'mh-lettertoolbar-compose
                         :help "Insert attachment")
      (tool-bar-add-item "spell" 'ispell-message 'mh-lettertoolbar-ispell
                         :help "Check spelling")
      (tool-bar-add-item-from-menu 'save-buffer "save")
      (tool-bar-add-item-from-menu 'undo "undo")
      (tool-bar-add-item-from-menu 'kill-region "cut")
      (tool-bar-add-item-from-menu 'menu-bar-kill-ring-save "copy")
      (tool-bar-add-item "close" 'mh-fully-kill-draft  'mh-lettertoolbar-kill
                         :help "Kill this draft")
      (tool-bar-add-item "preferences" (lambda ()
                                         (interactive)
                                         (customize-group "mh-compose"))
                         'mh-lettertoolbar-customize
                         :help "MH-E composition preferences")
      (tool-bar-add-item "help" (lambda ()
                                  (interactive)
                                  (Info-goto-node "(mh-e)Draft Editing"))
                         'mh-lettertoolbar-help :help "Help")
      tool-bar-map)))

;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
(eval-when-compile (defvar mh-letter-menu nil))
(cond
 ((fboundp 'easy-menu-define)
  (easy-menu-define
    mh-letter-menu mh-letter-mode-map "Menu for MH-E letter mode."
    '("Letter"
      ["Send This Draft"          mh-send-letter t]
      ["Split Current Line"       mh-open-line t]
      ["Check Recipient"          mh-check-whom t]
      ["Yank Current Message"     mh-yank-cur-msg t]
      ["Insert a Message..."      mh-insert-letter t]
      ["Insert Signature"         mh-insert-signature t]
      ["GPG Sign message"         mh-mml-secure-message-sign-pgpmime mh-gnus-pgp-support-flag]
      ["GPG Encrypt message"      mh-mml-secure-message-encrypt-pgpmime mh-gnus-pgp-support-flag]
      ["Compose Insertion (MIME)..."      mh-compose-insertion t]
;;    ["Compose Compressed tar (MIME)..." mh-mhn-compose-external-compressed-tar t]
;;    ["Compose Anon FTP (MIME)..."       mh-mhn-compose-anon-ftp t]
      ["Compose Forward (MIME)..."        mh-compose-forward t]
;; The next two will have to be merged.  But I also need to make sure the user
;; can't mix directives of both types.
      ["Pull in All Compositions (mhn)"   mh-edit-mhn mh-mhn-compose-insert-flag]
      ["Pull in All Compositions (gnus)"  mh-mml-to-mime mh-mml-compose-insert-flag]
      ["Revert to Non-MIME Edit (mhn)"  mh-revert-mhn-edit (equal mh-compose-insertion 'mhn)]
      ["Kill This Draft"          mh-fully-kill-draft t]))))

;;; Help Messages
;;; Group messages logically, more or less.
(defvar mh-letter-mode-help-messages
  '((nil
     "Send letter:          \\[mh-send-letter]"
     "\t\tOpen line:            \\[mh-open-line]\n"
     "Kill letter:          \\[mh-fully-kill-draft]"
     "\t\tInsert:\n"
     "Check recipients:     \\[mh-check-whom]"
     "\t\t  Current message:    \\[mh-yank-cur-msg]\n"
     "Encrypt message:      \\[mh-mml-secure-message-encrypt-pgpmime]"
     "\t\t  Attachment:         \\[mh-compose-insertion]\n"
     "Sign message:         \\[mh-mml-secure-message-sign-pgpmime]"
     "\t\t  Message to forward: \\[mh-compose-forward]\n"
     "                          "
     "\t\t  Signature:          \\[mh-insert-signature]"))
  "Key binding cheat sheet.

This is an associative array which is used to show the most common commands.
The key is a prefix char. The value is one or more strings which are
concatenated together and displayed in the minibuffer if ? is pressed after
the prefix character. The special key nil is used to display the
non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.")


(defun mh-fill-paragraph-function (arg)
  "Fill paragraph at or after point.
Prefix ARG means justify as well. This function enables `fill-paragraph' to
work better in MH-Letter mode."
  (interactive "P")
  (let ((fill-paragraph-function) (fill-prefix))
    (if (mh-in-header-p)
        (mail-mode-fill-paragraph arg)
      (fill-paragraph arg))))

;;;###autoload
(define-derived-mode mh-letter-mode text-mode "MH-Letter"
  "Mode for composing letters in MH-E.\\<mh-letter-mode-map>

When you have finished composing, type \\[mh-send-letter] to send the message
using the MH mail handling system.

If MH MIME directives are added manually, you must first run \\[mh-edit-mhn]
before sending the message. MIME directives that are added by MH-E commands
such as \\[mh-mhn-compose-insertion] are processed automatically when the
message is sent.

Options that control this mode can be changed with
\\[customize-group]; specify the \"mh-compose\" group.

When a message is composed, the hooks `text-mode-hook' and
`mh-letter-mode-hook' are run.

\\{mh-letter-mode-map}"

  (or mh-user-path (mh-find-path))
  (make-local-variable 'mh-send-args)
  (make-local-variable 'mh-annotate-char)
  (make-local-variable 'mh-annotate-field)
  (make-local-variable 'mh-previous-window-config)
  (make-local-variable 'mh-sent-from-folder)
  (make-local-variable 'mh-sent-from-msg)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mh-mail-header-separator) ;override sendmail.el
  (make-local-variable 'mh-help-messages)
  (setq mh-help-messages mh-letter-mode-help-messages)

  ;; From sendmail.el for proper paragraph fill
  ;; sendmail.el also sets a normal-auto-fill-function (not done here)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'mh-fill-paragraph-function)
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp
	(concat adaptive-fill-regexp
		"\\|[ \t]*[-[:alnum:]]*>+[ \t]*"))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (setq adaptive-fill-first-line-regexp
	(concat adaptive-fill-first-line-regexp
		"\\|[ \t]*[-[:alnum:]]*>+[ \t]*"))
  ;; `-- ' precedes the signature.  `-----' appears at the start of the
  ;; lines that delimit forwarded messages.
  ;; Lines containing just >= 3 dashes, perhaps after whitespace,
  ;; are also sometimes used and should be separators.
  (setq paragraph-start (concat (regexp-quote mail-header-separator)
                         "\\|\t*\\([-|#;>* ]\\|(?[0-9]+[.)]\\)+$"
                         "\\|[ \t]*[[:alnum:]]*>+[ \t]*$\\|[ \t]*$\\|"
                         "-- $\\|---+$\\|"
                         page-delimiter))
  (setq paragraph-separate paragraph-start)
  ;; --- End of code from sendmail.el ---

  (if (and (boundp 'tool-bar-mode) tool-bar-mode)
      (set (make-local-variable 'tool-bar-map) mh-letter-tool-bar-map))
  (make-local-variable 'font-lock-defaults)
  (cond
   ((or (equal mh-highlight-citation-p 'font-lock)
        (equal mh-highlight-citation-p 'gnus))
    ;; Let's use font-lock even if gnus is used in show-mode.  The reason
    ;; is that gnus uses static text properties which are not appropriate
    ;; for a buffer that will be edited.  So the choice here is either fontify
    ;; the citations and header...
    (setq font-lock-defaults '(mh-show-font-lock-keywords-with-cite t)))
   (t
    ;; ...or the header only
    (setq font-lock-defaults '(mh-show-font-lock-keywords t))))
  (easy-menu-add mh-letter-menu)
  ;; See if a "forw: -mime" message containing a MIME composition.
  ;; mode clears local vars, so can't do this in mh-forward.
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward (format "^\\(%s\\)?$" mail-header-separator) nil t)
               (= 0 (forward-line 1))
               (looking-at "^#forw"))
      (require 'mh-mime)    ;Need mh-mhn-compose-insert-flag local var
      (setq mh-mhn-compose-insert-flag t)))
  (setq fill-column mh-letter-fill-column)
  ;; if text-mode-hook turned on auto-fill, tune it for messages
  (when auto-fill-function
    (make-local-variable 'auto-fill-function)
    (setq auto-fill-function 'mh-auto-fill-for-letter)))

(defun mh-auto-fill-for-letter ()
  "Perform auto-fill for message.
Header is treated specially by inserting a tab before continuation lines."
  (if (mh-in-header-p)
      (let ((fill-prefix "\t"))
	(do-auto-fill))
    (do-auto-fill)))

(defun mh-insert-header-separator ()
  "Insert `mh-mail-header-separator', if absent."
  (save-excursion
    (goto-char (point-min))
    (rfc822-goto-eoh)
    (if (looking-at "$")
	(insert mh-mail-header-separator))))

(defun mh-to-field ()
  "Move point to the end of a specified header field.
The field is indicated by the previous keystroke (the last keystroke
of the command) according to the list in the variable `mh-to-field-choices'.
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
  "Insert the file named by `mh-signature-file-name' at point.
The value of `mh-letter-insert-signature-hook' is a list of functions to be
called, with no arguments, before the signature is actually inserted."
  (interactive)
  (let ((mh-signature-file-name mh-signature-file-name))
    (run-hooks 'mh-letter-insert-signature-hook)
    (if mh-signature-file-name
	(insert-file-contents mh-signature-file-name)))
  (force-mode-line-update))

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

(defun mh-insert-x-face ()
  "Append X-Face field to header.
If the field already exists, this function does nothing."
  (when (and (file-exists-p mh-x-face-file)
             (file-readable-p mh-x-face-file))
    (save-excursion
      (when (null (mh-position-on-field "X-Face"))
        (insert "X-Face: ")
        (goto-char (+ (point) (cadr (insert-file-contents mh-x-face-file))))
        (if (not (looking-at "^"))
            (insert "\n"))))))

(defun mh-insert-x-mailer ()
  "Append an X-Mailer field to the header.
The versions of MH-E, Emacs, and MH are shown."

  ;; Lazily initialize mh-x-mailer-string.
  (when (null mh-x-mailer-string)
    (save-window-excursion
      (mh-version)
      (set-buffer mh-temp-buffer)
      (if mh-nmh-flag
	  (search-forward-regexp "^nmh-\\(\\S +\\)")
	(search-forward-regexp "^MH \\(\\S +\\)" nil t))
      (let ((x-mailer-mh (buffer-substring (match-beginning 1) (match-end 1))))
	(setq mh-x-mailer-string
	      (format "MH-E %s; %s %s; %s %d.%d"
		      mh-version (if mh-nmh-flag "nmh" "MH") x-mailer-mh
                      (if mh-xemacs-flag
                          "XEmacs"
                        "Emacs")
		      emacs-major-version emacs-minor-version)))
      (kill-buffer mh-temp-buffer)))
  ;; Insert X-Mailer, but only if it doesn't already exist.
  (save-excursion
    (when (null (mh-goto-header-field "X-Mailer"))
	(mh-insert-fields "X-Mailer:" mh-x-mailer-string))))

(defun mh-regexp-in-field-p (regexp &rest fields)
  "Non-nil means REGEXP was found in FIELDS."
  (save-excursion
    (let ((search-result nil)
          (field))
      (while fields
        (setq field (car fields))
        (if (and (mh-goto-header-field field)
                 (re-search-forward
                  regexp (save-excursion (mh-header-field-end)(point)) t))
            (setq fields nil
                  search-result t)
          (setq fields (cdr fields))))
      search-result)))

(defun mh-insert-mail-followup-to ()
  "Insert Mail-Followup-To: if To or Cc match `mh-insert-mail-followup-to-list'."
  (save-excursion
    (if (and (or (mh-goto-header-field "To:")(mh-goto-header-field "cc:"))
             (not (mh-goto-header-field "Mail-Followup-To: ")))
        (let ((list mh-insert-mail-followup-to-list))
          (while list
            (let ((regexp (nth 0 (car list)))
                  (entry  (nth 1 (car list))))
              (when (mh-regexp-in-field-p regexp "To:" "cc:")
                (if (mh-goto-header-field "Mail-Followup-To: ")
                    (insert entry ", ")
                  (mh-goto-header-end 0)
                  (insert "Mail-Followup-To: " entry "\n")))
              (setq list (cdr list))))))))

(defun mh-compose-and-send-mail (draft send-args
				       sent-from-folder sent-from-msg
				       to subject cc
				       annotate-char annotate-field
				       config)
  "Edit and compose a draft message in buffer DRAFT and send or save it.
SEND-ARGS is the argument passed to the send command.
SENT-FROM-FOLDER is buffer containing scan listing of current folder, or
nil if none exists.
SENT-FROM-MSG is the message number or sequence name or nil.
The TO, SUBJECT, and CC fields are passed to the
`mh-compose-letter-function'.
If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of the
message.  In that case, the ANNOTATE-FIELD is used to build a string
for `mh-annotate-msg'.
CONFIG is the window configuration to restore after sending the letter."
  (pop-to-buffer draft)
  (if mh-insert-mail-followup-to-flag (mh-insert-mail-followup-to))
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

(defun mh-letter-mode-message ()
  "Display a help message for users of `mh-letter-mode'.
This should be the last function called when composing the draft."
  (message "%s" (substitute-command-keys
		 (concat "Type \\[mh-send-letter] to send message, "
			 "\\[mh-help] for help."))))

(defun mh-send-letter (&optional arg)
  "Send the draft letter in the current buffer.
If optional prefix argument ARG is provided, monitor delivery.
The value of `mh-before-send-letter-hook' is a list of functions to be called,
with no arguments, before doing anything.
Run `\\[mh-edit-mhn]' if variable `mh-mhn-compose-insert-flag' is set."
  (interactive "P")
  (run-hooks 'mh-before-send-letter-hook)
  (cond
   ((and (boundp 'mh-mhn-compose-insert-flag)
         mh-mhn-compose-insert-flag)
    (mh-edit-mhn))
   ((and (boundp 'mh-mml-compose-insert-flag)
         mh-mml-compose-insert-flag)
    (mh-mml-to-mime)))
  (if mh-insert-x-mailer-flag (mh-insert-x-mailer))
  (mh-insert-x-face)
  (save-buffer)
  (message "Sending...")
  (let ((draft-buffer (current-buffer))
	(file-name buffer-file-name)
	(config mh-previous-window-config)
	(coding-system-for-write
	 (if (and (local-variable-p 'buffer-file-coding-system
                                    (current-buffer)) ;XEmacs needs two args
		  ;; We're not sure why, but buffer-file-coding-system
		  ;; tends to get set to undecided-unix.
		  (not (memq buffer-file-coding-system
			     '(undecided undecided-unix undecided-dos))))
	     buffer-file-coding-system
	   (or (and (boundp 'sendmail-coding-system) sendmail-coding-system)
	       (and (boundp 'default-buffer-file-coding-system )
                    default-buffer-file-coding-system)
	       'iso-latin-1))))
    ;; The default BCC encapsulation will make a MIME message unreadable.
    ;; With nmh use the -mime arg to prevent this.
    (if (and mh-nmh-flag
	 (mh-goto-header-field "Bcc:")
	 (mh-goto-header-field "Content-Type:"))
	(setq mh-send-args (format "-mime %s" mh-send-args)))
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
Removes the message's headers using `mh-invisible-headers'.  Prefixes each
non-blank line with `mh-ins-buf-prefix', unless `mh-yank-from-start-of-msg'
is set for supercite and then use it to format the message.
Prompts for FOLDER and MESSAGE.  If prefix argument VERBATIM provided, do
not indent and do not delete headers.  Leaves the mark before the letter
and point after it."
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
      (insert-file-contents
       (expand-file-name message (mh-expand-file-name folder)))
      (when (not verbatim)
        (mh-clean-msg-header start mh-invisible-headers mh-visible-headers)
        (goto-char (point-max))     ;Needed for sc-cite-original
        (push-mark)                 ;Needed for sc-cite-original
        (goto-char (point-min))     ;Needed for sc-cite-original
        (mh-insert-prefix-string mh-ins-buf-prefix)))))

(defun mh-extract-from-attribution ()
  "Extract phrase or comment from From header field."
  (save-excursion
    (if (not (mh-goto-header-field "From: "))
        nil
      (skip-chars-forward " ")
      (cond
       ((looking-at "\"\\([^\"\n]+\\)\" \\(<.+>\\)")
        (format "%s %s %s" (match-string 1)(match-string 2)
                mh-extract-from-attribution-verb))
       ((looking-at "\\([^<\n]+<.+>\\)$")
        (format "%s %s" (match-string 1) mh-extract-from-attribution-verb))
       ((looking-at "\\([^ ]+@[^ ]+\\) +(\\(.+\\))$")
        (format "%s <%s> %s" (match-string 2)(match-string 1)
                mh-extract-from-attribution-verb))
       ((looking-at " *\\(.+\\)$")
        (format "%s %s" (match-string 1) mh-extract-from-attribution-verb))))))

(defun mh-yank-cur-msg ()
  "Insert the current message into the draft buffer.
Prefix each non-blank line in the message with the string in
`mh-ins-buf-prefix'.  If a region is set in the message's buffer, then
only the region will be inserted.  Otherwise, the entire message will
be inserted if `mh-yank-from-start-of-msg' is non-nil.  If this variable
is nil, the portion of the message following the point will be yanked.
If `mh-delete-yanked-msg-window-flag' is non-nil, any window displaying the
yanked message will be deleted."
  (interactive)
  (if (and mh-sent-from-folder
           (save-excursion (set-buffer mh-sent-from-folder) mh-show-buffer)
           (save-excursion (set-buffer mh-sent-from-folder)
                           (get-buffer mh-show-buffer))
           mh-sent-from-msg)
      (let ((to-point (point))
	    (to-buffer (current-buffer)))
	(set-buffer mh-sent-from-folder)
	(if mh-delete-yanked-msg-window-flag
	    (delete-windows-on mh-show-buffer))
	(set-buffer mh-show-buffer)	; Find displayed message
	(let* ((from-attr (mh-extract-from-attribution))
	       (yank-region (mh-mark-active-p nil))
               (mh-ins-str
                (cond ((and yank-region
                            (or (eq 'supercite mh-yank-from-start-of-msg)
                                (eq 'autosupercite mh-yank-from-start-of-msg)
                                (eq t mh-yank-from-start-of-msg)))
                       ;; supercite needs the full header
                       (concat
                        (buffer-substring (point-min) (mail-header-end))
                        "\n"
                        (buffer-substring (region-beginning) (region-end))))
                      (yank-region
                       (buffer-substring (region-beginning) (region-end)))
                      ((or (eq 'body mh-yank-from-start-of-msg)
                           (eq 'attribution
                               mh-yank-from-start-of-msg)
                           (eq 'autoattrib
                               mh-yank-from-start-of-msg))
                       (buffer-substring
                        (save-excursion
                          (goto-char (point-min))
                          (mh-goto-header-end 1)
                          (point))
                        (point-max)))
                      ((or (eq 'supercite mh-yank-from-start-of-msg)
                           (eq 'autosupercite mh-yank-from-start-of-msg)
                           (eq t mh-yank-from-start-of-msg))
                       (buffer-substring (point-min) (point-max)))
                      (t
                       (buffer-substring (point) (point-max))))))
	  (set-buffer to-buffer)
	  (save-restriction
	    (narrow-to-region to-point to-point)
	    (insert (mh-filter-out-non-text mh-ins-str))
            (goto-char (point-max))     ;Needed for sc-cite-original
	    (push-mark)                 ;Needed for sc-cite-original
            (goto-char (point-min))     ;Needed for sc-cite-original
	    (mh-insert-prefix-string mh-ins-buf-prefix)
            (if (or (eq 'attribution mh-yank-from-start-of-msg)
                    (eq 'autoattrib mh-yank-from-start-of-msg))
                (insert from-attr "\n\n"))
	    ;; If the user has selected a region, he has already "edited" the
	    ;; text, so leave the cursor at the end of the yanked text. In
	    ;; either case, leave a mark at the opposite end of the included
	    ;; text to make it easy to jump or delete to the other end of the
	    ;; text.
	    (push-mark)
	    (goto-char (point-max))
	    (if (null yank-region)
		(mh-exchange-point-and-mark-preserving-active-mark)))))
    (error "There is no current message")))

(defun mh-filter-out-non-text (string)
  "Return STRING but without adornments such as MIME buttons and smileys."
  (with-temp-buffer
    ;; Insert the string to filter
    (insert string)
    (goto-char (point-min))
    
    ;; Remove the MIME buttons
    (let ((can-move-forward t)
          (in-button nil))
      (while can-move-forward
        (cond ((and (not (get-text-property (point) 'mh-data))
                    in-button)
               (delete-region (save-excursion (forward-line -1) (point))
                              (point))
               (setq in-button nil))
              ((get-text-property (point) 'mh-data)
               (delete-region (point)
                              (save-excursion (forward-line) (point)))
               (setq in-button t))
              (t (setq can-move-forward (= (forward-line) 0))))))

    ;; Return the contents without properties... This gets rid of emphasis
    ;; and smileys
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mh-insert-prefix-string (mh-ins-string)
  "Insert prefix string before each line in buffer.
The inserted letter is cited using `sc-cite-original' if
`mh-yank-from-start-of-msg' is one of 'supercite or 'autosupercite. Otherwise,
simply insert MH-INS-STRING before each line."
  (goto-char (point-min))
  (cond ((or (eq mh-yank-from-start-of-msg 'supercite)
             (eq mh-yank-from-start-of-msg 'autosupercite))
         (sc-cite-original))
        (mail-citation-hook
	 (run-hooks 'mail-citation-hook))
	(mh-yank-hooks			;old hook name
	 (run-hooks 'mh-yank-hooks))
	(t
	 (or (bolp) (forward-line 1))
         (while (< (point) (point-max))
           (insert mh-ins-string)
           (forward-line 1))
         (goto-char (point-min)))))     ;leave point like sc-cite-original

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

(defun mh-current-fill-prefix ()
  "Return the `fill-prefix' on the current line as a string."
  (save-excursion
    (beginning-of-line)
    ;; This assumes that the major-mode sets up adaptive-fill-regexp
    ;; correctly such as mh-letter-mode or sendmail.el's mail-mode.  But
    ;; perhaps I should use the variable and simply inserts its value here,
    ;; and set it locally in a let scope.  --psg
    (if (re-search-forward adaptive-fill-regexp nil t)
        (match-string 0)
      "")))

(defun mh-open-line ()
  "Insert a newline and leave point after it.
In addition, insert newline and quoting characters before text after point.
This is useful in breaking up paragraphs in replies."
  (interactive)
  (let ((column (current-column))
        (prefix (mh-current-fill-prefix)))
    (if (> (length prefix) column)
        (message "Sorry, point seems to be within the line prefix")
      (newline 2)
      (insert prefix)
      (while (> column (current-column))
        (insert " "))
      (forward-line -1))))

;;; Build the letter-mode keymap:
;;; If this changes, modify mh-letter-mode-help-messages accordingly, above.
(gnus-define-keys  mh-letter-mode-map
  "\C-c?"		mh-help
  "\C-c\C-c"		mh-send-letter
  "\C-c\C-e"		mh-edit-mhn
  "\C-c\C-f\C-b"	mh-to-field
  "\C-c\C-f\C-c"	mh-to-field
  "\C-c\C-f\C-d"	mh-to-field
  "\C-c\C-f\C-f"	mh-to-fcc
  "\C-c\C-f\C-r"	mh-to-field
  "\C-c\C-f\C-s"	mh-to-field
  "\C-c\C-f\C-t"	mh-to-field
  "\C-c\C-fb"		mh-to-field
  "\C-c\C-fc"		mh-to-field
  "\C-c\C-fd"		mh-to-field
  "\C-c\C-ff"		mh-to-fcc
  "\C-c\C-fr"		mh-to-field
  "\C-c\C-fs"		mh-to-field
  "\C-c\C-ft"		mh-to-field
  "\C-c\C-i"		mh-insert-letter
  "\C-c\C-m\C-e"	mh-mml-secure-message-encrypt-pgpmime
  "\C-c\C-m\C-f"	mh-compose-forward
  "\C-c\C-m\C-i"	mh-compose-insertion
  "\C-c\C-m\C-m"	mh-mml-to-mime
  "\C-c\C-m\C-s"	mh-mml-secure-message-sign-pgpmime
  "\C-c\C-m\C-u"	mh-revert-mhn-edit
  "\C-c\C-me"		mh-mml-secure-message-encrypt-pgpmime
  "\C-c\C-mf"		mh-compose-forward
  "\C-c\C-mi"		mh-compose-insertion
  "\C-c\C-mm"		mh-mml-to-mime
  "\C-c\C-ms"		mh-mml-secure-message-sign-pgpmime
  "\C-c\C-mu"		mh-revert-mhn-edit
  "\C-c\C-o"		mh-open-line
  "\C-c\C-q"		mh-fully-kill-draft
  "\C-c\C-\\"		mh-fully-kill-draft ;if no C-q
  "\C-c\C-s"		mh-insert-signature
  "\C-c\C-^"		mh-insert-signature ;if no C-s
  "\C-c\C-w"		mh-check-whom
  "\C-c\C-y"		mh-yank-cur-msg)

;; "C-c /" prefix is used in mh-letter-mode by pgp.el and mailcrypt.el.

(defun mh-customize ()
  "Customize MH-E variables."
  (interactive)
  (customize-group 'mh))

(provide 'mh-comp)

;;; Local Variables:
;;; sentence-end-double-space: nil
;;; End:

;;; mh-comp.el ends here
