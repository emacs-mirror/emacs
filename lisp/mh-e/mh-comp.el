;;; mh-comp.el --- MH-E functions for composing messages

;; Copyright (C) 1993, 1995, 1997,
;;  2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Internal support for MH-E package.

;;; Change Log:

;;; Code:

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-e)
(require 'gnus-util)
(require 'easymenu)
(require 'mh-gnus)
(eval-when (compile load eval)
  (ignore-errors (require 'mailabbrev)))

;; Shush the byte-compiler
(defvar adaptive-fill-first-line-regexp)
(defvar font-lock-defaults)
(defvar mark-active)
(defvar sendmail-coding-system)
(defvar mh-identity-list)
(defvar mh-identity-default)
(defvar mh-mml-mode-default)
(defvar mh-identity-menu)

;;; Autoloads
(autoload 'mail-mode-fill-paragraph "sendmail")
(autoload 'mm-handle-displayed-p "mm-decode")

(autoload 'sc-cite-original "sc"
  "Workhorse citing function which performs the initial citation.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard.  See `sc-describe'
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

;;; Scan Line Formats

(defvar mh-note-repl ?-
  "Messages that have been replied to are marked by this character.")

(defvar mh-note-forw ?F
  "Messages that have been forwarded are marked by this character.")

(defvar mh-note-dist ?R
  "Messages that have been redistributed are marked by this character.")

(defvar mh-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the citation
text as modified.

This is a normal hook, misnamed for historical reasons.
It is semi-obsolete and is only used if `mail-citation-hook' is nil.")

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
message. Only used if `(mh-variant-p 'nmh)' is non-nil.
Default is \"replgroupcomps\".
If not an absolute file name, the file is searched for first in the user's MH
directory, then in the system MH lib directory.")

(defvar mh-rejected-letter-start
  (format "^%s$"
          (regexp-opt
           '("Content-Type: message/rfc822" ;MIME MDN
             "------ This is a copy of the message, including all the headers. ------";from exim
	     "--- Below this line is a copy of the message."; from qmail
             "   ----- Unsent message follows -----" ;from sendmail V5
             " --------Unsent Message below:" ; from sendmail at BU
             "   ----- Original message follows -----" ;from sendmail V8
             "------- Unsent Draft"     ;from MH itself
             "----------  Original Message  ----------" ;from zmailer
             "  --- The unsent message follows ---" ;from AIX mail system
             "    Your message follows:" ;from MMDF-II
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

(defvar mh-insert-auto-fields-done-local nil
  "Buffer-local variable set when `mh-insert-auto-fields' called successfully.")
(make-variable-buffer-local 'mh-insert-auto-fields-done-local)

;;;###autoload
(defun mh-smail ()
  "Compose and send mail with the MH mail system.
This function is an entry point to MH-E, the Emacs interface to the MH mail
system.

See `mh-send' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))

(defvar mh-error-if-no-draft nil)       ;raise error over using old draft

;;;###autoload
(defun mh-smail-batch (&optional to subject other-headers &rest ignored)
  "Set up a mail composition draft with the MH mail system.
This function is an entry point to MH-E, the Emacs interface to the MH mail
system. This function does not prompt the user for any header fields, and thus
is suitable for use by programs that want to create a mail buffer. Users
should use `mh-smail' to compose mail.

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

;;;###mh-autoload
(defun mh-edit-again (msg)
  "Clean up a draft or a message MSG previously sent and make it resendable.
Default is the current message.
The variable `mh-new-draft-cleaned-headers' specifies the headers to remove.

See also `mh-send'."
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
    (mh-letter-mode-message)
    (mh-letter-adjust-point)))

;;;###mh-autoload
(defun mh-extract-rejected-mail (msg)
  "Extract message MSG returned by the mail system and make it resendable.
Default is the current message.  The variable `mh-new-draft-cleaned-headers'
gives the headers to clean out of the original message.

See also `mh-send'."
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
           (message "Does not appear to be a rejected letter")))
    (mh-insert-header-separator)
    (goto-char (point-min))
    (save-buffer)
    (mh-compose-and-send-mail draft "" from-folder msg
                              (mh-get-header-field "To:")
                              (mh-get-header-field "From:")
                              (mh-get-header-field "Cc:")
                              nil nil config)
    (mh-letter-mode-message)))

;;;###mh-autoload
(defun mh-forward (to cc &optional range)
  "Forward messages to the recipients TO and CC.
Use optional RANGE argument to specify a message or sequence to forward.
Default is the displayed message.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

See also `mh-send'."
  (interactive (list (mh-interactive-read-address "To: ")
                     (mh-interactive-read-address "Cc: ")
                     (mh-interactive-range "Forward")))
  (let* ((folder mh-current-folder)
         (msgs (mh-range-to-msg-list range))
         (config (current-window-configuration))
         (fwd-msg-file (mh-msg-filename (car msgs) folder))
         ;; forw always leaves file in "draft" since it doesn't have -draft
         (draft-name (expand-file-name "draft" mh-user-path))
         (draft (cond ((or (not (file-exists-p draft-name))
                           (y-or-n-p "The file 'draft' exists.  Discard it? "))
                       (mh-exec-cmd "forw" "-build"
                                    (if (and (mh-variant-p 'nmh)
                                             mh-compose-forward-as-mime-flag)
                                        "-mime")
                                    mh-current-folder
                                    (mh-coalesce-msg-list msgs))
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
             (mh-forwarded-letter-subject orig-from orig-subject)))
        (mh-insert-fields "Subject:" forw-subject)
        (goto-char (point-min))
        ;; If using MML, translate mhn
        (if (equal mh-compose-insertion 'gnus)
            (save-excursion
              (goto-char (mh-mail-header-end))
              (while
                  (re-search-forward
                   "^#forw \\[\\([^]]+\\)\\] \\(+\\S-+\\) \\(.*\\)$"
                   (point-max) t)
                (let ((description (if (equal (match-string 1)
                                              "forwarded messages")
                                       "forwarded message %d"
                                     (match-string 1)))
                      (msgs (split-string (match-string 3)))
                      (i 0))
                  (beginning-of-line)
                  (delete-region (point) (progn (forward-line 1) (point)))
                  (dolist (msg msgs)
                    (setq i (1+ i))
                    (mh-mml-forward-message (format description i)
                                            folder msg))))))
        ;; Postition just before forwarded message
        (if (re-search-forward "^------- Forwarded Message" nil t)
            (forward-line -1)
          (goto-char (mh-mail-header-end))
          (forward-line 1))
        (delete-other-windows)
        (mh-add-msgs-to-seq msgs 'forwarded t)
        (mh-compose-and-send-mail draft "" folder msgs
                                  to forw-subject cc
                                  mh-note-forw "Forwarded:"
                                  config)
        (mh-letter-mode-message)
        (mh-letter-adjust-point)
        (run-hooks 'mh-forward-hook)))))

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
This function is an entry point to MH-E, the Emacs interface to the MH mail
system.

See `mh-send' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))

;;;###mh-autoload
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
      (mh-clean-msg-header
       (point-min)
       "^Message-Id:\\|^Received:\\|^Return-Path:\\|^Sender:\\|^Date:\\|^From:"
       nil)
      (save-buffer)
      (message "Redistributing...")
      (let ((env "mhdist=1"))
        ;; Setup environment...
        (setq env (concat env " mhaltmsg=" (if mh-redist-full-contents
                                               buffer-file-name
                                             (mh-msg-filename msg folder))))
        (unless mh-redist-full-contents
          (setq env (concat env " mhannotate=1")))
        ;; Redistribute...
        (if mh-redist-background
            (mh-exec-cmd-env-daemon env mh-send-prog nil buffer-file-name)
          (mh-exec-cmd-error env mh-send-prog "-push" buffer-file-name))
        ;; Annotate...
        (mh-annotate-msg msg folder mh-note-dist
                         "-component" "Resent:"
                         "-text" (format "\"%s %s\"" to cc)))
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
           (let ((number-start (mh-search-from-end ?/ buffer-file-name)))
             (car (read-from-string (substring buffer-file-name
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

;;;###mh-autoload
(defun mh-reply (message &optional reply-to includep)
  "Reply to MESSAGE.
Default is the displayed message.
If the optional argument REPLY-TO is not given, prompts for type of addresses
to reply to:
   from    sender only,
   to      sender and primary recipients,
   cc/all  sender and all recipients.
If optional prefix argument INCLUDEP provided, then include the message
in the reply using filter `mhl.reply' in your MH directory.
If the file named by `mh-repl-formfile' exists, it is used as a skeleton for
the reply. If REPLY-TO is cc or all and you're using either the nmh or GNU
mailutils variants and the file names by `mh-repl-group-formfile' exists, it
is used instead.

See also `mh-send'."
  (interactive (list
                (mh-get-msg-num t)
                (let ((minibuffer-help-form
                       "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
                  (or mh-reply-default-reply-to
                      (completing-read "Reply to whom: [from] "
                                       '(("from") ("to") ("cc") ("all"))
                                       nil
                                       t)))
                current-prefix-arg))
  (let* ((folder mh-current-folder)
         (show-buffer mh-show-buffer)
         (config (current-window-configuration))
         (group-reply (or (equal reply-to "cc") (equal reply-to "all")))
         (form-file (cond ((and (mh-variant-p 'nmh 'mu-mh) group-reply
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
                       (group-reply (if (mh-variant-p 'nmh 'mu-mh)
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

;;;###mh-autoload
(defun mh-send (to cc subject)
  "Compose and send a letter.
Do not call this function from outside MH-E; use \\[mh-smail] instead.

The file named by `mh-comp-formfile' will be used as the form.
The letter is composed in `mh-letter-mode'; see its documentation for more
details.
If `mh-compose-letter-function' is defined, it is called on the draft and
passed three arguments: TO, CC, and SUBJECT."
  (interactive (list
                (mh-interactive-read-address "To: ")
                (mh-interactive-read-address "Cc: ")
                (mh-interactive-read-string "Subject: ")))
  (let ((config (current-window-configuration)))
    (delete-other-windows)
    (mh-send-sub to cc subject config)))

;;;###mh-autoload
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
                (mh-interactive-read-address "To: ")
                (mh-interactive-read-address "Cc: ")
                (mh-interactive-read-string "Subject: ")))
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
                      (error "Can't find components file \"%s\""
                             components))))
                  nil)))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (goto-char (point-max))
      (mh-compose-and-send-mail draft "" folder msg-num
                                to subject cc
                                nil nil config)
      (mh-letter-mode-message)
      (mh-letter-adjust-point))))

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
           (pop-to-buffer "draft")      ; Create if necessary
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
      (save-buffer))                    ; Do not reuse draft name
  (buffer-name))

(defun mh-new-draft-name ()
  "Return the pathname of folder for draft messages."
  (save-excursion
    (mh-exec-cmd-quiet t "mhpath" mh-draft-folder "new")
    (buffer-substring (point-min) (1- (point-max)))))

(defun mh-annotate-msg (msg buffer note &rest args)
  "Mark MSG in BUFFER with character NOTE and annotate message with ARGS.
MSG can be a message number, a list of message numbers, or a sequence."
  (apply 'mh-exec-cmd "anno" buffer
         (if (listp msg) (append msg args) (cons msg args)))
  (save-excursion
    (cond ((get-buffer buffer)          ; Buffer may be deleted
           (set-buffer buffer)
           (mh-iterate-on-range nil msg
             (mh-notate nil note (1+ mh-cmd-note)))))))

(defun mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUES pairs in the current buffer.
If the field exists, append the value to it.
Do not insert any pairs whose value is the empty string."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
            (value (car (cdr name-values))))
        (if (not (string-match "^.*:$" field-name))
            (setq field-name (concat field-name ":")))
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

;;;###mh-autoload
(defun mh-get-header-field (field)
  "Find and return the body of FIELD in the mail header.
Returns the empty string if the field is not in the header of the
current buffer."
  (if (mh-goto-header-field field)
      (progn
        (skip-chars-forward " \t")      ;strip leading white space in body
        (let ((start (point)))
          (mh-header-field-end)
          (buffer-substring-no-properties start (point))))
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

(defun mh-extract-from-header-value ()
  "Extract From: string from header."
  (save-excursion
    (if (not (mh-goto-header-field "From:"))
        nil
      (skip-chars-forward " \t")
      (buffer-substring-no-properties
       (point) (progn (mh-header-field-end)(point))))))



;;; Mode for composing and sending a draft message.

(put 'mh-letter-mode 'mode-class 'special)

;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
(eval-when-compile (defvar mh-letter-menu nil))
(easy-menu-define
  mh-letter-menu mh-letter-mode-map "Menu for MH-E letter mode."
  '("Letter"
    ["Send This Draft"          mh-send-letter t]
    ["Split Current Line"       mh-open-line t]
    ["Check Recipient"          mh-check-whom t]
    ["Yank Current Message"     mh-yank-cur-msg t]
    ["Insert a Message..."      mh-insert-letter t]
    ["Insert Signature"         mh-insert-signature t]
    ("Encrypt/Sign Message"
     ["Sign Message"
      mh-mml-secure-message-sign mh-gnus-pgp-support-flag]
     ["Encrypt Message"
      mh-mml-secure-message-encrypt mh-gnus-pgp-support-flag]
     ["Sign+Encrypt Message"
      mh-mml-secure-message-signencrypt mh-gnus-pgp-support-flag]
     ["Disable Security"
      mh-mml-unsecure-message mh-gnus-pgp-support-flag]
     "--"
     "Security Method"
     ["PGP (MIME)" (setq mh-mml-method-default "pgpmime")
      :style radio
      :selected (equal mh-mml-method-default "pgpmime")]
     ["PGP" (setq mh-mml-method-default "pgp")
      :style radio
      :selected (equal mh-mml-method-default "pgp")]
     ["S/MIME" (setq mh-mml-method-default "smime")
      :style radio
      :selected (equal mh-mml-method-default "smime")]
     "--"
     ["Save Method as Default"
      (customize-save-variable 'mh-mml-method-default mh-mml-method-default) t]
     )
    ["Compose Insertion (MIME)..."      mh-compose-insertion t]
    ["Compose Compressed tar (MIME)..."
     mh-mhn-compose-external-compressed-tar t]
    ["Compose Get File (MIME)..."       mh-mhn-compose-anon-ftp t]
    ["Compose Forward (MIME)..."        mh-compose-forward t]
    ;; The next two will have to be merged. But I also need to make sure the
    ;; user can't mix directives of both types.
    ["Pull in All Compositions (mhn)"
     mh-edit-mhn (mh-mhn-directive-present-p)]
    ["Pull in All Compositions (gnus)"
     mh-mml-to-mime (mh-mml-directive-present-p)]
    ["Revert to Non-MIME Edit (mhn)"
     mh-revert-mhn-edit (equal mh-compose-insertion 'mhn)]
    ["Kill This Draft"          mh-fully-kill-draft t]))

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
     "\t\t  Attachment:             \\[mh-compose-insertion]\n"
     "\t\t  Message to forward:     \\[mh-compose-forward]\n"
     "                          "
     "Security:"
     "\t\t  Encrypt message:          \\[mh-mml-secure-message-encrypt]"
     "\t\t  Sign+Encrypt message:     \\[mh-mml-secure-message-signencrypt]"
     "\t\t  Sign message:             \\[mh-mml-secure-message-sign]\n"
     "                          "
     "\t\t  Signature:              \\[mh-insert-signature]"))
  "Key binding cheat sheet.

This is an associative array which is used to show the most common commands.
The key is a prefix char. The value is one or more strings which are
concatenated together and displayed in the minibuffer if ? is pressed after
the prefix character. The special key nil is used to display the
non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.")

;;;###mh-autoload
(defun mh-fill-paragraph-function (arg)
  "Fill paragraph at or after point.
Prefix ARG means justify as well. This function enables `fill-paragraph' to
work better in MH-Letter mode."
  (interactive "P")
  (let ((fill-paragraph-function) (fill-prefix))
    (if (mh-in-header-p)
        (mail-mode-fill-paragraph arg)
      (fill-paragraph arg))))

;; Avoid compiler warnings in XEmacs and Emacs 20
(eval-when-compile
  (defvar tool-bar-mode)
  (defvar tool-bar-map))

;;;###autoload
(define-derived-mode mh-letter-mode text-mode "MH-Letter"
  "Mode for composing letters in MH-E.\\<mh-letter-mode-map>

When you have finished composing, type \\[mh-send-letter] to send the message
using the MH mail handling system.

There are two types of MIME directives used by MH-E: Gnus and MH. The option
`mh-compose-insertion' controls what type of directives are inserted by MH-E
commands. These directives can be converted to MIME body parts by running
\\[mh-edit-mhn] for mhn directives or \\[mh-mml-to-mime] for Gnus directives.
This step is mandatory if these directives are added manually. If the
directives are inserted with MH-E commands such as \\[mh-compose-insertion],
the directives are expanded automatically when the letter is sent.

Options that control this mode can be changed with
\\[customize-group]; specify the \"mh-compose\" group.

When a message is composed, the hooks `text-mode-hook' and
`mh-letter-mode-hook' are run.

\\{mh-letter-mode-map}"
  (mh-find-path)
  (make-local-variable 'mh-send-args)
  (make-local-variable 'mh-annotate-char)
  (make-local-variable 'mh-annotate-field)
  (make-local-variable 'mh-previous-window-config)
  (make-local-variable 'mh-sent-from-folder)
  (make-local-variable 'mh-sent-from-msg)
  ;; Set the local value of mh-mail-header-separator according to what is
  ;; present in the buffer...
  (set (make-local-variable 'mh-mail-header-separator)
       (save-excursion
         (goto-char (mh-mail-header-end))
         (buffer-substring-no-properties (point) (line-end-position))))
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mh-mail-header-separator) ;override sendmail.el
  (make-local-variable 'mh-help-messages)
  (setq mh-help-messages mh-letter-mode-help-messages)
  (setq buffer-invisibility-spec '((vanish . t) t))
  (set (make-local-variable 'line-move-ignore-invisible) t)

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

  ;; Enable undo since a show-mode buffer might have been reused.
  (buffer-enable-undo)
  (set (make-local-variable 'tool-bar-map) mh-letter-tool-bar-map)
  (mh-funcall-if-exists mh-toolbar-init :letter)
  (make-local-variable 'font-lock-defaults)
  (cond
   ((or (equal mh-highlight-citation-p 'font-lock)
        (equal mh-highlight-citation-p 'gnus))
    ;; Let's use font-lock even if gnus is used in show-mode.  The reason
    ;; is that gnus uses static text properties which are not appropriate
    ;; for a buffer that will be edited.  So the choice here is either fontify
    ;; the citations and header...
    (setq font-lock-defaults '(mh-letter-font-lock-keywords t)))
   (t
    ;; ...or the header only
    (setq font-lock-defaults '(mh-show-font-lock-keywords t))))
  (easy-menu-add mh-letter-menu)
  (setq fill-column mh-letter-fill-column)
  ;; If text-mode-hook turned on auto-fill, tune it for messages
  (when auto-fill-function
    (make-local-variable 'auto-fill-function)
    (setq auto-fill-function 'mh-auto-fill-for-letter)))

(defun mh-font-lock-field-data (limit)
  "Find header field region between point and LIMIT."
  (and (< (point) (mh-letter-header-end))
       (< (point) limit)
       (let ((end (min limit (mh-letter-header-end)))
             (point (point))
             data-end data-begin field)
         (end-of-line)
         (setq data-end (if (re-search-forward "^[^ \t]" end t)
                            (match-beginning 0)
                          end))
         (goto-char (1- data-end))
         (if (not (re-search-backward "\\(^[^ \t][^:]*\\):[ \t]*" nil t))
             (setq data-begin (point-min))
           (setq data-begin (match-end 0))
           (setq field (match-string 1)))
         (setq data-begin (max point data-begin))
         (goto-char (if (equal point data-end) (1+ data-end) data-end))
         (cond ((and field (mh-letter-skipped-header-field-p field))
                (set-match-data nil)
                nil)
               (t (set-match-data
                   (list data-begin data-end data-begin data-end))
                  t)))))

(defun mh-letter-header-end ()
  "Find the end of the message header.
This function is to be used only for font locking. It works by searching for
`mh-mail-header-separator' in the buffer."
  (save-excursion
    (goto-char (point-min))
    (cond ((equal mh-mail-header-separator "") (point-min))
          ((search-forward (format "\n%s\n" mh-mail-header-separator) nil t)
           (line-beginning-position 0))
          (t (point-min)))))

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

;;;###mh-autoload
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
                         (assoc (logior last-input-char ?`)
                                mh-to-field-choices))))
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

;;;###mh-autoload
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
                               (funcall
                                mh-default-folder-for-message-function)))
                        "")
                    t)))
  (let ((last-input-char ?\C-f))
    (expand-abbrev)
    (save-excursion
      (mh-to-field)
      (insert (if (mh-folder-name-p folder)
                  (substring folder 1)
                folder)))))

(defun mh-file-is-vcard-p (file)
  "Return t if FILE is a .vcf vcard."
  (let ((case-fold-search t))
    (and (stringp file)
         (file-exists-p file)
         (or (and (not (mh-have-file-command))
                  (not (null (string-match "\.vcf$" file))))
             (and (mh-have-file-command)
                  (string-equal "text/x-vcard" (mh-file-mime-type file)))))))

;;;###mh-autoload
(defun mh-insert-signature (&optional file)
  "Insert the signature specified by `mh-signature-file-name' or FILE at point.
A signature separator (`-- ') will be added if the signature block does not
contain one and `mh-signature-separator-flag' is on.
The value of `mh-letter-insert-signature-hook' is a list of functions to be
called, with no arguments, after the signature is inserted.
The signature can also be inserted with `mh-identity-list'."
(interactive)
  (save-excursion
    (insert "\n")
    (let ((mh-signature-file-name (or file mh-signature-file-name))
          (mh-mhn-p (mh-mhn-directive-present-p))
          (mh-mml-p (mh-mml-directive-present-p)))
      (save-restriction
        (narrow-to-region (point) (point))
        (cond
         ((mh-file-is-vcard-p mh-signature-file-name)
          (if (equal mh-compose-insertion 'gnus)
              (insert "<#part type=\"text/x-vcard\" filename=\""
                      mh-signature-file-name
                      "\" disposition=inline description=VCard>\n<#/part>")
            (insert "#text/x-vcard; name=\""
                    (file-name-nondirectory mh-signature-file-name)
                    "\" [VCard] " (expand-file-name mh-signature-file-name))))
         (t
          (cond
           (mh-mhn-p
            (insert "#\n" "Content-Description: Signature\n"))
           (mh-mml-p
            (mml-insert-tag 'part 'type "text/plain" 'disposition "inline"
                            'description "Signature")))
          (cond ((null mh-signature-file-name))
                ((and (stringp mh-signature-file-name)
                      (file-readable-p mh-signature-file-name))
                 (insert-file-contents mh-signature-file-name))
                ((functionp mh-signature-file-name)
                 (funcall mh-signature-file-name)))))
        (save-restriction
          (widen)
          (run-hooks 'mh-letter-insert-signature-hook))
        (goto-char (point-min))
        (when (and (not (mh-file-is-vcard-p mh-signature-file-name))
                   mh-signature-separator-flag
                   (> (point-max) (point-min))
                   (not (mh-signature-separator-p)))
          (cond (mh-mhn-p
                 (forward-line 2))
                (mh-mml-p
                 (forward-line 1)))
          (insert mh-signature-separator))
        (if (not (> (point-max) (point-min)))
            (message "No signature found")))))
  (force-mode-line-update))

;;;###mh-autoload
(defun mh-check-whom ()
  "Verify recipients of the current letter, showing expansion of any aliases."
  (interactive)
  (let ((file-name buffer-file-name))
    (save-buffer)
    (message "Checking recipients...")
    (mh-in-show-buffer (mh-recipients-buffer)
      (bury-buffer (current-buffer))
      (erase-buffer)
      (mh-exec-cmd-output "whom" t file-name))
    (message "Checking recipients...done")))

(defun mh-tidy-draft-buffer ()
  "Run when a draft buffer is destroyed."
  (let ((buffer (get-buffer mh-recipients-buffer)))
    (if buffer
	(kill-buffer buffer))))



;;; Routines to compose and send a letter.

(defun mh-insert-x-face ()
  "Append X-Face, Face or X-Image-URL field to header.
If the field already exists, this function does nothing."
  (when (and (file-exists-p mh-x-face-file)
             (file-readable-p mh-x-face-file))
    (save-excursion
      (unless (or (mh-position-on-field "X-Face")
                  (mh-position-on-field "Face")
                  (mh-position-on-field "X-Image-URL"))
        (save-excursion
          (goto-char (+ (point) (cadr (insert-file-contents mh-x-face-file))))
          (if (not (looking-at "^"))
              (insert "\n")))
        (unless (looking-at "\\(X-Face\\|Face\\|X-Image-URL\\): ")
          (insert "X-Face: "))))))

(defvar mh-x-mailer-string nil
  "*String containing the contents of the X-Mailer header field.
If nil, this variable is initialized to show the version of MH-E, Emacs, and
MH the first time a message is composed.")

(defun mh-insert-x-mailer ()
  "Append an X-Mailer field to the header.
The versions of MH-E, Emacs, and MH are shown."
  ;; Lazily initialize mh-x-mailer-string.
  (when (and mh-insert-x-mailer-flag (null mh-x-mailer-string))
    (setq mh-x-mailer-string
          (format "MH-E %s; %s; %sEmacs %s"
                  mh-version mh-variant-in-use
                  (if mh-xemacs-flag "X" "GNU ")
                  (cond ((not mh-xemacs-flag) emacs-version)
                        ((string-match "[0-9.]*\\( +\([ a-z]+[0-9]+\)\\)?"
                                       emacs-version)
                         (match-string 0 emacs-version))
                        (t (format "%s.%s" emacs-major-version
                                   emacs-minor-version))))))
  ;; Insert X-Mailer, but only if it doesn't already exist.
  (save-excursion
    (when (and mh-insert-x-mailer-flag
               (null (mh-goto-header-field "X-Mailer")))
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

;;;###mh-autoload
(defun mh-insert-auto-fields (&optional non-interactive)
  "Insert custom fields if To or Cc match `mh-auto-fields-list'.
Sets buffer-local `mh-insert-auto-fields-done-local' when done and inserted
something.  If NON-INTERACTIVE is non-nil, do not be verbose and only
attempt matches if `mh-insert-auto-fields-done-local' is nil.

An `identity' entry is skipped if one was already entered manually.

Return t if fields added; otherwise return nil."
  (interactive)
  (when (or (not non-interactive)
            (not mh-insert-auto-fields-done-local))
    (save-excursion
      (when (and (or (mh-goto-header-field "To:")
                     (mh-goto-header-field "cc:")))
        (let ((list mh-auto-fields-list)
              (fields-inserted nil))
          (while list
            (let ((regexp (nth 0 (car list)))
                  (entries (nth 1 (car list))))
              (when (mh-regexp-in-field-p regexp "To:" "cc:")
                (setq mh-insert-auto-fields-done-local t)
                (setq fields-inserted t)
                (if (not non-interactive)
                    (message "Fields for %s added" regexp))
                (let ((entry-list entries))
                  (while entry-list
                    (let ((field (caar entry-list))
                          (value (cdar entry-list)))
                      (cond
                       ((equal ":identity" field)
                        (when ;;(and (not mh-identity-local)
                            ;; Bug 1204506.  But do we need to be able
                            ;;  to set an identity manually that won't be
                            ;;  overridden by mh-insert-auto-fields?
                                   (assoc value mh-identity-list)
                                   ;;)
                          (mh-insert-identity value)))
                       (t
                        (mh-modify-header-field field value
                                                (equal field "From")))))
                    (setq entry-list (cdr entry-list))))))
            (setq list (cdr list)))
          fields-inserted)))))

(defun mh-modify-header-field (field value &optional overwrite-flag)
  "To header FIELD add VALUE.
If OVERWRITE-FLAG is non-nil then the old value, if present, is discarded."
  (cond ((and overwrite-flag
              (mh-goto-header-field (concat field ":")))
         (insert " " value)
         (delete-region (point) (line-end-position)))
        ((and (not overwrite-flag)
              (mh-regexp-in-field-p (concat "\\b" value "\\b") field))
         ;; Already there, do nothing.
         )
        ((and (not overwrite-flag)
              (mh-goto-header-field (concat field ":")))
         (insert " " value ","))
        (t
         (mh-goto-header-end 0)
         (insert field ": " value "\n"))))

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
  (mh-letter-mode)

  ;; Insert identity.
  (if (and (boundp 'mh-identity-default)
           mh-identity-default
           (not mh-identity-local))
      (mh-insert-identity mh-identity-default))
  (mh-identity-make-menu)
  (easy-menu-add mh-identity-menu)

  ;; Insert extra fields.
  (mh-insert-x-mailer)
  (mh-insert-x-face)

  (mh-letter-hide-all-skipped-fields)

  (setq mh-sent-from-folder sent-from-folder)
  (setq mh-sent-from-msg sent-from-msg)
  (setq mh-send-args send-args)
  (setq mh-annotate-char annotate-char)
  (setq mh-annotate-field annotate-field)
  (setq mh-previous-window-config config)
  (setq mode-line-buffer-identification (list "    {%b}"))
  (mh-logo-display)
  (mh-make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'mh-tidy-draft-buffer nil t)
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
                         "\\[mh-help] for help"))))

(defun mh-ascii-buffer-p ()
  "Check if current buffer is entirely composed of ASCII.
The function doesn't work for XEmacs since `find-charset-region' doesn't exist
there."
  (loop for charset in (mh-funcall-if-exists
                        find-charset-region (point-min) (point-max))
        unless (eq charset 'ascii) return nil
        finally return t))

;;;###mh-autoload
(defun mh-send-letter (&optional arg)
  "Send the draft letter in the current buffer.
If optional prefix argument ARG is provided, monitor delivery.
The value of `mh-before-send-letter-hook' is a list of functions to be called,
with no arguments, before doing anything.
Run `\\[mh-edit-mhn]' if mhn directives are present; otherwise
run `\\[mh-mml-to-mime]' if mml directives are present."
  (interactive "P")
  (run-hooks 'mh-before-send-letter-hook)
  (if (and (mh-insert-auto-fields t)
           mh-auto-fields-prompt-flag
           (goto-char (point-min)))
      (if (not (y-or-n-p "Auto fields inserted, send? "))
          (error "Send aborted")))
  (cond ((mh-mhn-directive-present-p)
         (mh-edit-mhn))
        ((or (mh-mml-directive-present-p) (not (mh-ascii-buffer-p)))
         (mh-mml-to-mime)))
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
    (if (and (mh-variant-p 'nmh)
             (mh-goto-header-field "Bcc:")
             (mh-goto-header-field "Content-Type:"))
        (setq mh-send-args (format "-mime %s" mh-send-args)))
    (cond (arg
           (pop-to-buffer mh-mail-delivery-buffer)
           (erase-buffer)
           (mh-exec-cmd-output mh-send-prog t "-watch" "-nopush"
                               "-nodraftfolder" mh-send-args file-name)
           (goto-char (point-max))      ; show the interesting part
           (recenter -1)
           (set-buffer draft-buffer))   ; for annotation below
          (t
           (mh-exec-cmd-daemon mh-send-prog nil "-nodraftfolder" "-noverbose"
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

;;;###mh-autoload
(defun mh-insert-letter (folder message verbatim)
  "Insert a message into the current letter.
Removes the header fields according to the variable
`mh-invisible-header-fields-compiled'.
Prefixes each non-blank line with `mh-ins-buf-prefix', unless
`mh-yank-from-start-of-msg' is set for supercite in which case supercite is
used to format the message.
Prompts for FOLDER and MESSAGE.  If prefix argument VERBATIM provided, do
not indent and do not delete headers.  Leaves the mark before the letter
and point after it."
  (interactive
   (list (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
         (read-input (concat "Message number"
                             (if (numberp mh-sent-from-msg)
                                 (format " (default %d): " mh-sent-from-msg)
                               ": ")))
         current-prefix-arg))
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((start (point-min)))
      (if (and (equal message "") (numberp mh-sent-from-msg))
          (setq message (int-to-string mh-sent-from-msg)))
      (insert-file-contents
       (expand-file-name message (mh-expand-file-name folder)))
      (when (not verbatim)
        (mh-clean-msg-header start mh-invisible-header-fields-compiled nil)
        (goto-char (point-max))         ;Needed for sc-cite-original
        (push-mark)                     ;Needed for sc-cite-original
        (goto-char (point-min))         ;Needed for sc-cite-original
        (mh-insert-prefix-string mh-ins-buf-prefix)))))

(defun mh-extract-from-attribution ()
  "Extract phrase or comment from From header field."
  (save-excursion
    (if (not (mh-goto-header-field "From: "))
        nil
      (skip-chars-forward " ")
      (cond
       ((looking-at "\"\\([^\"\n]+\\)\" \\(<.+>\\)")
        (format "%s %s " (match-string 1)(match-string 2)))
       ((looking-at "\\([^<\n]+<.+>\\)$")
        (format "%s " (match-string 1)))
       ((looking-at "\\([^ ]+@[^ ]+\\) +(\\(.+\\))$")
        (format "%s <%s> " (match-string 2)(match-string 1)))
       ((looking-at " *\\(.+\\)$")
        (format "%s " (match-string 1)))))))

;;;###mh-autoload
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
        (set-buffer mh-show-buffer)     ; Find displayed message
        (let* ((from-attr (mh-extract-from-attribution))
               (yank-region (mh-mark-active-p nil))
               (mh-ins-str
                (cond ((and yank-region
                            (or (eq 'supercite mh-yank-from-start-of-msg)
                                (eq 'autosupercite mh-yank-from-start-of-msg)
                                (eq t mh-yank-from-start-of-msg)))
                       ;; supercite needs the full header
                       (concat
                        (buffer-substring (point-min) (mh-mail-header-end))
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
            (when (or (eq 'attribution mh-yank-from-start-of-msg)
                      (eq 'autoattrib mh-yank-from-start-of-msg))
              (insert from-attr)
              (mh-identity-insert-attribution-verb nil)
              (insert "\n\n"))
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
               (delete-region (1- (point)) (point))
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
        (mh-yank-hooks                  ;old hook name
         (run-hooks 'mh-yank-hooks))
        (t
         (or (bolp) (forward-line 1))
         (while (< (point) (point-max))
           (insert mh-ins-string)
           (forward-line 1))
         (goto-char (point-min)))))     ;leave point like sc-cite-original

;;;###mh-autoload
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

;;;###mh-autoload
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

(mh-do-in-xemacs (defvar mail-abbrevs))

;;;###mh-autoload
(defun mh-complete-word (word choices begin end)
  "Complete WORD at from CHOICES.
Any match found replaces the text from BEGIN to END."
  (let ((completion (try-completion word choices))
        (completions-buffer "*Completions*"))
    (cond ((eq completion t)
           (ignore-errors
             (kill-buffer completions-buffer))
           (message "Completed: %s" word))
          ((null completion)
           (ignore-errors
             (kill-buffer completions-buffer))
           (message "No completion for `%s'" word))
          ((stringp completion)
           (if (equal word completion)
               (with-output-to-temp-buffer completions-buffer
                 (display-completion-list (all-completions word choices)))
             (ignore-errors
               (kill-buffer completions-buffer))
             (delete-region begin end)
             (insert completion))))))

;;;###mh-autoload
(defun mh-beginning-of-word (&optional n)
  "Return position of the N th word backwards."
  (unless n (setq n 1))
  (let ((syntax-table (syntax-table)))
    (unwind-protect
        (save-excursion
          (mh-mail-abbrev-make-syntax-table)
          (set-syntax-table mail-abbrev-syntax-table)
          (backward-word n)
          (point))
      (set-syntax-table syntax-table))))

(defun mh-folder-expand-at-point ()
  "Do folder name completion in Fcc header field."
  (let* ((end (point))
         (beg (mh-beginning-of-word))
         (folder (buffer-substring beg end))
         (leading-plus (and (> (length folder) 0) (equal (aref folder 0) ?+)))
         (last-slash (mh-search-from-end ?/ folder))
         (prefix (and last-slash (substring folder 0 last-slash)))
         (choices (mapcar #'(lambda (x)
                              (list (cond (prefix (format "%s/%s" prefix x))
                                          (leading-plus (format "+%s" x))
                                          (t x))))
                          (mh-folder-completion-function folder nil t))))
    (mh-complete-word folder choices beg end)))

(defvar mh-letter-complete-function-alist
  '((cc . mh-alias-letter-expand-alias)
    (bcc . mh-alias-letter-expand-alias)
    (dcc . mh-alias-letter-expand-alias)
    (fcc . mh-folder-expand-at-point)
    (from . mh-alias-letter-expand-alias)
    (mail-followup-to . mh-alias-letter-expand-alias)
    (reply-to . mh-alias-letter-expand-alias)
    (to . mh-alias-letter-expand-alias))
  "Alist of header fields and completion functions to use.")

(defun mh-letter-complete (arg)
  "Perform completion on header field or word preceding point.
If the field contains addresses (for example, `To:' or `Cc:') or folders (for
example, `Fcc:') then this function will provide alias completion. Elsewhere,
this function runs `mh-letter-complete-function' instead and passes the prefix
ARG, if present."
  (interactive "P")
  (let ((func nil))
    (cond ((not (mh-in-header-p))
           (funcall mh-letter-complete-function arg))
          ((setq func (cdr (assoc (mh-letter-header-field-at-point)
                                  mh-letter-complete-function-alist)))
           (funcall func))
          (t (funcall mh-letter-complete-function arg)))))

(defun mh-letter-complete-or-space (arg)
  "Perform completion or insert space.
If `mh-compose-space-does-completion-flag' is nil (the default) a space is
inserted.

Otherwise, if point is in the message header and the preceding character is
not whitespace then do completion. Otherwise insert a space character.

ARG is the number of spaces inserted."
  (interactive "p")
  (let ((func nil)
        (end-of-prev (save-excursion
                       (goto-char (mh-beginning-of-word))
                       (mh-beginning-of-word -1))))
    (cond ((not mh-compose-space-does-completion-flag)
           (self-insert-command arg))
          ((not (mh-in-header-p)) (self-insert-command arg))
          ((> (point) end-of-prev) (self-insert-command arg))
          ((setq func (cdr (assoc (mh-letter-header-field-at-point)
                                  mh-letter-complete-function-alist)))
           (funcall func))
          (t (self-insert-command arg)))))

(defun mh-letter-confirm-address ()
  "Flash alias expansion if `mh-alias-flash-on-comma' is non-nil."
  (interactive)
  (cond ((not (mh-in-header-p)) (self-insert-command 1))
        ((eq (cdr (assoc (mh-letter-header-field-at-point)
                         mh-letter-complete-function-alist))
             'mh-alias-letter-expand-alias)
         (mh-alias-reload-maybe)
         (mh-alias-minibuffer-confirm-address))
        (t (self-insert-command 1))))

(defvar mh-letter-header-field-regexp "^\\([A-Za-z][A-Za-z0-9-]*\\):")

(defun mh-letter-header-field-at-point ()
  "Return the header field name at point.
A symbol is returned whose name is the string obtained by downcasing the field
name."
  (save-excursion
    (end-of-line)
    (and (re-search-backward mh-letter-header-field-regexp nil t)
         (intern (downcase (match-string 1))))))

;;;###mh-autoload
(defun mh-letter-next-header-field-or-indent (arg)
  "Move to next field or indent depending on point.
In the message header, go to the next field. Elsewhere call
`indent-relative' as usual with optional prefix ARG."
  (interactive "P")
  (let ((header-end (save-excursion
                      (goto-char (mh-mail-header-end))
                      (forward-line)
                      (point))))
    (if (> (point) header-end)
        (indent-relative arg)
      (mh-letter-next-header-field))))

(defun mh-letter-next-header-field ()
  "Cycle to the next header field.
If we are at the last header field go to the start of the message body."
  (let ((header-end (mh-mail-header-end)))
    (cond ((>= (point) header-end) (goto-char (point-min)))
          ((< (point) (progn
                        (beginning-of-line)
                        (re-search-forward mh-letter-header-field-regexp
                                           (line-end-position) t)
                        (point)))
           (beginning-of-line))
          (t (end-of-line)))
    (cond ((re-search-forward mh-letter-header-field-regexp header-end t)
           (if (mh-letter-skipped-header-field-p (match-string 1))
               (mh-letter-next-header-field)
             (mh-letter-skip-leading-whitespace-in-header-field)))
          (t (goto-char header-end)
             (forward-line)))))

;;;###mh-autoload
(defun mh-letter-previous-header-field ()
  "Cycle to the previous header field.
If we are at the first header field go to the start of the message body."
  (interactive)
  (let ((header-end (mh-mail-header-end)))
    (if (>= (point) header-end)
        (goto-char header-end)
      (mh-header-field-beginning))
    (cond ((re-search-backward mh-letter-header-field-regexp nil t)
           (if (mh-letter-skipped-header-field-p (match-string 1))
               (mh-letter-previous-header-field)
           (goto-char (match-end 0))
           (mh-letter-skip-leading-whitespace-in-header-field)))
          (t (goto-char header-end)
             (forward-line)))))

(defun mh-letter-skipped-header-field-p (field)
  "Check if FIELD is to be skipped."
  (let ((field (downcase field)))
    (loop for x in mh-compose-skipped-header-fields
          when (equal (downcase x) field) return t
          finally return nil)))

(defun mh-letter-skip-leading-whitespace-in-header-field ()
  "Skip leading whitespace in a header field.
If the header field doesn't have at least one space after the colon then a
space character is added."
  (let ((need-space t))
    (while (memq (char-after) '(?\t ?\ ))
      (forward-char)
      (setq need-space nil))
    (when need-space (insert " "))))

(defvar mh-hidden-header-keymap
  (let ((map (make-sparse-keymap)))
    (mh-do-in-gnu-emacs
      (define-key map [mouse-2] 'mh-letter-toggle-header-field-display-button))
    (mh-do-in-xemacs
      (define-key map '(button2)
        'mh-letter-toggle-header-field-display-button))
    map))

(defun mh-letter-toggle-header-field-display-button (event)
  "Toggle header field display at location of EVENT.
This function does the same thing as `mh-letter-toggle-header-field-display'
except that it is callable from a mouse button."
  (interactive "e")
  (mh-do-at-event-location event
    (mh-letter-toggle-header-field-display nil)))

(defun mh-letter-toggle-header-field-display (arg)
  "Toggle display of header field at point.
If the header is long or spread over multiple lines then hiding it will show
the first few characters and replace the rest with an ellipsis.

If ARG is negative then header is hidden, if positive it is displayed. If ARG
is the symbol `long' then keep at most the first 4 lines."
  (interactive (list nil))
  (when (and (mh-in-header-p)
             (progn
               (end-of-line)
               (re-search-backward mh-letter-header-field-regexp nil t)))
    (let ((buffer-read-only nil)
          (modified-flag (buffer-modified-p))
          (begin (point))
          end)
      (end-of-line)
      (setq end (1- (if (re-search-forward "^[^ \t]" nil t)
                        (match-beginning 0)
                      (point-max))))
      (goto-char begin)
      ;; Make it clickable...
      (add-text-properties begin end `(keymap ,mh-hidden-header-keymap
                                       mouse-face highlight))
      (unwind-protect
          (cond ((or (and (not arg)
                          (text-property-any begin end 'invisible 'vanish))
                     (and (numberp arg) (>= arg 0))
                     (and (eq arg 'long) (> (line-beginning-position 5) end)))
                 (remove-text-properties begin end '(invisible nil))
                 (search-forward ":" (line-end-position) t)
                 (mh-letter-skip-leading-whitespace-in-header-field))
                ((eq arg 'long)
                 (end-of-line 4)
                 (mh-letter-truncate-header-field end)
                 (beginning-of-line))
                (t (end-of-line)
                   (mh-letter-truncate-header-field end)
                   (beginning-of-line)))
        (set-buffer-modified-p modified-flag)))))

(defun mh-letter-truncate-header-field (end)
  "Replace text from current line till END with an ellipsis.
If the current line is too long truncate a part of it as well."
  (let ((max-len (min (window-width) 62)))
    (when (> (+ (current-column) 4) max-len)
      (backward-char (- (+ (current-column) 5) max-len)))
    (when (> end (point))
      (add-text-properties (point) end '(invisible vanish)))))

(defun mh-letter-hide-all-skipped-fields ()
  "Hide all skipped fields."
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (narrow-to-region (point) (mh-mail-header-end))
      (while (re-search-forward mh-letter-header-field-regexp nil t)
        (if (mh-letter-skipped-header-field-p (match-string 1))
            (mh-letter-toggle-header-field-display -1)
          (mh-letter-toggle-header-field-display 'long))
        (beginning-of-line 2)))))

(defun mh-interactive-read-address (prompt)
  "Read an address.
If `mh-compose-prompt-flag' is non-nil, then read an address with PROMPT.
Otherwise return the empty string."
  (if mh-compose-prompt-flag (mh-read-address prompt) ""))

(defun mh-interactive-read-string (prompt)
  "Read a string.
If `mh-compose-prompt-flag' is non-nil, then read a string with PROMPT.
Otherwise return the empty string."
  (if mh-compose-prompt-flag (read-string prompt) ""))

(defun mh-letter-adjust-point ()
  "Move cursor to first header field if are using the no prompt mode."
  (unless mh-compose-prompt-flag
    (goto-char (point-max))
    (mh-letter-next-header-field)))

;;; Build the letter-mode keymap:
;;; If this changes, modify mh-letter-mode-help-messages accordingly, above.
(gnus-define-keys  mh-letter-mode-map
  " "                   mh-letter-complete-or-space
  ","                   mh-letter-confirm-address
  "\C-c?"               mh-help
  "\C-c\C-\\"           mh-fully-kill-draft ;if no C-q
  "\C-c\C-^"            mh-insert-signature ;if no C-s
  "\C-c\C-c"            mh-send-letter
  "\C-c\C-d"            mh-insert-identity
  "\C-c\C-e"            mh-edit-mhn
  "\C-c\C-f\C-b"        mh-to-field
  "\C-c\C-f\C-c"        mh-to-field
  "\C-c\C-f\C-d"        mh-to-field
  "\C-c\C-f\C-f"        mh-to-fcc
  "\C-c\C-f\C-r"        mh-to-field
  "\C-c\C-f\C-s"        mh-to-field
  "\C-c\C-f\C-t"        mh-to-field
  "\C-c\C-fb"           mh-to-field
  "\C-c\C-fc"           mh-to-field
  "\C-c\C-fd"           mh-to-field
  "\C-c\C-ff"           mh-to-fcc
  "\C-c\C-fr"           mh-to-field
  "\C-c\C-fs"           mh-to-field
  "\C-c\C-ft"           mh-to-field
  "\C-c\C-i"            mh-insert-letter
  "\C-c\C-m\C-e"        mh-mml-secure-message-encrypt
  "\C-c\C-m\C-f"        mh-compose-forward
  "\C-c\C-m\C-g"        mh-mhn-compose-anon-ftp
  "\C-c\C-m\C-i"        mh-compose-insertion
  "\C-c\C-m\C-m"        mh-mml-to-mime
  "\C-c\C-m\C-n"        mh-mml-unsecure-message
  "\C-c\C-m\C-s"        mh-mml-secure-message-sign
  "\C-c\C-m\C-t"        mh-mhn-compose-external-compressed-tar
  "\C-c\C-m\C-u"        mh-revert-mhn-edit
  "\C-c\C-m\C-x"        mh-mhn-compose-external-type
  "\C-c\C-mee"          mh-mml-secure-message-encrypt
  "\C-c\C-mes"          mh-mml-secure-message-signencrypt
  "\C-c\C-mf"           mh-compose-forward
  "\C-c\C-mg"           mh-mhn-compose-anon-ftp
  "\C-c\C-mi"           mh-compose-insertion
  "\C-c\C-mm"           mh-mml-to-mime
  "\C-c\C-mn"           mh-mml-unsecure-message
  "\C-c\C-mse"          mh-mml-secure-message-signencrypt
  "\C-c\C-mss"          mh-mml-secure-message-sign
  "\C-c\C-mt"           mh-mhn-compose-external-compressed-tar
  "\C-c\C-mu"           mh-revert-mhn-edit
  "\C-c\C-mx"           mh-mhn-compose-external-type
  "\C-c\C-o"            mh-open-line
  "\C-c\C-q"            mh-fully-kill-draft
  "\C-c\C-s"            mh-insert-signature
  "\C-c\C-t"            mh-letter-toggle-header-field-display
  "\C-c\C-w"            mh-check-whom
  "\C-c\C-y"            mh-yank-cur-msg
  "\C-c\M-d"            mh-insert-auto-fields
  "\M-\t"               mh-letter-complete
  "\t"                  mh-letter-next-header-field-or-indent
  [backtab]             mh-letter-previous-header-field)

;; "C-c /" prefix is used in mh-letter-mode by pgp.el and mailcrypt.el.

(provide 'mh-comp)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 62865511-e610-4923-b0b5-f45a8ab70a34
;;; mh-comp.el ends here
