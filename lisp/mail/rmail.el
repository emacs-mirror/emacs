;;; rmail.el --- main code of "RMAIL" mail reader for Emacs

;; Copyright (C) 1985, 1986, 1987, 1988, 1993, 1994, 1995, 1996, 1997, 1998,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;;; Code:

;; Souped up by shane@mit-ajax based on ideas of rlk@athena.mit.edu
;;   New features include attribute and keyword support, message
;;   selection by dispatch table, summary by attributes and keywords,
;;   expunging by dispatch table, sticky options for file commands.

;; Extended by Bob Weiner of Motorola
;;   New features include: rmail and rmail-summary buffers remain
;;   synchronized and key bindings basically operate the same way in both
;;   buffers, summary by topic or by regular expression, rmail-reply-prefix
;;   variable, and a bury rmail buffer (wipe) command.
;;

(eval-when-compile
  (require 'font-lock)
  (require 'mailabbrev)
  (require 'mule-util))                ; for detect-coding-with-priority

(require 'rmaildesc)
(require 'rmailhdr)
(require 'rmailkwd)
(require 'mail-parse)

(defvar deleted-head)
(defvar font-lock-fontified)
(defvar mail-abbrev-syntax-table)
(defvar mail-abbrevs)
(defvar messages-head)
(defvar rmail-use-spam-filter)
(defvar rsf-beep)
(defvar rsf-sleep-after-message)
(defvar total-messages)

; These variables now declared in paths.el.
;(defvar rmail-spool-directory "/usr/spool/mail/"
;  "This is the name of the directory used by the system mailer for\n\
;delivering new mail.  Its name should end with a slash.")
;(defvar rmail-file-name
;  (expand-file-name "~/RMAIL")
;  "")

(defgroup rmail nil
  "Mail reader for Emacs."
  :group 'mail)

(defgroup rmail-retrieve nil
  "Rmail retrieval options."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-files nil
  "Rmail files."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-headers nil
  "Rmail header options."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-reply nil
  "Rmail reply options."
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-summary nil
  "Rmail summary options."
  :prefix "rmail-"
  :prefix "rmail-summary-"
  :group 'rmail)

(defgroup rmail-output nil
  "Output message to a file."
  :prefix "rmail-output-"
  :prefix "rmail-"
  :group 'rmail)

(defgroup rmail-edit nil
  "Rmail editing."
  :prefix "rmail-edit-"
  :group 'rmail)

(defgroup rmail-obsolete nil
  "Rmail obsolete customization variables."
  :group 'rmail)

(defcustom rmail-movemail-program nil
  "If non-nil, the file name of the `movemail' program."
  :group 'rmail-retrieve
  :type '(choice (const nil) string))

(defcustom rmail-pop-password nil
  "*Password to use when reading mail from POP server.
Please use `rmail-remote-password' instead."
  :type '(choice (string :tag "Password")
		 (const :tag "Not Required" nil))
  :group 'rmail-obsolete)

(defcustom rmail-pop-password-required nil
  "*Non-nil if a password is required when reading mail from a POP server.
Please use rmail-remote-password-required instead."
  :type 'boolean
  :group 'rmail-obsolete)

(defcustom rmail-remote-password nil
  "*Password to use when reading mail from a remote server.
This setting is ignored for mailboxes whose URL already contains a password."
  :type '(choice (string :tag "Password")
		 (const :tag "Not Required" nil))
  :set-after '(rmail-pop-password)
  :set #'(lambda (symbol value)
	   (set-default symbol
			(if (and (not value)
                                 (boundp 'rmail-pop-password)
				 rmail-pop-password)
			    rmail-pop-password
			  value))
	   (setq rmail-pop-password nil))
  :group 'rmail-retrieve
  :version "22.1")

(defcustom rmail-remote-password-required nil
  "*Non-nil if a password is required when reading mail from a remote server."
  :type 'boolean
  :set-after '(rmail-pop-password-required)
  :set #'(lambda (symbol value)
	   (set-default symbol
			(if (and (not value)
                                 (boundp 'rmail-pop-password-required)
				 rmail-pop-password-required)
			    rmail-pop-password-required
			  value))
	   (setq rmail-pop-password-required nil))
  :group 'rmail-retrieve
  :version "22.1")

(defcustom rmail-movemail-flags nil
  "*List of flags to pass to movemail.
Most commonly used to specify `-g' to enable GSS-API authentication
or `-k' to enable Kerberos authentication."
  :type '(repeat string)
  :group 'rmail-retrieve
  :version "20.3")

(defvar rmail-remote-password-error "invalid usercode or password\\|
unknown user name or bad password\\|Authentication failed\\|MU_ERR_AUTH_FAILURE"
  "Regular expression matching incorrect-password POP or IMAP server error
messages.
If you get an incorrect-password error that this expression does not match,
please report it with \\[report-emacs-bug].")

(defvar rmail-encoded-remote-password nil)

(defcustom rmail-preserve-inbox nil
  "*Non-nil means leave incoming mail in the user's inbox--don't delete it."
  :type 'boolean
  :group 'rmail-retrieve)

(defcustom rmail-movemail-search-path nil
    "*List of directories to search for movemail (in addition to `exec-path')."
    :group 'rmail-retrieve
    :type '(repeat (directory)))

(defun rmail-probe (prog)
  "Determine what flavor of movemail PROG is.
We do this by executing it with `--version' and analyzing its output."
  (with-temp-buffer
    (let ((tbuf (current-buffer)))
      (buffer-disable-undo tbuf)
      (call-process prog nil tbuf nil "--version")
      (if (not (buffer-modified-p tbuf))
	  ;; Should not happen...
	  nil
	(goto-char (point-min))
	(cond
	 ((looking-at ".*movemail: invalid option")
	  'emacs)    ;; Possibly...
	 ((looking-at "movemail (GNU Mailutils .*)")
	  'mailutils)
	 (t
	  ;; FIXME:
	  'emacs))))))

(defun rmail-autodetect ()
  "Determine and return the file name of the `movemail' program.
If `rmail-movemail-program' is non-nil, use it.
Otherwise, look for `movemail' in the directories in
`rmail-movemail-search-path', those in `exec-path', and `exec-directory'."
  (if rmail-movemail-program
      (rmail-probe rmail-movemail-program)
    (catch 'scan
      (dolist (dir (append rmail-movemail-search-path exec-path
			   (list exec-directory)))
	(when (and dir (file-accessible-directory-p dir))
	  (let ((progname (expand-file-name "movemail" dir)))
	    (when (and (not (file-directory-p progname))
		       (file-executable-p progname))
	      (let ((x (rmail-probe progname)))
		(when x
		  (setq rmail-movemail-program progname)
		  (throw 'scan x))))))))))

(defvar rmail-movemail-variant-in-use nil
  "The movemail variant currently in use. Known variants are:

  `emacs'     Means any implementation, compatible with the native Emacs one.
              This is the default;
  `mailutils' Means GNU mailutils implementation, capable of handling full
mail URLs as the source mailbox;")

;;;###autoload
(defun rmail-movemail-variant-p (&rest variants)
  "Return t if the current movemail variant is any of VARIANTS.
Currently known variants are 'emacs and 'mailutils."
  (when (not rmail-movemail-variant-in-use)
    ;; Autodetect
    (setq rmail-movemail-variant-in-use (rmail-autodetect)))
  (not (null (member rmail-movemail-variant-in-use variants))))

;;;###autoload
(defcustom rmail-dont-reply-to-names nil "\
*A regexp specifying addresses to prune from a reply message.
A value of nil means exclude your own email address as an address
plus whatever is specified by `rmail-default-dont-reply-to-names'."
  :type '(choice regexp (const :tag "Your Name" nil))
  :group 'rmail-reply)

;;;###autoload
(defvar rmail-default-dont-reply-to-names "\\`info-" "\
A regular expression specifying part of the default value of the
variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's email address and name.)
It is useful to set this variable in the site customization file.")

;;;###autoload
(defcustom rmail-ignored-headers
  (concat "^via:\\|^mail-from:\\|^origin:\\|^references:\\|^sender:"
	  "\\|^status:\\|^received:\\|^content-transfer-encoding:"
	  "\\|^x400-\\(received\\|mts-identifier\\|content-type\\|originator\\|recipients\\):"
	  "\\|^list-\\(help\\|post\\|subscribe\\|id\\|unsubscribe\\|archive\\):"
	  "\\|^resent-\\(face\\|x-.*\\|organization\\|openpgp\\|date\\|message-id\\):"
	  "\\|^thread-\\(topic\\|index\\)"
	  "\\|^summary-line:\\|^precedence:\\|^message-id:"
	  "\\|^path:\\|^face:\\|^delivered-to:\\|^lines:"
	  "\\|^return-path:\\|^errors-to:\\|^return-receipt-to:"
	  "\\|^content-\\(length\\|type\\|class\\|disposition\\):"
	  "\\|^nntp-posting-\\(host\\|date\\):\\|^user-agent"
	  "\\|^importance:\\|^envelope-to:\\|^delivery-date\\|^openpgp:"
	  "\\|^mbox-line:\\|^cancel-lock:\\|^in-reply-to:\\|^comment:"
	  "\\|^x-.*:\\|^domainkey-signature:\\|^mime-version:"
	  "\\|^original-recipient:\\|^from ")
  "*Regexp to match header fields that Rmail should normally hide.
\(See also `rmail-nonignored-headers', which overrides this regexp.)
This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Rmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[rmail-toggle-header] twice."
  :type 'regexp
  :group 'rmail-headers)

(defcustom rmail-nonignored-headers "^x-spam-status:"
  "*Regexp to match X header fields that Rmail should show.
This regexp overrides `rmail-ignored-headers'; if both this regexp
and that one match a certain header field, Rmail shows the field.

This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Rmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[rmail-toggle-header] twice."
  :type 'regexp
  :group 'rmail-headers)

;;;###autoload
(defcustom rmail-displayed-headers nil
  "*Regexp to match Header fields that Rmail should display.
If nil, display all header fields except those matched by
`rmail-ignored-headers'."
  :type '(choice regexp (const :tag "All"))
  :group 'rmail-headers)

;;;###autoload
(defcustom rmail-retry-ignored-headers "^x-authentication-warning:" "\
*Headers that should be stripped when retrying a failed message."
  :type '(choice regexp (const nil :tag "None"))
  :group 'rmail-headers)

;;;###autoload
(defcustom rmail-highlighted-headers "^From:\\|^Subject:" "\
*Regexp to match Header fields that Rmail should normally highlight.
A value of nil means don't highlight.
See also `rmail-highlight-face'."
  :type 'regexp
  :group 'rmail-headers)

(defface rmail-highlight
  '((t :default highlight))
  "Face to use for highlighting the most important header fields."
  :group 'rmail-headers
  :version "22.1")

;;;###autoload
(defcustom rmail-highlight-face 'rmail-highlight "\
*Face used by Rmail for highlighting sender and subject.
See `rmail-font-lock-keywords'."
  :type '(choice (const :tag "Default" nil)
		 face)
  :group 'rmail-headers)

;;;###autoload
(defcustom rmail-delete-after-output nil "\
*Non-nil means automatically delete a message that is copied to a file."
  :type 'boolean
  :group 'rmail-files)

;;;###autoload
(defcustom rmail-primary-inbox-list nil "\
*List of files which are inboxes for user's primary mail file `~/RMAIL'.
nil means the default, which is (\"/usr/spool/mail/$USER\")
\(the name varies depending on the operating system,
and the value of the environment variable MAIL overrides it)."
  ;; Don't use backquote here, because we don't want to need it
  ;; at load time.
  :type (list 'choice '(const :tag "Default" nil)
	      (list 'repeat ':value (list (or (getenv "MAIL")
					      (concat "/var/spool/mail/"
						      (getenv "USER"))))
		    'file))
  :group 'rmail-retrieve
  :group 'rmail-files)

;;;###autoload
(defcustom rmail-inbox-alist nil
  "*Alist of mail files and backup directory names.
Each element has the form (MAIL-FILE INBOX ...).  When running
rmail on MAIL-FILE, mails in all the INBOX files listed will be
moved to the MAIL-FILE.  Be sure to fully qualify your MAIL-FILE.

Example setting if procmail delivers all your spam to
~/Mail/SPAM.in and you read it from the file ~/Mail/SPAM:

\(setq rmail-inbox-alist '((\"~/Mail/SPAM\" \"~/Mail/SPAM.in\")))"
  :type '(alist :key-type file :value-type (repeat file))
  :group 'rmail-retrieve
  :group 'rmail-files
  :version "22.1")

;;;###autoload
(defcustom rmail-mail-new-frame nil
  "*Non-nil means Rmail makes a new frame for composing outgoing mail.
This is handy if you want to preserve the window configuration of
the frame where you have the RMAIL buffer displayed."
  :type 'boolean
  :group 'rmail-reply)

;;;###autoload
(defcustom rmail-secondary-file-directory "~/"
  "*Directory for additional secondary Rmail files."
  :type 'directory
  :group 'rmail-files)
;;;###autoload
(defcustom rmail-secondary-file-regexp "\\.xmail$"
  "*Regexp for which files are secondary Rmail files."
  :type 'regexp
  :group 'rmail-files)

;;;###autoload
(defcustom rmail-confirm-expunge 'y-or-n-p
  "*Whether and how to ask for confirmation before expunging deleted messages."
  :type '(choice (const :tag "No confirmation" nil)
		 (const :tag "Confirm with y-or-n-p" y-or-n-p)
		 (const :tag "Confirm with yes-or-no-p" yes-or-no-p))
  :version "21.1"
  :group 'rmail-files)

;;;###autoload
(defvar rmail-mode-hook nil
  "List of functions to call when Rmail is invoked.")

;;;###autoload
(defvar rmail-get-new-mail-hook nil
  "List of functions to call when Rmail has retrieved new mail.")

;;;###autoload
(defcustom rmail-show-message-hook nil
  "List of functions to call when Rmail displays a message."
  :type 'hook
  :options '(goto-address)
  :group 'rmail)

;;;###autoload
(defvar rmail-quit-hook nil
  "List of functions to call when quitting out of Rmail.")

;;;###autoload
(defvar rmail-delete-message-hook nil
  "List of functions to call when Rmail deletes a message.
When the hooks are called, the message has been marked deleted but is
still the current message in the Rmail buffer.")

;; These may be altered by site-init.el to match the format of mmdf files
;;  delimiting used on a given host (delim1 and delim2 from the config
;;  files).

(defvar rmail-mmdf-delim1 "^\001\001\001\001\n"
  "Regexp marking the start of an mmdf message.")
(defvar rmail-mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message.")

(defcustom rmail-message-filter nil
  "If non-nil, a filter function for new messages in RMAIL.
Called with region narrowed to the message, including headers,
before obeying `rmail-ignored-headers'."
  :group 'rmail-headers
  :type 'function)

(defcustom rmail-automatic-folder-directives nil
  "List of directives specifying where to put a message.
Each element of the list is of the form:

  (FOLDERNAME FIELD REGEXP [ FIELD REGEXP ] ... )

Where FOLDERNAME is the name of a BABYL Version 6 (also known as mbox
or Unix inbox format) folder to put the message.  If any of the field
regexp's are nil, then it is ignored.

If FOLDERNAME is \"/dev/null\", it is deleted.
If FOLDERNAME is nil then it is deleted, and skipped.

FIELD is the plain text name of a field in the message, such as
\"subject\" or \"from\".  A FIELD of \"to\" will automatically include
all text from the \"cc\" field as well.

REGEXP is an expression to match in the preceeding specified FIELD.
FIELD/REGEXP pairs continue in the list.

examples:
  (\"/dev/null\" \"from\" \"@spam.com\") ; delete all mail from spam.com
  (\"RMS\" \"from\" \"rms@\") ; save all mail from RMS."
  :group 'rmail
  :version "21.1"
  :type '(repeat (sexp :tag "Directive")))

(defvar rmail-reply-prefix "Re: "
  "String to prepend to Subject line when replying to a message.")

;; Some mailers use "Re(2):" or "Re^2:" or "Re: Re:" or "Re[2]:".
;; This pattern should catch all the common variants.  The pattern
;; also ignores mailing list identifiers sometimes added in square
;; brackets at the beginning of subject lines.
(defvar rmail-reply-regexp "\\`\\(\\[.+?\\] \\)?\\(Re\\(([0-9]+)\\|\\[[0-9]+\\]\\|\\^[0-9]+\\)?: *\\)*"
  "Regexp to delete from Subject line before inserting `rmail-reply-prefix'.")

(defcustom rmail-display-summary nil
  "*If non-nil, Rmail always displays the summary buffer."
  :group 'rmail-summary
  :type 'boolean)

(defvar rmail-inbox-list nil)
(put 'rmail-inbox-list 'permanent-local t)

(defvar rmail-keywords nil)
(put 'rmail-keywords 'permanent-local t)

(defvar rmail-buffer nil
  "The RMAIL buffer related to the current buffer.
In an RMAIL buffer, this holds the RMAIL buffer itself.
In a summary buffer, this holds the RMAIL buffer it is a summary for.")
(put 'rmail-buffer 'permanent-local t)

;; Message counters and markers.  Deleted flags.

(defvar rmail-current-message nil)
(put 'rmail-current-message 'permanent-local t)

(defvar rmail-total-messages nil)
(put 'rmail-total-messages 'permanent-local t)

(defvar rmail-overlay-list nil)
(put 'rmail-overlay-list 'permanent-local t)

;; These are used by autoloaded rmail-summary.

(defvar rmail-summary-buffer nil)
(put 'rmail-summary-buffer 'permanent-local t)

(defvar rmail-view-buffer nil
  "Buffer which holds RMAIL message for MIME displaying.")
(put 'rmail-view-buffer 'permanent-local t)

;; `Sticky' default variables.

;; Last individual label specified to a or k.
(defvar rmail-last-label nil)
(put 'rmail-last-label 'permanent-local t)

;; Last set of values specified to C-M-n, C-M-p, C-M-s or C-M-l.
(defvar rmail-last-multi-labels nil)

(defvar rmail-last-regexp nil)
(put 'rmail-last-regexp 'permanent-local t)

(defcustom rmail-default-file "~/xmail"
  "*Default file name for \\[rmail-output]."
  :type 'file
  :group 'rmail-files)

(defcustom rmail-default-rmail-file "~/XMAIL"
  "*Default file name for \\[rmail-output-to-rmail-file]."
  :type 'file
  :group 'rmail-files)

(defcustom rmail-default-body-file "~/mailout"
  "*Default file name for \\[rmail-output-body-to-file]."
  :type 'file
  :group 'rmail-files
  :version "20.3")

;; Mule and MIME related variables.

;;;###autoload
(defvar rmail-file-coding-system nil
  "Coding system used in RMAIL file.

This is set to nil by default.")

;;;###autoload
(defcustom rmail-enable-mime nil
  "*If non-nil, RMAIL uses MIME feature.
If the value is t, RMAIL automatically shows MIME decoded message.
If the value is neither t nor nil, RMAIL does not show MIME decoded message
until a user explicitly requires it."
  :type '(choice (const :tag "on" t)
		 (const :tag "off" nil)
		 (other :tag "when asked" ask))
  :group 'rmail)

(defvar rmail-enable-mime-composing nil
  "*If non-nil, RMAIL uses `rmail-insert-mime-forwarded-message-function' to forward.")

;;;###autoload
(defvar rmail-show-mime-function nil
  "Function to show MIME decoded message of RMAIL file.
This function is called when `rmail-enable-mime' is non-nil.
It is called with no argument.")

;;;###autoload
(defvar rmail-insert-mime-forwarded-message-function nil
  "Function to insert a message in MIME format so it can be forwarded.
This function is called if `rmail-enable-mime' or
`rmail-enable-mime-composing' is non-nil.
It is called with one argument FORWARD-BUFFER, which is a
buffer containing the message to forward.  The current buffer
is the outgoing mail buffer.")

;;;###autoload
(defvar rmail-insert-mime-resent-message-function nil
  "Function to insert a message in MIME format so it can be resent.
This function is called if `rmail-enable-mime' is non-nil.
It is called with one argument FORWARD-BUFFER, which is a
buffer containing the message to forward.  The current buffer
is the outgoing mail buffer.")

;;;###autoload
(defvar rmail-search-mime-message-function nil
  "Function to check if a regexp matches a MIME message.
This function is called if `rmail-enable-mime' is non-nil.
It is called with two arguments MSG and REGEXP, where
MSG is the message number, REGEXP is the regular expression.")

;;;###autoload
(defvar rmail-search-mime-header-function nil
  "Function to check if a regexp matches a header of MIME message.
This function is called if `rmail-enable-mime' is non-nil.
It is called with three arguments MSG, REGEXP, and LIMIT, where
MSG is the message number,
REGEXP is the regular expression,
LIMIT is the position specifying the end of header.")

;;;###autoload
(defvar rmail-mime-feature 'rmail-mime
  "Feature to require to load MIME support in Rmail.
When starting Rmail, if `rmail-enable-mime' is non-nil,
this feature is required with `require'.")

;;;###autoload
(defvar rmail-decode-mime-charset t
  "*Non-nil means a message is decoded by MIME's charset specification.
If this variable is nil, or the message has not MIME specification,
the message is decoded as normal way.

If the variable `rmail-enable-mime' is non-nil, this variables is
ignored, and all the decoding work is done by a feature specified by
the variable `rmail-mime-feature'.")

;;;###autoload
(defvar rmail-mime-charset-pattern
  "^content-type:[ ]*text/plain;[ \t\n]*charset=\"?\\([^ \t\n\";]+\\)\"?"
  "Regexp to match MIME-charset specification in a header of message.
The first parenthesized expression should match the MIME-charset name.")


;;; Regexp matching the delimiter of messages in UNIX mail format
;;; (UNIX From lines), with an initial ^.  Used in rmail-decode-from-line,
;;; which knows the exact ordering of the \\(...\\) subexpressions.
(defvar rmail-unix-mail-delimiter
  (let ((time-zone-regexp
	 (concat "\\([A-Z]?[A-Z]?[A-Z][A-Z]\\( DST\\)?"
		 "\\|[-+]?[0-9][0-9][0-9][0-9]"
		 "\\|"
		 "\\) *")))
    (concat
     "^From "

     ;; Many things can happen to an RFC 822 mailbox before it is put into
     ;; a `From' line.  The leading phrase can be stripped, e.g.
     ;; `Joe <@w.x:joe@y.z>' -> `<@w.x:joe@y.z>'.  The <> can be stripped, e.g.
     ;; `<@x.y:joe@y.z>' -> `@x.y:joe@y.z'.  Everything starting with a CRLF
     ;; can be removed, e.g.
     ;;		From: joe@y.z (Joe	K
     ;;			User)
     ;; can yield `From joe@y.z (Joe 	K Fri Mar 22 08:11:15 1996', and
     ;;		From: Joe User
     ;;			<joe@y.z>
     ;; can yield `From Joe User Fri Mar 22 08:11:15 1996'.
     ;; The mailbox can be removed or be replaced by white space, e.g.
     ;;		From: "Joe User"{space}{tab}
     ;;			<joe@y.z>
     ;; can yield `From {space}{tab} Fri Mar 22 08:11:15 1996',
     ;; where {space} and {tab} represent the Ascii space and tab characters.
     ;; We want to match the results of any of these manglings.
     ;; The following regexp rejects names whose first characters are
     ;; obviously bogus, but after that anything goes.
     "\\([^\0-\b\n-\r\^?].*\\)? "

     ;; The time the message was sent.
     "\\([^\0-\r \^?]+\\) +"				; day of the week
     "\\([^\0-\r \^?]+\\) +"				; month
     "\\([0-3]?[0-9]\\) +"				; day of month
     "\\([0-2][0-9]:[0-5][0-9]\\(:[0-6][0-9]\\)?\\) *"	; time of day

     ;; Perhaps a time zone, specified by an abbreviation, or by a
     ;; numeric offset.
     time-zone-regexp

     ;; The year.
     " \\([0-9][0-9]+\\) *"

     ;; On some systems the time zone can appear after the year, too.
     time-zone-regexp

     ;; Old uucp cruft.
     "\\(remote from .*\\)?"

     "\n"))
  nil)

(defvar rmail-font-lock-keywords
  ;; These are all matched case-insensitively.
  (eval-when-compile
    (let* ((cite-chars "[>|}]")
	   (cite-prefix "a-z")
	   (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
      (list '("^\\(Sender\\|Resent-From\\):"
	      . font-lock-function-name-face)
	    '("^Reply-To:.*$" . font-lock-function-name-face)
	    '("^\\(From:\\)\\(.*\\(\n[ \t]+.*\\)*\\)"
	      (1 font-lock-function-name-face)
	      (2 rmail-highlight-face))
	    '("^\\(Subject:\\)\\(.*\\(\n[ \t]+.*\\)*\\)"
	      (1 font-lock-comment-face)
	      (2 rmail-highlight-face))
	    '("^X-Spam-Status:" . font-lock-keyword-face)
	    '("^\\(To\\|Apparently-To\\|Cc\\|Newsgroups\\):"
	      . font-lock-keyword-face)
	    ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
	    `(,cite-chars
	      (,(concat "\\=[ \t]*"
			"\\(\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
			"\\(" cite-chars "[ \t]*\\)\\)+\\)"
			"\\(.*\\)")
	       (beginning-of-line) (end-of-line)
	       (1 font-lock-comment-delimiter-face nil t)
	       (5 font-lock-comment-face nil t)))
	    '("^\\(X-[a-z0-9-]+\\|In-reply-to\\|Date\\):.*\\(\n[ \t]+.*\\)*$"
	      . font-lock-string-face))))
  "Additional expressions to highlight in Rmail mode.")

;; Perform BODY in the summary buffer
;; in such a way that its cursor is properly updated in its own window.
(defmacro rmail-select-summary (&rest body)
  `(let ((total rmail-total-messages))
     (if (rmail-summary-displayed)
	 (let ((window (selected-window)))
	   (save-excursion
	     (unwind-protect
		 (progn
		   (pop-to-buffer rmail-summary-buffer)
		   ;; rmail-total-messages is a buffer-local var
		   ;; in the rmail buffer.
		   ;; This way we make it available for the body
		   ;; even tho the rmail buffer is not current.
		   (let ((rmail-total-messages total))
		     ,@body))
	       (select-window window))))
       (save-excursion
	 (set-buffer rmail-summary-buffer)
	 (let ((rmail-total-messages total))
	   ,@body)))
     (rmail-maybe-display-summary)))

;;;; *** Rmail Mode ***

;; This variable is dynamically bound.  The defvar is here to placate
;; the byte compiler.

(defvar rmail-enable-multibyte nil)

;; Avoid errors.
(defvar rmail-use-spam-filter nil)

(defun rmail-require-mime-maybe ()
  "Require `rmail-mime-feature' if that is non-nil.
Signal an error and set `rmail-mime-feature' to nil if the feature
isn't provided."
  (when (and rmail-enable-mime
	     (not (require rmail-mime-feature nil t)))
    (message "Feature `%s' not provided" rmail-mime-feature)
    (sit-for 1)
    (setq rmail-enable-mime nil)))

;;;###autoload
(defun rmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by `rmail-file-name' (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with file name as argument; then performs rmail editing on
that file, but does not copy any new mail into the file.
Interactively, if you supply a prefix argument, then you
have a chance to specify a file name with the minibuffer.

If `rmail-display-summary' is non-nil, make a summary for this RMAIL file."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail on RMAIL file: "))))
  (rmail-require-mime-maybe)
  (let* ((file-name (expand-file-name (or file-name-arg rmail-file-name)))
	 ;; Use find-buffer-visiting, not get-file-buffer, for those users
	 ;; who have find-file-visit-truename set to t.
	 (existed (find-buffer-visiting file-name))
	 ;; This binding is necessary because we must decide if we
	 ;; need code conversion while the buffer is unibyte
	 ;; (i.e. enable-multibyte-characters is nil).
         (rmail-enable-multibyte
          (if existed
	      (with-current-buffer existed enable-multibyte-characters)
            (default-value 'enable-multibyte-characters)))
	 ;; Since the file may contain messages of different encodings
	 ;; at the tail (non-BYBYL part), we can't decode them at once
	 ;; on reading.  So, at first, we read the file without text
	 ;; code conversion, then decode the messages one by one.
	 (coding-system-for-read (and rmail-enable-multibyte 'raw-text))
	 run-mail-hook msg-shown)
    (when (and existed (eq major-mode 'rmail-edit-mode))
      (error "Exit Rmail Edit mode before getting new mail"))
    (if (and existed (not (verify-visited-file-modtime existed)))
	(progn
	  (find-file file-name)
	  (when (and (verify-visited-file-modtime existed)
		     (eq major-mode 'rmail-mode))
	    (setq major-mode 'fundamental-mode)))
	(switch-to-buffer
	 (let ((enable-local-variables nil))
	   (find-file-noselect file-name)))
	;; As we have read a file as raw-text, the buffer is set to
	;; unibyte.  We must make it multibyte if necessary.
	(when (and rmail-enable-multibyte
		   (not enable-multibyte-characters))
	  (set-buffer-multibyte t)))
    ;; Make sure we're in rmail-mode, even if the buffer did exist and
    ;; the file was not changed.
    (unless (eq major-mode 'rmail-mode)
      ;; If file looks like a Babyl file, save it to a temp file,
      ;; convert it, and replace the current content with the
      ;; converted content.  Don't save -- let the user do it.
      (goto-char (point-min))
      (when (looking-at "BABYL OPTIONS:")
	(let ((old-file (make-temp-file "rmail"))
	      (new-file (make-temp-file "rmail")))
	  (unwind-protect
	      (progn
		(write-region (point-min) (point-max) old-file)
		(unrmail old-file new-file)
		(message "Replacing BABYL format with mbox format...")
		(let ((inhibit-read-only t))
		  (erase-buffer)
		  (insert-file-contents-literally new-file))
		(message "Replacing BABYL format with mbox format...done"))
	    (delete-file old-file)
	    (delete-file new-file))))
      (goto-char (point-max))
      (rmail-mode-2)
      ;; Convert all or parts of file to a format Rmail understands
      (rmail-convert-file)
      ;;  We use `run-mail-hook' to remember whether we should run
      ;; `rmail-mode-hook' at the end.
      (setq run-mail-hook t)
      ;; Initialize the Rmail state.
      (rmail-initialize-messages))
    ;; Now we're back in business.  The happens even if we had a
    ;; perfectly fine file.
    (unwind-protect
	(unless (and (not file-name-arg) (rmail-get-new-mail))
	  (rmail-show-message (rmail-first-unseen-message)))
      (when rmail-display-summary
	(rmail-summary))
      (rmail-construct-io-menu)
      ;; Run any callbacks if the buffer was not in rmail-mode
      (when run-mail-hook
	(run-hooks 'rmail-mode-hook)))))

(defun rmail-convert-file ()
  "Convert unconverted messages.
A message is unconverted if it doesn't have the BABYL header
specified in `rmail-header-attribute-header'; it is converted
using `rmail-convert-mbox-format'."
  (let ((convert
	 (save-restriction
	   (widen)
	   (let ((case-fold-search nil)
		 (start (point-max))
		 end)
	     (catch 'convert
	       (goto-char start)
	       (while (re-search-backward
		       rmail-unix-mail-delimiter nil t)
		 (setq end start)
		 (setq start (point))
		 (save-excursion
		   (save-restriction
		     (narrow-to-region start end)
		     (goto-char start)
		     (let ((attribute (rmail-header-get-header
				       rmail-header-attribute-header)))
		       (unless attribute
			 (throw 'convert t)))))))))))
    (if convert
	(let ((inhibit-read-only t))
	  (rmail-convert-mbox-format)))))

(defun rmail-initialize-messages ()
  "Initialize message state based on messages in the buffer."
  (setq rmail-total-messages 0
        rmail-current-message 1)
  (rmail-desc-clear-descriptors)
  (widen)
  (rmail-header-show-headers)
  (setq rmail-total-messages (rmail-process-new-messages)))

(defvar rmail-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "a"      'rmail-add-label)
    (define-key map "b"      'rmail-bury)
    (define-key map "c"      'rmail-continue)
    (define-key map "d"      'rmail-delete-forward)
    (define-key map "\C-d"   'rmail-delete-backward)
    (define-key map "e"      'rmail-edit-current-message)
    (define-key map "f"      'rmail-forward)
    (define-key map "g"      'rmail-get-new-mail)
    (define-key map "h"      'rmail-summary)
    (define-key map "i"      'rmail-input)
    (define-key map "j"      'rmail-show-message)
    (define-key map "k"      'rmail-kill-label)
    (define-key map "l"      'rmail-summary-by-labels)
    (define-key map "\e\C-h" 'rmail-summary)
    (define-key map "\e\C-l" 'rmail-summary-by-labels)
    (define-key map "\e\C-r" 'rmail-summary-by-recipients)
    (define-key map "\e\C-s" 'rmail-summary-by-regexp)
    (define-key map "\e\C-t" 'rmail-summary-by-topic)
    (define-key map "m"      'rmail-mail)
    (define-key map "\em"    'rmail-retry-failure)
    (define-key map "n"      'rmail-next-undeleted-message)
    (define-key map "\en"    'rmail-next-message)
    (define-key map "\e\C-n" 'rmail-next-labeled-message)
    (define-key map "o"      'rmail-output)
    (define-key map "\C-o"   'rmail-output)
    (define-key map "p"      'rmail-previous-undeleted-message)
    (define-key map "\ep"    'rmail-previous-message)
    (define-key map "\e\C-p" 'rmail-previous-labeled-message)
    (define-key map "q"      'rmail-quit)
    (define-key map "r"      'rmail-reply)
    ;; I find I can't live without the default M-r command -- rms.
    ;;  (define-key map "\er"  'rmail-search-backwards)
    (define-key map "s"      'rmail-expunge-and-save)
    (define-key map "\es"    'rmail-search)
    (define-key map "t"      'rmail-toggle-header)
    (define-key map "u"      'rmail-undelete-previous-message)
    (define-key map "w"      'rmail-output-body-to-file)
    (define-key map "x"      'rmail-expunge)
    (define-key map "."      'rmail-beginning-of-message)
    (define-key map "/"      'rmail-end-of-message)
    (define-key map "<"      'rmail-first-message)
    (define-key map ">"      'rmail-last-message)
    (define-key map " "      'scroll-up)
    (define-key map "\177"   'scroll-down)
    (define-key map "?"      'describe-mode)
    (define-key map "\C-c\C-s\C-d" 'rmail-sort-by-date)
    (define-key map "\C-c\C-s\C-s" 'rmail-sort-by-subject)
    (define-key map "\C-c\C-s\C-a" 'rmail-sort-by-author)
    (define-key map "\C-c\C-s\C-r" 'rmail-sort-by-recipient)
    (define-key map "\C-c\C-s\C-c" 'rmail-sort-by-correspondent)
    (define-key map "\C-c\C-s\C-l" 'rmail-sort-by-lines)
    (define-key map "\C-c\C-s\C-k" 'rmail-sort-by-labels)
    (define-key map "\C-c\C-n" 'rmail-next-same-subject)
    (define-key map "\C-c\C-p" 'rmail-previous-same-subject)
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar classify]
      (cons "Classify" (make-sparse-keymap "Classify")))
    (define-key map [menu-bar classify input-menu]
      nil)
    (define-key map [menu-bar classify output-menu]
      nil)
    (define-key map [menu-bar classify output-body]
      '("Output body to file..." . rmail-output-body-to-file))
    (define-key map [menu-bar classify output-inbox]
      '("Output (inbox)..." . rmail-output))
    (define-key map [menu-bar classify output]
      '("Output (Rmail)..." . rmail-output))
    (define-key map [menu-bar classify kill-label]
      '("Kill Label..." . rmail-kill-label))
    (define-key map [menu-bar classify add-label]
      '("Add Label..." . rmail-add-label))
    (define-key map [menu-bar summary]
      (cons "Summary" (make-sparse-keymap "Summary")))
    (define-key map [menu-bar summary senders]
      '("By Senders..." . rmail-summary-by-senders))
    (define-key map [menu-bar summary labels]
      '("By Labels..." . rmail-summary-by-labels))
    (define-key map [menu-bar summary recipients]
      '("By Recipients..." . rmail-summary-by-recipients))
    (define-key map [menu-bar summary topic]
      '("By Topic..." . rmail-summary-by-topic))
    (define-key map [menu-bar summary regexp]
      '("By Regexp..." . rmail-summary-by-regexp))
    (define-key map [menu-bar summary all]
      '("All" . rmail-summary))
    (define-key map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))
    (define-key map [menu-bar mail rmail-get-new-mail]
      '("Get New Mail" . rmail-get-new-mail))
    (define-key map [menu-bar mail lambda]
      '("----"))
    (define-key map [menu-bar mail continue]
      '("Continue" . rmail-continue))
    (define-key map [menu-bar mail resend]
      '("Re-send..." . rmail-resend))
    (define-key map [menu-bar mail forward]
      '("Forward" . rmail-forward))
    (define-key map [menu-bar mail retry]
      '("Retry" . rmail-retry-failure))
    (define-key map [menu-bar mail reply]
      '("Reply" . rmail-reply))
    (define-key map [menu-bar mail mail]
      '("Mail" . rmail-mail))
    (define-key map [menu-bar delete]
      (cons "Delete" (make-sparse-keymap "Delete")))
    (define-key map [menu-bar delete expunge/save]
      '("Expunge/Save" . rmail-expunge-and-save))
    (define-key map [menu-bar delete expunge]
      '("Expunge" . rmail-expunge))
    (define-key map [menu-bar delete undelete]
      '("Undelete" . rmail-undelete-previous-message))
    (define-key map [menu-bar delete delete]
      '("Delete" . rmail-delete-forward))
    (define-key map [menu-bar move]
      (cons "Move" (make-sparse-keymap "Move")))
    (define-key map [menu-bar move search-back]
      '("Search Back..." . rmail-search-backwards))
    (define-key map [menu-bar move search]
      '("Search..." . rmail-search))
    (define-key map [menu-bar move previous]
      '("Previous Nondeleted" . rmail-previous-undeleted-message))
    (define-key map [menu-bar move next]
      '("Next Nondeleted" . rmail-next-undeleted-message))
    (define-key map [menu-bar move last]
      '("Last" . rmail-last-message))
    (define-key map [menu-bar move first]
      '("First" . rmail-first-message))
    (define-key map [menu-bar move previous]
      '("Previous" . rmail-previous-message))
    (define-key map [menu-bar move next]
      '("Next" . rmail-next-message))
    map)
  "Keymap for `rmail-mode'.")

;; Rmail mode is suitable only for specially formatted data.
(put 'rmail-mode 'mode-class 'special)

(defun rmail-mode-kill-summary ()
  (if rmail-summary-buffer (kill-buffer rmail-summary-buffer)))

;;;###autoload
(defun rmail-mode ()
  "Rmail Mode is used by \\<rmail-mode-map>\\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

\\[rmail-beginning-of-message]	Move point to front of this message.
\\[rmail-end-of-message]	Move point to bottom of this message.
\\[scroll-up]	Scroll to next screen of this message.
\\[scroll-down]	Scroll to previous screen of this message.
\\[rmail-next-undeleted-message]	Move to Next non-deleted message.
\\[rmail-previous-undeleted-message]	Move to Previous non-deleted message.
\\[rmail-next-message]	Move to Next message whether deleted or not.
\\[rmail-previous-message]	Move to Previous message whether deleted or not.
\\[rmail-first-message]	Move to the first message in Rmail file.
\\[rmail-last-message]	Move to the last message in Rmail file.
\\[rmail-show-message]	Jump to message specified by numeric position in file.
\\[rmail-search]	Search for string and show message it is found in.
\\[rmail-delete-forward]	Delete this message, move to next nondeleted.
\\[rmail-delete-backward]	Delete this message, move to previous nondeleted.
\\[rmail-undelete-previous-message]	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
\\[rmail-edit-current-message]	Edit the current message.  \\[rmail-cease-edit] to return to Rmail.
\\[rmail-expunge]	Expunge deleted messages.
\\[rmail-expunge-and-save]	Expunge and save the file.
\\[rmail-quit]       Quit Rmail: expunge, save, then switch to another buffer.
\\[save-buffer] Save without expunging.
\\[rmail-get-new-mail]	Move new mail from system spool directory into this file.
\\[rmail-mail]	Mail a message (same as \\[mail-other-window]).
\\[rmail-continue]	Continue composing outgoing message started before.
\\[rmail-reply]	Reply to this message.  Like \\[rmail-mail] but initializes some fields.
\\[rmail-retry-failure]	Send this message again.  Used on a mailer failure message.
\\[rmail-forward]	Forward this message to another user.
\\[rmail-output-to-rmail-file]       Output this message to an Rmail file (append it).
\\[rmail-output]	Output this message to a Unix-format mail file (append it).
\\[rmail-output-body-to-file]	Save message body to a file.  Default filename comes from Subject line.
\\[rmail-input]	Input Rmail file.  Run Rmail on that file.
\\[rmail-add-label]	Add label to message.  It will be displayed in the mode line.
\\[rmail-kill-label]	Kill label.  Remove a label from current message.
\\[rmail-next-labeled-message]   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with \\[rmail-add-label].
\\[rmail-previous-labeled-message]   Move to Previous message with specified label
\\[rmail-summary]	Show headers buffer, with a one line summary of each message.
\\[rmail-summary-by-labels]	Summarize only messages with particular label(s).
\\[rmail-summary-by-recipients]   Summarize only messages with particular recipient(s).
\\[rmail-summary-by-regexp]   Summarize only messages with particular regexp(s).
\\[rmail-summary-by-topic]   Summarize only messages with subject line regexp(s).
\\[rmail-toggle-header]	Toggle display of complete header."
  (interactive)
  (let ((finding-rmail-file (not (eq major-mode 'rmail-mode))))
    (rmail-mode-2)
    (when (and finding-rmail-file
	       (null coding-system-for-read)
	       default-enable-multibyte-characters)
      (let ((rmail-enable-multibyte t))
	(rmail-require-mime-maybe)
	(goto-char (point-max))
	(set-buffer-multibyte t)))
    (rmail-show-message rmail-total-messages)
    (when finding-rmail-file
      (when rmail-display-summary
	(rmail-summary))
      (rmail-construct-io-menu))
    (run-mode-hooks 'rmail-mode-hook)))

(defun rmail-mode-2 ()
  (kill-all-local-variables)
  (rmail-mode-1)
  (rmail-perm-variables)
  (rmail-variables))

(defun rmail-mode-1 ()
  (setq major-mode 'rmail-mode)
  (setq mode-name "RMAIL")
  (setq buffer-read-only t)
  ;; No need to auto save RMAIL files in normal circumstances
  ;; because they contain no info except attribute changes
  ;; and deletion of messages.
  ;; The one exception is when messages are copied into an Rmail mode buffer.
  ;; rmail-output-to-rmail-file enables auto save when you do that.
  (setq buffer-auto-save-file-name nil)
  (setq mode-line-modified "--")
  (use-local-map rmail-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table))

;; Set up the permanent locals associated with an Rmail file.
(defun rmail-perm-variables ()
  (make-local-variable 'rmail-last-label)
  (make-local-variable 'rmail-last-regexp)
  (make-local-variable 'rmail-buffer)
  (setq rmail-buffer (current-buffer))
  (make-local-variable 'rmail-view-buffer)
  (setq rmail-view-buffer rmail-buffer)
  (make-local-variable 'rmail-summary-buffer)
  (make-local-variable 'rmail-current-message)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'rmail-overlay-list)
  (setq rmail-overlay-list nil)
  (make-local-variable 'rmail-inbox-list)
  (setq rmail-inbox-list (rmail-get-file-inbox-list))
  ;; Provide default set of inboxes for primary mail file ~/RMAIL.
  (and (null rmail-inbox-list)
       (or (equal buffer-file-name (expand-file-name rmail-file-name))
	   (equal buffer-file-truename
		  (abbreviate-file-name (file-truename rmail-file-name))))
       (setq rmail-inbox-list
	     (or rmail-primary-inbox-list
		 (list (or (getenv "MAIL")
			   (concat rmail-spool-directory
				   (user-login-name)))))))
  (make-local-variable 'rmail-keywords)
  ;; this gets generated as needed
  (setq rmail-keywords nil))

;; Set up the non-permanent locals associated with Rmail mode.
(defun rmail-variables ()
  (make-local-variable 'save-buffer-coding-system)
  ;; If we don't already have a value for save-buffer-coding-system,
  ;; get it from buffer-file-coding-system, and clear that
  ;; because it should be determined in rmail-show-message.
  (unless save-buffer-coding-system
    (setq save-buffer-coding-system (or buffer-file-coding-system 'undecided))
    (setq buffer-file-coding-system nil))
  ;; Don't let a local variables list in a message cause confusion.
  (make-local-variable 'local-enable-local-variables)
  (setq local-enable-local-variables nil)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'rmail-revert)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(rmail-font-lock-keywords
	  t t nil nil
	  (font-lock-maximum-size . nil)
	  (font-lock-fontify-buffer-function . rmail-fontify-buffer-function)
	  (font-lock-unfontify-buffer-function . rmail-unfontify-buffer-function)
	  (font-lock-inhibit-thing-lock . (lazy-lock-mode fast-lock-mode))))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'rmail-mode-kill-summary)
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag t)
  (make-local-variable 'desktop-save-buffer)
  (setq desktop-save-buffer t))

;; Handle M-x revert-buffer done in an rmail-mode buffer.
(defun rmail-revert (arg noconfirm)
  (with-current-buffer rmail-buffer
    (let* ((revert-buffer-function (default-value 'revert-buffer-function))
	   (rmail-enable-multibyte enable-multibyte-characters)
	   ;; See similar code in `rmail'.
	   (coding-system-for-read (and rmail-enable-multibyte 'raw-text)))
      ;; Call our caller again, but this time it does the default thing.
      (when (revert-buffer arg noconfirm)
	;; If the user said "yes", and we changed something, reparse the
	;; messages.
	(with-current-buffer rmail-buffer
	  (rmail-mode-2)
	  (rmail-convert-file)
	  ;; We have read the file as raw-text, so the buffer is set to
	  ;; unibyte.  Make it multibyte if necessary.
	  (when (and rmail-enable-multibyte
		     (not enable-multibyte-characters))
	    (set-buffer-multibyte t))
	  (rmail-initialize-messages)
	  (rmail-show-message rmail-total-messages)
	  (run-hooks 'rmail-mode-hook))))))

(defun rmail-get-file-inbox-list ()
  "Return a list of inbox files for this buffer."
  (let* ((filename (expand-file-name (buffer-file-name)))
	 (inboxes (cdr (or (assoc filename rmail-inbox-alist)
			   (assoc (abbreviate-file-name filename)
				  rmail-inbox-alist))))
	 (list nil))
    (dolist (i inboxes)
      (when (file-name-absolute-p i)
	(push (expand-file-name i) list)))
    (nreverse list)))

;;; mbox: ready
(defun rmail-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (rmail-expunge)
  (save-buffer)
  (rmail-display-summary-maybe))

;;; mbox: ready
(defun rmail-display-summary-maybe ()
  "If a summary buffer exists then make sure it is updated and displayed."
  (if (rmail-summary-exists)
      (let ((current-message rmail-current-message))
        (rmail-select-summary
         (rmail-summary-goto-msg current-message)
         (rmail-summary-rmail-update)
         (set-buffer-modified-p nil)))))

;;; mbox: ready
(defun rmail-quit ()
  "Quit out of RMAIL.
Hook `rmail-quit-hook' is run after expunging."
  (interactive)
  (rmail-expunge-and-save)
  (when (boundp 'rmail-quit-hook)
    (run-hooks 'rmail-quit-hook))
  ;; Don't switch to the summary buffer even if it was recently visible.
  (when rmail-summary-buffer
    (replace-buffer-in-windows rmail-summary-buffer)
    (bury-buffer rmail-summary-buffer))
  (if rmail-enable-mime
      (let ((obuf rmail-buffer)
	    (ovbuf rmail-view-buffer))
	(set-buffer rmail-view-buffer)
	(quit-window)
	(replace-buffer-in-windows ovbuf)
	(replace-buffer-in-windows obuf)
	(bury-buffer obuf))
    (let ((obuf (current-buffer)))
      (quit-window)
      (replace-buffer-in-windows obuf))))

;;; mbox: ready
(defun rmail-bury ()
  "Bury current Rmail buffer and its summary buffer."
  (interactive)
  ;; This let var was called rmail-buffer, but that interfered
  ;; with the buffer-local var used in summary buffers.
  (let ((buffer-to-bury (current-buffer)))
    (if (rmail-summary-exists)
	(let (window)
	  (while (setq window (get-buffer-window rmail-summary-buffer))
	    (quit-window nil window))
	  (bury-buffer rmail-summary-buffer)))
    (quit-window)))

;;;??? Fails to add descriptor for new message.
;;; mbox: ready
(defun rmail-duplicate-message ()
  "Create a duplicated copy of the current message.
The duplicate copy goes into the Rmail file just after the
original copy."
  (interactive)
  (widen)
  (let ((buffer-read-only nil)
	(number rmail-current-message)
	(string (buffer-substring (rmail-desc-get-start rmail-current-message)
				  (rmail-desc-get-end rmail-current-message))))
    (goto-char (rmail-desc-get-end rmail-current-message))
    (insert string)
    (rmail-show-message number)
    (message "Message duplicated")))

;;;###autoload
(defun rmail-input (filename)
  "Run Rmail on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  (rmail filename))

;; This used to scan subdirectories recursively, but someone pointed out
;; that if the user wants that, person can put all the files in one dir.
;; And the recursive scan was slow.  So I took it out.  rms, Sep 1996.
(defun rmail-find-all-files (start)
  "Return list of file in dir START that match `rmail-secondary-file-regexp'."
  (if (file-accessible-directory-p start)
      ;; Don't sort here.
      (let* ((case-fold-search t)
	     (files (directory-files start t rmail-secondary-file-regexp)))
	;; Sort here instead of in directory-files
	;; because this list is usually much shorter.
	(sort files 'string<))))

(defun rmail-list-to-menu (menu-name l action &optional full-name)
  (let ((menu (make-sparse-keymap menu-name)))
    (mapcar
     (function (lambda (item)
		 (let (command)
		   (if (consp item)
		       (progn
			 (setq command
			       (rmail-list-to-menu (car item) (cdr item)
						   action
						   (if full-name
						       (concat full-name "/"
							       (car item))
						     (car item))))
			 (setq name (car item)))
		     (progn
		       (setq name item)
		       (setq command
			     (list 'lambda () '(interactive)
				   (list action
					 (expand-file-name
					  (if full-name
					      (concat full-name "/" item)
					    item)
					  rmail-secondary-file-directory))))))
		   (define-key menu (vector (intern name))
		     (cons name command)))))
     (reverse l))
    menu))

;; This command is always "disabled" when it appears in a menu.
(put 'rmail-disable-menu 'menu-enable ''nil)

(defun rmail-construct-io-menu ()
  (let ((files (rmail-find-all-files rmail-secondary-file-directory)))
    (if files
	(progn
	  (define-key rmail-mode-map [menu-bar classify input-menu]
	    (cons "Input Rmail File"
		  (rmail-list-to-menu "Input Rmail File"
				      files
				      'rmail-input)))
	  (define-key rmail-mode-map [menu-bar classify output-menu]
	    (cons "Output Rmail File"
		  (rmail-list-to-menu "Output Rmail File"
				      files
				      'rmail-output))))

      (define-key rmail-mode-map [menu-bar classify input-menu]
	'("Input Rmail File" . rmail-disable-menu))
      (define-key rmail-mode-map [menu-bar classify output-menu]
	'("Output Rmail File" . rmail-disable-menu)))))


;;;; *** Rmail input ***

(defun rmail-get-inbox-files ()
  "Return all files from `rmail-inbox-list' without name conflicts.
A conflict happens when two inbox file names have the same name
according to `file-name-nondirectory'."
  (let (files last-names)
    (catch 'conflict
      (dolist (file rmail-inbox-list)
	(if (member (file-name-nondirectory file) last-names)
	    (throw 'conflict t)
	  (push file files))
	(push (file-name-nondirectory file) last-names)))
    (nreverse files)))

(defun rmail-delete-inbox-files (files)
  "Delete all files given in FILES.
If delete fails, truncate them to zero length."
  (dolist (file files)
    (condition-case nil
	;; First, try deleting.
	(condition-case nil
	    (delete-file file)
	  ;; If we can't delete it, truncate it.
	  (file-error (write-region (point) (point) file)))
      (file-error nil))))

(defun rmail-get-new-mail (&optional file-name)
  "Move any new mail from this mail file's inbox files.
The inbox files for the primary mail file are determined using
various means when setting up the buffer.  The list of inbox
files are stored in `rmail-inbox-list'.

The most important variable that determines the value of this
list is `rmail-inbox-alist' which lists the inbox files for any
mail files you might be using.

If the above yields no inbox files, and if this is the primary
mail file as determined by `rmail-file-name', the inbox lists
otherwise defaults to `rmail-primary-inbox-list' if set, or the
environment variable MAIL if set, or the user's mail file in
`rmail-spool-directory'.

This is why, by default, no mail file has inbox files, except for
the primary mail file ~/RMAIL, which gets its new mail from the
mail spool.

You can also specify the file to get new mail from interactively.
A prefix argument will read a file name and use that file as the
inbox.  Noninteractively, you can pass the inbox file name as an
argument.

If the variable `rmail-preserve-inbox' is non-nil, new mail will
always be left in inbox files rather than deleted.

This function runs `rmail-get-new-mail-hook' before saving the
updated file.  It returns t if it got any new messages."
  (interactive
   (list (when current-prefix-arg
	   (read-file-name "Get new mail from file: "))))
  (run-hooks 'rmail-before-get-new-mail-hook)
  ;; If the disk file has been changed from under us, revert to it
  ;; before we get new mail.
  (unless (verify-visited-file-modtime (current-buffer))
    (find-file (buffer-file-name)))
  (with-current-buffer rmail-buffer
    (widen)
    ;; Get rid of all undo records for this buffer.
    (unless (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
    (let ((rmail-enable-multibyte (default-value 'enable-multibyte-characters))
	  ;; If buffer has not changed yet, and has not been saved yet,
	  ;; don't replace the old backup file now.
	  (make-backup-files (and make-backup-files (buffer-modified-p)))
	  current-message found)
      (condition-case nil
	  (let ((buffer-read-only nil)
		(buffer-undo-list t)
		(delete-files nil)
		(new-messages 0))
	    (save-excursion
	      (save-restriction
		(goto-char (point-max))
		(narrow-to-region (point) (point))
		;; Read in the contents of the inbox files, renaming
		;; them as necessary, and adding to the list of files to
		;; delete eventually.
		(if file-name
		    (rmail-insert-inbox-text (list file-name) nil)
		  (setq delete-files (rmail-insert-inbox-text
				      (rmail-get-inbox-files) t)))
		;; Process newly found messages and save them into the
		;; RMAIL file.
		(unless (equal (point-min) (point-max))
		  (setq new-messages (rmail-convert-mbox-format))
		  (unless (zerop new-messages)
		    (rmail-process-new-messages)
		    (setq rmail-current-message (1+ rmail-total-messages)
			  rmail-total-messages (rmail-desc-get-count)))
		  (save-buffer))
		;; Delete the old files, now that the RMAIL file is
		;; saved.
		(when delete-files
		  (rmail-delete-inbox-files delete-files))))

	    (if (zerop new-messages)
		(when (or file-name rmail-inbox-list)
		  (message "(No new mail has arrived)"))

	      ;; Process the new messages for spam using the integrated
	      ;; spam filter.  The spam filter can mark messages for
	      ;; deletion and can output a message.
	      (setq current-message (rmail-first-unseen-message))
	      (when rmail-use-spam-filter
		(while (<= current-message rmail-total-messages)
		  (rmail-spam-filter current-message)
		  (setq current-message (1+ current-message))))
	      ;; Make the first unseen message the current message and
	      ;; update the summary buffer, if one exists.
	      (setq current-message (rmail-first-unseen-message))
	      (if (rmail-summary-exists)
		  (with-current-buffer rmail-summary-buffer
		    (rmail-update-summary)
		    (rmail-summary-goto-msg current-message))
		(rmail-show-message current-message))
	      ;; Run the after get new mail hook.
	      (run-hooks 'rmail-after-get-new-mail-hook)
	      (message "%d new message%s read"
		       new-messages (if (= 1 new-messages) "" "s"))
	      (setq found t))
	    found)
	;; Don't leave the buffer screwed up if we get a disk-full error.
	(file-error (or found (rmail-show-message)))))))

(defun rmail-parse-url (file)
  "Parse the supplied URL. Return (list MAILBOX-NAME REMOTE PASSWORD GOT-PASSWORD)
WHERE MAILBOX-NAME is the name of the mailbox suitable as argument to the
actual version of `movemail', REMOTE is non-nil if MAILBOX-NAME refers to
a remote mailbox, PASSWORD is the password if it should be
supplied as a separate argument to `movemail' or nil otherwise, GOT-PASSWORD
is non-nil if the user has supplied the password interactively.
"
  (cond
   ((string-match "^\\([^:]+\\)://\\(\\([^:@]+\\)\\(:\\([^@]+\\)\\)?@\\)?.*" file)
      (let (got-password supplied-password
	    (proto (match-string 1 file))
	    (user  (match-string 3 file))
	    (pass  (match-string 5 file))
	    (host  (substring file (or (match-end 2)
				       (+ 3 (match-end 1))))))

	(if (not pass)
	    (when rmail-remote-password-required
	      (setq got-password (not (rmail-have-password)))
	      (setq supplied-password (rmail-get-remote-password
				       (string-equal proto "imap")))))

	(if (rmail-movemail-variant-p 'emacs)
	    (if (string-equal proto "pop")
		(list (concat "po:" user ":" host)
		      t
		      (or pass supplied-password)
		      got-password)
	      (error "Emacs movemail does not support %s protocol" proto))
	  (list file
		(or (string-equal proto "pop") (string-equal proto "imap"))
		supplied-password
		got-password))))

   ((string-match "^po:\\([^:]+\\)\\(:\\(.*\\)\\)?" file)
    (let (got-password supplied-password
          (proto "pop")
	  (user  (match-string 1 file))
	  (host  (match-string 3 file)))

      (when rmail-remote-password-required
	(setq got-password (not (rmail-have-password)))
	(setq supplied-password (rmail-get-remote-password nil)))

      (list file "pop" supplied-password got-password)))

   (t
    (list file nil nil nil))))

(defun rmail-insert-inbox-text (files renamep)
  ;; Detect a locked file now, so that we avoid moving mail
  ;; out of the real inbox file.  (That could scare people.)
  (or (memq (file-locked-p buffer-file-name) '(nil t))
      (error "RMAIL file %s is locked"
	     (file-name-nondirectory buffer-file-name)))
  (let (file tofile delete-files movemail popmail got-password password)
    (while files
      ;; Handle remote mailbox names specially; don't expand as filenames
      ;; in case the userid contains a directory separator.
      (setq file (car files))
      (let ((url-data (rmail-parse-url file)))
	(setq file (nth 0 url-data))
	(setq popmail (nth 1 url-data))
	(setq password (nth 2 url-data))
	(setq got-password (nth 3 url-data)))

      (if popmail
	  (setq renamep t)
	(setq file (file-truename
		    (substitute-in-file-name (expand-file-name file)))))
      (setq tofile (expand-file-name
		    ;; Generate name to move to from inbox name,
		    ;; in case of multiple inboxes that need moving.
		    (concat ".newmail-"
			    (file-name-nondirectory
			     (if (memq system-type '(windows-nt cygwin))
				 ;; cannot have "po:" in file name
				 (substring file 3)
			       file)))
		    ;; Use the directory of this rmail file
		    ;; because it's a nuisance to use the homedir
		    ;; if that is on a full disk and this rmail
		    ;; file isn't.
		    (file-name-directory
		     (expand-file-name buffer-file-name))))
      ;; Always use movemail to rename the file,
      ;; since there can be mailboxes in various directories.
      (if (not popmail)
	  (progn
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (expand-file-name (user-login-name)
					     file)))))
      (cond (popmail
	     (message "Getting mail from the remote server ..."))
	    ((and (file-exists-p tofile)
		  (/= 0 (nth 7 (file-attributes tofile))))
	     (message "Getting mail from %s..." tofile))
	    ((and (file-exists-p file)
		  (/= 0 (nth 7 (file-attributes file))))
	     (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (and (not popmail)
					     (not (file-exists-p file))))
	     nil)
	    (t
	     (with-temp-buffer
	       (let ((errors (current-buffer)))
		 (buffer-disable-undo errors)
		 (let ((args
			(append
			 (list (or rmail-movemail-program
				   (expand-file-name "movemail"
						     exec-directory))
			       nil errors nil)
			 (if rmail-preserve-inbox
			     (list "-p")
			   nil)
			 (if (rmail-movemail-variant-p 'mailutils)
			     (append (list "--emacs") rmail-movemail-flags)
			   rmail-movemail-flags)
			 (list file tofile)
			 (if password (list password) nil))))
		   (apply 'call-process args))
		 (if (not (buffer-modified-p errors))
		     ;; No output => movemail won
		     nil
		   (set-buffer errors)
		   (subst-char-in-region (point-min) (point-max)
					 ?\n ?\  )
		   (goto-char (point-max))
		   (skip-chars-backward " \t")
		   (delete-region (point) (point-max))
		   (goto-char (point-min))
		   (if (looking-at "movemail: ")
		       (delete-region (point-min) (match-end 0)))
		   (beep t)
		   ;; If we just read the password, most likely it is
		   ;; wrong.  Otherwise, see if there is a specific
		   ;; reason to think that the problem is a wrong passwd.
		   (if (or got-password
			   (re-search-forward rmail-remote-password-error
					      nil t))
		       (rmail-set-remote-password nil))

		   ;; If using Mailutils, remove initial error code
		   ;; abbreviation
		   (when (rmail-movemail-variant-p 'mailutils)
		     (goto-char (point-min))
		     (when (looking-at "[A-Z][A-Z0-9_]*:")
		       (delete-region (point-min) (match-end 0))))

		   (message "movemail: %s"
			    (buffer-substring (point-min)
					      (point-max)))

		   (sit-for 3)
		   nil)))))

      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (let ((coding-system-for-read 'no-conversion)
		size)
	    (goto-char (point-max))
	    (setq size (nth 1 (insert-file-contents tofile)))
	    (goto-char (point-max))
	    (or (= (preceding-char) ?\n)
		(zerop size)
		(insert ?\n))
	    (if (not (and rmail-preserve-inbox (string= file tofile)))
		(setq delete-files (cons tofile delete-files)))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;;;; *** Rmail message decoding ***

(defun rmail-decode-region (from to coding)
  "Decode the region specified by FROM and TO by CODING.
If CODING is nil or an invalid coding system, decode by `undecided'."
  (unless (and coding (coding-system-p coding))
    (setq coding 'undecided))
  ;; Use -dos decoding, to remove ^M characters left from base64 or
  ;; rogue qp-encoded text.
  (decode-coding-region from to
			(coding-system-change-eol-conversion
			 coding 'dos))
  ;; Don't reveal the fact we used -dos decoding, as users generally
  ;; will not expect the RMAIL buffer to use DOS EOL format.
  (setq buffer-file-coding-system
	(setq last-coding-system-used
	      (coding-system-change-eol-conversion
	       coding 'unix))))

(defun rmail-decode-by-content-type (from to)
  "Decode message between FROM and TO according to Content-Type."
  (when (and (not rmail-enable-mime) rmail-enable-multibyte)
    (let ((coding-system-used nil)
	  (case-fold-search t))
      (save-restriction
	(narrow-to-region from to)
	(when (and (not rmail-enable-mime) rmail-enable-multibyte)
	  (let ((coding
		 (when (save-excursion
			 (goto-char (rmail-header-get-limit))
			 (re-search-backward
			  rmail-mime-charset-pattern
			  (point-min) t))
		   (intern (downcase (match-string 1))))))
	    (setq coding-system-used (rmail-decode-region
				      (point-min) (point-max)
				      coding)))))
      (setq last-coding-system-used coding-system-used))))

;;;; *** Rmail Message Formatting and Header Manipulation ***

(defun rmail-clear-headers (&optional ignored-headers)
  "Delete all header fields that Rmail should not show.
If the optional argument IGNORED-HEADERS is non-nil,
delete all header fields whose names match that regexp.
Otherwise, if `rmail-displayed-headers' is non-nil,
delete all header fields *except* those whose names match that regexp.
Otherwise, delete all header fields whose names match `rmail-ignored-headers'
unless they also match `rmail-nonignored-headers'."
  (when (search-forward "\n\n" nil t)
    (forward-char -1)
    (let ((case-fold-search t)
	  (buffer-read-only nil))
      (if (and rmail-displayed-headers (null ignored-headers))
	  (save-restriction
	    (narrow-to-region (point-min) (point))
	    (let (lim next)
	      (goto-char (point-min))
	      (while (and (not (eobp))
			  (save-excursion
			    (if (re-search-forward "\n[^ \t]" nil t)
				(setq lim (match-beginning 0)
				      next (1+ lim))
			      (setq lim nil next (point-max)))))
		(if (save-excursion
		      (re-search-forward rmail-displayed-headers lim t))
		  (goto-char next)
		  (delete-region (point) next))))
	    (goto-char (point-min)))
	(or ignored-headers (setq ignored-headers rmail-ignored-headers))
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (goto-char (point-min))
	  (while (and ignored-headers
		      (re-search-forward ignored-headers nil t))
	    (beginning-of-line)
	    (if (looking-at rmail-nonignored-headers)
		(forward-line 1)
	      (delete-region (point)
			     (save-excursion
			       (if (re-search-forward "\n[^ \t]" nil t)
				   (1- (point))
				 (point-max)))))))))))

(defun rmail-msg-is-pruned (&optional msg)
  "Determine if the headers for the current message are being
  displayed. If MSG is non-nil it will be used as the message number
  instead of the current message."
  (rmail-desc-get-header-display-state (or msg rmail-current-message)))

(defun rmail-toggle-header (&optional arg)
  "Show original message header if pruned header currently shown, or vice versa.
With argument ARG, show the message header pruned if ARG is greater than zero;
otherwise, show it in full."
  (interactive "P")
  (rmail-header-toggle-visibility arg))

;; Lifted from repos-count-screen-lines.
(defun rmail-count-screen-lines (start end)
  "Return number of screen lines between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (vertical-motion (- (point-max) (point-min))))))

;;;; *** Rmail Attributes and Keywords ***

;; Make a string describing the current message's attributes by
;; keywords and set it up as the name of a minor mode so it will
;; appear in the mode line.
(defun rmail-display-labels ()
  (let (keyword-list result)
    ;; Update the keyword list for the current message.
    (if (> rmail-current-message 0)
        (setq keyword-list (rmail-desc-get-keywords rmail-current-message)))
    ;; Generate the result string.
    (setq result (mapconcat 'identity keyword-list " "))
    ;; Update the mode line to display the keywords, the current
    ;; message index and the total number of messages.
    (setq mode-line-process
	  (format " %d/%d%s"
		  rmail-current-message rmail-total-messages
		  (if keyword-list (concat " " result) "")))
    ;; If rmail-enable-mime is non-nil, we may have to update
    ;; `mode-line-process' of rmail-view-buffer too.
    (if (and rmail-enable-mime
	     (not (eq (current-buffer) rmail-view-buffer))
	     (buffer-live-p rmail-view-buffer))
	(let ((mlp mode-line-process))
	  (with-current-buffer rmail-view-buffer
	    (setq mode-line-process mlp))))))

(defun rmail-set-attribute (attr state &optional msgnum)
  "Turn a attribute ATTR of a message on or off according to STATE.
ATTR is a string, MSGNUM is the optional message number.  By
default, the current message is changed."
  (save-excursion
    (save-restriction
      (let ((attr-index (rmail-desc-get-attr-index attr)))
	(set-buffer rmail-buffer)
	(or msgnum (setq msgnum rmail-current-message))
	(rmail-desc-set-attribute attr-index state msgnum)
        ;; Deal with the summary buffer.
        (when rmail-summary-buffer
	  (rmail-summary-update msgnum))))))

(defun rmail-message-labels-p (n labels)
  "Return t if message number N has keywords matching LABELS.
LABELS is a regular expression."
  (catch 'found
    (dolist (keyword (rmail-desc-get-keywords n))
      (when (string-match labels keyword)
	(throw 'found t)))))


;;;; *** Rmail Message Selection And Support ***

(defun rmail-msgbeg (n)
  (rmail-desc-get-start n))
(make-obsolete 'rmail-msgbeg 'rmail-desc-get-start "22.0")

(defun rmail-msgend (n)
  (rmail-desc-get-end n))
(make-obsolete 'rmail-msgend 'rmail-desc-get-end "22.0")

(defun rmail-widen-to-current-msgbeg (function)
  "Call FUNCTION with point at start of internal data of current message.
Assumes that bounds were previously narrowed to display the message in Rmail.
The bounds are widened enough to move point where desired, then narrowed
again afterward.

FUNCTION may not change the visible text of the message, but it may
change the invisible header text."
  (save-excursion
    (unwind-protect
	(progn
	  (narrow-to-region (rmail-desc-get-start rmail-current-message)
			    (point-max))
	  (goto-char (point-min))
	  (funcall function))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
      (narrow-to-region (rmail-desc-get-start rmail-current-message)
			(rmail-desc-get-end rmail-current-message)))))

(defun rmail-process-new-messages (&optional nomsg)
  "Process the new messages in the buffer.
The buffer has been narrowed to expose only the new messages.
For each new message append an entry to the message vector and,
if necessary, add a header that will capture the salient BABYL
information.  Return the number of new messages.  If NOMSG is
non-nil then do not show any progress messages."
  (let ((inhibit-read-only t)
        (case-fold-search nil)
	(new-message-counter 0)
	(start (point-max))
	end date keywords message-descriptor-list)
    (or nomsg (message "Processing new messages..."))
    ;; Process each message in turn starting from the back and
    ;; proceeding to the front of the region.  This is especially a
    ;; good approach since the buffer will likely have new headers
    ;; added.
    (save-excursion
      (goto-char start)
      (while (re-search-backward rmail-unix-mail-delimiter nil t)
	;; Cache the message date to facilitate generating a message
	;; summary later.  The format is '(DAY-OF-WEEK DAY-NUMBER MON
	;; YEAR TIME)
	(setq date
	      (list (buffer-substring (match-beginning 2) (match-end 2))
		    (buffer-substring (match-beginning 4) (match-end 4))
		    (buffer-substring (match-beginning 3) (match-end 3))
		    (buffer-substring (match-beginning 7) (match-end 7))
		    (buffer-substring (match-beginning 5) (match-end 5))))
	;;Set start and end to bracket this message.
	(setq end start)
	(setq start (point))
	(save-excursion
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char start)
	    ;; Bump the new message counter.
	    (setq new-message-counter (1+ new-message-counter))

	    ;; Set up keywords, if any.  The keywords are provided via a
	    ;; comma separated list and returned as a list of strings.
	    (setq keywords (rmail-header-get-keywords))
	    (when keywords
	      ;; Keywords do exist.  Register them with the keyword
	      ;; management library.
	      (rmail-register-keywords keywords))
	    ;; Insure that we have From and Date headers.
	    ;;(rmail-decode-from-line)

	    ;; Perform User defined filtering.
	    (save-excursion
	      (if rmail-message-filter (funcall rmail-message-filter)))
	    ;; Accumulate the message attributes along with the message
	    ;; markers and the message date list.
	    (setq message-descriptor-list
		  (vconcat (list (list (point-min-marker)
				       (rmail-header-get-header
					rmail-header-attribute-header)
				       keywords
				       date
				       (count-lines start end)
				       (cadr (mail-extract-address-components; does not like nil
					      (or (rmail-header-get-header "from") "")))
				       (or (rmail-header-get-header "subject")
					   "none")))
			   message-descriptor-list)))))
      ;; Add the new message data lists to the Rmail message descriptor
      ;; vector.
      (rmail-desc-add-descriptors message-descriptor-list)
      ;; Unless requested otherwise, show the number of new messages.
      ;; Return the number of new messages.
      (or nomsg (message "Processing new messages...done (%d)"
			 new-message-counter))
      new-message-counter)))

(defun rmail-convert-mbox-format ()
  (let ((case-fold-search nil)
	(message-count 0)
	(start (point-max))
	end)
    (save-excursion
      (goto-char start)
      (while (re-search-backward rmail-unix-mail-delimiter nil t)
	(setq end start)
	(setq start (point))
	(save-excursion
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char (point-min))
	    ;; Bump the new message counter.
	    (setq message-count (1+ message-count))
	    ;; Detect messages that have been added with DOS line endings
	    ;; and convert the line endings for such messages.
	    (when (save-excursion (end-of-line) (= (preceding-char) ?\r))
	      (let ((buffer-read-only nil)
		    (buffer-undo t)
		    (end-marker (copy-marker end)))
		(message
		 "Processing new messages...(converting line endings)")
		(save-excursion
		  (goto-char (point-max))
		  (while (search-backward "\r\n" (point-min) t)
		    (delete-char 1)))
		(setq end (marker-position end-marker))
		(set-marker end-marker nil)))
	    ;; Make sure we have an Rmail BABYL attribute header field.
	    ;; All we can assume is that the Rmail BABYL header field is
	    ;; in the header section.  It's placement can be modified by
	    ;; another mailer.
	    (let ((attributes (rmail-header-get-header
			       rmail-header-attribute-header)))
	      (unless attributes
		;; No suitable header exists.  Append the default BABYL
		;; data header for a new message.
		(rmail-header-add-header rmail-header-attribute-header
					 rmail-desc-default-attrs))))))
      message-count)))

(defun rmail-beginning-of-message ()
  "Show current message starting from the beginning."
  (interactive)
  (let ((rmail-show-message-hook
	 (list (function (lambda ()
			   (goto-char (point-min)))))))
    (rmail-show-message rmail-current-message)))

(defun rmail-end-of-message ()
  "Show bottom of current message."
  (interactive)
  (let ((rmail-show-message-hook
	 (list (function (lambda ()
			   (goto-char (point-max))
			   (recenter (1- (window-height))))))))
    (rmail-show-message rmail-current-message)))

(defun rmail-unknown-mail-followup-to ()
  "Handle a \"Mail-Followup-To\" header field with an unknown mailing list.
Ask the user whether to add that list name to `mail-mailing-lists'."
  (save-restriction
    (let ((mail-followup-to (rmail-header-get-header "mail-followup-to" nil t)))
      (when mail-followup-to
	(let ((addresses
	       (split-string
		(mail-strip-quoted-names mail-followup-to)
		",[[:space:]]+" t)))
	  (dolist (addr addresses)
	    (when (and (not (member addr mail-mailing-lists))
		       (and rmail-user-mail-address-regexp
			    (not (string-match rmail-user-mail-address-regexp
					       addr)))
		       (y-or-n-p
			(format "Add `%s' to `mail-mailing-lists'? "
				addr)))
	      (customize-save-variable 'mail-mailing-lists
				       (cons addr mail-mailing-lists)))))))))

(defun rmail-show-message (&optional n no-summary)
  "Show message number N (prefix argument), counting from start of file.
If NO-SUMMARY is non-nil, then do not update the summary buffer."
  (interactive "p")
  (unless (eq major-mode 'rmail-mode)
    (switch-to-buffer rmail-buffer))
  (if (zerop rmail-total-messages)
      (progn
        (message "No messages to show.  Add something better soon.")
        (force-mode-line-update))
    (let (blurb)
      ;; Set n to the first sane message based on the sign of n:
      ;; positive but greater than the total number of messages -> n;
      ;; negative -> 1.
      (if (not n)
	  (setq n rmail-current-message)
	(cond ((<= n 0)
	       (setq n 1
		     rmail-current-message 1
		     blurb "No previous message"))
	      ((> n rmail-total-messages)
	       (setq n rmail-total-messages
		     rmail-current-message rmail-total-messages
		     blurb "No following message"))
	      (t
	       (setq rmail-current-message n))))
      (let ((beg (rmail-desc-get-start n))
	    (end (rmail-desc-get-end n)))
        (rmail-header-show-headers)
        (widen)
	(narrow-to-region beg end)
        (goto-char (point-min))
	(condition-case nil
	    (let* ((coding-system-name
		    (rmail-header-get-header "X-Coding-System"))
		   (coding-system (intern coding-system-name)))
	      (check-coding-system coding-system)
	      (setq buffer-file-coding-system coding-system))
	  ;; no coding system or invalid coding system
	  (error (setq buffer-file-coding-system nil)))
        ;; Clear the "unseen" attribute when we show a message, unless
	;; it is already cleared.
	(when (rmail-desc-attr-p rmail-desc-unseen-index n)
	  (rmail-desc-set-attribute rmail-desc-unseen-index nil n))
	(rmail-display-labels)
	;; Deal with MIME
	(if (eq rmail-enable-mime t)
	    (funcall rmail-show-mime-function)
	  (setq rmail-view-buffer rmail-buffer))
	(when mail-mailing-lists
	  (rmail-unknown-mail-followup-to))
	(rmail-header-hide-headers)
	(when transient-mark-mode (deactivate-mark))
        ;; Make sure that point in the Rmail window is at the beginning
        ;; of the buffer.
	(goto-char (point-min))
        (set-window-point (get-buffer-window rmail-buffer) (point))
	;; Run any User code.
	(run-hooks 'rmail-show-message-hook)
	;; If there is a summary buffer, try to move to this message in
	;; that buffer.  But don't complain if this message is not
	;; mentioned in the summary.  Don't do this at all if we were
	;; called on behalf of cursor motion in the summary buffer.
	(when (and (rmail-summary-exists) (not no-summary))
	    (let ((curr-msg rmail-current-message))
	      ;; Set the summary current message, disabling the Rmail
	      ;; buffer update.
	      (with-current-buffer rmail-summary-buffer
		(rmail-summary-goto-msg curr-msg nil t))))
	(with-current-buffer rmail-buffer
	  (rmail-auto-file))
        ;; Post back any status messages.
	(when blurb
	  (message blurb))))))

(defun rmail-redecode-body (coding)
  "Decode the body of the current message using coding system CODING.
This is useful with mail messages that have malformed or missing
charset= headers.

This function assumes that the current message is already decoded
and displayed in the RMAIL buffer, but the coding system used to
decode it was incorrect.  It then encodes the message back to its
original form, and decodes it again, using the coding system CODING.

Note that if Emacs erroneously auto-detected one of the iso-2022
encodings in the message, this function might fail because the escape
sequences that switch between character sets and also single-shift and
locking-shift codes are impossible to recover.  This function is meant
to be used to fix messages encoded with 8-bit encodings, such as
iso-8859, koi8-r, etc."
  (interactive "zCoding system for re-decoding this message: ")
  (unless rmail-enable-mime
    (with-current-buffer rmail-buffer
      (save-excursion
	(let ((start (rmail-desc-get-start rmail-current-message))
	      (end (rmail-desc-get-end rmail-current-message))
	      header)
	  (narrow-to-region start end)
	  (setq header (rmail-header-get-header "X-Coding-System"))
	  (if header
	      (let ((old-coding (intern header))
		    (buffer-read-only nil))
		(check-coding-system old-coding)
		;; Make sure the new coding system uses the same EOL
		;; conversion, to prevent ^M characters from popping
		;; up all over the place.
		(setq coding
		      (coding-system-change-eol-conversion
		       coding
		       (coding-system-eol-type old-coding)))
		    ;; Do the actual recoding.
		(encode-coding-region start end old-coding)
		(decode-coding-region start end coding)
		;; Rewrite the x-coding-system header according to
		;; what we did.
		(setq last-coding-system-used coding)
		(rmail-header-add-header
		 "X-Coding-System"
		 (symbol-name last-coding-system-used))
		(rmail-show-message rmail-current-message))
	    (error "No X-Coding-System header found")))))))

;; FIXME: Double-check this
(defun rmail-auto-file ()
  "Automatically move a message into a sub-folder based on criteria.
Called when a new message is displayed."
  (if (or (member "filed" (rmail-desc-get-keywords rmail-current-message))
	  (not (string= (buffer-file-name)
			(expand-file-name rmail-file-name))))
      ;; Do nothing if it's already been filed.
      nil
    ;; Find out some basics (common fields)
    (let ((from (mail-fetch-field "from"))
	  (subj (mail-fetch-field "subject"))
	  (to   (concat (mail-fetch-field "to") "," (mail-fetch-field "cc")))
	  (directives rmail-automatic-folder-directives)
	  (directive-loop nil)
	  (folder nil))
      (while directives
	(setq folder (car (car directives))
	      directive-loop (cdr (car directives)))
	(while (and (car directive-loop)
		    (let ((f (cond
			      ((string= (car directive-loop) "from") from)
			      ((string= (car directive-loop) "to") to)
			      ((string= (car directive-loop) "subject") subj)
			      (t (mail-fetch-field (car directive-loop))))))
		      (and f (string-match (car (cdr directive-loop)) f))))
	  (setq directive-loop (cdr (cdr directive-loop))))
	;; If there are no directives left, then it was a complete match.
	(if (null directive-loop)
	    (if (null folder)
		(rmail-delete-forward)
	      (if (string= "/dev/null" folder)
		  (rmail-delete-message)
		(rmail-output folder 1 t)
		(setq directives nil))))
	(setq directives (cdr directives))))))

(defun rmail-next-message (n)
  "Show following message whether deleted or not.
With prefix arg N, moves forward N messages, or backward if N is
negative."
  (interactive "p")
  (with-current-buffer rmail-buffer
    (rmail-show-message (+ rmail-current-message n))))

(defun rmail-previous-message (n)
  "Show previous message whether deleted or not.
With prefix arg N, moves backward N messages, or forward if N is
negative."
  (interactive "p")
  (rmail-next-message (- n)))

(defun rmail-next-undeleted-message (n)
  "Show following non-deleted message.
With prefix arg N, moves forward N non-deleted messages, or
backward if N is negative.

Returns t if a new message is being shown, nil otherwise."
  (interactive "p")
  (let ((lastwin rmail-current-message)
	(original rmail-current-message)
	(current rmail-current-message))
    ;; Move forwards, remember the last undeleted message seen.
    (while (and (> n 0) (< current rmail-total-messages))
      (setq current (1+ current))
      (unless (rmail-desc-deleted-p current)
	(setq lastwin current
	      n (1- n))))
    ;; Same thing for moving backwards
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (unless (rmail-desc-deleted-p current)
	(setq lastwin current
	      n (1+ n))))
    ;; Show the message (even if no movement took place so that the
    ;; delete attribute is marked) and determine the result value.
    (rmail-show-message lastwin)
    (if (/= lastwin original)
        t
      (if (< n 0)
	  (message "No previous nondeleted message"))
      (if (> n 0)
	  (message "No following nondeleted message"))
      nil)))

(defun rmail-previous-undeleted-message (n)
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  (interactive "p")
  (rmail-next-undeleted-message (- n)))

(defun rmail-first-message ()
  "Show first message in file."
  (interactive)
  (rmail-show-message 1))

(defun rmail-last-message ()
  "Show last message in file."
  (interactive)
  (rmail-show-message rmail-total-messages))

(defun rmail-narrow-to-header (msg)
  "Narrow the buffer to the headers of message number MSG."
  (save-excursion
    (let ((start (rmail-desc-get-start msg))
	  (end (rmail-desc-get-end msg)))
      (widen)
      (goto-char start)
      (unless (search-forward "\n\n" end t)
	(error "Invalid message format"))
      (narrow-to-region start (point)))))

(defun rmail-message-recipients-p (msg recipients &optional primary-only)
  (save-restriction
    (or (string-match recipients (or (mail-fetch-field "To") ""))
	(string-match recipients (or (mail-fetch-field "From") ""))
	(if (not primary-only)
	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(defun rmail-message-regexp-p (msg regexp)
  "Return t, if for message number MSG, regexp REGEXP matches in the header."
  (save-excursion
    (save-restriction
      (rmail-narrow-to-header msg)
      (re-search-forward regexp nil t))))

(defun rmail-search-message (msg regexp)
  "Return non-nil, if for message number MSG, regexp REGEXP matches."
  (goto-char (rmail-desc-get-start msg))
  (if rmail-enable-mime
      (funcall rmail-search-mime-message-function msg regexp)
    (re-search-forward regexp (rmail-desc-get-end msg) t)))

(defvar rmail-search-last-regexp nil)
(defun rmail-search (regexp &optional n)
  "Show message containing next match for REGEXP (but not the current msg).
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (< (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if rmail-search-last-regexp
			(concat ", default "
				rmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (or n (setq n 1))
  (message "%sRmail search for %s..."
	   (if (< n 0) "Reverse " "")
	   regexp)
  (set-buffer rmail-buffer)
  (let ((omin (point-min))
	(omax (point-max))
	(opoint (point))
	(reversep (< n 0))
	(msg rmail-current-message)
        win)
    (unwind-protect
	(progn
	  (widen)
	  (while (/= n 0)
	    ;; Check messages one by one, advancing message number up or down
	    ;; but searching forward through each message.
	    (if reversep
		(while (and (null win) (> msg 1))
		  (setq msg (1- msg)
			win (rmail-search-message msg regexp)))
	      (while (and (null win) (< msg rmail-total-messages))
		(setq msg (1+ msg)
		      win (rmail-search-message msg regexp))))
	    (setq n (+ n (if reversep 1 -1)))))
      (if win
	  (progn
	    (rmail-show-message msg)
	    ;; Search forward (if this is a normal search) or backward
	    ;; (if this is a reverse search) through this message to
	    ;; position point.  This search may fail because REGEXP
	    ;; was found in the hidden portion of this message.  In
	    ;; that case, move point to the beginning of visible
	    ;; portion.
	    (if reversep
		(progn
		  (goto-char (point-max))
		  (re-search-backward regexp nil 'move))
	      (goto-char (point-min))
	      (re-search-forward regexp nil t))
	    (message "%sRmail search for %s...done"
		     (if reversep "Reverse " "")
		     regexp))
	(goto-char opoint)
	(narrow-to-region omin omax)
	(ding)
	(message "Search failed: %s" regexp)))))

(defun rmail-search-backwards (regexp &optional n)
  "Show message containing previous match for REGEXP.
Prefix argument gives repeat count; negative argument means search
forward (through later messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (>= (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if rmail-search-last-regexp
			(concat ", default "
				rmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (rmail-search regexp (- (or n 1))))

(defun rmail-first-unseen-message ()
  "Return the first message which has not been seen.  If all messages
have been seen, then return the last message."
  (let ((current 1)
	found)
    (while (and (not found) (<= current rmail-total-messages))
      (if (rmail-desc-attr-p rmail-desc-unseen-index current)
	  (setq found current))
      (setq current (1+ current)))
    (or found rmail-total-messages)))

(defun rmail-current-subject ()
  "Return the current subject.
The subject is stripped of leading and trailing whitespace, and
of typical reply prefixes such as Re:."
  (let ((subject (or (mail-fetch-field "Subject") "")))
    (if (string-match "\\`[ \t]+" subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match rmail-reply-regexp subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match "[ \t]+\\'" subject)
	(setq subject (substring subject 0 (match-beginning 0))))
    subject))

(defun rmail-current-subject-regexp ()
  "Return a regular expression matching the current subject.
The regular expression matches the subject header line of
messages about the same subject.  The subject itself is stripped
of leading and trailing whitespace, of typical reply prefixes
such as Re: and whitespace within the subject is replaced by a
regular expression matching whitespace in general in order to
take into account that subject header lines may include newlines
and more whitespace.  The returned regular expressions contains
`rmail-reply-regexp' and ends with a newline."
  (let ((subject (rmail-current-subject)))
    ;; If Subject is long, mailers will break it into several lines at
    ;; arbitrary places, so replace whitespace with a regexp that will
    ;; match any sequence of spaces, TABs, and newlines.
    (setq subject (regexp-quote subject))
    (setq subject
	  (replace-regexp-in-string "[ \t\n]+" "[ \t\n]+" subject t t))
    (concat "^Subject: "
	    (if (string= "\\`" (substring rmail-reply-regexp 0 2))
		(substring rmail-reply-regexp 2)
	      rmail-reply-regexp)
	    subject "[ \t]*\n")))

(defun rmail-next-same-subject (n)
  "Go to the next mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go backwards instead."
  (interactive "p")
  (let ((search-regexp (rmail-current-subject-regexp))
	(forward (> n 0))
	(i rmail-current-message)
	(case-fold-search t)
	found)
    (save-excursion
      (save-restriction
	(widen)
	(if forward
	    (while (and (/= n 0) (< i rmail-total-messages))
	      (let (done)
		(while (and (not done)
			    (< i rmail-total-messages))
		  (setq i (+ i 1))
		  (rmail-narrow-to-header i)
		  (goto-char (point-min))
		  (setq done (re-search-forward search-regexp (point-max) t)))
		(if done (setq found i)))
	      (setq n (1- n)))
	  (while (and (/= n 0) (> i 1))
	    (let (done)
	      (while (and (not done) (> i 1))
		(setq i (- i 1))
		(rmail-narrow-to-header i)
		(goto-char (point-min))
		(setq done (re-search-forward search-regexp (point-max) t)))
	      (if done (setq found i)))
	    (setq n (1+ n))))))
    (if found
	(rmail-show-message found)
      (error "No %s message with same subject"
	     (if forward "following" "previous")))))

(defun rmail-previous-same-subject (n)
  "Go to the previous mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go forwards instead."
  (interactive "p")
  (rmail-next-same-subject (- n)))

;;;; *** Rmail Message Deletion Commands ***

(defun rmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (rmail-desc-set-attribute rmail-desc-deleted-index t rmail-current-message)
  (run-hooks 'rmail-delete-message-hook)
  (rmail-show-message rmail-current-message))

(defun rmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
  (set-buffer rmail-buffer)
  (let ((msg rmail-current-message))
    (while (and (> msg 0)
		(not (rmail-desc-attr-p rmail-desc-deleted-index msg)))
      (setq msg (1- msg)))
    (if (= msg 0)
	(error "No previous deleted message")
      (rmail-desc-set-attribute rmail-desc-deleted-index nil msg)
      (rmail-show-message msg)
      (if (rmail-summary-exists)
	  (save-excursion
	    (set-buffer rmail-summary-buffer)
	    (rmail-summary-mark-undeleted msg)))
      (rmail-maybe-display-summary))))

;;; mbox: ready
(defun rmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward.

Returns t if a new message is displayed after the delete, or nil otherwise."
  (interactive "P")
  (rmail-desc-set-attribute rmail-desc-deleted-index t rmail-current-message)
  (run-hooks 'rmail-delete-message-hook)
  (let ((del-msg rmail-current-message))
    (if (rmail-summary-exists)
	(rmail-select-summary
	 (rmail-summary-mark-deleted del-msg)))
    (prog1 (rmail-next-undeleted-message (if backward -1 1))
      (rmail-maybe-display-summary))))

;;; mbox: ready
(defun rmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-delete-forward t))

(defun rmail-expunge-confirmed ()
  "Return t if deleted message should be expunged.  If necessary, ask the user.
See also user-option `rmail-confirm-expunge'."
  (set-buffer rmail-buffer)
  (let ((some-deleted))
    (dotimes (i rmail-total-messages)
      (if (rmail-desc-deleted-p (1+ i))
	  (setq some-deleted t)))
    (or (not some-deleted)
	(null rmail-confirm-expunge)
	(funcall rmail-confirm-expunge
		 "Erase deleted messages from Rmail file? "))))

(defun rmail-only-expunge ()
  "Actually erase all deleted messages in the file."
  (interactive)
  (message "Expunging deleted messages...")
  ;; Discard all undo records for this buffer.
  (or (eq buffer-undo-list t) (setq buffer-undo-list nil))
  ;; Remove the messages from the buffer and from the Rmail message
  ;; descriptor vector.
  (rmail-desc-prune-deleted-messages 'rmail-expunge-callback)
  ;; Update the Rmail message counter, deal with the summary buffer,
  ;; show the current message and update the User status.
  (setq rmail-total-messages (rmail-desc-get-count))
  (rmail-show-message rmail-current-message t)
  (when rmail-summary-buffer
    (with-current-buffer rmail-summary-buffer
      (rmail-update-summary)))
  (message "Expunging deleted messages...done"))

;;; mbox: ready
(defun rmail-expunge-callback (n)
  "Called after message N has been pruned to update the current Rmail
  message counter."
  (if (< n rmail-current-message)
      (setq rmail-current-message (1- rmail-current-message))))

;;; mbox: ready
(defun rmail-expunge ()
  "Erase deleted messages from Rmail file and summary buffer."
  (interactive)
  (when (rmail-expunge-confirmed)
    (rmail-only-expunge)))

;;;; *** Rmail Mailing Commands ***

;;; mbox: In progress.  I'm still not happy with the initial citation
;;; stuff. -pmr
(defun rmail-start-mail (&optional noerase to subject in-reply-to cc
				   replybuffer sendactions same-window others)
  (let (yank-action)
    (if replybuffer
	(setq yank-action (list 'insert-buffer replybuffer)))
    (setq others (cons (cons "cc" cc) others))
    (setq others (cons (cons "in-reply-to" in-reply-to) others))
    (if same-window
	(compose-mail to subject others
		      noerase nil
		      yank-action sendactions)
      (if rmail-mail-new-frame
	  (prog1
	      (compose-mail to subject others
			    noerase 'switch-to-buffer-other-frame
			    yank-action sendactions)
	    ;; This is not a standard frame parameter;
	    ;; nothing except sendmail.el looks at it.
	    (modify-frame-parameters (selected-frame)
				     '((mail-dedicated-frame . t))))
	(compose-mail to subject others
		      noerase 'switch-to-buffer-other-window
		      yank-action sendactions)))))

(defun rmail-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (rmail-start-mail nil nil nil nil nil rmail-view-buffer))

(defun rmail-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (rmail-start-mail t))

(defun rmail-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (if (= rmail-total-messages 0)
      (error "No messages in this file"))
  (save-excursion
    (save-restriction
      (let* ((msgnum rmail-current-message)
	     (from (rmail-header-get-header "from"))
	     (reply-to (or (rmail-header-get-header "reply-to" nil t) from))
	     (cc (unless just-sender
		   (rmail-header-get-header "cc" nil t)))
	     (subject (rmail-header-get-header "subject"))
	     (date (rmail-header-get-header "date"))
	     (to (or (rmail-header-get-header "to" nil t) ""))
	     (message-id (rmail-header-get-header "message-id"))
	     (references (rmail-header-get-header "references" nil nil t))
	     (resent-to (rmail-header-get-header "resent-reply-to" nil t))
	     (resent-cc (unless just-sender
			  (rmail-header-get-header "resent-cc" nil t)))
	     (resent-reply-to (or (rmail-header-get-header "resent-to" nil t) "")))
        ;; Merge the resent-to and resent-cc into the to and cc.
        (if (and resent-to (not (equal resent-to "")))
            (if (not (equal to ""))
                (setq to (concat to ", " resent-to))
              (setq to resent-to)))
        (if (and resent-cc (not (equal resent-cc "")))
            (if (not (equal cc ""))
                (setq cc (concat cc ", " resent-cc))
              (setq cc resent-cc)))
        ;; Add `Re: ' to subject if not there already.
        (and (stringp subject)
             (setq subject
                   (concat rmail-reply-prefix
                           (if (let ((case-fold-search t))
                                 (string-match rmail-reply-regexp subject))
                               (substring subject (match-end 0))
                             subject))))
        ;; Now setup the mail reply buffer.
        (rmail-start-mail
         nil
         ;; Using mail-strip-quoted-names is undesirable with newer
         ;; mailers since they can handle the names unstripped.  I
         ;; don't know whether there are other mailers that still need
         ;; the names to be stripped.
         (mail-strip-quoted-names reply-to)
         subject
         (rmail-make-in-reply-to-field from date message-id)
         (if just-sender
             nil
           ;; mail-strip-quoted-names is NOT necessary for
           ;; rmail-dont-reply-to to do its job.
           (let* ((cc-list (rmail-dont-reply-to
                            (mail-strip-quoted-names
                             (if (null cc) to (concat to ", " cc))))))
             (if (string= cc-list "") nil cc-list)))
         rmail-view-buffer
         (list (list 'rmail-reply-callback rmail-buffer "answered" t msgnum))
         nil
         (list (cons "References" (concat (mapconcat 'identity references " ")
                                          " " message-id))))))))

(defun rmail-reply-callback (buffer attr state n)
  "Mail reply callback function.
Sets ATTR (a string) if STATE is
non-nil, otherwise clears it.  N is the message number.
BUFFER, possibly narrowed, contains an mbox mail message."
  (save-excursion
    (set-buffer buffer)
    (rmail-set-attribute attr state n)))

(defun rmail-mark-message (msgnum-list attr-index)
  "Set attribute ATTRIBUTE-INDEX in the message of the car of MSGNUM-LIST.
This is used in the send-actions for
message buffers.  MSGNUM-LIST is a list of the form (MSGNUM)."
  (save-excursion
    (let ((n (car msgnum-list)))
      (set-buffer rmail-buffer)
      (rmail-narrow-to-message n)
      (rmail-desc-set-attribute attr-index t n))))

(defun rmail-narrow-to-message (n)
  "Narrow the current (rmail) buffer to bracket message N."
  (widen)
  (narrow-to-region (rmail-desc-get-start n) (rmail-desc-get-end n)))

(defun rmail-make-in-reply-to-field (from date message-id)
  (cond ((not from)
         (if message-id
             message-id
             nil))
        (mail-use-rfc822
         (require 'rfc822)
         (let ((tem (car (rfc822-addresses from))))
           (if message-id
               (if (or (not tem)
		       (string-match
			(regexp-quote (if (string-match "@[^@]*\\'" tem)
					  (substring tem 0
						     (match-beginning 0))
					tem))
			message-id))
                   ;; missing From, or Message-ID is sufficiently informative
                   message-id
                   (concat message-id " (" tem ")"))
	     ;; Copy TEM, discarding text properties.
	     (setq tem (copy-sequence tem))
	     (set-text-properties 0 (length tem) nil tem)
	     (setq tem (copy-sequence tem))
	     ;; Use prin1 to fake RFC822 quoting
	     (let ((field (prin1-to-string tem)))
	       (if date
		   (concat field "'s message of " date)
		   field)))))
        ((let* ((foo "[^][\000-\037()<>@,;:\\\" ]+")
                (bar "[^][\000-\037()<>@,;:\\\"]+"))
	   ;; These strings both match all non-ASCII characters.
           (or (string-match (concat "\\`[ \t]*\\(" bar
                                     "\\)\\(<" foo "@" foo ">\\)?[ \t]*\\'")
                             ;; "Unix Loser <Foo@bar.edu>" => "Unix Loser"
                             from)
               (string-match (concat "\\`[ \t]*<" foo "@" foo ">[ \t]*(\\("
                                     bar "\\))[ \t]*\\'")
                             ;; "<Bugs@bar.edu>" (Losing Unix) => "Losing Unix"
                             from)))
         (let ((start (match-beginning 1))
               (end (match-end 1)))
           ;; Trim whitespace which above regexp match allows
           (while (and (< start end)
                       (memq (aref from start) '(?\t ?\ )))
             (setq start (1+ start)))
           (while (and (< start end)
                       (memq (aref from (1- end)) '(?\t ?\ )))
             (setq end (1- end)))
           (let ((field (substring from start end)))
             (if date (setq field (concat "message from " field " on " date)))
             (if message-id
                 ;; "<AA259@bar.edu> (message from Unix Loser on 1-Apr-89)"
                 (concat message-id " (" field ")")
                 field))))
        (t
         ;; If we can't kludge it simply, do it correctly
         (let ((mail-use-rfc822 t))
           (rmail-make-in-reply-to-field from date message-id)))))

;;; mbox: ready
(defun rmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
  (if (= rmail-total-messages 0)
      (error "No messages in this file"))
  (if resend
      (call-interactively 'rmail-resend)
    (let ((forward-buffer rmail-buffer)
	  (msgnum rmail-current-message)
	  (subject (concat "["
			   (let ((from (or (mail-fetch-field "From")
					   (mail-fetch-field ">From"))))
			     (if from
				 (concat (mail-strip-quoted-names from) ": ")
			       ""))
			   (or (mail-fetch-field "Subject") "")
			   "]")))
      (if (rmail-start-mail
	   nil nil subject nil nil nil
	   (list (list 'rmail-mark-message
		       forward-buffer
		       (with-current-buffer rmail-buffer
			 (rmail-desc-get-start msgnum))
		       "forwarded"))
	   ;; If only one window, use it for the mail buffer.
	   ;; Otherwise, use another window for the mail buffer
	   ;; so that the Rmail buffer remains visible
	   ;; and sending the mail will get back to it.
	   (and (not rmail-mail-new-frame) (one-window-p t)))
	  ;; The mail buffer is now current.
	  (save-excursion
	    ;; Insert after header separator--before signature if any.
	    (goto-char (mail-text-start))
	    (if (or rmail-enable-mime rmail-enable-mime-composing)
		(funcall rmail-insert-mime-forwarded-message-function
			 forward-buffer)
	      (insert "------- Start of forwarded message -------\n")
	      ;; Quote lines with `- ' if they start with `-'.
	      (let ((beg (point)) end)
		(setq end (point-marker))
		(set-marker-insertion-type end t)
		(insert-buffer-substring forward-buffer)
		(goto-char beg)
		(while (re-search-forward "^-" end t)
		  (beginning-of-line)
		  (insert "- ")
		  (forward-line 1))
		(goto-char end)
		(skip-chars-backward "\n")
		(if (< (point) end)
		    (forward-char 1))
		(delete-region (point) end)
		(set-marker end nil))
	      (insert "------- End of forwarded message -------\n"))
	    (push-mark))))))

(defun rmail-resend (address &optional from comment mail-alias-file)
  "Resend current message to ADDRESSES.
ADDRESSES should be a single address, a string consisting of several
addresses separated by commas, or a list of addresses.

Optional FROM is the address to resend the message from, and
defaults from the value of `user-mail-address'.
Optional COMMENT is a string to insert as a comment in the resent message.
Optional ALIAS-FILE is alternate aliases file to be used by sendmail,
typically for purposes of moderating a list."
  (interactive "sResend to: ")
  (if (= rmail-total-messages 0)
      (error "No messages in this file"))
  (require 'sendmail)
  (require 'mailalias)
  (unless (or (eq rmail-view-buffer (current-buffer))
	      (eq rmail-buffer (current-buffer)))
    (error "Not an Rmail buffer"))
  (if (not from) (setq from user-mail-address))
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	(mail-personal-alias-file
	 (or mail-alias-file mail-personal-alias-file))
	(mailbuf rmail-buffer))
    (unwind-protect
	(with-current-buffer tembuf
	  ;;>> Copy message into temp buffer
	  (if rmail-enable-mime
	      (funcall rmail-insert-mime-resent-message-function mailbuf)
	    (insert-buffer-substring mailbuf))
	  (goto-char (point-min))
	  ;; Delete any Sender field, since that's not specifiable.
	  ; Only delete Sender fields in the actual header.
	  (re-search-forward "^$" nil 'move)
	  ; Using "while" here rather than "if" because some buggy mail
	  ; software may have inserted multiple Sender fields.
	  (while (re-search-backward "^Sender:" nil t)
	    (let (beg)
	      (setq beg (point))
	      (forward-line 1)
	      (while (looking-at "[ \t]")
		(forward-line 1))
	      (delete-region beg (point))))
	  ; Go back to the beginning of the buffer so the Resent- fields
	  ; are inserted there.
	  (goto-char (point-min))
	  ;;>> Insert resent-from:
	  (insert "Resent-From: " from "\n")
	  (insert "Resent-Date: " (mail-rfc822-date) "\n")
	  ;;>> Insert resent-to: and bcc if need be.
	  (let ((before (point)))
	    (if mail-self-blind
		(insert "Resent-Bcc: " (user-login-name) "\n"))
	    (insert "Resent-To: " (if (stringp address)
			       address
			     (mapconcat 'identity address ",\n\t"))
		    "\n")
	    ;; Expand abbrevs in the recipients.
	    (save-excursion
	      (if (featurep 'mailabbrev)
		  (let ((end (point-marker))
			(local-abbrev-table mail-abbrevs)
			(old-syntax-table (syntax-table)))
		    (if (and (not (vectorp mail-abbrevs))
			     (file-exists-p mail-personal-alias-file))
			(build-mail-abbrevs))
		    (unless mail-abbrev-syntax-table
		      (mail-abbrev-make-syntax-table))
		    (set-syntax-table mail-abbrev-syntax-table)
		    (goto-char before)
		    (while (and (< (point) end)
				(progn (forward-word 1)
				       (<= (point) end)))
		      (expand-abbrev))
		    (set-syntax-table old-syntax-table))
		(expand-mail-aliases before (point)))))
	  ;;>> Set up comment, if any.
	  (if (and (sequencep comment) (not (zerop (length comment))))
	      (let ((before (point))
		    after)
		(insert comment)
		(or (eolp) (insert "\n"))
		(setq after (point))
		(goto-char before)
		(while (< (point) after)
		  (insert "Resent-Comment: ")
		  (forward-line 1))))
	  ;; Don't expand aliases in the destination fields
	  ;; of the original message.
	  (let (mail-aliases)
	    (funcall send-mail-function)))
      (kill-buffer tembuf))
    (with-current-buffer rmail-buffer
      (rmail-set-attribute "resent" t rmail-current-message))))

(defvar mail-unsent-separator
  (concat "^ *---+ +Unsent message follows +---+ *$\\|"
	  "^ *---+ +Returned message +---+ *$\\|"
	  "^ *---+ *Returned mail follows *---+ *$\\|"
	  "^Start of returned message$\\|"
	  "^---+ Below this line is a copy of the message.$\\|"
	  "^ *---+ +Original message +---+ *$\\|"
	  "^ *--+ +begin message +--+ *$\\|"
	  "^ *---+ +Original message follows +---+ *$\\|"
	  "^ *---+ +Your message follows +---+ *$\\|"
	  "^|? *---+ +Message text follows: +---+ *|?$\\|"
	  "^ *---+ +This is a copy of \\w+ message, including all the headers.*---+ *$")
  "A regexp that matches the separator before the text of a failed message.")

(defvar mail-mime-unsent-header "^Content-Type: message/rfc822 *$"
 "A regexp that matches the header of a MIME body part with a failed message.")

(defun rmail-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message.
If the failed message is a MIME multipart message, it is searched for a
body part with a header which matches the variable `mail-mime-unsent-header'.
Otherwise, the variable `mail-unsent-separator' should match the string that
delimits the returned original message.
The variable `rmail-retry-ignored-headers' is a regular expression
specifying headers which should not be copied into the new message."
  (interactive)
  (if (= rmail-total-messages 0)
      (error "No messages in this file"))
  (require 'mail-utils)
  (let ((rmail-this-buffer (current-buffer))
	(msgnum rmail-current-message)
	bounce-start bounce-end bounce-indent resending
	(content-type
	 (save-excursion
	   (save-restriction
	     (rmail-header-get-header "Content-Type")))))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
	(if (and content-type
		 (string-match
		  ";[\n\t ]*boundary=\"?\\([-0-9a-z'()+_,./:=? ]+\\)\"?"
		  content-type))
	    ;; Handle a MIME multipart bounce message.
	    (let ((codestring
		   (concat "\n--"
			   (substring content-type (match-beginning 1)
				      (match-end 1)))))
	      (unless (re-search-forward mail-mime-unsent-header nil t)
		(error "Cannot find beginning of header in failed message"))
	      (unless (search-forward "\n\n" nil t)
		(error "Cannot find start of Mime data in failed message"))
	      (setq bounce-start (point))
	      (if (search-forward codestring nil t)
		  (setq bounce-end (match-beginning 0))
		(setq bounce-end (point-max))))
	  ;; Non-MIME bounce.
	  (or (re-search-forward mail-unsent-separator nil t)
	      (error "Cannot parse this as a failure message"))
	  (skip-chars-forward "\n")
	  ;; Support a style of failure message in which the original
	  ;; message is indented, and included within lines saying
	  ;; `Start of returned message' and `End of returned message'.
	  (if (looking-at " +Received:")
	      (progn
		(setq bounce-start (point))
		(skip-chars-forward " ")
		(setq bounce-indent (- (current-column)))
		(goto-char (point-max))
		(re-search-backward "^End of returned message$" nil t)
		(setq bounce-end (point)))
	    ;; One message contained a few random lines before
	    ;; the old message header.  The first line of the
	    ;; message started with two hyphens.  A blank line
	    ;; followed these random lines.  The same line
	    ;; beginning with two hyphens was possibly marking
	    ;; the end of the message.
	    (if (looking-at "^--")
		(let ((boundary (buffer-substring-no-properties
				 (point)
				 (progn (end-of-line) (point)))))
		  (search-forward "\n\n")
		  (skip-chars-forward "\n")
		  (setq bounce-start (point))
		  (goto-char (point-max))
		  (search-backward (concat "\n\n" boundary) bounce-start t)
		  (setq bounce-end (point)))
	      (setq bounce-start (point)
		    bounce-end (point-max)))
	    (unless (search-forward "\n\n" nil t)
	      (error "Cannot find end of header in failed message"))))))
    ;; We have found the message that bounced, within the current message.
    ;; Now start sending new message; default header fields from original.
    ;; Turn off the usual actions for initializing the message body
    ;; because we want to get only the text from the failure message.
    (let (mail-signature mail-setup-hook)
      (if (rmail-start-mail nil nil nil nil nil rmail-this-buffer
			    (list (list 'rmail-mark-message
					rmail-this-buffer
					(aref rmail-msgref-vector msgnum)
					"retried")))
	  ;; Insert original text as initial text of new draft message.
	  ;; Bind inhibit-read-only since the header delimiter
	  ;; of the previous message was probably read-only.
	  (let ((inhibit-read-only t)
		rmail-displayed-headers
		rmail-ignored-headers)
	    (erase-buffer)
	    (insert-buffer-substring rmail-this-buffer
				     bounce-start bounce-end)
	    (goto-char (point-min))
	    (if bounce-indent
		(indent-rigidly (point-min) (point-max) bounce-indent))
	    (rmail-clear-headers rmail-retry-ignored-headers)
	    (rmail-clear-headers "^sender:\\|^return-path:\\|^received:")
	    (mail-sendmail-delimit-header)
	    (save-restriction
	      (narrow-to-region (point-min) (mail-header-end))
	      (setq resending (mail-fetch-field "resent-to"))
	      (if mail-self-blind
		  (if resending
		      (insert "Resent-Bcc: " (user-login-name) "\n")
		    (insert "BCC: " (user-login-name) "\n"))))
	    (goto-char (point-min))
	    (mail-position-on-field (if resending "Resent-To" "To") t))))))

(defun rmail-summary-exists ()
  "Non-nil iff in an RMAIL buffer and an associated summary buffer exists.
In fact, the non-nil value returned is the summary buffer itself."
  (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
       rmail-summary-buffer))

(defun rmail-summary-displayed ()
  "t iff in RMAIL buffer and an associated summary buffer is displayed."
  (and rmail-summary-buffer (get-buffer-window rmail-summary-buffer)))

(defcustom rmail-redisplay-summary nil
  "*Non-nil means Rmail should show the summary when it changes.
This has an effect only if a summary buffer exists."
  :type 'boolean
  :group 'rmail-summary)

(defcustom rmail-summary-window-size nil
  "*Non-nil means specify the height for an Rmail summary window."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'rmail-summary)

;; Put the summary buffer back on the screen, if user wants that.
(defun rmail-maybe-display-summary ()
  (let ((selected (selected-window))
	window)
    ;; If requested, make sure the summary is displayed.
    (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
	 rmail-redisplay-summary
	 (if (get-buffer-window rmail-summary-buffer 0)
	     ;; It's already in some frame; show that one.
	     (let ((frame (window-frame
			   (get-buffer-window rmail-summary-buffer 0))))
	       (make-frame-visible frame)
	       (raise-frame frame))
	   (display-buffer rmail-summary-buffer)))
    ;; If requested, set the height of the summary window.
    (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
	 rmail-summary-window-size
	 (setq window (get-buffer-window rmail-summary-buffer))
	 ;; Don't try to change the size if just one window in frame.
	 (not (eq window (frame-root-window (window-frame window))))
	 (unwind-protect
	     (progn
	       (select-window window)
	       (enlarge-window (- rmail-summary-window-size (window-height))))
	   (select-window selected)))))

;;;; *** Rmail Local Fontification ***

(defun rmail-fontify-buffer-function ()
  ;; This function's symbol is bound to font-lock-fontify-buffer-function.
  (add-hook 'rmail-show-message-hook 'rmail-fontify-message nil t)
  ;; If we're already showing a message, fontify it now.
  (if rmail-current-message (rmail-fontify-message))
  ;; Prevent Font Lock mode from kicking in.
  (setq font-lock-fontified t))

(defun rmail-unfontify-buffer-function ()
  ;; This function's symbol is bound to font-lock-fontify-unbuffer-function.
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t) (inhibit-read-only t)
	before-change-functions after-change-functions
	buffer-file-name buffer-file-truename)
    (save-restriction
      (widen)
      (remove-hook 'rmail-show-message-hook 'rmail-fontify-message t)
      (remove-text-properties (point-min) (point-max) '(rmail-fontified nil))
      (font-lock-default-unfontify-buffer)
      (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil)))))

(defun rmail-fontify-message ()
  "Fontify the current message if it is not already fontified."
  (when (text-property-any (point-min) (point-max) 'rmail-fontified nil)
    (let ((modified (buffer-modified-p))
	  (buffer-undo-list t) (inhibit-read-only t)
	  before-change-functions after-change-functions
	  buffer-file-name buffer-file-truename)
      (save-excursion
	(save-match-data
	  (add-text-properties (point-min) (point-max) '(rmail-fontified t))
	  (font-lock-fontify-region (point-min) (point-max))
	  (and (not modified) (buffer-modified-p)
	       (set-buffer-modified-p nil)))))))

;;; Speedbar support for RMAIL files.
(eval-when-compile (require 'speedbar))

(defvar rmail-speedbar-match-folder-regexp "^[A-Z0-9]+\\(\\.[A-Z0-9]+\\)?$"
  "*This regex is used to match folder names to be displayed in speedbar.
Enabling this will permit speedbar to display your folders for easy
browsing, and moving of messages.")

(defvar rmail-speedbar-last-user nil
  "The last user to be displayed in the speedbar.")

(defvar rmail-speedbar-key-map nil
  "Keymap used when in rmail display mode.")

(defun rmail-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance rmail."
  (if rmail-speedbar-key-map
      nil
    (setq rmail-speedbar-key-map (speedbar-make-specialized-keymap))

    (define-key rmail-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "r" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "M"
      'rmail-speedbar-move-message-to-folder-on-line)))

(defvar rmail-speedbar-menu-items
  '(["Read Folder" speedbar-edit-line t]
    ["Move message to folder" rmail-speedbar-move-message-to-folder-on-line
     (save-excursion (beginning-of-line)
		     (looking-at "<M> "))])
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (rmail-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'rmail-install-speedbar-variables))

(defun rmail-speedbar-buttons (buffer)
  "Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder."
  (let ((from nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (not (re-search-forward "^Reply-To: " nil t))
	  (if (not (re-search-forward "^From:? " nil t))
	      (setq from t)))
      (if from
	  nil
	(setq from (buffer-substring (point) (save-excursion
					       (end-of-line)
					       (point))))))
    (goto-char (point-min))
    (if (and (looking-at "Reply to:")
	     (equal from rmail-speedbar-last-user))
	nil
      (setq rmail-speedbar-last-user from)
      (erase-buffer)
      (insert "Reply To:\n")
      (if (stringp from)
	  (speedbar-insert-button from 'speedbar-directory-face 'highlight
				  'rmail-speedbar-button 'rmail-reply))
      (insert "Folders:\n")
      (let* ((case-fold-search nil)
	     (df (directory-files (save-excursion (set-buffer buffer)
						  default-directory)
				  nil rmail-speedbar-match-folder-regexp)))
	(while df
	  (speedbar-insert-button "<M>" 'speedbar-button-face 'highlight
				  'rmail-speedbar-move-message (car df))
	  (speedbar-insert-button (car df) 'speedbar-file-face 'highlight
				  'rmail-speedbar-find-file nil t)
	  (setq df (cdr df)))))))

(defun rmail-speedbar-button (text token indent)
  "Execute an rmail command specified by TEXT.
The command used is TOKEN.  INDENT is not used."
  (speedbar-with-attached-buffer
   (funcall token t)))

(defun rmail-speedbar-find-file (text token indent)
  "Load in the rmail file TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Loading in RMAIL file %s..." text)
   (find-file text)))

(defun rmail-speedbar-move-message-to-folder-on-line ()
  "If the current line is a folder, move current message to it."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "<M> " (save-excursion (end-of-line) (point)) t)
	(progn
	  (forward-char -2)
	  (speedbar-do-function-pointer)))))

(defun rmail-speedbar-move-message (text token indent)
  "From button TEXT, copy current message to the rmail file specified by TOKEN.
TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Moving message to %s" token)
   (rmail-output-to-rmail-file token)))

; Functions for setting, getting and encoding the POP password.
; The password is encoded to prevent it from being easily accessible
; to "prying eyes."  Obviously, this encoding isn't "real security,"
; nor is it meant to be.

;;;###autoload
(defun rmail-set-remote-password (password)
  "Set PASSWORD to be used for retrieving mail from a POP or IMAP server."
  (interactive "sPassword: ")
  (if password
      (setq rmail-encoded-remote-password
	    (rmail-encode-string password (emacs-pid)))
    (setq rmail-remote-password nil)
    (setq rmail-encoded-remote-password nil)))

(defun rmail-get-remote-password (imap)
  "Get the password for retrieving mail from a POP or IMAP server.  If none
has been set, then prompt the user for one."
  (when (not rmail-encoded-remote-password)
    (if (not rmail-remote-password)
	(setq rmail-remote-password
	      (read-passwd (if imap
			       "IMAP password: "
			     "POP password: "))))
    (rmail-set-remote-password rmail-remote-password)
    (setq rmail-remote-password nil))
  (rmail-encode-string rmail-encoded-remote-password (emacs-pid)))

(defun rmail-have-password ()
  (or rmail-remote-password rmail-encoded-remote-password))

(defun rmail-encode-string (string mask)
 "Encode STRING with integer MASK, by taking the exclusive OR of the
lowest byte in the mask with the first character of string, the
second-lowest-byte with the second character of the string, etc.,
restarting at the lowest byte of the mask whenever it runs out.
Returns the encoded string.  Calling the function again with an
encoded string (and the same mask) will decode the string."
 (setq mask (abs mask))			; doesn't work if negative
 (let* ((string-vector (string-to-vector string)) (i 0)
	(len (length string-vector)) (curmask mask) charmask)
   (while (< i len)
     (if (= curmask 0)
	 (setq curmask mask))
     (setq charmask (% curmask 256))
     (setq curmask (lsh curmask -8))
     (aset string-vector i (logxor charmask (aref string-vector i)))
     (setq i (1+ i)))
   (concat string-vector)))

;;;;  Desktop support

(defun rmail-restore-desktop-buffer (desktop-buffer-file-name
                                     desktop-buffer-name
                                     desktop-buffer-misc)
  "Restore an rmail buffer specified in a desktop file."
  (condition-case error
      (progn
        (rmail-input desktop-buffer-file-name)
        (if (eq major-mode 'rmail-mode)
            (current-buffer)
          rmail-buffer))
    (file-locked
      (kill-buffer (current-buffer))
      nil)))

(add-to-list 'desktop-buffer-mode-handlers
	     '(rmail-mode . rmail-restore-desktop-buffer))

(provide 'rmail)

;;; arch-tag: cff0a950-57fe-4f73-a86e-91ff75afd06c
;;; rmail.el ends here
