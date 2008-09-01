;;; pmail.el --- main code of "PMAIL" mail reader for Emacs

;; Copyright (C) 1985, 1986, 1987, 1988, 1993, 1994, 1995, 1996, 1997, 1998,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Souped up by shane@mit-ajax based on ideas of rlk@athena.mit.edu
;;   New features include attribute and keyword support, message
;;   selection by dispatch table, summary by attributes and keywords,
;;   expunging by dispatch table, sticky options for file commands.

;; Extended by Bob Weiner of Motorola
;;   New features include: pmail and pmail-summary buffers remain
;;   synchronized and key bindings basically operate the same way in both
;;   buffers, summary by topic or by regular expression, pmail-reply-prefix
;;   variable, and a bury pmail buffer (wipe) command.
;;

(eval-when-compile
  (require 'font-lock)
  (require 'mailabbrev)
  (require 'mule-util))                ; for detect-coding-with-priority

(require 'pmaildesc)
(require 'pmailhdr)
(require 'pmailkwd)
(require 'mail-parse)

(defvar deleted-head)
(defvar font-lock-fontified)
(defvar mail-abbrev-syntax-table)
(defvar mail-abbrevs)
(defvar messages-head)
(defvar pmail-use-spam-filter)
(defvar rsf-beep)
(defvar rsf-sleep-after-message)
(defvar total-messages)
(defvar tool-bar-map)

; These variables now declared in paths.el.
;(defvar pmail-spool-directory "/usr/spool/mail/"
;  "This is the name of the directory used by the system mailer for\n\
;delivering new mail.  Its name should end with a slash.")
;(defvar pmail-file-name
;  (expand-file-name "~/PMAIL")
;  "")

;; Temporary support for mbox.
(defcustom pmail-file-name "~/PMAIL"
  "*Name of user's primary mail file."
  :type 'string
  :group 'rmail
  :version "21.1")

(defgroup pmail nil
  "Mail reader for Emacs."
  :group 'mail)

(defgroup pmail-retrieve nil
  "Pmail retrieval options."
  :prefix "pmail-"
  :group 'pmail)

(defgroup pmail-files nil
  "Pmail files."
  :prefix "pmail-"
  :group 'pmail)

(defgroup pmail-headers nil
  "Pmail header options."
  :prefix "pmail-"
  :group 'pmail)

(defgroup pmail-reply nil
  "Pmail reply options."
  :prefix "pmail-"
  :group 'pmail)

(defgroup pmail-summary nil
  "Pmail summary options."
  :prefix "pmail-"
  :prefix "pmail-summary-"
  :group 'pmail)

(defgroup pmail-output nil
  "Output message to a file."
  :prefix "pmail-output-"
  :prefix "pmail-"
  :group 'pmail)

(defgroup pmail-edit nil
  "Pmail editing."
  :prefix "pmail-edit-"
  :group 'pmail)

(defgroup pmail-obsolete nil
  "Pmail obsolete customization variables."
  :group 'pmail)

(defcustom pmail-movemail-program nil
  "If non-nil, the file name of the `movemail' program."
  :group 'pmail-retrieve
  :type '(choice (const nil) string))

(defcustom pmail-pop-password nil
  "*Password to use when reading mail from POP server.
Please use `pmail-remote-password' instead."
  :type '(choice (string :tag "Password")
		 (const :tag "Not Required" nil))
  :group 'pmail-obsolete)

(defcustom pmail-pop-password-required nil
  "*Non-nil if a password is required when reading mail from a POP server.
Please use pmail-remote-password-required instead."
  :type 'boolean
  :group 'pmail-obsolete)

(defcustom pmail-remote-password nil
  "*Password to use when reading mail from a remote server.
This setting is ignored for mailboxes whose URL already contains a password."
  :type '(choice (string :tag "Password")
		 (const :tag "Not Required" nil))
  :set-after '(pmail-pop-password)
  :set #'(lambda (symbol value)
	   (set-default symbol
			(if (and (not value)
                                 (boundp 'pmail-pop-password)
				 pmail-pop-password)
			    pmail-pop-password
			  value))
	   (setq pmail-pop-password nil))
  :group 'pmail-retrieve
  :version "22.1")

(defcustom pmail-remote-password-required nil
  "*Non-nil if a password is required when reading mail from a remote server."
  :type 'boolean
  :set-after '(pmail-pop-password-required)
  :set #'(lambda (symbol value)
	   (set-default symbol
			(if (and (not value)
                                 (boundp 'pmail-pop-password-required)
				 pmail-pop-password-required)
			    pmail-pop-password-required
			  value))
	   (setq pmail-pop-password-required nil))
  :group 'pmail-retrieve
  :version "22.1")

(defcustom pmail-movemail-flags nil
  "*List of flags to pass to movemail.
Most commonly used to specify `-g' to enable GSS-API authentication
or `-k' to enable Kerberos authentication."
  :type '(repeat string)
  :group 'pmail-retrieve
  :version "20.3")

(defvar pmail-remote-password-error "invalid usercode or password\\|
unknown user name or bad password\\|Authentication failed\\|MU_ERR_AUTH_FAILURE"
  "Regular expression matching incorrect-password POP or IMAP server error
messages.
If you get an incorrect-password error that this expression does not match,
please report it with \\[report-emacs-bug].")

(defvar pmail-encoded-remote-password nil)

(defcustom pmail-preserve-inbox nil
  "*Non-nil means leave incoming mail in the user's inbox--don't delete it."
  :type 'boolean
  :group 'pmail-retrieve)

(defcustom pmail-movemail-search-path nil
    "*List of directories to search for movemail (in addition to `exec-path')."
    :group 'pmail-retrieve
    :type '(repeat (directory)))

(declare-function mail-position-on-field "sendmail" (field &optional soft))
(declare-function mail-text-start "sendmail" ())
(declare-function pmail-update-summary "pmailsum" (&rest ignore))
(declare-function unrmail "unrmail" (file to-file))
(declare-function rmail-dont-reply-to "mail-utils" (destinations))
(declare-function pmail-summary-goto-msg "pmailsum" (&optional n nowarn skip-pmail))
(declare-function pmail-summary-pmail-update "pmailsum" ())
(declare-function pmail-summary-update "pmailsum" (n))

(defun pmail-probe (prog)
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

(defun pmail-autodetect ()
  "Determine the file name of the `movemail' program and return its flavor.
If `pmail-movemail-program' is non-nil, use it.
Otherwise, look for `movemail' in the directories in
`pmail-movemail-search-path', those in `exec-path', and `exec-directory'."
  (if pmail-movemail-program
      (pmail-probe pmail-movemail-program)
    (catch 'scan
      (dolist (dir (append pmail-movemail-search-path exec-path
			   (list exec-directory)))
	(when (and dir (file-accessible-directory-p dir))
	  ;; Previously, this didn't have to work on Windows, because
	  ;; pmail-insert-inbox-text before r1.439 fell back to using
	  ;; (expand-file-name "movemail" exec-directory) and just
	  ;; assuming it would work.
	  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-02/msg00087.html
	  (let ((progname (expand-file-name
			   (concat "movemail"
				   (if (memq system-type '(ms-dos windows-nt))
				       ".exe")) dir)))
	    (when (and (not (file-directory-p progname))
		       (file-executable-p progname))
	      (let ((x (pmail-probe progname)))
		(when x
		  (setq pmail-movemail-program progname)
		  (throw 'scan x))))))))))

(defvar pmail-movemail-variant-in-use nil
  "The movemail variant currently in use. Known variants are:

  `emacs'     Means any implementation, compatible with the native Emacs one.
              This is the default;
  `mailutils' Means GNU mailutils implementation, capable of handling full
mail URLs as the source mailbox.")

;;;###autoload
(defun pmail-movemail-variant-p (&rest variants)
  "Return t if the current movemail variant is any of VARIANTS.
Currently known variants are 'emacs and 'mailutils."
  (when (not pmail-movemail-variant-in-use)
    ;; Autodetect
    (setq pmail-movemail-variant-in-use (pmail-autodetect)))
  (not (null (member pmail-movemail-variant-in-use variants))))

;; Call for effect, to set pmail-movemail-program (if not set by the
;; user), and pmail-movemail-variant-in-use. Used by various functions.
;; I'm not sure if M-x pmail is the only entry point to this package.
;; If so, this can be moved there.
(pmail-movemail-variant-p)

;;;###autoload
(defcustom pmail-dont-reply-to-names nil "\
*A regexp specifying addresses to prune from a reply message.
A value of nil means exclude your own email address as an address
plus whatever is specified by `pmail-default-dont-reply-to-names'."
  :type '(choice regexp (const :tag "Your Name" nil))
  :group 'pmail-reply)

;;;###autoload
(defvar pmail-default-dont-reply-to-names "\\`info-" "\
A regular expression specifying part of the default value of the
variable `pmail-dont-reply-to-names', for when the user does not set
`pmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's email address and name.)
It is useful to set this variable in the site customization file.")

;;;###autoload
(defcustom pmail-ignored-headers
  (concat "^via:\\|^mail-from:\\|^origin:\\|^references:\\|^sender:"
	  "\\|^status:\\|^received:\\|^x400-originator:\\|^x400-recipients:"
	  "\\|^x400-received:\\|^x400-mts-identifier:\\|^x400-content-type:"
	  "\\|^\\(resent-\\|\\)message-id:\\|^summary-line:\\|^resent-date:"
	  "\\|^nntp-posting-host:\\|^path:\\|^x-char.*:\\|^x-face:\\|^face:"
	  "\\|^x-mailer:\\|^delivered-to:\\|^lines:"
	  "\\|^content-transfer-encoding:\\|^x-coding-system:"
	  "\\|^return-path:\\|^errors-to:\\|^return-receipt-to:"
	  "\\|^precedence:\\|^list-help:\\|^list-post:\\|^list-subscribe:"
	  "\\|^list-id:\\|^list-unsubscribe:\\|^list-archive:"
	  "\\|^content-length:\\|^nntp-posting-date:\\|^user-agent"
	  "\\|^importance:\\|^envelope-to:\\|^delivery-date\\|^openpgp:"
	  "\\|^mbox-line:\\|^cancel-lock:\\|^DomainKey-Signature:"
	  "\\|^resent-face:\\|^resent-x.*:\\|^resent-organization:\\|^resent-openpgp:"
	  "\\|^x-.*:\\|^domainkey-signature:\\|^original-recipient:\\|^from ")
  "*Regexp to match header fields that Pmail should normally hide.
\(See also `pmail-nonignored-headers', which overrides this regexp.)
This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Pmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[pmail-toggle-header] twice."
  :type 'regexp
  :group 'pmail-headers)

(defcustom pmail-nonignored-headers "^x-spam-status:"
  "*Regexp to match X header fields that Pmail should show.
This regexp overrides `pmail-ignored-headers'; if both this regexp
and that one match a certain header field, Pmail shows the field.
If this is nil, ignore all header fields in `pmail-ignored-headers'.

This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Pmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[pmail-toggle-header] twice."
  :type '(choice (const nil) (regexp))
  :group 'pmail-headers)

;;;###autoload
(defcustom pmail-displayed-headers nil
  "*Regexp to match Header fields that Pmail should display.
If nil, display all header fields except those matched by
`pmail-ignored-headers'."
  :type '(choice regexp (const :tag "All"))
  :group 'pmail-headers)

;;;###autoload
(defcustom pmail-retry-ignored-headers "^x-authentication-warning:" "\
*Headers that should be stripped when retrying a failed message."
  :type '(choice regexp (const nil :tag "None"))
  :group 'pmail-headers)

;;;###autoload
(defcustom pmail-highlighted-headers "^From:\\|^Subject:" "\
*Regexp to match Header fields that Pmail should normally highlight.
A value of nil means don't highlight.
See also `pmail-highlight-face'."
  :type 'regexp
  :group 'pmail-headers)

(defface pmail-highlight
  '((t (:inherit highlight)))
  "Face to use for highlighting the most important header fields."
  :group 'pmail-headers
  :version "22.1")

(defface pmail-header-name
  '((t (:inherit font-lock-function-name-face)))
  "Face to use for highlighting the header names."
  :group 'pmail-headers
  :version "23.1")

;;;###autoload
(defcustom pmail-highlight-face 'pmail-highlight "\
*Face used by Pmail for highlighting sender and subject.
See `pmail-font-lock-keywords'."
  :type '(choice (const :tag "Default" nil)
		 face)
  :group 'pmail-headers)

;;;###autoload
(defcustom pmail-delete-after-output nil "\
*Non-nil means automatically delete a message that is copied to a file."
  :type 'boolean
  :group 'pmail-files)

;;;###autoload
(defcustom pmail-primary-inbox-list nil "\
*List of files which are inboxes for user's primary mail file `~/PMAIL'.
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
  :group 'pmail-retrieve
  :group 'pmail-files)

;;;###autoload
(defcustom pmail-inbox-alist nil
  "*Alist of mail files and backup directory names.
Each element has the form (MAIL-FILE INBOX ...).  When running
pmail on MAIL-FILE, mails in all the INBOX files listed will be
moved to the MAIL-FILE.  Be sure to fully qualify your MAIL-FILE.

Example setting if procmail delivers all your spam to
~/Mail/SPAM.in and you read it from the file ~/Mail/SPAM:

\(setq pmail-inbox-alist '((\"~/Mail/SPAM\" \"~/Mail/SPAM.in\")))"
  :type '(alist :key-type file :value-type (repeat file))
  :group 'pmail-retrieve
  :group 'pmail-files
  :version "22.1")

;;;###autoload
(defcustom pmail-mail-new-frame nil
  "*Non-nil means Pmail makes a new frame for composing outgoing mail.
This is handy if you want to preserve the window configuration of
the frame where you have the PMAIL buffer displayed."
  :type 'boolean
  :group 'pmail-reply)

;;;###autoload
(defcustom pmail-secondary-file-directory "~/"
  "*Directory for additional secondary Pmail files."
  :type 'directory
  :group 'pmail-files)
;;;###autoload
(defcustom pmail-secondary-file-regexp "\\.xmail$"
  "*Regexp for which files are secondary Pmail files."
  :type 'regexp
  :group 'pmail-files)

;;;###autoload
(defcustom pmail-confirm-expunge 'y-or-n-p
  "*Whether and how to ask for confirmation before expunging deleted messages."
  :type '(choice (const :tag "No confirmation" nil)
		 (const :tag "Confirm with y-or-n-p" y-or-n-p)
		 (const :tag "Confirm with yes-or-no-p" yes-or-no-p))
  :version "21.1"
  :group 'pmail-files)

;;;###autoload
(defvar pmail-mode-hook nil
  "List of functions to call when Pmail is invoked.")

;;;###autoload
(defvar pmail-get-new-mail-hook nil
  "List of functions to call when Pmail has retrieved new mail.")

;;;###autoload
(defcustom pmail-show-message-hook nil
  "List of functions to call when Pmail displays a message."
  :type 'hook
  :options '(goto-address)
  :group 'pmail)

;;;###autoload
(defvar pmail-quit-hook nil
  "List of functions to call when quitting out of Pmail.")

;;;###autoload
(defvar pmail-delete-message-hook nil
  "List of functions to call when Pmail deletes a message.
When the hooks are called, the message has been marked deleted but is
still the current message in the Pmail buffer.")

;; These may be altered by site-init.el to match the format of mmdf files
;;  delimiting used on a given host (delim1 and delim2 from the config
;;  files).

(defvar pmail-mmdf-delim1 "^\001\001\001\001\n"
  "Regexp marking the start of an mmdf message.")
(defvar pmail-mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message.")

(defcustom pmail-message-filter nil
  "If non-nil, a filter function for new messages in PMAIL.
Called with region narrowed to the message, including headers,
before obeying `pmail-ignored-headers'."
  :group 'pmail-headers
  :type '(choice (const nil) function))

(defcustom pmail-automatic-folder-directives nil
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
  :group 'pmail
  :version "21.1"
  :type '(repeat (sexp :tag "Directive")))

(defvar pmail-reply-prefix "Re: "
  "String to prepend to Subject line when replying to a message.")

;; Some mailers use "Re(2):" or "Re^2:" or "Re: Re:" or "Re[2]:".
;; This pattern should catch all the common variants.
;; rms: I deleted the change to delete tags in square brackets
;; because they mess up RT tags.
(defvar pmail-reply-regexp "\\`\\(Re\\(([0-9]+)\\|\\[[0-9]+\\]\\|\\^[0-9]+\\)?: *\\)*"
  "Regexp to delete from Subject line before inserting `pmail-reply-prefix'.")

(defcustom pmail-display-summary nil
  "*If non-nil, Pmail always displays the summary buffer."
  :group 'pmail-summary
  :type 'boolean)

(defvar pmail-inbox-list nil)
(put 'pmail-inbox-list 'permanent-local t)

(defvar pmail-keywords nil)
(put 'pmail-keywords 'permanent-local t)

(defvar pmail-buffer nil
  "The PMAIL buffer related to the current buffer.
In an PMAIL buffer, this holds the PMAIL buffer itself.
In a summary buffer, this holds the PMAIL buffer it is a summary for.")
(put 'pmail-buffer 'permanent-local t)

;; Message counters and markers.  Deleted flags.

(defvar pmail-current-message nil)
(put 'pmail-current-message 'permanent-local t)

(defvar pmail-total-messages nil)
(put 'pmail-total-messages 'permanent-local t)

(defvar pmail-overlay-list nil)
(put 'pmail-overlay-list 'permanent-local t)

;; These are used by autoloaded pmail-summary.

(defvar pmail-summary-buffer nil)
(put 'pmail-summary-buffer 'permanent-local t)

(defvar pmail-view-buffer nil
  "Buffer which holds PMAIL message for MIME displaying.")
(put 'pmail-view-buffer 'permanent-local t)

;; `Sticky' default variables.

;; Last individual label specified to a or k.
(defvar pmail-last-label nil)
(put 'pmail-last-label 'permanent-local t)

;; Last set of values specified to C-M-n, C-M-p, C-M-s or C-M-l.
(defvar pmail-last-multi-labels nil)

(defvar pmail-last-regexp nil)
(put 'pmail-last-regexp 'permanent-local t)

(defcustom pmail-default-file "~/xmail"
  "*Default file name for \\[pmail-output]."
  :type 'file
  :group 'pmail-files)

(defcustom pmail-default-pmail-file "~/XMAIL"
  "*Default file name for \\[pmail-output-to-pmail-file]."
  :type 'file
  :group 'pmail-files)

(defcustom pmail-default-body-file "~/mailout"
  "*Default file name for \\[pmail-output-body-to-file]."
  :type 'file
  :group 'pmail-files
  :version "20.3")

;; Mule and MIME related variables.

;;;###autoload
(defvar pmail-file-coding-system nil
  "Coding system used in PMAIL file.

This is set to nil by default.")

;;;###autoload
(defcustom pmail-enable-mime nil
  "*If non-nil, PMAIL uses MIME feature.
If the value is t, PMAIL automatically shows MIME decoded message.
If the value is neither t nor nil, PMAIL does not show MIME decoded message
until a user explicitly requires it.

Even if the value is non-nil, you can't use MIME feature
if the feature specified by `pmail-mime-feature' is not available
in your session."
  :type '(choice (const :tag "on" t)
		 (const :tag "off" nil)
		 (other :tag "when asked" ask))
  :group 'pmail)

(defvar pmail-enable-mime-composing nil
  "*If non-nil, PMAIL uses `pmail-insert-mime-forwarded-message-function' to forward.")

;;;###autoload
(defvar pmail-show-mime-function nil
  "Function to show MIME decoded message of PMAIL file.
This function is called when `pmail-enable-mime' is non-nil.
It is called with no argument.")

;;;###autoload
(defvar pmail-insert-mime-forwarded-message-function nil
  "Function to insert a message in MIME format so it can be forwarded.
This function is called if `pmail-enable-mime' or
`pmail-enable-mime-composing' is non-nil.
It is called with one argument FORWARD-BUFFER, which is a
buffer containing the message to forward.  The current buffer
is the outgoing mail buffer.")

;;;###autoload
(defvar pmail-insert-mime-resent-message-function nil
  "Function to insert a message in MIME format so it can be resent.
This function is called if `pmail-enable-mime' is non-nil.
It is called with one argument FORWARD-BUFFER, which is a
buffer containing the message to forward.  The current buffer
is the outgoing mail buffer.")

;;;###autoload
(defvar pmail-search-mime-message-function nil
  "Function to check if a regexp matches a MIME message.
This function is called if `pmail-enable-mime' is non-nil.
It is called with two arguments MSG and REGEXP, where
MSG is the message number, REGEXP is the regular expression.")

;;;###autoload
(defvar pmail-search-mime-header-function nil
  "Function to check if a regexp matches a header of MIME message.
This function is called if `pmail-enable-mime' is non-nil.
It is called with three arguments MSG, REGEXP, and LIMIT, where
MSG is the message number,
REGEXP is the regular expression,
LIMIT is the position specifying the end of header.")

;;;###autoload
(defvar pmail-mime-feature 'pmail-mime
  "Feature to require to load MIME support in Pmail.
When starting Pmail, if `pmail-enable-mime' is non-nil,
this feature is required with `require'.

The default value is `pmail-mime'.  This feature is provided by
the pmail-mime package available at <http://www.m17n.org/pmail-mime/>.")

;;;###autoload
(defvar pmail-decode-mime-charset t
  "*Non-nil means a message is decoded by MIME's charset specification.
If this variable is nil, or the message has not MIME specification,
the message is decoded as normal way.

If the variable `pmail-enable-mime' is non-nil, this variables is
ignored, and all the decoding work is done by a feature specified by
the variable `pmail-mime-feature'.")

;;;###autoload
(defvar pmail-mime-charset-pattern
  (concat "^content-type:[ \t]*text/plain;"
	  "\\(?:[ \t\n]*\\(?:format\\|delsp\\)=\"?[-a-z0-9]+\"?;\\)*"
	  "[ \t\n]*charset=\"?\\([^ \t\n\";]+\\)\"?")
  "Regexp to match MIME-charset specification in a header of message.
The first parenthesized expression should match the MIME-charset name.")


;;; Regexp matching the delimiter of messages in UNIX mail format
;;; (UNIX From lines), with an initial ^.  Used in pmail-decode-from-line,
;;; which knows the exact ordering of the \\(...\\) subexpressions.
(defvar pmail-unix-mail-delimiter
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

(defvar pmail-font-lock-keywords
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
	      (2 pmail-highlight-face))
	    '("^\\(Subject:\\)\\(.*\\(\n[ \t]+.*\\)*\\)"
	      (1 font-lock-comment-face)
	      (2 pmail-highlight-face))
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
	      . 'pmail-header-name))))
  "Additional expressions to highlight in Pmail mode.")

;; Perform BODY in the summary buffer
;; in such a way that its cursor is properly updated in its own window.
(defmacro pmail-select-summary (&rest body)
  `(let ((total pmail-total-messages))
     (if (pmail-summary-displayed)
	 (let ((window (selected-window)))
	   (save-excursion
	     (unwind-protect
		 (progn
		   (pop-to-buffer pmail-summary-buffer)
		   ;; pmail-total-messages is a buffer-local var
		   ;; in the pmail buffer.
		   ;; This way we make it available for the body
		   ;; even tho the pmail buffer is not current.
		   (let ((pmail-total-messages total))
		     ,@body))
	       (select-window window))))
       (save-excursion
	 (set-buffer pmail-summary-buffer)
	 (let ((pmail-total-messages total))
	   ,@body)))
     (pmail-maybe-display-summary)))

;;;; *** Pmail Mode ***

;; This variable is dynamically bound.  The defvar is here to placate
;; the byte compiler.

(defvar pmail-enable-multibyte nil)

;; Avoid errors.
(defvar pmail-use-spam-filter nil)

(defun pmail-require-mime-maybe ()
  "Require `pmail-mime-feature' if that is non-nil.
Signal an error and set `pmail-mime-feature' to nil if the feature
isn't provided."
  (when pmail-enable-mime
    (condition-case err
	(require pmail-mime-feature)
      (error
       (display-warning
	:warning
	(format "Although MIME support is requested
by setting `pmail-enable-mime' to non-nil, the required feature
`%s' (the value of `pmail-mime-feature')
is not available in the current session.
So, the MIME support is turned off for the moment." 
		pmail-mime-feature))
       (setq pmail-enable-mime nil)))))

;;;###autoload
(defun pmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by `pmail-file-name' (a babyl format file)
 and edits that file in PMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of PMAIL commands.

May be called with file name as argument; then performs pmail editing on
that file, but does not copy any new mail into the file.
Interactively, if you supply a prefix argument, then you
have a chance to specify a file name with the minibuffer.

If `pmail-display-summary' is non-nil, make a summary for this PMAIL file."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run pmail on PMAIL file: "))))
  (pmail-require-mime-maybe)
  (let* ((file-name (expand-file-name (or file-name-arg pmail-file-name)))
	 ;; Use find-buffer-visiting, not get-file-buffer, for those users
	 ;; who have find-file-visit-truename set to t.
	 (existed (find-buffer-visiting file-name))
	 ;; This binding is necessary because we must decide if we
	 ;; need code conversion while the buffer is unibyte
	 ;; (i.e. enable-multibyte-characters is nil).
         (pmail-enable-multibyte
          (if existed
	      (with-current-buffer existed enable-multibyte-characters)
            (default-value 'enable-multibyte-characters)))
	 run-mail-hook msg-shown)
    (when (and existed (eq major-mode 'pmail-edit-mode))
      (error "Exit Pmail Edit mode before getting new mail"))
    (if (and existed (not (verify-visited-file-modtime existed)))
	(progn
	  (find-file file-name)
	  (when (and (verify-visited-file-modtime existed)
		     (eq major-mode 'pmail-mode))
	    (setq major-mode 'fundamental-mode)))
	(switch-to-buffer
	 (let ((enable-local-variables nil))
	   (find-file-noselect file-name)))
	;; As we have read a file as raw-text, the buffer is set to
	;; unibyte.  We must make it multibyte if necessary.
	(when (and pmail-enable-multibyte
		   (not enable-multibyte-characters))
	  (set-buffer-multibyte t)))
    ;; Make sure we're in pmail-mode, even if the buffer did exist and
    ;; the file was not changed.
    (unless (eq major-mode 'pmail-mode)
      ;; If file looks like a Babyl file, save it to a temp file,
      ;; convert it, and replace the current content with the
      ;; converted content.  Don't save -- let the user do it.
      (goto-char (point-min))
      (when (looking-at "BABYL OPTIONS:")
	(let ((old-file (make-temp-file "pmail"))
	      (new-file (make-temp-file "pmail")))
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
      (pmail-mode-2)
      ;; Convert all or parts of file to a format Pmail understands
      (pmail-convert-file)
      ;;  We use `run-mail-hook' to remember whether we should run
      ;; `pmail-mode-hook' at the end.
      (setq run-mail-hook t)
      ;; Initialize the Pmail state.
      (pmail-initialize-messages))
    ;; Now we're back in business.  The happens even if we had a
    ;; perfectly fine file.
    (unwind-protect
	(unless (and (not file-name-arg) (pmail-get-new-mail))
	  (pmail-show-message (pmail-first-unseen-message)))
      (when pmail-display-summary
	(pmail-summary))
      (pmail-construct-io-menu)
      ;; Run any callbacks if the buffer was not in pmail-mode
      (when run-mail-hook
	(run-hooks 'pmail-mode-hook)))))

(defun pmail-convert-file ()
  "Convert unconverted messages.
A message is unconverted if it doesn't have the BABYL header
specified in `pmail-header-attribute-header'; it is converted
using `pmail-convert-mbox-format'."
  (let ((convert
	 (save-restriction
	   (widen)
	   (let ((case-fold-search nil)
		 (start (point-max))
		 end)
	     (catch 'convert
	       (goto-char start)
	       (while (re-search-backward
		       pmail-unix-mail-delimiter nil t)
		 (setq end start)
		 (setq start (point))
		 (save-excursion
		   (save-restriction
		     (narrow-to-region start end)
		     (goto-char start)
		     (let ((attribute (pmail-header-get-header
				       pmail-header-attribute-header)))
		       (unless attribute
			 (throw 'convert t)))))))))))
    (if convert
	(let ((inhibit-read-only t))
	  (pmail-convert-mbox-format)))))

(defun pmail-initialize-messages ()
  "Initialize message state based on messages in the buffer."
  (setq pmail-total-messages 0
        pmail-current-message 1)
  (pmail-desc-clear-descriptors)
  (widen)
  (pmail-header-show-headers)
  (setq pmail-total-messages (pmail-process-new-messages)))

(defvar pmail-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "a"      'pmail-add-label)
    (define-key map "b"      'pmail-bury)
    (define-key map "c"      'pmail-continue)
    (define-key map "d"      'pmail-delete-forward)
    (define-key map "\C-d"   'pmail-delete-backward)
    (define-key map "e"      'pmail-edit-current-message)
    (define-key map "f"      'pmail-forward)
    (define-key map "g"      'pmail-get-new-mail)
    (define-key map "h"      'pmail-summary)
    (define-key map "i"      'pmail-input)
    (define-key map "j"      'pmail-show-message)
    (define-key map "k"      'pmail-kill-label)
    (define-key map "l"      'pmail-summary-by-labels)
    (define-key map "\e\C-h" 'pmail-summary)
    (define-key map "\e\C-l" 'pmail-summary-by-labels)
    (define-key map "\e\C-r" 'pmail-summary-by-recipients)
    (define-key map "\e\C-s" 'pmail-summary-by-regexp)
    (define-key map "\e\C-t" 'pmail-summary-by-topic)
    (define-key map "m"      'pmail-mail)
    (define-key map "\em"    'pmail-retry-failure)
    (define-key map "n"      'pmail-next-undeleted-message)
    (define-key map "\en"    'pmail-next-message)
    (define-key map "\e\C-n" 'pmail-next-labeled-message)
    (define-key map "o"      'pmail-output)
    (define-key map "\C-o"   'pmail-output)
    (define-key map "p"      'pmail-previous-undeleted-message)
    (define-key map "\ep"    'pmail-previous-message)
    (define-key map "\e\C-p" 'pmail-previous-labeled-message)
    (define-key map "q"      'pmail-quit)
    (define-key map "r"      'pmail-reply)
    ;; I find I can't live without the default M-r command -- rms.
    ;;  (define-key map "\er"  'pmail-search-backwards)
    (define-key map "s"      'pmail-expunge-and-save)
    (define-key map "\es"    'pmail-search)
    (define-key map "t"      'pmail-toggle-header)
    (define-key map "u"      'pmail-undelete-previous-message)
    (define-key map "w"      'pmail-output-body-to-file)
    (define-key map "x"      'pmail-expunge)
    (define-key map "."      'pmail-beginning-of-message)
    (define-key map "/"      'pmail-end-of-message)
    (define-key map "<"      'pmail-first-message)
    (define-key map ">"      'pmail-last-message)
    (define-key map " "      'scroll-up)
    (define-key map "\177"   'scroll-down)
    (define-key map "?"      'describe-mode)
    (define-key map "\C-c\C-s\C-d" 'pmail-sort-by-date)
    (define-key map "\C-c\C-s\C-s" 'pmail-sort-by-subject)
    (define-key map "\C-c\C-s\C-a" 'pmail-sort-by-author)
    (define-key map "\C-c\C-s\C-r" 'pmail-sort-by-recipient)
    (define-key map "\C-c\C-s\C-c" 'pmail-sort-by-correspondent)
    (define-key map "\C-c\C-s\C-l" 'pmail-sort-by-lines)
    (define-key map "\C-c\C-s\C-k" 'pmail-sort-by-labels)
    (define-key map "\C-c\C-n" 'pmail-next-same-subject)
    (define-key map "\C-c\C-p" 'pmail-previous-same-subject)
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar classify]
      (cons "Classify" (make-sparse-keymap "Classify")))
    (define-key map [menu-bar classify input-menu]
      nil)
    (define-key map [menu-bar classify output-menu]
      nil)
    (define-key map [menu-bar classify output-body]
      '("Output body to file..." . pmail-output-body-to-file))
    (define-key map [menu-bar classify output-inbox]
      '("Output (inbox)..." . pmail-output))
    (define-key map [menu-bar classify output]
      '("Output (Pmail)..." . pmail-output))
    (define-key map [menu-bar classify kill-label]
      '("Kill Label..." . pmail-kill-label))
    (define-key map [menu-bar classify add-label]
      '("Add Label..." . pmail-add-label))
    (define-key map [menu-bar summary]
      (cons "Summary" (make-sparse-keymap "Summary")))
    (define-key map [menu-bar summary senders]
      '("By Senders..." . pmail-summary-by-senders))
    (define-key map [menu-bar summary labels]
      '("By Labels..." . pmail-summary-by-labels))
    (define-key map [menu-bar summary recipients]
      '("By Recipients..." . pmail-summary-by-recipients))
    (define-key map [menu-bar summary topic]
      '("By Topic..." . pmail-summary-by-topic))
    (define-key map [menu-bar summary regexp]
      '("By Regexp..." . pmail-summary-by-regexp))
    (define-key map [menu-bar summary all]
      '("All" . pmail-summary))
    (define-key map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))
    (define-key map [menu-bar mail pmail-get-new-mail]
      '("Get New Mail" . pmail-get-new-mail))
    (define-key map [menu-bar mail lambda]
      '("----"))
    (define-key map [menu-bar mail continue]
      '("Continue" . pmail-continue))
    (define-key map [menu-bar mail resend]
      '("Re-send..." . pmail-resend))
    (define-key map [menu-bar mail forward]
      '("Forward" . pmail-forward))
    (define-key map [menu-bar mail retry]
      '("Retry" . pmail-retry-failure))
    (define-key map [menu-bar mail reply]
      '("Reply" . pmail-reply))
    (define-key map [menu-bar mail mail]
      '("Mail" . pmail-mail))
    (define-key map [menu-bar delete]
      (cons "Delete" (make-sparse-keymap "Delete")))
    (define-key map [menu-bar delete expunge/save]
      '("Expunge/Save" . pmail-expunge-and-save))
    (define-key map [menu-bar delete expunge]
      '("Expunge" . pmail-expunge))
    (define-key map [menu-bar delete undelete]
      '("Undelete" . pmail-undelete-previous-message))
    (define-key map [menu-bar delete delete]
      '("Delete" . pmail-delete-forward))
    (define-key map [menu-bar move]
      (cons "Move" (make-sparse-keymap "Move")))
    (define-key map [menu-bar move search-back]
      '("Search Back..." . pmail-search-backwards))
    (define-key map [menu-bar move search]
      '("Search..." . pmail-search))
    (define-key map [menu-bar move previous]
      '("Previous Nondeleted" . pmail-previous-undeleted-message))
    (define-key map [menu-bar move next]
      '("Next Nondeleted" . pmail-next-undeleted-message))
    (define-key map [menu-bar move last]
      '("Last" . pmail-last-message))
    (define-key map [menu-bar move first]
      '("First" . pmail-first-message))
    (define-key map [menu-bar move previous]
      '("Previous" . pmail-previous-message))
    (define-key map [menu-bar move next]
      '("Next" . pmail-next-message))
    map)
  "Keymap for `pmail-mode'.")

;; Pmail toolbar
(defvar pmail-tool-bar-map
  (if (display-graphic-p)
      (let ((map (make-sparse-keymap)))
	(tool-bar-local-item-from-menu 'pmail-get-new-mail "mail/inbox"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-next-undeleted-message "right-arrow"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-previous-undeleted-message "left-arrow"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-search "search"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-input "open"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-mail "mail/compose"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-reply "mail/reply-all"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-forward "mail/forward"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-delete-forward "close"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-output "mail/move"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-output-body-to-file "mail/save"
				       map pmail-mode-map)
	(tool-bar-local-item-from-menu 'pmail-expunge "delete"
				       map pmail-mode-map)
	map)))



;; Pmail mode is suitable only for specially formatted data.
(put 'pmail-mode 'mode-class 'special)

(defun pmail-mode-kill-summary ()
  (if pmail-summary-buffer (kill-buffer pmail-summary-buffer)))

;;;###autoload
(defun pmail-mode ()
  "Pmail Mode is used by \\<pmail-mode-map>\\[pmail] for editing Pmail files.
All normal editing commands are turned off.
Instead, these commands are available:

\\[pmail-beginning-of-message]	Move point to front of this message.
\\[pmail-end-of-message]	Move point to bottom of this message.
\\[scroll-up]	Scroll to next screen of this message.
\\[scroll-down]	Scroll to previous screen of this message.
\\[pmail-next-undeleted-message]	Move to Next non-deleted message.
\\[pmail-previous-undeleted-message]	Move to Previous non-deleted message.
\\[pmail-next-message]	Move to Next message whether deleted or not.
\\[pmail-previous-message]	Move to Previous message whether deleted or not.
\\[pmail-first-message]	Move to the first message in Pmail file.
\\[pmail-last-message]	Move to the last message in Pmail file.
\\[pmail-show-message]	Jump to message specified by numeric position in file.
\\[pmail-search]	Search for string and show message it is found in.
\\[pmail-delete-forward]	Delete this message, move to next nondeleted.
\\[pmail-delete-backward]	Delete this message, move to previous nondeleted.
\\[pmail-undelete-previous-message]	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
\\[pmail-edit-current-message]	Edit the current message.  \\[pmail-cease-edit] to return to Pmail.
\\[pmail-expunge]	Expunge deleted messages.
\\[pmail-expunge-and-save]	Expunge and save the file.
\\[pmail-quit]       Quit Pmail: expunge, save, then switch to another buffer.
\\[save-buffer] Save without expunging.
\\[pmail-get-new-mail]	Move new mail from system spool directory into this file.
\\[pmail-mail]	Mail a message (same as \\[mail-other-window]).
\\[pmail-continue]	Continue composing outgoing message started before.
\\[pmail-reply]	Reply to this message.  Like \\[pmail-mail] but initializes some fields.
\\[pmail-retry-failure]	Send this message again.  Used on a mailer failure message.
\\[pmail-forward]	Forward this message to another user.
\\[pmail-output-to-pmail-file]       Output this message to an Pmail file (append it).
\\[pmail-output]	Output this message to a Unix-format mail file (append it).
\\[pmail-output-body-to-file]	Save message body to a file.  Default filename comes from Subject line.
\\[pmail-input]	Input Pmail file.  Run Pmail on that file.
\\[pmail-add-label]	Add label to message.  It will be displayed in the mode line.
\\[pmail-kill-label]	Kill label.  Remove a label from current message.
\\[pmail-next-labeled-message]   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with \\[pmail-add-label].
\\[pmail-previous-labeled-message]   Move to Previous message with specified label
\\[pmail-summary]	Show headers buffer, with a one line summary of each message.
\\[pmail-summary-by-labels]	Summarize only messages with particular label(s).
\\[pmail-summary-by-recipients]   Summarize only messages with particular recipient(s).
\\[pmail-summary-by-regexp]   Summarize only messages with particular regexp(s).
\\[pmail-summary-by-topic]   Summarize only messages with subject line regexp(s).
\\[pmail-toggle-header]	Toggle display of complete header."
  (interactive)
  (let ((finding-pmail-file (not (eq major-mode 'pmail-mode))))
    (pmail-mode-2)
    (when (and finding-pmail-file
    	       (null coding-system-for-read)
    	       default-enable-multibyte-characters)
      (let ((pmail-enable-multibyte t))
    	(pmail-require-mime-maybe)
    	(goto-char (point-max))
    	(set-buffer-multibyte t)))
    (pmail-show-message pmail-total-messages)
    (when finding-pmail-file
      (when pmail-display-summary
	(pmail-summary))
      (pmail-construct-io-menu))
    (run-mode-hooks 'pmail-mode-hook)))

(defun pmail-mode-2 ()
  (kill-all-local-variables)
  (pmail-mode-1)
  (pmail-perm-variables)
  (pmail-variables))

(defun pmail-mode-1 ()
  (setq major-mode 'pmail-mode)
  (setq mode-name "PMAIL")
  (setq buffer-read-only t)
  ;; No need to auto save PMAIL files in normal circumstances
  ;; because they contain no info except attribute changes
  ;; and deletion of messages.
  ;; The one exception is when messages are copied into an Pmail mode buffer.
  ;; pmail-output-to-pmail-file enables auto save when you do that.
  (setq buffer-auto-save-file-name nil)
  (setq mode-line-modified "--")
  (use-local-map pmail-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table))

;; Set up the permanent locals associated with an Pmail file.
(defun pmail-perm-variables ()
  (make-local-variable 'pmail-last-label)
  (make-local-variable 'pmail-last-regexp)
  (make-local-variable 'pmail-buffer)
  (setq pmail-buffer (current-buffer))
  (make-local-variable 'pmail-view-buffer)
  (setq pmail-view-buffer pmail-buffer)
  (make-local-variable 'pmail-summary-buffer)
  (make-local-variable 'pmail-current-message)
  (make-local-variable 'pmail-total-messages)
  (make-local-variable 'pmail-overlay-list)
  (setq pmail-overlay-list nil)
  (make-local-variable 'pmail-desc-vector)
  (make-local-variable 'pmail-inbox-list)
  (setq pmail-inbox-list (pmail-get-file-inbox-list))
  ;; Provide default set of inboxes for primary mail file ~/PMAIL.
  (and (null pmail-inbox-list)
       (or (equal buffer-file-name (expand-file-name pmail-file-name))
	   (equal buffer-file-truename
		  (abbreviate-file-name (file-truename pmail-file-name))))
       (setq pmail-inbox-list
	     (or pmail-primary-inbox-list
		 (list (or (getenv "MAIL")
			   (concat rmail-spool-directory
				   (user-login-name)))))))
  (make-local-variable 'pmail-keywords)
  (set (make-local-variable 'tool-bar-map) pmail-tool-bar-map)
  ;; this gets generated as needed
  (setq pmail-keywords nil))

;; Set up the non-permanent locals associated with Pmail mode.
(defun pmail-variables ()
  ;; Don't let a local variables list in a message cause confusion.
  (make-local-variable 'local-enable-local-variables)
  (setq local-enable-local-variables nil)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'pmail-revert)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(pmail-font-lock-keywords
	  t t nil nil
	  (font-lock-maximum-size . nil)
	  (font-lock-fontify-buffer-function . pmail-fontify-buffer-function)
	  (font-lock-unfontify-buffer-function . pmail-unfontify-buffer-function)
	  (font-lock-inhibit-thing-lock . (lazy-lock-mode fast-lock-mode))))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'pmail-mode-kill-summary)
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag t)
  (make-local-variable 'desktop-save-buffer)
  (setq desktop-save-buffer t))

;; Handle M-x revert-buffer done in an pmail-mode buffer.
(defun pmail-revert (arg noconfirm)
  (with-current-buffer pmail-buffer
    (let* ((revert-buffer-function (default-value 'revert-buffer-function))
	   (pmail-enable-multibyte enable-multibyte-characters))
      ;; Call our caller again, but this time it does the default thing.
      (when (revert-buffer arg noconfirm)
	;; If the user said "yes", and we changed something, reparse the
	;; messages.
	(with-current-buffer pmail-buffer
	  (pmail-mode-2)
	  (pmail-convert-file)
	  ;; We have read the file as raw-text, so the buffer is set to
	  ;; unibyte.  Make it multibyte if necessary.
	  (when (and pmail-enable-multibyte
		     (not enable-multibyte-characters))
	    (set-buffer-multibyte t))
	  (pmail-initialize-messages)
	  (pmail-show-message pmail-total-messages)
	  (run-hooks 'pmail-mode-hook))))))

(defun pmail-get-file-inbox-list ()
  "Return a list of inbox files for this buffer."
  (let* ((filename (expand-file-name (buffer-file-name)))
	 (inboxes (cdr (or (assoc filename pmail-inbox-alist)
			   (assoc (abbreviate-file-name filename)
				  pmail-inbox-alist))))
	 (list nil))
    (dolist (i inboxes)
      (when (file-name-absolute-p i)
	(push (expand-file-name i) list)))
    (nreverse list)))

;;; mbox: ready
(defun pmail-expunge-and-save ()
  "Expunge and save PMAIL file."
  (interactive)
  (pmail-expunge)
  (save-buffer)
  (pmail-display-summary-maybe))

;;; mbox: ready
(defun pmail-display-summary-maybe ()
  "If a summary buffer exists then make sure it is updated and displayed."
  (if (pmail-summary-exists)
      (let ((current-message pmail-current-message))
        (pmail-select-summary
         (pmail-summary-goto-msg current-message)
         (pmail-summary-pmail-update)
         (set-buffer-modified-p nil)))))

;;; mbox: ready
(defun pmail-quit ()
  "Quit out of PMAIL.
Hook `pmail-quit-hook' is run after expunging."
  (interactive)
  (pmail-expunge-and-save)
  (when (boundp 'pmail-quit-hook)
    (run-hooks 'pmail-quit-hook))
  ;; Don't switch to the summary buffer even if it was recently visible.
  (when pmail-summary-buffer
    (replace-buffer-in-windows pmail-summary-buffer)
    (bury-buffer pmail-summary-buffer))
  (if pmail-enable-mime
      (let ((obuf pmail-buffer)
	    (ovbuf pmail-view-buffer))
	(set-buffer pmail-view-buffer)
	(quit-window)
	(replace-buffer-in-windows ovbuf)
	(replace-buffer-in-windows obuf)
	(bury-buffer obuf))
    (let ((obuf (current-buffer)))
      (quit-window)
      (replace-buffer-in-windows obuf))))

;;; mbox: ready
(defun pmail-bury ()
  "Bury current Pmail buffer and its summary buffer."
  (interactive)
  ;; This let var was called pmail-buffer, but that interfered
  ;; with the buffer-local var used in summary buffers.
  (let ((buffer-to-bury (current-buffer)))
    (if (pmail-summary-exists)
	(let (window)
	  (while (setq window (get-buffer-window pmail-summary-buffer))
	    (quit-window nil window))
	  (bury-buffer pmail-summary-buffer)))
    (quit-window)))

;;;??? Fails to add descriptor for new message.
;;; mbox: ready
(defun pmail-duplicate-message ()
  "Create a duplicated copy of the current message.
The duplicate copy goes into the Pmail file just after the
original copy."
  (interactive)
  (widen)
  (let ((buffer-read-only nil)
	(number pmail-current-message)
	(string (buffer-substring (pmail-desc-get-start pmail-current-message)
				  (pmail-desc-get-end pmail-current-message))))
    (goto-char (pmail-desc-get-end pmail-current-message))
    (insert string)
    (pmail-show-message number)
    (message "Message duplicated")))

;;;###autoload
(defun pmail-input (filename)
  "Run Pmail on file FILENAME."
  (interactive "FRun pmail on PMAIL file: ")
  (pmail filename))

;; This used to scan subdirectories recursively, but someone pointed out
;; that if the user wants that, person can put all the files in one dir.
;; And the recursive scan was slow.  So I took it out.  rms, Sep 1996.
(defun pmail-find-all-files (start)
  "Return list of file in dir START that match `pmail-secondary-file-regexp'."
  (if (file-accessible-directory-p start)
      ;; Don't sort here.
      (let* ((case-fold-search t)
	     (files (directory-files start t pmail-secondary-file-regexp)))
	;; Sort here instead of in directory-files
	;; because this list is usually much shorter.
	(sort files 'string<))))

(defun pmail-list-to-menu (menu-name l action &optional full-name)
  (let ((menu (make-sparse-keymap menu-name)))
    (mapc
     (function (lambda (item)
		 (let (command)
		   (if (consp item)
		       (progn
			 (setq command
			       (pmail-list-to-menu (car item) (cdr item)
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
					  pmail-secondary-file-directory))))))
		   (define-key menu (vector (intern name))
		     (cons name command)))))
     (reverse l))
    menu))

;; This command is always "disabled" when it appears in a menu.
(put 'pmail-disable-menu 'menu-enable ''nil)

(defun pmail-construct-io-menu ()
  (let ((files (pmail-find-all-files pmail-secondary-file-directory)))
    (if files
	(progn
	  (define-key pmail-mode-map [menu-bar classify input-menu]
	    (cons "Input Pmail File"
		  (pmail-list-to-menu "Input Pmail File"
				      files
				      'pmail-input)))
	  (define-key pmail-mode-map [menu-bar classify output-menu]
	    (cons "Output Pmail File"
		  (pmail-list-to-menu "Output Pmail File"
				      files
				      'pmail-output))))

      (define-key pmail-mode-map [menu-bar classify input-menu]
	'("Input Pmail File" . pmail-disable-menu))
      (define-key pmail-mode-map [menu-bar classify output-menu]
	'("Output Pmail File" . pmail-disable-menu)))))


;;;; *** Pmail input ***

(declare-function pmail-spam-filter "pmail-spam-filter" (msg))
(declare-function pmail-summary-goto-msg "pmailsum" (&optional n nowarn skip-pmail))
(declare-function pmail-summary-mark-undeleted "pmailsum" (n))
(declare-function pmail-summary-mark-deleted "pmailsum" (&optional n undel))
(declare-function rfc822-addresses "rfc822" (header-text))
(declare-function mail-abbrev-make-syntax-table "mailabbrev.el" ())
(declare-function mail-sendmail-delimit-header "sendmail" ())
(declare-function mail-header-end "sendmail" ())

(defun pmail-get-inbox-files ()
  "Return all files from `pmail-inbox-list' without name conflicts.
A conflict happens when two inbox file names have the same name
according to `file-name-nondirectory'."
  (let (files last-names)
    (catch 'conflict
      (dolist (file pmail-inbox-list)
	(if (member (file-name-nondirectory file) last-names)
	    (throw 'conflict t)
	  (push file files))
	(push (file-name-nondirectory file) last-names)))
    (nreverse files)))

(defun pmail-delete-inbox-files (files)
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

(defun pmail-get-new-mail (&optional file-name)
  "Move any new mail from this mail file's inbox files.
The inbox files for the primary mail file are determined using
various means when setting up the buffer.  The list of inbox
files are stored in `pmail-inbox-list'.

The most important variable that determines the value of this
list is `pmail-inbox-alist' which lists the inbox files for any
mail files you might be using.

If the above yields no inbox files, and if this is the primary
mail file as determined by `pmail-file-name', the inbox lists
otherwise defaults to `pmail-primary-inbox-list' if set, or the
environment variable MAIL if set, or the user's mail file in
`rmail-spool-directory'.

This is why, by default, no mail file has inbox files, except for
the primary mail file ~/PMAIL, which gets its new mail from the
mail spool.

You can also specify the file to get new mail from interactively.
A prefix argument will read a file name and use that file as the
inbox.  Noninteractively, you can pass the inbox file name as an
argument.

If the variable `pmail-preserve-inbox' is non-nil, new mail will
always be left in inbox files rather than deleted.

This function runs `pmail-get-new-mail-hook' before saving the
updated file.  It returns t if it got any new messages."
  (interactive
   (list (when current-prefix-arg
	   (read-file-name "Get new mail from file: "))))
  (run-hooks 'pmail-before-get-new-mail-hook)
  ;; If the disk file has been changed from under us, revert to it
  ;; before we get new mail.
  (unless (verify-visited-file-modtime (current-buffer))
    (find-file (buffer-file-name)))
  (with-current-buffer pmail-buffer
    (widen)
    ;; Get rid of all undo records for this buffer.
    (unless (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
    (let ((pmail-enable-multibyte (default-value 'enable-multibyte-characters))
	  ;; If buffer has not changed yet, and has not been saved yet,
	  ;; don't replace the old backup file now.
	  (make-backup-files (and make-backup-files (buffer-modified-p)))
	  current-message found)
      (condition-case nil
	  (let ((buffer-read-only nil)
		(buffer-undo-list t)
		(delete-files nil)
		(new-messages 0)
		(rsf-number-of-spam 0))
	    (save-excursion
	      (save-restriction
		(goto-char (point-max))
		(narrow-to-region (point) (point))
		;; Read in the contents of the inbox files, renaming
		;; them as necessary, and adding to the list of files to
		;; delete eventually.
		(if file-name
		    (pmail-insert-inbox-text (list file-name) nil)
		  (setq delete-files (pmail-insert-inbox-text
				      (pmail-get-inbox-files) t)))
		;; Process newly found messages and save them into the
		;; PMAIL file.
		(unless (equal (point-min) (point-max))
		  (setq new-messages (pmail-convert-mbox-format))
		  (unless (zerop new-messages)
		    (pmail-process-new-messages)
		    (setq pmail-current-message (1+ pmail-total-messages)
			  pmail-total-messages (pmail-desc-get-count)))
		  (save-buffer))
		;; Delete the old files, now that the PMAIL file is
		;; saved.
		(when delete-files
		  (pmail-delete-inbox-files delete-files))))

	    (if (zerop new-messages)
		(when (or file-name pmail-inbox-list)
		  (message "(No new mail has arrived)"))

	      ;; Process the new messages for spam using the integrated
	      ;; spam filter.  The spam filter can mark messages for
	      ;; deletion and can output a message.
	      ;; XXX pmail-spam-filter hasn't been tested at all with
	      ;; the mbox branch. --enberg
	      (setq current-message (pmail-first-unseen-message))
	      (when pmail-use-spam-filter
		(while (<= current-message pmail-total-messages)
		  (pmail-spam-filter current-message)
		  (setq current-message (1+ current-message))))
	      ;; Make the first unseen message the current message and
	      ;; update the summary buffer, if one exists.
	      (setq current-message (pmail-first-unseen-message))
	      (if (pmail-summary-exists)
		  (with-current-buffer pmail-summary-buffer
		    (pmail-update-summary)
		    (pmail-summary-goto-msg current-message))
		(pmail-show-message current-message))
	      ;; Run the after get new mail hook.
	      (run-hooks 'pmail-after-get-new-mail-hook)
	      (message "%d new message%s read"
		       new-messages (if (= 1 new-messages) "" "s"))
	      (setq found t))
	    found)
	;; Don't leave the buffer screwed up if we get a disk-full error.
	(file-error (or found (pmail-show-message)))))))

(defun pmail-parse-url (file)
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
	    (when pmail-remote-password-required
	      (setq got-password (not (pmail-have-password)))
	      (setq supplied-password (pmail-get-remote-password
				       (string-equal proto "imap")))))

	(if (pmail-movemail-variant-p 'emacs)
	    (if (string-equal proto "pop")
		(list (concat "po:" user ":" host)
		      t
		      (or pass supplied-password)
		      got-password)
	      (error "Emacs movemail does not support %s protocol" proto))
	  (list (concat proto "://" user "@" host)
		(or (string-equal proto "pop") (string-equal proto "imap"))
		(or supplied-password pass)
		got-password))))

   ((string-match "^po:\\([^:]+\\)\\(:\\(.*\\)\\)?" file)
    (let (got-password supplied-password
          (proto "pop")
	  (user  (match-string 1 file))
	  (host  (match-string 3 file)))

      (when pmail-remote-password-required
	(setq got-password (not (pmail-have-password)))
	(setq supplied-password (pmail-get-remote-password nil)))

      (list file "pop" supplied-password got-password)))

   (t
    (list file nil nil nil))))

(defun pmail-insert-inbox-text (files renamep)
  ;; Detect a locked file now, so that we avoid moving mail
  ;; out of the real inbox file.  (That could scare people.)
  (or (memq (file-locked-p buffer-file-name) '(nil t))
      (error "PMAIL file %s is locked"
	     (file-name-nondirectory buffer-file-name)))
  (let (file tofile delete-files movemail popmail got-password password)
    (while files
      ;; Handle remote mailbox names specially; don't expand as filenames
      ;; in case the userid contains a directory separator.
      (setq file (car files))
      (let ((url-data (pmail-parse-url file)))
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
			     (if (memq system-type '(windows-nt cygwin ms-dos))
				 ;; cannot have colons in file name
				 (replace-regexp-in-string ":" "-" file)
			       file)))
		    ;; Use the directory of this pmail file
		    ;; because it's a nuisance to use the homedir
		    ;; if that is on a full disk and this pmail
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
			 (list (or pmail-movemail-program "movemail") nil errors nil)
			 (if pmail-preserve-inbox
			     (list "-p")
			   nil)
			 (if (pmail-movemail-variant-p 'mailutils)
			     (append (list "--emacs") pmail-movemail-flags)
			   pmail-movemail-flags)
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
			   (re-search-forward pmail-remote-password-error
					      nil t))
		       (pmail-set-remote-password nil))

		   ;; If using Mailutils, remove initial error code
		   ;; abbreviation
		   (when (pmail-movemail-variant-p 'mailutils)
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
	    (if (not (and pmail-preserve-inbox (string= file tofile)))
		(setq delete-files (cons tofile delete-files)))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;;;; *** Pmail message decoding ***

;; these two are unused, and possibly harmul.

;; (defun pmail-decode-region (from to coding)
;;   "Decode the region specified by FROM and TO by CODING.
;; If CODING is nil or an invalid coding system, decode by `undecided'."
;;   (unless (and coding (coding-system-p coding))
;;     (setq coding 'undecided))
;;   ;; Use -dos decoding, to remove ^M characters left from base64 or
;;   ;; rogue qp-encoded text.
;;   (decode-coding-region from to
;; 			(coding-system-change-eol-conversion
;; 			 coding 'dos))
;;   ;; Don't reveal the fact we used -dos decoding, as users generally
;;   ;; will not expect the PMAIL buffer to use DOS EOL format.
;;   (setq buffer-file-coding-system
;; 	(setq last-coding-system-used
;; 	      (coding-system-change-eol-conversion
;; 	       coding 'unix))))

;; (defun pmail-decode-by-content-type (from to)
;;   "Decode message between FROM and TO according to Content-Type."
;;   (when (and (not pmail-enable-mime) pmail-enable-multibyte)
;;     (let ((coding-system-used nil)
;; 	  (case-fold-search t))
;;       (save-restriction
;; 	(narrow-to-region from to)
;; 	(when (and (not pmail-enable-mime) pmail-enable-multibyte)
;; 	  (let ((coding
;; 		 (when (save-excursion
;; 			 (goto-char (pmail-header-get-limit))
;; 			 (re-search-backward
;; 			  pmail-mime-charset-pattern
;; 			  (point-min) t))
;; 		   (intern (downcase (match-string 1))))))
;; 	    (setq coding-system-used (pmail-decode-region
;; 				      (point-min) (point-max)
;; 				      coding)))))
;;       (setq last-coding-system-used coding-system-used))))

;;;; *** Pmail Message Formatting and Header Manipulation ***

(defun pmail-clear-headers (&optional ignored-headers)
  "Delete all header fields that Pmail should not show.
If the optional argument IGNORED-HEADERS is non-nil,
delete all header fields whose names match that regexp.
Otherwise, if `pmail-displayed-headers' is non-nil,
delete all header fields *except* those whose names match that regexp.
Otherwise, delete all header fields whose names match `pmail-ignored-headers'
unless they also match `pmail-nonignored-headers'."
  (when (search-forward "\n\n" nil t)
    (forward-char -1)
    (let ((case-fold-search t)
	  (buffer-read-only nil))
      (if (and pmail-displayed-headers (null ignored-headers))
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
		      (re-search-forward pmail-displayed-headers lim t))
		  (goto-char next)
		  (delete-region (point) next))))
	    (goto-char (point-min)))
	(or ignored-headers (setq ignored-headers pmail-ignored-headers))
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (goto-char (point-min))
	  (while (and ignored-headers
		      (re-search-forward ignored-headers nil t))
	    (beginning-of-line)
	    (if (and pmail-nonignored-headers
		     (looking-at pmail-nonignored-headers))
		(forward-line 1)
	      (delete-region (point)
			     (save-excursion
			       (if (re-search-forward "\n[^ \t]" nil t)
				   (1- (point))
				 (point-max)))))))))))

(defun pmail-msg-is-pruned (&optional msg)
  "Determine if the headers for the current message are being
  displayed. If MSG is non-nil it will be used as the message number
  instead of the current message."
  (pmail-desc-get-header-display-state (or msg pmail-current-message)))

(defun pmail-toggle-header (&optional arg)
  "Show original message header if pruned header currently shown, or vice versa.
With argument ARG, show the message header pruned if ARG is greater than zero;
otherwise, show it in full."
  (interactive "P")
  (pmail-header-toggle-visibility arg))

;; Lifted from repos-count-screen-lines.
(defun pmail-count-screen-lines (start end)
  "Return number of screen lines between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (vertical-motion (- (point-max) (point-min))))))

;;;; *** Pmail Attributes and Keywords ***

;; Make a string describing the current message's attributes by
;; keywords and set it up as the name of a minor mode so it will
;; appear in the mode line.
(defun pmail-display-labels ()
  (let (keyword-list result)
    ;; Update the keyword list for the current message.
    (if (> pmail-current-message 0)
        (setq keyword-list (pmail-desc-get-keywords pmail-current-message)))
    ;; Generate the result string.
    (setq result (mapconcat 'identity keyword-list " "))
    ;; Update the mode line to display the keywords, the current
    ;; message index and the total number of messages.
    (setq mode-line-process
	  (format " %d/%d%s"
		  pmail-current-message pmail-total-messages
		  (if keyword-list (concat " " result) "")))
    ;; If pmail-enable-mime is non-nil, we may have to update
    ;; `mode-line-process' of pmail-view-buffer too.
    (if (and pmail-enable-mime
	     (not (eq (current-buffer) pmail-view-buffer))
	     (buffer-live-p pmail-view-buffer))
	(let ((mlp mode-line-process))
	  (with-current-buffer pmail-view-buffer
	    (setq mode-line-process mlp))))))

(defun pmail-set-attribute (attr state &optional msgnum)
  "Turn a attribute ATTR of a message on or off according to STATE.
ATTR is a string, MSGNUM is the optional message number.  By
default, the current message is changed."
  (save-excursion
    (save-restriction
      (let ((attr-index (pmail-desc-get-attr-index attr)))
	(set-buffer pmail-buffer)
	(or msgnum (setq msgnum pmail-current-message))
	(pmail-desc-set-attribute attr-index state msgnum)
        ;; Deal with the summary buffer.
        (when pmail-summary-buffer
	  (pmail-summary-update msgnum))))))

(defun pmail-message-labels-p (n labels)
  "Return t if message number N has keywords matching LABELS.
LABELS is a regular expression."
  (catch 'found
    (dolist (keyword (pmail-desc-get-keywords n))
      (when (string-match labels keyword)
	(throw 'found t)))))


;;;; *** Pmail Message Selection And Support ***

(defun pmail-msgbeg (n)
  (pmail-desc-get-start n))
(make-obsolete 'pmail-msgbeg 'pmail-desc-get-start "22.0")

(defun pmail-msgend (n)
  (pmail-desc-get-end n))
(make-obsolete 'pmail-msgend 'pmail-desc-get-end "22.0")

(defun pmail-widen-to-current-msgbeg (function)
  "Call FUNCTION with point at start of internal data of current message.
Assumes that bounds were previously narrowed to display the message in Pmail.
The bounds are widened enough to move point where desired, then narrowed
again afterward.

FUNCTION may not change the visible text of the message, but it may
change the invisible header text."
  (save-excursion
    (unwind-protect
	(progn
	  (narrow-to-region (pmail-desc-get-start pmail-current-message)
			    (point-max))
	  (goto-char (point-min))
	  (funcall function))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
      (narrow-to-region (pmail-desc-get-start pmail-current-message)
			(pmail-desc-get-end pmail-current-message)))))

(defun pmail-process-new-messages (&optional nomsg)
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
      (while (re-search-backward pmail-unix-mail-delimiter nil t)
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
	    (setq keywords (pmail-header-get-keywords))
	    (when keywords
	      ;; Keywords do exist.  Register them with the keyword
	      ;; management library.
	      (pmail-register-keywords keywords))
	    ;; Insure that we have From and Date headers.
	    ;;(pmail-decode-from-line)
	    ;; Perform User defined filtering.
	    (save-excursion
	      (if pmail-message-filter (funcall pmail-message-filter)))
	    ;; Accumulate the message attributes along with the message
	    ;; markers and the message date list.
	    (setq message-descriptor-list
		  (vconcat (list (list (point-min-marker)
				       (pmail-header-get-header
					pmail-header-attribute-header)
				       keywords
				       date
				       (count-lines start end)
				       (cadr (mail-extract-address-components; does not like nil
					      (or (pmail-header-get-header "from") "")))
				       (or (pmail-header-get-header "subject")
					   "none")))
			   message-descriptor-list)))))
      ;; Add the new message data lists to the Pmail message descriptor
      ;; vector.
      (pmail-desc-add-descriptors message-descriptor-list)
      ;; Unless requested otherwise, show the number of new messages.
      ;; Return the number of new messages.
      (or nomsg (message "Processing new messages...done (%d)"
			 new-message-counter))
      new-message-counter)))

(defun pmail-convert-mbox-format ()
  (let ((case-fold-search nil)
	(message-count 0)
	(start (point-max))
	end)
    (save-excursion
      (goto-char start)
      (while (re-search-backward pmail-unix-mail-delimiter nil t)
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
	    ;; Make sure we have an Pmail BABYL attribute header field.
	    ;; All we can assume is that the Pmail BABYL header field is
	    ;; in the header section.  It's placement can be modified by
	    ;; another mailer.
	    (let ((attributes (pmail-header-get-header
			       pmail-header-attribute-header)))
	      (unless attributes
		;; No suitable header exists.  Append the default BABYL
		;; data header for a new message.
		(pmail-header-add-header pmail-header-attribute-header
					 pmail-desc-default-attrs))))))
      message-count)))

(defun pmail-beginning-of-message ()
  "Show current message starting from the beginning."
  (interactive)
  (let ((pmail-show-message-hook
	 (list (function (lambda ()
			   (goto-char (point-min)))))))
    (pmail-show-message pmail-current-message)))

(defun pmail-end-of-message ()
  "Show bottom of current message."
  (interactive)
  (let ((pmail-show-message-hook
	 (list (function (lambda ()
			   (goto-char (point-max))
			   (recenter (1- (window-height))))))))
    (pmail-show-message pmail-current-message)))

(defun pmail-unknown-mail-followup-to ()
  "Handle a \"Mail-Followup-To\" header field with an unknown mailing list.
Ask the user whether to add that list name to `mail-mailing-lists'."
  (save-restriction
    (let ((mail-followup-to (pmail-header-get-header "mail-followup-to" nil t)))
      (when mail-followup-to
	(let ((addresses
	       (split-string
		(mail-strip-quoted-names mail-followup-to)
		",[[:space:]]+" t)))
	  (dolist (addr addresses)
	    (when (and (not (member addr mail-mailing-lists))
		       (and pmail-user-mail-address-regexp
			    (not (string-match pmail-user-mail-address-regexp
					       addr)))
		       (y-or-n-p
			(format "Add `%s' to `mail-mailing-lists'? "
				addr)))
	      (customize-save-variable 'mail-mailing-lists
				       (cons addr mail-mailing-lists)))))))))

(defun pmail-show-message (&optional n no-summary)
  "Show message number N (prefix argument), counting from start of file.
If NO-SUMMARY is non-nil, then do not update the summary buffer."
  (interactive "p")
  (unless (eq major-mode 'pmail-mode)
    (switch-to-buffer pmail-buffer))
  (if (zerop pmail-total-messages)
      (progn
        (message "No messages to show.  Add something better soon.")
        (force-mode-line-update))
    (let (blurb)
      ;; Set n to the first sane message based on the sign of n:
      ;; positive but greater than the total number of messages -> n;
      ;; negative -> 1.
      (if (not n)
	  (setq n pmail-current-message)
	(cond ((<= n 0)
	       (setq n 1
		     pmail-current-message 1
		     blurb "No previous message"))
	      ((> n pmail-total-messages)
	       (setq n pmail-total-messages
		     pmail-current-message pmail-total-messages
		     blurb "No following message"))
	      (t
	       (setq pmail-current-message n))))
      (let ((beg (pmail-desc-get-start n))
	    (end (pmail-desc-get-end n)))
        (pmail-header-show-headers)
        (widen)
	(narrow-to-region beg end)
        (goto-char (point-min))
        ;; Clear the "unseen" attribute when we show a message, unless
	;; it is already cleared.
	(when (pmail-desc-attr-p pmail-desc-unseen-index n)
	  (pmail-desc-set-attribute pmail-desc-unseen-index nil n))
	(pmail-display-labels)
	;; Deal with MIME
	(if (eq pmail-enable-mime t)
	    (funcall pmail-show-mime-function)
	  (setq pmail-view-buffer pmail-buffer))
	(when mail-mailing-lists
	  (pmail-unknown-mail-followup-to))
	(pmail-header-hide-headers)
	(when transient-mark-mode (deactivate-mark))
        ;; Make sure that point in the Pmail window is at the beginning
        ;; of the buffer.
	(goto-char (point-min))
        (set-window-point (get-buffer-window pmail-buffer) (point))
	;; Run any User code.
	(run-hooks 'pmail-show-message-hook)
	;; If there is a summary buffer, try to move to this message in
	;; that buffer.  But don't complain if this message is not
	;; mentioned in the summary.  Don't do this at all if we were
	;; called on behalf of cursor motion in the summary buffer.
	(when (and (pmail-summary-exists) (not no-summary))
	    (let ((curr-msg pmail-current-message))
	      ;; Set the summary current message, disabling the Pmail
	      ;; buffer update.
	      (with-current-buffer pmail-summary-buffer
		(pmail-summary-goto-msg curr-msg nil t))))
	(with-current-buffer pmail-buffer
	  (pmail-auto-file))
        ;; Post back any status messages.
	(when blurb
	  (message blurb))))))

(defun pmail-redecode-body (coding)
  "Decode the body of the current message using coding system CODING.
This is useful with mail messages that have malformed or missing
charset= headers.

This function assumes that the current message is already decoded
and displayed in the PMAIL buffer, but the coding system used to
decode it was incorrect.  It then encodes the message back to its
original form, and decodes it again, using the coding system CODING.

Note that if Emacs erroneously auto-detected one of the iso-2022
encodings in the message, this function might fail because the escape
sequences that switch between character sets and also single-shift and
locking-shift codes are impossible to recover.  This function is meant
to be used to fix messages encoded with 8-bit encodings, such as
iso-8859, koi8-r, etc."
  (interactive "zCoding system for re-decoding this message: ")
  (unless pmail-enable-mime
    (with-current-buffer pmail-buffer
      (save-excursion
	(let ((start (pmail-desc-get-start pmail-current-message))
	      (end (pmail-desc-get-end pmail-current-message))
	      header)
	  (narrow-to-region start end)
	  (setq header (pmail-header-get-header "X-Coding-System"))
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
		(pmail-header-add-header
		 "X-Coding-System"
		 (symbol-name last-coding-system-used))
		(pmail-show-message pmail-current-message))
	    (error "No X-Coding-System header found")))))))

;; FIXME: Double-check this
(defun pmail-auto-file ()
  "Automatically move a message into a sub-folder based on criteria.
Called when a new message is displayed."
  (if (or (member "filed" (pmail-desc-get-keywords pmail-current-message))
	  (not (string= (buffer-file-name)
			(expand-file-name pmail-file-name))))
      ;; Do nothing if it's already been filed.
      nil
    ;; Find out some basics (common fields)
    (let ((from (mail-fetch-field "from"))
	  (subj (mail-fetch-field "subject"))
	  (to   (concat (mail-fetch-field "to") "," (mail-fetch-field "cc")))
	  (directives pmail-automatic-folder-directives)
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
		(pmail-delete-forward)
	      (if (string= "/dev/null" folder)
		  (pmail-delete-message)
		(pmail-output folder 1 t)
		(setq directives nil))))
	(setq directives (cdr directives))))))

(defun pmail-next-message (n)
  "Show following message whether deleted or not.
With prefix arg N, moves forward N messages, or backward if N is
negative."
  (interactive "p")
  (with-current-buffer pmail-buffer
    (pmail-show-message (+ pmail-current-message n))))

(defun pmail-previous-message (n)
  "Show previous message whether deleted or not.
With prefix arg N, moves backward N messages, or forward if N is
negative."
  (interactive "p")
  (pmail-next-message (- n)))

(defun pmail-next-undeleted-message (n)
  "Show following non-deleted message.
With prefix arg N, moves forward N non-deleted messages, or
backward if N is negative.

Returns t if a new message is being shown, nil otherwise."
  (interactive "p")
  (let ((lastwin pmail-current-message)
	(original pmail-current-message)
	(current pmail-current-message))
    ;; Move forwards, remember the last undeleted message seen.
    (while (and (> n 0) (< current pmail-total-messages))
      (setq current (1+ current))
      (unless (pmail-desc-deleted-p current)
	(setq lastwin current
	      n (1- n))))
    ;; Same thing for moving backwards
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (unless (pmail-desc-deleted-p current)
	(setq lastwin current
	      n (1+ n))))
    ;; Show the message (even if no movement took place so that the
    ;; delete attribute is marked) and determine the result value.
    (pmail-show-message lastwin)
    (if (/= lastwin original)
        t
      (if (< n 0)
	  (message "No previous nondeleted message"))
      (if (> n 0)
	  (message "No following nondeleted message"))
      nil)))

(defun pmail-previous-undeleted-message (n)
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  (interactive "p")
  (pmail-next-undeleted-message (- n)))

(defun pmail-first-message ()
  "Show first message in file."
  (interactive)
  (pmail-show-message 1))

(defun pmail-last-message ()
  "Show last message in file."
  (interactive)
  (pmail-show-message pmail-total-messages))

(defun pmail-narrow-to-header (msg)
  "Narrow the buffer to the headers of message number MSG."
  (save-excursion
    (let ((start (pmail-desc-get-start msg))
	  (end (pmail-desc-get-end msg)))
      (widen)
      (goto-char start)
      (unless (search-forward "\n\n" end t)
	(error "Invalid message format"))
      (narrow-to-region start (point)))))

(defun pmail-message-recipients-p (msg recipients &optional primary-only)
  (save-restriction
    (or (string-match recipients (or (mail-fetch-field "To") ""))
	(string-match recipients (or (mail-fetch-field "From") ""))
	(if (not primary-only)
	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(defun pmail-message-regexp-p (msg regexp)
  "Return t, if for message number MSG, regexp REGEXP matches in the header."
  (save-excursion
    (save-restriction
      (pmail-narrow-to-header msg)
      (re-search-forward regexp nil t))))

(defun pmail-search-message (msg regexp)
  "Return non-nil, if for message number MSG, regexp REGEXP matches."
  (goto-char (pmail-desc-get-start msg))
  (if pmail-enable-mime
      (funcall pmail-search-mime-message-function msg regexp)
    (re-search-forward regexp (pmail-desc-get-end msg) t)))

(defvar pmail-search-last-regexp nil)
(defun pmail-search (regexp &optional n)
  "Show message containing next match for REGEXP (but not the current msg).
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (< (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Pmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if pmail-search-last-regexp
			(concat ", default "
				pmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq pmail-search-last-regexp regexp))
	    ((not pmail-search-last-regexp)
	     (error "No previous Pmail search string")))
      (list pmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (or n (setq n 1))
  (message "%sPmail search for %s..."
	   (if (< n 0) "Reverse " "")
	   regexp)
  (set-buffer pmail-buffer)
  (let ((omin (point-min))
	(omax (point-max))
	(opoint (point))
	(reversep (< n 0))
	(msg pmail-current-message)
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
			win (pmail-search-message msg regexp)))
	      (while (and (null win) (< msg pmail-total-messages))
		(setq msg (1+ msg)
		      win (pmail-search-message msg regexp))))
	    (setq n (+ n (if reversep 1 -1)))))
      (if win
	  (progn
	    (pmail-show-message msg)
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
	    (message "%sPmail search for %s...done"
		     (if reversep "Reverse " "")
		     regexp))
	(goto-char opoint)
	(narrow-to-region omin omax)
	(ding)
	(message "Search failed: %s" regexp)))))

(defun pmail-search-backwards (regexp &optional n)
  "Show message containing previous match for REGEXP.
Prefix argument gives repeat count; negative argument means search
forward (through later messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (>= (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Pmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if pmail-search-last-regexp
			(concat ", default "
				pmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq pmail-search-last-regexp regexp))
	    ((not pmail-search-last-regexp)
	     (error "No previous Pmail search string")))
      (list pmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (pmail-search regexp (- (or n 1))))

;; Show the first message which has the `unseen' attribute.
(defun pmail-first-unseen-message ()
  "Return the first message which has not been seen.  If all messages
have been seen, then return the last message."
  (let ((current 1)
	found)
    (while (and (not found) (<= current pmail-total-messages))
      (if (pmail-desc-attr-p pmail-desc-unseen-index current)
	  (setq found current))
      (setq current (1+ current)))
    (or found pmail-total-messages)))

(defun pmail-current-subject ()
  "Return the current subject.
The subject is stripped of leading and trailing whitespace, and
of typical reply prefixes such as Re:."
  (let ((subject (or (mail-fetch-field "Subject") "")))
    (if (string-match "\\`[ \t]+" subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match pmail-reply-regexp subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match "[ \t]+\\'" subject)
	(setq subject (substring subject 0 (match-beginning 0))))
    subject))

(defun pmail-current-subject-regexp ()
  "Return a regular expression matching the current subject.
The regular expression matches the subject header line of
messages about the same subject.  The subject itself is stripped
of leading and trailing whitespace, of typical reply prefixes
such as Re: and whitespace within the subject is replaced by a
regular expression matching whitespace in general in order to
take into account that subject header lines may include newlines
and more whitespace.  The returned regular expressions contains
`pmail-reply-regexp' and ends with a newline."
  (let ((subject (pmail-current-subject)))
    ;; If Subject is long, mailers will break it into several lines at
    ;; arbitrary places, so replace whitespace with a regexp that will
    ;; match any sequence of spaces, TABs, and newlines.
    (setq subject (regexp-quote subject))
    (setq subject
	  (replace-regexp-in-string "[ \t\n]+" "[ \t\n]+" subject t t))
    ;; Some mailers insert extra spaces after "Subject:", so allow any
    ;; amount of them.
    (concat "^Subject:[ \t]+"
	    (if (string= "\\`" (substring pmail-reply-regexp 0 2))
		(substring pmail-reply-regexp 2)
	      pmail-reply-regexp)
	    subject "[ \t]*\n")))

(defun pmail-next-same-subject (n)
  "Go to the next mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go backwards instead."
  (interactive "p")
  (let ((search-regexp (pmail-current-subject-regexp))
	(forward (> n 0))
	(i pmail-current-message)
	(case-fold-search t)
	found)
    (save-excursion
      (save-restriction
	(widen)
	(if forward
	    (while (and (/= n 0) (< i pmail-total-messages))
	      (let (done)
		(while (and (not done)
			    (< i pmail-total-messages))
		  (setq i (+ i 1))
		  (pmail-narrow-to-header i)
		  (goto-char (point-min))
		  (setq done (re-search-forward search-regexp (point-max) t)))
		(if done (setq found i)))
	      (setq n (1- n)))
	  (while (and (/= n 0) (> i 1))
	    (let (done)
	      (while (and (not done) (> i 1))
		(setq i (- i 1))
		(pmail-narrow-to-header i)
		(goto-char (point-min))
		(setq done (re-search-forward search-regexp (point-max) t)))
	      (if done (setq found i)))
	    (setq n (1+ n))))))
    (if found
	(pmail-show-message found)
      (error "No %s message with same subject"
	     (if forward "following" "previous")))))

(defun pmail-previous-same-subject (n)
  "Go to the previous mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go forwards instead."
  (interactive "p")
  (pmail-next-same-subject (- n)))

;;;; *** Pmail Message Deletion Commands ***

(defun pmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (pmail-desc-set-attribute pmail-desc-deleted-index t pmail-current-message)
  (run-hooks 'pmail-delete-message-hook)
  (pmail-show-message pmail-current-message))

(defun pmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
  (set-buffer pmail-buffer)
  (let ((msg pmail-current-message))
    (while (and (> msg 0)
		(not (pmail-desc-attr-p pmail-desc-deleted-index msg)))
      (setq msg (1- msg)))
    (if (= msg 0)
	(error "No previous deleted message")
      (pmail-desc-set-attribute pmail-desc-deleted-index nil msg)
      (pmail-show-message msg)
      (if (pmail-summary-exists)
	  (save-excursion
	    (set-buffer pmail-summary-buffer)
	    (pmail-summary-mark-undeleted msg)))
      (pmail-maybe-display-summary))))

;;; mbox: ready
(defun pmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[pmail-expunge] command is given.
With prefix argument, delete and move backward.

Returns t if a new message is displayed after the delete, or nil otherwise."
  (interactive "P")
  (pmail-desc-set-attribute pmail-desc-deleted-index t pmail-current-message)
  (run-hooks 'pmail-delete-message-hook)
  (let ((del-msg pmail-current-message))
    (if (pmail-summary-exists)
	(pmail-select-summary
	 (pmail-summary-mark-deleted del-msg)))
    (prog1 (pmail-next-undeleted-message (if backward -1 1))
      (pmail-maybe-display-summary))))

;;; mbox: ready
(defun pmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[pmail-expunge] command is given."
  (interactive)
  (pmail-delete-forward t))

(defun pmail-expunge-confirmed ()
  "Return t if deleted message should be expunged.  If necessary, ask the user.
See also user-option `pmail-confirm-expunge'."
  (set-buffer pmail-buffer)
  (let ((some-deleted))
    (dotimes (i pmail-total-messages)
      (if (pmail-desc-deleted-p (1+ i))
	  (setq some-deleted t)))
    (or (not some-deleted)
	(null pmail-confirm-expunge)
	(funcall pmail-confirm-expunge
		 "Erase deleted messages from Pmail file? "))))

(defun pmail-only-expunge (&optional dont-show)
  "Actually erase all deleted messages in the file."
  (interactive)
  (message "Expunging deleted messages...")
  ;; Discard all undo records for this buffer.
  (or (eq buffer-undo-list t) (setq buffer-undo-list nil))
  ;; Remove the messages from the buffer and from the Pmail message
  ;; descriptor vector.
  (pmail-desc-prune-deleted-messages 'pmail-expunge-callback)
  ;; Deal with the summary buffer and update
  ;; the User status.
  (let* ((omax (- (buffer-size) (point-max)))
	 (omin (- (buffer-size) (point-min)))
	 (opoint (if (and (> pmail-current-message 0)
			  (pmail-message-deleted-p pmail-current-message))
		     0
		   (if pmail-enable-mime
		       (with-current-buffer pmail-view-buffer
			 (- (point)(point-min)))
		     (- (point) (point-min))))))
    (when pmail-summary-buffer
      (with-current-buffer pmail-summary-buffer
	(pmail-update-summary)))
    (message "Expunging deleted messages...done")
    (if (not dont-show)
	(pmail-show-message
	 (if (zerop pmail-current-message) 1 nil)))
    (if pmail-enable-mime
	(goto-char (+ (point-min) opoint))
      (goto-char (+ (point) opoint)))))

;;; mbox: ready
(defun pmail-expunge-callback (n)
  "Called after message N has been pruned to update the current Pmail
  message counter."
  ;; Process the various possible states to set the current message
  ;; counter.
  (setq pmail-total-messages (1- pmail-total-messages)
	pmail-current-message
	(cond
	 ((= 0 pmail-total-messages) 0)
	 ((> pmail-current-message n) (pmail-desc-get-previous pmail-desc-deleted-index n))
	 ((> pmail-current-message n) 0)
	 (t pmail-current-message))))

;;; mbox: ready
(defun pmail-expunge ()
  "Erase deleted messages from Pmail file and summary buffer."
  (interactive)
  (when (pmail-expunge-confirmed)
    (pmail-only-expunge)))

;;;; *** Pmail Mailing Commands ***

;;; mbox: In progress.  I'm still not happy with the initial citation
;;; stuff. -pmr
(defun pmail-start-mail (&optional noerase to subject in-reply-to cc
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
      (if pmail-mail-new-frame
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

(defun pmail-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (pmail-start-mail nil nil nil nil nil pmail-view-buffer))

(defun pmail-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (pmail-start-mail t))

(defun pmail-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (if (= pmail-total-messages 0)
      (error "No messages in this file"))
  (save-excursion
    (save-restriction
      (let* ((msgnum pmail-current-message)
	     (from (pmail-header-get-header "from"))
	     (reply-to (or (pmail-header-get-header "reply-to" nil t) from))
	     (cc (unless just-sender
		   (pmail-header-get-header "cc" nil t)))
	     (subject (pmail-header-get-header "subject"))
	     (date (pmail-header-get-header "date"))
	     (to (or (pmail-header-get-header "to" nil t) ""))
	     (message-id (pmail-header-get-header "message-id"))
	     (references (pmail-header-get-header "references" nil nil t))
	     (resent-to (pmail-header-get-header "resent-reply-to" nil t))
	     (resent-cc (unless just-sender
			  (pmail-header-get-header "resent-cc" nil t)))
	     (resent-reply-to (or (pmail-header-get-header "resent-to" nil t) "")))
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
                   (concat pmail-reply-prefix
                           (if (let ((case-fold-search t))
                                 (string-match pmail-reply-regexp subject))
                               (substring subject (match-end 0))
                             subject))))
        ;; Now setup the mail reply buffer.
        (pmail-start-mail
         nil
         ;; Using mail-strip-quoted-names is undesirable with newer
         ;; mailers since they can handle the names unstripped.  I
         ;; don't know whether there are other mailers that still need
         ;; the names to be stripped.
;;;     (mail-strip-quoted-names reply-to)
	 ;; Remove unwanted names from reply-to, since Mail-Followup-To
	 ;; header causes all the names in it to wind up in reply-to, not
	 ;; in cc.  But if what's left is an empty list, use the original.
	 (let* ((reply-to-list (rmail-dont-reply-to reply-to)))
	   (if (string= reply-to-list "") reply-to reply-to-list))
         subject
         (pmail-make-in-reply-to-field from date message-id)
         (if just-sender
             nil
           ;; mail-strip-quoted-names is NOT necessary for
           ;; rmail-dont-reply-to to do its job.
           (let* ((cc-list (rmail-dont-reply-to
                            (mail-strip-quoted-names
                             (if (null cc) to (concat to ", " cc))))))
             (if (string= cc-list "") nil cc-list)))
         pmail-view-buffer
         (list (list 'pmail-reply-callback pmail-buffer "answered" t msgnum))
         nil
         (list (cons "References" (concat (mapconcat 'identity references " ")
                                          " " message-id))))))))

(defun pmail-reply-callback (buffer attr state n)
  "Mail reply callback function.
Sets ATTR (a string) if STATE is
non-nil, otherwise clears it.  N is the message number.
BUFFER, possibly narrowed, contains an mbox mail message."
  (save-excursion
    (set-buffer buffer)
    (pmail-set-attribute attr state n)
    (pmail-show-message)))

(defun pmail-mark-message (msgnum-list attr-index)
  "Set attribute ATTRIBUTE-INDEX in the message of the car of MSGNUM-LIST.
This is used in the send-actions for
message buffers.  MSGNUM-LIST is a list of the form (MSGNUM)."
  (save-excursion
    (let ((n (car msgnum-list)))
      (set-buffer pmail-buffer)
      (pmail-narrow-to-message n)
      (pmail-desc-set-attribute attr-index t n))))

(defun pmail-narrow-to-message (n)
  "Narrow the current (pmail) buffer to bracket message N."
  (widen)
  (narrow-to-region (pmail-desc-get-start n) (pmail-desc-get-end n)))

(defun pmail-make-in-reply-to-field (from date message-id)
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
           (pmail-make-in-reply-to-field from date message-id)))))

;;; mbox: ready
(defun pmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `pmail-resend'."
  (interactive "P")
  (if (= pmail-total-messages 0)
      (error "No messages in this file"))
  (if resend
      (call-interactively 'pmail-resend)
    (let ((forward-buffer pmail-buffer)
	  (msgnum pmail-current-message)
	  (subject (concat "["
			   (let ((from (or (mail-fetch-field "From")
					   (mail-fetch-field ">From"))))
			     (if from
				 (concat (mail-strip-quoted-names from) ": ")
			       ""))
			   (or (mail-fetch-field "Subject") "")
			   "]")))
      (if (pmail-start-mail
	   nil nil subject nil nil nil
	   (list (list 'pmail-mark-message
		       forward-buffer
		       (with-current-buffer pmail-buffer
			 (pmail-desc-get-start msgnum))
		       "forwarded"))
	   ;; If only one window, use it for the mail buffer.
	   ;; Otherwise, use another window for the mail buffer
	   ;; so that the Pmail buffer remains visible
	   ;; and sending the mail will get back to it.
	   (and (not pmail-mail-new-frame) (one-window-p t)))
	  ;; The mail buffer is now current.
	  (save-excursion
	    ;; Insert after header separator--before signature if any.
	    (goto-char (mail-text-start))
	    (if (or pmail-enable-mime pmail-enable-mime-composing)
		(funcall pmail-insert-mime-forwarded-message-function
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

(defun pmail-resend (address &optional from comment mail-alias-file)
  "Resend current message to ADDRESSES.
ADDRESSES should be a single address, a string consisting of several
addresses separated by commas, or a list of addresses.

Optional FROM is the address to resend the message from, and
defaults from the value of `user-mail-address'.
Optional COMMENT is a string to insert as a comment in the resent message.
Optional ALIAS-FILE is alternate aliases file to be used by sendmail,
typically for purposes of moderating a list."
  (interactive "sResend to: ")
  (if (= pmail-total-messages 0)
      (error "No messages in this file"))
  (require 'sendmail)
  (require 'mailalias)
  (unless (or (eq pmail-view-buffer (current-buffer))
	      (eq pmail-buffer (current-buffer)))
    (error "Not an Pmail buffer"))
  (if (not from) (setq from user-mail-address))
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	(mail-personal-alias-file
	 (or mail-alias-file mail-personal-alias-file))
	(mailbuf pmail-buffer))
    (unwind-protect
	(with-current-buffer tembuf
	  ;;>> Copy message into temp buffer
	  (if pmail-enable-mime
	      (funcall pmail-insert-mime-resent-message-function mailbuf)
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
    (with-current-buffer pmail-buffer
      (pmail-set-attribute "resent" t pmail-current-message))))

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

(defun pmail-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message.
If the failed message is a MIME multipart message, it is searched for a
body part with a header which matches the variable `mail-mime-unsent-header'.
Otherwise, the variable `mail-unsent-separator' should match the string that
delimits the returned original message.
The variable `pmail-retry-ignored-headers' is a regular expression
specifying headers which should not be copied into the new message."
  (interactive)
  (if (= pmail-total-messages 0)
      (error "No messages in this file"))
  (require 'mail-utils)
  (let ((pmail-this-buffer (current-buffer))
	(msgnum pmail-current-message)
	bounce-start bounce-end bounce-indent resending
	(content-type
	 (save-excursion
	   (save-restriction
	     (pmail-header-get-header "Content-Type")))))
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
      (if (pmail-start-mail nil nil nil nil nil pmail-this-buffer
			    (list (list 'pmail-mark-message
					pmail-this-buffer
					(with-current-buffer pmail-buffer
					  (pmail-desc-get-start msgnum))
					"retried")))
	  ;; Insert original text as initial text of new draft message.
	  ;; Bind inhibit-read-only since the header delimiter
	  ;; of the previous message was probably read-only.
	  (let ((inhibit-read-only t)
		pmail-displayed-headers
		pmail-ignored-headers)
	    (erase-buffer)
	    (insert-buffer-substring pmail-this-buffer
				     bounce-start bounce-end)
	    (goto-char (point-min))
	    (if bounce-indent
		(indent-rigidly (point-min) (point-max) bounce-indent))
	    (pmail-clear-headers pmail-retry-ignored-headers)
	    (pmail-clear-headers "^sender:\\|^return-path:\\|^received:")
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

(defun pmail-summary-exists ()
  "Non-nil if in an PMAIL buffer and an associated summary buffer exists.
In fact, the non-nil value returned is the summary buffer itself."
  (and pmail-summary-buffer (buffer-name pmail-summary-buffer)
       pmail-summary-buffer))

(defun pmail-summary-displayed ()
  "t if in PMAIL buffer and an associated summary buffer is displayed."
  (and pmail-summary-buffer (get-buffer-window pmail-summary-buffer)))

(defcustom pmail-redisplay-summary nil
  "*Non-nil means Pmail should show the summary when it changes.
This has an effect only if a summary buffer exists."
  :type 'boolean
  :group 'pmail-summary)

(defcustom pmail-summary-window-size nil
  "*Non-nil means specify the height for an Pmail summary window."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'pmail-summary)

;; Put the summary buffer back on the screen, if user wants that.
(defun pmail-maybe-display-summary ()
  (let ((selected (selected-window))
	window)
    ;; If requested, make sure the summary is displayed.
    (and pmail-summary-buffer (buffer-name pmail-summary-buffer)
	 pmail-redisplay-summary
	 (if (get-buffer-window pmail-summary-buffer 0)
	     ;; It's already in some frame; show that one.
	     (let ((frame (window-frame
			   (get-buffer-window pmail-summary-buffer 0))))
	       (make-frame-visible frame)
	       (raise-frame frame))
	   (display-buffer pmail-summary-buffer)))
    ;; If requested, set the height of the summary window.
    (and pmail-summary-buffer (buffer-name pmail-summary-buffer)
	 pmail-summary-window-size
	 (setq window (get-buffer-window pmail-summary-buffer))
	 ;; Don't try to change the size if just one window in frame.
	 (not (eq window (frame-root-window (window-frame window))))
	 (unwind-protect
	     (progn
	       (select-window window)
	       (enlarge-window (- pmail-summary-window-size (window-height))))
	   (select-window selected)))))

;;;; *** Pmail Local Fontification ***

(defun pmail-fontify-buffer-function ()
  ;; This function's symbol is bound to font-lock-fontify-buffer-function.
  (add-hook 'pmail-show-message-hook 'pmail-fontify-message nil t)
  ;; If we're already showing a message, fontify it now.
  (if pmail-current-message (pmail-fontify-message))
  ;; Prevent Font Lock mode from kicking in.
  (setq font-lock-fontified t))

(defun pmail-unfontify-buffer-function ()
  ;; This function's symbol is bound to font-lock-fontify-unbuffer-function.
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t) (inhibit-read-only t)
	before-change-functions after-change-functions
	buffer-file-name buffer-file-truename)
    (save-restriction
      (widen)
      (remove-hook 'pmail-show-message-hook 'pmail-fontify-message t)
      (remove-text-properties (point-min) (point-max) '(pmail-fontified nil))
      (font-lock-default-unfontify-buffer)
      (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil)))))

(defun pmail-fontify-message ()
  "Fontify the current message if it is not already fontified."
  (when (text-property-any (point-min) (point-max) 'pmail-fontified nil)
    (let ((modified (buffer-modified-p))
	  (buffer-undo-list t) (inhibit-read-only t)
	  before-change-functions after-change-functions
	  buffer-file-name buffer-file-truename)
      (save-excursion
	(save-match-data
	  (add-text-properties (point-min) (point-max) '(pmail-fontified t))
	  (font-lock-fontify-region (point-min) (point-max))
	  (and (not modified) (buffer-modified-p)
	       (set-buffer-modified-p nil)))))))

;;; Speedbar support for PMAIL files.
(eval-when-compile (require 'speedbar))

(defvar pmail-speedbar-match-folder-regexp "^[A-Z0-9]+\\(\\.[A-Z0-9]+\\)?$"
  "*This regex is used to match folder names to be displayed in speedbar.
Enabling this will permit speedbar to display your folders for easy
browsing, and moving of messages.")

(defvar pmail-speedbar-last-user nil
  "The last user to be displayed in the speedbar.")

(defvar pmail-speedbar-key-map nil
  "Keymap used when in pmail display mode.")

(defun pmail-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance pmail."
  (if pmail-speedbar-key-map
      nil
    (setq pmail-speedbar-key-map (speedbar-make-specialized-keymap))

    (define-key pmail-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key pmail-speedbar-key-map "r" 'speedbar-edit-line)
    (define-key pmail-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key pmail-speedbar-key-map "M"
      'pmail-speedbar-move-message-to-folder-on-line)))

(defvar pmail-speedbar-menu-items
  '(["Read Folder" speedbar-edit-line t]
    ["Move message to folder" pmail-speedbar-move-message-to-folder-on-line
     (save-excursion (beginning-of-line)
		     (looking-at "<M> "))])
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (pmail-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'pmail-install-speedbar-variables))

(defun pmail-speedbar-buttons (buffer)
  "Create buttons for BUFFER containing pmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that PMAIL folder."
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
	     (equal from pmail-speedbar-last-user))
	nil
      (setq pmail-speedbar-last-user from)
      (erase-buffer)
      (insert "Reply To:\n")
      (if (stringp from)
	  (speedbar-insert-button from 'speedbar-directory-face 'highlight
				  'pmail-speedbar-button 'pmail-reply))
      (insert "Folders:\n")
      (let* ((case-fold-search nil)
	     (df (directory-files (save-excursion (set-buffer buffer)
						  default-directory)
				  nil pmail-speedbar-match-folder-regexp)))
	(while df
	  (speedbar-insert-button "<M>" 'speedbar-button-face 'highlight
				  'pmail-speedbar-move-message (car df))
	  (speedbar-insert-button (car df) 'speedbar-file-face 'highlight
				  'pmail-speedbar-find-file nil t)
	  (setq df (cdr df)))))))

(defun pmail-speedbar-button (text token indent)
  "Execute an pmail command specified by TEXT.
The command used is TOKEN.  INDENT is not used."
  (speedbar-with-attached-buffer
   (funcall token t)))

(defun pmail-speedbar-find-file (text token indent)
  "Load in the pmail file TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Loading in PMAIL file %s..." text)
   (find-file text)))

(defun pmail-speedbar-move-message-to-folder-on-line ()
  "If the current line is a folder, move current message to it."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "<M> " (save-excursion (end-of-line) (point)) t)
	(progn
	  (forward-char -2)
	  (speedbar-do-function-pointer)))))

(defun pmail-speedbar-move-message (text token indent)
  "From button TEXT, copy current message to the pmail file specified by TOKEN.
TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Moving message to %s" token)
   (pmail-output-to-pmail-file token)))

; Functions for setting, getting and encoding the POP password.
; The password is encoded to prevent it from being easily accessible
; to "prying eyes."  Obviously, this encoding isn't "real security,"
; nor is it meant to be.

;;;###autoload
(defun pmail-set-remote-password (password)
  "Set PASSWORD to be used for retrieving mail from a POP or IMAP server."
  (interactive "sPassword: ")
  (if password
      (setq pmail-encoded-remote-password
	    (pmail-encode-string password (emacs-pid)))
    (setq pmail-remote-password nil)
    (setq pmail-encoded-remote-password nil)))

(defun pmail-get-remote-password (imap)
  "Get the password for retrieving mail from a POP or IMAP server.  If none
has been set, then prompt the user for one."
  (when (not pmail-encoded-remote-password)
    (if (not pmail-remote-password)
	(setq pmail-remote-password
	      (read-passwd (if imap
			       "IMAP password: "
			     "POP password: "))))
    (pmail-set-remote-password pmail-remote-password)
    (setq pmail-remote-password nil))
  (pmail-encode-string pmail-encoded-remote-password (emacs-pid)))

(defun pmail-have-password ()
  (or pmail-remote-password pmail-encoded-remote-password))

(defun pmail-encode-string (string mask)
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

(defun pmail-restore-desktop-buffer (desktop-buffer-file-name
                                     desktop-buffer-name
                                     desktop-buffer-misc)
  "Restore an pmail buffer specified in a desktop file."
  (condition-case error
      (progn
        (pmail-input desktop-buffer-file-name)
        (if (eq major-mode 'pmail-mode)
            (current-buffer)
          pmail-buffer))
    (file-locked
      (kill-buffer (current-buffer))
      nil)))

(add-to-list 'desktop-buffer-mode-handlers
	     '(pmail-mode . pmail-restore-desktop-buffer))

(provide 'pmail)

;; arch-tag: 65d257d3-c281-4a65-9c38-e61af95af2f0
;;; pmail.el ends here
