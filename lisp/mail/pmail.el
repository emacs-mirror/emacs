;;; pmail.el --- main code of "PMAIL" mail reader for Emacs

;; Copyright (C) 1985, 1986, 1987, 1988, 1993, 1994, 1995, 1996, 1997, 1998,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

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

(require 'mail-utils)
(eval-when-compile (require 'mule-util)) ; for detect-coding-with-priority

(defconst pmail-attribute-header "X-BABYL-V6-ATTRIBUTES"
  "The header that stores the Pmail attribute data.")

(defconst pmail-keyword-header "X-BABYL-V6-KEYWORDS"
  "The header that stores the Pmail keyword data.")

;;; Attribute indexes

(defconst pmail-answered-attr-index 0
  "The index for the `answered' attribute.")

(defconst pmail-deleted-attr-index 1
  "The index for the `deleted' attribute.")

(defconst pmail-edited-attr-index 2
  "The index for the `edited' attribute.")

(defconst pmail-filed-attr-index 3
  "The index for the `filed' attribute.")

(defconst pmail-resent-attr-index 4
  "The index for the `resent' attribute.")

(defconst pmail-stored-attr-index 5
  "The index for the `stored' attribute.")

(defconst pmail-unseen-attr-index 6
  "The index for the `unseen' attribute.")

(defconst pmail-attr-array
  '[(?A "answered")
    (?D "deleted")
    (?E "edited")
    (?F "filed")
    (?R "replied")
    (?S "stored")
    (?U "unseen")]
  "An array that provides a mapping between an attribute index,
it's character representation and it's display representation.")

(defconst pmail-attribute-field-name "x-babyl-v6-attributes"
  "The message header field added by Rmail to maintain status.")

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

(defvar pmail-buffers-swapped-p nil
  "A flag that is non-nil when the message view buffer and the
 message collection buffer are swapped, i.e. the Pmail buffer
 contains a single decoded message.")

(defvar pmail-header-style 'normal
  "The current header display style choice, one of
'normal (selected headers) or 'full (all headers).")

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
(declare-function pmail-dont-reply-to "mail-utils" (destinations))
(declare-function pmail-update-summary "pmailsum" (&rest ignore))

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
	  "\\|^x-.*:")
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
A value of nil means don't highlight."
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

Where FOLDERNAME is the name of a BABYL format folder to put the
message.  If any of the field regexp's are nil, then it is ignored.

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

(defvar pmail-message-vector nil)
(put 'pmail-message-vector 'permanent-local t)

(defvar pmail-deleted-vector nil)
(put 'pmail-deleted-vector 'permanent-local t)

(defvar pmail-msgref-vector nil
  "In an Pmail buffer, a vector whose Nth element is a list (N).
When expunging renumbers messages, these lists are modified
by substituting the new message number into the existing list.")
(put 'pmail-msgref-vector 'permanent-local t)

(defvar pmail-overlay-list nil)
(put 'pmail-overlay-list 'permanent-local t)

;; These are used by autoloaded pmail-summary.

(defvar pmail-summary-buffer nil)
(put 'pmail-summary-buffer 'permanent-local t)
(defvar pmail-summary-vector nil)
(put 'pmail-summary-vector 'permanent-local t)

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
;;; (UNIX From lines), minus the initial ^.  Note that if you change
;;; this expression, you must change the code in pmail-nuke-pinhead-header
;;; that knows the exact ordering of the \\( \\) subexpressions.
(defvar pmail-unix-mail-delimiter
  (let ((time-zone-regexp
	 (concat "\\([A-Z]?[A-Z]?[A-Z][A-Z]\\( DST\\)?"
		 "\\|[-+]?[0-9][0-9][0-9][0-9]"
		 "\\|"
		 "\\) *")))
    (concat
     "From "

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
      (list '("^\\(From\\|Sender\\|Resent-From\\):"
	      . 'pmail-header-name)
	    '("^Reply-To:.*$" . 'pmail-header-name)
	    '("^Subject:" . 'pmail-header-name)
	    '("^X-Spam-Status:" . 'pmail-header-name)
	    '("^\\(To\\|Apparently-To\\|Cc\\|Newsgroups\\):"
	      . 'pmail-header-name)
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
	 ;; Since the file may contain messages of different encodings
	 ;; at the tail (non-BYBYL part), we can't decode them at once
	 ;; on reading.  So, at first, we read the file without text
	 ;; code conversion, then decode the messages one by one by
	 ;; pmail-decode-babyl-format or
	 ;; pmail-convert-to-babyl-format.
	 (coding-system-for-read (and pmail-enable-multibyte 'raw-text))
	 run-mail-hook msg-shown)
    ;; Like find-file, but in the case where a buffer existed
    ;; and the file was reverted, recompute the message-data.
    ;; We used to bind enable-local-variables to nil here,
    ;; but that should not be needed now that pmail-mode
    ;; sets it locally to nil.
    ;; (Binding a variable locally with let is not safe if it has
    ;; buffer-local bindings.)
    (if (and existed (not (verify-visited-file-modtime existed)))
	(progn
	  (find-file file-name)
	  (when (and (verify-visited-file-modtime existed)
		     (eq major-mode 'pmail-mode))
	    (pmail-forget-messages)
	    (pmail-set-message-counters)))
      (switch-to-buffer
       (let ((enable-local-variables nil))
	 (find-file-noselect file-name))))
    (setq pmail-buffers-swapped-p nil)
    (if (eq major-mode 'pmail-edit-mode)
	(error "Exit Pmail Edit mode before getting new mail"))
    (if (and existed (> (buffer-size) 0))
	;; Buffer not new and not empty; ensure in proper mode, but that's all.
	(or (eq major-mode 'pmail-mode)
	    (progn (pmail-mode-2)
		   (setq run-mail-hook t)))
      (setq run-mail-hook t)
      (pmail-mode-2)
      (pmail-convert-file-maybe)
      (goto-char (point-max)))
    ;; As we have read a file by raw-text, the buffer is set to
    ;; unibyte.  We must make it multibyte if necessary.
    (if (and pmail-enable-multibyte
	     (not enable-multibyte-characters))
	(set-buffer-multibyte t))
    ;; If necessary, scan to find all the messages.
    (pmail-maybe-set-message-counters)
    (unwind-protect
	(unless (and (not file-name-arg) (pmail-get-new-mail))
	  (pmail-show-message (pmail-first-unseen-message)))
      (progn
	(if pmail-display-summary (pmail-summary))
	(pmail-construct-io-menu)
	(if run-mail-hook
	    (run-hooks 'pmail-mode-hook))))))

;; Given the value of MAILPATH, return a list of inbox file names.
;; This is turned off because it is not clear that the user wants
;; all these inboxes to feed into the primary pmail file.
; (defun pmail-convert-mailpath (string)
;   (let (idx list)
;     (while (setq idx (string-match "[%:]" string))
;       (let ((this (substring string 0 idx)))
; 	(setq string (substring string (1+ idx)))
; 	(setq list (cons (if (string-match "%" this)
; 			     (substring this 0 (string-match "%" this))
; 			   this)
; 			 list))))
;     list))

; I have checked that adding "-*- pmail -*-" to the BABYL OPTIONS line
; will not cause emacs 18.55 problems.

;; This calls pmail-decode-babyl-format if the file is already Babyl.

(defun pmail-convert-file-maybe ()
  "Determine if the file needs to be converted to mbox format."
  (widen)
  (goto-char (point-min))
  ;; Detect previous Babyl format files.
  (cond ((looking-at "BABYL OPTIONS:")
	 ;; The file is Babyl version 5.  Use unrmail to convert
	 ;; it.
	 (pmail-convert-babyl-to-mbox))
	((looking-at "Version: 5\n")
	 ;; Losing babyl file made by old version of Pmail.  Fix the
	 ;; babyl file header and use unrmail to convert to mbox
	 ;; format.
	 (let ((buffer-read-only nil))
	   (insert "BABYL OPTIONS: -*- pmail -*-\n")
	   (pmail-convert-babyl-to-mbox)))
	((equal (point-min) (point-max))
	 (message "Empty Pmail file."))
	((looking-at "From "))
	(t (error "Invalid mbox format mail file."))))

(defun pmail-convert-babyl-to-mbox ()
  "Convert the mail file from Babyl version 5 to mbox."
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

(defun pmail-insert-pmail-file-header ()
  (let ((buffer-read-only nil))
    ;; -*-pmail-*- is here so that visiting the file normally
    ;; recognizes it as an Pmail file.
    (insert "BABYL OPTIONS: -*- pmail -*-
Version: 5
Labels:
Note:   This is the header of an pmail file.
Note:   If you are seeing it in pmail,
Note:    it means the file has no messages in it.\n\^_")))

;; Decode Babyl formatted part at the head of current buffer by
;; pmail-file-coding-system, or if it is nil, do auto conversion.

(defun pmail-decode-babyl-format ()
  (let ((modifiedp (buffer-modified-p))
	(buffer-read-only nil)
	(coding-system pmail-file-coding-system)
	from to)
    (goto-char (point-min))
    (search-forward "\n\^_" nil t)	; Skip BABYL header.
    (setq from (point))
    (goto-char (point-max))
    (search-backward "\n\^_" from 'mv)
    (setq to (point))
    (unless (and coding-system
		 (coding-system-p coding-system))
      (setq coding-system
	    ;; If pmail-file-coding-system is nil, Emacs 21 writes
	    ;; PMAIL files in emacs-mule, Emacs 22 in utf-8, but
	    ;; earlier versions did that with the current buffer's
	    ;; encoding.  So we want to favor detection of emacs-mule
	    ;; (whose normal priority is quite low) and utf-8, but
	    ;; still allow detection of other encodings if they won't
	    ;; fit.  The call to with-coding-priority below achieves
	    ;; that.
	    (with-coding-priority '(emacs-mule utf-8)
	      (detect-coding-region from to 'highest))))
    (unless (eq (coding-system-type coding-system) 'undecided)
      (set-buffer-modified-p t)		; avoid locking when decoding
      (let ((buffer-undo-list t))
	(decode-coding-region from to coding-system))
      (setq coding-system last-coding-system-used))
    (set-buffer-modified-p modifiedp)
    (setq buffer-file-coding-system nil)
    (setq save-buffer-coding-system
	  (or coding-system 'undecided))))

(defvar pmail-mode-map nil)
(if pmail-mode-map
    nil
  (setq pmail-mode-map (make-keymap))
  (suppress-keymap pmail-mode-map)
  (define-key pmail-mode-map "a"      'pmail-add-label)
  (define-key pmail-mode-map "b"      'pmail-bury)
  (define-key pmail-mode-map "c"      'pmail-continue)
  (define-key pmail-mode-map "d"      'pmail-delete-forward)
  (define-key pmail-mode-map "\C-d"   'pmail-delete-backward)
  (define-key pmail-mode-map "e"      'pmail-edit-current-message)
  (define-key pmail-mode-map "f"      'pmail-forward)
  (define-key pmail-mode-map "g"      'pmail-get-new-mail)
  (define-key pmail-mode-map "h"      'pmail-summary)
  (define-key pmail-mode-map "i"      'pmail-input)
  (define-key pmail-mode-map "j"      'pmail-show-message)
  (define-key pmail-mode-map "k"      'pmail-kill-label)
  (define-key pmail-mode-map "l"      'pmail-summary-by-labels)
  (define-key pmail-mode-map "\e\C-h" 'pmail-summary)
  (define-key pmail-mode-map "\e\C-l" 'pmail-summary-by-labels)
  (define-key pmail-mode-map "\e\C-r" 'pmail-summary-by-recipients)
  (define-key pmail-mode-map "\e\C-s" 'pmail-summary-by-regexp)
  (define-key pmail-mode-map "\e\C-t" 'pmail-summary-by-topic)
  (define-key pmail-mode-map "m"      'pmail-mail)
  (define-key pmail-mode-map "\em"    'pmail-retry-failure)
  (define-key pmail-mode-map "n"      'pmail-next-undeleted-message)
  (define-key pmail-mode-map "\en"    'pmail-next-message)
  (define-key pmail-mode-map "\e\C-n" 'pmail-next-labeled-message)
  (define-key pmail-mode-map "o"      'pmail-output-to-pmail-file)
  (define-key pmail-mode-map "\C-o"   'pmail-output)
  (define-key pmail-mode-map "p"      'pmail-previous-undeleted-message)
  (define-key pmail-mode-map "\ep"    'pmail-previous-message)
  (define-key pmail-mode-map "\e\C-p" 'pmail-previous-labeled-message)
  (define-key pmail-mode-map "q"      'pmail-quit)
  (define-key pmail-mode-map "r"      'pmail-reply)
;; I find I can't live without the default M-r command -- rms.
;;  (define-key pmail-mode-map "\er"  'pmail-search-backwards)
  (define-key pmail-mode-map "s"      'pmail-expunge-and-save)
  (define-key pmail-mode-map "\es"    'pmail-search)
  (define-key pmail-mode-map "t"      'pmail-toggle-header)
  (define-key pmail-mode-map "u"      'pmail-undelete-previous-message)
  (define-key pmail-mode-map "w"      'pmail-output-body-to-file)
  (define-key pmail-mode-map "x"      'pmail-expunge)
  (define-key pmail-mode-map "."      'pmail-beginning-of-message)
  (define-key pmail-mode-map "/"      'pmail-end-of-message)
  (define-key pmail-mode-map "<"      'pmail-first-message)
  (define-key pmail-mode-map ">"      'pmail-last-message)
  (define-key pmail-mode-map " "      'scroll-up)
  (define-key pmail-mode-map "\177"   'scroll-down)
  (define-key pmail-mode-map "?"      'describe-mode)
  (define-key pmail-mode-map "\C-c\C-s\C-d" 'pmail-sort-by-date)
  (define-key pmail-mode-map "\C-c\C-s\C-s" 'pmail-sort-by-subject)
  (define-key pmail-mode-map "\C-c\C-s\C-a" 'pmail-sort-by-author)
  (define-key pmail-mode-map "\C-c\C-s\C-r" 'pmail-sort-by-recipient)
  (define-key pmail-mode-map "\C-c\C-s\C-c" 'pmail-sort-by-correspondent)
  (define-key pmail-mode-map "\C-c\C-s\C-l" 'pmail-sort-by-lines)
  (define-key pmail-mode-map "\C-c\C-s\C-k" 'pmail-sort-by-labels)
  (define-key pmail-mode-map "\C-c\C-n" 'pmail-next-same-subject)
  (define-key pmail-mode-map "\C-c\C-p" 'pmail-previous-same-subject)
  )

(define-key pmail-mode-map [menu-bar] (make-sparse-keymap))

(define-key pmail-mode-map [menu-bar classify]
  (cons "Classify" (make-sparse-keymap "Classify")))

(define-key pmail-mode-map [menu-bar classify input-menu]
  nil)

(define-key pmail-mode-map [menu-bar classify output-menu]
  nil)

(define-key pmail-mode-map [menu-bar classify output-body]
  '("Output body to file..." . pmail-output-body-to-file))

(define-key pmail-mode-map [menu-bar classify output-inbox]
  '("Output (inbox)..." . pmail-output))

(define-key pmail-mode-map [menu-bar classify output]
  '("Output (Pmail)..." . pmail-output-to-pmail-file))

(define-key pmail-mode-map [menu-bar classify kill-label]
  '("Kill Label..." . pmail-kill-label))

(define-key pmail-mode-map [menu-bar classify add-label]
  '("Add Label..." . pmail-add-label))

(define-key pmail-mode-map [menu-bar summary]
  (cons "Summary" (make-sparse-keymap "Summary")))

(define-key pmail-mode-map [menu-bar summary senders]
  '("By Senders..." . pmail-summary-by-senders))

(define-key pmail-mode-map [menu-bar summary labels]
  '("By Labels..." . pmail-summary-by-labels))

(define-key pmail-mode-map [menu-bar summary recipients]
  '("By Recipients..." . pmail-summary-by-recipients))

(define-key pmail-mode-map [menu-bar summary topic]
  '("By Topic..." . pmail-summary-by-topic))

(define-key pmail-mode-map [menu-bar summary regexp]
  '("By Regexp..." . pmail-summary-by-regexp))

(define-key pmail-mode-map [menu-bar summary all]
  '("All" . pmail-summary))

(define-key pmail-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))

(define-key pmail-mode-map [menu-bar mail pmail-get-new-mail]
  '("Get New Mail" . pmail-get-new-mail))

(define-key pmail-mode-map [menu-bar mail lambda]
  '("----"))

(define-key pmail-mode-map [menu-bar mail continue]
  '("Continue" . pmail-continue))

(define-key pmail-mode-map [menu-bar mail resend]
  '("Re-send..." . pmail-resend))

(define-key pmail-mode-map [menu-bar mail forward]
  '("Forward" . pmail-forward))

(define-key pmail-mode-map [menu-bar mail retry]
  '("Retry" . pmail-retry-failure))

(define-key pmail-mode-map [menu-bar mail reply]
  '("Reply" . pmail-reply))

(define-key pmail-mode-map [menu-bar mail mail]
  '("Mail" . pmail-mail))

(define-key pmail-mode-map [menu-bar delete]
  (cons "Delete" (make-sparse-keymap "Delete")))

(define-key pmail-mode-map [menu-bar delete expunge/save]
  '("Expunge/Save" . pmail-expunge-and-save))

(define-key pmail-mode-map [menu-bar delete expunge]
  '("Expunge" . pmail-expunge))

(define-key pmail-mode-map [menu-bar delete undelete]
  '("Undelete" . pmail-undelete-previous-message))

(define-key pmail-mode-map [menu-bar delete delete]
  '("Delete" . pmail-delete-forward))

(define-key pmail-mode-map [menu-bar move]
  (cons "Move" (make-sparse-keymap "Move")))

(define-key pmail-mode-map [menu-bar move search-back]
  '("Search Back..." . pmail-search-backwards))

(define-key pmail-mode-map [menu-bar move search]
  '("Search..." . pmail-search))

(define-key pmail-mode-map [menu-bar move previous]
  '("Previous Nondeleted" . pmail-previous-undeleted-message))

(define-key pmail-mode-map [menu-bar move next]
  '("Next Nondeleted" . pmail-next-undeleted-message))

(define-key pmail-mode-map [menu-bar move last]
  '("Last" . pmail-last-message))

(define-key pmail-mode-map [menu-bar move first]
  '("First" . pmail-first-message))

(define-key pmail-mode-map [menu-bar move previous]
  '("Previous" . pmail-previous-message))

(define-key pmail-mode-map [menu-bar move next]
  '("Next" . pmail-next-message))

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
	(pmail-convert-file-maybe)
	(goto-char (point-max))
	(set-buffer-multibyte t)))
    (pmail-set-message-counters)
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

(defun pmail-generate-viewer-buffer ()
  "Return a newly created buffer suitable for viewing messages."
  (let ((suffix (file-name-nondirectory (or buffer-file-name (buffer-name)))))
    (generate-new-buffer (format " *message-viewer %s*" suffix))))

;; Set up the permanent locals associated with an Pmail file.
(defun pmail-perm-variables ()
  (make-local-variable 'pmail-last-label)
  (make-local-variable 'pmail-last-regexp)
  (make-local-variable 'pmail-deleted-vector)
  (make-local-variable 'pmail-buffer)
  (setq pmail-buffer (current-buffer))
  (make-local-variable 'pmail-view-buffer)
  (setq pmail-view-buffer (pmail-generate-viewer-buffer))
  (make-local-variable 'pmail-summary-buffer)
  (make-local-variable 'pmail-summary-vector)
  (make-local-variable 'pmail-current-message)
  (make-local-variable 'pmail-total-messages)
  (make-local-variable 'pmail-overlay-list)
  (setq pmail-overlay-list nil)
  (make-local-variable 'pmail-message-vector)
  (make-local-variable 'pmail-msgref-vector)
  (make-local-variable 'pmail-inbox-list)
  (setq pmail-inbox-list (pmail-parse-file-inboxes))
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
  (make-local-variable 'pmail-buffers-swapped-p)
  ;; this gets generated as needed
  (setq pmail-keywords nil))

;; Set up the non-permanent locals associated with Pmail mode.
(defun pmail-variables ()
  (make-local-variable 'save-buffer-coding-system)
  ;; If we don't already have a value for save-buffer-coding-system,
  ;; get it from buffer-file-coding-system, and clear that
  ;; because it should be determined in pmail-show-message.
  (unless save-buffer-coding-system
    (setq save-buffer-coding-system (or buffer-file-coding-system 'undecided))
    (setq buffer-file-coding-system nil))
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
  (set-buffer pmail-buffer)
  (let* ((revert-buffer-function (default-value 'revert-buffer-function))
	 (pmail-enable-multibyte enable-multibyte-characters)
	 ;; See similar code in `pmail'.
	 (coding-system-for-read (and pmail-enable-multibyte 'raw-text)))
    ;; Call our caller again, but this time it does the default thing.
    (when (revert-buffer arg noconfirm)
      ;; If the user said "yes", and we changed something,
      ;; reparse the messages.
      (set-buffer pmail-buffer)
      (pmail-mode-2)
      ;; Convert all or part to Babyl file if possible.
      (pmail-convert-file-maybe)
      ;; We have read the file as raw-text, so the buffer is set to
      ;; unibyte.  Make it multibyte if necessary.
      (if (and pmail-enable-multibyte
	       (not enable-multibyte-characters))
	  (set-buffer-multibyte t))
      (goto-char (point-max))
      (pmail-set-message-counters)
      (pmail-show-message pmail-total-messages)
      (run-hooks 'pmail-mode-hook))))

;; Return a list of files from this buffer's Mail: option.
;; Does not assume that messages have been parsed.
;; Just returns nil if buffer does not look like Babyl format.
(defun pmail-parse-file-inboxes ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (cond ((looking-at "BABYL OPTIONS:")
	     (search-forward "\n\^_" nil 'move)
	     (narrow-to-region 1 (point))
	     (goto-char 1)
	     (when (search-forward "\nMail:" nil t)
	       (narrow-to-region (point) (progn (end-of-line) (point)))
	       (goto-char (point-min))
	       (mail-parse-comma-list)))))))

(defun pmail-expunge-and-save ()
  "Expunge and save PMAIL file."
  (interactive)
  (pmail-expunge)
  (set-buffer pmail-buffer)
  (save-buffer)
  (if (pmail-summary-exists)
      (pmail-select-summary (set-buffer-modified-p nil))))

(defun pmail-quit ()
  "Quit out of PMAIL.
Hook `pmail-quit-hook' is run after expunging."
  (interactive)
  ;; Determine if the buffers need to be swapped.
  (pmail-swap-buffers-maybe)
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

(defun pmail-duplicate-message ()
  "Create a duplicated copy of the current message.
The duplicate copy goes into the Pmail file just after the
original copy."
  (interactive)
  (widen)
  (let ((buffer-read-only nil)
	(number pmail-current-message)
	(string (buffer-substring (pmail-msgbeg pmail-current-message)
				  (pmail-msgend pmail-current-message))))
    (goto-char (pmail-msgend pmail-current-message))
    (insert string)
    (pmail-forget-messages)
    (pmail-show-message number)
    (message "Message duplicated")))

;;;###autoload
(defun pmail-input (filename)
  "Run Pmail on file FILENAME."
  (interactive "FRun pmail on PMAIL file: ")
  (pmail filename))


;; This used to scan subdirectories recursively, but someone pointed out
;; that if the user wants that, person can put all the files in one dir.
;; And the recursive scan was slow.  So I took it out.
;; rms, Sep 1996.
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
				      'pmail-output-to-pmail-file))))

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

;; RLK feature not added in this version:
;; argument specifies inbox file or files in various ways.

(defun pmail-get-new-mail (&optional file-name)
  "Move any new mail from this PMAIL file's inbox files.
The inbox files can be specified with the file's Mail: option.  The
variable `pmail-primary-inbox-list' specifies the inboxes for your
primary PMAIL file if it has no Mail: option.  By default, this is
your /usr/spool/mail/$USER.

You can also specify the file to get new mail from.  In this case, the
file of new mail is not changed or deleted.  Noninteractively, you can
pass the inbox file name as an argument.  Interactively, a prefix
argument causes us to read a file name and use that file as the inbox.

If the variable `pmail-preserve-inbox' is non-nil, new mail will
always be left in inbox files rather than deleted.

This function runs `pmail-get-new-mail-hook' before saving the updated file.
It returns t if it got any new messages."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (run-hooks 'pmail-before-get-new-mail-hook)
  ;; If the disk file has been changed from under us,
  ;; revert to it before we get new mail.
  (or (verify-visited-file-modtime (current-buffer))
      (find-file (buffer-file-name)))
  (set-buffer pmail-buffer)
  (pmail-maybe-set-message-counters)
  (widen)
  ;; Get rid of all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (let ((all-files (if file-name (list file-name)
		     pmail-inbox-list))
	(pmail-enable-multibyte (default-value 'enable-multibyte-characters))
	found)
    (unwind-protect
	(progn
	  (while all-files
	    (let ((opoint (point))
		  (new-messages 0)
		  (rsf-number-of-spam 0)
		  (delete-files ())
		  ;; If buffer has not changed yet, and has not been saved yet,
		  ;; don't replace the old backup file now.
		  (make-backup-files (and make-backup-files (buffer-modified-p)))
		  (buffer-read-only nil)
		  ;; Don't make undo records for what we do in getting mail.
		  (buffer-undo-list t)
		  success
		  ;; Files to insert this time around.
		  files
		  ;; Last names of those files.
		  file-last-names)
	      ;; Pull files off all-files onto files
	      ;; as long as there is no name conflict.
	      ;; A conflict happens when two inbox file names
	      ;; have the same last component.
	      (while (and all-files
			  (not (member (file-name-nondirectory (car all-files))
				       file-last-names)))
		(setq files (cons (car all-files) files)
		      file-last-names
		      (cons (file-name-nondirectory (car all-files)) files))
		(setq all-files (cdr all-files)))
	      ;; Put them back in their original order.
	      (setq files (nreverse files))

	      (goto-char (point-max))
	      (skip-chars-backward " \t\n") ; just in case of brain damage
	      (delete-region (point) (point-max)) ; caused by require-final-newline
	      (save-excursion
		(save-restriction
		  (narrow-to-region (point) (point))
		  ;; Read in the contents of the inbox files,
		  ;; renaming them as necessary,
		  ;; and adding to the list of files to delete eventually.
		  (if file-name
		      (pmail-insert-inbox-text files nil)
		    (setq delete-files (pmail-insert-inbox-text files t)))
		  ;; Scan the new text and convert each message to mbox format.
		  (goto-char (point-min))
		  (unwind-protect
		      (save-excursion
			(setq new-messages (pmail-add-babyl-headers)
			      success t))
		    ;; Try to delete the garbage just inserted.
		    (or success (delete-region (point-min) (point-max)))
		    ;; If we could not convert the file's inboxes,
		    ;; rename the files we tried to read
		    ;; so we won't over and over again.
		    (if (and (not file-name) (not success))
			(let ((delfiles delete-files)
			      (count 0))
			  (while delfiles
			    (while (file-exists-p (format "PMAILOSE.%d" count))
			      (setq count (1+ count)))
			    (rename-file (car delfiles)
					 (format "PMAILOSE.%d" count))
			    (setq delfiles (cdr delfiles))))))
		  (or (zerop new-messages)
		      (let (success)
			(goto-char (point-min))
			(pmail-count-new-messages)
			(run-hooks 'pmail-get-new-mail-hook)
			(save-buffer)))
		  ;; Delete the old files, now that babyl file is saved.
		  (while delete-files
		    (condition-case ()
			;; First, try deleting.
			(condition-case ()
			    (delete-file (car delete-files))
			  (file-error
			   ;; If we can't delete it, truncate it.
			   (write-region (point) (point) (car delete-files))))
		      (file-error nil))
		    (setq delete-files (cdr delete-files)))))
	      (if (= new-messages 0)
		  (progn (goto-char opoint)
			 (if (or file-name pmail-inbox-list)
			     (message "(No new mail has arrived)")))
		;; check new messages to see if any of them is spam:
		(if (and (featurep 'pmail-spam-filter)
			 pmail-use-spam-filter)
		    (let*
			((old-messages (- pmail-total-messages new-messages))
                         (rsf-scanned-message-number (1+ old-messages))
                         ;; save deletion flags of old messages: vector starts
                         ;; at zero (is one longer that no of messages),
                         ;; therefore take 1+ old-messages
                         (save-deleted
                          (substring pmail-deleted-vector 0 (1+
                          old-messages))))
                      ;; set all messages to undeleted
                      (setq pmail-deleted-vector
                            (make-string (1+ pmail-total-messages) ?\ ))
		      (while (<= rsf-scanned-message-number
		      pmail-total-messages)
			(progn
			  (if (not (pmail-spam-filter rsf-scanned-message-number))
			      (progn (setq rsf-number-of-spam (1+ rsf-number-of-spam)))
			    )
			  (setq rsf-scanned-message-number (1+ rsf-scanned-message-number))
			  ))
		      (if (> rsf-number-of-spam 0)
			  (progn
			    (when (pmail-expunge-confirmed)
                              (pmail-only-expunge t))
                            ))
                      (setq pmail-deleted-vector
                            (concat
                             save-deleted
                             (make-string (- pmail-total-messages old-messages)
                                          ?\ )))
		      ))
 		(if (pmail-summary-exists)
		    (pmail-select-summary
		     (pmail-update-summary)))
		(message "%d new message%s read%s"
			 new-messages (if (= 1 new-messages) "" "s")
			 ;; print out a message on number of spam messages found:
			 (if (and (featurep 'pmail-spam-filter)
				  pmail-use-spam-filter
				  (> rsf-number-of-spam 0))
			     (cond ((= 1 new-messages)
				    ", and appears to be spam")
				   ((= rsf-number-of-spam new-messages)
				    ", and all appear to be spam")
				   ((> rsf-number-of-spam 1)
				    (format ", and %d appear to be spam"
					    rsf-number-of-spam))
				   (t
				    ", and 1 appears to be spam"))
			   ""))
		(when (and (featurep 'pmail-spam-filter)
			   pmail-use-spam-filter
			   (> rsf-number-of-spam 0))
		  (if rsf-beep (beep t))
		  (sleep-for rsf-sleep-after-message))

		;; Move to the first new message
		;; unless we have other unseen messages before it.
		(pmail-show-message (pmail-first-unseen-message))
		(run-hooks 'pmail-after-get-new-mail-hook)
		(setq found t))))
	  found)
      ;; Don't leave the buffer screwed up if we get a disk-full error.
      (or found (pmail-show-message)))))

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
				       (string-equal proto "imap"))))
	  ;; The password is embedded.  Strip it out since movemail
	  ;; does not really like it, in spite of the movemail spec.
	  (setq file (concat proto "://" user "@" host)))
 
	(if (pmail-movemail-variant-p 'emacs)
	    (if (string-equal proto "pop")
		(list (concat "po:" user ":" host)
		      t
		      (or pass supplied-password)
		      got-password)
	      (error "Emacs movemail does not support %s protocol" proto))
	  (list file
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
      (when (not popmail)
	;; On some systems, /usr/spool/mail/foo is a directory
	;; and the actual inbox is /usr/spool/mail/foo/foo.
	(if (file-directory-p file)
	    (setq file (expand-file-name (user-login-name)
					 file))))
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
	    ;; Determine if a pair of newline message separators need
	    ;; to be added to the new collection of messages.  This is
	    ;; the case for all new message collections added to a
	    ;; non-empty mail file.
	    (unless (zerop size)
	      (save-restriction
		(let ((start (point-min)))
		  (widen)
		  (unless (eq start (point-min))
		    (goto-char start)
		    (insert "\n\n")
		    (setq size (+ 2 size))))))
	    (goto-char (point-max))
	    (or (= (preceding-char) ?\n)
		(zerop size)
		(insert ?\n))
	    (if (not (and pmail-preserve-inbox (string= file tofile)))
		(setq delete-files (cons tofile delete-files)))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;; Decode the region specified by FROM and TO by CODING.
;; If CODING is nil or an invalid coding system, decode by `undecided'.
(defun pmail-decode-region (from to coding)
  (if (or (not coding) (not (coding-system-p coding)))
      (setq coding 'undecided))
  ;; Use -dos decoding, to remove ^M characters left from base64 or
  ;; rogue qp-encoded text.
  (decode-coding-region from to
			(coding-system-change-eol-conversion coding 1))
  ;; Don't reveal the fact we used -dos decoding, as users generally
  ;; will not expect the PMAIL buffer to use DOS EOL format.
  (setq buffer-file-coding-system
	(setq last-coding-system-used
	      (coding-system-change-eol-conversion coding 0))))

(defun pmail-add-babyl-headers ()
  "Validate the RFC2822 format for the new messages.  Point, at
entry should be looking at the first new message.  An error will
be thrown if the new messages are not RCC2822 compliant.  Lastly,
unless one already exists, add an Rmail attribute header to the
new messages in the region "
  (let ((count 0)
	(start (point))
	limit)
    ;; Detect an empty inbox file.
    (unless (= start (point-max))
      ;; Scan the new messages to establish a count and to insure that
      ;; an attribute header is present.
      (while (looking-at "From ")
	;; Determine if a new attribute header needs to be added to
	;; the message.
	(if (search-forward "\n\n" nil t)
	    (progn
	      (setq count (1+ count))
	      (forward-char -1)
	      (narrow-to-region start (point))
	      (unless (mail-fetch-field pmail-attribute-header)
		(insert pmail-attribute-header ": ------U\n"))
	      (widen))
	  (error "Invalid mbox format detected in inbox file"))
	;; Move to the next message.
	(if (search-forward "\n\nFrom " nil 'move)
	    (forward-char -5))
	(setq start (point))))
    count))

;; the  pmail-break-forwarded-messages  feature is not implemented
(defun pmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil)
	(buffer-undo-list t)
	(invalid-input-resync
	 (function (lambda ()
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 3)
		     ;; Try to get back in sync with a real message.
		     (if (re-search-forward
			  (concat pmail-mmdf-delim1 "\\|^From") nil t)
			 (beginning-of-line)
		       (goto-char (point-max)))))))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(setq start (point))
	(cond ((looking-at "BABYL OPTIONS:")	;Babyl header
	       (if (search-forward "\n\^_" nil t)
		   ;; If we find the proper terminator, delete through there.
		   (delete-region (point-min) (point))
		 (funcall invalid-input-resync)
		 (delete-region (point-min) (point))))
	      ;; Babyl format message
	      ((looking-at "\^L")
	       (or (search-forward "\n\^_" nil t)
		   (funcall invalid-input-resync))
	       (setq count (1+ count))
	       ;; Make sure there is no extra white space after the ^_
	       ;; at the end of the message.
	       ;; Narrowing will make sure that whatever follows the junk
	       ;; will be treated properly.
	       (delete-region (point)
			      (save-excursion
				(skip-chars-forward " \t\n")
				(point)))
	       ;; The following let* form was wrapped in a `save-excursion'
	       ;; which in one case caused infinite looping, see:
	       ;; http://lists.gnu.org/archive/html/emacs-devel/2008-01/msg00968.html
	       ;; Removing that form leaves `point' at the end of the
	       ;; region decoded by `pmail-decode-region' which should
	       ;; be correct.
	       (let* ((header-end
		       (progn
			 (save-excursion
			   (goto-char start)
			   (forward-line 1)
			   (if (looking-at "0")
			       (forward-line 1)
			     (forward-line 2))
			   (save-restriction
			     (narrow-to-region (point) (point-max))
			     (rfc822-goto-eoh)
			     (point)))))
		      (case-fold-search t)
		      (quoted-printable-header-field-end
		       (save-excursion
			 (goto-char start)
			 (re-search-forward
			  "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
			  header-end t)))
		      (base64-header-field-end
		       (save-excursion
			 (goto-char start)
			 ;; Don't try to decode non-text data.
			 (and (re-search-forward
			       "^content-type:\\(\n?[\t ]\\)\\(text\\|message\\)/"
			       header-end t)
			      (goto-char start)
			      (re-search-forward
			       "^content-transfer-encoding:\\(\n?[\t ]\\)*base64\\(\n?[\t ]\\)*"
			       header-end t)))))
		 (if quoted-printable-header-field-end
		     (save-excursion
		       (unless
			   (mail-unquote-printable-region header-end (point) nil t t)
			 (message "Malformed MIME quoted-printable message"))
		       ;; Change "quoted-printable" to "8bit",
		       ;; to reflect the decoding we just did.
		       (goto-char quoted-printable-header-field-end)
		       (delete-region (point) (search-backward ":"))
		       (insert ": 8bit")))
		 (if base64-header-field-end
		     (save-excursion
		       (when
			   (condition-case nil
			       (progn
				 (base64-decode-region (1+ header-end)
						       (- (point) 2))
				 t)
			     (error nil))
			 ;; Change "base64" to "8bit", to reflect the
			 ;; decoding we just did.
			 (goto-char base64-header-field-end)
			 (delete-region (point) (search-backward ":"))
			 (insert ": 8bit"))))
		 (setq last-coding-system-used nil)
		 (or pmail-enable-mime
		     (not pmail-enable-multibyte)
		     (let ((mime-charset
			    (if (and pmail-decode-mime-charset
				     (save-excursion
				       (goto-char start)
				       (search-forward "\n\n" nil t)
				       (let ((case-fold-search t))
					 (re-search-backward
					  pmail-mime-charset-pattern
					  start t))))
				(intern (downcase (match-string 1))))))
		       (pmail-decode-region start (point) mime-charset))))
	       ;; Add an X-Coding-System: header if we don't have one.
	       (save-excursion
		 (goto-char start)
		 (forward-line 1)
		 (if (looking-at "0")
		     (forward-line 1)
		   (forward-line 2))
		 (or (save-restriction
		       (narrow-to-region (point) (point-max))
		       (rfc822-goto-eoh)
		       (goto-char (point-min))
		       (re-search-forward "^X-Coding-System:" nil t))
		     (insert "X-Coding-System: "
			     (symbol-name last-coding-system-used)
			     "\n")))
	       (narrow-to-region (point) (point-max))
	       (and (= 0 (% count 10))
		    (message "Converting to Babyl format...%d" count)))
	      ;;*** MMDF format
	      ((let ((case-fold-search t))
		 (looking-at pmail-mmdf-delim1))
	       (let ((case-fold-search t))
		 (replace-match "\^L\n0, unseen,,\n*** EOOH ***\n")
		 (re-search-forward pmail-mmdf-delim2 nil t)
		 (replace-match "\^_"))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t) ; single char "\^_"
		     (replace-match "\n^_"))))	; 2 chars: "^" and "_"
	       (setq last-coding-system-used nil)
	       (or pmail-enable-mime
		   (not pmail-enable-multibyte)
		   (decode-coding-region start (point) 'undecided))
	       (save-excursion
		 (goto-char start)
		 (forward-line 3)
		 (insert "X-Coding-System: "
			 (symbol-name last-coding-system-used)
			 "\n"))
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count))
	       (and (= 0 (% count 10))
		    (message "Converting to Babyl format...%d" count)))
	      ;;*** Mail format
	      ((looking-at "^From ")
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (pmail-nuke-pinhead-header)
	       ;; If this message has a Content-Length field,
	       ;; skip to the end of the contents.
	       (let* ((header-end (save-excursion
				    (and (re-search-forward "\n\n" nil t)
					 (1- (point)))))
		      (case-fold-search t)
		      (quoted-printable-header-field-end
		       (save-excursion
			 (re-search-forward
			  "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
			  header-end t)))
		      (base64-header-field-end
		       (and
			;; Don't decode non-text data.
			(save-excursion
			  (re-search-forward
			   "^content-type:\\(\n?[\t ]\\)\\(text\\|message\\)/"
			   header-end t))
			(save-excursion
			  (re-search-forward
			   "^content-transfer-encoding:\\(\n?[\t ]\\)*base64\\(\n?[\t ]\\)*"
			   header-end t))))
		      (size
		       ;; Get the numeric value from the Content-Length field.
		       (save-excursion
			 ;; Back up to end of prev line,
			 ;; in case the Content-Length field comes first.
			 (forward-char -1)
			 (and (search-forward "\ncontent-length: "
					      header-end t)
			      (let ((beg (point))
				    (eol (progn (end-of-line) (point))))
				(string-to-number (buffer-substring beg eol)))))))
		 (and size
		      (if (and (natnump size)
			       (<= (+ header-end size) (point-max))
			       ;; Make sure this would put us at a position
			       ;; that we could continue from.
			       (save-excursion
				 (goto-char (+ header-end size))
				 (skip-chars-forward "\n")
				 (or (eobp)
				     (and (looking-at "BABYL OPTIONS:")
					  (search-forward "\n\^_" nil t))
				     (and (looking-at "\^L")
					  (search-forward "\n\^_" nil t))
				     (let ((case-fold-search t))
				       (looking-at pmail-mmdf-delim1))
				     (looking-at "From "))))
			  (goto-char (+ header-end size))
			(message "Ignoring invalid Content-Length field")
			(sit-for 1 0 t)))
		 (if (let ((case-fold-search nil))
		       (re-search-forward
			(concat "^[\^_]?\\("
				pmail-unix-mail-delimiter
				"\\|"
				pmail-mmdf-delim1 "\\|"
				"^BABYL OPTIONS:\\|"
				"\^L\n[01],\\)") nil t))
		     (goto-char (match-beginning 1))
		   (goto-char (point-max)))
		 (setq count (1+ count))
		 (if quoted-printable-header-field-end
		     (save-excursion
		       (unless
			   (mail-unquote-printable-region header-end (point) nil t t)
			 (message "Malformed MIME quoted-printable message"))
		       ;; Change "quoted-printable" to "8bit",
		       ;; to reflect the decoding we just did.
		       (goto-char quoted-printable-header-field-end)
		       (delete-region (point) (search-backward ":"))
		       (insert ": 8bit")))
		 (if base64-header-field-end
		     (save-excursion
		       (when
			   (condition-case nil
			       (progn
				 (base64-decode-region
				  (1+ header-end)
				  (save-excursion
				    ;; Prevent base64-decode-region
				    ;; from removing newline characters.
				    (skip-chars-backward "\n\t ")
				    (point)))
				 t)
			     (error nil))
			 ;; Change "base64" to "8bit", to reflect the
			 ;; decoding we just did.
			 (goto-char base64-header-field-end)
			 (delete-region (point) (search-backward ":"))
			 (insert ": 8bit")))))

	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t) ; single char
		     (replace-match "\n^_"))))	; 2 chars: "^" and "_"
	       ;; This is for malformed messages that don't end in newline.
	       ;; There shouldn't be any, but some users say occasionally
	       ;; there are some.
 	       (or (bolp) (newline))
	       (insert ?\^_)
	       (setq last-coding-system-used nil)
	       (or pmail-enable-mime
		   (not pmail-enable-multibyte)
		   (let ((mime-charset
			  (if (and pmail-decode-mime-charset
				   (save-excursion
				     (goto-char start)
				     (search-forward "\n\n" nil t)
				     (let ((case-fold-search t))
				       (re-search-backward
					pmail-mime-charset-pattern
					start t))))
			      (intern (downcase (match-string 1))))))
		     (pmail-decode-region start (point) mime-charset)))
	       (save-excursion
		 (goto-char start)
		 (forward-line 3)
		 (insert "X-Coding-System: "
			 (symbol-name last-coding-system-used)
			 "\n"))
	       (narrow-to-region (point) (point-max))
	       (and (= 0 (% count 10))
		    (message "Converting to Babyl format...%d" count)))
	      ;;
	      ;; This kludge is because some versions of sendmail.el
	      ;; insert an extra newline at the beginning that shouldn't
	      ;; be there.  sendmail.el has been fixed, but old versions
	      ;; may still be in use.  -- rms, 7 May 1993.
	      ((eolp) (delete-char 1))
	      (t (error "Cannot convert to babyl format")))))
    (setq buffer-undo-list nil)
    count))

;; Delete the "From ..." line, creating various other headers with
;; information from it if they don't already exist.  Now puts the
;; original line into a mail-from: header line for debugging and for
;; use by the pmail-output function.
(defun pmail-nuke-pinhead-header ()
  (save-excursion
    (save-restriction
      (let ((start (point))
  	    (end (progn
		   (condition-case ()
		       (search-forward "\n\n")
		     (error
		      (goto-char (point-max))
		      (insert "\n\n")))
		   (point)))
	    has-from has-date)
	(narrow-to-region start end)
	(let ((case-fold-search t))
	  (goto-char start)
	  (setq has-from (search-forward "\nFrom:" nil t))
	  (goto-char start)
	  (setq has-date (and (search-forward "\nDate:" nil t) (point)))
	  (goto-char start))
	(let ((case-fold-search nil))
	  (if (re-search-forward (concat "^" pmail-unix-mail-delimiter) nil t)
	      (replace-match
		(concat
		  "Mail-from: \\&"
		  ;; Keep and reformat the date if we don't
		  ;;  have a Date: field.
		  (if has-date
		      ""
		    (concat
		     "Date: \\2, \\4 \\3 \\9 \\5 "

		     ;; The timezone could be matched by group 7 or group 10.
		     ;; If neither of them matched, assume EST, since only
		     ;; Easterners would be so sloppy.
		     ;; It's a shame the substitution can't use "\\10".
		     (cond
		      ((/= (match-beginning 7) (match-end 7)) "\\7")
		      ((/= (match-beginning 10) (match-end 10))
		       (buffer-substring (match-beginning 10)
					 (match-end 10)))
		      (t "EST"))
		     "\n"))
		  ;; Keep and reformat the sender if we don't
		  ;; have a From: field.
		  (if has-from
		      ""
		    "From: \\1\n"))
		t)))))))

;;;; *** Pmail Message Formatting and Header Manipulation ***

(defun pmail-copy-headers (beg end &optional ignored-headers)
  "Copy displayed header fields to the message viewer buffer.
BEG and END marks the start and end positions of the message in
the mail buffer.  If the optional argument IGNORED-HEADERS is
non-nil, ignore all header fields whose names match that regexp.
Otherwise, if `rmail-displayed-headers' is non-nil, copy only
those header fields whose names match that regexp.  Otherwise,
copy all header fields whose names do not match
`rmail-ignored-headers' (unless they also match
`rmail-nonignored-headers')."
  (let ((result "")
	(header-start-regexp "\n[^ \t]")
	lim)
    (with-current-buffer pmail-buffer
      (when (search-forward "\n\n" nil t)
	(forward-char -1)
	(save-restriction
	  ;; Put point right after the From header line.
	  (narrow-to-region beg (point))
	  (goto-char (point-min))
	  (unless (re-search-forward header-start-regexp nil t)
	    (error "Invalid mbox format; no header follows the From message separator."))
	  (forward-char -1)
	  (cond
	   ;; Handle the case where all headers should be copied.
	   ((eq pmail-header-style 'full)
	    (setq result (buffer-substring beg (point-max))))
	   ;; Handle the case where the headers matching the diplayed
	   ;; headers regexp should be copied.
	   ((and pmail-displayed-headers (null ignored-headers))
	    (while (not (eobp))
	      (save-excursion
		(setq lim (if (re-search-forward header-start-regexp nil t)
			      (1+ (match-beginning 0))
			    (point-max))))
	      (when (looking-at pmail-displayed-headers)
		(setq result (concat result (buffer-substring (point) lim))))
	      (goto-char lim)))
	   ;; Handle the ignored headers.
	   ((or ignored-headers (setq ignored-headers pmail-ignored-headers))
	    (while (and ignored-headers (not (eobp)))
	      (save-excursion
		(setq lim (if (re-search-forward header-start-regexp nil t)
			      (1+ (match-beginning 0))
			    (point-max))))
	      (if (and (looking-at ignored-headers)
		       (not (looking-at pmail-nonignored-headers)))
		  (goto-char lim)
		(setq result (concat result (buffer-substring (point) lim)))
		(goto-char lim))))
	   (t (error "No headers selected for display!"))))))
    result))

(defun pmail-copy-body (beg end)
  "Return the message body to be displayed in the view buffer.
BEG and END marks the start and end positions of the message in
the mail buffer."
  (with-current-buffer pmail-buffer
    (if (search-forward "\n\n" nil t)
	(buffer-substring (point) end)
      (error "Invalid message format: no header/body separator"))))

(defun pmail-toggle-header (&optional arg)
  "Show original message header if pruned header currently shown, or vice versa.
With argument ARG, show the message header pruned if ARG is greater than zero;
otherwise, show it in full."
  (interactive "P")
  (setq pmail-header-style
	(cond
	 ((and (numberp arg) (> arg 0)) 'normal)
	 ((eq pmail-header-style 'full) 'normal)
	 (t 'full)))
  (pmail-show-message))

;; Lifted from repos-count-screen-lines.
;; Return number of screen lines between START and END.
(defun pmail-count-screen-lines (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (vertical-motion (- (point-max) (point-min))))))

;;;; *** Pmail Attributes and Keywords ***

(defun pmail-get-header (name &optional msg)
  "Return the value of message header NAME, nil if no such header
exists.  MSG, if set identifies the message number to use.  The
current mail message will be used otherwise."
  (save-excursion
    (save-restriction
      (with-current-buffer pmail-buffer
	(widen)
	(let* ((n (or msg pmail-current-message))
	       (beg (pmail-msgbeg n))
	       end)
	  (goto-char beg)
	  (setq end (search-forward "\n\n" nil t))
	  (if end
	      (progn
		(narrow-to-region beg end)
		(mail-fetch-field name))
	    (error "Invalid mbox format encountered.")))))))
  
(defun pmail-get-attr-names (&optional msg)
  "Return the message attributes in a comma separated string.
MSG, if set identifies the message number to use.  The current
mail message will be used otherwise."
  (let ((value (pmail-get-header pmail-attribute-field-name msg))
	result temp)
    (dotimes (index (length value))
      (setq temp (and (not (= ?- (aref value index)))
		      (nth 1 (aref pmail-attr-array index)))
	    result
	    (cond
	     ((and temp result) (format "%s, %s" result temp))
	     (temp temp)
	     (t result))))
    result))

(defun pmail-get-keywords (&optional msg)
  "Return the message keywords in a comma separated string.
MSG, if set identifies the message number to use.  The current
mail message will be used otherwise."
  (pmail-get-header pmail-keyword-header msg))

(defun pmail-display-labels ()
  "Update the mode line with the (set) attributes and keywords
for the current message."
  (let (blurb attr-names keywords)
    ;; Combine the message attributes and keywords into a comma
    ;; separated list.
    (setq attr-names (pmail-get-attr-names pmail-current-message)
	  keywords (pmail-get-keywords pmail-current-message))
    (setq blurb
	  (cond
	   ((and attr-names keywords) (concat attr-names ", " keywords))
	   (attr-names attr-names)
	   (keywords keywords)
	   (t "")))
    (setq mode-line-process
	  (format " %d/%d%s"
		  pmail-current-message pmail-total-messages blurb))
    ;; If pmail-enable-mime is non-nil, we may have to update
    ;; `mode-line-process' of pmail-view-buffer too.
    (if (and pmail-enable-mime
	     (not (eq (current-buffer) pmail-view-buffer))
	     (buffer-live-p pmail-view-buffer))
	(let ((mlp mode-line-process))
	  (with-current-buffer pmail-view-buffer
	    (setq mode-line-process mlp))))))

(defun pmail-get-attr-value (attr state)
  "Return the character value for ATTR.
ATTR is a (numberic) index, an offset into the mbox attribute
header value. STATE is one of nil, t, or a character value."
  (cond
   ((numberp state) state)
   ((not state) ?-)
   (t (nth 0 (aref pmail-attr-array attr)))))

(defun pmail-set-attribute (attr state &optional msgnum)
  "Turn an attribute of a message on or off according to STATE.
STATE is either nil or the character (numeric) value associated
with the state (nil represents off and non-nil represents on).
ATTR is the index of the attribute.  MSGNUM is message number to
change; nil means current message."
  (set-buffer pmail-buffer)
  (let ((value (pmail-get-attr-value attr state))
	(omax (point-max-marker))
	(omin (point-min-marker))
	(buffer-read-only nil)
	limit)
    (or msgnum (setq msgnum pmail-current-message))
    (if (> msgnum 0)
	(unwind-protect
	    (save-excursion
	      ;; Determine if the current state is the desired state.
	      (widen)
	      (goto-char (pmail-msgbeg msgnum))
	      (save-excursion
		(setq limit (search-forward "\n\n" nil t)))
	      (when (search-forward (concat pmail-attribute-header ": ") limit t)
		(forward-char attr)
		(when (/= value (char-after))
		  (delete-char 1)
		  (insert value)))
	      (if (= attr pmail-deleted-attr-index)
		  (pmail-set-message-deleted-p msgnum state)))
	  ;; Note: we don't use save-restriction because that does not work right
	  ;; if changes are made outside the saved restriction
	  ;; before that restriction is restored.
	  (narrow-to-region omin omax)
	  (set-marker omin nil)
	  (set-marker omax nil)
	  (if (= msgnum pmail-current-message)
	      (pmail-display-labels))))))

(defun pmail-message-attr-p (msg attrs)
  "Return t if the attributes header for message MSG contains a
match for the regexp ATTRS."
  (save-excursion
    (save-restriction
      (let ((start (pmail-msgbeg msg))
	    limit)
	(widen)
	(goto-char start)
	(setq limit (search-forward "\n\n" (pmail-msgend msg) t))
	(goto-char start)
	(and limit
	     (search-forward (concat pmail-attribute-header ": ") limit t)
	     (looking-at attrs))))))

;;;; *** Pmail Message Selection And Support ***

(defun pmail-msgend (n)
  (marker-position (aref pmail-message-vector (1+ n))))

(defun pmail-msgbeg (n)
  (marker-position (aref pmail-message-vector n)))

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
	  (narrow-to-region (pmail-msgbeg pmail-current-message)
			    (point-max))
	  (goto-char (point-min))
	  (funcall function))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
      (narrow-to-region (pmail-msgbeg pmail-current-message)
			(pmail-msgend pmail-current-message)))))

(defun pmail-forget-messages ()
  (unwind-protect
      (if (vectorp pmail-message-vector)
	  (let* ((i 0)
		 (v pmail-message-vector)
		 (n (length v)))
	    (while (< i n)
	      (move-marker (aref v i)  nil)
	      (setq i (1+ i)))))
    (setq pmail-message-vector nil)
    (setq pmail-msgref-vector nil)
    (setq pmail-deleted-vector nil)))

(defun pmail-maybe-set-message-counters ()
  (if (not (and pmail-deleted-vector
		pmail-message-vector
		pmail-current-message
		pmail-total-messages))
      (pmail-set-message-counters)))

(defun pmail-count-new-messages (&optional nomsg)
  "Count the number of new messages in the region.
Output a helpful message unless NOMSG is non-nil."
  (let* ((case-fold-search nil)
	 (total-messages 0)
	 (messages-head nil)
	 (deleted-head nil))
    (or nomsg (message "Counting new messages..."))
    (goto-char (point-max))
    ;; Put at the end of messages-head
    ;; the entry for message N+1, which marks
    ;; the end of message N.  (N = number of messages).
    (setq messages-head (list (point-marker)))
    (pmail-set-message-counters-counter (point-min))
    (setq pmail-current-message (1+ pmail-total-messages))
    (setq pmail-total-messages
	  (+ pmail-total-messages total-messages))
    (setq pmail-message-vector
	  (vconcat pmail-message-vector (cdr messages-head)))
    (aset pmail-message-vector
	  pmail-current-message (car messages-head))
    (setq pmail-deleted-vector
	  (concat pmail-deleted-vector deleted-head))
    (setq pmail-summary-vector
	  (vconcat pmail-summary-vector (make-vector total-messages nil)))
    (setq pmail-msgref-vector
	  (vconcat pmail-msgref-vector (make-vector total-messages nil)))
    ;; Fill in the new elements of pmail-msgref-vector.
    (let ((i (1+ (- pmail-total-messages total-messages))))
      (while (<= i pmail-total-messages)
	(aset pmail-msgref-vector i (list i))
	(setq i (1+ i))))
    (goto-char (point-min))
    (or nomsg (message "Counting new messages...done (%d)" total-messages))))

(defun pmail-set-message-counters ()
  (pmail-forget-messages)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((point-save (point))
	     (total-messages 0)
	     (messages-after-point)
	     (case-fold-search nil)
	     (messages-head nil)
	     (deleted-head nil))
	;; Determine how many messages follow point.
	(message "Counting messages...")
	(goto-char (point-max))
	;; Put at the end of messages-head
	;; the entry for message N+1, which marks
	;; the end of message N.  (N = number of messages).
	(setq messages-head (list (point-marker)))
	(pmail-set-message-counters-counter (min (point) point-save))
	(setq messages-after-point total-messages)

	;; Determine how many precede point.
	(pmail-set-message-counters-counter)
	(setq pmail-total-messages total-messages)
	(setq pmail-current-message
	      (min total-messages
		   (max 1 (- total-messages messages-after-point))))
	(setq pmail-message-vector
	      (apply 'vector (cons (point-min-marker) messages-head))
	      pmail-deleted-vector (concat "0" deleted-head)
	      pmail-summary-vector (make-vector pmail-total-messages nil)
	      pmail-msgref-vector (make-vector (1+ pmail-total-messages) nil))
	(let ((i 0))
	  (while (<= i pmail-total-messages)
	    (aset pmail-msgref-vector i (list i))
	    (setq i (1+ i))))
	(message "Counting messages...done")))))


(defsubst pmail-collect-deleted (message-end)
  "Collect the message deletion flags for each message.
MESSAGE-END is the buffer position corresponding to the end of
the message.  Point is at the beginning of the message."
  ;; NOTE: This piece of code will be executed on a per-message basis.
  ;; In the face of thousands of messages, it has to be as fast as
  ;; possible, hence some brute force constant use is employed in
  ;; addition to inlining.
  (save-excursion
    (setq deleted-head
	  (cons (if (and (search-forward "X-BABYL-V6-ATTRIBUTES: " message-end t)
			 (looking-at "?D"))
		    ?D
		  ?\ ) deleted-head))))

(defun pmail-set-message-counters-counter (&optional stop)
  ;; Collect the start position for each message into 'messages-head.
  (let ((start (point)))
    (while (search-backward "\n\nFrom " stop t)
      (forward-char 2)
      (pmail-collect-deleted start)
      ;; Show progress after every 20 messages or so.
      (setq messages-head (cons (point-marker) messages-head)
	    total-messages (1+ total-messages)
	    start (point))
      (if (zerop (% total-messages 20))
	  (message "Counting messages...%d" total-messages)))
    ;; Handle the first message, maybe.
    (if stop
	(goto-char stop)
      (goto-char (point-min)))
    (unless (not (looking-at "From "))
      (pmail-collect-deleted start)
      (setq messages-head (cons (point-marker) messages-head)
	    total-messages (1+ total-messages)))))

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
     (let ((mail-followup-to (mail-fetch-field "mail-followup-to" nil t)))
       (when mail-followup-to
	 (let ((addresses
		(split-string
		 (mail-strip-quoted-names mail-followup-to)
		 ",[[:space:]]+" t)))
	   (dolist (addr addresses)
	     (when (and (not (member addr mail-mailing-lists))
			(not
			 ;; taken from pmailsum.el
			 (string-match
			  (or pmail-user-mail-address-regexp
			      (concat "^\\("
				      (regexp-quote (user-login-name))
				      "\\($\\|@\\)\\|"
				      (regexp-quote
				       (or user-mail-address
					   (concat (user-login-name) "@"
						   (or mail-host-address
						       (system-name)))))
				      "\\>\\)"))
			  addr))
			(y-or-n-p
			 (format "Add `%s' to `mail-mailing-lists'? "
				 addr)))
	       (customize-save-variable 'mail-mailing-lists
					(cons addr mail-mailing-lists)))))))))

(defun pmail-swap-buffers-maybe ()
  "Determine if the Pmail buffer is showing a message.
If so restore the actual mbox message collection."
  (unless (not pmail-buffers-swapped-p)
    (with-current-buffer pmail-buffer
      (buffer-swap-text pmail-view-buffer)
      (setq pmail-buffers-swapped-p nil))))

(defun pmail-show-message (&optional n no-summary)
  "Show message number N (prefix argument), counting from start of file.
If summary buffer is currently displayed, update current message there also."
  (interactive "p")
  (or (eq major-mode 'pmail-mode)
      (switch-to-buffer pmail-buffer))
  (pmail-swap-buffers-maybe)
  (pmail-maybe-set-message-counters)
  (widen)
  (let (blurb)
    (if (zerop pmail-total-messages)
	(save-excursion
	  (with-current-buffer pmail-view-buffer
	    (erase-buffer)
	    (setq blurb "No mail.")))
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
      (let ((buf pmail-buffer)
	    (beg (pmail-msgbeg n))
	    (end (pmail-msgend n))
	    headers body)
	(goto-char beg)
	(setq headers (pmail-copy-headers beg end)
	      body (pmail-copy-body beg end))
	(pmail-set-attribute pmail-unseen-attr-index nil)
	(with-current-buffer pmail-view-buffer
	  (erase-buffer)
	  (insert headers "\n")
	  (pmail-highlight-headers)
	  (insert body)
	  (goto-char (point-min)))))
    (when mail-mailing-lists
      (pmail-unknown-mail-followup-to))
    (if transient-mark-mode (deactivate-mark))
    (pmail-display-labels)
    (buffer-swap-text pmail-view-buffer)
    (setq pmail-buffers-swapped-p t)
    (run-hooks 'pmail-show-message-hook)
    ;; If there is a summary buffer, try to move to this message
    ;; in that buffer.  But don't complain if this message
    ;; is not mentioned in the summary.
    ;; Don't do this at all if we were called on behalf
    ;; of cursor motion in the summary buffer.
    (and (pmail-summary-exists) (not no-summary)
	 (let ((curr-msg pmail-current-message))
	   (pmail-select-summary
	    (pmail-summary-goto-msg curr-msg t t))))
    (with-current-buffer pmail-buffer
      (pmail-auto-file))
    (if blurb
	(message blurb))))

;; Find all occurrences of certain fields, and highlight them.
(defun pmail-highlight-headers ()
  ;; Do this only if the system supports faces.
  (if (and (fboundp 'internal-find-face)
	   pmail-highlighted-headers)
      (save-excursion
	(search-forward "\n\n" nil 'move)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (let ((case-fold-search t)
		(inhibit-read-only t)
		;; Highlight with boldface if that is available.
		;; Otherwise use the `highlight' face.
		(face (or 'pmail-highlight
			  (if (face-differs-from-default-p 'bold)
			      'bold 'highlight)))
		;; List of overlays to reuse.
		(overlays pmail-overlay-list))
	    (goto-char (point-min))
	    (while (re-search-forward pmail-highlighted-headers nil t)
	      (skip-chars-forward " \t")
	      (let ((beg (point))
		    overlay)
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		;; Back up over newline, then trailing spaces or tabs
		(forward-char -1)
		(while (member (preceding-char) '(?  ?\t))
		  (forward-char -1))
		(if overlays
		    ;; Reuse an overlay we already have.
		    (progn
		      (setq overlay (car overlays)
			    overlays (cdr overlays))
		      (overlay-put overlay 'face face)
		      (move-overlay overlay beg (point)))
		  ;; Make a new overlay and add it to
		  ;; pmail-overlay-list.
		  (setq overlay (make-overlay beg (point)))
		  (overlay-put overlay 'face face)
		  (setq pmail-overlay-list
			(cons overlay pmail-overlay-list))))))))))

(defun pmail-auto-file ()
  "Automatically move a message into a sub-folder based on criteria.
Called when a new message is displayed."
  (if (or (zerop pmail-total-messages)
	  (pmail-message-attr-p pmail-current-message "...F...")
	  (not (string= (buffer-file-name)
			(expand-file-name pmail-file-name))))
      ;; Do nothing if the message has already been filed or if there
      ;; are no messages.
      nil
    ;; Find out some basics (common fields)
    (let ((from (mail-fetch-field "from"))
	  (subj (mail-fetch-field "subject"))
	  (to   (concat (mail-fetch-field "to") "," (mail-fetch-field "cc")))
	  (d pmail-automatic-folder-directives)
	  (directive-loop nil)
	  (folder nil))
      (while d
	(setq folder (car (car d))
	      directive-loop (cdr (car d)))
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
		(pmail-output-to-pmail-file folder 1 t)
		(setq d nil))))
	(setq d (cdr d))))))

(defun pmail-next-message (n)
  "Show following message whether deleted or not.
With prefix arg N, moves forward N messages, or backward if N is negative."
  (interactive "p")
  (set-buffer pmail-buffer)
  (pmail-maybe-set-message-counters)
  (pmail-show-message (+ pmail-current-message n)))

(defun pmail-previous-message (n)
  "Show previous message whether deleted or not.
With prefix arg N, moves backward N messages, or forward if N is negative."
  (interactive "p")
  (pmail-next-message (- n)))

(defun pmail-next-undeleted-message (n)
  "Show following non-deleted message.
With prefix arg N, moves forward N non-deleted messages,
or backward if N is negative.

Returns t if a new message is being shown, nil otherwise."
  (interactive "p")
  (set-buffer pmail-buffer)
  (pmail-maybe-set-message-counters)
  (let ((lastwin pmail-current-message)
	(current pmail-current-message))
    (while (and (> n 0) (< current pmail-total-messages))
      (setq current (1+ current))
      (if (not (pmail-message-deleted-p current))
	  (setq lastwin current n (1- n))))
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (if (not (pmail-message-deleted-p current))
	  (setq lastwin current n (1+ n))))
    (if (/= lastwin pmail-current-message)
 	(progn (pmail-show-message lastwin)
 	       t)
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
  (pmail-maybe-set-message-counters)
  (pmail-show-message 1))

(defun pmail-last-message ()
  "Show last message in file."
  (interactive)
  (pmail-maybe-set-message-counters)
  (pmail-show-message pmail-total-messages))

(defun pmail-what-message ()
  (let ((where (point))
	(low 1)
	(high pmail-total-messages)
	(mid (/ pmail-total-messages 2)))
    (while (> (- high low) 1)
      (if (>= where (pmail-msgbeg mid))
	  (setq low mid)
	(setq high mid))
      (setq mid (+ low (/ (- high low) 2))))
    (if (>= where (pmail-msgbeg high)) high low)))

(defun pmail-message-recipients-p (msg recipients &optional primary-only)
  (save-restriction
    (goto-char (pmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
    (or (string-match recipients (or (mail-fetch-field "To") ""))
	(string-match recipients (or (mail-fetch-field "From") ""))
	(if (not primary-only)
	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(defun pmail-message-regexp-p (n regexp)
  "Return t, if for message number N, regexp REGEXP matches in the header."
  (let ((beg (pmail-msgbeg n))
	(end (pmail-msgend n)))
    (goto-char beg)
    (forward-line 1)
    (save-excursion
      (save-restriction
	(if (prog1 (= (following-char) ?0)
	      (forward-line 2)
	      ;; If there's a Summary-line in the (otherwise empty)
	      ;; header, we didn't yet get past the EOOH line.
	      (when (looking-at "^\\*\\*\\* EOOH \\*\\*\\*\n")
		(forward-line 1))
	      (setq beg (point))
	      (narrow-to-region (point) end))
	    (progn
	      (rfc822-goto-eoh)
	      (setq end (point)))
	  (setq beg (point))
	  (search-forward "\n*** EOOH ***\n" end t)
	  (setq end (1+ (match-beginning 0)))))
	(goto-char beg)
	(if pmail-enable-mime
	    (funcall pmail-search-mime-header-function n regexp end)
	  (re-search-forward regexp end t)))))

(defun pmail-search-message (msg regexp)
  "Return non-nil, if for message number MSG, regexp REGEXP matches."
  (goto-char (pmail-msgbeg msg))
  (if pmail-enable-mime
      (funcall pmail-search-mime-message-function msg regexp)
    (re-search-forward regexp (pmail-msgend msg) t)))

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
  (pmail-maybe-set-message-counters)
  (let ((omin (point-min))
	(omax (point-max))
	(opoint (point))
	win
	(reversep (< n 0))
	(msg pmail-current-message))
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


(defun pmail-first-unseen-message ()
  "Return the message index for the first message which has the
`unseen' attribute."
  (pmail-maybe-set-message-counters)
  (let ((current 1)
	found)
    (save-restriction
      (widen)
      (while (and (not found) (<= current pmail-total-messages))
	(if (pmail-message-attr-p current "......U")
	    (setq found current))
	(setq current (1+ current))))
    found))

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
	(while (and (/= n 0)
		    (if forward
			(< i pmail-total-messages)
		      (> i 1)))
	  (let (done)
	    (while (and (not done)
			(if forward
			    (< i pmail-total-messages)
			  (> i 1)))
	      (setq i (if forward (1+ i) (1- i)))
	      (goto-char (pmail-msgbeg i))
	      (search-forward "\n*** EOOH ***\n")
	      (let ((beg (point)) end)
		(search-forward "\n\n")
		(setq end (point))
		(goto-char beg)
		(setq done (re-search-forward search-regexp end t))))
	    (if done (setq found i)))
	  (setq n (if forward (1- n) (1+ n))))))
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

(defun pmail-message-deleted-p (n)
  (= (aref pmail-deleted-vector n) ?D))

(defun pmail-set-message-deleted-p (n state)
  (aset pmail-deleted-vector n (if state ?D ?\ )))

(defun pmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (pmail-set-attribute pmail-deleted-attr-index t)
  (run-hooks 'pmail-delete-message-hook))

(defun pmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
  (set-buffer pmail-buffer)
  (let ((msg pmail-current-message))
    (while (and (> msg 0)
		(not (pmail-message-deleted-p msg)))
      (setq msg (1- msg)))
    (if (= msg 0)
	(error "No previous deleted message")
      (if (/= msg pmail-current-message)
	  (pmail-show-message msg))
      (pmail-set-attribute pmail-deleted-attr-index nil)
      (if (pmail-summary-exists)
	  (save-excursion
	    (set-buffer pmail-summary-buffer)
	    (pmail-summary-mark-undeleted msg)))
      (pmail-maybe-display-summary))))

(defun pmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[pmail-expunge] command is given.
With prefix argument, delete and move backward.

Returns t if a new message is displayed after the delete, or nil otherwise."
  (interactive "P")
  (pmail-set-attribute pmail-deleted-attr-index t)
  (run-hooks 'pmail-delete-message-hook)
  (let ((del-msg pmail-current-message))
    (if (pmail-summary-exists)
	(pmail-select-summary
	 (pmail-summary-mark-deleted del-msg)))
    (prog1 (pmail-next-undeleted-message (if backward -1 1))
      (pmail-maybe-display-summary))))

(defun pmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[pmail-expunge] command is given."
  (interactive)
  (pmail-delete-forward t))

;; Compute the message number a given message would have after expunging.
;; The present number of the message is OLDNUM.
;; DELETEDVEC should be pmail-deleted-vector.
;; The value is nil for a message that would be deleted.
(defun pmail-msg-number-after-expunge (deletedvec oldnum)
  (if (or (null oldnum) (= (aref deletedvec oldnum) ?D))
      nil
    (let ((i 0)
	  (newnum 0))
      (while (< i oldnum)
	(if (/= (aref deletedvec i) ?D)
	    (setq newnum (1+ newnum)))
	(setq i (1+ i)))
      newnum)))

(defun pmail-expunge-confirmed ()
  "Return t if deleted message should be expunged. If necessary, ask the user.
See also user-option `pmail-confirm-expunge'."
  (set-buffer pmail-buffer)
  (or (not (stringp pmail-deleted-vector))
      (not (string-match "D" pmail-deleted-vector))
      (null pmail-confirm-expunge)
      (funcall pmail-confirm-expunge
	       "Erase deleted messages from Pmail file? ")))

(defun pmail-only-expunge (&optional dont-show)
  "Actually erase all deleted messages in the file."
  (interactive)
  (set-buffer pmail-buffer)
  (message "Expunging deleted messages...")
  ;; Discard all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (pmail-maybe-set-message-counters)
  (let* ((omax (- (buffer-size) (point-max)))
	 (omin (- (buffer-size) (point-min)))
	 (opoint (if (and (> pmail-current-message 0)
			  (pmail-message-deleted-p pmail-current-message))
		     0
		   (if pmail-enable-mime
		       (with-current-buffer pmail-view-buffer
			 (- (point)(point-min)))
		     (- (point) (point-min)))))
	 (messages-head (cons (aref pmail-message-vector 0) nil))
	 (messages-tail messages-head)
	 ;; Don't make any undo records for the expunging.
	 (buffer-undo-list t)
	 (win))
    (unwind-protect
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (let ((counter 0)
		(number 1)
		(total pmail-total-messages)
		(new-message-number pmail-current-message)
		(new-summary nil)
		(new-msgref (list (list 0)))
		(pmailbuf (current-buffer))
		(buffer-read-only nil)
		(messages pmail-message-vector)
		(deleted pmail-deleted-vector)
		(summary pmail-summary-vector))
	    (setq pmail-total-messages nil
		  pmail-current-message nil
		  pmail-message-vector nil
		  pmail-deleted-vector nil
		  pmail-summary-vector nil)

	    (while (<= number total)
	      (if (= (aref deleted number) ?D)
		  (progn
		    (delete-region
		      (marker-position (aref messages number))
		      (marker-position (aref messages (1+ number))))
		    (move-marker (aref messages number) nil)
		    (if (> new-message-number counter)
			(setq new-message-number (1- new-message-number))))
		(setq counter (1+ counter))
		(setq messages-tail
		      (setcdr messages-tail
			      (cons (aref messages number) nil)))
		(setq new-summary
		      (cons (if (= counter number) (aref summary (1- number)))
			    new-summary))
		(setq new-msgref
		      (cons (aref pmail-msgref-vector number)
			    new-msgref))
		(setcar (car new-msgref) counter))
	      (if (zerop (% (setq number (1+ number)) 20))
		  (message "Expunging deleted messages...%d" number)))
	    (setq messages-tail
		  (setcdr messages-tail
			  (cons (aref messages number) nil)))
	    (setq pmail-current-message new-message-number
		  pmail-total-messages counter
		  pmail-message-vector (apply 'vector messages-head)
		  pmail-deleted-vector (make-string (1+ counter) ?\ )
		  pmail-summary-vector (vconcat (nreverse new-summary))
		  pmail-msgref-vector (apply 'vector (nreverse new-msgref))
		  win t)))
      (message "Expunging deleted messages...done")
      (if (not win)
	  (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax)))
      (if (not dont-show)
	  (pmail-show-message
	   (if (zerop pmail-current-message) 1 nil)))
      (pmail-swap-buffers-maybe)
      (if pmail-enable-mime
	  (goto-char (+ (point-min) opoint))
	(goto-char (+ (point) opoint))))))

(defun pmail-expunge ()
  "Erase deleted messages from Pmail file and summary buffer."
  (interactive)
  (when (pmail-expunge-confirmed)
    (pmail-only-expunge)
    (if (pmail-summary-exists)
	(pmail-select-summary (pmail-update-summary)))))

;;;; *** Pmail Mailing Commands ***

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
  (let (from reply-to cc subject date to message-id references
	     resent-to resent-cc resent-reply-to
	     (msgnum pmail-current-message))
    (save-excursion
      (save-restriction
	(if pmail-enable-mime
	    (narrow-to-region
	     (goto-char (point-min))
	     (if (search-forward "\n\n" nil 'move)
		 (1+ (match-beginning 0))
	       (point)))
	  (widen)
	  (goto-char (pmail-msgbeg pmail-current-message))
	  (forward-line 1)
	  (if (= (following-char) ?0)
	      (narrow-to-region
	       (progn (forward-line 2)
		      (point))
	       (progn (search-forward "\n\n" (pmail-msgend pmail-current-message)
				      'move)
		      (point)))
	    (narrow-to-region (point)
			      (progn (search-forward "\n*** EOOH ***\n")
				     (beginning-of-line) (point)))))
	(setq from (mail-fetch-field "from")
	      reply-to (or (mail-fetch-field "mail-reply-to" nil t)
			   (mail-fetch-field "reply-to" nil t)
			   from)
	      subject (mail-fetch-field "subject")
	      date (mail-fetch-field "date")
	      message-id (mail-fetch-field "message-id")
	      references (mail-fetch-field "references" nil nil t)
	      resent-reply-to (mail-fetch-field "resent-reply-to" nil t)
	      resent-cc (and (not just-sender)
			     (mail-fetch-field "resent-cc" nil t))
	      resent-to (or (mail-fetch-field "resent-to" nil t) "")
;;;	      resent-subject (mail-fetch-field "resent-subject")
;;;	      resent-date (mail-fetch-field "resent-date")
;;;	      resent-message-id (mail-fetch-field "resent-message-id")
	      )
	(unless just-sender
	  (if (mail-fetch-field "mail-followup-to" nil t)
	      ;; If this header field is present, use it instead of the To and CC fields.
	      (setq to (mail-fetch-field "mail-followup-to" nil t))
	    (setq cc (or (mail-fetch-field "cc" nil t) "")
		  to (or (mail-fetch-field "to" nil t) ""))))

	))

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
    (pmail-start-mail
     nil
     ;; Using mail-strip-quoted-names is undesirable with newer mailers
     ;; since they can handle the names unstripped.
     ;; I don't know whether there are other mailers that still
     ;; need the names to be stripped.
;;;     (mail-strip-quoted-names reply-to)
     ;; Remove unwanted names from reply-to, since Mail-Followup-To
     ;; header causes all the names in it to wind up in reply-to, not
     ;; in cc.  But if what's left is an empty list, use the original.
     (let* ((reply-to-list (pmail-dont-reply-to reply-to)))
       (if (string= reply-to-list "") reply-to reply-to-list))
     subject
     (pmail-make-in-reply-to-field from date message-id)
     (if just-sender
	 nil
       ;; mail-strip-quoted-names is NOT necessary for pmail-dont-reply-to
       ;; to do its job.
       (let* ((cc-list (pmail-dont-reply-to
			(mail-strip-quoted-names
			 (if (null cc) to (concat to ", " cc))))))
	 (if (string= cc-list "") nil cc-list)))
     pmail-view-buffer
     (list (list 'pmail-mark-message
		 pmail-buffer
		 (with-current-buffer pmail-buffer
		   (aref pmail-msgref-vector msgnum))
		 "answered"))
     nil
     (list (cons "References" (concat (mapconcat 'identity references " ")
				      " " message-id))))))

(defun pmail-mark-message (buffer msgnum-list attribute)
  "Give BUFFER's message number in MSGNUM-LIST the attribute ATTRIBUTE.
This is use in the send-actions for message buffers.
MSGNUM-LIST is a list of the form (MSGNUM)
which is an element of pmail-msgref-vector."
  (save-excursion
    (set-buffer buffer)
    (if (car msgnum-list)
	(pmail-set-attribute attribute t (car msgnum-list)))))

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

(defun pmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `pmail-resend'."
  (interactive "P")
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
			 (aref pmail-msgref-vector msgnum))
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
      (pmail-set-attribute pmail-resent-attr-index t pmail-current-message))))

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
  (require 'mail-utils)
  (let ((pmail-this-buffer (current-buffer))
	(msgnum pmail-current-message)
	bounce-start bounce-end bounce-indent resending
	;; Fetch any content-type header in current message
	;; Must search thru the whole unpruned header.
	(content-type
	 (save-excursion
	   (save-restriction
	     (mail-fetch-field "Content-Type") ))))
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
					(aref pmail-msgref-vector msgnum)
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
  ;; Fontify the current message if it is not already fontified.
  (if (text-property-any (point-min) (point-max) 'pmail-fontified nil)
      (let ((modified (buffer-modified-p))
	    (buffer-undo-list t) (inhibit-read-only t)
	    before-change-functions after-change-functions
	    buffer-file-name buffer-file-truename)
	(save-excursion
	  (save-match-data
	    (add-text-properties (point-min) (point-max) '(pmail-fontified t))
	    (font-lock-fontify-region (point-min) (point-max))
	    (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil)))))))

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

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 65d257d3-c281-4a65-9c38-e61af95af2f0
;;; pmail.el ends here
