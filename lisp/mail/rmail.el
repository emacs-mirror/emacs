;;; rmail.el --- main code of "RMAIL" mail reader for Emacs

;; Copyright (C) 1985,86,87,88,93,94,95,96,97,98,2000, 2001
;;		Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

(require 'mail-utils)
(eval-when-compile (require 'mule-util)) ; for detect-coding-with-priority

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


(defcustom rmail-movemail-program nil
  "If non-nil, name of program for fetching new mail."
  :group 'rmail-retrieve
  :type '(choice (const nil) string))

(defcustom rmail-pop-password nil
  "*Password to use when reading mail from a POP server, if required."
  :type '(choice (string :tag "Password")
		 (const :tag "Not Required" nil))
  :group 'rmail-retrieve)

(defcustom rmail-pop-password-required nil
  "*Non-nil if a password is required when reading mail using POP."
  :type 'boolean
  :group 'rmail-retrieve)

(defcustom rmail-movemail-flags nil
  "*List of flags to pass to movemail.
Most commonly used to specify `-g' to enable GSS-API authentication
or `-k' to enable Kerberos authentication."
  :type '(repeat string)
  :group 'rmail-retrieve
  :version "20.3")

(defvar rmail-pop-password-error "invalid usercode or password\\|
unknown user name or bad password"
  "Regular expression matching incorrect-password POP server error messages.
If you get an incorrect-password error that this expression does not match,
please report it with \\[report-emacs-bug].")

(defvar rmail-encoded-pop-password nil)

(defcustom rmail-preserve-inbox nil
  "*Non-nil if incoming mail should be left in the user's inbox,
rather than deleted, after it is retrieved."
  :type 'boolean
  :group 'rmail-retrieve)

;;;###autoload
(defcustom rmail-dont-reply-to-names nil "\
*A regexp specifying addresses to prune from a reply message.
A value of nil means exclude your own email address as an address
plus whatever is specified by `rmail-default-dont-reply-to-names'."
  :type '(choice regexp (const :tag "Your Name" nil))
  :group 'rmail-reply)

;;;###autoload
(defvar rmail-default-dont-reply-to-names "info-" "\
A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's email address and name.)
It is useful to set this variable in the site customization file.")

;;;###autoload
(defcustom rmail-ignored-headers
  (concat "^via:\\|^mail-from:\\|^origin:\\|^references:"
	  "\\|^status:\\|^received:\\|^x400-originator:\\|^x400-recipients:"
	  "\\|^x400-received:\\|^x400-mts-identifier:\\|^x400-content-type:"
	  "\\|^\\(resent-\\|\\)message-id:\\|^summary-line:\\|^resent-date:"
	  "\\|^nntp-posting-host:\\|^path:\\|^x-char.*:\\|^x-face:"
	  "\\|^x-mailer:\\|^delivered-to:\\|^lines:\\|^mime-version:"
	  "\\|^content-transfer-encoding:\\|^x-coding-system:"
	  "\\|^return-path:\\|^errors-to:\\|^return-receipt-to:"
	  "\\|^x-sign:\\|^x-beenthere:\\|^x-mailman-version:"
	  "\\|^precedence:\\|^list-help:\\|^list-post:\\|^list-subscribe:"
	  "\\|^list-id:\\|^list-unsubscribe:\\|^list-archive:"
	  "\\|^content-type:\\|^content-length:"
	  "\\|^x-attribution:\\|^x-disclaimer:\\|^x-trace:"
	  "\\|^x-complaints-to:\\|^nntp-posting-date:\\|^user-agent:")
  "*Regexp to match header fields that Rmail should normally hide.
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

;;;###autoload
(defcustom rmail-highlight-face nil "\
*Face used by Rmail for highlighting headers."
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
`nil' means the default, which is (\"/usr/spool/mail/$USER\")
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
(defcustom rmail-mail-new-frame nil
  "*Non-nil means Rmail makes a new frame for composing outgoing mail."
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
  :options '(goto-addr)
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
  "Regexp marking the start of an mmdf message")
(defvar rmail-mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message")

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
  :group 'rmail
  :version "21.1"
  :type '(repeat (sexp :tag "Directive")))
  
(defvar rmail-reply-prefix "Re: "
  "String to prepend to Subject line when replying to a message.")

;; Some mailers use "Re(2):" or "Re^2:" or "Re: Re:" or "Re[2]:".
;; This pattern should catch all the common variants.
(defvar rmail-reply-regexp "\\`\\(Re\\(([0-9]+)\\|\\[[0-9]+\\]\\|\\^[0-9]+\\)?: *\\)*"
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

(defvar rmail-message-vector nil)
(put 'rmail-message-vector 'permanent-local t)

(defvar rmail-deleted-vector nil)
(put 'rmail-deleted-vector 'permanent-local t)

(defvar rmail-msgref-vector nil
  "In an Rmail buffer, a vector whose Nth element is a list (N).
When expunging renumbers messages, these lists are modified
by substituting the new message number into the existing list.")
(put 'rmail-msgref-vector 'permanent-local t)

(defvar rmail-overlay-list nil)
(put 'rmail-overlay-list 'permanent-local t)

;; These are used by autoloaded rmail-summary.

(defvar rmail-summary-buffer nil)
(put 'rmail-summary-buffer 'permanent-local t)
(defvar rmail-summary-vector nil)
(put 'rmail-summary-vector 'permanent-local t)

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
It is called with four arguments MSG, REGEXP, and LIMIT, where
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
  "^content-type:[ ]*text/plain;[ \t\n]*charset=\"?\\([^ \t\n\"]+\\)\"?"
  "Regexp to match MIME-charset specification in a header of message.
The first parenthesized expression should match the MIME-charset name.")


;;; Regexp matching the delimiter of messages in UNIX mail format
;;; (UNIX From lines), minus the initial ^.  Note that if you change
;;; this expression, you must change the code in rmail-nuke-pinhead-header
;;; that knows the exact ordering of the \\( \\) subexpressions.
(defvar rmail-unix-mail-delimiter
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

(defvar rmail-font-lock-keywords
  (eval-when-compile
    (let* ((cite-chars "[>|}]")
	   (cite-prefix "A-Za-z")
	   (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
      (list '("^\\(From\\|Sender\\):" . font-lock-function-name-face)
	    '("^Reply-To:.*$" . font-lock-function-name-face)
	    '("^Subject:" . font-lock-comment-face)
	    '("^\\(To\\|Apparently-To\\|Cc\\|Newsgroups\\):"
	      . font-lock-keyword-face)
	    ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
	    `(,cite-chars
	      (,(concat "\\=[ \t]*"
			"\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
			"\\(" cite-chars "[ \t]*\\)\\)+"
			"\\(.*\\)")
	       (beginning-of-line) (end-of-line)
	       (2 font-lock-constant-face nil t)
	       (4 font-lock-comment-face nil t)))
	    '("^\\(X-[A-Za-z0-9-]+\\|In-reply-to\\|Date\\):.*$"
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


(defun rmail-require-mime-maybe ()
  "Require `rmail-mime-feature' if that is non-nil.
Signal an error and set `rmail-mime-feature' to nil if the feature
isn't provided."
  (when rmail-enable-mime
    (condition-case err
	(require rmail-mime-feature)
      (error
       (message "Feature `%s' not provided" rmail-mime-feature)
       (sit-for 1)
       (setq rmail-enable-mime nil)))))


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
	 ;; code conversion, then decode the messages one by one by
	 ;; rmail-decode-babyl-format or
	 ;; rmail-convert-to-babyl-format.
	 (coding-system-for-read (and rmail-enable-multibyte 'raw-text))
	 run-mail-hook msg-shown)
    ;; Like find-file, but in the case where a buffer existed
    ;; and the file was reverted, recompute the message-data.
    ;; We used to bind enable-local-variables to nil here,
    ;; but that should not be needed now that rmail-mode
    ;; sets it locally to nil.
    ;; (Binding a variable locally with let is not safe if it has
    ;; buffer-local bindings.)
    (if (and existed (not (verify-visited-file-modtime existed)))
	(progn
	  (find-file file-name)
	  (if (and (verify-visited-file-modtime existed)
		   (eq major-mode 'rmail-mode))
	      (progn (rmail-forget-messages)
		     (rmail-set-message-counters))))
      (switch-to-buffer 
       (let ((enable-local-variables nil))
	 (find-file-noselect file-name))))
    (if (eq major-mode 'rmail-edit-mode)
	(error "Exit Rmail Edit mode before getting new mail"))
    (if (and existed (> (buffer-size) 0))
	;; Buffer not new and not empty; ensure in proper mode, but that's all.
	(or (eq major-mode 'rmail-mode)
	    (progn (rmail-mode-2)
		   (setq run-mail-hook t)))
      (setq run-mail-hook t)
      (rmail-mode-2)
      ;; Convert all or part to Babyl file if possible.
      (rmail-convert-file)
      (goto-char (point-max)))
    ;; As we have read a file by raw-text, the buffer is set to
    ;; unibyte.  We must make it multibyte if necessary.
    (if (and rmail-enable-multibyte
	     (not enable-multibyte-characters))
	(set-buffer-multibyte t))
    ;; If necessary, scan to find all the messages.
    (rmail-maybe-set-message-counters)
    (unwind-protect
	(unless (and (not file-name-arg)
		     (rmail-get-new-mail))
	  (rmail-show-message (rmail-first-unseen-message)))
      (progn
	(if rmail-display-summary (rmail-summary))
	(rmail-construct-io-menu)
	(if run-mail-hook
	    (run-hooks 'rmail-mode-hook))))))

;; Given the value of MAILPATH, return a list of inbox file names.
;; This is turned off because it is not clear that the user wants
;; all these inboxes to feed into the primary rmail file.
; (defun rmail-convert-mailpath (string)
;   (let (idx list)
;     (while (setq idx (string-match "[%:]" string))
;       (let ((this (substring string 0 idx)))
; 	(setq string (substring string (1+ idx)))
; 	(setq list (cons (if (string-match "%" this)
; 			     (substring this 0 (string-match "%" this))
; 			   this)
; 			 list))))
;     list))

; I have checked that adding "-*- rmail -*-" to the BABYL OPTIONS line
; will not cause emacs 18.55 problems.

;; This calls rmail-decode-babyl-format if the file is already Babyl.

(defun rmail-convert-file ()
  (let (convert)
    (widen)
    (goto-char (point-min))
    ;; If file doesn't start like a Babyl file,
    ;; convert it to one, by adding a header and converting each message.
    (cond ((looking-at "BABYL OPTIONS:"))
	  ((looking-at "Version: 5\n")
	   ;; Losing babyl file made by old version of Rmail.
	   ;; Just fix the babyl file header; don't make a new one,
	   ;; so we don't lose the Labels: file attribute, etc.
	   (let ((buffer-read-only nil))
	     (insert "BABYL OPTIONS: -*- rmail -*-\n")))
	  ((equal (point-min) (point-max))
	   ;; Empty RMAIL file.  Just insert the header.
	   (rmail-insert-rmail-file-header))
	  (t
	   ;; Non-empty file in non-RMAIL format.  Add header and convert.
	   (setq convert t)
	   (rmail-insert-rmail-file-header)))
    ;; If file was not a Babyl file or if there are
    ;; Unix format messages added at the end,
    ;; convert file as necessary.
    (if (or convert
	    (save-excursion
	      (goto-char (point-max))
	      (search-backward "\n\^_")
	      (forward-char 2)
	      (looking-at "\n*From ")))
	(let ((buffer-read-only nil))
	  (message "Converting to Babyl format...")
	  ;; If file needs conversion, convert it all,
	  ;; except for the BABYL header.
	  ;; (rmail-convert-to-babyl-format would delete the header.)
	  (goto-char (point-min))
	  (search-forward "\n\^_" nil t)
	  (narrow-to-region (point) (point-max))
	  (rmail-convert-to-babyl-format)
	  (message "Converting to Babyl format...done"))
      (if (and (not rmail-enable-mime)
	       rmail-enable-multibyte)
	  ;; We still have to decode BABYL part.
	  (rmail-decode-babyl-format)))))

(defun rmail-insert-rmail-file-header ()
  (let ((buffer-read-only nil))
    ;; -*-rmail-*- is here so that visiting the file normally
    ;; recognizes it as an Rmail file.
    (insert "BABYL OPTIONS: -*- rmail -*-
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.\n\^_")))

;; Decode Babyl formated part at the head of current buffer by
;; rmail-file-coding-system, or if it is nil, do auto conversion.

(defun rmail-decode-babyl-format ()
  (let ((modifiedp (buffer-modified-p))
	(buffer-read-only nil)
	(coding-system rmail-file-coding-system)
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
	    ;; Emacs 21.1 and later writes RMAIL files in emacs-mule, but
	    ;; earlier versions did that with the current buffer's encoding.
	    ;; So we want to favor detection of emacs-mule (whose normal
	    ;; priority is quite low), but still allow detection of other
	    ;; encodings if emacs-mule won't fit.  The call to
	    ;; detect-coding-with-priority below achieves that.
	    (car (detect-coding-with-priority
		  from to
		  '((coding-category-emacs-mule . emacs-mule))))))
    (unless (memq coding-system
		  '(undecided undecided-unix))
      (set-buffer-modified-p t)		; avoid locking when decoding
      (let ((buffer-undo-list t))
	(decode-coding-region from to coding-system))
      (setq coding-system last-coding-system-used))
    (set-buffer-modified-p modifiedp)
    (setq buffer-file-coding-system nil)
    (setq save-buffer-coding-system
	  (or coding-system 'undecided))))

(defvar rmail-mode-map nil)
(if rmail-mode-map
    nil
  (setq rmail-mode-map (make-keymap))
  (suppress-keymap rmail-mode-map)
  (define-key rmail-mode-map "a"      'rmail-add-label)
  (define-key rmail-mode-map "b"      'rmail-bury)
  (define-key rmail-mode-map "c"      'rmail-continue)
  (define-key rmail-mode-map "d"      'rmail-delete-forward)
  (define-key rmail-mode-map "\C-d"   'rmail-delete-backward)
  (define-key rmail-mode-map "e"      'rmail-edit-current-message)
  (define-key rmail-mode-map "f"      'rmail-forward)
  (define-key rmail-mode-map "g"      'rmail-get-new-mail)
  (define-key rmail-mode-map "h"      'rmail-summary)
  (define-key rmail-mode-map "i"      'rmail-input)
  (define-key rmail-mode-map "j"      'rmail-show-message)
  (define-key rmail-mode-map "k"      'rmail-kill-label)
  (define-key rmail-mode-map "l"      'rmail-summary-by-labels)
  (define-key rmail-mode-map "\e\C-h" 'rmail-summary)
  (define-key rmail-mode-map "\e\C-l" 'rmail-summary-by-labels)
  (define-key rmail-mode-map "\e\C-r" 'rmail-summary-by-recipients)
  (define-key rmail-mode-map "\e\C-s" 'rmail-summary-by-regexp)
  (define-key rmail-mode-map "\e\C-t" 'rmail-summary-by-topic)
  (define-key rmail-mode-map "m"      'rmail-mail)
  (define-key rmail-mode-map "\em"    'rmail-retry-failure)
  (define-key rmail-mode-map "n"      'rmail-next-undeleted-message)
  (define-key rmail-mode-map "\en"    'rmail-next-message)
  (define-key rmail-mode-map "\e\C-n" 'rmail-next-labeled-message)
  (define-key rmail-mode-map "o"      'rmail-output-to-rmail-file)
  (define-key rmail-mode-map "\C-o"   'rmail-output)
  (define-key rmail-mode-map "p"      'rmail-previous-undeleted-message)
  (define-key rmail-mode-map "\ep"    'rmail-previous-message)
  (define-key rmail-mode-map "\e\C-p" 'rmail-previous-labeled-message)
  (define-key rmail-mode-map "q"      'rmail-quit)
  (define-key rmail-mode-map "r"      'rmail-reply)
;; I find I can't live without the default M-r command -- rms.
;;  (define-key rmail-mode-map "\er"  'rmail-search-backwards)
  (define-key rmail-mode-map "s"      'rmail-expunge-and-save)
  (define-key rmail-mode-map "\es"    'rmail-search)
  (define-key rmail-mode-map "t"      'rmail-toggle-header)
  (define-key rmail-mode-map "u"      'rmail-undelete-previous-message)
  (define-key rmail-mode-map "w"      'rmail-output-body-to-file)
  (define-key rmail-mode-map "x"      'rmail-expunge)
  (define-key rmail-mode-map "."      'rmail-beginning-of-message)
  (define-key rmail-mode-map "<"      'rmail-first-message)
  (define-key rmail-mode-map ">"      'rmail-last-message)
  (define-key rmail-mode-map " "      'scroll-up)
  (define-key rmail-mode-map "\177"   'scroll-down)
  (define-key rmail-mode-map "?"      'describe-mode)
  (define-key rmail-mode-map "\C-c\C-s\C-d" 'rmail-sort-by-date)
  (define-key rmail-mode-map "\C-c\C-s\C-s" 'rmail-sort-by-subject)
  (define-key rmail-mode-map "\C-c\C-s\C-a" 'rmail-sort-by-author)
  (define-key rmail-mode-map "\C-c\C-s\C-r" 'rmail-sort-by-recipient)
  (define-key rmail-mode-map "\C-c\C-s\C-c" 'rmail-sort-by-correspondent)
  (define-key rmail-mode-map "\C-c\C-s\C-l" 'rmail-sort-by-lines)
  (define-key rmail-mode-map "\C-c\C-s\C-k" 'rmail-sort-by-labels)
  (define-key rmail-mode-map "\C-c\C-n" 'rmail-next-same-subject)
  (define-key rmail-mode-map "\C-c\C-p" 'rmail-previous-same-subject)
  )

(define-key rmail-mode-map [menu-bar] (make-sparse-keymap))

(define-key rmail-mode-map [menu-bar classify]
  (cons "Classify" (make-sparse-keymap "Classify")))

(define-key rmail-mode-map [menu-bar classify input-menu]
  nil)

(define-key rmail-mode-map [menu-bar classify output-menu]
  nil)

(define-key rmail-mode-map [menu-bar classify output-body]
  '("Output body to file..." . rmail-output-body-to-file))

(define-key rmail-mode-map [menu-bar classify output-inbox]
  '("Output (inbox)..." . rmail-output))

(define-key rmail-mode-map [menu-bar classify output]
  '("Output (Rmail)..." . rmail-output-to-rmail-file))

(define-key rmail-mode-map [menu-bar classify kill-label]
  '("Kill Label..." . rmail-kill-label))

(define-key rmail-mode-map [menu-bar classify add-label]
  '("Add Label..." . rmail-add-label))

(define-key rmail-mode-map [menu-bar summary]
  (cons "Summary" (make-sparse-keymap "Summary")))

(define-key rmail-mode-map [menu-bar summary senders]
  '("By Senders..." . rmail-summary-by-senders))

(define-key rmail-mode-map [menu-bar summary labels]
  '("By Labels..." . rmail-summary-by-labels))

(define-key rmail-mode-map [menu-bar summary recipients]
  '("By Recipients..." . rmail-summary-by-recipients))

(define-key rmail-mode-map [menu-bar summary topic]
  '("By Topic..." . rmail-summary-by-topic))

(define-key rmail-mode-map [menu-bar summary regexp]
  '("By Regexp..." . rmail-summary-by-regexp))

(define-key rmail-mode-map [menu-bar summary all]
  '("All" . rmail-summary))

(define-key rmail-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))

(define-key rmail-mode-map [menu-bar mail rmail-get-new-mail]
  '("Get New Mail" . rmail-get-new-mail))

(define-key rmail-mode-map [menu-bar mail lambda]
  '("----"))

(define-key rmail-mode-map [menu-bar mail continue]
  '("Continue" . rmail-continue))

(define-key rmail-mode-map [menu-bar mail resend]
  '("Re-send..." . rmail-resend))

(define-key rmail-mode-map [menu-bar mail forward]
  '("Forward" . rmail-forward))

(define-key rmail-mode-map [menu-bar mail retry]
  '("Retry" . rmail-retry-failure))

(define-key rmail-mode-map [menu-bar mail reply]
  '("Reply" . rmail-reply))

(define-key rmail-mode-map [menu-bar mail mail]
  '("Mail" . rmail-mail))

(define-key rmail-mode-map [menu-bar delete]
  (cons "Delete" (make-sparse-keymap "Delete")))

(define-key rmail-mode-map [menu-bar delete expunge/save]
  '("Expunge/Save" . rmail-expunge-and-save))

(define-key rmail-mode-map [menu-bar delete expunge]
  '("Expunge" . rmail-expunge))

(define-key rmail-mode-map [menu-bar delete undelete]
  '("Undelete" . rmail-undelete-previous-message))

(define-key rmail-mode-map [menu-bar delete delete]
  '("Delete" . rmail-delete-forward))

(define-key rmail-mode-map [menu-bar move]
  (cons "Move" (make-sparse-keymap "Move")))

(define-key rmail-mode-map [menu-bar move search-back]
  '("Search Back..." . rmail-search-backwards))

(define-key rmail-mode-map [menu-bar move search]
  '("Search..." . rmail-search))

(define-key rmail-mode-map [menu-bar move previous]
  '("Previous Nondeleted" . rmail-previous-undeleted-message))

(define-key rmail-mode-map [menu-bar move next]
  '("Next Nondeleted" . rmail-next-undeleted-message))

(define-key rmail-mode-map [menu-bar move last]
  '("Last" . rmail-last-message))

(define-key rmail-mode-map [menu-bar move first]
  '("First" . rmail-first-message))

(define-key rmail-mode-map [menu-bar move previous]
  '("Previous" . rmail-previous-message))

(define-key rmail-mode-map [menu-bar move next]
  '("Next" . rmail-next-message))

;; Rmail mode is suitable only for specially formatted data.
(put 'rmail-mode 'mode-class 'special)

(defun rmail-mode-kill-summary ()
  (if rmail-summary-buffer (kill-buffer rmail-summary-buffer)))

;;;###autoload
(defun rmail-mode ()
  "Rmail Mode is used by \\<rmail-mode-map>\\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

\\[rmail-beginning-of-message]	Move point to front of this message (same as \\[beginning-of-buffer]).
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
	(rmail-convert-file)
	(goto-char (point-max))
	(set-buffer-multibyte t)))
    (rmail-set-message-counters)
    (rmail-show-message rmail-total-messages)
    (when finding-rmail-file
      (when rmail-display-summary
	(rmail-summary))
      (rmail-construct-io-menu))
    (run-hooks 'rmail-mode-hook)))

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
  (make-local-variable 'rmail-deleted-vector)
  (make-local-variable 'rmail-buffer)
  (setq rmail-buffer (current-buffer))
  (make-local-variable 'rmail-view-buffer)
  (setq rmail-view-buffer rmail-buffer)
  (make-local-variable 'rmail-summary-buffer)
  (make-local-variable 'rmail-summary-vector)
  (make-local-variable 'rmail-current-message)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'rmail-overlay-list)
  (setq rmail-overlay-list nil)
  (make-local-variable 'rmail-message-vector)
  (make-local-variable 'rmail-msgref-vector)
  (make-local-variable 'rmail-inbox-list)
  (setq rmail-inbox-list (rmail-parse-file-inboxes))
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
	  t nil nil nil
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
  (setq file-precious-flag t))

;; Handle M-x revert-buffer done in an rmail-mode buffer.
(defun rmail-revert (arg noconfirm)
  (set-buffer rmail-buffer)
  (let* ((revert-buffer-function (default-value 'revert-buffer-function))
	 (rmail-enable-multibyte enable-multibyte-characters)
	 ;; See similar code in `rmail'.
	 (coding-system-for-read (and rmail-enable-multibyte 'raw-text)))
    ;; Call our caller again, but this time it does the default thing.
    (if (revert-buffer arg noconfirm)
	;; If the user said "yes", and we changed something,
	;; reparse the messages.
	(progn
	  (set-buffer rmail-buffer)
  	  (rmail-mode-2)
	  ;; Convert all or part to Babyl file if possible.
	  (rmail-convert-file)
	  ;; We have read the file as raw-text, so the buffer is set to
	  ;; unibyte.  Make it multibyte if necessary.
	  (if (and rmail-enable-multibyte
		   (not enable-multibyte-characters))
	      (set-buffer-multibyte t))
	  (goto-char (point-max))
	  (rmail-set-message-counters)
	  (rmail-show-message rmail-total-messages)
	  (run-hooks 'rmail-mode-hook)))))

;; Return a list of files from this buffer's Mail: option.
;; Does not assume that messages have been parsed.
;; Just returns nil if buffer does not look like Babyl format.
(defun rmail-parse-file-inboxes ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (cond ((looking-at "BABYL OPTIONS:")
	     (search-forward "\n\^_" nil 'move)
	     (narrow-to-region 1 (point))
	     (goto-char 1)
	     (if (search-forward "\nMail:" nil t)
		 (progn
		   (narrow-to-region (point) (progn (end-of-line) (point)))
		   (goto-char (point-min))
		   (mail-parse-comma-list))))))))

(defun rmail-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (rmail-expunge)
  (set-buffer rmail-buffer)
  (save-buffer)
  (if (rmail-summary-exists)
      (rmail-select-summary (set-buffer-modified-p nil))))

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

(defun rmail-duplicate-message ()
  "Create a duplicated copy of the current message.
The duplicate copy goes into the Rmail file just after the
original copy."
  (interactive)
  (widen)
  (let ((buffer-read-only nil)
	(number rmail-current-message)
	(string (buffer-substring (rmail-msgbeg rmail-current-message)
				  (rmail-msgend rmail-current-message))))
    (goto-char (rmail-msgend rmail-current-message))
    (insert string)
    (rmail-forget-messages)
    (rmail-show-message number)
    (message "Message duplicated")))

;;;###autoload
(defun rmail-input (filename)
  "Run Rmail on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  (rmail filename))


;; This used to scan subdirectories recursively, but someone pointed out
;; that if the user wants that, person can put all the files in one dir.
;; And the recursive scan was slow.  So I took it out.
;; rms, Sep 1996.
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
				      'rmail-output-to-rmail-file))))

      (define-key rmail-mode-map [menu-bar classify input-menu]
	'("Input Rmail File" . rmail-disable-menu))
      (define-key rmail-mode-map [menu-bar classify output-menu]
	'("Output Rmail File" . rmail-disable-menu)))))


;;;; *** Rmail input ***

;; RLK feature not added in this version:
;; argument specifies inbox file or files in various ways.

(defun rmail-get-new-mail (&optional file-name)
  "Move any new mail from this RMAIL file's inbox files.
The inbox files can be specified with the file's Mail: option.  The
variable `rmail-primary-inbox-list' specifies the inboxes for your
primary RMAIL file if it has no Mail: option.  By default, this is
your /usr/spool/mail/$USER.

You can also specify the file to get new mail from.  In this case, the
file of new mail is not changed or deleted.  Noninteractively, you can
pass the inbox file name as an argument.  Interactively, a prefix
argument causes us to read a file name and use that file as the inbox.

If the variable `rmail-preserve-inbox' is non-nil, new mail will
always be left in inbox files rather than deleted.

This function runs `rmail-get-new-mail-hook' before saving the updated file.
It returns t if it got any new messages."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (run-hooks 'rmail-before-get-new-mail-hook)
  ;; If the disk file has been changed from under us,
  ;; revert to it before we get new mail.
  (or (verify-visited-file-modtime (current-buffer))
      (find-file (buffer-file-name)))
  (set-buffer rmail-buffer)
  (rmail-maybe-set-message-counters)
  (widen)
  ;; Get rid of all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (let ((all-files (if file-name (list file-name)
		     rmail-inbox-list))
	(rmail-enable-multibyte (default-value 'enable-multibyte-characters))
	found)
    (unwind-protect
	(progn
	  (while all-files
	    (let ((opoint (point))
		  (new-messages 0)
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
		      (rmail-insert-inbox-text files nil)
		    (setq delete-files (rmail-insert-inbox-text files t)))
		  ;; Scan the new text and convert each message to babyl format.
		  (goto-char (point-min))
		  (unwind-protect
		      (save-excursion
			(setq new-messages (rmail-convert-to-babyl-format)
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
			    (while (file-exists-p (format "RMAILOSE.%d" count))
			      (setq count (1+ count)))
			    (rename-file (car delfiles)
					 (format "RMAILOSE.%d" count))
			    (setq delfiles (cdr delfiles))))))
		  (or (zerop new-messages)
		      (let (success)
			(widen)
			(search-backward "\n\^_" nil t)
			(narrow-to-region (point) (point-max))
			(goto-char (1+ (point-min)))
			(rmail-count-new-messages)
			(run-hooks 'rmail-get-new-mail-hook)
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
			 (if (or file-name rmail-inbox-list)
			     (message "(No new mail has arrived)")))
		(if (rmail-summary-exists)
		    (rmail-select-summary
		     (rmail-update-summary)))
		(message "%d new message%s read"
			 new-messages (if (= 1 new-messages) "" "s"))
		;; Move to the first new message
		;; unless we have other unseen messages before it.
		(rmail-show-message (rmail-first-unseen-message))
		(run-hooks 'rmail-after-get-new-mail-hook)
		(setq found t))))
	  found)
      ;; Don't leave the buffer screwed up if we get a disk-full error.
      (or found (rmail-show-message)))))

(defun rmail-insert-inbox-text (files renamep)
  ;; Detect a locked file now, so that we avoid moving mail
  ;; out of the real inbox file.  (That could scare people.)
  (or (memq (file-locked-p buffer-file-name) '(nil t))
      (error "RMAIL file %s is locked"
	     (file-name-nondirectory buffer-file-name)))
  (let (file tofile delete-files movemail popmail got-password password)
    (while files
      ;; Handle POP mailbox names specially; don't expand as filenames
      ;; in case the userid contains a directory separator.
      (setq file (car files))
      (setq popmail (string-match "^po:" file))
      (if popmail
	  (setq renamep t)
	(setq file (file-truename
		    (substitute-in-file-name (expand-file-name file)))))
      (setq tofile (expand-file-name
		    ;; Generate name to move to from inbox name,
		    ;; in case of multiple inboxes that need moving.
		    (concat ".newmail-" (file-name-nondirectory file))
		    ;; Use the directory of this rmail file
		    ;; because it's a nuisance to use the homedir
		    ;; if that is on a full disk and this rmail
		    ;; file isn't.
		    (file-name-directory
		     (expand-file-name buffer-file-name))))
      ;; Always use movemail to rename the file,
      ;; since there can be mailboxes in various directories.
      (setq movemail t)
;;;      ;; If getting from mail spool directory,
;;;      ;; use movemail to move rather than just renaming,
;;;      ;; so as to interlock with the mailer.
;;;      (setq movemail (string= file
;;;			      (file-truename
;;;			       (concat rmail-spool-directory
;;;				       (file-name-nondirectory file)))))
      (if (and movemail (not popmail))
	  (progn
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (expand-file-name (user-login-name)
					     file)))))
      (cond (popmail
	     (if rmail-pop-password-required
		 (progn (setq got-password (not (rmail-have-password)))
			(setq password (rmail-get-pop-password))))
	     (if (eq system-type 'windows-nt)
		 ;; cannot have "po:" in file name
		 (setq tofile
		       (expand-file-name
			(concat ".newmail-pop-"
				(file-name-nondirectory (substring file 3)))
			(file-name-directory
			 (expand-file-name buffer-file-name)))))
	     (message "Getting mail from post office ..."))
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
	    ((and (not movemail) (not popmail))
	     ;; Try copying.  If that fails (perhaps no space) and
	     ;; we're allowed to blow away the inbox, rename instead.
	     (if rmail-preserve-inbox
		 (copy-file file tofile nil)
	       (condition-case nil
		   (copy-file file tofile nil)
		 (error
		  ;; Third arg is t so we can replace existing file TOFILE.
		  (rename-file file tofile t))))
	     ;; Make the real inbox file empty.
	     ;; Leaving it deleted could cause lossage
	     ;; because mailers often won't create the file.
	     (if (not rmail-preserve-inbox)
		 (condition-case ()
		     (write-region (point) (point) file)
		   (file-error nil))))
	    (t
	     (let ((errors nil))
	       (unwind-protect
		   (save-excursion
		     (setq errors (generate-new-buffer " *rmail loss*"))
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
			     rmail-movemail-flags
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
		       (message "movemail: %s"
				(buffer-substring (point-min)
						  (point-max)))
		       ;; If we just read the password, most likely it is
		       ;; wrong.  Otherwise, see if there is a specific
		       ;; reason to think that the problem is a wrong passwd.
		       (if (or got-password
			       (re-search-forward rmail-pop-password-error
						  nil t))
			   (rmail-set-pop-password nil))
		       (sit-for 3)
		       nil))
		 (if errors (kill-buffer errors))))))
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

;; Decode the region specified by FROM and TO by CODING.
;; If CODING is nil or an invalid coding system, decode by `undecided'.
(defun rmail-decode-region (from to coding)
  (if (or (not coding) (not (coding-system-p coding)))
      (setq coding 'undecided))
  (decode-coding-region from to coding))

;; the  rmail-break-forwarded-messages  feature is not implemented
(defun rmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil)
	(invalid-input-resync
	 (function (lambda ()
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 3)
		     ;; Try to get back in sync with a real message.
		     (if (re-search-forward
			  (concat rmail-mmdf-delim1 "\\|^From") nil t)
			 (beginning-of-line)
		       (goto-char (point-max)))))))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(setq start (point))
	(cond ((looking-at "BABYL OPTIONS:");Babyl header
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
	       (setq last-coding-system-used nil)
	       (or rmail-enable-mime
		   (not rmail-enable-multibyte)
		   (decode-coding-region start (point)
					 (or rmail-file-coding-system
					     'undecided)))
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
	       (narrow-to-region (point) (point-max)))
	      ;;*** MMDF format
	      ((let ((case-fold-search t))
		 (looking-at rmail-mmdf-delim1))
	       (let ((case-fold-search t))
		 (replace-match "\^L\n0, unseen,,\n*** EOOH ***\n")
		 (re-search-forward rmail-mmdf-delim2 nil t)
		 (replace-match "\^_"))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char "\^_"
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (setq last-coding-system-used nil)
	       (or rmail-enable-mime
		   (not rmail-enable-multibyte)
		   (decode-coding-region start (point) 'undecided))
	       (save-excursion
		 (goto-char start)
		 (forward-line 3)
		 (insert "X-Coding-System: "
			 (symbol-name last-coding-system-used)
			 "\n"))
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count)))
	      ;;*** Mail format
	      ((looking-at "^From ")
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
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
				(string-to-int (buffer-substring beg eol)))))))
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
				       (looking-at rmail-mmdf-delim1))
				     (looking-at "From "))))
			  (goto-char (+ header-end size))
			(message "Ignoring invalid Content-Length field")
			(sit-for 1 0 t)))
		 (if (re-search-forward
		      (concat "^[\^_]?\\("
			      rmail-unix-mail-delimiter
			      "\\|"
			      rmail-mmdf-delim1 "\\|"
			      "^BABYL OPTIONS:\\|"
			      "\^L\n[01],\\)") nil t)
		     (goto-char (match-beginning 1))
		   (goto-char (point-max)))
		 (setq count (1+ count))
		 (if quoted-printable-header-field-end
		     (save-excursion
		       (rmail-decode-quoted-printable header-end (point))
		       ;; Change "quoted-printable" to "8bit",
		       ;; to reflect the decoding we just did.
		       (goto-char quoted-printable-header-field-end)
		       (delete-region (point) (search-backward ":"))
		       (insert ": 8bit"))))

	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (setq last-coding-system-used nil)
	       (or rmail-enable-mime
		   (not rmail-enable-multibyte)
		   (let ((mime-charset
			  (if (and rmail-decode-mime-charset
				   (save-excursion
				     (goto-char start)
				     (search-forward "\n\n" nil t)
				     (let ((case-fold-search t))
				       (re-search-backward
					rmail-mime-charset-pattern
					start t))))
			      (intern (downcase (match-string 1))))))
		     (rmail-decode-region start (point) mime-charset)))
	       (save-excursion
		 (goto-char start)
		 (forward-line 3)
		 (insert "X-Coding-System: "
			 (symbol-name last-coding-system-used)
			 "\n"))
	       (narrow-to-region (point) (point-max)))
	      ;;
	      ;; This kludge is because some versions of sendmail.el
	      ;; insert an extra newline at the beginning that shouldn't
	      ;; be there.  sendmail.el has been fixed, but old versions
	      ;; may still be in use.  -- rms, 7 May 1993.
	      ((eolp) (delete-char 1))
	      (t (error "Cannot convert to babyl format")))))
    count))

(defun rmail-hex-char-to-integer (character)
  "Return CHARACTER's value interpreted as a hex digit."
  (if (and (>= character ?0) (<= character ?9))
      (- character ?0)
    (let ((ch (logior character 32)))
      (if (and (>= ch ?a) (<= ch ?f))
	  (- ch (- ?a 10))
	(error "Invalid hex digit `%c'" ch)))))

(defun rmail-hex-string-to-integer (hex-string)
  "Return decimal integer for HEX-STRING."
  (let ((hex-num 0)
	(index 0))
    (while (< index (length hex-string))
      (setq hex-num (+ (* hex-num 16)
		       (rmail-hex-char-to-integer (aref hex-string index))))
      (setq index (1+ index)))
    hex-num))

(defun rmail-decode-quoted-printable (from to)
  "Decode Quoted-Printable in the region between FROM and TO."
  (interactive "r")
  (goto-char from)
  (or (markerp to)
      (setq to (copy-marker to)))
  (while (search-forward "=" to t)
    (cond ((eq (following-char) ?\n)
	   (delete-char -1)
	   (delete-char 1))
	  ((looking-at "[0-9A-F][0-9A-F]")
	   (let ((byte (rmail-hex-string-to-integer
			(buffer-substring (point) (+ 2 (point))))))
	     (delete-region (1- (point)) (+ 2 (point)))
	     (insert byte)))
	  ((looking-at "=")
	   (delete-char 1))
	  (t
	   (message "Malformed MIME quoted-printable message")))))

;; Delete the "From ..." line, creating various other headers with
;; information from it if they don't already exist.  Now puts the
;; original line into a mail-from: header line for debugging and for
;; use by the rmail-output function.
(defun rmail-nuke-pinhead-header ()
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
	  (if (re-search-forward (concat "^" rmail-unix-mail-delimiter) nil t)
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

;;;; *** Rmail Message Formatting and Header Manipulation ***

(defun rmail-reformat-message (beg end)
  (goto-char beg)
  (forward-line 1)
  (if (/= (following-char) ?0)
      (error "Bad format in RMAIL file"))
  (let ((inhibit-read-only t)
	(delta (- (buffer-size) end)))
    (delete-char 1)
    (insert ?1)
    (forward-line 1)
    (let ((case-fold-search t))
      (while (looking-at "Summary-line:\\|Mail-From:")
 	(forward-line 1)))
    (if (looking-at "\\*\\*\\* EOOH \\*\\*\\*\n")
	(delete-region (point)
		       (progn (forward-line 1) (point))))
    (let ((str (buffer-substring (point)
				 (save-excursion (search-forward "\n\n" end 'move)
						 (point)))))
      (insert str "*** EOOH ***\n")
      (narrow-to-region (point) (- (buffer-size) delta)))
    (goto-char (point-min))
    (if rmail-message-filter (funcall rmail-message-filter))
    (if (or rmail-displayed-headers rmail-ignored-headers)
	(rmail-clear-headers))))

(defun rmail-clear-headers (&optional ignored-headers)
  "Delete all header fields that Rmail should not show.
If the optional argument IGNORED-HEADERS is non-nil,
delete all header fields whose names match that regexp.
Otherwise, if `rmail-displayed-headers' is non-nil,
delete all header fields *except* those whose names match that regexp.
Otherwise, delete all header fields whose names match `rmail-ignored-headers'."
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
	  (while (and ignored-headers
		      (progn
			(goto-char (point-min))
			(re-search-forward ignored-headers nil t)))
	    (beginning-of-line)
	    (delete-region (point)
			   (if (re-search-forward "\n[^ \t]" nil t)
			       (1- (point))
			     (point-max)))))))))

(defun rmail-msg-is-pruned ()
  (rmail-maybe-set-message-counters)
  (save-restriction
    (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (= (following-char) ?1))))

(defun rmail-msg-restore-non-pruned-header ()
  (let ((old-point (point))
	new-point
	new-start
	(inhibit-read-only t))
    (save-excursion
      (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
      (goto-char (point-min))
      (forward-line 1)
      ;; Change 1 to 0.
      (delete-char 1)
      (insert ?0)
      ;; Insert new EOOH line at the proper place.
      (forward-line 1)
      (let ((case-fold-search t))
	(while (looking-at "Summary-Line:\\|Mail-From:")
	  (forward-line 1)))
      (insert "*** EOOH ***\n")
      (setq new-start (point))
      ;; Delete the old reformatted header.
      (forward-char -1)
      (search-forward "\n*** EOOH ***\n")
      (forward-line -1)
      (let ((start (point)))
	(search-forward "\n\n")
	(if (and (<= start old-point)
		 (<= old-point (point)))
	    (setq new-point new-start))
	(delete-region start (point)))
      ;; Narrow to after the new EOOH line.
      (narrow-to-region new-start (point-max)))
    (if new-point
	(goto-char new-point))))

(defun rmail-msg-prune-header ()
  (let ((new-point
	 (= (point) (point-min))))
    (save-excursion
      (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
      (rmail-reformat-message (point-min) (point-max)))
    (if new-point
	(goto-char (point-min)))))

(defun rmail-toggle-header (&optional arg)
  "Show original message header if pruned header currently shown, or vice versa.
With argument ARG, show the message header pruned if ARG is greater than zero;
otherwise, show it in full."
  (interactive "P")
  (let* ((pruned (with-current-buffer rmail-buffer
		   (rmail-msg-is-pruned)))
	 (prune (if arg
		    (> (prefix-numeric-value arg) 0)
		  (not pruned))))
    (if (eq pruned prune)
	t
      (set-buffer rmail-buffer)
      (rmail-maybe-set-message-counters)
      (if rmail-enable-mime
	  (let ((buffer-read-only nil))
	    (if pruned
		(rmail-msg-restore-non-pruned-header)
	      (rmail-msg-prune-header))
	    (funcall rmail-show-mime-function))
	(let* ((buffer-read-only nil)
	       (window (get-buffer-window (current-buffer)))
	       (at-point-min (= (point) (point-min)))
	       (all-headers-visible (= (window-start window) (point-min)))
	       (on-header
		(save-excursion
		  (and (not (search-backward "\n\n" nil t))
		       (progn
			 (end-of-line)
			 (re-search-backward "^[-A-Za-z0-9]+:" nil t))
		       (match-string 0))))
	       (old-screen-line
		(rmail-count-screen-lines (window-start window) (point))))
	  (if pruned
	      (rmail-msg-restore-non-pruned-header)
	    (rmail-msg-prune-header))
	  (cond (at-point-min
		 (goto-char (point-min)))
		(on-header
		 (goto-char (point-min))
		 (search-forward "\n\n")
		 (or (re-search-backward
		      (concat "^" (regexp-quote on-header)) nil t)
		     (goto-char (point-min))))
		(t
		 (save-selected-window
		   (select-window window)
		   (recenter old-screen-line)
		   (if (and all-headers-visible
			    (not (= (window-start) (point-min))))
		       (recenter (- (window-height) 2))))))))
      (rmail-highlight-headers))))

(defun rmail-narrow-to-non-pruned-header ()
  "Narrow to the whole (original) header of the current message."
  (let (start end)
    (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
    (goto-char (point-min))
    (forward-line 1)
    (if (= (following-char) ?1)
	(progn
	  (forward-line 1)
	  (setq start (point))
	  (search-forward "*** EOOH ***\n")
	  (setq end (match-beginning 0)))
      (forward-line 2)
      (setq start (point))
      (search-forward "\n\n")
      (setq end (1- (point))))
    (narrow-to-region start end)
    (goto-char start)))

;; Lifted from repos-count-screen-lines.
;; Return number of screen lines between START and END.
(defun rmail-count-screen-lines (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (vertical-motion (- (point-max) (point-min))))))

;;;; *** Rmail Attributes and Keywords ***

;; Make a string describing current message's attributes and keywords
;; and set it up as the name of a minor mode
;; so it will appear in the mode line.
(defun rmail-display-labels ()
  (let ((blurb "") (beg (point-min-marker)) (end (point-max-marker)))
    (save-excursion
      (unwind-protect
	  (progn
	    (widen)
	    (goto-char (rmail-msgbeg rmail-current-message))
	    (forward-line 1)
	    (if (looking-at "[01],")
		(progn
		  (narrow-to-region (point) (progn (end-of-line) (point)))
		  ;; Truly valid BABYL format requires a space before each
		  ;; attribute or keyword name.  Put them in if missing.
		  (let (buffer-read-only)
		    (goto-char (point-min))
		    (while (search-forward "," nil t)
		      (or (looking-at "[ ,]") (eobp)
			  (insert " "))))
		  (goto-char (point-max))
		  (if (search-backward ",," nil 'move)
		      (progn
			(if (> (point) (1+ (point-min)))
			    (setq blurb (buffer-substring (+ 1 (point-min)) (point))))
			(if (> (- (point-max) (point)) 2)
			    (setq blurb
				  (concat blurb
					  ";"
					  (buffer-substring (+ (point) 3)
							    (1- (point-max)))))))))))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
	(narrow-to-region beg end)
	(set-marker beg nil)
	(set-marker end nil)))
    (while (string-match " +," blurb)
      (setq blurb (concat (substring blurb 0 (match-beginning 0)) ","
			  (substring blurb (match-end 0)))))
    (while (string-match ", +" blurb)
      (setq blurb (concat (substring blurb 0 (match-beginning 0)) ","
			  (substring blurb (match-end 0)))))
    (setq mode-line-process
	  (format " %d/%d%s"
		  rmail-current-message rmail-total-messages blurb))
    ;; If rmail-enable-mime is non-nil, we may have to update
    ;; `mode-line-process' of rmail-view-buffer too.
    (if (and rmail-enable-mime
	     (not (eq (current-buffer) rmail-view-buffer))
	     (buffer-live-p rmail-view-buffer))
	(let ((mlp mode-line-process))
	  (with-current-buffer rmail-view-buffer
	    (setq mode-line-process mlp))))))

;; Turn an attribute of a message on or off according to STATE.
;; ATTR is the name of the attribute, as a string.
;; MSGNUM is message number to change; nil means current message.
(defun rmail-set-attribute (attr state &optional msgnum)
  (set-buffer rmail-buffer)
  (let ((omax (point-max-marker))
	(omin (point-min-marker))
	(buffer-read-only nil))
    (or msgnum (setq msgnum rmail-current-message))
    (if (> msgnum 0)
	(unwind-protect
	    (save-excursion
	      (widen)
	      (goto-char (+ 3 (rmail-msgbeg msgnum)))
	      (let ((curstate
		     (not
		      (null (search-backward (concat ", " attr ",")
					     (prog1 (point) (end-of-line)) t)))))
		(or (eq curstate (not (not state)))
		    (if curstate
			(delete-region (point) (1- (match-end 0)))
		      (beginning-of-line)
		      (forward-char 2)
		      (insert " " attr ","))))
	      (if (string= attr "deleted")
		  (rmail-set-message-deleted-p msgnum state)))
	  ;; Note: we don't use save-restriction because that does not work right
	  ;; if changes are made outside the saved restriction
	  ;; before that restriction is restored.
	  (narrow-to-region omin omax)
	  (set-marker omin nil)
	  (set-marker omax nil)
	  (if (= msgnum rmail-current-message)
	      (rmail-display-labels))))))

;; Return t if the attributes/keywords line of msg number MSG
;; contains a match for the regexp LABELS.
(defun rmail-message-labels-p (msg labels)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (rmail-msgbeg msg))
      (forward-char 3)
      (re-search-backward labels (prog1 (point) (end-of-line)) t))))

;;;; *** Rmail Message Selection And Support ***

(defun rmail-msgend (n)
  (marker-position (aref rmail-message-vector (1+ n))))

(defun rmail-msgbeg (n)
  (marker-position (aref rmail-message-vector n)))

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
	  (narrow-to-region (rmail-msgbeg rmail-current-message)
			    (point-max))
	  (goto-char (point-min))
	  (funcall function))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
      (narrow-to-region (rmail-msgbeg rmail-current-message)
			(rmail-msgend rmail-current-message)))))

(defun rmail-forget-messages ()
  (unwind-protect
      (if (vectorp rmail-message-vector)
	  (let* ((i 0)
		 (v rmail-message-vector)
		 (n (length v)))
	    (while (< i n)
	      (move-marker (aref v i)  nil)
	      (setq i (1+ i)))))
    (setq rmail-message-vector nil)
    (setq rmail-msgref-vector nil)
    (setq rmail-deleted-vector nil)))

(defun rmail-maybe-set-message-counters ()
  (if (not (and rmail-deleted-vector
		rmail-message-vector
		rmail-current-message
		rmail-total-messages))
      (rmail-set-message-counters)))

(defun rmail-count-new-messages (&optional nomsg)
  (let* ((case-fold-search nil)
	 (total-messages 0)
	 (messages-head nil)
	 (deleted-head nil))
    (or nomsg (message "Counting new messages..."))
    (goto-char (point-max))
    ;; Put at the end of messages-head
    ;; the entry for message N+1, which marks
    ;; the end of message N.  (N = number of messages).
    (search-backward "\n\^_")
    (forward-char 1)
    (setq messages-head (list (point-marker)))
    (rmail-set-message-counters-counter (point-min))
    (setq rmail-current-message (1+ rmail-total-messages))
    (setq rmail-total-messages
	  (+ rmail-total-messages total-messages))
    (setq rmail-message-vector
	  (vconcat rmail-message-vector (cdr messages-head)))
    (aset rmail-message-vector
	  rmail-current-message (car messages-head))
    (setq rmail-deleted-vector
	  (concat rmail-deleted-vector deleted-head))
    (setq rmail-summary-vector
	  (vconcat rmail-summary-vector (make-vector total-messages nil)))
    (setq rmail-msgref-vector
	  (vconcat rmail-msgref-vector (make-vector total-messages nil)))
    ;; Fill in the new elements of rmail-msgref-vector.
    (let ((i (1+ (- rmail-total-messages total-messages))))
      (while (<= i rmail-total-messages)
	(aset rmail-msgref-vector i (list i))
	(setq i (1+ i))))
    (goto-char (point-min))
    (or nomsg (message "Counting new messages...done (%d)" total-messages))))

(defun rmail-set-message-counters ()
  (rmail-forget-messages)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((point-save (point))
	     (total-messages 0)
	     (messages-after-point)
	     (case-fold-search nil)
	     (messages-head nil)
	     (deleted-head nil))
	(message "Counting messages...")
	(goto-char (point-max))
	;; Put at the end of messages-head
	;; the entry for message N+1, which marks
	;; the end of message N.  (N = number of messages).
	(search-backward "\n\^_" nil t)
	(if (/= (point) (point-max)) (forward-char 1))
	(setq messages-head (list (point-marker)))
	(rmail-set-message-counters-counter (min (point) point-save))
	(setq messages-after-point total-messages)
	(rmail-set-message-counters-counter)
	(setq rmail-total-messages total-messages)
	(setq rmail-current-message
	      (min total-messages
		   (max 1 (- total-messages messages-after-point))))
	(setq rmail-message-vector
	      (apply 'vector (cons (point-min-marker) messages-head))
	      rmail-deleted-vector (concat "0" deleted-head)
	      rmail-summary-vector (make-vector rmail-total-messages nil)
	      rmail-msgref-vector (make-vector (1+ rmail-total-messages) nil))
	(let ((i 0))
	  (while (<= i rmail-total-messages)
	    (aset rmail-msgref-vector i (list i))
	    (setq i (1+ i))))
	(message "Counting messages...done")))))
	
(defun rmail-set-message-counters-counter (&optional stop)
  (let ((start (point))
	next)
    (while (search-backward "\n\^_\^L" stop t)
      ;; Detect messages that have been added with DOS line endings and
      ;; convert the line endings for such messages.
      (setq next (point))
      (if (looking-at "\n\^_\^L\r\n")
	  (let ((buffer-read-only nil)
		(buffer-undo t))
	    (message "Counting messages...(converting line endings)")
	    (save-excursion
	      (goto-char start)
	      (while (search-backward "\r\n" next t)
		(delete-char 1)))))
      (setq start next)
      (forward-char 1)
      (setq messages-head (cons (point-marker) messages-head))
      (save-excursion
	(setq deleted-head
	      (cons (if (search-backward ", deleted,"
					 (prog1 (point)
					   (forward-line 2))
					 t)
			?D ?\ )
		    deleted-head)))
      (if (zerop (% (setq total-messages (1+ total-messages)) 20))
	  (message "Counting messages...%d" total-messages)))))

(defun rmail-beginning-of-message ()
  "Show current message starting from the beginning."
  (interactive)
  (rmail-show-message rmail-current-message))

(defun rmail-show-message (&optional n no-summary)
  "Show message number N (prefix argument), counting from start of file.
If summary buffer is currently displayed, update current message there also."
  (interactive "p")
  (or (eq major-mode 'rmail-mode)
      (switch-to-buffer rmail-buffer))
  (rmail-maybe-set-message-counters)
  (widen)
  (if (zerop rmail-total-messages)
      (progn (narrow-to-region (point-min) (1- (point-max)))
	     (goto-char (point-min))
	     (setq mode-line-process nil))
    (let (blurb coding-system)
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
      (let ((beg (rmail-msgbeg n)))
	(goto-char beg)
	(forward-line 1)
	(save-excursion
	  (let ((end (rmail-msgend n)))
	    (save-restriction
	      (if (prog1 (= (following-char) ?0)
		    (forward-line 2)
		    ;; If there's a Summary-line in the (otherwise empty)
		    ;; header, we didn't yet get past the EOOH line.
		    (if (looking-at "^\\*\\*\\* EOOH \\*\\*\\*\n")
			(forward-line 1))
		    (narrow-to-region (point) end))
		  (rfc822-goto-eoh)
		(search-forward "\n*** EOOH ***\n" end t))
	      (narrow-to-region beg (point))
	      (goto-char (point-min))
	      (if (re-search-forward "^X-Coding-System: *\\(.*\\)$" nil t)
		  (let ((coding-system (intern (match-string 1))))
		    (check-coding-system coding-system)
		    (setq buffer-file-coding-system coding-system))
		(setq buffer-file-coding-system nil)))))
	;; Clear the "unseen" attribute when we show a message.
	(rmail-set-attribute "unseen" nil)
	(let ((end (rmail-msgend n)))
	  ;; Reformat the header, or else find the reformatted header.
	  (if (= (following-char) ?0)
	      (rmail-reformat-message beg end)
	    (search-forward "\n*** EOOH ***\n" end t)
	    (narrow-to-region (point) end)))
	(goto-char (point-min))
	(walk-windows
	 (function (lambda (window)
		     (if (eq (window-buffer window) (current-buffer))
			 (set-window-point window (point)))))
	 nil t)
	(rmail-display-labels)
	(if (eq rmail-enable-mime t)
	    (funcall rmail-show-mime-function)
	  (setq rmail-view-buffer rmail-buffer)
	  )
	(rmail-highlight-headers)
	(if transient-mark-mode (deactivate-mark))
	(run-hooks 'rmail-show-message-hook)
	;; If there is a summary buffer, try to move to this message
	;; in that buffer.  But don't complain if this message
	;; is not mentioned in the summary.
	;; Don't do this at all if we were called on behalf
	;; of cursor motion in the summary buffer.
	(and (rmail-summary-exists) (not no-summary)
	     (let ((curr-msg rmail-current-message))
	       (rmail-select-summary
		(rmail-summary-goto-msg curr-msg t t))))
	(with-current-buffer rmail-buffer
	  (rmail-auto-file))
	(if blurb
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
  (when (not rmail-enable-mime)
    (or (eq major-mode 'rmail-mode)
	(switch-to-buffer rmail-buffer))
    (save-excursion
      (let ((pruned (rmail-msg-is-pruned)))
	(unwind-protect
	    (let ((msgbeg (rmail-msgbeg rmail-current-message))
		  (msgend (rmail-msgend rmail-current-message))
		  x-coding-header)
	      ;; We need the message headers pruned (we later restore
	      ;; the pruned stat to what it was, see the end of
	      ;; unwind-protect form).
	      (or pruned
		  (rmail-toggle-header 1))
	      (narrow-to-region msgbeg msgend)
	      (goto-char (point-min))
	      (when (search-forward "\n*** EOOH ***\n" (point-max) t)
		(narrow-to-region msgbeg (point)))
	      (goto-char (point-min))
	      (if (re-search-forward "^X-Coding-System: *\\(.*\\)$" nil t)
		  (let ((old-coding (intern (match-string 1)))
			(buffer-read-only nil))
		    (check-coding-system old-coding)
		    ;; Make sure the new coding system uses the same EOL
		    ;; conversion, to prevent ^M characters from popping
		    ;; up all over the place.
		    (setq coding
			  (coding-system-change-eol-conversion
			   coding
			   (coding-system-eol-type old-coding)))
		    (setq x-coding-header (point-marker))
		    (narrow-to-region msgbeg msgend)
		    (encode-coding-region (point) msgend old-coding)
		    (decode-coding-region (point) msgend coding)
		    (setq last-coding-system-used coding)
		    ;; Rewrite the coding-system header according
		    ;; to what we did.
		    (goto-char x-coding-header)
		    (delete-region (point)
				   (save-excursion
				     (beginning-of-line)
				     (point)))
		    (insert "X-Coding-System: "
			    (symbol-name last-coding-system-used))
		    (set-marker x-coding-header nil)
		    (rmail-show-message))
		(error "No X-Coding-System header found")))
	  (or pruned
	      (rmail-toggle-header 0)))))))

;; Find all occurrences of certain fields, and highlight them.
(defun rmail-highlight-headers ()
  ;; Do this only if the system supports faces.
  (if (and (fboundp 'internal-find-face)
	   rmail-highlighted-headers)
      (save-excursion
	(search-forward "\n\n" nil 'move)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (let ((case-fold-search t)
		(inhibit-read-only t)
		;; Highlight with boldface if that is available.
		;; Otherwise use the `highlight' face.
		(face (or rmail-highlight-face
			  (if (face-differs-from-default-p 'bold)
			      'bold 'highlight)))
		;; List of overlays to reuse.
		(overlays rmail-overlay-list))
	    (goto-char (point-min))
	    (while (re-search-forward rmail-highlighted-headers nil t)
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
		  ;; rmail-overlay-list.
		  (setq overlay (make-overlay beg (point)))
		  (overlay-put overlay 'face face)
		  (setq rmail-overlay-list
			(cons overlay rmail-overlay-list))))))))))

(defun rmail-auto-file ()
  "Automatically move a message into a sub-folder based on criteria.
Called when a new message is displayed."
  (if (or (rmail-message-labels-p rmail-current-message "filed")
	  (not (string= (buffer-file-name)
			(expand-file-name rmail-file-name))))
      ;; Do nothing if it's already been filed.
      nil
    ;; Find out some basics (common fields)
    (let ((from (mail-fetch-field "from"))
	  (subj (mail-fetch-field "subject"))
	  (to   (concat (mail-fetch-field "to") "," (mail-fetch-field "cc")))
	  (d rmail-automatic-folder-directives)
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
		(rmail-delete-forward)
	      (if (string= "/dev/null" folder)
		  (rmail-delete-message)
		(rmail-output-to-rmail-file folder 1 t)
		(setq d nil))))
	(setq d (cdr d))))))

(defun rmail-next-message (n)
  "Show following message whether deleted or not.
With prefix arg N, moves forward N messages, or backward if N is negative."
  (interactive "p")
  (set-buffer rmail-buffer)
  (rmail-maybe-set-message-counters)
  (rmail-show-message (+ rmail-current-message n)))

(defun rmail-previous-message (n)
  "Show previous message whether deleted or not.
With prefix arg N, moves backward N messages, or forward if N is negative."
  (interactive "p")
  (rmail-next-message (- n)))  

(defun rmail-next-undeleted-message (n)
  "Show following non-deleted message.
With prefix arg N, moves forward N non-deleted messages,
or backward if N is negative.

Returns t if a new message is being shown, nil otherwise."
  (interactive "p")
  (set-buffer rmail-buffer)
  (rmail-maybe-set-message-counters)
  (let ((lastwin rmail-current-message)
	(current rmail-current-message))
    (while (and (> n 0) (< current rmail-total-messages))
      (setq current (1+ current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1- n))))
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1+ n))))
    (if (/= lastwin rmail-current-message)
 	(progn (rmail-show-message lastwin)
 	       t)
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
  (rmail-maybe-set-message-counters)
  (rmail-show-message 1))

(defun rmail-last-message ()
  "Show last message in file."
  (interactive)
  (rmail-maybe-set-message-counters)
  (rmail-show-message rmail-total-messages))

(defun rmail-what-message ()
  (let ((where (point))
	(low 1)
	(high rmail-total-messages)
	(mid (/ rmail-total-messages 2)))
    (while (> (- high low) 1)
      (if (>= where (rmail-msgbeg mid))
	  (setq low mid)
	(setq high mid))
      (setq mid (+ low (/ (- high low) 2))))
    (if (>= where (rmail-msgbeg high)) high low)))

(defun rmail-message-recipients-p (msg recipients &optional primary-only)
  (save-restriction
    (goto-char (rmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
    (or (string-match recipients (or (mail-fetch-field "To") ""))
	(string-match recipients (or (mail-fetch-field "From") ""))
	(if (not primary-only)
	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(defun rmail-message-regexp-p (n regexp)
  "Return t, if for message number N, regexp REGEXP matches in the header."
  (let ((beg (rmail-msgbeg n))
	(end (rmail-msgend n)))
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
	(if rmail-enable-mime
	    (funcall rmail-search-mime-header-function n regexp end)
	  (re-search-forward regexp end t)))))

(defun rmail-search-message (msg regexp)
  "Return non-nil, if for message number MSG, regexp REGEXP matches."
  (goto-char (rmail-msgbeg msg))
  (if rmail-enable-mime
      (funcall rmail-search-mime-message-function msg regexp)
    (re-search-forward regexp (rmail-msgend msg) t)))

(defvar rmail-search-last-regexp nil)
(defun rmail-search (regexp &optional n)
  "Show message containing next match for REGEXP (but not the current msg).
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (< (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp): "))
	   regexp)
      (if rmail-search-last-regexp
	  (setq prompt (concat prompt
			       "(default "
			       rmail-search-last-regexp
			       ") ")))
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
  (rmail-maybe-set-message-counters)
  (let ((omin (point-min))
	(omax (point-max))
	(opoint (point))
	win
	(reversep (< n 0))
	(msg rmail-current-message))
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
	    (concat (if reversep "Reverse " "") "Rmail search (regexp): "))
	   regexp)
      (if rmail-search-last-regexp
	  (setq prompt (concat prompt
			       "(default "
			       rmail-search-last-regexp
			       ") ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  (rmail-search regexp (- (or n 1))))

;; Show the first message which has the `unseen' attribute.
(defun rmail-first-unseen-message ()
  (rmail-maybe-set-message-counters)
  (let ((current 1)
	found)
    (save-restriction
      (widen)
      (while (and (not found) (<= current rmail-total-messages))
	(if (rmail-message-labels-p current ", ?\\(unseen\\),")
	    (setq found current))
	(setq current (1+ current))))
;; Let the caller show the message.
;;    (if found
;;	(rmail-show-message found))
    found))

(defun rmail-next-same-subject (n)
  "Go to the next mail message having the same subject header.
With prefix argument N, do this N times.
If N is negative, go backwards instead."
  (interactive "p")
  (let ((subject (mail-fetch-field "Subject"))
	(forward (> n 0))
	(i rmail-current-message)
	(case-fold-search t)
	search-regexp found)
    (if (string-match "\\`[ \t]+" subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match "Re:[ \t]*" subject)
	(setq subject (substring subject (match-end 0))))
    (if (string-match "[ \t]+\\'" subject)
	(setq subject (substring subject 0 (match-beginning 0))))
    (setq search-regexp (concat "^Subject: *\\(Re:[ \t]*\\)?"
				(regexp-quote subject)
				"[ \t]*\n"))
    (save-excursion
      (save-restriction
	(widen)
	(while (and (/= n 0)
		    (if forward
			(< i rmail-total-messages)
		      (> i 1)))
	  (let (done)
	    (while (and (not done)
			(if forward
			    (< i rmail-total-messages)
			  (> i 1)))
	      (setq i (if forward (1+ i) (1- i)))
	      (goto-char (rmail-msgbeg i))
	      (search-forward "\n*** EOOH ***\n")
	      (let ((beg (point)) end)
		(search-forward "\n\n")
		(setq end (point))
		(goto-char beg)
		(setq done (re-search-forward search-regexp end t))))
	    (if done (setq found i)))
	  (setq n (if forward (1- n) (1+ n))))))
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

(defun rmail-message-deleted-p (n)
  (= (aref rmail-deleted-vector n) ?D))

(defun rmail-set-message-deleted-p (n state)
  (aset rmail-deleted-vector n (if state ?D ?\ )))

(defun rmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (rmail-set-attribute "deleted" t)
  (run-hooks 'rmail-delete-message-hook))

(defun rmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
  (set-buffer rmail-buffer)
  (let ((msg rmail-current-message))
    (while (and (> msg 0)
		(not (rmail-message-deleted-p msg)))
      (setq msg (1- msg)))
    (if (= msg 0)
	(error "No previous deleted message")
      (if (/= msg rmail-current-message)
	  (rmail-show-message msg))
      (rmail-set-attribute "deleted" nil)
      (if (rmail-summary-exists)
	  (save-excursion
	    (set-buffer rmail-summary-buffer)
	    (rmail-summary-mark-undeleted msg)))
      (rmail-maybe-display-summary))))

(defun rmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward.

Returns t if a new message is displayed after the delete, or nil otherwise."
  (interactive "P")
  (rmail-set-attribute "deleted" t)
  (run-hooks 'rmail-delete-message-hook)
  (let ((del-msg rmail-current-message))
    (if (rmail-summary-exists)
	(rmail-select-summary
	 (rmail-summary-mark-deleted del-msg)))
    (prog1 (rmail-next-undeleted-message (if backward -1 1))
      (rmail-maybe-display-summary))))

(defun rmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-delete-forward t))

;; Compute the message number a given message would have after expunging.
;; The present number of the message is OLDNUM.
;; DELETEDVEC should be rmail-deleted-vector.
;; The value is nil for a message that would be deleted.
(defun rmail-msg-number-after-expunge (deletedvec oldnum)
  (if (or (null oldnum) (= (aref deletedvec oldnum) ?D))
      nil
    (let ((i 0)
	  (newnum 0))
      (while (< i oldnum)
	(if (/= (aref deletedvec i) ?D)
	    (setq newnum (1+ newnum)))
	(setq i (1+ i)))
      newnum)))

(defun rmail-expunge-confirmed ()
  "Return t if deleted message should be expunged. If necessary, ask the user.
See also user-option `rmail-confirm-expunge'."
  (set-buffer rmail-buffer)
  (or (not (stringp rmail-deleted-vector))
      (not (string-match "D" rmail-deleted-vector))
      (null rmail-confirm-expunge)
      (funcall rmail-confirm-expunge
	       "Erase deleted messages from Rmail file? ")))

(defun rmail-only-expunge ()
  "Actually erase all deleted messages in the file."
  (interactive)
  (set-buffer rmail-buffer)
  (message "Expunging deleted messages...")
  ;; Discard all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (rmail-maybe-set-message-counters)
  (let* ((omax (- (buffer-size) (point-max)))
	 (omin (- (buffer-size) (point-min)))
	 (opoint (if (and (> rmail-current-message 0)
			  (rmail-message-deleted-p rmail-current-message))
		     0
		   (if rmail-enable-mime
		       (with-current-buffer rmail-view-buffer
			 (- (point)(point-min)))
		     (- (point) (point-min)))))
	 (messages-head (cons (aref rmail-message-vector 0) nil))
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
		(total rmail-total-messages)
		(new-message-number rmail-current-message)
		(new-summary nil)
		(new-msgref (list (list 0)))
		(rmailbuf (current-buffer))
		(buffer-read-only nil)
		(messages rmail-message-vector)
		(deleted rmail-deleted-vector)
		(summary rmail-summary-vector))
	    (setq rmail-total-messages nil
		  rmail-current-message nil
		  rmail-message-vector nil
		  rmail-deleted-vector nil
		  rmail-summary-vector nil)

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
		      (cons (aref rmail-msgref-vector number)
			    new-msgref))
		(setcar (car new-msgref) counter))
	      (if (zerop (% (setq number (1+ number)) 20))
		  (message "Expunging deleted messages...%d" number)))
	    (setq messages-tail
		  (setcdr messages-tail
			  (cons (aref messages number) nil)))
	    (setq rmail-current-message new-message-number
		  rmail-total-messages counter
		  rmail-message-vector (apply 'vector messages-head)
		  rmail-deleted-vector (make-string (1+ counter) ?\ )
		  rmail-summary-vector (vconcat (nreverse new-summary))
		  rmail-msgref-vector (apply 'vector (nreverse new-msgref))
		  win t)))
      (message "Expunging deleted messages...done")
      (if (not win)
	  (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax)))
      (rmail-show-message
       (if (zerop rmail-current-message) 1 nil))
      (if rmail-enable-mime
	  (goto-char (+ (point-min) opoint))
	(goto-char (+ (point) opoint))))))

(defun rmail-expunge ()
  "Erase deleted messages from Rmail file and summary buffer."
  (interactive)
  (when (rmail-expunge-confirmed)
    (rmail-only-expunge)
    (if (rmail-summary-exists)
	(rmail-select-summary (rmail-update-summary)))))

;;;; *** Rmail Mailing Commands ***

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
      (if (and (display-multi-frame-p) rmail-mail-new-frame)
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
  (let (from reply-to cc subject date to message-id references
	     resent-to resent-cc resent-reply-to
	     (msgnum rmail-current-message))
    (save-excursion
      (save-restriction
	(if rmail-enable-mime
	    (narrow-to-region
	     (goto-char (point-min))
	     (if (search-forward "\n\n" nil 'move)
		 (1+ (match-beginning 0))
	       (point)))
	  (widen)
	  (goto-char (rmail-msgbeg rmail-current-message))
	  (forward-line 1)
	  (if (= (following-char) ?0)
	      (narrow-to-region
	       (progn (forward-line 2)
		      (point))
	       (progn (search-forward "\n\n" (rmail-msgend rmail-current-message)
				      'move)
		      (point)))
	    (narrow-to-region (point)
			      (progn (search-forward "\n*** EOOH ***\n")
				     (beginning-of-line) (point)))))
	(setq from (mail-fetch-field "from")
	      reply-to (or (mail-fetch-field "reply-to" nil t)
			   from)
	      cc (and (not just-sender)
		      (mail-fetch-field "cc" nil t))
	      subject (mail-fetch-field "subject")
	      date (mail-fetch-field "date")
	      to (or (mail-fetch-field "to" nil t) "")
	      message-id (mail-fetch-field "message-id")
	      references (mail-fetch-field "references" nil nil t)
	      resent-reply-to (mail-fetch-field "resent-reply-to" nil t)
	      resent-cc (and (not just-sender)
			     (mail-fetch-field "resent-cc" nil t))
	      resent-to (or (mail-fetch-field "resent-to" nil t) "")
;;;	      resent-subject (mail-fetch-field "resent-subject")
;;;	      resent-date (mail-fetch-field "resent-date")
;;;	      resent-message-id (mail-fetch-field "resent-message-id")
	      )))
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
    (rmail-start-mail
     nil
     ;; Using mail-strip-quoted-names is undesirable with newer mailers
     ;; since they can handle the names unstripped.
     ;; I don't know whether there are other mailers that still
     ;; need the names to be stripped.
     (mail-strip-quoted-names reply-to)
     subject
     (rmail-make-in-reply-to-field from date message-id)
     (if just-sender
	 nil
       ;; mail-strip-quoted-names is NOT necessary for rmail-dont-reply-to
       ;; to do its job.
       (let* ((cc-list (rmail-dont-reply-to
			(mail-strip-quoted-names
			 (if (null cc) to (concat to ", " cc))))))
	 (if (string= cc-list "") nil cc-list)))
     rmail-view-buffer
     (list (list 'rmail-mark-message
		 rmail-buffer
		 (with-current-buffer rmail-buffer
		   (aref rmail-msgref-vector msgnum))
		 "answered"))
     nil
     (list (cons "References" (concat (mapconcat 'identity references " ")
				      " " message-id))))))

(defun rmail-mark-message (buffer msgnum-list attribute)
  "Give BUFFER's message number in MSGNUM-LIST the attribute ATTRIBUTE.
This is use in the send-actions for message buffers.
MSGNUM-LIST is a list of the form (MSGNUM)
which is an element of rmail-msgref-vector."
  (save-excursion
    (set-buffer buffer)
    (if (car msgnum-list)
	(rmail-set-attribute attribute t (car msgnum-list)))))

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

(defun rmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
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
			 (aref rmail-msgref-vector msgnum))
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
  (require 'sendmail)
  (require 'mailalias)
  (unless (or (eq rmail-view-buffer (current-buffer))
	      (eq rmail-buffer (current-buffer)))
    (error "Not an Rmail buffer"))
  (if (not from) (setq from user-mail-address))
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
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
  (require 'mail-utils)
  (let ((rmail-this-buffer (current-buffer))
	(msgnum rmail-current-message)
	bounce-start bounce-end bounce-indent resending
	;; Fetch any content-type header in current message
	;; Must search thru the whole unpruned header.
	(content-type
	 (save-excursion
	   (save-restriction
	     (rmail-narrow-to-non-pruned-header)
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
  ;; Fontify the current message if it is not already fontified.
  (if (text-property-any (point-min) (point-max) 'rmail-fontified nil)
      (let ((modified (buffer-modified-p))
	    (buffer-undo-list t) (inhibit-read-only t)
	    before-change-functions after-change-functions
	    buffer-file-name buffer-file-truename)
	(save-excursion
	  (save-match-data
	    (add-text-properties (point-min) (point-max) '(rmail-fontified t))
	    (font-lock-fontify-region (point-min) (point-max))
	    (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil)))))))

;;; Speedbar support for RMAIL files.
(eval-when-compile (require 'speedbar))

(defvar rmail-speedbar-match-folder-regexp "^[A-Z0-9]+\\(\\.[A-Z0-9]+\\)?$"
  "*This regex us used to match folder names to be displayed in speedbar.
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
(defun rmail-set-pop-password (password)
  "Set PASSWORD to be used for retrieving mail from a POP server."
  (interactive "sPassword: ")
  (if password
      (setq rmail-encoded-pop-password 
	    (rmail-encode-string password (emacs-pid)))
    (setq rmail-pop-password nil)
    (setq rmail-encoded-pop-password nil)))

(defun rmail-get-pop-password ()
  "Get the password for retrieving mail from a POP server.  If none
has been set, then prompt the user for one."
  (if (not rmail-encoded-pop-password)
      (progn (if (not rmail-pop-password)
		 (setq rmail-pop-password (read-passwd "POP password: ")))
	     (rmail-set-pop-password rmail-pop-password)
	     (setq rmail-pop-password nil)))
  (rmail-encode-string rmail-encoded-pop-password (emacs-pid)))

(defun rmail-have-password ()
  (or rmail-pop-password rmail-encoded-pop-password))

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

(provide 'rmail)

;;; rmail.el ends here
