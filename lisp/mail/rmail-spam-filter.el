;;; rmail-spam-filter.el  --- spam filter for rmail, the emacs mail reader.

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;; Keywords: email, spam, filter, rmail
;; Author: Eli Tziperman <eli AT deas.harvard.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;;; -----------

;;; Automatically recognize and delete junk email before it is
;;; displayed in rmail/rmail-summary.  Spam emails are defined by
;;; specifying one or more of the sender, subject and contents.
;;; URL: http://www.weizmann.ac.il/~eli/Downloads/rmail-spam-filter/

;;; Usage:
;;; ------

;;; put in your .emacs:

;;; (load "rmail-spam-filter.el")

;;; and use customize (in rmail-spam-filter group) to:

;;; (*) turn on the variable rmail-use-spam-filter,

;;; (*) specify in variable rsf-definitions-alist what sender,
;;; subject and contents make an email be considered spam.

;;; in addition, you may:

;;; (*) Block future mail with the subject or sender of a message
;;; while reading it in RMAIL: just click on the "Spam" item on the
;;; menubar, and add the subject or sender to the list of spam
;;; definitions using the mouse and the appropriate menu item. You
;;; need to later also save the list of spam definitions using the
;;; same menu item, or alternatively, see variable
;;; `rsf-autosave-newly-added-definitions'.

;;; (*) specify if blind-cc'ed mail (no "To:" header field) is to be
;;; treated as spam (variable rsf-no-blind-cc; Thanks to Ethan
;;; Brown <ethan@gso.saic.com> for this).

;;; (*) specify if rmail-spam-filter should ignore case of spam
;;; definitions (variable rsf-ignore-case; Thanks to
;;; Ethan Brown <ethan@gso.saic.com> for the suggestion).

;;; (*) Specify a "white-list" of trusted senders. If any
;;; rsf-white-list string matches a substring of the "From"
;;; header, the message is flagged as a valid, non-spam message (Ethan
;;; Brown <ethan@gso.saic.com>).

;;; (*) rmail-spam-filter is best used with a general purpose spam
;;; filter such as the procmail-based http://www.spambouncer.org/.
;;; Spambouncer is set to only mark messages as spam/blocked/bulk/OK
;;; via special headers, and these headers may then be defined in
;;; rmail-spam-filter such that the spam is rejected by
;;; rmail-spam-filter itself.

;;; (*) rmail spam filter also works with bbdb to prevent spam senders
;;; from entering into the .bbdb file.  See variable
;;; "rsf-auto-delete-spam-bbdb-entries".  This is done
;;; in two ways: (a) bbdb is made not to auto-create entries for
;;; messages that are deleted by the rmail-spam-filter, (b) when a
;;; message is deleted in rmail, the user is offered to delete the
;;; sender's bbdb entry as well _if_ it was created at the same day.

(require 'rmail)
(if (> emacs-major-version 20)
    (require 'rmailsum)
  (if (not (fboundp 'rmail-make-summary-line)) (load-library "rmailsum")))

(defvar bbdb/mail_auto_create_p)
(defvar rmail-summary-mode-map)

;; For find-if and other cool common lisp functions we may want to use.
(eval-when-compile
  (require 'cl))

(defgroup rmail-spam-filter nil
  "Spam filter for RMAIL, the mail reader for Emacs."
  :group 'rmail)

(defcustom rmail-use-spam-filter nil
  "*Non-nil to activate the rmail spam filter.
Specify `rsf-definitions-alist' to define what you consider spam
emails."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rsf-file "~/XRMAIL-SPAM"
  "*Name of rmail file for optionally saving some of the spam.
Spam may be either just deleted, or saved in a separate spam file to
be looked at at a later time.  Whether the spam is just deleted or
also saved in a separete spam file is specified for each definition of
spam, as one of the fields of `rsf-definitions-alist'"
  :type 'string
  :group 'rmail-spam-filter )

(defcustom rsf-no-blind-cc nil
  "*Non-nil to treat blind CC (no To: header) as spam."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rsf-ignore-case nil
  "*Non-nil to ignore case in `rsf-definitions-alist'."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rsf-beep nil
  "*Non-nil to beep if spam is found."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rsf-sleep-after-message 2.0
  "*Seconds to wait after display of message that spam was found."
  :type 'number
  :group 'rmail-spam-filter )

(defcustom rsf-min-region-to-spam-list 7
  "*Minimum size of region that you can add to the spam list.
This is a size limit on text that you can specify as
indicating a message is spam.  The aim is to avoid
accidentally adding a too short region, which would result
in false positive identification of spam."
  :type 'integer
  :group 'rmail-spam-filter )

(defcustom rsf-auto-delete-spam-bbdb-entries nil
  "*Non-nil to make sure no entries are made in bbdb for spam emails.
This is done in two ways: (1) bbdb is made not to auto-create entries
for messages that are deleted by the `rmail-spam-filter', (2) when a
message is deleted in rmail, the user is offered to delete the
sender's bbdb entry as well if it was created at the same day.  Note
that Emacs needs to be restarted after setting this option for it to
take an effect."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rsf-autosave-newly-added-definitions nil
  "*Non-nil to auto save new spam entries.
New entries entered via the spam menu bar item are then saved to
customization file immediately after being added via the menu bar, and
do not require explicitly saving the file after adding the new
entries."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rsf-white-list nil
  "*List of strings to identify valid senders.
If any rsf-white-list string matches a substring of the 'From'
header, the message is flagged as a valid, non-spam message.  Example:
If your domain is emacs.com then including 'emacs.com' in your
rsf-white-list would flag all mail from your colleagues as
valid."
  :type '(repeat string)
  :group 'rmail-spam-filter )

(defcustom rsf-definitions-alist nil
  "*Alist matching strings defining what messages are considered spam.
Each definition may contain specifications of one or more of the
elements {subject, sender, recipients or contents}, as well as a
definition of what to do with the spam (action item).  A spam e-mail
is defined as one that fits all of the specified elements of any one
of the spam definitions.  The strings that specify spam subject,
sender, etc, may be regexp.  For example, to specify that the subject
may be either 'this is spam' or 'another spam', use the regexp: 'this
is spam\\|another spam' (without the single quotes).  To specify that
if the contents contain both this and that the message is spam,
specify 'this\\&that' in the appropriate spam definition field."
  :type '(repeat
          (list :format "%v"
	   (cons :format "%v" :value (from . "")
		 (const :format ""  from)
		 (string :tag "From"  ""))
	   (cons :format "%v" :value (to . "")
		 (const :format ""  to)
		 (string :tag "To"  ""))
	   (cons :format "%v" :value (subject . "")
		 (const :format ""  subject)
		 (string :tag "Subject"  ""))
	   (cons :format "%v" :value (content-type . "")
		 (const :format ""  content-type)
		 (string :tag "Content-Type"  ""))
	   (cons :format "%v" :value (contents . "")
		 (const :format ""  contents)
		 (string :tag "Contents"  ""))
	   (cons :format "%v" :value (x-spam-status . "")
		 (const :format ""  x-spam-status)
		 (string :tag "X-Spam-Status"  ""))
	   (cons :format "%v" :value (action . output-and-delete)
		 (const :format "" action)
		 (choice :tag "Action selection"
		  (const :tag "output to spam folder and delete" output-and-delete)
		  (const :tag "delete spam" delete-spam)
		  ))
   ))
  :group 'rmail-spam-filter)

(defvar rsf-scanning-messages-now nil
  "Non-nil when `rmail-spam-filter' scans messages.
This is for interaction with `rsf-bbdb-auto-delete-spam-entries'.")

;; the advantage over the automatic filter definitions is the AND conjunction
;; of in-one-definition-elements
(defun check-field (field-symbol message-data definition result)
  "Check if field-symbol is in `rsf-definitions-alist'.
Capture maybe-spam and this-is-a-spam-email in a cons in result,
where maybe-spam is in first and this-is-a-spam-email is in rest.
The values are returned by destructively changing result.
If FIELD-SYMBOL field does not exist AND is not specified,
this may still be spam due to another element...
if (first result) is nil, we already have a contradiction in another
field"
  (let ((definition-field (cdr (assoc field-symbol definition))))
    (if (and (first result) (> (length definition-field) 0))
        ;; only in this case can maybe-spam change from t to nil
        ;; ... else, if FIELD-SYMBOL field does appear in the message,
        ;; and it also appears in spam definition list, this
        ;; is potentially a spam:
        (if (and message-data
                 (string-match definition-field message-data))
            ;; if we do not get a contradiction from another field, this is
            ;; spam
            (setf (rest result) t)
          ;; the message data contradicts the specification, this is no spam
          (setf (first result) nil)))))

(defun rmail-spam-filter (msg)
  "Return nil if msg is spam based on rsf-definitions-alist.
If spam, optionally output msg to a file `rsf-file' and delete
it from rmail file.  Called for each new message retrieved by
`rmail-get-new-mail'."

  (let ((old-message)
	(return-value)
	(this-is-a-spam-email)
	(maybe-spam)
	(message-sender)
	(message-recipients)
	(message-subject)
	(message-content-type)
	(message-spam-status)
	(num-spam-definition-elements)
	(num-element 0)
	(exit-while-loop nil)
	(saved-case-fold-search case-fold-search)
	(save-current-msg)
	(rsf-saved-bbdb/mail_auto_create_p nil)
	)

    ;; make sure bbdb does not create entries for messages while spam
    ;; filter is scanning the rmail file:
    (setq rsf-saved-bbdb/mail_auto_create_p 'bbdb/mail_auto_create_p)
    (setq bbdb/mail_auto_create_p nil)
    ;; let `rsf-bbdb-auto-delete-spam-entries' know that rmail spam
    ;; filter is running, so that deletion of rmail messages should be
    ;; ignored for now:
    (setq rsf-scanning-messages-now t)
    (save-excursion
      (save-restriction
	(setq this-is-a-spam-email nil)
	;; Narrow buffer to header of message and get Sender and
	;; Subject fields to be used below:
	(save-restriction
	  (goto-char (rmail-msgbeg msg))
	  (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
	  (setq message-sender (mail-fetch-field "From"))
	  (setq message-recipients
		(concat (mail-fetch-field "To")
			(if (mail-fetch-field "Cc")
			    (concat ", " (mail-fetch-field "Cc")))))
	  (setq message-subject (mail-fetch-field "Subject"))
	  (setq message-content-type (mail-fetch-field "Content-Type"))
	  (setq message-spam-status (mail-fetch-field "X-Spam-Status"))
	  )
	;; Find number of spam-definition elements in the list
	;; rsf-definitions-alist specified by user:
	(setq num-spam-definition-elements (safe-length
					    rsf-definitions-alist))

	;;; do we want to ignore case in spam definitions:
	  (setq case-fold-search rsf-ignore-case)

	;; Check for blind CC condition.  Set vars such that while
	;; loop will be bypassed and spam condition will trigger
	(if (and rsf-no-blind-cc
		 (null message-recipients))
	    (setq exit-while-loop t
                  maybe-spam t
                  this-is-a-spam-email t))

        ;; Check white list, and likewise cause while loop
        ;;  bypass.
        (if (and message-sender
                 (let ((white-list rsf-white-list)
                       (found nil))
                   (while (and (not found) white-list)
                     (if (string-match (car white-list) message-sender)
                         (setq found t)
                       (setq white-list (cdr white-list))))
                   found))
            (setq exit-while-loop t
                  maybe-spam nil
                  this-is-a-spam-email nil))

        ;; maybe-spam is in first, this-is-a-spam-email in rest, this
        ;; simplifies the call to check-field
        (setq maybe-spam (cons maybe-spam this-is-a-spam-email))

	;; scan all elements of the list rsf-definitions-alist
	(while (and
		(< num-element num-spam-definition-elements)
		(not exit-while-loop))
          (let ((definition (nth num-element rsf-definitions-alist)))
	    ;; Initialize maybe-spam which is set to t in one of two
	    ;; cases: (1) unspecified definition-elements are found in
	    ;; rsf-definitions-alist, (2) empty field is found
	    ;; in the message being scanned (e.g. empty subject,
	    ;; sender, recipients, etc).  The variable is set to nil
	    ;; if a non empty field of the scanned message does not
	    ;; match a specified field in
	    ;; rsf-definitions-alist.

	    ;; initialize this-is-a-spam-email to nil.  This variable
	    ;; is set to t if one of the spam definitions matches a
	    ;; field in the scanned message.
            (setq maybe-spam (cons t nil))

	    ;; start scanning incoming message:
	    ;;---------------------------------

            ;; Maybe the different fields should also be done in a
            ;; loop to make the whole thing more flexible
 	    ;; if sender field is not specified in message being
	    ;; scanned, AND if "from" field does not appear in spam
	    ;; definitions for this element, this may still be spam
	    ;; due to another element...
            (check-field 'from message-sender definition maybe-spam)
 	    ;; next, if spam was not ruled out already, check recipients:
            (check-field 'to message-recipients definition maybe-spam)
 	    ;; next, if spam was not ruled out already, check subject:
            (check-field 'subject message-subject definition maybe-spam)
 	    ;; next, if spam was not ruled out already, check content-type:
            (check-field 'content-type message-content-type
                         definition maybe-spam)
	    ;; next, if spam was not ruled out already, check
	    ;; contents: if contents field is not specified, this may
	    ;; still be spam due to another element...
            (check-field 'contents
                         (buffer-substring
                          (rmail-msgbeg msg) (rmail-msgend msg))
                         definition maybe-spam)

	    ;; finally, check the X-Spam-Status header.  You will typically
	    ;; look for the "Yes" string in this header field
	    (check-field 'x-spam-status message-spam-status
			 definition maybe-spam)

	    ;; if the search in rsf-definitions-alist found
	    ;; that this email is spam, output the email to the spam
	    ;; rmail file, mark the email for deletion, leave the
	    ;; while loop and return nil so that an rmail summary line
	    ;; wont be displayed for this message:
	    (if (and (first maybe-spam) (rest maybe-spam))
		;; found that this is spam, no need to look at the
		;; rest of the rsf-definitions-alist, exit
		;; loop:
		(setq exit-while-loop t)
	      ;; else, spam was not yet found, increment number of
	      ;; element in rsf-definitions-alist and proceed
	      ;; to next element:
	      (setq num-element (+ num-element 1)))
	    )
          )

        ;; (BK) re-set originally used variables
        (setq this-is-a-spam-email (rest maybe-spam)
              maybe-spam (first maybe-spam))

	(if (and this-is-a-spam-email maybe-spam)
	    (progn
	      ;;(message "Found spam!")
	      ;;(ding 1) (sleep-for 2)

	      ;; temprarily set rmail-current-message in order to
	      ;; output and delete the spam msg if needed:
	      (setq save-current-msg rmail-current-message)
	      (setq rmail-current-message msg)
	      ;; check action item and rsf-definitions-alist
	      ;; and do it:
	      (cond
	       ((equal (cdr (assoc 'action
				   (nth num-element rsf-definitions-alist)))
		       'output-and-delete)
		(progn
		  (rmail-output-to-rmail-file rsf-file 1 t)
                  ;; Don't delete if automatic deletion after output
                  ;; is turned on
		  (unless rmail-delete-after-output (rmail-delete-message))
		  ))
	       ((equal (cdr (assoc 'action
				   (nth num-element rsf-definitions-alist)))
		       'delete-spam)
		(progn
		  (rmail-delete-message)
		  ))
	       )
	       (setq rmail-current-message save-current-msg)
	       (setq bbdb/mail_auto_create_p
	       'rsf-saved-bbdb/mail_auto_create_p)
	      ;; set return value.  These lines must be last in the
	      ;; function, so that they will determine the value
	      ;; returned by rmail-spam-filter:
	      (setq return-value nil))
	    (setq return-value t))))
    (setq case-fold-search saved-case-fold-search)
    (setq rsf-scanning-messages-now nil)
    return-value))


;; define functions for interactively adding sender/subject of a
;; specific message to the spam definitions while reading it, using
;; the menubar:
(defun rsf-add-subject-to-spam-list ()
  (interactive)
  (set-buffer rmail-buffer)
  (let ((message-subject))
    (setq message-subject (mail-fetch-field "Subject"))
    ;; note the use of a backquote and comma on the subject line here,
    ;; to make sure message-subject is actually evaluated and its value
    ;; substituted:
    (add-to-list 'rsf-definitions-alist
		 (list '(from . "")
		       '(to . "")
		       `(subject . ,message-subject)
		       '(content-type . "")
		       '(contents . "")
		       '(action . output-and-delete))
		 t)
    (customize-mark-to-save 'rsf-definitions-alist)
    (if rsf-autosave-newly-added-definitions
	(progn
	  (custom-save-all)
	  (message "%s" (concat "added subject \n <<< \n" message-subject
			   " \n >>> \n to list of spam definitions. \n"
			   "and saved the spam definitions to file.")))
      (message "%s" (concat "added subject \n <<< \n" message-subject
		       " \n >>> \n to list of spam definitions. \n"
		       "Don't forget to save the spam definitions to file using the spam
		       menu"))
      )))

(defun rsf-add-sender-to-spam-list ()
  (interactive)
  (set-buffer rmail-buffer)
  (let ((message-sender))
    (setq message-sender (mail-fetch-field "From"))
    ;; note the use of a backquote and comma on the "from" line here,
    ;; to make sure message-sender is actually evaluated and its value
    ;; substituted:
    (add-to-list 'rsf-definitions-alist
		 (list `(from . ,message-sender)
		       '(to . "")
		       '(subject . "")
		       '(content-type . "")
		       '(contents . "")
		       '(action . output-and-delete))
		 t)
    (customize-mark-to-save 'rsf-definitions-alist)
    (if rsf-autosave-newly-added-definitions
	(progn
	  (custom-save-all)
	  (message "%s" (concat "added sender \n <<< \n" message-sender
			   " \n >>> \n to list of spam definitions. \n"
			   "and saved the spam definitions to file.")))
      (message "%s" (concat "added sender \n <<< \n " message-sender
		       " \n >>> \n to list of spam definitions."
		       "Don't forget to save the spam definitions to file using the spam
		       menu"))
      )))


(defun rsf-add-region-to-spam-list ()
  "Add the region makred by user in the rmail buffer to spam list.
Added to spam definitions as a contents field."
  (interactive)
  (set-buffer rmail-buffer)
  (let ((region-to-spam-list))
    ;; check if region is inactive or has zero size:
    (if (not (and mark-active (not (= (region-beginning) (region-end)))))
	;; if inactive, print error message:
	(message "you need to first highlight some text in the rmail buffer")
      (if (< (- (region-end) (region-beginning)) rsf-min-region-to-spam-list)
	  (message
	   (concat "highlighted region is too small; min length set by variable \n"
		   "rsf-min-region-to-spam-list"
		   " is " (number-to-string rsf-min-region-to-spam-list)))
	;; if region active and long enough, add to list of spam definisions:
	(progn
	  (setq region-to-spam-list (buffer-substring (region-beginning) (region-end)))
	  ;; note the use of a backquote and comma on the "from" line here,
	  ;; to make sure message-sender is actually evaluated and its value
	  ;; substituted:
	  (add-to-list 'rsf-definitions-alist
		       (list '(from . "")
			     '(to . "")
			     '(subject . "")
			     '(content-type . "")
			     `(contents . ,region-to-spam-list)
			     '(action . output-and-delete))
		       t)
	  (customize-mark-to-save 'rsf-definitions-alist)
	  (if rsf-autosave-newly-added-definitions
	      (progn
		(custom-save-all)
		(message "%s" (concat "added highlighted text \n <<< \n" region-to-spam-list
				 " \n >>> \n to list of spam definitions. \n"
				 "and saved the spam definitions to file.")))
	    (message "%s" (concat "added highlighted text \n <<< \n " region-to-spam-list
			     " \n >>> \n to list of spam definitions."
			     "Don't forget to save the spam definitions to file using the
			     spam menu"))
	    ))))))


(defun rsf-customize-spam-definitions ()
  (interactive)
  (customize-variable (quote rsf-definitions-alist)))

(defun rsf-customize-group ()
  (interactive)
  (customize-group (quote rmail-spam-filter)))

(defun rsf-custom-save-all ()
  (interactive)
  (custom-save-all))

;; add the actual menu items and keyboard shortcuts to both rmail and
;; rmail-summary menu-bars::
(define-key rmail-summary-mode-map [menu-bar spam]
  (cons "Spam" (make-sparse-keymap "Spam")))
(define-key rmail-mode-map [menu-bar spam]
  (cons "Spam" (make-sparse-keymap "Spam")))

(define-key rmail-summary-mode-map [menu-bar spam customize-group]
  '("Browse customizations of rmail spam filter" . rsf-customize-group))
(define-key rmail-mode-map [menu-bar spam customize-group]
  '("Browse customizations of rmail spam filter" . rsf-customize-group))
(define-key rmail-summary-mode-map "\C-cSg" 'rsf-customize-group)
(define-key rmail-mode-map "\C-cSg" 'rsf-customize-group)

(define-key rmail-summary-mode-map [menu-bar spam customize-spam-list]
  '("Customize list of spam definitions" . rsf-customize-spam-definitions))
(define-key rmail-mode-map [menu-bar spam customize-spam-list]
  '("Customize list of spam definitions" . rsf-customize-spam-definitions))
(define-key rmail-summary-mode-map "\C-cSd" 'rsf-customize-spam-definitions)
(define-key rmail-mode-map "\C-cSd" 'rsf-customize-spam-definitions)

(define-key rmail-summary-mode-map [menu-bar spam lambda] '("----"))
(define-key rmail-mode-map [menu-bar spam lambda] '("----"))

(define-key rmail-summary-mode-map [menu-bar spam my-custom-save-all]
  '("save newly added spam definitions to customization file" . rsf-custom-save-all))
(define-key rmail-mode-map [menu-bar spam my-custom-save-all]
  '("save newly added spam definitions to customization file" . rsf-custom-save-all))
(define-key rmail-summary-mode-map "\C-cSa" 'rsf-custom-save-all)
(define-key rmail-mode-map "\C-cSa" 'rsf-custom-save-all)

(define-key rmail-summary-mode-map [menu-bar spam add-region-to-spam-list]
  '("add region to spam list" . rsf-add-region-to-spam-list))
(define-key rmail-mode-map [menu-bar spam add-region-to-spam-list]
  '("add region to spam list" . rsf-add-region-to-spam-list))
(define-key rmail-summary-mode-map "\C-cSn" 'rsf-add-region-to-spam-list)
(define-key rmail-mode-map "\C-cSn" 'rsf-add-region-to-spam-list)

(define-key rmail-summary-mode-map [menu-bar spam add-sender-to-spam-list]
  '("add sender to spam list" . rsf-add-sender-to-spam-list))
(define-key rmail-mode-map [menu-bar spam add-sender-to-spam-list]
  '("add sender to spam list" . rsf-add-sender-to-spam-list))
(define-key rmail-summary-mode-map "\C-cSr" 'rsf-add-sender-to-spam-list)
(define-key rmail-mode-map "\C-cSr" 'rsf-add-sender-to-spam-list)

(define-key rmail-summary-mode-map [menu-bar spam add-subject-to-spam-list]
  '("add subject to spam list" . rsf-add-subject-to-spam-list))
(define-key rmail-mode-map [menu-bar spam add-subject-to-spam-list]
  '("add subject to spam list" . rsf-add-subject-to-spam-list))
(define-key rmail-summary-mode-map "\C-cSt" 'rsf-add-subject-to-spam-list)
(define-key rmail-mode-map "\C-cSt" 'rsf-add-subject-to-spam-list)

(defun rsf-add-content-type-field ()
  "Maintain backward compatibility for `rmail-spam-filter'.
The most recent version of `rmail-spam-filter' checks the contents
field of the incoming mail to see if it spam.  The format of
`rsf-definitions-alist' has therefore changed.  This function
checks to see if old format is used, and if it is, it converts
`rsf-definitions-alist' to the new format.  Invoked
automatically, no user input is required."
  (interactive)
  (if (and rsf-definitions-alist
           (not (assoc 'content-type (car rsf-definitions-alist))))
      (let ((result nil)
            (current nil)
            (definitions rsf-definitions-alist))
        (while definitions
          (setq current (car definitions))
          (setq definitions (cdr definitions))
          (setq result
                (append result
                        (list
                         (list (assoc 'from current)
                               (assoc 'to current)
                               (assoc 'subject current)
                               (cons 'content-type "")
                               (assoc 'contents current)
                               (assoc 'action current))))))
        (setq rsf-definitions-alist result)
        (customize-mark-to-save 'rsf-definitions-alist)
        (if rsf-autosave-newly-added-definitions
            (progn
              (custom-save-all)
              (message (concat "converted spam definitions to new format\n"
                               "and saved the spam definitions to file.")))
          (message (concat "converted spam definitions to new format\n"
                           "Don't forget to save the spam definitions to file using the
                           spam menu"))
          ))))

(provide 'rmail-spam-filter)

;;; arch-tag: 03e1d45d-b72f-4dd7-8f04-e7fd78249746
;;; rmail-spam-fitler ends here
