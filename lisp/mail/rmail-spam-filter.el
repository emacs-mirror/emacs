;;; rmail-spam-filter.el  --- spam filter for rmail, the emacs mail reader.

;; Copyright (C) 2002 
;;		Free Software Foundation, Inc.
;; Keywords: email, spam, filter, rmail
;; Author: Eli Tziperman <eli@beach.weizmann.ac.il>

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

;;; (*) specify in variable rmail-spam-definitions-alist what sender,
;;; subject and contents make an email be considered spam.

;;; in addition, you may:

;;; (*) Block future mail with the subject or sender of a message
;;; while reading it in RMAIL: just click on the "Spam" item on the
;;; menubar, and add the subject or sender to the list of spam
;;; definitions using the mouse and the appropriate menu item. Â  You
;;; need to later also save the list of spam definitions using the
;;; same menu item, or alternatively, see variable
;;; `rmail-spam-filter-autosave-newly-added-spam-definitions'.

;;; (*) specify if blind-cc'ed mail (no "To:" header field) is to be
;;; treated as spam (variable rmail-spam-no-blind-cc; Thanks to Ethan
;;; Brown <ethan@gso.saic.com> for this).

;;; (*) specify if rmail-spam-filter should ignore case of spam
;;; definitions (variable rmail-spam-filter-ignore-case; Thanks to
;;; Ethan Brown <ethan@gso.saic.com> for the suggestion).

;;; (*) Specify a "white-list" of trusted senders. If any
;;; rmail-spam-white-list string matches a substring of the "From"
;;; header, the message is flagged as a valid, non-spam message (Ethan
;;; Brown <ethan@gso.saic.com>).

;;; (*) rmail spam filter also works with bbdb to prevent spam senders
;;; from entering into the .bbdb file.  See variable
;;; "rmail-spam-filter-auto-delete-spam-bbdb-entries".  This is done
;;; in two ways: (a) bbdb is made not to auto-create entries for
;;; messages that are deleted by the rmail-spam-filter, (b) when a
;;; message is deleted in rmail, the user is offered to delete the
;;; sender's bbdb entry as well _if_ it was created at the same day.

(require 'rmail)

;; For find-if and other cool common lisp functions we may want to use. (EDB)
(require 'cl)				

(defgroup rmail-spam-filter nil
  "Spam filter for RMAIL, the mail reader for Emacs."
  :group 'rmail)

(defcustom rmail-use-spam-filter nil
  "*Non-nil to activate the rmail spam filter.
Specify `rmail-spam-definitions-alist' to define what you consider spam
emails."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-file "~/XRMAIL-SPAM"
  "*Name of rmail file for optionally saving some of the spam.
Spam may be either just deleted, or saved in a separate spam file to
be looked at at a later time.  Whether the spam is just deleted or
also saved in a separete spam file is specified for each definition of
spam, as one of the fields of `rmail-spam-definitions-alist'"
  :type 'string
  :group 'rmail-spam-filter )

(defcustom rmail-spam-no-blind-cc nil
  "*Non-nil to treat blind CC (no To: header) as spam."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-filter-ignore-case nil
  "*Non-nil to ignore case in `rmail-spam-definitions-alist'."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-filter-beep nil
  "*Non-nil to beep if spam is found."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-sleep-after-message 2.0
  "*Seconds to wait after display of message that spam was found."
  :type 'number
  :group 'rmail-spam-filter )
  
(defcustom rmail-spam-filter-auto-delete-spam-bbdb-entries nil
  "*Non-nil to make sure no entries are made in bbdb for spam emails.
This is done in two ways: (1) bbdb is made not to auto-create entries
for messages that are deleted by the `rmail-spam-filter', (2) when a
message is deleted in rmail, the user is offered to delete the
sender's bbdb entry as well if it was created at the same day.  Note
that Emacs needs to be restarted after setting this option for it to
take an effect."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-filter-autosave-newly-added-spam-definitions nil
  "*Non-nil to auto save new spam entries.
New entries entered via the spam menu bar item are then saved to
customization file immediately after being added via the menu bar, and
do not require explicitly saving the file after adding the new
entries."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-white-list nil
  "*List of strings to identify valid senders.
If any rmail-spam-white-list string matches a substring of the 'From'
header, the message is flagged as a valid, non-spam message.  Example:
If your domain is emacs.com then including 'emacs.com' in your
rmail-spam-white-list would flag all mail from your colleagues as
valid."
  :type '(repeat string)
  :group 'rmail-spam-filter )

(defcustom rmail-spam-definitions-alist nil
  "*Alist matching strings defining what messages are considered spam.
Each definition may contain specifications of one or more of the
elements {subject, sender, recipients or contents}, as well as a
definition of what to do with the spam (action item).  A spam e-mail
is defined as one that fits all of the specified elements of any one
of the spam definitions.  The strings that specify spam subject,
sender, etc, may be regexp.  For example, to specify that the subject
may be either 'this is spam' or 'another spam', use the regexp: 'this
is spam\|another spam' (without the single quotes)."
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
	   (cons :format "%v" :value (contents . "")
		 (const :format ""  contents)
		 (string :tag "Contents"  ""))
	   (cons :format "%v" :value (action . output-and-delete)
		 (const :format "" action)
		 (choice :tag "Action selection" 
		  (const :tag "output to spam folder and delete" output-and-delete)
		  (const :tag "delete spam" delete-spam)
		  ))
   ))
  :group 'rmail-spam-filter)

(defvar rmail-spam-filter-scanning-messages-now nil
  "Non nil when rmail-spam-filter scans messages,
for interaction with `rmail-bbdb-auto-delete-spam-entries'")

(defun rmail-spam-filter (msg)
  "Return nil if msg is spam based on rmail-spam-definitions-alist.
If spam, optionally output msg to a file `rmail-spam-file' and delete
it from rmail file.  Called for each new message retrieved by
`rmail-get-new-mail'."

  (let ((old-message)
	(return-value)
	(this-is-a-spam-email)
	(maybe-spam)
	(message-sender)
	(message-recipients)
	(message-subject)
	(num-spam-definition-elements)
	(num-element 0)
	(exit-while-loop nil)
	(saved-case-fold-search case-fold-search)
	(save-current-msg)
	(rmail-spam-filter-saved-bbdb/mail_auto_create_p nil)
	)
    
    ;; make sure bbdb does not create entries for messages while spam
    ;; filter is scanning the rmail file:
    (setq rmail-spam-filter-saved-bbdb/mail_auto_create_p 'bbdb/mail_auto_create_p)
    (setq bbdb/mail_auto_create_p nil)
    ;; let `rmail-bbdb-auto-delete-spam-entries' know that rmail spam
    ;; filter is running, so that deletion of rmail messages should be
    ;; ignored for now:
    (setq rmail-spam-filter-scanning-messages-now t)
    (save-excursion
      (save-restriction
	(setq this-is-a-spam-email nil)
	;; Narrow buffer to header of message and get Sender and
	;; Subject fields to be used below:
	(save-restriction
	  (goto-char (rmail-msgbeg msg))
	  (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
	  (setq message-sender (mail-fetch-field "From"))
	  (setq message-recipients (mail-fetch-field "To"))
	  (setq message-subject (mail-fetch-field "Subject"))
	  )
	;; Find number of spam-definition elements in the list
	;; rmail-spam-definitions-alist specified by user:
	(setq num-spam-definition-elements (safe-length
					    rmail-spam-definitions-alist))

	;;; do we want to ignore case in spam definitions:
	  (setq case-fold-search rmail-spam-filter-ignore-case)
	
	;; Check for blind CC condition.  Set vars such that while
	;; loop will be bypassed and spam condition will trigger (EDB)
	(if (and rmail-spam-no-blind-cc
		 (null message-recipients))
	    (progn
	      (setq exit-while-loop t)
	      (setq maybe-spam t)
	      (setq this-is-a-spam-email t)))
	
	  ;; Check white list, and likewise cause while loop
	  ;;  bypass. (EDB)
	  (if (find-if '(lambda (white-str)
			  (string-match white-str message-sender))
		       rmail-spam-white-list)
	      (progn
		(setq exit-while-loop t)
		(setq maybe-spam nil)
		(setq this-is-a-spam-email nil)))
	    
	;; scan all elements of the list rmail-spam-definitions-alist
	(while (and
		(< num-element num-spam-definition-elements)
		(not exit-while-loop))
	  (progn
	    ;; Initialize maybe-spam which is set to t in one of two
	    ;; cases: (1) unspecified definition-elements are found in
	    ;; rmail-spam-definitions-alist, (2) empty field is found
	    ;; in the message being scanned (e.g. empty subject,
	    ;; sender, recipients, etc).  The variable is set to nil
	    ;; if a non empty field of the scanned message does not
	    ;; match a specified field in
	    ;; rmail-spam-definitions-alist.
	    (setq maybe-spam t)
	    ;; initialize this-is-a-spam-email to nil.  This variable
	    ;; is set to t if one of the spam definitions matches a
	    ;; field in the scanned message.
	    (setq this-is-a-spam-email nil)

	    ;; start scanning incoming message:
	    ;;---------------------------------
	    
	    ;; if sender field is not specified in message being
	    ;; scanned, AND if "from" field does not appear in spam
	    ;; definitions for this element, this may still be spam
	    ;; due to another element...
	    (if (and (not message-sender)
		     (string-match
		      (cdr (assoc 'from (nth num-element
					     rmail-spam-definitions-alist))) ""))
		(setq maybe-spam t)
	      ;; ... else, if message-sender does appear in the
	      ;; message, and it also appears in the spam definition
	      ;; list, it is potentially spam:
	      (if (and message-sender
		       (string-match
			(cdr (assoc 'from (nth num-element
					       rmail-spam-definitions-alist)))
			message-sender)
		       )
		  (setq this-is-a-spam-email t)
		(setq maybe-spam nil)
		)
	      )
	    ;; next, if spam was not ruled out already, check recipients:
	    (if maybe-spam
		;; if To field does not exist AND is not specified,
		;; this may still be spam due to another element...
		(if (and (not message-recipients)
			 (string-match
			  (cdr (assoc 'to
				      (nth num-element
					   rmail-spam-definitions-alist))) ""))
		    (setq maybe-spam t)
		  ;; ... else, if To field does appear in the message,
		  ;; and it also appears in spam definition list, this
		  ;; is potentially a spam:
		  (if (and message-recipients
			   (string-match
			    (cdr (assoc 'to (nth num-element
						 rmail-spam-definitions-alist)))
			    message-recipients)
			   )
		      (setq this-is-a-spam-email t)
		    (setq maybe-spam nil)
		    )
		  )
	      )
	    ;; next, if spam was not ruled out already, check subject:
	    (if maybe-spam
		;; if subject field does not exist AND is not
		;; specified, this may still be spam due to another
		;; element...
		(if (and (not message-subject)
			(string-match
			 (cdr (assoc 'subject
				     (nth num-element
					  rmail-spam-definitions-alist)))
			 ""))
		    (setq maybe-spam t)
		  ;; ... else, if subject field does appear in the
		  ;; message, and it also appears in the spam
		  ;; definition list, this is potentially a spam:
		  (if (and message-subject
			   (string-match
			    (cdr (assoc 'subject (nth num-element
						      rmail-spam-definitions-alist)))
			    message-subject)
			   )
		      (setq this-is-a-spam-email t)
		    (setq maybe-spam nil)
		    )
		  )
	      )
	    ;; next, if spam was not ruled out already, check
	    ;; contents: if contents field is not specified, this may
	    ;; still be spam due to another element...
	    (if maybe-spam
		(if (string-match
		     (cdr (assoc 'contents
				 (nth num-element
				      rmail-spam-definitions-alist))) "")
		    (setq maybe-spam t)
		  ;; ... else, check to see if it appears in spam
		  ;; definition:
		  (if (string-match
		       (cdr (assoc 'contents
				   (nth num-element
					rmail-spam-definitions-alist)))
		       (buffer-substring
			(rmail-msgbeg msg) (rmail-msgend msg)))
		      (setq this-is-a-spam-email t)
		    (setq maybe-spam nil)))
	      )
	    ;; if the search in rmail-spam-definitions-alist found
	    ;; that this email is spam, output the email to the spam
	    ;; rmail file, mark the email for deletion, leave the
	    ;; while loop and return nil so that an rmail summary line
	    ;; wont be displayed for this message:
	    (if (and this-is-a-spam-email maybe-spam)
		;; found that this is spam, no need to look at the
		;; rest of the rmail-spam-definitions-alist, exit
		;; loop:
		(setq exit-while-loop t)
	      ;; else, spam was not yet found, increment number of
	      ;; element in rmail-spam-definitions-alist and proceed
	      ;; to next element:
	      (setq num-element (+ num-element 1)))
	    )
	  )
	(if (and this-is-a-spam-email maybe-spam)
	    (progn
	      ;;(message "Found spam!")
	      ;;(ding 1) (sleep-for 2)

	      ;; temprarily set rmail-current-message in order to
	      ;; output and delete the spam msg if needed:
	      (setq save-current-msg rmail-current-message)
	      (setq rmail-current-message msg)
	      ;; check action item and rmail-spam-definitions-alist
	      ;; and do it:
	      (cond
	       ((equal (cdr (assoc 'action
				   (nth num-element rmail-spam-definitions-alist)))
		       'output-and-delete)
		(progn
		  (rmail-output-to-rmail-file rmail-spam-file)
		  (rmail-delete-message)
		  ))
	       ((equal (cdr (assoc 'action
				   (nth num-element rmail-spam-definitions-alist)))
		       'delete-spam)
		(progn
		  (rmail-delete-message)
		  ))
	       )
	       (setq rmail-current-message save-current-msg)
	       (setq bbdb/mail_auto_create_p 'rmail-spam-filter-saved-bbdb/mail_auto_create_p)
	      ;; set return value.  These lines must be last in the
	      ;; function, so that they will determine the value
	      ;; returned by rmail-spam-filter:
	      (setq return-value nil))
	    (setq return-value t))))
    (setq case-fold-search saved-case-fold-search)
    (setq rmail-spam-filter-scanning-messages-now nil)
    return-value))


;; define functions for interactively adding sender/subject of a
;; specific message to the spam definitions while reading it, using
;; the menubar:
(defun rmail-spam-filter-add-subject-to-spam-list ()
  (interactive)
  (set-buffer rmail-buffer)
  (let ((message-subject))
    (setq message-subject (mail-fetch-field "Subject"))
    ;; note the use of a backquote and comma on the subject line here,
    ;; to make sure message-subject is actually evaluated and its value
    ;; substituted:
    (add-to-list 'rmail-spam-definitions-alist
		 (list '(from . "")
		       '(to . "")
		       `(subject . ,message-subject)
		       '(contents . "")
		       '(action . output-and-delete))
		 t)
    (customize-mark-to-save 'rmail-spam-definitions-alist)
    (if rmail-spam-filter-autosave-newly-added-spam-definitions
	(progn
	  (custom-save-all)
	  (message (concat "added subject \n <<< \n" message-subject
			   " \n >>> \n to list of spam definitions. \n"
			   "and saved the spam definitions to file.")))
      (message (concat "added subject \n <<< \n" message-subject
		       " \n >>> \n to list of spam definitions. \n"
		       "Don't forget to save the spam definitions to file using the spam menu"))
      )))

(defun rmail-spam-filter-add-sender-to-spam-list ()
  (interactive)
  (set-buffer rmail-buffer)
  (let ((message-sender))
    (setq message-sender (mail-fetch-field "From"))
    ;; note the use of a backquote and comma on the "from" line here,
    ;; to make sure message-sender is actually evaluated and its value
    ;; substituted:
    (add-to-list 'rmail-spam-definitions-alist
		 (list `(from . ,message-sender)
		       '(to . "")
		       '(subject . "")
		       '(contents . "")
		       '(action . output-and-delete))
		 t)
    (customize-mark-to-save 'rmail-spam-definitions-alist)
    (if rmail-spam-filter-autosave-newly-added-spam-definitions
	(progn
	  (custom-save-all)
	  (message (concat "added sender \n <<< \n" message-sender
			   " \n >>> \n to list of spam definitions. \n"
			   "and saved the spam definitions to file.")))
      (message (concat "added sender \n <<< \n " message-sender
		       " \n >>> \n to list of spam definitions."
		       "Don't forget to save the spam definitions to file using the spam menu"))
      )))


(defun rmail-spam-filter-add-region-to-spam-list ()
  "Add the region makred by user in the rmail buffer to the list of
  spam definitions as a contents field."
  (interactive)
  (set-buffer rmail-buffer)
  (let ((region-to-spam-list))
    ;; check if region is inactive or has zero size:
    (if (not (and mark-active (not (= (region-beginning) (region-end)))))
	;; if inactive, print error message:
	(message "you need to first highlight some text in the rmail buffer")
      ;; if active, add to list of spam definisions:
      (progn
	(setq region-to-spam-list (buffer-substring (region-beginning) (region-end)))
	;; note the use of a backquote and comma on the "from" line here,
	;; to make sure message-sender is actually evaluated and its value
	;; substituted:
	(add-to-list 'rmail-spam-definitions-alist
		     (list '(from . "")
			   '(to . "")
			   '(subject . "")
			   `(contents . ,region-to-spam-list)
			   '(action . output-and-delete))
		     t)
	(customize-mark-to-save 'rmail-spam-definitions-alist)
	(if rmail-spam-filter-autosave-newly-added-spam-definitions
	    (progn
	      (custom-save-all)
	      (message (concat "added highlighted text \n <<< \n" region-to-spam-list
			       " \n >>> \n to list of spam definitions. \n"
			       "and saved the spam definitions to file.")))
	  (message (concat "added highlighted text \n <<< \n " region-to-spam-list
			   " \n >>> \n to list of spam definitions."
			   "Don't forget to save the spam definitions to file using the spam menu"))
	  )))))


(defun rmail-spam-filter-customize-spam-definitions ()
  (interactive)
  (customize-variable (quote rmail-spam-definitions-alist)))

(defun rmail-spam-filter-customize-group ()
  (interactive)
  (customize-group (quote rmail-spam-filter)))

(defun rmail-spam-custom-save-all ()
  (interactive)
  (custom-save-all))

;; add the actual menu items and keyboard shortcuts to both rmail and
;; rmail-summary menu-bars::
(define-key rmail-summary-mode-map [menu-bar spam]
  (cons "Spam" (make-sparse-keymap "Spam")))
(define-key rmail-mode-map [menu-bar spam]
  (cons "Spam" (make-sparse-keymap "Spam")))

(define-key rmail-summary-mode-map [menu-bar spam customize-group]
  '("Browse customizations of rmail spam filter" . rmail-spam-filter-customize-group))
(define-key rmail-mode-map [menu-bar spam customize-group]
  '("Browse customizations of rmail spam filter" . rmail-spam-filter-customize-group))
(define-key rmail-summary-mode-map "\C-cSg" 'rmail-spam-filter-customize-group)
(define-key rmail-mode-map "\C-cSg" 'rmail-spam-filter-customize-group)

(define-key rmail-summary-mode-map [menu-bar spam customize-spam-list]
  '("Customize list of spam definitions" . rmail-spam-filter-customize-spam-definitions))
(define-key rmail-mode-map [menu-bar spam customize-spam-list]
  '("Customize list of spam definitions" . rmail-spam-filter-customize-spam-definitions))
(define-key rmail-summary-mode-map "\C-cSd" 'rmail-spam-filter-customize-spam-definitions)
(define-key rmail-mode-map "\C-cSd" 'rmail-spam-filter-customize-spam-definitions)

(define-key rmail-summary-mode-map [menu-bar spam lambda] '("----"))
(define-key rmail-mode-map [menu-bar spam lambda] '("----"))

(define-key rmail-summary-mode-map [menu-bar spam my-custom-save-all]
  '("save newly added spam definitions to customization file" . rmail-spam-custom-save-all))
(define-key rmail-mode-map [menu-bar spam my-custom-save-all]
  '("save newly added spam definitions to customization file" . rmail-spam-custom-save-all))
(define-key rmail-summary-mode-map "\C-cSa" 'rmail-spam-custom-save-all)
(define-key rmail-mode-map "\C-cSa" 'rmail-spam-custom-save-all)

(define-key rmail-summary-mode-map [menu-bar spam add-region-to-spam-list]
  '("add region to spam list" . rmail-spam-filter-add-region-to-spam-list))
(define-key rmail-mode-map [menu-bar spam add-region-to-spam-list]
  '("add region to spam list" . rmail-spam-filter-add-region-to-spam-list))
(define-key rmail-summary-mode-map "\C-cSn" 'rmail-spam-filter-add-region-to-spam-list)
(define-key rmail-mode-map "\C-cSn" 'rmail-spam-filter-add-region-to-spam-list)

(define-key rmail-summary-mode-map [menu-bar spam add-sender-to-spam-list]
  '("add sender to spam list" . rmail-spam-filter-add-sender-to-spam-list))
(define-key rmail-mode-map [menu-bar spam add-sender-to-spam-list]
  '("add sender to spam list" . rmail-spam-filter-add-sender-to-spam-list))
(define-key rmail-summary-mode-map "\C-cSr" 'rmail-spam-filter-add-sender-to-spam-list)
(define-key rmail-mode-map "\C-cSr" 'rmail-spam-filter-add-sender-to-spam-list)

(define-key rmail-summary-mode-map [menu-bar spam add-subject-to-spam-list]
  '("add subject to spam list" . rmail-spam-filter-add-subject-to-spam-list))
(define-key rmail-mode-map [menu-bar spam add-subject-to-spam-list]
  '("add subject to spam list" . rmail-spam-filter-add-subject-to-spam-list))
(define-key rmail-summary-mode-map "\C-cSt" 'rmail-spam-filter-add-subject-to-spam-list)
(define-key rmail-mode-map "\C-cSt" 'rmail-spam-filter-add-subject-to-spam-list)


(defun rmail-bbdb-auto-delete-spam-entries ()
  "When deleting a message in RMAIL, check to see if the bbdb entry
was created today, and if it was, prompt to delete it too.  This function 
needs to be called via the `rmail-delete-message-hook' like this:
\(add-hook 'rmail-delete-message-hook 'rmail-bbdb-auto-delete-spam-entries)"
  (interactive)
  (require 'bbdb-hooks)
  (if (not rmail-spam-filter-scanning-messages-now)
      (if (get-buffer "*BBDB*")
	  (save-excursion
	    (set-buffer (get-buffer "*BBDB*"))
	    (if (bbdb-current-record)
		(if (equal
		     (format-time-string bbdb-time-internal-format (current-time))
		     (bbdb-record-getprop (bbdb-current-record) 'creation-date))
		    (bbdb-delete-current-record (bbdb-current-record))))))))

(defun rmail-spam-filter-bbdb-dont-create-entries-for-spam ()
  "Make sure senderes of rmail messages marked as deleted are not added to bbdb.
Need to add this as a hook like this:
\(setq bbdb/mail-auto-create-p 'rmail-spam-filter-bbdb-dont-create-entries-for-spam)
and this is also used in conjunction with rmail-bbdb-auto-delete-spam-entries. 
More doc: rmail-bbdb-auto-delete-spam-entries will delete newly created bbdb 
entries of mail that is deleted.  However, if one scrolls back to the deleted 
messages, then the sender is again added to the bbdb.  This function 
prevents this.  Also, don't create entries for messages in the `rmail-spam-file'."
  (interactive)
  (not
   ;; don't create a bbdb entry if one of the following conditions is satisfied: 
   (or
    ;; 1) looking at a deleted message:
    (rmail-message-deleted-p rmail-current-message)
    ;; 2) looking at messages in rmail-spam-file:
    (string-match
     (expand-file-name rmail-spam-file)
     (expand-file-name (buffer-file-name rmail-buffer)))
    )))

;; activate bbdb-anti-spam measures:
(if rmail-spam-filter-auto-delete-spam-bbdb-entries
    (progn
      (add-hook 'rmail-delete-message-hook 'rmail-bbdb-auto-delete-spam-entries)
      (setq bbdb/mail-auto-create-p 'rmail-spam-filter-bbdb-dont-create-entries-for-spam)
      ))

(provide 'rmail-spam-filter)

;;; arch-tag: 03e1d45d-b72f-4dd7-8f04-e7fd78249746
;;; rmail-spam-filter ends here
