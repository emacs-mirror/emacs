;;; rmail.el --- main code of "RMAIL" mail reader for Emacs.

;; Copyright (C) 1985, 1986, 1987, 1988, 1993 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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

;; For Emacs V18 compatibility
(and (not (fboundp 'user-original-login-name))
     (fboundp 'user-real-login-name)
     (defalias 'user-original-login-name 'user-real-login-name))
(and (not (fboundp 'buffer-disable-undo))
     (fboundp 'buffer-flush-undo)
     (defalias 'buffer-disable-undo 'buffer-flush-undo))

; These variables now declared in paths.el.
;(defvar rmail-spool-directory "/usr/spool/mail/"
;  "This is the name of the directory used by the system mailer for\n\
;delivering new mail.  It's name should end with a slash.")
;(defvar rmail-file-name
;  (expand-file-name "~/RMAIL")
;  "")

;;;###autoload
(defvar rmail-dont-reply-to-names nil "\
*A regexp specifying names to prune of reply to messages.
A value of nil means exclude your own name only.")

;;;###autoload
(defvar rmail-default-dont-reply-to-names "info-" "\
A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customization file.")

;;;###autoload
(defvar rmail-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^message-id:\\|^summary-line:" "\
*Gubbish headers one would rather not see.")

;;;###autoload
(defvar rmail-delete-after-output nil "\
*Non-nil means automatically delete a message that is copied to a file.")

;;;###autoload
(defvar rmail-primary-inbox-list nil "\
*List of files which are inboxes for user's primary mail file `~/RMAIL'.
`nil' means the default, which is (\"/usr/spool/mail/$USER\")
\(the name varies depending on the operating system,
and the value of the environment variable MAIL overrides it).")

;;;###autoload
(defvar rmail-mail-new-frame nil
  "*Non-nil means Rmail makes a new frame for composing outgoing mail.")

;;;###autoload
(defvar rmail-retry-setup-hook nil
  "Hook that `rmail-retry-failure' uses in place of `mail-setup-hook'.")

;; These may be altered by site-init.el to match the format of mmdf files
;;  delimiting used on a given host (delim1 and delim2 from the config
;;  files).

(defvar mmdf-delim1 "^\001\001\001\001\n"
  "Regexp marking the start of an mmdf message")
(defvar mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message")

(defvar rmail-message-filter nil
  "If non nil, is a filter function for new headers in RMAIL.
Called with region narrowed to unformatted header.")

(defvar rmail-reply-prefix "Re: "
  "String to prepend to Subject line when replying to a message.")

(defvar rmail-mode-map nil)

(defvar rmail-inbox-list nil)
(defvar rmail-keywords nil)

;; Message counters and markers.  Deleted flags.

(defvar rmail-current-message nil)
(defvar rmail-total-messages nil)
(defvar rmail-message-vector nil)
(defvar rmail-deleted-vector nil)

;; These are used by autoloaded rmail-summary.

(defvar rmail-summary-buffer nil)
(defvar rmail-summary-vector nil)

;; `Sticky' default variables.

;; Last individual label specified to a or k.
(defvar rmail-last-label nil)
;; Last set of values specified to C-M-n, C-M-p, C-M-s or C-M-l.
(defvar rmail-last-multi-labels nil)
(defvar rmail-last-file nil)
(defvar rmail-last-regexp nil)
(defvar rmail-last-rmail-file (expand-file-name "~/XMAIL"))

;;; Regexp matching the delimiter of messages in UNIX mail format
;;; (UNIX From lines), minus the initial ^.  Note that if you change
;;; this expression, you must change the code in rmail-nuke-pinhead-header
;;; that knows the exact ordering of the \\( \\) subexpressions.
(defvar rmail-unix-mail-delimiter
  (let ((time-zone-regexp
	 (concat "\\([A-Z]?[A-Z][A-Z][A-Z]\\( DST\\)?"
		 "\\|[-+]?[0-9][0-9][0-9][0-9]"
		 "\\|"
		 "\\) *")))
    (concat
     "From "

     ;; Username, perhaps with a quoted section that can contain spaces.
     "\\("
     "[^ \n]*"
     "\\(\\|\".*\"[^ \n]*\\)"
     "\\)  ?"

     ;; The time the message was sent.
     "\\([^ \n]*\\) *"			; day of the week
     "\\([^ ]*\\) *"			; month
     "\\([0-9]*\\) *"			; day of month
     "\\([0-9:]*\\) *"			; time of day

     ;; Perhaps a time zone, specified by an abbreviation, or by a
     ;; numeric offset.
     time-zone-regexp

     ;; The year.
     " [0-9][0-9]\\([0-9]*\\) *"

     ;; On some systems the time zone can appear after the year, too.
     time-zone-regexp

     ;; I'm not sure what this is.
     "\\(remote from [^\n]*\\)?"

     "\n"))
  nil)

;; Perform BODY in the summary buffer
;; in such a way that its cursor is properly updated in its own window.
(defmacro rmail-select-summary (&rest body)
  (` (progn (if (rmail-summary-displayed)
		(let ((window (selected-window)))
		  (save-excursion
		    (unwind-protect
			(progn
			  (pop-to-buffer rmail-summary-buffer)
			  (,@ body))
		      (select-window window))))
	      (save-excursion
		(set-buffer rmail-summary-buffer)
		(progn (,@ body))))
	    (rmail-maybe-display-summary))))

;;;; *** Rmail Mode ***

;;;###autoload
(defun rmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by `rmail-file-name' (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with filename as argument; then performs rmail editing on
that file, but does not copy any new mail into the file."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail on RMAIL file: "
					 nil nil t))))
  (or rmail-last-file
      (setq rmail-last-file (expand-file-name "~/xmail")))
  (let* ((file-name (expand-file-name (or file-name-arg rmail-file-name)))
	 (existed (get-file-buffer file-name)))
    ;; Like find-file, but in the case where a buffer existed
    ;; and the file was reverted, recompute the message-data.
    (if (and existed (not (verify-visited-file-modtime existed)))
	(progn
	  ;; Don't be confused by apparent local-variables spec
	  ;; in the last message in the RMAIL file.
	  (let ((enable-local-variables nil))
	    (find-file file-name))
	  (if (and (verify-visited-file-modtime existed)
		   (eq major-mode 'rmail-mode))
	      (progn (rmail-forget-messages)
		     (rmail-set-message-counters))))
      (let ((enable-local-variables nil))
	(find-file file-name)))
    (if (eq major-mode 'rmail-edit-mode)
	(error "Exit Rmail Edit mode before getting new mail."))
    (if (and existed (> (buffer-size) 0))
	;; Buffer not new and not empty; ensure in proper mode, but that's all.
	(or (eq major-mode 'rmail-mode)
	    (rmail-mode-2))
      (rmail-mode-2)
      ;; Convert all or part to Babyl file if possible.
      (rmail-convert-file)
      (goto-char (point-max))
      (if (null rmail-inbox-list)
	  (progn
	    (rmail-set-message-counters)
	    (rmail-show-message))))
    (let ((existing-unseen (rmail-first-unseen-message)))
      (or file-name-arg
	  (rmail-get-new-mail))
      ;; Show the first unseen message, which might be from a previous session
      ;; or might have been just read in by rmail-get-new-mail.  Must
      ;; determine already unseen messages first, as rmail-get-new-mail
      ;; positions on the first new message, thus marking it as seen.
      (rmail-show-message existing-unseen))))

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
	      (search-backward "\^_")
	      (forward-char 1)
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
	  (message "Converting to Babyl format...done")))))

;;; I have checked that adding "-*- rmail -*-" to the BABYL OPTIONS line
;;; will not cause emacs 18.55 problems.

(defun rmail-insert-rmail-file-header ()
  (let ((buffer-read-only nil))
    (insert "BABYL OPTIONS: -*- rmail -*-
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.\n\^_")))

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
  (define-key rmail-mode-map "w"      'rmail-edit-current-message)
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
  )

(define-key rmail-mode-map [menu-bar] (make-sparse-keymap))

(define-key rmail-mode-map [menu-bar classify]
  (cons "Classify" (make-sparse-keymap "Classify")))

(define-key rmail-mode-map [menu-bar classify output-inbox]
  '("Output (inbox)" . rmail-output))

(define-key rmail-mode-map [menu-bar classify output]
  '("Output (Rmail)" . rmail-output-to-rmail-file))

(define-key rmail-mode-map [menu-bar classify kill-label]
  '("Kill Label" . rmail-kill-label))

(define-key rmail-mode-map [menu-bar classify add-label]
  '("Add Label" . rmail-add-label))

(define-key rmail-mode-map [menu-bar summary]
  (cons "Summary" (make-sparse-keymap "Summary")))

(define-key rmail-mode-map [menu-bar summary labels]
  '("By Labels" . rmail-summary-by-labels))

(define-key rmail-mode-map [menu-bar summary recipients]
  '("By Recipients" . rmail-summary-by-recipients))

(define-key rmail-mode-map [menu-bar summary topic]
  '("By Topic" . rmail-summary-by-topic))

(define-key rmail-mode-map [menu-bar summary regexp]
  '("By Regexp" . rmail-summary-by-regexp))

(define-key rmail-mode-map [menu-bar summary all]
  '("All" . rmail-summary))

(define-key rmail-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))

(define-key rmail-mode-map [menu-bar mail continue]
  '("Continue" . rmail-continue))

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
  '("Search Back" . rmail-search-backward))

(define-key rmail-mode-map [menu-bar move search]
  '("Search" . rmail-search))

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
  (rmail-mode-2)
  (rmail-set-message-counters)
  (rmail-show-message rmail-total-messages))

(defun rmail-mode-2 ()
  (kill-all-local-variables)
  (rmail-mode-1)
  (rmail-variables)
  (run-hooks 'rmail-mode-hook))

(defun rmail-mode-1 ()
  (setq major-mode 'rmail-mode)
  (setq mode-name "RMAIL")
  (setq buffer-read-only t)
  ;; No need to auto save RMAIL files.
  (setq buffer-auto-save-file-name nil)
  (if (boundp 'mode-line-modified)
      (setq mode-line-modified "--- ")
    (setq mode-line-format
	  (cons "--- " (cdr (default-value 'mode-line-format)))))
  (use-local-map rmail-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table))

(defun rmail-variables ()
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'rmail-revert)
  (make-local-variable 'rmail-last-label)
  (make-local-variable 'rmail-last-regexp)
  (make-local-variable 'rmail-deleted-vector)
  (make-local-variable 'rmail-summary-buffer)
  (make-local-variable 'rmail-summary-vector)
  (make-local-variable 'rmail-current-message)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag t)
  (make-local-variable 'rmail-message-vector)
  (make-local-variable 'rmail-last-file)
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
				   (user-original-login-name)))))))
  (make-local-variable 'rmail-keywords)
  ;; this gets generated as needed
  (setq rmail-keywords nil))

;; Handle M-x revert-buffer done in an rmail-mode buffer.
(defun rmail-revert (arg noconfirm)
  (let (revert-buffer-function)
    ;; Call our caller again, but this time it does the default thing.
    (if (revert-buffer arg noconfirm)
	;; If the user said "yes", and we changed something,
	;; reparse the messages.
	(progn
	  (rmail-convert-file)
	  (goto-char (point-max))
	  (rmail-set-message-counters)
	  (rmail-show-message)))))

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
  (save-buffer))

(defun rmail-quit ()
  "Quit out of RMAIL."
  (interactive)
  (rmail-expunge-and-save)
  ;; Don't switch to the summary buffer even if it was recently visible.
  (if rmail-summary-buffer
      (bury-buffer rmail-summary-buffer))
  (let ((obuf (current-buffer)))
    (switch-to-buffer (other-buffer))
    (bury-buffer obuf)))

;;;###autoload
(defun rmail-input (filename)
  "Run Rmail on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  (rmail filename))


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
argument causes us to read a file name and use that file as the inbox."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (or (verify-visited-file-modtime (current-buffer))
      (progn
	(find-file (buffer-file-name))
	(setq buffer-read-only t)
	(if (verify-visited-file-modtime (current-buffer))
	    (rmail-forget-messages))))
  (rmail-maybe-set-message-counters)
  (widen)
  ;; Get rid of all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (unwind-protect
      (let ((opoint (point))
	    (new-messages 0)
	    (delete-files ())
	    ;; If buffer has not changed yet, and has not been saved yet,
	    ;; don't replace the old backup file now.
	    (make-backup-files (and make-backup-files (buffer-modified-p)))
	    (buffer-read-only nil)
	    ;; Don't make undo records for what we do in getting mail.
	    (buffer-undo-list t))
	(goto-char (point-max))
	(skip-chars-backward " \t\n")	    ; just in case of brain damage
	(delete-region (point) (point-max)) ; caused by require-final-newline
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point) (point))
	    ;; Read in the contents of the inbox files,
	    ;; renaming them as necessary,
	    ;; and adding to the list of files to delete eventually.
	    (if file-name
		(rmail-insert-inbox-text (list file-name) nil)
	      (setq delete-files (rmail-insert-inbox-text rmail-inbox-list t)))
	    ;; Scan the new text and convert each message to babyl format.
	    (goto-char (point-min))
	    (save-excursion
	      (setq new-messages (rmail-convert-to-babyl-format)))
	    (or (zerop new-messages)
		(let (success)
		  (widen)
		  (search-backward "\n\^_")
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages)
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
	  (and (boundp 'display-time-string)
	       (string-match " Mail" display-time-string)
	       (setq display-time-string
		     (concat
		      (substring display-time-string 0 (match-beginning 0))
		      (substring display-time-string (match-end 0))))
	       (force-mode-line-update 'all))))
    ;; Don't leave the buffer screwed up if we get a disk-full error.
    (rmail-show-message)))

(defun rmail-insert-inbox-text (files renamep)
  (let (file tofile delete-files movemail popmail)
    (while files
      (setq file (expand-file-name (substitute-in-file-name (car files)))
	    ;;>> un*x specific <<
	    ;; The "+" used to be "~", which is an extremely poor choice;
	    ;; it might accidentally be deleted when space is low
	    ;; (as happened to me!).
	    tofile (concat file "+"))
      ;; If getting from mail spool directory,
      ;; use movemail to move rather than just renaming,
      ;; so as to interlock with the mailer.
      (setq movemail (equal (file-name-directory file) rmail-spool-directory)
	    popmail (string-match "^po:" (file-name-nondirectory file)))
      (if popmail (setq file (file-name-nondirectory file)
			renamep t))
      (if movemail
	  (progn
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
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (expand-file-name (user-original-login-name)
					     file)))))
      (if popmail
	  (message "Getting mail from post office ...")
	(if (or (and (file-exists-p tofile)
		     (/= 0 (nth 7 (file-attributes tofile))))
		(and (file-exists-p file)
		     (/= 0 (nth 7 (file-attributes file)))))
	    (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (and (not popmail)
					     (not (file-exists-p file))))
	     nil)
	    ((and (not movemail) (not popmail))
	     ;; Try copying.  If that fails (perhaps no space),
	     ;; rename instead.
	     (condition-case nil
		 (copy-file file tofile nil)
	       (error
		(rename-file file tofile nil)))
	     ;; Make the real inbox file empty.
	     ;; Leaving it deleted could cause lossage
	     ;; because mailers often won't create the file.
	     (condition-case ()
		 (write-region (point) (point) file)
	       (file-error nil)))
	    (t
	     (let ((errors nil))
	       (unwind-protect
		   (save-excursion
		     (setq errors (generate-new-buffer " *rmail loss*"))
		     (buffer-disable-undo errors)
		     (call-process
		       (expand-file-name "movemail" exec-directory)
		       nil errors nil file tofile)
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
		       (message (concat "movemail: "
					(buffer-substring (point-min)
							  (point-max))))
		       (sit-for 3)
		       nil))
		 (if errors (kill-buffer errors))))))
      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (let (size)
	    (goto-char (point-max))
	    (setq size (nth 1 (insert-file-contents tofile)))
	    (goto-char (point-max))
	    (or (= (preceding-char) ?\n)
		(zerop size)
		(insert ?\n))
	    (setq delete-files (cons tofile delete-files))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;; the  rmail-break-forwarded-messages  feature is not implemented
(defun rmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil)
	(invalid-input-resync
	 (function (lambda ()
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 1)
		     ;; Try to get back in sync with a real message.
		     (if (re-search-forward
			  (concat mmdf-delim1 "\\|^From") nil t)
			 (beginning-of-line)
		       (goto-char (point-max)))))))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
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
	       (narrow-to-region (point) (point-max)))
	      ;;*** MMDF format
	      ((let ((case-fold-search t))
		 (looking-at mmdf-delim1))
	       (let ((case-fold-search t))
		 (replace-match "\^L\n0, unseen,,\n*** EOOH ***\n")
		 (setq start (point))
		 (re-search-forward mmdf-delim2 nil t)
		 (replace-match "\^_"))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char "\^_"
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count)))
	      ;;*** Mail format
	      ((looking-at "^From ")
	       (setq start (point))
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       ;; If this message has a Content-Length field,
	       ;; skip to the end of the contents.
	       (let* ((header-end (save-excursion
				    (and (re-search-forward "\n\n" nil t)
					 (point))))
		      (case-fold-search t)
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
				(read (buffer-substring beg eol)))))))
		 (and size (numberp size) (>= size 0)
		      (goto-char (+ header-end size))))

	       (if (re-search-forward
		    (concat "^[\^_]?\\("
			    rmail-unix-mail-delimiter
			    "\\|"
			    mmdf-delim1 "\\|"
			    "^BABYL OPTIONS:\\|"
			    "\^L\n[01],\\)") nil t)
		   (goto-char (match-beginning 1))
		 (goto-char (point-max)))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;;
	      ;; This kludge is because some versions of sendmail.el
	      ;; insert an extra newline at the beginning that shouldn't
	      ;; be there.  sendmail.el has been fixed, but old versions
	      ;; may still be in use.  -- rms, 7 May 1993.
	      ((eolp) (delete-char 1))
	      (t (error "Cannot convert to babyl format")))))
    count))

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
		     "Date: \\3, \\5 \\4 \\9 \\6 "
		    
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
      (error "Bad format in RMAIL file."))
  (let ((buffer-read-only nil)
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
    (if rmail-ignored-headers (rmail-clear-headers))
    (if rmail-message-filter (funcall rmail-message-filter))))

(defun rmail-clear-headers ()
  (if (search-forward "\n\n" nil t)
      (save-restriction
        (narrow-to-region (point-min) (point))
	(let ((buffer-read-only nil))
	  (while (let ((case-fold-search t))
		   (goto-char (point-min))
		   (re-search-forward rmail-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (point)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (point))))))))

(defun rmail-toggle-header ()
  "Show original message header if pruned header currently shown, or vice versa."
  (interactive)
  (rmail-maybe-set-message-counters)
  (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (forward-line 1)
    (if (= (following-char) ?1)
	(progn (delete-char 1)
	       (insert ?0)
	       (forward-line 1)
	       (let ((case-fold-search t))
 		 (while (looking-at "Summary-Line:\\|Mail-From:")
 		   (forward-line 1)))
	       (insert "*** EOOH ***\n")
	       (forward-char -1)
	       (search-forward "\n*** EOOH ***\n")
	       (forward-line -1)
	       (let ((temp (point)))
		 (and (search-forward "\n\n" nil t)
		      (delete-region temp (point))))
	       (goto-char (point-min))
	       (search-forward "\n*** EOOH ***\n")
	       (narrow-to-region (point) (point-max)))
      (rmail-reformat-message (point-min) (point-max)))))

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
	  (concat " " rmail-current-message "/" rmail-total-messages
		  blurb))))

;; Turn an attribute of a message on or off according to STATE.
;; ATTR is the name of the attribute, as a string.
;; MSGNUM is message number to change; nil means current message.
(defun rmail-set-attribute (attr state &optional msgnum)
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
    (let ((obeg (- (point-max) (point-min))))
      (unwind-protect
	  (progn
	    (narrow-to-region (rmail-msgbeg rmail-current-message)
			      (point-max))
	    (goto-char (point-min))
	    (funcall function))
	;; Note: we don't use save-restriction because that does not work right
	;; if changes are made outside the saved restriction
	;; before that restriction is restored.
	;; Here we assume that changes made by FUNCTION
	;; occur before the visible region of the message.
	(narrow-to-region (- (point-max) obeg) (point-max))))))

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
	(search-backward "\n\^_")
	(forward-char 1)
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
	      rmail-deleted-vector (concat "D" deleted-head)
	      rmail-summary-vector (make-vector rmail-total-messages nil))
	(message "Counting messages...done")))))
	
(defun rmail-set-message-counters-counter (&optional stop)
  (while (search-backward "\n\^_\^L\n" stop t)
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
	(message "Counting messages...%d" total-messages))))

(defun rmail-beginning-of-message ()
  "Show current message starting from the beginning."
  (interactive)
  (rmail-show-message rmail-current-message))

(defun rmail-show-message (&optional n)
  "Show message number N (prefix argument), counting from start of file.
If summary buffer is currently displayed, update current message there also."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (widen)
  (if (zerop rmail-total-messages)
      (progn (narrow-to-region (point-min) (1- (point-max)))
	     (goto-char (point-min))
	     (setq mode-line-process nil))
    (let (blurb)
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
      (let ((beg (rmail-msgbeg n))
	    (end (rmail-msgend n)))
	(goto-char beg)
	(forward-line 1)
	(if (= (following-char) ?0)
	    (progn
	      (rmail-reformat-message beg end)
	      (rmail-set-attribute "unseen" nil))
	  (search-forward "\n*** EOOH ***\n" end t)
	  (narrow-to-region (point) end))
	(goto-char (point-min))
	(rmail-display-labels)
	(run-hooks 'rmail-show-message-hook)
	;; If there is a summary buffer, try to move to this message
	;; in that buffer.  But don't complain if this message
	;; is not mentioned in the summary.
	(if (rmail-summary-exists)
	    (let ((curr-msg rmail-current-message))
	      (rmail-select-summary
	       (rmail-summary-goto-msg curr-msg t t))))
	(if blurb
	    (message blurb))))))

(defun rmail-next-message (n)
  "Show following message whether deleted or not.
With prefix arg N, moves forward N messages, or backward if N is negative."
  (interactive "p")
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

(defun rmail-message-regexp-p (msg regexp)
  "Return t, if for message number MSG, regexp REGEXP matches in the header."
  (goto-char (rmail-msgbeg msg))
  (let ((end 
         (save-excursion 
           (search-forward "*** EOOH ***" (point-max)) (point))))
    (re-search-forward regexp end t)))

(defun rmail-search-backward (regexp &optional n)
  "Show message containing next match for REGEXP.
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
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
  (rmail-search regexp (- n)))

(defvar rmail-search-last-regexp nil)
(defun rmail-search (regexp &optional n)
  "Show message containing next match for REGEXP.
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
		  (goto-char (rmail-msgbeg (setq msg (1- msg))))
		  (setq win (re-search-forward
			     regexp (rmail-msgend msg) t)))
	      (while (and (null win) (< msg rmail-total-messages))
		(goto-char (rmail-msgbeg (setq msg (1+ msg))))
		(setq win (re-search-forward regexp (rmail-msgend msg) t))))
	    (setq n (+ n (if reversep 1 -1)))))
      (if win
	  (progn
	    ;; If this is a reverse search and we found a message,
	    ;; search backward thru this message to position point.
	    (if reversep
		(progn
		  (goto-char (rmail-msgend msg))
		  (re-search-backward
		   regexp (rmail-msgbeg msg) t)))
	    (setq win (point))
	    (rmail-show-message msg)
	    (message "%sRmail search for %s...done"
		     (if reversep "Reverse " "")
		     regexp)
	    (goto-char win))
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
  (rmail-search regexp (- (or n -1))))

;; Show the first message which has the `unseen' attribute.
(defun rmail-first-unseen-message ()
  (rmail-maybe-set-message-counters)
  (let ((current 1)
	found)
    (save-restriction
      (widen)
      (while (and (not found) (< current rmail-total-messages))
	(if (rmail-message-labels-p current ", ?\\(unseen\\),")
	    (setq found current))
	(setq current (1+ current))))
;; Let the caller show the message.
;;    (if found
;;	(rmail-show-message found))
    found))

;;;; *** Rmail Message Deletion Commands ***

(defun rmail-message-deleted-p (n)
  (= (aref rmail-deleted-vector n) ?D))

(defun rmail-set-message-deleted-p (n state)
  (aset rmail-deleted-vector n (if state ?D ?\ )))

(defun rmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (rmail-set-attribute "deleted" t))

(defun rmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
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
  (let ((del-msg rmail-current-message))
    (if (rmail-summary-exists)
	(save-excursion
	  (set-buffer rmail-summary-buffer)
	  (rmail-summary-mark-deleted del-msg)))
    (prog1 (rmail-next-undeleted-message (if backward -1 1))
      (rmail-maybe-display-summary))))

(defun rmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-delete-forward t))

(defun rmail-only-expunge ()
  "Actually erase all deleted messages in the file."
  (interactive)
  (message "Expunging deleted messages...")
  ;; Discard all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (rmail-maybe-set-message-counters)
  (let* ((omax (- (buffer-size) (point-max)))
	 (omin (- (buffer-size) (point-min)))
	 (opoint (if (and (> rmail-current-message 0)
			  (= ?D (aref rmail-deleted-vector rmail-current-message)))
		     0 (- (point) (point-min))))
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
			    new-summary)))
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
		  win t)))
      (message "Expunging deleted messages...done")
      (if (not win)
	  (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax)))
      (rmail-show-message
       (if (zerop rmail-current-message) 1 nil))
      (forward-char opoint))))

(defun rmail-expunge ()
  "Erase deleted messages from Rmail file and summary buffer."
  (interactive)
  (rmail-only-expunge)
  (if (rmail-summary-exists)
      (rmail-select-summary
	(rmail-update-summary))))

;;;; *** Rmail Mailing Commands ***

(defun rmail-start-mail (&rest args)
  (if (and window-system rmail-mail-new-frame)
      (prog1
	(apply 'mail-other-frame args)
	(modify-frame-parameters (selected-frame)
				 '((dedicated . t))))
    (apply 'mail-other-window args)))

(defun rmail-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (rmail-start-mail nil nil nil nil nil (current-buffer)))

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
  (let (from reply-to cc subject date to message-id resent-reply-to)
    (save-excursion
      (save-restriction
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
				   (beginning-of-line) (point))))
	(setq resent-reply-to (mail-fetch-field "resent-reply-to" t)
	      from (mail-fetch-field "from")
	      reply-to (or resent-reply-to
			   (mail-fetch-field "reply-to" nil t)
			   from)
	      cc (cond (just-sender nil)
		       (resent-reply-to (mail-fetch-field "resent-cc" t))
		       (t (mail-fetch-field "cc" nil t)))
	      subject (or (and resent-reply-to
			       (mail-fetch-field "resent-subject" t))
			  (mail-fetch-field "subject"))
	      date (cond (resent-reply-to
			  (mail-fetch-field "resent-date" t))
			 ((mail-fetch-field "date")))
	      to (cond (resent-reply-to
			(mail-fetch-field "resent-to" t))
		       ((mail-fetch-field "to" nil t))
		       ;((mail-fetch-field "apparently-to")) ack gag barf
		       (t ""))
	      message-id (cond (resent-reply-to
				(mail-fetch-field "resent-message-id" t))
			       ((mail-fetch-field "message-id"))))))
    (and (stringp subject)
	 (or (string-match (concat "\\`" (regexp-quote rmail-reply-prefix))
			   subject)
	     (setq subject (concat rmail-reply-prefix subject))))
    (rmail-start-mail nil
      (mail-strip-quoted-names reply-to)
      subject
      (rmail-make-in-reply-to-field from date message-id)
      (if just-sender
	  nil
	(let* ((cc-list (rmail-dont-reply-to
			  (mail-strip-quoted-names
			    (if (null cc) to (concat to ", " cc))))))
	  (if (string= cc-list "") nil cc-list)))
      (current-buffer)
      (list (list '(lambda (buf msgnum)
		     (save-excursion
		       (set-buffer buf)
		       (rmail-set-attribute "answered" t msgnum)))
		  (current-buffer) rmail-current-message)))))

(defun rmail-make-in-reply-to-field (from date message-id)
  (cond ((not from)
         (if message-id
             message-id
             nil))
        (mail-use-rfc822
         (require 'rfc822)
         (let ((tem (car (rfc822-addresses from))))
           (if message-id
               (if (string-match
                    (regexp-quote (if (string-match "@[^@]*\\'" tem)
                                      (substring tem 0 (match-beginning 0))
                                      tem))
                    message-id)
                   ;; Message-ID is sufficiently informative
                   message-id
                   (concat message-id " (" tem ")"))
               ;; Use prin1 to fake RFC822 quoting
               (let ((field (prin1-to-string tem)))
                 (if date
                     (concat field "'s message of " date)
                     field)))))
        ((let* ((foo "[^][\000-\037\177-\377()<>@,;:\\\" ]+")
                (bar "[^][\000-\037\177-\377()<>@,;:\\\"]+"))
           ;; Can't use format because format loses on \000 (unix *^&%*^&%$!!)
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
    (let ((forward-buffer (current-buffer))
	  (subject (concat "["
			   (let ((from (or (mail-fetch-field "From")
					   (mail-fetch-field ">From"))))
			     (if from
				 (concat (mail-strip-quoted-names from) ": ")
			       ""))
			   (or (mail-fetch-field "Subject") "")
			   "]")))
      ;; If only one window, use it for the mail buffer.
      ;; Otherwise, use another window for the mail buffer
      ;; so that the Rmail buffer remains visible
      ;; and sending the mail will get back to it.
      (if (funcall (if (and (not rmail-mail-new-frame) (one-window-p t))
		       (function mail)
		     (function rmail-start-mail))
		   nil nil subject nil nil nil
		   (list (list (function (lambda (buf msgnum)
					   (save-excursion
					     (set-buffer buf)
					     (rmail-set-attribute
					      "forwarded" t msgnum))))
			       (current-buffer)
			       rmail-current-message)))
	  (save-excursion
	    ;; Insert after header separator--before signature if any.
	    (goto-char (point-min))
	    (search-forward-regexp
	     (concat "^" (regexp-quote mail-header-separator)))
	    (forward-line 1)
	    (insert-buffer forward-buffer))))))

(defun rmail-resend (address &optional from comment mail-alias-file)
  "Resend current message to ADDRESSES.
ADDRESSES should be a single address, a a string consisting of several
addresses separated by commas, or a list of addresses.

Optional FROM is the address to resend the message from, and
defaults to the username of the person redistributing the message.
Optional COMMENT is a string that will be inserted as a comment in the
resent message.
Optional ALIAS-FILE is alternate aliases file to be used by sendmail,
typically for purposes of moderating a list."
  (interactive "sResend to: ")
  (require 'sendmail)
  (require 'mailalias)
  (if (not from) (setq from (user-login-name)))
  (let ((tembuf (generate-new-buffer " sendmail temp"))
	(mail-header-separator "")
	(case-fold-search nil)
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  ;;>> Copy message into temp buffer
	  (set-buffer tembuf)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-min))
	  ;; Delete any Sender field, since that's not specifyable.
	  (if (re-search-forward "^Sender:" nil t)
	      (let (beg)
		(beginning-of-line)
		(setq beg (point))
		(forward-line 1)
		(while (looking-at "[ \t]")
		  (forward-line 1))
		(delete-region beg (point))))
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
	    (expand-mail-aliases before (point)))
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
	    (sendmail-send-it)))
      (kill-buffer tembuf))
    (rmail-set-attribute "resent" t rmail-current-message)))

(defvar mail-unsent-separator
  (concat "^ *---+ +Unsent message follows +---+ *$\\|"
	  "^ *---+ +Returned message +---+ *$\\|"
	  "^ *---+ +Original message +---+ *$\\|"
	  "^ *--+ +begin message +--+ *$\\|"
	  "^ *---+ +Original message follows +---+ *$\\|"
	  "^|? *---+ +Message text follows: +---+ *|?$"))

(defun rmail-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message; otherwise copy the current message."
  (interactive)
  (require 'mail-utils)
  (let (to subj irp2 cc orig-message)
    (save-excursion
      ;; Narrow down to just the quoted original message
      (rmail-beginning-of-message)
      (let ((case-fold-search t))
	(or (re-search-forward mail-unsent-separator nil t)
	    (error "Cannot parse this as a failure message")))
      (save-restriction
	(narrow-to-region (point) (point-max))
	;; Now mail-fetch-field will get from headers of the original message,
	;; not from the headers of the rejection.
	(setq to   (mail-fetch-field "To")
	      subj (mail-fetch-field "Subject")
	      irp2 (mail-fetch-field "In-reply-to")
	      cc   (mail-fetch-field "Cc"))
	;; Get the entire text (not headers) of the original message.
	(setq orig-message
	      (buffer-substring
	       (progn (search-forward "\n\n") (point))
	       (point-max)))))
    ;; Start sending a new message; default header fields from the original.
    ;; Turn off the usual actions for initializing the message body
    ;; because we want to get only the text from the failure message.
    (let (mail-signature
	  (mail-setup-hook rmail-retry-setup-hook))
      (if (rmail-start-mail nil to subj irp2 cc (current-buffer))
	  ;; Insert original text as initial text of new draft message.
	  (progn
	    (goto-char (point-max))
	    (insert orig-message)
	    (goto-char (point-min))
	    (end-of-line))))))

(defun rmail-bury ()
  "Bury current Rmail buffer and its summary buffer."
  (interactive)
  (let ((rmail-buffer (current-buffer)))
    (if (rmail-summary-exists)
	(let (window)
	  (while (setq window (get-buffer-window rmail-summary-buffer))
	    (set-window-buffer window (other-buffer rmail-summary-buffer)))
	  (bury-buffer rmail-summary-buffer)))
    (switch-to-buffer (other-buffer (current-buffer)))
    (bury-buffer rmail-buffer)))

(defun rmail-summary-exists ()
  "Non-nil iff in an RMAIL buffer and an associated summary buffer exists.
Non-nil value returned is the summary buffer."
  (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
       rmail-summary-buffer))

(defun rmail-summary-displayed ()
  "t iff in RMAIL buffer and an associated summary buffer is displayed."
  (and rmail-summary-buffer (get-buffer-window rmail-summary-buffer)))

(defvar rmail-redisplay-summary nil
  "*Non-nil means Rmail should show the summary when it changes.
This has an effect only if a summary buffer exists.")

;; Put the summary buffer back on the screen, if user wants that.
(defun rmail-maybe-display-summary ()
  (and rmail-summary-buffer (buffer-name rmail-summary-buffer)
       rmail-redisplay-summary
       (display-buffer rmail-summary-buffer)))

;;;; *** Rmail Specify Inbox Files ***

(autoload 'set-rmail-inbox-list "rmailmsc"
  "Set the inbox list of the current RMAIL file to FILE-NAME.
This may be a list of file names separated by commas.
If FILE-NAME is empty, remove any inbox list."
  t)

;;;; *** Rmail Commands for Labels ***

(autoload 'rmail-add-label "rmailkwd"
  "Add LABEL to labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  t)

(autoload 'rmail-kill-label "rmailkwd"
  "Remove LABEL from labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  t)

(autoload 'rmail-next-labeled-message "rmailkwd"
  "Show next message with LABEL.  Defaults to last label used.
With prefix argument N moves forward N messages with this label."
  t)

(autoload 'rmail-previous-labeled-message "rmailkwd"
  "Show previous message with LABEL.  Defaults to last label used.
With prefix argument N moves backward N messages with this label."
  t)

(autoload 'rmail-read-label "rmailkwd"
  "PROMPT and read with completion an Rmail message label."
  t)

;;;; *** Rmail Edit Mode ***

(autoload 'rmail-edit-current-message "rmailedit"
  "Edit the contents of the current message"
  t)

;;;; *** Rmail Sorting ***

(autoload 'rmail-sort-by-date "rmailsort"
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order." t)

(autoload 'rmail-sort-by-subject "rmailsort"
  "Sort messages of current Rmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order." t)

(autoload 'rmail-sort-by-author "rmailsort"
  "Sort messages of current Rmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order." t)

(autoload 'rmail-sort-by-recipient "rmailsort"
  "Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order." t)

(autoload 'rmail-sort-by-correspondent "rmailsort"
  "Sort messages of current Rmail file by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order." t)

(autoload 'rmail-sort-by-lines "rmailsort"
  "Sort messages of current Rmail file by number of lines.
If prefix argument REVERSE is non-nil, sort them in reverse order." t)

;;;; *** Rmail Summary Mode ***

(autoload 'rmail-summary "rmailsum"
  "Display a summary of all messages, one line per message."
  t)

(autoload 'rmail-summary-by-labels "rmailsum"
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  t)

(autoload 'rmail-summary-by-recipients "rmailsum"
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers; but if PRIMARY-ONLY
is non-nil (prefix arg given), only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas."
  t)

(autoload 'rmail-summary-by-regexp "rmailsum"
  "Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Emacs will list the header line in the RMAIL-summary."
  t)

(autoload 'rmail-summary-by-topic "rmailsum"
  "Display a summary of all messages with the given SUBJECT.
Normally checks the Subject field of headers;
but if WHOLE-MESSAGE is non-nil (prefix arg given), 
 look in the whole message.
SUBJECT is a string of regexps separated by commas."
  t)

;;;; *** Rmail output messages to files ***

(autoload 'rmail-output-to-rmail-file "rmailout"
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file."
  t)

(autoload 'rmail-output "rmailout"
  "Append this message to Unix mail file named FILE-NAME."
  t)

;;;; *** Rmail undigestification ***

(autoload 'undigestify-rmail-message "undigest"
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  t)

(provide 'rmail)

;;; rmail.el ends here
