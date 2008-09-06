;;; pmailsum.el --- make summary buffers for the mail reader

;; Copyright (C) 1985, 1993, 1994, 1995, 1996, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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

;; All commands run from the summary buffer update the buffer local
;; variable `pmail-current-message'.  As part of the post command
;; processing point is moved to the beginning of the line describing
;; the current message.

;;; History:

;; Extended by Bob Weiner of Motorola
;;   Provided all commands from pmail-mode in pmail-summary-mode and made key
;;   bindings in both modes wholly compatible.

;; Overhauled by Paul Reilly to support mbox format.

;;; Code:

(defvar msgnum)

;; For pmail-select-summary
(require 'pmail)

;;;###autoload
(defcustom pmail-summary-scroll-between-messages t
  "*Non-nil means Pmail summary scroll commands move between messages."
  :type 'boolean
  :group 'pmail-summary)

;;;###autoload
(defcustom pmail-summary-line-count-flag t
  "*Non-nil if Pmail summary should show the number of lines in each message."
  :type 'boolean
  :group 'pmail-summary)

(defvar pmail-summary-font-lock-keywords
  '(("^.....D.*" . font-lock-string-face)			; Deleted.
    ("^.....-.*" . font-lock-type-face)				; Unread.
    ;; Neither of the below will be highlighted if either of the above are:
    ("^.....[^D-]....\\(......\\)" 1 font-lock-keyword-face)	; Date.
    ("{ \\([^\n}]+\\) }" 1 font-lock-comment-face))		; Labels.
  "Additional expressions to highlight in Pmail Summary mode.")

(defvar pmail-summary-redo nil
  "Private storage for Pmail summary history.")

(defvar pmail-summary-overlay nil
  "Private storage for an Pmail summary overlay cache")
(put 'pmail-summary-overlay 'permanent-local t)

(defvar pmail-summary-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [mouse-2] 'pmail-summary-mouse-goto-message)
    (define-key map "a"      'pmail-summary-add-label)
    (define-key map "b"      'pmail-summary-bury)
    (define-key map "B"      'pmail-summary-browse-body)
    (define-key map "c"      'pmail-summary-continue)
    (define-key map "d"      'pmail-summary-delete-forward)
    (define-key map "\C-d"   'pmail-summary-delete-backward)
    (define-key map "e"      'pmail-summary-edit-current-message)
    (define-key map "f"      'pmail-summary-forward)
    (define-key map "g"      'pmail-summary-get-new-mail)
    (define-key map "h"      'pmail-summary)
    (define-key map "i"      'pmail-summary-input)
    (define-key map "j"      'pmail-summary-goto-msg)
    (define-key map "\C-m"   'pmail-summary-goto-msg)
    (define-key map "k"      'pmail-summary-kill-label)
    (define-key map "l"      'pmail-summary-by-labels)
    (define-key map "\e\C-h" 'pmail-summary)
    (define-key map "\e\C-l" 'pmail-summary-by-labels)
    (define-key map "\e\C-r" 'pmail-summary-by-recipients)
    (define-key map "\e\C-f" 'pmail-summary-by-senders)
    (define-key map "\e\C-s" 'pmail-summary-by-regexp)
    (define-key map "\e\C-t" 'pmail-summary-by-topic)
    (define-key map "m"      'pmail-summary-mail)
    (define-key map "\M-m"   'pmail-summary-retry-failure)
    (define-key map "n"      'pmail-summary-next-msg)
    (define-key map "\en"    'pmail-summary-next-all)
    (define-key map "\e\C-n" 'pmail-summary-next-labeled-message)
    (define-key map "o"      'pmail-summary-output)
    (define-key map "\C-o"   'pmail-summary-output)
    (define-key map "p"      'pmail-summary-previous-msg)
    (define-key map "\ep"    'pmail-summary-previous-all)
    (define-key map "\e\C-p" 'pmail-summary-previous-labeled-message)
    (define-key map "q"      'pmail-summary-quit)
    (define-key map "Q"      'pmail-summary-wipe)
    (define-key map "r"      'pmail-summary-reply)
    (define-key map "s"      'pmail-summary-expunge-and-save)
    (define-key map "\es"    'pmail-summary-search)
    (define-key map "t"      'pmail-summary-toggle-header)
    (define-key map "u"      'pmail-summary-undelete)
    (define-key map "\M-u"   'pmail-summary-undelete-many)
    (define-key map "x"      'pmail-summary-expunge)
    (define-key map "w"      'pmail-summary-output-body)
    (define-key map "."      'pmail-summary-beginning-of-message)
    (define-key map "/"      'pmail-summary-end-of-message)
    (define-key map "<"      'pmail-summary-first-message)
    (define-key map ">"      'pmail-summary-last-message)
    (define-key map " "      'pmail-summary-scroll-msg-up)
    (define-key map "\177"   'pmail-summary-scroll-msg-down)
    (define-key map "?"      'describe-mode)
    (define-key map "\C-c\C-n" 'pmail-summary-next-same-subject)
    (define-key map "\C-c\C-p" 'pmail-summary-previous-same-subject)
    (define-key map "\C-c\C-s\C-d" 'pmail-summary-sort-by-date)
    (define-key map "\C-c\C-s\C-s" 'pmail-summary-sort-by-subject)
    (define-key map "\C-c\C-s\C-a" 'pmail-summary-sort-by-author)
    (define-key map "\C-c\C-s\C-r" 'pmail-summary-sort-by-recipient)
    (define-key map "\C-c\C-s\C-c" 'pmail-summary-sort-by-correspondent)
    (define-key map "\C-c\C-s\C-l" 'pmail-summary-sort-by-lines)
    (define-key map "\C-c\C-s\C-k" 'pmail-summary-sort-by-labels)
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar classify]
      (cons "Classify" (make-sparse-keymap "Classify")))
    (define-key map [menu-bar classify output-menu]
      '("Output (Pmail Menu)..." . pmail-summary-output-menu))
    (define-key map [menu-bar classify input-menu]
      '("Input Pmail File (menu)..." . pmail-input-menu))
    (define-key map [menu-bar classify input-menu] '(nil))
    (define-key map [menu-bar classify output-menu] '(nil))
    (define-key map [menu-bar classify output-body]
      '("Output (body)..." . pmail-summary-output-body))
    (define-key map [menu-bar classify output-inbox]
      '("Output (inbox)..." . pmail-summary-output))
    (define-key map [menu-bar classify output]
      '("Output (Pmail)..." . pmail-summary-output))
    (define-key map [menu-bar classify kill-label]
      '("Kill Label..." . pmail-summary-kill-label))
    (define-key map [menu-bar classify add-label]
      '("Add Label..." . pmail-summary-add-label))
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
    (define-key map [menu-bar mail pmail-summary-get-new-mail]
      '("Get New Mail" . pmail-summary-get-new-mail))
    (define-key map [menu-bar mail lambda]
      '("----"))
    (define-key map [menu-bar mail continue]
      '("Continue" . pmail-summary-continue))
    (define-key map [menu-bar mail resend]
      '("Re-send..." . pmail-summary-resend))
    (define-key map [menu-bar mail forward]
      '("Forward" . pmail-summary-forward))
    (define-key map [menu-bar mail retry]
      '("Retry" . pmail-summary-retry-failure))
    (define-key map [menu-bar mail reply]
      '("Reply" . pmail-summary-reply))
    (define-key map [menu-bar mail mail]
      '("Mail" . pmail-summary-mail))
    (define-key map [menu-bar delete]
      (cons "Delete" (make-sparse-keymap "Delete")))
    (define-key map [menu-bar delete expunge/save]
      '("Expunge/Save" . pmail-summary-expunge-and-save))
    (define-key map [menu-bar delete expunge]
      '("Expunge" . pmail-summary-expunge))
    (define-key map [menu-bar delete undelete]
      '("Undelete" . pmail-summary-undelete))
    (define-key map [menu-bar delete delete]
      '("Delete" . pmail-summary-delete-forward))
    (define-key map [menu-bar move]
      (cons "Move" (make-sparse-keymap "Move")))
    (define-key map [menu-bar move search-back]
      '("Search Back..." . pmail-summary-search-backward))
    (define-key map [menu-bar move search]
      '("Search..." . pmail-summary-search))
    (define-key map [menu-bar move previous]
      '("Previous Nondeleted" . pmail-summary-previous-msg))
    (define-key map [menu-bar move next]
      '("Next Nondeleted" . pmail-summary-next-msg))
    (define-key map [menu-bar move last]
      '("Last" . pmail-summary-last-message))
    (define-key map [menu-bar move first]
      '("First" . pmail-summary-first-message))
    (define-key map [menu-bar move previous]
      '("Previous" . pmail-summary-previous-all))
    (define-key map [menu-bar move next]
      '("Next" . pmail-summary-next-all))
    map)
  "Keymap for `pmail-summary-mode'.")

(declare-function pmail-abort-edit "pmailedit" ())
(declare-function pmail-cease-edit "pmailedit"())
(declare-function pmail-set-label "pmailkwd" (l state &optional n))
(declare-function pmail-output-read-file-name "pmailout" ())
(declare-function mail-comma-list-regexp "mail-utils" (labels))
(declare-function mail-send-and-exit "sendmail" (&optional arg))
(declare-function mail-strip-quoted-names "mail-utils" (address))

;; Entry points for making a summary buffer.

;; Regenerate the contents of the summary
;; using the same selection criterion as last time.
;; M-x revert-buffer in a summary buffer calls this function.
(defun pmail-update-summary (&rest ignore)
  (apply (car pmail-summary-redo) (cdr pmail-summary-redo)))

;;;###autoload
(defun pmail-summary ()
  "Display a summary of all messages, one line per message."
  (interactive)
  (pmail-new-summary "All" '(pmail-summary) nil))

;;;###autoload
(defun pmail-summary-by-labels (labels)
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  (interactive "sLabels to summarize by: ")
  (if (string= labels "")
      (setq labels (or pmail-last-multi-labels
		       (error "No label specified"))))
  (setq pmail-last-multi-labels labels)
  (pmail-new-summary (concat "labels " labels)
		     (list 'pmail-summary-by-labels labels)
		     'pmail-message-labels-p
		     (mail-comma-list-regexp labels)))

;;;###autoload
(defun pmail-summary-by-recipients (recipients &optional primary-only)
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas."
  (interactive "sRecipients to summarize by: \nP")
  (pmail-new-summary
   (concat "recipients " recipients)
   (list 'pmail-summary-by-recipients recipients primary-only)
   'pmail-message-recipients-p
   (mail-comma-list-regexp recipients) primary-only))

;;;###autoload
(defun pmail-summary-by-regexp (regexp)
  "Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Emacs will list the header line in the PMAIL-summary."
  (interactive "sRegexp to summarize by: ")
  (if (string= regexp "")
      (setq regexp (or pmail-last-regexp
			 (error "No regexp specified"))))
  (setq pmail-last-regexp regexp)
  (pmail-new-summary (concat "regexp " regexp)
		     (list 'pmail-summary-by-regexp regexp)
		     'pmail-message-regexp-p
                     regexp))

;;;###autoload
(defun pmail-summary-by-topic (subject &optional whole-message)
  "Display a summary of all messages with the given SUBJECT.
Normally checks the Subject field of headers;
but if WHOLE-MESSAGE is non-nil (prefix arg given),
 look in the whole message.
SUBJECT is a string of regexps separated by commas."
  (interactive
   (let* ((subject (with-current-buffer pmail-buffer
		     (pmail-current-subject)))
	  (prompt (concat "Topics to summarize by (regexp"
			  (if subject ", default current subject" "")
			  "): ")))
     (list (read-string prompt nil nil subject) current-prefix-arg)))
  (pmail-new-summary
   (concat "about " subject)
   (list 'pmail-summary-by-topic subject whole-message)
   'pmail-message-subject-p
   (mail-comma-list-regexp subject) whole-message))

(defun pmail-message-subject-p (msg subject &optional whole-message)
  "Return non-nil if SUBJECT is found in MSG.
If WHOLE-MESSAGE is nil only the subject header will be searched,
otherwise the whole message will be searched for text matching
SUBJECT.  Return nil to indicate that SUBJECT is not found,
non-nil otherwise."
  (save-restriction
    (narrow-to-region
     (pmail-desc-get-start msg)
     (pmail-desc-get-end msg))
    (goto-char (point-min))
    (if whole-message
	(re-search-forward subject nil t))
    (string-match subject
		  (let ((subj (pmail-header-get-header "subject")))
		    (if subj
			(funcall pmail-summary-line-decoder subj)
		      "")))))

;;;###autoload
(defun pmail-summary-by-senders (senders)
  "Display a summary of all messages with the given SENDERS.
SENDERS is a string of names separated by commas."
  (interactive
   (let* ((sender (when pmail-current-message
		    (pmail-desc-get-sender pmail-current-message)))
	  (sender-re (with-current-buffer pmail-buffer
		       (regexp-quote sender)))
	  (prompt (concat "Senders to summarize by (regexp"
			  (if sender ", default current sender" "")
			  "): ")))
     (list (read-string prompt nil nil sender))))
  (pmail-new-summary
   (concat "senders " senders)
   (list 'pmail-summary-by-senders senders)
   'pmail-message-senders-p
   (mail-comma-list-regexp senders)))

(defun pmail-message-senders-p (msg sender)
  "Return non-nil if SENDER is found in MSG.
The From header is tested."
  (save-restriction
    (narrow-to-region
     (pmail-desc-get-start msg)
     (pmail-desc-get-end msg))
    (goto-char (point-min))
    (string-match sender (or (mail-fetch-field "From") ""))))

;;;; General making of a summary buffer.

(defvar pmail-summary-symbol-number 0)

(defun pmail-new-summary (description redo-form function &rest args)
  "Create a summary of selected messages.
DESCRIPTION makes part of the mode line of the summary buffer.
For each message, FUNCTION is applied to the message number and ARGS...
and if the result is non-nil, that message is included.
nil for FUNCTION means all messages."
  (message "Computing summary lines...")
  (let ((summary-msgs ())
        (new-summary-line-count 0)
        (msgnum 1)
        current-message sumbuf was-in-summary)
    (save-excursion
      ;; Go to the Pmail buffer.
      (if (eq major-mode 'pmail-summary-mode)
	  (setq was-in-summary t))
      (set-buffer pmail-buffer)
      ;; Find its summary buffer, or make one.
      (setq current-message pmail-current-message
	    sumbuf
	    (if (and pmail-summary-buffer
		     (buffer-name pmail-summary-buffer))
		pmail-summary-buffer
	      (generate-new-buffer (concat (buffer-name) "-summary"))))
      ;; Collect the message summaries based on the filtering
      ;; argument (FUNCTION).
      (while (>= pmail-total-messages msgnum)
	(if (or (null function)
		(apply function (cons msgnum args)))
	    (setq summary-msgs
		  (cons (cons msgnum (pmail-summary-get-summary-line msgnum))
			summary-msgs)))
	(setq msgnum (1+ msgnum)))
      (setq summary-msgs (nreverse summary-msgs))
      ;; Place the collected summaries into the summary buffer.
      (setq pmail-summary-buffer nil)
      (save-excursion
	(let ((rbuf (current-buffer))
	      (vbuf pmail-view-buffer)
	      (total pmail-total-messages))
	  (set-buffer sumbuf)
	  ;; Set up the summary buffer's contents.
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (while summary-msgs
	      (princ (cdr (car summary-msgs)) sumbuf)
	      (setq summary-msgs (cdr summary-msgs)))
	    (goto-char (point-min)))
	  ;; Set up the rest of its state and local variables.
	  (setq buffer-read-only t)
	  (pmail-summary-mode)
	  (make-local-variable 'minor-mode-alist)
	  (setq minor-mode-alist (list (list t (concat ": " description))))
	  (setq pmail-buffer rbuf
		pmail-view-buffer vbuf
		pmail-summary-redo redo-form
		pmail-total-messages total
		pmail-current-message current-message)))
      (setq pmail-summary-buffer sumbuf))
    ;; Now display the summary buffer and go to the right place in it.
    (or was-in-summary
	(progn
	  (if (and (one-window-p)
		   pop-up-windows (not pop-up-frames))
	      ;; If there is just one window, put the summary on the top.
	      (progn
		(split-window (selected-window) pmail-summary-window-size)
		(select-window (next-window (frame-first-window)))
		(pop-to-buffer sumbuf)
		;; If pop-to-buffer did not use that window, delete that
		;; window.  (This can happen if it uses another frame.)
		(if (not (eq sumbuf (window-buffer (frame-first-window))))
		    (delete-other-windows)))
	    (pop-to-buffer sumbuf))
	  (set-buffer pmail-buffer)
	  ;; This is how pmail makes the summary buffer reappear.
	  ;; We do this here to make the window the proper size.
	  (pmail-select-summary nil)
	  (set-buffer pmail-summary-buffer)))
    (pmail-summary-goto-msg current-message nil t)
    (pmail-summary-construct-io-menu)
    (message "Computing summary lines...done")))

;;;; Low levels of generating a summary.

;;;###autoload
(defcustom pmail-summary-line-decoder (function identity)
  "*Function to decode summary-line.

By default, `identity' is set."
  :type 'function
  :group 'pmail-summary)

;;;###autoload
(defcustom pmail-user-mail-address-regexp
  (concat "^\\("
	  (regexp-quote (user-login-name))
	  "\\($\\|@\\)\\|"
	  (regexp-quote
	   (or user-mail-address
	       (concat (user-login-name) "@"
		       (or mail-host-address
			   (system-name)))))
	  "\\>\\)")
  "*Regexp matching user mail addresses.
If non-nil, this variable is used to identify the correspondent
when receiving new mail.  If it matches the address of the
sender, the recipient is taken as correspondent of a mail.  It is
initialized based on your `user-login-name' and
`user-mail-address'.

Usually you don't have to set this variable, except if you
collect mails sent by you under different user names.  Then it
should be a regexp matching your mail addresses.

Setting this variable has an effect only before reading a mail."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'pmail-retrieve
  :version "21.1")


;;;; Simple motion in a summary buffer.

(defun pmail-summary-next-all (&optional number)
  "Move to an nearby message.
If NUMBER is positive then move forward NUMBER messages.  If NUMBER is
negative then move backwards NUMBER messages.  If NUMBER is nil then
move forward one message."
  (interactive "p")
  (forward-line (if number number 1))
  ;; It doesn't look nice to move forward past the last message line.
  (and (eobp) (> number 0)
       (forward-line -1))
  (display-buffer pmail-buffer))

(defun pmail-summary-previous-all (&optional number)
  (interactive "p")
  (forward-line (- (if number number 1)))
  ;; It doesn't look nice to move forward past the last message line.
  (and (eobp) (< number 0)
       (forward-line -1))
  (display-buffer pmail-buffer))

(defun pmail-summary-next-msg (&optional number)
  "Display next non-deleted msg from pmail file.
With optional prefix argument NUMBER, moves forward this number of
non-deleted messages, or backward if NUMBER is negative."
  (interactive "p")
  (let (msg)
    (with-current-buffer pmail-buffer
      (pmail-next-undeleted-message number)
      (setq msg pmail-current-message))
    (pmail-summary-goto-msg msg)))

(defun pmail-summary-previous-msg (&optional number)
  "Display previous non-deleted msg from pmail file.
With optional prefix argument NUMBER, moves backward this number of
non-deleted messages."
  (interactive "p")
  (pmail-summary-next-msg (- (if number number 1))))

(defun pmail-summary-next-labeled-message (n labels)
  "Show next message with LABEL.  Defaults to last labels used.
With prefix argument N moves forward N messages with these labels."
  (interactive "p\nsMove to next msg with labels: ")
  (let (msg)
    (save-excursion
      (set-buffer pmail-buffer)
      (pmail-next-labeled-message n labels)
      (setq msg pmail-current-message))
    (setq pmail-current-message msg)))

(defun pmail-summary-previous-labeled-message (n labels)
  "Show previous message with LABEL.  Defaults to last labels used.
With prefix argument N moves backward N messages with these labels."
  (interactive "p\nsMove to previous msg with labels: ")
  (let (msg)
    (save-excursion
      (set-buffer pmail-buffer)
      (pmail-previous-labeled-message n labels)
      (setq msg pmail-current-message))
    (setq pmail-current-message msg)))

(defun pmail-summary-next-same-subject (n)
  "Go to the next message in the summary having the same subject.
With prefix argument N, do this N times.
If N is negative, go backwards."
  (interactive "p")
  (with-current-buffer pmail-buffer
    (pmail-next-same-subject n)))

(defun pmail-summary-previous-same-subject (n)
  "Go to the previous message in the summary having the same subject.
With prefix argument N, do this N times.
If N is negative, go forwards instead."
  (interactive "p")
  (pmail-summary-next-same-subject (- n)))


;; Delete and undelete summary commands.

(defun pmail-summary-delete-forward (&optional count)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[pmail-expunge] command is given.
A prefix argument serves as a repeat count;
a negative argument means to delete and move backward."
  (interactive "p")
  (unless (numberp count) (setq count 1))
  (let (end del-msg
	    (backward (< count 0)))
    (while (/= count 0)
      (pmail-summary-goto-msg)
      (with-current-buffer pmail-buffer
	(pmail-delete-message)
	(setq del-msg pmail-current-message))
      (pmail-summary-mark-deleted del-msg)
      (while (and (not (if backward (bobp) (eobp)))
		  (save-excursion (beginning-of-line)
				  (looking-at " *[0-9]+D")))
	(forward-line (if backward -1 1)))
      (setq count
	    (if (> count 0) (1- count) (1+ count))))
    ;; Update the summary buffer current message counter and show the
    ;; message in the Pmail buffer.
    (pmail-summary-goto-msg (pmail-summary-get-message-at-point))))

(defun pmail-summary-delete-backward (&optional count)
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[pmail-expunge] command is given.
A prefix argument serves as a repeat count;
a negative argument means to delete and move forward."
  (interactive "p")
  (pmail-summary-delete-forward (- count)))

(defun pmail-summary-mark-deleted (&optional n undel)
  ;; Since third arg is t, this only alters the summary, not the Pmail buf.
  (and n (pmail-summary-goto-msg n t))
  (or (eobp)
      (not (overlay-get pmail-summary-overlay 'face))
      (let ((buffer-read-only nil))
	(skip-chars-forward " ")
	(skip-chars-forward "[0-9]")
	(if undel
	    (if (looking-at "D")
		(progn (delete-char 1) (insert " ")))
	  (delete-char 1)
	  (insert "D"))))
  (beginning-of-line))

(defun pmail-summary-mark-undeleted (n)
  (pmail-summary-mark-deleted n t))

(defun pmail-summary-deleted-p (&optional n)
  (unless n (setq n pmail-current-message))
  (with-current-buffer pmail-buffer
    (pmail-desc-deleted-p n)))

(defun pmail-summary-undelete (&optional arg)
  "Undelete current message.
Optional prefix ARG means undelete ARG previous messages."
  (interactive "p")
  (if (/= arg 1)
      (pmail-summary-undelete-many arg)
    (let ((buffer-read-only nil)
	  (opoint (point)))
      (goto-char (line-end-position))
      (if (not (re-search-backward "\\(^ *[0-9]*\\)\\(D\\)" nil t))
	  (goto-char opoint)
	(replace-match "\\1 ")
	(pmail-summary-goto-msg)
	(if pmail-enable-mime
	    (set-buffer pmail-buffer)
	  (pop-to-buffer pmail-buffer))
	(when (pmail-message-deleted-p pmail-current-message)
	  (pmail-undelete-previous-message))
	(when pmail-enable-mime
	  (pop-to-buffer pmail-view-buffer))
	(pop-to-buffer pmail-summary-buffer)))))

(defun pmail-summary-undelete-many (&optional n)
  "Undelete all deleted msgs, optional prefix arg N means undelete N prev msgs."
  (interactive "P")
  (with-current-buffer pmail-buffer
    (let* ((init-msg (if n pmail-current-message pmail-total-messages))
	   (pmail-current-message init-msg)
	   (n (or n pmail-total-messages))
	   (msgs-undeled 0))
      (while (and (> pmail-current-message 0) (< msgs-undeled n))
	(when (pmail-message-deleted-p pmail-current-message)
	  (pmail-set-attribute "deleted" nil)
	  (setq msgs-undeled (1+ msgs-undeled)))
	(setq pmail-current-message (1- pmail-current-message)))
      (with-current-buffer pmail-summary-buffer
	(setq pmail-current-message init-msg msgs-undeled 0)
	(while (and (> pmail-current-message 0) (< msgs-undeled n))
	  (when (pmail-summary-deleted-p pmail-current-message)
	    (pmail-summary-mark-undeleted pmail-current-message)
	    (setq msgs-undeled (1+ msgs-undeled)))
	  (setq pmail-current-message (1- pmail-current-message)))))))

;; Pmail Summary mode is suitable only for specially formatted data.
(put 'pmail-summary-mode 'mode-class 'special)

(defun pmail-summary-mode ()
  "Pmail Summary Mode is invoked from Pmail Mode by using \\<pmail-mode-map>\\[pmail-summary].
As commands are issued in the summary buffer, they are applied to the
corresponding mail messages in the pmail buffer.

All normal editing commands are turned off.
Instead, nearly all the Pmail mode commands are available,
though many of them move only among the messages in the summary.

These additional commands exist:

\\[pmail-summary-undelete-many]	Undelete all or prefix arg deleted messages.
\\[pmail-summary-wipe] Delete the summary and go to the Pmail buffer.

Commands for filtering the summary:

\\[pmail-summary-by-labels] Filter by label.
\\[pmail-summary-by-topic] Filter by Subject.
      Filter by the entire message (header and body) if given a
      prefix argument.
\\[pmail-summary-by-senders] Filter by From field.
\\[pmail-summary-by-recipients] Filter by To, From, and Cc fields.
      Filter by To and From only if given a prefix argument.

The commands listed above take comma-separated lists of regular
expressions.

\\[pmail-summary-by-regexp] Filter by any header line.
\\[pmail-summary] Restore the default summary.

Commands for sorting the summary:

\\[pmail-summary-sort-by-date] Sort by date.
\\[pmail-summary-sort-by-subject] Sort by subject.
\\[pmail-summary-sort-by-author] Sort by author.
\\[pmail-summary-sort-by-recipient] Sort by recipient.
\\[pmail-summary-sort-by-correspondent] Sort by correspondent.
\\[pmail-summary-sort-by-lines] Sort by lines.
\\[pmail-summary-sort-by-labels] Sort by labels."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'pmail-summary-mode)
  (setq mode-name "PMAIL Summary")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'pmail-buffer)
  (make-local-variable 'pmail-view-buffer)
  (make-local-variable 'pmail-total-messages)
  (make-local-variable 'pmail-current-message)
  (setq pmail-current-message nil)
  (make-local-variable 'pmail-summary-redo)
  (setq pmail-summary-redo nil)
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pmail-summary-font-lock-keywords t))
  (pmail-summary-enable)
  (run-mode-hooks 'pmail-summary-mode-hook))

;; Summary features need to be disabled during edit mode.
(defun pmail-summary-disable ()
  (use-local-map text-mode-map)
  (remove-hook 'post-command-hook 'pmail-summary-pmail-update t)
  (setq revert-buffer-function nil))

(defun pmail-summary-enable ()
  (use-local-map pmail-summary-mode-map)
  (add-hook 'post-command-hook 'pmail-summary-pmail-update nil t)
  (setq revert-buffer-function 'pmail-update-summary))

(defvar pmail-summary-put-back-unseen nil
  "Used for communicating between calls to `pmail-summary-pmail-update'.
If it moves to a message within an Incremental Search, and removes
the `unseen' attribute from that message, it sets this flag
so that if the next motion between messages is in the same Incremental
Search, the `unseen' attribute is restored.")

(defun pmail-summary-pmail-update ()
  "Update the Pmail summary buffer.
Put the cursor on the beginning of the line containing the
current message and highlight the buffer.  Show in Pmail the
message described by the summary line that point is on, but only
if the Pmail buffer is already visible.  This is on
`post-command-hook' in summary buffers."
  (let (buffer-read-only)
    (save-excursion
      ;; If at end of buffer, pretend we are on the last text line.
      (when (eobp)
	(forward-line -1))
      ;; Determine the message number corresponding to line point is on.
      (beginning-of-line)
      (skip-chars-forward " ")
      (let ((msg-num (string-to-number (buffer-substring
					(point)
					(progn (skip-chars-forward "0-9")
					       (point))))))
	;; Always leave `unseen' removed if we get out of isearch mode.
	;; Don't let a subsequent isearch restore `unseen'.
	(when (not isearch-mode)
	  (setq pmail-summary-put-back-unseen nil))
	(or (eq pmail-current-message msg-num)
	    (let ((window (get-buffer-window pmail-view-buffer t))
		  (owin (selected-window)))
	      (if isearch-mode
		  (save-excursion
		    (set-buffer pmail-buffer)
		    ;; If we first saw the previous message in this
		    ;; search, and we have gone to a different message
		    ;; while searching, put back `unseen' on the former
		    ;; one.
		    (if pmail-summary-put-back-unseen
			(pmail-set-attribute "unseen" t
					     pmail-current-message))
		    ;; Arrange to do that later, for the new current message,
		    ;; if it still has `unseen'.
		    (setq pmail-summary-put-back-unseen
			  (member "unseen" (pmail-desc-get-keywords msg-num))))
		(setq pmail-summary-put-back-unseen nil))
	      ;; Go to the desired message.
	      (setq pmail-current-message msg-num)
	      ;; Update the summary to show the message has been seen.
	      (when (= (following-char) ?-)
		(delete-char 1)
		(insert " "))
	      (if window
		  ;; Using save-window-excursion would cause the new value
		  ;; of point to get lost.
		  (unwind-protect
		      (progn
			(select-window window)
			(pmail-show-message msg-num t))
		    (select-window owin))
		(when (buffer-name pmail-buffer)
		  (save-excursion
		      (set-buffer pmail-buffer)
		      (pmail-show-message msg-num t))))))
	(pmail-summary-update-highlight nil)))))

(defun pmail-summary-mouse-goto-message (event)
  "Select the message whose summary line you click on."
  (interactive "@e")
  (goto-char (posn-point (event-end event)))
  (setq pmail-current-message (pmail-summary-get-message-at-point))
  (pmail-summary-pmail-update))

(defun pmail-summary-get-message-at-point ()
  "Return the message number corresponding to the line containing point.
If the summary buffer contains no messages, nil is returned."
  (save-excursion
    ;; Position point at the beginning of a line.
    (if (eobp)
        (forward-line -1)
      (forward-line 0))
    ;; Parse the message number.
    (string-to-number
     (buffer-substring (point) (min (point-max) (+ 6 (point)))))))

(defun pmail-summary-goto-msg (&optional n nowarn skip-pmail)
  "Go to message N in the summary buffer and the Pmail buffer.
If N is nil, use the message corresponding to point in the summary
buffer and move to that message in the Pmail buffer.

If NOWARN, don't say anything if N is out of range.
If SKIP-PMAIL, don't do anything to the Pmail buffer."
  (interactive "P")
  (if (consp n) (setq n (prefix-numeric-value n)))
  ;; Do the end of buffer adjustment.
  (if (eobp) (forward-line -1))
  (beginning-of-line)
  ;; Set N to the current message unless it was already set by the
  ;; caller.
  (unless n (setq n (pmail-summary-get-message-at-point)))
  (let* ((obuf (current-buffer))
	 (buf pmail-buffer)
	 (cur (point))
	 message-not-found
	 (curmsg (string-to-number
		  (buffer-substring (point)
				    (min (point-max) (+ 6 (point))))))
	 (total (with-current-buffer buf
		  pmail-total-messages)))
    ;; Do a validity check on N.  If it is valid then set the current
    ;; summary message to N.  `pmail-summary-pmail-update' will then
    ;; actually move point to the selected message.
    (if (< n 1)
	(progn (message "No preceding message")
	       (setq n 1)))
    (if (and (> n total)
	     (> total 0))
	(progn (message "No following message")
	       (goto-char (point-max))
	       (pmail-summary-goto-msg nil nowarn skip-pmail)))
    (goto-char (point-min))
    (if (not (re-search-forward (format "^%5d[^0-9]" n) nil t))
	(progn (or nowarn (message "Message %d not found" n))
	       (setq n curmsg)
	       (setq message-not-found t)
	       (goto-char cur)))
    (beginning-of-line)
    (skip-chars-forward " ")
    (skip-chars-forward "0-9")
    (save-excursion (if (= (following-char) ?-)
			(let ((buffer-read-only nil))
			  (delete-char 1)
			  (insert " "))))
    (pmail-summary-update-highlight message-not-found)
    (beginning-of-line)
    ;; Determine if the Pmail buffer needs to be processed.
    (if skip-pmail
	nil
      ;; It does.
      (let ((selwin (selected-window)))
	(unwind-protect
	    (progn (pop-to-buffer buf)
		   (pmail-show-message n))
	  (select-window selwin)
	  ;; The actions above can alter the current buffer.  Preserve it.
	  (set-buffer obuf))))))

;; Update the highlighted line in an pmail summary buffer.
;; That should be current.  We highlight the line point is on.
;; If NOT-FOUND is non-nil, we turn off highlighting.
(defun pmail-summary-update-highlight (not-found)
  ;; Make sure we have an overlay to use.
  (or pmail-summary-overlay
      (progn
	(make-local-variable 'pmail-summary-overlay)
	(setq pmail-summary-overlay (make-overlay (point) (point)))))
  ;; If this message is in the summary, use the overlay to highlight it.
  ;; Otherwise, don't highlight anything.
  (if not-found
      (overlay-put pmail-summary-overlay 'face nil)
    (move-overlay pmail-summary-overlay
		  (save-excursion (beginning-of-line)
				  (skip-chars-forward " ")
				  (point))
		  (save-excursion (end-of-line) (point)))
    (overlay-put pmail-summary-overlay 'face 'highlight)))

(defun pmail-summary-scroll-msg-up (&optional dist)
  "Scroll the Pmail window forward.
If the Pmail window is displaying the end of a message,
advance to the next message."
  (interactive "P")
  (if (eq dist '-)
      (pmail-summary-scroll-msg-down nil)
    (let ((pmail-buffer-window (get-buffer-window pmail-view-buffer)))
      (if pmail-buffer-window
	  (if (let ((pmail-summary-window (selected-window)))
		(select-window pmail-buffer-window)
		(prog1
		    ;; Is EOB visible in the buffer?
		    (save-excursion
		      (let ((ht (window-height (selected-window))))
			(move-to-window-line (- ht 2))
			(end-of-line)
			(eobp)))
		  (select-window pmail-summary-window)))
	      (if (not pmail-summary-scroll-between-messages)
		  (error "End of buffer")
		(pmail-summary-next-msg (or dist 1)))
	    (let ((other-window-scroll-buffer pmail-view-buffer))
	      (scroll-other-window dist)))
	;; If it isn't visible at all, show the beginning.
	(pmail-summary-beginning-of-message)))))

(defun pmail-summary-scroll-msg-down (&optional dist)
  "Scroll the Pmail window backward.
If the Pmail window is now displaying the beginning of a message,
move to the previous message."
  (interactive "P")
  (if (eq dist '-)
      (pmail-summary-scroll-msg-up nil)
    (let ((pmail-buffer-window (get-buffer-window pmail-view-buffer)))
      (if pmail-buffer-window
	  (if (let ((pmail-summary-window (selected-window)))
		(select-window pmail-buffer-window)
		(prog1
		    ;; Is BOB visible in the buffer?
		    (save-excursion
		      (move-to-window-line 0)
		      (beginning-of-line)
		      (bobp))
		  (select-window pmail-summary-window)))
	      (if (not pmail-summary-scroll-between-messages)
		  (error "Beginning of buffer")
		(pmail-summary-previous-msg (or dist 1)))
	    (let ((other-window-scroll-buffer pmail-view-buffer))
	      (scroll-other-window-down dist)))
	;; If it isn't visible at all, show the beginning.
	(pmail-summary-beginning-of-message)))))

(defun pmail-summary-beginning-of-message ()
  "Show current message from the beginning."
  (interactive)
  (pmail-summary-show-message 'BEG))

(defun pmail-summary-end-of-message ()
  "Show bottom of current message."
  (interactive)
  (pmail-summary-show-message 'END))

(defun pmail-summary-show-message (where)
  "Show current mail message.
Position it according to WHERE which can be BEG or END"
  (if (and (one-window-p) (not pop-up-frames))
      ;; If there is just one window, put the summary on the top.
      (let ((buffer pmail-view-buffer))
	(split-window (selected-window) pmail-summary-window-size)
	(select-window (frame-first-window))
	(pop-to-buffer pmail-view-buffer)
	;; If pop-to-buffer did not use that window, delete that
	;; window.  (This can happen if it uses another frame.)
	(or (eq buffer (window-buffer (next-window (frame-first-window))))
	    (delete-other-windows)))
    (pop-to-buffer pmail-view-buffer))
  (cond ((eq where 'BEG)
	 (goto-char (point-min))
	 (search-forward "\n\n"))
	((eq where 'END)
	 (goto-char (point-max))
	 (recenter (1- (window-height)))))
  (pop-to-buffer pmail-summary-buffer))

(defun pmail-summary-bury ()
  "Bury the Pmail buffer and the Pmail summary buffer."
  (interactive)
  (let ((buffer-to-bury (current-buffer)))
    (let (window)
      (while (setq window (get-buffer-window pmail-buffer))
	(set-window-buffer window (other-buffer pmail-buffer)))
      (bury-buffer pmail-buffer))
    (switch-to-buffer (other-buffer buffer-to-bury))
    (bury-buffer buffer-to-bury)))

(defun pmail-summary-quit ()
  "Quit out of Pmail and Pmail summary."
  (interactive)
  (pmail-summary-wipe)
  (pmail-quit))

(defun pmail-summary-wipe ()
  "Kill and wipe away Pmail summary, remaining within Pmail."
  (interactive)
  (save-excursion (set-buffer pmail-buffer) (setq pmail-summary-buffer nil))
  (let ((local-pmail-buffer pmail-view-buffer))
    (kill-buffer (current-buffer))
    ;; Delete window if not only one.
    (if (not (eq (selected-window) (next-window nil 'no-minibuf)))
	(delete-window))
    ;; Switch windows to the pmail buffer, or switch to it in this window.
    (pop-to-buffer local-pmail-buffer)))

(defun pmail-summary-expunge ()
  "Actually erase all deleted messages and recompute summary headers."
  (interactive)
  (set-buffer pmail-buffer)
  (pmail-expunge)
  (set-buffer pmail-summary-buffer))

(defun pmail-summary-expunge-and-save ()
  "Expunge and save PMAIL file."
  (interactive)
  (set-buffer pmail-buffer)
  (pmail-expunge)
  (save-buffer)
  (set-buffer pmail-summary-buffer)
  (set-buffer-modified-p nil))

(defun pmail-summary-get-new-mail (&optional file-name)
  "Get new mail and recompute summary headers.

Optionally you can specify the file to get new mail from.  In this case,
the file of new mail is not changed or deleted.  Noninteractively, you can
pass the inbox file name as an argument.  Interactively, a prefix
argument says to read a file name and use that file as the inbox."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (let (current-message new-mail)
    (with-current-buffer pmail-buffer
      (setq new-mail (pmail-get-new-mail file-name)
	    current-message pmail-current-message))
    ;; If new mail was found, display of the correct message was
    ;; done elsewhere.
    (unless new-mail
      (pmail-summary-goto-msg current-message nil t))))

(defun pmail-summary-input (filename)
  "Run Pmail on file FILENAME."
  (interactive "FRun pmail on PMAIL file: ")
  ;; We switch windows here, then display the other Pmail file there.
  (pop-to-buffer pmail-buffer)
  (pmail filename))

(defun pmail-summary-first-message ()
  "Show first message in Pmail file from summary buffer."
  (interactive)
  (with-no-warnings
    (beginning-of-buffer)))

(defun pmail-summary-last-message ()
  "Show last message in Pmail file from summary buffer."
  (interactive)
  (with-no-warnings
    (end-of-buffer))
  (forward-line -1))

(defvar pmail-summary-edit-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (define-key map "\C-c\C-c" 'pmail-cease-edit)
    (define-key map "\C-c\C-]" 'pmail-abort-edit)
    map)
  "Mode map to use when editing the pmail summary.")

(defun pmail-summary-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (pop-to-buffer pmail-buffer)
  (pmail-edit-current-message)
  (use-local-map pmail-summary-edit-map))

(defun pmail-summary-cease-edit ()
  "Finish editing message, then go back to Pmail summary buffer."
  (interactive)
  (pmail-cease-edit)
  (pop-to-buffer pmail-summary-buffer))

(defun pmail-summary-abort-edit ()
  "Abort edit of current message; restore original contents.
Go back to summary buffer."
  (interactive)
  (pmail-abort-edit)
  (pop-to-buffer pmail-summary-buffer))

(defun pmail-summary-search-backward (regexp &optional n)
  "Show message containing next match for REGEXP.
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
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
  ;; Don't use save-excursion because that prevents point from moving
  ;; properly in the summary buffer.
  (let ((buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer pmail-buffer)
	  (pmail-search regexp (- n)))
      (set-buffer buffer))))

(defun pmail-summary-search (regexp &optional n)
  "Show message containing next match for REGEXP.
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
  ;; Don't use save-excursion because that prevents point from moving
  ;; properly in the summary buffer.
  (let ((buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer pmail-buffer)
	  (pmail-search regexp n))
      (set-buffer buffer))))

(defun pmail-summary-toggle-header ()
  "Show original message header if pruned header currently shown, or vice versa."
  (interactive)
  (with-current-buffer pmail-buffer
    (pmail-toggle-header)))

(defun pmail-summary-add-label (label)
  "Add LABEL to labels associated with current Pmail message.
Completion is performed over known labels when reading."
  (interactive (list (with-current-buffer pmail-buffer
		       (pmail-read-label "Add label"))))
  (with-current-buffer pmail-buffer
    (pmail-add-label label)))

(defun pmail-summary-kill-label (label)
  "Remove LABEL from labels associated with current Pmail message.
Completion is performed over known labels when reading."
  (interactive (list (with-current-buffer pmail-buffer
		       (pmail-read-label "Kill label" t))))
  (with-current-buffer pmail-buffer
    (pmail-kill-label label)))

;;;; *** Pmail Summary Mailing Commands ***

(defun pmail-summary-override-mail-send-and-exit ()
  "Replace bindings to 'mail-send-and-exit with 'pmail-summary-send-and-exit"
  (use-local-map (copy-keymap (current-local-map)))
  (dolist (key (where-is-internal 'mail-send-and-exit))
    (define-key (current-local-map) key 'pmail-summary-send-and-exit)))

(defun pmail-summary-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (let ((window (get-buffer-window pmail-buffer)))
    (if window
	(select-window window)
      (set-buffer pmail-buffer)))
  (pmail-start-mail nil nil nil nil nil (current-buffer))
  (pmail-summary-override-mail-send-and-exit))

(defun pmail-summary-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (let ((window (get-buffer-window pmail-buffer)))
    (if window
	(select-window window)
      (set-buffer pmail-buffer)))
  (pmail-start-mail t))

(defun pmail-summary-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (let ((window (get-buffer-window pmail-view-buffer)))
    (if window
	(select-window window)
      (set-buffer pmail-view-buffer)))
  (pmail-reply just-sender)
  (pmail-summary-override-mail-send-and-exit))

(defun pmail-summary-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message; otherwise copy the current message."
  (interactive)
  (let ((window (get-buffer-window pmail-buffer)))
    (if window
	(select-window window)
      (set-buffer pmail-buffer)))
  (pmail-retry-failure)
  (pmail-summary-override-mail-send-and-exit))

(defun pmail-summary-send-and-exit ()
  "Send mail reply and return to summary buffer."
  (interactive)
  (mail-send-and-exit t))

(defun pmail-summary-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `pmail-resend'."
  (interactive "P")
  (save-excursion
    (let ((window (get-buffer-window pmail-buffer)))
      (if window
	  (select-window window)
	(set-buffer pmail-buffer)))
    (pmail-forward resend)
    (pmail-summary-override-mail-send-and-exit)))

(defun pmail-summary-resend ()
  "Resend current message using `pmail-resend'."
  (interactive)
  (save-excursion
    (let ((window (get-buffer-window pmail-buffer)))
      (if window
	  (select-window window)
	(set-buffer pmail-buffer)))
    (call-interactively 'pmail-resend)))

;;;; Summary output commands.

(defun pmail-summary-output-to-pmail-file (&optional file-name n)
  "Append the current message to an Pmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file.

A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count."
  (interactive
   (progn (require 'pmailout)
	  (list (pmail-output-read-file-name)
		(prefix-numeric-value current-prefix-arg))))
  (let ((i 0) prev-msg)
    (while
	(and (< i n)
	     (progn (pmail-summary-goto-msg)
		    (not (eq prev-msg
			     (setq prev-msg
				   (with-current-buffer pmail-buffer
				     pmail-current-message))))))
      (setq i (1+ i))
      (with-current-buffer pmail-buffer
	(let ((pmail-delete-after-output nil))
	  (pmail-output-to-pmail-file file-name 1)))
      (if pmail-delete-after-output
	  (pmail-summary-delete-forward nil)
	(if (< i n)
	    (pmail-summary-next-msg 1))))))

(defun pmail-summary-output (&optional file-name n)
  "Append this message to Unix mail file named FILE-NAME.

A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count."
  (interactive
   (progn (require 'pmailout)
	  (list (pmail-output-read-file-name)
		(prefix-numeric-value current-prefix-arg))))
  (let ((i 0) prev-msg)
    (while
	(and (< i n)
	     (progn (pmail-summary-goto-msg)
		    (not (eq prev-msg
			     (setq prev-msg
				   (with-current-buffer pmail-buffer
				     pmail-current-message))))))
      (setq i (1+ i))
      (with-current-buffer pmail-buffer
	(let ((pmail-delete-after-output nil))
	  (pmail-output file-name 1)))
      (if pmail-delete-after-output
	  (pmail-summary-delete-forward nil)
	(if (< i n)
	    (pmail-summary-next-msg 1))))))

(defun pmail-summary-output-menu ()
  "Output current message to another Pmail file, chosen with a menu.
Also set the default for subsequent \\[pmail-output-to-pmail-file] commands.
The variables `pmail-secondary-file-directory' and
`pmail-secondary-file-regexp' control which files are offered in the menu."
  (interactive)
  (save-excursion
    (set-buffer pmail-buffer)
    (let ((pmail-delete-after-output nil))
      (call-interactively 'pmail-output-menu)))
  (if pmail-delete-after-output
      (pmail-summary-delete-forward nil)))

(defun pmail-summary-construct-io-menu ()
  (let ((files (pmail-find-all-files pmail-secondary-file-directory)))
    (if files
	(progn
	  (define-key pmail-summary-mode-map [menu-bar classify input-menu]
	    (cons "Input Pmail File"
		  (pmail-list-to-menu "Input Pmail File"
				      files
				      'pmail-summary-input)))
	  (define-key pmail-summary-mode-map [menu-bar classify output-menu]
	    (cons "Output Pmail File"
		  (pmail-list-to-menu "Output Pmail File"
				      files
				      'pmail-summary-output-to-pmail-file))))
      (define-key pmail-summary-mode-map [menu-bar classify input-menu]
	'("Input Pmail File" . pmail-disable-menu))
      (define-key pmail-summary-mode-map [menu-bar classify output-menu]
	'("Output Pmail File" . pmail-disable-menu)))))

(defun pmail-summary-output-body (&optional file-name)
  "Write this message body to the file FILE-NAME.
FILE-NAME defaults, interactively, from the Subject field of the message."
  (interactive)
  (save-excursion
    (set-buffer pmail-buffer)
    (let ((pmail-delete-after-output nil))
      (if file-name
	  (pmail-output-body-to-file file-name)
	(call-interactively 'pmail-output-body-to-file))))
  (if pmail-delete-after-output
      (pmail-summary-delete-forward nil)))

;; Sorting messages in Pmail Summary buffer.

(defun pmail-summary-sort-by-date (reverse)
  "Sort messages of current Pmail summary by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-from-summary (function pmail-sort-by-date) reverse))

(defun pmail-summary-sort-by-subject (reverse)
  "Sort messages of current Pmail summary by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-from-summary (function pmail-sort-by-subject) reverse))

(defun pmail-summary-sort-by-author (reverse)
  "Sort messages of current Pmail summary by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-from-summary (function pmail-sort-by-author) reverse))

(defun pmail-summary-sort-by-recipient (reverse)
  "Sort messages of current Pmail summary by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-from-summary (function pmail-sort-by-recipient) reverse))

(defun pmail-summary-sort-by-correspondent (reverse)
  "Sort messages of current Pmail summary by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-from-summary (function pmail-sort-by-correspondent) reverse))

(defun pmail-summary-sort-by-lines (reverse)
  "Sort messages of current Pmail summary by lines of the message.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-from-summary (function pmail-sort-by-lines) reverse))

(defun pmail-summary-sort-by-labels (reverse labels)
  "Sort messages of current Pmail summary by labels.
If prefix argument REVERSE is non-nil, sort them in reverse order.
KEYWORDS is a comma-separated list of labels."
  (interactive "P\nsSort by labels: ")
  (pmail-sort-from-summary
   (function (lambda (reverse)
	       (pmail-sort-by-labels reverse labels)))
   reverse))

(defun pmail-sort-from-summary (sortfun reverse)
  "Sort Pmail messages from Summary buffer and update it after sorting."
  (require 'pmailsort)
  (let ((selwin (selected-window)))
    (unwind-protect
	(progn (pop-to-buffer pmail-buffer)
	       (funcall sortfun reverse))
      (select-window selwin))))

(defun pmail-summary-get-sender (n)
  "Return the sender for message N.
If sender matches `pmail-user-mail-address-regexp' or
`user-mail-address', return the to-address instead."
  (let ((sender (pmail-desc-get-sender n)))
    (if (or (null sender)
	    (and pmail-user-mail-address-regexp
		 (string-match pmail-user-mail-address-regexp sender)))
	;; Either no sender known, or it's this user.
	(save-restriction
	  (narrow-to-region (pmail-desc-get-start n)
			    (pmail-desc-get-end n))
	  (concat "to: " (mail-strip-quoted-names
			  (pmail-header-get-header "to"))))
      sender)))

(defun pmail-summary-get-line-count (n)
  "Return a string containing the number of lines in message N.
If `pmail-summary-line-count-flag' is nil, return the empty string."
  (if pmail-summary-line-count-flag
      (let ((lines (pmail-desc-get-line-count n)))
	(format (cond ((<= lines     9) "   [%d]")
		      ((<= lines    99) "  [%d]")
		      ((<= lines   999) " [%3d]")
		      (t "[%d]"))
		lines))
    ""))

(defun pmail-summary-get-summary-attributes (n)
  "Return the attribute character codes for message N.
`-' means an unseen message, `D' means marked for deletion."
  (format "%s%s%s%s%s"
          (cond ((pmail-desc-attr-p pmail-desc-unseen-index n) "-")
                ((pmail-desc-attr-p pmail-desc-deleted-index n) "D")
                (t " "))
          (or (pmail-desc-get-attr-code pmail-desc-answered-index n) " ")
          (or (pmail-desc-get-attr-code pmail-desc-filed-index n) " ")
          (or (pmail-desc-get-attr-code pmail-desc-edited-index n) " ")
          (or (pmail-desc-get-attr-code pmail-desc-stored-index n) " ")))

(defun pmail-summary-get-summary-line (n)
  "Return a summary line for message N."
  (let (keywords str subj)
    (dolist (keyword (pmail-desc-get-keywords n))
      (when (and (pmail-keyword-p keyword)
		 (not (pmail-attribute-p keyword)))
	(setq keywords (cons keyword keywords))))
    (setq keywords (nreverse keywords)
	  str (if keywords
		  (concat "{ " (mapconcat 'identity keywords " ") " } ")
		"")
	  subj (replace-regexp-in-string "\\s-+" " "
					 (pmail-desc-get-subject n)))
    (funcall pmail-summary-line-decoder
	     (format "%5s%s%6s %25.25s%s %s\n"
		     n
		     (pmail-summary-get-summary-attributes n)
		     (concat (pmail-desc-get-day-number n) "-"
			     (pmail-desc-get-month n))
		     (pmail-summary-get-sender n)
		     (pmail-summary-get-line-count n)
		     (concat str subj)))))

(defun pmail-summary-update (n)
  "Rewrite the summary line for message N."
  (with-current-buffer pmail-buffer
    ;; we need to do this in the pmail-buffer lest the keywords are
    ;; not recognized
    (let ((summary (pmail-summary-get-summary-line n)))
      (with-current-buffer pmail-summary-buffer
	(save-excursion
	  (let ((buffer-read-only nil))
	    (pmail-summary-goto-msg n)
	    ;; summary line includes newline at the end
	    (delete-region (point) (1+ (line-end-position)))
	    (insert summary)))))))

(provide 'pmailsum)

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 80b0a27a-a50d-4f37-9466-83d32d1e0ca8
;;; pmailsum.el ends here
