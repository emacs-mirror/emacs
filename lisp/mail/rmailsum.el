;;; rmailsum.el --- make summary buffers for the mail reader

;; Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.

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

;;; Commentary:

;; Extended by Bob Weiner of Motorola
;;   Provided all commands from rmail-mode in rmail-summary-mode and made key
;;   bindings in both modes wholly compatible.

;;; Code:

(defvar rmail-summary-font-lock-keywords
  '(("^....D.*$" . font-lock-string-face)			; Deleted.
    ("^....-.*$" . font-lock-type-face)				; Unread.
    ;; Neither of the below will be highlighted if either of the above are:
    ("^....[^D-] \\(......\\)" 1 font-lock-keyword-face)	; Date.
    ("{ \\([^}]+\\),}" 1 font-lock-comment-face))		; Labels.
  "Additional expressions to highlight in Rmail Summary mode.")

;; Entry points for making a summary buffer.

;; Regenerate the contents of the summary
;; using the same selection criterion as last time.
;; M-x revert-buffer in a summary buffer calls this function.
(defun rmail-update-summary (&rest ignore)
  (apply (car rmail-summary-redo) (cdr rmail-summary-redo)))

(defun rmail-summary ()
  "Display a summary of all messages, one line per message."
  (interactive)
  (rmail-new-summary "All" '(rmail-summary) nil))

(defun rmail-summary-by-labels (labels)
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  (interactive "sLabels to summarize by: ")
  (if (string= labels "")
      (setq labels (or rmail-last-multi-labels
		       (error "No label specified"))))
  (setq rmail-last-multi-labels labels)
  (rmail-new-summary (concat "labels " labels)
		     (list 'rmail-summary-by-labels labels)
		     'rmail-message-labels-p
		     (concat ", \\(" (mail-comma-list-regexp labels) "\\),")))

(defun rmail-summary-by-recipients (recipients &optional primary-only)
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas."
  (interactive "sRecipients to summarize by: \nP")
  (rmail-new-summary
   (concat "recipients " recipients)
   (list 'rmail-summary-by-recipients recipients primary-only)
   'rmail-message-recipients-p
   (mail-comma-list-regexp recipients) primary-only))

(defun rmail-summary-by-regexp (regexp)
  "Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Emacs will list the header line in the RMAIL-summary."
  (interactive "sRegexp to summarize by: ")
  (if (string= regexp "")
      (setq regexp (or rmail-last-regexp
			 (error "No regexp specified."))))
  (setq rmail-last-regexp regexp)
  (rmail-new-summary (concat "regexp " regexp)
		     (list 'rmail-summary-by-regexp regexp)
		     'rmail-message-regexp-p
                     regexp))

;; rmail-summary-by-topic
;; 1989 R.A. Schnitzler

(defun rmail-summary-by-topic (subject &optional whole-message)
  "Display a summary of all messages with the given SUBJECT.
Normally checks the Subject field of headers;
but if WHOLE-MESSAGE is non-nil (prefix arg given), 
 look in the whole message.
SUBJECT is a string of regexps separated by commas."
  (interactive "sTopics to summarize by: \nP")
  (rmail-new-summary
   (concat "about " subject)
   (list 'rmail-summary-by-topic subject whole-message)
   'rmail-message-subject-p
   (mail-comma-list-regexp subject) whole-message))

(defun rmail-message-subject-p (msg subject &optional whole-message)
  (save-restriction
    (goto-char (rmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region
     (point)
     (progn (search-forward (if whole-message "\^_" "\n\n")) (point)))
    (goto-char (point-min))
    (if whole-message (re-search-forward subject nil t)
      (string-match subject (or (mail-fetch-field "Subject") "")) )))

(defun rmail-summary-by-senders (senders)
  "Display a summary of all messages with the given SENDERS.
SENDERS is a string of names separated by commas."
  (interactive "sSenders to summarize by: ")
  (rmail-new-summary
   (concat "senders " senders)
   (list 'rmail-summary-by-senders senders)
   'rmail-message-senders-p
   (mail-comma-list-regexp senders)))

(defun rmail-message-senders-p (msg senders)
  (save-restriction
    (goto-char (rmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
    (string-match senders (or (mail-fetch-field "From") ""))))

;; General making of a summary buffer.

(defvar rmail-summary-symbol-number 0)

(defun rmail-new-summary (description redo-form function &rest args)
  "Create a summary of selected messages.
DESCRIPTION makes part of the mode line of the summary buffer.
For each message, FUNCTION is applied to the message number and ARGS...
and if the result is non-nil, that message is included.
nil for FUNCTION means all messages."
  (message "Computing summary lines...")
  (let (sumbuf mesg was-in-summary)
    (save-excursion
      ;; Go to the Rmail buffer.
      (if (eq major-mode 'rmail-summary-mode)
	  (progn
	    (setq was-in-summary t)
	    (set-buffer rmail-buffer)))
      ;; Find its summary buffer, or make one.
      (setq sumbuf
	    (if (and rmail-summary-buffer
		     (buffer-name rmail-summary-buffer))
		rmail-summary-buffer
	      (generate-new-buffer (concat (buffer-name) "-summary"))))
      (setq mesg rmail-current-message)
      ;; Filter the messages; make or get their summary lines.
      (let ((summary-msgs ())
	    (new-summary-line-count 0))
	(let ((msgnum 1)
	      (buffer-read-only nil)
	      (old-min (point-min-marker))
	      (old-max (point-max-marker)))
	  ;; Can't use save-restriction here; that doesn't work if we
	  ;; plan to modify text outside the original restriction.
	  (save-excursion
	    (widen)
	    (goto-char (point-min))
	    (while (>= rmail-total-messages msgnum)
	      (if (or (null function)
		      (apply function (cons msgnum args)))
		  (setq summary-msgs
			(cons (cons msgnum (rmail-make-summary-line msgnum))
			      summary-msgs)))
	      (setq msgnum (1+ msgnum)))
	    (setq summary-msgs (nreverse summary-msgs)))
	  (narrow-to-region old-min old-max))
	;; Temporarily, while summary buffer is unfinished,
	;; we "don't have" a summary.
	(setq rmail-summary-buffer nil)
	(save-excursion
	  (let ((rbuf (current-buffer))
		(total rmail-total-messages))
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
	    (rmail-summary-mode)
	    (make-local-variable 'minor-mode-alist)
	    (setq minor-mode-alist (list '(t (concat ": " description))))
	    (setq rmail-buffer rbuf
		  rmail-summary-redo redo-form
		  rmail-total-messages total))))
      (setq rmail-summary-buffer sumbuf))
    ;; Now display the summary buffer and go to the right place in it.
    (or was-in-summary
	(if (and (one-window-p)
		 pop-up-windows (not pop-up-frames))
	    ;; If there is just one window, put the summary on the top.
	    (progn
	      (split-window)
	      (select-window (next-window (frame-first-window)))
	      (pop-to-buffer sumbuf)
	      ;; If pop-to-buffer did not use that window, delete that
	      ;; window.  (This can happen if it uses another frame.)
	      (if (not (eq sumbuf (window-buffer (frame-first-window))))
		  (delete-other-windows)))
	  (pop-to-buffer sumbuf)))
    (rmail-summary-goto-msg mesg t t)
    (message "Computing summary lines...done")))

;; Low levels of generating a summary.

(defun rmail-make-summary-line (msg)
  (let ((line (or (aref rmail-summary-vector (1- msg))
		  (progn
		    (setq new-summary-line-count
			  (1+ new-summary-line-count))
		    (if (zerop (% new-summary-line-count 10))
			(message "Computing summary lines...%d"
				 new-summary-line-count))
		    (rmail-make-summary-line-1 msg)))))
    ;; Fix up the part of the summary that says "deleted" or "unseen".
    (aset line 4
	  (if (rmail-message-deleted-p msg) ?\D
	    (if (= ?0 (char-after (+ 3 (rmail-msgbeg msg))))
		?\- ?\ )))
    line))

(defun rmail-make-summary-line-1 (msg)
  (goto-char (rmail-msgbeg msg))
  (let* ((lim (save-excursion (forward-line 2) (point)))
	 pos
	 (labels
	  (progn
	    (forward-char 3)
	    (concat
;	     (if (save-excursion (re-search-forward ",answered," lim t))
;		 "*" "")
;	     (if (save-excursion (re-search-forward ",filed," lim t))
;		 "!" "")
	     (if (progn (search-forward ",,") (eolp))
		 ""
	       (concat "{"
		       (buffer-substring (point)
					 (progn (end-of-line) (point)))
		       "} ")))))
	 (line
	  (progn
	    (forward-line 1)
	    (if (looking-at "Summary-line: ")
		(progn
		  (goto-char (match-end 0))
		  (setq line
			(buffer-substring (point)
					  (progn (forward-line 1) (point)))))))))
    ;; Obsolete status lines lacking a # should be flushed.
    (and line
	 (not (string-match "#" line))
	 (progn
	   (delete-region (point)
			  (progn (forward-line -1) (point)))
	   (setq line nil)))
    ;; If we didn't get a valid status line from the message,
    ;; make a new one and put it in the message.
    (or line
	(let* ((case-fold-search t)
	       (next (rmail-msgend msg))
	       (beg (if (progn (goto-char (rmail-msgbeg msg))
			       (search-forward "\n*** EOOH ***\n" next t))
			(point)
		      (forward-line 1)
		      (point)))
	       (end (progn (search-forward "\n\n" nil t) (point))))
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char beg)
	    (setq line (rmail-make-basic-summary-line)))
	  (goto-char (rmail-msgbeg msg))
	  (forward-line 2)
	  (insert "Summary-line: " line)))
    (setq pos (string-match "#" line))
    (aset rmail-summary-vector (1- msg)
	  (concat (format "%4d  " msg)
		  (substring line 0 pos)
		  labels
		  (substring line (1+ pos))))))

(defun rmail-make-basic-summary-line ()
  (goto-char (point-min))
  (concat (save-excursion
	    (if (not (re-search-forward "^Date:" nil t))
		"      "
	      (cond ((re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
		      (save-excursion (end-of-line) (point)) t)
		     (format "%2d-%3s"
			     (string-to-int (buffer-substring
					     (match-beginning 2)
					     (match-end 2)))
			     (buffer-substring
			      (match-beginning 4) (match-end 4))))
		    ((re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
		      (save-excursion (end-of-line) (point)) t)
		     (format "%2d-%3s"
			     (string-to-int (buffer-substring
					     (match-beginning 4)
					     (match-end 4)))
			     (buffer-substring
			      (match-beginning 2) (match-end 2))))
		    (t "??????"))))
	  "  "
	  (save-excursion
	    (if (not (re-search-forward "^From:[ \t]*" nil t))
		"                         "
	      (let* ((from (mail-strip-quoted-names
			    (buffer-substring
			     (1- (point))
			     ;; Get all the lines of the From field
			     ;; so that we get a whole comment if there is one,
			     ;; so that mail-strip-quoted-names can discard it.
			     (let ((opoint (point)))
			       (while (progn (forward-line 1)
					     (looking-at "[ \t]")))
			       ;; Back up over newline, then trailing spaces or tabs
			       (forward-char -1)
			       (skip-chars-backward " \t")
			       (point)))))
                     len mch lo)
		(if (string-match (concat "^"
					  (regexp-quote (user-login-name))
					  "\\($\\|@\\)")
				  from)
		    (save-excursion
		      (goto-char (point-min))
		      (if (not (re-search-forward "^To:[ \t]*" nil t))
			  nil
			(setq from
			      (concat "to: "
				      (mail-strip-quoted-names
				       (buffer-substring
					(point)
					(progn (end-of-line)
					       (skip-chars-backward " \t")
					       (point)))))))))
		(setq len (length from))
		(setq mch (string-match "[@%]" from))
		(format "%25s"
			(if (or (not mch) (<= len 25))
			    (substring from (max 0 (- len 25)))
			  (substring from
				     (setq lo (cond ((< (- mch 14) 0) 0)
						    ((< len (+ mch 11))
						     (- len 25))
						    (t (- mch 14))))
				     (min len (+ lo 25))))))))
	  "  #"
	  (if (re-search-forward "^Subject:" nil t)
	      (progn (skip-chars-forward " \t")
		     (buffer-substring (point)
				       (progn (end-of-line)
					      (point))))
	    (re-search-forward "[\n][\n]+" nil t)
	    (buffer-substring (point) (progn (end-of-line) (point))))
	  "\n"))

;; Simple motion in a summary buffer.

(defun rmail-summary-next-all (&optional number)
  (interactive "p")
  (forward-line (if number number 1))
  ;; It doesn't look nice to move forward past the last message line.
  (and (eobp) (> number 0)
       (forward-line -1))
  (display-buffer rmail-buffer))

(defun rmail-summary-previous-all (&optional number)
  (interactive "p")
  (forward-line (- (if number number 1)))
  ;; It doesn't look nice to move forward past the last message line.
  (and (eobp) (< number 0)
       (forward-line -1))
  (display-buffer rmail-buffer))

(defun rmail-summary-next-msg (&optional number)
  "Display next non-deleted msg from rmail file.
With optional prefix argument NUMBER, moves forward this number of non-deleted
messages, or backward if NUMBER is negative."
  (interactive "p")
  (forward-line 0)
  (and (> number 0) (end-of-line))
  (let ((count (if (< number 0) (- number) number))
	(search (if (> number 0) 're-search-forward 're-search-backward))
	(non-del-msg-found nil))
    (while (and (> count 0) (setq non-del-msg-found
				  (or (funcall search "^....[^D]" nil t)
				      non-del-msg-found)))
      (setq count (1- count))))
  (beginning-of-line)
  (display-buffer rmail-buffer))

(defun rmail-summary-previous-msg (&optional number)
  (interactive "p")
  (rmail-summary-next-msg (- (if number number 1))))

(defun rmail-summary-next-labeled-message (n labels)
  "Show next message with LABEL.  Defaults to last labels used.
With prefix argument N moves forward N messages with these labels."
  (interactive "p\nsMove to next msg with labels: ")
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-next-labeled-message n labels)))

(defun rmail-summary-previous-labeled-message (n labels)
  "Show previous message with LABEL.  Defaults to last labels used.
With prefix argument N moves backward N messages with these labels."
  (interactive "p\nsMove to previous msg with labels: ")
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-previous-labeled-message n labels)))

;; Delete and undelete summary commands.

(defun rmail-summary-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward."
  (interactive "P")
  (let (end)
    (rmail-summary-goto-msg)
    (pop-to-buffer rmail-buffer)
    (rmail-delete-message)
    (let ((del-msg rmail-current-message))
      (pop-to-buffer rmail-summary-buffer)
      (rmail-summary-mark-deleted del-msg)
      (while (and (not (if backward (bobp) (eobp)))
		  (save-excursion (beginning-of-line)
				  (looking-at " *[0-9]+D")))
	(forward-line (if backward -1 1)))
      ;; It looks ugly to move to the empty line at end of buffer.
      (and (eobp) (not backward)
	   (forward-line -1)))))

(defun rmail-summary-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-summary-delete-forward t))

(defun rmail-summary-mark-deleted (&optional n undel)
  (and n (rmail-summary-goto-msg n t t))
  (or (eobp)
      (not (overlay-get rmail-summary-overlay 'face))
      (let ((buffer-read-only nil))
	(skip-chars-forward " ")
	(skip-chars-forward "[0-9]")
	(if undel
	    (if (looking-at "D")
		(progn (delete-char 1) (insert " ")))
	  (delete-char 1)
	  (insert "D"))))
  (beginning-of-line))

(defun rmail-summary-mark-undeleted (n)
  (rmail-summary-mark-deleted n t))

(defun rmail-summary-deleted-p (&optional n)
  (save-excursion
    (and n (rmail-summary-goto-msg n nil t))
    (skip-chars-forward " ")
    (skip-chars-forward "[0-9]")
    (looking-at "D")))

(defun rmail-summary-undelete (&optional arg)
  "Undelete current message.
Optional prefix ARG means undelete ARG previous messages."
  (interactive "p")
  (if (/= arg 1)
      (rmail-summary-undelete-many arg)
    (let ((buffer-read-only nil)
	  (opoint (point)))
      (end-of-line)
      (cond ((re-search-backward "\\(^ *[0-9]*\\)\\(D\\)" nil t)
	     (replace-match "\\1 ")
	     (rmail-summary-goto-msg)
	     (pop-to-buffer rmail-buffer)
	     (and (rmail-message-deleted-p rmail-current-message)
		  (rmail-undelete-previous-message))
	     (pop-to-buffer rmail-summary-buffer))
	    (t (goto-char opoint))))))

(defun rmail-summary-undelete-many (&optional n)
  "Undelete all deleted msgs, optional prefix arg N means undelete N prev msgs."
  (interactive "P")
  (save-excursion
    (set-buffer rmail-buffer)
    (let* ((init-msg (if n rmail-current-message rmail-total-messages))
	   (rmail-current-message init-msg)
	   (n (or n rmail-total-messages))
	   (msgs-undeled 0))
      (while (and (> rmail-current-message 0)
		  (< msgs-undeled n))
	(if (rmail-message-deleted-p rmail-current-message)
	    (progn (rmail-set-attribute "deleted" nil)
		   (setq msgs-undeled (1+ msgs-undeled))))
	(setq rmail-current-message (1- rmail-current-message)))
      (set-buffer rmail-summary-buffer)
      (setq rmail-current-message init-msg msgs-undeled 0)
      (while (and (> rmail-current-message 0)
		  (< msgs-undeled n))
	(if (rmail-summary-deleted-p rmail-current-message)
	    (progn (rmail-summary-mark-undeleted rmail-current-message)
		   (setq msgs-undeled (1+ msgs-undeled))))
	(setq rmail-current-message (1- rmail-current-message))))
    (rmail-summary-goto-msg)))

;; Rmail Summary mode is suitable only for specially formatted data.
(put 'rmail-summary-mode 'mode-class 'special)

(defun rmail-summary-mode ()
  "Rmail Summary Mode is invoked from Rmail Mode by using \\<rmail-mode-map>\\[rmail-summary].
As commands are issued in the summary buffer, they are applied to the
corresponding mail messages in the rmail buffer.

All normal editing commands are turned off.
Instead, nearly all the Rmail mode commands are available,
though many of them move only among the messages in the summary.

These additional commands exist:

\\[rmail-summary-undelete-many]	Undelete all or prefix arg deleted messages.
\\[rmail-summary-wipe] Delete the summary and go to the Rmail buffer.

Commands for sorting the summary:

\\[rmail-summary-sort-by-date] Sort by date.
\\[rmail-summary-sort-by-subject] Sort by subject.
\\[rmail-summary-sort-by-author] Sort by author.
\\[rmail-summary-sort-by-recipient] Sort by recipient.
\\[rmail-summary-sort-by-correspondent] Sort by correspondent.
\\[rmail-summary-sort-by-lines] Sort by lines.
\\[rmail-summary-sort-by-keywords] Sort by keywords."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rmail-summary-mode)
  (setq mode-name "RMAIL Summary")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'rmail-buffer)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'rmail-current-message)
  (setq rmail-current-message nil)
  (make-local-variable 'rmail-summary-redo)
  (setq rmail-summary-redo nil)
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'post-command-hook)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rmail-summary-font-lock-keywords t))
  (rmail-summary-enable)
  (run-hooks 'rmail-summary-mode-hook))

;; Summary features need to be disabled during edit mode.
(defun rmail-summary-disable ()
  (use-local-map text-mode-map)
  (remove-hook 'post-command-hook 'rmail-summary-rmail-update)
  (setq revert-buffer-function nil))

(defun rmail-summary-enable ()
  (use-local-map rmail-summary-mode-map)
  (add-hook 'post-command-hook 'rmail-summary-rmail-update)
  (setq revert-buffer-function 'rmail-update-summary))

;; Show in Rmail the message described by the summary line that point is on,
;; but only if the Rmail buffer is already visible.
;; This is a post-command-hook in summary buffers.
(defun rmail-summary-rmail-update ()
  (let (buffer-read-only)
    (save-excursion
      ;; If at end of buffer, pretend we are on the last text line.
      (if (eobp)
	  (forward-line -1))
      (beginning-of-line)
      (skip-chars-forward " ")
      (let ((msg-num (string-to-int (buffer-substring
				     (point)
				     (progn (skip-chars-forward "0-9")
					    (point))))))
	(or (eq rmail-current-message msg-num)
	    (let ((window (get-buffer-window rmail-buffer))
		  (owin (selected-window)))
	      (setq rmail-current-message msg-num)
	      (if (= (following-char) ?-)
		  (progn
		    (delete-char 1)
		    (insert " ")))
	      (if window
		  ;; Using save-window-excursion would cause the new value
		  ;; of point to get lost.
		  (unwind-protect
		      (progn
			(select-window window)
			(rmail-show-message msg-num t))
		    (select-window owin))
		(if (buffer-name rmail-buffer)
		    (save-excursion
		      (set-buffer rmail-buffer)
		      (rmail-show-message msg-num t))))))
	(rmail-summary-update-highlight nil)))))

(defvar rmail-summary-mode-map nil)

(if rmail-summary-mode-map
    nil
  (setq rmail-summary-mode-map (make-keymap))
  (suppress-keymap rmail-summary-mode-map)
  (define-key rmail-summary-mode-map "a"      'rmail-summary-add-label)
  (define-key rmail-summary-mode-map "c"      'rmail-summary-continue)
  (define-key rmail-summary-mode-map "d"      'rmail-summary-delete-forward)
  (define-key rmail-summary-mode-map "\C-d"   'rmail-summary-delete-backward)
  (define-key rmail-summary-mode-map "e"      'rmail-summary-edit-current-message)
  (define-key rmail-summary-mode-map "f"      'rmail-summary-forward)
  (define-key rmail-summary-mode-map "g"      'rmail-summary-get-new-mail)
  (define-key rmail-summary-mode-map "h"      'rmail-summary)
  (define-key rmail-summary-mode-map "i"      'rmail-summary-input)
  (define-key rmail-summary-mode-map "j"      'rmail-summary-goto-msg)
  (define-key rmail-summary-mode-map "k"      'rmail-summary-kill-label)
  (define-key rmail-summary-mode-map "l"      'rmail-summary-by-labels)
  (define-key rmail-summary-mode-map "\e\C-h" 'rmail-summary)
  (define-key rmail-summary-mode-map "\e\C-l" 'rmail-summary-by-labels)
  (define-key rmail-summary-mode-map "\e\C-r" 'rmail-summary-by-recipients)
  (define-key rmail-summary-mode-map "\e\C-s" 'rmail-summary-by-regexp)
  (define-key rmail-summary-mode-map "\e\C-t" 'rmail-summary-by-topic)
  (define-key rmail-summary-mode-map "m"      'rmail-summary-mail)
  (define-key rmail-summary-mode-map "\M-m"   'rmail-summary-retry-failure)
  (define-key rmail-summary-mode-map "n"      'rmail-summary-next-msg)
  (define-key rmail-summary-mode-map "\en"    'rmail-summary-next-all)
  (define-key rmail-summary-mode-map "\e\C-n" 'rmail-summary-next-labeled-message)
  (define-key rmail-summary-mode-map "o"      'rmail-summary-output-to-rmail-file)
  (define-key rmail-summary-mode-map "\C-o"   'rmail-summary-output)
  (define-key rmail-summary-mode-map "p"      'rmail-summary-previous-msg)
  (define-key rmail-summary-mode-map "\ep"    'rmail-summary-previous-all)
  (define-key rmail-summary-mode-map "\e\C-p" 'rmail-summary-previous-labeled-message)
  (define-key rmail-summary-mode-map "q"      'rmail-summary-quit)
  (define-key rmail-summary-mode-map "r"      'rmail-summary-reply)
  (define-key rmail-summary-mode-map "s"      'rmail-summary-expunge-and-save)
  (define-key rmail-summary-mode-map "\es"    'rmail-summary-search)
  (define-key rmail-summary-mode-map "t"      'rmail-summary-toggle-header)
  (define-key rmail-summary-mode-map "u"      'rmail-summary-undelete)
  (define-key rmail-summary-mode-map "\M-u"   'rmail-summary-undelete-many)
  (define-key rmail-summary-mode-map "w"      'rmail-summary-wipe)
  (define-key rmail-summary-mode-map "x"      'rmail-summary-expunge)
  (define-key rmail-summary-mode-map "."      'rmail-summary-beginning-of-message)
  (define-key rmail-summary-mode-map "<"      'rmail-summary-first-message)
  (define-key rmail-summary-mode-map ">"      'rmail-summary-last-message)
  (define-key rmail-summary-mode-map " "      'rmail-summary-scroll-msg-up)
  (define-key rmail-summary-mode-map "\177"   'rmail-summary-scroll-msg-down)
  (define-key rmail-summary-mode-map "?"      'describe-mode)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-d"
    'rmail-summary-sort-by-date)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-s"
    'rmail-summary-sort-by-subject)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-a"
    'rmail-summary-sort-by-author)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-r"
    'rmail-summary-sort-by-recipient)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-c"
    'rmail-summary-sort-by-correspondent)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-l"
    'rmail-summary-sort-by-lines)
  (define-key rmail-summary-mode-map "\C-c\C-s\C-k"
    'rmail-summary-sort-by-keywords)
  )

;;; Menu bar bindings.

(define-key rmail-summary-mode-map [menu-bar] (make-sparse-keymap))

(define-key rmail-summary-mode-map [menu-bar classify]
  (cons "Classify" (make-sparse-keymap "Classify")))

(define-key rmail-summary-mode-map [menu-bar classify output-menu]
  '("Output (Rmail Menu)..." . rmail-summary-output-menu))

(define-key rmail-summary-mode-map [menu-bar classify input-menu]
  '("Input Rmail file (menu)..." . rmail-input-menu))

(define-key rmail-summary-mode-map [menu-bar classify input-menu]
  '(nil))

(define-key rmail-summary-mode-map [menu-bar classify output-menu]
  '(nil))

(define-key rmail-summary-mode-map [menu-bar classify output-inbox]
  '("Output (inbox)..." . rmail-summary-output))

(define-key rmail-summary-mode-map [menu-bar classify output]
  '("Output (Rmail)..." . rmail-summary-output-to-rmail-file))

(define-key rmail-summary-mode-map [menu-bar classify kill-label]
  '("Kill Label..." . rmail-summary-kill-label))

(define-key rmail-summary-mode-map [menu-bar classify add-label]
  '("Add Label..." . rmail-summary-add-label))

(define-key rmail-summary-mode-map [menu-bar summary]
  (cons "Summary" (make-sparse-keymap "Summary")))

(define-key rmail-summary-mode-map [menu-bar summary labels]
  '("By Labels..." . rmail-summary-by-labels))

(define-key rmail-summary-mode-map [menu-bar summary recipients]
  '("By Recipients..." . rmail-summary-by-recipients))

(define-key rmail-summary-mode-map [menu-bar summary topic]
  '("By Topic..." . rmail-summary-by-topic))

(define-key rmail-summary-mode-map [menu-bar summary regexp]
  '("By Regexp..." . rmail-summary-by-regexp))

(define-key rmail-summary-mode-map [menu-bar summary all]
  '("All" . rmail-summary))

(define-key rmail-summary-mode-map [menu-bar mail]
  (cons "Mail" (make-sparse-keymap "Mail")))

(define-key rmail-summary-mode-map [menu-bar mail rmail-summary-get-new-mail]
  '("Get New Mail" . rmail-summary-get-new-mail))

(define-key rmail-summary-mode-map [menu-bar mail lambda]
  '("----"))

(define-key rmail-summary-mode-map [menu-bar mail continue]
  '("Continue" . rmail-summary-continue))

(define-key rmail-summary-mode-map [menu-bar mail resend]
  '("Re-send..." . rmail-summary-resend))

(define-key rmail-summary-mode-map [menu-bar mail forward]
  '("Forward" . rmail-summary-forward))

(define-key rmail-summary-mode-map [menu-bar mail retry]
  '("Retry" . rmail-summary-retry-failure))

(define-key rmail-summary-mode-map [menu-bar mail reply]
  '("Reply" . rmail-summary-reply))

(define-key rmail-summary-mode-map [menu-bar mail mail]
  '("Mail" . rmail-summary-mail))

(define-key rmail-summary-mode-map [menu-bar delete]
  (cons "Delete" (make-sparse-keymap "Delete")))

(define-key rmail-summary-mode-map [menu-bar delete expunge/save]
  '("Expunge/Save" . rmail-summary-expunge-and-save))

(define-key rmail-summary-mode-map [menu-bar delete expunge]
  '("Expunge" . rmail-summary-expunge))

(define-key rmail-summary-mode-map [menu-bar delete undelete]
  '("Undelete" . rmail-summary-undelete))

(define-key rmail-summary-mode-map [menu-bar delete delete]
  '("Delete" . rmail-summary-delete-forward))

(define-key rmail-summary-mode-map [menu-bar move]
  (cons "Move" (make-sparse-keymap "Move")))

(define-key rmail-summary-mode-map [menu-bar move search-back]
  '("Search Back..." . rmail-summary-search-backward))

(define-key rmail-summary-mode-map [menu-bar move search]
  '("Search..." . rmail-summary-search))

(define-key rmail-summary-mode-map [menu-bar move previous]
  '("Previous Nondeleted" . rmail-summary-previous-msg))

(define-key rmail-summary-mode-map [menu-bar move next]
  '("Next Nondeleted" . rmail-summary-next-msg))

(define-key rmail-summary-mode-map [menu-bar move last]
  '("Last" . rmail-summary-last-message))

(define-key rmail-summary-mode-map [menu-bar move first]
  '("First" . rmail-summary-first-message))

(define-key rmail-summary-mode-map [menu-bar move previous]
  '("Previous" . rmail-summary-previous-all))

(define-key rmail-summary-mode-map [menu-bar move next]
  '("Next" . rmail-summary-next-all))

(defvar rmail-summary-overlay nil)

(defun rmail-summary-goto-msg (&optional n nowarn skip-rmail)
  (interactive "P")
  (if (consp n) (setq n (prefix-numeric-value n)))
  (if (eobp) (forward-line -1))
  (beginning-of-line)
  (let* ((obuf (current-buffer))
	 (buf rmail-buffer)
	 (cur (point))
	 message-not-found
	 (curmsg (string-to-int
		  (buffer-substring (point)
				    (min (point-max) (+ 5 (point))))))
	 (total (save-excursion (set-buffer buf) rmail-total-messages)))
    ;; If message number N was specified, find that message's line
    ;; or set message-not-found.
    ;; If N wasn't specified or that message can't be found.
    ;; set N by default.
    (if (not n)
	(setq n curmsg)
      (if (< n 1)
	  (progn (message "No preceding message")
		 (setq n 1)))
      (if (> n total)
	  (progn (message "No following message")
		 (goto-char (point-max))
		 (rmail-summary-goto-msg)))
      (goto-char (point-min))
      (if (not (re-search-forward (format "^%4d[^0-9]" n) nil t))
	  (progn (or nowarn (message "Message %d not found" n))
		 (setq n curmsg)
		 (setq message-not-found t)
		 (goto-char cur))))
    (beginning-of-line)
    (skip-chars-forward " ")
    (skip-chars-forward "0-9")
    (save-excursion (if (= (following-char) ?-)
			(let ((buffer-read-only nil))
			  (delete-char 1)
			  (insert " "))))
    (rmail-summary-update-highlight message-not-found)
    (beginning-of-line)
    (if skip-rmail
	nil
      (let ((selwin (selected-window)))
	(unwind-protect
	    (progn (pop-to-buffer buf)
		   (rmail-show-message n))
	  (select-window selwin)
	  ;; The actions above can alter the current buffer.  Preserve it.
	  (set-buffer obuf))))))

;; Update the highlighted line in an rmail summary buffer.
;; That should be current.  We highlight the line point is on.
;; If NOT-FOUND is non-nil, we turn off highlighting.
(defun rmail-summary-update-highlight (not-found)
  ;; Make sure we have an overlay to use.
  (or rmail-summary-overlay
      (progn
	(make-local-variable 'rmail-summary-overlay)
	(setq rmail-summary-overlay (make-overlay (point) (point)))))
  ;; If this message is in the summary, use the overlay to highlight it.
  ;; Otherwise, don't highlight anything.
  (if not-found
      (overlay-put rmail-summary-overlay 'face nil)
    (move-overlay rmail-summary-overlay
		  (save-excursion (beginning-of-line)
				  (skip-chars-forward " ")
				  (point))
		  (save-excursion (end-of-line) (point)))
    (overlay-put rmail-summary-overlay 'face 'highlight)))

(defun rmail-summary-scroll-msg-up (&optional dist)
  "Scroll the Rmail window forward.
If the Rmail window is displaying the end of a message,
advance to the next message."
  (interactive "P")
  (if (eq dist '-)
      (rmail-summary-scroll-msg-down nil)
    (let ((rmail-buffer-window (get-buffer-window rmail-buffer)))
      (if rmail-buffer-window
	  (if (let ((rmail-summary-window (selected-window)))
		(select-window rmail-buffer-window)
		(prog1
		    ;; Is EOB visible in the buffer?
		    (save-excursion
		      (let ((ht (window-height (selected-window))))
			(move-to-window-line (- ht 2))
			(end-of-line)
			(eobp)))
		  (select-window rmail-summary-window)))
	      (rmail-summary-next-msg (or dist 1))
	    (let ((other-window-scroll-buffer rmail-buffer))
	      (scroll-other-window dist)))
	;; This forces rmail-buffer to be sized correctly later.
	(display-buffer rmail-buffer)
	(setq rmail-current-message nil)))))

(defun rmail-summary-scroll-msg-down (&optional dist)
  "Scroll the Rmail window backward.
If the Rmail window is displaying the beginning of a message,
advance to the previous message."
  (interactive "P")
  (if (eq dist '-)
      (rmail-summary-scroll-msg-up nil)
    (let ((rmail-buffer-window (get-buffer-window rmail-buffer)))
      (if rmail-buffer-window
	  (if (let ((rmail-summary-window (selected-window)))
		(select-window rmail-buffer-window)
		(prog1
		    ;; Is BOB visible in the buffer?
		    (save-excursion
		      (move-to-window-line 0)
		      (beginning-of-line)
		      (bobp))
		  (select-window rmail-summary-window)))
	      (rmail-summary-previous-msg (or dist 1))
	    (let ((other-window-scroll-buffer rmail-buffer))
	      (scroll-other-window-down dist)))
	;; This forces rmail-buffer to be sized correctly later.
	(display-buffer rmail-buffer)
	(setq rmail-current-message nil)))))

(defun rmail-summary-beginning-of-message ()
  "Show current message from the beginning."
  (interactive)
  (pop-to-buffer rmail-buffer)
  (beginning-of-buffer)
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-quit ()
  "Quit out of Rmail and Rmail summary."
  (interactive)
  (rmail-summary-wipe)
  (rmail-quit))

(defun rmail-summary-wipe ()
  "Kill and wipe away Rmail summary, remaining within Rmail."
  (interactive)
  (save-excursion (set-buffer rmail-buffer) (setq rmail-summary-buffer nil))
  (let ((local-rmail-buffer rmail-buffer))
    (kill-buffer (current-buffer))
    ;; Delete window if not only one.
    (if (not (eq (selected-window) (next-window nil 'no-minibuf)))
	(delete-window))
    ;; Switch windows to the rmail buffer, or switch to it in this window.
    (pop-to-buffer local-rmail-buffer)))

(defun rmail-summary-expunge ()
  "Actually erase all deleted messages and recompute summary headers."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-only-expunge))
  (rmail-update-summary))

(defun rmail-summary-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-only-expunge))
  (rmail-update-summary)
  (save-excursion
    (set-buffer rmail-buffer)
    (save-buffer))
  (set-buffer-modified-p nil))

(defun rmail-summary-get-new-mail ()
  "Get new mail and recompute summary headers."
  (interactive)
  (let (msg)
    (save-excursion
      (set-buffer rmail-buffer)
      (rmail-get-new-mail)
      ;; Get the proper new message number.
      (setq msg rmail-current-message))
    ;; Make sure that message is displayed.
    (rmail-summary-goto-msg msg)))

(defun rmail-summary-input (filename)
  "Run Rmail on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  ;; We switch windows here, then display the other Rmail file there.
  (pop-to-buffer rmail-buffer)
  (rmail filename))

(defun rmail-summary-first-message ()
  "Show first message in Rmail file from summary buffer."
  (interactive)
  (beginning-of-buffer))

(defun rmail-summary-last-message ()
  "Show last message in Rmail file from summary buffer."
  (interactive)
  (end-of-buffer)
  (forward-line -1))

(defvar rmail-summary-edit-map nil)
(if rmail-summary-edit-map
    nil
  (setq rmail-summary-edit-map
	(nconc (make-sparse-keymap) text-mode-map))
  (define-key rmail-summary-edit-map "\C-c\C-c" 'rmail-cease-edit)
  (define-key rmail-summary-edit-map "\C-c\C-]" 'rmail-abort-edit))

(defun rmail-summary-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (pop-to-buffer rmail-buffer)
  (rmail-edit-current-message)
  (use-local-map rmail-summary-edit-map))

(defun rmail-summary-cease-edit ()
  "Finish editing message, then go back to Rmail summary buffer."
  (interactive)
  (rmail-cease-edit)
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-abort-edit ()
  "Abort edit of current message; restore original contents.
Go back to summary buffer."
  (interactive)
  (rmail-abort-edit)
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-search-backward (regexp &optional n)
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
  ;; Don't use save-excursion because that prevents point from moving
  ;; properly in the summary buffer.
  (let ((buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer rmail-buffer)
	  (rmail-search regexp (- n)))
      (set-buffer buffer))))

(defun rmail-summary-search (regexp &optional n)
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
  ;; Don't use save-excursion because that prevents point from moving
  ;; properly in the summary buffer.
  (let ((buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer rmail-buffer)
	  (rmail-search regexp n))
      (set-buffer buffer))))

(defun rmail-summary-toggle-header ()
  "Show original message header if pruned header currently shown, or vice versa."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-toggle-header)))

(defun rmail-summary-add-label (label)
  "Add LABEL to labels associated with current Rmail message.
Completion is performed over known labels when reading."
  (interactive (list (save-excursion
		       (set-buffer rmail-buffer)
		       (rmail-read-label "Add label"))))
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-add-label label)))

(defun rmail-summary-kill-label (label)
  "Remove LABEL from labels associated with current Rmail message.
Completion is performed over known labels when reading."
  (interactive (list (save-excursion
		       (set-buffer rmail-buffer)
		       (rmail-read-label "Kill label"))))
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-set-label label nil)))

;;;; *** Rmail Summary Mailing Commands ***

(defun rmail-summary-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (rmail-start-mail nil nil nil nil nil rmail-buffer)
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map)
    "\C-c\C-c" 'rmail-summary-send-and-exit))

(defun rmail-summary-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (rmail-start-mail t))

(defun rmail-summary-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (set-buffer rmail-buffer)
  (rmail-reply just-sender)
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map)
    "\C-c\C-c" 'rmail-summary-send-and-exit))

(defun rmail-summary-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message; otherwise copy the current message."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-retry-failure)
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map)
    "\C-c\C-c" 'rmail-summary-send-and-exit))

(defun rmail-summary-send-and-exit ()
  "Send mail reply and return to summary buffer."
  (interactive)
  (mail-send-and-exit t))

(defun rmail-summary-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
  (save-excursion
    (set-buffer rmail-buffer)
    (rmail-forward resend)
    (use-local-map (copy-keymap (current-local-map)))
    (define-key (current-local-map)
      "\C-c\C-c" 'rmail-summary-send-and-exit)))

(defun rmail-summary-resend ()
  "Resend current message using 'rmail-resend'."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (call-interactively 'rmail-resend)))

;; Summary output commands.

(defun rmail-summary-output-to-rmail-file (&optional file-name)
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (let ((rmail-delete-after-output nil))
      (if file-name
	  (rmail-output-to-rmail-file file-name)
	(call-interactively 'rmail-output-to-rmail-file))))
  (if rmail-delete-after-output
      (rmail-summary-delete-forward nil)))

(defun rmail-summary-output-menu ()
  "Output current message to another Rmail file, chosen with a menu.
Also set the default for subsequent \\[rmail-output-to-rmail-file] commands.
The variables `rmail-secondary-file-directory' and
`rmail-secondary-file-regexp' control which files are offered in the menu."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (let ((rmail-delete-after-output nil))
      (call-interactively 'rmail-output-menu)))
  (if rmail-delete-after-output
      (rmail-summary-delete-forward nil)))

(defun rmail-summary-output ()
  "Append this message to Unix mail file named FILE-NAME."
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (let ((rmail-delete-after-output nil))
      (call-interactively 'rmail-output)))
  (if rmail-delete-after-output
      (rmail-summary-delete-forward nil)))

(defun rmail-summary-construct-io-menu ()
  (let ((files (rmail-find-all-files rmail-secondary-file-directory)))
    (if (listp files)
	(progn
	  (define-key rmail-summary-mode-map [menu-bar classify input-menu]
	    (cons "Input Rmail File" 
		  (rmail-list-to-menu "Input Rmail File" 
				      (cdr files) 
				      'rmail-summary-input)))
	  (define-key rmail-summary-mode-map [menu-bar classify output-menu]
	    (cons "Output Rmail File" 
		  (rmail-list-to-menu "Output Rmail File" 
				      (cdr files) 
				      'rmail-summary-output-to-rmail-file)))))))


;; Sorting messages in Rmail Summary buffer.

(defun rmail-summary-sort-by-date (reverse)
  "Sort messages of current Rmail summary by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-date) reverse))

(defun rmail-summary-sort-by-subject (reverse)
  "Sort messages of current Rmail summary by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-subject) reverse))

(defun rmail-summary-sort-by-author (reverse)
  "Sort messages of current Rmail summary by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-author) reverse))

(defun rmail-summary-sort-by-recipient (reverse)
  "Sort messages of current Rmail summary by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-recipient) reverse))

(defun rmail-summary-sort-by-correspondent (reverse)
  "Sort messages of current Rmail summary by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-correspondent) reverse))

(defun rmail-summary-sort-by-lines (reverse)
  "Sort messages of current Rmail summary by lines of the message.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-lines) reverse))

(defun rmail-summary-sort-by-keywords (reverse labels)
  "Sort messages of current Rmail summary by keywords.
If prefix argument REVERSE is non-nil, sort them in reverse order.
KEYWORDS is a comma-separated list of labels."
  (interactive "P\nsSort by labels: ")
  (rmail-sort-from-summary
   (function (lambda (reverse)
	       (rmail-sort-by-keywords reverse labels)))
   reverse))

(defun rmail-sort-from-summary (sortfun reverse)
  "Sort Rmail messages from Summary buffer and update it after sorting."
  (require 'rmailsort)
  (let ((selwin (selected-window)))
    (unwind-protect
	(progn (pop-to-buffer rmail-buffer)
	       (funcall sortfun reverse))
      (select-window selwin))))

;;; rmailsum.el ends here
