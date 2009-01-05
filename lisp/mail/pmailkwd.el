;;; pmailkwd.el --- part of the "PMAIL" mail reader for Emacs

;; Copyright (C) 1985, 1988, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

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

(defvar pmail-buffer)
(defvar pmail-current-message)
(defvar pmail-last-label)
(defvar pmail-last-multi-labels)
(defvar pmail-summary-vector)
(defvar pmail-total-messages)

;; Global to all PMAIL buffers.  It exists primarily for the sake of
;; completion.  It is better to use strings with the label functions
;; and let them worry about making the label.

(defvar pmail-label-obarray (make-vector 47 0))

;; Named list of symbols representing valid message attributes in PMAIL.

(defconst pmail-attributes
  (cons 'pmail-keywords
	(mapcar (function (lambda (s) (intern s pmail-label-obarray)))
		'("deleted" "answered" "filed" "forwarded" "unseen" "edited"
		  "resent"))))

(defconst pmail-deleted-label (intern "deleted" pmail-label-obarray))

;; Named list of symbols representing valid message keywords in PMAIL.

(defvar pmail-keywords)

;;;###autoload
(defun pmail-add-label (string)
  "Add LABEL to labels associated with current PMAIL message.
Completion is performed over known labels when reading."
  (interactive (list (pmail-read-label "Add label")))
  (pmail-set-label string t))

;;;###autoload
(defun pmail-kill-label (string)
  "Remove LABEL from labels associated with current PMAIL message.
Completion is performed over known labels when reading."
  (interactive (list (pmail-read-label "Remove label")))
  (pmail-set-label string nil))

;;;###autoload
(defun pmail-read-label (prompt)
  (with-current-buffer pmail-buffer
    (if (not pmail-keywords) (pmail-parse-file-keywords))
    (let ((result
	   (completing-read (concat prompt
				    (if pmail-last-label
					(concat " (default "
						(symbol-name pmail-last-label)
						"): ")
				      ": "))
			    pmail-label-obarray
			    nil
			    nil)))
      (if (string= result "")
	  pmail-last-label
	(setq pmail-last-label (pmail-make-label result t))))))

(declare-function pmail-maybe-set-message-counters "pmail" ())
(declare-function pmail-display-labels "pmail" ())
(declare-function pmail-msgbeg "pmail" (n))
(declare-function pmail-set-message-deleted-p "pmail" (n state))
(declare-function pmail-message-labels-p "pmail" (msg labels))
(declare-function pmail-show-message "pmail" (&optional n no-summary))
(declare-function mail-comma-list-regexp "mail-utils" (labels))
(declare-function mail-parse-comma-list "mail-utils.el" ())

(defun pmail-set-label (l state &optional n)
  (with-current-buffer pmail-buffer
    (pmail-maybe-set-message-counters)
    (if (not n) (setq n pmail-current-message))
    (aset pmail-summary-vector (1- n) nil)
    (let* ((attribute (pmail-attribute-p l))
	   (keyword (and (not attribute)
			 (or (pmail-keyword-p l)
			     (pmail-install-keyword l))))
	   (label (or attribute keyword)))
      (if label
	  (let ((omax (- (buffer-size) (point-max)))
		(omin (- (buffer-size) (point-min)))
		(buffer-read-only nil)
		(case-fold-search t))
	    (unwind-protect
		(save-excursion
		  (widen)
		  (goto-char (pmail-msgbeg n))
		  (forward-line 1)
		  (if (not (looking-at "[01],"))
		      nil
		    (let ((start (1+ (point)))
			  (bound))
		      (narrow-to-region (point) (progn (end-of-line) (point)))
		      (setq bound (point-max))
		      (search-backward ",," nil t)
		      (if attribute
			  (setq bound (1+ (point)))
			(setq start (1+ (point))))
		      (goto-char start)
;		      (while (re-search-forward "[ \t]*,[ \t]*" nil t)
;			(replace-match ","))
;		      (goto-char start)
		      (if (re-search-forward
			   (concat ", " (pmail-quote-label-name label) ",")
			   bound
			   'move)
			  (if (not state) (replace-match ","))
			(if state (insert " " (symbol-name label) ",")))
		      (if (eq label pmail-deleted-label)
			  (pmail-set-message-deleted-p n state)))))
	      (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax))
	      (if (= n pmail-current-message) (pmail-display-labels))))))))

;; Commented functions aren't used by PMAIL but might be nice for user
;; packages that do stuff with PMAIL.  Note that pmail-message-labels-p
;; is in pmail.el now.

;(defun pmail-message-label-p (label &optional n)
;  "Returns symbol if LABEL (attribute or keyword) on NTH or current message."
;  (pmail-message-labels-p (or n pmail-current-message) (regexp-quote label)))

;(defun pmail-parse-message-labels (&optional n)
;  "Returns labels associated with NTH or current PMAIL message.
;The result is a list of two lists of strings.  The first is the
;message attributes and the second is the message keywords."
;  (let (atts keys)
;    (save-restriction
;      (widen)
;      (goto-char (pmail-msgbeg (or n pmail-current-message)))
;      (forward-line 1)
;      (or (looking-at "[01],") (error "Malformed label line"))
;      (forward-char 2)
;      (while (looking-at "[ \t]*\\([^ \t\n,]+\\),")
;	(setq atts (cons (buffer-substring (match-beginning 1) (match-end 1))
;			  atts))
;	(goto-char (match-end 0)))
;      (or (looking-at ",") (error "Malformed label line"))
;      (forward-char 1)
;      (while (looking-at "[ \t]*\\([^ \t\n,]+\\),")
;	(setq keys (cons (buffer-substring (match-beginning 1) (match-end 1))
;			 keys))
;	(goto-char (match-end 0)))
;      (or (looking-at "[ \t]*$") (error "Malformed label line"))
;      (list (nreverse atts) (nreverse keys)))))

(defun pmail-attribute-p (s)
  (let ((symbol (pmail-make-label s)))
    (if (memq symbol (cdr pmail-attributes)) symbol)))

(defun pmail-keyword-p (s)
  (let ((symbol (pmail-make-label s)))
    (if (memq symbol (cdr (pmail-keywords))) symbol)))

(defun pmail-make-label (s &optional forcep)
  (cond ((symbolp s) s)
	(forcep (intern (downcase s) pmail-label-obarray))
	(t  (intern-soft (downcase s) pmail-label-obarray))))

(defun pmail-force-make-label (s)
  (intern (downcase s) pmail-label-obarray))

(defun pmail-quote-label-name (label)
  (regexp-quote (symbol-name (pmail-make-label label t))))

;; Motion on messages with keywords.

;;;###autoload
(defun pmail-previous-labeled-message (n labels)
  "Show previous message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves backward N messages with these labels."
  (interactive "p\nsMove to previous msg with labels: ")
  (pmail-next-labeled-message (- n) labels))

;;;###autoload
(defun pmail-next-labeled-message (n labels)
  "Show next message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves forward N messages with these labels."
  (interactive "p\nsMove to next msg with labels: ")
  (if (string= labels "")
      (setq labels pmail-last-multi-labels))
  (or labels
      (error "No labels to find have been specified previously"))
  (set-buffer pmail-buffer)
  (setq pmail-last-multi-labels labels)
  (pmail-maybe-set-message-counters)
  (let ((lastwin pmail-current-message)
	(current pmail-current-message)
	(regexp (concat ", ?\\("
			(mail-comma-list-regexp labels)
			"\\),")))
    (save-restriction
      (widen)
      (while (and (> n 0) (< current pmail-total-messages))
	(setq current (1+ current))
	(if (pmail-message-labels-p current regexp)
	    (setq lastwin current n (1- n))))
      (while (and (< n 0) (> current 1))
	(setq current (1- current))
	(if (pmail-message-labels-p current regexp)
	    (setq lastwin current n (1+ n)))))
    (pmail-show-message lastwin)
    (if (< n 0)
	(message "No previous message with labels %s" labels))
    (if (> n 0)
	(message "No following message with labels %s" labels))))

;;; Manipulate the file's Labels option.

;; Return a list of symbols for all
;; the keywords (labels) recorded in this file's Labels option.
(defun pmail-keywords ()
  (or pmail-keywords (pmail-parse-file-keywords)))

;; Set pmail-keywords to a list of symbols for all
;; the keywords (labels) recorded in this file's Labels option.
(defun pmail-parse-file-keywords ()
  (save-restriction
    (save-excursion
      (widen)
      (goto-char 1)
      (setq pmail-keywords
	    (if (search-forward "\nLabels:" (pmail-msgbeg 1) t)
		(progn
		  (narrow-to-region (point) (progn (end-of-line) (point)))
		  (goto-char (point-min))
		  (cons 'pmail-keywords
			(mapcar 'pmail-force-make-label
				(mail-parse-comma-list)))))))))

;; Add WORD to the list in the file's Labels option.
;; Any keyword used for the first time needs this done.
(defun pmail-install-keyword (word)
  (let ((keyword (pmail-make-label word t))
	(keywords (pmail-keywords)))
    (if (not (or (pmail-attribute-p keyword)
		 (pmail-keyword-p keyword)))
	(let ((omin (- (buffer-size) (point-min)))
	      (omax (- (buffer-size) (point-max))))
	  (unwind-protect
	      (save-excursion
		(widen)
		(goto-char 1)
		(let ((case-fold-search t)
		      (buffer-read-only nil))
		  (or (search-forward "\nLabels:" nil t)
		      (progn
			(end-of-line)
			(insert "\nLabels:")))
		  (delete-region (point) (progn (end-of-line) (point)))
		  (setcdr keywords (cons keyword (cdr keywords)))
		  (while (setq keywords (cdr keywords))
		    (insert (symbol-name (car keywords)) ","))
		  (delete-char -1)))
	    (narrow-to-region (- (buffer-size) omin)
			      (- (buffer-size) omax)))))
    keyword))

(provide 'pmailkwd)

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 1149979c-8e47-4333-9629-cf3dc887a6a7
;;; pmailkwd.el ends here
