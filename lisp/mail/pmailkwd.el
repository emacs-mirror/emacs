;;; pmailkwd.el --- part of the "PMAIL" mail reader for Emacs

;; Copyright (C) 1985, 1988, 1994, 2001, 2002, 2003, 2004, 2005, 2006,
;;   2007, 2008 Free Software Foundation, Inc.

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

;; This library manages keywords (labels).  Labels are stored in the
;; variable `pmail-keywords'.

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

(eval-when-compile
  (require 'mail-utils))

;; Named list of symbols representing valid message attributes in PMAIL.

(defconst pmail-attributes
  '(deleted answered filed forwarded unseen edited resent)
  "Keywords with defined semantics used to label messages.
These have a well-defined meaning to the PMAIL system.")

(defconst pmail-deleted-label 'deleted)

;; Named list of symbols representing valid message keywords in PMAIL.

(defvar pmail-keywords nil
  "Keywords used to label messages.
These are all user-defined, unlike `pmail-attributes'.")


;; External library declarations.
(declare-function mail-comma-list-regexp "mail-utils" (labels))
(declare-function mail-parse-comma-list "mail-utils" ())
(declare-function pmail-desc-add-keyword "pmaildesc" (keyword n))
(declare-function pmail-desc-get-end "pmaildesc" (n))
(declare-function pmail-desc-get-keywords "pmaildesc" (n))
(declare-function pmail-desc-get-start "pmaildesc" (n))
(declare-function pmail-desc-remove-keyword "pmaildesc" (keyword n))
(declare-function pmail-display-labels "pmail" ())
(declare-function pmail-maybe-set-message-counters "pmail" ())
(declare-function pmail-message-labels-p "pmail" (msg labels))
(declare-function pmail-msgbeg "pmail" (n))
(declare-function pmail-set-attribute "pmail" (attr state &optional msgnum))
(declare-function pmail-set-message-deleted-p "pmail" (n state))
(declare-function pmail-show-message "pmail" (&optional n no-summary))
(declare-function pmail-summary-exists "pmail" ())
(declare-function pmail-summary-update "pmailsum" (n))

;;;; Low-level functions.

(defun pmail-attribute-p (s)
  "Non-nil if S is a known attribute.
See `pmail-attributes'."
  (let ((symbol (pmail-make-label s)))
    (memq symbol pmail-attributes)))

(defun pmail-keyword-p (s)
  "Non-nil if S is a known keyword for this Pmail file.
See `pmail-keywords'."
  (let ((symbol (pmail-make-label s)))
    (memq symbol pmail-keywords)))

(defun pmail-make-label (s &optional forcep)
  (cond ((symbolp s) s)
	(forcep (intern (downcase s)))
	(t  (intern-soft (downcase s)))))

(defun pmail-quote-label-name (label)
  (regexp-quote (symbol-name (pmail-make-label label t))))

;;;###autoload
(defun pmail-register-keywords (words)
  "Add the strings in WORDS to `pmail-keywords'."
  (dolist (word words)
    (pmail-register-keyword word)))

(defun pmail-register-keyword (word)
  "Append the string WORD to `pmail-keywords',
unless it already is a keyword or an attribute."
  (let ((keyword (pmail-make-label word t)))
    (unless (or (pmail-attribute-p keyword)
		(pmail-keyword-p keyword))
      (setq pmail-keywords (cons keyword pmail-keywords)))))

;;;; Adding and removing message keywords.

;;;###autoload
(defun pmail-add-label (string)
  "Add LABEL to labels associated with current PMAIL message."
  (interactive (list (pmail-read-label "Add label")))
  (pmail-set-label (pmail-make-label string) t)
  (pmail-display-labels))

;;;###autoload
(defun pmail-kill-label (string)
  "Remove LABEL from labels associated with current PMAIL message."
  (interactive (list (pmail-read-label "Remove label" t)))
  (pmail-set-label (pmail-make-label string) nil))

;;;###autoload
(defun pmail-read-label (prompt &optional existing)
  "Ask for a label using PROMPT.
If EXISTING is non-nil, ask for one of the labels of the current
message."
  (when (= pmail-total-messages 0)
    (error "No messages in this file"))
  (with-current-buffer pmail-buffer
    (let ((result (if existing
		      (let* ((keywords (pmail-desc-get-keywords
					pmail-current-message))
			     (last (symbol-name pmail-last-label))
			     (default (if (member last keywords)
					  last
					(car keywords))))
			(unless keywords
			  (error "No labels for the current message"))
			(completing-read
			 (concat prompt " (default " default "): ")
			 keywords nil t nil nil default))
		    (let ((default (symbol-name pmail-last-label)))
		      (completing-read
		       (concat prompt (if pmail-last-label
					  (concat " (default " default "): ")
					": "))
		       (mapcar 'list pmail-keywords)
		       nil nil nil nil default)))))
      (setq pmail-last-label (pmail-make-label result t))
      ;; return the string, not the symbol
      result)))

(defun pmail-set-label (l state &optional n)
  "Add or remove label L in message N.
The label L is added when STATE is non-nil, otherwise it is
removed.  If N is nil then use the current Pmail message.  The
current buffer, possibly narrowed, displays a message."
  (if (= pmail-total-messages 0)
      (error "No messages in this file"))
  (with-current-buffer pmail-buffer
    (if (not n) (setq n pmail-current-message))
    (save-restriction
      (widen)
      (narrow-to-region (pmail-desc-get-start n) (pmail-desc-get-end n))
      ;; FIXME: we should move all string-using functions to symbols!
      (let ((str (symbol-name l)))
	(if (pmail-attribute-p l)
	    (pmail-set-attribute str state n)
	  ;; Make sure the keyword is registered.
	  (pmail-register-keyword l)
	  (if state
	      (pmail-desc-add-keyword str n)
	    (pmail-desc-remove-keyword str n))))))
  (pmail-display-labels)
  ;; Deal with the summary buffer.
  (when (pmail-summary-exists)
    (pmail-summary-update n)))

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
  (when (string= labels "")
    (setq labels pmail-last-multi-labels))
  (unless labels
    (error "No labels to find have been specified previously"))
  (with-current-buffer pmail-buffer
    (setq pmail-last-multi-labels labels)
    (let ((lastwin pmail-current-message)
	  (current pmail-current-message)
	  (regexp (concat ", ?\\("
			  (mail-comma-list-regexp labels)
			  "\\),")))
      (save-restriction
	(widen)
	(while (and (> n 0) (< current pmail-total-messages))
	  (setq current (1+ current))
	  (when (pmail-message-labels-p current regexp)
	    (setq lastwin current n (1- n))))
	(while (and (< n 0) (> current 1))
	  (setq current (1- current))
	  (when (pmail-message-labels-p current regexp)
	    (setq lastwin current n (1+ n)))))
      (pmail-show-message lastwin)
      (when (< n 0)
	(message "No previous message with labels %s" labels))
      (when (> n 0)
	(message "No following message with labels %s" labels)))))

(provide 'pmailkwd)

;; arch-tag: 1149979c-8e47-4333-9629-cf3dc887a6a7
;;; pmailkwd.el ends here
