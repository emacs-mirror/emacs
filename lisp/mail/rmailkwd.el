;;; rmailkwd.el --- part of the "RMAIL" mail reader for Emacs

;; Copyright (C) 1985, 1988, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library manages keywords (labels).  Labels are stored in the
;; variable `rmail-keywords'.

;;; Code:

(defvar rmail-buffer)
(defvar rmail-current-message)
(defvar rmail-last-label)
(defvar rmail-last-multi-labels)
(defvar rmail-summary-vector)
(defvar rmail-total-messages)

;; Global to all RMAIL buffers.  It exists primarily for the sake of
;; completion.  It is better to use strings with the label functions
;; and let them worry about making the label.

(provide 'rmailkwd)

(eval-when-compile
  (require 'mail-utils))

(defvar rmail-label-obarray (make-vector 47 0))

;; Named list of symbols representing valid message attributes in RMAIL.

(defconst rmail-attributes
  (cons 'rmail-keywords
	(mapcar (function (lambda (s) (intern s rmail-label-obarray)))
		'("deleted" "answered" "filed" "forwarded" "unseen" "edited"
		  "resent"))))

(defconst rmail-deleted-label (intern "deleted" rmail-label-obarray))

;; Named list of symbols representing valid message keywords in RMAIL.

(defvar rmail-keywords)

;;;; Low-level functions.

;; Return a list of symbols for all the keywords (labels) recorded in
;; this file's Labels.
(defun rmail-keywords ()
  "Return a list of all known keywords."
  (or rmail-keywords (rmail-keyword-init)))

(defun rmail-keyword-init ()
  "Initialize the variable `rmail-keywords' to hold no keywords.
The value is actually (nil), since (cdr rmail-keywords) is the
actual list of keywords."
  (setq rmail-keywords (cons 'rmail-keywords nil)))

(defun rmail-attribute-p (s)
  (let ((symbol (rmail-make-label s)))
    (if (memq symbol (cdr rmail-attributes)) symbol)))

(defun rmail-keyword-p (s)
  "Non-nil if S is a known keyword for this Rmail file."
  (let ((symbol (rmail-make-label s)))
    (if (memq symbol (cdr (rmail-keywords))) symbol)))

(defun rmail-make-label (s &optional forcep)
  (cond ((symbolp s) s)
	(forcep (intern (downcase s) rmail-label-obarray))
	(t  (intern-soft (downcase s) rmail-label-obarray))))

;;; (defun rmail-force-make-label (s)
;;;   (intern (downcase s) rmail-label-obarray))

(defun rmail-quote-label-name (label)
  (regexp-quote (symbol-name (rmail-make-label label t))))

;;;###autoload
(defun rmail-keyword-register-keywords (keyword-list)
  "Add the strings in KEYWORD-LIST to `rmail-keywords'.
Return a list of the keywords newly added (those that were
not already known)."
  (delq nil (mapcar 'rmail-install-keyword keyword-list)))

;;; mbox: ready
;; Add WORD to the list in the file's Labels option.
;; Any keyword used for the first time needs this done.
(defun rmail-install-keyword (word)
  "Append WORD to the global list of keywords.  Ignore duplicates.
Return WORD if it is a new entry, nil otherwise."
  (let ((keyword (rmail-make-label word t))
	(keywords (rmail-keywords)))
    (if (not (or (rmail-attribute-p keyword)
		 (rmail-keyword-p keyword)))
        (progn
          (setcdr keywords (cons keyword (cdr keywords)))
          keyword))))

;;;; Adding and removing message keywords.

;;;###autoload
(defun rmail-add-label (string)
  "Add LABEL to labels associated with current RMAIL message."
  (interactive (list (rmail-read-label "Add label")))
  (rmail-set-label string t)
  (rmail-display-labels))

;;;###autoload
(defun rmail-kill-label (string)
  "Remove LABEL from labels associated with current RMAIL message."
  (interactive (list (rmail-read-label "Remove label" t)))
  (rmail-set-label string nil))

;;;###autoload
(defun rmail-read-label (prompt &optional existing)
  "Ask for a label using PROMPT.
If EXISTING is non-nil, ask for one of the labels of the current
message."
  (when (= rmail-total-messages 0)
    (error "No messages in this file"))
  (with-current-buffer rmail-buffer
    (let ((result (if existing
		      (let* ((keywords (rmail-desc-get-keywords
					rmail-current-message))
			     (last (symbol-name rmail-last-label))
			     (default (if (member last keywords)
					  last
					(car keywords))))
			(unless keywords
			  (error "No labels for the current message"))
			(completing-read
			 (concat prompt " (default " default "): ")
			 keywords nil t nil nil default))
		    (let ((default (symbol-name rmail-last-label)))
		      (completing-read
		       (concat prompt (if rmail-last-label
					  (concat " (default " default "): ")
					": "))
		       rmail-label-obarray nil nil nil nil default)))))
      (setq rmail-last-label (rmail-make-label result t)))))

(defun rmail-set-label (l state &optional n)
  "Add (STATE is non-nil) or remove (STATE is nil) label L in message N.
If N is nil then use the current Rmail message.  The current buffer,
possibly narrowed, displays a message."
  (if (= rmail-total-messages 0)
      (error "No messages in this file"))
  (with-current-buffer rmail-buffer
    (if (not n) (setq n rmail-current-message))
    (save-restriction
      (widen)
      (narrow-to-region (rmail-desc-get-start n) (rmail-desc-get-end n))
      (if (rmail-attribute-p l)
          (rmail-set-attribute l state n)
        ;; Make sure the keyword is registered.
        (or (rmail-keyword-p l) (rmail-install-keyword l))
        (let ((keyword (symbol-name l)))
          (if state
              (rmail-desc-add-keyword keyword n)
            (rmail-desc-remove-keyword keyword n)))
	;; FIXME: handle redisplay in the summary buffer
	(rmail-display-labels))))
  ;; Deal with the summary buffer.
  (when rmail-summary-buffer
    (rmail-summary-update n)))

;; Motion on messages with keywords.

;;;###autoload
(defun rmail-previous-labeled-message (n labels)
  "Show previous message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves backward N messages with these labels."
  (interactive "p\nsMove to previous msg with labels: ")
  (rmail-next-labeled-message (- n) labels))

;;;###autoload
(defun rmail-next-labeled-message (n labels)
  "Show next message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves forward N messages with these labels."
  (interactive "p\nsMove to next msg with labels: ")
  (when (string= labels "")
    (setq labels rmail-last-multi-labels))
  (unless labels
    (error "No labels to find have been specified previously"))
  (with-current-buffer rmail-buffer
    (setq rmail-last-multi-labels labels)
    (let ((lastwin rmail-current-message)
	  (current rmail-current-message)
	  (regexp (concat ", ?\\("
			  (mail-comma-list-regexp labels)
			  "\\),")))
      (save-restriction
	(widen)
	(while (and (> n 0) (< current rmail-total-messages))
	  (setq current (1+ current))
	  (when (rmail-message-labels-p current regexp)
	    (setq lastwin current n (1- n))))
	(while (and (< n 0) (> current 1))
	  (setq current (1- current))
	  (when (rmail-message-labels-p current regexp)
	    (setq lastwin current n (1+ n)))))
      (rmail-show-message lastwin)
      (when (< n 0)
	(message "No previous message with labels %s" labels))
      (when (> n 0)
	(message "No following message with labels %s" labels)))))

;;; arch-tag: b26b3392-99ca-4e1d-933a-dab59b04e9a8
;;; rmailkwd.el ends here
