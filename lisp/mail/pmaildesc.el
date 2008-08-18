;;; pmaildesc.el --- Low level message descriptor library for Pmail.

;; Copyright (C) 2002, 2006  Free Software Foundation, Inc.

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

;;; This package provides low level functions for tracking messages in Pmail.

;;; Code:

(require 'pmailhdr)

(defvar pmail-desc-attributes nil
  "A private variable providing temporary access to message attributes.")

(defvar pmail-desc-delete-callback nil
  "A function pointer called after a message has been deleted.
It expects one argument --- the message number.")

(defvar pmail-desc-vector nil
  "A vector of message descriptors.
A message descriptor contains data formatted as follows:

	(START ATTRIBUTES KEYWORDS DATE LINE-COUNT SENDER SUBJECT)

where

	START is a marker at the beginning of the header

	ATTRIBUTES is a string where each character encodes an
	attribute.  A hyphen (-) indicates that the attribute is not
	set:

		ANSWERED  The message has been replied to (A).
		DELETED	  The message has been marked for deletion (D).
                EDITED    The message has been edited (E).
		FILED     The message has been filed (F).
		RESENT    The message has been resent (R).
                STORED    The message has been saved to a file (S).
		UNSEEN	  The message has not been read (-).

	KEYWORDS is a list of User defined label strings.

	DATE is a list of strings describing the message date:

		DAY-OF-WEEK	Mon, Sun, etc.
		DAY-NUMBER	9, 13, 15, etc.
		MONTH		Feb, Jun, etc.
		YEAR		2001, 2002, etc.
		TIME		12:03:25, etc.

        LINE-COUNT is the number of lines in the message.

        SENDER is the name of the User sending the message.

        SUBJECT is the subject header, cached to support fast summary line generation.
")
(put 'pmail-desc-vector 'permanent-local t)

;;;; Constants supporting message vector processing.

(defconst pmail-desc-default-attrs "------U"
  "The default attributes for a new message.")

;;; Message component indexes.

(defconst pmail-desc-beg-index 0
  "The message descriptor element index for the start of the message text.")

(defconst pmail-desc-attrs-index 1
  "The message descriptor element index for the attributes string.")

(defconst pmail-desc-keywords-index 2
  "The message descriptor element index for the User defined labels.")

(defconst pmail-desc-date-index 3
  "The message descriptor element index for the message date information.")

(defconst pmail-desc-line-count-index 4
  "The message descriptor element index for the message line count.")

(defconst pmail-desc-sender-index 5
  "The message descriptor element index for the message line count.")

(defconst pmail-desc-subject-index 6
  "The message descriptor element index for the message line count.")

;;; Attribute indexes

(defconst pmail-desc-answered-index 0
  "The index for the `answered' attribute.")

(defconst pmail-desc-deleted-index 1
  "The index for the `deleted' attribute.")

(defconst pmail-desc-edited-index 2
  "The index for the `edited' attribute.")

(defconst pmail-desc-filed-index 3
  "The index for the `filed' attribute.")

(defconst pmail-desc-resent-index 4
  "The index for the `resent' attribute.")

(defconst pmail-desc-stored-index 5
  "The index for the `stored' attribute.")

(defconst pmail-desc-unseen-index 6
  "The index for the `unseen' attribute.")

(defconst pmail-desc-attr-code-index 0
  "The index for the attibute code.")

(defconst pmail-desc-attr-keyword-index 1
  "The index for the attribute keyword.")

(defconst pmail-desc-attr-summary-offset-index 2
  "The index for the attribute offset in a summary buffer.")

(defconst pmail-desc-attr-alist
  (list (cons pmail-desc-answered-index (list ?A "answered" 1))
        (cons pmail-desc-deleted-index (list ?D "deleted" 0))
	(cons pmail-desc-edited-index (list ?E "edited" 3))
	(cons pmail-desc-filed-index (list ?F "filed" 2))
	(cons pmail-desc-resent-index (list ?R "resent" nil))
	(cons pmail-desc-stored-index (list ?S "stored" 4))
	(cons pmail-desc-unseen-index (list ?  "unseen" 0)))
  "An alist mapping an attribute to a keycode, keyword and summary offset.")

(defconst pmail-desc-attr-index-map
  (list (cons "answered" pmail-desc-answered-index)
        (cons "deleted" pmail-desc-deleted-index)
        (cons "edited" pmail-desc-edited-index)
        (cons "filed" pmail-desc-filed-index)
        (cons "resent" pmail-desc-resent-index)
        (cons "stored" pmail-desc-stored-index)
        (cons "unseen" pmail-desc-unseen-index)))

;;; Date indexes

(defconst pmail-desc-date-day-of-week-index 0
  "The DAY-OF-WEEK index into the list of date information.")

(defconst pmail-desc-date-day-number-index 1
  "The DAY-NUMBER index into the list of date information.")

(defconst pmail-desc-date-month-index 2
  "The MONTH index into the list of date information.")

(defconst pmail-desc-date-year-index 3
  "The YEAR index into the list of date information.")

(defconst pmail-desc-date-time-index 4
  "The TIME index into the list of date information.")

(defsubst pmail-desc-get-descriptor (n)
  "Return a descriptor for message N.
N is 1 based, i.e. the first message number is 1."
  (aref pmail-desc-vector (1- n)))

(defsubst pmail-desc-get-start (n)
  "Return the position of the start of message N."
  (marker-position
   (nth pmail-desc-beg-index (pmail-desc-get-descriptor n))))

(defun pmail-desc-get-end (n)
  "Return the position of the end of message N."
  (if (= n (length pmail-desc-vector))
      (save-restriction
	(widen)
	(point-max))
    (pmail-desc-get-start (1+ n))))

(defun pmail-desc-add-descriptors (descriptor-list)
  "Append DESCRIPTOR-LIST to the Pmail message descriptor vector."
  (setq pmail-desc-vector
	(vconcat pmail-desc-vector descriptor-list)))

(defun pmail-desc-add-keyword (keyword n)
  "Add KEYWORD to the list of keywords for message N.
The current buffer must be narrowed to message N.  Both
`pmail-desc-vector' and the message headers are updated."
  (save-excursion
    (save-restriction
      (let ((keywords (pmail-desc-get-keywords n))
            (display-state (pmail-desc-get-header-display-state n)))
	(unless (member keyword keywords)
	  (setq keywords (cons keyword keywords))
	  (setcar (nthcdr pmail-desc-keywords-index (pmail-desc-get-descriptor n))
		  keywords)
	  (pmail-header-show-headers)
	  (pmail-header-add-header pmail-header-keyword-header
				   (mapconcat 'identity keywords ","))
	  (pmail-header-toggle-visibility display-state))))))

(defun pmail-desc-remove-keyword (keyword n)
  "Remove KEYWORD from the list of keywords for message N.
The current buffer must be narrowed to message N.  Both
`pmail-desc-vector' and the message headers are updated."
  (save-excursion
    (save-restriction
      (let ((keywords (pmail-desc-get-keywords n))
            (display-state (pmail-desc-get-header-display-state n)))
	(when (member keyword keywords)
	  (setq keywords (delete keyword keywords))
	  (setcar (nthcdr pmail-desc-keywords-index (pmail-desc-get-descriptor n))
		  keywords)
	  (pmail-header-show-headers)
	  (pmail-header-add-header pmail-header-keyword-header
				   (mapconcat 'identity keywords ","))
	  (pmail-header-toggle-visibility display-state))))))

(defun pmail-desc-attr-p (attr-index n)
  "Return the state of the the attribute denoted by ATTR-INDEX in
  message N."
  (let ((attrs (nth pmail-desc-attrs-index
                    (pmail-desc-get-descriptor n))))
    (not (equal "-" (substring attrs attr-index (1+ attr-index))))))

(defun pmail-desc-clear-descriptors ()
  "Clear the Pmail message vector of all messages."
  (setq pmail-desc-vector nil))

(defun pmail-desc-deleted-p (n)
  "Return non-nil if message N is marked for deletion."
  (pmail-desc-attr-p pmail-desc-deleted-index n))
(defalias 'pmail-message-deleted-p 'pmail-desc-deleted-p)

(defun pmail-desc-delete-maybe (n)
  "Determine if message N is marked for deletion.  If so then delete it.
Return t if the message is deleted, nil if not."
  (if (pmail-desc-deleted-p n)
      (progn
        (pmail-desc-delete n)
        t)))

(defun pmail-desc-delete (n)
  "Remove message N from the Pmail buffer and from the descriptor vector."
  (save-excursion
    (save-restriction
      ;; Enable the buffer to be written, ignore intangibility and do
      ;; not record these changes in the undo list.
      (let ((inhibit-read-only t)
            (inhibit-point-motion-hooks t)
            (buffer-undo-list t)
            start end)
        (widen)

        ;; Remove the message from the buffer and neutralize the
        ;; marker pointing to the start of the message.
        (delete-region (pmail-desc-get-start n) (pmail-desc-get-end n))
        (move-marker (nth pmail-desc-beg-index (pmail-desc-get-descriptor n)) nil)

        ;; Remove the message descriptor from the Pmail message vector
        ;; and execute the callback indicating the message has been
        ;; deleted.
        (aset pmail-desc-vector (1- n) t)
        (funcall pmail-desc-delete-callback n)))))

(defun pmail-desc-get-attr-code (attr-index n)
  "Return the attribute code for ATTR-INDEX in message N.
If the attribute is not set, return nil."
  (if (pmail-desc-attr-p attr-index n)
      (string (nth pmail-desc-attr-code-index
                   (cdr (assoc attr-index pmail-desc-attr-alist))))))

(defun pmail-desc-get-attr-index (attr)
  "Return the attribute index associated with attribute ATTR, a string."
  (cdr (assoc attr pmail-desc-attr-index-map)))

(defun pmail-desc-get-attributes (n)
  "Return the attribute vector for message N."
  (nth pmail-desc-attrs-index (pmail-desc-get-descriptor n)))

(defsubst pmail-desc-get-count ()
  "Return the number of messages described in the Pmail descriptor vector."
  (length pmail-desc-vector))

(defun pmail-desc-get-date (n)
  "Return the date list generated when the messages were read in."
  (nth pmail-desc-date-index (pmail-desc-get-descriptor n)))

(defun pmail-desc-get-day-number (n)
  "Return the day number (1..31) from the date associated with message N."
  (nth pmail-desc-date-day-number-index
       (nth pmail-desc-date-index (pmail-desc-get-descriptor n))))

(defun pmail-desc-get-day-of-week (n)
  "Return the day of week (Sun .. Sat) from the date associated with message N."
  (nth pmail-desc-date-day-of-week-index
       (nth pmail-desc-date-index (pmail-desc-get-descriptor n))))

(defun pmail-desc-get-header-display-state (n)
  "Return t if ignorable headers are being displayed, nil otherwise."
  (save-excursion
    (save-restriction
      (pmail-narrow-to-header n)
      (null (overlays-in (point-min) (point-max))))))

(defun pmail-desc-get-keyword (attr-index)
  "Return the keyword string associated with ATTR-INDEX."
  (nth pmail-desc-attr-keyword-index
       (cdr (assoc attr-index pmail-desc-attr-alist))))

(defun pmail-desc-get-keyword-list (n)
  "Return the list of user-defined labels for message N."
  (nth pmail-desc-keywords-index (pmail-desc-get-descriptor n)))

(defun pmail-desc-get-keyword-maybe (attribute)
  "Return the keyword associated with ATTRIBUTE if it is set, nil otherwise.
ATTRIBUTE is a cons cell associating an attribute index with a keyword string."
  (let ((index (car attribute)))
    (if (not (equal "-" (substring pmail-desc-attributes index (1+ index))))
	(nth pmail-desc-attr-keyword-index (cdr attribute)))))

(defun pmail-desc-get-keywords (n)
  "Return a list of keywords for message N.
This includes the attributes."
  (setq pmail-desc-attributes (pmail-desc-get-attributes n))
  (append (delq nil (mapcar
                     'pmail-desc-get-keyword-maybe
                     pmail-desc-attr-alist))
          (pmail-desc-get-keyword-list n)))

(defun pmail-desc-get-line-count (n)
  "Return the message body line count."
  (nth pmail-desc-line-count-index (pmail-desc-get-descriptor n)))

(defun pmail-desc-get-month (n)
  "Return the month (Jan .. Dec) from the date associated with message N."
  (nth pmail-desc-date-month-index
       (nth pmail-desc-date-index (pmail-desc-get-descriptor n))))

(defun pmail-desc-get-sender (n)
  "Return the User registered as the mail sender."
  (nth pmail-desc-sender-index (pmail-desc-get-descriptor n)))

(defun pmail-desc-get-subject (n)
  "Return the cached subject header."
  (nth pmail-desc-subject-index (pmail-desc-get-descriptor n)))

(defun pmail-desc-get-summary-offset (attr-index)
  "Return the summary buffer offset associated with ATTR-INDEX.
This is the relative position where the attribute code letter is
displayed in the Pmail summary buffer."
  (nth pmail-desc-attr-summary-offset-index
       (cdr (assoc attr-index pmail-desc-attr-alist))))

(defun pmail-desc-get-time (n)
  "Return the time (hh:mm:ss) from the date associated with message N."
  (nth pmail-desc-date-time-index
       (nth pmail-desc-date-index (pmail-desc-get-descriptor n))))

(defun pmail-desc-get-year (n)
  "Return the year (1969 ... 2###) from the date associated with message N."
  (nth pmail-desc-date-year-index
       (nth pmail-desc-date-index (pmail-desc-get-descriptor n))))

;; This is a strange thing to use.
;; Why not write a simple loop instead?
(defun pmail-desc-make-index-list ()
  "Return a list of integers from 1 to the total number of messages."
  (let ((result (make-vector (length pmail-desc-vector) nil))
	(index 0))
    (while (< index (length result))
      (aset result index (1+ index))
      (setq index (1+ index)))
    (append result nil)))

(defun pmail-desc-prune-deleted-messages (callback)
  "Remove all messages marked for marked for deletion.
Return the number of messages removed.  Invoke CALLBACK immediately
after a message has been deleted.."

  ;; Set the callback.
  (setq pmail-desc-delete-callback callback)

  ;; Remove all messages marked for deletion from the Pmail buffer and
  ;; their descriptors from the Pmail message vector.
  (let ((result (length (delq t (mapcar 'pmail-desc-delete-maybe
					(pmail-desc-make-index-list))))))
    (setq pmail-desc-vector
	  (vconcat (delq t (append pmail-desc-vector nil))))
    result))

(defun pmail-desc-set-attribute (attr-index state n)
  "Set the attribute denoted by ATTR-INDEX in message N according to STATE.
If STATE is non-nil the attribute will be set to the single character code
associated with ATTR-INDEX in pmail-desc-attr-alist, otherwise the attribute is
set to the hyphen character (-)."
  (let ((attributes (nth pmail-desc-attrs-index (pmail-desc-get-descriptor n)))
	code)
    (setq code (if state
		   (car (cdr (assoc attr-index pmail-desc-attr-alist)))
		 ?-))
    (aset attributes attr-index code)
    (pmail-header-add-header pmail-header-attribute-header attributes)))

(defun pmail-desc-set-start (n pos)
  "Set the start position for message N to POS."
  (set-marker (nth pmail-desc-beg-index (pmail-desc-get-descriptor n)) pos))

(defun pmail-desc-showing-message-p (n)
  "Return t if the current buffer is displaying message N, nil otherwise."
  (let ((beg (pmail-desc-get-start n))
        (end (pmail-desc-get-end n))
        (curpos (point)))
    (and (>= curpos beg) (< curpos end))))

(provide 'pmaildesc)

;;; pmaildesc.el ends here
