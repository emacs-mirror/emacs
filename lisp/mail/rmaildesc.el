;;; rmaildesc.el --- Low level message descriptor library for Rmail.

;; Copyright (C) 2002
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

;;; This package provides low level functions for tracking messages in Rmail.  

;;; Code:

;; Written by Paul Reilly as part of moving BABYL to mbox format.

(eval-when-compile
  (require 'rmailhdr)
  (require 'mail-utils))

(defvar rmail-desc-attributes nil
  "A private variable providing temporary access to message attributes.")

(defvar rmail-desc-delete-callback nil
  "A function pointer called after a message has been deleted.
It expects one argument --- the message number.")

(defvar rmail-desc-vector nil
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
(put 'rmail-desc-vector 'permanent-local t)

;;;; Constants supporting message vector processing.

;;; Message component indexes.

(defconst rmail-desc-beg-index 0
  "The message descriptor element index for the start of the message text.")

(defconst rmail-desc-attrs-index 1
  "The message descriptor element index for the attributes string.")

(defconst rmail-desc-keywords-index 2
  "The message descriptor element index for the User defined labels.")

(defconst rmail-desc-date-index 3
  "The message descriptor element index for the message date information.")

(defconst rmail-desc-line-count-index 4
  "The message descriptor element index for the message line count.")

(defconst rmail-desc-sender-index 5
  "The message descriptor element index for the message line count.")

(defconst rmail-desc-subject-index 6
  "The message descriptor element index for the message line count.")

;;; Attribute indexes

(defconst rmail-desc-answered-index 0
  "The index for the `answered' attribute.")

(defconst rmail-desc-deleted-index 1
  "The index for the `deleted' attribute.")

(defconst rmail-desc-edited-index 2
  "The index for the `edited' attirute.")

(defconst rmail-desc-filed-index 3
  "The index for the `filed' attribute.")

(defconst rmail-desc-resent-index 4
  "The index for the `resent' attribute.")

(defconst rmail-desc-stored-index 5
  "The index for the `stored' attribute.")

(defconst rmail-desc-unseen-index 6
  "The index for the `unseen' attribute.")

(defconst rmail-desc-attr-code-index 0
  "The index for the attibute code.")

(defconst rmail-desc-attr-keyword-index 1
  "The index for the attribute keyword.")

(defconst rmail-desc-attr-summary-offset-index 2
  "The index for the attribute offset in a summary buffer.")

(defconst rmail-desc-attr-alist
  (list (cons rmail-desc-answered-index (list ?A "answered" 1))
        (cons rmail-desc-deleted-index (list ?D "deleted" 0))
	(cons rmail-desc-edited-index (list ?E "edited" 3))
	(cons rmail-desc-filed-index (list ?F "filed" 2))
	(cons rmail-desc-resent-index (list ?R "resent" nil))
	(cons rmail-desc-stored-index (list ?S "stored" 4))
	(cons rmail-desc-unseen-index (list ?  "unseen" 0)))
  "An alist mapping an attribute to a keycode, keyword and summary offset.")

(defconst rmail-desc-attr-index-map
  (list (cons "answered" rmail-desc-answered-index)
        (cons "deleted" rmail-desc-deleted-index)
        (cons "edited" rmail-desc-edited-index)
        (cons "filed" rmail-desc-filed-index)
        (cons "resent" rmail-desc-resent-index)
        (cons "stored" rmail-desc-stored-index)
        (cons "unseen" rmail-desc-unseen-index)))

;;; Date indexes

(defconst rmail-desc-date-day-of-week-index 0
  "The DAY-OF-WEEK index into the list of date information.")

(defconst rmail-desc-date-day-number-index 1
  "The DAY-NUMBER index into the list of date information.")

(defconst rmail-desc-date-month-index 2
  "The MONTH index into the list of date information.")

(defconst rmail-desc-date-year-index 3
  "The YEAR index into the list of date information.")

(defconst rmail-desc-date-time-index 4
  "The TIME index into the list of date information.")

(defsubst rmail-desc-get-descriptor (n)
  "Return a descriptor for message N.
N is 1 based, i.e. the first message number is 1."
  (aref rmail-desc-vector (1- n)))

(defsubst rmail-desc-get-start (n)
  "Return the position of the start of message N."
  (marker-position
   (nth rmail-desc-beg-index (rmail-desc-get-descriptor n))))

(defun rmail-desc-get-end (n)
  "Return the position of the end of message N." 
  (if (= n (length rmail-desc-vector))
      (save-restriction
	(widen)
	(point-max))
    (rmail-desc-get-start (1+ n))))

(defun rmail-desc-add-descriptors (descriptor-list)
  "Append DESCRIPTOR-LIST to the Rmail message descriptor vector."
  (setq rmail-desc-vector
	(vconcat rmail-desc-vector descriptor-list)))

(defun rmail-desc-add-keyword (keyword n)
  "Add KEYWORD to the list of keywords for message N.
The current buffer, likely narrowed, contains message N."

  ;; Append KEYWORD to the descriptor for message N.
  (save-excursion
    (save-restriction
      (let ((keyword-list (rmail-desc-get-keyword-list n))
            (display-state (rmail-desc-get-header-display-state n)))
        (rmail-header-show-headers)
        (if keyword-list

            ;; ??? Don't use setcdr for this.
            ;; Just add it to the front of the list
            ;; and store the updated list back in its proper place.

            ;; Append the string to the list unless it already is there.
            (unless (member-ignore-case keyword keyword-list)
              (setcdr keyword-list (append (cdr keyword-list) (list keyword)))

              ;; Persist the label for this message.
              (rmail-header-add-header
               rmail-header-keyword-header
               (concat (rmail-header-get-header rmail-header-keyword-header)
                       "," keyword)))

          ;; Create the initial keyword list as well as the keyword header
          ;; and persist the header.
          (setq keyword-list
                (nthcdr rmail-desc-keywords-index (rmail-desc-get-descriptor n)))
          (setcar keyword-list (list keyword))
          (rmail-header-add-header rmail-header-keyword-header keyword))
        (rmail-header-toggle-visibility display-state)))))
        
(defun rmail-desc-remove-keyword (keyword n)
  "Remove KEYWORD from the list of keywords for message N.
The current buffer, likely narrowed, contains message N."

  ;; Remove KEYWORD from the descriptor for message N.
  (save-excursion
    (save-restriction
      (let ((desc-list (nthcdr rmail-desc-keywords-index
                               (rmail-desc-get-descriptor n)))
            (display-state (rmail-desc-get-header-display-state n)))

        ;; Remove the keyword from the descriptor.
        (setcar desc-list (delete keyword (car desc-list)))

        ;; Persist the change by removing the keyword for the keywords
        ;; header and restore the display state.
        (rmail-header-show-headers)
        (rmail-header-delete-keyword keyword)
        (rmail-header-toggle-visibility display-state)))))
        
(defun rmail-desc-attr-p (attr-index n)
  "Return the state of the the attribute denoted by ATTR-INDEX in
  message N."
  (let ((attrs (nth rmail-desc-attrs-index
                    (rmail-desc-get-descriptor n))))
    (not (equal "-" (substring attrs attr-index (1+ attr-index))))))

(defun rmail-desc-clear-descriptors ()
  "Clear the Rmail message vector of all messages."
  (setq rmail-desc-vector nil))

(defun rmail-desc-deleted-p (n)
  "Return non-nil if message N is marked for deletion."
  (rmail-desc-attr-p rmail-desc-deleted-index n))

(defun rmail-desc-delete-maybe (n)
  "Determine if message N is marked for deletion.  If so then delete it.
Return t if the message is deleted, nil if not."
  (if (rmail-desc-deleted-p n)
      (progn
        (rmail-desc-delete n)
        t)))

(defun rmail-desc-delete (n)
  "Remove message N from the Rmail buffer and from the descriptor vector."
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
        (delete-region (rmail-desc-get-start n) (rmail-desc-get-end n))
        (move-marker (nth rmail-desc-beg-index (rmail-desc-get-descriptor n)) nil)

        ;; Remove the message descriptor from the Rmail message vector
        ;; and execute the callback indicating the message has been
        ;; deleted.
        (aset rmail-desc-vector (1- n) t)
        (funcall rmail-desc-delete-callback n)))))

(defun rmail-desc-get-attr-code (attr-index n)
  "Return the attribute code for ATTR-INDEX in message N.
If the attribute is not set, return nil."
  (if (rmail-desc-attr-p attr-index n)
      (string (nth rmail-desc-attr-code-index
                   (cdr (assoc attr-index rmail-desc-attr-alist))))))

(defun rmail-desc-get-attr-index (attr)
  "Return the attribute index associated with attribute ATTR, a string."
  (cdr (assoc attr rmail-desc-attr-index-map)))

(defun rmail-desc-get-attributes (n)
  "Return the attribute vector for message N."
  (nth rmail-desc-attrs-index (rmail-desc-get-descriptor n)))

(defsubst rmail-desc-get-count ()
  "Return the number of messages described in the Rmail descriptor vector."
  (length rmail-desc-vector))

(defun rmail-desc-get-date (n)
  "Return the date list generated when the messages were read in."
  (nth rmail-desc-date-index (rmail-desc-get-descriptor n)))

(defun rmail-desc-get-day-number (n)
  "Return the day number (1..31) from the date associated with message N."
  (nth rmail-desc-date-day-number-index
       (nth rmail-desc-date-index (rmail-desc-get-descriptor n))))

(defun rmail-desc-get-day-of-week (n)
  "Return the day of week (Sun .. Sat) from the date associated with message N."
  (nth rmail-desc-date-day-of-week-index
       (nth rmail-desc-date-index (rmail-desc-get-descriptor n))))

(defun rmail-desc-get-default-attrs ()
  "Return the default attributes for a new message."
  (format "%s" "------U"))

(defun rmail-desc-get-header-display-state (n)
  "Return t if ignorable headers are being displayed, nil otherwise."
  (null (overlays-at (rmail-desc-get-start n))))

(defun rmail-desc-get-keyword (attr-index)
  "Return the keyword string associated with ATTR-INDEX."
  (nth rmail-desc-attr-keyword-index
       (cdr (assoc attr-index rmail-desc-attr-alist))))

(defun rmail-desc-get-keyword-list (n)
  "Return the list of User defined keywords for message N."
  (nth rmail-desc-keywords-index (rmail-desc-get-descriptor n)))

(defun rmail-desc-get-keyword-maybe (attribute)
  "Return the keyword associated with ATTRIBUTE if it is set, nil otherwise.
ATTRIBUTE is a cons cell associating an attribute index with a keyword string."
  (let ((index (car attribute)))
    (if (not (equal "-" (substring rmail-desc-attributes index (1+ index))))
	(nth rmail-desc-attr-keyword-index (cdr attribute)))))

(defun rmail-desc-get-keywords (n)
  "Return a list of keywords for message N."
  ;; Combine the attribute keywords with the User defined keywords.
  (setq rmail-desc-attributes (rmail-desc-get-attributes n))
  (append (delq nil (mapcar
                     'rmail-desc-get-keyword-maybe
                     rmail-desc-attr-alist))
          (rmail-desc-get-keyword-list n)))

(defun rmail-desc-get-line-count (n)
  "Return the message body line count."
  (nth rmail-desc-line-count-index (rmail-desc-get-descriptor n)))

(defun rmail-desc-get-month (n)
  "Return the month (Jan .. Dec) from the date associated with message N."
  (nth rmail-desc-date-month-index
       (nth rmail-desc-date-index (rmail-desc-get-descriptor n))))

(defun rmail-desc-get-sender (n)
  "Return the User registered as the mail sender."
  (nth rmail-desc-sender-index (rmail-desc-get-descriptor n)))

(defun rmail-desc-get-subject (n)
  "Return the cached subject header."
  (nth rmail-desc-subject-index (rmail-desc-get-descriptor n)))

(defun rmail-desc-get-summary-offset (attr-index)
  "Return the summary buffer offset associated with ATTR-INDEX.
This is the relative position where the attribute code letter is
displayed in the Rmail summary buffer."
  (nth rmail-desc-attr-summary-offset-index
       (cdr (assoc attr-index rmail-desc-attr-alist))))

(defun rmail-desc-get-time (n)
  "Return the time (hh:mm:ss) from the date associated with message N."
  (nth rmail-desc-date-time-index
       (nth rmail-desc-date-index (rmail-desc-get-descriptor n))))

(defun rmail-desc-get-year (n)
  "Return the year (1969 ... 2###) from the date associated with message N."
  (nth rmail-desc-date-year-index
       (nth rmail-desc-date-index (rmail-desc-get-descriptor n))))

;; This is a strange thing to use.
;; Why not write a simple loop instead?
(defun rmail-desc-make-index-list ()
  "Return a list of integers from 1 to the total number of messages."
  (let ((result (make-vector (length rmail-desc-vector) nil))
	(index 0))
    (while (< index (length result))
      (aset result index (1+ index))
      (setq index (1+ index)))
    (append result nil)))

(defun rmail-desc-prune-deleted-messages (callback)
  "Remove all messages marked for marked for deletion.
Return the number of messages removed.  Invoke CALLBACK immediately
after a message has been deleted.."

  ;; Set the callback.
  (setq rmail-desc-delete-callback callback)

  ;; Remove all messages marked for deletion from the Rmail buffer and
  ;; their descriptors from the Rmail message vector.
  (let ((result (length (delq t (mapcar 'rmail-desc-delete-maybe
					(rmail-desc-make-index-list))))))
    (setq rmail-desc-vector
	  (vconcat (delq t (append rmail-desc-vector nil))))
    result))

(defun rmail-desc-set-attribute (attr-index state n)
  "Set the attribute denoted by ATTR-INDEX in message N according to STATE.
If STATE is non-nil the attribute will be set to the single character code
associated with ATTR-INDEX in rmail-desc-attr-alist, otherwise the attribute is
set to the hyphen character (-)."
  (let ((attributes (nth rmail-desc-attrs-index (rmail-desc-get-descriptor n)))
	code)
    (setq code (if state
		   (car (cdr (assoc attr-index rmail-desc-attr-alist)))
		 ?-))
    (aset attributes attr-index code)
    (rmail-header-persist-attributes attributes)))

(defun rmail-desc-set-start (n pos)
  "Set the start position for message N to POS."
  (set-marker (nth rmail-desc-beg-index (rmail-desc-get-descriptor n)) pos))

(defun rmail-desc-showing-message-p (n)
  "Return t if the current buffer is displaying message N, nil otherwise."
  (let ((beg (rmail-desc-get-start n))
        (end (rmail-desc-get-end n))
        (curpos (point)))
    (and (>= curpos beg) (< curpos end))))

(provide 'rmaildesc)
