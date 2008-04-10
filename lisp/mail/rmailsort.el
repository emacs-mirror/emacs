;;; rmailsort.el --- Rmail: sort messages

;; Copyright (C) 1990, 1993, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Maintainer: FSF
;; Keywords: mail

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

;;; Code:

(require 'sort)

;; For rmail-select-summary
(require 'rmail)

(autoload 'timezone-make-date-sortable "timezone")

;; Sorting messages in Rmail buffer

;;;###autoload
(defun rmail-sort-by-date (reverse)
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (rmail-make-date-sortable
			   (rmail-fetch-field msg "Date"))))))

;;;###autoload
(defun rmail-sort-by-subject (reverse)
  "Sort messages of current Rmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (let ((key (or (rmail-fetch-field msg "Subject") ""))
				(case-fold-search t))
			    ;; Remove `Re:'
			    (if (string-match "^\\(re:[ \t]*\\)*" key)
				(substring key (match-end 0))
			      key))))))

;;;###autoload
(defun rmail-sort-by-author (reverse)
  "Sort messages of current Rmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (downcase	;Canonical name
			   (mail-strip-quoted-names
			    (or (rmail-fetch-field msg "From")
				(rmail-fetch-field msg "Sender") "")))))))

;;;###autoload
(defun rmail-sort-by-recipient (reverse)
  "Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (downcase	;Canonical name
			   (mail-strip-quoted-names
			    (or (rmail-fetch-field msg "To")
				(rmail-fetch-field msg "Apparently-To") "")
			    ))))))

;;;###autoload
(defun rmail-sort-by-correspondent (reverse)
  "Sort messages of current Rmail file by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (rmail-select-correspondent
			   msg
			   '("From" "Sender" "To" "Apparently-To"))))))

(defun rmail-select-correspondent (msg fields)
  (let ((ans ""))
    (while (and fields (string= ans ""))
      (setq ans
	    (rmail-dont-reply-to
	     (mail-strip-quoted-names
	      (or (rmail-fetch-field msg (car fields)) ""))))
      (setq fields (cdr fields)))
    ans))

;;;###autoload
(defun rmail-sort-by-lines (reverse)
  "Sort messages of current Rmail file by number of lines.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (count-lines (rmail-msgbeg msg)
				       (rmail-msgend msg))))))

;;;###autoload
(defun rmail-sort-by-labels (reverse labels)
  "Sort messages of current Rmail file by labels.
If prefix argument REVERSE is non-nil, sort them in reverse order.
KEYWORDS is a comma-separated list of labels."
  (interactive "P\nsSort by labels: ")
  (or (string-match "[^ \t]" labels)
      (error "No labels specified"))
  (setq labels (concat (substring labels (match-beginning 0)) ","))
  (let (labelvec)
    (while (string-match "[ \t]*,[ \t]*" labels)
      (setq labelvec (cons
		      (concat ", ?\\("
			      (substring labels 0 (match-beginning 0))
			      "\\),")
		      labelvec))
      (setq labels (substring labels (match-end 0))))
    (setq labelvec (apply 'vector (nreverse labelvec)))
    (rmail-sort-messages reverse
			 (function
			  (lambda (msg)
			    (let ((n 0))
			      (while (and (< n (length labelvec))
					  (not (rmail-message-labels-p
						msg (aref labelvec n))))
				(setq n (1+ n)))
			      n))))))

;; Basic functions
(declare-function rmail-update-summary "rmailsum" (&rest ignore))

(defun rmail-sort-messages (reverse keyfun)
  "Sort messages of current Rmail file.
If 1st argument REVERSE is non-nil, sort them in reverse order.
2nd argument KEYFUN is called with a message number, and should return a key."
  (save-current-buffer
    ;; If we are in a summary buffer, operate on the Rmail buffer.
    (if (eq major-mode 'rmail-summary-mode)
	(set-buffer rmail-buffer))
    (let ((buffer-read-only nil)
	  (point-offset (- (point) (point-min)))
	  (predicate nil)			;< or string-lessp
	  (sort-lists nil))
      (message "Finding sort keys...")
      (widen)
      (let ((msgnum 1))
	(while (>= rmail-total-messages msgnum)
	  (setq sort-lists
		(cons (list (funcall keyfun msgnum) ;Make sorting key
			    (eq rmail-current-message msgnum) ;True if current
			    (aref rmail-message-vector msgnum)
			    (aref rmail-message-vector (1+ msgnum)))
		      sort-lists))
	  (if (zerop (% msgnum 10))
	      (message "Finding sort keys...%d" msgnum))
	  (setq msgnum (1+ msgnum))))
      (or reverse (setq sort-lists (nreverse sort-lists)))
      ;; Decide predicate: < or string-lessp
      (if (numberp (car (car sort-lists))) ;Is a key numeric?
	  (setq predicate (function <))
	(setq predicate (function string-lessp)))
      (setq sort-lists
	    (sort sort-lists
		  (function
		   (lambda (a b)
		     (funcall predicate (car a) (car b))))))
      (if reverse (setq sort-lists (nreverse sort-lists)))
      ;; Now we enter critical region.  So, keyboard quit is disabled.
      (message "Reordering messages...")
      (let ((inhibit-quit t)		;Inhibit quit
	    (current-message nil)
	    (msgnum 1)
	    (msginfo nil))
	;; There's little hope that we can easily undo after that.
	(buffer-disable-undo (current-buffer))
	(goto-char (rmail-msgbeg 1))
	;; To force update of all markers.
	(insert-before-markers ?Z)
	(backward-char 1)
	;; Now reorder messages.
	(while sort-lists
	  (setq msginfo (car sort-lists))
	  ;; Swap two messages.
	  (insert-buffer-substring
	   (current-buffer) (nth 2 msginfo) (nth 3 msginfo))
	  (delete-region  (nth 2 msginfo) (nth 3 msginfo))
	  ;; Is current message?
	  (if (nth 1 msginfo)
	      (setq current-message msgnum))
	  (setq sort-lists (cdr sort-lists))
	  (if (zerop (% msgnum 10))
	      (message "Reordering messages...%d" msgnum))
	  (setq msgnum (1+ msgnum)))
	;; Delete the garbage inserted before.
	(delete-char 1)
	(setq quit-flag nil)
	(buffer-enable-undo)
	(rmail-set-message-counters)
	(rmail-show-message current-message)
	(goto-char (+ point-offset (point-min)))
	(if (rmail-summary-exists)
	    (rmail-select-summary
	     (rmail-update-summary)))))))

(defun rmail-fetch-field (msg field)
  "Return the value of the header FIELD of MSG.
Arguments are MSG and FIELD."
  (save-restriction
    (widen)
    (let ((next (rmail-msgend msg)))
      (goto-char (rmail-msgbeg msg))
      (narrow-to-region (if (search-forward "\n*** EOOH ***\n" next t)
			    (point)
			  (forward-line 1)
			  (point))
			(progn (search-forward "\n\n" nil t) (point)))
      (mail-fetch-field field))))

(defun rmail-make-date-sortable (date)
  "Make DATE sortable using the function string-lessp."
  ;; Assume the default time zone is GMT.
  (timezone-make-date-sortable date "GMT" "GMT"))

(provide 'rmailsort)

;; arch-tag: 0d90896b-0c35-46ac-b240-38be5ada2360
;;; rmailsort.el ends here
