;;; rmailsort.el --- Rmail: sort messages.

;; Copyright (C) 1990, 1993 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Version: $Header: rmailsort.el,v 1.6 93/05/26 22:24:42 umerin Exp $
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

;; LCD Archive Entry:
;; rmailsort|Masanobu UMEDA|umerin@mse.kyutech.ac.jp|
;; Rmail: sort messages.|
;; $Date: 93/05/26 22:24:42 $|$Revision: 1.6 $|~/misc/rmailsort.el.Z|

;;; Code:

(require 'rmail)
(require 'sort)

(autoload 'timezone-make-date-sortable "timezone")

;; GNUS compatible key bindings.

(define-key rmail-mode-map "\C-c\C-s\C-d" 'rmail-sort-by-date)
(define-key rmail-mode-map "\C-c\C-s\C-s" 'rmail-sort-by-subject)
(define-key rmail-mode-map "\C-c\C-s\C-a" 'rmail-sort-by-author)
(define-key rmail-mode-map "\C-c\C-s\C-r" 'rmail-sort-by-recipient)
(define-key rmail-mode-map "\C-c\C-s\C-c" 'rmail-sort-by-correspondent)
(define-key rmail-mode-map "\C-c\C-s\C-l" 'rmail-sort-by-lines)

;; Key binding may not be installed unless Rmail Summary mode is loaded.
(if (boundp 'rmail-summary-mode-map)
    (progn
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-d" 'rmail-summary-sort-by-date)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-s" 'rmail-summary-sort-by-subject)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-a" 'rmail-summary-sort-by-author)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-r" 'rmail-summary-sort-by-recipient)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-c" 'rmail-summary-sort-by-correspondent)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-l" 'rmail-summary-sort-by-lines)
      ))


;; Sorting messages in Rmail buffer

(defun rmail-sort-by-date (reverse)
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (rmail-make-date-sortable
			   (rmail-fetch-field msg "Date"))))))

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
			    (if (string-match "^\\(re:[ \t]+\\)*" key)
				(substring key (match-end 0)) key))))))

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

(defun rmail-sort-by-lines (reverse)
  "Sort messages of current Rmail file by lines of the message.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (count-lines (rmail-msgbeg msgnum)
				       (rmail-msgend msgnum))))))

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


;; Basic functions

(defun rmail-sort-messages (reverse keyfun)
  "Sort messages of current Rmail file.
If 1st argument REVERSE is non-nil, sort them in reverse order.
2nd argument KEYFUN is called with a message number, and should return a key."
  (let ((buffer-read-only nil)
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
      (buffer-flush-undo (current-buffer))
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
      (rmail-show-message current-message))
    ))

(defun rmail-sort-from-summary (sortfun reverse)
  "Sort Rmail messages from Summary buffer and update it after sorting."
  (pop-to-buffer rmail-buffer)
  (funcall sortfun reverse)
  (rmail-summary))

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

;; Copy of the function gnus-comparable-date in gnus.el version 3.13
;
;(defun rmail-make-date-sortable (date)
;  "Make sortable string by string-lessp from DATE."
;  (let ((month '(("JAN" . " 1")("FEB" . " 2")("MAR" . " 3")
;		 ("APR" . " 4")("MAY" . " 5")("JUN" . " 6")
;		 ("JUL" . " 7")("AUG" . " 8")("SEP" . " 9")
;		 ("OCT" . "10")("NOV" . "11")("DEC" . "12")))
;	(date (or date "")))
;    ;; Can understand the following styles:
;    ;; (1) 14 Apr 89 03:20:12 GMT
;    ;; (2) Fri, 17 Mar 89 4:01:33 GMT
;    (if (string-match
;	 "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\) \\([0-9:]+\\)" date)
;	(concat
;	 ;; Year
;	 (substring date (match-beginning 3) (match-end 3))
;	 ;; Month
;	 (cdr
;	  (assoc
;	   (upcase (substring date (match-beginning 2) (match-end 2))) month))
;	 ;; Day
;	 (format "%2d" (string-to-int
;			(substring date
;				   (match-beginning 1) (match-end 1))))
;	 ;; Time
;	 (substring date (match-beginning 4) (match-end 4)))
;      ;; Cannot understand DATE string.
;      date
;      )
;    ))

(provide 'rmailsort)

;;; rmailsort.el ends here
