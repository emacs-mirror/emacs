;;; rmailsort.el --- Rmail: sort messages.

;; Copyright (C) 1990 Free Software Foundation, Inc.

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

(require 'rmail)
(require 'sort)

;; GNUS compatible key bindings.
(define-key rmail-mode-map "\C-c\C-s\C-d" 'rmail-sort-by-date)
(define-key rmail-mode-map "\C-c\C-s\C-s" 'rmail-sort-by-subject)
(define-key rmail-mode-map "\C-c\C-s\C-a" 'rmail-sort-by-author)
(define-key rmail-mode-map "\C-c\C-s\C-r" 'rmail-sort-by-recipient)
(define-key rmail-mode-map "\C-c\C-s\C-c" 'rmail-sort-by-correspondent)
(define-key rmail-mode-map "\C-c\C-s\C-l" 'rmail-sort-by-size-lines)

(defun rmail-sort-by-date (reverse)
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (rmail-sortable-date-string
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
			  (mail-strip-quoted-names
			   (or (rmail-fetch-field msg "From")
			       (rmail-fetch-field msg "Sender") ""))))))

(defun rmail-sort-by-recipient (reverse)
  "Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (mail-strip-quoted-names
			   (or (rmail-fetch-field msg "To")
			       (rmail-fetch-field msg "Apparently-To") "")
			   )))))

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

(defun rmail-sort-by-size-lines (reverse)
  "Sort messages of current Rmail file by message size.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (format "%9d"
				  (count-lines (rmail-msgbeg msgnum)
					       (rmail-msgend msgnum)))))))


(defun rmail-sort-messages (reverse keyfunc)
  "Sort messages of current Rmail file.
1st argument REVERSE is non-nil, sort them in reverse order.
2nd argument KEYFUNC is called with message number, and should return a key."
 (or (eq major-mode 'rmail-mode)
     (error "Current buffer not in Rmail mode"))
  (let ((buffer-read-only nil)
	(sort-lists nil))
    (message "Finding sort keys...")
    (widen)
    (let ((msgnum 1))
      (while (>= rmail-total-messages msgnum)
	(setq sort-lists
	      (cons (cons (funcall keyfunc msgnum) ;A sort key.
			  (buffer-substring
			   (rmail-msgbeg msgnum) (rmail-msgend msgnum)))
		    sort-lists))
	(if (zerop (% msgnum 10))
	    (message "Finding sort keys...%d" msgnum))
	(setq msgnum (1+ msgnum))))
    (or reverse (setq sort-lists (nreverse sort-lists)))
    (setq sort-lists
	  (sort sort-lists
		(function
		 (lambda (a b)
		   (string-lessp (car a) (car b))))))
    (if reverse (setq sort-lists (nreverse sort-lists)))
    (message "Reordering buffer...")
    (delete-region (rmail-msgbeg 1) (rmail-msgend rmail-total-messages))
    (let ((msgnum 1))
      (while sort-lists
	(insert (cdr (car sort-lists)))
	(if (zerop (% msgnum 10))
	    (message "Reordering buffer...%d" msgnum))
	(setq sort-lists (cdr sort-lists))
	(setq msgnum (1+ msgnum))))
    (rmail-set-message-counters)
    (rmail-show-message 1)))

(defun rmail-fetch-field (msg field)
  "Return the value of the header field FIELD of MSG.
Arguments are MSG and FIELD."
  (let ((next (rmail-msgend msg)))
    (save-restriction
      (goto-char (rmail-msgbeg msg))
      (narrow-to-region (if (search-forward "\n*** EOOH ***\n" next t)
			    (point)
			  (forward-line 1)
			  (point))
			(progn (search-forward "\n\n" nil t) (point)))
      (mail-fetch-field field))))

;; Copy of the function gnus-comparable-date in gnus.el

(defun rmail-sortable-date-string (date)
  "Make sortable string by string-lessp from DATE."
  (let ((month '(("JAN" . " 1")("FEB" . " 2")("MAR" . " 3")
		 ("APR" . " 4")("MAY" . " 5")("JUN" . " 6")
		 ("JUL" . " 7")("AUG" . " 8")("SEP" . " 9")
		 ("OCT" . "10")("NOV" . "11")("DEC" . "12")
		 ("JANUARY" . " 1") ("FEBRUARY" . " 2")
		 ("MARCH" . " 3")   ("APRIL" . " 4")
		 ("MAY" . " 5")     ("JUNE" . " 6")
		 ("JULY" . " 7")    ("AUGUST" . " 8")
		 ("SEPTEMBER" " 9") ("OCTOBER" . "10")
		 ("NOVEMBER" "11")  ("DECEMBER" . "12")))
	(date (or date "")))
    ;; Can understand the following styles:
    ;; (1) 14 Apr 89 03:20:12 GMT
    ;; (2) Fri, 17 Mar 89 4:01:33 GMT
    (if (string-match
	 "\\([0-9]+\\) +\\([^ ,]+\\) +\\([0-9]+\\) +\\([0-9:]+\\)" date)
	(concat
	 ;; Year
	 (rmail-date-full-year
	  (substring date (match-beginning 3) (match-end 3)))
	 ;; Month
	 (cdr
	  (assoc
	   (upcase (substring date (match-beginning 2) (match-end 2))) month))
	 ;; Day
	 (format "%2d" (string-to-int
			(substring date
				   (match-beginning 1) (match-end 1))))
	 ;; Time
	 (substring date (match-beginning 4) (match-end 4)))

      ;; Handles this format Fri May 10 21:51:55 1991
      (if (string-match
         " \\([a-z][a-z][a-z]\\) +\\([0-9]+\\) \\([0-9:]+\\) \\([0-9]+\\)" date)
	  (concat
	   ;; Year
	   (rmail-date-full-year 
	    (substring date (match-beginning 4) (match-end 4)))
	   ;; Month
	   (cdr
	    (assoc
	     (upcase (substring date (match-beginning 1) (match-end 1))) month))
	   ;; Day
	   (format "%2d" (string-to-int
			  (substring date
				     (match-beginning 2) (match-end 2))))
	   ;; Time
	   (substring date (match-beginning 3) (match-end 3)))

      ;; Cannot understand DATE string.
      date))))

(defun rmail-date-full-year (year-string)
  (if (<= (length year-string) 2)
      (concat "19" year-string)
    year-string))

(provide 'rmailsort)

;;; rmailsort.el ends here
