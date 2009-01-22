;;; pmailsort.el --- Pmail: sort messages

;; Copyright (C) 1990, 1993, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
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

(eval-when-compile
  (require 'mail-utils)
  (require 'sort)
  (require 'pmail))

(autoload 'timezone-make-date-sortable "timezone")

(declare-function pmail-update-summary "pmailsum" (&rest ignore))

;; Sorting messages in Pmail buffer

;;;###autoload
(defun pmail-sort-by-date (reverse)
  "Sort messages of current Pmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (pmail-make-date-sortable
			   (pmail-get-header "Date" msg))))))

;;;###autoload
(defun pmail-sort-by-subject (reverse)
  "Sort messages of current Pmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (let ((key (or (pmail-get-header "Subject" msg) ""))
				(case-fold-search t))
			    ;; Remove `Re:'
			    (if (string-match "^\\(re:[ \t]*\\)*" key)
				(substring key (match-end 0))
			      key))))))

;;;###autoload
(defun pmail-sort-by-author (reverse)
  "Sort messages of current Pmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (downcase	;Canonical name
			   (mail-strip-quoted-names
			    (or (pmail-get-header "From" msg)
				(pmail-get-header "Sender" msg) "")))))))

;;;###autoload
(defun pmail-sort-by-recipient (reverse)
  "Sort messages of current Pmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (downcase	;Canonical name
			   (mail-strip-quoted-names
			    (or (pmail-get-header "To" msg)
				(pmail-get-header "Apparently-To" msg) "")
			    ))))))

;;;###autoload
(defun pmail-sort-by-correspondent (reverse)
  "Sort messages of current Pmail file by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (pmail-select-correspondent
			   msg
			   '("From" "Sender" "To" "Apparently-To"))))))

(defun pmail-select-correspondent (msg fields)
  (let ((ans ""))
    (while (and fields (string= ans ""))
      (setq ans
	    ;; NB despite the name, this lives in mail-utils.el.
	    (rmail-dont-reply-to
	     (mail-strip-quoted-names
	      (or (pmail-get-header (car fields) msg) ""))))
      (setq fields (cdr fields)))
    ans))

;;;###autoload
(defun pmail-sort-by-lines (reverse)
  "Sort messages of current Pmail file by number of lines.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (pmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (count-lines (pmail-msgbeg msg)
				       (pmail-msgend msg))))))

;;;###autoload
(defun pmail-sort-by-labels (reverse labels)
  "Sort messages of current Pmail file by labels.
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
    (pmail-sort-messages reverse
			 (function
			  (lambda (msg)
			    (let ((n 0))
			      (while (and (< n (length labelvec))
					  (not (pmail-message-labels-p
						msg (aref labelvec n))))
				(setq n (1+ n)))
			      n))))))

;; Basic functions

(defun pmail-sort-messages (reverse keyfun)
  "Sort messages of current Pmail file.
If 1st argument REVERSE is non-nil, sort them in reverse order.
2nd argument KEYFUN is called with a message number, and should return a key."
  (with-current-buffer pmail-buffer
    (let ((return-to-point
	   (if (pmail-buffers-swapped-p)
	       (point)))
	  (predicate nil)			;< or string-lessp
	  (sort-lists nil))
      (pmail-swap-buffers-maybe)
      (message "Finding sort keys...")
      (widen)
      (let ((msgnum 1))
	(while (>= pmail-total-messages msgnum)
	  (setq sort-lists
		(cons (list (funcall keyfun msgnum) ;Make sorting key
			    (eq pmail-current-message msgnum) ;True if current
			    (aref pmail-message-vector msgnum)
			    (aref pmail-message-vector (1+ msgnum)))
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
	    (inhibit-read-only t)
	    (current-message nil)
	    (msgnum 1)
	    (msginfo nil))
	;; There's little hope that we can easily undo after that.
	(buffer-disable-undo (current-buffer))
	(goto-char (pmail-msgbeg 1))
	;; To force update of all markers,
	;; keep the new copies separated from the remaining old messages.
	(insert-before-markers ?Z)
	(backward-char 1)
	;; Now reorder messages.
	(dolist (msginfo sort-lists)
	  ;; Swap two messages.
	  (insert-buffer-substring
	   (current-buffer) (nth 2 msginfo) (nth 3 msginfo))
	  ;; The last message may not have \n\n after it.
	  (unless (bobp)
	    (insert "\n"))
	  (unless (looking-back "\n\n")
	    (insert "\n"))
	  (delete-region (nth 2 msginfo) (nth 3 msginfo))
	  ;; Is current message?
	  (if (nth 1 msginfo)
	      (setq current-message msgnum))
	  (if (zerop (% msgnum 10))
	      (message "Reordering messages...%d" msgnum))
	  (setq msgnum (1+ msgnum)))
	;; Delete the dummy separator Z inserted before.
	(delete-char 1)
	(setq quit-flag nil)
	(pmail-set-message-counters)
	(pmail-show-message current-message)
	(if return-to-point
	    (goto-char return-to-point))
	(if (pmail-summary-exists)
	    (pmail-select-summary (pmail-update-summary)))))))

(defun pmail-make-date-sortable (date)
  "Make DATE sortable using the function string-lessp."
  ;; Assume the default time zone is GMT.
  (timezone-make-date-sortable date "GMT" "GMT"))

(provide 'pmailsort)

;; Local Variables:
;; change-log-default-name: "ChangeLog.pmail"
;; End:

;; arch-tag: 665da245-f6a7-4115-ad8c-ba19216988d5
;;; pmailsort.el ends here
