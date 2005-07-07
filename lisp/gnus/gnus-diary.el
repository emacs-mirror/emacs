;;; gnus-diary.el --- Wrapper around the NNDiary Gnus backend

;; Copyright (c) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
;; Copyright (C) 1999, 2000, 2001 Didier Verna.

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Tue Jul 20 10:42:55 1999
;; Keywords:      calendar mail news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; Description:
;; ===========

;; Gnus-Diary is a wrapper around the NNDiary Gnus backend.  It is here to
;; make your nndiary-user life easier in different ways.  So, you don't have
;; to use it if you don't want to.  But, really, you should.

;; Gnus-Diary offers the following features on top of the NNDiary backend:

;;  - A nice summary line format:
;;    Displaying diary messages in standard summary line format (usually
;;    something like "<From Joe>: <Subject>") is pretty useless.  Most of the
;;    time, you're the one who wrote the message, and you mostly want to see
;;    the event's date.  Gnus-Diary offers you a nice summary line format
;;    which will do this.  By default, a summary line will appear like this:
;;
;;     <Event Date>: <Subject> <Remaining time>
;;
;;   for example, here's how Joe's birthday is displayed in my
;;   "nndiary:birhdays" summary buffer (the message is expirable, but will
;;   never be deleted, as it specifies a regular event):
;;
;;   E  Sat, Sep 22 01, 12:00: Joe's birthday (in 6 months, 1 week)

;;  - More article sorting functions:
;;    Gnus-Diary adds a new sorting function called
;;    `gnus-summary-sort-by-schedule'.  This function lets you organize your
;;    diary summary buffers from the closest event to the farthest one.

;;  - Automatic generation of diary group parameters:
;;    When you create a new diary group, or visit one, Gnus-Diary checks your
;;    group parameters, and if needed, sets the summary line format to the
;;    diary-specific value, adds the diary-specific sorting functions, and
;;    also adds the different `X-Diary-*' headers to the group's
;;    posting-style.  It is then easier to send a diary message, because if
;;    you use `C-u a' or `C-u m' on a diary group to prepare a message, these
;;    headers will be inserted automatically (but not filled with proper
;;    values yet).

;;  - An interactive mail-to-diary convertion function:
;;    The function `gnus-diary-check-message' ensures that the current message
;;    contains all the required diary headers, and prompts you for values /
;;    correction if needed.  This function is hooked in the nndiary backend so
;;    that moving an article to an nndiary group will trigger it
;;    automatically.  It is also bound to `C-c D c' in message-mode and
;;    article-edit-mode in order to ease the process of converting a usual
;;    mail to a diary one.  This function takes a prefix argument which will
;;    force prompting of all diary headers, regardless of their
;;    presence/validity.  That way, you can very easily reschedule a diary
;;    message for instance.


;; Usage:
;; =====

;; 0/ Don't use any `gnus-user-format-function-[d|D]'.  Gnus-Diary provides
;;    both of these (sorry if you used them before).
;; 1/ Add '(require 'gnus-diary) to your gnusrc file.
;; 2/ Customize your gnus-diary options to suit your needs.



;; Bugs / Todo:
;; ===========


;;; Code:

(require 'nndiary)
(require 'message)
(require 'gnus-art)

(defgroup gnus-diary nil
  "Utilities on top of the nndiary backend for Gnus."
  :version "22.1"
  :group 'gnus)

(defcustom gnus-diary-summary-line-format "%U%R%z %uD: %(%s%) (%ud)\n"
  "*Summary line format for nndiary groups."
  :type 'string
  :group 'gnus-diary
  :group 'gnus-summary-format)

(defcustom gnus-diary-time-format "%a, %b %e %y, %H:%M"
  "*Time format to display appointements in nndiary summary buffers.
Please refer to `format-time-string' for information on possible values."
  :type 'string
  :group 'gnus-diary)

(defcustom gnus-diary-delay-format-function 'gnus-diary-delay-format-english
  "*Function called to format a diary delay string.
It is passed two arguments.  The first one is non nil if the delay is in
the past.  The second one is of the form ((NUM . UNIT) ...) where NUM is
an integer and UNIT is one of 'year 'month 'week 'day 'hour or 'minute.
It should return strings like \"In 2 months, 3 weeks\", \"3 hours,
1 minute ago\" and so on.

There are currently two built-in format functions:
`gnus-diary-delay-format-english' (the default)
`gnus-diary-delay-format-french'"
  :type '(choice (const  :tag "english" gnus-diary-delay-format-english)
		 (const  :tag "french"  gnus-diary-delay-format-french)
		 (symbol :tag "other"))
  :group 'gnus-diary)

(defconst gnus-diary-version nndiary-version
  "Current Diary backend version.")


;; Compatibility functions ==================================================

(eval-and-compile
  (if (fboundp 'kill-entire-line)
      (defalias 'gnus-diary-kill-entire-line 'kill-entire-line)
    (defun gnus-diary-kill-entire-line ()
      (beginning-of-line)
      (let ((kill-whole-line t))
	(kill-line)))))


;; Summary line format ======================================================

(defun gnus-diary-delay-format-french (past delay)
  (if (null delay)
      "maintenant!"
    ;; Keep only a precision of two degrees
    (and (> (length delay) 1) (setcdr (cdr delay) nil))
    (concat (if past "il y a " "dans ")
	    (let ((str "")
		  del)
	      (while (setq del (pop delay))
		(setq str (concat str
				  (int-to-string (car del)) " "
				  (cond ((eq (cdr del) 'year)
					 "an")
					((eq (cdr del) 'month)
					 "mois")
					((eq (cdr del) 'week)
					 "semaine")
					((eq (cdr del) 'day)
					 "jour")
					((eq (cdr del) 'hour)
					 "heure")
					((eq (cdr del) 'minute)
					 "minute"))
				  (unless (or (eq (cdr del) 'month)
					      (= (car del) 1))
				    "s")
				  (if delay ", "))))
	      str))))


(defun gnus-diary-delay-format-english (past delay)
  (if (null delay)
      "now!"
    ;; Keep only a precision of two degrees
    (and (> (length delay) 1) (setcdr (cdr delay) nil))
    (concat (unless past "in ")
	    (let ((str "")
		  del)
	      (while (setq del (pop delay))
		(setq str (concat str
				  (int-to-string (car del)) " "
				  (symbol-name (cdr del))
				  (and (> (car del) 1) "s")
				  (if delay ", "))))
	      str)
	    (and past " ago"))))


(defun gnus-diary-header-schedule (headers)
  ;; Same as `nndiary-schedule', but given a set of headers HEADERS
  (mapcar
   (lambda (elt)
     (let ((head (cdr (assoc (intern (format "X-Diary-%s" (car elt)))
			     headers))))
       (when head
	 (nndiary-parse-schedule-value head (cadr elt) (car (cddr elt))))))
   nndiary-headers))

;; #### NOTE: Gnus sometimes gives me a HEADER not corresponding to any
;; message, with all fields set to nil here. I don't know what it is for, and
;; I just ignore it.
(defun gnus-user-format-function-d (header)
  ;; Returns an aproximative delay string for the next occurence of this
  ;; message. The delay is given only in the first non zero unit.
  ;; Code partly stolen from article-make-date-line
  (let* ((extras (mail-header-extra header))
	 (sched (gnus-diary-header-schedule extras))
	 (occur (nndiary-next-occurence sched (current-time)))
	 (now (current-time))
	 (real-time (subtract-time occur now)))
    (if (null real-time)
	"?????"
      (let* ((sec (+ (* (float (car real-time)) 65536) (cadr real-time)))
	     (past (< sec 0))
	     delay)
	(and past (setq sec (- sec)))
	(unless (zerop sec)
	  ;; This is a bit convoluted, but basically we go through the time
	  ;; units for years, weeks, etc, and divide things to see whether
	  ;; that results in positive answers.
	  (let ((units `((year . ,(* 365.25 24 3600))
			 (month . ,(* 31 24 3600))
			 (week . ,(* 7 24 3600))
			 (day . ,(* 24 3600))
			 (hour . 3600)
			 (minute . 60)))
		unit num)
	    (while (setq unit (pop units))
	      (unless (zerop (setq num (ffloor (/ sec (cdr unit)))))
		(setq delay (append delay `((,(floor num) . ,(car unit))))))
	      (setq sec (- sec (* num (cdr unit)))))))
	(funcall gnus-diary-delay-format-function past delay)))
    ))

;; #### NOTE: Gnus sometimes gives me a HEADER not corresponding to any
;; message, with all fields set to nil here. I don't know what it is for, and
;; I just ignore it.
(defun gnus-user-format-function-D (header)
  ;; Returns a formatted time string for the next occurence of this message.
  (let* ((extras (mail-header-extra header))
	 (sched (gnus-diary-header-schedule extras))
	 (occur (nndiary-next-occurence sched (current-time))))
    (format-time-string gnus-diary-time-format occur)))


;; Article sorting functions ================================================

(defun gnus-article-sort-by-schedule (h1 h2)
  (let* ((now (current-time))
	 (e1 (mail-header-extra h1))
	 (e2 (mail-header-extra h2))
	 (s1 (gnus-diary-header-schedule e1))
	 (s2 (gnus-diary-header-schedule e2))
	 (o1 (nndiary-next-occurence s1 now))
	 (o2 (nndiary-next-occurence s2 now)))
    (if (and (= (car o1) (car o2)) (= (cadr o1) (cadr o2)))
	(< (mail-header-number h1) (mail-header-number h2))
      (time-less-p o1 o2))))


(defun gnus-thread-sort-by-schedule (h1 h2)
  (gnus-article-sort-by-schedule (gnus-thread-header h1)
				 (gnus-thread-header h2)))

(defun gnus-summary-sort-by-schedule (&optional reverse)
  "Sort nndiary summary buffers by schedule of appointements.
Optional prefix (or REVERSE argument) means sort in reverse order."
  (interactive "P")
  (gnus-summary-sort 'schedule reverse))

(defvar gnus-summary-misc-menu) ;; Avoid byte compiler warning.
(add-hook 'gnus-summary-menu-hook
	  (lambda ()
	    (easy-menu-add-item gnus-summary-misc-menu
				'("Sort")
				["Sort by schedule"
				 gnus-summary-sort-by-schedule
				 (eq (car (gnus-find-method-for-group
					   gnus-newsgroup-name))
				     'nndiary)]
				"Sort by number")))



;; Group parameters autosetting =============================================

(defun gnus-diary-update-group-parameters (group)
  ;; Ensure that nndiary groups have convenient group parameters:
  ;; - a posting style containing X-Diary headers
  ;; - a nice summary line format
  ;; - NNDiary specific sorting by schedule functions
  ;; In general, try not to mess with what the user might have modified.
  (let ((posting-style (gnus-group-get-parameter group 'posting-style t)))
    ;; Posting style:
    (mapcar (lambda (elt)
	      (let ((header (format "X-Diary-%s" (car elt))))
		(unless (assoc header posting-style)
		  (setq posting-style (append posting-style
					      `((,header "*")))))
		))
	    nndiary-headers)
    (gnus-group-set-parameter group 'posting-style posting-style)
    ;; Summary line format:
    (unless (gnus-group-get-parameter group 'gnus-summary-line-format t)
      (gnus-group-set-parameter group 'gnus-summary-line-format
				`(,gnus-diary-summary-line-format)))
    ;; Sorting by schedule:
    (unless (gnus-group-get-parameter group 'gnus-article-sort-functions)
      (gnus-group-set-parameter group 'gnus-article-sort-functions
				'((append gnus-article-sort-functions
					  (list
					   'gnus-article-sort-by-schedule)))))
    (unless (gnus-group-get-parameter group 'gnus-thread-sort-functions)
      (gnus-group-set-parameter group 'gnus-thread-sort-functions
				'((append gnus-thread-sort-functions
					  (list
					   'gnus-thread-sort-by-schedule)))))
    ))

;; Called when a group is subscribed. This is needed because groups created
;; because of mail splitting are *not* created with the backend function.
;; Thus, `nndiary-request-create-group-hooks' is inoperative.
(defun gnus-diary-maybe-update-group-parameters (group)
  (when (eq (car (gnus-find-method-for-group group)) 'nndiary)
    (gnus-diary-update-group-parameters group)))

(add-hook 'nndiary-request-create-group-hooks
	  'gnus-diary-update-group-parameters)
;; Now that we have `gnus-subscribe-newsgroup-hooks', this is not needed
;; anymore. Maybe I should remove this completely.
(add-hook 'nndiary-request-update-info-hooks
	  'gnus-diary-update-group-parameters)
(add-hook 'gnus-subscribe-newsgroup-hooks
	  'gnus-diary-maybe-update-group-parameters)


;; Diary Message Checking ===================================================

(defvar gnus-diary-header-value-history nil
  ;; History variable for header value prompting
  )

(defun gnus-diary-narrow-to-headers ()
  "Narrow the current buffer to the header part.
Point is left at the beginning of the region.
The buffer is assumed to contain a message, but the format is unknown."
  (cond ((eq major-mode 'message-mode)
	 (message-narrow-to-headers))
	(t
	 (goto-char (point-min))
	 (when (search-forward "\n\n" nil t)
	   (narrow-to-region (point-min) (- (point) 1))
	   (goto-char (point-min))))
	))

(defun gnus-diary-add-header (str)
  "Add a header to the current buffer.
The buffer is assumed to contain a message, but the format is unknown."
  (cond ((eq major-mode 'message-mode)
	 (message-add-header str))
	(t
	 (save-restriction
	   (gnus-diary-narrow-to-headers)
	   (goto-char (point-max))
	   (if (string-match "\n$" str)
	       (insert str)
	     (insert str ?\n))))
	))

(defun gnus-diary-check-message (arg)
  "Ensure that the current message is a valid for NNDiary.
This function checks that all NNDiary required headers are present and
valid, and prompts for values / correction otherwise.

If ARG (or prefix) is non-nil, force prompting for all fields."
  (interactive "P")
  (save-excursion
    (mapcar
     (lambda (head)
       (let ((header (concat "X-Diary-" (car head)))
	     (ask arg)
	     value invalid)
	 ;; First, try to find the header, and checks for validity:
	 (save-restriction
	   (gnus-diary-narrow-to-headers)
	   (when (re-search-forward (concat "^" header ":") nil t)
	     (unless (eq (char-after) ? )
	       (insert " "))
	     (setq value (buffer-substring (point) (gnus-point-at-eol)))
	     (and (string-match "[ \t]*\\([^ \t]+\\)[ \t]*" value)
		  (setq value (match-string 1 value)))
	     (condition-case ()
		 (nndiary-parse-schedule-value value
					       (nth 1 head) (nth 2 head))
	       (t
		(setq invalid t)))
	     ;; #### NOTE: this (along with the `gnus-diary-add-header'
	     ;; function) could be rewritten in a better way, in particular
	     ;; not to blindly remove an already present header and reinsert
	     ;; it somewhere else afterwards.
	     (when (or ask invalid)
	       (gnus-diary-kill-entire-line))
	     ))
	 ;; Now, loop until a valid value is provided:
	 (while (or ask (not value) invalid)
	   (let ((prompt (concat (and invalid
				      (prog1 "(current value invalid) "
					(beep)))
				 header ": ")))
	     (setq value
		   (if (listp (nth 1 head))
		       (completing-read prompt (cons '("*" nil) (nth 1 head))
					nil t value
					gnus-diary-header-value-history)
		     (read-string prompt value
				  gnus-diary-header-value-history))))
	   (setq ask nil)
	   (setq invalid nil)
	   (condition-case ()
	       (nndiary-parse-schedule-value value
					     (nth 1 head) (nth 2 head))
	     (t
	      (setq invalid t))))
	 (gnus-diary-add-header (concat header ": " value))
	 ))
     nndiary-headers)
    ))

(add-hook 'nndiary-request-accept-article-hooks
	  (lambda () (gnus-diary-check-message nil)))

(define-key message-mode-map "\C-cDc" 'gnus-diary-check-message)
(define-key gnus-article-edit-mode-map "\C-cDc" 'gnus-diary-check-message)


;; The end ==================================================================

(defun gnus-diary-version ()
  "Current Diary backend version."
  (interactive)
  (message "NNDiary version %s" nndiary-version))

(define-key message-mode-map "\C-cDv" 'gnus-diary-version)
(define-key gnus-article-edit-mode-map "\C-cDv" 'gnus-diary-version)


(provide 'gnus-diary)

;;; arch-tag: 98467e70-337e-4ddc-b92d-45d403ff1b4b
;;; gnus-diary.el ends here
