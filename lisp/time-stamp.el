;;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs
;;; Copyright 1989, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: Stephen Gildea <gildea@lcs.mit.edu>
;; Time-stamp: <94/02/14 15:02:07 gildea>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; If you put a time stamp template anywhere in the first 8 lines of a file,
;;; it can be updated every time you save the file.  See the top of
;;; time-stamp.el for a sample.  The template looks like one of the following:
;;;     Time-stamp: <>
;;;     Time-stamp: " "
;;; The time stamp is written between the brackets or quotes, resulting in
;;;     Time-stamp: <93/06/18 10:26:51 gildea>
;;; Here is an example which puts the file name and time stamp in the binary:
;;; static char *time_stamp = "sdmain.c Time-stamp: <>";

;;; To activate automatic time stamping, add this code to your .emacs file:
;;;
;;; (if (not (memq 'time-stamp write-file-hooks))
;;;     (setq write-file-hooks
;;;           (cons 'time-stamp write-file-hooks)))
;;;
;;; In Emacs 18 you will also need
;;; (autoload 'time-stamp "time-stamp" "Update the time stamp in a buffer." t)

;;; Change Log:

;;; Originally based on the 19 Dec 88 version of
;;;   date.el by John Sturdy <mcvax!harlqn.co.uk!jcgs@uunet.uu.net>

;;; Code:

(defvar time-stamp-active t
  "*Non-nil to enable time-stamping of files.  See the function time-stamp.")

(defvar time-stamp-format
  '(time-stamp-yy/mm/dd time-stamp-hh:mm:ss user-login-name)
  "*A list of functions to call to generate the time stamp string.
Each element of the list is called as a function and the results are
concatenated together separated by spaces.  Elements may also be strings,
which are included verbatim.  Spaces are not inserted around literal strings.")

;;; Do not change time-stamp-line-limit, time-stamp-start, or
;;; time-stamp-end in your .emacs or you will be incompatible
;;; with other people's files!  If you must change them,
;;; do so only in the local variables section of the file itself.

(defvar time-stamp-line-limit 8	    ;Do not change!
  "Number of lines at the beginning of a file that are searched.
The patterns `time-stamp-start' and `time-stamp-end' must be found on one
of the first `time-stamp-line-limit' lines of the file for the file to
be time-stamped by \\[time-stamp].

Do not change `time-stamp-line-limit', `time-stamp-start', or
`time-stamp-end' for yourself or you will be incompatible
with other people's files!  If you must change them for some application,
do so in the local variables section of the time-stamped file itself.")


(defvar time-stamp-start "Time-stamp:[ \t]+\\\\?[\"<]+"    ;Do not change!
  "Regexp after which the time stamp is written by \\[time-stamp].
See also the variables `time-stamp-end' and `time-stamp-line-limit'.

Do not change `time-stamp-line-limit', `time-stamp-start', or
`time-stamp-end' for yourself or you will be incompatible
with other people's files!  If you must change them for some application,
do so in the local variables section of the time-stamped file itself.")


(defvar time-stamp-end "\\\\?[\">]"    ;Do not change!
  "Regexp marking the text after the time stamp.
\\[time-stamp] deletes the text between the first match of `time-stamp-start'
and the following match of `time-stamp-end' on the same line,
then writes the time stamp specified by `time-stamp-format' between them.

Do not change `time-stamp-line-limit', `time-stamp-start', or
`time-stamp-end' for yourself or you will be incompatible
with other people's files!  If you must change them for some application,
do so in the local variables section of the time-stamped file itself.")


;;;###autoload
(defun time-stamp ()
  "Update the time stamp string in the buffer.
If you put a time stamp template anywhere in the first 8 lines of a file,
it can be updated every time you save the file.  See the top of
`time-stamp.el' for a sample.  The template looks like one of the following:
    Time-stamp: <>
    Time-stamp: \" \"
The time stamp is written between the brackets or quotes, resulting in
    Time-stamp: <93/06/18 10:26:51 gildea>
Only does its thing if the variable  time-stamp-active  is non-nil.
Typically used on  write-file-hooks  for automatic time-stamping.
The format of the time stamp is determined by the variable  time-stamp-format.
The variables time-stamp-line-limit, time-stamp-start, and time-stamp-end
control finding the template."
  (interactive)
  (if time-stamp-active
       (let ((case-fold-search nil))
	 (if (and (stringp time-stamp-start)
		  (stringp time-stamp-end))
	     (save-excursion
	       (goto-char (point-min))
	       (if (re-search-forward time-stamp-start
				      (save-excursion
					(forward-line time-stamp-line-limit)
					(point))
				      t)
		   (let ((start (point)))
		     (if (re-search-forward time-stamp-end
					    (save-excursion (end-of-line) (point))
					    t)
			 (let ((end (match-beginning 0)))
			   (delete-region start end)
			   (goto-char start)
			   (insert (time-stamp-string))
			   (setq end (point))
			   ;; remove any tabs used to format the time stamp
			   (goto-char start)
			   (if (search-forward "\t" end t)
			       (untabify start end)))))))
	   ;; don't signal an error in a write-file-hook
	   (message "time-stamp-start or time-stamp-end is not a string"))))
  ;; be sure to return nil so can be used on write-file-hooks
  nil)

(defun time-stamp-string ()
  "Generate the new string to be inserted by \\[time-stamp]."
  (time-stamp-fconcat time-stamp-format " "))

(defun time-stamp-fconcat (list sep)
  "Similar to (mapconcat 'funcall LIST SEP) but LIST allows literals.
If an element of LIST is a symbol, it is funcalled to get the string to use;
the separator SEP is used between two strings obtained by funcalling a
symbol.  Otherwise the element itself is inserted; no separator is used
around literals."
  (let ((return-string "")
	(insert-sep-p nil))
    (while list
      (cond ((symbolp (car list))
	     (if insert-sep-p
		 (setq return-string (concat return-string sep)))
	     (setq return-string (concat return-string (funcall (car list))))
	     (setq insert-sep-p t))
	    (t
	     (setq return-string (concat return-string (car list)))
	     (setq insert-sep-p nil)))
      (setq list (cdr list)))
    return-string))


(defconst time-stamp-month-numbers
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  "Alist of months and their number.")

(defconst time-stamp-month-full-names
  ["(zero)" "January" "February" "March" "April" "May" "June"
   "July" "August" "September" "October" "November" "December"])

(defvar time-stamp-mail-host nil
  "*Name of the host where the user receives mail.
See the function `time-stamp-mail-host-name'.")

;;; Some useful functions to use in time-stamp-format

;;; Could generate most of a message-id with
;;; '(time-stamp-yymmdd "" time-stamp-hhmm "@" time-stamp-mail-host-name)

(defun time-stamp-mail-host-name ()
  "Return the name of the host where the user receives mail.
This is the value of `time-stamp-mail-host' if bound and a string,
otherwise the value of the function system-name.
This function may be usefully referenced by `time-stamp-format'."
  (or (and (boundp 'time-stamp-mail-host)
	   (stringp time-stamp-mail-host)
	   time-stamp-mail-host)
      (system-name)))

;;; pretty form, suitable for a title page

(defun time-stamp-month-dd-yyyy ()
  "Return the current date as a string in \"Month DD, YYYY\" form."
  (let ((date (current-time-string)))
    (format "%s %d, %s"
	    (aref time-stamp-month-full-names
		  (cdr (assoc (substring date 4 7) time-stamp-month-numbers)))
	    (string-to-int (substring date 8 10))
	    (substring date -4))))

;;; same as __DATE__ in ANSI C

(defun time-stamp-mon-dd-yyyy ()
  "Return the current date as a string in \"Mon DD YYYY\" form.
The first character of DD is space if the value is less than 10."
  (let ((date (current-time-string)))
    (format "%s %2d %s"
	    (substring date 4 7)
	    (string-to-int (substring date 8 10))
	    (substring date -4))))

;;; RFC 822 date

(defun time-stamp-dd-mon-yy ()
  "Return the current date as a string in \"DD Mon YY\" form."
  (let ((date (current-time-string)))
    (format "%02d %s %s"
	    (string-to-int (substring date 8 10))
	    (substring date 4 7)
	    (substring date -2))))

;;; RCS 3 date

(defun time-stamp-yy/mm/dd ()
  "Return the current date as a string in \"YY/MM/DD\" form."
  (let ((date (current-time-string)))
    (format "%s/%02d/%02d"
	    (substring date -2)
	    (cdr (assoc (substring date 4 7) time-stamp-month-numbers))
	    (string-to-int (substring date 8 10)))))

;;; RCS 5 date

(defun time-stamp-yyyy/mm/dd ()
  "Return the current date as a string in \"YYYY/MM/DD\" form."
  (let ((date (current-time-string)))
    (format "%s/%02d/%02d"
	    (substring date -4)
	    (cdr (assoc (substring date 4 7) time-stamp-month-numbers))
	    (string-to-int (substring date 8 10)))))

;;; ISO 8601 date

(defun time-stamp-yyyy-mm-dd ()
  "Return the current date as a string in \"YYYY-MM-DD\" form."
  (let ((date (current-time-string)))
    (format "%s-%02d-%02d"
	    (substring date -4)
	    (cdr (assoc (substring date 4 7) time-stamp-month-numbers))
	    (string-to-int (substring date 8 10)))))

(defun time-stamp-yymmdd ()
  "Return the current date as a string in \"YYMMDD\" form."
  (let ((date (current-time-string)))
    (format "%s%02d%02d"
	    (substring date -2)
	    (cdr (assoc (substring date 4 7) time-stamp-month-numbers))
	    (string-to-int (substring date 8 10)))))

(defun time-stamp-hh:mm:ss ()
  "Return the current time as a string in \"HH:MM:SS\" form."
  (substring (current-time-string) 11 19))

(defun time-stamp-hhmm ()
  "Return the current time as a string in \"HHMM\" form."
  (let ((date (current-time-string)))
    (concat (substring date 11 13)
	    (substring date 14 16))))

(provide 'time-stamp)

;;; time-stamp.el ends here

