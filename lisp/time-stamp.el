;;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs
;;; Copyright 1989, 1993, 1994, 1995 Free Software Foundation, Inc.

;; Maintainer: Stephen Gildea <gildea@lcs.mit.edu>
;; Gildea's Last Time-stamp: <95/04/13 13:38:48 gildea>
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
;;;     Time-stamp: <95/01/18 10:20:51 gildea>
;;; Here is an example which puts the file name and time stamp in the binary:
;;; static char *time_stamp = "sdmain.c Time-stamp: <>";

;;; To activate automatic time stamping in GNU Emacs 19, add this code
;;; to your .emacs file:
;;; (add-hook 'write-file-hooks 'time-stamp)
;;;
;;; In Emacs 18 you will need to do this instead:
;;; (if (not (memq 'time-stamp write-file-hooks))
;;;     (setq write-file-hooks
;;;           (cons 'time-stamp write-file-hooks)))
;;; (autoload 'time-stamp "time-stamp" "Update the time stamp in a buffer." t)

;;; See the documentation for the function `time-stamp' for more details.

;;; Change Log:

;;; Originally based on the 19 Dec 88 version of
;;;   date.el by John Sturdy <mcvax!harlqn.co.uk!jcgs@uunet.uu.net>
;;; version 2, January 1995: replaced functions with %-escapes

;;; Code:

(defvar time-stamp-active t
  "*Non-nil to enable time-stamping of files.
Can be toggled by \\[time-stamp-toggle-active].
See also the variable time-stamp-warn-inactive.")

(defvar time-stamp-warn-inactive t
  "*Non-nil to have time-stamp warn if time-stamp-active is nil.")

(defvar time-stamp-format "%02y/%02m/%02d %02H:%02M:%02S %u"
  "*Template for the string inserted by the time-stamp function.
Value may be a string or a list.  (Lists are supported only for
backward compatibility.)  A string is used verbatim except for character
sequences beginning with %.  See the documentation for the function
time-stamp-strftime for a list of %-escapes.
     Each element of a list is called as a function and the results are
concatenated together separated by spaces.  List elements may also be
strings, which are included verbatim.  Spaces are not inserted around
literal strings.")


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
    Time-stamp: <95/01/18 10:20:51 gildea>
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
	      (save-restriction
		(widen)
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
				(untabify start end))))))))
	  ;; don't signal an error in a write-file-hook
	  (message "time-stamp-start or time-stamp-end is not a string")))
    (if time-stamp-warn-inactive
	(message "Did not time-stamp buffer.")))
  ;; be sure to return nil so can be used on write-file-hooks
  nil)

;;;###autoload
(defun time-stamp-toggle-active (&optional arg)
  "Toggle time-stamp-active, which enables time stamping of files.
With arg, turn time stamping on if and only if arg is positive."
  (interactive "P")
  (setq time-stamp-active
	(if (null arg)
	    (not time-stamp-active)
	  (> (prefix-numeric-value arg) 0)))
    (message "time-stamp is now %s." (if time-stamp-active "active" "off")))
  

(defun time-stamp-string ()
  "Generate the new string to be inserted by \\[time-stamp]."
  (if (stringp time-stamp-format)
      (time-stamp-strftime time-stamp-format)
    (time-stamp-fconcat time-stamp-format " "))) ;version 1 compatibility

(defun time-stamp-strftime (format &optional time)
  "Uses a FORMAT to format date, time, file, and user information.
Optional second argument TIME will be used instead of the current time.
Characters in the format are copied literally except for %-directives:

%a  weekday name: `Monday'.		%A gives uppercase: `MONDAY'
%b  month name: `January'.		%B gives uppercase: `JANUARY'
%d  day of month
%H  24-hour clock hour
%I  12-hour clock hour
%m  month number
%M  minute
%p  `am' or `pm'.			%P gives uppercase: `AM' or `PM'
%S  seconds
%w  day number of week, Sunday is 0
%y  year: `1995'
%z  time zone name: `est'.		%Z gives uppercase: `EST'

Non-date items:
%%  a literal percent character: `%'
%f  file name without directory		%F gives absolute pathname
%s  system name
%u  user's login name
%h  mail host name

Decimal digits between the % and the type character specify the
field width.  Strings are truncated on the right; numbers on the left.
A leading zero causes numbers to be zero-filled.

For example, to get the format used by the `date' command,
use \"%3a %3b %2d %02H:%02M:%02S %Z %y\""
  (let ((time-string (cond ((stringp time)
			    time)
			   (time
			    (current-time-string time))
			   (t
			    (current-time-string))))
	(fmt-len (length format))
	(ind 0)
	cur-char
	(result "")
	field-index
	field-width
	field-result)
    (while (< ind fmt-len)
      (setq cur-char (aref format ind))
      (setq
       result
       (concat result 
      (cond
       ((and (eq cur-char ?%)
	     (< (1+ ind) fmt-len))
	(setq field-index (1+ ind))
	(while (progn
		 (setq ind (1+ ind))
		 (setq cur-char (aref format ind))
		 (and (<= ?0 cur-char) (>= ?9 cur-char))))
	(setq field-width (substring format field-index ind))
	(setq field-result
	(cond
	 ((eq cur-char ?%)
	  "%")
	 ((or (eq cur-char ?a)		;weekday name
	      (eq cur-char ?A))
	  (let ((name
		 (aref time-stamp-weekday-full-names
		       (cdr (assoc (substring time-string 0 3)
				   time-stamp-weekday-numbers)))))
	    (if (eq cur-char ?a)
		name
	      (upcase name))))
	 ((or (eq cur-char ?b)		;month name
	      (eq cur-char ?B))
	  (let ((name
		 (aref time-stamp-month-full-names
		       (cdr (assoc (substring time-string 4 7)
				   time-stamp-month-numbers)))))
	    (if (eq cur-char ?b)
		name
	      (upcase name))))
	 ((eq cur-char ?d)		;day of month, 1-31
	  (string-to-int (substring time-string 8 10)))
	 ((eq cur-char ?H)		;hour, 0-23
	  (string-to-int (substring time-string 11 13)))
	 ((eq cur-char ?I)		;hour, 1-12
	  (let ((hour (string-to-int (substring time-string 11 13))))
	    (cond ((< hour 1)
		   (+ hour 12))
		  ((> hour 12)
		   (- hour 12))
		  (t
		   hour))))
	 ((eq cur-char ?m)		;month number, 1-12
	  (cdr (assoc (substring time-string 4 7)
		      time-stamp-month-numbers)))
	 ((eq cur-char ?M)		;minute, 0-59
	  (string-to-int (substring time-string 14 16)))
	 ((or (eq cur-char ?p)		;am or pm
	      (eq cur-char ?P))
	  (let ((name
		 (if (> 12 (string-to-int (substring time-string 11 13)))
		     "am"
		   "pm")))
	    (if (eq cur-char ?p)
		name
	      (upcase name))))
	 ((eq cur-char ?S)		;seconds, 00-60
	  (string-to-int (substring time-string 17 19)))
	 ((eq cur-char ?w)		;weekday number, Sunday is 0
	  (cdr (assoc (substring time-string 0 3) time-stamp-weekday-numbers)))
	 ((eq cur-char ?y)		;year
	  (string-to-int (substring time-string -4)))
	 ((or (eq cur-char ?z)		;time zone
	      (eq cur-char ?Z))
	  (let ((name
		 (if (fboundp 'current-time-zone)
		     (car (cdr (current-time-zone time))))))
	    (or name (setq name ""))
	    (if (eq cur-char ?z)
		(downcase name)
	      (upcase name))))
	 ((eq cur-char ?f)		;buffer-file-name, base name only
	  (if buffer-file-name
	      (file-name-nondirectory buffer-file-name)
	    "(no file)"))
	 ((eq cur-char ?F)		;buffer-file-name, full path
	  (or buffer-file-name
	      "(no file)"))
	 ((eq cur-char ?s)		;system name
	  (system-name))
	 ((eq cur-char ?u)		;user name
	  (user-login-name))
	 ((eq cur-char ?h)		;mail host name
	  (time-stamp-mail-host-name))
	 ))
	(if (string-equal field-width "")
	    field-result
	  (let ((padded-result
		 (format (format "%%%s%c"
				 field-width
				 (if (numberp field-result) ?d ?s))
			 (or field-result ""))))
	    (let ((initial-length (length padded-result))
		  (desired-length (string-to-int field-width)))
	      (if (> initial-length desired-length)
		  ;; truncate strings on right, numbers on left
		  (if (stringp field-result)
		      (substring padded-result 0 desired-length)
		    (substring padded-result (- desired-length)))
		padded-result)))))
       (t
	(char-to-string cur-char)))))
      (setq ind (1+ ind)))
    result))

(defconst time-stamp-month-numbers
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  "Alist of months and their number.")

(defconst time-stamp-month-full-names
  ["(zero)" "January" "February" "March" "April" "May" "June"
   "July" "August" "September" "October" "November" "December"])

(defconst time-stamp-weekday-numbers
  '(("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
    ("Thu" . 4) ("Fri" . 5) ("Sat" . 6))
  "Alist of weekdays and their number.")

(defconst time-stamp-weekday-full-names
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(defun time-stamp-mail-host-name ()
  "Return the name of the host where the user receives mail.
This is the value of `mail-host-address' if bound and a string,
otherwise the value of `time-stamp-mail-host' (for versions of Emacs
before 19.29) otherwise the value of the function system-name.
This function may be usefully referenced by `time-stamp-format'."
  (or (and (boundp 'mail-host-address)
	   (stringp mail-host-address)
	   mail-host-address)
      (and (boundp 'time-stamp-mail-host) ;for backward compatibility
	   (stringp time-stamp-mail-host)
	   time-stamp-mail-host)
      (system-name)))

;;; the rest of this file is for version 1 compatibility

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


;;; Some useful functions to use in time-stamp-format

;;; Could generate most of a message-id with
;;; '(time-stamp-yymmdd "" time-stamp-hhmm "@" time-stamp-mail-host-name)

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
