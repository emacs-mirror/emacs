;;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs

;; Copyright 1989, 1993, 1994, 1995, 1997 Free Software Foundation, Inc.

;; Maintainer's Time-stamp: <1997-08-07 14:46:50 gildea>
;; Maintainer: Stephen Gildea <gildea@alum.mit.edu>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A template in a file can be updated with a new time stamp when
;; you save the file.  For example:
;;     static char *ts = "sdmain.c Time-stamp: <1996-08-13 10:20:51 gildea>";
;; See the top of `time-stamp.el' for another example.

;; To use time-stamping, add this line to your .emacs file:
;;     (add-hook 'write-file-hooks 'time-stamp)
;; Now any time-stamp templates in your files will be updated automatically.

;; See the documentation for the functions `time-stamp'
;; and `time-stamp-toggle-active' for details.

;;; Code:

(defgroup time-stamp nil
  "Maintain last change time stamps in files edited by Emacs."
  :group 'data
  :group 'extensions)

(defcustom time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %u"
  "*Format of the string inserted by \\[time-stamp].
The value may be a string or a list.  Lists are supported only for
backward compatibility; see variable `time-stamp-old-format-warn'.

A string is used verbatim except for character sequences beginning with %:

%:a  weekday name: `Monday'.		%#A gives uppercase: `MONDAY'
%3a  abbreviated weekday: `Mon'.	%3A gives uppercase: `MON'
%:b  month name: `January'.		%#B gives uppercase: `JANUARY'
%3b  abbreviated month: `Jan'.		%3B gives uppercase: `JAN'
%02d day of month
%02H 24-hour clock hour
%02I 12-hour clock hour
%02m month number
%02M minute
%#p  `am' or `pm'.			%P  gives uppercase: `AM' or `PM'
%02S seconds
%w   day number of week, Sunday is 0
%02y 2-digit year: `97'			%:y 4-digit year: `1997'
%z   time zone name: `est'.		%Z  gives uppercase: `EST'

Non-date items:
%%   a literal percent character: `%'
%f   file name without directory	%F  gives absolute pathname
%s   system name
%u   user's login name
%h   mail host name

Decimal digits between the % and the type character specify the
field width.  Strings are truncated on the right; years on the left.
A leading zero causes numbers to be zero-filled.

For example, to get the format used by the `date' command,
use \"%3a %3b %2d %02H:%02M:%02S %Z %:y\".

In the future these formats will be aligned more with format-time-string.
Because of this transition, the default padding for numeric formats will
change in a future version.  Therefore either a padding width should be
specified, or the : modifier should be used to explicitly request the
historical default."
  :type 'string
  :group 'time-stamp)

(defcustom time-stamp-active t
  "*Non-nil to enable time-stamping of buffers by \\[time-stamp].
Can be toggled by \\[time-stamp-toggle-active].
See also the variable `time-stamp-warn-inactive'."
  :type 'boolean
  :group 'time-stamp)

(defcustom time-stamp-warn-inactive t
  "Non-nil to have \\[time-stamp] warn if a buffer did not get time-stamped.
A warning is printed if `time-stamp-active' is nil and the buffer contains
a time stamp template that would otherwise have been updated."
  :type 'boolean
  :group 'time-stamp)

(defcustom time-stamp-old-format-warn 'ask
  "Action to take if `time-stamp-format' is an old-style list.
If `error', the format is not used.  If `ask', the user is queried about
using the time-stamp-format.  If `warn', a warning is displayed.
If nil, no notification is given."
  :type '(choice (const :tag "No notification" nil)
                 (const :tag "Don't use the format" error)
                 (const ask) (const warn))
  :group 'time-stamp)

(defcustom time-stamp-time-zone nil
  "If non-nil, a string naming the timezone to be used by \\[time-stamp].
Format is the same as that used by the environment variable TZ on your system."
  :type '(choice (const nil) string)
  :group 'time-stamp)


;;; Do not change time-stamp-line-limit, time-stamp-start, or
;;; time-stamp-end in your .emacs or you will be incompatible
;;; with other people's files!  If you must change them,
;;; do so only in the local variables section of the file itself.


(defvar time-stamp-line-limit 8	    ;Do not change!
  "Lines of a file searched; positive counts from start, negative from end.
The patterns `time-stamp-start' and `time-stamp-end' must be found on one
of the first (last) `time-stamp-line-limit' lines of the file for the
file to be time-stamped by \\[time-stamp].

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
A template in a file can be automatically updated with a new time stamp
every time you save the file.  Add this line to your .emacs file:
    (add-hook 'write-file-hooks 'time-stamp)
Normally the template must appear in the first 8 lines of a file and
look like one of the following:
      Time-stamp: <>
      Time-stamp: \" \"
The time stamp is written between the brackets or quotes:
      Time-stamp: <1996-07-18 10:20:51 gildea>
The time stamp is updated only if the variable `time-stamp-active' is non-nil.
The format of the time stamp is set by the variable `time-stamp-format'.
The variables `time-stamp-line-limit', `time-stamp-start',
and `time-stamp-end' control finding the template."
  (interactive)
  (let ((case-fold-search nil)
	(start nil)
	(end nil)
	search-limit
	(line-limit time-stamp-line-limit))
    (cond ((not (integerp line-limit))
	   (setq line-limit 8)
	   (message "time-stamp-line-limit is not a number")
	   (sit-for 1)))
    (save-excursion
      (save-restriction
	(widen)
	(cond ((> line-limit 0)
	       (goto-char (setq start (point-min)))
	       (forward-line line-limit)
	       (setq search-limit (point)))
	      (t
	       (goto-char (setq search-limit (point-max)))
	       (forward-line line-limit)
	       (setq start (point))))
	(goto-char start)
	(while (and (< (point) search-limit)
		    (not end)
		    (re-search-forward time-stamp-start search-limit 'move))
	  (setq start (point))
	  (end-of-line)
	  (let ((line-end (point)))
	    (goto-char start)
	    (if (re-search-forward time-stamp-end line-end 'move)
		(setq end (match-beginning 0)))))))
	(if end
	    (progn
	      ;; do all warnings outside save-excursion
	      (cond
	       ((not time-stamp-active)
		(if time-stamp-warn-inactive
		    ;; don't signal an error in a write-file-hook
		    (progn
		      (message "Warning: time-stamp-active is off; did not time-stamp buffer.")
		      (sit-for 1))))
	       ((not (and (stringp time-stamp-start)
			  (stringp time-stamp-end)))
		(message "time-stamp-start or time-stamp-end is not a string")
		(sit-for 1))
	       (t
		(let ((new-time-stamp (time-stamp-string)))
		  (if (stringp new-time-stamp)
		      (save-excursion
			(save-restriction
			  (widen)
			  (delete-region start end)
			  (goto-char start)
			  (insert-and-inherit new-time-stamp)
			  (setq end (point))
			  ;; remove any tabs used to format time stamp
			  (goto-char start)
			  (if (search-forward "\t" end t)
			      (untabify start end)))))))))))
  ;; be sure to return nil so can be used on write-file-hooks
  nil)

;;;###autoload
(defun time-stamp-toggle-active (&optional arg)
  "Toggle `time-stamp-active', setting whether \\[time-stamp] updates a buffer.
With arg, turn time stamping on if and only if arg is positive."
  (interactive "P")
  (setq time-stamp-active
	(if (null arg)
	    (not time-stamp-active)
	  (> (prefix-numeric-value arg) 0)))
    (message "time-stamp is now %s." (if time-stamp-active "active" "off")))

(defconst time-stamp-no-file "(no file)"
  "String to use when the buffer is not associated with a file.")

;;; time-stamp is transitioning to using the new, expanded capabilities
;;; of format-time-string.  During the process, this function implements
;;; intermediate, compatible formats and complains about old, soon to
;;; be unsupported, formats.  This function will get a lot (a LOT) shorter
;;; when the transition is complete and we can just pass most things
;;; straight through to format-time-string.
;;;      At all times, all the formats recommended in the doc string
;;; of time-stamp-format will work not only in the current version of
;;; Emacs, but in all versions that have been released within the past
;;; two years.
;;;      The : modifier is a temporary conversion feature used to resolve
;;; ambiguous formats--formats that are changing (over time) incompatibly.
(defun time-stamp-string-preprocess (format &optional time)
  ;; Uses a FORMAT to format date, time, file, and user information.
  ;; Optional second argument TIME is only for testing.
  ;; Implements non-time extensions to format-time-string
  ;; and all time-stamp-format compatibility.
  (let ((fmt-len (length format))
	(ind 0)
	cur-char
	(prev-char nil)
	(result "")
	field-index
	field-width
	field-result
	alt-form change-case require-padding
	(paren-level 0))
    (while (< ind fmt-len)
      (setq cur-char (aref format ind))
      (setq
       result
       (concat result
      (cond
       ((eq cur-char ?%)
	;; eat any additional args to allow for future expansion
	(setq alt-form nil change-case nil require-padding nil)
	(while (progn
		 (setq ind (1+ ind))
		 (setq cur-char (if (< ind fmt-len)
				    (aref format ind)
				  ?\0))
		 (or (eq ?. cur-char)
		     (eq ?, cur-char) (eq ?: cur-char) (eq ?@ cur-char)
		     (eq ?- cur-char) (eq ?+ cur-char) (eq ?_ cur-char) 
		     (eq ?\  cur-char) (eq ?# cur-char) (eq ?^ cur-char)
		     (and (eq ?\( cur-char)
			  (not (eq prev-char ?\\))
			  (setq paren-level (1+ paren-level)))
		     (if (and (eq ?\) cur-char)
			      (not (eq prev-char ?\\))
			      (> paren-level 0))
			 (setq paren-level (1- paren-level))
		       (and (> paren-level 0)
			    (< ind fmt-len)))))
	  (setq prev-char cur-char)
	  ;; some characters we actually use
	  (cond ((eq cur-char ?:)
		 (setq alt-form t))
		((eq cur-char ?#)
		 (setq change-case t))))
	;; get format width
	(setq field-index ind)
	(setq ind (1- ind))
	(while (progn
		 (setq ind (1+ ind))
		 (setq cur-char (if (< ind fmt-len)
				    (aref format ind)
				  ?\0))
		 (and (<= ?0 cur-char) (>= ?9 cur-char))))
	(setq field-width (substring format field-index ind))
	(setq field-result
	(cond
	 ((eq cur-char ?%)
	  "%")
	 ((eq cur-char ?a)		;day of week
	  (if change-case
	      (format-time-string "%#A" time)
	    (or alt-form (not (string-equal field-width ""))
		(time-stamp-conv-warn "%a" "%:a"))
	    (if (and alt-form (not (string-equal field-width "")))
		""			;discourage "%:3a"
	      (format-time-string "%A" time))))
	 ((eq cur-char ?A)
	  (if alt-form
	      (format-time-string "%A" time)
	    (or change-case (not (string-equal field-width ""))
		(time-stamp-conv-warn "%A" "%#A"))
	    (format-time-string "%#A" time)))
	 ((eq cur-char ?b)		;month name
	  (if change-case
	      (format-time-string "%#B" time)
	    (or alt-form (not (string-equal field-width ""))
		(time-stamp-conv-warn "%b" "%:b"))
	    (if (and alt-form (not (string-equal field-width "")))
		""			;discourage "%:3b"
	    (format-time-string "%B" time))))
	 ((eq cur-char ?B)
	  (if alt-form
	      (format-time-string "%B" time)
	    (or change-case (not (string-equal field-width ""))
		(time-stamp-conv-warn "%B" "%#B"))
	    (format-time-string "%#B" time)))
	 ((eq cur-char ?d)		;day of month, 1-31
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?H)		;hour, 0-23
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?I)		;hour, 1-12
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?m)		;month number, 1-12
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?M)		;minute, 0-59
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?p)		;am or pm
	  (or change-case
	      (time-stamp-conv-warn "%p" "%#p"))
	  (format-time-string "%#p" time))
	 ((eq cur-char ?P)		;AM or PM
	  (format-time-string "%p" time))
	 ((eq cur-char ?S)		;seconds, 00-60
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?w)		;weekday number, Sunday is 0
	  (format-time-string "%w" time))
	 ((eq cur-char ?y)		;year
	  (or alt-form (not (string-equal field-width ""))
	      (time-stamp-conv-warn "%y" "%:y"))
	  (string-to-int (format-time-string "%Y" time)))
	 ((eq cur-char ?Y)		;4-digit year, new style
	  (string-to-int (format-time-string "%Y" time)))
	 ((eq cur-char ?z)		;time zone lower case
	  (if change-case
	      ""			;discourage %z variations
	    (format-time-string "%#Z" time)))
	 ((eq cur-char ?Z)
	  (if change-case
	      (format-time-string "%#Z" time)
	    (format-time-string "%Z" time)))
	 ((eq cur-char ?f)		;buffer-file-name, base name only
	  (if buffer-file-name
	      (file-name-nondirectory buffer-file-name)
	    time-stamp-no-file))
	 ((eq cur-char ?F)		;buffer-file-name, full path
	  (or buffer-file-name
	      time-stamp-no-file))
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
		  ;; truncate strings on right, years on left
		  (if (stringp field-result)
		      (substring padded-result 0 desired-length)
		    (if (eq cur-char ?y)
			(substring padded-result (- desired-length))
		      padded-result))	;non-year numbers don't truncate
		padded-result)))))
       (t
	(char-to-string cur-char)))))
      (setq ind (1+ ind)))
    result))

(defun time-stamp-do-number (format-char alt-form field-width time)
  ;; Handle a compatible FORMAT-CHAR where only
  ;; the default width/padding will change.
  ;; ALT-FORM is whether `#' specified.  FIELD-WIDTH is the string
  ;; width specification or "".  TIME is the time to convert.
  (let ((format-string (concat "%" (char-to-string format-char))))
    (and (not alt-form) (string-equal field-width "")
	 (time-stamp-conv-warn format-string
			       (format "%%:%c" format-char)))
    (if (and alt-form (not (string-equal field-width "")))
	""				;discourage "%:2d" and the like
      (string-to-int (format-time-string format-string time)))))

(defvar time-stamp-conversion-warn t
  "Non-nil to warn about soon-to-be-unsupported forms in time-stamp-format.
In would be a bad idea to disable these warnings!
You really need to update your files instead.

The new formats will work with old versions of Emacs.
New formats are being recommended now to allow time-stamp-format
to change in the future to be compatible with format-time-string.
The new forms being recommended now will continue to work then.")


(defun time-stamp-conv-warn (old-form new-form)
  ;; Display a warning about a soon-to-be-obsolete format.
  (cond
   (time-stamp-conversion-warn
    (save-excursion
      (set-buffer (get-buffer-create "*Time-stamp-compatibility*"))
      (goto-char (point-max))
      (if (bobp)
	  (progn
	    (insert
	     "The formats recognized in time-stamp-format will change in a future release\n"
	     "to be compatible with the new, expanded format-time-string function.\n\n"
	     "The following obsolescent time-stamp-format construct(s) were found:\n\n")))
      (insert "\"" old-form "\" -- use " new-form "\n"))
    (display-buffer "*Time-stamp-compatibility*"))))



(defun time-stamp-string ()
  "Generate the new string to be inserted by \\[time-stamp]."
  (if (stringp time-stamp-format)
      (if (stringp time-stamp-time-zone)
	  (let ((real-time-zone (getenv "TZ")))
	    (unwind-protect
		(progn
		  (setenv "TZ" time-stamp-time-zone)
		  (format-time-string
		   (time-stamp-string-preprocess time-stamp-format)))
	      (setenv "TZ" real-time-zone)))
	(format-time-string
	 (time-stamp-string-preprocess time-stamp-format)))
    ;; handle version 1 compatibility
    (cond ((or (eq time-stamp-old-format-warn 'error)
	       (and (eq time-stamp-old-format-warn 'ask)
		    (not (y-or-n-p "Use non-string time-stamp-format? "))))
	   (message "Warning: no time-stamp: time-stamp-format not a string")
	   (sit-for 1)
	   nil)
	  (t
	   (cond ((eq time-stamp-old-format-warn 'warn)
		  (message "Obsolescent time-stamp-format type; should be string")
		  (sit-for 1)))
	   (time-stamp-fconcat time-stamp-format " ")))))

(defconst time-stamp-no-file "(no file)"
  "String to use when the buffer is not associated with a file.")

(defun time-stamp-mail-host-name ()
  "Return the name of the host where the user receives mail.
This is the value of `mail-host-address' if bound and a string,
otherwise the value of the function system-name."
  (or (and (boundp 'mail-host-address)
	   (stringp mail-host-address)
	   mail-host-address)
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

;;; Some functions used in time-stamp-format

;;; Could generate most of a message-id with
;;; '(time-stamp-yymmdd "" time-stamp-hhmm "@" time-stamp-mail-host-name)

;;; pretty form, suitable for a title page

(defun time-stamp-month-dd-yyyy ()
  "Return the current date as a string in \"Month DD, YYYY\" form."
  (format-time-string "%B %e, %Y"))

(defun time-stamp-dd/mm/yyyy ()
  "Return the current date as a string in \"DD/MM/YYYY\" form."
  (format-time-string "%d/%m/%Y"))

;;; same as __DATE__ in ANSI C

(defun time-stamp-mon-dd-yyyy ()
  "Return the current date as a string in \"Mon DD YYYY\" form.
The first character of DD is space if the value is less than 10."
  (format-time-string "%b %d %Y"))

;;; RFC 822 date

(defun time-stamp-dd-mon-yy ()
  "Return the current date as a string in \"DD Mon YY\" form."
  (format-time-string "%d %b %y"))

;;; RCS 3 date

(defun time-stamp-yy/mm/dd ()
  "Return the current date as a string in \"YY/MM/DD\" form."
  (format-time-string "%y/%m/%d"))

;;; RCS 5 date

(defun time-stamp-yyyy/mm/dd ()
  "Return the current date as a string in \"YYYY/MM/DD\" form."
  (format-time-string "%Y/%m/%d"))

;;; ISO 8601 date

(defun time-stamp-yyyy-mm-dd ()
  "Return the current date as a string in \"YYYY-MM-DD\" form."
  (format-time-string "%Y-%m-%d"))

(defun time-stamp-yymmdd ()
  "Return the current date as a string in \"YYMMDD\" form."
  (format-time-string "%y%m%d"))

(defun time-stamp-hh:mm:ss ()
  "Return the current time as a string in \"HH:MM:SS\" form."
  (format-time-string "%T"))

(defun time-stamp-hhmm ()
  "Return the current time as a string in \"HHMM\" form."
  (format-time-string "%H%M"))

(provide 'time-stamp)

;;; time-stamp.el ends here
