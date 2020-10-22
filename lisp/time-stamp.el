;;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs

;; Copyright (C) 1989, 1993-1995, 1997, 2000-2020 Free Software
;; Foundation, Inc.

;; This file is part of GNU Emacs.

;; Maintainer: Stephen Gildea <stepheng+emacs@gildea.com>
;; Keywords: tools

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A template in a file can be updated with a new time stamp when
;; you save the file.  For example:
;;     static char *ts = "sdmain.c Time-stamp: <2001-08-13 10:20:51 gildea>";

;; To use time-stamping, add this line to your init file:
;;     (add-hook 'before-save-hook 'time-stamp)
;; Now any time-stamp templates in your files will be updated automatically.

;; See the documentation for the functions `time-stamp'
;; and `time-stamp-toggle-active' for details.

;;; Code:

(defgroup time-stamp nil
  "Maintain last change time stamps in files edited by Emacs."
  :group 'data
  :group 'extensions)

(defcustom time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %l"
  "Format of the string inserted by \\[time-stamp].
This is a string, used verbatim except for character sequences beginning
with %, as follows.  The values of non-numeric formatted items depend
on the locale setting recorded in `system-time-locale' and
`locale-coding-system'.  The examples here are for the default
\(`C') locale.

%:A  weekday name: `Monday'             %#A gives uppercase: `MONDAY'
%3a  abbreviated weekday: `Mon'         %#a gives uppercase: `MON'
%:B  month name: `January'              %#B gives uppercase: `JANUARY'
%3b  abbreviated month: `Jan'           %#b gives uppercase: `JAN'
%02d day of month
%02H 24-hour clock hour
%02I 12-hour clock hour
%02m month number
%02M minute
%#p  `am' or `pm'                       %P  gives uppercase: `AM' or `PM'
%02S seconds
%w   day number of week, Sunday is 0
%02y 2-digit year: `03'                 %Y  4-digit year: `2003'
%Z   time zone name: `EST'              %#Z gives lowercase: `est'
%5z  time zone offset: `-0500' (since Emacs 27; see note below)

Non-date items:
%%   a literal percent character: `%'
%f   file name without directory        %F  gives absolute pathname
%l   login name                         %L  full name of logged-in user
%q   unqualified host name              %Q  fully-qualified host name
%h   mail host name

Decimal digits between the % and the type character specify the
field width.  Strings are truncated on the right.
A leading zero in the field width zero-fills a number.

For example, to get the format used by the `date' command,
use \"%3a %3b %2d %02H:%02M:%02S %Z %Y\".

The default padding of some formats has changed to be more compatible
with format-time-string.  To be compatible with older versions of Emacs,
specify a padding width (as shown) or use the : modifier to request the
transitional behavior (again, as shown).

The behavior of `%5z' is new in Emacs 27.  If your files might be
edited by older versions of Emacs also, do not use this format yet."
  :type 'string
  :group 'time-stamp
  :version "27.1")
;;;###autoload(put 'time-stamp-format 'safe-local-variable 'stringp)

(defcustom time-stamp-active t
  "Non-nil to enable time-stamping of buffers by \\[time-stamp].
Can be toggled by \\[time-stamp-toggle-active].

This option does not affect when `time-stamp' is run, only what it
does when it runs.  To activate automatic time-stamping of buffers
when they are saved, either add this line to your init file:
    (add-hook \\='before-save-hook \\='time-stamp)
or customize option `before-save-hook'.

See also the variable `time-stamp-warn-inactive'."
  :type 'boolean
  :group 'time-stamp)

(defcustom time-stamp-warn-inactive t
  "Have \\[time-stamp] warn if a buffer did not get time-stamped.
If non-nil, a warning is displayed if `time-stamp-active' has
deactivated time stamping and the buffer contains a template that
otherwise would have been updated."
  :type 'boolean
  :group 'time-stamp
  :version "19.29")

(defcustom time-stamp-time-zone nil
  "The time zone to be used by \\[time-stamp].
Its format is that of the ZONE argument of the `format-time-string' function."
  :type '(choice (const :tag "Emacs local time" nil)
                 (const :tag "Universal Time" t)
                 (const :tag "system wall clock time" wall)
                 (string :tag "TZ environment variable value")
                 (list :tag "Offset and name"
                       (integer :tag "Offset (seconds east of UTC)")
                       (string :tag "Time zone abbreviation"))
                 (integer :tag "Offset (seconds east of UTC)"))
  :group 'time-stamp
  :version "20.1")
;;;###autoload(put 'time-stamp-time-zone 'safe-local-variable 'time-stamp-zone-type-p)

;;;###autoload
(defun time-stamp-zone-type-p (zone)
  "Return whether or not ZONE is of the correct type for a timezone rule.
Valid ZONE values are described in the documentation of `format-time-string'."
  (or (memq zone '(nil t wall))
      (stringp zone)
      (and (consp zone)
           (integerp (car zone))
           (consp (cdr zone))
           (stringp (cadr zone)))
      (integerp zone)))

;;; Do not change time-stamp-line-limit, time-stamp-start,
;;; time-stamp-end, time-stamp-pattern, time-stamp-inserts-lines,
;;; or time-stamp-count in your .emacs or you will be incompatible
;;; with other people's files!  If you must change them, do so only
;;; in the local variables section of the file itself.


(defvar time-stamp-line-limit 8	    ;Do not change!
  "Lines of a file searched; positive counts from start, negative from end.
The patterns `time-stamp-start' and `time-stamp-end' must be found in
the first (last) `time-stamp-line-limit' lines of the file for the
file to be time-stamped by \\[time-stamp].  A value of 0 searches the
entire buffer (use with care).

This value can also be set with the variable `time-stamp-pattern'.

Do not change `time-stamp-line-limit', `time-stamp-start',
`time-stamp-end', or `time-stamp-pattern' for yourself or you will be
incompatible with other people's files!  If you must change them for some
application, do so in the local variables section of the time-stamped file
itself.")
;;;###autoload(put 'time-stamp-line-limit 'safe-local-variable 'integerp)

(defvar time-stamp-start "Time-stamp:[ \t]+\\\\?[\"<]+"    ;Do not change!
  "Regexp after which the time stamp is written by \\[time-stamp].
See also the variables `time-stamp-end' and `time-stamp-line-limit'.

This value can also be set with the variable `time-stamp-pattern'.

Do not change `time-stamp-line-limit', `time-stamp-start',
`time-stamp-end', or `time-stamp-pattern' for yourself or you will be
incompatible with other people's files!  If you must change them for some
application, do so in the local variables section of the time-stamped file
itself.")
;;;###autoload(put 'time-stamp-start 'safe-local-variable 'stringp)

(defvar time-stamp-end "\\\\?[\">]"    ;Do not change!
  "Regexp marking the text after the time stamp.
\\[time-stamp] deletes the text between the first match of `time-stamp-start'
and the following match of `time-stamp-end', then writes the
time stamp specified by `time-stamp-format' between them.

This value can also be set with the variable `time-stamp-pattern'.

The end text normally starts on the same line as the start text ends,
but if there are any newlines in `time-stamp-format', the same number
of newlines must separate the start and end.  \\[time-stamp] tries
to not change the number of lines in the buffer.  `time-stamp-inserts-lines'
controls this behavior.

Do not change `time-stamp-start', `time-stamp-end', `time-stamp-pattern',
or `time-stamp-inserts-lines' for yourself or you will be incompatible
with other people's files!  If you must change them for some application,
do so in the local variables section of the time-stamped file itself.")
;;;###autoload(put 'time-stamp-end 'safe-local-variable 'stringp)


(defvar time-stamp-inserts-lines nil    ;Do not change!
  "Whether \\[time-stamp] can change the number of lines in a file.
If nil, \\[time-stamp] skips as many lines as there are newlines in
`time-stamp-format' before looking for the `time-stamp-end' pattern,
thus it tries not to change the number of lines in the buffer.
If non-nil, \\[time-stamp] starts looking for the end pattern
immediately after the start pattern.  This behavior can cause
unexpected changes in the buffer if used carelessly, but it is useful
for generating repeated time stamps.

Do not change `time-stamp-end' or `time-stamp-inserts-lines' for
yourself or you will be incompatible with other people's files!
If you must change them for some application, do so in the local
variables section of the time-stamped file itself.")
;;;###autoload(put 'time-stamp-inserts-lines 'safe-local-variable 'symbolp)


(defvar time-stamp-count 1		;Do not change!
  "How many templates \\[time-stamp] will look for in a buffer.
The same time stamp will be written in each case.

Do not change `time-stamp-count' for yourself or you will be
incompatible with other people's files!  If you must change it for
some application, do so in the local variables section of the
time-stamped file itself.")
;;;###autoload(put 'time-stamp-count 'safe-local-variable 'integerp)


(defvar time-stamp-pattern nil		;Do not change!
  "Convenience variable setting all `time-stamp' location and format values.
This string has four parts, each of which is optional.
These four parts set `time-stamp-line-limit', `time-stamp-start',
`time-stamp-format', and `time-stamp-end'.  See the documentation
for each of these variables for details.

The first part is a number followed by a slash; the number sets the number
of lines at the beginning (negative counts from end) of the file searched
for the time stamp.  The number and the slash may be omitted to use the
normal value.

The second part is a regexp identifying the pattern preceding the time stamp.
This part may be omitted to use the normal pattern.

The third part specifies the format of the time stamp inserted.  See
the documentation for `time-stamp-format' for details.  Specify this
part as \"%%\" to use the normal format.

The fourth part is a regexp identifying the pattern following the time stamp.
This part may be omitted to use the normal pattern.

Examples:

\"-10/\" (sets only `time-stamp-line-limit')

\"-9/^Last modified: %%$\" (sets `time-stamp-line-limit',
`time-stamp-start' and `time-stamp-end')

\"@set Time-stamp: %:B %1d, %Y$\" (sets `time-stamp-start',
`time-stamp-format' and `time-stamp-end')

\"newcommand{\\\\\\\\timestamp}{%%}\" (sets `time-stamp-start'
and `time-stamp-end')

Do not change `time-stamp-pattern' `time-stamp-line-limit',
`time-stamp-start', or `time-stamp-end' for yourself or you will be
incompatible with other people's files!  If you must change them for
some application, do so only in the local variables section of the
time-stamped file itself.")
;;;###autoload(put 'time-stamp-pattern 'safe-local-variable 'stringp)



;;;###autoload
(defun time-stamp ()
  "Update the time stamp string(s) in the buffer.
A template in a file can be automatically updated with a new time stamp
every time you save the file.  Add this line to your init file:
    (add-hook \\='before-save-hook \\='time-stamp)
or customize option `before-save-hook'.
Normally the template must appear in the first 8 lines of a file and
look like one of the following:
      Time-stamp: <>
      Time-stamp: \" \"
The time stamp is written between the brackets or quotes:
      Time-stamp: <2001-02-18 10:20:51 gildea>

The time stamp is updated only if the variable
`time-stamp-active' is non-nil.
The format of the time stamp is set by the variable
`time-stamp-pattern' or `time-stamp-format'.
The variables `time-stamp-pattern', `time-stamp-line-limit',
`time-stamp-start', `time-stamp-end', `time-stamp-count', and
`time-stamp-inserts-lines' control finding the template."
  (interactive)
  (let ((line-limit time-stamp-line-limit)
	(ts-start time-stamp-start)
	(ts-format time-stamp-format)
	(ts-end time-stamp-end)
	(ts-count time-stamp-count)
	(format-lines 0)
	(end-lines 1)
	(start nil)
	search-limit)
    (if (stringp time-stamp-pattern)
	(progn
	  (string-match "\\`\\(\\(-?[0-9]+\\)/\\)?\\([^%]+\\)?\\(\\(%[-.,:@+_ #^()0-9]*[A-Za-z%][^%]*\\)*%[-.,:@+_ #^()0-9]*[A-Za-z%]\\)?\\([^%]+\\)?\\'" time-stamp-pattern)
	  (and (match-beginning 2)
	       (setq line-limit
		     (string-to-number (match-string 2 time-stamp-pattern))))
	  (and (match-beginning 3)
	       (setq ts-start (match-string 3 time-stamp-pattern)))
	  (and (match-beginning 4)
	       (not (string-equal (match-string 4 time-stamp-pattern) "%%"))
	       (setq ts-format (match-string 4 time-stamp-pattern)))
	  (and (match-beginning 6)
	       (setq ts-end (match-string 6 time-stamp-pattern)))))
    (cond ((not (integerp line-limit))
	   (setq line-limit 8)
	   (message "time-stamp-line-limit is not an integer")
	   (sit-for 1)))
    (cond ((not (integerp ts-count))
	   (setq ts-count 1)
	   (message "time-stamp-count is not an integer")
	   (sit-for 1))
	  ((< ts-count 1)
	   ;; We need to call time-stamp-once at least once
	   ;; to output any warnings about time-stamp not being active.
	   (setq ts-count 1)))
    ;; Figure out what lines the end should be on.
    (if (stringp ts-format)
	(let ((nl-start 0))
	  (while (string-match "\n" ts-format nl-start)
	    (setq format-lines (1+ format-lines) nl-start (match-end 0)))))
    (let ((nl-start 0))
      (while (string-match "\n" ts-end nl-start)
	(setq end-lines (1+ end-lines) nl-start (match-end 0))))
    ;; Find overall what lines to look at
    (save-excursion
      (save-restriction
	(widen)
	(cond ((> line-limit 0)
	       (goto-char (setq start (point-min)))
	       (forward-line line-limit)
	       (setq search-limit (point)))
	      ((< line-limit 0)
	       (goto-char (setq search-limit (point-max)))
	       (forward-line line-limit)
	       (setq start (point)))
	      (t			;0 => no limit (use with care!)
	       (setq start (point-min))
	       (setq search-limit (point-max))))))
    (while (and start
		(< start search-limit)
		(> ts-count 0))
      (setq start (time-stamp-once start search-limit ts-start ts-end
				   ts-format format-lines end-lines))
      (setq ts-count (1- ts-count))))
  nil)

(defun time-stamp-once (start search-limit ts-start ts-end
			ts-format format-lines end-lines)
  "Update one time stamp.  Internal routine called by \\[time-stamp].
Returns the end point, which is where `time-stamp' begins the next search."
  (let ((case-fold-search nil)
	(end nil)
	end-search-start
	(end-length nil))
    (save-excursion
      (save-restriction
	(widen)
	;; Find the location of the time stamp.
	(while (and (< (goto-char start) search-limit)
		    (not end)
		    (re-search-forward ts-start search-limit 'move))
	  (setq start (point))
	  (if (not time-stamp-inserts-lines)
	      (forward-line format-lines))
	  (setq end-search-start (max start (point)))
	  (if (= (forward-line end-lines) 0)
	      (progn
	       (and (bolp) (backward-char))
	       (let ((line-end (min (point) search-limit)))
		 (if (>= line-end end-search-start)
		     (progn
		      (goto-char end-search-start)
		      (if (re-search-forward ts-end line-end t)
			  (progn
			    (setq end (match-beginning 0))
			    (setq end-length (- (match-end 0) end))))))))))))
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
	   ((not (and (stringp ts-start)
		      (stringp ts-end)))
	    (message "time-stamp-start or time-stamp-end is not a string")
	    (sit-for 1))
	   (t
	    (let ((new-time-stamp (time-stamp-string ts-format)))
	      (if (and (stringp new-time-stamp)
		       (not (string-equal (buffer-substring start end)
					  new-time-stamp)))
		  (save-excursion
		    (save-restriction
		      (widen)
		      (delete-region start end)
		      (goto-char start)
		      (insert-and-inherit new-time-stamp)
		      (setq end (point))
		      ;; remove any tabs used to format time stamp
		      (if (search-backward "\t" start t)
			  (progn
			    (untabify start end)
			    (setq end (point))))))))))))
    ;; return the location after this time stamp, if there was one
    (and end end-length
	 (+ end end-length))))


;;;###autoload
(defun time-stamp-toggle-active (&optional arg)
  "Toggle `time-stamp-active', setting whether \\[time-stamp] updates a buffer.
With ARG, turn time stamping on if and only if arg is positive."
  (interactive "P")
  (setq time-stamp-active
	(if (null arg)
	    (not time-stamp-active)
	  (> (prefix-numeric-value arg) 0)))
  (message "time-stamp is now %s." (if time-stamp-active "active" "off")))

(defun time-stamp--format (format time)
  (format-time-string format time time-stamp-time-zone))

(defun time-stamp-string (&optional ts-format time)
  "Generate the new string to be inserted by \\[time-stamp].
Optionally use format TS-FORMAT instead of `time-stamp-format' to
format the string.  Optional second argument TIME is only for testing;
normally the current time is used."
  (if (stringp (or ts-format (setq ts-format time-stamp-format)))
      (time-stamp-string-preprocess ts-format time)))


(defconst time-stamp-no-file "(no file)"
  "String to use when the buffer is not associated with a file.")

;;; time-stamp is transitioning to be compatible with format-time-string.
;;; During the process, this function implements
;;; intermediate, compatible formats.
;;;      At all times, all the formats recommended in the doc string
;;; of time-stamp-format will work not only in the current version of
;;; Emacs, but in all versions that have been released within the past
;;; two years.
;;;      The : modifier is a temporary conversion feature used to resolve
;;; ambiguous formats--formats that are changing (over time) incompatibly.
(defun time-stamp-string-preprocess (format &optional time)
  "Use a FORMAT to format date, time, file, and user information.
Optional second argument TIME is only for testing.
Implements non-time extensions to `format-time-string'
and all `time-stamp-format' compatibility."
  (let ((fmt-len (length format))
	(ind 0)
	cur-char
	(prev-char nil)
	(result "")
	field-width
	field-result
	alt-form change-case upcase
	(paren-level 0))
    (while (< ind fmt-len)
      (setq cur-char (aref format ind))
      (setq
       result
       (concat result
      (cond
       ((eq cur-char ?%)
	;; eat any additional args to allow for future expansion
	(setq alt-form 0 change-case nil upcase nil field-width "")
	(while (progn
		 (setq ind (1+ ind))
		 (setq cur-char (if (< ind fmt-len)
				    (aref format ind)
				  ?\0))
		 (or (eq ?. cur-char)
		     (eq ?, cur-char) (eq ?: cur-char) (eq ?@ cur-char)
		     (eq ?- cur-char) (eq ?+ cur-char) (eq ?_ cur-char)
		     (eq ?\s cur-char) (eq ?# cur-char) (eq ?^ cur-char)
		     (and (eq ?\( cur-char)
			  (not (eq prev-char ?\\))
			  (setq paren-level (1+ paren-level)))
		     (if (and (eq ?\) cur-char)
			      (not (eq prev-char ?\\))
			      (> paren-level 0))
			 (setq paren-level (1- paren-level))
		       (and (> paren-level 0)
			    (< ind fmt-len)))
		     (if (and (<= ?0 cur-char) (>= ?9 cur-char))
			 ;; get format width
			 (let ((field-index ind))
			   (while (progn
				    (setq ind (1+ ind))
				    (setq cur-char (if (< ind fmt-len)
						       (aref format ind)
						     ?\0))
				    (and (<= ?0 cur-char) (>= ?9 cur-char))))
			   (setq field-width (substring format field-index ind))
			   (setq ind (1- ind))
			   t))))
	  (setq prev-char cur-char)
	  ;; some characters we actually use
	  (cond ((eq cur-char ?:)
		 (setq alt-form (1+ alt-form)))
		((eq cur-char ?#)
		 (setq change-case t))
		((eq cur-char ?^)
		 (setq upcase t))
		((eq cur-char ?-)
		 (setq field-width "1"))
		((eq cur-char ?_)
		 (setq field-width "2"))))
	(setq field-result
	(cond
	 ((eq cur-char ?%)
	  "%")
	 ((eq cur-char ?a)		;day of week
          (if (> alt-form 0)
               (if (string-equal field-width "")
                   (time-stamp--format "%A" time)
                 "")			;discourage "%:3a"
            (if (or change-case upcase)
                (time-stamp--format "%#a" time)
	      (time-stamp--format "%a" time))))
	 ((eq cur-char ?A)
	  (if (or change-case upcase (not (string-equal field-width "")))
	      (time-stamp--format "%#A" time)
	    (time-stamp--format "%A" time)))
	 ((eq cur-char ?b)		;month name
          (if (> alt-form 0)
               (if (string-equal field-width "")
                   (time-stamp--format "%B" time)
                 "")			;discourage "%:3b"
            (if (or change-case upcase)
                (time-stamp--format "%#b" time)
	      (time-stamp--format "%b" time))))
	 ((eq cur-char ?B)
	  (if (or change-case upcase (not (string-equal field-width "")))
	      (time-stamp--format "%#B" time)
	    (time-stamp--format "%B" time)))
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
	  (if change-case
              (time-stamp--format "%#p" time)
            (time-stamp--format "%p" time)))
	 ((eq cur-char ?P)		;AM or PM
	  (time-stamp--format "%p" time))
	 ((eq cur-char ?S)		;seconds, 00-60
	  (time-stamp-do-number cur-char alt-form field-width time))
	 ((eq cur-char ?w)		;weekday number, Sunday is 0
	  (time-stamp--format "%w" time))
	 ((eq cur-char ?y)		;year
          (if (> alt-form 0)
              (string-to-number (time-stamp--format "%Y" time))
            (if (or (string-equal field-width "")
                    (<= (string-to-number field-width) 2))
                (string-to-number (time-stamp--format "%y" time))
              (time-stamp-conv-warn (format "%%%sy" field-width) "%Y")
              (string-to-number (time-stamp--format "%Y" time)))))
	 ((eq cur-char ?Y)		;4-digit year
	  (string-to-number (time-stamp--format "%Y" time)))
	 ((eq cur-char ?z)		;time zone offset
	  (if change-case
	      ""			;discourage %z variations
            (cond ((= alt-form 0)
                   (if (string-equal field-width "")
                       (progn
                         (time-stamp-conv-warn "%z" "%#Z")
                         (time-stamp--format "%#Z" time))
                     (cond ((string-equal field-width "1")
                            (setq field-width "3")) ;%-z -> "+00"
                           ((string-equal field-width "2")
                            (setq field-width "5")) ;%_z -> "+0000"
                           ((string-equal field-width "4")
                            (setq field-width "0"))) ;discourage %4z
                     (time-stamp--format "%z" time)))
                  ((= alt-form 1)
                   (time-stamp--format "%:z" time))
                  ((= alt-form 2)
                   (time-stamp--format "%::z" time))
                  ((= alt-form 3)
                   (time-stamp--format "%:::z" time)))))
	 ((eq cur-char ?Z)              ;time zone name
	  (if change-case
	      (time-stamp--format "%#Z" time)
	    (time-stamp--format "%Z" time)))
	 ((eq cur-char ?f)		;buffer-file-name, base name only
	  (if buffer-file-name
	      (file-name-nondirectory buffer-file-name)
	    time-stamp-no-file))
	 ((eq cur-char ?F)		;buffer-file-name, full path
	  (or buffer-file-name
	      time-stamp-no-file))
	 ((eq cur-char ?s)		;system name, legacy
	  (system-name))
	 ((eq cur-char ?u)		;user name, legacy
	  (user-login-name))
	 ((eq cur-char ?U)		;user full name, legacy
	  (user-full-name))
	 ((eq cur-char ?l)		;login name
	  (user-login-name))
	 ((eq cur-char ?L)		;full name of logged-in user
	  (user-full-name))
	 ((eq cur-char ?h)		;mail host name
	  (or mail-host-address (system-name)))
	 ((eq cur-char ?q)		;unqualified host name
	  (let ((qualname (system-name)))
	    (if (string-match "\\." qualname)
		(substring qualname 0 (match-beginning 0))
	      qualname)))
	 ((eq cur-char ?Q)		;fully-qualified host name
	  (system-name))
	 ))
        (and (numberp field-result)
             (= alt-form 0)
             (string-equal field-width "")
             ;; no width provided; set width for default
             (setq field-width "02"))
	(let ((padded-result
	       (format (format "%%%s%c"
			       field-width
			       (if (numberp field-result) ?d ?s))
		       (or field-result ""))))
	  (let* ((initial-length (length padded-result))
		 (desired-length (if (string-equal field-width "")
				     initial-length
				   (string-to-number field-width))))
	    (if (> initial-length desired-length)
		;; truncate strings on right
		(if (stringp field-result)
		    (substring padded-result 0 desired-length)
                  padded-result)	;numbers don't truncate
	      padded-result))))
       (t
	(char-to-string cur-char)))))
      (setq ind (1+ ind)))
    result))

(defun time-stamp-do-number (format-char alt-form field-width time)
  "Handle compatible FORMAT-CHAR where only default width/padding will change.
ALT-FORM is whether `#' specified.  FIELD-WIDTH is the string
width specification or \"\".  TIME is the time to convert."
  (let ((format-string (concat "%" (char-to-string format-char))))
    (if (and (> alt-form 0) (not (string-equal field-width "")))
	""				;discourage "%:2d" and the like
      (string-to-number (time-stamp--format format-string time)))))

(defvar time-stamp-conversion-warn t
  "Warn about soon-to-be-unsupported forms in `time-stamp-format'.
If nil, these warnings are disabled, which would be a bad idea!
You really need to update your files instead.

The new formats will work with old versions of Emacs.
New formats are being recommended now to allow `time-stamp-format'
to change in the future to be compatible with `format-time-string'.
The new forms being recommended now will continue to work then.")


(defun time-stamp-conv-warn (old-form new-form)
  "Display a warning about a soon-to-be-obsolete format.
Suggests replacing OLD-FORM with NEW-FORM."
  (cond
   (time-stamp-conversion-warn
    (with-current-buffer (get-buffer-create "*Time-stamp-compatibility*")
      (goto-char (point-max))
      (if (bobp)
	  (progn
	    (insert
	     "The formats recognized in time-stamp-format will change in a future release\n"
	     "to be more compatible with the format-time-string function.\n\n"
	     "The following obsolescent time-stamp-format construct(s) were found:\n\n")))
      (insert "\"" old-form "\" -- use " new-form "\n"))
    (display-buffer "*Time-stamp-compatibility*"))))

(provide 'time-stamp)

;;; time-stamp.el ends here
