;;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1989, 1993-1995, 1997, 2000-2026 Free Software
;; Foundation, Inc.

;; This file is part of GNU Emacs.

;; Author: Stephen Gildea <stepheng+emacs@gildea.com>
;; Keywords: files, tools

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
;;     static char *ts = "sdmain.c Time-stamp: <2025-03-28 21:31:56 gildea>";

;; To use time-stamping, add this line to your init file:
;;     (add-hook 'before-save-hook 'time-stamp)
;; Now any time-stamp templates in your files will be updated automatically.

;; For details, see the documentation for function `time-stamp'
;; and the Info node `Time Stamps'.

;;; Code:

(defgroup time-stamp nil
  "Maintain last change time stamps in files edited by Emacs."
  :group 'files
  :group 'extensions)


(defcustom time-stamp-format "%Y-%m-%d %H:%M:%S %l"
  "Format of the string inserted by \\[time-stamp].
The string is inserted verbatim except for character sequences beginning
with %, which are converted as follows:

%A    weekday name: `Monday'           %a    abbreviated weekday name: `Mon'
%B    month name: `January'            %b    abbreviated month name: `Jan'
%d    day of month
%H    24-hour clock hour               %I    12-hour clock hour
%m    month number
%M    minute
%p    meridian indicator: `AM', `PM'
%S    seconds
%w    day number of week, Sunday is 0
%Y    4-digit year                     %y    2-digit year
%Z    time zone name: `EST'
%-z   zone offset with hour: `-08'     %:::z adds colons as needed: `+05:30'
%5z   zone offset with mins: `-0800'   %:z   adds colon: `-08:00'

Non-date items:
%%    literal percent character: \"%\"
%f    file name without directory      %F    absolute file name
%l    login name                       %L    full name of logged-in user
%q    unqualified host name            %Q    fully-qualified host name
%h    mail host name

The % may be followed by a modifier affecting the letter case.
The modifier \"#\" changes the case of letters, usually to uppercase,
or if the word is already uppercase, to lowercase.
The modifier \"^\" converts letters to uppercase;
\"^\" may be followed by \"#\" to convert to lowercase.
The modifier \"*\" converts words to title case (capitalized).

Here are some example conversions on Mondays, in two locales:

        English         French
%A      Monday          lundi
%^A     MONDAY          LUNDI
%^#A    monday          lundi
%*A     Monday          Lundi

Decimal digits before the type character specify the minimum field
width.  A \"0\" before the field width adds insignificant zeroes
as appropriate, otherwise the padding is done with spaces.

If no padding is specified, a field that can be one or two digits is
padded with \"0\" to two digits if necessary.  Follow the % with \"_\"
to pad with a space instead, or follow it with \"-\" to suppress this
padding entirely.
Thus, on the 5th of the month, the day is converted as follows:

\"%d\"  -> \"05\"
\"%_d\" -> \" 5\"
\"%-d\" -> \"5\"

For example, to get a common format used by the \"date\" command,
use \"%a %b %_d %H:%M:%S %Z %Y\".

The values of non-numeric formatted items depend on the locale
setting recorded in `system-time-locale' and `locale-coding-system'.
The examples here are for the default (\"C\") locale.
`time-stamp-time-zone' controls the time zone used.

Some of the conversions recommended here work only in Emacs 27 or later.
The title-case and lowercase modifiers work only in Emacs 31 or later.
If your files might be edited by older versions of Emacs also, you should
limit yourself to the formats recommended by that older version."
  :type 'string
  :version "31.1")
;;;###autoload(put 'time-stamp-format 'safe-local-variable #'stringp)


(defcustom time-stamp-active t
  "Non-nil enables time-stamping of buffers by \\[time-stamp].
Can be toggled by \\[time-stamp-toggle-active] as an easy way to
temporarily disable `time-stamp' while saving a file.

This option does not affect when `time-stamp' is run, only what it
does when it runs.  To activate automatic time-stamping of buffers
when they are saved, either add this line to your init file:
    (add-hook \\='before-save-hook \\='time-stamp)
or customize option `before-save-hook'.

To enable automatic time-stamping for only a specific file, add this
line to a local variables list near the end of the file:
    eval: (add-hook \\='before-save-hook \\='time-stamp nil t)

See also the variable `time-stamp-warn-inactive'."
  :type 'boolean)


(defcustom time-stamp-warn-inactive t
  "Have \\[time-stamp] warn if a buffer did not get time-stamped.
If non-nil, a warning is displayed if `time-stamp-active' has
deactivated time stamping and the buffer contains a template that
otherwise would have been updated."
  :type 'boolean
  :version "19.29")


(defcustom time-stamp-time-zone nil
  "The time zone used by \\[time-stamp].  t uses UTC.
nil (the default) uses local time, and t or \"UTC0\" uses
Coordinated Universal Time (UTC).  For other possible values,
see the documentation of the ZONE argument of `format-time-string'."
  :type '(choice (const :tag "Emacs local time" nil)
                 (const :tag "Universal Time" t)
                 (const :tag "system wall clock time" wall)
                 (string :tag "TZ environment variable value")
                 (list :tag "Offset and name"
                       (integer :tag "Offset (seconds east of UTC)")
                       (string :tag "Time zone abbreviation"))
                 (integer :tag "Offset (seconds east of UTC)"))
  :version "20.1")
;;;###autoload(put 'time-stamp-time-zone 'safe-local-variable #'time-stamp-zone-type-p)


;;;###autoload
(defun time-stamp-zone-type-p (zone)
  "Return non-nil if ZONE looks like a valid timezone rule.
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
;;; or time-stamp-count in your init file or you will be incompatible
;;; with other people's files.  It is fine to change these variables
;;; in the local variables list of the file itself.


(defvar time-stamp-line-limit 8	    ;Do not change!
  "Lines searched; positive counts from start, negative from end.
The patterns `time-stamp-start' and `time-stamp-end' must be found
in the first (last) `time-stamp-line-limit' lines of the file for
\\[time-stamp] to update the region between them with the current
time stamp.  A value of 0 searches the entire buffer (use with care).

It may be more convenient to use `time-stamp-pattern' if you set more
than one of `time-stamp-line-limit', `time-stamp-start', `time-stamp-end',
or `time-stamp-format'.

These variables are best changed with file-local variables.
If you were to change `time-stamp-line-limit', `time-stamp-start',
`time-stamp-end', or `time-stamp-pattern' in your init file, you
would be incompatible with other people's files.")
;;;###autoload(put 'time-stamp-line-limit 'safe-local-variable #'integerp)


(defvar time-stamp-start "Time-stamp:[ \t]+\\\\?[\"<]+"    ;Do not change!
  "Regexp after which the time stamp is written by \\[time-stamp].

It may be more convenient to use `time-stamp-pattern' if you set more
than one of `time-stamp-line-limit', `time-stamp-start', `time-stamp-end',
or `time-stamp-format'.

These variables are best changed with file-local variables.
If you were to change `time-stamp-line-limit', `time-stamp-start',
`time-stamp-end', or `time-stamp-pattern' in your init file, you
would be incompatible with other people's files.")
;;;###autoload(put 'time-stamp-start 'safe-local-variable #'stringp)


(defvar time-stamp-end "\\\\?[\">]"    ;Do not change!
  "Regexp marking the text after the time stamp.
\\[time-stamp] deletes the text between the first match of `time-stamp-start'
and the following match of `time-stamp-end', then writes the
time stamp specified by `time-stamp-format' between them.

It may be more convenient to use `time-stamp-pattern' if you set more
than one of `time-stamp-line-limit', `time-stamp-start', `time-stamp-end',
or `time-stamp-format'.

The end text normally starts on the same line as the start text ends,
but if there are any newlines in `time-stamp-format', the same number
of newlines must separate the start and end.  Thus \\[time-stamp] tries
to not change the number of lines in the buffer; `time-stamp-inserts-lines'
controls this behavior.

These variables are best changed with file-local variables.
If you were to change `time-stamp-line-limit', `time-stamp-start',
`time-stamp-end', `time-stamp-pattern', or `time-stamp-inserts-lines' in
your init file, you would be incompatible with other people's files.")
;;;###autoload(put 'time-stamp-end 'safe-local-variable #'stringp)


(defvar time-stamp-inserts-lines nil    ;Do not change!
  "Whether \\[time-stamp] can change the number of lines in a file.
When `time-stamp-format' contains newline characters, the intent
is ambiguous: does the author want to update a single multi-line
time stamp, or create a repeated time stamp by inserting new lines?
This variable controls the interpretation.

If nil, `time-stamp' tries not to change the number of lines in the
buffer and treats the format as one single, multi-line time stamp.
The `time-stamp-end' must start N lines after the end of
`time-stamp-start', where N is the number of newlines in
`time-stamp-format'.

If this variable is non-nil, `time-stamp' is willing to add lines
to the buffer.  The end pattern must start somewhere in the
remainder of the same line where the start pattern ends.
This behavior lets a file accumulate repeated time stamps.

In the most common case that `time-stamp-format' contains no
newlines, this variable has no effect; the end of the start
and the start of the end are always on the same line.

These variables are best changed with file-local variables.
If you were to change `time-stamp-start', `time-stamp-end' or
`time-stamp-inserts-lines' in your init file, you would be
incompatible with other people's files.")
;;;###autoload(put 'time-stamp-inserts-lines 'safe-local-variable #'booleanp)


(defvar time-stamp-count 1		;Do not change!
  "How many templates \\[time-stamp] will look for in a buffer.

If the value is greater than 1, the same time stamp will be
written in each case.

If you want to insert different text on different lines,
then instead of changing this variable, include a newline (written as
\"\\n\") in `time-stamp-format' or the format part of `time-stamp-pattern'.
See the variable `time-stamp-inserts-lines'.

`time-stamp-count' is best changed with a file-local variable.
If you were to change it in your init file, you would be incompatible
with other people's files.")
;;;###autoload(put 'time-stamp-count 'safe-local-variable (lambda (c) (and (integerp c) (< c 100))))


(defvar time-stamp-pattern nil		;Do not change!
  "Shorthand variable for `time-stamp' location and format values.
This string has four parts, each of which is optional.
These four parts override `time-stamp-line-limit', `time-stamp-start',
`time-stamp-format' and `time-stamp-end', respectively.  See the
documentation for each of these variables for details.

The first part is a number followed by a slash; the number sets the number
of lines at the beginning (negative counts from end) of the file searched
for the time stamp.  The number and the slash may be omitted to use the
value of `time-stamp-line-limit' as the number.

The second part is a regexp identifying the pattern preceding the time stamp.
This part may be omitted to use the value of `time-stamp-start'.

The third part specifies the format of the time stamp inserted.
This part may be \"%%\" to use the value of `time-stamp-format'.

The fourth part is a regexp identifying the pattern following the time stamp.
This part may be omitted to use the value of `time-stamp-end'.

The pattern does not need to match the entire line of the time stamp.
The pattern will update time stamp information on multiple lines if the
pattern includes newlines, which can be written as \"\\n\".

These variables are best changed with file-local variables.
If you were to change `time-stamp-pattern', `time-stamp-line-limit',
`time-stamp-start', or `time-stamp-end' in your init file, you
would be incompatible with other people's files.

Examples:

;; time-stamp-pattern: \"-10/\"
    (sets only `time-stamp-line-limit')

// time-stamp-pattern: \"-9/^Last modified: %%$\"
    (sets `time-stamp-line-limit', `time-stamp-start' and `time-stamp-end')

@c time-stamp-pattern: \"@set Time-stamp: %B %-d, %Y$\"
    (sets `time-stamp-start', `time-stamp-format' and `time-stamp-end')

%% time-stamp-pattern: \"newcommand{\\\\\\\\timestamp}{%%}\"
    (sets `time-stamp-start' and `time-stamp-end')

// time-stamp-pattern: \"10/Author %L\\nRevised %-d %b %Y$\"
    (sets all four variables and updates text on two lines)

See Info node `Time Stamp Customization' for more discussion and more
in-depth examples.


See also `time-stamp-count' and `time-stamp-inserts-lines'.")
;;;###autoload(put 'time-stamp-pattern 'safe-local-variable #'stringp)



;;;###autoload
(defun time-stamp ()
  "Update any time stamp strings (timestamps) in the buffer.
Look for a time stamp template and update it with the current
date, time, author, and/or other info.

The template, which you manually create on one of the first 8 lines
of the file before running this function, by default can look like
one of the following (your choice):
      Time-stamp: <>
      Time-stamp: \" \"
This function writes the current time between the angle brackets
or quotes, by default formatted like this:
      Time-stamp: <2025-08-07 17:10:21 gildea>

Although you can run this function manually to update a time stamp
once, usually you want automatic time stamp updating.

A time stamp can be automatically updated with current information
every time you save a file.  To enable time-stamping for all files,
customize option `before-save-hook' or add this line to your init file:
    (add-hook \\='before-save-hook \\='time-stamp)

To enable automatic time-stamping for only a specific file, add
this line to a local variables list near the end of the file:
    eval: (add-hook \\='before-save-hook \\='time-stamp nil t)

If the file has no time stamp template or if `time-stamp-active' is nil,
this function does nothing.

You can set `time-stamp-pattern' in a file's local variables list
to customize the information in the time stamp, the surrounding
template, and where in the file it can occur."
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
           (time-stamp--message "time-stamp-line-limit is not an integer")))
    (cond ((not (integerp ts-count))
	   (setq ts-count 1)
           (time-stamp--message "time-stamp-count is not an integer"))
	  ((< ts-count 1)
	   ;; We need to call time-stamp-once at least once
	   ;; to output any warnings about time-stamp not being active.
	   (setq ts-count 1)))
    ;; Figure out what lines the end should be on.
    (if (stringp ts-format)
        (setq format-lines (time-stamp--count-newlines ts-format)))
    (cond
     ((not (and (stringp ts-start)
                (stringp ts-end)))
      (time-stamp--message "time-stamp-start or time-stamp-end is not a string"))
     (t
      (setq end-lines (1+ (time-stamp--count-newlines ts-end)))
      ;; Find overall what lines to look at
      (save-excursion
        (save-restriction
          (widen)
          (cond ((> line-limit 0)
                 (goto-char (setq start (point-min)))
                 (forward-line line-limit)
                 (setq search-limit (point-marker)))
                ((< line-limit 0)
                 (goto-char (setq search-limit (point-max-marker)))
                 (forward-line line-limit)
                 (setq start (point)))
                (t                      ;0 => no limit (use with care!)
                 (setq start (point-min))
                 (setq search-limit (point-max-marker))))))
      (while (and start
                  (< start search-limit)
                  (> ts-count 0))
        (setq start (time-stamp-once start search-limit ts-start ts-end
                                     ts-format format-lines end-lines))
        (setq ts-count (1- ts-count)))
      (set-marker search-limit nil))))
  nil)

(defun time-stamp-once (start search-limit ts-start ts-end
			ts-format format-lines end-lines)
  "Update one time stamp.  Internal routine called by `time-stamp'.
Returns the end point, which is where `time-stamp' begins the next search."
  (let ((case-fold-search nil)
	(end nil)
        (advance-nudge 0)
	end-search-start
	(end-length nil))
    (save-excursion
      (save-restriction
	(widen)
	;; Find the location of the time stamp.
	(while (and (< (goto-char start) search-limit)
		    (not end)
		    (re-search-forward ts-start search-limit 'move))
          ;; Whether or not we find a template, we must
          ;; advance through the buffer.
          (setq advance-nudge (if (> (point) start) 0 1))
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
                            (setq end-length (- (match-end 0) end)))
                        (setq start (+ start advance-nudge)))))))))))
    (if end
	(progn
	  ;; do all warnings outside save-excursion
	  (cond
	   ((not time-stamp-active)
	    (if time-stamp-warn-inactive
                (time-stamp--message
                 "Warning: time-stamp-active is off; did not time-stamp buffer."))
            nil)
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
                            (setq end (point))))))))
            ;; return the location after this time stamp
            (+ end (max advance-nudge end-length))))))))


;;;###autoload
(defun time-stamp-toggle-active (&optional arg)
  "Set `time-stamp-active' (whether \\[time-stamp] updates a buffer).
If ARG is unset, toggle `time-stamp-active'.  With an arg, set
`time-stamp-active' to t (turning on time stamping) if
ARG is positive, otherwise nil."
  (interactive "P")
  (setq time-stamp-active
	(if (null arg)
	    (not time-stamp-active)
	  (> (prefix-numeric-value arg) 0)))
  (message "time-stamp is now %s." (if time-stamp-active "active" "off")))

(defun time-stamp--format (format time)
  "FORMAT a TIME in zone `time-stamp-time-zone'.
Internal helper used by `time-stamp-string-preprocess'."
  (format-time-string format time time-stamp-time-zone))

(defun time-stamp-string (&optional ts-format time)
  "Return the time, date and other info formatted for `time-stamp'.
Optional first argument TS-FORMAT gives the format to use; it defaults
to the value of `time-stamp-format'.  Thus, with no arguments,
this function returns the string `time-stamp' would use to update
its template in the buffer.  The format accepted is similar to the
format used by `format-time-string' with some extensions; see the
documentation of `time-stamp-format' for details.
Optional second argument TIME is only for testing; normally the current
time is used.  The time zone is determined by `time-stamp-time-zone'."
  (if (stringp (or ts-format (setq ts-format time-stamp-format)))
      (time-stamp-string-preprocess ts-format time)))


(defconst time-stamp-no-file "(no file)"
  "String to use when the buffer is not associated with a file.")

;;; time-stamp is transitioning to be more compatible with format-time-string.
;;; This function implements the differences.
;;;      At all times, all the formats recommended in the doc string
;;; of time-stamp-format will work not only in the current version of
;;; Emacs, but in all versions that have been released within the past
;;; five years.
;;;      The : modifier is a temporary conversion feature used to resolve
;;; ambiguous formats--formats that are changing (over time) incompatibly.

(defun time-stamp-string-preprocess (format &optional time)
  "Use FORMAT to format date, time, and user information.
Optional second argument TIME is only for testing.
This is an internal routine implementing extensions to `format-time-string'
and all `time-stamp-format' compatibility."
  (let*
      ((fmt-len (length format))
       (ind 0)
       cur-char
       (result nil)
       (handle-one-conversion
        (lambda ()
	  (let ((prev-char nil)
		(field-width "")
		field-result
                (colon-cnt 0)
		(change-case nil)
                (title-case nil)
		(upcase nil)
		(flag-pad-with-spaces nil)
		(flag-pad-with-zeros nil)
		(flag-minimize nil)
		(paren-level 0))
	    ;; eat any additional args to allow for future expansion
	    (while (progn
		     (setq ind (1+ ind))
		     (setq cur-char (if (< ind fmt-len)
				        (aref format ind)
				      ?\0))
                     (or (eq ?. cur-char) (eq ?~ cur-char) (eq ?* cur-char)
		         (eq ?E cur-char) (eq ?O cur-char)
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
			     (let ((field-index ind)
			           (first-digit cur-char))
			       (while (progn
				        (setq ind (1+ ind))
				        (setq cur-char (if (< ind fmt-len)
						           (aref format ind)
						         ?\0))
					(and (<= ?0 cur-char)
					     (>= ?9 cur-char))))
			       (setq field-width
				     (substring format field-index ind))
			       (setq ind (1- ind))
			       (setq cur-char first-digit)
			       t))))
	      (setq prev-char cur-char)
	      ;; some characters we actually use
	      (cond ((eq cur-char ?:)
                     (setq colon-cnt (1+ colon-cnt)))
		    ((eq cur-char ?#)
		     (setq change-case t))
		    ((eq cur-char ?^)
                     (setq upcase t title-case nil change-case nil))
                    ((eq cur-char ?*)
                     (setq title-case t upcase nil change-case nil))
		    ((eq cur-char ?0)
		     (setq flag-pad-with-zeros t))
		    ((eq cur-char ?-)
		     (setq field-width "1" flag-minimize t))
		    ((eq cur-char ?_)
		     (setq field-width "2" flag-pad-with-spaces t))))
            (if (> (string-to-number field-width) 99)
                (setq field-width (if flag-pad-with-zeros "099" "99")))
            (setq field-result
                  (cond
	           ((eq cur-char ?%)
	            "%")
	           ((eq cur-char ?a)    ;day of week
                    (time-stamp-do-letter-case
                     nil upcase title-case change-case
                     (if (> colon-cnt 0)
                         (if (string-equal field-width "")
                             (time-stamp--format "%A" time)
                           "")           ;discourage "%:3a"
                       (time-stamp--format "%a" time))))
	           ((eq cur-char ?A)
                    (cond
                     ((and (>= (string-to-number field-width) 1)
                           (<= (string-to-number field-width) 3)
                           (not flag-minimize)
                           (not flag-pad-with-spaces))
                      (time-stamp-conv-warn "%3A" "%#a")
                      (time-stamp--format "%#a" time))
                     ((or (> colon-cnt 0)
                          change-case upcase title-case
                          flag-minimize flag-pad-with-spaces
                          (string-equal field-width ""))
                      (time-stamp-do-letter-case
                       nil upcase title-case change-case
                       (time-stamp--format "%A" time)))
                     (t (time-stamp-conv-warn (format "%%%sA" field-width)
                                              (format "%%#%sA" field-width)
                                              (format "%%:%sA" field-width))
                        (time-stamp--format "%#A" time))))
	           ((eq cur-char ?b)    ;month name
                    (time-stamp-do-letter-case
                     nil upcase title-case change-case
                     (if (> colon-cnt 0)
                         (if (string-equal field-width "")
                             (time-stamp--format "%B" time)
                           "")           ;discourage "%:3b"
                       (time-stamp--format "%b" time))))
		   ((eq cur-char ?B)
                    (cond
                     ((and (>= (string-to-number field-width) 1)
                           (<= (string-to-number field-width) 3)
                           (not flag-minimize)
                           (not flag-pad-with-spaces))
                      (time-stamp-conv-warn "%3B" "%#b")
                      (time-stamp--format "%#b" time))
                     ((or (> colon-cnt 0)
                          change-case upcase title-case
                          flag-minimize flag-pad-with-spaces
                          (string-equal field-width ""))
                      (time-stamp-do-letter-case
                       nil upcase title-case change-case
                       (time-stamp--format "%B" time)))
                     (t (time-stamp-conv-warn (format "%%%sB" field-width)
                                              (format "%%#%sB" field-width)
                                              (format "%%:%sB" field-width))
                        (time-stamp--format "%#B" time))))
	           ((eq cur-char ?d)    ;day of month, 1-31
                    (time-stamp-do-number cur-char colon-cnt field-width time))
	           ((eq cur-char ?H)    ;hour, 0-23
                    (time-stamp-do-number cur-char colon-cnt field-width time))
	           ((eq cur-char ?I)    ;hour, 1-12
                    (time-stamp-do-number cur-char colon-cnt field-width time))
	           ((eq cur-char ?m)    ;month number, 1-12
                    (time-stamp-do-number cur-char colon-cnt field-width time))
	           ((eq cur-char ?M)    ;minute, 0-59
                    (time-stamp-do-number cur-char colon-cnt field-width time))
	           ((eq cur-char ?p)    ;AM or PM
                    (time-stamp-do-letter-case
                     t upcase title-case change-case
                     (time-stamp--format "%p" time)))
	           ((eq cur-char ?P)    ;AM or PM
                    (if (and upcase (not change-case))
                        ""              ;discourage inconsistent "%^P"
                      (time-stamp-do-letter-case
                       t upcase title-case change-case
                       (time-stamp--format "%p" time))))
	           ((eq cur-char ?S)    ;seconds, 00-60
                    (time-stamp-do-number cur-char colon-cnt field-width time))
	           ((eq cur-char ?w)    ;weekday number, Sunday is 0
	            (time-stamp--format "%w" time))
	           ((eq cur-char ?y)    ;year
                    (if (= colon-cnt 0)
                        (if (or (string-equal field-width "")
                                (<= (string-to-number field-width) 2))
                            (string-to-number (time-stamp--format "%y" time))
                          (time-stamp-conv-warn
                           (format "%%%sy" field-width) "%Y")
                          (string-to-number (time-stamp--format "%Y" time)))
                      (time-stamp-conv-warn "%:y" "%Y")
                      (string-to-number (time-stamp--format "%Y" time))))
	           ((eq cur-char ?Y)    ;4-digit year
	            (string-to-number (time-stamp--format "%Y" time)))
	           ((eq cur-char ?z)    ;time zone offset
                    (let ((field-width-num (string-to-number field-width))
                          ;; Handle numeric time zone ourselves, because
                          ;; current-time-zone cannot handle offsets
                          ;; greater than 24 hours.
                          (offset-secs
                           (cond ((numberp time-stamp-time-zone)
                                  time-stamp-time-zone)
                                 ((and (consp time-stamp-time-zone)
                                       (numberp (car time-stamp-time-zone)))
                                  (car time-stamp-time-zone))
                                 ;; interpret text time zone
                                 (t (car (current-time-zone
                                          time time-stamp-time-zone))))))
	              ;; we do our own padding; do not let it be updated further
	              (setq field-width "")
	              (cond (change-case
		             "")        ;discourage %z variations
                            ((and (= colon-cnt 0)
			          (not flag-minimize)
			          (not flag-pad-with-spaces)
			          (not flag-pad-with-zeros)
			          (= field-width-num 0))
                             (time-stamp-conv-warn "%z" "%#Z" "%5z")
		             (time-stamp--format "%#Z" time))
			    (t (time-stamp-formatz-from-parsed-options
				flag-minimize
				flag-pad-with-spaces
				flag-pad-with-zeros
                                colon-cnt
				field-width-num
				offset-secs)))))
	           ((eq cur-char ?Z)    ;time zone name
                    (time-stamp-do-letter-case
                     t upcase title-case change-case
                     (time-stamp--format "%Z" time)))
	           ((eq cur-char ?f)    ;buffer-file-name, base name only
	            (if buffer-file-name
                        (time-stamp-filtered-buffer-file-name :nondirectory)
	              time-stamp-no-file))
	           ((eq cur-char ?F)    ;buffer-file-name, absolute name
                    (if buffer-file-name
                        (time-stamp-filtered-buffer-file-name :absolute)
                      time-stamp-no-file))
	           ((eq cur-char ?s)    ;system name, legacy
		    (time-stamp-conv-warn "%s" "%Q")
                    (time-stamp--system-name :full))
	           ((eq cur-char ?u)    ;user name, legacy
		    (time-stamp-conv-warn "%u" "%l")
	            (user-login-name))
	           ((eq cur-char ?U)    ;user full name, legacy
		    (time-stamp-conv-warn "%U" "%L")
	            (user-full-name))
	           ((eq cur-char ?l)    ;login name
	            (user-login-name))
	           ((eq cur-char ?L)    ;full name of logged-in user
	            (user-full-name))
	           ((eq cur-char ?h)    ;mail host name
                    (or mail-host-address (time-stamp--system-name :full)))
                   ((or (eq cur-char ?q)  ;unqualified host name
                        (eq cur-char ?x)) ;short system name, experimental
                    (time-stamp--system-name :short))
                   ((or (eq cur-char ?Q)  ;fully-qualified host name
                        (eq cur-char ?X)) ;full system name, experimental
                    (time-stamp--system-name :full))
	           ))
            (if (numberp field-result)
                (progn
                  (and (= colon-cnt 0)
                       (or (string-equal field-width "")
                           (string-equal field-width "0"))
                       ;; no width provided; set width for default
                       (setq field-width "02"))
	          (format (format "%%%sd" field-width)
                          (or field-result "")))
              (let* ((field-width-num (string-to-number field-width))
                     (needed-padding (- field-width-num
                                        (string-width (or field-result "")))))
                (if (> needed-padding 0)
                    (concat (make-string needed-padding ?\s) field-result)
                  field-result)))
            )))) ;end of handle-one-conversion
    ;; iterate over the format string
    (while (< ind fmt-len)
      (setq cur-char (aref format ind))
      (push (cond ((eq cur-char ?%)
                   (funcall handle-one-conversion))
                  (t
                   (char-to-string cur-char)))
            result)
      (setq ind (1+ ind)))
    (apply #'concat (nreverse result))))

(defun time-stamp-do-letter-case (change-is-downcase
                                  upcase title-case change-case text)
  "Apply upper- and lower-case conversions to TEXT per the flags.
CHANGE-IS-DOWNCASE non-nil indicates that modifier CHANGE-CASE
requests lowercase, otherwise the modifier requests uppercase.
UPCASE is non-nil if the \"^\" modifier is active.
TITLE-CASE is non-nil if the \"*\" modifier is active.
CHANGE-CASE is non-nil if the \"#\" modifier is active.
This is an internal helper for `time-stamp-string-preprocess'."
  (cond ((and upcase change-case)
         (downcase text))
        ((and title-case change-case)
         (upcase text))
        ((and change-is-downcase change-case)
         (downcase text))
        ((or change-case upcase)
         (upcase text))
        (title-case
         (capitalize text))
        (t
         text)))

(defun time-stamp-do-number (format-char colon-count field-width time)
  "Handle a FORMAT-CHAR mostly compatible with `format-time-string'.
The default width/padding may be different from `format-time-string'.
COLON-COUNT is non-0 if \":\" was specified.  FIELD-WIDTH is the string
width specification or \"\".  TIME is the time to convert.
This is an internal helper for `time-stamp-string-preprocess'."
  (let ((format-string (concat "%" (char-to-string format-char))))
    (if (and (> colon-count 0) (not (string-equal field-width "")))
	""				;discourage "%:2d" and the like
      (string-to-number (time-stamp--format format-string time)))))

(defun time-stamp-filtered-buffer-file-name (type)
  "Return a printable string representing the buffer file name.
Non-graphic characters are replaced by ?.  TYPE is :absolute
for the full name or :nondirectory for base name only."
  (declare (ftype (function ((member :absolute :nondirectory)) string)))
  (let ((file-name buffer-file-name)
        (safe-character-filter
         (lambda (chr)
           (let ((category (get-char-code-property chr 'general-category)))
             (if (or
                  ;; Letter, Mark, Number, Punctuation, or Symbol
                  (memq (aref (symbol-name category) 0) '(?L ?M ?N ?P ?S))
                  ;; spaces of various widths, but not ctrl chars like CR or LF
                  (eq category 'Zs))
                 chr
               ;; substitute "?" for format or control character
               ??)))))
    (when (eq type :nondirectory)
      (setq file-name (file-name-nondirectory file-name)))
    (apply #'string (mapcar safe-character-filter file-name))))

(defun time-stamp--count-newlines (str)
  "Return the number of newlines in STR."
  (declare (pure t))
  (let ((nl-count 0)
        (nl-start 0))
    (while (setq nl-start (string-match-p "\n" str nl-start))
      (setq nl-count (1+ nl-count)
            nl-start (1+ nl-start)))
    nl-count))

(defun time-stamp--message (warning-string)
  "Display WARNING-STRING for one second."
  (message "%s" warning-string)
  (sit-for 1))

(defun time-stamp--system-name (type)
  "Return the host name of this system.
TYPE is :short for the unqualified name, :full for the full name."
  (time-stamp--system-name-1 (system-name) type))

(defun time-stamp--system-name-1 (sysname type)
  "Return SYSNAME, shortened if TYPE is :short."
  (declare (pure t))
  (let (first-dot)
    (if (and (eq type :short)
             (setq first-dot (string-match-p "\\." sysname)))
        (substring sysname 0 first-dot)
      sysname)))

(defvar time-stamp-conversion-warn t
  "Enable warnings for old formats in `time-stamp-format'.
When non-nil, `time-stamp' warns about unstable and
soon-to-be-changing conversions found in that buffer's
`time-stamp-format' value.  The warning is displayed only
when a buffer's time stamp is updated; merely viewing a file
does not warn.

If nil, these warnings are disabled, which would be a bad idea.
Since you are changing your file anyway, please make one more
change and update its local variables list.

The recommended replacements will work with old versions of Emacs.
New formats are being recommended now to allow `time-stamp-format'
to change in the future to be compatible with `format-time-string'.
The new formats being recommended now will continue to work then.")


(defun time-stamp-conv-warn (old-format new-format &optional standard-format)
  "Display a warning about a soon-to-be-obsolete format.
Suggests replacing OLD-FORMAT with NEW-FORMAT (same effect, but stable)
or (if provided) STANDARD-FORMAT (the effect the user may have expected
if they didn't read the documentation).
This is an internal function called by `time-stamp'."
  (cond
   (time-stamp-conversion-warn
    (with-current-buffer (get-buffer-create "*Time-stamp-compatibility*")
      (goto-char (point-max))
      (cond
       ((bobp)
        (insert
         (substitute-quotes
          (concat
           "The conversions recognized in `time-stamp-format' will change in a future\n"
           "release to be more compatible with the function `format-time-string'.\n"
           (cond
            (standard-format
             (concat
              "Conversions that are changing are ambiguous and are best replaced by\n"
              "stable conversions that make your intention clear.\n")))
           "\n"
           "The following obsolescent `time-stamp-format' conversion(s) were found:\n\n")))))
      (insert old-format " -- use " new-format)
      (if standard-format
          (insert " or " standard-format))
      (insert "\n")
      (help-make-xrefs))
    (display-buffer "*Time-stamp-compatibility*"))))


;;; A principled, expressive implementation of time zone offset
;;; formatting ("%z" and variants).

;;; * Overarching principle for %z

;; The output should be clear and complete.
;;
;; That is,
;; a) it should be unambiguous what offset is represented, and
;; b) it should be possible to exactly recreate the offset.

;;; * Principles for %z

;; - The numeric fields are HHMMSS.
;; - The fixed point is at the left.  The first 2 digits are always
;;   hours, the next 2 (if they exist) minutes, and next 2 (if they
;;   exist) seconds.  "+11" is 11 hours (not 11 minutes, not 11 seconds).
;;   "+1015" is 10 hours 15 minutes (not 10 minutes 15 seconds).
;; - Each of the three numeric fields is two digits.
;;   "+1" and "+100" are illegal.  (Is that 1 hour? 10 hours? 100 hours?)
;; - The MMSS fields may be omitted only if both are 00.  Thus, the width
;;   of the field depends on the data.  (This is similar to how
;;   %B is always long enough to spell the entire month name.)
;; - The SS field may be omitted only if it is 00.
;; - Colons between the numeric fields are an option, unless the hours
;;   field is greater than 99, when colons are needed to prevent ambiguity.
;; - If padding with zeros, we must pad on the right, because the
;;   fixed point is at the left.  (This is similar to how %N,
;;   fractional seconds, must add its zeros on the right.)
;; - After zero-padding has filled out minutes and seconds with zeros,
;;   further padding can be blanks only.
;;   Any additional zeros would be confusing.

;;; * Padding for %z

;; Padding is under-specified, so we had to make choices.
;;
;; Principles guiding our choices:
;;
;; - The syntax should be easy to remember and the effect predictable.
;; - The syntax should enable as many useful effects as possible.
;;
;; Padding choices:
;;
;; - By default, pad with spaces, as other formats with non-digits do.
;;   The "0" flag pads first with zeros, until seconds are filled out.
;; - If padding with spaces, pad on the right.  This is consistent with
;;   how zero-padding works.  Padding on the right also keeps the fixed
;;   point in the same place, as other formats do for any given width.
;; - The %_z format always outputs seconds, allowing all added padding
;;   to be spaces.  Without this rule, there would be no way to
;;   request seconds that worked for both 2- and 3-digit hours.
;;   (We consider 3-digit hours not because such offsets are in use but
;;   instead to guide our design toward consistency and extensibility.)
;; - Conflicting options are rejected, lest users depend
;;   on incidental behavior.
;;
;; Padding combos that make no sense and are thus disallowed:
;;
;; %-:z   - minus minimizes to hours, : expands to minutes
;; %-::z  - minus minimizes to hours, :: expands to seconds
;; %_:z   - underscore requires seconds, : displays minutes
;; %_:::z - underscore requires seconds, ::: minimizes to hours
;;
;; Example padding effects (with offsets of 99 and 100 hours):
;;
;; %-7z   "+99    "   "+100:00"
;;  %7z   "+9900  "   "+100:00"
;; %07z   "+990000"   "+100:00"
;; %_7z   "+990000"   "+100:00:00"
;;
;; %7:::z "+99    "   "+100:00"
;; %7:z   "+99:00 "   "+100:00"
;; %07:z  "+99:00:00" "+100:00"
;; %7::z  "+99:00:00" "+100:00:00"

;;; * ABNF syntax of the offset string produced by %z

;; offset = sign ( hours [minutes [seconds]] /
;;                 hours [":" minutes [":" seconds]] /
;;                 bighours ":" minutes [":" seconds] ) padding
;; sign = "+" / "-"
;; hours = digitpair
;; minutes = digitpair
;; seconds = digitpair
;; digitpair = digit digit
;; digit = "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
;; bighours = 1*digit digitpair
;; padding = *" "

(defun time-stamp-formatz-from-parsed-options (flag-minimize
                                               flag-pad-spaces-only
                                               flag-pad-zeros-first
                                               colon-count
                                               field-width
                                               offset-secs)
  "Format a time offset according to a %z variation.

With no flags, the output includes hours and minutes: +-HHMM
unless there is a non-zero seconds part, in which case the seconds
are included: +-HHMMSS

FLAG-MINIMIZE is whether \"-\" was specified.  If non-nil, the
output may be limited to hours if minutes and seconds are zero.

FLAG-PAD-SPACES-ONLY is whether \"_\" was specified.  If non-nil,
seconds must be output, so that any padding can be spaces only.

FLAG-PAD-ZEROS-FIRST is whether \"0\" was specified.  If non-nil,
padding to the requested FIELD-WIDTH (if any) is done by adding
00 seconds before padding with spaces.

COLON-COUNT is the number of colons preceding the \"z\" (0-3).  One or
two colons put that many colons in the output (+-HH:MM or +-HH:MM:SS).
Three colons outputs only hours if minutes and seconds are zero and
includes colon separators if minutes and seconds are output.

FIELD-WIDTH is a whole number giving the minimum number of characters
in the output; 0 specifies no minimum.  Additional characters will be
added on the right if necessary.  The added characters will be spaces
unless FLAG-PAD-ZEROS-FIRST is non-nil.

OFFSET-SECS is the time zone offset (in seconds east of UTC) to be
formatted according to the preceding parameters.

This is an internal function used by `time-stamp'."
  ;; Callers of this function need to have already parsed the %z
  ;; format string; this function accepts just the parts of the format.
  ;; `time-stamp-string-preprocess' is the full-fledged parser normally
  ;; used.  The unit test (in time-stamp-tests.el) defines the simpler
  ;; parser `format-time-offset'.
  (let ((hrs (/ (abs offset-secs) 3600))
        (mins (/ (% (abs offset-secs) 3600) 60))
        (secs (% (abs offset-secs) 60))
        (result ""))
    ;; valid option combo?
    (cond
     ((not (or (and flag-minimize (> colon-count 0))
               (and flag-pad-spaces-only (> colon-count 0))
               (and flag-pad-spaces-only flag-minimize)
               (and flag-pad-spaces-only flag-pad-zeros-first)
               (and flag-pad-zeros-first flag-minimize)))
      (setq result (concat result (if (>= offset-secs 0) "+" "-")))
      (setq result (concat result (format "%02d" hrs)))
      ;; Need minutes?
      (cond
       ((or (> hrs 99)
            (> mins 0)
            (> secs 0)
            (not (or flag-minimize (= colon-count 3)))
            (and (> field-width (length result))
                 flag-pad-zeros-first))
        ;; Need colon before minutes?
        (if (or (> colon-count 0)
                (> hrs 99))
            (setq result (concat result ":")))
        (setq result (concat result (format "%02d" mins)))
        ;; Need seconds, too?
        (cond
         ((or (> secs 0)
              (= colon-count 2)
              flag-pad-spaces-only
              (and (> field-width (length result))
                   flag-pad-zeros-first))
          ;; Need colon before seconds?
          (if (or (> colon-count 0)
                  (> hrs 99))
              (setq result (concat result ":")))
          (setq result (concat result (format "%02d" secs)))))))
      ;; Need padding?
      (let ((needed-padding (- field-width (length result))))
        (if (> needed-padding 0)
            (setq result (concat result (make-string needed-padding ?\s)))))))
    result))

(provide 'time-stamp)

;;; time-stamp.el ends here
