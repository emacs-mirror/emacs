;;; icalendar.el --- iCalendar implementation

;; Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

;; Author:   Ulf Jasper <ulf.jasper@web.de>
;; Created:  August 2002
;; Keywords: calendar
;; Human-Keywords: calendar, diary, iCalendar, vCalendar

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is documented in the Emacs Manual.


;;; History:

;;  0.06  Bugfixes regarding icalendar-import-format-*.
;;        Fix in icalendar-convert-diary-to-ical -- thanks to Philipp Grau.

;;  0.05: New import format scheme: Replaced icalendar-import-prefix-*,
;;        icalendar-import-ignored-properties, and
;;        icalendar-import-separator with icalendar-import-format(-*).
;;        icalendar-import-file and icalendar-convert-diary-to-ical
;;        have an extra parameter which should prevent them from
;;        erasing their target files (untested!).
;;        Tested with Emacs 21.3.2

;;  0.04: Bugfix: import: double quoted param values did not work
;;        Read DURATION property when importing.
;;        Added parameter icalendar-duration-correction.

;;  0.03: Export takes care of european-calendar-style.
;;        Tested with Emacs 21.3.2 and XEmacs 21.4.12

;;  0.02: Should work in XEmacs now.  Thanks to Len Trigg for the
;;        XEmacs patches!
;;        Added exporting from Emacs diary to ical.
;;        Some bugfixes, after testing with calendars from
;;        http://icalshare.com.
;;        Tested with Emacs 21.3.2 and XEmacs 21.4.12

;;  0.01: First published version.  Trial version.  Alpha version.

;; ======================================================================
;; To Do:

;;  * Import from ical:
;;    + Need more properties for icalendar-import-format
;;    + check vcalendar version
;;    + check (unknown) elements
;;    + recurring events!
;;    + works for european style calendars only! Does it?
;;    + alarm
;;    + exceptions in recurring events
;;    + the parser is too soft
;;    + error log is incomplete
;;    + nice to have: #include "webcal://foo.com/some-calendar.ics"

;;  * Export into ical
;;    + diary-date, diary-float, and self-made sexp entries are not
;;      understood
;;    + timezones, currently all times are local!

;;  * Other things
;;    + defcustom icalendar-import-ignored-properties does not work with
;;      XEmacs.
;;    + clean up all those date/time parsing functions
;;    + Handle todo items?
;;    + Check iso 8601 for datetime and period
;;    + Which chars to (un)escape?
;;    + Time to find out how the profiler works?


;;; Code:

(defconst icalendar-version 0.06
  "Version number of icalendar.el.")

;; ======================================================================
;; Customizables
;; ======================================================================
(defgroup icalendar nil
  "Icalendar support."
  :prefix "icalendar-"
  :group 'calendar)

(defcustom icalendar-import-format
  "%s%d%l%o"
  "Format string for importing events from iCalendar into Emacs diary.
This string defines how iCalendar events are inserted into diary
file.  Meaning of the specifiers:
%d Description, see `icalendar-import-format-description'
%l Location, see `icalendar-import-format-location'
%o Organizer, see `icalendar-import-format-organizer'
%s Subject, see `icalendar-import-format-subject'"
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-subject
  "%s"
  "Format string defining how the subject element is formatted.
This applies only if the subject is not empty! `%s' is replaced
by the subject."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-description
  "\n Desc: %s"
  "Format string defining how the description element is formatted.
This applies only if the description is not empty! `%s' is
replaced by the description."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-location
  "\n Location: %s"
  "Format string defining how the location element is formatted.
This applies only if the location is not empty! `%s' is replaced
by the location."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-import-format-organizer
  "\n Organizer: %s"
  "Format string defining how the organizer element is formatted.
This applies only if the organizer is not empty! `%s' is
replaced by the organizer."
  :type 'string
  :group 'icalendar)

(defcustom icalendar-duration-correction
  t
  "Workaround for all-day events.
If non-nil the length=duration of iCalendar appointments that
have a length of exactly n days is decreased by one day.  This
fixes problems with all-day events, which appear to be one day
longer than they are."
  :type 'boolean
  :group 'icalendar)


;; ======================================================================
;; NO USER SERVICABLE PARTS BELOW THIS LINE
;; ======================================================================

(defconst icalendar-weekdayabbrev-table
  '(("mon\\(day\\)?"    . "MO")
    ("tue\\(sday\\)?"   . "TU")
    ("wed\\(nesday\\)?" . "WE")
    ("thu\\(rsday\\)?"  . "TH")
    ("fri\\(day\\)?"    . "FR")
    ("sat\\(urday\\)?"  . "SA")
    ("sun\\(day\\)?"    . "SU"))
  "Translation table for weekdays.")

(defconst icalendar-monthnumber-table
  '(("^jan\\(uar\\)?y?$"       . 1)
    ("^feb\\(ruar\\)?y?$"      . 2)
    ("^mar\\(ch\\)?\\|märz?$" . 3)
    ("^apr\\(il\\)?$"          . 4)
    ("^ma[iy]$"                . 5)
    ("^jun[ie]?$"              . 6)
    ("^jul[iy]?$"              . 7)
    ("^aug\\(ust\\)?$"         . 8)
    ("^sep\\(tember\\)?$"      . 9)
    ("^o[ck]t\\(ober\\)?$"     . 10)
    ("^nov\\(ember\\)?$"       . 11)
    ("^de[cz]\\(ember\\)?$"    . 12))
  "Regular expressions for month names.
Currently this matches only German and English.")

(defvar icalendar-debug nil ".")

;; ======================================================================
;; all the other libs we need
;; ======================================================================
(require 'calendar)
(require 'appt)

;; ======================================================================
;; Core functionality
;; Functions for parsing icalendars, importing and so on
;; ======================================================================

(defun icalendar-get-unfolded-buffer (folded-ical-buffer)
  "Return a new buffer containing the unfolded contents of a buffer.
Folding is the iCalendar way of wrapping long lines.  In the
created buffer all occurrences of CR LF BLANK are replaced by the
empty string.  Argument FOLDED-ICAL-BUFFER is the unfolded input
buffer."
  (let ((unfolded-buffer (get-buffer-create " *icalendar-work*")))
    (save-current-buffer
      (set-buffer unfolded-buffer)
      (erase-buffer)
      (insert-buffer folded-ical-buffer)
      (while (re-search-forward "\r?\n[ \t]" nil t)
        (replace-match "" nil nil))
      )
    unfolded-buffer))

;; Replace regexp RE with RP in string ST and return the new string.
;; This is here for compatibility with XEmacs.
(defsubst icalendar-rris (re rp st)
  ;; XEmacs:
  (if (fboundp 'replace-in-string)
      (save-match-data ;; apparently XEmacs needs save-match-data
        (replace-in-string st re rp))
    ;; Emacs:
    (replace-regexp-in-string re rp st)))

(defun icalendar-read-element (invalue inparams)
  "Recursively read the next iCalendar element in the current buffer.
INVALUE gives the current iCalendar element we are reading.
INPARAMS gives the current parameters.....
This function calls itself recursively for each nested calendar element
it finds"
  (let (element children line name params param param-name param-value
                value
        (continue t))
    (setq children '())
    (while (and continue
                (re-search-forward "^\\([A-Za-z0-9-]+\\)[;:]" nil t))
      (setq name (intern (match-string 1)))
      (backward-char 1)
      (setq params '())
      (setq line '())
      (while (looking-at ";")
        (re-search-forward ";\\([A-Za-z0-9-]+\\)=" nil nil)
        (setq param-name (intern (match-string 1)))
        (re-search-forward "\\(\\([^;,:\"]+\\)\\|\"\\([^\"]+\\)\"\\)[;:]"
                           nil t)
        (backward-char 1)
        (setq param-value (or (match-string 2) (match-string 3)))
        (setq param (list param-name param-value))
        (while (looking-at ",")
          (re-search-forward "\\(\\([^;,:]+\\)\\|\"\\([^\"]+\\)\"\\)"
                             nil t)
          (if (match-string 2)
              (setq param-value (match-string 2))
            (setq param-value (match-string 3)))
          (setq param (append param param-value)))
        (setq params (append params param)))
      (unless (looking-at ":")
        (error "Oops"))
      (forward-char 1)
      (re-search-forward  "\\(.*\\)\\(\r?\n[ \t].*\\)*" nil t)
      (setq value (icalendar-rris "\r?\n[ \t]" "" (match-string 0)))
      (setq line (list name params value))
      (cond ((eq name 'BEGIN)
             (setq children
                   (append children
                           (list (icalendar-read-element (intern value)
                                                         params)))))
            ((eq name 'END)
             (setq continue nil))
            (t
             (setq element (append element (list line))))))
    (if invalue
        (list invalue inparams element children)
      children)))

;; ======================================================================
;; helper functions for examining events
;; ======================================================================

(defsubst icalendar-get-all-event-properties (event)
  "Return the list of properties in this EVENT."
  (car (cddr event)))

(defun icalendar-get-event-property (event prop)
  "For the given EVENT return the value of the property PROP."
  (catch 'found
    (let ((props (car (cddr event))) pp)
      (while props
        (setq pp (car props))
        (if (eq (car pp) prop)
            (throw 'found (car (cddr pp))))
        (setq props (cdr props))))
    nil))

(defun icalendar-set-event-property (event prop new-value)
  "For the given EVENT set the property PROP to the value NEW-VALUE."
  (catch 'found
    (let ((props (car (cddr event))) pp)
      (while props
        (setq pp (car props))
        (when (eq (car pp) prop)
          (setcdr (cdr pp) new-value)
          (throw 'found (car (cddr pp))))
        (setq props (cdr props)))
      (setq props (car (cddr event)))
      (setcar (cddr event)
              (append props (list (list prop nil new-value)))))))

(defun icalendar-get-children (node name)
  "Return all children of the given NODE which have a name NAME.
For instance the VCALENDAR node can have VEVENT children as well as VTODO
children."
  (let ((result nil)
        (children (cadr (cddr node))))
    (when (eq (car node) name)
      (setq result node))
    ;;(message "%s" node)
    (when children
      (let ((subresult
             (delq nil
		   (mapcar (lambda (n)
			     (icalendar-get-children n name))
			   children))))
        (if subresult
            (if result
                (setq result (append result subresult))
              (setq result subresult)))))
    result))

; private
(defun icalendar-all-events (icalendar)
  "Return the list of all existing events in the given ICALENDAR."
  (interactive "")
  (icalendar-get-children (car icalendar) 'VEVENT))

(defun icalendar-split-value (value-string)
  "Splits VALUE-STRING at ';='."
  (let ((result '())
        param-name param-value)
    (when value-string
      (save-current-buffer
        (set-buffer (get-buffer-create " *ical-temp*"))
        (set-buffer-modified-p nil)
        (erase-buffer)
        (insert value-string)
        (goto-char (point-min))
        (while
	    (re-search-forward
	     "\\([A-Za-z0-9-]+\\)=\\(\\([^;,:]+\\)\\|\"\\([^\"]+\\)\"\\);?"
	     nil t)
          (setq param-name (intern (match-string 1)))
          (setq param-value (match-string 2))
          (setq result
		(append result (list (list param-name param-value)))))))
    result))

(defun icalendar-decode-isodatetime (isodatetimestring)
  "Return ISODATETIMESTRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.  If ISODATETIMESTRING
specifies UTC time (trailing letter Z) the decoded time is given in
the local time zone! FIXME: TZID-attributes are ignored....! FIXME:
multiple comma-separated values should be allowed!"
  (icalendar-dmsg isodatetimestring)
  (if isodatetimestring
      ;; day/month/year must be present
      (let ((year  (read (substring isodatetimestring 0 4)))
            (month (read (substring isodatetimestring 4 6)))
            (day   (read (substring isodatetimestring 6 8)))
            (hour 0)
            (minute 0)
            (second 0))
        (when (> (length isodatetimestring) 12)
	  ;; hour/minute present
          (setq hour (read (substring isodatetimestring 9 11)))
          (setq minute (read (substring isodatetimestring 11 13))))
        (when (> (length isodatetimestring) 14)
	  ;; seconds present
          (setq second (read (substring isodatetimestring 13 15))))
        (when (and (> (length isodatetimestring) 15)
		   ;; UTC specifier present
                   (char-equal ?Z (aref isodatetimestring 15)))
          ;; if not UTC add current-time-zone offset
          (setq second (+ (car (current-time-zone)) second)))
        ;; create the decoded date-time
        ;; FIXME!?!
        (condition-case nil
            (decode-time (encode-time second minute hour day month year))
          (error
           (message "Cannot decode \"%s\"" isodatetimestring)
           ;; hope for the best...
           (list second minute hour day month year 0 nil 0))))
    ;; isodatetimestring == nil
    nil))

(defun icalendar-decode-isoduration (isodurationstring)
  "Return ISODURATIONSTRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.  If ISODURATIONSTRING
specifies UTC time (trailing letter Z) the decoded time is given in
the local time zone! FIXME: TZID-attributes are ignored....! FIXME:
multiple comma-separated values should be allowed!"
  (if isodurationstring
      (save-match-data
        (string-match
         (concat
          "^P[+-]?\\("
          "\\(\\([0-9]+\\)D\\)"         ; days only
          "\\|"
          "\\(\\(\\([0-9]+\\)D\\)?T\\(\\([0-9]+\\)H\\)?" ; opt days
          "\\(\\([0-9]+\\)M\\)?\\(\\([0-9]+\\)S\\)?\\)" ; mand. time
          "\\|"
          "\\(\\([0-9]+\\)W\\)"         ; weeks only
          "\\)$") isodurationstring)
        (let ((seconds 0)
              (minutes 0)
              (hours 0)
              (days 0)
              (months 0)
              (years 0))
        (cond
         ((match-beginning 2)           ;days only
          (setq days (read (substring isodurationstring
                                      (match-beginning 3)
                                      (match-end 3))))
          (when icalendar-duration-correction
            (setq days (1- days))))
         ((match-beginning 4)           ;days and time
          (if (match-beginning 5)
              (setq days (* 7 (read (substring isodurationstring
                                               (match-beginning 6)
                                               (match-end 6))))))
          (if (match-beginning 7)
              (setq hours (read (substring isodurationstring
                                           (match-beginning 8)
                                           (match-end 8)))))
          (if (match-beginning 9)
              (setq minutes (read (substring isodurationstring
                                             (match-beginning 10)
                                             (match-end 10)))))
          (if (match-beginning 11)
              (setq seconds (read (substring isodurationstring
                                             (match-beginning 12)
                                             (match-end 12)))))
          )
         ((match-beginning 13)          ;weeks only
          (setq days (* 7 (read (substring isodurationstring
                                           (match-beginning 14)
                                           (match-end 14))))))
         )
        (list seconds minutes hours days months years)))
    ;; isodatetimestring == nil
    nil))

(defun icalendar-add-decoded-times (time1 time2)
  "Add TIME1 to TIME2.
Both times must be given in decoded form.  One of these times must be
valid (year > 1900 or something)."
  ;; FIXME: does this function exist already?
  (decode-time (encode-time
                (+ (nth 0 time1) (nth 0 time2))
                (+ (nth 1 time1) (nth 1 time2))
                (+ (nth 2 time1) (nth 2 time2))
                (+ (nth 3 time1) (nth 3 time2))
                (+ (nth 4 time1) (nth 4 time2))
                (+ (nth 5 time1) (nth 5 time2))
                nil
                nil
                ;;(or (nth 6 time1) (nth 6 time2)) ;; FIXME?
                )))

(defun icalendar-datetime-to-noneuropean-date (datetime)
  "Convert the decoded DATETIME to non-european-style format.
Non-European format: (month day year)."
  (if datetime
      (list (nth 4 datetime) ;month
            (nth 3 datetime) ;day
            (nth 5 datetime));year
    ;; datetime == nil
    nil))

(defun icalendar-datetime-to-european-date (datetime)
  "Convert the decoded DATETIME to European format.
European format: (day month year).
FIXME"
  (if datetime
      (format "%d %d %d" (nth 3 datetime); day
              (nth 4 datetime) ;month
              (nth 5 datetime));year
    ;; datetime == nil
    nil))

(defun icalendar-datetime-to-colontime (datetime)
  "Extract the time part of a decoded DATETIME into 24-hour format.
Note that this silently ignores seconds."
  (format "%02d:%02d" (nth 2 datetime) (nth 1 datetime)))

(defun icalendar-get-month-number (monthname)
  "Return the month number for the given MONTHNAME."
  (save-match-data
    (let ((case-fold-search t))
      (assoc-default monthname icalendar-monthnumber-table
                     'string-match))))

(defun icalendar-get-weekday-abbrev (weekday)
  "Return the abbreviated WEEKDAY."
  ;;FIXME: ISO-like(?).
  (save-match-data
    (let ((case-fold-search t))
      (assoc-default weekday icalendar-weekdayabbrev-table
                     'string-match))))

(defun icalendar-datestring-to-isodate (datestring &optional day-shift)
  "Convert diary-style DATESTRING to iso-style date.
If DAY-SHIFT is non-nil, the result is shifted by DAY-SHIFT days
-- DAY-SHIFT must be either nil or an integer.  This function
takes care of european-style."
  (let ((day -1) month year)
    (save-match-data
      (cond (;; numeric date
	     (string-match (concat "\\s-*"
				   "0?\\([1-9][0-9]?\\)[ \t/]\\s-*"
				   "0?\\([1-9][0-9]?\\),?[ \t/]\\s-*"
				   "\\([0-9]\\{4\\}\\)")
			   datestring)
	     (setq day (read (substring datestring (match-beginning 1)
					(match-end 1))))
	     (setq month (read (substring datestring (match-beginning 2)
					  (match-end 2))))
	     (setq year (read (substring datestring (match-beginning 3)
					 (match-end 3))))
	     (unless european-calendar-style
	       (let ((x month))
		 (setq month day)
		 (setq day x))))
	    (;; date contains month names -- european-style
	     (and european-calendar-style
		  (string-match (concat "\\s-*"
					"0?\\([123]?[0-9]\\)[ \t/]\\s-*"
					"\\([A-Za-z][^ ]+\\)[ \t/]\\s-*"
					"\\([0-9]\\{4\\}\\)")
				datestring))
		 (setq day (read (substring datestring (match-beginning 1)
					    (match-end 1))))
		 (setq month (icalendar-get-month-number
			      (substring datestring (match-beginning 2)
					 (match-end 2))))
		 (setq year (read (substring datestring (match-beginning 3)
					     (match-end 3)))))
	    (;; date contains month names -- non-european-style
	     (and (not european-calendar-style)
		  (string-match (concat "\\s-*"
					"\\([A-Za-z][^ ]+\\)[ \t/]\\s-*"
					"0?\\([123]?[0-9]\\),?[ \t/]\\s-*"
					"\\([0-9]\\{4\\}\\)")
				datestring))
	     (setq day (read (substring datestring (match-beginning 2)
					(match-end 2))))
	     (setq month (icalendar-get-month-number
			  (substring datestring (match-beginning 1)
				     (match-end 1))))
	     (setq year (read (substring datestring (match-beginning 3)
					 (match-end 3)))))
	    (t
	     nil)))
    (if (> day 0)
	(let ((mdy (calendar-gregorian-from-absolute
		    (+ (calendar-absolute-from-gregorian (list month day year))
		       (or day-shift 0)))))
	  (format "%04d%02d%02d" (nth 2 mdy) (nth 0 mdy) (nth 1 mdy)))
      nil)))

(defun icalendar-dmsg (&rest args)
  "Print message ARGS if `icalendar-debug' is non-nil."
  (if icalendar-debug
      (apply 'message args)))

(defun icalendar-diarytime-to-isotime (timestring ampmstring)
  "Convert a a time like 9:30pm to an iso-conform string like T213000.
In this example the TIMESTRING would be \"9:30\" and the AMPMSTRING
would be \"pm\"."
  (if timestring
      (let ((starttimenum (read (icalendar-rris ":" "" timestring))))
        ;; take care of am/pm style
        (if (and ampmstring (string= "pm" ampmstring))
            (setq starttimenum (+ starttimenum 1200)))
        (format "T%04d00" starttimenum))
    nil))

(defun icalendar-convert-string-for-export (s)
  "Escape comma and other critical characters in string S."
  (icalendar-rris "," "\\\\," s))

(defun icalendar-convert-for-import (string)
  "Remove escape chars for comma, semicolon etc. from STRING."
  (icalendar-rris
   "\\\\n" "\n " (icalendar-rris
               "\\\\\"" "\"" (icalendar-rris
                              "\\\\;" ";" (icalendar-rris
                                           "\\\\," "," string)))))

;; ======================================================================
;; export -- convert emacs-diary to icalendar
;; ======================================================================

(defun icalendar-convert-diary-to-ical (diary-filename ical-filename
                                        &optional do-not-clear-diary-file)
  "Export diary file to iCalendar format -- erases ical-filename!!!.
Argument DIARY-FILENAME is the input `diary-file'.
Argument ICAL-FILENAME is the output iCalendar file.
If DO-NOT-CLEAR-DIARY-FILE is not nil the target iCalendar file
is not erased."
  (interactive "FExport diary data from file:
Finto iCalendar file: ")
  (let ((result "")
        (start 0)
        (entry-main "")
        (entry-rest "")
        (header "")
        (contents)
        (oops nil)
        (nonmarker (concat "^" (regexp-quote diary-nonmarking-symbol)
			   "?")))
    (save-current-buffer
      (set-buffer (find-file diary-filename))
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([^ \t\n].*\\)\\(\n[ \t].*\\)*" nil t)
        (setq entry-main (match-string 1))
        (if (match-beginning 2)
            (setq entry-rest (match-string 2))
          (setq entry-rest ""))
        (setq header (format "\nBEGIN:VEVENT\nUID:emacs%d%d%d"
                             (car (current-time))
                             (cadr (current-time))
                             (car (cddr (current-time)))))
        (setq oops nil)
        (cond
         ;; anniversaries
         ((string-match
           (concat nonmarker
                   "%%(diary-anniversary \\([^)]+\\))\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "diary-anniversary %s" entry-main)
          (let* ((datetime (substring entry-main (match-beginning 1)
                                      (match-end 1)))
                 (summary (icalendar-convert-string-for-export
                           (substring entry-main (match-beginning 2)
                                      (match-end 2))))
                 (startisostring (icalendar-datestring-to-isodate
				  datetime))
                 (endisostring (icalendar-datestring-to-isodate
				datetime 1)))
            (setq contents
                  (concat "\nDTSTART;VALUE=DATE:" startisostring
                          "\nDTEND;VALUE=DATE:" endisostring
                          "\nSUMMARY:" summary
                          "\nRRULE:FREQ=YEARLY;INTERVAL=1"
                          ;; the following is redundant,
                          ;; but korganizer seems to expect this... ;(
                          ;; and evolution doesn't understand it... :(
                          ;; so... who is wrong?!
                          ";BYMONTH=" (substring startisostring 4 6)
                          ";BYMONTHDAY=" (substring startisostring 6 8)
                          )))
          (unless (string= entry-rest "")
            (setq contents (concat contents "\nDESCRIPTION:"
                                   (icalendar-convert-string-for-export
                                    entry-rest)))))
         ;; cyclic events
         ;; %%(diary-cyclic )
         ((string-match
           (concat nonmarker
                   "%%(diary-cyclic \\([^ ]+\\) +"
                   "\\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\))\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "diary-cyclic %s" entry-main)
          (let* ((frequency (substring entry-main (match-beginning 1)
                                       (match-end 1)))
                 (datetime (substring entry-main (match-beginning 2)
                                      (match-end 2)))
                 (summary (icalendar-convert-string-for-export
                           (substring entry-main (match-beginning 3)
                                      (match-end 3))))
                 (startisostring (icalendar-datestring-to-isodate
				  datetime))
                 (endisostring (icalendar-datestring-to-isodate
				datetime 1)))
            (setq contents
                  (concat "\nDTSTART;VALUE=DATE:" startisostring
                          "\nDTEND;VALUE=DATE:" endisostring
                          "\nSUMMARY:" summary
                          "\nRRULE:FREQ=DAILY;INTERVAL=" frequency
                          ;; strange: korganizer does not expect
                          ;; BYSOMETHING here...
                          )))
          (unless (string= entry-rest "")
            (setq contents (concat contents "\nDESCRIPTION:"
                                   (icalendar-convert-string-for-export
                                    entry-rest)))))
         ;; diary-date -- FIXME
         ((string-match
           (concat nonmarker
                   "%%(diary-date \\([^)]+\\))\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "diary-date %s" entry-main)
          (setq oops t))
         ;; float events -- FIXME
         ((string-match
           (concat nonmarker
                   "%%(diary-float \\([^)]+\\))\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "diary-float %s" entry-main)
          (setq oops t))
         ;; block events
         ((string-match
           (concat nonmarker
                   "%%(diary-block \\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\) +"
                   "\\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\))\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "diary-block %s" entry-main)
          (let* ((startstring (substring entry-main (match-beginning 1)
                                         (match-end 1)))
                 (endstring (substring entry-main (match-beginning 2)
                                       (match-end 2)))
                 (summary (icalendar-convert-string-for-export
                           (substring entry-main (match-beginning 3)
                                      (match-end 3))))
                 (startisostring (icalendar-datestring-to-isodate
				  startstring))
                 (endisostring (icalendar-datestring-to-isodate
				endstring 1)))
            (setq contents
                  (concat "\nDTSTART;VALUE=DATE:" startisostring
                          "\nDTEND;VALUE=DATE:" endisostring
                          "\nSUMMARY:" summary
                          ))
            (unless (string= entry-rest "")
              (setq contents (concat contents "\nDESCRIPTION:"
                                     (icalendar-convert-string-for-export
                                      entry-rest))))))
         ;; other sexp diary entries -- FIXME
         ((string-match
           (concat nonmarker
                   "%%(\\([^)]+\\))\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "diary-sexp %s" entry-main)
          (setq oops t))
         ;; weekly by day
         ;; Monday 8:30 Team meeting
         ((and (string-match
                (concat nonmarker
                        "\\([a-z]+\\)\\s-+"
                        "\\(0?\\([1-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                        "\\(-0?"
                        "\\([1-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
                        "\\)?"
                        "\\s-*\\(.*\\)$")
                entry-main)
               (icalendar-get-weekday-abbrev
		(substring entry-main (match-beginning 1) (match-end 1))))
          (icalendar-dmsg "weekly %s" entry-main)
          (let* ((day (icalendar-get-weekday-abbrev
                       (substring entry-main (match-beginning 1)
                                  (match-end 1))))
                 (starttimestring (icalendar-diarytime-to-isotime
                                   (if (match-beginning 3)
                                       (substring entry-main
                                                  (match-beginning 3)
                                                  (match-end 3))
                                     nil)
                                   (if (match-beginning 4)
                                       (substring entry-main
                                                  (match-beginning 4)
                                                  (match-end 4))
                                     nil)))
                 (endtimestring (icalendar-diarytime-to-isotime
                                 (if (match-beginning 6)
                                     (substring entry-main
						(match-beginning 6)
                                                (match-end 6))
                                   nil)
                                 (if (match-beginning 7)
                                     (substring entry-main
						(match-beginning 7)
                                                (match-end 7))
                                   nil)))
                 (summary (icalendar-convert-string-for-export
                           (substring entry-main (match-beginning 8)
                                      (match-end 8)))))
            (when starttimestring
              (unless endtimestring
                (let ((time (read (icalendar-rris "^T0?" ""
						  starttimestring))))
                  (setq endtimestring (format "T%06d" (+ 10000 time))))))
            (setq contents
                  (concat "\nDTSTART"
                          (if starttimestring "" ";VALUE=DATE")
                          ":19000101" ;; FIXME? Probability that this
                          ;; is the right day is 1/7
                          (or starttimestring "")
                          "\nDTEND"
                          (if endtimestring "" ";VALUE=DATE")
                          ":19000101" ;; FIXME?
                          (or endtimestring "")
                          "\nSUMMARY:" summary
                          "\nRRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=" day
                          )))
          (unless (string= entry-rest "")
            (setq contents (concat contents "\nDESCRIPTION:"
                                   (icalendar-convert-string-for-export
                                    entry-rest)))))
         ;; yearly by day
         ;; 1 May Tag der Arbeit
         ((string-match
           (concat nonmarker
                   (if european-calendar-style
                       "0?\\([1-9]+[0-9]?\\)\\s-+\\([a-z]+\\)\\s-+"
                     "\\([a-z]+\\)\\s-+0?\\([1-9]+[0-9]?\\)\\s-+")
                   "\\*?\\s-*"
                   "\\(0?\\([1-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                   "\\("
		   "-0?\\([1-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
		   "\\)?"
                   "\\s-*\\([^0-9]+.*\\)$"; must not match years
                   )
           entry-main)
          (icalendar-dmsg "yearly %s" entry-main)
          (let* ((daypos (if european-calendar-style 1 2))
                 (monpos (if european-calendar-style 2 1))
                 (day (read (substring entry-main (match-beginning daypos)
                                       (match-end daypos))))
                 (month (icalendar-get-month-number
                         (substring entry-main (match-beginning monpos)
                                    (match-end monpos))))
                 (starttimestring (icalendar-diarytime-to-isotime
                                   (if (match-beginning 4)
                                       (substring entry-main
                                                  (match-beginning 4)
                                                  (match-end 4))
                                     nil)
                                   (if (match-beginning 5)
                                       (substring entry-main
                                                  (match-beginning 5)
                                                  (match-end 5))
                                     nil)))
                 (endtimestring (icalendar-diarytime-to-isotime
                                 (if (match-beginning 7)
                                     (substring entry-main
						(match-beginning 7)
                                                (match-end 7))
                                   nil)
                                 (if (match-beginning 8)
                                     (substring entry-main
						(match-beginning 8)
                                                (match-end 8))
                                   nil)))
                 (summary (icalendar-convert-string-for-export
                           (substring entry-main (match-beginning 9)
                                      (match-end 9)))))
            (when starttimestring
              (unless endtimestring
                (let ((time (read (icalendar-rris "^T0?" ""
						  starttimestring))))
                  (setq endtimestring (format "T%06d" (+ 10000 time))))))
            (setq contents
                  (concat "\nDTSTART"
                          (if starttimestring "" ";VALUE=DATE")
                          (format ":1900%02d%02d" month day)
                          (or starttimestring "")
                          "\nDTEND"
                          (if endtimestring "" ";VALUE=DATE")
                          (format ":1900%02d%02d" month day)
                          (or endtimestring "")
                          "\nSUMMARY:" summary
                          "\nRRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH="
                          (format "%2d" month)
                          ";BYMONTHDAY="
                          (format "%2d" day)
                          )))
          (unless (string= entry-rest "")
            (setq contents (concat contents "\nDESCRIPTION:"
                                   (icalendar-convert-string-for-export
                                    entry-rest)))))
         ;; "ordinary" events, start and end time given
         ;; 1 Feb 2003 Hs Hochzeitsfeier, Dreieich
         ((string-match
           (concat nonmarker
                   "\\([^ /]+[ /]+[^ /]+[ /]+[^ ]+\\)\\s-+"
                   "\\(0?\\([1-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?"
                   "\\("
		   "-0?\\([1-9][0-9]?:[0-9][0-9]\\)\\([ap]m\\)?\\)?"
		   "\\)?"
                   "\\s-*\\(.*\\)")
           entry-main)
          (icalendar-dmsg "ordinary %s" entry-main)
          (let* ((datestring (icalendar-datestring-to-isodate
                              (substring entry-main (match-beginning 1)
                                         (match-end 1))))
                 (starttimestring (icalendar-diarytime-to-isotime
                                   (if (match-beginning 3)
                                       (substring entry-main
                                                  (match-beginning 3)
                                                  (match-end 3))
                                     nil)
                                   (if (match-beginning 4)
                                       (substring entry-main
                                                  (match-beginning 4)
                                                  (match-end 4))
                                     nil)))
                 (endtimestring (icalendar-diarytime-to-isotime
                                 (if (match-beginning 6)
                                     (substring entry-main
						(match-beginning 6)
                                                (match-end 6))
                                   nil)
                                 (if (match-beginning 7)
                                     (substring entry-main
						(match-beginning 7)
                                                (match-end 7))
                                   nil)))
                 (summary (icalendar-convert-string-for-export
                           (substring entry-main (match-beginning 8)
                                      (match-end 8)))))
            (when starttimestring
              (unless endtimestring
                (let ((time (read (icalendar-rris "^T0?" ""
						  starttimestring))))
                  (setq endtimestring (format "T%06d" (+ 10000 time))))))
            (setq contents (format
			    "\nDTSTART%s:%s%s\nDTEND%s:%s%s\nSUMMARY:%s"
			    (if starttimestring "" ";VALUE=DATE")
			    datestring
			    (or starttimestring "")
			    (if endtimestring ""
			      ";VALUE=DATE")
			    datestring
			    (or endtimestring "")
			    summary))
            (unless (string= entry-rest "")
              (setq contents (concat contents "\nDESCRIPTION:"
                                     (icalendar-convert-string-for-export
                                      entry-rest))))))
         ;; everything else
         (t
          ;; Oops! what's that?
          (setq oops t)))
        (if oops
            (message "Cannot export entry on line %d"
		     (count-lines (point-min) (point)))
          (setq result (concat result header contents "\nEND:VEVENT"))))
      ;; we're done, insert everything into the file
      (let ((coding-system-for-write 'utf8))
        (set-buffer (find-file ical-filename))
        (unless do-not-clear-diary-file
          (erase-buffer))
        (insert
	 "BEGIN:VCALENDAR\nPRODID:-//Emacs//NONSGML icalendar.el//EN")
        (insert "\nVERSION:2.0")
        (insert result)
        (insert "\nEND:VCALENDAR\n")))))


;; ======================================================================
;; import -- convert icalendar to emacs-diary
;; ======================================================================

;; user function
(defun icalendar-import-file (ical-filename diary-filename
                                            &optional non-marking
                                            do-not-clear-diary-file)
  "Import a iCalendar file and save to a diary file -- erases diary-file!
Argument ICAL-FILENAME output iCalendar file.
Argument DIARY-FILENAME input `diary-file'.
Optional argument NON-MARKING determines whether events are created as
non-marking or not.
If DO-NOT-CLEAR-DIARY-FILE is not nil the target diary file is
not erased."
  (interactive "fImport iCalendar data from file:
Finto diary file (will be erased!):
p")
  ;; clean up the diary file
  (save-current-buffer
    (unless do-not-clear-diary-file
      ;; clear the target diary file
      (set-buffer (find-file diary-filename))
      (erase-buffer))
    ;; now load and convert from the ical file
    (set-buffer (find-file ical-filename))
    (icalendar-extract-ical-from-buffer diary-filename t non-marking)))

; user function
(defun icalendar-extract-ical-from-buffer (&optional
					   diary-file do-not-ask
					   non-marking)
  "Extract iCalendar events from current buffer.

This function searches the current buffer for the first iCalendar
object, reads it and adds all VEVENT elements to the diary
DIARY-FILE.

It will ask for each appointment whether to add it to the diary
when DO-NOT-ASK is non-nil.  When called interactively,
DO-NOT-ASK is set to t, so that you are asked fore each event.

NON-MARKING determines whether diary events are created as
non-marking.

This function attempts to notify about problems that occur when
reading, parsing, or converting iCalendar data!"
  (interactive)
  (save-current-buffer
    ;; prepare ical
    (message "Preparing icalendar...")
    (set-buffer (icalendar-get-unfolded-buffer (current-buffer)))
    (goto-char (point-min))
    (message "Preparing icalendar...done")
    (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (let (ical-contents ical-errors)
          ;; read ical
          (message "Reading icalendar...")
          (beginning-of-line)
          (setq ical-contents (icalendar-read-element nil nil))
          (message "Reading icalendar...done")
          ;; convert ical
          (message "Converting icalendar...")
          (setq ical-errors (icalendar-convert-ical-to-diary
                             ical-contents
                             diary-file do-not-ask non-marking))
          (when diary-file
            ;; save the diary file
            (save-current-buffer
              (set-buffer (find-buffer-visiting diary-file))
              (save-buffer)))
          (message "Converting icalendar...done")
          (if (and ical-errors (y-or-n-p
				(concat "Something went wrong -- "
					"do you want to see the "
					"error log? ")))
              (switch-to-buffer " *icalendar-errors*")))
      (message
       "Current buffer does not contain icalendar contents!"))))

;; ----------------------------------------------------------------------
;; private area
;; ----------------------------------------------------------------------
(defun icalendar-format-ical-event (event)
  "Create a string representation of an iCalendar EVENT."
  (let ((string icalendar-import-format)
        (conversion-list
         '(("%d" DESCRIPTION icalendar-import-format-description)
           ("%s" SUMMARY     icalendar-import-format-subject)
           ("%l" LOCATION    icalendar-import-format-location)
           ("%o" ORGANIZER   icalendar-import-format-organizer))))
    ;; convert the specifiers in the format string
    (mapcar (lambda (i)
              (let* ((spec (car i))
                     (prop (cadr i))
                     (format (car (cddr i)))
                     (contents (icalendar-get-event-property event prop))
                     (formatted-contents ""))
                ;;(message "%s" event)
                ;;(message "contents%s = %s" prop contents)
                (when (and contents (> (length contents) 0))
                  (setq formatted-contents
                        (icalendar-rris "%s"
                                        (icalendar-convert-for-import
                                         contents)
                                        (symbol-value format))))
                (setq string (icalendar-rris spec
                                             formatted-contents
                                             string))))
            conversion-list)
    string))

(defun icalendar-convert-ical-to-diary (ical-list diary-file
                                                  &optional do-not-ask
                                                  non-marking)
  "Convert an iCalendar file to an Emacs diary file.
Import VEVENTS from the iCalendar object ICAL-LIST and saves them to a
DIARY-FILE.  If DO-NOT-ASK is nil the user is asked for each event
whether to actually import it.  NON-MARKING determines whether diary
events are created as non-marking.
This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer ` *icalendar-errors*'."
  (let* ((ev (icalendar-all-events ical-list))
         (error-string "")
         (event-ok t)
         (found-error nil)
         e diary-string)
    ;; step through all events/appointments
    (while ev
      (setq e (car ev))
      (setq ev (cdr ev))
      (setq event-ok nil)
      (condition-case error-val
          (let* ((dtstart (icalendar-decode-isodatetime
                           (icalendar-get-event-property e 'DTSTART)))
                 (start-d (calendar-date-string
                           (icalendar-datetime-to-noneuropean-date
			    dtstart)
                           t t))
                 (start-t (icalendar-datetime-to-colontime dtstart))
                 (dtend (icalendar-decode-isodatetime
                         (icalendar-get-event-property e 'DTEND)))
                 end-d
                 end-t
                 (subject (icalendar-convert-for-import
                           (or (icalendar-get-event-property e 'SUMMARY)
                               "No Subject")))
                 (rrule (icalendar-get-event-property e 'RRULE))
                 (rdate (icalendar-get-event-property e 'RDATE))
                 (duration (icalendar-get-event-property e 'DURATION)))
            (icalendar-dmsg "%s: %s" start-d subject)
            (when duration
              (let ((dtend2 (icalendar-add-decoded-times
                             dtstart
                             (icalendar-decode-isoduration duration))))
                (if (and dtend (not (eq dtend dtend2)))
                    (message "Inconsistent endtime and duration for %s"
                             subject))
                (setq dtend dtend2)))
            (setq end-d (if dtend
                            (calendar-date-string
                             (icalendar-datetime-to-noneuropean-date
			      dtend)
                             t t)
                          start-d))
            (setq end-t (if dtend
                            (icalendar-datetime-to-colontime dtend)
                          start-t))
            (icalendar-dmsg "start-d: %s, end-d: %s" start-d end-d)
            (cond
             ;; recurring event
             (rrule
              (icalendar-dmsg "recurring event")
              (let* ((rrule-props (icalendar-split-value rrule))
                     (frequency (car (cdr (assoc 'FREQ rrule-props))))
                     (until (car (cdr (assoc 'UNTIL rrule-props))))
                     (interval  (read (car (cdr (assoc 'INTERVAL
						       rrule-props))))))
                (cond ((string-equal frequency "WEEKLY")
                       (if (not start-t)
                           (progn
                             ;; weekly and all-day
                             (icalendar-dmsg "weekly all-day")
                             (setq diary-string
                                   (format
				    "%%%%(diary-cyclic %d %s)"
				    (* interval 7)
				    (icalendar-datetime-to-european-date
				     dtstart))))
                         ;; weekly and not all-day
                         (let* ((byday (cadr (assoc 'BYDAY rrule-props)))
                                (weekday
				 (cdr (rassoc
				       byday
				       icalendar-weekdayabbrev-table))))
                           (icalendar-dmsg "weekly not-all-day")
                           (if weekday
                               (setq diary-string
                                     (format "%s %s%s%s" weekday
                                             start-t (if end-t "-" "")
                                             (or end-t "")))
                             ;; FIXME!!!!
                             ;; DTSTART;VALUE=DATE-TIME:20030919T090000
                             ;; DTEND;VALUE=DATE-TIME:20030919T113000
                             (setq diary-string
                                   (format
				    "%%%%(diary-cyclic %s %s) %s%s%s"
				    (* interval 7)
				    (icalendar-datetime-to-european-date
				     dtstart)
				    start-t (if end-t "-" "") (or end-t ""))))
                           (setq event-ok t))))
                      ;; yearly
                      ((string-equal frequency "YEARLY")
                       (icalendar-dmsg "yearly")
                       (setq diary-string
                             (format
			      "%%%%(diary-anniversary %s)"
			      (icalendar-datetime-to-european-date dtstart)))
                       (setq event-ok t))
                      ;; FIXME: war auskommentiert:
                      ((and (string-equal frequency "DAILY")
                            ;;(not (string= start-d end-d))
                            ;;(not start-t)
                            ;;(not end-t)
                            )
                       (let ((ds (icalendar-datetime-to-noneuropean-date
                                  (icalendar-decode-isodatetime
                                   (icalendar-get-event-property e
								 'DTSTART))))
                             (de (icalendar-datetime-to-noneuropean-date
                                  (icalendar-decode-isodatetime
                                   until))))
                         (setq diary-string
                               (format
				"%%%%(diary-block %d %d %d  %d %d %d)"
				(nth 1 ds) (nth 0 ds) (nth 2 ds)
				(nth 1 de) (nth 0 de) (nth 2 de))))
                       (setq event-ok t)))
                ))
             (rdate
              (icalendar-dmsg "rdate event")
              (setq diary-string "")
              (mapcar (lambda (datestring)
                        (setq diary-string
                              (concat diary-string
                                      (format "......"))))
                      (icalendar-split-value rdate)))
             ;; non-recurring event
             ;; long event
             ((not (string= start-d end-d))
              (icalendar-dmsg "non-recurring event")
              (let ((ds (icalendar-datetime-to-noneuropean-date dtstart))
                    (de (icalendar-datetime-to-noneuropean-date dtend)))
                (setq diary-string
                      (format "%%%%(diary-block %d %d %d   %d %d %d)"
                              (nth 1 ds) (nth 0 ds) (nth 2 ds)
                              (nth 1 de) (nth 0 de) (nth 2 de))))
              (setq event-ok t))
             ;; not all-day
             ((and start-t (or (not end-t)
                               (not (string= start-t end-t))))
              (icalendar-dmsg "not all day event")
              (cond (end-t
                     (setq diary-string (format "%s %s-%s" start-d
						start-t end-t)))
                    (t
                     (setq diary-string (format "%s %s" start-d
						start-t))))
              (setq event-ok t))
             ;; all-day event
             (t
              (icalendar-dmsg "all day event")
              (setq diary-string start-d)
              (setq event-ok t)))
            ;; add all other elements unless the user doesn't want to have
            ;; them
            (if event-ok
                (progn
                  (setq diary-string
			(concat diary-string " "
				(icalendar-format-ical-event e)))
                  (if do-not-ask (setq subject nil))
                  (icalendar-add-diary-entry diary-string diary-file
                                             non-marking subject))
              ;; event was not ok
              (setq found-error t)
              (setq error-string
		    (format "%s\nCannot handle this event:%s"
			    error-string e))))
        ;; handle errors
        (error
         (message "Ignoring event \"%s\"" e)
         (setq found-error t)
         (setq error-string (format "%s\nCannot handle this event: %s"
                                    error-string e)))))
    (if found-error
        (save-current-buffer
          (set-buffer (get-buffer-create " *icalendar-errors*"))
          (erase-buffer)
          (insert error-string)))
    (message "Converting icalendar...done")
    found-error))

(defun icalendar-add-diary-entry (string diary-file non-marking
                                         &optional subject)
  "Add STRING to the diary file DIARY-FILE.
STRING must be a properly formatted valid diary entry.  NON-MARKING
determines whether diary events are created as non-marking.  If
SUBJECT is not nil it must be a string that gives the subject of the
entry.  In this case the user will be asked whether he wants to insert
the entry."
  (when (or (not subject)                       ;
            (y-or-n-p (format "Add appointment for `%s' to diary? "
			      subject)))
    (when subject
      (setq non-marking
            (y-or-n-p (format "Make appointment non-marking? "))))
    (save-window-excursion
      (unless diary-file
        (setq diary-file
              (read-file-name "Add appointment to this diary file: ")))
      (make-diary-entry string non-marking diary-file))))

;; ======================================================================
;; (add-hook 'list-diary-entries-hook 'include-icalendar-files)
;; ======================================================================
(defun include-icalendar-files ()
  "Not yet implemented.")

(provide 'icalendar)

;; arch-tag: 74fdbe8e-0451-4e38-bb61-4416e822f4fc
;;; icalendar.el ends here
