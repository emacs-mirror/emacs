;;; cal-loaddefs.el --- automatically extracted autoloads for calendar.el
;;
;;; Code:


;;;### (autoloads (diary-bahai-insert-yearly-entry diary-bahai-insert-monthly-entry
;;;;;;  diary-bahai-insert-entry calendar-bahai-goto-date calendar-bahai-print-date
;;;;;;  calendar-bahai-date-string) "cal-bahai" "cal-bahai.el" "57647d548ada7136e713b79855c83f39")
;;; Generated autoloads from cal-bahai.el

(autoload 'calendar-bahai-date-string "cal-bahai" "\
String of Bahá'í date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-bahai-print-date "cal-bahai" "\
Show the Bahá'í calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-bahai-goto-date "cal-bahai" "\
Move cursor to Bahá'í date DATE.
Echo Bahá'í date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'diary-bahai-insert-entry "cal-bahai" "\
Insert a diary entry.
For the Bahá'í date corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-bahai-insert-monthly-entry "cal-bahai" "\
Insert a monthly diary entry.
For the day of the Bahá'í month corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-bahai-insert-yearly-entry "cal-bahai" "\
Insert an annual diary entry.
For the day of the Bahá'í year corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (calendar-goto-chinese-date calendar-print-chinese-date
;;;;;;  calendar-chinese-date-string) "cal-china" "cal-china.el"
;;;;;;  "537d922793e5aaaaecb43336d2a476cf")
;;; Generated autoloads from cal-china.el

(autoload 'calendar-chinese-date-string "cal-china" "\
String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-chinese-date "cal-china" "\
Show the Chinese date equivalents of date.

\(fn)" t nil)

(autoload 'calendar-goto-chinese-date "cal-china" "\
Move cursor to Chinese date DATE.
Echo Chinese date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-ethiopic-date calendar-print-ethiopic-date
;;;;;;  calendar-ethiopic-date-string calendar-goto-coptic-date calendar-print-coptic-date
;;;;;;  calendar-coptic-date-string) "cal-coptic" "cal-coptic.el"
;;;;;;  "b2d8b9f621544c44d4b298d75789409a")
;;; Generated autoloads from cal-coptic.el

(autoload 'calendar-coptic-date-string "cal-coptic" "\
String of Coptic date of Gregorian DATE.
Returns the empty string if DATE is pre-Coptic calendar.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-coptic-date "cal-coptic" "\
Show the Coptic calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-goto-coptic-date "cal-coptic" "\
Move cursor to Coptic date DATE.
Echo Coptic date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'calendar-ethiopic-date-string "cal-coptic" "\
String of Ethiopic date of Gregorian DATE.
Returns the empty string if DATE is pre-Ethiopic calendar.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-ethiopic-date "cal-coptic" "\
Show the Ethiopic calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-goto-ethiopic-date "cal-coptic" "\
Move cursor to Ethiopic date DATE.
Echo Ethiopic date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-french-date calendar-print-french-date
;;;;;;  calendar-french-date-string) "cal-french" "cal-french.el"
;;;;;;  "56b16db7f28080b25b47630a942cabf8")
;;; Generated autoloads from cal-french.el

(autoload 'calendar-french-date-string "cal-french" "\
String of French Revolutionary date of Gregorian DATE.
Returns the empty string if DATE is pre-French Revolutionary.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-french-date "cal-french" "\
Show the French Revolutionary calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-goto-french-date "cal-french" "\
Move cursor to French Revolutionary date DATE.
Echo French Revolutionary date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (cal-html-cursor-year cal-html-cursor-month) "cal-html"
;;;;;;  "cal-html.el" "3b1f91234619ffe6af854c3cafe7a855")
;;; Generated autoloads from cal-html.el

(autoload 'cal-html-cursor-month "cal-html" "\
Write an HTML calendar file for numeric MONTH of four-digit YEAR.
The output directory DIR is created if necessary.  Interactively,
MONTH and YEAR are taken from the calendar cursor position.  Note
that any existing output files are overwritten.

\(fn MONTH YEAR DIR)" t nil)

(autoload 'cal-html-cursor-year "cal-html" "\
Write HTML calendar files (index and monthly pages) for four-digit YEAR.
The output directory DIR is created if necessary.  Interactively,
YEAR is taken from the calendar cursor position.  Note that any
existing output files are overwritten.

\(fn YEAR DIR)" t nil)

;;;***

;;;### (autoloads (insert-yearly-islamic-diary-entry insert-monthly-islamic-diary-entry
;;;;;;  insert-islamic-diary-entry calendar-goto-islamic-date calendar-print-islamic-date
;;;;;;  calendar-islamic-date-string) "cal-islam" "cal-islam.el"
;;;;;;  "c7032e739f28d96220d9297182962611")
;;; Generated autoloads from cal-islam.el

(autoload 'calendar-islamic-date-string "cal-islam" "\
String of Islamic date before sunset of Gregorian DATE.
Returns the empty string if DATE is pre-Islamic.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-islamic-date "cal-islam" "\
Show the Islamic calendar equivalent of the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-goto-islamic-date "cal-islam" "\
Move cursor to Islamic DATE; echo Islamic date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'insert-islamic-diary-entry "cal-islam" "\
Insert a diary entry.
For the Islamic date corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'insert-monthly-islamic-diary-entry "cal-islam" "\
Insert a monthly diary entry.
For the day of the Islamic month corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'insert-yearly-islamic-diary-entry "cal-islam" "\
Insert an annual diary entry.
For the day of the Islamic year corresponding to the date indicated by point.
Prefix arg will make the entry nonmarking.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (calendar-goto-iso-week calendar-goto-iso-date
;;;;;;  calendar-print-iso-date calendar-iso-date-string) "cal-iso"
;;;;;;  "cal-iso.el" "17d81911282d3fcf3db98c5feea84bdc")
;;; Generated autoloads from cal-iso.el

(autoload 'calendar-iso-date-string "cal-iso" "\
String of ISO date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-iso-date "cal-iso" "\
Show equivalent ISO date for the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-goto-iso-date "cal-iso" "\
Move cursor to ISO DATE; echo ISO date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'calendar-goto-iso-week "cal-iso" "\
Move cursor to ISO DATE; echo ISO date unless NOECHO is t.
Interactively, goes to the first day of the specified week.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-astro-day-number calendar-print-astro-day-number
;;;;;;  calendar-astro-date-string calendar-astro-from-absolute calendar-absolute-from-astro
;;;;;;  calendar-goto-julian-date calendar-print-julian-date calendar-julian-date-string
;;;;;;  calendar-julian-from-absolute) "cal-julian" "cal-julian.el"
;;;;;;  "0d289ec51112315cd7a7e238fc06238c")
;;; Generated autoloads from cal-julian.el

(autoload 'calendar-julian-from-absolute "cal-julian" "\
Compute the Julian (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC.

\(fn DATE)" nil nil)

(autoload 'calendar-julian-date-string "cal-julian" "\
String of Julian date of Gregorian DATE.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-julian-date "cal-julian" "\
Show the Julian calendar equivalent of the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-goto-julian-date "cal-julian" "\
Move cursor to Julian DATE; echo Julian date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'calendar-absolute-from-astro "cal-julian" "\
Absolute date of astronomical (Julian) day number D.

\(fn D)" nil nil)

(autoload 'calendar-astro-from-absolute "cal-julian" "\
Astronomical (Julian) day number of absolute date D.

\(fn D)" nil nil)

(autoload 'calendar-astro-date-string "cal-julian" "\
String of astronomical (Julian) day number after noon UTC of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-astro-day-number "cal-julian" "\
Show astronomical (Julian) day number after noon UTC on date shown by cursor.

\(fn)" t nil)

(autoload 'calendar-goto-astro-day-number "cal-julian" "\
Move cursor to astronomical (Julian) DAYNUMBER.
Echo astronomical (Julian) day number unless NOECHO is t.

\(fn DAYNUMBER &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-mayan-long-count-date calendar-print-mayan-date
;;;;;;  calendar-mayan-date-string calendar-previous-calendar-round-date
;;;;;;  calendar-next-calendar-round-date calendar-previous-tzolkin-date
;;;;;;  calendar-next-tzolkin-date calendar-previous-haab-date calendar-next-haab-date)
;;;;;;  "cal-mayan" "cal-mayan.el" "5713952aeb0e2ba73ecd81736f4a7630")
;;; Generated autoloads from cal-mayan.el

(autoload 'calendar-next-haab-date "cal-mayan" "\
Move cursor to next instance of Mayan HAAB-DATE.
Echo Mayan date if NOECHO is t.

\(fn HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-previous-haab-date "cal-mayan" "\
Move cursor to previous instance of Mayan HAAB-DATE.
Echo Mayan date if NOECHO is t.

\(fn HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-next-tzolkin-date "cal-mayan" "\
Move cursor to next instance of Mayan TZOLKIN-DATE.
Echo Mayan date if NOECHO is t.

\(fn TZOLKIN-DATE &optional NOECHO)" t nil)

(autoload 'calendar-previous-tzolkin-date "cal-mayan" "\
Move cursor to previous instance of Mayan TZOLKIN-DATE.
Echo Mayan date if NOECHO is t.

\(fn TZOLKIN-DATE &optional NOECHO)" t nil)

(autoload 'calendar-next-calendar-round-date "cal-mayan" "\
Move cursor to next instance of Mayan HAAB-DATE TZOLKIN-DATE combination.
Echo Mayan date if NOECHO is t.

\(fn TZOLKIN-DATE HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-previous-calendar-round-date "cal-mayan" "\
Move to previous instance of Mayan TZOLKIN-DATE HAAB-DATE combination.
Echo Mayan date if NOECHO is t.

\(fn TZOLKIN-DATE HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-date-string "cal-mayan" "\
String of Mayan date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-mayan-date "cal-mayan" "\
Show the Mayan long count, tzolkin, and haab equivalents of date.

\(fn)" t nil)

(autoload 'calendar-goto-mayan-long-count-date "cal-mayan" "\
Move cursor to Mayan long count DATE.  Echo Mayan date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-day-of-year calendar-goto-date calendar-cursor-to-visible-date
;;;;;;  calendar-end-of-year calendar-beginning-of-year calendar-end-of-month
;;;;;;  calendar-beginning-of-month calendar-end-of-week calendar-beginning-of-week
;;;;;;  calendar-backward-week calendar-forward-week calendar-backward-day
;;;;;;  calendar-forward-day calendar-cursor-to-nearest-date calendar-scroll-right-three-months
;;;;;;  calendar-scroll-left-three-months calendar-scroll-right calendar-scroll-left
;;;;;;  calendar-backward-year calendar-backward-month calendar-forward-year
;;;;;;  calendar-forward-month calendar-goto-today) "cal-move" "cal-move.el"
;;;;;;  "9352ec61a952829a97c36a27fa1b89f3")
;;; Generated autoloads from cal-move.el

(autoload 'calendar-goto-today "cal-move" "\
Reposition the calendar window so the current date is visible.

\(fn)" t nil)

(autoload 'calendar-forward-month "cal-move" "\
Move the cursor forward ARG months.
Movement is backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-forward-year "cal-move" "\
Move the cursor forward by ARG years.
Movement is backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-month "cal-move" "\
Move the cursor backward by ARG months.
Movement is forward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-year "cal-move" "\
Move the cursor backward ARG years.
Movement is forward is ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-scroll-left "cal-move" "\
Scroll the displayed calendar left by ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.

\(fn &optional ARG EVENT)" t nil)

(autoload 'calendar-scroll-right "cal-move" "\
Scroll the displayed calendar window right by ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.

\(fn &optional ARG EVENT)" t nil)

(autoload 'calendar-scroll-left-three-months "cal-move" "\
Scroll the displayed calendar window left by 3*ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.

\(fn ARG)" t nil)

(autoload 'calendar-scroll-right-three-months "cal-move" "\
Scroll the displayed calendar window right by 3*ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.

\(fn ARG)" t nil)

(autoload 'calendar-cursor-to-nearest-date "cal-move" "\
Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position.

\(fn)" nil nil)

(autoload 'calendar-forward-day "cal-move" "\
Move the cursor forward ARG days.
Moves backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-day "cal-move" "\
Move the cursor back ARG days.
Moves forward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-forward-week "cal-move" "\
Move the cursor forward ARG weeks.
Moves backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-week "cal-move" "\
Move the cursor back ARG weeks.
Moves forward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-beginning-of-week "cal-move" "\
Move the cursor back ARG calendar-week-start-day's.

\(fn ARG)" t nil)

(autoload 'calendar-end-of-week "cal-move" "\
Move the cursor forward ARG calendar-week-start-day+6's.

\(fn ARG)" t nil)

(autoload 'calendar-beginning-of-month "cal-move" "\
Move the cursor backward ARG month beginnings.

\(fn ARG)" t nil)

(autoload 'calendar-end-of-month "cal-move" "\
Move the cursor forward ARG month ends.

\(fn ARG)" t nil)

(autoload 'calendar-beginning-of-year "cal-move" "\
Move the cursor backward ARG year beginnings.

\(fn ARG)" t nil)

(autoload 'calendar-end-of-year "cal-move" "\
Move the cursor forward ARG year beginnings.

\(fn ARG)" t nil)

(autoload 'calendar-cursor-to-visible-date "cal-move" "\
Move the cursor to DATE that is on the screen.

\(fn DATE)" nil nil)

(autoload 'calendar-goto-date "cal-move" "\
Move cursor to DATE.

\(fn DATE)" t nil)

(autoload 'calendar-goto-day-of-year "cal-move" "\
Move cursor to YEAR, DAY number; echo DAY/YEAR unless NOECHO is t.
Negative DAY counts backward from end of year.

\(fn YEAR DAY &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-persian-date calendar-print-persian-date
;;;;;;  calendar-persian-date-string) "cal-persia" "cal-persia.el"
;;;;;;  "b132a22d38fb382143c555d21fb3d62f")
;;; Generated autoloads from cal-persia.el

(autoload 'calendar-persian-date-string "cal-persia" "\
String of Persian date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-print-persian-date "cal-persia" "\
Show the Persian calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-goto-persian-date "cal-persia" "\
Move cursor to Persian date DATE.
Echo Persian date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (cal-tex-cursor-day cal-tex-cursor-filofax-daily
;;;;;;  cal-tex-cursor-filofax-week cal-tex-cursor-filofax-2week
;;;;;;  cal-tex-cursor-week-monday cal-tex-cursor-week-iso cal-tex-cursor-week2
;;;;;;  cal-tex-cursor-week cal-tex-cursor-month cal-tex-cursor-month-landscape
;;;;;;  cal-tex-cursor-filofax-year cal-tex-cursor-year-landscape
;;;;;;  cal-tex-cursor-year) "cal-tex" "cal-tex.el" "8c57c64c859dc569ad36e672a1dfa0af")
;;; Generated autoloads from cal-tex.el

(autoload 'cal-tex-cursor-year "cal-tex" "\
Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument ARG specifies number of years.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-year-landscape "cal-tex" "\
Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument ARG specifies number of years.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-filofax-year "cal-tex" "\
Make a Filofax one page yearly calendar of year indicated by cursor.
Optional prefix argument ARG specifies number of years.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-month-landscape "cal-tex" "\
Make a LaTeX calendar buffer for the month the cursor is on.
Optional prefix argument ARG specifies number of months to be
produced (default 1).  The output is in landscape format, one
month to a page.  It shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-month "cal-tex" "\
Make a LaTeX calendar buffer for the month the cursor is on.
Optional prefix argument ARG specifies number of months to be
produced (default 1).  The calendar is condensed onto one page.
It shows holiday and diary entries if `cal-tex-holidays' and
`cal-tex-diary', respectively, are non-nil.

\(fn ARG)" t nil)

(autoload 'cal-tex-cursor-week "cal-tex" "\
Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in.  The optional prefix
argument ARG specifies the number of weeks (default 1).  The calendar
shows holidays if `cal-tex-holidays' is non-nil (note that diary
entries are not shown).  The calendar shows the hours 8-12am, 1-5pm.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-week2 "cal-tex" "\
Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in.  Optional prefix
argument ARG specifies number of weeks (default 1).  The calendar
shows holidays if `cal-tex-holidays' is non-nil (note that diary
entries are not shown).  The calendar shows the hours 8-12am, 1-5pm

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-week-iso "cal-tex" "\
Make a LaTeX calendar buffer for a one page ISO-style weekly calendar.
Optional prefix argument ARG specifies number of weeks (default 1).
The calendar shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
It does not show hours of the day.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-week-monday "cal-tex" "\
Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in, and starts on Monday.
Optional prefix argument ARG specifies number of weeks (default 1).
The calendar shows holidays if `cal-tex-holidays' is
non-nil (note that diary entries are not shown).   The calendar shows
the hours 8-12am, 1-5pm.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-filofax-2week "cal-tex" "\
Two-weeks-at-a-glance Filofax style calendar for week cursor is in.
Optional prefix argument ARG specifies number of weeks (default 1).
The calendar shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-filofax-week "cal-tex" "\
One-week-at-a-glance Filofax style calendar for week indicated by cursor.
Optional prefix argument ARG specifies number of weeks (default 1),
starting on Mondays.  The calendar shows holiday and diary entries
if `cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-filofax-daily "cal-tex" "\
Day-per-page Filofax style calendar for week indicated by cursor.
Optional prefix argument ARG specifies number of weeks (default 1),
starting on Mondays.  The calendar shows holiday and diary
entries if `cal-tex-holidays' and `cal-tex-diary', respectively,
are non-nil.  Pages are ruled if `cal-tex-rules' is non-nil.

\(fn &optional ARG)" t nil)

(autoload 'cal-tex-cursor-day "cal-tex" "\
Make a buffer with LaTeX commands for the day cursor is on.
Optional prefix argument ARG specifies number of days.  The calendar shows
the hours between `cal-tex-daily-start' and `cal-tex-daily-end', using
the 24-hour clock if `cal-tex-24' is non-nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (calendar-two-frame-setup calendar-only-one-frame-setup
;;;;;;  calendar-one-frame-setup) "cal-x" "cal-x.el" "50a3f20db4e121a3a6ae3ab75b9730b6")
;;; Generated autoloads from cal-x.el

(autoload 'calendar-one-frame-setup "cal-x" "\
Start calendar and display it in a dedicated frame together with the diary.
This function requires a display capable of multiple frames, else
`calendar-basic-setup' is used instead.

\(fn &optional ARG)" nil nil)

(autoload 'calendar-only-one-frame-setup "cal-x" "\
Start calendar and display it in a dedicated frame.
This function requires a display capable of multiple frames, else
`calendar-basic-setup' is used instead.

\(fn &optional ARG)" nil nil)

(autoload 'calendar-two-frame-setup "cal-x" "\
Start calendar and diary in separate, dedicated frames.
This function requires a display capable of multiple frames, else
`calendar-basic-setup' is used instead.

\(fn &optional ARG)" nil nil)

;;;***

(provide 'cal-loaddefs)

;; Local Variables:
;; coding: utf-8
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;; arch-tag: 6bec5025-5432-48cd-839f-4d6b626b0104
;;; cal-loaddefs.el ends here
