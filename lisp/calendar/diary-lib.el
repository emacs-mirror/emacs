;;; diary-lib.el --- diary functions

;; Copyright (C) 1989, 1990, 1992, 1993, 1994, 1995, 2003, 2004, 2005
;;           Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <gmorris@ast.cam.ac.uk>
;; Keywords: calendar

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This collection of functions implements the diary features as described
;; in calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;;; Code:

(require 'calendar)

(defun diary-check-diary-file ()
  "Check that the file specified by `diary-file' exists and is readable.
If so, return the expanded file name, otherwise signal an error."
  (let ((d-file (substitute-in-file-name diary-file)))
    (if (and d-file (file-exists-p d-file))
        (if (file-readable-p d-file)
            d-file
          (error "Diary file `%s' is not readable" diary-file))
      (error "Diary file `%s' does not exist" diary-file))))

;;;###autoload
(defun diary (&optional arg)
  "Generate the diary window for ARG days starting with the current date.
If no argument is provided, the number of days of diary entries is governed
by the variable `number-of-diary-entries'.  A value of ARG less than 1
does nothing.  This function is suitable for execution in a `.emacs' file."
  (interactive "P")
  (diary-check-diary-file)
  (let ((date (calendar-current-date)))
    (list-diary-entries
     date
     (cond (arg (prefix-numeric-value arg))
           ((vectorp number-of-diary-entries)
            (aref number-of-diary-entries (calendar-day-of-week date)))
           (t number-of-diary-entries)))))

(defun view-diary-entries (arg)
  "Prepare and display a buffer with diary entries.
Searches the file named in `diary-file' for entries that
match ARG days starting with the date indicated by the cursor position
in the displayed three-month calendar."
  (interactive "p")
  (diary-check-diary-file)
  (list-diary-entries (calendar-cursor-to-date t) arg))

(defun view-other-diary-entries (arg d-file)
  "Prepare and display buffer of diary entries from an alternative diary file.
Searches for entries that match ARG days, starting with the date indicated
by the cursor position in the displayed three-month calendar.
D-FILE specifies the file to use as the diary file."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-file-name "Enter diary file name: " default-directory nil t)))
  (let ((diary-file d-file))
    (view-diary-entries arg)))

(autoload 'check-calendar-holidays "holidays"
  "Check the list of holidays for any that occur on DATE.
The value returned is a list of strings of relevant holiday descriptions.
The holidays are those in the list `calendar-holidays'.")

(autoload 'calendar-holiday-list "holidays"
  "Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list `calendar-holidays'.")

(autoload 'diary-french-date "cal-french"
  "French calendar equivalent of date diary entry.")

(autoload 'diary-mayan-date "cal-mayan"
  "Mayan calendar equivalent of date diary entry.")

(autoload 'diary-iso-date "cal-iso"
  "ISO calendar equivalent of date diary entry.")

(autoload 'diary-julian-date "cal-julian"
  "Julian calendar equivalent of date diary entry.")

(autoload 'diary-astro-day-number "cal-julian"
  "Astronomical (Julian) day number diary entry.")

(autoload 'diary-chinese-date "cal-china"
  "Chinese calendar equivalent of date diary entry.")

(autoload 'diary-islamic-date "cal-islam"
  "Islamic calendar equivalent of date diary entry.")

(autoload 'list-islamic-diary-entries "cal-islam"
  "Add any Islamic date entries from the diary file to `diary-entries-list'.")

(autoload 'mark-islamic-diary-entries "cal-islam"
  "Mark days in the calendar window that have Islamic date diary entries.")

(autoload 'mark-islamic-calendar-date-pattern "cal-islam"
   "Mark dates in calendar window that conform to Islamic date MONTH/DAY/YEAR.")

(autoload 'diary-bahai-date "cal-bahai"
  "Baha'i calendar equivalent of date diary entry."
  t)

(autoload 'list-bahai-diary-entries "cal-bahai"
  "Add any Baha'i date entries from the diary file to `diary-entries-list'."
  t)

(autoload 'mark-bahai-diary-entries "cal-bahai"
  "Mark days in the calendar window that have Baha'i date diary entries."
  t)

(autoload 'mark-bahai-calendar-date-pattern "cal-bahai"
   "Mark dates in calendar window that conform to Baha'i date MONTH/DAY/YEAR."
  t)

(autoload 'diary-hebrew-date "cal-hebrew"
  "Hebrew calendar equivalent of date diary entry.")

(autoload 'diary-omer "cal-hebrew"
  "Omer count diary entry.")

(autoload 'diary-yahrzeit "cal-hebrew"
  "Yahrzeit diary entry--entry applies if date is yahrzeit or the day before.")

(autoload 'diary-parasha "cal-hebrew"
  "Parasha diary entry--entry applies if date is a Saturday.")

(autoload 'diary-rosh-hodesh "cal-hebrew"
  "Rosh Hodesh diary entry.")

(autoload 'list-hebrew-diary-entries "cal-hebrew"
  "Add any Hebrew date entries from the diary file to `diary-entries-list'.")

(autoload 'mark-hebrew-diary-entries "cal-hebrew"
  "Mark days in the calendar window that have Hebrew date diary entries.")

(autoload 'mark-hebrew-calendar-date-pattern "cal-hebrew"
   "Mark dates in calendar window that conform to Hebrew date MONTH/DAY/YEAR.")

(autoload 'diary-coptic-date "cal-coptic"
  "Coptic calendar equivalent of date diary entry.")

(autoload 'diary-ethiopic-date "cal-coptic"
  "Ethiopic calendar equivalent of date diary entry.")

(autoload 'diary-persian-date "cal-persia"
  "Persian calendar equivalent of date diary entry.")

(autoload 'diary-phases-of-moon "lunar" "Moon phases diary entry.")

(autoload 'diary-sunrise-sunset "solar"
  "Local time of sunrise and sunset as a diary entry.")

(autoload 'diary-sabbath-candles "solar"
  "Local time of candle lighting diary entry--applies if date is a Friday.
No diary entry if there is no sunset on that date.")

(defvar diary-syntax-table (copy-syntax-table (standard-syntax-table))
  "The syntax table used when parsing dates in the diary file.
It is the standard syntax table used in Fundamental mode, but with the
syntax of `*' and `:' changed to be word constituents.")

(modify-syntax-entry ?* "w" diary-syntax-table)
(modify-syntax-entry ?: "w" diary-syntax-table)

(defvar diary-entries-list)
(defvar displayed-year)
(defvar displayed-month)
(defvar entry)
(defvar date)
(defvar number)
(defvar date-string)
(defvar original-date)

(defun diary-attrtype-convert (attrvalue type)
  "Convert string ATTRVALUE to TYPE appropriate for a face description.
Valid TYPEs are: string, symbol, int, stringtnil, tnil."
  (let (ret)
    (setq ret (cond ((eq type 'string) attrvalue)
		    ((eq type 'symbol) (read attrvalue))
		    ((eq type 'int) (string-to-number attrvalue))
		    ((eq type 'stringtnil)
		     (cond ((string= "t" attrvalue) t)
			   ((string= "nil" attrvalue) nil)
			   (t attrvalue)))
		    ((eq type 'tnil)
		     (cond ((string= "t" attrvalue) t)
			   ((string= "nil" attrvalue) nil)))))
;    (message "(%s)[%s]=[%s]" (print type) attrvalue ret)
    ret))


(defun diary-pull-attrs (entry fileglobattrs)
  "Pull the face-related attributes off the entry, merge with the
fileglobattrs, and return the (possibly modified) entry and face
data in a list of attrname attrvalue values.
The entry will be modified to drop all tags that are used for face matching.
If entry is nil, then the fileglobattrs are being searched for,
the fileglobattrs variable is ignored, and
diary-glob-file-regexp-prefix is prepended to the regexps before each
search."
  (save-excursion
    (let (regexp regnum attrname attr-list attrname attrvalue type
                 ret-attr attr)
      (if (null entry)
	  (progn
	    (setq ret-attr '()
		  attr-list diary-face-attrs)
	    (while attr-list
	      (goto-char (point-min))
	      (setq attr (car attr-list)
		    regexp (nth 0 attr)
		    regnum (nth 1 attr)
		    attrname (nth 2 attr)
		    type (nth 3 attr)
		    regexp (concat diary-glob-file-regexp-prefix regexp))
	      (setq attrvalue nil)
	      (if (re-search-forward regexp (point-max) t)
		  (setq attrvalue (buffer-substring-no-properties
				   (match-beginning regnum)
				   (match-end regnum))))
	      (if (and attrvalue
		       (setq attrvalue (diary-attrtype-convert attrvalue type)))
		  (setq ret-attr (append ret-attr (list attrname attrvalue))))
	      (setq attr-list (cdr attr-list)))
	    (setq fileglobattrs ret-attr))
	(progn
	  (setq ret-attr fileglobattrs
		attr-list diary-face-attrs)
	  (while attr-list
	    (goto-char (point-min))
	    (setq attr (car attr-list)
		  regexp (nth 0 attr)
		  regnum (nth 1 attr)
		  attrname (nth 2 attr)
		  type (nth 3 attr))
	    (setq attrvalue nil)
	    (if (string-match regexp entry)
		(progn
		  (setq attrvalue (substring-no-properties entry
							   (match-beginning regnum)
							   (match-end regnum)))
		  (setq entry (replace-match "" t t entry))))
	    (if (and attrvalue
		     (setq attrvalue (diary-attrtype-convert attrvalue type)))
		(setq ret-attr (append ret-attr (list attrname attrvalue))))
	    (setq attr-list (cdr attr-list)))))
      (list entry ret-attr))))


;; This can be removed once the kill/yank treatment of invisible text
;; (see etc/TODO) is fixed. -- gm
(defcustom diary-header-line-flag t
  "*If non-nil, `simple-diary-display' will show a header line.
The format of the header is specified by `diary-header-line-format'."
  :group   'diary
  :type    'boolean
  :version "22.1")

(defcustom diary-header-line-format
  '(:eval (calendar-string-spread
           (list (if selective-display
                     "Selective display active - press \"s\" in calendar \
before edit/copy"
                   "Diary"))
           ?\s (frame-width)))
  "*Format of the header line displayed by `simple-diary-display'.
Only used if `diary-header-line-flag' is non-nil."
  :group   'diary
  :type    'sexp
  :version "22.1")

(defvar diary-saved-point)		; internal

(defun list-diary-entries (date number)
  "Create and display a buffer containing the relevant lines in diary-file.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.  If NUMBER is less than 1, this function does nothing.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((month day year) string specifier) where
\(month day year) is the date of the entry, string is the entry text, and
specifier is the applicability.  If the variable `diary-list-include-blanks'
is t, this list includes a dummy diary entry consisting of the empty string)
for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`list-diary-entries-hook', `diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file.  Usually used for Hebrew or Islamic
        diary entries in files.  Applied to *each* file.

    `list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `diary-display-hook' does the actual display of information.  If this is
        nil, simple-diary-display will be used.  Use add-hook to set this to
        fancy-diary-display, if desired.  If you want no diary display, use
        add-hook to set this to ignore.

    `diary-hook' is run last.  This can be used for an appointment
        notification function."

  (when (> number 0)
    (let ((original-date date);; save for possible use in the hooks
          old-diary-syntax-table
          diary-entries-list
          file-glob-attrs
          (date-string (calendar-date-string date))
          (d-file (substitute-in-file-name diary-file)))
      (message "Preparing diary...")
      (save-excursion
        (let ((diary-buffer (find-buffer-visiting d-file)))
          (if (not diary-buffer)
              (set-buffer (find-file-noselect d-file t))
            (set-buffer diary-buffer)
            (or (verify-visited-file-modtime diary-buffer)
                (revert-buffer t t))))
        ;; d-s-p is passed to the diary display function.
        (let ((diary-saved-point (point)))
          (save-excursion
            (setq file-glob-attrs (nth 1 (diary-pull-attrs nil "")))
            (setq selective-display t)
            (setq selective-display-ellipses nil)
            (if diary-header-line-flag
                (setq header-line-format diary-header-line-format))
            (setq old-diary-syntax-table (syntax-table))
            (set-syntax-table diary-syntax-table)
            (unwind-protect
                (let ((buffer-read-only nil)
                      (diary-modified (buffer-modified-p))
                      (mark (regexp-quote diary-nonmarking-symbol)))
                  ;; First and last characters must be ^M or \n for
                  ;; selective display to work properly
                  (goto-char (1- (point-max)))
                  (if (not (looking-at "\^M\\|\n"))
                      (progn
                        (goto-char (point-max))
                        (insert "\^M")))
                  (goto-char (point-min))
                  (if (not (looking-at "\^M\\|\n"))
                      (insert "\^M"))
                  (subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
                  (calendar-for-loop
                   i from 1 to number do
                   (let ((d diary-date-forms)
                         (month (extract-calendar-month date))
                         (day (extract-calendar-day date))
                         (year (extract-calendar-year date))
                         (entry-found (list-sexp-diary-entries date)))
                     (while d
                       (let*
                           ((date-form (if (equal (car (car d)) 'backup)
                                           (cdr (car d))
                                         (car d)))
                            (backup (equal (car (car d)) 'backup))
                            (dayname
                             (format "%s\\|%s\\.?"
                                     (calendar-day-name date)
                                     (calendar-day-name date 'abbrev)))
                            (monthname
                             (format "\\*\\|%s\\|%s\\.?"
                                     (calendar-month-name month)
                                     (calendar-month-name month 'abbrev)))
                            (month (concat "\\*\\|0*" (int-to-string month)))
                            (day (concat "\\*\\|0*" (int-to-string day)))
                            (year
                             (concat
                              "\\*\\|0*" (int-to-string year)
                              (if abbreviated-calendar-year
                                  (concat "\\|" (format "%02d" (% year 100)))
                                "")))
                            (regexp
                             (concat
                              "\\(\\`\\|\^M\\|\n\\)" mark "?\\("
                              (mapconcat 'eval date-form "\\)\\(")
                              "\\)"))
                            (case-fold-search t))
                         (goto-char (point-min))
                         (while (re-search-forward regexp nil t)
                           (if backup (re-search-backward "\\<" nil t))
                           (if (and (or (char-equal (preceding-char) ?\^M)
                                        (char-equal (preceding-char) ?\n))
                                    (not (looking-at " \\|\^I")))
                               ;;  Diary entry that consists only of date.
                               (backward-char 1)
                             ;; Found a nonempty diary entry--make it
                             ;; visible and add it to the list.
                             (setq entry-found t)
                             (let ((entry-start (point))
                                   date-start temp)
                               (re-search-backward "\^M\\|\n\\|\\`")
                               (setq date-start (point))
                               (re-search-forward "\^M\\|\n" nil t 2)
                               (while (looking-at " \\|\^I")
                                 (re-search-forward "\^M\\|\n" nil t))
                               (backward-char 1)
                               (subst-char-in-region date-start
                                                     (point) ?\^M ?\n t)
                               (setq entry (buffer-substring entry-start (point))
                                     temp (diary-pull-attrs entry file-glob-attrs)
                                     entry (nth 0 temp))
                               (add-to-diary-list
                                date
                                entry
                                (buffer-substring
                                 (1+ date-start) (1- entry-start))
                                (copy-marker entry-start) (nth 1 temp))))))
                       (setq d (cdr d)))
                     (or entry-found
                         (not diary-list-include-blanks)
                         (setq diary-entries-list
                               (append diary-entries-list
                                       (list (list date "" "" "" "")))))
                     (setq date
                           (calendar-gregorian-from-absolute
                            (1+ (calendar-absolute-from-gregorian date))))
                     (setq entry-found nil)))
                  (set-buffer-modified-p diary-modified))
              (set-syntax-table old-diary-syntax-table))
            (goto-char (point-min))
            (run-hooks 'nongregorian-diary-listing-hook
                       'list-diary-entries-hook)
            (if diary-display-hook
                (run-hooks 'diary-display-hook)
              (simple-diary-display))
            (run-hooks 'diary-hook)
            diary-entries-list))))))

(defun include-other-diary-files ()
  "Include the diary entries from other diary files with those of diary-file.
This function is suitable for use in `list-diary-entries-hook';
it enables you to use shared diary files together with your own.
The files included are specified in the diaryfile by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
  (goto-char (point-min))
  (while (re-search-forward
          (concat
           "\\(\\`\\|\^M\\|\n\\)"
           (regexp-quote diary-include-string)
           " \"\\([^\"]*\\)\"")
          nil t)
    (let* ((diary-file (substitute-in-file-name
                        (buffer-substring-no-properties
                         (match-beginning 2) (match-end 2))))
           (diary-list-include-blanks nil)
           (list-diary-entries-hook 'include-other-diary-files)
           (diary-display-hook 'ignore)
           (diary-hook nil)
           (d-buffer (find-buffer-visiting diary-file))
           (diary-modified (if d-buffer
                               (save-excursion
                                 (set-buffer d-buffer)
                                 (buffer-modified-p)))))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (unwind-protect
                  (setq diary-entries-list
                        (append diary-entries-list
                                (list-diary-entries original-date number)))
                (save-excursion
                  (set-buffer (find-buffer-visiting diary-file))
		  (let ((inhibit-read-only t))
		    (subst-char-in-region (point-min) (point-max) ?\^M ?\n t))
                  (setq selective-display nil)
                  (set-buffer-modified-p diary-modified)))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
    (goto-char (point-min)))

(defun simple-diary-display ()
  "Display the diary buffer if there are any relevant entries or holidays."
  (let* ((holiday-list (if holidays-in-diary-buffer
                           (check-calendar-holidays original-date)))
         (hol-string (format "%s%s%s"
                             date-string
                             (if holiday-list ": " "")
                             (mapconcat 'identity holiday-list "; ")))
         (msg (format "No diary entries for %s" hol-string))
         ;; If selected window is dedicated (to the calendar),
         ;; need a new one to display the diary.
         (pop-up-frames (window-dedicated-p (selected-window))))
    (calendar-set-mode-line (format "Diary for %s" hol-string))
    (if (or (not diary-entries-list)
            (and (not (cdr diary-entries-list))
                 (string-equal (car (cdr (car diary-entries-list))) "")))
        (if (< (length msg) (frame-width))
            (message "%s" msg)
          (set-buffer (get-buffer-create holiday-buffer))
          (setq buffer-read-only nil)
          (calendar-set-mode-line date-string)
          (erase-buffer)
          (insert (mapconcat 'identity holiday-list "\n"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (display-buffer holiday-buffer)
          (message  "No diary entries for %s" date-string))
      (with-current-buffer
          (find-buffer-visiting (substitute-in-file-name diary-file))
        (let ((window (display-buffer (current-buffer))))
          ;; d-s-p is passed from list-diary-entries.
          (set-window-point window diary-saved-point)
          (set-window-start window (point-min))))
      (message "Preparing diary...done"))))

(defface diary-button '((((type pc) (class color))
			 (:foreground "lightblue")))
  "Default face used for buttons."
  :version "22.1"
  :group 'diary)
;; backward-compatibility alias
(put 'diary-button-face 'face-alias 'diary-button)

(define-button-type 'diary-entry
  'action #'diary-goto-entry
  'face 'diary-button)

(defun diary-goto-entry (button)
  (let ((marker (button-get button 'marker)))
    (when marker
      (pop-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))))

(defun fancy-diary-display ()
  "Prepare a diary buffer with relevant entries in a fancy, noneditable form.
This function is provided for optional use as the `diary-display-hook'."
  (save-excursion;; Turn off selective-display in the diary file's buffer.
    (set-buffer (find-buffer-visiting (substitute-in-file-name diary-file)))
    (let ((diary-modified (buffer-modified-p)))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
      (setq selective-display nil)
      (kill-local-variable 'mode-line-format)
      (set-buffer-modified-p diary-modified)))
  (if (or (not diary-entries-list)
          (and (not (cdr diary-entries-list))
               (string-equal (car (cdr (car diary-entries-list))) "")))
      (let* ((holiday-list (if holidays-in-diary-buffer
                               (check-calendar-holidays original-date)))
             (msg (format "No diary entries for %s %s"
                          (concat date-string (if holiday-list ":" ""))
                          (mapconcat 'identity holiday-list "; "))))
        (if (<= (length msg) (frame-width))
            (message "%s" msg)
          (set-buffer (get-buffer-create holiday-buffer))
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (mapconcat 'identity holiday-list "\n"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (display-buffer holiday-buffer)
          (message  "No diary entries for %s" date-string)))
    (save-excursion;; Prepare the fancy diary buffer.
      (set-buffer (make-fancy-diary-buffer))
      (setq buffer-read-only nil)
      (let ((entry-list diary-entries-list)
            (holiday-list)
            (holiday-list-last-month 1)
            (holiday-list-last-year 1)
            (date (list 0 0 0)))
        (while entry-list
          (if (not (calendar-date-equal date (car (car entry-list))))
              (progn
                (setq date (car (car entry-list)))
                (and holidays-in-diary-buffer
                     (calendar-date-compare
                      (list (list holiday-list-last-month
                                  (calendar-last-day-of-month
                                   holiday-list-last-month
                                   holiday-list-last-year)
                                  holiday-list-last-year))
                      (list date))
                     ;; We need to get the holidays for the next 3 months.
                     (setq holiday-list-last-month
                           (extract-calendar-month date))
                     (setq holiday-list-last-year
                           (extract-calendar-year date))
                     (progn
                       (increment-calendar-month
                        holiday-list-last-month holiday-list-last-year 1)
                       t)
                     (setq holiday-list
                           (let ((displayed-month holiday-list-last-month)
                                 (displayed-year holiday-list-last-year))
                             (calendar-holiday-list)))
                     (increment-calendar-month
                      holiday-list-last-month holiday-list-last-year 1))
                (let* ((date-string (calendar-date-string date))
                       (date-holiday-list
                        (let ((h holiday-list)
                              (d))
                          ;; Make a list of all holidays for date.
                          (while h
                            (if (calendar-date-equal date (car (car h)))
                                (setq d (append d (cdr (car h)))))
                            (setq h (cdr h)))
                          d)))
                  (insert (if (= (point) (point-min)) "" ?\n) date-string)
                  (if date-holiday-list (insert ":  "))
                  (let* ((l (current-column))
                         (longest 0))
                    (insert (mapconcat (lambda (x)
					 (if (< longest (length x))
					     (setq longest (length x)))
					 x)
                                       date-holiday-list
                                       (concat "\n" (make-string l ? ))))
                    (insert ?\n (make-string (+ l longest) ?=) ?\n)))))

	  (setq entry (car (cdr (car entry-list))))
	  (if (< 0 (length entry))
	      (progn
		(if (nth 3 (car entry-list))
		    (insert-button (concat entry "\n")
				   'marker (nth 3 (car entry-list))
				   :type 'diary-entry)
		  (insert entry ?\n))
		(save-excursion
                  (let* ((marks (nth 4 (car entry-list)))
                         (temp-face (make-symbol
                                     (apply
                                      'concat "temp-face-"
                                      (mapcar '(lambda (sym)
                                                 (if (stringp sym)
                                                     sym
                                                   (symbol-name sym)))
                                              marks))))
                         (faceinfo marks))
                    (make-face temp-face)
                    ;; Remove :face info from the marks,
                    ;; copy the face info into temp-face
                    (while (setq faceinfo (memq :face faceinfo))
                      (copy-face (read (nth 1 faceinfo)) temp-face)
                      (setcar faceinfo nil)
                      (setcar (cdr faceinfo) nil))
                    (setq marks (delq nil marks))
		  ;; Apply the font aspects
                    (apply 'set-face-attribute temp-face nil marks)
                    (search-backward entry)
                    (overlay-put
                     (make-overlay (match-beginning 0) (match-end 0))
                     'face temp-face)))))
	  (setq entry-list (cdr entry-list))))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (display-buffer fancy-diary-buffer)
      (fancy-diary-display-mode)
      (calendar-set-mode-line date-string)
      (message "Preparing diary...done"))))

(defun make-fancy-diary-buffer ()
  "Create and return the initial fancy diary buffer."
  (save-excursion
    (set-buffer (get-buffer-create fancy-diary-buffer))
    (setq buffer-read-only nil)
    (calendar-set-mode-line "Diary Entries")
    (erase-buffer)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (get-buffer fancy-diary-buffer)))

(defun print-diary-entries ()
  "Print a hard copy of the diary display.

If the simple diary display is being used, prepare a temp buffer with the
visible lines of the diary buffer, add a heading line composed from the mode
line, print the temp buffer, and destroy it.

If the fancy diary display is being used, just print the buffer.

The hooks given by the variable `print-diary-entries-hook' are called to do
the actual printing."
  (interactive)
  (if (bufferp (get-buffer fancy-diary-buffer))
      (save-excursion
        (set-buffer (get-buffer fancy-diary-buffer))
        (run-hooks 'print-diary-entries-hook))
    (let ((diary-buffer
           (find-buffer-visiting (substitute-in-file-name diary-file))))
      (if diary-buffer
          (let ((temp-buffer (get-buffer-create "*Printable Diary Entries*"))
                (heading))
            (save-excursion
              (set-buffer diary-buffer)
              (setq heading
                    (if (not (stringp mode-line-format))
                        "All Diary Entries"
                      (string-match "^-*\\([^-].*[^-]\\)-*$" mode-line-format)
                      (substring mode-line-format
                                 (match-beginning 1) (match-end 1))))
              (copy-to-buffer temp-buffer (point-min) (point-max))
              (set-buffer temp-buffer)
              (while (re-search-forward "\^M.*$" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (insert heading "\n"
                      (make-string (length heading) ?=) "\n")
              (run-hooks 'print-diary-entries-hook)
              (kill-buffer temp-buffer)))
        (error "You don't have a diary buffer!")))))

(defun show-all-diary-entries ()
  "Show all of the diary entries in the diary file.
This function gets rid of the selective display of the diary file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created."
  (interactive)
  (let ((d-file (diary-check-diary-file))
        (pop-up-frames (window-dedicated-p (selected-window))))
    (save-excursion
      (set-buffer (or (find-buffer-visiting d-file)
                      (find-file-noselect d-file t)))
      (let ((buffer-read-only nil)
            (diary-modified (buffer-modified-p)))
        (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
        (setq selective-display nil
              mode-line-format default-mode-line-format)
        (display-buffer (current-buffer))
        (set-buffer-modified-p diary-modified)))))

(defcustom diary-mail-addr
  (if (boundp 'user-mail-address) user-mail-address "")
  "*Email address that `diary-mail-entries' will send email to."
  :group 'diary
  :type  'string
  :version "20.3")

(defcustom diary-mail-days 7
  "*Default number of days for `diary-mail-entries' to check."
  :group 'diary
  :type 'integer
  :version "20.3")

;;;###autoload
(defun diary-mail-entries (&optional ndays)
  "Send a mail message showing diary entries for next NDAYS days.
If no prefix argument is given, NDAYS is set to `diary-mail-days'.
Mail is sent to the address specified by `diary-mail-addr'.

You can call `diary-mail-entries' every night using an at/cron job.
For example, this script will run the program at 2am daily.  Since
`emacs -batch' does not load your `.emacs' file, you must ensure that
all relevant variables are set, as done here.

#!/bin/sh
# diary-rem.sh -- repeatedly run the Emacs diary-reminder
emacs -batch \\
-eval \"(setq diary-mail-days 3 \\
             diary-file \\\"/path/to/diary.file\\\" \\
             european-calendar-style t \\
             diary-mail-addr \\\"user@host.name\\\" )\" \\
-l diary-lib -f diary-mail-entries
at -f diary-rem.sh 0200 tomorrow

You may have to tweak the syntax of the `at' command to suit your
system.  Alternatively, you can specify a cron entry:
0 1 * * * diary-rem.sh
to run it every morning at 1am."
  (interactive "P")
  (if (string-equal diary-mail-addr "")
      (error "You must set `diary-mail-addr' to use this command")
    (let ((diary-display-hook 'fancy-diary-display))
      (list-diary-entries (calendar-current-date) (or ndays diary-mail-days)))
    (compose-mail diary-mail-addr
                  (concat "Diary entries generated "
                          (calendar-date-string (calendar-current-date))))
    (insert
     (if (get-buffer fancy-diary-buffer)
         (save-excursion
           (set-buffer fancy-diary-buffer)
           (buffer-substring (point-min) (point-max)))
       "No entries found"))
    (call-interactively (get mail-user-agent 'sendfunc))))

(defun diary-name-pattern (string-array &optional abbrev-array paren)
  "Return a regexp matching the strings in the array STRING-ARRAY.
If the optional argument ABBREV-ARRAY is present, then the function
`calendar-abbrev-construct' is used to construct abbreviations from the
two supplied arrays. The returned regexp will then also match these
abbreviations, with or without final `.' characters.  If the optional
argument PAREN is non-nil, the regexp is surrounded by parentheses."
  (regexp-opt (append string-array
                      (if abbrev-array
                          (calendar-abbrev-construct abbrev-array
                                                     string-array))
                      (if abbrev-array
                          (calendar-abbrev-construct abbrev-array
                                                     string-array
                                                     'period))
                      nil)
              paren))

(defvar marking-diary-entries nil
  "True during the marking of diary entries, nil otherwise.")

(defvar marking-diary-entry nil
  "True during the marking of diary entries, if current entry is marking.")

(defun mark-diary-entries (&optional redraw)
  "Mark days in the calendar window that have diary entries.
Each entry in the diary file visible in the calendar window is
marked.  After the entries are marked, the hooks
`nongregorian-diary-marking-hook' and `mark-diary-entries-hook'
are run.  If the optional argument REDRAW is non-nil (which is
the case interactively, for example) then any existing diary
marks are first removed. This is intended to deal with deleted
diary entries."
  (interactive "p")
  ;; To remove any deleted diary entries. Do not redraw when:
  ;; i) processing #include diary files (else only get the marks from
  ;; the last #include file processed).
  ;; ii) called via calendar-redraw (since calendar has already been
  ;; erased).
  ;; Use of REDRAW handles both of these cases.
  (when (and redraw mark-diary-entries-in-calendar)
    (setq mark-diary-entries-in-calendar nil)
    (redraw-calendar))
  (let ((marking-diary-entries t)
        file-glob-attrs marks)
    (save-excursion
      (set-buffer (find-file-noselect (diary-check-diary-file) t))
      (setq mark-diary-entries-in-calendar t)
      (message "Marking diary entries...")
      (setq file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
      (let ((d diary-date-forms)
            (old-diary-syntax-table (syntax-table))
            temp)
        (set-syntax-table diary-syntax-table)
        (while d
          (let* ((date-form (if (equal (car (car d)) 'backup)
                                (cdr (car d))
                              (car d)));; ignore 'backup directive
                 (dayname
                  (diary-name-pattern calendar-day-name-array
                                      calendar-day-abbrev-array))
                 (monthname
                  (format "%s\\|\\*"
                   (diary-name-pattern calendar-month-name-array
                                       calendar-month-abbrev-array)))
                 (month "[0-9]+\\|\\*")
                 (day "[0-9]+\\|\\*")
                 (year "[0-9]+\\|\\*")
                 (l (length date-form))
                 (d-name-pos (- l (length (memq 'dayname date-form))))
                 (d-name-pos (if (/= l d-name-pos) (+ 2 d-name-pos)))
                 (m-name-pos (- l (length (memq 'monthname date-form))))
                 (m-name-pos (if (/= l m-name-pos) (+ 2 m-name-pos)))
                 (d-pos (- l (length (memq 'day date-form))))
                 (d-pos (if (/= l d-pos) (+ 2 d-pos)))
                 (m-pos (- l (length (memq 'month date-form))))
                 (m-pos (if (/= l m-pos) (+ 2 m-pos)))
                 (y-pos (- l (length (memq 'year date-form))))
                 (y-pos (if (/= l y-pos) (+ 2 y-pos)))
                 (regexp
                  (concat
                   "\\(\\`\\|\^M\\|\n\\)\\("
                   (mapconcat 'eval date-form "\\)\\(")
                   "\\)"))
                 (case-fold-search t))
            (goto-char (point-min))
            (while (re-search-forward regexp nil t)
              (let* ((dd-name
                      (if d-name-pos
                          (buffer-substring-no-properties
                           (match-beginning d-name-pos)
                           (match-end d-name-pos))))
                     (mm-name
                      (if m-name-pos
                          (buffer-substring-no-properties
                           (match-beginning m-name-pos)
                           (match-end m-name-pos))))
                     (mm (string-to-number
                          (if m-pos
                              (buffer-substring-no-properties
                               (match-beginning m-pos)
                               (match-end m-pos))
                            "")))
                     (dd (string-to-number
                          (if d-pos
                              (buffer-substring-no-properties
                               (match-beginning d-pos)
                               (match-end d-pos))
                            "")))
                     (y-str (if y-pos
                                (buffer-substring-no-properties
                                 (match-beginning y-pos)
                                 (match-end y-pos))))
                     (yy (if (not y-str)
                             0
                           (if (and (= (length y-str) 2)
                                    abbreviated-calendar-year)
                               (let* ((current-y
                                       (extract-calendar-year
                                        (calendar-current-date)))
                                      (y (+ (string-to-number y-str)
                                            (* 100
                                               (/ current-y 100)))))
                                 (if (> (- y current-y) 50)
                                     (- y 100)
                                   (if (> (- current-y y) 50)
                                       (+ y 100)
                                     y)))
                             (string-to-number y-str))))
                     (save-excursion
                       (setq entry (buffer-substring-no-properties
                                    (point) (line-end-position))
                             temp (diary-pull-attrs entry file-glob-attrs)
                             entry (nth 0 temp)
                             marks (nth 1 temp))))
                (if dd-name
                    (mark-calendar-days-named
                     (cdr (assoc-string
                           dd-name
                           (calendar-make-alist
                            calendar-day-name-array
                            0 nil calendar-day-abbrev-array) t)) marks)
                  (if mm-name
                      (setq mm
                            (if (string-equal mm-name "*") 0
                              (cdr (assoc-string
                                    mm-name
                                    (calendar-make-alist
                                     calendar-month-name-array
                                     1 nil calendar-month-abbrev-array) t)))))
                  (mark-calendar-date-pattern mm dd yy marks))))
            (setq d (cdr d))))
        (mark-sexp-diary-entries)
        (run-hooks 'nongregorian-diary-marking-hook
                   'mark-diary-entries-hook)
        (set-syntax-table old-diary-syntax-table)
        (message "Marking diary entries...done")))))

(defun mark-sexp-diary-entries ()
  "Mark days in the calendar window that have sexp diary entries.
Each entry in the diary file (or included files) visible in the calendar window
is marked.  See the documentation for the function `list-sexp-diary-entries'."
  (let* ((sexp-mark (regexp-quote sexp-diary-entry-symbol))
         (s-entry (concat "\\(\\`\\|\^M\\|\n\\)\\("
                          sexp-mark "(\\)\\|\\("
                          (regexp-quote diary-nonmarking-symbol)
                          sexp-mark "(diary-remind\\)"))
         (file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
         m y first-date last-date mark file-glob-attrs)
    (save-excursion
      (set-buffer calendar-buffer)
      (setq m displayed-month)
      (setq y displayed-year))
    (increment-calendar-month m y -1)
    (setq first-date
          (calendar-absolute-from-gregorian (list m 1 y)))
    (increment-calendar-month m y 2)
    (setq last-date
          (calendar-absolute-from-gregorian
           (list m (calendar-last-day-of-month m y) y)))
    (goto-char (point-min))
    (while (re-search-forward s-entry nil t)
      (setq marking-diary-entry (char-equal (preceding-char) ?\())
      (re-search-backward "(")
      (let ((sexp-start (point))
            sexp entry entry-start line-start marks)
        (forward-sexp)
        (setq sexp (buffer-substring-no-properties sexp-start (point)))
        (save-excursion
          (re-search-backward "\^M\\|\n\\|\\`")
          (setq line-start (point)))
        (forward-char 1)
        (if (and (or (char-equal (preceding-char) ?\^M)
                     (char-equal (preceding-char) ?\n))
                 (not (looking-at " \\|\^I")))
            (progn;; Diary entry consists only of the sexp
              (backward-char 1)
              (setq entry ""))
          (setq entry-start (point))
          ;; Find end of entry
          (re-search-forward "\^M\\|\n" nil t)
          (while (looking-at " \\|\^I")
 	    (or (re-search-forward "\^M\\|\n" nil t)
 		(re-search-forward "$" nil t)))
          (if (or (char-equal (preceding-char) ?\^M)
 		  (char-equal (preceding-char) ?\n))
 	      (backward-char 1))
          (setq entry (buffer-substring-no-properties entry-start (point)))
          (while (string-match "[\^M]" entry)
            (aset entry (match-beginning 0) ?\n )))
        (calendar-for-loop date from first-date to last-date do
          (if (setq mark (diary-sexp-entry sexp entry
                                (calendar-gregorian-from-absolute date)))
	      (progn
		(setq marks (diary-pull-attrs entry file-glob-attrs)
		      marks (nth 1 (diary-pull-attrs entry file-glob-attrs)))
		(mark-visible-calendar-date
		 (calendar-gregorian-from-absolute date)
		 (if (< 0 (length marks))
		     marks
		   (if (consp mark)
		     (car mark)))))))))))

(defun mark-included-diary-files ()
  "Mark the diary entries from other diary files with those of the diary file.
This function is suitable for use as the `mark-diary-entries-hook'; it enables
you to use shared diary files together with your own.  The files included are
specified in the diary-file by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
  (goto-char (point-min))
  (while (re-search-forward
          (concat
           "\\(\\`\\|\^M\\|\n\\)"
           (regexp-quote diary-include-string)
           " \"\\([^\"]*\\)\"")
          nil t)
    (let* ((diary-file (substitute-in-file-name
                        (match-string-no-properties 2)))
           (mark-diary-entries-hook 'mark-included-diary-files)
           (dbuff (find-buffer-visiting diary-file)))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (progn
                (mark-diary-entries)
                (unless dbuff
                  (kill-buffer (find-buffer-visiting diary-file))))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
  (goto-char (point-min)))

(defun mark-calendar-days-named (dayname &optional color)
  "Mark all dates in the calendar window that are day DAYNAME of the week.
0 means all Sundays, 1 means all Mondays, and so on."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((prev-month displayed-month)
          (prev-year displayed-year)
          (succ-month displayed-month)
          (succ-year displayed-year)
          (last-day)
          (day))
      (increment-calendar-month succ-month succ-year 1)
      (increment-calendar-month prev-month prev-year -1)
      (setq day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day 1 dayname prev-month prev-year)))
      (setq last-day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day -1 dayname succ-month succ-year)))
      (while (<= day last-day)
        (mark-visible-calendar-date (calendar-gregorian-from-absolute day) color)
        (setq day (+ day 7))))))

(defun mark-calendar-date-pattern (month day year &optional color)
  "Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((m displayed-month)
          (y displayed-year))
      (increment-calendar-month m y -1)
      (calendar-for-loop i from 0 to 2 do
          (mark-calendar-month m y month day year color)
          (increment-calendar-month m y 1)))))

(defun mark-calendar-month (month year p-month p-day p-year &optional color)
  "Mark dates in the MONTH/YEAR that conform to pattern P-MONTH/P_DAY/P-YEAR.
A value of 0 in any position of the pattern is a wildcard."
  (if (or (and (= month p-month)
               (or (= p-year 0) (= year p-year)))
          (and (= p-month 0)
               (or (= p-year 0) (= year p-year))))
      (if (= p-day 0)
          (calendar-for-loop
              i from 1 to (calendar-last-day-of-month month year) do
            (mark-visible-calendar-date (list month i year) color))
        (mark-visible-calendar-date (list month p-day year) color))))

(defun sort-diary-entries ()
  "Sort the list of diary entries by time of day."
  (setq diary-entries-list (sort diary-entries-list 'diary-entry-compare)))

(defun diary-entry-compare (e1 e2)
  "Returns t if E1 is earlier than E2."
  (or (calendar-date-compare e1 e2)
      (and (calendar-date-equal (car e1) (car e2))
           (let* ((ts1 (cadr e1)) (t1 (diary-entry-time ts1))
                  (ts2 (cadr e2)) (t2 (diary-entry-time ts2)))
             (or (< t1 t2)
                 (and (= t1 t2)
                      (string-lessp ts1 ts2)))))))

(defcustom diary-unknown-time
  -9999
  "*Value returned by diary-entry-time when no time is found.
The default value -9999 causes entries with no recognizable time to be placed
before those with times; 9999 would place entries with no recognizable time
after those with times."
  :type 'integer
  :group 'diary
  :version "20.3")

(defun diary-entry-time (s)
  "Return time at the beginning of the string S as a military-style integer.
For example, returns 1325 for 1:25pm.

Returns `diary-unknown-time' (default value -9999) if no time is recognized.
The recognized forms are XXXX, X:XX, or XX:XX (military time), and XXam,
XXAM, XXpm, XXPM, XX:XXam, XX:XXAM XX:XXpm, or XX:XXPM.  A period (.) can
be used instead of a colon (:) to separate the hour and minute parts."
  (let ((case-fold-search nil))
    (cond ((string-match        ; Military time
	    "\\`[ \t\n\\^M]*\\([0-9]?[0-9]\\)[:.]?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)"
            s)
	   (+ (* 100 (string-to-number
		      (substring s (match-beginning 1) (match-end 1))))
	      (string-to-number (substring s (match-beginning 2) (match-end 2)))))
	  ((string-match        ; Hour only  XXam or XXpm
	    "\\`[ \t\n\\^M]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
	   (+ (* 100 (% (string-to-number
			   (substring s (match-beginning 1) (match-end 1)))
			  12))
	      (if (equal ?a (downcase (aref s (match-beginning 2))))
		  0 1200)))
	  ((string-match        ; Hour and minute  XX:XXam or XX:XXpm
	    "\\`[ \t\n\\^M]*\\([0-9]?[0-9]\\)[:.]\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
	   (+ (* 100 (% (string-to-number
			   (substring s (match-beginning 1) (match-end 1)))
			  12))
	      (string-to-number (substring s (match-beginning 2) (match-end 2)))
	      (if (equal ?a (downcase (aref s (match-beginning 3))))
		  0 1200)))
	  (t diary-unknown-time)))) ; Unrecognizable

;; Unrecognizable

(defun list-sexp-diary-entries (date)
  "Add sexp entries for DATE from the diary file to `diary-entries-list'.
Also, Make them visible in the diary file.  Returns t if any entries were
found.

Sexp diary entries must be prefaced by a `sexp-diary-entry-symbol' (normally
`%%').  The form of a sexp diary entry is

                  %%(SEXP) ENTRY

Both ENTRY and DATE are globally available when the SEXP is evaluated.  If the
SEXP yields the value nil, the diary entry does not apply.  If it yields a
non-nil value, ENTRY will be taken to apply to DATE; if the non-nil value is a
string, that string will be the diary entry in the fancy diary display.

For example, the following diary entry will apply to the 21st of the month
if it is a weekday and the Friday before if the 21st is on a weekend:

      &%%(let ((dayname (calendar-day-of-week date))
               (day (extract-calendar-day date)))
           (or
             (and (= day 21) (memq dayname '(1 2 3 4 5)))
             (and (memq day '(19 20)) (= dayname 5)))
         ) UIUC pay checks deposited

A number of built-in functions are available for this type of diary entry:

      %%(diary-date MONTH DAY YEAR &optional MARK) text
                  Entry applies if date is MONTH, DAY, YEAR if
                  `european-calendar-style' is nil, and DAY, MONTH, YEAR if
                  `european-calendar-style' is t.  DAY, MONTH, and YEAR
                  can be lists of integers, the constant t, or an integer.
                  The constant t means all values.  An optional parameter
                  MARK specifies a face or single-character string to use
                  when highlighting the day in the calendar.

      %%(diary-float MONTH DAYNAME N &optional DAY MARK) text
                  Entry will appear on the Nth DAYNAME of MONTH.
                  (DAYNAME=0 means Sunday, 1 means Monday, and so on;
                  if N is negative it counts backward from the end of
                  the month.  MONTH can be a list of months, a single
                  month, or t to specify all months. Optional DAY means
                  Nth DAYNAME of MONTH on or after/before DAY.  DAY defaults
                  to 1 if N>0 and the last day of the month if N<0.  An
                  optional parameter MARK specifies a face or single-character
                  string to use when highlighting the day in the calendar.

      %%(diary-block M1 D1 Y1 M2 D2 Y2 &optional MARK) text
                  Entry will appear on dates between M1/D1/Y1 and M2/D2/Y2,
                  inclusive.  (If `european-calendar-style' is t, the
                  order of the parameters should be changed to D1, M1, Y1,
                  D2, M2, Y2.)  An optional parameter MARK specifies a face
                  or single-character string to use when highlighting the
                  day in the calendar.

      %%(diary-anniversary MONTH DAY YEAR &optional MARK) text
                  Entry will appear on anniversary dates of MONTH DAY, YEAR.
                  (If `european-calendar-style' is t, the order of the
                  parameters should be changed to DAY, MONTH, YEAR.)  Text
                  can contain %d or %d%s; %d will be replaced by the number
                  of years since the MONTH DAY, YEAR and %s will be replaced
                  by the ordinal ending of that number (that is, `st', `nd',
                  `rd' or `th', as appropriate.  The anniversary of February
                  29 is considered to be March 1 in a non-leap year.  An
                  optional parameter MARK specifies a face or single-character
                  string to use when highlighting the day in the calendar.

      %%(diary-cyclic N MONTH DAY YEAR &optional MARK) text
                  Entry will appear every N days, starting MONTH DAY, YEAR.
                  (If `european-calendar-style' is t, the order of the
                  parameters should be changed to N, DAY, MONTH, YEAR.)  Text
                  can contain %d or %d%s; %d will be replaced by the number
                  of repetitions since the MONTH DAY, YEAR and %s will
                  be replaced by the ordinal ending of that number (that is,
                  `st', `nd', `rd' or `th', as appropriate.  An optional
                  parameter MARK specifies a face or single-character string
                  to use when highlighting the day in the calendar.

      %%(diary-remind SEXP DAYS &optional MARKING) text
                  Entry is a reminder for diary sexp SEXP.  DAYS is either a
                  single number or a list of numbers indicating the number(s)
                  of days before the event that the warning(s) should occur.
                  If the current date is (one of) DAYS before the event
                  indicated by EXPR, then a suitable message (as specified
                  by `diary-remind-message') appears.  In addition to the
                  reminders beforehand, the diary entry also appears on
                  the date itself.  If optional MARKING is non-nil then the
                  *reminders* are marked on the calendar.  Marking of
                  reminders is independent of whether the entry *itself* is
                  a marking or nonmarking one.

      %%(diary-day-of-year)
                  Diary entries giving the day of the year and the number of
                  days remaining in the year will be made every day.  Note
                  that since there is no text, it makes sense only if the
                  fancy diary display is used.

      %%(diary-iso-date)
                  Diary entries giving the corresponding ISO commercial date
                  will be made every day.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.

      %%(diary-french-date)
                  Diary entries giving the corresponding French Revolutionary
                  date will be made every day.  Note that since there is no
                  text, it makes sense only if the fancy diary display is used.

      %%(diary-islamic-date)
                  Diary entries giving the corresponding Islamic date will be
                  made every day.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-hebrew-date)
                  Diary entries giving the corresponding Hebrew date will be
                  made every day.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-astro-day-number) Diary entries giving the corresponding
                  astronomical (Julian) day number will be made every day.
                  Note that since there is no text, it makes sense only if the
                  fancy diary display is used.

      %%(diary-julian-date) Diary entries giving the corresponding
                 Julian date will be made every day.  Note that since
                 there is no text, it makes sense only if the fancy diary
                 display is used.

      %%(diary-sunrise-sunset)
                  Diary entries giving the local times of sunrise and sunset
                  will be made every day.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.
                  Floating point required.

      %%(diary-phases-of-moon)
                  Diary entries giving the times of the phases of the moon
                  will be when appropriate.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.
                  Floating point required.

      %%(diary-yahrzeit MONTH DAY YEAR) text
                  Text is assumed to be the name of the person; the date is
                  the date of death on the *civil* calendar.  The diary entry
                  will appear on the proper Hebrew-date anniversary and on the
                  day before.  (If `european-calendar-style' is t, the order
                  of the parameters should be changed to DAY, MONTH, YEAR.)

      %%(diary-rosh-hodesh)
                  Diary entries will be made on the dates of Rosh Hodesh on
                  the Hebrew calendar.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-parasha)
                  Diary entries giving the weekly parasha will be made on
                  every Saturday.  Note that since there is no text, it
                  makes sense only if the fancy diary display is used.

      %%(diary-omer)
                  Diary entries giving the omer count will be made every day
                  from Passover to Shavuot.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.

Marking these entries is *extremely* time consuming, so these entries are
best if they are nonmarking."
  (let ((s-entry (concat "\\(\\`\\|\^M\\|\n\\)"
                         (regexp-quote diary-nonmarking-symbol)
                         "?"
                         (regexp-quote sexp-diary-entry-symbol)
                         "("))
        entry-found file-glob-attrs marks)
    (goto-char (point-min))
    (save-excursion
      (setq file-glob-attrs (nth 1 (diary-pull-attrs nil '()))))
    (while (re-search-forward s-entry nil t)
      (backward-char 1)
      (let ((sexp-start (point))
            sexp entry specifier entry-start line-start)
        (forward-sexp)
        (setq sexp (buffer-substring-no-properties sexp-start (point)))
        (save-excursion
          (re-search-backward "\^M\\|\n\\|\\`")
          (setq line-start (point)))
        (setq specifier
              (buffer-substring-no-properties (1+ line-start) (point))
	      entry-start (1+ line-start))
        (forward-char 1)
        (if (and (or (char-equal (preceding-char) ?\^M)
                     (char-equal (preceding-char) ?\n))
                 (not (looking-at " \\|\^I")))
            (progn;; Diary entry consists only of the sexp
              (backward-char 1)
              (setq entry ""))
          (setq entry-start (point))
          (re-search-forward "\^M\\|\n" nil t)
          (while (looking-at " \\|\^I")
            (re-search-forward "\^M\\|\n" nil t))
          (backward-char 1)
          (setq entry (buffer-substring-no-properties entry-start (point)))
          (while (string-match "[\^M]" entry)
            (aset entry (match-beginning 0) ?\n )))
        (let ((diary-entry (diary-sexp-entry sexp entry date))
              temp)
	  (setq entry (if (consp diary-entry)
			  (cdr diary-entry)
			diary-entry))
          (if diary-entry
	      (progn
		(subst-char-in-region line-start (point) ?\^M ?\n t)
		(if (< 0 (length entry))
		    (setq temp (diary-pull-attrs entry file-glob-attrs)
			  entry (nth 0 temp)
			  marks (nth 1 temp)))))
	  (add-to-diary-list date
			     entry
			     specifier
			     (if entry-start (copy-marker entry-start)
			       nil)
			     marks)
	  (setq entry-found (or entry-found diary-entry)))))
    entry-found))

(defun diary-sexp-entry (sexp entry date)
  "Process a SEXP diary ENTRY for DATE."
  (let ((result (if calendar-debug-sexp
                    (let ((stack-trace-on-error t))
                      (eval (car (read-from-string sexp))))
                  (condition-case nil
                      (eval (car (read-from-string sexp)))
                    (error
                     (beep)
                     (message "Bad sexp at line %d in %s: %s"
                              (save-excursion
                                (save-restriction
                                  (narrow-to-region 1 (point))
                                  (goto-char (point-min))
                                  (let ((lines 1))
                                    (while (re-search-forward "\n\\|\^M" nil t)
                                      (setq lines (1+ lines)))
                                    lines)))
                              diary-file sexp)
                     (sleep-for 2))))))
    (cond ((stringp result) result)
	  ((and (consp result)
		(stringp (cdr result))) result)
	  (result entry)
          (t nil))))

(defun diary-date (month day year &optional mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR if `european-calendar-style' is nil,
and DAY, MONTH, YEAR if `european-calendar-style' is t.  DAY, MONTH, and YEAR
can be lists of integers, the constant t, or an integer.  The constant t means
all values.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let ((dd (if european-calendar-style
                month
              day))
        (mm (if european-calendar-style
                day
              month))
        (m (extract-calendar-month date))
        (y (extract-calendar-year date))
        (d (extract-calendar-day date)))
    (if (and
         (or (and (listp dd) (memq d dd))
             (equal d dd)
             (eq dd t))
         (or (and (listp mm) (memq m mm))
             (equal m mm)
             (eq mm t))
         (or (and (listp year) (memq y year))
             (equal y year)
             (eq year t)))
        (cons mark entry))))

(defun diary-block (m1 d1 y1 m2 d2 y2 &optional mark)
  "Block diary entry.
Entry applies if date is between, or on one of, two dates.
The order of the parameters is
M1, D1, Y1, M2, D2, Y2 if `european-calendar-style' is nil, and
D1, M1, Y1, D2, M2, Y2 if `european-calendar-style' is t.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."

  (let ((date1 (calendar-absolute-from-gregorian
                (if european-calendar-style
                    (list d1 m1 y1)
                  (list m1 d1 y1))))
        (date2 (calendar-absolute-from-gregorian
                (if european-calendar-style
                    (list d2 m2 y2)
                  (list m2 d2 y2))))
        (d (calendar-absolute-from-gregorian date)))
    (if (and (<= date1 d) (<= d date2))
        (cons mark entry))))

(defun diary-float (month dayname n &optional day mark)
  "Floating diary entry--entry applies if date is the nth dayname of month.
Parameters are MONTH, DAYNAME, N.  MONTH can be a list of months, the constant
t, or an integer.  The constant t means all months.  If N is negative, count
backward from the end of the month.

An optional parameter DAY means the Nth DAYNAME on or after/before MONTH DAY.
Optional MARK specifies a face or single-character string to use when
highlighting the day in the calendar."
;; This is messy because the diary entry may apply, but the date on which it
;; is based can be in a different month/year.  For example, asking for the
;; first Monday after December 30.  For large values of |n| the problem is
;; more grotesque.
  (and (= dayname (calendar-day-of-week date))
       (let* ((m (extract-calendar-month date))
              (d (extract-calendar-day date))
              (y (extract-calendar-year date))
              (limit; last (n>0) or first (n<0) possible base date for entry
               (calendar-nth-named-absday (- n) dayname m y d))
              (last-abs (if (> n 0) limit (+ limit 6)))
              (first-abs (if (> n 0) (- limit 6) limit))
              (last (calendar-gregorian-from-absolute last-abs))
              (first (calendar-gregorian-from-absolute first-abs))
              ; m1, d1 is first possible base date
              (m1 (extract-calendar-month first))
              (d1 (extract-calendar-day first))
              (y1 (extract-calendar-year first))
              ; m2, d2 is last possible base date
              (m2 (extract-calendar-month last))
              (d2 (extract-calendar-day last))
              (y2 (extract-calendar-year last)))
	 (if (or (and (= m1 m2)	; only possible base dates in one month
		      (or (eq month t)
			  (if (listp month)
                              (memq m1 month)
			    (= m1 month)))
		      (let ((d (or day (if (> n 0)
					   1
					 (calendar-last-day-of-month m1 y1)))))
			(and (<= d1 d) (<= d d2))))
		 ;; only possible base dates straddle two months
		 (and (or (< y1 y2)
 			  (and (= y1 y2) (< m1 m2)))
		      (or
		       ;; m1, d1 works as a base date
		       (and
			(or (eq month t)
			    (if (listp month)
                                (memq m1 month)
			      (= m1 month)))
			(<= d1 (or day (if (> n 0)
					   1
					 (calendar-last-day-of-month m1 y1)))))
		       ;; m2, d2 works as a base date
		       (and (or (eq month t)
				(if (listp month)
                                    (memq m2 month)
				  (= m2 month)))
			    (<= (or day (if (> n 0)
					    1
					  (calendar-last-day-of-month m2 y2)))
				d2)))))
	     (cons mark entry)))))


(defun diary-anniversary (month day year &optional mark)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR if
`european-calendar-style' is nil, and DAY, MONTH, YEAR if
`european-calendar-style' is t.  Diary entry can contain `%d' or `%d%s'; the
%d will be replaced by the number of years since the MONTH DAY, YEAR and the
%s will be replaced by the ordinal ending of that number (that is, `st', `nd',
`rd' or `th', as appropriate.  The anniversary of February 29 is considered
to be March 1 in non-leap years.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((d (if european-calendar-style
                month
              day))
         (m (if european-calendar-style
                day
              month))
         (y (extract-calendar-year date))
         (diff (- y year)))
    (if (and (= m 2) (= d 29) (not (calendar-leap-year-p y)))
        (setq m 3
              d 1))
    (if (and (> diff 0) (calendar-date-equal (list m d y) date))
        (cons mark (format entry diff (diary-ordinal-suffix diff))))))

(defun diary-cyclic (n month day year &optional mark)
  "Cycle diary entry--entry applies every N days starting at MONTH, DAY, YEAR.
If `european-calendar-style' is t, parameters are N, DAY, MONTH, YEAR.
ENTRY can contain `%d' or `%d%s'; the %d will be replaced by the number of
repetitions since the MONTH DAY, YEAR and %s will be replaced by the
ordinal ending of that number (that is, `st', `nd', `rd' or `th', as
appropriate.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((d (if european-calendar-style
                month
              day))
         (m (if european-calendar-style
                day
              month))
         (diff (- (calendar-absolute-from-gregorian date)
                  (calendar-absolute-from-gregorian
                   (list m d year))))
         (cycle (/ diff n)))
    (if (and (>= diff 0) (zerop (% diff n)))
        (cons mark (format entry cycle (diary-ordinal-suffix cycle))))))

(defun diary-ordinal-suffix (n)
  "Ordinal suffix for N. (That is, `st', `nd', `rd', or `th', as appropriate.)"
  (if (or (memq (% n 100) '(11 12 13))
          (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

(defun diary-day-of-year ()
  "Day of year and number of days remaining in the year of date diary entry."
  (calendar-day-of-year-string date))

(defcustom diary-remind-message
  '("Reminder: Only "
    (if (= 0 (% days 7))
        (concat (int-to-string (/ days 7)) (if (= 7 days) " week" " weeks"))
      (concat (int-to-string days) (if (= 1 days) " day" " days")))
    " until "
    diary-entry)
  "*Pseudo-pattern giving form of reminder messages in the fancy diary
display.

Used by the function `diary-remind', a pseudo-pattern is a list of
expressions that can involve the keywords `days' (a number), `date' (a list of
month, day, year), and `diary-entry' (a string)."
  :type 'sexp
  :group 'diary)

(defun diary-remind (sexp days &optional marking)
  "Provide a reminder of a diary entry.
SEXP is a diary-sexp.  DAYS is either a single number or a list of numbers
indicating the number(s) of days before the event that the warning(s) should
occur on.  If the current date is (one of) DAYS before the event indicated by
SEXP, then a suitable message (as specified by `diary-remind-message' is
returned.

In addition to the reminders beforehand, the diary entry also appears on the
date itself.

A `diary-nonmarking-symbol' at the beginning of the line of the diary-remind
entry specifies that the diary entry (not the reminder) is non-marking.
Marking of reminders is independent of whether the entry itself is a marking
or nonmarking; if optional parameter MARKING is non-nil then the reminders are
marked on the calendar."
  (let ((diary-entry (eval sexp)))
    (cond
     ;; Diary entry applies on date
     ((and diary-entry
           (or (not marking-diary-entries) marking-diary-entry))
      diary-entry)
     ;; Diary entry may apply to `days' before date
     ((and (integerp days)
           (not diary-entry); Diary entry does not apply to date
           (or (not marking-diary-entries) marking))
      (let ((date (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian date) days))))
        (when (setq diary-entry (eval sexp)) ; re-evaluate with adjusted date
          ;; Discard any mark portion from diary-anniversary, etc.
          (if (consp diary-entry) (setq diary-entry (cdr diary-entry)))
          (mapconcat 'eval diary-remind-message ""))))
     ;; Diary entry may apply to one of a list of days before date
     ((and (listp days) days)
      (or (diary-remind sexp (car days) marking)
          (diary-remind sexp (cdr days) marking))))))

(defvar diary-modify-entry-list-string-function nil
  "Function applied to entry string before putting it into the entries list.
This is so that program that use the emacs diary for other purposes (e.g.
planner.el and org.el) can modify the string or add properties to it.")

(defun add-to-diary-list (date string specifier &optional marker globcolor)
  "Add the entry (DATE STRING SPECIFIER MARKER GLOBCOLOR) to `diary-entries-list'.
Do nothing if DATE or STRING is nil."
  (when (and date string)
    (if diary-file-name-prefix
        (let ((prefix (funcall diary-file-name-prefix-function
                               (buffer-file-name))))
          (or (string= prefix "")
              (setq string (format "[%s] %s" prefix string)))))
    (and diary-modify-entry-list-string-function
	 (setq string (funcall diary-modify-entry-list-string-function
			       string)))
    (setq diary-entries-list
          (append diary-entries-list
                  (list (list date string specifier marker globcolor))))))

(defun diary-redraw-calendar ()
  "If `calendar-buffer' is live and diary entries are marked, redraw it."
  (and mark-diary-entries-in-calendar
       (save-excursion
         (redraw-calendar)))
  ;; Return value suitable for `write-contents-functions'.
  nil)

(defun make-diary-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to
`diary-file'.  Adds `diary-redraw-calendar' to
`write-contents-functions' for FILE, so that the calendar will be
redrawn with the new entry marked, if necessary."
  (let ((pop-up-frames (window-dedicated-p (selected-window))))
    (find-file-other-window (substitute-in-file-name (or file diary-file))))
  (add-hook 'write-contents-functions 'diary-redraw-calendar nil t)
  (when selective-display
    (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
    (setq selective-display nil)
    (kill-local-variable 'mode-line-format))
  (widen)
  (goto-char (point-max))
  (when (let ((case-fold-search t))
          (search-backward "Local Variables:"
                           (max (- (point-max) 3000) (point-min))
                           t))
    (beginning-of-line)
    (insert "\n")
    (previous-line 1))
  (insert
   (if (bolp) "" "\n")
   (if nonmarking diary-nonmarking-symbol "")
   string " "))

(defun insert-diary-entry (arg)
  "Insert a diary entry for the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t t)
                    arg))

(defun insert-weekly-diary-entry (arg)
  "Insert a weekly diary entry for the day of the week indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (make-diary-entry (calendar-day-name (calendar-cursor-to-date t))
                    arg))

(defun insert-monthly-diary-entry (arg)
  "Insert a monthly diary entry for the day of the month indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " * ")
           '("* " day))))
    (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t)
                      arg)))

(defun insert-yearly-diary-entry (arg)
  "Insert an annual diary entry for the day of the year indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " " monthname)
           '(monthname " " day))))
    (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t)
                      arg)))

(defun insert-anniversary-diary-entry (arg)
  "Insert an anniversary diary entry for the date given by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " " month " " year)
           '(month " " day " " year))))
    (make-diary-entry
     (format "%s(diary-anniversary %s)"
             sexp-diary-entry-symbol
             (calendar-date-string (calendar-cursor-to-date t) nil t))
     arg)))

(defun insert-block-diary-entry (arg)
  "Insert a block diary entry for the days between the point and marked date.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " " month " " year)
           '(month " " day " " year)))
         (cursor (calendar-cursor-to-date t))
         (mark (or (car calendar-mark-ring)
                   (error "No mark set in this buffer")))
         start end)
    (if (< (calendar-absolute-from-gregorian mark)
           (calendar-absolute-from-gregorian cursor))
        (setq start mark
              end cursor)
      (setq start cursor
              end mark))
    (make-diary-entry
     (format "%s(diary-block %s %s)"
      sexp-diary-entry-symbol
      (calendar-date-string start nil t)
      (calendar-date-string end nil t))
     arg)))

(defun insert-cyclic-diary-entry (arg)
  "Insert a cyclic diary entry starting at the date given by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " " month " " year)
           '(month " " day " " year))))
    (make-diary-entry
     (format "%s(diary-cyclic %d %s)"
             sexp-diary-entry-symbol
             (calendar-read "Repeat every how many days: "
                            (lambda (x) (> x 0)))
             (calendar-date-string (calendar-cursor-to-date t) nil t))
     arg)))

;;;###autoload
(define-derived-mode diary-mode fundamental-mode
  "Diary"
  "Major mode for editing the diary file."
  (set (make-local-variable 'font-lock-defaults)
       '(diary-font-lock-keywords t)))

(define-derived-mode fancy-diary-display-mode fundamental-mode
  "Diary"
  "Major mode used while displaying diary entries using Fancy Display."
  (set (make-local-variable 'font-lock-defaults)
       '(fancy-diary-font-lock-keywords t))
  (define-key (current-local-map) "q" 'quit-window))


(defvar fancy-diary-font-lock-keywords
  (list
   (cons
    (concat
     (let ((dayname (diary-name-pattern calendar-day-name-array nil t))
           (monthname (diary-name-pattern calendar-month-name-array nil t))
	   (day "[0-9]+")
           (month "[0-9]+")
	   (year "-?[0-9]+"))
       (mapconcat 'eval calendar-date-display-form ""))
     "\\(\\(: .*\\)\\|\\(\n +.*\\)\\)*\n=+$")
    'diary-face)
   '("^.*anniversary.*$" . font-lock-keyword-face)
   '("^.*birthday.*$" . font-lock-keyword-face)
   '("^.*Yahrzeit.*$" . font-lock-reference-face)
   '("^\\(Erev \\)?Rosh Hodesh.*" . font-lock-function-name-face)
   '("^Day.*omer.*$" . font-lock-builtin-face)
   '("^Parashat.*$" . font-lock-comment-face)
   '("^[ \t]*[0-9]?[0-9]\\([:.]?[0-9][0-9]\\)?\\(am\\|pm\\|AM\\|PM\\)?\\(-[0-9]?[0-9]\\([:.]?[0-9][0-9]\\)?\\(am\\|pm\\|AM\\|PM\\)?\\)?"
     . font-lock-variable-name-face))
  "Keywords to highlight in fancy diary display")


(defun font-lock-diary-sexps (limit)
  "Recognize sexp diary entry for font-locking."
  (if (re-search-forward
       (concat "^" (regexp-quote diary-nonmarking-symbol)
               "?\\(" (regexp-quote sexp-diary-entry-symbol) "\\)")
       limit t)
      (condition-case nil
	  (save-restriction
	    (narrow-to-region (point-min) limit)
	    (let ((start (point)))
	      (forward-sexp 1)
	      (store-match-data (list start (point)))
	      t))
	(error t))))

(defun font-lock-diary-date-forms (month-array &optional symbol abbrev-array)
  "Create font-lock patterns for `diary-date-forms' using MONTH-ARRAY.
If given, optional SYMBOL must be a prefix to entries.
If optional ABBREV-ARRAY is present, the abbreviations constructed
from this array by the function `calendar-abbrev-construct' are
matched (with or without a final `.'), in addition to the full month
names."
  (let ((dayname (diary-name-pattern calendar-day-name-array
                                     calendar-day-abbrev-array t))
        (monthname (format "\\(%s\\|\\*\\)"
                           (diary-name-pattern month-array abbrev-array)))
        (month "\\([0-9]+\\|\\*\\)")
        (day "\\([0-9]+\\|\\*\\)")
        (year "-?\\([0-9]+\\|\\*\\)"))
    (mapcar '(lambda (x)
               (cons
                (concat "^" (regexp-quote diary-nonmarking-symbol) "?"
                        (if symbol (regexp-quote symbol) "") "\\("
                        (mapconcat 'eval
                                   ;; If backup, omit first item (backup)
                                   ;; and last item (not part of date)
                                   (if (equal (car x) 'backup)
                                       (reverse (cdr (reverse (cdr x))))
                                     x)
                                   "")
                        ;; With backup, last item is not part of date
                        (if (equal (car x) 'backup)
                            (concat "\\)" (eval (car (reverse x))))
                          "\\)"))
                '(1 diary-face)))
            diary-date-forms)))

(eval-when-compile (require 'cal-hebrew)
                   (require 'cal-islam))

(defvar diary-font-lock-keywords
      (append
       (font-lock-diary-date-forms calendar-month-name-array
                                   nil calendar-month-abbrev-array)
       (when (or (memq 'mark-hebrew-diary-entries
                       nongregorian-diary-marking-hook)
                 (memq 'list-hebrew-diary-entries
                       nongregorian-diary-listing-hook))
         (require 'cal-hebrew)
         (font-lock-diary-date-forms
          calendar-hebrew-month-name-array-leap-year
          hebrew-diary-entry-symbol))
       (when (or (memq 'mark-islamic-diary-entries
                       nongregorian-diary-marking-hook)
                 (memq 'list-islamic-diary-entries
                       nongregorian-diary-listing-hook))
         (require 'cal-islam)
         (font-lock-diary-date-forms
          calendar-islamic-month-name-array
          islamic-diary-entry-symbol))
       (list
        (cons
         (concat "^" (regexp-quote diary-include-string) ".*$")
         'font-lock-keyword-face)
        (cons
         (concat "^" (regexp-quote diary-nonmarking-symbol)
                 "?\\(" (regexp-quote sexp-diary-entry-symbol) "\\)")
         '(1 font-lock-reference-face))
        (cons
         (concat "^" (regexp-quote diary-nonmarking-symbol))
         'font-lock-reference-face)
        (cons
         (concat "^" (regexp-quote diary-nonmarking-symbol)
                 "?\\(" (regexp-quote hebrew-diary-entry-symbol) "\\)")
         '(1 font-lock-reference-face))
        (cons
         (concat "^" (regexp-quote diary-nonmarking-symbol)
                 "?\\(" (regexp-quote islamic-diary-entry-symbol) "\\)")
         '(1 font-lock-reference-face))
        '(font-lock-diary-sexps . font-lock-keyword-face)
        '("[0-9]?[0-9]\\([:.]?[0-9][0-9]\\)?\\(am\\|pm\\|AM\\|PM\\)\\(-[0-9]?[0-9]\\([:.]?[0-9][0-9]\\)?\\(am\\|pm\\|AM\\|PM\\)\\)?"
          . font-lock-function-name-face)))
      "Forms to highlight in diary-mode")


;; Following code from Dave Love <fx@gnu.org>.
;; Import Outlook-format appointments from mail messages in Gnus or
;; Rmail using command `diary-from-outlook'.  This, or the specialized
;; functions `diary-from-outlook-gnus' and `diary-from-outlook-rmail',
;; could be run from hooks to notice appointments automatically (in
;; which case they will prompt about adding to the diary).  The
;; message formats recognized are customizable through
;; `diary-outlook-formats'.

(defcustom diary-outlook-formats
  '(
    ;; When: 11 October 2001 12:00-14:00 (GMT) Greenwich Mean Time : Dublin, ...
    ;; [Current UK format?  The timezone is meaningless.  Sometimes the
    ;; Where is missing.]
    ("When: \\([0-9]+ [[:alpha:]]+ [0-9]+\\) \
\\([^ ]+\\) [^\n]+
\[^\n]+
\\(?:Where: \\([^\n]+\\)\n+\\)?
\\*~\\*~\\*~\\*~\\*~\\*~\\*~\\*~\\*~\\*"
     . "\\1\n \\2 %s, \\3")
    ;; When: Tuesday, April 30, 2002 03:00 PM-03:30 PM (GMT) Greenwich Mean ...
    ;; [Old UK format?]
    ("^When: [[:alpha:]]+, \\([[:alpha:]]+\\) \\([0-9][0-9]*\\), \\([0-9]\\{4\\}\\) \
\\([^ ]+\\) [^\n]+
\[^\n]+
\\(?:Where: \\([^\n]+\\)\\)?\n+"
     . "\\2 \\1 \\3\n \\4 %s, \\5")
    (
     ;; German format, apparently.
     "^Zeit: [^ ]+, +\\([0-9]+\\)\. +\\([[:upper:]][[:lower:]][[:lower:]]\\)[^ ]* +\\([0-9]+\\) +\\([^ ]+\\).*$"
     . "\\1 \\2 \\3\n \\4 %s"))
  "Alist of regexps matching message text and replacement text.

The regexp must match the start of the message text containing an
appointment, but need not include a leading `^'.  If it matches the
current message, a diary entry is made from the corresponding
template.  If the template is a string, it should be suitable for
passing to `replace-match', and so will have occurrences of `\\D' to
substitute the match for the Dth subexpression.  It must also contain
a single `%s' which will be replaced with the text of the message's
Subject field.  Any other `%' characters must be doubled, so that the
template can be passed to `format'.

If the template is actually a function, it is called with the message
body text as argument, and may use `match-string' etc. to make a
template following the rules above."
  :type '(alist :key-type (regexp :tag "Regexp matching time/place")
		:value-type (choice
			     (string :tag "Template for entry")
			     (function :tag "Unary function providing template")))
  :version "22.1"
  :group 'diary)


;; Dynamically bound.
(defvar body)
(defvar subject)

(defun diary-from-outlook-internal (&optional test-only)
  "Snarf a diary entry from a message assumed to be from MS Outlook.
Assumes `body' is bound to a string comprising the body of the message and
`subject' is bound to a string comprising its subject.
Arg TEST-ONLY non-nil means return non-nil if and only if the
message contains an appointment, don't make a diary entry."
  (catch 'finished
    (let (format-string)
      (dotimes (i (length diary-outlook-formats))
	(when (eq 0 (string-match (car (nth i diary-outlook-formats))
				  body))
	  (unless test-only
	    (setq format-string (cdr (nth i diary-outlook-formats)))
	    (save-excursion
	      (save-window-excursion
		;; Fixme: References to optional fields in the format
		;; are treated literally, not replaced by the empty
		;; string.  I think this is an Emacs bug.
		(make-diary-entry
		 (format (replace-match (if (functionp format-string)
					    (funcall format-string body)
					  format-string)
					t nil (match-string 0 body))
			 subject))
		(save-buffer))))
	  (throw 'finished t))))
    nil))

(defun diary-from-outlook (&optional noconfirm)
  "Maybe snarf diary entry from current Outlook-generated message.
Currently knows about Gnus and Rmail modes.  Unless the optional
argument NOCONFIRM is non-nil (which is the case when this
function is called interactively), then if an entry is found the
user is asked to confirm its addition."
  (interactive "p")
  (let ((func (cond
	       ((eq major-mode 'rmail-mode)
		#'diary-from-outlook-rmail)
	       ((memq major-mode '(gnus-summary-mode gnus-article-mode))
		#'diary-from-outlook-gnus)
	       (t (error "Don't know how to snarf in `%s'" major-mode)))))
    (funcall func noconfirm)))


(defvar gnus-article-mime-handles)
(defvar gnus-article-buffer)

(autoload 'gnus-fetch-field "gnus-util")
(autoload 'gnus-narrow-to-body "gnus")
(autoload 'mm-get-part "mm-decode")

(defun diary-from-outlook-gnus (&optional noconfirm)
  "Maybe snarf diary entry from Outlook-generated message in Gnus.
Unless the optional argument NOCONFIRM is non-nil (which is the case when
this function is called interactively), then if an entry is found the
user is asked to confirm its addition.
Add this function to `gnus-article-prepare-hook' to notice appointments
automatically."
  (interactive "p")
  (with-current-buffer gnus-article-buffer
    (let ((subject (gnus-fetch-field "subject"))
	  (body (if gnus-article-mime-handles
		    ;; We're multipart.  Don't get confused by part
		    ;; buttons &c.  Assume info is in first part.
		    (mm-get-part (nth 1 gnus-article-mime-handles))
		  (save-restriction
		    (gnus-narrow-to-body)
		    (buffer-string)))))
      (when (diary-from-outlook-internal t)
	(when (or noconfirm (y-or-n-p "Snarf diary entry? "))
	  (diary-from-outlook-internal)
	  (message "Diary entry added"))))))

(custom-add-option 'gnus-article-prepare-hook 'diary-from-outlook-gnus)


(defvar rmail-buffer)

(defun diary-from-outlook-rmail (&optional noconfirm)
  "Maybe snarf diary entry from Outlook-generated message in Rmail.
Unless the optional argument NOCONFIRM is non-nil (which is the case when
this function is called interactively), then if an entry is found the
user is asked to confirm its addition."
  (interactive "p")
  (with-current-buffer rmail-buffer
    (let ((subject (mail-fetch-field "subject"))
	  (body (buffer-substring (save-excursion
				    (rfc822-goto-eoh)
				    (point))
				  (point-max))))
      (when (diary-from-outlook-internal t)
	(when (or noconfirm (y-or-n-p "Snarf diary entry? "))
	  (diary-from-outlook-internal)
	  (message "Diary entry added"))))))


(provide 'diary-lib)

;;; arch-tag: 22dd506e-2e33-410d-9ae1-095a0c1b2010
;;; diary-lib.el ends here
