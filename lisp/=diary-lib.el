;;; diary-lib.el --- diary functions.

;; Copyright (C) 1989, 1990, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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

;;;###autoload
(defun diary (&optional arg)
  "Generate the diary window for ARG days starting with the current date.
If no argument is provided, the number of days of diary entries is governed
by the variable `number-of-diary-entries'.  This function is suitable for
execution in a `.emacs' file."
  (interactive "P")
  (let ((d-file (substitute-in-file-name diary-file))
        (date (calendar-current-date)))
    (if (and d-file (file-exists-p d-file))
        (if (file-readable-p d-file)
            (list-diary-entries
             date
             (cond
              (arg (prefix-numeric-value arg))
              ((vectorp number-of-diary-entries)
               (aref number-of-diary-entries (calendar-day-of-week date)))
              (t number-of-diary-entries)))
        (error "Your diary file is not readable!"))
      (error "You don't have a diary file!"))))

(defun view-diary-entries (arg)
  "Prepare and display a buffer with diary entries.
Searches the file named in `diary-file' for entries that
match ARG days starting with the date indicated by the cursor position
in the displayed three-month calendar."
  (interactive "p")
  (let ((d-file (substitute-in-file-name diary-file)))
    (if (and d-file (file-exists-p d-file))
        (if (file-readable-p d-file)
            (list-diary-entries (calendar-cursor-to-date t) arg)
          (error "Diary file is not readable!"))
      (error "You don't have a diary file!"))))

(defun view-other-diary-entries (arg diary-file)
  "Prepare and display buffer of diary entries from an alternative diary file.
Prompts for a file name and searches that file for entries that match ARG
days starting with the date indicated by the cursor position in the displayed
three-month calendar."
  (interactive
   (list (cond ((null current-prefix-arg) 1)
               ((listp current-prefix-arg) (car current-prefix-arg))
               (t current-prefix-arg))
         (setq diary-file (read-file-name "Enter diary file name: "
                                          default-directory nil t))))
  (view-diary-entries arg))

(autoload 'check-calendar-holidays "holidays"
  "Check the list of holidays for any that occur on DATE.
The value returned is a list of strings of relevant holiday descriptions.
The holidays are those in the list `calendar-holidays'."
  t)

(autoload 'calendar-holiday-list "holidays"
  "Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list `calendar-holidays'."
  t)

(autoload 'diary-french-date "cal-french"
  "French calendar equivalent of date diary entry."
  t)

(autoload 'diary-mayan-date "cal-mayan"
  "Mayan calendar equivalent of date diary entry."
  t)

(autoload 'diary-phases-of-moon "lunar" "Moon phases diary entry." t)

(autoload 'diary-sunrise-sunset "solar"
  "Local time of sunrise and sunset as a diary entry."
  t)

(autoload 'diary-sabbath-candles "solar"
  "Local time of candle lighting diary entry--applies if date is a Friday.
No diary entry if there is no sunset on that date."
  t)

(defvar diary-syntax-table (copy-syntax-table (standard-syntax-table))
  "The syntax table used when parsing dates in the diary file.
It is the standard syntax table used in Fundamental mode, but with the
syntax of `*' changed to be a word constituent.")

(modify-syntax-entry ?* "w" diary-syntax-table)

(defun list-diary-entries (date number)
  "Create and display a buffer containing the relevant lines in diary-file.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((month day year) string).  If the variable
`diary-list-include-blanks' is t, this list includes a dummy diary entry
\(consisting of the empty string) for a date with no diary entries.

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

  (if (< 0 number)
      (let* ((original-date date);; save for possible use in the hooks
             (old-diary-syntax-table)
             (diary-entries-list)
             (date-string (calendar-date-string date))
             (d-file (substitute-in-file-name diary-file)))
        (message "Preparing diary...")
        (save-excursion
          (let ((diary-buffer (get-file-buffer d-file)))
            (set-buffer (if diary-buffer
                            diary-buffer
                         (find-file-noselect d-file t))))
          (setq selective-display t)
          (setq selective-display-ellipses nil)
          (setq old-diary-syntax-table (syntax-table))
          (set-syntax-table diary-syntax-table)
          (unwind-protect
            (let ((buffer-read-only nil)
                  (diary-modified (buffer-modified-p))
                  (mark (regexp-quote diary-nonmarking-symbol)))
              (goto-char (1- (point-max)))
              (if (not (looking-at "\^M\\|\n"))
                  (progn
                    (forward-char 1)
                    (insert-string "\^M")))
              (goto-char (point-min))
              (if (not (looking-at "\^M\\|\n"))
                  (insert-string "\^M"))
              (subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
              (calendar-for-loop i from 1 to number do
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
                           (concat
                            (calendar-day-name date) "\\|"
                            (substring (calendar-day-name date) 0 3) ".?"))
                          (monthname
                           (concat
                            "\\*\\|"
                            (calendar-month-name month) "\\|"
                            (substring (calendar-month-name month) 0 3) ".?"))
                          (month (concat "\\*\\|0*" (int-to-string month)))
                          (day (concat "\\*\\|0*" (int-to-string day)))
                          (year
                           (concat
                            "\\*\\|0*" (int-to-string year)
                            (if abbreviated-calendar-year
                                (concat "\\|" (int-to-string (% year 100)))
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
                           ;; Found a nonempty diary entry--make it visible and
                           ;; add it to the list.
                           (setq entry-found t)
                           (let ((entry-start (point))
                                 (date-start))
                             (re-search-backward "\^M\\|\n\\|\\`")
                             (setq date-start (point))
                             (re-search-forward "\^M\\|\n" nil t 2)
                             (while (looking-at " \\|\^I")
                               (re-search-forward "\^M\\|\n" nil t))
                             (backward-char 1)
                             (subst-char-in-region date-start
                                (point) ?\^M ?\n t)
                             (add-to-diary-list
                               date (buffer-substring entry-start (point)))))))
                     (setq d (cdr d)))
                   (or entry-found
                       (not diary-list-include-blanks)
                       (setq diary-entries-list 
                             (append diary-entries-list
                                     (list (list date "")))))
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
        diary-entries-list))))

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
    (let ((diary-file (substitute-in-file-name
                       (buffer-substring (match-beginning 2) (match-end 2))))
          (diary-list-include-blanks nil)
          (list-diary-entries-hook 'include-other-diary-files)
          (diary-display-hook 'ignore)
          (diary-hook nil))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (unwind-protect
                  (setq diary-entries-list
                        (append diary-entries-list
                                (list-diary-entries original-date number)))
                (kill-buffer (get-file-buffer diary-file)))
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
         (msg (format "No diary entries for %s %s"
                      (concat date-string (if holiday-list ":" ""))
                      (mapconcat 'identity holiday-list "; "))))
    (if (or (not diary-entries-list)
            (and (not (cdr diary-entries-list))
                 (string-equal (car (cdr (car diary-entries-list))) "")))
        (if (<= (length msg) (frame-width))
            (message msg)
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
      (calendar-set-mode-line
       (concat "Diary for " date-string
               (if holiday-list ": " "")
               (mapconcat 'identity holiday-list "; ")))
      (display-buffer (get-file-buffer d-file))
      (message "Preparing diary...done"))))

(defun fancy-diary-display ()
  "Prepare a diary buffer with relevant entries in a fancy, noneditable form.
This function is provided for optional use as the `diary-display-hook'."
  (save-excursion;; Turn off selective-display in the diary file's buffer.
    (set-buffer (get-file-buffer (substitute-in-file-name diary-file)))
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
            (message msg)
          (set-buffer (get-buffer-create holiday-buffer))
          (setq buffer-read-only nil)
          (calendar-set-mode-line date-string)
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
                     (increment-calendar-month
                      holiday-list-last-month holiday-list-last-year 1)
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
                  (let ((l (current-column)))
                    (insert (mapconcat 'identity date-holiday-list
                                       (concat "\n" (make-string l ? )))))
                  (let ((l (current-column)))
                    (insert ?\n (make-string l ?=) ?\n)))))
          (if (< 0 (length (car (cdr (car entry-list)))))
              (insert (car (cdr (car entry-list))) ?\n))
          (setq entry-list (cdr entry-list))))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (display-buffer fancy-diary-buffer)
      (message "Preparing diary...done"))))

(defun make-fancy-diary-buffer ()
  "Create and return the initial fancy diary buffer."
  (save-excursion
    (set-buffer (get-buffer-create fancy-diary-buffer))
    (setq buffer-read-only nil)
    (make-local-variable 'mode-line-format)
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
           (get-file-buffer (substitute-in-file-name diary-file))))
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
  (let ((d-file (substitute-in-file-name diary-file)))
    (if (and d-file (file-exists-p d-file))
        (if (file-readable-p d-file)
            (save-excursion
              (let ((diary-buffer (get-file-buffer d-file)))
                (set-buffer (if diary-buffer
                                diary-buffer
                              (find-file-noselect d-file t)))
                (let ((buffer-read-only nil)
                      (diary-modified (buffer-modified-p)))
                  (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
                  (setq selective-display nil)
                  (make-local-variable 'mode-line-format)
                  (setq mode-line-format default-mode-line-format)
                  (display-buffer (current-buffer))
                  (set-buffer-modified-p diary-modified))))
          (error "Your diary file is not readable!"))
      (error "You don't have a diary file!"))))

(defun diary-name-pattern (string-array &optional fullname)
  "Convert an STRING-ARRAY, an array of strings to a pattern.
The pattern will match any of the strings, either entirely or abbreviated
to three characters.  An abbreviated form will match with or without a period;
If the optional FULLNAME is t, abbreviations will not match, just the full
name."
  (let ((pattern ""))
    (calendar-for-loop i from 0 to (1- (length string-array)) do
      (setq pattern
            (concat
             pattern
             (if (string-equal pattern "") "" "\\|")
             (aref string-array i)
             (if fullname
                 ""
               (concat
                "\\|"
                (substring (aref string-array i) 0 3) ".?")))))
    pattern))

(defun mark-diary-entries ()
  "Mark days in the calendar window that have diary entries.
Each entry in the diary file visible in the calendar window is marked.
After the entries are marked, the hooks `nongregorian-diary-marking-hook' and
`mark-diary-entries-hook' are run."
  (interactive)
  (setq mark-diary-entries-in-calendar t)
  (let ((d-file (substitute-in-file-name diary-file)))
    (if (and d-file (file-exists-p d-file))
        (if (file-readable-p d-file)
            (save-excursion
              (message "Marking diary entries...")
              (set-buffer (find-file-noselect d-file t))
              (let ((d diary-date-forms)
                    (old-diary-syntax-table))
                (setq old-diary-syntax-table (syntax-table))
                (set-syntax-table diary-syntax-table)
                (while d
                  (let*
                      ((date-form (if (equal (car (car d)) 'backup)
                                      (cdr (car d))
                                    (car d)));; ignore 'backup directive
                       (dayname (diary-name-pattern calendar-day-name-array))
                       (monthname
                        (concat
                         (diary-name-pattern calendar-month-name-array)
                         "\\|\\*"))
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
                                  (buffer-substring
                                   (match-beginning d-name-pos)
                                   (match-end d-name-pos))))
                             (mm-name
                              (if m-name-pos
                                  (buffer-substring
                                   (match-beginning m-name-pos)
                                   (match-end m-name-pos))))
                             (mm (string-to-int
                                  (if m-pos
                                      (buffer-substring
                                       (match-beginning m-pos)
                                       (match-end m-pos))
                                    "")))
                             (dd (string-to-int
                                  (if d-pos
                                      (buffer-substring
                                       (match-beginning d-pos)
                                       (match-end d-pos))
                                    "")))
                             (y-str (if y-pos
                                        (buffer-substring
                                         (match-beginning y-pos)
                                         (match-end y-pos))))
                             (yy (if (not y-str)
                                     0
                                   (if (and (= (length y-str) 2)
                                            abbreviated-calendar-year)
                                       (let* ((current-y
                                               (extract-calendar-year
                                                (calendar-current-date)))
                                              (y (+ (string-to-int y-str)
                                                    (* 100
                                                       (/ current-y 100)))))
                                         (if (> (- y current-y) 50)
                                             (- y 100)
                                           (if (> (- current-y y) 50)
                                               (+ y 100)
                                             y)))
                                     (string-to-int y-str)))))
                        (if dd-name
                            (mark-calendar-days-named
                             (cdr (assoc (capitalize (substring dd-name 0 3))
                                         (calendar-make-alist
                                          calendar-day-name-array
                                          0
                                          '(lambda (x) (substring x 0 3))))))
                          (if mm-name
                              (if (string-equal mm-name "*")
                                  (setq mm 0)
                                (setq mm
                                      (cdr (assoc
                                            (capitalize
                                             (substring mm-name 0 3))
                                            (calendar-make-alist
                                             calendar-month-name-array
                                             1
                                             '(lambda (x) (substring x 0 3)))
                                            )))))
                          (mark-calendar-date-pattern mm dd yy))))
                    (setq d (cdr d))))
                (mark-sexp-diary-entries)
                (run-hooks 'nongregorian-diary-marking-hook
                           'mark-diary-entries-hook)
                (set-syntax-table old-diary-syntax-table)
                (message "Marking diary entries...done")))
          (error "Your diary file is not readable!"))
      (error "You don't have a diary file!"))))

(defun mark-sexp-diary-entries ()
  "Mark days in the calendar window that have sexp diary entries.
Each entry in the diary file (or included files) visible in the calendar window
is marked.  See the documentation for the function `list-sexp-diary-entries'."
  (let* ((sexp-mark (regexp-quote sexp-diary-entry-symbol))
         (s-entry (concat "\\(\\`\\|\^M\\|\n\\)" sexp-mark "("))
         (m)
         (y)
         (first-date)
         (last-date))
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
      (backward-char 1)
      (let ((sexp-start (point))
            (sexp)
            (entry)
            (entry-start)
            (line-start))
        (forward-sexp)
        (setq sexp (buffer-substring sexp-start (point)))
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
          (re-search-forward "\^M\\|\n" nil t)
          (while (looking-at " \\|\^I")
            (re-search-forward "\^M\\|\n" nil t))
          (backward-char 1)
          (setq entry (buffer-substring entry-start (point)))
          (while (string-match "[\^M]" entry)
            (aset entry (match-beginning 0) ?\n )))
        (calendar-for-loop date from first-date to last-date do
          (if (diary-sexp-entry sexp entry
                                (calendar-gregorian-from-absolute date))
              (mark-visible-calendar-date
               (calendar-gregorian-from-absolute date))))))))

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
    (let ((diary-file (substitute-in-file-name
                       (buffer-substring (match-beginning 2) (match-end 2))))
          (mark-diary-entries-hook 'mark-included-diary-files))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (progn
                (mark-diary-entries)
                (kill-buffer (get-file-buffer diary-file)))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
  (goto-char (point-min)))

(defun mark-calendar-days-named (dayname)
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
        (mark-visible-calendar-date (calendar-gregorian-from-absolute day))
        (setq day (+ day 7))))))

(defun mark-calendar-date-pattern (month day year)
  "Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((m displayed-month)
          (y displayed-year))
      (increment-calendar-month m y -1)
      (calendar-for-loop i from 0 to 2 do
          (mark-calendar-month m y month day year)
          (increment-calendar-month m y 1)))))

(defun mark-calendar-month (month year p-month p-day p-year)
  "Mark dates in the MONTH/YEAR that conform to pattern P-MONTH/P_DAY/P-YEAR.
A value of 0 in any position of the pattern is a wildcard."
  (if (or (and (= month p-month)
               (or (= p-year 0) (= year p-year)))
          (and (= p-month 0)
               (or (= p-year 0) (= year p-year))))
      (if (= p-day 0)
          (calendar-for-loop
              i from 1 to (calendar-last-day-of-month month year) do
            (mark-visible-calendar-date (list month i year)))
        (mark-visible-calendar-date (list month p-day year)))))

(defun sort-diary-entries ()
  "Sort the list of diary entries by time of day."
  (setq diary-entries-list (sort diary-entries-list 'diary-entry-compare)))

(defun diary-entry-compare (e1 e2)
  "Returns t if E1 is earlier than E2."
  (or (calendar-date-compare e1 e2)
      (and (calendar-date-equal (car e1) (car e2))
           (< (diary-entry-time (car (cdr e1)))
              (diary-entry-time (car (cdr e2)))))))

(defun diary-entry-time (s)
  "Time at the beginning of the string S in a military-style integer.
For example, returns 1325 for 1:25pm.  Returns -9999 if no time is recognized.
The recognized forms are XXXX or X:XX or XX:XX (military time), XXam or XXpm,
and XX:XXam or XX:XXpm."
  (cond ((string-match;; Military time  
          "^[ \t]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)" s)
         (+ (* 100 (string-to-int
                    (substring s (match-beginning 1) (match-end 1))))
            (string-to-int (substring s (match-beginning 2) (match-end 2)))))
        ((string-match;; Hour only  XXam or XXpm
          "^[ \t]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
         (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 1) (match-end 1)))
                        12))
            (if (string-equal "a"
                              (substring s (match-beginning 2) (match-end 2)))
                0 1200)))
        ((string-match;; Hour and minute  XX:XXam or XX:XXpm
          "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
         (+ (* 100 (% (string-to-int
                         (substring s (match-beginning 1) (match-end 1)))
                        12))
            (string-to-int (substring s (match-beginning 2) (match-end 2)))
            (if (string-equal "a"
                              (substring s (match-beginning 3) (match-end 3)))
                0 1200)))
        (t -9999)));; Unrecognizable

(defun list-hebrew-diary-entries ()
  "Add any Hebrew date entries from the diary file to `diary-entries-list'.
Hebrew date diary entries must be prefaced by `hebrew-diary-entry-symbol'
\(normally an `H').  The same diary date forms govern the style of the Hebrew
calendar entries, except that the Hebrew month names must be spelled in full.
The Hebrew months are numbered from 1 to 13 with Nisan being 1, 12 being
Adar I and 13 being Adar II; you must use `Adar I' if you want Adar of a
common Hebrew year.  If a Hebrew date diary entry begins with a
`diary-nonmarking-symbol', the entry will appear in the diary listing, but will
not be marked in the calendar.  This function is provided for use with the
`nongregorian-diary-listing-hook'."
  (if (< 0 number)
      (let ((buffer-read-only nil)
            (diary-modified (buffer-modified-p))
            (gdate original-date)
            (mark (regexp-quote diary-nonmarking-symbol)))
        (calendar-for-loop i from 1 to number do
           (let* ((d diary-date-forms)
                  (hdate (calendar-hebrew-from-absolute 
                          (calendar-absolute-from-gregorian gdate)))
                  (month (extract-calendar-month hdate))
                  (day (extract-calendar-day hdate))
                  (year (extract-calendar-year hdate)))
             (while d
               (let*
                   ((date-form (if (equal (car (car d)) 'backup)
                                   (cdr (car d))
                                 (car d)))
                    (backup (equal (car (car d)) 'backup))
                    (dayname
                     (concat
                      (calendar-day-name gdate) "\\|"
                      (substring (calendar-day-name gdate) 0 3) ".?"))
                    (calendar-month-name-array
                     calendar-hebrew-month-name-array-leap-year)
                    (monthname
                     (concat
                      "\\*\\|"
                      (calendar-month-name month)))
                    (month (concat "\\*\\|0*" (int-to-string month)))
                    (day (concat "\\*\\|0*" (int-to-string day)))
                    (year
                     (concat
                      "\\*\\|0*" (int-to-string year)
                      (if abbreviated-calendar-year
                          (concat "\\|" (int-to-string (% year 100)))
                        "")))
                    (regexp
                     (concat
                      "\\(\\`\\|\^M\\|\n\\)" mark "?"
                      (regexp-quote hebrew-diary-entry-symbol)
                      "\\("
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
                     ;;  Found a nonempty diary entry--make it visible and
                     ;;  add it to the list.
                     (let ((entry-start (point))
                           (date-start))
                       (re-search-backward "\^M\\|\n\\|\\`")
                       (setq date-start (point))
                       (re-search-forward "\^M\\|\n" nil t 2)
                       (while (looking-at " \\|\^I")
                         (re-search-forward "\^M\\|\n" nil t))
                       (backward-char 1)
                       (subst-char-in-region date-start (point) ?\^M ?\n t)
                       (add-to-diary-list
                         gdate (buffer-substring entry-start (point)))))))
               (setq d (cdr d))))
           (setq gdate
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-absolute-from-gregorian gdate)))))
           (set-buffer-modified-p diary-modified))
        (goto-char (point-min))))

(defun mark-hebrew-diary-entries ()
  "Mark days in the calendar window that have Hebrew date diary entries.
Each entry in diary-file (or included files) visible in the calendar window
is marked.  Hebrew date entries are prefaced by a hebrew-diary-entry-symbol
\(normally an `H').  The same diary-date-forms govern the style of the Hebrew
calendar entries, except that the Hebrew month names must be spelled in full.
The Hebrew months are numbered from 1 to 13 with Nisan being 1, 12 being
Adar I and 13 being Adar II; you must use `Adar I' if you want Adar of a
common Hebrew year.  Hebrew date diary entries that begin with a
diary-nonmarking symbol will not be marked in the calendar.  This function
is provided for use as part of the nongregorian-diary-marking-hook."
  (let ((d diary-date-forms))
    (while d
      (let*
          ((date-form (if (equal (car (car d)) 'backup)
                          (cdr (car d))
                        (car d)));; ignore 'backup directive
           (dayname (diary-name-pattern calendar-day-name-array))
           (monthname
            (concat
             (diary-name-pattern calendar-hebrew-month-name-array-leap-year t)
             "\\|\\*"))
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
             "\\(\\`\\|\^M\\|\n\\)"
             (regexp-quote hebrew-diary-entry-symbol)
             "\\("
             (mapconcat 'eval date-form "\\)\\(")
             "\\)"))
           (case-fold-search t))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let* ((dd-name
                  (if d-name-pos
                      (buffer-substring
                       (match-beginning d-name-pos)
                       (match-end d-name-pos))))
                 (mm-name
                  (if m-name-pos
                      (buffer-substring
                       (match-beginning m-name-pos)
                       (match-end m-name-pos))))
                 (mm (string-to-int
                      (if m-pos
                          (buffer-substring
                           (match-beginning m-pos)
                           (match-end m-pos))
                        "")))
                 (dd (string-to-int
                      (if d-pos
                          (buffer-substring
                           (match-beginning d-pos)
                           (match-end d-pos))
                        "")))
                 (y-str (if y-pos
                            (buffer-substring
                             (match-beginning y-pos)
                             (match-end y-pos))))
                 (yy (if (not y-str)
                         0
                       (if (and (= (length y-str) 2)
                                abbreviated-calendar-year)
                           (let* ((current-y
                                   (extract-calendar-year
                                    (calendar-hebrew-from-absolute
                                     (calendar-absolute-from-gregorian
                                      (calendar-current-date)))))
                                  (y (+ (string-to-int y-str)
                                        (* 100 (/ current-y 100)))))
                             (if (> (- y current-y) 50)
                                 (- y 100)
                               (if (> (- current-y y) 50)
                                   (+ y 100)
                                 y)))
                         (string-to-int y-str)))))
            (if dd-name
                (mark-calendar-days-named
                 (cdr (assoc (capitalize (substring dd-name 0 3))
                             (calendar-make-alist
                               calendar-day-name-array
                               0
                              '(lambda (x) (substring x 0 3))))))
              (if mm-name
                  (if (string-equal mm-name "*")
                      (setq mm 0)
                    (setq
                      mm
                      (cdr 
                        (assoc
                          (capitalize mm-name)
                            (calendar-make-alist
                               calendar-hebrew-month-name-array-leap-year))))))
              (mark-hebrew-calendar-date-pattern mm dd yy)))))
      (setq d (cdr d)))))

(defun mark-hebrew-calendar-date-pattern (month day year)
  "Mark dates in calendar window that conform to Hebrew date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (if (and (/= 0 month) (/= 0 day))
        (if (/= 0 year)
            ;; Fully specified Hebrew date.
            (let ((date (calendar-gregorian-from-absolute
                         (calendar-absolute-from-hebrew
                          (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (mark-visible-calendar-date date)))
          ;; Month and day in any year--this taken from the holiday stuff.
          (if (memq displayed-month;;  This test is only to speed things up a
                    (list          ;;  bit; it works fine without the test too.
                     (if (< 11 month) (- month 11) (+ month 1))
                     (if (< 10 month) (- month 10) (+ month 2))
                     (if (<  9 month) (- month  9) (+ month 3))
                     (if (<  8 month) (- month  8) (+ month 4))
                     (if (<  7 month) (- month  7) (+ month 5))))
              (let ((m1 displayed-month)
                    (y1 displayed-year)
                    (m2 displayed-month)
                    (y2 displayed-year)
                    (year))
                (increment-calendar-month m1 y1 -1)
                (increment-calendar-month m2 y2 1)
                (let* ((start-date (calendar-absolute-from-gregorian
                                    (list m1 1 y1)))
                       (end-date (calendar-absolute-from-gregorian
                                  (list m2
                                        (calendar-last-day-of-month m2 y2)
                                        y2)))
                       (hebrew-start
                        (calendar-hebrew-from-absolute start-date))
                       (hebrew-end (calendar-hebrew-from-absolute end-date))
                       (hebrew-y1 (extract-calendar-year hebrew-start))
                       (hebrew-y2 (extract-calendar-year hebrew-end)))
                  (setq year (if (< 6 month) hebrew-y2 hebrew-y1))
                  (let ((date (calendar-gregorian-from-absolute
                               (calendar-absolute-from-hebrew
                                (list month day year)))))
                    (if (calendar-date-is-visible-p date)
                        (mark-visible-calendar-date date)))))))
      ;; Not one of the simple cases--check all visible dates for match.
      ;; Actually, the following code takes care of ALL of the cases, but
      ;; it's much too slow to be used for the simple (common) cases.
      (let ((m displayed-month)
            (y displayed-year)
            (first-date)
            (last-date))
        (increment-calendar-month m y -1)
        (setq first-date
              (calendar-absolute-from-gregorian
               (list m 1 y)))
        (increment-calendar-month m y 2)
        (setq last-date
              (calendar-absolute-from-gregorian
               (list m (calendar-last-day-of-month m y) y)))
        (calendar-for-loop date from first-date to last-date do
          (let* ((h-date (calendar-hebrew-from-absolute date))
                 (h-month (extract-calendar-month h-date))
                 (h-day (extract-calendar-day h-date))
                 (h-year (extract-calendar-year h-date)))
            (and (or (zerop month)
                     (= month h-month))
                 (or (zerop day)
                     (= day h-day))
                 (or (zerop year)
                     (= year h-year))
                 (mark-visible-calendar-date
                  (calendar-gregorian-from-absolute date)))))))))

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

      %%(diary-float MONTH DAYNAME N) text
                  Entry will appear on the Nth DAYNAME of MONTH.
                  (DAYNAME=0 means Sunday, 1 means Monday, and so on;
                  if N is negative it counts backward from the end of
                  the month.  MONTH can be a list of months, a single
                  month, or t to specify all months.

      %%(diary-block M1 D1 Y1 M2 D2 Y2) text
                  Entry will appear on dates between M1/D1/Y1 and M2/D2/Y2,
                  inclusive.  (If `european-calendar-style' is t, the
                  order of the parameters should be changed to D1, M1, Y1,
                  D2, M2, Y2.)

      %%(diary-anniversary MONTH DAY YEAR) text
                  Entry will appear on anniversary dates of MONTH DAY, YEAR.
                  (If `european-calendar-style' is t, the order of the
                  parameters should be changed to DAY, MONTH, YEAR.)  Text
                  can contain %d or %d%s; %d will be replaced by the number
                  of years since the MONTH DAY, YEAR and %s will be replaced
                  by the ordinal ending of that number (that is, `st', `nd',
                  `rd' or `th', as appropriate.  The anniversary of February
                  29 is considered to be March 1 in a non-leap year.

      %%(diary-cyclic N MONTH DAY YEAR) text
                  Entry will appear every N days, starting MONTH DAY, YEAR.
                  (If `european-calendar-style' is t, the order of the
                  parameters should be changed to N, DAY, MONTH, YEAR.)  Text
                  can contain %d or %d%s; %d will be replaced by the number
                  of repetitions since the MONTH DAY, YEAR and %s will
                  be replaced by the ordinal ending of that number (that is,
                  `st', `nd', `rd' or `th', as appropriate.

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
                  from Passover to Shavuoth.  Note that since there is no text,
                  it makes sense only if the fancy diary display is used.

Marking these entries is *extremely* time consuming, so these entries are
best if they are nonmarking."
  (let* ((mark (regexp-quote diary-nonmarking-symbol))
         (sexp-mark (regexp-quote sexp-diary-entry-symbol))
         (s-entry (concat "\\(\\`\\|\^M\\|\n\\)" mark "?" sexp-mark "("))
         (entry-found))
    (goto-char (point-min))
    (while (re-search-forward s-entry nil t)
      (backward-char 1)
      (let ((sexp-start (point))
            (sexp)
            (entry)
            (entry-start)
            (line-start))
        (forward-sexp)
        (setq sexp (buffer-substring sexp-start (point)))
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
          (re-search-forward "\^M\\|\n" nil t)
          (while (looking-at " \\|\^I")
            (re-search-forward "\^M\\|\n" nil t))
          (backward-char 1)
          (setq entry (buffer-substring entry-start (point)))
          (while (string-match "[\^M]" entry)
            (aset entry (match-beginning 0) ?\n )))
        (let ((diary-entry (diary-sexp-entry sexp entry date)))
          (if diary-entry
              (subst-char-in-region line-start (point) ?\^M ?\n t))
          (add-to-diary-list date diary-entry)
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
    (if (stringp result)
        result
      (if result
          entry
        nil))))

(defun diary-block (m1 d1 y1 m2 d2 y2)
  "Block diary entry.
Entry applies if date is between two dates.  Order of the parameters is
M1, D1, Y1, M2, D2, Y2 `european-calendar-style' is nil, and
D1, M1, Y1, D2, M2, Y2 if `european-calendar-style' is t."
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
        entry)))

(defun diary-float (month dayname n)
  "Floating diary entry--entry applies if date is the nth dayname of month.
Parameters are MONTH, DAYNAME, N.  MONTH can be a list of months, the constant
t, or an integer.  The constant t means all months.  If N is negative, count
backward from the end of the month."
  (let ((m (extract-calendar-month date))
        (y (extract-calendar-year date)))
    (if (and
         (or (and (listp month) (memq m month))
             (equal m month)
             (eq month t))
         (calendar-date-equal date (calendar-nth-named-day n dayname m y)))
        entry)))

(defun diary-anniversary (month day year)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR if
`european-calendar-style' is nil, and DAY, MONTH, YEAR if
`european-calendar-style' is t.  Diary entry can contain `%d' or `%d%s'; the
%d will be replaced by the number of years since the MONTH DAY, YEAR and the
%s will be replaced by the ordinal ending of that number (that is, `st', `nd',
`rd' or `th', as appropriate.  The anniversary of February 29 is considered
to be March 1 in non-leap years."
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
        (format entry diff (diary-ordinal-suffix diff)))))

(defun diary-cyclic (n month day year)
  "Cycle diary entry--entry applies every N days starting at MONTH, DAY, YEAR.
If `european-calendar-style' is t, parameters are N, DAY, MONTH, YEAR.
ENTRY can contain `%d' or `%d%s'; the %d will be replaced by the number of
years since the MONTH DAY, YEAR and the %s will be replaced by the ordinal
ending of that number (that is, `st', `nd', `rd' or `th', as appropriate."
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
        (format entry cycle (diary-ordinal-suffix cycle)))))

(defun diary-ordinal-suffix (n)
  "Ordinal suffix for N. (That is, `st', `nd', `rd', or `th', as appropriate.)"
  (if (or (memq (% n 100) '(11 12 13))
          (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

(defun diary-day-of-year ()
  "Day of year and number of days remaining in the year of date diary entry."
  (calendar-day-of-year-string date))

(defun diary-iso-date ()
  "ISO calendar equivalent of date diary entry."
  (format "ISO date: %s" (calendar-iso-date-string date)))

(defun diary-islamic-date ()
  "Islamic calendar equivalent of date diary entry."
  (let ((i (calendar-islamic-date-string (calendar-cursor-to-date t))))
    (if (string-equal i "")
        "Date is pre-Islamic"
      (format "Islamic date (until sunset): %s" i))))

(defun diary-hebrew-date ()
  "Hebrew calendar equivalent of date diary entry."
  (format "Hebrew date (until sunset): %s" (calendar-hebrew-date-string date)))

(defun diary-julian-date ()
  "Julian calendar equivalent of date diary entry."
  (format "Julian date: %s" (calendar-julian-date-string date)))

(defun diary-astro-day-number ()
  "Astronomical (Julian) day number diary entry."
  (format "Astronomical (Julian) day number %s"
          (calendar-astro-date-string date)))

(defun diary-omer ()
  "Omer count diary entry.
Entry applies if date is within 50 days after Passover."
  (let* ((passover
          (calendar-absolute-from-hebrew
           (list 1 15 (+ (extract-calendar-year date) 3760))))
         (omer (- (calendar-absolute-from-gregorian date) passover))
         (week (/ omer 7))
         (day (% omer 7)))
    (if (and (> omer 0) (< omer 50))
        (format "Day %d%s of the omer (until sunset)"
                omer
                (if (zerop week)
                    ""
                  (format ", that is, %d week%s%s"
                          week
                          (if (= week 1) "" "s")
                          (if (zerop day)
                              ""
                            (format " and %d day%s"
                                    day (if (= day 1) "" "s")))))))))

(defun diary-yahrzeit (death-month death-day death-year)
  "Yahrzeit diary entry--entry applies if date is yahrzeit or the day before.
Parameters are DEATH-MONTH, DEATH-DAY, DEATH-YEAR; the diary entry is assumed
to be the name of the person.  Date of death is on the *civil* calendar;
although the date of death is specified by the civil calendar, the proper
Hebrew calendar yahrzeit is determined.  If `european-calendar-style' is t, the
order of the parameters is changed to DEATH-DAY, DEATH-MONTH, DEATH-YEAR."
  (let* ((h-date (calendar-hebrew-from-absolute
                  (calendar-absolute-from-gregorian
                   (if european-calendar-style
                       (list death-day death-month death-year)
                   (list death-month death-day death-year)))))
         (h-month (extract-calendar-month h-date))
         (h-day (extract-calendar-day h-date))
         (h-year (extract-calendar-year h-date))
         (d (calendar-absolute-from-gregorian date))
         (yr (extract-calendar-year (calendar-hebrew-from-absolute d)))
         (diff (- yr h-year))
         (y (hebrew-calendar-yahrzeit h-date yr)))
    (if (and (> diff 0) (or (= y d) (= y (1+ d))))
        (format "Yahrzeit of %s%s: %d%s anniversary"
                entry
                (if (= y d) "" " (evening)")
                diff
                (cond ((= (% diff 10) 1) "st")
                      ((= (% diff 10) 2) "nd")
                      ((= (% diff 10) 3) "rd")
                      (t "th"))))))

(defun diary-rosh-hodesh ()
  "Rosh Hodesh diary entry.
Entry applies if date is Rosh Hodesh, the day before, or the Saturday before."
  (let* ((d (calendar-absolute-from-gregorian date))
         (h-date (calendar-hebrew-from-absolute d))
         (h-month (extract-calendar-month h-date))
         (h-day (extract-calendar-day h-date))
         (h-year (extract-calendar-year h-date))
         (leap-year (hebrew-calendar-leap-year-p h-year))
         (last-day (hebrew-calendar-last-day-of-month h-month h-year))
         (h-month-names
          (if leap-year
              calendar-hebrew-month-name-array-leap-year
            calendar-hebrew-month-name-array-common-year))
         (this-month (aref h-month-names (1- h-month)))
         (h-yesterday (extract-calendar-day
                       (calendar-hebrew-from-absolute (1- d)))))
    (if (or (= h-day 30) (and (= h-day 1) (/= h-month 7)))
        (format
         "Rosh Hodesh %s"
         (if (= h-day 30)
             (format
              "%s (first day)"
              ;; next month must be in the same year since this
              ;; month can't be the last month of the year since
              ;; it has 30 days
              (aref h-month-names h-month))
           (if (= h-yesterday 30)
               (format "%s (second day)" this-month)
             this-month)))
      (if (= (% d 7) 6);; Saturday--check for Shabbat Mevarhim
          (cond ((and (> h-day 22) (/= h-month 6) (= 29 last-day))
                 (format "Mevarhim Rosh Hodesh %s (%s)"
                         (aref h-month-names
                               (if (= h-month
                                      (hebrew-calendar-last-month-of-year
                                       h-year))
                                   0 h-month))
                         (aref calendar-day-name-array (- 29 h-day))))
                ((and (< h-day 30) (> h-day 22) (= 30 last-day))
                 (format "Mevarhim Rosh Hodesh %s (%s-%s)"
                         (aref h-month-names h-month)
                         (if (= h-day 29)
                             "tomorrow"
                           (aref calendar-day-name-array (- 29 h-day)))
                         (aref calendar-day-name-array
                               (% (- 30 h-day) 7)))))
        (if (and (= h-day 29) (/= h-month 6))
            (format "Erev Rosh Hodesh %s"
                    (aref h-month-names
                          (if (= h-month
                                 (hebrew-calendar-last-month-of-year
                                  h-year))
                              0 h-month))))))))

(defun diary-parasha ()
  "Parasha diary entry--entry applies if date is a Saturday."
  (let ((d (calendar-absolute-from-gregorian date)))
    (if (= (% d 7) 6);;  Saturday
        (let*
            ((h-year (extract-calendar-year
                      (calendar-hebrew-from-absolute d)))
             (rosh-hashannah
              (calendar-absolute-from-hebrew (list 7 1 h-year)))
             (passover
              (calendar-absolute-from-hebrew (list 1 15 h-year)))
             (rosh-hashannah-day
              (aref calendar-day-name-array (% rosh-hashannah 7)))
             (passover-day
              (aref calendar-day-name-array (% passover 7)))
             (long-h (hebrew-calendar-long-heshvan-p h-year))
             (short-k (hebrew-calendar-short-kislev-p h-year))
             (type (cond ((and long-h (not short-k)) "complete")
                         ((and (not long-h) short-k) "incomplete")
                         (t "regular")))
             (year-format
              (symbol-value
               (intern (format "hebrew-calendar-year-%s-%s-%s";; keviah
                               rosh-hashannah-day type passover-day))))
             (first-saturday;; of Hebrew year
              (calendar-dayname-on-or-before 6 (+ 6 rosh-hashannah)))
             (saturday;; which Saturday of the Hebrew year
              (/ (- d first-saturday) 7))
             (parasha (aref year-format saturday)))
          (if parasha
              (format
               "Parashat %s"
               (if (listp parasha);; Israel differs from diaspora
                   (if (car parasha)
                       (format "%s (diaspora), %s (Israel)"
                               (hebrew-calendar-parasha-name (car parasha))
                               (hebrew-calendar-parasha-name (cdr parasha)))
                     (format "%s (Israel)"
                             (hebrew-calendar-parasha-name (cdr parasha))))
                 (hebrew-calendar-parasha-name parasha))))))))

(defun add-to-diary-list (date string)
  "Add the entry (DATE STRING) to `diary-entries-list'.
Do nothing if DATE or STRING is nil."
  (and date string
       (setq diary-entries-list 
             (append diary-entries-list (list (list date string))))))

(defvar hebrew-calendar-parashiot-names
["Bereshith"   "Noah"      "Lech L'cha" "Vayera"    "Hayei Sarah" "Toledoth"
 "Vayetze"     "Vayishlah" "Vayeshev"   "Mikketz"   "Vayiggash"   "Vayhi"
 "Shemoth"     "Vaera"     "Bo"         "Beshallah" "Yithro"      "Mishpatim"
 "Terumah"     "Tetzavveh" "Ki Tissa"   "Vayakhel"  "Pekudei"     "Vayikra"
 "Tzav"        "Shemini"   "Tazria"     "Metzora"   "Aharei Moth" "Kedoshim"
 "Emor"        "Behar"     "Behukkotai" "Bemidbar"  "Naso"       "Behaalot'cha"
 "Shelah L'cha" "Korah"    "Hukkath"    "Balak"     "Pinhas"      "Mattoth"
 "Masei"       "Devarim"   "Vaethanan"  "Ekev"      "Reeh"        "Shofetim"
 "Ki Tetze"    "Ki Tavo"   "Nitzavim"   "Vayelech"  "Haazinu"]
  "The names of the parashiot in the Torah.")

;; The seven ordinary year types (keviot)

(defconst hebrew-calendar-year-Saturday-incomplete-Sunday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
    23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
    43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year starts on Saturday, is `incomplete' (Heshvan and Kislev each have
29 days), and has Passover start on Sunday.")

(defconst hebrew-calendar-year-Saturday-complete-Tuesday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
    23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
    43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Saturday, is `complete' (Heshvan and Kislev each
have 30 days), and has Passover start on Tuesday.")

(defconst hebrew-calendar-year-Monday-incomplete-Tuesday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
    23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
    43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `incomplete' (Heshvan and Kislev each
have 29 days), and has Passover start on Tuesday.")

(defconst hebrew-calendar-year-Monday-complete-Thursday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
   23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 (nil . 34) (34 . 35) (35 . 36)
   (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `complete' (Heshvan and Kislev each have
30 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Tuesday-regular-Thursday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22]
   23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 (nil . 34) (34 . 35) (35 . 36)
   (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Tuesday, is `regular' (Heshvan has 29 days and
Kislev has 30 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Thursday-regular-Saturday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [21 22] 23
   24 nil (nil . 25) (25 . [26 27]) ([26 27] . [28 29]) ([28 29] . 30)
   (30 . 31) ([31 32] . 32) 33 34 35 36 37 38 39 40 [41 42] 43 44 45 46 47 48
   49 50]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `regular' (Heshvan has 29 days and
Kislev has 30 days), and has Passover start on Saturday.")

(defconst hebrew-calendar-year-Thursday-complete-Sunday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
    23 24 nil 25 [26 27] [28 29] 30 [31 32] 33 34 35 36 37 38 39 40 [41 42]
    43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `complete' (Heshvan and Kislev each
have 30 days), and has Passover start on Sunday.")

;; The seven leap year types (keviot)

(defconst hebrew-calendar-year-Saturday-incomplete-Tuesday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
    23 24 25 26 27 nil 28 29 30 31 32 33 34 35 36 37 38 39 40 [41 42]
    43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Saturday, is `incomplete' (Heshvan and Kislev each
have 29 days), and has Passover start on Tuesday.")

(defconst hebrew-calendar-year-Saturday-complete-Thursday
  [nil 52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
   23 24 25 26 27 nil 28 29 30 31 32 33 (nil . 34) (34 . 35) (35 . 36)
   (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Saturday, is `complete' (Heshvan and Kislev each
have 30 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Monday-incomplete-Thursday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
   23 24 25 26 27 nil 28 29 30 31 32 33 (nil . 34) (34 . 35) (35 . 36)
   (36 . 37) (37 . 38) ([38 39] . 39) 40 [41 42] 43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `incomplete' (Heshvan and Kislev each
have 29 days), and has Passover start on Thursday.")

(defconst hebrew-calendar-year-Monday-complete-Saturday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
   23 24 25 26 27 nil (nil . 28) (28 . 29) (29 . 30) (30 . 31) (31 . 32)
   (32 . 33) (33 . 34) (34 . 35) (35 . 36) (36 . 37) (37 . 38) (38 . 39)
   (39 . 40) (40 . 41) ([41 42] . 42) 43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Monday, is `complete' (Heshvan and Kislev each have
30 days), and has Passover start on Saturday.")

(defconst hebrew-calendar-year-Tuesday-regular-Saturday
  [51 52 nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
   23 24 25 26 27 nil (nil . 28) (28 . 29) (29 . 30) (30 . 31) (31 . 32)
   (32 . 33) (33 . 34) (34 . 35) (35 . 36) (36 . 37) (37 . 38) (38 . 39)
   (39 . 40) (40 . 41) ([41 42] . 42) 43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Tuesday, is `regular' (Heshvan has 29 days and
Kislev has 30 days), and has Passover start on Saturday.")

(defconst hebrew-calendar-year-Thursday-incomplete-Sunday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
    23 24 25 26 27 28 nil 29 30 31 32 33 34 35 36 37 38 39 40 41 42
    43 44 45 46 47 48 49 50]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `incomplete' (Heshvan and Kislev both
have 29 days), and has Passover start on Sunday.")

(defconst hebrew-calendar-year-Thursday-complete-Tuesday
  [52 nil nil 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
    23 24 25 26 27 28 nil 29 30 31 32 33 34 35 36 37 38 39 40 41 42
    43 44 45 46 47 48 49 [50 51]]
  "The structure of the parashiot.
Hebrew year that starts on Thursday, is `complete' (Heshvan and Kislev both
have 30 days), and has Passover start on Tuesday.")

(defun hebrew-calendar-parasha-name (p)
  "Name(s) corresponding to parasha P."
  (if (arrayp p);; combined parasha
      (format "%s/%s"
              (aref hebrew-calendar-parashiot-names (aref p 0))
              (aref hebrew-calendar-parashiot-names (aref p 1)))
    (aref hebrew-calendar-parashiot-names p)))

(defun list-islamic-diary-entries ()
  "Add any Islamic date entries from the diary file to `diary-entries-list'.
Islamic date diary entries must be prefaced by an `islamic-diary-entry-symbol'
\(normally an `I').  The same diary date forms govern the style of the Islamic
calendar entries, except that the Islamic month names must be spelled in full.
The Islamic months are numbered from 1 to 12 with Muharram being 1 and 12 being
Dhu al-Hijjah.  If an Islamic date diary entry begins with a
`diary-nonmarking-symbol', the entry will appear in the diary listing, but will
not be marked in the calendar.  This function is provided for use with the
`nongregorian-diary-listing-hook'."
  (if (< 0 number)
      (let ((buffer-read-only nil)
            (diary-modified (buffer-modified-p))
            (gdate original-date)
            (mark (regexp-quote diary-nonmarking-symbol)))
        (calendar-for-loop i from 1 to number do
           (let* ((d diary-date-forms)
                  (idate (calendar-islamic-from-absolute 
                          (calendar-absolute-from-gregorian gdate)))
                  (month (extract-calendar-month idate))
                  (day (extract-calendar-day idate))
                  (year (extract-calendar-year idate)))
             (while d
               (let*
                   ((date-form (if (equal (car (car d)) 'backup)
                                   (cdr (car d))
                                 (car d)))
                    (backup (equal (car (car d)) 'backup))
                    (dayname
                     (concat
                      (calendar-day-name gdate) "\\|"
                      (substring (calendar-day-name gdate) 0 3) ".?"))
                    (calendar-month-name-array
                     calendar-islamic-month-name-array)
                    (monthname
                     (concat
                      "\\*\\|"
                      (calendar-month-name month)))
                    (month (concat "\\*\\|0*" (int-to-string month)))
                    (day (concat "\\*\\|0*" (int-to-string day)))
                    (year
                     (concat
                      "\\*\\|0*" (int-to-string year)
                      (if abbreviated-calendar-year
                          (concat "\\|" (int-to-string (% year 100)))
                        "")))
                    (regexp
                     (concat
                      "\\(\\`\\|\^M\\|\n\\)" mark "?"
                      (regexp-quote islamic-diary-entry-symbol)
                      "\\("
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
                     ;;  Found a nonempty diary entry--make it visible and
                     ;;  add it to the list.
                     (let ((entry-start (point))
                           (date-start))
                       (re-search-backward "\^M\\|\n\\|\\`")
                       (setq date-start (point))
                       (re-search-forward "\^M\\|\n" nil t 2)
                       (while (looking-at " \\|\^I")
                         (re-search-forward "\^M\\|\n" nil t))
                       (backward-char 1)
                       (subst-char-in-region date-start (point) ?\^M ?\n t)
                       (add-to-diary-list
                         gdate (buffer-substring entry-start (point)))))))
               (setq d (cdr d))))
           (setq gdate
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-absolute-from-gregorian gdate)))))
           (set-buffer-modified-p diary-modified))
        (goto-char (point-min))))

(defun mark-islamic-diary-entries ()
  "Mark days in the calendar window that have Islamic date diary entries.
Each entry in diary-file (or included files) visible in the calendar window
is marked.  Islamic date entries are prefaced by a islamic-diary-entry-symbol
\(normally an `I').  The same diary-date-forms govern the style of the Islamic
calendar entries, except that the Islamic month names must be spelled in full.
The Islamic months are numbered from 1 to 12 with Muharram being 1 and 12 being
Dhu al-Hijjah.  Islamic date diary entries that begin with a
diary-nonmarking-symbol will not be marked in the calendar.  This function is
provided for use as part of the nongregorian-diary-marking-hook."
  (let ((d diary-date-forms))
    (while d
      (let*
          ((date-form (if (equal (car (car d)) 'backup)
                          (cdr (car d))
                        (car d)));; ignore 'backup directive
           (dayname (diary-name-pattern calendar-day-name-array))
           (monthname
            (concat
             (diary-name-pattern calendar-islamic-month-name-array t)
             "\\|\\*"))
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
             "\\(\\`\\|\^M\\|\n\\)"
             (regexp-quote islamic-diary-entry-symbol)
             "\\("
             (mapconcat 'eval date-form "\\)\\(")
             "\\)"))
           (case-fold-search t))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let* ((dd-name
                  (if d-name-pos
                      (buffer-substring
                       (match-beginning d-name-pos)
                       (match-end d-name-pos))))
                 (mm-name
                  (if m-name-pos
                      (buffer-substring
                       (match-beginning m-name-pos)
                       (match-end m-name-pos))))
                 (mm (string-to-int
                      (if m-pos
                          (buffer-substring
                           (match-beginning m-pos)
                           (match-end m-pos))
                        "")))
                 (dd (string-to-int
                      (if d-pos
                          (buffer-substring
                           (match-beginning d-pos)
                           (match-end d-pos))
                        "")))
                 (y-str (if y-pos
                            (buffer-substring
                             (match-beginning y-pos)
                             (match-end y-pos))))
                 (yy (if (not y-str)
                         0
                       (if (and (= (length y-str) 2)
                                abbreviated-calendar-year)
                           (let* ((current-y
                                   (extract-calendar-year
                                    (calendar-islamic-from-absolute
                                     (calendar-absolute-from-gregorian
                                      (calendar-current-date)))))
                                  (y (+ (string-to-int y-str)
                                        (* 100 (/ current-y 100)))))
                             (if (> (- y current-y) 50)
                                 (- y 100)
                               (if (> (- current-y y) 50)
                                   (+ y 100)
                                 y)))
                         (string-to-int y-str)))))
            (if dd-name
                (mark-calendar-days-named
                 (cdr (assoc (capitalize (substring dd-name 0 3))
                             (calendar-make-alist
                               calendar-day-name-array
                               0
                               '(lambda (x) (substring x 0 3))))))
              (if mm-name
                  (if (string-equal mm-name "*")
                      (setq mm 0)
                    (setq mm
                          (cdr (assoc
                                (capitalize mm-name)
                                (calendar-make-alist
                                  calendar-islamic-month-name-array))))))
              (mark-islamic-calendar-date-pattern mm dd yy)))))
      (setq d (cdr d)))))

(defun mark-islamic-calendar-date-pattern (month day year)
  "Mark dates in calendar window that conform to Islamic date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (if (and (/= 0 month) (/= 0 day))
        (if (/= 0 year)
            ;; Fully specified Islamic date.
            (let ((date (calendar-gregorian-from-absolute
                         (calendar-absolute-from-islamic
                          (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (mark-visible-calendar-date date)))
          ;; Month and day in any year--this taken from the holiday stuff.
          (let* ((islamic-date (calendar-islamic-from-absolute
                                (calendar-absolute-from-gregorian
                                 (list displayed-month 15 displayed-year))))
                 (m (extract-calendar-month islamic-date))
                 (y (extract-calendar-year islamic-date))
                 (date))
            (if (< m 1)
                nil;;   Islamic calendar doesn't apply.
              (increment-calendar-month m y (- 10 month))
              (if (> m 7);;  Islamic date might be visible
                  (let ((date (calendar-gregorian-from-absolute
                               (calendar-absolute-from-islamic
                                (list month day y)))))
                    (if (calendar-date-is-visible-p date)
                        (mark-visible-calendar-date date)))))))
      ;; Not one of the simple cases--check all visible dates for match.
      ;; Actually, the following code takes care of ALL of the cases, but
      ;; it's much too slow to be used for the simple (common) cases.
      (let ((m displayed-month)
            (y displayed-year)
            (first-date)
            (last-date))
        (increment-calendar-month m y -1)
        (setq first-date
              (calendar-absolute-from-gregorian
               (list m 1 y)))
        (increment-calendar-month m y 2)
        (setq last-date
              (calendar-absolute-from-gregorian
               (list m (calendar-last-day-of-month m y) y)))
        (calendar-for-loop date from first-date to last-date do
          (let* ((i-date (calendar-islamic-from-absolute date))
                 (i-month (extract-calendar-month i-date))
                 (i-day (extract-calendar-day i-date))
                 (i-year (extract-calendar-year i-date)))
            (and (or (zerop month)
                     (= month i-month))
                 (or (zerop day)
                     (= day i-day))
                 (or (zerop year)
                     (= year i-year))
                 (mark-visible-calendar-date
                  (calendar-gregorian-from-absolute date)))))))))

(provide 'diary-lib)

;;; diary-lib.el ends here
