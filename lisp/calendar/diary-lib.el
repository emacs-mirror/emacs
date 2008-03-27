;;; diary-lib.el --- diary functions

;; Copyright (C) 1989, 1990, 1992, 1993, 1994, 1995, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Code:

(require 'calendar)

(eval-and-compile
  (load "diary-loaddefs" nil 'quiet))

(defcustom diary-include-string "#include"
  "The string indicating inclusion of another file of diary entries.
See the documentation for the function `include-other-diary-files'."
  :type 'string
  :group 'diary)

(defcustom diary-list-include-blanks nil
  "If nil, do not include days with no diary entry in the list of diary entries.
Such days will then not be shown in the fancy diary buffer, even if they
are holidays."
  :type 'boolean
  :group 'diary)

(defcustom diary-face 'diary
  "Face name to use for diary entries."
  :type 'face
  :group 'diary)
(make-obsolete-variable 'diary-face "customize the face `diary' instead."
                        "23.1")

(defface diary-anniversary '((t :inherit font-lock-keyword-face))
  "Face used for anniversaries in the fancy diary display."
  :version "22.1"
  :group 'diary)

(defface diary-time '((t :inherit font-lock-variable-name-face))
  "Face used for times of day in the diary."
  :version "22.1"
  :group 'diary)

(defface diary-button '((((type pc) (class color))
                         (:foreground "lightblue")))
  "Default face used for buttons."
  :version "22.1"
  :group 'diary)
;; Backward-compatibility alias. FIXME make obsolete.
(put 'diary-button-face 'face-alias 'diary-button)

;; Face markup of calendar and diary displays: Any entry line that
;; ends with [foo:value] where foo is a face attribute (except :box
;; :stipple) or with [face:blah] tags, will have these values applied
;; to the calendar and fancy diary displays.  These attributes "stack"
;; on calendar displays.  File-wide attributes can be defined as
;; follows: the first line matching "^# [tag:value]" defines the value
;; for that particular tag.
(defcustom diary-face-attrs
  '((" *\\[foreground:\\([-a-z]+\\)\\]$" 1 :foreground string)
    (" *\\[background:\\([-a-z]+\\)\\]$" 1 :background string)
    (" *\\[width:\\([-a-z]+\\)\\]$" 1 :width symbol)
    (" *\\[height:\\([.0-9]+\\)\\]$" 1 :height int)
    (" *\\[weight:\\([-a-z]+\\)\\]$" 1 :weight symbol)
    (" *\\[slant:\\([-a-z]+\\)\\]$" 1 :slant symbol)
    (" *\\[underline:\\([-a-z]+\\)\\]$" 1 :underline stringtnil)
    (" *\\[overline:\\([-a-z]+\\)\\]$" 1 :overline stringtnil)
    (" *\\[strike-through:\\([-a-z]+\\)\\]$" 1 :strike-through stringtnil)
    (" *\\[inverse-video:\\([-a-z]+\\)\\]$" 1 :inverse-video tnil)
    (" *\\[face:\\([-0-9a-z]+\\)\\]$" 1 :face string)
    (" *\\[font:\\([-a-z0-9]+\\)\\]$" 1 :font string)
    ;; Unsupported.
;;;    (" *\\[box:\\([-a-z]+\\)\\]$" 1 :box)
;;;    (" *\\[stipple:\\([-a-z]+\\)\\]$" 1 :stipple)
    )
  "Alist of (REGEXP SUBEXP ATTRIBUTE TYPE) elements.
This is used by `diary-pull-attrs' to fontify certain diary
elements.  REGEXP is a regular expression to for, and SUBEXP is
the numbered sub-expression to extract.  `diary-glob-file-regexp-prefix'
is pre-pended to REGEXP for file-wide specifiers.  ATTRIBUTE
specifies which face attribute (e.g. `:foreground') to modify, or
that this is a face (`:face') to apply.  TYPE is the type of
attribute being applied.  Available TYPES (see `diary-attrtype-convert')
are: `string', `symbol', `int', `tnil',`stringtnil.'"
  :type '(repeat (list (string :tag "Regular expression")
                       (integer :tag "Sub-expression")
                       (symbol :tag "Attribute (e.g. :foreground)")
                       (choice (const string :tag "A string")
                               (const symbol :tag "A symbol")
                               (const int :tag "An integer")
                               (const tnil :tag "`t' or `nil'")
                               (const stringtnil
                                      :tag "A string, `t', or `nil'"))))
  :group 'diary)

(defcustom diary-glob-file-regexp-prefix "^\\#"
  "Regular expression pre-pended to `diary-face-attrs' for file-wide specifiers."
  :type 'regexp
  :group 'diary)

(defcustom diary-file-name-prefix nil
  "Non-nil means prefix each diary entry with the name of the file defining it."
  :type 'boolean
  :group 'diary)

(defcustom diary-file-name-prefix-function 'identity
  "The function that will take a diary file name and return the desired prefix."
  :type 'function
  :group 'diary)

(defcustom sexp-diary-entry-symbol "%%"
  "The string used to indicate a sexp diary entry in `diary-file'.
See the documentation for the function `list-sexp-diary-entries'."
  :type 'string
  :group 'diary)

(defcustom list-diary-entries-hook nil
  "List of functions called after diary file is culled for relevant entries.
You might wish to add `include-other-diary-files', in which case
you will probably also want to add `mark-included-diary-files' to
`mark-diary-entries-hook'.  For example, you could use

     (add-hook 'list-diary-entries-hook 'include-other-diary-files)
     (add-hook 'list-diary-entries-hook 'sort-diary-entries)
     (add-hook 'diary-display-hook 'fancy-diary-display)

in your `.emacs' file to cause the fancy diary buffer to be displayed with
diary entries from various included files, each day's entries sorted into
lexicographic order."
  :type 'hook
  :options '(include-other-diary-files sort-diary-entries)
  :group 'diary)

(defcustom mark-diary-entries-hook nil
  "List of functions called after marking diary entries in the calendar.
You might wish to add `mark-included-diary-files', in which case
you will probably also want to add `include-other-diary-files' to
`list-diary-entries-hook'."
  :type 'hook
  :options '(mark-included-diary-files)
  :group 'diary)

(defcustom nongregorian-diary-listing-hook nil
  "List of functions called for listing diary file and included files.
As the files are processed for diary entries, these functions are used
to cull relevant entries.  You can use any or all of
`list-hebrew-diary-entries', `list-islamic-diary-entries' and
`diary-bahai-list-entries'.  The documentation for these functions
describes the style of such diary entries."
  :type 'hook
  :options '(list-hebrew-diary-entries
             list-islamic-diary-entries
             diary-bahai-list-entries)
  :group 'diary)

(defcustom nongregorian-diary-marking-hook nil
  "List of functions called for marking diary file and included files.
As the files are processed for diary entries, these functions are used
to cull relevant entries.  You can use any or all of
`mark-hebrew-diary-entries', `mark-islamic-diary-entries' and
`bahai-mark-diary-entries'.  The documentation for these functions
describes the style of such diary entries."
  :type 'hook
  :options '(mark-hebrew-diary-entries
             mark-islamic-diary-entries
             diary-bahai-mark-entries)
  :group 'diary)

(defcustom print-diary-entries-hook 'lpr-buffer
  "List of functions called after a temporary diary buffer is prepared.
The buffer shows only the diary entries currently visible in the diary
buffer.  The default just does the printing.  Other uses might include, for
example, rearranging the lines into order by day and time, saving the buffer
instead of deleting it, or changing the function used to do the printing."
  :type 'hook
  :group 'diary)

(defcustom diary-unknown-time -9999
  "Value returned by `diary-entry-time' when no time is found.
The default value -9999 causes entries with no recognizable time to be placed
before those with times; 9999 would place entries with no recognizable time
after those with times."
  :type 'integer
  :group 'diary
  :version "20.3")

(defcustom diary-mail-addr
  (if (boundp 'user-mail-address) user-mail-address "")
  "Email address that `diary-mail-entries' will send email to."
  :group 'diary
  :type  'string
  :version "20.3")

(defcustom diary-mail-days 7
  "Default number of days for `diary-mail-entries' to check."
  :group 'diary
  :type 'integer
  :version "20.3")

(defcustom diary-remind-message
  '("Reminder: Only "
    (if (zerop (% days 7))
        (format "%d week%s" (/ days 7) (if (= 7 days) "" "s"))
      (format "%d day%s" days (if (= 1 days) "" "s")))
    " until "
    diary-entry)
  "Pseudo-pattern giving form of reminder messages in the fancy diary display.

Used by the function `diary-remind', a pseudo-pattern is a list of
expressions that can involve the keywords `days' (a number), `date' (a list of
month, day, year), and `diary-entry' (a string)."
  :type 'sexp
  :group 'diary)

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
                             (function :tag
                                       "Unary function providing template")))
  :version "22.1"
  :group 'diary)


(defun diary-set-header (symbol value)
  "Set SYMBOL's value to VALUE, and redraw the diary header if necessary."
  (let ((oldvalue (symbol-value symbol))
        (dbuff (and diary-file
                    (find-buffer-visiting
                     (substitute-in-file-name diary-file)))))
    (custom-set-default symbol value)
    (and dbuff
         (not (equal value oldvalue))
         (with-current-buffer dbuff
           (if (eq major-mode 'diary-mode)
               (setq header-line-format (and diary-header-line-flag
                                             diary-header-line-format)))))))

;; This can be removed once the kill/yank treatment of invisible text
;; (see etc/TODO) is fixed. -- gm
(defcustom diary-header-line-flag t
  "Non-nil means `simple-diary-display' will show a header line.
The format of the header is specified by `diary-header-line-format'."
  :group   'diary
  :type    'boolean
  :initialize 'custom-initialize-default
  :set 'diary-set-header
  :version "22.1")

(defvar diary-selective-display nil
  "Internal diary variable; non-nil if some diary text is hidden.")

(defcustom diary-header-line-format
  '(:eval (calendar-string-spread
           (list (if diary-selective-display
                     "Some text is hidden - press \"s\" in calendar \
before edit/copy"
                   "Diary"))
           ?\s (frame-width)))
  "Format of the header line displayed by `simple-diary-display'.
Only used if `diary-header-line-flag' is non-nil."
  :group 'diary
  :type 'sexp
  :initialize 'custom-initialize-default
  :set 'diary-set-header
  :version "22.1")

;; The first version of this also checked for diary-selective-display
;; in the non-fancy case. This was an attempt to distinguish between
;; displaying the diary and just visiting the diary file. However,
;; when using fancy diary, calling diary when there are no entries to
;; display does not create the fancy buffer, nor does it set
;; diary-selective-display in the diary buffer. This means some
;; customizations will not take effect, eg:
;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-03/msg00466.html
;; So the check for diary-selective-display was dropped. This means the
;; diary will be displayed if one customizes a diary variable while
;; just visiting the diary-file. This is i) unlikely, and ii) no great loss.
;;;###cal-autoload
(defun diary-live-p ()
  "Return non-nil if the diary is being displayed."
  (or (get-buffer fancy-diary-buffer)
      (and diary-file
           (find-buffer-visiting (substitute-in-file-name diary-file)))))

;;;###cal-autoload
(defun diary-set-maybe-redraw (symbol value)
  "Set SYMBOL's value to VALUE, and redraw the diary if necessary.
Redraws the diary if it is being displayed (note this is not the same as
just visiting the `diary-file'), and SYMBOL's value is to be changed."
  (let ((oldvalue (symbol-value symbol)))
    (custom-set-default symbol value)
    (and (not (equal value oldvalue))
         (diary-live-p)
         ;; Note this assumes diary was called without prefix arg.
         (diary))))

(defcustom number-of-diary-entries 1
  "Specifies how many days of diary entries are to be displayed initially.
This variable affects the diary display when the command \\[diary] is used,
or if the value of the variable `view-diary-entries-initially' is non-nil.
For example, if the default value 1 is used, then only the current day's diary
entries will be displayed.  If the value 2 is used, then both the current
day's and the next day's entries will be displayed.

The value can also be a vector such as [0 2 2 2 2 4 1]; this value
says to display no diary entries on Sunday, the entries for
the current date and the day after on Monday through Thursday,
Friday through Monday's entries on Friday, and only Saturday's
entries on Saturday.

This variable does not affect the diary display with the `d' command
from the calendar; in that case, the prefix argument controls the
number of days of diary entries displayed."
  :type '(choice (integer :tag "Entries")
                 (vector :value [0 0 0 0 0 0 0]
                         (integer :tag "Sunday")
                         (integer :tag "Monday")
                         (integer :tag "Tuesday")
                         (integer :tag "Wednesday")
                         (integer :tag "Thursday")
                         (integer :tag "Friday")
                         (integer :tag "Saturday")))
  :initialize 'custom-initialize-default
  :set 'diary-set-maybe-redraw
  :group 'diary)

;;; More user options in calendar.el.


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
  (diary-list-entries (calendar-current-date)
                      (if arg (prefix-numeric-value arg))))

;;;###cal-autoload
(defun diary-view-entries (&optional arg)
  "Prepare and display a buffer with diary entries.
Searches the file named in `diary-file' for entries that
match ARG days starting with the date indicated by the cursor position
in the displayed three-month calendar."
  (interactive "p")
  (diary-check-diary-file)
  (diary-list-entries (calendar-cursor-to-date t) arg))

(define-obsolete-function-alias 'view-diary-entries 'diary-view-entries)


;;;###cal-autoload
(defun view-other-diary-entries (arg dfile)
  "Prepare and display buffer of diary entries from an alternative diary file.
Searches for entries that match ARG days, starting with the date indicated
by the cursor position in the displayed three-month calendar.
DFILE specifies the file to use as the diary file."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-file-name "Enter diary file name: " default-directory nil t)))
  (let ((diary-file dfile))
    (diary-view-entries arg)))

(defvar diary-syntax-table
  (let ((st (copy-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?* "w" st)
    (modify-syntax-entry ?: "w" st)
    st)
  "The syntax table used when parsing dates in the diary file.
It is the standard syntax table used in Fundamental mode, but with the
syntax of `*' and `:' changed to be word constituents.")

(defun diary-attrtype-convert (attrvalue type)
  "Convert string ATTRVALUE to TYPE appropriate for a face description.
Valid TYPEs are: string, symbol, int, stringtnil, tnil."
  (cond ((eq type 'string) attrvalue)
        ((eq type 'symbol) (intern-soft attrvalue))
        ((eq type 'int) (string-to-number attrvalue))
        ((eq type 'stringtnil)
         (cond ((string-equal "t" attrvalue) t)
               ((string-equal "nil" attrvalue) nil)
               (t attrvalue)))
        ((eq type 'tnil) (string-equal "t" attrvalue))))

(defun diary-pull-attrs (entry fileglobattrs)
  "Search for matches for regexps from `diary-face-attrs'.
If ENTRY is nil, searches from the start of the current buffer, and
prepends all regexps with `diary-glob-file-regexp-prefix'.
If ENTRY is a string, search for matches in that string, and remove them.
Returns a list of ENTRY followed by (ATTRIBUTE VALUE) pairs.
When ENTRY is non-nil, FILEGLOBATTRS forms the start of the (ATTRIBUTE VALUE)
pairs."
  (let (regexp regnum attrname attrname attrvalue type ret-attr)
    (if (null entry)
        (save-excursion
          (dolist (attr diary-face-attrs)
            ;; FIXME inefficient searching.
            (goto-char (point-min))
            (setq regexp (concat diary-glob-file-regexp-prefix (car attr))
                  regnum (cadr attr)
                  attrname (nth 2 attr)
                  type (nth 3 attr)
                  attrvalue (if (re-search-forward regexp nil t)
                                (match-string-no-properties regnum)))
            (and attrvalue
                 (setq attrvalue (diary-attrtype-convert attrvalue type))
                 (setq ret-attr (append ret-attr
                                        (list attrname attrvalue))))))
      (setq ret-attr fileglobattrs)
      (dolist (attr diary-face-attrs)
        (setq regexp (car attr)
              regnum (cadr attr)
              attrname (nth 2 attr)
              type (nth 3 attr)
              attrvalue nil)
        ;; FIXME multiple matches?
        (if (string-match regexp entry)
            (setq attrvalue (match-string-no-properties regnum entry)
                  entry (replace-match "" t t entry)))
        (and attrvalue
             (setq attrvalue (diary-attrtype-convert attrvalue type))
             (setq ret-attr (append ret-attr (list attrname attrvalue))))))
    (list entry ret-attr)))



(defvar diary-modify-entry-list-string-function nil
  "Function applied to entry string before putting it into the entries list.
Can be used by programs integrating a diary list into other buffers (e.g.
org.el and planner.el) to modify the string or add properties to it.
The function takes a string argument and must return a string.")

(defvar diary-entries-list)             ; bound in diary-list-entries

(defun add-to-diary-list (date string specifier &optional marker
                               globcolor literal)
  "Add an entry to `diary-entries-list'.
Do nothing if DATE or STRING is nil.  DATE is the (MONTH DAY
YEAR) for which the entry applies; STRING is the text of the
entry as it will appear in the diary (i.e. with any format
strings such as \"%d\" expanded); SPECIFIER is the date part of
the entry as it appears in the diary-file; LITERAL is the entry
as it appears in the diary-file (i.e. before expansion).  If
LITERAL is nil, it is taken to be the same as STRING.

The entry is added to the list as (DATE STRING SPECIFIER LOCATOR
GLOBCOLOR), where LOCATOR has the form (MARKER FILENAME LITERAL),
FILENAME being the file containing the diary entry."
  (when (and date string)
    (if diary-file-name-prefix
        (let ((prefix (funcall diary-file-name-prefix-function
                               (buffer-file-name))))
          (or (string-equal prefix "")
              (setq string (format "[%s] %s" prefix string)))))
    (and diary-modify-entry-list-string-function
         (setq string (funcall diary-modify-entry-list-string-function
                               string)))
    (setq diary-entries-list
          (append diary-entries-list
                  (list (list date string specifier
                              (list marker (buffer-file-name) literal)
                              globcolor))))))

(defvar number)                     ; not clear this should use number

(defun diary-list-entries-2 (date mark globattr list-only
                                  &optional months symbol)
  "Internal subroutine of `diary-list-entries'.
Find diary entries applying to DATE, by searching from point-min for
each element of `diary-date-forms'.  MARK indicates an entry is non-marking.
GLOBATTR is the list of global file attributes.  If LIST-ONLY is
non-nil, don't change the buffer, only return a list of entries.
Optional array MONTHS replaces `calendar-month-name-array', and
means months cannot be abbreviated.  Optional string SYMBOL marks diary
entries of the desired type.  Returns non-nil if any entries were found."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (dayname (format "%s\\|%s\\.?" (calendar-day-name date)
                          (calendar-day-name date 'abbrev)))
         (calendar-month-name-array (or months calendar-month-name-array))
         (monthname (format "\\*\\|%s%s" (calendar-month-name month)
                            (if months ""
                              (format "\\|%s\\.?"
                                      (calendar-month-name month 'abbrev)))))
         (month (format "\\*\\|0*%d" month))
         (day (format "\\*\\|0*%d" day))
         (year (format "\\*\\|0*%d%s" year
                       (if abbreviated-calendar-year
                           ;; FIXME was %d in non-greg case.
                           (format "\\|%02d" (% year 100))
                         "")))
        (case-fold-search t)
        entry-found)
    (dolist (date-form diary-date-forms)
      (let ((backup (when (eq (car date-form) 'backup)
                      (setq date-form (cdr date-form))
                      t))
            ;; date-form uses day etc as set above.
            (regexp (format "^%s?%s\\(%s\\)" (regexp-quote mark)
                            (if symbol (regexp-quote symbol) "")
                            (mapconcat 'eval date-form "\\)\\(?:")))
            entry-start date-start temp)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (if backup (re-search-backward "\\<" nil t))
          (if (and (bolp) (not (looking-at "[ \t]")))
              ;;  Diary entry that consists only of date.
              (backward-char 1)
            ;; Found a nonempty diary entry--make it
            ;; visible and add it to the list.
            ;; Actual entry starts on the next-line?
            (if (looking-at "[ \t]*\n[ \t]") (forward-line 1))
            (setq entry-found t
                  entry-start (point)
                  ;; If bolp, must have done (forward-line 1).
                  ;; FIXME Why number > 1?
                  date-start (line-end-position (if (and (bolp) (> number 1))
                                                    -1 0)))
            (forward-line 1)
            (while (looking-at "[ \t]") ; continued entry
              (forward-line 1))
            (unless (and (eobp) (not (bolp)))
              (backward-char 1))
            (unless list-only
              (remove-overlays date-start (point) 'invisible 'diary))
            (setq temp (diary-pull-attrs
                        (buffer-substring-no-properties
                         entry-start (point)) globattr))
            (add-to-diary-list
             date (car temp)
             (buffer-substring-no-properties (1+ date-start) (1- entry-start))
             (copy-marker entry-start) (cadr temp))))))
    entry-found))

(defvar original-date)                  ; from diary-list-entries
(defvar file-glob-attrs)
(defvar list-only)

(defun diary-list-entries-1 (months symbol absfunc)
  "List diary entries of a certain type.
MONTHS is an array of month names.  SYMBOL marks diary entries of the type
in question.  ABSFUNC is a function that converts absolute dates to dates
of the appropriate type."
  (let ((gdate original-date))
    (dotimes (idummy number)
      (diary-list-entries-2
       (funcall absfunc (calendar-absolute-from-gregorian gdate))
       diary-nonmarking-symbol file-glob-attrs list-only months symbol)
      (setq gdate
            (calendar-gregorian-from-absolute
             (1+ (calendar-absolute-from-gregorian gdate))))))
  (goto-char (point-min)))

;; FIXME non-greg and list hooks run same number of times?
(define-obsolete-function-alias 'list-diary-entries 'diary-list-entries)
(defun diary-list-entries (date number &optional list-only)
  "Create and display a buffer containing the relevant lines in `diary-file'.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using overlays.  If NUMBER is less than 1, this function does nothing.

Returns a list of all relevant diary entries found, if any, in order by date.
The list entries have the form ((MONTH DAY YEAR) STRING SPECIFIER) where
\(MONTH DAY YEAR) is the date of the entry, STRING is the entry text, and
SPECIFIER is the applicability.  If the variable `diary-list-include-blanks'
is non-nil, this list includes a dummy diary entry consisting of the empty
string for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`list-diary-entries-hook', `diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file, for example to process Islamic diary
        entries.  Applied to *each* file.

    `list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `diary-display-hook' does the actual display of information.  If this is
        nil, `simple-diary-display' will be used.  Use `add-hook' to use
        `fancy-diary-display', if desired.  If you want no diary display, use
        add-hook to set this to `ignore'.

    `diary-hook' is run last.  This can be used for an appointment
        notification function.

Functions called by these hooks may use the variables ORIGINAL-DATE
and NUMBER, which are the arguments with which this function was called.
Note that hook functions should _not_ use DATE, but ORIGINAL-DATE.
\(Sexp diary entries may use DATE - see `list-sexp-diary-entries'.)

If LIST-ONLY is non-nil don't modify or display the buffer, only return a list."
  (unless number
    (setq number (if (vectorp number-of-diary-entries)
                     (aref number-of-diary-entries (calendar-day-of-week date))
                   number-of-diary-entries)))
  (when (> number 0)
    (let ((original-date date)    ; save for possible use in the hooks
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
        ;; Setup things like the header-line-format and invisibility-spec.
        (if (eq major-mode default-major-mode)
            (diary-mode)
          ;; This kludge is to make customizations to
          ;; diary-header-line-flag after diary has been displayed
          ;; take effect. Unconditionally calling (diary-mode)
          ;; clobbers file local variables.
          ;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-03/msg00363.html
          ;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-04/msg00404.html
          (if (eq major-mode 'diary-mode)
              (setq header-line-format (and diary-header-line-flag
                                            diary-header-line-format))))
        ;; d-s-p is passed to the diary display function.
        (let ((diary-saved-point (point)))
          (save-excursion
            ;; FIXME move after goto? Syntax?
            (setq file-glob-attrs (cadr (diary-pull-attrs nil "")))
            (with-syntax-table diary-syntax-table
              (goto-char (point-min))
              (unless list-only
                (let ((ol (make-overlay (point-min) (point-max) nil t nil)))
                  (set (make-local-variable 'diary-selective-display) t)
                  (overlay-put ol 'invisible 'diary)
                  (overlay-put ol 'evaporate t)))
              (dotimes (idummy number)
                (let ((sexp-found (list-sexp-diary-entries date))
                      (entry-found (diary-list-entries-2
                                    date diary-nonmarking-symbol
                                    file-glob-attrs list-only)))
                  (if diary-list-include-blanks
                      (or sexp-found entry-found
                          (add-to-diary-list date "" "" "" "")))
                  (setq date
                        (calendar-gregorian-from-absolute
                         (1+ (calendar-absolute-from-gregorian date)))))))
            (goto-char (point-min))
            (run-hooks 'nongregorian-diary-listing-hook
                       'list-diary-entries-hook)
            (unless list-only
              (if diary-display-hook
                  (run-hooks 'diary-display-hook)
                (simple-diary-display)))
            (run-hooks 'diary-hook)
            diary-entries-list))))))

(defun diary-unhide-everything ()
  "Show all invisible text in the diary."
  (kill-local-variable 'diary-selective-display)
  (remove-overlays (point-min) (point-max) 'invisible 'diary)
  (kill-local-variable 'mode-line-format))

(defvar original-date)                  ; bound in diary-list-entries
(defvar number)

(defun include-other-diary-files ()
  "Include the diary entries from other diary files with those of `diary-file'.
This function is suitable for use with `list-diary-entries-hook';
it enables you to use shared diary files together with your own.
The files included are specified in the `diary-file' by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
  (goto-char (point-min))
  (while (re-search-forward
          (format "^%s \"\\([^\"]*\\)\"" (regexp-quote diary-include-string))
          nil t)
    (let ((diary-file (substitute-in-file-name
                       (match-string-no-properties 1)))
          (diary-list-include-blanks nil)
          (list-diary-entries-hook 'include-other-diary-files)
          (diary-display-hook 'ignore)
          (diary-hook nil))
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (unwind-protect
                  (setq diary-entries-list
                        (append diary-entries-list
                                (diary-list-entries original-date number)))
                (with-current-buffer (find-buffer-visiting diary-file)
                  (diary-unhide-everything)))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
  (goto-char (point-min)))

(defvar date-string)                    ; bound in diary-list-entries

(defun diary-display-no-entries ()
  "Common subroutine of `simple-diary-display' and `fancy-diary-display'.
Handles the case where there are no diary entries.
Returns a cons (NOENTRIES . HOLIDAY-STRING)."
    (let* ((holiday-list (if holidays-in-diary-buffer
                             (calendar-check-holidays original-date)))
           (hol-string (format "%s%s%s"
                               date-string
                               (if holiday-list ": " "")
                               (mapconcat 'identity holiday-list "; ")))
           (msg (format "No diary entries for %s" hol-string))
           ;; Empty list, or single item with no text.
           ;; FIXME multiple items with no text?
           (noentries (or (not diary-entries-list)
                          (and (not (cdr diary-entries-list))
                               (string-equal "" (cadr
                                                 (car diary-entries-list)))))))
      ;; Inconsistency: whether or not the holidays are displayed in a
      ;; separate buffer depends on if there are diary entries.
      (when noentries
        (if (or (< (length msg) (frame-width))
                (not holiday-list))
            (message "%s" msg)
          ;; holiday-list which is too wide for a message gets a buffer.
          (calendar-in-read-only-buffer holiday-buffer
            (calendar-set-mode-line (format "Holidays for %s" date-string))
            (insert (mapconcat 'identity holiday-list "\n")))
          (message "No diary entries for %s" date-string)))
      (cons noentries hol-string)))


(defvar diary-saved-point)              ; bound in diary-list-entries

(defun simple-diary-display ()
  "Display the diary buffer if there are any relevant entries or holidays."
  ;; If selected window is dedicated (to the calendar), need a new one
  ;; to display the diary.
  (let* ((pop-up-frames (or pop-up-frames
                            (window-dedicated-p (selected-window))))
         (dbuff (find-buffer-visiting (substitute-in-file-name diary-file)))
         (empty (diary-display-no-entries)))
    ;; This may be too wide, but when simple diary is used there is
    ;; nowhere else for the holidays to go.  Also, it is documented in
    ;; holidays-in-diary-buffer that the holidays go in the mode-line.
    ;; FIXME however if there are no diary entries a separate buffer
    ;; is displayed - this is inconsistent.
    (with-current-buffer dbuff
      (calendar-set-mode-line (format "Diary for %s" (cdr empty))))
    (unless (car empty)                 ; no entries
      (with-current-buffer dbuff
        (let ((window (display-buffer (current-buffer))))
          ;; d-s-p is passed from diary-list-entries.
          (set-window-point window diary-saved-point)
          (set-window-start window (point-min))))
      (message "Preparing diary...done"))))

(define-button-type 'diary-entry
  'action #'diary-goto-entry
  'face 'diary-button)

(defun diary-goto-entry (button)
  "Jump to the diary entry for the BUTTON at point."
  (let* ((locator (button-get button 'locator))
         (marker (car locator))
         markbuf file)
    ;; If marker pointing to diary location is valid, use that.
    (if (and marker (setq markbuf (marker-buffer marker)))
        (progn
          (pop-to-buffer markbuf)
          (goto-char (marker-position marker)))
      ;; Marker is invalid (eg buffer has been killed).
      (or (and (setq file (cadr locator))
               (file-exists-p file)
               (find-file-other-window file)
               (progn
                 (when (eq major-mode default-major-mode) (diary-mode))
                 (goto-char (point-min))
                 (if (re-search-forward (format "%s.*\\(%s\\)"
                                                (regexp-quote (nth 2 locator))
                                                (regexp-quote (nth 3 locator)))
                                        nil t)
                     (goto-char (match-beginning 1)))))
          (message "Unable to locate this diary entry")))))

(defun fancy-diary-display ()
  "Prepare a diary buffer with relevant entries in a fancy, noneditable form.
To use this function, add it to `diary-display-hook'."
  ;; Turn off selective-display in the diary file's buffer.
  (with-current-buffer
      (find-buffer-visiting (substitute-in-file-name diary-file))
    (diary-unhide-everything))
  (unless (car (diary-display-no-entries)) ; no entries
    ;; Prepare the fancy diary buffer.
    (calendar-in-read-only-buffer fancy-diary-buffer
      (calendar-set-mode-line "Diary Entries")
      (let ((holiday-list-last-month 1)
            (holiday-list-last-year 1)
            (date (list 0 0 0))
            holiday-list)
        (dolist (entry diary-entries-list)
          (unless (calendar-date-equal date (car entry))
            (setq date (car entry))
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
                       (extract-calendar-month date)
                       holiday-list-last-year
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
            (let (date-holiday-list)
              ;; Make a list of all holidays for date.
              (dolist (h holiday-list)
                (if (calendar-date-equal date (car h))
                    (setq date-holiday-list (append date-holiday-list
                                                    (cdr h)))))
              (insert (if (bobp) "" ?\n) (calendar-date-string date))
              (if date-holiday-list (insert ":  "))
              (let ((l (current-column))
                    (longest 0))
                (insert (mapconcat (lambda (x)
                                     (if (< longest (length x))
                                         (setq longest (length x)))
                                     x)
                                   date-holiday-list
                                   (concat "\n" (make-string l ? ))))
                (insert ?\n (make-string (+ l longest) ?=) ?\n))))
          (let ((this-entry (cadr entry))
                this-loc)
            (unless (zerop (length this-entry))
              (if (setq this-loc (nth 3 entry))
                  (insert-button (concat this-entry "\n")
                                 ;; (MARKER FILENAME SPECIFIER LITERAL)
                                 'locator (list (car this-loc)
                                                (cadr this-loc)
                                                (nth 2 entry)
                                                (or (nth 2 this-loc)
                                                    (nth 1 entry)))
                                 :type 'diary-entry)
                (insert this-entry ?\n))
              (save-excursion
                (let* ((marks (nth 4 entry))
                       (faceinfo marks)
                       temp-face)
                  (when marks
                    (setq temp-face (make-symbol
                                     (apply
                                      'concat "temp-face-"
                                      (mapcar (lambda (sym)
                                                (if (stringp sym)
                                                    sym
                                                  (symbol-name sym)))
                                              marks))))
                    (make-face temp-face)
                    ;; Remove :face info from the marks,
                    ;; copy the face info into temp-face
                    (while (setq faceinfo (memq :face faceinfo))
                      (copy-face (read (nth 1 faceinfo)) temp-face)
                      (setcar faceinfo nil)
                      (setcar (cdr faceinfo) nil))
                    (setq marks (delq nil marks))
                    ;; Apply the font aspects.
                    (apply 'set-face-attribute temp-face nil marks)
                    (search-backward this-entry)
                    (overlay-put
                     (make-overlay (match-beginning 0) (match-end 0))
                     'face temp-face))))))))
      (fancy-diary-display-mode)
      (calendar-set-mode-line date-string)
      (message "Preparing diary...done"))))

;; FIXME modernize?
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
      (with-current-buffer (get-buffer fancy-diary-buffer)
        (run-hooks 'print-diary-entries-hook))
    (let ((diary-buffer
           (find-buffer-visiting (substitute-in-file-name diary-file))))
      (if diary-buffer
          ;; Name affects printing?
          (let ((temp-buffer (get-buffer-create " *Printable Diary Entries*"))
                heading)
            (with-current-buffer diary-buffer
              (setq heading
                    (if (not (stringp mode-line-format))
                        "All Diary Entries"
                      (string-match "^-*\\([^-].*[^-]\\)-*$" mode-line-format)
                      (match-string 1 mode-line-format)))
              (let ((start (point-min))
                    end)
                (while
                    (progn
                      (setq end (next-single-char-property-change
                                 start 'invisible))
                      (unless (get-char-property start 'invisible)
                        (with-current-buffer temp-buffer
                          (insert-buffer-substring diary-buffer
                                                   start (or end (point-max)))))
                      (setq start end)
                      (and end (< end (point-max))))))
              (set-buffer temp-buffer)
              (goto-char (point-min))
              (insert heading "\n"
                      (make-string (length heading) ?=) "\n")
              (run-hooks 'print-diary-entries-hook)
              (kill-buffer temp-buffer)))
        (error "You don't have a diary buffer!")))))

(define-obsolete-function-alias 'show-all-diary-entries 'diary-show-all-entries)
;;;###cal-autoload
(defun diary-show-all-entries ()
  "Show all of the diary entries in the diary file.
This function gets rid of the selective display of the diary file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created."
  (interactive)
  (let ((d-file (diary-check-diary-file))
        (pop-up-frames (window-dedicated-p (selected-window))))
    (with-current-buffer (or (find-buffer-visiting d-file)
                             (find-file-noselect d-file t))
      (when (eq major-mode default-major-mode) (diary-mode))
      (diary-unhide-everything)
      (display-buffer (current-buffer)))))

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
      (diary-list-entries (calendar-current-date) (or ndays diary-mail-days)))
    (compose-mail diary-mail-addr
                  (concat "Diary entries generated "
                          (calendar-date-string (calendar-current-date))))
    (insert
     (if (get-buffer fancy-diary-buffer)
         (with-current-buffer fancy-diary-buffer (buffer-string))
       "No entries found"))
    (call-interactively (get mail-user-agent 'sendfunc))))

(defun diary-name-pattern (string-array &optional abbrev-array paren)
  "Return a regexp matching the strings in the array STRING-ARRAY.
If the optional argument ABBREV-ARRAY is present, then the function
`calendar-abbrev-construct' is used to construct abbreviations from the
two supplied arrays.  The returned regexp will then also match these
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

;; file-glob-attrs bound in mark-diary-entries.
(defun diary-mark-entries-1 (markfunc &optional months symbol absfunc)
  "Mark diary entries of a certain type.
MARKFUNC is a function that marks entries of the appropriate type
matching a given date pattern.  MONTHS is an array of month names.
SYMBOL marks diary entries of the type in question.  ABSFUNC is a
function that converts absolute dates to dates of the appropriate type.  "
  (let ((dayname (diary-name-pattern calendar-day-name-array
                                     calendar-day-abbrev-array))
        (monthname (format "%s\\|\\*"
                           (if months
                               (diary-name-pattern months)
                             (diary-name-pattern calendar-month-name-array
                                                 calendar-month-abbrev-array))))
        (month "[0-9]+\\|\\*")
        (day "[0-9]+\\|\\*")
        (year "[0-9]+\\|\\*")
        (case-fold-search t)
        marks)
    (dolist (date-form diary-date-forms)
      (if (eq (car date-form) 'backup)  ; ignore 'backup directive
          (setq date-form (cdr date-form)))
      (let* ((l (length date-form))
             (d-name-pos (- l (length (memq 'dayname date-form))))
             (d-name-pos (if (/= l d-name-pos) (1+ d-name-pos)))
             (m-name-pos (- l (length (memq 'monthname date-form))))
             (m-name-pos (if (/= l m-name-pos) (1+ m-name-pos)))
             (d-pos (- l (length (memq 'day date-form))))
             (d-pos (if (/= l d-pos) (1+ d-pos)))
             (m-pos (- l (length (memq 'month date-form))))
             (m-pos (if (/= l m-pos) (1+ m-pos)))
             (y-pos (- l (length (memq 'year date-form))))
             (y-pos (if (/= l y-pos) (1+ y-pos)))
             (regexp (format "^%s\\(%s\\)"
                             (if symbol (regexp-quote symbol) "")
                             (mapconcat 'eval date-form "\\)\\("))))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let* ((dd-name
                  (if d-name-pos
                      (match-string-no-properties d-name-pos)))
                 (mm-name
                  (if m-name-pos
                      (match-string-no-properties m-name-pos)))
                 (mm (string-to-number
                      (if m-pos
                          (match-string-no-properties m-pos)
                        "")))
                 (dd (string-to-number
                      (if d-pos
                          (match-string-no-properties d-pos)
                        "")))
                 (y-str (if y-pos
                            (match-string-no-properties y-pos)))
                 (yy (if (not y-str)
                         0
                       (if (and (= (length y-str) 2)
                                abbreviated-calendar-year)
                           (let* ((current-y
                                   (extract-calendar-year
                                    (if absfunc
                                        (funcall
                                         absfunc
                                         (calendar-absolute-from-gregorian
                                          (calendar-current-date)))
                                      (calendar-current-date))))
                                  (y (+ (string-to-number y-str)
                                        (* 100 (/ current-y 100)))))
                             (if (> (- y current-y) 50)
                                 (- y 100)
                               (if (> (- current-y y) 50)
                                   (+ y 100)
                                 y)))
                         (string-to-number y-str)))))
            (setq marks (cadr (diary-pull-attrs
                               (buffer-substring-no-properties
                                (point) (line-end-position))
                               file-glob-attrs)))
            (if dd-name
                (mark-calendar-days-named
                 (cdr (assoc-string dd-name
                                    (calendar-make-alist
                                     calendar-day-name-array
                                     0 nil calendar-day-abbrev-array) t)) marks)
              (if mm-name
                  (setq mm
                        (if (string-equal mm-name "*") 0
                          (cdr (assoc-string
                                mm-name
                                (if months (calendar-make-alist months)
                                  (calendar-make-alist
                                   calendar-month-name-array
                                   1 nil calendar-month-abbrev-array)) t)))))
              (funcall markfunc mm dd yy marks))))))))

;;;###cal-autoload
(defun mark-diary-entries (&optional redraw)
  "Mark days in the calendar window that have diary entries.
Each entry in the diary file visible in the calendar window is
marked.  After the entries are marked, the hooks
`nongregorian-diary-marking-hook' and `mark-diary-entries-hook'
are run.  If the optional argument REDRAW is non-nil (which is
the case interactively, for example) then any existing diary
marks are first removed.  This is intended to deal with deleted
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
        file-glob-attrs)
    (with-current-buffer (find-file-noselect (diary-check-diary-file) t)
      (save-excursion
        (when (eq major-mode default-major-mode) (diary-mode))
        (setq mark-diary-entries-in-calendar t)
        (message "Marking diary entries...")
        (setq file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
        (with-syntax-table diary-syntax-table
          (diary-mark-entries-1 'mark-calendar-date-pattern)
          (mark-sexp-diary-entries)
          (run-hooks 'nongregorian-diary-marking-hook
                     'mark-diary-entries-hook))
        (message "Marking diary entries...done")))))


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
                              (count-lines (point-min) (point))
                              diary-file sexp)
                     (sleep-for 2))))))
    (cond ((stringp result) result)
          ((and (consp result)
                (stringp (cdr result))) result)
          (result entry)
          (t nil))))

(defvar displayed-year)                 ; bound in generate-calendar
(defvar displayed-month)

(defun mark-sexp-diary-entries ()
  "Mark days in the calendar window that have sexp diary entries.
Each entry in the diary file (or included files) visible in the calendar window
is marked.  See the documentation for the function `list-sexp-diary-entries'."
  (let* ((sexp-mark (regexp-quote sexp-diary-entry-symbol))
         (s-entry (format "^\\(%s(\\)\\|\\(%s%s(diary-remind\\)" sexp-mark
                          (regexp-quote diary-nonmarking-symbol)
                          sexp-mark))
         (file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
         m y first-date last-date mark file-glob-attrs)
    (with-current-buffer calendar-buffer
      (setq m displayed-month
            y displayed-year))
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
            sexp entry entry-start marks)
        (forward-sexp)
        (setq sexp (buffer-substring-no-properties sexp-start (point)))
        (forward-char 1)
        (if (and (bolp) (not (looking-at "[ \t]")))
            ;; Diary entry consists only of the sexp.
            (progn
              (backward-char 1)
              (setq entry ""))
          (setq entry-start (point))
          ;; Find end of entry.
          (forward-line 1)
          (while (looking-at "[ \t]")
            (forward-line 1))
          (if (bolp) (backward-char 1))
          (setq entry (buffer-substring-no-properties entry-start (point))))
        (calendar-for-loop date from first-date to last-date do
          (when (setq mark (diary-sexp-entry
                            sexp entry
                            (calendar-gregorian-from-absolute date)))
            ;; FIXME does this make sense?
            (setq marks (diary-pull-attrs entry file-glob-attrs)
                  marks (nth 1 (diary-pull-attrs entry file-glob-attrs)))
            (mark-visible-calendar-date
             (calendar-gregorian-from-absolute date)
             (if (< 0 (length marks))
                 marks
               (if (consp mark) (car mark))))))))))

(defun mark-included-diary-files ()
  "Mark the diary entries from other diary files with those of the diary file.
This function is suitable for use with `mark-diary-entries-hook'; it enables
you to use shared diary files together with your own.  The files included are
specified in the `diary-file' by lines of this form:
        #include \"filename\"
This is recursive; that is, #include directives in diary files thus included
are obeyed.  You can change the `#include' to some other string by
changing the variable `diary-include-string'."
  (goto-char (point-min))
  (while (re-search-forward
          (format "^%s \"\\([^\"]*\\)\"" (regexp-quote diary-include-string))
          nil t)
    (let* ((diary-file (substitute-in-file-name
                        (match-string-no-properties 1)))
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
0 means all Sundays, 1 means all Mondays, and so on.
Optional argument COLOR is passed to `mark-visible-calendar-date' as MARK."
  (with-current-buffer calendar-buffer
    (let ((prev-month displayed-month)
          (prev-year displayed-year)
          (succ-month displayed-month)
          (succ-year displayed-year)
          (last-day)
          (day))
      (increment-calendar-month succ-month succ-year 1)
      (increment-calendar-month prev-month prev-year -1)
      (setq day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day 1 dayname prev-month prev-year))
            last-day (calendar-absolute-from-gregorian
                      (calendar-nth-named-day -1 dayname succ-month succ-year)))
      (while (<= day last-day)
        (mark-visible-calendar-date (calendar-gregorian-from-absolute day)
                                    color)
        (setq day (+ day 7))))))

(defun mark-calendar-month (month year p-month p-day p-year &optional color)
  "Mark dates in the MONTH/YEAR that conform to pattern P-MONTH/P-DAY/P-YEAR.
A value of 0 in any position of the pattern is a wildcard.
Optional argument COLOR is passed to `mark-visible-calendar-date' as MARK."
  (if (or (and (= month p-month)
               (or (zerop p-year) (= year p-year)))
          (and (zerop p-month)
               (or (zerop p-year) (= year p-year))))
      (if (zerop p-day)
          (dotimes (i (calendar-last-day-of-month month year))
            (mark-visible-calendar-date (list month (1+ i) year) color))
        (mark-visible-calendar-date (list month p-day year) color))))

(defun mark-calendar-date-pattern (month day year &optional color)
  "Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `mark-visible-calendar-date' as MARK."
  (with-current-buffer calendar-buffer
    (let ((m displayed-month)
          (y displayed-year))
      (increment-calendar-month m y -1)
      (dotimes (idummy 3)
        (mark-calendar-month m y month day year color)
        (increment-calendar-month m y 1)))))


;; Bahai, Hebrew, Islamic.
(defun calendar-mark-complex (month day year fromabs &optional color)
  "Mark dates in the calendar conforming to MONTH DAY YEAR of some system.
The function FROMABS converts absolute dates to the appropriate date system.
Optional argument COLOR is passed to `mark-visible-calendar-date' as MARK."
  ;; Not one of the simple cases--check all visible dates for match.
  ;; Actually, the following code takes care of ALL of the cases, but
  ;; it's much too slow to be used for the simple (common) cases.
  (let ((m displayed-month)
        (y displayed-year)
        first-date last-date)
    (increment-calendar-month m y -1)
    (setq first-date (calendar-absolute-from-gregorian (list m 1 y)))
    (increment-calendar-month m y 2)
    (setq last-date (calendar-absolute-from-gregorian
                     (list m (calendar-last-day-of-month m y) y)))
    (calendar-for-loop date from first-date to last-date do
      (let* ((i-date (funcall fromabs date))
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
              (calendar-gregorian-from-absolute date) color))))))

;; Bahai, Islamic.
(defun calendar-mark-1 (month day year fromabs toabs &optional color)
  "Mark dates in the calendar conforming to MONTH DAY YEAR of some system.
The function FROMABS converts absolute dates to the appropriate date system.
The function TOABDS carries out the inverse operation.  Optional argument
COLOR is passed to `mark-visible-calendar-date' as MARK."
  (save-excursion
    (set-buffer calendar-buffer)
    (if (and (not (zerop month)) (not (zerop day)))
        (if (not (zerop year))
            ;; Fully specified date.
            (let ((date (calendar-gregorian-from-absolute
                         (funcall toabs (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (mark-visible-calendar-date date color)))
          ;; Month and day in any year--this taken from the holiday stuff.
          (let* ((i-date (funcall fromabs
                                  (calendar-absolute-from-gregorian
                                   (list displayed-month 15 displayed-year))))
                 (m (extract-calendar-month i-date))
                 (y (extract-calendar-year i-date))
                 date)
            (unless (< m 1)             ; calendar doesn't apply
              (increment-calendar-month m y (- 10 month))
              (if (> m 7)               ; date might be visible
                  (let ((date (calendar-gregorian-from-absolute
                               (funcall toabs (list month day y)))))
                    (if (calendar-date-is-visible-p date)
                        (mark-visible-calendar-date date color)))))))
      (calendar-mark-complex month day year
                             'calendar-bahai-from-absolute color))))


(defun diary-entry-time (s)
  "Return time at the beginning of the string S as a military-style integer.
For example, returns 1325 for 1:25pm.

Returns `diary-unknown-time' (default value -9999) if no time is recognized.
The recognized forms are XXXX, X:XX, or XX:XX (military time), and XXam,
XXAM, XXpm, XXPM, XX:XXam, XX:XXAM XX:XXpm, or XX:XXPM.  A period (.) can
be used instead of a colon (:) to separate the hour and minute parts."
  (let ((case-fold-search nil))
    (cond ((string-match                ; military time
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)[:.]?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)"
            s)
           (+ (* 100 (string-to-number (match-string 1 s)))
              (string-to-number (match-string 2 s))))
          ((string-match                ; hour only (XXam or XXpm)
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
           (+ (* 100 (% (string-to-number (match-string 1 s)) 12))
              (if (equal ?a (downcase (aref s (match-beginning 2))))
                  0 1200)))
          ((string-match        ; hour and minute (XX:XXam or XX:XXpm)
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)[:.]\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
           (+ (* 100 (% (string-to-number (match-string 1 s)) 12))
              (string-to-number (match-string 2 s))
              (if (equal ?a (downcase (aref s (match-beginning 3))))
                  0 1200)))
          (t diary-unknown-time))))     ; unrecognizable

(defun diary-entry-compare (e1 e2)
  "Return t if E1 is earlier than E2."
  (or (calendar-date-compare e1 e2)
      (and (calendar-date-equal (car e1) (car e2))
           (let* ((ts1 (cadr e1)) (t1 (diary-entry-time ts1))
                  (ts2 (cadr e2)) (t2 (diary-entry-time ts2)))
             (or (< t1 t2)
                 (and (= t1 t2)
                      (string-lessp ts1 ts2)))))))

(defun sort-diary-entries ()
  "Sort the list of diary entries by time of day."
  (setq diary-entries-list (sort diary-entries-list 'diary-entry-compare)))


(defun list-sexp-diary-entries (date)
  "Add sexp entries for DATE from the diary file to `diary-entries-list'.
Also, make them visible in the diary file.  Returns t if any entries were
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
                  `european-calendar-style' is nil (otherwise DAY, MONTH,
                  YEAR).  DAY, MONTH, and YEAR can be lists of integers,
                  `t' (meaning all values), or an integer.  An optional
                  parameter MARK specifies a face or single-character string
                  to use when highlighting the day in the calendar.

      %%(diary-float MONTH DAYNAME N &optional DAY MARK) text
                  Entry will appear on the Nth DAYNAME of MONTH.
                  (DAYNAME=0 means Sunday, 1 means Monday, and so on;
                  if N is negative it counts backward from the end of
                  the month.  MONTH can be a list of months, a single
                  month, or t to specify all months.  Optional DAY means
                  Nth DAYNAME of MONTH on or after/before DAY.  DAY defaults
                  to 1 if N>0 and the last day of the month if N<0.  An
                  optional parameter MARK specifies a face or single-character
                  string to use when highlighting the day in the calendar.

      %%(diary-block M1 D1 Y1 M2 D2 Y2 &optional MARK) text
                  Entry will appear on dates between M1/D1/Y1 and M2/D2/Y2,
                  inclusive.  (If `european-calendar-style' is non-nil, the
                  order of the parameters should be changed to D1, M1, Y1,
                  D2, M2, Y2.)  An optional parameter MARK specifies a face
                  or single-character string to use when highlighting the
                  day in the calendar.

      %%(diary-anniversary MONTH DAY YEAR &optional MARK) text
                  Entry will appear on anniversary dates of MONTH DAY, YEAR.
                  (If `european-calendar-style' is non-nil, the order of the
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
                  (If `european-calendar-style' is non-nil, the order of the
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
                  day before.  (If `european-calendar-style' is non-nil, the
                  parameter order should be changed to DAY, MONTH, YEAR.)

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
  (let ((s-entry (format "^%s?%s(" (regexp-quote diary-nonmarking-symbol)
                         (regexp-quote sexp-diary-entry-symbol)))
        entry-found file-glob-attrs marks)
    (goto-char (point-min))
    (save-excursion
      (setq file-glob-attrs (nth 1 (diary-pull-attrs nil '()))))
    (while (re-search-forward s-entry nil t)
      (backward-char 1)
      (let ((sexp-start (point))
            sexp entry specifier entry-start line-start)
        (forward-sexp)
        (setq sexp (buffer-substring-no-properties sexp-start (point))
              line-start (line-end-position 0)
              specifier
              (buffer-substring-no-properties (1+ line-start) (point))
              entry-start (1+ line-start))
        (forward-char 1)
        (if (and (bolp) (not (looking-at "[ \t]")))
            ;; Diary entry consists only of the sexp.
            (progn
              (backward-char 1)
              (setq entry ""))
          (setq entry-start (point))
          (forward-line 1)
          (while (looking-at "[ \t]")
            (forward-line 1))
          (backward-char 1)
          (setq entry (buffer-substring-no-properties entry-start (point))))
        (let ((diary-entry (diary-sexp-entry sexp entry date))
              temp literal)
          (setq literal entry           ; before evaluation
                entry (if (consp diary-entry)
                          (cdr diary-entry)
                        diary-entry))
          (when diary-entry
            (remove-overlays line-start (point) 'invisible 'diary)
            (if (< 0 (length entry))
                (setq temp (diary-pull-attrs entry file-glob-attrs)
                      entry (nth 0 temp)
                      marks (nth 1 temp))))
          (add-to-diary-list date
                             entry
                             specifier
                             (if entry-start (copy-marker entry-start))
                             marks
                             literal)
          (setq entry-found (or entry-found diary-entry)))))
    entry-found))


;;; Sexp diary functions.

(defvar date)
(defvar entry)

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-date (month day year &optional mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR if `european-calendar-style' is nil,
and DAY, MONTH, YEAR otherwise.  DAY, MONTH, and YEAR can be lists of
integers, `t' (meaning all values), or an integer.

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

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-block (m1 d1 y1 m2 d2 y2 &optional mark)
  "Block diary entry.
Entry applies if date is between, or on one of, two dates.
The order of the parameters is M1, D1, Y1, M2, D2, Y2 if
`european-calendar-style' is nil, and D1, M1, Y1, D2, M2, Y2 otherwise.

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

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-float (month dayname n &optional day mark)
  "Floating diary entry--entry applies if date is the nth dayname of month.
Parameters are MONTH, DAYNAME, N.  MONTH can be a list of months, an integer,
or `t' (meaning all months).  If N is negative, count backward from the end
of the month.

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
              ;; Last (n>0) or first (n<0) possible base date for entry.
              (limit
               (calendar-nth-named-absday (- n) dayname m y d))
              (last-abs (if (> n 0) limit (+ limit 6)))
              (first-abs (if (> n 0) (- limit 6) limit))
              (last (calendar-gregorian-from-absolute last-abs))
              (first (calendar-gregorian-from-absolute first-abs))
              ;; m1, d1 is first possible base date.
              (m1 (extract-calendar-month first))
              (d1 (extract-calendar-day first))
              (y1 (extract-calendar-year first))
              ;; m2, d2 is last possible base date.
              (m2 (extract-calendar-month last))
              (d2 (extract-calendar-day last))
              (y2 (extract-calendar-year last)))
         (if (or (and (= m1 m2) ; only possible base dates in one month
                      (or (eq month t)
                          (if (listp month)
                              (memq m1 month)
                            (= m1 month)))
                      (let ((d (or day (if (> n 0)
                                           1
                                         (calendar-last-day-of-month m1 y1)))))
                        (and (<= d1 d) (<= d d2))))
                 ;; Only possible base dates straddle two months.
                 (and (or (< y1 y2)
                          (and (= y1 y2) (< m1 m2)))
                      (or
                       ;; m1, d1 works as a base date.
                       (and
                        (or (eq month t)
                            (if (listp month)
                                (memq m1 month)
                              (= m1 month)))
                        (<= d1 (or day (if (> n 0)
                                           1
                                         (calendar-last-day-of-month m1 y1)))))
                       ;; m2, d2 works as a base date.
                       (and (or (eq month t)
                                (if (listp month)
                                    (memq m2 month)
                                  (= m2 month)))
                            (<= (or day (if (> n 0)
                                            1
                                          (calendar-last-day-of-month m2 y2)))
                                d2)))))
             (cons mark entry)))))

(defun diary-ordinal-suffix (n)
  "Ordinal suffix for N. (That is, `st', `nd', `rd', or `th', as appropriate.)"
  (if (or (memq (% n 100) '(11 12 13))
          (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-anniversary (month day &optional year mark)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR if
`european-calendar-style' is nil, and DAY, MONTH, YEAR otherwise.  The
diary entry can contain `%d' or `%d%s'; the %d will be replaced by the
number of years since the MONTH DAY, YEAR and the %s will be replaced by
the ordinal ending of that number (that is, `st', `nd', `rd' or `th', as
appropriate.  The anniversary of February 29 is considered to be March 1
in non-leap years.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((d (if european-calendar-style
                month
              day))
         (m (if european-calendar-style
                day
              month))
         (y (extract-calendar-year date))
         (diff (if year (- y year) 100)))
    (if (and (= m 2) (= d 29) (not (calendar-leap-year-p y)))
        (setq m 3
              d 1))
    (if (and (> diff 0) (calendar-date-equal (list m d y) date))
        (cons mark (format entry diff (diary-ordinal-suffix diff))))))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-cyclic (n month day year &optional mark)
  "Cycle diary entry--entry applies every N days starting at MONTH, DAY, YEAR.
If `european-calendar-style' is non-nil, parameters are N, DAY, MONTH, YEAR.
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

(defun diary-day-of-year ()
  "Day of year and number of days remaining in the year of date diary entry."
  (calendar-day-of-year-string date))

(defun diary-remind (sexp days &optional marking)
  "Provide a reminder of a diary entry.
SEXP is a diary-sexp.  DAYS is either a single number or a list of numbers
indicating the number(s) of days before the event that the warning(s) should
occur on.  If the current date is (one of) DAYS before the event indicated by
SEXP, then a suitable message (as specified by `diary-remind-message' is
returned.

In addition to the reminders beforehand, the diary entry also appears on the
date itself.

A `diary-nonmarking-symbol' at the beginning of the line of the `diary-remind'
entry specifies that the diary entry (not the reminder) is non-marking.
Marking of reminders is independent of whether the entry itself is a marking
or nonmarking; if optional parameter MARKING is non-nil then the reminders are
marked on the calendar."
  (let ((diary-entry (eval sexp)))
    (cond
     ;; Diary entry applies on date.
     ((and diary-entry
           (or (not marking-diary-entries) marking-diary-entry))
      diary-entry)
     ;; Diary entry may apply to `days' before date.
     ((and (integerp days)
           (not diary-entry)      ; diary entry does not apply to date
           (or (not marking-diary-entries) marking))
      (let ((date (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian date) days))))
        (when (setq diary-entry (eval sexp)) ; re-evaluate with adjusted date
          ;; Discard any mark portion from diary-anniversary, etc.
          (if (consp diary-entry) (setq diary-entry (cdr diary-entry)))
          (mapconcat 'eval diary-remind-message ""))))
     ;; Diary entry may apply to one of a list of days before date.
     ((and (listp days) days)
      (or (diary-remind sexp (car days) marking)
          (diary-remind sexp (cdr days) marking))))))


;;; Diary insertion functions.

;;;###cal-autoload
(defun make-diary-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to
`diary-file'."
  (let ((pop-up-frames (window-dedicated-p (selected-window))))
    (find-file-other-window (substitute-in-file-name (or file diary-file))))
  (when (eq major-mode default-major-mode) (diary-mode))
  (widen)
  (diary-unhide-everything)
  (goto-char (point-max))
  ;; FIXME cf hack-local-variables.
  (when (let ((case-fold-search t))
          (search-backward "Local Variables:"
                           (max (- (point-max) 3000) (point-min))
                           t))
    (beginning-of-line)
    (insert "\n")
    (forward-line -1))
  (insert
   (if (bolp) "" "\n")
   (if nonmarking diary-nonmarking-symbol "")
   string " "))

;;;###cal-autoload
(defun insert-diary-entry (arg)
  "Insert a diary entry for the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t t)
                    arg))

;;;###cal-autoload
(defun insert-weekly-diary-entry (arg)
  "Insert a weekly diary entry for the day of the week indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (make-diary-entry (calendar-day-name (calendar-cursor-to-date t))
                    arg))

;;;###cal-autoload
(defun insert-monthly-diary-entry (arg)
  "Insert a monthly diary entry for the day of the month indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " * ")
           '("* " day))))
    (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t)
                      arg)))

;;;###cal-autoload
(defun insert-yearly-diary-entry (arg)
  "Insert an annual diary entry for the day of the year indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " " monthname)
           '(monthname " " day))))
    (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t)
                      arg)))

;;;###cal-autoload
(defun insert-anniversary-diary-entry (arg)
  "Insert an anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking."
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

;;;###cal-autoload
(defun insert-block-diary-entry (arg)
  "Insert a block diary entry for the days between the point and marked date.
Prefix argument ARG makes the entry nonmarking."
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

;;;###cal-autoload
(defun insert-cyclic-diary-entry (arg)
  "Insert a cyclic diary entry starting at the date given by point.
Prefix argument ARG makes the entry nonmarking."
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

;;; Diary mode.

(defun diary-redraw-calendar ()
  "If `calendar-buffer' is live and diary entries are marked, redraw it."
  (and mark-diary-entries-in-calendar
       (save-excursion
         (redraw-calendar)))
  ;; Return value suitable for `write-contents-functions'.
  nil)

(defvar diary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'diary-show-all-entries)
    (define-key map "\C-c\C-q" 'quit-window)
    map)
  "Keymap for `diary-mode'.")

(defun diary-font-lock-sexps (limit)
  "Recognize sexp diary entry up to LIMIT for font-locking."
  (if (re-search-forward
       (format "^%s?\\(%s\\)" (regexp-quote diary-nonmarking-symbol)
               (regexp-quote sexp-diary-entry-symbol))
       limit t)
      (condition-case nil
          (save-restriction
            (narrow-to-region (point-min) limit)
            (let ((start (point)))
              (forward-sexp 1)
              (store-match-data (list start (point)))
              t))
        (error t))))

(defun diary-font-lock-date-forms (month-array &optional symbol abbrev-array)
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
    (mapcar (lambda (x)
              (cons
               (concat "^" (regexp-quote diary-nonmarking-symbol) "?"
                       (if symbol (regexp-quote symbol) "") "\\("
                       (mapconcat 'eval
                                  ;; If backup, omit first item (backup)
                                  ;; and last item (not part of date).
                                  (if (equal (car x) 'backup)
                                      (nreverse (cdr (reverse (cdr x))))
                                    x)
                                  "")
                       ;; With backup, last item is not part of date.
                       (if (equal (car x) 'backup)
                           (concat "\\)" (eval (car (reverse x))))
                         "\\)"))
               '(1 diary-face)))
            diary-date-forms)))

(defmacro diary-font-lock-keywords-1 (markfunc listfunc feature months symbol)
  "Subroutine of the function `diary-font-lock-keywords'.
If MARKFUNC is a member of `nongregorian-diary-marking-hook', or
LISTFUNC of `nongregorian-diary-listing-hook', then require FEATURE
and return a font-lock pattern matching array of MONTHS and marking SYMBOL."
  `(when (or (memq ',markfunc nongregorian-diary-marking-hook)
             (memq ',listfunc nongregorian-diary-listing-hook))
     (require ',feature)
     (diary-font-lock-date-forms ,months ,symbol)))

(defconst diary-time-regexp
  ;; Accepted formats: 10:00 10.00 10h00 10h 10am 10:00am 10.00am
  ;; Use of "." as a separator annoyingly matches numbers, eg "123.45".
  ;; Hence often prefix this with "\\(^\\|\\s-\\)."
  (concat "[0-9]?[0-9]\\([AaPp][mM]\\|\\("
          "[Hh]\\([0-9][0-9]\\)?\\|[:.][0-9][0-9]"
          "\\)\\([AaPp][Mm]\\)?\\)")
  "Regular expression matching a time of day.")

(defvar calendar-hebrew-month-name-array-leap-year)
(defvar calendar-islamic-month-name-array)
(defvar calendar-bahai-month-name-array)

;;;###cal-autoload
(defun diary-font-lock-keywords ()
  "Return a value for the variable `diary-font-lock-keywords'."
  (append
   (diary-font-lock-date-forms calendar-month-name-array
                               nil calendar-month-abbrev-array)
   (diary-font-lock-keywords-1 mark-hebrew-diary-entries
                               list-hebrew-diary-entries
                               cal-hebrew
                               calendar-hebrew-month-name-array-leap-year
                               hebrew-diary-entry-symbol)
   (diary-font-lock-keywords-1 mark-islamic-diary-entries
                               list-islamic-diary-entries
                               cal-islam
                               calendar-islamic-month-name-array
                               islamic-diary-entry-symbol)
   (diary-font-lock-keywords-1 diary-bahai-mark-entries
                               diary-bahai-list-entries
                               cal-bahai
                               calendar-bahai-month-name-array
                               bahai-diary-entry-symbol)
   (list
    (cons
     (format "^%s.*$" (regexp-quote diary-include-string))
     'font-lock-keyword-face)
    (cons
     (format "^%s?\\(%s\\)" (regexp-quote diary-nonmarking-symbol)
             (regexp-quote sexp-diary-entry-symbol))
     '(1 font-lock-reference-face))
    (cons
     (format "^%s" (regexp-quote diary-nonmarking-symbol))
     'font-lock-reference-face)
    (cons
     (format "^%s?%s" (regexp-quote diary-nonmarking-symbol)
             (regexp-opt (mapcar 'regexp-quote
                                 (list hebrew-diary-entry-symbol
                                       islamic-diary-entry-symbol
                                       bahai-diary-entry-symbol))
                         t))
     '(1 font-lock-reference-face))
    '(diary-font-lock-sexps . font-lock-keyword-face)
    `(,(format "\\(^\\|\\s-\\)%s\\(-%s\\)?" diary-time-regexp
               diary-time-regexp)
      . 'diary-time))))

(defvar diary-font-lock-keywords (diary-font-lock-keywords)
  "Forms to highlight in `diary-mode'.")

;;;###autoload
(define-derived-mode diary-mode fundamental-mode "Diary"
  "Major mode for editing the diary file."
  (set (make-local-variable 'font-lock-defaults)
       '(diary-font-lock-keywords t))
  (add-to-invisibility-spec '(diary . nil))
  (add-hook 'after-save-hook 'diary-redraw-calendar nil t)
  (if diary-header-line-flag
      (setq header-line-format diary-header-line-format)))


;;; Fancy Diary Mode.

(defvar diary-fancy-date-pattern
  (concat
   (let ((dayname (diary-name-pattern calendar-day-name-array nil t))
         (monthname (diary-name-pattern calendar-month-name-array nil t))
         (day "[0-9]+")
         (month "[0-9]+")
         (year "-?[0-9]+"))
     (mapconcat 'eval calendar-date-display-form ""))
   ;; Optional ": holiday name" after the date.
   "\\(: .*\\)?")
  "Regular expression matching a date header in Fancy Diary.")

(defvar fancy-diary-font-lock-keywords
  (list
   (list
    ;; Any number of " other holiday name" lines, followed by "==" line.
    (concat diary-fancy-date-pattern "\\(\n +.*\\)*\n=+$")
    '(0 (progn (put-text-property (match-beginning 0) (match-end 0)
                                  'font-lock-multiline t)
               diary-face)))
   '("^.*\\([aA]nniversary\\|[bB]irthday\\).*$" . 'diary-anniversary)
   '("^.*Yahrzeit.*$" . font-lock-reference-face)
   '("^\\(Erev \\)?Rosh Hodesh.*" . font-lock-function-name-face)
   '("^Day.*omer.*$" . font-lock-builtin-face)
   '("^Parashat.*$" . font-lock-comment-face)
   `(,(format "\\(^\\|\\s-\\)%s\\(-%s\\)?" diary-time-regexp
              diary-time-regexp) . 'diary-time))
  "Keywords to highlight in fancy diary display.")

;; If region looks like it might start or end in the middle of a
;; multiline pattern, extend the region to encompass the whole pattern.
(defun diary-fancy-font-lock-fontify-region-function (beg end &optional verbose)
  "Function to use for `font-lock-fontify-region-function' in Fancy Diary.
Needed to handle multiline keyword in `fancy-diary-font-lock-keywords'.
Fontify the region between BEG and END, quietly unless VERBOSE is non-nil."
  (goto-char beg)
  (forward-line 0)
  (if (looking-at "=+$") (forward-line -1))
  (while (and (looking-at " +[^ ]")
              (zerop (forward-line -1))))
  ;; This check not essential.
  (if (looking-at diary-fancy-date-pattern)
      (setq beg (line-beginning-position)))
  (goto-char end)
  (forward-line 0)
  (while (and (looking-at " +[^ ]")
              (zerop (forward-line 1))))
  (if (looking-at "=+$")
      (setq end (line-beginning-position 2)))
  (font-lock-default-fontify-region beg end verbose))

(define-derived-mode fancy-diary-display-mode fundamental-mode
  "Diary"
  "Major mode used while displaying diary entries using Fancy Display."
  (set (make-local-variable 'font-lock-defaults)
       '(fancy-diary-font-lock-keywords
         t nil nil nil
         (font-lock-fontify-region-function
          . diary-fancy-font-lock-fontify-region-function)))
  (local-set-key "q" 'quit-window))


;; Following code from Dave Love <fx@gnu.org>.
;; Import Outlook-format appointments from mail messages in Gnus or
;; Rmail using command `diary-from-outlook'.  This, or the specialized
;; functions `diary-from-outlook-gnus' and `diary-from-outlook-rmail',
;; could be run from hooks to notice appointments automatically (in
;; which case they will prompt about adding to the diary).  The
;; message formats recognized are customizable through
;; `diary-outlook-formats'.

(defvar subject)                        ; bound in diary-from-outlook-gnus

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

(provide 'diary-lib)

;; arch-tag: 22dd506e-2e33-410d-9ae1-095a0c1b2010
;;; diary-lib.el ends here
