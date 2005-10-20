;;; org.el --- Outline-based notes management and organizer
;; Carstens outline-mode for keeping track of everything.
;; Copyright (c) 2004, 2005 Free Software Foundation
;;
;; Author: Carsten Dominik <dominik at science dot uva dot nl>
;; Keywords: outlines, hypermedia, calendar
;; Homepage: http://www.astro.uva.nl/~dominik/Tools/org/
;; Version: 3.17
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Org-mode is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org-mode develops organizational tasks around a NOTES file that contains
;; information about projects as plain text.  Org-mode is implemented on top
;; of outline-mode - ideal to keep the content of large files well structured.
;; It supports ToDo items, deadlines and time stamps, which can be extracted
;; to create a daily/weekly agenda that also integrates the diary of the Emacs
;; calendar.  Tables are easily created with a built-in table editor.  Plain
;; text URL-like links connect to websites, emails (VM, RMAIL, WANDERLUST),
;; Usenet messages (Gnus), BBDB entries, and any files related to the
;; projects.  For printing and sharing of notes, an Org-mode file (or a part
;; of it) can be exported as a structured ASCII file, or as HTML.
;;
;; Installation
;; ------------
;; If Org-mode is part of the Emacs distribution or an XEmacs package, you
;; only need to copy the following lines to your .emacs file.  The last two
;; lines define *global* keys for the commands `org-store-link' and
;; `org-agenda' - please choose suitable keys yourself.
;;
;;    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;    (define-key global-map "\C-cl" 'org-store-link)
;;    (define-key global-map "\C-ca" 'org-agenda)
;;
;; If you have downloaded Org-mode from the Web, you must byte-compile
;; org.el and put it on your load path.  In addition to the Emacs Lisp
;; lines above, you also need to add the following lines to .emacs:
;;
;;    (autoload 'org-mode "org" "Org mode" t)
;;    (autoload 'org-diary "org" "Diary entries from Org mode")
;;    (autoload 'org-agenda "org" "Multi-file agenda from Org mode" t)
;;    (autoload 'org-store-link "org" "Store a link to the current location" t)
;;    (autoload 'orgtbl-mode "org" "Org tables as a minor mode" t)
;;    (autoload 'turn-on-orgtbl "org" "Org tables as a minor mode")
;;
;; This setup will put all files with extension ".org" into Org-mode.  As
;; an alternative, make the first line of a file look like this:
;;
;;     MY PROJECTS    -*- mode: org; -*-
;;
;; which will select Org-mode for this buffer no matter what the file's
;; name is.
;;
;; Documentation
;; -------------
;; The documentation of Org-mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the homepage of
;; Org-mode, you can read the same text online as HTML.  There is also an
;; excellent reference card made by Philip Rooke.
;;
;; Changes:
;; -------
;; Version 3.17
;;    - HTML export specifies character set depending on coding-system.
;;
;; Version 3.16
;;    - In tables, directly after the field motion commands like TAB and RET,
;;      typing a character will blank the field.  Can be turned off with
;;      variable `org-table-auto-blank-field'.
;;    - Inactive timestamps with `C-c !'.  These do not trigger the agenda
;;      and are not linked to the calendar.
;;    - Additional key bindings to allow Org-mode to function on a tty emacs.
;;    - `C-c C-h' prefix key replaced by `C-c C-x', and `C-c C-x C-h' replaced
;;      by `C-c C-x b' (b=Browser).  This was necessary to recover the
;;      standard meaning of C-h after a prefix key (show prefix bindings).
;;
;; Version 3.15
;;    - QUOTE keyword at the beginning of an entry causes fixed-width export
;;      of unmodified entry text. `C-c :' toggles this keyword.
;;    - New face `org-special-keyword' which is used for COMMENT, QUOTE,
;;      DEADLINE and SCHEDULED, and priority cookies.  Default is only a weak
;;      color, to reduce the amount of aggressive color in the buffer.
;;
;; Version 3.14
;;    - Formulas for individual fields in table.
;;    - Automatic recalculation in calculating tables.
;;    - Named fields and columns in tables.
;;    - Fixed bug with calling `org-archive' several times in a row.
;;
;; Version 3.13
;;    - Efficiency improvements:  Fewer table re-alignments needed.
;;    - New special lines in tables, for defining names for individual cells.
;;
;; Version 3.12
;;    - Tables can store formulas (one per column) and compute fields.
;;      Not quite like a full spreadsheet, but very powerful.
;;    - table.el keybinding is now `C-c ~'.
;;    - Numeric argument to org-cycle does `show-subtree' above on level ARG.
;;    - Small changes to keys in agenda buffer.  Affected keys:
;;      [w] weekly view; [d] daily view; [D] toggle diary inclusion.
;;    - Bug fixes.
;;
;; Version 3.11
;;    - Links inserted with C-c C-l are now by default enclosed in angle
;;      brackets.  See the new variable `org-link-format'.
;;    - ">" terminates a link, this is a way to have several links in a line.
;;      Both "<" and ">" are no longer allowed as characters in a link.
;;    - Archiving of finished tasks.
;;    - C-<up>/<down> bindings removed, to allow access to paragraph commands.
;;    - Compatibility with CUA-mode (see variable `org-CUA-compatible').
;;    - Compatibility problems with viper-mode fixed.
;;    - Improved html export of tables.
;;    - Various clean-up changes.
;;
;; Version 3.10
;;    - Using `define-derived-mode' to derive `org-mode' from `outline-mode'.
;;
;; Version 3.09
;;    - Time-of-day specifications in agenda are extracted and placed
;;      into the prefix.  Timed entries can be placed into a time grid for
;;      day.
;;
;; Version 3.08
;;    - "|" no longer allowed as part of a link, to allow links in tables.
;;    - The prefix of items in the agenda buffer can be configured.
;;    - Cleanup.
;;
;; Version 3.07
;;    - Some folding incinsistencies removed.
;;    - BBDB links to company-only entries.
;;    - Bug fixes and global cleanup.
;;
;; Version 3.06
;;    - M-S-RET inserts a new TODO heading.
;;    - New startup option `content'.
;;    - Better visual response when TODO items in agenda change status.
;;    - Window positioning after visibility state changes optimized and made
;;      configurable.  See `org-cycle-hook' and `org-occur-hook'.
;;
;; Version 3.05
;;    - Agenda entries from the diary are linked to the diary file, so
;;      adding and editing diary entries can be done directly from the agenda.
;;    - Many calendar/diary commands available directly from agenda.
;;    - Field copying in tables with S-RET does increment.
;;    - C-c C-x C-v extracts the visible part of the buffer for printing.
;;    - Moving subtrees up and down preserves the whitespace at the tree end.
;;
;; Version 3.04
;;    - Table editor optimized to need fewer realignments, and to keep
;;      table shape when typing in fields.
;;    - A new minor mode, orgtbl-mode, introduces the Org-mode table editor
;;      into arbitrary major modes.
;;    - Fixed bug with realignment in XEmacs.
;;    - Startup options can be set with special #+STARTUP line.
;;    - Heading following a match in org-occur can be suppressed.
;;
;; Version 3.03
;;    - Copyright transfer to the FSF.
;;    - Effect of C-u and C-u C-u in org-timeline swapped.
;;    - Timeline now always contains today, and `.' jumps to it.
;;    - Table editor:
;;      - cut and paste of rectangular regions in tables
;;      - command to convert org-mode table to table.el table and back
;;      - command to treat several cells like a paragraph and fill it
;;      - command to convert a buffer region to a table
;;      - import/export tables as tab-separated files (exchange with Excel)
;;    - Agenda:
;;      - Sorting mechanism for agenda items rewritten from scratch.
;;      - Sorting fully configurable.
;;      - Entries specifying a time are sorted together.
;;    - Completion also covers option keywords after `#-'.
;;    - Bug fixes.
;;
;; Version 3.01
;;    - New reference card, thanks to Philip Rooke for creating it.
;;    - Single file agenda renamed to "Timeline".  It no longer shows
;;      warnings about upcoming deadlines/overdue scheduled items.
;;      That functionality is now limited to the (multifile) agenda.
;;    - When reading a date, the calendar can be manipulated with keys.
;;    - Link support for RMAIL and Wanderlust (from planner.el, untested).
;;    - Minor bug fixes and documentation improvements.

;;; Code:

(eval-when-compile (require 'cl) (require 'calendar))
(require 'outline)
(require 'time-date)
(require 'easymenu)

(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(defvar font-lock-unfontify-region-function)

;;; Customization variables

(defvar org-version "3.17"
  "The version number of the file org.el.")
(defun org-version ()
  (interactive)
  (message "Org-mode version %s" org-version))

;; The following two constants are for compatibility with different Emacs
;; versions (Emacs versus XEmacs) and with different versions of outline.el.
;; The compatibility code in org.el is based on these two constants.
(defconst org-xemacs-p (featurep 'xemacs)
  "Are we running xemacs?")
(defconst org-noutline-p (featurep 'noutline)
  "Are we using the new outline mode?")

(defgroup org nil
  "Outline-based notes management and organizer "
  :tag "Org"
  :group 'outlines
  :group 'hypermedia
  :group 'calendar)

(defgroup org-startup nil
  "Options concerning startup of Org-mode."
  :tag "Org Startup"
  :group 'org)

(defcustom org-CUA-compatible nil
  "Non-nil means use alternative key bindings for S-<cursor movement>.
Org-mode used S-<cursor movement> for changing timestamps and priorities.
S-<cursor movement> is also used for example by `CUA-mode' to select text.
If you want to use Org-mode together with `CUA-mode', Org-mode needs to use
alternative bindings.  Setting this variable to t will replace the following
keys both in Org-mode and in the Org-agenda buffer.

S-RET   -> C-S-RET
S-up    -> M-p
S-down  -> M-n
S-left  -> M--
S-right -> M-+

If you do not like the alternative keys, take a look at the variable
`org-disputed-keys'.

This option is only relevant at load-time of Org-mode.  Changing it requires
a restart of Emacs to become effective."
  :group 'org-startup
  :type 'boolean)

(defvar org-disputed-keys
  '((S-up     [(shift up)]      [(meta ?p)])
    (S-down   [(shift down)]    [(meta ?n)])
    (S-left   [(shift left)]    [(meta ?-)])
    (S-right  [(shift right)]   [(meta ?+)])
    (S-return [(shift return)]  [(control shift return)]))
  "Keys for which Org-mode and other modes compete.
This is an alist, cars are symbols for lookup, 1st element is the default key,
second element will be used when `org-CUA-compatible' is t.")

(defun org-key (key)
  "Select a key according to `org-CUA-compatible'."
  (nth (if org-CUA-compatible 2 1)
       (or (assq key org-disputed-keys)
	   (error "Invalid Key %s in `org-key'" key))))

(defcustom org-startup-folded t
  "Non-nil means, entering Org-mode will switch to OVERVIEW.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: fold
   #+STARTUP: nofold
   #+STARTUP: content"
  :group 'org-startup
  :type '(choice
	  (const :tag "nofold: show all" nil)
	  (const :tag "fold: overview" t)
	  (const :tag "content: all headlines" content)))

(defcustom org-startup-truncated t
  "Non-nil means, entering Org-mode will set `truncate-lines'.
This is useful since some lines containing links can be very long and
uninteresting.  Also tables look terrible when wrapped."
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-with-deadline-check nil
  "Non-nil means, entering Org-mode will run the deadline check.
This means, if you start editing an org file, you will get an
immediate reminder of any due deadlines.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: dlcheck
   #+STARTUP: nodlcheck"
  :group 'org-startup
  :type 'boolean)

(defcustom org-insert-mode-line-in-empty-file nil
  "Non-nil means insert the first line setting Org-mode in empty files.
When the function `org-mode' is called interactively in an empty file, this
normally means that the file name does not automatically trigger Org-mode.
To ensure that the file will always be in Org-mode in the future, a
line enforcing Org-mode will be inserted into the buffer, if this option
has been set."
  :group 'org-startup
  :type 'boolean)

(defgroup org-keywords nil
  "Options concerning TODO items in Org-mode."
  :tag "Org Keywords"
  :group 'org)

(defcustom org-todo-keywords '("TODO" "DONE")
  "List of TODO entry keywords.
\\<org-mode-map>By default, this is '(\"TODO\" \"DONE\").  The last entry in the list is
considered to mean that the entry is \"done\".  All the other mean that
action is required, and will make the entry show up in todo lists, diaries
etc.
The command \\[org-todo] cycles an entry through these states, and an
additional state where no keyword is present.  For details about this
cycling, see also the variable `org-todo-interpretation'
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type '(repeat (string :tag "Keyword")))

(defcustom org-todo-interpretation 'sequence
  "Controls how TODO keywords are interpreted.
\\<org-mode-map>Possible values are `sequence' and `type'.
This variable is only relevant if `org-todo-keywords' contains more than two
states.  There are two ways how these keywords can be used:

- As a sequence in the process of working on a TODO item, for example
  (setq org-todo-keywords '(\"TODO\" \"STARTED\" \"VERIFY\" \"DONE\")
	org-todo-interpretation 'sequence)

- As different types of TODO items, for example
  (setq org-todo-keywords '(\"URGENT\" \"RELAXED\" \"REMIND\" \"FOR_TOM\" \"DONE\")
	org-todo-interpretation 'type)

When the states are interpreted as a sequence, \\[org-todo] always cycles
to the next state, in order to walk through all different states.  So with
\\[org-todo], you turn an empty entry into the state TODO.  When you started
working on the item, you use \\[org-todo] again to switch it to \"STARTED\",
later to VERIFY and finally to DONE.

When the states are interpreted as types, \\[org-todo] still cycles through
when it is called several times in direct succession, in order to initially
select the type.  However, if not called immediately after a previous
\\[org-todo], it switches from each type directly to DONE.  So with the
above example, you could use `\\[org-todo] \\[org-todo]' to label an entry
RELAXED.  If you later return to this entry and press \\[org-todo] again,
RELAXED will not be changed REMIND, but directly to DONE.

You can create a large number of types.  To initially select a
type, it is then best to use \\[universal-argument] \\[org-todo] in order to specify the
type with completion.  Of course, you can also type the keyword
directly into the buffer.  M-TAB completes TODO keywords at the
beginning of a headline."
  :group 'org-keywords
  :type '(choice (const sequence)
		 (const type)))

(defcustom org-default-priority ?B
  "The default priority of TODO items.
This is the priority an item get if no explicit priority is given."
  :group 'org-keywords
  :type 'character)

(defcustom org-lowest-priority ?C
  "The lowest priority of TODO items.  A character like ?A, ?B etc."
  :group 'org-keywords
  :type 'character)

(defcustom org-deadline-string "DEADLINE:"
  "String to mark deadline entries.
A deadline is this string, followed by a time stamp.  Should be a word,
terminated by a colon.  You can insert a schedule keyword and
a timestamp with \\[org-deadline].
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type 'string)

(defcustom org-scheduled-string "SCHEDULED:"
  "String to mark scheduled TODO entries.
A schedule is this string, followed by a time stamp.  Should be a word,
terminated by a colon.  You can insert a schedule keyword and
a timestamp with \\[org-schedule].
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type 'string)

(defcustom org-comment-string "COMMENT"
  "Entries starting with this keyword will never be exported.
An entry can be toggled between COMMENT and normal with
\\[org-toggle-comment].
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type 'string)

(defcustom org-quote-string "QUOTE"
  "Entries starting with this keyword will be exported in fixed-width font.
Quoting applies only to the text in the entry following the headline, and does
not extend beyond the next headline, even if that is lower level.
An entry can be toggled between QUOTE and normal with
\\[org-toggle-fixed-width-section]"
  :group 'org-keywords
  :type 'string)

(defcustom org-after-todo-state-change-hook nil
  "Hook which is run after the state of a TODO item was changed.
The new state (a string with a todo keyword, or nil) is available in the
Lisp variable `state'."
  :group 'org-keywords
  :type 'hook)

;; Variables for pre-computed regular expressions, all buffer local
(defvar org-todo-kwd-priority-p nil
  "Do TODO items have priorities?")
(make-variable-buffer-local 'org-todo-kwd-priority-p)
(defvar org-todo-kwd-max-priority nil
  "Maximum priority of TODO items.")
(make-variable-buffer-local 'org-todo-kwd-max-priority)
(defvar org-ds-keyword-length 12
  "Maximum length of the Deadline and SCHEDULED keywords.")
(make-variable-buffer-local 'org-ds-keyword-length)
(defvar org-done-string nil
  "The last string in `org-todo-keywords', indicating an item is DONE.")
(make-variable-buffer-local 'org-done-string)
(defvar org-todo-regexp nil
  "Matches any of the TODO state keywords.")
(make-variable-buffer-local 'org-todo-regexp)
(defvar org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.")
(make-variable-buffer-local 'org-not-done-regexp)
(defvar org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.")
(make-variable-buffer-local 'org-todo-line-regexp)
(defvar org-nl-done-regexp nil
  "Matches newline followed by a headline with the DONE keyword.")
(make-variable-buffer-local 'org-nl-done-regexp)
(defvar org-looking-at-done-regexp nil
  "Matches the DONE keyword a point.")
(make-variable-buffer-local 'org-looking-at-done-regexp)
(defvar org-deadline-regexp nil
  "Matches the DEADLINE keyword.")
(make-variable-buffer-local 'org-deadline-regexp)
(defvar org-deadline-time-regexp nil
  "Matches the DEADLINE keyword together with a time stamp.")
(make-variable-buffer-local 'org-deadline-time-regexp)
(defvar org-deadline-line-regexp nil
  "Matches the DEADLINE keyword and the rest of the line.")
(make-variable-buffer-local 'org-deadline-line-regexp)
(defvar org-scheduled-regexp nil
  "Matches the SCHEDULED keyword.")
(make-variable-buffer-local 'org-scheduled-regexp)
(defvar org-scheduled-time-regexp nil
  "Matches the SCHEDULED keyword together with a time stamp.")
(make-variable-buffer-local 'org-scheduled-time-regexp)

(defvar org-category nil
  "Variable used by org files to set a category for agenda display.
Such files should use a file variable to set it, for example

   -*- mode: org; org-category: \"ELisp\"

or contain a special line

#+CATEGORY: ELisp

If the file does not specify a category, then file's base name
is used instead.")

(defgroup org-time nil
  "Options concerning time stamps and deadlines in Org-mode."
  :tag "Org Time"
  :group 'org)

(defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
  "Formats for `format-time-string' which are used for time stamps.
It is not recommended to change this constant.")


(defcustom org-deadline-warning-days 30
  "No. of days before expiration during which a deadline becomes active.
This variable governs the display in the org file."
  :group 'org-time
  :type 'number)

(defcustom org-popup-calendar-for-date-prompt t
  "Non-nil means, pop up a calendar when prompting for a date.
In the calendar, the date can be selected with mouse-1.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'number)

(defcustom org-calendar-follow-timestamp-change t
  "Non-nil means, make the calendar window follow timestamp changes.
When a timestamp is modified and the calendar window is visible, it will be
moved to the new date."
  :group 'org-time
  :type 'boolean)

(defgroup org-agenda nil
  "Options concerning agenda display Org-mode."
  :tag "Org Agenda"
  :group 'org)

(defcustom org-agenda-files nil
  "A list of org files for agenda/diary display.
Entries are added to this list with \\[org-add-file] and removed with
\\[org-remove-file].  You can also use customize to edit the list."
  :group 'org-agenda
  :type '(repeat file))

(defcustom org-select-timeline-window t
  "Non-nil means, after creating a timeline, move cursor into Timeline window.
When nil, cursor will remain in the current window."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-select-agenda-window t
  "Non-nil means, after creating an agenda, move cursor into Agenda window.
When nil, cursor will remain in the current window."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-fit-agenda-window t
  "Non-nil means, change window size of agenda to fit content."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-show-all-dates t
  "Non-nil means, `org-agenda' shows every day in the selected range.
When nil, only the days which actually have entries are shown."
  :group 'org-agenda
  :type 'boolean)

;; FIXME: First day of month works only for current month because it would
;; require a variable ndays treatment.
(defcustom org-agenda-start-on-weekday 1
  "Non-nil means, start the overview always on the specified weekday.
0 Denotes Sunday, 1 denotes Monday etc.
When nil, always start on the current day."
  :group 'org-agenda
  :type '(choice (const :tag "Today" nil)
		 (const :tag "First day of month" t)
		 (number :tag "Weekday No.")))

(defcustom org-agenda-ndays 7
  "Number of days to include in overview display."
  :group 'org-agenda
  :type 'number)

(defcustom org-agenda-include-all-todo t
  "Non-nil means, the agenda will always contain all TODO entries.
When nil, date-less entries will only be shown if `org-agenda' is called
with a prefix argument.
When non-nil, the TODO entries will be listed at the top of the agenda, before
the entries for specific days."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-include-diary nil
  "If non-nil, include in the agenda entries from the Emacs Calendar's diary."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-calendar-to-agenda-key [?c]
  "The key to be installed in `calendar-mode-map' for switching to the agenda.
The command `org-calendar-goto-agenda' will be bound to this key.  The
default is the character `c' because then`c' can be used to switch back and
force between agenda and calendar."
  :group 'org-agenda
  :type 'sexp)

(defcustom org-agenda-sorting-strategy '(time-up category-keep priority-down)
  "Sorting structure for the agenda items of a single day.
This is a list of symbols which will be used in sequence to determine
if an entry should be listed before another entry.  The following
symbols are recognized.

time-up         Put entries with time-of-day indications first, early first
time-down       Put entries with time-of-day indications first, late first
category-keep   Keep the default order of categories, corresponding to the
		sequence in `org-agenda-files'.
category-up     Sort alphabetically by category, A-Z.
category-down   Sort alphabetically by category, Z-A.
priority-up     Sort numerically by priority, high priority last.
priority-down   Sort numerically by priority, high priority first.

The different possibilities will be tried in sequence, and testing stops
if one comparison returns a \"not-equal\".  For example, the default
    '(time-up category-keep priority-down)
means: Pull out all entries having a specified time of day and sort them,
in order to make a time schedule for the current day the first thing in the
agenda listing for the day.  Of the entries without a time indication, keep
the grouped in categories, don't sort the categories, but keep them in
the sequence given in `org-agenda-files'.  Within each category sort by
priority.

Leaving out `category-keep' would mean that items will be sorted across
categories by priority."
  :group 'org-agenda
  :type '(repeat
	  (choice
	   (const time-up)
	   (const time-down)
	   (const category-keep)
	   (const category-up)
	   (const category-down)
	   (const priority-up)
	   (const priority-down))))

(defcustom org-agenda-prefix-format "  %-12:c%?-12t% s"
  "Format specification for the prefix of items in the agenda buffer.
This format works similar to a printf format, with the following meaning:

  %c   the category of the item, \"Diary\" for entries from the diary, or
       as given by the CATEGORY keyword or derived from the file name.
  %t   the time-of-day specification if one applies to the entry, in the
       format HH:MM
  %s   Scheduling/Deadline information, a short string

All specifiers work basically like the standard `%s' of printf, but may
contain two additional characters:  A question mark just after the `%' and
a whitespace/punctuation character just before the final letter.

If the first character after `%' is a question mark, the entire field
will only be included if the corresponding value applies to the
current entry.  This is useful for fields which should have fixed
width when present, but zero width when absent.  For example,
\"%?-12t\" will result in a 12 character time field if a time of the
day is specified, but will completely disappear in entries which do
not contain a time.

If there is punctuation or whitespace character just before the final
format letter, this character will be appended to the field value if
the value is not empty.  For example, the format \"%-12:c\" leads to
\"Diary: \" if the category is \"Diary\".  If the category were be
empty, no additional colon would be interted.

The default value of this option is \"  %-12:c%?-12t% s\", meaning:
- Indent the line with two space characters
- Give the category in a 12 chars wide field, padded with whitespace on
  the right (because of `-').  Append a colon if there is a category
  (because of `:').
- If there is a time-of-day, put it into a 12 chars wide field.  If no
  time, don't put in an empty field, just skip it (because of '?').
- Finally, put the scheduling information and append a whitespace.

As another example, if you don't want the time-of-day of entries in
the prefix, you could use:

  (setq org-agenda-prefix-format \"  %-11:c% s\")

See also the variable `org-agenda-remove-times-when-in-prefix'."
  :type 'string
  :group 'org-agenda)

(defcustom org-timeline-prefix-format "  % s"
  "Like `org-agenda-prefix-format', but for the timeline of a single file."
  :type 'string
  :group 'org-agenda)

(defvar org-prefix-format-compiled nil
  "The compiled version of the most recently used prefix format.
Depending on which command was used last, this may be the compiled version
of `org-agenda-prefix-format' or `org-timeline-prefix-format'.")

(defcustom org-agenda-use-time-grid t
  "Non-nil means, show a time grid in the agenda schedule.
A time grid is a set of lines for specific times (like every two hours between
8:00 and 20:00.  The items scheduled for a day at specific times are
sorted in between these lines.
For deails about when the grid will be shown, and what it will look like, see
the variable `org-agenda-time-grid'."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1000 1200 1400 1600 1800 2000))

  "The settings for time grid for agenda display.
This is a list of three items.  The first item is again a list.  It contains
symbols specifying conditions when the grid should be displayed:

 daily         if the agenda shows a single day
 weekly        if the agenda shows an entire week
 today         show grid on current date, independent of daily/weekly display
 require-timed show grid only if at least on item has a time specification

The second item is a string which will be places behing the grid time.

The third item is a list of integers, indicating the times that should have
a grid line."
  :group 'org-agenda
  :type
  '(list
    (set :greedy t :tag "Grid Display Options"
	 (const :tag "Show grid in single day agenda display" daily)
	 (const :tag "Show grid in weekly agenda display" weekly)
	 (const :tag "Always show grid for today" today)
	 (const :tag "Show grid only if any timed entries are present"
		require-timed)
	 (const :tag "Skip grid times already present in an entry"
		remove-match))
    (string :tag "Grid String")
    (repeat :tag "Grid Times" (integer :tag "Time"))))

(defcustom org-agenda-remove-times-when-in-prefix t
  "Non-nil means, remove duplicate time specifications in agenda items.
When the format `org-agenda-prefix-format' contains a `%t' specifier, a
time-of-day specification in a headline or diary entry is extracted and
placed into the prefix.  If this option is non-nil, the original specification
\(a timestamp or -range, or just a plain time(range) specification like
11:30-4pm) will be removed for agenda display.  This makes the agenda less
cluttered.
The option can be t or nil.  It may also be the symbol `beg', indicating
that the time should only be removed what it is located at the beginning of
the headline/diary entry."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When at beginning of entry" beg)))

(defcustom org-sort-agenda-notime-is-late t
  "Non-nil means, items without time are considered late.
This is only relevant for sorting.  When t, items which have no explicit
time like 15:30 will be considered as 24:01, i.e. later than any items which
do have a time.  When nil, the default time is before 0:00.  You can use this
option to decide if the schedule for today should come before or after timeless
agenda entries."
  :group 'org-agenda
  :type 'boolean)

(defgroup org-structure nil
  "Options concerning structure editing in Org-mode."
  :tag "Org Structure"
  :group 'org)

(defcustom org-cycle-hook '(org-optimize-window-after-visibility-change)
  "Hook that is run after `org-cycle' has changed the buffer visibility.
The function(s) in this hook must accept a single argument which indicates
the new state that was set by the most recent `org-cycle' command.  The
argument is a symbol.  After a global state change, it can have the values
`overview', `content', or `all'.  After a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-structure
  :type 'hook)

(defcustom org-occur-hook '(org-first-headline-recenter)
  "Hook that is run after `org-occur' has constructed a sparse tree.
This can be used to recenter the window to show as much of the structure
as possible."
  :group 'org-structure
  :type 'hook)

(defcustom org-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
After changing this, requires restart of Emacs to become effective."
  :group 'org-structure
  :type 'boolean)

(defcustom org-adapt-indentation t
  "Non-nil means, adapt indentation when promoting and demoting.
When this is set and the *entire* text in an entry is indented, the
indentation is increased by one space in a demotion command, and
decreased by one in a promotion command.  If any line in the entry
body starts at column 0, indentation is not changed at all."
  :group 'org-structure
  :type 'boolean)

(defcustom org-enable-fixed-width-editor t
  "Non-nil means, lines starting with \":\" are treated as fixed-width.
This currently only means, they are never auto-wrapped.
When nil, such lines will be treated like ordinary lines.
See also the QUOTE keyword."
  :group 'org-structure
  :type 'boolean)

(defcustom org-cycle-emulate-tab t
  "Where should `org-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'org-structure
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Everywhere except in headlines" t)
		 ))

(defcustom org-show-following-heading t
  "Non-nil means, show heading following match in `org-occur'.
When doing an `org-occur' it is useful to show the headline which
follows the match, even if they do not match the regexp.  This makes it
easier to edit directly inside the sparse tree.  However, if you use
org-occur mainly as an overview, the following headlines are
unnecessary clutter."
  :group 'org-structure
  :type 'boolean)

(defcustom org-archive-location "%s_archive::"
  "The location where subtrees should be archived.
This string consists of two parts, separated by a double-colon.

The first part is a file name - when omitted, archiving happens in the same
file.  %s will be replaced by the current file name (without directory part).
Archiving to a different file is useful to keep archived entries from
contributing to the Org-mode Agenda.

The part after the double colon is a headline.  The archived entries will be
filed under that headline.  When omitted, the subtrees are simply filed away
at the end of the file, as top-level entries.

Here are a few examples:
\"%s_archive::\"
	If the current file is Projects.org, archive in file
	Projects.org_archive, as top-level trees.  This is the default.

\"::* Archived Tasks\"
	Archive in the current file, under the top-level headline
	\"* Archived Tasks\".

\"~/org/archive.org::\"
	Archive in file ~/org/archive.org (absolute path), as top-level trees.

\"basement::** Finished Tasks\"
	Archive in file ./basement (relative path), as level 3 trees
	below the level 2 heading \"** Finished Tasks\".

You may set this option on a per-file basis by adding to the buffer a
line like

#+ARCHIVE: basement::** Finished Tasks"
  :group 'org-structure
  :type 'string)

(defcustom org-archive-mark-done t
  "Non-nil means, mark archived entries as DONE."
  :group 'org-structure
  :type 'boolean)

(defcustom org-archive-stamp-time t
  "Non-nil means, add a time stamp to archived entries.
The time stamp will be added directly after the TODO state keyword in the
first line, so it is probably best to use this in combinations with
`org-archive-mark-done'."
  :group 'org-structure
  :type 'boolean)

(defgroup org-link nil
  "Options concerning links in Org-mode."
  :tag "Org Link"
  :group 'org)

(defcustom org-link-format "<%s>"
  "Default format for linkes in the buffer.
This is a format string for printf, %s will be replaced by the link text.
If you want to make sure that your link is always properly terminated,
include angle brackets into this format, like \"<%s>\".  Some people also
recommend an additional URL: prefix, so the format would be \"<URL:%s>\"."
  :group 'org-link
  :type '(choice
	  (const :tag "\"%s\"   (e.g. http://www.there.com)" "%s")
	  (const :tag "\"<%s>\"  (e.g. <http://www.there.com>)" "<%s>")
	  (const :tag "\"<URL:%s>\"  (e.g. <URL:http://www.there.com>)" "<URL:%s>")
	  (string :tag "Other" :value "<%s>")))

(defcustom org-allow-space-in-links t
  "Non-nil means, file names in links may contain space characters.
When nil, it becomes possible to put several links into a line.
Note that in tables, a link never extends accross fields, so in a table
it is always possible to put several links into a line.
Changing this varable requires a re-launch of Emacs of become effective."
  :group 'org-link
  :type 'boolean)

(defcustom org-line-numbers-in-file-links t
  "Non-nil means, file links from `org-store-link' contain line numbers.
The line number will be added to the file name with :NNN and interpreted
by the command `org-open-at-point'.
Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link
  :type 'boolean)

(defcustom org-keep-stored-link-after-insertion nil
  "Non-nil means, keep link in list for entire session.

The command `org-store-link' adds a link pointing to the current
location to an internal list.  These links accumulate during a session.
The command `org-insert-link' can be used to insert links into any
Org-mode file (offering completion for all stored links).  When this
option is nil, every link which has been inserted once using \\[org-insert-link]
will be removed from the list, to make completing the unused links
more efficient."
  :group 'org-link
  :type 'boolean)

(defcustom org-link-frame-setup
  '((vm . vm-visit-folder-other-frame)
    (gnus . gnus-other-frame)
    (file . find-file-other-window))
  "Setup the frame configuration for following links.
When following a link with Emacs, it may often be useful to display
this link in another window or frame.  This variable can be used to
set this up for the different types of links.
For VM, use any of
    `vm-visit-folder'
    `vm-visit-folder-other-frame'
For Gnus, use any of
    `gnus'
    `gnus-other-frame'
For FILE, use any of
    `find-file'
    `find-file-other-window'
    `find-file-other-frame'
For the calendar, use the variable `calendar-setup'.
For BBDB, it is currently only possible to display the matches in
another window."
  :group 'org-link
  :type '(list
	  (cons (const vm)
		(choice
		 (const vm-visit-folder)
		 (const vm-visit-folder-other-window)
		 (const vm-visit-folder-other-frame)))
	  (cons (const gnus)
		(choice
		 (const gnus)
		 (const gnus-other-frame)))
	  (cons (const file)
		(choice
		 (const find-file)
		 (const find-file-other-window)
		 (const find-file-other-frame)))))

(defcustom org-usenet-links-prefer-google nil
  "Non-nil means, `org-store-link' will create web links to google groups.
When nil, Gnus will be used for such links.
Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link
  :type 'boolean)

(defcustom org-open-non-existing-files nil
  "Non-nil means, `org-open-file' will open non-existing file.
When nil, an error will be generated."
  :group 'org-link
  :type 'boolean)

(defcustom org-confirm-shell-links t
  "Non-nil means, ask for confirmation before executing shell links.
The default is true, to keep new users from shooting into their own foot."
  :group 'org-link
  :type 'boolean)

(defconst org-file-apps-defaults-gnu
  '((t        . emacs)
    ("jpg"    . "xv %s")
    ("gif"    . "xv %s")
    ("ppm"    . "xv %s")
    ("pgm"    . "xv %s")
    ("pbm"    . "xv %s")
    ("tif"    . "xv %s")
    ("png"    . "xv %s")
    ("ps"     . "gv %s")
    ("ps.gz"  . "gv %s")
    ("eps"    . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("mpeg"   . "plaympeg %s")
    ("mp3"    . "plaympeg %s")
    ("fig"    . "xfig %s")
    ("pdf"    . "acroread %s")
    ("doc"    . "soffice %s")
    ("ppt"    . "soffice %s")
    ("pps"    . "soffice %s")
    ("html"   . "netscape -remote openURL(%s,new-window)")
    ("htm"    . "netscape -remote openURL(%s,new-window)")
    ("xs"     . "soffice %s"))
  "Default file applications on a UNIX/LINUX system.
See `org-file-apps'.")

(defconst org-file-apps-defaults-macosx
  '((t        . "open %s")
    ("ps"     . "gv %s")
    ("ps.gz"  . "gv %s")
    ("eps"    . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("fig"    . "xfig %s"))
  "Default file applications on a MacOS X system.
The system \"open\" is known as a default, but we use X11 applications
for some files for which the OS does not have a good default.
See `org-file-apps'.")

(defconst org-file-apps-defaults-windowsnt
  '((t        . (w32-shell-execute "open" file)))
  "Default file applications on a Windows NT system.
The system \"open\" is used for most files.
See `org-file-apps'.")

(defcustom org-file-apps
  '(
    ("txt" . emacs)
    ("tex" . emacs)
    ("ltx" . emacs)
    ("org" . emacs)
    ("el"  . emacs)
    )
  "External applications for opening `file:path' items in a document.
Org-mode uses system defaults for different file types, but
you can use this variable to set the application for a given file
extension.  The entries in this list are cons cells with a file extension
and the corresponding command.  Possible values for the command are:
 `emacs'     The file will be visited by the current Emacs process.
 `default'   Use the default application for this file type.
 string      A command to be executed by a shell; %s will be replaced
	     by the path to the file.
 sexp        A Lisp form which will be evaluated.  The file path will
	     be available in the Lisp variable `file'.
For more examples, see the system specific constants
`org-file-apps-defaults-macosx'
`org-file-apps-defaults-windowsnt'
`org-file-apps-defaults-gnu'."
  :group 'org-link
  :type '(repeat
	  (cons (string :tag "Extension")
		(choice :value ""
		 (const :tag "Visit with Emacs" 'emacs)
		 (const :tag "Use system default" 'default)
		 (string :tag "Command")
		 (sexp :tag "Lisp form")))))


(defgroup org-remember nil
  "Options concerning interaction with remember.el."
  :tag "Org Remember"
  :group 'org)

(defcustom org-directory "~/org"
  "Directory with org files.
This directory will be used as default to prompt for org files.
Used by the hooks for remember.el."
  :group 'org-remember
  :type 'directory)

(defcustom org-default-notes-file "~/.notes"
  "Default target for storing notes.
Used by the hooks for remember.el.  This can be a string, or nil to mean
the value of `remember-data-file'."
  :group 'org-remember
  :type '(choice
	  (const :tag "Default from remember-data-file" nil)
	  file))

(defcustom org-reverse-note-order nil
  "Non-nil means, store new notes at the beginning of a file or entry.
When nil, new notes will be filed to the end of a file or entry."
  :group 'org-remember
  :type '(choice
	  (const :tag "Reverse always" t)
	  (const :tag "Reverse never" nil)
	  (repeat :tag "By file name regexp"
		  (cons regexp boolean))))

(defgroup org-table nil
  "Options concerning tables in Org-mode."
  :tag "Org Table"
  :group 'org)

(defcustom org-enable-table-editor 'optimized
  "Non-nil means, lines starting with \"|\" are handled by the table editor.
When nil, such lines will be treated like ordinary lines.

When equal to the symbol `optimized', the table editor will be optimized to
do the following
- Use automatic overwrite mode in front of whitespace in table fields.
  This make the structure of the table stay in tact as long as the edited
  field does not exceed the column width.
- Minimize the number of realigns.  Normally, the table is aligned each time
  TAB or RET are pressed to move to another field.  With optimization this
  happens only if changes to a field might have changed the column width.
Optimization requires replacing the functions `self-insert-command',
`delete-char', and `backward-delete-char' in Org-mode buffers, with a
slight (in fact: unnoticeable) speed impact for normal typing.  Org-mode is
very good at guessing when a re-align will be necessary, but you can always
force one with \\[org-ctrl-c-ctrl-c].

If you would like to use the optimized version in Org-mode, but the
un-optimized version in OrgTbl-mode, see the variable `orgtbl-optimized'.

This variable can be used to turn on and off the table editor during a session,
but in order to toggle optimization, a restart is required.

See also the variable `org-table-auto-blank-field'."
  :group 'org-table
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "on" t)
	  (const :tag "on, optimized" optimized)))

(defcustom org-table-auto-blank-field t
  "Non-nil means, automatically blank table field when starting to type into it.
This only happens when typing immediately after a field motion
command (TAB, S-TAB or RET).
Only relevant when `org-enable-table-editor' is equal to `optimized'."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table
   :type 'string)

(defcustom org-table-automatic-realign t
  "Non-nil means, automatically re-align table when pressing TAB or RETURN.
When nil, aligning is only done with \\[org-table-align], or after column
removal/insertion."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-spaces-around-separators '(1 . 1)
  "The number of spaces to be placed before and after separators."
  :group 'org-table
  :type '(cons (number :tag "Before \"|\"") (number :tag " After \"|\"")))

(defcustom org-table-spaces-around-invisible-separators '(1 . 2)
  "The number of spaces to be placed before and after separators.
This option applies when the column separators have been made invisible."
  :group 'org-table
  :type '(cons (number :tag "Before \"|\"") (number :tag " After \"|\"")))

(defcustom org-table-number-regexp "^[<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left.

The default value of this option is a regular expression which allows
anything which looks remotely like a number as used in scientific
context.  For example, all of the following will be considered a
number:
    12    12.2    2.4e-08    2x10^12    4.034+-0.02    2.7(10)  >3.5

Other options offered by the customize interface are more restrictive."
  :group 'org-table
  :type '(choice
	  (const :tag "Positive Integers"
		 "^[0-9]+$")
	  (const :tag "Integers"
		 "^[-+]?[0-9]+$")
	  (const :tag "Floating Point Numbers"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)$")
	  (const :tag "Floating Point Number or Integer"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)$")
	  (const :tag "Exponential, Floating point, Integer"
		 "^[-+]?[0-9.]+\\([eEdD][-+0-9]+\\)?$")
	  (const :tag "Very General Number-Like"
		 "^[<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*$")
	  (string :tag "Regexp:")))

(defcustom org-table-number-fraction 0.5
  "Fraction of numbers in a column required to make the column align right.
In a column all non-white fields are considered.  If at least this
fraction of fields is matched by `org-table-number-fraction',
alignment to the right border applies."
  :group 'org-table
  :type 'number)

(defcustom org-export-highlight-first-table-line t
  "Non-nil means, highlight the first table line.
In HTML export, this means use <th> instead of <td>.
In tables created with table.el, this applies to the first table line.
In Org-mode tables, all lines before the first horizontal separator
line will be formatted with <th> tags."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-tab-recognizes-table.el t
  "Non-nil means, TAB will automatically notice a table.el table.
When it sees such a table, it moves point into it and - if necessary -
calls `table-recognize-table'."
  :group 'org-table
  :type 'boolean)

(defgroup org-table-calculation nil
  "Options concerning tables in Org-mode."
  :tag "Org Table Calculation"
  :group 'org)

(defcustom org-table-copy-increment t
  "Non-nil means, increment when copying current field with \\[org-table-copy-down]."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-calc-default-modes
  '(calc-internal-prec 12
    calc-float-format  (float 5)
    calc-angle-mode    deg
    calc-prefer-frac   nil
    calc-symbolic-mode nil
    calc-date-format (YYYY "-" MM "-" DD " " Www (" " HH ":" mm))
    calc-display-working-message t
    )
  "List with Calc mode settings for use in calc-eval for table formulas.
The list must contain alternating symbols (calc modes variables and values.
Don't remove any of the default settings, just change the values.  Org-mode
relies on the variables to be present in the list."
  :group 'org-table-calculation
  :type 'plist)

(defcustom org-table-formula-evaluate-inline t
  "Non-nil means, TAB and RET evaluate a formula in current table field.
If the current field starts with an equal sign, it is assumed to be a formula
which should be evaluated as described in the manual and in the documentation
string of the command `org-table-eval-formula'.  This feature requires the
Emacs calc package.
When this variable is nil, formula calculation is only available through
the command \\[org-table-eval-formula]."
  :group 'org-table-calculation
  :type 'boolean)


(defcustom org-table-formula-use-constants t
  "Non-nil means, interpret constants in formulas in tables.
A constant looks like `$c' or `$Grav' and will be replaced before evaluation
by the value given in `org-table-formula-constants', or by a value obtained
from the `constants.el' package."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-constants nil
  "Alist with constant names and values, for use in table formulas.
The car of each element is a name of a constant, without the `$' before it.
The cdr is the value as a string.  For example, if you'd like to use the
speed of light in a formula, you would configure

  (setq org-table-formula-constants '((\"c\" . \"299792458.\")))

and then use it in an equation like `$1*$c'."
  :group 'org-table-calculation
  :type '(repeat
	  (cons (string :tag "name")
		(string :tag "value"))))

(defcustom org-table-formula-numbers-only nil
  "Non-nil means, calculate only with numbers in table formulas.
Then all input fields will be converted to a number, and the result
must also be a number.  When nil, calc's full potential is available
in table calculations, including symbolics etc."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-allow-automatic-line-recalculation t
  "Non-nil means, lines makred with |#| or |*| will be recomputed automatically.
Automatically means, when TAB or RET or C-c C-c are pressed in the line."
  :group 'org-table-calculation
  :type 'boolean)

(defgroup org-export nil
  "Options for exporting org-listings."
  :tag "Org Export"
  :group 'org)

(defcustom org-export-language-setup
  '(("en"  "Author"          "Date"  "Table of Contents")
    ("da"  "Ophavsmand"      "Dato"  "Indhold")
    ("de"  "Autor"           "Datum" "Inhaltsverzeichnis")
    ("es"  "Autor"           "Fecha" "\xccndice")
    ("fr"  "Auteur"          "Date"  "Table des Mati\xe8res")
    ("it"  "Autore"          "Data"  "Indice")
    ("nl"  "Auteur"          "Datum" "Inhoudsopgave")
    ("nn"  "Forfattar"       "Dato"  "Innhold")  ;; nn = Norsk (nynorsk)
    ("sv"  "F\xf6rfattarens" "Datum" "Inneh\xe5ll"))
  "Terms used in export text, translated to different languages.
Use the variable `org-export-default-language' to set the language,
or use the +OPTION lines for a per-file setting."
  :group 'org-export
  :type '(repeat
	  (list
	   (string :tag "HTML language tag")
	   (string :tag "Author")
	   (string :tag "Date")
	   (string :tag "Table of Contents"))))

(defcustom org-export-default-language "en"
  "The default language of HTML export, as a string.
This should have an association in `org-export-language-setup'"
  :group 'org-export
  :type 'string)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.
Inferior levels will produce itemize lists when exported.
Note that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the +OPTIONS line, e.g. \"H:2\"."
  :group 'org-export
  :type 'number)

(defcustom org-export-with-section-numbers t
  "Non-nil means, add section numbers to headlines when exporting.

This option can also be set with the +OPTIONS line, e.g. \"num:t\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-toc t
  "Non-nil means, create a table of contents in exported files.
The TOC contains headlines with levels up to`org-export-headline-levels'.

Headlines which contain any TODO items will be marked with \"(*)\" in
ASCII export, and with red color in HTML output.

In HTML output, the TOC will be clickable.

This option can also be set with the +OPTIONS line, e.g. \"toc:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-preserve-breaks nil
  "Non-nil means, preserve all line breaks when exporting.
Normally, in HTML output paragraphs will be reformatted.  In ASCII
export, line breaks will always be preserved, regardless of this variable.

This option can also be set with the +OPTIONS line, e.g. \"\\n:t\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-inline-images t
  "Non-nil means, inline images into exported HTML pages.
The link will still be to the original location of the image file.
So if you are moving the page, lets say to your public HTML site,
you will have to move the image and maybe change the link."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-expand t
  "Non-nil means, for HTML export, treat @<...> as HTML tag.
When nil, these tags will be exported as plain text and therefore
not be interpreted by a browser.

This option can also be set with the +OPTIONS line, e.g. \"@:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-fixed-width t
  "Non-nil means, lines starting with \":\" will be in fixed width font.
This can be used to have pre-formatted text, fragments of code etc.  For
example
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  In ASCII export, this option
has no effect.

This option can also be set with the +OPTIONS line, e.g. \"::nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-tables t
  "If non-nil, lines starting with \"|\" define a table.
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------|
  | Arthur Dent | England  | 29.2.2100 |

In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-prefer-native-exporter-for-tables nil
  "Non-nil means, always export tables created with table.el natively.
Natively means, use the HTML code generator in table.el.
When nil, Org-mode's own HTML generator is used when possible (i.e. if
the table does not use row- or column-spanning).  This has the
advantage, that the automatic HTML conversions for math symbols and
sub/superscripts can be applied.  Org-mode's HTML generator is also
much faster."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-table-tag
  "<table border=1 cellspacing=0 cellpadding=6>"
  "The HTML tag used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export
  :type 'string)

(defcustom org-export-with-emphasize t
  "Non-nil means, interpret *word*, /word/, and _word_ as emphasized text.
If the export target supports emphasizing text, the word will be
typeset in bold, italic, or underlined, respectively.  Works only for
single words, but you can say: I *really* *mean* *this*.
In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.
This has to be set before loading org.el to be effective."
  :group 'org-export
  :type 'integer)

;; FIXME: Should () parens be removed as well in sub/superscripts?
(defcustom org-export-with-sub-superscripts t
  "Non-nil means, interpret \"_\" and \"^\" for export.
When this option is turned on, you can use TeX-like syntax for sub- and
superscripts.  Several characters after \"_\" or \"^\" will be
considered as a single item - so grouping with {} is normally not
needed.  For example, the following things will be parsed as single
sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose the
sub/superscript.
In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"^:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-TeX-macros t
  "Non-nil means, interpret simple TeX-like macros when exporting.
For example, HTML export converts \\alpha to &alpha; and \\AA to &Aring;.
No only real TeX macros will work here, but the standard HTML entities
for math can be used as macro names as well.  For a list of supported
names in HTML export, see the constant `org-html-entities'.
In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"TeX:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-with-timestamp nil
  "If non-nil, write `org-export-html-html-helper-timestamp'
into the exported html text.  Otherwise, the buffer will just be saved
to a file."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-html-helper-timestamp
  "<br><br><hr><p><!-- hhmts start --> <!-- hhmts end -->\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode."
  :group 'org-export
  :type 'string)

(defcustom org-export-ascii-show-new-buffer t
  "Non-nil means, popup buffer containing the exported ASCII text.
Otherwise the buffer will just be saved to a file and stay hidden."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-show-new-buffer nil
  "Non-nil means,  popup buffer containing the exported html text.
Otherwise, the buffer will just be saved to a file and stay hidden."
  :group 'org-export
  :type 'boolean)

(defgroup org-faces nil
  "Faces for highlighting in Org-mode."
  :tag "Org Faces"
  :group 'org)

(defface org-level-1 ;; font-lock-function-name-face
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "Face used for level 1 headlines."
  :group 'org-faces)

(defface org-level-2 ;; font-lock-variable-name-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face used for level 2 headlines."
  :group 'org-faces)

(defface org-level-3 ;; font-lock-keyword-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face used for level 3 headlines."
  :group 'org-faces)

(defface org-level-4   ;; font-lock-comment-face
  '((((type tty pc) (class color) (background light)) (:foreground "red"))
    (((type tty pc) (class color) (background dark)) (:foreground "red1"))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:bold t :italic t)))
  "Face used for level 4 headlines."
  :group 'org-faces)

(defface org-level-5 ;; font-lock-type-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:bold t :underline t)))
  "Face used for level 5 headlines."
  :group 'org-faces)

(defface org-level-6 ;; font-lock-constant-face
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Face used for level 6 headlines."
  :group 'org-faces)

(defface org-level-7 ;; font-lock-builtin-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Face used for level 7 headlines."
  :group 'org-faces)

(defface org-level-8 ;; font-lock-string-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used for level 8 headlines."
  :group 'org-faces)

(defface org-special-keyword ;; font-lock-string-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used for level 8 headlines."
  :group 'org-faces)

(defface org-warning ;; font-lock-warning-face
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Red1" :bold t))
;    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:inverse-video t :bold t)))
  "Face for deadlines and TODO keywords."
  :group 'org-faces)

(defcustom org-fontify-done-headline nil
  "Non-nil means, change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-faces
  :type 'boolean)

(defface org-headline-done ;; font-lock-string-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used to indicate that a headline is DONE.  See also the variable
`org-fontify-done-headline'."
  :group 'org-faces)

;; Inheritance does not yet work for xemacs. So we just copy...

(defface org-deadline-announce
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "Face for upcoming deadlines."
  :group 'org-faces)

(defface org-scheduled-today
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "DarkGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:bold t :underline t)))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-scheduled-previously
  '((((type tty pc) (class color) (background light)) (:foreground "red"))
    (((type tty pc) (class color) (background dark)) (:foreground "red1"))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:bold t :italic t)))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defface org-formula
  '((((type tty pc) (class color) (background light)) (:foreground "red"))
    (((type tty pc) (class color) (background dark)) (:foreground "red1"))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:bold t :italic t)))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defface org-link
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face for links."
  :group 'org-faces)

(defface org-done ;; font-lock-type-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (background dark)) (:foreground "PaleGreen" :bold t))
    (t (:bold t :underline t)))
  "Face used for DONE."
  :group 'org-faces)

(defface org-table ;; font-lock-function-name-face
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "Face used for tables."
  :group 'org-faces)

(defface org-time-grid ;; font-lock-variable-name-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face used for level 2 headlines."
  :group 'org-faces)

(defvar org-level-faces
  '(
    org-level-1
    org-level-2
    org-level-3
    org-level-4
    org-level-5
    org-level-6
    org-level-7
    org-level-8
    ))
(defvar org-n-levels (length org-level-faces))

(defun org-set-regexps-and-options ()
  "Precompute regular expressions for current buffer."
  (when (eq major-mode 'org-mode)
    (let ((re (org-make-options-regexp
	       '("CATEGORY" "SEQ_TODO" "PRI_TODO" "TYP_TODO"
		 "STARTUP" "ARCHIVE")))
	  (splitre "[ \t]+")
	  kwds int key value cat arch)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (setq key (match-string 1) value (match-string 2))
	    (cond
	     ((equal key "CATEGORY")
	      (if (string-match "[ \t]+$" value)
		  (setq value (replace-match "" t t value)))
	      (setq cat (intern value)))
	     ((equal key "SEQ_TODO")
	      (setq int 'sequence
		    kwds (append kwds (org-split-string value splitre))))
	     ((equal key "PRI_TODO")
	      (setq int 'priority
		    kwds (append kwds (org-split-string value splitre))))
	     ((equal key "TYP_TODO")
	      (setq int 'type
		    kwds (append kwds (org-split-string value splitre))))
	     ((equal key "STARTUP")
	      (let ((opts (org-split-string value splitre))
		    (set '(("fold" org-startup-folded t)
			   ("nofold" org-startup-folded nil)
			   ("content" org-startup-folded content)
			   ("dlcheck" org-startup-with-deadline-check t)
			   ("nodlcheck" org-startup-with-deadline-check nil)))
		    l var val)
		(while (setq l (assoc (pop opts) set))
		  (setq var (nth 1 l) val (nth 2 l))
		  (set (make-local-variable var) val))))
	     ((equal key "ARCHIVE")
	      (string-match " *$" value)
	      (setq arch (replace-match "" t t value))
	      (remove-text-properties 0 (length arch)
				      '(face t fontified t) arch)))
	    )))
      (and cat (set (make-local-variable 'org-category) cat))
      (and kwds (set (make-local-variable 'org-todo-keywords) kwds))
      (and arch (set (make-local-variable 'org-archive-location) arch))
      (and int (set (make-local-variable 'org-todo-interpretation) int)))
    ;; Compute the regular expressions and other local variables
    (setq org-todo-kwd-priority-p (equal org-todo-interpretation 'priority)
	  org-todo-kwd-max-priority (1- (length org-todo-keywords))
	  org-ds-keyword-length (+ 2 (max (length org-deadline-string)
					  (length org-scheduled-string)))
	  org-done-string
	  (nth (1- (length org-todo-keywords)) org-todo-keywords)
	  org-todo-regexp
	  (concat "\\<\\(" (mapconcat 'regexp-quote org-todo-keywords
				      "\\|") "\\)\\>")
	  org-not-done-regexp
	  (concat "\\<\\("
		  (mapconcat 'regexp-quote
			     (nreverse (cdr (reverse org-todo-keywords)))
			     "\\|")
		  "\\)\\>")
	  org-todo-line-regexp
	  (concat "^\\(\\*+\\)[ \t]*\\("
		  (mapconcat 'regexp-quote org-todo-keywords "\\|")
		  "\\)? *\\(.*\\)")
	  org-nl-done-regexp
	  (concat "[\r\n]\\*+[ \t]+" org-done-string "\\>")
	  org-looking-at-done-regexp (concat "^" org-done-string "\\>")
	  org-deadline-regexp (concat "\\<" org-deadline-string)
	  org-deadline-time-regexp
	  (concat "\\<" org-deadline-string " *<\\([^>]+\\)>")
	  org-deadline-line-regexp
	  (concat "\\<\\(" org-deadline-string "\\).*")
	  org-scheduled-regexp
	  (concat "\\<" org-scheduled-string)
	  org-scheduled-time-regexp
	  (concat "\\<" org-scheduled-string " *<\\([^>]+\\)>"))
    (org-set-font-lock-defaults)))

;; Tell the compiler about dynamically scoped variables,
;; and variables from other packages
(eval-when-compile
  (defvar zmacs-regions)
  (defvar original-date)
  (defvar org-transient-mark-mode)
  (defvar org-old-auto-fill-inhibit-regexp)
  (defvar orgtbl-mode-menu)
  (defvar org-html-entities)
  (defvar org-goto-start-pos)
  (defvar org-cursor-color)
  (defvar org-time-was-given)
  (defvar org-ts-what)
  (defvar mark-active)
  (defvar timecnt)
  (defvar levels-open)
  (defvar title)
  (defvar author)
  (defvar email)
  (defvar text)
  (defvar entry)
  (defvar date)
  (defvar language)
  (defvar options)
  (defvar ans1)
  (defvar ans2)
  (defvar starting-day)
  (defvar include-all-loc)
  (defvar vm-message-pointer)
  (defvar vm-folder-directory)
  (defvar wl-summary-buffer-elmo-folder)
  (defvar wl-summary-buffer-folder-name)
  (defvar gnus-group-name)
  (defvar gnus-article-current)
  (defvar w3m-current-url)
  (defvar org-selected-point)
  (defvar calendar-mode-map)
  (defvar remember-save-after-remembering)
  (defvar remember-data-file))


;;; Define the mode

(defvar org-mode-map (copy-keymap outline-mode-map)
  "Keymap for Org-mode.")

(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)

;; We use a before-change function to check if a table might need
;; an update.
(defvar org-table-may-need-update t
  "Indicates of a table might need an update.
This variable is set by `org-before-change-function'. `org-table-align'
sets it back to nil.")
(defvar org-mode-hook nil)
(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.


;;;###autoload
(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carstens outline-mode for keeping track of everything.\"

Org-mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org-mode is
implemented on top of outline-mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org-mode file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}"
  (easy-menu-add org-org-menu)
  (easy-menu-add org-tbl-menu)
  (org-install-agenda-files-menu)
  (setq outline-regexp "\\*+")
  (if org-startup-truncated (setq truncate-lines t))
  (org-set-regexps-and-options)
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'org-unfontify-region)
  ;; Activate before-change-function
  (set (make-local-variable 'org-table-may-need-update) t)
  (make-local-hook 'before-change-functions)  ;; needed for XEmacs
  (add-hook 'before-change-functions 'org-before-change-function nil
	    'local)
  ;; Paragraph regular expressions
  (set (make-local-variable 'paragraph-separate) "\f\\|[ 	]*$\\|\\([*\f]+\\)")
  (set (make-local-variable 'paragraph-start) "\f\\|[ 	]*$\\|\\([*\f]+\\)")
  ;; Inhibit auto-fill for headers, tables and fixed-width lines.
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       (concat "\\*\\|#"
	       (if (or org-enable-table-editor org-enable-fixed-width-editor)
		   (concat
		    "\\|[ \t]*["
		    (if org-enable-table-editor "|" "")
		   (if org-enable-fixed-width-editor ":"  "")
		   "]"))))
  (set (make-local-variable 'fill-paragraph-function) 'org-fill-paragraph)
  ;; Settings for Calc embedded mode
  (set (make-local-variable 'calc-embedded-open-formula) "|\\|\n")
  (set (make-local-variable 'calc-embedded-close-formula) "|\\|\n")
  (if (and org-insert-mode-line-in-empty-file
	   (interactive-p)
	   (= (point-min) (point-max)))
      (insert "    -*- mode: org -*-\n\n"))

  ;; Get rid of Outline menus, they are not needed
  ;; Need to do this here because define-derived-mode sets up
  ;; the keymap so late.
  (if org-xemacs-p
      (progn
	(delete-menu-item '("Headings"))
	(delete-menu-item '("Show"))
	(delete-menu-item '("Hide"))
	(set-menubar-dirty-flag))
    (define-key org-mode-map [menu-bar headings] 'undefined)
    (define-key org-mode-map [menu-bar hide] 'undefined)
    (define-key org-mode-map [menu-bar show] 'undefined))

  (unless org-inhibit-startup
    (if org-startup-with-deadline-check
	(call-interactively 'org-check-deadlines)
      (cond
       ((eq org-startup-folded t)
	(org-cycle '(4)))
       ((eq org-startup-folded 'content)
	(let ((this-command 'org-cycle) (last-command 'org-cycle))
	  (org-cycle '(4)) (org-cycle '(4))))))))

(defun org-fill-paragraph (&optional justify)
  "Re-align a table, pass through to fill-paragraph if no table."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "\\s-*\\(|\\|\\+-+\\)")))

(defsubst org-current-line (&optional pos)
  (+ (if (bolp) 1 0) (count-lines (point-min) (or pos (point)))))

;;; Font-Lock stuff

(defvar org-mouse-map (make-sparse-keymap))
(define-key org-mouse-map
  (if org-xemacs-p [button2] [mouse-2]) 'org-open-at-mouse)
(define-key org-mouse-map
  (if org-xemacs-p [button3] [mouse-3]) 'org-find-file-at-mouse)

(require 'font-lock)

(defconst org-non-link-chars "\t\n\r|<>\000")
(defconst org-link-regexp
  (if org-allow-space-in-links
      (concat
       "\\(https?\\|ftp\\|mailto\\|file\\|news\\|bbdb\\|vm\\|wl\\|rmail\\|gnus\\|shell\\):\\([^" org-non-link-chars "]+[^ " org-non-link-chars "]\\)")
    (concat
     "\\(https?\\|ftp\\|mailto\\|file\\|news\\|bbdb\\|vm\\|wl\\|rmail\\|gnus\\|shell\\):\\([^ " org-non-link-chars "]+\\)")
    )
  "Regular expression for matching links.")
(defconst org-link-maybe-angles-regexp
  (concat "<?\\(" org-link-regexp "\\)>?")
  "Matches a link and optionally surrounding angle brackets.")
(defconst org-protected-link-regexp
  (concat "\000" org-link-regexp "\000")
  "Matches a link and optionally surrounding angle brackets.")

(defconst org-ts-lengths
  (cons (length (format-time-string (car org-time-stamp-formats)))
	(length (format-time-string (cdr org-time-stamp-formats))))
  "This holds the lengths of the two different time formats.")
(defconst org-ts-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^\r\n>]*\\)>"
  "Regular expression for fast time stamp matching.")
(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([^0-9>\r\n]*\\)\\(\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")
(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 ">")
  "Regular expression matching time stamps, with groups.")
(defconst org-tr-regexp (concat org-ts-regexp "--?-?" org-ts-regexp)
  "Regular expression matching a time stamp range.")
(defconst org-tsr-regexp (concat org-ts-regexp "\\(--?-?"
				 org-ts-regexp "\\)?")
  "Regular expression matching a time stamp or time stamp range.")

(defun org-activate-links (limit)
  "Run through the buffer and add overlays to links."
  (if (re-search-forward org-link-regexp limit t)
      (progn
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map))
	t)))

(defun org-activate-dates (limit)
  "Run through the buffer and add overlays to dates."
  (if (re-search-forward org-tsr-regexp limit t)
      (progn
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map))
	t)))

(defun org-font-lock-level ()
  (save-excursion
    (org-back-to-heading t)
    (- (match-end 0) (match-beginning 0))))

(defvar org-font-lock-keywords nil)

(defun org-set-font-lock-defaults ()
  (let ((org-font-lock-extra-keywords
	 (list
	  '(org-activate-links (0 'org-link))
	  '(org-activate-dates (0 'org-link))
	  (list (concat "^\\*+[ \t]*" org-not-done-regexp)
		'(1 'org-warning t))
	  (list (concat "\\[#[A-Z]\\]") '(0 'org-special-keyword t))
;	  (list (concat "\\<" org-deadline-string) '(0 'org-warning t))
;	  (list (concat "\\<" org-scheduled-string) '(0 'org-warning t))
	  (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
	  ;; '("\\(\\s-\\|^\\)\\(\\*\\([a-zA-Z]+\\)\\*\\)\\([^a-zA-Z*]\\|$\\)"
	  ;; (3 'bold))
	  ;; '("\\(\\s-\\|^\\)\\(/\\([a-zA-Z]+\\)/\\)\\([^a-zA-Z*]\\|$\\)"
	  ;; (3 'italic))
	  ;; '("\\(\\s-\\|^\\)\\(_\\([a-zA-Z]+\\)_\\)\\([^a-zA-Z*]\\|$\\)"
	  ;; (3 'underline))
;	  (list (concat "^\\*+[ \t]*\\<\\(" org-comment-string "\\)\\>")
;		'(1 'org-warning t))
	  (list (concat "^\\*+[ \t]*\\<\\(" org-comment-string
			"\\|" org-quote-string "\\)\\>")
		'(1 'org-special-keyword t))
	  '("^#.*" (0 'font-lock-comment-face t))
	  (if org-fontify-done-headline
	      (list (concat "^[*]+ +\\<\\(" org-done-string "\\)\\(.*\\)\\>")
		    '(1 'org-done t) '(2 'org-headline-done t))
	    (list (concat "^[*]+ +\\<\\(" org-done-string "\\)\\>")
		  '(1 'org-done t)))
	  '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
	    (1 'org-table t))
	  '("^[ \t]*\\(:.*\\)" (1 'org-table t))
	  '("| *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	  '("^[ \t]*| *\\([#!$*_^]\\) *|" (1 'org-formula t))
	  )))
    (set (make-local-variable 'org-font-lock-keywords)
	 (append
	  (if org-noutline-p     ; FIXME:  I am not sure if eval will work
				 ; on XEmacs if noutline is ever ported
	      `((eval . (list "^\\(\\*+\\).*"
			      ,(if org-level-color-stars-only 1 0)
			      '(nth   ;; FIXME:  1<->0 ????
				  (% (- (match-end 1) (match-beginning 1) 1)
				     org-n-levels)
				  org-level-faces)
			      nil t)))
	    `(("^\\(\\(\\*+\\)[^\r\n]*\\)[\n\r]"
	       (,(if org-level-color-stars-only 2 0)
	        (nth (% (- (match-end 2) (match-beginning 2) 1)
			org-n-levels)
		     org-level-faces)
		nil t))))
	  org-font-lock-extra-keywords))
    (set (make-local-variable 'font-lock-defaults)
	 '(org-font-lock-keywords t nil nil backward-paragraph))
    (kill-local-variable 'font-lock-keywords) nil))

(defun org-unfontify-region (beg end &optional maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark buffer-file-name buffer-file-truename)
    (remove-text-properties beg end '(mouse-face nil keymap nil))))

;;; Visibility cycling

(defvar org-cycle-global-status nil)
(defvar org-cycle-subtree-status nil)
(defun org-cycle (&optional arg)
  "Visibility cycling for Org-mode.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
	       this state, you can move to one of the children and
	       zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is the the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg."
  (interactive "P")

  (if (or (and (bobp) (not (looking-at outline-regexp)))
	  (equal arg '(4)))
      ;; special case:  use global cycling
      (setq arg t))

  (cond

   ((org-at-table-p 'any)
    ;; Enter the table or move to the next field in the table
    (or (org-table-recognize-table.el)
	(progn
	  (org-table-justify-field-maybe)
	  (org-table-next-field))))

   ((eq arg t) ;; Global cycling

    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (message "CONTENTS...")
      (save-excursion
	;; Visit all headings and show their offspring
	(goto-char (point-max))
	(catch 'exit
	  (while (and (progn (condition-case nil
				 (outline-previous-visible-heading 1)
			       (error (goto-char (point-min))))
			     t)
		      (looking-at outline-regexp))
	    (show-branches)
	    (if (bobp) (throw 'exit nil))))
	(message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (show-all)
      (message "SHOW ALL")
      (setq org-cycle-global-status 'all)
      (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (hide-sublevels 1)
      (message "OVERVIEW")
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview))))

   ((integerp arg)
    ;; Show-subtree, ARG levels up from here.
    (save-excursion
      (org-back-to-heading)
      (outline-up-heading (if (< arg 0) (- arg)
			    (- (outline-level) arg)))
      (org-show-subtree)))

   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (org-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; First, some boundaries
      (save-excursion
	(org-back-to-heading)
	(save-excursion
	  (beginning-of-line 2)
	  (while (and (not (eobp))   ;; this is like `next-line'
		      (get-char-property (1- (point)) 'invisible))
	    (beginning-of-line 2)) (setq eol (point)))
	(outline-end-of-heading)   (setq eoh (point))
	(outline-end-of-subtree)   (setq eos (point))
	(outline-next-heading))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
	;; Nothing is hidden behind this heading
	(message "EMPTY ENTRY")
	(setq org-cycle-subtree-status nil))
       ((>= eol eos)
	;; Entire subtree is hidden in one line: open it
	(org-show-entry)
	(show-children)
	(message "CHILDREN")
	(setq org-cycle-subtree-status 'children)
	(run-hook-with-args 'org-cycle-hook 'children))
       ((and (eq last-command this-command)
	     (eq org-cycle-subtree-status 'children))
	;; We just showed the children, now show everything.
	(org-show-subtree)
	(message "SUBTREE")
	(setq org-cycle-subtree-status 'subtree)
	(run-hook-with-args 'org-cycle-hook 'subtree))
       (t
	;; Default action: hide the subtree.
	(hide-subtree)
	(message "FOLDED")
	(setq org-cycle-subtree-status 'folded)
	(run-hook-with-args 'org-cycle-hook 'folded)))))

   ;; TAB emulation
   (buffer-read-only (org-back-to-heading))
   ((if (and (eq org-cycle-emulate-tab 'white)
	     (save-excursion (beginning-of-line 1) (looking-at "[ \t]+$")))
	t
      (eq org-cycle-emulate-tab t))
    (if (and (looking-at "[ \n\r\t]")
	     (string-match "^[ \t]*$" (buffer-substring
				       (point-at-bol) (point))))
	(progn
	  (beginning-of-line 1)
	  (and (looking-at "[ \t]+") (replace-match ""))))
    (indent-relative))

   (t (save-excursion
	(org-back-to-heading)
	(org-cycle)))))

(defun org-optimize-window-after-visibility-change (state)
  "Adjust the window after a change in outline visibility.
This function is the default value of the hook `org-cycle-hook'."
  (cond
   ((eq state 'overview) (org-first-headline-recenter 1))
   ((eq state 'content)  nil)
   ((eq state 'all)      nil)
   ((eq state 'folded)   nil)
   ((eq state 'children) (or (org-subtree-end-visible-p) (recenter 1)))
   ((eq state 'subtree)  (or (org-subtree-end-visible-p) (recenter 1)))))

(defun org-subtree-end-visible-p ()
  "Is the end of the current subtree visible?"
  (pos-visible-in-window-p
   (save-excursion (outline-end-of-subtree) (point))))

(defun org-first-headline-recenter (&optional N)
  "Move cursor to the first headline and recenter the headline.
Optional argument N means, put the headline into the Nth line of the window."
  (goto-char (point-min))
  (when (re-search-forward (concat "^" outline-regexp) nil t)
    (beginning-of-line)
    (recenter (prefix-numeric-value N))))

(defvar org-goto-window-configuration nil)
(defvar org-goto-marker nil)
(defvar org-goto-map (make-sparse-keymap))
(let ((cmds '(isearch-forward isearch-backward)) cmd)
  (while (setq cmd (pop cmds))
    (substitute-key-definition cmd cmd org-goto-map global-map)))
(define-key org-goto-map "\C-m"     'org-goto-ret)
(define-key org-goto-map [(left)]   'org-goto-left)
(define-key org-goto-map [(right)]  'org-goto-right)
(define-key org-goto-map [(?q)]     'org-goto-quit)
(define-key org-goto-map [(control ?g)] 'org-goto-quit)
(define-key org-goto-map "\C-i" 'org-cycle)
(define-key org-goto-map [(down)] 'outline-next-visible-heading)
(define-key org-goto-map [(up)] 'outline-previous-visible-heading)
(define-key org-goto-map "n" 'outline-next-visible-heading)
(define-key org-goto-map "p" 'outline-previous-visible-heading)
(define-key org-goto-map "f" 'outline-forward-same-level)
(define-key org-goto-map "b" 'outline-backward-same-level)
(define-key org-goto-map "u" 'outline-up-heading)
(define-key org-goto-map "\C-c\C-n" 'outline-next-visible-heading)
(define-key org-goto-map "\C-c\C-p" 'outline-previous-visible-heading)
(define-key org-goto-map "\C-c\C-f" 'outline-forward-same-level)
(define-key org-goto-map "\C-c\C-b" 'outline-backward-same-level)
(define-key org-goto-map "\C-c\C-u" 'outline-up-heading)
(let ((l '(1 2 3 4 5 6 7 8 9 0)))
  (while l (define-key org-goto-map (int-to-string (pop l)) 'digit-argument)))

(defconst org-goto-help
"Select a location to jump to, press RET
\[Up]/[Down]=next/prev headline   TAB=cycle visibility   RET=select   [Q]uit")

(defun org-goto ()
  "Go to a different location of the document, keeping current visibility.

When you want to go to a different location in a document, the fastest way
is often to fold the entire buffer and then dive into the tree.  This
method has the disadvantage, that the previous location will be folded,
which may not be what you want.

This command works around this by showing a copy of the current buffer in
overview mode.  You can dive into the tree in that copy, to find the
location you want to reach.  When pressing RET, the command returns to the
original buffer in which the visibility is still unchanged.  It then jumps
to the new location, making it and the headline hierarchy above it visible."
  (interactive)
  (let* ((org-goto-start-pos (point))
	 (selected-point
	  (org-get-location (current-buffer) org-goto-help)))
    (if selected-point
	(progn
	  (goto-char selected-point)
	  (if (org-invisible-p) (org-show-hierarchy-above)))
      (error "Quit"))))

(defun org-get-location (buf help)
  "Let the user select a location in the Org-mode buffer BUF.
This function uses a recursive edit.  It returns the selected position
or nil."
  (let (org-selected-point)
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(switch-to-buffer (get-buffer-create "*org-goto*"))
	(with-output-to-temp-buffer "*Help*"
	  (princ help))
	(shrink-window-if-larger-than-buffer (get-buffer-window "*Help*"))
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert-buffer-substring buf)
	(let ((org-startup-truncated t)
	      (org-startup-folded t)
	      (org-startup-with-deadline-check nil))
	  (org-mode))
	(setq buffer-read-only t)
	(if (boundp 'org-goto-start-pos)
	    (goto-char org-goto-start-pos)
	  (goto-char (point-min)))
	(org-beginning-of-line)
	(message "Select location and press RET")
	;; now we make sure that during selection, ony very few keys work
	;; and that it is impossible to switch to another window.
	(let ((gm (current-global-map))
	      (overriding-local-map org-goto-map))
	  (unwind-protect
	      (progn
		(use-global-map org-goto-map)
		(recursive-edit))
	    (use-global-map gm)))))
    (kill-buffer "*org-goto*")
    org-selected-point))

;;  FIXME:  It may not be a good idea to temper with the prefix argument...
(defun org-goto-ret (&optional arg)
  "Finish org-goto by going to the new location."
  (interactive "P")
  (setq org-selected-point (point)
	current-prefix-arg arg)
  (throw 'exit nil))

(defun org-goto-left ()
  "Finish org-goto by going to the new location."
  (interactive)
  (if (org-on-heading-p)
      (progn
	(beginning-of-line 1)
	(setq org-selected-point (point)
	      current-prefix-arg (- (match-end 0) (match-beginning 0)))
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-right ()
  "Finish org-goto by going to the new location."
  (interactive)
  (if (org-on-heading-p)
      (progn
	(outline-end-of-subtree)
	(or (eobp) (forward-char 1))
	(setq org-selected-point (point)
	      current-prefix-arg (- (match-end 0) (match-beginning 0)))
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-quit ()
  "Finish org-goto without cursor motion."
  (interactive)
  (setq org-selected-point nil)
  (throw 'exit nil))

;;; Promotion, Demotion, Inserting new headlines

(defvar org-ignore-region nil
  "To temporarily disable the active region.")

(defun org-insert-heading ()
  "Insert a new heading with same depth at point."
  (interactive)
  (let* ((head (save-excursion
		 (condition-case nil
		     (org-back-to-heading)
		   (error (outline-next-heading)))
		 (prog1 (match-string 0)
		   (funcall outline-level)))))
    (unless (bolp) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (unless (equal (char-before) ?\ )
      (insert " "))
    (run-hooks 'org-insert-heading-hook)))

(defun org-insert-todo-heading (arg)
  "Insert a new heading with the same level and TODO state as current heading.
If the heading has no TODO state, or if the state is DONE, use the first
state (TODO by default).  Also with prefix arg, force first state."
  (interactive "P")
  (org-insert-heading)
  (save-excursion
    (org-back-to-heading)
    (outline-previous-heading)
    (looking-at org-todo-line-regexp))
  (if (or arg
	  (not (match-beginning 2))
	  (equal (match-string 2) org-done-string))
      (insert (car org-todo-keywords) " ")
    (insert (match-string 2) " ")))

(defun org-promote-subtree ()
  "Promote the entire subtree.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-map-tree 'org-promote)))

(defun org-demote-subtree ()
  "Demote the entire subtree.  See `org-demote'.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-map-tree 'org-demote)))

(defun org-do-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'org-promote (region-beginning) (region-end))
      (org-promote)))
  (org-fix-position-after-promote))

(defun org-do-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'org-demote (region-beginning) (region-end))
      (org-demote)))
  (org-fix-position-after-promote))

(defun org-fix-position-after-promote ()
  "Make sure that after pro/demotion cursor position is right."
  (and (equal (char-after) ?\ )
       (equal (char-before) ?*)
       (forward-char 1)))

(defun org-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (up-head (make-string (1- level) ?*)))
    (if (= level 1) (error "Cannot promote to level 0. UNDO to recover"))
    (replace-match up-head nil t)
    (if org-adapt-indentation
	(org-fixup-indentation "^ " "" "^ ?\\S-"))))

(defun org-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (down-head (make-string (1+ level) ?*)))
    (replace-match down-head nil t)
    (if org-adapt-indentation
	(org-fixup-indentation "^ " "  " "^\\S-"))))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading)
  (let ((level (outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let ((org-ignore-region t))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (if (and (re-search-forward (concat "^" outline-regexp) nil t)
	       (< (point) end))
	  (funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(defun org-fixup-indentation (from to prohibit)
  "Change the indentation in the current entry by re-replacing FROM with TO.
However, if the regexp PROHIBIT matches at all, don't do anything.
This is being used to change indentation along with the length of the
heading marker.  But if there are any lines which are not indented, nothing
is changed at all."
  (save-excursion
    (let ((end (save-excursion (outline-next-heading)
			       (point-marker))))
      (unless (save-excursion (re-search-forward prohibit end t))
	(while (re-search-forward from end t)
	  (replace-match to)
	  (beginning-of-line 2)))
      (move-marker end nil))))

;;; Vertical tree motion, cutting and pasting of subtrees

(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (org-move-subtree-down (- (prefix-numeric-value arg))))

(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (let ((movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	beg end txt folded)
    ;; Select the tree
    (org-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (outline-end-of-subtree))
    (outline-next-heading)
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (and (funcall movfunc) (looking-at outline-regexp))
	  (progn (goto-char beg)
		 (error "Cannot move past superior level or buffer limit")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (outline-next-heading)
	       (if (not (or (looking-at (concat "^" outline-regexp))
			    (bolp)))
		   (newline))))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (insert txt)
    (goto-char ins-point)
    (if folded (hide-subtree))
    (move-marker ins-point nil)))

(defvar org-subtree-clip ""
  "Clipboard for cut and paste of subtrees.
This is actually only a copy of the kill, because we use the normal kill
ring.  We need it to check if the kill was created by `org-copy-subtree'.")

(defvar org-subtree-clip-folded nil
  "Was the last copied subtree folded?
This is used to fold the tree back after pasting.")

(defun org-cut-subtree ()
  "Cut the current subtree into the clipboard.
This is a short-hand for marking the subtree and then cutting it."
  (interactive)
  (org-copy-subtree 'cut))

(defun org-copy-subtree (&optional cut)
  "Cut the current subtree into the clipboard.
This is a short-hand for marking the subtree and then copying it.
If CUT is non nil, actually cut the subtree."
  (interactive)
  (let (beg end folded)
    (org-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (outline-end-of-subtree))
    (if (equal (char-after) ?\n) (forward-char 1))
    (setq end (point))
    (goto-char beg)
    (when (> end beg)
      (setq org-subtree-clip-folded folded)
      (if cut (kill-region beg end) (copy-region-as-kill beg end))
      (setq org-subtree-clip (current-kill 0))
      (message "%s: Subtree with %d characters"
	       (if cut "Cut" "Copied")
	       (length org-subtree-clip)))))

(defun org-paste-subtree (&optional level tree)
  "Paste the clipboard as a subtree, with modification of headline level.
The entire subtree is promoted or demoted in order to match a new headline
level.  By default, the new level is derived from the visible headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If you want to insert the tree as is, just use \\[yank].

If optional TREE is given, use this text instead of the kill ring."
  (interactive "P")
  (unless (org-kill-is-subtree-p tree)
    (error
     (substitute-command-keys
      "The kill is not a (set of) tree(s) - please use \\[yank] to yank anyway")))
  (let* ((txt (or tree (current-kill 0)))
	 (^re (concat "^\\(" outline-regexp "\\)"))
	 (re  (concat "\\(" outline-regexp "\\)"))
	 (^re_ (concat "\\(" outline-regexp "\\)[  \t]*"))

	 (old-level (if (string-match ^re txt)
			(- (match-end 0) (match-beginning 0))
		      -1))
	 (force-level (cond (level (prefix-numeric-value level))
			    ((string-match
			      ^re_ (buffer-substring (point-at-bol) (point)))
			     (- (match-end 0) (match-beginning 0)))
			    (t nil)))
	 (previous-level (save-excursion
			   (condition-case nil
			       (progn
				 (outline-previous-visible-heading 1)
				 (if (looking-at re)
				     (- (match-end 0) (match-beginning 0))
				   1))
			     (error 1))))
	 (next-level (save-excursion
		       (condition-case nil
			   (progn
			     (outline-next-visible-heading 1)
			     (if (looking-at re)
				 (- (match-end 0) (match-beginning 0))
			       1))
			 (error 1))))
	 (new-level (or force-level (max previous-level next-level)))
	 (shift (if (or (= old-level -1)
			(= new-level -1)
			(= old-level new-level))
		    0
		  (- new-level old-level)))
	 (shift1 shift)
	 (delta (if (> shift 0) -1 1))
	 (func (if (> shift 0) 'org-demote 'org-promote))
	 beg end)
    ;; Remove the forces level indicator
    (if force-level
	(delete-region (point-at-bol) (point)))
    ;; Make sure we start at the beginning of an empty line
    (if (not (bolp)) (insert "\n"))
    (if (not (looking-at "[ \t]*$"))
	(progn (insert "\n") (backward-char 1)))
    ;; Paste
    (setq beg (point))
    (insert txt)
    (setq end (point))
    (goto-char beg)
    ;; Shift if necessary
    (if (= shift 0)
	(message "Pasted at level %d, without shift" new-level)
      (save-restriction
	(narrow-to-region beg end)
	(while (not (= shift 0))
	  (org-map-region func (point-min) (point-max))
	  (setq shift (+ delta shift)))
	(goto-char (point-min))
	(message "Pasted at level %d, with shift by %d levels"
		 new-level shift1)))
    (if (and (eq org-subtree-clip (current-kill 0))
	     org-subtree-clip-folded)
	;; The tree was folded before it was killed/copied
	(hide-subtree))))

(defun org-kill-is-subtree-p (&optional txt)
  "Check if the current kill is an outline subtree, or a set of trees.
Returns nil if kill does not start with a headline, or if the first
headline level is not the largest headline level in the tree.
So this will actually accept several entries of equal levels as well,
which is OK for `org-paste-subtree'.
If optional TXT is given, check this string instead of the current kill."
  (let* ((kill (or txt (current-kill 0) ""))
	 (start-level (and (string-match (concat "\\`" outline-regexp) kill)
			   (- (match-end 0) (match-beginning 0))))
	 (re (concat "^" outline-regexp))
	 (start 1))
    (if (not start-level)
	nil  ;; does not even start with a heading
      (catch 'exit
	(while (setq start (string-match re kill (1+ start)))
	  (if (< (- (match-end 0) (match-beginning 0)) start-level)
	      (throw 'exit nil)))
	t))))

(defun org-archive-subtree ()
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current file, or in
a different file.  The tree will be moved to that location, the subtree
heading be marked DONE, and the current time will be added."
  (interactive)
  ;; Save all relevant TODO keyword-relatex variables
  (let ((tr-org-todo-line-regexp org-todo-line-regexp) ; keep despite compiler
	(tr-org-todo-keywords org-todo-keywords)
	(tr-org-todo-interpretation org-todo-interpretation)
	(tr-org-done-string org-done-string)
	(tr-org-todo-regexp org-todo-regexp)
	(tr-org-todo-line-regexp org-todo-line-regexp)
	(this-buffer (current-buffer))
	file heading buffer level newfile-p)
    (if (string-match "\\(.*\\)::\\(.*\\)" org-archive-location)
	(progn
	  (setq file (format (match-string 1 org-archive-location)
			     (file-name-nondirectory (buffer-file-name)))
		heading (match-string 2 org-archive-location)))
      (error "Invalid `org-archive-location'"))
    (if (> (length file) 0)
	(setq newfile-p (not (file-exists-p file))
	      buffer (find-file-noselect file))
      (setq buffer (current-buffer)))
    (unless buffer
      (error "Cannot access file \"%s\"" file))
    (if (and (> (length heading) 0)
	     (string-match "^\\*+" heading))
	(setq level (match-end 0))
      (setq heading nil level 0))
    (save-excursion
      ;; We first only copy, in case something goes wrong
      ;; we need to protect this-command, to avoid kill-region sets it,
      ;; which would lead to duplication of subtrees
      (let (this-command) (org-copy-subtree))
      (set-buffer buffer)
      ;; Enforce org-mode for the archive buffer
      (if (not (eq major-mode 'org-mode))
	  ;; Force the mode for future visits.
	  (let ((org-insert-mode-line-in-empty-file t))
	    (call-interactively 'org-mode)))
      (when newfile-p
	(goto-char (point-max))
	(insert (format "\nArchived entries from file %s\n\n"
			(buffer-file-name this-buffer))))
      ;; Force the TODO keywords of the original buffer
      (let ((org-todo-line-regexp tr-org-todo-line-regexp)
	    (org-todo-keywords tr-org-todo-keywords)
	    (org-todo-interpretation tr-org-todo-interpretation)
	    (org-done-string tr-org-done-string)
	    (org-todo-regexp tr-org-todo-regexp)
	    (org-todo-line-regexp tr-org-todo-line-regexp))
	(goto-char (point-min))
	(if heading
	    (progn
	      (if (re-search-forward
		   (concat "\\(^\\|\r\\)"
			   (regexp-quote heading) "[ \t]*\\($\\|\r\\)")
		   nil t)
		  (goto-char (match-end 0))
		;; Heading not found, just insert it at the end
		(goto-char (point-max))
		(or (bolp) (insert "\n"))
		(insert "\n" heading "\n")
		(end-of-line 0))
	      ;; Make the heading visible, and the following as well
	      (let ((org-show-following-heading t)) (org-show-hierarchy-above))
	      (if (re-search-forward
		   (concat "^" (regexp-quote (make-string level ?*)) "[ \t]")
		   nil t)
		  (progn (goto-char (match-beginning 0)) (insert "\n")
			 (beginning-of-line 0))
		(goto-char (point-max)) (insert "\n")))
	  (goto-char (point-max)) (insert "\n"))
	;; Paste
	(org-paste-subtree (1+ level))
	;; Mark the entry as done, i.e. set to last work in org-todo-keywords
	(if org-archive-mark-done
	    (org-todo (length org-todo-keywords)))
	;; Move cursor to right after the TODO keyword
	(when org-archive-stamp-time
	  (beginning-of-line 1)
	  (looking-at org-todo-line-regexp)
	  (goto-char (or (match-end 2) (match-beginning 3)))
	  (insert "(" (format-time-string (cdr org-time-stamp-formats)
					  (current-time))
		  ")"))
	;; Save the buffer, if it is not the same buffer.
	(if (not (eq this-buffer buffer)) (save-buffer))))
    ;; Here we are back in the original buffer.  Everything seems to have
    ;; worked.  So now cut the tree and finish up.
    (let (this-command) (org-cut-subtree))
    (if (looking-at "[ \t]*$") (kill-line))
    (message "Subtree archived %s"
	     (if (eq this-buffer buffer)
		 (concat "under heading: " heading)
	       (concat "in file: " (abbreviate-file-name file))))))

;;; Completion

(defun org-complete (&optional arg)
  "Perform completion on word at point.
At the beginning of a headline, this completes TODO keywords as given in
`org-todo-keywords'.
If the current word is preceded by a backslash, completes the TeX symbols
that are supported for HTML support.
If the current word is preceded by \"#+\", completes special words for
setting file options.
At all other locations, this simply calls `ispell-complete-word'."
  (interactive "P")
  (catch 'exit
    (let* ((end (point))
	   (beg (save-excursion
		  (if (equal (char-before (point)) ?\ ) (backward-char 1))
		  (skip-chars-backward "a-zA-Z0-9_:$")
		  (point)))
	   (texp (equal (char-before beg) ?\\))
	   (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
					 beg)
		       "#+"))
	   (pattern (buffer-substring-no-properties beg end))
	   (completion-ignore-case opt)
	   (type nil)
	   (table (cond
		   (opt
		    (setq type :opt)
		    (mapcar (lambda (x)
			      (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
			      (cons (match-string 2 x) (match-string 1 x)))
			    (org-split-string (org-get-current-options) "\n")))
		   (texp
		    (setq type :tex)
		    org-html-entities)
		   ((string-match "\\`\\*+[ \t]*\\'"
				  (buffer-substring (point-at-bol) beg))
		    (setq type :todo)
		    (mapcar 'list org-todo-keywords))
		   (t (progn (ispell-complete-word arg) (throw 'exit nil)))))
	   (completion (try-completion pattern table)))
      (cond ((eq completion t)
	     (if (equal type :opt)
		 (insert (substring (cdr (assoc (upcase pattern) table))
				    (length pattern)))))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region beg end)
	     (if (string-match " +$" completion)
		 (setq completion (replace-match "" t t completion)))
	     (insert completion)
	     (if (get-buffer-window "*Completions*")
		 (delete-window (get-buffer-window "*Completions*")))
	     (if (and (eq type :todo)
		      (assoc completion table))
		 (insert " "))
	     (if (and (equal type :opt) (assoc completion table))
		 (message "%s" (substitute-command-keys
				"Press \\[org-complete] again to insert example settings"))))
	    (t
	     (message "Making completion list...")
	     (let ((list (sort (all-completions pattern table) 'string<)))
	       (with-output-to-temp-buffer "*Completions*"
		 (display-completion-list list pattern)))
	     (message "Making completion list...%s" "done"))))))

;;; Comments, TODO and DEADLINE

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (looking-at (concat outline-regexp
			    "\\( +\\<" org-comment-string "\\>\\)"))
	(replace-match "" t t nil 1)
      (if (looking-at outline-regexp)
	  (progn
	    (goto-char (match-end 0))
	    (insert " " org-comment-string))))))

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defun org-todo (&optional arg)
  "Change the TODO state of an item.
The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.  By
default the available states are \"TODO\" and \"DONE\".
So for this example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.

With prefix arg, use completion to determined the new state.  With numeric
prefix arg, switch to that state."
  (interactive "P")
  (save-excursion
    (org-back-to-heading)
    (if (looking-at outline-regexp) (goto-char (match-end 0)))
    (or (looking-at (concat " +" org-todo-regexp " *"))
	(looking-at " *"))
    (let* ((this (match-string 1))
	   (completion-ignore-case t)
	   (member (member this org-todo-keywords))
	   (tail (cdr member))
	   (state (cond
		  ((equal arg '(4))
		   ;; Read a state with completion
		   (completing-read "State: " (mapcar (lambda(x) (list x))
						      org-todo-keywords)
				    nil t))
		  (arg
		   ;; user requests a specific state
		   (nth (1- (prefix-numeric-value arg))
			org-todo-keywords))
		  ((null member) (car org-todo-keywords))
		  ((null tail) nil) ;; -> first entry
		  ((eq org-todo-interpretation 'sequence)
		   (car tail))
		  ((memq org-todo-interpretation '(type priority))
		   (if (eq this-command last-command)
		       (car tail)
		     (if (> (length tail) 0) org-done-string nil)))
		  (t nil)))
	   (next (if state (concat " " state " ") " ")))
      (replace-match next t t)
      (setq org-last-todo-state-is-todo
	    (not (equal state org-done-string)))
      (run-hooks 'org-after-todo-state-change-hook)))
  ;; Fixup cursor location if close to the keyword
  (if (and (outline-on-heading-p)
	   (not (bolp))
	   (save-excursion (beginning-of-line 1)
			   (looking-at org-todo-line-regexp))
	   (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
      (progn
	(goto-char (or (match-end 2) (match-end 1)))
	(just-one-space))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match."
  (interactive "P")
  (let ((case-fold-search nil)
	(kwd-re (if arg org-todo-regexp org-not-done-regexp)))
    (message "%d TODO entries found"
	     (org-occur (concat "^" outline-regexp " +" kwd-re )))))

(defun org-deadline ()
  "Insert the DEADLINE: string to make a deadline.
A timestamp is also inserted - use \\[org-timestamp-up] and \\[org-timestamp-down]
to modify it to the correct date."
  (interactive)
  (insert
   org-deadline-string " "
   (format-time-string (car org-time-stamp-formats)
		       (org-read-date nil 'to-time)))
  (message "%s" (substitute-command-keys
		 "Use \\[org-timestamp-up-day] and \\[org-timestamp-down-day] to change the date.")))

(defun org-schedule ()
  "Insert the SCHEDULED: string to schedule a TODO item.
A timestamp is also inserted - use \\[org-timestamp-up] and \\[org-timestamp-down]
to modify it to the correct date."
  (interactive)
  (insert
   org-scheduled-string " "
   (format-time-string (car org-time-stamp-formats)
		       (org-read-date nil 'to-time)))
  (message "%s" (substitute-command-keys
		 "Use \\[org-timestamp-up-day] and \\[org-timestamp-down-day] to change the date.")))


(defun org-occur (regexp &optional callback)
  "Make a compact tree which shows all matches of REGEXP.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.  It will also show the heading after the match,
to make sure editing the matching entry is easy.
if CALLBACK is non-nil, it is a function which is called to confirm
that the match should indeed be shown."
  (interactive "sRegexp: ")
  (setq regexp (org-check-occur-regexp regexp))
  (let ((cnt 0))
    (save-excursion
      (goto-char (point-min))
      (hide-sublevels 1)
      (while (re-search-forward regexp nil t)
	(when (or (not callback)
		  (funcall callback))
	  (setq cnt (1+ cnt))
	  (org-show-hierarchy-above))))
    (run-hooks 'org-occur-hook)
    (if (interactive-p)
	(message "%d match(es) for regexp %s" cnt regexp))
    cnt))

(defun org-show-hierarchy-above ()
  "Make sure point and the headings hierarchy above is visible."
  (if (org-on-heading-p t)
      (org-flag-heading nil)    ; only show the heading
    (org-show-hidden-entry))    ; show entire entry
  (save-excursion
    (and org-show-following-heading
	 (outline-next-heading)
	 (org-flag-heading nil)))  ; show the next heading
  (save-excursion                  ; show all higher headings
    (while (condition-case nil
	       (progn (org-up-heading-all 1) t)
	     (error nil))
      (org-flag-heading nil))))

;;; Priorities

(defvar org-priority-regexp ".*?\\(\\[#\\([A-Z]\\)\\] ?\\)"
  "Regular expression matching the priority indicator.")

(defvar org-remove-priority-next-time nil)

(defun org-priority-up ()
  "Increase the priority of the current item."
  (interactive)
  (org-priority 'up))

(defun org-priority-down ()
  "Decrease the priority of the current item."
  (interactive)
  (org-priority 'down))

(defun org-priority (&optional action)
  "Change the priority of an item by ARG.
ACTION can be set, up, or down."
  (interactive)
  (setq action (or action 'set))
  (let (current new news have remove)
    (save-excursion
      (org-back-to-heading)
      (if (looking-at org-priority-regexp)
	  (setq current (string-to-char (match-string 2))
		have t)
	(setq current org-default-priority))
      (cond
       ((eq action 'set)
	(message "Priority A-%c, SPC to remove: " org-lowest-priority)
	(setq new (read-char-exclusive))
	(cond ((equal new ?\ ) (setq remove t))
	      ((or (< (upcase new) ?A) (> (upcase new) org-lowest-priority))
	       (error "Priority must be between `%c' and `%c'"
		      ?A org-lowest-priority))))
       ((eq action 'up)
	(setq new (1- current)))
       ((eq action 'down)
	(setq new (1+ current)))
       (t (error "Invalid action")))
      (setq new (min (max ?A (upcase new)) org-lowest-priority))
      (setq news (format "%c" new))
      (if have
	  (if remove
	      (replace-match "" t t nil 1)
	    (replace-match news t t nil 2))
	(if remove
	    (error "No priority cookie found in line")
	  (looking-at org-todo-line-regexp)
	  (if (match-end 2)
	      (progn
		(goto-char (match-end 2))
		(insert " [#" news "]"))
	    (goto-char (match-beginning 3))
	    (insert "[#" news "] ")))))
    (if remove
	(message "Priority removed")
      (message "Priority of current item set to %s" news))))


(defun org-get-priority (s)
  "Find priority cookie and return priority."
  (save-match-data
    (if (not (string-match org-priority-regexp s))
	(* 1000 (- org-lowest-priority org-default-priority))
      (* 1000 (- org-lowest-priority
		 (string-to-char (match-string 2 s)))))))

;;; Timestamps

(defvar org-last-changed-timestamp nil)

(defun org-time-stamp (arg)
  "Prompt for a date/time and insert a time stamp.
If the user specifies a time like HH:MM, or if this command is called
with a prefix argument, the time stamp will contain date and time.
Otherwise, only the date will be included.  All parts of a date not
specified by the user will be filled in from the current date/time.
So if you press just return without typing anything, the time stamp
will represent the current date/time.  If there is already a timestamp
at the cursor, it will be modified."
  (interactive "P")
  (let ((fmt (if arg (cdr org-time-stamp-formats)
	       (car org-time-stamp-formats)))
	(org-time-was-given nil)
	time)
    (cond
     ((and (org-at-timestamp-p)
	   (eq last-command 'org-time-stamp)
	   (eq this-command 'org-time-stamp))
      (insert "--")
      (setq time (let ((this-command this-command))
		  (org-read-date arg 'totime)))
      (if org-time-was-given (setq fmt (cdr org-time-stamp-formats)))
      (insert (format-time-string fmt time)))
     ((org-at-timestamp-p)
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime)))
      (and (org-at-timestamp-p) (replace-match
				 (setq org-last-changed-timestamp
				       (format-time-string fmt time))
				 t t))
      (message "Timestamp updated"))
     (t
      (setq time (let ((this-command this-command))
		  (org-read-date arg 'totime)))
      (if org-time-was-given (setq fmt (cdr org-time-stamp-formats)))
      (insert (format-time-string fmt time))))))

(defun org-time-stamp-inactive (&optional arg)
  "Insert an inactive time stamp.
An inactive time stamp is enclosed in square brackets instead of angle
brackets.  It is inactive in the sense that it does not trigger agenda entries,
does not link to the calendar and cannot be changed with the S-cursor keys."
  (interactive "P")
  (let ((fmt (if arg (cdr org-time-stamp-formats)
	       (car org-time-stamp-formats)))
	(org-time-was-given nil)
	time)
    (setq time (org-read-date arg 'totime))
    (if org-time-was-given (setq fmt (cdr org-time-stamp-formats)))
    (setq fmt (concat "[" (substring fmt 1 -1) "]"))
    (insert (format-time-string fmt time))))

;;; FIXME: Make the function take "Fri" as "next friday"
(defun org-read-date (&optional with-time to-time)
  "Read a date and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month ,year,
hour and minute.  For example,
  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  etc.
The function understands only English month and weekday abbreviations,
but this can be configured with the variables `parse-time-months' and
`parse-time-weekdays'.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
month.  To scroll it to other months, use the keys `>' and `<'.
If you don't like the calendar, turn it off with
       \(setq org-popup-calendar-for-date-prompt nil).

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to also
insert a time.  Note that when WITH-TIME is not set, you can still
enter a time, and this function will inform the calling routine about
this change.  The calling routine may then choose to change the format
used to insert the time stamp into the buffer to include the time."
  (let* ((default-time
	   ;; Default time is either today, or, when entering a range,
	   ;; the range start.
	   (if (save-excursion
		 (re-search-backward
		  (concat org-ts-regexp "--\\=")
		  (- (point) 20) t))
	       (apply
		'encode-time
		(mapcar (lambda(x) (or x 0))  ;; FIXME: Problem with timezone?
			(parse-time-string (match-string 1))))
	     (current-time)))
	 (timestr (format-time-string
		   (if with-time "%Y-%m-%d %H:%M" "%Y-%m-%d") default-time))
	 (prompt (format "YYYY-MM-DD [%s]: " timestr))
	 ans ans1 ans2
	 second minute hour day month year tl)

    (if org-popup-calendar-for-date-prompt
	;; Also show a calendar for date selection
	;; Copied (with modifications) from planner.el by John Wiegley
	(save-excursion
	  (save-window-excursion
	    (calendar)
	    (calendar-forward-day (- (time-to-days default-time)
				     (calendar-absolute-from-gregorian
				      (calendar-current-date))))
	    (let* ((old-map (current-local-map))
		   (map (copy-keymap calendar-mode-map))
		   (minibuffer-local-map (copy-keymap minibuffer-local-map)))
	      (define-key map (kbd "RET") 'org-calendar-select)
	      (define-key map (if org-xemacs-p [button1] [mouse-1])
		'org-calendar-select)
	      (define-key minibuffer-local-map [(meta shift left)]
		(lambda () (interactive)
		  (org-eval-in-calendar '(calendar-backward-month 1))))
	      (define-key minibuffer-local-map [(meta shift right)]
		(lambda () (interactive)
		  (org-eval-in-calendar '(calendar-forward-month 1))))
	      (define-key minibuffer-local-map [(shift up)]
		(lambda () (interactive)
		  (org-eval-in-calendar '(calendar-backward-week 1))))
	      (define-key minibuffer-local-map [(shift down)]
		(lambda () (interactive)
		  (org-eval-in-calendar '(calendar-forward-week 1))))
	      (define-key minibuffer-local-map [(shift left)]
		(lambda () (interactive)
		  (org-eval-in-calendar '(calendar-backward-day 1))))
	      (define-key minibuffer-local-map [(shift right)]
		(lambda () (interactive)
		  (org-eval-in-calendar '(calendar-forward-day 1))))
	      (define-key minibuffer-local-map ">"
		(lambda () (interactive)
		  (org-eval-in-calendar '(scroll-calendar-left 1))))
	      (define-key minibuffer-local-map "<"
		(lambda () (interactive)
		  (org-eval-in-calendar '(scroll-calendar-right 1))))
	      (unwind-protect
		  (progn
		    (use-local-map map)
		    (setq ans (read-string prompt "" nil nil))
		    (setq ans (or ans1 ans2 ans)))
		(use-local-map old-map)))))
      ;; Naked prompt only
      (setq ans (read-string prompt "" nil timestr)))

    (if (string-match
	 "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
	(progn
	  (setq year (if (match-end 2)
			 (string-to-number (match-string 2 ans))
		       (string-to-number (format-time-string "%Y")))
		month (string-to-number (match-string 3 ans))
		day (string-to-number (match-string 4 ans)))
	  (if (< year 100) (setq year (+ 2000 year)))
	  (setq ans (replace-match (format "%04d-%02d-%02d" year month day)
				   t t ans))))
    (setq tl (parse-time-string ans)
	  year (or (nth 5 tl) (string-to-number (format-time-string "%Y")))
	  month (or (nth 4 tl) (string-to-number (format-time-string "%m")))
	  day (or (nth 3 tl) (string-to-number (format-time-string "%d")))
	  hour (or (nth 2 tl) (string-to-number (format-time-string "%H")))
	  minute (or (nth 1 tl) (string-to-number (format-time-string "%M")))
	  second (or (nth 0 tl) 0))
    (if (and (boundp 'org-time-was-given)
	     (nth 2 tl))
	(setq org-time-was-given t))
    (if (< year 100) (setq year (+ 2000 year)))
    (if to-time
	(encode-time second minute hour day month year)
      (if (or (nth 1 tl) (nth 2 tl))
	  (format "%04d-%02d-%02d %02d:%02d" year month day hour minute)
	(format "%04d-%02d-%02d" year month day)))))

(defun org-eval-in-calendar (form)
  "Eval FORM in the calendar window and return to current window.
Also, store the cursor date in variable ans2."
  (let ((sw (selected-window)))
    (select-window (get-buffer-window "*Calendar*"))
    (eval form)
    (when (calendar-cursor-to-date)
      (let* ((date (calendar-cursor-to-date))
	     (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	(setq ans2 (format-time-string "%Y-%m-%d" time))))
    (select-window sw)))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq ans1 (format-time-string "%Y-%m-%d" time)))
    (if (active-minibuffer-window) (exit-minibuffer))))

(defun org-check-deadlines (ndays)
  "Check if there are any deadlines due or past due.
A deadline is considered due if it happens within `org-deadline-warning-days'
days from today's date.  If the deadline appears in an entry marked DONE,
it is not shown.  The prefix arg NDAYS can be used to test that many
days.  If the prefix is a raw \\[universal-argument] prefix, all deadlines are shown."
  (interactive "P")
  (let* ((org-warn-days
	  (cond
	   ((equal ndays '(4)) 100000)
	   (ndays (prefix-numeric-value ndays))
	   (t org-deadline-warning-days)))
	 (case-fold-search nil)
	 (regexp (concat "\\<" org-deadline-string " *<\\([^>]+\\)>"))
	 (callback
	  (lambda ()
	    (and (let ((d1 (time-to-days (current-time)))
		       (d2 (time-to-days
			    (org-time-string-to-time (match-string 1)))))
		   (< (- d2 d1) org-warn-days))
		 (not (org-entry-is-done-p))))))
    (message "%d deadlines past-due or due within %d days"
	     (org-occur regexp callback)
	     org-warn-days)))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (save-excursion
    (unless (org-at-date-range-p)
      (goto-char (point-at-bol))
      (re-search-forward org-tr-regexp (point-at-eol) t))
    (if (not (org-at-date-range-p))
	(error "Not at a time-stamp range, and none found in current line")))
  (let* ((ts1 (match-string 1))
	 (ts2 (match-string 2))
	 (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	 (match-end (match-end 0))
	 (time1 (org-time-string-to-time ts1))
	 (time2 (org-time-string-to-time ts2))
	 (t1 (time-to-seconds time1))
	 (t2 (time-to-seconds time2))
	 (diff (abs (- t2 t1)))
	 (negative (< (- t2 t1) 0))
	 ;; (ys (floor (* 365 24 60 60)))
	 (ds (* 24 60 60))
	 (hs (* 60 60))
	 (fy "%dy %dd %02d:%02d")
	 (fy1 "%dy %dd")
	 (fd "%dd %02d:%02d")
	 (fd1 "%dd")
	 (fh "%02d:%02d")
	 y d h m align)
    ;; FIXME:  Should I re-introduce years, make year refer to same date?
    ;; This would be the only useful way to have years, actually.
    (if havetime
	(setq ; y (floor (/ diff ys))  diff (mod diff ys)
	 y 0
	 d (floor (/ diff ds))  diff (mod diff ds)
	 h (floor (/ diff hs))  diff (mod diff hs)
	 m (floor (/ diff 60)))
      (setq ; y (floor (/ diff ys))  diff (mod diff ys)
       y 0
       d (floor (+ (/ diff ds) 0.5))
       h 0 m 0))
    (if (not to-buffer)
	(message (org-make-tdiff-string y d h m))
      (when (org-at-table-p)
	(goto-char match-end)
	(setq align t)
	(and (looking-at " *|") (goto-char (match-end 0))))
      (if (looking-at
	   "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	  (replace-match ""))
      (if negative (insert " -"))
      (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	(if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	  (insert " " (format fh h m))))
      (if align (org-table-align))
      (message "Time difference inserted"))))

(defun org-make-tdiff-string (y d h m)
  (let ((fmt "")
	(l nil))
    (if (> y 0) (setq fmt (concat fmt "%d year" (if (> y 1) "s" "") " ")
		      l (push y l)))
    (if (> d 0) (setq fmt (concat fmt "%d day"  (if (> d 1) "s" "") " ")
		      l (push d l)))
    (if (> h 0) (setq fmt (concat fmt "%d hour" (if (> h 1) "s" "") " ")
		      l (push h l)))
    (if (> m 0) (setq fmt (concat fmt "%d minute" (if (> m 1) "s" "") " ")
		      l (push m l)))
    (apply 'format fmt (nreverse l))))

(defun org-time-string-to-time (s)
  (apply 'encode-time (org-parse-time-string s)))

(defun org-parse-time-string (s &optional nodefault)
  "Parse the standard Org-mode time string.
This should be a lot faster than the normal `parse-time-string'.
If time is not given, defaults to 0:00.  However, with optional NODEFAULT,
hour and minute fields will be nil if not given."
  (if (string-match org-ts-regexp1 s)
      (list 0
	    (if (or (match-beginning 8) (not nodefault))
		(string-to-number (or (match-string 8 s) "0")))
	    (if (or (match-beginning 7) (not nodefault))
		(string-to-number (or (match-string 7 s) "0")))
	    (string-to-number (match-string 4 s))
	    (string-to-number (match-string 3 s))
	    (string-to-number (match-string 2 s))
	    nil nil nil)
    (make-list 9 0)))

(defun org-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month or
the day, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg)))

(defun org-timestamp-down (&optional arg)
  "Decrease the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month or
the day, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg))))

(defun org-timestamp-up-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg) 'day))

(defun org-timestamp-down-day (&optional arg)
  "Decrease the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg)) 'day))

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun org-at-timestamp-p ()
  "Determine if the cursor is  or at a timestamp."
  (interactive)
  (let* ((tsr org-ts-regexp2)
	 (pos (point))
	 (ans (or (looking-at tsr)
		  (save-excursion
		    (skip-chars-backward "^<\n\r\t")
		    (if (> (point) 1) (backward-char 1))
		    (and (looking-at tsr)
			 (> (- (match-end 0) pos) -1))))))
    (and (boundp 'org-ts-what)
	 (setq org-ts-what
	      (cond
	       ((org-pos-in-match-range pos 2)      'year)
	       ((org-pos-in-match-range pos 3)      'month)
	       ((org-pos-in-match-range pos 7)      'hour)
	       ((org-pos-in-match-range pos 8)      'minute)
	       ((or (org-pos-in-match-range pos 4)
		    (org-pos-in-match-range pos 5)) 'day)
	       (t 'day))))
    ans))

(defun org-timestamp-change (n &optional what)
  "Change the date in the time stamp at point.
The date will be changed by N times WHAT.  WHAT can be `day', `month',
`year', `minute', `second'.  If WHAT is not given, the cursor position
in the timestamp determines what will be changed."
  (let ((fmt (car org-time-stamp-formats))
	org-ts-what
	(pos (point))
	ts time time0)
    (if (not (org-at-timestamp-p))
	(error "Not at a timestamp"))
    (setq org-ts-what (or what org-ts-what))
    (setq fmt (if (<= (abs (- (cdr org-ts-lengths)
			      (- (match-end 0) (match-beginning 0))))
		      1)
		  (cdr org-time-stamp-formats)
		(car org-time-stamp-formats)))
    (setq ts (match-string 0))
    (replace-match "")
    (setq time0 (org-parse-time-string ts))
    (setq time
	  (apply 'encode-time
		 (append
		  (list (or (car time0) 0))
		  (list (+ (if (eq org-ts-what 'minute) n 0) (nth 1 time0)))
		  (list (+ (if (eq org-ts-what 'hour) n 0)   (nth 2 time0)))
		  (list (+ (if (eq org-ts-what 'day) n 0)    (nth 3 time0)))
		  (list (+ (if (eq org-ts-what 'month) n 0)  (nth 4 time0)))
		  (list (+ (if (eq org-ts-what 'year) n 0)   (nth 5 time0)))
		  (nthcdr 6 time0))))
    (if (eq what 'calendar)
	(let ((cal-date
	       (save-excursion
		 (save-match-data
		   (set-buffer "*Calendar*")
		   (calendar-cursor-to-date)))))
	  (setcar (nthcdr 4 time0) (nth 0 cal-date)) ; month
	  (setcar (nthcdr 3 time0) (nth 1 cal-date)) ; day
	  (setcar (nthcdr 5 time0) (nth 2 cal-date)) ; year
	  (setcar time0 (or (car time0) 0))
	  (setcar (nthcdr 1 time0) (or (nth 1 time0) 0))
	  (setcar (nthcdr 2 time0) (or (nth 1 time0) 0))
	  (setq time (apply 'encode-time time0))))
    (insert (setq org-last-changed-timestamp (format-time-string fmt time)))
    (goto-char pos)
    ;; Try to recenter the calendar window, if any
    (if (and org-calendar-follow-timestamp-change
	     (get-buffer-window "*Calendar*" t)
	     (memq org-ts-what '(day month year)))
	(org-recenter-calendar (time-to-days time)))))

(defun org-recenter-calendar (date)
  "If the calendar is visible, recenter it to DATE."
  (let* ((win (selected-window))
	 (cwin (get-buffer-window "*Calendar*" t)))
    (when cwin
      (select-window cwin)
      (calendar-goto-date (if (listp date) date
			    (calendar-gregorian-from-absolute date)))
      (select-window win))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used force the current date."
  (interactive "P")
  (let ((tsr org-ts-regexp) diff)
    (if (or (org-at-timestamp-p)
	    (save-excursion
	      (beginning-of-line 1)
	      (looking-at (concat ".*" tsr))))
	(let ((d1 (time-to-days (current-time)))
	      (d2 (time-to-days
		   (org-time-string-to-time (match-string 1)))))
	  (setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (if (and diff (not arg)) (calendar-forward-day diff))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (org-timestamp-change 0 'calendar))

;;; Agenda, and Diary Integration

;;; Define the mode

(defvar org-agenda-mode-map (make-sparse-keymap)
  "Keymap for `org-agenda-mode'.")

(defvar org-agenda-menu)
(defvar org-agenda-follow-mode nil)
(defvar org-agenda-buffer-name "*Org Agenda*")
(defvar org-agenda-redo-command nil)
(defvar org-agenda-mode-hook nil)

;;;###autoload
(defun org-agenda-mode ()
  "Mode for time-sorted view on action items in Org-mode files.

The following commands are available:

\\{org-agenda-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'org-agenda-mode)
  (setq mode-name "Org-Agenda")
  (use-local-map org-agenda-mode-map)
  (easy-menu-add org-agenda-menu)
  (if org-startup-truncated (setq truncate-lines t))
  (add-hook 'post-command-hook 'org-agenda-post-command-hook nil 'local)
  (add-hook 'pre-command-hook 'org-unhighlight nil 'local)
  (setq org-agenda-follow-mode nil)
  (easy-menu-change
   '("Agenda") "Agenda Files"
   (append
    (list
     ["Edit File List" (customize-variable 'org-agenda-files) t]
     "--")
   (mapcar 'org-file-menu-entry org-agenda-files)))
  (org-agenda-set-mode-name)
  (apply
   (if (fboundp 'run-mode-hooks) 'run-mode-hooks 'run-hooks)
   org-agenda-mode-hook))

(define-key org-agenda-mode-map "\C-i"     'org-agenda-goto)
(define-key org-agenda-mode-map "\C-m"     'org-agenda-switch-to)
(define-key org-agenda-mode-map " "        'org-agenda-show)
(define-key org-agenda-mode-map "\C-c\C-t" 'org-agenda-todo)
(define-key org-agenda-mode-map "o"        'delete-other-windows)
(define-key org-agenda-mode-map "l"        'org-agenda-recenter)
(define-key org-agenda-mode-map "t"        'org-agenda-todo)
(define-key org-agenda-mode-map "."        'org-agenda-goto-today)
(define-key org-agenda-mode-map "d"        'org-agenda-day-view)
(define-key org-agenda-mode-map "w"        'org-agenda-week-view)
(define-key org-agenda-mode-map (org-key 'S-right) 'org-agenda-date-later)
(define-key org-agenda-mode-map (org-key 'S-left) 'org-agenda-date-earlier)
(define-key org-agenda-mode-map [?\C-c ?\C-x (right)] 'org-agenda-date-later)
(define-key org-agenda-mode-map [?\C-c ?\C-x (left)] 'org-agenda-date-earlier)

(define-key org-agenda-mode-map ">" 'org-agenda-date-prompt)
(let ((l '(1 2 3 4 5 6 7 8 9 0)))
  (while l (define-key org-agenda-mode-map
	     (int-to-string (pop l)) 'digit-argument)))

(define-key org-agenda-mode-map "f" 'org-agenda-follow-mode)
(define-key org-agenda-mode-map "D" 'org-agenda-toggle-diary)
(define-key org-agenda-mode-map "g" 'org-agenda-toggle-time-grid)
(define-key org-agenda-mode-map "r" 'org-agenda-redo)
(define-key org-agenda-mode-map "q" 'org-agenda-quit)
(define-key org-agenda-mode-map "x" 'org-agenda-exit)
(define-key org-agenda-mode-map "P" 'org-agenda-show-priority)
(define-key org-agenda-mode-map "n" 'next-line)
(define-key org-agenda-mode-map "p" 'previous-line)
(define-key org-agenda-mode-map "\C-n" 'org-agenda-next-date-line)
(define-key org-agenda-mode-map "\C-p" 'org-agenda-previous-date-line)
(define-key org-agenda-mode-map "," 'org-agenda-priority)
(define-key org-agenda-mode-map "\C-c," 'org-agenda-priority)
(define-key org-agenda-mode-map "i" 'org-agenda-diary-entry)
(define-key org-agenda-mode-map "c" 'org-agenda-goto-calendar)
(eval-after-load "calendar"
  '(define-key calendar-mode-map org-calendar-to-agenda-key
     'org-calendar-goto-agenda))
(define-key org-agenda-mode-map "C" 'org-agenda-convert-date)
(define-key org-agenda-mode-map "m" 'org-agenda-phases-of-moon)
(define-key org-agenda-mode-map "M" 'org-agenda-phases-of-moon)
(define-key org-agenda-mode-map "s" 'org-agenda-sunrise-sunset)
(define-key org-agenda-mode-map "S" 'org-agenda-sunrise-sunset)
(define-key org-agenda-mode-map "h" 'org-agenda-holidays)
(define-key org-agenda-mode-map "H" 'org-agenda-holidays)
(define-key org-agenda-mode-map "+" 'org-agenda-priority-up)
(define-key org-agenda-mode-map "-" 'org-agenda-priority-down)
(define-key org-agenda-mode-map (org-key 'S-up) 'org-agenda-priority-up)
(define-key org-agenda-mode-map (org-key 'S-down) 'org-agenda-priority-down)
(define-key org-agenda-mode-map [?\C-c ?\C-x (up)] 'org-agenda-priority-up)
(define-key org-agenda-mode-map [?\C-c ?\C-x (down)] 'org-agenda-priority-down)
(define-key org-agenda-mode-map [(right)] 'org-agenda-later)
(define-key org-agenda-mode-map [(left)] 'org-agenda-earlier)

(defvar org-agenda-keymap (copy-keymap org-agenda-mode-map)
  "Local keymap for agenda entries from Org-mode.")

(define-key org-agenda-keymap
  (if org-xemacs-p [(button2)] [(mouse-2)]) 'org-agenda-goto-mouse)
(define-key org-agenda-keymap
  (if org-xemacs-p [(button3)] [(mouse-3)]) 'org-agenda-show-mouse)

(easy-menu-define org-agenda-menu org-agenda-mode-map "Agenda menu"
  '("Agenda"
    ("Agenda Files")
    "--"
    ["Show" org-agenda-show t]
    ["Go To (other window)" org-agenda-goto t]
    ["Go To (one window)" org-agenda-switch-to t]
    ["Follow Mode" org-agenda-follow-mode
     :style toggle :selected org-agenda-follow-mode :active t]
    "--"
    ["Cycle TODO" org-agenda-todo t]
    ("Reschedule"
     ["Reschedule +1 day" org-agenda-date-later t]
     ["Reschedule -1 day" org-agenda-date-earlier t]
     "--"
     ["Reschedule to ..." org-agenda-date-prompt t])
    ("Priority"
     ["Set Priority" org-agenda-priority t]
     ["Increase Priority" org-agenda-priority-up t]
     ["Decrease Priority" org-agenda-priority-down t]
     ["Show Priority" org-agenda-show-priority t])
    "--"
    ["Rebuild buffer" org-agenda-redo t]
    ["Goto Today" org-agenda-goto-today t]
    ["Next Dates" org-agenda-later (local-variable-p 'starting-day)]
    ["Previous Dates" org-agenda-earlier (local-variable-p 'starting-day)]
    "--"
    ["Day View" org-agenda-day-view :active (local-variable-p 'starting-day)
     :style radio :selected (equal org-agenda-ndays 1)]
    ["Week View" org-agenda-week-view :active (local-variable-p 'starting-day)
     :style radio :selected (equal org-agenda-ndays 7)]
    "--"
    ["Include Diary" org-agenda-toggle-diary
     :style toggle :selected org-agenda-include-diary :active t]
    ["Use Time Grid" org-agenda-toggle-time-grid
     :style toggle :selected org-agenda-use-time-grid :active t]
    "--"
    ["New Diary Entry" org-agenda-diary-entry t]
    ("Calendar Commands"
     ["Goto Calendar" org-agenda-goto-calendar t]
     ["Phases of the Moon" org-agenda-phases-of-moon t]
     ["Sunrise/Sunset" org-agenda-sunrise-sunset t]
     ["Holidays" org-agenda-holidays t]
     ["Convert" org-agenda-convert-date t])
    "--"
    ["Quit" org-agenda-quit t]
    ["Exit and Release Buffers" org-agenda-exit t]
    ))

(defvar org-agenda-markers nil
  "List of all currently active markers created by `org-agenda'.")
(defvar org-agenda-last-marker-time (time-to-seconds (current-time))
  "Creation time of the last agenda marker.")

(defun org-agenda-new-marker (&optional pos)
  "Return a new agenda marker.
Org-mode keeps a list of these markers and resets them when they are
no longer in use."
  (let ((m (copy-marker (or pos (point)))))
    (setq org-agenda-last-marker-time (time-to-seconds (current-time)))
    (push m org-agenda-markers)
    m))

(defun org-agenda-maybe-reset-markers (&optional force)
  "Reset markers created by `org-agenda'.  But only if they are old enough."
  (if (or force
	  (> (- (time-to-seconds (current-time))
		org-agenda-last-marker-time)
	     5))
      (while org-agenda-markers
	(move-marker (pop org-agenda-markers) nil))))

(defvar org-agenda-new-buffers nil
  "Buffers created to visit agenda files.")

(defun org-get-agenda-file-buffer (file)
  "Get a buffer visiting FILE.  If the buffer needs to be created, add
it to the list of buffers which might be released later."
  (let ((buf (find-buffer-visiting file)))
    (if buf
	buf  ; just return it
      ;; Make a new buffer and remember it
      (setq buf (find-file-noselect file))
      (if buf (push buf org-agenda-new-buffers))
      buf)))

(defun org-release-buffers (blist)
  "Release all buffers in list, asking the user for confirmation when needed.
When a buffer is unmodified, it is just killed.  When modified, it is saved
\(if the user agrees) and then killed."
  (let (buf file)
    (while (setq buf (pop blist))
      (setq file (buffer-file-name buf))
      (when (and (buffer-modified-p buf)
		 file
		 (y-or-n-p (format "Save file %s? " file)))
	(with-current-buffer buf (save-buffer)))
      (kill-buffer buf))))

(defvar org-respect-restriction nil)    ; Dynamically-scoped param.

(defun org-timeline (&optional include-all)
  "Show a time-sorted view of the entries in the current org file.
Only entries with a time stamp of today or later will be listed.  With
one \\[universal-argument] prefix argument, past entries will also be listed.
With two \\[universal-argument] prefixes, all unfinished TODO items will also be shown,
under the current date.
If the buffer contains an active region, only check the region for
dates."
  (interactive "P")
  (require 'calendar)
  (org-agenda-maybe-reset-markers 'force)
  (org-compile-prefix-format org-timeline-prefix-format)
  (let* ((dopast include-all)
	 (dotodo (equal include-all '(16)))
	 (entry (buffer-file-name))
	 (org-agenda-files (list (buffer-file-name)))
	 (date (calendar-current-date))
	 (win (selected-window))
	 (pos1 (point))
	 (beg (if (org-region-active-p) (region-beginning) (point-min)))
	 (end (if (org-region-active-p) (region-end) (point-max)))
	 (day-numbers (org-get-all-dates beg end 'no-ranges
					 t)) ; always include today
	 (today (time-to-days (current-time)))
	 (org-respect-restriction t)
	 (past t)
	 s e rtn d)
    (setq org-agenda-redo-command
	  (list 'progn
		(list 'switch-to-buffer-other-window (current-buffer))
		(list 'org-timeline (list 'quote include-all))))
    (if (not dopast)
	;; Remove past dates from the list of dates.
	(setq day-numbers (delq nil (mapcar (lambda(x)
					      (if (>= x today) x nil))
					    day-numbers))))
    (switch-to-buffer-other-window
     (get-buffer-create org-agenda-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-agenda-mode) (setq buffer-read-only nil)
    (while (setq d (pop day-numbers))
      (if (and (>= d today)
	       dopast
	       past)
	  (progn
	    (setq past nil)
	    (insert (make-string 79 ?-) "\n")))
      (setq date (calendar-gregorian-from-absolute d))
      (setq s (point))
      (if dotodo
	  (setq rtn (org-agenda-get-day-entries
		     entry date :todo :timestamp))
	(setq rtn (org-agenda-get-day-entries entry date :timestamp)))
      (if (or rtn (equal d today))
	  (progn
	    (insert (calendar-day-name date) " "
		    (number-to-string (extract-calendar-day date)) " "
		    (calendar-month-name (extract-calendar-month date)) " "
		    (number-to-string (extract-calendar-year date)) "\n")
	    (put-text-property s (1- (point)) 'face
			       'org-link)
	    (if (equal d today)
		(put-text-property s (1- (point)) 'org-today t))
	    (insert (org-finalize-agenda-entries rtn) "\n")
	    (put-text-property s (1- (point)) 'day d))))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (goto-char (or (text-property-any (point-min) (point-max) 'org-today t)
		   (point-min)))
    (when (not org-select-timeline-window)
      (select-window win)
      (goto-char pos1))))

;;;###autoload
(defun org-agenda (&optional include-all start-day ndays)
  "Produce a weekly view from all files in variable `org-agenda-files'.
The view will be for the current week, but from the overview buffer you
will be able to go to other weeks.
With one \\[universal-argument] prefix argument INCLUDE-ALL, all unfinished TODO items will
also be shown, under the current date.
START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.
NDAYS defaults to `org-agenda-ndays'."
  (interactive "P")
  (org-agenda-maybe-reset-markers 'force)
  (org-compile-prefix-format org-agenda-prefix-format)
  (require 'calendar)
  (let* ((org-agenda-start-on-weekday
	  (if (or (equal ndays 1)
		  (and (null ndays) (equal 1 org-agenda-ndays)))
	      nil org-agenda-start-on-weekday))
	 (files (copy-sequence org-agenda-files))
	 (win (selected-window))
	 (today (time-to-days (current-time)))
	 (sd (or start-day today))
	 (start (if (or (null org-agenda-start-on-weekday)
			(< org-agenda-ndays 7))
		    sd
		  (let* ((nt (calendar-day-of-week
			      (calendar-gregorian-from-absolute sd)))
			 (n1 org-agenda-start-on-weekday)
			 (d (- nt n1)))
		    (- sd (+ (if (< d 0) 7 0) d)))))
	 (day-numbers (list start))
	 (inhibit-redisplay t)
	 s e rtn rtnall file date d start-pos end-pos todayp nd)
    (setq org-agenda-redo-command
	  (list 'org-agenda (list 'quote include-all) start-day ndays))
    ;; Make the list of days
    (setq ndays (or ndays org-agenda-ndays)
	  nd ndays)
    (while (> ndays 1)
      (push (1+ (car day-numbers)) day-numbers)
      (setq ndays (1- ndays)))
    (setq day-numbers (nreverse day-numbers))
    (if (not (equal (current-buffer) (get-buffer org-agenda-buffer-name)))
	(progn
	  (delete-other-windows)
	  (switch-to-buffer-other-window
	   (get-buffer-create org-agenda-buffer-name))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-agenda-mode) (setq buffer-read-only nil)
    (set (make-local-variable 'starting-day) (car day-numbers))
    (set (make-local-variable 'include-all-loc) include-all)
    (when (and (or include-all org-agenda-include-all-todo)
	       (member today day-numbers))
      (setq files org-agenda-files
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq date (calendar-gregorian-from-absolute today)
		rtn (org-agenda-get-day-entries
		     file date :todo))
	  (setq rtnall (append rtnall rtn))))
      (when rtnall
	(insert "ALL CURRENTLY OPEN TODO ITEMS:\n")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-link))
	(insert (org-finalize-agenda-entries rtnall) "\n")))
    (while (setq d (pop day-numbers))
      (setq date (calendar-gregorian-from-absolute d)
	    s (point))
      (if (or (setq todayp (= d today))
	      (and (not start-pos) (= d sd)))
	  (setq start-pos (point))
	(if (and start-pos (not end-pos))
	    (setq end-pos (point))))
      (setq files org-agenda-files
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq rtn (org-agenda-get-day-entries file date))
	  (setq rtnall (append rtnall rtn))))
      (if org-agenda-include-diary
	  (progn
	    (require 'diary-lib)
	    (setq rtn (org-get-entries-from-diary date))
	    (setq rtnall (append rtnall rtn))))
      (if (or rtnall org-agenda-show-all-dates)
	  (progn
	    (insert (format "%-9s %2d %s %4d\n"
			    (calendar-day-name date)
			    (extract-calendar-day date)
			    (calendar-month-name (extract-calendar-month date))
			    (extract-calendar-year date)))
	    (put-text-property s (1- (point)) 'face
			       'org-link)
	    (if rtnall (insert
			(org-finalize-agenda-entries ;; FIXME: condition needed
			 (org-agenda-add-time-grid-maybe
			  rtnall nd todayp))
			"\n"))
	    (put-text-property s (1- (point)) 'day d))))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (if org-fit-agenda-window
	(fit-window-to-buffer nil (/ (* (frame-height) 3) 4)
			      (/ (frame-height) 2)))
    (unless (and (pos-visible-in-window-p (point-min))
		 (pos-visible-in-window-p (point-max)))
      (goto-char (1- (point-max)))
      (recenter -1)
      (if (not (pos-visible-in-window-p (or start-pos 1)))
	  (progn
	    (goto-char (or start-pos 1))
	    (recenter 1))))
    (goto-char (or start-pos 1))
    (if (not org-select-agenda-window) (select-window win))
    (message "")))

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  ;; FIXME:  this does not correctly change the menus
  ;; Could probably be fixed by explicitly going to the buffer.
  (when (not (file-exists-p file))
    (message "non-existent file %s.  [R]emove from agenda-files or [A]bort?"
	     file)
    (let ((r (downcase (read-char-exclusive))))
      (cond
       ((equal r ?r)
	(org-remove-file file)
	(throw 'nextfile t))
       (t (error "Abort"))))))

(defun org-agenda-quit ()
  "Exit agenda by removing the window or the buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (one-window-p)) (delete-window))
    (kill-buffer buf)
    (org-agenda-maybe-reset-markers 'force)))

(defun org-agenda-exit ()
  "Exit agenda by removing the window or the buffer.
Also kill all Org-mode buffers which have been loaded by `org-agenda'.
Org-mode buffers visited directly by the user will not be touched."
  (interactive)
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (org-agenda-quit))

(defun org-agenda-redo ()
  "Rebuild Agenda."
  (interactive)
  (eval org-agenda-redo-command))

(defun org-agenda-goto-today ()
  "Go to today."
  (interactive)
  (if (boundp 'starting-day)
      (let ((cmd (car org-agenda-redo-command))
	    (iall (nth 1 org-agenda-redo-command))
	    (nday (nth 3 org-agenda-redo-command)))
	(eval (list cmd iall nil nday)))
    (goto-char (or (text-property-any (point-min) (point-max) 'org-today t)
		   (point-min)))))

(defun org-agenda-later (arg)
  "Go forward in time by `org-agenda-ndays' days.
With prefix ARG, go forward that many times `org-agenda-ndays'."
  (interactive "p")
  (unless (boundp 'starting-day)
    (error "Not allowed"))
  (org-agenda (if (boundp 'include-all-loc) include-all-loc nil)
	      (+ starting-day (* arg org-agenda-ndays))))

(defun org-agenda-earlier (arg)
  "Go back in time by `org-agenda-ndays' days.
With prefix ARG, go back that many times `org-agenda-ndays'."
  (interactive "p")
  (unless (boundp 'starting-day)
    (error "Not allowed"))
  (org-agenda (if (boundp 'include-all-loc) include-all-loc nil)
	      (- starting-day (* arg org-agenda-ndays))))

(defun org-agenda-week-view ()
  "Switch to weekly view for agenda."
  (interactive)
  (unless (boundp 'starting-day)
    (error "Not allowed"))
  (setq org-agenda-ndays 7)
  (org-agenda include-all-loc
	      (or (get-text-property (point) 'day)
		  starting-day))
  (org-agenda-set-mode-name)
  (message "Switched to week view"))

(defun org-agenda-day-view ()
  "Switch to weekly view for agenda."
  (interactive)
  (unless (boundp 'starting-day)
    (error "Not allowed"))
  (setq org-agenda-ndays 1)
  (org-agenda include-all-loc
	      (or (get-text-property (point) 'day)
		  starting-day))
  (org-agenda-set-mode-name)
  (message "Switched to day view"))

(defun org-agenda-next-date-line (&optional arg)
  "Jump to the next line indicating a date in agenda buffer."
  (interactive "p")
  (beginning-of-line 1)
  (if (looking-at "^\\S-") (forward-char 1))
  (if (not (re-search-forward "^\\S-" nil t arg))
      (progn
	(backward-char 1)
	(error "No next date after this line in this buffer")))
  (goto-char (match-beginning 0)))

(defun org-agenda-previous-date-line (&optional arg)
  "Jump to the next line indicating a date in agenda buffer."
  (interactive "p")
  (beginning-of-line 1)
  (if (not (re-search-backward "^\\S-" nil t arg))
      (error "No previous date before this line in this buffer")))

;; Initialize the highlight
(defvar org-hl (funcall (if org-xemacs-p 'make-extent 'make-overlay) 1 1))
(funcall (if org-xemacs-p 'set-extent-property 'overlay-put) org-hl
	 'face 'highlight)

(defun org-highlight (begin end &optional buffer)
  "Highlight a region with overlay."
  (funcall (if org-xemacs-p 'set-extent-endpoints 'move-overlay)
	   org-hl begin end (or buffer (current-buffer))))

(defun org-unhighlight ()
  "Detach overlay INDEX."
  (funcall (if org-xemacs-p 'detach-extent 'delete-overlay) org-hl))


(defun org-agenda-follow-mode ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (setq org-agenda-follow-mode (not org-agenda-follow-mode))
  (org-agenda-set-mode-name)
  (message "Follow mode is %s"
	   (if org-agenda-follow-mode "on" "off")))

(defun org-agenda-toggle-diary ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (setq org-agenda-include-diary (not org-agenda-include-diary))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Diary inclusion turned %s"
	   (if org-agenda-include-diary "on" "off")))

(defun org-agenda-toggle-time-grid ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (setq org-agenda-use-time-grid (not org-agenda-use-time-grid))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Time-grid turned %s"
	   (if org-agenda-use-time-grid "on" "off")))

(defun org-agenda-set-mode-name ()
  "Set the mode name to indicate all the small mode settings."
  (setq mode-name
	(concat "Org-Agenda"
		(if (equal org-agenda-ndays 1) " Day"    "")
		(if (equal org-agenda-ndays 7) " Week"   "")
		(if org-agenda-follow-mode     " Follow" "")
		(if org-agenda-include-diary   " Diary"  "")
		(if org-agenda-use-time-grid   " Grid"   "")))
  (force-mode-line-update))

(defun org-agenda-post-command-hook ()
  (and (eolp) (not (bolp)) (backward-char 1))
  (if (and org-agenda-follow-mode
	   (get-text-property (point) 'org-marker))
      (org-agenda-show)))

(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.

(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (let* ((fancy-diary-buffer "*temporary-fancy-diary-buffer*")
	 (diary-display-hook '(fancy-diary-display))
	 (list-diary-entries-hook
	  (cons 'org-diary-default-entry list-diary-entries-hook))
	 (diary-file-name-prefix-function nil) ; turn this feature off
	 (diary-modify-entry-list-string-function 'org-modify-diary-entry-string)
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
	(list-diary-entries date 1)))
    (if (not (get-buffer fancy-diary-buffer))
	(setq entries nil)
      (with-current-buffer fancy-diary-buffer
	(setq buffer-read-only nil)
	(if (= (point-max) 1)
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (= (point-max) 1)
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))))
	(set-buffer-modified-p nil)
	(kill-buffer fancy-diary-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-format-agenda-item "" x "Diary" 'time))
	       ;; Extend the text properties to the beginning of the line
	       (add-text-properties
		0 (length x)
		(text-properties-at (1- (length x)) x)
		x)
	       x)
	     entries)))))

(defun org-agenda-cleanup-fancy-diary ()
  "Remove unwanted stuff in buffer created by fancy-diary-display.
This gets rid of the date, the underline under the date, and
the dummy entry installed by `org-mode' to ensure non-empty diary for each
date.  Itt also removes lines that contain only whitespace."
  (goto-char (point-min))
  (if (looking-at ".*?:[ \t]*")
      (progn
	(replace-match "")
	(re-search-forward "\n=+$" nil t)
	(replace-match "")
	(while (re-search-backward "^ +\n?" nil t) (replace-match "")))
    (re-search-forward "\n=+$" nil t)
    (delete-region (point-min) (min (point-max) (1+ (match-end 0)))))
  (goto-char (point-min))
  (while (re-search-forward "^ +\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (if (re-search-forward "^Org-mode dummy\n?" nil t)
      (replace-match "")))

;; Make sure entries from the diary have the right text properties.
(eval-after-load "diary-lib"
  '(if (boundp 'diary-modify-entry-list-string-function)
       ;; We can rely on the hook, nothing to do
       nil
     ;; Hook not avaiable, must use advice to make this work
     (defadvice add-to-diary-list (before org-mark-diary-entry activate)
       "Make the position visible."
       (if (and org-disable-agenda-to-diary  ;; called from org-agenda
		(stringp string)
		(buffer-file-name))
	   (setq string (org-modify-diary-entry-string string))))))

(defun org-modify-diary-entry-string (string)
  "Add text properties to string, allowing org-mode to act on it."
  (add-text-properties
   0 (length string)
   (list 'mouse-face 'highlight
	 'keymap org-agenda-keymap
	 'help-echo
	 (format
	  "mouse-2 or RET jump to diary file %s"
	  (abbreviate-file-name (buffer-file-name)))
	 'org-agenda-diary-link t
	 'org-marker (org-agenda-new-marker (point-at-bol)))
   string)
  string)

(defun org-diary-default-entry ()
  "Add a dummy entry to the diary.
Needed to avoid empty dates which mess up holiday display."
  ;; Catch the error if dealing with the new add-to-diary-alist
  (when org-disable-agenda-to-diary
    (condition-case nil
	(add-to-diary-list original-date "Org-mode dummy" "")
      (error
       (add-to-diary-list original-date  "Org-mode dummy" "" nil)))))

(defun org-add-file (&optional file)
  "Add current file to the list of files in variable `org-agenda-files'.
These are the files which are being checked for agenda entries.
Optional argument FILE means, use this file instead of the current.
It is possible (but not recommended) to add this function to the
`org-mode-hook'."
  (interactive)
  (catch 'exit
    (let* ((file (or file (buffer-file-name)
		     (if (interactive-p)
			 (error "Buffer is not visiting a file")
		       (throw 'exit nil))))
	   (true-file (file-truename file))
	   (afile (abbreviate-file-name file))
	   (present (delq nil (mapcar
			       (lambda (x)
				 (equal true-file (file-truename x)))
			       org-agenda-files))))
      (if (not present)
	  (progn
	    (setq org-agenda-files
		  (cons afile org-agenda-files))
	    ;; Make sure custom.el does not end up with Org-mode
	    (let ((org-mode-hook nil) (default-major-mode 'fundamental-mode))
	      (customize-save-variable 'org-agenda-files org-agenda-files))
	    (org-install-agenda-files-menu)
	    (message "Added file: %s" afile))
	(message "File was already in list: %s" afile)))))

(defun org-remove-file (&optional file)
  "Remove current file from the list of files in variable `org-agenda-files'.
These are the files which are being checked for agenda entries.
Optional argument FILE means, use this file instead of the current."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (true-file (file-truename file))
	 (afile (abbreviate-file-name file))
	 (files (delq nil (mapcar
			   (lambda (x)
			     (if (equal true-file
					(file-truename x))
				 nil x))
			   org-agenda-files))))
    (if (not (= (length files) (length org-agenda-files)))
	(progn
	  (setq org-agenda-files files)
	  (customize-save-variable 'org-agenda-files org-agenda-files)
	  (org-install-agenda-files-menu)
	  (message "Removed file: %s" afile))
      (message "File was not in list: %s" afile))))

(defun org-file-menu-entry (file)
  (vector file (list 'find-file file) t))

(defun org-get-all-dates (beg end &optional no-ranges force-today)
  "Return a list of all relevant day numbers from BEG to END buffer positions.
If NO-RANGES is non-nil, include only the start and end dates of a range,
not every single day in the range.  If FORCE-TODAY is non-nil, make
sure that TODAY is included in the list."
  (let (dates date day day1 day2 ts1 ts2)
    (if force-today
	(setq dates (list (time-to-days (current-time)))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-ts-regexp end t)
	(setq day (time-to-days (org-time-string-to-time
				 (substring (match-string 1) 0 10))))
	(or (memq day dates) (push day dates)))
      (unless no-ranges
	(goto-char beg)
	(while (re-search-forward org-tr-regexp end t)
	  (setq ts1 (substring (match-string 1) 0 10)
		ts2 (substring (match-string 2) 0 10)
		day1 (time-to-days (org-time-string-to-time ts1))
		day2 (time-to-days (org-time-string-to-time ts2)))
	  (while (< (setq day1 (1+ day1)) day2)
	    (or (memq day1 dates) (push day1 dates)))))
      (sort dates '<))))

;;;###autoload
(defun org-diary (&rest args)
  "Return diary information from org-files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  The following arguments are allowed:

   :timestamp    List the headlines of items containing a date stamp or
		 date range matching the selected date.  Deadlines will
		 also be listed, on the expiration day.

   :deadline     List any deadlines past due, or due within
		 `org-deadline-warning-days'.  The listing occurs only
		 in the diary for *today*, not at any other date.  If
		 an entry is marked DONE, it is no longer listed.

   :scheduled    List all items which are scheduled for the given date.
		 The diary for *today* also contains items which were
		 scheduled earlier and are not yet marked DONE.

   :todo         List all TODO items from the org-file.  This may be a
		 long list - so this is not turned on by default.
		 Like deadlines, these entries only show up in the
		 diary for *today*, not at any other date.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default
arguments (:deadline :scheduled :timestamp) are used.  So the example above may
also be written as

   &%%(org-diary :deadline :timestamp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead."
  (org-agenda-maybe-reset-markers)
  (org-compile-prefix-format org-agenda-prefix-format)
  (setq args (or args '(:deadline :scheduled :timestamp)))
  (let* ((files (if (and entry (stringp entry) (string-match "\\S-" entry))
		    (list entry)
		  org-agenda-files))
	 file rtn results)
    ;; If this is called during org-agenda, don't return any entries to
    ;; the calendar.  Org Agenda will list these entries itself.
    (if org-disable-agenda-to-diary (setq files nil))
    (while (setq file (pop files))
      (setq rtn (apply 'org-agenda-get-day-entries file date args))
      (setq results (append results rtn)))
    (if results
	(concat (org-finalize-agenda-entries results) "\n"))))

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-diary'."
  (setq args (or args '(:deadline :scheduled :timestamp)))
  (let* ((org-startup-with-deadline-check nil)
	 (org-startup-folded nil)
	 (buffer (if (file-exists-p file)
		     (org-get-agenda-file-buffer file)
		   (error "No such file %s" file)))
	 arg results rtn)
    (if (not buffer)
	;; If file does not exist, make sure an error message ends up in diary
	(list (format "ORG-AGENDA-ERROR: No such org-file %s" file))
      (with-current-buffer buffer
	(unless (eq major-mode 'org-mode)
	  (error "Agenda file %s is not in `org-mode'" file))
	(let ((case-fold-search nil))
	  (save-excursion
	    (save-restriction
	      (if org-respect-restriction
		  (if (org-region-active-p)
		      ;; Respect a region to restrict search
		      (narrow-to-region (region-beginning) (region-end)))
		;; If we work for the calendar or many files,
		;; get rid of any restriction
		(widen))
	      ;; The way we repeatedly append to `results' makes it O(n^2) :-(
	      (while (setq arg (pop args))
		(cond
		 ((and (eq arg :todo)
		       (equal date (calendar-current-date)))
		  (setq rtn (org-agenda-get-todos))
		  (setq results (append results rtn)))
		 ((eq arg :timestamp)
		  (setq rtn (org-agenda-get-blocks))
		  (setq results (append results rtn))
		  (setq rtn (org-agenda-get-timestamps))
		  (setq results (append results rtn)))
		 ((eq arg :scheduled)
		  (setq rtn (org-agenda-get-scheduled))
		  (setq results (append results rtn)))
		 ((and (eq arg :deadline)
		       (equal date (calendar-current-date)))
		  (setq rtn (org-agenda-get-deadlines))
		  (setq results (append results rtn))))))))
	results))))

(defun org-entry-is-done-p ()
  "Is the current entry marked DONE?"
  (save-excursion
    (and (re-search-backward "[\r\n]\\*" nil t)
	 (looking-at org-nl-done-regexp))))

(defun org-at-date-range-p ()
  "Is the cursor inside a date range?"
  (interactive)
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^<\r\n")
	(skip-chars-backward "<")
	(and (looking-at org-tr-regexp)
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<\r\n")
	(skip-chars-backward "<")
	(and (looking-at org-tr-regexp)
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defun org-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
		      'done-face 'org-done
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name (buffer-file-name)))))
	 (regexp (concat "[\n\r]\\*+ *\\(" org-not-done-regexp
			 "[^\n\r]*\\)"))
	 marker priority
	 ee txt)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (goto-char (match-beginning 1))
      (setq marker (org-agenda-new-marker (point-at-bol))
	    txt (org-format-agenda-item "" (match-string 1))
	    priority
	    (+ (org-get-priority txt)
	       (if org-todo-kwd-priority-p
			 (- org-todo-kwd-max-priority -2
			    (length
			     (member (match-string 2) org-todo-keywords)))
		       1)))
      (add-text-properties
       0 (length txt) (append (list 'org-marker marker 'org-hd-marker marker
				    'priority priority)
			      props)
       txt)
      (push txt ee)
      (goto-char (match-end 1)))
    (nreverse ee)))

(defconst org-agenda-no-heading-message
  "No heading for this item in buffer or region")

(defun org-agenda-get-timestamps ()
  "Return the date stamp information for agenda display."
  (let* ((props (list 'face nil
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name (buffer-file-name)))))
	 (regexp (regexp-quote
		  (substring
		   (format-time-string
		    (car org-time-stamp-formats)
		    (apply 'encode-time  ; DATE bound by calendar
			   (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
		   0 11)))
	 marker hdmarker deadlinep scheduledp donep tmp priority
	 ee txt timestr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (if (not (save-match-data (org-at-date-range-p)))
	  (progn
	    (setq marker (org-agenda-new-marker (match-beginning 0))
		  tmp (buffer-substring (max (point-min)
					     (- (match-beginning 0)
						org-ds-keyword-length))
					(match-beginning 0))
		  timestr (buffer-substring (match-beginning 0) (point-at-eol))
		  deadlinep (string-match org-deadline-regexp tmp)
		  scheduledp (string-match org-scheduled-regexp tmp)
		  donep (org-entry-is-done-p))
	    (if (string-match ">" timestr)
		;; substring should only run to end of time stamp
		(setq timestr (substring timestr 0 (match-end 0))))
	    (save-excursion
	      (if (re-search-backward "\\(^\\|\r\\)\\*+" nil t)
		  (progn
		    (goto-char (match-end 1))
		    (setq hdmarker (org-agenda-new-marker))
		    (looking-at "\\*+[ \t]*\\([^\r\n]+\\)")
		    (setq txt (org-format-agenda-item
			       (format "%s%s"
				       (if deadlinep  "Deadline:  " "")
				       (if scheduledp "Scheduled: " ""))
			       (match-string 1) nil timestr)))
		(setq txt org-agenda-no-heading-message))
	      (setq priority (org-get-priority txt))
	      (add-text-properties
	       0 (length txt) (append (list 'org-marker marker
					    'org-hd-marker hdmarker) props)
	       txt)
	      (if deadlinep
		  (add-text-properties
		   0 (length txt)
		   (list 'face
			 (if donep 'org-done 'org-warning)
			 'undone-face 'org-warning
			 'done-face 'org-done
			 'priority (+ 100 priority))
		   txt)
		(if scheduledp
		    (add-text-properties
		     0 (length txt)
		     (list 'face 'org-scheduled-today
			   'undone-face 'org-scheduled-today
			   'done-face 'org-done
			   priority (+ 99 priority))
		     txt)
		  (add-text-properties
		   0 (length txt)
		   (list 'priority priority) txt)))
	      (push txt ee))
	    (outline-next-heading))))
    (nreverse ee)))

(defun org-agenda-get-deadlines ()
  "Return the deadline information for agenda display."
  (let* ((wdays org-deadline-warning-days)
	 (props (list 'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name (buffer-file-name)))))
	 (regexp org-deadline-time-regexp)
	 (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
	 (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
	 d2 diff pos pos1
	 ee txt head)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq pos (1- (match-beginning 1))
	    d2 (time-to-days
		(org-time-string-to-time (match-string 1)))
	    diff (- d2 d1))
      ;; When to show a deadline in the calendar:
      ;; If the expiration is within wdays warning time.
      ;; Past-due deadlines are only shown on the current date
      (if (and (< diff wdays) todayp (not (= diff 0)))
	  (save-excursion
	    (if (re-search-backward "\\(^\\|\r\\)\\*+[ \t]*" nil t)
		(progn
		  (goto-char (match-end 0))
		  (setq pos1 (match-end 1))
		  (setq head (buffer-substring-no-properties
			      (point)
			      (progn (skip-chars-forward "^\r\n")
				     (point))))
		  (if (string-match org-looking-at-done-regexp head)
		      (setq txt nil)
		    (setq txt (org-format-agenda-item
			       (format "In %3d d.: " diff) head))))
	      (setq txt org-agenda-no-heading-message))
	    (when txt
	      (add-text-properties
	       0 (length txt)
	       (append
		(list 'org-marker (org-agenda-new-marker pos)
		      'org-hd-marker (org-agenda-new-marker pos1)
		      'priority (+ (- 10 diff) (org-get-priority txt))
		      'face (cond ((<= diff 0) 'org-warning)
				  ((<= diff 5) 'org-scheduled-previously)
				  (t nil))
		      'undone-face (cond
				    ((<= diff 0) 'org-warning)
				    ((<= diff 5) 'org-scheduled-previously)
				    (t nil))
		      'done-face 'org-done)
		props)
	       txt)
	      (push txt ee)))))
    ee))

(defun org-agenda-get-scheduled ()
  "Return the scheduled information for agenda display."
  (let* ((props (list 'face 'org-scheduled-previously
		      'undone-face 'org-scheduled-previously
		      'done-face 'org-done
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name (buffer-file-name)))))
	 (regexp org-scheduled-time-regexp)
	 (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
	 (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
	 d2 diff pos pos1
	 ee txt head)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq pos (1- (match-beginning 1))
	    d2 (time-to-days
		(org-time-string-to-time (match-string 1)))
	    diff (- d2 d1))
      ;; When to show a scheduled item in the calendar:
      ;; If it is on or past the date.
      (if (and (< diff 0) todayp)
	  (save-excursion
	    (if (re-search-backward "\\(^\\|\r\\)\\*+[ \t]*" nil t)
		(progn
		  (goto-char (match-end 0))
		  (setq pos1 (match-end 1))
		  (setq head (buffer-substring-no-properties
			      (point)
			      (progn (skip-chars-forward "^\r\n") (point))))
		  (if (string-match org-looking-at-done-regexp head)
		      (setq txt nil)
		    (setq txt (org-format-agenda-item
			       (format "Sched.%2dx: " (- 1 diff)) head))))
	      (setq txt org-agenda-no-heading-message))
	    (when txt
	      (add-text-properties
	       0 (length txt)
	       (append (list 'org-marker (org-agenda-new-marker pos)
			     'org-hd-marker (org-agenda-new-marker pos1)
			     'priority (+ (- 5 diff) (org-get-priority txt)))
		       props) txt)
	      (push txt ee)))))
    ee))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (let* ((props (list 'face nil
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name (buffer-file-name)))))
	 (regexp org-tr-regexp)
	 (d0 (calendar-absolute-from-gregorian date))
	 marker hdmarker ee txt d1 d2 s1 s2 timestr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq timestr (match-string 0)
	    s1 (match-string 1)
	    s2 (match-string 2)
	    d1 (time-to-days (org-time-string-to-time s1))
	    d2 (time-to-days (org-time-string-to-time s2)))
      (if (and (> (- d0 d1) -1) (> (- d2 d0) -1))
	  ;; Only allow days between the limits, because the normal
	  ;; date stamps will catch the limits.
	  (save-excursion
	    (setq marker (org-agenda-new-marker (point)))
	    (if (re-search-backward "\\(^\\|\r\\)\\*+" nil t)
		(progn
		  (setq hdmarker (org-agenda-new-marker (match-end 1)))
		  (goto-char (match-end 1))
		  (looking-at "\\*+[ \t]*\\([^\r\n]+\\)")
		  (setq txt (org-format-agenda-item
			     (format (if (= d1 d2) "" "(%d/%d): ")
				     (1+ (- d0 d1)) (1+ (- d2 d1)))
			     (match-string 1) nil (if (= d0 d1) timestr))))
	      (setq txt org-agenda-no-heading-message))
	    (add-text-properties
	     0 (length txt) (append (list 'org-marker marker
					  'org-hd-marker hdmarker
					  'priority (org-get-priority txt))
				    props)
	     txt)
	    (push txt ee)))
      (outline-next-heading))
    ;; Sort the entries by expiration date.
    (nreverse ee)))



(defconst org-plain-time-of-day-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\(--?"
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\)?")
  "Regular expression to match a plain time or time range.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
1  the first time, range or not
8  the second time, if it is a range.")

(defconst org-stamp-time-of-day-regexp
  (concat
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +[a-zA-Z]+ +\\)"
   "\\([012][0-9]:[0-5][0-9]\\)>"
   "\\(--?"
   "<\\1\\([012][0-9]:[0-5][0-9]\\)>\\)?")
  "Regular expression to match a timestamp time or time range.
After a match, the following groups carry important information:
0  the full match
1  date plus weekday, for backreferencing to make sure both times on same day
2  the first time, range or not
4  the second time, if it is a range.")

(defvar org-prefix-has-time nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%t'.")

(defun org-format-agenda-item (extra txt &optional category dotime noprefix)
  "Format TXT to be inserted into the agenda buffer.
In particular, it adds the prefix and corresponding text properties.  EXTRA
must be a string and replaces the `%s' specifier in the prefix format.
CATEGORY (string, symbol or nil) may be used to overule the default
category taken from local variable or file name. It will replace the `%c'
specifier in the format.  DOTIME, when non-nil, indicates that a
time-of-day should be extracted from TXT for sorting of this entry, and for
the `%t' specifier in the format.  When DOTIME is a string, this string is
searched for a time before TXT is.  NOPREFIX is a flag and indicates that
only the correctly processes TXT should be returned - this is used by
`org-agenda-change-all-lines'."
  (save-match-data
    ;; Diary entries sometimes have extra whitespace at the beginning
    (if (string-match "^ +" txt) (setq txt (replace-match "" nil nil txt)))
    (let* ((category (or category
			 org-category
			 (if (buffer-file-name)
			     (file-name-sans-extension
			      (file-name-nondirectory (buffer-file-name)))
			   "")))
	   time              ;; needed for the eval of the prefix format
	   (ts (if dotime (concat (if (stringp dotime) dotime "") txt)))
	   (time-of-day (and dotime (org-get-time-of-day ts)))
	   stamp plain s0 s1 s2 rtn)
      (when (and dotime time-of-day org-prefix-has-time)
	;; Extract starting and ending time and move them to prefix
	(when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		  (setq plain (string-match org-plain-time-of-day-regexp ts)))
	  (setq s0 (match-string 0 ts)
		s1 (match-string (if plain 1 2) ts)
		s2 (match-string (if plain 8 4) ts))

	  ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	  ;; them, we might want to remove them there to avoid duplication.
	  ;; The user can turn this off with a variable.
	  (if (and org-agenda-remove-times-when-in-prefix (or stamp plain)
		   (string-match (concat (regexp-quote s0) " *") txt)
		   (if (eq org-agenda-remove-times-when-in-prefix 'beg)
		       (= (match-beginning 0) 0)
		     t))
	      (setq txt (replace-match "" nil nil txt))))
	;; Normalize the time(s) to 24 hour
	(if s1 (setq s1 (org-get-time-of-day s1 'string)))
	(if s2 (setq s2 (org-get-time-of-day s2 'string))))

      ;; Create the final string
      (if noprefix
	  (setq rtn txt)
	;; Prepare the variables needed in the eval of the compiled format
	(setq time (cond (s2 (concat s1 "-" s2))
			 (s1 (concat s1 "......"))
			 (t ""))
	      extra (or extra "")
	      category (if (symbolp category) (symbol-name category) category))
	;; Evaluate the compiled format
	(setq rtn (concat (eval org-prefix-format-compiled) txt)))

      ;; And finally add the text properties
      (add-text-properties
       0 (length rtn) (list 'category (downcase category)
			    'prefix-length (- (length rtn) (length txt))
			    'time-of-day time-of-day
			    'dotime dotime)
       rtn)
      rtn)))

(defun org-agenda-add-time-grid-maybe (list ndays todayp)
  (catch 'exit
    (cond ((not org-agenda-use-time-grid) (throw 'exit list))
	  ((and todayp (member 'today (car org-agenda-time-grid))))
	  ((and (= ndays 1) (member 'daily (car org-agenda-time-grid))))
	  ((member 'weekly (car org-agenda-time-grid)))
	  (t (throw 'exit list)))
    (let* ((have (delq nil (mapcar
			    (lambda (x) (get-text-property 1 'time-of-day x))
			    list)))
	   (string (nth 1 org-agenda-time-grid))
	   (gridtimes (nth 2 org-agenda-time-grid))
	   (req (car org-agenda-time-grid))
	   (remove (member 'remove-match req))
	   new time)
      (if (and (member 'require-timed req) (not have))
	  ;; don't show empty grid
	  (throw 'exit list))
      (while (setq time (pop gridtimes))
	(unless (and remove (member time have))
	  (setq time (int-to-string time))
	  (push (org-format-agenda-item
		 nil string "" ;; FIXME: put a category?
		 (concat (substring time 0 -2) ":" (substring time -2)))
		new)
	  (put-text-property
	   1 (length (car new)) 'face 'org-time-grid (car new))))
      (if (member 'time-up org-agenda-sorting-strategy)
	  (append new list)
	(append list new)))))

(defun org-compile-prefix-format (format)
  "Compile the prefix format into a Lisp form that can be evaluated.
The resulting form is returned and stored in the variable
`org-prefix-format-compiled'."
  (setq org-prefix-has-time nil)
  (let ((start 0) varform vars var (s format) c f opt)
    (while (string-match "%\\(\\?\\)?\\([-+]?[0-9.]*\\)\\([ .;,:!?=|/<>]?\\)\\([cts]\\)"
			 s start)
      (setq var (cdr (assoc (match-string 4 s)
			    '(("c" . category) ("t" . time) ("s" . extra))))
	    c (or (match-string 3 s) "")
	    opt (match-beginning 1)
	    start (1+ (match-beginning 0)))
      (if (equal var 'time) (setq org-prefix-has-time t))
      (setq f (concat "%" (match-string 2 s) "s"))
      (if opt
	  (setq varform
		`(if (equal "" ,var)
		     ""
		   (format ,f (if (equal "" ,var) "" (concat ,var ,c)))))
	(setq varform `(format ,f (if (equal ,var "") "" (concat ,var ,c)))))
      (setq s (replace-match "%s" t nil s))
      (push varform vars))
    (setq vars (nreverse vars))
    (setq org-prefix-format-compiled `(format ,s ,@vars))))

(defun org-get-time-of-day (s &optional string)
  "Check string S for a time of day.
If found, return it as a military time number between 0 and 2400.
If not found, return nil.
The optional STRING argument forces conversion into a 5 character wide string
HH:MM."
  (save-match-data
    (when
     (or
      (string-match
       "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)\\([AaPp][Mm]\\)?\\> *" s)
      (string-match
       "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\([AaPp][Mm]\\)\\> *" s))
     (let* ((t0 (+ (* 100
		      (+ (string-to-number (match-string 1 s))
			 (if (and (match-beginning 4)
				  (equal (downcase (match-string 4 s)) "pm"))
			     12 0)))
		   (if (match-beginning 3)
		       (string-to-number (match-string 3 s))
		     0)))
	    (t1 (concat " " (int-to-string t0))))
       (if string (concat (substring t1 -4 -2) ":" (substring t1 -2)) t0)))))

(defun org-finalize-agenda-entries (list)
  "Sort and concatenate the agenda items."
  (mapconcat 'identity (sort list 'org-entries-lessp) "\n"))

(defsubst org-cmp-priority (a b)
  "Compare the priorities of string a and b."
  (let ((pa (or (get-text-property 1 'priority a) 0))
	(pb (or (get-text-property 1 'priority b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))

(defsubst org-cmp-category (a b)
  "Compare the string values of categories of strings a and b."
  (let ((ca (or (get-text-property 1 'category a) ""))
	(cb (or (get-text-property 1 'category b) "")))
    (cond ((string-lessp ca cb) -1)
	  ((string-lessp cb ca) +1)
	  (t nil))))

(defsubst org-cmp-time (a b)
  "Compare the time-of-day values of strings a and b."
  (let* ((def (if org-sort-agenda-notime-is-late 2401 -1))
	 (ta (or (get-text-property 1 'time-of-day a) def))
	 (tb (or (get-text-property 1 'time-of-day b) def)))
    (cond ((< ta tb) -1)
	  ((< tb ta) +1)
	  (t nil))))

(defun org-entries-lessp (a b)
  "Predicate for sorting agenda entries."
  ;; The following variables will be used when the form is evaluated.
  (let* ((time-up (org-cmp-time a b))
	 (time-down (if time-up (- time-up) nil))
	 (priority-up (org-cmp-priority a b))
	 (priority-down (if priority-up (- priority-up) nil))
	 (category-up (org-cmp-category a b))
	 (category-down (if category-up (- category-up) nil))
	 (category-keep (if category-up +1 nil)))  ; FIXME +1 or -1?
    (cdr (assoc
	  (eval (cons 'or org-agenda-sorting-strategy))
	  '((-1 . t) (1 . nil) (nil . nil))))))

(defun org-agenda-show-priority ()
  "Show the priority of the current item.
This priority is composed of the main priority given with the [#A] cookies,
and by additional input from the age of a schedules or deadline entry."
  (interactive)
  (let* ((pri (get-text-property (point-at-bol) 'priority)))
    (message "Priority is %d" (if pri pri -1000))))

(defun org-agenda-goto (&optional highlight)
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (goto-char pos)
    (when (eq major-mode 'org-mode)
      (org-show-hidden-entry)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))) ; show the next heading
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

(defun org-agenda-switch-to ()
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (delete-other-windows)
    (widen)
    (goto-char pos)
    (when (eq major-mode 'org-mode)
      (org-show-hidden-entry)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil))))))  ; show the next heading

(defun org-agenda-goto-mouse (ev)
  "Go to the Org-mode file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-goto))

(defun org-agenda-show ()
  "Display the Org-mode file which contains the item at point."
  (interactive)
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (select-window win)))

(defun org-agenda-recenter (arg)
  "Display the Org-mode file which contains the item at point and recenter."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (recenter arg)
    (select-window win)))

(defun org-agenda-show-mouse (ev)
  "Display the Org-mode file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-show))

(defun org-agenda-check-no-diary ()
  "Check if the entry is a diary link and abort if yes."
  (if (get-text-property (point) 'org-agenda-diary-link)
      (org-agenda-error)))

(defun org-agenda-error ()
  (error "Command not allowed in this line"))

(defvar org-last-heading-marker (make-marker)
  "Marker pointing to the headline that last changed its TODO state
by a remote command from the agenda.")

(defun org-agenda-todo ()
  "Cycle TODO state of line at point, also in Org-mode file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (buffer-read-only nil)
	 newhead)
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-show-hidden-entry)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))   ; show the next heading
      (org-todo)
      (forward-char 1)
      (setq newhead (org-get-heading))
      (save-excursion
	(org-back-to-heading)
	(move-marker org-last-heading-marker (point))))
    (beginning-of-line 1)
    (save-excursion
      (org-agenda-change-all-lines newhead hdmarker 'fixface))
    (move-to-column col)))

(defun org-agenda-change-all-lines (newhead hdmarker &optional fixface)
  "Change all lines in the agenda buffer which match hdmarker.
The new content of the line will be NEWHEAD (as modified by
`org-format-agenda-item').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified acording to
the new TODO state."
  (let* (props m pl undone-face done-face finish new dotime)
;    (setq newhead (org-format-agenda-item "x" newhead "x" nil 'noprefix))
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line 1)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (get-text-property (point) 'org-hd-marker))
		   (equal m hdmarker))
	  (setq props (text-properties-at (point))
		dotime (get-text-property (point) 'dotime)
		new (org-format-agenda-item "x" newhead "x" dotime 'noprefix)
		pl (get-text-property (point) 'prefix-length)
		undone-face (get-text-property (point) 'undone-face)
		done-face (get-text-property (point) 'done-face))
	  (move-to-column pl)
	  (if (looking-at ".*")
	      (progn
		(replace-match new t t)
		(beginning-of-line 1)
		(add-text-properties (point-at-bol) (point-at-eol) props)
		(if fixface
		    (add-text-properties
		     (point-at-bol) (point-at-eol)
		     (list 'face
			   (if org-last-todo-state-is-todo
			       undone-face done-face))))
		(beginning-of-line 1))
	    (error "Line update did not work")))
	(beginning-of-line 0)))))

(defun org-agenda-priority-up ()
  "Increase the priority of line at point, also in Org-mode file."
  (interactive)
  (org-agenda-priority 'up))

(defun org-agenda-priority-down ()
  "Decrease the priority of line at point, also in Org-mode file."
  (interactive)
  (org-agenda-priority 'down))

(defun org-agenda-priority (&optional force-direction)
  "Set the priority of line at point, also in Org-mode file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (buffer-read-only nil)
	 newhead)
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-show-hidden-entry)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))   ; show the next heading
      (funcall 'org-priority force-direction)
      (end-of-line 1)
      (setq newhead (org-get-heading)))
    (org-agenda-change-all-lines newhead hdmarker)
    (beginning-of-line 1)))

(defun org-agenda-date-later (arg &optional what)
  "Change the date of this item to one day later."
  (interactive "p")
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (if (not (org-at-timestamp-p))
	  (error "Cannot find time stamp"))
      (org-timestamp-change arg (or what 'day))
      (message "Time stamp changed to %s" org-last-changed-timestamp))))

(defun org-agenda-date-earlier (arg &optional what)
  "Change the date of this item to one day earlier."
  (interactive "p")
  (org-agenda-date-later (- arg) what))

(defun org-agenda-date-prompt (arg)
  "Change the date of this item.  Date is prompted for, with default today.
The prefix ARG is passed to the `org-time-stamp' command and can therefore
be used to request time specification in the time stamp."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (if (not (org-at-timestamp-p))
	  (error "Cannot find time stamp"))
      (org-time-stamp arg)
      (message "Time stamp changed to %s" org-last-changed-timestamp))))

(defun org-get-heading ()
  "Return the heading of the current entry, without the stars."
  (save-excursion
    (if (and (re-search-backward "[\r\n]\\*" nil t)
	     (looking-at "[\r\n]\\*+[ \t]+\\(.*\\)"))
	(match-string 1)
      "")))

(defun org-agenda-diary-entry ()
  "Make a diary entry, like the `i' command from the calendar.
All the standard commands work: block, weekly etc"
  (interactive)
  (require 'diary-lib)
  (let* ((char (progn
		 (message "Diary entry: [d]ay [w]eekly [m]onthly [y]early [a]nniversary [b]lock [c]yclic")
		 (read-char-exclusive)))
	 (cmd (cdr (assoc char
			  '((?d . insert-diary-entry)
			    (?w . insert-weekly-diary-entry)
			    (?m . insert-monthly-diary-entry)
			    (?y . insert-yearly-diary-entry)
			    (?a . insert-anniversary-diary-entry)
			    (?b . insert-block-diary-entry)
			    (?c . insert-cyclic-diary-entry)))))
	 (oldf (symbol-function 'calendar-cursor-to-date))
	 (point (point))
	 (mark (or (mark t) (point))))
    (unless cmd
      (error "No command associated with <%c>" char))
    (unless (and (get-text-property point 'day)
		 (or (not (equal ?b char))
		     (get-text-property mark 'day)))
      (error "Don't know which date to use for diary entry"))
    ;; We implement this by hacking the `calendar-cursor-to-date' function
    ;; and the `calendar-mark-ring' variable.  Saves a lot of code.
    (let ((calendar-mark-ring
	   (list (calendar-gregorian-from-absolute
		  (or (get-text-property mark 'day)
		      (get-text-property point 'day))))))
      (unwind-protect
	  (progn
	    (fset 'calendar-cursor-to-date
		  (lambda (&optional error)
		    (calendar-gregorian-from-absolute
		     (get-text-property point 'day))))
	    (call-interactively cmd))
	(fset 'calendar-cursor-to-date oldf)))))


(defun org-agenda-execute-calendar-command (cmd)
  "Execute a calendar command from the agenda, with the date associated to
the cursor position."
  (require 'diary-lib)
  (unless (get-text-property (point) 'day)
    (error "Don't know which date to use for calendar command"))
  (let* ((oldf (symbol-function 'calendar-cursor-to-date))
	 (point (point))
	 (date (calendar-gregorian-from-absolute
		(get-text-property point 'day)))
	 (displayed-day (extract-calendar-day date))
	 (displayed-month (extract-calendar-month date))
	 (displayed-year (extract-calendar-year date)))
      (unwind-protect
	  (progn
	    (fset 'calendar-cursor-to-date
		  (lambda (&optional error)
		    (calendar-gregorian-from-absolute
		     (get-text-property point 'day))))
	    (call-interactively cmd))
	(fset 'calendar-cursor-to-date oldf))))

(defun org-agenda-phases-of-moon ()
  "Display the phases of the moon for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'calendar-phases-of-moon))

(defun org-agenda-holidays ()
  "Display the holidays for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'list-calendar-holidays))

(defun org-agenda-sunrise-sunset (arg)
  "Display sunrise and sunset for the cursor date.
Latitude and longitude can be specified with the variables
`calendar-latitude' and `calendar-longitude'.  When called with prefix
argument, latitude and longitude will be prompted for."
  (interactive "P")
  (let ((calendar-longitude (if arg nil calendar-longitude))
	(calendar-latitude  (if arg nil calendar-latitude))
	(calendar-location-name
	 (if arg "the given coordinates" calendar-location-name)))
    (org-agenda-execute-calendar-command 'calendar-sunrise-sunset)))

(defun org-agenda-goto-calendar ()
  "Open the Emacs calendar with the date at the cursor."
  (interactive)
  (let* ((day (or (get-text-property (point) 'day)
		  (error "Don't know which date to open in calendar")))
	 (date (calendar-gregorian-from-absolute day)))
    (calendar)
    (calendar-goto-date date)))

(defun org-calendar-goto-agenda ()
  "Compute the Org-mode agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'."
  (interactive)
  (org-agenda nil (calendar-absolute-from-gregorian
		   (calendar-cursor-to-date))))

(defun org-agenda-convert-date ()
  (interactive)
  (let ((day (get-text-property (point) 'day))
	date s)
    (unless day
      (error "Don't know which date to convert"))
    (setq date (calendar-gregorian-from-absolute day))
    (setq s (concat
	     "Gregorian:  " (calendar-date-string date) "\n"
	     "ISO:        " (calendar-iso-date-string date) "\n"
	     "Day of Yr:  " (calendar-day-of-year-string date) "\n"
	     "Julian:     " (calendar-julian-date-string date) "\n"
	     "Astron. JD: " (calendar-astro-date-string date)
	     " (Julian date number at noon UTC)\n"
	     "Hebrew:     " (calendar-hebrew-date-string date) " (until sunset)\n"
	     "Islamic:    " (calendar-islamic-date-string date) " (until sunset)\n"
	     "French:     " (calendar-french-date-string date) "\n"
	     "Mayan:      " (calendar-mayan-date-string date) "\n"
	     "Coptic:     " (calendar-coptic-date-string date) "\n"
	     "Ethiopic:   " (calendar-ethiopic-date-string date) "\n"
	     "Persian:    " (calendar-persian-date-string date) "\n"
	     "Chinese:    " (calendar-chinese-date-string date) "\n"))
    (with-output-to-temp-buffer "*Dates*"
      (princ s))
    (fit-window-to-buffer (get-buffer-window "*Dates*"))))

;;; Link Stuff

(defun org-find-file-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point 'in-emacs))

(defun org-open-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point))

(defun org-open-at-point (&optional in-emacs)
  "Open link at or after point.
If there is no link at point, this function will search forward up to
the end of the current subtree.
Normally, files will be opened by an appropriate application.  If the
optional argument IN-EMACS is non-nil, Emacs will visit the file."
  (interactive "P")
  (if (org-at-timestamp-p)
      (org-agenda nil (time-to-days (org-time-string-to-time
				     (substring (match-string 1) 0 10)))
		  1)
    (let (type path line (pos (point)))
      (save-excursion
	(skip-chars-backward
	 (concat (if org-allow-space-in-links "^" "^ ")
		 org-non-link-chars))
	(if (re-search-forward
	     org-link-regexp
	     (save-excursion
	       (condition-case nil
		   (progn (outline-end-of-subtree) (max pos (point)))
		 (error (end-of-line 1) (point))))
	     t)
	    (setq type (match-string 1)
		  path (match-string 2)))
	(unless path
	  (error "No link found"))
	;; Remove any trailing spaces in path
	(if (string-match " +\\'" path)
	    (setq path (replace-match "" t t path)))

	(cond

	 ((string= type "file")
	  (if (string-match ":\\([0-9]+\\)\\'" path)
	      (setq line (string-to-number (match-string 1 path))
		    path (substring path 0 (match-beginning 0))))
	  (org-open-file path in-emacs line))

	 ((string= type "news")
	  (org-follow-gnus-link path))

	 ((string= type "bbdb")
	  (org-follow-bbdb-link path))

	 ((string= type "gnus")
	  (let (group article)
	    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
		(error "Error in Gnus link"))
	    (setq group (match-string 1 path)
		  article (match-string 3 path))
	    (org-follow-gnus-link group article)))

	 ((string= type "vm")
	  (let (folder article)
	    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
		(error "Error in VM link"))
	    (setq folder (match-string 1 path)
		  article (match-string 3 path))
	    ;; in-emacs is the prefix arg, will be interpreted as read-only
	    (org-follow-vm-link folder article in-emacs)))

	 ((string= type "wl")
	  (let (folder article)
	    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
		(error "Error in Wanderlust link"))
	    (setq folder (match-string 1 path)
		  article (match-string 3 path))
	    (org-follow-wl-link folder article)))

	 ((string= type "rmail")
	  (let (folder article)
	    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
		(error "Error in RMAIL link"))
	    (setq folder (match-string 1 path)
		  article (match-string 3 path))
	    (org-follow-rmail-link folder article)))

	 ((string= type "shell")
	  (let ((cmd path))
	    (while (string-match "@{" cmd)
	      (setq cmd (replace-match "<" t t cmd)))
	    (while (string-match "@}" cmd)
	      (setq cmd (replace-match ">" t t cmd)))
	    (if (or (not org-confirm-shell-links)
		    (yes-or-no-p (format "Execute \"%s\" in the shell? " cmd)))
		(shell-command cmd)
	      (error "Abort"))))

	 (t
	  (browse-url-at-point)))))))

(defun org-follow-bbdb-link (name)
  "Follow a BBDB link to NAME."
  (require 'bbdb)
  (let ((inhibit-redisplay t))
    (catch 'exit
      ;; Exact match on name
      (bbdb-name (concat "\\`" name "\\'") nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Exact match on name
      (bbdb-company (concat "\\`" name "\\'") nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Partial match on name
      (bbdb-name name nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Partial match on company
      (bbdb-company name nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; General match including network address and notes
      (bbdb name nil)
      (when (= 0 (buffer-size (get-buffer "*BBDB*")))
	(delete-window (get-buffer-window "*BBDB*"))
	(error "No matching BBDB record")))))

(defun org-follow-gnus-link (&optional group article)
  "Follow a Gnus link to GROUP and ARTICLE."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if group (gnus-fetch-group group))
  (if article
      (or (gnus-summary-goto-article article nil 'force)
	  (if (fboundp 'gnus-summary-insert-cached-articles)
	      (progn
		(gnus-summary-insert-cached-articles)
		(gnus-summary-goto-article article nil 'force))
	    (message "Message could not be found.")))))

(defun org-follow-vm-link (&optional folder article readonly)
  "Follow a VM link to FOLDER and ARTICLE."
  (require 'vm)
  (if (string-match "^//\\([a-zA-Z]+@\\)?\\([^:]+\\):\\(.*\\)" folder)
      ;; ange-ftp or efs or tramp access
      (let ((user (or (match-string 1 folder) (user-login-name)))
	    (host (match-string 2 folder))
	    (file (match-string 3 folder)))
	(cond
	 ((featurep 'tramp)
	  ;; use tramp to access the file
	  (if org-xemacs-p
	      (setq folder (format "[%s@%s]%s" user host file))
	    (setq folder (format "/%s@%s:%s" user host file))))
	 (t
	  ;; use ange-ftp or efs
	  (require (if org-xemacs-p 'efs 'ange-ftp))
	  (setq folder (format "/%s@%s:%s" user host file))))))
  (when folder
    (funcall (cdr (assq 'vm org-link-frame-setup)) folder readonly)
    (sit-for 0.1)
    (when article
      (vm-select-folder-buffer)
      (widen)
      (let ((case-fold-search t))
	(goto-char (point-min))
	(if (not (re-search-forward
		  (concat "^" "message-id: *" (regexp-quote article))))
	    (error "Could not find the specified message in this folder"))
	(vm-isearch-update)
	(vm-isearch-narrow)
	(vm-beginning-of-message)
	(vm-summarize)))))

(defun org-follow-wl-link (folder article)
  "Follow a Wanderlust link to FOLDER and ARTICLE."
  (wl-summary-goto-folder-subr folder 'no-sync t nil t)
  (if article (wl-summary-jump-to-msg-by-message-id article))
  (wl-summary-redisplay))

(defun org-follow-rmail-link (folder article)
  "Follow an RMAIL link to FOLDER and ARTICLE."
  (let (message-number)
    (save-excursion
      (save-window-excursion
	(rmail (if (string= folder "RMAIL") rmail-file-name folder))
	(setq message-number
	      (save-restriction
		(widen)
		(goto-char (point-max))
		(if (re-search-backward
		     (concat "^Message-ID:\\s-+" (regexp-quote
						  (or article "")))
		     nil t)
		    (rmail-what-message))))))
    (if message-number
	(progn
	  (rmail (if (string= folder "RMAIL") rmail-file-name folder))
	  (rmail-show-message message-number)
	  message-number)
      (error "Message not found"))))

(defun org-open-file (path &optional in-emacs line)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.
If no application is found, Emacs simply visits the file.
With optional argument IN-EMACS, Emacs will visit the file.
If the file does not exist, an error is thrown."
  (let* ((file (convert-standard-filename (org-expand-file-name path)))
	 (dfile (downcase file))
	 ext cmd apps)
    (if (and (not (file-exists-p file))
	     (not org-open-non-existing-files))
	(error "No such file: %s" file))
    (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\.gz\\)$" dfile)
	(setq ext (match-string 1 dfile))
      (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\)$" dfile)
	  (setq ext (match-string 1 dfile))))
    (setq apps (append org-file-apps (org-default-apps)))
    (if in-emacs
	(setq cmd 'emacs)
      (setq cmd (or (cdr (assoc ext apps))
		    (cdr (assoc t apps)))))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      (setq cmd (format cmd (concat "\"" file "\"")))
      (save-window-excursion
	(shell-command (concat cmd " & &"))))
     ((or (stringp cmd)
	  (eq cmd 'emacs))
      (funcall (cdr (assq 'file org-link-frame-setup)) file)
      (if line (goto-line line)))
     ((consp cmd)
      (eval cmd))
     (t (funcall (cdr (assq 'file org-link-frame-setup)) file)))))

(defun org-default-apps ()
  "Return the default applications for this operating system."
  (cond
   ((eq system-type 'darwin)
    org-file-apps-defaults-macosx)
   ((eq system-type 'windows-nt)
    org-file-apps-defaults-windowsnt)
   (t org-file-apps-defaults-gnu)))

(defun org-expand-file-name (path)
  "Replace special path abbreviations and expand the file name."
  (expand-file-name path))


(defvar org-insert-link-history nil
  "Minibuffer history for links inserted with `org-insert-link'.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

;;;###autoload
(defun org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link can later be inserted into an org-buffer with
\\[org-insert-link].
For some link types, a prefix arg is interpreted:
For links to usenet articles, arg negates `org-usenet-links-prefer-google'.
For file links, arg negates `org-line-numbers-in-file-links'."
  (interactive "P")
  (let (link cpltxt)
    (cond

     ((eq major-mode 'bbdb-mode)
      (setq cpltxt (concat
		    "bbdb:"
		    (or (bbdb-record-name (bbdb-current-record))
			(bbdb-record-company (bbdb-current-record))))
	    link (org-make-link cpltxt)))

     ((eq major-mode 'calendar-mode)
      (let ((cd (calendar-cursor-to-date)))
	(setq link
	      (format-time-string
	       (car org-time-stamp-formats)
	       (apply 'encode-time
		      (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			    nil nil nil))))))

     ((or (eq major-mode 'vm-summary-mode)
	  (eq major-mode 'vm-presentation-mode))
      (and (eq major-mode 'vm-presentation-mode) (vm-summarize))
      (vm-follow-summary-cursor)
      (save-excursion
       (vm-select-folder-buffer)
       (let* ((message (car vm-message-pointer))
	      (folder (buffer-file-name))
	      (subject (vm-su-subject message))
	      (author (vm-su-full-name message))
	      (message-id (vm-su-message-id message)))
	 (setq folder (abbreviate-file-name folder))
	 (if (string-match (concat "^" (regexp-quote vm-folder-directory))
			   folder)
	     (setq folder (replace-match "" t t folder)))
	 (setq cpltxt (concat author " on: " subject))
	 (setq link (concat cpltxt "\n  "
			    (org-make-link
			     "vm:" folder "#" message-id))))))

     ((eq major-mode 'wl-summary-mode)
      (let* ((msgnum (wl-summary-message-number))
	     (message-id (elmo-message-field wl-summary-buffer-elmo-folder
					     msgnum 'message-id))
	     (wl-message-entity (elmo-msgdb-overview-get-entity
				 msgnum (wl-summary-buffer-msgdb)))
	     (author (wl-summary-line-from)) ; FIXME: how to get author name?
	     (subject "???"))   ; FIXME: How to get subject of email?
	(setq cpltxt (concat author  " on: " subject))
	(setq link (concat cpltxt "\n  "
			   (org-make-link
			    "wl:" wl-summary-buffer-folder-name
			   "#" message-id)))))

     ((eq major-mode 'rmail-mode)
      (save-excursion
	(save-restriction
	  (rmail-narrow-to-non-pruned-header)
	  (let ((folder (buffer-file-name))
		(message-id (mail-fetch-field "message-id"))
		(author (mail-fetch-field "from"))
		(subject (mail-fetch-field "subject")))
	    (setq cpltxt (concat author  " on: " subject))
	    (setq link (concat cpltxt "\n  "
			       (org-make-link
				"rmail:" folder "#" message-id)))))))

     ((eq major-mode 'gnus-group-mode)
      (let ((group (cond ((fboundp 'gnus-group-group-name) ; depending on Gnus
			  (gnus-group-group-name))         ; version
			 ((fboundp 'gnus-group-name)
			  (gnus-group-name))
			 (t "???"))))
	(setq cpltxt (concat
		      (if (org-xor arg org-usenet-links-prefer-google)
			  "http://groups.google.com/groups?group="
			"gnus:")
		      group)
	      link (org-make-link cpltxt))))

     ((memq major-mode '(gnus-summary-mode gnus-article-mode))
      (and (eq major-mode 'gnus-article-mode) (gnus-article-show-summary))
      (gnus-summary-beginning-of-article)
      (let* ((group (car gnus-article-current))
	     (article (cdr gnus-article-current))
	     (header (gnus-summary-article-header article))
	     (author (mail-header-from header))
	     (message-id (mail-header-id header))
	     (date (mail-header-date header))
	     (subject (gnus-summary-subject-string)))
	(setq cpltxt (concat author " on: " subject))
	(if (org-xor arg org-usenet-links-prefer-google)
	    (setq link
		  (concat
		   cpltxt "\n  "
		   (format "http://groups.google.com/groups?as_umsgid=%s"
			   (org-fixup-message-id-for-http message-id))))
	  (setq link (concat cpltxt "\n"
			     (org-make-link
			      "gnus:" group
			      "#" (number-to-string article)))))))

     ((eq major-mode 'w3-mode)
      (setq cpltxt (url-view-url t)
	    link (org-make-link cpltxt)))
     ((eq major-mode 'w3m-mode)
      (setq cpltxt w3m-current-url
	    link (org-make-link cpltxt)))

     ((buffer-file-name)
      ;; Just link to this file here.
      (setq cpltxt (concat "file:"
			   (abbreviate-file-name (buffer-file-name))))
      ;; Add the line number?
      (if (org-xor org-line-numbers-in-file-links arg)
	  (setq cpltxt
		(concat cpltxt
			":" (int-to-string
			     (+ (if (bolp) 1 0) (count-lines
						 (point-min) (point)))))))
      (setq link (org-make-link cpltxt)))

     ((interactive-p)
      (error "Cannot link to a buffer which is not visiting a file"))

     (t (setq link nil)))

    (if (and (interactive-p) link)
	(progn
	  (setq org-stored-links
		(cons (cons (or cpltxt link) link) org-stored-links))
	  (message "Stored: %s" (or cpltxt link)))
      link)))

(defun org-make-link (&rest strings)
  "Concatenate STRINGS, format resulting string with `org-link-format'."
  (format org-link-format (apply 'concat strings)))

(defun org-xor (a b)
  "Exclusive or."
  (if a (not b) b))

(defun org-get-header (header)
  "Find a header field in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t) s)
      (cond
       ((eq header 'from)
	(if (re-search-forward "^From:\\s-+\\(.*\\)" nil t)
	    (setq s (match-string 1)))
	(while (string-match "\"" s)
	  (setq s (replace-match "" t t s)))
	(if (string-match "[<(].*" s)
	    (setq s (replace-match "" t t s))))
       ((eq header 'message-id)
	(if (re-search-forward "^message-id:\\s-+\\(.*\\)" nil t)
	    (setq s (match-string 1))))
       ((eq header 'subject)
	(if (re-search-forward "^subject:\\s-+\\(.*\\)" nil t)
	    (setq s (match-string 1)))))
      (if (string-match "\\`[ \t\]+" s) (setq s (replace-match "" t t s)))
      (if (string-match "[ \t\]+\\'" s) (setq s (replace-match "" t t s)))
      s)))


(defun org-fixup-message-id-for-http (s)
  "Replace special characters in a message id, so it can be used in an http query."
  (while (string-match "<" s)
    (setq s (replace-match "%3C" t t s)))
  (while (string-match ">" s)
    (setq s (replace-match "%3E" t t s)))
  (while (string-match "@" s)
    (setq s (replace-match "%40" t t s)))
  s)

(defun org-insert-link (&optional complete-file)
  "Insert a link.  At the prompt, enter the link.

Completion can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press RET at the prompt), the link defaults to the most recently
stored link.  As SPC triggers completion in the minibuffer, you need to
use M-SPC or C-q SPC to force the insertion of a space character.

With a \\[universal-argument] prefix, prompts for a file to link to.  The file name can be
selected using completion.  The path to the file will be relative to
the current directory if the file is in the current directory or a
subdirectory.  Otherwise, the link will be the absolute path as
completed in the minibuffer (i.e. normally ~/path/to/file).

With two \\[universal-argument] prefixes, enforce an absolute path even if the file
is in the current directory or below."
  (interactive "P")
  (let ((link (if complete-file
		  (read-file-name "File: ")
		(completing-read
		 "Link: " org-stored-links nil nil nil
		 org-insert-link-history
		 (or (car (car org-stored-links))))))
	linktxt matched)
    (if (or (not link) (equal link ""))
      (error "No links available"))
    (if complete-file
	(let ((pwd (file-name-as-directory (expand-file-name "."))))
	  (cond
	   ((equal complete-file '(16))
	    (insert
	     (org-make-link
	      "file:" (abbreviate-file-name (expand-file-name link)))))
	   ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
			  (expand-file-name link))
	    (insert
	     (org-make-link
	      "file:" (match-string 1 (expand-file-name link)))))
	   (t (insert (org-make-link "file:" link)))))
      (setq linktxt (cdr (assoc link org-stored-links)))
      (if (not org-keep-stored-link-after-insertion)
	  (setq org-stored-links (delq (assoc link org-stored-links)
				       org-stored-links)))
      (if (not linktxt) (setq link (org-make-link link)))
      (let ((lines (org-split-string (or linktxt link) "\n")))
	(insert (car lines))
	(setq matched (string-match org-link-regexp (car lines)))
	(setq lines (cdr lines))
	(while lines
	  (insert "\n")
	  (if (save-excursion
		(beginning-of-line 0)
		(looking-at "[ \t]+\\S-"))
	      (indent-relative))
	  (setq matched (or matched
			    (string-match org-link-regexp (car lines))))
	  (insert (car lines))
	  (setq lines (cdr lines))))
      (unless matched
	(error "Add link type: http(s),ftp,mailto,file,news,bbdb,vm,wl,rmail,gnus, or shell")))))

;;; Hooks for remember.el
;;;###autoload
(defun org-remember-annotation ()
  "Return a link to the current location as an annotation for remember.el.
If you are using Org-mode files as target for data storage with
remember.el, then the annotations should include a link compatible with the
conventions in Org-mode.  This function returns such a link."
  (org-store-link nil))

(defconst org-remember-help
"Select a destination location for the note.
UP/DOWN=headline   TAB=cycle visibility  [Q]uit   RET/<left>/<right>=Store
RET at beg-of-buf -> Append to file as level 2 headline
RET on headline   -> Store as sublevel entry to current headline
<left>/<right>    -> before/after current headline, same headings level")

;;;###autoload
(defun org-remember-handler ()
  "Store stuff from remember.el into an org file.
First prompts for an org file.  If the user just presses return, the value
of `org-default-notes-file' is used.
Then the command offers the headings tree of the selected file in order to
file the text at a specific location.
You can either immediately press RET to get the note appended to the
file.  Or you can use vertical cursor motion and visibility cycling (TAB) to
find a better place.  Then press RET or <left> or <right> in insert the note.

Key      Cursor position   Note gets inserted
-----------------------------------------------------------------------------
RET      buffer-start      as level 2 heading at end of file
RET      on headline       as sublevel of the heading at cursor
RET      no heading        at cursor position, level taken from context.
			   Or use prefix arg to specify level manually.
<left>   on headline       as same level, before current heading
<right>  on headline       as same level, after current heading

So the fastest way to store the note is to press RET RET to append it to
the default file.  This way your current train of thought is not
interrupted, in accordance with the principles of remember.el.  But with
little extra effort, you can push it directly to the correct location.

Before being stored away, the function ensures that the text has a
headline, i.e. a first line that starts with a \"*\".  If not, a headline
is constructed from the current date and some additional data.

If the variable `org-adapt-indentation' is non-nil, the entire text is
also indented so that it starts in the same column as the headline
\(i.e. after the stars).

See also the variable `org-reverse-note-order'."
  (catch 'quit
    (let* ((txt (buffer-substring (point-min) (point-max)))
	   (fastp current-prefix-arg)
	   (file (if fastp org-default-notes-file (org-get-org-file)))
	   (visiting (find-buffer-visiting file))
	   (org-startup-with-deadline-check nil)
	   (org-startup-folded nil)
	   spos level indent reversed)
      ;; Modify text so that it becomes a nice subtree which can be inserted
      ;; into an org tree.
      (let* ((lines (split-string txt "\n"))
	     (first (car lines))
	     (lines (cdr lines)))
	(if (string-match "^\\*+" first)
	    ;; Is already a headline
	    (setq indent (make-string (- (match-end 0) (match-beginning 0)
					 -1) ?\ ))
	  ;; We need to add a headline:  Use time and first buffer line
	  (setq lines (cons first lines)
		first (concat "* " (current-time-string)
			      " (" (remember-buffer-desc) ")")
		indent "  "))
	(if org-adapt-indentation
	    (setq lines (mapcar (lambda (x) (concat indent x)) lines)))
	(setq txt (concat first "\n"
			  (mapconcat 'identity lines "\n"))))
      ;; Find the file
      (if (not visiting)
	  (find-file-noselect file))
      (with-current-buffer (get-file-buffer file)
	(setq reversed (org-notes-order-reversed-p))
	(save-excursion
	  (save-restriction
	    (widen)
	    ;; Ask the User for a location
	    (setq spos (if fastp 1 (org-get-location
				    (current-buffer)
				    org-remember-help)))
	    (if (not spos) (throw 'quit nil)) ; return nil to show we did
					      ; not handle this note
	    (goto-char spos)
	    (cond ((bobp)
		   ;; Put it at the start or end, as level 2
		   (save-restriction
		     (widen)
		     (goto-char (if reversed (point-min) (point-max)))
		     (if (not (bolp)) (newline))
		     (org-paste-subtree (or current-prefix-arg 2) txt)))
		  ((and (org-on-heading-p nil) (not current-prefix-arg))
		   ;; Put it below this entry, at the beg/end of the subtree
		   (org-back-to-heading)
		   (setq level (outline-level))
		   (if reversed
		       (outline-end-of-heading)
		     (outline-end-of-subtree))
		   (if (not (bolp)) (newline))
		   (beginning-of-line 1)
		   (org-paste-subtree (1+ level) txt))
		  (t
		   ;; Put it right there, with automatic level determined by
		   ;; org-paste-subtree or from prefix arg
		   (org-paste-subtree current-prefix-arg txt)))
	    (when remember-save-after-remembering
	      (save-buffer)
	      (if (not visiting) (kill-buffer (current-buffer)))))))))
  t)    ;; return t to indicate that we took care of this note.

(defun org-get-org-file ()
  "Read a filename, with default directory `org-directory'."
  (let ((default (or org-default-notes-file remember-data-file)))
    (read-file-name (format "File name [%s]: " default)
		    (file-name-as-directory org-directory)
		    default)))

(defun org-notes-order-reversed-p ()
  "Check if the current file should receive notes in reversed order."
  (cond
   ((not org-reverse-note-order) nil)
   ((eq t org-reverse-note-order) t)
   ((not (listp org-reverse-note-order)) nil)
   (t (catch 'exit
	(let  ((all org-reverse-note-order)
	       entry)
	  (while (setq entry (pop all))
	    (if (string-match (car entry) (buffer-file-name))
		(throw 'exit (cdr entry))))
	  nil)))))

;;; Tables

;; Watch out:  Here we are talking about two different kind of tables.
;; Most of the code is for the tables created with the Org-mode table editor.
;; Sometimes, we talk about tables created and edited with the table.el
;; Emacs package.  We call the former org-type tables, and the latter
;; table.el-type tables.


(defun org-before-change-function (beg end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))

(defconst org-table-line-regexp "^[ \t]*|"
  "Detects an org-type table line.")
(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detects an org-type table line.")
(defconst org-table-auto-recalculate-regexp "^[ \t]*| *# *\\(|\\|$\\)"
  "Detects a table line marked for automatic recalculation.")
(defconst org-table-recalculate-regexp "^[ \t]*| *[#*] *\\(|\\|$\\)"
  "Detects a table line marked for automatic recalculation.")
(defconst org-table-calculate-mark-regexp "^[ \t]*| *[!$^_#*] *\\(|\\|$\\)"
  "Detects a table line marked for automatic recalculation.")
(defconst org-table-hline-regexp "^[ \t]*|-"
  "Detects an org-type table hline.")
(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detects a table-type table hline.")
(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detects an org-type or table-type table.")
(defconst org-table-border-regexp "^[ \t]*[^| \t]"
  "Searching from within a table (any type) this finds the first line
outside the table.")
(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Searching from within a table (any type) this finds the first line
outside the table.")

(defun org-table-create-with-table.el ()
  "Use the table.el package to insert a new table.
If there is already a table at point, convert between Org-mode tables
and table.el tables."
  (interactive)
  (require 'table)
  (cond
   ((org-at-table.el-p)
    (if (y-or-n-p "Convert table to Org-mode table? ")
	(org-table-convert)))
   ((org-at-table-p)
    (if (y-or-n-p "Convert table to table.el table? ")
	(org-table-convert)))
   (t (call-interactively 'table-insert))))

(defun org-table-create (&optional size)
  "Query for a size and insert a table skeleton.
SIZE is a string Columns x Rows like for example \"3x2\"."
  (interactive "P")
  (unless size
    (setq size (read-string
		(concat "Table size Columns x Rows [e.g. "
			org-table-default-size "]: ")
		"" nil org-table-default-size)))

  (let* ((pos (point))
	 (indent (make-string (current-column) ?\ ))
	 (split (org-split-string size " *x *"))
	 (rows (string-to-number (nth 1 split)))
	 (columns (string-to-number (car split)))
	 (line (concat (apply 'concat indent "|" (make-list columns "  |"))
		       "\n")))
    (if (string-match "^[ \t]*$" (buffer-substring-no-properties
				  (point-at-bol) (point)))
	(beginning-of-line 1)
      (newline))
    ;; (mapcar (lambda (x) (insert line)) (make-list rows t))
    (dotimes (i rows) (insert line))
    (goto-char pos)
    (if (> rows 1)
	;; Insert a hline after the first row.
	(progn
	  (end-of-line 1)
	  (insert "\n|-")
	  (goto-char pos)))
    (org-table-align)))

(defun org-table-convert-region (beg0 end0 nspace)
  "Convert region to a table.
The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is included.
When NSPACE is non-nil, it indicates the minimum number of spaces that
separate columns (default: just one space)"
  (let* ((beg (min beg0 end0))
	 (end (max beg0 end0))
	 (tabsep t)
	 re)
    (goto-char beg)
    (beginning-of-line 1)
    (setq beg (move-marker (make-marker) (point)))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (move-marker (make-marker) (point)))
    ;; Lets see if this is tab-separated material.  If every nonempty line
    ;; contains a tab, we will assume that it is tab-separated material
    (if nspace
	(setq tabsep nil)
      (goto-char beg)
      (and (re-search-forward "^[^\n\t]+$" end t) (setq tabsep nil)))
    (if nspace (setq tabsep nil))
    (if tabsep
	(setq re "^\\|\t")
      (setq re (format "^ *\\| *\t *\\| \\{%d,\\}"
		       (max 1 (prefix-numeric-value nspace)))))
    (goto-char beg)
    (while (re-search-forward re end t)
      (replace-match "|" t t))
    (goto-char beg)
    (insert " ")
    (org-table-align)))

(defun org-table-import (file arg)
  "Import FILE as a table.
The file is assumed to be tab-separated.  Such files can be produced by most
spreadsheet and database applications.  If no tabs (at least one per line)
are found, lines will be split on whitespace into fields."
  (interactive "f\nP")
  (or (bolp) (newline))
  (let ((beg (point))
	(pm (point-max)))
    (insert-file-contents file)
    (org-table-convert-region beg (+ (point) (- (point-max) pm)) arg)))

(defun org-table-export ()
  "Export table as a tab-separated file.
Such a file can be imported into a spreadsheet program like Excel."
  (interactive)
  (let* ((beg (org-table-begin))
	 (end (org-table-end))
	 (table (buffer-substring beg end))
	 (file (read-file-name "Export table to: "))
	 buf)
    (unless (or (not (file-exists-p file))
		(y-or-n-p (format "Overwrite file %s? " file)))
      (error "Abort"))
    (with-current-buffer (find-file-noselect file)
      (setq buf (current-buffer))
      (erase-buffer)
      (fundamental-mode)
      (insert table)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|[ \t]*" nil t)
	(replace-match "" t t)
	(end-of-line 1))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*$" nil t)
	(replace-match "" t t)
	(goto-char (min (1+ (point)) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "^-[-+]*$" nil t)
	(replace-match "")
	(if (looking-at "\n")
	    (delete-char 1)))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*" nil t)
	(replace-match "\t" t t))
      (save-buffer))
    (kill-buffer buf)))

(defvar org-table-aligned-begin-marker (make-marker)
  "Marker at the beginning of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")
(defvar org-table-aligned-end-marker (make-marker)
  "Marker at the end of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")
(defvar org-table-last-alignment nil
  "List of flags for flushright alignment, from the last re-alignment.
This is being used to correctly align a single field after TAB or RET.")
;; FIXME:  The following is currently not used.
(defvar org-table-last-column-widths nil
  "List of max width of fields in each column.
This is being used to correctly align a single field after TAB or RET.")

(defvar org-last-recalc-line nil)

(defun org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  ;; (message "align") (sit-for 2)
  (let* (
	 ;; Limits of table
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
	 (colpos (org-table-current-column))
	 (winstart (window-start))
	 text lines (new "") lengths l typenums ty fields maxfields i
	 column
	 (indent "") cnt frac
	 rfmt hfmt
	 (spaces (if (org-in-invisibility-spec-p '(org-table))
		     org-table-spaces-around-invisible-separators
		   org-table-spaces-around-separators))
	 (sp1 (car spaces))
	 (sp2 (cdr spaces))
	 (rfmt1 (concat
		 (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
	 (hfmt1 (concat
		 (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
	 emptystrings)
    (untabify beg end)
    ;; (message "Aligning table...")
    ;; Get the rows
    (setq lines (org-split-string
		 (buffer-substring-no-properties beg end) "\n"))
    ;; Store the indentation of the first line
    (if (string-match "^ *" (car lines))
	(setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    ;; Mark the hlines
    (setq lines (mapcar (lambda (l)
			  (if (string-match "^ *|-" l)
			      nil
			    (if (string-match "[ \t]+$" l)
				(substring l 0 (match-beginning 0))
			      l)))
			lines))
    ;; Get the data fields
    (setq fields (mapcar
		  (lambda (l)
		      (org-split-string l " *| *"))
		  (delq nil (copy-sequence lines))))
    ;; How many fields in the longest line?
    (condition-case nil
	(setq maxfields (apply 'max (mapcar 'length fields)))
      (error
       (kill-region beg end)
       (org-table-create org-table-default-size)
       (error "Empty table - created default table")))
    ;; A list of empty string to fill any short rows on output
    (setq emptystrings (make-list maxfields ""))
    ;; Get the maximum length of a field and the most common datatype
    ;; for each column
    (setq i -1)
    (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
      (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
      ;; maximum length
      (push (apply 'max 1 (mapcar 'length column)) lengths)
      ;; compute the fraction stepwise, ignoring empty fields
      (setq cnt 0 frac 0.0)
      (mapcar
       (lambda (x)
	 (if (equal x "")
	     nil
	   (setq frac ( / (+ (* frac cnt)
			     (if (string-match org-table-number-regexp x) 1 0))
			  (setq cnt (1+ cnt))))))
       column)
      (push (>= frac org-table-number-fraction) typenums))
    (setq lengths (nreverse lengths)
	  typenums (nreverse typenums))
    (setq org-table-last-alignment typenums
	  org-table-last-column-widths lengths)
    ;; Compute the formats needed for output of the table
    (setq rfmt (concat indent "|") hfmt (concat indent "|"))
    (while (setq l (pop lengths))
      (setq ty (if (pop typenums) "" "-")) ; number types flushright
      (setq rfmt (concat rfmt (format rfmt1 ty l))
	    hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
    (setq rfmt (concat rfmt "\n")
	  hfmt (concat (substring hfmt 0 -1) "|\n"))
    ;; Produce the new table
    ;;(while lines
    ;;  (setq l (pop lines))
    ;;  (if l
    ;;      (setq new (concat new (apply 'format rfmt
    ;;                                   (append (pop fields) emptystrings))))
    ;;    (setq new (concat new hfmt))))
    (setq new (mapconcat
	       (lambda (l)
		 (if l (apply 'format rfmt
			      (append (pop fields) emptystrings))
		   hfmt))
	       lines ""))
    ;; Replace the old one
    (delete-region beg end)
    (move-marker end nil)
    (move-marker org-table-aligned-begin-marker (point))
    (insert new)
    (move-marker org-table-aligned-end-marker (point))
    ;; Try to move to the old location (approximately)
    (goto-line linepos)
    (set-window-start (selected-window) winstart 'noforce)
    (org-table-goto-column colpos)
    (setq org-table-may-need-update nil)
    (if (org-in-invisibility-spec-p '(org-table))
	(org-table-add-invisible-to-vertical-lines))
    ))

(defun org-table-begin (&optional table-type)
  "Find the beginning of the table and return its position.
With argument TABLE-TYPE, go to the beginning of a table.el-type table."
  (save-excursion
    (if (not (re-search-backward
	      (if table-type org-table-any-border-regexp
		org-table-border-regexp)
	      nil t))
	(error "Can't find beginning of table")
      (goto-char (match-beginning 0))
      (beginning-of-line 2)
      (point))))

(defun org-table-end (&optional table-type)
  "Find the end of the table and return its position.
With argument TABLE-TYPE, go to the end of a table.el-type table."
  (save-excursion
    (if (not (re-search-forward
	      (if table-type org-table-any-border-regexp
		org-table-border-regexp)
	      nil t))
	(goto-char (point-max))
      (goto-char (match-beginning 0)))
    (point-marker)))

(defun org-table-justify-field-maybe (&optional new)
  "Justify the current field, text to left, number to right.
Optional argument NEW may specify text to replace the current field content."
  (cond
   ((and (not new) org-table-may-need-update)) ; Realignment will happen anyway
   ((org-at-table-hline-p)
    ;; FIXME:  I used to enforce realign here, but I think this is not needed.
    ;; (setq org-table-may-need-update t)
    )
   ((and (not new)
	 (or (not (equal (marker-buffer org-table-aligned-begin-marker)
			 (current-buffer)))
	     (< (point) org-table-aligned-begin-marker)
	     (>= (point) org-table-aligned-end-marker)))
    ;; This is not the same table, force a full re-align
    (setq org-table-may-need-update t))
   (t ;; realign the current field, based on previous full realign
    (let* ((pos (point)) s
	   (col (org-table-current-column))
	   (num (nth (1- col) org-table-last-alignment))
	   l f n o e)
      (when (> col 0)
	(skip-chars-backward "^|\n")
	(if (looking-at " *\\([^|\n]*?\\) *\\(|\\|$\\)")
	    (progn
	      (setq s (match-string 1)
		    o (match-string 0)
		    l (max 1 (- (match-end 0) (match-beginning 0) 3))
		    e (not (= (match-beginning 2) (match-end 2))))
	      (setq f (format (if num " %%%ds %s" " %%-%ds %s")
			      l (if e "|" (setq org-table-may-need-update t) ""))
		    n (format f s t t))
	      (if new
		  (if (<= (length new) l)
		      (setq n (format f new t t))  ;; FIXME: why t t?????
		    (setq n (concat new "|") org-table-may-need-update t)))
	      (or (equal n o)
		  (let (org-table-may-need-update)
		    (replace-match n))))
	  (setq org-table-may-need-update t))
	(goto-char pos))))))

(defun org-table-next-field ()
  "Go to the next field in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (if (org-at-table-hline-p)
      (end-of-line 1))
  (condition-case nil
      (progn
	(re-search-forward "|" (org-table-end))
	(if (looking-at "[ \t]*$")
	    (re-search-forward "|" (org-table-end)))
	(if (looking-at "-")
	    (progn
	      (beginning-of-line 0)
	      (org-table-insert-row 'below))
	  (if (looking-at " ") (forward-char 1))))
    (error
     (org-table-insert-row 'below))))

(defun org-table-previous-field ()
  "Go to the previous field in the table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (if (org-at-table-hline-p)
      (end-of-line 1))
  (re-search-backward "|" (org-table-begin))
  (re-search-backward "|" (org-table-begin))
  (while (looking-at "|\\(-\\|[ \t]*$\\)")
    (re-search-backward "|" (org-table-begin)))
  (if (looking-at "| ?")
      (goto-char (match-end 0))))

(defun org-table-next-row ()
  "Go to the next row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (or (looking-at "[ \t]*$")
	  (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
    (if (and org-table-automatic-realign
	     org-table-may-need-update)
	(org-table-align))
    (let ((col (org-table-current-column)))
      (beginning-of-line 2)
      (if (or (not (org-at-table-p))
	      (org-at-table-hline-p))
	  (progn
	    (beginning-of-line 0)
	    (org-table-insert-row 'below)))
      (org-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (if (looking-at " ") (forward-char 1)))))

(defun org-table-copy-down (n)
  "Copy a field down in the current column.
If the field at the cursor is empty, copy into it the content of the nearest
non-empty field above.  With argument N, use the Nth non-empty field.
If the current field is not empty, it is copied down to the next row, and
the cursor is moved with it.  Therefore, repeating this command causes the
column to be filled row-by-row.
If the variable `org-table-copy-increment' is non-nil and the field is an
integer, it will be incremented while copying."
  (interactive "p")
  (let* ((colpos (org-table-current-column))
	 (field (org-table-get-field))
	 (non-empty (string-match "[^ \t]" field))
	 (beg (org-table-begin))
	 txt)
    (org-table-check-inside-data-field)
    (if non-empty
	(progn
	  (setq txt (org-trim field))
	  (org-table-next-row)
	  (org-table-blank-field))
      (save-excursion
	(setq txt
	      (catch 'exit
		(while (progn (beginning-of-line 1)
			      (re-search-backward org-table-dataline-regexp
						  beg t))
		  (org-table-goto-column colpos t)
		  (if (and (looking-at
			    "|[ \t]*\\([^| \t][^|]*?\\)[ \t]*|")
			   (= (setq n (1- n)) 0))
		      (throw 'exit (match-string 1))))))))
    (if txt
	(progn
	  (if (and org-table-copy-increment
		   (string-match "^[0-9]+$" txt))
	      (setq txt (format "%d" (+ (string-to-number txt) 1))))
	  (insert txt)
	  (org-table-maybe-recalculate-line)
	  (org-table-align))
      (error "No non-empty field found"))))

(defun org-table-check-inside-data-field ()
  "Is point inside a table data field?
I.e. not on a hline or before the first or after the last column?
This actually throws an error, so it aborts the current command."
  (if (or (not (org-at-table-p))
	  (= (org-table-current-column) 0)
	  (org-at-table-hline-p)
	  (looking-at "[ \t]*$"))
      (error "Not in table data field")))

(defvar org-table-clip nil
  "Clipboard for table regions.")

(defun org-table-blank-field ()
  "Blank the current table field or active region."
  (interactive)
  (org-table-check-inside-data-field)
  (if (and (interactive-p) (org-region-active-p))
      (let (org-table-clip)
	(org-table-cut-region (region-beginning) (region-end)))
    (skip-chars-backward "^|")
    (backward-char 1)
    (if (looking-at "|[^|\n]+")
	(let* ((pos (match-beginning 0))
	       (match (match-string 0))
	       (len (length match)))
	  (replace-match (concat "|" (make-string (1- len) ?\ )))
	  (goto-char (+ 2 pos))
	  (substring match 1)))))

(defun org-table-get-field (&optional n replace)
  "Return the value of the field in column N of current row.
N defaults to current field.
If REPLACE is a string, replace field with this value.  The return value
is always the old value."
  (and n (org-table-goto-column n))
  (skip-chars-backward "^|\n")
  (backward-char 1)
  (if (looking-at "|[^|\r\n]*")
      (let* ((pos (match-beginning 0))
	     (val (buffer-substring (1+ pos) (match-end 0))))
	(if replace
	    (replace-match (concat "|" replace)))
	(goto-char (min (point-at-eol) (+ 2 pos)))
	val)
    (forward-char 1) ""))

(defun org-table-current-column ()
  "Find out which column we are in.
When called interactively, column is also displayed in echo area."
  (interactive)
  (if (interactive-p) (org-table-check-inside-data-field))
  (save-excursion
    (let ((cnt 0) (pos (point)))
      (beginning-of-line 1)
      (while (search-forward "|" pos t)
	(setq cnt (1+ cnt)))
      (if (interactive-p) (message "This is table column %d" cnt))
      cnt)))

(defun org-table-goto-column (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (let ((pos (point-at-eol)))
    (beginning-of-line 1)
    (when (> n 0)
      (while (and (> (setq n (1- n)) -1)
		  (or (search-forward "|" pos t)
		      (and force
			   (progn (end-of-line 1)
				  (skip-chars-backward "^|")
				  (insert " | "))))))
;                                  (backward-char 2) t)))))
      (when (and force (not (looking-at ".*|")))
	(save-excursion (end-of-line 1) (insert " | ")))
      (if on-delim
	  (backward-char 1)
	(if (looking-at " ") (forward-char 1))))))

(defun org-at-table-p (&optional table-type)
  "Return t if the cursor is inside an org-type table.
If TABLE-TYPE is non-nil, also chack for table.el-type tables."
  (if org-enable-table-editor
      (save-excursion
	(beginning-of-line 1)
	(looking-at (if table-type org-table-any-line-regexp
		      org-table-line-regexp)))
    nil))

(defun org-table-recognize-table.el ()
  "If there is a table.el table nearby, recognize it and move into it."
  (if org-table-tab-recognizes-table.el
      (if (org-at-table.el-p)
	  (progn
	    (beginning-of-line 1)
	    (if (looking-at org-table-dataline-regexp)
		nil
	      (if (looking-at org-table1-hline-regexp)
		  (progn
		    (beginning-of-line 2)
		    (if (looking-at org-table-any-border-regexp)
			(beginning-of-line -1)))))
	    (if (re-search-forward "|" (org-table-end t) t)
		(progn
		  (require 'table)
		  (if (table--at-cell-p (point))
		      t
		    (message "recognizing table.el table...")
		    (table-recognize-table)
		    (message "recognizing table.el table...done")))
	      (error "This should not happen..."))
	    t)
	nil)
    nil))

(defun org-at-table.el-p ()
  "Return t if the cursor is inside a table.el-type table."
  (save-excursion
    (if (org-at-table-p 'any)
	(progn
	  (goto-char (org-table-begin 'any))
	  (looking-at org-table1-hline-regexp))
      nil)))

(defun org-at-table-hline-p ()
  "Return t if the cursor is inside a hline in a table."
  (if org-enable-table-editor
      (save-excursion
	(beginning-of-line 1)
	(looking-at org-table-hline-regexp))
    nil))

(defun org-table-insert-column ()
  "Insert a new column into the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (let* ((col (max 1 (org-table-current-column)))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
	 (colpos col))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
	  nil
	(org-table-goto-column col t)
	(insert "|   "))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos)
    (org-table-align)
    (org-table-modify-formulas 'insert col)))

(defun org-table-find-dataline ()
  "Find a dataline in the current table, which is needed for column commands."
  (if (and (org-at-table-p)
	   (not (org-at-table-hline-p)))
      t
    (let ((col (current-column))
	  (end (org-table-end)))
      (move-to-column col)
      (while (and (< (point) end)
		  (or (not (= (current-column) col))
		      (org-at-table-hline-p)))
	(beginning-of-line 2)
	(move-to-column col))
      (if (and (org-at-table-p)
	       (not (org-at-table-hline-p)))
	  t
	(error
	 "Please position cursor in a data line for column operations")))))

(defun org-table-delete-column ()
  "Delete a column into the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
	 (colpos col))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
	  nil
	(org-table-goto-column col t)
	(and (looking-at "|[^|\n]+|")
	     (replace-match "|")))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos)
    (org-table-align)
    (org-table-modify-formulas 'remove col)))

(defun org-table-move-column-right ()
  "Move column to the right."
  (interactive)
  (org-table-move-column nil))
(defun org-table-move-column-left ()
  "Move column to the left."
  (interactive)
  (org-table-move-column 'left))

(defun org-table-move-column (&optional left)
  "Move the current column to the right.  With arg LEFT, move to the left."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (col1 (if left (1- col) col))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
	 (colpos (if left (1- col) (1+ col))))
    (if (and left (= col 1))
	(error "Cannot move column further left"))
    (if (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
	(error "Cannot move column further right"))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
	  nil
	(org-table-goto-column col1 t)
	(and (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
	     (replace-match "|\\2|\\1|")))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos)
    (org-table-align)
    (org-table-modify-formulas 'swap col (if left (1- col) (1+ col)))))

(defun org-table-move-row-down ()
  "move table row down."
  (interactive)
  (org-table-move-row nil))
(defun org-table-move-row-up ()
  "move table row up."
  (interactive)
  (org-table-move-row 'up))

(defun org-table-move-row (&optional up)
  "Move the current table line down.  With arg UP, move it up."
  (interactive "P")
  (let ((col (current-column))
	(pos (point))
	(tonew (if up 0 2))
	txt)
    (beginning-of-line tonew)
    (if (not (org-at-table-p))
	(progn
	  (goto-char pos)
	  (error "Cannot move row further")))
    (goto-char pos)
    (beginning-of-line 1)
    (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt)
    (beginning-of-line 0)
    (move-to-column col)))

(defun org-table-insert-row (&optional arg)
  "Insert a new row above the current line into the table.
With prefix ARG, insert below the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	 new)
    (if (string-match "^[ \t]*|-" line)
	(setq new (mapcar (lambda (x) (if (member x '(?| ?+)) ?| ?\ )) line))
      (setq new (mapcar (lambda (x) (if (equal x ?|) ?| ?\ )) line)))
    ;; Fix the first field if necessary
    (setq new (concat new))
    (if (string-match "^[ \t]*| *[#$] *|" line)
	(setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line (if arg 2 1))
    (let (org-table-may-need-update)
      (insert-before-markers new)
      (insert-before-markers "\n"))
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and org-table-may-need-update (org-table-align))))

(defun org-table-insert-hline (&optional arg)
  "Insert a horizontal-line below the current line into the table.
With prefix ARG, insert above the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	(col (current-column))
	start)
    (if (string-match "^[ \t]*|-" line)
	(setq line
	      (mapcar (lambda (x) (if (member x '(?| ?+))
				      (prog1 (if start ?+ ?|) (setq start t))
				    (if start ?- ?\ )))
		      line))
      (setq line
	    (mapcar (lambda (x) (if (equal x ?|)
				    (prog1 (if start ?+ ?|) (setq start t))
				    (if start ?- ?\ )))
		    line)))
    (beginning-of-line (if arg 1 2))
    (apply 'insert line)
    (if (equal (char-before (point)) ?+)
	(progn (backward-delete-char 1) (insert "|")))
    (insert "\n")
    (beginning-of-line 0)
    (move-to-column col)))

(defun org-table-kill-row ()
  "Delete the current row or horizontal line from the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((col (current-column)))
    (kill-region (point-at-bol) (min (1+ (point-at-eol)) (point-max)))
    (if (not (org-at-table-p)) (beginning-of-line 0))
    (move-to-column col)))


(defun org-table-cut-region (beg end)
  "Copy region in table to the clipboard and blank all relevant fields."
  (interactive "r")
  (org-table-copy-region beg end 'cut))

(defun org-table-copy-region (beg end &optional cut)
  "Copy rectangular region in table to clipboard.
A special clipboard is used which can only be accessed
with `org-table-paste-rectangle'"
  (interactive "rP")
  (let* (l01 c01 l02 c02 l1 c1 l2 c2 ic1 ic2
	 region cols
	 (rpl (if cut "  " nil)))
    (goto-char beg)
    (org-table-check-inside-data-field)
    (setq l01 (count-lines (point-min) (point))
	  c01 (org-table-current-column))
    (goto-char end)
    (org-table-check-inside-data-field)
    (setq l02 (count-lines (point-min) (point))
	  c02 (org-table-current-column))
    (setq l1 (min l01 l02) l2 (max l01 l02)
	  c1 (min c01 c02) c2 (max c01 c02))
    (catch 'exit
      (while t
	(catch 'nextline
	  (if (> l1 l2) (throw 'exit t))
	  (goto-line l1)
	  (if (org-at-table-hline-p) (throw 'nextline (setq l1 (1+ l1))))
	  (setq cols nil ic1 c1 ic2 c2)
	  (while (< ic1 (1+ ic2))
	    (push (org-table-get-field ic1 rpl) cols)
	    (setq ic1 (1+ ic1)))
	  (push (nreverse cols) region)
	  (setq l1 (1+ l1)))))
    (setq org-table-clip (nreverse region))
    (if cut (org-table-align))
    org-table-clip))

(defun org-table-paste-rectangle ()
  "Paste a rectangular region into a table.
The upper right corner ends up in the current field.  All involved fields
will be overwritten.  If the rectangle does not fit into the present table,
the table is enlarged as needed.  The process ignores horizontal separator
lines."
  (interactive)
  (unless (and org-table-clip (listp org-table-clip))
    (error "First cut/copy a region to paste!"))
  (org-table-check-inside-data-field)
  (let* ((clip org-table-clip)
	 (line (count-lines (point-min) (point)))
	 (col (org-table-current-column))
	 (org-enable-table-editor t)
	 (org-table-automatic-realign nil)
	 c cols field)
    (while (setq cols (pop clip))
      (while (org-at-table-hline-p) (beginning-of-line 2))
      (if (not (org-at-table-p))
	  (progn (end-of-line 0) (org-table-next-field)))
      (setq c col)
      (while (setq field (pop cols))
	(org-table-goto-column c nil 'force)
	(org-table-get-field nil field)
	(setq c (1+ c)))
      (beginning-of-line 2))
    (goto-line line)
    (org-table-goto-column col)
    (org-table-align)))

(defun org-table-convert ()
  "Convert from `org-mode' table to table.el and back.
Obviously, this only works within limits.  When an Org-mode table is
converted to table.el, all horizontal separator lines get lost, because
table.el uses these as cell boundaries and has no notion of horizontal lines.
A table.el table can be converted to an Org-mode table only if it does not
do row or column spanning.  Multiline cells will become multiple cells.
Beware, Org-mode does not test if the table can be successfully converted - it
blindly applies a recipe that works for simple tables."
  (interactive)
  (require 'table)
  (if (org-at-table.el-p)
      ;; convert to Org-mode table
      (let ((beg (move-marker (make-marker) (org-table-begin t)))
	    (end (move-marker (make-marker) (org-table-end t))))
	(table-unrecognize-region beg end)
	(goto-char beg)
	(while (re-search-forward "^\\([ \t]*\\)\\+-.*\n" end t)
	  (replace-match ""))
	(goto-char beg))
    (if (org-at-table-p)
	;; convert to table.el table
	(let ((beg (move-marker (make-marker) (org-table-begin)))
	      (end (move-marker (make-marker) (org-table-end))))
	  ;; first, get rid of all horizontal lines
	  (goto-char beg)
	  (while (re-search-forward "^\\([ \t]*\\)|-.*\n" end t)
	    (replace-match ""))
	  ;; insert a hline before first
	  (goto-char beg)
	  (org-table-insert-hline 'above)
	  ;; insert a hline after each line
	  (while (progn (beginning-of-line 2) (< (point) end))
	    (org-table-insert-hline))
	  (goto-char beg)
	  (setq end (move-marker end (org-table-end)))
	  ;; replace "+" at beginning and ending of hlines
	  (while (re-search-forward "^\\([ \t]*\\)|-" end t)
	    (replace-match "\\1+-"))
	  (goto-char beg)
	  (while (re-search-forward "-|[ \t]*$" end t)
	    (replace-match "-+"))
	  (goto-char beg)))))

(defun org-table-wrap-region (arg)
  "Wrap several fields in a column like a paragraph.
This is useful if you'd like to spread the contents of a field over several
lines, in order to keep the table compact.

If there is an active region, and both point and mark are in the same column,
the text in the column is wrapped to minimum width for the given number of
lines.  Generally, this makes the table more compact.  A prefix ARG may be
used to change the number of desired lines.  For example, `C-2 \\[org-table-wrap]'
formats the selected text to two lines.  If the region was longer than 2
lines, the remaining lines remain empty.  A negative prefix argument reduces
the current number of lines by that amount.  The wrapped text is pasted back
into the table.  If you formatted it to more lines than it was before, fields
further down in the table get overwritten - so you might need to make space in
the table first.

If there is no region, the current field is split at the cursor position and
the text fragment to the right of the cursor is prepended to the field one
line down.

If there is no region, but you specify a prefix ARG, the current field gets
blank, and the content is appended to the field above."
  (interactive "P")
  (org-table-check-inside-data-field)
  (if (org-region-active-p)
      ;; There is a region:  fill as a paragraph
      (let ((beg (region-beginning))
	    nlines)
	(org-table-cut-region (region-beginning) (region-end))
	(if (> (length (car org-table-clip)) 1)
	    (error "Region must be limited to single column"))
	(setq nlines (if arg
			 (if (< arg 1)
			     (+ (length org-table-clip) arg)
			   arg)
		       (length org-table-clip)))
	(setq org-table-clip
	      (mapcar 'list (org-wrap (mapconcat 'car org-table-clip " ")
				      nil nlines)))
	(goto-char beg)
	(org-table-paste-rectangle))
    ;; No region, split the current field at point
    (if arg
	;; combine with field above
	(let ((s (org-table-blank-field))
	      (col (org-table-current-column)))
	  (beginning-of-line 0)
	  (while (org-at-table-hline-p) (beginning-of-line 0))
	  (org-table-goto-column col)
	  (skip-chars-forward "^|")
	  (skip-chars-backward " ")
	  (insert " " (org-trim s))
	  (org-table-align))
      ;;  split field
      (when (looking-at "\\([^|]+\\)+|")
	(let ((s (match-string 1)))
	  (replace-match " |")
	  (goto-char (match-beginning 0))
	  (org-table-next-row)
	  (insert (org-trim s) " ")
	  (org-table-align))))))

(defun org-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "^[ \t]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t]+$" s) (setq s (replace-match "" t t s)))
  s)

(defun org-wrap (string &optional width lines)
  "Wrap string to either a number of lines, or a width in characters.
If WIDTH is non-nil, the string is wrapped to that width, however many lines
that costs.  If there is a word longer than WIDTH, the text is actually
wrapped to the length of that word.
IF WIDTH is nil and LINES is non-nil, the string is forced into at most that
many lines, whatever width that takes.
The return value is a list of lines, without newlines at the end."
  (let* ((words (org-split-string string "[ \t\n]+"))
	 (maxword (apply 'max (mapcar 'length words)))
	 w ll)
    (cond (width
	   (org-do-wrap words (max maxword width)))
	  (lines
	   (setq w maxword)
	   (setq ll (org-do-wrap words maxword))
	   (if (<= (length ll) lines)
	       ll
	     (setq ll words)
	     (while (> (length ll) lines)
	       (setq w (1+ w))
	       (setq ll (org-do-wrap words w)))
	     ll))
	  (t (error "Cannot wrap this")))))


(defun org-do-wrap (words width)
  "Create lines of maximum width WIDTH (in characters) from word list WORDS."
  (let (lines line)
    (while words
      (setq line (pop words))
      (while (and words (< (+ (length line) (length (car words))) width))
	(setq line (concat line " " (pop words))))
      (setq lines (push line lines)))
    (nreverse lines)))

;; FIXME: I think I can make this more efficient
(defun org-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
No empty strings are returned if there are matches at the beginning
and end of string."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun org-table-add-invisible-to-vertical-lines ()
  "Add an `invisible' property to vertical lines of current table."
  (interactive)
  (let* ((beg (org-table-begin))
	 (end (org-table-end))
	 (end1))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(setq end1 (point-at-eol))
	(if (looking-at org-table-dataline-regexp)
	    (while (re-search-forward "|" end1 t)
	      (add-text-properties (1- (point)) (point)
				   '(invisible org-table)))
	  (while (re-search-forward "[+|]" end1 t)
	    (add-text-properties (1- (point)) (point)
				 '(invisible org-table))))
	(beginning-of-line 2)))))

(defun org-table-toggle-vline-visibility (&optional arg)
  "Toggle the visibility of table vertical lines.
The effect is immediate and on all tables in the file.
With prefix ARG, make lines invisible when ARG is positive, make lines
visible when ARG is not positive"
  (interactive "P")
  (let ((action (cond
		 ((and arg (> (prefix-numeric-value arg) 0)) 'on)
		 ((and arg (< (prefix-numeric-value arg) 1)) 'off)
		 (t (if (org-in-invisibility-spec-p '(org-table))
			'off
		      'on)))))
    (if (eq action 'off)
	(progn
	  (org-remove-from-invisibility-spec '(org-table))
	  (org-table-map-tables 'org-table-align)
	  (message "Vertical table lines visible")
	  (if (org-at-table-p)
	      (org-table-align)))
      (org-add-to-invisibility-spec '(org-table))
      (org-table-map-tables 'org-table-align)
      (message "Vertical table lines invisible"))
    (redraw-frame (selected-frame))))

(defun org-table-map-tables (function)
  "Apply FUNCTION to the start of all tables in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward org-table-any-line-regexp nil t)
	(message "Mapping tables: %d%%" (/ (* 100.0 (point)) (buffer-size)))
	(beginning-of-line 1)
	(if (looking-at org-table-line-regexp)
	    (save-excursion (funcall function)))
	(re-search-forward org-table-any-border-regexp nil 1)))))

(defun org-table-sum (&optional beg end nlast)
  "Sum numbers in region of current table column.
The result will be displayed in the echo area, and will be available
as kill to be inserted with \\[yank].

If there is an active region, it is interpreted as a rectangle and all
numbers in that rectangle will be summed.  If there is no active
region and point is located in a table column, sum all numbers in that
column.

If at least one number looks like a time HH:MM or HH:MM:SS, all other
numbers are assumed to be times as well (in decimal hours) and the
numbers are added as such.

If NLAST is a number, only the NLAST fields will actually be summed."
  (interactive)
  (save-excursion
    (let (col (timecnt 0) diff h m s org-table-clip)
      (cond
       ((and beg end))   ; beg and end given explicitly
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       (t
	(setq col (org-table-current-column))
	(goto-char (org-table-begin))
	(unless (re-search-forward "^[ \t]*|[^-]" nil t)
	  (error "No table data"))
	(org-table-goto-column col)
;not needed?        (skip-chars-backward "^|")
	(setq beg (point))
	(goto-char (org-table-end))
	(unless (re-search-backward "^[ \t]*|[^-]" nil t)
	  (error "No table data"))
	(org-table-goto-column col)
;not needed?        (skip-chars-forward "^|")
	(setq end (point))))
      (let* ((items (apply 'append (org-table-copy-region beg end)))
	     (items1 (cond ((not nlast) items)
			   ((>= nlast (length items)) items)
			   (t (setq items (reverse items))
			      (setcdr (nthcdr (1- nlast) items) nil)
			      (nreverse items))))
	     (numbers (delq nil (mapcar 'org-table-get-number-for-summing
					items1)))
	     (res (apply '+ numbers))
	     (sres (if (= timecnt 0)
		       (format "%g" res)
		     (setq diff (* 3600 res)
			   h (floor (/ diff 3600)) diff (mod diff 3600)
			   m (floor (/ diff 60)) diff (mod diff 60)
			   s diff)
		     (format "%d:%02d:%02d" h m s))))
	(kill-new sres)
	(if (interactive-p)
	    (message "%s"
		     (substitute-command-keys
		      (format "Sum of %d items: %-20s     (\\[yank] will insert result into buffer)"
			      (length numbers) sres))))
	sres))))

(defun org-table-get-number-for-summing (s)
  (let (n)
    (if (string-match "^ *|? *" s)
	(setq s (replace-match "" nil nil s)))
    (if (string-match " *|? *$" s)
	(setq s (replace-match "" nil nil s)))
    (setq n (string-to-number s))
    (cond
     ((and (string-match "0" s)
	   (string-match "\\`[-+ \t0.edED]+\\'" s)) 0)
     ((string-match "\\`[ \t]+\\'" s)         nil)
     ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\'" s)
      (let ((h (string-to-number (or (match-string 1 s) "0")))
	    (m (string-to-number (or (match-string 2 s) "0")))
	    (s (string-to-number (or (match-string 4 s) "0"))))
	(if (boundp 'timecnt) (setq timecnt (1+ timecnt)))
	(* 1.0 (+ h (/ m 60.0) (/ s 3600.0)))))
     ((equal n 0)                             nil)
     (t n))))

(defun org-table-get-vertical-vector (desc &optional tbeg col)
  "Get a calc vector from a column, accorting to desctiptor DESC.
Optional arguments TBEG and COL can give the beginning of the table and
the current column, to avoid unnecessary parsing."
  (save-excursion
    (or tbeg (setq tbeg (org-table-begin)))
    (or col (setq col (org-table-current-column)))
    (let (beg end nn n n1 n2 l (thisline (org-current-line)) hline-list)
      (cond
       ((string-match "\\(I+\\)\\(-\\(I+\\)\\)?" desc)
	(setq n1 (- (match-end 1) (match-beginning 1)))
	(if (match-beginning 3)
	    (setq n2 (- (match-end 2) (match-beginning 3))))
	(setq n (if n2 (max n1 n2) n1))
	(setq n1 (if n2 (min n1 n2)))
	(setq nn n)
	(while (and (> nn 0)
		    (re-search-backward org-table-hline-regexp tbeg t))
	  (push (org-current-line) hline-list)
	  (setq nn (1- nn)))
	(setq hline-list (nreverse hline-list))
	(goto-line (nth (1- n) hline-list))
	(when (re-search-forward org-table-dataline-regexp)
	  (org-table-goto-column col)
	  (setq beg (point)))
	(goto-line (if n1 (nth (1- n1) hline-list) thisline))
	(when (re-search-backward org-table-dataline-regexp)
	  (org-table-goto-column col)
	  (setq end (point)))
	(setq l (apply 'append (org-table-copy-region beg end)))
	(concat "[" (mapconcat (lambda (x) (setq x (org-trim x))
				 (if (equal x "") "0" x))
			       l ",") "]"))
       ((string-match "\\([0-9]+\\)-\\([0-9]+\\)" desc)
	(setq n1 (string-to-number (match-string 1 desc))
	      n2 (string-to-number (match-string 2 desc)))
	(beginning-of-line 1)
	(save-excursion
	  (when (re-search-backward org-table-dataline-regexp tbeg t n1)
	    (org-table-goto-column col)
	    (setq beg (point))))
	(when (re-search-backward org-table-dataline-regexp tbeg t n2)
	  (org-table-goto-column col)
	  (setq end (point)))
	(setq l (apply 'append (org-table-copy-region beg end)))
	(concat "[" (mapconcat
		     (lambda (x) (setq x (org-trim x))
		       (if (equal x "") "0" x))
		     l ",") "]"))
       ((string-match "\\([0-9]+\\)" desc)
	(beginning-of-line 1)
	(when (re-search-backward org-table-dataline-regexp tbeg t
				  (string-to-number (match-string 0 desc)))
	  (org-table-goto-column col)
	  (org-trim (org-table-get-field))))))))

(defvar org-table-formula-history nil)

(defvar org-table-column-names nil
  "Alist with column names, derived from the `!' line.")
(defvar org-table-column-name-regexp nil
  "Regular expression matching the current column names.")
(defvar org-table-local-parameters nil
  "Alist with parameter names, derived from the `$' line.")
(defvar org-table-named-field-locations nil
  "Alist with locations of named fields.")

(defun org-table-get-formula (&optional equation named)
  "Read a formula from the minibuffer, offer stored formula as default."
  (let* ((name (car (rassoc (list (org-current-line)
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (scol (if named
		   (if name name
		     (error "Not in a named field"))
		 (int-to-string (org-table-current-column))))
	 (dummy (and name (not named)
		     (not (y-or-n-p "Replace named-field formula with column equation? " ))
		 (error "Abort")))
	 (org-table-may-need-update nil)
	 (stored-list (org-table-get-stored-formulas))
	 (stored (cdr (assoc scol stored-list)))
	 (eq (cond
	      ((and stored equation (string-match "^ *=? *$" equation))
	       stored)
	      ((stringp equation)
	       equation)
	      (t (read-string
		  (format "%s formula $%s=" (if named "Field" "Column") scol)
		  (or stored "") 'org-table-formula-history
		  ;stored
		  ))))
	 mustsave)
    (when (not (string-match "\\S-" eq))
      ;; remove formula
      (setq stored-list (delq (assoc scol stored-list) stored-list))
      (org-table-store-formulas stored-list)
      (error "Formula removed"))
    (if (string-match "^ *=?" eq) (setq eq (replace-match "" t t eq)))
    (if (string-match " *$" eq) (setq eq (replace-match "" t t eq)))
    (if (and name (not named))
	;; We set the column equation, delete the named one.
	(setq stored-list (delq (assoc name stored-list) stored-list)
	      mustsave t))
    (if stored
	(setcdr (assoc scol stored-list) eq)
      (setq stored-list (cons (cons scol eq) stored-list)))
    (if (or mustsave (not (equal stored eq)))
	(org-table-store-formulas stored-list))
    eq))

(defun org-table-store-formulas (alist)
  "Store the list of formulas below the current table."
  (setq alist (sort alist (lambda (a b) (string< (car a) (car b)))))
  (save-excursion
    (goto-char (org-table-end))
    (if (looking-at "\\([ \t]*\n\\)*#\\+TBLFM:.*\n?")
	(delete-region (point) (match-end 0)))
    (insert "#+TBLFM: "
	    (mapconcat (lambda (x)
			 (concat "$" (car x) "=" (cdr x)))
		       alist "::")
	    "\n")))

(defun org-table-get-stored-formulas ()
  "Return an alist with the t=stored formulas directly after current table."
  (interactive)
  (let (scol eq eq-alist strings string seen)
    (save-excursion
      (goto-char (org-table-end))
      (when (looking-at "\\([ \t]*\n\\)*#\\+TBLFM: *\\(.*\\)")
	(setq strings (org-split-string (match-string 2) " *:: *"))
	(while (setq string (pop strings))
	  (when (string-match "\\$\\([a-zA-Z0-9]+\\) *= *\\(.*[^ \t]\\)" string)
	    (setq scol (match-string 1 string)
		  eq (match-string 2 string)
		  eq-alist (cons (cons scol eq) eq-alist))
	    (if (member scol seen)
		(error "Double definition `$%s=' in TBLFM line, please fix by hand" scol)
	      (push scol seen))))))
    (nreverse eq-alist)))

(defun org-table-modify-formulas (action &rest columns)
  "Modify the formulas stored below the current table.
ACTION can be `remove', `insert', `swap'.  For `swap', two column numbers are
expected, for the other action only a single column number is needed."
  (let ((list (org-table-get-stored-formulas))
	(nmax (length (org-split-string
		       (buffer-substring (point-at-bol) (point-at-eol))
		       "|")))
	col col1 col2 scol si sc1 sc2)
    (cond
     ((null list))   ; No action needed if there are no stored formulas
     ((eq action 'remove)
      (setq col (car columns)
	    scol (int-to-string col))
      (org-table-replace-in-formulas list scol "INVALID")
      (if (assoc scol list) (setq list (delq (assoc scol list) list)))
      (loop for i from (1+ col) upto nmax by 1 do
	    (setq si (int-to-string i))
	    (org-table-replace-in-formulas list si (int-to-string (1- i)))
	    (if (assoc si list) (setcar (assoc si list)
					(int-to-string (1- i))))))
     ((eq action 'insert)
      (setq col (car columns))
      (loop for i from nmax downto col by 1 do
	    (setq si (int-to-string i))
	    (org-table-replace-in-formulas list si (int-to-string (1+ i)))
	    (if (assoc si list) (setcar (assoc si list)
					(int-to-string (1+ i))))))
     ((eq action 'swap)
      (setq col1 (car columns) col2 (nth 1 columns)
	    sc1 (int-to-string col1) sc2 (int-to-string col2))
      ;; Hopefully, ZqZ will never be a name in a table...  FIXME:
      (org-table-replace-in-formulas list sc1 "ZqZ")
      (org-table-replace-in-formulas list sc2 sc1)
      (org-table-replace-in-formulas list "ZqZ" sc2)
      (if (assoc sc1 list) (setcar (assoc sc1 list) "ZqZ"))
      (if (assoc sc2 list) (setcar (assoc sc2 list) sc1))
      (if (assoc "ZqZ" list)  (setcar (assoc "ZqZ" list) sc2)))
     (t (error "Invalid action in `org-table-modify-formulas'")))
    (if list (org-table-store-formulas list))))

(defun org-table-replace-in-formulas (list s1 s2)
  (let (elt re s)
    (setq s1 (concat "$" (if (integerp s1) (int-to-string s1) s1))
	  s2 (concat "$" (if (integerp s2) (int-to-string s2) s2))
	  re (concat (regexp-quote s1) "\\>"))
    (while (setq elt (pop list))
      (setq s (cdr elt))
      (while (string-match re s)
	(setq s (replace-match s2 t t s)))
      (setcdr elt s))))

(defun org-table-get-specials ()
  "Get the column nmaes and local parameters for this table."
  (save-excursion
    (let ((beg (org-table-begin)) (end (org-table-end))
	  names name fields fields1 field cnt c v line col)
      (setq org-table-column-names nil
	    org-table-local-parameters nil
	    org-table-named-field-locations nil)
      (goto-char beg)
      (when (re-search-forward "^[ \t]*| *! *\\(|.*\\)" end t)
	(setq names (org-split-string (match-string 1) " *| *")
	      cnt 1)
	(while (setq name (pop names))
	  (setq cnt (1+ cnt))
	  (if (string-match "^[a-zA-Z][a-zA-Z0-9]*$" name)
	      (push (cons name (int-to-string cnt)) org-table-column-names))))
      (setq org-table-column-names (nreverse org-table-column-names))
      (setq org-table-column-name-regexp
	    (concat "\\$\\(" (mapconcat 'car org-table-column-names "\\|") "\\)\\>"))
      (goto-char beg)
      (while (re-search-forward "^[ \t]*| *\\$ *\\(|.*\\)" end t)
	(setq fields (org-split-string (match-string 1) " *| *"))
	(while (setq field (pop fields))
	  (if (string-match "^\\([a-zA-Z][a-zA-Z0-9]*\\|%\\) *= *\\(.*\\)" field)
	      (push (cons (match-string 1 field) (match-string 2 field))
		    org-table-local-parameters))))
      (goto-char beg)
      (while (re-search-forward "^[ \t]*| *\\([_^]\\) *\\(|.*\\)" end t)
	(setq c (match-string 1)
	      fields (org-split-string (match-string 2) " *| *"))
	(save-excursion
	  (beginning-of-line (if (equal c "_") 2 0))
	  (setq line (org-current-line) col 1)
	  (and (looking-at "^[ \t]*|[^|]*\\(|.*\\)")
	       (setq fields1 (org-split-string (match-string 1) " *| *"))))
	(while (and fields1 (setq field (pop fields)))
	  (setq v (pop fields1) col (1+ col))
	  (when (and (stringp field) (stringp v)
		     (string-match "^[a-zA-Z][a-zA-Z0-9]*$" field))
	      (push (cons field v) org-table-local-parameters)
	      (push (list field line col) org-table-named-field-locations)))))))

(defun org-this-word ()
  ;; Get the current word
  (save-excursion
    (let ((beg (progn (skip-chars-backward "^ \t\n") (point)))
	  (end (progn (skip-chars-forward "^ \t\n") (point))))
      (buffer-substring-no-properties beg end))))

(defun org-table-maybe-eval-formula ()
  "Check if the current field starts with \"=\" or \":=\".
If yes, store the formula and apply it."
  ;; We already know we are in a table.  Get field will only return a formula
  ;; when appropriate.  It might return a separator line, but no problem.
  (when org-table-formula-evaluate-inline
    (let* ((field (org-trim (or (org-table-get-field) "")))
	   named eq)
      (when (string-match "^:?=\\(.*\\)" field)
	(setq named (equal (string-to-char field) ?:)
	      eq (match-string 1 field))
	(if (fboundp 'calc-eval)
	    (org-table-eval-formula (if named '(4) nil) eq))))))

(defvar org-recalc-commands nil
  "List of commands triggering the reccalculation of a line.
Will be filled automatically during use.")

(defvar org-recalc-marks
  '((" " . "Unmarked: no special line, no automatic recalculation")
    ("#" . "Automatically recalculate this line upon TAB, RET, and C-c C-c in the line")
    ("*" . "Recalculate only when entire table is recalculated with `C-u C-c *'")
    ("!" . "Column name definition line. Reference in formula as $name.")
    ("$" . "Parameter definition line name=value. Reference in formula as $name.")
    ("_" . "Names for values in row below this one.")
    ("^" . "Names for values in row above this one.")))

(defun org-table-rotate-recalc-marks (&optional newchar)
  "Rotate the recalculation mark in the first column.
If in any row, the first field is not consistent with a mark,
insert a new column for the makers.
When there is an active region, change all the lines in the region,
after prompting for the marking character.
After each change, a message will be displayed indication the meaning
of the new mark."
  (interactive)
  (unless (org-at-table-p) (error "Not at a table"))
  (let* ((marks (append (mapcar 'car org-recalc-marks) '(" ")))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 (l (org-current-line))
	 (l1 (if (org-region-active-p) (org-current-line (region-beginning))))
	 (l2 (if (org-region-active-p) (org-current-line (region-end))))
	 (have-col
	  (save-excursion
	    (goto-char beg)
	    (not (re-search-forward "^[ \t]*|[^-|][^|]*[^#!$*_^| \t][^|]*|" end t))))
	 (col (org-table-current-column))
	 (forcenew (car (assoc newchar org-recalc-marks)))
	 epos new)
    (when l1
      (message "Change region to what mark?  Type # * ! $ or SPC: ")
      (setq newchar (char-to-string (read-char-exclusive))
	    forcenew (car (assoc newchar org-recalc-marks))))
    (if (and newchar (not forcenew))
	(error "Invalid NEWCHAR `%s' in `org-table-rotate-recalc-marks'"
	       newchar))
    (if l1 (goto-line l1))
    (save-excursion
      (beginning-of-line 1)
      (unless (looking-at org-table-dataline-regexp)
	(error "Not at a table data line")))
    (unless have-col
      (org-table-goto-column 1)
      (org-table-insert-column)
      (org-table-goto-column (1+ col)))
    (setq epos (point-at-eol))
    (save-excursion
      (beginning-of-line 1)
      (org-table-get-field
       1 (if (looking-at "^[ \t]*| *\\([#!$*^_ ]\\) *|")
	     (concat " "
		     (setq new (or forcenew
				   (cadr (member (match-string 1) marks))))
		     " ")
	   " # ")))
    (if (and l1 l2)
	(progn
	  (goto-line l1)
	  (while (progn (beginning-of-line 2) (not (= (org-current-line) l2)))
	    (and (looking-at org-table-dataline-regexp)
		 (org-table-get-field 1 (concat " " new " "))))
	  (goto-line l1)))
    (if (not (= epos (point-at-eol))) (org-table-align))
    (goto-line l)
    (and (interactive-p) (message (cdr (assoc new org-recalc-marks))))))

(defun org-table-maybe-recalculate-line ()
  "Recompute the current line if marked for it, and if we haven't just done it."
  (interactive)
  (and org-table-allow-automatic-line-recalculation
       (not (and (memq last-command org-recalc-commands)
		 (equal org-last-recalc-line (org-current-line))))
       (save-excursion (beginning-of-line 1)
		       (looking-at org-table-auto-recalculate-regexp))
       (fboundp 'calc-eval)
       (org-table-recalculate) t))

(defvar org-table-formula-debug nil
  "Non-nil means, debug table formulas.
When nil, simply write \"#ERROR\" in corrupted fields.")

(defvar modes)
(defsubst org-set-calc-mode (var &optional value)
  (if (stringp var)
      (setq var (assoc var '(("D" calc-angle-mode deg)
			     ("R" calc-angle-mode rad)
			     ("F" calc-prefer-frac t)
			     ("S" calc-symbolic-mode t)))
	    value (nth 2 var) var (nth 1 var)))
  (if (memq var modes)
      (setcar (cdr (memq var modes)) value)
    (cons var (cons value modes)))
  modes)

(defun org-table-eval-formula (&optional arg equation
					 suppress-align suppress-const
					 suppress-store)
  "Replace the table field value at the cursor by the result of a calculation.

This function makes use of Dave Gillespie's calc package, in my view the
most exciting program ever written for GNU Emacs.  So you need to have calc
installed in order to use this function.

In a table, this command replaces the value in the current field with the
result of a formula.  It also installes the formula as the \"current\" column
formula, by storing it in a special line below the table.  When called
with a `C-u' prefix, the current field must ba a named field, and the
formula is installed as valid in only this specific field.

When called, the command first prompts for a formula, which is read in
the minibuffer.  Previously entered formulas are available through the
history list, and the last used formula is offered as a default.
These stored formulas are adapted correctly when moving, inserting, or
deleting columns with the corresponding commands.

The formula can be any algebraic expression understood by the calc package.
For details, see the Org-mode manual.

This function can also be called from Lisp programs and offers
additional Arguments: EQUATION can be the formula to apply.  If this
argument is given, the user will not be prompted.  SUPPRESS-ALIGN is
used to speed-up recursive calls by by-passing unnecessary aligns.
SUPPRESS-CONST suppresses the interpretation of constants in the
formula, assuming that this has been done already outside the fuction.
SUPPRESS-STORE means the formula should not be stored, either because
it is already stored, or because it is a modified equation that should
not overwrite the stored one."
  (interactive "P")
  (require 'calc)
  (org-table-check-inside-data-field)
  (org-table-get-specials)
  (let* (fields
	 (ndown (if (integerp arg) arg 1))
	 (org-table-automatic-realign nil)
	 (case-fold-search nil)
	 (down (> ndown 1))
	 (formula (if (and equation suppress-store)
		      equation
		    (org-table-get-formula equation (equal arg '(4)))))
	 (n0 (org-table-current-column))
	 (modes (copy-sequence org-calc-default-modes))
	 n form fmt x ev orig c)
    ;; Parse the format string.  Since we have a lot of modes, this is
    ;; a lot of work.  However, I think calc still uses most of the time.
    (if (string-match ";" formula)
	(let ((tmp (org-split-string formula ";")))
	  (setq formula (car tmp)
		fmt (concat (cdr (assoc "%" org-table-local-parameters))
			    (nth 1 tmp)))
	  (while (string-match "[pnfse]\\(-?[0-9]+\\)" fmt)
	    (setq c (string-to-char (match-string 1 fmt))
		  n (string-to-number (or (match-string 1 fmt) "")))
	    (if (= c ?p) (setq modes (org-set-calc-mode 'calc-internal-prec n))
	      (setq modes (org-set-calc-mode
			   'calc-float-format
			   (list (cdr (assoc c '((?n. float) (?f. fix)
						 (?s. sci) (?e. eng))))
				 n))))
	    (setq fmt (replace-match "" t t fmt)))
	  (while (string-match "[DRFS]" fmt)
	    (setq modes (org-set-calc-mode (match-string 0 fmt)))
	    (setq fmt (replace-match "" t t fmt)))
	  (unless (string-match "\\S-" fmt)
	    (setq fmt nil))))
    (if (and (not suppress-const) org-table-formula-use-constants)
	(setq formula (org-table-formula-substitute-names formula)))
    (setq orig (or (get-text-property 1 :orig-formula formula) "?"))
    (while (> ndown 0)
      (setq fields (org-split-string
		    (buffer-substring
		     (point-at-bol) (point-at-eol)) " *| *"))
      (if org-table-formula-numbers-only
	  (setq fields (mapcar
			(lambda (x) (number-to-string (string-to-number x)))
			fields)))
      (setq ndown (1- ndown))
      (setq form (copy-sequence formula))
      ;; Insert the references to fields in same row
      (while (string-match "\\$\\([0-9]+\\)?" form)
	(setq n (if (match-beginning 1)
		    (string-to-number (match-string 1 form))
		  n0)
	      x (nth (1- n) fields))
	(unless x (error "Invalid field specifier \"%s\""
			 (match-string 0 form)))
	(if (equal x "") (setq x "0"))
	(setq form (replace-match (concat "(" x ")") t t form)))
      ;; Insert ranges in current column
      (while (string-match "\\&[-I0-9]+" form)
	(setq form (replace-match
		     (save-match-data
		       (org-table-get-vertical-vector (match-string 0 form)
						      nil n0))
		     t t form)))
      (setq ev (calc-eval (cons form modes)
			  (if org-table-formula-numbers-only 'num)))

      (when org-table-formula-debug
	(with-output-to-temp-buffer "*Help*"
	  (princ (format "Substitution history of formula
Orig:   %s
$xyz->  %s
$1->    %s\n" orig formula form))
	  (if (listp ev)
	      (princ (format "       %s^\nError:  %s"
			     (make-string (car ev) ?\-) (nth 1 ev)))
	    (princ (format "Result: %s\nFormat: %s\nFinal:  %s"
			   ev (or fmt "NONE")
			   (if fmt (format fmt (string-to-number ev)) ev)))))
	(shrink-window-if-larger-than-buffer (get-buffer-window "*Help*"))
	(unless (and (interactive-p) (not ndown))
	  (unless (let (inhibit-redisplay)
		    (y-or-n-p "Debugging Formula. Continue to next? "))
	    (org-table-align)
	    (error "Abort"))
	  (delete-window (get-buffer-window "*Help*"))
	  (message "")))
      (if (listp ev) (setq fmt nil ev "#ERROR"))
      (org-table-justify-field-maybe
       (if fmt (format fmt (string-to-number ev)) ev))
      (if (and down (> ndown 0) (looking-at ".*\n[ \t]*|[^-]"))
	  (call-interactively 'org-return)
	(setq ndown 0)))
    (and down (org-table-maybe-recalculate-line))
    (or suppress-align (and org-table-may-need-update
			    (org-table-align)))))

(defun org-table-recalculate (&optional all noalign)
  "Recalculate the current table line by applying all stored formulas."
  (interactive "P")
  (or (memq this-command org-recalc-commands)
      (setq org-recalc-commands (cons this-command org-recalc-commands)))
  (unless (org-at-table-p) (error "Not at a table"))
  (org-table-get-specials)
  (let* ((eqlist (sort (org-table-get-stored-formulas)
		       (lambda (a b) (string< (car a) (car b)))))
	 (inhibit-redisplay t)
	 (line-re org-table-dataline-regexp)
	 (thisline (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
	 (thiscol (org-table-current-column))
	 beg end entry eqlnum eqlname eql (cnt 0) eq a name)
    ;; Insert constants in all formulas
    (setq eqlist
	  (mapcar (lambda (x)
		    (setcdr x (org-table-formula-substitute-names (cdr x)))
		    x)
		  eqlist))
    ;; Split the equation list
    (while (setq eq (pop eqlist))
      (if (<= (string-to-char (car eq)) ?9)
	  (push eq eqlnum)
	(push eq eqlname)))
    (setq eqlnum (nreverse eqlnum) eqlname (nreverse eqlname))
    (if all
	(progn
	  (setq end (move-marker (make-marker) (1+ (org-table-end))))
	  (goto-char (setq beg (org-table-begin)))
	  (if (re-search-forward org-table-calculate-mark-regexp end t)
	      ;; This is a table with marked lines, only compute selected lines
	      (setq line-re org-table-recalculate-regexp)
	    ;; Move forward to the first non-header line
	    (if (and (re-search-forward org-table-dataline-regexp end t)
		     (re-search-forward org-table-hline-regexp end t)
		     (re-search-forward org-table-dataline-regexp end t))
		(setq beg (match-beginning 0))
	      nil))) ;; just leave beg where it is
      (setq beg (point-at-bol)
	    end (move-marker (make-marker) (1+ (point-at-eol)))))
    (goto-char beg)
    (and all (message "Re-applying formulas to full table..."))
    (while (re-search-forward line-re end t)
      (unless (string-match "^ *[_^!$] *$" (org-table-get-field 1))
	;; Unprotected line, recalculate
	(and all (message "Re-applying formulas to full table...(line %d)"
			  (setq cnt (1+ cnt))))
	(setq org-last-recalc-line (org-current-line))
	(setq eql eqlnum)
	(while (setq entry (pop eql))
	  (goto-line org-last-recalc-line)
	  (org-table-goto-column (string-to-number (car entry)) nil 'force)
	  (org-table-eval-formula nil (cdr entry) 'noalign 'nocst 'nostore))))
    (goto-line thisline)
    (org-table-goto-column thiscol)
    (or noalign (and org-table-may-need-update (org-table-align))
	(and all (message "Re-applying formulas to %d lines...done" cnt)))
    ;; Now do the names fields
    (while (setq eq (pop eqlname))
      (setq name (car eq)
	    a (assoc name org-table-named-field-locations))
      (when a
	(message "Re-applying formula to named field: %s" name)
	(goto-line (nth 1 a))
	(org-table-goto-column (nth 2 a))
	(org-table-eval-formula nil (cdr eq) 'noalign 'nocst 'nostore)))
    ;; back to initial position
    (goto-line thisline)
    (org-table-goto-column thiscol)
    (or noalign (and org-table-may-need-update (org-table-align))
	(and all (message "Re-applying formulas...done")))))

(defun org-table-formula-substitute-names (f)
  "Replace $const with values in string F."
  (let ((start 0) a n1 n2 nn1 nn2 s (f1 f))
    ;; First, check for column names
    (while (setq start (string-match org-table-column-name-regexp f start))
      (setq start (1+ start))
      (setq a (assoc (match-string 1 f) org-table-column-names))
      (setq f (replace-match (concat "$" (cdr a)) t t f)))
    ;; Expand ranges to vectors
    (while (string-match "\\$\\([0-9]+\\)\\.\\.\\.?\\$\\([0-9]+\\)" f)
      (setq n1 (string-to-number (match-string 1 f))
	    n2 (string-to-number (match-string 2 f))
	    nn1 (1+ (min n1 n2)) nn2 (max n1 n2)
	    s (concat "[($" (number-to-string (1- nn1)) ")"))
      (loop for i from nn1 upto nn2 do
	    (setq s (concat s ",($" (int-to-string i) ")")))
      (setq s (concat s "]"))
      (if (< n2 n1) (setq s (concat "rev(" s ")")))
      (setq f (replace-match s t t f)))
    ;; Parameters and constants
    (setq start 0)
    (while (setq start (string-match "\\$\\([a-zA-Z][a-zA-Z0-9]*\\)" f start))
      (setq start (1+ start))
      (if (setq a (save-match-data
		    (org-table-get-constant (match-string 1 f))))
	  (setq f (replace-match (concat "(" a ")") t t f))))
    (if org-table-formula-debug
	(put-text-property 0 (length f) :orig-formula f1 f))
    f))

(defun org-table-get-constant (const)
  "Find the value for a parameter or constant in a formula.
Parameters get priority."
  (or (cdr (assoc const org-table-local-parameters))
      (cdr (assoc const org-table-formula-constants))
      (and (fboundp 'constants-get) (constants-get const))
      "#UNDEFINED_NAME"))

(defvar org-edit-formulas-map (make-sparse-keymap))
(define-key org-edit-formulas-map "\C-c\C-c" 'org-finish-edit-formulas)
(define-key org-edit-formulas-map "\C-c\C-q" 'org-abort-edit-formulas)
(define-key org-edit-formulas-map "\C-c?" 'org-show-variable)

(defvar org-pos)
(defvar org-window-configuration)

(defun org-table-edit-formulas ()
  "Edit the formulas of the current table in a separate buffer."
  (interactive)
  (unless (org-at-table-p)
    (error "Not at a table"))
  (org-table-get-specials)
  (let ((eql (org-table-get-stored-formulas))
	(pos (move-marker (make-marker) (point)))
	(wc (current-window-configuration))
	entry loc s)
    (switch-to-buffer-other-window "*Edit Formulas*")
    (erase-buffer)
    (fundamental-mode)
    (set (make-local-variable 'org-pos) pos)
    (set (make-local-variable 'org-window-configuration) wc)
    (use-local-map org-edit-formulas-map)
    (setq s "# Edit formulas and finish with `C-c C-c'.
# Use `C-u C-c C-c' to also appy them immediately to the entire table.
# Use `C-c ?' to get information about $name at point.
# To cancel editing, press `C-c C-q'.\n")
    (put-text-property 0 (length s) 'face 'font-lock-comment-face s)
    (insert s)
    (while (setq entry (pop eql))
      (when (setq loc (assoc (car entry) org-table-named-field-locations))
	(setq s (format "# Named formula, referring to column %d in line %d\n"
			(nth 2 loc) (nth 1 loc)))
	(put-text-property 0 (length s) 'face 'font-lock-comment-face s)
	(insert s))
      (setq s (concat "$" (car entry) "=" (cdr entry) "\n"))
      (remove-text-properties 0 (length s) '(face nil) s)
      (insert s))
    (goto-char (point-min))
    (message "Edit formulas and finish with `C-c C-c'.")))

(defun org-show-variable ()
  "Show the location/value of the $ expression at point."
  (interactive)
  (let (var (pos org-pos) (win (selected-window)) e)
    (save-excursion
      (or (looking-at "\\$") (skip-chars-backward "$a-zA-Z0-9"))
      (if (looking-at "\\$\\([a-zA-Z0-9]+\\)")
	  (setq var (match-string 1))
	(error "No variable at point")))
    (cond
     ((setq e (assoc var org-table-named-field-locations))
      (switch-to-buffer-other-window (marker-buffer pos))
      (goto-line (nth 1 e))
      (org-table-goto-column (nth 2 e))
      (select-window win)
      (message "Named field, column %d of line %d" (nth 2 e) (nth 1 e)))
     ((setq e (assoc var org-table-column-names))
      (switch-to-buffer-other-window (marker-buffer pos))
      (goto-char pos)
      (goto-char (org-table-begin))
      (if (re-search-forward (concat "^[ \t]*| *! *.*?| *\\(" var "\\) *|")
			     (org-table-end) t)
	  (progn
	    (goto-char (match-beginning 1))
	    (message "Named column (column %s)" (cdr e)))
	(error "Column name not found"))
      (select-window win))
     ((string-match "^[0-9]$" var)
      ;; column number
      (switch-to-buffer-other-window (marker-buffer pos))
      (goto-char pos)
      (goto-char (org-table-begin))
      (recenter 1)
      (if (re-search-forward org-table-dataline-regexp
			     (org-table-end) t)
	  (progn
	    (goto-char (match-beginning 0))
	    (org-table-goto-column (string-to-number var))
	    (message "Column %s" var))
	(error "Column name not found"))
      (select-window win))
     ((setq e (assoc var org-table-local-parameters))
      (switch-to-buffer-other-window (marker-buffer pos))
      (goto-char pos)
      (goto-char (org-table-begin))
      (if (re-search-forward (concat "^[ \t]*| *\\$ *.*?| *\\(" var "=\\)") nil t)
	  (progn
	    (goto-char (match-beginning 1))
	    (message "Local parameter."))
	(error "Parameter not found"))
      (select-window win))
     (t
      (cond
       ((setq e (assoc var org-table-formula-constants))
	(message "Constant: $%s=%s in `org-table-formula-constants'." var (cdr e)))
       ((setq e (and (fboundp 'constants-get) (constants-get var)))
	(message "Constant: $%s=%s, retrieved from `constants.el'." var e))
       (t (error "Undefined name $%s" var)))))))

(defun org-finish-edit-formulas (&optional arg)
  "Parse the buffer for formula definitions and install them.
With prefix ARG, apply the new formulas to the table."
  (interactive "P")
  (let ((pos org-pos) eql)
    (goto-char (point-min))
    (while (re-search-forward "^\\$\\([a-zA-Z0-9]+\\) *= *\\(.*?\\) *$" nil t)
      (push (cons (match-string 1) (match-string 2)) eql))
    (set-window-configuration org-window-configuration)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (unless (org-at-table-p)
      (error "Lost table position - cannot install formulae"))
    (org-table-store-formulas eql)
    (move-marker pos nil)
    (kill-buffer "*Edit Formulas*")
    (if arg
	(org-table-recalculate 'all)
      (message "New formulas installed - press C-u C-c C-c to apply."))))

(defun org-abort-edit-formulas ()
  "Abort editing formulas, without installing the changes."
  (interactive)
  (let ((pos org-pos))
    (set-window-configuration org-window-configuration)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (message "Formula editing aborted without installing changes")))

;;; The orgtbl minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the org-mode table editor.

;; This is really a hack, because the org-mode table editor uses several
;; keys which normally belong to the major mode, for example the TAB and
;; RET keys.  Here is how it works: The minor mode defines all the keys
;; necessary to operate the table editor, but wraps the commands into a
;; function which tests if the cursor is currently inside a table.  If that
;; is the case, the table editor command is executed.  However, when any of
;; those keys is used outside a table, the function uses `key-binding' to
;; look up if the key has an associated command in another currently active
;; keymap (minor modes, major mode, global), and executes that command.
;; There might be problems if any of the keys used by the table editor is
;; otherwise used as a prefix key.

;; Another challenge is that the key binding for TAB can be tab or \C-i,
;; likewise the binding for RET can be return or \C-m.  Orgtbl-mode
;; addresses this by checking explicitly for both bindings.

;; The optimized version (see variable `orgtbl-optimized') takes over
;; all keys which are bound to `self-insert-command' in the *global map*.
;; Some modes bind other commands to simple characters, for example
;; AUCTeX binds the double quote to `Tex-insert-quote'.  With orgtbl-mode
;; active, this binding is ignored inside tables and replaced with a
;; modified self-insert.

(defcustom orgtbl-optimized (eq org-enable-table-editor 'optimized)
  "Non-nil means, use the optimized table editor version for `orgtbl-mode'.
In the optimized version, the table editor takes over all simple keys that
normally just insert a character.  In tables, the characters are inserted
in a way to minimize disturbing the table structure (i.e. in overwrite mode
for empty fields).  Outside tables, the correct binding of the keys is
restored.

The default for this option is t if the optimized version is also used in
Org-mode.  See the variable `org-enable-table-editor' for details.  Changing
this variable requires a restart of Emacs to become effective."
  :group 'org-table
  :type 'boolean)

(defvar orgtbl-mode nil
  "Variable controlling `orgtbl-mode', a minor mode enabling the `org-mode'
table editor in arbitrary modes.")
(make-variable-buffer-local 'orgtbl-mode)

(defvar orgtbl-mode-map (make-keymap)
  "Keymap for `orgtbl-mode'.")

;;;###autoload
(defun turn-on-orgtbl ()
  "Unconditionally turn on `orgtbl-mode'."
  (orgtbl-mode 1))

;;;###autoload
(defun orgtbl-mode (&optional arg)
  "The `org-mode' table editor as a minor mode for use in other modes."
  (interactive)
  (if (eq major-mode 'org-mode)
      ;; Exit without error, in case some hook functions calls this
      ;; by accident in org-mode.
      (message "Orgtbl-mode is not useful in org-mode, command ignored")
    (setq orgtbl-mode
	  (if arg (> (prefix-numeric-value arg) 0) (not orgtbl-mode)))
    (if orgtbl-mode
	(progn
	  (and (orgtbl-setup) (defun orgtbl-setup () nil))
	  ;; Make sure we are first in minor-mode-map-alist
	  (let ((c (assq 'orgtbl-mode minor-mode-map-alist)))
	    (and c (setq minor-mode-map-alist
			 (cons c (delq c minor-mode-map-alist)))))
	  (set (make-local-variable (quote org-table-may-need-update)) t)
	  (make-local-hook (quote before-change-functions)) ; needed for XEmacs
	  (add-hook 'before-change-functions 'org-before-change-function
		    nil 'local)
	  (set (make-local-variable 'org-old-auto-fill-inhibit-regexp)
	       auto-fill-inhibit-regexp)
	  (set (make-local-variable 'auto-fill-inhibit-regexp)
	       (if auto-fill-inhibit-regexp
		   (concat "\\([ \t]*|\\|" auto-fill-inhibit-regexp)
		 "[ \t]*|"))
	  (easy-menu-add orgtbl-mode-menu)
	  (run-hooks 'orgtbl-mode-hook))
      (setq auto-fill-inhibit-regexp org-old-auto-fill-inhibit-regexp)
      (remove-hook 'before-change-functions 'org-before-change-function t)
      (easy-menu-remove orgtbl-mode-menu)
      (force-mode-line-update 'all))))

;; Install it as a minor mode.
(put 'orgtbl-mode :included t)
(put 'orgtbl-mode :menu-tag "Org Table Mode")
(add-minor-mode 'orgtbl-mode " OrgTbl" orgtbl-mode-map)

(defun orgtbl-make-binding (fun n &rest keys)
  "Create a function for binding in the table minor mode.
FUN is the command to call inside a table. N is used to create a unique
command name. KEYS are keys that should be checked in for a command
to execute outside of tables."
  (eval
   (list 'defun
	 (intern (concat "orgtbl-hijacker-command-" (int-to-string n)))
	 '(arg)
	 (concat "In tables, run `" (symbol-name fun) "'.\n"
		 "Outside of tables, run the binding of `"
		 (mapconcat (lambda (x) (format "%s" x)) keys "' or `")
		 "'.")
	 '(interactive "p")
	 (list 'if
	       '(org-at-table-p)
	       (list 'call-interactively (list 'quote fun))
	       (list 'let '(orgtbl-mode)
		     (list 'call-interactively
			   (append '(or)
				   (mapcar (lambda (k)
					     (list 'key-binding k))
					   keys)
				   '('orgtbl-error))))))))

(defun orgtbl-error ()
  "Error when there is no default binding for a table key."
  (interactive)
  (error "This key is has no function outside tables"))

(defun orgtbl-setup ()
  "Setup orgtbl keymaps."
  (let ((nfunc 0)
	(bindings
	 (list
	  '([(meta shift left)]  org-table-delete-column)
	  '([(meta left)]        org-table-move-column-left)
	  '([(meta right)]       org-table-move-column-right)
	  '([(meta shift right)] org-table-insert-column)
	  '([(meta shift up)]    org-table-kill-row)
	  '([(meta shift down)]  org-table-insert-row)
	  '([(meta up)]          org-table-move-row-up)
	  '([(meta down)]        org-table-move-row-down)
	  '("\C-c\C-w"           org-table-cut-region)
	  '("\C-c\M-w"           org-table-copy-region)
	  '("\C-c\C-y"           org-table-paste-rectangle)
	  '("\C-c-"              org-table-insert-hline)
;	  '([(shift tab)]        org-table-previous-field)
	  '("\C-m"               org-table-next-row)
	  (list (org-key 'S-return) 'org-table-copy-down)
	  '([(meta return)]      org-table-wrap-region)
	  '("\C-c\C-q"           org-table-wrap-region)
	  '("\C-c?"              org-table-current-column)
	  '("\C-c "              org-table-blank-field)
	  '("\C-c+"              org-table-sum)
	  '("\C-c|"              org-table-toggle-vline-visibility)
	  '("\C-c="              org-table-eval-formula)
	  '("\C-c'"              org-table-edit-formulas)
	  '("\C-c*"              org-table-recalculate)
	  '([(control ?#)]       org-table-rotate-recalc-marks)))
	elt key fun cmd)
    (while (setq elt (pop bindings))
      (setq nfunc (1+ nfunc))
      (setq key (car elt)
	    fun (nth 1 elt)
	    cmd (orgtbl-make-binding fun nfunc key))
      (define-key orgtbl-mode-map key cmd))
    ;; Special treatment needed for TAB and RET
    (define-key orgtbl-mode-map [(return)]
      (orgtbl-make-binding 'orgtbl-ret 100 [(return)] "\C-m"))
    (define-key orgtbl-mode-map "\C-m"
      (orgtbl-make-binding 'orgtbl-ret 101 "\C-m" [(return)]))
    (define-key orgtbl-mode-map [(tab)]
      (orgtbl-make-binding 'orgtbl-tab 102 [(tab)] "\C-i"))
    (define-key orgtbl-mode-map "\C-i"
      (orgtbl-make-binding 'orgtbl-tab 103 "\C-i" [(tab)])))
    (define-key orgtbl-mode-map "\C-i"
      (orgtbl-make-binding 'orgtbl-tab 104 [(shift tab)]))
    (define-key orgtbl-mode-map "\C-c\C-c"
      (orgtbl-make-binding 'org-ctrl-c-ctrl-c 105 "\C-c\C-c"))
    (when orgtbl-optimized
      ;; If the user wants maximum table support, we need to hijack
      ;; some standard editing functions
      (org-remap orgtbl-mode-map
		 'self-insert-command 'orgtbl-self-insert-command
		 'delete-char 'orgtbl-delete-char
		 'delete-backward-char 'orgtbl-delete-backward-char)
      (define-key orgtbl-mode-map "|" 'org-force-self-insert))
    (easy-menu-define orgtbl-mode-menu orgtbl-mode-map "OrgTbl menu"
      '("OrgTbl"
	["Align" org-ctrl-c-ctrl-c :active (org-at-table-p) :keys "C-c C-c"]
	["Next Field" org-cycle :active (org-at-table-p) :keys "TAB"]
	["Previous Field" org-shifttab :active (org-at-table-p) :keys "S-TAB"]
	["Next Row" org-return :active (org-at-table-p) :keys "RET"]
	"--"
	["Blank Field" org-table-blank-field :active (org-at-table-p) :keys "C-c SPC"]
	["Copy Field from Above"
	 org-table-copy-down :active (org-at-table-p) :keys "S-RET"]
	"--"
	("Column"
	 ["Move Column Left" org-metaleft :active (org-at-table-p) :keys "M-<left>"]
	 ["Move Column Right" org-metaright :active (org-at-table-p) :keys "M-<right>"]
	 ["Delete Column" org-shiftmetaleft :active (org-at-table-p) :keys "M-S-<left>"]
	 ["Insert Column" org-shiftmetaright :active (org-at-table-p) :keys "M-S-<right>"])
	("Row"
	 ["Move Row Up" org-metaup :active (org-at-table-p) :keys "M-<up>"]
	 ["Move Row Down" org-metadown :active (org-at-table-p) :keys "M-<down>"]
	 ["Delete Row" org-shiftmetaup :active (org-at-table-p) :keys "M-S-<up>"]
	 ["Insert Row" org-shiftmetadown :active (org-at-table-p) :keys "M-S-<down>"]
	 "--"
	 ["Insert Hline" org-table-insert-hline :active (org-at-table-p) :keys "C-c -"])
	("Rectangle"
	 ["Copy Rectangle" org-copy-special :active (org-at-table-p) :keys "C-c C-x M-w"]
	 ["Cut Rectangle" org-cut-special :active (org-at-table-p) :keys "C-c C-x C-w"]
	 ["Paste Rectangle" org-paste-special :active (org-at-table-p) :keys "C-c C-x C-y"]
	 ["Fill Rectangle" org-table-wrap-region :active (org-at-table-p) :keys "C-c C-q"])
	"--"
	["Set Column Formula" org-table-eval-formula :active (org-at-table-p) :keys "C-c ="]
	["Set Named Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
	["Edit Formulas" org-table-edit-formulas :active (org-at-table-p) :keys "C-c '"]
	["Recalculate line" org-table-recalculate :active (org-at-table-p) :keys "C-c *"]
	["Recalculate all" (org-table-recalculate '(4)) :active (org-at-table-p) :keys "C-u C-c *"]
	["Toggle Recalculate Mark" org-table-rotate-recalc-marks :active (org-at-table-p) :keys "C-c #"]
	["Sum Column/Rectangle" org-table-sum
	 :active (or (org-at-table-p) (org-region-active-p)) :keys "C-c +"]
	["Which Column?" org-table-current-column :active (org-at-table-p) :keys "C-c ?"]
	["Debug Formulas"
	 (setq org-table-formula-debug (not org-table-formula-debug))
	 :style toggle :selected org-table-formula-debug]
	))
    t)

(defun orgtbl-tab ()
  "Justification and field motion for `orgtbl-mode'."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-next-field))

(defun orgtbl-ret ()
  "Justification and field motion for `orgtbl-mode'."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-next-row))

(defun orgtbl-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-at-table-p)
	   (or
	    (and org-table-auto-blank-field
		 (member last-command
			 '(orgtbl-hijacker-command-100
			   orgtbl-hijacker-command-101
			   orgtbl-hijacker-command-102
			   orgtbl-hijacker-command-103
			   orgtbl-hijacker-command-104
			   orgtbl-hijacker-command-105))
		 (org-table-blank-field))
	    t)
	   (eq N 1)
	   (looking-at "[^|\n]*  +|"))
      (let (org-table-may-need-update)
	(goto-char (1- (match-end 0)))
	(delete-backward-char 1)
	(goto-char (match-beginning 0))
	(self-insert-command N))
    (setq org-table-may-need-update t)
    (let (orgtbl-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-force-self-insert (N)
  "Needed to enforce self-insert under remapping."
  (interactive "p")
  (self-insert-command N))

(defun orgtbl-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment, because a narrow field may lead to a
reduced column width."
  (interactive "p")
  (if (and (org-at-table-p)
	   (eq N 1)
	   (string-match "|" (buffer-substring (point-at-bol) (point)))
	   (looking-at ".*?|"))
      (let ((pos (point)))
	(backward-delete-char N)
	(skip-chars-forward "^|")
	(insert " ")
	(goto-char (1- pos)))
    (delete-backward-char N)))

(defun orgtbl-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table
will still be marked for re-alignment, because a narrow field may lead to
a reduced column width."
  (interactive "p")
  (if (and (org-at-table-p)
	   (not (bolp))
	   (not (= (char-after) ?|))
	   (eq N 1))
      (if (looking-at ".*?|")
	  (let ((pos (point)))
	    (replace-match (concat
			    (substring (match-string 0) 1 -1)
			    " |"))
	    (goto-char pos)))
    (delete-char N)))

;;; Exporting

(defconst org-level-max 20)

(defun org-export-find-first-heading-line (list)
  "Remove all lines from LIST which are before the first headline."
  (let ((orig-list list)
	(re (concat "^" outline-regexp)))
    (while (and list
		(not (string-match re (car list))))
      (pop list))
    (or list orig-list)))

(defun org-skip-comments (lines)
  "Skip lines starting with \"#\" and subtrees starting with COMMENT."
  (let ((re1 (concat "^\\(\\*+\\)[ \t]+" org-comment-string))
	(re2 "^\\(\\*+\\)[ \t\n\r]")
	rtn line level)
    (while (setq line (pop lines))
      (cond
       ((and (string-match re1 line)
	     (setq level (- (match-end 1) (match-beginning 1))))
	;; Beginning of a COMMENT subtree.  Skip it.
	(while (and (setq line (pop lines))
		    (or (not (string-match re2 line))
			(> (- (match-end 1) (match-beginning 1)) level))))
	(setq lines (cons line lines)))
       ((string-match "^#" line)
	;; an ordinary comment line
	)
       (t (setq rtn (cons line rtn)))))
    (nreverse rtn)))

;; ASCII

(defconst org-ascii-underline '(?\$ ?\# ?^ ?\~ ?\= ?\-)
  "Characters for underlining headings in ASCII export.")

(defconst org-html-entities
  '(("nbsp")
    ("iexcl")
    ("cent")
    ("pound")
    ("curren")
    ("yen")
    ("brvbar")
    ("sect")
    ("uml")
    ("copy")
    ("ordf")
    ("laquo")
    ("not")
    ("shy")
    ("reg")
    ("macr")
    ("deg")
    ("plusmn")
    ("sup2")
    ("sup3")
    ("acute")
    ("micro")
    ("para")
    ("middot")
    ("odot"."o")
    ("star"."*")
    ("cedil")
    ("sup1")
    ("ordm")
    ("raquo")
    ("frac14")
    ("frac12")
    ("frac34")
    ("iquest")
    ("Agrave")
    ("Aacute")
    ("Acirc")
    ("Atilde")
    ("Auml")
    ("Aring") ("AA"."&Aring;")
    ("AElig")
    ("Ccedil")
    ("Egrave")
    ("Eacute")
    ("Ecirc")
    ("Euml")
    ("Igrave")
    ("Iacute")
    ("Icirc")
    ("Iuml")
    ("ETH")
    ("Ntilde")
    ("Ograve")
    ("Oacute")
    ("Ocirc")
    ("Otilde")
    ("Ouml")
    ("times")
    ("Oslash")
    ("Ugrave")
    ("Uacute")
    ("Ucirc")
    ("Uuml")
    ("Yacute")
    ("THORN")
    ("szlig")
    ("agrave")
    ("aacute")
    ("acirc")
    ("atilde")
    ("auml")
    ("aring")
    ("aelig")
    ("ccedil")
    ("egrave")
    ("eacute")
    ("ecirc")
    ("euml")
    ("igrave")
    ("iacute")
    ("icirc")
    ("iuml")
    ("eth")
    ("ntilde")
    ("ograve")
    ("oacute")
    ("ocirc")
    ("otilde")
    ("ouml")
    ("divide")
    ("oslash")
    ("ugrave")
    ("uacute")
    ("ucirc")
    ("uuml")
    ("yacute")
    ("thorn")
    ("yuml")
    ("fnof")
    ("Alpha")
    ("Beta")
    ("Gamma")
    ("Delta")
    ("Epsilon")
    ("Zeta")
    ("Eta")
    ("Theta")
    ("Iota")
    ("Kappa")
    ("Lambda")
    ("Mu")
    ("Nu")
    ("Xi")
    ("Omicron")
    ("Pi")
    ("Rho")
    ("Sigma")
    ("Tau")
    ("Upsilon")
    ("Phi")
    ("Chi")
    ("Psi")
    ("Omega")
    ("alpha")
    ("beta")
    ("gamma")
    ("delta")
    ("epsilon")
    ("varepsilon"."&epsilon;")
    ("zeta")
    ("eta")
    ("theta")
    ("iota")
    ("kappa")
    ("lambda")
    ("mu")
    ("nu")
    ("xi")
    ("omicron")
    ("pi")
    ("rho")
    ("sigmaf") ("varsigma"."&sigmaf;")
    ("sigma")
    ("tau")
    ("upsilon")
    ("phi")
    ("chi")
    ("psi")
    ("omega")
    ("thetasym") ("vartheta"."&thetasym;")
    ("upsih")
    ("piv")
    ("bull") ("bullet"."&bull;")
    ("hellip") ("dots"."&hellip;")
    ("prime")
    ("Prime")
    ("oline")
    ("frasl")
    ("weierp")
    ("image")
    ("real")
    ("trade")
    ("alefsym")
    ("larr") ("leftarrow"."&larr;") ("gets"."&larr;")
    ("uarr") ("uparrow"."&uarr;")
    ("rarr") ("to"."&rarr;") ("rightarrow"."&rarr;")
    ("darr")("downarrow"."&darr;")
    ("harr") ("leftrightarrow"."&harr;")
    ("crarr") ("hookleftarrow"."&crarr;") ; has round hook, not quite CR
    ("lArr") ("Leftarrow"."&lArr;")
    ("uArr") ("Uparrow"."&uArr;")
    ("rArr") ("Rightarrow"."&rArr;")
    ("dArr") ("Downarrow"."&dArr;")
    ("hArr") ("Leftrightarrow"."&hArr;")
    ("forall")
    ("part") ("partial"."&part;")
    ("exist") ("exists"."&exist;")
    ("empty") ("emptyset"."&empty;")
    ("nabla")
    ("isin") ("in"."&isin;")
    ("notin")
    ("ni")
    ("prod")
    ("sum")
    ("minus")
    ("lowast") ("ast"."&lowast;")
    ("radic")
    ("prop") ("proptp"."&prop;")
    ("infin") ("infty"."&infin;")
    ("ang") ("angle"."&ang;")
    ("and") ("vee"."&and;")
    ("or") ("wedge"."&or;")
    ("cap")
    ("cup")
    ("int")
    ("there4")
    ("sim")
    ("cong") ("simeq"."&cong;")
    ("asymp")("approx"."&asymp;")
    ("ne") ("neq"."&ne;")
    ("equiv")
    ("le")
    ("ge")
    ("sub") ("subset"."&sub;")
    ("sup") ("supset"."&sup;")
    ("nsub")
    ("sube")
    ("supe")
    ("oplus")
    ("otimes")
    ("perp")
    ("sdot") ("cdot"."&sdot;")
    ("lceil")
    ("rceil")
    ("lfloor")
    ("rfloor")
    ("lang")
    ("rang")
    ("loz") ("Diamond"."&loz;")
    ("spades") ("spadesuit"."&spades;")
    ("clubs") ("clubsuit"."&clubs;")
    ("hearts") ("diamondsuit"."&hearts;")
    ("diams") ("diamondsuit"."&diams;")
    ("quot")
    ("amp")
    ("lt")
    ("gt")
    ("OElig")
    ("oelig")
    ("Scaron")
    ("scaron")
    ("Yuml")
    ("circ")
    ("tilde")
    ("ensp")
    ("emsp")
    ("thinsp")
    ("zwnj")
    ("zwj")
    ("lrm")
    ("rlm")
    ("ndash")
    ("mdash")
    ("lsquo")
    ("rsquo")
    ("sbquo")
    ("ldquo")
    ("rdquo")
    ("bdquo")
    ("dagger")
    ("Dagger")
    ("permil")
    ("lsaquo")
    ("rsaquo")
    ("euro")

    ("arccos"."arccos")
    ("arcsin"."arcsin")
    ("arctan"."arctan")
    ("arg"."arg")
    ("cos"."cos")
    ("cosh"."cosh")
    ("cot"."cot")
    ("coth"."coth")
    ("csc"."csc")
    ("deg"."deg")
    ("det"."det")
    ("dim"."dim")
    ("exp"."exp")
    ("gcd"."gcd")
    ("hom"."hom")
    ("inf"."inf")
    ("ker"."ker")
    ("lg"."lg")
    ("lim"."lim")
    ("liminf"."liminf")
    ("limsup"."limsup")
    ("ln"."ln")
    ("log"."log")
    ("max"."max")
    ("min"."min")
    ("Pr"."Pr")
    ("sec"."sec")
    ("sin"."sin")
    ("sinh"."sinh")
    ("sup"."sup")
    ("tan"."tan")
    ("tanh"."tanh")
    )
  "Entities for TeX->HTML translation.
Entries can be like (\"ent\"), in which case \"\\ent\" will be translated to
\"&ent;\".  An entry can also be a dotted pair like (\"ent\".\"&other;\").
In that case, \"\\ent\" will be translated to \"&other;\".
The list contains HTML entities for Latin-1, Greek and other symbols.
It is supplemented by a number of commonly used TeX macros with appropriate
translations.  There is currently no way for users to extend this.")

(defvar org-last-level nil) ; dynamically scoped variable

(defun org-export-as-ascii (arg)
  "Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((region
	  (buffer-substring
	   (if (org-region-active-p) (region-beginning) (point-min))
	   (if (org-region-active-p) (region-end) (point-max))))
	 (lines (org-export-find-first-heading-line
		 (org-skip-comments (org-split-string region "[\r\n]"))))
	 (org-startup-with-deadline-check nil)
	 (level 0) line txt
	 (umax nil)
	 (case-fold-search nil)
	 (filename (concat (file-name-sans-extension (buffer-file-name))
			   ".txt"))
	 (buffer (find-file-noselect filename))
	 (levels-open (make-vector org-level-max nil))
	 (date  (format-time-string "%Y/%m/%d" (current-time)))
	 (time  (format-time-string "%X" (current-time)))
	 (author      user-full-name)
	 (title       (buffer-name))
	 (options     nil)
	 (email       user-mail-address)
	 (language    org-export-default-language)
	 (text        nil)
	 (todo nil)
	 (lang-words nil))

    (setq org-last-level 1)
    (org-init-section-numbers)

    (find-file-noselect filename)

    ;; Search for the export key lines
    (org-parse-key-lines)

    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
    (if org-export-ascii-show-new-buffer
	(switch-to-buffer-other-window buffer)
      (set-buffer buffer))
    (erase-buffer)
    (fundamental-mode)
    (if options (org-parse-export-options options))
    (setq umax (if arg (prefix-numeric-value arg)
		 org-export-headline-levels))

    ;; File header
    (if title (org-insert-centered title ?=))
    (insert "\n")
    (if (or author email)
	(insert (concat (nth 1 lang-words) ": " (or author "")
			(if email (concat " <" email ">") "")
			"\n")))
    (if (and date time)
	(insert (concat (nth 2 lang-words) ": " date " " time "\n")))
    (if text (insert (concat (org-html-expand-for-ascii text) "\n\n")))

    (insert "\n\n")

    (if org-export-with-toc
	(progn
	  (insert (nth 3 lang-words) "\n"
		  (make-string (length (nth 3 lang-words)) ?=) "\n")
	  (mapcar '(lambda (line)
		     (if (string-match org-todo-line-regexp
				       line)
			 ;; This is a headline
			 (progn
			   (setq level (- (match-end 1) (match-beginning 1))
				 txt (match-string 3 line)
				 todo
				 (or (and (match-beginning 2)
					  (not (equal (match-string 2 line)
						      org-done-string)))
					; TODO, not DONE
				     (and (= level umax)
					  (org-search-todo-below
					   line lines level))))
			   (setq txt (org-html-expand-for-ascii txt))

			   (if org-export-with-section-numbers
			       (setq txt (concat (org-section-number level)
						 " " txt)))
			   (if (<= level umax)
			       (progn
				 (insert
				  (make-string (* (1- level) 4) ?\ )
				  (format (if todo "%s (*)\n" "%s\n") txt))
				 (setq org-last-level level))
			     ))))
		  lines)))

    (org-init-section-numbers)
    (while (setq line (pop lines))
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]*\\(.*\\)" line)
	;; a Headline
	(setq level (- (match-end 1) (match-beginning 1))
	      txt (match-string 2 line))
	(org-ascii-level-start level txt umax))
       (t (insert line "\n"))))
    (normal-mode)
    (save-buffer)
    (goto-char (point-min))))

(defun org-search-todo-below (line lines level)
  "Search the subtree below LINE for any TODO entries."
  (let ((rest (cdr (memq line lines)))
	(re org-todo-line-regexp)
	line lv todo)
    (catch 'exit
      (while (setq line (pop rest))
	(if (string-match re line)
	    (progn
	      (setq lv (- (match-end 1) (match-beginning 1))
		    todo (and (match-beginning 2)
			      (not (equal (match-string 2 line)
					  org-done-string))))
					; TODO, not DONE
	      (if (<= lv level) (throw 'exit nil))
	      (if todo (throw 'exit t))))))))

;; FIXME: Try to handle <b> and <i> as faces via text properties.
;; FIXME: Can I implement *bold*,/italic/ and _underline_ for ASCII export?
(defun org-html-expand-for-ascii (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
	;; We just remove the tags for now.
	(setq line (replace-match "" nil nil line))))
  line)

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- 80 (length s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
	(insert (make-string ind ?\ )
		(make-string (length s) underline)
		"\n"))))

(defun org-ascii-level-start (level title umax)
  "Insert a new level in ASCII export."
  (let (char)
    (if (> level umax)
	(insert (make-string (* 2 (- level umax 1)) ?\ ) "* " title "\n")
      (if (or (not (equal (char-before) ?\n))
	      (not (equal (char-before (1- (point))) ?\n)))
	  (insert "\n"))
      (setq char (nth (- umax level) (reverse org-ascii-underline)))
      (if org-export-with-section-numbers
	  (setq title (concat (org-section-number level) " " title)))
      (insert title "\n" (make-string (string-width title) char) "\n"))))

(defun org-export-copy-visible ()
  "Copy the visible part of the buffer to another buffer, for printing.
Also removes the first line of the buffer if it specifies a mode,
and all options lines."
  (interactive)
  (let* ((filename (concat (file-name-sans-extension (buffer-file-name))
			   ".txt"))
	 (buffer (find-file-noselect filename))
	 (ore (concat
	       (org-make-options-regexp
		'("CATEGORY" "SEQ_TODO" "PRI_TODO" "TYP_TODO"
		  "STARTUP" "ARCHIVE"
		  "TITLE" "AUTHOR" "EMAIL" "TEXT" "OPTIONS" "LANGUAGE"))
	       (if org-noutline-p "\\(\n\\|$\\)" "")))
	 s e)
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode))
    (save-excursion
      (setq s (goto-char (point-min)))
      (while (not (= (point) (point-max)))
	(goto-char (org-find-invisible))
	(append-to-buffer buffer s (point))
	(setq s (goto-char (org-find-visible)))))
    (switch-to-buffer-other-window buffer)
    (newline)
    (goto-char (point-min))
    (if (looking-at ".*-\\*- mode:.*\n")
	(replace-match ""))
    (while (re-search-forward ore nil t)
      (replace-match ""))
    (goto-char (point-min))))

(defun org-find-visible ()
  (if (featurep 'noutline)
      (let ((s (point)))
	(while (and (not (= (point-max) (setq s (next-overlay-change s))))
		    (get-char-property s 'invisible)))
	s)
    (skip-chars-forward "^\n")
    (point)))
(defun org-find-invisible ()
  (if (featurep 'noutline)
      (let ((s (point)))
	(while (and (not (= (point-max) (setq s (next-overlay-change s))))
		    (not (get-char-property s 'invisible))))
	s)
    (skip-chars-forward "^\r")
    (point)))

;; HTML

(defun org-get-current-options ()
  "Return a string with current options as keyword options.
Does include HTML export options as well as TODO and CATEGORY stuff."
  (format
   "#+TITLE:     %s
#+AUTHOR:    %s
#+EMAIL:     %s
#+LANGUAGE:  %s
#+TEXT:      Some descriptive text to be emitted.  Several lines OK.
#+OPTIONS:   H:%d num:%s toc:%s \\n:%s @:%s ::%s |:%s ^:%s *:%s TeX:%s
#+CATEGORY:  %s
#+SEQ_TODO:  %s
#+TYP_TODO:  %s
#+STARTUP:   %s %s
#+ARCHIVE:   %s
"
   (buffer-name) (user-full-name) user-mail-address org-export-default-language
   org-export-headline-levels
   org-export-with-section-numbers
   org-export-with-toc
   org-export-preserve-breaks
   org-export-html-expand
   org-export-with-fixed-width
   org-export-with-tables
   org-export-with-sub-superscripts
   org-export-with-emphasize
   org-export-with-TeX-macros
   (file-name-nondirectory (buffer-file-name))
   (if (equal org-todo-interpretation 'sequence)
       (mapconcat 'identity org-todo-keywords " ")
     "TODO FEEDBACK VERIFY DONE")
   (if (equal org-todo-interpretation 'type)
       (mapconcat 'identity org-todo-keywords " ")
     "Me Jason Marie DONE")
   (cdr (assoc org-startup-folded
	       '((nil . "nofold")(t . "fold")(content . "content"))))
   (if org-startup-with-deadline-check "dlcheck" "nodlcheck")
   org-archive-location
   ))

(defun org-insert-export-options-template ()
  "Insert into the buffer a template with information for exporting."
  (interactive)
  (if (not (bolp)) (newline))
  (let ((s (org-get-current-options)))
    (and (string-match "#\\+CATEGORY" s)
	 (setq s (substring s 0 (match-beginning 0))))
    (insert s)))

(defun org-toggle-fixed-width-section (arg)
  "Toggle the fixed-width export.
If there is no active region, the QUOTE keyword at the current headline is
inserted or removed.  When present, it causes the text between this headline
and the next to be exported as fixed-width text, and unmodified.
If there is an active region, this command adds or removes a colon as the
first character of this line.  If the first character of a line is a colon,
this line is also exported in fixed-width font."
  (interactive "P")
  (let* ((cc 0)
	 (regionp (org-region-active-p))
	 (beg (if regionp (region-beginning) (point)))
	 (end (if regionp (region-end)))
	 (nlines (or arg (if (and beg end) (count-lines beg end) 1)))
	 (re "[ \t]*\\(:\\)")
	 off)
    (if regionp
	(save-excursion
	  (goto-char beg)
	  (setq cc (current-column))
	  (beginning-of-line 1)
	  (setq off (looking-at re))
	  (while (> nlines 0)
	    (setq nlines (1- nlines))
	    (beginning-of-line 1)
	    (cond
	     (arg
	      (move-to-column cc t)
	      (insert ":\n")
	      (forward-line -1))
	     ((and off (looking-at re))
	      (replace-match "" t t nil 1))
	     ((not off) (move-to-column cc t) (insert ":")))
	    (forward-line 1)))
      (save-excursion
	(org-back-to-heading)
	(if (looking-at (concat outline-regexp
				"\\( +\\<" org-quote-string "\\>\\)"))
	    (replace-match "" t t nil 1)
	  (if (looking-at outline-regexp)
	      (progn
		(goto-char (match-end 0))
		(insert " " org-quote-string))))))))

(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-html arg 'hidden)
  (org-open-file (buffer-file-name)))

(defun org-export-as-html-batch ()
  "Call `org-export-as-html', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-html org-export-headline-levels 'hidden))

(defun org-export-as-html (arg &optional hidden)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-string org-done-string)
  (let* ((region-p (org-region-active-p))
         (region
          (buffer-substring
           (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))))
         (all_lines
          (org-skip-comments (org-split-string region "[\r\n]")))
         (lines (org-export-find-first-heading-line all_lines))
         (level 0) (line "") (origline "") txt todo
         (umax nil)
         (filename (concat (file-name-sans-extension (buffer-file-name))
                           ".html"))
         (buffer (find-file-noselect filename))
         (levels-open (make-vector org-level-max nil))
	 (date (format-time-string "%Y/%m/%d" (current-time)))
	 (time  (format-time-string "%X" (current-time)))
         (author      user-full-name)
	 (title       (buffer-name))
         (options     nil)
	 (quote-re    (concat "^\\*+[ \t]*" org-quote-string "\\>"))
	 (inquote     nil)
	 (email       user-mail-address)
         (language    org-export-default-language)
	 (text        nil)
         (lang-words  nil)
	 (head-count  0) cnt
	 (start       0)
	 ;; FIXME: The following returns always nil under XEmacs
	 (coding-system (and (fboundp 'coding-system-get)
			     (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (charset (and coding-system
		       (coding-system-get coding-system 'mime-charset)))
	 table-open type
	 table-buffer table-orig-buffer
	 )
    (message "Exporting...")

    (setq org-last-level 1)
    (org-init-section-numbers)

    ;; Search for the export key lines
    (org-parse-key-lines)
    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer
    (if (or hidden (not org-export-html-show-new-buffer))
        (set-buffer buffer)
      (switch-to-buffer-other-window buffer))
    (erase-buffer)
    (fundamental-mode)
    (let ((case-fold-search nil))
      (if options (org-parse-export-options options))
      (setq umax (if arg (prefix-numeric-value arg)
                   org-export-headline-levels))

      ;; File header
      (insert (format
               "<html lang=\"%s\"><head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\">
<meta name=generator content=\"Org-mode\">
<meta name=generated content=\"%s %s\">
<meta name=author content=\"%s\">
</head><body>
"
	       language (org-html-expand title) (or charset "iso-8859-1")
	       date time author))
      (if title     (insert (concat "<H1 align=\"center\">"
				    (org-html-expand title) "</H1>\n")))
      (if author    (insert (concat (nth 1 lang-words) ": " author "\n")))
      (if email	  (insert (concat "<a href=\"mailto:" email "\">&lt;"
                                  email "&gt;</a>\n")))
      (if (or author email) (insert "<br>\n"))
      (if (and date time) (insert (concat (nth 2 lang-words) ": "
                                          date " " time "<br>\n")))
      (if text      (insert (concat "<p>\n" (org-html-expand text))))
      (if org-export-with-toc
	  (progn
	    (insert (format "<H2>%s</H2>\n" (nth 3 lang-words)))
	    (insert "<ul>\n")
	    (mapcar '(lambda (line)
		       (if (string-match org-todo-line-regexp line)
			   ;; This is a headline
			   (progn
			     (setq level (- (match-end 1) (match-beginning 1))
				   txt (save-match-data
					 (org-html-expand
					  (match-string 3 line)))
				   todo
				   (or (and (match-beginning 2)
					    (not (equal (match-string 2 line)
							org-done-string)))
					; TODO, not DONE
				       (and (= level umax)
					    (org-search-todo-below
					     line lines level))))
			     (if org-export-with-section-numbers
				 (setq txt (concat (org-section-number level)
						   " " txt)))
			     (if (<= level umax)
				 (progn
				   (setq head-count (+ head-count 1))
				   (if (> level org-last-level)
				       (progn
					 (setq cnt (- level org-last-level))
					 (while (>= (setq cnt (1- cnt)) 0)
					   (insert "<ul>"))
					 (insert "\n")))
				   (if (< level org-last-level)
				       (progn
					 (setq cnt (- org-last-level level))
					 (while (>= (setq cnt (1- cnt)) 0)
					   (insert "</ul>"))
					 (insert "\n")))
				   (insert
				    (format
				     (if todo
					 "<li><a href=\"#sec-%d\"><span style='color:red'>%s</span></a></li>\n"
				       "<li><a href=\"#sec-%d\">%s</a></li>\n")
				     head-count txt))
				   (setq org-last-level level))
			       ))))
		    lines)
	    (while (> org-last-level 0)
	      (setq org-last-level (1- org-last-level))
	      (insert "</ul>\n"))
	    ))
      (setq head-count 0)
      (org-init-section-numbers)

      (while (setq line (pop lines) origline line)
	;; end of quote?
	(when (and inquote (string-match "^\\*+" line))
	  (insert "</pre>\n")
	  (setq inquote nil))
	;; inquote
	(if inquote
	    (progn
	      (insert line "\n")
	      (setq line (org-html-expand line))) ;;????? FIXME: not needed?

	  ;; Protect the links
	  (setq start 0)
	  (while (string-match org-link-maybe-angles-regexp line start)
	    (setq start (match-end 0))
	    (setq line (replace-match
			(concat "\000" (match-string 1 line) "\000")
			t t line)))

	  ;; replace "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  (setq line (org-html-expand line))

	  ;; Verbatim lines
	  (if (and org-export-with-fixed-width
		   (string-match "^[ \t]*:\\(.*\\)" line))
	      (progn
		(let ((l (match-string 1 line)))
		  (while (string-match " " l)
		    (setq l (replace-match "&nbsp;" t t l)))
		  (insert "\n<span style='font-family:Courier'>"
			  l "</span>"
			  (if (and lines
				   (not (string-match "^[ \t]+\\(:.*\\)"
						      (car lines))))
			      "<br>\n" "\n"))))

	    (setq start 0)
	    (while (string-match org-protected-link-regexp line start)
	      (setq start (- (match-end 0) 2))
	      (setq type (match-string 1 line))
	      (cond
	       ((member type '("http" "https" "ftp" "mailto" "news"))
		;; standard URL
		(setq line (replace-match
					;                          "<a href=\"\\1:\\2\">&lt;\\1:\\2&gt;</a>"
			    "<a href=\"\\1:\\2\">\\1:\\2</a>"
			    nil nil line)))
	       ((string= type "file")
		;; FILE link
		(let* ((filename (match-string 2 line))
		       (abs-p (file-name-absolute-p filename))
		       (thefile (if abs-p (expand-file-name filename) filename))
		       (thefile (save-match-data
				  (if (string-match ":[0-9]+$" thefile)
				      (replace-match "" t t thefile)
				    thefile)))
		       (file-is-image-p
			(save-match-data
			  (string-match (org-image-file-name-regexp) thefile))))
		  (setq line (replace-match
			      (if (and org-export-html-inline-images
				       file-is-image-p)
				  (concat "<img src=\"" thefile "\"/>")
				(concat "<a href=\"" thefile "\">\\1:\\2</a>"))
			      nil nil line))))

	       ((member type '("bbdb" "vm" "wl" "rmail" "gnus" "shell"))
		(setq line (replace-match
			    "<i>&lt;\\1:\\2&gt;</i>" nil nil line)))))

	    ;; TODO items
	    (if (and (string-match org-todo-line-regexp line)
		     (match-beginning 2))
		(if (equal (match-string 2 line) org-done-string)
		    (setq line (replace-match
				"<span style='color:green'>\\2</span>"
				nil nil line 2))
		  (setq line (replace-match "<span style='color:red'>\\2</span>"
					    nil nil line 2))))

	    ;; DEADLINES
	    (if (string-match org-deadline-line-regexp line)
		(progn
		  (if (save-match-data
			(string-match "<a href"
				      (substring line 0 (match-beginning 0))))
		      nil     ; Don't do the replacement - it is inside a link
		    (setq line (replace-match "<span style='color:red'>\\&</span>"
					      nil nil line 1)))))


	    (cond
	     ((string-match "^\\(\\*+\\)[ \t]*\\(.*\\)" line)
	      ;; This is a headline
	      (setq level (- (match-end 1) (match-beginning 1))
		    txt (match-string 2 line))
	      (if (<= level umax) (setq head-count (+ head-count 1)))
	      (org-html-level-start level txt umax
				    (and org-export-with-toc (<= level umax))
				    head-count)
	      ;; QUOTES
	      (when (string-match quote-re line)
		(insert "<pre>")
		(setq inquote t)))

	     ((and org-export-with-tables
		   (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	      (if (not table-open)
		  ;; New table starts
		  (setq table-open t table-buffer nil table-orig-buffer nil))
	      ;; Accumulate lines
	      (setq table-buffer (cons line table-buffer)
		    table-orig-buffer (cons origline table-orig-buffer))
	      (when (or (not lines)
			(not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
					   (car lines))))
		(setq table-open nil
		      table-buffer (nreverse table-buffer)
		      table-orig-buffer (nreverse table-orig-buffer))
		(insert (org-format-table-html table-buffer table-orig-buffer))))
	     (t
	      ;; Normal lines
	      ;; Lines starting with "-", and empty lines make new paragraph.
	      ;; FIXME: Should we add + and *?
	      (if (string-match "^ *-\\|^[ \t]*$" line) (insert "<p>"))
	      (insert line (if org-export-preserve-breaks "<br>\n" "\n"))))
	    )))
	(if org-export-html-with-timestamp
	    (insert org-export-html-html-helper-timestamp))
	(insert "</body>\n</html>\n")
	(normal-mode)
	(save-buffer)
	(goto-char (point-min)))))

(defun org-format-table-html (lines olines)
  "Find out which HTML converter to use and return the HTML code."
  (if (string-match "^[ \t]*|" (car lines))
      ;; A normal org table
      (org-format-org-table-html lines)
    ;; Table made by table.el - test for spanning
    (let* ((hlines (delq nil (mapcar
			      (lambda (x)
				(if (string-match "^[ \t]*\\+-" x) x
				  nil))
			      lines)))
	   (first (car hlines))
	   (ll (and (string-match "\\S-+" first)
		    (match-string 0 first)))
	   (re (concat "^[ \t]*" (regexp-quote ll)))
	   (spanning (delq nil (mapcar (lambda (x) (not (string-match re x)))
				       hlines))))
      (if (and (not spanning)
	       (not org-export-prefer-native-exporter-for-tables))
	  ;; We can use my own converter with HTML conversions
	  (org-format-table-table-html lines)
	;; Need to use the code generator in table.el, with the original text.
	(org-format-table-table-html-using-table-generate-source olines)))))

(defun org-format-org-table-html (lines)
  "Format a table into html."
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (let ((head (and org-export-highlight-first-table-line
		   (delq nil (mapcar
			      (lambda (x) (string-match "^[ \t]*|-" x))
			      (cdr lines)))))
	line fields html)
    (setq html (concat org-export-html-table-tag "\n"))
    (while (setq line (pop lines))
      (catch 'next-line
	(if (string-match "^[ \t]*|-" line)
	    (progn
	      (setq head nil)   ;; head ends here, first time around
	      ;; ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(setq html (concat
		    html
		    "<tr>"
		    (mapconcat (lambda (x)
				 (if head
				     (concat "<th>" x "</th>")
				   (concat "<td valign=\"top\">" x "</td>")))
			       fields "")
		    "</tr>\n"))))
    (setq html (concat html "</table>\n"))
    html))

(defun org-fake-empty-table-line (line)
  "Replace everything except \"|\" with spaces."
  (let ((i (length line))
	(newstr (copy-sequence line)))
    (while (> i 0)
      (setq i (1- i))
      (if (not (eq (aref newstr i) ?|))
	  (aset newstr i ?\ )))
    newstr))

(defun org-format-table-table-html (lines)
  "Format a table generated by table.el into html.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (head org-export-highlight-first-table-line)
	     fields html empty)
    (setq html (concat org-export-html-table-tag "\n"))
    (while (setq line (pop lines))
      (setq empty "&nbsp")
      (catch 'next-line
	(if (string-match "^[ \t]*\\+-" line)
	    (progn
	      (if field-buffer
		  (progn
		    (setq html (concat
				html
				"<tr>"
				(mapconcat
				 (lambda (x)
				   (if (equal x "") (setq x empty))
				   (if head
				       (concat "<th valign=\"top\">" x
					       "</th>\n")
				     (concat "<td valign=\"top\">" x
					     "</td>\n")))
				 field-buffer "\n")
				"</tr>\n"))
		    (setq head nil)
		    (setq field-buffer nil)))
	      ;; Ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields and store the fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(if field-buffer
	    (setq field-buffer (mapcar
				(lambda (x)
				  (concat x "<br>" (pop fields)))
				field-buffer))
	  (setq field-buffer fields))))
    (setq html (concat html "</table>\n"))
    html))

(defun org-format-table-table-html-using-table-generate-source (lines)
  "Format a table into html, using `table-generate-source' from table.el.
This has the advantage that cell- or row-spanning is allowed.
But it has the disadvantage, that Org-mode's HTML conversions cannot be used."
  (require 'table)
  (with-current-buffer (get-buffer-create " org-tmp1 ")
    (erase-buffer)
    (insert (mapconcat 'identity lines "\n"))
    (goto-char (point-min))
    (if (not (re-search-forward "|[^+]" nil t))
	(error "Error processing table"))
    (table-recognize-table)
    (with-current-buffer (get-buffer-create " org-tmp2 ") (erase-buffer))
    (table-generate-source 'html " org-tmp2 ")
    (set-buffer " org-tmp2 ")
    (buffer-substring (point-min) (point-max))))

(defun org-html-expand (string)
  "Prepare STRING for HTML export.  Applies all active conversions."
  ;; First check if there is a link in the line - if yes, apply conversions
  ;; only before the start of the link.
  (let* ((m (string-match org-link-regexp string))
	 (s (if m (substring string 0 m) string))
	 (r (if m (substring string m) "")))
    ;; convert < to &lt; and > to &gt;
    (while (string-match "<" s)
      (setq s (replace-match "&lt;" t t s)))
    (while (string-match ">" s)
      (setq s (replace-match "&gt;" t t s)))
    (if org-export-html-expand
	(while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	  (setq s (replace-match "<\\1>" nil nil s))))
    (if org-export-with-emphasize
	(setq s (org-export-html-convert-emphasize s)))
    (if org-export-with-sub-superscripts
	(setq s (org-export-html-convert-sub-super s)))
    (if org-export-with-TeX-macros
	(let ((start 0) wd ass)
	  (while (setq start (string-match "\\\\\\([a-zA-Z]+\\)" s start))
	    (setq wd (match-string 1 s))
	    (if (setq ass (assoc wd org-html-entities))
		(setq s (replace-match (or (cdr ass)
					   (concat "&" (car ass) ";"))
				       t t s))
	      (setq start (+ start (length wd)))))))
    (concat s r)))

(defun org-create-multibrace-regexp (left right n)
  "Create a regular expression which will match a balanced sexp.
Opening delimiter is LEFT, and closing delimiter is RIGHT, both given
as single character strings.
The regexp returned will match the entire expression including the
delimiters.  It will also define a single group which contains the
match except for the outermost delimiters.  The maximum depth of
stacked delimiters is N.  Escaping delimiters is not possible."
  (let* ((nothing (concat "[^" "\\" left "\\" right "]*?"))
	 (or "\\|")
	 (re nothing)
	 (next (concat "\\(?:" nothing left nothing right "\\)+" nothing)))
    (while (> n 1)
      (setq n (1- n)
	    re (concat re or next)
	    next (concat "\\(?:" nothing left next right "\\)+" nothing)))
    (concat left "\\(" re "\\)" right)))

(defvar org-match-substring-regexp
  (concat
   "\\([^\\]\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(\\(?:\\*\\|[-+]?[^-+*!@#$%^_ \t\r\n,:\"?<>~;./{}=()]+\\)\\)\\)")
  "The regular expression matching a sub- or superscript.")

(defun org-export-html-convert-sub-super (string)
  "Convert sub- and superscripts in STRING to HTML."
  (let (key c)
    (while (string-match org-match-substring-regexp string)
      (setq key (if (string= (match-string 2 string) "_") "sub" "sup"))
      (setq c (or (match-string 8 string)
		  (match-string 6 string)
		  (match-string 5 string)))
      (setq string (replace-match
		    (concat (match-string 1 string)
			    "<" key ">" c "</" key ">")
		    t t string)))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string))))
  string)

(defun org-export-html-convert-emphasize (string)
  (while (string-match
	  "\\(\\s-\\|^\\)\\(\\*\\([a-zA-Z]+\\)\\*\\)\\([^a-zA-Z*]\\|$\\)"
	  string)
    (setq string (replace-match
		  (concat "<b>" (match-string 3 string) "</b>")
		  t t string 2)))
  (while (string-match
	  "\\(\\s-\\|^\\)\\(/\\([a-zA-Z]+\\)/\\)\\([^a-zA-Z*]\\|$\\)"
	  string)
    (setq string (replace-match
		  (concat "<i>" (match-string 3 string) "</i>")
		  t t string 2)))
  (while (string-match
	  "\\(\\s-\\|^\\)\\(_\\([a-zA-Z]+\\)_\\)\\([^a-zA-Z*]\\|$\\)"
	  string)
    (setq string (replace-match
		  (concat "<u>" (match-string 3 string) "</u>")
		  t t string 2)))
  string)

(defun org-parse-key-lines ()
  "Find the special key lines with the information for exporters."
  (save-excursion
    (goto-char 0)
    (let ((re (org-make-options-regexp
	       '("TITLE" "AUTHOR" "EMAIL" "TEXT" "OPTIONS" "LANGUAGE")))
	  key)
      (while (re-search-forward re nil t)
	(setq key (match-string 1))
	(cond ((string-equal key "TITLE")
	       (setq title (match-string 2)))
	      ((string-equal key "AUTHOR")
	       (setq author (match-string 2)))
	      ((string-equal key "EMAIL")
	       (setq email (match-string 2)))
	      ((string-equal key "LANGUAGE")
	       (setq language (match-string 2)))
	      ((string-equal key "TEXT")
	       (setq text (concat text "\n" (match-string 2))))
	      ((string-equal key "OPTIONS")
	       (setq options (match-string 2))))))))

(defun org-parse-export-options (s)
  "Parse the export options line."
  (let ((op '(("H"     . org-export-headline-levels)
	      ("num"   . org-export-with-section-numbers)
	      ("toc"   . org-export-with-toc)
	      ("\\n"   . org-export-preserve-breaks)
	      ("@"     . org-export-html-expand)
	      (":"     . org-export-with-fixed-width)
	      ("|"     . org-export-with-tables)
	      ("^"     . org-export-with-sub-superscripts)
	      ("*"     . org-export-with-emphasize)
	      ("TeX"   . org-export-with-TeX-macros)))
	o)
    (while (setq o (pop op))
      (if (string-match (concat (regexp-quote (car o)) ":\\([^ \t\n\r;,.]*\\)")
			s)
	  (set (make-local-variable (cdr o))
	       (car (read-from-string (match-string 1 s))))))))

(defun org-html-level-start (level title umax with-toc head-count)
  "Insert a new level in HTML export."
  (let ((l (1+ (max level umax))))
    (while (<= l org-level-max)
      (if (aref levels-open (1- l))
	  (progn
	    (org-html-level-close l)
	    (aset levels-open (1- l) nil)))
      (setq l (1+ l)))
    (if (> level umax)
	(progn
	  (if (aref levels-open (1- level))
	      (insert "<li>" title "<p>\n")
	    (aset levels-open (1- level) t)
	    (insert "<ul><li>" title "<p>\n")))
      (if org-export-with-section-numbers
	  (setq title (concat (org-section-number level) " " title)))
      (setq level (+ level 1))
      (if with-toc
	  (insert (format "\n<H%d><a name=\"sec-%d\">%s</a></H%d>\n"
			  level head-count title level))
	(insert (format "\n<H%d>%s</H%d>\n" level title level))))))

(defun org-html-level-close (&rest args)
  "Terminate one level in HTML export."
  (insert "</ul>"))


;; Variable holding the vector with section numbers
(defvar org-section-numbers (make-vector org-level-max 0))

(defun org-init-section-numbers ()
  "Initialize the vector for the section numbers."
  (let* ((level  -1)
	 (numbers (nreverse (org-split-string "" "\\.")))
	 (depth (1- (length org-section-numbers)))
	 (i depth) number-string)
    (while (>= i 0)
      (if (> i level)
	  (aset org-section-numbers i 0)
	(setq number-string (or (car numbers) "0"))
	(if (string-match "\\`[A-Z]\\'" number-string)
	    (aset org-section-numbers i
		  (- (string-to-char number-string) ?A -1))
	    (aset org-section-numbers i (string-to-number number-string)))
	(pop numbers))
      (setq i (1- i)))))

(defun org-section-number (&optional level)
  "Return a string with the current section number.
When LEVEL is non-nil, increase section numbers on that level."
  (let* ((depth (1- (length org-section-numbers))) idx n (string ""))
    (when level
      (when (> level -1)
	(aset org-section-numbers
	      level (1+ (aref org-section-numbers level))))
      (setq idx (1+ level))
      (while (<= idx depth)
	(if (not (= idx 1))
	    (aset org-section-numbers idx 0))
	(setq idx (1+ idx))))
    (setq idx 0)
    (while (<= idx depth)
      (setq n (aref org-section-numbers idx))
      (setq string (concat string (if (not (string= string "")) "." "")
			   (int-to-string n)))
      (setq idx (1+ idx)))
    (save-match-data
      (if (string-match "\\`\\([@0]\\.\\)+" string)
	  (setq string (replace-match "" nil nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
	  (setq string (replace-match "" nil nil string))))
    string))


;;; Key bindings

;; - Bindings in Org-mode map are currently
;;   0123456789abcdefghijklmnopqrstuvwxyz!?@#$%^&-+*/=()_{}[]:;"|,.<>~`'\t  the alphabet
;;             abcd fgh j lmnopqrstuvwxyz ? #$   -+*/=     [] ; |,.<>~  \t  necessary bindings
;;                 e                                                        (?) useful from outline-mode
;;                     i k                 @                                expendable from outline-mode
;;   0123456789                          !    %^&     ()_{}    "      `'    free

;; Make `C-c C-x' a prefix key
(define-key org-mode-map "\C-c\C-x" (make-sparse-keymap))

;; TAB key with modifiers
(define-key org-mode-map "\C-i"       'org-cycle)
(define-key org-mode-map [(meta tab)] 'org-complete)
(define-key org-mode-map "\M-\C-i"    'org-complete)            ; for tty emacs
;; The following line is necessary under Suse GNU/Linux
(unless org-xemacs-p
  (define-key org-mode-map [S-iso-lefttab]  'org-shifttab))
(define-key org-mode-map [(shift tab)]    'org-shifttab)

(define-key org-mode-map (org-key 'S-return)   'org-table-copy-down)
(define-key org-mode-map "\C-c\C-xc"           'org-table-copy-down)     ; tty
(define-key org-mode-map [(meta shift return)] 'org-insert-todo-heading)
(define-key org-mode-map "\C-c\C-xM"           'org-insert-todo-heading) ; tty
(define-key org-mode-map [(meta return)]       'org-meta-return)
(define-key org-mode-map "\C-c\C-xm"           'org-meta-return)  ; tty emacs
(define-key org-mode-map [?\e (return)]        'org-meta-return)   ; tty emacs

;; Cursor keys with modifiers
(define-key org-mode-map [(meta left)]  'org-metaleft)
(define-key org-mode-map [?\e (left)]   'org-metaleft)          ; for tty emacs
(define-key org-mode-map "\C-c\C-xl"    'org-metaleft)          ; for tty emacs
(define-key org-mode-map [(meta right)] 'org-metaright)
(define-key org-mode-map [?\e (right)]  'org-metaright)         ; for tty emacs
(define-key org-mode-map "\C-c\C-xr"    'org-metaright)         ; for tty emacs
(define-key org-mode-map [(meta up)]    'org-metaup)
(define-key org-mode-map [?\e (up)]     'org-metaup)            ; for tty emacs
(define-key org-mode-map "\C-c\C-xu"    'org-metaup)            ; for tty emacs
(define-key org-mode-map [(meta down)]  'org-metadown)
(define-key org-mode-map [?\e (down)]   'org-metadown)          ; for tty emacs
(define-key org-mode-map "\C-c\C-xd"    'org-metadown)          ; for tty emacs

(define-key org-mode-map [(meta shift left)]       'org-shiftmetaleft)
(define-key org-mode-map "\C-c\C-xL"               'org-shiftmetaleft)  ; tty
(define-key org-mode-map [(meta shift right)]      'org-shiftmetaright)
(define-key org-mode-map "\C-c\C-xR"               'org-shiftmetaright) ; tty
(define-key org-mode-map [(meta shift up)]         'org-shiftmetaup)
(define-key org-mode-map "\C-c\C-xU"               'org-shiftmetaup)    ; tty
(define-key org-mode-map [(meta shift down)]       'org-shiftmetadown)
(define-key org-mode-map "\C-c\C-xD"               'org-shiftmetadown)  ; tty
(define-key org-mode-map (org-key 'S-up)       'org-shiftup)
(define-key org-mode-map [?\C-c ?\C-x (up)]    'org-shiftup)
(define-key org-mode-map (org-key 'S-down)     'org-shiftdown)
(define-key org-mode-map [?\C-c ?\C-x (down)]  'org-shiftdown)
(define-key org-mode-map (org-key 'S-left)     'org-timestamp-down-day)
(define-key org-mode-map [?\C-c ?\C-x (left)]  'org-timestamp-down-day)
(define-key org-mode-map (org-key 'S-right)    'org-timestamp-up-day)
(define-key org-mode-map [?\C-c ?\C-x (right)] 'org-timestamp-up-day)

;; All the other keys
(define-key org-mode-map "\C-c$"    'org-archive-subtree)
(define-key org-mode-map "\C-c\C-j" 'org-goto)
(define-key org-mode-map "\C-c\C-t" 'org-todo)
(define-key org-mode-map "\C-c\C-s" 'org-schedule)
(define-key org-mode-map "\C-c\C-d" 'org-deadline)
(define-key org-mode-map "\C-c;"    'org-toggle-comment)
(define-key org-mode-map "\C-c\C-v" 'org-show-todo-tree)
(define-key org-mode-map "\C-c\C-w" 'org-check-deadlines)
(define-key org-mode-map "\C-c/"    'org-occur)   ; Minor-mode reserved
(define-key org-mode-map "\C-c\C-m" 'org-insert-heading)
(define-key org-mode-map "\M-\C-m"  'org-insert-heading)
(define-key org-mode-map "\C-c\C-l" 'org-insert-link)
(define-key org-mode-map "\C-c\C-o" 'org-open-at-point)
(define-key org-mode-map "\C-c\C-z" 'org-time-stamp)  ; Alternative binding
(define-key org-mode-map "\C-c."    'org-time-stamp)  ; Minor-mode reserved
(define-key org-mode-map "\C-c!"    'org-time-stamp-inactive) ; Minor-mode r.
(define-key org-mode-map "\C-c,"    'org-priority)    ; Minor-mode reserved
(define-key org-mode-map "\C-c\C-y" 'org-evaluate-time-range)
(define-key org-mode-map "\C-c>"    'org-goto-calendar)
(define-key org-mode-map "\C-c<"    'org-date-from-calendar)
(define-key org-mode-map "\C-c["    'org-add-file)
(define-key org-mode-map "\C-c]"    'org-remove-file)
(define-key org-mode-map "\C-c\C-r"       'org-timeline)
(define-key org-mode-map "\C-c-"          'org-table-insert-hline)
(define-key org-mode-map "\C-c\C-c"       'org-ctrl-c-ctrl-c)
(define-key org-mode-map "\C-m"           'org-return)
(define-key org-mode-map "\C-c?"          'org-table-current-column)
(define-key org-mode-map "\C-c "          'org-table-blank-field)
(define-key org-mode-map "\C-c+"          'org-table-sum)
(define-key org-mode-map "\C-c|"          'org-table-toggle-vline-visibility)
(define-key org-mode-map "\C-c="          'org-table-eval-formula)
(define-key org-mode-map "\C-c'"          'org-table-edit-formulas)
(define-key org-mode-map "\C-c*"          'org-table-recalculate)
(define-key org-mode-map [(control ?#)]   'org-table-rotate-recalc-marks)
(define-key org-mode-map "\C-c~"          'org-table-create-with-table.el)
(define-key org-mode-map "\C-c\C-q"       'org-table-wrap-region)
(define-key org-mode-map "\C-c\C-xa"      'org-export-as-ascii)
(define-key org-mode-map "\C-c\C-x\C-a"   'org-export-as-ascii)
(define-key org-mode-map "\C-c\C-xv"      'org-export-copy-visible)
(define-key org-mode-map "\C-c\C-x\C-v"   'org-export-copy-visible)
;; OPML support is only planned
;(define-key org-mode-map "\C-c\C-xo"      'org-export-as-opml)
;(define-key org-mode-map "\C-c\C-x\C-o"   'org-export-as-opml)
(define-key org-mode-map "\C-c\C-xt"      'org-insert-export-options-template)
(define-key org-mode-map "\C-c:"          'org-toggle-fixed-width-section)
(define-key org-mode-map "\C-c\C-xh"      'org-export-as-html)
(define-key org-mode-map "\C-c\C-xb"      'org-export-as-html-and-open)
(define-key org-mode-map "\C-c\C-x\C-b"   'org-export-as-html-and-open)

(define-key org-mode-map "\C-c\C-x\C-w"   'org-cut-special)
(define-key org-mode-map "\C-c\C-x\M-w"   'org-copy-special)
(define-key org-mode-map "\C-c\C-x\C-y"   'org-paste-special)

(defsubst org-table-p () (org-at-table-p))

(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-table-p)
	   (or
	    (and org-table-auto-blank-field
		 (member last-command
			 '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
		 (org-table-blank-field))
	    t)
	   (eq N 1)
	   (looking-at "[^|\n]*  +|"))
      (let (org-table-may-need-update)
	(goto-char (1- (match-end 0)))
	(delete-backward-char 1)
	(goto-char (match-beginning 0))
	(self-insert-command N))
    (setq org-table-may-need-update t)
    (self-insert-command N)))

;; FIXME:
;; The following two functions might still be optimized to trigger
;; re-alignment less frequently.

(defun org-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment, because a narrow field may lead to a
reduced column width."
  (interactive "p")
  (if (and (org-table-p)
	   (eq N 1)
	   (string-match "|" (buffer-substring (point-at-bol) (point)))
	   (looking-at ".*?|"))
      (let ((pos (point)))
	(backward-delete-char N)
	(skip-chars-forward "^|")
	(insert " ")
	(goto-char (1- pos)))
    (backward-delete-char N)))

(defun org-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table
will still be marked for re-alignment, because a narrow field may lead to
a reduced column width."
  (interactive "p")
  (if (and (org-table-p)
	   (not (bolp))
	   (not (= (char-after) ?|))
	   (eq N 1))
      (if (looking-at ".*?|")
	  (let ((pos (point)))
	    (replace-match (concat
			    (substring (match-string 0) 1 -1)
			    " |"))
	    (goto-char pos)))
    (delete-char N)))

;; How to do this: Measure non-white length of current string
;; If equal to column width, we should realign.

(defun org-remap (map &rest commands)
  "In MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (if (fboundp 'command-remapping)
	  (define-key map (vector 'remap old) new)
	(substitute-key-definition old new map global-map)))))
  
(when (eq org-enable-table-editor 'optimized)
  ;; If the user wants maximum table support, we need to hijack
  ;; some standard editing functions
  (org-remap org-mode-map
	     'self-insert-command 'org-self-insert-command
	     'delete-char 'org-delete-char
	     'delete-backward-char 'org-delete-backward-char)
  (define-key org-mode-map "|" 'org-force-self-insert))

(defun org-shiftcursor-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (error "This command is only active in tables and on headlines"))

(defun org-shifttab ()
  "Global visibility cycling or move to previous table field.
Calls `(org-cycle t)' or `org-table-previous-field', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((org-at-table-p) (org-table-previous-field))
   (t (org-cycle '(4)))))

(defun org-shiftmetaleft ()
  "Promote subtree or delete table column.
Calls `org-promote-subtree' or `org-table-delete-column', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((org-at-table-p) (org-table-delete-column))
   ((org-on-heading-p) (org-promote-subtree))
   (t (org-shiftcursor-error))))

(defun org-shiftmetaright ()
  "Demote subtree or insert table column.
Calls `org-demote-subtree' or `org-table-insert-column', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((org-at-table-p) (org-table-insert-column))
   ((org-on-heading-p) (org-demote-subtree))
   (t (org-shiftcursor-error))))

(defun org-shiftmetaup (&optional arg)
  "Move subtree up or kill table row.
Calls `org-move-subtree-up' or `org-table-kill-row', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-kill-row))
   ((org-on-heading-p) (org-move-subtree-up arg))
   (t (org-shiftcursor-error))))
(defun org-shiftmetadown (&optional arg)
  "Move subtree down or insert table row.
Calls `org-move-subtree-down' or `org-table-insert-row', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-insert-row arg))
   ((org-on-heading-p) (org-move-subtree-down arg))
   (t (org-shiftcursor-error))))

(defun org-metaleft (&optional arg)
  "Promote heading or move table column to left.
Calls `org-do-promote' or `org-table-move-column', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-move-column 'left))
   ((or (org-on-heading-p) (org-region-active-p)) (org-do-promote))
   (t (backward-word (prefix-numeric-value arg)))))

(defun org-metaright (&optional arg)
  "Demote subtree or move table column to right.
Calls `org-do-demote' or `org-table-move-column', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-move-column nil))
   ((or (org-on-heading-p) (org-region-active-p)) (org-do-demote))
   (t (forward-word (prefix-numeric-value arg)))))

(defun org-metaup (&optional arg)
  "Move subtree up or move table row up.
Calls `org-move-subtree-up' or `org-table-move-row', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-move-row 'up))
   ((org-on-heading-p) (org-move-subtree-up arg))
   (t (org-shiftcursor-error))))

(defun org-metadown (&optional arg)
  "Move subtree down or move table row down.
Calls `org-move-subtree-down' or `org-table-move-row', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-move-row nil))
   ((org-on-heading-p) (org-move-subtree-down arg))
   (t (org-shiftcursor-error))))

(defun org-shiftup (&optional arg)
  "Increase item in timestamp or increase priority of current item.
Calls `org-timestamp-up' or `org-priority-up', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-timestamp-p) (org-timestamp-up arg))
   (t (org-priority-up))))

(defun org-shiftdown (&optional arg)
  "Decrease item in timestamp or decrease priority of current item.
Calls `org-timestamp-down' or `org-priority-down', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-timestamp-p) (org-timestamp-down arg))
   (t (org-priority-down))))

(defun org-copy-special ()
  "Copy region in table or copy current subtree.
Calls `org-table-copy' or `org-copy-subtree', depending on context.
See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p) 'org-table-copy-region 'org-copy-subtree)))

(defun org-cut-special ()
  "Cut region in table or cut current subtree.
Calls `org-table-copy' or `org-cut-subtree', depending on context.
See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p) 'org-table-cut-region 'org-cut-subtree)))

(defun org-paste-special (arg)
  "Paste rectangular region into table, or past subtree relative to level.
Calls `org-table-paste-rectangle' or `org-paste-subtree', depending on context.
See the individual commands for more information."
  (interactive "P")
  (if (org-at-table-p)
      (org-table-paste-rectangle)
    (org-paste-subtree arg)))

(defun org-ctrl-c-ctrl-c (&optional arg)
  "Call realign table, or recognize a table.el table, or update keywords.
When the cursor is inside a table created by the table.el package,
activate that table.  Otherwise, if the cursor is at a normal table
created with org.el, re-align that table.  This command works even if
the automatic table editor has been turned off.
If the cursor is in one of the special #+KEYWORD lines, this triggers
scanning the buffer for these lines and updating the information.
If the cursor is on a #+TBLFM line, re-apply the formulae to the table."
  (interactive "P")
  (let  ((org-enable-table-editor t))
    (cond
     ((org-at-table.el-p)
      (require 'table)
      (beginning-of-line 1)
      (re-search-forward "|" (save-excursion (end-of-line 2) (point)))
      (table-recognize-table))
     ((org-at-table-p)
      (org-table-maybe-eval-formula)
      (if arg
	  (org-table-recalculate t)
	(org-table-maybe-recalculate-line))
      (org-table-align))
     ((save-excursion (beginning-of-line 1) (looking-at "#\\+\\([A-Z]+\\)"))
      (cond
       ((equal (match-string 1) "TBLFM")
	;; Recalculate the table before this line
	(save-excursion
	  (beginning-of-line 1)
	  (skip-chars-backward " \r\n\t")
	  (if (org-at-table-p) (org-table-recalculate t))))
       (t
	(org-mode-restart))))
     ((org-region-active-p)
      (org-table-convert-region (region-beginning) (region-end) arg))
     ((and (region-beginning) (region-end))
      (if (y-or-n-p "Convert inactive region to table? ")
	  (org-table-convert-region (region-beginning) (region-end) arg)
	(error "Abort")))
     (t (error "No table at point, and no region to make one")))))

(defun org-mode-restart ()
  "Restart Org-mode, to scan again for special lines.
Also updates the keyword regular expressions."
  (interactive)
  (let ((org-inhibit-startup t)) (org-mode))
  (message "Org-mode restarted to refresh keyword and special line setup"))

(defun org-return ()
  "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((org-at-table-p)
    (org-table-justify-field-maybe)
    (org-table-next-row))
   (t (newline))))

(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading' or `org-table-wrap-region', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p)
    (org-table-wrap-region arg))
   (t (org-insert-heading))))

;;; Menu entries

;; Define the Org-mode menus
(easy-menu-define org-tbl-menu org-mode-map "Tbl menu"
  '("Tbl"
    ["Align" org-ctrl-c-ctrl-c (org-at-table-p)]
    ["Next Field" org-cycle (org-at-table-p)]
    ["Previous Field" org-shifttab (org-at-table-p)]
    ["Next Row" org-return (org-at-table-p)]
    "--"
    ["Blank Field" org-table-blank-field (org-at-table-p)]
    ["Copy Field from Above" org-table-copy-down (org-at-table-p)]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft (org-at-table-p)]
     ["Move Column Right" org-metaright (org-at-table-p)]
     ["Delete Column" org-shiftmetaleft (org-at-table-p)]
     ["Insert Column" org-shiftmetaright (org-at-table-p)])
    ("Row"
     ["Move Row Up" org-metaup (org-at-table-p)]
     ["Move Row Down" org-metadown (org-at-table-p)]
     ["Delete Row" org-shiftmetaup (org-at-table-p)]
     ["Insert Row" org-shiftmetadown (org-at-table-p)]
     "--"
     ["Insert Hline" org-table-insert-hline (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Named Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-table-edit-formulas (org-at-table-p)]
     "--"
     ["Recalculate line" org-table-recalculate (org-at-table-p)]
     ["Recalculate all" (lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (org-region-active-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     (setq org-table-formula-debug (not org-table-formula-debug))
     :style toggle :selected org-table-formula-debug]
    "--"
    ["Invisible Vlines" org-table-toggle-vline-visibility
     :style toggle :selected (org-in-invisibility-spec-p '(org-table))]
    "--"
    ["Create" org-table-create (and (not (org-at-table-p))
				    org-enable-table-editor)]
    ["Convert Region" org-ctrl-c-ctrl-c (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert from/to table.el" org-table-create-with-table.el t]))

(easy-menu-define org-org-menu org-mode-map "Org menu"
  '("Org"
    ["Cycle Visibility" org-cycle (or (bobp) (outline-on-heading-p))]
    ["Cycle Global Visibility" org-shifttab (not (org-at-table-p))]
    ["Sparse Tree" org-occur t]
    ["Show All" show-all t]
    "--"
    ["New Heading" org-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Move Subtree Up" org-shiftmetaup (not (org-at-table-p))]
     ["Move Subtree Down" org-shiftmetadown (not (org-at-table-p))]
     "--"
     ["Copy Subtree"  org-copy-special (not (org-at-table-p))]
     ["Cut Subtree"  org-cut-special (not (org-at-table-p))]
     ["Paste Subtree"  org-paste-special (not (org-at-table-p))]
     "--"
     ["Promote Heading" org-metaleft (not (org-at-table-p))]
     ["Promote Subtree" org-shiftmetaleft (not (org-at-table-p))]
     ["Demote Heading"  org-metaright (not (org-at-table-p))]
     ["Demote Subtree"  org-shiftmetaright (not (org-at-table-p))]
     "--"
     ["Archive Subtree" org-archive-subtree t])
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ["Show TODO Tree" org-show-todo-tree t]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t])
    ("Dates and Scheduling"
     ["Timestamp" org-time-stamp t]
     ["Timestamp (inactive)" org-time-stamp-inactive t]
     ("Change Date"
      ["1 Day Later" org-timestamp-up-day t]
      ["1 Day Earlier" org-timestamp-down-day t]
      ["1 ... Later" org-shiftup t]
      ["1 ... Earlier" org-shiftdown t])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule t]
     ["Deadline" org-deadline t]
     "--"
     ["Goto Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t])
    "--"
    ("Timeline/Agenda"
     ["Show TODO Tree this File"  org-show-todo-tree t]
     ["Check Deadlines this File" org-check-deadlines t]
     ["Timeline Current File" org-timeline t]
     "--"
     ["Agenda" org-agenda t])
    ("File List for Agenda")
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t])
    "--"
    ("Export"
     ["ASCII" org-export-as-ascii t]
     ["Extract Visible Text" org-export-copy-visible t]
     ["HTML"  org-export-as-html t]
     ["HTML and Open" org-export-as-html-and-open t]
;     ["OPML" org-export-as-opml nil]
     "--"
     ["Option Template" org-insert-export-options-template t]
     ["Toggle Fixed Width" org-toggle-fixed-width-section t])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Info Documentation" org-info t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Build Full Customize Menu" org-create-customize-menu
      (fboundp 'customize-menu-create)])
    "--"
    ["Refresh setup" org-mode-restart t]
    ))

(defun org-info (&optional node)
  "Read documentation for Org-mode in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (require 'info)
  (Info-goto-node (format "(org)%s" (or node ""))))

(defun org-install-agenda-files-menu ()
  (easy-menu-change
   '("Org") "File List for Agenda"
   (append
    (list
     ["Edit File List" (customize-variable 'org-agenda-files) t]
     ["Add Current File to List" org-add-file t]
     ["Remove Current File from List" org-remove-file t]
     "--")
    (mapcar 'org-file-menu-entry org-agenda-files))))

;;; Documentation

(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org-mode, insert it into the menu."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
	(easy-menu-change
	 '("Org") "Customize"
	 `(["Browse Org group" org-customize t]
	   "--"
	   ,(customize-menu-create 'org)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"Org\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

;;; Miscellaneous stuff

(defun org-move-line-down (arg)
  "Move the current line down.  With prefix argument, move it past ARG lines."
  (interactive "p")
  (let ((col (current-column))
	beg end pos)
    (beginning-of-line 1) (setq beg (point))
    (beginning-of-line 2) (setq end (point))
    (beginning-of-line (+ 1 arg))
    (setq pos (move-marker (make-marker) (point)))
    (insert (delete-and-extract-region beg end))
    (goto-char pos)
    (move-to-column col)))

(defun org-move-line-up (arg)
  "Move the current line up.  With prefix argument, move it past ARG lines."
  (interactive "p")
  (let ((col (current-column))
	beg end pos)
    (beginning-of-line 1) (setq beg (point))
    (beginning-of-line 2) (setq end (point))
    (beginning-of-line (- arg))
    (setq pos (move-marker (make-marker) (point)))
    (insert (delete-and-extract-region beg end))
    (goto-char pos)
    (move-to-column col)))

;; Functions needed for Emacs/XEmacs region compatibility

(defun org-region-active-p ()
  "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
  (if org-ignore-region
      nil
    (if org-xemacs-p
	(and zmacs-regions (region-active-p))
      (and transient-mark-mode mark-active))))

(defun org-add-to-invisibility-spec (arg)
  "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (cond
   ((fboundp 'add-to-invisibility-spec)
    (add-to-invisibility-spec arg))
   ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
	(setq buffer-invisibility-spec (list arg)))
   (t
    (setq buffer-invisibility-spec
	  (cons arg buffer-invisibility-spec)))))

(defun org-remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (fboundp 'remove-from-invisibility-spec)
      (remove-from-invisibility-spec arg)
    (if (consp buffer-invisibility-spec)
	(setq buffer-invisibility-spec
	      (delete arg buffer-invisibility-spec)))))

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?."
  (if (consp buffer-invisibility-spec)
      (member arg buffer-invisibility-spec)
    nil))

(defun org-image-file-name-regexp ()
  "Return regexp matching the file names of images."
  (if (fboundp 'image-file-name-regexp)
      (image-file-name-regexp)
    (let ((image-file-name-extensions
	   '("png" "jpeg" "jpg" "gif" "tiff" "tif"
	     "xbm" "xpm" "pbm" "pgm" "ppm")))
      (concat "\\."
	      (regexp-opt (nconc (mapcar 'upcase
					 image-file-name-extensions)
				 image-file-name-extensions)
			  t)
	      "\\'"))))

;; Functions needed for compatibility with old outline.el

;; The following functions capture almost the entire compatibility code
;; between the different versions of outline-mode.  The only other place
;; where this is important are the font-lock-keywords.  Search for
;; `org-noutline-p' to find it.

;; C-a should go to the beginning of a *visible* line, also in the
;; new outline.el.  I guess this should be patched into Emacs?
(defun org-beginning-of-line ()
  "Go to the beginning of the current line.  If that is invisible, continue
to a visible line beginning.  This makes the function of C-a more intuitive."
  (interactive)
  (beginning-of-line 1)
  (if (bobp)
      nil
    (backward-char 1)
    (if (org-invisible-p)
	(while (and (not (bobp)) (org-invisible-p))
	  (backward-char 1)
	  (beginning-of-line 1))
      (forward-char 1))))
(when org-noutline-p
  (define-key org-mode-map "\C-a" 'org-beginning-of-line))

(defun org-invisible-p ()
  "Check if point is at a character currently not visible."
  (if org-noutline-p
      ;; Early versions of noutline don't have `outline-invisible-p'.
      (if (fboundp 'outline-invisible-p)
	  (outline-invisible-p)
	(get-char-property (point) 'invisible))
    (save-excursion
      (skip-chars-backward "^\r\n")
      (equal (char-before) ?\r))))

(defun org-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (if org-noutline-p
      (outline-back-to-heading invisible-ok)
    (if (looking-at outline-regexp)
	t
      (if (re-search-backward (concat (if invisible-ok "\\([\r\n]\\|^\\)" "^")
				      outline-regexp)
			      nil t)
	  (if invisible-ok
	      (progn (goto-char (match-end 1))
		     (looking-at outline-regexp)))
	(error "Before first heading")))))

(defun org-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (if org-noutline-p
      (outline-on-heading-p 'invisible-ok)
    (save-excursion
      (skip-chars-backward "^\n\r")
      (and (looking-at outline-regexp)
	   (or invisible-ok
	       (bobp)
	       (equal (char-before) ?\n))))))

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (if org-noutline-p
      (if (fboundp 'outline-up-heading-all)
	  (outline-up-heading-all arg)   ; emacs 21 version of outline.el
	(outline-up-heading arg t))      ; emacs 22 version of outline.el
    (org-back-to-heading t)
    (looking-at outline-regexp)
    (if (<= (- (match-end 0) (match-beginning 0)) arg)
	(error "Cannot move up %d levels" arg)
    (re-search-backward
     (concat "[\n\r]" (regexp-quote
		       (make-string (- (match-end 0) (match-beginning 0) arg)
				    ?*))
	     "[^*]"))
    (forward-char 1))))

(defun org-show-hidden-entry ()
  "Show an entry where even the heading is hidden."
  (save-excursion
    (if (not org-noutline-p)
	(progn
	  (org-back-to-heading t)
	  (org-flag-heading nil)))
    (org-show-entry)))

(defun org-check-occur-regexp (regexp)
  "If REGEXP starts with \"^\", modify it to check for \\r as well.
Of course, only for the old outline mode."
  (if org-noutline-p
      regexp
    (if (string-match "^\\^" regexp)
	(concat "[\n\r]" (substring regexp 1))
      regexp)))

(defun org-flag-heading (flag &optional entry)
  "Flag the current heading.  FLAG non-nil means make invisible.
When ENTRY is non-nil, show the entire entry."
  (save-excursion
    (org-back-to-heading t)
    (if (not org-noutline-p)
	;; Make the current headline visible
	(outline-flag-region (max 1 (1- (point))) (point) (if flag ?\r ?\n)))
    ;; Check if we should show the entire entry
    (if entry
	(progn
	  (org-show-entry)
	  (save-excursion  ;; FIXME: Is this the fix for points in the   -|
			   ;;        middle of text?                      |
	    (and (outline-next-heading)   ;;                              |
		 (org-flag-heading nil))))  ; show the next heading      _|
      (outline-flag-region (max 1 (1- (point)))
			   (save-excursion (outline-end-of-heading) (point))
			   (if org-noutline-p
			       flag
			     (if flag ?\r ?\n))))))

(defun org-show-subtree ()
  "Show everything after this heading at deeper levels."
  (outline-flag-region
   (point)
   (save-excursion
     (outline-end-of-subtree) (outline-next-heading) (point))
   (if org-noutline-p nil ?\n)))

(defun org-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (outline-flag-region
     (1- (point))
     (save-excursion
       (re-search-forward (concat "[\r\n]\\(" outline-regexp "\\)") nil 'move)
       (or (match-beginning 1) (point-max)))
     (if org-noutline-p nil ?\n))))


(defun org-make-options-regexp (kwds)
  "Make a regular expression for keyword lines."
  (concat
   (if org-noutline-p "^" "[\n\r]")
   "#?[ \t]*\\+\\("
   (mapconcat 'regexp-quote kwds "\\|")
   "\\):[ \t]*"
   (if org-noutline-p "\\(.+\\)" "\\([^\n\r]+\\)")))

;; Make `bookmark-jump' show the jump location if it was hidden.
(eval-after-load "bookmark"
  '(if (boundp 'bookmark-after-jump-hook)
       ;; We can use the hook
       (add-hook 'bookmark-after-jump-hook 'org-bookmark-jump-unhide)
     ;; Hook not available, use advice
     (defadvice bookmark-jump (after org-make-visible activate)
       "Make the position visible."
       (org-bookmark-jump-unhide))))

(defun org-bookmark-jump-unhide ()
  "Unhide the current position, to show the bookmark location."
  (and (eq major-mode 'org-mode)
       (or (org-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (org-invisible-p)))
       (org-show-hierarchy-above)))

;;; Finish up

(provide 'org)

(run-hooks 'org-load-hook)

;; arch-tag: e77da1a7-acc7-4336-b19e-efa25af3f9fd
;;; org.el ends here
