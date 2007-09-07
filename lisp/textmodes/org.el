;;; org.el --- Outline-based notes management and organizer
;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 5.08
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Org-mode is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org-mode develops organizational tasks around NOTES files that contain
;; information about projects as plain text.  Org-mode is implemented on
;; top of outline-mode, which makes it possible to keep the content of
;; large files well structured.  Visibility cycling and structure editing
;; help to work with the tree.  Tables are easily created with a built-in
;; table editor.  Org-mode supports ToDo items, deadlines, time stamps,
;; and scheduling.  It dynamically compiles entries into an agenda that
;; utilizes and smoothly integrates much of the Emacs calendar and diary.
;; Plain text URL-like links connect to websites, emails, Usenet
;; messages, BBDB entries, and any files related to the projects.  For
;; printing and sharing of notes, an Org-mode file can be exported as a
;; structured ASCII file, as HTML, or (todo and agenda items only) as an
;; iCalendar file.  It can also serve as a publishing tool for a set of
;; linked webpages.
;;
;; Installation and Activation
;; ---------------------------
;; See the corresponding sections in the manual at
;;
;;   http://orgmode.org/org.html#Installation
;;
;; Documentation
;; -------------
;; The documentation of Org-mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the homepage of
;; Org-mode, you can read the same text online as HTML.  There is also an
;; excellent reference card made by Philip Rooke.  This card can be found
;; in the etc/ directory of Emacs 22.
;;
;; A list of recent changes can be found at
;; http://orgmode.org/Changes
;;
;;; Code:

;;;; Require other packages

(eval-when-compile
  (require 'cl)
  (require 'gnus-sum)
  (require 'calendar))
;; For XEmacs, noutline is not yet provided by outline.el, so arrange for
;; the file noutline.el being loaded.
(if (featurep 'xemacs) (condition-case nil (require 'noutline)))
;; We require noutline, which might be provided in outline.el
(require 'outline) (require 'noutline)
;; Other stuff we need.
(require 'time-date)
(require 'easymenu)

;;;; Customization variables

;;; Version

(defconst org-version "5.08"
  "The version number of the file org.el.")
(defun org-version ()
  (interactive)
  (message "Org-mode version %s" org-version))

;;; Compatibility constants
(defconst org-xemacs-p (featurep 'xemacs)) ; not used by org.el itself
(defconst org-format-transports-properties-p
  (let ((x "a"))
    (add-text-properties 0 1 '(test t) x)
    (get-text-property 0 'test (format "%s" x)))
  "Does format transport text properties?")

(defmacro org-unmodified (&rest body)
  "Execute body without changing buffer-modified-p."
  `(set-buffer-modified-p
    (prog1 (buffer-modified-p) ,@body)))

(defmacro org-re (s)
  "Replace posix classes in regular expression."
  (if (featurep 'xemacs)
      (let ((ss s))
	(save-match-data
	  (while (string-match "\\[:alnum:\\]" ss)
	    (setq ss (replace-match "a-zA-Z0-9" t t ss)))
	  (while (string-match "\\[:alpha:\\]" ss)
	    (setq ss (replace-match "a-zA-Z" t t ss)))
	  ss))
    s))

(defmacro org-preserve-lc (&rest body)
  `(let ((_line (org-current-line))
	 (_col (current-column)))
     (unwind-protect
	 (progn ,@body)
       (goto-line _line)
       (move-to-column _col))))     

;;; The custom variables

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'hypermedia
  :group 'calendar)

(defgroup org-startup nil
  "Options concerning startup of Org-mode."
  :tag "Org Startup"
  :group 'org)

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

(defcustom org-startup-align-all-tables nil
  "Non-nil means, align all tables when visiting a file.
This is useful when the column width in tables is forced with <N> cookies
in table fields.  Such tables will look correct only after the first re-align.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: align
   #+STARTUP: noalign"
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

(defcustom org-replace-disputed-keys nil
  "Non-nil means use alternative key bindings for some keys.
Org-mode uses S-<cursor> keys for changing timestamps and priorities.
These keys are also used by other packages like `CUA-mode' or `windmove.el'.
If you want to use Org-mode together with one of these other modes,
or more generally if you would like to move some Org-mode commands to
other keys, set this variable and configure the keys with the variable
`org-disputed-keys'.

This option is only relevant at load-time of Org-mode, and must be set
*before* org.el is loaded.  Changing it requires a restart of Emacs to
become effective."
  :group 'org-startup
  :type 'boolean)

(if (fboundp 'defvaralias)
    (defvaralias 'org-CUA-compatible 'org-replace-disputed-keys))

(defcustom org-disputed-keys
  '(([(shift up)]		. [(meta p)])
    ([(shift down)]		. [(meta n)])
    ([(shift left)]		. [(meta -)])
    ([(shift right)]		. [(meta +)])
    ([(control shift right)] 	. [(meta shift +)])
    ([(control shift left)]	. [(meta shift -)]))
  "Keys for which Org-mode and other modes compete.
This is an alist, cars are the default keys, second element specifies
the alternative to use when `org-replace-disputed-keys' is t.

Keys can be specified in any syntax supported by `define-key'.
The value of this option takes effect only at Org-mode's startup,
therefore you'll have to restart Emacs to apply it after changing."
  :group 'org-startup
  :type 'alist)

(defun org-key (key)
  "Select key according to `org-replace-disputed-keys' and `org-disputed-keys'.
Or return the original if not disputed."
  (if org-replace-disputed-keys
      (let* ((nkey (key-description key))
	     (x (org-find-if (lambda (x)
			       (equal (key-description (car x)) nkey))
			     org-disputed-keys)))
	(if x (cdr x) key))
    key))

(defun org-find-if (predicate seq)
  (catch 'exit
    (while seq
      (if (funcall predicate (car seq))
	  (throw 'exit (car seq))
	(pop seq)))))

(defun org-defkey (keymap key def)
  "Define a key, possibly translated, as returned by `org-key'."
  (define-key keymap (org-key key) def))

(defcustom org-ellipsis nil
  "The ellipsis to use in the Org-mode outline.
When nil, just use the standard three dots.  When a string, use that instead,
When a face, use the standart 3 dots, but with the specified face.
The change affects only Org-mode (which will then use its own display table).
Changing this requires executing `M-x org-mode' in a buffer to become
effective."
  :group 'org-startup
  :type '(choice (const :tag "Default" nil)
		 (face :tag "Face" :value org-warning)
		 (string :tag "String" :value "...#")))

(defvar org-display-table nil
  "The display table for org-mode, in case `org-ellipsis' is non-nil.")

(defgroup org-keywords nil
  "Keywords in Org-mode."
  :tag "Org Keywords"
  :group 'org)

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

(defcustom org-closed-string "CLOSED:"
  "String used as the prefix for timestamps logging closing a TODO entry."
  :group 'org-keywords
  :type 'string)

(defcustom org-clock-string "CLOCK:"
  "String used as prefix for timestamps clocking work hours on an item."
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
\\[org-toggle-fixed-width-section]."
  :group 'org-keywords
  :type 'string)

(defconst org-repeat-re
  (concat "\\(?:\\<\\(?:" org-scheduled-string "\\|" org-deadline-string "\\)"
	  " +<[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*\\)\\(\\+[0-9]+[dwmy]\\)")
  "Regular expression for specifying repeated events.
After a match, group 1 contains the repeat expression.")

(defgroup org-structure nil
  "Options concerning the general structure of Org-mode files."
  :tag "Org Structure"
  :group 'org)

(defgroup org-reveal-location nil
  "Options about how to make context of a location visible."
  :tag "Org Reveal Location"
  :group 'org-structure)

(defcustom org-show-hierarchy-above '((default . t))
  "Non-nil means, show full hierarchy when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the hierarchy of headings
above the exposed location is shown.
Turning this off for example for sparse trees makes them very compact.
Instead of t, this can also be an alist specifying this option for different
contexts.  Valid contexts are
  agenda         when exposing an entry from the agenda
  org-goto       when using the command `org-goto' on key C-c C-j
  occur-tree     when using the command `org-occur' on key C-c /
  tags-tree      when constructing a sparse tree based on tags matches
  link-search    when exposing search matches associated with a link
  mark-goto      when exposing the jump goal of a mark
  bookmark-jump  when exposing a bookmark location
  isearch        when exiting from an incremental search
  default        default for all contexts not set explicitly"
  :group 'org-reveal-location
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
		    (const agenda)
		    (const org-goto)
		    (const occur-tree)
		    (const tags-tree)
		    (const link-search)
		    (const mark-goto)
		    (const bookmark-jump)
		    (const isearch)
		    (const default))
		   (boolean)))))

(defcustom org-show-following-heading '((default . nil))
  "Non-nil means, show following heading when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the heading following the
match is shown.
Turning this off for example for sparse trees makes them very compact,
but makes it harder to edit the location of the match.  In such a case,
use the command \\[org-reveal] to show more context.
Instead of t, this can also be an alist specifying this option for different
contexts.  See `org-show-hierarchy-above' for valid contexts."
  :group 'org-reveal-location
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
		    (const agenda)
		    (const org-goto)
		    (const occur-tree)
		    (const tags-tree)
		    (const link-search)
		    (const mark-goto)
		    (const bookmark-jump)
		    (const isearch)
		    (const default))
		   (boolean)))))

(defcustom org-show-siblings '((default . nil) (isearch t))
  "Non-nil means, show all sibling heading when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the sibling of the current entry
heading are all made visible.  If `org-show-hierarchy-above' is t,
the same happens on each level of the hierarchy above the current entry.

By default this is on for the isearch context, off for all other contexts.
Turning this off for example for sparse trees makes them very compact,
but makes it harder to edit the location of the match.  In such a case,
use the command \\[org-reveal] to show more context.
Instead of t, this can also be an alist specifying this option for different
contexts.  See `org-show-hierarchy-above' for valid contexts."
  :group 'org-reveal-location
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
		    (const agenda)
		    (const org-goto)
		    (const occur-tree)
		    (const tags-tree)
		    (const link-search)
		    (const mark-goto)
		    (const bookmark-jump)
		    (const isearch)
		    (const default))
		   (boolean)))))

(defgroup org-cycle nil
  "Options concerning visibility cycling in Org-mode."
  :tag "Org Cycle"
  :group 'org-structure)

(defcustom org-drawers '("PROPERTIES")
  "Names of drawers.  Drawers are not opened by cycling on the headline above.
Drawers only open with a TAB on the drawer line itself.  A drawer looks like
this:
   :DRAWERNAME:
   .....
   :END:
The drawer \"PROPERTIES\" is special for capturing properties through
the property API."
  :group 'org-structure
  :type '(repeat (string :tag "Drawer Name")))

(defcustom org-cycle-global-at-bob nil
  "Cycle globally if cursor is at beginning of buffer and not at a headline.
This makes it possible to do global cycling without having to use S-TAB or
C-u TAB.  For this special case to work, the first line of the buffer
must not be a headline - it may be empty ot some other text.  When used in
this way, `org-cycle-hook' is disables temporarily, to make sure the
cursor stays at the beginning of the buffer.
When this option is nil, don't do anything special at the beginning
of the buffer."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-emulate-tab t
  "Where should `org-cycle' emulate TAB.
nil         Never
white       Only in completely white lines
whitestart  Only at the beginning of lines, before the first non-white char.
t           Everywhere except in headlines
exc-hl-bol  Everywhere except at the start of a headline
If TAB is used in a place where it does not emulate TAB, the current subtree
visibility is cycled."
  :group 'org-cycle
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Before first char in a line" whitestart)
		 (const :tag "Everywhere except in headlines" t)
		 (const :tag "Everywhere except at bol in headlines" exc-hl-bol)
		 ))

(defcustom org-cycle-separator-lines 2
  "Number of empty lines needed to keep an empty line between collapsed trees.
If you leave an empty line between the end of a subtree and the following
headline, this empty line is hidden when the subtree is folded.
Org-mode will leave (exactly) one empty line visible if the number of
empty lines is equal or larger to the number given in this variable.
So the default 2 means, at least 2 empty lines after the end of a subtree
are needed to produce free space between a collapsed subtree and the
following headline.

Special case: when 0, never leave empty lines in collapsed view."
  :group 'org-cycle
  :type 'integer)

(defcustom org-cycle-hook '(org-cycle-hide-archived-subtrees
			    org-cycle-hide-drawers
			    org-cycle-show-empty-lines
			    org-optimize-window-after-visibility-change)
  "Hook that is run after `org-cycle' has changed the buffer visibility.
The function(s) in this hook must accept a single argument which indicates
the new state that was set by the most recent `org-cycle' command.  The
argument is a symbol.  After a global state change, it can have the values
`overview', `content', or `all'.  After a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :type 'hook)

(defgroup org-edit-structure nil
  "Options concerning structure editing in Org-mode."
  :tag "Org Edit Structure"
  :group 'org-structure)

(defcustom org-special-ctrl-a/e nil
  "Non-nil means `C-a' and `C-e' behave specially in headlines and items.
When t, `C-a' will bring back the cursor to the beginning of the
headline text, i.e. after the stars and after a possible TODO keyword.
In an item, this will be the position after the bullet.
When the cursor is already at that position, another `C-a' will bring
it to the beginning of the line.
`C-e' will jump to the end of the headline, ignoring the presence of tags
in the headline.  A second `C-e' will then jump to the true end of the
line, after any tags.
When set to the symbol `reversed', the first `C-a' or `C-e' works normally,
and only a directly following, identical keypress will bring the cursor
to the special positions."
  :group 'org-edit-structure
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "after bullet first" t)
	  (const :tag "border first" reversed)))

(if (fboundp 'defvaralias)
    (defvaralias 'org-special-ctrl-a 'org-special-ctrl-a/e))

(defcustom org-odd-levels-only nil
  "Non-nil means, skip even levels and only use odd levels for the outline.
This has the effect that two stars are being added/taken away in
promotion/demotion commands.  It also influences how levels are
handled by the exporters.
Changing it requires restart of `font-lock-mode' to become effective
for fontification also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: odd
   #+STARTUP: oddeven"
  :group 'org-edit-structure
  :group 'org-font-lock
  :type 'boolean)

(defcustom org-adapt-indentation t
  "Non-nil means, adapt indentation when promoting and demoting.
When this is set and the *entire* text in an entry is indented, the
indentation is increased by one space in a demotion command, and
decreased by one in a promotion command.  If any line in the entry
body starts at column 0, indentation is not changed at all."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-blank-before-new-entry '((heading . nil)
					(plain-list-item . nil))
  "Should `org-insert-heading' leave a blank line before new heading/item?
The value is an alist, with `heading' and `plain-list-item' as car,
and a boolean flag as cdr."
  :group 'org-edit-structure
  :type '(list
	  (cons (const heading) (boolean))
	  (cons (const plain-list-item) (boolean))))

(defcustom org-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-enable-fixed-width-editor t
  "Non-nil means, lines starting with \":\" are treated as fixed-width.
This currently only means, they are never auto-wrapped.
When nil, such lines will be treated like ordinary lines.
See also the QUOTE keyword."
  :group 'org-edit-structure
  :type 'boolean)

(defgroup org-sparse-trees nil
  "Options concerning sparse trees in Org-mode."
  :tag "Org Sparse Trees"
  :group 'org-structure)

(defcustom org-highlight-sparse-tree-matches t
  "Non-nil means, highlight all matches that define a sparse tree.
The highlights will automatically disappear the next time the buffer is
changed by an edit command."
  :group 'org-sparse-trees
  :type 'boolean)

(defcustom org-remove-highlights-with-change t
  "Non-nil means, any change to the buffer will remove temporary highlights.
Such highlights are created by `org-occur' and `org-clock-display'.
When nil, `C-c C-c needs to be used to get rid of the highlights.
The highlights created by `org-preview-latex-fragment' always need
`C-c C-c' to be removed."
  :group 'org-sparse-trees
  :group 'org-time
  :type 'boolean)


(defcustom org-occur-hook '(org-first-headline-recenter)
  "Hook that is run after `org-occur' has constructed a sparse tree.
This can be used to recenter the window to show as much of the structure
as possible."
  :group 'org-sparse-trees
  :type 'hook)

(defgroup org-plain-lists nil
  "Options concerning plain lists in Org-mode."
  :tag "Org Plain lists"
  :group 'org-structure)

(defcustom org-cycle-include-plain-lists nil
  "Non-nil means, include plain lists into visibility cycling.
This means that during cycling, plain list items will *temporarily* be
interpreted as outline headlines with a level given by 1000+i where i is the
indentation of the bullet.  In all other operations, plain list items are
not seen as headlines.  For example, you cannot assign a TODO keyword to
such an item."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-plain-list-ordered-item-terminator t
  "The character that makes a line with leading number an ordered list item.
Valid values are ?. and ?\).  To get both terminators, use t.  While
?. may look nicer, it creates the danger that a line with leading
number may be incorrectly interpreted as an item.  ?\) therefore is
the safe choice."
  :group 'org-plain-lists
  :type '(choice (const :tag "dot like in \"2.\"" ?.)
		 (const :tag "paren like in \"2)\"" ?\))
		 (const :tab "both" t)))

(defcustom org-auto-renumber-ordered-lists t
  "Non-nil means, automatically renumber ordered plain lists.
Renumbering happens when the sequence have been changed with
\\[org-shiftmetaup] or \\[org-shiftmetadown].  After other editing commands,
use \\[org-ctrl-c-ctrl-c] to trigger renumbering."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-provide-checkbox-statistics t
  "Non-nil means, update checkbox statistics after insert and toggle.
When this is set, checkbox statistics is updated each time you either insert
a new checkbox with \\[org-insert-todo-heading] or toggle a checkbox
with \\[org-ctrl-c-ctrl-c\\]."
  :group 'org-plain-lists
  :type 'boolean)

(defgroup org-archive nil
  "Options concerning archiving in Org-mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-archive-tag "ARCHIVE"
  "The tag that marks a subtree as archived.
An archived subtree does not open during visibility cycling, and does
not contribute to the agenda listings."
  :group 'org-archive
  :group 'org-keywords
  :type 'string)

(defcustom org-agenda-skip-archived-trees t
  "Non-nil means, the agenda will skip any items located in archived trees.
An archived tree is a tree marked with the tag ARCHIVE."
  :group 'org-archive
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-cycle-open-archived-trees nil
  "Non-nil means, `org-cycle' will open archived trees.
An archived tree is a tree marked with the tag ARCHIVE.
When nil, archived trees will stay folded.  You can still open them with
normal outline commands like `show-all', but not with the cycling commands."
  :group 'org-archive
  :group 'org-cycle
  :type 'boolean)

(defcustom org-sparse-tree-open-archived-trees nil
  "Non-nil means sparse tree construction shows matches in archived trees.
When nil, matches in these trees are highlighted, but the trees are kept in
collapsed state."
  :group 'org-archive
  :group 'org-sparse-trees
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
  :group 'org-archive
  :type 'string)

(defcustom org-archive-mark-done t
  "Non-nil means, mark entries as DONE when they are moved to the archive file.
This can be a string to set the keyword to use.  When t, Org-mode will
use the first keyword in its list that means done."
  :group 'org-archive
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (string :tag "Use this keyword")))

(defcustom org-archive-stamp-time t
  "Non-nil means, add a time stamp to entries moved to an archive file."
  :group 'org-archive
  :type 'boolean)

(defcustom org-archive-save-context-info '(time file category todo itags)
  "Parts of context info that should be stored as properties when archiving.
When a subtree is moved to an archive file, it looses information given by
context, like inherited tags, the category, and possibly also the TODO
state (depending on the variable `org-archive-mark-done').
This variable can be a list of any of the following symbols:

time       The time of archiving.
file       The file where the entry originates.
itags      The local tags, in the headline of the subtree.
ltags      The tags the subtree inherits from further up the hierarchy.
todo       The pre-archive TODO state.
category   The category, taken from file name or #+CATEGORY lines.

For each symbol present in the list, a property will be created in
the archived entry, with a prefix \"PRE_ARCHIVE_\", to remember this
information."
  :group 'org-archive
  :type '(set
	  (const :tag "File" file)
	  (const :tag "Category" category)
	  (const :tag "TODO state" todo)
	  (const :tag "TODO state" priority)
	  (const :tag "Inherited tags" itags)
	  (const :tag "Local tags" ltags)))

(defgroup org-table nil
  "Options concerning tables in Org-mode."
  :tag "Org Table"
  :group 'org)

(defcustom org-enable-table-editor 'optimized
  "Non-nil means, lines starting with \"|\" are handled by the table editor.
When nil, such lines will be treated like ordinary lines.

When equal to the symbol `optimized', the table editor will be optimized to
do the following:
- Automatic overwrite mode in front of whitespace in table fields.
  This makes the structure of the table stay in tact as long as the edited
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

(defcustom orgtbl-radio-table-templates
  '((latex-mode "% BEGIN RECEIVE ORGTBL %n
% END RECEIVE ORGTBL %n
\\begin{comment}
#+ORGTBL: SEND %n orgtbl-to-latex :splice nil :skip 0
| | |
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGTBL %n
@c END RECEIVE ORGTBL %n
@ignore
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGTBL %n -->
<!-- END RECEIVE ORGTBL %n -->
<!--
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
-->\n"))
  "Templates for radio tables in different major modes.
All occurrences of %n in a template will be replaced with the name of the
table, obtained by prompting the user."
  :group 'org-table
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

(defgroup org-table-settings nil
  "Settings for tables in Org-mode."
  :tag "Org Table Settings"
  :group 'org-table)

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table-settings
   :type 'string)

(defcustom org-table-number-regexp
  "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|\\(0[xX]\\)[0-9a-fA-F]+\\|nan\\)$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left.

The default value of this option is a regular expression which allows
anything which looks remotely like a number as used in scientific
context.  For example, all of the following will be considered a
number:
    12    12.2    2.4e-08    2x10^12    4.034+-0.02    2.7(10)  >3.5

Other options offered by the customize interface are more restrictive."
  :group 'org-table-settings
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
	  (const :tag "Very General Number-Like, including hex"
		 "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*\\|\\(0[xX]\\)[0-9a-fA-F]+\\|nan\\)$")
	  (string :tag "Regexp:")))

(defcustom org-table-number-fraction 0.5
  "Fraction of numbers in a column required to make the column align right.
In a column all non-white fields are considered.  If at least this
fraction of fields is matched by `org-table-number-fraction',
alignment to the right border applies."
  :group 'org-table-settings
  :type 'number)

(defgroup org-table-editing nil
  "Bahavior of tables during editing in Org-mode."
  :tag "Org Table Editing"
  :group 'org-table)

(defcustom org-table-automatic-realign t
  "Non-nil means, automatically re-align table when pressing TAB or RETURN.
When nil, aligning is only done with \\[org-table-align], or after column
removal/insertion."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-auto-blank-field t
  "Non-nil means, automatically blank table field when starting to type into it.
This only happens when typing immediately after a field motion
command (TAB, S-TAB or RET).
Only relevant when `org-enable-table-editor' is equal to `optimized'."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-tab-jumps-over-hlines t
  "Non-nil means, tab in the last column of a table with jump over a hline.
If a horizontal separator line is following the current line,
`org-table-next-field' can either create a new row before that line, or jump
over the line.  When this option is nil, a new line will be created before
this line."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-tab-recognizes-table.el t
  "Non-nil means, TAB will automatically notice a table.el table.
When it sees such a table, it moves point into it and - if necessary -
calls `table-recognize-table'."
  :group 'org-table-editing
  :type 'boolean)

(defgroup org-table-calculation nil
  "Options concerning tables in Org-mode."
  :tag "Org Table Calculation"
  :group 'org-table)

(defcustom org-table-use-standard-references t
  "Should org-mode work with table refrences like B3 instead of @3$2?
Possible values are:
nil     never use them
from    accept as input, do not present for editing
t:      accept as input and present for editing"
  :group 'org-table-calculation
  :type '(choice
	  (const :tag "Never, don't even check unser input for them" nil)
	  (const :tag "Always, both as user input, and when editing" t)
	  (const :tag "Convert user input, don't offer during editing" 'from)))

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
The list must contain alternating symbols (Calc modes variables and values).
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

and then use it in an equation like `$1*$c'.

Constants can also be defined on a per-file basis using a line like

#+CONSTANTS: c=299792458. pi=3.14 eps=2.4e-6"
  :group 'org-table-calculation
  :type '(repeat
	  (cons (string :tag "name")
		(string :tag "value"))))

(defvar org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")
(make-variable-buffer-local 'org-table-formula-constants-local)

(defcustom org-table-allow-automatic-line-recalculation t
  "Non-nil means, lines marked with |#| or |*| will be recomputed automatically.
Automatically means, when TAB or RET or C-c C-c are pressed in the line."
  :group 'org-table-calculation
  :type 'boolean)

(defgroup org-link nil
  "Options concerning links in Org-mode."
  :tag "Org Link"
  :group 'org)

(defvar org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the #+LINK lines.")
(make-variable-buffer-local 'org-link-abbrev-alist-local)

(defcustom org-link-abbrev-alist nil
  "Alist of link abbreviations.
The car of each element is a string, to be replaced at the start of a link.
The cdrs are replacement values, like (\"linkkey\" . REPLACE).  Abbreviated
links in Org-mode buffers can have an optional tag after a double colon, e.g.

     [[linkkey:tag][description]]

If REPLACE is a string, the tag will simply be appended to create the link.
If the string contains \"%s\", the tag will be inserted there.  REPLACE may
also be a function that will be called with the tag as the only argument to
create the link.  See the manual for examples."
  :group 'org-link
  :type 'alist)

(defcustom org-descriptive-links t
  "Non-nil means, hide link part and only show description of bracket links.
Bracket links are like [[link][descritpion]]. This variable sets the initial
state in new org-mode buffers.  The setting can then be toggled on a
per-buffer basis from the Org->Hyperlinks menu."
  :group 'org-link
  :type 'boolean)

(defcustom org-link-file-path-type 'adaptive
  "How the path name in file links should be stored.
Valid values are:

relative  relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  absolute path, if possible with ~ for home directory.
noabbrev  absolute path, no abbreviation of home directory.
adaptive  Use relative path for files in the current directory and sub-
          directories of it.  For other files, use an absolute path."
  :group 'org-link
  :type '(choice
	  (const relative)
	  (const absolute)
	  (const noabbrev)
	  (const adaptive)))

(defcustom org-activate-links '(bracket angle plain radio tag date)
  "Types of links that should be activated in Org-mode files.
This is a list of symbols, each leading to the activation of a certain link
type.  In principle, it does not hurt to turn on most link types - there may
be a small gain when turning off unused link types.  The types are:

bracket   The recommended [[link][description]] or [[link]] links with hiding.
angular   Links in angular brackes that may contain whitespace like
          <bbdb:Carsten Dominik>.
plain     Plain links in normal text, no whitespace, like http://google.com.
radio     Text that is matched by a radio target, see manual for details.
tag       Tag settings in a headline (link to tag search).
date      Time stamps (link to calendar).

Changing this variable requires a restart of Emacs to become effective."
  :group 'org-link
  :type '(set (const :tag "Double bracket links (new style)" bracket)
	      (const :tag "Angular bracket links (old style)" angular)
	      (const :tag "plain text links" plain)
	      (const :tag "Radio target matches" radio)
	      (const :tag "Tags" tag)
	      (const :tag "Tags" target)
	      (const :tag "Timestamps" date)))

(defgroup org-link-store nil
  "Options concerning storing links in Org-mode"
  :tag "Org Store Link"
  :group 'org-link)

(defcustom org-email-link-description-format "Email %c: %.30s"
  "Format of the description part of a link to an email or usenet message.
The following %-excapes will be replaced by corresponding information:

%F   full \"From\" field
%f   name, taken from \"From\" field, address if no name
%T   full \"To\" field
%t   first name in \"To\" field, address if no name
%c   correspondent.  Unually \"from NAME\", but if you sent it yourself, it
     will be \"to NAME\".  See also the variable `org-from-is-user-regexp'.
%s   subject
%m   message-id.

You may use normal field width specification between the % and the letter.
This is for example useful to limit the length of the subject.

Examples: \"%f on: %.30s\", \"Email from %f\", \"Email %c\""
  :group 'org-link-store
  :type 'string)

(defcustom org-from-is-user-regexp
  (let (r1 r2)
    (when (and user-mail-address (not (string= user-mail-address "")))
      (setq r1 (concat "\\<" (regexp-quote user-mail-address) "\\>")))
    (when (and user-full-name (not (string= user-full-name "")))
      (setq r2 (concat "\\<" (regexp-quote user-full-name) "\\>")))
    (if (and r1 r2) (concat r1 "\\|" r2) (or r1 r2)))
  "Regexp mached against the \"From:\" header of an email or usenet message.
It should match if the message is from the user him/herself."
  :group 'org-link-store
  :type 'regexp)

(defcustom org-context-in-file-links t
  "Non-nil means, file links from `org-store-link' contain context.
A search string will be added to the file name with :: as separator and
used to find the context when the link is activated by the command
`org-open-at-point'.
Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link-store
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
  :group 'org-link-store
  :type 'boolean)

(defcustom org-usenet-links-prefer-google nil
  "Non-nil means, `org-store-link' will create web links to Google groups.
When nil, Gnus will be used for such links.
Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link-store
  :type 'boolean)

(defgroup org-link-follow nil
  "Options concerning following links in Org-mode"
  :tag "Org Follow Link"
  :group 'org-link)

(defcustom org-tab-follows-link nil
  "Non-nil means, on links TAB will follow the link.
Needs to be set before org.el is loaded."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-return-follows-link nil
  "Non-nil means, on links RET will follow the link.
Needs to be set before org.el is loaded."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-mouse-1-follows-link t
  "Non-nil means, mouse-1 on a link will follow the link.
A longer mouse click will still set point.  Does not wortk on XEmacs.
Needs to be set before org.el is loaded."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-mark-ring-length 4
  "Number of different positions to be recorded in the ring
Changing this requires a restart of Emacs to work correctly."
  :group 'org-link-follow
  :type 'interger)

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
  :group 'org-link-follow
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

(defcustom org-display-internal-link-with-indirect-buffer nil
  "Non-nil means, use indirect buffer to display infile links.
Activating internal links (from one location in a file to another location
in the same file) normally just jumps to the location.  When the link is
activated with a C-u prefix (or with mouse-3), the link is displayed in
another window.  When this option is set, the other window actually displays
an indirect buffer clone of the current buffer, to avoid any visibility
changes to the current buffer."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-open-non-existing-files nil
  "Non-nil means, `org-open-file' will open non-existing files.
When nil, an error will be generated."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-link-mailto-program '(browse-url "mailto:%a?subject=%s")
  "Function and arguments to call for following mailto links.
This is a list with the first element being a lisp function, and the
remaining elements being arguments to the function.  In string arguments,
%a will be replaced by the address, and %s will be replaced by the subject
if one was given like in <mailto:arthur@galaxy.org::this subject>."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "browse-url" (browse-url-mail "mailto:%a?subject=%s"))
	  (const :tag "compose-mail" (compose-mail "%a" "%s"))
	  (const :tag "message-mail" (message-mail "%a" "%s"))
	  (cons :tag "other" (function) (repeat :tag "argument" sexp))))

(defcustom org-confirm-shell-link-function 'yes-or-no-p
  "Non-nil means, ask for confirmation before executing shell links.
Shell links can be dangerous, just thing about a link

     [[shell:rm -rf ~/*][Google Search]]

This link would show up in your Org-mode document as \"Google Search\"
but really it would remove your entire home directory.
Therefore I *definitely* advise against setting this variable to nil.
Just change it to `y-or-n-p' of you want to confirm with a single key press
rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))

(defcustom org-confirm-elisp-link-function 'yes-or-no-p
  "Non-nil means, ask for confirmation before executing elisp links.
Elisp links can be dangerous, just think about a link

     [[elisp:(shell-command \"rm -rf ~/*\")][Google Search]]

This link would show up in your Org-mode document as \"Google Search\"
but really it would remove your entire home directory.
Therefore I *definitely* advise against setting this variable to nil.
Just change it to `y-or-n-p' of you want to confirm with a single key press
rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))

(defconst org-file-apps-defaults-gnu
  '((remote . emacs)
    (t . mailcap))
  "Default file applications on a UNIX or GNU/Linux system.
See `org-file-apps'.")

(defconst org-file-apps-defaults-macosx
  '((remote . emacs)
    (t . "open %s")
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
  (list
   '(remote . emacs)
   (cons t
	 (list (if (featurep 'xemacs)
		   'mswindows-shell-execute
		 'w32-shell-execute)
	       "open" 'file)))
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
    ("bib" . emacs)
    )
  "External applications for opening `file:path' items in a document.
Org-mode uses system defaults for different file types, but
you can use this variable to set the application for a given file
extension.  The entries in this list are cons cells where the car identifies
files and the cdr the corresponding command.  Possible values for the
file identifier are
 \"ext\"         A string identifying an extension
 `directory'   Matches a directory
 `remote'      Matches a remote file, accessible through tramp or efs.
               Remote files most likely should be visited through Emacs
               because external applications cannot handle such paths.
 t             Default for all remaining files

Possible values for the command are:
 `emacs'       The file will be visited by the current Emacs process.
 `default'     Use the default application for this file type.
 string        A command to be executed by a shell; %s will be replaced
	       by the path to the file.
 sexp          A Lisp form which will be evaluated.  The file path will
	       be available in the Lisp variable `file'.
For more examples, see the system specific constants
`org-file-apps-defaults-macosx'
`org-file-apps-defaults-windowsnt'
`org-file-apps-defaults-gnu'."
  :group 'org-link-follow
  :type '(repeat
	  (cons (choice :value ""
			(string :tag "Extension")
			(const :tag "Default for unrecognized files" t)
			(const :tag "Remote file" remote)
			(const :tag "Links to a directory" directory))
		(choice :value ""
			(const :tag "Visit with Emacs" emacs)
			(const :tag "Use system default" default)
			(string :tag "Command")
			(sexp :tag "Lisp form")))))

(defcustom org-mhe-search-all-folders nil
  "Non-nil means, that the search for the mh-message will be extended to
all folders if the message cannot be found in the folder given in the link.
Searching all folders is very efficient with one of the search engines
supported by MH-E, but will be slow with pick."
  :group 'org-link-follow
  :type 'boolean)

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
the value of `remember-data-file'.
You can set this on a per-template basis with the variable
`org-remember-templates'."
  :group 'org-remember
  :type '(choice
	  (const :tag "Default from remember-data-file" nil)
	  file))

(defcustom org-remember-store-without-prompt nil
  "Non-nil means, `C-c C-c' stores remember note without further promts.
In this case, you need `C-u C-c C-c' to get the prompts for
note file and headline.
When this variable is nil, `C-c C-c' give you the prompts, and
`C-u C-c C-c' trigger the fasttrack."
  :group 'org-remember
  :type 'boolean)
  
(defcustom org-remember-default-headline ""
  "The headline that should be the default location in the notes file.
When filing remember notes, the cursor will start at that position.
You can set this on a per-template basis with the variable
`org-remember-templates'."
  :group 'org-remember
  :type 'string)

(defcustom org-remember-templates nil
  "Templates for the creation of remember buffers.
When nil, just let remember make the buffer.
When not nil, this is a list of 4-element lists.  In each entry, the first
element is a character, a unique key to select this template.
The second element is the template.  The third element is optional and can
specify a destination file for remember items created with this template.
The default file is given by `org-default-notes-file'.  An optional forth
element can specify the headline in that file that should be offered
first when the user is asked to file the entry.  The default headline is
given in the variable `org-remember-default-headline'.

The template specifies the structure of the remember buffer.  It should have
a first line starting with a star, to act as the org-mode headline.
Furthermore, the following %-escapes will be replaced with content:

  %^{prompt}  prompt the user for a string and replace this sequence with it.
  %t          time stamp, date only
  %T          time stamp with date and time
  %u, %U      like the above, but inactive time stamps
  %^t         like %t, but prompt for date.  Similarly %^T, %^u, %^U
              You may define a prompt like %^{Please specify birthday}t
  %n          user name (taken from `user-full-name')
  %a          annotation, normally the link created with org-store-link
  %i          initial content, the region when remember is called with C-u.
              If %i is indented, the entire inserted text will be indented
              as well.

  %?          After completing the template, position cursor here.

Apart from these general escapes, you can access information specific to the
link type that is created.  For example, calling `remember' in emails or gnus
will record the author and the subject of the message, which you can access
with %:author and %:subject, respectively.  Here is a complete list of what
is recorded for each link type.

Link type          |  Available information
-------------------+------------------------------------------------------
bbdb               |  %:type %:name %:company
vm, wl, mh, rmail  |  %:type %:subject %:message-id
                   |  %:from %:fromname %:fromaddress
                   |  %:to   %:toname   %:toaddress
                   |  %:fromto (either \"to NAME\" or \"from NAME\")
gnus               |  %:group, for messages also all email fields
w3, w3m            |  %:type %:url
info               |  %:type %:file %:node
calendar           |  %:type %:date"
  :group 'org-remember
  :get (lambda (var) ; Make sure all entries have 4 elements
	 (mapcar (lambda (x)
		   (cond ((= (length x) 3) (append x '("")))
			 ((= (length x) 2) (append x '("" "")))
			 (t x)))
		 (default-value var)))
  :type '(repeat
	  :tag "enabled"
	  (list :value (?a "\n" nil nil)
		(character :tag "Selection Key")
		(string :tag "Template")
		(file :tag "Destination file (optional)")
		(string :tag "Destination headline (optional)"))))

(defcustom org-reverse-note-order nil
  "Non-nil means, store new notes at the beginning of a file or entry.
When nil, new notes will be filed to the end of a file or entry."
  :group 'org-remember
  :type '(choice
	  (const :tag "Reverse always" t)
	  (const :tag "Reverse never" nil)
	  (repeat :tag "By file name regexp"
		  (cons regexp boolean))))

(defgroup org-todo nil
  "Options concerning TODO items in Org-mode."
  :tag "Org TODO"
  :group 'org)

(defgroup org-progress nil
  "Options concerning Progress logging in Org-mode."
  :tag "Org Progress"
  :group 'org-time)

(defcustom org-todo-keywords '((sequence "TODO" "DONE"))
  "List of TODO entry keyword sequences and their interpretation.
\\<org-mode-map>This is a list of sequences.

Each sequence starts with a symbol, either `sequence' or `type',
indicating if the keywords should be interpreted as a sequence of
action steps, or as different types of TODO items.  The first
keywords are states requiring action - these states will select a headline
for inclusion into the global TODO list Org-mode produces.  If one of
the \"keywords\" is the vertical bat \"|\" the remaining keywords
signify that no further action is necessary.  If \"|\" is not found,
the last keyword is treated as the only DONE state of the sequence.

The command \\[org-todo] cycles an entry through these states, and one
additional state where no keyword is present.  For details about this
cycling, see the manual.

TODO keywords and interpretation can also be set on a per-file basis with
the special #+SEQ_TODO and #+TYP_TODO lines.

For backward compatibility, this variable may also be just a list
of keywords - in this case the interptetation (sequence or type) will be
taken from the (otherwise obsolete) variable `org-todo-interpretation'."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice
	  (repeat :tag "Old syntax, just keywords"
		  (string :tag "Keyword"))
	  (repeat :tag "New syntax"
		  (cons
		   (choice
		    :tag "Interpretation"
		    (const :tag "Sequence (cycling hits every state)" sequence)
		    (const :tag "Type     (cycling directly to DONE)" type))
		   (repeat
		    (string :tag "Keyword"))))))

(defvar org-todo-keywords-1 nil)
(make-variable-buffer-local 'org-todo-keywords-1)
(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-not-done-keywords nil)
(make-variable-buffer-local 'org-not-done-keywords)
(defvar org-done-keywords nil)
(make-variable-buffer-local 'org-done-keywords)
(defvar org-todo-heads nil)
(make-variable-buffer-local 'org-todo-heads)
(defvar org-todo-sets nil)
(make-variable-buffer-local 'org-todo-sets)
(defvar org-todo-kwd-alist nil)
(make-variable-buffer-local 'org-todo-kwd-alist)
(defvar org-todo-key-alist nil)
(make-variable-buffer-local 'org-todo-key-alist)
(defvar org-todo-key-trigger nil)
(make-variable-buffer-local 'org-todo-key-trigger)

(defcustom org-todo-interpretation 'sequence
  "Controls how TODO keywords are interpreted.
This variable is in principle obsolete and is only used for
backward compatibility, if the interpretation of todo keywords is
not given already in `org-todo-keywords'.  See that variable for
more information."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice (const sequence)
		 (const type)))

(defcustom org-use-fast-todo-selection 'prefix
  "Non-nil means, use the fast todo selection scheme with C-c C-t.
This variable describes if and under what circumstances the cycling
mechanism for TODO keywords will be replaced by a single-key, direct
selection scheme.

When nil, fast selection is never used.

When the symbol `prefix', it will be used when `org-todo' is called with
a prefix argument,  i.e. `C-u C-c C-t' in an Org-mode buffer, and `C-u t'
in an agenda buffer.

When t, fast selection is used by default.  In this case, the prefix
argument forces cycling instead.

In all cases, the special interface is only used if access keys have actually
been assigned by the user, i.e. if keywords in the configuration are followed
by a letter in parenthesis, like TODO(t)."
  :group 'org-todo
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "By default" t)
	  (const :tag "Only with C-u C-c C-t" prefix)))

(defcustom org-after-todo-state-change-hook nil
  "Hook which is run after the state of a TODO item was changed.
The new state (a string with a TODO keyword, or nil) is available in the
Lisp variable `state'."
  :group 'org-todo
  :type 'hook)

(defcustom org-log-done nil
  "When set, insert a (non-active) time stamp when TODO entry is marked DONE.
When the state of an entry is changed from nothing or a DONE state to
a not-done TODO state, remove a previous closing date.

This can also be a list of symbols indicating under which conditions
the time stamp recording the action should be annotated with a short note.
Valid members of this list are

  done       Offer to record a note when marking entries done
  state      Offer to record a note whenever changing the TODO state
             of an item.  This is only relevant if TODO keywords are
             interpreted as sequence, see variable `org-todo-interpretation'.
             When `state' is set, this includes tracking `done'.
  clock-out  Offer to record a note when clocking out of an item.

A separate window will then pop up and allow you to type a note.
After finishing with C-c C-c, the note will be added directly after the
timestamp, as a plain list item.  See also the variable
`org-log-note-headings'.

Logging can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: logdone
   #+STARTUP: nologging
   #+STARTUP: lognotedone
   #+STARTUP: lognotestate
   #+STARTUP: lognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "on" t)
	  (set :tag "on, with notes, detailed control" :greedy t :value (done)
	       (const :tag "when item is marked DONE" done)
	       (const :tag "when TODO state changes" state)
	       (const :tag "when clocking out" clock-out))))

(defcustom org-log-done-with-time t
  "Non-nil means, the CLOSED time stamp will contain date and time.
When nil, only the date will be recorded."
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-note-headings
  '((done . "CLOSING NOTE %t")
    (state . "State %-12s %t")
    (clock-out . ""))
  "Headings for notes added when clocking out or closing TODO items.
The value is an alist, with the car being a symbol indicating the note
context, and the cdr is the heading to be used.  The heading may also be the
empty string.
%t in the heading will be replaced by a time stamp.
%s will be replaced by the new TODO state, in double quotes.
%u will be replaced by the user name.
%U will be replaced by the full user name."
  :group  'org-todo
  :group  'org-progress
  :type '(list :greedy t
	  (cons (const :tag "Heading when closing an item" done) string)
	  (cons (const :tag
		       "Heading when changing todo state (todo sequence only)"
		       state) string)
	  (cons (const :tag "Heading when clocking out" clock-out) string)))

(defcustom org-log-states-order-reversed t
  "Non-nil means, the latest state change note will be directly after heading.
When nil, the notes will be orderer according to time."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-repeat t
  "Non-nil means, prompt for a note when REPEAT is resetting a TODO entry.
When nil, no note will be taken."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-clock-out-when-done t
  "When t, the clock will be stopped when the relevant entry is marked DONE.
Nil means, clock will keep running until stopped explicitly with
`C-c C-x C-o', or until the clock is started in a different item."
  :group 'org-progress
  :type 'boolean)

(defgroup org-priorities nil
  "Priorities in Org-mode."
  :tag "Org Priorities"
  :group 'org-todo)

(defcustom org-highest-priority ?A
  "The highest priority of TODO items.  A character like ?A, ?B etc.
Must have a smaller ASCII number than `org-lowest-priority'."
  :group 'org-priorities
  :type 'character)

(defcustom org-lowest-priority ?C
  "The lowest priority of TODO items.  A character like ?A, ?B etc.
Must have a larger ASCII number than `org-highest-priority'."
  :group 'org-priorities
  :type 'character)

(defcustom org-default-priority ?B
  "The default priority of TODO items.
This is the priority an item get if no explicit priority is given."
  :group 'org-priorities
  :type 'character)

(defgroup org-time nil
  "Options concerning time stamps and deadlines in Org-mode."
  :tag "Org Time"
  :group 'org)

(defcustom org-insert-labeled-timestamps-at-point nil
  "Non-nil means, SCHEDULED and DEADLINE timestamps are inserted at point.
When nil, these labeled time stamps are forces into the second line of an
entry, just after the headline.  When scheduling from the global TODO list,
the time stamp will always be forced into the second line."
  :group 'org-time
  :type 'boolean)

(defcustom org-insert-labeled-timestamps-before-properties-drawer t
  "Non-nil means, always insert planning info before property drawer.
When this is nil and there is a property drawer *directly* after
the headline, move the planning info into the drawer.  If the property
drawer separated from the headline by at least one line, this variable
has no effect."
  :group 'org-time
  :type 'boolean)

(defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
  "Formats for `format-time-string' which are used for time stamps.
It is not recommended to change this constant.")

(defcustom org-time-stamp-rounding-minutes 0
  "Number of minutes to round time stamps to upon insertion.
When zero, insert the time unmodified.  Useful rounding numbers
should be factors of 60, so for example 5, 10, 15.
When this is not zero, you can still force an exact time-stamp by using
a double prefix argument to a time-stamp command like `C-c .' or `C-c !'."
  :group 'org-time
  :type 'integer)

(defcustom org-display-custom-times nil
  "Non-nil means, overlay custom formats over all time stamps.
The formats are defined through the variable `org-time-stamp-custom-formats'.
To turn this on on a per-file basis, insert anywhere in the file:
   #+STARTUP: customtime"
  :group 'org-time
  :set 'set-default
  :type 'sexp)
(make-variable-buffer-local 'org-display-custom-times)

(defcustom org-time-stamp-custom-formats
  '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>") ; american
  "Custom formats for time stamps.  See `format-time-string' for the syntax.
These are overlayed over the default ISO format if the variable
`org-display-custom-times' is set.  Time like %H:%M should be at the
end of the second format."
  :group 'org-time
  :type 'sexp)

(defun org-time-stamp-format (&optional long inactive)
  "Get the right format for a time string."
  (let ((f (if long (cdr org-time-stamp-formats)
	     (car org-time-stamp-formats))))
    (if inactive
	(concat "[" (substring f 1 -1) "]")
      f)))

(defcustom org-deadline-warning-days 14
  "No. of days before expiration during which a deadline becomes active.
This variable governs the display in sparse trees and in the agenda.
When negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified."
  :group 'org-time
  :type 'number)

(defcustom org-popup-calendar-for-date-prompt t
  "Non-nil means, pop up a calendar when prompting for a date.
In the calendar, the date can be selected with mouse-1.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)

(defcustom org-edit-timestamp-down-means-later nil
  "Non-nil means, S-down will increase the time in a time stamp.
When nil, S-up will increase."
  :group 'org-time
  :type 'boolean)

(defcustom org-calendar-follow-timestamp-change t
  "Non-nil means, make the calendar window follow timestamp changes.
When a timestamp is modified and the calendar window is visible, it will be
moved to the new date."
  :group 'org-time
  :type 'boolean)

(defgroup org-tags nil
  "Options concerning tags in Org-mode."
  :tag "Org Tags"
  :group 'org)

(defcustom org-tag-alist nil
  "List of tags allowed in Org-mode files.
When this list is nil, Org-mode will base TAG input on what is already in the
buffer.
The value of this variable is an alist, the car of each entry must be a
keyword as a string, the cdr may be a character that is used to select
that tag through the fast-tag-selection interface.
See the manual for details."
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons   (string    :tag "Tag name")
		   (character :tag "Access char"))
	   (const :tag "Start radio group" (:startgroup))
	   (const :tag "End radio group" (:endgroup)))))

(defcustom org-use-fast-tag-selection 'auto
  "Non-nil means, use fast tag selection scheme.
This is a special interface to select and deselect tags with single keys.
When nil, fast selection is never used.
When the symbol `auto', fast selection is used if and only if selection
characters for tags have been configured, either through the variable
`org-tag-alist' or through a #+TAGS line in the buffer.
When t, fast selection is always used and selection keys are assigned
automatically if necessary."
  :group 'org-tags
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When selection characters are configured" 'auto)))

(defcustom org-fast-tag-selection-single-key nil
  "Non-nil means, fast tag selection exits after first change.
When nil, you have to press RET to exit it.
During fast tag selection, you can toggle this flag with `C-c'.
This variable can also have the value `expert'.  In this case, the window
displaying the tags menu is not even shown, until you press C-c again."
  :group 'org-tags
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (const :tag "Expert" expert)))

(defcustom org-fast-tag-selection-include-todo nil
  "Non-nil means, fast tags selection interface will also offer TODO states."
  :group 'org-tags
  :group 'org-todo
  :type 'boolean)

(defcustom org-tags-column 48
  "The column to which tags should be indented in a headline.
If this number is positive, it specifies the column.  If it is negative,
it means that the tags should be flushright to that column.  For example,
-79 works well for a normal 80 character screen."
  :group 'org-tags
  :type 'integer)

(defcustom org-auto-align-tags t
  "Non-nil means, realign tags after pro/demotion of TODO state change.
These operations change the length of a headline and therefore shift
the tags around.  With this options turned on, after each such operation
the tags are again aligned to `org-tags-column'."
  :group 'org-tags
  :type 'boolean)

(defcustom org-use-tag-inheritance t
  "Non-nil means, tags in levels apply also for sublevels.
When nil, only the tags directly given in a specific line apply there.
If you turn off this option, you very likely want to turn on the
companion option `org-tags-match-list-sublevels'."
  :group 'org-tags
  :type 'boolean)

(defcustom org-tags-match-list-sublevels nil
  "Non-nil means list also sublevels of headlines matching tag search.
Because of tag inheritance (see variable `org-use-tag-inheritance'),
the sublevels of a headline matching a tag search often also match
the same search.  Listing all of them can create very long lists.
Setting this variable to nil causes subtrees of a match to be skipped.
This option is off by default, because inheritance in on.  If you turn
inheritance off, you very likely want to turn this option on.

As a special case, if the tag search is restricted to TODO items, the
value of this variable is ignored and sublevels are always checked, to
make sure all corresponding TODO items find their way into the list."
  :group 'org-tags
  :type 'boolean)

(defvar org-tags-history nil
  "History of minibuffer reads for tags.")
(defvar org-last-tags-completion-table nil
  "The last used completion table for tags.")

(defgroup org-properties nil
  "Options concerning properties in Org-mode."
  :tag "Org Properties"
  :group 'org)

(defcustom org-property-format "%-10s %s"
  "How property key/value pairs should be formatted by `indent-line'.
When `indent-line' hits a property definition, it will format the line
according to this format, mainly to make sure that the values are
lined-up with respect to each other."
  :group 'org-properties
  :type 'string)

(defcustom org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
  "The default column format, if no other format has been defined.
This variable can be set on the per-file basis by inserting a line

#+COLUMNS: %25ITEM ....."
  :group 'org-properties
  :type 'string)

(defcustom org-global-properties nil
  "List of property/value pairs that can be inherited by any entry.
You can set buffer-local values for this by adding lines like

#+PROPERTY: NAME VALUE"
  :group 'org-properties
  :type '(repeat
	  (cons (string :tag "Property")
		(string :tag "Value"))))

(defvar org-local-properties nil
  "List of property/value pairs that can be inherited by any entry.
Valid for the current buffer.
This variable is populated from #+PROPERTY lines.")

(defgroup org-agenda nil
  "Options concerning agenda views in Org-mode."
  :tag "Org Agenda"
  :group 'org)

(defvar org-category nil
  "Variable used by org files to set a category for agenda display.
Such files should use a file variable to set it, for example

#   -*- mode: org; org-category: \"ELisp\"

or contain a special line

#+CATEGORY: ELisp

If the file does not specify a category, then file's base name
is used instead.")
(make-variable-buffer-local 'org-category)

(defcustom org-agenda-files nil
  "The files to be used for agenda display.
Entries may be added to this list with \\[org-agenda-file-to-front] and removed with
\\[org-remove-file].  You can also use customize to edit the list.

If the value of the variable is not a list but a single file name, then
the list of agenda files is actually stored and maintained in that file, one
agenda file per line."
  :group 'org-agenda
  :type '(choice
	  (repeat :tag "List of files" file)
	  (file :tag "Store list in a file\n" :value "~/.agenda_files")))

(defcustom org-agenda-skip-unavailable-files nil
  "t means to just skip non-reachable files in `org-agenda-files'.
Nil means to remove them, after a query, from the list."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-confirm-kill 1
  "When set, remote killing from the agenda buffer needs confirmation.
When t, a confirmation is always needed.  When a number N, confirmation is
only needed when the text to be killed contains more than N non-white lines."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (number :tag "When more than N lines")))

(defcustom org-calendar-to-agenda-key [?c]
  "The key to be installed in `calendar-mode-map' for switching to the agenda.
The command `org-calendar-goto-agenda' will be bound to this key.  The
default is the character `c' because then `c' can be used to switch back and
forth between agenda and calendar."
  :group 'org-agenda
  :type 'sexp)

(defgroup org-agenda-export nil
 "Options concerning exporting agenda views in Org-mode."
 :tag "Org Agenda Export"
 :group 'org-agenda)

(defcustom org-agenda-with-colors t
  "Non-nil means, use colors in agenda views."
  :group 'org-agenda-export
  :type 'boolean)

(defcustom org-agenda-exporter-settings nil
  "Alist of variable/value pairs that should be active during agenda export.
This is a good place to set uptions for ps-print and for htmlize."
  :group 'org-agenda-export
  :type '(repeat
	  (list
	   (variable)
	   (sexp :tag "Value"))))

(defcustom org-agenda-export-html-style ""
  "The style specification for exported HTML Agenda files.
If this variable contains a string, it will replace the default <style>
section as produced by `htmlize'.
Since there are different ways of setting style information, this variable
needs to contain the full HTML structure to provide a style, including the
surrounding HTML tags.  The style specifications should include definitions
the fonts used by the agenda, here is an example:

   <style type=\"text/css\">
       p { font-weight: normal; color: gray; }
       .org-agenda-structure {
          font-size: 110%;
          color: #003399;
          font-weight: 600;
       }
       .org-todo {
          color: #cc6666;Week-agenda:
          font-weight: bold;
       }
       .org-done {
          color: #339933;
       }
       .title { text-align: center; }
       .todo, .deadline { color: red; }
       .done { color: green; }
    </style>

or, if you want to keep the style in a file,

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to also add other text to the header.  However,
<style>...</style> is required, if not present the variable will be ignored."
  :group 'org-agenda-export
  :group 'org-export-html
  :type 'string)

(defgroup org-agenda-custom-commands nil
 "Options concerning agenda views in Org-mode."
 :tag "Org Agenda Custom Commands"
 :group 'org-agenda)

(defcustom org-agenda-custom-commands nil
  "Custom commands for the agenda.
These commands will be offered on the splash screen displayed by the
agenda dispatcher \\[org-agenda].  Each entry is a list like this:

   (key type match options files)

key     The key (a single char as a string) to be associated with the command.
type    The command type, any of the following symbols:
         todo        Entries with a specific TODO keyword, in all agenda files.
         tags        Tags match in all agenda files.
         tags-todo   Tags match in all agenda files, TODO entries only.
         todo-tree   Sparse tree of specific TODO keyword in *current* file.
         tags-tree   Sparse tree with all tags matches in *current* file.
         occur-tree  Occur sparse tree for *current* file.
match   What to search for:
         - a single keyword for TODO keyword searches
         - a tags match expression for tags searches
         - a regular expression for occur searches
options  A list of option settings, similar to that in a let form, so like
         this: ((opt1 val1) (opt2 val2) ...)
files    A list of files file to write the produced agenda buffer to
         with the command `org-store-agenda-views'.
         If a file name ends in \".html\", an HTML version of the buffer
         is written out.  If it ends in \".ps\", a postscript version is
         produced.  Otherwide, only the plain text is written to the file.

You can also define a set of commands, to create a composite agenda buffer.
In this case, an entry looks like this:

  (key desc (cmd1 cmd2 ...) general-options file)

where

desc   A description string to be displayed in the dispatcher menu.
cmd    An agenda command, similar to the above.  However, tree commands
       are no allowed, but instead you can get agenda and global todo list.
       So valid commands for a set are:
       (agenda)
       (alltodo)
       (stuck)
       (todo \"match\" options files)
       (tags \"match\" options files)
       (tags-todo \"match\" options files)

Each command can carry a list of options, and another set of options can be
given for the whole set of commands.  Individual command options take
precedence over the general options."
  :group 'org-agenda-custom-commands
  :type '(repeat
	  (choice :value ("a" tags "" nil)
	   (list :tag "Single command"
		 (string :tag "Key")
		 (choice
		  (const :tag "Agenda" agenda)
		  (const :tag "TODO list" alltodo)
		  (const :tag "Stuck projects" stuck)
		  (const :tag "Tags search (all agenda files)" tags)
		  (const :tag "Tags search of TODO entries (all agenda files)" tags-todo)
		  (const :tag "TODO keyword search (all agenda files)" todo)
		  (const :tag "Tags sparse tree (current buffer)" tags-tree)
		  (const :tag "TODO keyword tree (current buffer)" todo-tree)
		  (const :tag "Occur tree (current buffer)" occur-tree)
		  (symbol :tag "Other, user-defined function"))
		 (string :tag "Match")
		 (repeat :tag "Local options"
			 (list (variable :tag "Option") (sexp :tag "Value")))
		 (option (repeat :tag "Export" (file :tag "Export to"))))
	   (list :tag "Command series, all agenda files"
		 (string :tag "Key")
		 (string :tag "Description")
		 (repeat
		  (choice
		   (const :tag "Agenda" (agenda))
		   (const :tag "TODO list" (alltodo))
		   (const :tag "Stuck projects" (stuck))
		   (list :tag "Tags search"
			 (const :format "" tags)
			 (string :tag "Match")
			 (repeat :tag "Local options"
				 (list (variable :tag "Option")
				       (sexp :tag "Value"))))

		   (list :tag "Tags search, TODO entries only"
			 (const :format "" tags-todo)
			 (string :tag "Match")
			 (repeat :tag "Local options"
				 (list (variable :tag "Option")
				       (sexp :tag "Value"))))

		   (list :tag "TODO keyword search"
			 (const :format "" todo)
			 (string :tag "Match")
			 (repeat :tag "Local options"
				 (list (variable :tag "Option")
				       (sexp :tag "Value"))))

		   (list :tag "Other, user-defined function"
			 (symbol :tag "function")
			 (string :tag "Match")
			 (repeat :tag "Local options"
				 (list (variable :tag "Option")
				       (sexp :tag "Value"))))))

		 (repeat :tag "General options"
			 (list (variable :tag "Option")
			       (sexp :tag "Value")))
		 (option (repeat :tag "Export" (file :tag "Export to")))))))

(defcustom org-stuck-projects
  '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "")
  "How to identify stuck projects.
This is a list of four items:
1. A tags/todo matcher string that is used to identify a project.
   The entire tree below a headline matched by this is considered one project.
2. A list of TODO keywords identifying non-stuck projects.
   If the project subtree contains any headline with one of these todo
   keywords, the project is considered to be not stuck.  If you specify
   \"*\" as a keyword, any TODO keyword will mark the project unstuck.
3. A list of tags identifying non-stuck projects.
   If the project subtree contains any headline with one of these tags,
   the project is considered to be not stuck.  If you specify \"*\" as
   a tag, any tag will mark the project unstuck.
4. An arbitrary regular expression matching non-stuck projects.

After defining this variable, you may use \\[org-agenda-list-stuck-projects]
or `C-c a #' to produce the list."
  :group 'org-agenda-custom-commands
  :type '(list
	  (string :tag "Tags/TODO match to identify a project")
	  (repeat :tag "Projects are *not* stuck if they have an entry with TODO keyword any of" (string))
	  (repeat :tag "Projects are *not* stuck if they have an entry with TAG being any of" (string))
	  (regexp :tag "Projects are *not* stuck if this regexp matches\ninside the subtree")))


(defgroup org-agenda-skip nil
 "Options concerning skipping parts of agenda files."
 :tag "Org Agenda Skip"
 :group 'org-agenda)

(defcustom org-agenda-todo-list-sublevels t
  "Non-nil means, check also the sublevels of a TODO entry for TODO entries.
When nil, the sublevels of a TODO entry are not checked, resulting in
potentially much shorter TODO lists."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-todo-ignore-scheduled nil
  "Non-nil means, don't show scheduled entries in the global todo list.
The idea behind this is that by scheduling it, you have already taken care
of this item."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-todo-ignore-deadlines nil
  "Non-nil means, don't show near deadline entries in the global todo list.
Near means closer than `org-deadline-warning-days' days.
The idea behind this is that such items will appear in the agenda anyway."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-skip-scheduled-if-done nil
  "Non-nil means don't show scheduled items in agenda when they are done.
This is relevant for the daily/weekly agenda, not for the TODO list.  And
it applied only to the actualy date of the scheduling.  Warnings about
an item with a past scheduling dates are always turned off when the item
is DONE."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-skip-deadline-if-done nil
  "Non-nil means don't show deadines when the corresponding item is done.
When nil, the deadline is still shown and should give you a happy feeling.
This is relevant for the daily/weekly agenda.  And it applied only to the
actualy date of the deadline.  Warnings about approching and past-due
deadlines are always turned off when the item is DONE."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-timeline-show-empty-dates 3
  "Non-nil means, `org-timeline' also shows dates without an entry.
When nil, only the days which actually have entries are shown.
When t, all days between the first and the last date are shown.
When an integer, show also empty dates, but if there is a gap of more than
N days, just insert a special line indicating the size of the gap."
  :group 'org-agenda-skip
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "All" t)
	  (number :tag "at most")))


(defgroup org-agenda-startup nil
  "Options concerning initial settings in the Agenda in Org Mode."
  :tag "Org Agenda Startup"
  :group 'org-agenda)

(defcustom org-finalize-agenda-hook nil
  "Hook run just before displaying an agenda buffer."
  :group 'org-agenda-startup
  :type 'hook)

(defcustom org-agenda-mouse-1-follows-link nil
  "Non-nil means, mouse-1 on a link will follow the link in the agenda.
A longer mouse click will still set point.  Does not wortk on XEmacs.
Needs to be set before org.el is loaded."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-start-with-follow-mode nil
  "The initial value of follow-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :type 'boolean)

(defgroup org-agenda-windows nil
  "Options concerning the windows used by the Agenda in Org Mode."
  :tag "Org Agenda Windows"
  :group 'org-agenda)

(defcustom org-agenda-window-setup 'reorganize-frame
  "How the agenda buffer should be displayed.
Possible values for this option are:

current-window    Show agenda in the current window, keeping all other windows.
other-frame       Use `switch-to-buffer-other-frame' to display agenda.
other-window      Use `switch-to-buffer-other-window' to display agenda.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the agenda.
See also the variable `org-agenda-restore-windows-after-quit'."
  :group 'org-agenda-windows
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defcustom org-agenda-restore-windows-after-quit nil
  "Non-nil means, restore window configuration open exiting agenda.
Before the window configuration is changed for displaying the agenda,
the current status is recorded.  When the agenda is exited with
`q' or `x' and this option is set, the old state is restored.  If
`org-agenda-window-setup' is `other-frame', the value of this
option will be ignored.."
  :group 'org-agenda-windows
  :type 'boolean)

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?
This applies to indirect buffers created with the commands
\\[org-tree-to-indirect-buffer] and \\[org-agenda-tree-to-indirect-buffer].
Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and re-use it each time.
new-frame        Make a new frame each time."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defgroup org-agenda-daily/weekly nil
  "Options concerning the daily/weekly agenda."
  :tag "Org Agenda Daily/Weekly"
  :group 'org-agenda)

(defcustom org-agenda-ndays 7
  "Number of days to include in overview display.
Should be 1 or 7."
  :group 'org-agenda-daily/weekly
  :type 'number)

(defcustom org-agenda-start-on-weekday 1
  "Non-nil means, start the overview always on the specified weekday.
0 denotes Sunday, 1 denotes Monday etc.
When nil, always start on the current day."
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Today" nil)
		 (number :tag "Weekday No.")))

(defcustom org-agenda-show-all-dates t
  "Non-nil means, `org-agenda' shows every day in the selected range.
When nil, only the days which actually have entries are shown."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-date-format "%A %d %B %Y"
  "Format string for displaying dates in the agenda.
Used by the daily/weekly agenda and by the timeline.  This should be
a format string understood by `format-time-string'.
FIXME: Not used currently, because of timezone problem."
  :group 'org-agenda-daily/weekly
  :type 'string)

(defcustom org-agenda-include-diary nil
  "If non-nil, include in the agenda entries from the Emacs Calendar's diary."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-include-all-todo nil
  "Set  means weekly/daily agenda will always contain all TODO entries.
The TODO entries will be listed at the top of the agenda, before
the entries for specific days."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-repeating-timestamp-show-all t
  "Non-nil means, show all occurences of a repeating stamp in the agenda.
When nil, only one occurence is shown, either today or the
nearest into the future."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defgroup org-agenda-time-grid nil
  "Options concerning the time grid in the Org-mode Agenda."
  :tag "Org Agenda Time Grid"
  :group 'org-agenda)

(defcustom org-agenda-use-time-grid t
  "Non-nil means, show a time grid in the agenda schedule.
A time grid is a set of lines for specific times (like every two hours between
8:00 and 20:00).  The items scheduled for a day at specific times are
sorted in between these lines.
For details about when the grid will be shown, and what it will look like, see
the variable `org-agenda-time-grid'."
  :group 'org-agenda-time-grid
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
 require-timed show grid only if at least one item has a time specification

The second item is a string which will be places behing the grid time.

The third item is a list of integers, indicating the times that should have
a grid line."
  :group 'org-agenda-time-grid
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

(defgroup org-agenda-sorting nil
  "Options concerning sorting in the Org-mode Agenda."
  :tag "Org Agenda Sorting"
  :group 'org-agenda)

(let ((sorting-choice
       '(choice
	 (const time-up) (const time-down)
	 (const category-keep) (const category-up) (const category-down)
	 (const tag-down) (const tag-up)
	 (const priority-up) (const priority-down))))

  (defcustom org-agenda-sorting-strategy
    '((agenda time-up category-keep priority-down)
      (todo category-keep priority-down)
      (tags category-keep priority-down))
    "Sorting structure for the agenda items of a single day.
This is a list of symbols which will be used in sequence to determine
if an entry should be listed before another entry.  The following
symbols are recognized:

time-up         Put entries with time-of-day indications first, early first
time-down       Put entries with time-of-day indications first, late first
category-keep   Keep the default order of categories, corresponding to the
		sequence in `org-agenda-files'.
category-up     Sort alphabetically by category, A-Z.
category-down   Sort alphabetically by category, Z-A.
tag-up          Sort alphabetically by last tag, A-Z.
tag-down        Sort alphabetically by last tag, Z-A.
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
  :group 'org-agenda-sorting
  :type `(choice
	  (repeat :tag "General" ,sorting-choice)
	  (list :tag "Individually"
		(cons (const :tag "Strategy for Weekly/Daily agenda" agenda)
		      (repeat ,sorting-choice))
		(cons (const :tag "Strategy for TODO lists" todo)
		      (repeat ,sorting-choice))
		(cons (const :tag "Strategy for Tags matches" tags)
		      (repeat ,sorting-choice))))))

(defcustom org-sort-agenda-notime-is-late t
  "Non-nil means, items without time are considered late.
This is only relevant for sorting.  When t, items which have no explicit
time like 15:30 will be considered as 99:01, i.e. later than any items which
do have a time.  When nil, the default time is before 0:00.  You can use this
option to decide if the schedule for today should come before or after timeless
agenda entries."
  :group 'org-agenda-sorting
  :type 'boolean)

(defgroup org-agenda-prefix nil
  "Options concerning the entry prefix in the Org-mode agenda display."
  :tag "Org Agenda Prefix"
  :group 'org-agenda)

(defcustom org-agenda-prefix-format
  '((agenda  . "  %-12:c%?-12t% s")
    (timeline  . "  % s")
    (todo  . "  %-12:c")
    (tags  . "  %-12:c"))
  "Format specifications for the prefix of items in the agenda views.
An alist with four entries, for the different agenda types.  The keys to the
sublists are `agenda', `timeline', `todo', and `tags'.  The values
are format strings.
This format works similar to a printf format, with the following meaning:

  %c   the category of the item, \"Diary\" for entries from the diary, or
       as given by the CATEGORY keyword or derived from the file name.
  %T   the *last* tag of the item.  Last because inherited tags come
       first in the list.
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

See also the variables `org-agenda-remove-times-when-in-prefix' and
`org-agenda-remove-tags'."
  :type '(choice
	  (string :tag "General format")
	  (list :greedy t :tag "View dependent"
		(cons  (const agenda) (string :tag "Format"))
		(cons  (const timeline) (string :tag "Format"))
		(cons  (const todo) (string :tag "Format"))
		(cons  (const tags) (string :tag "Format"))))
  :group 'org-agenda-prefix)

(defvar org-prefix-format-compiled nil
  "The compiled version of the most recently used prefix format.
See the variable `org-agenda-prefix-format'.")

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
  :group 'org-agenda-prefix
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When at beginning of entry" beg)))


(defcustom org-agenda-default-appointment-duration nil
  "Default duration for appointments that only have a starting time.
When nil, no duration is specified in such cases.
When non-nil, this must be the number of minutes, e.g. 60 for one hour."
  :group 'org-agenda-prefix
  :type '(choice
	  (integer :tag "Minutes")
	  (const :tag "No default duration")))


(defcustom org-agenda-remove-tags nil
  "Non-nil means, remove the tags from the headline copy in the agenda.
When this is the symbol `prefix', only remove tags when
`org-agenda-prefix-format' contains a `%T' specifier."
  :group 'org-agenda-prefix
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When prefix format contains %T" prefix)))

(if (fboundp 'defvaralias)
    (defvaralias 'org-agenda-remove-tags-when-in-prefix
      'org-agenda-remove-tags))

(defcustom org-agenda-align-tags-to-column 65
  "Shift tags in agenda items to this column."
  :group 'org-agenda-prefix
  :type 'integer)

(defgroup org-latex nil
  "Options for embedding LaTeX code into Org-mode"
  :tag "Org LaTeX"
  :group 'org)

(defcustom org-format-latex-options
  '(:foreground default :background default :scale 1.0
    :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
    :matchers ("begin" "$" "$$" "\\(" "\\["))
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in emacs, e.g. \"Black\".
             `default' means use the forground of the default face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
:scale       a scaling factor for the size of the images
:html-foreground, :html-background, :html-scale
             The same numbers for HTML export.
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\"  find environments
             \"$\"      find math expressions surrounded by $...$
             \"$$\"     find math expressions surrounded by $$....$$
             \"\\(\"     find math expressions surrounded by \\(...\\)
             \"\\ [\"    find math expressions surrounded by \\ [...\\]"
  :group 'org-export-latex
  :type 'plist)

(defcustom org-format-latex-header "\\documentclass{article}
\\usepackage{fullpage}         % do not remove
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
\\pagestyle{empty}             % do not remove"
  "The document header used for processing LaTeX fragments."
  :group 'org-export-latex
  :type 'string)

(defgroup org-export nil
  "Options for exporting org-listings."
  :tag "Org Export"
  :group 'org)

(defgroup org-export-general nil
  "General options for exporting Org-mode files."
  :tag "Org Export General"
  :group 'org-export)

(defcustom org-export-publishing-directory "."
  "Path to the location where exported files should be located.
This path may be relative to the directory where the Org-mode file lives.
The default is to put them into the same directory as the Org-mode file.
The variable may also be an alist with export types `:html', `:ascii',
`:ical', `:LaTeX', or `:xoxo' and the corresponding directories.
If a directory path is relative, it is interpreted relative to the
directory where the exported Org-mode files lives."
  :group 'org-export-general
  :type '(choice
	  (directory)
	  (repeat
	   (cons
	    (choice :tag "Type"
		    (const :html) (const :LaTeX) 
		    (const :ascii) (const :ical) (const :xoxo))
	    (directory)))))

(defcustom org-export-language-setup
  '(("en"  "Author"          "Date"  "Table of Contents")
    ("cs"  "Autor"           "Datum" "Obsah")
    ("da"  "Ophavsmand"      "Dato"  "Indhold")
    ("de"  "Autor"           "Datum" "Inhaltsverzeichnis")
    ("es"  "Autor"           "Fecha" "\xcdndice")
    ("fr"  "Auteur"          "Date"  "Table des mati\xe8res")
    ("it"  "Autore"          "Data"  "Indice")
    ("nl"  "Auteur"          "Datum" "Inhoudsopgave")
    ("nn"  "Forfattar"       "Dato"  "Innhold")  ;; nn = Norsk (nynorsk)
    ("sv"  "F\xf6rfattarens" "Datum" "Inneh\xe5ll"))
  "Terms used in export text, translated to different languages.
Use the variable `org-export-default-language' to set the language,
or use the +OPTION lines for a per-file setting."
  :group 'org-export-general
  :type '(repeat
	  (list
	   (string :tag "HTML language tag")
	   (string :tag "Author")
	   (string :tag "Date")
	   (string :tag "Table of Contents"))))

(defcustom org-export-default-language "en"
  "The default language of HTML export, as a string.
This should have an association in `org-export-language-setup'."
  :group 'org-export-general
  :type 'string)

(defcustom org-export-skip-text-before-1st-heading t
  "Non-nil means, skip all text before the first headline when exporting.
When nil, that text is exported as well."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.
Inferior levels will produce itemize lists when exported.
Note that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the +OPTIONS line, e.g. \"H:2\"."
  :group 'org-export-general
  :type 'number)

(defcustom org-export-with-section-numbers t
  "Non-nil means, add section numbers to headlines when exporting.

This option can also be set with the +OPTIONS line, e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-toc t
  "Non-nil means, create a table of contents in exported files.
The TOC contains headlines with levels up to`org-export-headline-levels'.
When an integer, include levels up to N in the toc, this may then be
different from `org-export-headline-levels', but it will not be allowed
to be larger than the number of headline levels.
When nil, no table of contents is made.

Headlines which contain any TODO items will be marked with \"(*)\" in
ASCII export, and with red color in HTML output, if the option
`org-export-mark-todo-in-toc' is set.

In HTML output, the TOC will be clickable.

This option can also be set with the +OPTIONS line, e.g. \"toc:nil\"
or \"toc:3\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No Table of Contents" nil)
	  (const :tag "Full Table of Contents" t)
	  (integer :tag "TOC to level")))

(defcustom org-export-mark-todo-in-toc nil
  "Non-nil means, mark TOC lines that contain any open TODO items."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-preserve-breaks nil
  "Non-nil means, preserve all line breaks when exporting.
Normally, in HTML output paragraphs will be reformatted.  In ASCII
export, line breaks will always be preserved, regardless of this variable.

This option can also be set with the +OPTIONS line, e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-archived-trees 'headline
  "Whether subtrees with the ARCHIVE tag should be exported.
This can have three different values
nil       Do not export, pretend this tree is not present
t         Do export the entire tree
headline  Only export the headline, but skip the tree below it."
  :group 'org-export-general
  :group 'org-archive
  :type '(choice
	  (const :tag "not at all" nil)
	  (const :tag "headline only" 'headline)
	  (const :tag "entirely" t)))

(defcustom org-export-author-info t
  "Non-nil means, insert author name and email into the exported file.

This option can also be set with the +OPTIONS line,
e.g. \"author-info:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-time-stamp-file t
  "Non-nil means, insert a time stamp into the exported file.
The time stamp shows when the file was created.

This option can also be set with the +OPTIONS line,
e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-timestamps t
  "If nil, do not export time stamps and associated keywords."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-remove-timestamps-from-toc t
  "If nil, remove timestamps from the table of contents entries."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-tags 'not-in-toc
  "If nil, do not export tags, just remove them from headlines.
If this is the symbol `not-in-toc', tags will be removed from table of
contents entries, but still be shown in the headlines of the document."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Off" nil)
	  (const :tag "Not in TOC" not-in-toc)
	  (const :tag "On" t)))

(defcustom org-export-with-property-drawer nil
  "Non-nil means, export property drawers.
When nil, these drawers are removed before export.

This option can also be set with the +OPTIONS line, e.g. \"p:t\"."
  :group 'org-export-general
  :type 'boolean)

(defgroup org-export-translation nil
  "Options for translating special ascii sequences for the export backends."
  :tag "Org Export Translation"
  :group 'org-export)

(defcustom org-export-with-emphasize t
  "Non-nil means, interpret *word*, /word/, and _word_ as emphasized text.
If the export target supports emphasizing text, the word will be
typeset in bold, italic, or underlined, respectively.  Works only for
single words, but you can say: I *really* *mean* *this*.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-footnotes t
  "If nil, export [1] as a footnote marker.
Lines starting with [1] will be formatted as footnotes.

This option can also be set with the +OPTIONS line, e.g. \"f:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-sub-superscripts t
  "Non-nil means, interpret \"_\" and \"^\" for export.
When this option is turned on, you can use TeX-like syntax for sub- and
superscripts.  Several characters after \"_\" or \"^\" will be
considered as a single item - so grouping with {} is normally not
needed.  For example, the following things will be parsed as single
sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose the
sub/superscript.  If you set this variable to the symbol `{}',
the braces are *required* in order to trigger interpretations as
sub/superscript.  This can be helpful in documents that need \"_\"
frequently in plain text.

Not all export backends support this, but HTML does.

This option can also be set with the +OPTIONS line, e.g. \"^:nil\"."
  :group 'org-export-translation
  :type '(choice
	  (const :tag "Always interpret" t)
	  (const :tag "Only with braces" {})
	  (const :tag "Never interpret" nil)))

(defcustom org-export-with-TeX-macros t
  "Non-nil means, interpret simple TeX-like macros when exporting.
For example, HTML export converts \\alpha to &alpha; and \\AA to &Aring;.
No only real TeX macros will work here, but the standard HTML entities
for math can be used as macro names as well.  For a list of supported
names in HTML export, see the constant `org-html-entities'.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"TeX:nil\"."
  :group 'org-export-translation
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-with-LaTeX-fragments nil
  "Non-nil means, convert LaTeX fragments to images when exporting to HTML.
When set, the exporter will find LaTeX environments if the \\begin line is
the first non-white thing on a line.  It will also find the math delimiters
like $a=b$ and \\( a=b \\) for inline math,  $$a=b$$ and \\[ a=b \\] for
display math.

This option can also be set with the +OPTIONS line, e.g. \"LaTeX:t\"."
  :group 'org-export-translation
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-with-fixed-width t
  "Non-nil means, lines starting with \":\" will be in fixed width font.
This can be used to have pre-formatted text, fragments of code etc.  For
example:
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  See also the QUOTE keyword.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"::nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.
This has to be set before loading org.el to be effective."
  :group 'org-export-translation
  :type 'integer)

(defgroup org-export-tables nil
  "Options for exporting tables in Org-mode."
  :tag "Org Export Tables"
  :group 'org-export)

(defcustom org-export-with-tables t
  "If non-nil, lines starting with \"|\" define a table.
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------|
  | Arthur Dent | England  | 29.2.2100 |

Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-highlight-first-table-line t
  "Non-nil means, highlight the first table line.
In HTML export, this means use <th> instead of <td>.
In tables created with table.el, this applies to the first table line.
In Org-mode tables, all lines before the first horizontal separator
line will be formatted with <th> tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-table-remove-special-lines t
  "Remove special lines and marking characters in calculating tables.
This removes the special marking character column from tables that are set
up for spreadsheet calculations.  It also removes the entire lines
marked with `!', `_', or `^'.  The lines with `$' are kept, because
the values of constants may be useful to have."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-prefer-native-exporter-for-tables nil
  "Non-nil means, always export tables created with table.el natively.
Natively means, use the HTML code generator in table.el.
When nil, Org-mode's own HTML generator is used when possible (i.e. if
the table does not use row- or column-spanning).  This has the
advantage, that the automatic HTML conversions for math symbols and
sub/superscripts can be applied.  Org-mode's HTML generator is also
much faster."
  :group 'org-export-tables
  :type 'boolean)

(defgroup org-export-ascii nil
  "Options specific for ASCII export of Org-mode files."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-export-ascii-underline '(?\$ ?\# ?^ ?\~ ?\= ?\-)
  "Characters for underlining headings in ASCII export.
In the given sequence, these characters will be used for level 1, 2, ..."
  :group 'org-export-ascii
  :type '(repeat character))

(defcustom org-export-ascii-bullets '(?* ?+ ?-)
  "Bullet characters for headlines converted to lists in ASCII export.
The first character is is used for the first lest level generated in this
way, and so on.  If there are more levels than characters given here,
the list will be repeated.
Note that plain lists will keep the same bullets as the have in the
Org-mode file."
  :group 'org-export-ascii
  :type '(repeat character))

(defgroup org-export-xml nil
  "Options specific for XML export of Org-mode files."
  :tag "Org Export XML"
  :group 'org-export)

(defgroup org-export-html nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-export-html-coding-system nil
  ""
  :group 'org-export-html
  :type 'coding-system)

(defcustom org-export-html-style
"<style type=\"text/css\">
  html {
	font-family: Times, serif;
	font-size: 12pt;
  }
  .title { text-align: center; }
  .todo  { color: red; }
  .done { color: green; }
  .timestamp { color: grey }
  .timestamp-kwd { color: CadetBlue }
  .tag { background-color:lightblue; font-weight:normal }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
  }
  table { border-collapse: collapse; }
  td, th {
	vertical-align: top;
	<!--border: 1pt solid #ADB9CC;-->
  }
</style>"
  "The default style specification for exported HTML files.
Since there are different ways of setting style information, this variable
needs to contain the full HTML structure to provide a style, including the
surrounding HTML tags.  The style specifications should include definitions
for new classes todo, done, title, and deadline.  For example, legal values
would be:

   <style type=\"text/css\">
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .deadline { color: red; }
      .done { color: green; }
   </style>

or, if you want to keep the style in a file,

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header."
  :group 'org-export-html
  :type 'string)


(defcustom org-export-html-title-format "<h1 class=\"title\">%s</h1>\n"
  "Format for typesetting the document title in HTML export."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-link-org-files-as-html t
  "Non-nil means, make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-inline-images 'maybe
  "Non-nil means, inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

;; FIXME: rename
(defcustom org-export-html-expand t
  "Non-nil means, for HTML export, treat @<...> as HTML tag.
When nil, these tags will be exported as plain text and therefore
not be interpreted by a browser.

This option can also be set with the +OPTIONS line, e.g. \"@:nil\"."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-table-header-tags '("<th>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-html-with-timestamp nil
  "If non-nil, write `org-export-html-html-helper-timestamp'
into the exported HTML text.  Otherwise, the buffer will just be saved
to a file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-html-helper-timestamp
  "<br/><br/><hr><p><!-- hhmts start --> <!-- hhmts end --></p>\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode."
  :group 'org-export-html
  :type 'string)

(defgroup org-export-icalendar nil
  "Options specific for iCalendar export of Org-mode files."
  :tag "Org Export iCalendar"
  :group 'org-export)

(defcustom org-combined-agenda-icalendar-file "~/org.ics"
  "The file name for the iCalendar file covering all agenda files.
This file is created with the command \\[org-export-icalendar-all-agenda-files].
The file name should be absolute."
  :group 'org-export-icalendar
  :type 'file)

(defcustom org-icalendar-include-todo nil
  "Non-nil means, export to iCalendar files should also cover TODO items."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "Unfinished" t)
	  (const :tag "All" all)))

(defcustom org-icalendar-include-sexps t
  "Non-nil means, export to iCalendar files should also cover sexp entries.
These are entries like in the diary, but directly in an Org-mode file."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-combined-name "OrgMode"
  "Calendar name for the combined iCalendar representing all agenda files."
  :group 'org-export-icalendar
  :type 'string)

(defgroup org-font-lock nil
  "Font-lock settings for highlighting in Org-mode."
  :tag "Org Font Lock"
  :group 'org)

(defcustom org-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified."
  :group 'org-font-lock
  :type 'boolean)

(defcustom org-hide-leading-stars nil
  "Non-nil means, hide the first N-1 stars in a headline.
This works by using the face `org-hide' for these stars.  This
face is white for a light background, and black for a dark
background.  You may have to customize the face `org-hide' to
make this work.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: hidestars
   #+STARTUP: showstars"
  :group 'org-font-lock
  :type 'boolean)

(defcustom org-fontify-done-headline nil
  "Non-nil means, change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-font-lock
  :type 'boolean)

(defcustom org-fontify-emphasized-text t
  "Non-nil means fontify *bold*, /italic/ and _underlined_ text.
Changing this variable requires a restart of Emacs to take effect."
  :group 'org-font-lock
  :type 'boolean)

(defvar org-emph-re nil
  "Regular expression for matching emphasis.")
(defvar org-emphasis-regexp-components) ; defined just below
(defvar org-emphasis-alist) ; defined just below
(defun org-set-emph-re (var val)
  "Set variable and compute the emphasis regular expression."
  (set var val)
  (when (and (boundp 'org-emphasis-alist)
	     (boundp 'org-emphasis-regexp-components)
	     org-emphasis-alist org-emphasis-regexp-components)
    (let* ((e org-emphasis-regexp-components)
	   (pre (car e))
	   (post (nth 1 e))
	   (border (nth 2 e))
	   (body (nth 3 e))
	   (nl (nth 4 e))
	   (stacked (nth 5 e))
	   (body1 (concat body "*?"))
	   (markers (mapconcat 'car org-emphasis-alist "")))
      ;; make sure special characters appear at the right position in the class
      (if (string-match "\\^" markers)
	  (setq markers (concat (replace-match "" t t markers) "^")))
      (if (string-match "-" markers)
	  (setq markers (concat (replace-match "" t t markers) "-")))
      (if (> nl 0)
          (setq body1 (concat body1 "\\(?:\n" body "*?\\)\\{0,"
                              (int-to-string nl) "\\}")))
      ;; Make the regexp
      (setq org-emph-re
	    (concat "\\([" pre (if stacked markers) "]\\|^\\)"
		    "\\("
		    "\\([" markers "]\\)"
		    "\\("
		    "[^" border (if (and nil stacked) markers) "]"
		    body1
		    "[^" border (if (and nil stacked) markers) "]"
		    "\\)"
		    "\\3\\)"
		    "\\([" post (if stacked markers) "]\\|$\\)")))))

(defcustom org-emphasis-regexp-components
  '(" \t('\"" "- \t.,:?;'\")" " \t\r\n,\"'" "." 1 nil)
  "Components used to build the reqular expression for emphasis.
This is a list with 6 entries.  Terminology:  In an emphasis string
like \" *strong word* \", we call the initial space PREMATCH, the final
space POSTMATCH, the stars MARKERS, \"s\" and \"d\" are BORDER characters
and \"trong wor\" is the body.  The different components in this variable
specify what is allowed/forbidden in each part:

pre          Chars allowed as prematch.  Beginning of line will be allowed too.
post         Chars allowed as postmatch.  End of line will be allowed too.
border       The chars *forbidden* as border characters.
body-regexp  A regexp like \".\" to match a body character.  Don't use
             non-shy groups here, and don't allow newline here.
newline      The maximum number of newlines allowed in an emphasis exp.
stacked      Non-nil means, allow stacked styles.  This works only in HTML
             export.  When this is set, all marker characters (as given in
             `org-emphasis-alist') will be allowed as pre/post, aiding
             inside-out matching.
Use customize to modify this, or restart Emacs after changing it."
  :group 'org-font-lock
  :set 'org-set-emph-re
  :type '(list
	  (sexp    :tag "Allowed chars in pre      ")
	  (sexp    :tag "Allowed chars in post     ")
	  (sexp    :tag "Forbidden chars in border ")
	  (sexp    :tag "Regexp for body           ")
	  (integer :tag "number of newlines allowed")
	  (boolean :tag "Stacking allowed          ")))

(defcustom org-emphasis-alist
  '(("*" bold "<b>" "</b>")
    ("/" italic "<i>" "</i>")
    ("_" underline "<u>" "</u>")
    ("=" org-code "<code>" "</code>")
    ("+" (:strike-through t) "<del>" "</del>")
    )
"Special syntax for emphasized text.
Text starting and ending with a special character will be emphasized, for
example *bold*, _underlined_ and /italic/.  This variable sets the marker
characters, the face to be used by font-lock for highlighting in Org-mode
Emacs buffers, and the HTML tags to be used for this.
Use customize to modify this, or restart Emacs after changing it."
  :group 'org-font-lock
  :set 'org-set-emph-re
  :type '(repeat
	  (list
	   (string :tag "Marker character")
	   (choice
	    (face :tag "Font-lock-face")
	    (plist :tag "Face property list"))
	   (string :tag "HTML start tag")
	   (string :tag "HTML end tag"))))

;;; The faces

(defgroup org-faces nil
  "Faces in Org-mode."
  :tag "Org Faces"
  :group 'org-font-lock)

;; FIXME: convert that into a macro?  Not critical, because this
;;        is only executed a few times at load time.
(defun org-compatible-face (specs)
  "Make a compatible face specification.
XEmacs and Emacs 21 do not know about the `min-colors' attribute.
For them we convert a (min-colors 8) entry to a `tty' entry and move it
to the top of the list.  The `min-colors' attribute will be removed from
any other entries, and any resulting duplicates will be removed entirely."
  (if (or (featurep 'xemacs) (< emacs-major-version 22))
      (let (r e a)
	(while (setq e (pop specs))
	  (cond
	   ((memq (car e) '(t default)) (push e r))
	   ((setq a (member '(min-colors 8) (car e)))
	    (nconc r (list (cons (cons '(type tty) (delq (car a) (car e)))
				 (cdr e)))))
	   ((setq a (assq 'min-colors (car e)))
	    (setq e (cons (delq a (car e)) (cdr e)))
	    (or (assoc (car e) r) (push e r)))
	   (t (or (assoc (car e) r) (push e r)))))
	(nreverse r))
    specs))

(defface org-hide
  '((((background light)) (:foreground "white"))
    (((background dark)) (:foreground "black")))
  "Face used to hide leading stars in headlines.
The forground color of this face should be equal to the background
color of the frame."
  :group 'org-faces)

(defface org-level-1 ;; font-lock-function-name-face
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
     (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 8)) (:foreground "blue" :bold t))
     (t (:bold t))))
  "Face used for level 1 headlines."
  :group 'org-faces)

(defface org-level-2 ;; font-lock-variable-name-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
     (((class color) (min-colors 16) (background dark))  (:foreground "LightGoldenrod"))
     (((class color) (min-colors 8)  (background light)) (:foreground "yellow"))
     (((class color) (min-colors 8)  (background dark))  (:foreground "yellow" :bold t))
     (t (:bold t))))
  "Face used for level 2 headlines."
  :group 'org-faces)

(defface org-level-3 ;; font-lock-keyword-face
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Purple"))
     (((class color) (min-colors 88) (background dark))  (:foreground "Cyan1"))
     (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
     (((class color) (min-colors 16) (background dark))  (:foreground "Cyan"))
     (((class color) (min-colors 8)  (background light)) (:foreground "purple" :bold t))
     (((class color) (min-colors 8)  (background dark))  (:foreground "cyan" :bold t))
     (t (:bold t))))
  "Face used for level 3 headlines."
  :group 'org-faces)

(defface org-level-4   ;; font-lock-comment-face
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
     (((class color) (min-colors 88) (background dark))  (:foreground "chocolate1"))
     (((class color) (min-colors 16) (background light)) (:foreground "red"))
     (((class color) (min-colors 16) (background dark))  (:foreground "red1"))
     (((class color) (min-colors 8) (background light))  (:foreground "red" :bold t))
     (((class color) (min-colors 8) (background dark))   (:foreground "red" :bold t))
     (t (:bold t))))
  "Face used for level 4 headlines."
  :group 'org-faces)

(defface org-level-5 ;; font-lock-type-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
     (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
     (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 5 headlines."
  :group 'org-faces)

(defface org-level-6 ;; font-lock-constant-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
     (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
     (((class color) (min-colors 8)) (:foreground "magenta"))))
  "Face used for level 6 headlines."
  :group 'org-faces)

(defface org-level-7 ;; font-lock-builtin-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
     (((class color) (min-colors 8)) (:foreground "blue"))))
  "Face used for level 7 headlines."
  :group 'org-faces)

(defface org-level-8 ;; font-lock-string-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
     (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 8 headlines."
  :group 'org-faces)

(defface org-special-keyword ;; font-lock-string-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
     (t (:italic t))))
  "Face used for special keywords."
  :group 'org-faces)

(defface org-drawer ;; font-lock-function-name-face
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
     (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 8)) (:foreground "blue" :bold t))
     (t (:bold t))))
  "Face used for drawers."
  :group 'org-faces)

(defface org-property-value nil
  "Face used for the value of a property."
  :group 'org-faces)

(defface org-column
  (org-compatible-face
   '((((class color) (min-colors 16) (background light))
      (:background "grey90"))
     (((class color) (min-colors 16) (background dark))
      (:background "grey30"))
     (((class color) (min-colors 8))
      (:background "cyan" :foreground "black"))
     (t (:inverse-video t))))
  "Face for column display of entry properties."
  :group 'org-faces)

(when (fboundp 'set-face-attribute)
  ;; Make sure that a fixed-width face is used when we have a column table.
  (set-face-attribute 'org-column nil
		      :height (face-attribute 'default :height)
		      :family (face-attribute 'default :family)))  

(defface org-warning ;; font-lock-warning-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
     (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
     (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
     (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
     (t (:bold t))))
  "Face for deadlines and TODO keywords."
  :group 'org-faces)

(defface org-archived    ; similar to shadow
  (org-compatible-face
   '((((class color grayscale) (min-colors 88) (background light))
      (:foreground "grey50"))
     (((class color grayscale) (min-colors 88) (background dark))
      (:foreground "grey70"))
     (((class color) (min-colors 8) (background light))
      (:foreground "green"))
     (((class color) (min-colors 8) (background dark))
      (:foreground "yellow"))))
   "Face for headline with the ARCHIVE tag."
   :group 'org-faces)

(defface org-link
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-target
  '((((class color) (background light)) (:underline t))
    (((class color) (background dark)) (:underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-date
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-sexp-date
  '((((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-tag
  '((t (:bold t)))
  "Face for tags."
  :group 'org-faces)

(defface org-todo ;; font-lock-warning-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
     (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
     (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
     (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
     (t (:inverse-video t :bold t))))
  "Face for TODO keywords."
  :group 'org-faces)

(defface org-done ;; font-lock-type-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold t))
     (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold t))
     (((class color) (min-colors 8)) (:foreground "green"))
     (t (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-headline-done ;; font-lock-string-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
     (((class color) (min-colors 8)  (background light)) (:bold nil))))
  "Face used to indicate that a headline is DONE.
This face is only used if `org-fontify-done-headline' is set.  If applies
to the part of the headline after the DONE keyword."
  :group 'org-faces)

(defcustom org-todo-keyword-faces nil
  "Faces for specific TODO keywords.
This is a list of cons cells, with TODO keywords in the car
and faces in the cdr.  The face can be a symbol, or a property
list of attributes, like (:foreground \"blue\" :weight bold :underline t)."
  :group 'org-faces
  :group 'org-todo
  :type '(repeat
	  (cons
	   (string :tag "keyword")
	   (sexp :tag "face"))))

(defface org-table ;; font-lock-function-name-face
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
     (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 8)  (background light)) (:foreground "blue"))
     (((class color) (min-colors 8)  (background dark)))))
  "Face used for tables."
  :group 'org-faces)

(defface org-formula
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
     (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
     (((class color) (min-colors 8)  (background light)) (:foreground "red"))
     (((class color) (min-colors 8)  (background dark)) (:foreground "red"))
     (t (:bold t :italic t))))
  "Face for formulas."
  :group 'org-faces)

(defface org-code
  (org-compatible-face
   '((((class color grayscale) (min-colors 88) (background light))
      (:foreground "grey50"))
     (((class color grayscale) (min-colors 88) (background dark))
      (:foreground "grey70"))
     (((class color) (min-colors 8) (background light))
      (:foreground "green"))
     (((class color) (min-colors 8) (background dark))
      (:foreground "yellow"))))
   "Face for fixed-with text like code snippets."
   :group 'org-faces
   :version "22.1")

(defface org-agenda-structure ;; font-lock-function-name-face
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
     (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
     (((class color) (min-colors 8)) (:foreground "blue" :bold t))
     (t (:bold t))))
  "Face used in agenda for captions and dates."
  :group 'org-faces)

(defface org-scheduled-today
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))
     (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
     (((class color) (min-colors 8)) (:foreground "green"))
     (t (:bold t :italic t))))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-scheduled-previously
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
     (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
     (((class color) (min-colors 8)  (background light)) (:foreground "red"))
     (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
     (t (:bold t))))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defface org-upcoming-deadline
  (org-compatible-face
   '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
     (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
     (((class color) (min-colors 8)  (background light)) (:foreground "red"))
     (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
     (t (:bold t))))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defcustom org-agenda-deadline-faces
  '((1.0 . org-warning)
    (0.5 . org-upcoming-deadline)
    (0.0 . default))
  "Faces for showing deadlines in the agenda.
This is a list of cons cells.  The cdr of each cess is a face to be used,
and it can also just be a like like '(:foreground \"yellow\").
Each car is a fraction of the head-warning time that must have passed for
this the face in the cdr to be used for display.  The numbers must be
given in descending order.  The head-warning time is normally taken
from `org-deadline-warning-days', but can also be specified in the deadline
timestamp itself, like this:

   DEADLINE: <2007-08-13 Mon -8d>

You may use d for days, w for weeks, m for months and y for years.  Months
and years will only be treated in an approximate fashion (30.4 days for a
month and 365.24 days for a year)."
  :group 'org-faces
  :group 'org-agenda-daily/weekly
  :type '(repeat
	  (cons
	   (number :tag "Fraction of head-warning time passed")
	   (sexp :tag "Face"))))

(defface org-time-grid ;; font-lock-variable-name-face
  (org-compatible-face
   '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
     (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
     (((class color) (min-colors 8)) (:foreground "yellow" :weight light))))
  "Face used for time grids."
  :group 'org-faces)

(defconst org-level-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8
    ))

(defcustom org-n-level-faces (length org-level-faces)
  "The number different faces to be used for headlines.
Org-mode defines 8 different headline faces, so this can be at most 8.
If it is less than 8, the level-1 face gets re-used for level N+1 etc."
  :type 'number
  :group 'org-faces)

;;; Variables for pre-computed regular expressions, all buffer local

(defvar org-drawer-regexp nil
  "Matches first line of a hidden block.")
(make-variable-buffer-local 'org-drawer-regexp)
(defvar org-todo-regexp nil
  "Matches any of the TODO state keywords.")
(make-variable-buffer-local 'org-todo-regexp)
(defvar org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.")
(make-variable-buffer-local 'org-not-done-regexp)
(defvar org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.")
(make-variable-buffer-local 'org-todo-line-regexp)
(defvar org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")
(make-variable-buffer-local 'org-todo-line-tags-regexp)
(defvar org-nl-done-regexp nil
  "Matches newline followed by a headline with the DONE keyword.")
(make-variable-buffer-local 'org-nl-done-regexp)
(defvar org-looking-at-done-regexp nil
  "Matches the DONE keyword a point.")
(make-variable-buffer-local 'org-looking-at-done-regexp)
(defvar org-ds-keyword-length 12
  "Maximum length of the Deadline and SCHEDULED keywords.")
(make-variable-buffer-local 'org-ds-keyword-length)
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
(defvar org-closed-time-regexp nil
  "Matches the CLOSED keyword together with a time stamp.")
(make-variable-buffer-local 'org-closed-time-regexp)

(defvar org-keyword-time-regexp nil
  "Matches any of the 4 keywords, together with the time stamp.")
(make-variable-buffer-local 'org-keyword-time-regexp)
(defvar org-keyword-time-not-clock-regexp nil
  "Matches any of the 3 keywords, together with the time stamp.")
(make-variable-buffer-local 'org-keyword-time-not-clock-regexp)
(defvar org-maybe-keyword-time-regexp nil
  "Matches a timestamp, possibly preceeded by a keyword.")
(make-variable-buffer-local 'org-maybe-keyword-time-regexp)
(defvar org-planning-or-clock-line-re nil
  "Matches a line with planning or clock info.")
(make-variable-buffer-local 'org-planning-or-clock-line-re)

(defconst org-rm-props '(invisible t face t keymap t intangible t mouse-face t
				   rear-nonsticky t mouse-map t fontified t)
  "Properties to remove when a string without properties is wanted.")

(defsubst org-match-string-no-properties (num &optional string)
  (if (featurep 'xemacs)
      (let ((s (match-string num string)))
	(remove-text-properties 0 (length s) org-rm-props s)
	s)
    (match-string-no-properties num string)))

(defsubst org-no-properties (s)
  (if (fboundp 'set-text-properties)
      (set-text-properties 0 (length s) nil s)
    (remove-text-properties 0 (length s) org-rm-props s))
  s)

(defsubst org-get-alist-option (option key)
  (cond ((eq key t) t)
	((eq option t) t)
	((assoc key option) (cdr (assoc key option)))
	(t (cdr (assq 'default option)))))

(defsubst org-inhibit-invisibility ()
  "Modified `buffer-invisibility-spec' for Emacs 21.
Some ops with invisible text do not work correctly on Emacs 21.  For these
we turn off invisibility temporarily.  Use this in a `let' form."
  (if (< emacs-major-version 22) nil buffer-invisibility-spec))

(defsubst org-set-local (var value)
  "Make VAR local in current buffer and set it to VALUE."
  (set (make-variable-buffer-local var) value))

(defsubst org-mode-p ()
  "Check if the current buffer is in Org-mode."
  (eq major-mode 'org-mode))

(defsubst org-last (list)
  "Return the last element of LIST."
  (car (last list)))

(defun org-let (list &rest body)
  (eval (cons 'let (cons list body))))
(put 'org-let 'lisp-indent-function 1)

(defun org-let2 (list1 list2 &rest body)
  (eval (cons 'let (cons list1 (list (cons 'let (cons list2 body)))))))
(put 'org-let2 'lisp-indent-function 2)
(defconst org-startup-options
  '(("fold" org-startup-folded t)
    ("overview" org-startup-folded t)
    ("nofold" org-startup-folded nil)
    ("showall" org-startup-folded nil)
    ("content" org-startup-folded content)
    ("hidestars" org-hide-leading-stars t)
    ("showstars" org-hide-leading-stars nil)
    ("odd" org-odd-levels-only t)
    ("oddeven" org-odd-levels-only nil)
    ("align" org-startup-align-all-tables t)
    ("noalign" org-startup-align-all-tables nil)
    ("customtime" org-display-custom-times t)
    ("logging" org-log-done t)
    ("logdone" org-log-done t)
    ("nologging" org-log-done nil)
    ("lognotedone" org-log-done done push)
    ("lognotestate" org-log-done state push)
    ("lognoteclock-out" org-log-done clock-out push)
    ("logrepeat" org-log-repeat t)
    ("nologrepeat" org-log-repeat nil)
    ("constcgs" constants-unit-system cgs)
    ("constSI" constants-unit-system SI))
  "Variable associated with STARTUP options for org-mode.
Each element is a list of three items: The startup options as written
in the #+STARTUP line, the corresponding variable, and the value to
set this variable to if the option is found.  An optional forth element PUSH
means to push this value onto the list in the variable.")

(defun org-set-regexps-and-options ()
  "Precompute regular expressions for current buffer."
  (when (org-mode-p)
    (org-set-local 'org-todo-kwd-alist nil)
    (org-set-local 'org-todo-key-alist nil)
    (org-set-local 'org-todo-key-trigger nil)
    (org-set-local 'org-todo-keywords-1 nil)
    (org-set-local 'org-done-keywords nil)
    (org-set-local 'org-todo-heads nil)
    (org-set-local 'org-todo-sets nil)
    (let ((re (org-make-options-regexp
	       '("CATEGORY" "SEQ_TODO" "PRI_TODO" "TYP_TODO" "COLUMNS"
		 "STARTUP" "ARCHIVE" "TAGS" "LINK" "PRIORITIES"
		 "CONSTANTS" "PROPERTY")))
	  (splitre "[ \t]+")
	  kwds kws0 kwsa key value cat arch tags const links hw dws
	  tail sep kws1 prio props)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (setq key (match-string 1) value (org-match-string-no-properties 2))
	    (cond
	     ((equal key "CATEGORY")
	      (if (string-match "[ \t]+$" value)
		  (setq value (replace-match "" t t value)))
	      (setq cat (intern value)))
	     ((equal key "SEQ_TODO")
	      (push (cons 'sequence (org-split-string value splitre)) kwds))
	     ((equal key "TYP_TODO")
	      (push (cons 'type (org-split-string value splitre)) kwds))
	     ((equal key "TAGS")
	      (setq tags (append tags (org-split-string value splitre))))
	     ((equal key "COLUMNS")
	      (org-set-local 'org-columns-default-format value))
	     ((equal key "LINK")
	      (when (string-match "^\\(\\S-+\\)[ \t]+\\(.+\\)" value)
		(push (cons (match-string 1 value)
			    (org-trim (match-string 2 value)))
		      links)))
	     ((equal key "PRIORITIES")
	      (setq prio (org-split-string value " +")))
	     ((equal key "PROPERTY")
	      (when (string-match "\\(\\S-+\\)\\s-+\\(.*\\)" value)
		(push (cons (match-string 1 value) (match-string 2 value))
		      props)))
	     ((equal key "CONSTANTS")
	      (setq const (append const (org-split-string value splitre))))
	     ((equal key "STARTUP")
	      (let ((opts (org-split-string value splitre))
		    l var val)
		(while (setq l (pop opts))
		  (when (setq l (assoc l org-startup-options))
		    (setq var (nth 1 l) val (nth 2 l))
		    (if (not (nth 3 l))
			(set (make-local-variable var) val)
		      (if (not (listp (symbol-value var)))
			  (set (make-local-variable var) nil))
		      (set (make-local-variable var) (symbol-value var))
		      (add-to-list var val))))))
	     ((equal key "ARCHIVE")
	      (string-match " *$" value)
	      (setq arch (replace-match "" t t value))
	      (remove-text-properties 0 (length arch)
				      '(face t fontified t) arch)))
	    )))
      (and cat (org-set-local 'org-category cat))
      (when prio
	(if (< (length prio) 3) (setq prio '("A" "C" "B")))
	(setq prio (mapcar 'string-to-char prio))
	(org-set-local 'org-highest-priority (nth 0 prio))
	(org-set-local 'org-lowest-priority  (nth 1 prio))
	(org-set-local 'org-default-priority (nth 2 prio)))
      (and props (org-set-local 'org-local-properties (nreverse props)))
      (and arch (org-set-local 'org-archive-location arch))
      (and links (setq org-link-abbrev-alist-local (nreverse links)))
      ;; Process the TODO keywords
      (unless kwds
	;; Use the global values as if they had been given locally.
	(setq kwds (default-value 'org-todo-keywords))
	(if (stringp (car kwds))
	    (setq kwds (list (cons org-todo-interpretation
				   (default-value 'org-todo-keywords)))))
	(setq kwds (reverse kwds)))
      (setq kwds (nreverse kwds))
      (let (inter kws)
	(while (setq kws (pop kwds))
	  (setq inter (pop kws) sep (member "|" kws)
		kws0 (delete "|" (copy-sequence kws))
		kwsa nil
		kws1 (mapcar (lambda (x)
			       (if (string-match "\\(.*\\)(\\(.\\))" x)
				   (progn
				     (push (cons (match-string 1 x)
						 (string-to-char
						  (match-string 2 x))) kwsa)
				     (match-string 1 x))
				 (push (list x) kwsa)
				 x))
			     kws0)
		kwsa (if kwsa (append '((:startgroup))
				      (nreverse kwsa)
				      '((:endgroup))))
		hw (car kws1)
		dws (if sep (org-remove-keyword-keys (cdr sep)) (last kws1))
		tail (list inter hw (car dws) (org-last dws)))
	  (add-to-list 'org-todo-heads hw 'append)
	  (push kws1 org-todo-sets)
	  (setq org-done-keywords (append org-done-keywords dws nil))
	  (setq org-todo-key-alist (append org-todo-key-alist kwsa))
	  (mapc (lambda (x) (push (cons x tail) org-todo-kwd-alist)) kws1)
	  (setq org-todo-keywords-1 (append org-todo-keywords-1 kws1 nil)))
	(setq org-todo-sets (nreverse org-todo-sets)
	      org-todo-kwd-alist (nreverse org-todo-kwd-alist)
	      org-todo-key-trigger (delq nil (mapcar 'cdr org-todo-key-alist))
	      org-todo-key-alist (org-assign-fast-keys org-todo-key-alist)))
      ;; Process the constants
      (when const
	(let (e cst)
	  (while (setq e (pop const))
	    (if (string-match "^\\([a-zA-Z0][_a-zA-Z0-9]*\\)=\\(.*\\)" e)
		(push (cons (match-string 1 e) (match-string 2 e)) cst)))
	  (setq org-table-formula-constants-local cst)))

      ;; Process the tags.
      (when tags
	(let (e tgs)
	  (while (setq e (pop tags))
	    (cond
	     ((equal e "{") (push '(:startgroup) tgs))
	     ((equal e "}") (push '(:endgroup) tgs))
	     ((string-match (org-re "^\\([[:alnum:]_@]+\\)(\\(.\\))$") e)
	      (push (cons (match-string 1 e)
			  (string-to-char (match-string 2 e)))
		    tgs))
	     (t (push (list e) tgs))))
	  (org-set-local 'org-tag-alist nil)
	  (while (setq e (pop tgs))
	    (or (and (stringp (car e))
		     (assoc (car e) org-tag-alist))
		(push e org-tag-alist))))))

    ;; Compute the regular expressions and other local variables
    (if (not org-done-keywords)
	(setq org-done-keywords (list (org-last org-todo-keywords-1))))
    (setq org-ds-keyword-length (+ 2 (max (length org-deadline-string)
					  (length org-scheduled-string)))
	  org-drawer-regexp
	  (concat "^[ \t]*:\\("
		  (mapconcat 'regexp-quote org-drawers "\\|")
		  "\\):[ \t]*$")
	  org-not-done-keywords
	  (org-delete-all org-done-keywords (copy-sequence org-todo-keywords-1))
	  org-todo-regexp
	  (concat "\\<\\(" (mapconcat 'regexp-quote org-todo-keywords-1
				      "\\|") "\\)\\>")
	  org-not-done-regexp
	  (concat "\\<\\("
		  (mapconcat 'regexp-quote org-not-done-keywords "\\|")
		  "\\)\\>")
	  org-todo-line-regexp
	  (concat "^\\(\\*+\\)[ \t]+\\(?:\\("
		  (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		  "\\)\\>\\)?[ \t]*\\(.*\\)")
	  org-nl-done-regexp
	  (concat "\n\\*+[ \t]+"
		  "\\(?:" (mapconcat 'regexp-quote org-done-keywords "\\|")
		  "\\)" "\\>")
	  org-todo-line-tags-regexp
	  (concat "^\\(\\*+\\)[ \t]+\\(?:\\("
		  (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		  (org-re
		   "\\)\\>\\)? *\\(.*?\\([ \t]:[[:alnum:]:_@]+:[ \t]*\\)?$\\)"))
	  org-looking-at-done-regexp
	  (concat "^" "\\(?:"
		  (mapconcat 'regexp-quote org-done-keywords "\\|") "\\)"
		  "\\>")
	  org-deadline-regexp (concat "\\<" org-deadline-string)
	  org-deadline-time-regexp
	  (concat "\\<" org-deadline-string " *<\\([^>]+\\)>")
	  org-deadline-line-regexp
	  (concat "\\<\\(" org-deadline-string "\\).*")
	  org-scheduled-regexp
	  (concat "\\<" org-scheduled-string)
	  org-scheduled-time-regexp
	  (concat "\\<" org-scheduled-string " *<\\([^>]+\\)>")
	  org-closed-time-regexp
	  (concat "\\<" org-closed-string " *\\[\\([^]]+\\)\\]")
	  org-keyword-time-regexp
	  (concat "\\<\\(" org-scheduled-string
		  "\\|" org-deadline-string
		  "\\|" org-closed-string
		  "\\|" org-clock-string "\\)"
		  " *[[<]\\([^]>]+\\)[]>]")
	  org-keyword-time-not-clock-regexp
	  (concat "\\<\\(" org-scheduled-string
		  "\\|" org-deadline-string
		  "\\|" org-closed-string
		  "\\)"
		  " *[[<]\\([^]>]+\\)[]>]")
	  org-maybe-keyword-time-regexp
	  (concat "\\(\\<\\(" org-scheduled-string
		  "\\|" org-deadline-string
		  "\\|" org-closed-string
		  "\\|" org-clock-string "\\)\\)?"
		  " *\\([[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]\r\n>]*?[]>]\\|<%%([^\r\n>]*>\\)")
	  org-planning-or-clock-line-re
	  (concat "\\(?:^[ \t]*\\(" org-scheduled-string
		  "\\|" org-deadline-string
		  "\\|" org-closed-string "\\|" org-clock-string
		  "\\)\\>\\)")
	  )

    (org-set-font-lock-defaults)))

(defun org-remove-keyword-keys (list)
  (mapcar (lambda (x)
	    (if (string-match "(.)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))

;;; Some variables ujsed in various places

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")
(defvar org-finish-function nil
  "Function to be called when `C-c C-c' is used.
This is for getting out of special buffers like remember.")

;;; Foreign variables, to inform the compiler

;; XEmacs only
(defvar outline-mode-menu-heading)
(defvar outline-mode-menu-show)
(defvar outline-mode-menu-hide)
(defvar zmacs-regions) ; XEmacs regions
;; Emacs only
(defvar mark-active)

;; Packages that org-mode interacts with
(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(defvar font-lock-unfontify-region-function)
(defvar org-goto-start-pos)
(defvar vm-message-pointer)
(defvar vm-folder-directory)
(defvar wl-summary-buffer-elmo-folder)
(defvar wl-summary-buffer-folder-name)
(defvar gnus-other-frame-object)
(defvar gnus-group-name)
(defvar gnus-article-current)
(defvar w3m-current-url)
(defvar w3m-current-title)
(defvar mh-progs)
(defvar mh-current-folder)
(defvar mh-show-folder-buffer)
(defvar mh-index-folder)
(defvar mh-searcher)
(defvar calendar-mode-map)
(defvar Info-current-file)
(defvar Info-current-node)
(defvar texmathp-why)
(defvar remember-save-after-remembering)
(defvar remember-data-file)
(defvar annotation) ; from remember.el, dynamically scoped in `remember-mode'
(defvar initial)    ; from remember.el, dynamically scoped in `remember-mode'
(defvar org-latex-regexps)
(defvar constants-unit-system)

(defvar original-date) ; dynamically scoped in calendar.el does scope this

;; FIXME: Occasionally check by commenting these, to make sure
;;        no other functions uses these, forgetting to let-bind them.
(defvar entry)
(defvar state)
(defvar last-state)
(defvar date)
(defvar description)


;; Defined somewhere in this file, but used before definition.
(defvar orgtbl-mode-menu) ; defined when orgtbl mode get initialized
(defvar org-agenda-buffer-name)
(defvar org-agenda-undo-list)
(defvar org-agenda-pending-undo-list)
(defvar org-agenda-overriding-header)
(defvar orgtbl-mode)
(defvar org-html-entities)
(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)
(defvar org-agenda-keymap)
(defvar org-category-table)

;;;; Emacs/XEmacs compatibility

;; Overlay compatibility functions
(defun org-make-overlay (beg end &optional buffer)
  (if (featurep 'xemacs)
      (make-extent beg end buffer)
    (make-overlay beg end buffer)))
(defun org-delete-overlay (ovl)
  (if (featurep 'xemacs) (delete-extent ovl) (delete-overlay ovl)))
(defun org-detach-overlay (ovl)
  (if (featurep 'xemacs) (detach-extent ovl) (delete-overlay ovl)))
(defun org-move-overlay (ovl beg end &optional buffer)
  (if (featurep 'xemacs)
      (set-extent-endpoints ovl beg end (or buffer (current-buffer)))
    (move-overlay ovl beg end buffer)))
(defun org-overlay-put (ovl prop value)
  (if (featurep 'xemacs)
      (set-extent-property ovl prop value)
    (overlay-put ovl prop value)))
(defun org-overlay-display (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if (featurep 'xemacs)
      (let ((gl (make-glyph text)))
	(and face (set-glyph-face gl face))
	(set-extent-property ovl 'invisible t)
	(set-extent-property ovl 'end-glyph gl))
    (overlay-put ovl 'display text)
    (if face (overlay-put ovl 'face face))
    (if evap (overlay-put ovl 'evaporate t))))
(defun org-overlay-before-string (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if (featurep 'xemacs)
      (let ((gl (make-glyph text)))
	(and face (set-glyph-face gl face))
	(set-extent-property ovl 'begin-glyph gl))
    (if face (org-add-props text nil 'face face))
    (overlay-put ovl 'before-string text)
    (if evap (overlay-put ovl 'evaporate t))))
(defun org-overlay-get (ovl prop)
  (if (featurep 'xemacs)
      (extent-property ovl prop)
    (overlay-get ovl prop)))
(defun org-overlays-at (pos)
  (if (featurep 'xemacs) (extents-at pos) (overlays-at pos)))
;; FIXME: this is currently not used
(defun org-overlays-in (&optional start end)
  (if (featurep 'xemacs)
      (extent-list nil start end)
    (overlays-in start end)))
(defun org-overlay-start (o)
  (if (featurep 'xemacs) (extent-start-position o) (overlay-start o)))
(defun org-overlay-end (o)
  (if (featurep 'xemacs) (extent-end-position o) (overlay-end o)))
;; FIXME: this is currently not used
(defun org-find-overlays (prop &optional pos delete)
  "Find all overlays specifying PROP at POS or point.
If DELETE is non-nil, delete all those overlays."
  (let ((overlays (org-overlays-at (or pos (point))))
	ov found)
    (while (setq ov (pop overlays))
      (if (org-overlay-get ov prop)
          (if delete (org-delete-overlay ov) (push ov found))))
    found))

;; Region compatibility

(defun org-add-hook (hook function &optional append local)
  "Add-hook, compatible with both Emacsen."
  (if (and local (featurep 'xemacs))
      (add-local-hook hook function append)
    (add-hook hook function append local)))

(defvar org-ignore-region nil
  "To temporarily disable the active region.")

(defun org-region-active-p ()
  "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
  (if org-ignore-region
      nil
    (if (featurep 'xemacs)
	(and zmacs-regions (region-active-p))
      (and transient-mark-mode mark-active))))

;; Invisibility compatibility

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

;; FIXME: this is currently not used
(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (if (consp buffer-invisibility-spec)
      (member arg buffer-invisibility-spec)
    nil))

;;;; Define the Org-mode

(if (and (not (keymapp outline-mode-map)) (featurep 'allout))
    (error "Conflict with outdated version of allout.el.  Load org.el before allout.el, or ugrade to newer allout, for example by switching to Emacs 22."))


;; We use a before-change function to check if a table might need
;; an update.
(defvar org-table-may-need-update t
  "Indicates that a table might need an update.
This variable is set by `org-before-change-function'.
`org-table-align' sets it back to nil.")
(defvar org-mode-map)
(defvar org-mode-hook nil)
(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar org-table-buffer-is-an nil)


;;;###autoload
(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

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

  ;; Get rid of Outline menus, they are not needed
  ;; Need to do this here because define-derived-mode sets up
  ;; the keymap so late.  Still, it is a waste to call this each time
  ;; we switch another buffer into org-mode.
  (if (featurep 'xemacs)
      (when (boundp 'outline-mode-menu-heading)
	;; Assume this is Greg's port, it used easymenu
	(easy-menu-remove outline-mode-menu-heading)
	(easy-menu-remove outline-mode-menu-show)
	(easy-menu-remove outline-mode-menu-hide))
    (define-key org-mode-map [menu-bar headings] 'undefined)
    (define-key org-mode-map [menu-bar hide] 'undefined)
    (define-key org-mode-map [menu-bar show] 'undefined))

  (easy-menu-add org-org-menu)
  (easy-menu-add org-tbl-menu)
  (org-install-agenda-files-menu)
  (if org-descriptive-links (org-add-to-invisibility-spec '(org-link)))
  (org-add-to-invisibility-spec '(org-cwidth))
  (when (featurep 'xemacs)
    (org-set-local 'line-move-ignore-invisible t))
  (org-set-local 'outline-regexp "\\*+ ")
  (setq outline-level 'org-outline-level)
  (when (and org-ellipsis
             (fboundp 'set-display-table-slot) (boundp 'buffer-display-table)
	     (fboundp 'make-glyph-code))
    (unless org-display-table
      (setq org-display-table (make-display-table)))
    (set-display-table-slot
     org-display-table 4
     (vconcat (mapcar
	       (lambda (c) (make-glyph-code c (and (not (stringp org-ellipsis))
						   org-ellipsis)))
	       (if (stringp org-ellipsis) org-ellipsis "..."))))
    (setq buffer-display-table org-display-table))
  (org-set-regexps-and-options)
  ;; Calc embedded
  (org-set-local 'calc-embedded-open-mode "# ")
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?@ "w")
  (if org-startup-truncated (setq truncate-lines t))
  (org-set-local 'font-lock-unfontify-region-function
		 'org-unfontify-region)
  ;; Activate before-change-function
  (org-set-local 'org-table-may-need-update t)
  (org-add-hook 'before-change-functions 'org-before-change-function nil
		'local)
  ;; Check for running clock before killing a buffer
  (org-add-hook 'kill-buffer-hook 'org-check-running-clock nil 'local)
  ;; Paragraphs and auto-filling
  (org-set-autofill-regexps)
  (setq indent-line-function 'org-indent-line-function)
  (org-update-radio-target-regexp)

  ;; Comment characters
;  (org-set-local 'comment-start "#") ;; FIXME: this breaks wrapping
  (org-set-local 'comment-padding " ")

  ;; Make isearch reveal context
  (if (or (featurep 'xemacs)
	  (not (boundp 'outline-isearch-open-invisible-function)))
      ;; Emacs 21 and XEmacs make use of the hook
      (org-add-hook 'isearch-mode-end-hook 'org-isearch-end 'append 'local)
    ;; Emacs 22 deals with this through a special variable
    (org-set-local 'outline-isearch-open-invisible-function
		   (lambda (&rest ignore) (org-show-context 'isearch))))

  ;; If empty file that did not turn on org-mode automatically, make it to.
  (if (and org-insert-mode-line-in-empty-file
	   (interactive-p)
	   (= (point-min) (point-max)))
      (insert "#    -*- mode: org -*-\n\n"))

  (unless org-inhibit-startup
    (when org-startup-align-all-tables
      (let ((bmp (buffer-modified-p)))
	(org-table-map-tables 'org-table-align)
	(set-buffer-modified-p bmp)))
    (org-cycle-hide-drawers 'all)
    (cond
     ((eq org-startup-folded t)
      (org-cycle '(4)))
     ((eq org-startup-folded 'content)
      (let ((this-command 'org-cycle) (last-command 'org-cycle))
	(org-cycle '(4)) (org-cycle '(4)))))))

(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)

(defsubst org-call-with-arg (command arg)
  "Call COMMAND interactively, but pretend prefix are was ARG."
  (let ((current-prefix-arg arg)) (call-interactively command)))

(defsubst org-current-line (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    ;; works also in narrowed buffer, because we start at 1, not point-min
    (+ (if (bolp) 1 0) (count-lines 1 (point)))))

(defun org-current-time ()
  "Current time, possibly rounded to `org-time-stamp-rounding-minutes'."
  (if (> org-time-stamp-rounding-minutes 0)
      (let ((r org-time-stamp-rounding-minutes)
	    (time (decode-time)))
	(apply 'encode-time
	       (append (list 0 (* r (floor (+ .5 (/ (float (nth 1 time)) r)))))
		       (nthcdr 2 time))))
    (current-time)))

(defun org-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)
(put 'org-add-props 'lisp-indent-function 2)


;;;; Font-Lock stuff, including the activators

(defvar org-mouse-map (make-sparse-keymap))
(org-defkey org-mouse-map
  (if (featurep 'xemacs) [button2] [mouse-2]) 'org-open-at-mouse)
(org-defkey org-mouse-map
  (if (featurep 'xemacs) [button3] [mouse-3]) 'org-find-file-at-mouse)
(when org-mouse-1-follows-link
  (org-defkey org-mouse-map [follow-link] 'mouse-face))
(when org-tab-follows-link
  (org-defkey org-mouse-map [(tab)] 'org-open-at-point)
  (org-defkey org-mouse-map "\C-i" 'org-open-at-point))
(when org-return-follows-link
  (org-defkey org-mouse-map [(return)] 'org-open-at-point)
  (org-defkey org-mouse-map "\C-m" 'org-open-at-point))

(require 'font-lock)

(defconst org-non-link-chars "]\t\n\r<>")
(defvar org-link-types '("http" "https" "ftp" "mailto" "file" "news" "bbdb" "vm"
			   "wl" "mhe" "rmail" "gnus" "shell" "info" "elisp"))
(defvar org-link-re-with-space nil
   "Matches a link with spaces, optional angular brackets around it.")
(defvar org-link-re-with-space2 nil
   "Matches a link with spaces, optional angular brackets around it.")
(defvar org-angle-link-re nil
   "Matches link with angular brackets, spaces are allowed.")
(defvar org-plain-link-re nil
   "Matches plain link, without spaces.")
(defvar org-bracket-link-regexp nil
  "Matches a link in double brackets.")
(defvar org-bracket-link-analytic-regexp nil
  "Regular expression used to analyze links.
Here is what the match groups contain after a match:
1: http:
2: http
3: path
4: [desc]
5: desc")
(defvar org-any-link-re nil
  "Regular expression matching any link.")

(defun org-make-link-regexps ()
  "Update the link regular expressions.
This should be called after the variable `org-link-types' has changed."
  (setq org-link-re-with-space
	(concat
	 "<?\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^" org-non-link-chars "]*"
	 "[^" org-non-link-chars " ]\\)>?")
	org-link-re-with-space2
	(concat
	 "<?\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^]\t\n\r]*"
	 "[^" org-non-link-chars " ]\\)>?")
	org-angle-link-re
	(concat
	 "<\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^" org-non-link-chars "]*"
	 "\\)>")
	org-plain-link-re
	(concat
	 "\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^]\t\n\r<>,;() ]+\\)")
	org-bracket-link-regexp
	"\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]"
	org-bracket-link-analytic-regexp
	(concat
	 "\\[\\["
	 "\\(\\(" (mapconcat 'identity org-link-types "\\|") "\\):\\)?"
	 "\\([^]]+\\)"
	 "\\]"
	 "\\(\\[" "\\([^]]+\\)" "\\]\\)?"
	 "\\]")
	org-any-link-re
	(concat "\\(" org-bracket-link-regexp "\\)\\|\\("
		org-angle-link-re "\\)\\|\\("
		org-plain-link-re "\\)")))

(org-make-link-regexps)

(defconst org-ts-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^\r\n>]*?\\)>"
  "Regular expression for fast time stamp matching.")
(defconst org-ts-regexp-both "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^\r\n>]*?\\)[]>]"
  "Regular expression for fast time stamp matching.")
(defconst org-ts-regexp0 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([^]0-9>\r\n]*\\)\\(\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date.")
(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([^]0-9>\r\n]*\\)\\(\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")
(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 "[^>\n]\\{0,11\\}>")
  "Regular expression matching time stamps, with groups.")
(defconst org-ts-regexp3 (concat "[[<]" org-ts-regexp1 "[^]>\n]\\{0,11\\}[]>]")
  "Regular expression matching time stamps (also [..]), with groups.")
(defconst org-tr-regexp (concat org-ts-regexp "--?-?" org-ts-regexp)
  "Regular expression matching a time stamp range.")
(defconst org-tr-regexp-both
  (concat org-ts-regexp-both "--?-?" org-ts-regexp-both)
  "Regular expression matching a time stamp range.")
(defconst org-tsr-regexp (concat org-ts-regexp "\\(--?-?"
				 org-ts-regexp "\\)?")
  "Regular expression matching a time stamp or time stamp range.")
(defconst org-tsr-regexp-both (concat org-ts-regexp-both "\\(--?-?"
				      org-ts-regexp-both "\\)?")
  "Regular expression matching a time stamp or time stamp range.
The time stamps may be either active or inactive.")

(defvar org-emph-face nil)

(defun org-do-emphasis-faces (limit)
  "Run through the buffer and add overlays to links."
  (let (rtn)
    (while (and (not rtn) (re-search-forward org-emph-re limit t))
      (if (not (= (char-after (match-beginning 3))
		  (char-after (match-beginning 4))))
	  (progn
	    (setq rtn t)
	    (font-lock-prepend-text-property (match-beginning 2) (match-end 2)
					     'face
					     (nth 1 (assoc (match-string 3)
							   org-emphasis-alist)))
	    (add-text-properties (match-beginning 2) (match-end 2)
				 '(font-lock-multiline t))
	    (backward-char 1))))
    rtn))

(defun org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be either the marker character, or the first character of the
HTML tag associated with that emphasis.  If CHAR is a space, the means
to remove the emphasis of the selected region.
If char is not given (for example in an interactive call) it
will be prompted for."
  (interactive)
  (let ((eal org-emphasis-alist) e det
	(erc org-emphasis-regexp-components)
	(prompt "")
	(string "") beg end move tag c s)
    (if (org-region-active-p)
	(setq beg (region-beginning) end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (while (setq e (pop eal))
      (setq tag (car (org-split-string (nth 2 e) "[ <>/]+"))
	    c (aref tag 0))
      (push (cons c (string-to-char (car e))) det)
      (setq prompt (concat prompt (format " [%s%c]%s" (car e) c
					  (substring tag 1)))))
    (unless char
      (message "%s" (concat "Emphasis marker or tag:" prompt))
      (setq char (read-char-exclusive)))
    (setq char (or (cdr (assoc char det)) char))
    (if (equal char ?\ )
	(setq s "" move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (if beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert " "))
    (unless (string-match (concat "[" (nth 1 erc) "\n]")
			  (char-to-string (char-after (point))))
      (insert " ") (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

(defun org-activate-plain-links (limit)
  "Run through the buffer and add overlays to links."
  (catch 'exit
    (let (f)
      (while (re-search-forward org-plain-link-re limit t)
	(setq f (get-text-property (match-beginning 0) 'face))
	(if (or (eq f 'org-tag)
		(and (listp f) (memq 'org-tag f)))
	    nil
	  (add-text-properties (match-beginning 0) (match-end 0)
			       (list 'mouse-face 'highlight
				     'rear-nonsticky t
				     'keymap org-mouse-map
				     ))
	  (throw 'exit t))))))

(defun org-activate-angle-links (limit)
  "Run through the buffer and add overlays to links."
  (if (re-search-forward org-angle-link-re limit t)
      (progn
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'rear-nonsticky t
				   'keymap org-mouse-map
				   ))
	t)))

(defmacro org-maybe-intangible (props)
  "Add '(intangigble t) to PROPS if Emacs version is earlier than Emacs 22.
In emacs 21, invisible text is not avoided by the command loop, so the
intangible property is needed to make sure point skips this text.
In Emacs 22, this is not necessary.  The intangible text property has
led to problems with flyspell.  These problems are fixed in flyspell.el,
but we still avoid setting the property in Emacs 22 and later.
We use a macro so that the test can happen at compilation time."
  (if (< emacs-major-version 22)
      `(append '(intangible t) ,props)
    props))

(defun org-activate-bracket-links (limit)
  "Run through the buffer and add overlays to bracketed links."
  (if (re-search-forward org-bracket-link-regexp limit t)
      (let* ((help (concat "LINK: "
			   (org-match-string-no-properties 1)))
	     ;; FIXME: above we should remove the escapes.
	     ;; but that requires another match, protecting match data,
	     ;; a lot of overhead for font-lock.
	     (ip (org-maybe-intangible
		  (list 'invisible 'org-link 'rear-nonsticky t
			'keymap org-mouse-map 'mouse-face 'highlight
			'help-echo help)))
	     (vp (list 'rear-nonsticky t
		       'keymap org-mouse-map 'mouse-face 'highlight
		       'help-echo help)))
	;; We need to remove the invisible property here.  Table narrowing
	;; may have made some of this invisible.
	(remove-text-properties (match-beginning 0) (match-end 0)
				'(invisible nil))
	(if (match-end 3)
	    (progn
	      (add-text-properties (match-beginning 0) (match-beginning 3) ip)
	      (add-text-properties (match-beginning 3) (match-end 3) vp)
	      (add-text-properties (match-end 3) (match-end 0) ip))
	  (add-text-properties (match-beginning 0) (match-beginning 1) ip)
	  (add-text-properties (match-beginning 1) (match-end 1) vp)
	  (add-text-properties (match-end 1) (match-end 0) ip))
	t)))

(defun org-activate-dates (limit)
  "Run through the buffer and add overlays to dates."
  (if (re-search-forward org-tsr-regexp-both limit t)
      (progn
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'rear-nonsticky t
				   'keymap org-mouse-map))
	(when org-display-custom-times
	  (if (match-end 3)
	      (org-display-custom-time (match-beginning 3) (match-end 3)))
	  (org-display-custom-time (match-beginning 1) (match-end 1)))
	t)))

(defvar org-target-link-regexp nil
  "Regular expression matching radio targets in plain text.")
(defvar org-target-regexp "<<\\([^<>\n\r]+\\)>>"
  "Regular expression matching a link target.")
(defvar org-radio-target-regexp "<<<\\([^<>\n\r]+\\)>>>"
  "Regular expression matching a radio target.")
(defvar org-any-target-regexp "<<<?\\([^<>\n\r]+\\)>>>?" ; FIXME, not exact, would match <<<aaa>>  as a radio target.
  "Regular expression matching any target.")

(defun org-activate-target-links (limit)
  "Run through the buffer and add overlays to target matches."
  (when org-target-link-regexp
    (let ((case-fold-search t))
      (if (re-search-forward org-target-link-regexp limit t)
	  (progn
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'mouse-face 'highlight
				       'rear-nonsticky t
				       'keymap org-mouse-map
				       'help-echo "Radio target link"
				       'org-linked-text t))
	    t)))))

(defun org-update-radio-target-regexp ()
  "Find all radio targets in this file and update the regular expression."
  (interactive)
  (when (memq 'radio org-activate-links)
    (setq org-target-link-regexp
	  (org-make-target-link-regexp (org-all-targets 'radio)))
    (org-restart-font-lock)))

(defun org-hide-wide-columns (limit)
  (let (s e)
    (setq s (text-property-any (point) (or limit (point-max))
			       'org-cwidth t))
    (when s
      (setq e (next-single-property-change s 'org-cwidth))
      (add-text-properties s e (org-maybe-intangible '(invisible org-cwidth)))
      (goto-char e)
      t)))

(defun org-restart-font-lock ()
  "Restart font-lock-mode, to force refontification."
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    ;; FIXME: Could font-lock-fontify-buffer be enough???
    (font-lock-mode -1)
    (font-lock-mode 1)))

(defun org-all-targets (&optional radio)
  "Return a list of all targets in this file.
With optional argument RADIO, only find radio targets."
  (let ((re (if radio org-radio-target-regexp org-target-regexp))
	rtn)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(add-to-list 'rtn (downcase (org-match-string-no-properties 1))))
      rtn)))

(defun org-make-target-link-regexp (targets)
  "Make regular expression matching all strings in TARGETS.
The regular expression finds the targets also if there is a line break
between words."
  (and targets
       (concat
	"\\<\\("
	(mapconcat
	 (lambda (x)
	   (while (string-match " +" x)
	     (setq x (replace-match "\\s-+" t t x)))
	   x)
	 targets
	 "\\|")
	"\\)\\>")))

(defun org-activate-tags (limit)
  (if (re-search-forward (org-re "^\\*+.*[ \t]\\(:[[:alnum:]_@:]+:\\)[ \r\n]") limit t)
      (progn
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'mouse-face 'highlight
				   'rear-nonsticky t
				   'keymap org-mouse-map))
	t)))

(defun org-outline-level ()
  (save-excursion
    (looking-at outline-regexp)
    (if (match-beginning 1)
	(+ (org-get-string-indentation (match-string 1)) 1000)
      (1- (- (match-end 0) (match-beginning 0))))))

(defvar org-font-lock-keywords nil)

(defconst org-property-re (org-re "^[ \t]*\\(:\\([[:alnum:]_]+\\):\\)[ \t]*\\(\\S-.*\\)")
  "Regular expression matching a property line.")

(defun org-set-font-lock-defaults ()
  (let* ((em org-fontify-emphasized-text)
	 (lk org-activate-links)
	 (org-font-lock-extra-keywords
	  (list
	   ;; Headlines
	   '("^\\(\\**\\)\\(\\* \\)\\(.*\\)" (1 (org-get-level-face 1))
	     (2 (org-get-level-face 2)) (3 (org-get-level-face 3)))
	   ;; Table lines
	   '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
	     (1 'org-table))
	   ;; Links
	   (if (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
	   (if (memq 'angle lk) '(org-activate-angle-links (0 'org-link t)))
	   (if (memq 'plain lk) '(org-activate-plain-links (0 'org-link t)))
	   (if (memq 'bracket lk) '(org-activate-bracket-links (0 'org-link t)))
	   (if (memq 'radio lk) '(org-activate-target-links (0 'org-link t)))
	   (if (memq 'date lk) '(org-activate-dates (0 'org-date t)))
	   '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
	   '(org-hide-wide-columns (0 nil append))
	   ;; TODO lines
	   (list (concat "^\\*+[ \t]+" org-todo-regexp)
		 '(1 (org-get-todo-face 1) t))
	   ;; DONE
	   (if org-fontify-done-headline
	       (list (concat "^[*]+ +\\<\\("
			     (mapconcat 'regexp-quote org-done-keywords "\\|")
			     "\\)\\(.*\\)")
		     '(2 'org-headline-done t))
	     nil)
	   ;; Priorities
	   (list (concat "\\[#[A-Z0-9]\\]") '(0 'org-special-keyword t))
	   ;; Special keywords
	   (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
	   ;; Emphasis
	   (if em
               (if (featurep 'xemacs)
                   '(org-do-emphasis-faces (0 nil append))
                 '(org-do-emphasis-faces)))
	   ;; Checkboxes, similar to Frank Ruell's org-checklet.el
	   '("^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)"
	     2 'bold prepend)
	   (if org-provide-checkbox-statistics
	       '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
		 (0 (org-get-checkbox-statistics-face) t)))
	   ;; COMMENT
	   (list (concat "^\\*+[ \t]+\\<\\(" org-comment-string
			 "\\|" org-quote-string "\\)\\>")
		 '(1 'org-special-keyword t))
	   '("^#.*" (0 'font-lock-comment-face t))
	   ;; Code
	   '("^[ \t]*\\(:.*\\)" (1 'org-code t))
	   ;; Table internals
	   '("| *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	   '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
	   '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
	   ;; Drawers
	   (list org-drawer-regexp '(0 'org-special-keyword t))
	   (list "^[ \t]*:END:" '(0 'org-special-keyword t))
	   ;; Properties
	   (list org-property-re
		 '(1 'org-special-keyword t)
		 '(3 'org-property-value t))
	   (if org-format-transports-properties-p
	       '("| *\\(<[0-9]+>\\) *" (1 'org-formula t)))
	   '("^\\*+ \\(.*:ARCHIVE:.*\\)" (1 'org-archived prepend))
	   )))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    ;; Now set the full font-lock-keywords
    (org-set-local 'org-font-lock-keywords org-font-lock-extra-keywords)
    (org-set-local 'font-lock-defaults
		   '(org-font-lock-keywords t nil nil backward-paragraph))
    (kill-local-variable 'font-lock-keywords) nil))

(defvar org-m nil)
(defvar org-l nil)
(defvar org-f nil)
(defun org-get-level-face (n)
  "Get the right face for match N in font-lock matching of healdines."
  (setq org-l (- (match-end 2) (match-beginning 1) 1))
  (if org-odd-levels-only (setq org-l (1+ (/ org-l 2))))
  (setq org-f (nth (% (1- org-l) org-n-level-faces) org-level-faces))
  (cond
   ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
   ((eq n 2) org-f)
   (t (if org-level-color-stars-only nil org-f))))

(defun org-get-todo-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (or (cdr (assoc kwd org-todo-keyword-faces))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

(defun org-unfontify-region (beg end &optional maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark buffer-file-name buffer-file-truename)
    (remove-text-properties beg end
			    '(mouse-face t keymap t org-linked-text t
					 rear-nonsticky t
					 invisible t intangible t))))

;;;; Visibility cycling, including org-goto and indirect buffer

;;; Cycling

(defvar org-cycle-global-status nil)
(make-variable-buffer-local 'org-cycle-global-status)
(defvar org-cycle-subtree-status nil)
(make-variable-buffer-local 'org-cycle-subtree-status)

;;;###autoload
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
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is at the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg.
  But only if also the variable `org-cycle-global-at-bob' is t."
  (interactive "P")
  (let* ((outline-regexp
	  (if (and (org-mode-p) org-cycle-include-plain-lists)
	      "\\(?:\\*+ \\|\\([ \t]*\\)\\([-+*]\\|[0-9]+[.)]\\) \\)"
	    outline-regexp))
	 (bob-special (and org-cycle-global-at-bob (bobp)
			   (not (looking-at outline-regexp))))
	 (org-cycle-hook
	  (if bob-special
	      (delq 'org-optimize-window-after-visibility-change
		    (copy-sequence org-cycle-hook))
	    org-cycle-hook))
	 (pos (point)))

    (if (or bob-special (equal arg '(4)))
	;; special case:  use global cycling
	(setq arg t))

    (cond

     ((org-at-table-p 'any)
      ;; Enter the table or move to the next field in the table
      (or (org-table-recognize-table.el)
	  (progn
	    (if arg (org-table-edit-field t)
	      (org-table-justify-field-maybe)
	      (call-interactively 'org-table-next-field)))))

     ((eq arg t) ;; Global cycling

      (cond
       ((and (eq last-command this-command)
	     (eq org-cycle-global-status 'overview))
	;; We just created the overview - now do table of contents
	;; This can be slow in very large buffers, so indicate action
	(message "CONTENTS...")
	(org-content)
	(message "CONTENTS...done")
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
	(org-overview)
	(message "OVERVIEW")
	(setq org-cycle-global-status 'overview)
	(run-hook-with-args 'org-cycle-hook 'overview))))

     ((and org-drawers org-drawer-regexp
	   (save-excursion
	     (beginning-of-line 1)
	     (looking-at org-drawer-regexp)))
      ;; Toggle block visibility
      (org-flag-drawer
       (not (get-char-property (match-end 0) 'invisible))))

     ((integerp arg)
      ;; Show-subtree, ARG levels up from here.
      (save-excursion
	(org-back-to-heading)
	(outline-up-heading (if (< arg 0) (- arg)
			      (- (funcall outline-level) arg)))
	(org-show-subtree)))

     ((and (save-excursion (beginning-of-line 1) (looking-at outline-regexp))
	   (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
      ;; At a heading: rotate between three different views
      (org-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
	;; First, some boundaries
	(save-excursion
	  (org-back-to-heading)
	  (save-excursion
	    (beginning-of-line 2)
	    (while (and (not (eobp)) ;; this is like `next-line'
			(get-char-property (1- (point)) 'invisible))
	      (beginning-of-line 2)) (setq eol (point)))
	  (outline-end-of-heading)   (setq eoh (point))
	  (org-end-of-subtree t)
	  (unless (eobp)
	    (skip-chars-forward " \t\n")
	    (beginning-of-line 1) ; in case this is an item
	    )
	  (setq eos (1- (point))))
	;; Find out what to do next and set `this-command'
	(cond
	 ((= eos eoh)
	  ;; Nothing is hidden behind this heading
	  (message "EMPTY ENTRY")
	  (setq org-cycle-subtree-status nil)
	  (save-excursion
	    (goto-char eos)
	    (outline-next-heading)
	    (if (org-invisible-p) (org-flag-heading nil))))
	 ((or (>= eol eos)
	      (not (string-match "\\S-" (buffer-substring eol eos))))
	  ;; Entire subtree is hidden in one line: open it
	  (org-show-entry)
	  (show-children)
	  (message "CHILDREN")
	  (save-excursion
	    (goto-char eos)
	    (outline-next-heading)
	    (if (org-invisible-p) (org-flag-heading nil)))
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

     ((org-try-cdlatex-tab))

     ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
	   (or (not (bolp))
	       (not (looking-at outline-regexp))))
      (call-interactively (global-key-binding "\t")))

     ((if (and (memq org-cycle-emulate-tab '(white whitestart))
	       (save-excursion (beginning-of-line 1) (looking-at "[ \t]*"))
	       (or (and (eq org-cycle-emulate-tab 'white)
			(= (match-end 0) (point-at-eol)))
		   (and (eq org-cycle-emulate-tab 'whitestart)
			(>= (match-end 0) pos))))
	  t
	(eq org-cycle-emulate-tab t))
      (if (and (looking-at "[ \n\r\t]")
	       (string-match "^[ \t]*$" (buffer-substring
					 (point-at-bol) (point))))
	  (progn
	    (beginning-of-line 1)
	    (and (looking-at "[ \t]+") (replace-match ""))))
      (call-interactively (global-key-binding "\t")))

     (t (save-excursion
	  (org-back-to-heading)
	  (org-cycle))))))

;;;###autoload
(defun org-global-cycle (&optional arg)
  "Cycle the global visibility.  For details see `org-cycle'."
  (interactive "P")
  (let ((org-cycle-include-plain-lists
	 (if (org-mode-p) org-cycle-include-plain-lists nil)))
    (if (integerp arg)
	(progn
	  (show-all)
	  (hide-sublevels arg)
	  (setq org-cycle-global-status 'contents))
      (org-cycle '(4)))))

(defun org-overview ()
  "Switch to overview mode, shoing only top-level headlines.
Really, this shows all headlines with level equal or greater than the level
of the first headline in the buffer.  This is important, because if the
first headline is not level one, then (hide-sublevels 1) gives confusing
results."
  (interactive)
  (let ((level (save-excursion
		 (goto-char (point-min))
		 (if (re-search-forward (concat "^" outline-regexp) nil t)
		     (progn
		       (goto-char (match-beginning 0))
		       (funcall outline-level))))))
    (and level (hide-sublevels level))))

(defun org-content (&optional arg)
  "Show all headlines in the buffer, like a table of contents.
With numerical argument N, show content up to level N."
  (interactive "P")
  (save-excursion
    ;; Visit all headings and show their offspring
    (and (integerp arg) (org-overview))
    (goto-char (point-max))
    (catch 'exit
      (while (and (progn (condition-case nil
			     (outline-previous-visible-heading 1)
			   (error (goto-char (point-min))))
			 t)
		  (looking-at outline-regexp))
	(if (integerp arg)
	    (show-children (1- arg))
	  (show-branches))
	(if (bobp) (throw 'exit nil))))))


(defun org-optimize-window-after-visibility-change (state)
  "Adjust the window after a change in outline visibility.
This function is the default value of the hook `org-cycle-hook'."
  (when (get-buffer-window (current-buffer))
    (cond
;     ((eq state 'overview) (org-first-headline-recenter 1))
;     ((eq state 'overview) (org-beginning-of-line))
     ((eq state 'content)  nil)
     ((eq state 'all)      nil)
     ((eq state 'folded)   nil)
     ((eq state 'children) (or (org-subtree-end-visible-p) (recenter 1)))
     ((eq state 'subtree)  (or (org-subtree-end-visible-p) (recenter 1))))))


(defun org-cycle-show-empty-lines (state)
  "Show empty lines above all visible headlines.
The region to be covered depends on STATE when called through
`org-cycle-hook'.  Lisp program can use t for STATE to get the
entire buffer covered.  Note that an empty line is only shown if there
are at least `org-cycle-separator-lines' empty lines before the headeline."
  (when (> org-cycle-separator-lines 0)
    (save-excursion
      (let* ((n org-cycle-separator-lines)
	     (re (cond
		  ((= n 1) "\\(\n[ \t]*\n\\*+\\) ")
		  ((= n 2) "^[ \t]*\\(\n[ \t]*\n\\*+\\) ")
		  (t (let ((ns (number-to-string (- n 2))))
		       (concat "^\\(?:[ \t]*\n\\)\\{" ns "," ns "\\}"
			       "[ \t]*\\(\n[ \t]*\n\\*+\\) ")))))
	     beg end)
	(cond
	 ((memq state '(overview contents t))
	  (setq beg (point-min) end (point-max)))
	 ((memq state '(children folded))
	  (setq beg (point) end (progn (org-end-of-subtree t t)
				       (beginning-of-line 2)
				       (point)))))
	(when beg
	  (goto-char beg)
	  (while (re-search-forward re end t)
	    (if (not (get-char-property (match-end 1) 'invisible))
		(outline-flag-region
		 (match-beginning 1) (match-end 1) nil)))))))
  ;; Never hide empty lines at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (outline-end-of-heading)
    (if (and (looking-at "[ \t\n]+")
	     (= (match-end 0) (point-max)))
	(outline-flag-region (point) (match-end 0) nil))))

(defun org-subtree-end-visible-p ()
  "Is the end of the current subtree visible?"
  (pos-visible-in-window-p
   (save-excursion (org-end-of-subtree t) (point))))

(defun org-first-headline-recenter (&optional N)
  "Move cursor to the first headline and recenter the headline.
Optional argument N means, put the headline into the Nth line of the window."
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\(" outline-regexp "\\)") nil t)
    (beginning-of-line)
    (recenter (prefix-numeric-value N))))

;;; Org-goto

(defvar org-goto-window-configuration nil)
(defvar org-goto-marker nil)
(defvar org-goto-map
  (let ((map (make-sparse-keymap)))
    (let ((cmds '(isearch-forward isearch-backward)) cmd)
      (while (setq cmd (pop cmds))
	(substitute-key-definition cmd cmd map global-map)))
    (org-defkey map "\C-m"     'org-goto-ret)
    (org-defkey map [(left)]   'org-goto-left)
    (org-defkey map [(right)]  'org-goto-right)
    (org-defkey map [(?q)]     'org-goto-quit)
    (org-defkey map [(control ?g)] 'org-goto-quit)
    (org-defkey map "\C-i" 'org-cycle)
    (org-defkey map [(tab)] 'org-cycle)
    (org-defkey map [(down)] 'outline-next-visible-heading)
    (org-defkey map [(up)] 'outline-previous-visible-heading)
    (org-defkey map "n" 'outline-next-visible-heading)
    (org-defkey map "p" 'outline-previous-visible-heading)
    (org-defkey map "f" 'outline-forward-same-level)
    (org-defkey map "b" 'outline-backward-same-level)
    (org-defkey map "u" 'outline-up-heading)
    (org-defkey map "\C-c\C-n" 'outline-next-visible-heading)
    (org-defkey map "\C-c\C-p" 'outline-previous-visible-heading)
    (org-defkey map "\C-c\C-f" 'outline-forward-same-level)
    (org-defkey map "\C-c\C-b" 'outline-backward-same-level)
    (org-defkey map "\C-c\C-u" 'outline-up-heading)
    ;; FIXME: Could we use suppress-keymap?
    (let ((l '(1 2 3 4 5 6 7 8 9 0)))
      (while l (org-defkey map (int-to-string (pop l)) 'digit-argument)))
    map))

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
	  (org-mark-ring-push org-goto-start-pos)
	  (goto-char selected-point)
	  (if (or (org-invisible-p) (org-invisible-p2))
	      (org-show-context 'org-goto)))
      (error "Quit"))))

(defvar org-selected-point nil) ; dynamically scoped parameter

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
	      (org-startup-align-all-tables nil))
	  (org-mode))
	(setq buffer-read-only t)
	(if (and (boundp 'org-goto-start-pos)
		 (integer-or-marker-p org-goto-start-pos))
	    (let ((org-show-hierarchy-above t)
		  (org-show-siblings t)
		  (org-show-following-heading t))
	      (goto-char org-goto-start-pos)
	      (and (org-invisible-p) (org-show-context)))
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

(defun org-goto-ret (&optional arg)
  "Finish `org-goto' by going to the new location."
  (interactive "P")
  (setq org-selected-point (point)
	current-prefix-arg arg)
  (throw 'exit nil))

(defun org-goto-left ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-on-heading-p)
      (progn
	(beginning-of-line 1)
	(setq org-selected-point (point)
	      current-prefix-arg (- (match-end 0) (match-beginning 0)))
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-right ()
  "Finish `org-goto' by going to the new location."
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
  "Finish `org-goto' without cursor motion."
  (interactive)
  (setq org-selected-point nil)
  (throw 'exit nil))

;;; Indirect buffer display of subtrees

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.
With numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.
Normally this command removes the indirect buffer previously made
with this command.  However, when called with a C-u prefix, the last buffer
is kept so that you can work with several indirect buffers at the same time.
If `org-indirect-buffer-display' is `dedicated-frame', the C-u prefix also
requests that a new frame be made for the new buffer, so that the dedicated
frame is not changed."
  (interactive "P")
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf)
    (save-excursion
      (org-back-to-heading t)
      (when (numberp arg)
	(setq level (org-outline-level))
	(if (< arg 0) (setq arg (+ level arg)))
	(while (> (setq level (org-outline-level)) arg)
	  (outline-up-heading 1 t)))
      (setq beg (point)
	    heading (org-get-heading))
      (org-end-of-subtree t) (setq end (point)))
    (if (and (not arg)
	     (buffer-live-p org-last-indirect-buffer))
	(kill-buffer org-last-indirect-buffer))
    (setq ibuf (org-get-indirect-buffer cbuf)
	  org-last-indirect-buffer ibuf)
    (cond
     ((or (eq org-indirect-buffer-display 'new-frame)
	  (and arg (eq org-indirect-buffer-display 'dedicated-frame)))
      (select-frame (make-frame))
      (delete-other-windows)
      (switch-to-buffer ibuf)
      (org-set-frame-title heading))
     ((eq org-indirect-buffer-display 'dedicated-frame)
      (raise-frame
       (select-frame (or (and org-indirect-dedicated-frame
			      (frame-live-p org-indirect-dedicated-frame)
			      org-indirect-dedicated-frame)
			 (setq org-indirect-dedicated-frame (make-frame)))))
      (delete-other-windows)
      (switch-to-buffer ibuf)
      (org-set-frame-title (concat "Indirect: " heading)))
     ((eq org-indirect-buffer-display 'current-window)
      (switch-to-buffer ibuf))
     ((eq org-indirect-buffer-display 'other-window)
      (pop-to-buffer ibuf))
     (t (error "Invalid value.")))
    (if (featurep 'xemacs)
        (save-excursion (org-mode) (turn-on-font-lock)))
    (narrow-to-region beg end)
    (show-all)
    (goto-char pos)
    (and (window-live-p cwin) (select-window cwin))))

(defun org-get-indirect-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (while (buffer-live-p
	    (get-buffer (setq bname (concat base "-" (number-to-string n)))))
      (setq n (1+ n)))
    (condition-case nil
        (make-indirect-buffer buffer bname 'clone)
      (error (make-indirect-buffer buffer bname)))))

(defun org-set-frame-title (title)
  "Set the title of the current frame to the string TITLE."
  ;; FIXME: how to name a single frame in XEmacs???
  (unless (featurep 'xemacs)
    (modify-frame-parameters (selected-frame) (list (cons 'name title)))))

;;;; Structure editing

;;; Inserting headlines

(defun org-insert-heading (&optional force-heading)
  "Insert a new heading or item with same depth at point.
If point is in a plain list and FORCE-HEADING is nil, create a new list item.
If point is at the beginning of a headline, insert a sibling before the
current headline.  If point is in the middle of a headline, split the headline
at that position and make the rest of the headline part of the sibling below
the current headline."
  (interactive "P")
  (if (= (buffer-size) 0)
      (insert "\n* ")
    (when (or force-heading (not (org-insert-item)))
      (let* ((head (save-excursion
		     (condition-case nil
			 (progn
			   (org-back-to-heading)
			   (match-string 0))
		       (error "*"))))
	     (blank (cdr (assq 'heading org-blank-before-new-entry)))
	     pos)
	(cond
	 ((and (org-on-heading-p) (bolp)
	       (or (bobp)
		   (save-excursion (backward-char 1) (not (org-invisible-p)))))
	  (open-line (if blank 2 1)))
	 ((and (bolp)
	       (or (bobp)
		   (save-excursion
		     (backward-char 1) (not (org-invisible-p)))))
	  nil)
	 (t (newline (if blank 2 1))))
	(insert head) (just-one-space)
	(setq pos (point))
	(end-of-line 1)
	(unless (= (point) pos) (just-one-space) (backward-delete-char 1))
	(run-hooks 'org-insert-heading-hook)))))

(defun org-insert-todo-heading (arg)
  "Insert a new heading with the same level and TODO state as current heading.
If the heading has no TODO state, or if the state is DONE, use the first
state (TODO by default).  Also with prefix arg, force first state."
  (interactive "P")
  (when (not (org-insert-item 'checkbox))
    (org-insert-heading)
    (save-excursion
      (org-back-to-heading)
      (outline-previous-heading)
      (looking-at org-todo-line-regexp))
    (if (or arg
	    (not (match-beginning 2))
	    (member (match-string 2) org-done-keywords))
	(insert (car org-todo-keywords-1) " ")
      (insert (match-string 2) " "))))

(defun org-insert-subheading (arg)
  "Insert a new subheading and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-heading arg)
  (cond
   ((org-on-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item 1))))

(defun org-insert-todo-subheading (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-todo-heading arg)
  (cond
   ((org-on-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item 1))))
  
;;; Promotion and Demotion

(defun org-promote-subtree ()
  "Promote the entire subtree.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-map-tree 'org-promote))
  (org-fix-position-after-promote))

(defun org-demote-subtree ()
  "Demote the entire subtree.  See `org-demote'.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-map-tree 'org-demote))
  (org-fix-position-after-promote))


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
  (let ((pos (point)))
    (when (save-excursion
	    (beginning-of-line 1)
	    (looking-at org-todo-line-regexp)
	    (or (equal pos (match-end 1)) (equal pos (match-end 2))))
      (cond ((eobp) (insert " "))
	    ((eolp) (insert " "))
	    ((equal (char-after) ?\ ) (forward-char 1))))))

(defun org-reduced-level (l)
  (if org-odd-levels-only (1+ (floor (/ l 2))) l))

(defun org-get-legal-level (level &optional change)
  "Rectify a level change under the influence of `org-odd-levels-only'
LEVEL is a current level, CHANGE is by how much the level should be
modified.  Even if CHANGE is nil, LEVEL may be returned modified because
even level numbers will become the next higher odd number."
  (if org-odd-levels-only
      (cond ((or (not change) (= 0 change)) (1+ (* 2 (/ level 2))))
	    ((> change 0) (1+ (* 2 (/ (+ level (* 2 change)) 2))))
	    ((< change 0) (max 1 (1+ (* 2 (/ (+ level (* 2 change)) 2))))))
    (max 1 (+ level change))))

(defun org-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (up-head (concat (make-string (org-get-legal-level level -1) ?*) " "))
	 (diff (abs (- level (length up-head) -1))))
    (if (= level 1) (error "Cannot promote to level 0. UNDO to recover if necessary"))
    (replace-match up-head nil t)
    ;; Fixup tag positioning
    (and org-auto-align-tags (org-set-tags nil t))
    (if org-adapt-indentation (org-fixup-indentation (- diff)))))

(defun org-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (down-head (concat (make-string (org-get-legal-level level 1) ?*) " "))
	 (diff (abs (- level (length down-head) -1))))
    (replace-match down-head nil t)
    ;; Fixup tag positioning
    (and org-auto-align-tags (org-set-tags nil t))
    (if org-adapt-indentation (org-fixup-indentation diff))))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading)
  (let ((level (funcall outline-level)))
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

(defun org-fixup-indentation (diff)
  "Change the indentation in the current entry by DIFF
However, if any line in the current entry has no indentation, or if it
would end up with no indentation after the change, nothing at all is done."
  (save-excursion
    (let ((end (save-excursion (outline-next-heading)
			       (point-marker)))
	  (prohibit (if (> diff 0)
			"^\\S-"
		      (concat "^ \\{0," (int-to-string (- diff)) "\\}\\S-")))
	  col)
      (unless (save-excursion (end-of-line 1)
			      (re-search-forward prohibit end t))
	(while (re-search-forward "^[ \t]+" end t)
	  (goto-char (match-end 0))
	  (setq col (current-column))
	  (if (< diff 0) (replace-match ""))
	  (indent-to (+ diff col))))
      (move-marker end nil))))

(defun org-convert-to-odd-levels ()
  "Convert an org-mode file with all levels allowed to one with odd levels.
This will leave level 1 alone, convert level 2 to level 3, level 3 to
level 5 etc."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd? ")
    (let ((org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (- (length (match-string 0)) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-demote))
	  (end-of-line 1))))))


(defun org-convert-to-oddeven-levels ()
  "Convert an org-mode file with only odd levels to one with odd and even levels.
This promotes level 3 to level 2, level 5 to level 3 etc.  If the file contains a
section with an even level, conversion would destroy the structure of the file.  An error
is signaled in this case."
  (interactive)
  (goto-char (point-min))
  ;; First check if there are no even levels
  (when (re-search-forward "^\\(\\*\\*\\)+ " nil t)
    (org-show-context t)
    (error "Not all levels are odd in this file.  Conversion not possible."))
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd-even? ")
    (let ((org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (/ (length (1- (match-string 0))) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-promote))
	  (end-of-line 1))))))

(defun org-tr-level (n)
  "Make N odd if required."
  (if org-odd-levels-only (1+ (/ n 2)) n))

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
    (or (bolp) (insert "\n"))
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
If CUT is non-nil, actually cut the subtree."
  (interactive)
  (let (beg end folded)
    (if (interactive-p)
	(org-back-to-heading nil) ; take what looks like a subtree
      (org-back-to-heading t)) ; take what is really there
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
  (let* ((txt (or tree (and kill-ring (current-kill 0))))
	 (^re (concat "^\\(" outline-regexp "\\)"))
	 (re  (concat "\\(" outline-regexp "\\)"))
	 (^re_ (concat "\\(" outline-regexp "\\)[  \t]*"))

	 (old-level (if (string-match ^re txt)
			(- (match-end 0) (match-beginning 0) 1)
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
	 (org-odd-levels-only nil)
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
    (if (string-match "[ \t\r\n]+\\'" txt)
	(setq txt (replace-match "\n" t t txt)))
    (insert txt)
    (setq end (point))
    (if (looking-at "[ \t\r\n]+")
	(replace-match "\n"))
    (goto-char beg)
    ;; Shift if necessary
    (unless (= shift 0)
      (save-restriction
	(narrow-to-region beg end)
	(while (not (= shift 0))
	  (org-map-region func (point-min) (point-max))
	  (setq shift (+ delta shift)))
	(goto-char (point-min))))
    (when (interactive-p)
      (message "Clipboard pasted as level %d subtree" new-level))
    (if (and kill-ring
	     (eq org-subtree-clip (current-kill 0))
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
  (let* ((kill (or txt (and kill-ring (current-kill 0)) ""))
	 (start-level (and kill
			   (string-match (concat "\\`" outline-regexp) kill)
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

(defun org-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (narrow-to-region
     (progn (org-back-to-heading) (point))
     (progn (org-end-of-subtree t t) (point)))))


;;; Outline Sorting

(defun org-sort (with-case)
  "Call `org-sort-entries' or `org-table-sort-lines', depending on context."
  (interactive "P")
  (if (org-at-table-p)
      (org-call-with-arg 'org-table-sort-lines with-case)
    (org-call-with-arg 'org-sort-entries with-case)))

(defun org-sort-entries (&optional with-case sorting-type)
  "Sort entries on a certain level of an outline tree.
If there is an active region, the entries in the region are sorted.
Else, if the cursor is before the first entry, sort the top-level items.
Else, the children of the entry at point are sorted.

Sorting can be alphabetically, numerically, and by date/time as given by
the first time stamp in the entry.  The command prompts for the sorting
type unless it has been given to the function through the SORTING-TYPE
argument, which needs to a character, any of (?n ?N ?a ?A ?t ?T).

Comparing entries ignores case by default.  However, with an optional argument
WITH-CASE, the sorting considers case as well.  With two prefix arguments
`C-u C-u', sorting is case-sensitive and duplicate entries will be removed."
  (interactive "P")
  (let ((unique (equal with-case '(16)))
	start beg end entries stars re re2 p nentries (nremoved 0)
	last txt what)
    ;; Find beginning and end of region to sort
    (cond
     ((org-region-active-p)
      ;; we will sort the region
      (setq end (region-end)
	    what "region")
      (goto-char (region-beginning))
      (if (not (org-on-heading-p)) (outline-next-heading))
      (setq start (point)))
     ((or (org-on-heading-p)
	  (condition-case nil (progn (org-back-to-heading) t) (error nil)))
      ;; we will sort the children of the current headline
      (org-back-to-heading)
      (setq start (point) end (org-end-of-subtree) what "children")
      (goto-char start)
      (show-subtree)
      (outline-next-heading))
     (t
      ;; we will sort the top-level entries in this file
      (goto-char (point-min))
      (or (org-on-heading-p) (outline-next-heading))
      (setq start (point) end (point-max) what "top-level")
      (goto-char start)
      (show-all)))
    (setq beg (point))
    (if (>= (point) end) (error "Nothing to sort"))
    (looking-at "\\(\\*+\\)")
    (setq stars (match-string 1)
	  re (concat "^" (regexp-quote stars) " +")
	  re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[^*]")
	  txt (buffer-substring beg end))
    (if (not (equal (substring txt -1) "\n")) (setq txt (concat txt "\n")))
    (if (and (not (equal stars "*")) (string-match re2 txt))
	(error "Region to sort contains a level above the first entry"))
    ;; Make a list that can be sorted.
    ;; The car is the string for comparison, the cdr is the subtree
    (message "Sorting entries...")
    (setq entries
	  (mapcar
	   (lambda (x)
	     (string-match "^.*\\(\n.*\\)?" x) ; take two lines
	     (cons (match-string 0 x) x))
	   (org-split-string txt re)))

    ;; Sort the list
    (save-excursion
      (goto-char start)
      (setq entries (org-do-sort entries what with-case sorting-type)))

    ;; Delete the old stuff
    (goto-char beg)
    (kill-region beg end)
    (setq nentries (length entries))
    ;; Insert the sorted entries, and remove duplicates if this is required
    (while (setq p (pop entries))
      (if (and unique (equal last (setq last (org-trim (cdr p)))))
	  (setq nremoved (1+ nremoved)) ; same entry as before, skip it
	(insert stars " " (cdr p))))
    (goto-char start)
    (message "Sorting entries...done (%d entries%s)"
	     nentries
	     (if unique (format ", %d duplicates removed" nremoved) ""))))

(defvar org-priority-regexp) ; defined later in the file

(defun org-do-sort (table what &optional with-case sorting-type)
  "Sort TABLE of WHAT according to SORTING-TYPE.
The user will be prompted for the SORTING-TYPE if the call to this
function does not specify it.  WHAT is only for the prompt, to indicate
what is being sorted.  The sorting key will be extracted from
the car of the elements of the table.
If WITH-CASE is non-nil, the sorting will be case-sensitive."
  (unless sorting-type
    (message
     "Sort %s: [a]lphabetic. [n]umeric. [t]ime  [p]riority.  A/N/T/P means reversed:"
     what)
    (setq sorting-type (read-char-exclusive)))
  (let ((dcst (downcase sorting-type))
	extractfun comparefun)
    ;; Define the appropriate functions
    (cond
     ((= dcst ?n)
      (setq extractfun 'string-to-number
	    comparefun (if (= dcst sorting-type) '< '>)))
     ((= dcst ?a)
      (setq extractfun (if with-case 'identity 'downcase)
	    comparefun (if (= dcst sorting-type)
			   'string<
			 (lambda (a b) (and (not (string< a b))
					    (not (string= a b)))))))
     ((= dcst ?t)
      (setq extractfun
	    (lambda (x)
	      (if (string-match org-ts-regexp x)
		  (time-to-seconds
		   (org-time-string-to-time (match-string 0 x)))
		0))
	    comparefun (if (= dcst sorting-type) '< '>)))
     ((= dcst ?p)
      (setq extractfun
	    (lambda (x)
	      (if (string-match org-priority-regexp x)
		  (string-to-char (match-string 2 x))
		org-default-priority))
	    comparefun (if (= dcst sorting-type) '< '>)))		  
     (t (error "Invalid sorting type `%c'" sorting-type)))

    (sort (mapcar (lambda (x) (cons (funcall extractfun (car x)) (cdr x)))
		  table)
	  (lambda (a b) (funcall comparefun (car a) (car b))))))

;;;; Plain list items, including checkboxes

;;; Plain list items

(defun org-at-item-p ()
  "Is point in a line starting a hand-formatted item?"
  (let ((llt org-plain-list-ordered-item-terminator))
    (save-excursion
      (goto-char (point-at-bol))
      (looking-at
       (cond
	((eq llt t)  "\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
	((= llt ?.)  "\\([ \t]*\\([-+]\\|\\([0-9]+\\.\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
	((= llt ?\)) "\\([ \t]*\\([-+]\\|\\([0-9]+)\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
	(t (error "Invalid value of `org-plain-list-ordered-item-terminator'")))))))

(defun org-in-item-p ()
  "It the cursor inside a plain list item.
Does not have to be the first line."
  (save-excursion
    (condition-case nil
	(progn
	  (org-beginning-of-item)
	  (org-at-item-p)
	  t)
      (error nil))))

(defun org-insert-item (&optional checkbox)
  "Insert a new item at the current level.
Return t when things worked, nil when we are not in an item."
  (when (save-excursion
	  (condition-case nil
	      (progn
		(org-beginning-of-item)
		(org-at-item-p)
		(if (org-invisible-p) (error "Invisible item"))
		t)
	    (error nil)))
    (let* ((bul (match-string 0))
	   (eow (save-excursion (beginning-of-line 1) (looking-at "[ \t]*")
				(match-end 0)))
	   (blank (cdr (assq 'plain-list-item org-blank-before-new-entry)))
	   pos)
      (cond
       ((and (org-at-item-p) (<= (point) eow))
	;; before the bullet
	(beginning-of-line 1)
	(open-line (if blank 2 1)))
       ((<= (point) eow)
	(beginning-of-line 1))
       (t (newline (if blank 2 1))))
      (insert bul (if checkbox "[ ]" ""))
      (just-one-space)
      (setq pos (point))
      (end-of-line 1)
      (unless (= (point) pos) (just-one-space) (backward-delete-char 1)))
    (org-maybe-renumber-ordered-list)
    (and checkbox (org-update-checkbox-count-maybe))
    t))

;;; Checkboxes

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t")
	 (looking-at "\\[[- X]\\]"))))

(defun org-toggle-checkbox (&optional arg)
  "Toggle the checkbox in the current line."
  (interactive "P")
  (catch 'exit
    (let (beg end status (firstnew 'unknown))
      (cond
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       ((org-on-heading-p)
	(setq beg (point) end (save-excursion (outline-next-heading) (point))))
       ((org-at-item-checkbox-p)
	(save-excursion
	  (replace-match
	   (cond (arg "[-]")
		 ((member (match-string 0) '("[ ]" "[-]")) "[X]")
		 (t "[ ]"))
	   t t))
	(throw 'exit t))
       (t (error "Not at a checkbox or heading, and no active region")))
      (save-excursion
	(goto-char beg)
	(while (< (point) end)
	  (when (org-at-item-checkbox-p)
	    (setq status (equal (match-string 0) "[X]"))
	    (when (eq firstnew 'unknown)
	      (setq firstnew (not status)))
	    (replace-match
	     (if (if arg (not status) firstnew) "[X]" "[ ]") t t))
	  (beginning-of-line 2)))))
  (org-update-checkbox-count-maybe))

(defun org-update-checkbox-count-maybe ()
  "Update checkbox statistics unless turned off by user."
  (when org-provide-checkbox-statistics
    (org-update-checkbox-count)))

(defun org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update them
with the current numbers.  With optional prefix argument ALL, do this for
the whole buffer."
  (interactive "P")
  (save-excursion
    (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) ; Emacs 21
	   (beg (condition-case nil
		    (progn (outline-back-to-heading) (point))
		  (error (point-min))))
	   (end (move-marker (make-marker)
			     (progn (outline-next-heading) (point))))
	   (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
	   (re-box "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	   b1 e1 f1 c-on c-off lim (cstat 0))
      (when all
	(goto-char (point-min))
	(outline-next-heading)
	(setq beg (point) end (point-max)))
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq cstat (1+ cstat)
	      b1 (match-beginning 0)
	      e1 (match-end 0)
	      f1 (match-beginning 1)
	      lim (cond
		   ((org-on-heading-p) (outline-next-heading) (point))
		   ((org-at-item-p) (org-end-of-item) (point))
		   (t nil))
	      c-on 0 c-off 0)
	(goto-char e1)
	(when lim
	  (while (re-search-forward re-box lim t)
	    (if (member (match-string 2) '("[ ]" "[-]"))
		(setq c-off (1+ c-off))
	      (setq c-on (1+ c-on))))
;	  (delete-region b1 e1)
	  (goto-char b1)
	  (insert (if f1
		      (format "[%d%%]" (/ (* 100 c-on) (max 1 (+ c-on c-off))))
		    (format "[%d/%d]" c-on (+ c-on c-off))))
	  (and (looking-at "\\[.*?\\]")
	       (replace-match ""))))
      (when (interactive-p)
	(message "Checkbox satistics updated %s (%d places)"
		 (if all "in entire file" "in current outline entry") cstat)))))

(defun org-get-checkbox-statistics-face ()
  "Select the face for checkbox statistics.
The face will be `org-done' when all relevant boxes are checked.  Otherwise
it will be `org-todo'."
  (if (match-end 1)
      (if (equal (match-string 1) "100%") 'org-done 'org-todo)
    (if (and (> (match-end 2) (match-beginning 2))
	     (equal (match-string 2) (match-string 3)))
	'org-done
      'org-todo)))

(defun org-get-indentation (&optional line)
  "Get the indentation of the current line, interpreting tabs.
When LINE is given, assume it represents a line and compute its indentation."
  (if line
      (if (string-match "^ *" (org-remove-tabs line))
	  (match-end 0))
    (save-excursion
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (current-column))))

(defun org-remove-tabs (s &optional width)
  "Replace tabulators in S with spaces.
Assumes that s is a single line, starting in column 0."
  (setq width (or width tab-width))
  (while (string-match "\t" s)
    (setq s (replace-match
	     (make-string
	      (- (* width (/ (+ (match-beginning 0) width) width))
		 (match-beginning 0)) ?\ )
	     t t s)))
  s)

(defun org-fix-indentation (line ind)
  "Fix indentation in LINE.
IND is a cons cell with target and minimum indentation.
If the current indenation in LINE is smaller than the minimum,
leave it alone.  If it is larger than ind, set it to the target."
  (let* ((l (org-remove-tabs line))
	 (i (org-get-indentation l))
	 (i1 (car ind)) (i2 (cdr ind)))
    (if (>= i i2) (setq l (substring line i2)))
    (if (> i1 0)
	(concat (make-string i1 ?\ ) l)
      l)))

(defcustom org-empty-line-terminates-plain-lists nil
  "Non-nil means, an empty line ends all plain list levels.
When nil, empty lines are part of the preceeding item."
  :group 'org-plain-lists
  :type 'boolean)

(defun org-beginning-of-item ()
  "Go to the beginning of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let ((pos (point))
	(limit (save-excursion
		 (condition-case nil
		     (progn
		       (org-back-to-heading)
		       (beginning-of-line 2) (point))
		   (error (point-min)))))
	(ind-empty (if org-empty-line-terminates-plain-lists 0 10000))
	ind ind1)
    (if (org-at-item-p)
	(beginning-of-line 1)
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (setq ind (current-column))
      (if (catch 'exit
	    (while t
	      (beginning-of-line 0)
	      (if (or (bobp) (< (point) limit)) (throw 'exit nil))

	      (if (looking-at "[ \t]*$")
		  (setq ind1 ind-empty)
		(skip-chars-forward " \t")
		(setq ind1 (current-column)))
	      (if (< ind1 ind)
		  (progn (beginning-of-line 1) (throw 'exit (org-at-item-p))))))
	  nil
	(goto-char pos)
	(error "Not in an item")))))

(defun org-end-of-item ()
  "Go to the end of the current hand-formatted item.
If the cursor is not in an item, throw an error."
  (interactive)
  (let* ((pos (point))
	 ind1
	 (ind-empty (if org-empty-line-terminates-plain-lists 0 10000))
	 (limit (save-excursion (outline-next-heading) (point)))
	 (ind (save-excursion
		(org-beginning-of-item)
		(skip-chars-forward " \t")
		(current-column)))
	 (end (catch 'exit
		(while t
		  (beginning-of-line 2)
		  (if (eobp) (throw 'exit (point)))
		  (if (>= (point) limit) (throw 'exit (point-at-bol)))
		  (if (looking-at "[ \t]*$")
		      (setq ind1 ind-empty)
		    (skip-chars-forward " \t")
		    (setq ind1 (current-column)))
		  (if (<= ind1 ind)
		      (throw 'exit (point-at-bol)))))))
    (if end
	(goto-char end)
      (goto-char pos)
      (error "Not in an item"))))

(defun org-next-item ()
  "Move to the beginning of the next item in the current plain list.
Error if not at a plain list, or if this is the last item in the list."
  (interactive)
  (let (ind ind1 (pos (point)))
    (org-beginning-of-item)
    (setq ind (org-get-indentation))
    (org-end-of-item)
    (setq ind1 (org-get-indentation))
    (unless (and (org-at-item-p) (= ind ind1))
      (goto-char pos)
      (error "On last item"))))

(defun org-previous-item ()
  "Move to the beginning of the previous item in the current plain list.
Error if not at a plain list, or if this is the first item in the list."
  (interactive)
  (let (beg ind ind1 (pos (point)))
    (org-beginning-of-item)
    (setq beg (point))
    (setq ind (org-get-indentation))
    (goto-char beg)
    (catch 'exit
      (while t
	(beginning-of-line 0)
	(if (looking-at "[ \t]*$")
	    nil
	  (if (<= (setq ind1 (org-get-indentation)) ind)
	      (throw 'exit t)))))
    (condition-case nil
	(if (or (not (org-at-item-p))
		(< ind1 (1- ind)))
	    (error "")
	  (org-beginning-of-item))
      (error (goto-char pos)
	     (error "On first item")))))

(defun org-move-item-down ()
  "Move the plain list item at point down, i.e. swap with following item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (let (beg end ind ind1 (pos (point)) txt)
    (org-beginning-of-item)
    (setq beg (point))
    (setq ind (org-get-indentation))
    (org-end-of-item)
    (setq end (point))
    (setq ind1 (org-get-indentation))
    (if (and (org-at-item-p) (= ind ind1))
	(progn
	  (org-end-of-item)
	  (setq txt (buffer-substring beg end))
	  (save-excursion
	    (delete-region beg end))
	  (setq pos (point))
	  (insert txt)
	  (goto-char pos)
	  (org-maybe-renumber-ordered-list))
      (goto-char pos)
      (error "Cannot move this item further down"))))

(defun org-move-item-up (arg)
  "Move the plain list item at point up, i.e. swap with previous item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive "p")
  (let (beg end ind ind1 (pos (point)) txt)
    (org-beginning-of-item)
    (setq beg (point))
    (setq ind (org-get-indentation))
    (org-end-of-item)
    (setq end (point))
    (goto-char beg)
    (catch 'exit
      (while t
	(beginning-of-line 0)
	(if (looking-at "[ \t]*$")
	    (if org-empty-line-terminates-plain-lists
		(progn
		  (goto-char pos)
		  (error "Cannot move this item further up"))
	      nil)
	  (if (<= (setq ind1 (org-get-indentation)) ind)
	      (throw 'exit t)))))
    (condition-case nil
	(org-beginning-of-item)
      (error (goto-char beg)
	     (error "Cannot move this item further up")))
    (setq ind1 (org-get-indentation))
    (if (and (org-at-item-p) (= ind ind1))
	(progn
	  (setq txt (buffer-substring beg end))
	  (save-excursion
	    (delete-region beg end))
	  (setq pos (point))
	  (insert txt)
	  (goto-char pos)
	  (org-maybe-renumber-ordered-list))
      (goto-char pos)
      (error "Cannot move this item further up"))))

(defun org-maybe-renumber-ordered-list ()
  "Renumber the ordered list at point if setup allows it.
This tests the user option `org-auto-renumber-ordered-lists' before
doing the renumbering."
  (interactive)
  (when (and org-auto-renumber-ordered-lists
	     (org-at-item-p))
    (if (match-beginning 3)
	(org-renumber-ordered-list 1)
      (org-fix-bullet-type 1))))

(defun org-maybe-renumber-ordered-list-safe ()
  (condition-case nil
      (save-excursion
	(org-maybe-renumber-ordered-list))
    (error nil)))

(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'  ->  `+'  ->  `*'  ->  `1.'  ->  `1)'

If WHICH is a string, use that as the new bullet.  If WHICH is an integer,
0 meand `-', 1 means `+' etc."
  (interactive "P")
  (org-preserve-lc
   (org-beginning-of-item-list)
   (org-at-item-p)
   (beginning-of-line 1)
   (let ((current (match-string 0)) new)
     (setq new (cond
		((and which (nth (1- which) '("-" "+" "*" "1." "1)"))))
		((string-match "-" current) "+")
		((string-match "\\+" current)
		 (if (looking-at "\\S-") "1." "*"))
		((string-match "\\*" current) "1.")
		((string-match "\\." current) "1)")
		((string-match ")" current) "-")
		(t (error "This should not happen"))))
     (and (looking-at "\\([ \t]*\\)\\S-+") (replace-match (concat "\\1" new)))
     (org-fix-bullet-type 1)
     (org-maybe-renumber-ordered-list))))

(defun org-get-string-indentation (s)
  "What indentation has S due to SPACE and TAB at the beginning of the string?"
  (let ((n -1) (i 0) (w tab-width) c)
    (catch 'exit
      (while (< (setq n (1+ n)) (length s))
	(setq c (aref s n))
	(cond ((= c ?\ ) (setq i (1+ i)))
	      ((= c ?\t) (setq i (* (/ (+ w i) w) w)))
	      (t (throw 'exit t)))))
    i))

(defun org-renumber-ordered-list (arg)
  "Renumber an ordered plain list.
Cursor needs to be in the first line of an item, the line that starts
with something like \"1.\" or \"2)\"."
  (interactive "p")
  (unless (and (org-at-item-p)
	       (match-beginning 3))
    (error "This is not an ordered list"))
  (let ((line (org-current-line))
	(col (current-column))
	(ind (org-get-string-indentation
	      (buffer-substring (point-at-bol) (match-beginning 3))))
	;; (term (substring (match-string 3) -1))
	ind1 (n (1- arg))
	fmt)
    ;; find where this list begins
    (org-beginning-of-item-list)
    (looking-at "[ \t]*[0-9]+\\([.)]\\)")
    (setq fmt (concat "%d" (match-string 1)))
    (beginning-of-line 0)
    ;; walk forward and replace these numbers
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 2)
	  (if (eobp) (throw 'exit nil))
	  (if (looking-at "[ \t]*$") (throw 'next nil))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (> ind1 ind) (throw 'next t))
	  (if (< ind1 ind) (throw 'exit t))
	  (if (not (org-at-item-p)) (throw 'exit nil))
	  (delete-region (match-beginning 2) (match-end 2))
	  (goto-char (match-beginning 2))
	  (insert (format fmt (setq n (1+ n)))))))
    (goto-line line)
    (move-to-column col)))

(defun org-fix-bullet-type (arg)
  "Make sure all items in this list have the same bullet."
  (interactive "p")
  (unless (org-at-item-p) (error "This is not a list"))
  (let ((line (org-current-line))
	(col (current-column))
	(ind (current-indentation))
	ind1 bullet)
    ;; find where this list begins
    (org-beginning-of-item-list)
    (beginning-of-line 1)
    ;; find out what the bullet type is
    (looking-at "[ \t]*\\(\\S-+\\)")
    (setq bullet (match-string 1))
    ;; walk forward and replace these numbers
    (beginning-of-line 0)
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 2)
	  (if (eobp) (throw 'exit nil))
	  (if (looking-at "[ \t]*$") (throw 'next nil))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (> ind1 ind) (throw 'next t))
	  (if (< ind1 ind) (throw 'exit t))
	  (if (not (org-at-item-p)) (throw 'exit nil))
	  (skip-chars-forward " \t")
	  (looking-at "\\S-+")
	  (replace-match bullet))))
    (goto-line line)
    (move-to-column col)
    (if (string-match "[0-9]" bullet)
	(org-renumber-ordered-list 1))))

(defun org-beginning-of-item-list ()
  "Go to the beginning of the current item list.
I.e. to the first item in this list."
  (interactive)
  (org-beginning-of-item)
  (let ((pos (point-at-bol))
        (ind (org-get-indentation))
	ind1)
    ;; find where this list begins
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 0)
	  (if (looking-at "[ \t]*$")
	      (throw (if (bobp) 'exit 'next) t))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (or (< ind1 ind)
		  (and (= ind1 ind)
		       (not (org-at-item-p)))
		  (bobp))
	      (throw 'exit t)
	    (when (org-at-item-p) (setq pos (point-at-bol)))))))
    (goto-char pos)))

(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))

(defun org-outdent-item (arg)
  "Outdent a local list item."
  (interactive "p")
  (org-indent-item (- arg)))

(defun org-indent-item (arg)
  "Indent a local list item."
  (interactive "p")
  (unless (org-at-item-p)
    (error "Not on an item"))
  (save-excursion
    (let (beg end ind ind1 tmp delta ind-down ind-up)
      (if (memq last-command '(org-shiftmetaright org-shiftmetaleft))
	  (setq beg org-last-indent-begin-marker
		end org-last-indent-end-marker)
	(org-beginning-of-item)
	(setq beg (move-marker org-last-indent-begin-marker (point)))
	(org-end-of-item)
	(setq end (move-marker org-last-indent-end-marker (point))))
      (goto-char beg)
      (setq tmp (org-item-indent-positions)
	    ind (car tmp)
	    ind-down (nth 2 tmp)
	    ind-up (nth 1 tmp)
	    delta (if (> arg 0)
		      (if ind-down (- ind-down ind) 2)
		    (if ind-up (- ind-up ind) -2)))
      (if (< (+ delta ind) 0) (error "Cannot outdent beyond margin"))
      (while (< (point) end)
	(beginning-of-line 1)
	(skip-chars-forward " \t") (setq ind1 (current-column))
	(delete-region (point-at-bol) (point))
	(or (eolp) (indent-to-column (+ ind1 delta)))
	(beginning-of-line 2))))
  (org-maybe-renumber-ordered-list-safe)
  (save-excursion
    (beginning-of-line 0)
    (condition-case nil (org-beginning-of-item) (error nil))
    (org-maybe-renumber-ordered-list-safe)))


(defun org-item-indent-positions ()
  "Assumes cursor in item line. FIXME"
  (let* ((bolpos (point-at-bol))
	 (ind (org-get-indentation))
	 ind-down ind-up pos)
    (save-excursion
      (org-beginning-of-item-list)
      (skip-chars-backward "\n\r \t")
      (when (org-in-item-p)
	(org-beginning-of-item)
	(setq ind-up (org-get-indentation))))
    (setq pos (point))
    (save-excursion
      (cond
       ((and (condition-case nil (progn (org-previous-item) t)
	       (error nil))
	     (or (forward-char 1) t)
	     (re-search-forward "^\\([ \t]*\\([-+]\\|\\([0-9]+[.)]\\)\\)\\|[ \t]+\\*\\)\\( \\|$\\)" bolpos t))
	(setq ind-down (org-get-indentation)))
       ((and (goto-char pos)
	     (org-at-item-p))
	(goto-char (match-end 0))
	(skip-chars-forward " \t")
	(setq ind-down (current-column)))))
    (list ind ind-up ind-down)))

;;; The orgstruct minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the org-mode structure editing commands.

;; This is really a hack, because the org-mode structure commands use
;; keys which normally belong to the major mode.  Here is how it
;; works: The minor mode defines all the keys necessary to operate the
;; structure commands, but wraps the commands into a function which
;; tests if the cursor is currently at a headline or a plain list
;; item.  If that is the case, the structure command is used,
;; temporarily setting many Org-mode variables like regular
;; expressions for filling etc.  However, when any of those keys is
;; used at a different location, function uses `key-binding' to look
;; up if the key has an associated command in another currently active
;; keymap (minor modes, major mode, global), and executes that
;; command.  There might be problems if any of the keys is otherwise
;; used as a prefix key.

;; Another challenge is that the key binding for TAB can be tab or \C-i,
;; likewise the binding for RET can be return or \C-m.  Orgtbl-mode
;; addresses this by checking explicitly for both bindings.

(defvar orgstruct-mode-map (make-sparse-keymap)
  "Keymap for the minor `orgstruct-mode'.")

;;;###autoload
(define-minor-mode orgstruct-mode
  "Toggle the minor more `orgstruct-mode'.
This mode is for using Org-mode structure commands in other modes.
The following key behave as if Org-mode was active, if the cursor
is on a headline, or on a plain list item (both in the definition
of Org-mode).

M-up        Move entry/item up
M-down	    Move entry/item down
M-left	    Promote
M-right	    Demote
M-S-up	    Move entry/item up
M-S-down    Move entry/item down
M-S-left    Promote subtree
M-S-right   Demote subtree
M-q	    Fill paragraph and items like in Org-mode
C-c ^	    Sort entries
C-c -	    Cycle list bullet
TAB         Cycle item visibility
M-RET       Insert new heading/item
S-M-RET     Insert new TODO heading / Chekbox item
C-c C-c     Set tags / toggle checkbox"
  nil " OrgStruct" nil
  (and (orgstruct-setup) (defun orgstruct-setup () nil)))

;;;###autoload
(defun turn-on-orgstruct ()
  "Unconditionally turn on `orgstruct-mode'."
  (orgstruct-mode 1))

(defun orgstruct-error ()
  "Error when there is no default binding for a structure key."
  (interactive)
  (error "This key is has no function outside structure elements"))

(defvar org-local-vars nil
  "List of local variables, for use by `orgstruct-mode'")

(defun orgstruct-setup ()
  "Setup orgstruct keymaps."
  (let ((nfunc 0)
	(bindings
	 (list
	  '([(meta up)]           org-metaup)
	  '([(meta down)]         org-metadown)
	  '([(meta left)]         org-metaleft)
	  '([(meta right)]        org-metaright)
	  '([(meta shift up)]     org-shiftmetaup)
	  '([(meta shift down)]   org-shiftmetadown)
	  '([(meta shift left)]   org-shiftmetaleft)
	  '([(meta shift right)]  org-shiftmetaright)
	  '([(shift up)]          org-shiftup)
	  '([(shift down)]        org-shiftdown)
	  '("\C-c\C-c"            org-ctrl-c-ctrl-c)
	  '("\M-q"                fill-paragraph)
	  '("\C-c^"               org-sort)
	  '("\C-c-"               org-cycle-list-bullet)))
	elt key fun cmd)
    (while (setq elt (pop bindings))
      (setq nfunc (1+ nfunc))
      (setq key (org-key (car elt))
	    fun (nth 1 elt)
	    cmd (orgstruct-make-binding fun nfunc key))
      (org-defkey orgstruct-mode-map key cmd))

    ;; Special treatment needed for TAB and RET
    (org-defkey orgstruct-mode-map [(tab)]
		(orgstruct-make-binding 'org-cycle 102 [(tab)] "\C-i"))
    (org-defkey orgstruct-mode-map "\C-i"
		(orgstruct-make-binding 'org-cycle 103 "\C-i" [(tab)]))
    
    (org-defkey orgstruct-mode-map "\M-\C-m"
		(orgstruct-make-binding 'org-insert-heading 105
				     "\M-\C-m" [(meta return)]))
    (org-defkey orgstruct-mode-map [(meta return)]
		(orgstruct-make-binding 'org-insert-heading 106
				     [(meta return)] "\M-\C-m"))

    (org-defkey orgstruct-mode-map [(shift meta return)]
		(orgstruct-make-binding 'org-insert-todo-heading 107
				     [(meta return)] "\M-\C-m"))
    
    (unless org-local-vars
      (setq org-local-vars (org-get-local-variables)))
    
    t))

(defun orgstruct-make-binding (fun n &rest keys)
  "Create a function for binding in the structure minor mode.
FUN is the command to call inside a table.  N is used to create a unique
command name.  KEYS are keys that should be checked in for a command
to execute outside of tables."
  (eval
   (list 'defun
	 (intern (concat "orgstruct-hijacker-command-" (int-to-string n)))
	 '(arg)
	 (concat "In Structure, run `" (symbol-name fun) "'.\n"
		 "Outside of structure, run the binding of `"
		 (mapconcat (lambda (x) (format "%s" x)) keys "' or `")
		 "'.")
	 '(interactive "p")
	 (list 'if
	       '(org-context-p 'headline 'item)
	       (list 'org-run-like-in-org-mode (list 'quote fun))
	       (list 'let '(orgstruct-mode)
		     (list 'call-interactively
			   (append '(or)
				   (mapcar (lambda (k)
					     (list 'key-binding k))
					   keys)
				   '('orgstruct-error))))))))

(defun org-context-p (&rest contexts)
  "FIXME:"
  (let ((pos (point)))
    (goto-char (point-at-bol))
    (prog1 (or (and (memq 'table contexts)
		    (looking-at "[ \t]*|"))
	       (and (memq 'headline contexts)
		    (looking-at "\\*+"))
	       (and (memq 'item contexts)
		    (looking-at "[ \t]*\\([-+*] \\|[0-9]+[.)] \\)")))
      (goto-char pos))))

(defun org-get-local-variables ()
  "Return a list of all local variables in an org-mode buffer."
  (let (varlist)
    (with-current-buffer (get-buffer-create "*Org tmp*")
      (erase-buffer)
      (org-mode)
      (setq varlist (buffer-local-variables)))
    (kill-buffer "*Org tmp*")
    (delq nil
	  (mapcar
	   (lambda (x) 
	     (setq x
		   (if (symbolp x)
		       (list x)
		     (list (car x) (list 'quote (cdr x)))))
	     (if (string-match
		  "^\\(org-\\|orgtbl-\\|outline-\\|comment-\\|paragraph-\\|auto-fill\\|fill-paragraph\\|adaptive-fill\\|indent-\\)"
		  (symbol-name (car x)))
		 x nil))
	   varlist))))

;;;###autoload
(defun org-run-like-in-org-mode (cmd)
  (unless org-local-vars
    (setq org-local-vars (org-get-local-variables)))
  (eval (list 'let org-local-vars
	      (list 'call-interactively (list 'quote cmd)))))

;;;; Archiving

(defalias 'org-advertized-archive-subtree 'org-archive-subtree)

(defun org-archive-subtree (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current file, or in
a different file.  The tree will be moved to that location, the subtree
heading be marked DONE, and the current time will be added.

When called with prefix argument FIND-DONE, find whole trees without any
open TODO items and archive them (after getting confirmation from the user).
If the cursor is not at a headline when this comand is called, try all level
1 trees.  If the cursor is on a headline, only try the direct children of
this heading."
  (interactive "P")
  (if find-done
      (org-archive-all-done)
    ;; Save all relevant TODO keyword-relatex variables

    (let ((tr-org-todo-line-regexp org-todo-line-regexp) ; keep despite compiler
	  (tr-org-todo-keywords-1 org-todo-keywords-1)
	  (tr-org-todo-kwd-alist org-todo-kwd-alist)
	  (tr-org-done-keywords org-done-keywords)
	  (tr-org-todo-regexp org-todo-regexp)
	  (tr-org-todo-line-regexp org-todo-line-regexp)
	  (tr-org-odd-levels-only org-odd-levels-only)
	  (this-buffer (current-buffer))
	  (org-archive-location org-archive-location)
	  (re "^#\\+ARCHIVE:[ \t]+\\(\\S-.*\\S-\\)[ \t]*$")
	  (file (abbreviate-file-name (buffer-file-name)))
	  (time (format-time-string
		 (substring (cdr org-time-stamp-formats) 1 -1)
		 (current-time)))
	  afile heading buffer level newfile-p
	  category todo priority ltags itags)

      ;; Try to find a local archive location
      (save-excursion
	(save-restriction
	  (widen)
	  (if (or (re-search-backward re nil t) (re-search-forward re nil t))
	      (setq org-archive-location (match-string 1)))))

      (if (string-match "\\(.*\\)::\\(.*\\)" org-archive-location)
	  (progn
	    (setq afile (format (match-string 1 org-archive-location)
			       (file-name-nondirectory buffer-file-name))
		  heading (match-string 2 org-archive-location)))
	(error "Invalid `org-archive-location'"))
      (if (> (length afile) 0)
	  (setq newfile-p (not (file-exists-p afile))
		buffer (find-file-noselect afile))
	(setq buffer (current-buffer)))
      (unless buffer
	(error "Cannot access file \"%s\"" afile))
      (if (and (> (length heading) 0)
	       (string-match "^\\*+" heading))
	  (setq level (match-end 0))
	(setq heading nil level 0))
      (save-excursion
	(org-back-to-heading t)
	;; Get context information that will be lost by moving the tree
	(setq category (org-get-category)
	      todo (and (looking-at org-todo-line-regexp)
			    (match-string 2))
	      priority (org-get-priority (if (match-end 3) (match-string 3) ""))
	      ltags (org-split-string (org-get-tags) ":")
	      itags (org-delete-all ltags (org-get-tags-at)))
	(setq ltags (mapconcat 'identity ltags " ")
	      itags (mapconcat 'identity itags " "))
	;; We first only copy, in case something goes wrong
	;; we need to protect this-command, to avoid kill-region sets it,
	;; which would lead to duplication of subtrees
	(let (this-command) (org-copy-subtree))
	(set-buffer buffer)
	;; Enforce org-mode for the archive buffer
	(if (not (org-mode-p))
	    ;; Force the mode for future visits.
	    (let ((org-insert-mode-line-in-empty-file t)
		  (org-inhibit-startup t))
	      (call-interactively 'org-mode)))
	(when newfile-p
	  (goto-char (point-max))
	  (insert (format "\nArchived entries from file %s\n\n"
			  (buffer-file-name this-buffer))))
	;; Force the TODO keywords of the original buffer
	(let ((org-todo-line-regexp tr-org-todo-line-regexp)
	      (org-todo-keywords-1 tr-org-todo-keywords-1)
	      (org-todo-kwd-alist tr-org-todo-kwd-alist)
	      (org-done-keywords tr-org-done-keywords)
	      (org-todo-regexp tr-org-todo-regexp)
	      (org-todo-line-regexp tr-org-todo-line-regexp)
	      (org-odd-levels-only
	       (if (local-variable-p 'org-odd-levels-only (current-buffer))
		   org-odd-levels-only
		 tr-org-odd-levels-only)))
	  (goto-char (point-min))
	  (if heading
	      (progn
		(if (re-search-forward
		     (concat "^" (regexp-quote heading)
			     (org-re "[ \t]*\\(:[[:alnum:]_@:]+:\\)?[ \t]*\\($\\|\r\\)"))
		     nil t)
		    (goto-char (match-end 0))
		  ;; Heading not found, just insert it at the end
		  (goto-char (point-max))
		  (or (bolp) (insert "\n"))
		  (insert "\n" heading "\n")
		  (end-of-line 0))
		;; Make the subtree visible
		(show-subtree)
		(org-end-of-subtree t)
		(skip-chars-backward " \t\r\n")
		(and (looking-at "[ \t\r\n]*")
		     (replace-match "\n\n")))
	    ;; No specific heading, just go to end of file.
	    (goto-char (point-max)) (insert "\n"))
	  ;; Paste
	  (org-paste-subtree (org-get-legal-level level 1))

	  ;; Mark the entry as done
	  (when (and org-archive-mark-done
		     (looking-at org-todo-line-regexp)
		     (or (not (match-end 2))
			 (not (member (match-string 2) org-done-keywords))))
	    (let (org-log-done)
	      (org-todo
	       (car (or (member org-archive-mark-done org-done-keywords)
			org-done-keywords)))))

	  ;; Add the context info
	  (when org-archive-save-context-info
	    (let ((l org-archive-save-context-info) e n v)
	      (while (setq e (pop l))
		(when (and (setq v (symbol-value e))
			   (stringp v) (string-match "\\S-" v))
		  (setq n (concat "ARCHIVE_" (upcase (symbol-name e))))
		  (org-entry-put (point) n v)))))

	  ;; Save the buffer, if it is not the same buffer.
	  (if (not (eq this-buffer buffer)) (save-buffer))))
      ;; Here we are back in the original buffer.  Everything seems to have
      ;; worked.  So now cut the tree and finish up.
      (let (this-command) (org-cut-subtree))
      (if (and (not (eobp)) (looking-at "[ \t]*$")) (kill-line))
      (message "Subtree archived %s"
	       (if (eq this-buffer buffer)
		   (concat "under heading: " heading)
		 (concat "in file: " (abbreviate-file-name afile)))))))

(defun org-archive-all-done (&optional tag)
  "Archive sublevels of the current tree without open TODO items.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (let ((re (concat "^\\*+ +" org-not-done-regexp)) re1
	(rea (concat ".*:" org-archive-tag ":"))
	(begm (make-marker))
	(endm (make-marker))
	(question (if tag "Set ARCHIVE tag (no open TODO items)? "
		    "Move subtree to archive (no open TODO items)? "))
	beg end (cntarch 0))
    (if (org-on-heading-p)
	(progn
	  (setq re1 (concat "^" (regexp-quote
				 (make-string
				  (1+ (- (match-end 0) (match-beginning 0)))
				  ?*))
			    " "))
	  (move-marker begm (point))
	  (move-marker endm (org-end-of-subtree t)))
      (setq re1 "^* ")
      (move-marker begm (point-min))
      (move-marker endm (point-max)))
    (save-excursion
      (goto-char begm)
      (while (re-search-forward re1 endm t)
	(setq beg (match-beginning 0)
	      end (save-excursion (org-end-of-subtree t) (point)))
	(goto-char beg)
	(if (re-search-forward re end t)
	    (goto-char end)
	  (goto-char beg)
	  (if (and (or (not tag) (not (looking-at rea)))
		   (y-or-n-p question))
	      (progn
		(if tag
		    (org-toggle-tag org-archive-tag 'on)
		  (org-archive-subtree))
		(setq cntarch (1+ cntarch)))
	    (goto-char end)))))
    (message "%d trees archived" cntarch)))

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (org-mode-p)
	     (not (memq state '(overview folded))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max) (org-end-of-subtree t))))
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp end t)
	  (org-flag-drawer t))))))

(defun org-flag-drawer (flag)
  (save-excursion
    (beginning-of-line 1)
    (when (looking-at "^[ \t]*:[a-zA-Z][a-zA-Z0-9]*:")
      (let ((b (match-end 0)))
	(if (re-search-forward
	     "^[ \t]*:END:"
	     (save-excursion (outline-next-heading) (point)) t)
	    (outline-flag-region b (point-at-eol) flag)
	  (error ":END: line missing"))))))

(defun org-cycle-hide-archived-subtrees (state)
  "Re-hide all archived subtrees after a visibility state change."
  (when (and (not org-cycle-open-archived-trees)
             (not (memq state '(overview folded))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max) (org-end-of-subtree t))))
	(org-hide-archived-subtrees beg end)
	(goto-char beg)
	(if (looking-at (concat ".*:" org-archive-tag ":"))
	    (message (substitute-command-keys
		      "Subtree is archived and stays closed.  Use \\[org-force-cycle-archived] to cycle it anyway.")))))))

(defun org-force-cycle-archived ()
  "Cycle subtree even if it is archived."
  (interactive)
  (setq this-command 'org-cycle)
  (let ((org-cycle-open-archived-trees t))
    (call-interactively 'org-cycle)))

(defun org-hide-archived-subtrees (beg end)
  "Re-hide all archived subtrees after a visibility state change."
  (save-excursion
    (let* ((re (concat ":" org-archive-tag ":")))
      (goto-char beg)
      (while (re-search-forward re end t)
	(and (org-on-heading-p) (hide-subtree))
	(org-end-of-subtree t)))))

(defun org-toggle-tag (tag &optional onoff)
  "Toggle the tag TAG for the current line.
If ONOFF is `on' or `off', don't toggle but set to this state."
  (unless (org-on-heading-p t) (error "Not on headling"))
  (let (res current)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward (org-re "[ \t]:\\([[:alnum:]_@:]+\\):[ \t]*$")
			     (point-at-eol) t)
	  (progn
	    (setq current (match-string 1))
	    (replace-match ""))
	(setq current ""))
      (setq current (nreverse (org-split-string current ":")))
      (cond
       ((eq onoff 'on)
	(setq res t)
	(or (member tag current) (push tag current)))
       ((eq onoff 'off)
	(or (not (member tag current)) (setq current (delete tag current))))
       (t (if (member tag current)
	      (setq current (delete tag current))
	    (setq res t)
	    (push tag current))))
      (end-of-line 1)
      (when current
	(insert " :" (mapconcat 'identity (nreverse current) ":") ":"))
      (org-set-tags nil t))
    res))

(defun org-toggle-archive-tag (&optional arg)
  "Toggle the archive tag for the current headline.
With prefix ARG, check all children of current headline and offer tagging
the children that do not contain any open TODO items."
  (interactive "P")
  (if arg
      (org-archive-all-done 'tag)
    (let (set)
      (save-excursion
	(org-back-to-heading t)
	(setq set (org-toggle-tag org-archive-tag))
	(when set (hide-subtree)))
      (and set (beginning-of-line 1))
      (message "Subtree %s" (if set "archived" "unarchived")))))


;;;; Tables

;;; The table editor

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

(defvar org-table-last-highlighted-reference nil)
(defvar org-table-formula-history nil)

(defvar org-table-column-names nil
  "Alist with column names, derived from the `!' line.")
(defvar org-table-column-name-regexp nil
  "Regular expression matching the current column names.")
(defvar org-table-local-parameters nil
  "Alist with parameter names, derived from the `$' line.")
(defvar org-table-named-field-locations nil
  "Alist with locations of named fields.")

(defvar org-table-current-line-types nil
  "Table row types, non-nil only for the duration of a comand.")
(defvar org-table-current-begin-line nil
  "Table begin line, non-nil only for the duration of a comand.")
(defvar org-table-current-begin-pos nil
  "Table begin position, non-nil only for the duration of a comand.")
(defvar org-table-dlines nil
  "Vector of data line line numbers in the current table.")
(defvar org-table-hlines nil
  "Vector of hline line numbers in the current table.")

(defconst org-table-range-regexp
   "@\\([-+]?I*[-+]?[0-9]*\\)?\\(\\$[-+]?[0-9]+\\)?\\(\\.\\.@?\\([-+]?I*[-+]?[0-9]*\\)?\\(\\$[-+]?[0-9]+\\)?\\)?"
   ;;   1                        2                    3          4                        5
  "Regular expression for matching ranges in formulas.")

(defconst org-table-range-regexp2
  (concat
   "\\(" "@[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)"
   "\\.\\."
   "\\(" "@?[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)")
  "Match a range for reference display.")

(defconst org-table-translate-regexp
  (concat "\\(" "@[-0-9I$]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\)")
  "Match a reference that needs translation, for reference display.")

(defvar org-inhibit-highlight-removal nil) ; dynamically scoped param

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

(defun org-table-create-or-convert-from-region (arg)
  "Convert region to table, or create an empty table.
If there is an active region, convert it to a table, using the function
`org-table-convert-region'.
If there is no such region, create an empty table with `org-table-create'."
  (interactive "P")
  (if (org-region-active-p)
      (org-table-convert-region (region-beginning) (region-end) arg)
    (org-table-create arg)))

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

(defun org-table-convert-region (beg0 end0 &optional nspace)
  "Convert region to a table.
The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is included.
When NSPACE is non-nil, it indicates the minimum number of spaces that
separate columns.  By default, the function first checks if every line
contains at lease one TAB.  If yes, it assumes that the material is TAB
separated.  If not, it assumes a single space as separator."
  (interactive "rP")
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
      (replace-match "| " t t))
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
(defvar org-table-last-column-widths nil
  "List of max width of fields in each column.
This is being used to correctly align a single field after TAB or RET.")
(defvar org-table-overlay-coordinates nil
  "Overlay coordinates after each align of a table.")
(make-variable-buffer-local 'org-table-overlay-coordinates)

(defvar org-last-recalc-line nil)
(defconst org-narrow-column-arrow "=>"
  "Used as display property in narrowed table columns.")

(defun org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let* (
	 ;; Limits of table
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
	 (colpos (org-table-current-column))
	 (winstart (window-start))
	 (winstartline (org-current-line (min winstart (1- (point-max)))))
	 lines (new "") lengths l typenums ty fields maxfields i
	 column
	 (indent "") cnt frac
	 rfmt hfmt
	 (spaces '(1 . 1))
	 (sp1 (car spaces))
	 (sp2 (cdr spaces))
	 (rfmt1 (concat
		 (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
	 (hfmt1 (concat
		 (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
	 emptystrings links dates narrow fmax f1 len c e)
    (untabify beg end)
    (remove-text-properties beg end '(org-cwidth t org-dwidth t display t))
    ;; Check if we have links or dates
    (goto-char beg)
    (setq links (re-search-forward org-bracket-link-regexp end t))
    (goto-char beg)
    (setq dates (and org-display-custom-times
		     (re-search-forward org-ts-regexp-both end t)))
    ;; Make sure the link properties are right
    (when links (goto-char beg) (while (org-activate-bracket-links end)))
    ;; Make sure the date properties are right
    (when dates (goto-char beg) (while (org-activate-dates end)))

    ;; Check if we are narrowing any columns
    (goto-char beg)
    (setq narrow (and org-format-transports-properties-p
		      (re-search-forward "<[0-9]+>" end t)))
    ;; Get the rows
    (setq lines (org-split-string
		 (buffer-substring beg end) "\n"))
    ;; Store the indentation of the first line
    (if (string-match "^ *" (car lines))
	(setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    ;; Mark the hlines by setting the corresponding element to nil
    ;; At the same time, we remove trailing space.
    (setq lines (mapcar (lambda (l)
			  (if (string-match "^ *|-" l)
			      nil
			    (if (string-match "[ \t]+$" l)
				(substring l 0 (match-beginning 0))
			      l)))
			lines))
    ;; Get the data fields by splitting the lines.
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
    ;; A list of empty strings to fill any short rows on output
    (setq emptystrings (make-list maxfields ""))
    ;; Check for special formatting.
    (setq i -1)
    (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
      (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
      ;; Check if there is an explicit width specified
      (when narrow
	(setq c column fmax nil)
	(while c
	  (setq e (pop c))
	  (if (and (stringp e) (string-match "^<\\([0-9]+\\)>$" e))
	      (setq fmax (string-to-number (match-string 1 e)) c nil)))
	;; Find fields that are wider than fmax, and shorten them
	(when fmax
	  (loop for xx in column do
		(when (and (stringp xx)
			   (> (org-string-width xx) fmax))
		  (org-add-props xx nil
		    'help-echo
		    (concat "Clipped table field, use C-c ` to edit. Full value is:\n" (org-no-properties (copy-sequence xx))))
		  (setq f1 (min fmax (or (string-match org-bracket-link-regexp xx) fmax)))
		  (unless (> f1 1)
		    (error "Cannot narrow field starting with wide link \"%s\""
			   (match-string 0 xx)))
		  (add-text-properties f1 (length xx) (list 'org-cwidth t) xx)
		  (add-text-properties (- f1 2) f1
				       (list 'display org-narrow-column-arrow)
				       xx)))))
      ;; Get the maximum width for each column
      (push (apply 'max 1 (mapcar 'org-string-width column)) lengths)
      ;; Get the fraction of numbers, to decide about alignment of the column
      (setq cnt 0 frac 0.0)
      (loop for x in column do
	    (if (equal x "")
		nil
	      (setq frac ( / (+ (* frac cnt)
				(if (string-match org-table-number-regexp x) 1 0))
			     (setq cnt (1+ cnt))))))
      (push (>= frac org-table-number-fraction) typenums))
    (setq lengths (nreverse lengths) typenums (nreverse typenums))

    ;; Store the alignment of this table, for later editing of single fields
    (setq org-table-last-alignment typenums
	  org-table-last-column-widths lengths)

    ;; With invisible characters, `format' does not get the field width right
    ;; So we need to make these fields wide by hand.
    (when links
      (loop for i from 0 upto (1- maxfields) do
	    (setq len (nth i lengths))
	    (loop for j from 0 upto (1- (length fields)) do
		  (setq c (nthcdr i (car (nthcdr j fields))))
		  (if (and (stringp (car c))
			   (string-match org-bracket-link-regexp (car c))
			   (< (org-string-width (car c)) len))
		      (setcar c (concat (car c) (make-string (- len (org-string-width (car c))) ?\ )))))))

    ;; Compute the formats needed for output of the table
    (setq rfmt (concat indent "|") hfmt (concat indent "|"))
    (while (setq l (pop lengths))
      (setq ty (if (pop typenums) "" "-")) ; number types flushright
      (setq rfmt (concat rfmt (format rfmt1 ty l))
	    hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
    (setq rfmt (concat rfmt "\n")
	  hfmt (concat (substring hfmt 0 -1) "|\n"))

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
    (when (and orgtbl-mode (not (org-mode-p)))
      (goto-char org-table-aligned-begin-marker)
      (while (org-hide-wide-columns org-table-aligned-end-marker)))
    ;; Try to move to the old location
    (goto-line winstartline)
    (setq winstart (point-at-bol))
    (goto-line linepos)
    (set-window-start (selected-window) winstart 'noforce)
    (org-table-goto-column colpos)
    (and org-table-overlay-coordinates (org-table-overlay-coordinates))
    (setq org-table-may-need-update nil)
    ))

(defun org-string-width (s)
  "Compute width of string, ignoring invisible characters.
This ignores character with invisibility property `org-link', and also
characters with property `org-cwidth', because these will become invisible
upon the next fontification round."
  (let (b l)
    (when (or (eq t buffer-invisibility-spec)
	      (assq 'org-link buffer-invisibility-spec))
      (while (setq b (text-property-any 0 (length s)
					'invisible 'org-link s))
	(setq s (concat (substring s 0 b)
			(substring s (or (next-single-property-change
					  b 'invisible s) (length s)))))))
    (while (setq b (text-property-any 0 (length s) 'org-cwidth t s))
      (setq s (concat (substring s 0 b)
		      (substring s (or (next-single-property-change
					b 'org-cwidth s) (length s))))))
    (setq l (string-width s) b -1)
    (while (setq b (text-property-any (1+ b) (length s) 'org-dwidth t s))
      (setq l (- l (get-text-property b 'org-dwidth-n s))))
    l))

(defun org-table-begin (&optional table-type)
  "Find the beginning of the table and return its position.
With argument TABLE-TYPE, go to the beginning of a table.el-type table."
  (save-excursion
    (if (not (re-search-backward
	      (if table-type org-table-any-border-regexp
		org-table-border-regexp)
	      nil t))
	(progn (goto-char (point-min)) (point))
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
   ((org-at-table-hline-p))
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
	   (num (if (> col 0) (nth (1- col) org-table-last-alignment)))
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
		    n (format f s))
	      (if new
		  (if (<= (length new) l)      ;; FIXME: length -> str-width?
		      (setq n (format f new))
		    (setq n (concat new "|") org-table-may-need-update t)))
	      (or (equal n o)
		  (let (org-table-may-need-update)
		    (replace-match n t t))))
	  (setq org-table-may-need-update t))
	(goto-char pos))))))

(defun org-table-next-field ()
  "Go to the next field in the current table, creating new lines as needed.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (let ((end (org-table-end)))
    (if (org-at-table-hline-p)
	(end-of-line 1))
    (condition-case nil
	(progn
	  (re-search-forward "|" end)
	  (if (looking-at "[ \t]*$")
	      (re-search-forward "|" end))
	  (if (and (looking-at "-")
		   org-table-tab-jumps-over-hlines
		   (re-search-forward "^[ \t]*|\\([^-]\\)" end t))
	      (goto-char (match-beginning 1)))
	  (if (looking-at "-")
	      (progn
		(beginning-of-line 0)
		(org-table-insert-row 'below))
	    (if (looking-at " ") (forward-char 1))))
      (error
       (org-table-insert-row 'below)))))

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
integer or a timestamp, it will be incremented while copying.  In the case of
a timestamp, if the cursor is on the year, change the year.  If it is on the
month or the day, change that.  Point will stay on the current date field
in order to easily repeat the interval."
  (interactive "p")
  (let* ((colpos (org-table-current-column))
	 (col (current-column))
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
	  (move-to-column col)
	  (if (and org-table-copy-increment (org-at-timestamp-p t))
	      (org-timestamp-up 1)
	    (org-table-maybe-recalculate-line))
	  (org-table-align)
	  (move-to-column col))
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
	       (len (org-string-width match)))
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
	    (replace-match (concat "|" replace) t t))
	(goto-char (min (point-at-eol) (+ 2 pos)))
	val)
    (forward-char 1) ""))

(defun org-table-field-info (arg)
  "Show info about the current field, and highlight any reference at point."
  (interactive "P")
  (org-table-get-specials)
  (save-excursion
    (let* ((pos (point))
	   (col (org-table-current-column))
	   (cname (car (rassoc (int-to-string col) org-table-column-names)))
	   (name (car (rassoc (list (org-current-line) col)
			      org-table-named-field-locations)))
	   (eql (org-table-get-stored-formulas))
	   (dline (org-table-current-dline))
	   (ref (format "@%d$%d" dline col))
	   (ref1 (org-table-convert-refs-to-an ref))
	   (fequation (or (assoc name eql) (assoc ref eql)))
	   (cequation (assoc (int-to-string col) eql))
	   (eqn (or fequation cequation)))
      (goto-char pos)
      (condition-case nil
	  (org-table-show-reference 'local)
	(error nil))
      (message "line @%d, col $%s%s, ref @%d$%d or %s%s%s"
	       dline col
	       (if cname (concat " or $" cname) "")
	       dline col ref1
	       (if name (concat " or $" name) "")
	       ;; FIXME: formula info not correct if special table line
	       (if eqn
		   (concat ", formula: "
			   (org-table-formula-to-user
			    (concat
			     (if (string-match "^[$@]"(car eqn)) "" "$")
			     (car eqn) "=" (cdr eqn))))
		 "")))))

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

(defun org-table-current-dline ()
  "Find out what table data line we are in.
Only datalins count for this."
  (interactive)
  (if (interactive-p) (org-table-check-inside-data-field))
  (save-excursion
    (let ((cnt 0) (pos (point)))
      (goto-char (org-table-begin))
      (while (<= (point) pos)
	(if (looking-at org-table-dataline-regexp) (setq cnt (1+ cnt)))
	(beginning-of-line 2))
      (if (interactive-p) (message "This is table line %d" cnt))
      cnt)))

(defun org-table-goto-column (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (interactive "p")
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
If TABLE-TYPE is non-nil, also check for table.el-type tables."
  (if org-enable-table-editor
      (save-excursion
	(beginning-of-line 1)
	(looking-at (if table-type org-table-any-line-regexp
		      org-table-line-regexp)))
    nil))

(defun org-at-table.el-p ()
  "Return t if and only if we are at a table.el table."
  (and (org-at-table-p 'any)
       (save-excursion
	 (goto-char (org-table-begin 'any))
	 (looking-at org-table1-hline-regexp))))

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
	 (linepos (org-current-line))
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
    (org-table-fix-formulas "$" nil (1- col) 1)))

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
  "Delete a column from the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
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
    (org-table-fix-formulas "$" (list (cons (number-to-string col) "INVALID"))
			    col -1 col)))

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
	 (linepos (org-current-line))
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
    (org-table-fix-formulas
     "$" (list (cons (number-to-string col) (number-to-string colpos))
	       (cons (number-to-string colpos) (number-to-string col))))))

(defun org-table-move-row-down ()
  "Move table row down."
  (interactive)
  (org-table-move-row nil))
(defun org-table-move-row-up ()
  "Move table row up."
  (interactive)
  (org-table-move-row 'up))

(defun org-table-move-row (&optional up)
  "Move the current table line down.  With arg UP, move it up."
  (interactive "P")
  (let* ((col (current-column))
	 (pos (point))
	 (hline1p (save-excursion (beginning-of-line 1)
				  (looking-at org-table-hline-regexp)))
	 (dline1 (org-table-current-dline))
	 (dline2 (+ dline1 (if up -1 1)))
	 (tonew (if up 0 2))
	 txt hline2p)
    (beginning-of-line tonew)
    (unless (org-at-table-p)
      (goto-char pos)
      (error "Cannot move row further"))
    (setq hline2p (looking-at org-table-hline-regexp))
    (goto-char pos)
    (beginning-of-line 1)
    (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt)
    (beginning-of-line 0)
    (move-to-column col)
    (unless (or hline1p hline2p)
      (org-table-fix-formulas
       "@" (list (cons (number-to-string dline1) (number-to-string dline2))
		 (cons (number-to-string dline2) (number-to-string dline1)))))))

(defun org-table-insert-row (&optional arg)
  "Insert a new row above the current line into the table.
With prefix ARG, insert below the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
	 (new (org-table-clean-line line)))
    ;; Fix the first field if necessary
    (if (string-match "^[ \t]*| *[#$] *|" line)
	(setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line (if arg 2 1))
    (let (org-table-may-need-update) (insert-before-markers new "\n"))
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and (or org-table-may-need-update org-table-overlay-coordinates)
	 (org-table-align))
    (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) 1)))

(defun org-table-insert-hline (&optional above)
  "Insert a horizontal-line below the current line into the table.
With prefix ABOVE, insert above the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((line (org-table-clean-line
	       (buffer-substring (point-at-bol) (point-at-eol))))
	(col (current-column)))
    (while (string-match "|\\( +\\)|" line)
      (setq line (replace-match
		  (concat "+" (make-string (- (match-end 1) (match-beginning 1))
					   ?-) "|") t t line)))
    (and (string-match "\\+" line) (setq line (replace-match "|" t t line)))
    (beginning-of-line (if above 1 2))
    (insert line "\n")
    (beginning-of-line (if above 1 -1))
    (move-to-column col)
    (and org-table-overlay-coordinates (org-table-align))))

(defun org-table-hline-and-move (&optional same-column)
  "Insert a hline and move to the row below that line."
  (interactive "P")
  (let ((col (org-table-current-column)))
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line)
    (org-table-insert-hline)
    (end-of-line 2)
    (if (looking-at "\n[ \t]*|-")
	(progn (insert "\n|") (org-table-align))
      (org-table-next-field))
    (if same-column (org-table-goto-column col))))

(defun org-table-clean-line (s)
  "Convert a table line S into a string with only \"|\" and space.
In particular, this does handle wide and invisible characters."
  (if (string-match "^[ \t]*|-" s)
      ;; It's a hline, just map the characters
      (setq s (mapconcat (lambda (x) (if (member x '(?| ?+)) "|" " ")) s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
	       (concat "|" (make-string (org-string-width (match-string 1 s))
					?\ ) "|")
	       t t s)))
    s))

(defun org-table-kill-row ()
  "Delete the current row or horizontal line from the table."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((col (current-column))
	(dline (org-table-current-dline)))
    (kill-region (point-at-bol) (min (1+ (point-at-eol)) (point-max)))
    (if (not (org-at-table-p)) (beginning-of-line 0))
    (move-to-column col)
    (org-table-fix-formulas "@" (list (cons (number-to-string dline) "INVALID"))
			    dline -1 dline)))


(defun org-table-sort-lines (with-case &optional sorting-type)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist.  If point is before the first column, you will be prompted
for the sorting column.  If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically, numerically, or by time (as given in a time stamp
in the field).  Sorting in reverse order is also possible.

With prefix argument WITH-CASE, alphabetic sorting will be case-sensitive.

If SORTING-TYPE is specified when this function is called from a Lisp
program, no prompting will take place.  SORTING-TYPE must be a character,
any of (?a ?A ?n ?N ?t ?T) where the capital letter indicate that sorting
should be done in reverse order."
  (interactive "P")
  (let* ((thisline (org-current-line))
	 (thiscol (org-table-current-column))
	 beg end bcol ecol tend tbeg column lns pos)
    (when (equal thiscol 0)
      (if (interactive-p)
	  (setq thiscol
		(string-to-number
		 (read-string "Use column N for sorting: ")))
	(setq thiscol 1))
      (org-table-goto-column thiscol))
    (org-table-check-inside-data-field)
    (if (org-region-active-p)
	(progn
	  (setq beg (region-beginning) end (region-end))
	  (goto-char beg)
	  (setq column (org-table-current-column)
		beg (point-at-bol))
	  (goto-char end)
	  (setq end (point-at-bol 2)))
      (setq column (org-table-current-column)
	    pos (point)
	    tbeg (org-table-begin)
	    tend (org-table-end))
      (if (re-search-backward org-table-hline-regexp tbeg t)
	  (setq beg (point-at-bol 2))
	(goto-char tbeg)
	(setq beg (point-at-bol 1)))
      (goto-char pos)
      (if (re-search-forward org-table-hline-regexp tend t)
	  (setq end (point-at-bol 1))
	(goto-char tend)
	(setq end (point-at-bol))))
    (setq beg (move-marker (make-marker) beg)
	  end (move-marker (make-marker) end))
    (untabify beg end)
    (goto-char beg)
    (org-table-goto-column column)
    (skip-chars-backward "^|")
    (setq bcol (current-column))
    (org-table-goto-column (1+ column))
    (skip-chars-backward "^|")
    (setq ecol (1- (current-column)))
    (org-table-goto-column column)
    (setq lns (mapcar (lambda(x) (cons (org-trim (substring x bcol ecol)) x))
		      (org-split-string (buffer-substring beg end) "\n")))
    (setq lns (org-do-sort lns "Table" with-case sorting-type))
    (delete-region beg end)
    (move-marker beg nil)
    (move-marker end nil)
    (insert (mapconcat 'cdr lns "\n") "\n")
    (goto-line thisline)
    (org-table-goto-column thiscol)
    (message "%d lines sorted, based on column %d" (length lns) column)))

(defun org-table-cut-region (beg end)
  "Copy region in table to the clipboard and blank all relevant fields."
  (interactive "r")
  (org-table-copy-region beg end 'cut))

(defun org-table-copy-region (beg end &optional cut)
  "Copy rectangular region in table to clipboard.
A special clipboard is used which can only be accessed
with `org-table-paste-rectangle'."
  (interactive "rP")
  (let* (l01 c01 l02 c02 l1 c1 l2 c2 ic1 ic2
	 region cols
	 (rpl (if cut "  " nil)))
    (goto-char beg)
    (org-table-check-inside-data-field)
    (setq l01 (org-current-line)
	  c01 (org-table-current-column))
    (goto-char end)
    (org-table-check-inside-data-field)
    (setq l02 (org-current-line)
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
	 (line (org-current-line))
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
	  (beginning-of-line -1)
	  ;; insert a hline after each line
	  (while (progn (beginning-of-line 3) (< (point) end))
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
formats the selected text to two lines.  If the region was longer than two
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
      (let* ((beg (region-beginning))
	     (cline (save-excursion (goto-char beg) (org-current-line)))
	     (ccol (save-excursion (goto-char beg) (org-table-current-column)))
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
	(goto-line cline)
	(org-table-goto-column ccol)
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

(defvar org-field-marker nil)

(defun org-table-edit-field (arg)
  "Edit table field in a different window.
This is mainly useful for fields that contain hidden parts.
When called with a \\[universal-argument] prefix, just make the full field visible so that
it can be edited in place."
  (interactive "P")
  (if arg
      (let ((b (save-excursion (skip-chars-backward "^|") (point)))
	    (e (save-excursion (skip-chars-forward "^|\r\n") (point))))
	(remove-text-properties b e '(org-cwidth t invisible t
						 display t intangible t))
	(if (and (boundp 'font-lock-mode) font-lock-mode)
	    (font-lock-fontify-block)))
    (let ((pos (move-marker (make-marker) (point)))
	  (field (org-table-get-field))
	  (cw (current-window-configuration))
	  p)
      (org-switch-to-buffer-other-window "*Org tmp*")
      (erase-buffer)
      (insert "#\n# Edit field and finish with C-c C-c\n#\n")
      (let ((org-inhibit-startup t)) (org-mode))
      (goto-char (setq p (point-max)))
      (insert (org-trim field))
      (remove-text-properties p (point-max)
			      '(invisible t org-cwidth t display t
					  intangible t))
      (goto-char p)
      (org-set-local 'org-finish-function 'org-table-finish-edit-field)
      (org-set-local 'org-window-configuration cw)
      (org-set-local 'org-field-marker pos)
      (message "Edit and finish with C-c C-c"))))

(defun org-table-finish-edit-field ()
  "Finish editing a table data field.
Remove all newline characters, insert the result into the table, realign
the table and kill the editing buffer."
  (let ((pos org-field-marker)
	(cw org-window-configuration)
	(cb (current-buffer))
	text)
    (goto-char (point-min))
    (while (re-search-forward "^#.*\n?" nil t) (replace-match ""))
    (while (re-search-forward "\\([ \t]*\n[ \t]*\\)+" nil t)
      (replace-match " "))
    (setq text (org-trim (buffer-string)))
    (set-window-configuration cw)
    (kill-buffer cb)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (move-marker pos nil)
    (org-table-check-inside-data-field)
    (org-table-get-field nil text)
    (org-table-align)
    (message "New field value inserted")))

(defun org-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "^[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+$" s) (setq s (replace-match "" t t s)))
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
	 (maxword (apply 'max (mapcar 'org-string-width words)))
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
	(re-search-forward org-table-any-border-regexp nil 1))))
  (message "Mapping tables: done"))

(defvar org-timecnt) ; dynamically scoped parameter

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
    (let (col (org-timecnt 0) diff h m s org-table-clip)
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
	(setq beg (point))
	(goto-char (org-table-end))
	(unless (re-search-backward "^[ \t]*|[^-]" nil t)
	  (error "No table data"))
	(org-table-goto-column col)
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
	     (sres (if (= org-timecnt 0)
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
	(if (boundp 'org-timecnt) (setq org-timecnt (1+ org-timecnt)))
	(* 1.0 (+ h (/ m 60.0) (/ s 3600.0)))))
     ((equal n 0)                             nil)
     (t n))))

(defun org-table-current-field-formula (&optional key noerror)
  "Return the formula active for the current field.
Assumes that specials are in place.
If KEY is given, return the key to this formula.
Otherwise return the formula preceeded with \"=\" or \":=\"."
  (let* ((name (car (rassoc (list (org-current-line)
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (col (org-table-current-column))
	 (scol (int-to-string col))
	 (ref (format "@%d$%d" (org-table-current-dline) col))
	 (stored-list (org-table-get-stored-formulas noerror))
	 (ass (or (assoc name stored-list)
		  (assoc ref stored-list)
		  (assoc scol stored-list))))
    (if key
	(car ass)
      (if ass (concat (if (string-match "^[0-9]+$" (car ass)) "=" ":=")
		      (cdr ass))))))

(defun org-table-get-formula (&optional equation named)
  "Read a formula from the minibuffer, offer stored formula as default.
When NAMED is non-nil, look for a named equation."
  (let* ((stored-list (org-table-get-stored-formulas))
	 (name (car (rassoc (list (org-current-line)
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (ref (format "@%d$%d" (org-table-current-dline)
		      (org-table-current-column)))
	 (refass (assoc ref stored-list))
	 (scol (if named
		   (if name name ref)
		 (int-to-string (org-table-current-column))))
	 (dummy (and (or name refass) (not named)
		     (not (y-or-n-p "Replace field formula with column formula? " ))
		     (error "Abort")))
	 (name (or name ref))
	 (org-table-may-need-update nil)
	 (stored (cdr (assoc scol stored-list)))
	 (eq (cond
	      ((and stored equation (string-match "^ *=? *$" equation))
	       stored)
	      ((stringp equation)
	       equation)
	      (t (org-table-formula-from-user
		  (read-string
		   (org-table-formula-to-user
		    (format "%s formula %s%s="
			    (if named "Field" "Column")
			    (if (member (string-to-char scol) '(?$ ?@)) "" "$")
			    scol))
		   (if stored (org-table-formula-to-user stored) "")
		   'org-table-formula-history
		   )))))
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
  (setq alist (sort alist 'org-table-formula-less-p))
  (save-excursion
    (goto-char (org-table-end))
    (if (looking-at "\\([ \t]*\n\\)*#\\+TBLFM:\\(.*\n?\\)")
	(progn
	  ;; don't overwrite TBLFM, we might use text properties to store stuff
	  (goto-char (match-beginning 2))
	  (delete-region (match-beginning 2) (match-end 0)))
      (insert "#+TBLFM:"))
    (insert " "
	    (mapconcat (lambda (x)
			 (concat
			  (if (equal (string-to-char (car x)) ?@) "" "$")
			  (car x) "=" (cdr x)))
		       alist "::")
	    "\n")))

(defsubst org-table-formula-make-cmp-string (a)
  (when (string-match "^\\(@\\([0-9]+\\)\\)?\\(\\$?\\([0-9]+\\)\\)?\\(\\$?[a-zA-Z0-9]+\\)?" a)
    (concat
     (if (match-end 2) (format "@%05d" (string-to-number (match-string 2 a))) "")
     (if (match-end 4) (format "$%05d" (string-to-number (match-string 4 a))) "")
     (if (match-end 5) (concat "@@" (match-string 5 a))))))

(defun org-table-formula-less-p (a b)
  "Compare two formulas for sorting."
  (let ((as (org-table-formula-make-cmp-string (car a)))
	(bs (org-table-formula-make-cmp-string (car b))))
    (and as bs (string< as bs))))

(defun org-table-get-stored-formulas (&optional noerror)
  "Return an alist with the stored formulas directly after current table."
  (interactive)
  (let (scol eq eq-alist strings string seen)
    (save-excursion
      (goto-char (org-table-end))
      (when (looking-at "\\([ \t]*\n\\)*#\\+TBLFM: *\\(.*\\)")
	(setq strings (org-split-string (match-string 2) " *:: *"))
	(while (setq string (pop strings))
	  (when (string-match "\\(@[0-9]+\\$[0-9]+\\|\\$\\([a-zA-Z0-9]+\\)\\) *= *\\(.*[^ \t]\\)" string)
	    (setq scol (if (match-end 2)
			   (match-string 2 string)
			 (match-string 1 string))
		  eq (match-string 3 string)
		  eq-alist (cons (cons scol eq) eq-alist))
	    (if (member scol seen)
                (if noerror
                    (progn
                      (message "Double definition `$%s=' in TBLFM line, please fix by hand" scol)
                      (ding)
                      (sit-for 2))
                  (error "Double definition `$%s=' in TBLFM line, please fix by hand" scol))
	      (push scol seen))))))
    (nreverse eq-alist)))

(defun org-table-fix-formulas (key replace &optional limit delta remove)
  "Modify the equations after the table structure has been edited.
KEY is \"@\" or \"$\".  REPLACE is an alist of numbers to replace.
For all numbers larger than LIMIT, shift them by DELTA."
  (save-excursion
    (goto-char (org-table-end))
    (when (looking-at "#\\+TBLFM:")
      (let ((re (concat key "\\([0-9]+\\)"))
	    (re2
	     (when remove
	       (if (equal key "$")
		   (format "\\(@[0-9]+\\)?\\$%d=.*?\\(::\\|$\\)" remove)
		 (format "@%d\\$[0-9]+=.*?\\(::\\|$\\)" remove))))
	    s n a)
	(when remove
	  (while (re-search-forward re2 (point-at-eol) t)
	    (replace-match "")))
	(while (re-search-forward re (point-at-eol) t)
	  (setq s (match-string 1) n (string-to-number s))
	  (cond
	   ((setq a (assoc s replace))
	    (replace-match (concat key (cdr a)) t t))
	   ((and limit (> n limit))
	    (replace-match (concat key (int-to-string (+ n delta))) t t))))))))

(defun org-table-get-specials ()
  "Get the column names and local parameters for this table."
  (save-excursion
    (let ((beg (org-table-begin)) (end (org-table-end))
	  names name fields fields1 field cnt
	  c v l line col types dlines hlines)
      (setq org-table-column-names nil
	    org-table-local-parameters nil
	    org-table-named-field-locations nil
	    org-table-current-begin-line nil
	    org-table-current-begin-pos nil
	    org-table-current-line-types nil)
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
	  (if (string-match "^\\([a-zA-Z][_a-zA-Z0-9]*\\|%\\) *= *\\(.*\\)" field)
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
	      (push (list field line col) org-table-named-field-locations))))
      ;; Analyse the line types
      (goto-char beg)
      (setq org-table-current-begin-line (org-current-line)
	    org-table-current-begin-pos (point)
	    l org-table-current-begin-line)
      (while (looking-at "[ \t]*|\\(-\\)?")
	(push (if (match-end 1) 'hline 'dline) types)
	(if (match-end 1) (push l hlines) (push l dlines))
	(beginning-of-line 2)
	(setq l (1+ l)))
      (setq org-table-current-line-types (apply 'vector (nreverse types))
	    org-table-dlines (apply 'vector (cons nil (nreverse dlines)))
	    org-table-hlines (apply 'vector (cons nil (nreverse hlines)))))))

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
	(if (or (fboundp 'calc-eval)
		(equal (substring eq 0 (min 2 (length eq))) "'("))
	    (org-table-eval-formula (if named '(4) nil)
				    (org-table-formula-from-user eq))
	  (error "Calc does not seem to be installed, and is needed to evaluate the formula"))))))

(defvar org-recalc-commands nil
  "List of commands triggering the recalculation of a line.
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
insert a new column for the markers.
When there is an active region, change all the lines in the region,
after prompting for the marking character.
After each change, a message will be displayed indicating the meaning
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
       (org-table-recalculate) t))

(defvar org-table-formula-debug nil
  "Non-nil means, debug table formulas.
When nil, simply write \"#ERROR\" in corrupted fields.")
(make-variable-buffer-local 'org-table-formula-debug)

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
					 suppress-store suppress-analysis)
  "Replace the table field value at the cursor by the result of a calculation.

This function makes use of Dave Gillespie's Calc package, in my view the
most exciting program ever written for GNU Emacs.  So you need to have Calc
installed in order to use this function.

In a table, this command replaces the value in the current field with the
result of a formula.  It also installs the formula as the \"current\" column
formula, by storing it in a special line below the table.  When called
with a `C-u' prefix, the current field must ba a named field, and the
formula is installed as valid in only this specific field.

When called with two `C-u' prefixes, insert the active equation
for the field back into the current field, so that it can be
edited there.  This is useful in order to use \\[org-table-show-reference]
to check the referenced fields.

When called, the command first prompts for a formula, which is read in
the minibuffer.  Previously entered formulas are available through the
history list, and the last used formula is offered as a default.
These stored formulas are adapted correctly when moving, inserting, or
deleting columns with the corresponding commands.

The formula can be any algebraic expression understood by the Calc package.
For details, see the Org-mode manual.

This function can also be called from Lisp programs and offers
additional arguments: EQUATION can be the formula to apply.  If this
argument is given, the user will not be prompted.  SUPPRESS-ALIGN is
used to speed-up recursive calls by by-passing unnecessary aligns.
SUPPRESS-CONST suppresses the interpretation of constants in the
formula, assuming that this has been done already outside the function.
SUPPRESS-STORE means the formula should not be stored, either because
it is already stored, or because it is a modified equation that should
not overwrite the stored one."
  (interactive "P")
  (org-table-check-inside-data-field)
  (or suppress-analysis (org-table-get-specials))
  (if (equal arg '(16))
      (let ((eq (org-table-current-field-formula)))
	(or eq (error "No equation active for current field"))
	(org-table-get-field nil eq)
	(org-table-align)
	(setq org-table-may-need-update t))
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
	   (numbers nil) ; was a variable, now fixed default
	   (keep-empty nil)
	   n form form0 bw fmt x ev orig c lispp literal)
      ;; Parse the format string.  Since we have a lot of modes, this is
      ;; a lot of work.  However, I think calc still uses most of the time.
      (if (string-match ";" formula)
	  (let ((tmp (org-split-string formula ";")))
	    (setq formula (car tmp)
		  fmt (concat (cdr (assoc "%" org-table-local-parameters))
			      (nth 1 tmp)))
	    (while (string-match "\\([pnfse]\\)\\(-?[0-9]+\\)" fmt)
	      (setq c (string-to-char (match-string 1 fmt))
		    n (string-to-number (match-string 2 fmt)))
	      (if (= c ?p)
		  (setq modes (org-set-calc-mode 'calc-internal-prec n))
		(setq modes (org-set-calc-mode
			     'calc-float-format
			     (list (cdr (assoc c '((?n . float) (?f . fix)
						   (?s . sci) (?e . eng))))
				   n))))
	      (setq fmt (replace-match "" t t fmt)))
	    (if (string-match "[NT]" fmt)
		(setq numbers (equal (match-string 0 fmt) "N")
		      fmt (replace-match "" t t fmt)))
	    (if (string-match "L" fmt)
		(setq literal t
		      fmt (replace-match "" t t fmt)))
	    (if (string-match "E" fmt)
		(setq keep-empty t
		      fmt (replace-match "" t t fmt)))
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
		      (org-no-properties
		       (buffer-substring (point-at-bol) (point-at-eol)))
		      " *| *"))
	(if (eq numbers t)
	    (setq fields (mapcar
			  (lambda (x) (number-to-string (string-to-number x)))
			  fields)))
	(setq ndown (1- ndown))
	(setq form (copy-sequence formula)
	      lispp (and (> (length form) 2)(equal (substring form 0 2) "'(")))
	(if (and lispp literal) (setq lispp 'literal))
	;; Check for old vertical references
	(setq form (org-rewrite-old-row-references form))
	;; Insert complex ranges
	(while (string-match org-table-range-regexp form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (org-table-get-range (match-string 0 form) nil n0)
		    keep-empty numbers lispp))
		 t t form)))
	;; Insert simple ranges
	(while (string-match "\\$\\([0-9]+\\)\\.\\.\\$\\([0-9]+\\)"  form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (org-sublist
		     fields (string-to-number (match-string 1 form))
		     (string-to-number (match-string 2 form)))
		    keep-empty numbers lispp))
		 t t form)))
	(setq form0 form)
	;; Insert the references to fields in same row
	(while (string-match "\\$\\([0-9]+\\)" form)
	  (setq n (string-to-number (match-string 1 form))
		x (nth (1- (if (= n 0) n0 n)) fields))
	  (unless x (error "Invalid field specifier \"%s\""
			   (match-string 0 form)))
	  (setq form (replace-match
		      (save-match-data
			(org-table-make-reference x nil numbers lispp))
		      t t form)))

	(if lispp
	    (setq ev (condition-case nil
			 (eval (eval (read form)))
		       (error "#ERROR"))
		  ev (if (numberp ev) (number-to-string ev) ev))
	  (or (fboundp 'calc-eval)
	      (error "Calc does not seem to be installed, and is needed to evaluate the formula"))
	  (setq ev (calc-eval (cons form modes)
			      (if numbers 'num))))

	(when org-table-formula-debug
	  (with-output-to-temp-buffer "*Substitution History*"
	    (princ (format "Substitution history of formula
Orig:   %s
$xyz->  %s
@r$c->  %s
$1->    %s\n" orig formula form0 form))
	    (if (listp ev)
		(princ (format "       %s^\nError:  %s"
			       (make-string (car ev) ?\-) (nth 1 ev)))
	      (princ (format "Result: %s\nFormat: %s\nFinal:  %s"
			     ev (or fmt "NONE")
			     (if fmt (format fmt (string-to-number ev)) ev)))))
	  (setq bw (get-buffer-window "*Substitution History*"))
	  (shrink-window-if-larger-than-buffer bw)
	  (unless (and (interactive-p) (not ndown))
	    (unless (let (inhibit-redisplay)
		      (y-or-n-p "Debugging Formula. Continue to next? "))
	      (org-table-align)
	      (error "Abort"))
	    (delete-window bw)
	    (message "")))
	(if (listp ev) (setq fmt nil ev "#ERROR"))
	(org-table-justify-field-maybe
	 (if fmt (format fmt (string-to-number ev)) ev))
	(if (and down (> ndown 0) (looking-at ".*\n[ \t]*|[^-]"))
	    (call-interactively 'org-return)
	  (setq ndown 0)))
      (and down (org-table-maybe-recalculate-line))
      (or suppress-align (and org-table-may-need-update
			      (org-table-align))))))

(defun org-table-put-field-property (prop value)
  (save-excursion
    (put-text-property (progn (skip-chars-backward "^|") (point))
		       (progn (skip-chars-forward "^|") (point))
		       prop value)))

(defun org-table-get-range (desc &optional tbeg col highlight)
  "Get a calc vector from a column, accorting to descriptor DESC.
Optional arguments TBEG and COL can give the beginning of the table and
the current column, to avoid unnecessary parsing.
HIGHLIGHT means, just highlight the range."
  (if (not (equal (string-to-char desc) ?@))
      (setq desc (concat "@" desc)))
  (save-excursion
    (or tbeg (setq tbeg (org-table-begin)))
    (or col (setq col (org-table-current-column)))
    (let ((thisline (org-current-line))
	  beg end c1 c2 r1 r2 rangep tmp)
      (unless (string-match org-table-range-regexp desc)
	(error "Invalid table range specifier `%s'" desc))
      (setq rangep (match-end 3)
	    r1 (and (match-end 1) (match-string 1 desc))
	    r2 (and (match-end 4) (match-string 4 desc))
	    c1 (and (match-end 2) (substring (match-string 2 desc) 1))
	    c2 (and (match-end 5) (substring (match-string 5 desc) 1)))

      (and c1 (setq c1 (+ (string-to-number c1)
			  (if (memq (string-to-char c1) '(?- ?+)) col 0))))
      (and c2 (setq c2 (+ (string-to-number c2)
			  (if (memq (string-to-char c2) '(?- ?+)) col 0))))
      (if (equal r1 "") (setq r1 nil))
      (if (equal r2 "") (setq r2 nil))
      (if r1 (setq r1 (org-table-get-descriptor-line r1)))
      (if r2 (setq r2 (org-table-get-descriptor-line r2)))
;      (setq r2 (or r2 r1) c2 (or c2 c1))
      (if (not r1) (setq r1 thisline))
      (if (not r2) (setq r2 thisline))
      (if (not c1) (setq c1 col))
      (if (not c2) (setq c2 col))
      (if (or (not rangep) (and (= r1 r2) (= c1 c2)))
	  ;; just one field
	  (progn
	    (goto-line r1)
	    (while (not (looking-at org-table-dataline-regexp))
	      (beginning-of-line 2))
	    (prog1 (org-trim (org-table-get-field c1))
	      (if highlight (org-table-highlight-rectangle (point) (point)))))
	;; A range, return a vector
	;; First sort the numbers to get a regular ractangle
	(if (< r2 r1) (setq tmp r1 r1 r2 r2 tmp))
	(if (< c2 c1) (setq tmp c1 c1 c2 c2 tmp))
	(goto-line r1)
	(while (not (looking-at org-table-dataline-regexp))
	  (beginning-of-line 2))
	(org-table-goto-column c1)
	(setq beg (point))
	(goto-line r2)
	(while (not (looking-at org-table-dataline-regexp))
	  (beginning-of-line 0))
	(org-table-goto-column c2)
	(setq end (point))
	(if highlight
	    (org-table-highlight-rectangle
	     beg (progn (skip-chars-forward "^|\n") (point))))
	;; return string representation of calc vector
	(mapcar 'org-trim
		(apply 'append (org-table-copy-region beg end)))))))

(defun org-table-get-descriptor-line (desc &optional cline bline table)
  "Analyze descriptor DESC and retrieve the corresponding line number.
The cursor is currently in line CLINE, the table begins in line BLINE,
and TABLE is a vector with line types."
  (if (string-match "^[0-9]+$" desc)
      (aref org-table-dlines (string-to-number desc))
    (setq cline (or cline (org-current-line))
	  bline (or bline org-table-current-begin-line)
	  table (or table org-table-current-line-types))
    (if (or
	 (not (string-match "^\\(\\([-+]\\)?\\(I+\\)\\)?\\(\\([-+]\\)?\\([0-9]+\\)\\)?" desc))
	 ;;                     1  2          3           4  5          6
	 (and (not (match-end 3)) (not (match-end 6)))
	 (and (match-end 3) (match-end 6) (not (match-end 5))))
	(error "invalid row descriptor `%s'" desc))
    (let* ((hdir (and (match-end 2) (match-string 2 desc)))
	   (hn (if (match-end 3) (- (match-end 3) (match-beginning 3)) nil))
	   (odir (and (match-end 5) (match-string 5 desc)))
	   (on (if (match-end 6) (string-to-number (match-string 6 desc))))
	   (i (- cline bline))
	   (rel (and (match-end 6)
		     (or (and (match-end 1) (not (match-end 3)))
			 (match-end 5)))))
      (if (and hn (not hdir))
	  (progn
	    (setq i 0 hdir "+")
	    (if (eq (aref table 0) 'hline) (setq hn (1- hn)))))
      (if (and (not hn) on (not odir))
	  (error "should never happen");;(aref org-table-dlines on)
	(if (and hn (> hn 0))
	    (setq i (org-find-row-type table i 'hline (equal hdir "-") nil hn)))
	(if on
	    (setq i (org-find-row-type table i 'dline (equal odir "-") rel on)))
	(+ bline i)))))

(defun org-find-row-type (table i type backwards relative n)
  (let ((l (length table)))
    (while (> n 0)
      (while (and (setq i (+ i (if backwards -1 1)))
		  (>= i 0) (< i l)
		  (not (eq (aref table i) type))
		  (if (and relative (eq (aref table i) 'hline))
		      (progn (setq i (- i (if backwards -1 1)) n 1) nil)
		    t)))
      (setq n (1- n)))
    (if (or (< i 0) (>= i l))
	(error "Row descriptior leads outside table")
      i)))

(defun org-rewrite-old-row-references (s)
  (if (string-match "&[-+0-9I]" s)
      (error "Formula contains old &row reference, please rewrite using @-syntax")
    s))

(defun org-table-make-reference (elements keep-empty numbers lispp)
  "Convert list ELEMENTS to something appropriate to insert into formula.
KEEP-EMPTY indicated to keep empty fields, default is to skip them.
NUMBERS indicates that everything should be converted to numbers.
LISPP means to return something appropriate for a Lisp list."
  (if (stringp elements) ; just a single val
      (if lispp
	  (if (eq lispp 'literal)
	      elements
	    (prin1-to-string (if numbers (string-to-number elements) elements)))
	(if (equal elements "") (setq elements "0"))
	(if numbers (number-to-string (string-to-number elements)) elements))
    (unless keep-empty
      (setq elements
	    (delq nil
		  (mapcar (lambda (x) (if (string-match "\\S-" x) x nil))
			  elements))))
    (setq elements (or elements '("0")))
    (if lispp
	(mapconcat
	 (lambda (x)
	   (if (eq lispp 'literal)
	       x
	     (prin1-to-string (if numbers (string-to-number x) x))))
	 elements " ")
      (concat "[" (mapconcat
		   (lambda (x)
		     (if numbers (number-to-string (string-to-number x)) x))
		   elements
		   ",") "]"))))

(defun org-table-recalculate (&optional all noalign)
  "Recalculate the current table line by applying all stored formulas.
With prefix arg ALL, do this for all lines in the table."
  (interactive "P")
  (or (memq this-command org-recalc-commands)
      (setq org-recalc-commands (cons this-command org-recalc-commands)))
  (unless (org-at-table-p) (error "Not at a table"))
  (if (equal all '(16))
      (org-table-iterate)
    (org-table-get-specials)
    (let* ((eqlist (sort (org-table-get-stored-formulas)
			 (lambda (a b) (string< (car a) (car b)))))
	   (inhibit-redisplay (not debug-on-error))
	   (line-re org-table-dataline-regexp)
	   (thisline (org-current-line))
	   (thiscol (org-table-current-column))
	   beg end entry eqlnum eqlname eqlname1 eql (cnt 0) eq a name)
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
		;; This is a table with marked lines, compute selected lines
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

      ;; First find the named fields, and mark them untouchanble
      (remove-text-properties beg end '(org-untouchable t))
      (while (setq eq (pop eqlname))
	(setq name (car eq)
	      a (assoc name org-table-named-field-locations))
	(and (not a)
	     (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" name)
	     (setq a (list name
			   (aref org-table-dlines
				 (string-to-number (match-string 1 name)))
			   (string-to-number (match-string 2 name)))))
	(when (and a (or all (equal (nth 1 a) thisline)))
	  (message "Re-applying formula to field: %s" name)
	  (goto-line (nth 1 a))
	  (org-table-goto-column (nth 2 a))
	  (push (append a (list (cdr eq))) eqlname1)
;; FIXME	  (org-table-eval-formula nil (cdr eq) 'noalign 'nocst
;; FIXME				  'nostore 'noanalysis)
	  (org-table-put-field-property :org-untouchable t)))

      ;; Now evauluate the column formulas, but skip fields covered by
      ;; field formulas
      (goto-char beg)
      (while (re-search-forward line-re end t)
	(unless (string-match "^ *[_^!$/] *$" (org-table-get-field 1))
	  ;; Unprotected line, recalculate
	  (and all (message "Re-applying formulas to full table...(line %d)"
			    (setq cnt (1+ cnt))))
	  (setq org-last-recalc-line (org-current-line))
	  (setq eql eqlnum)
	  (while (setq entry (pop eql))
	    (goto-line org-last-recalc-line)
	    (org-table-goto-column (string-to-number (car entry)) nil 'force)
	    (unless (get-text-property (point) :org-untouchable)
	      (org-table-eval-formula nil (cdr entry)
				      'noalign 'nocst 'nostore 'noanalysis)))))

      ;; Now evaluate the field formulas
      (while (setq eq (pop eqlname1))
	(message "Re-applying formula to field: %s" (car eq))
	(goto-line (nth 1 eq))
	(org-table-goto-column (nth 2 eq))
	(org-table-eval-formula nil (nth 3 eq) 'noalign 'nocst
				'nostore 'noanalysis))

      (goto-line thisline)
      (org-table-goto-column thiscol)
      (remove-text-properties (point-min) (point-max) '(org-untouchable t))
      (or noalign (and org-table-may-need-update (org-table-align))
	  (and all (message "Re-applying formulas to %d lines...done" cnt)))

      ;; back to initial position
      (message "Re-applying formulas...done")
      (goto-line thisline)
      (org-table-goto-column thiscol)
      (or noalign (and org-table-may-need-update (org-table-align))
	  (and all (message "Re-applying formulas...done"))))))

(defun org-table-iterate (&optional arg)
  "Recalculate the table until it does not change anymore."
  (interactive "P")
  (let ((imax (if arg (prefix-numeric-value arg) 10))
	(i 0)
	(lasttbl (buffer-substring (org-table-begin) (org-table-end)))
	thistbl)
    (catch 'exit
      (while (< i imax)
	(setq i (1+ i))
	(org-table-recalculate 'all)
	(setq thistbl (buffer-substring (org-table-begin) (org-table-end)))
	(if (not (string= lasttbl thistbl))
	    (setq lasttbl thistbl)
	  (if (> i 1)
	      (message "Convergence after %d iterations" i)
	    (message "Table was already stable"))
	  (throw 'exit t)))
      (error "No convergence after %d iterations" i))))

(defun org-table-formula-substitute-names (f)
  "Replace $const with values in string F."
  (let ((start 0) a (f1 f))
    ;; First, check for column names
    (while (setq start (string-match org-table-column-name-regexp f start))
      (setq start (1+ start))
      (setq a (assoc (match-string 1 f) org-table-column-names))
      (setq f (replace-match (concat "$" (cdr a)) t t f)))
    ;; Parameters and constants
    (setq start 0)
    (while (setq start (string-match "\\$\\([a-zA-Z][_a-zA-Z0-9]*\\)" f start))
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
      (cdr (assoc const org-table-formula-constants-local))
      (cdr (assoc const org-table-formula-constants))
      (and (fboundp 'constants-get) (constants-get const))
      (and (string= (substring const 0 (min 5 (length const))) "PROP_")
	   (org-entry-get nil (substring const 5) 'inherit))
      "#UNDEFINED_NAME"))

(defvar org-table-fedit-map
  (let ((map (make-sparse-keymap)))
    (org-defkey map "\C-x\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-c"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-q"      'org-table-fedit-abort)
    (org-defkey map "\C-c?"         'org-table-show-reference)
    (org-defkey map [(meta shift up)]    'org-table-fedit-line-up)
    (org-defkey map [(meta shift down)]  'org-table-fedit-line-down)
    (org-defkey map [(shift up)]    'org-table-fedit-ref-up)
    (org-defkey map [(shift down)]  'org-table-fedit-ref-down)
    (org-defkey map [(shift left)]  'org-table-fedit-ref-left)
    (org-defkey map [(shift right)] 'org-table-fedit-ref-right)
    (org-defkey map [(meta up)]     'org-table-fedit-scroll-down)
    (org-defkey map [(meta down)]   'org-table-fedit-scroll)
    (org-defkey map [(meta tab)]    'lisp-complete-symbol)
    (org-defkey map "\M-\C-i"       'lisp-complete-symbol)
    (org-defkey map [(tab)]         'org-table-fedit-lisp-indent)
    (org-defkey map "\C-i"          'org-table-fedit-lisp-indent)
    (org-defkey map "\C-c\C-r" 'org-table-fedit-toggle-ref-type)
    (org-defkey map "\C-c}"    'org-table-fedit-toggle-coordinates)
    map))

(easy-menu-define org-table-fedit-menu org-table-fedit-map "Org Edit Formulas Menu"
  '("Edit-Formulas"
    ["Finish and Install" org-table-fedit-finish t]
    ["Finish, Install, and Apply" (org-table-fedit-finish t) :keys "C-u C-c C-c"]
    ["Abort" org-table-fedit-abort t]
    "--"
    ["Pretty-Print Lisp Formula" org-table-fedit-lisp-indent t]
    ["Complete Lisp Symbol" lisp-complete-symbol t]
    "--"
    "Shift Reference at Point"
    ["Up" org-table-fedit-ref-up t]
    ["Down" org-table-fedit-ref-down t]
    ["Left" org-table-fedit-ref-left t]
    ["Right" org-table-fedit-ref-right t]
    "-"
    "Change Test Row for Column Formulas"
    ["Up" org-table-fedit-line-up t]
    ["Down" org-table-fedit-line-down t]
    "--"
    ["Scroll Table Window" org-table-fedit-scroll t]
    ["Scroll Table Window down" org-table-fedit-scroll-down t]
    ["Show Table Grid" org-table-fedit-toggle-coordinates
     :style toggle :selected (with-current-buffer (marker-buffer org-pos)
			       org-table-overlay-coordinates)]
    "--"
    ["Standard Refs (B3 instead of @3$2)" org-table-fedit-toggle-ref-type
     :style toggle :selected org-table-buffer-is-an]))

(defvar org-pos)

(defun org-table-edit-formulas ()
  "Edit the formulas of the current table in a separate buffer."
  (interactive)
  (when (save-excursion (beginning-of-line 1) (looking-at "#\\+TBLFM"))
    (beginning-of-line 0))
  (unless (org-at-table-p) (error "Not at a table"))
  (org-table-get-specials)
  (let ((key (org-table-current-field-formula 'key 'noerror))
	(eql (sort (org-table-get-stored-formulas 'noerror)
                   'org-table-formula-less-p))
	(pos (move-marker (make-marker) (point)))
	(startline 1)
	(wc (current-window-configuration))
	(titles '((column . "# Column Formulas\n")
		  (field . "# Field Formulas\n")
		  (named . "# Named Field Formulas\n")))
	entry s type title)
    (org-switch-to-buffer-other-window "*Edit Formulas*")
    (erase-buffer)
    ;; Keep global-font-lock-mode from turning on font-lock-mode
    (let ((font-lock-global-modes '(not fundamental-mode)))
      (fundamental-mode))
    (org-set-local 'font-lock-global-modes (list 'not major-mode))
    (org-set-local 'org-pos pos)
    (org-set-local 'org-window-configuration wc)
    (use-local-map org-table-fedit-map)
    (org-add-hook 'post-command-hook 'org-table-fedit-post-command t t)
    (easy-menu-add org-table-fedit-menu)
    (setq startline (org-current-line))
    (while (setq entry (pop eql))
      (setq type (cond
		  ((equal (string-to-char (car entry)) ?@) 'field)
		  ((string-match "^[0-9]" (car entry)) 'column)
		  (t 'named)))
      (when (setq title (assq type titles))
	(or (bobp) (insert "\n"))
	(insert (org-add-props (cdr title) nil 'face font-lock-comment-face))
	(setq titles (delq title titles)))
      (if (equal key (car entry)) (setq startline (org-current-line)))
      (setq s (concat (if (equal (string-to-char (car entry)) ?@) "" "$")
		      (car entry) " = " (cdr entry) "\n"))
      (remove-text-properties 0 (length s) '(face nil) s)
      (insert s))
    (if (eq org-table-use-standard-references t)
	(org-table-fedit-toggle-ref-type))
    (goto-line startline)
    (message "Edit formulas and finish with `C-c C-c'.  See menu for more commands.")))

(defun org-table-fedit-post-command ()
  (when (not (memq this-command '(lisp-complete-symbol)))
    (let ((win (selected-window)))
      (save-excursion
	(condition-case nil
	    (org-table-show-reference)
	  (error nil))
	(select-window win)))))

(defun org-table-formula-to-user (s)
  "Convert a formula from internal to user representation."
  (if (eq org-table-use-standard-references t)
      (org-table-convert-refs-to-an s)
    s))

(defun org-table-formula-from-user (s)
  "Convert a formula from user to internal representation."
  (if org-table-use-standard-references
      (org-table-convert-refs-to-rc s)
    s))

(defun org-table-convert-refs-to-rc (s)
  "Convert spreadsheet references from AB7 to @7$28.
Works for single references, but also for entire formulas and even the
full TBLFM line."
  (let ((start 0))
    (while (string-match "\\<\\([a-zA-Z]+\\)\\([0-9]+\\>\\|&\\)\\|\\(;[^\r\n:]+\\)" s start)
      (cond
       ((match-end 3)
	;; format match, just advance
	(setq start (match-end 0)))
       ((and (> (match-beginning 0) 0)
	     (equal ?. (aref s (max (1- (match-beginning 0)) 0)))
	     (not (equal ?. (aref s (max (- (match-beginning 0) 2) 0)))))
	;; 3.e5 or something like this.  FIXME: is this ok????
	(setq start (match-end 0)))
       (t
	(setq start (match-beginning 0)
	      s (replace-match
		 (if (equal (match-string 2 s) "&")
		     (format "$%d" (org-letters-to-number (match-string 1 s)))
		   (format "@%d$%d"
			   (string-to-number (match-string 2 s))
			   (org-letters-to-number (match-string 1 s))))
		 t t s)))))
    s))

(defun org-table-convert-refs-to-an (s)
  "Convert spreadsheet references from to @7$28 to AB7.
Works for single references, but also for entire formulas and even the
full TBLFM line."
  (while (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" s)
    (setq s (replace-match
	     (format "%s%d"
		     (org-number-to-letters
		      (string-to-number (match-string 2 s)))
		     (string-to-number (match-string 1 s)))
	     t t s)))
  (while (string-match "\\(^\\|[^0-9a-zA-Z]\\)\\$\\([0-9]+\\)" s)
    (setq s (replace-match (concat "\\1"
				   (org-number-to-letters
				    (string-to-number (match-string 2 s))) "&")
			   t nil s)))
  s)

(defun org-letters-to-number (s)
  "Convert a base 26 number represented by letters into an integer.
For example:  AB -> 28."
  (let ((n 0))
    (setq s (upcase s))
    (while (> (length s) 0)
	  (setq n (+ (* n 26) (string-to-char s) (- ?A) 1)
		s (substring s 1)))
    n))

(defun org-number-to-letters (n)
  "Convert an integer into a base 26 number represented by letters.
For example:  28 -> AB."
  (let ((s ""))
    (while (> n 0)
      (setq s (concat (char-to-string (+ (mod (1- n) 26) ?A)) s)
	    n (/ (1- n) 26)))
    s))

(defun org-table-fedit-convert-buffer (function)
  "Convert all references in this buffer, using FUNTION."
  (let ((line (org-current-line)))
    (goto-char (point-min))
    (while (not (eobp))
      (insert (funcall function (buffer-substring (point) (point-at-eol))))
      (delete-region (point) (point-at-eol))
      (or (eobp) (forward-char 1)))
    (goto-line line)))

(defun org-table-fedit-toggle-ref-type ()
  "Convert all references in the buffer from B3 to @3$2 and back."
  (interactive)
  (org-set-local 'org-table-buffer-is-an (not org-table-buffer-is-an))
  (org-table-fedit-convert-buffer
   (if org-table-buffer-is-an
       'org-table-convert-refs-to-an 'org-table-convert-refs-to-rc))
  (message "Reference type switched to %s"
	   (if org-table-buffer-is-an "A1 etc" "@row$column")))

(defun org-table-fedit-ref-up ()
  "Shift the reference at point one row/hline up."
  (interactive)
  (org-table-fedit-shift-reference 'up))
(defun org-table-fedit-ref-down ()
  "Shift the reference at point one row/hline down."
  (interactive)
  (org-table-fedit-shift-reference 'down))
(defun org-table-fedit-ref-left ()
  "Shift the reference at point one field to the left."
  (interactive)
  (org-table-fedit-shift-reference 'left))
(defun org-table-fedit-ref-right ()
  "Shift the reference at point one field to the right."
  (interactive)
  (org-table-fedit-shift-reference 'right))

(defun org-table-fedit-shift-reference (dir)
  (cond
   ((org-at-regexp-p "\\(\\<[a-zA-Z]\\)&")
    (if (memq dir '(left right))
	(org-rematch-and-replace 1 (eq dir 'left))
      (error "Cannot shift reference in this direction")))
   ((org-at-regexp-p "\\(\\<[a-zA-Z]\\{1,2\\}\\)\\([0-9]+\\)")
    ;; A B3-like reference
    (if (memq dir '(up down))
	(org-rematch-and-replace 2 (eq dir 'up))
      (org-rematch-and-replace 1 (eq dir 'left))))
   ((org-at-regexp-p
     "\\(@\\|\\.\\.\\)\\([-+]?\\(I+\\>\\|[0-9]+\\)\\)\\(\\$\\([-+]?[0-9]+\\)\\)?")
    ;; An internal reference
    (if (memq dir '(up down))
	(org-rematch-and-replace 2 (eq dir 'up) (match-end 3))
      (org-rematch-and-replace 5 (eq dir 'left))))))

(defun org-rematch-and-replace (n &optional decr hline)
  "Re-match the group N, and replace it with the shifted refrence."
  (or (match-end n) (error "Cannot shift reference in this direction"))
  (goto-char (match-beginning n))
  (and (looking-at (regexp-quote (match-string n)))
       (replace-match (org-shift-refpart (match-string 0) decr hline)
		      t t)))

(defun org-shift-refpart (ref &optional decr hline)
  "Shift a refrence part REF.
If DECR is set, decrease the references row/column, else increase.
If HLINE is set, this may be a hline reference, it certainly is not
a translation reference."
  (save-match-data
    (let* ((sign (string-match "^[-+]" ref)) n)

      (if sign (setq sign (substring ref 0 1) ref (substring ref 1)))
      (cond
       ((and hline (string-match "^I+" ref))
	(setq n (string-to-number (concat sign (number-to-string (length ref)))))
	(setq n (+ n (if decr -1 1)))
	(if (= n 0) (setq n (+ n (if decr -1 1))))
	(if sign
	    (setq sign (if (< n 0) "-" "+") n (abs n))
	  (setq n (max 1 n)))
	(concat sign (make-string n ?I)))

       ((string-match "^[0-9]+" ref)
	(setq n (string-to-number (concat sign ref)))
	(setq n (+ n (if decr -1 1)))
	(if sign
	    (concat (if (< n 0) "-" "+") (number-to-string (abs n)))
	  (number-to-string (max 1 n))))

       ((string-match "^[a-zA-Z]+" ref)
	(org-number-to-letters
	 (max 1 (+ (org-letters-to-number ref) (if decr -1 1)))))

       (t (error "Cannot shift reference"))))))

(defun org-table-fedit-toggle-coordinates ()
  "Toggle the display of coordinates in the refrenced table."
  (interactive)
  (let ((pos (marker-position org-pos)))
    (with-current-buffer (marker-buffer org-pos)
      (save-excursion
	(goto-char pos)
	(org-table-toggle-coordinate-overlays)))))

(defun org-table-fedit-finish (&optional arg)
  "Parse the buffer for formula definitions and install them.
With prefix ARG, apply the new formulas to the table."
  (interactive "P")
  (org-table-remove-rectangle-highlight)
  (if org-table-use-standard-references
      (progn
	(org-table-fedit-convert-buffer 'org-table-convert-refs-to-rc)
	(setq org-table-buffer-is-an nil)))
  (let ((pos org-pos) eql var form)
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(@[0-9]+\\$[0-9]+\\|\\$\\([a-zA-Z0-9]+\\)\\) *= *\\(.*\\(\n[ \t]+.*$\\)*\\)"
	    nil t)
      (setq var (if (match-end 2) (match-string 2) (match-string 1))
	    form (match-string 3))
      (setq form (org-trim form))
      (when (not (equal form ""))
	(while (string-match "[ \t]*\n[ \t]*" form)
	  (setq form (replace-match " " t t form)))
	(when (assoc var eql)
	  (error "Double formulas for %s" var))
	(push (cons var form) eql)))
    (setq org-pos nil)
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

(defun org-table-fedit-abort ()
  "Abort editing formulas, without installing the changes."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (let ((pos org-pos))
    (set-window-configuration org-window-configuration)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (move-marker pos nil)
    (message "Formula editing aborted without installing changes")))

(defun org-table-fedit-lisp-indent ()
  "Pretty-print and re-indent Lisp expressions in the Formula Editor."
  (interactive)
  (let ((pos (point)) beg end ind)
    (beginning-of-line 1)
    (cond
     ((looking-at "[ \t]")
      (goto-char pos)
      (call-interactively 'lisp-indent-line))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *[^ \t\n']") (goto-char pos))
     ((not (fboundp 'pp-buffer))
      (error "Cannot pretty-print.  Command `pp-buffer' is not available."))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *'(")
      (goto-char (- (match-end 0) 2))
      (setq beg (point))
      (setq ind (make-string (current-column) ?\ ))
      (condition-case nil (forward-sexp 1)
	(error
	 (error "Cannot pretty-print Lisp expression: Unbalanced parenthesis")))
      (setq end (point))
      (save-restriction
	(narrow-to-region beg end)
	(if (eq last-command this-command)
	    (progn
	      (goto-char (point-min))
	      (setq this-command nil)
	      (while (re-search-forward "[ \t]*\n[ \t]*" nil t)
		(replace-match " ")))
	  (pp-buffer)
	  (untabify (point-min) (point-max))
	  (goto-char (1+ (point-min)))
	  (while (re-search-forward "^." nil t)
	    (beginning-of-line 1)
	    (insert ind))
	  (goto-char (point-max))
	  (backward-delete-char 1)))
      (goto-char beg))
     (t nil))))

(defvar org-show-positions nil)

(defun org-table-show-reference (&optional local)
  "Show the location/value of the $ expression at point."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (catch 'exit
    (let ((pos (if local (point) org-pos))
          (face2 'highlight)
          (org-inhibit-highlight-removal t)
	  (win (selected-window))
	  (org-show-positions nil)
	  var name e what match dest)
      (if local (org-table-get-specials))
      (setq what (cond
		  ((or (org-at-regexp-p org-table-range-regexp2)
		       (org-at-regexp-p org-table-translate-regexp)
		       (org-at-regexp-p org-table-range-regexp))
		   (setq match
			 (save-match-data
			   (org-table-convert-refs-to-rc (match-string 0))))
		   'range)
		  ((org-at-regexp-p "\\$[a-zA-Z][a-zA-Z0-9]*") 'name)
		  ((org-at-regexp-p "\\$[0-9]+") 'column)
		  ((not local) nil)
		  (t (error "No reference at point")))
	    match (and what (or match (match-string 0))))
      (when (and  match (not (equal (match-beginning 0) (point-at-bol))))
	(org-table-add-rectangle-overlay (match-beginning 0) (match-end 0)
					 'secondary-selection))
      (org-add-hook 'before-change-functions
		    'org-table-remove-rectangle-highlight)
      (if (eq what 'name) (setq var (substring match 1)))
      (when (eq what 'range)
	(or (equal (string-to-char match) ?@) (setq match (concat "@" match)))
	(setq match (org-table-formula-substitute-names match)))
      (unless local
	(save-excursion
	  (end-of-line 1)
	  (re-search-backward "^\\S-" nil t)
	  (beginning-of-line 1)
	  (when (looking-at "\\(\\$[0-9a-zA-Z]+\\|@[0-9]+\\$[0-9]+\\|[a-zA-Z]+\\([0-9]+\\|&\\)\\) *=")
	    (setq dest
		  (save-match-data
		    (org-table-convert-refs-to-rc (match-string 1))))
	    (org-table-add-rectangle-overlay
	     (match-beginning 1) (match-end 1) face2))))
      (if (and (markerp pos) (marker-buffer pos))
	  (if (get-buffer-window (marker-buffer pos))
	      (select-window (get-buffer-window (marker-buffer pos)))
	    (org-switch-to-buffer-other-window (get-buffer-window
					    (marker-buffer pos)))))
      (goto-char pos)
      (org-table-force-dataline)
      (when dest
	(setq name (substring dest 1))
	(cond
	 ((string-match "^\\$[a-zA-Z][a-zA-Z0-9]*" dest)
	  (setq e (assoc name org-table-named-field-locations))
	  (goto-line (nth 1 e))
	  (org-table-goto-column (nth 2 e)))
	 ((string-match "^@\\([0-9]+\\)\\$\\([0-9]+\\)" dest)
	  (let ((l (string-to-number (match-string 1 dest)))
		(c (string-to-number (match-string 2 dest))))
	    (goto-line (aref org-table-dlines l))
	    (org-table-goto-column c)))
	 (t (org-table-goto-column (string-to-number name))))
	(move-marker pos (point))
	(org-table-highlight-rectangle nil nil face2))
      (cond
       ((equal dest match))
       ((not match))
       ((eq what 'range)
	(condition-case nil
	    (save-excursion
	      (org-table-get-range match nil nil 'highlight))
	  (error nil)))
       ((setq e (assoc var org-table-named-field-locations))
	(goto-line (nth 1 e))
	(org-table-goto-column (nth 2 e))
	(org-table-highlight-rectangle (point) (point))
	(message "Named field, column %d of line %d" (nth 2 e) (nth 1 e)))
       ((setq e (assoc var org-table-column-names))
	(org-table-goto-column (string-to-number (cdr e)))
	(org-table-highlight-rectangle (point) (point))
	(goto-char (org-table-begin))
	(if (re-search-forward (concat "^[ \t]*| *! *.*?| *\\(" var "\\) *|")
			       (org-table-end) t)
	    (progn
	      (goto-char (match-beginning 1))
	      (org-table-highlight-rectangle)
	      (message "Named column (column %s)" (cdr e)))
	  (error "Column name not found")))
       ((eq what 'column)
	;; column number
	(org-table-goto-column (string-to-number (substring match 1)))
	(org-table-highlight-rectangle (point) (point))
	(message "Column %s" (substring match 1)))
       ((setq e (assoc var org-table-local-parameters))
	(goto-char (org-table-begin))
	(if (re-search-forward (concat "^[ \t]*| *\\$ *.*?| *\\(" var "=\\)") nil t)
	    (progn
	      (goto-char (match-beginning 1))
	      (org-table-highlight-rectangle)
	      (message "Local parameter."))
	  (error "Parameter not found")))
       (t
	(cond
	 ((not var) (error "No reference at point"))
	 ((setq e (assoc var org-table-formula-constants-local))
	  (message "Local Constant: $%s=%s in #+CONSTANTS line."
		   var (cdr e)))
	 ((setq e (assoc var org-table-formula-constants))
	  (message "Constant: $%s=%s in `org-table-formula-constants'."
		   var (cdr e)))
	 ((setq e (and (fboundp 'constants-get) (constants-get var)))
	  (message "Constant: $%s=%s, from `constants.el'%s."
		   var e (format " (%s units)" constants-unit-system)))
	 (t (error "Undefined name $%s" var)))))
      (goto-char pos)
      (when (and org-show-positions
                 (not (memq this-command '(org-table-fedit-scroll
                                           org-table-fedit-scroll-down))))
	(push pos org-show-positions)
	(push org-table-current-begin-pos org-show-positions)
	(let ((min (apply 'min org-show-positions))
	      (max (apply 'max org-show-positions)))
          (goto-char min) (recenter 0)
          (goto-char max)
          (or (pos-visible-in-window-p max) (recenter -1))))
      (select-window win))))

(defun org-table-force-dataline ()
  "Make sure the cursor is in a dataline in a table."
  (unless (save-excursion
	    (beginning-of-line 1)
	    (looking-at org-table-dataline-regexp))
    (let* ((re org-table-dataline-regexp)
	   (p1 (save-excursion (re-search-forward re nil 'move)))
	   (p2 (save-excursion (re-search-backward re nil 'move))))
      (cond ((and p1 p2)
	     (goto-char (if (< (abs (- p1 (point))) (abs (- p2 (point))))
			    p1 p2)))
	    ((or p1 p2) (goto-char (or p1 p2)))
	    (t (error "No table dataline around here"))))))

(defun org-table-fedit-line-up ()
  "Move cursor one line up in the window showing the table."
  (interactive)
  (org-table-fedit-move 'previous-line))

(defun org-table-fedit-line-down ()
  "Move cursor one line down in the window showing the table."
  (interactive)
 (org-table-fedit-move 'next-line))

(defun org-table-fedit-move (command)
  "Move the cursor in the window shoinw the table.
Use COMMAND to do the motion, repeat if necessary to end up in a data line."
  (let ((org-table-allow-automatic-line-recalculation nil)
	(pos org-pos) (win (selected-window)) p)
    (select-window (get-buffer-window (marker-buffer org-pos)))
    (setq p (point))
    (call-interactively command)
    (while (and (org-at-table-p)
		(org-at-table-hline-p))
      (call-interactively command))
    (or (org-at-table-p) (goto-char p))
    (move-marker pos (point))
    (select-window win)))

(defun org-table-fedit-scroll (N)
  (interactive "p")
  (let ((other-window-scroll-buffer (marker-buffer org-pos)))
    (scroll-other-window N)))

(defun org-table-fedit-scroll-down (N)
  (interactive "p")
  (org-table-fedit-scroll (- N)))

(defvar org-table-rectangle-overlays nil)

(defun org-table-add-rectangle-overlay (beg end &optional face)
  "Add a new overlay."
  (let ((ov (org-make-overlay beg end)))
    (org-overlay-put ov 'face (or face 'secondary-selection))
    (push ov org-table-rectangle-overlays)))

(defun org-table-highlight-rectangle (&optional beg end face)
  "Highlight rectangular region in a table."
  (setq beg (or beg (point)) end (or end (point)))
  (let ((b (min beg end))
	(e (max beg end))
	l1 c1 l2 c2 tmp)
    (and (boundp 'org-show-positions)
	 (setq org-show-positions (cons b (cons e org-show-positions))))
    (goto-char (min beg end))
    (setq l1 (org-current-line)
	  c1 (org-table-current-column))
    (goto-char (max beg end))
    (setq l2 (org-current-line)
	  c2 (org-table-current-column))
    (if (> c1 c2) (setq tmp c1 c1 c2 c2 tmp))
    (goto-line l1)
    (beginning-of-line 1)
    (loop for line from l1 to l2 do
	  (when (looking-at org-table-dataline-regexp)
	    (org-table-goto-column c1)
	    (skip-chars-backward "^|\n") (setq beg (point))
	    (org-table-goto-column c2)
	    (skip-chars-forward "^|\n")  (setq end (point))
	    (org-table-add-rectangle-overlay beg end face))
	  (beginning-of-line 2))
    (goto-char b))
  (add-hook 'before-change-functions 'org-table-remove-rectangle-highlight))

(defun org-table-remove-rectangle-highlight (&rest ignore)
  "Remove the rectangle overlays."
  (unless org-inhibit-highlight-removal
    (remove-hook 'before-change-functions 'org-table-remove-rectangle-highlight)
    (mapc 'org-delete-overlay org-table-rectangle-overlays)
    (setq org-table-rectangle-overlays nil)))

(defvar org-table-coordinate-overlays nil
  "Collects the cooordinate grid overlays, so that they can be removed.")
(make-variable-buffer-local 'org-table-coordinate-overlays)

(defun org-table-overlay-coordinates ()
  "Add overlays to the table at point, to show row/column coordinates."
  (interactive)
  (mapc 'org-delete-overlay org-table-coordinate-overlays)
  (setq org-table-coordinate-overlays nil)
  (save-excursion
    (let ((id 0) (ih 0) hline eol s1 s2 str ic ov beg)
      (goto-char (org-table-begin))
      (while (org-at-table-p)
	(setq eol (point-at-eol))
	(setq ov (org-make-overlay (point-at-bol) (1+ (point-at-bol))))
	(push ov org-table-coordinate-overlays)
	(setq hline (looking-at org-table-hline-regexp))
	(setq str (if hline (format "I*%-2d" (setq ih (1+ ih)))
		    (format "%4d" (setq id (1+ id)))))
	(org-overlay-before-string ov str 'org-special-keyword 'evaporate)
	(when hline
	  (setq ic 0)
	  (while (re-search-forward "[+|]\\(-+\\)" eol t)
	    (setq beg (1+ (match-beginning 0))
		  ic (1+ ic)
		  s1 (concat "$" (int-to-string ic))
		  s2 (org-number-to-letters ic)
		  str (if (eq org-table-use-standard-references t) s2 s1))
	    (setq ov (org-make-overlay beg (+ beg (length str))))
	    (push ov org-table-coordinate-overlays)
	    (org-overlay-display ov str 'org-special-keyword 'evaporate)))
	(beginning-of-line 2)))))

(defun org-table-toggle-coordinate-overlays ()
  "Toggle the display of Row/Column numbers in tables."
  (interactive)
  (setq org-table-overlay-coordinates (not org-table-overlay-coordinates))
  (message "Row/Column number display turned %s"
	   (if org-table-overlay-coordinates "on" "off"))
  (if (and (org-at-table-p) org-table-overlay-coordinates)
      (org-table-align))
  (unless org-table-overlay-coordinates
    (mapc 'org-delete-overlay org-table-coordinate-overlays)
    (setq org-table-coordinate-overlays nil)))

(defun org-table-toggle-formula-debugger ()
  "Toggle the formula debugger in tables."
  (interactive)
  (setq org-table-formula-debug (not org-table-formula-debug))
  (message "Formula debugging has been turned %s"
	   (if org-table-formula-debug "on" "off")))

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

(defvar org-old-auto-fill-inhibit-regexp nil
  "Local variable used by `orgtbl-mode'")

(defconst orgtbl-line-start-regexp "[ \t]*\\(|\\|#\\+\\(TBLFM\\|ORGTBL\\):\\)"
  "Matches a line belonging to an orgtbl.")

(defconst orgtbl-extra-font-lock-keywords
  (list (list (concat "^" orgtbl-line-start-regexp ".*")
	      0 (quote 'org-table) 'prepend))
  "Extra font-lock-keywords to be added when orgtbl-mode is active.")

;;;###autoload
(defun orgtbl-mode (&optional arg)
  "The `org-mode' table editor as a minor mode for use in other modes."
  (interactive)
  (if (org-mode-p)
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
	  (org-set-local (quote org-table-may-need-update) t)
	  (org-add-hook 'before-change-functions 'org-before-change-function
			nil 'local)
	  (org-set-local 'org-old-auto-fill-inhibit-regexp
			 auto-fill-inhibit-regexp)
	  (org-set-local 'auto-fill-inhibit-regexp
			 (if auto-fill-inhibit-regexp
			     (concat orgtbl-line-start-regexp "\\|"
				     auto-fill-inhibit-regexp)
			   orgtbl-line-start-regexp))
	  (org-add-to-invisibility-spec '(org-cwidth))
	  (when (fboundp 'font-lock-add-keywords)
	    (font-lock-add-keywords nil orgtbl-extra-font-lock-keywords)
	    (org-restart-font-lock))
	  (easy-menu-add orgtbl-mode-menu)
	  (run-hooks 'orgtbl-mode-hook))
      (setq auto-fill-inhibit-regexp org-old-auto-fill-inhibit-regexp)
      (org-cleanup-narrow-column-properties)
      (org-remove-from-invisibility-spec '(org-cwidth))
      (remove-hook 'before-change-functions 'org-before-change-function t)
      (when (fboundp 'font-lock-remove-keywords)
	(font-lock-remove-keywords nil orgtbl-extra-font-lock-keywords)
	(org-restart-font-lock))
      (easy-menu-remove orgtbl-mode-menu)
      (force-mode-line-update 'all))))

(defun org-cleanup-narrow-column-properties ()
  "Remove all properties related to narrow-column invisibility."
  (let ((s 1))
    (while (setq s (text-property-any s (point-max)
				      'display org-narrow-column-arrow))
      (remove-text-properties s (1+ s) '(display t)))
    (setq s 1)
    (while (setq s (text-property-any s (point-max) 'org-cwidth 1))
      (remove-text-properties s (1+ s) '(org-cwidth t)))
    (setq s 1)
    (while (setq s (text-property-any s (point-max) 'invisible 'org-cwidth))
      (remove-text-properties s (1+ s) '(invisible t)))))

;; Install it as a minor mode.
(put 'orgtbl-mode :included t)
(put 'orgtbl-mode :menu-tag "Org Table Mode")
(add-minor-mode 'orgtbl-mode " OrgTbl" orgtbl-mode-map)

(defun orgtbl-make-binding (fun n &rest keys)
  "Create a function for binding in the table minor mode.
FUN is the command to call inside a table.  N is used to create a unique
command name.  KEYS are keys that should be checked in for a command
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
	  '("\C-c}"              org-table-toggle-coordinate-overlays)
	  '("\C-c{"              org-table-toggle-formula-debugger)
	  '("\C-m"               org-table-next-row)
	  '([(shift return)]     org-table-copy-down)
	  '("\C-c\C-q"           org-table-wrap-region)
	  '("\C-c?"              org-table-field-info)
	  '("\C-c "              org-table-blank-field)
	  '("\C-c+"              org-table-sum)
	  '("\C-c="              org-table-eval-formula)
	  '("\C-c'"              org-table-edit-formulas)
	  '("\C-c`"              org-table-edit-field)
	  '("\C-c*"              org-table-recalculate)
	  '("\C-c|"              org-table-create-or-convert-from-region)
	  '("\C-c^"              org-table-sort-lines)
	  '([(control ?#)]       org-table-rotate-recalc-marks)))
	elt key fun cmd)
    (while (setq elt (pop bindings))
      (setq nfunc (1+ nfunc))
      (setq key (org-key (car elt))
	    fun (nth 1 elt)
	    cmd (orgtbl-make-binding fun nfunc key))
      (org-defkey orgtbl-mode-map key cmd))

    ;; Special treatment needed for TAB and RET
    (org-defkey orgtbl-mode-map [(return)]
      (orgtbl-make-binding 'orgtbl-ret 100 [(return)] "\C-m"))
    (org-defkey orgtbl-mode-map "\C-m"
      (orgtbl-make-binding 'orgtbl-ret 101 "\C-m" [(return)]))

    (org-defkey orgtbl-mode-map [(tab)]
      (orgtbl-make-binding 'orgtbl-tab 102 [(tab)] "\C-i"))
    (org-defkey orgtbl-mode-map "\C-i"
      (orgtbl-make-binding 'orgtbl-tab 103 "\C-i" [(tab)]))

    (org-defkey orgtbl-mode-map [(shift tab)]
      (orgtbl-make-binding 'org-table-previous-field 104
			   [(shift tab)] [(tab)] "\C-i"))

    (org-defkey orgtbl-mode-map "\M-\C-m"
      (orgtbl-make-binding 'org-table-wrap-region 105
			   "\M-\C-m" [(meta return)]))
    (org-defkey orgtbl-mode-map [(meta return)]
      (orgtbl-make-binding 'org-table-wrap-region 106
			   [(meta return)] "\M-\C-m"))

    (org-defkey orgtbl-mode-map "\C-c\C-c" 'orgtbl-ctrl-c-ctrl-c)
    (when orgtbl-optimized
      ;; If the user wants maximum table support, we need to hijack
      ;; some standard editing functions
      (org-remap orgtbl-mode-map
		 'self-insert-command 'orgtbl-self-insert-command
		 'delete-char 'org-delete-char
		 'delete-backward-char 'org-delete-backward-char)
      (org-defkey orgtbl-mode-map "|" 'org-force-self-insert))
    (easy-menu-define orgtbl-mode-menu orgtbl-mode-map "OrgTbl menu"
      '("OrgTbl"
	["Align" org-ctrl-c-ctrl-c :active (org-at-table-p) :keys "C-c C-c"]
	["Next Field" org-cycle :active (org-at-table-p) :keys "TAB"]
	["Previous Field" org-shifttab :active (org-at-table-p) :keys "S-TAB"]
	["Next Row" org-return :active (org-at-table-p) :keys "RET"]
	"--"
	["Blank Field" org-table-blank-field :active (org-at-table-p) :keys "C-c SPC"]
	["Edit Field" org-table-edit-field :active (org-at-table-p) :keys "C-c ` "]
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
	 ["Sort lines in region" org-table-sort-lines (org-at-table-p) :keys "C-c ^"]
	 "--"
	 ["Insert Hline" org-table-insert-hline :active (org-at-table-p) :keys "C-c -"])
	("Rectangle"
	 ["Copy Rectangle" org-copy-special :active (org-at-table-p)]
	 ["Cut Rectangle" org-cut-special :active (org-at-table-p)]
	 ["Paste Rectangle" org-paste-special :active (org-at-table-p)]
	 ["Fill Rectangle" org-table-wrap-region :active (org-at-table-p)])
	"--"
	("Radio tables"
	 ["Insert table template" orgtbl-insert-radio-table
	  (assq major-mode orgtbl-radio-table-templates)]
	 ["Comment/uncomment table" orgtbl-toggle-comment t])
	"--"
	["Set Column Formula" org-table-eval-formula :active (org-at-table-p) :keys "C-c ="]
	["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
	["Edit Formulas" org-table-edit-formulas :active (org-at-table-p) :keys "C-c '"]
	["Recalculate line" org-table-recalculate :active (org-at-table-p) :keys "C-c *"]
	["Recalculate all" (org-table-recalculate '(4)) :active (org-at-table-p) :keys "C-u C-c *"]
	["Iterate all" (org-table-recalculate '(16)) :active (org-at-table-p) :keys "C-u C-u C-c *"]
	["Toggle Recalculate Mark" org-table-rotate-recalc-marks :active (org-at-table-p) :keys "C-c #"]
	["Sum Column/Rectangle" org-table-sum
	 :active (or (org-at-table-p) (org-region-active-p)) :keys "C-c +"]
	["Which Column?" org-table-current-column :active (org-at-table-p) :keys "C-c ?"]
	["Debug Formulas"
	 org-table-toggle-formula-debugger :active (org-at-table-p)
	 :keys "C-c {"
	 :style toggle :selected org-table-formula-debug]
	["Show Col/Row Numbers"
	 org-table-toggle-coordinate-overlays :active (org-at-table-p)
	 :keys "C-c }"
	 :style toggle :selected org-table-overlay-coordinates]
	))
    t))

(defun orgtbl-ctrl-c-ctrl-c (arg)
  "If the cursor is inside a table, realign the table.
It it is a table to be sent away to a receiver, do it.
With prefix arg, also recompute table."
  (interactive "P")
  (let ((pos (point)) action)
    (save-excursion
      (beginning-of-line 1)
      (setq action (cond ((looking-at "#\\+ORGTBL:.*\n[ \t]*|") (match-end 0))
			 ((looking-at "[ \t]*|") pos)
			 ((looking-at "#\\+TBLFM:") 'recalc))))
    (cond
     ((integerp action)
      (goto-char action)
      (org-table-maybe-eval-formula)
      (if arg
	  (call-interactively 'org-table-recalculate)
	(org-table-maybe-recalculate-line))
      (call-interactively 'org-table-align)
      (orgtbl-send-table 'maybe))
     ((eq action 'recalc)
      (save-excursion
	(beginning-of-line 1)
	(skip-chars-backward " \r\n\t")
	(if (org-at-table-p)
	    (org-call-with-arg 'org-table-recalculate t))))
     (t (let (orgtbl-mode)
	  (call-interactively (key-binding "\C-c\C-c")))))))

(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode'."
  (interactive "P")
  (if arg (org-table-edit-field t)
    (org-table-justify-field-maybe)
    (org-table-next-field)))

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

(defvar orgtbl-exp-regexp "^\\([-+]?[0-9][0-9.]*\\)[eE]\\([-+]?[0-9]+\\)$"
  "Regula expression matching exponentials as produced by calc.")

(defvar org-table-clean-did-remove-column nil)

(defun orgtbl-export (table target)
  (let ((func (intern (concat "orgtbl-to-" (symbol-name target))))
	(lines (org-split-string table "[ \t]*\n[ \t]*"))
	org-table-last-alignment org-table-last-column-widths
	maxcol column)
    (if (not (fboundp func))
	(error "Cannot export orgtbl table to %s" target))
    (setq lines (org-table-clean-before-export lines))
    (setq table
	  (mapcar
	   (lambda (x)
	     (if (string-match org-table-hline-regexp x)
		 'hline
	       (org-split-string (org-trim x) "\\s-*|\\s-*")))
	   lines))
    (setq maxcol (apply 'max (mapcar (lambda (x) (if (listp x) (length x) 0))
				     table)))
    (loop for i from (1- maxcol) downto 0 do
	  (setq column (mapcar (lambda (x) (if (listp x) (nth i x) nil)) table))
	  (setq column (delq nil column))
	  (push (apply 'max (mapcar 'string-width column)) org-table-last-column-widths)
	  (push (> (/ (apply '+ (mapcar (lambda (x) (if (string-match org-table-number-regexp x) 1 0)) column)) maxcol) org-table-number-fraction) org-table-last-alignment))
    (funcall func table nil)))

(defun orgtbl-send-table (&optional maybe)
  "Send a tranformed version of this table to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined for
this table."
  (interactive)
  (catch 'exit
    (unless (org-at-table-p) (error "Not at a table"))
    ;; when non-interactive, we assume align has just happened.
    (when (interactive-p) (org-table-align))
    (save-excursion
      (goto-char (org-table-begin))
      (beginning-of-line 0)
      (unless (looking-at "#\\+ORGTBL: *SEND +\\([a-zA-Z0-9_]+\\) +\\([^ \t\r\n]+\\)\\( +.*\\)?")
	(if maybe
	    (throw 'exit nil)
	  (error "Don't know how to transform this table."))))
    (let* ((name (match-string 1))
	   beg
	   (transform (intern (match-string 2)))
	   (params (if (match-end 3) (read (concat "(" (match-string 3) ")"))))
	   (skip (plist-get params :skip))
	   (skipcols (plist-get params :skipcols))
	   (txt (buffer-substring-no-properties
		 (org-table-begin) (org-table-end)))
	   (lines (nthcdr (or skip 0) (org-split-string txt "[ \t]*\n[ \t]*")))
	   (lines (org-table-clean-before-export lines))
	   (i0 (if org-table-clean-did-remove-column 2 1))
	   (table (mapcar
		   (lambda (x)
		     (if (string-match org-table-hline-regexp x)
			 'hline
		       (org-remove-by-index
			(org-split-string (org-trim x) "\\s-*|\\s-*")
			skipcols i0)))
		   lines))
	   (fun (if (= i0 2) 'cdr 'identity))
	   (org-table-last-alignment
	    (org-remove-by-index (funcall fun org-table-last-alignment)
				 skipcols i0))
	   (org-table-last-column-widths
	    (org-remove-by-index (funcall fun org-table-last-column-widths)
				 skipcols i0)))

      (unless (fboundp transform)
	(error "No such transformation function %s" transform))
      (setq txt (funcall transform table params))
      ;; Find the insertion place
      (save-excursion
	(goto-char (point-min))
	(unless (re-search-forward
		 (concat "BEGIN RECEIVE ORGTBL +" name "\\([ \t]\\|$\\)") nil t)
	  (error "Don't know where to insert translated table"))
	(goto-char (match-beginning 0))
	(beginning-of-line 2)
	(setq beg (point))
	(unless (re-search-forward (concat "END RECEIVE ORGTBL +" name) nil t)
	  (error "Cannot find end of insertion region"))
	(beginning-of-line 1)
	(delete-region beg (point))
	(goto-char beg)
	(insert txt "\n"))
      (message "Table converted and installed at receiver location"))))

(defun org-remove-by-index (list indices &optional i0)
  "Remove the elements in LIST with indices in INDICES.
First element has index 0, or I0 if given."
  (if (not indices)
      list
    (if (integerp indices) (setq indices (list indices)))
    (setq i0 (1- (or i0 0)))
    (delq :rm (mapcar (lambda (x)
			(setq i0 (1+ i0))
			(if (memq i0 indices) :rm x))
		      list))))

(defun orgtbl-toggle-comment ()
  "Comment or uncomment the orgtbl at point."
  (interactive)
  (let* ((re1 (concat "^" (regexp-quote comment-start) orgtbl-line-start-regexp))
	 (re2 (concat "^" orgtbl-line-start-regexp))
	 (commented (save-excursion (beginning-of-line 1)
			     (cond ((looking-at re1) t)
				   ((looking-at re2) nil)
				   (t (error "Not at an org table")))))
	 (re (if commented re1 re2))
	 beg end)
    (save-excursion
      (beginning-of-line 1)
      (while (looking-at re) (beginning-of-line 0))
      (beginning-of-line 2)
      (setq beg (point))
      (while (looking-at re) (beginning-of-line 2))
      (setq end (point)))
    (comment-region beg end (if commented '(4) nil))))

(defun orgtbl-insert-radio-table ()
  "Insert a radio table template appropriate for this major mode."
  (interactive)
  (let* ((e (assq major-mode orgtbl-radio-table-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (error "No radio table setup defined for %s" major-mode))
    (setq name (read-string "Table name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

(defun org-get-param (params header i sym &optional hsym)
  "Get parameter value for symbol SYM.
If this is a header line, actually get the value for the symbol with an
additional \"h\" inserted after the colon.
If the value is a protperty list, get the element for the current column.
Assumes variables VAL, PARAMS, HEAD and I to be scoped into the function."
  (let ((val (plist-get params sym)))
    (and hsym header (setq val (or (plist-get params hsym) val)))
    (if (consp val) (plist-get val i) val)))

(defun orgtbl-to-generic (table params)
  "Convert the orgtbl-mode TABLE to some other format.
This generic routine can be used for many standard cases.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
For the generic converter, some parameters are obligatory:  You need to
specify either :lfmt, or all of (:lstart :lend :sep).  If you do not use
:splice, you must have :tstart and :tend.

Valid parameters are

:tstart     String to start the table.  Ignored when :splice is t.
:tend       String to end the table.  Ignored when :splice is t.

:splice     When set to t, return only table body lines, don't wrap
            them into :tstart and :tend.  Default is nil.

:hline      String to be inserted on horizontal separation lines.
            May be nil to ignore hlines.

:lstart     String to start a new table line.
:lend       String to end a table line
:sep        Separator between two fields
:lfmt       Format for entire line, with enough %s to capture all fields.
            If this is present, :lstart, :lend, and :sep are ignored.
:fmt        A format to be used to wrap the field, should contain
            %s for the original field value.  For example, to wrap
            everything in dollars, you could use :fmt \"$%s$\".
            This may also be a property list with column numbers and
            formats. for example :fmt (2 \"$%s$\" 4 \"%s%%\")

:hlstart :hlend :hlsep :hlfmt :hfmt
            Same as above, specific for the header lines in the table.
            All lines before the first hline are treated as header.
            If any of these is not present, the data line value is used.

:efmt       Use this format to print numbers with exponentials.
            The format should have %s twice for inserting mantissa
            and exponent, for example \"%s\\\\times10^{%s}\".  This
            may also be a property list with column numbers and
            formats.  :fmt will still be applied after :efmt.

In addition to this, the parameters :skip and :skipcols are always handled
directly by `orgtbl-send-table'.  See manual."
  (interactive)
  (let* ((p params)
	 (splicep (plist-get p :splice))
	 (hline (plist-get p :hline))
	 rtn line i fm efm lfmt h)

    ;; Do we have a header?
    (if (and (not splicep) (listp (car table)) (memq 'hline table))
	(setq h t))

    ;; Put header
    (unless splicep
      (push (or (plist-get p :tstart) "ERROR: no :tstart") rtn))

    ;; Now loop over all lines
    (while (setq line (pop table))
      (if (eq line 'hline)
	  ;; A horizontal separator line
	  (progn (if hline (push hline rtn))
		 (setq h nil))               ; no longer in header
	;; A normal line.  Convert the fields, push line onto the result list
	(setq i 0)
	(setq line
	      (mapcar
	       (lambda (f)
		 (setq i (1+ i)
		       fm (org-get-param p h i :fmt :hfmt)
		       efm (org-get-param p h i :efmt))
		 (if (and efm (string-match orgtbl-exp-regexp f))
		     (setq f (format
			      efm (match-string 1 f) (match-string 2 f))))
		 (if fm (setq f (format fm f)))
		 f)
	       line))
	(if (setq lfmt (org-get-param p h i :lfmt :hlfmt))
	    (push (apply 'format lfmt line) rtn)
	  (push (concat
		 (org-get-param p h i :lstart :hlstart)
		 (mapconcat 'identity line (org-get-param p h i :sep :hsep))
		 (org-get-param p h i :lend :hlend))
		rtn))))

    (unless splicep
      (push (or (plist-get p :tend) "ERROR: no :tend") rtn))

    (mapconcat 'identity (nreverse rtn) "\n")))

(defun orgtbl-to-latex (table params)
  "Convert the orgtbl-mode TABLE to LaTeX.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Supports all parameters from `orgtbl-to-generic'.  Most important for
LaTeX are:

:splice    When set to t, return only table body lines, don't wrap
           them into a tabular environment.  Default is nil.

:fmt       A format to be used to wrap the field, should contain %s for the
           original field value.  For example, to wrap everything in dollars,
           use :fmt \"$%s$\".  This may also be a property list with column
           numbers and formats. for example :fmt (2 \"$%s$\" 4 \"%s%%\")

:efmt      Format for transforming numbers with exponentials.  The format
           should have %s twice for inserting mantissa and exponent, for
           example \"%s\\\\times10^{%s}\".  LaTeX default is \"%s\\\\,(%s)\".
           This may also be a property list with column numbers and formats.

The general parameters :skip and :skipcols have already been applied when
this function is called."
  (let* ((alignment (mapconcat (lambda (x) (if x "r" "l"))
			       org-table-last-alignment ""))
	 (params2
	  (list
	   :tstart (concat "\\begin{tabular}{" alignment "}")
	   :tend "\\end{tabular}"
	   :lstart "" :lend " \\\\" :sep " & "
	   :efmt "%s\\,(%s)" :hline "\\hline")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(defun orgtbl-to-html (table params)
  "Convert the orgtbl-mode TABLE to LaTeX.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Currently this function recognizes the following parameters:

:splice    When set to t, return only table body lines, don't wrap
           them into a <table> environment.  Default is nil.

The general parameters :skip and :skipcols have already been applied when
this function is called.  The function does *not* use `orgtbl-to-generic',
so you cannot specify parameters for it."
  (let* ((splicep (plist-get params :splice))
	 html)
    ;; Just call the formatter we already have
    ;; We need to make text lines for it, so put the fields back together.
    (setq html (org-format-org-table-html
		(mapcar
		 (lambda (x)
		   (if (eq x 'hline)
		       "|----+----|"
		     (concat "| " (mapconcat 'identity x " | ") " |")))
		 table)
		splicep))
    (if (string-match "\n+\\'" html)
	(setq html (replace-match "" t t html)))
    html))

(defun orgtbl-to-texinfo (table params)
  "Convert the orgtbl-mode TABLE to TeXInfo.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Supports all parameters from `orgtbl-to-generic'.  Most important for
TeXInfo are:

:splice nil/t      When set to t, return only table body lines, don't wrap
                   them into a multitable environment.  Default is nil.

:fmt fmt           A format to be used to wrap the field, should contain
                   %s for the original field value.  For example, to wrap
                   everything in @kbd{}, you could use :fmt \"@kbd{%s}\".
                   This may also be a property list with column numbers and
                   formats. for example :fmt (2 \"@kbd{%s}\" 4 \"@code{%s}\").

:cf \"f1 f2..\"    The column fractions for the table.  Bye default these
                   are computed automatically from the width of the columns
                   under org-mode.

The general parameters :skip and :skipcols have already been applied when
this function is called."
  (let* ((total (float (apply '+ org-table-last-column-widths)))
	 (colfrac (or (plist-get params :cf)
		      (mapconcat
		       (lambda (x) (format "%.3f" (/ (float x) total)))
		       org-table-last-column-widths " ")))
	 (params2
	  (list
	   :tstart (concat "@multitable @columnfractions " colfrac)
	   :tend "@end multitable"
	   :lstart "@item " :lend "" :sep " @tab "
	   :hlstart "@headitem ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

;;;; Link Stuff

;;; Link abbreviations

(defun org-link-expand-abbrev (link)
  "Apply replacements as defined in `org-link-abbrev-alist."
  (if (string-match "^\\([a-zA-Z][-_a-zA-Z0-9]*\\)\\(::?\\(.*\\)\\)?$" link)
      (let* ((key (match-string 1 link))
	     (as (or (assoc key org-link-abbrev-alist-local)
		     (assoc key org-link-abbrev-alist)))
	     (tag (and (match-end 2) (match-string 3 link)))
	     rpl)
	(if (not as)
	    link
	  (setq rpl (cdr as))
	  (cond
	   ((symbolp rpl) (funcall rpl tag))
	   ((string-match "%s" rpl) (replace-match (or tag "") t t rpl))
	   (t (concat rpl tag)))))
    link))

;;; Storing and inserting links

(defvar org-insert-link-history nil
  "Minibuffer history for links inserted with `org-insert-link'.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

(defvar org-store-link-plist nil
  "Plist with info about the most recently link created with `org-store-link'.")

(defvar org-link-protocols nil
  "Link protocols added to Org-mode using `org-add-link-type'.")

(defvar org-store-link-functions nil
  "List of functions that are called to create and store a link.
Each function will be called in turn until one returns a non-nil
value.  Each function should check if it is responsible for creating
this link (for example by looking at the major mode).
If not, it must exit and return nil.
If yes, it should return a non-nil value after a calling
`org-store-link-properties' with a list of properties and values.
Special properties are:

:type         The link prefix. like \"http\".  This must be given.
:link         The link, like \"http://www.astro.uva.nl/~dominik\".
              This is obligatory as well.
:description  Optional default description for the second pair
              of brackets in an Org-mode link.  The user can still change
              this when inserting this link into an Org-mode buffer.

In addition to these, any additional properties can be specified
and then used in remember templates.")

(defun org-add-link-type (type &optional follow publish)
  "Add TYPE to the list of `org-link-types'.
Re-compute all regular expressions depending on `org-link-types'
FOLLOW and PUBLISH are two functions.  Both take the link path as
an argument.
FOLLOW should do whatever is necessary to follow the link, for example
to find a file or display a mail message.
PUBLISH takes the path and retuns the string that should be used when
this document is published."
  (add-to-list 'org-link-types type t)
  (org-make-link-regexps)
  (add-to-list 'org-link-protocols
	       (list type follow publish)))

(defun org-add-agenda-custom-command (entry)
  "Replace or add a command in `org-agenda-custom-commands'.
This is mostly for hacking and trying a new command - once the command
works you probably want to add it to `org-agenda-custom-commands' for good."
  (let ((ass (assoc (car entry) org-agenda-custom-commands)))
    (if ass
	(setcdr ass (cdr entry))
      (push entry org-agenda-custom-commands))))

;;;###autoload
(defun org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link can later be inserted into an org-buffer with
\\[org-insert-link].
For some link types, a prefix arg is interpreted:
For links to usenet articles, arg negates `org-usenet-links-prefer-google'.
For file links, arg negates `org-context-in-file-links'."
  (interactive "P")
  (setq org-store-link-plist nil)  ; reset
  (let (link cpltxt desc description search txt)
    (cond

     ((run-hook-with-args-until-success 'org-store-link-functions)
      (setq link (plist-get org-store-link-plist :link)
	    desc (or (plist-get org-store-link-plist :description) link)))

     ((eq major-mode 'bbdb-mode)
      (let ((name (bbdb-record-name (bbdb-current-record)))
	    (company (bbdb-record-getprop (bbdb-current-record) 'company)))
	(setq cpltxt (concat "bbdb:" (or name company))
	      link (org-make-link cpltxt))
	(org-store-link-props :type "bbdb" :name name :company company)))

     ((eq major-mode 'Info-mode)
      (setq link (org-make-link "info:"
				(file-name-nondirectory Info-current-file)
				":" Info-current-node))
      (setq cpltxt (concat (file-name-nondirectory Info-current-file)
			   ":" Info-current-node))
      (org-store-link-props :type "info" :file Info-current-file
			    :node Info-current-node))

     ((eq major-mode 'calendar-mode)
      (let ((cd (calendar-cursor-to-date)))
	(setq link
	      (format-time-string
	       (car org-time-stamp-formats)
	       (apply 'encode-time
		      (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			    nil nil nil))))
	(org-store-link-props :type "calendar" :date cd)))

     ((or (eq major-mode 'vm-summary-mode)
	  (eq major-mode 'vm-presentation-mode))
      (and (eq major-mode 'vm-presentation-mode) (vm-summarize))
      (vm-follow-summary-cursor)
      (save-excursion
       (vm-select-folder-buffer)
       (let* ((message (car vm-message-pointer))
	      (folder buffer-file-name)
	      (subject (vm-su-subject message))
	      (to (vm-get-header-contents message "To"))
	      (from (vm-get-header-contents message "From"))
	      (message-id (vm-su-message-id message)))
	 (org-store-link-props :type "vm" :from from :to to :subject subject
			       :message-id message-id)
	 (setq message-id (org-remove-angle-brackets message-id))
	 (setq folder (abbreviate-file-name folder))
	 (if (string-match (concat "^" (regexp-quote vm-folder-directory))
			   folder)
	     (setq folder (replace-match "" t t folder)))
	 (setq cpltxt (org-email-link-description))
	 (setq link (org-make-link "vm:" folder "#" message-id)))))

     ((eq major-mode 'wl-summary-mode)
      (let* ((msgnum (wl-summary-message-number))
	     (message-id (elmo-message-field wl-summary-buffer-elmo-folder
					     msgnum 'message-id))
	     (wl-message-entity
	      (if (fboundp 'elmo-message-entity)
		  (elmo-message-entity
		   wl-summary-buffer-elmo-folder msgnum)
		  (elmo-msgdb-overview-get-entity
		   msgnum (wl-summary-buffer-msgdb))))
	     (from (wl-summary-line-from))
	     (to (car (elmo-message-entity-field wl-message-entity 'to)))
	     (subject (let (wl-thr-indent-string wl-parent-message-entity)
			(wl-summary-line-subject))))
	(org-store-link-props :type "wl" :from from :to to
			      :subject subject :message-id message-id)
	(setq message-id (org-remove-angle-brackets message-id))
	(setq cpltxt (org-email-link-description))
	(setq link (org-make-link "wl:" wl-summary-buffer-folder-name
				  "#" message-id))))

     ((or (equal major-mode 'mh-folder-mode)
	  (equal major-mode 'mh-show-mode))
      (let ((from (org-mhe-get-header "From:"))
	    (to (org-mhe-get-header "To:"))
	    (message-id (org-mhe-get-header "Message-Id:"))
	    (subject (org-mhe-get-header "Subject:")))
	(org-store-link-props :type "mh" :from from :to to
			      :subject subject :message-id message-id)
	(setq cpltxt (org-email-link-description))
	(setq link (org-make-link "mhe:" (org-mhe-get-message-real-folder) "#"
				  (org-remove-angle-brackets message-id)))))

     ((eq major-mode 'rmail-mode)
      (save-excursion
	(save-restriction
	  (rmail-narrow-to-non-pruned-header)
	  (let ((folder buffer-file-name)
		(message-id (mail-fetch-field "message-id"))
		(from (mail-fetch-field "from"))
		(to (mail-fetch-field "to"))
		(subject (mail-fetch-field "subject")))
	    (org-store-link-props
	     :type "rmail" :from from :to to
	     :subject subject :message-id message-id)
	    (setq message-id (org-remove-angle-brackets message-id))
	    (setq cpltxt (org-email-link-description))
	    (setq link (org-make-link "rmail:" folder "#" message-id))))))

     ((eq major-mode 'gnus-group-mode)
      (let ((group (cond ((fboundp 'gnus-group-group-name) ; depending on Gnus
			  (gnus-group-group-name))         ; version
			 ((fboundp 'gnus-group-name)
			  (gnus-group-name))
			 (t "???"))))
	(unless group (error "Not on a group"))
	(org-store-link-props :type "gnus" :group group)
	(setq cpltxt (concat
		      (if (org-xor arg org-usenet-links-prefer-google)
			  "http://groups.google.com/groups?group="
			"gnus:")
		      group)
	      link (org-make-link cpltxt))))

     ((memq major-mode '(gnus-summary-mode gnus-article-mode))
      (and (eq major-mode 'gnus-article-mode) (gnus-article-show-summary))
      (let* ((group gnus-newsgroup-name)
	     (article (gnus-summary-article-number))
	     (header (gnus-summary-article-header article))
	     (from (mail-header-from header))
	     (message-id (mail-header-id header))
	     (date (mail-header-date header))
	     (subject (gnus-summary-subject-string)))
	(org-store-link-props :type "gnus" :from from :subject subject
			      :message-id message-id :group group)
	(setq cpltxt (org-email-link-description))
	(if (org-xor arg org-usenet-links-prefer-google)
	    (setq link
		  (concat
		   cpltxt "\n  "
		   (format "http://groups.google.com/groups?as_umsgid=%s"
			   (org-fixup-message-id-for-http message-id))))
	  (setq link (org-make-link "gnus:" group
				    "#" (number-to-string article))))))

     ((eq major-mode 'w3-mode)
      (setq cpltxt (url-view-url t)
	    link (org-make-link cpltxt))
      (org-store-link-props :type "w3" :url (url-view-url t)))

     ((eq major-mode 'w3m-mode)
      (setq cpltxt (or w3m-current-title w3m-current-url)
	    link (org-make-link w3m-current-url))
      (org-store-link-props :type "w3m" :url (url-view-url t)))

     ((setq search (run-hook-with-args-until-success
		    'org-create-file-search-functions))
      (setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			 "::" search))
      (setq cpltxt (or description link)))

     ((eq major-mode 'image-mode)
      (setq cpltxt (concat "file:"
			   (abbreviate-file-name buffer-file-name))
	    link (org-make-link cpltxt))
      (org-store-link-props :type "image" :file buffer-file-name))

     ((eq major-mode 'dired-mode)
      ;; link to the file in the current line
      (setq cpltxt (concat "file:"
			   (abbreviate-file-name
			    (expand-file-name
			     (dired-get-filename nil t))))
	    link (org-make-link cpltxt)))

     ((and buffer-file-name (org-mode-p))
      ;; Just link to current headline
      (setq cpltxt (concat "file:"
			   (abbreviate-file-name buffer-file-name)))
      ;; Add a context search string
      (when (org-xor org-context-in-file-links arg)
	;; Check if we are on a target
	(if (org-in-regexp "<<\\(.*?\\)>>")
	    (setq cpltxt (concat cpltxt "::" (match-string 1)))
	  (setq txt (cond
		     ((org-on-heading-p) nil)
		     ((org-region-active-p)
		      (buffer-substring (region-beginning) (region-end)))
		     (t (buffer-substring (point-at-bol) (point-at-eol)))))
	  (when (or (null txt) (string-match "\\S-" txt))
	    (setq cpltxt
		  (concat cpltxt "::" (org-make-org-heading-search-string txt))
		  desc "NONE"))))
      (if (string-match "::\\'" cpltxt)
	  (setq cpltxt (substring cpltxt 0 -2)))
      (setq link (org-make-link cpltxt)))

     (buffer-file-name
      ;; Just link to this file here.
      (setq cpltxt (concat "file:"
			   (abbreviate-file-name buffer-file-name)))
      ;; Add a context string
      (when (org-xor org-context-in-file-links arg)
	(setq txt (if (org-region-active-p)
		      (buffer-substring (region-beginning) (region-end))
		    (buffer-substring (point-at-bol) (point-at-eol))))
	;; Only use search option if there is some text.
	(when (string-match "\\S-" txt)
	  (setq cpltxt
		(concat cpltxt "::" (org-make-org-heading-search-string txt))
		desc "NONE")))
      (setq link (org-make-link cpltxt)))

     ((interactive-p)
      (error "Cannot link to a buffer which is not visiting a file"))

     (t (setq link nil)))

    (if (consp link) (setq cpltxt (car link) link (cdr link)))
    (setq link (or link cpltxt)
	  desc (or desc cpltxt))
    (if (equal desc "NONE") (setq desc nil))

    (if (and (interactive-p) link)
	(progn
	  (setq org-stored-links
		(cons (list link desc) org-stored-links))
	  (message "Stored: %s" (or desc link)))
      (and link (org-make-link-string link desc)))))

(defun org-store-link-props (&rest plist)
  "Store link properties, extract names and addresses."
  (let (x adr)
    (when (setq x (plist-get plist :from))
      (setq adr (mail-extract-address-components x))
      (plist-put plist :fromname (car adr))
      (plist-put plist :fromaddress (nth 1 adr)))
    (when (setq x (plist-get plist :to))
      (setq adr (mail-extract-address-components x))
      (plist-put plist :toname (car adr))
      (plist-put plist :toaddress (nth 1 adr))))
  (let ((from (plist-get plist :from))
	(to (plist-get plist :to)))
    (when (and from to org-from-is-user-regexp)
      (plist-put plist :fromto
		 (if (string-match org-from-is-user-regexp from)
		     (concat "to %t")
		   (concat "from %f")))))
  (setq org-store-link-plist plist))

(defun org-email-link-description (&optional fmt)
  "Return the description part of an email link.
This takes information from `org-store-link-plist' and formats it
according to FMT (default from `org-email-link-description-format')."
  (setq fmt (or fmt org-email-link-description-format))
  (let* ((p org-store-link-plist)
	 (to (plist-get p :toaddress))
	 (from (plist-get p :fromaddress))
	 (table
	  (list
	   (cons "%c" (plist-get p :fromto))
	   (cons "%F" (plist-get p :from))
	   (cons "%f" (or (plist-get p :fromname) (plist-get p :fromaddress) "?"))
	   (cons "%T" (plist-get p :to))
	   (cons "%t" (or (plist-get p :toname) (plist-get p :toaddress) "?"))
	   (cons "%s" (plist-get p :subject))
	   (cons "%m" (plist-get p :message-id)))))
    (when (string-match "%c" fmt)
      ;; Check if the user wrote this message
      (if (and org-from-is-user-regexp from to
	       (save-match-data (string-match org-from-is-user-regexp from)))
	  (setq fmt (replace-match "to %t" t t fmt))
	(setq fmt (replace-match "from %f" t t fmt))))
    (org-replace-escapes fmt table)))

(defun org-make-org-heading-search-string (&optional string heading)
  "Make search string for STRING or current headline."
  (interactive)
  (let ((s (or string (org-get-heading))))
    (unless (and string (not heading))
      ;; We are using a headline, clean up garbage in there.
      (if (string-match org-todo-regexp s)
	  (setq s (replace-match "" t t s)))
      (if (string-match (org-re ":[[:alnum:]_@:]+:[ \t]*$") s)
	  (setq s (replace-match "" t t s)))
      (setq s (org-trim s))
      (if (string-match (concat "^\\(" org-quote-string "\\|"
				org-comment-string "\\)") s)
	  (setq s (replace-match "" t t s)))
      (while (string-match org-ts-regexp s)
	(setq s (replace-match "" t t s))))
    (while (string-match "[^a-zA-Z_0-9 \t]+" s)
      (setq s (replace-match " " t t s)))
    (or string (setq s (concat "*" s)))  ; Add * for headlines
    (mapconcat 'identity (org-split-string s "[ \t]+") " ")))

(defun org-make-link (&rest strings)
  "Concatenate STRINGS."
  (apply 'concat strings))

(defun org-make-link-string (link &optional description)
  "Make a link with brackets, consisting of LINK and DESCRIPTION."
  (unless (string-match "\\S-" link)
    (error "Empty link"))
  (when (stringp description)
    ;; Remove brackets from the description, they are fatal.
    (while (string-match "\\[\\|\\]" description)
      (setq description (replace-match "" t t description))))
  (when (equal (org-link-escape link) description)
    ;; No description needed, it is identical
    (setq description nil))
  (when (and (not description)
	     (not (equal link (org-link-escape link))))
    (setq description link))
  (concat "[[" (org-link-escape link) "]"
	  (if description (concat "[" description "]") "")
	  "]"))

(defconst org-link-escape-chars 
  '((" " . "%20")
    ("\340" . "%E0")  ; `a
    ("\342" . "%E2")  ; ^a  
    ("\347" . "%E7")  ; ,c
    ("\350" . "%E8")  ; `e
    ("\351" . "%E9")  ; 'e
    ("\352" . "%EA")  ; ^e
    ("\356" . "%EE")  ; ^i
    ("\364" . "%F4")  ; ^o
    ("\371" . "%F9")  ; `u
    ("\373" . "%FB")  ; ^u
    (";" . "%3B")
    ("?" . "%3F")
    ("=" . "%3D")
    ("+" . "%2B")
    )
  "Association list of escapes for some characters problematic in links.")

(defun org-link-escape (text)
  "Escape charaters in TEXT that are problematic for links."
  (when text
    (let ((re (mapconcat (lambda (x) (regexp-quote (car x)))
			 org-link-escape-chars "\\|")))
      (while (string-match re text)
	(setq text
	      (replace-match
	       (cdr (assoc (match-string 0 text) org-link-escape-chars))
	       t t text)))
      text)))

(defun org-link-unescape (text)
  "Reverse the action of `org-link-escape'."
  (when text
    (let ((re (mapconcat (lambda (x) (regexp-quote (cdr x)))
			 org-link-escape-chars "\\|")))
      (while (string-match re text)
	(setq text
	      (replace-match
	       (car (rassoc (match-string 0 text) org-link-escape-chars))
	       t t text)))
      text)))

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

;;;###autoload
(defun org-insert-link-global ()
  "Insert a link like Org-mode does.
This command can be called in any mode to insert a link in Org-mode syntax."
  (interactive)
  (org-run-like-in-org-mode 'org-insert-link))

(defun org-insert-link (&optional complete-file)
  "Insert a link.  At the prompt, enter the link.

Completion can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press RET at the prompt), the link defaults to the most recently
stored link.  As SPC triggers completion in the minibuffer, you need to
use M-SPC or C-q SPC to force the insertion of a space character.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit link
and description parts.

With a \\[universal-argument] prefix, prompts for a file to link to.  The file name can be
selected using completion.  The path to the file will be relative to
the current directory if the file is in the current directory or a
subdirectory.  Otherwise, the link will be the absolute path as
completed in the minibuffer (i.e. normally ~/path/to/file).

With two \\[universal-argument] prefixes, enforce an absolute path even if the file
is in the current directory or below.
With three \\[universal-argument] prefixes, negate the meaning of
`org-keep-stored-link-after-insertion'."
  (interactive "P")
  (let ((wcf (current-window-configuration))
	(region (if (org-region-active-p)
		    (prog1 (buffer-substring (region-beginning) (region-end))
		      (delete-region (region-beginning) (region-end)))))
        tmphist ; byte-compile incorrectly complains about this
	link desc entry remove file)
    (cond
     ((org-in-regexp org-bracket-link-regexp 1)
      ;; We do have a link at point, and we are going to edit it.
      (setq remove (list (match-beginning 0) (match-end 0)))
      (setq desc (if (match-end 3) (org-match-string-no-properties 3)))
      (setq link (read-string "Link: "
			      (org-link-unescape
			       (org-match-string-no-properties 1)))))
     ((or (org-in-regexp org-angle-link-re)
	  (org-in-regexp org-plain-link-re))
      ;; Convert to bracket link
      (setq remove (list (match-beginning 0) (match-end 0))
	    link (read-string "Link: "
			      (org-remove-angle-brackets (match-string 0)))))
     ((equal complete-file '(4))
      ;; Completing read for file names.
      (setq file (read-file-name "File: "))
      (let ((pwd (file-name-as-directory (expand-file-name ".")))
	    (pwd1 (file-name-as-directory (abbreviate-file-name
					   (expand-file-name ".")))))
	(cond
	 ((equal complete-file '(16))
	  (setq link (org-make-link
		      "file:"
		      (abbreviate-file-name (expand-file-name file)))))
	 ((string-match (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	  (setq link  (org-make-link "file:" (match-string 1 file))))
	 ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
			(expand-file-name file))
	  (setq link  (org-make-link
		       "file:" (match-string 1 (expand-file-name file)))))
	 (t (setq link (org-make-link "file:" file))))))
     (t
      ;; Read link, with completion for stored links.
      (with-output-to-temp-buffer "*Org Links*"
	(princ "Insert a link.  Use TAB to complete valid link prefixes.\n")
	(when org-stored-links
	  (princ "\nStored links are available with <up>/<down> (most recent with RET):\n\n")
	  (princ (mapconcat
		  (lambda (x)
		    (if (nth 1 x) (concat (car x) " (" (nth 1 x) ")") (car x)))
		  (reverse org-stored-links) "\n"))))
      (let ((cw (selected-window)))
	(select-window (get-buffer-window "*Org Links*"))
	(shrink-window-if-larger-than-buffer)
	(setq truncate-lines t)
	(select-window cw))
      ;; Fake a link history, containing the stored links.
      (setq tmphist (append (mapcar 'car org-stored-links)
			    org-insert-link-history))
      (unwind-protect
	  (setq link (org-completing-read
		      "Link: "
		      (append
		       (mapcar (lambda (x) (list (concat (car x) ":")))
			       (append org-link-abbrev-alist-local org-link-abbrev-alist))
		       (mapcar (lambda (x) (list (concat x ":")))
			       org-link-types))
		      nil nil nil
		      'tmphist
		      (or (car (car org-stored-links)))))
	(set-window-configuration wcf)
	(kill-buffer "*Org Links*"))
      (setq entry (assoc link org-stored-links))
      (or entry (push link org-insert-link-history))
      (if (funcall (if (equal complete-file '(64)) 'not 'identity)
		   (not org-keep-stored-link-after-insertion))
	  (setq org-stored-links (delq (assoc link org-stored-links)
				       org-stored-links)))
      (setq desc (or region desc (nth 1 entry)))))
    
    (if (string-match org-plain-link-re link)
	;; URL-like link, normalize the use of angular brackets.
	(setq link (org-make-link (org-remove-angle-brackets link))))

    ;; Check if we are linking to the current file with a search option
    ;; If yes, simplify the link by using only the search option.
    (when (and buffer-file-name
	       (string-match "\\<file:\\(.+?\\)::\\([^>]+\\)" link))
      (let* ((path (match-string 1 link))
	     (case-fold-search nil)
	     (search (match-string 2 link)))
	(save-match-data
	  (if (equal (file-truename buffer-file-name) (file-truename path))
	      ;; We are linking to this same file, with a search option
	      (setq link search)))))

    ;; Check if we can/should use a relative path.  If yes, simplify the link
    (when (string-match "\\<file:\\(.*\\)" link)
      (let* ((path (match-string 1 link))
	     (desc-is-link (equal link desc))
	     (case-fold-search nil))
	(cond
	 ((eq org-link-file-path-type 'absolute)
	  (setq path (abbreviate-file-name (expand-file-name path))))
	 ((eq org-link-file-path-type 'noabbrev)
	  (setq path (expand-file-name path)))
	 ((eq org-link-file-path-type 'relative)
	  (setq path (file-relative-name path)))
	 (t
	  (save-match-data
	    (if (string-match (concat "^" (regexp-quote
					   (file-name-as-directory
					    (expand-file-name "."))))
			      (expand-file-name path))
		;; We are linking a file with relative path name.
		(setq path (substring (expand-file-name path)
				      (match-end 0)))))))
	(setq link (concat "file:" path))
	(if desc (setq desc link))))

    (setq desc (read-string "Description: " desc))
    (unless (string-match "\\S-" desc) (setq desc nil))
    (if remove (apply 'delete-region remove))
    (insert (org-make-link-string link desc))))

(defun org-completing-read (&rest args)
  (let ((minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (org-defkey minibuffer-local-completion-map " " 'self-insert-command)
    (apply 'completing-read args)))

;;; Opening/following a link
(defvar org-link-search-failed nil)

(defun org-next-link ()
  "Move forward to the next link.
If the link is in hidden text, expose it."
  (interactive)
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-min))
    (message "Link search wrapped back to beginning of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
	 (ct (org-context))
	 (a (assoc :link ct)))
    (if a (goto-char (nth 2 a)))
    (if (re-search-forward org-any-link-re nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (if (org-invisible-p) (org-show-context)))
      (goto-char pos)
      (setq org-link-search-failed t)
      (error "No further link found"))))

(defun org-previous-link ()
  "Move backward to the previous link.
If the link is in hidden text, expose it."
  (interactive)
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-max))
    (message "Link search wrapped back to end of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
	 (ct (org-context))
	 (a (assoc :link ct)))
    (if a (goto-char (nth 1 a)))
    (if (re-search-backward org-any-link-re nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (if (org-invisible-p) (org-show-context)))
      (goto-char pos)
      (setq org-link-search-failed t)
      (error "No further link found"))))

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

(defvar org-window-config-before-follow-link nil
  "The window configuration before following a link.
This is saved in case the need arises to restore it.")

(defvar org-open-link-marker (make-marker)
  "Marker pointing to the location where `org-open-at-point; was called.")

;;;###autoload
(defun org-open-at-point-global ()
  "Follow a link like Org-mode does.
This command can be called in any mode to follow a link that has
Org-mode syntax."
  (interactive)
  (org-run-like-in-org-mode 'org-open-at-point))

(defun org-open-at-point (&optional in-emacs)
  "Open link at or after point.
If there is no link at point, this function will search forward up to
the end of the current subtree.
Normally, files will be opened by an appropriate application.  If the
optional argument IN-EMACS is non-nil, Emacs will visit the file."
  (interactive "P")
  (move-marker org-open-link-marker (point))
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (if (org-at-timestamp-p t)
      (org-follow-timestamp-link)
    (let (type path link line search (pos (point)))
      (catch 'match
	(save-excursion
	  (skip-chars-forward "^]\n\r")
	  (when (org-in-regexp org-bracket-link-regexp)
	    (setq link (org-link-unescape (org-match-string-no-properties 1)))
	    (while (string-match " *\n *" link)
	      (setq link (replace-match " " t t link)))
	    (setq link (org-link-expand-abbrev link))
	    (if (string-match org-link-re-with-space2 link)
		(setq type (match-string 1 link) path (match-string 2 link))
	      (setq type "thisfile" path link))
	    (throw 'match t)))

	(when (get-text-property (point) 'org-linked-text)
	  (setq type "thisfile"
		pos (if (get-text-property (1+ (point)) 'org-linked-text)
			(1+ (point)) (point))
		path (buffer-substring
		      (previous-single-property-change pos 'org-linked-text)
		      (next-single-property-change pos 'org-linked-text)))
	  (throw 'match t))

	(save-excursion
	  (when (or (org-in-regexp org-angle-link-re)
		    (org-in-regexp org-plain-link-re))
	    (setq type (match-string 1) path (match-string 2))
	    (throw 'match t)))
	(when (org-in-regexp "\\<\\([^><\n]+\\)\\>")
	  (setq type "tree-match"
		path (match-string 1))
	  (throw 'match t))
	(save-excursion
	  (when (org-in-regexp (org-re "\\(:[[:alnum:]_@:]+\\):[ \t]*$"))
	    (setq type "tags"
		  path (match-string 1))
	    (while (string-match ":" path)
	      (setq path (replace-match "+" t t path)))
	    (throw 'match t))))
      (unless path
	(error "No link found"))
      ;; Remove any trailing spaces in path
      (if (string-match " +\\'" path)
	  (setq path (replace-match "" t t path)))

      (cond

       ((assoc type org-link-protocols)
	(funcall (nth 1 (assoc type org-link-protocols)) path))

       ((equal type "mailto")
	(let ((cmd (car org-link-mailto-program))
	      (args (cdr org-link-mailto-program)) args1
	      (address path) (subject "") a)
	  (if (string-match "\\(.*\\)::\\(.*\\)" path)
	      (setq address (match-string 1 path)
		    subject (org-link-escape (match-string 2 path))))
	  (while args
	    (cond
	     ((not (stringp (car args))) (push (pop args) args1))
	     (t (setq a (pop args))
		(if (string-match "%a" a)
		    (setq a (replace-match address t t a)))
		(if (string-match "%s" a)
		    (setq a (replace-match subject t t a)))
		(push a args1))))
	  (apply cmd (nreverse args1))))

       ((member type '("http" "https" "ftp" "news"))
	(browse-url (concat type ":" path)))

       ((string= type "tags")
	(org-tags-view in-emacs path))
       ((string= type "thisfile")
	(if in-emacs
	    (switch-to-buffer-other-window
	     (org-get-buffer-for-internal-link (current-buffer)))
	  (org-mark-ring-push))
	(let ((cmd `(org-link-search
		     ,path
		     ,(cond ((equal in-emacs '(4)) 'occur)
			    ((equal in-emacs '(16)) 'org-occur)
			    (t nil))
		     ,pos)))
	  (condition-case nil (eval cmd)
	    (error (progn (widen) (eval cmd))))))

       ((string= type "tree-match")
	(org-occur (concat "\\[" (regexp-quote path) "\\]")))

       ((string= type "file")
	(if (string-match "::\\([0-9]+\\)\\'" path)
	    (setq line (string-to-number (match-string 1 path))
		  path (substring path 0 (match-beginning 0)))
	  (if (string-match "::\\(.+\\)\\'" path)
	      (setq search (match-string 1 path)
		    path (substring path 0 (match-beginning 0)))))
	(org-open-file path in-emacs line search))

       ((string= type "news")
	(org-follow-gnus-link path))

       ((string= type "bbdb")
	(org-follow-bbdb-link path))

       ((string= type "info")
	(org-follow-info-link path))

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

       ((string= type "mhe")
	(let (folder article)
	  (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	      (error "Error in MHE link"))
	  (setq folder (match-string 1 path)
		article (match-string 3 path))
	  (org-follow-mhe-link folder article)))

       ((string= type "rmail")
	(let (folder article)
	  (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	      (error "Error in RMAIL link"))
	  (setq folder (match-string 1 path)
		article (match-string 3 path))
	  (org-follow-rmail-link folder article)))

       ((string= type "shell")
	(let ((cmd path))
	  ;; FIXME: the following is only for backward compatibility
	  (while (string-match "@{" cmd) (setq cmd (replace-match "<" t t cmd)))
	  (while (string-match "@}" cmd) (setq cmd (replace-match ">" t t cmd)))
	  (if (or (not org-confirm-shell-link-function)
		  (funcall org-confirm-shell-link-function
			   (format "Execute \"%s\" in shell? "
				   (org-add-props cmd nil
				     'face 'org-warning))))
	      (progn
		(message "Executing %s" cmd)
		(shell-command cmd))
	    (error "Abort"))))

       ((string= type "elisp")
	(let ((cmd path))
	  (if (or (not org-confirm-elisp-link-function)
		  (funcall org-confirm-elisp-link-function
			   (format "Execute \"%s\" as elisp? "
				   (org-add-props cmd nil
						  'face 'org-warning))))
              (message "%s => %s" cmd (eval (read cmd)))
	    (error "Abort"))))

       (t
	(browse-url-at-point)))))
  (move-marker org-open-link-marker nil))


;;; File search

(defvar org-create-file-search-functions nil
  "List of functions to construct the right search string for a file link.
These functions are called in turn with point at the location to
which the link should point.

A function in the hook should first test if it would like to
handle this file type, for example by checking the major-mode or
the file extension.  If it decides not to handle this file, it
should just return nil to give other functions a chance.  If it
does handle the file, it must return the search string to be used
when following the link.  The search string will be part of the
file link, given after a double colon, and `org-open-at-point'
will automatically search for it.  If special measures must be
taken to make the search successful, another function should be
added to the companion hook `org-execute-file-search-functions',
which see.

A function in this hook may also use `setq' to set the variable
`description' to provide a suggestion for the descriptive text to
be used for this link when it gets inserted into an Org-mode
buffer with \\[org-insert-link].")

(defvar org-execute-file-search-functions nil
  "List of functions to execute a file search triggered by a link.

Functions added to this hook must accept a single argument, the
search string that was part of the file link, the part after the
double colon.  The function must first check if it would like to
handle this search, for example by checking the major-mode or the
file extension.  If it decides not to handle this search, it
should just return nil to give other functions a chance.  If it
does handle the search, it must return a non-nil value to keep
other functions from trying.

Each function can access the current prefix argument through the
variable `current-prefix-argument'.  Note that a single prefix is
used to force opening a link in Emacs, so it may be good to only
use a numeric or double prefix to guide the search function.

In case this is needed, a function in this hook can also restore
the window configuration before `org-open-at-point' was called using:

    (set-window-configuration org-window-config-before-follow-link)")

(defun org-link-search (s &optional type avoid-pos)
  "Search for a link search option.
If S is surrounded by forward slashes, it is interpreted as a
regular expression.  In org-mode files, this will create an `org-occur'
sparse tree.  In ordinary files, `occur' will be used to list matches.
If the current buffer is in `dired-mode', grep will be used to search
in all files.  If AVOID-POS is given, ignore matches near that position."
  (let ((case-fold-search t)
	(s0 (mapconcat 'identity (org-split-string s "[ \t\r\n]+") " "))
	(markers (concat "\\(?:" (mapconcat (lambda (x) (regexp-quote (car x)))
					    (append '(("") (" ") ("\t") ("\n"))
						    org-emphasis-alist)
					    "\\|") "\\)"))
	(pos (point))
	(pre "") (post "")
	words re0 re1 re2 re3 re4 re5 re2a reall)
    (cond
     ;; First check if there are any special
     ((run-hook-with-args-until-success 'org-execute-file-search-functions s))
     ;; Now try the builtin stuff
     ((save-excursion
	(goto-char (point-min))
	(and
	 (re-search-forward
	  (concat "<<" (regexp-quote s0) ">>") nil t)
	 (setq pos (match-beginning 0))))
      ;; There is an exact target for this
      (goto-char pos))
     ((string-match "^/\\(.*\\)/$" s)
      ;; A regular expression
      (cond
       ((org-mode-p)
	(org-occur (match-string 1 s)))
       ;;((eq major-mode 'dired-mode)
       ;; (grep (concat "grep -n -e '" (match-string 1 s) "' *")))
       (t (org-do-occur (match-string 1 s)))))
     (t
      ;; A normal search strings
      (when (equal (string-to-char s) ?*)
	;; Anchor on headlines, post may include tags.
	(setq pre "^\\*+[ \t]+\\(?:\\sw+\\)?[ \t]*"
	      post (org-re "[ \t]*\\(?:[ \t]+:[[:alnum:]_@:+]:[ \t]*\\)?$")
	      s (substring s 1)))
      (remove-text-properties
       0 (length s)
       '(face nil mouse-face nil keymap nil fontified nil) s)
      ;; Make a series of regular expressions to find a match
      (setq words (org-split-string s "[ \n\r\t]+")
	    re0 (concat "\\(<<" (regexp-quote s0) ">>\\)")
	    re2 (concat markers "\\(" (mapconcat 'downcase words "[ \t]+")
			"\\)" markers)
	    re2a (concat "[ \t\r\n]\\(" (mapconcat 'downcase words "[ \t\r\n]+") "\\)[ \t\r\n]")
	    re4 (concat "[^a-zA-Z_]\\(" (mapconcat 'downcase words "[^a-zA-Z_\r\n]+") "\\)[^a-zA-Z_]")
	    re1 (concat pre re2 post)
	    re3 (concat pre re4 post)
	    re5 (concat pre ".*" re4)
	    re2 (concat pre re2)
	    re2a (concat pre re2a)
	    re4 (concat pre re4)
	    reall (concat "\\(" re0 "\\)\\|\\(" re1 "\\)\\|\\(" re2
			  "\\)\\|\\(" re3 "\\)\\|\\(" re4 "\\)\\|\\("
			  re5 "\\)"
			  ))
      (cond
       ((eq type 'org-occur) (org-occur reall))
       ((eq type 'occur) (org-do-occur (downcase reall) 'cleanup))
       (t (goto-char (point-min))
	  (if (or (org-search-not-self 1 re0 nil t)
		  (org-search-not-self 1 re1 nil t)
		  (org-search-not-self 1 re2 nil t)
		  (org-search-not-self 1 re2a nil t)
		  (org-search-not-self 1 re3 nil t)
		  (org-search-not-self 1 re4 nil t)
		  (org-search-not-self 1 re5 nil t)
		  )
	      (goto-char (match-beginning 1))
	    (goto-char pos)
	    (error "No match")))))
     (t
      ;; Normal string-search
      (goto-char (point-min))
      (if (search-forward s nil t)
	  (goto-char (match-beginning 0))
	(error "No match"))))
    (and (org-mode-p) (org-show-context 'link-search))))

(defun org-search-not-self (group &rest args)
  "Execute `re-search-forward', but only accept matches that do not
enclose the position of `org-open-link-marker'."
  (let ((m org-open-link-marker))
    (catch 'exit
      (while (apply 're-search-forward args)
	(unless (get-text-property (match-end group) 'intangible) ; Emacs 21
	  (goto-char (match-end group))
	  (if (and (or (not (eq (marker-buffer m) (current-buffer)))
		       (> (match-beginning 0) (marker-position m))
		       (< (match-end 0) (marker-position m)))
		   (save-match-data
		     (or (not (org-in-regexp
			       org-bracket-link-analytic-regexp 1))
			 (not (match-end 4))  ; no description
			 (and (<= (match-beginning 4) (point))
			      (>= (match-end 4) (point))))))
	      (throw 'exit (point))))))))

(defun org-get-buffer-for-internal-link (buffer)
  "Return a buffer to be used for displaying the link target of internal links."
  (cond
   ((not org-display-internal-link-with-indirect-buffer)
    buffer)
   ((string-match "(Clone)$" (buffer-name buffer))
    (message "Buffer is already a clone, not making another one")
    ;; we also do not modify visibility in this case
    buffer)
   (t ; make a new indirect buffer for displaying the link
    (let* ((bn (buffer-name buffer))
	   (ibn (concat bn "(Clone)"))
	   (ib (or (get-buffer ibn) (make-indirect-buffer buffer ibn 'clone))))
      (with-current-buffer ib (org-overview))
      ib))))

(defun org-do-occur (regexp &optional cleanup)
  "Call the Emacs command `occur'.
If CLEANUP is non-nil, remove the printout of the regular expression
in the *Occur* buffer.  This is useful if the regex is long and not useful
to read."
  (occur regexp)
  (when cleanup
    (let ((cwin (selected-window)) win beg end)
      (when (setq win (get-buffer-window "*Occur*"))
	(select-window win))
      (goto-char (point-min))
      (when (re-search-forward "match[a-z]+" nil t)
	(setq beg (match-end 0))
	(if (re-search-forward "^[ \t]*[0-9]+" nil t)
	    (setq end (1- (match-beginning 0)))))
      (and beg end (let ((inhibit-read-only t)) (delete-region beg end)))
      (goto-char (point-min))
      (select-window cwin))))

;;; The mark ring for links jumps

(defvar org-mark-ring nil
  "Mark ring for positions before jumps in Org-mode.")
(defvar org-mark-ring-last-goto nil
  "Last position in the mark ring used to go back.")
;; Fill and close the ring
(setq org-mark-ring nil org-mark-ring-last-goto nil) ;; in case file is reloaded
(loop for i from 1 to org-mark-ring-length do
      (push (make-marker) org-mark-ring))
(setcdr (nthcdr (1- org-mark-ring-length) org-mark-ring)
	org-mark-ring)

(defun org-mark-ring-push (&optional pos buffer)
  "Put the current position or POS into the mark ring and rotate it."
  (interactive)
  (setq pos (or pos (point)))
  (setq org-mark-ring (nthcdr (1- org-mark-ring-length) org-mark-ring))
  (move-marker (car org-mark-ring)
	       (or pos (point))
	       (or buffer (current-buffer)))
  (message
   (substitute-command-keys
    "Position saved to mark ring, go back with \\[org-mark-ring-goto].")))

(defun org-mark-ring-goto (&optional n)
  "Jump to the previous position in the mark ring.
With prefix arg N, jump back that many stored positions.  When
called several times in succession, walk through the entire ring.
Org-mode commands jumping to a different position in the current file,
or to another Org-mode file, automatically push the old position
onto the ring."
  (interactive "p")
  (let (p m)
    (if (eq last-command this-command)
	(setq p (nthcdr n (or org-mark-ring-last-goto org-mark-ring)))
      (setq p org-mark-ring))
    (setq org-mark-ring-last-goto p)
    (setq m (car p))
    (switch-to-buffer (marker-buffer m))
    (goto-char m)
    (if (or (org-invisible-p) (org-invisible-p2)) (org-show-context 'mark-goto))))

(defun org-remove-angle-brackets (s)
  (if (equal (substring s 0 1) "<") (setq s (substring s 1)))
  (if (equal (substring s -1) ">") (setq s (substring s 0 -1)))
  s)
(defun org-add-angle-brackets (s)
  (if (equal (substring s 0 1) "<") nil (setq s (concat "<" s)))
  (if (equal (substring s -1) ">") nil (setq s (concat s ">")))
  s)

;;; Following specific links

(defun org-follow-timestamp-link ()
  (cond
   ((org-at-date-range-p t)
    (let ((org-agenda-start-on-weekday)
	  (t1 (match-string 1))
	  (t2 (match-string 2)))
      (setq t1 (time-to-days (org-time-string-to-time t1))
	    t2 (time-to-days (org-time-string-to-time t2)))
      (org-agenda-list nil t1 (1+ (- t2 t1)))))
   ((org-at-timestamp-p t)
    (org-agenda-list nil (time-to-days (org-time-string-to-time
					(substring (match-string 1) 0 10)))
		     1))
   (t (error "This should not happen"))))


(defun org-follow-bbdb-link (name)
  "Follow a BBDB link to NAME."
  (require 'bbdb)
  (let ((inhibit-redisplay (not debug-on-error))
	(bbdb-electric-p nil))
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

(defun org-follow-info-link (name)
  "Follow an info file & node link  to NAME."
  (if (or (string-match "\\(.*\\)::?\\(.*\\)" name)
          (string-match "\\(.*\\)" name))
      (progn
	(require 'info)
        (if (match-string 2 name) ; If there isn't a node, choose "Top"
            (Info-find-node (match-string 1 name) (match-string 2 name))
          (Info-find-node (match-string 1 name) "Top")))
    (message (concat "Could not open: " name))))

(defun org-follow-gnus-link (&optional group article)
  "Follow a Gnus link to GROUP and ARTICLE."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if gnus-other-frame-object (select-frame gnus-other-frame-object))
  (cond ((and group article)
	 (gnus-group-read-group 1 nil group)
	 (gnus-summary-goto-article (string-to-number article) nil t))
	(group (gnus-group-jump-to-group group))))

(defun org-follow-vm-link (&optional folder article readonly)
  "Follow a VM link to FOLDER and ARTICLE."
  (require 'vm)
  (setq article (org-add-angle-brackets article))
  (if (string-match "^//\\([a-zA-Z]+@\\)?\\([^:]+\\):\\(.*\\)" folder)
      ;; ange-ftp or efs or tramp access
      (let ((user (or (match-string 1 folder) (user-login-name)))
	    (host (match-string 2 folder))
	    (file (match-string 3 folder)))
	(cond
	 ((featurep 'tramp)
	  ;; use tramp to access the file
	  (if (featurep 'xemacs)
	      (setq folder (format "[%s@%s]%s" user host file))
	    (setq folder (format "/%s@%s:%s" user host file))))
	 (t
	  ;; use ange-ftp or efs
	  (require (if (featurep 'xemacs) 'efs 'ange-ftp))
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
  (if (and (string= folder "%")
	   article
	   (string-match "^\\([^#]+\\)\\(#\\(.*\\)\\)?" article))
      ;; XXX: imap-uw supports folders starting with '#' such as "#mh/inbox".
      ;; Thus, we recompose folder and article ids.
      (setq folder (format "%s#%s" folder (match-string 1 article))
	    article (match-string 3 article)))
  (if (not (elmo-folder-exists-p (wl-folder-get-elmo-folder folder)))
      (error "No such folder: %s" folder))
  (wl-summary-goto-folder-subr folder 'no-sync t nil t nil nil)
  (and article
       (wl-summary-jump-to-msg-by-message-id (org-add-angle-brackets article))
       (wl-summary-redisplay)))

(defun org-follow-rmail-link (folder article)
  "Follow an RMAIL link to FOLDER and ARTICLE."
  (setq article (org-add-angle-brackets article))
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

;;; mh-e integration based on planner-mode
(defun org-mhe-get-message-real-folder ()
  "Return the name of the current message real folder, so if you use
sequences, it will now work."
  (save-excursion
    (let* ((folder
            (if (equal major-mode 'mh-folder-mode)
                mh-current-folder
              ;; Refer to the show buffer
              mh-show-folder-buffer))
           (end-index
            (if (boundp 'mh-index-folder)
                (min (length mh-index-folder) (length folder))))
           )
      ;; a simple test on mh-index-data does not work, because
      ;; mh-index-data is always nil in a show buffer.
      (if (and (boundp 'mh-index-folder)
               (string= mh-index-folder (substring folder 0 end-index)))
          (if (equal major-mode 'mh-show-mode)
              (save-window-excursion
		(let (pop-up-frames)
		  (when (buffer-live-p (get-buffer folder))
		    (progn
		      (pop-to-buffer folder)
		      (org-mhe-get-message-folder-from-index)
		      )
		    )))
            (org-mhe-get-message-folder-from-index)
            )
        folder
        )
      )))

(defun org-mhe-get-message-folder-from-index ()
  "Returns the name of the message folder in a index folder buffer."
  (save-excursion
    (mh-index-previous-folder)
    (re-search-forward "^\\(+.*\\)$" nil t)
    (message (match-string 1))))

(defun org-mhe-get-message-folder ()
  "Return the name of the current message folder.  Be careful if you
use sequences."
  (save-excursion
    (if (equal major-mode 'mh-folder-mode)
        mh-current-folder
      ;; Refer to the show buffer
      mh-show-folder-buffer)))

(defun org-mhe-get-message-num ()
  "Return the number of the current message.  Be careful if you
use sequences."
  (save-excursion
    (if (equal major-mode 'mh-folder-mode)
        (mh-get-msg-num nil)
      ;; Refer to the show buffer
      (mh-show-buffer-message-number))))

(defun org-mhe-get-header (header)
  "Return a header of the message in folder mode. This will create a
show buffer for the corresponding message. If you have a more clever
idea..."
  (let* ((folder (org-mhe-get-message-folder))
         (num (org-mhe-get-message-num))
         (buffer (get-buffer-create (concat "show-" folder)))
         (header-field))
  (with-current-buffer buffer
    (mh-display-msg num folder)
    (if (equal major-mode 'mh-folder-mode)
        (mh-header-display)
      (mh-show-header-display))
    (set-buffer buffer)
    (setq header-field (mh-get-header-field header))
    (if (equal major-mode 'mh-folder-mode)
        (mh-show)
      (mh-show-show))
    header-field)))

(defun org-follow-mhe-link (folder article)
  "Follow an MHE link to FOLDER and ARTICLE.
If ARTICLE is nil FOLDER is shown.  If the configuration variable
`org-mhe-search-all-folders' is t and `mh-searcher' is pick,
ARTICLE is searched in all folders.  Indexed searches (swish++,
namazu, and others supported by MH-E) will always search in all
folders."
  (require 'mh-e)
  (require 'mh-search)
  (require 'mh-utils)
  (mh-find-path)
  (if (not article)
      (mh-visit-folder (mh-normalize-folder-name folder))
    (setq article (org-add-angle-brackets article))
    (mh-search-choose)
    (if (equal mh-searcher 'pick)
        (progn
          (mh-search folder (list "--message-id" article))
          (when (and org-mhe-search-all-folders
                     (not (org-mhe-get-message-real-folder)))
            (kill-this-buffer)
            (mh-search "+" (list "--message-id" article))))
      (mh-search "+" article))
    (if (org-mhe-get-message-real-folder)
        (mh-show-msg 1)
      (kill-this-buffer)
      (error "Message not found"))))

;;; BibTeX links

;; Use the custom search meachnism to construct and use search strings for
;; file links to BibTeX database entries.

(defun org-create-file-search-in-bibtex ()
  "Create the search string and description for a BibTeX database entry."
  (when (eq major-mode 'bibtex-mode)
    ;; yes, we want to construct this search string.
    ;; Make a good description for this entry, using names, year and the title
    ;; Put it into the `description' variable which is dynamically scoped.
    (let ((bibtex-autokey-names 1)
	  (bibtex-autokey-names-stretch 1)
	  (bibtex-autokey-name-case-convert-function 'identity)
	  (bibtex-autokey-name-separator " & ")
	  (bibtex-autokey-additional-names " et al.")
	  (bibtex-autokey-year-length 4)
	  (bibtex-autokey-name-year-separator " ")
	  (bibtex-autokey-titlewords 3)
	  (bibtex-autokey-titleword-separator " ")
	  (bibtex-autokey-titleword-case-convert-function 'identity)
	  (bibtex-autokey-titleword-length 'infty)
	  (bibtex-autokey-year-title-separator ": "))
      (setq description (bibtex-generate-autokey)))
    ;; Now parse the entry, get the key and return it.
    (save-excursion
      (bibtex-beginning-of-entry)
      (cdr (assoc "=key=" (bibtex-parse-entry))))))

(defun org-execute-file-search-in-bibtex (s)
  "Find the link search string S as a key for a database entry."
  (when (eq major-mode 'bibtex-mode)
    ;; Yes, we want to do the search in this file.
    ;; We construct a regexp that searches for "@entrytype{" followed by the key
    (goto-char (point-min))
    (and (re-search-forward (concat "@[a-zA-Z]+[ \t\n]*{[ \t\n]*"
				    (regexp-quote s) "[ \t\n]*,") nil t)
	 (goto-char (match-beginning 0)))
    (if (and (match-beginning 0) (equal current-prefix-arg '(16)))
	;; Use double prefix to indicate that any web link should be browsed
	(let ((b (current-buffer)) (p (point)))
	  ;; Restore the window configuration because we just use the web link
	  (set-window-configuration org-window-config-before-follow-link)
	  (save-excursion (set-buffer b) (goto-char p)
	    (bibtex-url)))
      (recenter 0))  ; Move entry start to beginning of window
  ;; return t to indicate that the search is done.
    t))

;; Finally add the functions to the right hooks.
(add-hook 'org-create-file-search-functions 'org-create-file-search-in-bibtex)
(add-hook 'org-execute-file-search-functions 'org-execute-file-search-in-bibtex)

;; end of Bibtex link setup

;;; Following file links

(defun org-open-file (path &optional in-emacs line search)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.
If no application is found, Emacs simply visits the file.
With optional argument IN-EMACS, Emacs will visit the file.
Optional LINE specifies a line to go to, optional SEARCH a string to
search for.  If LINE or SEARCH is given, the file will always be
opened in Emacs.
If the file does not exist, an error is thrown."
  (setq in-emacs (or in-emacs line search))
  (let* ((file (if (equal path "")
		   buffer-file-name
		 (substitute-in-file-name (expand-file-name path))))
	 (apps (append org-file-apps (org-default-apps)))
	 (remp (and (assq 'remote apps) (org-file-remote-p file)))
	 (dirp (if remp nil (file-directory-p file)))
	 (dfile (downcase file))
	 (old-buffer (current-buffer))
	 (old-pos (point))
	 (old-mode major-mode)
	 ext cmd)
    (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\.gz\\)$" dfile)
	(setq ext (match-string 1 dfile))
      (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\)$" dfile)
	  (setq ext (match-string 1 dfile))))
    (if in-emacs
	(setq cmd 'emacs)
      (setq cmd (or (and remp (cdr (assoc 'remote apps)))
		    (and dirp (cdr (assoc 'directory apps)))
		    (cdr (assoc ext apps))
		    (cdr (assoc t apps)))))
    (when (eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or ext "")))
	     (command (mailcap-mime-info mime-type)))
	(if (stringp command)
	    (setq cmd command)
	  (setq cmd 'emacs))))
    (if (and (not (eq cmd 'emacs)) ; Emacs has no problems with non-ex files
	     (not (file-exists-p file))
	     (not org-open-non-existing-files))
	(error "No such file: %s" file))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      ;; Remove quotes around the file name - we'll use shell-quote-argument.
      (if (string-match "['\"]%s['\"]" cmd)
	  (setq cmd (replace-match "%s" t t cmd)))
      (setq cmd (format cmd (shell-quote-argument file)))
      (save-window-excursion
	(shell-command (concat cmd " &"))))
     ((or (stringp cmd)
	  (eq cmd 'emacs))
      (funcall (cdr (assq 'file org-link-frame-setup)) file)
      (widen)
      (if line (goto-line line)
	(if search (org-link-search search))))
     ((consp cmd)
      (eval cmd))
     (t (funcall (cdr (assq 'file org-link-frame-setup)) file)))
    (and (org-mode-p) (eq old-mode 'org-mode)
	 (or (not (equal old-buffer (current-buffer)))
	     (not (equal old-pos (point))))
	 (org-mark-ring-push old-pos old-buffer))))

(defun org-default-apps ()
  "Return the default applications for this operating system."
  (cond
   ((eq system-type 'darwin)
    org-file-apps-defaults-macosx)
   ((eq system-type 'windows-nt)
    org-file-apps-defaults-windowsnt)
   (t org-file-apps-defaults-gnu)))

(defvar ange-ftp-name-format) ; to silence the XEmacs compiler.
(defun org-file-remote-p (file)
  "Test whether FILE specifies a location on a remote system.
Return non-nil if the location is indeed remote.

For example, the filename \"/user@host:/foo\" specifies a location
on the system \"/user@host:\"."
  (cond ((fboundp 'file-remote-p)
         (file-remote-p file))
        ((fboundp 'tramp-handle-file-remote-p)
         (tramp-handle-file-remote-p file))
        ((and (boundp 'ange-ftp-name-format)
              (string-match (car ange-ftp-name-format) file))
         t)
        (t nil)))


;;;; Hooks for remember.el

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
(defun org-remember-apply-template (&optional use-char skip-interactive)
  "Initialize *remember* buffer with template, invoke `org-mode'.
This function should be placed into `remember-mode-hook' and in fact requires
to be run from that hook to fucntion properly."
  (if org-remember-templates

      (let* ((char (or use-char
		       (if (= (length org-remember-templates) 1)
			   (caar org-remember-templates)
			 (message "Select template: %s"
				  (mapconcat
				   (lambda (x) (char-to-string (car x)))
				   org-remember-templates " "))
			 (read-char-exclusive))))
	     (entry (cdr (assoc char org-remember-templates)))
	     (tpl (car entry))
	     (plist-p (if org-store-link-plist t nil))
	     (file (if (and (nth 1 entry) (stringp (nth 1 entry))
			    (string-match "\\S-" (nth 1 entry)))
		       (nth 1 entry)
		     org-default-notes-file))
	     (headline (nth 2 entry))
	     (v-t (format-time-string (car org-time-stamp-formats) (org-current-time)))
	     (v-T (format-time-string (cdr org-time-stamp-formats) (org-current-time)))
	     (v-u (concat "[" (substring v-t 1 -1) "]"))
	     (v-U (concat "[" (substring v-T 1 -1) "]"))
	     (v-i initial)      ; defined in `remember-mode'
	     (v-a (if (equal annotation "[[]]") "" annotation)) ; likewise
	     (v-A (if (string-match "\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" v-a)
		      (replace-match "[\\1[%^{Link description}]]" nil nil v-a)
		    v-a))
	     (v-n user-full-name)
	     (org-startup-folded nil)
	     org-time-was-given org-end-time-was-given x prompt char time)
	(setq org-store-link-plist
	      (append (list :annotation v-a :initial v-i)
		      org-store-link-plist))
	(unless tpl (setq tpl "")	(message "No template") (ding))
	(erase-buffer)
	(insert (substitute-command-keys
		 (format
		  "## `%sC-c C-c' to file directly, `%sC-c C-c' to file interactively.
## Target file \"%s\", headline \"%s\"
## To switch templates, use `\\[org-remember]'.\n\n"
		  (if org-remember-store-without-prompt "" "C-u ")
		  (if org-remember-store-without-prompt "C-u " "")
		  (abbreviate-file-name (or file org-default-notes-file))
		  (or headline ""))))
	(insert tpl) (goto-char (point-min))
	;; Simple %-escapes
	(while (re-search-forward "%\\([tTuUaiA]\\)" nil t)
	  (when (and initial (equal (match-string 0) "%i"))
	    (save-match-data
	      (let* ((lead (buffer-substring
			    (point-at-bol) (match-beginning 0))))
		(setq v-i (mapconcat 'identity
				     (org-split-string initial "\n")
				     (concat "\n" lead))))))
	  (replace-match
	   (or (eval (intern (concat "v-" (match-string 1)))) "")
	   t t))
	;; From the property list
	(when plist-p
	  (goto-char (point-min))
	  (while (re-search-forward "%\\(:[-a-zA-Z]+\\)" nil t)
	    (and (setq x (plist-get org-store-link-plist
				    (intern (match-string 1))))
		 (replace-match x t t))))
	;; Turn on org-mode in the remember buffer, set local variables
	(org-mode)
	(org-set-local 'org-finish-function 'remember-buffer)
	(if (and file (string-match "\\S-" file) (not (file-directory-p file)))
	    (org-set-local 'org-default-notes-file file))
	(if (and headline (stringp headline) (string-match "\\S-" headline))
	    (org-set-local 'org-remember-default-headline headline))
	;; Interactive template entries
	(goto-char (point-min))
	(while (re-search-forward "%^\\({\\([^}]*\\)}\\)?\\([guUtT]\\)?" nil t)
	  (setq char (if (match-end 3) (match-string 3))
		prompt (if (match-end 2) (match-string 2)))
	  (goto-char (match-beginning 0))
	  (replace-match "")
	  (cond
	   ((member char '("G" "g"))
	    (let* ((org-last-tags-completion-table
		    (org-global-tags-completion-table
		     (if (equal char "G") (org-agenda-files) (and file (list file)))))
		   (org-add-colon-after-tag-completion t)
		   (ins (completing-read
			 (if prompt (concat prompt ": ") "Tags: ")
			 'org-tags-completion-function nil nil nil
			 'org-tags-history)))
	      (setq ins (mapconcat 'identity
				  (org-split-string ins (org-re "[^[:alnum:]]+"))
				  ":"))
	      (when (string-match "\\S-" ins)
		(or (equal (char-before) ?:) (insert ":"))
		(insert ins)
		(or (equal (char-after) ?:) (insert ":")))))		
	   (char
	    (setq org-time-was-given (equal (upcase char) char))
	    (setq time (org-read-date (equal (upcase char) "U") t nil
				      prompt))
	    (org-insert-time-stamp time org-time-was-given
				   (member char '("u" "U"))
				   nil nil (list org-end-time-was-given)))
	   (t
	    (insert (read-string
		     (if prompt (concat prompt ": ") "Enter string"))))))
	(goto-char (point-min))
	(if (re-search-forward "%\\?" nil t)
	    (replace-match "")
	  (and (re-search-forward "^[^#\n]" nil t) (backward-char 1))))
    (org-mode)
    (org-set-local 'org-finish-function 'remember-buffer)))

;;;###autoload
(defun org-remember ()
  "Call `remember'.  If this is already a remember buffer, re-apply template.
If there is an active region, make sure remember uses it as initial content
of the remember buffer."
  (interactive)
  (if (eq org-finish-function 'remember-buffer)
      (progn
	(when (< (length org-remember-templates) 2)
	  (error "No other template available"))
	(erase-buffer)
	(let ((annotation (plist-get org-store-link-plist :annotation))
	      (initial (plist-get org-store-link-plist :initial)))
	  (org-remember-apply-template))
	(message "Press C-c C-c to remember data"))
    (if (org-region-active-p)
	(remember (buffer-substring (point) (mark)))
      (call-interactively 'remember))))

;;;###autoload
(defun org-remember-handler ()
  "Store stuff from remember.el into an org file.
First prompts for an org file.  If the user just presses return, the value
of `org-default-notes-file' is used.
Then the command offers the headings tree of the selected file in order to
file the text at a specific location.
You can either immediately press RET to get the note appended to the
file, or you can use vertical cursor motion and visibility cycling (TAB) to
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
interrupted, in accordance with the principles of remember.el.
You can also get the fast execution without prompting by using
C-u C-c C-c to exit the remember buffer.  See also the variable
`org-remember-store-without-prompt'.

Before being stored away, the function ensures that the text has a
headline, i.e. a first line that starts with a \"*\".  If not, a headline
is constructed from the current date and some additional data.

If the variable `org-adapt-indentation' is non-nil, the entire text is
also indented so that it starts in the same column as the headline
\(i.e. after the stars).

See also the variable `org-reverse-note-order'."
  (goto-char (point-min))
  (while (looking-at "^[ \t]*\n\\|^##.*\n")
    (replace-match ""))
  (catch 'quit
    (let* ((txt (buffer-substring (point-min) (point-max)))
	   (fastp (org-xor (equal current-prefix-arg '(4))
			   org-remember-store-without-prompt))
	   (file (if fastp org-default-notes-file (org-get-org-file)))
	   (heading org-remember-default-headline)
	   (visiting (org-find-base-buffer-visiting file))
	   (org-startup-folded nil)
	   (org-startup-align-all-tables nil)
	   (org-goto-start-pos 1)
	   spos level indent reversed)
      (setq current-prefix-arg nil)
      ;; Modify text so that it becomes a nice subtree which can be inserted
      ;; into an org tree.
      (let* ((lines (split-string txt "\n"))
	     first)
	(setq first (car lines) lines (cdr lines))
	(if (string-match "^\\*+ " first)
	    ;; Is already a headline
	    (setq indent nil)
	  ;; We need to add a headline:  Use time and first buffer line
	  (setq lines (cons first lines)
		first (concat "* " (current-time-string)
			      " (" (remember-buffer-desc) ")")
		indent "  "))
	(if (and org-adapt-indentation indent)
	    (setq lines (mapcar (lambda (x) (concat indent x)) lines)))
	(setq txt (concat first "\n"
			  (mapconcat 'identity lines "\n"))))
      ;; Find the file
      (if (not visiting) (find-file-noselect file))
      (with-current-buffer (or visiting (get-file-buffer file))
	(save-excursion
	  (save-restriction
	    (widen)
	    (and (goto-char (point-min))
		 (not (re-search-forward "^\\* " nil t))
		 (insert "\n* Notes\n"))
	    (setq reversed (org-notes-order-reversed-p))

	    ;; Find the default location
	    (when (and heading (stringp heading) (string-match "\\S-" heading))
	      (goto-char (point-min))
	      (if (re-search-forward
		   (concat "^\\*+[ \t]+" (regexp-quote heading)
			   (org-re "\\([ \t]+:[[:alnum:]@_:]*\\)?[ \t]*$"))
		   nil t)
		  (setq org-goto-start-pos (match-beginning 0))))

	    ;; Ask the User for a location
	    (setq spos (if fastp
			   org-goto-start-pos
			 (org-get-location (current-buffer) org-remember-help)))
	    (if (not spos) (throw 'quit nil)) ; return nil to show we did
					; not handle this note
	    (goto-char spos)
	    (cond ((and (bobp) (not reversed))
		   ;; Put it at the end, one level below level 1
		   (save-restriction
		     (widen)
		     (goto-char (point-max))
		     (if (not (bolp)) (newline))
		     (org-paste-subtree (org-get-legal-level 1 1) txt)))
		  ((and (bobp) reversed)
		   ;; Put it at the start, as level 1
		   (save-restriction
		     (widen)
		     (goto-char (point-min))
		     (re-search-forward "^\\*+ " nil t)
		     (beginning-of-line 1)
		     (org-paste-subtree 1 txt)))
		  ((and (org-on-heading-p t) (not current-prefix-arg))
		   ;; Put it below this entry, at the beg/end of the subtree
		   (org-back-to-heading t)
		   (setq level (funcall outline-level))
		   (if reversed
		       (outline-next-heading)
		     (org-end-of-subtree t))
		   (if (not (bolp)) (newline))
		   (beginning-of-line 1)
		   (org-paste-subtree (org-get-legal-level level 1) txt))
		  (t
		   ;; Put it right there, with automatic level determined by
		   ;; org-paste-subtree or from prefix arg
		   (org-paste-subtree
		    (if (numberp current-prefix-arg) current-prefix-arg)
		    txt)))
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
	    (if (string-match (car entry) buffer-file-name)
		(throw 'exit (cdr entry))))
	  nil)))))

;;;; Dynamic blocks

(defun org-find-dblock (name)
  "Find the first dynamic block with name NAME in the buffer.
If not found, stay at current position and return nil."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (setq pos (and (re-search-forward (concat "^#\\+BEGIN:[ \t]+" name "\\>")
					nil t)
		     (match-beginning 0))))
    (if pos (goto-char pos))
    pos))

(defconst org-dblock-start-re
  "^#\\+BEGIN:[ \t]+\\(\\S-+\\)\\([ \t]+\\(.*\\)\\)?"
  "Matches the startline of a dynamic block, with parameters.")

(defconst org-dblock-end-re "^#\\+END\\([: \t\r\n]\\|$\\)"
  "Matches the end of a dyhamic block.")

(defun org-create-dblock (plist)
  "Create a dynamic block section, with parameters taken from PLIST.
PLIST must containe a :name entry which is used as name of the block."
  (unless (bolp) (newline))
  (let ((name (plist-get plist :name)))
    (insert "#+BEGIN: " name)
    (while plist
      (if (eq (car plist) :name)
	  (setq plist (cddr plist))
	(insert " " (prin1-to-string (pop plist)))))
    (insert "\n\n#+END:\n")
    (beginning-of-line -2)))

(defun org-prepare-dblock ()
  "Prepare dynamic block for refresh.
This empties the block, puts the cursor at the insert position and returns
the property list including an extra property :name with the block name."
  (unless (looking-at org-dblock-start-re)
    (error "Not at a dynamic block"))
  (let* ((begdel (1+ (match-end 0)))
	 (name (match-string 1))
	 (params (append (list :name name)
			 (read (concat "(" (match-string 3) ")")))))
    (unless (re-search-forward org-dblock-end-re nil t)
      (error "Dynamic block not terminated"))
    (delete-region begdel (match-beginning 0))
    (goto-char begdel)
    (open-line 1)
    params))

(defun org-map-dblocks (&optional command)
  "Apply COMMAND to all dynamic blocks in the current buffer.
If COMMAND is not given, use `org-update-dblock'."
  (let ((cmd (or command 'org-update-dblock))
	pos)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-dblock-start-re nil t)
	(goto-char (setq pos (match-beginning 0)))
	(condition-case nil
	    (funcall cmd)
	  (error (message "Error during update of dynamic block")))
	(goto-char pos)
	(unless (re-search-forward org-dblock-end-re nil t)
	  (error "Dynamic block not terminated"))))))

(defun org-dblock-update (&optional arg)
  "User command for updating dynamic blocks.
Update the dynamic block at point.  With prefix ARG, update all dynamic
blocks in the buffer."
  (interactive "P")
  (if arg
      (org-update-all-dblocks)
    (or (looking-at org-dblock-start-re)
	(org-beginning-of-dblock))
    (org-update-dblock)))

(defun org-update-dblock ()
  "Update the dynamic block at point
This means to empty the block, parse for parameters and then call
the correct writing function."
  (let* ((pos (point))
	 (params (org-prepare-dblock))
	 (name (plist-get params :name))
	 (cmd (intern (concat "org-dblock-write:" name))))
    (funcall cmd params)
    (goto-char pos)))

(defun org-beginning-of-dblock ()
  "Find the beginning of the dynamic block at point.
Error if there is no scuh block at point."
  (let ((pos (point))
	beg)
    (end-of-line 1)
    (if (and (re-search-backward org-dblock-start-re nil t)
	     (setq beg (match-beginning 0))
	     (re-search-forward org-dblock-end-re nil t)
	     (> (match-end 0) pos))
	(goto-char beg)
      (goto-char pos)
      (error "Not in a dynamic block"))))

(defun org-update-all-dblocks ()
  "Update all dynamic blocks in the buffer.
This function can be used in a hook."
  (when (org-mode-p)
    (org-map-dblocks 'org-update-dblock)))


;;;; Completion

(defun org-complete (&optional arg)
  "Perform completion on word at point.
At the beginning of a headline, this completes TODO keywords as given in
`org-todo-keywords'.
If the current word is preceded by a backslash, completes the TeX symbols
that are supported for HTML support.
If the current word is preceded by \"#+\", completes special words for
setting file options.
In the line after \"#+STARTUP:, complete valid keywords.\"
At all other locations, this simply calls `ispell-complete-word'."
  (interactive "P")
  (catch 'exit
    (let* ((end (point))
	   (beg1 (save-excursion
		   (skip-chars-backward (org-re "[:alnum:]_@"))
		   (point)))
	   (beg (save-excursion
		  (skip-chars-backward "a-zA-Z0-9_:$")
		  (point)))
	   (confirm (lambda (x) (stringp (car x))))
	   (searchhead (equal (char-before beg) ?*))
	   (tag (and (equal (char-before beg1) ?:)
		     (equal (char-after (point-at-bol)) ?*)))
	   (prop (and (equal (char-before beg1) ?:)
		      (not (equal (char-after (point-at-bol)) ?*))))
	   (texp (equal (char-before beg) ?\\))
	   (link (equal (char-before beg) ?\[))
	   (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
					 beg)
		       "#+"))
	   (startup (string-match "^#\\+STARTUP:.*"
				 (buffer-substring (point-at-bol) (point))))
	   (completion-ignore-case opt)
	   (type nil)
	   (tbl nil)
	   (table (cond
		   (opt
		    (setq type :opt)
		    (mapcar (lambda (x)
			      (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
			      (cons (match-string 2 x) (match-string 1 x)))
			    (org-split-string (org-get-current-options) "\n")))
		   (startup
		    (setq type :startup)
		    org-startup-options)
		   (link (append org-link-abbrev-alist-local
				 org-link-abbrev-alist))
		   (texp
		    (setq type :tex)
		    org-html-entities)
		   ((string-match "\\`\\*+[ \t]+\\'"
				  (buffer-substring (point-at-bol) beg))
		    (setq type :todo)
		    (mapcar 'list org-todo-keywords-1))
		   (searchhead
		    (setq type :searchhead)
		    (save-excursion
		      (goto-char (point-min))
		      (while (re-search-forward org-todo-line-regexp nil t)
			(push (list
			       (org-make-org-heading-search-string
				  (match-string 3) t))
			      tbl)))
		    tbl)
		   (tag (setq type :tag beg beg1)
			(or org-tag-alist (org-get-buffer-tags)))
		   (prop (setq type :prop beg beg1)
			 (mapcar 'list (org-buffer-property-keys)))
		   (t (progn (ispell-complete-word arg) (throw 'exit nil)))))
	   (pattern (buffer-substring-no-properties beg end))
	   (completion (try-completion pattern table confirm)))
      (cond ((eq completion t)
	     (if (equal type :opt)
		 (insert (substring (cdr (assoc (upcase pattern) table))
				    (length pattern)))
	       (if (memq type '(:tag :prop)) (insert ":"))))
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
	     (if (assoc completion table)
		 (if (eq type :todo) (insert " ")
		   (if (memq type '(:tag :prop)) (insert ":"))))
	     (if (and (equal type :opt) (assoc completion table))
		 (message "%s" (substitute-command-keys
				"Press \\[org-complete] again to insert example settings"))))
	    (t
	     (message "Making completion list...")
	     (let ((list (sort (all-completions pattern table confirm)
			       'string<)))
	       (with-output-to-temp-buffer "*Completions*"
		 (condition-case nil
		     ;; Protection needed for XEmacs and emacs 21
		     (display-completion-list list pattern)
		   (error (display-completion-list list)))))
	     (message "Making completion list...%s" "done"))))))

;;;; TODO, DEADLINE, Comments

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (looking-at (concat outline-regexp
			    "\\( *\\<" org-comment-string "\\>\\)"))
	(replace-match "" t t nil 1)
      (if (looking-at outline-regexp)
	  (progn
	    (goto-char (match-end 0))
	    (insert org-comment-string " "))))))

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defvar org-setting-tags nil) ; dynamically skiped

(defun org-todo (&optional arg)
  "Change the TODO state of an item.
The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".
So for this example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.

With C-u prefix arg, use completion to determine the new state.
With numeric prefix arg, switch to that state.

For calling through lisp, arg is also interpreted in the following way:
'none             -> empty state
\"\"(empty string)  -> switch to empty state
'done             -> switch to DONE
'nextset          -> switch to the next set of keywords
'previousset      -> switch to the previous set of keywords
\"WAITING\"         -> switch to the specified keyword, but only if it
                     really is a member of `org-todo-keywords'."
  (interactive "P")
  (when (and org-todo-key-trigger ; keys have been set up by the user
	     (or (and (equal arg '(4)) (eq org-use-fast-todo-selection 'prefix))
		 (and (not arg) org-use-fast-todo-selection
		      (not (eq org-use-fast-todo-selection 'prefix)))))
    ;; Get the keyword with direct selction
    (setq arg (org-fast-todo-selection)))
  (save-excursion
    (org-back-to-heading)
    (if (looking-at outline-regexp) (goto-char (1- (match-end 0))))
    (or (looking-at (concat " +" org-todo-regexp " *"))
	(looking-at " *"))
    (let* ((this (match-string 1))
	   (head (org-get-todo-sequence-head this))
	   (ass (assoc head org-todo-kwd-alist))
	   (interpret (nth 1 ass))
	   (done-word (nth 3 ass))
	   (final-done-word (nth 4 ass))
	   (last-state (or this ""))
	   (completion-ignore-case t)
	   (member (member this org-todo-keywords-1))
	   (tail (cdr member))
	   (state (cond
		   ;; FIXME: most the fast interface here
		   ((equal arg '(4))
		    ;; Read a state with completion
		    (completing-read "State: " (mapcar (lambda(x) (list x))
						       org-todo-keywords-1)
				     nil t))
		   ((eq arg 'right)
		    (if this
			(if tail (car tail) nil)
		      (car org-todo-keywords-1)))
		   ((eq arg 'left)
		    (if (equal member org-todo-keywords-1)
			nil
		      (if this
			  (nth (- (length org-todo-keywords-1) (length tail) 2)
			       org-todo-keywords-1)
			(org-last org-todo-keywords-1))))
		   (arg
		    ;; user or caller requests a specific state
		    (cond
		     ((equal arg "") nil)
		     ((eq arg 'none) nil)
		     ((eq arg 'done) (or done-word (car org-done-keywords)))
		     ((eq arg 'nextset)
		      (or (car (cdr (member head org-todo-heads)))
			  (car org-todo-heads)))
		     ((eq arg 'previousset)
		      (let ((org-todo-heads (reverse org-todo-heads)))
			(or (car (cdr (member head org-todo-heads)))
			    (car org-todo-heads))))
		     ((car (member arg org-todo-keywords-1)))
		     ((nth (1- (prefix-numeric-value arg))
			     org-todo-keywords-1))))
		   ((null member) (or head (car org-todo-keywords-1)))
		   ((equal this final-done-word) nil) ;; -> make empty
		   ((null tail) nil) ;; -> first entry
		   ((eq interpret 'sequence)
		    (car tail))
		   ((memq interpret '(type priority))
		    (if (eq this-command last-command)
			(car tail)
		      (if (> (length tail) 0)
			  (or done-word (car org-done-keywords))
			nil)))
		   (t nil)))
	   (next (if state (concat " " state " ") " "))
	   dostates)
      (replace-match next t t)
      (unless head
	(setq head (org-get-todo-sequence-head state)
	      ass (assoc head org-todo-kwd-alist)
	      interpret (nth 1 ass)
	      done-word (nth 3 ass)
	      final-done-word (nth 4 ass)))
      (when (memq arg '(nextset previousset))
	(message "Keyword-Set %d/%d: %s"
		 (- (length org-todo-sets) -1
		    (length (memq (assoc state org-todo-sets) org-todo-sets)))
		 (length org-todo-sets)
		 (mapconcat 'identity (assoc state org-todo-sets) " ")))
      (setq org-last-todo-state-is-todo
	    (not (member state org-done-keywords)))
      (when (and org-log-done (not (memq arg '(nextset previousset))))
	(setq dostates (and (eq interpret 'sequence)
			    (listp org-log-done) (memq 'state org-log-done)))
	(cond
	 ((and state (member state org-not-done-keywords)
	       (not (member this org-not-done-keywords)))
	  ;; This is now a todo state and was not one before
	  ;; Remove any CLOSED timestamp, and possibly log the state change
	  (org-add-planning-info nil nil 'closed)
	  (and dostates (org-add-log-maybe 'state state 'findpos)))
	 ((and state dostates)
	  ;; This is a non-nil state, and we need to log it
	  (org-add-log-maybe 'state state 'findpos))
	 ((and (member state org-done-keywords)
	       (not (member this org-done-keywords)))
	  ;; It is now done, and it was not done before
	  ;; FIXME: We used to remove scheduling info....
;	  (org-add-planning-info 'closed (org-current-time)
;				 (if (org-get-repeat) nil 'scheduled))
	  (org-add-planning-info 'closed (org-current-time))
	  (org-add-log-maybe 'done state 'findpos))))
      ;; Fixup tag positioning
      (and org-auto-align-tags (not org-setting-tags) (org-set-tags nil t))
      (run-hooks 'org-after-todo-state-change-hook)
      (and (member state org-done-keywords) (org-auto-repeat-maybe))
      (if (and arg (not (member state org-done-keywords)))
	  (setq head (org-get-todo-sequence-head state)))
      (put-text-property (point-at-bol) (point-at-eol) 'org-todo-head head)))
  ;; Fixup cursor location if close to the keyword
  (if (and (outline-on-heading-p)
	   (not (bolp))
	   (save-excursion (beginning-of-line 1)
			   (looking-at org-todo-line-regexp))
	   (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
      (progn
	(goto-char (or (match-end 2) (match-end 1)))
	(just-one-space))))

(defun org-get-todo-sequence-head (kwd)
  "Return the head of the TODO sequence to which KWD belongs.
If KWD is not set, check if there is a text property remembering the
right sequence."
  (let (p)
    (cond
     ((not kwd)
      (or (get-text-property (point-at-bol) 'org-todo-head)
	  (progn
	    (setq p (next-single-property-change (point-at-bol) 'org-todo-head
						 nil (point-at-eol)))
	    (get-text-property p 'org-todo-head))))
     ((not (member kwd org-todo-keywords-1))
      (car org-todo-keywords-1))
     (t (nth 2 (assoc kwd org-todo-kwd-alist))))))

(defun org-fast-todo-selection ()
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur."
  (let* ((fulltable org-todo-key-alist)
	 (done-keywords org-done-keywords) ;; needed for the faces.
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (buf (current-buffer))
	 (expert nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 tg cnt e c char c1 c2 ntable tbl rtn
	 groups ingroup)
    (save-window-excursion
      (if expert
	  (set-buffer (get-buffer-create " *Org todo*"))
;	(delete-other-windows)
;	(split-window-vertically)
	(org-switch-to-buffer-other-window (get-buffer-create " *Org tags*")))
      (erase-buffer)
      (org-set-local 'org-done-keywords done-keywords)
      (setq tbl fulltable char ?a cnt 0)
      (while (setq e (pop tbl))
	(cond
	 ((equal e '(:startgroup))
	  (push '() groups) (setq ingroup t)
	  (when (not (= cnt 0))
	    (setq cnt 0)
	    (insert "\n"))
	  (insert "{ "))
	 ((equal e '(:endgroup))
	  (setq ingroup nil cnt 0)
	  (insert "}\n"))
	 (t
	  (setq tg (car e) c (cdr e))
	  (if ingroup (push tg (car groups)))
	  (setq tg (org-add-props tg nil 'face
				  (org-get-todo-face tg)))
	  (if (and (= cnt 0) (not ingroup)) (insert "  "))
	  (insert "[" c "] " tg (make-string
				 (- fwidth 4 (length tg)) ?\ ))
	  (when (= (setq cnt (1+ cnt)) ncol)
	    (insert "\n")
	    (if ingroup (insert "  "))
	    (setq cnt 0)))))
      (insert "\n")
      (goto-char (point-min))
      (if (and (not expert) (fboundp 'fit-window-to-buffer))
	  (fit-window-to-buffer))
      (message "[a-z..]:Set [SPC]:clear")
      (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
      (cond
       ((or (= c ?\C-g)
	    (and (= c ?q) (not (rassoc c fulltable))))
	(setq quit-flag t))
       ((= c ?\ ) 'none)
       ((setq e (rassoc c fulltable) tg (car e))
	tg)
       (t (setq quit-flag t))))))

(defun org-get-repeat ()
  "Check if tere is a deadline/schedule with repeater in this entry."
  (save-match-data
    (save-excursion
      (org-back-to-heading t)
      (if (re-search-forward
	   org-repeat-re (save-excursion (outline-next-heading) (point)) t)
	  (match-string 1)))))

(defvar org-last-changed-timestamp)
(defvar org-log-post-message)
(defun org-auto-repeat-maybe ()
  "Check if the current headline contains a repeated deadline/schedule.
If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.
This function should be run in the `org-after-todo-state-change-hook'."
  ;; last-state is dynamically scoped into this function
  (let* ((repeat (org-get-repeat))
	 (aa (assoc last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (done-word (nth 3 aa))
	 (whata '(("d" . day) ("m" . month) ("y" . year)))
	 (msg "Entry repeats: ")
	 (org-log-done)
	 re type n what ts)
    (when repeat
      (org-todo (if (eq interpret 'type) last-state head))
      (when (and org-log-repeat
		 (not (memq 'org-add-log-note
			    (default-value 'post-command-hook))))
	;; Make sure a note is taken
	(let ((org-log-done '(done)))
	  (org-add-log-maybe 'done (or done-word (car org-done-keywords))
			     'findpos)))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed)
      (setq re (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
		       org-deadline-time-regexp "\\)"))
      (while (re-search-forward
	      re (save-excursion (outline-next-heading) (point)) t)
	(setq type (if (match-end 1) org-scheduled-string org-deadline-string)
	      ts (match-string (if (match-end 2) 2 4)))
	(when (string-match "\\([-+]?[0-9]+\\)\\([dwmy]\\)" ts)
	  (setq	n (string-to-number (match-string 1 ts))
		what (match-string 2 ts))
	  (if (equal what "w") (setq n (* n 7) what "d"))
	  (org-timestamp-change n (cdr (assoc what whata))))
	(setq msg (concat msg type org-last-changed-timestamp " ")))
      (setq org-log-post-message msg)
      (message msg))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.
With \\[universal-argument] prefix, also show the DONE entries.
With a numeric prefix N, construct a sparse tree for the Nth element
of `org-todo-keywords-1'."
  (interactive "P")
  (let ((case-fold-search nil)
	(kwd-re
	 (cond ((null arg) org-not-done-regexp)
	       ((equal arg '(4))
		(let ((kwd (completing-read "Keyword (or KWD1|KWD2|...): "
					    (mapcar 'list org-todo-keywords-1))))
		  (concat "\\("
			  (mapconcat 'identity (org-split-string kwd "|") "\\|")
			  "\\)\\>")))
	       ((<= (prefix-numeric-value arg) (length org-todo-keywords-1))
		(regexp-quote (nth (1- (prefix-numeric-value arg))
				   org-todo-keywords-1)))
	       (t (error "Invalid prefix argument: %s" arg)))))
    (message "%d TODO entries found"
	     (org-occur (concat "^" outline-regexp " *" kwd-re )))))

(defun org-deadline ()
  "Insert the DEADLINE: string to make a deadline.
A timestamp is also inserted - use \\[org-timestamp-up] and \\[org-timestamp-down]
to modify it to the correct date."
  (interactive)
  (org-add-planning-info 'deadline nil 'closed))

(defun org-schedule ()
  "Insert the SCHEDULED: string to schedule a TODO item.
A timestamp is also inserted - use \\[org-timestamp-up] and \\[org-timestamp-down]
to modify it to the correct date."
  (interactive)
  (org-add-planning-info 'scheduled nil 'closed))

(defun org-add-planning-info (what &optional time &rest remove)
  "Insert new timestamp with keyword in the line directly after the headline.
WHAT indicates what kind of time stamp to add.  TIME indicated the time to use.
If non is given, the user is prompted for a date.
REMOVE indicates what kind of entries to remove.  An old WHAT entry will also
be removed."
  (interactive)
  (let (org-time-was-given org-end-time-was-given)
    (when what (setq time (or time (org-read-date nil 'to-time))))
    (when (and org-insert-labeled-timestamps-at-point
	       (member what '(scheduled deadline)))
      (insert
       (if (eq what 'scheduled) org-scheduled-string org-deadline-string) " ")
      (org-insert-time-stamp time org-time-was-given
			     nil nil nil (list org-end-time-was-given))
      (setq what nil))
    (save-excursion
      (save-restriction
	(let (col list elt ts buffer-invisibility-spec)
	  (org-back-to-heading t)
	  (looking-at (concat outline-regexp "\\( *\\)[^\r\n]*"))
	  (goto-char (match-end 1))
	  (setq col (current-column))
	  (goto-char (match-end 0))
	  (if (eobp) (insert "\n"))
	  (forward-char 1)
	  (when (and (not org-insert-labeled-timestamps-before-properties-drawer)
		     (looking-at "[ \t]*:PROPERTIES:[ \t]*$"))
	    (goto-char (match-end 0))
	    (if (eobp) (insert "\n"))
	    (forward-char 1))
	  (if (and (not (looking-at outline-regexp))
		   (looking-at (concat "[^\r\n]*?" org-keyword-time-regexp
				       "[^\r\n]*"))
		   (not (equal (match-string 1) org-clock-string)))
	      (narrow-to-region (match-beginning 0) (match-end 0))
	    (insert-before-markers "\n")
	    (backward-char 1)
	    (narrow-to-region (point) (point))
	    (indent-to-column col))
	  ;; Check if we have to remove something.
	  (setq list (cons what remove))
	  (while list
	    (setq elt (pop list))
	    (goto-char (point-min))
	    (when (or (and (eq elt 'scheduled)
			   (re-search-forward org-scheduled-time-regexp nil t))
		      (and (eq elt 'deadline)
			   (re-search-forward org-deadline-time-regexp nil t))
		      (and (eq elt 'closed)
			   (re-search-forward org-closed-time-regexp nil t)))
	      (replace-match "")
	      (if (looking-at "--+<[^>]+>") (replace-match ""))
	      (if (looking-at " +") (replace-match ""))))
	  (goto-char (point-max))
	  (when what
	    (insert
	     (if (not (equal (char-before) ?\ )) " " "")
	     (cond ((eq what 'scheduled) org-scheduled-string)
		   ((eq what 'deadline) org-deadline-string)
		   ((eq what 'closed) org-closed-string))
	     " ")
	    (org-insert-time-stamp
	     time
	     (or org-time-was-given
		 (and (eq what 'closed) org-log-done-with-time))
	     (eq what 'closed)
	     nil nil (list org-end-time-was-given))
	    (end-of-line 1))
	  (goto-char (point-min))
	  (widen)
	  (if (looking-at "[ \t]+\r?\n")
	      (replace-match ""))
	  ts)))))

(defvar org-log-note-marker (make-marker))
(defvar org-log-note-purpose nil)
(defvar org-log-note-state nil)
(defvar org-log-note-window-configuration nil)
(defvar org-log-note-return-to (make-marker))
(defvar org-log-post-message nil
  "Message to be displayed after a log note has been stored.
The auto-repeater uses this.")

(defun org-add-log-maybe (&optional purpose state findpos)
  "Set up the post command hook to take a note."
  (save-excursion
    (when (and (listp org-log-done)
	       (memq purpose org-log-done))
      (when findpos
	(org-back-to-heading t)
	(looking-at (concat outline-regexp "\\( *\\)[^\r\n]*"
			    "\\(\n[^\r\n]*?" org-keyword-time-not-clock-regexp
			    "[^\r\n]*\\)?"))
	(goto-char (match-end 0))
	(unless org-log-states-order-reversed
	  (and (= (char-after) ?\n) (forward-char 1))
	  (org-skip-over-state-notes)
	  (skip-chars-backward " \t\n\r")))
      (move-marker org-log-note-marker (point))
      (setq org-log-note-purpose purpose)
      (setq org-log-note-state state)
      (add-hook 'post-command-hook 'org-add-log-note 'append))))

(defun org-skip-over-state-notes ()
  "Skip past the list of State notes in an entry."
  (if (looking-at "\n[ \t]*- State") (forward-char 1))
  (while (looking-at "[ \t]*- State")
    (condition-case nil
	(org-next-item)
      (error (org-end-of-item)))))

(defun org-add-log-note (&optional purpose)
  "Pop up a window for taking a note, and add this note later at point."
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-note-window-configuration (current-window-configuration))
  (delete-other-windows)
  (move-marker org-log-note-return-to (point))
  (switch-to-buffer (marker-buffer org-log-note-marker))
  (goto-char org-log-note-marker)
  (org-switch-to-buffer-other-window "*Org Note*")
  (erase-buffer)
  (let ((org-inhibit-startup t)) (org-mode))
  (insert (format "# Insert note for %s, finish with C-c C-c, or cancel with C-u C-c C-c.\n\n"
		  (cond
		   ((eq org-log-note-purpose 'clock-out) "stopped clock")
		   ((eq org-log-note-purpose 'done)  "closed todo item")
		   ((eq org-log-note-purpose 'state) "state change")
		   (t (error "This should not happen")))))
  (org-set-local 'org-finish-function 'org-store-log-note))

(defun org-store-log-note ()
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (buffer-string))
	(note (cdr (assq org-log-note-purpose org-log-note-headings)))
	lines ind)
    (kill-buffer (current-buffer))
    (if (string-match "^#.*\n[ \t\n]*" txt)
	(setq txt (replace-match "" t t txt)))
    (if (string-match "\\s-+\\'" txt)
	(setq txt (replace-match "" t t txt)))
    (setq lines (org-split-string txt "\n"))
    (when (and note (string-match "\\S-" note))
      (setq note
	    (org-replace-escapes
	     note
	     (list (cons "%u" (user-login-name))
		   (cons "%U" user-full-name)
		   (cons "%t" (format-time-string
			       (org-time-stamp-format 'long 'inactive)
			       (current-time)))
		   (cons "%s" (if org-log-note-state
				  (concat "\"" org-log-note-state "\"")
				"")))))
      (if lines (setq note (concat note " \\\\")))
      (push note lines))
    (when current-prefix-arg (setq lines nil))
    (when lines
      (save-excursion
	(set-buffer (marker-buffer org-log-note-marker))
	(save-excursion
	  (goto-char org-log-note-marker)
	  (move-marker org-log-note-marker nil)
	  (end-of-line 1)
	  (if (not (bolp)) (insert "\n")) (indent-relative nil)
	  (insert "  - " (pop lines))
	  (org-indent-line-function)
	  (beginning-of-line 1)
	  (looking-at "[ \t]*")
	  (setq ind (concat (match-string 0) "  "))
	  (end-of-line 1)
	  (while lines (insert "\n" ind (pop lines)))))))
  (set-window-configuration org-log-note-window-configuration)
  (with-current-buffer (marker-buffer org-log-note-return-to)
    (goto-char org-log-note-return-to))
  (move-marker org-log-note-return-to nil)
  (and org-log-post-message (message org-log-post-message)))

(defvar org-occur-highlights nil)
(make-variable-buffer-local 'org-occur-highlights)

(defun org-occur (regexp &optional keep-previous callback)
  "Make a compact tree which shows all matches of REGEXP.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.  It will also show the heading after the match,
to make sure editing the matching entry is easy.
If KEEP-PREVIOUS is non-nil, highlighting and exposing done by a previous
call to `org-occur' will be kept, to allow stacking of calls to this
command.
If CALLBACK is non-nil, it is a function which is called to confirm
that the match should indeed be shown."
  (interactive "sRegexp: \nP")
  (or keep-previous (org-remove-occur-highlights nil nil t))
  (let ((cnt 0))
    (save-excursion
      (goto-char (point-min))
      (if (or (not keep-previous)          ; do not want to keep
	      (not org-occur-highlights))  ; no previous matches
	  ;; hide everything
	  (org-overview))
      (while (re-search-forward regexp nil t)
	(when (or (not callback)
		  (save-match-data (funcall callback)))
	  (setq cnt (1+ cnt))
	  (when org-highlight-sparse-tree-matches
	    (org-highlight-new-match (match-beginning 0) (match-end 0)))
	  (org-show-context 'occur-tree))))
    (when org-remove-highlights-with-change
      (org-add-hook 'before-change-functions 'org-remove-occur-highlights
		    nil 'local))
    (unless org-sparse-tree-open-archived-trees
      (org-hide-archived-subtrees (point-min) (point-max)))
    (run-hooks 'org-occur-hook)
    (if (interactive-p)
	(message "%d match(es) for regexp %s" cnt regexp))
    cnt))

(defun org-show-context (&optional key)
  "Make sure point and context and visible.
How much context is shown depends upon the variables
`org-show-hierarchy-above', `org-show-following-heading'. and
`org-show-siblings'."
  (let ((heading-p   (org-on-heading-p t))
	(hierarchy-p (org-get-alist-option org-show-hierarchy-above key))
	(following-p (org-get-alist-option org-show-following-heading key))
	(siblings-p  (org-get-alist-option org-show-siblings key)))
    (catch 'exit
      ;; Show heading or entry text
      (if heading-p
	  (org-flag-heading nil)    ; only show the heading
	(and (or (org-invisible-p) (org-invisible-p2))
	     (org-show-hidden-entry)))    ; show entire entry
      (when following-p
	;; Show next sibling, or heading below text
	(save-excursion
	  (and (if heading-p (org-goto-sibling) (outline-next-heading))
	       (org-flag-heading nil))))
      (when siblings-p (org-show-siblings))
      (when hierarchy-p
	;; show all higher headings, possibly with siblings
	(save-excursion
	  (while (and (condition-case nil
			  (progn (org-up-heading-all 1) t)
			(error nil))
		      (not (bobp)))
	    (org-flag-heading nil)
	    (when siblings-p (org-show-siblings))))))))

(defun org-reveal (&optional siblings)
  "Show current entry, hierarchy above it, and the following headline.
This can be used to show a consistent set of context around locations
exposed with `org-show-hierarchy-above' or `org-show-following-heading'
not t for the search context.

With optional argument SIBLINGS, on each level of the hierarchy all
siblings are shown.  This repairs the tree structure to what it would
look like when opened with hierarchical calls to `org-cycle'."
  (interactive "P")
  (let ((org-show-hierarchy-above t)
	(org-show-following-heading t)
	(org-show-siblings (if siblings t org-show-siblings)))
    (org-show-context nil)))

(defun org-highlight-new-match (beg end)
  "Highlight from BEG to END and mark the highlight is an occur headline."
  (let ((ov (org-make-overlay beg end)))
    (org-overlay-put ov 'face 'secondary-selection)
    (push ov org-occur-highlights)))

(defun org-remove-occur-highlights (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc 'org-delete-overlay org-occur-highlights)
    (setq org-occur-highlights nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-remove-occur-highlights 'local))))

;;;; Priorities

(defvar org-priority-regexp ".*?\\(\\[#\\([A-Z0-9]\\)\\] ?\\)"
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
ACTION can be `set', `up', `down', or a character."
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
       ((or (eq action 'set) (integerp action))
	(if (integerp action)
	    (setq new action)
	  (message "Priority %c-%c, SPC to remove: " org-highest-priority org-lowest-priority)
	  (setq new (read-char-exclusive)))
	(cond ((equal new ?\ ) (setq remove t))
	      ((or (< (upcase new) org-highest-priority) (> (upcase new) org-lowest-priority))
	       (error "Priority must be between `%c' and `%c'"
		      org-highest-priority org-lowest-priority))))
       ((eq action 'up)
	(setq new (1- current)))
       ((eq action 'down)
	(setq new (1+ current)))
       (t (error "Invalid action")))
      (setq new (min (max org-highest-priority (upcase new)) org-lowest-priority))
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
    (org-preserve-lc (org-set-tags nil 'align))
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

;;;; Tags

(defun org-scan-tags (action matcher &optional todo-only)
  "Scan headline tags with inheritance and produce output ACTION.
ACTION can be `sparse-tree' or `agenda'.  MATCHER is a Lisp form to be
evaluated, testing if a given set of tags qualifies a headline for
inclusion.  When TODO-ONLY is non-nil, only lines with a TODO keyword
are included in the output."
  (let* ((re (concat "[\n\r]" outline-regexp " *\\(\\<\\("
		     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		     (org-re
		      "\\>\\)\\)? *\\(.*?\\)\\(:[[:alnum:]_@:]+:\\)?[ \t]*$")))
	 (props (list 'face nil
		      'done-face 'org-done
		      'undone-face nil
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (case-fold-search nil)
         lspos
	 tags tags-list tags-alist (llast 0) rtn level category i txt
	 todo marker entry priority)
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree) (org-overview))
      (while (re-search-forward re nil t)
	(catch :skip
	  (setq todo (if (match-end 1) (match-string 2))
		tags (if (match-end 4) (match-string 4)))
	  (goto-char (setq lspos (1+ (match-beginning 0))))
	  (setq level (org-reduced-level (funcall outline-level))
		category (org-get-category))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the nex tags
	  (when tags
	    (setq tags (mapcar 'downcase (org-split-string tags ":"))
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr tags-alist))
		  tags))
	  (when (and (or (not todo-only) (member todo org-not-done-keywords))
		     (eval matcher)
		     (or (not org-agenda-skip-archived-trees)
			 (not (member org-archive-tag tags-list))))
	    (and (eq action 'agenda) (org-agenda-skip))
	    ;; list this headline
	    (if (eq action 'sparse-tree)
		(progn
		  (org-show-context 'tags-tree))
	      (setq txt (org-format-agenda-item
			 ""
			 (concat
			  (if org-tags-match-list-sublevels
			      (make-string (1- level) ?.) "")
			  (org-get-heading))
			 category tags-list)
		    priority (org-get-priority txt))
	      (goto-char lspos)
	      (setq marker (org-agenda-new-marker))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker marker 'org-category category
		'priority priority 'type "tagsmatch")
	      (push txt rtn))
	    ;; if we are to skip sublevels, jump to end of subtree
	    (or org-tags-match-list-sublevels (org-end-of-subtree t))))))
    (when (and (eq action 'sparse-tree)
	       (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))

(defvar todo-only) ;; dynamically scoped

(defun org-tags-sparse-tree (&optional todo-only match)
  "Create a sparse tree according to tags  string MATCH.
MATCH can contain positive and negative selection of tags, like
\"+WORK+URGENT-WITHBOSS\".
If optional argument TODO_ONLY is non-nil, only select lines that are
also TODO lines."
  (interactive "P")
  (org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match)) todo-only))

(defvar org-cached-props nil)
(defun org-cached-entry-get (pom property)
  (cdr (assoc property (or org-cached-props
			   (setq org-cached-props
				 (org-entry-properties pom))))))

(defun org-global-tags-completion-table (&optional files)
  "Return the list of all tags in all agenda buffer/files."
  (save-excursion
    (org-uniquify
     (apply 'append
	    (mapcar
	     (lambda (file)
	       (set-buffer (find-file-noselect file))
	       (org-get-buffer-tags))
	     (if (and files (car files))
		 files
	       (org-agenda-files)))))))

(defun org-make-tags-matcher (match)
  "Create the TAGS//TODO matcher form for the selection string MATCH."
  ;; todo-only is scoped dynamically into this function, and the function
  ;; may change it it the matcher asksk for it.
  (unless match
    ;; Get a new match request, with completion
    (let ((org-last-tags-completion-table
	   (org-global-tags-completion-table)))
      (setq match (completing-read
		   "Match: " 'org-tags-completion-function nil nil nil
		   'org-tags-history))))
  
  ;; Parse the string and create a lisp form
  (let ((match0 match)
	(re (org-re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL=\\([0-9]+\\)\\|\\([[:alnum:]]+\\)=\\({[^}]+}\\|\"[^\"]+\"\\)\\|[[:alnum:]_@]+\\)"))
	minus tag mm
	tagsmatch todomatch tagsmatcher todomatcher kwd matcher
	orterms term orlist re-p level-p prop-p pn pv)
    (if (string-match "/+" match)
	;; match contains also a todo-matching request
	(progn
	  (setq tagsmatch (substring match 0 (match-beginning 0))
		todomatch (substring match (match-end 0)))
	  (if (string-match "^!" todomatch)
	      (setq todo-only t todomatch (substring todomatch 1)))
	  (if (string-match "^\\s-*$" todomatch)
	      (setq todomatch nil)))
      ;; only matching tags
      (setq tagsmatch match todomatch nil))

    ;; Make the tags matcher
    (if (or (not tagsmatch) (not (string-match "\\S-" tagsmatch)))
	(setq tagsmatcher t)
      (setq orterms (org-split-string tagsmatch "|") orlist nil)
      (while (setq term (pop orterms))
	(while (and (equal (substring term -1) "\\") orterms)
	  (setq term (concat term "|" (pop orterms)))) ; repair bad split
	(while (string-match re term)
	  (setq minus (and (match-end 1)
			   (equal (match-string 1 term) "-"))
		tag (match-string 2 term)
		re-p (equal (string-to-char tag) ?{)
		level-p (match-end 3)
		prop-p (match-end 4)
		mm (cond
		    (re-p `(org-match-any-p ,(substring tag 1 -1) tags-list))
		    (level-p `(= level ,(string-to-number
					 (match-string 3 term))))
		    (prop-p
		     (setq pn (match-string 4 term)
			   pv (match-string 5 term)
			   re-p (equal (string-to-char pv) ?{)
			   pv (substring pv 1 -1))
		     (if re-p
			 `(string-match ,pv (org-cached-entry-get nil ,pn))
		       `(equal ,pv (org-cached-entry-get nil ,pn))))
		    (t `(member ,(downcase tag) tags-list)))
		mm (if minus (list 'not mm) mm)
		term (substring term (match-end 0)))
	  (push mm tagsmatcher))
	(push (if (> (length tagsmatcher) 1)
		  (cons 'and tagsmatcher)
		(car tagsmatcher))
	      orlist)
	(setq tagsmatcher nil))
      (setq tagsmatcher (if (> (length orlist) 1) (cons 'or orlist) (car orlist)))
      (setq tagsmatcher
	    (list 'progn '(setq org-cached-props nil) tagsmatcher)))

    ;; Make the todo matcher
    (if (or (not todomatch) (not (string-match "\\S-" todomatch)))
	(setq todomatcher t)
      (setq orterms (org-split-string todomatch "|") orlist nil)
      (while (setq term (pop orterms))
	(while (string-match re term)
	  (setq minus (and (match-end 1)
			   (equal (match-string 1 term) "-"))
		kwd (match-string 2 term)
		re-p (equal (string-to-char kwd) ?{)
		term (substring term (match-end 0))
		mm (if re-p
		       `(string-match  ,(substring kwd 1 -1) todo)
		     (list 'equal 'todo kwd))
		mm (if minus (list 'not mm) mm))
	  (push mm todomatcher))
	(push (if (> (length todomatcher) 1)
		  (cons 'and todomatcher)
		(car todomatcher))
	      orlist)
	(setq todomatcher nil))
      (setq todomatcher (if (> (length orlist) 1)
			    (cons 'or orlist) (car orlist))))

    ;; Return the string and lisp forms of the matcher
    (setq matcher (if todomatcher
		      (list 'and tagsmatcher todomatcher)
		    tagsmatcher))
    (cons match0 matcher)))

(defun org-match-any-p (re list)
  "Does re match any element of list?"
  (setq list (mapcar (lambda (x) (string-match re x)) list))
  (delq nil list))

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically skoped param
(defvar org-tags-overlay (org-make-overlay 1 1))
(org-detach-overlay org-tags-overlay)

(defun org-align-tags-here (to-col)
  ;; Assumes that this is a headline
  (let ((pos (point)) (col (current-column)) tags)
    (beginning-of-line 1)
    (if	(and (looking-at (org-re ".*?\\([ \t]+\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
	     (< pos (match-beginning 2)))
	(progn
	  (setq tags (match-string 2))
	  (goto-char (match-beginning 1))
	  (insert " ")
	  (delete-region (point) (1+ (match-end 0)))
	  (backward-char 1)
	  (move-to-column
	   (max (1+ (current-column))
		(1+ col)
		(if (> to-col 0)
		    to-col
		  (- (abs to-col) (length tags))))
	   t)
	  (insert tags)
	  (move-to-column (min (current-column) col) t))
      (goto-char pos))))

(defun org-set-tags (&optional arg just-align)
  "Set the tags for the current headline.
With prefix ARG, realign all tags in headings in the current buffer."
  (interactive "P")
  (let* ((re (concat "^" outline-regexp))
	 (current (org-get-tags))
	 (col (current-column))
	 (org-setting-tags t)
	 table current-tags inherited-tags ; computed below when needed
	 tags p0 c0 c1 rpl)
    (if arg
	(save-excursion
	  (goto-char (point-min))
	  (let ((buffer-invisibility-spec (org-inhibit-invisibility)))
	    (while (re-search-forward re nil t)
	      (org-set-tags nil t)
	      (end-of-line 1)))
	  (message "All tags realigned to column %d" org-tags-column))
      (if just-align
	  (setq tags current)
	;; Get a new set of tags from the user
	(save-excursion
	  (setq table (or org-tag-alist (org-get-buffer-tags))
		org-last-tags-completion-table table
		current-tags (org-split-string current ":")
		inherited-tags (nreverse
				(nthcdr (length current-tags)
					(nreverse (org-get-tags-at))))
		tags
		(if (or (eq t org-use-fast-tag-selection)
			(and org-use-fast-tag-selection
			     (delq nil (mapcar 'cdr table))))
		    (org-fast-tag-selection
		     current-tags inherited-tags table
		     (if org-fast-tag-selection-include-todo org-todo-key-alist))
		  (let ((org-add-colon-after-tag-completion t))
		    (org-trim
		     (completing-read "Tags: " 'org-tags-completion-function
				      nil nil current 'org-tags-history))))))
	(while (string-match "[-+&]+" tags)
	  ;; No boolean logic, just a list
	  (setq tags (replace-match ":" t t tags))))
      
      (if (string-match "\\`[\t ]*\\'" tags)
          (setq tags "")
	(unless (string-match ":$" tags) (setq tags (concat tags ":")))
	(unless (string-match "^:" tags) (setq tags (concat ":" tags))))
      
      ;; Insert new tags at the correct column
      (beginning-of-line 1)
      (cond
       ((and (equal current "") (equal tags "")))
       ((re-search-forward
	 (concat "\\([ \t]*" (regexp-quote current) "\\)[ \t]*$")
	 (point-at-eol) t)
	(if (equal tags "")
	    (setq rpl "")
	  (goto-char (match-beginning 0))
	  (setq c0 (current-column) p0 (point)
		c1 (max (1+ c0) (if (> org-tags-column 0)
				    org-tags-column
				  (- (- org-tags-column) (length tags))))
		rpl (concat (make-string (max 0 (- c1 c0)) ?\ ) tags)))
	(replace-match rpl t t)
	(and (not (featurep 'xemacs)) c0 (tabify p0 (point)))
	tags)
       (t (error "Tags alignment failed")))
      (move-to-column col))))

(defun org-change-tag-in-region (beg end tag off)
  "Add or remove TAG for each entry in the region.
This works in the agenda, and also in an org-mode buffer."
  (interactive
   (list (region-beginning) (region-end)
	 (let ((org-last-tags-completion-table
		(if (org-mode-p)
		    (org-get-buffer-tags)
		  (org-global-tags-completion-table))))
	   (completing-read
	    "Tag: " 'org-tags-completion-function nil nil nil
	    'org-tags-history))
	 (progn
	   (message "[s]et or [r]emove? ")
	   (equal (read-char-exclusive) ?r))))
  (if (fboundp 'deactivate-mark) (deactivate-mark))
  (let ((agendap (equal major-mode 'org-agenda-mode))
	l1 l2 m buf pos newhead (cnt 0))
    (goto-char end)
    (setq l2 (1- (org-current-line)))
    (goto-char beg)
    (setq l1 (org-current-line))
    (loop for l from l1 to l2 do
	  (goto-line l)
	  (setq m (get-text-property (point) 'org-hd-marker))
	  (when (or (and (org-mode-p) (org-on-heading-p))
		    (and agendap m))
	    (setq buf (if agendap (marker-buffer m) (current-buffer))
		  pos (if agendap m (point)))
	    (with-current-buffer buf
	      (save-excursion
		(save-restriction
		  (goto-char pos)
		  (setq cnt (1+ cnt))
		  (org-toggle-tag tag (if off 'off 'on))
		  (setq newhead (org-get-heading)))))
	    (and agendap (org-agenda-change-all-lines newhead m))))
    (message "Tag :%s: %s in %d headings" tag (if off "removed" "set") cnt)))

(defun org-tags-completion-function (string predicate &optional flag)
  (let (s1 s2 rtn (ctable org-last-tags-completion-table)
	   (confirm (lambda (x) (stringp (car x)))))
    (if (string-match "^\\(.*[-+:&|]\\)\\([^-+:&|]*\\)$" string)
        (setq s1 (match-string 1 string)
              s2 (match-string 2 string))
      (setq s1 "" s2 string))
    (cond
     ((eq flag nil)
      ;; try completion
      (setq rtn (try-completion s2 ctable confirm))
      (if (stringp rtn)
	  (setq rtn
		(concat s1 s2 (substring rtn (length s2))
			(if (and org-add-colon-after-tag-completion
				 (assoc rtn ctable))
			    ":" ""))))
      rtn)
     ((eq flag t)
      ;; all-completions
      (all-completions s2 ctable confirm)
      )
     ((eq flag 'lambda)
      ;; exact match?
      (assoc s2 ctable)))
    ))

(defun org-fast-tag-insert (kwd tags face &optional end)
  "Insert KDW, and the TAGS, the latter with face FACE.  Also inser END."
  (insert (format "%-12s" (concat kwd ":"))
	  (org-add-props (mapconcat 'identity tags " ") nil 'face face)
	  (or end "")))

(defun org-fast-tag-show-exit (flag)
  (save-excursion
    (goto-line 3)
    (if (re-search-forward "[ \t]+Next change exits" (point-at-eol) t)
	(replace-match ""))
    (when flag
      (end-of-line 1)
      (move-to-column (- (window-width) 19) t)
      (insert (org-add-props " Next change exits" nil 'face 'org-warning)))))

(defun org-set-current-tags-overlay (current prefix)
  (let ((s (concat ":" (mapconcat 'identity current ":") ":")))
    (if (featurep 'xemacs)
	(org-overlay-display org-tags-overlay (concat prefix s)
			     'secondary-selection)
      (put-text-property 0 (length s) 'face '(secondary-selection org-tag) s)
      (org-overlay-display org-tags-overlay (concat prefix s)))))

(defun org-fast-tag-selection (current inherited table &optional todo-table)
  "Fast tag selection with single keys.
CURRENT is the current list of tags in the headline, INHERITED is the
list of inherited tags, and TABLE is an alist of tags and corresponding keys,
possibly with grouping information.  TODO-TABLE is a similar table with
TODO keywords, should these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* ((fulltable (append table todo-table))
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (buf (current-buffer))
	 (expert (eq org-fast-tag-selection-single-key 'expert))
	 (buffer-tags nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 (i-face 'org-done)
	 (c-face 'org-todo)
	 tg cnt e c char c1 c2 ntable tbl rtn
	 ov-start ov-end ov-prefix
	 (exit-after-next org-fast-tag-selection-single-key)
	 (done-keywords org-done-keywords)
	 groups ingroup)
    (save-excursion
      (beginning-of-line 1)
      (if (looking-at
	   (org-re ".*[ \t]\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
	  (setq ov-start (match-beginning 1)
		ov-end (match-end 1)
		ov-prefix "")
	(setq ov-start (1- (point-at-eol))
	      ov-end (1+ ov-start))
	(skip-chars-forward "^\n\r")
	(setq ov-prefix
	      (concat
	       (buffer-substring (1- (point)) (point))
	       (if (> (current-column) org-tags-column)
		   " "
		 (make-string (- org-tags-column (current-column)) ?\ ))))))
    (org-move-overlay org-tags-overlay ov-start ov-end)
    (save-window-excursion
      (if expert
	  (set-buffer (get-buffer-create " *Org tags*"))
	(delete-other-windows)
	(split-window-vertically)
	(org-switch-to-buffer-other-window (get-buffer-create " *Org tags*")))
      (erase-buffer)
      (org-set-local 'org-done-keywords done-keywords)
      (org-fast-tag-insert "Inherited" inherited i-face "\n")
      (org-fast-tag-insert "Current" current c-face "\n\n")
      (org-fast-tag-show-exit exit-after-next)
      (org-set-current-tags-overlay current ov-prefix)
      (setq tbl fulltable char ?a cnt 0)
      (while (setq e (pop tbl))
	(cond
	 ((equal e '(:startgroup))
	  (push '() groups) (setq ingroup t)
	  (when (not (= cnt 0))
	    (setq cnt 0)
	    (insert "\n"))
	  (insert "{ "))
	 ((equal e '(:endgroup))
	  (setq ingroup nil cnt 0)
	  (insert "}\n"))
	 (t
	  (setq tg (car e) c2 nil)
	  (if (cdr e)
	      (setq c (cdr e))
	    ;; automatically assign a character.
	    (setq c1 (string-to-char
		      (downcase (substring
				 tg (if (= (string-to-char tg) ?@) 1 0)))))
	    (if (or (rassoc c1 ntable) (rassoc c1 table))
		(while (or (rassoc char ntable) (rassoc char table))
		  (setq char (1+ char)))
	      (setq c2 c1))
	    (setq c (or c2 char)))
	  (if ingroup (push tg (car groups)))
	  (setq tg (org-add-props tg nil 'face
				  (cond
				   ((not (assoc tg table))
				    (org-get-todo-face tg))
				   ((member tg current) c-face)
				   ((member tg inherited) i-face)
				   (t nil))))
	  (if (and (= cnt 0) (not ingroup)) (insert "  "))
	  (insert "[" c "] " tg (make-string
				 (- fwidth 4 (length tg)) ?\ ))
	  (push (cons tg c) ntable)
	  (when (= (setq cnt (1+ cnt)) ncol)
	    (insert "\n")
	    (if ingroup (insert "  "))
	    (setq cnt 0)))))
      (setq ntable (nreverse ntable))
      (insert "\n")
      (goto-char (point-min))
      (if (and (not expert) (fboundp 'fit-window-to-buffer))
	  (fit-window-to-buffer))
      (setq rtn
	    (catch 'exit
	      (while t
		(message "[a-z..]:Toggle [SPC]:clear [RET]:accept [TAB]:free%s%s"
			 (if groups " [!] no groups" " [!]groups")
			 (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
		(cond
		 ((= c ?\r) (throw 'exit t))
		 ((= c ?!)
		  (setq groups (not groups))
		  (goto-char (point-min))
		  (while (re-search-forward "[{}]" nil t) (replace-match " ")))
		 ((= c ?\C-c)
		  (if (not expert)
		      (org-fast-tag-show-exit
		       (setq exit-after-next (not exit-after-next)))
		    (setq expert nil)
		    (delete-other-windows)
		    (split-window-vertically)
		    (org-switch-to-buffer-other-window " *Org tags*")
		    (and (fboundp 'fit-window-to-buffer)
			 (fit-window-to-buffer))))
		 ((or (= c ?\C-g)
		      (and (= c ?q) (not (rassoc c ntable))))
		  (org-detach-overlay org-tags-overlay)
		  (setq quit-flag t))
		 ((= c ?\ )
		  (setq current nil)
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((= c ?\t)
		  (condition-case nil
		      (setq tg (completing-read
				"Tag: "
				(or buffer-tags
				    (with-current-buffer buf
				      (org-get-buffer-tags)))))
		    (quit (setq tg "")))
		  (when (string-match "\\S-" tg)
		    (add-to-list 'buffer-tags (list tg))
		    (if (member tg current)
			(setq current (delete tg current))
		      (push tg current)))
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((setq e (rassoc c todo-table) tg (car e))
		  (with-current-buffer buf
		    (save-excursion (org-todo tg)))
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((setq e (rassoc c ntable) tg (car e))
		  (if (member tg current)
		      (setq current (delete tg current))
		    (loop for g in groups do
			  (if (member tg g)
			      (mapcar (lambda (x)
					(setq current (delete x current)))
				      g)))
		    (push tg current))
		  (if exit-after-next (setq exit-after-next 'now))))

		;; Create a sorted list
		(setq current
		      (sort current
			    (lambda (a b)
			      (assoc b (cdr (memq (assoc a ntable) ntable))))))
		(if (eq exit-after-next 'now) (throw 'exit t))
		(goto-char (point-min))
		(beginning-of-line 2)
		(delete-region (point) (point-at-eol))
		(org-fast-tag-insert "Current" current c-face)
		(org-set-current-tags-overlay current ov-prefix)
		(while (re-search-forward
			(org-re "\\[.\\] \\([[:alnum:]_@]+\\)") nil t)
		  (setq tg (match-string 1))
		  (add-text-properties
		   (match-beginning 1) (match-end 1)
		   (list 'face
			 (cond
			  ((member tg current) c-face)
			  ((member tg inherited) i-face)
			  (t (get-text-property (match-beginning 1) 'face))))))
		(goto-char (point-min)))))
      (org-detach-overlay org-tags-overlay)
      (if rtn
	  (mapconcat 'identity current ":")
	nil))))

(defun org-get-tags ()
  "Get the TAGS string in the current headline."
  (unless (org-on-heading-p t)
    (error "Not on a heading"))
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at (org-re ".*[ \t]\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
	(org-match-string-no-properties 1)
      "")))

(defun org-get-buffer-tags ()
  "Get a table of all tags used in the buffer, for completion."
  (let (tags)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward 
	      (org-re "[ \t]:\\([[:alnum:]_@:]+\\):[ \t\r\n]") nil t)
	(when (equal (char-after (point-at-bol 0)) ?*)
	  (mapc (lambda (x) (add-to-list 'tags x))
		(org-split-string (org-match-string-no-properties 1) ":")))))
    (mapcar 'list tags)))


;;;; Properties

;;; Setting and retrieving properties

(defconst org-special-properties
  '("TODO" "TAGS" "ALLTAGS" "DEADLINE" "SCHEDULED"
    "CLOCK" "PRIORITY")
  "The special properties valid in Org-mode.

These are properties that are not defined in the property drawer,
but in some other way.")

(defconst org-property-start-re "^[ \t]*:PROPERTIES:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-property-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defun org-property-action ()
  "Do an action on properties."
  (interactive)
  (let (c prop)
    (org-at-property-p)
    (setq prop (match-string 2))
    (message "Property Action:  [s]et  [d]elete  [D]elete globally  [c]ompute")
    (setq c (read-char-exclusive))
    (cond
     ((equal c ?s)
      (call-interactively 'org-set-property))
     ((equal c ?d)
      (call-interactively 'org-delete-property))
     ((equal c ?D)
      (call-interactively 'org-delete-property-globally))
     ((equal c ?c)
      (call-interactively 'org-compute-property-at-point))
     (t (error "No such property action %c" c)))))

(defun org-at-property-p ()
  "Is the cursor in a property line?"
  ;; FIXME: Does not check if we are actually in the drawer.
  ;; FIXME: also returns true on any drawers.....
  ;; This is used by C-c C-c for property action.
  (save-excursion
    (beginning-of-line 1)
    (looking-at (org-re "^[ \t]*\\(:\\([[:alpha:]][[:alnum:]_-]*\\):\\)[ \t]*\\(.*\\)"))))

(defmacro org-with-point-at (pom &rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  (declare (indent 1) (debug t))
  `(save-excursion
     (if (markerp pom) (set-buffer (marker-buffer pom)))
     (save-excursion
       (goto-char (or pom (point)))
       ,@body)))

(defun org-get-property-block (&optional beg end force)
  "Return the (beg . end) range of the body of the property drawer.
BEG and END can be beginning and end of subtree, if not given
they will be found.
If the drawer does not exist and FORCE is non-nil, create the drawer."
  (catch 'exit
    (save-excursion
      (let* ((beg (or beg (progn (org-back-to-heading t) (point))))
	     (end (or end (progn (outline-next-heading) (point)))))
	(goto-char beg)
	(if (re-search-forward org-property-start-re end t)
	    (setq beg (1+ (match-end 0)))
	  (if force
	      (save-excursion
		(org-insert-property-drawer)
		(setq end (progn (outline-next-heading) (point))))
	    (throw 'exit nil))
	  (goto-char beg)
	  (if (re-search-forward org-property-start-re end t)
	      (setq beg (1+ (match-end 0)))))
	(if (re-search-forward org-property-end-re end t)
	    (setq end (match-beginning 0))
	  (or force (throw 'exit nil))
	  (goto-char beg)
	  (setq end beg)
	  (org-indent-line-function)
	  (insert ":END:\n"))
	(cons beg end)))))

(defun org-entry-properties (&optional pom which)
  "Get all properties of the entry at point-or-marker POM.
This includes the TODO keyword, the tags, time strings for deadline,
scheduled, and clocking, and any additional properties defined in the
entry.  The return value is an alist, keys may occur multiple times
if the property key was used several times.
POM may also be nil, in which case the current entry is used.
If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass."
  (setq which (or which 'all))
  (org-with-point-at pom
    (let ((clockstr (substring org-clock-string 0 -1))
	  (excluded '("TODO" "TAGS" "ALLTAGS" "PRIORITY"))
	  beg end range props sum-props key value)
      (save-excursion
	(when (condition-case nil (org-back-to-heading t) (error nil))
	  (setq beg (point))
	  (setq sum-props (get-text-property (point) 'org-summaries))
	  (outline-next-heading)
	  (setq end (point))
	  (when (memq which '(all special))
	    ;; Get the special properties, like TODO and tags
	    (goto-char beg)
	    (when (and (looking-at org-todo-line-regexp) (match-end 2))
	      (push (cons "TODO" (org-match-string-no-properties 2)) props))
	    (when (looking-at org-priority-regexp)
	      (push (cons "PRIORITY" (org-match-string-no-properties 2)) props))
	    (when (and (setq value (org-get-tags)) (string-match "\\S-" value))
	      (push (cons "TAGS" value) props))
	    (when (setq value (org-get-tags-at))
	      (push (cons "ALLTAGS" (concat ":" (mapconcat 'identity value ":") ":"))
		    props))
	    (while (re-search-forward org-keyword-time-regexp end t)
	      (setq key (substring (org-match-string-no-properties 1) 0 -1))
	      (unless (member key excluded) (push key excluded))
	      (push (cons key
			  (if (equal key clockstr)
			      (org-no-properties
			       (org-trim
				(buffer-substring
				 (match-beginning 2) (point-at-eol))))
			    (org-match-string-no-properties 2)))
		    props)))
	  (when (memq which '(all standard))
	    ;; Get the standard properties, like :PORP: ...
	    (setq range (org-get-property-block beg end))
	    (when range
	      (goto-char (car range))
	      (while (re-search-forward
		      (org-re "^[ \t]*:\\([[:alpha:]][[:alnum:]_-]*\\):[ \t]*\\(\\S-.*\\)?")
		      (cdr range) t)
		(setq key (org-match-string-no-properties 1)
		      value (org-trim (or (org-match-string-no-properties 2) "")))
		(unless (member key excluded)
		  (push (cons key (or value "")) props)))))
	  (append sum-props (nreverse props)))))))
  
(defun org-entry-get (pom property &optional inherit)
  "Get value of PROPERTY for entry at point-or-marker POM.
If INHERIT is non-nil and the entry does not have the property,
then also check higher levels of the hierarchy.
If the property is present but empty, the return value is the empty string.
If the property is not present at all, nil is returned."
  (org-with-point-at pom
    (if inherit
	(org-entry-get-with-inheritance property)
      (if (member property org-special-properties)
	  ;; We need a special property.  Use brute force, get all properties.
	  (cdr (assoc property (org-entry-properties nil 'special)))
	(let ((range (org-get-property-block)))
	  (if (and range
		   (goto-char (car range))
		   (re-search-forward
		    (concat "^[ \t]*:" property ":[ \t]*\\(.*\\S-\\)?")
		    (cdr range) t))
	      ;; Found the property, return it.
	      (if (match-end 1)
		  (org-match-string-no-properties 1)
		"")))))))

(defun org-entry-delete (pom property)
  "Delete the property PROPERTY from entry at point-or-marker POM."
  (org-with-point-at pom
    (if (member property org-special-properties)
	nil ; cannot delete these properties.
      (let ((range (org-get-property-block)))
	(if (and range
		 (goto-char (car range))
		 (re-search-forward
		  (concat "^[ \t]*:" property ":[ \t]*\\(.*\\S-\\)")
		  (cdr range) t))
	    (progn
	      (delete-region (match-beginning 0) (1+ (point-at-eol)))
	      t)
	  nil)))))

(defvar org-entry-property-inherited-from (make-marker))

(defun org-entry-get-with-inheritance (property)
  "Get entry property, and search higher levels if not present."
  (let (tmp)
    (save-excursion
      (catch 'ex
	(while t
	  (when (setq tmp (org-entry-get nil property))
	    (org-back-to-heading t)
	    (move-marker org-entry-property-inherited-from (point))
	    (throw 'ex tmp))
	  (condition-case nil
	      (org-up-heading-all 1)
	    (error (throw 'ex nil))))))
    (or tmp (cdr (assoc property org-local-properties))
	(cdr (assoc property org-global-properties)))))

(defun org-entry-put (pom property value)
  "Set PROPERTY to VALUE for entry at point-or-marker POM."
  (org-with-point-at pom
    (org-back-to-heading t)
    (let ((beg (point)) (end (save-excursion (outline-next-heading) (point)))
	  range)
      (cond
       ((equal property "TODO")
	(when (and (stringp value) (string-match "\\S-" value)
		   (not (member value org-todo-keywords-1)))
	  (error "\"%s\" is not a valid TODO state" value))
	(if (or (not value)
		(not (string-match "\\S-" value)))
	    (setq value 'none))
	(org-todo value)
	(org-set-tags nil 'align))
       ((equal property "PRIORITY")
	(org-priority (if (and value (stringp value) (string-match "\\S-" value))
			       (string-to-char value) ?\ ))
	(org-set-tags nil 'align))
       ((equal property "SCHEDULED")
	(if (re-search-forward org-scheduled-time-regexp end t)
	    (cond
	     ((eq value 'earlier) (org-timestamp-change -1 'day))
	     ((eq value 'later) (org-timestamp-change 1 'day))
	     (t (call-interactively 'org-schedule)))
	  (call-interactively 'org-schedule)))
       ((equal property "DEADLINE")
	(if (re-search-forward org-deadline-time-regexp end t)
	    (cond
	     ((eq value 'earlier) (org-timestamp-change -1 'day))
	     ((eq value 'later) (org-timestamp-change 1 'day))
	     (t (call-interactively 'org-deadline)))
	  (call-interactively 'org-deadline)))
       ((member property org-special-properties)
	(error "The %s property can not yet be set with `org-entry-put'"
	       property))
       (t ; a non-special property
	(setq range (org-get-property-block beg end 'force))
	(goto-char (car range))
	(if (re-search-forward
	     (concat "^[ \t]*:" property ":\\(.*\\)") (cdr range) t)
	    (progn
	      (delete-region (match-beginning 1) (match-end 1))
	      (goto-char (match-beginning 1)))
	  (goto-char (cdr range))
	  (insert "\n")
	  (backward-char 1)
	  (org-indent-line-function)
	  (insert ":" property ":"))
	(and value (insert " " value))
	(org-indent-line-function))))))

(defun org-buffer-property-keys (&optional include-specials)
  "Get all property keys in the current buffer."
  (let (rtn range)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward org-property-start-re nil t)
	  (setq range (org-get-property-block))
	  (goto-char (car range))
	  (while (re-search-forward
		  (org-re "^[ \t]*:\\([[:alnum:]_-]+\\):")
		  (cdr range) t)
	    (add-to-list 'rtn (org-match-string-no-properties 1)))
	  (outline-next-heading))))
    (when include-specials
      (setq rtn (append org-special-properties rtn)))
    (sort rtn (lambda (a b) (string< (upcase a) (upcase b))))))

(defun org-insert-property-drawer ()
  "Insert a property drawer into the current entry."
  (interactive)
  (org-back-to-heading t)
  (let ((beg (point))
	(re (concat "^[ \t]*" org-keyword-time-regexp))
	end hiddenp)
    (outline-next-heading)
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward re end t))
    (setq hiddenp (org-invisible-p))
    (end-of-line 1)
    (and (= (char-after) ?\n) (forward-char 1))
    (org-skip-over-state-notes)
    (end-of-line 0)
    (insert "\n:PROPERTIES:\n:END:")
    (beginning-of-line 0)
    (org-indent-line-function)
    (beginning-of-line 2)
    (org-indent-line-function)
    (beginning-of-line 0)
    (if hiddenp
	(save-excursion
	  (org-back-to-heading t)
	  (hide-entry))
      (org-flag-drawer t))))

(defun org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE."
  (interactive
   (let* ((prop	(completing-read "Property: " 
				 (mapcar 'list (org-buffer-property-keys))))
	  (cur (org-entry-get nil prop))
	  (allowed (org-property-get-allowed-values nil prop 'table))
	  (val (if allowed
		   (completing-read "Value: " allowed nil 'req-match)
		 (read-string
		  (concat "Value" (if (and cur (string-match "\\S-" cur))
				      (concat "[" cur "]") "")
			  ": ")
		  "" cur))))
     (list prop (if (equal val "") cur val))))
  (unless (equal (org-entry-get nil property) value)
    (org-entry-put nil property value)))

(defun org-delete-property (property)
  "In the current entry, delete PROPERTY."
  (interactive
   (let* ((prop (completing-read
		 "Property: " (org-entry-properties nil 'standard))))
     (list prop)))
  (message (concat "Property " property 
		   (if (org-entry-delete nil property)
		       " deleted"
		     " was not present in the entry"))))

(defun org-delete-property-globally (property)
  "Remove PROPERTY globally, from all entries."
  (interactive
   (let* ((prop (completing-read
		 "Globally remove property: " 
		 (mapcar 'list (org-buffer-property-keys)))))
     (list prop)))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((cnt 0))
	(while (re-search-forward
		(concat "^[ \t]*:" (regexp-quote property) ":.*\n?")
		nil t)
	  (setq cnt (1+ cnt))
	  (replace-match ""))
	(message "Property \"%s\" removed from %d entries" property cnt)))))

(defvar org-columns-current-fmt-compiled) ; defined below

(defun org-compute-property-at-point ()
  "FIXME:"
  (interactive)
  (unless (org-at-property-p)
    (error "Not at a property"))
  (let ((prop (org-match-string-no-properties 2)))
    (org-columns-get-format-and-top-level)
    (unless (nth 3 (assoc prop org-columns-current-fmt-compiled))
      (error "No operator defined for property %s" prop))
    (org-columns-compute prop)))

(defun org-property-get-allowed-values (pom property &optional table)
  "Get allowed values for the property PROPERTY.
When TABLE is non-nil, return an alist that can directly be used for
completion."
  (let (vals)
    (cond
     ((equal property "TODO")
      (setq vals (org-with-point-at pom 
		   (append org-todo-keywords-1 '("")))))
     ((equal property "PRIORITY")
      (let ((n org-lowest-priority))
	(while (>= n org-highest-priority)
	  (push (char-to-string n) vals)
	  (setq n (1- n)))))
     ((member property org-special-properties))
     (t
      (setq vals (org-entry-get pom (concat property "_ALL") 'inherit))
	    
      (when (and vals (string-match "\\S-" vals))
	(setq vals (car (read-from-string (concat "(" vals ")"))))
	(setq vals (mapcar (lambda (x)
			     (cond ((stringp x) x)
				   ((numberp x) (number-to-string x))
				   ((symbolp x) (symbol-name x))
				   (t "???")))
			   vals)))))
    (if table (mapcar 'list vals) vals)))

(defun org-property-previous-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (org-property-next-allowed-value t))

(defun org-property-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (unless (org-at-property-p)
    (error "Not at a property"))
  (let* ((key (match-string 2))
	 (value (match-string 3))
	 (allowed (or (org-property-get-allowed-values (point) key)
		      (and (member value  '("[ ]" "[-]" "[X]"))
			   '("[ ]" "[X]"))))
	 nval)
    (unless allowed
      (error "Allowed values for this property have not been defined"))
    (if previous (setq allowed (reverse allowed)))
    (if (member value allowed)
	(setq nval (car (cdr (member value allowed)))))
    (setq nval (or nval (car allowed)))
    (if (equal nval value)
	(error "Only one allowed value for this property"))
    (org-at-property-p)
    (replace-match (concat " :" key ": " nval) t t)
    (org-indent-line-function)
    (beginning-of-line 1)
    (skip-chars-forward " \t")))

;;; Column View

(defvar org-columns-overlays nil
  "Holds the list of current column overlays.")

(defvar org-columns-current-fmt nil
  "Local variable, holds the currently active column format.")
(defvar org-columns-current-fmt-compiled nil
  "Local variable, holds the currently active column format.
This is the compiled version of the format.")
(defvar org-columns-current-maxwidths nil
  "Loval variable, holds the currently active maximum column widths.")
(defvar org-columns-begin-marker (make-marker)
  "Points to the position where last a column creation command was called.")
(defvar org-columns-top-level-marker (make-marker)
  "Points to the position where current columns region starts.")

(defvar org-columns-map (make-sparse-keymap)
  "The keymap valid in column display.")

(defun org-columns-content ()
  "Switch to contents view while in columns view."
  (interactive)
  (org-overview)
  (org-content))

(org-defkey org-columns-map "c" 'org-columns-content)
(org-defkey org-columns-map "o" 'org-overview)
(org-defkey org-columns-map "e" 'org-columns-edit-value)
(org-defkey org-columns-map "v" 'org-columns-show-value)
(org-defkey org-columns-map "q" 'org-columns-quit)
(org-defkey org-columns-map "r" 'org-columns-redo)
(org-defkey org-columns-map [left] 'backward-char)
(org-defkey org-columns-map "a" 'org-columns-edit-allowed)
(org-defkey org-columns-map "s" 'org-columns-edit-attributes)
(org-defkey org-columns-map [right] 'forward-char)
(org-defkey org-columns-map [right] (lambda () (interactive) (goto-char (1+ (point)))))
(org-defkey org-columns-map [(shift right)] 'org-columns-next-allowed-value)
(org-defkey org-columns-map "\C-c\C-c" 'org-columns-next-allowed-value)
(org-defkey org-columns-map "n" 'org-columns-next-allowed-value)
(org-defkey org-columns-map [(shift left)] 'org-columns-previous-allowed-value)
(org-defkey org-columns-map "p" 'org-columns-previous-allowed-value)
(org-defkey org-columns-map "<" 'org-columns-narrow)
(org-defkey org-columns-map ">" 'org-columns-widen)
(org-defkey org-columns-map [(meta right)] 'org-columns-move-right)
(org-defkey org-columns-map [(meta left)] 'org-columns-move-left)
(org-defkey org-columns-map [(shift meta right)] 'org-columns-new)
(org-defkey org-columns-map [(shift meta left)] 'org-columns-delete)

(easy-menu-define org-columns-menu org-columns-map "Org Column Menu"
  '("Column"
    ["Edit property" org-columns-edit-value t]
    ["Next allowed value" org-columns-next-allowed-value t]
    ["Previous allowed value" org-columns-previous-allowed-value t]
    ["Show full value" org-columns-show-value t]
    ["Edit allowed" org-columns-edit-allowed t]
    "--"
    ["Edit column attributes" org-columns-edit-attributes t]
    ["Increase column width" org-columns-widen t]
    ["Decrease column width" org-columns-narrow t]
    "--"
    ["Move column right" org-columns-move-right t]
    ["Move column left" org-columns-move-left t]
    ["Add column" org-columns-new t]
    ["Delete column" org-columns-delete t]
    "--"
    ["CONTENTS" org-columns-content t]
    ["OVERVIEW" org-overview t]
    ["Refresh columns display" org-columns-redo t]
    "--"
    ["Quit" org-columns-quit t]))

(defun org-columns-new-overlay (beg end &optional string face)
  "Create a new column overlay and add it to the list."
  (let ((ov (org-make-overlay beg end)))
    (org-overlay-put ov 'face (or face 'secondary-selection))
    (org-overlay-display ov string face)
    (push ov org-columns-overlays)
    ov))

(defun org-columns-display-here (&optional props)
  "Overlay the current line with column display."
  (interactive)
  (let* ((fmt org-columns-current-fmt-compiled)
	 (beg (point-at-bol))
	 (level-face (save-excursion
		       (beginning-of-line 1)
		       (and (looking-at "\\(\\**\\)\\(\\* \\)")
			    (org-get-level-face 2))))
	 (color (list :foreground 
		      (face-attribute (or level-face 'default) :foreground)))
	 props pom property ass width f string ov column)
    ;; Check if the entry is in another buffer.
    (unless props
      (if (eq major-mode 'org-agenda-mode)
	  (setq pom (or (get-text-property (point) 'org-hd-marker)
			(get-text-property (point) 'org-marker))
		props (if pom (org-entry-properties pom) nil))
	(setq props (org-entry-properties nil))))
    ;; Walk the format
    (while (setq column (pop fmt))
      (setq property (car column)
	    ass (if (equal property "ITEM")
		    (cons "ITEM"
			  (save-match-data
			    (org-no-properties
			     (org-remove-tabs
			      (buffer-substring-no-properties
			       (point-at-bol) (point-at-eol))))))
		  (assoc property props))
	    width (or (cdr (assoc property org-columns-current-maxwidths))
		      (nth 2 column))
	    f (format "%%-%d.%ds | " width width)
	    string (format f (or (cdr ass) "")))
      ;; Create the overlay
      (org-unmodified
       (setq ov (org-columns-new-overlay
		 beg (setq beg (1+ beg)) string
		 (list color 'org-column)))
;;;       (list (get-text-property (point-at-bol) 'face) 'org-column)))
       (org-overlay-put ov 'keymap org-columns-map)
       (org-overlay-put ov 'org-columns-key property)
       (org-overlay-put ov 'org-columns-value (cdr ass))
       (org-overlay-put ov 'org-columns-pom pom)
       (org-overlay-put ov 'org-columns-format f))
      (if (or (not (char-after beg))
	      (equal (char-after beg) ?\n))
	  (let ((inhibit-read-only t))
	    (save-excursion
	      (goto-char beg)
	      (insert " ")))))
    ;; Make the rest of the line disappear.
    (org-unmodified
     (setq ov (org-columns-new-overlay beg (point-at-eol)))
     (org-overlay-put ov 'invisible t)
     (org-overlay-put ov 'keymap org-columns-map)
     (org-overlay-put ov 'intangible t)
     (push ov org-columns-overlays)
     (setq ov (org-make-overlay (1- (point-at-eol)) (1+ (point-at-eol))))
     (org-overlay-put ov 'keymap org-columns-map)
     (push ov org-columns-overlays)
     (let ((inhibit-read-only t))
       (put-text-property (max (point-min) (1- (point-at-bol)))
			  (min (point-max) (1+ (point-at-eol)))
			  'read-only "Type `e' to edit property")))))

(defvar org-previous-header-line-format nil
  "The header line format before column view was turned on.")
(defvar org-columns-inhibit-recalculation nil
  "Inhibit recomputing of columns on column view startup.")

(defvar header-line-format)
(defun org-columns-display-here-title ()
  "Overlay the newline before the current line with the table title."
  (interactive)
  (let ((fmt org-columns-current-fmt-compiled)
	string (title "")
	property width f column str)
    (while (setq column (pop fmt))
      (setq property (car column)
	    str (or (nth 1 column) property)
	    width (or (cdr (assoc property org-columns-current-maxwidths))
		      (nth 2 column))
	    f (format "%%-%d.%ds | " width width)
	    string (format f str)
	    title (concat title string)))
    (setq title (concat
		 (org-add-props " " nil 'display '(space :align-to 0))
		 (org-add-props title nil 'face '(:weight bold :underline t))))
    (org-set-local 'org-previous-header-line-format header-line-format)
    (setq header-line-format title)))

(defun org-columns-remove-overlays ()
  "Remove all currently active column overlays."
  (interactive)
  (when (marker-buffer org-columns-begin-marker)
    (with-current-buffer (marker-buffer org-columns-begin-marker)
      (when (local-variable-p 'org-previous-header-line-format)
	(setq header-line-format org-previous-header-line-format)
	(kill-local-variable 'org-previous-header-line-format))
      (move-marker org-columns-begin-marker nil)
      (move-marker org-columns-top-level-marker nil)
      (org-unmodified
       (mapc 'org-delete-overlay org-columns-overlays)
       (setq org-columns-overlays nil)
       (let ((inhibit-read-only t))
	 (remove-text-properties (point-min) (point-max) '(read-only t)))))))

(defun org-columns-show-value ()
  "Show the full value of the property."
  (interactive)
  (let ((value (get-char-property (point) 'org-columns-value)))
    (message "Value is: %s" (or value ""))))

(defun org-columns-quit ()
  "Remove the column overlays and in this way exit column editing."
  (interactive)
  (org-unmodified
   (org-columns-remove-overlays)
   (let ((inhibit-read-only t))
     ;; FIXME: is this safe???
     ;; or are there other reasons why there may be a read-only property????
     (remove-text-properties (point-min) (point-max) '(read-only t))))
  (when (eq major-mode 'org-agenda-mode)
    (message "Modification not yet reflected in Agenda buffer, use `r' to refresh")))

(defun org-columns-edit-value ()
  "Edit the value of the property at point in column view.
Where possible, use the standard interface for changing this line."
  (interactive)
  (let* ((col (current-column))
	 (key (get-char-property (point) 'org-columns-key))
	 (value (get-char-property (point) 'org-columns-value))
	 (bol (point-at-bol)) (eol (point-at-eol))
	 (pom (or (get-text-property bol 'org-hd-marker)
		  (point))) ; keep despite of compiler waring
	 (line-overlays
	  (delq nil (mapcar (lambda (x)
			      (and (eq (overlay-buffer x) (current-buffer))
				   (>= (overlay-start x) bol)
				   (<= (overlay-start x) eol)
				   x))
			    org-columns-overlays)))
	 nval eval allowed)
    (when (equal key "ITEM")
      (error "Cannot edit item headline from here"))
    
    (cond
     ((equal key "TODO")
      (setq eval '(org-with-point-at pom
		    (let ((current-prefix-arg '(4))) (org-todo '(4))))))
     ((equal key "PRIORITY")
      (setq eval '(org-with-point-at pom
		    (call-interactively 'org-priority))))
     ((equal key "TAGS")
      (setq eval '(org-with-point-at pom
		    (let ((org-fast-tag-selection-single-key
			   (if (eq org-fast-tag-selection-single-key 'expert)
			       t org-fast-tag-selection-single-key)))
		      (call-interactively 'org-set-tags)))))
     ((equal key "DEADLINE")
      (setq eval '(org-with-point-at pom
		    (call-interactively 'org-deadline))))
     ((equal key "SCHEDULED")
      (setq eval '(org-with-point-at pom
		    (call-interactively 'org-schedule))))
     (t
      (setq allowed (org-property-get-allowed-values pom key 'table))
      (if allowed
	  (setq nval (completing-read "Value: " allowed nil t))
	(setq nval (read-string "Edit: " value)))
      (setq nval (org-trim nval))
      (when (not (equal nval value))
	(setq eval '(org-entry-put pom key nval)))))
    (when eval
      (let ((inhibit-read-only t))
	(remove-text-properties (1- bol) eol '(read-only t))
	(unwind-protect
	    (progn
	      (setq org-columns-overlays 
		    (org-delete-all line-overlays org-columns-overlays))
	      (mapc 'org-delete-overlay line-overlays)
	      (org-columns-eval eval))
	  (org-columns-display-here))))
    (move-to-column col)
    (if (nth 3 (assoc key org-columns-current-fmt-compiled))
	(org-columns-update key))))

(defun org-columns-edit-allowed ()
  "Edit the list of allowed values for the current property."
  (interactive)
  (let* ((col (current-column))
	 (key (get-char-property (point) 'org-columns-key))
	 (key1 (concat key "_ALL"))
	 (value (get-char-property (point) 'org-columns-value))
	 (allowed (org-entry-get (point) key1 t))
	 nval)
    (setq nval (read-string "Allowed: " allowed))
    (org-entry-put 
     (cond ((marker-position org-entry-property-inherited-from)
	    org-entry-property-inherited-from)
	   ((marker-position org-columns-top-level-marker)
	    org-columns-top-level-marker))
     key1 nval)))

(defun org-columns-eval (form)
  (let (hidep)
    (save-excursion
      (beginning-of-line 1)
      (next-line 1)
      (setq hidep (org-on-heading-p 1)))
    (eval form)
    (and hidep (hide-entry))))

(defun org-columns-previous-allowed-value ()
  "Switch to the previous allowed value for this column."
  (interactive)
  (org-columns-next-allowed-value t))

(defun org-columns-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this column."
  (interactive)
  (let* ((col (current-column))
	 (key (get-char-property (point) 'org-columns-key))
	 (value (get-char-property (point) 'org-columns-value))
	 (bol (point-at-bol)) (eol (point-at-eol))
	 (pom (or (get-text-property bol 'org-hd-marker)
		  (point))) ; keep despite of compiler waring
	 (line-overlays
	  (delq nil (mapcar (lambda (x)
			      (and (eq (overlay-buffer x) (current-buffer))
				   (>= (overlay-start x) bol)
				   (<= (overlay-start x) eol)
				   x))
			    org-columns-overlays)))
	 (allowed (or (org-property-get-allowed-values pom key)
		      (and (equal
			    (nth 4 (assoc key org-columns-current-fmt-compiled))
			    'checkbox) '("[ ]" "[X]"))))
	 nval)
    (when (equal key "ITEM")
      (error "Cannot edit item headline from here"))
    (unless (or allowed (member key '("SCHEDULED" "DEADLINE")))
      (error "Allowed values for this property have not been defined"))
    (if (member key '("SCHEDULED" "DEADLINE"))
	(setq nval (if previous 'earlier 'later))
      (if previous (setq allowed (reverse allowed)))
      (if (member value allowed)
	  (setq nval (car (cdr (member value allowed)))))
      (setq nval (or nval (car allowed)))
      (if (equal nval value)
	  (error "Only one allowed value for this property")))
    (let ((inhibit-read-only t))
      (remove-text-properties (1- bol) eol '(read-only t))
      (unwind-protect
	  (progn
	    (setq org-columns-overlays 
		  (org-delete-all line-overlays org-columns-overlays))
	    (mapc 'org-delete-overlay line-overlays)
	    (org-columns-eval '(org-entry-put pom key nval)))
	(org-columns-display-here)))
    (move-to-column col)
    (if (nth 3 (assoc key org-columns-current-fmt-compiled))
	(org-columns-update key))))

(defun org-verify-version (task)
  (cond
   ((eq task 'columns)
    (if (or (featurep 'xemacs)
	    (< emacs-major-version 22))
	(error "Emacs 22 is required for the columns feature")))))

(defun org-columns-get-format-and-top-level ()
  (let (fmt)
    (when (condition-case nil (org-back-to-heading) (error nil))
      (move-marker org-entry-property-inherited-from nil)
      (setq fmt (org-entry-get nil "COLUMNS" t)))
    (setq fmt (or fmt org-columns-default-format))
    (org-set-local 'org-columns-current-fmt fmt)
    (org-columns-compile-format fmt)
    (if (marker-position org-entry-property-inherited-from)
	(move-marker org-columns-top-level-marker
		     org-entry-property-inherited-from)
      (move-marker org-columns-top-level-marker (point)))
    fmt))

(defun org-columns ()
  "Turn on column view on an org-mode file."
  (interactive)
  (org-verify-version 'columns)
  (org-columns-remove-overlays)
  (move-marker org-columns-begin-marker (point))
  (let (beg end fmt cache maxwidths)
    (setq fmt (org-columns-get-format-and-top-level))
    (save-excursion
      (goto-char org-columns-top-level-marker)
      (setq beg (point))
      (unless org-columns-inhibit-recalculation
	(org-columns-compute-all))
      (setq end (or (condition-case nil (org-end-of-subtree t t) (error nil))
		    (point-max)))
      (goto-char beg)
      ;; Get and cache the properties
      (while (re-search-forward (concat "^" outline-regexp) end t)
	(push (cons (org-current-line) (org-entry-properties)) cache))
      (when cache
	(setq maxwidths (org-columns-get-autowidth-alist fmt cache))
	(org-set-local 'org-columns-current-maxwidths maxwidths)
	(org-columns-display-here-title)
	(mapc (lambda (x)
		(goto-line (car x))
		(org-columns-display-here (cdr x)))
	      cache)))))

(defun org-columns-new (&optional prop title width op fmt)
  "Insert a new column, to the leeft o the current column."
  (interactive)
  (let ((editp (and prop (assoc prop org-columns-current-fmt-compiled)))
	cell)
    (setq prop (completing-read
		"Property: " (mapcar 'list (org-buffer-property-keys t))
		nil nil prop))
    (setq title (read-string (concat "Column title [" prop "]: ") (or title prop)))
    (setq width (read-string "Column width: " (if width (number-to-string width))))
    (if (string-match "\\S-" width)
	(setq width (string-to-number width))
      (setq width nil))
    (setq fmt (completing-read "Summary [none]: "
			       '(("none") ("add_numbers") ("add_times") ("checkbox"))
			       nil t))
    (if (string-match "\\S-" fmt)
	(setq fmt (intern fmt))
      (setq fmt nil))
    (if (eq fmt 'none) (setq fmt nil))
    (if editp
	(progn
	  (setcar editp prop)
	  (setcdr editp (list title width nil fmt)))
      (setq cell (nthcdr (1- (current-column))
			 org-columns-current-fmt-compiled))
      (setcdr cell (cons (list prop title width nil fmt)
			 (cdr cell))))
    (org-columns-store-format)
    (org-columns-redo)))

(defun org-columns-delete ()
  "Delete the column at point from columns view."
  (interactive)
  (let* ((n (current-column))
	 (title (nth 1 (nth n org-columns-current-fmt-compiled))))
    (when (y-or-n-p
	   (format "Are you sure you want to remove column \"%s\"? " title))
      (setq org-columns-current-fmt-compiled
	    (delq (nth n org-columns-current-fmt-compiled)
		  org-columns-current-fmt-compiled))
      (org-columns-store-format)
      (org-columns-redo)
      (if (>= (current-column) (length org-columns-current-fmt-compiled))
	  (backward-char 1)))))

(defun org-columns-edit-attributes ()
  "Edit the attributes of the current column."
  (interactive)
  (let* ((n (current-column))
	 (info (nth n org-columns-current-fmt-compiled)))
    (apply 'org-columns-new info)))

(defun org-columns-widen (arg)
  "Make the column wider by ARG characters."
  (interactive "p")
  (let* ((n (current-column))
	 (entry (nth n org-columns-current-fmt-compiled))
	 (width (or (nth 2 entry)
		    (cdr (assoc (car entry) org-columns-current-maxwidths)))))
    (setq width (max 1 (+ width arg)))
    (setcar (nthcdr 2 entry) width)
    (org-columns-store-format)
    (org-columns-redo)))

(defun org-columns-narrow (arg)
  "Make the column nrrower by ARG characters."
  (interactive "p")
  (org-columns-widen (- arg)))

(defun org-columns-move-right ()
  "Swap this column with the one to the right."
  (interactive)
  (let* ((n (current-column))
	 (cell (nthcdr n org-columns-current-fmt-compiled))
	 e)
    (when (>= n (1- (length org-columns-current-fmt-compiled)))
      (error "Cannot shift this column further to the right"))
    (setq e (car cell))
    (setcar cell (car (cdr cell)))
    (setcdr cell (cons e (cdr (cdr cell))))
    (org-columns-store-format)
    (org-columns-redo)
    (forward-char 1)))

(defun org-columns-move-left ()
  "Swap this column with the one to the left."
  (interactive)
  (let* ((n (current-column)))
    (when (= n 0)
      (error "Cannot shift this column further to the left"))
    (backward-char 1)
    (org-columns-move-right)
    (backward-char 1)))    

(defun org-columns-store-format ()
  "Store the text version of the current columns format in appropriate place.
This is either in the COLUMNS property of the node starting the current column
display, or in the #+COLUMNS line of the current buffer."
  (let (fmt)
    (setq fmt (org-columns-uncompile-format org-columns-current-fmt-compiled))
    (if (marker-position org-columns-top-level-marker)
	(save-excursion
	  (goto-char org-columns-top-level-marker)
	  (if (org-entry-get nil "COLUMNS")
	      (org-entry-put nil "COLUMNS" fmt)
	    (goto-char (point-min))
	    (while (re-search-forward "^#\\+COLUMNS:.*" nil t)
	      (replace-match (concat "#+COLUMNS: " fmt t t)))))
      (setq org-columns-current-fmt fmt))))

(defvar org-overriding-columns-format nil
  "When set, overrides any other definition.")
(defvar org-agenda-view-columns-initially nil
  "When set, switch to columns view immediately after creating the agenda.")

(defun org-agenda-columns ()
  "Turn on column view in the agenda."
  (interactive)
  (org-verify-version 'columns)
  (org-columns-remove-overlays)
  (move-marker org-columns-begin-marker (point))
  (let (fmt cache maxwidths m)
    (cond
     ((and (local-variable-p 'org-overriding-columns-format)
	   org-overriding-columns-format)
      (setq fmt org-overriding-columns-format))
     ((setq m (get-text-property (point-at-bol) 'org-hd-marker))
      (setq fmt (org-entry-get m "COLUMNS" t)))
     ((and (boundp 'org-columns-current-fmt)
	   (local-variable-p 'org-columns-current-fmt)
	   org-columns-current-fmt)
      (setq fmt org-columns-current-fmt))
     ((setq m (next-single-property-change (point-min) 'org-hd-marker))
      (setq m (get-text-property m 'org-hd-marker))
      (setq fmt (org-entry-get m "COLUMNS" t))))
    (setq fmt (or fmt org-columns-default-format))
    (org-set-local 'org-columns-current-fmt fmt)
    (org-columns-compile-format fmt)
    (save-excursion
      ;; Get and cache the properties
      (goto-char (point-min))
      (while (not (eobp))
	(when (setq m (or (get-text-property (point) 'org-hd-marker)
			  (get-text-property (point) 'org-marker)))
	  (push (cons (org-current-line) (org-entry-properties m)) cache))
	(beginning-of-line 2))
      (when cache
	(setq maxwidths (org-columns-get-autowidth-alist fmt cache))
	(org-set-local 'org-columns-current-maxwidths maxwidths)
	(org-columns-display-here-title)
	(mapc (lambda (x)
		(goto-line (car x))
		(org-columns-display-here (cdr x)))
	      cache)))))

(defun org-columns-get-autowidth-alist (s cache)
  "Derive the maximum column widths from the format and the cache."
  (let ((start 0) rtn)
    (while (string-match (org-re "%\\([[:alpha:]]\\S-*\\)") s start)
      (push (cons (match-string 1 s) 1) rtn)
      (setq start (match-end 0)))
    (mapc (lambda (x)
	    (setcdr x (apply 'max
			     (mapcar
			      (lambda (y)
				(length (or (cdr (assoc (car x) (cdr y))) " ")))
			      cache))))
	  rtn)
    rtn))

(defun org-columns-compute-all ()
  "Compute all columns that have operators defined."
  (org-unmodified
   (remove-text-properties (point-min) (point-max) '(org-summaries t)))
  (let ((columns org-columns-current-fmt-compiled) col)
    (while (setq col (pop columns))
      (when (nth 3 col)
	(save-excursion
	  (org-columns-compute (car col)))))))

(defun org-columns-update (property)
  "Recompute PROPERTY, and update the columns display for it."
  (org-columns-compute property)
  (let (fmt val pos)
    (save-excursion
      (mapc (lambda (ov)
	      (when (equal (org-overlay-get ov 'org-columns-key) property)
		(setq pos (org-overlay-start ov))
		(goto-char pos)
		(when (setq val (cdr (assoc property
					    (get-text-property (point-at-bol) 'org-summaries))))
		  (setq fmt (org-overlay-get ov 'org-columns-format))
		  (org-overlay-put ov 'display (format fmt val)))))
	    org-columns-overlays))))

(defun org-columns-compute (property)
  "Sum the values of property PROPERTY hierarchically, for the entire buffer."
  (interactive)
  (let* ((re (concat "^" outline-regexp))
	 (lmax 30) ; Does anyone use deeper levels???
	 (lsum (make-vector lmax 0))
	 (level 0)
	 (ass (assoc property org-columns-current-fmt-compiled))
	 (format (nth 4 ass))
	 (beg org-columns-top-level-marker)
	 last-level val end sumpos sum-alist sum str)
    (save-excursion
      ;; Find the region to compute
      (goto-char beg)
      (setq end (condition-case nil (org-end-of-subtree t) (error (point-max))))
      (goto-char end)
      ;; Walk the tree from the back and do the computations
      (while (re-search-backward re beg t)
	(setq sumpos (match-beginning 0)
	      last-level level
	      level (org-outline-level)
	      val (org-entry-get nil property))
	(cond
	 ((< level last-level)
	  ;; put the sum of lower levels here as a property
	  (setq sum (aref lsum last-level)
		str (org-column-number-to-string sum format)
		sum-alist (get-text-property sumpos 'org-summaries))
	  (if (assoc property sum-alist)
	      (setcdr (assoc property sum-alist) str)
	    (push (cons property str) sum-alist)
	    (org-unmodified
	     (add-text-properties sumpos (1+ sumpos)
				  (list 'org-summaries sum-alist))))
	  (when val ;?????????????????????????????????? and force?????
	    (org-entry-put nil property str))
	  ;; add current to current  level accumulator
	  (aset lsum level (+ (aref lsum level) sum))
	  ;; clear accumulators for deeper levels
	  (loop for l from (1+ level) to (1- lmax) do (aset lsum l 0)))
	 ((>= level last-level)
	  ;; add what we have here to the accumulator for this level
	  (aset lsum level (+ (aref lsum level)
				(org-column-string-to-number (or val "0") format))))
	 (t (error "This should not happen")))))))

(defun org-columns-redo ()
  "Construct the column display again."
  (interactive)
  (message "Recomputing columns...")
  (save-excursion
    (if (marker-position org-columns-begin-marker)
	(goto-char org-columns-begin-marker))
    (org-columns-remove-overlays)
    (if (org-mode-p)
	(call-interactively 'org-columns)
      (call-interactively 'org-agenda-columns)))
  (message "Recomputing columns...done"))

(defun org-columns-not-in-agenda ()
  (if (eq major-mode 'org-agenda-mode)
      (error "This command is only allowed in Org-mode buffers")))


(defun org-string-to-number (s)
  "Convert string to number, and interpret hh:mm:ss."
  (if (not (string-match ":" s))
      (string-to-number s)
    (let ((l (nreverse (org-split-string s ":"))) (sum 0.0))
      (while l
	(setq sum (+ (string-to-number (pop l)) (/ sum 60))))
      sum)))

(defun org-column-number-to-string (n fmt)
  "Convert a computed column number to a string value, according to FMT."
  (cond
   ((eq fmt 'add_times)
    (let* ((h (floor n)) (m (floor (+ 0.5 (* 60 (- n h))))))
      (format "%d:%02d" h m)))
   ((eq fmt 'checkbox)
    (cond ((= n (floor n)) "[X]")
	  ((> n 1.) "[-]")
	  (t "[ ]")))
   (t (number-to-string n))))

(defun org-column-string-to-number (s fmt)
  "Convert a column value to a number that can be used for column computing."
  (cond
   ((string-match ":" s)
    (let ((l (nreverse (org-split-string s ":"))) (sum 0.0))
      (while l
	(setq sum (+ (string-to-number (pop l)) (/ sum 60))))
      sum))
   ((eq fmt 'checkbox)
    (if (equal s "[X]") 1. 0.000001))
   (t (string-to-number s))))

(defun org-columns-uncompile-format (cfmt)
  "Turn the compiled columns format back into a string representation."
  (let ((rtn "") e s prop title op width fmt)
    (while (setq e (pop cfmt))
      (setq prop (car e)
	    title (nth 1 e)
	    width (nth 2 e)
	    op (nth 3 e)
	    fmt (nth 4 e))
      (cond
       ((eq fmt 'add_times) (setq op ":"))
       ((eq fmt 'checkbox) (setq op "X"))
       ((eq fmt 'add_numbers) (setq op "+")))
      (if (equal title prop) (setq title nil))
      (setq s (concat "%" (if width (number-to-string width))
		      prop
		      (if title (concat "(" title ")"))
		      (if op (concat "{" op "}"))))
      (setq rtn (concat rtn " " s)))
    (org-trim rtn)))

(defun org-columns-compile-format (fmt)
  "FIXME"
  (let ((start 0) width prop title op f)
    (setq org-columns-current-fmt-compiled nil)
    (while (string-match
	    (org-re "%\\([0-9]+\\)?\\([[:alnum:]_-]+\\)\\(?:(\\([^)]+\\))\\)?\\(?:{\\([^}]+\\)}\\)?\\s-*")
	    fmt start)
      (setq start (match-end 0)
	    width (match-string 1 fmt)
	    prop (match-string 2 fmt)
	    title (or (match-string 3 fmt) prop)
	    op (match-string 4 fmt)
	    f nil)
      (if width (setq width (string-to-number width)))
      (cond
       ((equal op "+") (setq f 'add_numbers))
       ((equal op ":") (setq f 'add_times))
       ((equal op "X") (setq f 'checkbox)))
      (push (list prop title width op f) org-columns-current-fmt-compiled))
    (setq org-columns-current-fmt-compiled
	  (nreverse org-columns-current-fmt-compiled))))

;;;; Timestamps

(defvar org-last-changed-timestamp nil)
(defvar org-time-was-given) ; dynamically scoped parameter
(defvar org-end-time-was-given) ; dynamically scoped parameter
(defvar org-ts-what) ; dynamically scoped parameter

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
  (let (org-time-was-given org-end-time-was-given time)
    (cond
     ((and (org-at-timestamp-p)
	   (eq last-command 'org-time-stamp)
	   (eq this-command 'org-time-stamp))
      (insert "--")
      (setq time (let ((this-command this-command))
		  (org-read-date arg 'totime)))
      (org-insert-time-stamp time (or org-time-was-given arg)))
     ((org-at-timestamp-p)
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime)))
      (when (org-at-timestamp-p) ; just to get the match data
	(replace-match "")
	(setq org-last-changed-timestamp
	      (org-insert-time-stamp
	       time (or org-time-was-given arg)
	       nil nil nil (list org-end-time-was-given))))
      (message "Timestamp updated"))
     (t
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime)))
      (org-insert-time-stamp time (or org-time-was-given arg)
			     nil nil nil (list org-end-time-was-given))))))      

(defun org-time-stamp-inactive (&optional arg)
  "Insert an inactive time stamp.
An inactive time stamp is enclosed in square brackets instead of angle
brackets.  It is inactive in the sense that it does not trigger agenda entries,
does not link to the calendar and cannot be changed with the S-cursor keys.
So these are more for recording a certain time/date."
  (interactive "P")
  (let (org-time-was-given org-end-time-was-given time)
    (setq time (org-read-date arg 'totime))
    (org-insert-time-stamp time (or org-time-was-given arg) 'inactive
			   nil nil (list org-end-time-was-given))))

(defvar org-date-ovl (org-make-overlay 1 1))
(org-overlay-put org-date-ovl 'face 'org-warning)
(org-detach-overlay org-date-ovl)

(defvar org-ans1) ; dynamically scoped parameter
(defvar org-ans2) ; dynamically scoped parameter

(defvar org-plain-time-of-day-regexp) ; defined below
(defun org-read-date (&optional with-time to-time from-string prompt)
  "Read a date and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  For example,
  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday (today or later)
  +4            --> four days from today (only if +N is the only thing given)
  etc.
The function understands only English month and weekday abbreviations,
but this can be configured with the variables `parse-time-months' and
`parse-time-weekdays'.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
If you don't like the calendar, turn it off with
       \(setq org-popup-calendar-for-date-prompt nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to also
insert a time.  Note that when WITH-TIME is not set, you can still
enter a time, and this function will inform the calling routine about
this change.  The calling routine may then choose to change the format
used to insert the time stamp into the buffer to include the time."
  (require 'parse-time)
  (let* ((org-time-stamp-rounding-minutes
	  (if (equal with-time '(16)) 0 org-time-stamp-rounding-minutes))
	 (ct (org-current-time))
	 (default-time
	   ;; Default time is either today, or, when entering a range,
	   ;; the range start.
	   (if (save-excursion
		 (re-search-backward
		  (concat org-ts-regexp "--?-?\\=") ; 1-3 minuses
		  (- (point) 20) t))
	       (apply
		'encode-time
		(mapcar (lambda(x) (or x 0))
			(parse-time-string (match-string 1))))
	     ct))
	 (calendar-move-hook nil)
	 (view-diary-entries-initially nil)
	 (view-calendar-holidays-initially nil)
	 (timestr (format-time-string
		   (if with-time "%Y-%m-%d %H:%M" "%Y-%m-%d") default-time))
	 (prompt (concat (if prompt (concat prompt " ") "")
			 (format "Date and/or time (default [%s]): " timestr)))
	 ans (org-ans0 "") org-ans1 org-ans2 (deltadays 0)
	 second minute hour day month year tl wday wday1 pm)

    (cond
     (from-string (setq ans from-string))
     (org-popup-calendar-for-date-prompt
      (save-excursion
	(save-window-excursion
	  (calendar)
	  (calendar-forward-day (- (time-to-days default-time)
				   (calendar-absolute-from-gregorian
				    (calendar-current-date))))
	  (org-eval-in-calendar nil t)
	  (let* ((old-map (current-local-map))
		 (map (copy-keymap calendar-mode-map))
		 (minibuffer-local-map (copy-keymap minibuffer-local-map)))
	    (org-defkey map (kbd "RET") 'org-calendar-select)
	    (org-defkey map (if (featurep 'xemacs) [button1] [mouse-1])
	      'org-calendar-select-mouse)
	    (org-defkey map (if (featurep 'xemacs) [button2] [mouse-2])
	      'org-calendar-select-mouse)
	    (org-defkey minibuffer-local-map [(meta shift left)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-backward-month 1))))
	    (org-defkey minibuffer-local-map [(meta shift right)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-forward-month 1))))
	    (org-defkey minibuffer-local-map [(shift up)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-backward-week 1))))
	    (org-defkey minibuffer-local-map [(shift down)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-forward-week 1))))
	    (org-defkey minibuffer-local-map [(shift left)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-backward-day 1))))
	    (org-defkey minibuffer-local-map [(shift right)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-forward-day 1))))
	    (org-defkey minibuffer-local-map ">"
	      (lambda () (interactive)
		(org-eval-in-calendar '(scroll-calendar-left 1))))
	    (org-defkey minibuffer-local-map "<"
	      (lambda () (interactive)
		(org-eval-in-calendar '(scroll-calendar-right 1))))
	    (unwind-protect
		(progn
		  (use-local-map map)
		  (setq org-ans0 (read-string prompt "" nil nil))
		  ;; org-ans0: from prompt
		  ;; org-ans1: from mouse click
		  ;; org-ans2: from calendar motion
		  (setq ans (concat org-ans0 " " (or org-ans1 org-ans2))))
	      (use-local-map old-map))))))
     (t ; Naked prompt only
      (setq ans (read-string prompt "" nil timestr))))
    (org-detach-overlay org-date-ovl)

    (if (string-match "^[ \t]*[-+][0-9]+[ \t]*$" org-ans0)
	(setq deltadays (string-to-number ans) ans ""))

    ;; Help matching ISO dates with single digit month ot day, like 2006-8-11.
    (when (string-match
	   "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
      (setq year (if (match-end 2)
		     (string-to-number (match-string 2 ans))
		   (string-to-number (format-time-string "%Y")))
	    month (string-to-number (match-string 3 ans))
	    day (string-to-number (match-string 4 ans)))
      (if (< year 100) (setq year (+ 2000 year)))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))
    ;; Help matching am/pm times, because `parse-time-string' does not do that.
    ;; If there is a time with am/pm, and *no* time without it, we convert
    ;; so that matching will be successful.
    ;; FIXME: make this replace twice, so that we catch the end time.
    (when (and (not (string-match "[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
	       (string-match "\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\(am\\|AM\\|pm\\|PM\\)\\>" ans))
      (setq hour (string-to-number (match-string 1 ans))
	    minute (if (match-end 3) (string-to-number (match-string 3 ans)) 0)
	    pm (equal ?p (string-to-char (downcase (match-string 4 ans)))))
      (if (and (= hour 12) (not pm))
	  (setq hour 0)
	(if (and pm (< hour 12)) (setq hour (+ 12 hour))))
      (setq ans (replace-match (format "%02d:%02d" hour minute) t t ans)))

    ;; Check if there is a time range
    (when (and (boundp 'org-end-time-was-given)
	       (string-match org-plain-time-of-day-regexp ans)
	       (match-end 8))
      (setq org-end-time-was-given (match-string 8 ans))
      (setq ans (concat (substring ans 0 (match-beginning 7))
			(substring ans (match-end 7)))))

    (setq tl (parse-time-string ans)
	  year (or (nth 5 tl) (string-to-number (format-time-string "%Y" ct)))
	  month (or (nth 4 tl) (string-to-number (format-time-string "%m" ct)))
	  day (or (nth 3 tl) (string-to-number (format-time-string "%d" ct)))
	  hour (or (nth 2 tl) (string-to-number (format-time-string "%H" ct)))
	  minute (or (nth 1 tl) (string-to-number (format-time-string "%M" ct)))
	  second (or (nth 0 tl) 0)
	  wday (nth 6 tl))
    (setq day (+ day deltadays))
    (when (and wday (not (nth 3 tl)))
      ;; Weekday was given, but no day, so pick that day in the week
      ;; on or after the derived date.
      (setq wday1 (nth 6 (decode-time (encode-time 0 0 0 day month year))))
      (unless (equal wday wday1)
	(setq day (+ day (% (- wday wday1 -7) 7)))))
    (if (and (boundp 'org-time-was-given)
	     (nth 2 tl))
	(setq org-time-was-given t))
    (if (< year 100) (setq year (+ 2000 year)))
    (if to-time
	(encode-time second minute hour day month year)
      (if (or (nth 1 tl) (nth 2 tl))
	  (format "%04d-%02d-%02d %02d:%02d" year month day hour minute)
	(format "%04d-%02d-%02d" year month day)))))

(defun org-eval-in-calendar (form &optional keepdate)
  "Eval FORM in the calendar window and return to current window.
Also, store the cursor date in variable org-ans2."
  (let ((sw (selected-window)))
    (select-window (get-buffer-window "*Calendar*"))
    (eval form)
    (when (and (not keepdate) (calendar-cursor-to-date))
      (let* ((date (calendar-cursor-to-date))
	     (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	(setq org-ans2 (format-time-string "%Y-%m-%d" time))))
    (org-move-overlay org-date-ovl (1- (point)) (1+ (point)) (current-buffer))
    (select-window sw)
    ;; Update the prompt to show new default date
    (save-excursion
      (goto-char (point-min))
      (when (and org-ans2
		 (re-search-forward "\\[[-0-9]+\\]" nil t)
		 (get-text-property (match-end 0) 'field))
	(let ((inhibit-read-only t))
	  (replace-match (concat "[" org-ans2 "]") t t)
	  (add-text-properties (point-min) (1+ (match-end 0))
			       (text-properties-at (1+ (point-min)))))))))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (if (active-minibuffer-window) (exit-minibuffer))))

(defun org-insert-time-stamp (time &optional with-hm inactive pre post extra)
  "Insert a date stamp for the date given by the internal TIME.
WITH-HM means, use the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
The command returns the inserted time stamp."
  (let ((fmt (funcall (if with-hm 'cdr 'car) org-time-stamp-formats))
	stamp)
    (if inactive (setq fmt (concat "[" (substring fmt 1 -1) "]")))
    (insert (or pre ""))
    (insert (setq stamp (format-time-string fmt time)))
    (when (listp extra)
      (setq extra (car extra))
      (if (and (stringp extra)
	       (string-match "\\([0-9]+\\):\\([0-9]+\\)" extra))
	  (setq extra (format "-%02d:%02d"
			      (string-to-number (match-string 1 extra))
			      (string-to-number (match-string 2 extra))))
	(setq extra nil)))
    (when extra
      (backward-char 1)
      (insert extra)
      (forward-char 1))
    (insert (or post ""))
    stamp))

(defun org-toggle-time-stamp-overlays ()
  "Toggle the use of custom time stamp formats."
  (interactive)
  (setq org-display-custom-times (not org-display-custom-times))
  (unless org-display-custom-times
    (let ((p (point-min)) (bmp (buffer-modified-p)))
      (while (setq p (next-single-property-change p 'display))
	(if (and (get-text-property p 'display)
		 (eq (get-text-property p 'face) 'org-date))
	    (remove-text-properties
	     p (setq p (next-single-property-change p 'display))
	     '(display t))))
      (set-buffer-modified-p bmp)))
  (if (featurep 'xemacs)
      (remove-text-properties (point-min) (point-max) '(end-glyph t)))
  (org-restart-font-lock)
  (setq org-table-may-need-update t)
  (if org-display-custom-times
      (message "Time stamps are overlayed with custom format")
    (message "Time stamp overlays removed")))

(defun org-display-custom-time (beg end)
  "Overlay modified time stamp format over timestamp between BED and END."
  (let* ((ts (buffer-substring beg end))
	 t1 w1 with-hm tf time str w2 (off 0))
    (save-match-data
      (setq t1 (org-parse-time-string ts t))
      (if (string-match "\\(-[0-9]+:[0-9]+\\)?\\( \\+[0-9]+[dwmy]\\)?\\'" ts)
	  (setq off (- (match-end 0) (match-beginning 0)))))
    (setq end (- end off))
    (setq w1 (- end beg)
	  with-hm (and (nth 1 t1) (nth 2 t1))
	  tf (funcall (if with-hm 'cdr 'car) org-time-stamp-custom-formats)
	  time (org-fix-decoded-time t1)
	  str (org-add-props
		  (format-time-string
		   (substring tf 1 -1) (apply 'encode-time time))
		  nil 'mouse-face 'highlight)
	  w2 (length str))
    (if (not (= w2 w1))
	(add-text-properties (1+ beg) (+ 2 beg)
			     (list 'org-dwidth t 'org-dwidth-n (- w1 w2))))
    (if (featurep 'xemacs)
	(progn
	  (put-text-property beg end 'invisible t)
	  (put-text-property beg end 'end-glyph (make-glyph str)))
      (put-text-property beg end 'display str))))

(defun org-translate-time (string)
  "Translate all timestamps in STRING to custom format.
But do this only if the variable `org-display-custom-times' is set."
  (when org-display-custom-times
    (save-match-data
      (let* ((start 0)
	     (re org-ts-regexp-both)
	     t1 with-hm inactive tf time str beg end)
	(while (setq start (string-match re string start))
	  (setq beg (match-beginning 0)
		end (match-end 0)
		t1 (save-match-data
		     (org-parse-time-string (substring string beg end) t))
		with-hm (and (nth 1 t1) (nth 2 t1))
		inactive (equal (substring string beg (1+ beg)) "[")
		tf (funcall (if with-hm 'cdr 'car)
			    org-time-stamp-custom-formats)
		time (org-fix-decoded-time t1)
		str (format-time-string
		     (concat
		      (if inactive "[" "<") (substring tf 1 -1)
		      (if inactive "]" ">"))
		     (apply 'encode-time time))
		string (replace-match str t t string)
		start (+ start (length str)))))))
  string)

(defun org-fix-decoded-time (time)
  "Set 0 instead of nil for the first 6 elements of time.
Don't touch the rest."
  (let ((n 0))
    (mapcar (lambda (x) (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))

(defun org-days-to-time (timestamp-string)
  "Difference between TIMESTAMP-STRING and now in days."
  (- (time-to-days (org-time-string-to-time timestamp-string))
     (time-to-days (current-time))))

(defun org-deadline-close (timestamp-string &optional ndays)
  "Is the time in TIMESTAMP-STRING close to the current date?"
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (< (org-days-to-time timestamp-string) ndays)
       (not (org-entry-is-done-p))))

(defun org-get-wdays (ts)
  "Get the deadline lead time appropriate for timestring TS."
  (cond
   ((<= org-deadline-warning-days 0)
    ;; 0 or negative, enforce this value no matter what
    (- org-deadline-warning-days))
   ((string-match "-\\([0-9]+\\)\\([dwmy]\\)\\(\\'\\|>\\)" ts)
    ;; lead time is specified.
    (floor (* (string-to-number (match-string 1 ts))
	      (cdr (assoc (match-string 2 ts)
			  '(("d" . 1)    ("w" . 7)
			    ("m" . 30.4) ("y" . 365.25)))))))
   ;; go for the default.
   (t org-deadline-warning-days)))

(defun org-calendar-select-mouse (ev)
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive "e")
  (mouse-set-point ev)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
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
	   (t (abs org-deadline-warning-days))))
	 (case-fold-search nil)
	 (regexp (concat "\\<" org-deadline-string " *<\\([^>]+\\)>"))
	 (callback
	  (lambda () (org-deadline-close (match-string 1) org-warn-days))))

    (message "%d deadlines past-due or due within %d days"
	     (org-occur regexp nil callback)
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
  (or
   (org-clock-update-time-maybe)
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
       (message "Time difference inserted")))))

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

(defun org-time-string-to-absolute (s &optional daynr)
  "Convert a time stamp to an absolute day number.
If there is a specifyer for a cyclic time stamp, get the closest date to
DAYNR."
  (cond
   ((and daynr (string-match "\\`%%\\((.*)\\)" s))
    (if (org-diary-sexp-entry (match-string 1 s) "" date)
	daynr
      (+ daynr 1000)))
   ((and daynr (string-match "\\+[0-9]+[dwmy]" s))
    (org-closest-date s (if (and (boundp 'daynr) (integerp daynr)) daynr
			  (time-to-days (current-time))) (match-string 0 s)))
   (t (time-to-days (apply 'encode-time (org-parse-time-string s))))))

(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org-mode."
  (let ((hl (check-calendar-holidays date)))
    (if hl (mapconcat 'identity hl "; "))))

(defun org-diary-sexp-entry (sexp entry date)
  "Process a SEXP diary ENTRY for DATE."
  (require 'diary-lib)
  (let ((result (if calendar-debug-sexp
                    (let ((stack-trace-on-error t))
                      (eval (car (read-from-string sexp))))
                  (condition-case nil
                      (eval (car (read-from-string sexp)))
                    (error
                     (beep)
                     (message "Bad sexp at line %d in %s: %s"
			      (org-current-line)
			      (buffer-file-name) sexp)
                     (sleep-for 2))))))
    (cond ((stringp result) result)
	  ((and (consp result)
		(stringp (cdr result))) (cdr result))
	  (result entry)
          (t nil))))

(defun org-diary-to-ical-string (frombuf)
  "FIXME"
  (let* ((tmpdir (if (featurep 'xemacs)
		     (temp-directory)
		   temporary-file-directory))
	 (tmpfile (make-temp-name
		   (expand-file-name "orgics" tmpdir)))
	 buf rtn b e)
    (save-excursion
      (set-buffer frombuf)
      (icalendar-export-region (point-min) (point-max) tmpfile)
      (setq buf (find-buffer-visiting tmpfile))
      (set-buffer buf)
      (goto-char (point-min))
      (if (re-search-forward "^BEGIN:VEVENT" nil t)
	  (setq b (match-beginning 0)))
      (goto-char (point-max))
      (if (re-search-backward "^END:VEVENT" nil t)
	  (setq e (match-end 0)))
      (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
    (kill-buffer buf)
    (kill-buffer frombuf)
    (delete-file tmpfile)
    rtn))

(defun org-closest-date (start current change)
  "Find the date closest to CURRENT that is consistent with START and CHANGE."
  ;; Make the proper lists from the dates
  (catch 'exit
    (let ((a1 '(("d" . day) ("w" . week) ("m" . month) ("y" . year)))
	  dn dw sday cday n1 n2
	  d m y y1 y2 date1 date2 nmonths nm ny m2)

      (setq start (org-date-to-gregorian start)
	    current (org-date-to-gregorian
		     (if org-agenda-repeating-timestamp-show-all
			 current
		       (time-to-days (current-time))))
	    sday (calendar-absolute-from-gregorian start)
	    cday  (calendar-absolute-from-gregorian current))

      (if (<= cday sday) (throw 'exit sday))

      (if (string-match "\\(\\+[0-9]+\\)\\([dwmy]\\)" change)
	  (setq dn (string-to-number (match-string 1 change))
		dw (cdr (assoc (match-string 2 change) a1)))
	(error "Invalid change specifyer: %s" change))
      (if (eq dw 'week) (setq dw 'day dn (* 7 dn)))
      (cond
       ((eq dw 'day)
	(setq n1 (+ sday (* dn (floor (/ (- cday sday) dn))))
	      n2 (+ n1 dn)))
       ((eq dw 'year)
	(setq d (nth 1 start) m (car start) y1 (nth 2 start) y2 (nth 2 current))
	(setq y1 (+ (* (floor (/ (- y2 y1) dn)) dn) y1))
	(setq date1 (list m d y1)
	      n1 (calendar-absolute-from-gregorian date1)
	      date2 (list m d (+ y1 (* (if (< n1 cday) 1 -1) dn)))
	      n2 (calendar-absolute-from-gregorian date2)))
       ((eq dw 'month)
	;; approx number of month between the tow dates
	(setq nmonths (floor (/ (- cday sday) 30.436875)))
	;; How often does dn fit in there?
	(setq d (nth 1 start) m (car start) y (nth 2 start)
	      nm (* dn (max 0 (1- (floor (/ nmonths dn)))))
	      m (+ m nm)
	      ny (floor (/ m 12))
	      y (+ y ny)
	      m (- m (* ny 12)))
	(while (> m 12) (setq m (- m 12) y (1+ y)))
	(setq n1 (calendar-absolute-from-gregorian (list m d y)))
	(setq m2 (+ m dn) y2 y)
	(if (> m2 12) (setq y2 (1+ y2) m2 (- m2 12)))
	(setq n2 (calendar-absolute-from-gregorian (list m2 d y2)))
	(while (< n2 cday)
	  (setq n1 n2 m m2 y y2)
	  (setq m2 (+ m dn) y2 y)
	  (if (> m2 12) (setq y2 (1+ y2) m2 (- m2 12)))
	  (setq n2 (calendar-absolute-from-gregorian (list m2 d y2))))))

      (if org-agenda-repeating-timestamp-show-all
	  (if (> (abs (- cday n1)) (abs (- cday n2))) n2 n1)
	(if (= cday n1) n1 n2)))))

(defun org-date-to-gregorian (date)
  "Turn any specification of DATE into a gregorian date for the calendar."
  (cond ((integerp date) (calendar-gregorian-from-absolute date))
	((and (listp date) (= (length date) 3)) date)
	((stringp date)
	 (setq date (org-parse-time-string date))
	 (list (nth 4 date) (nth 3 date) (nth 5 date)))
	((listp date)
	 (list (nth 4 date) (nth 3 date) (nth 5 date)))))

(defun org-parse-time-string (s &optional nodefault)
  "Parse the standard Org-mode time string.
This should be a lot faster than the normal `parse-time-string'.
If time is not given, defaults to 0:00.  However, with optional NODEFAULT,
hour and minute fields will be nil if not given."
  (if (string-match org-ts-regexp0 s)
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
  (if (and (not (org-at-timestamp-p t))
	   (org-on-heading-p))
      (org-todo 'up)
    (org-timestamp-change (prefix-numeric-value arg) 'day)))

(defun org-timestamp-down-day (&optional arg)
  "Decrease the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p t))
	   (org-on-heading-p))
      (org-todo 'down)
    (org-timestamp-change (- (prefix-numeric-value arg)) 'day)))

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun org-at-timestamp-p (&optional inactive-ok)
  "Determine if the cursor is in or at a timestamp."
  (interactive)
  (let* ((tsr (if inactive-ok org-ts-regexp3 org-ts-regexp2))
	 (pos (point))
	 (ans (or (looking-at tsr)
		  (save-excursion
		    (skip-chars-backward "^[<\n\r\t")
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
	       ((and (> pos (or (match-end 8) (match-end 5)))
		     (< pos (match-end 0)))
		(- pos (or (match-end 8) (match-end 5))))
	       (t 'day))))
    ans))

(defun org-timestamp-change (n &optional what)
  "Change the date in the time stamp at point.
The date will be changed by N times WHAT.  WHAT can be `day', `month',
`year', `minute', `second'.  If WHAT is not given, the cursor position
in the timestamp determines what will be changed."
  (let ((pos (point))
	with-hm inactive
	org-ts-what
	extra
	ts time time0)
    (if (not (org-at-timestamp-p t))
	(error "Not at a timestamp"))
    (if (and (not what) (not (eq org-ts-what 'day))
	     org-display-custom-times
	     (get-text-property (point) 'display)
	     (not (get-text-property (1- (point)) 'display)))
	(setq org-ts-what 'day))
    (setq org-ts-what (or what org-ts-what)
	  inactive (= (char-after (match-beginning 0)) ?\[)
	  ts (match-string 0))
    (replace-match "")
    (if (string-match
	 "\\(\\(-[012][0-9]:[0-5][0-9]\\)?\\( [-+][0-9]+[dwmy]\\)*\\)[]>]"
	 ts)
	(setq extra (match-string 1 ts)))
    (if (string-match "^.\\{10\\}.*?[0-9]+:[0-9][0-9]" ts)
	(setq with-hm t))
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
    (when (integerp org-ts-what)
      (setq extra (org-modify-ts-extra extra org-ts-what n)))
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
    (setq org-last-changed-timestamp
	  (org-insert-time-stamp time with-hm inactive nil nil extra))
    (org-clock-update-time-maybe)
    (goto-char pos)
    ;; Try to recenter the calendar window, if any
    (if (and org-calendar-follow-timestamp-change
	     (get-buffer-window "*Calendar*" t)
	     (memq org-ts-what '(day month year)))
	(org-recenter-calendar (time-to-days time)))))

(defun org-modify-ts-extra (s pos n)
  "FIXME"
  (let ((idx '(("d" . 0) ("w" . 1) ("m" . 2) ("y" . 3) ("d" . -1) ("y" . 4)))
	ng h m new)
    (when (string-match "\\(-\\([012][0-9]\\):\\([0-5][0-9]\\)\\)?\\( \\+\\([0-9]+\\)\\([dmwy]\\)\\)?" s)
      (cond
       ((or (org-pos-in-match-range pos 2)
	    (org-pos-in-match-range pos 3))
	(setq m (string-to-number (match-string 3 s))
	      h (string-to-number (match-string 2 s)))
	(if (org-pos-in-match-range pos 2)
	    (setq h (+ h n))
	  (setq m (+ m n)))
	(if (< m 0) (setq m (+ m 60) h (1- h)))
	(if (> m 59) (setq m (- m 60) h (1+ h)))
	(setq h (min 24 (max 0 h)))
	(setq ng 1 new (format "-%02d:%02d" h m)))
       ((org-pos-in-match-range pos 6)
	(setq ng 6 new (car (rassoc (+ n (cdr (assoc (match-string 6 s) idx))) idx))))
       ((org-pos-in-match-range pos 5)
	(setq ng 5 new (format "%d" (max 1 (+ n (string-to-number (match-string 5 s))))))))
	
      (when ng
	(setq s (concat
		 (substring s 0 (match-beginning ng))
		 new
		 (substring s (match-end ng))))))
    s))

(defun org-recenter-calendar (date)
  "If the calendar is visible, recenter it to DATE."
  (let* ((win (selected-window))
	 (cwin (get-buffer-window "*Calendar*" t))
	 (calendar-move-hook nil))
    (when cwin
      (select-window cwin)
      (calendar-goto-date (if (listp date) date
			    (calendar-gregorian-from-absolute date)))
      (select-window win))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used to force the current date."
  (interactive "P")
  (let ((tsr org-ts-regexp) diff
	(calendar-move-hook nil)
	(view-calendar-holidays-initially nil)
	(view-diary-entries-initially nil))
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

;;; The clock for measuring work time.

(defvar org-mode-line-string "")
(put 'org-mode-line-string 'risky-local-variable t)

(defvar org-mode-line-timer nil)
(defvar org-clock-heading "")
(defvar org-clock-start-time "")

(defun org-update-mode-line ()
  (let* ((delta (- (time-to-seconds (current-time))
                   (time-to-seconds org-clock-start-time)))
	 (h (floor delta 3600))
	 (m (floor (- delta (* 3600 h)) 60)))
    (setq org-mode-line-string
	  (propertize (format "-[%d:%02d (%s)]" h m org-clock-heading)
		      'help-echo "Org-mode clock is running"))
    (force-mode-line-update)))

(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")
(defvar org-clock-mode-line-entry nil
  "Information for the modeline about the running clock.")

(defun org-clock-in ()
  "Start the clock on the current item.
If necessary, clock-out of the currently active clock."
  (interactive)
  (org-clock-out t)
  (let (ts)
    (save-excursion
      (org-back-to-heading t)
      (if (looking-at org-todo-line-regexp)
	  (setq org-clock-heading (match-string 3))
	(setq org-clock-heading "???"))
      (setq org-clock-heading (propertize org-clock-heading 'face nil))
      (beginning-of-line 2)
      (while
	  (or (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		   (not (equal (match-string 1) org-clock-string)))
	      (and (looking-at "[ \t]*:PROPERTIES:")
		   (not org-insert-labeled-timestamps-before-properties-drawer)))
	;; Scheduling info, or properties drawer, move one line further
	(beginning-of-line 2)
	(or (bolp) (newline)))
      (insert "\n") (backward-char 1)
      (indent-relative)
      (insert org-clock-string " ")
      (setq org-clock-start-time (current-time))
      (setq ts (org-insert-time-stamp (current-time) 'with-hm 'inactive))
      (move-marker org-clock-marker (point) (buffer-base-buffer))
      (or global-mode-string (setq global-mode-string '("")))
      (or (memq 'org-mode-line-string global-mode-string)
	  (setq global-mode-string
		(append global-mode-string '(org-mode-line-string))))
      (org-update-mode-line)
      (setq org-mode-line-timer (run-with-timer 60 60 'org-update-mode-line))
      (message "Clock started at %s" ts))))

(defun org-clock-out (&optional fail-quietly)
  "Stop the currently running clock.
If there is no running clock, throw an error, unless FAIL-QUIETLY is set."
  (interactive)
  (catch 'exit
  (if (not (marker-buffer org-clock-marker))
      (if fail-quietly (throw 'exit t) (error "No active clock")))
  (let (ts te s h m)
    (save-excursion
      (set-buffer (marker-buffer org-clock-marker))
      (goto-char org-clock-marker)
      (beginning-of-line 1)
      (if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
	       (equal (match-string 1) org-clock-string))
	  (setq ts (match-string 2))
	(if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
      (goto-char (match-end 0))
      (delete-region (point) (point-at-eol))
      (insert "--")
      (setq te (org-insert-time-stamp (current-time) 'with-hm 'inactive))
      (setq s (- (time-to-seconds (apply 'encode-time (org-parse-time-string te)))
		 (time-to-seconds (apply 'encode-time (org-parse-time-string ts))))
	    h (floor (/ s 3600))
	    s (- s (* 3600 h))
	    m (floor (/ s 60))
	    s (- s (* 60 s)))
      (insert " => " (format "%2d:%02d" h m))
      (move-marker org-clock-marker nil)
      (org-add-log-maybe 'clock-out)
      (when org-mode-line-timer
	(cancel-timer org-mode-line-timer)
	(setq org-mode-line-timer nil))
      (setq global-mode-string
	    (delq 'org-mode-line-string global-mode-string))
      (force-mode-line-update)
      (message "Clock stopped at %s after HH:MM = %d:%02d" te h m)))))

(defun org-clock-cancel ()
  "Cancel the running clock be removing the start timestamp."
  (interactive)
  (if (not (marker-buffer org-clock-marker))
      (error "No active clock"))
  (save-excursion
    (set-buffer (marker-buffer org-clock-marker))
    (goto-char org-clock-marker)
    (delete-region (1- (point-at-bol)) (point-at-eol)))
  (message "Clock canceled"))

(defvar org-clock-file-total-minutes nil
  "Holds the file total time in minutes, after a call to `org-clock-sum'.")
  (make-variable-buffer-local 'org-clock-file-total-minutes)

(defun org-clock-sum (&optional tstart tend)
  "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline."
  (interactive)
  (let* ((bmp (buffer-modified-p))
	 (re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		     org-clock-string
		     "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	 (lmax 30)
	 (ltimes (make-vector lmax 0))
	 (t1 0)
	 (level 0)
	 ts te dt
	 time)
    (remove-text-properties (point-min) (point-max) '(:org-clock-minutes t))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward re nil t)
	(cond
	 ((match-end 2)
	  ;; Two time stamps
	  (setq ts (match-string 2)
		te (match-string 3)
		ts (time-to-seconds
		    (apply 'encode-time (org-parse-time-string ts)))
		te (time-to-seconds
		    (apply 'encode-time (org-parse-time-string te)))
		ts (if tstart (max ts tstart) ts)
		te (if tend (min te tend) te)
		dt (- te ts)
		t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1)))
	 ((match-end 4)
	  ;; A naket time
	  (setq t1 (+ t1 (string-to-number (match-string 5))
		      (* 60 (string-to-number (match-string 4))))))
	 (t ;; A headline
	  (setq level (- (match-end 1) (match-beginning 1)))
	  (when (or (> t1 0) (> (aref ltimes level) 0))
	    (loop for l from 0 to level do
		  (aset ltimes l (+ (aref ltimes l) t1)))
	    (setq t1 0 time (aref ltimes level))
	    (loop for l from level to (1- lmax) do
		  (aset ltimes l 0))
	    (goto-char (match-beginning 0))
	    (put-text-property (point) (point-at-eol) :org-clock-minutes time)))))
      (setq org-clock-file-total-minutes (aref ltimes 0)))
    (set-buffer-modified-p bmp)))

(defun org-clock-display (&optional total-only)
  "Show subtree times in the entire buffer.
If TOTAL-ONLY is non-nil, only show the total time for the entire file
in the echo area."
  (interactive)
  (org-remove-clock-overlays)
  (let (time h m p)
    (org-clock-sum)
    (unless total-only
      (save-excursion
	(goto-char (point-min))
	(while (setq p (next-single-property-change (point) :org-clock-minutes))
	  (goto-char p)
	  (when (setq time (get-text-property p :org-clock-minutes))
	    (org-put-clock-overlay time (funcall outline-level))))
	(setq h (/ org-clock-file-total-minutes 60)
	      m (- org-clock-file-total-minutes (* 60 h)))
	;; Arrange to remove the overlays upon next change.
	(when org-remove-highlights-with-change
	  (org-add-hook 'before-change-functions 'org-remove-clock-overlays
			nil 'local))))
    (message "Total file time: %d:%02d (%d hours and %d minutes)" h m h m)))

(defvar org-clock-overlays nil)
(make-variable-buffer-local 'org-clock-overlays)

(defun org-put-clock-overlay (time &optional level)
  "Put an overlays on the current line, displaying TIME.
If LEVEL is given, prefix time with a corresponding number of stars.
This creates a new overlay and stores it in `org-clock-overlays', so that it
will be easy to remove."
  (let* ((c 60) (h (floor (/ time 60))) (m (- time (* 60 h)))
	 (l (if level (org-get-legal-level level 0) 0))
	 (off 0)
	 ov tx)
    (move-to-column c)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (setq ov (org-make-overlay (1- (point)) (point-at-eol))
	  tx (concat (buffer-substring (1- (point)) (point))
		     (make-string (+ off (max 0 (- c (current-column)))) ?.)
		     (org-add-props (format "%s %2d:%02d%s"
					    (make-string l ?*) h m
					    (make-string (- 10 l) ?\ ))
			 '(face secondary-selection))
		     ""))
    (if (not (featurep 'xemacs))
	(org-overlay-put ov 'display tx)
      (org-overlay-put ov 'invisible t)
      (org-overlay-put ov 'end-glyph (make-glyph tx)))
    (push ov org-clock-overlays)))

(defun org-remove-clock-overlays (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc 'org-delete-overlay org-clock-overlays)
    (setq org-clock-overlays nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-remove-clock-overlays 'local))))

(defun org-clock-out-if-current ()
  "Clock out if the current entry contains the running clock.
This is used to stop the clock after a TODO entry is marked DONE,
and is only done if the variable `org-clock-out-when-done' is not nil."
  (when (and org-clock-out-when-done
	     (member state org-done-keywords)
	     (equal (marker-buffer org-clock-marker) (current-buffer))
	     (< (point) org-clock-marker)
	     (> (save-excursion (outline-next-heading) (point))
		org-clock-marker))
    ;; Clock out, but don't accept a logging message for this.
    (let ((org-log-done (if (and (listp org-log-done)
				 (member 'clock-out org-log-done))
			    '(done)
			  org-log-done)))
      (org-clock-out))))

(add-hook 'org-after-todo-state-change-hook
	  'org-clock-out-if-current)

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

(defun org-clock-report ()
  "Create a table containing a report about clocked time.
If the buffer contains lines
#+BEGIN: clocktable :maxlevel 3 :emphasize nil

#+END: clocktable
then the table will be inserted between these lines, replacing whatever
is was there before.  If these lines are not in the buffer, the table
is inserted at point, surrounded by the special lines.
The BEGIN line can contain parameters.  Allowed are:
:maxlevel   The maximum level to be included in the table.  Default is 3.
:emphasize  t/nil, if levell 1 and level 2 should be bold/italic in the table."
  (interactive)
  (org-remove-clock-overlays)
  (unless (org-find-dblock "clocktable")
    (org-create-dblock (list :name "clocktable"
			     :maxlevel 2 :emphasize nil)))
  (org-update-dblock))

(defun org-clock-update-time-maybe ()
  "If this is a CLOCK line, update it and return t.
Otherwise, return nil."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?"))
	    ts te h m s)
	(if (not (looking-at re))
	    nil
	  (and (match-end 3) (delete-region (match-beginning 3) (match-end 3)))
	  (end-of-line 1)
	  (setq ts (match-string 1)
		te (match-string 2))
	  (setq s (- (time-to-seconds
		      (apply 'encode-time (org-parse-time-string te)))
		     (time-to-seconds
		      (apply 'encode-time (org-parse-time-string ts))))
		h (floor (/ s 3600))
		s (- s (* 3600 h))
		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format "%2d:%02d" h m))
	  t)))))

(defun org-clock-special-range (key &optional time as-strings)
  "Return two times bordering a special time range.
Key is a symbol specifying the range and can be one of `today', `yesterday',
`thisweek', `lastweek', `thismonth', `lastmonth', `thisyear', `lastyear'.
A week starts Monday 0:00 and ends Sunday 24:00.
The range is determined relative to TIME.  TIME defaults to the current time.
The return value is a cons cell with two internal times like the ones
returned by `current time' or `encode-time'. if AS-STRINGS is non-nil,
the returned times will be formatted strings."
  (let* ((tm (decode-time (or time (current-time))))
	 (s 0) (m (nth 1 tm)) (h (nth 2 tm))
	 (d (nth 3 tm)) (month (nth 4 tm)) (y (nth 5 tm))
	 (dow (nth 6 tm))
	 s1 m1 h1 d1 month1 y1 diff ts te fm)
    (cond
     ((eq key 'today)
      (setq h 0 m 0 h1 24 m1 0))
     ((eq key 'yesterday)
      (setq d (1- d) h 0 m 0 h1 24 m1 0))
     ((eq key 'thisweek)
      (setq diff (if (= dow 0) 6 (1- dow))
	    m 0 h 0 d (- d diff) d1 (+ 7 d)))
     ((eq key 'lastweek)
      (setq diff (+ 7 (if (= dow 0) 6 (1- dow)))
	    m 0 h 0 d (- d diff) d1 (+ 7 d)))
     ((eq key 'thismonth)
      (setq d 1 h 0 m 0 d1 1 month1 (1+ month) h1 0 m1 0))
     ((eq key 'lastmonth)
      (setq d 1 h 0 m 0 d1 1 month (1- month) month1 (1+ month) h1 0 m1 0))
     ((eq key 'thisyear)
      (setq m 0 h 0 d 1 month 1 y1 (1+ y)))
     ((eq key 'lastyear)
      (setq m 0 h 0 d 1 month 1 y (1- y) y1 (1+ y)))
     (t (error "No such time block %s" key)))
    (setq ts (encode-time s m h d month y)
	  te (encode-time (or s1 s) (or m1 m) (or h1 h)
			  (or d1 d) (or month1 month) (or y1 y)))
    (setq fm (cdr org-time-stamp-formats))
    (if as-strings
	(cons (format-time-string fm ts) (format-time-string fm te))
      (cons ts te))))

(defun org-dblock-write:clocktable (params)
  "Write the standard clocktable."
  (let ((hlchars '((1 . "*") (2 . ?/)))
	(emph nil)
	(ins (make-marker))
	ipos time h m p level hlc hdl maxlevel
	ts te cc block)
    (setq maxlevel (or (plist-get params :maxlevel) 3)
	  emph (plist-get params :emphasize)
	  ts (plist-get params :tstart)
	  te (plist-get params :tend)
	  block (plist-get params :block))
    (when block
      (setq cc (org-clock-special-range block nil t)
	    ts (car cc) te (cdr cc)))
    (if ts (setq ts (time-to-seconds
		     (apply 'encode-time (org-parse-time-string ts)))))
    (if te (setq te (time-to-seconds
		     (apply 'encode-time (org-parse-time-string te)))))
    (move-marker ins (point))
    (setq ipos (point))
    (insert-before-markers "Clock summary at ["
			   (substring
			    (format-time-string (cdr org-time-stamp-formats))
			    1 -1)
			   "]."
			   (if block
			       (format "  Considered range is /%s/." block)
			     "")
			   "\n\n|L|Headline|Time|\n")
    (org-clock-sum ts te)
    (setq h (/ org-clock-file-total-minutes 60)
	  m (- org-clock-file-total-minutes (* 60 h)))
    (insert-before-markers "|-\n|0|" "*Total file time*| "
			   (format "*%d:%02d*" h m)
			   "|\n")
    (goto-char (point-min))
    (while (setq p (next-single-property-change (point) :org-clock-minutes))
      (goto-char p)
      (when (setq time (get-text-property p :org-clock-minutes))
	(save-excursion
	  (beginning-of-line 1)
	  (when (and (looking-at (org-re "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@:]+:\\)?[ \t]*$"))
		     (setq level (- (match-end 1) (match-beginning 1)))
		     (<= level maxlevel))
	    (setq hlc (if emph (or (cdr (assoc level hlchars)) "") "")
		  hdl (match-string 2)
		  h (/ time 60)
		  m (- time (* 60 h)))
	    (goto-char ins)
	    (if (= level 1) (insert-before-markers "|-\n"))
	    (insert-before-markers
	     "| " (int-to-string level) "|" hlc hdl hlc " |"
	     (make-string (1- level) ?|)
	     hlc
	     (format "%d:%02d" h m)
	     hlc
	     " |\n")))))
    (goto-char ins)
    (backward-delete-char 1)
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)))

;; FIXME: I don't think anybody uses this, ask David
(defun org-collect-clock-time-entries ()
  "Return an internal list with clocking information.
This list has one entry for each CLOCK interval.
FIXME: describe the elements."
  (interactive)
  (let ((re (concat "^[ \t]*" org-clock-string
		    " *\\[\\(.*?\\)\\]--\\[\\(.*?\\)\\]"))
	rtn beg end next cont level title total closedp leafp
	clockpos titlepos h m donep)
    (save-excursion
      (org-clock-sum)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(setq clockpos (match-beginning 0)
	      beg (match-string 1) end (match-string 2)
	      cont (match-end 0))
	(setq beg (apply 'encode-time (org-parse-time-string beg))
	      end (apply 'encode-time (org-parse-time-string end)))
	(org-back-to-heading t)
	(setq donep (org-entry-is-done-p))
	(setq titlepos (point)
	      total (or (get-text-property (1+ (point)) :org-clock-minutes) 0)
	      h (/ total 60) m (- total (* 60 h))
	      total (cons h m))
	(looking-at "\\(\\*+\\) +\\(.*\\)")
	(setq level (- (match-end 1) (match-beginning 1))
	      title (org-match-string-no-properties 2))
	(save-excursion (outline-next-heading) (setq next (point)))
	(setq closedp (re-search-forward org-closed-time-regexp next t))
	(goto-char next)
	(setq leafp (and (looking-at "^\\*+ ")
			 (<= (- (match-end 0) (point)) level)))
	(push (list beg end clockpos closedp donep
		    total title titlepos level leafp)
	      rtn)
	(goto-char cont)))
    (nreverse rtn)))

;;;; Agenda, and Diary Integration

;;; Define the Org-agenda-mode

(defvar org-agenda-mode-map (make-sparse-keymap)
  "Keymap for `org-agenda-mode'.")

(defvar org-agenda-menu) ; defined later in this file.
(defvar org-agenda-follow-mode nil)
(defvar org-agenda-show-log nil)
(defvar org-agenda-redo-command nil)
(defvar org-agenda-mode-hook nil)
(defvar org-agenda-type nil)
(defvar org-agenda-force-single-file nil)

(defun org-agenda-mode ()
  "Mode for time-sorted view on action items in Org-mode files.

The following commands are available:

\\{org-agenda-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq org-agenda-undo-list nil
	org-agenda-pending-undo-list nil)
  (setq major-mode 'org-agenda-mode)
  ;; Keep global-font-lock-mode from turning on font-lock-mode
  (org-set-local 'font-lock-global-modes (list 'not major-mode))
  (setq mode-name "Org-Agenda")
  (use-local-map org-agenda-mode-map)
  (easy-menu-add org-agenda-menu)
  (if org-startup-truncated (setq truncate-lines t))
  (org-add-hook 'post-command-hook 'org-agenda-post-command-hook nil 'local)
  (org-add-hook 'pre-command-hook 'org-unhighlight nil 'local)
  ;; Make sure properties are removed when copying text
  (when (boundp 'buffer-substring-filters)
    (org-set-local 'buffer-substring-filters
		   (cons (lambda (x)
                           (set-text-properties 0 (length x) nil x) x)
			 buffer-substring-filters)))
  (unless org-agenda-keep-modes
    (setq org-agenda-follow-mode org-agenda-start-with-follow-mode
	  org-agenda-show-log nil))
  (easy-menu-change
   '("Agenda") "Agenda Files"
   (append
    (list
     (vector
      (if (get 'org-agenda-files 'org-restrict)
	  "Restricted to single file"
	"Edit File List")
      '(org-edit-agenda-file-list)
      (not (get 'org-agenda-files 'org-restrict)))
     "--")
    (mapcar 'org-file-menu-entry (org-agenda-files))))
  (org-agenda-set-mode-name)
  (apply
   (if (fboundp 'run-mode-hooks) 'run-mode-hooks 'run-hooks)
   (list 'org-agenda-mode-hook)))

(substitute-key-definition 'undo 'org-agenda-undo
			   org-agenda-mode-map global-map)
(org-defkey org-agenda-mode-map "\C-i"     'org-agenda-goto)
(org-defkey org-agenda-mode-map [(tab)]    'org-agenda-goto)
(org-defkey org-agenda-mode-map "\C-m"     'org-agenda-switch-to)
(org-defkey org-agenda-mode-map "\C-k"     'org-agenda-kill)
(org-defkey org-agenda-mode-map "\C-c$"    'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-s" 'org-agenda-archive)
(org-defkey org-agenda-mode-map "$"        'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c\C-o" 'org-agenda-open-link)
(org-defkey org-agenda-mode-map " "        'org-agenda-show)
(org-defkey org-agenda-mode-map "\C-c\C-t" 'org-agenda-todo)
(org-defkey org-agenda-mode-map [(control shift right)] 'org-agenda-todo-nextset)
(org-defkey org-agenda-mode-map [(control shift left)]  'org-agenda-todo-previousset)
(org-defkey org-agenda-mode-map "\C-c\C-xb" 'org-agenda-tree-to-indirect-buffer)
(org-defkey org-agenda-mode-map "b"        'org-agenda-tree-to-indirect-buffer)
(org-defkey org-agenda-mode-map "o"        'delete-other-windows)
(org-defkey org-agenda-mode-map "L"        'org-agenda-recenter)
(org-defkey org-agenda-mode-map "t"        'org-agenda-todo)
(org-defkey org-agenda-mode-map "a"        'org-agenda-toggle-archive-tag)
(org-defkey org-agenda-mode-map ":"        'org-agenda-set-tags)
(org-defkey org-agenda-mode-map "."        'org-agenda-goto-today)
(org-defkey org-agenda-mode-map "j"        'org-agenda-goto-date)
(org-defkey org-agenda-mode-map "d"        'org-agenda-day-view)
(org-defkey org-agenda-mode-map "w"        'org-agenda-week-view)
(org-defkey org-agenda-mode-map "m"        'org-agenda-month-view)
(org-defkey org-agenda-mode-map "y"        'org-agenda-year-view)
(org-defkey org-agenda-mode-map [(shift right)] 'org-agenda-date-later)
(org-defkey org-agenda-mode-map [(shift left)] 'org-agenda-date-earlier)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (right)] 'org-agenda-date-later)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (left)] 'org-agenda-date-earlier)

(org-defkey org-agenda-mode-map ">" 'org-agenda-date-prompt)
(org-defkey org-agenda-mode-map "\C-c\C-s" 'org-agenda-schedule)
(org-defkey org-agenda-mode-map "\C-c\C-d" 'org-agenda-deadline)
(let ((l '(1 2 3 4 5 6 7 8 9 0)))
  (while l (org-defkey org-agenda-mode-map
	     (int-to-string (pop l)) 'digit-argument)))

(org-defkey org-agenda-mode-map "f" 'org-agenda-follow-mode)
(org-defkey org-agenda-mode-map "l" 'org-agenda-log-mode)
(org-defkey org-agenda-mode-map "D" 'org-agenda-toggle-diary)
(org-defkey org-agenda-mode-map "g" 'org-agenda-toggle-time-grid)
(org-defkey org-agenda-mode-map "r" 'org-agenda-redo)
(org-defkey org-agenda-mode-map "q" 'org-agenda-quit)
(org-defkey org-agenda-mode-map "x" 'org-agenda-exit)
(org-defkey org-agenda-mode-map "\C-x\C-w" 'org-write-agenda)
(org-defkey org-agenda-mode-map "s" 'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "P" 'org-agenda-show-priority)
(org-defkey org-agenda-mode-map "T" 'org-agenda-show-tags)
(org-defkey org-agenda-mode-map "n" 'next-line)
(org-defkey org-agenda-mode-map "p" 'previous-line)
(org-defkey org-agenda-mode-map "\C-n" 'org-agenda-next-date-line)
(org-defkey org-agenda-mode-map "\C-p" 'org-agenda-previous-date-line)
(org-defkey org-agenda-mode-map "," 'org-agenda-priority)
(org-defkey org-agenda-mode-map "\C-c," 'org-agenda-priority)
(org-defkey org-agenda-mode-map "i" 'org-agenda-diary-entry)
(org-defkey org-agenda-mode-map "c" 'org-agenda-goto-calendar)
(eval-after-load "calendar"
  '(org-defkey calendar-mode-map org-calendar-to-agenda-key
     'org-calendar-goto-agenda))
(org-defkey org-agenda-mode-map "C" 'org-agenda-convert-date)
(org-defkey org-agenda-mode-map "M" 'org-agenda-phases-of-moon)
(org-defkey org-agenda-mode-map "S" 'org-agenda-sunrise-sunset)
(org-defkey org-agenda-mode-map "h" 'org-agenda-holidays)
(org-defkey org-agenda-mode-map "H" 'org-agenda-holidays)
(org-defkey org-agenda-mode-map "I" 'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "O" 'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "X" 'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "+" 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map "-" 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [(shift up)] 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [(shift down)] 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (up)] 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (down)] 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [(right)] 'org-agenda-later)
(org-defkey org-agenda-mode-map [(left)] 'org-agenda-earlier)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-c" 'org-agenda-columns)

(defvar org-agenda-keymap (copy-keymap org-agenda-mode-map)
  "Local keymap for agenda entries from Org-mode.")

(org-defkey org-agenda-keymap
  (if (featurep 'xemacs) [(button2)] [(mouse-2)]) 'org-agenda-goto-mouse)
(org-defkey org-agenda-keymap
  (if (featurep 'xemacs) [(button3)] [(mouse-3)]) 'org-agenda-show-mouse)
(when org-agenda-mouse-1-follows-link
  (org-defkey org-agenda-keymap [follow-link] 'mouse-face))
(easy-menu-define org-agenda-menu org-agenda-mode-map "Agenda menu"
  '("Agenda"
    ("Agenda Files")
    "--"
    ["Show" org-agenda-show t]
    ["Go To (other window)" org-agenda-goto t]
    ["Go To (this window)" org-agenda-switch-to t]
    ["Follow Mode" org-agenda-follow-mode
     :style toggle :selected org-agenda-follow-mode :active t]
    ["Tree to indirect frame" org-agenda-tree-to-indirect-buffer t]
    "--"
    ["Cycle TODO" org-agenda-todo t]
    ["Archive subtree" org-agenda-archive t]
    ["Delete subtree" org-agenda-kill t]
    "--"
    ["Goto Today" org-agenda-goto-today (org-agenda-check-type nil 'agenda 'timeline)]
    ["Next Dates" org-agenda-later (org-agenda-check-type nil 'agenda)]
    ["Previous Dates" org-agenda-earlier (org-agenda-check-type nil 'agenda)]
    ["Jump to date" org-agenda-goto-date (org-agenda-check-type nil 'agenda)]
    "--"
    ("Tags and Properties"
     ["Show all Tags" org-agenda-show-tags t]
     ["Set Tags current line" org-agenda-set-tags (not (org-region-active-p))]
     ["Change tag in region" org-agenda-set-tags (org-region-active-p)]
     "--"
     ["Column View" org-columns t])
    ("Date/Schedule"
     ["Schedule" org-agenda-schedule t]
     ["Set Deadline" org-agenda-deadline t]
     "--"
     ["Change Date +1 day" org-agenda-date-later (org-agenda-check-type nil 'agenda 'timeline)]
     ["Change Date -1 day" org-agenda-date-earlier (org-agenda-check-type nil 'agenda 'timeline)]
     ["Change Date to ..." org-agenda-date-prompt (org-agenda-check-type nil 'agenda 'timeline)])
    ("Priority"
     ["Set Priority" org-agenda-priority t]
     ["Increase Priority" org-agenda-priority-up t]
     ["Decrease Priority" org-agenda-priority-down t]
     ["Show Priority" org-agenda-show-priority t])
    ("Calendar/Diary"
     ["New Diary Entry" org-agenda-diary-entry (org-agenda-check-type nil 'agenda 'timeline)]
     ["Goto Calendar" org-agenda-goto-calendar (org-agenda-check-type nil 'agenda 'timeline)]
     ["Phases of the Moon" org-agenda-phases-of-moon (org-agenda-check-type nil 'agenda 'timeline)]
     ["Sunrise/Sunset" org-agenda-sunrise-sunset (org-agenda-check-type nil 'agenda 'timeline)]
     ["Holidays" org-agenda-holidays (org-agenda-check-type nil 'agenda 'timeline)]
     ["Convert" org-agenda-convert-date (org-agenda-check-type nil 'agenda 'timeline)]
     "--"
     ["Create iCalendar file" org-export-icalendar-combine-agenda-files t])
    "--"
    ("View"
     ["Day View" org-agenda-day-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (equal org-agenda-ndays 1)]
     ["Week View" org-agenda-week-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (equal org-agenda-ndays 7)]
     ["Month View" org-agenda-month-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (member org-agenda-ndays '(28 29 30 31))]
     ["Year View" org-agenda-year-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (member org-agenda-ndays '(365 366))]
     "--"
     ["Show Logbook entries" org-agenda-log-mode
      :style toggle :selected org-agenda-show-log :active (org-agenda-check-type nil 'agenda 'timeline)]
     ["Include Diary" org-agenda-toggle-diary
      :style toggle :selected org-agenda-include-diary :active (org-agenda-check-type nil 'agenda)]
     ["Use Time Grid" org-agenda-toggle-time-grid
      :style toggle :selected org-agenda-use-time-grid :active (org-agenda-check-type nil 'agenda)])
    ["Write view to file" org-write-agenda t]
    ["Rebuild buffer" org-agenda-redo t]
    ["Save all Org-mode Buffers" org-save-all-org-buffers t]
    "--"
    ["Undo Remote Editing" org-agenda-undo org-agenda-undo-list]
    "--"
    ["Quit" org-agenda-quit t]
    ["Exit and Release Buffers" org-agenda-exit t]
    ))

;;; Agenda undo

(defvar org-agenda-allow-remote-undo t
  "Non-nil means, allow remote undo from the agenda buffer.")
(defvar org-agenda-undo-list nil
  "List of undoable operations in the agenda since last refresh.")
(defvar org-agenda-undo-has-started-in nil
  "Buffers that have already seen `undo-start' in the current undo sequence.")
(defvar org-agenda-pending-undo-list nil
  "In a series of undo commands, this is the list of remaning undo items.")

(defmacro org-if-unprotected (&rest body)
  "Execute BODY if there is no `org-protected' text property at point."
  (declare (debug t))
  `(unless (get-text-property (point) 'org-protected)
     ,@body))

(defmacro org-with-remote-undo (_buffer &rest _body)
  "Execute BODY while recording undo information in two buffers."
  (declare (indent 1) (debug t))
  `(let ((_cline (org-current-line))
	 (_cmd this-command)
	 (_buf1 (current-buffer))
	 (_buf2 ,_buffer)
	 (_undo1 buffer-undo-list)
	 (_undo2 (with-current-buffer ,_buffer buffer-undo-list))
	 _c1 _c2)
     ,@_body
     (when org-agenda-allow-remote-undo
       (setq _c1 (org-verify-change-for-undo
		  _undo1 (with-current-buffer _buf1 buffer-undo-list))
	     _c2 (org-verify-change-for-undo
		  _undo2 (with-current-buffer _buf2 buffer-undo-list)))
       (when (or _c1 _c2)
	 ;; make sure there are undo boundaries
	 (and _c1 (with-current-buffer _buf1 (undo-boundary)))
	 (and _c2 (with-current-buffer _buf2 (undo-boundary)))
	 ;; remember which buffer to undo
	 (push (list _cmd _cline _buf1 _c1 _buf2 _c2)
	       org-agenda-undo-list)))))

(defun org-agenda-undo ()
  "Undo a remote editing step in the agenda.
This undoes changes both in the agenda buffer and in the remote buffer
that have been changed along."
  (interactive)
  (or org-agenda-allow-remote-undo
      (error "Check the variable `org-agenda-allow-remote-undo' to activate remote undo."))
  (if (not (eq this-command last-command))
      (setq org-agenda-undo-has-started-in nil
	    org-agenda-pending-undo-list org-agenda-undo-list))
  (if (not org-agenda-pending-undo-list)
      (error "No further undo information"))
  (let* ((entry (pop org-agenda-pending-undo-list))
	 buf line cmd rembuf)
    (setq cmd (pop entry) line (pop entry))
    (setq rembuf (nth 2 entry))
    (org-with-remote-undo rembuf
      (while (bufferp (setq buf (pop entry)))
	(if (pop entry)
	    (with-current-buffer buf
	      (let ((last-undo-buffer buf)
                    (inhibit-read-only t))
		(unless (memq buf org-agenda-undo-has-started-in)
		  (push buf org-agenda-undo-has-started-in)
		  (make-local-variable 'pending-undo-list)
		  (undo-start))
		(while (and pending-undo-list
			    (listp pending-undo-list)
			    (not (car pending-undo-list)))
		  (pop pending-undo-list))
		(undo-more 1))))))
    (goto-line line)
    (message "`%s' undone (buffer %s)" cmd (buffer-name rembuf))))

(defun org-verify-change-for-undo (l1 l2)
  "Verify that a real change occurred between the undo lists L1 and L2."
  (while (and l1 (listp l1) (null (car l1))) (pop l1))
  (while (and l2 (listp l2) (null (car l2))) (pop l2))
  (not (eq l1 l2)))

;;; Agenda dispatch

(defvar org-agenda-restrict nil)
(defvar org-agenda-restrict-begin (make-marker))
(defvar org-agenda-restrict-end (make-marker))
(defvar org-agenda-last-dispatch-buffer nil)

;;;###autoload
(defun org-agenda (arg)
  "Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a character to select a command.  Any prefix arg will be passed
on to the selected command.  The default selections are:
g
a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
L     Create a timeline for the current buffer.
e     Export views to associated files.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org-mode and visiting a file, you can also
first press `1' to indicate that the agenda should be temporarily (until the
next use of \\[org-agenda]) restricted to the current file."
  (interactive "P")
  (catch 'exit
    (let* ((buf (current-buffer))
	   (bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (org-mode-p)))
	   (custom org-agenda-custom-commands)
	   c entry key type match lprops)
      ;; Turn off restriction
      (put 'org-agenda-files 'org-restrict nil)
      (setq org-agenda-restrict nil)
      (move-marker org-agenda-restrict-begin nil)
      (move-marker org-agenda-restrict-end nil)
      ;; Delete old local properties
      (put 'org-agenda-redo-command 'org-lprops nil)
      ;; Remember where this call originated
      (setq org-agenda-last-dispatch-buffer (current-buffer))
      (save-window-excursion
	(delete-other-windows)
	(org-switch-to-buffer-other-window " *Agenda Commands*")
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header
"Press key for an agenda command:
--------------------------------         C   Configure custom agenda commands
a   Agenda for current week or day       e   Export agenda views
t   List of all TODO entries             T   Entries with special TODO kwd
m   Match a TAGS query                   M   Like m, but only TODO entries
L   Timeline for current buffer          #   List stuck projects (!=configure)
")
			(start 0))
		    (while (string-match "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)" header start)
		      (setq start (match-end 0))
		      (add-text-properties (match-beginning 2) (match-end 2)
					   '(face bold) header))
		    header)))
	(while (setq entry (pop custom))
	  (setq key (car entry) type (nth 1 entry) match (nth 2 entry))
	  (insert (format "\n%-4s%-14s: %s"
			  (org-add-props (copy-sequence key)
			      '(face bold))
			  (cond
			   ((stringp type) type)
			   ((eq type 'agenda) "Agenda for current week or day")
			   ((eq type 'alltodo) "List of all TODO entries")
			   ((eq type 'stuck) "List of stuck projects")
			   ((eq type 'todo) "TODO keyword")
			   ((eq type 'tags) "Tags query")
			   ((eq type 'tags-todo) "Tags (TODO)")
			   ((eq type 'tags-tree) "Tags tree")
			   ((eq type 'todo-tree) "TODO kwd tree")
			   ((eq type 'occur-tree) "Occur tree")
			   ((functionp type) (symbol-name type))
			   (t "???"))
			  (if (stringp match)
			      (org-add-props match nil 'face 'org-warning)
			    (format "set of %d commands" (length match))))))
	(if restrict-ok
	    (insert "\n"
		    (org-add-props "1   Restrict call to current buffer      0   Restrict call to region or subtree" nil 'face 'org-table)))
	(goto-char (point-min))
	(if (fboundp 'fit-window-to-buffer) (fit-window-to-buffer))
	(message "Press key for agenda command%s"
		 (if restrict-ok ", or [1] or [0] to restrict" ""))
	(setq c (read-char-exclusive))
	(message "")
	(when (memq c '(?L ?1 ?0))
	  (if restrict-ok
	      (put 'org-agenda-files 'org-restrict (list bfn))
	    (error "Cannot restrict agenda to current buffer"))
	  (with-current-buffer " *Agenda Commands*"
	    (goto-char (point-max))
	    (delete-region (point-at-bol) (point))
	    (goto-char (point-min)))
	  (when (eq c ?0)
	    (setq org-agenda-restrict t)
	    (with-current-buffer buf
	      (if (org-region-active-p)
		  (progn
		    (move-marker org-agenda-restrict-begin (region-beginning))
		    (move-marker org-agenda-restrict-end (region-end)))
		(save-excursion
		  (org-back-to-heading t)
		  (move-marker org-agenda-restrict-begin (point))
		  (move-marker org-agenda-restrict-end
			       (progn (org-end-of-subtree t)))))))
	  (unless (eq c ?L)
	    (message "Press key for agenda command%s"
		     (if restrict-ok " (restricted to current file)" ""))
	    (setq c (read-char-exclusive)))
	  (message "")))
      (require 'calendar)  ; FIXME: can we avoid this for some commands?
      ;; For example the todo list should not need it (but does...)
      (cond
       ((setq entry (assoc (char-to-string c) org-agenda-custom-commands))
	(if (symbolp (nth 1 entry))
	    (progn
	      (setq type (nth 1 entry) match (nth 2 entry) lprops (nth 3 entry)
		    lprops (nth 3 entry))
	      (put 'org-agenda-redo-command 'org-lprops lprops)
	      (cond
	       ((eq type 'agenda)
		(org-let lprops '(org-agenda-list current-prefix-arg)))
	       ((eq type 'alltodo)
		(org-let lprops '(org-todo-list current-prefix-arg)))
	       ((eq type 'stuck)
		(org-let lprops '(org-agenda-list-stuck-projects
				  current-prefix-arg)))
	       ((eq type 'tags)
		(org-let lprops '(org-tags-view current-prefix-arg match)))
	       ((eq type 'tags-todo)
		(org-let lprops '(org-tags-view '(4) match)))
	       ((eq type 'todo)
		(org-let lprops '(org-todo-list match)))
	       ((eq type 'tags-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-tags-sparse-tree current-prefix-arg match)))
	       ((eq type 'todo-tree)
		(org-check-for-org-mode)
		(org-let lprops
		  '(org-occur (concat "^" outline-regexp "[ \t]*"
				      (regexp-quote match) "\\>"))))
	       ((eq type 'occur-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-occur match)))
	       ((fboundp type)
		(org-let lprops '(funcall type match)))
	       (t (error "Invalid custom agenda command type %s" type))))
	  (org-run-agenda-series (nth 1 entry) (cddr entry))))
       ((equal c ?C) (customize-variable 'org-agenda-custom-commands))
       ((equal c ?a) (call-interactively 'org-agenda-list))
       ((equal c ?t) (call-interactively 'org-todo-list))
       ((equal c ?T) (org-call-with-arg 'org-todo-list (or arg '(4))))
       ((equal c ?m) (call-interactively 'org-tags-view))
       ((equal c ?M) (org-call-with-arg 'org-tags-view (or arg '(4))))
       ((equal c ?e) (call-interactively 'org-store-agenda-views))
       ((equal c ?L)
	(unless restrict-ok
	  (error "This is not an Org-mode file"))
	(org-call-with-arg 'org-timeline arg))
       ((equal c ?#) (call-interactively 'org-agenda-list-stuck-projects))
       ((equal c ?!) (customize-variable 'org-stuck-projects))
       (t (error "Invalid key"))))))

(defun org-run-agenda-series (name series)
  (org-prepare-agenda name)
  (let* ((org-agenda-multi t)
	 (redo (list 'org-run-agenda-series name (list 'quote series)))
	 (cmds (car series))
	 (gprops (nth 1 series))
	 match ;; The byte compiler incorrectly complains about this.  Keep it!
	 cmd type lprops)
    (while (setq cmd (pop cmds))
      (setq type (car cmd) match (nth 1 cmd) lprops (nth 2 cmd))
      (cond
       ((eq type 'agenda)
	(org-let2 gprops lprops
	  '(call-interactively 'org-agenda-list)))
       ((eq type 'alltodo)
	(org-let2 gprops lprops
	  '(call-interactively 'org-todo-list)))
       ((eq type 'stuck)
	(org-let2 gprops lprops
	  '(call-interactively 'org-agenda-list-stuck-projects)))
       ((eq type 'tags)
	(org-let2 gprops lprops
		  '(org-tags-view current-prefix-arg match)))
       ((eq type 'tags-todo)
	(org-let2 gprops lprops
		  '(org-tags-view '(4) match)))
       ((eq type 'todo)
	(org-let2 gprops lprops
		  '(org-todo-list match)))
       ((fboundp type)
	(org-let2 gprops lprops
	  '(funcall type match)))
       (t (error "Invalid type in command series"))))
    (widen)
    (setq org-agenda-redo-command redo)
    (goto-char (point-min)))
  (org-finalize-agenda))

;;;###autoload
(defmacro org-batch-agenda (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string is is used as a tags/todo match string.
Paramters are alternating variable names and values that will be bound
before running the agenda command."
  (let (pars)
    (while parameters
      (push (list (pop parameters) (if parameters (pop parameters))) pars))
    (if (> (length cmd-key) 1)
	(eval (list 'let (nreverse pars)
		    (list 'org-tags-view nil cmd-key)))
      (flet ((read-char-exclusive () (string-to-char cmd-key)))
	(eval (list 'let (nreverse pars) '(org-agenda nil)))))
    (set-buffer org-agenda-buffer-name)
    (princ (org-encode-for-stdout (buffer-string)))))

(defun org-encode-for-stdout (string)
  (if (fboundp 'encode-coding-string)
      (encode-coding-string string buffer-file-coding-system)
    string))

(defvar org-agenda-info nil)

;;;###autoload
(defmacro org-batch-agenda-csv (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string is is used as a tags/todo match string.
Paramters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        Sting with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed"

  (let (pars)
    (while parameters
      (push (list (pop parameters) (if parameters (pop parameters))) pars))
    (push (list 'org-agenda-remove-tags t) pars)
    (if (> (length cmd-key) 1)
	(eval (list 'let (nreverse pars)
		    (list 'org-tags-view nil cmd-key)))
      (flet ((read-char-exclusive () (string-to-char cmd-key)))
	(eval (list 'let (nreverse pars) '(org-agenda nil)))))
    (set-buffer org-agenda-buffer-name)
    (let* ((lines (org-split-string (buffer-string) "\n"))
	   line)
      (while (setq line (pop lines))
	(catch 'next
	  (if (not (get-text-property 0 'org-category line)) (throw 'next nil))
	  (setq org-agenda-info
		(org-fix-agenda-info (text-properties-at 0 line)))
	  (princ
	   (org-encode-for-stdout
	    (mapconcat 'org-agenda-export-csv-mapper
		       '(org-category txt type todo tags date time-of-day extra
				      priority-letter priority agenda-day)
		      ",")))
	  (princ "\n"))))))

(defun org-fix-agenda-info (props)
  "FIXME"
  (let (tmp re)
    (when (setq tmp (plist-get props 'tags))
      (setq props (plist-put props 'tags (mapconcat 'identity tmp ":"))))
    (when (setq tmp (plist-get props 'date))
      (if (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form '(year "-" month "-" day)))
	'((format "%4d, %9s %2s, %4s" dayname monthname day year))

	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'date tmp)))
    (when (setq tmp (plist-get props 'day))
      (if (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form '(year "-" month "-" day)))
	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'day tmp))
      (setq props (plist-put props 'agenda-day tmp)))
    (when (setq tmp (plist-get props 'txt))
      (when (string-match "\\[#\\([A-Z0-9]\\)\\] ?" tmp)
	(plist-put props 'priority-letter (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (when (and (setq re (plist-get props 'org-todo-regexp))
		 (setq re (concat "\\`\\.*" re " ?"))
		 (string-match re tmp))
	(plist-put props 'todo (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (plist-put props 'txt tmp)))
  props)

(defun org-agenda-export-csv-mapper (prop)
  (let ((res (plist-get org-agenda-info prop)))
    (setq res
	  (cond
	   ((not res) "")
	   ((stringp res) res)
	   (t (prin1-to-string res))))
    (while (string-match "," res)
      (setq res (replace-match ";" t t res)))
    (org-trim res)))


;;;###autoload
(defun org-store-agenda-views (&rest parameters)
  (interactive)
  (eval (list 'org-batch-store-agenda-views)))

;; FIXME, why is this a macro?????
;;;###autoload
(defmacro org-batch-store-agenda-views (&rest parameters)
  "Run all custom agenda commands that have a file argument."
  (let ((cmds org-agenda-custom-commands)
	(pop-up-frames nil)
	(dir default-directory)
	pars cmd thiscmdkey files opts)
    (while parameters
      (push (list (pop parameters) (if parameters (pop parameters))) pars))
    (setq pars (reverse pars))
    (save-window-excursion
      (while cmds
	(setq cmd (pop cmds)
	      thiscmdkey (car cmd)
	      opts (nth 3 cmd)
	      files (nth 4 cmd))
	(if (stringp files) (setq files (list files)))
	(when files
	  (flet ((read-char-exclusive () (string-to-char thiscmdkey)))
	    (eval (list 'let (append org-agenda-exporter-settings opts pars)
			'(org-agenda nil))))
	  (set-buffer org-agenda-buffer-name)
	  (while files
	    (eval (list 'let (append org-agenda-exporter-settings opts pars)
			(list 'org-write-agenda
			      (expand-file-name (pop files) dir) t))))
	  (and (get-buffer org-agenda-buffer-name)
	       (kill-buffer org-agenda-buffer-name)))))))

(defun org-write-agenda (file &optional nosettings)
  "Write the current buffer (an agenda view) as a file.
Depending on the extension of the file name, plain text (.txt),
HTML (.html or .htm) or Postscript (.ps) is produced.
If NOSETTINGS is given, do not scope the settings of
`org-agenda-exporter-settings' into the export commands.  This is used when
the settings have already been scoped and we do not wish to overrule other,
higher priority settings."
  (interactive "FWrite agenda to file: ")
  (if (not (file-writable-p file))
      (error "Cannot write agenda to file %s" file))
  (cond
   ((string-match "\\.html?\\'" file) (require 'htmlize))
   ((string-match "\\.ps\\'" file) (require 'ps-print)))
  (org-let (if nosettings nil org-agenda-exporter-settings)
    '(save-excursion
       (save-window-excursion
	 (cond
	  ((string-match "\\.html?\\'" file)
	   (set-buffer (htmlize-buffer (current-buffer)))

	   (when (and org-agenda-export-html-style
		      (string-match "<style>" org-agenda-export-html-style))
	     ;; replace <style> section with org-agenda-export-html-style
	     (goto-char (point-min))
	     (kill-region (- (search-forward "<style") 6)
			  (search-forward "</style>"))
	     (insert org-agenda-export-html-style))
	   (write-file file)
	   (kill-buffer (current-buffer))
	   (message "HTML written to %s" file))
	  ((string-match "\\.ps\\'" file)
	   (ps-print-buffer-with-faces file)
	   (message "Postscript written to %s" file))
	  (t
	   (let ((bs (buffer-string)))
	     (find-file file)
	     (insert bs)
	     (save-buffer 0)
	     (kill-buffer (current-buffer))
	     (message "Plain text written to %s" file))))))
    (set-buffer org-agenda-buffer-name)))

(defmacro org-no-read-only (&rest body)
  "Inhibit read-only for BODY."
  `(let ((inhibit-read-only t)) ,@body))

(defun org-check-for-org-mode ()
  "Make sure current buffer is in org-mode.  Error if not."
  (or (org-mode-p)
      (error "Cannot execute org-mode agenda command on buffer in %s."
	     major-mode)))

(defun org-fit-agenda-window ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (fit-window-to-buffer nil (/ (* (frame-height) 3) 4)
                             (/ (frame-height) 2))))

;;; Agenda file list

(defun org-agenda-files (&optional unrestricted)
  "Get the list of agenda files.
Optional UNRESTRICTED means return the full list even if a restriction
is currently in place."
  (let ((files
	 (cond
	  ((and (not unrestricted) (get 'org-agenda-files 'org-restrict)))
	  ((stringp org-agenda-files) (org-read-agenda-file-list))
	  ((listp org-agenda-files) org-agenda-files)
	  (t (error "Invalid value of `org-agenda-files'")))))
    (if org-agenda-skip-unavailable-files
	(delq nil
	      (mapcar (function
		       (lambda (file)
			 (and (file-readable-p file) file)))
		      files))
      files))) ; `org-check-agenda-file' will remove them from the list

(defun org-edit-agenda-file-list ()
  "Edit the list of agenda files.
Depending on setup, this either uses customize to edit the variable
`org-agenda-files', or it visits the file that is holding the list.  In the
latter case, the buffer is set up in a way that saving it automatically kills
the buffer and restores the previous window configuration."
  (interactive)
  (if (stringp org-agenda-files)
      (let ((cw (current-window-configuration)))
	(find-file org-agenda-files)
	(org-set-local 'org-window-configuration cw)
	(org-add-hook 'after-save-hook
		      (lambda ()
			(set-window-configuration
			 (prog1 org-window-configuration
			   (kill-buffer (current-buffer))))
			(org-install-agenda-files-menu)
			(message "New agenda file list installed"))
		      nil 'local)
	(message (substitute-command-keys
		  "Edit list and finish with \\[save-buffer]")))
    (customize-variable 'org-agenda-files)))

(defun org-store-new-agenda-file-list (list)
  "Set new value for the agenda file list and save it correcly."
  (if (stringp org-agenda-files)
      (let ((f org-agenda-files) b)
	(while (setq b (find-buffer-visiting f)) (kill-buffer b))
	(with-temp-file f
	  (insert (mapconcat 'identity list "\n") "\n")))
    (let ((org-mode-hook nil) (default-major-mode 'fundamental-mode))
      (setq org-agenda-files list)
      (customize-save-variable 'org-agenda-files org-agenda-files))))

(defun org-read-agenda-file-list ()
  "Read the list of agenda files from a file."
  (when (stringp org-agenda-files)
    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (org-split-string (buffer-string) "[ \t\r\n]*?[\r\n][ \t\r\n]*"))))


;;;###autoload
(defun org-cycle-agenda-files ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (org-agenda-files t))
	 (files (append fs (list (car fs))))
	 (tcf (if buffer-file-name (file-truename buffer-file-name)))
	 file)
    (unless files (error "No agenda files"))
    (catch 'exit
      (while (setq file (pop files))
	(if (equal (file-truename file) tcf)
	    (when (car files)
	      (find-file (car files))
	      (throw 'exit t))))
      (find-file (car fs)))
    (if (buffer-base-buffer) (switch-to-buffer (buffer-base-buffer)))))

(defun org-agenda-file-to-front (&optional to-end)
  "Move/add the current file to the top of the agenda file list.
If the file is not present in the list, it is added to the front.  If it is
present, it is moved there.  With optional argument TO-END, add/move to the
end of the list."
  (interactive "P")
  (let ((org-agenda-skip-unavailable-files nil)
	(file-alist (mapcar (lambda (x)
			      (cons (file-truename x) x))
			    (org-agenda-files t)))
	(ctf (file-truename buffer-file-name))
	x had)
    (setq x (assoc ctf file-alist) had x)

    (if (not x) (setq x (cons ctf (abbreviate-file-name buffer-file-name))))
    (if to-end
	(setq file-alist (append (delq x file-alist) (list x)))
      (setq file-alist (cons x (delq x file-alist))))
    (org-store-new-agenda-file-list (mapcar 'cdr file-alist))
    (org-install-agenda-files-menu)
    (message "File %s to %s of agenda file list"
	     (if had "moved" "added") (if to-end "end" "front"))))

(defun org-remove-file (&optional file)
  "Remove current file from the list of files in variable `org-agenda-files'.
These are the files which are being checked for agenda entries.
Optional argument FILE means, use this file instead of the current."
  (interactive)
  (let* ((org-agenda-skip-unavailable-files nil)
	 (file (or file buffer-file-name))
	 (true-file (file-truename file))
	 (afile (abbreviate-file-name file))
	 (files (delq nil (mapcar
			   (lambda (x)
			     (if (equal true-file
					(file-truename x))
				 nil x))
			   (org-agenda-files t)))))
    (if (not (= (length files) (length (org-agenda-files t))))
	(progn
	  (org-store-new-agenda-file-list files)
	  (org-install-agenda-files-menu)
	  (message "Removed file: %s" afile))
      (message "File was not in list: %s" afile))))

(defun org-file-menu-entry (file)
  (vector file (list 'find-file file) t))

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (when (not (file-exists-p file))
    (message "non-existent file %s. [R]emove from list or [A]bort?"
	     (abbreviate-file-name file))
    (let ((r (downcase (read-char-exclusive))))
      (cond
       ((equal r ?r)
	(org-remove-file file)
	(throw 'nextfile t))
       (t (error "Abort"))))))

;;; Agenda prepare and finalize

(defvar org-agenda-multi nil)  ; dynammically scoped
(defvar org-agenda-buffer-name "*Org Agenda*")
(defvar org-pre-agenda-window-conf nil)
(defvar org-agenda-name nil)
(defun org-prepare-agenda (&optional name)
  (setq org-todo-keywords-for-agenda nil)
  (setq org-done-keywords-for-agenda nil)
  (if org-agenda-multi
      (progn
	(setq buffer-read-only nil)
	(goto-char (point-max))
	(unless (bobp)
	  (insert "\n" (make-string (window-width) ?=) "\n"))
	(narrow-to-region (point) (point-max)))
    (org-agenda-maybe-reset-markers 'force)
    (org-prepare-agenda-buffers (org-agenda-files))
    (setq org-todo-keywords-for-agenda
	  (org-uniquify org-todo-keywords-for-agenda))
    (setq org-done-keywords-for-agenda
	  (org-uniquify org-done-keywords-for-agenda))
    (let* ((abuf (get-buffer-create org-agenda-buffer-name))
	   (awin (get-buffer-window abuf)))
      (cond
       ((equal (current-buffer) abuf) nil)
       (awin (select-window awin))
       ((not (setq org-pre-agenda-window-conf (current-window-configuration))))
       ((equal org-agenda-window-setup 'current-window)
	(switch-to-buffer abuf))
       ((equal org-agenda-window-setup 'other-window)
	(org-switch-to-buffer-other-window abuf))
       ((equal org-agenda-window-setup 'other-frame)
	(switch-to-buffer-other-frame abuf))
       ((equal org-agenda-window-setup 'reorganize-frame)
	(delete-other-windows)
	(org-switch-to-buffer-other-window abuf))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-agenda-mode)
    (and name (not org-agenda-name)
	 (org-set-local 'org-agenda-name name)))
  (setq buffer-read-only nil))

(defun org-finalize-agenda ()
  "Finishing touch for the agenda buffer, called just before displaying it."
  (unless org-agenda-multi
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(while (org-activate-bracket-links (point-max))
	  (add-text-properties (match-beginning 0) (match-end 0)
			       '(face org-link)))
	(org-agenda-align-tags)
	(unless org-agenda-with-colors
	  (remove-text-properties (point-min) (point-max) '(face nil))))
      (if (and (boundp 'org-overriding-columns-format)
	       org-overriding-columns-format)
	  (org-set-local 'org-overriding-columns-format
			 org-overriding-columns-format))
      (if (and (boundp 'org-agenda-view-columns-initially)
	       org-agenda-view-columns-initially)
	  (org-agenda-columns))
      (run-hooks 'org-finalize-agenda-hook))))

(defun org-prepare-agenda-buffers (files)
  "Create buffers for all agenda files, protect archived trees and comments."
  (interactive)
  (let ((pa '(:org-archived t))
	(pc '(:org-comment t))
	(pall '(:org-archived t :org-comment t))
	(inhibit-read-only t)
	(rea (concat ":" org-archive-tag ":"))
	     bmp file re)
    (save-excursion
      (save-restriction
	(while (setq file (pop files))
	  (org-check-agenda-file file)
	  (set-buffer (org-get-agenda-file-buffer file))
	  (widen)
	  (setq bmp (buffer-modified-p))
	  (setq org-todo-keywords-for-agenda
		(append org-todo-keywords-for-agenda org-todo-keywords-1))
	  (setq org-done-keywords-for-agenda
		(append org-done-keywords-for-agenda org-done-keywords))
	  (save-excursion
	    (remove-text-properties (point-min) (point-max) pall)
	    (when org-agenda-skip-archived-trees
	      (goto-char (point-min))
	      (while (re-search-forward rea nil t)
		(if (org-on-heading-p t)
		    (add-text-properties (point-at-bol) (org-end-of-subtree t) pa))))
	    (goto-char (point-min))
	    (setq re (concat "^\\*+ +" org-comment-string "\\>"))
	    (while (re-search-forward re nil t)
	      (add-text-properties
	       (match-beginning 0) (org-end-of-subtree t) pc)))
	  (set-buffer-modified-p bmp))))))

(defvar org-agenda-skip-function nil
  "Function to be called at each match during agenda construction.
If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued.
This may also be a Lisp form, it will be evaluated.
Never set this variable using `setq' or so, because then it will apply
to all future agenda commands.  Instead, bind it with `let' to scope
it dynamically into the agenda-constructing command.  A good way to set
it is through options in org-agenda-custom-commands.")

(defun org-agenda-skip ()
  "Throw to `:skip' in places that should be skipped.
Also moves point to the end of the skipped region, so that search can
continue from there."
  (let ((p (point-at-bol)) to fp)
    (and org-agenda-skip-archived-trees
	 (get-text-property p :org-archived)
	 (org-end-of-subtree t)
	 (throw :skip t))
    (and (get-text-property p :org-comment)
	 (org-end-of-subtree t)
	 (throw :skip t))
    (if (equal (char-after p) ?#) (throw :skip t))
    (when (and (or (setq fp (functionp org-agenda-skip-function))
		   (consp org-agenda-skip-function))
	       (setq to (save-excursion
			  (save-match-data
			    (if fp
				(funcall org-agenda-skip-function)
			      (eval org-agenda-skip-function))))))
      (goto-char to)
      (throw :skip t))))

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
  (if (or (and force (not org-agenda-multi))
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
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
	buf ; just return it
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

(defvar org-category-table nil)
(defun org-get-category-table ()
  "Get the table of categories and positions in current buffer."
  (let (tbl)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^#\\+CATEGORY:[ \t]*\\(.*\\)"
				  nil t)
	  (push (cons (match-beginning 1)
		      (org-trim (match-string 1))) tbl))))
    tbl))

(defun org-get-category (&optional pos)
  "Get the category applying to position POS."
  (if (not org-category-table)
      (cond
       ((null org-category)
	(setq org-category
	      (if buffer-file-name
		  (file-name-sans-extension
		   (file-name-nondirectory buffer-file-name))
		"???")))
       ((symbolp org-category) (symbol-name org-category))
       (t org-category))
    (let ((tbl org-category-table)
	  (pos (or pos (point))))
      (while (and tbl (> (caar tbl) pos))
	(pop tbl))
      (or (cdar tbl) (cdr (nth (1- (length org-category-table))
			       org-category-table))))))
;;; Agenda timeline

(defun org-timeline (&optional include-all)
  "Show a time-sorted view of the entries in the current org file.
Only entries with a time stamp of today or later will be listed.  With
\\[universal-argument] prefix, all unfinished TODO items will also be shown,
under the current date.
If the buffer contains an active region, only check the region for
dates."
  (interactive "P")
  (require 'calendar)
  (org-compile-prefix-format 'timeline)
  (org-set-sorting-strategy 'timeline)
  (let* ((dopast t)
	 (dotodo include-all)
	 (doclosed org-agenda-show-log)
	 (entry buffer-file-name)
	 (date (calendar-current-date))
	 (beg (if (org-region-active-p) (region-beginning) (point-min)))
	 (end (if (org-region-active-p) (region-end) (point-max)))
	 (day-numbers (org-get-all-dates beg end 'no-ranges
					 t doclosed ; always include today
					 org-timeline-show-empty-dates))
	 (today (time-to-days (current-time)))
	 (past t)
	 args
	 s e rtn d emptyp)
    (setq org-agenda-redo-command
	  (list 'progn
		(list 'org-switch-to-buffer-other-window (current-buffer))
		(list 'org-timeline (list 'quote include-all))))
    (if (not dopast)
	;; Remove past dates from the list of dates.
	(setq day-numbers (delq nil (mapcar (lambda(x)
					      (if (>= x today) x nil))
					    day-numbers))))
    (org-prepare-agenda (concat "Timeline "
				(file-name-nondirectory buffer-file-name)))
    (if doclosed (push :closed args))
    (push :timestamp args)
    (push :sexp args)
    (if dotodo (push :todo args))
    (while (setq d (pop day-numbers))
      (if (and (listp d) (eq (car d) :omitted))
	  (progn
	    (setq s (point))
	    (insert (format "\n[... %d empty days omitted]\n\n" (cdr d)))
	    (put-text-property s (1- (point)) 'face 'org-agenda-structure))
	(if (listp d) (setq d (car d) emptyp t) (setq emptyp nil))
	(if (and (>= d today)
		 dopast
		 past)
	    (progn
	      (setq past nil)
	      (insert (make-string 79 ?-) "\n")))
	(setq date (calendar-gregorian-from-absolute d))
	(setq s (point))
	(setq rtn (and (not emptyp)
		       (apply 'org-agenda-get-day-entries
			      entry date args)))
	(if (or rtn (equal d today) org-timeline-show-empty-dates)
	    (progn
	      (insert (calendar-day-name date) " "
		      (number-to-string (extract-calendar-day date)) " "
		      (calendar-month-name (extract-calendar-month date)) " "
		      (number-to-string (extract-calendar-year date)) "\n")
; FIXME: this gives a timezone problem
;	      (insert (format-time-string org-agenda-date-format
;					  (calendar-time-from-absolute d 0))
;		      "\n")
	      (put-text-property s (1- (point)) 'face 'org-agenda-structure)
	      (put-text-property s (1- (point)) 'org-date-line t)
	      (if (equal d today)
		  (put-text-property s (1- (point)) 'org-today t))
	      (and rtn (insert (org-finalize-agenda-entries rtn) "\n"))
	      (put-text-property s (1- (point)) 'day d)))))
    (goto-char (point-min))
    (goto-char (or (text-property-any (point-min) (point-max) 'org-today t)
		   (point-min)))
    (add-text-properties (point-min) (point-max) '(org-agenda-type timeline))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

(defun org-get-all-dates (beg end &optional no-ranges force-today inactive empty)
  "Return a list of all relevant day numbers from BEG to END buffer positions.
If NO-RANGES is non-nil, include only the start and end dates of a range,
not every single day in the range.  If FORCE-TODAY is non-nil, make
sure that TODAY is included in the list.  If INACTIVE is non-nil, also
inactive time stamps (those in square brackets) are included.
When EMPTY is non-nil, also include days without any entries."
  (let ((re (if inactive org-ts-regexp-both org-ts-regexp))
	 dates dates1 date day day1 day2 ts1 ts2)
    (if force-today
	(setq dates (list (time-to-days (current-time)))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward re end t)
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
      (setq dates (sort dates '<))
      (when empty
	(while (setq day (pop dates))
	  (setq day2 (car dates))
	  (push day dates1)
	  (when (and day2 empty)
	    (if (or (eq empty t)
		    (and (numberp empty) (<= (- day2 day) empty)))
		(while (< (setq day (1+ day)) day2)
		  (push (list day) dates1))
	      (push (cons :omitted (- day2 day)) dates1))))
	(setq dates (nreverse dates1)))
      dates)))

;;; Agenda Daily/Weekly

(defvar org-agenda-overriding-arguments nil) ; dynamically scoped parameter
(defvar org-agenda-start-day nil) ; dynamically scoped parameter
(defvar org-agenda-last-arguments nil
  "The arguments of the previous call to org-agenda")
(defvar org-starting-day nil) ; local variable in the agenda buffer
(defvar org-agenda-span nil) ; local variable in the agenda buffer
(defvar org-include-all-loc nil) ; local variable
(defvar org-agenda-remove-date nil) ; dynamically scoped

;;;###autoload
(defun org-agenda-list (&optional include-all start-day ndays)
  "Produce a weekly view from all files in variable `org-agenda-files'.
The view will be for the current week, but from the overview buffer you
will be able to go to other weeks.
With one \\[universal-argument] prefix argument INCLUDE-ALL, all unfinished TODO items will
also be shown, under the current date.
With two \\[universal-argument] prefix argument INCLUDE-ALL, all TODO entries marked DONE
on the days are also shown.  See the variable `org-log-done' for how
to turn on logging.
START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.
NDAYS defaults to `org-agenda-ndays'."
  (interactive "P")
  (setq ndays (or ndays org-agenda-ndays)
	start-day (or start-day org-agenda-start-day))
  (if org-agenda-overriding-arguments
      (setq include-all (car org-agenda-overriding-arguments)
	    start-day (nth 1 org-agenda-overriding-arguments)
	    ndays (nth 2 org-agenda-overriding-arguments)))
  (if (stringp start-day)
      ;; Convert to an absolute day number
      (setq start-day (time-to-days (org-read-date nil t start-day))))
  (setq org-agenda-last-arguments (list include-all start-day ndays))
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)
  (require 'calendar)
  (let* ((org-agenda-start-on-weekday
	  (if (or (equal ndays 7) (and (null ndays) (equal 7 org-agenda-ndays)))
	      org-agenda-start-on-weekday nil))
	 (thefiles (org-agenda-files))
	 (files thefiles)
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
	 (inhibit-redisplay (not debug-on-error))
	 s e rtn rtnall file date d start-pos end-pos todayp nd)
    (setq org-agenda-redo-command
	  (list 'org-agenda-list (list 'quote include-all) start-day ndays))
    ;; Make the list of days
    (setq ndays (or ndays org-agenda-ndays)
	  nd ndays)
    (while (> ndays 1)
      (push (1+ (car day-numbers)) day-numbers)
      (setq ndays (1- ndays)))
    (setq day-numbers (nreverse day-numbers))
    (org-prepare-agenda "Day/Week")
    (org-set-local 'org-starting-day (car day-numbers))
    (org-set-local 'org-include-all-loc include-all)
    (org-set-local 'org-agenda-span
		   (org-agenda-ndays-to-span nd))
    (when (and (or include-all org-agenda-include-all-todo)
	       (member today day-numbers))
      (setq files thefiles
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
			     (list 'face 'org-agenda-structure))
	(insert (org-finalize-agenda-entries rtnall) "\n")))
    (setq s (point))
    (insert (capitalize (symbol-name (org-agenda-ndays-to-span nd)))
	    "-agenda:\n")
    (add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
					      'org-date-line t))
    (while (setq d (pop day-numbers))
      (setq date (calendar-gregorian-from-absolute d)
	    s (point))
      (if (or (setq todayp (= d today))
	      (and (not start-pos) (= d sd)))
	  (setq start-pos (point))
	(if (and start-pos (not end-pos))
	    (setq end-pos (point))))
      (setq files thefiles
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (if org-agenda-show-log
	      (setq rtn (org-agenda-get-day-entries
			 file date
			 :deadline :scheduled :timestamp :sexp :closed))
	    (setq rtn (org-agenda-get-day-entries
		       file date
		       :deadline :scheduled :sexp :timestamp)))
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
; FIXME: this gives a timezone problem
;	    (insert (format-time-string org-agenda-date-format
;					(calendar-time-from-absolute d 0)) "\n")
	    (put-text-property s (1- (point)) 'face 'org-agenda-structure)
	    (put-text-property s (1- (point)) 'org-date-line t)
	    (if todayp (put-text-property s (1- (point)) 'org-today t))
	    (if rtnall (insert
			(org-finalize-agenda-entries
			 (org-agenda-add-time-grid-maybe
			  rtnall nd todayp))
			"\n"))
	    (put-text-property s (1- (point)) 'day d))))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (unless (and (pos-visible-in-window-p (point-min))
		 (pos-visible-in-window-p (point-max)))
      (goto-char (1- (point-max)))
      (recenter -1)
      (if (not (pos-visible-in-window-p (or start-pos 1)))
	  (progn
	    (goto-char (or start-pos 1))
	    (recenter 1))))
    (goto-char (or start-pos 1))
    (add-text-properties (point-min) (point-max) '(org-agenda-type agenda))
    (org-finalize-agenda)
    (setq buffer-read-only t)
    (message "")))

(defun org-agenda-ndays-to-span (n)
  (cond ((< n 7) 'day) ((= n 7) 'week) ((< n 32) 'month) (t 'year)))

;;; Agenda TODO list

(defvar org-select-this-todo-keyword nil)
(defvar org-last-arg nil)

;;;###autoload
(defun org-todo-list (arg)
  "Show all TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using \\[universal-argument], you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'."
  (interactive "P")
  (require 'calendar)
  (org-compile-prefix-format 'todo)
  (org-set-sorting-strategy 'todo)
  (org-prepare-agenda "TODO")
  (let* ((today (time-to-days (current-time)))
	 (date (calendar-gregorian-from-absolute today))
	 (kwds org-todo-keywords-for-agenda)
	 (completion-ignore-case t)
	 (org-select-this-todo-keyword
	  (if (stringp arg) arg
	    (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
	 rtn rtnall files file pos)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
	    (completing-read "Keyword (or KWD1|K2D2|...): "
			     (mapcar 'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (org-set-local 'org-last-arg arg)
    (setq org-agenda-redo-command
	  '(org-todo-list (or current-prefix-arg org-last-arg)))
    (setq files (org-agenda-files)
	  rtnall nil)
    (while (setq file (pop files))
      (catch 'nextfile
	(org-check-agenda-file file)
	(setq rtn (org-agenda-get-day-entries file date :todo))
	(setq rtnall (append rtnall rtn))))
    (if org-agenda-overriding-header
	(insert (org-add-props (copy-sequence org-agenda-overriding-header)
		    nil 'face 'org-agenda-structure) "\n")
      (insert "Global list of TODO items of type: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure))
      (setq pos (point))
      (insert (or org-select-this-todo-keyword "ALL") "\n")
      (add-text-properties pos (1- (point)) (list 'face 'org-warning))
      (setq pos (point))
      (unless org-agenda-multi
	(insert "Available with `N r': (0)ALL")
	(let ((n 0) s)
	  (mapc (lambda (x)
		  (setq s (format "(%d)%s" (setq n (1+ n)) x))
		  (if (> (+ (current-column) (string-width s) 1) (frame-width))
		      (insert "\n                     "))
		  (insert " " s))
		kwds))
	(insert "\n"))
      (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
    (when rtnall
      (insert (org-finalize-agenda-entries rtnall) "\n"))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (add-text-properties (point-min) (point-max) '(org-agenda-type todo))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

;;; Agenda tags match

;;;###autoload
(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (org-compile-prefix-format 'tags)
  (org-set-sorting-strategy 'tags)
  (let* ((org-tags-match-list-sublevels
	  (if todo-only t org-tags-match-list-sublevels))
	 (completion-ignore-case t)
	 rtn rtnall files file pos matcher
	 buffer)
    (setq matcher (org-make-tags-matcher match)
	  match (car matcher) matcher (cdr matcher))
    (org-prepare-agenda (concat "TAGS " match))
    (setq org-agenda-redo-command
	  (list 'org-tags-view (list 'quote todo-only)
		(list 'if 'current-prefix-arg nil match)))
    (setq files (org-agenda-files)
	  rtnall nil)
    (while (setq file (pop files))
      (catch 'nextfile
	(org-check-agenda-file file)
	(setq buffer (if (file-exists-p file)
			 (org-get-agenda-file-buffer file)
		       (error "No such file %s" file)))
	(if (not buffer)
	    ;; If file does not exist, merror message to agenda
	    (setq rtn (list
		       (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		  rtnall (append rtnall rtn))
	  (with-current-buffer buffer
	    (unless (org-mode-p)
	      (error "Agenda file %s is not in `org-mode'" file))
	    (setq org-category-table (org-get-category-table))
	    (save-excursion
	      (save-restriction
		(if org-agenda-restrict
		    (narrow-to-region org-agenda-restrict-begin
				      org-agenda-restrict-end)
		  (widen))
		(setq rtn (org-scan-tags 'agenda matcher todo-only))
		(setq rtnall (append rtnall rtn))))))))
    (if org-agenda-overriding-header
	(insert (org-add-props (copy-sequence org-agenda-overriding-header)
		    nil 'face 'org-agenda-structure) "\n")
      (insert "Headlines with TAGS match: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure))
      (setq pos (point))
      (insert match "\n")
      (add-text-properties pos (1- (point)) (list 'face 'org-warning))
      (setq pos (point))
      (unless org-agenda-multi
	(insert "Press `C-u r' to search again with new search string\n"))
      (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
    (when rtnall
      (insert (org-finalize-agenda-entries rtnall) "\n"))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (add-text-properties (point-min) (point-max) '(org-agenda-type tags))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

;;; Agenda Finding stuck projects

(defvar org-agenda-skip-regexp nil
  "Regular expression used in skipping subtrees for the agenda.
This is basically a temporary global variable that can be set and then
used by user-defined selections using `org-agenda-skip-function'.")

(defvar org-agenda-overriding-header nil
  "When this is set during todo and tags searches, will replace header.")

(defun org-agenda-skip-subtree-when-regexp-matches ()
  "Checks if the current subtree contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of this tree, causing agenda commands
to skip this subtree.  This is a function that can be put into
`org-agenda-skip-function' for the duration of a command."
  (let ((end (save-excursion (org-end-of-subtree t)))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip end)))

(defun org-agenda-skip-entry-if (&rest conditions)
  "Skip entry is any of CONDITIONS is true.
See `org-agenda-skip-if for details."
  (org-agenda-skip-if nil conditions))
(defun org-agenda-skip-subtree-if (&rest conditions)
  "Skip entry is any of CONDITIONS is true.
See `org-agenda-skip-if for details."
  (org-agenda-skip-if t conditions))

(defun org-agenda-skip-if (subtree conditions)
  "Checks current entity for CONDITIONS.
If SUBTREE is non-nil, the entire subtree is checked.  Otherwise, only
the entry, i.e. the text before the next heading is checked.

CONDITIONS is a list of symbols, boolean OR is used to combine the results
from different tests.  Valid conditions are:

scheduled     Check if there is a scheduled cookie
notscheduled  Check if there is no scheduled cookie
deadline      Check if there is a deadline
notdeadline   Check if there is no deadline
regexp        Check if regexp matches
notregexp     Check if regexp does not match.

The regexp is taken from the conditions list, it must com right after the
`regexp' of `notregexp' element.

If any of these conditions is met, this function returns the end point of
the entity, causing the search to continue from there.  This is a function
that can be put into `org-agenda-skip-function' for the duration of a command."
  (let (beg end m r)
    (org-back-to-heading t)
    (setq beg (point)
	  end (if subtree
		  (progn (org-end-of-subtree t) (point))
		(progn (outline-next-heading) (1- (point)))))
    (goto-char beg)
    (and
     (or
      (and (memq 'scheduled conditions)
	   (re-search-forward org-scheduled-time-regexp end t))
      (and (memq 'notscheduled conditions)
	   (not (re-search-forward org-scheduled-time-regexp end t)))
      (and (memq 'deadline conditions)
	   (re-search-forward org-deadline-time-regexp end t))
      (and (memq 'notdeadline conditions)
	   (not (re-search-forward org-deadline-time-regexp end t)))
      (and (setq m (memq 'regexp conditions))
	   (stringp (setq r (nth 1 m)))
	   (re-search-forward (nth 1 m) end t))
      (and (setq m (memq 'notregexp conditions))
	   (stringp (setq r (nth 1 m)))
	   (not (re-search-forward (nth 1 m) end t))))
     end)))

(defun org-agenda-list-stuck-projects (&rest ignore)
  "Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.
MATCH is being ignored."
  (interactive)
  (let* ((org-agenda-skip-function 'org-agenda-skip-subtree-when-regexp-matches)
	 ;; FIXME: we could have used org-agenda-skip-if here.
	 (org-agenda-overriding-header "List of stuck projects: ")
	 (matcher (nth 0 org-stuck-projects))
	 (todo (nth 1 org-stuck-projects))
	 (todo-wds (if (member "*" todo)
		       (progn
			 (org-prepare-agenda-buffers (org-agenda-files))
			 (org-delete-all
			  org-done-keywords-for-agenda
			  (copy-sequence org-todo-keywords-for-agenda)))
		     todo))
	 (todo-re (concat "^\\*+[ \t]+\\("
			  (mapconcat 'identity todo-wds "\\|")
			  "\\)\\>"))
	 (tags (nth 2 org-stuck-projects))
	 (tags-re (if (member "*" tags)
		      (org-re "^\\*+ .*:[[:alnum:]_@]+:[ \t]*$")
		    (concat "^\\*+ .*:\\("
			    (mapconcat 'identity tags "\\|")
			    (org-re "\\):[[:alnum:]_@:]*[ \t]*$"))))
	 (gen-re (nth 3 org-stuck-projects))
	 (re-list
	  (delq nil
		(list
		 (if todo todo-re)
		 (if tags tags-re)
		 (and gen-re (stringp gen-re) (string-match "\\S-" gen-re)
		      gen-re)))))
    (setq org-agenda-skip-regexp
	  (if re-list
	      (mapconcat 'identity re-list "\\|")
	    (error "No information how to identify unstuck projects")))
    (org-tags-view nil matcher)
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
	    '(org-agenda-list-stuck-projects
	      (or current-prefix-arg org-last-arg))))))

;;; Diary integration

(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.

(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (let* ((fancy-diary-buffer "*temporary-fancy-diary-buffer*")
	 (diary-display-hook '(fancy-diary-display))
	 (pop-up-frames nil)
	 (list-diary-entries-hook
	  (cons 'org-diary-default-entry list-diary-entries-hook))
	 (diary-file-name-prefix-function nil) ; turn this feature off
	 (diary-modify-entry-list-string-function 'org-modify-diary-entry-string)
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
	(list-diary-entries date 1)))  ;; Keep this name for now, compatibility
    (if (not (get-buffer fancy-diary-buffer))
	(setq entries nil)
      (with-current-buffer fancy-diary-buffer
	(setq buffer-read-only nil)
	(if (zerop (buffer-size))
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (zerop (buffer-size))
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))))
	(set-buffer-modified-p nil)
	(kill-buffer fancy-diary-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-format-agenda-item "" x "Diary" nil 'time))
	       ;; Extend the text properties to the beginning of the line
	       (org-add-props x (text-properties-at (1- (length x)) x)
		 'type "diary" 'date date))
	     entries)))))

(defun org-agenda-cleanup-fancy-diary ()
  "Remove unwanted stuff in buffer created by `fancy-diary-display'.
This gets rid of the date, the underline under the date, and
the dummy entry installed by `org-mode' to ensure non-empty diary for each
date.  It also removes lines that contain only whitespace."
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
		buffer-file-name)
	   (setq string (org-modify-diary-entry-string string))))))

(defun org-modify-diary-entry-string (string)
  "Add text properties to string, allowing org-mode to act on it."
  (org-add-props string nil
    'mouse-face 'highlight
    'keymap org-agenda-keymap
    'help-echo (if buffer-file-name
		   (format "mouse-2 or RET jump to diary file %s"
			   (abbreviate-file-name buffer-file-name))
		 "")
    'org-agenda-diary-link t
    'org-marker (org-agenda-new-marker (point-at-bol))))

(defun org-diary-default-entry ()
  "Add a dummy entry to the diary.
Needed to avoid empty dates which mess up holiday display."
  ;; Catch the error if dealing with the new add-to-diary-alist
  (when org-disable-agenda-to-diary
    (condition-case nil
	(add-to-diary-list original-date "Org-mode dummy" "")
      (error
       (add-to-diary-list original-date  "Org-mode dummy" "" nil)))))

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

   :sexp         FIXME

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
arguments (:deadline :scheduled :timestamp :sexp) are used.
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead."
  (org-agenda-maybe-reset-markers)
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)
  (setq args (or args '(:deadline :scheduled :timestamp :sexp)))
  (let* ((files (if (and entry (stringp entry) (string-match "\\S-" entry))
		    (list entry)
		  (org-agenda-files t)))
	 file rtn results)
    (org-prepare-agenda-buffers files)
    ;; If this is called during org-agenda, don't return any entries to
    ;; the calendar.  Org Agenda will list these entries itself.
    (if org-disable-agenda-to-diary (setq files nil))
    (while (setq file (pop files))
      (setq rtn (apply 'org-agenda-get-day-entries file date args))
      (setq results (append results rtn)))
    (if results
	(concat (org-finalize-agenda-entries results) "\n"))))

;;; Agenda entry finders

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-diary'."
  (setq args (or args '(:deadline :scheduled :timestamp :sexp)))
  (let* ((org-startup-folded nil)
	 (org-startup-align-all-tables nil)
	 (buffer (if (file-exists-p file)
		     (org-get-agenda-file-buffer file)
		   (error "No such file %s" file)))
	 arg results rtn)
    (if (not buffer)
	;; If file does not exist, make sure an error message ends up in diary
	(list (format "ORG-AGENDA-ERROR: No such org-file %s" file))
      (with-current-buffer buffer
	(unless (org-mode-p)
	  (error "Agenda file %s is not in `org-mode'" file))
	(setq org-category-table (org-get-category-table))
	(let ((case-fold-search nil))
	  (save-excursion
	    (save-restriction
	      (if org-agenda-restrict
		  (narrow-to-region org-agenda-restrict-begin
				    org-agenda-restrict-end)
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
		 ((eq arg :sexp)
		  (setq rtn (org-agenda-get-sexps))
		  (setq results (append results rtn)))
		 ((eq arg :scheduled)
		  (setq rtn (org-agenda-get-scheduled))
		  (setq results (append results rtn)))
		 ((eq arg :closed)
		  (setq rtn (org-agenda-get-closed))
		  (setq results (append results rtn)))
		 ((eq arg :deadline)
		  (setq rtn (org-agenda-get-deadlines))
		  (setq results (append results rtn))))))))
	results))))

;; FIXME: this works only if the cursor is not at the
;; beginning of the entry
(defun org-entry-is-done-p ()
  "Is the current entry marked DONE?"
  (save-excursion
    (and (re-search-backward "[\r\n]\\*+ " nil t)
	 (looking-at org-nl-done-regexp))))

(defun org-at-date-range-p (&optional inactive-ok)
  "Is the cursor inside a date range?"
  (interactive)
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defun org-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
		      'done-face 'org-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 ;; FIXME: get rid of the \n at some point  but watch out
	 (regexp (concat "^\\*+[ \t]+\\("
			 (if org-select-this-todo-keyword
			     (if (equal org-select-this-todo-keyword "*")
				 org-todo-regexp
			       (concat "\\<\\("
				       (mapconcat 'identity (org-split-string org-select-this-todo-keyword "|") "\\|")
				     "\\)\\>"))
			   org-not-done-regexp)
			 "[^\n\r]*\\)"))
	 marker priority category tags
	 ee txt beg end)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(save-match-data
	  (beginning-of-line)
	  (setq beg (point) end (progn (outline-next-heading) (point)))
	  (when (or (and org-agenda-todo-ignore-scheduled (goto-char beg)
			 (re-search-forward org-scheduled-time-regexp end t))
		    (and org-agenda-todo-ignore-deadlines (goto-char beg)
			 (re-search-forward org-deadline-time-regexp end t)
			 (org-deadline-close (match-string 1))))
	    (goto-char (1+ beg))
	    (or org-agenda-todo-list-sublevels (org-end-of-subtree 'invisible))
	    (throw :skip nil)))
	(goto-char beg)
	(org-agenda-skip)
	(goto-char (match-beginning 1))
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      category (org-get-category)
	      tags (org-get-tags-at (point))
	      txt (org-format-agenda-item "" (match-string 1) category tags)
	      priority (1+ (org-get-priority txt)))
	(org-add-props txt props
	  'org-marker marker 'org-hd-marker marker
	  'priority priority 'org-category category
	  'type "todo")
	(push txt ee)
	(if org-agenda-todo-list-sublevels
	    (goto-char (match-end 1))
	  (org-end-of-subtree 'invisible))))
    (nreverse ee)))

(defconst org-agenda-no-heading-message
  "No heading for this item in buffer or region.")

(defun org-agenda-get-timestamps ()
  "Return the date stamp information for agenda display."
  (let* ((props (list 'face nil
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (d1 (calendar-absolute-from-gregorian date))
	 (remove-re
	  (concat
	   (regexp-quote
	    (format-time-string
	     "<%Y-%m-%d"
	     (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	   ".*?>"))
	 (regexp
	  (concat
	   (regexp-quote
	    (substring
	     (format-time-string
	      (car org-time-stamp-formats)
	      (apply 'encode-time  ; DATE bound by calendar
		     (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
	     0 11))
	   "\\|\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
	   "\\|\\(<%%\\(([^>\n]+)\\)>\\)"))
	 marker hdmarker deadlinep scheduledp donep tmp priority category
	 ee txt timestr tags b0 b3 e3)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq b0 (match-beginning 0)
	    b3 (match-beginning 3) e3 (match-end 3))
      (catch :skip
	(and (org-at-date-range-p) (throw :skip nil))
	(org-agenda-skip)
	(if (and (match-end 1)
		 (not (= d1 (org-time-string-to-absolute (match-string 1) d1))))
	    (throw :skip nil))
	(if (and e3
		 (not (org-diary-sexp-entry (buffer-substring b3 e3) "" date)))
	    (throw :skip nil))
	(setq marker (org-agenda-new-marker b0)
	      category (org-get-category b0)
	      tmp (buffer-substring (max (point-min)
					 (- b0 org-ds-keyword-length))
				    b0)
	      timestr (if b3 "" (buffer-substring b0 (point-at-eol)))
	      deadlinep (string-match org-deadline-regexp tmp)
	      scheduledp (string-match org-scheduled-regexp tmp)
	      donep (org-entry-is-done-p))
	(if (or scheduledp deadlinep) (throw :skip t))
	(if (string-match ">" timestr)
	    ;; substring should only run to end of time stamp
	    (setq timestr (substring timestr 0 (match-end 0))))
	(save-excursion
	  (if (re-search-backward "^\\*+ " nil t)
	      (progn
		(goto-char (match-beginning 0))
		(setq hdmarker (org-agenda-new-marker)
		      tags (org-get-tags-at))
		(looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
		(setq txt (org-format-agenda-item
			   nil (match-string 1) category tags timestr nil
			   remove-re)))
	    (setq txt org-agenda-no-heading-message))
	  (setq priority (org-get-priority txt))
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker)
	  (org-add-props txt nil 'priority priority
			 'org-category category 'date date
			 'type "timestamp")
	  (push txt ee))
	(outline-next-heading)))
    (nreverse ee)))

(defun org-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (require 'diary-lib)
  (let* ((props (list 'face nil
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp "^&?%%(")
	 marker category ee txt tags entry result beg b sexp sexp-entry)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq beg (match-beginning 0))
	(goto-char (1- (match-end 0)))
	(setq b (point))
	(forward-sexp 1)
	(setq sexp (buffer-substring b (point)))
	(setq sexp-entry (if (looking-at "[ \t]*\\(\\S-.*\\)")
			     (org-trim (match-string 1))
			   ""))
	(setq result (org-diary-sexp-entry sexp sexp-entry date))
	(when result
	  (setq marker (org-agenda-new-marker beg)
		category (org-get-category beg))

	  (if (string-match "\\S-" result)
	      (setq txt result)
	    (setq txt "SEXP entry returned empty string"))

	  (setq txt (org-format-agenda-item
                     "" txt category tags 'time))
	  (org-add-props txt props 'org-marker marker)
	  (org-add-props txt nil
	    'org-category category 'date date
	    'type "sexp")
	  (push txt ee))))
    (nreverse ee)))

(defun org-agenda-get-closed ()
  "Return the logged TODO entries for agenda display."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (concat
		  "\\<\\(" org-closed-string "\\|" org-clock-string "\\) *\\["
		  (regexp-quote
		   (substring
		    (format-time-string
		     (car org-time-stamp-formats)
		     (apply 'encode-time  ; DATE bound by calendar
			    (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
		    1 11))))
	 marker hdmarker priority category tags closedp
	 ee txt timestr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      closedp (equal (match-string 1) org-closed-string)
	      category (org-get-category (match-beginning 0))
	      timestr (buffer-substring (match-beginning 0) (point-at-eol))
	      ;; donep (org-entry-is-done-p)
	      )
	(if (string-match "\\]" timestr)
	    ;; substring should only run to end of time stamp
	    (setq timestr (substring timestr 0 (match-end 0))))
	(save-excursion
	  (if (re-search-backward "^\\*+ " nil t)
	      (progn
		(goto-char (match-beginning 0))
		(setq hdmarker (org-agenda-new-marker)
		      tags (org-get-tags-at))
		(looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
		(setq txt (org-format-agenda-item
			   (if closedp "Closed:    " "Clocked:   ")
			   (match-string 1) category tags timestr)))
	    (setq txt org-agenda-no-heading-message))
	  (setq priority 100000)
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker 'face 'org-done
	    'priority priority 'org-category category
	    'type "closed" 'date date
	    'undone-face 'org-warning 'done-face 'org-done)
	  (push txt ee))
	(outline-next-heading)))
    (nreverse ee)))

(defun org-agenda-get-deadlines ()
  "Return the deadline information for agenda display."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-deadline-time-regexp)
	 (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
	 (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
	 d2 diff dfrac wdays pos pos1 category tags
	 ee txt head face s upcomingp donep timestr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq s (match-string 1)
	      pos (1- (match-beginning 1))
	      d2 (org-time-string-to-absolute (match-string 1) d1)
	      diff (- d2 d1)
	      wdays (org-get-wdays s)
	      dfrac (/ (* 1.0 (- wdays diff)) wdays)
	      upcomingp (and todayp (> diff 0)))
	;; When to show a deadline in the calendar:
	;; If the expiration is within wdays warning time.
	;; Past-due deadlines are only shown on the current date
	(if (or (and (<= diff wdays) todayp)
		(= diff 0))
	    (save-excursion
	      (setq category (org-get-category))
	      (if (re-search-backward "^\\*+[ \t]+" nil t)
		  (progn
		    (goto-char (match-end 0))
		    (setq pos1 (match-beginning 0))
		    (setq tags (org-get-tags-at pos1))
		    (setq head (buffer-substring-no-properties
				(point)
				(progn (skip-chars-forward "^\r\n")
				       (point))))
		    (setq donep (string-match org-looking-at-done-regexp head))
		    (if (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
			(setq timestr
			      (concat (substring s (match-beginning 1)) " "))
		      (setq timestr 'time))
		    (if (and donep
			     (or org-agenda-skip-deadline-if-done
				 (not (= diff 0))))
			(setq txt nil)
		      (setq txt (org-format-agenda-item
				 (if (= diff 0)
				     "Deadline:  "
				   (format "In %3d d.: " diff))
				 head category tags timestr))))
		(setq txt org-agenda-no-heading-message))
	      (when txt
		(setq face (org-agenda-deadline-face dfrac))
		(org-add-props txt props
		  'org-marker (org-agenda-new-marker pos)
		  'org-hd-marker (org-agenda-new-marker pos1)
		  'priority (+ (if upcomingp (floor (* dfrac 10.)) 100)
			       (org-get-priority txt))
		  'org-category category
		  'type (if upcomingp "upcoming-deadline" "deadline")
		  'date (if upcomingp date d2)
		  'face (if donep 'org-done face)
		  'undone-face face 'done-face 'org-done)
		(push txt ee))))))
    (nreverse ee)))

(defun org-agenda-deadline-face (fraction)
  "Return the face to displaying a deadline item.
FRACTION is what fraction of the head-warning time has passed."
  (let ((faces org-agenda-deadline-faces) f)
    (catch 'exit
      (while (setq f (pop faces))
	(if (>= fraction (car f)) (throw 'exit (cdr f)))))))

(defun org-agenda-get-scheduled ()
  "Return the scheduled information for agenda display."
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'done-face 'org-done
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-scheduled-time-regexp)
	 (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
	 (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
	 d2 diff pos pos1 category tags
	 ee txt head pastschedp donep face timestr s)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq s (match-string 1)
	      pos (1- (match-beginning 1))
	      d2 (org-time-string-to-absolute (match-string 1) d1)
	      diff (- d2 d1))
	(setq pastschedp (and todayp (< diff 0)))
	;; When to show a scheduled item in the calendar:
	;; If it is on or past the date.
	(if (or (and (< diff 0) todayp)
		(= diff 0))
	    (save-excursion
	      (setq category (org-get-category))
	      (if (re-search-backward "^\\*+[ \t]+" nil t)
		  (progn
		    (goto-char (match-end 0))
		    (setq pos1 (match-beginning 0))
		    (setq tags (org-get-tags-at))
		    (setq head (buffer-substring-no-properties
				(point)
				(progn (skip-chars-forward "^\r\n") (point))))
		    (setq donep (string-match org-looking-at-done-regexp head))
		    (if (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
			(setq timestr
			      (concat (substring s (match-beginning 1)) " "))
		      (setq timestr 'time))
		    (if (and donep
			     (or org-agenda-skip-scheduled-if-done
				 (not (= diff 0))))
			(setq txt nil)
		      (setq txt (org-format-agenda-item
				 (if (= diff 0)
				     "Scheduled: "
				   (format "Sched.%2dx: " (- 1 diff)))
				 head category tags timestr))))
		(setq txt org-agenda-no-heading-message))
	      (when txt
		(setq face (if pastschedp
			       'org-scheduled-previously
			     'org-scheduled-today))
		(org-add-props txt props
		  'undone-face face
		  'face (if donep 'org-done face)
		  'org-marker (org-agenda-new-marker pos)
		  'org-hd-marker (org-agenda-new-marker pos1)
		  'type (if pastschedp "past-scheduled" "scheduled")
		  'date (if pastschedp d2 date)
		  'priority (+ (- 5 diff) (org-get-priority txt))
		  'org-category category)
		(push txt ee))))))
    (nreverse ee)))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (let* ((props (list 'face nil
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-tr-regexp)
	 (d0 (calendar-absolute-from-gregorian date))
	 marker hdmarker ee txt d1 d2 s1 s2 timestr category tags pos)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq pos (point))
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
	      (setq category (org-get-category))
	      (if (re-search-backward "^\\*+ " nil t)
		  (progn
		    (goto-char (match-beginning 0))
		    (setq hdmarker (org-agenda-new-marker (point)))
		    (setq tags (org-get-tags-at))
		    (looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
		    (setq txt (org-format-agenda-item
			       (format (if (= d1 d2) "" "(%d/%d): ")
				       (1+ (- d0 d1)) (1+ (- d2 d1)))
			       (match-string 1) category tags
			       (if (= d0 d1) timestr))))
		(setq txt org-agenda-no-heading-message))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker hdmarker
		'type "block" 'date date
		'priority (org-get-priority txt) 'org-category category)
	      (push txt ee)))
	(goto-char pos)))
    ;; Sort the entries by expiration date.
    (nreverse ee)))

;;; Agenda presentation and sorting

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
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +\\sw+ +\\)"
   "\\([012][0-9]:[0-5][0-9]\\(-\\([012][0-9]:[0-5][0-9]\\)\\)?[^\n\r>]*?\\)>"
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
(defvar org-prefix-has-tag nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%T'.")

(defun org-format-agenda-item (extra txt &optional category tags dotime
				     noprefix remove-re)
  "Format TXT to be inserted into the agenda buffer.
In particular, it adds the prefix and corresponding text properties.  EXTRA
must be a string and replaces the `%s' specifier in the prefix format.
CATEGORY (string, symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.  DOTIME, when non-nil, indicates that a
time-of-day should be extracted from TXT for sorting of this entry, and for
the `%t' specifier in the format.  When DOTIME is a string, this string is
searched for a time before TXT is.  NOPREFIX is a flag and indicates that
only the correctly processes TXT should be returned - this is used by
`org-agenda-change-all-lines'.  TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  (save-match-data
    ;; Diary entries sometimes have extra whitespace at the beginning
    (if (string-match "^ +" txt) (setq txt (replace-match "" nil nil txt)))
    (let* ((category (or category
			 org-category
			 (if buffer-file-name
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))
			   "")))
	   (tag (if tags (nth (1- (length tags)) tags) ""))
	   time    ; time and tag are needed for the eval of the prefix format
	   (ts (if dotime (concat (if (stringp dotime) dotime "") txt)))
	   (time-of-day (and dotime (org-get-time-of-day ts)))
	   stamp plain s0 s1 s2 rtn srp)
      (when (and dotime time-of-day org-prefix-has-time)
	;; Extract starting and ending time and move them to prefix
	(when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		  (setq plain (string-match org-plain-time-of-day-regexp ts)))
	  (setq s0 (match-string 0 ts)
		srp (and stamp (match-end 3))
		s1 (match-string (if plain 1 2) ts)
		s2 (match-string (if plain 8 (if srp 4 6)) ts))

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
	(if s1 (setq s1 (org-get-time-of-day s1 'string t)))
	(if s2 (setq s2 (org-get-time-of-day s2 'string t))))

      (when (and s1 (not s2) org-agenda-default-appointment-duration
		 (string-match "\\([0-9]+\\):\\([0-9]+\\)" s1))
	(let ((m (+ (string-to-number (match-string 2 s1))
		    (* 60 (string-to-number (match-string 1 s1)))
		    org-agenda-default-appointment-duration))
	      h)
	  (setq h (/ m 60) m (- m (* h 60)))
	  (setq s2 (format "%02d:%02d" h m))))

      (when (string-match (org-re "\\([ \t]+\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$")
			  txt)
	;; Tags are in the string
	(if (or (eq org-agenda-remove-tags t)
		(and org-agenda-remove-tags
		     org-prefix-has-tag))
	    (setq txt (replace-match "" t t txt))
	  (setq txt (replace-match
		     (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			     (match-string 2 txt))
		     t t txt))))

      (when remove-re
	(while (string-match remove-re txt)
	  (setq txt (replace-match "" t t txt))))

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
      (org-add-props rtn nil
	'org-category (downcase category) 'tags tags
	'prefix-length (- (length rtn) (length txt))
	'time-of-day time-of-day
	'txt txt
	'time time
	'extra extra
	'dotime dotime))))

(defvar org-agenda-sorting-strategy) ;; FIXME: can be removed?
(defvar org-agenda-sorting-strategy-selected nil)

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
		 nil string "" nil
		 (concat (substring time 0 -2) ":" (substring time -2)))
		new)
	  (put-text-property
	   1 (length (car new)) 'face 'org-time-grid (car new))))
      (if (member 'time-up org-agenda-sorting-strategy-selected)
	  (append new list)
	(append list new)))))

(defun org-compile-prefix-format (key)
  "Compile the prefix format into a Lisp form that can be evaluated.
The resulting form is returned and stored in the variable
`org-prefix-format-compiled'."
  (setq org-prefix-has-time nil org-prefix-has-tag nil)
  (let ((s (cond
	    ((stringp org-agenda-prefix-format)
	     org-agenda-prefix-format)
	    ((assq key org-agenda-prefix-format)
	     (cdr (assq key org-agenda-prefix-format)))
	    (t "  %-12:c%?-12t% s")))
	(start 0)
	varform vars var e c f opt)
    (while (string-match "%\\(\\?\\)?\\([-+]?[0-9.]*\\)\\([ .;,:!?=|/<>]?\\)\\([cts]\\)"
			 s start)
      (setq var (cdr (assoc (match-string 4 s)
			    '(("c" . category) ("t" . time) ("s" . extra)
			      ("T" . tag))))
	    c (or (match-string 3 s) "")
	    opt (match-beginning 1)
	    start (1+ (match-beginning 0)))
      (if (equal var 'time) (setq org-prefix-has-time t))
      (if (equal var 'tag)  (setq org-prefix-has-tag  t))
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

(defun org-set-sorting-strategy (key)
  (if (symbolp (car org-agenda-sorting-strategy))
      ;; the old format
      (setq org-agenda-sorting-strategy-selected org-agenda-sorting-strategy)
    (setq org-agenda-sorting-strategy-selected
	  (or (cdr (assq key org-agenda-sorting-strategy))
	      (cdr (assq 'agenda org-agenda-sorting-strategy))
	      '(time-up category-keep priority-down)))))

(defun org-get-time-of-day (s &optional string mod24)
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
     (let* ((h (string-to-number (match-string 1 s)))
	    (m (if (match-end 3) (string-to-number (match-string 3 s)) 0))
	    (ampm (if (match-end 4) (downcase (match-string 4 s))))
	    (am-p (equal ampm "am"))
	    (h1   (cond ((not ampm) h)
			((= h 12) (if am-p 0 12))
			(t (+ h (if am-p 0 12)))))
	    (h2 (if (and string mod24 (not (and (= m 0) (= h1 24))))
		    (mod h1 24) h1))
	    (t0 (+ (* 100 h2) m))
	    (t1 (concat (if (>= h1 24) "+" " ")
			(if (< t0 100) "0" "")
			(if (< t0 10)  "0" "")
			(int-to-string t0))))
       (if string (concat (substring t1 -4 -2) ":" (substring t1 -2)) t0)))))

(defun org-finalize-agenda-entries (list &optional nosort)
  "Sort and concatenate the agenda items."
  (setq list (mapcar 'org-agenda-highlight-todo list))
  (if nosort
      list
    (mapconcat 'identity (sort list 'org-entries-lessp) "\n")))

(defun org-agenda-highlight-todo (x)
  (let (re pl)
    (if (eq x 'line)
	(save-excursion
	  (beginning-of-line 1)
	  (setq re (get-text-property (point) 'org-todo-regexp))
	  (goto-char (+ (point) (or (get-text-property (point) 'prefix-length) 0)))
	  (and (looking-at (concat "[ \t]*\\.*" re))
	       (add-text-properties (match-beginning 0) (match-end 0)
				    (list 'face (org-get-todo-face 0)))))
      (setq re (concat (get-text-property 0 'org-todo-regexp x))
	    pl (get-text-property 0 'prefix-length x))
      (and re (equal (string-match (concat "\\(\\.*\\)" re) x (or pl 0)) pl)
	   (add-text-properties
	    (or (match-end 1) (match-end 0)) (match-end 0)
	    (list 'face (org-get-todo-face (match-string 2 x)))
	    x))
      x)))

(defsubst org-cmp-priority (a b)
  "Compare the priorities of string A and B."
  (let ((pa (or (get-text-property 1 'priority a) 0))
	(pb (or (get-text-property 1 'priority b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))

(defsubst org-cmp-category (a b)
  "Compare the string values of categories of strings A and B."
  (let ((ca (or (get-text-property 1 'org-category a) ""))
	(cb (or (get-text-property 1 'org-category b) "")))
    (cond ((string-lessp ca cb) -1)
	  ((string-lessp cb ca) +1)
	  (t nil))))

(defsubst org-cmp-tag (a b)
  "Compare the string values of categories of strings A and B."
  (let ((ta (car (last (get-text-property 1 'tags a))))
	(tb (car (last (get-text-property 1 'tags b)))))
    (cond ((not ta) +1)
	  ((not tb) -1)
	  ((string-lessp ta tb) -1)
	  ((string-lessp tb ta) +1)
	  (t nil))))

(defsubst org-cmp-time (a b)
  "Compare the time-of-day values of strings A and B."
  (let* ((def (if org-sort-agenda-notime-is-late 9901 -1))
	 (ta (or (get-text-property 1 'time-of-day a) def))
	 (tb (or (get-text-property 1 'time-of-day b) def)))
    (cond ((< ta tb) -1)
	  ((< tb ta) +1)
	  (t nil))))

(defun org-entries-lessp (a b)
  "Predicate for sorting agenda entries."
  ;; The following variables will be used when the form is evaluated.
  ;; So even though the compiler complains, keep them.
  (let* ((time-up (org-cmp-time a b))
	 (time-down (if time-up (- time-up) nil))
	 (priority-up (org-cmp-priority a b))
	 (priority-down (if priority-up (- priority-up) nil))
	 (category-up (org-cmp-category a b))
	 (category-down (if category-up (- category-up) nil))
	 (category-keep (if category-up +1 nil))
	 (tag-up (org-cmp-tag a b))
	 (tag-down (if tag-up (- tag-up) nil)))
    (cdr (assoc
	  (eval (cons 'or org-agenda-sorting-strategy-selected))
	  '((-1 . t) (1 . nil) (nil . nil))))))

;;; Agenda commands

(defun org-agenda-check-type (error &rest types)
  "Check if agenda buffer is of allowed type.
If ERROR is non-nil, throw an error, otherwise just return nil."
  (if (memq org-agenda-type types)
      t
    (if error
	(error "Not allowed in %s-type agenda buffers" org-agenda-type)
      nil)))

(defun org-agenda-quit ()
  "Exit agenda by removing the window or the buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (one-window-p)) (delete-window))
    (kill-buffer buf)
    (org-agenda-maybe-reset-markers 'force)
    (org-columns-remove-overlays))
  ;; Maybe restore the pre-agenda window configuration.
  (and org-agenda-restore-windows-after-quit
       (not (eq org-agenda-window-setup 'other-frame))
       org-pre-agenda-window-conf
       (set-window-configuration org-pre-agenda-window-conf)))

(defun org-agenda-exit ()
  "Exit agenda by removing the window or the buffer.
Also kill all Org-mode buffers which have been loaded by `org-agenda'.
Org-mode buffers visited directly by the user will not be touched."
  (interactive)
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (org-agenda-quit))

(defun org-save-all-org-buffers ()
  "Save all Org-mode buffers without user confirmation."
  (interactive)
  (message "Saving all Org-mode buffers...")
  (save-some-buffers t 'org-mode-p)
  (message "Saving all Org-mode buffers... done"))

(defun org-agenda-redo ()
  "Rebuild Agenda.
When this is the global TODO list, a prefix argument will be interpreted."
  (interactive)
  (let* ((org-agenda-keep-modes t)
	 (line (org-current-line))
	 (window-line (- line (org-current-line (window-start))))
	 (lprops (get 'org-agenda-redo-command 'org-lprops)))
    (message "Rebuilding agenda buffer...")
    (org-let lprops '(eval org-agenda-redo-command))
    (setq org-agenda-undo-list nil
	  org-agenda-pending-undo-list nil)
    (message "Rebuilding agenda buffer...done")
    (goto-line line)
    (recenter window-line)))

(defun org-agenda-goto-date (date)
  "Jump to DATE in agenda."
  (interactive (list (org-read-date)))
  (org-agenda-list nil date))

(defun org-agenda-goto-today ()
  "Go to today."
  (interactive)
  (org-agenda-check-type t 'timeline 'agenda)
  (let ((tdpos (text-property-any (point-min) (point-max) 'org-today t)))
    (cond
     (tdpos (goto-char tdpos))
     ((eq org-agenda-type 'agenda)
      (let* ((sd (time-to-days (current-time)))
	     (comp (org-agenda-compute-time-span sd org-agenda-span))
	     (org-agenda-overriding-arguments org-agenda-last-arguments))
	(setf (nth 1 org-agenda-overriding-arguments) (car comp))
	(setf (nth 2 org-agenda-overriding-arguments) (cdr comp))
	(org-agenda-redo)
	(org-agenda-find-today-or-agenda)))
     (t (error "Cannot find today")))))

(defun org-agenda-find-today-or-agenda ()
  (goto-char
   (or (text-property-any (point-min) (point-max) 'org-today t)
       (text-property-any (point-min) (point-max) 'org-agenda-type 'agenda)
       (point-min))))

(defun org-agenda-later (arg)
  "Go forward in time by thee current span.
With prefix ARG, go forward that many times the current span."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (let* ((span org-agenda-span)
	 (sd org-starting-day)
	 (greg (calendar-gregorian-from-absolute sd))
	 greg2 nd)
    (cond
     ((eq span 'day)
      (setq sd (+ arg sd) nd 1))
     ((eq span 'week)
      (setq sd (+ (* 7 arg) sd) nd 7))
     ((eq span 'month)
      (setq greg2 (list (+ (car greg) arg) (nth 1 greg) (nth 2 greg))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar greg2 (1+ (car greg2)))
      (setq nd (- (calendar-absolute-from-gregorian greg2) sd)))
     ((eq span 'year)
      (setq greg2 (list (car greg) (nth 1 greg) (+ arg (nth 2 greg)))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar (nthcdr 2 greg2) (1+ (nth 2 greg2)))
      (setq nd (- (calendar-absolute-from-gregorian greg2) sd))))
    (let ((org-agenda-overriding-arguments
	   (list (car org-agenda-last-arguments) sd nd t)))
    (org-agenda-redo)
    (org-agenda-find-today-or-agenda))))
 
(defun org-agenda-earlier (arg)
  "Go backward in time by the current span.
With prefix ARG, go backward that many times the current span."
  (interactive "p")
  (org-agenda-later (- arg)))

(defun org-agenda-day-view ()
  "Switch to daily view for agenda."
  (interactive)
  (setq org-agenda-ndays 1)
  (org-agenda-change-time-span 'day))
(defun org-agenda-week-view ()
  "Switch to daily view for agenda."
  (interactive)
  (setq org-agenda-ndays 7)
  (org-agenda-change-time-span 'week))
(defun org-agenda-month-view ()
  "Switch to daily view for agenda."
  (interactive)
  (org-agenda-change-time-span 'month))
(defun org-agenda-year-view ()
  "Switch to daily view for agenda."
  (interactive)
  (if (y-or-n-p "Are you sure you want to compute the agenda for an entire year? ")
      (org-agenda-change-time-span 'year)
    (error "Abort")))

(defun org-agenda-change-time-span (span)
  "Change the agenda view to SPAN.
SPAN may be `day', `week', `month', `year'."
  (org-agenda-check-type t 'agenda)
  (if (equal org-agenda-span span)
      (error "Viewing span is already \"%s\"" span))
  (let* ((sd (or (get-text-property (point) 'day)
		org-starting-day))
	 (computed (org-agenda-compute-time-span sd span))
	 (org-agenda-overriding-arguments
	  (list (car org-agenda-last-arguments)
		(car computed) (cdr computed) t)))
    (org-agenda-redo)
    (org-agenda-find-today-or-agenda))
  (org-agenda-set-mode-name)
  (message "Switched to %s view" span))

(defun org-agenda-compute-time-span (sd span)
  "Compute starting date and number of days for agenda.
SPAN may be `day', `week', `month', `year'.  The return value
is a cons cell with the starting date and the number of days,
so that the date SD will be in that range."
  (let* ((greg (calendar-gregorian-from-absolute sd))
	 nd)
    (cond
     ((eq span 'day)
      (setq nd 1))
     ((eq span 'week)
      (let* ((nt (calendar-day-of-week
		  (calendar-gregorian-from-absolute sd)))
	     (d (if org-agenda-start-on-weekday
		    (- nt org-agenda-start-on-weekday)
		  0)))
	(setq sd (- sd (+ (if (< d 0) 7 0) d)))
	(setq nd 7)))
     ((eq span 'month)
      (setq sd (calendar-absolute-from-gregorian
		(list (car greg) 1 (nth 2 greg)))
	    nd (- (calendar-absolute-from-gregorian
		   (list (1+ (car greg)) 1 (nth 2 greg)))
		  sd)))
     ((eq span 'year)
      (setq sd (calendar-absolute-from-gregorian
		(list 1 1 (nth 2 greg)))
	    nd (- (calendar-absolute-from-gregorian
		   (list 1 1 (1+ (nth 2 greg))))
		  sd))))
    (cons sd nd)))

;; FIXME: this no longer works if user make date format that starts with a blank
(defun org-agenda-next-date-line (&optional arg)
  "Jump to the next line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda 'timeline)
  (beginning-of-line 1)
  (if (looking-at "^\\S-") (forward-char 1))
  (if (not (re-search-forward "^\\S-" nil t arg))
      (progn
	(backward-char 1)
	(error "No next date after this line in this buffer")))
  (goto-char (match-beginning 0)))

(defun org-agenda-previous-date-line (&optional arg)
  "Jump to the previous line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda 'timeline)
  (beginning-of-line 1)
  (if (not (re-search-backward "^\\S-" nil t arg))
      (error "No previous date before this line in this buffer")))

;; Initialize the highlight
(defvar org-hl (org-make-overlay 1 1))
(org-overlay-put org-hl 'face 'highlight)

(defun org-highlight (begin end &optional buffer)
  "Highlight a region with overlay."
  (funcall (if (featurep 'xemacs) 'set-extent-endpoints 'move-overlay)
	   org-hl begin end (or buffer (current-buffer))))

(defun org-unhighlight ()
  "Detach overlay INDEX."
  (funcall (if (featurep 'xemacs) 'detach-extent 'delete-overlay) org-hl))

;; FIXME this is currently not used.
(defun org-highlight-until-next-command (beg end &optional buffer)
  (org-highlight beg end buffer)
  (add-hook 'pre-command-hook 'org-unhighlight-once))

(defun org-unhighlight-once ()
  (remove-hook 'pre-command-hook 'org-unhighlight-once)
  (org-unhighlight))

(defun org-agenda-follow-mode ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (setq org-agenda-follow-mode (not org-agenda-follow-mode))
  (org-agenda-set-mode-name)
  (message "Follow mode is %s"
	   (if org-agenda-follow-mode "on" "off")))

(defun org-agenda-log-mode ()
  "Toggle log mode in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
  (setq org-agenda-show-log (not org-agenda-show-log))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Log mode is %s"
	   (if org-agenda-show-log "on" "off")))

(defun org-agenda-toggle-diary ()
  "Toggle diary inclusion in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-include-diary (not org-agenda-include-diary))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Diary inclusion turned %s"
	   (if org-agenda-include-diary "on" "off")))

(defun org-agenda-toggle-time-grid ()
  "Toggle time grid in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
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
		(if org-agenda-use-time-grid   " Grid"   "")
		(if org-agenda-show-log        " Log"    "")))
  (force-mode-line-update))

(defun org-agenda-post-command-hook ()
  (and (eolp) (not (bolp)) (backward-char 1))
  (setq org-agenda-type (get-text-property (point) 'org-agenda-type))
  (if (and org-agenda-follow-mode
	   (get-text-property (point) 'org-marker))
      (org-agenda-show)))

(defun org-agenda-show-priority ()
  "Show the priority of the current item.
This priority is composed of the main priority given with the [#A] cookies,
and by additional input from the age of a schedules or deadline entry."
  (interactive)
  (let* ((pri (get-text-property (point-at-bol) 'priority)))
    (message "Priority is %d" (if pri pri -1000))))

(defun org-agenda-show-tags ()
  "Show the tags applicable to the current item."
  (interactive)
  (let* ((tags (get-text-property (point-at-bol) 'tags)))
    (if tags
	(message "Tags are :%s:"
		 (org-no-properties (mapconcat 'identity tags ":")))
      (message "No tags associated with this line"))))

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
    (when (org-mode-p)
      (org-show-context 'agenda)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))) ; show the next heading
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

(defvar org-agenda-after-show-hook nil
  "Normal hook run after an item has been shown from the agenda.
Point is in the buffer where the item originated.")

(defun org-agenda-kill ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (type (get-text-property (point) 'type))
	 dbeg dend (n 0) conf)
    (org-with-remote-undo buffer
     (with-current-buffer buffer
       (save-excursion
	 (goto-char pos)
	 (if (and (org-mode-p) (not (member type '("sexp"))))
	     (setq dbeg (progn (org-back-to-heading t) (point))
		   dend (org-end-of-subtree t t))
	   (setq dbeg (point-at-bol)
		 dend (min (point-max) (1+ (point-at-eol)))))
	 (goto-char dbeg)
	 (while (re-search-forward "^[ \t]*\\S-" dend t) (setq n (1+ n)))))
     (setq conf (or (eq t org-agenda-confirm-kill)
		    (and (numberp org-agenda-confirm-kill)
			 (> n org-agenda-confirm-kill))))
     (and conf
	  (not (y-or-n-p
		(format "Delete entry with %d lines in buffer \"%s\"? "
			n (buffer-name buffer))))
	  (error "Abort"))
     (org-remove-subtree-entries-from-agenda buffer dbeg dend)
     (with-current-buffer buffer (delete-region dbeg dend))
     (message "Agenda item and source killed"))))

(defun org-agenda-archive ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(if (org-mode-p)
	    (save-excursion
	      (goto-char pos)
	      (org-remove-subtree-entries-from-agenda)
	      (org-back-to-heading t)
	      (org-archive-subtree))
	  (error "Archiving works only in Org-mode files"))))))

(defun org-remove-subtree-entries-from-agenda (&optional buf beg end)
  "Remove all lines in the agenda that correspond to a given subtree.
The subtree is the one in buffer BUF, starting at BEG and ending at END.
If this information is not given, the function uses the tree at point."
  (let ((buf (or buf (current-buffer))) m p)
    (save-excursion
      (unless (and beg end)
	(org-back-to-heading t)
	(setq beg (point))
	(org-end-of-subtree t)
	(setq end (point)))
      (set-buffer (get-buffer org-agenda-buffer-name))
      (save-excursion
	(goto-char (point-max))
	(beginning-of-line 1)
	(while (not (bobp))
	  (when (and (setq m (get-text-property (point) 'org-marker))
		     (equal buf (marker-buffer m))
		     (setq p (marker-position m))
		     (>= p beg)
		     (<= p end))
	    (let ((inhibit-read-only t))
	      (delete-region (point-at-bol) (1+ (point-at-eol)))))
	  (beginning-of-line 0))))))

(defun org-agenda-open-link ()
  "Follow the link in the current line, if any."
  (interactive)
  (let ((eol (point-at-eol)))
    (save-excursion
      (if (or (re-search-forward org-bracket-link-regexp eol t)
	      (re-search-forward org-angle-link-re eol t)
	      (re-search-forward org-plain-link-re eol t))
	  (call-interactively 'org-open-at-point)
	(error "No link in current line")))))

(defun org-agenda-switch-to (&optional delete-other-windows)
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (and delete-other-windows (delete-other-windows))
    (widen)
    (goto-char pos)
    (when (org-mode-p)
      (org-show-context 'agenda)
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

(defun org-agenda-tree-to-indirect-buffer ()
  "Show the subtree corresponding to the current entry in an indirect buffer.
This calls the command `org-tree-to-indirect-buffer' from the original
Org-mode buffer.
With numerical prefix arg ARG, go up to this level and then take that tree.
With a C-u prefix, make a separate frame for this tree (i.e. don't use the
dedicated frame)."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
	(goto-char pos)
	(call-interactively 'org-tree-to-indirect-buffer)))))

(defvar org-last-heading-marker (make-marker)
  "Marker pointing to the headline that last changed its TODO state
by a remote command from the agenda.")

(defun org-agenda-todo-nextset ()
  "Switch TODO entry to next sequence."
  (interactive)
  (org-agenda-todo 'nextset))

(defun org-agenda-todo-previousset ()
  "Switch TODO entry to previous sequence."
  (interactive)
  (org-agenda-todo 'previousset))

(defun org-agenda-todo (&optional arg)
  "Cycle TODO state of line at point, also in Org-mode file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(org-todo arg)
	(and (bolp) (forward-char 1))
	(setq newhead (org-get-heading))
	(save-excursion
	  (org-back-to-heading)
	  (move-marker org-last-heading-marker (point))))
      (beginning-of-line 1)
      (save-excursion
	(org-agenda-change-all-lines newhead hdmarker 'fixface))
      (move-to-column col))))

(defun org-agenda-change-all-lines (newhead hdmarker &optional fixface)
  "Change all lines in the agenda buffer which match HDMARKER.
The new content of the line will be NEWHEAD (as modified by
`org-format-agenda-item').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified acording to
the new TODO state."
  (let* ((inhibit-read-only t)
	 props m pl undone-face done-face finish new dotime cat tags)
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line 1)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (get-text-property (point) 'org-hd-marker))
		   (equal m hdmarker))
	  (setq props (text-properties-at (point))
		dotime (get-text-property (point) 'dotime)
		cat (get-text-property (point) 'org-category)
		tags (get-text-property (point) 'tags)
		new (org-format-agenda-item "x" newhead cat tags dotime 'noprefix)
		pl (get-text-property (point) 'prefix-length)
		undone-face (get-text-property (point) 'undone-face)
		done-face (get-text-property (point) 'done-face))
	  (move-to-column pl)
	  (cond
	   ((equal new "")
	    (beginning-of-line 1)
	    (and (looking-at ".*\n?") (replace-match "")))
	   ((looking-at ".*")
	    (replace-match new t t)
	    (beginning-of-line 1)
	    (add-text-properties (point-at-bol) (point-at-eol) props)
	    (when fixface
	      (add-text-properties
	       (point-at-bol) (point-at-eol)
	       (list 'face
		     (if org-last-todo-state-is-todo
			 undone-face done-face))))
	    (org-agenda-highlight-todo 'line)
	    (beginning-of-line 1))
	   (t (error "Line update did not work"))))
	(beginning-of-line 0)))
    (org-finalize-agenda)))

;; FIXME: allow negative value for org-agenda-align-tags-to-column
;; See the code in set-tags for the way to do this.
(defun org-agenda-align-tags (&optional line)
  "Align all tags in agenda items to `org-agenda-align-tags-to-column'."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (re-search-forward (org-re "\\([ \t]+\\):[[:alnum:]_@:]+:[ \t]*$")
				(if line (point-at-eol) nil) t)
	(delete-region (match-beginning 1) (match-end 1))
	(goto-char (match-beginning 1))
	(insert (org-add-props
		    (make-string (max 1 (- org-agenda-align-tags-to-column
					   (current-column))) ?\ )
		    (text-properties-at (point))))))))

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
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(funcall 'org-priority force-direction)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

(defun org-get-tags-at (&optional pos)
  "Get a list of all headline tags applicable at POS.
POS defaults to point.  If tags are inherited, the list contains
the targets in the same sequence as the headlines appear, i.e.
the tags of the current headline come last."
  (interactive)
  (let (tags lastpos)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (or pos (point)))
	(save-match-data
	  (org-back-to-heading t)
	  (condition-case nil
	      (while (not (equal lastpos (point)))
		(setq lastpos (point))
		(if (looking-at (org-re "[^\r\n]+?:\\([[:alnum:]_@:]+\\):[ \t]*$"))
		    (setq tags (append (org-split-string
					(org-match-string-no-properties 1) ":")
				       tags)))
	      (or org-use-tag-inheritance (error ""))
	      (org-up-heading-all 1))
	    (error nil))))
      tags)))

;; FIXME: should fix the tags property of the agenda line.
(defun org-agenda-set-tags ()
  "Set tags for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (if (and (org-region-active-p) (interactive-p))
      (call-interactively 'org-change-tag-in-region)
    (org-agenda-show)   ;;; FIXME This is a stupid hack and should not be needed
    (let* ((hdmarker (or (get-text-property (point) 'org-hd-marker)
			 (org-agenda-error)))
	   (buffer (marker-buffer hdmarker))
	   (pos (marker-position hdmarker))
	   (inhibit-read-only t)
	   newhead)
      (org-with-remote-undo buffer
	(with-current-buffer buffer
	  (widen)
	  (goto-char pos)
	  (save-excursion
	    (org-show-context 'agenda))
	  (save-excursion
	    (and (outline-next-heading)
		 (org-flag-heading nil)))   ; show the next heading
	  (goto-char pos)
	  (call-interactively 'org-set-tags)
	  (end-of-line 1)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker)
	(beginning-of-line 1)))))

(defun org-agenda-toggle-archive-tag ()
  "Toggle the archive tag for the current entry."
  (interactive)
  (org-agenda-check-no-diary)
  (org-agenda-show)   ;;; FIXME This is a stupid hack and should not be needed
  (let* ((hdmarker (or (get-text-property (point) 'org-hd-marker)
                       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(call-interactively 'org-toggle-archive-tag)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

(defun org-agenda-date-later (arg &optional what)
  "Change the date of this item to one day later."
  (interactive "p")
  (org-agenda-check-type t 'agenda 'timeline)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
     (with-current-buffer buffer
       (widen)
       (goto-char pos)
       (if (not (org-at-timestamp-p))
	   (error "Cannot find time stamp"))
       (org-timestamp-change arg (or what 'day)))
     (org-agenda-show-new-time marker org-last-changed-timestamp))
    (message "Time stamp changed to %s" org-last-changed-timestamp)))

(defun org-agenda-date-earlier (arg &optional what)
  "Change the date of this item to one day earlier."
  (interactive "p")
  (org-agenda-date-later (- arg) what))

(defun org-agenda-show-new-time (marker stamp)
  "Show new date stamp via text properties."
  ;; We use text properties to make this undoable
  (let ((inhibit-read-only t))
    (setq stamp (concat " => " stamp))
    (save-excursion
      (goto-char (point-max))
      (while (not (bobp))
	(when (equal marker (get-text-property (point) 'org-marker))
	  (move-to-column (- (window-width) (length stamp)) t)
          (if (featurep 'xemacs)
	      ;; Use `duplicable' property to trigger undo recording
              (let ((ex (make-extent nil nil))
                    (gl (make-glyph stamp)))
                (set-glyph-face gl 'secondary-selection)
                (set-extent-properties
                 ex (list 'invisible t 'end-glyph gl 'duplicable t))
                (insert-extent ex (1- (point)) (point-at-eol)))
            (add-text-properties
             (1- (point)) (point-at-eol)
	     (list 'display (org-add-props stamp nil
			      'face 'secondary-selection))))
	  (beginning-of-line 1))
	(beginning-of-line 0)))))

(defun org-agenda-date-prompt (arg)
  "Change the date of this item.  Date is prompted for, with default today.
The prefix ARG is passed to the `org-time-stamp' command and can therefore
be used to request time specification in the time stamp."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(if (not (org-at-timestamp-p))
	    (error "Cannot find time stamp"))
	(org-time-stamp arg)
	(message "Time stamp changed to %s" org-last-changed-timestamp)))))

(defun org-agenda-schedule (arg)
  "Schedule the item at point."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (org-insert-labeled-timestamps-at-point nil)
	 ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-schedule))
	(message "Item scheduled for %s" ts)))))

(defun org-agenda-deadline (arg)
  "Schedule the item at point."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (org-insert-labeled-timestamps-at-point nil)
	 ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-deadline))
	(message "Deadline for this item set to %s" ts)))))

(defun org-get-heading ()
  "Return the heading of the current entry, without the stars."
  (save-excursion
    (org-back-to-heading t)
    (if (looking-at "\\*+[ \t]+\\([^\r\n]*\\)")	(match-string 1) "")))

(defun org-agenda-clock-in (&optional arg)
  "Start the clock on the currently selected item."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (pos (marker-position marker)))
    (org-with-remote-undo (marker-buffer marker)
      (with-current-buffer (marker-buffer marker)
	(widen)
	(goto-char pos)
	(org-clock-in)))))

(defun org-agenda-clock-out (&optional arg)
  "Stop the currently running clock."
  (interactive "P")
  (unless (marker-buffer org-clock-marker)
    (error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-out)))

(defun org-agenda-clock-cancel (&optional arg)
  "Cancel the currently running clock."
  (interactive "P")
  (unless (marker-buffer org-clock-marker)
    (error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-cancel)))

(defun org-agenda-diary-entry ()
  "Make a diary entry, like the `i' command from the calendar.
All the standard commands work: block, weekly etc."
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
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
;	 (buf (get-file-buffer (substitute-in-file-name diary-file)))
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
  (org-agenda-check-type t 'agenda 'timeline)
  (require 'diary-lib)
  (unless (get-text-property (point) 'day)
    (error "Don't know which date to use for calendar command"))
  (let* ((oldf (symbol-function 'calendar-cursor-to-date))
	 (point (point))
	 (date (calendar-gregorian-from-absolute
		(get-text-property point 'day)))
         ;; the following 3 vars are needed in the calendar
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
  (org-agenda-check-type t 'agenda 'timeline)
  (let* ((day (or (get-text-property (point) 'day)
		  (error "Don't know which date to open in calendar")))
	 (date (calendar-gregorian-from-absolute day))
	 (calendar-move-hook nil)
	 (view-calendar-holidays-initially nil)
	 (view-diary-entries-initially nil))
    (calendar)
    (calendar-goto-date date)))

(defun org-calendar-goto-agenda ()
  "Compute the Org-mode agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'."
  (interactive)
  (org-agenda-list nil (calendar-absolute-from-gregorian
			(calendar-cursor-to-date))
		   nil))

(defun org-agenda-convert-date ()
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
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
	     "Baha'i:     " (calendar-bahai-date-string date) " (until sunset)\n"
	     "Mayan:      " (calendar-mayan-date-string date) "\n"
	     "Coptic:     " (calendar-coptic-date-string date) "\n"
	     "Ethiopic:   " (calendar-ethiopic-date-string date) "\n"
	     "Persian:    " (calendar-persian-date-string date) "\n"
	     "Chinese:    " (calendar-chinese-date-string date) "\n"))
    (with-output-to-temp-buffer "*Dates*"
      (princ s))
    (if (fboundp 'fit-window-to-buffer)
	(fit-window-to-buffer (get-buffer-window "*Dates*")))))


;;;; Embedded LaTeX

(defvar org-cdlatex-mode-map (make-sparse-keymap)
  "Keymap for the minor `org-cdlatex-mode'.")

(org-defkey org-cdlatex-mode-map "_" 'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map "^" 'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map "`" 'cdlatex-math-symbol)
(org-defkey org-cdlatex-mode-map "'" 'org-cdlatex-math-modify)
(org-defkey org-cdlatex-mode-map "\C-c{" 'cdlatex-environment)

(defvar org-cdlatex-texmathp-advice-is-done nil
  "Flag remembering if we have applied the advice to texmathp already.")

(define-minor-mode org-cdlatex-mode
  "Toggle the minor `org-cdlatex-mode'.
This mode supports entering LaTeX environment and math in LaTeX fragments
in Org-mode.
\\{org-cdlatex-mode-map}"
  nil " OCDL" nil
  (when org-cdlatex-mode (require 'cdlatex))
  (unless org-cdlatex-texmathp-advice-is-done
    (setq org-cdlatex-texmathp-advice-is-done t)
    (defadvice texmathp (around org-math-always-on activate)
      "Always return t in org-mode buffers.
This is because we want to insert math symbols without dollars even outside
the LaTeX math segments.  If Orgmode thinks that point is actually inside
en embedded LaTeX fragement, let texmathp do its job.
\\[org-cdlatex-mode-map]"
      (interactive)
      (let (p)
	(cond
	 ((not (org-mode-p)) ad-do-it)
	 ((eq this-command 'cdlatex-math-symbol)
	  (setq ad-return-value t
		texmathp-why '("cdlatex-math-symbol in org-mode" . 0)))
	 (t
	  (let ((p (org-inside-LaTeX-fragment-p)))
	    (if (and p (member (car p) (plist-get org-format-latex-options :matchers)))
		(setq ad-return-value t
		      texmathp-why '("Org-mode embedded math" . 0))
	      (if p ad-do-it)))))))))

(defun turn-on-org-cdlatex ()
  "Unconditionally turn on `org-cdlatex-mode'."
  (org-cdlatex-mode 1))

(defun org-inside-LaTeX-fragment-p ()
  "Test if point is inside a LaTeX fragment.
I.e. after a \\begin, \\(, \\[, $, or $$, without the corresponding closing
sequence appearing also before point.
Even though the matchers for math are configurable, this function assumes
that \\begin, \\(, \\[, and $$ are always used.  Only the single dollar
delimiters are skipped when they have been removed by customization.
The return value is nil, or a cons cell with the delimiter and
and the position of this delimiter.

This function does a reasonably good job, but can locally be fooled by
for example currency specifications.  For example it will assume being in
inline math after \"$22.34\".  The LaTeX fragment formatter will only format
fragments that are properly closed, but during editing, we have to live
with the uncertainty caused by missing closing delimiters.  This function
looks only before point, not after."
  (catch 'exit
    (let ((pos (point))
	  (dodollar (member "$" (plist-get org-format-latex-options :matchers)))
	  (lim (progn
		 (re-search-backward (concat "^\\(" paragraph-start "\\)") nil t)
		 (point)))
	  dd-on str (start 0) m re)
      (goto-char pos)
      (when dodollar
	(setq str (concat (buffer-substring lim (point)) "\000 X$.")
	      re (nth 1 (assoc "$" org-latex-regexps)))
	(while (string-match re str start)
	  (cond
	   ((= (match-end 0) (length str))
	    (throw 'exit (cons "$" (+ lim (match-beginning 0) 1))))
	   ((= (match-end 0) (- (length str) 5))
	    (throw 'exit nil))
	   (t (setq start (match-end 0))))))
      (when (setq m (re-search-backward "\\(\\\\begin{[^}]*}\\|\\\\(\\|\\\\\\[\\)\\|\\(\\\\end{[^}]*}\\|\\\\)\\|\\\\\\]\\)\\|\\(\\$\\$\\)" lim t))
	(goto-char pos)
	(and (match-beginning 1) (throw 'exit (cons (match-string 1) m)))
	(and (match-beginning 2) (throw 'exit nil))
	;; count $$
	(while (re-search-backward "\\$\\$" lim t)
	  (setq dd-on (not dd-on)))
	(goto-char pos)
	(if dd-on (cons "$$" m))))))


(defun org-try-cdlatex-tab ()
  "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `org-cdlatex-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
  (when org-cdlatex-mode
    (cond
     ((save-excursion
	(skip-chars-backward "a-zA-Z0-9*")
	(skip-chars-backward " \t")
	(bolp))
      (cdlatex-tab) t)
     ((org-inside-LaTeX-fragment-p)
      (cdlatex-tab) t)
     (t nil))))

(defun org-cdlatex-underscore-caret (&optional arg)
  "Execute `cdlatex-sub-superscript' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-sub-superscript)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-cdlatex-math-modify (&optional arg)
  "Execute `cdlatex-math-modify' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-math-modify)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defvar org-latex-fragment-image-overlays nil
  "List of overlays carrying the images of latex fragments.")
(make-variable-buffer-local 'org-latex-fragment-image-overlays)

(defun org-remove-latex-fragment-image-overlays ()
  "Remove all overlays with LaTeX fragment images in current buffer."
  (mapc 'org-delete-overlay org-latex-fragment-image-overlays)
  (setq org-latex-fragment-image-overlays nil))

(defun org-preview-latex-fragment (&optional subtree)
  "Preview the LaTeX fragment at point, or all locally or globally.
If the cursor is in a LaTeX fragment, create the image and overlay
it over the source code.  If there is no fragment at point, display
all fragments in the current text, from one headline to the next.  With
prefix SUBTREE, display all fragments in the current subtree.  With a
double prefix `C-u C-u', or when the cursor is before the first headline,
display all fragments in the buffer.
The images can be removed again with \\[org-ctrl-c-ctrl-c]."
  (interactive "P")
  (org-remove-latex-fragment-image-overlays)
  (save-excursion
    (save-restriction
      (let (beg end at msg)
	(cond
	 ((or (equal subtree '(16))
	      (not (save-excursion
		     (re-search-backward (concat "^" outline-regexp) nil t))))
	  (setq beg (point-min) end (point-max)
		msg "Creating images for buffer...%s"))
	 ((equal subtree '(4))
	  (org-back-to-heading)
	  (setq beg (point) end (org-end-of-subtree t)
		msg "Creating images for subtree...%s"))
	 (t
	  (if (setq at (org-inside-LaTeX-fragment-p))
	      (goto-char (max (point-min) (- (cdr at) 2)))
	    (org-back-to-heading))
	  (setq beg (point) end (progn (outline-next-heading) (point))
		msg (if at "Creating image...%s"
		      "Creating images for entry...%s"))))
	(message msg "")
	(narrow-to-region beg end)
	(goto-char beg)
	(org-format-latex
	 (concat "ltxpng/" (file-name-sans-extension
			    (file-name-nondirectory
			     buffer-file-name)))
	 default-directory 'overlays msg at 'forbuffer)
      (message msg "done.  Use `C-c C-c' to remove images.")))))

(defvar org-latex-regexps
  '(("begin" "^[ \t]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)[^\000]+?\\\\end{\\2}\\)" 1 t)
    ;; ("$" "\\([ 	(]\\|^\\)\\(\\(\\([$]\\)\\([^ 	\r\n,.$].*?\\(\n.*?\\)\\{0,5\\}[^ 	\r\n,.$]\\)\\4\\)\\)\\([ 	.,?;:'\")]\\|$\\)" 2 nil)
    ;; \000 in the following regex is needed for org-inside-LaTeX-fragment-p
    ("$" "\\([^$]\\)\\(\\(\\$\\([^ 	\r\n,;.$][^$\n\r]*?\\(\n[^$\n\r]*?\\)\\{0,2\\}[^ 	\r\n,.$]\\)\\$\\)\\)\\([ 	.,?;:'\")\000]\\|$\\)" 2 nil)
    ("\\(" "\\\\([^\000]*?\\\\)" 0 nil)
    ("\\[" "\\\\\\[[^\000]*?\\\\\\]" 0 t)
    ("$$" "\\$\\$[^\000]*?\\$\\$" 0 t))
  "Regular expressions for matching embedded LaTeX.")

(defun org-format-latex (prefix &optional dir overlays msg at forbuffer)
  "Replace LaTeX fragments with links to an image, and produce images."
  (if (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (let* ((prefixnodir (file-name-nondirectory prefix))
	 (absprefix (expand-file-name prefix dir))
	 (todir (file-name-directory absprefix))
	 (opt org-format-latex-options)
	 (matchers (plist-get opt :matchers))
	 (re-list org-latex-regexps)
	 (cnt 0) txt link beg end re e checkdir
	 m n block linkfile movefile ov)
    ;; Check if there are old images files with this prefix, and remove them
    (when (file-directory-p todir)
      (mapc 'delete-file
	    (directory-files
	     todir 'full
	     (concat (regexp-quote prefixnodir) "_[0-9]+\\.png$"))))
    ;; Check the different regular expressions
    (while (setq e (pop re-list))
      (setq m (car e) re (nth 1 e) n (nth 2 e)
	    block (if (nth 3 e) "\n\n" ""))
      (when (member m matchers)
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (when (or (not at) (equal (cdr at) (match-beginning n)))
	    (setq txt (match-string n)
		  beg (match-beginning n) end (match-end n)
		  cnt (1+ cnt)
		  linkfile (format "%s_%04d.png" prefix cnt)
		  movefile (format "%s_%04d.png" absprefix cnt)
		  link (concat block "[[file:" linkfile "]]" block))
	    (if msg (message msg cnt))
	    (goto-char beg)
	    (unless checkdir ; make sure the directory exists
	      (setq checkdir t)
	      (or (file-directory-p todir) (make-directory todir)))
	    (org-create-formula-image
	     txt movefile opt forbuffer)
	    (if overlays
		(progn
		  (setq ov (org-make-overlay beg end))
		  (if (featurep 'xemacs)
		      (progn
			(org-overlay-put ov 'invisible t)
			(org-overlay-put
			 ov 'end-glyph
			 (make-glyph (vector 'png :file movefile))))
		    (org-overlay-put
		     ov 'display
		     (list 'image :type 'png :file movefile :ascent 'center)))
		  (push ov org-latex-fragment-image-overlays)
		  (goto-char end))
	      (delete-region beg end)
	      (insert link))))))))

;; This function borrows from Ganesh Swami's latex2png.el
(defun org-create-formula-image (string tofile options buffer)
  (let* ((tmpdir (if (featurep 'xemacs)
		     (temp-directory)
		   temporary-file-directory))
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (dvifile (concat texfilebase ".dvi"))
	 (pngfile (concat texfilebase ".png"))
	 (fnh (face-attribute 'default :height nil))
	 (scale (or (plist-get options (if buffer :scale :html-scale)) 1.0))
	 (dpi (number-to-string (* scale (floor (* 0.9 (if buffer fnh 140.))))))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent")))
    (if (eq fg 'default) (setq fg (org-dvipng-color :foreground)))
    (if (eq bg 'default) (setq bg (org-dvipng-color :background)))
    (with-temp-file texfile
      (insert org-format-latex-header
	      "\n\\begin{document}\n" string "\n\\end{document}\n"))
    (let ((dir default-directory))
      (condition-case nil
	  (progn
	    (cd tmpdir)
	    (call-process "latex" nil nil nil texfile))
	(error nil))
      (cd dir))
    (if (not (file-exists-p dvifile))
	(progn (message "Failed to create dvi file from %s" texfile) nil)
      (call-process "dvipng" nil nil nil
		    "-E" "-fg" fg "-bg" bg
                    "-D" dpi
		    ;;"-x" scale "-y" scale
		    "-T" "tight"
		    "-o" pngfile
		    dvifile)
      (if (not (file-exists-p pngfile))
	  (progn (message "Failed to create png file from %s" texfile) nil)
	;; Use the requested file name and clean up
	(copy-file pngfile tofile 'replace)
	(loop for e in '(".dvi" ".tex" ".aux" ".log" ".png") do
	      (delete-file (concat texfilebase e)))
	pngfile))))

(defun org-dvipng-color (attr)
  "Return an rgb color specification for dvipng."
  (apply 'format "rgb %s %s %s"
	 (mapcar 'org-normalize-color
		 (color-values (face-attribute 'default attr nil)))))

(defun org-normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))

;;;; Exporting

;;; Variables, constants, and parameter plists

(defconst org-level-max 20)

(defvar org-export-html-preamble nil
  "Preamble, to be inserted just after <body>.  Set by publishing functions.")
(defvar org-export-html-postamble nil
  "Preamble, to be inserted just before </body>.  Set by publishing functions.")
(defvar org-export-html-auto-preamble t
  "Should default preamble be inserted?  Set by publishing functions.")
(defvar org-export-html-auto-postamble t
  "Should default postamble be inserted?  Set by publishing functions.")
(defvar org-current-export-file nil) ; dynamically scoped parameter
(defvar org-current-export-dir nil) ; dynamically scoped parameter


(defconst org-export-plist-vars
  '((:language             . org-export-default-language)
    (:customtime           . org-display-custom-times)
    (:headline-levels      . org-export-headline-levels)
    (:section-numbers      . org-export-with-section-numbers)
    (:table-of-contents    . org-export-with-toc)
    (:preserve-breaks      . org-export-preserve-breaks)
    (:archived-trees       . org-export-with-archived-trees)
    (:emphasize            . org-export-with-emphasize)
    (:sub-superscript      . org-export-with-sub-superscripts)
    (:footnotes            . org-export-with-footnotes)
    (:property-drawer      . org-export-with-property-drawer)
    (:TeX-macros           . org-export-with-TeX-macros)
    (:LaTeX-fragments      . org-export-with-LaTeX-fragments)
    (:skip-before-1st-heading . org-export-skip-text-before-1st-heading)
    (:fixed-width          . org-export-with-fixed-width)
    (:timestamps           . org-export-with-timestamps)
    (:author-info          . org-export-author-info)
    (:time-stamp-file      . org-export-time-stamp-file)
    (:tables               . org-export-with-tables)
    (:table-auto-headline  . org-export-highlight-first-table-line)
    (:style                . org-export-html-style)
    (:agenda-style         . org-agenda-export-html-style) ;; FIXME: Does this work????
    (:convert-org-links    . org-export-html-link-org-files-as-html)
    (:inline-images        . org-export-html-inline-images)
    (:expand-quoted-html   . org-export-html-expand)
    (:timestamp            . org-export-html-with-timestamp)
    (:publishing-directory . org-export-publishing-directory)
    (:preamble             . org-export-html-preamble)
    (:postamble            . org-export-html-postamble)
    (:auto-preamble        . org-export-html-auto-preamble)
    (:auto-postamble       . org-export-html-auto-postamble)
    (:author               . user-full-name)
    (:email                . user-mail-address)))

(defun org-default-export-plist ()
  "Return the property list with default settings for the export variables."
  (let ((l org-export-plist-vars) rtn e)
    (while (setq e (pop l))
      (setq rtn (cons (car e) (cons (symbol-value (cdr e)) rtn))))
    rtn))

(defun org-infile-export-plist ()
  "Return the property list with file-local settings for export."
  (save-excursion
    (goto-char 0)
    (let ((re (org-make-options-regexp
	       '("TITLE" "AUTHOR" "DATE" "EMAIL" "TEXT" "OPTIONS" "LANGUAGE")))
	  p key val text options)
      (while (re-search-forward re nil t)
	(setq key (org-match-string-no-properties 1)
	      val (org-match-string-no-properties 2))
	(cond
	 ((string-equal key "TITLE") (setq p (plist-put p :title val)))
	 ((string-equal key "AUTHOR")(setq p (plist-put p :author val)))
	 ((string-equal key "EMAIL") (setq p (plist-put p :email val)))
	 ((string-equal key "DATE") (setq p (plist-put p :date val)))
	 ((string-equal key "LANGUAGE") (setq p (plist-put p :language val)))
	 ((string-equal key "TEXT")
	  (setq text (if text (concat text "\n" val) val)))
	 ((string-equal key "OPTIONS") (setq options val))))
      (setq p (plist-put p :text text))
      (when options
	(let ((op '(("H"     . :headline-levels)
		    ("num"   . :section-numbers)
		    ("toc"   . :table-of-contents)
		    ("\\n"   . :preserve-breaks)
		    ("@"     . :expand-quoted-html)
		    (":"     . :fixed-width)
		    ("|"     . :tables)
		    ("^"     . :sub-superscript)
		    ("f"     . :footnotes)
		    ("p"     . :property-drawer)
		    ("*"     . :emphasize)
		    ("TeX"   . :TeX-macros)
		    ("LaTeX" . :LaTeX-fragments)
		    ("skip"  . :skip-before-1st-heading)
		    ("author" . :author-info)
		    ("timestamp" . :time-stamp-file)))
	      o)
	  (while (setq o (pop op))
	    (if (string-match (concat (regexp-quote (car o))
				      ":\\([^ \t\n\r;,.]*\\)")
			      options)
		(setq p (plist-put p (cdr o)
				   (car (read-from-string
					 (match-string 1 options)))))))))
      p)))

(defun org-export-directory (type plist)
  (let* ((val (plist-get plist :publishing-directory))
	 (dir (if (listp val)
		  (or (cdr (assoc type val)) ".")
		val)))
    dir))

(defun org-skip-comments (lines)
  "Skip lines starting with \"#\" and subtrees starting with COMMENT."
  (let ((re1 (concat "^\\(\\*+\\)[ \t]+" org-comment-string))
	(re2 "^\\(\\*+\\)[ \t\n\r]")
	(case-fold-search nil)
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
       ((and org-export-table-remove-special-lines
	     (string-match "^[ \t]*|" line)
	     (or (string-match "^[ \t]*| *[!_^] *|" line)
		 (and (string-match "| *<[0-9]+> *|" line)
		      (not (string-match "| *[^ <|]" line)))))
	;; a special table line that should be removed
	)
       (t (setq rtn (cons line rtn)))))
    (nreverse rtn)))

(defun org-export (&optional arg)
  (interactive)
  (let ((help "[t]   insert the export option template
\[v]   limit export to visible part of outline tree

\[a] export as ASCII

\[h] export as HTML
\[H] export as HTML to temporary buffer
\[R] export region as HTML
\[b] export as HTML and browse immediately
\[x] export as XOXO

\[l] export as LaTeX
\[L] export as LaTeX to temporary buffer

\[i] export current file as iCalendar file
\[I] export all agenda files as iCalendar files
\[c] export agenda files into combined iCalendar file

\[F] publish current file
\[P] publish current project
\[X] publish... (project will be prompted for)
\[A] publish all projects")
	(cmds
	 '((?t . org-insert-export-options-template)
	   (?v . org-export-visible)
	   (?a . org-export-as-ascii)
	   (?h . org-export-as-html)
	   (?b . org-export-as-html-and-open)
	   (?H . org-export-as-html-to-buffer)
	   (?R . org-export-region-as-html)
	   (?x . org-export-as-xoxo)
	   (?l . org-export-as-latex)
	   (?L . org-export-as-latex-to-buffer)
	   (?i . org-export-icalendar-this-file)
	   (?I . org-export-icalendar-all-agenda-files)
	   (?c . org-export-icalendar-combine-agenda-files)
	   (?F . org-publish-current-file)
	   (?P . org-publish-current-project)
	   (?X . org-publish)
	   (?A . org-publish-all)))
	r1 r2 ass)
    (save-window-excursion
      (delete-other-windows)
      (with-output-to-temp-buffer "*Org Export/Publishing Help*"
	(princ help))
      (message "Select command: ")
      (setq r1 (read-char-exclusive)))
    (setq r2 (if (< r1 27) (+ r1 96) r1))
    (if (setq ass (assq r2 cmds))
	(call-interactively (cdr ass))
      (error "No command associated with key %c" r1))))

(defconst org-html-entities
  '(("nbsp")
    ("iexcl")
    ("cent")
    ("pound")
    ("curren")
    ("yen")
    ("brvbar")
    ("vert" . "&#124;")
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
    ("smile"."&#9786;") ("blacksmile"."&#9787;") ("sad"."&#9785;")
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

;;; General functions for all backends

(defun org-cleaned-string-for-export (string &rest parameters)
  "Cleanup a buffer STRING so that links can be created safely."
  (interactive)
  (let* ((re-radio (and org-target-link-regexp
			(concat "\\([^<]\\)\\(" org-target-link-regexp "\\)")))
	 (re-plain-link (concat "\\([^[<]\\)" org-plain-link-re))
	 (re-angle-link (concat "\\([^[]\\)" org-angle-link-re))
	 (re-archive (concat ":" org-archive-tag ":"))
	 (re-quote (concat "^\\*+[ \t]+" org-quote-string "\\>"))
	 (re-commented (concat "^\\*+[ \t]+" org-comment-string "\\>"))
	 (htmlp (plist-get parameters :for-html))
	 (asciip (plist-get parameters :for-ascii))
	 (latexp (plist-get parameters :for-LaTeX))
	 (commentsp (plist-get parameters :comments))
	 (archived-trees (plist-get parameters :archived-trees))
	 (inhibit-read-only t)
	 (outline-regexp "\\*+ ")
	 a b xx
	 rtn p)
    (with-current-buffer (get-buffer-create " org-mode-tmp")
      (erase-buffer)
      (insert string)
      ;; Remove license-to-kill stuff
      (while (setq p (text-property-any (point-min) (point-max)
					:org-license-to-kill t))
	(delete-region p (next-single-property-change p :org-license-to-kill)))

      (let ((org-inhibit-startup t)) (org-mode))
      (untabify (point-min) (point-max))

      ;; Get the correct stuff before the first headline
      (when (plist-get parameters :skip-before-1st-heading)
	(goto-char (point-min))
	(when (re-search-forward "^\\*+[ \t]" nil t)
	  (delete-region (point-min) (match-beginning 0))
	  (goto-char (point-min))
	  (insert "\n")))
      (when (plist-get parameters :add-text)
	(goto-char (point-min))
	(insert (plist-get parameters :add-text) "\n"))

      ;; Get rid of archived trees
      (when (not (eq archived-trees t))
	(goto-char (point-min))
	(while (re-search-forward re-archive nil t)
	  (if (not (org-on-heading-p t))
	      (org-end-of-subtree t)
	    (beginning-of-line 1)
	    (setq a (if archived-trees
			(1+ (point-at-eol)) (point))
		  b (org-end-of-subtree t))
	    (if (> b a) (delete-region a b)))))

      ;; Get rid of property drawers
      (unless org-export-with-property-drawer
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*\n\\([^@]*?\n\\)?[ \t]*:END:[ \t]*\n" nil t)
	  (replace-match "")))

      ;; Find targets in comments and move them out of comments,
      ;; but mark them as targets that should be invisible
      (goto-char (point-min))
      (while (re-search-forward "^#.*?\\(<<<?[^>\r\n]+>>>?\\).*" nil t)
	(replace-match "\\1(INVISIBLE)"))

      ;; Protect backend specific stuff, throw away the others.
      (goto-char (point-min))
      (let ((formatters
	     `((,htmlp "HTML" "BEGIN_HTML" "END_HTML")
	       (,asciip "ASCII" "BEGIN_ASCII" "END_ASCII")
	       (,latexp "LaTeX" "BEGIN_LaTeX" "END_LaTeX")))
	    fmt)
	(while (re-search-forward "^[ \t]*:.*\\(\n[ \t]*:.*\\)*" nil t)
	  (add-text-properties (match-beginning 0) (match-end 0)
			       '(org-protected t)))
	(while formatters
	  (setq fmt (pop formatters))
	  (when (car fmt)
	    (goto-char (point-min))
	    (while (re-search-forward (concat "^#\\+" (cadr fmt) 
					      ":[ \t]*\\(.*\\)") nil t)
	      (replace-match "\\1" t)
	      (add-text-properties
	       (point-at-bol) (min (1+ (point-at-eol)) (point-max))
	       '(org-protected t))))
	  (goto-char (point-min))
	  (while (re-search-forward
		  (concat "^#\\+" 
			  (caddr fmt) "\\>.*\\(\\(\n.*\\)*?\n\\)#\\+"
			  (cadddr fmt) "\\>.*\n?") nil t)
	    (if (car fmt)
		(add-text-properties (match-beginning 1) (1+ (match-end 1))
				     '(org-protected t))
	      (delete-region (match-beginning 0) (match-end 0))))))

      ;; Protect quoted subtrees
      (goto-char (point-min))
      (while (re-search-forward re-quote nil t)
	(goto-char (match-beginning 0))
	(end-of-line 1)
	(add-text-properties (point) (org-end-of-subtree t)
			     '(org-protected t)))

      ;; Remove subtrees that are commented
      (goto-char (point-min))
      (while (re-search-forward re-commented nil t)
	(goto-char (match-beginning 0))
	(delete-region (point) (org-end-of-subtree t)))

      ;; Remove special table lines
      (when org-export-table-remove-special-lines
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*|" nil t)
	  (beginning-of-line 1)
	  (if (or (looking-at "[ \t]*| *[!_^] *|")
		  (and (looking-at ".*?| *<[0-9]+> *|")
		       (not (looking-at ".*?| *[^ <|]"))))
	      (delete-region (max (point-min) (1- (point-at-bol)))
			     (point-at-eol))
	    (end-of-line 1))))

      ;; Specific LaTeX stuff
      (when latexp
	(require 'org-export-latex nil t)
	(org-export-latex-cleaned-string commentsp))

      ;; Specific HTML stuff
      (when htmlp
	;; Convert LaTeX fragments to images
	(when (plist-get parameters :LaTeX-fragments)
	  (org-format-latex
	   (concat "ltxpng/" (file-name-sans-extension
			      (file-name-nondirectory
			       org-current-export-file)))
	   org-current-export-dir nil "Creating LaTeX image %s"))
	(message "Exporting..."))

      ;; Remove or replace comments
      (goto-char (point-min))
      (while (re-search-forward "^#\\(.*\n?\\)" nil t)
	(if commentsp
	    (progn (add-text-properties
		    (match-beginning 0) (match-end 0) '(org-protected t))
		   (replace-match (format commentsp (match-string 1)) t t))
	  (replace-match "")))

      ;; Find matches for radio targets and turn them into internal links
      (goto-char (point-min))
      (when re-radio
	(while (re-search-forward re-radio nil t)
	  (org-if-unprotected
	   (replace-match "\\1[[\\2]]"))))

      ;; Find all links that contain a newline and put them into a single line
      (goto-char (point-min))
      (while (re-search-forward "\\(\\(\\[\\|\\]\\)\\[[^]]*?\\)[ \t]*\n[ \t]*\\([^]]*\\]\\(\\[\\|\\]\\)\\)" nil t)
	(org-if-unprotected
	 (replace-match "\\1 \\3")
	 (goto-char (match-beginning 0))))


      ;; Normalize links: Convert angle and plain links into bracket links
      ;; Expand link abbreviations
      (goto-char (point-min))
      (while (re-search-forward re-plain-link nil t)
	(goto-char (1- (match-end 0)))
	(org-if-unprotected
	 (let* ((s (concat (match-string 1) "[[" (match-string 2)
			   ":" (match-string 3) "]]")))
	   ;; added 'org-link face to links
	   (put-text-property 0 (length s) 'face 'org-link s)
	   (replace-match s t t))))
      (goto-char (point-min))
      (while (re-search-forward re-angle-link nil t)
	(goto-char (1- (match-end 0)))
	(org-if-unprotected
	 (let* ((s (concat (match-string 1) "[[" (match-string 2)
			   ":" (match-string 3) "]]")))
	   (put-text-property 0 (length s) 'face 'org-link s)
	   (replace-match s t t))))
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
	(org-if-unprotected
	 (let* ((s (concat "[[" (setq xx (save-match-data
					   (org-link-expand-abbrev (match-string 1))))
			   "]"
			   (if (match-end 3)
			       (match-string 2)
			     (concat "[" xx "]"))
			   "]")))
	   (put-text-property 0 (length s) 'face 'org-link s)
	   (replace-match s t t))))

      ;; Find multiline emphasis and put them into single line
      (when (plist-get  parameters :emph-multiline)
	(goto-char (point-min))
	(while (re-search-forward org-emph-re nil t)
	  (if (not (= (char-after (match-beginning 3))
		      (char-after (match-beginning 4))))
	      (org-if-unprotected
	       (subst-char-in-region (match-beginning 0) (match-end 0)
				     ?\n ?\  t)
	       (goto-char (1- (match-end 0))))
	    (goto-char (1+ (match-beginning 0))))))

      (setq rtn (buffer-string)))
    (kill-buffer " org-mode-tmp")
    rtn))

(defun org-export-grab-title-from-buffer ()
  "Get a title for the current document, from looking at the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((end (save-excursion (outline-next-heading) (point))))
	(when (re-search-forward "^[ \t]*[^|# \t\r\n].*\n" end t)
	  ;; Mark the line so that it will not be exported as normal text.
	  (org-unmodified
	   (add-text-properties (match-beginning 0) (match-end 0)
				(list :org-license-to-kill t)))
	  ;; Return the title string
	  (org-trim (match-string 0)))))))

(defun org-export-get-title-from-subtree ()
  "Return subtree title and exclude it from export."
  (let (title (m (mark)))
    (save-excursion
      (goto-char (region-beginning))
      (when (and (org-at-heading-p)
		 (>= (org-end-of-subtree t t) (region-end)))
	;; This is a subtree, we take the title from the first heading
	(goto-char (region-beginning))
	(looking-at org-todo-line-regexp)
	(setq title (match-string 3))
	(org-unmodified
	 (add-text-properties (point) (1+ (point-at-eol))
			      (list :org-license-to-kill t)))))
    title))
	 
(defun org-solidify-link-text (s &optional alist)
  "Take link text and make a safe target out of it."
  (save-match-data
    (let* ((rtn
	    (mapconcat
	     'identity
	     (org-split-string s "[ \t\r\n]+") "--"))
	   (a (assoc rtn alist)))
      (or (cdr a) rtn))))

(defun org-get-min-level (lines)
  "Get the minimum level in LINES."
  (let ((re "^\\(\\*+\\) ") l min)
    (catch 'exit
      (while (setq l (pop lines))
	(if (string-match re l)
	    (throw 'exit (org-tr-level (length (match-string 1 l))))))
      1)))

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
	  (setq string (replace-match "" t nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
	  (setq string (replace-match "" t nil string))))
    string))

;;; ASCII export

(defvar org-last-level nil) ; dynamically scoped variable
(defvar org-min-level nil) ; dynamically scoped variable
(defvar org-levels-open nil) ; dynamically scoped parameter
(defvar org-ascii-current-indentation nil) ; For communication

(defun org-export-as-ascii (arg)
  "Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	 (region-p (org-region-active-p))
	 (subtree-p
	  (when region-p
	    (save-excursion
	      (goto-char (region-beginning))
	      (and (org-at-heading-p)
		   (>= (org-end-of-subtree t t) (region-end))))))
	 (custom-times org-display-custom-times)
	 (org-ascii-current-indentation '(0 . 0))
	 (level 0) line txt
	 (umax nil)
	 (umax-toc nil)
	 (case-fold-search nil)
         (filename (concat (file-name-as-directory
			    (org-export-directory :ascii opt-plist))
			   (file-name-sans-extension
			    (or (and subtree-p
				     (org-entry-get (region-beginning)
						    "EXPORT_FILE_NAME" t))
				(file-name-nondirectory buffer-file-name)))
			   ".txt"))
	 (filename (if (equal (file-truename filename)
			      (file-truename buffer-file-name))
		       (concat filename ".txt")
		     filename))
	 (buffer (find-file-noselect filename))
	 (org-levels-open (make-vector org-level-max nil))
	 (odd org-odd-levels-only)
	 (date  (plist-get opt-plist :date))
	 (author      (plist-get opt-plist :author))
	 (title       (or (and subtree-p (org-export-get-title-from-subtree))
			  (plist-get opt-plist :title)
			  (and (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (file-name-sans-extension
			   (file-name-nondirectory buffer-file-name))))
	 (email       (plist-get opt-plist :email))
	 (language    (plist-get opt-plist :language))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
;	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]*" org-quote-string "\\>\\)"))
	 (todo nil)
	 (lang-words nil)
	 (region
	  (buffer-substring
	   (if (org-region-active-p) (region-beginning) (point-min))
	   (if (org-region-active-p) (region-end) (point-max))))
	 (lines (org-split-string
		 (org-cleaned-string-for-export
		  region
		  :for-ascii t
		  :skip-before-1st-heading
		  (plist-get opt-plist :skip-before-1st-heading)
		  :archived-trees
		  (plist-get opt-plist :archived-trees)
		  :add-text (plist-get opt-plist :text))
		 "[\r\n]")) ;; FIXME: why \r here???/
	 thetoc have-headings first-heading-pos
	 table-open table-buffer)

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (setq org-min-level (org-get-min-level lines))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (find-file-noselect filename)

    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (fundamental-mode)
    ;; create local variables for all options, to make sure all called
    ;; functions get the correct information
    (mapcar (lambda (x)
	      (set (make-local-variable (cdr x))
		   (plist-get opt-plist (car x))))
	    org-export-plist-vars)
    (org-set-local 'org-odd-levels-only odd)
    (setq umax (if arg (prefix-numeric-value arg)
		 org-export-headline-levels))
    (setq umax-toc (if (integerp org-export-with-toc)
		       (min org-export-with-toc umax)
		     umax))

    ;; File header
    (if title (org-insert-centered title ?=))
    (insert "\n")
    (if (and (or author email)
	     org-export-author-info)
	(insert (concat (nth 1 lang-words) ": " (or author "")
			(if email (concat " <" email ">") "")
			"\n")))

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date (current-time))))
     (date)
     (t (setq date (format-time-string "%Y/%m/%d %X" (current-time)))))

    (if (and date org-export-time-stamp-file)
	(insert (concat (nth 2 lang-words) ": " date"\n")))

    (insert "\n\n")

    (if org-export-with-toc
	(progn
	  (push (concat (nth 3 lang-words) "\n") thetoc)
	  (push (concat (make-string (length (nth 3 lang-words)) ?=) "\n") thetoc)
	  (mapcar '(lambda (line)
		     (if (string-match org-todo-line-regexp
				       line)
			 ;; This is a headline
			 (progn
			   (setq have-headings t)
			   (setq level (- (match-end 1) (match-beginning 1))
				 level (org-tr-level level)
				 txt (match-string 3 line)
				 todo
				 (or (and org-export-mark-todo-in-toc
					  (match-beginning 2)
					  (not (member (match-string 2 line)
						       org-done-keywords)))
					; TODO, not DONE
				     (and org-export-mark-todo-in-toc
					  (= level umax-toc)
					  (org-search-todo-below
					   line lines level))))
			   (setq txt (org-html-expand-for-ascii txt))

			   (if (and (memq org-export-with-tags '(not-in-toc nil))
				    (string-match
				     (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$")
				     txt))
			       (setq txt (replace-match "" t t txt)))
			   (if (string-match quote-re0 txt)
			       (setq txt (replace-match "" t t txt)))

			   (if org-export-with-section-numbers
			       (setq txt (concat (org-section-number level)
						 " " txt)))
			   (if (<= level umax-toc)
			       (progn
				 (push
				  (concat
				   (make-string
				    (* (max 0 (- level org-min-level)) 4) ?\ )
				   (format (if todo "%s (*)\n" "%s\n") txt))
				  thetoc)
				 (setq org-last-level level))
			     ))))
		  lines)
	  (setq thetoc (if have-headings (nreverse thetoc) nil))))

    (org-init-section-numbers)
    (while (setq line (pop lines))
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      ;; Remove targets
      (while (string-match "<<<?[^<>]*>>>?[ \t]*\n?" line)
	(setq line (replace-match "" t t line)))
      ;; Replace internal links
      (while (string-match org-bracket-link-regexp line)
	(setq line (replace-match
		    (if (match-end 3) "[\\3]" "[\\1]")
		    t nil line)))
      (when custom-times
	(setq line (org-translate-time line)))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	;; a Headline
	(setq first-heading-pos (or first-heading-pos (point)))
	(setq level (org-tr-level (- (match-end 1) (match-beginning 1)))
	      txt (match-string 2 line))
	(org-ascii-level-start level txt umax lines))

       ((and org-export-with-tables
	     (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	(if (not table-open)
	    ;; New table starts
	    (setq table-open t table-buffer nil))
	;; Accumulate lines
	(setq table-buffer (cons line table-buffer))
	(when (or (not lines)
		  (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
				     (car lines))))
	  (setq table-open nil
		table-buffer (nreverse table-buffer))
	  (insert (mapconcat
		   (lambda (x)
		     (org-fix-indentation x org-ascii-current-indentation))
		   (org-format-table-ascii table-buffer)
		   "\n") "\n")))
       (t
	(setq line (org-fix-indentation line org-ascii-current-indentation))
	(if (and org-export-with-fixed-width
		 (string-match "^\\([ \t]*\\)\\(:\\)" line))
	    (setq line (replace-match "\\1" nil nil line)))
	(insert line "\n"))))

    (normal-mode)

    ;; insert the table of contents
    (when thetoc
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\[TABLE-OF-CONTENTS\\][ \t]*$" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (replace-match ""))
	(goto-char first-heading-pos))
      (mapc 'insert thetoc)
      (or (looking-at "[ \t]*\n[ \t]*\n")
	  (insert "\n\n")))

    (save-buffer)
    ;; remove display and invisible chars
    (let (beg end)
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'display))
	(setq end (next-single-property-change beg 'display))
	(delete-region beg end)
	(goto-char beg)
	(insert "=>"))
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'org-cwidth))
	(setq end (next-single-property-change beg 'org-cwidth))
	(delete-region beg end)
	(goto-char beg)))
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
			      (not (member (match-string 2 line)
					  org-done-keywords))))
					; TODO, not DONE
	      (if (<= lv level) (throw 'exit nil))
	      (if todo (throw 'exit t))))))))

(defun org-html-expand-for-ascii (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
	;; We just remove the tags for now.
	(setq line (replace-match "" nil nil line))))
  line)

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- 80 (string-width s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
	(insert (make-string ind ?\ )
		(make-string (string-width s) underline)
		"\n"))))

(defun org-ascii-level-start (level title umax &optional lines)
  "Insert a new level in ASCII export."
  (let (char (n (- level umax 1)) (ind 0))
    (if (> level umax)
	(progn
	  (insert (make-string (* 2 n) ?\ )
		  (char-to-string (nth (% n (length org-export-ascii-bullets))
				       org-export-ascii-bullets))
		  " " title "\n")
	  ;; find the indentation of the next non-empty line
	  (catch 'stop
	    (while lines
	      (if (string-match "^\\* " (car lines)) (throw 'stop nil))
	      (if (string-match "^\\([ \t]*\\)\\S-" (car lines))
		  (throw 'stop (setq ind (org-get-indentation (car lines)))))
	      (pop lines)))
	  (setq org-ascii-current-indentation (cons (* 2 (1+ n)) ind)))
      (if (or (not (equal (char-before) ?\n))
	      (not (equal (char-before (1- (point))) ?\n)))
	  (insert "\n"))
      (setq char (nth (- umax level) (reverse org-export-ascii-underline)))
      (unless org-export-with-tags
	(if (string-match (org-re "[ \t]+\\(:[[:alnum:]_@:]+:\\)[ \t]*$") title)
	    (setq title (replace-match "" t t title))))
      (if org-export-with-section-numbers
	  (setq title (concat (org-section-number level) " " title)))
      (insert title "\n" (make-string (string-width title) char) "\n")
      (setq org-ascii-current-indentation '(0 . 0)))))

(defun org-export-visible (type arg)
  "Create a copy of the visible part of the current buffer, and export it.
The copy is created in a temporary buffer and removed after use.
TYPE is the final key (as a string) that also select the export command in
the `C-c C-e' export dispatcher.
As a special case, if the you type SPC at the prompt, the temporary
org-mode file will not be removed but presented to you so that you can
continue to use it.  The prefix arg ARG is passed through to the exporting
command."
  (interactive
   (list (progn
	   (message "Export visible: [a]SCII  [h]tml  [b]rowse HTML [H/R]uffer with HTML  [x]OXO  [ ]keep buffer")
	   (read-char-exclusive))
	 current-prefix-arg))
  (if (not (member type '(?a ?\C-a ?b ?\C-b ?h ?x ?\ )))
      (error "Invalid export key"))
  (let* ((binding (cdr (assoc type
			      '((?a . org-export-as-ascii)
				(?\C-a . org-export-as-ascii)
				(?b . org-export-as-html-and-open)
				(?\C-b . org-export-as-html-and-open)
				(?h . org-export-as-html)
				(?H . org-export-as-html-to-buffer)
				(?R . org-export-region-as-html)
				(?x . org-export-as-xoxo)))))
	 (keepp (equal type ?\ ))
	 (file buffer-file-name)
	 (buffer (get-buffer-create "*Org Export Visible*"))
	 s e)
    ;; Need to hack the drawers here.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
	(goto-char (match-beginning 1))
	(or (org-invisible-p) (org-flag-drawer nil))))
    (with-current-buffer buffer (erase-buffer))
    (save-excursion
      (setq s (goto-char (point-min)))
      (while (not (= (point) (point-max)))
	(goto-char (org-find-invisible))
	(append-to-buffer buffer s (point))
	(setq s (goto-char (org-find-visible))))
      (org-cycle-hide-drawers 'all)
      (goto-char (point-min))
      (unless keepp
	;; Copy all comment lines to the end, to make sure #+ settings are
	;; still available for the second export step.  Kind of a hack, but
	;; does do the trick.
	(if (looking-at "#[^\r\n]*")
	    (append-to-buffer buffer (match-beginning 0) (1+ (match-end 0))))
	(while (re-search-forward "[\n\r]#[^\n\r]*" nil t)
	  (append-to-buffer buffer (1+ (match-beginning 0))
			    (min (point-max) (1+ (match-end 0))))))
      (set-buffer buffer)
      (let ((buffer-file-name file)
	    (org-inhibit-startup t))
	(org-mode)
	(show-all)
	(unless keepp (funcall binding arg))))
    (if (not keepp)
	(kill-buffer buffer)
      (switch-to-buffer-other-window buffer)
      (goto-char (point-min)))))

(defun org-find-visible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(get-char-property s 'invisible)))
    s))
(defun org-find-invisible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(not (get-char-property s 'invisible))))
    s))

;;; HTML export

(defun org-get-current-options ()
  "Return a string with current options as keyword options.
Does include HTML export options as well as TODO and CATEGORY stuff."
  (format
   "#+TITLE:     %s
#+AUTHOR:    %s
#+EMAIL:     %s
#+LANGUAGE:  %s
#+TEXT:      Some descriptive text to be emitted.  Several lines OK.
#+OPTIONS:   H:%d num:%s toc:%s \\n:%s @:%s ::%s |:%s ^:%s f:%s *:%s TeX:%s LaTeX:%s skip:%s p:%s
#+CATEGORY:  %s
#+SEQ_TODO:  %s
#+TYP_TODO:  %s
#+PRIORITIES: %c %c %c
#+STARTUP:   %s %s %s %s %s
#+TAGS:      %s
#+ARCHIVE:   %s
#+LINK:      %s
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
   org-export-with-footnotes
   org-export-with-emphasize
   org-export-with-TeX-macros
   org-export-with-LaTeX-fragments
   org-export-skip-text-before-1st-heading
   org-export-with-property-drawer
   (file-name-nondirectory buffer-file-name)
   "TODO FEEDBACK VERIFY DONE"
   "Me Jason Marie DONE"
   org-highest-priority org-lowest-priority org-default-priority
   (cdr (assoc org-startup-folded
	       '((nil . "showall") (t . "overview") (content . "content"))))
   (if org-odd-levels-only "odd" "oddeven")
   (if org-hide-leading-stars "hidestars" "showstars")
   (if org-startup-align-all-tables "align" "noalign")
   (cond ((eq t org-log-done) "logdone")
	 ((not org-log-done) "nologging")
	 ((listp org-log-done)
	  (mapconcat (lambda (x) (concat "lognote" (symbol-name x)))
		     org-log-done " ")))
   (or (mapconcat (lambda (x)
		    (cond
		     ((equal '(:startgroup) x) "{")
		     ((equal '(:endgroup) x) "}")
		     ((cdr x) (format "%s(%c)" (car x) (cdr x)))
		     (t (car x))))
		  (or org-tag-alist (org-get-buffer-tags)) " ") "")
   org-archive-location
   "org file:~/org/%s.org"
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
				"\\( *\\<" org-quote-string "\\>\\)"))
	    (replace-match "" t t nil 1)
	  (if (looking-at outline-regexp)
	      (progn
		(goto-char (match-end 0))
		(insert org-quote-string " "))))))))

(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-html arg 'hidden)
  (org-open-file buffer-file-name))

(defun org-export-as-html-batch ()
  "Call `org-export-as-html', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-html org-export-headline-levels 'hidden))

(defun org-export-as-html-to-buffer (arg)
  "Call `org-exort-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'."
  (interactive "P")
  (org-export-as-html arg nil nil "*Org HTML Export*")
  (switch-to-buffer-other-window "*Org HTML Export*"))

(defun org-replace-region-by-html (beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it."
  (interactive "r")
  (let (reg html buf pop-up-frames)
    (save-window-excursion
      (if (org-mode-p)
	  (setq html (org-export-region-as-html
		      beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq html (org-export-region-as-html
		      (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert html)))

(defun org-export-region-as-html (beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only retunr the buffer."
  (interactive "r\nP")
  (when (interactive-p)
    (setq buffer "*Org HTML Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	rtn)
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-html
	       nil nil nil
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (interactive-p) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

(defun org-export-as-html (arg &optional hidden ext-plist
			       to-buffer body-only)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  When HIDDEN is non-nil, don't display the HTML buffer.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol `string',
don't leave any buffer behind but just return the resulting HTML as
a string.  When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of <body>...</body>, without even
the body tags themselves."
  (interactive "P")

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export.")))

  (message "Exporting...")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					ext-plist
					(org-infile-export-plist)))

	 (style (plist-get opt-plist :style))
	 (link-validate (plist-get opt-plist :link-validation-function))
	 valid thetoc have-headings first-heading-pos
	 (odd org-odd-levels-only)
	 (region-p (org-region-active-p))
	 (subtree-p
	  (when region-p
	    (save-excursion
	      (goto-char (region-beginning))
	      (and (org-at-heading-p)
		   (>= (org-end-of-subtree t t) (region-end))))))
	 ;; The following two are dynamically scoped into other
	 ;; routines below.
	 (org-current-export-dir (org-export-directory :html opt-plist))
	 (org-current-export-file buffer-file-name)
         (level 0) (line "") (origline "") txt todo
         (umax nil)
         (umax-toc nil)
         (filename (if to-buffer nil
		     (concat (file-name-as-directory
			      (org-export-directory :html opt-plist))
			     (file-name-sans-extension
			      (or (and subtree-p
				       (org-entry-get (region-beginning)
						      "EXPORT_FILE_NAME" t))
				  (file-name-nondirectory buffer-file-name)))
			     ".html")))
	 (current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string) (get-buffer-create "*Org HTML Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
         (org-levels-open (make-vector org-level-max nil))
	 (date (plist-get opt-plist :date))
         (author      (plist-get opt-plist :author))
	 (title       (or (and subtree-p (org-export-get-title-from-subtree))
			  (plist-get opt-plist :title)
			  (and (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (and buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name)))
			  "UNTITLED"))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]+" org-quote-string "\\>\\)"))
	 (inquote     nil)
	 (infixed     nil)
	 (in-local-list nil)
	 (local-list-num nil)
	 (local-list-indent nil)
	 (llt org-plain-list-ordered-item-terminator)
	 (email       (plist-get opt-plist :email))
         (language    (plist-get opt-plist :language))
	 (lang-words  nil)
	 (target-alist nil) tg
	 (head-count  0) cnt
	 (start       0)
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-html-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-html-coding-system
					coding-system))
	 (charset (and coding-system-for-write
		       (fboundp 'coding-system-get)
		       (coding-system-get coding-system-for-write
					  'mime-charset)))
         (region
          (buffer-substring
           (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))))
         (lines
          (org-split-string
	   (org-cleaned-string-for-export
	    region
	    :emph-multiline t
	    :for-html t
	    :skip-before-1st-heading
	    (plist-get opt-plist :skip-before-1st-heading)
	    :archived-trees
	    (plist-get opt-plist :archived-trees)
	    :add-text
	    (plist-get opt-plist :text)
	    :LaTeX-fragments
	    (plist-get opt-plist :LaTeX-fragments))
	   "[\r\n]"))
	 table-open type
	 table-buffer table-orig-buffer
	 ind start-is-num starter didclose
	 rpl path desc descp desc1 desc2 link
	 )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (message "Exporting...")

    (setq org-min-level (org-get-min-level lines))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date (current-time))))
     (date)
     (t (setq date (format-time-string "%Y/%m/%d %X" (current-time)))))

    ;; Get the language-dependent settings
    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer
    (set-buffer buffer)
    (erase-buffer)
    (fundamental-mode)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapcar (lambda (x)
		(set (make-local-variable (cdr x))
		     (plist-get opt-plist (car x))))
	      org-export-plist-vars)
      (setq umax (if arg (prefix-numeric-value arg)
                   org-export-headline-levels))
      (setq umax-toc (if (integerp org-export-with-toc)
			 (min org-export-with-toc umax)
		       umax))
      (unless body-only
	;; File header
	(insert (format
		 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
%s
</head><body>
"
		 language language (org-html-expand title)
		 (or charset "iso-8859-1") date author style))

	(insert (or (plist-get opt-plist :preamble) ""))

	(when (plist-get opt-plist :auto-preamble)
	  (if title (insert (format org-export-html-title-format
				    (org-html-expand title))))))

      (if (and org-export-with-toc (not body-only))
	  (progn
	    (push (format "<h%d>%s</h%d>\n"
			  org-export-html-toplevel-hlevel
			  (nth 3 lang-words)
			  org-export-html-toplevel-hlevel)
		  thetoc)
	    (push "<ul>\n<li>" thetoc)
	    (setq lines
		  (mapcar '(lambda (line)
		    (if (string-match org-todo-line-regexp line)
			;; This is a headline
			(progn
			  (setq have-headings t)
			  (setq level (- (match-end 1) (match-beginning 1))
				level (org-tr-level level)
				txt (save-match-data
				      (org-html-expand
				       (org-export-cleanup-toc-line
					(match-string 3 line))))
				todo
				(or (and org-export-mark-todo-in-toc
					 (match-beginning 2)
					 (not (member (match-string 2 line)
						      org-done-keywords)))
					; TODO, not DONE
				    (and org-export-mark-todo-in-toc
					 (= level umax-toc)
					 (org-search-todo-below
					  line lines level))))
			  (if (and (memq org-export-with-tags '(not-in-toc nil))
				   (string-match
				    (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$")
				    txt))
			      (setq txt (replace-match "" t t txt)))
			  (if (string-match quote-re0 txt)
			      (setq txt (replace-match "" t t txt)))
			  (if org-export-with-section-numbers
			      (setq txt (concat (org-section-number level)
						" " txt)))
			  (if (<= level (max umax umax-toc))
			      (setq head-count (+ head-count 1)))
			  (if (<= level umax-toc)
			      (progn
				(if (> level org-last-level)
				    (progn
				      (setq cnt (- level org-last-level))
				      (while (>= (setq cnt (1- cnt)) 0)
					(push "\n<ul>\n<li>" thetoc))
				      (push "\n" thetoc)))
				(if (< level org-last-level)
				    (progn
				      (setq cnt (- org-last-level level))
				      (while (>= (setq cnt (1- cnt)) 0)
					(push "</li>\n</ul>" thetoc))
				      (push "\n" thetoc)))
				;; Check for targets
				(while (string-match org-target-regexp line)
				  (setq tg (match-string 1 line)
					line (replace-match
					      (concat "@<span class=\"target\">" tg "@</span> ")
					      t t line))
				  (push (cons (org-solidify-link-text tg)
					      (format "sec-%d" head-count))
					target-alist))
				(while (string-match "&lt;\\(&lt;\\)+\\|&gt;\\(&gt;\\)+" txt)
				  (setq txt (replace-match "" t t txt)))
				(push
				 (format
				  (if todo
				      "</li>\n<li><a href=\"#sec-%d\"><span class=\"todo\">%s</span></a>"
				    "</li>\n<li><a href=\"#sec-%d\">%s</a>")
				  head-count txt) thetoc)

				(setq org-last-level level))
			    )))
		    line)
			  lines))
	    (while (> org-last-level (1- org-min-level))
	      (setq org-last-level (1- org-last-level))
	      (push "</li>\n</ul>\n" thetoc))
	    (setq thetoc (if have-headings (nreverse thetoc) nil))))

      (setq head-count 0)
      (org-init-section-numbers)

      (while (setq line (pop lines) origline line)
	(catch 'nextline

	  ;; end of quote section?
 	  (when (and inquote (string-match "^\\*+ " line))
	    (insert "</pre>\n")
	    (setq inquote nil))
	  ;; inside a quote section?
	  (when inquote
	    (insert (org-html-protect line) "\n")
	    (throw 'nextline nil))

	  ;; verbatim lines
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(.*\\)" line))
	    (when (not infixed)
	      (setq infixed t)
	      (insert "<pre>\n"))
	    (insert (org-html-protect (match-string 1 line)) "\n")
	    (when (and lines
		       (not (string-match "^[ \t]*\\(:.*\\)"
					  (car lines))))
	      (setq infixed nil)
	      (insert "</pre>\n"))
	    (throw 'nextline nil))

	  ;; Protected HTML
	  (when (get-text-property 0 'org-protected line)
	    (let (par)
	      (when (re-search-backward
		     "\\(<p>\\)\\([ \t\r\n]*\\)\\=" (- (point) 100) t)
		(setq par (match-string 1))
		(replace-match "\\2\n"))
	      (insert line "\n")
	      (while (and lines
			  (get-text-property 0 'org-protected (car lines)))
		(insert (pop lines) "\n"))
	      (and par (insert "<p>\n")))
	    (throw 'nextline nil))

	  ;; Horizontal line
	  (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
	    (insert "\n<hr/>\n")
	    (throw 'nextline nil))

	  ;; make targets to anchors
	  (while (string-match "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line)
	    (cond
	     ((match-end 2)
	      (setq line (replace-match
			  (concat "@<a name=\""
				  (org-solidify-link-text (match-string 1 line))
				  "\">\\nbsp@</a>")
			  t t line)))
	     ((and org-export-with-toc (equal (string-to-char line) ?*))
	      (setq line (replace-match
			  (concat "@<span class=\"target\">" (match-string 1 line) "@</span> ")
;			  (concat "@<i>" (match-string 1 line) "@</i> ")
			  t t line)))
	     (t
	      (setq line (replace-match
			  (concat "@<a name=\""
				  (org-solidify-link-text (match-string 1 line))
				  "\" class=\"target\">" (match-string 1 line) "@</a> ")
			  t t line)))))

	  (setq line (org-html-handle-time-stamps line))

	  ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  ;; Also handle sub_superscripts and checkboxes
	  (setq line (org-html-expand line))

	  ;; Format the links
	  (setq start 0)
	  (while (string-match org-bracket-link-analytic-regexp line start)
	    (setq start (match-beginning 0))
	    (setq type (if (match-end 2) (match-string 2 line) "internal"))
	    (setq path (match-string 3 line))
	    (setq desc1 (if (match-end 5) (match-string 5 line))
		  desc2 (if (match-end 2) (concat type ":" path) path)
		  descp (and desc1 (not (equal desc1 desc2)))
		  desc (or desc1 desc2))
	    ;; Make an image out of the description if that is so wanted
	    (when (and descp (org-file-image-p desc))
	      (save-match-data
		(if (string-match "^file:" desc)
		    (setq desc (substring desc (match-end 0)))))
	      (setq desc (concat "<img src=\"" desc "\"/>")))
	    ;; FIXME: do we need to unescape here somewhere?
	    (cond
	     ((equal type "internal")
	      (setq rpl
		    (concat
		     "<a href=\"#"
		     (org-solidify-link-text
		      (save-match-data (org-link-unescape path)) target-alist)
		     "\">" desc "</a>")))
	     ((member type '("http" "https")) ; FIXME: need to test this.
	      ;; standard URL, just check if we need to inline an image
	      (if (and (or (eq t org-export-html-inline-images)
			   (and org-export-html-inline-images (not descp)))
		       (org-file-image-p path))
		  (setq rpl (concat "<img src=\"" type ":" path "\"/>"))
		(setq link (concat type ":" path))
		(setq rpl (concat "<a href=\"" link "\">" desc "</a>"))))
	     ((member type '("ftp" "mailto" "news"))
	      ;; standard URL
	      (setq link (concat type ":" path))
	      (setq rpl (concat "<a href=\"" link "\">" desc "</a>")))
	     ((string= type "file")
	      ;; FILE link
	      (let* ((filename path)
		     (abs-p (file-name-absolute-p filename))
		     thefile file-is-image-p search)
		(save-match-data
		  (if (string-match "::\\(.*\\)" filename)
		      (setq search (match-string 1 filename)
			    filename (replace-match "" t nil filename)))
		  (setq valid
			(if (functionp link-validate)
			    (funcall link-validate filename current-dir)
			  t))
		  (setq file-is-image-p (org-file-image-p filename))
		  (setq thefile (if abs-p (expand-file-name filename) filename))
		  (when (and org-export-html-link-org-files-as-html
			     (string-match "\\.org$" thefile))
		    (setq thefile (concat (substring thefile 0
						     (match-beginning 0))
					  ".html"))
		    (if (and search
			     ;; make sure this is can be used as target search
			     (not (string-match "^[0-9]*$" search))
			     (not (string-match "^\\*" search))
			     (not (string-match "^/.*/$" search)))
			(setq thefile (concat thefile "#"
					      (org-solidify-link-text
					       (org-link-unescape search)))))
		    (when (string-match "^file:" desc)
		      (setq desc (replace-match "" t t desc))
		      (if (string-match "\\.org$" desc)
			  (setq desc (replace-match "" t t desc))))))
		(setq rpl (if (and file-is-image-p
				   (or (eq t org-export-html-inline-images)
				       (and org-export-html-inline-images
					    (not descp))))
			      (concat "<img src=\"" thefile "\"/>")
			    (concat "<a href=\"" thefile "\">" desc "</a>")))
		(if (not valid) (setq rpl desc))))
	     ((member type '("bbdb" "vm" "wl" "mhe" "rmail" "gnus" "shell" "info" "elisp"))
	      (setq rpl (concat "<i>&lt;" type ":"
				(save-match-data (org-link-unescape path))
				"&gt;</i>"))))
	    (setq line (replace-match rpl t t line)
		  start (+ start (length rpl))))

	  ;; TODO items
	  (if (and (string-match org-todo-line-regexp line)
		   (match-beginning 2))

              (setq line 
                    (concat (substring line 0 (match-beginning 2))
                            "<span class=\""
                            (if (member (match-string 2 line)
                                        org-done-keywords)
                                "done" "todo")
                            "\">" (match-string 2 line)
                            "</span>" (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (while (string-match "\\([^* \t].*?\\)\\[\\([0-9]+\\)\\]" line)
	      (let ((n (match-string 2 line)))
		(setq line
		      (replace-match
		       (format
			"%s<sup><a class=\"footref\" name=\"fnr.%s\" href=\"#fn.%s\">%s</a></sup>"
			(match-string 1 line) n n n)
		       t t line)))))

	  (cond
	   ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)))
		  txt (match-string 2 line))
	    (if (string-match quote-re0 txt)
		(setq txt (replace-match "" t t txt)))
	    (if (<= level (max umax umax-toc))
		(setq head-count (+ head-count 1)))
	    (when in-local-list
	      ;; Close any local lists before inserting a new header line
	      (while local-list-num
		(org-close-li)
		(insert (if (car local-list-num) "</ol>\n" "</ul>"))
		(pop local-list-num))
	      (setq local-list-indent nil
		    in-local-list nil))
	    (setq first-heading-pos (or first-heading-pos (point)))
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
	      (org-close-par-maybe)
	      (insert (org-format-table-html table-buffer table-orig-buffer))))
	   (t
	    ;; Normal lines
	    (when (string-match
		   (cond
		    ((eq llt t) "^\\([ \t]*\\)\\(\\([-+*] \\)\\|\\([0-9]+[.)]\\) \\)?\\( *[^ \t\n\r]\\|[ \t]*$\\)")
		    ((= llt ?.) "^\\([ \t]*\\)\\(\\([-+*] \\)\\|\\([0-9]+\\.\\) \\)?\\( *[^ \t\n\r]\\|[ \t]*$\\)")
		    ((= llt ?\)) "^\\( \t]*\\)\\(\\([-+*] \\)\\|\\([0-9]+)\\) \\)?\\( *[^ \t\n\r]\\|[ \t]*$\\)")
		    (t (error "Invalid value of `org-plain-list-ordered-item-terminator'")))
		   line)
	      (setq ind (org-get-string-indentation line)
		    start-is-num (match-beginning 4)
		    starter (if (match-beginning 2)
				(substring (match-string 2 line) 0 -1))
		    line (substring line (match-beginning 5)))
	      (unless (string-match "[^ \t]" line)
		;; empty line.  Pretend indentation is large.
		(setq ind (if org-empty-line-terminates-plain-lists
			      0
			    (1+ (or (car local-list-indent) 1)))))
	      (setq didclose nil)
	      (while (and in-local-list
			  (or (and (= ind (car local-list-indent))
				   (not starter))
			      (< ind (car local-list-indent))))
		(setq didclose t)
		(org-close-li)
		(insert (if (car local-list-num) "</ol>\n" "</ul>"))
		(pop local-list-num) (pop local-list-indent)
		(setq in-local-list local-list-indent))
	      (cond
	       ((and starter
		     (or (not in-local-list)
			 (> ind (car local-list-indent))))
		;; Start new (level of) list
		(org-close-par-maybe)
		(insert (if start-is-num "<ol>\n<li>\n" "<ul>\n<li>\n"))
		(push start-is-num local-list-num)
		(push ind local-list-indent)
		(setq in-local-list t))
	       (starter
		;; continue current list
		(org-close-li)
		(insert "<li>\n"))
	       (didclose
		;; we did close a list, normal text follows: need <p>
		(org-open-par)))
	      (if (string-match "^[ \t]*\\[\\([X ]\\)\\]" line)
		  (setq line
			(replace-match
			 (if (equal (match-string 1 line) "X")
			     "<b>[X]</b>"
			   "<b>[<span style=\"visibility:hidden;\">X</span>]</b>")
			   t t line))))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (if (string-match "^ [-+*]-\\|^[ \t]*$" line) (org-open-par))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-close-par-maybe)
		(let ((n (match-string 1 line)))
		  (setq line (replace-match
			      (format "<p class=\"footnote\"><sup><a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a></sup>" n n n) t t line)))))

	    ;; Check if the line break needs to be conserved
	    (cond
	     ((string-match "\\\\\\\\[ \t]*$" line)
	      (setq line (replace-match "<br/>" t t line)))
	     (org-export-preserve-breaks
	      (setq line (concat line "<br/>"))))

	    (insert line "\n")))))

      ;; Properly close all local lists and other lists
      (when inquote (insert "</pre>\n"))
      (when in-local-list
	;; Close any local lists before inserting a new header line
	(while local-list-num
	  (org-close-li)
	  (insert (if (car local-list-num) "</ol>\n" "</ul>\n"))
	  (pop local-list-num))
	(setq local-list-indent nil
	      in-local-list nil))
      (org-html-level-start 1 nil umax
			    (and org-export-with-toc (<= level umax))
			    head-count)

      (unless body-only
	(when (plist-get opt-plist :auto-postamble)
	  (when (and org-export-author-info author)
	    (insert "<p class=\"author\"> "
		    (nth 1 lang-words) ": " author "\n")
	    (when email
	      (insert "<a href=\"mailto:" email "\">&lt;"
		      email "&gt;</a>\n"))
	    (insert "</p>\n"))
	  (when (and date org-export-time-stamp-file)
	    (insert "<p class=\"date\"> "
		    (nth 2 lang-words) ": "
		    date "</p>\n")))

	(if org-export-html-with-timestamp
	    (insert org-export-html-html-helper-timestamp))
	(insert (or (plist-get opt-plist :postamble) ""))
	(insert "</body>\n</html>\n"))

      (normal-mode)
      (if (eq major-mode default-major-mode) (html-mode))

      ;; insert the table of contents
      (goto-char (point-min))
      (when thetoc
	(if (or (re-search-forward
		 "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
		(re-search-forward
		 "\\[TABLE-OF-CONTENTS\\]" nil t))
	    (progn
	      (goto-char (match-beginning 0))
	      (replace-match ""))
	  (goto-char first-heading-pos)
	  (when (looking-at "\\s-*</p>")
	    (goto-char (match-end 0))
	    (insert "\n")))
	(mapc 'insert thetoc))
      ;; remove empty paragraphs and lists
      (goto-char (point-min))
      (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "<li>[ \r\n\t]*</li>\n?" nil t)
	(replace-match ""))
      (or to-buffer (save-buffer))
      (goto-char (point-min))
      (message "Exporting... done")
      (if (eq to-buffer 'string)
	  (prog1 (buffer-substring (point-min) (point-max))
	    (kill-buffer (current-buffer)))
	(current-buffer)))))

(defvar org-table-colgroup-info nil) ;; FIXME: mode to a better place
(defun org-format-table-ascii (lines)
  "Format a table for ascii export."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (not (string-match "^[ \t]*|" (car lines)))
      ;; Table made by table.el - test for spanning
      lines

    ;; A normal org table
    ;; Get rid of hlines at beginning and end
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (when org-export-table-remove-special-lines
      ;; Check if the table has a marking column.  If yes remove the
      ;; column and the special lines
      (setq lines (org-table-clean-before-export lines)))
    ;; Get rid of the vertical lines except for grouping
    (let ((vl (org-colgroup-info-to-vline-list org-table-colgroup-info))
	  rtn line vl1 start)
      (while (setq line (pop lines))
	(if (string-match org-table-hline-regexp line)
	    (and (string-match "|\\(.*\\)|" line)
		 (setq line (replace-match " \\1" t nil line)))
	  (setq start 0 vl1 vl)
	  (while (string-match "|" line start)
	    (setq start (match-end 0))
	    (or (pop vl1) (setq line (replace-match " " t t line)))))
	(push line rtn))
      (nreverse rtn))))

(defun org-colgroup-info-to-vline-list (info)
  (let (vl new last)
    (while info
      (setq last new new (pop info))
      (if (or (memq last '(:end :startend))
	      (memq new  '(:start :startend)))
	  (push t vl)
	(push nil vl)))
    (setq vl (cons nil (nreverse vl)))))


(defun org-format-table-html (lines olines)
  "Find out which HTML converter to use and return the HTML code."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
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

(defun org-format-org-table-html (lines &optional splice)
  "Format a table into HTML."
  ;; Get rid of hlines at beginning and end
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (when org-export-table-remove-special-lines
    ;; Check if the table has a marking column.  If yes remove the
    ;; column and the special lines
    (setq lines (org-table-clean-before-export lines)))

  (let ((head (and org-export-highlight-first-table-line
		   (delq nil (mapcar
			      (lambda (x) (string-match "^[ \t]*|-" x))
			      (cdr lines)))))
	(nlines 0) fnum i
	tbopen line fields html gr colgropen)
    (if splice (setq head nil))
    (unless splice (push (if head "<thead>" "<tbody>") html))
    (setq tbopen t)
    (while (setq line (pop lines))
      (catch 'next-line
	(if (string-match "^[ \t]*|-" line)
	    (progn
	      (unless splice
		(push (if head "</thead>" "</tbody>") html)
		(if lines (push "<tbody>" html) (setq tbopen nil)))
	      (setq head nil)   ;; head ends here, first time around
	      ;; ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(unless fnum (setq fnum (make-vector (length fields) 0)))
	(setq nlines (1+ nlines) i -1)
	(push (concat "<tr>"
		      (mapconcat
		       (lambda (x)
			 (setq i (1+ i))
			 (if (and (< i nlines)
				  (string-match org-table-number-regexp x))
			     (incf (aref fnum i)))
			 (if head
			     (concat (car org-export-table-header-tags) x
				     (cdr org-export-table-header-tags))
			   (concat (car org-export-table-data-tags) x
				   (cdr org-export-table-data-tags))))
		       fields "")
		      "</tr>")
	      html)))
    (unless splice (if tbopen (push "</tbody>" html)))
    (unless splice (push "</table>\n" html))
    (setq html (nreverse html))
    (unless splice
      ;; Put in COL tags with the alignment (unfortuntely often ignored...)
      (push (mapconcat
	     (lambda (x)
	       (setq gr (pop org-table-colgroup-info))
	       (format "%s<COL align=\"%s\"></COL>%s"
		       (if (memq gr '(:start :startend)) 
			   (prog1
			       (if colgropen "</colgroup>\n<colgroup>" "<colgroup>")
			     (setq colgropen t))
			 "")
		       (if (> (/ (float x) nlines) org-table-number-fraction)
			   "right" "left")
		       (if (memq gr '(:end :startend)) 
			   (progn (setq colgropen nil) "</colgroup>")
			 "")))
	     fnum "")
	    html)
      (if colgropen (setq html (cons (car html) (cons "</colgroup>" (cdr html)))))
      (push org-export-html-table-tag html))
    (concat (mapconcat 'identity html "\n") "\n")))

(defun org-table-clean-before-export (lines)
  "Check if the table has a marking column.
If yes remove the column and the special lines."
  (setq org-table-colgroup-info nil)
  (if (memq nil
	    (mapcar
	     (lambda (x) (or (string-match "^[ \t]*|-" x)
			     (string-match "^[ \t]*| *\\([#!$*_^ /]\\) *|" x)))
	     lines))
      (progn
	(setq org-table-clean-did-remove-column nil)
	(delq nil
	      (mapcar
	       (lambda (x)
		 (cond
		  ((string-match  "^[ \t]*| */ *|" x)
		   (setq org-table-colgroup-info
			 (mapcar (lambda (x)
				   (cond ((member x '("<" "&lt;")) :start)
					 ((member x '(">" "&gt;")) :end)
					 ((member x '("<>" "&lt;&gt;")) :startend)
					 (t nil)))
				 (org-split-string x "[ \t]*|[ \t]*")))
		   nil)
		  (t x)))
	       lines)))
    (setq org-table-clean-did-remove-column t)
    (delq nil
	  (mapcar
	   (lambda (x)
	     (cond
	      ((string-match  "^[ \t]*| */ *|" x)
	       (setq org-table-colgroup-info
		     (mapcar (lambda (x)
			       (cond ((member x '("<" "&lt;")) :start)
				     ((member x '(">" "&gt;")) :end)
				     ((member x '("<>" "&lt;&gt;")) :startend)
				     (t nil)))
			     (cdr (org-split-string x "[ \t]*|[ \t]*"))))
	       nil)
	      ((string-match "^[ \t]*| *[!_^/] *|" x)
	       nil) ; ignore this line
	      ((or (string-match "^\\([ \t]*\\)|-+\\+" x)
		   (string-match "^\\([ \t]*\\)|[^|]*|" x))
	       ;; remove the first column
	       (replace-match "\\1|" t nil x))
	      (t (error "This should not happen"))))
	   lines))))

(defun org-format-table-table-html (lines)
  "Format a table generated by table.el into HTML.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (head org-export-highlight-first-table-line)
	     fields html empty)
    (setq html (concat org-export-html-table-tag "\n"))
    (while (setq line (pop lines))
      (setq empty "&nbsp;")
      (catch 'next-line
	(if (string-match "^[ \t]*\\+-" line)
	    (progn
	      (if field-buffer
		  (progn
		    (setq
		     html
		     (concat
		      html
		      "<tr>"
		      (mapconcat
		       (lambda (x)
			 (if (equal x "") (setq x empty))
			 (if head
			     (concat (car org-export-table-header-tags) x
				     (cdr org-export-table-header-tags))
			   (concat (car org-export-table-data-tags) x
				   (cdr org-export-table-data-tags))))
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
				  (concat x "<br/>" (pop fields)))
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

(defun org-html-handle-time-stamps (s)
  "Format time stamps in string S, or remove them."
  (catch 'exit
    (let (r b)
      (while (string-match org-maybe-keyword-time-regexp s)
	(if (and (match-end 1) (equal (match-string 1 s) org-clock-string))
	    ;; never export CLOCK
	    (throw 'exit ""))
	(or b (setq b (substring s 0 (match-beginning 0))))
	(if (not org-export-with-timestamps)
	    (setq r (concat r (substring s 0 (match-beginning 0)))
		  s (substring s (match-end 0)))
	  (setq r (concat
		   r (substring s 0 (match-beginning 0))
		   (if (match-end 1)
		       (format "@<span class=\"timestamp-kwd\">%s @</span>"
			       (match-string 1 s)))
		   (format " @<span class=\"timestamp\">%s@</span>"
			   (substring
			    (org-translate-time (match-string 3 s)) 1 -1)))
		s (substring s (match-end 0)))))
      ;; Line break if line started and ended with time stamp stuff
      (if (not r)
	  s
	(setq r (concat r s))
	(unless (string-match "\\S-" (concat b s))
	  (setq r (concat r "@<br/>")))
	r))))

(defun org-html-protect (s)
  ;; convert & to &amp;, < to &lt; and > to &gt;
  (let ((start 0))
    (while (string-match "&" s start)
      (setq s (replace-match "&amp;" t t s)
	    start (1+ (match-beginning 0))))
    (while (string-match "<" s)
      (setq s (replace-match "&lt;" t t s)))
    (while (string-match ">" s)
      (setq s (replace-match "&gt;" t t s))))
  s)

(defun org-export-cleanup-toc-line (s)
  "Remove tags and time staps from lines going into the toc."
  (if (string-match (org-re " +:[[:alnum:]_@:]+: *$") s)
      (setq s (replace-match "" t t s)))
  (when org-export-remove-timestamps-from-toc
    (while (string-match org-maybe-keyword-time-regexp s)
      (setq s (replace-match "" t t s))))
  (while (string-match org-bracket-link-regexp s)
    (setq s (replace-match (match-string (if (match-end 3) 3 1) s)
			   t t s)))
  s)

(defun org-html-expand (string)
  "Prepare STRING for HTML export.  Applies all active conversions.
If there are links in the string, don't modify these."
  (let* (m s l res)
    (while (setq m (string-match org-bracket-link-regexp string))
      (setq s (substring string 0 m)
	    l (match-string 0 string)
	    string (substring string (match-end 0)))
      (push (org-html-do-expand s) res)
      (push l res))
    (push (org-html-do-expand string) res)
    (apply 'concat (nreverse res))))

(defun org-html-do-expand (s)
  "Apply all active conversions to translate special ASCII to HTML."
  (setq s (org-html-protect s))
  (if org-export-html-expand
      (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	(setq s (replace-match "<\\1>" t nil s))))
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
  s)

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

;(let ((s "a\\_b"))
;  (and (string-match org-match-substring-regexp s)
;       (conca	     t (match-string 1 s) ":::" (match-string 2 s))))

(defun org-export-html-convert-sub-super (string)
  "Convert sub- and superscripts in STRING to HTML."
  (let (key c (s 0) (requireb (eq org-export-with-sub-superscripts '{})))
    (while (string-match org-match-substring-regexp string s)
      (if (and requireb (match-end 8))
	  (setq s (match-end 2))
	(setq s (match-end 1)
	      key (if (string= (match-string 2 string) "_") "sub" "sup")
	      c (or (match-string 8 string)
		    (match-string 6 string)
		    (match-string 5 string))
	      string (replace-match
		      (concat (match-string 1 string)
			      "<" key ">" c "</" key ">")
		      t t string))))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string)))
    string))

(defun org-export-html-convert-emphasize (string)
  "Apply emphasis."
  (let ((s 0))
    (while (string-match org-emph-re string s)
      (if (not (equal
		(substring string (match-beginning 3) (1+ (match-beginning 3)))
		(substring string (match-beginning 4) (1+ (match-beginning 4)))))
	  (setq string (replace-match
			(concat "\\1" (nth 2 (assoc (match-string 3 string) org-emphasis-alist))
				"\\4" (nth 3 (assoc (match-string 3 string) org-emphasis-alist))
				"\\5") t nil string))
	(setq s (1+ s))))
    string))

(defvar org-par-open nil)
(defun org-open-par ()
  "Insert <p>, but first close previous paragraph if any."
  (org-close-par-maybe)
  (insert "\n<p>")
  (setq org-par-open t))
(defun org-close-par-maybe ()
  "Close paragraph if there is one open."
  (when org-par-open
    (insert "</p>")
    (setq org-par-open nil)))
(defun org-close-li ()
  "Close <li> if necessary."
  (org-close-par-maybe)
  (insert "</li>\n"))

(defvar body-only) ; dynamically scoped into this.
(defun org-html-level-start (level title umax with-toc head-count)
  "Insert a new level in HTML export.
When TITLE is nil, just close all open levels."
  (org-close-par-maybe)
  (let ((l (1+ (max level umax))))
    (while (<= l org-level-max)
      (if (aref org-levels-open (1- l))
	  (progn
	    (org-html-level-close l)
	    (aset org-levels-open (1- l) nil)))
      (setq l (1+ l)))
    (when title
      ;; If title is nil, this means this function is called to close
      ;; all levels, so the rest is done only if title is given
	(when (string-match (org-re "\\(:[[:alnum:]_@:]+:\\)[ \t]*$") title)
	  (setq title (replace-match
		       (if org-export-with-tags
			   (save-match-data
			     (concat
			      "&nbsp;&nbsp;&nbsp;<span class=\"tag\">"
			      (mapconcat 'identity (org-split-string
						    (match-string 1 title) ":")
					 "&nbsp;")
			      "</span>"))
			 "")
		       t t title)))
      (if (> level umax)
	  (progn
	    (if (aref org-levels-open (1- level))
		(progn
		  (org-close-li)
		  (insert "<li>" title "<br/>\n"))
	      (aset org-levels-open (1- level) t)
	      (org-close-par-maybe)
	      (insert "<ul>\n<li>" title "<br/>\n")))
	(if (and org-export-with-section-numbers (not body-only))
	    (setq title (concat (org-section-number level) " " title)))
	(setq level (+ level org-export-html-toplevel-hlevel -1))
	(if with-toc
	    (insert (format "\n<h%d id=\"sec-%d\">%s</h%d>\n"
			    level head-count title level))
	  (insert (format "\n<h%d>%s</h%d>\n" level title level)))
	(org-open-par)))))

(defun org-html-level-close (&rest args)
  "Terminate one level in HTML export."
  (org-close-li)
  (insert "</ul>\n"))

;;; iCalendar export

;;;###autoload
(defun org-export-icalendar-this-file ()
  "Export current file as an iCalendar file.
The iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'."
  (interactive)
  (org-export-icalendar nil buffer-file-name))

;;;###autoload
(defun org-export-icalendar-all-agenda-files ()
  "Export all files in `org-agenda-files' to iCalendar .ics files.
Each iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'."
  (interactive)
  (apply 'org-export-icalendar nil (org-agenda-files t)))

;;;###autoload
(defun org-export-icalendar-combine-agenda-files ()
  "Export all files in `org-agenda-files' to a single combined iCalendar file.
The file is stored under the name `org-combined-agenda-icalendar-file'."
  (interactive)
  (apply 'org-export-icalendar t (org-agenda-files t)))

(defun org-export-icalendar (combine &rest files)
  "Create iCalendar files for all elements of FILES.
If COMBINE is non-nil, combine all calendar entries into a single large
file and store it under the name `org-combined-agenda-icalendar-file'."
  (save-excursion
    (org-prepare-agenda-buffers files)
    (let* ((dir (org-export-directory
		 :ical (list :publishing-directory
			     org-export-publishing-directory)))
	   file ical-file ical-buffer category started org-agenda-new-buffers)

      (and (get-buffer "*ical-tmp*") (kill-buffer "*ical-tmp*"))
      (when combine
	(setq ical-file
	      (if (file-name-absolute-p org-combined-agenda-icalendar-file)
		  org-combined-agenda-icalendar-file
		(expand-file-name org-combined-agenda-icalendar-file dir))
	      ical-buffer (org-get-agenda-file-buffer ical-file))
	(set-buffer ical-buffer) (erase-buffer))
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (set-buffer (org-get-agenda-file-buffer file))
	  (unless combine
	    (setq ical-file (concat (file-name-as-directory dir)
				    (file-name-sans-extension
				     (file-name-nondirectory buffer-file-name))
				    ".ics"))
	    (setq ical-buffer (org-get-agenda-file-buffer ical-file))
	    (with-current-buffer ical-buffer (erase-buffer)))
	  (setq category (or org-category
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))))
	  (if (symbolp category) (setq category (symbol-name category)))
	  (let ((standard-output ical-buffer))
	    (if combine
		(and (not started) (setq started t)
		     (org-start-icalendar-file org-icalendar-combined-name))
	      (org-start-icalendar-file category))
	    (org-print-icalendar-entries combine)
	    (when (or (and combine (not files)) (not combine))
	      (org-finish-icalendar-file)
	      (set-buffer ical-buffer)
	      (save-buffer)
	      (run-hooks 'org-after-save-iCalendar-file-hook)))))
      (org-release-buffers org-agenda-new-buffers))))

(defvar org-after-save-iCalendar-file-hook nil
  "Hook run after an iCalendar file has been saved.
The iCalendar buffer is still current when this hook is run.
A good way to use this is to tell a desktop calenndar application to re-read
the iCalendar file.")

(defun org-print-icalendar-entries (&optional combine)
  "Print iCalendar entries for the current Org-mode file to `standard-output'.
When COMBINE is non nil, add the category to each line."
  (let ((re1 (concat org-ts-regexp "\\|<%%([^>\n]+>"))
	(re2 (concat "--?-?\\(" org-ts-regexp "\\)"))
	(org-category-table (org-get-category-table))
	(dts (org-ical-ts-to-string
	      (format-time-string (cdr org-time-stamp-formats) (current-time))
	      "DTSTART"))
	hd ts ts2 state status (inc t) pos b sexp rrule
	scheduledp deadlinep tmp pri category
	(sexp-buffer (get-buffer-create "*ical-tmp*")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re1 nil t)
	(catch :skip
	  (org-agenda-skip)
	  (setq pos (match-beginning 0)
		ts (match-string 0)
		inc t
		hd (org-get-heading)
		category (org-get-category))
	  (if (looking-at re2)
	      (progn
		(goto-char (match-end 0))
		(setq ts2 (match-string 1) inc nil))
	    (setq tmp (buffer-substring (max (point-min)
					     (- pos org-ds-keyword-length))
					pos)
		  ts2 (if (string-match "[0-9]\\{1,2\\}:[0-9][0-9]-\\([0-9]\\{1,2\\}:[0-9][0-9]\\)" ts)
			  (progn
			    (setq inc nil)
			    (replace-match "\\1" t nil ts))
			ts)
		  deadlinep (string-match org-deadline-regexp tmp)
		  scheduledp (string-match org-scheduled-regexp tmp)
		  ;; donep (org-entry-is-done-p)
		  ))
	  (if (or (string-match org-tr-regexp hd)
		  (string-match org-ts-regexp hd))
	      (setq hd (replace-match "" t t hd)))
	  (if (string-match "\\+\\([0-9]+\\)\\([dwmy]\\)>" ts)
	      (setq rrule
		    (concat "\nRRULE:FREQ="
			    (cdr (assoc
				  (match-string 2 ts)
				  '(("d" . "DAILY")("w" . "WEEKLY")
				    ("m" . "MONTHLY")("y" . "YEARLY"))))
			    ";INTERVAL=" (match-string 1 ts)))
	    (setq rrule ""))
	  (if (string-match org-bracket-link-regexp hd)
	      (setq hd (replace-match (if (match-end 3) (match-string 3 hd)
					(match-string 1 hd))
				      t t hd)))
	  (if deadlinep (setq hd (concat "DL: " hd)))
	  (if scheduledp (setq hd (concat "S: " hd)))
	  (if (string-match "\\`<%%" ts)
	      (with-current-buffer sexp-buffer
		(insert (substring ts 1 -1) " " hd "\n"))
	    (princ (format "BEGIN:VEVENT
%s
%s%s
SUMMARY:%s
CATEGORIES:%s
END:VEVENT\n"
			   (org-ical-ts-to-string ts "DTSTART")
			   (org-ical-ts-to-string ts2 "DTEND" inc)
			   rrule hd category)))))

      (when (and org-icalendar-include-sexps
		 (condition-case nil (require 'icalendar) (error nil))
		 (fboundp 'icalendar-export-region))
	;; Get all the literal sexps
	(goto-char (point-min))
	(while (re-search-forward "^&?%%(" nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (setq b (match-beginning 0))
	    (goto-char (1- (match-end 0)))
	    (forward-sexp 1)
	    (end-of-line 1)
	    (setq sexp (buffer-substring b (point)))
	    (with-current-buffer sexp-buffer
	      (insert sexp "\n"))
	    (princ (org-diary-to-ical-string sexp-buffer)))))

      (when org-icalendar-include-todo
	(goto-char (point-min))
	(while (re-search-forward org-todo-line-regexp nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (setq state (match-string 2))
	    (setq status (if (member state org-done-keywords)
			     "COMPLETED" "NEEDS-ACTION"))
	    (when (and state
		       (or (not (member state org-done-keywords))
			   (eq org-icalendar-include-todo 'all))
		       (not (member org-archive-tag (org-get-tags-at)))
		       )
	      (setq hd (match-string 3))
	      (if (string-match org-bracket-link-regexp hd)
		  (setq hd (replace-match (if (match-end 3) (match-string 3 hd)
					    (match-string 1 hd))
					  t t hd)))
	      (if (string-match org-priority-regexp hd)
		  (setq pri (string-to-char (match-string 2 hd))
			hd (concat (substring hd 0 (match-beginning 1))
				   (substring hd (match-end 1))))
		(setq pri org-default-priority))
	      (setq pri (floor (1+ (* 8. (/ (float (- org-lowest-priority pri))
					    (- org-lowest-priority org-highest-priority))))))

	      (princ (format "BEGIN:VTODO
%s
SUMMARY:%s
CATEGORIES:%s
SEQUENCE:1
PRIORITY:%d
STATUS:%s
END:VTODO\n"
			     dts hd category pri status)))))))))

(defun org-start-icalendar-file (name)
  "Start an iCalendar file by inserting the header."
  (let ((user user-full-name)
	(name (or name "unknown"))
	(timezone (cadr (current-time-zone))))
    (princ
     (format "BEGIN:VCALENDAR
VERSION:2.0
X-WR-CALNAME:%s
PRODID:-//%s//Emacs with Org-mode//EN
X-WR-TIMEZONE:%s
CALSCALE:GREGORIAN\n" name user timezone))))

(defun org-finish-icalendar-file ()
  "Finish an iCalendar file by inserting the END statement."
  (princ "END:VCALENDAR\n"))

(defun org-ical-ts-to-string (s keyword &optional inc)
  "Take a time string S and convert it to iCalendar format.
KEYWORD is added in front, to make a complete line like DTSTART....
When INC is non-nil, increase the hour by two (if time string contains
a time), or the day by one (if it does not contain a time)."
  (let ((t1 (org-parse-time-string s 'nodefault))
	t2 fmt have-time time)
    (if (and (car t1) (nth 1 t1) (nth 2 t1))
	(setq t2 t1 have-time t)
      (setq t2 (org-parse-time-string s)))
    (let ((s (car t2))   (mi (nth 1 t2)) (h (nth 2 t2))
	  (d (nth 3 t2)) (m  (nth 4 t2)) (y (nth 5 t2)))
      (when inc
	(if have-time
	    (if org-agenda-default-appointment-duration
		(setq mi (+ org-agenda-default-appointment-duration mi))
	      (setq h (+ 2 h)))
	  (setq d (1+ d))))
      (setq time (encode-time s mi h d m y)))
    (setq fmt (if have-time ":%Y%m%dT%H%M%S" ";VALUE=DATE:%Y%m%d"))
    (concat keyword (format-time-string fmt time))))

;;; XOXO export

(defun org-export-as-xoxo-insert-into (buffer &rest output)
  (with-current-buffer buffer
    (apply 'insert output)))
(put 'org-export-as-xoxo-insert-into 'lisp-indent-function 1)

(defun org-export-as-xoxo (&optional buffer)
  "Export the org buffer as XOXO.
The XOXO buffer is named *xoxo-<source buffer name>*"
  (interactive (list (current-buffer)))
  ;; A quickie abstraction

  ;; Output everything as XOXO
  (with-current-buffer (get-buffer buffer)
    (goto-char (point-min))  ;; CD:  beginning-of-buffer is not allowed.
    (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	   (filename (concat (file-name-as-directory
			      (org-export-directory :xoxo opt-plist))
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))
			     ".html"))
	   (out (find-file-noselect filename))
	   (last-level 1)
	   (hanging-li nil))
      ;; Check the output buffer is empty.
      (with-current-buffer out (erase-buffer))
      ;; Kick off the output
      (org-export-as-xoxo-insert-into out "<ol class='xoxo'>\n")
      (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.+\\)" (point-max) 't)
        (let* ((hd (match-string-no-properties 1))
               (level (length hd))
               (text (concat
                      (match-string-no-properties 2)
                      (save-excursion
                        (goto-char (match-end 0))
                        (let ((str ""))
                          (catch 'loop
                            (while 't
                              (forward-line)
                              (if (looking-at "^[ \t]\\(.*\\)")
                                  (setq str (concat str (match-string-no-properties 1)))
                                (throw 'loop str)))))))))

          ;; Handle level rendering
          (cond
           ((> level last-level)
            (org-export-as-xoxo-insert-into out "\n<ol>\n"))

           ((< level last-level)
            (dotimes (- (- last-level level) 1)
              (if hanging-li
                  (org-export-as-xoxo-insert-into out "</li>\n"))
              (org-export-as-xoxo-insert-into out "</ol>\n"))
            (when hanging-li
              (org-export-as-xoxo-insert-into out "</li>\n")
              (setq hanging-li nil)))

           ((equal level last-level)
            (if hanging-li
                (org-export-as-xoxo-insert-into out "</li>\n")))
           )

          (setq last-level level)

          ;; And output the new li
          (setq hanging-li 't)
          (if (equal ?+ (elt text 0))
              (org-export-as-xoxo-insert-into out "<li class='" (substring text 1) "'>")
            (org-export-as-xoxo-insert-into out "<li>" text))))

      ;; Finally finish off the ol
      (dotimes (- last-level 1)
        (if hanging-li
            (org-export-as-xoxo-insert-into out "</li>\n"))
        (org-export-as-xoxo-insert-into out "</ol>\n"))

      ;; Finish the buffer off and clean it up.
      (switch-to-buffer-other-window out)
      (indent-region (point-min) (point-max) nil)
      (save-buffer)
      (goto-char (point-min))
      )))


;;;; Key bindings

;; Make `C-c C-x' a prefix key
(org-defkey org-mode-map "\C-c\C-x" (make-sparse-keymap))

;; TAB key with modifiers
(org-defkey org-mode-map "\C-i"       'org-cycle)
(org-defkey org-mode-map [(tab)]      'org-cycle)
(org-defkey org-mode-map [(control tab)] 'org-force-cycle-archived)
(org-defkey org-mode-map [(meta tab)] 'org-complete)
(org-defkey org-mode-map "\M-\t" 'org-complete)
(org-defkey org-mode-map "\M-\C-i"      'org-complete)
;; The following line is necessary under Suse GNU/Linux
(unless (featurep 'xemacs)
  (org-defkey org-mode-map [S-iso-lefttab]  'org-shifttab))
(org-defkey org-mode-map [(shift tab)]    'org-shifttab)
(define-key org-mode-map (kbd "<backtab>") 'org-shifttab)

(org-defkey org-mode-map [(shift return)]   'org-table-copy-down)
(org-defkey org-mode-map [(meta shift return)] 'org-insert-todo-heading)
(org-defkey org-mode-map [(meta return)]       'org-meta-return)

;; Cursor keys with modifiers
(org-defkey org-mode-map [(meta left)]  'org-metaleft)
(org-defkey org-mode-map [(meta right)] 'org-metaright)
(org-defkey org-mode-map [(meta up)]    'org-metaup)
(org-defkey org-mode-map [(meta down)]  'org-metadown)

(org-defkey org-mode-map [(meta shift left)]   'org-shiftmetaleft)
(org-defkey org-mode-map [(meta shift right)]  'org-shiftmetaright)
(org-defkey org-mode-map [(meta shift up)]     'org-shiftmetaup)
(org-defkey org-mode-map [(meta shift down)]   'org-shiftmetadown)

(org-defkey org-mode-map [(shift up)]          'org-shiftup)
(org-defkey org-mode-map [(shift down)]        'org-shiftdown)
(org-defkey org-mode-map [(shift left)]        'org-shiftleft)
(org-defkey org-mode-map [(shift right)]       'org-shiftright)

(org-defkey org-mode-map [(control shift right)] 'org-shiftcontrolright)
(org-defkey org-mode-map [(control shift left)]  'org-shiftcontrolleft)

;;; Extra keys for tty access.
;;  We only set them when really needed because otherwise the
;;  menus don't show the simple keys

(when (or (featurep 'xemacs)   ;; because XEmacs supports multi-device stuff
	  (not window-system))
  (org-defkey org-mode-map "\C-c\C-xc"    'org-table-copy-down)
  (org-defkey org-mode-map "\C-c\C-xM"    'org-insert-todo-heading)
  (org-defkey org-mode-map "\C-c\C-xm"    'org-meta-return)
  (org-defkey org-mode-map [?\e (return)] 'org-meta-return)
  (org-defkey org-mode-map [?\e (left)]   'org-metaleft)
  (org-defkey org-mode-map "\C-c\C-xl"    'org-metaleft)
  (org-defkey org-mode-map [?\e (right)]  'org-metaright)
  (org-defkey org-mode-map "\C-c\C-xr"    'org-metaright)
  (org-defkey org-mode-map [?\e (up)]     'org-metaup)
  (org-defkey org-mode-map "\C-c\C-xu"    'org-metaup)
  (org-defkey org-mode-map [?\e (down)]   'org-metadown)
  (org-defkey org-mode-map "\C-c\C-xd"    'org-metadown)
  (org-defkey org-mode-map "\C-c\C-xL"    'org-shiftmetaleft)
  (org-defkey org-mode-map "\C-c\C-xR"    'org-shiftmetaright)
  (org-defkey org-mode-map "\C-c\C-xU"    'org-shiftmetaup)
  (org-defkey org-mode-map "\C-c\C-xD"    'org-shiftmetadown)
  (org-defkey org-mode-map [?\C-c (up)]    'org-shiftup)
  (org-defkey org-mode-map [?\C-c (down)]  'org-shiftdown)
  (org-defkey org-mode-map [?\C-c (left)]  'org-shiftleft)
  (org-defkey org-mode-map [?\C-c (right)] 'org-shiftright)
  (org-defkey org-mode-map [?\C-c ?\C-x (right)] 'org-shiftcontrolright)
  (org-defkey org-mode-map [?\C-c ?\C-x (left)] 'org-shiftcontrolleft))

  ;; All the other keys

(org-defkey org-mode-map "\C-c\C-a" 'show-all)  ; in case allout messed up.
(org-defkey org-mode-map "\C-c\C-r" 'org-reveal)
(org-defkey org-mode-map "\C-xns" 'org-narrow-to-subtree)
(org-defkey org-mode-map "\C-c$"    'org-archive-subtree)
(org-defkey org-mode-map "\C-c\C-x\C-s" 'org-advertized-archive-subtree)
(org-defkey org-mode-map "\C-c\C-x\C-a" 'org-toggle-archive-tag)
(org-defkey org-mode-map "\C-c\C-xb" 'org-tree-to-indirect-buffer)
(org-defkey org-mode-map "\C-c\C-j" 'org-goto)
(org-defkey org-mode-map "\C-c\C-t" 'org-todo)
(org-defkey org-mode-map "\C-c\C-s" 'org-schedule)
(org-defkey org-mode-map "\C-c\C-d" 'org-deadline)
(org-defkey org-mode-map "\C-c;"    'org-toggle-comment)
(org-defkey org-mode-map "\C-c\C-v" 'org-show-todo-tree)
(org-defkey org-mode-map "\C-c\C-w" 'org-check-deadlines)
(org-defkey org-mode-map "\C-c/"    'org-occur)   ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\\"   'org-tags-sparse-tree) ; Minor-mode res.
(org-defkey org-mode-map "\C-c\C-m" 'org-ctrl-c-ret)
(org-defkey org-mode-map "\M-\C-m"  'org-insert-heading)
(org-defkey org-mode-map "\C-c\C-x\C-n" 'org-next-link)
(org-defkey org-mode-map "\C-c\C-x\C-p" 'org-previous-link)
(org-defkey org-mode-map "\C-c\C-l" 'org-insert-link)
(org-defkey org-mode-map "\C-c\C-o" 'org-open-at-point)
(org-defkey org-mode-map "\C-c%"    'org-mark-ring-push)
(org-defkey org-mode-map "\C-c&"    'org-mark-ring-goto)
(org-defkey org-mode-map "\C-c\C-z" 'org-time-stamp)  ; Alternative binding
(org-defkey org-mode-map "\C-c."    'org-time-stamp)  ; Minor-mode reserved
(org-defkey org-mode-map "\C-c!"    'org-time-stamp-inactive) ; Minor-mode r.
(org-defkey org-mode-map "\C-c,"    'org-priority)    ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\C-y" 'org-evaluate-time-range)
(org-defkey org-mode-map "\C-c>"    'org-goto-calendar)
(org-defkey org-mode-map "\C-c<"    'org-date-from-calendar)
(org-defkey org-mode-map [(control ?,)]     'org-cycle-agenda-files)
(org-defkey org-mode-map [(control ?\')]     'org-cycle-agenda-files)
(org-defkey org-mode-map "\C-c["    'org-agenda-file-to-front)
(org-defkey org-mode-map "\C-c]"    'org-remove-file)
(org-defkey org-mode-map "\C-c-"    'org-ctrl-c-minus)
(org-defkey org-mode-map "\C-c^"    'org-sort)
(org-defkey org-mode-map "\C-c\C-c" 'org-ctrl-c-ctrl-c)
(org-defkey org-mode-map "\C-c#"    'org-update-checkbox-count)
(org-defkey org-mode-map "\C-m"     'org-return)
(org-defkey org-mode-map "\C-c?"    'org-table-field-info)
(org-defkey org-mode-map "\C-c "    'org-table-blank-field)
(org-defkey org-mode-map "\C-c+"    'org-table-sum)
(org-defkey org-mode-map "\C-c="    'org-table-eval-formula)
(org-defkey org-mode-map "\C-c'"    'org-table-edit-formulas)
(org-defkey org-mode-map "\C-c`"    'org-table-edit-field)
(org-defkey org-mode-map "\C-c|"    'org-table-create-or-convert-from-region)
(org-defkey org-mode-map "\C-c*"    'org-table-recalculate)
(org-defkey org-mode-map [(control ?#)] 'org-table-rotate-recalc-marks)
(org-defkey org-mode-map "\C-c~"    'org-table-create-with-table.el)
(org-defkey org-mode-map "\C-c\C-q" 'org-table-wrap-region)
(org-defkey org-mode-map "\C-c}"    'org-table-toggle-coordinate-overlays)
(org-defkey org-mode-map "\C-c{"    'org-table-toggle-formula-debugger)
(org-defkey org-mode-map "\C-c\C-e" 'org-export)
(org-defkey org-mode-map "\C-c:"    'org-toggle-fixed-width-section)
(org-defkey org-mode-map "\C-c\C-x\C-f" 'org-emphasize)

(org-defkey org-mode-map "\C-c\C-x\C-k" 'org-cut-special)
(org-defkey org-mode-map "\C-c\C-x\C-w" 'org-cut-special)
(org-defkey org-mode-map "\C-c\C-x\M-w" 'org-copy-special)
(org-defkey org-mode-map "\C-c\C-x\C-y" 'org-paste-special)

(org-defkey org-mode-map "\C-c\C-x\C-t" 'org-toggle-time-stamp-overlays)
(org-defkey org-mode-map "\C-c\C-x\C-i" 'org-clock-in)
(org-defkey org-mode-map "\C-c\C-x\C-o" 'org-clock-out)
(org-defkey org-mode-map "\C-c\C-x\C-x" 'org-clock-cancel)
(org-defkey org-mode-map "\C-c\C-x\C-d" 'org-clock-display)
(org-defkey org-mode-map "\C-c\C-x\C-r" 'org-clock-report)
(org-defkey org-mode-map "\C-c\C-x\C-u" 'org-dblock-update)
(org-defkey org-mode-map "\C-c\C-x\C-l" 'org-preview-latex-fragment)
(org-defkey org-mode-map "\C-c\C-x\C-b" 'org-toggle-checkbox)

(define-key org-mode-map "\C-c\C-x\C-c" 'org-columns)

(when (featurep 'xemacs)
  (org-defkey org-mode-map 'button3   'popup-mode-menu))

(defsubst org-table-p () (org-at-table-p))

(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-table-p)
	   (progn
	     ;; check if we blank the field, and if that triggers align
	     (and org-table-auto-blank-field
		  (member last-command
			  '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
		  (if (or (equal (char-after) ?\ ) (looking-at "[^|\n]*  |"))
		      ;; got extra space, this field does not determine column width
		      (let (org-table-may-need-update) (org-table-blank-field))
		    ;; no extra space, this field may determine column width
		    (org-table-blank-field)))
	     t)
	   (eq N 1)
	   (looking-at "[^|\n]*  |"))
      (let (org-table-may-need-update)
	(goto-char (1- (match-end 0)))
	(delete-backward-char 1)
	(goto-char (match-beginning 0))
	(self-insert-command N))
    (setq org-table-may-need-update t)
    (self-insert-command N)
    (org-fix-tags-on-the-fly)))

(defun org-fix-tags-on-the-fly ()
  (when (and (equal (char-after (point-at-bol)) ?*)
	     (org-on-heading-p))
    (org-align-tags-here org-tags-column)))

(defun org-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (if (and (org-table-p)
	   (eq N 1)
	   (string-match "|" (buffer-substring (point-at-bol) (point)))
	   (looking-at ".*?|"))
      (let ((pos (point))
	    (noalign (looking-at "[^|\n\r]*  |"))
	    (c org-table-may-need-update))
	(backward-delete-char N)
	(skip-chars-forward "^|")
	(insert " ")
	(goto-char (1- pos))
	;; noalign: if there were two spaces at the end, this field
	;; does not determine the width of the column.
	(if noalign (setq org-table-may-need-update c)))
    (backward-delete-char N)
    (org-fix-tags-on-the-fly)))

(defun org-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (if (and (org-table-p)
	   (not (bolp))
	   (not (= (char-after) ?|))
	   (eq N 1))
      (if (looking-at ".*?|")
	  (let ((pos (point))
		(noalign (looking-at "[^|\n\r]*  |"))
		(c org-table-may-need-update))
	    (replace-match (concat
			    (substring (match-string 0) 1 -1)
			    " |"))
	    (goto-char pos)
	    ;; noalign: if there were two spaces at the end, this field
	    ;; does not determine the width of the column.
	    (if noalign (setq org-table-may-need-update c)))
	(delete-char N))
    (delete-char N)
    (org-fix-tags-on-the-fly)))

;; Make `delete-selection-mode' work with org-mode and orgtbl-mode
(put 'org-self-insert-command 'delete-selection t)
(put 'orgtbl-self-insert-command 'delete-selection t)
(put 'org-delete-char 'delete-selection 'supersede)
(put 'org-delete-backward-char 'delete-selection 'supersede)

;; Make `flyspell-mode' delay after some commands
(put 'org-self-insert-command 'flyspell-delayed t)
(put 'orgtbl-self-insert-command 'flyspell-delayed t)
(put 'org-delete-char 'flyspell-delayed t)
(put 'org-delete-backward-char 'flyspell-delayed t)

;; How to do this: Measure non-white length of current string
;; If equal to column width, we should realign.

(defun org-remap (map &rest commands)
  "In MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (if (fboundp 'command-remapping)
	  (org-defkey map (vector 'remap old) new)
	(substitute-key-definition old new map global-map)))))

(when (eq org-enable-table-editor 'optimized)
  ;; If the user wants maximum table support, we need to hijack
  ;; some standard editing functions
  (org-remap org-mode-map
	     'self-insert-command 'org-self-insert-command
	     'delete-char 'org-delete-char
	     'delete-backward-char 'org-delete-backward-char)
  (org-defkey org-mode-map "|" 'org-force-self-insert))

(defun org-shiftcursor-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (error "This command is active in special context like tables, headlines or timestamps"))

(defun org-shifttab (&optional arg)
  "Global visibility cycling or move to previous table field.
Calls `org-cycle' with argument t, or `org-table-previous-field', depending
on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-previous-field))
   (arg (message  "Content view to level: ")
	(org-content (prefix-numeric-value arg))
	(setq org-cycle-global-status 'overview))
   (t (call-interactively 'org-global-cycle))))

(defun org-shiftmetaleft ()
  "Promote subtree or delete table column.
Calls `org-promote-subtree', `org-outdent-item',
or `org-table-delete-column', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-delete-column))
   ((org-on-heading-p) (call-interactively 'org-promote-subtree))
   ((org-at-item-p) (call-interactively 'org-outdent-item))
   (t (org-shiftcursor-error))))

(defun org-shiftmetaright ()
  "Demote subtree or insert table column.
Calls `org-demote-subtree', `org-indent-item',
or `org-table-insert-column', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-insert-column))
   ((org-on-heading-p) (call-interactively 'org-demote-subtree))
   ((org-at-item-p) (call-interactively 'org-indent-item))
   (t (org-shiftcursor-error))))

(defun org-shiftmetaup (&optional arg)
  "Move subtree up or kill table row.
Calls `org-move-subtree-up' or `org-table-kill-row' or
`org-move-item-up' depending on context.  See the individual commands
for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-kill-row))
   ((org-on-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   (t (org-shiftcursor-error))))
(defun org-shiftmetadown (&optional arg)
  "Move subtree down or insert table row.
Calls `org-move-subtree-down' or `org-table-insert-row' or
`org-move-item-down', depending on context.  See the individual
commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-insert-row))
   ((org-on-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p) (call-interactively 'org-move-item-down))
   (t (org-shiftcursor-error))))

(defun org-metaleft (&optional arg)
  "Promote heading or move table column to left.
Calls `org-do-promote' or `org-table-move-column', depending on context.
With no specific context, calls the Emacs default `backward-word'.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ((or (org-on-heading-p) (org-region-active-p))
    (call-interactively 'org-do-promote))
   ((org-at-item-p) (call-interactively 'org-outdent-item))
   (t (call-interactively 'backward-word))))

(defun org-metaright (&optional arg)
  "Demote subtree or move table column to right.
Calls `org-do-demote' or `org-table-move-column', depending on context.
With no specific context, calls the Emacs default `forward-word'.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-move-column))
   ((or (org-on-heading-p) (org-region-active-p))
    (call-interactively 'org-do-demote))
   ((org-at-item-p) (call-interactively 'org-indent-item))
   (t (call-interactively 'forward-word))))

(defun org-metaup (&optional arg)
  "Move subtree up or move table row up.
Calls `org-move-subtree-up' or `org-table-move-row' or
`org-move-item-up', depending on context.  See the individual commands
for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-move-row 'up))
   ((org-on-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   (t (org-shiftcursor-error))))

(defun org-metadown (&optional arg)
  "Move subtree down or move table row down.
Calls `org-move-subtree-down' or `org-table-move-row' or
`org-move-item-down', depending on context.  See the individual
commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-move-row))
   ((org-on-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p) (call-interactively 'org-move-item-down))
   (t (org-shiftcursor-error))))

(defun org-shiftup (&optional arg)
  "Increase item in timestamp or increase priority of current headline.
Calls `org-timestamp-up' or `org-priority-up', or `org-previous-item',
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-timestamp-p t)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ((org-on-heading-p) (call-interactively 'org-priority-up))
   ((org-at-item-p) (call-interactively 'org-previous-item))
   (t (call-interactively 'org-beginning-of-item) (beginning-of-line 1))))

(defun org-shiftdown (&optional arg)
  "Decrease item in timestamp or decrease priority of current headline.
Calls `org-timestamp-down' or `org-priority-down', or `org-next-item'
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-timestamp-p t)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ((org-on-heading-p) (call-interactively 'org-priority-down))
   (t (call-interactively 'org-next-item))))

(defun org-shiftright ()
  "Next TODO keyword or timestamp one day later, depending on context."
  (interactive)
  (cond
   ((org-at-timestamp-p t) (call-interactively 'org-timestamp-up-day))
   ((org-on-heading-p) (org-call-with-arg 'org-todo 'right))
   ((org-at-property-p) (call-interactively 'org-property-next-allowed-value))
   (t (org-shiftcursor-error))))

(defun org-shiftleft ()
  "Previous TODO keyword or timestamp one day earlier, depending on context."
  (interactive)
  (cond
   ((org-at-timestamp-p t) (call-interactively 'org-timestamp-down-day))
   ((org-on-heading-p) (org-call-with-arg 'org-todo 'left))
   ((org-at-property-p)
    (call-interactively 'org-property-previous-allowed-value))
   (t (org-shiftcursor-error))))

(defun org-shiftcontrolright ()
  "Switch to next TODO set."
  (interactive)
  (cond
   ((org-on-heading-p) (org-call-with-arg 'org-todo 'nextset))
   (t (org-shiftcursor-error))))

(defun org-shiftcontrolleft ()
  "Switch to previous TODO set."
  (interactive)
  (cond
   ((org-on-heading-p) (org-call-with-arg 'org-todo 'previousset))
   (t (org-shiftcursor-error))))

(defun org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-heading' dep. on context."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-heading))))

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
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If the cursor is in a headline, prompt for tags and insert them
  into the current line, aligned to `org-tags-column'.  When called
  with prefix arg, realign all tags in the current buffer.

- If the cursor is in one of the special #+KEYWORD lines, this
  triggers scanning the buffer for these lines and updating the
  information.

- If the cursor is inside a table, realign the table.  This command
  works even if the automatic table editor has been turned off.

- If the cursor is on a #+TBLFM line, re-apply the formulas to
  the entire table.

- If the cursor is inside a table created by the table.el package,
  activate that table.

- If the current buffer is a remember buffer, close note and file it.
  with a prefix argument, file it without further interaction to the default
  location.

- If the cursor is on a <<<target>>>, update radio targets and corresponding
  links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list."
  (interactive "P")
  (let  ((org-enable-table-editor t))
    (cond
     ((or org-clock-overlays
	  org-occur-highlights
	  org-latex-fragment-image-overlays)
      (org-remove-clock-overlays)
      (org-remove-occur-highlights)
      (org-remove-latex-fragment-image-overlays)
      (message "Temporary highlights/overlays removed from current buffer"))
     ((and (local-variable-p 'org-finish-function (current-buffer))
	   (fboundp org-finish-function))
      (funcall org-finish-function))
     ((org-at-property-p)
      (call-interactively 'org-property-action))
     ((org-on-target-p) (call-interactively 'org-update-radio-target-regexp))
     ((org-on-heading-p) (call-interactively 'org-set-tags))
     ((org-at-table.el-p)
      (require 'table)
      (beginning-of-line 1)
      (re-search-forward "|" (save-excursion (end-of-line 2) (point)))
      (call-interactively 'table-recognize-table))
     ((org-at-table-p)
      (org-table-maybe-eval-formula)
      (if arg
	  (call-interactively 'org-table-recalculate)
	(org-table-maybe-recalculate-line))
      (call-interactively 'org-table-align))
     ((org-at-item-checkbox-p)
      (call-interactively 'org-toggle-checkbox))
     ((org-at-item-p)
      (call-interactively 'org-maybe-renumber-ordered-list))
     ((save-excursion (beginning-of-line 1) (looking-at "#\\+\\([A-Z]+\\)"))
      (cond
       ((equal (match-string 1) "TBLFM")
	;; Recalculate the table before this line
	(save-excursion
	  (beginning-of-line 1)
	  (skip-chars-backward " \r\n\t")
	  (if (org-at-table-p)
	      (org-call-with-arg 'org-table-recalculate t))))
       (t
	(call-interactively 'org-mode-restart))))
     (t (error "C-c C-c can do nothing useful at this location.")))))

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
   ((bobp) (newline))
   ((org-at-table-p)
    (org-table-justify-field-maybe)
    (call-interactively 'org-table-next-row))
   (t (newline))))

(defun org-ctrl-c-minus ()
  "Insert separator line in table or modify bullet type in list.
Calls `org-table-insert-hline' or `org-cycle-list-bullet',
depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-insert-hline))
   ((org-in-item-p)
    (call-interactively 'org-cycle-list-bullet))
   (t (error "`C-c -' does have no function here."))))

(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading' or `org-table-wrap-region', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-wrap-region))
   (t (call-interactively 'org-insert-heading))))

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
    ["Edit Field" org-table-edit-field (org-at-table-p)]
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
     ["Sort lines in region" org-table-sort-lines (org-at-table-p)]
     "--"
     ["Insert Hline" org-ctrl-c-minus (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-table-edit-formulas (org-at-table-p)]
     "--"
     ["Recalculate line" org-table-recalculate (org-at-table-p)]
     ["Recalculate all" (lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Iterate all" (lambda () (interactive) (org-table-recalculate '(16))) :active (org-at-table-p) :keys "C-u C-u C-c *"]
     "--"
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (org-region-active-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     org-table-toggle-formula-debugger
     :style toggle :selected org-table-formula-debug]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle :selected org-table-overlay-coordinates]
    "--"
    ["Create" org-table-create (and (not (org-at-table-p))
				    org-enable-table-editor)]
    ["Convert Region" org-table-convert-region (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert from/to table.el" org-table-create-with-table.el t]))

(easy-menu-define org-org-menu org-mode-map "Org menu"
  '("Org"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab (not (org-at-table-p))]
     ["Sparse Tree" org-occur t]
     ["Reveal Context" org-reveal t]
     ["Show All" show-all t]
     "--"
     ["Subtree to indirect buffer" org-tree-to-indirect-buffer t])
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
     ["Sort Region/Children" org-sort  (not (org-at-table-p))]
     "--"
     ["Convert to odd levels" org-convert-to-odd-levels t]
     ["Convert to odd/even levels" org-convert-to-oddeven-levels t])
    ("Editing"
     ["Emphasis..." org-emphasize t])
    ("Archive"
     ["Toggle ARCHIVE tag" org-toggle-archive-tag t]
;     ["Check and Tag Children" (org-toggle-archive-tag (4))
;      :active t :keys "C-u C-c C-x C-a"]
     ["Sparse trees open ARCHIVE trees"
      (setq org-sparse-tree-open-archived-trees
	    (not org-sparse-tree-open-archived-trees))
      :style toggle :selected org-sparse-tree-open-archived-trees]
     ["Cycling opens ARCHIVE trees"
      (setq org-cycle-open-archived-trees (not org-cycle-open-archived-trees))
      :style toggle :selected org-cycle-open-archived-trees]
     ["Agenda includes ARCHIVE trees"
      (setq org-agenda-skip-archived-trees (not org-agenda-skip-archived-trees))
      :style toggle :selected (not org-agenda-skip-archived-trees)]
     "--"
     ["Move Subtree to Archive" org-advertized-archive-subtree t]
 ;    ["Check and Move Children" (org-archive-subtree '(4))
 ;     :active t :keys "C-u C-c C-x C-s"]
     )
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ("Select keyword"
      ["Next keyword" org-shiftright (org-on-heading-p)]
      ["Previous keyword" org-shiftleft (org-on-heading-p)]
      ["Complete Keyword" org-complete (assq :todo-keyword (org-context))]
      ["Next keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-on-heading-p))]
      ["Previous keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-on-heading-p))])
     ["Show TODO Tree" org-show-todo-tree t]
     ["Global TODO list" org-todo-list t]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t])
    ("TAGS and Properties"
     ["Set Tags" 'org-ctrl-c-ctrl-c (org-at-heading-p)]
     ["Change tag in region" 'org-change-tag-in-region (org-region-active-p)] ;FIXME
     ["Column view of properties" org-columns t])
    ("Dates and Scheduling"
     ["Timestamp" org-time-stamp t]
     ["Timestamp (inactive)" org-time-stamp-inactive t]
     ("Change Date"
      ["1 Day Later" org-shiftright t]
      ["1 Day Earlier" org-shiftleft t]
      ["1 ... Later" org-shiftup t]
      ["1 ... Earlier" org-shiftdown t])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule t]
     ["Deadline" org-deadline t]
     "--"
     ["Custom time format" org-toggle-time-stamp-overlays
      :style radio :selected org-display-custom-times]
     "--"
     ["Goto Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t])
    ("Logging work"
     ["Clock in" org-clock-in t]
     ["Clock out" org-clock-out t]
     ["Clock cancel" org-clock-cancel t]
     ["Display times" org-clock-display t]
     ["Create clock table" org-clock-report t]
     "--"
     ["Record DONE time"
      (progn (setq org-log-done (not org-log-done))
	     (message "Switching to %s will %s record a timestamp"
		      (car org-done-keywords)
		      (if org-log-done "automatically" "not")))
      :style toggle :selected org-log-done])
    "--"
    ["Agenda Command..." org-agenda t]
    ("File List for Agenda")
    ("Special views current file"
     ["TODO Tree"  org-show-todo-tree t]
     ["Check Deadlines" org-check-deadlines t]
     ["Timeline" org-timeline t]
     ["Tags Tree" org-tags-sparse-tree t])
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t]
     "--"
     ["Next link" org-next-link t]
     ["Previous link" org-previous-link t]
     "--"
     ["Descriptive Links"
      (progn (org-add-to-invisibility-spec '(org-link)) (org-restart-font-lock))
      :style radio :selected (member '(org-link) buffer-invisibility-spec)]
     ["Literal Links"
      (progn
	(org-remove-from-invisibility-spec '(org-link)) (org-restart-font-lock))
      :style radio :selected (not (member '(org-link) buffer-invisibility-spec))])
    "--"
    ["Export/Publish..." org-export t]
    ("LaTeX"
     ["Org CDLaTeX mode" org-cdlatex-mode :style toggle
      :selected org-cdlatex-mode]
     ["Insert Environment" cdlatex-environment (fboundp 'cdlatex-environment)]
     ["Insert math symbol" cdlatex-math-symbol (fboundp 'cdlatex-math-symbol)]
     ["Modify math symbol" org-cdlatex-math-modify
      (org-inside-LaTeX-fragment-p)]
     ["Export LaTeX fragments as images"
      (setq org-export-with-LaTeX-fragments (not org-export-with-LaTeX-fragments))
      :style toggle :selected org-export-with-LaTeX-fragments])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Info Documentation" org-info t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Expand This Menu" org-create-customize-menu
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
  (let ((bl (buffer-list)))
    (save-excursion
      (while bl
	(set-buffer (pop bl))
	(if (org-mode-p) (setq bl nil)))
      (when (org-mode-p)
	(easy-menu-change
	 '("Org") "File List for Agenda"
	 (append
	  (list
	   ["Edit File List" (org-edit-agenda-file-list) t]
	   ["Add/Move Current File to Front of List" org-agenda-file-to-front t]
	   ["Remove Current File from List" org-remove-file t]
	   ["Cycle through agenda files" org-cycle-agenda-files t]
	   "--")
	  (mapcar 'org-file-menu-entry (org-agenda-files t))))))))

;;;; Documentation

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

;;;; Miscellaneous stuff


;;; Generally useful functions

(defun org-context ()
  "Return a list of contexts of the current cursor position.
If several contexts apply, all are returned.
Each context entry is a list with a symbol naming the context, and
two positions indicating start and end of the context.  Possible
contexts are:

:headline         anywhere in a headline
:headline-stars   on the leading stars in a headline
:todo-keyword     on a TODO keyword (including DONE) in a headline
:tags             on the TAGS in a headline
:priority         on the priority cookie in a headline
:item             on the first line of a plain list item
:item-bullet      on the bullet/number of a plain list item
:checkbox         on the checkbox in a plain list item
:table            in an org-mode table
:table-special    on a special filed in a table
:table-table      in a table.el table
:link             on a hyperlink
:keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE,COMMENT, QUOTE.
:target           on a <<target>>
:radio-target     on a <<<radio-target>>>
:latex-fragment   on a LaTeX fragment
:latex-preview    on a LaTeX fragment with overlayed preview image

This function expects the position to be visible because it uses font-lock
faces as a help to recognize the following contexts: :table-special, :link,
and :keyword."
  (let* ((f (get-text-property (point) 'face))
	 (faces (if (listp f) f (list f)))
	 (p (point)) clist o)
    ;; First the large context
    (cond
     ((org-on-heading-p t)
      (push (list :headline (point-at-bol) (point-at-eol)) clist)
      (when (progn
	      (beginning-of-line 1)
	      (looking-at org-todo-line-tags-regexp))
	(push (org-point-in-group p 1 :headline-stars) clist)
	(push (org-point-in-group p 2 :todo-keyword) clist)
	(push (org-point-in-group p 4 :tags) clist))
      (goto-char p)
      (skip-chars-backward "^[\n\r \t") (or (eobp) (backward-char 1))
      (if (looking-at "\\[#[A-Z0-9]\\]")
	  (push (org-point-in-group p 0 :priority) clist)))

     ((org-at-item-p)
      (push (org-point-in-group p 2 :item-bullet) clist)
      (push (list :item (point-at-bol)
		  (save-excursion (org-end-of-item) (point)))
	    clist)
      (and (org-at-item-checkbox-p)
	   (push (org-point-in-group p 0 :checkbox) clist)))

     ((org-at-table-p)
      (push (list :table (org-table-begin) (org-table-end)) clist)
      (if (memq 'org-formula faces)
	  (push (list :table-special
		      (previous-single-property-change p 'face)
		      (next-single-property-change p 'face)) clist)))
     ((org-at-table-p 'any)
      (push (list :table-table) clist)))
    (goto-char p)

    ;; Now the small context
    (cond
     ((org-at-timestamp-p)
      (push (org-point-in-group p 0 :timestamp) clist))
     ((memq 'org-link faces)
      (push (list :link
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face)) clist))
     ((memq 'org-special-keyword faces)
      (push (list :keyword
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face)) clist))
     ((org-on-target-p)
      (push (org-point-in-group p 0 :target) clist)
      (goto-char (1- (match-beginning 0)))
      (if (looking-at org-radio-target-regexp)
	  (push (org-point-in-group p 0 :radio-target) clist))
      (goto-char p))
     ((setq o (car (delq nil
			 (mapcar
			  (lambda (x)
			    (if (memq x org-latex-fragment-image-overlays) x))
			  (org-overlays-at (point))))))
      (push (list :latex-fragment
		  (org-overlay-start o) (org-overlay-end o)) clist)
      (push (list :latex-preview
		  (org-overlay-start o) (org-overlay-end o)) clist))
     ((org-inside-LaTeX-fragment-p)
      ;; FIXME: positions wrong.
      (push (list :latex-fragment (point) (point)) clist)))

    (setq clist (nreverse (delq nil clist)))
    clist))

;; FIXME Compare with at-regexp-p
(defun org-in-regexp (re &optional nlines visually)
  "Check if point is inside a match of regexp.
Normally only the current line is checked, but you can include NLINES extra
lines both before and after point into the search.
If VISUALLY is set, require that the cursor is not after the match but
really on, so that the block visually is on the match."
  (catch 'exit
    (let ((pos (point))
          (eol (point-at-eol (+ 1 (or nlines 0))))
	  (inc (if visually 1 0)))
      (save-excursion
	(beginning-of-line (- 1 (or nlines 0)))
	(while (re-search-forward re eol t)
	  (if (and (<= (match-beginning 0) pos)
		   (>= (+ inc (match-end 0)) pos))
	      (throw 'exit (cons (match-beginning 0) (match-end 0)))))))))

(defun org-at-regexp-p (regexp)
  "Is point inside a match of REGEXP in the current line?"
  (catch 'exit
    (save-excursion
      (let ((pos (point)) (end (point-at-eol)))
	(beginning-of-line 1)
	(while (re-search-forward regexp end t)
	  (if (and (<= (match-beginning 0) pos)
		   (>= (match-end 0) pos))
	      (throw 'exit t)))
	nil))))

(defun org-uniquify (list)
  "Remove duplicate elements from LIST."
  (let (res)
    (mapc (lambda (x) (add-to-list 'res x 'append)) list)
    res))

(defun org-delete-all (elts list)
  "Remove all elements in ELTS from LIST."
  (while elts
    (setq list (delete (pop elts) list)))
  list)

(defun org-point-in-group (point group &optional context)
  "Check if POINT is in match-group GROUP.
If CONTEXT is non-nil, return a list with CONTEXT and the boundaries of the
match.  If the match group does ot exist or point is not inside it,
return nil."
  (and (match-beginning group)
       (>= point (match-beginning group))
       (<= point (match-end group))
       (if context
	   (list context (match-beginning group) (match-end group))
	 t)))

(defun org-switch-to-buffer-other-window (&rest args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames."
  (let (pop-up-frames special-display-buffer-names special-display-regexps
		      special-display-function)
    (apply 'switch-to-buffer-other-window args)))

(defun org-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

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

(defun org-replace-escapes (string table)
  "Replace %-escapes in STRING with values in TABLE.
TABLE is an association list with keys line \"%a\" and string values.
The sequences in STRING may contain normal field width and padding information,
for example \"%-5s\".  Replacements happen in the sequence given by TABLE,
so values can contain further %-escapes if they are define later in TABLE."
  (let ((case-fold-search nil)
	e re rpl)
    (while (setq e (pop table))
      (setq re (concat "%-?[0-9.]*" (substring (car e) 1)))
      (while (string-match re string)
	(setq rpl (format (concat (substring (match-string 0 string) 0 -1) "s")
			  (cdr e)))
	(setq string (replace-match rpl t t string))))
    string))


(defun org-sublist (list start end)
  "Return a section of LIST, from START to END.
Counting starts at 1."
  (let (rtn (c start))
    (setq list (nthcdr (1- start) list))
    (while (and list (<= c end))
      (push (pop list) rtn)
      (setq c (1+ c)))
    (nreverse rtn)))

(defun org-find-base-buffer-visiting (file)
  "Like `find-buffer-visiting' but alway return the base buffer and
not an indirect buffer"
  (let ((buf (find-buffer-visiting file)))
    (or (buffer-base-buffer buf) buf)))

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

(defun org-file-image-p (file)
  "Return non-nil if FILE is an image."
  (save-match-data
    (string-match (org-image-file-name-regexp) file)))

;;; Paragraph filling stuff.
;; We want this to be just right, so use the full arsenal.

(defun org-indent-line-function ()
  "Indent line like previous, but further if previous was headline or item."
  (interactive)
  (let* ((pos (point))
	 (itemp (org-at-item-p))
	 column bpos bcol tpos tcol bullet btype bullet-type)
    ;; Find the previous relevant line
    (beginning-of-line 1)
    (cond
     ((looking-at "#") (setq column 0))
     ((looking-at "\\*+ ") (setq column 0))
     (t
      (beginning-of-line 0)
      (while (and (not (bobp)) (looking-at "[ \t]*[\n:#|]"))
	(beginning-of-line 0))
      (cond
       ((looking-at "\\*+[ \t]+")
	(goto-char (match-end 0))
	(setq column (current-column)))
       ((org-in-item-p)
	(org-beginning-of-item)
;	(looking-at "[ \t]*\\(\\S-+\\)[ \t]*")
	(looking-at "[ \t]*\\(\\S-+\\)[ \t]*\\(\\[[- X]\\][ \t]*\\)?")
	(setq bpos (match-beginning 1) tpos (match-end 0)
	      bcol (progn (goto-char bpos) (current-column))
	      tcol (progn (goto-char tpos) (current-column))
	      bullet (match-string 1)
	      bullet-type (if (string-match "[0-9]" bullet) "n" bullet))
	(if (not itemp)
	    (setq column tcol)
	  (goto-char pos)
	  (beginning-of-line 1)
	  (looking-at "[ \t]*\\(\\S-+\\)[ \t]*")
	  (setq bullet (match-string 1)
		btype (if (string-match "[0-9]" bullet) "n" bullet))
	  (setq column (if (equal btype bullet-type) bcol tcol))))
       (t (setq column (org-get-indentation))))))
    (goto-char pos)
    (if (<= (current-column) (current-indentation))
	(indent-line-to column)
      (save-excursion (indent-line-to column)))
    (setq column (current-column))
    (beginning-of-line 1)
    (if (looking-at
	 "\\([ \t]+\\)\\(:[0-9a-zA-Z]+:\\)[ \t]*\\(\\S-.*\\(\\S-\\|$\\)\\)")
	(replace-match (concat "\\1" (format org-property-format
					     (match-string 2) (match-string 3)))
		       t nil))
    (move-to-column column)))

(defun org-set-autofill-regexps ()
  (interactive)
  ;; In the paragraph separator we include headlines, because filling
  ;; text in a line directly attached to a headline would otherwise
  ;; fill the headline as well.
  (org-set-local 'comment-start-skip "^#+[ \t]*")
  (org-set-local 'paragraph-separate "\f\\|\\*+ \\|[ 	]*$\\|[ \t]*[:|]")
;; FIXME!!!!!!!  (org-set-local 'paragraph-separate "\f\\|[ 	]*$")
  ;; The paragraph starter includes hand-formatted lists.
  (org-set-local 'paragraph-start
		 "\f\\|[ 	]*$\\|\\*+ \\|\f\\|[ \t]*\\([-+*][ \t]+\\|[0-9]+[.)][ \t]+\\)\\|[ \t]*[:|]")
  ;; Inhibit auto-fill for headers, tables and fixed-width lines.
  ;; But only if the user has not turned off tables or fixed-width regions
  (org-set-local
   'auto-fill-inhibit-regexp
   (concat "\\*+ \\|#\\+"
	   "\\|[ \t]*" org-keyword-time-regexp
	   (if (or org-enable-table-editor org-enable-fixed-width-editor)
	       (concat
		"\\|[ \t]*["
		(if org-enable-table-editor "|" "")
		(if org-enable-fixed-width-editor ":"  "")
		"]"))))
  ;; We use our own fill-paragraph function, to make sure that tables
  ;; and fixed-width regions are not wrapped.  That function will pass
  ;; through to `fill-paragraph' when appropriate.
  (org-set-local 'fill-paragraph-function 'org-fill-paragraph)
  ; Adaptive filling: To get full control, first make sure that
  ;; `adaptive-fill-regexp' never matches.  Then install our own matcher.
  (org-set-local 'adaptive-fill-regexp "\000")
  (org-set-local 'adaptive-fill-function
		 'org-adaptive-fill-function))

(defun org-fill-paragraph (&optional justify)
  "Re-align a table, pass through to fill-paragraph if no table."
  (let ((table-p (org-at-table-p))
	(table.el-p (org-at-table.el-p)))
    (cond ((equal (char-after (point-at-bol)) ?*) t) ; skip headlines
	  (table.el-p t)                             ; skip table.el tables
	  (table-p (org-table-align) t)              ; align org-mode tables
	  (t nil))))                                 ; call paragraph-fill

;; For reference, this is the default value of adaptive-fill-regexp
;;  "[ \t]*\\([-|#;>*]+[ \t]*\\|(?[0-9]+[.)][ \t]*\\)*"

(defun org-adaptive-fill-function ()
  "Return a fill prefix for org-mode files.
In particular, this makes sure hanging paragraphs for hand-formatted lists
work correctly."
  (cond ((looking-at "#[ \t]+")
	 (match-string 0))
	((looking-at "[ \t]*\\([-*+] \\|[0-9]+[.)] \\)?")
	 (save-excursion
	   (goto-char (match-end 0))
	   (make-string (current-column) ?\ )))
	(t nil)))

;;;; Functions extending outline functionality

(defun org-beginning-of-line (&optional arg)
  "Go to the beginning of the current line.  If that is invisible, continue
to a visible line beginning.  This makes the function of C-a more intuitive.
If this is a headline, and `org-special-ctrl-a/e' is set, ignore tags on the
first attempt, and only move to after the tags when the cursor is already
beyond the end of the headline."
  (interactive "P")
  (let ((pos (point)))
    (beginning-of-line 1)
    (if (bobp)
	nil
      (backward-char 1)
      (if (org-invisible-p)
	  (while (and (not (bobp)) (org-invisible-p))
	    (backward-char 1)
	    (beginning-of-line 1))
	(forward-char 1)))
    (when org-special-ctrl-a/e
      (cond
       ((and (looking-at org-todo-line-regexp)
	     (= (char-after (match-end 1)) ?\ ))
	(goto-char
	 (if (eq org-special-ctrl-a/e t)
	     (cond ((> pos (match-beginning 3)) (match-beginning 3))
		   ((= pos (point)) (match-beginning 3))
		   (t (point)))
	   (cond ((> pos (point)) (point))
		 ((not (eq last-command this-command)) (point))
		 (t (match-beginning 3))))))
       ((org-at-item-p)
	(goto-char
	 (if (eq org-special-ctrl-a/e t)
	     (cond ((> pos (match-end 4)) (match-end 4))
		   ((= pos (point)) (match-end 4))
		   (t (point)))
	   (cond ((> pos (point)) (point))
		 ((not (eq last-command this-command)) (point))
		 (t (match-end 4))))))))))

(defun org-end-of-line (&optional arg)
  "Go to the end of the line.
If this is a headline, and `org-special-ctrl-a/e' is set, ignore tags on the
first attempt, and only move to after the tags when the cursor is already
beyond the end of the headline."
  (interactive "P")
  (if (or (not org-special-ctrl-a/e)
	  (not (org-on-heading-p)))
      (end-of-line arg)
    (let ((pos (point)))
      (beginning-of-line 1)
      (if (looking-at (org-re ".*?\\([ \t]*\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
	  (if (eq org-special-ctrl-a/e t)
	      (if (or (< pos (match-beginning 1))
		      (= pos (match-end 0)))
		  (goto-char (match-beginning 1))
		(goto-char (match-end 0)))
	    (if (or (< pos (match-end 0)) (not (eq this-command last-command)))
		(goto-char (match-end 0))
	      (goto-char (match-beginning 1))))
	(end-of-line arg)))))

(define-key org-mode-map "\C-a" 'org-beginning-of-line)
(define-key org-mode-map "\C-e" 'org-end-of-line)

(defun org-invisible-p ()
  "Check if point is at a character currently not visible."
  ;; Early versions of noutline don't have `outline-invisible-p'.
  (if (fboundp 'outline-invisible-p)
      (outline-invisible-p)
    (get-char-property (point) 'invisible)))

(defun org-invisible-p2 ()
  "Check if point is at a character currently not visible."
  (save-excursion
    (if (and (eolp) (not (bobp))) (backward-char 1))
    ;; Early versions of noutline don't have `outline-invisible-p'.
    (if (fboundp 'outline-invisible-p)
	(outline-invisible-p)
      (get-char-property (point) 'invisible))))

(defalias 'org-back-to-heading 'outline-back-to-heading)
(defalias 'org-on-heading-p 'outline-on-heading-p)
(defalias 'org-at-heading-p 'outline-on-heading-p)
(defun org-at-heading-or-item-p ()
  (or (org-on-heading-p) (org-at-item-p)))

(defun org-on-target-p ()
  (or (org-in-regexp org-radio-target-regexp)
      (org-in-regexp org-target-regexp)))

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (if (fboundp 'outline-up-heading-all)
      (outline-up-heading-all arg)   ; emacs 21 version of outline.el
    (outline-up-heading arg t)))     ; emacs 22 version of outline.el

(defun org-goto-sibling (&optional previous)
  "Goto the next sibling, even if it is invisible.
When PREVIOUS is set, go to the previous sibling instead.  Returns t
when a sibling was found.  When none is found, return nil and don't
move point."
  (let ((fun (if previous 're-search-backward 're-search-forward))
	(pos (point))
	(re (concat "^" outline-regexp))
	level l)
    (when (condition-case nil (org-back-to-heading t) (error nil))
      (setq level (funcall outline-level))
      (catch 'exit
	(or previous (forward-char 1))
	(while (funcall fun re nil t)
	  (setq l (funcall outline-level))
	  (when (< l level) (goto-char pos) (throw 'exit nil))
	  (when (= l level) (goto-char (match-beginning 0)) (throw 'exit t)))
	(goto-char pos)
	nil))))

(defun org-show-siblings ()
  "Show all siblings of the current headline."
  (save-excursion
    (while (org-goto-sibling) (org-flag-heading nil)))
  (save-excursion
    (while (org-goto-sibling 'previous)
      (org-flag-heading nil))))

(defun org-show-hidden-entry ()
  "Show an entry where even the heading is hidden."
  (save-excursion
    (org-show-entry)))

(defun org-flag-heading (flag &optional entry)
  "Flag the current heading.  FLAG non-nil means make invisible.
When ENTRY is non-nil, show the entire entry."
  (save-excursion
    (org-back-to-heading t)
    ;; Check if we should show the entire entry
    (if entry
	(progn
	  (org-show-entry)
	  (save-excursion
	    (and (outline-next-heading)
		 (org-flag-heading nil))))
      (outline-flag-region (max (point-min) (1- (point)))
			   (save-excursion (outline-end-of-heading) (point))
			   flag))))

(defun org-end-of-subtree (&optional invisible-OK to-heading)
  ;; This is an exact copy of the original function, but it uses
  ;; `org-back-to-heading', to make it work also in invisible
  ;; trees.  And is uses an invisible-OK argument.
  ;; Under Emacs this is not needed, but the old outline.el needs this fix.
  (org-back-to-heading invisible-OK)
  (let ((first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (unless to-heading
      (if (memq (preceding-char) '(?\n ?\^M))
	  (progn
	    ;; Go to end of line before heading
	    (forward-char -1)
	    (if (memq (preceding-char) '(?\n ?\^M))
		;; leave blank line before heading
		(forward-char -1))))))
  (point))

(defun org-show-subtree ()
  "Show everything after this heading at deeper levels."
  (outline-flag-region
   (point)
   (save-excursion
     (outline-end-of-subtree) (outline-next-heading) (point))
   nil))

(defun org-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (outline-flag-region
     (max (point-min) (1- (point)))
     (save-excursion
       (re-search-forward (concat "[\r\n]\\(" outline-regexp "\\)") nil 'move)
       (or (match-beginning 1) (point-max)))
     nil)))

(defun org-make-options-regexp (kwds)
  "Make a regular expression for keyword lines."
  (concat
   "^"
   "#?[ \t]*\\+\\("
   (mapconcat 'regexp-quote kwds "\\|")
   "\\):[ \t]*"
   "\\(.+\\)"))

;; Make isearch reveal the necessary context
(defun org-isearch-end ()
  "Reveal context after isearch exits."
  (when isearch-success ; only if search was successful
    (if (featurep 'xemacs)
	;; Under XEmacs, the hook is run in the correct place,
	;; we directly show the context.
	(org-show-context 'isearch)
      ;; In Emacs the hook runs *before* restoring the overlays.
      ;; So we have to use a one-time post-command-hook to do this.
      ;; (Emacs 22 has a special variable, see function `org-mode')
      (unless (and (boundp 'isearch-mode-end-hook-quit)
		   isearch-mode-end-hook-quit)
	;; Only when the isearch was not quitted.
	(org-add-hook 'post-command-hook 'org-isearch-post-command
		      'append 'local)))))

(defun org-isearch-post-command ()
  "Remove self from hook, and show context."
  (remove-hook 'post-command-hook 'org-isearch-post-command 'local)
  (org-show-context 'isearch))


;;;; Address problems with some other packages

;; Make flyspell not check words in links, to not mess up our keymap
(defun org-mode-flyspell-verify ()
  "Don't let flyspell put overlays at active buttons."
  (not (get-text-property (point) 'keymap)))

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
  (and (org-mode-p)
       (or (org-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (org-invisible-p)))
       (org-show-context 'bookmark-jump)))

;; Make session.el ignore our circular variable
(eval-after-load "session"
  '(add-to-list 'session-globals-exclude 'org-mark-ring))

;;;; Experimental code

;; Make appt aware of appointments from the agenda
(defun org-agenda-to-appt ()
  "Activate appointments found in `org-agenda-files'."
  (interactive)
  (require 'org)
  (let* ((today (org-date-to-gregorian
		 (time-to-days (current-time))))
	 (files org-agenda-files) entries file)
    (while (setq file (pop files))
      (setq entries (append entries (org-agenda-get-day-entries
				     file today :timestamp))))
    (setq entries (delq nil entries))
    (mapc (lambda(x)
	    (let* ((event (org-trim (get-text-property 1 'txt x)))
		   (time-of-day (get-text-property 1 'time-of-day x)) tod)
	      (when time-of-day
		(setq tod (number-to-string time-of-day)
		      tod (when (string-match 
				  "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)" tod)
			     (concat (match-string 1 tod) ":" 
				     (match-string 2 tod))))
		(if tod (appt-add tod event))))) entries)))

(defun org-closed-in-range ()
  "Sparse tree of items closed in a certain time range.
Still experimental, may disappear in the furture."
  (interactive)
  ;; Get the time interval from the user.
  (let* ((time1 (time-to-seconds
                 (org-read-date nil 'to-time nil "Starting date: ")))
         (time2 (time-to-seconds
                 (org-read-date nil 'to-time nil "End date:")))
         ;; callback function
         (callback (lambda ()
                     (let ((time
                            (time-to-seconds
                             (apply 'encode-time
                                    (org-parse-time-string
                                     (match-string 1))))))
                       ;; check if time in interval
                       (and (>= time time1) (<= time time2))))))
    ;; make tree, check each match with the callback
    (org-occur "CLOSED: +\\[\\(.*?\\)\\]" nil callback)))

(defun org-fill-paragraph-experimental (&optional justify)
  "Re-align a table, pass through to fill-paragraph if no table."
  (let ((table-p (org-at-table-p))
	(table.el-p (org-at-table.el-p)))
    (cond ((equal (char-after (point-at-bol)) ?*) t) ; skip headlines
	  (table.el-p t)                             ; skip table.el tables
	  (table-p (org-table-align) t)              ; align org-mode tables
	  ((save-excursion
	     (let ((pos (1+ (point-at-eol))))
	       (backward-paragraph 1)
	       (re-search-forward "\\\\\\\\[ \t]*$" pos t)))
	   (save-excursion
	     (save-restriction
	       (narrow-to-region (1+ (match-end 0)) (point-max))
	       (fill-paragraph nil)
	       t)))
	  (t nil))))                                 ; call paragraph-fill

(defun org-assign-fast-keys (alist)
  "Assign fast keys to a keyword-key alist.
Respect keys that are already there."
  (let (new e k c c1 c2 (char ?a))
    (while (setq e (pop alist))
      (cond
       ((equal e '(:startgroup)) (push e new))
       ((equal e '(:endgroup)) (push e new))
       (t
	(setq k (car e) c2 nil)
	(if (cdr e)
	    (setq c (cdr e))
	  ;; automatically assign a character.
	  (setq c1 (string-to-char
		    (downcase (substring
			       k (if (= (string-to-char k) ?@) 1 0)))))
	  (if (or (rassoc c1 new) (rassoc c1 alist))
	      (while (or (rassoc char new) (rassoc char alist))
		(setq char (1+ char)))
	    (setq c2 c1))
	  (setq c (or c2 char)))
	(push (cons k c) new))))
    (nreverse new)))

;;;; Finish up

(provide 'org)

(run-hooks 'org-load-hook)

;; arch-tag: e77da1a7-acc7-4336-b19e-efa25af3f9fd
;;; org.el ends here

