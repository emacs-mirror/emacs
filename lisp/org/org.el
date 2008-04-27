;;; org.el --- Outline-based notes management and organizer
;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
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
;; http://orgmode.org/Changes.html
;;
;;; Code:

(defvar org-inhibit-highlight-removal nil) ; dynamically scoped param
(defvar org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")
(make-variable-buffer-local 'org-table-formula-constants-local)

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
(unless (fboundp 'time-subtract) (defalias 'time-subtract 'subtract-time))
(require 'easymenu)

(require 'org-macs)
(require 'org-compat)
(require 'org-faces)

;;;; Customization variables

;;; Version

(defconst org-version "6.02b"
  "The version number of the file org.el.")

(defun org-version (&optional here)
  "Show the org-mode version in the echo area.
With prefix arg HERE, insert it at point."
  (interactive "P")
  (let ((version (format "Org-mode version %s" org-version)))
    (message version)
    (if here
	(insert version))))

;;; Compatibility constants

;;; The custom variables

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'hypermedia
  :group 'calendar)

(defcustom org-load-hook nil
  "Hook that is run after org.el has been loaded."
  :group 'org
  :type 'hook)

(defvar org-modules)  ; defined below
(defvar org-modules-loaded nil
  "Have the modules been loaded already?")

(defun org-load-modules-maybe (&optional force)
  "Load all extensions listed in `org-default-extensions'."
  (when (or force (not org-modules-loaded))
    (mapc (lambda (ext)
	    (condition-case nil (require ext)
	      (error (message "Problems while trying to load feature `%s'" ext))))
	  org-modules)
    (setq org-modules-loaded t)))

(defun org-set-modules (var value)
  "Set VAR to VALUE and call `org-load-modules-maybe' with the force flag."
  (set var value)
  (when (featurep 'org)
    (org-load-modules-maybe 'force)))

(defcustom org-modules '(org-bbdb org-bibtex org-gnus org-info org-infojs org-irc org-mew org-mhe org-rmail org-vm org-wl)
  "Modules that should always be loaded together with org.el.
If a description starts with <C>, the file is not part of emacs
and loading it will require that you have downloaded and properly installed
the org-mode distribution.

You can also use this system to load external packages (i.e. neither Org
core modules, not modules from the CONTRIB directory).  Just add symbols
to the end of the list.  If the package is called org-xyz.e, then you need
to add the symbol `xyz', and the package must have a call to

   (provide 'org-xyz)"
  :group 'org
  :set 'org-set-modules
  :type
  '(set :greedy t
	(const :tag "   bbdb:              Links to BBDB entries" org-bbdb)
	(const :tag "   bibtex:            Links to BibTeX entries" org-bibtex)
	(const :tag "   gnus:              Links to GNUS folders/messages" org-gnus)
	(const :tag "   info:              Links to Info nodes" org-info)
	(const :tag "   infojs:            Set up Sebastian Rose's JavaScript org-info.js" org-infojs)
	(const :tag "   irc:               Links to IRC/ERC chat sessions" org-irc)
	(const :tag "   mac-message:       Links to messages in Apple Mail" org-mac-message)
	(const :tag "   mew                Links to Mew folders/messages" org-mew)
	(const :tag "   mhe:               Links to MHE folders/messages" org-mhe)
	(const :tag "   rmail:             Links to RMAIL folders/messages" org-rmail)
	(const :tag "   vm:                Links to VM folders/messages" org-vm)
	(const :tag "   wl:                Links to Wanderlust folders/messages" org-wl)
	(const :tag "   mouse:             Additional mouse support" org-mouse)

	(const :tag "C  annotate-file:     Annotate a file with org syntax" org-annotate-file)
	(const :tag "C  bookmark:          Org links to bookmarks" org-bookmark)
	(const :tag "C  depend:            TODO dependencies for Org-mode" org-depend)
	(const :tag "C  elisp-symbol:      Org links to emacs-lisp symbols" org-elisp-symbol)
	(const :tag "C  expiry:            Expiry mechanism for Org entries" org-expiry)
	(const :tag "C  id:                Global id's for identifying entries" org-id)
	(const :tag "C  interactive-query: Interactive modification of tags query" org-interactive-query)
	(const :tag "C  mairix:            Hook mairix search into Org for different MUAs" org-mairix)
	(const :tag "C  man:               Support for links to manpages in Org-mode" org-man)
	(const :tag "C  mew:               Support for links to messages in Mew" org-mew)
	(const :tag "C  panel:             Simple routines for us with bad memory" org-panel)
	(const :tag "C  registry:          A registry for Org links" org-registry)
	(const :tag "C  org2rem:           Convert org appointments into reminders" org2rem)
	(const :tag "C  screen:            Visit screen sessions through Org-mode links" org-screen)
	(const :tag "C  toc:               Table of contents for Org-mode buffer" org-toc)
	(const :tag "C  sqlinsert:         Convert Org-mode tables to SQL insertions" orgtbl-sqlinsert)
	(repeat :tag "External packages" :inline t (symbol :tag "Package"))))


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
  "<[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*\\([.+]?\\+[0-9]+[dwmy]\\)"
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

(defconst org-context-choice
  '(choice
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
	     (boolean))))
  "Contexts for the reveal options.")

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
  :type org-context-choice)

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
  :type org-context-choice)

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
  :type org-context-choice)

(defcustom org-show-entry-below '((default . nil))
  "Non-nil means, show the entry below a headline when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the text below the headline that is
exposed is also shown.

By default this is off for all contexts.
Instead of t, this can also be an alist specifying this option for different
contexts.  See `org-show-hierarchy-above' for valid contexts."
  :group 'org-reveal-location
  :type org-context-choice)

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?
This applies to indirect buffers created with the commands
\\[org-tree-to-indirect-buffer] and \\[org-agenda-tree-to-indirect-buffer].
Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and re-use it each time.
new-frame        Make a new frame each time.  Note that in this case
                 previously-made indirect buffers are kept, and you need to
                 kill these buffers yourself."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defgroup org-cycle nil
  "Options concerning visibility cycling in Org-mode."
  :tag "Org Cycle"
  :group 'org-structure)

(defcustom org-drawers '("PROPERTIES" "CLOCK")
  "Names of drawers.  Drawers are not opened by cycling on the headline above.
Drawers only open with a TAB on the drawer line itself.  A drawer looks like
this:
   :DRAWERNAME:
   .....
   :END:
The drawer \"PROPERTIES\" is special for capturing properties through
the property API.

Drawers can be defined on the per-file basis with a line like:

#+DRAWERS: HIDDEN STATE PROPERTIES"
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
whitestart  Only at the beginning of lines, before the first non-white char
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

(defcustom org-special-ctrl-k nil
  "Non-nil means `C-k' will behave specially in headlines.
When nil, `C-k' will call the default `kill-line' command.
When t, the following will happen while the cursor is in the headline:

- When the cursor is at the beginning of a headline, kill the entire
  line and possible the folded subtree below the line.
- When in the middle of the headline text, kill the headline up to the tags.
- When after the headline text, kill the tags."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-M-RET-may-split-line '((default . t))
  "Non-nil means, M-RET will split the line at the cursor position.
When nil, it will go to the end of the line before making a
new line.
You may also set this option in a different way for different
contexts.  Valid contexts are:

headline  when creating a new headline
item      when creating a new item
table     in a table field
default   the value to be used for all contexts not explicitly
          customized"
  :group 'org-structure
  :group 'org-table
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const headline)
			   (const item)
			   (const table)
			   (const default))
		   (boolean)))))


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
  :type 'hook)

(defcustom org-enable-fixed-width-editor t
  "Non-nil means, lines starting with \":\" are treated as fixed-width.
This currently only means, they are never auto-wrapped.
When nil, such lines will be treated like ordinary lines.
See also the QUOTE keyword."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-goto-auto-isearch t
  "Non-nil means, typing characters in org-goto starts incremental search."
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

(defcustom org-empty-line-terminates-plain-lists nil
  "Non-nil means, an empty line ends all plain list levels.
When nil, empty lines are part of the preceeding item."
  :group 'org-plain-lists
  :type 'boolean)

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


(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org-mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org-mode headlines.
This also applied for speedbar access."
  :group 'org-imenu-and-speedbar
  :type 'number)

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

(defcustom org-table-tab-recognizes-table.el t
  "Non-nil means, TAB will automatically notice a table.el table.
When it sees such a table, it moves point into it and - if necessary -
calls `table-recognize-table'."
  :group 'org-table-editing
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
If the string contains \"%s\", the tag will be inserted there.

REPLACE may also be a function that will be called with the tag as the
only argument to create the link, which should be returned as a string.

See the manual for examples."
  :group 'org-link
  :type 'alist)

(defcustom org-descriptive-links t
  "Non-nil means, hide link part and only show description of bracket links.
Bracket links are like [[link][descritpion]].  This variable sets the initial
state in new org-mode buffers.  The setting can then be toggled on a
per-buffer basis from the Org->Hyperlinks menu."
  :group 'org-link
  :type 'boolean)

(defcustom org-link-file-path-type 'adaptive
  "How the path name in file links should be stored.
Valid values are:

relative  Relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  Absolute path, if possible with ~ for home directory.
noabbrev  Absolute path, no abbreviation of home directory.
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
	      (const :tag "Plain text links" plain)
	      (const :tag "Radio target matches" radio)
	      (const :tag "Tags" tag)
	      (const :tag "Timestamps" date)))

(defcustom org-make-link-description-function nil
  "Function to use to generate link descriptions from links. If
nil the link location will be used. This function must take two
parameters; the first is the link and the second the description
org-insert-link has generated, and should return the description
to use."
  :group 'org-link
  :type 'function)

(defgroup org-link-store nil
  "Options concerning storing links in Org-mode."
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

(defgroup org-link-follow nil
  "Options concerning following links in Org-mode."
  :tag "Org Follow Link"
  :group 'org-link)

(defcustom org-follow-link-hook nil
  "Hook that is run after a link has been followed."
  :group 'org-link-follow
  :type 'hook)

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

(defcustom org-mouse-1-follows-link
  (if (boundp 'mouse-1-click-follows-link) mouse-1-click-follows-link t)
  "Non-nil means, mouse-1 on a link will follow the link.
A longer mouse click will still set point.  Does not work on XEmacs.
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
Shell links can be dangerous: just think about a link

     [[shell:rm -rf ~/*][Google Search]]

This link would show up in your Org-mode document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' of you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))

(defcustom org-confirm-elisp-link-function 'yes-or-no-p
  "Non-nil means, ask for confirmation before executing Emacs Lisp links.
Elisp links can be dangerous: just think about a link

     [[elisp:(shell-command \"rm -rf ~/*\")][Google Search]]

This link would show up in your Org-mode document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' of you want to confirm with a
single keystroke rather than having to type \"yes\"."
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

(defgroup org-refile nil
  "Options concerning refiling entries in Org-mode."
  :tag "Org Remember"
  :group 'org)

(defcustom org-directory "~/org"
  "Directory with org files.
This directory will be used as default to prompt for org files.
Used by the hooks for remember.el."
  :group 'org-refile
  :group 'org-remember
  :type 'directory)

(defcustom org-default-notes-file "~/.notes"
  "Default target for storing notes.
Used by the hooks for remember.el.  This can be a string, or nil to mean
the value of `remember-data-file'.
You can set this on a per-template basis with the variable
`org-remember-templates'."
  :group 'org-refile
  :group 'org-remember
  :type '(choice
	  (const :tag "Default from remember-data-file" nil)
	  file))

(defcustom org-goto-interface 'outline
  "The default interface to be used for `org-goto'.
Allowed vaues are:
outline                  The interface shows an outline of the relevant file
                         and the correct heading is found by moving through
                         the outline or by searching with incremental search.
outline-path-completion  Headlines in the current buffer are offered via
                         completion."
  :group 'org-refile
  :type '(choice
	  (const :tag "Outline" outline)
	  (const :tag "Outline-path-completion" outline-path-completion)))

(defcustom org-reverse-note-order nil
  "Non-nil means, store new notes at the beginning of a file or entry.
When nil, new notes will be filed to the end of a file or entry.
This can also be a list with cons cells of regular expressions that
are matched against file names, and values."
  :group 'org-remember
  :type '(choice
	  (const :tag "Reverse always" t)
	  (const :tag "Reverse never" nil)
	  (repeat :tag "By file name regexp"
		  (cons regexp boolean))))

(defcustom org-refile-targets nil
  "Targets for refiling entries with \\[org-refile].
This is list of cons cells.  Each cell contains:
- a specification of the files to be considered, either a list of files,
  or a symbol whose function or variable value will be used to retrieve
  a file name or a list of file names.  Nil means, refile to a different
  heading in the current buffer.
- A specification of how to find candidate refile targets.  This may be
  any of
  - a cons cell (:tag . \"TAG\") to identify refile targets by a tag.
    This tag has to be present in all target headlines, inheritance will
    not be considered.
  - a cons cell (:todo . \"KEYWORD\") to identify refile targets by
    todo keyword.
  - a cons cell (:regexp . \"REGEXP\") with a regular expression matching
    headlines that are refiling targets.
  - a cons cell (:level . N).  Any headline of level N is considered a target.
  - a cons cell (:maxlevel . N). Any headline with level <= N is a target."
  :group 'org-remember
  :type '(repeat
	  (cons
	   (choice :value org-agenda-files
		   (const :tag "All agenda files" org-agenda-files)
		   (const :tag "Current buffer" nil)
		   (function) (variable) (file))
	   (choice :tag "Identify target headline by"
	    (cons :tag "Specific tag" (const :tag) (string))
	    (cons :tag "TODO keyword" (const :todo) (string))
	    (cons :tag "Regular expression" (const :regexp) (regexp))
	    (cons :tag "Level number" (const :level) (integer))
	    (cons :tag "Max Level number" (const :maxlevel) (integer))))))

(defcustom org-refile-use-outline-path nil
  "Non-nil means, provide refile targets as paths.
So a level 3 headline will be available as level1/level2/level3.
When the value is `file', also include the file name (without directory)
into the path.  When `full-file-path', include the full file path."
  :group 'org-remember
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Yes" t)
	  (const :tag "Start with file name" file)
	  (const :tag "Start with full file path" full-file-path)))

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

Each keyword can optionally specify a character for fast state selection
\(in combination with the variable `org-use-fast-todo-selection')
and specifiers for state change logging, using the same syntax
that is used in the \"#+TODO:\" lines.  For example, \"WAIT(w)\" says
that the WAIT state can be selected with the \"w\" key. \"WAIT(w!)\"
indicates to record a time stamp each time this state is selected.

Each keyword may also specify if a timestamp or a note should be
recorded when entering or leaving the state, by adding additional
characters in the parenthesis after the keyword.  This looks like this:
\"WAIT(w@/!)\".  \"@\" means to add a note (with time), \"!\" means to
record only the time of the state change.  With X and Y being either
\"@\" or \"!\", \"X/Y\" means use X when entering the state, and use
Y when leaving the state if and only if the *target* state does not
define X.  You may omit any of the fast-selection key or X or /Y,
so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

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

(defvar org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(make-variable-buffer-local 'org-todo-keywords-1)
(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-agenda-contributing-files nil)
(defvar org-not-done-keywords nil)
(make-variable-buffer-local 'org-not-done-keywords)
(defvar org-done-keywords nil)
(make-variable-buffer-local 'org-done-keywords)
(defvar org-todo-heads nil)
(make-variable-buffer-local 'org-todo-heads)
(defvar org-todo-sets nil)
(make-variable-buffer-local 'org-todo-sets)
(defvar org-todo-log-states nil)
(make-variable-buffer-local 'org-todo-log-states)
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
  "Non-nil means, record a CLOSED timestamp when moving an entry to DONE.
When equal to the list (done), also prompt for a closing note.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: logdone
   #+STARTUP: lognotedone
   #+STARTUP: nologdone"
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record CLOSED timestamp" time)
	  (const :tag "Record CLOSED timestamp with closing note." note)))

;; Normalize old uses of org-log-done.
(cond
 ((eq org-log-done t) (setq org-log-done 'time))
 ((and (listp org-log-done) (memq 'done org-log-done))
  (setq org-log-done 'note)))

(defcustom org-log-note-clock-out nil
  "Non-nil means, recored a note when clocking out of an item.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: lognoteclock-out
   #+STARTUP: nolognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-done-with-time t
  "Non-nil means, the CLOSED time stamp will contain date and time.
When nil, only the date will be recorded."
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-note-headings
  '((done .  "CLOSING NOTE %t")
    (state . "State %-12s %t")
    (note .  "Note taken on %t")
    (clock-out . ""))
  "Headings for notes added to entries.
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
	  (cons (const :tag "Heading when just taking a note" note) string)
	  (cons (const :tag "Heading when clocking out" clock-out) string)))

(unless (assq 'note org-log-note-headings)
  (push '(note . "%t") org-log-note-headings))

(defcustom org-log-states-order-reversed t
  "Non-nil means, the latest state change note will be directly after heading.
When nil, the notes will be orderer according to time."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-repeat 'time
  "Non-nil means, record moving through the DONE state when triggering repeat.
An auto-repeating tasks  is immediately switched back to TODO when marked
done.  If you are not logging state changes (by adding \"@\" or \"!\" to
the TODO keyword definition, or recording a cloing note by setting
`org-log-done', there will be no record of the task moving trhough DONE.
This variable forces taking a note anyway.  Possible values are:

nil     Don't force a record
time    Record a time stamp
note    Record a note

This option can also be set with on a per-file-basis with

   #+STARTUP: logrepeat
   #+STARTUP: lognoterepeat
   #+STARTUP: nologrepeat

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Don't force a record" nil)
	  (const :tag "Force recording the DONE state" time)
	  (const :tag "Force recording a note with the DONE state" note)))


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

(defcustom org-priority-start-cycle-with-default t
  "Non-nil means, start with default priority when starting to cycle.
When this is nil, the first step in the cycle will be (depending on the
command used) one higher or lower that the default priority."
  :group 'org-priorities
  :type 'boolean)

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

(defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
  "Formats for `format-time-string' which are used for time stamps.
It is not recommended to change this constant.")

(defcustom org-time-stamp-rounding-minutes '(0 5)
  "Number of minutes to round time stamps to.
These are two values, the first applies when first creating a time stamp.
The second applies when changing it with the commands `S-up' and `S-down'.
When changing the time stamp, this means that it will change in steps
of N minutes, as given by the second value.

When a setting is 0 or 1, insert the time unmodified.  Useful rounding
numbers should be factors of 60, so for example 5, 10, 15.

When this is larger than 1, you can still force an exact time-stamp by using
a double prefix argument to a time-stamp command like `C-c .' or `C-c !',
and by using a prefix arg to `S-up/down' to specify the exact number
of minutes to shift."
  :group 'org-time
  :get '(lambda (var) ; Make sure all entries have 5 elements
	  (if (integerp (default-value var))
	      (list (default-value var) 5)
	    (default-value var)))
  :type '(list
	  (integer :tag "when inserting times")
	  (integer :tag "when modifying times")))

;; Normalize old customizations of this variable.
(when (integerp org-time-stamp-rounding-minutes)
  (setq org-time-stamp-rounding-minutes
	(list org-time-stamp-rounding-minutes
	      org-time-stamp-rounding-minutes)))

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
When 0 or negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :type 'number)

(defcustom org-read-date-prefer-future t
  "Non-nil means, assume future for incomplete date input from user.
This affects the following situations:
1. The user gives a day, but no month.
   For example, if today is the 15th, and you enter \"3\", Org-mode will
   read this as the third of *next* month.  However, if you enter \"17\",
   it will be considered as *this* month.
2. The user gives a month but not a year.
   For example, if it is april and you enter \"feb 2\", this will be read
   as feb 2, *next* year.  \"May 5\", however, will be this year.

Currently this does not work for ISO week specifications.

When this option is nil, the current month and year will always be used
as defaults."
  :group 'org-time
  :type 'boolean)

(defcustom org-read-date-display-live t
  "Non-nil means, display current interpretation of date prompt live.
This display will be in an overlay, in the minibuffer."
  :group 'org-time
  :type 'boolean)

(defcustom org-read-date-popup-calendar t
  "Non-nil means, pop up a calendar when prompting for a date.
In the calendar, the date can be selected with mouse-1.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)
(if (fboundp 'defvaralias)
    (defvaralias 'org-popup-calendar-for-date-prompt
      'org-read-date-popup-calendar))

(defcustom org-extend-today-until 0
  "The hour when your day really ends.
This has influence for the following applications:
- When switching the agenda to \"today\".  It it is still earlier than
  the time given here, the day recognized as TODAY is actually yesterday.
- When a date is read from the user and it is still before the time given
  here, the current date and time will be assumed to be yesterday, 23:59.

FIXME:
IMPORTANT:  This is still a very experimental feature, it may disappear
again or it may be extended to mean more things."
  :group 'org-time
  :type 'number)

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

(defvar org-fast-tag-selection-include-todo nil
  "Non-nil means, fast tags selection interface will also offer TODO states.
This is an undocumented feature, you should not rely on it.")

(defcustom org-tags-column (if (featurep 'xemacs) -79 -80)
  "The column to which tags should be indented in a headline.
If this number is positive, it specifies the column.  If it is negative,
it means that the tags should be flushright to that column.  For example,
-80 works well for a normal 80 character screen."
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
companion option `org-tags-match-list-sublevels'.

This may also be a list of tags that should be inherited, or a regexp that
matches tags that should be inherited."
  :group 'org-tags
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific tags" (string :tag "Tag"))
	  (regexp :tag "Tags matched by regexp")))

(defun org-tag-inherit-p (tag)
  "Check if TAG is one that should be inherited."
  (cond
   ((eq org-use-tag-inheritance t) t)
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (string-match org-use-tag-inheritance tag))
   ((listp org-use-tag-inheritance)
    (member tag org-use-tag-inheritance))
   (t (error "Invalid setting of `org-use-tag-inheritance'"))))

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
(defvar org-after-tags-change-hook nil
  "Hook that is run after the tags in a line have changed.")

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

(defcustom org-use-property-inheritance nil
  "Non-nil means, properties apply also for sublevels.

This setting is chiefly used during property searches. Turning it on can
cause significant overhead when doing a search, which is why it is not
on by default.

When nil, only the properties directly given in the current entry count.
When t, every property is inherited.  The value may also be a list of
properties that should have inheritance, or a regular expression matching
properties that should be inherited.

However, note that some special properties use inheritance under special
circumstances (not in searches).  Examples are CATEGORY, ARCHIVE, COLUMNS,
and the properties ending in \"_ALL\" when they are used as descriptor
for valid values of a property.

Note for programmers:
When querying an entry with `org-entry-get',  you can control if inheritance
should be used.  By default, `org-entry-get' looks only at the local
properties.  You can request inheritance by setting the inherit argument
to t (to force inheritance) or to `selective' (to respect the setting
in this variable)."
  :group 'org-properties
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific properties" (string :tag "Property"))
	  (regexp :tag "Properties matched by regexp")))

(defun org-property-inherit-p (property)
  "Check if PROPERTY is one that should be inherited."
  (cond
   ((eq org-use-property-inheritance t) t)
   ((not org-use-property-inheritance) nil)
   ((stringp org-use-property-inheritance)
    (string-match org-use-property-inheritance property))
   ((listp org-use-property-inheritance)
    (member property org-use-property-inheritance))
   (t (error "Invalid setting of `org-use-property-inheritance'"))))

(defcustom org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
  "The default column format, if no other format has been defined.
This variable can be set on the per-file basis by inserting a line

#+COLUMNS: %25ITEM ....."
  :group 'org-properties
  :type 'string)

(defcustom org-effort-property "Effort"
  "The property that is being used to keep track of effort estimates.
Effort estimates given in this property need to have the format H:MM."
  :group 'org-properties
  :group 'org-progress
  :type '(string :tag "Property"))

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

If an entry is a directory, all files in that directory that are matched by
`org-agenda-file-regexp' will be part of the file list.

If the value of the variable is not a list but a single file name, then
the list of agenda files is actually stored and maintained in that file, one
agenda file per line."
  :group 'org-agenda
  :type '(choice
	  (repeat :tag "List of files and directories" file)
	  (file :tag "Store list in a file\n" :value "~/.agenda_files")))

(defcustom org-agenda-file-regexp "\\`[^.].*\\.org\\'"
  "Regular expression to match files for `org-agenda-files'.
If any element in the list in that variable contains a directory instead
of a normal file, all files in that directory that are matched by this
regular expression will be included."
  :group 'org-agenda
  :type 'regexp)

(defcustom org-agenda-text-search-extra-files nil
  "List of extra files to be searched by text search commands.
These files will be search in addition to the agenda files by the
commands `org-search-view' (`C-c a s') and `org-occur-in-agenda-files'.
Note that these files will only be searched for text search commands,
not for the other agenda views like todo lists, tag searches or the weekly
agenda.  This variable is intended to list notes and possibly archive files
that should also be searched by these two commands.
In fact, if the first element in the list is the symbol `agenda-archives',
than all archive files of all agenda files will be added to the search
scope."
  :group 'org-agenda
  :type '(set :greedy t
	   (const :tag "Agenda Archives" agenda-archives)
	   (repeat :inline t (file))))

(if (fboundp 'defvaralias)
    (defvaralias 'org-agenda-multi-occur-extra-files
      'org-agenda-text-search-extra-files))

(defcustom org-agenda-skip-unavailable-files nil
  "t means to just skip non-reachable files in `org-agenda-files'.
Nil means to remove them, after a query, from the list."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-calendar-to-agenda-key [?c]
  "The key to be installed in `calendar-mode-map' for switching to the agenda.
The command `org-calendar-goto-agenda' will be bound to this key.  The
default is the character `c' because then `c' can be used to switch back and
forth between agenda and calendar."
  :group 'org-agenda
  :type 'sexp)

(eval-after-load "calendar"
  '(org-defkey calendar-mode-map org-calendar-to-agenda-key
     'org-calendar-goto-agenda))

(defgroup org-latex nil
  "Options for embedding LaTeX code into Org-mode."
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
  :group 'org-latex
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
  :group 'org-latex
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

(defcustom org-highlight-latex-fragments-and-specials nil
  "Non-nil means, fontify what is treated specially by the exporters."
  :group 'org-font-lock
  :type 'boolean)

(defcustom org-hide-emphasis-markers nil
  "Non-nil mean font-lock should hide the emphasis marker characters."
  :group 'org-font-lock
  :type 'boolean)

(defvar org-emph-re nil
  "Regular expression for matching emphasis.")
(defvar org-verbatim-re nil
  "Regular expression for matching verbatim text.")
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
	   (stacked (and nil (nth 5 e))) ; stacked is no longer allowed, forced to nil
	   (body1 (concat body "*?"))
	   (markers (mapconcat 'car org-emphasis-alist ""))
	   (vmarkers (mapconcat
		      (lambda (x) (if (eq (nth 4 x) 'verbatim) (car x) ""))
		      org-emphasis-alist "")))
      ;; make sure special characters appear at the right position in the class
      (if (string-match "\\^" markers)
	  (setq markers (concat (replace-match "" t t markers) "^")))
      (if (string-match "-" markers)
	  (setq markers (concat (replace-match "" t t markers) "-")))
      (if (string-match "\\^" vmarkers)
	  (setq vmarkers (concat (replace-match "" t t vmarkers) "^")))
      (if (string-match "-" vmarkers)
	  (setq vmarkers (concat (replace-match "" t t vmarkers) "-")))
      (if (> nl 0)
          (setq body1 (concat body1 "\\(?:\n" body "*?\\)\\{0,"
                              (int-to-string nl) "\\}")))
      ;; Make the regexp
      (setq org-emph-re
	    (concat "\\([" pre (if (and nil stacked) markers) "]\\|^\\)"
		    "\\("
		    "\\([" markers "]\\)"
		    "\\("
		    "[^" border "]\\|"
		    "[^" border (if (and nil stacked) markers) "]"
		    body1
		    "[^" border (if (and nil stacked) markers) "]"
		    "\\)"
		    "\\3\\)"
		    "\\([" post (if (and nil stacked) markers) "]\\|$\\)"))
      (setq org-verbatim-re
	    (concat "\\([" pre "]\\|^\\)"
		    "\\("
		    "\\([" vmarkers "]\\)"
		    "\\("
		    "[^" border "]\\|"
		    "[^" border "]"
		    body1
		    "[^" border "]"
		    "\\)"
		    "\\3\\)"
		    "\\([" post  "]\\|$\\)")))))

(defcustom org-emphasis-regexp-components
  '(" \t('\"" "- \t.,:?;'\")" " \t\r\n,\"'" "." 1)
  "Components used to build the regular expression for emphasis.
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

Use customize to modify this, or restart Emacs after changing it."
  :group 'org-font-lock
  :set 'org-set-emph-re
  :type '(list
	  (sexp    :tag "Allowed chars in pre      ")
	  (sexp    :tag "Allowed chars in post     ")
	  (sexp    :tag "Forbidden chars in border ")
	  (sexp    :tag "Regexp for body           ")
	  (integer :tag "number of newlines allowed")
	  (option (boolean :tag "Stacking (DISABLED)       "))))

(defcustom org-emphasis-alist
  `(("*" bold "<b>" "</b>")
    ("/" italic "<i>" "</i>")
    ("_" underline "<u>" "</u>")
    ("=" org-code "<code>" "</code>" verbatim)
    ("~" org-verbatim "" "" verbatim)
    ("+" ,(if (featurep 'xemacs) 'org-table '(:strike-through t))
     "<del>" "</del>")
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
	   (string :tag "HTML end tag")
	   (option (const verbatim)))))

;;; Miscellaneous options

(defgroup org-completion nil
  "Completion in Org-mode."
  :tag "Org Completion"
  :group 'org)

(defcustom org-completion-fallback-command 'hippie-expand
  "The expansion command called by \\[org-complete] in normal context.
Normal means, no org-mode-specific context."
  :group 'org-completion
  :type 'function)

;;; Functions and variables from ther packages
;;  Declared here to avoid compiler warnings

;; XEmacs only
(defvar outline-mode-menu-heading)
(defvar outline-mode-menu-show)
(defvar outline-mode-menu-hide)
(defvar zmacs-regions) ; XEmacs regions

;; Emacs only
(defvar mark-active)

;; Various packages
(declare-function calendar-absolute-from-iso    "cal-iso"    (&optional date))
(declare-function calendar-forward-day          "cal-move"   (arg))
(declare-function calendar-goto-date            "cal-move"   (date))
(declare-function calendar-goto-today           "cal-move"   ())
(declare-function calendar-iso-from-absolute    "cal-iso"    (&optional date))
(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(declare-function cdlatex-tab "ext:cdlatex" ())
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(defvar font-lock-unfontify-region-function)
(declare-function iswitchb-mode "iswitchb" (&optional arg))
(declare-function iswitchb-read-buffer (prompt &optional default require-match start matches-set))
(defvar iswitchb-temp-buflist)
(declare-function org-gnus-follow-link "org-gnus" (&optional group article))
(declare-function org-agenda-skip "org-agenda" ())
(declare-function org-format-agenda-item "org-agenda"
		  (extra txt &optional category tags dotime noprefix remove-re))
(declare-function org-agenda-new-marker "org-agenda" (&optional pos))
(declare-function org-agenda-change-all-lines "org-agenda"
		  (newhead hdmarker &optional fixface))
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-maybe-redo "org-agenda" ())
(declare-function parse-time-string "parse-time" (string))
(declare-function remember "remember" (&optional initial))
(declare-function remember-buffer-desc "remember" ())
(declare-function remember-finalize "remember" ())
(defvar remember-save-after-remembering)
(defvar remember-data-file)
(defvar remember-register)
(defvar remember-buffer)
(defvar remember-handler-functions)
(defvar remember-annotation-functions)
(defvar texmathp-why)
(declare-function speedbar-line-directory "speedbar" (&optional depth))
(declare-function table--at-cell-p "table" (position &optional object at-column))

(defvar w3m-current-url)
(defvar w3m-current-title)

(defvar org-latex-regexps)

;;; Autoload and prepare some org modules

;; Some table stuff that needs to be defined here, because it is used
;; by the functions setting up org-mode or checking for table context.

(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detects an org-type or table-type table.")
(defconst org-table-line-regexp "^[ \t]*|"
  "Detects an org-type table line.")
(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detects an org-type table line.")
(defconst org-table-hline-regexp "^[ \t]*|-"
  "Detects an org-type table hline.")
(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detects a table-type table hline.")
(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Searching from within a table (any type) this finds the first line
outside the table.")

;; Autoload the functions in org-table.el that are needed by functions here.

(eval-and-compile
  (org-autoload "org-table"
		'(org-table-align org-table-begin org-table-blank-field
   org-table-convert org-table-convert-region org-table-copy-down
   org-table-copy-region org-table-create
   org-table-create-or-convert-from-region
   org-table-create-with-table.el org-table-current-dline
   org-table-cut-region org-table-delete-column org-table-edit-field
   org-table-edit-formulas org-table-end org-table-eval-formula
   org-table-export org-table-field-info
   org-table-get-stored-formulas org-table-goto-column
   org-table-hline-and-move org-table-import org-table-insert-column
   org-table-insert-hline org-table-insert-row org-table-iterate
   org-table-justify-field-maybe org-table-kill-row
   org-table-maybe-eval-formula org-table-maybe-recalculate-line
   org-table-move-column org-table-move-column-left
   org-table-move-column-right org-table-move-row
   org-table-move-row-down org-table-move-row-up
   org-table-next-field org-table-next-row org-table-paste-rectangle
   org-table-previous-field org-table-recalculate
   org-table-rotate-recalc-marks org-table-sort-lines org-table-sum
   org-table-toggle-coordinate-overlays
   org-table-toggle-formula-debugger org-table-wrap-region
   orgtbl-mode turn-on-orgtbl)))

(defun org-at-table-p (&optional table-type)
  "Return t if the cursor is inside an org-type table.
If TABLE-TYPE is non-nil, also check for table.el-type tables."
  (if org-enable-table-editor
      (save-excursion
	(beginning-of-line 1)
	(looking-at (if table-type org-table-any-line-regexp
		      org-table-line-regexp)))
    nil))
(defsubst org-table-p () (org-at-table-p))

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

(defvar org-table-clean-did-remove-column nil)

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

;; Declare and autoload functions from org-exp.el

(declare-function org-default-export-plist "org-exp")
(declare-function org-infile-export-plist "org-exp")
(declare-function org-get-current-options "org-exp")
(eval-and-compile
  (org-autoload "org-exp"
		'(org-export org-export-as-ascii org-export-visible
   org-insert-export-options-template org-export-as-html-and-open
   org-export-as-html-batch org-export-as-html-to-buffer
   org-replace-region-by-html org-export-region-as-html
   org-export-as-html org-export-icalendar-this-file
   org-export-icalendar-all-agenda-files
   org-export-icalendar-combine-agenda-files org-export-as-xoxo)))

;; Declare and autoload functions from org-exp.el

(eval-and-compile
  (org-autoload "org-exp"
		'(org-agenda org-agenda-list org-search-view
   org-todo-list org-tags-view org-agenda-list-stuck-projects
   org-diary org-agenda-to-appt)))

;; Autoload org-remember

(eval-and-compile
  (org-autoload "org-remember"
		'(org-remember-insinuate org-remember-annotation
   org-remember-apply-template org-remember org-remember-handler)))

;; Autoload org-clock.el

(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")

(eval-and-compile
  (org-autoload
   "org-clock"
   '(org-clock-in org-clock-out org-clock-cancel
		  org-clock-goto org-clock-sum org-clock-display
		  org-remove-clock-overlays org-clock-report
		  org-clocktable-shift org-dblock-write:clocktable
		  org-get-clocktable)))

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

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

(defun org-clocktable-try-shift (dir n)
  "Check if this line starts a clock table, if yes, shift the time block."
  (when (org-match-line "#\\+BEGIN: clocktable\\>")
    (org-clocktable-shift dir n)))

;; Autoload archiving code
;; The stuff that is needed for cycling and tags has to be defined here.

(defgroup org-archive nil
  "Options concerning archiving in Org-mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-archive-location "%s_archive::"
  "The location where subtrees should be archived.

Otherwise, the value of this variable is a string, consisting of two
parts, separated by a double-colon.

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

#+ARCHIVE: basement::** Finished Tasks

You may also define it locally for a subtree by setting an ARCHIVE property
in the entry.  If such a property is found in an entry, or anywhere up
the hierarchy, it will be used."
  :group 'org-archive
  :type 'string)

(defcustom org-archive-tag "ARCHIVE"
  "The tag that marks a subtree as archived.
An archived subtree does not open during visibility cycling, and does
not contribute to the agenda listings.
After changing this, font-lock must be restarted in the relevant buffers to
get the proper fontification."
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
	    (message "%s" (substitute-command-keys
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

(defalias 'org-advertized-archive-subtree 'org-archive-subtree)

(eval-and-compile
  (org-autoload "org-archive"
   '(org-add-archive-files org-archive-subtree
     org-archive-to-archive-sibling org-toggle-archive-tag)))

;; Autoload Column View Code

(declare-function org-columns-number-to-string "org-colview")
(declare-function org-columns-get-format-and-top-level "org-colview")
(declare-function org-columns-compute "org-colview")

(org-autoload (if (featurep 'xemacs) "org-colview-xemacs" "org-colview")
 '(org-columns-number-to-string org-columns-get-format-and-top-level
   org-columns-compute org-agenda-columns org-columns-remove-overlays
   org-columns org-insert-columns-dblock))

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
(defvar org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:
group 1: the stars
group 2: The todo keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags")
(make-variable-buffer-local 'org-complex-heading-regexp)
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

(defconst org-plain-time-extension-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\+\\([0-9]+\\)\\(:\\([0-5][0-9]\\)\\)?")
  "Regular expression to match a time range like 13:30+2:10 = 13:30-15:40.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
7  hours of duration
9  minutes of duration")

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
    ("logdone" org-log-done time)
    ("lognotedone" org-log-done note)
    ("nologdone" org-log-done nil)
    ("lognoteclock-out" org-log-note-clock-out t)
    ("nolognoteclock-out" org-log-note-clock-out nil)
    ("logrepeat" org-log-repeat state)
    ("lognoterepeat" org-log-repeat note)
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
    (org-set-local 'org-todo-log-states nil)
    (let ((re (org-make-options-regexp
	       '("CATEGORY" "SEQ_TODO" "TYP_TODO" "TODO" "COLUMNS"
		 "STARTUP" "ARCHIVE" "TAGS" "LINK" "PRIORITIES"
		 "CONSTANTS" "PROPERTY" "DRAWERS")))
	  (splitre "[ \t]+")
	  kwds kws0 kwsa key log value cat arch tags const links hw dws
	  tail sep kws1 prio props drawers)
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
	      (setq cat value))
	     ((member key '("SEQ_TODO" "TODO"))
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
	     ((equal key "DRAWERS")
	      (setq drawers (org-split-string value splitre)))
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
      (when cat
	(org-set-local 'org-category (intern cat))
	(push (cons "CATEGORY" cat) props))
      (when prio
	(if (< (length prio) 3) (setq prio '("A" "C" "B")))
	(setq prio (mapcar 'string-to-char prio))
	(org-set-local 'org-highest-priority (nth 0 prio))
	(org-set-local 'org-lowest-priority  (nth 1 prio))
	(org-set-local 'org-default-priority (nth 2 prio)))
      (and props (org-set-local 'org-local-properties (nreverse props)))
      (and drawers (org-set-local 'org-drawers drawers))
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
      (let (inter kws kw)
	(while (setq kws (pop kwds))
	  (setq inter (pop kws) sep (member "|" kws)
		kws0 (delete "|" (copy-sequence kws))
		kwsa nil
		kws1 (mapcar
		      (lambda (x)
			;;                     1              2
			(if (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?.*?)\\)?$" x)
			    (progn
			      (setq kw (match-string 1 x)
				    key (and (match-end 2) (match-string 2 x))
				    log (org-extract-log-state-settings x))
			      (push (cons kw (and key (string-to-char key))) kwsa)
			      (and log (push log org-todo-log-states))
			      kw)
			  (error "Invalid TODO keyword %s" x)))
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
					  (length org-scheduled-string)
					  (length org-clock-string)
					  (length org-closed-string)))
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
	  org-complex-heading-regexp
	  (concat "^\\(\\*+\\)\\(?:[ \t]+\\("
		  (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		  "\\)\\>\\)?\\(?:[ \t]*\\(\\[#.\\]\\)\\)?[ \t]*\\(.*?\\)"
		  "\\(?:[ \t]+\\(:[[:alnum:]_@:]+:\\)\\)?[ \t]*$")
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
    (org-compute-latex-and-specials-regexp)
    (org-set-font-lock-defaults)))

(defun org-extract-log-state-settings (x)
  "Extract the log state setting from a TODO keyword string.
This will extract info from a string like \"WAIT(w@/!)\"."
  (let (kw key log1 log2)
    (when (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?\\([!@]\\)?\\(?:/\\([!@]\\)\\)?)\\)?$" x)
      (setq kw (match-string 1 x)
	    key (and (match-end 2) (match-string 2 x))
	    log1 (and (match-end 3) (match-string 3 x))
	    log2 (and (match-end 4) (match-string 4 x)))
      (and (or log1 log2)
	   (list kw
		 (and log1 (if (equal log1 "!") 'time 'note))
		 (and log2 (if (equal log2 "!") 'time 'note)))))))

(defun org-remove-keyword-keys (list)
  "Remove a pair of parenthesis at the end of each string in LIST."
  (mapcar (lambda (x)
	    (if (string-match "(.*)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))

;; FIXME: this could be done much better, using second characters etc.
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

;;; Some variables used in various places

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")
(defvar org-finish-function nil
  "Function to be called when `C-c C-c' is used.
This is for getting out of special buffers like remember.")


;; FIXME: Occasionally check by commenting these, to make sure
;;        no other functions uses these, forgetting to let-bind them.
(defvar entry)
(defvar state)
(defvar last-state)
(defvar date)
(defvar description)

;; Defined somewhere in this file, but used before definition.
(defvar org-html-entities)
(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)
(defvar org-agenda-keymap)

;;;; Define the Org-mode

(if (and (not (keymapp outline-mode-map)) (featurep 'allout))
    (error "Conflict with outdated version of allout.el.  Load org.el before allout.el, or ugrade to newer allout, for example by switching to Emacs 22."))


;; We use a before-change function to check if a table might need
;; an update.
(defvar org-table-may-need-update t
  "Indicates that a table might need an update.
This variable is set by `org-before-change-function'.
`org-table-align' sets it back to nil.")
(defun org-before-change-function (beg end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))
(defvar org-mode-map)
(defvar org-mode-hook nil)
(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar org-table-buffer-is-an nil)
(defconst org-outline-regexp "\\*+ ")

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

  (org-load-modules-maybe)
  (easy-menu-add org-org-menu)
  (easy-menu-add org-tbl-menu)
  (org-install-agenda-files-menu)
  (if org-descriptive-links (org-add-to-invisibility-spec '(org-link)))
  (org-add-to-invisibility-spec '(org-cwidth))
  (when (featurep 'xemacs)
    (org-set-local 'line-move-ignore-invisible t))
  (org-set-local 'outline-regexp org-outline-regexp)
  (org-set-local 'outline-level 'org-outline-level)
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

  ;; Align options lines
  (org-set-local
   'align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode)))))

  ;; Imenu
  (org-set-local 'imenu-create-index-function
		 'org-imenu-get-tree)

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

(defun org-current-time ()
  "Current time, possibly rounded to `org-time-stamp-rounding-minutes'."
  (if (> (car org-time-stamp-rounding-minutes) 1)
      (let ((r (car org-time-stamp-rounding-minutes))
	    (time (decode-time)))
	(apply 'encode-time
	       (append (list 0 (* r (floor (+ .5 (/ (float (nth 1 time)) r)))))
		       (nthcdr 2 time))))
    (current-time)))

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
(defvar org-link-types '("http" "https" "ftp" "mailto" "file" "news"
			   "shell" "elisp"))
(defvar org-link-types-re nil
   "Matches a link that has a url-like prefix like \"http:\"")
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
  (setq org-link-types-re
	(concat
	 "\\`\\(" (mapconcat 'identity org-link-types "\\|") "\\):")
	org-link-re-with-space
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
	 "\\<\\(" (mapconcat 'identity org-link-types "\\|") "\\):"
	 "\\([^]\t\n\r<>() ]+[^]\t\n\r<>,.;() ]\\)")
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
(defconst org-ts-regexp0 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]-+0-9>\r\n ]*\\)\\( \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be used
on a string that terminates immediately after the date.")
(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) +\\([^]-+0-9>\r\n ]*\\)\\( \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")
(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 "[^>\n]\\{0,16\\}>")
  "Regular expression matching time stamps, with groups.")
(defconst org-ts-regexp3 (concat "[[<]" org-ts-regexp1 "[^]>\n]\\{0,16\\}[]>]")
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
	    (when org-hide-emphasis-markers
	      (add-text-properties (match-end 4) (match-beginning 5)
				   '(invisible org-link))
	      (add-text-properties (match-beginning 3) (match-end 3)
				   '(invisible org-link)))))
      (backward-char 1))
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

(defconst org-nonsticky-props
  '(mouse-face highlight keymap invisible intangible help-echo org-linked-text))


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
				     'rear-nonsticky org-nonsticky-props
				     'keymap org-mouse-map
				     ))
	  (throw 'exit t))))))

(defun org-activate-code (limit)
  (if (re-search-forward "^[ \t]*\\(:.*\\)" limit t)
      (unless (get-text-property (match-beginning 1) 'face)
	(remove-text-properties (match-beginning 0) (match-end 0)
				'(display t invisible t intangible t))
	t)))

(defun org-activate-angle-links (limit)
  "Run through the buffer and add overlays to links."
  (if (re-search-forward org-angle-link-re limit t)
      (progn
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'rear-nonsticky org-nonsticky-props
				   'keymap org-mouse-map
				   ))
	t)))

(defun org-activate-bracket-links (limit)
  "Run through the buffer and add overlays to bracketed links."
  (if (re-search-forward org-bracket-link-regexp limit t)
      (let* ((help (concat "LINK: "
			   (org-match-string-no-properties 1)))
	     ;; FIXME: above we should remove the escapes.
	     ;; but that requires another match, protecting match data,
	     ;; a lot of overhead for font-lock.
	     (ip (org-maybe-intangible
		  (list 'invisible 'org-link 'rear-nonsticky org-nonsticky-props
			'keymap org-mouse-map 'mouse-face 'highlight
			'font-lock-multiline t 'help-echo help)))
	     (vp (list 'rear-nonsticky org-nonsticky-props
		       'keymap org-mouse-map 'mouse-face 'highlight
		       ' font-lock-multiline t 'help-echo help)))
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
				   'rear-nonsticky org-nonsticky-props
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
				       'rear-nonsticky org-nonsticky-props
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

(defvar org-latex-and-specials-regexp nil
  "Regular expression for highlighting export special stuff.")
(defvar org-match-substring-regexp)
(defvar org-match-substring-with-braces-regexp)
(defvar org-export-html-special-string-regexps)

(defun org-compute-latex-and-specials-regexp ()
  "Compute regular expression for stuff treated specially by exporters."
  (if (not org-highlight-latex-fragments-and-specials)
      (org-set-local 'org-latex-and-specials-regexp nil)
    (require 'org-exp)
    (let*
	((matchers (plist-get org-format-latex-options :matchers))
	 (latexs (delq nil (mapcar (lambda (x) (if (member (car x) matchers) x))
				   org-latex-regexps)))
	 (options (org-combine-plists (org-default-export-plist)
				      (org-infile-export-plist)))
	 (org-export-with-sub-superscripts (plist-get options :sub-superscript))
	 (org-export-with-LaTeX-fragments (plist-get options :LaTeX-fragments))
	 (org-export-with-TeX-macros (plist-get options :TeX-macros))
	 (org-export-html-expand (plist-get options :expand-quoted-html))
	 (org-export-with-special-strings (plist-get options :special-strings))
	 (re-sub
	  (cond
	   ((equal org-export-with-sub-superscripts '{})
	    (list org-match-substring-with-braces-regexp))
	   (org-export-with-sub-superscripts
	    (list org-match-substring-regexp))
	   (t nil)))
	 (re-latex
	  (if org-export-with-LaTeX-fragments
	      (mapcar (lambda (x) (nth 1 x)) latexs)))
	 (re-macros
	  (if org-export-with-TeX-macros
	      (list (concat "\\\\"
			    (regexp-opt
			     (append (mapcar 'car org-html-entities)
				     (if (boundp 'org-latex-entities)
					 org-latex-entities nil))
			     'words))) ; FIXME
	    ))
    ;;			(list "\\\\\\(?:[a-zA-Z]+\\)")))
	 (re-special (if org-export-with-special-strings
			 (mapcar (lambda (x) (car x))
				 org-export-html-special-string-regexps)))
	 (re-rest
	  (delq nil
		(list
		 (if org-export-html-expand "@<[^>\n]+>")
		 ))))
      (org-set-local
       'org-latex-and-specials-regexp
       (mapconcat 'identity (append re-latex re-sub re-macros re-special
				    re-rest) "\\|")))))

(defun org-do-latex-and-special-faces (limit)
  "Run through the buffer and add overlays to links."
  (when org-latex-and-specials-regexp
    (let (rtn d)
      (while (and (not rtn) (re-search-forward org-latex-and-specials-regexp
					       limit t))
	(if (not (memq (car-safe (get-text-property (1+ (match-beginning 0))
						    'face))
		       '(org-code org-verbatim underline)))
	    (progn
	      (setq rtn t
		    d (cond ((member (char-after (1+ (match-beginning 0)))
				     '(?_ ?^)) 1)
			    (t 0)))
	      (font-lock-prepend-text-property
	       (+ d (match-beginning 0)) (match-end 0)
	       'face 'org-latex-and-export-specials)
	      (add-text-properties (+ d (match-beginning 0)) (match-end 0)
				   '(font-lock-multiline t)))))
      rtn)))

(defun org-restart-font-lock ()
  "Restart font-lock-mode, to force refontification."
  (when (and (boundp 'font-lock-mode) font-lock-mode)
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
				   'rear-nonsticky org-nonsticky-props
				   'keymap org-mouse-map))
	t)))

(defun org-outline-level ()
  (save-excursion
    (looking-at outline-regexp)
    (if (match-beginning 1)
	(+ (org-get-string-indentation (match-string 1)) 1000)
      (1- (- (match-end 0) (match-beginning 0))))))

(defvar org-font-lock-keywords nil)

(defconst org-property-re (org-re "^[ \t]*\\(:\\([[:alnum:]_]+\\):\\)[ \t]*\\([^ \t\r\n].*\\)")
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
	     (1 'org-table t))
	   ;; Table internals
	   '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
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
	   ;; Checkboxes
	   '("^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)"
	     2 'bold prepend)
	   (if org-provide-checkbox-statistics
	       '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
		 (0 (org-get-checkbox-statistics-face) t)))
	   (list (concat "^\\*+ \\(.*:" org-archive-tag ":.*\\)")
		 '(1 'org-archived prepend))
	   ;; Specials
	   '(org-do-latex-and-special-faces)
	   ;; Code
	   '(org-activate-code (1 'org-code t))
	   ;; COMMENT
	   (list (concat "^\\*+[ \t]+\\<\\(" org-comment-string
			 "\\|" org-quote-string "\\)\\>")
		 '(1 'org-special-keyword t))
	   '("^#.*" (0 'font-lock-comment-face t))
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
  (org-load-modules-maybe)
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

(defun org-compact-display-after-subtree-move ()
  (let (beg end)
    (save-excursion
      (if (org-up-heading-safe)
	  (progn
	    (hide-subtree)
	    (show-entry)
	    (show-children)
	    (org-cycle-show-empty-lines 'children)
	    (org-cycle-hide-drawers 'children))
	(org-overview)))))

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
      (let ((b (match-end 0))
	    (outline-regexp org-outline-regexp))
	(if (re-search-forward
	     "^[ \t]*:END:"
	     (save-excursion (outline-next-heading) (point)) t)
	    (outline-flag-region b (point-at-eol) flag)
	  (error ":END: line missing"))))))



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
    (let ((cmds '(isearch-forward isearch-backward kill-ring-save set-mark-command mouse-drag-region universal-argument org-occur)) cmd)
      (while (setq cmd (pop cmds))
	(substitute-key-definition cmd cmd map global-map)))
    (suppress-keymap map)
    (org-defkey map "\C-m"     'org-goto-ret)
    (org-defkey map [(return)] 'org-goto-ret)
    (org-defkey map [(left)]   'org-goto-left)
    (org-defkey map [(right)]  'org-goto-right)
    (org-defkey map [(control ?g)] 'org-goto-quit)
    (org-defkey map "\C-i" 'org-cycle)
    (org-defkey map [(tab)] 'org-cycle)
    (org-defkey map [(down)] 'outline-next-visible-heading)
    (org-defkey map [(up)] 'outline-previous-visible-heading)
    (if org-goto-auto-isearch
	(if (fboundp 'define-key-after)
	    (define-key-after map [t] 'org-goto-local-auto-isearch)
	  nil)
      (org-defkey map "q" 'org-goto-quit)
      (org-defkey map "n" 'outline-next-visible-heading)
      (org-defkey map "p" 'outline-previous-visible-heading)
      (org-defkey map "f" 'outline-forward-same-level)
      (org-defkey map "b" 'outline-backward-same-level)
      (org-defkey map "u" 'outline-up-heading))
    (org-defkey map "/" 'org-occur)
    (org-defkey map "\C-c\C-n" 'outline-next-visible-heading)
    (org-defkey map "\C-c\C-p" 'outline-previous-visible-heading)
    (org-defkey map "\C-c\C-f" 'outline-forward-same-level)
    (org-defkey map "\C-c\C-b" 'outline-backward-same-level)
    (org-defkey map "\C-c\C-u" 'outline-up-heading)
    map))

(defconst org-goto-help
"Browse buffer copy, to find location or copy text.  Just type for auto-isearch.
RET=jump to location             [Q]uit and return to previous location
\[Up]/[Down]=next/prev headline   TAB=cycle visibility   [/] org-occur")

(defvar org-goto-start-pos) ; dynamically scoped parameter

(defun org-goto (&optional alternative-interface)
  "Look up a different location in the current file, keeping current visibility.

When you want look-up or go to a different location in a document, the
fastest way is often to fold the entire buffer and then dive into the tree.
This method has the disadvantage, that the previous location will be folded,
which may not be what you want.

This command works around this by showing a copy of the current buffer
in an indirect buffer, in overview mode.  You can dive into the tree in
that copy, use org-occur and incremental search to find a location.
When pressing RET or `Q', the command returns to the original buffer in
which the visibility is still unchanged.  After RET is will also jump to
the location selected in the indirect buffer and expose the
the headline hierarchy above."
  (interactive "P")
  (let* ((org-refile-targets '((nil . (:maxlevel . 10))))
	 (org-refile-use-outline-path t)
	 (interface
	  (if (not alternative-interface)
	      org-goto-interface
	    (if (eq org-goto-interface 'outline)
		'outline-path-completion
	      'outline)))
	 (org-goto-start-pos (point))
	 (selected-point
	  (if (eq interface 'outline)
	      (car (org-get-location (current-buffer) org-goto-help))
	    (nth 3 (org-refile-get-location "Goto: ")))))
    (if selected-point
	(progn
	  (org-mark-ring-push org-goto-start-pos)
	  (goto-char selected-point)
	  (if (or (org-invisible-p) (org-invisible-p2))
	      (org-show-context 'org-goto)))
      (message "Quit"))))

(defvar org-goto-selected-point nil) ; dynamically scoped parameter
(defvar org-goto-exit-command nil) ; dynamically scoped parameter
(defvar org-goto-local-auto-isearch-map) ; defined below

(defun org-get-location (buf help)
  "Let the user select a location in the Org-mode buffer BUF.
This function uses a recursive edit.  It returns the selected position
or nil."
  (let ((isearch-mode-map org-goto-local-auto-isearch-map)
	(isearch-hide-immediately nil)
	(isearch-search-fun-function
	 (lambda () 'org-goto-local-search-forward-headings))
	(org-goto-selected-point org-goto-exit-command))
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(and (get-buffer "*org-goto*") (kill-buffer "*org-goto*"))
	(switch-to-buffer
	 (condition-case nil
	     (make-indirect-buffer (current-buffer) "*org-goto*")
	   (error (make-indirect-buffer (current-buffer) "*org-goto*"))))
	(with-output-to-temp-buffer "*Help*"
	  (princ help))
	(shrink-window-if-larger-than-buffer (get-buffer-window "*Help*"))
	(setq buffer-read-only nil)
	(let ((org-startup-truncated t)
	      (org-startup-folded nil)
	      (org-startup-align-all-tables nil))
	  (org-mode)
	  (org-overview))
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
	(use-local-map org-goto-map)
	(recursive-edit)
	))
    (kill-buffer "*org-goto*")
    (cons org-goto-selected-point org-goto-exit-command)))

(defvar org-goto-local-auto-isearch-map (make-sparse-keymap))
(set-keymap-parent org-goto-local-auto-isearch-map isearch-mode-map)
(define-key org-goto-local-auto-isearch-map "\C-i" 'isearch-other-control-char)
(define-key org-goto-local-auto-isearch-map "\C-m" 'isearch-other-control-char)

(defun org-goto-local-search-forward-headings (string bound noerror)
  "Search and make sure that anu matches are in headlines."
  (catch 'return
    (while (search-forward string bound noerror)
      (when (let ((context (mapcar 'car (save-match-data (org-context)))))
	      (and (member :headline context)
		   (not (member :tags context))))
	(throw 'return (point))))))

(defun org-goto-local-auto-isearch ()
  "Start isearch."
 (interactive)
 (goto-char (point-min))
 (let ((keys (this-command-keys)))
   (when (eq (lookup-key isearch-mode-map keys) 'isearch-printing-char)
     (isearch-mode t)
     (isearch-process-search-char (string-to-char keys)))))

(defun org-goto-ret (&optional arg)
  "Finish `org-goto' by going to the new location."
  (interactive "P")
  (setq org-goto-selected-point (point)
	org-goto-exit-command 'return)
  (throw 'exit nil))

(defun org-goto-left ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-on-heading-p)
      (progn
	(beginning-of-line 1)
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'left)
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-right ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-on-heading-p)
      (progn
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'right)
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-quit ()
  "Finish `org-goto' without cursor motion."
  (interactive)
  (setq org-goto-selected-point nil)
  (setq org-goto-exit-command 'quit)
  (throw 'exit nil))

;;; Indirect buffer display of subtrees

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.
With numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.
If `org-indirect-buffer-display' is not `new-frame', the command removes the
indirect buffer previously made with this command, to avoid proliferation of
indirect buffers.  However, when you call the command with a `C-u' prefix, or
when `org-indirect-buffer-display' is `new-frame', the last buffer
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
    (if (and (buffer-live-p org-last-indirect-buffer)
	     (not (eq org-indirect-buffer-display 'new-frame))
	     (not arg))
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
current headline.  If point is not at the beginning, do not split the line,
but create the new hedline after the current line."
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
	  ;; insert before the current line
	  (open-line (if blank 2 1)))
	 ((and (bolp)
	       (or (bobp)
		   (save-excursion
		     (backward-char 1) (not (org-invisible-p)))))
	  ;; insert right here
	  nil)
	 (t
	  ;; in the middle of the line
	  (org-show-entry)
	  (let ((split
		 (org-get-alist-option org-M-RET-may-split-line 'headline))
		tags pos)
	    (if (org-on-heading-p)
		(progn
		  (looking-at ".*?\\([ \t]+\\(:[[:alnum:]_@:]+:\\)\\)?[ \t]*$")
		  (setq tags (and (match-end 2) (match-string 2)))
		  (and (match-end 1)
		       (delete-region (match-beginning 1) (match-end 1)))
		  (setq pos (point-at-bol))
		  (or split (end-of-line 1))
		  (delete-horizontal-space)
		  (newline (if blank 2 1))
		  (when tags
		    (save-excursion
		      (goto-char pos)
		      (end-of-line 1)
		      (insert " " tags)
		      (org-set-tags nil 'align))))
	      (or split (end-of-line 1))
	      (newline (if blank 2 1))))))
	(insert head) (just-one-space)
	(setq pos (point))
	(end-of-line 1)
	(unless (= (point) pos) (just-one-space) (backward-delete-char 1))
	(run-hooks 'org-insert-heading-hook)))))

(defun org-get-heading (&optional no-tags)
  "Return the heading of the current entry, without the stars."
  (save-excursion
    (org-back-to-heading t)
    (if (looking-at
	 (if no-tags
	     (org-re "\\*+[ \t]+\\([^\n\r]*?\\)\\([ \t]+:[[:alnum:]:_@]+:[ \t]*\\)?$")
	   "\\*+[ \t]+\\([^\r\n]*\\)"))
	(match-string 1) "")))

(defun org-insert-heading-after-current ()
  "Insert a new heading with same level as current, after current subtree."
  (interactive)
  (org-back-to-heading)
  (org-insert-heading)
  (org-move-subtree-down)
  (end-of-line 1))

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

(defun org-get-valid-level (level &optional change)
  "Rectify a level change under the influence of `org-odd-levels-only'
LEVEL is a current level, CHANGE is by how much the level should be
modified.  Even if CHANGE is nil, LEVEL may be returned modified because
even level numbers will become the next higher odd number."
  (if org-odd-levels-only
      (cond ((or (not change) (= 0 change)) (1+ (* 2 (/ level 2))))
	    ((> change 0) (1+ (* 2 (/ (+ level (* 2 change)) 2))))
	    ((< change 0) (max 1 (1+ (* 2 (/ (+ level (* 2 change)) 2))))))
    (max 1 (+ level change))))

(if (boundp 'define-obsolete-function-alias)
    (if (or (featurep 'xemacs) (< emacs-major-version 23))
	(define-obsolete-function-alias 'org-get-legal-level
	  'org-get-valid-level)
      (define-obsolete-function-alias 'org-get-legal-level
	'org-get-valid-level "23.1")))

(defun org-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (up-head (concat (make-string (org-get-valid-level level -1) ?*) " "))
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
	 (down-head (concat (make-string (org-get-valid-level level 1) ?*) " "))
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
	(while (and (< (point) end)
		    (re-search-forward "^[ \t]+" end t))
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
	  (setq n (/ (1- (length (match-string 0))) 2))
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
	beg beg0 end txt folded ne-beg ne-end ne-ins ins-end)
    ;; Select the tree
    (org-back-to-heading)
    (setq beg0 (point))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (outline-end-of-subtree))
    (outline-next-heading)
    (setq ne-end (org-back-over-empty-lines))
    (setq end (point))
    (goto-char beg0)
    (when (and (> arg 0) (org-first-sibling-p) (< ne-end ne-beg))
      ;; include less whitespace
      (save-excursion
	(goto-char beg)
	(forward-line (- ne-beg ne-end))
	(setq beg (point))))
    ;; Find insertion point, with error handling
    (while (> cnt 0)
      (or (and (funcall movfunc) (looking-at outline-regexp))
	  (progn (goto-char beg0)
		 (error "Cannot move past superior level or buffer limit")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (org-end-of-subtree t t)
	       (save-excursion
		 (org-back-over-empty-lines)
		 (or (bolp) (newline)))))
    (setq ne-ins (org-back-over-empty-lines))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (outline-flag-region (1- beg) beg nil)
    (outline-flag-region (1- (point)) (point) nil)
    (insert txt)
    (or (bolp) (insert "\n"))
    (setq ins-end (point))
    (goto-char ins-point)
    (org-skip-whitespace)
    (when (and (< arg 0)
	       (org-first-sibling-p)
	       (> ne-ins ne-beg))
      ;; Move whitespace back to beginning
      (save-excursion
	(goto-char ins-end)
	(let ((kill-whole-line t))
	  (kill-line (- ne-ins ne-beg)) (point)))
      (insert (make-string (- ne-ins ne-beg) ?\n)))
    (move-marker ins-point nil)
    (org-compact-display-after-subtree-move)
    (unless folded
      (org-show-entry)
      (show-children)
      (org-cycle-hide-drawers 'children))))

(defvar org-subtree-clip ""
  "Clipboard for cut and paste of subtrees.
This is actually only a copy of the kill, because we use the normal kill
ring.  We need it to check if the kill was created by `org-copy-subtree'.")

(defvar org-subtree-clip-folded nil
  "Was the last copied subtree folded?
This is used to fold the tree back after pasting.")

(defun org-cut-subtree (&optional n)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then cutting it."
  (interactive "p")
  (org-copy-subtree n 'cut))

(defun org-copy-subtree (&optional n cut)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then copying it.
If CUT is non-nil, actually cut the subtree."
  (interactive "p")
  (let (beg end folded (beg0 (point)))
    (if (interactive-p)
	(org-back-to-heading nil) ; take what looks like a subtree
      (org-back-to-heading t)) ; take what is really there
    (org-back-over-empty-lines)
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (condition-case nil
	  (outline-forward-same-level (1- n))
	(error nil))
      (org-end-of-subtree t t))
    (org-back-over-empty-lines)
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (setq org-subtree-clip-folded folded)
      (if cut (kill-region beg end) (copy-region-as-kill beg end))
      (setq org-subtree-clip (current-kill 0))
      (message "%s: Subtree(s) with %d characters"
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
    (error "%s"
     (substitute-command-keys
      "The kill is not a (set of) tree(s) - please use \\[yank] to yank anyway")))
  (let* ((txt (or tree (and kill-ring (current-kill 0))))
	 (^re (concat "^\\(" outline-regexp "\\)"))
	 (re  (concat "\\(" outline-regexp "\\)"))
	 (^re_ (concat "\\(\\*+\\)[  \t]*"))

	 (old-level (if (string-match ^re txt)
			(- (match-end 0) (match-beginning 0) 1)
		      -1))
	 (force-level (cond (level (prefix-numeric-value level))
			    ((string-match
			      ^re_ (buffer-substring (point-at-bol) (point)))
			     (- (match-end 1) (match-beginning 1)))
			    (t nil)))
	 (previous-level (save-excursion
			   (condition-case nil
			       (progn
				 (outline-previous-visible-heading 1)
				 (if (looking-at re)
				     (- (match-end 0) (match-beginning 0) 1)
				   1))
			     (error 1))))
	 (next-level (save-excursion
		       (condition-case nil
			   (progn
			     (or (looking-at outline-regexp)
				 (outline-next-visible-heading 1))
			     (if (looking-at re)
				 (- (match-end 0) (match-beginning 0) 1)
			       1))
			 (error 1))))
	 (new-level (or force-level (max previous-level next-level)))
	 (shift (if (or (= old-level -1)
			(= new-level -1)
			(= old-level new-level))
		    0
		  (- new-level old-level)))
	 (delta (if (> shift 0) -1 1))
	 (func (if (> shift 0) 'org-demote 'org-promote))
	 (org-odd-levels-only nil)
	 beg end)
    ;; Remove the forced level indicator
    (if force-level
	(delete-region (point-at-bol) (point)))
    ;; Paste
    (beginning-of-line 1)
    (org-back-over-empty-lines)
    (setq beg (point))
    (insert-before-markers txt)
    (unless (string-match "\n\\'" txt) (insert "\n"))
    (setq end (point))
    (goto-char beg)
    (skip-chars-forward " \t\n\r")
    (setq beg (point))
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
			   (string-match (concat "\\`\\([ \t\n\r]*?\n\\)?\\("
						 org-outline-regexp "\\)")
					 kill)
			   (- (match-end 2) (match-beginning 2) 1)))
	 (re (concat "^" org-outline-regexp))
	 (start (1+ (match-beginning 2))))
    (if (not start-level)
	(progn
	  nil)  ;; does not even start with a heading
      (catch 'exit
	(while (setq start (string-match re kill (1+ start)))
	  (when (< (- (match-end 0) (match-beginning 0) 1) start-level)
	    (throw 'exit nil)))
	t))))

(defun org-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (narrow-to-region
       (progn (org-back-to-heading) (point))
       (progn (org-end-of-subtree t t) (point))))))


;;; Outline Sorting

(defun org-sort (with-case)
  "Call `org-sort-entries-or-items' or `org-table-sort-lines'.
Optional argument WITH-CASE means sort case-sensitively."
  (interactive "P")
  (if (org-at-table-p)
      (org-call-with-arg 'org-table-sort-lines with-case)
    (org-call-with-arg 'org-sort-entries-or-items with-case)))

(defun org-sort-remove-invisible (s)
  (remove-text-properties 0 (length s) org-rm-props s)
  (while (string-match org-bracket-link-regexp s)
    (setq s (replace-match (if (match-end 2)
			       (match-string 3 s)
			     (match-string 1 s)) t t s)))
  s)

(defvar org-priority-regexp) ; defined later in the file

(defun org-sort-entries-or-items (&optional with-case sorting-type getkey-func property)
  "Sort entries on a certain level of an outline tree.
If there is an active region, the entries in the region are sorted.
Else, if the cursor is before the first entry, sort the top-level items.
Else, the children of the entry at point are sorted.

Sorting can be alphabetically, numerically, and by date/time as given by
the first time stamp in the entry.  The command prompts for the sorting
type unless it has been given to the function through the SORTING-TYPE
argument, which needs to a character, any of (?n ?N ?a ?A ?t ?T ?p ?P ?f ?F).
If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies a function to be
called with point at the beginning of the record.  It must return either
a string or a number that should serve as the sorting key for that record.

Comparing entries ignores case by default.  However, with an optional argument
WITH-CASE, the sorting considers case as well."
  (interactive "P")
  (let ((case-func (if with-case 'identity 'downcase))
        start beg end stars re re2
        txt what tmp plain-list-p)
    ;; Find beginning and end of region to sort
    (cond
     ((org-region-active-p)
      ;; we will sort the region
      (setq end (region-end)
            what "region")
      (goto-char (region-beginning))
      (if (not (org-on-heading-p)) (outline-next-heading))
      (setq start (point)))
     ((org-at-item-p)
      ;; we will sort this plain list
      (org-beginning-of-item-list) (setq start (point))
      (org-end-of-item-list) (setq end (point))
      (goto-char start)
      (setq plain-list-p t
	    what "plain list"))
     ((or (org-on-heading-p)
          (condition-case nil (progn (org-back-to-heading) t) (error nil)))
      ;; we will sort the children of the current headline
      (org-back-to-heading)
      (setq start (point)
	    end (progn (org-end-of-subtree t t)
		       (org-back-over-empty-lines)
		       (point))
	    what "children")
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
    (if (>= beg end) (error "Nothing to sort"))

    (unless plain-list-p
      (looking-at "\\(\\*+\\)")
      (setq stars (match-string 1)
	    re (concat "^" (regexp-quote stars) " +")
	    re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[^*]")
	    txt (buffer-substring beg end))
      (if (not (equal (substring txt -1) "\n")) (setq txt (concat txt "\n")))
      (if (and (not (equal stars "*")) (string-match re2 txt))
	  (error "Region to sort contains a level above the first entry")))

    (unless sorting-type
      (message
       (if plain-list-p
	   "Sort %s: [a]lpha [n]umeric [t]ime [f]unc  A/N/T/F means reversed:"
	 "Sort %s: [a]lpha [n]umeric [t]ime [p]riority p[r]operty todo[o]rder [f]unc  A/N/T/P/O/F means reversed:")
       what)
      (setq sorting-type (read-char-exclusive))

      (and (= (downcase sorting-type) ?f)
           (setq getkey-func
                 (completing-read "Sort using function: "
                                  obarray 'fboundp t nil nil))
           (setq getkey-func (intern getkey-func)))

      (and (= (downcase sorting-type) ?r)
           (setq property
                 (completing-read "Property: "
				  (mapcar 'list (org-buffer-property-keys t))
                                  nil t))))

    (message "Sorting entries...")

    (save-restriction
      (narrow-to-region start end)

      (let ((dcst (downcase sorting-type))
            (now (current-time)))
        (sort-subr
         (/= dcst sorting-type)
         ;; This function moves to the beginning character of the "record" to
         ;; be sorted.
	 (if plain-list-p
	     (lambda nil
	       (if (org-at-item-p) t (goto-char (point-max))))
	   (lambda nil
	     (if (re-search-forward re nil t)
		 (goto-char (match-beginning 0))
	       (goto-char (point-max)))))
         ;; This function moves to the last character of the "record" being
         ;; sorted.
	 (if plain-list-p
	     'org-end-of-item
	   (lambda nil
	     (save-match-data
	       (condition-case nil
		   (outline-forward-same-level 1)
		 (error
		  (goto-char (point-max)))))))

         ;; This function returns the value that gets sorted against.
	 (if plain-list-p
	     (lambda nil
	       (when (looking-at "[ \t]*[-+*0-9.)]+[ \t]+")
		 (cond
		  ((= dcst ?n)
		   (string-to-number (buffer-substring (match-end 0)
						       (point-at-eol))))
		  ((= dcst ?a)
		   (buffer-substring (match-end 0) (point-at-eol)))
		  ((= dcst ?t)
		   (if (re-search-forward org-ts-regexp
					  (point-at-eol) t)
		       (org-time-string-to-time (match-string 0))
		     now))
		  ((= dcst ?f)
		   (if getkey-func
		       (progn
			 (setq tmp (funcall getkey-func))
			 (if (stringp tmp) (setq tmp (funcall case-func tmp)))
			 tmp)
		     (error "Invalid key function `%s'" getkey-func)))
		  (t (error "Invalid sorting type `%c'" sorting-type)))))
	   (lambda nil
	     (cond
	      ((= dcst ?n)
	       (if (looking-at outline-regexp)
		   (string-to-number (buffer-substring (match-end 0)
						       (point-at-eol)))
		 nil))
	      ((= dcst ?a)
	       (funcall case-func (buffer-substring (point-at-bol)
						    (point-at-eol))))
	      ((= dcst ?t)
	       (if (re-search-forward org-ts-regexp
				      (save-excursion
					(forward-line 2)
					(point)) t)
		   (org-time-string-to-time (match-string 0))
		 now))
	      ((= dcst ?p)
	       (if (re-search-forward org-priority-regexp (point-at-eol) t)
		   (string-to-char (match-string 2))
		 org-default-priority))
	      ((= dcst ?r)
	       (or (org-entry-get nil property) ""))
	      ((= dcst ?o)
	       (if (looking-at org-complex-heading-regexp)
		   (- 9999 (length (member (match-string 2)
					   org-todo-keywords-1)))))
	      ((= dcst ?f)
	       (if getkey-func
		   (progn
		     (setq tmp (funcall getkey-func))
		     (if (stringp tmp) (setq tmp (funcall case-func tmp)))
		     tmp)
		 (error "Invalid key function `%s'" getkey-func)))
	      (t (error "Invalid sorting type `%c'" sorting-type)))))
         nil
         (cond
          ((= dcst ?a) 'string<)
          ((= dcst ?t) 'time-less-p)
          (t nil)))))
    (message "Sorting entries...done")))

(defun org-do-sort (table what &optional with-case sorting-type)
  "Sort TABLE of WHAT according to SORTING-TYPE.
The user will be prompted for the SORTING-TYPE if the call to this
function does not specify it.  WHAT is only for the prompt, to indicate
what is being sorted.  The sorting key will be extracted from
the car of the elements of the table.
If WITH-CASE is non-nil, the sorting will be case-sensitive."
  (unless sorting-type
    (message
     "Sort %s: [a]lphabetic. [n]umeric. [t]ime.  A/N/T means reversed:"
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
      (setq extractfun (if with-case (lambda(x) (org-sort-remove-invisible x))
			 (lambda(x) (downcase (org-sort-remove-invisible x))))
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
	((= llt ?\)) "\\([ \t]*\\([-+]\\|\\([0-9]+))\\)\\|[ \t]+\\*\\)\\( \\|$\\)")
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
       (t
	(unless (org-get-alist-option org-M-RET-may-split-line 'item)
	  (end-of-line 1)
	  (delete-horizontal-space))
	(newline (if blank 2 1))))
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
	(let ((pos (point)))
	  (replace-match
	   (cond (arg "[-]")
		 ((member (match-string 0) '("[ ]" "[-]")) "[X]")
		 (t "[ ]"))
	   t t)
	  (goto-char pos))
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
	  (re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	  (re-box "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	  (re-find (concat re "\\|" re-box))
	  beg-cookie end-cookie is-percent c-on c-off lim
          eline curr-ind next-ind continue-from startsearch
          (cstat 0)
          )
     (when all
       (goto-char (point-min))
       (outline-next-heading)
       (setq beg (point) end (point-max)))
     (goto-char end)
     ;; find each statistic cookie
     (while (re-search-backward re-find beg t)
       (setq beg-cookie (match-beginning 1)
             end-cookie (match-end 1)
	     cstat (+ cstat (if end-cookie 1 0))
	     startsearch (point-at-eol)
	     continue-from (point-at-bol)
             is-percent (match-beginning 2)
	     lim (cond
		  ((org-on-heading-p) (outline-next-heading) (point))
		  ((org-at-item-p) (org-end-of-item) (point))
		  (t nil))
             c-on 0
             c-off 0)
       (when lim
         ;; find first checkbox for this cookie and gather
         ;; statistics from all that are at this indentation level
         (goto-char startsearch)
         (if (re-search-forward re-box lim t)
             (progn
               (org-beginning-of-item)
               (setq curr-ind (org-get-indentation))
               (setq next-ind curr-ind)
               (while (and (bolp) (org-at-item-p) (= curr-ind next-ind))
                 (save-excursion (end-of-line) (setq eline (point)))
                 (if (re-search-forward re-box eline t)
		     (if (member (match-string 2) '("[ ]" "[-]"))
			 (setq c-off (1+ c-off))
                       (setq c-on (1+ c-on))
                       )
                   )
                 (org-end-of-item)
                 (setq next-ind (org-get-indentation))
                 )))
	 (goto-char continue-from)
         ;; update cookie
	 (when end-cookie
	   (delete-region beg-cookie end-cookie)
	   (goto-char beg-cookie)
	   (insert
	    (if is-percent
		(format "[%d%%]" (/ (* 100 c-on) (max 1 (+ c-on c-off))))
	      (format "[%d/%d]" c-on (+ c-on c-off)))))
         ;; update items checkbox if it has one
         (when (org-at-item-p)
           (org-beginning-of-item)
           (when (and (> (+ c-on c-off) 0)
		      (re-search-forward re-box (point-at-eol) t))
             (setq beg-cookie (match-beginning 2)
                   end-cookie (match-end       2))
             (delete-region beg-cookie end-cookie)
             (goto-char beg-cookie)
             (cond ((= c-off 0) (insert "[X]"))
                   ((= c-on  0) (insert "[ ]"))
                   (t           (insert "[-]")))
             )))
       (goto-char continue-from))
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

(defun org-first-list-item-p ()
  "Is this heading the item in a plain list?"
  (unless (org-at-item-p)
    (error "Not at a plain list item"))
  (org-beginning-of-item)
  (= (point) (save-excursion (org-beginning-of-item-list))))

(defun org-move-item-down ()
  "Move the plain list item at point down, i.e. swap with following item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive)
  (let (beg beg0 end end0 ind ind1 (pos (point)) txt ne-end ne-beg)
    (org-beginning-of-item)
    (setq beg0 (point))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (goto-char beg0)
    (setq ind (org-get-indentation))
    (org-end-of-item)
    (setq end0 (point))
    (setq ind1 (org-get-indentation))
    (setq ne-end (org-back-over-empty-lines))
    (setq end (point))
    (goto-char beg0)
    (when (and (org-first-list-item-p) (< ne-end ne-beg))
      ;; include less whitespace
      (save-excursion
	(goto-char beg)
	(forward-line (- ne-beg ne-end))
	(setq beg (point))))
    (goto-char end0)
    (if (and (org-at-item-p) (= ind ind1))
	(progn
	  (org-end-of-item)
	  (org-back-over-empty-lines)
	  (setq txt (buffer-substring beg end))
	  (save-excursion
	    (delete-region beg end))
	  (setq pos (point))
	  (insert txt)
	  (goto-char pos) (org-skip-whitespace)
	  (org-maybe-renumber-ordered-list))
      (goto-char pos)
      (error "Cannot move this item further down"))))

(defun org-move-item-up (arg)
  "Move the plain list item at point up, i.e. swap with previous item.
Subitems (items with larger indentation) are considered part of the item,
so this really moves item trees."
  (interactive "p")
  (let (beg beg0 end ind ind1 (pos (point)) txt
	    ne-beg ne-ins ins-end)
    (org-beginning-of-item)
    (setq beg0 (point))
    (setq ind (org-get-indentation))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (goto-char beg0)
    (org-end-of-item)
    (setq end (point))
    (goto-char beg0)
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
	  (setq ne-ins (org-back-over-empty-lines))
	  (setq txt (buffer-substring beg end))
	  (save-excursion
	    (delete-region beg end))
	  (setq pos (point))
	  (insert txt)
	  (setq ins-end (point))
	  (goto-char pos) (org-skip-whitespace)

	  (when (and (org-first-list-item-p) (> ne-ins ne-beg))
	    ;; Move whitespace back to beginning
	    (save-excursion
	      (goto-char ins-end)
	      (let ((kill-whole-line t))
		(kill-line (- ne-ins ne-beg)) (point)))
	    (insert (make-string (- ne-ins ne-beg) ?\n)))

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
      (org-fix-bullet-type))))

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
   (let ((current (match-string 0))
	 (prevp (eq which 'previous))
	 new)
     (setq new (cond
		((and (numberp which)
		      (nth (1- which) '("-" "+" "*" "1." "1)"))))
		((string-match "-" current) (if prevp "1)" "+"))
		((string-match "\\+" current)
		 (if prevp "-" (if (looking-at "\\S-") "1." "*")))
		((string-match "\\*" current) (if prevp "+" "1."))
		((string-match "\\." current) (if prevp "*" "1)"))
		((string-match ")" current) (if prevp "1." "-"))
		(t (error "This should not happen"))))
     (and (looking-at "\\([ \t]*\\)\\S-+") (replace-match (concat "\\1" new)))
     (org-fix-bullet-type)
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
    (org-move-to-column col)))

(defun org-fix-bullet-type ()
  "Make sure all items in this list have the same bullet as the firsst item."
  (interactive)
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
    (org-move-to-column col)
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


(defun org-end-of-item-list ()
  "Go to the end of the current item list.
I.e. to the text after the last item."
  (interactive)
  (org-beginning-of-item)
  (let ((pos (point-at-bol))
        (ind (org-get-indentation))
	ind1)
    ;; find where this list begins
    (catch 'exit
      (while t
	(catch 'next
	  (beginning-of-line 2)
	  (if (looking-at "[ \t]*$")
	      (throw (if (eobp) 'exit 'next) t))
	  (skip-chars-forward " \t") (setq ind1 (current-column))
	  (if (or (< ind1 ind)
		  (and (= ind1 ind)
		       (not (org-at-item-p)))
		  (eobp))
	      (progn
		(setq pos (point-at-bol))
		(throw 'exit t))))))
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
	(or (eolp) (org-indent-to-column (+ ind1 delta)))
	(beginning-of-line 2))))
  (org-fix-bullet-type)
  (org-maybe-renumber-ordered-list-safe)
  (save-excursion
    (beginning-of-line 0)
    (condition-case nil (org-beginning-of-item) (error nil))
    (org-maybe-renumber-ordered-list-safe)))

(defun org-item-indent-positions ()
  "Return indentation for plain list items.
This returns a list with three values:  The current indentation, the
parent indentation and the indentation a child should habe.
Assumes cursor in item line."
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

(defvar org-local-vars nil
  "List of local variables, for use by `orgstruct-mode'")

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
  (org-load-modules-maybe)
  (and (orgstruct-setup) (defun orgstruct-setup () nil)))

;;;###autoload
(defun turn-on-orgstruct ()
  "Unconditionally turn on `orgstruct-mode'."
  (orgstruct-mode 1))

;;;###autoload
(defun turn-on-orgstruct++ ()
  "Unconditionally turn on `orgstruct-mode', and force org-mode indentations.
In addition to setting orgstruct-mode, this also exports all indentation and
autofilling variables from org-mode into the buffer.  Note that turning
off orgstruct-mode will *not* remove these additional settings."
  (orgstruct-mode 1)
  (let (var val)
    (mapc
     (lambda (x)
       (when (string-match
	      "^\\(paragraph-\\|auto-fill\\|fill-paragraph\\|adaptive-fill\\|indent-\\)"
	      (symbol-name (car x)))
	 (setq var (car x) val (nth 1 x))
	 (org-set-local var (if (eq (car-safe val) 'quote) (nth 1 val) val))))
     org-local-vars)))

(defun orgstruct-error ()
  "Error when there is no default binding for a structure key."
  (interactive)
  (error "This key has no function outside structure elements"))

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
  "Check if local context is and of CONTEXTS.
Possible values in the list of contexts are `table', `headline', and `item'."
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
  (org-load-modules-maybe)
  (unless org-local-vars
    (setq org-local-vars (org-get-local-variables)))
  (eval (list 'let org-local-vars
	      (list 'call-interactively (list 'quote cmd)))))

;;;; Archiving

(defun org-get-category (&optional pos)
  "Get the category applying to position POS."
  (get-text-property (or pos (point)) 'org-category))

(defun org-refresh-category-properties ()
  "Refresh category text properties in the buffer."
  (let ((def-cat (cond
		  ((null org-category)
		   (if buffer-file-name
		       (file-name-sans-extension
			(file-name-nondirectory buffer-file-name))
		     "???"))
		  ((symbolp org-category) (symbol-name org-category))
		  (t org-category)))
	beg end cat pos optionp)
    (org-unmodified
     (save-excursion
       (save-restriction
	 (widen)
	 (goto-char (point-min))
	 (put-text-property (point) (point-max) 'org-category def-cat)
	 (while (re-search-forward
		 "^\\(#\\+CATEGORY:\\|[ \t]*:CATEGORY:\\)\\(.*\\)" nil t)
	   (setq pos (match-end 0)
		 optionp (equal (char-after (match-beginning 0)) ?#)
		 cat (org-trim (match-string 2)))
	   (if optionp
	       (setq beg (point-at-bol) end (point-max))
	     (org-back-to-heading t)
	     (setq beg (point) end (org-end-of-subtree t t)))
	   (put-text-property beg end 'org-category cat)
	   (goto-char pos)))))))


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
`org-store-link-props' with a list of properties and values.
Special properties are:

:type         The link prefix. like \"http\".  This must be given.
:link         The link, like \"http://www.astro.uva.nl/~dominik\".
              This is obligatory as well.
:description  Optional default description for the second pair
              of brackets in an Org-mode link.  The user can still change
              this when inserting this link into an Org-mode buffer.

In addition to these, any additional properties can be specified
and then used in remember templates.")

(defun org-add-link-type (type &optional follow export)
  "Add TYPE to the list of `org-link-types'.
Re-compute all regular expressions depending on `org-link-types'

FOLLOW and EXPORT are two functions.

FOLLOW should take the link path as the single argument and do whatever
is necessary to follow the link, for example find a file or display
a mail message.

EXPORT should format the link path for export to one of the export formats.
It should be a function accepting three arguments:

  path    the path of the link, the text after the prefix (like \"http:\")
  desc    the description of the link, if any, nil if there was no descripton
  format  the export format, a symbol like `html' or `latex'.

The function may use the FORMAT information to return different values
depending on the format.  The return value will be put literally into
the exported file.
Org-mode has a built-in default for exporting links.  If you are happy with
this default, there is no need to define an export function for the link
type.  For a simple example of an export function, see `org-bbdb.el'."
  (add-to-list 'org-link-types type t)
  (org-make-link-regexps)
  (if (assoc type org-link-protocols)
      (setcdr (assoc type org-link-protocols) (list follow export))
    (push (list type follow export) org-link-protocols)))


;;;###autoload
(defun org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted:
For links to usenet articles, arg negates `org-usenet-links-prefer-google'.
For file links, arg negates `org-context-in-file-links'."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-store-link-plist nil)  ; reset
  (let (link cpltxt desc description search txt)
    (cond

     ((run-hook-with-args-until-success 'org-store-link-functions)
      (setq link (plist-get org-store-link-plist :link)
	    desc (or (plist-get org-store-link-plist :description) link)))

     ((eq major-mode 'calendar-mode)
      (let ((cd (calendar-cursor-to-date)))
	(setq link
	      (format-time-string
	       (car org-time-stamp-formats)
	       (apply 'encode-time
		      (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			    nil nil nil))))
	(org-store-link-props :type "calendar" :date cd)))

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
		     (t nil)))
	  (when (or (null txt) (string-match "\\S-" txt))
	    (setq cpltxt
		  (concat cpltxt "::" (org-make-org-heading-search-string txt))
		  desc "NONE"))))
      (if (string-match "::\\'" cpltxt)
	  (setq cpltxt (substring cpltxt 0 -2)))
      (setq link (org-make-link cpltxt)))

     ((buffer-file-name (buffer-base-buffer))
      ;; Just link to this file here.
      (setq cpltxt (concat "file:"
			   (abbreviate-file-name
			    (buffer-file-name (buffer-base-buffer)))))
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

(defun org-add-link-props (&rest plist)
  "Add these properties to the link property list."
  (let (key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (setq org-store-link-plist
	    (plist-put org-store-link-plist key value)))))

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
    (while (string-match "\\[" description)
      (setq description (replace-match "{" t t description)))
    (while (string-match "\\]" description)
      (setq description (replace-match "}" t t description))))
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
  '((?\    . "%20")
    (?\[   . "%5B")
    (?\]   . "%5D")
    (?\340 . "%E0")  ; `a
    (?\342 . "%E2")  ; ^a
    (?\347 . "%E7")  ; ,c
    (?\350 . "%E8")  ; `e
    (?\351 . "%E9")  ; 'e
    (?\352 . "%EA")  ; ^e
    (?\356 . "%EE")  ; ^i
    (?\364 . "%F4")  ; ^o
    (?\371 . "%F9")  ; `u
    (?\373 . "%FB")  ; ^u
    (?\;   . "%3B")
    (??    . "%3F")
    (?=    . "%3D")
    (?+    . "%2B")
    )
  "Association list of escapes for some characters problematic in links.
This is the list that is used for internal purposes.")

(defconst org-link-escape-chars-browser
  '((?\  . "%20")) ; 32 for the SPC char
  "Association list of escapes for some characters problematic in links.
This is the list that is used before handing over to the browser.")

(defun org-link-escape (text &optional table)
  "Escape charaters in TEXT that are problematic for links."
  (setq table (or table org-link-escape-chars))
  (when text
    (let ((re (mapconcat (lambda (x) (regexp-quote
				      (char-to-string (car x))))
			 table "\\|")))
      (while (string-match re text)
	(setq text
	      (replace-match
	       (cdr (assoc (string-to-char (match-string 0 text))
			   table))
	       t t text)))
      text)))

(defun org-link-unescape (text &optional table)
  "Reverse the action of `org-link-escape'."
  (setq table (or table org-link-escape-chars))
  (when text
    (let ((re (mapconcat (lambda (x) (regexp-quote (cdr x)))
			 table "\\|")))
      (while (string-match re text)
	(setq text
	      (replace-match
	       (char-to-string (car (rassoc (match-string 0 text) table)))
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
  (org-load-modules-maybe)
  (org-run-like-in-org-mode 'org-insert-link))

(defun org-insert-link (&optional complete-file link-location)
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

With a \\[universal-argument] prefix, prompts for a file to link to. The file name can
be selected using completion. The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).

With two \\[universal-argument] prefixes, enforce an absolute path even if the file is in
the current directory or below. With three \\[universal-argument] prefixes, negate the meaning
of `org-keep-stored-link-after-insertion'.

If `org-make-link-description-function' is non-nil, this function will be
called with the link target, and the result will be the default
link description.

If the LINK-LOCATION parameter is non-nil, this value will be
used as the link location instead of reading one interactively."
  (interactive "P")
  (let* ((wcf (current-window-configuration))
	 (region (if (org-region-active-p)
		     (buffer-substring (region-beginning) (region-end))))
	 (remove (and region (list (region-beginning) (region-end))))
	 (desc region)
	 tmphist ; byte-compile incorrectly complains about this
	 (link link-location)
	 entry file)
    (cond
     (link-location) ; specified by arg, just use it.
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
	  (princ "\nStored links are available with <up>/<down> or M-p/n (most recent with RET):\n\n")
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
      (setq desc (or desc (nth 1 entry)))))

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
	     (origpath path)
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
	(if (equal desc origpath)
	    (setq desc path))))

    (if org-make-link-description-function
	(setq desc (funcall org-make-link-description-function link desc)))

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

;;;###autoload
(defun org-open-link-from-string (s &optional arg)
  "Open a link in the string S, as if it was in Org-mode."
  (interactive "sLink: \nP")
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (org-mode)
      (insert s)
      (goto-char (point-min))
      (org-open-at-point arg))))

(defun org-open-at-point (&optional in-emacs)
  "Open link at or after point.
If there is no link at point, this function will search forward up to
the end of the current subtree.
Normally, files will be opened by an appropriate application.  If the
optional argument IN-EMACS is non-nil, Emacs will visit the file."
  (interactive "P")
  (org-load-modules-maybe)
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
	(browse-url (concat type ":" (org-link-escape
				      path org-link-escape-chars-browser))))

       ((member type '("message"))
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
	(if (string-match "[*?{]" (file-name-nondirectory path))
	    (dired path)
	  (org-open-file path in-emacs line search)))

       ((string= type "news")
	(require 'org-gnus)
	(org-gnus-follow-link path))

       ((string= type "shell")
	(let ((cmd path))
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
  (move-marker org-open-link-marker nil)
  (run-hook-with-args 'org-follow-link-hook))

;;;; Time estimates

(defun org-get-effort (&optional pom)
  "Get the effort estimate for the current entry."
  (org-entry-get pom org-effort-property))

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
	(pre nil) (post nil)
	words re0 re1 re2 re3 re4_ re4 re5 re2a re2a_ reall)
    (cond
     ;; First check if there are any special
     ((run-hook-with-args-until-success 'org-execute-file-search-functions s))
     ;; Now try the builtin stuff
     ((save-excursion
	(goto-char (point-min))
	(and
	 (re-search-forward
	  (concat "<<" (regexp-quote s0) ">>") nil t)
	 (setq type 'dedicated
	       pos (match-beginning 0))))
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
	    re2a_ (concat "\\(" (mapconcat 'downcase words "[ \t\r\n]+") "\\)[ \t\r\n]")
	    re2a (concat "[ \t\r\n]" re2a_)
	    re4_ (concat "\\(" (mapconcat 'downcase words "[^a-zA-Z_\r\n]+") "\\)[^a-zA-Z_]")
	    re4 (concat "[^a-zA-Z_]" re4_)

	    re1 (concat pre re2 post)
	    re3 (concat pre (if pre re4_ re4) post)
	    re5 (concat pre ".*" re4)
	    re2 (concat pre re2)
	    re2a (concat pre (if pre re2a_ re2a))
	    re4 (concat pre (if pre re4_ re4))
	    reall (concat "\\(" re0 "\\)\\|\\(" re1 "\\)\\|\\(" re2
			  "\\)\\|\\(" re3 "\\)\\|\\(" re4 "\\)\\|\\("
			  re5 "\\)"
			  ))
      (cond
       ((eq type 'org-occur) (org-occur reall))
       ((eq type 'occur) (org-do-occur (downcase reall) 'cleanup))
       (t (goto-char (point-min))
	  (setq type 'fuzzy)
	  (if (or (and (org-search-not-self 1 re0 nil t) (setq type 'dedicated))
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
    (and (org-mode-p) (org-show-context 'link-search))
    type))

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
  (message "%s"
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


;;; Following file links
(defvar org-wait nil)
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
      (while (string-match "['\"]%s['\"]" cmd)
	(setq cmd (replace-match "%s" t t cmd)))
      (while (string-match "%s" cmd)
	(setq cmd (replace-match
		   (save-match-data (shell-quote-argument file))
		   t t cmd)))
      (save-window-excursion
	(start-process-shell-command cmd nil cmd)
	(and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))
	))
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


;;;; Refiling

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

(defvar org-refile-target-table nil
  "The list of refile targets, created by `org-refile'.")

(defvar org-agenda-new-buffers nil
  "Buffers created to visit agenda files.")

(defun org-get-refile-targets (&optional default-buffer)
  "Produce a table with refile targets."
  (let ((entries (or org-refile-targets '((nil . (:level . 1)))))
	targets txt re files f desc descre)
    (with-current-buffer (or default-buffer (current-buffer))
      (while (setq entry (pop entries))
	(setq files (car entry) desc (cdr entry))
	(cond
	 ((null files) (setq files (list (current-buffer))))
	 ((eq files 'org-agenda-files)
	  (setq files (org-agenda-files 'unrestricted)))
	 ((and (symbolp files) (fboundp files))
	  (setq files (funcall files)))
	 ((and (symbolp files) (boundp files))
	  (setq files (symbol-value files))))
	(if (stringp files) (setq files (list files)))
	(cond
	 ((eq (car desc) :tag)
	  (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
	 ((eq (car desc) :todo)
	  (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
	 ((eq (car desc) :regexp)
	  (setq descre (cdr desc)))
	 ((eq (car desc) :level)
	  (setq descre (concat "^\\*\\{" (number-to-string
					  (if org-odd-levels-only
					      (1- (* 2 (cdr desc)))
					    (cdr desc)))
			       "\\}[ \t]")))
	 ((eq (car desc) :maxlevel)
	  (setq descre (concat "^\\*\\{1," (number-to-string
					    (if org-odd-levels-only
						(1- (* 2 (cdr desc)))
					      (cdr desc)))
			       "\\}[ \t]")))
	 (t (error "Bad refiling target description %s" desc)))
	(while (setq f (pop files))
	  (save-excursion
	    (set-buffer (if (bufferp f) f (org-get-agenda-file-buffer f)))
	    (if (bufferp f) (setq f (buffer-file-name (buffer-base-buffer f))))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(while (re-search-forward descre nil t)
		  (goto-char (point-at-bol))
		  (when (looking-at org-complex-heading-regexp)
		    (setq txt (match-string 4)
			  re (concat "^" (regexp-quote
					  (buffer-substring (match-beginning 1)
							    (match-end 4)))))
		    (if (match-end 5) (setq re (concat re "[ \t]+"
						       (regexp-quote
							(match-string 5)))))
		    (setq re (concat re "[ \t]*$"))
		    (when org-refile-use-outline-path
		      (setq txt (mapconcat 'identity
					   (append
					    (if (eq org-refile-use-outline-path 'file)
						(list (file-name-nondirectory
						       (buffer-file-name (buffer-base-buffer))))
					      (if (eq org-refile-use-outline-path 'full-file-path)
						  (list (buffer-file-name (buffer-base-buffer)))))
					    (org-get-outline-path)
					    (list txt))
					   "/")))
		    (push (list txt f re (point)) targets))
		  (goto-char (point-at-eol))))))))
      (nreverse targets))))

(defun org-get-outline-path ()
  "Return the outline path to the current entry, as a list."
  (let (rtn)
    (save-excursion
      (while (org-up-heading-safe)
	(when (looking-at org-complex-heading-regexp)
	  (push (org-match-string-no-properties 4) rtn)))
      rtn)))

(defvar org-refile-history nil
  "History for refiling operations.")

(defun org-refile (&optional goto default-buffer)
  "Move the entry at point to another heading.
The list of target headings is compiled using the information in
`org-refile-targets', which see.  This list is created before each use
and will therefore always be up-to-date.

At the target location, the entry is filed as a subitem of the target heading.
Depending on `org-reverse-note-order', the new subitem will either be the
first of the last subitem.

With prefix arg GOTO, the command will only visit the target location,
not actually move anything.
With a double prefix `C-c C-c', go to the location where the last refiling
operation has put the subtree."
  (interactive "P")
  (let* ((cbuf (current-buffer))
	 (filename (buffer-file-name (buffer-base-buffer cbuf)))
	 pos it nbuf file re level reversed)
    (if (equal goto '(16))
	(org-refile-goto-last-stored)
      (when (setq it (org-refile-get-location
		      (if goto "Goto: " "Refile to: ") default-buffer))
	(setq file (nth 1 it)
	      re (nth 2 it)
	      pos (nth 3 it))
	(setq nbuf (or (find-buffer-visiting file)
		       (find-file-noselect file)))
	(if goto
	    (progn
	      (switch-to-buffer nbuf)
	      (goto-char pos)
	      (org-show-context 'org-goto))
	  (org-copy-special)
	  (save-excursion
	    (set-buffer (setq nbuf (or (find-buffer-visiting file)
				       (find-file-noselect file))))
	    (setq reversed (org-notes-order-reversed-p))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char pos)
		(looking-at outline-regexp)
		(setq level (org-get-valid-level (funcall outline-level) 1))
		(goto-char
		 (if reversed
		     (outline-next-heading)
		   (or (save-excursion (outline-get-next-sibling))
		       (org-end-of-subtree t t)
		       (point-max))))
		(bookmark-set "org-refile-last-stored")
		(org-paste-subtree level))))
	  (org-cut-special)
	  (message "Entry refiled to \"%s\"" (car it)))))))

(defun org-refile-goto-last-stored ()
  "Go to the location where the last refile was stored."
  (interactive)
  (bookmark-jump "org-refile-last-stored")
  (message "This is the location of the last refile"))

(defun org-refile-get-location (&optional prompt default-buffer)
  "Prompt the user for a refile location, using PROMPT."
  (let ((org-refile-targets org-refile-targets)
	(org-refile-use-outline-path org-refile-use-outline-path))
    (setq org-refile-target-table (org-get-refile-targets default-buffer)))
  (unless org-refile-target-table
    (error "No refile targets"))
  (let* ((cbuf (current-buffer))
	 (filename (buffer-file-name (buffer-base-buffer cbuf)))
	 (fname (and filename (file-truename filename)))
	 (tbl (mapcar
	       (lambda (x)
		 (if (not (equal fname (file-truename (nth 1 x))))
		     (cons (concat (car x) " (" (file-name-nondirectory
						 (nth 1 x)) ")")
			   (cdr x))
		   x))
	       org-refile-target-table))
	 (completion-ignore-case t))
    (assoc (completing-read prompt tbl nil t nil 'org-refile-history)
	   tbl)))

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
	 (name (org-no-properties (match-string 1)))
	 (params (append (list :name name)
			 (read (concat "(" (match-string 3) ")")))))
    (unless (re-search-forward org-dblock-end-re nil t)
      (error "Dynamic block not terminated"))
    (setq params
	  (append params
		  (list :content (buffer-substring
				  begdel (match-beginning 0)))))
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
  (save-window-excursion
    (let* ((pos (point))
	   (line (org-current-line))
	   (params (org-prepare-dblock))
	   (name (plist-get params :name))
	   (cmd (intern (concat "org-dblock-write:" name))))
      (message "Updating dynamic block `%s' at line %d..." name line)
      (funcall cmd params)
      (message "Updating dynamic block `%s' at line %d...done" name line)
      (goto-char pos))))

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

(defconst org-additional-option-like-keywords
  '("BEGIN_HTML" "BEGIN_LaTeX" "END_HTML" "END_LaTeX"
    "ORGTBL" "HTML:" "LaTeX:" "BEGIN:" "END:" "TBLFM"
    "BEGIN_EXAMPLE" "END_EXAMPLE"))

(defun org-complete (&optional arg)
  "Perform completion on word at point.
At the beginning of a headline, this completes TODO keywords as given in
`org-todo-keywords'.
If the current word is preceded by a backslash, completes the TeX symbols
that are supported for HTML support.
If the current word is preceded by \"#+\", completes special words for
setting file options.
In the line after \"#+STARTUP:, complete valid keywords.\"
At all other locations, this simply calls the value of
`org-completion-fallback-command'."
  (interactive "P")
  (org-without-partial-completion
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
		     (require 'org-exp)
		     (append
		      (mapcar
		       (lambda (x)
			 (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
			 (cons (match-string 2 x) (match-string 1 x)))
		       (org-split-string (org-get-current-options) "\n"))
		      (mapcar 'list org-additional-option-like-keywords)))
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
			  (mapcar 'list (org-buffer-property-keys nil t t)))
		    (t (progn
			 (call-interactively org-completion-fallback-command)
			 (throw 'exit nil)))))
	    (pattern (buffer-substring-no-properties beg end))
	    (completion (try-completion pattern table confirm)))
       (cond ((eq completion t)
	      (if (not (assoc (upcase pattern) table))
		  (message "Already complete")
		(if (and (equal type :opt)
			 (not (member (car (assoc (upcase pattern) table))
				      org-additional-option-like-keywords)))
		    (insert (substring (cdr (assoc (upcase pattern) table))
				       (length pattern)))
		  (if (memq type '(:tag :prop)) (insert ":")))))
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
	      (message "Making completion list...%s" "done")))))))

;;;; TODO, DEADLINE, Comments

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let (case-fold-search)
      (if (looking-at (concat outline-regexp
			      "\\( *\\<" org-comment-string "\\>[ \t]*\\)"))
	  (replace-match "" t t nil 1)
	(if (looking-at outline-regexp)
	    (progn
	      (goto-char (match-end 0))
	      (insert org-comment-string " ")))))))

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defvar org-setting-tags nil) ; dynamically skiped

(defun org-parse-local-options (string var)
  "Parse STRING for startup setting relevant for variable VAR."
  (let ((rtn (symbol-value var))
	e opts)
    (save-match-data
      (if (or (not string) (not (string-match "\\S-" string)))
	  rtn
	(setq opts (delq nil (mapcar (lambda (x)
				       (setq e (assoc x org-startup-options))
				       (if (eq (nth 1 e) var) e nil))
				     (org-split-string string "[ \t]+"))))
	(if (not opts)
	    rtn
	  (setq rtn nil)
	  (while (setq e (pop opts))
	    (if (not (nth 3 e))
		(setq rtn (nth 2 e))
	      (if (not (listp rtn)) (setq rtn nil))
	      (push (nth 2 e) rtn)))
	  rtn)))))

(defvar org-blocker-hook nil
  "Hook for functions that are allowed to block a state change.

Each function gets as its single argument a property list, see
`org-trigger-hook' for more information about this list.

If any of the functions in this hook returns nil, the state change
is blocked.")

(defvar org-trigger-hook nil
  "Hook for functions that are triggered by a state change.

Each function gets as its single argument a property list with at least
the following elements:

 (:type type-of-change :position pos-at-entry-start
  :from old-state :to new-state)

Depending on the type, more properties may be present.

This mechanism is currently implemented for:

TODO state changes
------------------
:type  todo-state-change
:from  previous state (keyword as a string), or nil
:to    new state (keyword as a string), or nil")


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
  (save-excursion
    (catch 'exit
      (org-back-to-heading)
      (if (looking-at outline-regexp) (goto-char (1- (match-end 0))))
      (or (looking-at (concat " +" org-todo-regexp " *"))
	  (looking-at " *"))
      (let* ((match-data (match-data))
	     (startpos (point-at-bol))
	     (logging (save-match-data (org-entry-get nil "LOGGING" t)))
	     (org-log-done org-log-done)
	     (org-log-repeat org-log-repeat)
	     (org-todo-log-states org-todo-log-states)
	     (this (match-string 1))
	     (hl-pos (match-beginning 0))
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
		     ((and org-todo-key-trigger
			   (or (and (equal arg '(4)) (eq org-use-fast-todo-selection 'prefix))
			       (and (not arg) org-use-fast-todo-selection
				    (not (eq org-use-fast-todo-selection 'prefix)))))
		      ;; Use fast selection
		      (org-fast-todo-selection))
		     ((and (equal arg '(4))
			   (or (not org-use-fast-todo-selection)
			       (not org-todo-key-trigger)))
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
		     ((and (eq org-use-fast-todo-selection t) (equal arg '(4))
			   (setq arg nil))) ; hack to fall back to cycling
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
	     (change-plist (list :type 'todo-state-change :from this :to state
				 :position startpos))
	     dolog now-done-p)
	(when org-blocker-hook
	  (unless (save-excursion
		    (save-match-data
		      (run-hook-with-args-until-failure
		       'org-blocker-hook change-plist)))
	    (if (interactive-p)
		(error "TODO state change from %s to %s blocked" this state)
	      ;; fail silently
	      (message "TODO state change from %s to %s blocked" this state)
	      (throw 'exit nil))))
	(store-match-data match-data)
	(replace-match next t t)
	(unless (pos-visible-in-window-p hl-pos)
	  (message "TODO state changed to %s" (org-trim next)))
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
	(setq now-done-p (and (member state org-done-keywords)
			      (not (member this org-done-keywords))))
	(and logging (org-local-logging logging))
	(when (and (or org-todo-log-states org-log-done)
		   (not (memq arg '(nextset previousset))))
	  ;; we need to look at recording a time and note
	  (setq dolog (or (nth 1 (assoc state org-todo-log-states))
			  (nth 2 (assoc this org-todo-log-states))))
	  (when (and state
		     (member state org-not-done-keywords)
		     (not (member this org-not-done-keywords)))
	    ;; This is now a todo state and was not one before
	    ;; If there was a CLOSED time stamp, get rid of it.
	    (org-add-planning-info nil nil 'closed))
	  (when (and now-done-p org-log-done)
	    ;; It is now done, and it was not done before
	    (org-add-planning-info 'closed (org-current-time))
	    (if (and (not dolog) (eq 'note org-log-done))
		(org-add-log-setup 'done state 'findpos 'note)))
	  (when (and state dolog)
	    ;; This is a non-nil state, and we need to log it
	    (org-add-log-setup 'state state 'findpos dolog)))
	;; Fixup tag positioning
	(and org-auto-align-tags (not org-setting-tags) (org-set-tags nil t))
	(run-hooks 'org-after-todo-state-change-hook)
	(if (and arg (not (member state org-done-keywords)))
	    (setq head (org-get-todo-sequence-head state)))
	(put-text-property (point-at-bol) (point-at-eol) 'org-todo-head head)
	;; Do we need to trigger a repeat?
	(when now-done-p (org-auto-repeat-maybe state))
	;; Fixup cursor location if close to the keyword
	(if (and (outline-on-heading-p)
		 (not (bolp))
		 (save-excursion (beginning-of-line 1)
				 (looking-at org-todo-line-regexp))
		 (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
	    (progn
	      (goto-char (or (match-end 2) (match-end 1)))
	      (just-one-space)))
	(when org-trigger-hook
	  (save-excursion
	    (run-hook-with-args 'org-trigger-hook change-plist)))))))

(defun org-local-logging (value)
  "Get logging settings from a property VALUE."
  (let* (words w a)
    ;; directly set the variables, they are already local.
    (setq org-log-done nil
	  org-log-repeat nil
	  org-todo-log-states nil)
    (setq words (org-split-string value))
    (while (setq w (pop words))
      (cond
       ((setq a (assoc w org-startup-options))
	(and (member (nth 1 a) '(org-log-done org-log-repeat))
	     (set (nth 1 a) (nth 2 a))))
       ((setq a (org-extract-log-state-settings w))
	(and (member (car a) org-todo-keywords-1)
	     (push a org-todo-log-states)))))))

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
	 (expert nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 tg cnt e c tbl
	 groups ingroup)
    (save-window-excursion
      (if expert
	  (set-buffer (get-buffer-create " *Org todo*"))
	(org-switch-to-buffer-other-window (get-buffer-create " *Org todo*")))
      (erase-buffer)
      (org-set-local 'org-done-keywords done-keywords)
      (setq tbl fulltable cnt 0)
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
       ((= c ?\ ) nil)
       ((setq e (rassoc c fulltable) tg (car e))
	tg)
       (t (setq quit-flag t))))))

(defun org-entry-is-todo-p ()
  (member (org-get-todo-state) org-not-done-keywords))

(defun org-entry-is-done-p ()
  (member (org-get-todo-state) org-done-keywords))

(defun org-get-todo-state ()
  (save-excursion
    (org-back-to-heading t)
    (and (looking-at org-todo-line-regexp)
	 (match-end 2)
	 (match-string 2))))

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
(defvar org-log-note-purpose)
(defvar org-log-note-how)
(defun org-auto-repeat-maybe (done-word)
  "Check if the current headline contains a repeated deadline/schedule.
If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.
This function is run automatically after each state change to a DONE state."
  ;; last-state is dynamically scoped into this function
  (let* ((repeat (org-get-repeat))
	 (aa (assoc last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (whata '(("d" . day) ("m" . month) ("y" . year)))
	 (msg "Entry repeats: ")
	 (org-log-done nil)
	 (org-todo-log-states nil)
	 (nshiftmax 10) (nshift 0)
	 re type n what ts mb0 time)
    (when repeat
      (if (eq org-log-repeat t) (setq org-log-repeat 'state))
      (org-todo (if (eq interpret 'type) last-state head))
      (when org-log-repeat
	(if (or (memq 'org-add-log-note (default-value 'post-command-hook))
		(memq 'org-add-log-note post-command-hook))
	    ;; OK, we are already setup for some record
	    (if (eq org-log-repeat 'note)
		;; make sure we take a note, not only a time stamp
		(setq org-log-note-how 'note))
	  ;; Set up for taking a record
	  (org-add-log-setup 'state (or done-word (car org-done-keywords))
			     'findpos org-log-repeat)))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed)
      (setq re (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
		       org-deadline-time-regexp "\\)\\|\\("
		       org-ts-regexp "\\)"))
      (while (re-search-forward
	      re (save-excursion (outline-next-heading) (point)) t)
	(setq type (if (match-end 1) org-scheduled-string
		     (if (match-end 3) org-deadline-string "Plain:"))
	      ts (match-string (if (match-end 2) 2 (if (match-end 4) 4 0)))
	      mb0 (match-beginning 0))
	(when (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([dwmy]\\)" ts)
	  (setq	n (string-to-number (match-string 2 ts))
		what (match-string 3 ts))
	  (if (equal what "w") (setq n (* n 7) what "d"))
	  ;; Preparation, see if we need to modify the start date for the change
	  (when (match-end 1)
	    (setq time (save-match-data (org-time-string-to-time ts)))
	    (cond
	     ((equal (match-string 1 ts) ".")
	      ;; Shift starting date to today
	      (org-timestamp-change
	       (- (time-to-days (current-time)) (time-to-days time))
	       'day))
	     ((equal (match-string 1 ts) "+")
	      (while (or (= nshift 0)
			 (<= (time-to-days time) (time-to-days (current-time))))
		(when (= (incf nshift) nshiftmax)
		  (or (y-or-n-p (message "%d repeater intervals were not enough to shift date past today.  Continue? " nshift))
		      (error "Abort")))
		(org-timestamp-change n (cdr (assoc what whata)))
		(org-at-timestamp-p t)
		(setq ts (match-string 1))
		(setq time (save-match-data (org-time-string-to-time ts))))
	      (org-timestamp-change (- n) (cdr (assoc what whata)))
	      ;; rematch, so that we have everything in place for the real shift
	      (org-at-timestamp-p t)
	      (setq ts (match-string 1))
	      (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([dwmy]\\)" ts))))
	  (org-timestamp-change n (cdr (assoc what whata)))
	  (setq msg (concat msg type org-last-changed-timestamp " "))))
      (setq org-log-post-message msg)
      (message "%s" msg))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.
With a \\[universal-argument] prefix, also show the DONE entries.
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

(defun org-deadline (&optional remove)
  "Insert the \"DEADLINE:\" string with a timestamp to make a deadline.
With argument REMOVE, remove any deadline from the item."
  (interactive "P")
  (if remove
      (progn
	(org-remove-timestamp-with-keyword org-deadline-string)
	(message "Item no longer has a deadline."))
    (org-add-planning-info 'deadline nil 'closed)))

(defun org-schedule (&optional remove)
  "Insert the SCHEDULED: string with a timestamp to schedule a TODO item.
With argument REMOVE, remove any scheduling date from the item."
  (interactive "P")
  (if remove
      (progn
	(org-remove-timestamp-with-keyword org-scheduled-string)
	(message "Item is no longer scheduled."))
    (org-add-planning-info 'scheduled nil 'closed)))

(defun org-remove-timestamp-with-keyword (keyword)
  "Remove all time stamps with KEYWORD in the current entry."
  (let ((re (concat "\\<" (regexp-quote keyword) " +<[^>\n]+>[ \t]*"))
	beg)
    (save-excursion
      (org-back-to-heading t)
      (setq beg (point))
      (org-end-of-subtree t t)
      (while (re-search-backward re beg t)
	(replace-match "")
	(unless (string-match "\\S-" (buffer-substring (point-at-bol) (point)))
	  (delete-region (point-at-bol) (min (1+ (point)) (point-max))))))))

(defun org-add-planning-info (what &optional time &rest remove)
  "Insert new timestamp with keyword in the line directly after the headline.
WHAT indicates what kind of time stamp to add.  TIME indicated the time to use.
If non is given, the user is prompted for a date.
REMOVE indicates what kind of entries to remove.  An old WHAT entry will also
be removed."
  (interactive)
  (let (org-time-was-given org-end-time-was-given ts
			   end default-time default-input)

    (when (and (not time) (memq what '(scheduled deadline)))
      ;; Try to get a default date/time from existing timestamp
      (save-excursion
	(org-back-to-heading t)
	(setq end (save-excursion (outline-next-heading) (point)))
	(when (re-search-forward (if (eq what 'scheduled)
				     org-scheduled-time-regexp
				   org-deadline-time-regexp)
				 end t)
	  (setq ts (match-string 1)
		default-time
		(apply 'encode-time (org-parse-time-string ts))
		default-input (and ts (org-get-compact-tod ts))))))
    (when what
      ;; If necessary, get the time from the user
      (setq time (or time (org-read-date nil 'to-time nil nil
					 default-time default-input))))

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
	  (if (eobp) (insert "\n") (forward-char 1))
	  (if (and (not (looking-at outline-regexp))
		   (looking-at (concat "[^\r\n]*?" org-keyword-time-regexp
				       "[^\r\n]*"))
		   (not (equal (match-string 1) org-clock-string)))
	      (narrow-to-region (match-beginning 0) (match-end 0))
	    (insert-before-markers "\n")
	    (backward-char 1)
	    (narrow-to-region (point) (point))
	    (org-indent-to-column col))
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
	    (setq ts (org-insert-time-stamp
		      time
		      (or org-time-was-given
			  (and (eq what 'closed) org-log-done-with-time))
		      (eq what 'closed)
		      nil nil (list org-end-time-was-given)))
	    (end-of-line 1))
	  (goto-char (point-min))
	  (widen)
	  (if (and (looking-at "[ \t]+\n")
		   (equal (char-before) ?\n))
	      (backward-delete-char 1))
	  ts)))))

(defvar org-log-note-marker (make-marker))
(defvar org-log-note-purpose nil)
(defvar org-log-note-state nil)
(defvar org-log-note-how nil)
(defvar org-log-note-window-configuration nil)
(defvar org-log-note-return-to (make-marker))
(defvar org-log-post-message nil
  "Message to be displayed after a log note has been stored.
The auto-repeater uses this.")

(defun org-add-note ()
  "Add a note to the current entry.
This is done in the same way as adding a state change note."
  (interactive)
  (org-add-log-setup 'note nil t nil))

(defun org-add-log-setup (&optional purpose state findpos how)
  "Set up the post command hook to take a note.
If this is about to TODO state change, the new state is expected in STATE.
When FINDPOS is non-nil, find the correct position for the note in
the current entry.  If not, assume that it can be inserted at point."
  (save-excursion
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
    (setq org-log-note-purpose purpose
	  org-log-note-state state
	  org-log-note-how how)
    (add-hook 'post-command-hook 'org-add-log-note 'append)))

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
  (if (memq org-log-note-how '(time state))
      (org-store-log-note)
    (let ((org-inhibit-startup t)) (org-mode))
    (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
		    (cond
		     ((eq org-log-note-purpose 'clock-out) "stopped clock")
		     ((eq org-log-note-purpose 'done)  "closed todo item")
		     ((eq org-log-note-purpose 'state)
		      (format "state change to \"%s\"" org-log-note-state))
		     ((eq org-log-note-purpose 'note)
		      "this entry")
		     (t (error "This should not happen")))))
    (org-set-local 'org-finish-function 'org-store-log-note)))

(defvar org-note-abort nil) ; dynamically scoped
(defun org-store-log-note ()
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (buffer-string))
	(note (cdr (assq org-log-note-purpose org-log-note-headings)))
	lines ind)
    (kill-buffer (current-buffer))
    (while (string-match "\\`#.*\n[ \t\n]*" txt)
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
    (when (or current-prefix-arg org-note-abort) (setq lines nil))
    (when lines
      (save-excursion
	(set-buffer (marker-buffer org-log-note-marker))
	(save-excursion
	  (goto-char org-log-note-marker)
	  (move-marker org-log-note-marker nil)
	  (end-of-line 1)
	  (if (not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
	  (indent-relative nil)
	  (insert "- " (pop lines))
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
  (and org-log-post-message (message "%s" org-log-post-message)))

(defun org-sparse-tree (&optional arg)
  "Create a sparse tree, prompt for the details.
This command can create sparse trees.  You first need to select the type
of match used to create the tree:

t      Show entries with a specific TODO keyword.
T      Show entries selected by a tags match.
p      Enter a property name and its value (both with completion on existing
       names/values) and show entries with that property.
r      Show entries matching a regular expression
d      Show deadlines due within `org-deadline-warning-days'."
  (interactive "P")
  (let (ans kwd value)
    (message "Sparse tree: [/]regexp [t]odo-kwd [T]ag [p]roperty [d]eadlines [b]efore-date")
    (setq ans (read-char-exclusive))
    (cond
     ((equal ans ?d)
      (call-interactively 'org-check-deadlines))
     ((equal ans ?b)
      (call-interactively 'org-check-before-date))
     ((equal ans ?t)
      (org-show-todo-tree '(4)))
     ((equal ans ?T)
      (call-interactively 'org-tags-sparse-tree))
     ((member ans '(?p ?P))
      (setq kwd (completing-read "Property: "
				 (mapcar 'list (org-buffer-property-keys))))
      (setq value (completing-read "Value: "
				   (mapcar 'list (org-property-values kwd))))
      (unless (string-match "\\`{.*}\\'" value)
	(setq value (concat "\"" value "\"")))
      (org-tags-sparse-tree arg (concat kwd "=" value)))
     ((member ans '(?r ?R ?/))
      (call-interactively 'org-occur))
     (t (error "No such sparse tree command \"%c\"" ans)))))

(defvar org-occur-highlights nil
  "List of overlays used for occur matches.")
(make-variable-buffer-local 'org-occur-highlights)
(defvar org-occur-parameters nil
  "Parameters of the active org-occur calls.
This is a list, each call to org-occur pushes as cons cell,
containing the regular expression and the callback, onto the list.
The list can contain several entries if `org-occur' has been called
several time with the KEEP-PREVIOUS argument.  Otherwise, this list
will only contain one set of parameters.  When the highlights are
removed (for example with `C-c C-c', or with the next edit (depending
on `org-remove-highlights-with-change'), this variable is emptied
as well.")
(make-variable-buffer-local 'org-occur-parameters)

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
  (unless keep-previous
    (org-remove-occur-highlights nil nil t))
  (push (cons regexp callback) org-occur-parameters)
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
	(entry-p     (org-get-alist-option org-show-entry-below key))
	(siblings-p  (org-get-alist-option org-show-siblings key)))
    (catch 'exit
      ;; Show heading or entry text
      (if (and heading-p (not entry-p))
	  (org-flag-heading nil)    ; only show the heading
	(and (or entry-p (org-invisible-p) (org-invisible-p2))
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
    (setq org-occur-parameters nil)
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
       ((or (eq action 'set)
	    (if (featurep 'xemacs) (characterp action) (integerp action)))
	(if (not (eq action 'set))
	    (setq new action)
	  (message "Priority %c-%c, SPC to remove: "
		   org-highest-priority org-lowest-priority)
	  (setq new (read-char-exclusive)))
	(if (and (= (upcase org-highest-priority) org-highest-priority)
		 (= (upcase org-lowest-priority) org-lowest-priority))
	    (setq new (upcase new)))
	(cond ((equal new ?\ ) (setq remove t))
	      ((or (< (upcase new) org-highest-priority) (> (upcase new) org-lowest-priority))
	       (error "Priority must be between `%c' and `%c'"
		      org-highest-priority org-lowest-priority))))
       ((eq action 'up)
	(if (and (not have) (eq last-command this-command))
	    (setq new org-lowest-priority)
	  (setq new (if (and org-priority-start-cycle-with-default (not have))
			org-default-priority (1- current)))))
       ((eq action 'down)
	(if (and (not have) (eq last-command this-command))
	    (setq new org-highest-priority)
	  (setq new (if (and org-priority-start-cycle-with-default (not have))
			org-default-priority (1+ current)))))
       (t (error "Invalid action")))
      (if (or (< (upcase new) org-highest-priority)
	      (> (upcase new) org-lowest-priority))
	  (setq remove t))
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
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 (case-fold-search nil)
         lspos
	 tags tags-list tags-alist (llast 0) rtn level category i txt
	 todo marker entry priority)
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
	(org-overview)
	(org-remove-occur-highlights))
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
	  ;; add the next tags
	  (when tags
	    (setq tags (mapcar 'downcase (org-split-string tags ":"))
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr tags-alist))
		  tags))
	  (when (and tags org-use-tag-inheritance
		     (not (eq t org-use-tag-inheritance)))
	    ;; selective inheritance, remove uninherited ones
	    (setcdr (car tags-alist)
		    (org-remove-uniherited-tags (cdar tags-alist))))
	  (when (and (or (not todo-only) (member todo org-not-done-keywords))
		     (eval matcher)
		     (or (not org-agenda-skip-archived-trees)
			 (not (member org-archive-tag tags-list))))
	    (and (eq action 'agenda) (org-agenda-skip))
	    ;; list this headline

	    (if (eq action 'sparse-tree)
		(progn
		  (and org-highlight-sparse-tree-matches
		       (org-get-heading) (match-end 0)
		       (org-highlight-new-match
			(match-beginning 0) (match-beginning 1)))
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

(defun org-remove-uniherited-tags (tags)
  "Remove all tags that are not inherited from the list TAGS."
  (cond
   ((eq org-use-tag-inheritance t) tags)
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (delq nil (mapcar
	       (lambda (x) (if (string-match org-use-tag-inheritance x) x nil))
	       tags)))
   ((listp org-use-tag-inheritance)
    (org-delete-all org-use-tag-inheritance tags))))

(defvar todo-only) ;; dynamically scoped

(defun org-tags-sparse-tree (&optional todo-only match)
  "Create a sparse tree according to tags  string MATCH.
MATCH can contain positive and negative selection of tags, like
\"+WORK+URGENT-WITHBOSS\".
If optional argument TODO_ONLY is non-nil, only select lines that are
also TODO lines."
  (interactive "P")
  (org-prepare-agenda-buffers (list (current-buffer)))
  (org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match)) todo-only))

(defvar org-cached-props nil)
(defun org-cached-entry-get (pom property)
  (if (or (eq t org-use-property-inheritance)
	  (and (stringp org-use-property-inheritance)
	       (string-match org-use-property-inheritance property))
	  (and (listp org-use-property-inheritance)
	       (member property org-use-property-inheritance)))
      ;; Caching is not possible, check it directly
      (org-entry-get pom property 'inherit)
    ;; Get all properties, so that we can do complicated checks easily
    (cdr (assoc property (or org-cached-props
			     (setq org-cached-props
				   (org-entry-properties pom)))))))

(defun org-global-tags-completion-table (&optional files)
  "Return the list of all tags in all agenda buffer/files."
  (save-excursion
    (org-uniquify
     (delq nil
	   (apply 'append
		  (mapcar
		   (lambda (file)
		     (set-buffer (find-file-noselect file))
		     (append (org-get-buffer-tags)
			     (mapcar (lambda (x) (if (stringp (car-safe x))
						     (list (car-safe x)) nil))
				     org-tag-alist)))
		   (if (and files (car files))
		       files
		     (org-agenda-files))))))))

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
	(re (org-re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL\\([<=>]\\{1,2\\}\\)\\([0-9]+\\)\\|\\([[:alnum:]_]+\\)\\([<>=]\\{1,2\\}\\)\\({[^}]+}\\|\"[^\"]*\"\\|-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)\\|[[:alnum:]_@]+\\)"))
	minus tag mm
	tagsmatch todomatch tagsmatcher todomatcher kwd matcher
	orterms term orlist re-p str-p level-p level-op
	prop-p pn pv po cat-p gv)
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
		level-p (match-end 4)
		prop-p (match-end 5)
		mm (cond
		    (re-p `(org-match-any-p ,(substring tag 1 -1) tags-list))
		    (level-p
		     (setq level-op (org-op-to-function (match-string 3 term)))
		     `(,level-op level ,(string-to-number
					 (match-string 4 term))))
		    (prop-p
		     (setq pn (match-string 5 term)
			   po (match-string 6 term)
			   pv (match-string 7 term)
			   cat-p (equal pn "CATEGORY")
			   re-p (equal (string-to-char pv) ?{)
			   str-p (equal (string-to-char pv) ?\")
			   pv (if (or re-p str-p) (substring pv 1 -1) pv))
		     (setq po (org-op-to-function po str-p))
		     (if (equal pn "CATEGORY")
			 (setq gv '(get-text-property (point) 'org-category))
		       (setq gv `(org-cached-entry-get nil ,pn)))
		     (if re-p
			 (if (eq po 'org<>)
			     `(not (string-match ,pv (or ,gv "")))
			   `(string-match ,pv (or ,gv "")))
		       (if str-p
			   `(,po (or ,gv "") ,pv)
			 `(,po (string-to-number (or ,gv ""))
			       ,(string-to-number pv) ))))
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

(defun org-op-to-function (op &optional stringp)
  (setq op
	(cond
	 ((equal  op   "<"       ) '(<     string<      ))
	 ((equal  op   ">"       ) '(>     org-string>  ))
	 ((member op '("<=" "=<")) '(<=    org-string<= ))
	 ((member op '(">=" "=>")) '(>=    org-string>= ))
	 ((member op '("="  "==")) '(=     string=      ))
	 ((member op '("<>" "!=")) '(org<> org-string<> ))))
  (nth (if stringp 1 0) op))

(defun org<> (a b) (not (= a b)))
(defun org-string<= (a b) (or (string= a b) (string< a b)))
(defun org-string>= (a b) (not (string< a b)))
(defun org-string>  (a b) (and (not (string= a b)) (not (string< a b))))
(defun org-string<> (a b) (not (string= a b)))

(defun org-match-any-p (re list)
  "Does re match any element of list?"
  (setq list (mapcar (lambda (x) (string-match re x)) list))
  (delq nil list))

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically skoped param
(defvar org-tags-overlay (org-make-overlay 1 1))
(org-detach-overlay org-tags-overlay)

(defun org-get-tags-at (&optional pos)
  "Get a list of all headline tags applicable at POS.
POS defaults to point.  If tags are inherited, the list contains
the targets in the same sequence as the headlines appear, i.e.
sthe tags of the current headline come last."
  (interactive)
  (let (tags ltags lastpos parent)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (or pos (point)))
	(save-match-data
	  (condition-case nil
	      (progn
		(org-back-to-heading t)
		(while (not (equal lastpos (point)))
		  (setq lastpos (point))
		  (when (looking-at (org-re "[^\r\n]+?:\\([[:alnum:]_@:]+\\):[ \t]*$"))
		    (setq ltags (org-split-string
				 (org-match-string-no-properties 1) ":"))
		    (setq tags (append (org-remove-uniherited-tags ltags)
				       tags)))
		  (or org-use-tag-inheritance (error ""))
		  (org-up-heading-all 1)
		  (setq parent t)))
	    (error nil))))
      tags)))

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
      (if current
	  (progn
	    (insert " :" (mapconcat 'identity (nreverse current) ":") ":")
	    (org-set-tags nil t))
	(delete-horizontal-space))
      (run-hooks 'org-after-tags-change-hook))
    res))

(defun org-align-tags-here (to-col)
  ;; Assumes that this is a headline
  (let ((pos (point)) (col (current-column)) ncol tags-l p)
    (beginning-of-line 1)
    (if	(and (looking-at (org-re ".*?\\([ \t]+\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
	     (< pos (match-beginning 2)))
	(progn
	  (setq tags-l (- (match-end 2) (match-beginning 2)))
	  (goto-char (match-beginning 1))
	  (insert " ")
	  (delete-region (point) (1+ (match-beginning 2)))
	  (setq ncol (max (1+ (current-column))
			  (1+ col)
			  (if (> to-col 0)
			      to-col
			    (- (abs to-col) tags-l))))
	  (setq p (point))
	  (insert (make-string (- ncol (current-column)) ?\ ))
	  (setq ncol (current-column))
	  (tabify p (point-at-eol))
	  (org-move-to-column (min ncol col) t))
      (goto-char pos))))

(defun org-set-tags (&optional arg just-align)
  "Set the tags for the current headline.
With prefix ARG, realign all tags in headings in the current buffer."
  (interactive "P")
  (let* ((re (concat "^" outline-regexp))
	 (current (org-get-tags-string))
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
		     (org-without-partial-completion
		      (completing-read "Tags: " 'org-tags-completion-function
				       nil nil current 'org-tags-history)))))))
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
	(and (not (featurep 'xemacs)) c0 indent-tabs-mode (tabify p0 (point)))
	tags)
       (t (error "Tags alignment failed")))
      (org-move-to-column col)
      (unless just-align
	(run-hooks 'org-after-tags-change-hook)))))

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
      (org-move-to-column (- (window-width) 19) t)
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
			      (mapc (lambda (x)
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

(defun org-get-tags-string ()
  "Get the TAGS string in the current headline."
  (unless (org-on-heading-p t)
    (error "Not on a heading"))
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at (org-re ".*[ \t]\\(:[[:alnum:]_@:]+:\\)[ \t]*$"))
	(org-match-string-no-properties 1)
      "")))

(defun org-get-tags ()
  "Get the list of tags specified in the current headline."
  (org-split-string (org-get-tags-string) ":"))

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
  '("TODO" "TAGS" "ALLTAGS" "DEADLINE" "SCHEDULED" "CLOCK" "PRIORITY"
    "TIMESTAMP" "TIMESTAMP_IA")
  "The special properties valid in Org-mode.

These are properties that are not defined in the property drawer,
but in some other way.")

(defconst org-default-properties
  '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION"
    "LOCATION" "LOGGING" "COLUMNS")
  "Some properties that are used by Org-mode for various purposes.
Being in this list makes sure that they are offered for completion.")

(defconst org-property-start-re "^[ \t]*:PROPERTIES:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-property-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defun org-property-action ()
  "Do an action on properties."
  (interactive)
  (let (c)
    (org-at-property-p)
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
	  beg end range props sum-props key value string clocksum)
      (save-excursion
	(when (condition-case nil (org-back-to-heading t) (error nil))
	  (setq beg (point))
	  (setq sum-props (get-text-property (point) 'org-summaries))
	  (setq clocksum (get-text-property (point) :org-clock-minutes))
	  (outline-next-heading)
	  (setq end (point))
	  (when (memq which '(all special))
	    ;; Get the special properties, like TODO and tags
	    (goto-char beg)
	    (when (and (looking-at org-todo-line-regexp) (match-end 2))
	      (push (cons "TODO" (org-match-string-no-properties 2)) props))
	    (when (looking-at org-priority-regexp)
	      (push (cons "PRIORITY" (org-match-string-no-properties 2)) props))
	    (when (and (setq value (org-get-tags-string))
		       (string-match "\\S-" value))
	      (push (cons "TAGS" value) props))
	    (when (setq value (org-get-tags-at))
	      (push (cons "ALLTAGS" (concat ":" (mapconcat 'identity value ":") ":"))
		    props))
	    (while (re-search-forward org-maybe-keyword-time-regexp end t)
	      (setq key (if (match-end 1) (substring (org-match-string-no-properties 1) 0 -1))
		    string (if (equal key clockstr)
			       (org-no-properties
				(org-trim
				 (buffer-substring
				  (match-beginning 3) (goto-char (point-at-eol)))))
			     (substring (org-match-string-no-properties 3) 1 -1)))
	      (unless key
		(if (= (char-after (match-beginning 3)) ?\[)
		    (setq key "TIMESTAMP_IA")
		  (setq key "TIMESTAMP")))
	      (when (or (equal key clockstr) (not (assoc key props)))
		(push (cons key string) props)))

	    )

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
	  (if clocksum
	      (push (cons "CLOCKSUM"
			  (org-columns-number-to-string (/ (float clocksum) 60.)
						       'add_times))
		    props))
	  (append sum-props (nreverse props)))))))

(defun org-entry-get (pom property &optional inherit)
  "Get value of PROPERTY for entry at point-or-marker POM.
If INHERIT is non-nil and the entry does not have the property,
then also check higher levels of the hierarchy.
If INHERIT is the symbol `selective', use inheritance only if the setting
in `org-use-property-inheritance' selects PROPERTY for inheritance.
If the property is present but empty, the return value is the empty string.
If the property is not present at all, nil is returned."
  (org-with-point-at pom
    (if (and inherit (if (eq inherit 'selective)
			 (org-property-inherit-p property)
		       t))
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

(defun org-property-or-variable-value (var &optional inherit)
  "Check if there is a property fixing the value of VAR.
If yes, return this value.  If not, return the current value of the variable."
  (let ((prop (org-entry-get nil (symbol-name var) inherit)))
    (if (and prop (stringp prop) (string-match "\\S-" prop))
	(read prop)
      (symbol-value var))))

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

;; Multi-values properties are properties that contain multiple values
;; These values are assumed to be single words, separated by whitespace.
(defun org-entry-add-to-multivalued-property (pom property value)
  "Add VALUE to the words in the PROPERTY in entry at point-or-marker POM."
  (let* ((old (org-entry-get pom property))
	 (values (and old (org-split-string old "[ \t]"))))
    (unless (member value values)
      (setq values (cons value values))
      (org-entry-put pom property
		     (mapconcat 'identity values " ")))))

(defun org-entry-remove-from-multivalued-property (pom property value)
  "Remove VALUE from words in the PROPERTY in entry at point-or-marker POM."
  (let* ((old (org-entry-get pom property))
	 (values (and old (org-split-string old "[ \t]"))))
    (when (member value values)
      (setq values (delete value values))
      (org-entry-put pom property
		     (mapconcat 'identity values " ")))))

(defun org-entry-member-in-multivalued-property (pom property value)
  "Is VALUE one of the words in the PROPERTY in entry at point-or-marker POM?"
  (let* ((old (org-entry-get pom property))
	 (values (and old (org-split-string old "[ \t]"))))
    (member value values)))

(defvar org-entry-property-inherited-from (make-marker))

(defun org-entry-get-with-inheritance (property)
  "Get entry property, and search higher levels if not present."
  (let (tmp)
    (save-excursion
      (save-restriction
	(widen)
	(catch 'ex
	  (while t
	    (when (setq tmp (org-entry-get nil property))
	      (org-back-to-heading t)
	      (move-marker org-entry-property-inherited-from (point))
	      (throw 'ex tmp))
	    (or (org-up-heading-safe) (throw 'ex nil)))))
      (or tmp (cdr (assoc property org-local-properties))
	  (cdr (assoc property org-global-properties))))))

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
	(let ((buffer-invisibility-spec (org-inhibit-invisibility))) ; Emacs 21
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
	  (org-indent-line-function)))))))

(defun org-buffer-property-keys (&optional include-specials include-defaults include-columns)
  "Get all property keys in the current buffer.
With INCLUDE-SPECIALS, also list the special properties that relect things
like tags and TODO state.
With INCLUDE-DEFAULTS, also include properties that has special meaning
internally: ARCHIVE, CATEGORY, SUMMARY, DESCRIPTION, LOCATION, and LOGGING.
With INCLUDE-COLUMNS, also include property names given in COLUMN
formats in the current buffer."
  (let (rtn range cfmt cols s p)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward org-property-start-re nil t)
	  (setq range (org-get-property-block))
	  (goto-char (car range))
	  (while (re-search-forward
		  (org-re "^[ \t]*:\\([-[:alnum:]_]+\\):")
		  (cdr range) t)
	    (add-to-list 'rtn (org-match-string-no-properties 1)))
	  (outline-next-heading))))

    (when include-specials
      (setq rtn (append org-special-properties rtn)))

    (when include-defaults
      (mapc (lambda (x) (add-to-list 'rtn x)) org-default-properties))

    (when include-columns
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward
		  "^\\(#\\+COLUMNS:\\|[ \t]*:COLUMNS:\\)[ \t]*\\(.*\\)"
		  nil t)
	    (setq cfmt (match-string 2) s 0)
	    (while (string-match (org-re "%[0-9]*\\([-[:alnum:]_]+\\)")
				 cfmt s)
	      (setq s (match-end 0)
		    p (match-string 1 cfmt))
	      (unless (or (equal p "ITEM")
			  (member p org-special-properties))
		(add-to-list 'rtn (match-string 1 cfmt))))))))

    (sort rtn (lambda (a b) (string< (upcase a) (upcase b))))))

(defun org-property-values (key)
  "Return a list of all values of property KEY."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((re (concat "^[ \t]*:" key ":[ \t]*\\(\\S-.*\\)"))
	    values)
	(while (re-search-forward re nil t)
	  (add-to-list 'values (org-trim (match-string 1))))
	(delete "" values)))))

(defun org-insert-property-drawer ()
  "Insert a property drawer into the current entry."
  (interactive)
  (org-back-to-heading t)
  (looking-at outline-regexp)
  (let ((indent (- (match-end 0)(match-beginning 0)))
	(beg (point))
	(re (concat "^[ \t]*" org-keyword-time-regexp))
	end hiddenp)
    (outline-next-heading)
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward re end t))
    (setq hiddenp (org-invisible-p))
    (end-of-line 1)
    (and (equal (char-after) ?\n) (forward-char 1))
    (while (looking-at "^[ \t]*\\(:CLOCK:\\|CLOCK\\|:END:\\)")
      (beginning-of-line 2))
    (org-skip-over-state-notes)
    (skip-chars-backward " \t\n\r")
    (if (eq (char-before) ?*) (forward-char 1))
    (let ((inhibit-read-only t)) (insert "\n:PROPERTIES:\n:END:"))
    (beginning-of-line 0)
    (org-indent-to-column indent)
    (beginning-of-line 2)
    (org-indent-to-column indent)
    (beginning-of-line 0)
    (if hiddenp
	(save-excursion
	  (org-back-to-heading t)
	  (hide-entry))
      (org-flag-drawer t))))

(defun org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.
When called interactively, this will prompt for a property name, offering
completion on existing and default properties.  And then it will prompt
for a value, offering competion either on allowed values (via an inherited
xxx_ALL property) or on existing values in other instances of this property
in the current file."
  (interactive
   (let* ((prop	(completing-read
		 "Property: " (mapcar 'list (org-buffer-property-keys nil t t))))
	  (cur (org-entry-get nil prop))
	  (allowed (org-property-get-allowed-values nil prop 'table))
	  (existing (mapcar 'list (org-property-values prop)))
	  (val (if allowed
		   (completing-read "Value: " allowed nil 'req-match)
		 (completing-read
		  (concat "Value" (if (and cur (string-match "\\S-" cur))
				      (concat "[" cur "]") "")
			  ": ")
		  existing nil nil "" nil cur))))
     (list prop (if (equal val "") cur val))))
  (unless (equal (org-entry-get nil property) value)
    (org-entry-put nil property value)))

(defun org-delete-property (property)
  "In the current entry, delete PROPERTY."
  (interactive
   (let* ((prop (completing-read
		 "Property: " (org-entry-properties nil 'standard))))
     (list prop)))
  (message "Property %s %s" property
	   (if (org-entry-delete nil property)
	       "deleted"
	     "was not present in the entry")))

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

(defvar org-columns-current-fmt-compiled) ; defined in org-colview.el

(defun org-compute-property-at-point ()
  "Compute the property at point.
This looks for an enclosing column format, extracts the operator and
then applies it to the proerty in the column format's scope."
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

(defun org-find-entry-with-id (ident)
  "Locate the entry that contains the ID property with exact value IDENT.
IDENT can be a string, a symbol or a number, this function will search for
the string representation of it.
Return the position where this entry starts, or nil if there is no such entry."
  (let ((id (cond
	     ((stringp ident) ident)
	     ((symbol-name ident) (symbol-name ident))
	     ((numberp ident) (number-to-string ident))
	     (t (error "IDENT %s must be a string, symbol or number" ident))))
	(case-fold-search nil))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (re-search-forward
	       (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id) "[ \t]*$")
	       nil t)
	  (org-back-to-heading)
	  (point))))))

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
  (let* ((ts nil)
	 (default-time
	   ;; Default time is either today, or, when entering a range,
	   ;; the range start.
	   (if (or (and (org-at-timestamp-p t) (setq ts (match-string 0)))
		   (save-excursion
		     (re-search-backward
		      (concat org-ts-regexp "--?-?\\=") ; 1-3 minuses
		      (- (point) 20) t)))
	       (apply 'encode-time (org-parse-time-string (match-string 1)))
	     (current-time)))
	 (default-input (and ts (org-get-compact-tod ts)))
	 org-time-was-given org-end-time-was-given time)
    (cond
     ((and (org-at-timestamp-p)
	   (eq last-command 'org-time-stamp)
	   (eq this-command 'org-time-stamp))
      (insert "--")
      (setq time (let ((this-command this-command))
		  (org-read-date arg 'totime nil nil default-time default-input)))
      (org-insert-time-stamp time (or org-time-was-given arg)))
     ((org-at-timestamp-p)
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime nil nil default-time default-input)))
      (when (org-at-timestamp-p) ; just to get the match data
	(replace-match "")
	(setq org-last-changed-timestamp
	      (org-insert-time-stamp
	       time (or org-time-was-given arg)
	       nil nil nil (list org-end-time-was-given))))
      (message "Timestamp updated"))
     (t
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime nil nil default-time default-input)))
      (org-insert-time-stamp time (or org-time-was-given arg)
			     nil nil nil (list org-end-time-was-given))))))

;; FIXME: can we use this for something else, like computing time differences?
(defun org-get-compact-tod (s)
  (when (string-match "\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\(-\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\)?" s)
    (let* ((t1 (match-string 1 s))
	   (h1 (string-to-number (match-string 2 s)))
	   (m1 (string-to-number (match-string 3 s)))
	   (t2 (and (match-end 4) (match-string 5 s)))
	   (h2 (and t2 (string-to-number (match-string 6 s))))
	   (m2 (and t2 (string-to-number (match-string 7 s))))
	   dh dm)
      (if (not t2)
	  t1
	(setq dh (- h2 h1) dm (- m2 m1))
	(if (< dm 0) (setq dm (+ dm 60) dh (1- dh)))
	(concat t1 "+" (number-to-string dh)
		(if (/= 0 dm) (concat ":" (number-to-string dm))))))))

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

(defvar org-read-date-overlay nil)
(defvar org-dcst nil) ; dynamically scoped

(defun org-read-date (&optional with-time to-time from-string prompt
				default-time default-input)
  "Read a date, possibly a time, and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  If this command is called to replace a timestamp at point,
of to enter the second timestamp of a range, the default time is taken from the
existing stamp.  For example,
  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday (today or later)
  etc.

Furthermore you can specify a relative date by giving, as the *first* thing
in the input:  a plus/minus sign, a number and a letter [dwmy] to indicate
change in days weeks, months, years.
With a single plus or minus, the date is relative to today.  With a double
plus or minus, it is relative to the date in DEFAULT-TIME.  E.g.
  +4d           --> four days from today
  +4            --> same as above
  +2w           --> two weeks from today
  ++5           --> five days from default date

The function understands only English month and weekday abbreviations,
but this can be configured with the variables `parse-time-months' and
`parse-time-weekdays'.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
If you don't like the calendar, turn it off with
       \(setq org-read-date-popup-calendar nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to also
insert a time.  Note that when WITH-TIME is not set, you can still
enter a time, and this function will inform the calling routine about
this change.  The calling routine may then choose to change the format
used to insert the time stamp into the buffer to include the time.
With optional argument FROM-STRING, read from this string instead from
the user.  PROMPT can overwrite the default prompt.  DEFAULT-TIME is
the time/date that is used for everything that is not specified by the
user."
  (require 'parse-time)
  (let* ((org-time-stamp-rounding-minutes
	  (if (equal with-time '(16)) '(0 0) org-time-stamp-rounding-minutes))
	 (org-dcst org-display-custom-times)
	 (ct (org-current-time))
	 (def (or default-time ct))
	 (defdecode (decode-time def))
	 (dummy (progn
		  (when (< (nth 2 defdecode) org-extend-today-until)
		    (setcar (nthcdr 2 defdecode) -1)
		    (setcar (nthcdr 1 defdecode) 59)
		    (setq def (apply 'encode-time defdecode)
			  defdecode (decode-time def)))))
	 (calendar-move-hook nil)
	 (calendar-view-diary-initially-flag nil)
	 (view-diary-entries-initially nil)
	 (calendar-view-holidays-initially-flag nil)
	 (view-calendar-holidays-initially nil)
	 (timestr (format-time-string
		   (if with-time "%Y-%m-%d %H:%M" "%Y-%m-%d") def))
	 (prompt (concat (if prompt (concat prompt " ") "")
			 (format "Date+time [%s]: " timestr)))
	 ans (org-ans0 "") org-ans1 org-ans2 final)

    (cond
     (from-string (setq ans from-string))
     (org-read-date-popup-calendar
      (save-excursion
	(save-window-excursion
	  (calendar)
	  (calendar-forward-day (- (time-to-days def)
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
	    (org-defkey minibuffer-local-map [(meta shift up)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-backward-year 1))))
	    (org-defkey minibuffer-local-map [(meta shift down)]
	      (lambda () (interactive)
		(org-eval-in-calendar '(calendar-forward-year 1))))
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
		  (add-hook 'post-command-hook 'org-read-date-display)
		  (setq org-ans0 (read-string prompt default-input nil nil))
		  ;; org-ans0: from prompt
		  ;; org-ans1: from mouse click
		  ;; org-ans2: from calendar motion
		  (setq ans (concat org-ans0 " " (or org-ans1 org-ans2))))
	      (remove-hook 'post-command-hook 'org-read-date-display)
	      (use-local-map old-map)
	      (when org-read-date-overlay
		(org-delete-overlay org-read-date-overlay)
		(setq org-read-date-overlay nil)))))))

     (t ; Naked prompt only
      (unwind-protect
	  (setq ans (read-string prompt default-input nil timestr))
	(when org-read-date-overlay
	  (org-delete-overlay org-read-date-overlay)
	  (setq org-read-date-overlay nil)))))

    (setq final (org-read-date-analyze ans def defdecode))

    (if to-time
	(apply 'encode-time final)
      (if (and (boundp 'org-time-was-given) org-time-was-given)
	  (format "%04d-%02d-%02d %02d:%02d"
		  (nth 5 final) (nth 4 final) (nth 3 final)
		  (nth 2 final) (nth 1 final))
	(format "%04d-%02d-%02d" (nth 5 final) (nth 4 final) (nth 3 final))))))
(defvar def)
(defvar defdecode)
(defvar with-time)
(defun org-read-date-display ()
  "Display the currrent date prompt interpretation in the minibuffer."
  (when org-read-date-display-live
    (when org-read-date-overlay
      (org-delete-overlay org-read-date-overlay))
    (let ((p (point)))
      (end-of-line 1)
      (while (not (equal (buffer-substring
			  (max (point-min) (- (point) 4)) (point))
			 "    "))
	(insert " "))
      (goto-char p))
    (let* ((ans (concat (buffer-substring (point-at-bol) (point-max))
			" " (or org-ans1 org-ans2)))
	   (org-end-time-was-given nil)
	   (f (org-read-date-analyze ans def defdecode))
	   (fmts (if org-dcst
		     org-time-stamp-custom-formats
		   org-time-stamp-formats))
	   (fmt (if (or with-time
			(and (boundp 'org-time-was-given) org-time-was-given))
		    (cdr fmts)
		  (car fmts)))
	   (txt (concat "=> " (format-time-string fmt (apply 'encode-time f)))))
      (when (and org-end-time-was-given
		 (string-match org-plain-time-of-day-regexp txt))
	(setq txt (concat (substring txt 0 (match-end 0)) "-"
			  org-end-time-was-given
			  (substring txt (match-end 0)))))
      (setq org-read-date-overlay
	    (make-overlay (1- (point-at-eol)) (point-at-eol)))
      (org-overlay-display org-read-date-overlay txt 'secondary-selection))))

(defun org-read-date-analyze (ans def defdecode)
  "Analyze the combined answer of the date prompt."
  ;; FIXME: cleanup and comment
  (let (delta deltan deltaw deltadef year month day
	      hour minute second wday pm h2 m2 tl wday1
	      iso-year iso-weekday iso-week iso-year iso-date)

    (when (setq delta (org-read-date-get-relative ans (current-time) def))
      (setq ans (replace-match "" t t ans)
	    deltan (car delta)
	    deltaw (nth 1 delta)
            deltadef (nth 2 delta)))

    ;; Check if there is an iso week date in there
    ;; If yes, sore the info and ostpone interpreting it until the rest
    ;; of the parsing is done
    (when (string-match "\\<\\(?:\\([0-9]+\\)-\\)?[wW]\\([0-9]\\{1,2\\}\\)\\(?:-\\([0-6]\\)\\)?\\([ \t]\\|$\\)" ans)
      (setq iso-year (if (match-end 1) (org-small-year-to-year (string-to-number (match-string 1 ans))))
	    iso-weekday (if (match-end 3) (string-to-number (match-string 3 ans)))
	    iso-week (string-to-number (match-string 2 ans)))
      (setq ans (replace-match "" t t ans)))

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
    (loop for i from 1 to 2 do ; twice, for end time as well
	  (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
		     (string-match "\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\(am\\|AM\\|pm\\|PM\\)\\>" ans))
	    (setq hour (string-to-number (match-string 1 ans))
		  minute (if (match-end 3)
			     (string-to-number (match-string 3 ans))
			   0)
		  pm (equal ?p
			    (string-to-char (downcase (match-string 4 ans)))))
	    (if (and (= hour 12) (not pm))
		(setq hour 0)
	      (if (and pm (< hour 12)) (setq hour (+ 12 hour))))
	    (setq ans (replace-match (format "%02d:%02d" hour minute)
				     t t ans))))

    ;; Check if a time range is given as a duration
    (when (string-match "\\([012]?[0-9]\\):\\([0-6][0-9]\\)\\+\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?" ans)
      (setq hour (string-to-number (match-string 1 ans))
	    h2 (+ hour (string-to-number (match-string 3 ans)))
	    minute (string-to-number (match-string 2 ans))
	    m2 (+ minute (if (match-end 5) (string-to-number
					    (match-string 5 ans))0)))
      (if (>= m2 60) (setq h2 (1+ h2) m2 (- m2 60)))
      (setq ans (replace-match (format "%02d:%02d-%02d:%02d" hour minute h2 m2)
			       t t ans)))

    ;; Check if there is a time range
    (when (boundp 'org-end-time-was-given)
      (setq org-time-was-given nil)
      (when (and (string-match org-plain-time-of-day-regexp ans)
		 (match-end 8))
	(setq org-end-time-was-given (match-string 8 ans))
	(setq ans (concat (substring ans 0 (match-beginning 7))
			  (substring ans (match-end 7))))))

    (setq tl (parse-time-string ans)
	  day (or (nth 3 tl) (nth 3 defdecode))
	  month (or (nth 4 tl)
		    (if (and org-read-date-prefer-future
			     (nth 3 tl) (< (nth 3 tl) (nth 3 defdecode)))
			(1+ (nth 4 defdecode))
		      (nth 4 defdecode)))
	  year (or (nth 5 tl)
		   (if (and org-read-date-prefer-future
			    (nth 4 tl) (< (nth 4 tl) (nth 4 defdecode)))
		       (1+ (nth 5 defdecode))
		     (nth 5 defdecode)))
	  hour (or (nth 2 tl) (nth 2 defdecode))
	  minute (or (nth 1 tl) (nth 1 defdecode))
	  second (or (nth 0 tl) 0)
	  wday (nth 6 tl))

    ;; Special date definitions below
    (cond
     (iso-week
      ;; There was an iso week
      (setq year (or iso-year year)
	    day (or iso-weekday wday 1)
	    wday nil ; to make sure that the trigger below does not match
	    iso-date (calendar-gregorian-from-absolute
		      (calendar-absolute-from-iso
		       (list iso-week day year))))
; FIXME:  Should we also push ISO weeks into the future?
;      (when (and org-read-date-prefer-future
;		 (not iso-year)
;		 (< (calendar-absolute-from-gregorian iso-date)
;		    (time-to-days (current-time))))
;	(setq year (1+ year)
;	      iso-date (calendar-gregorian-from-absolute
;			(calendar-absolute-from-iso
;			 (list iso-week day year)))))
      (setq month (car iso-date)
	    year (nth 2 iso-date)
	    day (nth 1 iso-date)))
     (deltan
      (unless deltadef
	(let ((now (decode-time (current-time))))
	  (setq day (nth 3 now) month (nth 4 now) year (nth 5 now))))
      (cond ((member deltaw '("d" "")) (setq day (+ day deltan)))
	    ((equal deltaw "w") (setq day (+ day (* 7 deltan))))
	    ((equal deltaw "m") (setq month (+ month deltan)))
	    ((equal deltaw "y") (setq year (+ year deltan)))))
     ((and wday (not (nth 3 tl)))
      ;; Weekday was given, but no day, so pick that day in the week
      ;; on or after the derived date.
      (setq wday1 (nth 6 (decode-time (encode-time 0 0 0 day month year))))
      (unless (equal wday wday1)
	(setq day (+ day (% (- wday wday1 -7) 7))))))
    (if (and (boundp 'org-time-was-given)
	     (nth 2 tl))
	(setq org-time-was-given t))
    (if (< year 100) (setq year (+ 2000 year)))
    (if (< year 1970) (setq year (nth 5 defdecode))) ; not representable
    (list second minute hour day month year)))

(defvar parse-time-weekdays)

(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
  (when (string-match
	 (concat
	  "\\`[ \t]*\\([-+]\\{1,2\\}\\)"
	  "\\([0-9]+\\)?"
	  "\\([dwmy]\\|\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\)?"
	  "\\([ \t]\\|$\\)") s)
    (let* ((dir (if (match-end 1)
		    (string-to-char (substring (match-string 1 s) -1))
		  ?+))
	   (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
	   (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
	   (what (if (match-end 3) (match-string 3 s) "d"))
	   (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
	   (date (if rel default today))
	   (wday (nth 6 (decode-time date)))
	   delta)
      (if wday1
	  (progn
	    (setq delta (mod (+ 7 (- wday1 wday)) 7))
	    (if (= dir ?-) (setq delta (- delta 7)))
	    (if (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
	    (list delta "d" rel))
	(list (* n (if (= dir ?-) -1 1)) what rel)))))

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
    (select-window sw)))

;    ;; Update the prompt to show new default date
;    (save-excursion
;      (goto-char (point-min))
;      (when (and org-ans2
;		 (re-search-forward "\\[[-0-9]+\\]" nil t)
;		 (get-text-property (match-end 0) 'field))
;	(let ((inhibit-read-only t))
;	  (replace-match (concat "[" org-ans2 "]") t t)
;	  (add-text-properties (point-min) (1+ (match-end 0))
;			       (text-properties-at (1+ (point-min)))))))))

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
    (insert-before-markers (or pre ""))
    (insert-before-markers (setq stamp (format-time-string fmt time)))
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
      (insert-before-markers extra)
      (forward-char 1))
    (insert-before-markers (or post ""))
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
      (if (string-match "\\(-[0-9]+:[0-9]+\\)?\\( [.+]?\\+[0-9]+[dwmy]\\)?\\'" ts)
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

(defun org-check-before-date (date)
  "Check if there are deadlines or scheduled entries before DATE."
  (interactive (list (org-read-date)))
  (let ((case-fold-search nil)
	(regexp (concat "\\<\\(" org-deadline-string
			"\\|" org-scheduled-string
			"\\) *<\\([^>]+\\)>"))
	(callback
	 (lambda () (time-less-p
		     (org-time-string-to-time (match-string 2))
		     (org-time-string-to-time date)))))
    (message "%d entries before %s"
	     (org-occur regexp nil callback) date)))

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
     (unless (org-at-date-range-p t)
       (goto-char (point-at-bol))
       (re-search-forward org-tr-regexp-both (point-at-eol) t))
     (if (not (org-at-date-range-p t))
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
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
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

(defun org-time-string-to-absolute (s &optional daynr prefer show-all)
  "Convert a time stamp to an absolute day number.
If there is a specifyer for a cyclic time stamp, get the closest date to
DAYNR.
PREFER and SHOW_ALL are passed through to `org-closest-date'."
  (cond
   ((and daynr (string-match "\\`%%\\((.*)\\)" s))
    (if (org-diary-sexp-entry (match-string 1 s) "" date)
	daynr
      (+ daynr 1000)))
   ((and daynr (string-match "\\+[0-9]+[dwmy]" s))
    (org-closest-date s (if (and (boundp 'daynr) (integerp daynr)) daynr
			  (time-to-days (current-time))) (match-string 0 s)
			  prefer show-all))
   (t (time-to-days (apply 'encode-time (org-parse-time-string s))))))

(defun org-days-to-iso-week (days)
  "Return the iso week number."
  (require 'cal-iso)
  (car (calendar-iso-from-absolute days)))

(defun org-small-year-to-year (year)
  "Convert 2-digit years into 4-digit years.
38-99 are mapped into 1938-1999.  1-37 are mapped into 2001-2007.
The year 2000 cannot be abbreviated.  Any year lager than 99
is retrned unchanged."
  (if (< year 38)
      (setq year (+ 2000 year))
    (if (< year 100)
	(setq year (+ 1900 year))))
  year)

(defun org-time-from-absolute (d)
  "Return the time corresponding to date D.
D may be an absolute day number, or a calendar-type list (month day year)."
  (if (numberp d) (setq d (calendar-gregorian-from-absolute d)))
  (encode-time 0 0 0 (nth 1 d) (car d) (nth 2 d)))

(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org-mode."
  (require 'holidays)
  (let ((hl (funcall
	     (if (fboundp 'calendar-check-holidays)
		 'calendar-check-holidays 'check-calendar-holidays) date)))
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
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
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

(defun org-closest-date (start current change prefer show-all)
  "Find the date closest to CURRENT that is consistent with START and CHANGE.
When PREFER is `past' return a date that is either CURRENT or past.
When PREFER is `future', return a date that is either CURRENT or future.
When SHOW-ALL is nil, only return the current occurence of a time stamp."
  ;; Make the proper lists from the dates
  (catch 'exit
    (let ((a1 '(("d" . day) ("w" . week) ("m" . month) ("y" . year)))
	  dn dw sday cday n1 n2
	  d m y y1 y2 date1 date2 nmonths nm ny m2)

      (setq start (org-date-to-gregorian start)
	    current (org-date-to-gregorian
		     (if show-all
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

      (if show-all
	  (cond
	   ((eq prefer 'past) n1)
	   ((eq prefer 'future) (if (= cday n1) n1 n2))
	   (t (if (> (abs (- cday n1)) (abs (- cday n2))) n2 n1)))
	(cond
	 ((eq prefer 'past) n1)
	 ((eq prefer 'future) (if (= cday n1) n1 n2))
	 (t (if (= cday n1) n1 n2)))))))

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

(defun org-at-timestamp-p (&optional inactive-ok)
  "Determine if the cursor is in or at a timestamp."
  (interactive)
  (let* ((tsr (if inactive-ok org-ts-regexp3 org-ts-regexp2))
	 (pos (point))
	 (ans (or (looking-at tsr)
		  (save-excursion
		    (skip-chars-backward "^[<\n\r\t")
		    (if (> (point) (point-min)) (backward-char 1))
		    (and (looking-at tsr)
			 (> (- (match-end 0) pos) -1))))))
    (and ans
	 (boundp 'org-ts-what)
	 (setq org-ts-what
	      (cond
	       ((= pos (match-beginning 0))         'bracket)
	       ((= pos (1- (match-end 0)))          'bracket)
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

(defun org-toggle-timestamp-type ()
  "Toggle the type (<active> or [inactive]) of a time stamp."
  (interactive)
  (when (org-at-timestamp-p t)
    (save-excursion
      (goto-char (match-beginning 0))
      (insert (if (equal (char-after) ?<) "[" "<")) (delete-char 1)
      (goto-char (1- (match-end 0)))
      (insert (if (equal (char-after) ?>) "]" ">")) (delete-char 1))
    (message "Timestamp is now %sactive"
	     (if (equal (char-before) ?>) "in" ""))))

(defun org-timestamp-change (n &optional what)
  "Change the date in the time stamp at point.
The date will be changed by N times WHAT.  WHAT can be `day', `month',
`year', `minute', `second'.  If WHAT is not given, the cursor position
in the timestamp determines what will be changed."
  (let ((pos (point))
	with-hm inactive
	(dm (max (nth 1 org-time-stamp-rounding-minutes) 1))
	org-ts-what
	extra rem
	ts time time0)
    (if (not (org-at-timestamp-p t))
	(error "Not at a timestamp"))
    (if (and (not what) (eq org-ts-what 'bracket))
	(org-toggle-timestamp-type)
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
	   "\\(\\(-[012][0-9]:[0-5][0-9]\\)?\\( +[.+]?[-+][0-9]+[dwmy]\\)*\\)[]>]"
	   ts)
	  (setq extra (match-string 1 ts)))
      (if (string-match "^.\\{10\\}.*?[0-9]+:[0-9][0-9]" ts)
	  (setq with-hm t))
      (setq time0 (org-parse-time-string ts))
      (when (and (eq org-ts-what 'minute)
		 (eq current-prefix-arg nil))
	(setq n (* dm (cond ((> n 0) 1) ((< n 0) -1) (t 0))))
	(when (not (= 0 (setq rem (% (nth 1 time0) dm))))
	  (setcar (cdr time0) (+ (nth 1 time0)
				 (if (> n 0) (- rem) (- dm rem))))))
      (setq time
	    (encode-time (or (car time0) 0)
			 (+ (if (eq org-ts-what 'minute) n 0) (nth 1 time0))
			 (+ (if (eq org-ts-what 'hour) n 0)   (nth 2 time0))
			 (+ (if (eq org-ts-what 'day) n 0)    (nth 3 time0))
			 (+ (if (eq org-ts-what 'month) n 0)  (nth 4 time0))
			 (+ (if (eq org-ts-what 'year) n 0)   (nth 5 time0))
			 (nthcdr 6 time0)))
      (when (integerp org-ts-what)
	(setq extra (org-modify-ts-extra extra org-ts-what n dm)))
      (if (eq what 'calendar)
	  (let ((cal-date (org-get-date-from-calendar)))
	    (setcar (nthcdr 4 time0) (nth 0 cal-date)) ; month
	    (setcar (nthcdr 3 time0) (nth 1 cal-date)) ; day
	    (setcar (nthcdr 5 time0) (nth 2 cal-date)) ; year
	    (setcar time0 (or (car time0) 0))
	    (setcar (nthcdr 1 time0) (or (nth 1 time0) 0))
	    (setcar (nthcdr 2 time0) (or (nth 2 time0) 0))
	    (setq time (apply 'encode-time time0))))
      (setq org-last-changed-timestamp
	    (org-insert-time-stamp time with-hm inactive nil nil extra))
      (org-clock-update-time-maybe)
      (goto-char pos)
      ;; Try to recenter the calendar window, if any
      (if (and org-calendar-follow-timestamp-change
	       (get-buffer-window "*Calendar*" t)
	       (memq org-ts-what '(day month year)))
	  (org-recenter-calendar (time-to-days time))))))

(defun org-modify-ts-extra (s pos n dm)
  "Change the different parts of the lead-time and repeat fields in timestamp."
  (let ((idx '(("d" . 0) ("w" . 1) ("m" . 2) ("y" . 3) ("d" . -1) ("y" . 4)))
	ng h m new rem)
    (when (string-match "\\(-\\([012][0-9]\\):\\([0-5][0-9]\\)\\)?\\( +\\+\\([0-9]+\\)\\([dmwy]\\)\\)?\\( +-\\([0-9]+\\)\\([dmwy]\\)\\)?" s)
      (cond
       ((or (org-pos-in-match-range pos 2)
	    (org-pos-in-match-range pos 3))
	(setq m (string-to-number (match-string 3 s))
	      h (string-to-number (match-string 2 s)))
	(if (org-pos-in-match-range pos 2)
	    (setq h (+ h n))
	  (setq n (* dm (org-no-warnings (signum n))))
	  (when (not (= 0 (setq rem (% m dm))))
	    (setq m (+ m (if (> n 0) (- rem) (- dm rem)))))
	  (setq m (+ m n)))
	(if (< m 0) (setq m (+ m 60) h (1- h)))
	(if (> m 59) (setq m (- m 60) h (1+ h)))
	(setq h (min 24 (max 0 h)))
	(setq ng 1 new (format "-%02d:%02d" h m)))
       ((org-pos-in-match-range pos 6)
	(setq ng 6 new (car (rassoc (+ n (cdr (assoc (match-string 6 s) idx))) idx))))
       ((org-pos-in-match-range pos 5)
	(setq ng 5 new (format "%d" (max 1 (+ n (string-to-number (match-string 5 s)))))))

       ((org-pos-in-match-range pos 9)
	(setq ng 9 new (car (rassoc (+ n (cdr (assoc (match-string 9 s) idx))) idx))))
       ((org-pos-in-match-range pos 8)
	(setq ng 8 new (format "%d" (max 0 (+ n (string-to-number (match-string 8 s))))))))

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
	(calendar-view-holidays-initially-flag nil)
	(view-calendar-holidays-initially nil)
	(calendar-view-diary-initially-flag nil)
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

(defun org-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer "*Calendar*"
    (save-match-data
      (calendar-cursor-to-date))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (if (org-at-timestamp-p t)
      (org-timestamp-change 0 'calendar)
    (let ((cal-date (org-get-date-from-calendar)))
      (org-insert-time-stamp
       (encode-time 0 0 0 (nth 1 cal-date) (car cal-date) (nth 2 cal-date))))))

(defun org-minutes-to-hh:mm-string (m)
  "Compute H:MM from a number of minutes."
  (let ((h (/ m 60)))
    (setq m (- m (* 60 h)))
    (format "%d:%02d" h m)))

(defun org-hh:mm-string-to-minutes (s)
  "Convert a string H:MM to a number of minutes."
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" s)
      (+ (* (string-to-number (match-string 1 s)) 60)
	 (string-to-number (match-string 2 s)))
    0))

;;;; Agenda files

;;;###autoload
(defun org-iswitchb (&optional arg)
  "Use `iswitchb-read-buffer' to prompt for an Org buffer to switch to.
With a prefix argument, restrict available to files.
With two prefix arguments, restrict available buffers to agenda files.

Due to some yet unresolved reason, global function
`iswitchb-mode' needs to be active for this function to work."
  (interactive "P")
  (require 'iswitchb)
  (let ((enabled iswitchb-mode) blist)
    (or enabled (iswitchb-mode 1))
    (setq blist (cond ((equal arg '(4)) (org-buffer-list 'files))
		      ((equal arg '(16)) (org-buffer-list 'agenda))
		      (t (org-buffer-list))))
   (unwind-protect
       (let ((iswitchb-make-buflist-hook
	      (lambda ()
		(setq iswitchb-temp-buflist
		      (mapcar 'buffer-name blist)))))
	 (switch-to-buffer
	  (iswitchb-read-buffer
	   "Switch-to: " nil t))
	 (or enabled (iswitchb-mode -1))))))

(defun org-buffer-list (&optional predicate tmp)
  "Return a list of Org buffers.
PREDICATE can be either 'export, 'files or 'agenda.

'export restrict the list to Export buffers.
'files  restrict the list to buffers visiting Org files.
'agenda restrict the list to buffers visiting agenda files.

If TMP is non-nil, don't include temporary buffers."
  (let (filter blist)
    (setq filter
	  (cond ((eq predicate 'files) "\.org$")
		((eq predicate 'export) "\*Org .*Export")
		(t "\*Org \\|\.org$")))
    (setq blist
	  (mapcar
	   (lambda(b)
	     (let ((bname (buffer-name b))
		   (bfile (buffer-file-name b)))
	       (if (and (string-match filter bname)
			(if (eq predicate 'agenda)
			    (member bfile
				    (mapcar (lambda(f) (file-truename f))
					    org-agenda-files)) t)
			(if tmp (not (string-match "tmp" bname)) t)) b)))
	   (buffer-list)))
    (delete nil blist)))

(defun org-agenda-files (&optional unrestricted ext)
  "Get the list of agenda files.
Optional UNRESTRICTED means return the full list even if a restriction
is currently in place.
When EXT is non-nil, try to add all files that are created by adding EXT
to the file nemes.  Basically, this is a way to add the archive files
to the list, by setting EXT to \"_archive\"  If EXT is non-nil, but not
a string, \"_archive\" will be used."
  (let ((files
	 (cond
	  ((and (not unrestricted) (get 'org-agenda-files 'org-restrict)))
	  ((stringp org-agenda-files) (org-read-agenda-file-list))
	  ((listp org-agenda-files) org-agenda-files)
	  (t (error "Invalid value of `org-agenda-files'")))))
    (setq files (apply 'append
		       (mapcar (lambda (f)
				 (if (file-directory-p f)
				     (directory-files
				      f t org-agenda-file-regexp)
				   (list f)))
			       files)))
    (when org-agenda-skip-unavailable-files
      (setq files (delq nil
			(mapcar (function
				 (lambda (file)
				   (and (file-readable-p file) file)))
				files))))
    (when ext
      (setq ext (if (and (stringp ext) (string-match "\\S-" ext))
		    ext "_archive"))
      (setq files (apply 'append
			 (mapcar
			  (lambda (f)
			    (if (file-exists-p (concat f ext))
				(list f (concat f ext))
			      (list f)))
			  files))))
    files))

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
	(message "%s" (substitute-command-keys
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
  (when (file-directory-p org-agenda-files)
    (error "`org-agenda-files' cannot be a single directory"))
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
      (message "File was not in list: %s (not removed)" afile))))

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
	  (if (bufferp file)
	      (set-buffer file)
	    (org-check-agenda-file file)
	    (set-buffer (org-get-agenda-file-buffer file)))
	  (widen)
	  (setq bmp (buffer-modified-p))
	  (org-refresh-category-properties)
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
	 (fnh (if (featurep 'xemacs)
                  (font-height (get-face-font 'default))
                (face-attribute 'default :height nil)))
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
(define-key org-mode-map [backtab] 'org-shifttab)

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
(org-defkey org-mode-map "\C-c\C-xa" 'org-toggle-archive-tag)
(org-defkey org-mode-map "\C-c\C-xA" 'org-archive-to-archive-sibling)
(org-defkey org-mode-map "\C-c\C-xb" 'org-tree-to-indirect-buffer)
(org-defkey org-mode-map "\C-c\C-j" 'org-goto)
(org-defkey org-mode-map "\C-c\C-t" 'org-todo)
(org-defkey org-mode-map "\C-c\C-s" 'org-schedule)
(org-defkey org-mode-map "\C-c\C-d" 'org-deadline)
(org-defkey org-mode-map "\C-c;"    'org-toggle-comment)
(org-defkey org-mode-map "\C-c\C-v" 'org-show-todo-tree)
(org-defkey org-mode-map "\C-c\C-w" 'org-refile)
(org-defkey org-mode-map "\C-c/"    'org-sparse-tree)   ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\\"   'org-tags-sparse-tree) ; Minor-mode res.
(org-defkey org-mode-map "\C-c\C-m" 'org-ctrl-c-ret)
(org-defkey org-mode-map "\M-\C-m"  'org-insert-heading)
(org-defkey org-mode-map [(control return)] 'org-insert-heading-after-current)
(org-defkey org-mode-map "\C-c\C-x\C-n" 'org-next-link)
(org-defkey org-mode-map "\C-c\C-x\C-p" 'org-previous-link)
(org-defkey org-mode-map "\C-c\C-l" 'org-insert-link)
(org-defkey org-mode-map "\C-c\C-o" 'org-open-at-point)
(org-defkey org-mode-map "\C-c%"    'org-mark-ring-push)
(org-defkey org-mode-map "\C-c&"    'org-mark-ring-goto)
(org-defkey org-mode-map "\C-c\C-z" 'org-add-note)  ; Alternative binding
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
(org-defkey org-mode-map "\C-c\C-x<" 'org-agenda-set-restriction-lock)
(org-defkey org-mode-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
(org-defkey org-mode-map "\C-c-"    'org-ctrl-c-minus)
(org-defkey org-mode-map "\C-c*"    'org-ctrl-c-star)
(org-defkey org-mode-map "\C-c^"    'org-sort)
(org-defkey org-mode-map "\C-c\C-c" 'org-ctrl-c-ctrl-c)
(org-defkey org-mode-map "\C-c\C-k" 'org-kill-note-or-show-branches)
(org-defkey org-mode-map "\C-c#"    'org-update-checkbox-count)
(org-defkey org-mode-map "\C-m"     'org-return)
(org-defkey org-mode-map "\C-j"     'org-return-indent)
(org-defkey org-mode-map "\C-c?"    'org-table-field-info)
(org-defkey org-mode-map "\C-c "    'org-table-blank-field)
(org-defkey org-mode-map "\C-c+"    'org-table-sum)
(org-defkey org-mode-map "\C-c="    'org-table-eval-formula)
(org-defkey org-mode-map "\C-c'"    'org-table-edit-formulas)
(org-defkey org-mode-map "\C-c`"    'org-table-edit-field)
(org-defkey org-mode-map "\C-c|"    'org-table-create-or-convert-from-region)
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
(org-defkey org-mode-map "\C-c\C-x\C-j" 'org-clock-goto)
(org-defkey org-mode-map "\C-c\C-x\C-x" 'org-clock-cancel)
(org-defkey org-mode-map "\C-c\C-x\C-d" 'org-clock-display)
(org-defkey org-mode-map "\C-c\C-x\C-r" 'org-clock-report)
(org-defkey org-mode-map "\C-c\C-x\C-u" 'org-dblock-update)
(org-defkey org-mode-map "\C-c\C-x\C-l" 'org-preview-latex-fragment)
(org-defkey org-mode-map "\C-c\C-x\C-b" 'org-toggle-checkbox)
(org-defkey org-mode-map "\C-c\C-xp"    'org-set-property)
(org-defkey org-mode-map "\C-c\C-xr"    'org-insert-columns-dblock)

(define-key org-mode-map "\C-c\C-x\C-c" 'org-columns)

(when (featurep 'xemacs)
  (org-defkey org-mode-map 'button3   'popup-mode-menu))

(defvar org-table-auto-blank-field) ; defined in org-table.el
(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-table-p)
	   (progn
	     ;; check if we blank the field, and if that triggers align
	     (and (featurep 'org-table) org-table-auto-blank-field
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

;; Make pabbrev-mode expand after org-mode commands
(put 'org-self-insert-command 'pabbrev-expand-after-command t)
(put 'orgybl-self-insert-command 'pabbrev-expand-after-command t)

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
   (t (transpose-lines 1) (beginning-of-line -1))))

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
   (t (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))))

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
   ((org-clocktable-try-shift 'up arg))
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
   ((org-clocktable-try-shift 'down arg))
   (t (call-interactively 'org-next-item))))

(defun org-shiftright (&optional arg)
  "Next TODO keyword or timestamp one day later, depending on context."
  (interactive "P")
  (cond
   ((org-at-timestamp-p t) (call-interactively 'org-timestamp-up-day))
   ((org-on-heading-p) (org-call-with-arg 'org-todo 'right))
   ((org-at-item-p) (org-call-with-arg 'org-cycle-list-bullet nil))
   ((org-at-property-p) (call-interactively 'org-property-next-allowed-value))
   ((org-clocktable-try-shift 'right arg))
   (t (org-shiftcursor-error))))

(defun org-shiftleft (&optional arg)
  "Previous TODO keyword or timestamp one day earlier, depending on context."
  (interactive "P")
  (cond
   ((org-at-timestamp-p t) (call-interactively 'org-timestamp-down-day))
   ((org-on-heading-p) (org-call-with-arg 'org-todo 'left))
   ((org-at-item-p) (org-call-with-arg 'org-cycle-list-bullet 'previous))
   ((org-at-property-p)
    (call-interactively 'org-property-previous-allowed-value))
   ((org-clocktable-try-shift 'left arg))
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

- If the cursor is a the beginning of a dynamic block, update it.

- If the cursor is inside a table created by the table.el package,
  activate that table.

- If the current buffer is a remember buffer, close note and file it.
  with a prefix argument, file it without further interaction to the default
  location.

- If the cursor is on a <<<target>>>, update radio targets and corresponding
  links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list.

- If the cursor is on a checkbox, toggle it."
  (interactive "P")
  (let  ((org-enable-table-editor t))
    (cond
     ((or (and (boundp 'org-clock-overlays) org-clock-overlays)
	  org-occur-highlights
	  org-latex-fragment-image-overlays)
      (and (boundp 'org-clock-overlays) (org-remove-clock-overlays))
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
     ((save-excursion (beginning-of-line 1) (looking-at "#\\+BEGIN:"))
      ;; Dynamic block
      (beginning-of-line 1)
      (org-update-dblock))
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

(defun org-kill-note-or-show-branches ()
  "If this is a Note buffer, abort storing the note.  Else call `show-branches'."
  (interactive)
  (if (not org-finish-function)
      (call-interactively 'show-branches)
    (let ((org-note-abort t))
      (funcall org-finish-function))))

(defun org-return (&optional indent)
  "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((bobp) (if indent (newline-and-indent) (newline)))
   ((and (org-at-heading-p)
	 (looking-at
	  (org-re "\\([ \t]+\\(:[[:alnum:]_@:]+:\\)\\)[ \t]*$")))
    (org-show-entry)
    (end-of-line 1)
    (newline))
   ((org-at-table-p)
    (org-table-justify-field-maybe)
    (call-interactively 'org-table-next-row))
   (t (if indent (newline-and-indent) (newline)))))

(defun org-return-indent ()
  "Goto next table row or insert a newline and indent.
Calls `org-table-next-row' or `newline-and-indent', depending on
context.  See the individual commands for more information."
  (interactive)
  (org-return t))

(defun org-ctrl-c-star ()
  "Compute table, or change heading status of lines.
Calls `org-table-recalculate' or `org-toggle-region-headlines',
depending on context.  This will also turn a plain list item or a normal
line into a subheading."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-recalculate))
   ((org-region-active-p)
    ;; Convert all lines in region to list items
    (call-interactively 'org-toggle-region-headings))
   ((org-on-heading-p)
    (org-toggle-region-headings (point-at-bol)
				(min (1+ (point-at-eol)) (point-max))))
   ((org-at-item-p)
    ;; Convert to heading
    (let ((level (save-match-data
		   (save-excursion
		     (condition-case nil
			 (progn
			   (org-back-to-heading t)
			   (funcall outline-level))
		       (error 0))))))
      (replace-match
       (concat (make-string (org-get-valid-level level 1) ?*) " ") t t)))
   (t (org-toggle-region-headings (point-at-bol)
				  (min (1+ (point-at-eol)) (point-max))))))

(defun org-ctrl-c-minus ()
  "Insert separator line in table or modify bullet status of line.
Also turns a plain line or a region of lines into list items.
Calls `org-table-insert-hline', `org-toggle-region-items', or
`org-cycle-list-bullet', depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-insert-hline))
   ((org-on-heading-p)
    ;; Convert to item
    (save-excursion
      (beginning-of-line 1)
      (if (looking-at "\\*+ ")
	  (replace-match (concat (make-string (- (match-end 0) (point) 1) ?\ ) "- ")))))
   ((org-region-active-p)
    ;; Convert all lines in region to list items
    (call-interactively 'org-toggle-region-items))
   ((org-in-item-p)
    (call-interactively 'org-cycle-list-bullet))
   (t (org-toggle-region-items (point-at-bol)
			       (min (1+ (point-at-eol)) (point-max))))))

(defun org-toggle-region-items (beg end)
  "Convert all lines in region to list items.
If the first line is already an item, convert all list items in the region
to normal lines."
  (interactive "r")
  (let (l2 l)
    (save-excursion
      (goto-char end)
      (setq l2 (org-current-line))
      (goto-char beg)
      (beginning-of-line 1)
      (setq l (1- (org-current-line)))
      (if (org-at-item-p)
	  ;; We already have items, de-itemize
	  (while (< (setq l (1+ l)) l2)
	    (when (org-at-item-p)
	      (goto-char (match-beginning 2))
	      (delete-region (match-beginning 2) (match-end 2))
	      (and (looking-at "[ \t]+") (replace-match "")))
	    (beginning-of-line 2))
	(while (< (setq l (1+ l)) l2)
	  (unless (org-at-item-p)
	    (if (looking-at "\\([ \t]*\\)\\(\\S-\\)")
		(replace-match "\\1- \\2")))
	  (beginning-of-line 2))))))

(defun org-toggle-region-headings (beg end)
  "Convert all lines in region to list items.
If the first line is already an item, convert all list items in the region
to normal lines."
  (interactive "r")
  (let (l2 l)
    (save-excursion
      (goto-char end)
      (setq l2 (org-current-line))
      (goto-char beg)
      (beginning-of-line 1)
      (setq l (1- (org-current-line)))
      (if (org-on-heading-p)
	  ;; We already have headlines, de-star them
	  (while (< (setq l (1+ l)) l2)
	    (when (org-on-heading-p t)
	      (and (looking-at outline-regexp) (replace-match "")))
	    (beginning-of-line 2))
	(let* ((stars (save-excursion
			(re-search-backward org-complex-heading-regexp nil t)
			(or (match-string 1) "*")))
	       (add-stars (if org-odd-levels-only "**" "*"))
	       (rpl (concat stars add-stars " \\2")))
	  (while (< (setq l (1+ l)) l2)
	    (unless (org-on-heading-p)
	      (if (looking-at "\\([ \t]*\\)\\(\\S-\\)")
		  (replace-match rpl)))
	    (beginning-of-line 2)))))))

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
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p)]
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
     :style toggle :selected (org-bound-and-true-p org-table-formula-debug)]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle
     :selected (org-bound-and-true-p org-table-overlay-coordinates)]
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
     ["Cycle Visibility" org-cycle :active (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab :active (not (org-at-table-p))]
     ["Sparse Tree..." org-sparse-tree t]
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
     ["Change tag in region" 'org-change-tag-in-region (org-region-active-p)]
     "--"
     ["Set property" 'org-set-property t]
     ["Column view of properties" org-columns t]
     ["Insert Column View DBlock" org-insert-columns-dblock t])
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
     ["Goto running clock" org-clock-goto t]
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
    ["Set Restriction Lock" org-agenda-set-restriction-lock t]
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
      :style radio
      :selected (member '(org-link) buffer-invisibility-spec)]
     ["Literal Links"
      (progn
	(org-remove-from-invisibility-spec '(org-link)) (org-restart-font-lock))
      :style radio
      :selected (not (member '(org-link) buffer-invisibility-spec))])
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
      (if (featurep 'org-exp)
	  (setq org-export-with-LaTeX-fragments
		(not org-export-with-LaTeX-fragments))
	(require 'org-exp))
      :style toggle :selected (and (boundp 'org-export-with-LaTeX-fragments)
				   org-export-with-LaTeX-fragments)])
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
  (info (format "(org)%s" (or node ""))))

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
	   ["Occur in all agenda files" org-occur-in-agenda-files t]
	   "--")
	  (mapcar 'org-file-menu-entry (org-agenda-files t))))))))

;;;; Documentation

(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc 'require
	'(org-agenda org-archive org-clock org-colview
		     org-exp org-export-latex org-publish
		     org-remember org-table)))

(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org-mode, insert it into the menu."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
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

(defun org-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun org-force-self-insert (N)
  "Needed to enforce self-insert under remapping."
  (interactive "p")
  (self-insert-command N))

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


(defun org-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
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

;; FIXME: Compare with at-regexp-p Do we need both?
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

(defun org-occur-in-agenda-files (regexp &optional nlines)
  "Call `multi-occur' with buffers for all agenda files."
  (interactive "sOrg-files matching: \np")
  (let* ((files (org-agenda-files))
	 (tnames (mapcar 'file-truename files))
	 (extra org-agenda-text-search-extra-files)
	 f)
    (when (eq (car extra) 'agenda-archives)
      (setq extra (cdr extra))
      (setq files (org-add-archive-files files)))
    (while (setq f (pop extra))
      (unless (member (file-truename f) tnames)
	(add-to-list 'files f 'append)
	(add-to-list 'tnames (file-truename f) 'append)))
    (multi-occur
     (mapcar (lambda (x) (or (get-file-buffer x) (find-file-noselect x))) files)
     regexp)))

(if (boundp 'occur-mode-find-occurrence-hook)
    ;; Emacs 23
    (add-hook 'occur-mode-find-occurrence-hook
	      (lambda ()
		(when (org-mode-p)
		  (org-reveal))))
  ;; Emacs 22
  (defadvice occur-mode-goto-occurrence
    (after org-occur-reveal activate)
    (and (org-mode-p) (org-reveal)))
  (defadvice occur-mode-goto-occurrence-other-window
    (after org-occur-reveal activate)
    (and (org-mode-p) (org-reveal)))
  (defadvice occur-mode-display-occurrence
    (after org-occur-reveal activate)
    (when (org-mode-p)
      (let ((pos (occur-mode-find-occurrence)))
	(with-current-buffer (marker-buffer pos)
	  (save-excursion
	    (goto-char pos)
	    (org-reveal)))))))

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

(defun org-back-over-empty-lines ()
  "Move backwards over witespace, to the beginning of the first empty line.
Returns the number of empty lines passed."
  (let ((pos (point)))
    (skip-chars-backward " \t\n\r")
    (beginning-of-line 2)
    (goto-char (min (point) pos))
    (count-lines (point) pos)))

(defun org-skip-whitespace ()
  (skip-chars-forward " \t\n\r"))

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
    (org-move-to-column col)))

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
    (org-move-to-column col)))

(defun org-replace-escapes (string table)
  "Replace %-escapes in STRING with values in TABLE.
TABLE is an association list with keys like \"%a\" and string values.
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
not an indirect buffer."
  (let ((buf (find-buffer-visiting file)))
    (if buf
	(or (buffer-base-buffer buf) buf)
      nil)))

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
	  (if (looking-at "\\S-")
	      (progn
		(looking-at "[ \t]*\\(\\S-+\\)[ \t]*")
		(setq bullet (match-string 1)
		      btype (if (string-match "[0-9]" bullet) "n" bullet))
		(setq column (if (equal btype bullet-type) bcol tcol)))
	    (setq column (org-get-indentation)))))
       (t (setq column (org-get-indentation))))))
    (goto-char pos)
    (if (<= (current-column) (current-indentation))
	(org-indent-line-to column)
      (save-excursion (org-indent-line-to column)))
    (setq column (current-column))
    (beginning-of-line 1)
    (if (looking-at
	 "\\([ \t]+\\)\\(:[-_0-9a-zA-Z]+:\\)[ \t]*\\(\\S-.*\\(\\S-\\|$\\)\\)")
	(replace-match (concat "\\1" (format org-property-format
					     (match-string 2) (match-string 3)))
		       t nil))
    (org-move-to-column column)))

(defun org-set-autofill-regexps ()
  (interactive)
  ;; In the paragraph separator we include headlines, because filling
  ;; text in a line directly attached to a headline would otherwise
  ;; fill the headline as well.
  (org-set-local 'comment-start-skip "^#+[ \t]*")
  (org-set-local 'paragraph-separate "\f\\|\\*+ \\|[ 	]*$\\|[ \t]*[:|]")
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
		 'org-adaptive-fill-function)
  (org-set-local
   'align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode))))))

(defun org-fill-paragraph (&optional justify)
  "Re-align a table, pass through to fill-paragraph if no table."
  (let ((table-p (org-at-table-p))
	(table.el-p (org-at-table.el-p)))
    (cond ((and (equal (char-after (point-at-bol)) ?*)
		(save-excursion (goto-char (point-at-bol))
				(looking-at outline-regexp)))
	   t)					     ; skip headlines
	  (table.el-p t)			     ; skip table.el tables
	  (table-p (org-table-align) t)		     ; align org-mode tables
	  (t nil))))				     ; call paragraph-fill

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

;;; Other stuff.

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
	 (case-fold-search nil)
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
	      (org-move-to-column cc t)
	      (insert ":\n")
	      (forward-line -1))
	     ((and off (looking-at re))
	      (replace-match "" t t nil 1))
	     ((not off) (org-move-to-column cc t) (insert ":")))
	    (forward-line 1)))
      (save-excursion
	(org-back-to-heading)
	(if (looking-at (concat outline-regexp
				"\\( *\\<" org-quote-string "\\>[ \t]*\\)"))
	    (replace-match "" t t nil 1)
	  (if (looking-at outline-regexp)
	      (progn
		(goto-char (match-end 0))
		(insert org-quote-string " "))))))))

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

(defun org-kill-line (&optional arg)
  "Kill line, to tags or end of line."
  (interactive "P")
  (cond
   ((or (not org-special-ctrl-k)
	(bolp)
	(not (org-on-heading-p)))
    (call-interactively 'kill-line))
   ((looking-at (org-re ".*?\\S-\\([ \t]+\\(:[[:alnum:]_@:]+:\\)\\)[ \t]*$"))
    (kill-region (point) (match-beginning 1))
    (org-set-tags nil t))
   (t (kill-region (point) (point-at-eol)))))

(define-key org-mode-map "\C-k" 'org-kill-line)

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

(defun org-up-heading-safe ()
  "Move to the heading line of which the present line is a subheading.
This version will not throw an error.  It will return the level of the
headline found, or nil if no higher level is found."
  (let ((pos (point)) start-level level
	(re (concat "^" outline-regexp)))
    (catch 'exit
      (outline-back-to-heading t)
      (setq start-level (funcall outline-level))
      (if (equal start-level 1) (throw 'exit nil))
      (while (re-search-backward re nil t)
	(setq level (funcall outline-level))
	(if (< level start-level) (throw 'exit level)))
      nil)))

(defun org-first-sibling-p ()
  "Is this heading the first child of its parents?"
  (interactive)
  (let ((re (concat "^" outline-regexp))
	level l)
    (unless (org-at-heading-p t)
      (error "Not at a heading"))
    (setq level (funcall outline-level))
    (save-excursion
      (if (not (re-search-backward re nil t))
	  t
	(setq l (funcall outline-level))
	(< l level)))))

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
    (condition-case nil
	(progn
	  (org-back-to-heading t)
	  (outline-flag-region
	   (max (point-min) (1- (point)))
	   (save-excursion
	     (re-search-forward
	      (concat "[\r\n]\\(" outline-regexp "\\)") nil 'move)
	     (or (match-beginning 1) (point-max)))
	   nil))
      (error nil))))

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


;;;; Integration with and fixes for other packages

;;; Imenu support

(defvar org-imenu-markers nil
  "All markers currently used by Imenu.")
(make-variable-buffer-local 'org-imenu-markers)

(defun org-imenu-new-marker (&optional pos)
  "Return a new marker for use by Imenu, and remember the marker."
  (let ((m (make-marker)))
    (move-marker m (or pos (point)))
    (push m org-imenu-markers)
    m))

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (mapc (lambda (x) (move-marker x nil)) org-imenu-markers)
  (setq org-imenu-markers nil)
  (let* ((n org-imenu-depth)
	 (re (concat "^" outline-regexp))
	 (subs (make-vector (1+ n) nil))
	 (last-level 0)
	 m tree level head)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(while (re-search-backward re nil t)
	  (setq level (org-reduced-level (funcall outline-level)))
	  (when (<= level n)
	    (looking-at org-complex-heading-regexp)
	    (setq head (org-match-string-no-properties 4)
		  m (org-imenu-new-marker))
	    (org-add-props head nil 'org-imenu-marker m 'org-imenu t)
	    (if (>= level last-level)
		(push (cons head m) (aref subs level))
	      (push (cons head (aref subs (1+ level))) (aref subs level))
	      (loop for i from (1+ level) to n do (aset subs i nil)))
	    (setq last-level level)))))
    (aref subs 1)))

(eval-after-load "imenu"
  '(progn
     (add-hook 'imenu-after-jump-hook
	       (lambda () (org-show-context 'org-goto)))))

;; Speedbar support

(defvar org-speedbar-restriction-lock-overlay (org-make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(org-overlay-put org-speedbar-restriction-lock-overlay
		 'face 'org-agenda-restriction-lock)
(org-overlay-put org-speedbar-restriction-lock-overlay
		 'help-echo "Agendas are currently limited to this item.")
(org-detach-overlay org-speedbar-restriction-lock-overlay)

(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
To get rid of the restriction, use \\[org-agenda-remove-restriction-lock]."
  (interactive)
  (require 'org-agenda)
  (let (p m tp np dir txt w)
    (cond
     ((setq p (text-property-any (point-at-bol) (point-at-eol)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (save-excursion
	(save-restriction
	  (set-buffer (marker-buffer m))
	  (goto-char m)
	  (org-agenda-set-restriction-lock 'subtree))))
     ((setq p (text-property-any (point-at-bol) (point-at-eol)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (save-excursion
	(save-restriction
	  (set-buffer (find-file-noselect
		       (let ((default-directory dir))
			 (expand-file-name txt))))
	  (unless (org-mode-p)
	    (error "Cannot restrict to non-Org-mode file"))
	  (org-agenda-set-restriction-lock 'file))))
     (t (error "Don't know how to restrict Org-mode's agenda")))
    (org-move-overlay org-speedbar-restriction-lock-overlay
		      (point-at-bol) (point-at-eol))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".org")
     (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
     (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
     (add-hook 'speedbar-visiting-tag-hook
	       (lambda () (org-show-context 'org-goto)))))


;;; Fixes and Hacks for problems with other packages

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

(defun org-closed-in-range ()
  "Sparse tree of items closed in a certain time range.
Still experimental, may disappear in the future."
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


;;;; Finish up

(provide 'org)

(run-hooks 'org-load-hook)

;; arch-tag: e77da1a7-acc7-4336-b19e-efa25af3f9fd
;;; org.el ends here


