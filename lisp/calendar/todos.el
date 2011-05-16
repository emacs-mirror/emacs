;;; Todos.el --- major mode for displaying and editing Todo lists

;; Copyright (C) 1997, 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009  Free Software Foundation, Inc.

;; Author: Oliver Seidel <privat@os10000.net>
;; Maintainer: Stephen Berman <stephen.berman@gmx.net>
;; Created: 2 Aug 1997
;; Keywords: calendar, todo

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; ---------------------------------------------------------------------------

;;; Commentary:

;;  Mode Description
;;
;;	TODO is a major mode for EMACS which offers functionality to
;;	treat most lines in one buffer as a list of items one has to
;;	do.  There are facilities to add new items, which are
;;	categorised, to edit or even delete items from the buffer.
;;	The buffer contents are currently compatible with the diary,
;;	so that the list of todos-items will show up in the FANCY diary
;;	mode.
;;
;;	Notice: Besides the major mode, this file also exports the
;;	function `todos-show' which will change to the one specific
;;	TODO file that has been specified in the todos-file-do
;;	variable.  If this file does not conform to the TODO mode
;;	conventions, the todos-show function will add the appropriate
;;	header and footer.  I don't anticipate this to cause much
;;	grief, but be warned, in case you attempt to read a plain text
;;	file.
;;
;;  Preface, Quickstart Installation
;;
;;      To get this to work, make Emacs execute the line
;;
;;          (autoload 'todos "todos"
;;                    "Major mode for editing TODO lists." t)
;;          (autoload 'todos-show "todos"
;;                    "Show TODO items." t)
;;          (autoload 'todos-insert-item "todos"
;;                    "Add TODO item." t)
;;
;;      You may now enter new items by typing "M-x todos-insert-item",
;;      or enter your TODO list file by typing "M-x todos-show".
;;
;;      The TODO list file has a special format and some auxiliary
;;      information, which will be added by the todos-show function if
;;      it attempts to visit an un-initialised file.  Hence it is
;;      recommended to use the todos-show function for the first time,
;;      in order to initialise the file, but it is not necessary
;;      afterwards.
;;
;;      As these commands are quite long to type, I would recommend
;;      the addition of two bindings to your to your global keymap.  I
;;      personally have the following in my initialisation file:
;;
;;          (global-set-key "\C-ct" 'todos-show)  ; switch to TODO buffer
;;	    (global-set-key "\C-ci" 'todos-insert-item) ; insert new item
;;
;;      Note, however, that this recommendation has prompted some
;;      criticism, since the keys C-c LETTER are reserved for user
;;      functions.  I believe my recommendation is acceptable, since
;;      the Emacs Lisp Manual *Tips* section also details that the
;;      mode itself should not bind any functions to those keys.  The
;;      express aim of the above two bindings is to work outside the
;;      mode, which doesn't need the show function and offers a
;;      different binding for the insert function.  They serve as
;;      shortcuts and are not even needed (since the TODO mode will be
;;      entered by visiting the TODO file, and later by switching to
;;      its buffer).
;;
;;      If you are an advanced user of this package, please consult
;;      the whole source code for autoloads, because there are several
;;      extensions that are not explicitly listed in the above quick
;;      installation.
;;
;;  Pre-Requisites
;;
;;      This package will require the following packages to be
;;      available on the load-path:
;;
;;          time-stamp
;;          easymenu
;;
;;  Operation
;;
;;	You will have the following facilities available:
;;
;;	    M-x todos-show   will enter the todo list screen, here type
;;
;;	    +  to go to next category
;;          -  to go to previous category
;;          d  to file the current entry, including a
;;            			    comment and timestamp
;;          e  to edit the current entry
;;          E  to edit a multi-line entry
;;          f  to file the current entry, including a
;;            			    comment and timestamp
;;          i  to insert a new entry, with prefix, omit category
;;          I  to insert a new entry at current cursor position
;;	    j  jump to category
;;          k  to kill the current entry
;;          l  to lower the current entry's priority
;;          n  for the next entry
;;          p  for the previous entry
;;	    P  print
;;          q  to save the list and exit the buffer
;;          r  to raise the current entry's priority
;;          s  to save the list
;;          S  to save the list of top priorities
;;	    t  show top priority items for each category
;;
;;	When you add a new entry, you are asked for the text and then
;;	for the category.  I for example have categories for things
;;	that I want to do in the office (like mail my mum), that I
;;	want to do in town (like buy cornflakes) and things I want to
;;	do at home (move my suitcases).  The categories can be
;;	selected with the cursor keys and if you type in the name of a
;;	category which didn't exist before, an empty category of the
;;	desired name will be added and filled with the new entry.
;;
;;  Configuration
;;
;;  Variable todos-prefix
;;
;;	I would like to recommend that you use the prefix "*/*" (by
;;	leaving the variable 'todos-prefix' untouched) so that the
;;	diary displays each entry every day.
;;
;;	To understand what I mean, please read the documentation that
;;	goes with the calendar since that will tell you how you can
;;	set up the fancy diary display and use the #include command to
;;	include your todo list file as part of your diary.
;;
;;	If you have the diary package set up to usually display more
;;	than one day's entries at once, consider using
;;
;;	    "&%%(equal (calendar-current-date) date)"
;;
;;	as the value of `todos-prefix'.  Please note that this may slow
;;	down the processing of your diary file some.
;;
;;      Carsten Dominik <dominik@strw.LeidenUniv.nl> suggested that
;;
;;          "&%%(todos-cp)"
;;
;;      might be nicer and to that effect a function has been declared
;;      further down in the code.  You may wish to auto-load this.
;;
;;      Carsten also writes that that *changing* the prefix after the
;;      todo list is already established is not as simple as changing
;;      the variable - the todo files have to be changed by hand.
;;
;;  Variable todos-file-do
;;
;;	This variable is fairly self-explanatory.  You have to store
;;	your TODO list somewhere.  This variable tells the package
;;	where to go and find this file.
;;
;;  Variable todos-file-done
;;
;;	Even when you're done, you may wish to retain the entries.
;;	Given that they're timestamped and you are offered to add a
;;	comment, this can make a useful diary of past events.  It will
;;	even blend in with the EMACS diary package.  So anyway, this
;;	variable holds the name of the file for the filed todos-items.
;;
;;  Variable todos-file-top
;;
;;      File storing the top priorities of your TODO list when
;;      todos-save-top-priorities is non-nil.  Nice to include in your
;;      diary instead of the complete TODO list.
;;
;;  Variable todos-mode-hook
;;
;;	Just like other modes, too, this mode offers to call your
;;	functions before it goes about its business.  This variable
;;	will be inspected for any functions you may wish to have
;;	called once the other TODO mode preparations have been
;;	completed.
;;
;;  Variable todos-insert-threshold
;;
;;     	Another nifty feature is the insertion accuracy.  If you have
;;     	8 items in your TODO list, then you may get asked 4 questions
;;     	by the binary insertion algorithm.  However, you may not
;;     	really have a need for such accurate priorities amongst your
;;     	TODO items.  If you now think about the binary insertion
;;     	halving the size of the window each time, then the threshold
;;     	is the window size at which it will stop.  If you set the
;;     	threshold to zero, the upper and lower bound will coincide at
;;     	the end of the loop and you will insert your item just before
;;     	that point.  If you set the threshold to, e.g. 8, it will stop
;;     	as soon as the window size drops below that amount and will
;;     	insert the item in the approximate center of that window.  I
;;     	got the idea for this feature after reading a very helpful
;;     	e-mail reply from Trey Jackson <trey@cs.berkeley.edu> who
;;     	corrected some of my awful coding and pointed me towards some
;;     	good reading.  Thanks Trey!
;;
;;  Things to do
;;
;;      These originally were my ideas, but now also include all the
;;      suggestions that I included before forgetting them:
;;
;;      o   Fancy fonts for todo/top-priority buffer
;;      o   Remove todos-prefix option in todos-top-priorities
;;      o   Rename category
;;      o   Move entry from one category to another one
;;      o   Entries which both have the generic */* prefix and a
;;          "deadline" entry which are understood by diary, indicating
;;          an event (unless marked by &)
;;      o   The optional COUNT variable of todos-forward-item should be
;;          applied to the other functions performing similar tasks
;;      o   Modularization could be done for repeated elements of
;;          the code, like the completing-read lines of code.
;;	o   license / version function
;;	o   export to diary file
;;	o   todos-report-bug
;;	o   GNATS support
;;	o   elide multiline (as in bbdb, or, to a lesser degree, in
;;          outline mode)
;;	o   rewrite complete package to store data as Lisp objects
;;          and have display modes for display, for diary export,
;;          etc.  (Richard Stallman pointed out this is a bad idea)
;;      o   so base todos.el on generic-mode.el instead
;;
;;  History and Gossip
;;
;;	Many thanks to all the ones who have contributed to the
;;	evolution of this package!  I hope I have listed all of you
;;	somewhere in the documentation or at least in the RCS history!
;;
;;	Enjoy this package and express your gratitude by sending nice
;;	things to my parents' address!
;;
;;	Oliver Seidel
;;	(Lessingstr.  8, 65760 Eschborn, Federal Republic of Germany)

;;; Code:

;; (require 'time-stamp)
;; (require 'calendar) ; required by diary-lib
(require 'diary-lib)

;; ---------------------------------------------------------------------------
;;; Customizable options

(defgroup todos nil
  "Maintain categorized lists of todo items."
  :link '(emacs-commentary-link "todos")
  :version "24.1"
  :group 'calendar)

;; FIXME: need this?
(defcustom todos-initial-category "Todo"
  "Default category name offered on initializing a new Todos file."
  :type 'string
  :group 'todos)

(defcustom todos-display-categories-first nil
  "Non-nil to display category list on first visit to a Todos file."
  :type 'boolean
  :group 'todos)

(defcustom todos-prefix ""
  "String prefixed to todo items for visual distinction."
  :type 'string
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

(defcustom todos-number-prefix t
  "Non-nil to show item prefixes as consecutively increasing integers.
These reflect the priorities of the items in each category."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

;; FIXME: Update when window-width changes (add todos-reset-separator to
;; window-configuration-change-hook in todos-mode?)
(defcustom todos-done-separator (make-string (window-width) ?-)
  "String used to visual separate done from not done items.
Displayed in a before-string overlay by `todos-toggle-view-done-items'."
  :type 'string
  :initialize 'custom-initialize-default
  :set 'todos-reset-separator
  :group 'todos)

(defcustom todos-done-string "DONE "
  "Identifying string appended to the front of done todos items."
  :type 'string
  ;; :initialize 'custom-initialize-default
  ;; :set 'todos-reset-done-string
  :group 'todos)

(defcustom todos-show-with-done nil
  "Non-nil to display done items in all categories."
  :type 'boolean
  :group 'todos)

(defcustom todos-files-directory (locate-user-emacs-file "todos/")
  "Directory where user's Todos files are saved."
  :type 'directory
  :group 'todos)

(defun todos-files (&optional archives)
  "Default value of `todos-files-function'.
This returns the case-insensitive alphabetically sorted list of
files in `todos-files-directory' with the extension \".todo\".
With non-nil ARCHIVES return the list of archive files."
  (sort (directory-files todos-files-directory t
			 (if archives "\.toda$" "\.todo$") t)
	(lambda (s1 s2) (let ((cis1 (upcase s1))
			      (cis2 (upcase s2)))
			  (string< cis1 cis2)))))

(defcustom todos-files-function 'todos-files
  "Function returning the value of the variable `todos-files'.
If this function is called with an optional non-nil argument,
then it returns the value of the variable `todos-archives'."
  :type 'function
  :group 'todos)

(defcustom todos-merged-files nil
  "List of files for `todos-merged-top-priorities'."
  :type `(set ,@(mapcar (lambda (x) (list 'const x))
			(funcall todos-files-function)))
  :group 'todos)

(defcustom todos-prompt-merged-files nil
  "Non-nil to prompt for merging files for `todos-top-priorities'."
  :type 'boolean
  :group 'todos)

(defcustom todos-auto-switch-todos-file nil ;FIXME: t by default?
  "Non-nil to make a Todos file current upon changing to it."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-toggle-switch-todos-file-noninteractively
  :group 'todos)

(defcustom todos-default-todos-file (car (funcall todos-files-function))
  "Todos file visited by first session invocation of `todos-show'.
Normally this should be set by invoking `todos-change-default-file'
either directly or as a side effect of `todos-add-file'."
  :type `(radio ,@(mapcar (lambda (x) (list 'const x))
			  (funcall todos-files-function)))
  :group 'todos)

;; FIXME: make a defvar instead of a defcustom, and one for each member of todos-file
(defcustom todos-file-top "~/todos.todt" ;FIXME
  "TODO mode top priorities file."
  :type 'file
  :group 'todos)

(defcustom todos-categories-buffer "*Todos Categories*"
  "Name of buffer displayed by `todos-display-categories'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-category-label "Category"
  "Category button label in `todos-categories-buffer'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-todo-label "Todo"
  "Todo button label in `todos-categories-buffer'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-diary-label "Diary"
  "Diary button label in `todos-categories-buffer'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-done-label "Done"
  "Done button label in `todos-categories-buffer'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-archived-label "Archived"
  "Archived button label in `todos-categories-buffer'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-number-separator " | "
  "String between number and category in `todos-categories-mode'.
This separates the number from the category name in the default
categories display according to priority."
  :type 'string
  :group 'todos)

(defcustom todos-categories-align 'center
  ""
  :type '(radio (const left) (const center) (const right))
  :group 'todos)

;; FIXME: set for each Todos file?
(defcustom todos-ignore-archived-categories nil
  "Non-nil to ignore categories with only archived items.
When non-nil such categories are omitted from `todos-categories'
and hence from commands that use this variable.  An exception is
\\[todos-display-categories], which displays all categories; but
those with only archived items are shown in `todos-archived-only'
face and clicking them in Todos Categories mode visits the
archived categories."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-reset-categories
  :group 'todos)

(defcustom todos-archived-categories-buffer "*Todos Archived Categories*"
  "Name of buffer displayed by `todos-display-categories'."
  :type 'string
  :group 'todos)

(defcustom todos-edit-buffer "*Todos Edit*"
  "TODO Edit buffer name."
  :type 'string
  :group 'todos)

(defcustom todos-include-in-diary nil
  "Non-nil to allow new Todo items to be included in the diary."
  :type 'boolean
  :group 'todos)

(defcustom todos-nondiary-marker '("[" "]")
  "List of strings surrounding item date to block diary inclusion.
The first string is inserted before the item date and must be a
non-empty string that does not match a diary date in order to
have its intended effect.  The second string is inserted after
the diary date."
  :type '(list string string)
  :group 'todos
  :initialize 'custom-initialize-default
  :set 'todos-reset-nondiary-marker)

(defcustom todos-print-function 'ps-print-buffer-with-faces
  "Function to print the current buffer."
  :type 'symbol
  :group 'todos)

(defcustom todos-show-priorities 1
  "Default number of priorities to show by \\[todos-top-priorities].
0 means show all entries."
  :type 'integer
  :group 'todos)

(defcustom todos-print-priorities 0
  "Default number of priorities to print by \\[todos-print].
0 means print all entries."
  :type 'integer
  :group 'todos)

(defcustom todos-save-top-priorities-too t
  "Non-nil makes `todos-save' automatically save top-priorities in `todos-file-top'."
  :type 'boolean
  :group 'todos)

(defcustom todos-completion-ignore-case t ;; FIXME: nil for release
  "Non-nil means don't consider case significant in `todos-read-category'."
  :type 'boolean
  :group 'todos)

(defcustom todos-always-add-time-string nil
  "Non-nil adds current time to a new item's date header by default.
When the Todos insertion commands have a non-nil \"maybe-notime\"
argument, this reverses the effect of
`todos-always-add-time-string': if t, these commands omit the
current time, if nil, they include it."
  :type 'boolean
  :group 'todos)

(defcustom todos-wrap-lines t
  ""
  :group 'todos
  :type 'boolean)

(defcustom todos-line-wrapping-function 'todos-wrap-and-indent
  ""
  :group 'todos
  :type 'function)

(defcustom todos-indent-to-here 6
    ""
  :type 'integer
  :group 'todos)

;; ---------------------------------------------------------------------------
;;; Faces

(defgroup todos-faces nil
  "Faces for the Todos modes."
  :version "24.1"
  :group 'todos)

(defface todos-prefix-string
  '((t
     :inherit font-lock-constant-face
     ))
  "Face for Todos prefix string."
  :group 'todos-faces)

(defface todos-button
  '((t
     :inherit widget-field
     ))
  "Face for buttons in todos-display-categories."
  :group 'todos-faces)

(defface todos-sorted-column
  '((t
     :inherit fringe
     ))
  "Face for buttons in todos-display-categories."
  :group 'todos-faces)

(defface todos-archived-only
  '((t
     (:inherit (shadow))
     ))
  "Face for archived-only categories in todos-display-categories."
  :group 'todos-faces)

(defface todos-search
  '((t
     :inherit match
     ))
  "Face for matches found by todos-search."
  :group 'todos-faces)

(defface todos-date
  '((t
     :inherit diary
     ))
  "Face for Todos prefix string."
  :group 'todos-faces)
(defvar todos-date-face 'todos-date)

(defface todos-time
  '((t
     :inherit diary-time
     ))
  "Face for Todos prefix string."
  :group 'todos-faces)
(defvar todos-time-face 'todos-time)

(defface todos-done
  '((t
     :inherit font-lock-comment-face
     ))
  "Face for done Todos item header string."
  :group 'todos-faces)
(defvar todos-done-face 'todos-done)

(defface todos-done-sep
  '((t
     :inherit font-lock-type-face
     ))
  "Face for separator string bewteen done and not done Todos items."
  :group 'todos-faces)
(defvar todos-done-sep-face 'todos-done-sep)

(defvar todos-font-lock-keywords
  (list
   '(todos-date-string-match 1 todos-date-face t)
   '(todos-time-string-match 1 todos-time-face t)
   '(todos-done-string-match 0 todos-done-face t)
   '(todos-category-string-match 1 todos-done-sep-face t))
  "Font-locking for Todos mode.")

;; ---------------------------------------------------------------------------
;;; Modes setup

(defvar todos-files (funcall todos-files-function)
  "List of user's Todos files.")

(defvar todos-archives (funcall todos-files-function t)
  "List of user's Todos archives.")

(defvar todos-categories nil
  "List of categories in the current Todos file.
The elements are lists whose car is a category name and whose cdr
is the category's property list.")

(defvar todos-insertion-map
  (let ((map (make-keymap)))
    (define-key map "i"     'todos-insert-item)
    (define-key map "h"     'todos-insert-item-here)
    (define-key map "dd"    'todos-insert-item-ask-date)
    (define-key map "dtt"   'todos-insert-item-ask-date-time)
    (define-key map "dtyy"  'todos-insert-item-ask-date-time-for-diary)
    (define-key map "dtyh"  'todos-insert-item-ask-date-time-for-diary-here)
    (define-key map "dth"   'todos-insert-item-ask-date-time-here)
    (define-key map "dmm"   'todos-insert-item-ask-date-maybe-notime)
    (define-key map "dmyy"  'todos-insert-item-ask-date-maybe-notime-for-diary)
    (define-key map "dmyh"  'todos-insert-item-ask-date-maybe-notime-for-diary-here)
    (define-key map "dmh"   'todos-insert-item-ask-date-maybe-notime-here)
    (define-key map "dyy"   'todos-insert-item-ask-date-for-diary)
    (define-key map "dyh"   'todos-insert-item-ask-date-for-diary-here)
    (define-key map "dh"    'todos-insert-item-ask-date-here)
    (define-key map "nn"    'todos-insert-item-ask-dayname)
    (define-key map "ntt"   'todos-insert-item-ask-dayname-time)
    (define-key map "ntyy"  'todos-insert-item-ask-dayname-time-for-diary)
    (define-key map "ntyh"  'todos-insert-item-ask-dayname-time-for-diary-here)
    (define-key map "nth"   'todos-insert-item-ask-dayname-time-here)
    (define-key map "nmm"   'todos-insert-item-ask-dayname-maybe-notime)
    (define-key map "nmyy"  'todos-insert-item-ask-dayname-maybe-notime-for-diary)
    (define-key map "nmyh"  'todos-insert-item-ask-dayname-maybe-notime-for-diary-here)
    (define-key map "nmh"   'todos-insert-item-ask-dayname-maybe-notime-here)
    (define-key map "nyy"   'todos-insert-item-ask-dayname-for-diary)
    (define-key map "nyh"   'todos-insert-item-ask-dayname-for-diary-here)
    (define-key map "nh"    'todos-insert-item-ask-dayname-here)
    (define-key map "tt"    'todos-insert-item-ask-time)
    (define-key map "tyy"   'todos-insert-item-ask-time-for-diary)
    (define-key map "tyh"   'todos-insert-item-ask-time-for-diary-here)
    (define-key map "th"    'todos-insert-item-ask-time-here)
    (define-key map "mm"    'todos-insert-item-maybe-notime)
    (define-key map "myy"   'todos-insert-item-maybe-notime-for-diary)
    (define-key map "myh"   'todos-insert-item-maybe-notime-for-diary-here)
    (define-key map "mh"    'todos-insert-item-maybe-notime-here)
    (define-key map "yy"    'todos-insert-item-for-diary)
    (define-key map "yh"    'todos-insert-item-for-diary-here)
    map)
  "Keymap for Todos mode insertion commands.")

(defvar todos-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    ;; navigation commands
    (define-key map "f" 'todos-forward-category)
    (define-key map "b" 'todos-backward-category)
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "J" 'todos-jump-to-category-other-file)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    (define-key map "S" 'todos-search)
    (define-key map "X" 'todos-clear-matches)
    ;; display commands
    (define-key map "Cd" 'todos-display-categories) ;FIXME: Cs todos-show-categories?
    ;; (define-key map "" 'todos-display-categories-alphabetically)
    (define-key map "H" 'todos-highlight-item)
    (define-key map "N" 'todos-toggle-item-numbering)
    ;; (define-key map "" 'todos-toggle-display-date-time)
    (define-key map "P" 'todos-print)
    (define-key map "v" 'todos-toggle-view-done-items)
    (define-key map "V" 'todos-toggle-show-done-only)
    (define-key map "Av" 'todos-view-archived-items)
    (define-key map "As" 'todos-switch-to-archive)
    (define-key map "Ac" 'todos-choose-archive)
    (define-key map "Y" 'todos-diary-items)
    (define-key map "t" 'todos-top-priorities)
    (define-key map "T" 'todos-merged-top-priorities)
    ;; (define-key map "" 'todos-save-top-priorities)
    ;; editing commands
    (define-key map "Fa" 'todos-add-file)
    (define-key map "Ca" 'todos-add-category)
    (define-key map "Cr" 'todos-rename-category)
    (define-key map "Cm" 'todos-move-category)
    (define-key map "Ck" 'todos-delete-category)
    (define-key map "d" 'todos-item-done)
    (define-key map "ee" 'todos-edit-item)
    (define-key map "em" 'todos-edit-multiline)
    (define-key map "eh" 'todos-edit-item-header)
    (define-key map "ed" 'todos-edit-item-date)
    (define-key map "et" 'todos-edit-item-time)
    (define-key map "i" todos-insertion-map)
    (define-key map "k" 'todos-delete-item)
    (define-key map "m" 'todos-move-item)
    (define-key map "M" 'todos-move-item-to-file)
    (define-key map "-" 'todos-raise-item-priority)
    (define-key map "+" 'todos-lower-item-priority)
    (define-key map "#" 'todos-set-item-priority)
    (define-key map "u" 'todos-item-undo)
    (define-key map "Ad" 'todos-archive-done-items)
    (define-key map "y" 'todos-toggle-item-diary-inclusion)
    ;; (define-key map "" 'todos-toggle-diary-inclusion)
    (define-key map "s" 'todos-save)
    (define-key map "q" 'todos-quit)
    (define-key map [remap newline] 'newline-and-indent)
    map)
  "Todos mode keymap.")

(defvar todos-archive-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; navigation commands
    (define-key map "f" 'todos-forward-category)
    (define-key map "b" 'todos-backward-category)
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    ;; display commands
    (define-key map "C" 'todos-display-categories)
    (define-key map "H" 'todos-highlight-item)
    (define-key map "N" 'todos-toggle-item-numbering)
    ;; (define-key map "" 'todos-toggle-display-date-time)
    (define-key map "P" 'todos-print)
    (define-key map "q" 'todos-quit)
    (define-key map "s" 'todos-save)
    (define-key map "S" 'todos-search)
    (define-key map "t" 'todos-show)	;FIXME: should show same category
    (define-key map "u" 'todos-unarchive-category)
    map)
  "Todos Archive mode keymap.")

(defvar todos-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-q" 'todos-edit-quit)
    (define-key map [remap newline] 'newline-and-indent)
    map)
  "Todos Edit mode keymap.")

(defvar todos-categories-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" 'todos-display-categories-alphabetically)
    (define-key map "c" 'todos-display-categories)
    (define-key map "+" 'todos-lower-category)
    (define-key map "-" 'todos-raise-category)
    (define-key map "n" 'forward-button)
    (define-key map "p" 'backward-button)
    (define-key map [tab] 'forward-button)
    (define-key map [backtab] 'backward-button)
    (define-key map "q" 'todos-quit)
    ;; (define-key map "A" 'todos-add-category)
    ;; (define-key map "D" 'todos-delete-category)
    ;; (define-key map "R" 'todos-rename-category)
    map)
  "Todos Categories mode keymap.")

(defvar todos-top-priorities-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    ;; navigation commands
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    ;; (define-key map "S" 'todos-search)
    ;; display commands
    (define-key map "C" 'todos-display-categories)
    ;; (define-key map "" 'todos-display-categories-alphabetically)
    (define-key map "H" 'todos-highlight-item)
    (define-key map "N" 'todos-toggle-item-numbering)
    ;; (define-key map "" 'todos-toggle-display-date-time)
    (define-key map "P" 'todos-print)
    (define-key map "q" 'todos-quit)
    (define-key map "s" 'todos-save)
    (define-key map "V" 'todos-view-archive)
    (define-key map "v" 'todos-toggle-view-done-items)
    (define-key map "Y" 'todos-diary-items)
    ;; (define-key map "S" 'todos-save-top-priorities)
    ;; editing commands
    (define-key map "l" 'todos-lower-item-priority)
    (define-key map "r" 'todos-raise-item-priority)
    (define-key map "#" 'todos-set-item-priority)
    map)
  "Todos Top Priorities mode keymap.")

(defvar todos-current-todos-file nil
  "Variable holding the name of the currently active Todos file.
Automatically set by `todos-switch-todos-file'.")

(defvar todos-category-number 0
  "Number.")

(defvar todos-tmp-buffer-name " *todo tmp*")

(defvar todos-category-beg "--==-- "
  "String marking beginning of category (inserted with its name).")

(defvar todos-category-done "==--== DONE "
  "String marking beginning of category's done items.")

(defvar todos-nondiary-start (nth 0 todos-nondiary-marker)
  "String inserted before item date to block diary inclusion.")

(defvar todos-nondiary-end (nth 1 todos-nondiary-marker)
  "String inserted after item date matching todos-nondiary-start.")

(defvar todos-show-done-only nil
  "If non-nil display only done items in current category.
Set by `todos-toggle-show-done-only' and used by
`todos-category-select'.")

(easy-menu-define
  todos-menu todos-mode-map "Todos Menu"
  '("Todos"
    ("Navigation"
     ["Next Item"            todos-forward-item t]
     ["Previous Item"        todos-backward-item t]
     "---"
     ["Next Category"        todos-forward-category t]
     ["Previous Category"    todos-backward-category t]
     ["Jump to Category"     todos-jump-to-category t]
     ["Jump to Category in Other File" todos-jump-to-category-other-file t]
     "---"
     ["Search Todos File"    todos-search t]
     ["Clear Highlighting on Search Matches" todos-category-done t])
    ("Display"
     ["List Current Categories" todos-display-categories t]
     ["List Categories Alphabetically" todos-display-categories-alphabetically t]
     ["Turn Item Highlighting on/off" todos-highlight-item t]
     ["Turn Item Numbering on/off" todos-toggle-item-numbering t]
     ["Turn Item Time Stamp on/off" todos-toggle-display-date-time t]
     ["View/Hide Done Items" todos-toggle-view-done-items t]
     "---"
     ["View Diary Items" todos-diary-items t]
     ["View Top Priority Items" todos-top-priorities t]
     ["View Merged Top Priority Items" todos-merged-top-priorities t]
     "---"
     ["View Archive" todos-view-archive t]
     ["Print Category"     todos-print-category t])
    ("Editing"
     ["Insert New Item"      todos-insert-item t]
     ["Insert Item Here"     todos-insert-item-here t]
     ("More Insertion Commands")
     ["Edit Item"            todos-edit-item t]
     ["Edit Multiline Item"  todos-edit-multiline t]
     ["Edit Item Header"     todos-edit-item-header t]
     ["Edit Item Date"       todos-edit-item-date t]
     ["Edit Item Time"       todos-edit-item-time t]
     "---"
     ["Lower Item Priority"  todos-lower-item-priority t]
     ["Raise Item Priority"  todos-raise-item-priority t]
     ["Set Item Priority" todos-set-item-priority t]
     ["Move (Recategorize) Item" todos-move-item t]
     ["Delete Item"          todos-delete-item t]
     ["Undo Done Item" todos-item-undo t]
     ["Mark/Unmark Item for Diary" todos-toggle-item-diary-inclusion t]
     ["Mark/Unmark Items for Diary" todos-toggle-diary-inclusion t]
     ["Mark & Hide Done Item" todos-item-done t]
     ["Archive Done Items" todos-archive-done-items t]
     "---"
     ["Add New Todos File" todos-add-file t]
     ["Add New Category" todos-add-category t]
     ["Delete Current Category" todos-delete-category t]
     ["Rename Current Category" todos-rename-category t]
     "---"
     ["Save Todos File"      todos-save t]
     ["Save Top Priorities"  todos-save-top-priorities t])
    "---"
    ["Quit"                 todos-quit t]
    ))

;; FIXME: remove when part of Emacs
(add-to-list 'auto-mode-alist '("\\.todo\\'" . todos-mode))
(add-to-list 'auto-mode-alist '("\\.toda\\'" . todos-archive-mode))

(defun todos-modes-set-1 ()
  ""
  (set (make-local-variable 'font-lock-defaults) '(todos-font-lock-keywords t))
  (set (make-local-variable 'indent-line-function) 'todos-indent)
  (when todos-wrap-lines (funcall todos-line-wrapping-function))
)  

(defun todos-modes-set-2 ()
  ""
  (add-to-invisibility-spec 'todos)
  (setq buffer-read-only t)
  (set (make-local-variable 'hl-line-range-function)
       (lambda() (when (todos-item-end)
		   (cons (todos-item-start) (todos-item-end)))))
)  

;; ;; As calendar reads included Todos file before todos-mode is loaded.
;; ;;;###autoload
(define-derived-mode todos-mode nil "Todos" ()
  "Major mode for displaying, navigating and editing Todo lists.

\\{todos-mode-map}"
  (easy-menu-add todos-menu)
  (todos-modes-set-1)
  (todos-modes-set-2)
  (set (make-local-variable 'todos-show-done-only) nil)
  (when todos-auto-switch-todos-file
    (add-hook 'post-command-hook
	      'todos-switch-todos-file nil t)))

(define-derived-mode todos-archive-mode nil "Todos-Arch" ()
  "Major mode for archived Todos categories.

\\{todos-archive-mode-map}"
  (todos-modes-set-1)
  (todos-modes-set-2)
  (set (make-local-variable 'todos-show-done-only) t)
  (when todos-auto-switch-todos-file
      (add-hook 'post-command-hook
		'todos-switch-todos-file nil t)))

(define-derived-mode todos-edit-mode nil "Todos-Ed" ()
  "Major mode for editing multiline Todo items.

\\{todos-edit-mode-map}"
  (todos-modes-set-1))

(define-derived-mode todos-categories-mode nil "Todos-Cats" ()
  "Major mode for displaying and editing Todos categories.

\\{todos-categories-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (setq buffer-read-only t))

(define-derived-mode todos-top-priorities-mode nil "Todos-Top" ()
  "Mode for displaying and reprioritizing top priority Todos.

\\{todos-top-priorites-mode-map}"
  (todos-modes-set-1)
  (todos-modes-set-2))

(defun todos-save ()
  "Save the TODO list."
  (interactive)
  ;; (todos-update-categories-sexp)
  (save-buffer)
  ;; (if todos-save-top-priorities-too (todos-save-top-priorities))
  )

(defun todos-quit ()
  "Done with TODO list for now."
  (interactive)
  (cond ((eq major-mode 'todos-categories-mode)
	 (kill-buffer)
	 (setq todos-descending-counts-store nil)
	 (setq todos-categories nil)
	 (todos-show))
	((member major-mode (list 'todos-mode 'todos-archive-mode))
	 (todos-save)
	 (bury-buffer))))

;; ---------------------------------------------------------------------------
;;; Commands

;;; Display

;;;###autoload
(defun todos-show (&optional solicit-file)
  "Visit the current Todos file and display one of its categories.

With non-nil prefix argument SOLICIT-FILE ask for file to visit,
otherwise the first invocation of this command in a session
visits `todos-default-todos-file' (creating it if it does not yet
exist).  Subsequent invocations from outside of Todos mode
revisit this file or whichever Todos file has been made
current (e.g. by calling `todos-switch-todos-file').

The category displayed is initially the first member of
`todos-categories' for the current Todos file, subsequently
whichever category is current.  If
`todos-display-categories-first' is non-nil, then the first
invocation of `todos-show' displays a clickable listing of the
categories in the current Todos file."
  (interactive "P")
  ;; ;; Make this a no-op if called interactively in narrowed Todos mode, since
  ;; ;; it is redundant in that case, but in particular to work around the bug of
  ;; ;; item prefix reduplication with show-paren-mode enabled.
  ;; (unless (and (called-interactively-p)
  ;; 	       (or (eq major-mode 'todos-mode) (eq major-mode 'todos-archive-mode))
  ;; 	       (< (- ( point-max) (point-min)) (buffer-size)))
    (when (and (called-interactively-p)
	       (or solicit-file
		   (member todos-current-todos-file todos-archives)))
      (setq todos-current-todos-file nil
	    todos-categories nil
	    todos-category-number 0))
    (let ((first-visit (or (not todos-current-todos-file) ;first call
			   ;; after switching to a not yet visited Todos file
			   (not (buffer-live-p
				 (get-file-buffer todos-current-todos-file))))))
      (if solicit-file
	  (setq todos-current-todos-file
		(todos-read-file-name "Select a Todos file to visit: "))
	(or todos-current-todos-file
	    (setq todos-current-todos-file (or todos-default-todos-file
					       (todos-add-file)))))
      (if (and first-visit todos-display-categories-first)
	  (todos-display-categories)
	(find-file todos-current-todos-file)
	;; (or (eq major-mode 'todos-mode) (todos-mode))
	;; initialize new Todos file
	(if (zerop (buffer-size))
	    (setq todos-category-number (todos-add-category))
	  ;; FIXME: let user choose category?
	  (if (zerop todos-category-number) (setq todos-category-number 1)))
	(or todos-categories
	    (setq todos-categories (if todos-ignore-archived-categories
				       (todos-truncate-categories-list)
				     (todos-make-categories-list))))
	(save-excursion (todos-category-select)))));)

;; FIXME: make core of this internal?
(defun todos-display-categories (&optional sortkey)
  "Display the category names of the current Todos file.
The numbers indicate the current order of the categories.

With non-nil SORTKEY display a non-numbered alphabetical list.
The lists are in Todos Categories mode.

The category names are buttonized, and pressing a button displays
the category in Todos mode."
  (interactive)
  (let* ((cats0 (if (and todos-ignore-archived-categories
			 (not (eq major-mode 'todos-categories-mode)))
		    (todos-make-categories-list t)
		  todos-categories))
	 (cats (todos-sort cats0 sortkey))
	 ;; used by todos-insert-category-line
	 (num 0))
    (with-current-buffer (get-buffer-create todos-categories-buffer)
      (switch-to-buffer (current-buffer))
      (let (buffer-read-only)
	(erase-buffer)
	(kill-all-local-variables)
	(insert (format "Category counts for Todos file \"%s\"."
			(file-name-sans-extension
			 (file-name-nondirectory todos-current-todos-file))))
	(newline 2)
	;; FIXME: abstract format from here and todos-insert-category-line
	(insert (make-string (+ 3 (length todos-categories-number-separator)) 32))
	(save-excursion
	  (todos-insert-sort-button todos-categories-category-label)
	  (if (member todos-current-todos-file todos-archives)
	      (insert (concat (make-string 6 32)
			      (format "%s" todos-categories-archived-label)))
	    (insert (make-string 3 32))
	    (todos-insert-sort-button todos-categories-todo-label)
	    (insert (make-string 2 32))
	    (todos-insert-sort-button todos-categories-diary-label)
	    (insert (make-string 2 32))
	    (todos-insert-sort-button todos-categories-done-label)
	    (insert (make-string 2 32))
	    (todos-insert-sort-button todos-categories-archived-label))
	  (newline 2)
	  (mapc (lambda (cat) (todos-insert-category-line cat sortkey))
		(mapcar 'car cats))))
      (todos-categories-mode))))

;; FIXME: make this toggle with todos-display-categories
(defun todos-display-categories-alphabetically ()
  ""
  (interactive)
  (todos-display-sorted 'alpha))

;; FIXME: provide key bindings for these or delete them
(defun todos-display-categories-sorted-by-todo ()
  ""
  (interactive)
  (todos-display-sorted 'todo))

(defun todos-display-categories-sorted-by-diary ()
  ""
  (interactive)
  (todos-display-sorted 'diary))

(defun todos-display-categories-sorted-by-done ()
  ""
  (interactive)
  (todos-display-sorted 'done))

(defun todos-display-categories-sorted-by-archived ()
  ""
  (interactive)
  (todos-display-sorted 'archived))

(defun todos-toggle-item-numbering ()
  ""
  (interactive)
  (todos-reset-prefix 'todos-number-prefix (not todos-number-prefix)))

(defun todos-toggle-view-done-items ()
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((todos-show-with-done
	   (if (re-search-forward todos-done-string-start nil t)
	       nil
	     t))
	  (cat (todos-current-category)))
      (todos-category-select)
      (when (zerop (todos-get-count 'done cat))
	(message "There are no done items in this category.")))))

(defun todos-toggle-show-done-only ()
  ""
  (interactive)
  (setq todos-show-done-only (not todos-show-done-only))
  (todos-category-select))

(defun todos-view-archived-items ()
  "Display the archived items of the current category.
The buffer showing these items is in Todos Archive mode."
  (interactive)
  (let ((cat (todos-current-category)))
    (if (zerop (todos-get-count 'archived cat))
	(message "There are no archived items from this category.")
      (let* ((tfile-base (file-name-sans-extension todos-current-todos-file))
	     (afile (concat tfile-base ".toda")))
	(find-file afile)
	(todos-archive-mode)
	(unless (string= todos-current-todos-file afile)
	  (setq todos-current-todos-file afile)
	  (setq todos-categories nil))
	(unless todos-categories
	  (setq todos-categories (todos-make-categories-list)))
	(setq todos-category-number
	      (- (length todos-categories)
		 (length (member cat todos-categories)))) ;FIXME
	(todos-jump-to-category cat)))))

(defun todos-switch-to-archive (&optional ask)
  "Visit the archive of the current Todos file, if it exists.
The buffer showing the archive is in Todos Archive mode. The
first visit in a session displays the first category in the
archive, subsequent visits return to the last category
displayed."
  (interactive)
  (let* ((tfile-base (file-name-sans-extension todos-current-todos-file))
	 (afile (if ask
		    (todos-read-file-name "Choose a Todos archive: " t)
		  (concat tfile-base ".toda"))))
    (if (not (file-exists-p afile))
	(message "There is currently no Todos archive for this file.")
      (find-file afile)
      (todos-archive-mode)
      (unless (string= todos-current-todos-file afile)
	(setq todos-current-todos-file afile)
	(setq todos-categories nil))
      (unless todos-categories
	(setq todos-categories (todos-make-categories-list))
	(setq todos-category-number 1))
      (todos-category-select))))

(defun todos-choose-archive ()
  "Choose an archive and visit it."
  (interactive)
  (todos-switch-to-archive t))

(defun todos-highlight-item ()
  "Highlight the todo item the cursor is on."
  (interactive)
  (if hl-line-mode ; todos-highlight-item
      (hl-line-mode 0)
    (hl-line-mode 1)))

;; FIXME: make this a customizable option for whole Todos file
(defun todos-toggle-display-date-time ()
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((ovs (overlays-in (point) (line-end-position)))
	  ov hidden)
      (while ovs
      	(setq ov (car ovs))
	(if (equal (overlay-get ov 'display) "")
	    (setq ovs nil
		  hidden t)
	  (setq ovs (cdr ovs))))
      (if hidden (remove-overlays (point-min) (point-max) 'display "")
	(while (not (eobp))
	  (re-search-forward (concat todos-date-string-start todos-date-pattern
				     "\\( " diary-time-regexp "\\)?\\]? ")
					; FIXME: this space in header? ^
			     nil t)
	  ;; FIXME: wrong match data if search fails
	  (setq ov (make-overlay (match-beginning 0) (match-end 0) nil t))
	  (overlay-put ov 'display "")
	  (forward-line))))))

(defun todos-update-merged-files ()
  ""
  (interactive)
  (let ((files (funcall todos-files-function)))
    (dolist (f files)
      (if (member f todos-merged-files)
	  (and (y-or-n-p
		(format "Remove \"%s\" from list of merged Todos files? "
			(file-name-sans-extension (file-name-nondirectory f))))
	       (setq todos-merged-files (delete f todos-merged-files)))
	(and (y-or-n-p
	      (format "Add \"%s\" to list of merged Todos files? "
		      (file-name-sans-extension (file-name-nondirectory f))))
	     (setq todos-merged-files
		   (append todos-merged-files (list f)))))))
  (customize-save-variable 'todos-merged-files todos-merged-files))

(defun todos-top-priorities (&optional num merge) ;FIXME: rename b/c of diary items
  "List top priorities for each category.

Number of entries for each category is given by NUM which
defaults to \'todos-show-priorities\'.  With non-nil argument
MERGE list top priorities of all Todos files in
`todos-merged-files'.  If `todos-prompt-merged-files' is non-nil,
prompt to update the list of merged files."
  (interactive "p")
  (or num (setq num todos-show-priorities))
  (let ((todos-print-buffer-name todos-tmp-buffer-name)
	(files (list todos-current-todos-file))
	file bufstr cat beg end done)
    (when merge
      (if (or todos-prompt-merged-files (null todos-merged-files))
	  (todos-update-merged-files))
      (setq files todos-merged-files))
    (if (buffer-live-p (get-buffer todos-print-buffer-name))
	(kill-buffer todos-print-buffer-name))
    (save-current-buffer
      (dolist (f files)
	(find-file f)
	(todos-switch-todos-file)
	(setq file (file-name-sans-extension
		    (file-name-nondirectory todos-current-todos-file)))
	(with-current-buffer (get-file-buffer f)
	  (save-restriction
	    (widen)
	    (setq bufstr (buffer-string))))
	(with-temp-buffer
	  (insert bufstr)
	  (goto-char (point-min))
	  (unless (looking-at (concat "^" (regexp-quote todos-category-beg)))
	    (kill-line 1))
	  (while (re-search-forward
		  (concat "^" (regexp-quote todos-category-beg) "\\(.+\\)\n")
		  nil t)
	    (setq cat (match-string 1))
	    (delete-region (match-beginning 0) (match-end 0))
	    (setq beg (point)) ;Start of first entry.
	    (setq end (if (re-search-forward
			   (concat "^" (regexp-quote todos-category-beg)) nil t)
			  (match-beginning 0)
			(point-max)))
	    (goto-char beg)
	    (setq done
		  (if (re-search-forward
		       (concat "\n" (regexp-quote todos-category-done)) end t)
		      (match-beginning 0)
		    end))
	    (delete-region done end)
	    (setq end done)
	    (narrow-to-region beg end)    ;In case we have too few entries.
	    (goto-char (point-min))
	    (cond ((< num 0)		; get only diary items
		   (while (not (eobp))
		     (if (looking-at (regexp-quote todos-nondiary-start))
			 (todos-remove-item)
		       (todos-forward-item))))
		  ((zerop num)		; keep all items
		   (goto-char end))
		  (t
		   (todos-forward-item num)))
	    (setq beg (point))
	    (if (>= num 0) (delete-region beg end))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (when (re-search-forward (concat todos-date-string-start
					       todos-date-pattern
					       "\\( " diary-time-regexp "\\)?\\]?")
				       nil t)
		(insert (concat " [" (if merge (concat file ":")) cat "]")))
	      (forward-line))
	    (widen))
	  (append-to-buffer todos-print-buffer-name (point-min) (point-max)))))
    (with-current-buffer todos-print-buffer-name
      (todos-prefix-overlays)
      (todos-top-priorities-mode)
      (goto-char (point-min))         ;Due to display buffer
      ;; (make-local-variable 'font-lock-defaults)
      ;; (setq font-lock-defaults '(todos-font-lock-keywords t))
      (font-lock-fontify-buffer))
    ;; (setq buffer-read-only t))
    ;; Could have used switch-to-buffer as it has a norecord argument,
    ;; which is nice when we are called from e.g. todos-print.
    ;; Else we could have used pop-to-buffer.
    (display-buffer todos-print-buffer-name)
    (message "Type C-x 1 to remove %s window.  M-C-v to scroll the help."
             todos-print-buffer-name)))

(defun todos-merged-top-priorities (&optional num)
  ""
  (interactive "p")
  (todos-top-priorities num t))

(defun todos-diary-items (&optional merge)
  "Display todo items marked for diary inclusion.
The items are those in the current Todos file, or with prefix
argument MERGE those in all Todos files in `todos-merged-files'."
  (interactive "P")
  (todos-top-priorities -1 merge))

;;; Navigation

(defun todos-forward-category ()
  "Go forward to TODO list of next category."
  (interactive)
  (setq todos-category-number
        (1+ (mod todos-category-number (length todos-categories))))
  (todos-category-select)
  (goto-char (point-min)))

(defun todos-backward-category ()
  "Go back to TODO list of previous category."
  (interactive)
  (setq todos-category-number
	(1+ (mod (- todos-category-number 2) (length todos-categories))))
  (todos-category-select)
  (goto-char (point-min)))

;; FIXME: Document that a non-existing name creates that category, and add
;; y-or-n-p confirmation -- or eliminate this possibility?
(defun todos-jump-to-category (&optional cat other-file)
  "Jump to a category in a Todos file.
When called interactively, prompt for the category.
Non-interactively, the argument CAT provides the category.  With
non-nil argument OTHER-FILE, prompt for a Todos file, otherwise
stay with the current Todos file.  See also
`todos-jump-to-category-other-file'."
  (interactive)
  (when (or (and other-file
		 (setq todos-current-todos-file
		       (todos-read-file-name "Choose a Todos file: ")))
	    (and cat
		 todos-ignore-archived-categories
		 (zerop (todos-get-count 'todo cat))
		 (zerop (todos-get-count 'done cat))
		 (not (zerop (todos-get-count 'archived cat)))
		 (setq todos-current-todos-file
		       (concat (file-name-sans-extension todos-current-todos-file)
			       ".toda"))))
    (with-current-buffer (find-file-noselect todos-current-todos-file)
      ;; (or (eq major-mode 'todos-mode) (todos-mode))
      (setq todos-categories (todos-make-categories-list))))
  (let ((category (or (and (assoc cat todos-categories) cat)
		      (todos-read-category "Jump to category: "))))
    (if (string= "" category)
        (setq category (todos-current-category)))
    (if (string= (buffer-name) todos-categories-buffer)
	(kill-buffer))
    (if (or cat other-file)
	(switch-to-buffer (get-file-buffer todos-current-todos-file)))
    (setq todos-category-number
	  (or (todos-category-number category)
	      (todos-add-category category)))
    (todos-category-select)
    (goto-char (point-min))))

(defun todos-jump-to-category-other-file ()
  ""
  (interactive)
  (todos-jump-to-category nil t))

;; FIXME ? todos-{backward,forward}-item skip over empty line between done and
;; not done items (but todos-forward-item gets there when done items are not
;; displayed).  Also disallow prefix arg value < 1 (re-search-* allows these)
(defun todos-backward-item (&optional count)
  "Select COUNT-th previous entry of TODO list."
  (interactive "P")
  ;; FIXME ? this moves to bob if on the first item (but so does previous-line)
  (todos-item-start)
  (unless (bobp)
    (re-search-backward todos-item-start nil t (or count 1))))

(defun todos-forward-item (&optional count)
  "Select COUNT-th next entry of TODO list."
  (interactive "P")
  (goto-char (line-end-position))
  (if (re-search-forward todos-item-start nil t (or count 1))
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun todos-search ()
  "Perform a search for a regular expression, with repetition.
The search encompasses all todo and done items within the current Todos file; it excludes category names.  Matches are highlighted
"
  (interactive)
  (let ((regex (read-from-minibuffer "Enter a search string (regexp): "))
	(opoint (point))
	matches match cat in-done ov mlen msg)
    (widen)
    (goto-char (point-min))
    (while (not (eobp))
      (setq match (re-search-forward regex nil t))
      (goto-char (line-beginning-position))
      (unless (or (equal (point) 1)
		  (looking-at (concat "^" (regexp-quote todos-category-beg))))
	(if match (push match matches)))
      (forward-line))
    (setq matches (reverse matches))
    (if matches
	(catch 'stop
	  (while matches
	    (setq match (pop matches))
	    (goto-char match)
	    (todos-item-start)
	    (when (looking-at todos-done-string-start)
	      (setq in-done t))
	    (re-search-backward (concat "^" (regexp-quote todos-category-beg)
					"\\(.*\\)\n") nil t)
	    (setq cat (match-string-no-properties 1))
	    (todos-category-number cat)
	    (todos-category-select)
	    (if in-done (unless todos-show-with-done (todos-toggle-view-done-items)))
	    (goto-char match)
	    (setq ov (make-overlay (- (point) (length regex)) (point)))
	    (overlay-put ov 'face 'todos-search)
	    (when matches
	      (setq mlen (length matches))
	      (if (y-or-n-p
		   (if (> mlen 1)
		       (format "There are %d more matches; go to next match? " mlen)
		     "There is one more match; go to it? "))
		  (widen)
		(throw 'stop (setq msg (if (> mlen 1)
					   (format "There are %d more matches." mlen)
					 "There is one more match."))))))
	  (setq msg "There are no more matches."))
      (todos-category-select)
      (goto-char opoint)
      (message "No match for \"%s\"" regex))
    (when msg
      (if (y-or-n-p (concat msg "\nUnhighlight matches? "))
	  (todos-clear-matches)
	(message "You can unhighlight the matches later by typing %s"
		 (key-description (car (where-is-internal
					'todos-clear-matches))))))))

(defun todos-clear-matches ()
  "Removing highlighting on matches found by todos-search."
  (interactive)
  (remove-overlays 1 (1+ (buffer-size)) 'face 'todos-search))

;;; Editing

(defun todos-add-file (&optional arg)
  ""
  (interactive "p")
  (let ((default-file (if todos-default-todos-file
			  (file-name-sans-extension
			   (file-name-nondirectory todos-default-todos-file))))
	file prompt)
    (while
	(and
	 (cond
	  ((or (not file) (member file todos-files))
	   (setq prompt (concat "Enter name of new Todos file "
				"(TAB or SPC to see existing Todos files): ")))
	  ((string-equal file "")
	   (setq prompt "Enter a non-empty name: "))
	  ((string-match "\\`\\s-+\\'" file)
	   (setq prompt "Enter a name that is not only white space: ")))
	 (setq file (todos-read-file-name prompt))))
    (if (or (not default-file)
	    (yes-or-no-p (concat "Make %s new default Todos file "
				 "[current default is \"%s\"]? ")
			 file default-file))
	(todos-change-default-file file)
      (message "\"%s\" remains the default Todos file." default-file))
    (with-current-buffer (get-buffer-create todos-default-todos-file)
      (erase-buffer)
      (write-region (point-min) (point-max) todos-default-todos-file
		    nil 'nomessage nil t))
    (if arg (todos-show) file)))

;; FIXME: omit this and just use defcustom?
(defun todos-change-default-file (&optional file)
  ""
  (interactive)
  (let ((new-default (or file
			 (todos-read-file-name "Choose new default Todos file: "))))
    (customize-save-variable 'todos-default-todos-file new-default)
    (message "\"%s\" is new default Todos file."
	     (file-name-sans-extension (file-name-nondirectory new-default)))))

(defun todos-add-category (&optional cat)
  "Add new category CAT to the TODO list."
  (interactive)
  (let* ((buffer-read-only)
	 (buf (find-file-noselect todos-current-todos-file t))
	 (num (1+ (length todos-categories)))
	 (counts (make-vector 4 0)))	; [todo diary done archived]
	 ;; (counts (list 'todo 0 'diary 0 'done 0 'archived 0)))
    (unless (zerop (buffer-size buf))
      (and (null todos-categories)
	   (error "Error in %s: File is non-empty but contains no category"
		  todos-current-todos-file)))
    (unless cat (setq cat (read-from-minibuffer "Enter new category name: ")))
    (with-current-buffer buf
      (setq cat (todos-validate-category-name cat))
      (setq todos-categories (append todos-categories (list (cons cat counts))))
      (widen)
      (goto-char (point-max))
      (save-excursion			; for subsequent todos-category-select
	(insert todos-category-beg cat "\n\n" todos-category-done "\n"))
      (todos-update-categories-sexp)
      (if (called-interactively-p 'any)	  ; FIXME
	  ;; properly display the newly added category
	  (progn
	    (setq todos-category-number num)
	    (todos-category-select))
	num))))

(defun todos-rename-category ()
  "Rename current Todos category."
  (interactive)
  (let* ((cat (todos-current-category))
	 (new (read-from-minibuffer (format "Rename category \"%s\" to: " cat))))
    (setq new (todos-validate-category-name new))
    (let* ((ofile (buffer-file-name))
	   (archive (concat (file-name-sans-extension ofile) ".toda"))
	   (buffers (append (list ofile)
			    (unless (zerop (todos-get-count 'archived cat))
			      (list archive)))))
      (dolist (buf buffers)
	(with-current-buffer (find-file-noselect buf)
	  (let (buffer-read-only)
	    ;; (setq todos-categories (if (string= buf archive)
	    ;; 			       (todos-make-categories-list t)
	    ;; 			     todos-categories))
	    (todos-set-categories)
	    (save-excursion
	      (save-restriction
		(setcar (assoc cat todos-categories) new)
		(widen)
		(goto-char (point-min))
		(todos-update-categories-sexp)
		(re-search-forward (concat (regexp-quote todos-category-beg) "\\("
					   (regexp-quote cat) "\\)\n") nil t)
		(replace-match new t t nil 1)))))))
    (setq mode-line-buffer-identification
	  (format "Category %d: %s" todos-category-number new)))
  (save-excursion (todos-category-select)))

;; FIXME: what if cat has archived items?
(defun todos-delete-category (&optional arg)
  "Delete current Todos category provided it is empty.
With ARG non-nil delete the category unconditionally,
i.e. including all existing entries."
  (interactive "P")
  (let* ((cat (todos-current-category))
	 (todo (todos-get-count 'todo cat))
	 (done (todos-get-count 'done cat)))
    (if (and (not arg)
	     (or (> todo 0) (> done 0)))
	(message "To delete a non-empty category, type C-u D.")
      (when (y-or-n-p (concat "Permanently remove category \"" cat
			      "\"" (and arg " and all its entries") "? "))
	(widen)
	(let ((buffer-read-only)
	      (beg (re-search-backward
		    (concat "^" (regexp-quote (concat todos-category-beg cat))
			    "\n") nil t))
	      (end (if (re-search-forward
			(concat "\n\\(" (regexp-quote todos-category-beg)
				".*\n\\)") nil t)
		       (match-beginning 1)
		     (point-max))))
	  (remove-overlays beg end)
	  (delete-region beg end)
	  (setq todos-categories (delete (assoc cat todos-categories)
					 todos-categories))
	  (todos-update-categories-sexp)
	  (setq todos-category-number
		(1+ (mod todos-category-number (length todos-categories))))
	  (todos-category-select)
	  (goto-char (point-min))
	  (message "Deleted category %s" cat))))))

(defun todos-raise-category (&optional lower)
  "Raise priority of category point is on in Categories buffer.
With non-nil argument LOWER, lower the category's priority."
  (interactive)
  (let (num)
    (save-excursion
      (forward-line 0)
      (skip-chars-forward " ")
      (setq num (number-at-point)))
    (when (and num (if lower
		       (< num (length todos-categories))
		     (> num 1)))
      (let* ((col (current-column))
	     (beg (progn (forward-line (if lower 0 -1)) (point)))
	     (num1 (progn (skip-chars-forward " ") (1- (number-at-point))))
	     (num2 (1+ num1))
	     (end (progn (forward-line 2) (point)))
	     (catvec (vconcat todos-categories))
	     (cat1-list (aref catvec num1))
	     (cat2-list (aref catvec num2))
	     (cat1 (car cat1-list))
	     (cat2 (car cat2-list))
	     (buffer-read-only))
	(delete-region beg end)
	(setq num1 (1+ num1))
	(setq num2 (1- num2))
	(setq num num2)
	(todos-insert-category-line cat2)
	(setq num num1)
	(todos-insert-category-line cat1)
	(aset catvec num2 (cons cat2 (cdr cat2-list)))
	(aset catvec num1 (cons cat1 (cdr cat1-list)))
	(setq todos-categories (append catvec nil))
	(with-current-buffer (get-file-buffer todos-current-todos-file)
	  (todos-update-categories-sexp))
	(forward-line (if lower -1 -2))
	(forward-char col)))))

(defun todos-lower-category ()
  "Lower priority of category point is on in Categories buffer."
  (interactive)
  (todos-raise-category t))

;; FIXME: use save-restriction?
(defun todos-move-category ()
  "Move current category to a different Todos file.
If current category has archived items, also move those to the
archive of the file moved to, creating it if it does not exist."
  (interactive)
  ;; FIXME: warn if only category in file?  If so, delete file after moving category
  (when (or (> (length todos-categories) 1)
	    (y-or-n-p (concat "This is the only category in this file; "
			      "moving it will delete the file.\n"
			      "Do you want to proceed? ")))
    (let* ((ofile (buffer-file-name))
	   (cat (todos-current-category))
	   ;; FIXME: check if cat exists in nfile, and if so rename it
	   (nfile (todos-read-file-name "Choose a Todos file: "))
	   (archive (concat (file-name-sans-extension ofile) ".toda"))
	   (buffers (append (list ofile)
			    (unless (zerop (todos-get-count 'archived cat))
			      (list archive)))))
      (dolist (buf buffers)
	(with-current-buffer (find-file-noselect buf)
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char (point-max))
	      (let ((buffer-read-only nil)
		    (beg (re-search-backward
			  (concat "^"
				  (regexp-quote (concat todos-category-beg cat)))
			  nil t))
		    (end (if (re-search-forward
			      (concat "^" (regexp-quote todos-category-beg))
			      nil t 2)
			     (match-beginning 0)
			   (point-max)))
		    (content (buffer-substring-no-properties beg end)))
		(with-current-buffer
		    (find-file-noselect
		     ;; regenerate todos-archives in case there
		     ;; is a newly created archive
		     (if (member buf (funcall todos-files-function t))
			 (concat (file-name-sans-extension nfile) ".toda")
		       nfile))
		  (let (buffer-read-only)
		    (save-excursion
		      (save-restriction
			(widen)
			(goto-char (point-max))
			(insert content)
			(goto-char (point-min))
			(if (zerop (buffer-size))
			    (progn
			      (set-buffer-modified-p nil) ; no questions
			      (delete-file (buffer-file-name))
			      (kill-buffer))
			  (unless (looking-at
				   (concat "^" (regexp-quote todos-category-beg)))
			    (kill-whole-line))
			  (save-buffer)))))
		  (remove-overlays beg end)
		  (delete-region beg end)
		  (goto-char (point-min))
		  (if (zerop (buffer-size))
		      (progn
			(set-buffer-modified-p nil)
			(delete-file (buffer-file-name))
			(kill-buffer))
		    (unless (looking-at
			     (concat "^" (regexp-quote todos-category-beg)))
		      (kill-whole-line))
		    (save-buffer))))))))
      ;; (todos-switch-todos-file nfile))))
      (find-file nfile)
      (setq todos-current-todos-file nfile
      	    todos-categories (todos-make-categories-list t)
      	    todos-category-number (todos-category-number cat))
      (todos-category-select))))

(defun todos-merge-category ()
  "Merge this category's items to another category in this file.
The todo and done items are appended to the todo and done items,
respectively, of the category merged to, which becomes the
current category, and the category merged from is deleted."
  (interactive)
  (let ((buffer-read-only nil)
	(cat (todos-current-category))
	(goal (todos-read-category "Category to merge to: ")))
    (widen)
    ;; FIXME: what if cat has archived items?
    (let* ((cbeg (progn
		   (re-search-backward
		    (concat "^" (regexp-quote todos-category-beg)) nil t)
		   (point)))
	   (tbeg (progn (forward-line) (point)))
	   (dbeg (progn
		   (re-search-forward
		    (concat "^" (regexp-quote todos-category-done)) nil t)
		   (match-beginning 0)))
	   (tend (forward-line -1))
	   (cend (progn
		   (if (re-search-forward
			(concat "^" (regexp-quote todos-category-beg)) nil t)
		       (match-beginning 0)
		     (point-max))))
	   (todo (buffer-substring-no-properties tbeg tend))
	   (done (buffer-substring-no-properties dbeg cend))
	   here)
      (goto-char (point-min))
      (re-search-forward
       (concat "^" (regexp-quote todos-category-beg goal)) nil t)
      (re-search-forward
       (concat "^" (regexp-quote todos-category-done)) nil t)
      (forward-line -1)
      (setq here (point))
      (insert todo)
      (goto-char (if (re-search-forward
		      (concat "^" (regexp-quote todos-category-beg)) nil t)
		     (match-beginning 0)
		   (point-max)))
      (insert done)
      (remove-overlays cbeg cend)
      (delete-region cbeg cend)
      (setq todos-categories (delete (assoc cat todos-categories)
				     todos-categories))
      (todos-update-categories-sexp)
      (setq todos-category-number (todos-category-number goal))
      (todos-category-select)
      ;; Put point at the start of the merged todo items
      ;; FIXME: what if there are no merged todo items but only done items?
      (goto-char here))))
      
(defun todos-merge-categories ()
  ""
  (interactive)
  (let* ((cats (mapcar 'car todos-categories))
	 (goal (todos-read-category "Category to merge to: "))
	 (prompt (format "Merge to %s (type C-g to finish)? " goal))
	 (source (let ((inhibit-quit t) l)
		  (while (not (eq last-input-event 7))
		    (dolist (c cats)
		      (when (y-or-n-p prompt)
			(push c l)
			(setq cats (delete c cats))))))))
    (widen)
  ))

;;;###autoload
(defun todos-insert-item (&optional arg date-type time diary here)
  "Insert new TODO list item.

With prefix argument ARG solicit the category, otherwise use the
current category.

Argument DATE-TYPE sets the form of the item's mandatory date
string.  With the value `date' this is the full date (whose
format is set by `calendar-date-display-form', with year, month
and day individually solicited (month with tab completion).  With
the value `dayname' a weekday name is used, solicited with tab
completion.  With the value `calendar' the full date string is
used and set by selecting from the Calendar.  With any other
value (including none) the full current date is used.

Argument TIME determines the occurrence and value of the time
string.  With the value `omit' insert the item without a time
string.  With the value `ask' solicit a time string; this may be
empty or else must match `date-time-regexp'.  With any other
value add or omit the current time in accordance with
`todos-always-add-time-string'.

With non-nil argument DIARY mark item for inclusion in user's diary.  If `todos-include-in-diary' is non-nil

With non-nil argument HERE insert the new item directly above the
item at point.  If point is on an empty line, insert the new item
there."
  (interactive "P")
  (unless (or (todos-done-item-p)
	      (save-excursion (forward-line -1) (todos-done-item-p)))
    ;; FIXME: deletable if command not autoloaded
    (when (not (derived-mode-p 'todos-mode)) (todos-show))
    (let* ((buffer-read-only)
	   (date-string (cond
			 ((eq date-type 'ask-date)
			  (todos-read-date))
			 ((eq date-type 'ask-dayname)
			  (todos-read-dayname))
			 ((eq date-type 'calendar)
			  ;; FIXME: should only be executed from Calendar
			  (with-current-buffer "*Calendar*"
			    (calendar-date-string (calendar-cursor-to-date t) t t)))
			 (t (calendar-date-string (calendar-current-date) t t))))
	   (time-string (cond ((eq time 'ask-time)
			       (todos-read-time))
			      (todos-always-add-time-string
			       (substring (current-time-string) 11 16))
			      (t nil)))
	   (new-item (concat (unless (or diary todos-include-in-diary)
			       todos-nondiary-start)
			     date-string (when time-string (concat " " time-string))
			     (unless (or diary todos-include-in-diary)
			       todos-nondiary-end)
			     " "
			     (read-from-minibuffer "New TODO entry: ")))
	   (cat (if arg (todos-read-category "Insert item in category: ")
		  (todos-current-category))))
      ;; indent newlines inserted by C-q C-j if nonspace char follows
      (setq new-item (replace-regexp-in-string
		      "\\(\n\\)[^[:blank:]]"
		      (concat "\n" (make-string todos-indent-to-here 32)) new-item
		      nil nil 1))
      (unless (assoc cat todos-categories) (todos-add-category cat))
      ;; (unless here (todos-set-item-priority new-item cat))
      ;; (todos-insert-with-overlays new-item)
      (if here
	  (todos-insert-with-overlays new-item)
	(todos-set-item-priority new-item cat))
      (todos-item-counts cat 'insert)
      (if (or diary todos-include-in-diary) (todos-item-counts cat 'diary))
      (todos-update-categories-sexp))))

;; FIXME: make insertion options customizable per category

;; current date ~ current day ~ ask date ~ ask day
;; current time ~ ask time ~ maybe no time
;; for diary ~ not for diary
;; here ~ ask priority

;; date-type: date name (calendar) - (maybe-no)time - diary - here

;; ii     todos-insert-item + current-date/dayname + current/no-time
;; ih     todos-insert-item-here
;; idd    todos-insert-item-ask-date
;; idtt   todos-insert-item-ask-date-time
;; idtyy  todos-insert-item-ask-date-time-for-diary
;; idtyh  todos-insert-item-ask-date-time-for-diary-here
;; idth   todos-insert-item-ask-date-time-here
;; idmm   todos-insert-item-ask-date-maybe-notime
;; idmyy  todos-insert-item-ask-date-maybe-notime-for-diary
;; idmyh  todos-insert-item-ask-date-maybe-notime-for-diary-here
;; idmh   todos-insert-item-ask-date-maybe-notime-here
;; idyy   todos-insert-item-ask-date-for-diary
;; idyh   todos-insert-item-ask-date-for-diary-here
;; idh    todos-insert-item-ask-date-here
;; inn    todos-insert-item-ask-dayname
;; intt   todos-insert-item-ask-dayname-time
;; intyy  todos-insert-item-ask-dayname-time-for-diary
;; intyh  todos-insert-item-ask-dayname-time-for-diary-here
;; inth   todos-insert-item-ask-dayname-time-here
;; inmm   todos-insert-item-ask-dayname-maybe-notime
;; inmyy  todos-insert-item-ask-dayname-maybe-notime-for-diary
;; inmyh  todos-insert-item-ask-dayname-maybe-notime-for-diary-here
;; inmh   todos-insert-item-ask-dayname-maybe-notime-here
;; inyy   todos-insert-item-ask-dayname-for-diary
;; inyh   todos-insert-item-ask-dayname-for-diary-here
;; inh    todos-insert-item-ask-dayname-here
;; itt    todos-insert-item-ask-time
;; ityy   todos-insert-item-ask-time-for-diary
;; ityh   todos-insert-item-ask-time-for-diary-here
;; ith    todos-insert-item-ask-time-here
;; im     todos-insert-item-maybe-notime
;; imyy   todos-insert-item-maybe-notime-for-diary
;; imyh   todos-insert-item-maybe-notime-for-diary-here
;; imh    todos-insert-item-maybe-notime-here
;; iyy    todos-insert-item-for-diary
;; iyh    todos-insert-item-for-diary-here

(defun todos-insert-item-ask-date (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-date))

(defun todos-insert-item-ask-date-time (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-date 'ask-time))

(defun todos-insert-item-ask-date-time-for-diary (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-date 'ask-time t))

(defun todos-insert-item-ask-date-time-for-diary-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-date 'ask-time t t))

(defun todos-insert-item-ask-date-time-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-date 'ask-time nil t))

(defun todos-insert-item-ask-date-maybe-notime (&optional arg)
  ""
  (interactive "P")
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item arg 'ask-date)))

(defun todos-insert-item-ask-date-maybe-notime-for-diary (&optional arg)
  ""
  (interactive "P")
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item arg 'ask-date nil t)))

(defun todos-insert-item-ask-date-maybe-notime-for-diary-here ()
  ""
  (interactive)
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item nil 'ask-date nil t t)))

(defun todos-insert-item-ask-date-maybe-notime-here ()
  ""
  (interactive)
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item nil 'ask-date nil nil nil t)))

(defun todos-insert-item-ask-date-for-diary (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-date nil t))

(defun todos-insert-item-ask-date-for-diary-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-date nil t t))

(defun todos-insert-item-ask-date-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-date nil nil t))

(defun todos-insert-item-ask-dayname (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-dayname))

(defun todos-insert-item-ask-dayname-time (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-dayname 'ask-time))

(defun todos-insert-item-ask-dayname-time-for-diary (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-dayname 'ask-time t))

(defun todos-insert-item-ask-dayname-time-for-diary-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-dayname 'ask-time t t))

(defun todos-insert-item-ask-dayname-time-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-dayname 'ask-time nil t))

(defun todos-insert-item-ask-dayname-maybe-notime (&optional arg)
  ""
  (interactive "P")
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item arg 'ask-dayname)))

(defun todos-insert-item-ask-dayname-maybe-notime-for-diary (&optional arg)
  ""
  (interactive "P")
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item arg 'ask-dayname nil t)))

(defun todos-insert-item-ask-dayname-maybe-notime-for-diary-here ()
  ""
  (interactive)
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item nil 'ask-dayname nil t t)))

(defun todos-insert-item-ask-dayname-maybe-notime-here ()
  ""
  (interactive)
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item nil 'ask-dayname nil nil t)))

(defun todos-insert-item-ask-dayname-for-diary (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-dayname nil t))

(defun todos-insert-item-ask-dayname-for-diary-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-dayname nil t t))

(defun todos-insert-item-ask-dayname-here ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-dayname nil nil t))

(defun todos-insert-item-ask-time (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg nil 'ask-time))

(defun todos-insert-item-ask-time-for-diary (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg nil 'ask-time t))

(defun todos-insert-item-ask-time-for-diary-here ()
  ""
  (interactive)
  (todos-insert-item nil nil 'ask-time t t))

(defun todos-insert-item-ask-time-here ()
  ""
  (interactive)
  (todos-insert-item nil nil 'ask-time nil t))

(defun todos-insert-item-maybe-notime (&optional arg)
  ""
  (interactive "P")
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item arg)))

(defun todos-insert-item-maybe-notime-for-diary (&optional arg)
  ""
  (interactive "P")
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item arg nil nil t)))

(defun todos-insert-item-maybe-notime-for-diary-here ()
  ""
  (interactive)
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item nil nil nil t t)))

(defun todos-insert-item-maybe-notime-here ()
  ""
  (interactive)
  (let ((todos-always-add-time-string (not todos-always-add-time-string)))
    (todos-insert-item nil nil nil nil t)))

(defun todos-insert-item-for-diary (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item nil nil nil t))

(defun todos-insert-item-for-diary-here ()
  ""
  (interactive)
  (todos-insert-item nil nil nil t t))

(defun todos-insert-item-here ()
  "Insert new Todo item directly above the item at point.
If point is on an empty line, insert the new item there."
  (interactive)
  (todos-insert-item nil nil nil nil t))

;; FIXME: autoload when key-binding is defined in calendar.el
(defun todos-insert-item-from-calendar ()
  ""
  (interactive)
  (pop-to-buffer (file-name-nondirectory todos-current-todos-file))
  (todos-show)
  (todos-insert-item t 'calendar))

;; FIXME: calendar is loaded before todos
;; (add-hook 'calendar-load-hook
	  ;; (lambda ()
	    (define-key calendar-mode-map "it" 'todos-insert-item-from-calendar);))

(defun todos-delete-item ()
  "Delete current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((buffer-read-only)
	     (item (todos-item-string-start))
	     (diary-item (todos-diary-item-p))
	     (cat (todos-current-category))
             (answer (y-or-n-p (concat "Permanently remove '" item "'? "))))
        (when answer
          (todos-remove-item)
          (when (and (bolp) (eolp)
		     ;; not if last item was deleted
		     (< (point-min) (point-max)))
	    (todos-backward-item))
	  (todos-item-counts cat 'delete)
	  (and diary-item (todos-item-counts cat 'nondiary))
	  (todos-update-categories-sexp)
	  (todos-prefix-overlays)))
    (message "No TODO list entry to delete"))) ;FIXME: better message

(defun todos-edit-item ()
  "Edit current TODO list entry."
  (interactive)
  (when (todos-item-string)
    (let* ((buffer-read-only)
	   (start (todos-item-start))
	   (item-beg (progn
		       (re-search-forward
			(concat todos-date-string-start todos-date-pattern
				"\\( " diary-time-regexp "\\)?"
				(regexp-quote todos-nondiary-end) "?")
			(line-end-position) t)
		       (1+ (- (point) start))))
	   (item (todos-item-string))
	   (opoint (point)))
      (if (todos-string-multiline-p item)
	  (todos-edit-multiline)
	(let ((new (read-string "Edit: " (cons item item-beg))))
	  (while (not (string-match (concat todos-date-string-start
					    todos-date-pattern) new))
	    (setq new (read-from-minibuffer "Item must start with a date: " new)))
	  ;; indent newlines inserted by C-q C-j if nonspace char follows
	  (setq new (replace-regexp-in-string
		     "\\(\n\\)[^[:blank:]]"
		     (concat "\n" (make-string todos-indent-to-here 32)) new
		     nil nil 1))
	  ;; If user moved point during editing, make sure it moves back.
	  (goto-char opoint)
	  (todos-remove-item)
	  (todos-insert-with-overlays new)
	  (move-to-column item-beg))))))
  
;; FIXME: run todos-check-format on exiting buffer (or check for date string
;; and indentation)
(defun todos-edit-multiline ()
  "Set up a buffer for editing a multiline TODO list entry."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name todos-edit-buffer)))
    (switch-to-buffer
     (make-indirect-buffer
      (file-name-nondirectory todos-current-todos-file) buffer-name))
    (narrow-to-region (todos-item-start) (todos-item-end))
    (todos-edit-mode)
    (message "Type %s to return to Todos mode."
	     (key-description (car (where-is-internal 'todos-edit-quit))))))

(defun todos-edit-quit ()
  ""
  (interactive)
  (todos-save)
  ;; (unlock-buffer)
  (kill-buffer)
  (save-excursion (todos-category-select)))

(defun todos-edit-item-header (&optional part)
  ""
  (interactive)
  (todos-item-start)
  (re-search-forward (concat todos-date-string-start "\\(?1:" todos-date-pattern
  			     "\\)\\(?2: " diary-time-regexp "\\)?")
		     (line-end-position) t)
  (let* ((odate (match-string-no-properties 1))
	 (otime (match-string-no-properties 2))
	 (buffer-read-only)
	 ndate ntime nheader)
    (unless (eq part 'timeonly)
      (setq ndate (if (save-match-data (string-match "[0-9]+" odate))
		      (if (y-or-n-p "Change date? ")
			  (todos-read-date)
			(todos-read-dayname))
		    (if (y-or-n-p "Change day? ")
			(todos-read-dayname)
		      (todos-read-date))))
      (replace-match ndate nil nil nil 1))
    (unless (eq part 'dateonly)
      (setq ntime (save-match-data (todos-read-time)))
      (when (< 0 (length ntime)) (setq ntime (concat " " ntime)))
      (if otime
	  (replace-match ntime nil nil nil 2)
	(goto-char (match-end 1))
	(insert ntime)))))

(defun todos-edit-item-date ()
  ""
  (interactive)
  (todos-edit-item-header 'dateonly))

(defun todos-edit-item-date-is-today ()
  ""
  (interactive)
  (todos-edit-item-header 'today))
 
(defun todos-edit-item-time ()
  ""
  (interactive)
  (todos-edit-item-header 'timeonly))

;; (progn
;;   (re-search-forward "\\(?1:foo\\)\\(ba\\)\\(?2:z\\)?" nil t)
;;   (goto-char (point-max))
;;   (concat (match-string-no-properties 1) ", " (match-string-no-properties 2)))

;; foobaz


(defun todos-raise-item-priority ()
  "Raise priority of current entry."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let (buffer-read-only)
      (if (> (count-lines (point-min) (point)) 0)
	  (let ((item (todos-item-string)))
	    (when (eq major-mode 'todos-top-priorities-mode)
	      (let ((cat1 (save-excursion
			    (re-search-forward
			     (concat todos-date-string-start todos-date-pattern
				     "\\( " diary-time-regexp
				     "\\)?\\]?\\(?1: \\[\\(.+:\\)?.+\\]\\)")
			     nil t)
			    (match-string 1)))
		    (cat2 (save-excursion
			    (todos-backward-item)
			    (re-search-forward
			     (concat todos-date-string-start todos-date-pattern
				     "\\( " diary-time-regexp
				     "\\)?\\]?\\(?1: \\[\\(.+:\\)?.+\\]\\)")
			     nil t)
			    (match-string 1))))
		(if (string= cat1 cat2)
		    (error "Cannot change item's priority in its category; do this in Todos mode"))))
	    (todos-remove-item)
	    (todos-backward-item)
	    (todos-insert-with-overlays item))
	(message "No TODO list entry to raise"))))) ;FIXME: better message

(defun todos-lower-item-priority ()
  "Lower priority of current entry."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let (buffer-read-only)
      (if (save-excursion
	    ;; can only lower non-final unfinished item
	    (todos-forward-item)
	    (and (looking-at todos-item-start)
		 (not (todos-done-item-p))))
	  ;; Assume there is a final newline
	  (let ((item (todos-item-string)))
	    (when (eq major-mode 'todos-top-priorities-mode)
	      (let ((cat1 (save-excursion
			    (re-search-forward
			     (concat todos-date-string-start todos-date-pattern
				     "\\( " diary-time-regexp
				     "\\)?\\]?\\(?1: \\[\\(.+:\\)?.+\\]\\)")
			     nil t)
			    (match-string 1)))
		    (cat2 (save-excursion
			    (todos-forward-item)
			    (re-search-forward
			     (concat todos-date-string-start todos-date-pattern
				     "\\( " diary-time-regexp
				     "\\)?\\]?\\(?1: \\[\\(.+:\\)?.+\\]\\)")
			     nil t)
			    (match-string 1))))
		(if (string= cat1 cat2)
		    (error "Cannot change item's priority in its category; do this in Todos mode"))))
	    (todos-remove-item)
	    (todos-forward-item)
	    (when (todos-done-item-p) (forward-line -1))
	    (todos-insert-with-overlays item))
	(message "No TODO list entry to lower"))))) ;FIXME: better message

(defun todos-set-item-priority (item cat)
  "Set priority of todo ITEM in category CAT and move item to suit."
  (interactive (list (todos-item-string) (todos-current-category)))
  (unless (called-interactively-p t)
    (todos-category-number cat)
    (todos-category-select))
  (let* ((todo (todos-get-count 'todo cat))
	 (maxnum (1+ todo))
	 (buffer-read-only)
	 priority candidate prompt)
    (unless (zerop todo)
      (while (not priority)
	(setq candidate
	      (string-to-number (read-from-minibuffer
				 (concat prompt
					 (format "Set item priority (1-%d): "
						 maxnum)))))
	(setq prompt
	      (when (or (< candidate 1) (> candidate maxnum))
		(format "Priority must be an integer between 1 and %d.\n" maxnum)))
	(unless prompt (setq priority candidate)))
      ;; interactively, just relocate the item within its category
      (when (called-interactively-p) (todos-remove-item))
      (goto-char (point-min))
      (unless (= priority 1) (todos-forward-item (1- priority))))
    (todos-insert-with-overlays item)))

;; (defun todos-set-item-top-priority ()
;;   "Set priority of item at point in the top priorities listing."
;;   (interactive)
;;   (let* ((item (todos-item-string))
;; 	 (cat (save-excursion
;; 		(re-search-forward
;; 		 (concat todos-date-string-start todos-date-pattern
;; 			 "\\( " diary-time-regexp
;; 			 "\\)?\\]?\\(?1: \\[\\(.+:\\)?.+\\]\\)")
;; 		 nil t)
;; 		(match-string 1)))
;; 	 (opoint (point))
;; 	 (count 1)
;; 	 (old-priority (save-excursion
;; 			 (goto-char (point-min))
;; 			 (while (< (point) opoint)
;; 			   (todos-forward-item)
;; 			   (setq count (1+ count))))))
;;     )
	  
(defun todos-move-item (&optional file)
  "Move the current todo item to another, interactively named, category.

If the named category is not one of the current todo categories,
then it is created and the item becomes the first entry in that
category.

With optional non-nil argument FILE, first ask for another Todos
file and then solicit a category within that file to move the
item to."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let ((buffer-read-only)
	  (modified (buffer-modified-p))
	  (oldfile todos-current-todos-file)
	  (oldnum todos-category-number)
	  (oldcat (todos-current-category))
	  (item (todos-item-string))
	  (diary-item (todos-diary-item-p))
	  (newfile (if file (todos-read-file-name "Choose a Todos file: ")))
	  (opoint (point))
	  (orig-mrk (progn (todos-item-start) (point-marker)))
	  newcat moved)
      (unwind-protect
	  (progn
	    (todos-remove-item)
	    (todos-item-counts oldcat 'delete)
	    (and diary-item (todos-item-counts oldcat 'nondiary))
	    (when newfile
	      (find-file-existing newfile)
	      (setq todos-current-todos-file newfile
		    todos-categories (todos-make-categories-list)))
	    (setq newcat (todos-read-category "Move item to category: "))
	    (unless (assoc newcat todos-categories) (todos-add-category newcat))
	    (todos-set-item-priority item newcat)	
	    (setq moved t)
	    (todos-item-counts newcat 'insert)
	    (and diary-item (todos-item-counts newcat 'diary)))
	(unless moved
	  (if newfile
	      (find-file-existing oldfile)
	    (setq todos-current-todos-file oldfile
		  todos-categories (todos-make-categories-list)))
	  (widen)
	  (goto-char orig-mrk)
	  (todos-insert-with-overlays item)
	  (setq todos-category-number oldnum)
	  (todos-item-counts oldcat 'insert)
	  (and diary-item (todos-item-counts oldcat 'diary))
	  (todos-category-select)
	  (set-buffer-modified-p modified)
	  (goto-char opoint))
	(set-marker orig-mrk nil)))))

(defun todos-move-item-to-file ()
  ""
  (interactive)
  (todos-move-item t))

(defun todos-item-done ()
  "Mark current item as done and move it to category's done section."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))
    (let* ((buffer-read-only)
	   (cat (todos-current-category))
	   (item (todos-item-string))
	   (diary-item (todos-diary-item-p))
	   (date-string (calendar-date-string (calendar-current-date) t t))
	   (time-string (if todos-always-add-time-string ;FIXME: delete condition
			    (concat " " (substring (current-time-string) 11 16))
			  ""))
	   ;; FIXME: todos-nondiary-*
	   (done-item (concat "[" todos-done-string date-string time-string "] "
			      item)))
      (todos-remove-item)
      (save-excursion
	(widen)
	(re-search-forward (concat "^" (regexp-quote todos-category-done)) nil t)
	(forward-char)
	(todos-insert-with-overlays done-item))
      (todos-item-counts cat 'done)
      (and diary-item (todos-item-counts cat 'nondiary))
      (save-excursion (todos-category-select)))))

(defun todos-item-undo ()
  ""
  (interactive)
  (when (todos-done-item-p)
    (let* ((buffer-read-only)
	   (cat (todos-current-category))
	   (done-item (todos-item-string))
	   (opoint (point))
	   (orig-mrk (progn (todos-item-start) (point-marker)))
	   (start (search-forward "] "))	; end of done date string
	   (item (buffer-substring start (todos-item-end)))
	   undone)
      (todos-remove-item)
      (unwind-protect
	  (progn
	    (todos-set-item-priority item cat)
	    (setq undone t)
	    (todos-item-counts cat 'undo)
	    (and (todos-diary-item-p) (todos-item-counts cat 'diary)))
	(unless undone
	  (widen)
	  (goto-char orig-mrk)
	  (todos-insert-with-overlays done-item)
	  (let ((todos-show-with-done t))
	    (todos-category-select)
	    (goto-char opoint)))
	(set-marker orig-mrk nil)))))

(defun todos-archive-done-items ()
  "Archive the done items in the current category."
  (interactive)
  (let ((cat (todos-current-category)))
    (if (zerop (todos-get-count 'done cat))
	(message "No done items in this category")
      (when (y-or-n-p "Move all done items in this category to the archive? ")
	(let* ((afile (concat (file-name-sans-extension (buffer-file-name)) ".toda"))
	       (archive (find-file-noselect afile t))
	       beg end
	       (buffer-read-only nil))
	  (save-excursion
	    (save-restriction
	      (goto-char (point-min))
	      (widen)
	      (setq beg (progn
			  (re-search-forward todos-done-string-start nil t)
			  (match-beginning 0)))
	      (setq end (if (re-search-forward
			     (concat "^" (regexp-quote todos-category-beg)) nil t)
			    (match-beginning 0)
			  (point-max)))
	      (setq done (buffer-substring beg end))
	      (with-current-buffer archive
		(let (buffer-read-only)
		  (widen)
		  (goto-char (point-min))
		  (if (progn
			(re-search-forward
			 (concat "^" (regexp-quote (concat todos-category-beg cat)))
			 nil t)
			(re-search-forward (regexp-quote todos-category-done) nil t))
		      (forward-char)
		    (insert todos-category-beg cat "\n\n" todos-category-done "\n"))
		  (insert done)
		  (save-buffer)))
		(remove-overlays beg end)
		(delete-region beg end)
		(todos-item-counts cat 'archive)))))
      (message "Done items archived."))))

(defun todos-unarchive-category ()
  "Restore this archived category to done items in Todos file."
  (interactive)
  (when (y-or-n-p "Restore all items in this category to Todos file as done items? ")
    (let ((buffer-read-only nil)
	  (tbuf (find-file-noselect
		  (concat (file-name-sans-extension (buffer-file-name)) ".todo")
		  t))
	  (cat (todos-current-category))
	  (items (buffer-substring (point-min) (point-max))))
      (with-current-buffer tbuf
	(let (buffer-read-only)
	  (widen)
	  (goto-char (point-min))
	  (re-search-forward (concat "^" (regexp-quote
					  (concat todos-category-beg cat)))
			     nil t)
	  (if (re-search-forward (concat "^" (regexp-quote todos-category-beg))
				 nil t)
	      (goto-char (match-beginning 0))
	    (goto-char (point-max)))
	  (insert items)))
      (widen)
      (let ((beg (re-search-backward (concat "^"
					     (regexp-quote todos-category-beg)
					     cat) nil t))
	    (end (if (re-search-forward
		      (concat "^" (regexp-quote todos-category-beg)) nil t 2)
		     (match-beginning 0)
		   (point-max))))
	(remove-overlays beg end)
	(delete-region beg end))
      (goto-char (point-min))
      (if (re-search-forward
	   (concat "^" (regexp-quote todos-category-beg)) nil t)
	  (progn
	    ;; delete category from archive
	    (setq todos-categories (delete (assoc cat todos-categories)
					   todos-categories))
	    (todos-update-categories-sexp))
	;; no more categories in archive, so delete it
	(set-buffer-modified-p nil)	; no questions
	(delete-file (buffer-file-name))
	(kill-buffer))
      (let ((tfile (buffer-file-name tbuf))
	    (todos-show-with-done t))
	(find-file tfile)
	(setq todos-current-todos-file tfile
	      ;; also updates item counts
	      todos-categories (todos-make-categories-list t)
	      todos-category-number (todos-category-number cat))
	(todos-show)
	(message "Items unarchived.")))))

(defun todos-toggle-item-diary-inclusion ()
  ""
  (interactive)
  (save-excursion
    (let* ((buffer-read-only)
	   (beg (todos-item-start))
	   (lim (save-excursion (todos-item-end)))
	   (end (save-excursion
		 (or (todos-time-string-match lim)
		     (todos-date-string-match lim))))
	   (cat (todos-current-category)))
      (if (looking-at (regexp-quote todos-nondiary-start))
	  (progn
	    (replace-match "")
	    (search-forward todos-nondiary-end (1+ end) t)
	    (replace-match "")
	    (todos-item-counts cat 'nondiary))
	(when end
	  (insert todos-nondiary-start)
	  (goto-char (1+ end))
	  (insert todos-nondiary-end)
	  (todos-item-counts cat 'diary))))))

(defun todos-toggle-diary-inclusion (arg)
  ""
  (interactive "p")
  (save-excursion
    (save-restriction
      (when (eq arg 2) (widen))		;FIXME: don't toggle done items
      (when (or (eq arg 1) (eq arg 2))
	(goto-char (point-min))
	(when (eq arg 2)
	  (re-search-forward (concat "^" (regexp-quote todos-category-beg)) nil t)
	  (forward-line)
	  (when (looking-at (regexp-quote todos-category-done)) (forward-line)))
	(while (not (eobp))
	  (todos-toggle-item-diary-inclusion)
	  (todos-forward-item))))))

(defun todos-toggle-item-diary-nonmarking ()
  ""
  (interactive)
  (let ((buffer-read-only))
    (save-excursion
      (todos-item-start)
      (unless (looking-at (regexp-quote todos-nondiary-start))
	(if (looking-at (regexp-quote diary-nonmarking-symbol))
	    (replace-match "")
	  (insert diary-nonmarking-symbol))))))

(defun todos-toggle-diary-nonmarking ()
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (todos-toggle-item-diary-nonmarking)
      (todos-forward-item))))

;; FIXME: save to a file named according to the current todos file
(defun todos-save-top-priorities (&optional nof-priorities)
  "Save top priorities for each category in `todos-file-top'.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to `todos-show-priorities'."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (todos-top-priorities nof-priorities)
        (set-buffer todos-tmp-buffer-name)
        (write-file todos-file-top)
        (kill-this-buffer)))))

;; ;;;###autoload
;; (defun todos-print (&optional category-pr-page)
;;   "Print todo summary using `todos-print-function'.
;; If CATEGORY-PR-PAGE is non-nil, a page separator `^L' is inserted
;; between each category.

;; Number of entries for each category is given by `todos-print-priorities'."
;;   (interactive "P")
;;   (when (yes-or-no-p "Print Todos list? ")
;;     (save-window-excursion
;;       (save-excursion
;; 	(save-restriction
;; 	  (todos-top-priorities todos-print-priorities
;; 				category-pr-page)
;; 	  (set-buffer todos-tmp-buffer-name)
;; 	  (and (funcall todos-print-function)
;; 	       (kill-this-buffer))
;; 	  (message "Todo printing done."))))))

(defun todos-print ()
  ""
  (interactive)
  (let ((buf (cond ((eq major-mode 'todos-mode)
		    (concat "Category: " (todos-current-category) " ("
			    (file-name-nondirectory todos-current-todos-file) ") "))
		   ((eq major-mode 'todos-top-priorities-mode)
		    "Todos Top Priorities")))
	(prefix (propertize (concat todos-prefix " ") 'face 'todos-prefix-string))
	(num 0)
	(fill-prefix (make-string todos-indent-to-here 32))
	(content (buffer-string)))
    (with-current-buffer (get-buffer-create buf)
      (insert content)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((beg (point))
	      (end (save-excursion (todos-item-end))))
	  (when todos-number-prefix
	    (setq num (1+ num))
	    (setq prefix (propertize (concat (number-to-string num) " ")
				     'face 'todos-prefix-string)))
	  (insert prefix)
	  (fill-region beg end))
	(todos-forward-item))
      ;; FIXME: ask user to choose between sending to printer:
      ;; (ps-print-buffer-with-faces)
      ;; and printing to a file:
      (ps-spool-buffer-with-faces)
      ;; (write-file )
      )
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------

;;; Internals

(defvar todos-date-pattern		;FIXME: start with "^" ?
  (let ((dayname (diary-name-pattern calendar-day-name-array nil t)))
    (concat "\\(?:" dayname "\\|"
	    (let ((dayname)
		  (monthname (format "\\(?:%s\\|\\*\\)"
				     (diary-name-pattern calendar-month-name-array
							 calendar-month-abbrev-array
							 t)))
		  (month "\\(?:[0-9]+\\|\\*\\)")
		  (day "\\(?:[0-9]+\\|\\*\\)")
		  (year "-?\\(?:[0-9]+\\|\\*\\)"))
	      (mapconcat 'eval calendar-date-display-form ""))
	    "\\)"))
  "Regular expression matching a Todos date header.")

(defvar todos-date-string-start
  (concat "^\\(" (regexp-quote todos-nondiary-start) "\\|"
	  (regexp-quote diary-nonmarking-symbol) "\\)?") ;FIXME: matches anything
  "Regular expression matching part of item header before the date.")

(defvar todos-done-string-start
  (concat "^" (regexp-quote todos-nondiary-start) (regexp-quote todos-done-string))
  "Regular expression matching start of done item.")

;; FIXME: rename these *-matcher
(defun todos-date-string-match (lim)
  "Search for Todos date strings within LIM for font-locking."
  (re-search-forward (concat todos-date-string-start "\\(?1:"
			     todos-date-pattern "\\)") lim t))

(defun todos-time-string-match (lim)
  "Search for Todos time strings within LIM for font-locking."
  (re-search-forward (concat todos-date-string-start todos-date-pattern
		      " \\(?1:" diary-time-regexp "\\)") lim t))

(defun todos-done-string-match (lim)
  "Search for Todos done headers within LIM for font-locking."
  (re-search-forward (concat todos-done-string-start
		      "[^][]+]")
		     lim t))

(defun todos-category-string-match (lim)
  "Search for Todos category headers within LIM for font-locking."
  (if (eq major-mode 'todos-top-priorities-mode)
      (re-search-forward
       ;; (concat "^\\(?1:" (regexp-quote todos-category-beg) ".*\\)$")
       (concat "\\(?:^\\[?" todos-date-pattern "\\(?: " diary-time-regexp
	       "\\)?\\]?\\) \\(?1:\\[.+\\]\\)") lim t)))

(defun todos-check-format ()
  "Signal an error if the current Todos file is ill-formatted."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((legit (concat "^\\(" (regexp-quote todos-category-beg) "\\)"
			   "\\|\\(\\[?" todos-date-pattern "\\)"
			   "\\|\\([ \t]+[^ \t]*\\)"
			   "\\|$")))
	(while (not (eobp))
	  (unless (looking-at legit)
	    (error "Illegitimate Todos file format at line %d"
		   (line-number-at-pos (point))))
	  (forward-line)))))
  (message "This Todos file is well-formatted."))

(defun todos-wrap-and-indent ()
  ""
  (make-local-variable 'word-wrap)
  (setq word-wrap t)
  (make-local-variable 'wrap-prefix)
  (setq wrap-prefix (make-string todos-indent-to-here 32))
  (unless (member '(continuation) fringe-indicator-alist)
    (push '(continuation) fringe-indicator-alist)))

(defun todos-indent ()
  ""
  (indent-to todos-indent-to-here todos-indent-to-here))

(defun todos-prefix-overlays ()
  ""
  (when (or todos-number-prefix
	    (not (string-match "^[[:space:]]*$" todos-prefix)))
    (let ((prefix (propertize (concat todos-prefix " ") 'face 'todos-prefix-string))
	  (num 0))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when (or (todos-date-string-match (line-end-position))
		    (todos-done-string-match (line-end-position)))
	    (goto-char (match-beginning 0))
	    (when todos-number-prefix
	      (setq num (1+ num))
	      ;; reset number for done items
	      (when
		  ;; FIXME: really need this?
		  ;; if last not done item is multiline, then
		  ;; todos-done-string-match skips empty line, so have
		  ;; to look back.
		  (and (looking-at ;; (concat "^\\[" (regexp-quote todos-done-string))
				   todos-done-string-start)
		       (looking-back (concat "^" (regexp-quote todos-category-done)
					     "\n")))
		(setq num 1))
	      (setq prefix (propertize (concat (number-to-string num) " ")
				       'face 'todos-prefix-string)))
	    (let* ((ovs (overlays-in (point) (point)))
		   (ov-pref (car ovs))
		   (val (when ov-pref (overlay-get ov-pref 'before-string))))
	      ;; FIXME: is this possible?
	      (when (and (> (length ovs) 1)
			 (not (equal val prefix)))
		(setq ov-pref (cadr ovs)))
	      (when (not (equal val prefix))
		;; (when ov-pref (delete-overlay ov-pref)) ; why doesn't this work ???
		(remove-overlays (point) (point)); 'before-string val) ; or this ???
		(setq ov-pref (make-overlay (point) (point)))
		(overlay-put ov-pref 'before-string prefix))))
	(forward-line))))))

(defun todos-reset-prefix (symbol value)
  "Set SYMBOL's value to VALUE, and ."	; FIXME
  (let ((oldvalue (symbol-value symbol))
	(files (append todos-files todos-archives)))
    (custom-set-default symbol value)
    (when (not (equal value oldvalue))
      (dolist (f files)
	(with-current-buffer (find-file-noselect f)
	  (save-window-excursion
	    (todos-show)
	    (save-excursion
	      (widen)
	      (goto-char (point-min))
	      (while (not (eobp))
		(remove-overlays (point) (point)); 'before-string prefix)
		(forward-line)))
	    ;; activate the new setting (save-restriction does not help)
	    (save-excursion (todos-category-select))))))))

(defun todos-reset-separator (symbol value)
  "Set SYMBOL's value to VALUE, and ."	; FIXME
  (let ((oldvalue (symbol-value symbol))
	(files (append todos-files todos-archives)))
    (custom-set-default symbol value)
    (when (not (equal value oldvalue))
      (dolist (f files)
	(with-current-buffer (find-file-noselect f)
	  (save-window-excursion
	    (todos-show)
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward
		     ;; (concat "^\\[" (regexp-quote todos-done-string))
		     todos-done-string-start nil t)
		(remove-overlays (point) (point))))
	    ;; activate the new setting (save-restriction does not help)
	    ;; FIXME: need to wrap in save-excursion ?
	    (todos-category-select)))))))

(defun todos-reset-done-string (symbol value)
  "Set SYMBOL's value to VALUE, and ."	; FIXME
  ;; (let ((oldvalue (symbol-value symbol)))
  ;;   (custom-set-default symbol value)
  ;;   (when (not (equal value oldvalue))
  ;;     (save-window-excursion
  ;; 	(todos-show)
  ;; 	(save-excursion
  ;; 	  (goto-char (point-min))
  ;; 	  (when (re-search-forward ;; (concat "^\\[" (regexp-quote todos-done-string))
  ;; 		 todos-done-string-start nil t)
  ;; 	    (remove-overlays (point) (point))))
  ;; 	;; activate the new setting (save-restriction does not help)
  ;; 	;; FIXME: need to wrap in save-excursion ?
  ;; 	(todos-category-select))))
  )

(defun todos-reset-categories (symbol value)
  "Set SYMBOL's value to VALUE, and ."	; FIXME
  (custom-set-default symbol value)
  (save-window-excursion
    (todos-show)
    (setq todos-categories
	  (if value
	      (todos-truncate-categories-list)
	    ;; FIXME: with-current-buffer Todos
	    ;; file and update
	    ;; todos-categories-sexp
	    (todos-make-categories-list t)))))
	;; (save-excursion
	;; ;; activate the new setting (save-restriction does not help)
	;; ;; FIXME: need to wrap in save-excursion ?
	;; (todos-category-select)))))

(defun todos-toggle-switch-todos-file-noninteractively (symbol value)
  ""
  (custom-set-default symbol value)
  (if value
      (add-hook 'post-command-hook
		'todos-switch-todos-file nil t)
    (remove-hook 'post-command-hook
		'todos-switch-todos-file t)))

(defun todos-switch-todos-file (&optional file) ;FIXME: need FILE?
  "Make another Todos file the current Todos file.
Called by post-command-hook if `todos-auto-switch-todos-file' is
non-nil (and also in `todos-top-priorities'), it makes the
current buffer the current Todos file if it is visiting a Todos
file."
  (let ((file (or file (buffer-file-name)))
	(files (if todos-show-done-only		    ;FIXME: should only hold for
		   (funcall todos-files-function t) ; todos-archives
		 (funcall todos-files-function)))
	cat)
    (when (and (member file files)
	       (not (equal todos-current-todos-file file)))
	       ;; (let ((catbuf (get-buffer todos-categories-buffer)))
	       ;; 	 (if catbuf (not (eq (other-buffer) catbuf)))))
      (if todos-ignore-archived-categories
	  (progn
	    (setq todos-categories nil)
	    (setq todos-categories (todos-truncate-categories-list)))
	(setq todos-categories (todos-make-categories-list)))
      ;; if file is already in a buffer, redisplay the previous current category
      (when (< (- (point-max) (point-min)) (buffer-size))
	(widen)
	(when (re-search-backward (concat "^" (regexp-quote todos-category-beg)
					  "\\(.+\\)\n") nil t)
	  (setq cat (match-string-no-properties 1))
	  (setq todos-category-number (todos-category-number cat))))
      (setq todos-current-todos-file file)
      ;; (or todos-category-number (setq todos-category-number 1))	
      ;; (if (zerop todos-category-number) (setq todos-category-number 1))
      (todos-show))))

(defun todos-category-number (cat)
  "Set todos-category-number to index of CAT in todos-categories."
  (let ((categories (mapcar 'car todos-categories)))
    (setq todos-category-number
	  (1+ (- (length categories)
		 (length (member cat categories)))))))

(defun todos-current-category ()
  "Return the name of the current category."
  (car (nth (1- todos-category-number) todos-categories)))

;; FIXME: wrap in save-excursion (or else have to use todos-show in
;; e.g. todos-{forward, backward}-category)
(defun todos-category-select ()
  "Display the current category correctly.

With non-nil `todos-show-with-done' display the category's done
\(but not archived) items below the unfinished todo items; else
display just the todo items."
  (let ((name (todos-current-category))
	cat-begin cat-end done-start done-sep-start done-end)
    (widen)
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote (concat todos-category-beg name)) "$") nil t)
    (setq cat-begin (1+ (line-end-position)))
    (setq cat-end (if (re-search-forward
		       (concat "^" (regexp-quote todos-category-beg)) nil t)
		      (match-beginning 0)
		    (point-max)))
    (setq mode-line-buffer-identification
	  (concat (format "Category %d: %s" todos-category-number name)))
    (narrow-to-region cat-begin cat-end)
    (todos-prefix-overlays)
    (goto-char (point-min))
    (if (re-search-forward (concat "\n\\(" (regexp-quote todos-category-done)
				   "\\)") nil t)
	(progn
	  (setq done-start (match-beginning 0))
	  (setq done-sep-start (match-beginning 1))
	  (setq done-end (match-end 0)))
      (error "Category %s is missing todos-category-done string" name))
    (if todos-show-done-only
	(narrow-to-region (1+ done-end) (point-max))
      ;; display or hide done items as per todos-show-with-done
      ;; FIXME: use todos-done-string-start ?
      (when (re-search-forward (concat "\n\\(\\[" (regexp-quote todos-done-string)
				       "\\)") nil t)
	(let (done-sep prefix ov-pref ov-done)
	  ;; FIXME: delete overlay when not viewing done items
	  (when todos-show-with-done
	    (setq done-sep todos-done-separator)
	    (setq done-start cat-end)
	    (setq ov-pref (make-overlay done-sep-start done-end))
	    (overlay-put ov-pref 'display done-sep))))
      (narrow-to-region (point-min) done-start))))

(defun todos-insert-with-overlays (item)
  ""
  (todos-item-start)
  (insert item "\n")
  (todos-backward-item)
  (todos-prefix-overlays))

(defun todos-item-string-start ()
  "Return the start of this TODO list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (todos-item-string)))
    (if (> (length item) 60)
        (setq item (concat (substring item 0 56) "...")))
    item))

(defvar todos-item-start ;; (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
			 ;; 	 "\\)?\\)?" todos-date-pattern)
  (concat "\\(" todos-date-string-start "\\|" todos-done-string-start
	  "\\)" todos-date-pattern)
  "String identifying start of a Todos item.")

(defun todos-item-start ()
  "Move to start of current TODO list item and return its position."
  (unless (or (looking-at "^$")		; last item or between done and not done
	      (looking-at (regexp-quote todos-category-beg))) ; for todos-count-items
    (goto-char (line-beginning-position))
    (while (not (looking-at todos-item-start))
      (forward-line -1))
    (point)))

(defun todos-item-end ()
  "Move to end of current TODO list item and return its position."
  (unless (looking-at "^$")		; FIXME:
    (let ((done (todos-done-item-p)))
      (todos-forward-item)
      ;; adjust if item is last unfinished one before displayed done items
      (when (and (not done) (todos-done-item-p))
	(forward-line -1))
      (backward-char))
    (point)))

(defun todos-remove-item ()
  "Delete the current entry from the TODO list."
  (let* ((beg (todos-item-start))
	 (end (progn (todos-item-end) (1+ (point))))
	 (ov-start (car (overlays-in beg beg))))
    (when ov-start
      (delete-overlay ov-start))
    (delete-region beg end)))

(defun todos-item-string ()
  "Return current TODO list entry as a string."
  (let ((opoint (point))
	(start (todos-item-start))
	(end (todos-item-end)))
    (goto-char opoint)
    (and start end (buffer-substring-no-properties start end))))

(defun todos-diary-item-p ()
  ""
  (save-excursion
    (todos-item-start)
    (looking-at todos-date-pattern)))

(defun todos-done-item-p ()
  ""
  (save-excursion
    (todos-item-start)
    (looking-at todos-done-string-start)))

;; FIXME: should be defsubst?
(defun todos-counts (cat)
  "Plist/Vector of item type counts in category CAT.
The counted types are all todo items, todo items for diary
inclusion, done items and archived items."
  (cdr (assoc cat todos-categories)))

(defun todos-get-count (type cat)
  "Return count of TYPE items in category CAT."
  (let (idx)
    (cond ((eq type 'todo)
	   (setq idx 0))
	  ((eq type 'diary)
	   (setq idx 1))
	  ((eq type 'done)
	   (setq idx 2))
	  ((eq type 'archived)
	   (setq idx 3)))
    (aref (todos-counts cat) idx)
  ;; (plist-get (todos-counts cat) type)
  ))

(defun todos-set-count (type counts increment)
  "Increment count of item TYPE in vector COUNTS by INCREMENT."
  (let (idx)
    (cond ((eq type 'todo)
	   (setq idx 0))
	  ((eq type 'diary)
	   (setq idx 1))
	  ((eq type 'done)
	   (setq idx 2))
	  ((eq type 'archived)
	   (setq idx 3)))
    (aset counts idx (+ increment (aref counts idx)))
  ;; (plist-put counts type (1+ (plist-get counts type)))
  ))

(defun todos-set-categories ()
  "Set todos-categories from the sexp at the top of the file."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (looking-at "\(\(\"")
	  (setq todos-categories (read (buffer-substring-no-properties
					(line-beginning-position)
					(line-end-position))))
	(error "Invalid or missing todos-categories sexp")))))

(defun todos-make-categories-list (&optional force)
  "Return a list of Todos categories and their item counts.
The items counts are contained in a vector specifying the numbers
of todo items, done items and archived items in the category, in
that order."
  (setq todos-categories nil)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (counts cat archive)
	;; FIXME: can todos-archives be too old here?
	(unless (member buffer-file-name (funcall todos-files-function t))
	  (setq archive (concat (file-name-sans-extension
				 todos-current-todos-file) ".toda")))
	(while (not (eobp))
	  (cond ((looking-at (concat (regexp-quote todos-category-beg)
				     "\\(.*\\)\n"))
		 (setq cat (match-string-no-properties 1))
		 ;; counts for each category: [todo diary done archive]
		 (setq counts (make-vector 4 0))
		 ;; (setq counts (list 'todo 0 'diary 0 'done 0 'archived 0))
		 (setq todos-categories
		       (append todos-categories (list (cons cat counts))))
		 ;; todos-archives may be too old here (e.g. during
		 ;; todos-move-category)
		 (when (member archive (funcall todos-files-function t))
		   (with-current-buffer (find-file-noselect archive)
		     (widen)
		     (goto-char (point-min))
		     (when (re-search-forward
			    (concat (regexp-quote todos-category-beg) cat)
			    (point-max) t)
		       (forward-line)
		       (while (not (or (looking-at
					(concat (regexp-quote todos-category-beg)
						"\\(.*\\)\n"))
				       (eobp)))
			 (when (looking-at todos-done-string-start)
			   (todos-set-count 'archived counts 1))
			 (forward-line))))))
		((looking-at todos-done-string-start)
		 (todos-set-count 'done counts 1))
		((looking-at (concat "^\\(" (regexp-quote diary-nonmarking-symbol)
				     "\\)?" todos-date-pattern))
		 (todos-set-count 'diary counts 1)
		 (todos-set-count 'todo counts 1))
		((looking-at (concat todos-date-string-start todos-date-pattern))
		 (todos-set-count 'todo counts 1))
		;; if first line is todos-categories list, use it and end loop
		;; unless forced by non-nil parameter `force' to scan whole file
		((bobp)
		 (unless force
		   (setq todos-categories (read (buffer-substring-no-properties
						 (line-beginning-position)
						 (line-end-position))))
		   (goto-char (1- (point-max))))))
	  (forward-line)))))
  todos-categories)

;; FIXME: don't let truncated list get written by todos-update-categories-sexp
(defun todos-truncate-categories-list ()
  "Return a truncated list of Todos categories plus item counts.
Categories containing only archived items are omitted.  This list
is used in Todos mode when `todos-ignore-archived-categories' is
non-nil."
  (let (cats)
    (unless todos-categories
      (setq todos-categories (todos-make-categories-list)))
    (dolist (catcons todos-categories cats)
      (let ((cat (car catcons)))
	(setq cats
	      (append cats
		      (unless (and (zerop (todos-get-count 'todo cat))
				   (zerop (todos-get-count 'done cat))
				   (not (zerop (todos-get-count 'archived cat))))
			     (list catcons))))))))

(defun todos-update-categories-sexp ()
  ""
  (let (buffer-read-only)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if (looking-at (concat "^" (regexp-quote todos-category-beg)))
	    (progn (newline) (goto-char (point-min)))
	  (kill-line))
	(prin1 todos-categories (current-buffer))))))

;; FIXME: should done diary items count as diary?
(defun todos-item-counts (cat &optional type)
  ""
  (let ((counts (todos-counts cat)))
    (cond ((eq type 'insert)
	   (todos-set-count 'todo counts 1))
	  ((eq type 'diary)
	   (todos-set-count 'diary counts 1))
	  ((eq type 'nondiary)
	   (todos-set-count 'diary counts -1))
	  ((eq type 'delete)
	   ;; FIXME: ok if last done item was deleted?
	   (if (save-excursion
		 (re-search-backward (concat "^" (regexp-quote
						  todos-category-done)) nil t))
	       (todos-set-count 'done counts -1)
	     (todos-set-count 'todo counts -1)))
	  ((eq type 'done)
	   (todos-set-count 'todo counts -1)
	   (todos-set-count 'done counts 1))
	  ((eq type 'undo)
	   (todos-set-count 'todo counts 1)
	   (todos-set-count 'done counts -1))
	  ((eq type 'archive)
	   (todos-set-count 'archived counts (todos-get-count 'done cat)) ;arch+done
	   (todos-set-count 'done counts (- (todos-get-count 'done cat))))) ; 0
    (todos-update-categories-sexp)))

(defun todos-longest-category-name-length (categories)
  ""
  (let ((longest 0))
    (dolist (c categories longest)
      (setq longest (max longest (length c))))))

(defun todos-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todos-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (todos-string-count-lines string) 1))

(defun todos-read-file-name (prompt &optional archive)
  ""
  (unless (file-exists-p todos-files-directory)
    (make-directory todos-files-directory))
  (let* ((completion-ignore-case t)
	 (files (mapcar 'file-name-sans-extension
			(directory-files todos-files-directory nil
					 (if archive "\.toda$" "\.todo$"))))
	 (file (concat todos-files-directory
		       (completing-read prompt files nil t)
		       (if archive ".toda" ".todo"))))
    (expand-file-name file)))

(defun todos-read-category (prompt)
  "Return a category name from the current Todos file, with completion.
Prompt with PROMPT."
  ;; allow SPC to insert spaces, for adding new category names with
  ;; todos-move-item
  (let ((map minibuffer-local-completion-map))
    (define-key map " " nil)
    ;; make a copy of todos-categories in case history-delete-duplicates is
    ;; non-nil, which makes completing-read alter todos-categories
    (let* ((categories (copy-sequence todos-categories))
	   (history (cons 'todos-categories (1+ todos-category-number)))
	   ;; (default (todos-current-category)) ;FIXME: why this default?
	   (completion-ignore-case todos-completion-ignore-case)
	   (category (completing-read prompt
		      ;; (concat "Category [" default "]: ")
		      todos-categories nil nil nil history))); default)))
      ;; restore the original value of todos-categories
      (setq todos-categories categories)
      category)))

(defun todos-validate-category-name (cat)
  "Check new category name CAT and when valid return it."
  (let (prompt)
    (while
	(and (cond ((string= "" cat)
		    (if todos-categories
			(setq prompt "Enter a non-empty category name: ")
		      ;; prompt for initial category of a new Todos file
		      (setq prompt (concat "Initial category name ["
					   todos-initial-category "]: "))))
		   ((string-match "\\`\\s-+\\'" cat)
		    (setq prompt
			  "Enter a category name that is not only white space: "))
		   ((assoc cat todos-categories)
		    (setq prompt "Enter a non-existing category name: ")))
	     (setq cat (if todos-categories
			   (read-from-minibuffer prompt)
			 ;; offer default initial category name
			 ;; FIXME: if input is just whitespace, raises "End of
			 ;; file during parsing" error
			 (prin1-to-string
			  (read-from-minibuffer prompt nil nil t nil
						(list todos-initial-category))))))))
  cat)

;; adapted from calendar-read-date and calendar-date-string
(defun todos-read-date ()
  "Prompt for Gregorian date and return it in the current format.
Also accepts `*' as an unspecified month, day, or year."
  (let* ((year (calendar-read
                "Year (>0 or * for any year): "
                (lambda (x) (or (eq x '*) (> x 0)))
                (number-to-string (calendar-extract-year
				   (calendar-current-date)))))
         (month-array (vconcat calendar-month-name-array (vector "*")))
	 (abbrevs (vconcat calendar-month-abbrev-array (vector "*")))
         (completion-ignore-case t)
	 (monthname (completing-read
		     "Month name (RET for current month, * for any month): "
		     (mapcar 'list (append month-array nil))
		     nil t nil nil
		     (calendar-month-name (calendar-extract-month
					   (calendar-current-date)) t)))
         (month (cdr (assoc-string
		      monthname (calendar-make-alist month-array nil nil abbrevs))))
         (last (if (eq month 13)
		   31			; FIXME: what about shorter months?
		 (let ((yr (if (eq year '*)
			       1999	; FIXME: no Feb. 29
			     year)))
		   (calendar-last-day-of-month month yr))))
	 day dayname)
    (while (if (numberp day) (or (< day 0) (< last day)) (not (eq day '*)))
      (setq day (read-from-minibuffer
		 (format "Day (1-%d or RET for today or * for any day): " last)
		 nil nil t nil
		 (number-to-string
		  (calendar-extract-day (calendar-current-date))))))
    (setq year (if (eq year '*) (symbol-name '*) (number-to-string year)))
    (setq day (if (eq day '*) (symbol-name '*) (number-to-string day)))
    ;; FIXME: make abbreviation customizable
    (setq monthname
	  (calendar-month-name (calendar-extract-month (list month day year)) t))
    (mapconcat 'eval calendar-date-display-form "")))

(defun todos-read-dayname ()
  ""
  (let ((completion-ignore-case t))
    (completing-read "Enter a day name: "
		     (append calendar-day-name-array nil)
		     nil t)))
  
(defun todos-read-time ()
  ""
  (let (valid answer)
    (while (not valid)
      (setq answer (read-from-minibuffer
		    "Enter a clock time (or return for none): "))
      (when (or (string= "" answer)
		(string-match diary-time-regexp answer))
	(setq valid t)))
    answer))

(defun todos-padded-string (str)
  ""
  (let* ((categories (mapcar 'car todos-categories))
	 (len (todos-longest-category-name-length categories))
	 (strlen (length str))
	 (strlen-odd (eq (logand strlen 1) 1)) ; oddp from cl.el
	 (padding (max 0 (/ (- len strlen) 2)))
	 (padding-left (cond ((eq todos-categories-align 'left) 0)
			     ((eq todos-categories-align 'center) padding)
			     ((eq todos-categories-align 'right)
			      (if strlen-odd (1+ (* padding 2)) (* padding 2)))))
	 (padding-right (cond ((eq todos-categories-align 'left)
			       (if strlen-odd (1+ (* padding 2)) (* padding 2)))
			      ((eq todos-categories-align 'center)
			       (if strlen-odd (1+ padding) padding))
			      ((eq todos-categories-align 'right) 0))))
    (concat (make-string padding-left 32) str (make-string padding-right 32))))

(defvar todos-descending-counts-store nil
  "Alist of current sorted category counts, keyed by sort key.")

;; FIXME: rename to todos-insert-category-info ?
(defun todos-sort (list &optional key)
  "Return a copy of LIST, possibly sorted according to KEY." ;FIXME
  (let* ((l (copy-sequence list))
	 (fn (if (eq key 'alpha)
		   (lambda (x) (upcase x)) ;alphabetize case insensitively
		 (lambda (x) (todos-get-count key x))))
	 (descending (member key todos-descending-counts-store))
	 (cmp (if (eq key 'alpha)
		  'string<
		(if descending '< '>)))
	 (pred (lambda (s1 s2) (let ((t1 (funcall fn (car s1)))
				     (t2 (funcall fn (car s2))))
				 (funcall cmp t1 t2)))))
    (when key
      (setq l (sort l pred))
      (if descending
	  (setq todos-descending-counts-store
		(delete key todos-descending-counts-store))
	(push key todos-descending-counts-store)))
    l))

(defun todos-display-sorted (type)
  "Keep point on the count sorting button just clicked."
  (let ((opoint (point)))
    (todos-display-categories type)
    (goto-char opoint)))

(defun todos-label-to-key (label)
  "Return symbol for sort key associated with LABEL."
  (let (key)
    (cond ((string= label todos-categories-category-label)
	   (setq key 'alpha))
	  ((string= label todos-categories-todo-label)
	   (setq key 'todo))
	  ((string= label todos-categories-diary-label)
	   (setq key 'diary))
	  ((string= label todos-categories-done-label)
	   (setq key 'done))
	  ((string= label todos-categories-archived-label)
	   (setq key 'archived)))
    key))

(defun todos-insert-sort-button (label)
  ""
  (setq str (if (string= label todos-categories-category-label)
		(todos-padded-string label)
	      label))
  (setq beg (point))
  (setq end (+ beg (length str)))
  (insert-button str 'face nil
		 'action
		 `(lambda (button)
		    (let ((key (todos-label-to-key ,label)))
		      (if (and (member key todos-descending-counts-store)
			       (eq key 'alpha))
			  (progn
			    (todos-display-categories)
			    (setq todos-descending-counts-store
				  (delete key todos-descending-counts-store)))
			(todos-display-sorted key)))))
  (setq ovl (make-overlay beg end))
  (overlay-put ovl 'face 'todos-button))

(defun todos-insert-category-line (cat &optional nonum)
  ""
  (let ((archive (member todos-current-todos-file todos-archives))
	(str (todos-padded-string cat))
	(opoint (point)))
	;; beg end ovl)
    ;; num is declared in caller
    (setq num (1+ num))
    ;; (if nonum
    ;; 	(insert (make-string 4 32))
    ;;   (insert " " (format "%2d" num) " | "))
    ;; (setq beg (point))
    ;; (setq end (+ beg (length str)))
    (insert-button
     ;; FIXME: use mapconcat?
     (concat (if nonum
		 (make-string (+ 3 (length todos-categories-number-separator)) 32)
	       (format " %2d%s" num todos-categories-number-separator))
	     str
	     (make-string (+ 2 (/ (length todos-categories-todo-label) 2)) 32)
	     (unless archive
	       (concat
		(format "%2d" (todos-get-count 'todo cat))
		(make-string (+ 2 (/ (length todos-categories-diary-label) 2)) 32)))
	     (unless archive
	       (concat
		(format "%2d" (todos-get-count 'diary cat))
		(make-string (+ 3 (/ (length todos-categories-done-label) 2)) 32)))
	     (format "%2d" (todos-get-count 'done cat))
	     (unless archive
	       (concat
		(make-string (+ 2 (/ (length todos-categories-archived-label) 2)) 32)
		(format "%2d" (todos-get-count 'archived cat))
		(make-string 2 32))))
     'face (if (and todos-ignore-archived-categories
		    (zerop (todos-get-count 'todo cat))
		    (zerop (todos-get-count 'done cat))
		    (not (zerop (todos-get-count 'archived cat))))
	       'todos-archived-only
	     nil)
     'action `(lambda (button) (todos-jump-to-category ,cat)))
    ;; (setq ovl (make-overlay beg end))
    ;; (overlay-put ovl 'face 'todos-button)
    (let* ((beg1 (+ opoint 6 (length str)))
	   end1 ovl1)
      (cond ((eq nonum 'todo)
	     (setq beg1 (+ beg1 1 (/ (length todos-categories-todo-label) 2))))
	    ((eq nonum 'diary)
	     (setq beg1 (+ beg1 1 (length todos-categories-todo-label)
			   2 (/ (length todos-categories-diary-label) 2))))
	    ((eq nonum 'done)
	     (setq beg1 (+ beg1 1 (length todos-categories-todo-label)
			   2 (length todos-categories-diary-label)
			   2 (/ (length todos-categories-done-label) 2))))
	    ((eq nonum 'archived)
	     (setq beg1 (+ beg1 1 (length todos-categories-todo-label)
			   2 (length todos-categories-diary-label)
			   2 (length todos-categories-done-label)
			   2 (/ (length todos-categories-archived-label) 2)))))
      (unless (= beg1 (+ opoint 6 (length str)))
	(setq end1 (+ beg1 4))
	(setq ovl1 (make-overlay beg1 end1))
	(overlay-put ovl1 'face 'todos-sorted-column)))
    (insert (concat "\n"))))

(provide 'todos)

;;; UI
;; - display
;;   - show todos in cat
;;   - show done in cat
;;   - show catlist
;;   - show top priorities in all cats
;;   - show archived
;; - navigation
;;   - 
;; - editing
;;
;;; Internals
;; - cat props: name, number, todos, done, archived
;; - item props: priority, date-time, status?
;; - file format
;;   - cat begin
;;   - todo items 0...n
;;   - empty line
;;   - done-separator
;;   - done item 0...n

;;; todos.el ends here
