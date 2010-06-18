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
  "Maintain lists of todo items."
  :link '(emacs-commentary-link "todos")
  :version "21.1"
  :group 'calendar)

(defcustom todos-prefix  "§" ;  "*/*"  FIXME ascii default
  "String prefixed to todo items for visual distinction."
  :type 'string
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

(defcustom todos-number-prefix t
  "Non-nil to show item prefixes as consecutively increasing integers."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

;; FIXME: length (window-width) causes problems.  Also, bad when window-width changes
(defcustom todos-done-separator (make-string (1- (window-width)) ?-)
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
  ;; :set
  :group 'todos)

(defcustom todos-show-with-done nil
  "Non-nil to display done items in all categories."
  :type 'boolean
  :group 'todos)

;; FIXME: use user-emacs-directory here and below
(defcustom todos-file-do (convert-standard-filename "~/.emacs.d/.todos-do")
  "TODO mode list file."
  :type 'file
  :group 'todos)

(defcustom todos-files '((convert-standard-filename "~/.emacs.d/.todos"))
  "List of Todos files."
  :type 'list
  :group 'todos)

(defcustom todos-archive-file (convert-standard-filename "~/.emacs.d/.todos-archive")
  "File of finished Todos categories."
  :type 'file
  :group 'todos)

(defcustom todos-mode-hook nil
  "TODO mode hooks."
  :type 'hook
  :group 'todos)

(defcustom todos-edit-mode-hook nil
  "TODO Edit mode hooks."
  :type 'hook
  :group 'todos)

(defcustom todos-categories-buffer "*TODOS Categories*"
  "Name of buffer displayed by `todos-display-categories'."
  :type 'string
  :group 'todos)

(defcustom todos-archived-categories-buffer "*TODOS Archived Categories*"
  "Name of buffer displayed by `todos-display-categories'."
  :type 'string
  :group 'todos)

(defcustom todos-edit-buffer " *TODO Edit*"
  "TODO Edit buffer name."
  :type 'string
  :group 'todos)

(defcustom todos-file-top (convert-standard-filename "~/.todos-top")
  "TODO mode top priorities file.

Not in TODO format, but diary compatible.
Automatically generated when `todos-save-top-priorities' is non-nil."
  :type 'string
  :group 'todos)

(defcustom todos-include-in-diary nil
  "Non-nil to allow new Todo items to be included in the diary."
  :type 'boolean
  :group 'todos)

(defcustom todos-exclusion-start "["
  "String prepended to item date to block diary inclusion."
  :type 'string
  :group 'todos
  ;; :initialize 'custom-initialize-default
  ;; :set ; change in whole Todos file
  )

(defcustom todos-exclusion-end "]"
  "String appended to item date to match `todos-exclusion-start'."
  :type 'string
  :group 'todos
  ;; :initialize 'custom-initialize-default
  ;; :set ; change in whole Todos file
  )

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

(defcustom todos-always-add-time-string t
  "Add current time to date string inserted in front of new items."
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

(defface todos-prefix-string
  '((t
     :inherit font-lock-constant-face
     ))
  "Face for Todos prefix string."
  :group 'todos)

(defface todos-button
  '((t
     :inherit tool-bar
     ))
  "Face for buttons in todos-display-categories."
  :group 'todos)

(defface todos-date
  '((t
     :inherit diary
     ))
  "Face for Todos prefix string."
  :group 'todos)
(defvar todos-date-face 'todos-date)

(defface todos-time
  '((t
     :inherit diary-time
     ))
  "Face for Todos prefix string."
  :group 'todos)
(defvar todos-time-face 'todos-time)

(defface todos-done
  '((t
     :inherit font-lock-comment-face
     ))
  "Face for done Todos item header string."
  :group 'todos)
(defvar todos-done-face 'todos-done)

(defface todos-done-sep
  '((t
     :inherit font-lock-type-face
     ))
  "Face for separator string bewteen done and not done Todos items."
  :group 'todos)
(defvar todos-done-sep-face 'todos-done-sep)

(defvar todos-font-lock-keywords
  (list
   '(todos-date-string-match 1 todos-date-face t)
   '(todos-time-string-match 1 todos-time-face t)
   '(todos-done-string-match 0 todos-done-face t)
   '(todos-category-string-match 0 todos-done-sep-face t))
  "Font-locking for Todos mode.")

;; ---------------------------------------------------------------------------
;;; Mode setup

(defvar todos-categories nil
  "TODO categories.")

(defvar todos-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    ;; navigation commands
    (define-key map "+" 'todos-forward-category)
    (define-key map "-" 'todos-backward-category)
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    (define-key map "S" 'todos-search)
    ;; display commands
    (define-key map "C" 'todos-display-categories)
    (define-key map "h" 'todos-highlight-item)
    (define-key map "N" 'todos-toggle-item-numbering)
    ;; (define-key map "" 'todos-toggle-display-date-time)
    (define-key map "P" 'todos-print)
    (define-key map "q" 'todos-quit)
    (define-key map "s" 'todos-save)
    (define-key map "v" 'todos-toggle-view-done-items)
    (define-key map "Y" 'todos-diary-items)
    ;; (define-key map "S" 'todos-save-top-priorities)
    (define-key map "t" 'todos-top-priorities)
    ;; editing commands
    (define-key map "A" 'todos-add-category)
    (define-key map "d" 'todos-item-done)
    ;; (define-key map "" 'todos-archive-done-items)
    (define-key map "D" 'todos-delete-category)
    (define-key map "e" 'todos-edit-item)
    (define-key map "E" 'todos-edit-multiline)
    ;; (define-key map "" 'todos-change-date)
    ;; (define-key map "f" 'todos-file-item)
    (define-key map "ii" 'todos-insert-item)
    (define-key map "ih" 'todos-insert-item-here)
    (define-key map "ia" 'todos-insert-item-ask-date-time)
    (define-key map "id" 'todos-insert-item-for-diary)
    ;; (define-key map "in" 'todos-insert-item-no-time)
    (define-key map "k" 'todos-delete-item)
    (define-key map "l" 'todos-lower-item)
    (define-key map "m" 'todos-move-item)
    (define-key map "r" 'todos-raise-item)
    (define-key map "R" 'todos-rename-category)
    (define-key map "u" 'todos-item-undo)
    (define-key map "y" 'todos-toggle-item-diary-inclusion)
    ;; (define-key map "" 'todos-toggle-diary-inclusion)
    (define-key map [remap newline] 'newline-and-indent)
    map)
  "Todos mode keymap.")

(defvar todos-archive-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; navigation commands
    (define-key map "+" 'todos-forward-category)
    (define-key map "-" 'todos-backward-category)
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    ;; display commands
    (define-key map "C" 'todos-display-categories)
    (define-key map "h" 'todos-highlight-item)
    (define-key map "N" 'todos-toggle-item-numbering)
    ;; (define-key map "" 'todos-toggle-display-date-time)
    (define-key map "P" 'todos-print)
    (define-key map "q" 'todos-quit)
    (define-key map "s" 'todos-save)
    (define-key map "S" 'todos-search)
    map)
  "Todos Archive mode keymap.")

(defvar todos-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-q" 'todos-edit-quit)
    (define-key map [remap newline] 'newline-and-indent)
    map)
  "Todos Edit mode keymap.")

(defvar todos-categories-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" 'todos-display-categories-alphabetically)
    (define-key map "c" 'todos-display-categories)
    (define-key map "l" 'todos-lower-category)
    (define-key map "r" 'todos-raise-category)
    (define-key map "q" 'bury-buffer)	;FIXME ?
    ;; (define-key map "A" 'todos-add-category)
    ;; (define-key map "D" 'todos-delete-category)
    ;; (define-key map "R" 'todos-rename-category)
    map)
  "Todos Categories mode keymap.")

(defvar todos-category-number 0 "TODO category number.")

(defvar todos-tmp-buffer-name " *todo tmp*")

(defvar todos-category-beg "--==-- "
  "Category start separator to be prepended onto category name.")

(easy-menu-define todos-menu todos-mode-map "Todo Menu"
                  '("Todo"
                    ["Next category"        todos-forward-category t]
                    ["Previous category"    todos-backward-category t]
                    ["Jump to category"     todos-jump-to-category t]
                    ["Show top priority items" todos-top-priorities t]
                    ["Print categories"     todos-print t]
                    "---"
                    ["Edit item"            todos-edit-item t]
                    ["File item"            todos-file-item t]
                    ["Insert new item"      todos-insert-item t]
                    ["Insert item here"     todos-insert-item-here t]
                    ["Kill item"            todos-delete-item t]
                    "---"
                    ["Lower item priority"  todos-lower-item t]
                    ["Raise item priority"  todos-raise-item t]
                    "---"
                    ["Next item"            todos-forward-item t]
                    ["Previous item"        todos-backward-item t]
                    "---"
                    ["Save"                 todos-save t]
                    ["Save Top Priorities"  todos-save-top-priorities t]
                    "---"
                    ["Quit"                 todos-quit t]
                    ))

;; As calendar reads .todos-do before todos-mode is loaded.
;;;###autoload
(defun todos-mode ()
  "Major mode for displaying, navigating and editing Todo lists.

\\{todos-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todos-mode)
  (setq mode-name "TODOS")
  (use-local-map todos-mode-map)
  (easy-menu-add todos-menu)
  (when todos-wrap-lines (funcall todos-line-wrapping-function))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'todos-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (make-local-variable 'hl-line-range-function)
  (setq hl-line-range-function
	(lambda() (when (todos-item-end)
		    (cons (todos-item-start) (todos-item-end)))))
  ;; (add-hook 'post-command-hook 'todos-show-paren-hack nil t)
  (add-to-invisibility-spec 'todos)
  (setq buffer-read-only t)
  (run-mode-hooks 'todos-mode-hook))

(defun todos-archive-mode ()
  "Major mode for archived Todos categories.

\\{todos-archive-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todos-archive-mode)
  (setq mode-name "TODOS Archive")
  (use-local-map todos-archive-mode-map)
  ;; (easy-menu-add todos-menu)
  (when todos-wrap-lines (funcall todos-line-wrapping-function))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (make-local-variable 'hl-line-range-function)
  (setq hl-line-range-function
	(lambda() (when (todos-item-end)
		    (cons (todos-item-start) (todos-item-end)))))
  ;; (add-hook 'post-command-hook 'todos-show-paren-hack nil t)
  (add-to-invisibility-spec 'todos)
  (run-mode-hooks 'todos-mode-hook))

(defun todos-edit-mode ()
  "Major mode for editing multiline Todo items.

\\{todos-edit-mode-map}"
  (interactive)
  (setq major-mode 'todos-edit-mode)
  (setq mode-name "TODOS Edit")
  (use-local-map todos-edit-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'todos-indent)
  (when todos-wrap-lines (funcall todos-line-wrapping-function)))

(defun todos-categories-mode ()
  "Major mode for displaying and editing Todos categories.

\\{todos-categories-mode-map}"
  (interactive)
  (setq major-mode 'todos-categories-mode)
  (setq mode-name "TODOS Categories")
  (use-local-map todos-categories-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (setq buffer-read-only t)
)

(defun todos-save ()
  "Save the TODO list."
  (interactive)
  (save-excursion
    (save-restriction
      (save-buffer)))
  ;; (if todos-save-top-priorities-too (todos-save-top-priorities)))
  )

(defun todos-quit ()
  "Done with TODO list for now."
  (interactive)
  (widen)
  (todos-save)
  (message "")
  (bury-buffer))

;; ---------------------------------------------------------------------------
;;; Commands

;;; Display

;;;###autoload
(defun todos-show ()
  "Show TODO list."
  (interactive)
  ;; Make this a no-op if called interactively in narrowed Todos mode, since
  ;; it is in that case redundant, but in particular to work around the bug of
  ;; item prefix reduplication with show-paren-mode enabled.
  (unless (and (called-interactively-p)
	       (eq major-mode 'todos-mode)
	       (< (- ( point-max) (point-min)) (buffer-size)))
    ;; Call todos-initial-setup only if there is neither a Todo file nor
    ;; a corresponding unsaved buffer.
    (if (or (file-exists-p todos-file-do)
	    (let* ((buf (get-buffer (file-name-nondirectory todos-file-do)))
		   (bufname (buffer-file-name buf)))
	      (equal (expand-file-name todos-file-do) bufname)))
	(find-file todos-file-do)
      (todos-initial-setup))
    (unless (eq major-mode 'todos-mode) (todos-mode))
    (unless todos-categories-alist
      (setq todos-categories-alist (todos-make-categories-alist)))
    (unless todos-categories
      (setq todos-categories (mapcar 'car todos-categories-alist)))
    (save-excursion
      (todos-category-select)
      ;; (todos-show-paren-hack)
      )))

(defun todos-display-categories (&optional alpha)
  "Display a numbered list of the Todos category names.
The numbers give the order of the categories.

With non-nil ALPHA display a non-numbered alphabetical list.
The lists are in Todos Categories mode.

The category names are buttonized, and pressing a button displays
the category in Todos mode."
  (interactive)
  (let ((categories (copy-sequence todos-categories))
	(num 0))
    (when alpha				;alphabetize the list case insensitively
      (setq categories (sort categories (lambda (s1 s2) (let ((cis1 (upcase s1))
							      (cis2 (upcase s2)))
							  (string< cis1 cis2))))))
    (with-current-buffer (get-buffer-create todos-categories-buffer)
      (switch-to-buffer (current-buffer))
      (let (buffer-read-only)
	(erase-buffer)
	(kill-all-local-variables)
	(insert "Press a button to display the corresponding category.\n\n")
	;; FIXME: abstract format from here and todos-insert-category-name
	(insert (make-string 4 32) (todos-padded-string "Category")
		(make-string 7 32) "Todos   Done\n\n")
	(save-excursion
	  (mapc '(lambda (cat) (todos-insert-category-name cat alpha)) categories)))
      (todos-categories-mode))))

(defun todos-display-categories-alphabetically ()
  ""
  (interactive)
  (todos-display-categories t))

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
	   (if (re-search-forward (concat "\n\\(\\[" (regexp-quote todos-done-string)
				   "\\)") nil t)
	       nil
	     t)))
      (todos-category-select))))

(defun todos-view-archive (&optional cat)
  ""
  (interactive)
  (if (file-exists-p todos-archive-file)
      (progn
	(find-file todos-archive-file)
	(if cat
	    (if (member cat (todos-categories-list (current-buffer)))
		(todos-jump-to-category-noninteractively cat)
	      (error "No archived items from this category"))
	  (todos-category-select)))
    (error "There is currently no Todos archive")))

;; FIXME: slow
(defun todos-diary-items ()
  "Display all todo items marked for diary inclusion."
  (interactive)
  (let ((bufname "*Todo diary entries*")
	opoint)
    (save-restriction
      (save-current-buffer
	(widen)
	(copy-to-buffer bufname (point-min) (point-max))))
    (with-current-buffer bufname
      ;; (todos-mode)
      (goto-char (point-min))
      (while (not (eobp))
	(setq opoint (point))
	(cond ((looking-at "\\[")
	       (progn
		 (todos-forward-item)
		 (if (string-match
		      (concat "^" (regexp-quote todos-category-beg) ".*$")
		      (buffer-substring opoint (point)))
		     (kill-region opoint (+ opoint (match-beginning 0)))
		   (kill-region opoint (point)))))
	      ((looking-at "^$")
	       (kill-line))
	      (t
	       (todos-forward-item))))
      (goto-char (point-min))
      (while (not (eobp))
	(setq opoint (point))
	(if (looking-at (regexp-quote todos-category-beg))
	    (when (progn (forward-line)
			 (or (looking-at (regexp-quote todos-category-beg))
			     ;; category has done but no unfinished items
			     (and (looking-at "^$") (forward-line))
			     (eobp)))
	      (kill-region opoint (point)))
	  (forward-line)))
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults '(todos-font-lock-keywords t))
      (font-lock-fontify-buffer)
      (setq buffer-read-only t))
    (display-buffer bufname)))

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
	  (re-search-forward (concat "^\\[?" todos-date-pattern
				     "\\( " diary-time-regexp "\\)?\\]? ")
					; FIXME: this space in header? ^
			     nil t)
	  (setq ov (make-overlay (match-beginning 0) (match-end 0) nil t))
	  (overlay-put ov 'display "")
	  (forward-line)))
      ;; FIXME: need this?
      ;; (todos-update-numbered-prefix)
      )))
      
;;;###autoload
(defun todos-top-priorities (&optional nof-priorities category-pr-page show-done)
  "List top priorities for each category.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to \'todos-show-priorities\'.

If CATEGORY-PR-PAGE is non-nil, a page separator \'^L\' is inserted
between each category.

With non-nil SHOW-DONE, include done items in the listing."

  (interactive "P")
  (or nof-priorities (setq nof-priorities todos-show-priorities))
  (if (listp nof-priorities)            ;universal argument
      (setq nof-priorities (car nof-priorities)))
  (let ((todos-print-buffer-name todos-tmp-buffer-name)
        (todos-category-break (if category-pr-page "" ""))
        beg end done)
    (save-excursion
      (todos-show))
    (save-restriction
      (save-current-buffer
	(widen)
	(if (buffer-live-p (get-buffer todos-print-buffer-name))
	    (kill-buffer todos-print-buffer-name))
	(copy-to-buffer todos-print-buffer-name (point-min) (point-max))))
    (with-current-buffer todos-print-buffer-name
      (goto-char (point-min))
      (while (re-search-forward       ;Find category start
	      (concat "^" (regexp-quote todos-category-beg))
	      nil t)
	(setq beg (+ (line-end-position) 1)) ;Start of first entry.
	(setq end (if (re-search-forward todos-category-beg nil t)
		      (match-beginning 0)
		    (point-max)))
	(goto-char beg)
	(setq done
	      (if (re-search-forward
		   (concat
		    (if (looking-at "^$") "" "\n") ; no unfinished items
		    "\n\\(\\[" (regexp-quote todos-done-string) "\\)")
		   end t)
		  (match-beginning 1)
		end))
	(unless show-done
	  (delete-region done end)
	  (setq end done))
	(narrow-to-region beg end)    ;In case we have too few entries.
	(goto-char (point-min))
	(if (zerop nof-priorities)      ;Traverse entries.
	    (goto-char end)            ;All entries
	  (todos-forward-item nof-priorities))
	(setq beg (point))
	(delete-region beg end)
	(widen))
      (and (looking-at "") (replace-match "")) ;Remove trailing form-feed.
      (goto-char (point-min))         ;Due to display buffer
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults '(todos-font-lock-keywords t))
      (font-lock-fontify-buffer)
      (setq buffer-read-only t))
    ;; Could have used switch-to-buffer as it has a norecord argument,
    ;; which is nice when we are called from e.g. todos-print.
    ;; Else we could have used pop-to-buffer.
    ;; (display-buffer todos-print-buffer-name)
    (display-buffer todos-print-buffer-name)
    (message "Type C-x 1 to remove %s window.  M-C-v to scroll the help."
             todos-print-buffer-name)))

;;; Navigation

(defun todos-forward-category ()
  "Go forward to TODO list of next category."
  (interactive)
  (setq todos-category-number
        (mod (1+ todos-category-number) (length todos-categories)))
  (todos-category-select))

(defun todos-backward-category ()
  "Go back to TODO list of previous category."
  (interactive)
  (setq todos-category-number
        (mod (1- todos-category-number) (length todos-categories)))
  (todos-category-select))

;; FIXME: Document that a non-existing name creates that category, and add
;; y-or-n-p confirmation -- or eliminate this possibility?
(defun todos-jump-to-category ()
  "Jump to a category.  Default is previous category."
  (interactive)
  (let ((category (todos-read-category)))
    (if (string= "" category)
        (setq category (todos-current-category)))
    (setq todos-category-number
          (if (member category todos-categories)
              (- (length todos-categories)
                 (length (member category todos-categories)))
            (todos-add-category category)))
    ;; (todos-show)))
    (todos-category-select)))

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

;; FIXME: continue search with same regexp
(defvar todos-search-string nil
  ""
  )
(defun todos-search ()
  ""
  (interactive)
  (let ((regex (read-from-minibuffer "Enter a search string (regexp): "))
	(start (point))
	found cat in-done)
    (widen)
    (goto-char (point-min))
    (while (and (setq found (re-search-forward regex nil t))
		(save-excursion
		  (goto-char (line-beginning-position))
		  (looking-at (concat "^" (regexp-quote todos-category-beg)))))
      (forward-line))
    (if found
	(progn
	  (setq found (match-beginning 0))
	  (todos-item-start)
	  (when (looking-at (concat "^\\[" (regexp-quote todos-done-string)))
	    (setq in-done t))
	  (re-search-backward (concat "^" (regexp-quote todos-category-beg)
				      "\\(.*\\)\n") nil t)
	  (setq cat (match-string-no-properties 1))
	  (todos-category-number cat)
	  (todos-category-select)
	  (when in-done	(unless todos-show-with-done (todos-toggle-view-done-items)))
	  (goto-char found))
      (todos-category-select)
      (goto-char start)
      (message "No match for \"%s\"" regex))))

;;; Editing

;;;###autoload
(defun todos-add-category (&optional cat)
  "Add new category CAT to the TODO list."
  (interactive)
  (let ((buffer-read-only)
	(buf (find-file-noselect todos-file-do t)))
    (unless (zerop (buffer-size buf))
      (and (null todos-categories)
	   (error "Error in %s: File is non-empty but contains no category"
		  todos-file-do)))
    (unless cat (setq cat (read-from-minibuffer "Category: ")))
    (with-current-buffer buf
      (setq cat (todos-check-category-name cat))
      ;; initialize a newly created Todo buffer for Todo mode
      (unless (file-exists-p todos-file-do) (todos-mode))
      (push cat todos-categories)
      (push (list cat (cons 0 0)) todos-categories-alist)
      (widen)
      (goto-char (point-min))
      ;; make sure file does not begin with empty lines (shouldn't, but may be
      ;; added by mistake), otherwise new categories will contain them, so
      ;; won't be really empty
      (while (looking-at "^$") (kill-line))
      (insert todos-category-beg cat "\n")
      (if (interactive-p)
	  ;; properly display the newly added category
	  (progn (setq todos-category-number 0) (todos-show))
	0))))

(defun todos-rename-category ()
  "Rename current Todos category."
  (interactive)
  (let* ((buffer-read-only)
	 (cat (todos-current-category))
	 (vec (vconcat todos-categories))
	 (new (read-from-minibuffer (format "Rename category \"%s\" to: " cat))))
    (setq new (todos-check-category-name new))
    (aset vec todos-category-number new)
    (setq todos-categories (append vec nil))
    (setcar (assoc cat todos-categories-alist) new)
    (save-excursion
      (widen)
      (re-search-backward (concat (regexp-quote todos-category-beg) "\\("
				  (regexp-quote cat) "\\)\n") nil t)
      (replace-match new t t nil 1)
      (goto-char (point-min))
      (setq mode-line-buffer-identification (concat "Category:  " new))))
  (todos-category-select))

(defun todos-delete-category (&optional arg)
  "Delete current Todos category provided it is empty.
With ARG non-nil delete the category unconditionally,
i.e. including all existing entries."
  (interactive "P")
  (let* ((cat (todos-current-category))
	 (not-done (car (todos-item-counts cat)))
	 (done (cdr (todos-item-counts cat)))
	 beg end)
    (if (and (null arg)
	     (or (> not-done 0) (> done 0)))
	(message "To delete a non-empty category, type C-u D.")
      (when (y-or-n-p (concat "Permanently remove category \"" cat
			      "\"" (and arg " and all its entries") "? "))
	(let ((buffer-read-only))
	  (widen)
	  (setq beg (re-search-backward (concat "^" (regexp-quote todos-category-beg)
						cat "\n") nil t)
		end (progn
		      (re-search-forward (concat "\n\\("
						 (regexp-quote todos-category-beg)
						 ".*\n\\)") nil t)
		      (match-beginning 1)))
	  (remove-overlays beg end)
	  (kill-region beg end)
	  (setq todos-categories (delete cat todos-categories))
	  (setq todos-categories-alist
		(delete (assoc cat todos-categories-alist) todos-categories-alist))
	  (todos-category-select)
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
	     (cat1 (aref catvec num1))
	     (cat2 (aref catvec num2))
	     (buffer-read-only))
	(delete-region beg end)
	(setq num1 (1+ num1)
	      num2 (1- num2))
	(setq num num2)
	(todos-insert-category-name cat2)
	(setq num num1)
	(todos-insert-category-name cat1)
	(aset catvec num2 cat2)
	(aset catvec num1 cat1)
	(setq todos-categories (append catvec nil))
	(forward-line (if lower -1 -2))
	(forward-char col)))))

(defun todos-lower-category ()
  "Lower priority of category point is on in Categories buffer."
  (interactive)
  (todos-raise-category t))

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
    (if (not (derived-mode-p 'todos-mode)) (todos-show))
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
	   (time-string (cond ((eq time 'omit) nil)
			      ((eq time 'ask-time)
			       (todos-read-time))
			      (todos-always-add-time-string
			       (substring (current-time-string) 11 16))))
	   (new-item (concat (unless (or diary todos-include-in-diary) "[") ;FIXME
			     date-string (when time-string (concat " " time-string))
			     ;; FIXME
			     (unless (or diary todos-include-in-diary) "]") " "
			     (read-from-minibuffer "New TODO entry: ")))
	   (cat (if arg (todos-read-category) (todos-current-category))))
      ;; indent newlines inserted by C-q C-j if nonspace char follows
      (setq new-item (replace-regexp-in-string
		      "\\(\n\\)[^[:blank:]]"
		      (concat "\n" (make-string todos-indent-to-here 32)) new-item
		      nil nil 1))
      ;; (if here
      ;; 	  (todos-insert-with-overlays new-item)
      ;; 	(todos-add-item-non-interactively new-item cat))
      (unless here (todos-set-item-priority new-item cat))
      (todos-insert-with-overlays new-item)
      (todos-item-counts cat 'insert))))

;; FIXME: make insertion options customizable per category
;; date-type: d n (c) - time - diary - here
;; idd            inn            itt  iyy ih
;; idtt  idyy idh intt  inyy inh ityy iyh 
;; idtyy idyh     intyy inyh     ityh 
;; idtyh          intyh 
;; idth	          inth  

;; todos-insert-item
;; todos-insert-item-ask-date
;; todos-insert-item-ask-date-time
;; todos-insert-item-ask-dayname
;; todos-insert-item-ask-dayname-time
;; todos-insert-item-ask-time
;; todos-insert-item-for-diary
;; todos-insert-item-for-diary-ask-date
;; todos-insert-item-for-diary-ask-date-time
;; todos-insert-item-for-diary-ask-dayname
;; todos-insert-item-for-diary-ask-dayname-time
;; todos-insert-item-for-diary-ask-time
;; todos-insert-item-here
;; todos-insert-item-here-ask-date
;; todos-insert-item-here-ask-date-time
;; todos-insert-item-here-ask-dayname
;; todos-insert-item-here-ask-dayname-time
;; todos-insert-item-here-ask-time
;; todos-insert-item-here-ask-time-diary
;; todos-insert-item-here-for-diary
;; todos-insert-item-here-for-diary-ask-date-time
;; todos-insert-item-here-for-diary-ask-time
;; todos-insert-item-here-for-diary-ask-dayname-time

(defun todos-insert-item-here ()
  ""
  (interactive)
  (todos-insert-item nil nil nil t))

(defun todos-insert-item-here-ask-date-time ()
  ""
  (interactive)
  (todos-insert-item nil 'ask-date 'ask-time t))

;; (defun todos-insert-item-no-time ()
;;   ""
;;   (interactive)
;;   (todos-insert-item nil nil 'omit t))

(defun todos-insert-item-ask-date-time (&optional arg)
  ""
  (interactive "P")
  (todos-insert-item arg 'ask-date 'ask-time))

(defun todos-insert-item-ask-dayname-time (&optional arg)
  ""
  (interactive)
  (todos-insert-item arg 'ask-dayname 'ask-time))

(defun todos-insert-item-for-diary (&optional arg)
  ""
  (interactive "P")
  (let ((todos-include-in-diary t))
    (todos-insert-item arg)))

(defun todos-insert-item-for-diary-ask-date-time (&optional arg)
  ""
  (interactive "P")
  (let ((todos-include-in-diary t))
    (todos-insert-item arg 'ask-dayname 'ask-time)))

;; FIXME: autoload when key-binding is defined in calendar.el
(defun todos-insert-item-from-calendar ()
  ""
  (interactive)
  (pop-to-buffer (file-name-nondirectory todos-file-do))
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
	     (todos-entry (todos-item-string-start))
             (todos-answer (y-or-n-p (concat "Permanently remove '"
                                            todos-entry "'? "))))
        (when todos-answer
          (todos-remove-item)
          (when (and (bolp) (eolp)
		     ;; not if last item was deleted
		     (< (point-min) (point-max)))
	    (todos-backward-item))
	  (todos-item-counts (todos-current-category) 'delete)
	  ;; FIXME: is todos-prefix-overlays part of if-sexp, and is it needed
	  ;; at all?
	  ;; (if todos-number-prefix
	  ;;     (todos-update-numbered-prefix)
	  (todos-prefix-overlays)));)
    (error "No TODO list entry to delete")))

(defun todos-edit-item ()
  "Edit current TODO list entry."
  (interactive)
  (let ((buffer-read-only)
	(item (todos-item-string))
	(opoint (point)))
    (if (todos-string-multiline-p item)
        (todos-edit-multiline)
      (let ((new (read-from-minibuffer "Edit: " item)))
	(while (not (string-match (concat "^\\[?" todos-date-pattern) new))
	  (setq new (read-from-minibuffer "Item must start with a date: " new)))
	;; indent newlines inserted by C-q C-j if nonspace char follows
	(setq new (replace-regexp-in-string
		   "\\(\n\\)[^[:blank:]]"
		   (concat "\n" (make-string todos-indent-to-here 32)) new
		   nil nil 1))
	;; If user moved point during editing, make sure it moves back.
	(goto-char opoint)
        (todos-remove-item)
	(todos-insert-with-overlays new)))))
  
;; FIXME: run todos-check-format on exiting buffer (or check for date string
;; and indentation)
(defun todos-edit-multiline ()
  "Set up a buffer for editing a multiline TODO list entry."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name todos-edit-buffer)))
    (switch-to-buffer
     (make-indirect-buffer
      (file-name-nondirectory todos-file-do) buffer-name))
    (message "To exit, simply kill this buffer and return to list.")
    (todos-edit-mode)
    (narrow-to-region (todos-item-start) (todos-item-end))))

(defun todos-edit-quit ()
  ""
  (interactive)
  (save-excursion (todos-category-select)))

;; FIXME: complete
(defun todos-edit-item-header ()
  ""
  (interactive)
  (todos-item-start)
  (re-search-forward (concat "^\\[?\\(?1:" todos-date-pattern
			     "\\) \\(?2:" diary-time-regexp "\\)")
		     (line-end-position) t)
  ;; ask date or dayname
  (replace-match new-date nil nil nil 1)
  ;; ask time
  (replace-match new-date nil nil nil 2))

(defun todos-raise-item ()
  "Raise priority of current entry."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let (buffer-read-only)
      (if (> (count-lines (point-min) (point)) 0)
	  (let ((item (todos-item-string)))
	    (todos-remove-item)
	    (todos-backward-item)
	    (todos-insert-with-overlays item))
	(error "No TODO list entry to raise")))))

(defun todos-lower-item ()
  "Lower priority of current entry."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let* ((buffer-read-only)
	   ;; (end (save-excursion (todos-forward-item) (point)))
	   ;; (done (save-excursion
	   ;; 	   (if (re-search-forward (concat "\n\n\\\["
	   ;; 					  (regexp-quote todos-done-string))
	   ;; 				  nil t)
	   ;; 	       (match-beginning 0)
	   ;; 	     (point-max))))
	   )
      ;; (if (> (count-lines (point) done) 1)
      (if (save-excursion
	    ;; can only lower non-final unfinished item
	    (todos-forward-item)
	    (and (looking-at todos-item-start)
		 (not (todos-done-item-p))))
	  ;; Assume there is a final newline
	  (let ((item (todos-item-string)))
	    (todos-remove-item)
	    (todos-forward-item)
	    (when (todos-done-item-p) (forward-line -1))
	    (todos-insert-with-overlays item))
	(error "No TODO list entry to lower"))))) ;FIXME: better message

(defun todos-move-item ()
  "Move the current todo item to another, interactively named, category.

If the named category is not one of the current todo categories, then
it is created and the item becomes the first entry in that category."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let ((buffer-read-only)
	  (oldnum todos-category-number)
	  (oldcat (todos-current-category))
	  (item (todos-item-string))
	  (newcat (todos-read-category))
	  (opoint (point))
	  (orig-mrk (progn (todos-item-start) (point-marker)))
	  moved)
      (todos-remove-item)
      ;; numbered prefix isn't cached (see todos-remove-item) so have to update
      ;; (if todos-number-prefix (todos-update-numbered-prefix))
      (unwind-protect
	  (progn
	    ;; (todos-add-item-non-interactively item newcat)
	    (todos-set-item-priority item newcat)
	    (todos-insert-with-overlays item)
	    (setq moved t)
	    (todos-item-counts oldcat 'delete)
	    (todos-item-counts newcat 'insert))
	(unless moved
	  (widen)
	  (goto-char orig-mrk)
	  (todos-insert-with-overlays item)
	  (setq todos-category-number oldnum)
	  ;; (todos-item-counts oldcat 'move-failed)
	  ;; (todos-item-counts newcat 'move-failed)
	  (todos-category-select)
	  ;; FIXME: does this work?
	  (goto-char opoint))
	(set-marker orig-mrk nil)))))

(defun todos-item-done ()
  "Mark current item as done and move it to category's done section."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))
    (let* ((buffer-read-only)
	   (item (todos-item-string))
	   (date-string (calendar-date-string (calendar-current-date) t t))
	   (time-string (if todos-always-add-time-string ;FIXME: delete condition
			    (concat " " (substring (current-time-string) 11 16))
			  ""))
	   (done-item (concat "[" todos-done-string date-string time-string "] " item))
	   (items-end (point-max))
	   next-cat)
      (todos-remove-item)
      (save-excursion
	(widen)
	(setq next-cat
	      (save-excursion
		(if (re-search-forward (concat "^" (regexp-quote todos-category-beg))
				       nil t)
		    (match-beginning 0)
		  (point-max))))
	;; insert next done item at the top of the done items list
	(if (re-search-forward (concat "^\\[" (regexp-quote todos-done-string))
			       next-cat t)
	    (goto-char (match-beginning 0))
	  ;; need empty line between done and not done items in order not to have
	  ;; hanging todos-prefix when done items are hidden
	  (goto-char next-cat)
	  (newline))
	(todos-insert-with-overlays done-item)))
    (todos-item-counts (todos-current-category) 'done)
    (todos-show)))

(defun todos-archive-done-items ()
  "Archive the done items in the current category."
  (interactive)
  (let ((archive (find-file-noselect todos-archive-file t))
	(cat (todos-current-category))
	beg end)
    (save-excursion
      (save-restriction
	(widen)
	(re-search-forward (concat "^" (regexp-quote todos-category-beg)) nil t)
	(setq end (or (match-beginning 0) (point-max)))
	(re-search-backward (concat "^" (regexp-quote todos-category-beg)
				    (regexp-quote cat))
			    nil t)
	(if (not (re-search-forward (concat "^\\[" (regexp-quote todos-done-string))
				    nil t))
	    (error "No done items in this category")
	  (setq beg (match-beginning 0))
	  (setq done (buffer-substring beg end))
	  ;; FIXME: update archive alist
	  (with-current-buffer archive
	    (goto-char (point-min))
	    (if (re-search-forward (regexp-quote (concat "^" todos-category-beg cat))
				   nil t)
		(forward-char)
	      (insert todos-category-beg cat "\n"))
	    (insert done))
	  (delete-region beg end)
	  (remove-overlays beg end)
	  (kill-line -1)
	  (todos-item-counts cat 'archive)))))
  (message "Done items archived."))

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
      ;; (if todos-number-prefix (todos-update-numbered-prefix))
      (unwind-protect
	  (progn
	    ;; (todos-add-item-non-interactively item cat)
	    (todos-set-item-priority item cat)
	    (todos-insert-with-overlays item)
	    (setq undone t)
	    (todos-item-counts cat 'undo))
	(unless undone
	  (widen)
	  (goto-char orig-mrk)
	  (todos-insert-with-overlays done-item)
	  ;; (todos-item-counts cat 'done)
	  (let ((todos-show-with-done t))
	    (todos-category-select)
	    (goto-char opoint)))
	(set-marker orig-mrk nil)))))

(defun todos-toggle-item-diary-inclusion ()
  ""
  (interactive)
  (save-excursion
    (let* ((buffer-read-only)
	   (beg (todos-item-start))
	   (lim (save-excursion (todos-item-end)))
	   (end (save-excursion
		 (or (todos-time-string-match lim)
		     (todos-date-string-match lim)))))
      (if (looking-at "\\[") ; FIXME use todos-exclusion-start
	  (progn
	    (replace-match "")
	    (search-forward "]" (1+ end) t) ; FIXME use todos-exclusion-end
	    (replace-match ""))
	(when end
	  (insert "[") ; FIXME use todos-exclusion-start
	  (goto-char (1+ end))
	  (insert "]")))))) ; FIXME use todos-exclusion-end

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
	  (when (looking-at (regexp-quote todos-category-end)) (forward-line)))
	(while (not (eobp))
	  (todos-toggle-item-diary-inclusion)
	  (todos-forward-item))))))

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

;;;###autoload
(defun todos-print (&optional category-pr-page)
  "Print todo summary using `todos-print-function'.
If CATEGORY-PR-PAGE is non-nil, a page separator `^L' is inserted
between each category.

Number of entries for each category is given by `todos-print-priorities'."
  (interactive "P")
  (when (yes-or-no-p "Print Todos list? ")
    (save-window-excursion
      (save-excursion
	(save-restriction
	  (todos-top-priorities todos-print-priorities
				category-pr-page)
	  (set-buffer todos-tmp-buffer-name)
	  (and (funcall todos-print-function)
	       (kill-this-buffer))
	  (message "Todo printing done."))))))

;; ---------------------------------------------------------------------------

;;; Internal functions

(defvar todos-date-pattern
  (let ((dayname (diary-name-pattern calendar-day-name-array nil t)))
    (concat "\\(" dayname "\\|"
	    (let ((dayname)
		  (monthname (format "\\(%s\\|\\*\\)"
				     (diary-name-pattern calendar-month-name-array
							 calendar-month-abbrev-array
							 t)))
		  (month "\\([0-9]+\\|\\*\\)")
		  (day "\\([0-9]+\\|\\*\\)")
		  (year "-?\\([0-9]+\\|\\*\\)"))
	      (mapconcat 'eval calendar-date-display-form ""))
	    "\\)"))
  "Regular expression matching a Todos date header.")

(defun todos-date-string-match (lim)
  "Find Todos date strings within LIM for font-locking."
  (re-search-forward (concat "^\\[?" todos-date-pattern) lim t))

(defun todos-time-string-match (lim)
  "Find Todos time strings within LIM for font-locking."
  (re-search-forward (concat "^\\[?" todos-date-pattern
			       " \\(?1:" diary-time-regexp "\\)") lim t))

(defun todos-done-string-match (lim)
  "Find Todos done headers within LIM for font-locking."
  (re-search-forward (concat "^\\[" (regexp-quote todos-done-string) "[^][]+]")
		       lim t))

(defun todos-category-string-match (lim)
  "Find Todos category headers within LIM for font-locking."
  (re-search-forward (concat "^" (regexp-quote todos-category-beg) ".*$")
		       lim t))

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
  ;; (setq wrap-prefix (make-string (+ 5 (length todos-prefix)) 32))
  (setq wrap-prefix (make-string todos-indent-to-here 32))
  (unless (member '(continuation) fringe-indicator-alist)
    (push '(continuation) fringe-indicator-alist)))

(defun todos-indent ()
  ""
  (indent-to todos-indent-to-here todos-indent-to-here))

(defun todos-reset-prefix (symbol value)
  "Set SYMBOL's value to VALUE, and ."	; FIXME
  (let ((oldvalue (symbol-value symbol)))
    (custom-set-default symbol value)
    (when (not (equal value oldvalue))
      (save-window-excursion
	(todos-show)
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (remove-overlays (point) (point)); 'before-string prefix)
	    (forward-line)))
	;; activate the prefix setting (save-restriction does not help)
	;; (todos-show)
	(todos-category-select)
	))))

;; FIXME: ??? with todos-lower-item leaves overlay of lower item if this is
;; the third or greater item number -- but not in edebug
;; (defun todos-update-numbered-prefix ()
;;   "Update consecutive item numbering in the current category."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (not (eobp))
;;       (let ((ov (car (overlays-in (point) (point))))
;; 	     val)
;; 	(when ov
;; 	  (setq val (overlay-get ov 'before-string))
;; 	  (remove-overlays (point) (point) 'before-string val)))
;;       (todos-forward-item))
;;     (todos-show)))

;; (defun todos-update-numbered-prefix ()
;;   "Update consecutive item numbering in the current category."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (not (eobp))
;;       (remove-overlays (point) (point))
;;       (todos-forward-item))
;;     ;; FIXME: is todos-prefix-overlays enough?
;;     (todos-show)))

;; (defvar todos-item-start-overlays nil "")

;; (defvar todos-done-overlays nil "")

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
	      (when ;; (or
		   ;; ;; FIXME: really need this?
		   ;; (looking-at (concat "\n\\[" (regexp-quote todos-done-string)))
		  ;; if last not done item is multiline, then
		  ;; todos-done-string-match skips empty line, so have
		  ;; to look back.
		  (and (looking-at (concat "^\\[" (regexp-quote todos-done-string)))
		       (looking-back "\n\n"));)
		  (setq num 1))
	      (setq prefix (propertize (concat (number-to-string num) " ")
				       'face 'todos-prefix-string)))
	    ;; (let ((ovs (overlays-in (point) (point))))
	    ;;   (or (and (setq ov-pref (car ovs))
	    ;; 	       ;; when done-separator overlay is in front of prefix overlay
	    ;; 	       (if (and (> (length ovs) 1)
	    ;; 			(not (equal (overlay-get ov-pref 'before-string)
	    ;; 				    prefix)))
	    ;; 	       	   (setq ov-pref (cadr ovs))
	    ;; 		 t)
	    ;; 	       (equal (overlay-get ov-pref 'before-string) prefix))
	    ;; 	  ;; non-numerical prefix
	    ;; 	  (and (setq ov-pref (pop todos-item-start-overlays))
	    ;; 	       (move-overlay ov-pref (point) (point)))
	    ;; 	  (and (setq ov-pref (make-overlay (point) (point)))
	    ;; 	       (overlay-put ov-pref 'before-string prefix))))
	    (let* ((ovs (overlays-in (point) (point)))
		   (ov-pref (car ovs))
		   (val (when ov-pref (overlay-get ov-pref 'before-string))))
	      (when (and (> (length ovs) 1)
			 (not (equal val prefix)))
		(setq ov-pref (cadr ovs)))
	      (when (not (equal val prefix))
		;; (delete-overlay ov-pref)
		(remove-overlays (point) (point)); 'before-string val)
		(setq ov-pref (make-overlay (point) (point)))
		(overlay-put ov-pref 'before-string prefix))))
	(forward-line))))))

(defun todos-reset-separator (symbol value)
  "Set SYMBOL's value to VALUE, and ."	; FIXME
  (let ((oldvalue (symbol-value symbol)))
    (custom-set-default symbol value)
    ;; (setq todos-done-overlays nil)
    (when (not (equal value oldvalue))
      (save-window-excursion
	(todos-show)
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward (concat "^\\[" (regexp-quote todos-done-string))
				   nil t)
	    (remove-overlays (point) (point))))
	;; activate the prefix setting (save-restriction does not help)
	(todos-show)))))

;; FIXME: should be defsubst?
(defun todos-category-number (cat)
  "Set todos-category-number to index of CAT in todos-categories."
  (setq todos-category-number (- (length todos-categories)
				 (length (member cat todos-categories)))))
(defun todos-current-category ()
  "Return the name of the current category."
  (nth todos-category-number todos-categories))

(defun todos-category-select ()
  "Make TODO mode display the current category correctly."
  (let ((name (todos-current-category)))
    (setq mode-line-buffer-identification (concat "Category: " name))
    (widen)
    (goto-char (point-min))
    (search-forward-regexp
     (concat "^" (regexp-quote (concat todos-category-beg name))
             "$"))
    (let ((begin (1+ (line-end-position)))
	  (end (or (and (re-search-forward (concat "^" todos-category-beg) nil t)
			(match-beginning 0))
		   (point-max))))
      (narrow-to-region begin end)
      (goto-char (point-min))))
  (todos-prefix-overlays)
  ;; display or hide done items as per todos-show-with-done
  (save-excursion
    (when (re-search-forward (concat "\n\\(\\[" (regexp-quote todos-done-string)
				     "\\)") nil t)
      (let (done end done-sep prefix ov-pref ov-done)
	(setq done (match-beginning 1)
	      end  (match-beginning 0))
	(if todos-show-with-done
	    (progn
	      (setq done-sep todos-done-separator)
	      (unless (string-match "^[[:space:]]*$" todos-done-separator)
		(setq done-sep (propertize (concat todos-done-separator "\n")
					   'face 'todos-done-sep))
		(setq prefix (propertize
			      (concat (if todos-number-prefix "1" todos-prefix) " ")
			      'face 'todos-prefix-string))
		;; FIXME? Just deleting done-sep overlay results in bad
		;; display (except when stepping though in edebug)
		(remove-overlays done done)
		;; must make separator overlay after making prefix overlay to get
		;; the order separator before prefix
		(setq ov-pref (make-overlay done done)
		      ov-done (make-overlay done done))
		(overlay-put ov-pref 'before-string prefix)
		(overlay-put ov-done 'before-string done-sep)))
	  (narrow-to-region (point-min) end))))))

;; FIXME: why autoload?
;;;###autoload
;; (defun todos-add-item-non-interactively (item category)
;;   "Insert item ITEM into category CATEGORY and set its priority."
;;   (todos-category-number category)
;;   (todos-show)			; now at point-min
;;   (unless (or (eq (point-min) (point-max)) ; no unfinished items
;; 	      (when (re-search-forward (concat "^\\["
;; 					       (regexp-quote todos-done-string))
;; 				       nil t)
;; 		(forward-line -1)
;; 		(bobp))) ; there are done items but no unfinished items
;;     (let* ((maxnum (1+ (car (todos-item-counts category))))
;; 	   priority candidate prompt)
;;       (while (null priority)
;; 	(setq candidate
;; 	      (string-to-number (read-from-minibuffer
;; 				 (concat prompt
;; 					 (format "Set item priority (1-%d): "
;; 						 maxnum)))))
;; 	(setq prompt
;; 	      (when (or (< candidate 1) (> candidate maxnum))
;; 		(format "Priority must be an integer between 1 and %d.\n" maxnum)))
;; 	(unless prompt (setq priority candidate)))
;;       (goto-char (point-min))
;;       (unless (= priority 1) (todos-forward-item (1- priority)))))
;;   (todos-insert-with-overlays item))

(defun todos-set-item-priority (item cat)
  "Set the priority of unfinished item ITEM in category CAT."
  (todos-category-number cat)
  (todos-category-select)
  (let* ((not-done (car (todos-item-counts cat)))
	 (maxnum (1+ not-done))
	 priority candidate prompt)
    (unless (zerop not-done)
      (while (null priority)
	(setq candidate
	      (string-to-number (read-from-minibuffer
				 (concat prompt
					 (format "Set item priority (1-%d): "
						 maxnum)))))
	(setq prompt
	      (when (or (< candidate 1) (> candidate maxnum))
		(format "Priority must be an integer between 1 and %d.\n" maxnum)))
	(unless prompt (setq priority candidate)))
      (goto-char (point-min))
      (unless (= priority 1) (todos-forward-item (1- priority))))))

(defun todos-jump-to-category-noninteractively (cat)
  ""
  (let ((bufname (buffer-name)))
    (cond ((string= bufname todos-categories-buffer)
	   (switch-to-buffer (file-name-nondirectory todos-file-do)))
	  ((string= bufname todos-archived-categories-buffer)
	   ;; FIXME: is pop-to-buffer better for this case?
	   (switch-to-buffer (file-name-nondirectory todos-archive-file))))
    (kill-buffer bufname))
  (widen)
  (goto-char (point-min))
  (todos-category-number cat)
  (todos-category-select))

(defun todos-insert-with-overlays (item)
  ""
  (todos-item-start)
  (insert item "\n")
  (todos-backward-item)
  ;; (if todos-number-prefix
  ;;     (todos-update-numbered-prefix)
    (todos-prefix-overlays));)

(defun todos-item-string-start ()
  "Return the start of this TODO list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (todos-item-string)))
    (if (> (length item) 60)
        (setq item (concat (substring item 0 56) "...")))
    item))

(defvar todos-item-start (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
				 "\\)?\\)?" todos-date-pattern)
  "String identifying start of a Todos item.")

(defun todos-item-start ()
  "Move to start of current TODO list item and return its position."
  (unless (or (looking-at "^$")		; last item or between done and not done
	      (looking-at (regexp-quote todos-category-beg))) ; for todos-count-items
    (goto-char (line-beginning-position))
    (while (not (looking-at todos-item-start))
      (forward-line -1)))
  (point))

(defun todos-item-end ()
  "Move to end of current TODO list item and return its position."
  (unless (looking-at "^$")		; FIXME:
    (let ((done (todos-done-item-p)))
      (todos-forward-item)
      ;; adjust if item is last unfinished one before displayed done items
      (when (and (not done) (todos-done-item-p))
	(forward-line -1))
      (backward-char)))
  (point))

(defun todos-remove-item ()
  "Delete the current entry from the TODO list."
  (let* ((beg (todos-item-start))
	 (end (progn (todos-item-end) (1+ (point))))
	 (ov-start (car (overlays-in beg beg))))
    (when ov-start
      ;; ;; don't cache numbers, since they can be popped out of order in
      ;; ;; todos-prefix-overlays
      ;; (unless todos-number-prefix
      ;; 	(push ov-start todos-item-start-overlays))
      (delete-overlay ov-start))
    (delete-region beg end)))

(defun todos-item-string ()
  "Return current TODO list entry as a string."
  (buffer-substring (todos-item-start) (todos-item-end)))

(defun todos-done-item-p ()
  ""
  (save-excursion
    (todos-item-start)
    (looking-at (concat "^\\[" (regexp-quote todos-done-string)))))


(defvar todos-categories-alist nil
  "Variable for storing the result of todos-make-categories-alist.")
(defun todos-make-categories-alist ()
  "Return an alist of categories and some of their properties.
The properties are at least the numbers of the unfinished and
done items in the category."
  (let (todos-categories-alist)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((not-done 0)
	      (done 0)
	      category beg end)
	  (while (not (eobp))
	    (cond ((looking-at (concat (regexp-quote todos-category-beg)
				       "\\(.*\\)\n"))
		   (setq not-done 0 done 0)
		   (push (list (match-string-no-properties 1) (cons not-done done))
			 todos-categories-alist))
		  ((looking-at (concat "^\\[" (regexp-quote todos-done-string)))
		   (setq done (1+ done))
		   (setcdr (cadr (car todos-categories-alist)) done))
		  ((looking-at (concat "^\\[?" todos-date-pattern))
		   (setq not-done (1+ not-done))
		   (setcar (cadr (car todos-categories-alist)) not-done)))
	    (forward-line)))))
    todos-categories-alist))

(defun todos-item-counts (cat &optional how)
  ""
  (let* ((counts (cadr (assoc cat todos-categories-alist)))
	 (not-done (car counts))
	 (done (cdr counts)))
    (cond ((eq how 'insert)
	   (setcar counts (1+ not-done)))
	  ((eq how 'delete)
	   (if (todos-done-item-p)	;FIXME: fails if last done item was deleted
	       (setcdr counts (1- done))
	     (setcar counts (1- not-done))))
	  ;; ((eq how 'move-failed)
	  ;;  (setcar counts not-done))
	  ((eq how 'done)
	   (setcar counts (1- not-done))
	   (setcdr counts (1+ done)))
	  ((eq how 'undo)
	   (setcar counts (1+ not-done))
	   (setcdr counts (1- done)))
	  ((eq how 'archive)
	   (setcdr counts 0))
	  (t
	   (cons not-done done)))))

(defun todos-longest-category-name-length (categories)
  ""
  (let ((longest 0))
    (dolist (c categories longest)
      (setq longest (max longest (length (car c)))))))

(defun todos-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todos-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (todos-string-count-lines string) 1))

(defun todos-read-category ()
  "Return an existing category name, with tab completion."
  ;; allow SPC to insert spaces, for adding new category names with
  ;; todos-move-item
  (let ((map minibuffer-local-completion-map))
    (define-key map " " nil)
    ;; make a copy of todos-categories in case history-delete-duplicates is
    ;; non-nil, which makes completing-read alter todos-categories
    (let* ((categories (copy-sequence todos-categories))
	   (history (cons 'todos-categories (1+ todos-category-number)))
	   (default (todos-current-category)) ;FIXME: why this default?
	   (completion-ignore-case todos-completion-ignore-case)
	   (category (completing-read
		      (concat "Category [" default "]: ")
		      todos-categories nil nil nil history default)))
      ;; restore the original value of todos-categories
      (setq todos-categories categories)
      category)))

(defun todos-check-category-name (cat)
  "Reject names for category CAT that could yield bugs or confusion."
  (let (prompt)
    (while (and (cond ((string= "" cat)
		       (setq prompt "Enter a non-empty category name: "))
		      ((string-match "\\`\\s-+\\'" cat)
		       (setq prompt "Enter a category name that is not only white space: "))
		      ((member cat todos-categories)
		       (setq prompt "Enter a non-existing category name: ")))
		(setq cat (read-from-minibuffer prompt)))))
  cat)

;; adapted from calendar-read-date
(defun todos-read-date ()
  "Prompt for Gregorian date and return it in the current format."
  (let* ((year (calendar-read
                "Year (>0): "
                (lambda (x) (> x 0))
                (number-to-string (calendar-extract-year
				   (calendar-current-date)))))
         (month-array calendar-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc-string
		      (completing-read
		       "Month name (RET for current month): "
		       (mapcar 'list (append month-array nil))
		       nil t nil nil
		       (calendar-month-name (calendar-extract-month
					     (calendar-current-date))))
                      (calendar-make-alist month-array 1) t)))
         (last (calendar-last-day-of-month month year))
	 day)
    (while (or (not (numberp day)) (< day 0) (< last day))
      (setq day (read-from-minibuffer
		 (format "Day (1-%d): " last) nil nil t nil
		 (number-to-string (calendar-extract-day (calendar-current-date))))))
    (calendar-date-string (list month day year) t t)))

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
		    "Enter a clock time: "))
      (when (or (string= "" answer)
		(string-match diary-time-regexp answer))
	(setq valid t)))
    answer))

(defun todos-categories-list (buf)
  "Return a list of the Todo mode categories in buffer BUF."
  (let (categories)
    (with-current-buffer buf
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-max))
	  (while (re-search-backward (concat "^" (regexp-quote todos-category-beg)
					     "\\(.*\\)\n") nil t)
	    (push (match-string-no-properties 1) categories)))))
    categories))

(defun todos-padded-string (str)
  ""
  (let* ((len (todos-longest-category-name-length todos-categories-alist))
	 (strlen (length str))
	 (strlen-odd (eq (logand strlen 1) 1)) ; oddp from cl.el
	 (padding (/ (- len strlen) 2)))
    (concat (make-string padding 32) str
	    (make-string (if strlen-odd (1+ padding) padding) 32))))  

(defun todos-insert-category-name (cat &optional nonum)
  ""
  (let* ((buf (get-buffer (file-name-nondirectory todos-file-do)))
	 (cat-alist todos-categories-alist)
	 (counts (todos-item-counts cat)))
    ;; num is declared in caller
    (setq num (1+ num))
    (if nonum
	(insert (make-string 4 32))
      (insert " " (format "%2d" num) " "))
    (insert-button (todos-padded-string cat)
		   'face 'todos-button
		   'action
		   `(lambda (button)
		      (todos-jump-to-category-noninteractively ,cat)))
    (insert (make-string 8 32)
	    (format "%2d" (car counts))
	    (make-string 5 32)
	    (format "%2d" (cdr counts)))
    (newline)))

(defun todos-initial-setup ()
  "Set up things to work properly in TODO mode."
  (find-file todos-file-do)
  (erase-buffer)
  (todos-mode)
  (todos-add-category "Todos"))

(provide 'todos)

;;; todos.el ends here
