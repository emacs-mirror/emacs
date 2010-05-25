;;; todos.el --- major mode for editing TODO list files

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
;;      To get this to work, make emacs execute the line
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
;;	o   rewrite complete package to store data as lisp objects
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
  
;;   "TODO mode prefix for entries.

;; This is useful in conjunction with `calendar' and `diary' if you use

;; #include \"~/.todos-do\"

;; in your diary file to include your todo list file as part of your
;; diary.  With the default value \"*/*\" the diary displays each entry
;; every day and it may also be marked on every day of the calendar.
;; Using \"&%%(equal (calendar-current-date) date)\" instead will only
;; show and mark todo entries for today, but may slow down processing of
;; the diary file somewhat."
;;   :type 'string
;;   :group 'todos)

(defcustom todos-number-prefix t
  "Non-nil to show item prefixes as consecutively increasing integers."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

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

;; (defcustom todos-file-done (convert-standard-filename "~/.emacs.d/.todos-done")
;;   "TODO mode archive file."
;;   :type 'file
;;   :group 'todos)

(defcustom todos-mode-hook nil
  "TODO mode hooks."
  :type 'hook
  :group 'todos)

(defcustom todos-edit-mode-hook nil
  "TODO Edit mode hooks."
  :type 'hook
  :group 'todos)

;; (defcustom todos-insert-threshold 0
;;   "TODO mode insertion accuracy.

;; If you have 8 items in your TODO list, then you may get asked 4
;; questions by the binary insertion algorithm.  However, you may not
;; really have a need for such accurate priorities amongst your TODO
;; items.  If you now think about the binary insertion halving the size
;; of the window each time, then the threshold is the window size at
;; which it will stop.  If you set the threshold to zero, the upper and
;; lower bound will coincide at the end of the loop and you will insert
;; your item just before that point.  If you set the threshold to,
;; e.g. 8, it will stop as soon as the window size drops below that
;; amount and will insert the item in the approximate center of that
;; window."
;;   :type 'integer
;;   :group 'todos)

(defcustom todos-categories-buffer "*TODOS Categories*"
  "Name of buffer displayed by `todos-display-categories'"
  :type 'string
  :group 'todos)

(defcustom todos-archived-categories-buffer "*TODOS Archived Categories*"
  "Name of buffer displayed by `todos-display-categories'"
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
  "String appended to item date to match todos-exclusion-start."
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
;; (defcustom todos-remove-separator t
;;   "Non-nil to remove category separators in\
;; \\[todos-top-priorities] and \\[todos-print]."
;;   :type 'boolean
;;   :group 'todos)

(defcustom todos-save-top-priorities-too t
  "Non-nil makes `todos-save' automatically save top-priorities in `todos-file-top'."
  :type 'boolean
  :group 'todos)

(defcustom todos-completion-ignore-case t ;; FIXME: nil for release
  "Non-nil means don't consider case significant in todos-completing-read."
  :type 'boolean
  :group 'todos)

(defcustom todos-add-time-string t
  "Add current time to date string inserted in front of new items."
  :type 'boolean
  :group 'todos)

(defcustom todos-wrap-lines t
  ""					;FIXME
  :group 'todos
  :type 'boolean)

(defcustom todos-line-wrapping-function 'todos-wrap-and-indent
  ""					;FIXME
  :group 'todos
  :type 'function)

;; ---------------------------------------------------------------------------

;;; Faces
(defface todos-prefix-string
  '((t
     :inherit font-lock-constant-face
     ))
  "Face for Todos prefix string."
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

;;; Internal variables
(defvar todos-categories nil
  "TODO categories.")

(defvar todos-previous-line 0
  "Previous line asked about.")

(defvar todos-previous-answer 0
  "Previous answer got.")

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
    (define-key map "ia" 'todos-insert-item-ask-date)
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
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-q" 'todos-edit-quit)
    map)
  "Todos Edit mode keymap.")

(defvar todos-category-number 0 "TODO category number.")

(defvar todos-tmp-buffer-name " *todo tmp*")

(defvar todos-category-beg "--==-- "
  "Category start separator to be prepended onto category name.")

;; ---------------------------------------------------------------------------

;;; Commands

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
  (let ((category (todos-completing-read)))
    (if (string= "" category)
        (setq category (nth todos-category-number todos-categories)))
    (setq todos-category-number
          (if (member category todos-categories)
              (- (length todos-categories)
                 (length (member category todos-categories)))
            (todos-add-category category)))
    ;; (todos-show)))
    (todos-category-select)))

;; FIXME ? todos-{backward,forward}-item skip over empty line between done and
;; not done items (but todos-forward-item gets there when done items are not
;; displayed)
(defun todos-backward-item (&optional count)
  "Select previous entry of TODO list."
  (interactive "P")
  ;; FIXME ? this moves to bob if on the first item (but so does previous-line)
  (todos-item-start)
  (unless (bobp)
    (re-search-backward (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
				"\\)?\\)?\\(" todos-date-pattern "\\)")
			nil t (or count 1))))

(defun todos-forward-item (&optional count)
  "Select COUNT-th next entry of TODO list."
  (interactive "P")
  (goto-char (line-end-position))
  (if (re-search-forward (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
				 "\\)?\\)?\\(" todos-date-pattern "\\)")
			 nil t (or count 1))
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

;; (defun todos-forward-item (&optional count)
;;   "Select COUNT-th next entry of TODO list."
;;   (interactive "P")
;;   (let ((opoint (point))
;; 	(done (save-excursion
;; 		(if (re-search-forward (concat "\n\n\\\(\\["
;; 					       (regexp-quote todos-done-string)
;; 					       "\\)") nil t)
;; 		    (match-beginning 1)))))
;;     ;; FIXME: can this be simplified?
;;     (if (looking-at (concat "^\\(\\[\\(" (regexp-quote todos-done-string) "\\)?\\)?"
;; 			    todos-date-pattern)) ; on item header
;; 	(re-search-forward (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
;; 				   "\\)?\\)?\\(" todos-date-pattern "\\)")
;; 			   nil t (if count (1+ count) 2))
;;       (re-search-forward (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
;; 				 "\\)?\\)?\\(" todos-date-pattern "\\)")
;; 			 nil t (or count 1)))
;;     (cond ((save-excursion
;; 	     (goto-char opoint)
;; 	     (looking-at "^$"))		; between done and not done items
;; 	   (forward-line 0))
;; 	  ((and done (> (point) done))
;; 	   (forward-line -1))		; FIXME: count ?
;; 	  ((eq (point) opoint)		; on last item
;; 	   (goto-char (point-max)))
;; 	  (t 
;; 	   (goto-char (match-beginning 0))))))

(defvar todos-search-string nil
  ""					;FIXME
  )
(defun todos-search ()
  ""					;FIXME
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
	  (setq todos-category-number
		(- (length todos-categories) (length (member cat todos-categories))))
	  (todos-category-select)
	  (when in-done	(unless todos-show-with-done (todos-toggle-view-done-items)))
	  (goto-char found))
      (todos-category-select)
      (goto-char start)
      (message "No match for \"%s\"" regex))))

;;; Display

(defun todos-display-categories ()
  "Display an alphabetical list of clickable Todos category names.
Click or type RET on a category name to go to it."
  (interactive)
  (let ((categories (copy-sequence todos-categories))
	(cat-alist (todos-categories-alist))
	(len (todos-longest-category-name-length))
	beg)
    ;; alphabetize the list case insensitively
    (setq categories (sort categories (lambda (s1 s2) (let ((cis1 (upcase s1))
							    (cis2 (upcase s2)))
							(string< cis1 cis2)))))
    (with-current-buffer (get-buffer-create todos-categories-buffer)
      (switch-to-buffer (current-buffer))
      (erase-buffer)
      (kill-all-local-variables)
      (insert "Press a button to display the corresponding category.\n\n")
      (setq beg (point))
      (mapc (lambda (cat)
	      (let* ((catlen (length cat))
		     (catlen-odd (eq (logand catlen 1) 1)) ; oddp from cl.el
		     (padding (/ (- len catlen) 2)))
		(insert-button (concat (make-string padding 32) cat
				       (make-string (if catlen-odd
							(1+ padding)
						      padding)
						    32))
			       'face 'tool-bar
			       'action
			       `(lambda (button)
				  (todos-jump-to-category-noninteractively ,cat)))
		(insert (make-string 8 32)
			"(not done: "
			(number-to-string (car (cadr (assoc cat cat-alist))))
			", done: "
			(number-to-string (cdr (cadr (assoc cat cat-alist))))
			")")
		(newline)))
	    categories))))
    ;; (require 'widget)
    ;; (eval-when-compile
    ;;   (require 'wid-edit))
    ;; (with-current-buffer (get-buffer-create todos-categories-buffer)
    ;;   (switch-to-buffer (current-buffer))
    ;;   (erase-buffer)
    ;;   (kill-all-local-variables)
    ;;   (widget-insert "Press a button to display the corresponding category.\n\n")
    ;;   (setq beg (point))
    ;;   (mapc (lambda (cat)
    ;; 	      (widget-create 'push-button
    ;; 			     :notify (lambda (widget &rest ignore)
    ;; 				       (todos-jump-to-category-noninteractively
    ;; 					(widget-get widget :value)))
			     
    ;; 			     cat)
    ;; 	      (widget-insert " (not done: "
    ;; 			     (number-to-string (car (cadr (assoc cat cat-alist))))
    ;; 			     ", done: "
    ;; 			     (number-to-string (cdr (cadr (assoc cat cat-alist))))
    ;; 			     ")\n"))
    ;; 	    categories)
    ;;   (use-local-map widget-keymap)
    ;;   (widget-setup))))

(defun todos-toggle-item-numbering ()
  ""					;FIXME
  (interactive)
  (todos-reset-prefix 'todos-number-prefix (not todos-number-prefix)))

(defun todos-toggle-view-done-items ()
  ""					; FIXME
  (interactive)
  (let ((beg (point-min))
	(done-sep (if (string-match "^[[:space:]]*$" todos-done-separator)
		      todos-done-separator
		    (propertize (concat todos-done-separator "\n")
				'face 'todos-done-sep)))
	(todos-show-with-done nil)
	(done (point-max))
	end ov)
    (save-excursion
      (goto-char beg)
      (if (re-search-forward (concat "\n\\[" (regexp-quote todos-done-string))
			     nil t)
	  ;; hide done items
	  (progn (setq end (match-beginning 0))
		 (narrow-to-region beg end))
	(widen)
	(re-search-forward (concat "^" (regexp-quote todos-category-beg)) nil t)
	(setq end (or (match-beginning 0) (point-max)))
	(goto-char beg)
	(if (re-search-forward
	     (concat (if (eq beg done) "" "\n") ; no newline if no unfinished items
		     "\n\\(\\[" (regexp-quote todos-done-string) "\\)")
	     end t)
	    ;; show done items
	    (let ((prefix (propertize
			   (concat (if todos-number-prefix "1" todos-prefix) " ")
			   'face 'todos-prefix-string))
		  ov-done ov-pref)
	      (setq done (match-beginning 1))
	      (narrow-to-region beg end)
	      (todos-prefix-overlays)
	      ;; add non-empty separator overlay in front of prefix overlay on
	      ;; first done item
	      (unless (string= done-sep todos-done-separator)
		(goto-char done)
		(remove-overlays done done)
		;; must make separator overlay after making prefix overlay to
		;; get the order separator before prefix
		(setq ov-pref (make-overlay done done)
		      ov-done (make-overlay done done))
		(overlay-put ov-pref 'before-string prefix)
		(overlay-put ov-done 'before-string done-sep)))
	  ;; (when (setq ov (car (overlays-in done done)))
	  ;;   (when (equal (overlay-get ov 'before-string) done-sep)
	  ;;     (push ov todos-done-overlays)
	  ;;     (delete-overlay ov)))
	  (todos-category-select)
	  (error "No done items in this category"))))))

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

;; FIXME: very slow
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
  ""					; FIXME
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
				     " \\(" diary-time-regexp "\\)?\\]? ")
					; FIXME: this space in header? ^
			     nil t)
	  (setq ov (make-overlay (match-beginning 0) (match-end 0) nil t))
	  (overlay-put ov 'display "")
	  (forward-line)))
      (todos-update-numbered-prefix))))
      
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

;;; Editing

;;;###autoload
(defun todos-add-category (&optional cat)
  "Add new category CAT to the TODO list."
  (interactive)
  (let ((buffer-read-only)
	(buf (find-file-noselect todos-file-do t))
	(prompt "Category: "))
    (unless (zerop (buffer-size buf))
      (and (null todos-categories)
	   (error "Error in %s: File is non-empty but contains no category" 
		  todos-file-do)))
    (unless cat (setq cat (read-from-minibuffer prompt)))
    (with-current-buffer buf
      ;; reject names that could induce bugs and confusion
      (while (and (cond ((string= "" cat)
			 (setq prompt "Enter a non-empty category name: "))
			((string-match "\\`\\s-+\\'" cat)
			 (setq prompt "Enter a category name that is not only white space: "))
			((member cat todos-categories)
			 (setq prompt "Enter a non-existing category name: ")))
		  (setq cat (read-from-minibuffer prompt))))
      ;; initialize a newly created Todo buffer for Todo mode
      (unless (file-exists-p todos-file-do) (todos-mode))
      (setq todos-categories (cons cat todos-categories))
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

;; FIXME: use function for category name choice here and in todos-add-category
(defun todos-rename-category ()
  "Rename current Todos category."
  (interactive)
  (let* ((buffer-read-only)
	 (cat (nth todos-category-number todos-categories))
	 (vec (vconcat todos-categories))
	 (new (read-from-minibuffer (format "Rename category \"%s\" to: " cat)))
	 prompt)
    (while (and (cond ((string= "" new)
		       (setq prompt "Enter a non-empty category name: "))
		      ((string-match "\\`\\s-+\\'" new)
		       (setq prompt "Enter a category name that is not only white space: "))
		      ((member new todos-categories)
		       (setq prompt "Enter a non-existing category name: ")))
		(setq new (read-from-minibuffer prompt))))
      (aset vec todos-category-number new)
    (setq todos-categories (append vec nil))
    (save-excursion
      (widen)
      (re-search-backward (concat (regexp-quote todos-category-beg) "\\("
				  (regexp-quote cat) "\\)\n") nil t)
      (replace-match new t t nil 1)
      (goto-char (point-min))
      (setq mode-line-buffer-identification
	    (concat "Category:  " new))))
;;	    (concat "Category: " (format "%18s" new)))))
  (todos-category-select))

(defun todos-delete-category (&optional arg)
  "Delete current Todos category provided it is empty.
With ARG non-nil delete the category unconditionally,
i.e. including all existing entries."
  (interactive "P")
  (if (and (null arg)
	   ;; FIXME: what about done items?
	   (not (eq (point-max) (point-min))))
      (message "To delete a non-empty category, call the command with a prefix argument.")
    (let ((cat (nth todos-category-number todos-categories)) beg end)
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
	  (todos-category-select)
	  (message "Deleted category %s" cat))))))

;;;###autoload
(defun todos-insert-item (&optional arg here date-time) ; FIXME revise docstring
  "Insert new TODO list item.

With prefix argument ARG solicit the category, otherwise use the
current category.

With non-nil argument HERE insert the new item directly above the
item at point.  If point is on an empty line, insert the new item
there.

If the value of TIME is `omit', insert the item without a time
string; with the value `ask', solicit a time string; with any
other value, add or omit the current time in accordance with
`todos-add-time-string'."
  (interactive "P")
  (unless (or (todos-done-item-p)
	      (save-excursion (forward-line -1) (todos-done-item-p)))
    (if (not (derived-mode-p 'todos-mode)) (todos-show))
    (let* ((buffer-read-only)
	   (date-string (cond ;; ((eq date-time 'omit) "")
			 ((eq date-time 'ask)
			  (read-from-minibuffer "Enter a date: "))
			 ((eq date-time 'to-date)
			  (with-current-buffer "*Calendar*"
			    (calendar-date-string (calendar-cursor-to-date t) t t)))
			 (t (calendar-date-string (calendar-current-date) t t))))
	   (time-string (if todos-add-time-string
			    (cond ((eq date-time 'omit) "")
				  ((eq date-time 'ask)
				   (read-from-minibuffer "Enter a clock time: "))
				  (t (substring (current-time-string) 11 16)))
			  ""))
	   (new-item (concat (unless todos-include-in-diary "[")
			     date-string (unless (string= time-string "")
					   (concat " " time-string))
			     (unless todos-include-in-diary "]") " "
			     (read-from-minibuffer "New TODO entry: ")))
	   (current-category (nth todos-category-number todos-categories))
	   (category (if arg (todos-completing-read) current-category)))
      (if here
	  (todos-insert-with-overlays new-item)
	(todos-add-item-non-interactively new-item category)))))

(defun todos-insert-item-here (&optional date-time)
  ""					;FIXME add docstring
  (interactive)
  (todos-insert-item nil t date-time))

(defun todos-insert-item-no-time (&optional here)
  ""					;FIXME add docstring
  (interactive)
  (todos-insert-item nil here 'omit))

(defun todos-insert-item-ask-date (&optional here)
  ""					;FIXME add docstring
  (interactive)
  (todos-insert-item nil here 'ask))

(defun todos-insert-item-for-diary (&optional arg here date-time)
  ""					;FIXME
  (interactive "P")
  (let ((todos-include-in-diary t))
    (todos-insert-item arg here date-time)))

;; FIXME: autoload when key-binding is defined in calendar.el
(defun todos-insert-item-from-calendar ()
  ""					;FIXME
  (interactive)
  (pop-to-buffer (file-name-nondirectory todos-file-do))
  (todos-show)
  (todos-insert-item t nil 'to-date))

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
	  ;; FIXME: is todos-prefix-overlays part of if-sexp, and is it needed
	  ;; at all?
	  (if todos-number-prefix
	    (todos-update-numbered-prefix)
	  (todos-prefix-overlays))))
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
  ""					;FIXME
  (interactive)
  (save-excursion (todos-category-select)))

;; FIXME
(defun todos-change-date (&optional event)
  ""					;FIXME
  (interactive)
  (let (dmarker
	calendar-view-diary-initially-flag
	new-date)
    (save-excursion
      (todos-item-start)
      (setq dmarker (point-marker)))
    (calendar)
    (message "Put the cursor on the desired date in the Calendar and press `q'")
    (setq new-date
	  (calendar-date-string (calendar-cursor-to-date t) t t))
    ;; (pop-to-buffer (file-name-nondirectory todos-file-do))
    ;; (todos-show)
    (when (eq last-command 'calendar-exit)
      (goto-char (marker-position dmarker))
      (re-search-forward (concat "^\\[?\\(" todos-date-pattern "\\)\\]?")
			 (line-end-position) t)
      (replace-match new-date nil nil nil 1))))

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
    (let ((buffer-read-only)
	  (done (save-excursion
		  (if (re-search-forward (concat "\n\n\\\["
						 (regexp-quote todos-done-string))
					 nil t)
		      (match-beginning 0)
		    (point-max)))))
      (if (> (count-lines (point) done) 1)
	  ;; Assume there is a final newline
	  (let ((item (todos-item-string))
		opoint)
	    (todos-remove-item)
	    (todos-forward-item)
	    (todos-insert-with-overlays item))
	(error "No TODO list entry to lower")))))

;; FIXME: moves last not done item when point on empty line below it
;; (defun todos-move-item ()
;;   "Move the current todo item to another, interactively named, category.

;; If the named category is not one of the current todo categories, then
;; it is created and the item becomes the first entry in that category."
;;   (interactive)
;;   (unless (or (todos-done-item-p)
;; 	      (looking-at "^$"))	; between done and not done items
;;     (let ((item (todos-item-string))
;; 	  (category (todos-completing-read))
;; 	  orig moved)
;;       (setq (save-excursion (todos-item-start)))
;;       (todos-remove-item)
;;       ;; numbered prefix isn't cached (see todos-remove-item) so have to update
;;       (if todos-number-prefix (todos-update-numbered-prefix))
;;       (setq chgr (prepare-change-group))
;;       ;; FIXME
;;       (unwind-protect
;; 	  (progn
;; 	    (activate-change-group chgr)
;; 	    (todos-add-item-non-interactively item category)
;; 	    (setq moved t))
;; 	(if moved
;; 	    (accept-change-group chgr)
;; 	  (cancel-change-group chgr))))))

(defun todos-move-item ()
  "Move the current todo item to another, interactively named, category.

If the named category is not one of the current todo categories, then
it is created and the item becomes the first entry in that category."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; between done and not done items
    (let ((buffer-read-only)
	  (oldnum todos-category-number)
	  (oldcat (nth todos-category-number todos-categories))
	  (item (todos-item-string))
	  (newcat (todos-completing-read))
	  (opoint (point))
	  (orig-mrk (save-excursion (todos-item-start) (point-marker)))
	  moved)
      (todos-remove-item)
      ;; numbered prefix isn't cached (see todos-remove-item) so have to update
      (if todos-number-prefix (todos-update-numbered-prefix))
      (unwind-protect
	  (progn
	    (todos-add-item-non-interactively item newcat)
	    (setq moved t))
	(unless moved
	  (widen)
	  (goto-char orig-mrk)
	  (todos-insert-with-overlays item)
	  (setq todos-category-number oldnum)
	  (todos-category-select)
	  ;; FIXME: does this work?
	  (goto-char opoint))
	(set-marker orig-mrk nil)))))

;; (defun todos-file-item (&optional comment)
;;   "File the current TODO list entry away, annotated with an optional COMMENT."
;;   (interactive "sComment: ")
;;   (or (> (count-lines (point-min) (point-max)) 0)
;;       (error "No TODO list entry to file away"))
;;   (let ((time-stamp-format todos-time-string-format))
;;     (when (and comment (> (length comment) 0))
;;       (goto-char (todos-item-end))
;;       (insert
;;        (if (save-excursion (beginning-of-line)
;;                            (looking-at (regexp-quote todos-prefix)))
;;            " "
;;          "\n\t")
;;        "(" comment ")"))
;;     (goto-char (todos-item-end))
;;     (insert " [" (nth todos-category-number todos-categories) "]")
;;     (goto-char (todos-item-start))
;;     (let ((temp-point (point)))
;;       (if (looking-at (regexp-quote todos-prefix))
;;   	  (replace-match (time-stamp-string))
;;   	;; Standard prefix -> timestamp
;;   	;; Else prefix non-standard item start with timestamp
;;   	(insert (time-stamp-string)))
;;       (append-to-file temp-point (1+ (todos-item-end)) todos-file-done)
;;       (delete-region temp-point (1+ (todos-item-end))))
;;     (todos-backward-item)
;;     (message ""))

(defun todos-item-done ()
  "Mark current item as done and move it to category's done section."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))
    (let* ((buffer-read-only)
	   (item (todos-item-string))
	   (date-string (calendar-date-string (calendar-current-date) t t))
	   (time-string (if todos-add-time-string
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
    (todos-show)))

(defun todos-archive-done-items ()
  "Archive the done items in the current category."
  (interactive)
  (let ((archive (find-file-noselect todos-archive-file t))
	(cat (nth todos-category-number todos-categories))
	beg end)
    (save-excursion
      (save-restriction
	(widen)
	(re-search-forward (concat "^" (regexp-quote todos-category-beg)) nil t)
	(setq end (or (match-beginning 0) (point-max)))
	(re-search-backward (concat "^" (regexp-quote todos-category-beg)
				    (regexp-quote cat))
			    nil t)
	(if (not (re-search-forward (concat "\\[" (regexp-quote todos-done-string))
				    nil t))
	    (error "No done items in this category")
	  (setq beg (match-beginning 0))
	  (setq done (buffer-substring beg end))
	  (with-current-buffer archive
	    (goto-char (point-min))
	    (if (re-search-forward (regexp-quote (concat "^" todos-category-beg cat))
				   nil t)
		(forward-char)
	      (insert todos-category-beg cat "\n"))
	    (insert done))
	  (delete-region beg end)
	  (remove-overlays beg end)
	  (kill-line -1)))))
  (message "Done items archived."))

;; FIXME: undone item leaves item number overlay behind
(defun todos-item-undo ()
  ""					;FIXME
  (interactive)
  (when (todos-done-item-p)
    (let* ((buffer-read-only)
	   (cat (nth todos-category-number todos-categories))
	   (start (progn
		    (todos-item-start)
		    (search-forward "] ")))	; end of done date string
	   (item (buffer-substring start (todos-item-end))))
      (todos-remove-item)
      (todos-add-item-non-interactively item cat))))

(defun todos-toggle-item-diary-inclusion ()
  ""					;FIXME add docstring
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
  ""					;FIXME add docstring
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

;; "Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec")
;; (regexp-opt (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31")))

;; FIXME: use diary-date-forms instead?
;; (defun todos-date-string ()
;;   "Return a regexp matching a diary date string."
;;   (let ((month (regexp-opt (list "Jan" "Feb" "Mar" "Apr" "May" "Jun"
;; 				 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
;; 	(day "[0-3]?[0-9]")
;; 	(year "[0-9]\\{4\\}"))
;;     (concat month " " day ", " year)))

;; FIXME: use diary-time-regexp
;; (defun todos-time-string ()
;;   "Return a regexp matching a diary time string."
;;   "[0-9]?[0-9][:.][0-9]\\{2\\}")

(defvar todos-date-nodayname-pattern
  (let ((dayname)
	(monthname (format "\\(%s\\|\\*\\)"
			   (diary-name-pattern calendar-month-name-array
					       calendar-month-abbrev-array t)))
        (month "\\([0-9]+\\|\\*\\)")
        (day "\\([0-9]+\\|\\*\\)")
        (year "-?\\([0-9]+\\|\\*\\)"))
    (mapconcat 'eval calendar-date-display-form ""))
  "Regular expression matching a Todos date header without day name.")

;; (defvar todos-dayname-pattern
;;   (diary-name-pattern calendar-day-name-array nil t)
;;   "Regular expression matching a day name in a Todos date header.")

(defvar todos-dayname-date-pattern
  (let ((dayname (diary-name-pattern calendar-day-name-array nil t)))
    (concat dayname "\\(?:, " todos-date-nodayname-pattern "\\)?"))
  "Regular expression matching a Todos date header with day name.")

(defvar todos-date-pattern
  (concat "\\(?:" todos-date-nodayname-pattern "\\)\\|"
	  "\\(?:" todos-date-dayname-pattern "\\)")
  "Regular expression matching a Todos date header.")

(defun todos-date-string-match (lim)
  "Find Todos date strings for font-locking."
  (re-search-forward (concat "^\\[?\\(" todos-date-pattern "\\)") lim t))

(defun todos-time-string-match (lim)
  "Find Todos time strings for font-locking."
  (re-search-forward (concat "^\\[?\\(?:" todos-date-pattern "\\)"
			       " \\(?1:" diary-time-regexp "\\)") lim t))

(defun todos-done-string-match (lim)
  "Find Todos done headers for font-locking."
  (re-search-forward (concat "^\\[" (regexp-quote todos-done-string) "[^][]+]")
		       lim t))

(defun todos-category-string-match (lim)
  "Find Todos category headers for font-locking."
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
  ""					;FIXME
  (make-local-variable 'word-wrap)
  (setq word-wrap t)
  (make-local-variable 'wrap-prefix)
  (setq wrap-prefix (make-string (+ 5 (length todos-prefix)) 32))
  (unless (member '(continuation) fringe-indicator-alist)
    (push '(continuation) fringe-indicator-alist)))

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
	(todos-show)))))

;; FIXME: rename and/or rewrite
(defun todos-update-numbered-prefix ()
  "Update consecutive item numbering in the current category."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (remove-overlays (point) (point) 'before-string)
      (todos-forward-item))
    (todos-show)))

(defvar todos-item-start-overlays nil "")

;; (defvar todos-done-overlays nil "")

;; (defun todos-check-overlay (prop)
;;   ""					;FIXME add docstring
;;   ;; (let ((ovlist (overlays-in (1- (point)) (1+ (point)))))
;;   (let ((ovlist (overlays-in (point) (point))))
;;     (when ovlist (overlay-get (car ovlist) prop))))

(defun todos-prefix-overlays ()
  ""					;FIXME add docstring
  (when (or todos-number-prefix
	    (not (string-match "^[[:space:]]*$" todos-prefix)))
    (let ((prefix (propertize (concat todos-prefix " ") 'face 'todos-prefix-string))
	  (num 0)
	  lim ov-pref)
      (save-excursion
	(goto-char (point-min))
	(while (or (todos-date-string-match lim)
		   (todos-done-string-match lim))
	  (goto-char (match-beginning 0))
	  (when todos-number-prefix
	    (setq num (1+ num))
	    ;; reset number for done items
	    (if (or (looking-at (concat "\n\\[" (regexp-quote todos-done-string)))
		    ;; if last not done item is multiline, then
		    ;; todos-done-string-match skips empty line, so have
		    ;; to look back
		    (and (looking-at (concat "^\\[" (regexp-quote todos-done-string)))
			 (looking-back "\n\n")))
		(setq num 1))
	    (setq prefix (propertize (concat (number-to-string num) " ")
				     'face 'todos-prefix-string)))
	  (or (and (setq ov-pref (car (overlays-in (point) (point))))
		   (equal (overlay-get ov-pref 'before-string) prefix))
	      (and (setq ov-pref (pop todos-item-start-overlays))
		   (move-overlay ov-pref (point) (point)))
	      (and (setq ov-pref (make-overlay (point) (point)))
		   (overlay-put ov-pref 'before-string prefix)))
	  (forward-line))))))

;; (defun todos-show-paren-hack ()
;;   "Purge overlay duplication due to show-paren-mode."
;;   (save-excursion
;;     (when show-paren-mode
;;       (goto-char (point-min))
;;       (while (not (eobp))
;; 	;; (let ((ovlist (overlays-in (1- (point)) (1+ (point))))
;; 	(let ((ovlist (overlays-in (point) (point)))
;; 	      ov)
;; 	  (while (> (length ovlist) 1)
;; 	    (setq ov (pop ovlist))
;; 	    (delete-overlay ov)))
;; 	(forward-line))
;;       (if (and (bolp) (eolp))
;; 	  ;; (let ((ovlist (overlays-in (1- (point)) (1+ (point)))))
;; 	  (let ((ovlist (overlays-in (point) (point))))
;; 	    (remove-overlays (1- (point)) (1+ (point))))))))

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
	  (when (re-search-forward (concat "^\\[" (regexp-quote todos-done-string)) nil t)
	    (remove-overlays (point) (point))))
	;; activate the prefix setting (save-restriction does not help)
	(todos-show)))))

;; FIXME: use this; should be defsubst?
(defun todos-current-category ()
  "Return the name of the current category."
  (nth todos-category-number todos-categories))

(defun todos-category-select ()
  "Make TODO mode display the current category correctly."
  (let ((name (nth todos-category-number todos-categories)))
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
  (let ((beg (point-min))
	(done-sep (if (string-match "^[[:space:]]*$" todos-done-separator)
		      todos-done-separator
		    (propertize (concat todos-done-separator "\n")
				'face 'todos-done-sep)))
	done ov)
    (when (re-search-forward (concat "\n\\(\\[" (regexp-quote todos-done-string)
				     "\\)") nil t)
      (setq done (match-beginning 1)
	    end (match-beginning 0))
      (if todos-show-with-done
	  ;; with an empty separator just display the done items
	  (if (string= done-sep todos-done-separator)
	      (narrow-to-region (point-min) (point-max))
	    ;; else display the separator in an overlay in front of the prefix
	    ;; overlay on first done item
	    (let ((prefix (propertize
			   (concat (if todos-number-prefix "1" todos-prefix) " ")
			   'face 'todos-prefix-string)))
	      (goto-char done)
	      (remove-overlays done done)
	      ;; must make separator overlay after making prefix overlay to get
	      ;; the order separator before prefix
	      (setq ov-pref (make-overlay done done)
		    ov-done (make-overlay done done))
	      (overlay-put ov-pref 'before-string prefix)
	      (overlay-put ov-done 'before-string done-sep)))
	;; hide done items
	(narrow-to-region (point-min) end))))
  (goto-char (point-min)))

;; FIXME: using numbering for priority instead of importance?
;;;###autoload
(defun todos-add-item-non-interactively (new-item category)
  "Insert NEW-ITEM in TODO list as a new entry in CATEGORY."
  ;; FIXME: really need this? (and in save-excursion?)
  (save-excursion
    (todos-show))
  (if (string= "" category)
      (setq category (nth todos-category-number todos-categories)))
  (let ((cat-exists (member category todos-categories)))
    (setq todos-category-number
	  (if cat-exists
	      (- (length todos-categories) (length cat-exists))
	    (todos-add-category category))))
  ;; FIXME: really need this? (yes for todos-move-item, to show moved to category)
  (todos-show)			; now at point-min
  ;; (setq todos-previous-line 0)
  ;; (let* ((top 1)
  ;; 	 (end (save-excursion
  ;; 		 (goto-char (point-min))
  ;; 		 (if (re-search-forward (concat "\n\n\\\(\\["
  ;; 						(regexp-quote todos-done-string)
  ;; 						"\\)") nil t)
  ;; 		     (match-beginning 1)
  ;; 		   (point-max))))
  ;; 	 (bottom (count-lines (point-min) end)))
  ;;   (while (> (- bottom top) todos-insert-threshold)
  ;;     (let* ((current (/ (+ top bottom) 2))
  ;; 	     (answer (if (< current bottom)
  ;; 			 (todos-more-important-p current) nil)))
  ;; 	(if answer
  ;; 	    (setq bottom current)
  ;; 	  (setq top (1+ current)))))
  ;;   (setq top (/ (+ top bottom) 2))
  ;;   (goto-char (point-min))
  ;;   (forward-line (1- top)))
  (unless (or (eq (point-min) (point-max)) ; no unfinished items
	      (when (re-search-forward (concat "^\\["
					       (regexp-quote todos-done-string))
				       nil t)
		(forward-line -1)
		(bobp))) ; there are done items but no unfinished items
    (let* ((num-items (1+ (car (todos-count-items-in-category))))
	   (priority (string-to-number (read-from-minibuffer
					(format "Set item priority (1-%d): "
						num-items))))
	   prompt)
      (while (cond ((not (integerp priority))
		    (setq prompt "Priority must be an integer.\n"))
		   ((< priority 1)
		    (setq prompt "Priority cannot be higher than 1.\n"))
		   ((> priority num-items)
		    (setq prompt (format "Priority cannot be lower than %d.\n"
					 num-items))))
	(setq priority
	      (string-to-number (read-from-minibuffer
				 (concat prompt
					 (format "Set item priority (1-%d): "
						 num-items))))))
      (goto-char (point-min))
      (todos-forward-item (1- priority))))
  (todos-insert-with-overlays new-item))

(defun todos-jump-to-category-noninteractively (cat)
  (let ((bufname (buffer-name)))
    (cond ((string= bufname todos-categories-buffer)
	   (switch-to-buffer (file-name-nondirectory todos-file-do)))
	  ((string= bufname todos-archived-categories-buffer)
	   ;; Is pop-to-buffer better for this case?
	   (switch-to-buffer (file-name-nondirectory todos-archive-file))))
    (kill-buffer bufname))
  (widen)
  (goto-char (point-min))
  (setq todos-category-number (- (length todos-categories)
				 (length (member cat todos-categories))))
  (todos-category-select))

(defun todos-insert-with-overlays (item)
  ""					;FIXME add docstring
  ;; FIXME: breaks without narrowing, e.g. todos-item-done
  ;; (unless (and (bolp) (eolp)) (goto-char (todos-item-start)))
  (insert item "\n")
  (todos-backward-item)
  (if todos-number-prefix
      (todos-update-numbered-prefix)
    (todos-prefix-overlays)))

;; (defun todos-more-important-p (line)
;;   "Ask whether entry is more important than the one at LINE."
;;   (unless (equal todos-previous-line line)
;;     (setq todos-previous-line line)
;;     (goto-char (point-min))
;;     (forward-line (1- todos-previous-line))
;;     (let ((item (todos-item-string-start)))
;;       (setq todos-previous-answer
;;             (y-or-n-p (concat "More important than '" item "'? ")))))
;;   todos-previous-answer)

(defun todos-line-string ()
  "Return current line in buffer as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun todos-item-string-start ()
  "Return the start of this TODO list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (todos-item-string)))
    (if (> (length item) 60)
        (setq item (concat (substring item 0 56) "...")))
    item))

(defun todos-item-start ()
  "Move to start of current TODO list item and return its position."
  (unless (or (looking-at "^$")		; last item or between done and not done
	      (looking-at (regexp-quote todos-category-beg))) ; for todos-count-items
    (goto-char (line-beginning-position))
    (while (not (looking-at (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
				    "\\)?\\)?" todos-date-pattern)))
      (forward-line -1)))
  (point))

(defun todos-item-end ()
  "Move to end of current TODO list item and return its position."
  (unless (looking-at "^$")		; last item or between done and not done
    (todos-forward-item)
    (backward-char))
  (point))

(defun todos-remove-item ()
  "Delete the current entry from the TODO list."
  (let* ((end (progn (todos-forward-item) (point)))
	 (beg (progn (todos-backward-item) (point)))
	 (ov-start (car (overlays-in beg beg))))
    (when ov-start
      ;; don't cache numbers, since they can be popped out of order in
      ;; todos-prefix-overlays
      (unless todos-number-prefix
	(push ov-start todos-item-start-overlays))
      (delete-overlay ov-start))
    (delete-region beg end)))

(defun todos-item-string ()
  "Return current TODO list entry as a string."
  (buffer-substring (todos-item-start) (todos-item-end)))

(defun todos-done-item-p ()
  ""					;FIXME
  (save-excursion
    (todos-item-start)
    (looking-at (concat "^\\[" (regexp-quote todos-done-string)))))

(defun todos-count-items-in-category ()
  "Return number of not done and done items in current category."
  (save-excursion
    (let ((not-done 0)
	  (done 0)
	  (beg (point-min))
	  end)
      (save-restriction
	(widen)
	(re-search-forward (concat "^" (regexp-quote todos-category-beg)) nil t)
	(setq end (or (match-beginning 0) (point-max)))
	(goto-char beg)
	(while (> end (point))
	  (if (todos-done-item-p)
	      (setq done (1+ done))
	    (setq not-done (1+ not-done)))
	  (todos-forward-item)
	  (when (and (not (> (point) end))
		     (looking-at "^$")
		     (not (eobp)))
	    ;; point is between done and not done items
	    (setq not-done (1- not-done))))
	(cons not-done done)))))

;; FIXME: rename, since *-alist is by convention a variable name
(defun todos-categories-alist ()
  "Return alist of categories and some of their properties.
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

(defun todos-count-all-items ()
  ""
  (let ((unfinished 0)
	(done 0))
    (dolist (l (todos-categories-alist))
      (setq unfinished (+ unfinished (car (cadr l)))
	    done (+ done (cdr (cadr l)))))
    (cons unfinished done)))

(defun todos-longest-category-name-length ()
  ""
  (let ((longest 0))
    (dolist (c (todos-categories-alist) longest)
      (setq longest (max longest (length (car c)))))))

(defun todos-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todos-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (todos-string-count-lines string) 1))

(defun todos-completing-read ()
  "Return a category name, with completion, for use in Todo mode."
  ;; allow SPC to insert spaces, for adding new category names with
  ;; todos-move-item
  (let ((map minibuffer-local-completion-map))
    (define-key map " " nil)
    ;; make a copy of todos-categories in case history-delete-duplicates is
    ;; non-nil, which makes completing-read alter todos-categories
    (let* ((categories (copy-sequence todos-categories))
	   (history (cons 'todos-categories (1+ todos-category-number)))
	   (default (nth todos-category-number todos-categories))
	   (completion-ignore-case todos-completion-ignore-case)
	   (category (completing-read
		      (concat "Category [" default "]: ")
		      todos-categories nil nil nil history default)))
      ;; restore the original value of todos-categories
      (setq todos-categories categories)
      category)))

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

;; ---------------------------------------------------------------------------
;;; Mode setup

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
  "Major mode for editing TODO lists.

\\{todos-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todos-mode)
  (setq mode-name "TODOS")
  (use-local-map todos-mode-map)
  (easy-menu-add todos-menu)
  (when todos-wrap-lines (funcall todos-line-wrapping-function))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (make-local-variable 'hl-line-range-function)
  (setq hl-line-range-function
	(lambda() (when (todos-item-end)
		    (cons (todos-item-start) (todos-item-end)))))
  ;; (add-hook 'post-command-hook 'todos-show-paren-hack nil t)
  (add-to-invisibility-spec 'todos)
  ;; FIXME: use this and let-bind in editing commands?
  (setq buffer-read-only t)
  (run-mode-hooks 'todos-mode-hook))

(defun todos-archive-mode ()
  "Major mode for archived Todos categories.

\\{todos-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'todos-archive-mode)
  (setq mode-name "TODOS Arch")
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
  "Major mode for editing items in the TODO list.

\\{todos-edit-mode-map}"
  (interactive)
  (setq major-mode 'todos-edit-mode)
  (setq mode-name "TODOS Edit")
  (use-local-map todos-edit-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(todos-font-lock-keywords t))
  (when todos-wrap-lines (funcall todos-line-wrapping-function)))

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
    (unless todos-categories
      (setq todos-categories (todos-categories-list (buffer-name))))
    ;; (beginning-of-line)
    (save-excursion
      (todos-category-select)
      ;; (todos-show-paren-hack)
      )))

(defun todos-initial-setup ()
  "Set up things to work properly in TODO mode."
  (find-file todos-file-do)
  (erase-buffer)
  (todos-mode)
  (todos-add-category "Todos"))

(provide 'todos)

;; arch-tag: 6fd91be5-776e-4464-a109-da4ea0e4e497
;;; todos.el ends here
