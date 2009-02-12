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

(require 'time-stamp)


;; User-configurable variables:

(defgroup todos nil
  "Maintain a list of todo items."
  :link '(emacs-commentary-link "todos")
  :version "21.1"
  :group 'calendar)

(defcustom todos-prefix     "*/*"
  "TODO mode prefix for entries.

This is useful in conjunction with `calendar' and `diary' if you use

#include \"~/.todos-do\"

in your diary file to include your todo list file as part of your
diary.  With the default value \"*/*\" the diary displays each entry
every day and it may also be marked on every day of the calendar.
Using \"&%%(equal (calendar-current-date) date)\" instead will only
show and mark todo entries for today, but may slow down processing of
the diary file somewhat."
  :type 'string
  :group 'todos)
(defcustom todos-file-do    (convert-standard-filename "~/.todos-do")
  "TODO mode list file."
  :type 'file
  :group 'todos)
(defcustom todos-file-done  (convert-standard-filename "~/.todos-done")
  "TODO mode archive file."
  :type 'file
  :group 'todos)
(defcustom todos-mode-hook  nil
  "TODO mode hooks."
  :type 'hook
  :group 'todos)
(defcustom todos-edit-mode-hook nil
  "TODO Edit mode hooks."
  :type 'hook
  :group 'todos)
(defcustom todos-insert-threshold 0
  "TODO mode insertion accuracy.

If you have 8 items in your TODO list, then you may get asked 4
questions by the binary insertion algorithm.  However, you may not
really have a need for such accurate priorities amongst your TODO
items.  If you now think about the binary insertion halving the size
of the window each time, then the threshold is the window size at
which it will stop.  If you set the threshold to zero, the upper and
lower bound will coincide at the end of the loop and you will insert
your item just before that point.  If you set the threshold to,
e.g. 8, it will stop as soon as the window size drops below that
amount and will insert the item in the approximate center of that
window."
  :type 'integer
  :group 'todos)
(defvar todos-edit-buffer " *TODO Edit*"
  "TODO Edit buffer name.")
(defcustom todos-file-top (convert-standard-filename "~/.todos-top")
  "TODO mode top priorities file.

Not in TODO format, but diary compatible.
Automatically generated when `todos-save-top-priorities' is non-nil."
  :type 'string
  :group 'todos)

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
(defcustom todos-remove-separator t
  "Non-nil to remove category separators in\
\\[todos-top-priorities] and \\[todos-print]."
  :type 'boolean
  :group 'todos)
(defcustom todos-save-top-priorities-too t
  "Non-nil makes `todos-save' automatically save top-priorities in `todos-file-top'."
  :type 'boolean
  :group 'todos)

;; Thanks for the ISO time stamp format go to Karl Eichwalder <ke@suse.de>
;; My format string for the appt.el package is "%3b %2d, %y, %02I:%02M%p".
;;
(defcustom todos-time-string-format
  "%:y-%02m-%02d %02H:%02M"
  "TODO mode time string format for done entries.
For details see the variable `time-stamp-format'."
  :type 'string
  :group 'todos)

(defcustom todos-entry-prefix-function 'todos-entry-timestamp-initials
  "Function producing text to insert at start of todo entry."
  :type 'symbol
  :group 'todos)
(defcustom todos-initials (or (getenv "INITIALS") (user-login-name))
  "Initials of todo item author."
  :type 'string
  :group 'todos)

(defun todos-entry-timestamp-initials ()
  "Prepend timestamp and your initials to the head of a TODO entry."
  (let ((time-stamp-format todos-time-string-format))
    (concat (time-stamp-string) " " todos-initials ": ")))

;; ---------------------------------------------------------------------------

;; Set up some helpful context ...

(defvar todos-categories nil
  "TODO categories.")

(defvar todos-previous-line 0
  "Previous line asked about.")

(defvar todos-previous-answer 0
  "Previous answer got.")

(defvar todos-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'todos-forward-category)
    (define-key map "-" 'todos-backward-category)
    (define-key map "d" 'todos-file-item) ;done/delete
    (define-key map "e" 'todos-edit-item)
    (define-key map "E" 'todos-edit-multiline)
    (define-key map "f" 'todos-file-item)
    (define-key map "i" 'todos-insert-item)
    (define-key map "I" 'todos-insert-item-here)
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "k" 'todos-delete-item)
    (define-key map "l" 'todos-lower-item)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    (define-key map "P" 'todos-print)
    (define-key map "q" 'todos-quit)
    (define-key map "r" 'todos-raise-item)
    (define-key map "s" 'todos-save)
    (define-key map "S" 'todos-save-top-priorities)
    (define-key map "t" 'todos-top-priorities)
    map)
  "TODO mode keymap.")

(defvar todos-category-number 0 "TODO category number.")

(defvar todos-tmp-buffer-name " *todo tmp*")

(defvar todos-category-sep (make-string 75 ?-)
  "Category separator.")

(defvar todos-category-beg " --- "
  "Category start separator to be prepended onto category name.")

(defvar todos-category-end "--- End"
  "Separator after a category.")

(defvar todos-header "-*- mode: todo; "
  "Header of todo files.")

;; ---------------------------------------------------------------------------

(defun todos-category-select ()
  "Make TODO mode display the current category correctly."
  (let ((name (nth todos-category-number todos-categories)))
    (setq mode-line-buffer-identification
;;          (concat "Category: " name))
          (concat "Category: " (format "%18s" name)))
    (widen)
    (goto-char (point-min))
    (search-forward-regexp
     (concat "^"
             (regexp-quote (concat todos-prefix todos-category-beg name))
             "$"))
    (let ((begin (1+ (line-end-position))))
      (search-forward-regexp (concat "^" todos-category-end))
      (narrow-to-region begin (line-beginning-position))
      (goto-char (point-min)))))

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

(defun todos-backward-item ()
  "Select previous entry of TODO list."
  (interactive)
  (search-backward-regexp (concat "^" (regexp-quote todos-prefix)) nil t)
  (message ""))

(defun todos-forward-item (&optional count)
  "Select COUNT-th next entry of TODO list."
  (interactive "P")
  (if (listp count) (setq count (car count)))
  (end-of-line)
  (search-forward-regexp (concat "^" (regexp-quote todos-prefix))
                         nil 'goto-end count)
  (beginning-of-line)
  (message ""))

(defun todos-save ()
  "Save the TODO list."
  (interactive)
  (save-excursion
    (save-restriction
      (save-buffer)))
  (if todos-save-top-priorities-too (todos-save-top-priorities)))

(defun todos-quit ()
  "Done with TODO list for now."
  (interactive)
  (widen)
  (todos-save)
  (message "")
  (bury-buffer))

(defun todos-edit-item ()
  "Edit current TODO list entry."
  (interactive)
  (let ((item (todos-item-string)))
    (if (todos-string-multiline-p item)
        (todos-edit-multiline)
      (let ((new (read-from-minibuffer "Edit: " item)))
        (todos-remove-item)
        (insert new "\n")
        (todos-backward-item)
        (message "")))))

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

;;;###autoload
(defun todos-add-category (&optional cat)
  "Add new category CAT to the TODO list."
  (interactive)
  (let ((buf (find-file-noselect todos-file-do t))
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
      (if (search-forward "-*- mode: todo; " 17 t)
	  (kill-line)
	(insert "-*- mode: todo; \n")
	(forward-char -1))
      (insert (format "todos-categories: %S; -*-" todos-categories))
      (forward-char 1)
      (insert (format "%s%s%s\n%s\n%s %s\n"
		      todos-prefix todos-category-beg cat
		      todos-category-end
		      todos-prefix todos-category-sep))
      (if (interactive-p)
	  ;; properly display the newly added category
	  (progn (setq todos-category-number 0) (todos-show))
	0))))

;;;###autoload
(defun todos-add-item-non-interactively (new-item category)
  "Insert NEW-ITEM in TODO list as a new entry in CATEGORY."
  (save-excursion
    (todos-show))
  (save-excursion
    (if (string= "" category)
        (setq category (nth todos-category-number todos-categories)))
    (let ((cat-exists (member category todos-categories)))
      (setq todos-category-number
            (if cat-exists
                (- (length todos-categories) (length cat-exists))
              (todos-add-category category))))
    (todos-show)
    (setq todos-previous-line 0)
    (let ((top 1)
	  (bottom (1+ (count-lines (point-min) (point-max)))))
      (while (> (- bottom top) todos-insert-threshold)
	(let* ((current (/ (+ top bottom) 2))
	       (answer (if (< current bottom)
			   (todos-more-important-p current) nil)))
	  (if answer
	      (setq bottom current)
	    (setq top (1+ current)))))
      (setq top (/ (+ top bottom) 2))
      ;; goto-line doesn't have the desired behavior in a narrowed buffer.
      (goto-char (point-min))
      (forward-line (1- top)))
    (insert new-item "\n")
    (todos-backward-item)
    (todos-save)
    (message "")))

;;;###autoload
(defun todos-insert-item (arg)
  "Insert new TODO list entry.
With a prefix argument solicit the category, otherwise use the current
category."
  (interactive "P")
  (save-excursion
    (if (not (derived-mode-p 'todos-mode)) (todos-show))
    (let* ((new-item (concat todos-prefix " "
			     (read-from-minibuffer
			      "New TODO entry: "
			      (if todos-entry-prefix-function
				  (funcall todos-entry-prefix-function)))))
	   (current-category (nth todos-category-number todos-categories))
	   (category (if arg (todos-completing-read) current-category)))
      (todos-add-item-non-interactively new-item category))))

(defun todos-insert-item-here ()
  "Insert a new TODO list entry directly above the entry at point.
If point is on an empty line, insert the entry there."
  (interactive)
  (if (not (derived-mode-p 'todos-mode)) (todos-show))
  (let ((new-item (concat todos-prefix " "
			  (read-from-minibuffer
			   "New TODO entry: "
			   (if todos-entry-prefix-function
			       (funcall todos-entry-prefix-function))))))
    (unless (and (bolp) (eolp)) (goto-char (todos-item-start)))
    (insert (concat new-item "\n"))
    (backward-char)
    ;; put point at start of new entry
    (goto-char (todos-item-start))))

(defun todos-more-important-p (line)
  "Ask whether entry is more important than the one at LINE."
  (unless (equal todos-previous-line line)
    (setq todos-previous-line line)
    (goto-char (point-min))
    (forward-line (1- todos-previous-line))
    (let ((item (todos-item-string-start)))
      (setq todos-previous-answer
            (y-or-n-p (concat "More important than '" item "'? ")))))
  todos-previous-answer)

(defun todos-delete-item ()
  "Delete current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((todos-entry (todos-item-string-start))
             (todos-answer (y-or-n-p (concat "Permanently remove '"
                                            todos-entry "'? "))))
        (when todos-answer
          (todos-remove-item)
          (todos-backward-item))
        (message ""))
    (error "No TODO list entry to delete")))

(defun todos-raise-item ()
  "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point)) 0)
      (let ((item (todos-item-string)))
        (todos-remove-item)
        (todos-backward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No TODO list entry to raise")))

(defun todos-lower-item ()
  "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point) (point-max)) 1)
      ;; Assume there is a final newline
      (let ((item (todos-item-string)))
        (todos-remove-item)
        (todos-forward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No TODO list entry to lower")))

(defun todos-file-item (&optional comment)
  "File the current TODO list entry away, annotated with an optional COMMENT."
  (interactive "sComment: ")
  (or (> (count-lines (point-min) (point-max)) 0)
      (error "No TODO list entry to file away"))
  (let ((time-stamp-format todos-time-string-format))
    (when (and comment (> (length comment) 0))
      (goto-char (todos-item-end))
      (insert
       (if (save-excursion (beginning-of-line)
                           (looking-at (regexp-quote todos-prefix)))
           " "
         "\n\t")
       "(" comment ")"))
    (goto-char (todos-item-end))
    (insert " [" (nth todos-category-number todos-categories) "]")
    (goto-char (todos-item-start))
    (let ((temp-point (point)))
      (if (looking-at (regexp-quote todos-prefix))
	  (replace-match (time-stamp-string))
	;; Standard prefix -> timestamp
	;; Else prefix non-standard item start with timestamp
	(insert (time-stamp-string)))
      (append-to-file temp-point (1+ (todos-item-end)) todos-file-done)
      (delete-region temp-point (1+ (todos-item-end))))
    (todos-backward-item)
    (message "")))

;; ---------------------------------------------------------------------------

;; Utility functions:


;;;###autoload
(defun todos-top-priorities (&optional nof-priorities category-pr-page)
  "List top priorities for each category.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to \'todos-show-priorities\'.

If CATEGORY-PR-PAGE is non-nil, a page separator \'^L\' is inserted
between each category."

  (interactive "P")
  (or nof-priorities (setq nof-priorities todos-show-priorities))
  (if (listp nof-priorities)            ;universal argument
      (setq nof-priorities (car nof-priorities)))
  (let ((todos-print-buffer-name todos-tmp-buffer-name)
        ;;(todos-print-category-number 0)
        (todos-category-break (if category-pr-page "" ""))
        (cat-end
         (concat
          (if todos-remove-separator
              (concat todos-category-end "\n"
                      (regexp-quote todos-prefix) " " todos-category-sep "\n")
            (concat todos-category-end "\n"))))
        beg end)
    (todos-show)
    (save-excursion
      (save-restriction
        (widen)
        (copy-to-buffer todos-print-buffer-name (point-min) (point-max))
        (set-buffer todos-print-buffer-name)
        (goto-char (point-min))
        (when (re-search-forward (regexp-quote todos-header) nil t)
	  (beginning-of-line 1)
	  (delete-region (point) (line-end-position)))
        (while (re-search-forward       ;Find category start
                (regexp-quote (concat todos-prefix todos-category-beg))
                nil t)
          (setq beg (+ (line-end-position) 1)) ;Start of first entry.
          (re-search-forward cat-end nil t)
          (setq end (match-beginning 0))
          (replace-match todos-category-break)
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
        ))
    ;; Could have used switch-to-buffer as it has a norecord argument,
    ;; which is nice when we are called from e.g. todos-print.
    ;; Else we could have used pop-to-buffer.
    (display-buffer todos-print-buffer-name)
    (message "Type C-x 1 to remove %s window.  M-C-v to scroll the help."
             todos-print-buffer-name)))

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
  (save-window-excursion
    (save-excursion
      (save-restriction
	(todos-top-priorities todos-print-priorities
			     category-pr-page)
	(set-buffer todos-tmp-buffer-name)
	(and (funcall todos-print-function)
	     (kill-this-buffer))
	(message "Todo printing done.")))))

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
    (todos-show)))

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
  "Return point at start of current TODO list item."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at (regexp-quote todos-prefix)))
        (search-backward-regexp
         (concat "^" (regexp-quote todos-prefix)) nil t))
    (point)))

(defun todos-item-end ()
  "Return point at end of current TODO list item."
  (save-excursion
    (end-of-line)
    (search-forward-regexp
     (concat "^" (regexp-quote todos-prefix)) nil 'goto-end)
    (1- (line-beginning-position))))

(defun todos-remove-item ()
  "Delete the current entry from the TODO list."
  (delete-region (todos-item-start) (1+ (todos-item-end))))

(defun todos-item-string ()
  "Return current TODO list entry as a string."
  (buffer-substring (todos-item-start) (todos-item-end)))

(defun todos-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todos-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (todos-string-count-lines string) 1))

(defun todos-completing-read ()
  "Return a category name, with completion, for use in Todo mode."
  ;; make a copy of todos-categories in case history-delete-duplicates is
  ;; non-nil, which makes completing-read alter todos-categories
  (let* ((categories (copy-sequence todos-categories))
	 (history (cons 'todos-categories (1+ todos-category-number)))
	 (default (nth todos-category-number todos-categories))
	 (category (completing-read
		    (concat "Category [" default "]: ")
		    todos-categories nil nil nil history default)))
    ;; restore the original value of todos-categories
    (setq todos-categories categories)
    category))

;; ---------------------------------------------------------------------------

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
  (setq mode-name "TODO")
  (use-local-map todos-mode-map)
  (easy-menu-add todos-menu)
  (run-mode-hooks 'todos-mode-hook))

(defvar date)
(defvar entry)

;; t-c should be used from diary code, which requires calendar.
(declare-function calendar-current-date "calendar" nil)

;; Read about this function in the setup instructions above!
;;;###autoload
(defun todos-cp ()
  "Make a diary entry appear only in the current date's diary."
  (if (equal (calendar-current-date) date)
      entry))

(define-derived-mode todos-edit-mode text-mode "TODO Edit"
  "Major mode for editing items in the TODO list.

\\{todos-edit-mode-map}")

;;;###autoload
(defun todos-show ()
  "Show TODO list."
  (interactive)
  ;; Call todos-initial-setup only if there is neither a Todo file nor
  ;; a corresponding unsaved buffer.
  (if (or (file-exists-p todos-file-do)
	  (let* ((buf (get-buffer (file-name-nondirectory todos-file-do)))
		 (bufname (buffer-file-name buf)))
	    (equal (expand-file-name todos-file-do) bufname)))
      (find-file todos-file-do)
    (todos-initial-setup))
  (if (null todos-categories)
      (if (null todos-cats)
          (error "Error in %s: No categories in list `todos-categories'"
                 todos-file-do)
        (goto-char (point-min))
        (and (search-forward "todos-cats:" nil t)
             (replace-match "todos-categories:"))
        (make-local-variable 'todos-categories)
        (setq todos-categories todos-cats)))
  (beginning-of-line)
  (todos-category-select))

(defun todos-initial-setup ()
  "Set up things to work properly in TODO mode."
  (find-file todos-file-do)
  (erase-buffer)
  (todos-mode)
  (todos-add-category "Todos"))

(provide 'todos)

;; arch-tag: 6fd91be5-776e-4464-a109-da4ea0e4e497
;;; todos.el ends here
