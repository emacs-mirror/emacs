;;; todo-mode.el -- Major mode for editing TODO list files

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Oliver.Seidel@cl.cam.ac.uk (was valid on Aug 2, 1997)
;; Created: 2 Aug 1997
;; Version: $Id: todo-mode.el,v 1.13 1997/08/19 14:00:36 seidel Exp os10000 $
;; Keywords: Categorised TODO list editor, todo-mode

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ---------------------------------------------------------------------------

;;; Commentary:

;; Quickstart Installation:
;; ========================
;;
;; To get this to work, make emacs execute the line
;;
;; (require 'todo-mode)				;; load the TODO package
;;
;; You may now enter new items by typing "M-x todo-insert-item", or enter
;; your the TODO list file by typing "M-x todo-show".
;;
;; The TODO list file has a special format and some auxiliary information,
;; which will be added by the todo-show function if it attempts to visit
;; an un-initialised file.  Hence it is recommended to use the todo-show
;; function for the first time, in order to initialise the file, but it
;; is not necessary afterwards.
;;
;; As these commands are quite long to type, I would recommend the addition
;; of two bindings to your to your global keymap.  I personally have the
;; following in my initialisation file:
;;
;; (global-set-key "\C-ct" 'todo-show)		;; switch to TODO buffer
;; (global-set-key "\C-ci" 'todo-insert-item)	;; insert new item
;;
;; Note, however, that this recommendation has prompted some criticism,
;; since the keys C-c LETTER are reserved for user functions.  I believe
;; my recommendation is acceptable, since the Emacs Lisp Manual *Tips*
;; section also details that the mode itself should not bind any functions
;; to those keys.  The express aim of the above two bindings is to work
;; outside the mode, which doesn't need the show function and offers
;; a different binding for the insert function.  They serve as shortcuts
;; and are not even needed (since the TODO mode will be entered by
;; visiting the TODO file, and later by switching to its buffer).
;;
;;
;;
;; Pre-Requisites
;; ==============
;;
;; This package will require the following packages to be available on
;; the load-path:
;;                 - time-stamp
;;                 - easymenu
;;
;;
;;
;; Description:
;; ============
;;
;; TODO is a major mode for EMACS which offers functionality to treat
;; most lines in one buffer as a list of items one has to do.  There
;; are facilities to add new items, which are categorised, to edit or
;; even delete items from the buffer.  The buffer contents are currently
;; compatible with the diary, so that the list of todo-items will show
;; up in the FANCY diary mode.
;;
;; Notice:  Besides the major mode, this file also exports the function
;; "todo-show" which will change to the one specific TODO file that has
;; been specified in the todo-file-do variable.  If this file does not
;; conform to the TODO mode conventions, the todo-show function will add
;; the appropriate header and footer.  I don't anticipate this to cause
;; much grief, but be warned, in case you attempt to read a plain text file.
;;
;;
;;
;; Operation:
;; ==========
;;
;; You will have the following facilities available:
;;
;; M-x todo-show              will enter the todo list screen, here type
;;
;; +                          to go to next category
;; -                          to go to previous category
;; e                          to edit the current entry
;; f                          to file the current entry, including a
;;                                                 comment and timestamp
;; i                          to insert a new entry
;; k                          to kill the current entry
;; l                          to lower the current entry's priority
;; n                          for the next entry
;; p                          for the previous entry
;; q                          to save the list and exit the buffer
;; r                          to raise the current entry's priority
;; s                          to save the list
;;
;; When you add a new entry, you are asked for the text and then for the
;; category.  I for example have categories for things that I want to do
;; in the office (like mail my mum), that I want to do in town (like buy
;; cornflakes) and things I want to do at home (move my suitcases).  The
;; categories can be selected with the cursor keys and if you type in the
;; name of a category which didn't exist before, an empty category of the
;; desired name will be added and filled with the new entry.
;;
;;
;;
;; Configuration:
;; ==============
;;
;; --- todo-prefix
;;
;; I would like to recommend that you use the prefix "*/*" (by
;; leaving the variable 'todo-prefix' untouched) so that the diary
;; displays each entry every day.
;;
;; To understand what I mean, please read the documentation that goes
;; with the calendar since that will tell you how you can set up the
;; fancy diary display and use the #include command to include your
;; todo list file as part of your diary.
;;
;; If you have the diary package set up to usually display more than
;; one day's entries at once, consider using
;;   "&%%(equal (calendar-current-date) date)"
;; as the value of `todo-prefix'.  Please note that this may slow down
;; the processing of your diary file some.
;;
;;
;; --- todo-file-do
;;
;; This variable is fairly self-explanatory.  You have to store your TODO
;; list somewhere.  This variable tells the package where to go and find
;; this file.
;;
;;
;; --- todo-file-done
;;
;; Even when you're done, you may wish to retain the entries.  Given
;; that they're timestamped and you are offered to add a comment, this
;; can make a useful diary of past events.  It will even blend in with
;; the EMACS diary package.  So anyway, this variable holds the name
;; of the file for the filed todo-items.
;;
;;
;; --- todo-mode-hook
;;
;; Just like other modes, too, this mode offers to call your functions
;; before it goes about its business.  This variable will be inspected
;; for any functions you may wish to have called once the other TODO
;; mode preparations have been completed.
;;
;;
;; --- todo-insert-treshold
;;
;; Another nifty feature is the insertion accuracy.  If you have 8 items
;; in your TODO list, then you may get asked 4 questions by the binary
;; insertion algorithm.  However, you may not really have a need for such
;; accurate priorities amongst your TODO items.  If you now think about
;; the binary insertion halfing the size of the window each time, then
;; the threshhold is the window size at which it will stop.  If you set
;; the threshhold to zero, the upper and lower bound will coincide at the
;; end of the loop and you will insert your item just before that point.
;; If you set the threshhold to i.e. 8, it will stop as soon as the window
;; size drops below that amount and will insert the item in the approximate
;; centre of that window.  I got the idea for this feature after reading
;; a very helpful e-mail reply from Trey Jackson <trey@cs.berkeley.edu>
;; who corrected some of my awful coding and pointed me towards some good
;; reading.  Thanks Trey!
;;
;;
;;
;;
;; Things to do:
;; =============
;;
;; - licence / version function
;; - export to diary file
;; - todo-report-bug
;; - GNATS support
;; - add idea from Urban Boquist <boquist@cs.chalmers.se>: multi-line-entries
;; - 'e' opens buffer for multi-line entry
;; - elide multiline
;; - rewrite complete package to store data as lisp objects and have
;;   display modes for display, for diary export, etc.
;;
;;
;;
;; History and Gossip:
;; ===================
;;
;; Many thanks to all the ones who have contributed to the evolution of this
;; package!  I hope I have listed all of you somewhere in the documentation
;; or at least in the RCS history!
;;
;; Enjoy this package and express your gratitude by sending nice things
;; to my parents' address!
;;
;; Oliver Seidel
;;
;; (O Seidel, Lessingstr. 8, 65760 Eschborn, Federal Republic of Germany)
;;

;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------

;;; Change Log:

;; $Log: todo-mode.el,v $
;; Revision 1.13  1997/08/19  14:00:36  seidel
;; - changed name to todo-mode
;; - fixed menu descriptions
;; - fixed "pressing abort while filing"
;; - attempted Emacs Lisp Manual *Tips* section compliance
;;
;; Revision 1.12  1997/08/06  10:56:15  os10000
;; Fixed header, typos, layout, documentation.
;;
;; Revision 1.11  1997/08/06  09:14:25  os10000
;; Applied patch from Istvan Marko <istvan@cmdmail.amd.com>
;; to make menus work anywhere.
;;
;; Revision 1.10  1997/08/06  08:56:03  os10000
;; Acted upon suggestion from Shane Holder <holder@rsn.hp.com>:
;; Cancelling the editing of an entry will not delete it any more.
;;
;; Revision 1.9  1997/08/06 08:12:03  os10000
;; Improved documentation.  Broke some lines to comply with
;; Richard Stallman's email to please keep in sync with the
;; rest of the Emacs distribution files.
;;
;; Revision 1.8  1997/08/05 22:39:04  os10000
;; Made todo-mode.el available under GPL.
;;
;; Revision 1.7  1997/08/05 22:34:14  os10000
;; Fixed insertion routine with help from Trey Jackson
;; <trey@cs.berkeley.edu>; added todo-inst-tresh;
;; fixed keyboard layout to remove unwanted keys.
;;
;; Revision 1.6  1997/08/05 16:47:01  os10000
;; Incorporated menus for XEmacs from Allan.Cochrane@soton.sc.philips.com,
;; fixed TYPO, fixed todo-file-cmd, cleaned up rcs history.
;;
;; Revision 1.5  1997/08/05  14:43:39  os10000
;; Added improvements from Ron Gut <rgut@aware.com>.
;; Added category management.
;;
;; Revision 1.4  1997/08/04  16:18:45  os10000
;; Added Raise/Lower item.
;;
;; Revision 1.3  1997/08/03  12:47:26  os10000
;; Cleaned up variables, prefix and cursor position.
;;
;; Revision 1.2  1997/08/03 12:15:28  os10000
;; It appears to work.
;;
;; Revision 1.1  1997/08/03 12:15:13  os10000
;; Initial revision
;;

;; ---------------------------------------------------------------------------

;;; Code:

;; User-configurable variables:

(defvar todo-prefix	"*/*"		"TODO mode prefix for entries.")
(defvar todo-file-do	"~/.todo-do"	"TODO mode list file.")
(defvar todo-file-done	"~/.todo-done"	"TODO mode archive file.")
(defvar todo-mode-hook	nil		"TODO mode hooks.")
(defvar todo-edit-mode-hook nil		"TODO Edit mode hooks.")
(defvar todo-insert-treshold 0		"TODO mode insertion accuracy.")
(defvar todo-edit-buffer " *TODO Edit*"	"TODO Edit buffer name.")

;; Thanks for the ISO time stamp format go to Karl Eichwalder <ke@suse.de>
;; My format string for the appt.el package is "%3b %2d, %y, %02I:%02M%p".
;;
(defvar todo-time-string-format "%y-%02m-%02d %02H:%02M"
  "TODO mode time string format for done entries.
For details see the variable `time-stamp-format'.")

;; ---------------------------------------------------------------------------

;; Get some outside help ...

(require 'time-stamp)
(require 'easymenu)

;; ---------------------------------------------------------------------------

;; Set up some helpful context ...

(defvar todo-categories		nil	"TODO categories.")
(defvar todo-previous-line	0	"previous line that I asked about.")
(defvar todo-previous-answer	0	"previous answer that I got.")
(defvar todo-mode-map		nil	"TODO mode keymap.")
(defvar todo-category-number	0	"TODO category number.")

;; ---------------------------------------------------------------------------

(if todo-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'todo-forward-category)
    (define-key map "-" 'todo-backward-category)
    (define-key map "e" 'todo-edit-item)
    (define-key map "E" 'todo-edit-multiline)
    (define-key map "f" 'todo-file-item)
    (define-key map "i" 'todo-insert-item)
    (define-key map "k" 'todo-delete-item)
    (define-key map "l" 'todo-lower-item)
    (define-key map "n" 'todo-forward-item)
    (define-key map "p" 'todo-backward-item)
    (define-key map "q" 'todo-quit)
    (define-key map "r" 'todo-raise-item)
    (define-key map "s" 'todo-save)
    (setq todo-mode-map map)))

(defun todo-category-select ()
  "Make TODO mode display the current category correctly."
  (let ((name (nth todo-category-number todo-categories)))
    (setq mode-line-buffer-identification
	  (concat "Category: " name))
    (widen)
    (goto-char (point-min))
    (search-forward-regexp
     (concat "^" (regexp-quote (concat todo-prefix " --- " name))))
    (let ((begin (1+ (point-at-eol))))
      (search-forward-regexp "^--- End")
      (narrow-to-region begin (point-at-bol))
      (goto-char (point-min)))))
(defalias 'todo-cat-slct 'todo-category-select)

(defun todo-forward-category () "Go forward to TODO list of next category."
  (interactive)
  (setq todo-category-number
	(mod (1+ todo-category-number) (length todo-categories)))
  (todo-category-select))
(defalias 'todo-cmd-forw 'todo-forward-category)

(defun todo-backward-category () "Go back to TODO list of previous category."
  (interactive)
  (setq todo-category-number
	(mod (1- todo-category-number) (length todo-categories)))
  (todo-category-select))
(defalias 'todo-cmd-back 'todo-backward-category)

(defun todo-backward-item () "Select previous entry of TODO list."
  (interactive)
  (search-backward-regexp (concat "^" (regexp-quote todo-prefix)) nil t)
  (message ""))
(defalias 'todo-cmd-prev 'todo-backward-item)

(defun todo-forward-item () "Select next entry of TODO list."
  (interactive)
  (end-of-line)
  (search-forward-regexp (concat "^" (regexp-quote todo-prefix)) nil 'goto-end)
  (beginning-of-line)
  (message ""))
(defalias 'todo-cmd-next 'todo-forward-item)

(defun todo-save () "Save the TODO list."
  (interactive)
  (save-buffer))
(defalias 'todo-cmd-save 'todo-save)

(defun todo-quit () "Done with TODO list for now."
  (interactive)
  (widen)
  (save-buffer)
  (message "")
  (bury-buffer))
(defalias 'todo-cmd-done 'todo-quit)

(defun todo-edit-item () "Edit current TODO list entry."
  (interactive)
  (let ((item (todo-item-string)))
    (if (todo-string-multiline-p item)
	(todo-edit-multiline)
      (let ((new (read-from-minibuffer "Edit: " item)))
	(todo-remove-item)
	(insert new "\n")
	(todo-backward-item)
	(message "")))))
(defalias 'todo-cmd-edit 'todo-edit-item)

(defun todo-edit-multiline ()
  "Set up a buffer for editing a multiline TODO list entry."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name todo-edit-buffer)))
    (pop-to-buffer (make-indirect-buffer (file-name-nondirectory todo-file-do)
					 buffer-name))
    (todo-edit-mode)
    (narrow-to-region (todo-item-start) (todo-item-end))))

(defun todo-add-category (cat) "Add a new category to the TODO list."
  (interactive)
  (save-window-excursion
    (setq todo-categories (cons cat todo-categories))
    (find-file todo-file-do)
    (widen)
    (goto-char (point-min))
    (let ((posn (search-forward "-*- mode: todo; " 17 t)))
      (if (not (null posn)) (goto-char posn))
      (if (equal posn nil)
	  (progn
	    (insert "-*- mode: todo; \n")
	    (forward-char -1))
	(kill-line)))
    (insert (format "todo-categories: %S; -*-" todo-categories))
    (forward-char 1)
    (insert (format "%s --- %s\n--- End\n%s %s\n"
		    todo-prefix cat todo-prefix (make-string 75 ?-))))
  0)

(defun todo-insert-item ()
  "Insert new TODO list entry."
  (interactive)
  (let* ((new-item (concat todo-prefix " "
			   (read-from-minibuffer "New TODO entry: ")))
         (categories todo-categories)
         (history (cons 'categories (1+ todo-category-number)))
	 (category (completing-read "Category: "
				    (todo-category-alist) nil nil
				    (nth todo-category-number todo-categories)
				    history)))
    (let ((cat-exists (member category todo-categories)))
      (setq todo-category-number
	    (if cat-exists
		(- (length todo-categories) (length cat-exists))
	      (todo-add-category category))))
    (todo-show)
    (setq todo-previous-line 0)
    (let ((top 1)
	  (bottom (1+ (count-lines (point-min) (point-max)))))
      (while (> (- bottom top) todo-insert-treshold)
	(let* ((current (/ (+ top bottom) 2))
	       (answer (if (< current bottom)
			   (todo-more-important-p current) nil)))
	  (if answer
	      (setq bottom current)
	    (setq top (1+ current)))))
      (setq top (/ (+ top bottom) 2))
      ;; goto-line doesn't have the desired behavior in a narrowed buffer
      (goto-char (point-min))
      (forward-line (1- top)))
    (insert new-item "\n")
    (todo-backward-item)
    (save-buffer)
    (message "")))
(defalias 'todo-cmd-inst 'todo-insert-item)

(defun todo-more-important-p (line) 
  "Ask whether entry is more important than the one at LINE."
  (if (not (equal todo-previous-line line))
      (progn
        (setq todo-previous-line line)
        (goto-char (point-min))
        (forward-line (1- todo-previous-line))
	(let ((item (todo-item-string-start)))
	  (setq todo-previous-answer
		(y-or-n-p (concat "More important than '" item "'? "))))))
  todo-previous-answer)
(defalias 'todo-ask-p 'todo-more-important-p)

(defun todo-delete-item () "Delete current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((todo-entry (todo-item-string-start))
	     (todo-answer (y-or-n-p (concat "Permanently remove '"
					    todo-entry "'? "))))
	(if todo-answer
	    (progn
	      (todo-remove-item)
	      (todo-backward-item)))
	(message ""))
    (error "No TODO list entry to delete")))
(defalias 'todo-cmd-kill 'todo-delete-item)

(defun todo-raise-item () "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point)) 0)
      (let ((item (todo-item-string)))
	(todo-remove-item)
	(todo-backward-item)
	(save-excursion
	  (insert item "\n"))
	(message ""))
    (error "No TODO list entry to raise")))
(defalias 'todo-cmd-rais 'todo-raise-item)

(defun todo-lower-item () "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point) (point-max)) 1) ; Assume there is a final newline
      (let ((item (todo-item-string)))
	(todo-remove-item)
	(todo-forward-item)
	(save-excursion
	  (insert item "\n"))
	(message ""))
    (error "No TODO list entry to lower")))
(defalias 'todo-cmd-lowr 'todo-lower-item)

(defun todo-file-item () "File the current TODO list entry away."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let ((comment (read-from-minibuffer "Comment: "))
	    (time-stamp-format todo-time-string-format))
	(goto-char (todo-item-start))
	(delete-region (point) (search-forward todo-prefix))
	(insert (time-stamp-string))
	(goto-char (todo-item-end))
	(insert (if (save-excursion (beginning-of-line)
				    (looking-at (regexp-quote todo-prefix)))
		    " "
		  "\n\t")
		"(" (nth todo-category-number todo-categories) ": "
		comment ")")
	(append-to-file (todo-item-start) (todo-item-end) todo-file-done)
	(todo-remove-item)
	(todo-backward-item)
	(message ""))
    (error "No TODO list entry to file away")))

;; ---------------------------------------------------------------------------

;; Utility functions:

(defun todo-line-string () "Return current line in buffer as a string."
  (buffer-substring (point-at-bol) (point-at-eol)))

(defun todo-item-string-start ()
  "Return the start of this TODO list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (todo-item-string)))
    (if (> (length item) 60)
	(setq item (concat (substring item 0 56) "...")))
    item))

(defun todo-item-start () "Return point at start of current TODO list item."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at (regexp-quote todo-prefix)))
	(search-backward-regexp
	 (concat "^" (regexp-quote todo-prefix)) nil t))
    (point)))

(defun todo-item-end () "Return point at end of current TODO list item."
  (save-excursion
    (end-of-line)
    (search-forward-regexp (concat "^" (regexp-quote todo-prefix)) nil 'goto-end)
    (1- (point-at-bol))))

(defun todo-remove-item () "Delete the current entry from the TODO list."
  (delete-region (todo-item-start) (1+ (todo-item-end))))

(defun todo-item-string () "Return current TODO list entry as a string."
  (buffer-substring (todo-item-start) (todo-item-end)))

(defun todo-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun todo-string-multiline-p (string)
  "Returns non-nil if STRING spans several lines"
  (> (todo-string-count-lines string) 1))

(defun todo-category-alist ()
  "Generate an alist fro use in `completing-read' from `todo-categories'"
  (let (alist)
    (mapcar (lambda (cat) (setq alist (cons (cons cat nil) alist)))
	    todo-categories)
    alist))

;; utility functions:  These are available in XEmacs, but not in Emacs 19.34

(if (not (fboundp 'point-at-bol))
    (defun point-at-bol () "Return value of point at beginning of line."
      (save-excursion
	(beginning-of-line)
	(point))))

(if (not (fboundp 'point-at-eol))
    (defun point-at-eol () "Return value of point at end of line."
      (save-excursion
	(end-of-line)
	(point))))

;; ---------------------------------------------------------------------------

(easy-menu-define todo-menu todo-mode-map "Todo Menu"
		  '("Todo"
		    ["Next category"        todo-forward-category t]
		    ["Previous category"    todo-backward-category t]
		    "---"
		    ["Edit item"            todo-edit-item t]
		    ["File item"            todo-file-item t]
		    ["Insert new item"      todo-insert-item t]
		    ["Kill item"            todo-delete-item t]
		    "---"
		    ["Lower item priority"  todo-lower-item t]
		    ["Raise item priority"  todo-raise-item t]
		    "---"
		    ["Next item"            todo-forward-item t]
		    ["Previous item"        todo-backward-item t]
		    "---"
		    ["Save"                 todo-save t]
		    "---"
		    ["Quit"                 todo-quit t]
		    ))

(defun todo-mode () "Major mode for editing TODO lists.\n\n\\{todo-mode-map}"
  (interactive)
  (setq major-mode 'todo-mode)
  (setq mode-name "TODO")
  (use-local-map todo-mode-map)
  (easy-menu-add todo-menu)
  (run-hooks 'todo-mode-hook))

(defun todo-edit-mode ()
  "Major mode for editing items in the TODO list\n\n\\{todo-edit-mode-map}"
  (text-mode)
  (setq major-mode 'todo-edit-mode)
  (setq mode-name "TODO Edit")
  (run-hooks 'todo-edit-mode-hook))

(defun todo-show () "Show TODO list."
  (interactive)
  (if (file-exists-p todo-file-do)
      (find-file todo-file-do)
    (todo-initial-setup))
  (if (null todo-categories)
      (if (null todo-cats)
	  (error "Error in %s: No categories in list `todo-categories'"
		 todo-file-do)
	(make-local-variable todo-categories)
	(setq todo-categories todo-cats)))
  (beginning-of-line)
  (todo-category-select))

(defun todo-initial-setup () "Set up things to work properly in TODO mode."
  (find-file todo-file-do)
  (erase-buffer)
  (todo-mode)
  (todo-add-category "Todo"))

(provide 'todo-mode)

;; ---------------------------------------------------------------------------
;;; todo-mode.el ends here
;; ---------------------------------------------------------------------------
