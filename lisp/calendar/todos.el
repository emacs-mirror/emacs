;;; Todos.el --- facilities for making and maintaining Todo lists

;; Copyright (C) 1997, 1999, 2001-2011  Free Software Foundation, Inc.

;; Author: Oliver Seidel <privat@os10000.net>
;;         Stephen Berman <stephen.berman@gmx.net>
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

;;; Commentary:

;; UI
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
;; Internals
;; - cat props: name, number, todos, done, archived
;; - item props: priority, date-time, status?
;; - file format
;;   - cat begin
;;   - todo items 0...n
;;   - empty line
;;   - done-separator
;;   - done item 0...n

;;; Code:

(require 'diary-lib)

;; ---------------------------------------------------------------------------
;;; User options

(defgroup todos nil
  "Create and maintain categorized lists of todo items."
  :link '(emacs-commentary-link "todos")
  :version "24.1"
  :group 'calendar)

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
  "Non-nil to prefix items with consecutively increasing integers.
These reflect the priorities of the items in each category."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

;; FIXME: Update when window-width changes.  Add todos-reset-separator to
;; window-configuration-change-hook in todos-mode?  But this depends on the
;; value being window-width instead of a constant length.
(defcustom todos-done-separator (make-string (window-width) ?-)
  "String used to visual separate done from not done items.
Displayed in a before-string overlay by `todos-toggle-view-done-items'."
  :type 'string
  :initialize 'custom-initialize-default
  :set 'todos-reset-prefix
  :group 'todos)

(defcustom todos-done-string "DONE "
  "Identifying string appended to the front of done todos items."
  :type 'string
  :initialize 'custom-initialize-default
  :set 'todos-reset-done-string
  :group 'todos)

(defcustom todos-comment-string "COMMENT"
  "String inserted before optional comment appended to done item."
  :type 'string
  :initialize 'custom-initialize-default
  :set 'todos-reset-comment-string
  :group 'todos)

(defcustom todos-show-with-done nil
  "Non-nil to display done items in all categories."
  :type 'boolean
  :group 'todos)

(defun todos-mode-line-control (cat)
  "Return a mode line control for Todos buffers.
Argument CAT is the name of the current Todos category.
This function is the value of the user variable
`todos-mode-line-function'."
  (let ((file (file-name-sans-extension
	       (file-name-nondirectory todos-current-todos-file))))
  (format "%s category %d: %s" file todos-category-number cat)))

(defcustom todos-mode-line-function 'todos-mode-line-control
  "Function that returns a mode line control for Todos buffers.
The function is expected to take one argument that holds the name
of the current Todos category.  The resulting control becomes the
local value of `mode-line-buffer-identification' in each Todos
buffer."
  :type 'function
  :group 'todos)

(defcustom todos-files-directory (locate-user-emacs-file "todos/")
  "Directory where user's Todos files are saved."
  :type 'directory
  :group 'todos)

(defun todos-files (&optional archives)
  "Default value of `todos-files-function'.
This returns the case-insensitive alphabetically sorted list of
file truenames in `todos-files-directory' with the extension
\".todo\".  With non-nil ARCHIVES return the list of archive file
truenames (those with the extension \".toda\")."
  (let ((files (mapcar 'file-truename
		       (directory-files todos-files-directory t
					(if archives "\.toda$" "\.todo$") t))))
    (sort files (lambda (s1 s2) (let ((cis1 (upcase s1))
				      (cis2 (upcase s2)))
				  (string< cis1 cis2))))))

(defcustom todos-files-function 'todos-files
  "Function returning the value of the variable `todos-files'.
This function should take an optional argument that, if non-nil,
makes it return the value of the variable `todos-archives'."
  :type 'function
  :group 'todos)

(defcustom todos-filter-function nil
  ""
  :type 'function
  :group 'todos)

(defcustom todos-priorities-rules (list)
  "List of rules for choosing top priorities of each Todos file.
The rules should be set interactively by invoking
`todos-set-top-priorities'.

Each rule is a list whose first element is a member of
`todos-files', whose second element is a number specifying the
default number of top priority items for the categories in that
file, and whose third element is an alist whose elements are
conses of a category name in that file and the number of top
priority items in that category that `todos-top-priorities' shows
by default, which overrides the number for the file."
  :type 'list
  :group 'todos)

(defcustom todos-merged-files nil
  "List of files for `todos-merged-top-priorities'."
  :type `(set ,@(mapcar (lambda (x) (list 'const x))
			(funcall todos-files-function)))
  :group 'todos)

(defcustom todos-prompt-merged-files nil
  "Non-nil to prompt for merging files for `todos-filter-items'."
  :type 'boolean
  :group 'todos)

(defcustom todos-show-current-file t
  "Non-nil to make `todos-show' visit the current Todos file.
Otherwise, `todos-show' always visits `todos-default-todos-file'."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'todos-toggle-show-current-file
  :group 'todos)

;; FIXME: omit second sentence from doc string?
(defcustom todos-default-todos-file (car (funcall todos-files-function))
  "Todos file visited by first session invocation of `todos-show'.
Normally this should be set by invoking `todos-change-default-file'
either directly or as a side effect of `todos-add-file'."
  :type `(radio ,@(mapcar (lambda (x) (list 'const x))
			  (funcall todos-files-function)))
  :group 'todos)

(defcustom todos-visit-files-commands (list 'find-file 'dired-find-file)
  "List of commands to visit files for `todos-after-find-file'.
Invoking these commands to visit a Todos or Todos Archive file
calls `todos-show' or `todos-show-archive', so that the file is
displayed correctly."
  :type '(repeat function)
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

(defcustom todos-categories-totals-label "Totals"
  "String to label total item counts in `todos-categories-buffer'."
  :type 'string
  :group 'todos)

(defcustom todos-categories-number-separator " | "
  "String between number and category in `todos-categories-buffer'.
This separates the number from the category name in the default
categories display according to priority."
  :type 'string
  :group 'todos)

(defcustom todos-categories-align 'center
  "Alignment of category names in `todos-categories-buffer'."
  :type '(radio (const left) (const center) (const right))
  :group 'todos)

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

;; FIXME
(defcustom todos-edit-buffer "*Todos Edit*"
  "Name of current buffer in Todos Edit mode."
  :type 'string
  :group 'todos)

;; (defcustom todos-edit-buffer "*Todos Top Priorities*"
;;   "TODO Edit buffer name."
;;   :type 'string
;;   :group 'todos)

;; (defcustom todos-edit-buffer "*Todos Diary Entries*"
;;   "TODO Edit buffer name."
;;   :type 'string
;;   :group 'todos)

(defcustom todos-use-only-highlighted-region t
  "Non-nil to enable inserting only highlighted region as new item."
  :type 'boolean
  :group 'todos)

(defcustom todos-include-in-diary nil
  "Non-nil to allow new Todo items to be included in the diary."
  :type 'boolean
  :group 'todos)

(defcustom todos-diary-nonmarking nil
  "Non-nil to insert new Todo diary items as nonmarking by default.
This appends `diary-nonmarking-symbol' to the front of an item on
insertion provided it doesn't begin with `todos-nondiary-marker'."
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
  "Function called to print buffer content; see `todos-print'."
  :type 'symbol
  :group 'todos)

;; FIXME: rename, change meaning of zero, refer to todos-priorities-rules
(defcustom todos-show-priorities 1
  "Default number of priorities to show by `todos-top-priorities'.
0 means show all entries."
  :type 'integer
  :group 'todos)

(defcustom todos-print-priorities 0
  "Default number of priorities to print by `todos-print'.
0 means print all entries."
  :type 'integer
  :group 'todos)

(defcustom todos-completion-ignore-case t ;; FIXME: nil for release?
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
  "Non-nil to wrap long lines by `todos-line-wrapping-function'." ;FIXME
  :group 'todos
  :type 'boolean)

(defcustom todos-line-wrapping-function 'todos-wrap-and-indent
  "Function called when `todos-wrap-lines' is non-nil." ;FIXME
  :group 'todos
  :type 'function)

(defcustom todos-indent-to-here 6
    "Number of spaces `todos-line-wrapping-function' indents to."
  :type 'integer
  :group 'todos)

;; ---------------------------------------------------------------------------
;;; Faces

(defgroup todos-faces nil
  "Faces for the Todos modes."
  :version "24.1"
  :group 'todos)

(defface todos-prefix-string
  '((t :inherit font-lock-constant-face))
  "Face for Todos prefix string."
  :group 'todos-faces)

(defface todos-mark
  '((t :inherit font-lock-warning-face))
  "Face for marks on Todos items."
  :group 'todos-faces)

(defface todos-button
  '((t :inherit widget-field))
  "Face for buttons in todos-display-categories."
  :group 'todos-faces)

(defface todos-sorted-column
  '((t :inherit fringe))
  "Face for buttons in todos-display-categories."
  :group 'todos-faces)

(defface todos-archived-only
  '((t (:inherit (shadow))))
  "Face for archived-only categories in todos-display-categories."
  :group 'todos-faces)

(defface todos-search
  '((t :inherit match))
  "Face for matches found by todos-search."
  :group 'todos-faces)

(defface todos-date
  '((t :inherit diary))
  "Face for Todos prefix string."
  :group 'todos-faces)
(defvar todos-date-face 'todos-date)

(defface todos-time
  '((t :inherit diary-time))
  "Face for Todos prefix string."
  :group 'todos-faces)
(defvar todos-time-face 'todos-time)

(defface todos-done
  '((t :inherit font-lock-comment-face))
  "Face for done Todos item header string."
  :group 'todos-faces)
(defvar todos-done-face 'todos-done)

(defface todos-comment
  '((t :inherit font-lock-comment-face))
  "Face for comments appended to done Todos items."
  :group 'todos-faces)
(defvar todos-comment-face 'todos-comment)

(defface todos-done-sep
  '((t :inherit font-lock-type-face))
  "Face for separator string bewteen done and not done Todos items."
  :group 'todos-faces)
(defvar todos-done-sep-face 'todos-done-sep)

(defvar todos-font-lock-keywords
  (list
   '(todos-date-string-matcher 1 todos-date-face t)
   '(todos-time-string-matcher 1 todos-time-face t)
   '(todos-done-string-matcher 0 todos-done-face t)
   '(todos-comment-string-matcher 1 todos-done-face t)
   '(todos-category-string-matcher 1 todos-done-sep-face t))
  "Font-locking for Todos mode.")

;; ---------------------------------------------------------------------------
;;; Modes setup

(defvar todos-files (funcall todos-files-function)
  "List of truenames of user's Todos files.")

(defvar todos-archives (funcall todos-files-function t)
  "List of truenames of user's Todos archives.")

(defvar todos-categories nil
  "Alist of categories in the current Todos file.
The elements are cons cells whose car is a category name and
whose cdr is a vector of the category's item counts.  These are,
in order, the numbers of todo items, todo items included in the
Diary, done items and archived items.")

(defvar todos-categories-full nil
  "Variable holding non-truncated copy of `todos-categories'.
Set when `todos-ignore-archived-categories' is set to non-nil, to
restore full `todos-categories' list when
`todos-ignore-archived-categories' is reset to nil.")

(defvar todos-current-todos-file nil
  "Variable holding the name of the currently active Todos file.")
;; Automatically set by `todos-switch-todos-file'.")

;; FIXME: Add function to kill-buffer-hook that sets this to the latest
;; existing Todos file or else todos-default-todos-file on killing the buffer
;; of a Todos file
(defvar todos-global-current-todos-file nil
  "Variable holding name of current Todos file.
Used by functions called from outside of Todos mode to visit the
current Todos file rather than the default Todos file (i.e. when
users option `todos-show-current-file' is non-nil).")

(defun todos-reset-global-current-todos-file ()
  ""
  (let ((buflist (copy-sequence (buffer-list)))
	(cur todos-global-current-todos-file))
    (catch 'done
      (while buflist
	(let* ((buf (pop buflist))
	       (bufname (buffer-file-name buf)))
	  (when bufname (setq bufname (file-truename bufname)))
	  (when (and (member bufname todos-files)
		     (not (eq buf (current-buffer))))
	    (setq todos-global-current-todos-file bufname)
	    (throw 'done nil)))))
    (if (equal cur todos-global-current-todos-file)
	(setq todos-global-current-todos-file todos-default-todos-file))))    

(defvar todos-category-number 1
  "Variable holding the number of the current Todos category.
This number is one more than the index of the category in
`todos-categories'.")

(defvar todos-first-visit t
  "Non-nil if first display of this file in the current session.
See `todos-display-categories-first'.")

;; FIXME: rename?
(defvar todos-tmp-buffer-name " *todo tmp*")

(defvar todos-category-beg "--==-- "
  "String marking beginning of category (inserted with its name).")

(defvar todos-category-done "==--== DONE "
  "String marking beginning of category's done items.")

(defvar todos-nondiary-start (nth 0 todos-nondiary-marker)
  "String inserted before item date to block diary inclusion.")

(defvar todos-nondiary-end (nth 1 todos-nondiary-marker)
  "String inserted after item date matching `todos-nondiary-start'.")

(defvar todos-show-done-only nil
  "If non-nil display only done items in current category.
Set by `todos-toggle-show-done-only' and used by
`todos-category-select'.")

;;; Todos insertion commands, key bindings and keymap

;; http://rosettacode.org/wiki/Power_set#Common_Lisp (GFDL)
(defun powerset (l)
  (if (null l)
      (list nil)
    (let ((prev (powerset (cdr l))))
      (append (mapcar #'(lambda (elt) (cons (car l) elt)) prev)
	      prev))))

;; Return list of lists of non-nil atoms produced from ARGLIST.  The elements
;; of ARGLIST may be atoms or lists.
(defun todos-gen-arglists (arglist)
  (let (arglists)
    (while arglist
      (let ((arg (pop arglist)))
	(cond ((symbolp arg)
	       (setq arglists (if arglists
				  (mapcar (lambda (l) (push arg l)) arglists)
				(list (push arg arglists)))))
	      ((listp arg)
	       (setq arglists
		     (mapcar (lambda (a)
			       (if (= 1 (length arglists))
				   (apply (lambda (l) (push a l)) arglists)
				 (mapcar (lambda (l) (push a l)) arglists)))
			     arg))))))
    (setq arglists (mapcar 'reverse (apply 'append (mapc 'car arglists))))))

(defvar todos-insertion-commands-args-genlist
  '(diary nonmarking (calendar date dayname) time (here region))
  "Generator list for argument lists of Todos insertion commands.")

(eval-when-compile (require 'cl))	; remove-duplicates

(defvar todos-insertion-commands-args
  (let ((argslist (todos-gen-arglists todos-insertion-commands-args-genlist))
	res new)
    (setq res (remove-duplicates
	       (apply 'append (mapcar 'powerset argslist)) :test 'equal))
    (dolist (l res)
      (unless (= 5 (length l))
	(let ((v (make-vector 5 nil)) elt)
	  (while l
	    (setq elt (pop l))
	    (cond ((eq elt 'diary)
		   (aset v 0 elt))
		  ((eq elt 'nonmarking)
		   (aset v 1 elt))
		  ((or (eq elt 'calendar)
		       (eq elt 'date)
		       (eq elt 'dayname))
		   (aset v 2 elt))
		  ((eq elt 'time)
		   (aset v 3 elt))
		  ((or (eq elt 'here)
		       (eq elt 'region))
		   (aset v 4 elt))))
	  (setq l (append v nil))))
      (setq new (append new (list l))))
    new)
  "List of all argument lists for Todos insertion commands.")

(defun todos-insertion-command-name (arglist)
  "Generate Todos insertion command name from ARGLIST."
  (replace-regexp-in-string
   "-\\_>" ""
   (replace-regexp-in-string
    "-+" "-"
    (concat "todos-item-insert-"
	    (mapconcat (lambda (e) (if e (symbol-name e))) arglist "-")))))

(defvar todos-insertion-commands-names
  (mapcar (lambda (l)
	   (todos-insertion-command-name l))
	  todos-insertion-commands-args)
  "List of names of Todos insertion commands.")

(defmacro todos-define-insertion-command (&rest args)
  (let ((name (intern (todos-insertion-command-name args)))
	(arg0 (nth 0 args))
	(arg1 (nth 1 args))
	(arg2 (nth 2 args))
	(arg3 (nth 3 args))
	(arg4 (nth 4 args)))
    `(defun ,name (&optional arg)
       "Todos item insertion command."
       (interactive)
       (todos-insert-item arg ',arg0 ',arg1 ',arg2 ',arg3 ',arg4))))

(defvar todos-insertion-commands
  (mapcar (lambda (c)
	  (eval `(todos-define-insertion-command ,@c)))
	todos-insertion-commands-args)
  "List of Todos insertion commands.")

(defvar todos-insertion-commands-arg-key-list
  '(("diary" "y" "yy")
    ("nonmarking" "k" "kk")
    ("calendar" "c" "cc")
    ("date" "d" "dd")
    ("dayname" "n" "nn")
    ("time" "t" "tt")
    ("here" "h" "h")
    ("region" "r" "r"))
  "")    

(defun todos-insertion-key-bindings (map)
  ""
  (dolist (c todos-insertion-commands)
    (let* ((key "")
	   (cname (symbol-name c)))
      ;; (if (string-match "diary\\_>" cname) (setq key (concat key "yy")))
      ;; (if (string-match "diary.+" cname) (setq key (concat key "y")))
      ;; (if (string-match "nonmarking\\_>" cname) (setq key (concat key "kk")))
      ;; (if (string-match "nonmarking.+" cname) (setq key (concat key "k")))
      ;; (if (string-match "calendar\\_>" cname) (setq key (concat key "cc")))
      ;; (if (string-match "calendar.+" cname) (setq key (concat key "c")))
      ;; (if (string-match "date\\_>" cname) (setq key (concat key "dd")))
      ;; (if (string-match "date.+" cname) (setq key (concat key "d")))
      ;; (if (string-match "dayname\\_>" cname) (setq key (concat key "nn")))
      ;; (if (string-match "dayname.+" cname) (setq key (concat key "n")))
      ;; (if (string-match "time\\_>" cname) (setq key (concat key "tt")))
      ;; (if (string-match "time.+" cname) (setq key (concat key "t")))
      ;; (if (string-match "here" cname) (setq key (concat key "h")))
      ;; (if (string-match "region" cname) (setq key (concat key "r")))
      (mapc (lambda (l)
	      (let ((arg (nth 0 l))
		    (key1 (nth 1 l))
		    (key2 (nth 2 l)))
		(if (string-match (concat (regexp-quote arg) "\\_>") cname)
		    (setq key (concat key key2)))
		(if (string-match (concat (regexp-quote arg) ".+") cname)
		    (setq key (concat key key1)))))
	    todos-insertion-commands-arg-key-list)
      (if (string-match (concat (regexp-quote "todos-item-insert") "\\_>") cname)
	  (setq key (concat key "i")))
      (define-key map key c))))

(defvar todos-insertion-map
  (let ((map (make-keymap)))
    (todos-insertion-key-bindings map)
    map)
  "Keymap for Todos mode insertion commands.")

(defvar todos-mode-map
  (let ((map (make-keymap)))
    ;; Don't suppress digit keys, so they can supply prefix arguments.
    (suppress-keymap map)
    ;; display commands
    (define-key map "Cd" 'todos-display-categories) ;FIXME: Cs todos-show-categories?
    ;; (define-key map "" 'todos-display-categories-alphabetically)
    (define-key map "H" 'todos-highlight-item)
    (define-key map "N" 'todos-toggle-item-numbering)
    (define-key map "D" 'todos-toggle-display-date-time)
    (define-key map "*" 'todos-toggle-mark-item)
    (define-key map "C*" 'todos-mark-category)
    (define-key map "Cu" 'todos-unmark-category)
    (define-key map "P" 'todos-print)
    ;; (define-key map "" 'todos-print-to-file)
    (define-key map "v" 'todos-toggle-view-done-items)
    (define-key map "V" 'todos-toggle-show-done-only)
    (define-key map "Av" 'todos-view-archived-items)
    (define-key map "As" 'todos-show-archive)
    (define-key map "Ac" 'todos-choose-archive)
    (define-key map "Y" 'todos-diary-items)
    ;; (define-key map "" 'todos-update-merged-files)
    ;; (define-key map "" 'todos-set-top-priorities)
    (define-key map "Ftt" 'todos-top-priorities)
    (define-key map "Ftm" 'todos-merged-top-priorities)
    (define-key map "Fdd" 'todos-diary-items)
    (define-key map "Fdm" 'todos-merged-diary-items)
    (define-key map "Frr" 'todos-regexp-items)
    (define-key map "Frm" 'todos-merged-regexp-items)
    (define-key map "Fcc" 'todos-custom-items)
    (define-key map "Fcm" 'todos-merged-custom-items)
    ;; (define-key map "" 'todos-save-top-priorities)
    ;; navigation commands
    (define-key map "f" 'todos-forward-category)
    (define-key map "b" 'todos-backward-category)
    (define-key map "j" 'todos-jump-to-category)
    (define-key map "J" 'todos-jump-to-category-other-file)
    (define-key map "n" 'todos-forward-item)
    (define-key map "p" 'todos-backward-item)
    (define-key map "S" 'todos-search)
    (define-key map "X" 'todos-clear-matches)
    ;; editing commands
    (define-key map "Fa" 'todos-add-file)
    ;; (define-key map "" 'todos-change-default-file)
    (define-key map "Ca" 'todos-add-category)
    (define-key map "Cr" 'todos-rename-category)
    (define-key map "Cg" 'todos-merge-category)
    ;; (define-key map "" 'todos-merge-categories)
    (define-key map "Cm" 'todos-move-category)
    (define-key map "Ck" 'todos-delete-category)
    (define-key map "d" 'todos-item-done)
    (define-key map "ee" 'todos-edit-item)
    (define-key map "em" 'todos-edit-multiline)
    (define-key map "eh" 'todos-edit-item-header)
    (define-key map "ed" 'todos-edit-item-date)
    (define-key map "ey" 'todos-edit-item-date-is-today)
    (define-key map "et" 'todos-edit-item-time)
    (define-key map "ec" 'todos-comment-done-item) ;FIXME: or just "c"?
    (define-key map "i" todos-insertion-map)
    (define-key map "k" 'todos-delete-item)
    (define-key map "m" 'todos-move-item)
    (define-key map "M" 'todos-move-item-to-file)
    ;; FIXME: This prevents `-' from being used in a numerical prefix argument
    ;; without typing C-u
    (define-key map "-" 'todos-raise-item-priority)
    (define-key map "r" 'todos-raise-item-priority)
    (define-key map "+" 'todos-lower-item-priority)
    (define-key map "l" 'todos-lower-item-priority)
    (define-key map "#" 'todos-set-item-priority)
    (define-key map "u" 'todos-item-undo)
    (define-key map "Ad" 'todos-archive-done-item-or-items)  ;FIXME
    (define-key map "AD" 'todos-archive-category-done-items) ;FIXME
    ;; (define-key map "" 'todos-unarchive-items)
    ;; (define-key map "" 'todos-unarchive-category)
    (define-key map "y" 'todos-toggle-diary-inclusion)
    ;; (define-key map "" 'todos-toggle-diary-inclusion)
    ;; (define-key map "" 'todos-toggle-item-diary-nonmarking)
    ;; (define-key map "" 'todos-toggle-diary-nonmarking)
    (define-key map "s" 'todos-save)
    (define-key map "q" 'todos-quit)
    (define-key map [remap newline] 'newline-and-indent)
    map)
  "Todos mode keymap.")

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
     ;; ["List Categories Alphabetically" todos-display-categories-alphabetically t]
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
     ["Print Category"     todos-print t]) ;FIXME
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
     ["Archive Done Items" todos-archive-category-done-items t] ;FIXME
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
    ;; (define-key map "u" 'todos-unarchive-item)
    (define-key map "U" 'todos-unarchive-category)
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
    ;; (define-key map "a" 'todos-display-categories-alphabetically)
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

(defvar todos-filter-items-mode-map
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

;; Autoloading isn't needed if files are identified by auto-mode-alist
;; ;; As calendar reads included Todos file before todos-mode is loaded.
;; ;;;###autoload
(define-derived-mode todos-mode nil "Todos" () ;FIXME: derive from special-mode?
  "Major mode for displaying, navigating and editing Todo lists.

\\{todos-mode-map}"
  (easy-menu-add todos-menu)
  (todos-modes-set-1)
  (todos-modes-set-2)
  (when (member (file-truename (buffer-file-name))
		(funcall todos-files-function))
    (set (make-local-variable 'todos-current-todos-file)
  	 (file-truename (buffer-file-name))))
  (set (make-local-variable 'todos-categories-full) nil)
  ;; todos-set-categories sets todos-categories-full.
  (set (make-local-variable 'todos-categories) (todos-set-categories))
  (set (make-local-variable 'todos-first-visit) t)
  (set (make-local-variable 'todos-category-number) 1) ;0)
  (set (make-local-variable 'todos-show-done-only) nil)
  (set (make-local-variable 'todos-categories-with-marks) nil)
  (when todos-show-current-file
    (add-hook 'pre-command-hook 'todos-show-current-file nil t))
  (add-hook 'post-command-hook 'todos-after-find-file nil t)
  (add-hook 'kill-buffer-hook 'todos-reset-global-current-todos-file nil t))

;; FIXME:
(defun todos-unload-hook ()
  ""
  (remove-hook 'pre-command-hook 'todos-show-current-file t)
  (remove-hook 'post-command-hook 'todos-after-find-file t)
  (remove-hook 'kill-buffer-hook 'todos-reset-global-current-todos-file t))

(define-derived-mode todos-archive-mode nil "Todos-Arch" ()
  "Major mode for archived Todos categories.

\\{todos-archive-mode-map}"
  (todos-modes-set-1)
  (todos-modes-set-2)
  (set (make-local-variable 'todos-show-done-only) t)
  (set (make-local-variable 'todos-current-todos-file)
       (file-truename (buffer-file-name)))
  (set (make-local-variable 'todos-categories) (todos-set-categories))
  (set (make-local-variable 'todos-category-number) 1) ; 0)
  (add-hook 'post-command-hook 'todos-after-find-file nil t))

;; FIXME: return to Todos or Archive mode
(define-derived-mode todos-raw-mode nil "Todos Raw" ()
  "Emergency repair mode for Todos files."
  (when (member major-mode '(todos-mode todos-archive-mode))
    (setq buffer-read-only nil)
    (set (make-local-variable 'font-lock-defaults) '(todos-font-lock-keywords t))
    (widen)
    ;; FIXME: doesn't DTRT here
    (todos-prefix-overlays)))

(define-derived-mode todos-edit-mode nil "Todos-Ed" ()
  "Major mode for editing multiline Todo items.

\\{todos-edit-mode-map}"
  (todos-modes-set-1))

(define-derived-mode todos-categories-mode nil "Todos-Cats" ()
  "Major mode for displaying and editing Todos categories.

\\{todos-categories-mode-map}"
  (set (make-local-variable 'todos-current-todos-file)
       todos-global-current-todos-file)
  (let ((cats (with-current-buffer (get-file-buffer todos-current-todos-file)
		(if todos-ignore-archived-categories
		    todos-categories-full
		  (todos-set-categories)))))
    (set (make-local-variable 'todos-categories) cats)))

(define-derived-mode todos-filter-items-mode nil "Todos-Top" ()
  "Mode for displaying and reprioritizing top priority Todos.

\\{todos-filter-items-mode-map}"
  (todos-modes-set-1)
  (todos-modes-set-2))

;; FIXME: need this?
(defun todos-save ()
  "Save the current Todos file."
  (interactive)
  ;; (todos-update-categories-sexp)
  (save-buffer)
  ;; (if todos-save-top-priorities-too (todos-save-top-priorities))
  )

(defun todos-quit ()
  "Exit the current Todos-related buffer.
Depending on the specific mode, this either kills and the buffer
or buries it."
  (interactive)
  (cond ((eq major-mode 'todos-categories-mode)
	 (kill-buffer)
	 (setq todos-descending-counts nil)
	 (todos-show))
	((eq major-mode 'todos-filter-items-mode)
	 (kill-buffer)
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

With non-nil prefix argument SOLICIT-FILE ask for file to visit.
Otherwise, the first invocation of this command in a session
visits `todos-default-todos-file' (creating it if it does not yet
exist); subsequent invocations from outside of Todos mode revisit
this file or, if user option `todos-show-current-file' is
non-nil, whichever Todos file was visited last.

The category displayed on initial invocation is the first member
of `todos-categories' for the current Todos file, on subsequent
invocations whichever category was displayed last.  If
`todos-display-categories-first' is non-nil, then the first
invocation of `todos-show' displays a clickable listing of the
categories in the current Todos file."
  (interactive "P")
  (let ((file (cond (solicit-file
		     (if (funcall todos-files-function)
			 (todos-read-file-name "Select a Todos file to visit: "
					       nil t)
		       (error "There are no Todos files")))
		    ((eq major-mode 'todos-archive-mode)
		     ;; FIXME: should it visit same category?
		     (concat (file-name-sans-extension todos-current-todos-file)
			     ".todo"))
		    (t
		     (or todos-current-todos-file
			 (and todos-show-current-file
			      todos-global-current-todos-file)
			 todos-default-todos-file
			 (todos-add-file))))))
    (if (and todos-first-visit todos-display-categories-first)
	(todos-display-categories)
      (set-window-buffer (selected-window)
			 (set-buffer (find-file-noselect file)))
      ;; If no Todos file exists, initialize one.
      (if (zerop (buffer-size))
	  ;; Call with empty category name to get initial prompt.
	  (setq todos-category-number (todos-add-category "")))
      (save-excursion (todos-category-select)))
    (setq todos-first-visit nil)))

(defun todos-toggle-item-numbering ()
  ""
  (interactive)
  (todos-reset-prefix 'todos-number-prefix (not todos-number-prefix)))

(defun todos-toggle-view-done-items ()
  "Show hidden or hide visible done items in current category."
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

;; FIXME: should there be `todos-toggle-view-todo-items'?
(defun todos-toggle-show-done-only ()
  "Make category display done or back to todo items." ;FIXME
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
	(set-window-buffer (selected-window) (set-buffer
					      (find-file-noselect afile)))
	(todos-category-number cat)
	(todos-jump-to-category cat)))))

(defun todos-show-archive (&optional ask)
  "Visit the archive of the current Todos file, if it exists.
With non-nil argument ASK prompt to choose an archive to visit;
see `todos-choose-archive'.  The buffer showing the archive is in
Todos Archive mode.  The first visit in a session displays the
first category in the archive, subsequent visits return to the
last category displayed."
  (interactive)
  (let* ((tfile-base (file-name-sans-extension todos-current-todos-file))
	 (afile (if ask
		    (todos-read-file-name "Choose a Todos archive: " t t)
		  (concat tfile-base ".toda"))))
    (if (not (file-exists-p afile))
	(message "There is currently no Todos archive for this file.")
      (set-window-buffer (selected-window) (set-buffer
					    (find-file-noselect afile)))
      (todos-category-select))))

(defun todos-choose-archive ()
  "Choose an archive and visit it."
  (interactive)
  (todos-show-archive t))

(defun todos-highlight-item ()
  "Highlight the todo item the cursor is on."
  (interactive)
  (if hl-line-mode ; todos-highlight-item
      (hl-line-mode 0)
    (hl-line-mode 1)))

(defun todos-toggle-display-date-time (&optional all)
  "Hide or show date/time of todo items in current category.
With non-nil prefix argument ALL do this in the whole file."
  (interactive "P")
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (let ((ovs (overlays-in (point) (1+ (point))))
	    ov hidden)
	(while ovs
	  (setq ov (pop ovs))
	  (if (equal (overlay-get ov 'display) "")
	      (setq ovs nil hidden t)))
	(when all (widen) (goto-char (point-min)))
	(if hidden
	    (remove-overlays (point-min) (point-max) 'display "")
	  (while (not (eobp))
	    (when (re-search-forward
		   (concat todos-date-string-start todos-date-pattern
			   "\\( " diary-time-regexp "\\)?"
			   (regexp-quote todos-nondiary-end) "? ")
		   nil t)
	      (unless (save-match-data (todos-done-item-p))
		(setq ov (make-overlay (match-beginning 0) (match-end 0) nil t))
		(overlay-put ov 'display "")))
	    (todos-forward-item)))))))

(defun todos-toggle-mark-item (&optional n all)
  "Mark item at point if unmarked, or unmark it if marked.

With a positive numerical prefix argument N, change the
markedness of the next N items.  With non-nil argument ALL, mark
all visible items in the category (depending on visibility, all
todo and done items, or just todo or just done items).

The mark is the character \"*\" inserted in front of the item's
priority number or the `todos-prefix' string; if `todos-prefix'
is \"*\", then the mark is \"@\"."
  (interactive "p")
  (if all (goto-char (point-min)))
  (unless (> n 0) (setq n 1))
  (let ((i 0))
    (while (or (and all (not (eobp)))
	       (< i n))
      (let* ((cat (todos-current-category))
	     (ov (todos-item-marked-p))
	     (marked (assoc cat todos-categories-with-marks)))
	(if (and ov (not all))
	    (progn
	      (delete-overlay ov)
	      (if (= (cdr marked) 1)	; Deleted last mark in this category.
		  (setq todos-categories-with-marks
			(assq-delete-all cat todos-categories-with-marks))
		(setcdr marked (1- (cdr marked)))))
	  (when (todos-item-start)
	    (unless (and all (todos-item-marked-p))
	      (setq ov (make-overlay (point) (point)))
	      (overlay-put ov 'before-string todos-item-mark)
	      (if marked
		  (setcdr marked (1+ (cdr marked)))
		(push (cons cat 1) todos-categories-with-marks))))))
      (todos-forward-item)
      (setq i (1+ i)))))

(defun todos-mark-category ()
  "Put the \"*\" mark on all items in this category.
\(If `todos-prefix' is \"*\", then the mark is \"@\".)"
  (interactive)
  (todos-toggle-mark-item 0 t))

(defun todos-unmark-category ()
  "Remove the \"*\" mark from all items in this category.
\(If `todos-prefix' is \"*\", then the mark is \"@\".)"
  (interactive)
  (remove-overlays (point-min) (point-max) 'before-string todos-item-mark)
  (setq todos-categories-with-marks
	(delq (assoc (todos-current-category) todos-categories-with-marks)
	      todos-categories-with-marks)))

(defun todos-update-merged-files ()
  "Interactively add files to or remove from `todos-merged-files'.
You can also customize `todos-merged-files' directly."
  (interactive)				;FIXME
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

(defvar todos-top-priorities-widgets nil
  "Widget placeholder used by `todos-set-top-priorities'.
This variable temporarily holds user changed values which are
saved to `todos-priorities-rules'.")

(defun todos-set-top-priorities ()
  ""
  (interactive)
  (let ((buf (get-buffer-create "*Todos Top Priorities*"))
	(files (funcall todos-files-function))
	file frules cats fwidget cwidgets rules)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer))
      (remove-overlays)
      (kill-all-local-variables)
      (setq todos-top-priorities-widgets nil)
      (dolist (f files)
	(with-temp-buffer
	  (insert-file-contents f)
	  (setq file (file-name-sans-extension (file-name-nondirectory f))
		frules (assoc file todos-priorities-rules)
		cats (mapcar 'car (todos-set-categories))))
	(setq fwidget
	      (widget-create 'editable-field
			     :size 2
			     :value (or (and frules (cadr frules))
					"")
			     :tag file
			     :format " %v : %t\n"))
	(dolist (c cats)
	  (let ((tp-num (cdr (assoc c cats)))
		cwidget)
	    (widget-insert "  ")
	    (setq cwidget (widget-create 'editable-field
					 :size 2
					 :value (or tp-num "")
					 :tag c
					 :format " %v : %t\n"))
	    (push cwidget cwidgets)))
	(push (cons fwidget cwidgets) todos-top-priorities-widgets))
      (widget-insert "\n\n")
      (widget-create 'push-button
		     :notify (lambda (widget &rest ignore)
			       (kill-buffer))
		     "Cancel")
      (widget-insert " ")
      (widget-create 'push-button
		     :notify (lambda (&rest ignore)
			       (let ((widgets todos-top-priorities-widgets)
				     (rules todos-priorities-rules)
				     tp-cats)
				 (setq rules nil)
				 (dolist (w widgets)
				   (let* ((fwid (car w))
					  (cwids (cdr w))
					  (fname (widget-get fwid :tag))
					  (fval (widget-value fwid)))
				     (dolist (c cwids)
				       (let ((cat (widget-get c :tag))
					     (cval (widget-value c)))
					 (push (cons cat cval) tp-cats)))
				     (push (list fname fval tp-cats) rules)))
				 (setq todos-priorities-rules rules)
				 (customize-save-variable 'todos-priorities-rules
							  todos-priorities-rules)))
		     "Apply")
      (use-local-map widget-keymap)
      (widget-setup))
    (set-window-buffer (selected-window) (set-buffer buf))))

(defun todos-filter-items (&optional filter merge)
  "Display a filtered list of items from different categories.

The special items are either the first NUM items (the top priority items) or the items marked as diary entries in each category of the current Todos file.

Number of entries for each category is given by NUM, which
defaults to `todos-show-priorities'.  With non-nil argument
MERGE list top priorities of all Todos files in
`todos-merged-files'.  If `todos-prompt-merged-files' is non-nil,
prompt to update the list of merged files."
  (let ((num (if (consp filter) (cdr filter) todos-show-priorities))
	(buf (get-buffer-create todos-tmp-buffer-name))
	(files (list todos-current-todos-file))
	regexp fname bufstr cat beg end done)
    (when merge
      ;; FIXME: same or different treatment for top priorities and other
      ;; filters?  And what about todos-prompt-merged-files?
      (setq files (if (member filter '(diary regexp custom))
		      (or (and todos-prompt-merged-files
			       (todos-update-merged-files))
			  todos-merged-files
			  (todos-update-merged-files))
		    ;; Set merged files for top priorities.
		    (or (mapcar (lambda (f)
			      (let ((file (car f))
				    (val (nth 1 f)))
				(and val (not (zerop val))
				     (push file files))))
			    todos-priorities-rules)
			(if (y-or-n-p "Choose files for merging top priorities? ")
			    (progn (todos-set-top-priorities) (error ""))
			  (error "No files are set for merging top priorities"))))))
    (with-current-buffer buf
      (erase-buffer)
      (kill-all-local-variables)
      (todos-filter-items-mode))
    (when (eq filter 'regexp)
      (setq regexp (read-string "Enter a regular expression: ")))
    (save-current-buffer
      (dolist (f files)
	(setq fname (file-name-sans-extension (file-name-nondirectory f)))
	(with-temp-buffer
	  (insert-file-contents f)
	  (goto-char (point-min))
	  ;; Unless the number of items to show was supplied by prefix
	  ;; argument of caller, override `todos-show-priorities' with the
	  ;; nonzero file-wide value from `todos-priorities-rules'.
	  (unless (consp filter)
	    (let ((tp-val (nth 1 (assoc fname todos-priorities-rules))))
	      (unless (zerop (length tp-val))
		(setq num (string-to-number tp-val)))))
	  (unless (looking-at (concat "^" (regexp-quote todos-category-beg)))
	    (kill-line 1))
	  (while (re-search-forward
		  (concat "^" (regexp-quote todos-category-beg) "\\(.+\\)\n")
		  nil t)
	    (setq cat (match-string 1))
	    ;; Unless the number of items to show was supplied by prefix
	    ;; argument of caller, override `todos-show-priorities' with the
	    ;; nonzero category-wide value from `todos-priorities-rules'.
	    (unless (consp filter)
	      (let* ((cats (nth 2 (assoc fname todos-priorities-rules)))
		     (tp-val (cdr (assoc cat cats))))
		(unless (zerop (length tp-val))
		  (setq num (string-to-number tp-val)))))
	    (delete-region (match-beginning 0) (match-end 0))
	    (setq beg (point)) ; Start of first entry.
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
	    (narrow-to-region beg end)	; Process current category.
	    (goto-char (point-min))
	    ;; Apply the filter.
	    (cond ((eq filter 'diary)
		   (while (not (eobp))
		     (if (looking-at (regexp-quote todos-nondiary-start))
			 (todos-remove-item)
		       (todos-forward-item))))
		  ((eq filter 'regexp)
		   (while (not (eobp))
		     (if (string-match regexp (todos-item-string))
			 (todos-forward-item)
		       (todos-remove-item))))
		  ((eq filter 'custom)
		   (if todos-filter-function
		       (funcall todos-filter-function)
		     (error "No custom filter function has been defined")))
		  (t ; Filter top priority items.
		   (todos-forward-item num)))
	      (setq beg (point))
	      (unless (member filter '(diary regexp custom))
		(delete-region beg end))
	      (goto-char (point-min))
	      ;; Add file (if using merged files) and category tags to item.
	      (while (not (eobp))
		(when (re-search-forward
		       (concat todos-date-string-start todos-date-pattern
			       "\\( " diary-time-regexp "\\)?"
			       (regexp-quote todos-nondiary-end) "?")
		       nil t)
		  (insert (concat " [" (if merge (concat fname ":")) cat "]")))
		(forward-line))
	      (widen))
	  (setq bufstr (buffer-string))
	  (with-current-buffer buf
	    (let (buffer-read-only)
	      (insert bufstr))))))
    (set-window-buffer (selected-window) (set-buffer buf))
    (todos-prefix-overlays)
    (goto-char (point-min))
    ;; FIXME: this is necessary -- why?
    (font-lock-fontify-buffer)))

(defun todos-top-priorities (&optional num)
  "List top priorities of each category in `todos-merged-files'.
Number of entries for each category is given by NUM, which
defaults to `todos-show-priorities'."
  (interactive "p")
  (let ((arg (if num (cons 'top num) 'top)))
    (todos-filter-items arg)))

(defun todos-merged-top-priorities (&optional num)
  "List top priorities of each category in `todos-merged-files'.
Number of entries for each category is given by NUM, which
defaults to `todos-show-priorities'."
  (interactive "p")
  (let ((arg (if num (cons 'top num) 'top)))
    (todos-filter-items arg t)))

(defun todos-diary-items ()
  "Display todo items for diary inclusion in this Todos file."
  (interactive)
  (todos-filter-items 'diary))

(defun todos-merged-diary-items ()
  "Display todo items for diary inclusion in one or more Todos file.
The files are those listed in `todos-merged-files'."
  (interactive)
  (todos-filter-items 'diary t))

(defun todos-regexp-items ()
  "Display todo items matching a user-entered regular expression.
The items are those in the current Todos file."
  (interactive)
  (todos-filter-items 'regexp))

(defun todos-merged-regexp-items ()
  "Display todo items matching a user-entered regular expression.
The items are those in the files listed in `todos-merged-files'."
  (interactive)
  (todos-filter-items 'regexp t))

(defun todos-custom-items ()
  "Display todo items filtered by `todos-filter-function'.
The items are those in the current Todos file."
  (interactive)
  (todos-filter-items 'custom))

(defun todos-merged-custom-items ()
  "Display todo items filtered by `todos-filter-function'.
The items are those in the files listed in `todos-merged-files'."
  (interactive)
  (todos-filter-items 'custom t))

;;; Navigation

(defun todos-forward-category (&optional back)
  "Visit the numerically next category in this Todos file.
With non-nil argument BACK, visit the numerically previous
category."
  (interactive)
  (setq todos-category-number
        (1+ (mod (- todos-category-number (if back 2 0))
		 (length todos-categories))))
  (todos-category-select)
  (goto-char (point-min)))

(defun todos-backward-category ()
  "Visit the numerically previous category in this Todos file."
  (interactive)
  (todos-forward-category t))

;; FIXME: autoload?
(defun todos-jump-to-category (&optional cat other-file)
  "Jump to a category in this or another Todos file.
Optional argument CAT provides the category name.  Otherwise,
prompt for the category, with TAB completion on existing
categories.  If a non-existing category name is entered, ask
whether to add a new category with this name, if affirmed, do so,
then jump to that category.  With non-nil argument OTHER-FILE,
prompt for a Todos file, otherwise jump within the current Todos
file."
  (interactive)
  (let ((file (or (and other-file
		       (todos-read-file-name "Choose a Todos file: " nil t))
		  ;; Jump to archived-only Categories from Todos Categories mode.
		  (and cat
		       todos-ignore-archived-categories
		       (zerop (todos-get-count 'todo cat))
		       (zerop (todos-get-count 'done cat))
		       (not (zerop (todos-get-count 'archived cat)))
		       (concat (file-name-sans-extension
				todos-current-todos-file) ".toda"))
		  todos-current-todos-file
		  ;; If invoked from outside of Todos mode before todos-show...
		  todos-default-todos-file)))
	(with-current-buffer (find-file-noselect file)
	  (and other-file (setq todos-current-todos-file file))
	  (let ((category (or (and (assoc cat todos-categories) cat)
			      (todos-read-category "Jump to category: "))))
	    ;; ;; FIXME: why is this needed?
	    ;; (if (string= "" category)
	    ;; 	(setq category (todos-current-category)))
	    ;; Clean up after selecting category in Todos Categories mode.
	    (if (string= (buffer-name) todos-categories-buffer)
		(kill-buffer))
	    (if (or cat other-file)
		(set-window-buffer (selected-window)
				   (set-buffer (get-file-buffer file))))
	    (unless todos-global-current-todos-file
	      (setq todos-global-current-todos-file todos-current-todos-file))
	    (todos-category-number category)
	    (if (> todos-category-number (length todos-categories))
		(setq todos-category-number (todos-add-category category)))
	    (todos-category-select)
	    (goto-char (point-min))))))

(defun todos-jump-to-category-other-file ()
  "Jump to a category in another Todos file.
The category is chosen by prompt, with TAB completion."
  (interactive)
  (todos-jump-to-category nil t))

;; FIXME ? disallow prefix arg value < 1 (re-search-* allows these)
(defun todos-forward-item (&optional count)
  "Move point down to start of item with next lower priority.
With numerical prefix COUNT, move point COUNT items downward,"
  (interactive "P")
  (let* ((not-done (not (or (todos-done-item-p) (looking-at "^$"))))
	 (start (line-end-position)))
    (goto-char start)
    (if (re-search-forward todos-item-start nil t (or count 1))
	(goto-char (match-beginning 0))
      (goto-char (point-max)))
    ;; If points advances by one from a todo to a done item, go back to the
    ;; space above todos-done-separator, since that is a legitimate place to
    ;; insert an item.  But skip this space if count > 1, since that should
    ;; only stop on an item (FIXME: or not?)
    (when (and not-done (todos-done-item-p))
      (if (or (not count) (= count 1))
	  (re-search-backward "^$" start t)))))

(defun todos-backward-item (&optional count)
  "Move point up to start of item with next higher priority.
With numerical prefix COUNT, move point COUNT items upward,"
  (interactive "P")
  (let* ((done (todos-done-item-p)))
    ;; FIXME ? this moves to bob if on the first item (but so does previous-line)
    (todos-item-start)
    (unless (bobp)
      (re-search-backward todos-item-start nil t (or count 1)))
    ;; If points advances by one from a done to a todo item, go back to the
    ;; space above todos-done-separator, since that is a legitimate place to
    ;; insert an item.  But skip this space if count > 1, since that should
    ;; only stop on an item (FIXME: or not?)
    (when (and done (not (todos-done-item-p))
	       (or (not count) (= count 1)))
      (re-search-forward (concat "^" (regexp-quote todos-category-done)) nil t)
      (forward-line -1))))

(defun todos-search ()
  "Search for a regular expression in this Todos file.
The search runs through the whole file and encompasses all and
only todo and done items; it excludes category names.  Multiple
matches are shown sequentially, highlighted in `todos-search'
face."
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
	    (if in-done
		(unless todos-show-with-done (todos-toggle-view-done-items)))
	    (goto-char match)
	    (setq ov (make-overlay (- (point) (length regex)) (point)))
	    (overlay-put ov 'face 'todos-search)
	    (when matches
	      (setq mlen (length matches))
	      (if (y-or-n-p
		   (if (> mlen 1)
		       (format "There are %d more matches; go to next match? "
			       mlen)
		     "There is one more match; go to it? "))
		  (widen)
		(throw 'stop (setq msg (if (> mlen 1)
					   (format "There are %d more matches."
						   mlen)
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
  "Remove highlighting on matches found by todos-search."
  (interactive)
  (remove-overlays 1 (1+ (buffer-size)) 'face 'todos-search))

;;; Editing

(defun todos-add-file ()
  "Name and add a new Todos file.
Interactively, prompt for a category and display it.
Noninteractively, return the name of the new file."
  (interactive)
  (let ((default-file (if todos-default-todos-file
			  (file-name-sans-extension
			   (file-name-nondirectory todos-default-todos-file))))
	file prompt shortname)
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
    (setq shortname (file-name-sans-extension (file-name-nondirectory file)))
    (with-current-buffer (get-buffer-create file)
      (erase-buffer)
      (write-region (point-min) (point-max) file nil 'nomessage nil t)
      (kill-buffer file))
    ;; FIXME: todos-change-default-file yields a Custom mismatch
    ;; (if (or (not default-file)
    ;; 	    (yes-or-no-p (concat (format "Make \"%s\" new default Todos file "
    ;; 					 shortname)
    ;; 				 (format "[current default is \"%s\"]? "
    ;; 					 default-file))))
    ;; 	(todos-change-default-file file)
    ;;   (message "\"%s\" remains the default Todos file." default-file))
    (if (called-interactively-p)
	(progn
	  (setq todos-current-todos-file file)
	  (todos-show))
      file)))

;; FIXME: omit this and just use defcustom?  Says "changed outside of Custom
;; (mismatch)"
(defun todos-change-default-file (&optional file)
  ""
  (interactive)
  (let ((new-default (or file
			 (todos-read-file-name "Choose new default Todos file: "
					       nil t))))
    (customize-save-variable 'todos-default-todos-file new-default)
    (message "\"%s\" is new default Todos file."
	     (file-name-sans-extension (file-name-nondirectory new-default)))))

(defun todos-add-category (&optional cat)
  "Add a new category to the current Todos file.
Called interactively, prompt for category name, then visit the
category in Todos mode.  Non-interactively, argument CAT provides
the category name, which is also the return value."
  (interactive)
  (let* ((buffer-read-only)
	 ;; FIXME: check against todos-archive-done-item-or-items with empty file
	 (buf (find-file-noselect todos-current-todos-file t))
	 ;; (buf (get-file-buffer todos-current-todos-file))
	 (num (1+ (length todos-categories)))
	 (counts (make-vector 4 0)))	; [todo diary done archived]
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
      (save-excursion			; Save point for todos-category-select.
	(insert todos-category-beg cat "\n\n" todos-category-done "\n"))
      (todos-update-categories-sexp)
      ;; If called by command, display the newly added category, else return
      ;; the category number to the caller.
      (if (called-interactively-p 'any)	  ; FIXME?
	  (progn
	    (setq todos-category-number num)
	    (todos-category-select))
	num))))

(defun todos-rename-category ()
  "Rename current Todos category.
If this file has an archive containing this category, rename the
category there as well."
  (interactive)
  (let* ((cat (todos-current-category))
	 (new (read-from-minibuffer (format "Rename category \"%s\" to: " cat))))
    (setq new (todos-validate-category-name new))
    (let* ((ofile todos-current-todos-file)
	   (archive (concat (file-name-sans-extension ofile) ".toda"))
	   (buffers (append (list ofile)
			    (unless (zerop (todos-get-count 'archived cat))
			      (list archive)))))
      (dolist (buf buffers)
	(with-current-buffer (find-file-noselect buf)
	  (let (buffer-read-only)
	    (setq todos-categories (todos-set-categories))
	    (save-excursion
	      (save-restriction
		(setcar (assoc cat todos-categories) new)
		(widen)
		(goto-char (point-min))
		(todos-update-categories-sexp)
		(re-search-forward (concat (regexp-quote todos-category-beg)
					   "\\(" (regexp-quote cat) "\\)\n")
				   nil t)
		(replace-match new t t nil 1)))))))
    (setq mode-line-buffer-identification
	  (funcall todos-mode-line-function new)))
  (save-excursion (todos-category-select)))

(defun todos-delete-category (&optional arg)
  "Delete current Todos category provided it is empty.
With ARG non-nil delete the category unconditionally,
i.e. including all existing todo and done items."
  (interactive "P")
  (let* ((cat (todos-current-category))
	 (todo (todos-get-count 'todo cat))
	 (done (todos-get-count 'done cat))
	 (archived (todos-get-count 'archived cat)))
    (if (and (not arg)
	     (or (> todo 0) (> done 0)))
	(message "To delete a non-empty category, type C-u D.")
      (when (yes-or-no-p (concat "Permanently remove category \"" cat
				 "\"" (and arg " and all its entries") "? "))
	;; FIXME ? optionally delete archived category as well?
	(when (and archived
		   (y-or-n-p (concat "This category has archived items; "
				     "the archived category will remain\n"
				     "after deleting the todo category.  "
				     "Do you still want to delete it\n"
				     "(see 'todos-ignore-archived-categories' "
				     "for another option)? ")))
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
	    (message "Deleted category %s" cat)))))))

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
	     buffer-read-only newcats)
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
	(setq newcats todos-categories)
	(with-current-buffer (get-file-buffer todos-current-todos-file)
	  (setq todos-categories newcats)
	  (todos-update-categories-sexp))
	(forward-line (if lower -1 -2))
	(forward-char col)))))

(defun todos-lower-category ()
  "Lower priority of category point is on in Categories buffer."
  (interactive)
  (todos-raise-category t))

(defun todos-move-category ()
  "Move current category to a different Todos file.
If current category has archived items, also move those to the
archive of the file moved to, creating it if it does not exist."
  (interactive)
  (when (or (> (length todos-categories) 1)
	    (y-or-n-p (concat "This is the only category in this file; "
			      "moving it will also delete the file.\n"
			      "Do you want to proceed? ")))
    (let* ((ofile todos-current-todos-file)
	   (cat (todos-current-category))
	   (nfile (todos-read-file-name "Choose a Todos file: " nil t))
	   (archive (concat (file-name-sans-extension ofile) ".toda"))
	   (buffers (append (list ofile)
			    (unless (zerop (todos-get-count 'archived cat))
			      (list archive))))
	   new)
      (dolist (buf buffers)
	(with-current-buffer (find-file-noselect buf)
	  (widen)
	  (goto-char (point-max))
	  (let* ((beg (re-search-backward
		       (concat "^"
			       (regexp-quote (concat todos-category-beg cat)))
		       nil t))
		 (end (if (re-search-forward
			   (concat "^" (regexp-quote todos-category-beg))
			   nil t 2)
			  (match-beginning 0)
			(point-max)))
		 (content (buffer-substring-no-properties beg end))
		 (counts (cdr (assoc cat todos-categories)))
		 buffer-read-only)
	    ;; Move the category to the new file.  Also update or create
	    ;; archive file if necessary.
	    (with-current-buffer
		(find-file-noselect
		 ;; Regenerate todos-archives in case there
		 ;; is a newly created archive.
		 (if (member buf (funcall todos-files-function t))
		     (concat (file-name-sans-extension nfile) ".toda")
		   nfile))
	      (let* ((nfile-short (file-name-sans-extension
				   (file-name-nondirectory nfile)))
		     (prompt (concat
			      (format "Todos file \"%s\" already has "
				      nfile-short)
			      (format "the category \"%s\";\n" cat)
			      "enter a new category name: "))
		     buffer-read-only)
		(widen)
		(goto-char (point-max))
		(insert content)
		;; If the file moved to has a category with the same
		;; name, rename the moved category.
		(when (assoc cat todos-categories)
		  (unless (member (file-truename (buffer-file-name))
				  (funcall todos-files-function t))
		    (setq new (read-from-minibuffer prompt))
		    (setq new (todos-validate-category-name new))))
		;; Replace old with new name in Todos and archive files.
		(when new
		  (goto-char (point-max))
		  (re-search-backward
		   (concat "^" (regexp-quote todos-category-beg)
			   "\\(" (regexp-quote cat) "\\)") nil t)
		  (replace-match new nil nil nil 1)))
	      (setq todos-categories
		    (append todos-categories (list (cons new counts))))
	      (todos-update-categories-sexp)
	      ;; If archive was just created, save it to avoid "File <xyz> no
	      ;; longer exists!" message on invoking
	      ;; `todos-view-archived-items'.  FIXME: maybe better to save
	      ;; unconditionally?
	      (unless (file-exists-p (buffer-file-name))
		(save-buffer))
	      (todos-category-number (or new cat))
	      (todos-category-select))
	    ;; Delete the category from the old file, and if that was the
	    ;; last category, delete the file.  Also handle archive file
	    ;; if necessary.
	    (remove-overlays beg end)
	    (delete-region beg end)
	    (goto-char (point-min))
	    ;; Put point after todos-categories sexp.
	    (forward-line)
	    (if (eobp)		; Aside from sexp, file is empty.
		(progn
		  ;; Skip confirming killing the archive buffer.
		  (set-buffer-modified-p nil)
		  (delete-file todos-current-todos-file)
		  (kill-buffer))
	      (setq todos-categories (delete (assoc cat todos-categories)
					     todos-categories))
	      (todos-update-categories-sexp)
	      (todos-category-select)))))
      (set-window-buffer (selected-window)
			 (set-buffer (find-file-noselect nfile)))
      (todos-category-number (or new cat))
      (todos-category-select))))

(defun todos-merge-category ()
  "Merge this category with chosen category in this file. The
current category's todo and done items are appended to the chosen
category's todo and done items, respectively, which becomes the
current category, and the category moved from is deleted."
  (interactive)
  (let ((buffer-read-only nil)
	(cat (todos-current-category))
	(goal (todos-read-category "Category to merge to: " t)))
    (widen)
    ;; FIXME: check if cat has archived items and merge those too
    (let* ((cbeg (progn
		   (re-search-backward
		    (concat "^" (regexp-quote todos-category-beg)) nil t)
		   (point)))
	   (tbeg (progn (forward-line) (point)))
	   (dbeg (progn
		   (re-search-forward
		    (concat "^" (regexp-quote todos-category-done)) nil t)
		   (forward-line) (point)))
	   (tend (progn (forward-line -2) (point)))
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
       (concat "^" (regexp-quote (concat todos-category-beg goal))) nil t)
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
      (todos-set-count 'todo (todos-get-count 'todo cat) goal)
      (todos-set-count 'done (todos-get-count 'done cat) goal)
      (setq todos-categories (delete (assoc cat todos-categories)
				     todos-categories))
      (todos-update-categories-sexp)
      (todos-category-number goal)
      (todos-category-select)
      ;; Put point at the start of the merged todo items.
      ;; FIXME: what if there are no merged todo items but only done items?
      (goto-char here))))
      
;; FIXME
(defun todos-merge-categories ()
  ""
  (interactive)
  (let* ((cats (mapcar 'car todos-categories))
	 (goal (todos-read-category "Category to merge to: " t))
	 (prompt (format "Merge to %s (type C-g to finish)? " goal))
	 (source (let ((inhibit-quit t) l)
		  (while (not (eq last-input-event 7))
		    (dolist (c cats)
		      (when (y-or-n-p prompt)
			(push c l)
			(setq cats (delete c cats))))))))
    (widen)
  ))

;; FIXME: make insertion options customizable per category
;;;###autoload
;; (defun todos-insert-item (&optional arg use-point date-type time
;; 				    diary nonmarking)
(defun todos-insert-item (&optional arg diary nonmarking date-type time
				    region-or-here)
  "Add a new Todo item to a category.
See the note at the end of this document string about key
bindings and convenience commands derived from this command.

With no (or nil) prefix argument ARG, add the item to the current
category; with one prefix argument (C-u), prompt for a category
from the current Todos file; with two prefix arguments (C-u C-u),
first prompt for a Todos file, then a category in that file.  If
a non-existing category is entered, ask whether to add it to the
Todos file; if answered affirmatively, add the category and
insert the item there.

When argument DIARY is non-nil, this overrides the intent of the
user option `todos-include-in-diary' for this item: if
`todos-include-in-diary' is nil, include the item in the Fancy
Diary display, and if it is non-nil, exclude the item from the
Fancy Diary display.  When DIARY is nil, `todos-include-in-diary'
has its intended effect.

When the item is included in the Fancy Diary display and the
argument NONMARKING is non-nil, this overrides the intent of the
user option `todos-diary-nonmarking' for this item: if
`todos-diary-nonmarking' is nil, append `diary-nonmarking-symbol'
to the item, and if it is non-nil, omit `diary-nonmarking-symbol'.

The argument DATE-TYPE determines the content of the item's
mandatory date header string and how it is added:
- If DATE-TYPE is the symbol `calendar', the Calendar pops up and
  when the user puts the cursor on a date and hits RET, that
  date, in the format set by `calendar-date-display-form',
  becomes the date in the header.
- If DATE-TYPE is the symbol `date', the header contains the date
  in the format set by `calendar-date-display-form', with year,
  month and day individually prompted for (month with tab
  completion).
- If DATE-TYPE is the symbol `dayname' the header contains a
  weekday name instead of a date, prompted for with tab
  completion.
- If DATE-TYPE has any other value (including nil or none) the
  header contains the current date (in the format set by
  `calendar-date-display-form').

With non-nil argument TIME prompt for a time string; this must
either be empty or else match `diary-time-regexp'.  If TIME is
nil, add or omit the current time according to value of the user
option `todos-always-add-time-string'.

The argument REGION-OR-HERE determines the source and location of
the new item:
- If the REGION-OR-HERE is the symbol `here', prompt for the text
  of the new item and insert it directly above the todo item at
  point, or if point is on the empty line below the last todo
  item, insert the new item there.  An error is signalled if
  `todos-insert-item' is invoked with `here' outside of the
  current category.
- If REGION-OR-HERE is the symbol `region', use the region of the
  current buffer as the text of the new item, depending on the
  value of user option `todos-use-only-highlighted-region': if
  this is non-nil, then use the region only when it is
  highlighted; otherwise, use the region regardless of
  highlighting.  An error is signalled if there is no region in
  the current buffer.  Prompt for the item's priority in the
  category (an integer between 1 and one more than the number of
  items in the category), and insert the item accordingly.
- If REGION-OR-HERE has any other value (in particular, nil or
  none), prompt for the text and the item's priority, and insert
  the item accordingly.

To facilitate using these arguments when inserting a new todo
item, convenience commands have been defined for all admissible
combinations (96 in all!) together with mnenomic key bindings
based on on the name of the arguments and their order: _h_ere or
_r_egion - _c_alendar or _d_ate or day_n_ame - _t_ime - diar_y_ -
nonmar_k_ing.  An alternative interface for customizing key
binding is also provided with the function
`todos-insertion-bindings'."		;FIXME
  (interactive "P")
  (let ((region (eq region-or-here 'region))
	(here (eq region-or-here 'here)))
    (when region
      ;; FIXME: better to use use-region-p or region-active-p?
      (unless (and (if todos-use-only-highlighted-region
		       transient-mark-mode
		     t)
		   mark-active)
	(error "The mark is not set now, so there is no region")))
    (let* ((buf (current-buffer))
	   (new-item (if region
			 ;; FIXME: or keep properties?
			 (buffer-substring-no-properties
			  (region-beginning) (region-end))
		       (read-from-minibuffer "Todo item: ")))
	   (date-string (cond
			 ((eq date-type 'date)
			  (todos-read-date))
			 ((eq date-type 'dayname)
			  (todos-read-dayname))
			 ((eq date-type 'calendar)
			  (setq todos-date-from-calendar t)
			  (let (calendar-view-diary-initially-flag)
			    (calendar))
			  (with-current-buffer "*Calendar*"
			    (todos-set-date-from-calendar))
			  todos-date-from-calendar)
			 (t (calendar-date-string (calendar-current-date) t t))))
	   ;; FIXME: should TIME override `todos-always-add-time-string'?  But
	   ;; then add another option to use current time or prompt for time
	   ;; string?
	   (time-string (or (and time (todos-read-time))
			    (and todos-always-add-time-string
				 (substring (current-time-string) 11 16)))))
      (setq todos-date-from-calendar nil)
      (cond ((equal arg '(16))		; FIXME: cf. set-mark-command
	     (todos-jump-to-category nil t)
	     (set-window-buffer
	      (selected-window)
	      (set-buffer (get-file-buffer todos-global-current-todos-file))))
	    ((equal arg '(4))		; FIXME: just arg?
	     (todos-jump-to-category)
	     (set-window-buffer
	      (selected-window)
	      (set-buffer (get-file-buffer todos-global-current-todos-file))))
	    (t
	     (when (not (derived-mode-p 'todos-mode)) (todos-show))))
      (let (buffer-read-only)
	(setq new-item
	      ;; Add date, time and diary marking as required.
	      (concat (if (not (and diary (not todos-include-in-diary)))
			  todos-nondiary-start
			(when (and nonmarking (not todos-diary-nonmarking))
			  diary-nonmarking-symbol))
		      date-string (when time-string
				    (concat " " time-string))
		      (when (not (and diary (not todos-include-in-diary)))
			todos-nondiary-end)
		      " " new-item))
	;; Indent newlines inserted by C-q C-j if nonspace char follows.
	(setq new-item (replace-regexp-in-string
			"\\(\n\\)[^[:blank:]]"
			(concat "\n" (make-string todos-indent-to-here 32))
			new-item nil nil 1))
	(if here
	    (cond ((not (eq major-mode 'todos-mode))
		   (error "Cannot insert a todo item here outside of Todos mode"))
		  ((not (eq buf (current-buffer)))
		   (error "Cannot insert an item here after changing buffer"))
		  ((or (todos-done-item-p)
		       ;; Point on last blank line.
		       (save-excursion (forward-line -1) (todos-done-item-p)))
		   (error "Cannot insert a new item in the done item section"))
		  (t
		   (todos-insert-with-overlays new-item)))
	  (todos-set-item-priority new-item (todos-current-category) t))
	(todos-set-count 'todo 1)
	(if (or diary todos-include-in-diary) (todos-set-count 'diary 1))
	(todos-update-categories-sexp)))))

;; FIXME: autoload when key-binding is defined in calendar.el
(defun todos-insert-item-from-calendar ()
  ""
  (interactive)
  ;; FIXME: todos-current-todos-file is nil here, better to solicit Todos file?
  ;; FIXME: t-g-c-t-f is nil if no Todos file has been visited 
  (pop-to-buffer (file-name-nondirectory todos-global-current-todos-file))
  (todos-show)
  ;; FIXME: this now calls todos-set-date-from-calendar
  (todos-insert-item t 'calendar))

;; FIXME: calendar is loaded before todos
;; (add-hook 'calendar-load-hook
	  ;; (lambda ()
	    (define-key calendar-mode-map "it" 'todos-insert-item-from-calendar);))

(defvar todos-date-from-calendar nil)
(defun todos-set-date-from-calendar ()
  ""
  (when todos-date-from-calendar
    (local-set-key (kbd "RET") 'exit-recursive-edit)
    (message "Put cursor on a date and type <return> to set it.")
    ;; FIXME: is there a better way than recursive-edit?
    ;; FIXME: use unwind-protect?  Check recursive-depth?
    (recursive-edit)
    (setq todos-date-from-calendar
	  (calendar-date-string (calendar-cursor-to-date t) t t))
    (calendar-exit)))

(defun todos-delete-item ()
  "Delete at least one item in this category.

If there are marked items, delete all of these; otherwise, delete
the item at point."
  (interactive)
  (let* ((cat (todos-current-category))
	 (marked (assoc cat todos-categories-with-marks))
	 (item (unless marked (todos-item-string)))
	 (ov (make-overlay (save-excursion (todos-item-start))
			   (save-excursion (todos-item-end))))
	 ;; FIXME: make confirmation an option
	 (answer (if marked
		     (y-or-n-p "Permanently delete all marked items? ")
		   (when item
		     (overlay-put ov 'face 'todos-search)
		     (y-or-n-p (concat "Permanently delete this item? ")))))
	 (opoint (point))
	 buffer-read-only)
    (when answer
      (and marked (goto-char (point-min)))
      (catch 'done
	(while (not (eobp))
	  (if (or (and marked (todos-item-marked-p)) item)
	      (progn
		(if (todos-done-item-p)
		    (todos-set-count 'done -1)
		  (todos-set-count 'todo -1 cat)
		  (and (todos-diary-item-p) (todos-set-count 'diary -1)))
		(delete-overlay ov)
		(todos-remove-item)
		;; Don't leave point below last item.
		(and item (bolp) (eolp) (< (point-min) (point-max))
		     (todos-backward-item))
		(when item 
		  (throw 'done (setq item nil))))
	    (todos-forward-item))))
      (when marked
	(remove-overlays (point-min) (point-max) 'before-string todos-item-mark)
	(setq todos-categories-with-marks
	      (assq-delete-all cat todos-categories-with-marks))
	(goto-char opoint))
      (todos-update-categories-sexp)
      (todos-prefix-overlays))
    (if ov (delete-overlay ov))))

(defun todos-edit-item ()
  "Edit current Todo item in the minibuffer."
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
	   (multiline (> (length (split-string item "\n")) 1))
	   (opoint (point)))
      (if multiline
	  (todos-edit-multiline)
	(let ((new (read-string "Edit: " (cons item item-beg))))
	  (while (not (string-match
		       (concat todos-date-string-start todos-date-pattern) new))
	    (setq new (read-from-minibuffer
		       "Item must start with a date: " new)))
	  ;; Indent newlines inserted by C-q C-j if nonspace char follows.
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
  "Edit current Todo item in Todos Edit mode.
Use of newlines invokes `todos-indent' to insure compliance with
the format of Diary entries."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name todos-edit-buffer)))
    (set-window-buffer
     (selected-window)
     (set-buffer (make-indirect-buffer
		  (file-name-nondirectory todos-current-todos-file)
		  buffer-name)))
    (narrow-to-region (todos-item-start) (todos-item-end))
    (todos-edit-mode)
    (message "Type %s to return to Todos mode."
	     (key-description (car (where-is-internal 'todos-edit-quit))))))

(defun todos-edit-quit ()
  "Return from Todos Edit mode to Todos mode."
  (interactive)
  (kill-buffer)
  (todos-show))

(defun todos-edit-item-header (&optional what)
  "Edit date/time header of at least one item.

Interactively, ask whether to edit year, month and day or day of
the week, as well as time.  If there are marked items, apply the
changes to all of these; otherwise, edit just the item at point.

Non-interactively, argument WHAT specifies whether to edit only
the date or only the time, or to set the date to today."
  (interactive)
  (let* ((cat (todos-current-category))
	 (marked (assoc cat todos-categories-with-marks))
	 (first t)
	 ndate ntime nheader)
    (save-excursion
      (or (and marked (goto-char (point-min))) (todos-item-start))
      (catch 'stop
	(while (not (eobp))
	  (and marked
	       (while (not (todos-item-marked-p))
		 (todos-forward-item)
		 (and (eobp) (throw 'stop nil))))
	  (re-search-forward (concat todos-date-string-start "\\(?1:"
				     todos-date-pattern
				     "\\)\\(?2: " diary-time-regexp "\\)?")
			     (line-end-position) t)
	  (let* ((odate (match-string-no-properties 1))
		 (otime (match-string-no-properties 2))
		 (buffer-read-only))
	    (if (eq what 'today)
		(progn
		  (setq ndate (calendar-date-string (calendar-current-date) t t))
		  (replace-match ndate nil nil nil 1))
	      (unless (eq what 'timeonly)
		(when first
		  (setq ndate (if (save-match-data (string-match "[0-9]+" odate))
				  (if (y-or-n-p "Change date? ")
				      (todos-read-date)
				    (todos-read-dayname))
				(if (y-or-n-p "Change day? ")
				    (todos-read-dayname)
				  (todos-read-date)))))
		(replace-match ndate nil nil nil 1))
	      (unless (eq what 'dateonly)
		(when first
		  (setq ntime (save-match-data (todos-read-time)))
		  (when (< 0 (length ntime)) (setq ntime (concat " " ntime))))
		(if otime
		    (replace-match ntime nil nil nil 2)
		  (goto-char (match-end 1))
		  (insert ntime))))
	    (setq first nil))
	  (if marked
	      (todos-forward-item)
	    (goto-char (point-max))))))))

(defun todos-edit-item-date ()
  "Prompt For and apply changes to current item's date."
  (interactive)
  (todos-edit-item-header 'dateonly))

(defun todos-edit-item-date-is-today ()
  "Set item date to today's date."
  (interactive)
  (todos-edit-item-header 'today))
 
(defun todos-edit-item-time ()
  "Prompt For and apply changes to current item's time."
  (interactive)
  (todos-edit-item-header 'timeonly))

(defun todos-raise-item-priority (&optional lower)
  "Raise priority of current item by moving it up by one item.
With non-nil argument LOWER lower item's priority."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; We're between todo and done items.
    (let (buffer-read-only)
      (if (or (and lower
		   (save-excursion
		     ;; Can't lower final todo item.
		     (todos-forward-item)
		     (and (looking-at todos-item-start)
			  (not (todos-done-item-p)))))
	      ;; Can't raise or lower only todo item.
	      (> (count-lines (point-min) (point)) 0))
	  (let ((item (todos-item-string))
		(marked (todos-item-marked-p)))
	    ;; In Todos Top Priorities mode, an item's priority can be changed
	    ;; wrt items in another category, but not wrt items in the same
	    ;; category.
	    (when (eq major-mode 'todos-filter-items-mode)
	      (let* ((regexp (concat todos-date-string-start todos-date-pattern
				     "\\( " diary-time-regexp "\\)?"
				     (regexp-quote todos-nondiary-end)
				     "?\\(?1: \\[\\(.+:\\)?.+\\]\\)"))
		     (cat1 (save-excursion
			    (re-search-forward regexp nil t)
			    (match-string 1)))
		     (cat2 (save-excursion
			     (if lower
				 (todos-forward-item)
			       (todos-backward-item))
			     (re-search-forward regexp nil t)
			     (match-string 1))))
		(if (string= cat1 cat2)
		    ;; FIXME: better message
		    (error (concat "Cannot change item's priority in its "
				   "category; do this in Todos mode")))))
	    (todos-remove-item)
	    (if lower (todos-forward-item) (todos-backward-item))
	    (todos-insert-with-overlays item)
	    ;; If item was marked, retore the mark.
	    (and marked (overlay-put (make-overlay (point) (point))
				     'before-string todos-item-mark)))
	(message ""))))) ;FIXME: no message ?

(defun todos-lower-item-priority ()
  "Lower priority of current item by moving it down by one item."
  (interactive)
  (todos-raise-item-priority t))

;; FIXME: incorporate todos-(raise|lower)-item-priority ?
(defun todos-set-item-priority (item cat &optional new)
  "Set todo ITEM's priority in category CAT, moving item as needed.
Interactively, the item and the category are the current ones,
and the priority is a number between 1 and the number of items in
the category.  Non-interactively with argument NEW, the lowest
priority is one more than the number of items in CAT."
  (interactive (list (todos-item-string) (todos-current-category)))
  (unless (called-interactively-p t)
    (todos-category-number cat)
    (todos-category-select))
  (let* ((todo (todos-get-count 'todo cat))
	 (maxnum (if new (1+ todo) todo))
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
		(format "Priority must be an integer between 1 and %d.\n"
			maxnum)))
	(unless prompt (setq priority candidate)))
      ;; Interactively, just relocate the item within its category.
      (when (called-interactively-p) (todos-remove-item))
      (goto-char (point-min))
      (unless (= priority 1) (todos-forward-item (1- priority))))
    (todos-insert-with-overlays item)))

;; FIXME: apply to marked items?
(defun todos-move-item (&optional file)
  "Move at least one todo item to another category.

If there are marked items, move all of these; otherwise, move
the item at point.

With non-nil argument FILE, first prompt for another Todos file and
then a category in that file to move the item or items to.

If the chosen category is not one of the existing categories,
then it is created and the item(s) become(s) the first
entry/entries in that category."
  (interactive)
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))	; We're between todo and done items.
    (let* ((buffer-read-only)
	   (file1 todos-current-todos-file)
	   (cat1 (todos-current-category))
	   (marked (assoc cat1 todos-categories-with-marks))
	   (num todos-category-number)
	   (item (todos-item-string))
	   (diary-item (todos-diary-item-p))
	   (omark (save-excursion (todos-item-start) (point-marker)))
	   (file2 (if file
		      (todos-read-file-name "Choose a Todos file: " nil t)
		    file1))
	   (count 0)
	   (count-diary 0)
	   cat2 nmark)
      (set-buffer (find-file-noselect file2))
      (setq cat2 (let* ((pl (if (and marked (> (cdr marked) 1)) "s" ""))
			(name (todos-read-category
			       (concat "Move item" pl " to category: ")))
			(prompt (concat "Choose a different category than "
					"the current one\n(type `"
					(key-description
					 (car (where-is-internal
					       'todos-set-item-priority)))
					"' to reprioritize item "
					"within the same category): ")))
		   (while (equal name cat1)
		     (setq name (todos-read-category prompt)))
		   name))
      (set-buffer (get-file-buffer file1))
      (if marked
	  (progn
	   (setq item nil)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (when (todos-item-marked-p)
	       (setq item (concat item (todos-item-string) "\n"))
	       (setq count (1+ count))
	       (when (todos-diary-item-p)
		 (setq count-diary (1+ count-diary))))
	     (todos-forward-item))
	   ;; Chop off last newline.
	   (setq item (substring item 0 -1)))
	(setq count 1)
	(when (todos-diary-item-p) (setq count-diary 1)))
      (set-window-buffer (selected-window)
			 (set-buffer (find-file-noselect file2)))
      (unless (assoc cat2 todos-categories) (todos-add-category cat2))
      (todos-set-item-priority item cat2 t)
      (setq nmark (point-marker))
      (todos-set-count 'todo count)
      (todos-set-count 'diary count-diary)
      (todos-update-categories-sexp)
      (with-current-buffer (get-file-buffer file1)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char omark)
	    (if marked
		(let (beg end)
		  (setq item nil)
		  (re-search-backward
		   (concat "^" (regexp-quote todos-category-beg)) nil t)
		  (forward-line)
		  (setq beg (point))
		  (re-search-forward
		   (concat "^" (regexp-quote todos-category-done)) nil t)
		  (setq end (match-beginning 0))
		  (goto-char beg)
		  (while (< (point) end)
		    (if (todos-item-marked-p)
			(todos-remove-item)
		      (todos-forward-item))))
	      (todos-remove-item))))
	(todos-set-count 'todo (- count) cat1)
	(todos-set-count 'diary (- count-diary) cat1)
	(todos-update-categories-sexp))
      (set-window-buffer (selected-window)
			 (set-buffer (find-file-noselect file2)))
      (setq todos-category-number (todos-category-number cat2))
      (todos-category-select)
      (goto-char nmark))))

(defun todos-move-item-to-file ()
  "Move the current todo item to a category in another Todos file."
  (interactive)
  (todos-move-item t))

;; FIXME: apply to marked items?
(defun todos-item-done (&optional arg)
  "Tag this item as done and move it to category's done section.
With prefix argument ARG prompt for a comment and append it to the
done item."
  (interactive "P")
  (unless (or (todos-done-item-p)
	      (looking-at "^$"))
    (let* ((buffer-read-only)
	   (item (todos-item-string))
	   (diary-item (todos-diary-item-p))
	   (date-string (calendar-date-string (calendar-current-date) t t))
	   (time-string (if todos-always-add-time-string ;FIXME: delete condition
			    (concat " " (substring (current-time-string) 11 16))
			  ""))
	   ;; FIXME: todos-nondiary-* ?
	   (done-item (concat "[" todos-done-string date-string time-string "] "
			      item))
	   (comment (and arg (read-string "Enter a comment: "))))
      (todos-remove-item)
      (unless (zerop (length comment))
	(setq done-item (concat done-item " [" todos-comment-string ": "
				comment "]")))
      (save-excursion
	(widen)
	(re-search-forward (concat "^" (regexp-quote todos-category-done)) nil t)
	(forward-char)
	(todos-insert-with-overlays done-item))
      (todos-set-count 'todo -1)
      (todos-set-count 'done 1)
      (and diary-item (todos-set-count 'diary -1))
      (todos-update-categories-sexp)
      (save-excursion (todos-category-select)))))

(defun todos-comment-done-item ()
  "Add a comment to this done item."
  (interactive)
  (when (todos-done-item-p)
    (let ((comment (read-string "Enter a comment: "))
	  buffer-read-only)
      (todos-item-end)
      (insert " [" todos-comment-string ": " comment "]"))))

;; FIXME: implement this or done item editing?
(defun todos-uncomment-done-item ()
  ""
  )

;; FIXME: delete comment from restored item or just leave it up to user?
(defun todos-item-undo ()
  "Restore this done item to the todo section of this category."
  (interactive)
  (when (todos-done-item-p)
    (let* ((buffer-read-only)
	   (done-item (todos-item-string))
	   (opoint (point))
	   (orig-mrk (progn (todos-item-start) (point-marker)))
	   ;; Find the end of the date string added upon making item done.
	   (start (search-forward "] "))
	   (item (buffer-substring start (todos-item-end)))
	   undone)
      (todos-remove-item)
      ;; If user cancels before setting new priority, then restore everything.
      (unwind-protect
	  (progn
	    (todos-set-item-priority item (todos-current-category) t)
	    (setq undone t)
	    (todos-set-count 'todo 1)
	    (todos-set-count 'done -1)
	    (and (todos-diary-item-p) (todos-set-count 'diary 1))
	    (todos-update-categories-sexp))
	(unless undone
	  (widen)
	  (goto-char orig-mrk)
	  (todos-insert-with-overlays done-item)
	  (let ((todos-show-with-done t))
	    (todos-category-select)
	    (goto-char opoint)))
	(set-marker orig-mrk nil)))))

(defun todos-archive-done-item-or-items (&optional all)
  "Archive at least one done item in this category.

If there are marked done items (and no marked todo items),
archive all of these; otherwise, with non-nil argument ALL,
archive all done items in this category; otherwise, archive the
done item at point.

If the archive of this file does not exist, it is created.  If
this category does not exist in the archive, it is created."
  (interactive)
  (when (not (member (buffer-file-name) (funcall todos-files-function t)))
    (if (and all (zerop (todos-get-count 'done cat)))
	(message "No done items in this category")
      (catch 'end
	(let* ((cat (todos-current-category))
	       (tbuf (current-buffer))
	       (marked (assoc cat todos-categories-with-marks))
	       (afile (concat (file-name-sans-extension
			       todos-current-todos-file) ".toda"))
	       (archive (if (file-exists-p afile)
			    (find-file-noselect afile t)
			  (progn
			    ;; todos-add-category requires an exisiting file...
			    (with-current-buffer (get-buffer-create afile)
			      (erase-buffer)
			      (write-region (point-min) (point-max) afile
					    nil 'nomessage nil t)))
			  ;; ...but the file still lacks a categories sexp, so
			  ;; visiting the file would barf on todos-set-categories,
			  ;; hence we just return the buffer.
			  (get-buffer afile)))
	       (item (and (todos-done-item-p) (concat (todos-item-string) "\n")))
	       (count 0)
	       marked-items beg end all-done
	       buffer-read-only)
	  (cond
	   (marked
	    (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
		(if (todos-item-marked-p)
		    (if (not (todos-done-item-p))
			(throw 'end (message "Only done items can be archived"))
		      (concat marked-items (todos-item-string) "\n")
		      (setq count (1+ count)))
		  (todos-forward-item)))))
	   (all
	    (if (y-or-n-p "Archive all done items in this category? ")
		(save-excursion
		  (save-restriction
		    (goto-char (point-min))
		    (widen)
		    (setq beg (progn
				(re-search-forward todos-done-string-start nil t)
				(match-beginning 0))
			  end (if (re-search-forward
				   (concat "^" (regexp-quote todos-category-beg))
				   nil t)
				  (match-beginning 0)
				(point-max))
			  all-done (buffer-substring beg end)
			  count (todos-get-count 'done))))
	      (throw 'end nil))))
	  (when (or marked all item)
	    (with-current-buffer archive
	      (let ((current todos-global-current-todos-file)
		    (buffer-read-only))
		(widen)
		(goto-char (point-min))
		(if (progn
		      (re-search-forward
		       (concat "^" (regexp-quote (concat todos-category-beg cat)))
		       nil t)
		      (re-search-forward (regexp-quote todos-category-done) nil t))
		    (forward-char)
		  ;; todos-add-category uses t-c-t-f, so temporarily set it.
		  (setq todos-current-todos-file afile)
		  (todos-add-category cat)
		  (goto-char (point-max)))
		(insert (cond (marked marked-items)
			      (all all-done)
			      (item)))
		(todos-set-count 'done (if (or marked all) count 1))
		(todos-update-categories-sexp)
		;; Save to file now (using write-region in order not to visit
		;; afile) so we can visit it later with todos-view-archived-items
		;; or todos-show-archive.
		(write-region nil nil afile)
		(setq todos-current-todos-file current)))
	    (with-current-buffer tbuf
	      (cond ((or marked item)
		     (and marked (goto-char (point-min)))
		     (catch 'done
		       (while (not (eobp))
			 (if (or (and marked (todos-item-marked-p)) item)
			     (progn
			       (todos-remove-item)
			       (todos-set-count 'done -1)
			       (todos-set-count 'archived 1)
			       ;; Don't leave point below last item.
			       (and item (bolp) (eolp) (< (point-min) (point-max))
				    (todos-backward-item))
			       (when item 
				 (throw 'done (setq item nil))))
			   (todos-forward-item)))))
		    (all
		     (remove-overlays beg end)
		     (delete-region beg end)
		     (todos-set-count 'done (- count))
		     (todos-set-count 'archived count)))
	      (when marked
		(remove-overlays (point-min) (point-max)
				 'before-string todos-item-mark)
		(setq todos-categories-with-marks
		      (assq-delete-all cat todos-categories-with-marks))
		(goto-char opoint))
	      (todos-update-categories-sexp)
	      (todos-prefix-overlays)
	      ;; FIXME: Heisenbug: item displays mark -- but not when edebugging
	      (remove-overlays (point-min) (point-max)
			       'before-string todos-item-mark)))
	  (display-buffer (find-file-noselect afile) t)
	  ;; FIXME: how to avoid switch-to-buffer and still get tbuf above
	  ;; afile?  What about pop-to-buffer-same-window in recent trunk?
	  (switch-to-buffer tbuf))))))

(defun todos-archive-category-done-items ()
  "Move all done items in this category to its archive."
  (interactive)
  (todos-archive-done-item-or-items t))

(defun todos-unarchive-items (&optional all)
  "Unarchive at least one item in this archive category.

If there are marked items, unarchive all of these; otherwise,
with non-nil argument ALL, unarchive all items in this category;
otherwise, unarchive the item at point.

Unarchived items are restored as done items to the corresponding
category in the Todos file, inserted at the end of done section.
If all items in the archive category were restored, the category
is deleted from the archive.  If this was the only category in the
archive, the archive file is deleted."
  (interactive)
  (when (member (buffer-file-name) (funcall todos-files-function t))
    (catch 'end
      (let* ((buffer-read-only nil)
	     (tbuf (find-file-noselect
		    (concat (file-name-sans-extension todos-current-todos-file)
			    ".todo") t))
	     (cat (todos-current-category))
	     (marked (assoc cat todos-categories-with-marks))
	     (item (concat (todos-item-string) "\n"))
	     (all-items (buffer-substring (point-min) (point-max)))
	     (all-count (todos-get-count 'done))
	     marked-items marked-count)
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when (todos-item-marked-p)
	      (concat marked-items (todos-item-string) "\n")
	      (setq marked-count (1+ marked-count)))
	    (todos-forward-item)))
	;; Restore items to end of category's done section and update counts.
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
	    (cond (marked
		   (insert marked-items)
		   (todos-set-count 'done marked-count)
		   (todos-set-count 'archived (- marked-count)))
		  (all
		   (if (y-or-n-p (concat "Restore this category's items "
					 "to Todos file as done items "
					 "and delete this category? "))
		       (progn (insert all-items)
			      (todos-set-count 'done all-count)
			      (todos-set-count 'archived (- all-count)))
		     (throw 'end nil)))
		  (t
		   (insert item)
		   (todos-set-count 'done 1)
		   (todos-set-count 'archived -1)))
	    (todos-update-categories-sexp)))
	;; Delete restored items from archive.
	(cond ((or marked item)
	       (and marked (goto-char (point-min)))
	       (catch 'done
		 (while (not (eobp))
		   (if (or (and marked (todos-item-marked-p)) item)
		       (progn
			 (todos-remove-item)
			 (todos-set-count 'done -1)
			 ;; Don't leave point below last item.
			 (and item (bolp) (eolp) (< (point-min) (point-max))
			      (todos-backward-item))
			 (when item 
			   (throw 'done (setq item nil))))
		     (todos-forward-item)))))
	      (all
	       (remove-overlays (point-min) (point-max))
	       (delete-region (point-min) (point-max))
	       (todos-set-count 'done (- all-count))))
	;; If that was the last category in the archive, delete the whole file.
	(if (= (length todos-categories) 1)
	    (progn
	      (delete-file todos-current-todos-file)
	      ;; Don't bother confirming killing the archive buffer.
	      (set-buffer-modified-p nil)
	      (kill-buffer))
	  ;; Otherwise, if the archive category is now empty, delete it.
	  (when (eq (point-min) (point-max))
	    (widen)
	    (let ((beg (re-search-backward
			(concat "^" (regexp-quote todos-category-beg) cat)
			nil t))
		  (end (if (re-search-forward
			    (concat "^" (regexp-quote todos-category-beg))
			    nil t 2)
			   (match-beginning 0)
			 (point-max))))
	      (remove-overlays beg end)
	      (delete-region beg end)
	      (setq todos-categories (delete (assoc cat todos-categories)
					     todos-categories))
	      (todos-update-categories-sexp))))
	;; Visit category in Todos file and show restored done items.
	(let ((tfile (buffer-file-name tbuf))
	      (todos-show-with-done t))
	  (set-window-buffer (selected-window)
			     (set-buffer (find-file-noselect tfile)))
	  (todos-category-number cat)
	  (todos-show)
	  (message "Items unarchived."))))))

(defun todos-unarchive-category ()
  "Unarchive all items in this category.  See `todos-unarchive-items'."
  (interactive)
  (todos-unarchive-items t))

(defun todos-toggle-diary-inclusion (&optional all)
  "Toggle diary status of one or more todo items in this category.

If a candidate item is marked with `todos-nondiary-marker',
remove this marker; otherwise, insert it.

With non-nil argument ALL toggle the diary status of all todo
items in this category; otherwise, if there are marked todo
items, toggle the diary status of all and only these, otherwise
toggle the diary status of the item at point.  "
  (interactive)
  (let ((marked (assoc (todos-current-category)
		       todos-categories-with-marks)))
    (catch 'stop
      (save-excursion
	(save-restriction
	  (when (or marked all) (goto-char (point-min)))
	  (while (not (eobp))
	    (if (todos-done-item-p)
		(throw 'stop (message "Done items cannot be changed"))
	      (unless (and marked (not (todos-item-marked-p)))
		(save-excursion
		  (let* ((buffer-read-only)
			 (beg (todos-item-start))
			 (lim (save-excursion (todos-item-end)))
			 (end (save-excursion
			       (or (todos-time-string-matcher lim)
				   (todos-date-string-matcher lim)))))
		    (if (looking-at (regexp-quote todos-nondiary-start))
			(progn
			  (replace-match "")
			  (search-forward todos-nondiary-end (1+ end) t)
			  (replace-match "")
			  (todos-set-count 'diary 1))
		      (when end
			(insert todos-nondiary-start)
			(goto-char (1+ end))
			(insert todos-nondiary-end)
			(todos-set-count 'diary -1))))))
	      (unless (or marked all) (throw 'stop nil))
	      (todos-forward-item))))))
    (todos-update-categories-sexp)))

(defun todos-toggle-item-diary-nonmarking ()
  "Mark or unmark this todos diary item for calendar display.
See `diary-nonmarking-symbol'."
  (interactive)
  (let ((buffer-read-only))
    (save-excursion
      (todos-item-start)
      (unless (looking-at (regexp-quote todos-nondiary-start))
	(if (looking-at (regexp-quote diary-nonmarking-symbol))
	    (replace-match "")
	  (insert diary-nonmarking-symbol))))))

(defun todos-toggle-diary-nonmarking ()
  "Mark or unmark this category's todos diary items for calendar.
See `diary-nonmarking-symbol'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (todos-toggle-item-diary-nonmarking)
      (todos-forward-item))))

(defun todos-print (&optional to-file)
  "Produce a printable version of the current Todos buffer.
This includes overlays, indentation, and, depending on the value
of `todos-print-function', faces.  With non-nil argument TO-FILE
write the printable version to a file; otherwise, send it to the
default printer."
  (interactive)
  (let ((buf todos-tmp-buffer-name)	;FIXME
	(header (cond
		 ((eq major-mode 'todos-mode)
		  (concat "Todos File: "
			  (file-name-sans-extension
			   (file-name-nondirectory todos-current-todos-file))
			  "\nCategory: " (todos-current-category)))
		 ((eq major-mode 'todos-filter-items-mode)
		  "Todos Top Priorities")))
	(prefix (propertize (concat todos-prefix " ")
			    'face 'todos-prefix-string))
	(num 0)
	(fill-prefix (make-string todos-indent-to-here 32))
	(content (buffer-string))
	file)
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
	;; Calling todos-forward-item infloops at todos-item-start due to
	;; non-overlay prefix, so search for item start instead.
	(if (re-search-forward todos-item-start nil t)
	    (beginning-of-line)
	  (goto-char (point-max))))
      (if (re-search-backward (concat "^" (regexp-quote todos-category-done))
			      nil t)
	  (replace-match todos-done-separator))
      (goto-char (point-min))
      (insert header)
      (newline 2)
      (if to-file
	  (let ((file (read-file-name "Print to file: ")))
	    (funcall todos-print-function file))
	(funcall todos-print-function)))
    (kill-buffer buf)))

(defun todos-print-to-file ()
  "Save printable version of this Todos buffer to a file."
  (interactive)
  (todos-print t))

;; ---------------------------------------------------------------------------

;;; Internals

(defvar todos-date-pattern		;FIXME: start with "^" ?
  (let ((dayname (diary-name-pattern calendar-day-name-array nil t)))
    (concat "\\(?:" dayname "\\|"
	    (let ((dayname)
		  (monthname (format "\\(?:%s\\|\\*\\)"
				     (diary-name-pattern
				      calendar-month-name-array
				      calendar-month-abbrev-array t)))
		  (month "\\(?:[0-9]+\\|\\*\\)")
		  (day "\\(?:[0-9]+\\|\\*\\)")
		  (year "-?\\(?:[0-9]+\\|\\*\\)"))
	      (mapconcat 'eval calendar-date-display-form ""))
	    "\\)"))
  "Regular expression matching a Todos date header.")

(defvar todos-date-string-start
  ;; FIXME: with ? matches anything
  (concat "^\\(" (regexp-quote todos-nondiary-start) "\\|"
	  (regexp-quote diary-nonmarking-symbol) "\\)?")
  "Regular expression matching part of item header before the date.")

(defvar todos-done-string-start
  (concat "^\\[" (regexp-quote todos-done-string))
  "Regular expression matching start of done item.")

(defun todos-date-string-matcher (lim)
  "Search for Todos date strings within LIM for font-locking."
  (re-search-forward
   (concat todos-date-string-start "\\(?1:" todos-date-pattern "\\)") lim t))

(defun todos-time-string-matcher (lim)
  "Search for Todos time strings within LIM for font-locking."
  (re-search-forward (concat todos-date-string-start todos-date-pattern
			     " \\(?1:" diary-time-regexp "\\)") lim t))

(defun todos-done-string-matcher (lim)
  "Search for Todos done headers within LIM for font-locking."
  (re-search-forward (concat todos-done-string-start
		      "[^][]+]")
		     lim t))

(defun todos-comment-string-matcher (lim)
  "Search for Todos done comment within LIM for font-locking."
  (re-search-forward (concat "\\[\\(?1:" todos-comment-string "\\):")
		     lim t))

(defun todos-category-string-matcher (lim)
  "Search for Todos category headers within LIM for font-locking."
  (if (eq major-mode 'todos-filter-items-mode)
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

(defun todos-after-find-file ()
  "Show Todos files correctly when visited from outside of Todos mode."
  (and (member this-command todos-visit-files-commands)
       (= (- (point-max) (point-min)) (buffer-size))
       (member major-mode '(todos-mode todos-archive-mode))
       (todos-category-select)))

(defun todos-wrap-and-indent ()
  "Use word wrapping on long lines and indent with a wrap prefix.
The amount of indentation is given by user option
`todos-indent-to-here'."
  (set (make-local-variable 'word-wrap) t)
  (set (make-local-variable 'wrap-prefix) (make-string todos-indent-to-here 32))
  (unless (member '(continuation) fringe-indicator-alist)
    (push '(continuation) fringe-indicator-alist)))

(defun todos-indent ()
  "Indent from point to `todos-indent-to-here'."
  (indent-to todos-indent-to-here todos-indent-to-here))

(defun todos-prefix-overlays ()
  "Put before-string overlay in front of this category's items.
The overlay's value is the string `todos-prefix' or with non-nil
`todos-number-prefix' an integer in the sequence from 1 to the
number of todo or done items in the category indicating the
item's priority.  Todo and done items are numbered independently
of each other."
  (when (or todos-number-prefix
	    (not (string-match "^[[:space:]]*$" todos-prefix)))
    (let ((prefix (propertize (concat todos-prefix " ")
			      'face 'todos-prefix-string))
	  (num 0))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when (or (todos-date-string-matcher (line-end-position))
		    (todos-done-string-matcher (line-end-position)))
	    (goto-char (match-beginning 0))
	    (when todos-number-prefix
	      (setq num (1+ num))
	      ;; Reset number for done items.
	      (when
		  ;; FIXME: really need this?
		  ;; If last not done item is multiline, then
		  ;; todos-done-string-matcher skips empty line, so have
		  ;; to look back.
		  (and (looking-at todos-done-string-start)
		       (looking-back (concat "^"
					     (regexp-quote todos-category-done)
					     "\n")))
		(setq num 1))
	      (setq prefix (propertize (concat (number-to-string num) " ")
				       'face 'todos-prefix-string)))
	    (let ((ovs (overlays-in (point) (point)))
		  marked ov-pref)
	      (if ovs
		  (dolist (ov ovs)
		    (let ((val (overlay-get ov 'before-string)))
		      (if (equal val "*")
			  (setq marked t)
			(setq ov-pref val)))))
	      (unless (equal ov-pref prefix)
		(remove-overlays (point) (point)) ; 'before-string) doesn't work
		(overlay-put (make-overlay (point) (point))
			     'before-string prefix)
		(and marked (overlay-put (make-overlay (point) (point))
					 'before-string todos-item-mark)))))
	  (forward-line))))))

(defun todos-reset-prefix (symbol value)
  "The :set function for `todos-prefix' and `todos-number-prefix'."
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
	    ;; Activate the new setting (save-restriction does not help).
	    (save-excursion (todos-category-select))))))))

(defun todos-reset-nondiary-marker (symbol value)
  "The :set function for user option `todos-nondiary-marker'."
  (let ((oldvalue (symbol-value symbol))
	(files (append todos-files todos-archives)))
    (custom-set-default symbol value)
    ;; Need to reset these to get font-locking right.
    (setq todos-nondiary-start (nth 0 todos-nondiary-marker)
	  todos-nondiary-end (nth 1 todos-nondiary-marker)
	  todos-date-string-start
	  ;; FIXME: with ? matches anything
	  (concat "^\\(" (regexp-quote todos-nondiary-start) "\\|"
		  (regexp-quote diary-nonmarking-symbol) "\\)?"))
    (when (not (equal value oldvalue))
      (dolist (f files)
	(with-current-buffer (find-file-noselect f)
	  (let (buffer-read-only)
	    (widen)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (re-search-forward
		   (concat "^\\(" todos-done-string-start "[^][]+] \\)?"
			   "\\(?1:" (regexp-quote (car oldvalue))
			   "\\)" todos-date-pattern "\\( "
			   diary-time-regexp "\\)?\\(?2:"
			   (regexp-quote (cadr oldvalue)) "\\)")
		   nil t)
		  (progn
		    (replace-match (nth 0 value) t t nil 1)
		    (replace-match (nth 1 value) t t nil 2))
		(forward-line)))
	    (todos-category-select)))))))

(defun todos-reset-done-string (symbol value)
  "The :set function for user option `todos-done-string'."
  (let ((oldvalue (symbol-value symbol))
	(files (append todos-files todos-archives)))
    (custom-set-default symbol value)
    ;; Need to reset this to get font-locking right.
    (setq todos-done-string-start
	  (concat "^\\[" (regexp-quote todos-done-string)))
    (when (not (equal value oldvalue))
      (dolist (f files)
	(with-current-buffer (find-file-noselect f)
	  (let (buffer-read-only)
	    (widen)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (re-search-forward
		   (concat "^" (regexp-quote todos-nondiary-start)
			   "\\(" (regexp-quote oldvalue) "\\)")
		   nil t)
		  (replace-match value t t nil 1)
		(forward-line)))
	    (todos-category-select)))))))

(defun todos-reset-comment-string (symbol value)
  "The :set function for user option `todos-comment-string'."
  (let ((oldvalue (symbol-value symbol))
  	(files (append todos-files todos-archives)))
    (custom-set-default symbol value)
    (when (not (equal value oldvalue))
      (dolist (f files)
  	(with-current-buffer (find-file-noselect f)
  	  (let (buffer-read-only)
  	    (save-excursion
	      (widen)
	      (goto-char (point-min))
	      (while (not (eobp))
		(if (re-search-forward
		     (concat
			     "\\[\\(" (regexp-quote oldvalue) "\\): [^]]*\\]")
		     nil t)
		    (replace-match value t t nil 1)
		  (forward-line)))
	      (todos-category-select))))))))

(defun todos-reset-categories (symbol value)
  "The :set function for `todos-ignore-archived-categories'."
  (custom-set-default symbol value)
  (dolist (f (funcall todos-files-function))
    (with-current-buffer (find-file-noselect f)
      (if value
	  (setq todos-categories-full todos-categories
		todos-categories (todos-truncate-categories-list))
	(setq todos-categories todos-categories-full
	      todos-categories-full nil))
      (todos-category-select))))

(defun todos-toggle-show-current-file (symbol value)
  "The :set function for user option `todos-show-current-file'."
  (custom-set-default symbol value)
  (if value
      (add-hook 'pre-command-hook 'todos-show-current-file nil t)
    (remove-hook 'pre-command-hook 'todos-show-current-file t)))

(defun todos-show-current-file ()
  "Visit current instead of default Todos file with `todos-show'.
This function is added to `pre-command-hook' when user option
`todos-show-current-file' is set to non-nil."
  (setq todos-global-current-todos-file todos-current-todos-file))
  ;; (and (eq major-mode 'todos-mode)
       ;; (setq todos-global-current-todos-file (buffer-file-name))))

;; FIXME: rename to todos-set-category-number ?
(defun todos-category-number (cat)
  "Set and return buffer-local value of `todos-category-number'.
This value is one more than the index of category CAT, starting
with one instead of zero, so that the highest priority
category (see `todos-display-categories') has the number one."
  (let ((categories (mapcar 'car todos-categories)))
    (setq todos-category-number
	  (1+ (- (length categories)
		 (length (member cat categories)))))))

(defun todos-current-category ()
  "Return the name of the current category."
  (car (nth (1- todos-category-number) todos-categories)))

(defun todos-category-select ()
  "Display the current category correctly.

With non-nil user option `todos-show-done-only' display only the
category's done (but not archived) items; else (the default)
display just the todo items, or with non-nil user option
`todos-show-with-done' also display the category's done items
below the todo items."
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
	  (funcall todos-mode-line-function name))
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
      ;; Display or hide done items as per todos-show-with-done.
      ;; FIXME: use todos-done-string-start ?
      (when (re-search-forward (concat "\n\\(\\["
				       (regexp-quote todos-done-string)
				       "\\)") nil t)
	(let (done-sep prefix ov-pref ov-done)
	  ;; FIXME: delete overlay when not viewing done items?
	  (when todos-show-with-done
	    (setq done-sep todos-done-separator)
	    (setq done-start cat-end)
	    (setq ov-pref (make-overlay done-sep-start done-end))
	    (overlay-put ov-pref 'display done-sep))))
      (narrow-to-region (point-min) done-start))))

(defun todos-insert-with-overlays (item)
  "Insert ITEM and update prefix/priority number overlays."
  (todos-item-start)
  (insert item "\n")
  (todos-backward-item)
  (todos-prefix-overlays))

(defvar todos-item-start ;; (concat "^\\(\\[\\(" (regexp-quote todos-done-string)
			 ;; 	 "\\)?\\)?" todos-date-pattern)
  (concat "\\(" todos-date-string-start "\\|" todos-done-string-start
	  "\\)" todos-date-pattern)
  "String identifying start of a Todos item.")

(defun todos-item-start ()
  "Move to start of current Todos item and return its position."
  (unless (or
	   ;; Point is either on last item in this category or on the empty
	   ;; line between done and not done items.
	   (looking-at "^$")
	   ;; There are no done items in this category yet.
	   (looking-at (regexp-quote todos-category-beg)))
    (goto-char (line-beginning-position))
    (while (not (looking-at todos-item-start))
      (forward-line -1))
    (point)))

(defun todos-item-end ()
  "Move to end of current Todos item and return its position."
  ;; Items cannot end with a blank line.
  (unless (looking-at "^$")
    (let ((done (todos-done-item-p)))
      (todos-forward-item)
      ;; Adjust if item is last unfinished one before displayed done items.
      (when (and (not done) (todos-done-item-p))
	(forward-line -1))
      (backward-char))
    (point)))

(defun todos-remove-item ()
  "Internal function called in editing, deleting or moving items."
  (let* ((beg (todos-item-start))
	 (end (progn (todos-item-end) (1+ (point))))
	 (ovs (overlays-in beg beg)))
    ;; There can be both prefix/number and mark overlays.
    (while ovs (delete-overlay (car ovs)) (pop ovs))
    (delete-region beg end)))

(defun todos-item-string ()
  "Return bare text of current item as a string."
  (let ((opoint (point))
	(start (todos-item-start))
	(end (todos-item-end)))
    (goto-char opoint)
    (and start end (buffer-substring-no-properties start end))))

(defun todos-diary-item-p ()
  "Return non-nil if item at point is marked for diary inclusion."
  (save-excursion
    (todos-item-start)
    (looking-at todos-date-pattern)))

(defun todos-done-item-p ()
  "Return non-nil if item at point is a done item."
  (save-excursion
    (todos-item-start)
    (looking-at todos-done-string-start)))

(defvar todos-item-mark (propertize (if (equal todos-prefix "*") "@" "*")
				    'face 'todos-mark)
  "String used to mark items.")

(defun todos-item-marked-p ()
  "If this item is marked, return mark overlay."
  (let ((ovs (overlays-in (line-beginning-position) (line-beginning-position)))
	(mark todos-item-mark)
	ov marked)
    (catch 'stop
      (while ovs
	(setq ov (pop ovs))
	(and (equal (overlay-get ov 'before-string) mark)
	     (throw 'stop (setq marked t)))))
    (when marked ov)))

(defvar todos-categories-with-marks nil
  "Alist of categories and number of marked items they contain.")

(defun todos-get-count (type &optional category)
  "Return count of TYPE items in CATEGORY.
If CATEGORY is nil, default to the current category."
  (let* ((cat (or category (todos-current-category)))
	 (counts (cdr (assoc cat todos-categories)))
	 (idx (cond ((eq type 'todo) 0)
		    ((eq type 'diary) 1)
		    ((eq type 'done) 2)
		    ((eq type 'archived) 3))))
    (aref counts idx)))

(defun todos-set-count (type increment &optional category)
  "Increment count of TYPE items in CATEGORY by INCREMENT.
If CATEGORY is nil, default to the current category."
  (let* ((cat (or category (todos-current-category)))
	 (counts (cdr (assoc cat todos-categories)))
	 (idx (cond ((eq type 'todo) 0)
		    ((eq type 'diary) 1)
		    ((eq type 'done) 2)
		    ((eq type 'archived) 3))))
    (aset counts idx (+ increment (aref counts idx)))))

;; (defun todos-item-counts (operation &optional cat1 cat2)
;;   "Update item counts in category CAT1 changed by OPERATION.
;; If CAT1 is nil, update counts from the current category.  With
;; non-nil CAT2 include specified counts from that category in the
;; calculation for CAT1.
;; After updating the item counts, update the `todos-categories' sexp."
;;   (let* ((cat (or cat1 (todos-current-category))))
;;     (cond ((eq type 'insert)
;; 	   (todos-set-count 'todo 1 cat))
;; 	  ((eq type 'diary)
;; 	   (todos-set-count 'diary 1 cat))
;; 	  ((eq type 'nondiary)
;; 	   (todos-set-count 'diary -1 cat))
;; 	  ((eq type 'delete)
;; 	   ;; FIXME: ok if last done item was deleted?
;; 	   (if (save-excursion
;; 		 (re-search-backward (concat "^" (regexp-quote
;; 						  todos-category-done)) nil t))
;; 	       (todos-set-count 'done -1 cat)
;; 	     (todos-set-count 'todo -1 cat)))
;; 	  ((eq type 'done)
;; 	   (unless (member (buffer-file-name) (funcall todos-files-function t))
;; 	     (todos-set-count 'todo -1 cat))
;; 	   (todos-set-count 'done 1 cat))
;; 	  ((eq type 'undo)
;; 	   (todos-set-count 'todo 1 cat)
;; 	   (todos-set-count 'done -1 cat))
;; 	  ((eq type 'archive1)
;; 	   (todos-set-count 'archived 1 cat)
;; 	   (todos-set-count 'done -1 cat))
;; 	  ((eq type 'archive)
;; 	   (if (member (buffer-file-name) (funcall todos-files-function t))
;; 	       ;; In Archive file augment done count with cat's previous
;; 	       ;; done count,
;; 	       (todos-set-count 'done (todos-get-count 'done cat) cat)
;; 	     ;; In Todos file augment archive count with cat's previous
;; 	     ;; done count, and make the latter zero.
;; 	     (todos-set-count 'archived (todos-get-count 'done cat) cat)
;; 	     (todos-set-count 'done (- (todos-get-count 'done cat)) cat)))
;; 	  ((eq type 'merge)
;; 	   ;; Augment todo and done counts of cat by those of cat2.
;; 	   (todos-set-count 'todo (todos-get-count 'todo cat2) cat)
;; 	   (todos-set-count 'done (todos-get-count 'done cat2) cat)))
;;     (todos-update-categories-sexp)))

(defun todos-set-categories ()
  "Set `todos-categories' from the sexp at the top of the file."
  ;; New archive files created by `todos-move-category' are empty, which would
  ;; make the sexp test fail and raise an error, so in this case we skip it.
  (unless (zerop (buffer-size))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	;; todos-truncate-categories-list needs non-nil todos-categories.
	(setq todos-categories-full
	      (if (looking-at "\(\(\"")
		  (read (buffer-substring-no-properties
			 (line-beginning-position)
			 (line-end-position)))
		(error "Invalid or missing todos-categories sexp"))
	      todos-categories todos-categories-full)))
    (if (and todos-ignore-archived-categories
	     (eq major-mode 'todos-mode))
	(todos-truncate-categories-list)
      todos-categories-full)))

;; FIXME: currently unused -- make this a command to rebuild a corrupted
;; todos-cats sexp ?
(defun todos-make-categories-list (&optional force)
  "Return an alist of Todos categories and their item counts.
With non-nil argument FORCE parse the entire file to build the
list; otherwise, get the value by reading the sexp at the top of
the file."
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
		 ;; Counts for each category: [todo diary done archive]
		 (setq counts (make-vector 4 0))
		 (setq todos-categories
		       (append todos-categories (list (cons cat counts))))
		 ;; todos-archives may be too old here (e.g. during
		 ;; todos-move-category).
		 (when (member archive (funcall todos-files-function t))
		   (with-current-buffer (find-file-noselect archive)
		     (widen)
		     (goto-char (point-min))
		     (when (re-search-forward
			    (concat (regexp-quote todos-category-beg) cat)
			    (point-max) t)
		       (forward-line)
		       (while (not (or (looking-at
					(concat
					 (regexp-quote todos-category-beg)
					 "\\(.*\\)\n"))
				       (eobp)))
			 (when (looking-at todos-done-string-start)
			   (todos-set-count 'archived 1 cat))
			 (forward-line))))))
		((looking-at todos-done-string-start)
		 (todos-set-count 'done 1 cat))
		((looking-at (concat "^\\("
				     (regexp-quote diary-nonmarking-symbol)
				     "\\)?" todos-date-pattern))
		 (todos-set-count 'diary 1 cat)
		 (todos-set-count 'todo 1 cat))
		((looking-at (concat todos-date-string-start todos-date-pattern))
		 (todos-set-count 'todo 1 cat))
		;; If first line is todos-categories list, use it and end loop
		;; unless forced by non-nil parameter `force' to scan whole file.
		((bobp)
		 (unless force
		   (setq todos-categories (read (buffer-substring-no-properties
						 (line-beginning-position)
						 (line-end-position))))
		   (goto-char (1- (point-max))))))
	  (forward-line)))))
  todos-categories)

(defun todos-truncate-categories-list ()
  "Return a truncated alist of Todos categories plus item counts.
Categories containing only archived items are omitted.  This list
is used in Todos mode when `todos-ignore-archived-categories' is
non-nil."
  (let (cats)
    (dolist (catcons todos-categories-full cats)
      (let ((cat (car catcons)))
	(setq cats
	      (append cats
		      (unless (and (zerop (todos-get-count 'todo cat))
				   (zerop (todos-get-count 'done cat))
				   (not (zerop (todos-get-count 'archived cat))))
			(list catcons))))))))

(defun todos-update-categories-sexp ()
  "Update the `todos-categories' sexp at the top of the file."
  (let (buffer-read-only)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if (looking-at (concat "^" (regexp-quote todos-category-beg)))
	    (progn (newline) (goto-char (point-min)))
	  ;; With empty buffer (e.g. with new archive in
	  ;; `todos-move-category') `kill-line' signals end of buffer.
	  (kill-region (line-beginning-position) (line-end-position)))
	;; FIXME
	;; (prin1 todos-categories (current-buffer))))))
	(prin1 todos-categories-full (current-buffer))))))

(defun todos-read-file-name (prompt &optional archive mustmatch)
  "Choose and return the name of a Todos file, prompting with PROMPT.
Show completions with TAB or SPC; the names are shown in short
form but the absolute truename is returned.  With non-nil ARCHIVE
return the absolute truename of a Todos archive file.  With non-nil
MUSTMATCH the name of an existing file must be chosen;
otherwise, a new file name is allowed."	;FIXME: is this possible?
  (unless (file-exists-p todos-files-directory)
    (make-directory todos-files-directory))
  (let* ((completion-ignore-case t)
	 (files (mapcar 'file-name-sans-extension
			(directory-files todos-files-directory nil
					 (if archive "\.toda$" "\.todo$"))))
	 (file (concat todos-files-directory
		       (completing-read prompt files nil mustmatch)
		       (if archive ".toda" ".todo"))))
    (file-truename file)))

(defun todos-read-category (prompt &optional mustmatch)
  "Choose and return a category name, prompting with PROMPT.
Show completions with TAB or SPC.  With non-nil MUSTMATCH the
name must be that of an existing category; otherwise, a new
category name is allowed, after checking its validity."
  ;; Allow SPC to insert spaces, for adding new category names.
  (let ((map minibuffer-local-completion-map))
    (define-key map " " nil)
    ;; Make a copy of todos-categories in case history-delete-duplicates is
    ;; non-nil, which makes completing-read alter todos-categories.
    (let* ((categories (copy-sequence todos-categories))
	   (history (cons 'todos-categories (1+ todos-category-number)))
	   (completion-ignore-case todos-completion-ignore-case)
	   (category (completing-read prompt todos-categories nil
				      mustmatch nil history
				      (if todos-categories
					  (todos-current-category)
					;; Trigger prompt for initial category
					""))))
      ;; FIXME: let "" return todos-current-category
      (unless mustmatch
	(when (and (not (assoc category categories))
		   (y-or-n-p (format (concat "There is no category \"%s\" in "
					     "this file; add it? ") category)))
	  (todos-validate-category-name category)
	  (todos-add-category category)))
      ;; Restore the original value of todos-categories.
      (setq todos-categories categories)
      category)))

(defun todos-validate-category-name (cat)
  "Check new category name CAT and when valid return it."
  (let (prompt)
    (while
	(and (cond ((string= "" cat)
		    ;; (if todos-categories
		    ;; 	(setq prompt "Enter a non-empty category name: ")
		      ;; Prompt for initial category of a new Todos file.
		      (setq prompt (concat "Initial category name ["
					   todos-initial-category "]: ")));)
		   ((string-match "\\`\\s-+\\'" cat)
		    (setq prompt
			  "Enter a category name that is not only white space: "))
		   ;; FIXME: add completion
		   ((assoc cat todos-categories)
		    (setq prompt "Enter a non-existing category name: ")))
	     (setq cat (if todos-categories
			   (read-from-minibuffer prompt)
			 ;; Offer default initial category name.
			 (prin1-to-string
			  (read-from-minibuffer prompt nil nil t nil
						(list todos-initial-category))))))))
  cat)

;; (defun todos-read-category (prompt)
;;   "Prompt with PROMPT for an existing category name and return it.
;; Show completions with TAB or SPC."
;;   ;; Make a copy of todos-categories in case history-delete-duplicates is
;;   ;; non-nil, which makes completing-read alter todos-categories.
;;   (let* ((categories (copy-sequence todos-categories))
;; 	 (history (cons 'todos-categories (1+ todos-category-number)))
;; 	 (completion-ignore-case todos-completion-ignore-case)
;; 	 (category (completing-read prompt todos-categories nil
;; 				    mustmatch nil history)))
;;     (setq category (completing-read prompt todos-categories nil t))
;;     ;; Restore the original value of todos-categories.
;;     (setq todos-categories categories)
;;     category))

;; (defun todos-new-category-name (prompt)
;;   "Prompt with PROMPT for a new category name and return it."
;;   (let ((map minibuffer-local-completion-map)
;; 	prompt-n)
;;     ;; Allow SPC to insert spaces, for adding new category names.
;;     (define-key map " " nil)
;;     (while
;; 	;; Validate entered category name.
;; 	(and (cond ((string= "" cat)
;; 		    (setq prompt-n
;; 			  (if todos-categories
;; 			      "Enter a non-empty category name: "
;; 			    ;; Prompt for initial category of a new Todos file.
;; 			    (concat "Initial category name ["
;; 				    todos-initial-category "]: "))))
;; 		   ((string-match "\\`\\s-+\\'" cat)
;; 		    (setq prompt-n
;; 			  "Enter a category name that is not only white space: "))
;; 		   ((assoc cat todos-categories)
;; 		    (setq prompt-n "Enter a non-existing category name: ")))
;; 	     (setq cat (if todos-categories
;; 			   (read-from-minibuffer prompt)
;; 			 ;; Offer default initial category name.
;; 			 (prin1-to-string
;; 			  (read-from-minibuffer
;; 			   (or prompt prompt-n) nil nil t nil
;; 			   (list todos-initial-category))))))
;;       (setq prompt nil)))
;;   cat)

;; ;; Adapted from calendar-read-date and calendar-date-string.
(defun todos-read-date ()
  "Prompt for Gregorian date and return it in the current format.
Also accepts `*' as an unspecified month, day, or year."
  (let* ((year (calendar-read
		;; FIXME: maybe better like monthname with RET for current month
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
		      monthname (calendar-make-alist month-array nil nil
						     abbrevs))))
         (last (if (= month 13)
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
	  (or (and (= month 13) "*")
	      (calendar-month-name (calendar-extract-month (list month day year))
				   t)))
    (mapconcat 'eval calendar-date-display-form "")))

(defun todos-read-dayname ()
  "Choose name of a day of the week with completion and return it."
  (let ((completion-ignore-case t))
    (completing-read "Enter a day name: "
		     (append calendar-day-name-array nil)
		     nil t)))
  
(defun todos-read-time ()
  "Prompt for and return a valid clock time as a string.
Valid time strings are those matching `diary-time-regexp'."
  (let (valid answer)
    (while (not valid)
      (setq answer (read-from-minibuffer
		    "Enter a clock time (or return for none): "))
      (when (or (string= "" answer)
		(string-match diary-time-regexp answer))
	(setq valid t)))
    answer))

;;; Sorting and display routines for todos-categories-mode.

(defun todos-display-categories (&optional sortkey)
  "Display a table of the current file's categories and item counts.

In the initial display the categories are numbered, indicating
their current order for navigating by \\[todos-forward-category]
and \\[todos-backward-category].  You can persistantly change the
order of the category at point by typing \\[todos-raise-category]
or \\[todos-lower-category].

The labels above the category names and item counts are buttons,
and clicking these changes the display: sorted by category name
or by the respective item counts (alternately descending or
ascending).  In these displays the categories are not numbered
and \\[todos-raise-category] and \\[todos-lower-category] are
disabled.  (Programmatically, the sorting is triggered by passing
a non-nil SORTKEY argument.)

In addition, the lines with the category names and item counts
are buttonized, and pressing one of these button jumps to the
category in Todos mode (or Todos Archive mode, for categories
containing only archived items, provided user option
`todos-ignore-archived-categories' is non-nil.  These categories
are shown in `todos-archived-only' face."
  (interactive)
  (unless (eq major-mode 'todos-categories-mode)
    (setq todos-global-current-todos-file (or todos-current-todos-file
					      todos-default-todos-file)))
  (let* ((cats0 (if (and todos-ignore-archived-categories
			 (not (eq major-mode 'todos-categories-mode)))
		    todos-categories-full
		  todos-categories))
	 (cats (todos-sort cats0 sortkey))
	 (archive (member todos-current-todos-file todos-archives))
	 ;; `num' is used by todos-insert-category-line.
	 (num 0))
    (set-window-buffer (selected-window)
		       (set-buffer (get-buffer-create todos-categories-buffer)))
    (let (buffer-read-only)
      (erase-buffer)
      (kill-all-local-variables)
      (todos-categories-mode)
      ;; FIXME: add usage tips?
      (insert (format "Category counts for Todos file \"%s\"."
		      (file-name-sans-extension
		       (file-name-nondirectory todos-current-todos-file))))
      (newline 2)
      ;; Make space for the column of category numbers.
      (insert (make-string (+ 4 (length todos-categories-number-separator)) 32))
      ;; Add the category and item count buttons (if this is the list of
      ;; categories in an archive, show only done item counts).
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
	;; Fill in the table with buttonized lines, each showing a category and
	;; its item counts.
	(mapc (lambda (cat) (todos-insert-category-line cat sortkey))
	      (mapcar 'car cats))
	(newline)
	;; Add a line showing item count totals.
	(insert (make-string (+ 4 (length todos-categories-number-separator)) 32)
		(todos-padded-string todos-categories-totals-label)
		(mapconcat
		 (lambda (elt)
		   (concat
		    (make-string (1+ (/ (length (car elt)) 2)) 32)
		    (format "%3d" (nth (cdr elt) (todos-total-item-counts)))
		    ;; Add an extra space if label length is odd (using
		    ;; definition of oddp from cl.el).
		    (if (eq (logand (length (car elt)) 1) 1) " ")))
		 (if archive
		     (list (cons todos-categories-done-label 2))
		   (list (cons todos-categories-todo-label 0)
			 (cons todos-categories-diary-label 1)
			 (cons todos-categories-done-label 2)
			 (cons todos-categories-archived-label 3)))
		 ""))))
    (setq buffer-read-only t)))

;; ;; FIXME: make this toggle with todos-display-categories
;; (defun todos-display-categories-alphabetically ()
;;   ""
;;   (interactive)
;;   (todos-display-sorted 'alpha))

;; ;; FIXME: provide key bindings for these or delete them
;; (defun todos-display-categories-sorted-by-todo ()
;;   ""
;;   (interactive)
;;   (todos-display-sorted 'todo))

;; (defun todos-display-categories-sorted-by-diary ()
;;   ""
;;   (interactive)
;;   (todos-display-sorted 'diary))

;; (defun todos-display-categories-sorted-by-done ()
;;   ""
;;   (interactive)
;;   (todos-display-sorted 'done))

;; (defun todos-display-categories-sorted-by-archived ()
;;   ""
;;   (interactive)
;;   (todos-display-sorted 'archived))

(defun todos-longest-category-name-length (categories)
  "Return the length of the longest name in list CATEGORIES."
  (let ((longest 0))
    (dolist (c categories longest)
      (setq longest (max longest (length c))))))

(defun todos-padded-string (str)
  "Return string STR padded with spaces.
The placement of the padding is determined by the value of user
option `todos-categories-align'."
  (let* ((categories (mapcar 'car todos-categories))
	 (len (max (todos-longest-category-name-length categories)
		   (length todos-categories-category-label)))
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

(defvar todos-descending-counts nil
  "List of keys for category counts sorted in descending order.")

(defun todos-sort (list &optional key)
  "Return a copy of LIST, possibly sorted according to KEY."
  (let* ((l (copy-sequence list))
	 (fn (if (eq key 'alpha)
		   (lambda (x) (upcase x)) ; Alphabetize case insensitively.
		 (lambda (x) (todos-get-count key x))))
	 (descending (member key todos-descending-counts))
	 (cmp (if (eq key 'alpha)
		  'string<
		(if descending '< '>)))
	 (pred (lambda (s1 s2) (let ((t1 (funcall fn (car s1)))
				     (t2 (funcall fn (car s2))))
				 (funcall cmp t1 t2)))))
    (when key
      (setq l (sort l pred))
      (if descending
	  (setq todos-descending-counts
		(delete key todos-descending-counts))
	(push key todos-descending-counts)))
    l))

(defun todos-display-sorted (type)
  "Keep point on the TYPE count sorting button just clicked."
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
  "Insert button for displaying categories sorted by item counts.
LABEL determines which type of count is sorted."
  (setq str (if (string= label todos-categories-category-label)
		(todos-padded-string label)
	      label))
  (setq beg (point))
  (setq end (+ beg (length str)))
  (insert-button str 'face nil
		 'action
		 `(lambda (button)
		    (let ((key (todos-label-to-key ,label)))
		      (if (and (member key todos-descending-counts)
			       (eq key 'alpha))
			  (progn
			    (todos-display-categories)
			    (setq todos-descending-counts
				  (delete key todos-descending-counts)))
			(todos-display-sorted key)))))
  (setq ovl (make-overlay beg end))
  (overlay-put ovl 'face 'todos-button))

(defun todos-total-item-counts ()
  "Return a list of total item counts for the current file."
  (mapcar (lambda (i) (apply '+ (mapcar (lambda (l) (aref l i))
					(mapcar 'cdr todos-categories))))
	  (list 0 1 2 3)))

(defun todos-insert-category-line (cat &optional nonum)
  "Insert button displaying category CAT's name and item counts.
With non-nil argument NONUM show only these; otherwise, insert a
number in front of the button indicating the category's priority.
The number and the category name are separated by the string
which is the value of the user option
`todos-categories-number-separator'."
  (let* ((archive (member todos-current-todos-file todos-archives))
	(str (todos-padded-string cat))
	(opoint (point)))
    ;; num is declared in caller.
    (setq num (1+ num))
    (insert-button
     (concat (if nonum
		 (make-string (+ 4 (length todos-categories-number-separator))
			      32)
	       (format " %3d%s" num todos-categories-number-separator))
	     str
	     (mapconcat (lambda (elt)
			  (concat
			   (make-string (1+ (/ (length (car elt)) 2)) 32) ; label
			   (format "%3d" (todos-get-count (cdr elt) cat)) ; count
			   ;; Add an extra space if label length is odd
			   ;; (using def of oddp from cl.el).
			   (if (eq (logand (length (car elt)) 1) 1) " ")))
			(if archive
			    (list (cons todos-categories-done-label 'done))
			  (list (cons todos-categories-todo-label 'todo)
				(cons todos-categories-diary-label 'diary)
				(cons todos-categories-done-label 'done)
				(cons todos-categories-archived-label
				      'archived)))
			  ""))
     'face (if (and todos-ignore-archived-categories
		    (zerop (todos-get-count 'todo cat))
		    (zerop (todos-get-count 'done cat))
		    (not (zerop (todos-get-count 'archived cat))))
	       'todos-archived-only
	     nil)
     'action `(lambda (button) (let ((buf (current-buffer)))
				 (todos-jump-to-category ,cat)
				 (kill-buffer buf))))
    ;; Highlight the sorted count column.
    (let* ((beg (+ opoint 6 (length str)))
	   end ovl)
      (cond ((eq nonum 'todo)
	     (setq beg (+ beg 1 (/ (length todos-categories-todo-label) 2))))
	    ((eq nonum 'diary)
	     (setq beg (+ beg 1 (length todos-categories-todo-label)
			   2 (/ (length todos-categories-diary-label) 2))))
	    ((eq nonum 'done)
	     (setq beg (+ beg 1 (length todos-categories-todo-label)
			   2 (length todos-categories-diary-label)
			   2 (/ (length todos-categories-done-label) 2))))
	    ((eq nonum 'archived)
	     (setq beg (+ beg 1 (length todos-categories-todo-label)
			   2 (length todos-categories-diary-label)
			   2 (length todos-categories-done-label)
			   2 (/ (length todos-categories-archived-label) 2)))))
      (unless (= beg (+ opoint 6 (length str)))
	(setq end (+ beg 4))
	(setq ovl (make-overlay beg end))
	(overlay-put ovl 'face 'todos-sorted-column)))
    (newline)))

(provide 'todos)

;;; todos.el ends here

;;; necessitated adaptations to diary-lib.el

;; (defun diary-goto-entry (button)
;;   "Jump to the diary entry for the BUTTON at point."
;;   (let* ((locator (button-get button 'locator))
;;          (marker (car locator))
;;          markbuf file opoint)
;;     ;; If marker pointing to diary location is valid, use that.
;;     (if (and marker (setq markbuf (marker-buffer marker)))
;;         (progn
;;           (pop-to-buffer markbuf)
;;           (goto-char (marker-position marker)))
;;       ;; Marker is invalid (eg buffer has been killed, as is the case with
;;       ;; included diary files).
;;       (or (and (setq file (cadr locator))
;;                (file-exists-p file)
;;                (find-file-other-window file)
;;                (progn
;;                  (when (eq major-mode (default-value 'major-mode)) (diary-mode))
;; 		 (when (eq major-mode 'todos-mode) (widen))
;;                  (goto-char (point-min))
;;                  (when (re-search-forward (format "%s.*\\(%s\\)"
;; 						  (regexp-quote (nth 2 locator))
;; 						  (regexp-quote (nth 3 locator)))
;; 					  nil t)
;; 		   (goto-char (match-beginning 1))
;; 		   (when (eq major-mode 'todos-mode)
;; 		     (setq opoint (point))
;; 		     (re-search-backward (concat "^"
;; 						 (regexp-quote todos-category-beg)
;; 						 "\\(.*\\)\n")
;; 					 nil t)
;; 		     (todos-category-number (match-string 1))
;; 		     (todos-category-select)
;; 		     (goto-char opoint)))))
;;           (message "Unable to locate this diary entry")))))
