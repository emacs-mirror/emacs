;;; imenu.el --- framework for mode-specific buffer indexes

;; Copyright (C) 1994, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.

;; Author: Ake Stenhoff <etxaksf@aom.ericsson.se>
;;         Lars Lindberg <lli@sypro.cap.se>
;; Maintainer: FSF
;; Created: 8 Feb 1994
;; Keywords: tools convenience

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

;;; Commentary:

;; Purpose of this package:
;;   To present a framework for mode-specific buffer indexes.
;;   A buffer index is an alist of names and buffer positions.
;;   For instance all functions in a C-file and their positions.
;;
;;   It is documented in the Emacs Lisp manual.
;;
;; How it works:

;;   A mode-specific function is called to generate the index.  It is
;;   then presented to the user, who can choose from this index.
;;
;;   The package comes with a set of example functions for how to
;;   utilize this package.

;;   There are *examples* for index gathering functions/regular
;;   expressions for C/C++ and Lisp/Emacs Lisp but it is easy to
;;   customize for other modes.  A function for jumping to the chosen
;;   index position is also supplied.

;;; History:
;;  Thanks go to
;;  [simon] - Simon Leinen simon@lia.di.epfl.ch
;;  [dean] - Dean Andrews ada@unison.com
;;  [alon] - Alon Albert al@mercury.co.il
;;  [greg] - Greg Thompson gregt@porsche.visix.COM
;;  [wolfgang] - Wolfgang Bangerth zcg51122@rpool1.rus.uni-stuttgart.de
;;  [kai] - Kai Grossjohann grossjoh@linus.informatik.uni-dortmund.de
;;  [david] - David M. Smith dsmith@stats.adelaide.edu.au
;;  [christian] - Christian Egli Christian.Egli@hcsd.hac.com
;;  [karl] - Karl Fogel kfogel@floss.life.uiuc.edu

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup imenu nil
  "Mode-specific buffer indexes."
  :group 'matching
  :group 'frames
  :group 'convenience
  :link '(custom-manual "(elisp)Imenu"))

(defcustom imenu-use-markers t
  "*Non-nil means use markers instead of integers for Imenu buffer positions.

Setting this to nil makes Imenu work a little faster but editing the
buffer will make the generated index positions wrong.

This might not yet be honored by all index-building functions."
  :type 'boolean
  :group 'imenu)


(defcustom imenu-max-item-length 60
  "*If a number, truncate Imenu entries to that length."
  :type '(choice integer
		 (const :tag "Unlimited"))
  :group 'imenu)

(defcustom imenu-auto-rescan nil
  "*Non-nil means Imenu should always rescan the buffers."
  :type 'boolean
  :group 'imenu)

(defcustom imenu-auto-rescan-maxout 60000
  "*Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
This variable is buffer-local."
  :type 'integer
  :group 'imenu)

(defcustom imenu-always-use-completion-buffer-p nil
  "*Set this to non-nil for displaying the index in a completion buffer.

`never' means never automatically display a listing of any kind.
A value of nil (the default) means display the index as a mouse menu
if the mouse was used to invoke `imenu'.
Another non-nil value means always display the index in a completion buffer."
  :type '(choice (const :tag "On Mouse" nil)
		 (const :tag "Never" never)
		 (other :tag "Always" t))
  :group 'imenu)

(defcustom imenu-after-jump-hook nil
  "*Hooks called after jumping to a place in the buffer.

Useful things to use here include `reposition-window', `recenter', and
\(lambda () (recenter 0)) to show at top of screen."
  :type 'hook
  :group 'imenu)

;;;###autoload
(defcustom imenu-sort-function nil
  "*The function to use for sorting the index mouse-menu.

Affects only the mouse index menu.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

Set it to `imenu--sort-by-name' if you want alphabetic sorting.

The function should take two arguments and return t if the first
element should come before the second.  The arguments are cons cells;
\(NAME . POSITION).  Look at `imenu--sort-by-name' for an example."
  :type '(choice (const :tag "No sorting" nil)
		 (const :tag "Sort by name" imenu--sort-by-name)
		 (function :tag "Another function"))
  :group 'imenu)

(defcustom imenu-max-items 25
  "*Maximum number of elements in a mouse menu for Imenu."
  :type 'integer
  :group 'imenu)

(defcustom imenu-scanning-message "Scanning buffer for index (%3d%%)"
  "*Progress message during the index scanning of the buffer.
If non-nil, user gets a message during the scanning of the buffer.

Relevant only if the mode-specific function that creates the buffer
index use `imenu-progress-message', and not useful if that is fast, in
which case you might as well set this to nil."
  :type '(choice string
		 (const :tag "None" nil))
  :group 'imenu)

(defcustom imenu-space-replacement "."
  "*The replacement string for spaces in index names.
Used when presenting the index in a completion buffer to make the
names work as tokens."
  :type 'string
  :group 'imenu)

(defcustom imenu-level-separator ":"
  "*The separator between index names of different levels.
Used for making mouse-menu titles and for flattening nested indexes
with name concatenation."
  :type 'string
  :group 'imenu)

;;;###autoload
(defvar imenu-generic-expression nil
  "The regex pattern to use for creating a buffer index.

If non-nil this pattern is passed to `imenu--generic-function'
to create a buffer index.

The value should be an alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX)
or like this:
 (MENU-TITLE REGEXP INDEX FUNCTION ARGUMENTS...)
with zero or more ARGUMENTS.  The former format creates a simple element in
the index alist when it matches; the latter creates a special element
of the form  (NAME POSITION-MARKER FUNCTION ARGUMENTS...)
with FUNCTION and ARGUMENTS copied from `imenu-generic-expression'.

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

The variable is buffer-local.

The variable `imenu-case-fold-search' determines whether or not the
regexp matches are case sensitive, and `imenu-syntax-alist' can be
used to alter the syntax table for the search.

For example, see the value of `fortran-imenu-generic-expression' used by
`fortran-mode' with `imenu-syntax-alist' set locally to give the
characters which normally have \"symbol\" syntax \"word\" syntax
during matching.")

;;;###autoload
(make-variable-buffer-local 'imenu-generic-expression)

;;;; Hooks

;;;###autoload
(defvar imenu-create-index-function 'imenu-default-create-index-function
  "The function to use for creating a buffer index.

It should be a function that takes no arguments and returns an index
of the current buffer as an alist.

Simple elements in the alist look like (INDEX-NAME . INDEX-POSITION).
Special elements look like (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...).
A nested sub-alist element looks like (INDEX-NAME SUB-ALIST).
The function `imenu--subalist-p' tests an element and returns t
if it is a sub-alist.

This function is called within a `save-excursion'.

The variable is buffer-local.")
;;;###autoload
(make-variable-buffer-local 'imenu-create-index-function)

;;;###autoload
(defvar imenu-prev-index-position-function 'beginning-of-defun
  "Function for finding the next index position.

If `imenu-create-index-function' is set to
`imenu-default-create-index-function', then you must set this variable
to a function that will find the next index, looking backwards in the
file.

The function should leave point at the place to be connected to the
index and it should return nil when it doesn't find another index.

This variable is local in all buffers.")
;;;###autoload
(make-variable-buffer-local 'imenu-prev-index-position-function)

;;;###autoload
(defvar imenu-extract-index-name-function nil
  "Function for extracting the index item name, given a position.

This function is called after `imenu-prev-index-position-function'
finds a position for an index item, with point at that position.
It should return the name for that index item.

This variable is local in all buffers.")
;;;###autoload
(make-variable-buffer-local 'imenu-extract-index-name-function)

;;;###autoload
(defvar imenu-name-lookup-function nil
  "Function to compare string with index item.

This function will be called with two strings, and should return
non-nil if they match.

If nil, comparison is done with `string='.
Set this to some other function for more advanced comparisons,
such as \"begins with\" or \"name matches and number of
arguments match\".

This variable is local in all buffers.")
;;;###autoload
(make-variable-buffer-local 'imenu-name-lookup-function)

;;;###autoload
(defvar imenu-default-goto-function 'imenu-default-goto-function
  "The default function called when selecting an Imenu item.
The function in this variable is called when selecting a normal index-item.")
;;;###autoload
(make-variable-buffer-local 'imenu-default-goto-function)


(defun imenu--subalist-p (item)
  (and (consp (cdr item)) (listp (cadr item))
       (not (eq (car (cadr item)) 'lambda))))

;; Macro to display a progress message.
;; RELPOS is the relative position to display.
;; If RELPOS is nil, then the relative position in the buffer
;; is calculated.
;; PREVPOS is the variable in which we store the last position displayed.
(defmacro imenu-progress-message (prevpos &optional relpos reverse)
  `(and
    imenu-scanning-message
    (let ((pos ,(if relpos
		    relpos
		  `(imenu--relative-position ,reverse))))
      (if ,(if relpos t
	     `(> pos (+ 5 ,prevpos)))
	  (progn
	    (message imenu-scanning-message pos)
	    (setq ,prevpos pos))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Some examples of functions utilizing the framework of this
;;;; package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the current/previous sexp and the location of the sexp (its
;; beginning) without moving the point.
(defun imenu-example--name-and-position ()
  (save-excursion
    (forward-sexp -1)
    ;; [ydi] modified for imenu-use-markers
    (let ((beg (if imenu-use-markers (point-marker) (point)))
	  (end (progn (forward-sexp) (point))))
      (cons (buffer-substring beg end)
	    beg))))

;;;
;;; Lisp
;;;

(defun imenu-example--lisp-extract-index-name ()
  ;; Example of a candidate for `imenu-extract-index-name-function'.
  ;; This will generate a flat index of definitions in a lisp file.
  (save-match-data
    (and (looking-at "(def")
	 (condition-case nil
	     (progn
	       (down-list 1)
	       (forward-sexp 2)
	       (let ((beg (point))
		     (end (progn (forward-sexp -1) (point))))
		 (buffer-substring beg end)))
	   (error nil)))))

(defun imenu-example--create-lisp-index ()
  ;; Example of a candidate for `imenu-create-index-function'.
  ;; It will generate a nested index of definitions.
  (let ((index-alist '())
	(index-var-alist '())
	(index-type-alist '())
	(index-unknown-alist '())
	prev-pos)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (while (beginning-of-defun)
      (imenu-progress-message prev-pos nil t)
	  (save-match-data
	    (and (looking-at "(def")
		 (save-excursion
	       (down-list 1)
		   (cond
		((looking-at "def\\(var\\|const\\)")
		     (forward-sexp 2)
		     (push (imenu-example--name-and-position)
			   index-var-alist))
		((looking-at "def\\(un\\|subst\\|macro\\|advice\\)")
		     (forward-sexp 2)
		     (push (imenu-example--name-and-position)
			   index-alist))
		((looking-at "def\\(type\\|struct\\|class\\|ine-condition\\)")
		     (forward-sexp 2)
 		 (if (= (char-after (1- (point))) ?\))
			 (progn
 		       (forward-sexp -1)
			   (down-list 1)
 		       (forward-sexp 1)))
		     (push (imenu-example--name-and-position)
			   index-type-alist))
		    (t
		     (forward-sexp 2)
		     (push (imenu-example--name-and-position)
		       index-unknown-alist)))))))
    (imenu-progress-message prev-pos 100)
    (and index-var-alist
	 (push (cons "Variables" index-var-alist)
	       index-alist))
    (and index-type-alist
 	 (push (cons "Types" index-type-alist)
  	       index-alist))
    (and index-unknown-alist
	 (push (cons "Syntax-unknown" index-unknown-alist)
	       index-alist))
    index-alist))

;; Regular expression to find C functions
(defvar imenu-example--function-name-regexp-c
  (concat
   "^[a-zA-Z0-9]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"	; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
   "\\([*&]+[ \t]*\\)?"			; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; name
   ))

(defun imenu-example--create-c-index (&optional regexp)
  (let ((index-alist '())
	prev-pos char)
    (goto-char (point-min))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (save-match-data
      (while (re-search-forward
	      (or regexp imenu-example--function-name-regexp-c)
	      nil t)
	(imenu-progress-message prev-pos)
	(backward-up-list 1)
	(save-excursion
	  (goto-char (scan-sexps (point) 1))
	  (setq char (following-char)))
	;; Skip this function name if it is a prototype declaration.
	(if (not (eq char ?\;))
	    (push (imenu-example--name-and-position) index-alist))))
    (imenu-progress-message prev-pos 100)
    (nreverse index-alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The item to use in the index for rescanning the buffer.
(defconst imenu--rescan-item '("*Rescan*" . -99))

;; The latest buffer index.
;; Buffer local.
(defvar imenu--index-alist nil
  "The buffer index computed for this buffer in Imenu.
Simple elements in the alist look like (INDEX-NAME . INDEX-POSITION).
Special elements look like (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...).
A nested sub-alist element looks like (INDEX-NAME SUB-ALIST).

This variable is local in all buffers, once set.")

(make-variable-buffer-local 'imenu--index-alist)

(defvar imenu--last-menubar-index-alist nil
  "The latest buffer index used to update the menu bar menu.")

(make-variable-buffer-local 'imenu--last-menubar-index-alist)

;; History list for 'jump-to-function-in-buffer'.
;; Making this buffer local caused it not to work!
(defvar imenu--history-list nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal support functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Sort function
;;; Sorts the items depending on their index name.
;;; An item looks like (NAME . POSITION).
;;;
(defun imenu--sort-by-name (item1 item2)
  (string-lessp (car item1) (car item2)))

(defun imenu--sort-by-position (item1 item2)
  (< (cdr item1) (cdr item2)))

(defun imenu--relative-position (&optional reverse)
  ;; Support function to calculate relative position in buffer
  ;; Beginning of buffer is 0 and end of buffer is 100
  ;; If REVERSE is non-nil then the beginning is 100 and the end is 0.
  (let ((pos (point))
	(total (buffer-size)))
    (and reverse (setq pos (- total pos)))
    (if (> total 50000)
	;; Avoid overflow from multiplying by 100!
	(/ (1- pos) (max (/ total 100) 1))
      (/ (* 100 (1- pos)) (max total 1)))))

;; Split LIST into sublists of max length N.
;; Example (imenu--split '(1 2 3 4 5 6 7 8) 3)-> '((1 2 3) (4 5 6) (7 8))
(defun imenu--split (list n)
  (let ((remain list)
	(result '())
	(sublist '())
	(i 0))
    (while remain
      (push (pop remain) sublist)
      (incf i)
      (and (= i n)
	   ;; We have finished a sublist
	   (progn (push (nreverse sublist) result)
		  (setq i 0)
		  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
	 (push (nreverse sublist) result))
    (nreverse result)))

;;; Split the alist MENULIST into a nested alist, if it is long enough.
;;; In any case, add TITLE to the front of the alist.
(defun imenu--split-menu (menulist title)
  (let (keep-at-top tail)
    (if (memq imenu--rescan-item menulist)
	(setq keep-at-top (cons imenu--rescan-item nil)
	      menulist (delq imenu--rescan-item menulist)))
    (setq tail menulist)
    (dolist (item tail)
      (if (imenu--subalist-p item)
	  (setq keep-at-top (cons item keep-at-top)
		menulist (delq item menulist))))
    (if imenu-sort-function
	(setq menulist
	      (sort
	       (copy-sequence menulist)
	       imenu-sort-function)))
    (if (> (length menulist) imenu-max-items)
	(let ((count 0))
	  (setq menulist
		(mapcar
		 (function
		  (lambda (menu)
		    (cons (format "From: %s" (caar menu)) menu)))
		 (imenu--split menulist imenu-max-items)))))
    (cons title
	  (nconc (nreverse keep-at-top) menulist))))

;;; Split up each long alist that are nested within ALIST
;;; into nested alists.
(defun imenu--split-submenus (alist)
  (mapcar (function
	   (lambda (elt)
	     (if (and (consp elt)
		      (stringp (car elt))
		      (listp (cdr elt)))
		 (imenu--split-menu (cdr elt) (car elt))
	       elt)))
	  alist))

;;; Truncate all strings in MENULIST to imenu-max-item-length
(defun imenu--truncate-items (menulist)
  (mapcar (function
	   (lambda (item)
	     (cond
	      ((consp (cdr item))
	       (imenu--truncate-items (cdr item)))
	      (t
	       ;; truncate if necessary
	       (if (and (numberp imenu-max-item-length)
			(> (length (car item)) imenu-max-item-length))
		   (setcar item (substring (car item) 0
					   imenu-max-item-length)))))))
	  menulist))


(defun imenu--make-index-alist (&optional noerror)
  "Create an index-alist for the definitions in the current buffer.

Report an error if the list is empty unless NOERROR is supplied and
non-nil.

Simple elements in the alist look like (INDEX-NAME . INDEX-POSITION).
Special elements look like (INDEX-NAME FUNCTION ARGUMENTS...).
A nested sub-alist element looks like (INDEX-NAME SUB-ALIST).
The function `imenu--subalist-p' tests an element and returns t
if it is a sub-alist.

There is one simple element with negative POSITION; that's intended
as a way for the user to ask to recalculate the buffer's index alist."
  (or (and imenu--index-alist
	   (or (not imenu-auto-rescan)
	       (and imenu-auto-rescan
		    (> (buffer-size)  imenu-auto-rescan-maxout))))
      ;; Get the index; truncate if necessary
      (progn
	(setq imenu--index-alist
	      (save-excursion
		(save-restriction
		  (widen)
		  (funcall imenu-create-index-function))))
	(imenu--truncate-items imenu--index-alist)))
  (or imenu--index-alist noerror
      (error "No items suitable for an index found in this buffer"))
  (or imenu--index-alist
      (setq imenu--index-alist (list nil)))
  ;; Add a rescan option to the index.
  (cons imenu--rescan-item imenu--index-alist))

;;; Find all markers in alist and makes
;;; them point nowhere.
;;; The top-level call uses nil as the argument;
;;; non-nil arguments are in recursivecalls.
(defvar imenu--cleanup-seen)

(defun imenu--cleanup (&optional alist)
  ;; If alist is provided use that list.
  ;; If not, empty the table of lists already seen
  ;; and use imenu--index-alist.
  (if alist
      (setq imenu--cleanup-seen (cons alist imenu--cleanup-seen))
    (setq alist imenu--index-alist imenu--cleanup-seen (list alist)))

  (and alist
       (mapc
	(lambda (item)
	  (cond
	   ((markerp (cdr item))
	    (set-marker (cdr item) nil))
	   ;; Don't process one alist twice.
	   ((memq (cdr item) imenu--cleanup-seen))
	   ((imenu--subalist-p item)
	    (imenu--cleanup (cdr item)))))
	alist)
       t))

(defun imenu--create-keymap-1 (title alist)
  (let ((counter 0))
    (list* 'keymap title
	   (mapcar
	    (lambda (item)
	      (list* (car item) (car item)
		     (cond
		      ((imenu--subalist-p item)
		       (imenu--create-keymap-1 (car item) (cdr item)))
		      (t
		       `(lambda () (interactive)
			  (imenu--menubar-select ',item))))))
	    alist))))

(defun imenu--in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res)
    (setq res nil)
    (while alist
      (setq elt (car alist)
	    tail (cdr elt)
	    alist (cdr alist)
	    head (car elt))
      ;; A nested ALIST element looks like
      ;;   (INDEX-NAME (INDEX-NAME . INDEX-POSITION) ...)
      ;; while a bottom-level element looks like
      ;;   (INDEX-NAME . INDEX-POSITION)
      ;; We are only interested in the bottom-level elements, so we need to
      ;; recurse if TAIL is a list.
      (cond ((listp tail)
	     (if (setq res (imenu--in-alist str tail))
		 (setq alist nil)))
	    ((if imenu-name-lookup-function
                 (funcall imenu-name-lookup-function str head)
               (string= str head))
	     (setq alist nil res elt))))
    res))

(defvar imenu-syntax-alist nil
  "Alist of syntax table modifiers to use while in `imenu--generic-function'.

The car of the assocs may be either a character or a string and the
cdr is a syntax description appropriate fo `modify-syntax-entry'.  For
a string, all the characters in the string get the specified syntax.

This is typically used to give word syntax to characters which
normally have symbol syntax to simplify `imenu-expression'
and speed-up matching.")
;;;###autoload
(make-variable-buffer-local 'imenu-syntax-alist)

(defun imenu-default-create-index-function ()
  "*Wrapper for index searching functions.

Moves point to end of buffer and then repeatedly calls
`imenu-prev-index-position-function' and `imenu-extract-index-name-function'.
Their results are gathered into an index alist."
  ;; These should really be done by setting imenu-create-index-function
  ;; in these major modes.  But save that change for later.
  (cond ((and imenu-prev-index-position-function
	      imenu-extract-index-name-function)
	 (let ((index-alist '())
	       prev-pos name)
	   (goto-char (point-max))
	   (imenu-progress-message prev-pos 0 t)
	   ;; Search for the function
	   (while (funcall imenu-prev-index-position-function)
	     (imenu-progress-message prev-pos nil t)
	     (save-excursion
	       (setq name (funcall imenu-extract-index-name-function)))
	     (and (stringp name)
 		  ;; [ydi] updated for imenu-use-markers
		  (push (cons name (if imenu-use-markers (point-marker) (point)))
			index-alist)))
	   (imenu-progress-message prev-pos 100 t)
	   index-alist))
	;; Use generic expression if possible.
	((and imenu-generic-expression)
	 (imenu--generic-function imenu-generic-expression))
	(t
	 (error "This buffer cannot use `imenu-default-create-index-function'"))))

;; Not used and would require cl at run time
;;; (defun imenu--flatten-index-alist (index-alist &optional concat-names prefix)
;;;   ;; Takes a nested INDEX-ALIST and returns a flat index alist.
;;;   ;; If optional CONCAT-NAMES is non-nil, then a nested index has its
;;;   ;; name and a space concatenated to the names of the children.
;;;   ;; Third argument PREFIX is for internal use only.
;;;   (mapcan
;;;    (lambda (item)
;;;      (let* ((name (car item))
;;; 	    (pos (cdr item))
;;; 	    (new-prefix (and concat-names
;;; 			     (if prefix
;;; 				 (concat prefix imenu-level-separator name)
;;; 			       name))))
;;;        (cond
;;; 	((or (markerp pos) (numberp pos))
;;; 	 (list (cons new-prefix pos)))
;;; 	(t
;;; 	 (imenu--flatten-index-alist pos new-prefix)))))
;;;    index-alist))

;;;
;;; Generic index gathering function.
;;;

(defvar imenu-case-fold-search t
  "Defines whether `imenu--generic-function' should fold case when matching.

This variable should be set (only) by initialization code
for modes which use `imenu--generic-function'.  If it is not set, that
function will use the current value of `case-fold-search' to match
patterns.")
;;;###autoload
(make-variable-buffer-local 'imenu-case-fold-search)

;; Originally "Built on some ideas that Erik Naggum <erik@naggum.no>
;; once posted to comp.emacs" but since substantially re-written.
(defun imenu--generic-function (patterns)
  "Return an index of the current buffer as an alist.

PATTERNS is an alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX).

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

See `lisp-imenu-generic-expression' for an example of PATTERNS.

Returns an index of the current buffer as an alist.  The elements in
the alist look like: (INDEX-NAME . INDEX-POSITION).  They may also be
nested index lists like (INDEX-NAME . INDEX-ALIST) depending on
PATTERNS."

  (let ((index-alist (list 'dummy))
	prev-pos beg
        (case-fold-search imenu-case-fold-search)
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    ;; Modify the syntax table used while matching regexps.
    (dolist (syn slist)
      ;; The character(s) to modify may be a single char or a string.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table)
	(dolist (c (car syn))
	  (modify-syntax-entry c (cdr syn) table))))
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0 t)
    (unwind-protect			; for syntax table
	(save-match-data
	  (set-syntax-table table)
	  ;; map over the elements of imenu-generic-expression
	  ;; (typically functions, variables ...)
	  (dolist (pat patterns)
	    (let ((menu-title (car pat))
		  (regexp (nth 1 pat))
		  (index (nth 2 pat))
		  (function (nth 3 pat))
		  (rest (nthcdr 4 pat)))
	      ;; Go backwards for convenience of adding items in order.
	      (goto-char (point-max))
	      (while (re-search-backward regexp nil t)
		(imenu-progress-message prev-pos nil t)
		(setq beg (match-beginning index))
		;; Add this sort of submenu only when we've found an
		;; item for it, avoiding empty, duff menus.
		(unless (assoc menu-title index-alist)
		  (push (list menu-title) index-alist))
		(if imenu-use-markers
		    (setq beg (copy-marker beg)))
		(let ((item
		       (if function
			   (nconc (list (match-string-no-properties index)
					beg function)
				  rest)
			 (cons (match-string-no-properties index)
			       beg)))
		      ;; This is the desired submenu,
		      ;; starting with its title (or nil).
		      (menu (assoc menu-title index-alist)))
		  ;; Insert the item unless it is already present.
		  (unless (member item (cdr menu))
		    (setcdr menu
			    (cons item (cdr menu))))))))
	  (set-syntax-table old-table)))
    (imenu-progress-message prev-pos 100 t)
    ;; Sort each submenu by position.
    ;; This is in case one submenu gets items from two different regexps.
    (dolist (item index-alist)
      (when (listp item)
	(setcdr item (sort (cdr item) 'imenu--sort-by-position))))
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist))
	     (cdr main-element)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main functions for this package!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See also info-lookup-find-item
(defun imenu-find-default (guess completions)
  "Fuzzily find an item based on GUESS inside the alist COMPLETIONS."
  (catch 'found
    (let ((case-fold-search t))
      (if (assoc guess completions) guess
	(dolist (re (list (concat "\\`" (regexp-quote guess) "\\'")
			  (concat "\\`" (regexp-quote guess))
			  (concat (regexp-quote guess) "\\'")
			  (regexp-quote guess)))
	  (dolist (x completions)
	    (if (string-match re (car x)) (throw 'found (car x)))))))))

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Returns t for rescan and otherwise a position number."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice
	(prepared-index-alist
	 (mapcar
	  (lambda (item)
	    (cons (subst-char-in-string ?\ (aref imenu-space-replacement 0)
					(car item))
		  (cdr item)))
	  index-alist)))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (cond (prompt)
	  ((and name (imenu--in-alist name prepared-index-alist))
	   (setq prompt (format "Index item (default %s): " name)))
	  (t (setq prompt "Index item: ")))
    (if (eq imenu-always-use-completion-buffer-p 'never)
  	(setq name (completing-read prompt
  				    prepared-index-alist
 				    nil t nil 'imenu--history-list name))
      (save-window-excursion
	;; Display the completion buffer
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list
	   (all-completions "" prepared-index-alist )))
	(let ((minibuffer-setup-hook
	       (function
		(lambda ()
		  (let ((buffer (current-buffer)))
		    (with-current-buffer "*Completions*"
		      (setq completion-reference-buffer buffer)))))))
	  ;; Make a completion question
	  (setq name (completing-read prompt
				      prepared-index-alist
				      nil t nil 'imenu--history-list name)))))
    (cond ((not (stringp name)) nil)
	  ((string= name (car imenu--rescan-item)) t)
	  (t
	   (setq choice (assoc name prepared-index-alist))
	   (if (imenu--subalist-p choice)
	       (imenu--completion-buffer (cdr choice) prompt)
	     choice)))))

(defun imenu--mouse-menu (index-alist event &optional title)
  "Let the user select from a buffer index from a mouse menu.

INDEX-ALIST is the buffer index and EVENT is a mouse event.

Returns t for rescan and otherwise an element or subelement of INDEX-ALIST."
  (setq index-alist (imenu--split-submenus index-alist))
  (let* ((menu (imenu--split-menu index-alist (or title (buffer-name))))
	 (map (imenu--create-keymap-1 (car menu)
				      (if (< 1 (length (cdr menu)))
					  (cdr menu)
					(cdr (car (cdr menu)))))))
    (popup-menu map event)))

(defun imenu-choose-buffer-index (&optional prompt alist)
  "Let the user select from a buffer index and return the chosen index.

If the user originally activated this function with the mouse, a mouse
menu is used.  Otherwise a completion buffer is used and the user is
prompted with PROMPT.

If you call this function with index alist ALIST, then it lets the user
select from ALIST.

With no index alist ALIST, it calls `imenu--make-index-alist' to
create the index alist.

If `imenu-always-use-completion-buffer-p' is non-nil, then the
completion buffer is always used, no matter if the mouse was used or
not.

The returned value is of the form (INDEX-NAME . INDEX-POSITION)."
  (let (index-alist
	(mouse-triggered (listp last-nonmenu-event))
	(result t))
    ;; If selected by mouse, see to that the window where the mouse is
    ;; really is selected.
    (and mouse-triggered
	 (not (equal last-nonmenu-event '(menu-bar)))
	 (let ((window (posn-window (event-start last-nonmenu-event))))
	   (or (framep window) (null window) (select-window window))))
    ;; Create a list for this buffer only when needed.
    (while (eq result t)
      (setq index-alist (if alist alist (imenu--make-index-alist)))
      (setq result
	    (if (and mouse-triggered
		     (not imenu-always-use-completion-buffer-p))
		(imenu--mouse-menu index-alist last-nonmenu-event)
	      (imenu--completion-buffer index-alist prompt)))
      (and (eq result t)
	   (imenu--cleanup)
	   (setq imenu--index-alist nil)))
    result))

;;;###autoload
(defun imenu-add-to-menubar (name)
  "Add an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information."
  (interactive "sImenu menu item name: ")
  (if (or (and imenu-prev-index-position-function
	       imenu-extract-index-name-function)
	  imenu-generic-expression
	  (not (eq imenu-create-index-function
		   'imenu-default-create-index-function)))
      (let ((newmap (make-sparse-keymap)))
	(set-keymap-parent newmap (current-local-map))
	(setq imenu--last-menubar-index-alist nil)
	(define-key newmap [menu-bar index]
	  `(menu-item ,name ,(make-sparse-keymap "Imenu")))
	(use-local-map newmap)
	(add-hook 'menu-bar-update-hook 'imenu-update-menubar))
    (error "The mode `%s' does not support Imenu" mode-name)))

;;;###autoload
(defun imenu-add-menubar-index ()
  "Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook."
  (interactive)
  (imenu-add-to-menubar "Index"))

(defvar imenu-buffer-menubar nil)

(defvar imenu-update-menubar-modified-tick 0
  "The value of (buffer-modified-tick) as of last call to `imenu-update-menubar'.
This value becomes local in every buffer when it is set.")
(make-variable-buffer-local 'imenu-update-menubar-modified-tick)

(defun imenu-update-menubar ()
  (and (current-local-map)
       (keymapp (lookup-key (current-local-map) [menu-bar index]))
       (not (eq (buffer-modified-tick)
		imenu-update-menubar-modified-tick))
       (let ((index-alist (imenu--make-index-alist t)))
	 ;; Don't bother updating if the index-alist has not changed
	 ;; since the last time we did it.
	 (or (equal index-alist imenu--last-menubar-index-alist)
	     (let (menu menu1 old)
	       (setq imenu--last-menubar-index-alist index-alist)
	       (setq index-alist (imenu--split-submenus index-alist))
	       (setq menu (imenu--split-menu index-alist
                                             (buffer-name)))
	       (setq menu1 (imenu--create-keymap-1 (car menu)
                                                   (if (< 1 (length (cdr menu)))
                                                       (cdr menu)
						     (cdr (car (cdr menu))))))
	       (setq imenu-update-menubar-modified-tick
		     (buffer-modified-tick))
	       (setq old (lookup-key (current-local-map) [menu-bar index]))
	       (setcdr old (cdr menu1)))))))

(defun imenu--menubar-select (item)
  "Use Imenu to select the function or variable named in this menu ITEM."
  (if (equal item imenu--rescan-item)
      (progn
	(imenu--cleanup)
	(setq imenu--index-alist nil)
	(imenu-update-menubar)
	t)
    (imenu item)
    nil))

(defun imenu-default-goto-function (name position &optional rest)
  "Move the point to the given position.

NAME is ignored.  POSITION is where to move.  REST is also ignored.
The ignored args just make this function have the same interface as a
function placed in a special index-item."
  (if (or (< position (point-min))
	  (> position (point-max)))
      ;; widen if outside narrowing
      (widen))
  (goto-char position))

;;;###autoload
(defun imenu (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information."
  (interactive (list (imenu-choose-buffer-index)))
  ;; Convert a string to an alist element.
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (when index-item
    (push-mark)
    (let* ((is-special-item (listp (cdr index-item)))
	   (function
	    (if is-special-item
		(nth 2 index-item) imenu-default-goto-function))
	   (position (if is-special-item
			 (cadr index-item) (cdr index-item)))
	   (rest (if is-special-item (cddr index-item))))
      (apply function (car index-item) position rest))
    (run-hooks 'imenu-after-jump-hook)))

(dolist (mess
	 '("^No items suitable for an index found in this buffer$"
	   "^This buffer cannot use `imenu-default-create-index-function'$"
	   "^The mode `.*' does not support Imenu$"))
  (add-to-list 'debug-ignored-errors mess))

(provide 'imenu)

;;; imenu.el ends here
