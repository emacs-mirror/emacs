;;; imenu.el --- Framework for mode-specific buffer indexes.

;; Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Ake Stenhoff <etxaksf@aom.ericsson.se>
;;         Lars Lindberg <lli@sypro.cap.se>
;; Created: 8 Feb 1994
;; Keywords: tools

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

;;; Thanks goes to
;;  [simon] - Simon Leinen simon@lia.di.epfl.ch
;;  [dean] - Dean Andrews ada@unison.com
;;  [alon] - Alon Albert al@mercury.co.il 
;;  [greg] - Greg Thompson gregt@porsche.visix.COM
;;  [wolfgang] - Wolfgang Bangerth zcg51122@rpool1.rus.uni-stuttgart.de
;;  [kai] - Kai Grossjohann grossjoh@linus.informatik.uni-dortmund.de
;;  [david] - David M. Smith dsmith@stats.adelaide.edu.au
;;  [christian] - Christian Egli Christian.Egli@hcsd.hac.com
;;  [karl] - Karl Fogel kfogel@floss.life.uiuc.edu

;;; Code

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar imenu-auto-rescan nil
  "*Non-nil means Imenu should always rescan the buffers.")

(defvar imenu-auto-rescan-maxout 60000 
  "* auto-rescan is disabled in buffers larger than this.
This variable is buffer-local.")

(defvar imenu-always-use-completion-buffer-p nil
  "*Set this to non-nil for displaying the index in a completion buffer.

Non-nil means always display the index in a completion buffer.
Nil means display the index as a mouse menu when the mouse was
used to invoke `imenu'.
`never' means never automatically display a listing of any kind.")

(defvar imenu-sort-function nil
  "*The function to use for sorting the index mouse-menu.

Affects only the mouse index menu.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

Set it to `imenu--sort-by-name' if you want alphabetic sorting.

The function should take two arguments and return T if the first
element should come before the second.  The arguments are cons cells;
\(NAME . POSITION).  Look at `imenu--sort-by-name' for an example.")

(defvar imenu-max-items 25
  "*Maximum number of elements in an mouse menu for Imenu.")

(defvar imenu-scanning-message "Scanning buffer for index (%3d%%)"
  "*Progress message during the index scanning of the buffer.
If non-nil, user gets a message during the scanning of the buffer

Relevant only if the mode-specific function that creates the buffer
index use `imenu-progress-message'.")

(defvar imenu-space-replacement "^"
  "*The replacement string for spaces in index names.
Used when presenting the index in a completion-buffer to make the
names work as tokens.")

(defvar imenu-level-separator ":"
  "*The separator between index names of different levels.
Used for making mouse-menu titles and for flattening nested indexes
with name concatenation.")

;;;###autoload
(defvar imenu-generic-expression nil
  "The regex pattern to use for creating a buffer index.

If non-nil this pattern is passed to `imenu-create-index-with-pattern'
to create a buffer index.

It is an alist with elements that look like this: (MENU-TITLE
REGEXP INDEX). 

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

For emacs-lisp-mode for example PATTERN would look like:

'((nil \"^\\\\s-*(def\\\\(un\\\\|subst\\\\|macro\\\\|advice\\\\)\\\\s-+\\\\([-A-Za-z0-9+]+\\\\)\" 2)
  (\"*Vars*\" \"^\\\\s-*(def\\\\(var\\\\|const\\\\)\\\\s-+\\\\([-A-Za-z0-9+]+\\\\)\" 2)
  (\"*Types*\" \"^\\\\s-*(def\\\\(type\\\\|struct\\\\|class\\\\|ine-condition\\\\)\\\\s-+\\\\([-A-Za-z0-9+]+\\\\)\" 2))

The variable is buffer-local.")

;;;###autoload
(make-variable-buffer-local 'imenu-generic-expression)

;;;; Hooks

(defvar imenu-create-index-function 'imenu-default-create-index-function
  "The function to use for creating a buffer index.

It should be a function that takes no arguments and returns an index
of the current buffer as an alist.  The elements in the alist look
like: (INDEX-NAME . INDEX-POSITION).  You may also nest index list like
\(INDEX-NAME . INDEX-ALIST).

This function is called within a `save-excursion'.

The variable is buffer-local.")
(make-variable-buffer-local 'imenu-create-index-function)

(defvar imenu-prev-index-position-function 'beginning-of-defun
  "Function for finding the next index position.

If `imenu-create-index-function' is set to
`imenu-default-create-index-function', then you must set this variable
to a function that will find the next index, looking backwards in the
file.

The function should leave point at the place to be connected to the
index and it should return nil when it doesn't find another index.")
(make-variable-buffer-local 'imenu-prev-index-position-function)

(defvar imenu-extract-index-name-function nil
  "Function for extracting the index name.

This function is called after the function pointed out by
`imenu-prev-index-position-function'.")
(make-variable-buffer-local 'imenu-extract-index-name-function)

;;;
;;; Macro to display a progress message.
;;; RELPOS is the relative position to display.
;;; If RELPOS is nil, then the relative position in the buffer
;;; is calculated.
;;; PREVPOS is the variable in which we store the last position displayed.
(defmacro imenu-progress-message (prevpos &optional relpos reverse)
  (` (and
      imenu-scanning-message
      (let ((pos (, (if relpos
			relpos
		      (` (imenu--relative-position (, reverse)))))))
	(if (, (if relpos t
		 (` (> pos (+ 5 (, prevpos))))))
	    (progn
	      (message imenu-scanning-message pos)
	      (setq (, prevpos) pos)))))))


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
    (let ((beg (point))
	  (end (progn (forward-sexp) (point)))
	  (marker (make-marker)))
      (set-marker marker beg)
      (cons (buffer-substring beg end)
	    marker))))

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
(defvar imenu--index-alist nil)
(make-variable-buffer-local 'imenu--index-alist)

;; The latest buffer index used to update the menu bar menu.
(defvar imenu--last-menubar-index-alist nil)
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
;;; An item look like (NAME . POSITION).
;;;
(defun imenu--sort-by-name (item1 item2)
  (string-lessp (car item1) (car item2)))

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
  (if (> (length menulist) imenu-max-items)
      (let ((count 0))
	(cons title
	      (mapcar
	       (function
		(lambda (menu)
		  (cons (format "(%s-%d)" title (setq count (1+ count)))
			menu)))
	       (imenu--split menulist imenu-max-items))))
    (cons title menulist)))

;;; Split up each long alist that are nested within ALIST
;;; into nested alists.
(defun imenu--split-submenus (alist)
  (mapcar (function (lambda (elt)
		      (if (and (consp elt)
			       (stringp (car elt))
			       (listp (cdr elt)))
			  (imenu--split-menu (cdr elt) (car elt))
			elt)))
	  alist))

;;;
;;; Find all items in this buffer that should be in the index.
;;; Returns an alist on the form
;;; ((NAME . POSITION) (NAME . POSITION) ...)
;;;

(defun imenu--make-index-alist (&optional noerror)
  ;; Create a list for this buffer only when needed.
  (or (and imenu--index-alist
	   (or (not imenu-auto-rescan)
	       (and imenu-auto-rescan
		    (> (buffer-size)  imenu-auto-rescan-maxout))))
      ;; Get the index
      (setq imenu--index-alist
	    (save-excursion
	      (funcall imenu-create-index-function))))
  (or imenu--index-alist noerror
      (error "No items suitable for an index found in this buffer"))
  (or imenu--index-alist
      (setq imenu--index-alist (list nil)))
  ;; Add a rescan option to the index.
  (cons imenu--rescan-item imenu--index-alist))
;;;
;;; Find all markers in alist and makes
;;; them point nowhere.
;;;
(defun imenu--cleanup (&optional alist)
  ;; Sets the markers in imenu--index-alist 
  ;; point nowhere.
  ;; if alist is provided use that list.
  (or alist
      (setq alist imenu--index-alist))
  (and alist
       (mapcar
	(function
	 (lambda (item)
	   (cond
	    ((markerp (cdr item))
	     (set-marker (cdr item) nil))
	    ((consp (cdr item))
	     (imenu--cleanup (cdr item))))))
	alist)
       t))

(defun imenu--create-keymap-2 (alist counter &optional commands)
  (let ((map nil))
    (mapcar
     (function
      (lambda (item)
	(cond
	 ((listp (cdr item))
	  (append (list (setq counter (1+ counter))
			(car item) 'keymap (car item))
		  (imenu--create-keymap-2 (cdr item) (+ counter 10) commands)))
	 (t
	  (let ((end (if commands `(lambda () (interactive)
				     (imenu--menubar-select ',item))
		       (cons '(nil) t))))
	    (cons (car item)
		  (cons (car item) end))))
	 )))
     alist)))

;; If COMMANDS is non-nil, make a real keymap
;; with a real command used as the definition.
;; If it is nil, make something suitable for x-popup-menu.
(defun imenu--create-keymap-1 (title alist &optional commands)
  (append (list 'keymap title) (imenu--create-keymap-2 alist 0 commands)))


(defun imenu--in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res)
    (setq res nil)
    (while alist
      (setq elt (car alist) 
	    tail (cdr elt)
	    alist (cdr alist) 
	    head (car elt)) 
      (if (string= str head)
	  (setq alist nil res elt)
	(if (and (listp tail)
		 (setq res (imenu--in-alist str tail)))
	    (setq alist nil))))
    res))

(defun imenu-default-create-index-function ()
  "*Wrapper for index searching functions.

Moves point to end of buffer and then repeatedly calls
`imenu-prev-index-position-function' and `imenu-extract-index-name-function'.
Their results are gathered into an index alist."
  ;; These should really be done by setting imenu-create-index-function
  ;; in these major modes.  But save that change for later.
  (cond ((and (fboundp imenu-prev-index-position-function)
	      (fboundp imenu-extract-index-name-function))
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
		  (push (cons name (point)) index-alist)))
	   (imenu-progress-message prev-pos 100 t)
	   index-alist))
	;; Use generic expression if possible.
	((and imenu-generic-expression)
	 (imenu--generic-function imenu-generic-expression)) 
	(t
	 (error "The mode \"%s\" does not take full advantage of imenu.el yet."
		mode-name))))      

(defun imenu--replace-spaces (name replacement)
  ;; Replace all spaces in NAME with REPLACEMENT.
  ;; That second argument should be a string.
  (mapconcat
   (function
    (lambda (ch)
      (if (char-equal ch ?\ )
	  replacement
	(char-to-string ch))))
   name
   ""))

(defun imenu--flatten-index-alist (index-alist &optional concat-names prefix)
  ;; Takes a nested INDEX-ALIST and returns a flat index alist.
  ;; If optional CONCAT-NAMES is non-nil, then a nested index has its
  ;; name and a space concatenated to the names of the children.
  ;; Third argument PREFIX is for internal use only.
  (mapcan
   (function
    (lambda (item)
      (let* ((name (car item))
	     (pos (cdr item))
	     (new-prefix (and concat-names
			      (if prefix
				  (concat prefix imenu-level-separator name)
				name))))
	(cond
	 ((or (markerp pos) (numberp pos))
	  (list (cons new-prefix pos)))
	 (t
	  (imenu--flatten-index-alist pos new-prefix))))))
   index-alist))

;;;
;;; Generic index gathering function.
;;;

(defun imenu--generic-function (patterns)
;; Built on some ideas that Erik Naggum <erik@naggum.no> once posted
;; to comp.emacs
  "Return an index of the current buffer as an alist.

PATTERN is an alist with elements that look like this: (MENU-TITLE
REGEXP INDEX).

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

For emacs-lisp-mode for example PATTERN would look like:

'((nil \"^\\\\s-*(def\\\\(un\\\\|subst\\\\|macro\\\\|advice\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2)
  (\"*Vars*\" \"^\\\\s-*(def\\\\(var\\\\|const\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2)
  (\"*Types*\" \"^\\\\s-*(def\\\\(type\\\\|struct\\\\|class\\\\|ine-condition\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2))'

Returns an index of the current buffer as an alist.  The elements in
the alist look like: (INDEX-NAME . INDEX-POSITION).  They may also be
nested index lists like (INDEX-NAME . INDEX-ALIST) depending on
pattern.

\(imenu--generic-function PATTERN\)."

  (let ((index-alist (list 'dummy))
        (found nil)
	(global-regexp 
	 (concat "\\(" 
		 (mapconcat
		  (function (lambda (pattern) (identity (cadr pattern)))) 
		  patterns "\\)\\|\\(") 
		 "\\)"))
	prev-pos)

    (goto-char (point-max))
    (imenu-progress-message prev-pos 0 t)
    (save-match-data
      (while (re-search-backward global-regexp nil t)
	(imenu-progress-message prev-pos nil t)
        (setq found nil)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (mapcar 
	   (function 
	    (lambda (pat) 
	      (let ((menu-title (car pat))
		    (regexp (cadr pat))
		    (index (caddr pat)))
		    (if (and (not found) ; Only allow one entry;
			     (looking-at regexp))
			(let ((beg (match-beginning index))
			      (end (match-end index)))
			  (setq found t)
			  (push 
			   (cons (buffer-substring-no-properties beg end) beg)
			   (cdr 
			    (or (assoc menu-title index-alist)
				(car (push 
				      (cons menu-title '()) 
				      index-alist))))))))))
	   patterns))))
    (imenu-progress-message prev-pos 100 t)
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist)) main-element))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main functions for this package!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Returns t for rescan and otherwise a position number."
  ;; Create a list for this buffer only when needed.
  (let (name choice
	     (prepared-index-alist
	      (mapcar
	       (function
		(lambda (item)
		  (cons (imenu--replace-spaces (car item) imenu-space-replacement)
			(cdr item))))
	       index-alist)))
    (if (eq imenu-always-use-completion-buffer-p 'never)
  	(setq name (completing-read (or prompt "Index item: ")
  				    prepared-index-alist
 				    nil t nil 'imenu--history-list))
      (save-window-excursion
	;; Display the completion buffer
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list
	   (all-completions "" prepared-index-alist )))
	(let ((minibuffer-setup-hook
	       (function (lambda ()
			   (let ((buffer (current-buffer)))
			     (save-excursion
			       (set-buffer "*Completions*")
			       (setq completion-reference-buffer buffer)))))))
	  ;; Make a completion question
	  (setq name (completing-read (or prompt "Index item: ")
				      prepared-index-alist
				      nil t nil 'imenu--history-list)))))
    (cond ((not (stringp name))
	   nil)
	  ((string= name (car imenu--rescan-item))
	   t)
	  (t
	   (setq choice (assoc name prepared-index-alist))
	   (if (listp (cdr choice))
	       (imenu--completion-buffer (cdr choice) prompt)
	     choice)))))

(defun imenu--mouse-menu (index-alist event &optional title)
  "Let the user select from a buffer index from a mouse menu.

INDEX-ALIST is the buffer index and EVENT is a mouse event.

Returns t for rescan and otherwise a position number."
  (setq index-alist (imenu--split-submenus index-alist))
  (let* ((menu 	(imenu--split-menu
		 (if imenu-sort-function
		     (sort
		      (let ((res nil)
			    (oldlist index-alist))
			;; Copy list method from the cl package `copy-list'
			(while (consp oldlist) (push (pop oldlist) res))
			(prog1 (nreverse res) (setcdr res oldlist)))
		      imenu-sort-function)
		   index-alist)
		 (or title (buffer-name))))
	 position)
    (setq menu (imenu--create-keymap-1 (car menu) 
				       (if (< 1 (length (cdr menu)))
					   (cdr menu)
					 (cdr (cadr menu)))))
    (setq position (x-popup-menu event menu))
    (cond ((and (listp position)
		(numberp (car position))
		(stringp (nth (1- (length position)) position)))
	   (setq position (nth (1- (length position)) position)))
	  ((and (stringp (car position))
		(null (cdr position)))
	   (setq position (car position))))
    (cond ((eq position nil)
	   position)
	  ((listp position)
	   (imenu--mouse-menu position event
			      (if title
				  (concat title imenu-level-separator
					  (car (rassq position index-alist)))
				(car (rassq position index-alist)))))
	  ((stringp position)
	   (or (string= position (car imenu--rescan-item))
	       (imenu--in-alist position index-alist)))
	  ((or (= position (cdr imenu--rescan-item))
	       (and (stringp position)
		    (string= position (car imenu--rescan-item))))
	   t)
	  (t
	   (rassq position index-alist)))))

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

The returned value is on the form (INDEX-NAME . INDEX-POSITION)."
  (let (index-alist
	(mouse-triggered (listp last-nonmenu-event))
	(result t) )
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
  "Adds an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information."
  (interactive "sImenu menu item name: ")
  (let ((newmap (make-sparse-keymap))
	(menu-bar (lookup-key (current-local-map) [menu-bar])))
    (define-key newmap [menu-bar]
      (append (make-sparse-keymap) menu-bar))
    (define-key newmap [menu-bar index]
      (cons name (nconc (make-sparse-keymap "Imenu")
			(make-sparse-keymap))))
    (use-local-map (append newmap (current-local-map))))
  (add-hook 'menu-bar-update-hook 'imenu-update-menubar))

(defvar imenu-buffer-menubar nil)

(defun imenu-update-menubar ()
  (and (current-local-map)
       (keymapp (lookup-key (current-local-map) [menu-bar index]))
       (let ((index-alist (imenu--make-index-alist t)))
	 ;; Don't bother updating if the index-alist has not changed
	 ;; since the last time we did it.
	 (or (equal index-alist imenu--last-menubar-index-alist)
	     (let (menu menu1 old)
	       (setq imenu--last-menubar-index-alist index-alist)
	       (setq index-alist (imenu--split-submenus index-alist))
	       (setq menu (imenu--split-menu
			   (if imenu-sort-function
			       (sort
				(let ((res nil)
				      (oldlist index-alist))
				  ;; Copy list method from the cl package `copy-list'
				  (while (consp oldlist) (push (pop oldlist) res))
				  (prog1 (nreverse res) (setcdr res oldlist)))
				imenu-sort-function)
			     index-alist)
			   (buffer-name)))
	       (setq menu1 (imenu--create-keymap-1 (car menu) 
						   (if (< 1 (length (cdr menu)))
						       (cdr menu)
						     (cdr (car (cdr menu))))
						   t))
	       (setq old (lookup-key (current-local-map) [menu-bar index]))
	       (setcdr old (cdr menu1)))))))

(defun imenu--menubar-select (item)
  "Use Imenu to select the function or variable named in this menu item."
  (if (equal item '("*Rescan*" . -99))
      (progn
	(imenu--cleanup)
	(setq imenu--index-alist nil)
	(imenu-update-menubar))
    (imenu item)))

;;;###autoload
(defun imenu (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
See `imenu-choose-buffer-index' for more information."
  (interactive
   (list (save-restriction 
	   (widen)
	   (imenu-choose-buffer-index))))
  ;; Convert a string to an alist element.
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (and index-item
       (progn
	 (push-mark)
	 (cond
	  ((markerp (cdr index-item))
	   (if (or ( > (marker-position (cdr index-item)) (point-min))
		   ( < (marker-position (cdr index-item)) (point-max)))
	       ;; widen if outside narrowing
	       (widen))
	   (goto-char (marker-position (cdr index-item))))
	  (t
	   (if (or ( > (cdr index-item) (point-min))
		   ( < (cdr index-item) (point-max)))
	       ;; widen if outside narrowing
	       (widen))
	   (goto-char (cdr index-item)))))))

(provide 'imenu)

;;; imenu.el ends here
