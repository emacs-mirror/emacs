;;; imenu.el --- Framework for mode-specific buffer indexes.

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Ake Stenhoff <etxaksf@aom.ericsson.se>
;;         Lars Lindberg <lli@sypro.cap.se>
;; Created: 8 Feb 1994
;; Version: 1.12
;; Keywords: tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
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

;;   There are *examples* for index gathering functions for C/C++ and
;;   Lisp/Emacs Lisp but it is easy to customize for other modes.  A
;;   function for jumping to the chosen index position is also
;;   supplied.

;;; Thanks goes to
;;  [simon] - Simon Leinen simon@lia.di.epfl.ch
;;  [dean] - Dean Andrews ada@unison.com
;;  [alon] - Alon Albert al@mercury.co.il 
;;  [greg] - Greg Thompson gregt@porsche.visix.COM
;;  [kai] - Kai Grossjohann grossjoh@linus.informatik.uni-dortmund.de

;;; Code
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar imenu-always-use-completion-buffer-p nil
  "*Set this to non-nil for displaying the index in a completion buffer.

Non-nil means always display the index in a completion buffer.
Nil means display the index as a mouse menu when the mouse was
used to invoke `imenu'.")

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
  "*Maximum number of elements in an index mouse-menu.")

(defvar imenu-scanning-message "Scanning buffer for index. (%3d%%)"
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

(defvar imenu-submenu-name-format "%s..."
  "*The format for making a submenu name.")

;;;; Hooks

(defvar imenu-create-index-function 'imenu-default-create-index-function
  "The function to use for creating a buffer index.

It should be a function that takes no arguments and returns an index
of the current buffer as an alist. The elements in the alist look
like: (INDEX-NAME . INDEX-POSITION). You may also nest index list like
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
index and it should return nil when it doesn't find another index. ")
(make-variable-buffer-local 'imenu-prev-index-position-function)

(defvar imenu-extract-index-name-function nil
  "Function for extracting the index name.

This function is called after the function pointed out by
`imenu-prev-index-position-function'.")
(make-variable-buffer-local 'imenu-extract-index-name-function)

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

;; History list for 'jump-to-function-in-buffer'.
;; Buffer local.
(defvar imenu--history-list nil)
(make-variable-buffer-local 'imenu--history-list)

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

;;;
;;; Function for suporting general looking submenu names.
;;; Uses `imenu-submenu-name-format' for creating the name.
;;; NAME is the base of the new submenu name.
;;;
(defun imenu-create-submenu-name (name)
  (format imenu-submenu-name-format name))

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

;;;
;;; Split a menu in to several menus.
;;;
(defun imenu--split-menu (menulist title)
  (cons "Function menus"
	(mapcar
	 (function
	  (lambda (menu)
	    (cons (format "(%s)" title) menu)))
	 (imenu--split menulist imenu-max-items))))

;;;
;;; Find all items in this buffer that should be in the index.
;;; Returns an alist on the form
;;; ((NAME . POSITION) (NAME . POSITION) ...)
;;;

(defun imenu--make-index-alist ()
  ;; Create a list for this buffer only when needed.
  (or imenu--index-alist
      ;; Get the index
      (setq imenu--index-alist
	    (save-excursion
	      (funcall imenu-create-index-function))))
  (or imenu--index-alist
      (error "No items suitable for an index found in this buffer."))
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
  (and imenu--index-alist
       (mapcar
	(function
	 (lambda (item)
	   (cond
	    ((markerp (cdr item))
	     (set-marker (cdr item) nil))
	    ((listp (cdr item))
	     (imenu--cleanup (cdr item))))))
	 (if alist alist imenu--index-alist))
	t))
(defun imenu-default-create-index-function ()
  "*Wrapper for index searching functions.

Moves point to end of buffer and then repeatedly calls
`imenu-prev-index-position-function' and `imenu-extract-index-name-function'.
Their results are gathered into an index alist."
  ;; These should really be done by setting imenu-create-index-function
  ;; in these major modes.  But save that change for later.
  (cond ((eq major-mode 'emacs-lisp-mode)
	 (imenu-example--create-lisp-index))
	((eq major-mode 'lisp-mode)
	 (imenu-example--create-lisp-index))
	((eq major-mode 'c++-mode)
	 (imenu-example--create-c++-index))
	((eq major-mode 'c-mode)
	 (imenu-example--create-c-index))
	(t
	 (or (and (fboundp imenu-prev-index-position-function)
		  (fboundp imenu-extract-index-name-function))
	     (error "The mode \"%s\" does not take full advantage of imenu.el yet."
		    mode-name))      
	 (let ((index-alist '())
	       name prev-pos)
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
	   index-alist))))

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
				    nil t nil 'imenu--history-list))))
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
    (setq position (x-popup-menu event menu))
    (cond
     ((eq position nil)
      position)
     ((listp position)
      (imenu--mouse-menu position event
			 (if title
			     (concat title imenu-level-separator
				     (car (rassq position index-alist)))
			   (car (rassq position index-alist)))))
     ((= position (cdr imenu--rescan-item))
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
	 (let ((window (posn-window (event-start last-nonmenu-event))))
	   (or (framep window) (select-window window))))
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

(defun imenu-add-to-menubar (name)
  "Adds an \"imenu\" entry to the menubar for the 
current local keymap.
NAME is the string naming the menu to be added.
See 'imenu' for more information."
  (interactive "sMenu name: ")
  (and window-system
       (define-key (current-local-map) [menu-bar index]
	 (cons name 'imenu))))

;;;###autoload
(defun imenu ()
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
See `imenu-choose-buffer-index' for more information."
  (interactive)
  (let ((index-item (save-restriction 
		      (widen)
		      (imenu-choose-buffer-index))))
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
	     (goto-char (cdr index-item))))))))

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
	 (push (cons (imenu-create-submenu-name "Variables") index-var-alist)
	       index-alist))
    (and index-type-alist
 	 (push (cons (imenu-create-submenu-name "Types") index-type-alist)
  	       index-alist))
    (and index-unknown-alist
	 (push (cons (imenu-create-submenu-name "Syntax-unknown") index-unknown-alist)
	       index-alist))
    index-alist))
    

;;;
;;; C
;;;
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

;;; 
;;; C++
;;; 
;; Regular expression to find C++ functions
(defvar imenu-example--function-name-regexp-c++
  (concat 
   "^[a-zA-Z0-9:]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_:~*]+[ \t]+\\)?"	; more than 3 tokens, right?
   "\\([a-zA-Z0-9_:~*]+[ \t]+\\)?"
   "\\([*&]+[ \t]*\\)?"			; pointer
   "\\([a-zA-Z0-9_:*]+\\)[ \t]*("	; name
   ))
(defun imenu-example--create-c++-index ()
  (imenu-example--create-c-index imenu-example--function-name-regexp-c++))

(provide 'imenu)

;;; imenu.el ends here
