;;; easymenu.el --- support the easymenu interface for defining a menu.

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Keywords: emulations
;; Author: rms

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This is compatible with easymenu.el by Per Abrahamsen
;;; but it is much simpler as it doesn't try to support other Emacs versions.
;;; The code was mostly derived from lmenu.el.

;;; Code:

;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The menu keymap is stored in symbol SYMBOL, both as its value
and as its function definition.   DOC is used as the doc string for SYMBOL.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbol defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.
This is normally not needed because keyboard equivalents are usually
computed automatically.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style 
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.  
        Currently just prepend the name with the string \"Toggle \".
radio: A radio button. 
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.
Currently just disable radio buttons, no effect on checkboxes.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above."
  (` (progn
       (defvar (, symbol) nil (, doc))
       (easy-menu-do-define (quote (, symbol)) (, maps) (, doc) (, menu)))))

;;;###autoload
(defun easy-menu-do-define (symbol maps doc menu)
  ;; We can't do anything that might differ between Emacs dialects in
  ;; `easy-menu-define' in order to make byte compiled files
  ;; compatible.  Therefore everything interesting is done in this
  ;; function. 
  (set symbol (easy-menu-create-keymaps (car menu) (cdr menu)))
  (fset symbol (` (lambda (event) (, doc) (interactive "@e")
		    (easy-popup-menu event (, symbol)))))
  (mapcar (function (lambda (map) 
	    (define-key map (vector 'menu-bar (intern (car menu)))
	      (cons (car menu) (symbol-value symbol)))))
	  (if (keymapp maps) (list maps) maps)))

(defvar easy-menu-item-count 0)

;; Return a menu keymap corresponding to a Lucid-style menu list
;; MENU-ITEMS, and with name MENU-NAME.
;;;###autoload
(defun easy-menu-create-keymaps (menu-name menu-items)
  (let ((menu (make-sparse-keymap menu-name)))
    ;; Process items in reverse order,
    ;; since the define-key loop reverses them again.
    (setq menu-items (reverse menu-items))
    (while menu-items
      (let* ((item (car menu-items))
	     (callback (if (vectorp item) (aref item 1)))
	     command enabler name)
	(cond ((stringp item)
	       (setq command nil)
	       (setq name (if (string-match "^-+$" item) "" item)))
	      ((consp item)
	       (setq command (easy-menu-create-keymaps (car item) (cdr item)))
	       (setq name (car item)))
	      ((vectorp item)
	       (setq command (make-symbol (format "menu-function-%d"
						  easy-menu-item-count)))
	       (setq easy-menu-item-count (1+ easy-menu-item-count))
	       (setq name (aref item 0))
	       (let ((keyword (aref item 2)))
		 (if (and (symbolp keyword)
			  (= ?: (aref (symbol-name keyword) 0)))
		     (let ((count 2)
			   style selected active keys
			   arg)
		       (while (> (length item) count)
			 (setq keyword (aref item count))
			 (setq arg (aref item (1+ count)))
			 (setq count (+ 2 count))
			 (cond ((eq keyword ':keys)
				(setq keys arg))
			       ((eq keyword ':active)
				(setq active arg))
			       ((eq keyword ':suffix)
				(setq name (concat name " " arg)))
			       ((eq keyword ':style)
				(setq style arg))
			       ((eq keyword ':selected)
				(setq selected arg))))
		       (if keys
			   (setq name (concat name "  (" keys ")")))
		       (if (eq style 'toggle)
			   ;; Simulate checkboxes.
			   (setq name (concat "Toggle " name)))
		       (if active 
			   (put command 'menu-enable active)
			 (and (eq style 'radio)
			      selected
			      ;; Simulate radio buttons with menu-enable.
			      (put command 'menu-enable
				   (list 'not selected)))))	       
		   (put command 'menu-enable keyword)))
	       (if (keymapp callback)
		   (setq name (concat name " ...")))
	       (if (symbolp callback)
		   (fset command callback)
		 (fset command (list 'lambda () '(interactive) callback)))))
	(if (null command)
	    ;; Handle inactive strings specially--allow any number
	    ;; of identical ones.
	    (setcdr menu (cons (list nil name) (cdr menu)))
	  (if name 
	      (define-key menu (vector (intern name)) (cons name command)))))
      (setq menu-items (cdr menu-items)))
    menu))

(defun easy-menu-change (path name items)
  "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu containing NAME in the
menu bar.  ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that map.

Call this from `activate-menubar-hook' to implement dynamic menus."
  (let ((map (key-binding (apply 'vector
				 'menu-bar
				 (mapcar 'intern (append path (list name)))))))
    (if (keymapp map)
	(setcdr map (cdr (easy-menu-create-keymaps name items)))
      (error "Malformed menu in `easy-menu-change'"))))

(defun easy-menu-remove (menu))

(defun easy-menu-add (menu &optional map))

(provide 'easymenu)

;;; easymenu.el ends here
