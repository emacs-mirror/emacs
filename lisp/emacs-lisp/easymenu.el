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
The arguments SYMBOL and DOC are ignored; they are present for
compatibility only.  SYMBOL is not evaluated.  In other Emacs versions
these arguments may be used as a variable to hold the menu data, and a
doc string for that variable.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is a symbol; if its value is non-nil, the item is enabled
for selection.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above."
  (` (let* ((maps (, maps))
	    (menu (, menu))
	    (keymap (easy-menu-keymap (car menu) (cdr menu))))
       (and (keymapp maps) (setq maps (list maps)))
       (while maps
	 (define-key (car maps) (vector 'menu-bar (intern (car menu)))
	   (cons (car menu) keymap))
	 (setq maps (cdr maps))))))

(defvar easy-menu-item-count 0)

;; Return a menu keymap corresponding to a Lucid-style menu list
;; MENU-ITEMS, and with name MENU-NAME.
(defun easy-menu-keymap (menu-name menu-items)
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
	       (setq command (easy-menu-keymap (car item) (cdr item)))
	       (setq name (concat (car item) "...")))
	      ((vectorp item)
	       (setq command (make-symbol (format "menu-function-%d"
						  easy-menu-item-count)))
	       (setq easy-menu-item-count (1+ easy-menu-item-count))
	       (put command 'menu-enable (aref item 2))
	       (setq name (aref item 0))	       
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

(defmacro easy-menu-remove (menu))

(defmacro easy-menu-add (menu &optional map))

(provide 'easymenu)

;;; easymenu.el ends here
