;;; easymenu.el --- support the easymenu interface for defining a menu.

;; Keywords: emulations

;; Copyright (C) 1994 Free Software Foundation, Inc.

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

;;;###autoload.
(defun easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The arguments SYMBOL and DOC are ignored; they are present for
compatibility only.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is a vector of three elements:

 - the name of the menu item (a string);
 - the `callback' of that item;
 - t.

If the `callback' of a menu item is a symbol, then it must name a
command.  It will be invoked with `call-interactively'.  If it is a
list, then it is evaluated with `eval'.

If an element of a menu is a string, then that string appears in the
menu as unselectable text.

If an element of a menu is a string consisting solely of hyphens, then that
item is displayed as a solid horizontal line.

If an element of a menu is a list, it is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above."
  (let ((keymap (easy-menu-keymap (car menu) (cdr menu))))
    (mapcar (function (lambda (map) 
	      (define-key map (vector 'menu-bar (intern (car menu)))
		(cons (car menu) keymap))))
	    (if (keymapp maps) (list maps) maps))))

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
	       (setq command (make-lucid-menu-keymap (car item) (cdr item)))
	       (setq name (car item)))
	      ((vectorp item)
	       (setq command (make-symbol (format "menu-function-%d"
						  add-menu-item-count)))
	       (setq enabler (make-symbol (format "menu-function-%d-enabler"
						  add-menu-item-count)))
	       (setq add-menu-item-count (1+ add-menu-item-count))
	       (put command 'menu-enable enabler)
	       (set enabler (aref item 2))
	       (setq name (aref item 0))	       
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

(provide 'easymenu)

;;; easymenu.el ends here
