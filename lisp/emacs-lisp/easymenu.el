;;; easymenu.el --- support the easymenu interface for defining a menu.

;; Copyright (C) 1994, 1996, 1998 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is compatible with easymenu.el by Per Abrahamsen
;; but it is much simpler as it doesn't try to support other Emacs versions.
;; The code was mostly derived from lmenu.el.

;;; Code:

;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The menu keymap is stored in symbol SYMBOL, both as its value
and as its function definition.   DOC is used as the doc string for SYMBOL.

The first element of MENU must be a string.  It is the menu bar item name.
It may be followed by the keyword argument pair
   :filter FUNCTION
FUNCTION is a function with one argument, the menu.  It returns the actual
menu displayed.

The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbols defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.
This is normally not needed because keyboard equivalents are usually
computed automatically.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style STYLE
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.
        Prepend the name with '(*) ' or '( ) ' depending on if selected or not.
radio: A radio button.
       Prepend the name with '[X] ' or '[ ] ' depending on if selected or not.
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item name in the top-level menu.  It may be followed by the :filter
FUNCTION keyword argument pair.  The rest of the submenu list are menu items,
as above."
  `(progn
     (defvar ,symbol nil ,doc)
     (easy-menu-do-define (quote ,symbol) ,maps ,doc ,menu)))

;;;###autoload
(defun easy-menu-do-define (symbol maps doc menu)
  ;; We can't do anything that might differ between Emacs dialects in
  ;; `easy-menu-define' in order to make byte compiled files
  ;; compatible.  Therefore everything interesting is done in this
  ;; function. 
  (set symbol (easy-menu-create-menu (car menu) (cdr menu)))
  (fset symbol (` (lambda (event) (, doc) (interactive "@e")
		    (x-popup-menu event (, symbol)))))
  (mapcar (function (lambda (map) 
	    (define-key map (vector 'menu-bar (intern (car menu)))
	      (cons (car menu) (symbol-value symbol)))))
	  (if (keymapp maps) (list maps) maps)))

(defun easy-menu-filter-return (menu)
 "Convert MENU to the right thing to return from a menu filter.
MENU is a menu as computed by `easy-menu-define' or `easy-menu-create-menu' or
a symbol whose value is such a menu.
In Emacs a menu filter must return a menu (a keymap), in XEmacs a filter must
return a menu items list (without menu name and keywords). This function
returns the right thing in the two cases."
 (easy-menu-get-map menu nil))		; Get past indirections.

;;;###autoload
(defun easy-menu-create-menu (menu-name menu-items)
  "Create a menu called MENU-NAME with items described in MENU-ITEMS.
MENU-NAME is a string, the name of the menu.  MENU-ITEMS is a list of items
possibly preceded by keyword pairs as described in `easy-menu-define'."
  (let ((menu (make-sparse-keymap menu-name))
	keyword filter have-buttons)
    ;; Look for keywords.
    (while (and menu-items (cdr menu-items)
		(symbolp (setq keyword (car menu-items)))
		(= ?: (aref (symbol-name keyword) 0)))
      (if (eq keyword ':filter) (setq filter (cadr menu-items)))
      (setq menu-items (cddr menu-items)))
    (while menu-items
      (setq have-buttons
	    (easy-menu-do-add-item menu (car menu-items) have-buttons))
      (setq menu-items (cdr menu-items)))
    (when filter
      (setq menu (easy-menu-make-symbol menu))
      (put menu 'menu-enable
	   `(easy-menu-filter (quote ,menu) (quote ,filter))))
    menu))


;; Button prefixes.
(defvar easy-menu-button-prefix
  '((radio ?* . "( ) ") (toggle ?X . "[ ] ")))

(defun easy-menu-do-add-item (menu item have-buttons &optional before top)
  ;; Parse an item description and add the item to a keymap.  This is
  ;; the function that is used for item definition by the other easy-menu
  ;; functions.
  ;; MENU is a sparse keymap i.e. a list starting with the symbol `keymap'.
  ;; ITEM defines an item as in `easy-menu-define'.
  ;; HAVE-BUTTONS is a string or nil.  If not nil, use as item prefix for
  ;; items that are not toggle or radio buttons to compensate for the
  ;; button prefix.
  ;; Optional argument BEFORE is nil or a symbol used as a key in MENU. If
  ;; BEFORE is not nil put item before BEFORE in MENU, otherwise if item is
  ;; already present in MENU, just change it, otherwise put it last in MENU.
  ;; If optional TOP is true, this is an item in the menu bar itself so
  ;; don't use prefix.  In this case HAVE-BUTTONS will be nil.
  (let (command name item-string is-button done inserted)
    (cond
     ((stringp item)
      (setq item-string
	    (if (string-match	; If an XEmacs separator
		 "^\\(-+\\|\
--:\\(\\(no\\|\\(sing\\|doub\\)le\\(Dashed\\)?\\)Line\\|\
shadow\\(Double\\)?Etched\\(In\\|Out\\)\\(Dash\\)?\\)\\)$"
		 item) ""		; use a single line separator.
	      (concat have-buttons item))))
     ((consp item)
      (setq name (setq item-string (car item)))
      (setq command (if (keymapp (setq item (cdr item))) item
		      (easy-menu-create-menu name item))))
     ((vectorp item)
      (setq name (setq item-string (aref item 0)))
      (setq command (easy-menu-make-symbol (aref item 1) t))
      (let ((active (if (> (length item) 2) (aref item 2) t))
	    (active-specified (> (length item) 2))
	    (count 2)
	    style selected)
	(if (and (symbolp active) (= ?: (aref (symbol-name active) 0)))
	    (let ((count 2) keyword arg suffix keys)
	      (setq active-specified nil)
	      (while (> (length item) count)
		(setq keyword (aref item count))
		(setq arg (aref item (1+ count)))
		(setq count (+ 2 count))
		(cond
		 ((eq keyword ':keys) (setq keys arg))
		 ((eq keyword ':active) (setq active arg active-specified t))
		 ((eq keyword ':suffix) (setq suffix (concat " " arg)))
		 ((eq keyword ':style) (setq style arg))
		 ((eq keyword ':selected) (setq selected arg))))
	      (if keys (setq suffix (concat suffix "  (" keys ")")))
	      (if suffix (setq item-string (concat item-string " " suffix)))
	      (when (and selected
			 (setq style (assq style easy-menu-button-prefix)))
		;; Simulate checkboxes and radio buttons.
		(setq item-string (concat (cddr style) item-string))
		(put command 'menu-enable
		     `(easy-menu-update-button ,item-string
					       ,(cadr style)
					       ,selected
					       ,(or active t)))
		(setq is-button t)
		(setq active-specified nil)	; Already taken care of active.
		(when (not (or have-buttons top))
		  (setq have-buttons "    ")
		  ;; Add prefix to menu items defined so far.
		  (easy-menu-change-prefix menu t))))
	  (and (null active) active-specified
	       (setq active ''nil)))
	(if active-specified (put command 'menu-enable active))))
     (t "Invalid menu item in easymenu"))
    (when name
      (and (not is-button) have-buttons
	   (setq item-string (concat have-buttons item-string)))
      (setq name (intern name)))
    (setq item (cons item-string command))
    (if before (setq before (intern before)))
    ;; The following loop is simlar to `define-key-after'. It
    ;; inserts (name . item) in keymap menu.
    ;; If name is not nil then delete any duplications.
    ;; If before is not nil, insert before before. Otherwise
    ;; if name is not nil and it is found in menu, insert there, else
    ;; insert at end.
    (while (not done)
      (cond
       ((or (setq done (or (null (cdr menu)) (keymapp (cdr menu))))
	    (and before (eq (car-safe (cadr menu)) before)))
	;; If name is nil, stop here, otherwise keep going past the
	;; inserted element so we can delete any duplications that come
	;; later.
	(if (null name) (setq done t))
	(unless inserted		; Don't insert more than once.
	  (setcdr menu (cons (cons name item) (cdr menu)))
	  (setq inserted t)
	  (setq menu (cdr menu))))
       ((and name (eq (car-safe (cadr menu)) name))
	(if (and before			; Wanted elsewere and
		 (not (setq done	; not the last in this keymap.
			    (or (null (cddr menu)) (keymapp (cddr menu))))))
	      (setcdr menu (cddr menu))
	  (setcdr (cadr menu) item) ; Change item.
	  (setq inserted t))))
      (setq menu (cdr menu)))
    have-buttons))

(defvar easy-menu-item-count 0)

(defun easy-menu-make-symbol (callback &optional call)
  ;; Return a unique symbol with CALLBACK as function value.
  ;; If CALL is false then this is a keymap, not a function.
  ;; Else if CALLBACK is a symbol, avoid the indirection when looking for
  ;; key-bindings in menu.
  ;; Else make a lambda expression of CALLBACK.
  (let ((command
	 (make-symbol (format "menu-function-%d" easy-menu-item-count))))
    (setq easy-menu-item-count (1+ easy-menu-item-count))
    (fset command
	  (cond
	   ((not call) callback)
	   ((symbolp callback)
	    ;; Try find key-bindings for callback instead of for command
	    (put command 'menu-alias t) ; when displaying menu.
	    callback)
	   (t `(lambda () (interactive) ,callback))))
    command))

(defun easy-menu-filter (name filter)
  "Used as menu-enable property to filter menus.
A call to this function is used as the menu-enable property for a menu with
a filter function.
NAME is a symbol with a keymap as function value.  Call the function FILTER
with this keymap as argument.  FILTER must return a keymap which becomes the
new function value for NAME.  Use `easy-menu-filter-return' to return the
correct value in a way portable to XEmacs. If the new keymap is `eq' the old,
then the menu is not updated."
  (let* ((old (symbol-function name))
	 (new (funcall filter old)))
    (or (eq old new)			; No change
	(and (fset name new)
	     ;; Make sure the menu gets updated by returning a
	     ;; different value than last time to cheat the cache. 
	     (random)))))

(defun easy-menu-update-button (item ch selected active)
  "Used as menu-enable property to update buttons.
A call to this function is used as the menu-enable property for buttons.
ITEM is the item-string into which CH or ` ' is inserted depending on if
SELECTED is true or not.  The menu entry in enabled iff ACTIVE is true."
  (let ((new (if selected ch ? ))
	(old (aref item 1)))
    (if (eq new old)
	;; No change, just use the active value.
	active
      ;; It has changed.  Update the entry.
      (aset item 1 new)
      ;; If the entry is active, make sure the menu gets updated by
      ;; returning a different value than last time to cheat the cache. 
      (and active
	   (random)))))

(defun easy-menu-change (path name items &optional before)
  "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu containing NAME in the
menu bar.  ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that map.
If NAME is not present in the menu located by PATH, then add item NAME to
that menu. If the optional argument BEFORE is present add NAME in menu
just before BEFORE, otherwise add at end of menu.

Either call this from `menu-bar-update-hook' or use a menu filter,
to implement dynamic menus."
  (easy-menu-add-item nil path (cons name items) before))

;; XEmacs needs the following two functions to add and remove menus.
;; In Emacs this is done automatically when switching keymaps, so
;; here these functions are noops.
(defun easy-menu-remove (menu))

(defun easy-menu-add (menu &optional map))

(defun easy-menu-add-item (menu path item &optional before)
  "At the end of the submenu of MENU with path PATH add ITEM.
If ITEM is already present in this submenu, then this item will be changed.
otherwise ITEM will be added at the end of the submenu, unless the optional
argument BEFORE is present, in which case ITEM will instead be added
before the item named BEFORE.
MENU is either a symbol, which have earlier been used as the first
argument in a call to `easy-menu-define', or the value of such a symbol
i.e. a menu, or nil which stands for the menu-bar itself.
PATH is a list of strings for locating the submenu where ITEM is to be
added.  If PATH is nil, MENU itself is used.  Otherwise, the first
element should be the name of a submenu directly under MENU.  This
submenu is then traversed recursively with the remaining elements of PATH.
ITEM is either defined as in `easy-menu-define' or a menu defined earlier
by `easy-menu-define' or `easy-menu-create-menu'."
  (let ((top (not (or menu path))))
    (setq menu (easy-menu-get-map menu path))
    (if (or (keymapp item)
	    (and (symbolp item) (keymapp (symbol-value item))))
	;; Item is a keymap, find the prompt string and use as item name.
	(let ((tail (easy-menu-get-map item nil)) name)
	  (if (not (keymapp item)) (setq item tail))
	  (while (and (null name) (consp (setq tail (cdr tail)))
		      (not (keymapp tail)))
	    (if (stringp (car tail)) (setq name (car tail)) ; Got a name.
	      (setq tail (cdr tail))))
	  (setq item (cons name item))))
    (easy-menu-do-add-item menu item
			   (and (not top) (easy-menu-have-button menu)
				"    ")
			   before top)))

(defun easy-menu-item-present-p (menu path name)
  "In submenu of MENU with path PATH, return true iff item NAME is present.
MENU and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be looked for."
  (lookup-key (easy-menu-get-map menu path) (vector (intern name))))

(defun easy-menu-remove-item (menu path name)
  "From submenu of MENU with path PATH remove item NAME.
MENU and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be removed."
  (let ((item (vector (intern name)))
	(top (not (or menu path)))
	tmp)
    (setq menu (easy-menu-get-map menu path))
    (when (setq tmp (lookup-key menu item))
      (define-key menu item nil)
      (and (not top)
	   (easy-menu-is-button tmp)	; Removed item was a button and
	   (not (easy-menu-have-button menu)) ; no buttons left then
	   ;; remove prefix from items in menu
	   (easy-menu-change-prefix menu nil)))))

(defun easy-menu-get-map (menu path)
  ;; Return a sparse keymap in which to add or remove an item.
  ;; MENU and PATH are as defined in `easy-menu-remove-item'.
  (if (null menu)
      (setq menu (key-binding (vconcat '(menu-bar) (mapcar 'intern path))))
    (if (and (symbolp menu) (not (keymapp menu)))
	(setq menu (symbol-value menu)))
    (if path (setq menu (lookup-key menu (vconcat (mapcar 'intern path))))))
  (while (and (symbolp menu) (keymapp menu))
    (setq menu (symbol-function menu)))
  (or (keymapp menu) (error "Malformed menu in easy-menu: (%s)" menu))
  menu)

(defun easy-menu-is-button (val)
  ;; VAL is a real menu binding.  Return true iff it is a toggle or
  ;; radio button.
  (and (symbolp val)
       (consp (setq val (get val 'menu-enable)))
       (eq (car val) 'easy-menu-update-button)))

(defun easy-menu-have-button (map)
  ;; MAP is a sparse keymap.  Return true iff there is any toggle or radio
  ;; button in MAP.
  (let ((have nil) tmp)
    (while (and (consp map) (not have))
      (and (consp (setq tmp (car map)))
	   (consp (setq tmp (cdr tmp)))
	   (stringp (car tmp))
	   (setq have (easy-menu-is-button (easy-menu-real-binding tmp))))
      (setq map (cdr map)))
    have))

(defun easy-menu-real-binding (val)
  ;; Val is a menu keymap binding.  Skip item string.
  ;; Also skip a possible help string and/or key-binding cache.
  (if (and (consp (setq val (cdr val))) (stringp (car val)))
      (setq val (cdr val)))		; Skip help string.
  (if (and (consp val) (consp (car val))
	   (or (null (caar val)) (vectorp (caar val))))
      (setq val (cdr val)))		; Skip key-binding cache.
  val)

(defun easy-menu-change-prefix (map add)
  ;; MAP is a sparse keymap.
  ;; If ADD is true add a button compensating prefix to each menu item in MAP.
  ;; Else remove prefix instead.
  (let (tmp val)
    (while (consp map)
      (when (and (consp (setq tmp (car map)))
		 (consp (setq tmp (cdr tmp)))
		 (stringp (car tmp)))
	(cond
	 (add (setcar tmp (concat "    " (car tmp))))
	 ((string-match "$    " (car tmp))
	  (setcar tmp (substring (car tmp) (match-end 0))))))
      (setq map (cdr map)))))

(provide 'easymenu)

;;; easymenu.el ends here
