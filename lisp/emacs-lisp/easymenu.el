;;; easymenu.el --- support the easymenu interface for defining a menu

;; Copyright (C) 1994, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.

;; Keywords: emulations
;; Author: Richard Stallman <rms@gnu.org>

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

(defcustom easy-menu-precalculate-equivalent-keybindings t
  "Determine when equivalent key bindings are computed for easy-menu menus.
It can take some time to calculate the equivalent key bindings that are shown
in a menu.  If the variable is on, then this calculation gives a (maybe
noticeable) delay when a mode is first entered.  If the variable is off, then
this delay will come when a menu is displayed the first time.  If you never use
menus, turn this variable off, otherwise it is probably better to keep it on."
  :type 'boolean
  :group 'menu
  :version "20.3")

(defsubst easy-menu-intern (s)
  (if (stringp s) (intern (downcase s)) s))

;;;###autoload
(put 'easy-menu-define 'lisp-indent-function 'defun)
;;;###autoload
(defmacro easy-menu-define (symbol maps doc menu)
  "Define a menu bar submenu in maps MAPS, according to MENU.
The menu keymap is stored in symbol SYMBOL, both as its value
and as its function definition.   DOC is used as the doc string for SYMBOL.

The first element of MENU must be a string.  It is the menu bar item name.
It may be followed by the following keyword argument pairs

   :filter FUNCTION

FUNCTION is a function with one argument, the menu.  It returns the actual
menu displayed.

   :visible INCLUDE

INCLUDE is an expression; this menu is only visible if this
expression has a non-nil value.  `:include' is an alias for `:visible'.

   :active ENABLE

ENABLE is an expression; the menu is enabled for selection
whenever this expression's value is non-nil.

The rest of the elements in MENU, are menu items.

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
KEYS is expanded with `substitute-command-keys' before it is used.

   :key-sequence KEYS

KEYS is nil, a string or a vector; nil or a keyboard equivalent to this
menu item.
This is a hint that will considerably speed up Emacs' first display of
a menu.  Use `:key-sequence nil' when you know that this menu item has no
keyboard equivalent.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :included INCLUDE

INCLUDE is an expression; this item is only visible if this
expression has a non-nil value.

   :suffix FORM

FORM is an expression that will be dynamically evaluated and whose
value will be concatenated to the menu entry's NAME.

   :style STYLE

STYLE is a symbol describing the type of menu item.  The following are
defined:

toggle: A checkbox.
        Prepend the name with `(*) ' or `( ) ' depending on if selected or not.
radio: A radio button.
       Prepend the name with `[X] ' or `[ ] ' depending on if selected or not.
button: Surround the name with `[' and `]'.  Use this for an item in the
        menu bar itself.
anything else means an ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.

   :help HELP

HELP is a string, the help to display for the menu item.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list with the same format as MENU.  This is a submenu."
  `(progn
     (defvar ,symbol nil ,doc)
     (easy-menu-do-define (quote ,symbol) ,maps ,doc ,menu)))

;;;###autoload
(defun easy-menu-do-define (symbol maps doc menu)
  ;; We can't do anything that might differ between Emacs dialects in
  ;; `easy-menu-define' in order to make byte compiled files
  ;; compatible.  Therefore everything interesting is done in this
  ;; function.
  (let ((keymap (easy-menu-create-menu (car menu) (cdr menu))))
    (set symbol keymap)
    (fset symbol
	  `(lambda (event) ,doc (interactive "@e")
	     ;; FIXME: XEmacs uses popup-menu which calls the binding
	     ;; while x-popup-menu only returns the selection.
	     (x-popup-menu event
			   (or (and (symbolp ,symbol)
				    (funcall
				     (or (plist-get (get ,symbol 'menu-prop)
						    :filter)
					 'identity)
				     (symbol-function ,symbol)))
			       ,symbol))))
    (mapcar (lambda (map)
	      (define-key map (vector 'menu-bar (easy-menu-intern (car menu)))
		(cons 'menu-item
		      (cons (car menu)
			    (if (not (symbolp keymap))
				(list keymap)
			      (cons (symbol-function keymap)
				    (get keymap 'menu-prop)))))))
	    (if (keymapp maps) (list maps) maps))))

(defun easy-menu-filter-return (menu &optional name)
 "Convert MENU to the right thing to return from a menu filter.
MENU is a menu as computed by `easy-menu-define' or `easy-menu-create-menu' or
a symbol whose value is such a menu.
In Emacs a menu filter must return a menu (a keymap), in XEmacs a filter must
return a menu items list (without menu name and keywords).
This function returns the right thing in the two cases.
If NAME is provided, it is used for the keymap."
 (when (and (not (keymapp menu)) (consp menu))
   ;; If it's a cons but not a keymap, then it can't be right
   ;; unless it's an XEmacs menu.
   (setq menu (easy-menu-create-menu (or name "") menu)))
 (easy-menu-get-map menu nil))		; Get past indirections.

;;;###autoload
(defun easy-menu-create-menu (menu-name menu-items)
  "Create a menu called MENU-NAME with items described in MENU-ITEMS.
MENU-NAME is a string, the name of the menu.  MENU-ITEMS is a list of items
possibly preceded by keyword pairs as described in `easy-menu-define'."
  (let ((menu (make-sparse-keymap menu-name))
	prop keyword arg label enable filter visible help)
    ;; Look for keywords.
    (while (and menu-items
		(cdr menu-items)
		(keywordp (setq keyword (car menu-items))))
      (setq arg (cadr menu-items))
      (setq menu-items (cddr menu-items))
      (cond
       ((eq keyword :filter)
	(setq filter `(lambda (menu)
			(easy-menu-filter-return (,arg menu) ,menu-name))))
       ((eq keyword :active) (setq enable (or arg ''nil)))
       ((eq keyword :label) (setq label arg))
       ((eq keyword :help) (setq help arg))
       ((or (eq keyword :included) (eq keyword :visible))
	(setq visible (or arg ''nil)))))
    (if (equal visible ''nil)
	nil				; Invisible menu entry, return nil.
      (if (and visible (not (easy-menu-always-true visible)))
	  (setq prop (cons :visible (cons visible prop))))
      (if (and enable (not (easy-menu-always-true enable)))
	  (setq prop (cons :enable (cons enable prop))))
      (if filter (setq prop (cons :filter (cons filter prop))))
      (if help (setq prop (cons :help (cons help prop))))
      (if label (setq prop (cons nil (cons label prop))))
      (if filter
	  ;; The filter expects the menu in its XEmacs form and the pre-filter
	  ;; form will only be passed to the filter anyway, so we'd better
	  ;; not convert it at all (it will be converted on the fly by
	  ;; easy-menu-filter-return).
	  (setq menu menu-items)
	(setq menu (append menu (mapcar 'easy-menu-convert-item menu-items))))
      (when prop
	(setq menu (easy-menu-make-symbol menu 'noexp))
	(put menu 'menu-prop prop))
      menu)))


;; Known button types.
(defvar easy-menu-button-prefix
  '((radio . :radio) (toggle . :toggle)))

(defun easy-menu-do-add-item (menu item &optional before)
  (setq item (easy-menu-convert-item item))
  (easy-menu-define-key menu (easy-menu-intern (car item)) (cdr item) before))

(defvar easy-menu-converted-items-table (make-hash-table :test 'equal))

(defun easy-menu-convert-item (item)
  "Memoize the value returned by `easy-menu-convert-item-1' called on ITEM.
This makes key-shortcut-caching work a *lot* better when this
conversion is done from within a filter.
This also helps when the NAME of the entry is recreated each time:
since the menu is built and traversed separately, the lookup
would always fail because the key is `equal' but not `eq'."
  (or (gethash item easy-menu-converted-items-table)
      (puthash item (easy-menu-convert-item-1 item)
	       easy-menu-converted-items-table)))

(defun easy-menu-convert-item-1 (item)
  "Parse an item description and add the item to a keymap.
This is the function that is used for item definition by the other easy-menu
functions.
MENU is a sparse keymap i.e. a list starting with the symbol `keymap'.
ITEM defines an item as in `easy-menu-define'.
Optional argument BEFORE is nil or a key in MENU.  If BEFORE is not nil
put item before BEFORE in MENU, otherwise if item is already present in
MENU, just change it, otherwise put it last in MENU."
  (let (name command label prop remove help)
    (cond
     ((stringp item)			; An item or separator.
      (setq label item))
     ((consp item)			; A sub-menu
      (setq label (setq name (car item)))
      (setq command (cdr item))
      (if (not (keymapp command))
	  (setq command (easy-menu-create-menu name command)))
      (if (null command)
	  ;; Invisible menu item. Don't insert into keymap.
	  (setq remove t)
	(when (and (symbolp command) (setq prop (get command 'menu-prop)))
	  (when (null (car prop))
	    (setq label (cadr prop))
	    (setq prop (cddr prop)))
	  (setq command (symbol-function command)))))
     ((vectorp item)			; An item.
      (let* ((ilen (length item))
	     (active (if (> ilen 2) (or (aref item 2) ''nil) t))
	     (no-name (not (symbolp (setq command (aref item 1)))))
	     cache cache-specified)
	(setq label (setq name (aref item 0)))
	(if no-name (setq command (easy-menu-make-symbol command)))
	(if (keywordp active)
	    (let ((count 2)
		  keyword arg suffix visible style selected keys)
	      (setq active nil)
	      (while (> ilen count)
		(setq keyword (aref item count))
		(setq arg (aref item (1+ count)))
		(setq count (+ 2 count))
		(cond
		 ((or (eq keyword :included) (eq keyword :visible))
		  (setq visible (or arg ''nil)))
		 ((eq keyword :key-sequence)
		  (setq cache arg cache-specified t))
		 ((eq keyword :keys) (setq keys arg no-name nil))
		 ((eq keyword :label) (setq label arg))
		 ((eq keyword :active) (setq active (or arg ''nil)))
		 ((eq keyword :help) (setq prop (cons :help (cons arg prop))))
		 ((eq keyword :suffix) (setq suffix arg))
		 ((eq keyword :style) (setq style arg))
		 ((eq keyword :selected) (setq selected (or arg ''nil)))))
	      (if suffix
		  (setq label
			(if (stringp suffix)
			    (if (stringp label) (concat label " " suffix)
			      (list 'concat label (concat " " suffix)))
			  (if (stringp label)
			      (list 'concat (concat label " ") suffix)
			    (list 'concat label " " suffix)))))
	      (cond
	       ((eq style 'button)
		(setq label (if (stringp label) (concat "[" label "]")
			      (list 'concat "[" label "]"))))
	       ((and selected
		     (setq style (assq style easy-menu-button-prefix)))
		(setq prop (cons :button
				 (cons (cons (cdr style) selected) prop)))))
	      (when (stringp keys)
		 (if (string-match "^[^\\]*\\(\\\\\\[\\([^]]+\\)]\\)[^\\]*$"
				   keys)
		     (let ((prefix
			    (if (< (match-beginning 0) (match-beginning 1))
				(substring keys 0 (match-beginning 1))))
			   (postfix
			    (if (< (match-end 1) (match-end 0))
				(substring keys (match-end 1))))
			   (cmd (intern (substring keys (match-beginning 2)
						   (match-end 2)))))
		       (setq keys (and (or prefix postfix)
				       (cons prefix postfix)))
		       (setq keys
			     (and (or keys (not (eq command cmd)))
				  (cons cmd keys))))
		   (setq cache-specified nil))
		 (if keys (setq prop (cons :keys (cons keys prop)))))
	      (if (and visible (not (easy-menu-always-true visible)))
		  (if (equal visible ''nil)
		      ;; Invisible menu item. Don't insert into keymap.
		      (setq remove t)
		    (setq prop (cons :visible (cons visible prop)))))))
	(if (and active (not (easy-menu-always-true active)))
	    (setq prop (cons :enable (cons active prop))))
	(if (and (or no-name cache-specified)
		 (or (null cache) (stringp cache) (vectorp cache)))
	    (setq prop (cons :key-sequence (cons cache prop))))))
     (t (error "Invalid menu item in easymenu")))
    ;; `intern' the name so as to merge multiple entries with the same name.
    ;; It also makes it easier/possible to lookup/change menu bindings
    ;; via keymap functions.
    (cons (easy-menu-intern name)
	  (and (not remove)
	       (cons 'menu-item
		     (cons label
			   (and name
				(cons command prop))))))))

(defun easy-menu-define-key (menu key item &optional before)
  "Add binding in MENU for KEY => ITEM.  Similar to `define-key-after'.
If KEY is not nil then delete any duplications.
If ITEM is nil, then delete the definition of KEY.

Optional argument BEFORE is nil or a key in MENU.  If BEFORE is not nil,
put binding before the item in MENU named BEFORE; otherwise,
if a binding for KEY is already present in MENU, just change it;
otherwise put the new binding last in MENU.
BEFORE can be either a string (menu item name) or a symbol
\(the fake function key for the menu item).
KEY does not have to be a symbol, and comparison is done with equal."
  (let ((inserted (null item))		; Fake already inserted.
	tail done)
    (while (not done)
      (cond
       ((or (setq done (or (null (cdr menu)) (keymapp (cdr menu))))
	    (and before (easy-menu-name-match before (cadr menu))))
	;; If key is nil, stop here, otherwise keep going past the
	;; inserted element so we can delete any duplications that come
	;; later.
	(if (null key) (setq done t))
	(unless inserted		; Don't insert more than once.
	  (setcdr menu (cons (cons key item) (cdr menu)))
	  (setq inserted t)
	  (setq menu (cdr menu)))
	(setq menu (cdr menu)))
       ((and key (equal (car-safe (cadr menu)) key))
	(if (or inserted		; Already inserted or
		(and before		;  wanted elsewhere and
		     (setq tail (cddr menu)) ; not last item and not
		     (not (keymapp tail))
		     (not (easy-menu-name-match
			   before (car tail))))) ; in position
	    (setcdr menu (cddr menu))	; Remove item.
	  (setcdr (cadr menu) item)	; Change item.
	  (setq inserted t)
	  (setq menu (cdr menu))))
       (t (setq menu (cdr menu)))))))

(defun easy-menu-name-match (name item)
  "Return t if NAME is the name of menu item ITEM.
NAME can be either a string, or a symbol."
  (if (consp item)
      (if (symbolp name) 
	  (eq (car-safe item) name)
	(if (stringp name)
	    ;; Match against the text that is displayed to the user.
	    (or (member-ignore-case name item)
		;; Also check the string version of the symbol name,
		;; for backwards compatibility.
		(eq (car-safe item) (intern name)))))))

(defun easy-menu-always-true (x)
  "Return true if form X never evaluates to nil."
  (if (consp x) (and (eq (car x) 'quote) (cadr x))
    (or (eq x t) (not (symbolp x)))))

(defvar easy-menu-item-count 0)

(defun easy-menu-make-symbol (callback &optional noexp)
  "Return a unique symbol with CALLBACK as function value.
When non-nil, NOEXP indicates that CALLBACK cannot be an expression
\(i.e. does not need to be turned into a function)."
  (let ((command
	 (make-symbol (format "menu-function-%d" easy-menu-item-count))))
    (setq easy-menu-item-count (1+ easy-menu-item-count))
    (fset command
	  (if (or (keymapp callback) noexp) callback
	    `(lambda () (interactive) ,callback)))
    command))

;;;###autoload
(defun easy-menu-change (path name items &optional before)
  "Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu that
should contain a submenu named NAME.
ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that submenu.

If the menu located by PATH has no submenu named NAME, add one.
If the optional argument BEFORE is present, add it just before
the submenu named BEFORE, otherwise add it at the end of the menu.

Either call this from `menu-bar-update-hook' or use a menu filter,
to implement dynamic menus."
  (easy-menu-add-item nil path (cons name items) before))

;; XEmacs needs the following two functions to add and remove menus.
;; In Emacs this is done automatically when switching keymaps, so
;; here easy-menu-remove is a noop and easy-menu-add only precalculates
;; equivalent keybindings (if easy-menu-precalculate-equivalent-keybindings
;; is on).
(defalias 'easy-menu-remove 'ignore)

(defun easy-menu-add (menu &optional map)
  "Maybe precalculate equivalent key bindings.
Do it if `easy-menu-precalculate-equivalent-keybindings' is on,"
  (when easy-menu-precalculate-equivalent-keybindings
    (if (and (symbolp menu) (not (keymapp menu)) (boundp menu))
	(setq menu (symbol-value menu)))
    (if (keymapp menu) (x-popup-menu nil menu))))

(defun add-submenu (menu-path submenu &optional before in-menu)
  "Add submenu SUBMENU in the menu at MENU-PATH.
If BEFORE is non-nil, add before the item named BEFORE.
If IN-MENU is non-nil, follow MENU-PATH in IN-MENU.
This is a compatibility function; use `easy-menu-add-item'."
  (easy-menu-add-item (or in-menu (current-global-map))
		      (cons "menu-bar" menu-path)
		      submenu before))

(defun easy-menu-add-item (map path item &optional before)
  "To the submenu of MAP with path PATH, add ITEM.

If an item with the same name is already present in this submenu,
then ITEM replaces it.  Otherwise, ITEM is added to this submenu.
In the latter case, ITEM is normally added at the end of the submenu.
However, if BEFORE is a string and there is an item in the submenu
with that name, then ITEM is added before that item.

MAP should normally be a keymap; nil stands for the global menu-bar keymap.
It can also be a symbol, which has earlier been used as the first
argument in a call to `easy-menu-define', or the value of such a symbol.

PATH is a list of strings for locating the submenu where ITEM is to be
added.  If PATH is nil, MAP itself is used.  Otherwise, the first
element should be the name of a submenu directly under MAP.  This
submenu is then traversed recursively with the remaining elements of PATH.

ITEM is either defined as in `easy-menu-define' or a non-nil value returned
by `easy-menu-item-present-p' or `easy-menu-remove-item' or a menu defined
earlier by `easy-menu-define' or `easy-menu-create-menu'."
  (setq map (easy-menu-get-map map path
			       (and (null map) (null path)
				    (stringp (car-safe item))
				    (car item))))
  (if (and (consp item) (consp (cdr item)) (eq (cadr item) 'menu-item))
      ;; This is a value returned by `easy-menu-item-present-p' or
      ;; `easy-menu-remove-item'.
      (easy-menu-define-key map (easy-menu-intern (car item))
			    (cdr item) before)
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
    (easy-menu-do-add-item map item before)))

(defun easy-menu-item-present-p (map path name)
  "In submenu of MAP with path PATH, return true iff item NAME is present.
MAP and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be looked for."
  (easy-menu-return-item (easy-menu-get-map map path) name))

(defun easy-menu-remove-item (map path name)
  "From submenu of MAP with path PATH remove item NAME.
MAP and PATH are defined as in `easy-menu-add-item'.
NAME should be a string, the name of the element to be removed."
  (setq map (easy-menu-get-map map path))
  (let ((ret (easy-menu-return-item map name)))
    (if ret (easy-menu-define-key map (easy-menu-intern name) nil))
    ret))

(defun easy-menu-return-item (menu name)
  "In menu MENU try to look for menu item with name NAME.
If a menu item is found, return (NAME . item), otherwise return nil.
If item is an old format item, a new format item is returned."
  (let ((item (lookup-key menu (vector (easy-menu-intern name))))
	ret enable cache label)
    (cond
     ((stringp (car-safe item))
      ;; This is the old menu format. Convert it to new format.
      (setq label (car item))
      (when (stringp (car (setq item (cdr item)))) ; Got help string
	(setq ret (list :help (car item)))
	(setq item (cdr item)))
      (when (and (consp item) (consp (car item))
		 (or (null (caar item)) (numberp (caar item))))
	(setq cache (car item))		; Got cache
	(setq item (cdr item)))
      (and (symbolp item) (setq enable (get item 'menu-enable))	; Got enable
	   (setq ret (cons :enable (cons enable ret))))
      (if cache (setq ret (cons cache ret)))
      (cons name (cons 'menu-enable (cons label (cons item ret)))))
     (item ; (or (symbolp item) (keymapp item) (eq (car-safe item) 'menu-item))
      (cons name item))			; Keymap or new menu format
     )))

(defun easy-menu-get-map-look-for-name (name submap)
  (while (and submap (not (easy-menu-name-match name (car submap))))
    (setq submap (cdr submap)))
  submap)

;; This should really be in keymap.c
(defun easy-menu-current-active-maps ()
  (let ((maps (list (current-local-map) global-map)))
    (dolist (minor minor-mode-map-alist)
      (when (and (boundp (car minor))
		 (symbol-value (car minor)))
	(push (cdr minor) maps)))
    (delq nil maps)))

(defun easy-menu-get-map (map path &optional to-modify)
  "Return a sparse keymap in which to add or remove an item.
MAP and PATH are as defined in `easy-menu-add-item'.

TO-MODIFY, if non-nil, is the name of the item the caller
wants to modify in the map that we return.
In some cases we use that to select between the local and global maps."
  (setq map
	(catch 'found
	  (let* ((key (vconcat (unless map '(menu-bar))
			       (mapcar 'easy-menu-intern path)))
		 (maps (mapcar (lambda (map)
				 (setq map (lookup-key map key))
				 (while (and (symbolp map) (keymapp map))
				   (setq map (symbol-function map)))
				 map)
			       (if map
				   (list (if (and (symbolp map)
						  (not (keymapp map)))
					     (symbol-value map) map))
				 (easy-menu-current-active-maps)))))
	    ;; Prefer a map that already contains the to-be-modified entry.
	    (when to-modify
	      (dolist (map maps)
		(when (and map (not (integerp map))
			   (easy-menu-get-map-look-for-name to-modify map))
		  (throw 'found map))))
	    ;; Use the first valid map.
	    (dolist (map maps)
	      (when (and map (not (integerp map)))
		(throw 'found map)))
	    ;; Otherwise, make one up.
	    ;; Hardcoding current-local-map is lame, but it's difficult
	    ;; to know what the caller intended for us to do ;-(
	    (let* ((name (if path (format "%s" (car (reverse path)))))
		   (newmap (make-sparse-keymap name)))
	      (define-key (or map (current-local-map)) key
		(if name (cons name newmap) newmap))
	      newmap))))
  (or (keymapp map) (error "Malformed menu in easy-menu: (%s)" map))
  map)

(provide 'easymenu)

;;; easymenu.el ends here
