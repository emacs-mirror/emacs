;;; lmenu.el --- emulate Lucid's menubar support

;; Keywords: emulations

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

;;; Code:


;; First, emulate the Lucid menubar support in GNU Emacs 19.

;; Arrange to use current-menubar to set up part of the menu bar.

(setq recompute-lucid-menubar 'recompute-lucid-menubar)
(defun recompute-lucid-menubar ()
  (define-key lucid-menubar-map [menu-bar]
    (condition-case nil
	(make-lucid-menu-keymap "menu-bar" current-menubar)
      (error (message "Invalid data in current-menubar moved to lucid-failing-menubar")
	     (sit-for 1)
	     (setq lucid-failing-menubar current-menubar
		   current-menubar nil))))
  (setq lucid-menu-bar-dirty-flag nil))

(defvar lucid-menubar-map (make-sparse-keymap))
(or (assq 'current-menubar minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'current-menubar lucid-menubar-map)
		minor-mode-map-alist)))

(defun set-menubar-dirty-flag ()
  (force-mode-line-update)
  (setq lucid-menu-bar-dirty-flag t))

(defvar add-menu-item-count 0)

;; Return a menu keymap corresponding to a Lucid-style menu list
;; MENU-ITEMS, and with name MENU-NAME.
(defun make-lucid-menu-keymap (menu-name menu-items)
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
	       (setq name item))
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
	(if name 
	    (define-key menu (vector (intern name)) (cons name command))))
      (setq menu-items (cdr menu-items)))
    menu))

(defun popup-menu (menu-desc)
  "Pop up the given menu.
A menu is a list of menu items, strings, and submenus.

The first element of a menu must be a string, which is the name of the
menu.  This is the string that will be displayed in the parent menu, if
any.  For toplevel menus, it is ignored.  This string is not displayed
in the menu itself.

A menu item is a vector of three or four elements:

 - the name of the menu item (a string);
 - the `callback' of that item;
 - whether this item is active (selectable);
 - and an optional string to append to the name.

If the `callback' of a menu item is a symbol, then it must name a command.
It will be invoked with `call-interactively'.  If it is a list, then it is
evaluated with `eval'.

The fourth element of a menu item is a convenient way of adding the name
of a command's ``argument'' to the menu, like ``Kill Buffer NAME''.

If an element of a menu is a string, then that string will be presented in
the menu as unselectable text.

If an element of a menu is a string consisting solely of hyphens, then that
item will be presented as a solid horizontal line.

If an element of a menu is a list, it is treated as a submenu.  The name of
that submenu (the first element in the list) will be used as the name of the
item representing this menu on the parent.

The syntax, more precisely:

   form		:=  <something to pass to `eval'>
   command	:=  <a symbol or string, to pass to `call-interactively'>
   callback 	:=  command | form
   active-p	:=  <t or nil, whether this thing is selectable>
   text		:=  <string, non selectable>
   name		:=  <string>
   argument	:=  <string>
   menu-item	:=  '['  name callback active-p [ argument ]  ']'
   menu		:=  '(' name [ menu-item | menu | text ]+ ')'
"
  (let ((menu (make-lucid-menu-keymap (car menu-desc) (cdr menu-desc)))
	(pos (mouse-position))
	answer)
    (setq answer (x-popup-menu (list (list (nth 1 pos) (nthcdr 2 pos))
				     (car pos))
			       menu))
    (setq cmd (lookup-key menu (vector answer)))
    (if cmd (call-interactively cmd))))

(defconst default-menubar
  '(("File"	["New Frame"		x-new-frame		t]
		["Open File..."		find-file		t]
		["Save Buffer"		save-buffer		t  nil]
		["Save Buffer As..."	write-file		t]
		["Revert Buffer"	revert-buffer		t  nil]
		"-----"
		["Print Buffer"		lpr-buffer		t  nil]
		"-----"
		["Delete Frame"		delete-frame		t]
;;		["Kill Buffer..."	kill-buffer		t]
		["Kill Buffer"		kill-this-buffer	t  nil]
		["Exit Emacs"		save-buffers-kill-emacs	t]
		)
    ("Edit"	["Undo"			advertised-undo		   t]
		["Cut"			x-kill-primary-selection   t]
		["Copy"			x-copy-primary-selection   t]
		["Paste"		x-yank-clipboard-selection t]
		["Clear"		x-delete-primary-selection t]
		)
    ("Buffers"	"")

    nil		; the partition: menus after this are flushright

    ("Help"	["Info"			info			t]
		["Describe Mode"	describe-mode		t]
		["Command Apropos..."	command-apropos		t]
		["List Keybindings"	describe-bindings	t]
		["Describe Key..."	describe-key		t]
		["Describe Function..."	describe-function	t]
		["Describe Variable..."	describe-variable	t]
		"-----"
		["Man..."		manual-entry		t]
		["Emacs Tutorial"	help-with-tutorial	t]
		["Emacs News"		view-emacs-news		t]
		)
    ))


(defun kill-this-buffer ()	; for the menubar
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun x-new-frame (&optional frame-name)
  "Creates a new Emacs frame (that is, a new X window.)"
  (interactive)
  (select-frame (x-create-frame
		  (append (if frame-name
			      (list (cons 'name frame-name))
			    nil)
			  frame-default-alist)))
  (switch-to-buffer (get-buffer-create "*scratch*"))
  )

(defun set-menubar (menubar)
  "Set the default menubar to be menubar."
  (setq-default current-menubar (copy-sequence menubar))
  (set-menubar-dirty-flag))

(defun set-buffer-menubar (menubar)
  "Set the buffer-local menubar to be menubar."
  (make-local-variable 'current-menubar)
  (setq current-menubar (copy-sequence menubar))
  (set-menubar-dirty-flag))


;;; menu manipulation functions

(defun find-menu-item (menubar item-path-list &optional parent)
  "Searches MENUBAR for item given by ITEM-PATH-LIST.
Returns (ITEM . PARENT), where PARENT is the immediate parent of
 the item found.
Signals an error if the item is not found."
  (or parent (setq item-path-list (mapcar 'downcase item-path-list)))
  (if (not (consp menubar))
      nil
    (let ((rest menubar)
	  result)
      (while rest
	(if (and (car rest)
		 (equal (car item-path-list)
			(downcase (if (vectorp (car rest))
				      (aref (car rest) 0)
				    (if (stringp (car rest))
					(car rest)
				      (car (car rest)))))))
	    (setq result (car rest) rest nil)
	  (setq rest (cdr rest))))
      (if (cdr item-path-list)
	  (if (consp result)
	      (find-menu-item (cdr result) (cdr item-path-list) result)
	    (if result
		(signal 'error (list "not a submenu" result))
	      (signal 'error (list "no such submenu" (car item-path-list)))))
	(cons result parent)))))


(defun disable-menu-item (path)
  "Make the named menu item be unselectable.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu "No such menu item" "No such menu")
			     path)))
    (if (consp item) (error "can't disable menus, only menu items"))
    (aset item 2 nil)
    (set-menubar-dirty-flag)
    item))


(defun enable-menu-item (path)
  "Make the named menu item be selectable.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu "No such menu item" "No such menu")
			     path)))
    (if (consp item) (error "%S is a menu, not a menu item" path))
    (aset item 2 t)
    (set-menubar-dirty-flag)
    item))


(defun add-menu-item-1 (item-p menu-path item-name item-data enabled-p before)
  (if before (setq before (downcase before)))
  (let* ((menubar current-menubar)
	 (menu (condition-case ()
		   (car (find-menu-item menubar menu-path))
		 (error nil)))
	 (item (if (listp menu)
		   (car (find-menu-item (cdr menu) (list item-name)))
		 (signal 'error (list "not a submenu" menu-path)))))
    (or menu
	(let ((rest menu-path)
	      (so-far menubar))
	  (while rest
;;;	    (setq menu (car (find-menu-item (cdr so-far) (list (car rest)))))
	    (setq menu
		  (if (eq so-far menubar)
		      (car (find-menu-item so-far (list (car rest))))
		    (car (find-menu-item (cdr so-far) (list (car rest))))))
	    (or menu
		(let ((rest2 so-far))
		  (while (and (cdr rest2) (car (cdr rest2)))
		    (setq rest2 (cdr rest2)))
		  (setcdr rest2
		  (nconc (list (setq menu (list (car rest))))
			 (cdr rest2)))))
	    (setq so-far menu)
	    (setq rest (cdr rest)))))
    (or menu (setq menu menubar))
    (if item
	nil	; it's already there
      (if item-p
	  (setq item (vector item-name item-data enabled-p))
	(setq item (cons item-name item-data)))
      ;; if BEFORE is specified, try to add it there.
      (if before
	  (setq before (car (find-menu-item menu (list before)))))
      (let ((rest menu)
	    (added-before nil))
	(while rest
	  (if (eq before (car (cdr rest)))
	      (progn
		(setcdr rest (cons item (cdr rest)))
		(setq rest nil added-before t))
	    (setq rest (cdr rest))))
	(if (not added-before)
	    ;; adding before the first item on the menubar itself is harder
	    (if (and (eq menu menubar) (eq before (car menu)))
		(setq menu (cons item menu)
		      current-menubar menu)
	      ;; otherwise, add the item to the end.
	      (nconc menu (list item))))))
    (if item-p
	(progn
	  (aset item 1 item-data)
	  (aset item 2 (not (null enabled-p))))
      (setcar item item-name)
      (setcdr item item-data))
    (set-menubar-dirty-flag)
    item))

(defun add-menu-item (menu-path item-name function enabled-p &optional before)
  "Add a menu item to some menu, creating the menu first if necessary.
If the named item exists already, it is changed.
MENU-PATH identifies the menu under which the new menu item should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
ITEM-NAME is the string naming the menu item to be added.
FUNCTION is the command to invoke when this menu item is selected.
 If it is a symbol, then it is invoked with `call-interactively', in the same
 way that functions bound to keys are invoked.  If it is a list, then the 
 list is simply evaluated.
ENABLED-P controls whether the item is selectable or not.
BEFORE, if provided, is the name of a menu item before which this item should
 be added, if this item is not on the menu already.  If the item is already
 present, it will not be moved."
  (or menu-path (error "must specify a menu path"))
  (or item-name (error "must specify an item name"))
  (add-menu-item-1 t menu-path item-name function enabled-p before))


(defun delete-menu-item (path)
  "Remove the named menu item from the menu hierarchy.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (or (cdr pair) menubar)))
    (if (not item)
	nil
      ;; the menubar is the only special case, because other menus begin
      ;; with their name.
      (if (eq menu current-menubar)
	  (setq current-menubar (delq item menu))
	(delq item menu))
      (set-menubar-dirty-flag)
      item)))


(defun relabel-menu-item (path new-name)
  "Change the string of the specified menu item.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\".
NEW-NAME is the string that the menu item will be printed as from now on."
  (or (stringp new-name)
      (setq new-name (signal 'wrong-type-argument (list 'stringp new-name))))
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu "No such menu item" "No such menu")
			     path)))
    (if (and (consp item)
	     (stringp (car item)))
	(setcar item new-name)
      (aset item 0 new-name))
    (set-menubar-dirty-flag)
    item))

(defun add-menu (menu-path menu-name menu-items &optional before)
  "Add a menu to the menubar or one of its submenus.
If the named menu exists already, it is changed.
MENU-PATH identifies the menu under which the new menu should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
 If MENU-PATH is nil, then the menu will be added to the menubar itself.
MENU-NAME is the string naming the menu to be added.
MENU-ITEMS is a list of menu item descriptions.
 Each menu item should be a vector of three elements:
   - a string, the name of the menu item;
   - a symbol naming a command, or a form to evaluate;
   - and t or nil, whether this item is selectable.
BEFORE, if provided, is the name of a menu before which this menu should
 be added, if this menu is not on its parent already.  If the menu is already
 present, it will not be moved."
  (or menu-name (error "must specify a menu name"))
  (or menu-items (error "must specify some menu items"))
  (add-menu-item-1 nil menu-path menu-name menu-items t before))



(defvar put-buffer-names-in-file-menu t)

(defun sensitize-file-and-edit-menus-hook ()
  "For use as a value of activate-menubar-hook.
This function changes the sensitivity of these File and Edit menu items:

  Cut    sensitive only when emacs owns the primary X Selection.
  Copy   sensitive only when emacs owns the primary X Selection.
  Clear  sensitive only when emacs owns the primary X Selection.
  Paste  sensitive only when there is an owner for the X Clipboard Selection.
  Undo   sensitive only when there is undo information.
         While in the midst of an undo, this is changed to \"Undo More\".

  Kill Buffer    has the name of the current buffer appended to it.
  Print Buffer   has the name of the current buffer appended to it.
  Save Buffer    has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer is modified.
  Revert Buffer  has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer has a file.
  Delete Frame   sensitive only when there is more than one visible frame."
  ;;
  ;; the hair in here to not update the menubar unless something has changed
  ;; isn't really necessary (the menubar code is fast enough) but it makes
  ;; me feel better (and creates marginally less list garbage.)
  (let* ((file-menu (cdr (car (find-menu-item current-menubar '("File")))))
	 (edit-menu (cdr (car (find-menu-item current-menubar '("Edit")))))
	 (save	(car (find-menu-item file-menu '("Save Buffer"))))
	 (rvt   (car (find-menu-item file-menu '("Revert Buffer"))))
	 (del   (car (find-menu-item file-menu '("Delete Frame"))))
	 (print (car (find-menu-item file-menu '("Print Buffer"))))
	 (kill  (car (find-menu-item file-menu '("Kill Buffer"))))
	 (cut   (car (find-menu-item edit-menu '("Cut"))))
	 (copy  (car (find-menu-item edit-menu '("Copy"))))
	 (paste (car (find-menu-item edit-menu '("Paste"))))
	 (clear (car (find-menu-item edit-menu '("Clear"))))
	 (undo  (or (car (find-menu-item edit-menu '("Undo")))
		    (car (find-menu-item edit-menu '("Undo More")))))
	 (name (buffer-name))
	 (emacs-owns-selection-p (x-selection-owner-p))
	 (clipboard-exists-p (x-selection-exists-p 'CLIPBOARD))
	 undo-available undoing-more
	 (undo-info-available (not (null (and (not (eq t buffer-undo-list))
				   (if (eq last-command 'undo)
				       (setq undoing-more
					     (and (boundp 'pending-undo-list)
					    pending-undo-list)
				     buffer-undo-list))))))
	 undo-name undo-state
	 (change-p
	  (or (and cut   (not (eq emacs-owns-selection-p (aref cut 2))))
	      (and copy  (not (eq emacs-owns-selection-p (aref copy 2))))
	      (and clear (not (eq emacs-owns-selection-p (aref clear 2))))
	      (and paste (not (eq clipboard-exists-p (aref paste 2))))
	      (and save  (not (eq (buffer-modified-p) (aref save 2))))
	      (and rvt   (not (eq (not (not buffer-file-name)) (aref rvt 2))))
	      (and del   (not (eq (null (cdr (visible-frame-list))) (aref del 2))))
	      )))
    (if (not put-buffer-names-in-file-menu)
	nil
      (if (= (length save)  4) (progn (aset save  3 name) (setq change-p t)))
      (if (= (length rvt)   4) (progn (aset rvt   3 name) (setq change-p t)))
      (if (= (length print) 4) (progn (aset print 3 name) (setq change-p t)))
      (if (= (length kill)  4) (progn (aset kill  3 name) (setq change-p t))))
    (if save  (aset save  2 (buffer-modified-p)))
    (if rvt   (aset rvt   2 (not (not buffer-file-name))))
    (if del   (aset del   2 (null (cdr (visible-frame-list)))))
    (if cut   (aset cut   2 emacs-owns-selection-p))
    (if copy  (aset copy  2 emacs-owns-selection-p))
    (if clear (aset clear 2 emacs-owns-selection-p))
    (if paste (aset paste 2 clipboard-exists-p))

    ;; we could also do this with the third field of the item.
    (if (eq last-command 'undo)
	(setq undo-name "Undo More"
	      undo-state (not (null (and (boundp 'pending-undo-list)
					 pending-undo-list))))
      (setq undo-name "Undo"
	    undo-state (and (not (eq buffer-undo-list t))
			    (not (null
				  (or buffer-undo-list
				      (and (boundp 'pending-undo-list)
					   pending-undo-list)))))))
    (if buffer-read-only (setq undo-state nil))
    (if (and undo
	     (or (not (equal undo-name (aref undo 0)))
		 (not (eq undo-state (aref undo 2)))))
	(progn (aset undo 0 undo-name)
	       (aset undo 2 undo-state)
	       (setq change-p t)))
    ;; if we made any changes, return nil
    ;; otherwise return t to indicate that we haven't done anything.
    (not change-p)))

;; this version is too slow
(defun format-buffers-menu-line (buffer)
  "Returns a string to represent the given buffer in the Buffer menu.
nil means the buffer shouldn't be listed.  You can redefine this."
  (if (string-match "\\` " (buffer-name buffer))
      nil
    (save-excursion
     (set-buffer buffer)
     (let ((size (buffer-size)))
       (format "%s%s %-19s %6s %-15s %s"
	       (if (buffer-modified-p) "*" " ")
	       (if buffer-read-only "%" " ")
	       (buffer-name)
	       size
	       mode-name
	       (or (buffer-file-name) ""))))))
	   
(defun format-buffers-menu-line (buffer)
  (if (string-match "\\` " (setq buffer (buffer-name buffer)))
      nil
    buffer))

(defvar buffers-menu-max-size 10
  "*Maximum number of entries which may appear on the \"Buffers\" menu.
If this is 10, then only the ten most-recently-selected buffers will be
shown.  If this is nil, then all buffers will be shown.  Setting this to
a large number or nil will slow down menu responsiveness.")

(defvar complex-buffers-menu-p nil
  "*If true, the buffers menu will contain several commands, as submenus
of each buffer line.  If this is false, then there will be only one command:
select that buffer.")

(defvar buffers-menu-switch-to-buffer-function 'switch-to-buffer
  "*The function to call to select a buffer from the buffers menu.
`switch-to-buffer' is a good choice, as is `pop-to-buffer'.")


(defun buffer-menu-save-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (save-buffer)))

(defun buffer-menu-write-file (buffer)
  (save-excursion
    (set-buffer buffer)
    (write-file (read-file-name
		 (concat "Write " (buffer-name (current-buffer))
			 " to file: ")))))


(defsubst build-buffers-menu-internal (buffers)
  (let (name line)
    (mapcar
     (if complex-buffers-menu-p
	 (function
	  (lambda (buffer)
	    (if (setq line (format-buffers-menu-line buffer))
		(list line
		      (vector "Switch to Buffer"
			      (list buffers-menu-switch-to-buffer-function
				    (setq name (buffer-name buffer)))
			      t)
		      (if (and (buffer-modified-p buffer)
			       (buffer-file-name buffer))
			  (vector "Save Buffer"
				  (list 'buffer-menu-save-buffer name) t)
			["Save Buffer" nil nil])
		      (vector "Save Buffer As..."
			      (list 'buffer-menu-write-file name) t)
		      (vector "Kill Buffer" (list 'kill-buffer name) t)))))
       (function (lambda (buffer)
		   (if (setq line (format-buffers-menu-line buffer))
		       (vector line
			       (list buffers-menu-switch-to-buffer-function
				     (buffer-name buffer))
			       t)))))
     buffers)))

(defun build-buffers-menu-hook ()
  "For use as a value of activate-menubar-hook.
This function changes the contents of the \"Buffers\" menu to correspond
to the current set of buffers.  Only the most-recently-used few buffers
will be listed on the menu, for efficiency reasons.  You can control how
many buffers will be shown by setting `buffers-menu-max-size'.
You can control the text of the menu items by redefining the function
`format-buffers-menu-line'."
  (let ((buffer-menu (car (find-menu-item current-menubar '("Buffers"))))
	name
	buffers)
    (if (not buffer-menu)
	nil
      (setq buffers (buffer-list))

      (if (and (integerp buffers-menu-max-size)
	       (> buffers-menu-max-size 1))
	  (if (> (length buffers) buffers-menu-max-size)
	      (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

      (setq buffers (build-buffers-menu-internal buffers))
      (setq buffers (nconc (delq nil buffers)
			   '("----" ["List All Buffers" list-buffers t])))
      ;; slightly (only slightly) more efficient to not install the menubar
      ;; if it hasn't visibly changed.
      (if (equal buffers (cdr buffer-menu))
	  t  ; return t meaning "no change"
	(setcdr buffer-menu buffers)
	(set-menubar-dirty-flag)
	nil))))

(add-hook 'activate-menubar-hook 'build-buffers-menu-hook)
(add-hook 'activate-menubar-hook 'sensitize-file-and-edit-menus-hook)

(let ((frames (frame-list)))
  (while frames
    (modify-frame-parameters (car frames) '((menu-bar-lines . 1)))
    (setq frames (cdr frames))))
(or (assq 'menu-bar-lines default-frame-alist)
    (setq default-frame-alist
	  (cons '(menu-bar-lines . 1) default-frame-alist)))

(set-menubar default-menubar)

(provide 'menubar)

;;; lmenu.el ends here
