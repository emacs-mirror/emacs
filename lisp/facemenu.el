;;; facemenu.el -- Create a face menu for interactively adding fonts to text
;; Copyright (c) 1994 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@cs.rochester.edu>
;; Keywords: faces

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

;;; Commentary:
;; This file defines a menu of faces (bold, italic, etc) which allows you to
;; set the face used for a region of the buffer.  Some faces also have
;; keybindings, which are shown in the menu.  Faces with names beginning with
;; "fg:" or "bg:", as in "fg:red", are treated specially.  It is assumed that
;; Such faces are assumed to consist only of a foreground (if "fg:") or
;; background (if "bg:") color.  They are thus put into the color submenus
;; rather than the general Face submenu.  Such faces can also be created on
;; demand from the "Other..." menu items.

;;; Usage:
;; Selecting a face from the menu or typing the keyboard equivalent will
;; change the region to use that face.  If you use transient-mark-mode and the
;; region is not active, the face will be remembered and used for the next
;; insertion.  It will be forgotten if you move point or make other
;; modifications before inserting or typing anything.
;;
;; Faces can be selected from the keyboard as well.  
;; The standard keybindings are M-s (or ESC s) + letter:
;; M-s i = "set italic",  M-s b = "set bold", etc.

;;; Customization:
;; An alternative set of keybindings that may be easier to type can be set up
;; using "Hyper" keys.  This requires that you set up a hyper-key on your
;; keyboard.  On my system, putting the following command in my .xinitrc:
;;    xmodmap -e "keysym Alt_L = Hyper_L" -e "add Mod2 = Hyper_L"
;; makes the key labelled "Alt" act as a hyper key, but check with local
;; X-perts for how to do it on your system. If you do this, then put the
;; following in your .emacs before the (require 'facemenu):
;;   (setq facemenu-keybindings
;;    '((default     . [?\H-d])
;;      (bold        . [?\H-b])
;;      (italic      . [?\H-i])
;;      (bold-italic . [?\H-o])
;;      (underline   . [?\H-u])))
;;   (setq facemenu-keymap global-map)
;;   (setq facemenu-key nil)
;;
;; In general, the order of the faces that appear in the menu and their
;; keybindings can be controlled by setting the variable
;; `facemenu-keybindings'.  Faces that you never want to add to your
;; document (e.g., `region') are listed in `facemenu-unlisted-faces'.

;;; Known Problems:
;; There is at present no way to display what the faces look like in
;; the menu itself.
;;
;; `list-faces-display' shows the faces in a different order than
;; this menu, which could be confusing.  I do /not/ sort the list
;; alphabetically, because I like the default order: it puts the most
;; basic, common fonts first.
;;
;; Please send me any other problems, comments or ideas.

;;; Code:

(provide 'facemenu)

(defvar facemenu-key "\M-g"
  "Prefix to use for facemenu commands.")

(defvar facemenu-keybindings
  '((default     . "d")
    (bold        . "b")
    (italic      . "i")
    (bold-italic . "o")  ; O for "Oblique" or "bOld"...
    (underline   . "u"))
  "Alist of interesting faces and keybindings. 
Each element is itself a list: the car is the name of the face,
the next element is the key to use as a keyboard equivalent of the menu item;
the binding is made in facemenu-keymap.

The faces specifically mentioned in this list are put at the top of
the menu, in the order specified.  All other faces which are defined,
except for those in `facemenu-unlisted-faces', are listed after them, 
but get no keyboard equivalents.

If you change this variable after loading facemenu.el, you will need to call
`facemenu-update' to make it take effect.")

(defvar facemenu-unlisted-faces
  '(modeline region secondary-selection highlight scratch-face)
  "Faces that are not included in the Face menu.
Set this before loading facemenu.el, or call `facemenu-update' after
changing it.")

(defvar facemenu-face-menu 
  (let ((map (make-sparse-keymap "Face")))
    (define-key map [other] (cons "Other..." 'facemenu-set-face))
    map)
  "Menu keymap for faces.")

(defvar facemenu-foreground-menu 
  (let ((map (make-sparse-keymap "Foreground Color")))
    (define-key map "o" (cons "Other" 'facemenu-set-foreground))
    map)
  "Menu keymap for foreground colors.")

(defvar facemenu-background-menu
  (let ((map (make-sparse-keymap "Background Color")))
    (define-key map "o" (cons "Other" 'facemenu-set-background))
    map)
  "Menu keymap for background colors")

(defvar facemenu-special-menu 
  (let ((map (make-sparse-keymap "Special")))
    (define-key map [read-only] (cons "Read-Only" 'facemenu-set-read-only))
    (define-key map [invisible] (cons "Invisible" 'facemenu-set-invisible))
    map)
  "Menu keymap for non-face text-properties.")

(defvar facemenu-menu 
  (let ((map (make-sparse-keymap "Face")))
    (define-key map [display]  (cons "Display Faces" 'list-faces-display))
    (define-key map [remove]   (cons "Remove Props" 'facemenu-remove-all))
    (define-key map [sep1]     (list "-----------------"))
    (define-key map [special]  (cons "Special Props" facemenu-special-menu))
    (define-key map [bg]       (cons "Background Color" facemenu-background-menu))
    (define-key map [fg]       (cons "Foreground Color" facemenu-foreground-menu))
    (define-key map [face]     (cons "Face" facemenu-face-menu))
    map)
  "Facemenu top-level menu keymap.")

(defvar facemenu-keymap (make-sparse-keymap "Set face")
  "Map for keyboard face-changing commands.
`Facemenu-update' fills in the keymap according to the bindings
requested in `facemenu-keybindings'.")

;;; Internal Variables

(defvar facemenu-color-alist nil
  ;; Don't initialize here; that doesn't work if preloaded.
  "Alist of colors, used for completion.
If null, `facemenu-read-color' will set it.")

(defvar facemenu-next nil) ; set when we are going to set a face on next char.
(defvar facemenu-loc nil)

(defun facemenu-update ()
  "Add or update the \"Face\" menu in the menu bar.
You can call this to update things if you change any of the menu configuration
variables."
  (interactive)
  
  ;; Global bindings:
  (define-key global-map [C-down-mouse-2] facemenu-menu)
  (if facemenu-key (define-key global-map facemenu-key facemenu-keymap))

  ;; Add each defined face to the menu.
  (facemenu-iterate 'facemenu-add-new-face
		    (facemenu-complete-face-list facemenu-keybindings)))

;;;###autoload
(defun facemenu-set-face (face &optional start end)
  "Add FACE to the region or next character typed.
It will be added to the top of the face list; any faces lower on the list that
will not show through at all will be removed.

Interactively, the face to be used is prompted for.
If the region is active, it will be set to the requested face.  If 
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (read-face-name "Use face: ")))
  (if mark-active
      (let ((start (or start (region-beginning)))
	    (end (or end (region-end))))
	(facemenu-add-face face start end))
    (setq facemenu-next face
	  facemenu-loc (point))))

;;;###autoload
(defun facemenu-set-foreground (color &optional start end)
  "Set the foreground color of the region or next character typed.
The color is prompted for.  A face named `fg:color' is used \(or created).
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (facemenu-read-color "Foreground color: ")))
  (let ((face (intern (concat "fg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (facemenu-set-face face start end)))

;;;###autoload
(defun facemenu-set-background (color &optional start end)
  "Set the background color of the region or next character typed.
The color is prompted for.  A face named `bg:color' is used \(or created).
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (facemenu-read-color "Background color: ")))
  (let ((face (intern (concat "bg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (facemenu-set-face face start end)))

(defun facemenu-set-face-from-menu (face start end)
  "Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list last-command-event
		     (if mark-active (region-beginning))
		     (if mark-active (region-end))))
  (facemenu-get-face face)
  (if start 
      (facemenu-add-face face start end)
    (setq facemenu-next face facemenu-loc (point))))

(defun facemenu-set-invisible (start end)
  "Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-all'."
  (interactive "r")
  (put-text-property start end 'invisible t))

(defun facemenu-set-intangible (start end)
  "Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-all'."
  (interactive "r")
  (put-text-property start end 'intangible t))

(defun facemenu-set-read-only (start end)
  "Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-all'."
  (interactive "r")
  (put-text-property start end 'read-only t))

(defun facemenu-remove-all (start end)
  "Remove all text properties that facemenu added to region."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties 
     start end '(face nil invisible nil intangible nil 
		      read-only nil category nil))))

;;;###autoload
(defun facemenu-read-color (prompt)
  "Read a color using the minibuffer."
  (let ((col (completing-read (or  "Color: ") 
			      (or facemenu-color-alist
				  (if (eq 'x window-system)
				      (mapcar 'list (x-defined-colors))))
			      nil t)))
    (if (equal "" col)
	nil
      col)))

(defun facemenu-add-face (face start end)
  "Add FACE to text between START and END.
For each section of that region that has a different face property, FACE will
be consed onto it, and other faces that are completely hidden by that will be
removed from the list.

As a special case, if FACE is `default', then the region is left with NO face
text property.  Otherwise, selecting the default face would not have any
effect."
  (interactive "*xFace:\nr")
  (if (eq face 'default)
      (remove-text-properties start end '(face default))
    (let ((part-start start) part-end)
      (while (not (= part-start end))
	(setq part-end (next-single-property-change part-start 'face nil end))
	(let ((prev (get-text-property part-start 'face)))
	  (put-text-property part-start part-end 'face
			     (if (null prev)
				 face
			       (facemenu-discard-redundant-faces
				(cons face
				      (if (listp prev) prev (list prev)))))))
	(setq part-start part-end)))))

(defun facemenu-discard-redundant-faces (face-list &optional mask)
  "Remove from FACE-LIST any faces that won't show at all.
This means they have no non-nil elements that aren't also non-nil in an
earlier face."
  (let ((useful nil))
    (cond ((null face-list) nil)
	  ((null mask)
	   (cons (car face-list)
		 (facemenu-discard-redundant-faces
		  (cdr face-list) 
		  (copy-sequence (internal-get-face (car face-list))))))
	  ((let ((i (length mask))
		 (face (internal-get-face (car face-list))))
	     (while (>= (setq i (1- i)) 0)
	       (if (and (aref face i)
			(not (aref mask i)))
		   (progn (setq useful t)
			  (aset mask i t))))
	     useful)
	   (cons (car face-list)
		 (facemenu-discard-redundant-faces (cdr face-list) mask)))
	  (t (facemenu-discard-redundant-faces (cdr face-list) mask)))))

(defun facemenu-get-face (symbol)
  "Make sure FACE exists.
If not, it is created.  If it is created and is of the form `fg:color', then
set the foreground to that color. If of the form `bg:color', set the
background.  In any case, add it to the appropriate menu.  Returns nil if
given a bad color."
  (or (internal-find-face symbol)
      (let* ((face (make-face symbol))
	     (name (symbol-name symbol))
	     (color (substring name 3)))
	(cond ((string-match "^fg:" name)
	       (set-face-foreground face color)
	       (and (eq 'x window-system) (x-color-defined-p color)))
	      ((string-match "^bg:" name)
	       (set-face-background face color)
	       (and (eq 'x window-system) (x-color-defined-p color)))
	      (t)))))

(defun facemenu-add-new-face (face)
  "Add a FACE to the appropriate Face menu.
Automatically called when a new face is created."
  (let* ((name (symbol-name face))
	 (menu (cond ((string-match "^fg:" name) 
		      (setq name (substring name 3))
		      facemenu-foreground-menu)
		     ((string-match "^bg:" name) 
		      (setq name (substring name 3))
		      facemenu-background-menu)
		     (t facemenu-face-menu)))
	 key)
    (cond ((memq face facemenu-unlisted-faces)
	   nil)
	  ((setq key (cdr (assoc face facemenu-keybindings)))
	   (let ((function (intern (concat "facemenu-set-" name))))
	     (fset function
		   (` (lambda () (interactive)
			(facemenu-set-face (quote (, face))))))
	     (define-key facemenu-keymap key (cons name function))
	     (define-key menu key (cons name function))))
	  (t (define-key menu (vector face) 
	       (cons name 'facemenu-set-face-from-menu)))))
  ;; Return nil for facemenu-iterate's benefit:
  nil)

(defun facemenu-after-change (begin end old-length)
  "May set the face of just-inserted text to user's request.
This only happens if the change is an insertion, and
`facemenu-set-face[-from-menu]' was called with point at the
beginning of the insertion."
  (if (null facemenu-next)		; exit immediately if no work
      nil
    (if (and (= 0 old-length)		; insertion
	     (= facemenu-loc begin))	; point wasn't moved in between
	(facemenu-add-face facemenu-next begin end))
    (setq facemenu-next nil)))

(defun facemenu-complete-face-list (&optional oldlist)
  "Return list of all faces that are look different.
Starts with given ALIST of faces, and adds elements only if they display 
differently from any face already on the list.
The faces on ALIST will end up at the end of the returned list, in reverse 
order."
  (let ((list (nreverse (mapcar 'car oldlist))))
    (facemenu-iterate 
     (lambda (new-face) 
       (if (not (memq new-face list))
	   (setq list (cons new-face list)))
       nil)
     (nreverse (face-list)))
    list))

(defun facemenu-iterate (func iterate-list)
  "Apply FUNC to each element of LIST until one returns non-nil.
Returns the non-nil value it found, or nil if all were nil."
  (while (and iterate-list (not (funcall func (car iterate-list))))
    (setq iterate-list (cdr iterate-list)))
  (car iterate-list))

(facemenu-update)
(add-hook 'after-change-functions 'facemenu-after-change)

;;; facemenu.el ends here
