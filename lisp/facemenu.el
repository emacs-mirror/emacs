;;; facemenu.el --- create a face menu for interactively adding fonts to text

;; Copyright (c) 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file defines a menu of faces (bold, italic, etc) which allows you to
;; set the face used for a region of the buffer.  Some faces also have
;; keybindings, which are shown in the menu.  Faces with names beginning with
;; "fg:" or "bg:", as in "fg:red", are treated specially.
;; Such faces are assumed to consist only of a foreground (if "fg:") or
;; background (if "bg:") color.  They are thus put into the color submenus
;; rather than the general Face submenu.  These faces can also be
;; automatically created by selecting the "Other..." menu items in the
;; "Foreground" and "Background" submenus.
;;
;; The menu also contains submenus for indentation and justification-changing
;; commands.

;;; Usage:
;; Selecting a face from the menu or typing the keyboard equivalent will
;; change the region to use that face.  If you use transient-mark-mode and the
;; region is not active, the face will be remembered and used for the next
;; insertion.  It will be forgotten if you move point or make other
;; modifications before inserting or typing anything.
;;
;; Faces can be selected from the keyboard as well.  
;; The standard keybindings are M-g (or ESC g) + letter:
;; M-g i = "set italic",  M-g b = "set bold", etc.

;;; Customization:
;; An alternative set of keybindings that may be easier to type can be set up
;; using "Alt" or "Hyper" keys.  This requires that you either have or create
;; an Alt or Hyper key on your keyboard.  On my keyboard, there is a key
;; labeled "Alt", but to make it act as an Alt key I have to put this command
;; into my .xinitrc:
;;    xmodmap -e "add Mod3 = Alt_L"
;; Or, I can make it into a Hyper key with this:
;;    xmodmap -e "keysym Alt_L = Hyper_L" -e "add Mod2 = Hyper_L"
;; Check with local X-perts for how to do it on your system.
;; Then you can define your keybindings with code like this in your .emacs:
;;   (setq facemenu-keybindings
;;    '((default     . [?\H-d])
;;      (bold        . [?\H-b])
;;      (italic      . [?\H-i])
;;      (bold-italic . [?\H-l])
;;      (underline   . [?\H-u])))
;;   (facemenu-update)
;;   (setq facemenu-keymap global-map)
;;   (define-key global-map [?\H-c] 'facemenu-set-foreground) ; set fg color
;;   (define-key global-map [?\H-C] 'facemenu-set-background) ; set bg color
;;
;; The order of the faces that appear in the menu and their keybindings can be
;; controlled by setting the variables `facemenu-keybindings' and
;; `facemenu-new-faces-at-end'.  List faces that you don't use in documents
;; (eg, `region') in `facemenu-unlisted-faces'.

;;; Known Problems:
;; Bold and Italic do not combine to create bold-italic if you select them
;; both, although most other combinations (eg bold + underline + some color)
;; do the intuitive thing.
;;
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

;;; Provide some binding for startup:
;;;###autoload (define-key global-map "\M-g" 'facemenu-keymap)
;;;###autoload (autoload 'facemenu-keymap "facemenu" "Keymap for face-changing commands." t 'keymap)
  
;; Global bindings:
(define-key global-map [C-down-mouse-2] 'facemenu-menu)
(define-key global-map "\M-g" 'facemenu-keymap)

(defgroup facemenu nil
  "Create a face menu for interactively adding fonts to text"
  :group 'faces
  :prefix "facemenu-")

(defcustom facemenu-keybindings
  '((default     . "d")
    (bold        . "b")
    (italic      . "i")
    (bold-italic . "l") ; {bold} intersect {italic} = {l}
    (underline   . "u"))
  "Alist of interesting faces and keybindings. 
Each element is itself a list: the car is the name of the face,
the next element is the key to use as a keyboard equivalent of the menu item;
the binding is made in `facemenu-keymap'.

The faces specifically mentioned in this list are put at the top of
the menu, in the order specified.  All other faces which are defined,
except for those in `facemenu-unlisted-faces', are listed after them, 
but get no keyboard equivalents.

If you change this variable after loading facemenu.el, you will need to call
`facemenu-update' to make it take effect."
  :type '(repeat (cons face string))
  :group 'facemenu)

(defcustom facemenu-new-faces-at-end t
  "*Where in the menu to insert newly-created faces.
This should be nil to put them at the top of the menu, or t to put them
just before \"Other\" at the end."
  :type 'boolean
  :group 'facemenu)

(defcustom facemenu-unlisted-faces
  '(modeline region secondary-selection highlight scratch-face
    "^font-lock-" "^gnus-" "^message-" "^ediff-" "^term-" "^vc-"
    "^widget-" "^custom-" "^vm-")
  "*List of faces not to include in the Face menu.
Each element may be either a symbol, which is the name of a face, or a string,
which is a regular expression to be matched against face names.  Matching
faces will not be added to the menu.

You can set this list before loading facemenu.el, or add a face to it before
creating that face if you do not want it to be listed.  If you change the
variable so as to eliminate faces that have already been added to the menu,
call `facemenu-update' to recalculate the menu contents.

If this variable is t, no faces will be added to the menu.  This is useful for
temporarily turning off the feature that automatically adds faces to the menu
when they are created."
  :type '(choice (const :tag "Don't add" t)
		 (const :tag "None" nil)
		 (repeat (choice symbol regexp)))
  :group 'facemenu)

;;;###autoload
(defvar facemenu-face-menu
  (let ((map (make-sparse-keymap "Face")))
    (define-key map "o" (cons "Other..." 'facemenu-set-face))
    map)
  "Menu keymap for faces.")
;;;###autoload
(defalias 'facemenu-face-menu facemenu-face-menu)

;;;###autoload
(defvar facemenu-foreground-menu 
  (let ((map (make-sparse-keymap "Foreground Color")))
    (define-key map "o" (cons "Other..." 'facemenu-set-foreground))
    map)
  "Menu keymap for foreground colors.")
;;;###autoload
(defalias 'facemenu-foreground-menu facemenu-foreground-menu)

;;;###autoload
(defvar facemenu-background-menu
  (let ((map (make-sparse-keymap "Background Color")))
    (define-key map "o" (cons "Other..." 'facemenu-set-background))
    map)
  "Menu keymap for background colors")
;;;###autoload
(defalias 'facemenu-background-menu facemenu-background-menu)

;;;###autoload
(defvar facemenu-special-menu 
  (let ((map (make-sparse-keymap "Special")))
    (define-key map [?s] (cons "Remove Special" 'facemenu-remove-special))
    (define-key map [?t] (cons "Intangible" 'facemenu-set-intangible))
    (define-key map [?v] (cons "Invisible" 'facemenu-set-invisible))
    (define-key map [?r] (cons "Read-Only" 'facemenu-set-read-only))
    map)
  "Menu keymap for non-face text-properties.")
;;;###autoload
(defalias 'facemenu-special-menu facemenu-special-menu)

;;;###autoload
(defvar facemenu-justification-menu
  (let ((map (make-sparse-keymap "Justification")))
    (define-key map [?c] (cons "Center" 'set-justification-center))
    (define-key map [?b] (cons "Full" 'set-justification-full))
    (define-key map [?r] (cons "Right" 'set-justification-right))
    (define-key map [?l] (cons "Left" 'set-justification-left))
    (define-key map [?u] (cons "Unfilled" 'set-justification-none))
    map)
  "Submenu for text justification commands.")
;;;###autoload
(defalias 'facemenu-justification-menu facemenu-justification-menu)

;;;###autoload
(defvar facemenu-indentation-menu
  (let ((map (make-sparse-keymap "Indentation")))
    (define-key map [decrease-right-margin] 
      (cons "Indent Right Less" 'decrease-right-margin))
    (define-key map [increase-right-margin]
      (cons "Indent Right More" 'increase-right-margin))
    (define-key map [decrease-left-margin]
      (cons "Indent Less" 'decrease-left-margin))
    (define-key map [increase-left-margin]
      (cons "Indent More" 'increase-left-margin))
    map)
  "Submenu for indentation commands.")
;;;###autoload
(defalias 'facemenu-indentation-menu facemenu-indentation-menu)

;; This is split up to avoid an overlong line in loaddefs.el.
;;;###autoload
(defvar facemenu-menu nil
  "Facemenu top-level menu keymap.")
;;;###autoload
(setq facemenu-menu (make-sparse-keymap "Text Properties"))
;;;###autoload
(let ((map facemenu-menu))
  (define-key map [dc] (cons "Display Colors" 'list-colors-display))
  (define-key map [df] (cons "Display Faces" 'list-faces-display))
  (define-key map [dp] (cons "List Properties" 'list-text-properties-at))
  (define-key map [ra] (cons "Remove Text Properties" 'facemenu-remove-all))
  (define-key map [rm] (cons "Remove Face Properties" 'facemenu-remove-face-props))
  (define-key map [s1] (list "-----------------")))
;;;###autoload
(let ((map facemenu-menu))
  (define-key map [in] (cons "Indentation" 'facemenu-indentation-menu))
  (define-key map [ju] (cons "Justification" 'facemenu-justification-menu))
  (define-key map [s2] (list "-----------------"))
  (define-key map [sp] (cons "Special Properties" 'facemenu-special-menu))
  (define-key map [bg] (cons "Background Color" 'facemenu-background-menu))
  (define-key map [fg] (cons "Foreground Color" 'facemenu-foreground-menu))
  (define-key map [fc] (cons "Face" 'facemenu-face-menu)))
;;;###autoload
(defalias 'facemenu-menu facemenu-menu)

(defvar facemenu-keymap 
  (let ((map (make-sparse-keymap "Set face")))
    (define-key map "o" (cons "Other..." 'facemenu-set-face))
    map)
  "Keymap for face-changing commands.
`Facemenu-update' fills in the keymap according to the bindings
requested in `facemenu-keybindings'.")
(defalias 'facemenu-keymap facemenu-keymap)


(defcustom facemenu-add-face-function nil
  "Function called at beginning of text to change or `nil'.
This function is passed the FACE to set and END of text to change, and must
return a string which is inserted.  It may set `facemenu-end-add-face'."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'facemenu)

(defcustom facemenu-end-add-face nil
  "String to insert or function called at end of text to change or `nil'.
This function is passed the FACE to set, and must return a string which is
inserted."
  :type '(choice (const :tag "None" nil)
		 string
		 function)
  :group 'facemenu)

(defcustom facemenu-remove-face-function nil
  "When non-nil, this is a function called to remove faces.
This function is passed the START and END of text to change.
May also be `t' meaning to use `facemenu-add-face-function'."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Use add-face" t)
		 function)
  :group 'facemenu)

;;; Internal Variables

(defvar facemenu-color-alist nil
  ;; Don't initialize here; that doesn't work if preloaded.
  "Alist of colors, used for completion.
If null, `facemenu-read-color' will set it.")

(defun facemenu-update ()
  "Add or update the \"Face\" menu in the menu bar.
You can call this to update things if you change any of the menu configuration
variables."
  (interactive)

  ;; Add each defined face to the menu.
  (facemenu-iterate 'facemenu-add-new-face
		    (facemenu-complete-face-list facemenu-keybindings)))

;;;###autoload
(defun facemenu-set-face (face &optional start end)
  "Add FACE to the region or next character typed.
It will be added to the top of the face list; any faces lower on the list that
will not show through at all will be removed.

Interactively, the face to be used is read with the minibuffer.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." 
  (interactive (list (read-face-name "Use face: ")))
  (barf-if-buffer-read-only)
  (facemenu-add-new-face face)
  (if (and mark-active (not current-prefix-arg))
      (let ((start (or start (region-beginning)))
	    (end (or end (region-end))))
	(facemenu-add-face face start end))
    (facemenu-add-face face)))

;;;###autoload
(defun facemenu-set-foreground (color &optional start end)
  "Set the foreground color of the region or next character typed.
The color is prompted for.  A face named `fg:color' is used \(or created).
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
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
the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (facemenu-read-color "Background color: ")))
  (let ((face (intern (concat "bg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (facemenu-set-face face start end)))

;;;###autoload
(defun facemenu-set-face-from-menu (face start end)
  "Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." 
  (interactive (list last-command-event
		     (if (and mark-active (not current-prefix-arg))
			 (region-beginning))
		     (if (and mark-active (not current-prefix-arg))
			 (region-end))))
  (barf-if-buffer-read-only)
  (facemenu-get-face face)
  (if start 
      (facemenu-add-face face start end)
    (facemenu-add-face face)))

;;;###autoload
(defun facemenu-set-invisible (start end)
  "Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "r")
  (add-text-properties start end '(invisible t)))

;;;###autoload
(defun facemenu-set-intangible (start end)
  "Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "r")
  (add-text-properties start end '(intangible t)))

;;;###autoload
(defun facemenu-set-read-only (start end)
  "Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "r")
  (add-text-properties start end '(read-only t)))

;;;###autoload
(defun facemenu-remove-face-props (start end)
  "Remove `face' and `mouse-face' text properties."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties 
     start end '(face nil mouse-face nil))))

;;;###autoload
(defun facemenu-remove-all (start end)
  "Remove all text properties from the region."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (set-text-properties start end nil)))

;;;###autoload
(defun facemenu-remove-special (start end)
  "Remove all the \"special\" text properties from the region.
These special properties include `invisible', `intangible' and `read-only'."
  (interactive "*r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties 
     start end '(invisible nil intangible nil read-only nil))))

;;;###autoload
(defun list-text-properties-at (p)
  "Pop up a buffer listing text-properties at LOCATION."
  (interactive "d")
  (let ((props (text-properties-at p))
	category
	str)
    (if (null props)
	(message "None")
      (if (and (not (cdr (cdr props)))
	       (not (eq (car props) 'category))
	       (< (length (setq str (format "Text property at %d:  %s  %S"
					    p (car props) (car (cdr props)))))
		  (frame-width)))
	  (message "%s" str)
	(with-output-to-temp-buffer "*Text Properties*"
	  (princ (format "Text properties at %d:\n\n" p))
	  (while props
	    (if (eq (car props) 'category)
		(setq category (car (cdr props))))
	    (princ (format "%-20s %S\n"
			   (car props) (car (cdr props))))
	    (setq props (cdr (cdr props))))
	  (if category
	      (progn
		(setq props (symbol-plist category))
		(princ (format "\nCategory %s:\n\n" category))
		(while props
		  (princ (format "%-20s %S\n"
				 (car props) (car (cdr props))))
		  (if (eq (car props) 'category)
		      (setq category (car (cdr props))))
		  (setq props (cdr (cdr props)))))))))))

;;;###autoload
(defun facemenu-read-color (&optional prompt)
  "Read a color using the minibuffer."
  (let ((col (completing-read (or prompt "Color: ") 
			      (or facemenu-color-alist
				  (mapcar 'list (defined-colors)))
			      nil t)))
    (if (equal "" col)
	nil
      col)))

;;;###autoload
(defun list-colors-display (&optional list)
  "Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list
of colors that the current display can handle."
  (interactive)
  (when (null list)
    (setq list (defined-colors))
    ;; Delete duplicate colors.
    (let ((l list))
      (while (cdr l)
	(if (facemenu-color-equal (car l) (car (cdr l)))
	    (setcdr l (cdr (cdr l)))
	  (setq l (cdr l))))))
  (with-output-to-temp-buffer "*Colors*"
    (save-excursion
      (set-buffer standard-output)
      (let (s)
	(while list
	  (setq s (point))
	  (insert (car list))
	  (indent-to 20)
	  (put-text-property s (point) 'face 
			     (cons 'background-color (car list)))
	  (setq s (point))
	  (insert "  " (car list) "\n")
	  (put-text-property s (point) 'face 
			     (cons 'foreground-color (car list)))
	  (setq list (cdr list)))))))

(defun facemenu-color-equal (a b)
  "Return t if colors A and B are the same color.
A and B should be strings naming colors.
This function queries the display system to find out what the color
names mean.  It returns nil if the colors differ or if it can't
determine the correct answer."
  (cond ((equal a b) t)
	((equal (color-values a) (color-values b)))))

(defun facemenu-add-face (face &optional start end)
  "Add FACE to text between START and END.
If START is `nil' or START to END is empty, add FACE to next typed character
instead.  For each section of that region that has a different face property,
FACE will be consed onto it, and other faces that are completely hidden by
that will be removed from the list.
If `facemenu-add-face-function' and maybe `facemenu-end-add-face' are non-`nil'
they are used to set the face information.

As a special case, if FACE is `default', then the region is left with NO face
text property.  Otherwise, selecting the default face would not have any
effect.  See `facemenu-remove-face-function'."
  (interactive "*xFace: \nr")
  (if (and (eq face 'default)
	   (not (eq facemenu-remove-face-function t)))
      (if facemenu-remove-face-function
	  (funcall facemenu-remove-face-function start end)
	(if (and start (< start end))
	    (remove-text-properties start end '(face default))
	  (setq self-insert-face 'default
		self-insert-face-command this-command)))
    (if facemenu-add-face-function
	(save-excursion
	  (if end (goto-char end))
	  (save-excursion
	    (if start (goto-char start))
	    (insert-before-markers
	     (funcall facemenu-add-face-function face end)))
	  (if facemenu-end-add-face
	      (insert (if (stringp facemenu-end-add-face)
			  facemenu-end-add-face
			(funcall facemenu-end-add-face face)))))
      (if (and start (< start end))
	  (let ((part-start start) part-end)
	    (while (not (= part-start end))
	      (setq part-end (next-single-property-change part-start 'face
							  nil end))
	      (let ((prev (get-text-property part-start 'face)))
		(put-text-property part-start part-end 'face
				   (if (null prev)
				       face
				     (facemenu-active-faces
				      (cons face
					    (if (listp prev)
						prev
					      (list prev)))))))
	      (setq part-start part-end)))
	(setq self-insert-face (if (eq last-command self-insert-face-command)
				   (cons face (if (listp self-insert-face)
						  self-insert-face
						(list self-insert-face)))
				 face)
	      self-insert-face-command this-command)))))

(defun facemenu-active-faces (face-list &optional frame)
  "Return from FACE-LIST those faces that would be used for display.
This means each face attribute is not specified in a face earlier in FACE-LIST
and such a face is therefore active when used to display text.
If the optional argument FRAME is given, use the faces in that frame; otherwise
use the selected frame.  If t, then the global, non-frame faces are used."
  (let* ((mask-atts (copy-sequence (internal-get-face (car face-list) frame)))
	 (active-list (list (car face-list)))
	 (face-list (cdr face-list))
	 (mask-len (length mask-atts)))
    (while face-list
      (if (let ((face-atts (internal-get-face (car face-list) frame))
		(i mask-len) (useful nil))
	    (while (> (setq i (1- i)) 1)
	      (and (aref face-atts i) (not (aref mask-atts i))
		   (aset mask-atts i (setq useful t))))
	    useful)
	  (setq active-list (cons (car face-list) active-list)))
      (setq face-list (cdr face-list)))
    (nreverse active-list)))

(defun facemenu-get-face (symbol)
  "Make sure FACE exists.
If not, create it and add it to the appropriate menu.  Return the symbol.

If a window system is in use, and this function creates a face named
`fg:color', then it sets the foreground to that color.  Likewise, `bg:color'
means to set the background.  In either case, if the color is undefined,
no color is set and a warning is issued."
  (let ((name (symbol-name symbol))
	foreground)
    (cond ((internal-find-face symbol))
	  ((and window-system
		(or (setq foreground (string-match "^fg:" name))
		    (string-match "^bg:" name)))
	   (let ((face (make-face symbol))
		 (color (substring name 3)))
	     (if (x-color-defined-p color)
		 (if foreground
		     (set-face-foreground face color)
		   (set-face-background face color))
	       (message "Color \"%s\" undefined" color))))
	  (t (make-face symbol))))
  symbol)

(defun facemenu-add-new-face (face)
  "Add a FACE to the appropriate Face menu.
Automatically called when a new face is created."
  (let* ((name (symbol-name face))
	 menu docstring
	 (key (cdr (assoc face facemenu-keybindings)))
	 function menu-val)
    (cond ((string-match "^fg:" name) 
	   (setq name (substring name 3))
	   (setq docstring
		 (format "Select foreground color %s for subsequent insertion."
			 name))
	   (setq menu 'facemenu-foreground-menu))
	  ((string-match "^bg:" name) 
	   (setq name (substring name 3))
	   (setq docstring
		 (format "Select background color %s for subsequent insertion."
			 name))
	   (setq menu 'facemenu-background-menu))
	  (t
	   (setq docstring
		 (format "Select face `%s' for subsequent insertion."
			 name))
	   (setq menu 'facemenu-face-menu)))
    (cond ((eq t facemenu-unlisted-faces))
	  ((memq face facemenu-unlisted-faces))
	  ;; test against regexps in facemenu-unlisted-faces
	  ((let ((unlisted facemenu-unlisted-faces)
		 (matched nil))
	     (while (and unlisted (not matched))
	       (if (and (stringp (car unlisted))
			(string-match (car unlisted) name))
		   (setq matched t)
		 (setq unlisted (cdr unlisted))))
	     matched))
	  (key ; has a keyboard equivalent.  These go at the front.
	   (setq function (intern (concat "facemenu-set-" name)))
	   (fset function
		 `(lambda ()
		    ,docstring
		    (interactive)
		    (facemenu-set-face (quote ,face))))
	   (define-key 'facemenu-keymap key (cons name function))
	   (define-key menu key (cons name function)))
	  ((facemenu-iterate ; check if equivalent face is already in the menu
	    (lambda (m) (and (listp m) 
			     (symbolp (car m))
			     (face-equal (car m) face)))
	    (cdr (symbol-function menu))))
	  (t   ; No keyboard equivalent.  Figure out where to put it:
	   (setq key (vector face)
		 function 'facemenu-set-face-from-menu
		 menu-val (symbol-function menu))
	   (if (and facemenu-new-faces-at-end
		   (> (length menu-val) 3))
	       (define-key-after menu-val key (cons name function)
		 (car (nth (- (length menu-val) 3) menu-val)))
	     (define-key menu key (cons name function))))))
  nil) ; Return nil for facemenu-iterate

(defun facemenu-complete-face-list (&optional oldlist)
  "Return list of all faces that look different.
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

;;; facemenu.el ends here
