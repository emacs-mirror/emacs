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
;; This file defines a menu of faces (bold, italic, etc) which
;; allows you to set the face used for a region of the buffer.
;; Some faces also have keybindings, which are shown in the menu.  

;;; Installation:
;; Put this file somewhere on emacs's load-path, and put
;;   (require 'facemenu)
;; in your .emacs file.

;;; Usage:
;; Selecting a face from the menu or typing the keyboard equivalent
;; will change the region to use that face.  
;; If you use transient-mark-mode and the region is not active, the
;; face will be remembered and used for the next insertion.  It will
;; be forgotten if you move point or make other modifications before
;; inserting or typing anything.
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
;; Only works with Emacs 19.23 and later.
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

(defvar facemenu-key "\M-s"
  "Prefix to use for facemenu commands.")

(defvar facemenu-keymap nil
  "Map for keybindings of face commands.
If nil, `facemenu-update' will create one.
`Facemenu-update' also fills in the keymap according to the bindings
requested in facemenu-keybindings.")

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

(defvar facemenu-next nil) ; set when we are going to set a face on next char.
(defvar facemenu-loc nil)

(defun facemenu-update ()
  "Add or update the \"Face\" menu in the menu bar."
  (interactive)
  
  ;; Set up keymaps
  (fset 'facemenu-menu (setq facemenu-menu (make-sparse-keymap "Face")))
  (if (null facemenu-keymap)
      (fset 'facemenu-keymap 
	    (setq facemenu-keymap (make-sparse-keymap "Set face"))))
  (if facemenu-key
      (define-key global-map facemenu-key facemenu-keymap))

  ;; Define basic keys
  ;; We construct this list structure explicitly because a quoted constant
  ;; would be pure.
  (define-key facemenu-menu [other]     (cons "Other..." 'facemenu-set-face))
  (define-key facemenu-menu [sep2]      (list "---Special---"))
  (define-key facemenu-menu [invisible] (cons "Invisible"
					  'facemenu-set-invisible))
  (define-key facemenu-menu [read-only] (cons "Read-Only"
					      'facemenu-set-read-only))
  (define-key facemenu-menu [remove]    (cons "Remove Properties"
					      'facemenu-remove-all))
  (define-key facemenu-menu [sep1]      (list "-------------"))
  (define-key facemenu-menu [display]   (cons "Display" 'list-faces-display))
  (define-key facemenu-menu [update]    (cons "Update Menu" 'facemenu-update))

  ;; Define commands for face-changing
  (facemenu-iterate
   (function 
    (lambda (f)
      (let ((face (car f))
	    (name (symbol-name (car f)))
	    (key  (cdr f)))
	(cond ((memq face facemenu-unlisted-faces)
	       nil)
	      ((null key) (define-key facemenu-menu (vector face) 
			     (cons name 'facemenu-set-face-from-menu)))
	      (t (let ((function (intern (concat "facemenu-set-" name))))
		   (fset function
			 (` (lambda () (interactive)
			      (facemenu-set-face (quote (, face))))))
		   (define-key facemenu-keymap key (cons name function))
		   (define-key facemenu-menu key (cons name function))))))
      nil))
   (facemenu-complete-face-list facemenu-keybindings))

  (define-key global-map (vector 'menu-bar 'Face) 
    (cons "Face" facemenu-menu)))

; We'd really like to name the menu items as follows,
; but we can't since menu entries don't display text properties (yet?)
; (let ((s (copy-sequence (symbol-name face))))
;    (put-text-property 0 (1- (length s)) 
;                       'face face s)
;   s)

;;;###autoload
(defun facemenu-set-face (face &optional start end)
  "Set the face of the region or next character typed.
The face to be used is prompted for.  
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (read-face-name "Use face: ")))
  (if mark-active
      (put-text-property (or start (region-beginning))
			 (or end (region-end))
			 'face face)
    (setq facemenu-next face facemenu-loc (point))))

(defun facemenu-set-face-from-menu (face start end)
  "Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (let ((keys (this-command-keys)))
		 (list (elt keys (1- (length keys)))
		       (if mark-active (region-beginning))
		       (if mark-active (region-end)))))
  (if start 
      (put-text-property start end 'face face)
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

(defun facemenu-after-change (begin end old-length)
  "May set the face of just-inserted text to user's request.
This only happens if the change is an insertion, and
`facemenu-set-face[-from-menu]' was called with point at the
beginning of the insertion."
  (if (null facemenu-next)		; exit immediately if no work
      nil
    (if (and (= 0 old-length)		; insertion
	     (= facemenu-loc begin))	; point wasn't moved in between
	(put-text-property begin end 'face facemenu-next))
    (setq facemenu-next nil)))


(defun facemenu-complete-face-list (&optional oldlist)
  "Return alist of all faces that are look different.
Starts with given LIST of faces, and adds elements only if they display 
differently from any face already on the list.
The original LIST will end up at the end of the returned list, in reverse 
order.  The elements added will have null cdrs."
  (let ((list nil))
    (facemenu-iterate 
     (function
      (lambda (item)
	(if (internal-find-face (car item))
	    (setq list (cons item list)))
	nil))
     oldlist)
    (facemenu-iterate 
     (function
      (lambda (new-face) 
	(if (not (facemenu-iterate 
		  (function 
		   (lambda (item) (face-equal (car item) new-face t)))
		  list))
	    (setq list (cons (cons new-face nil) list)))
	nil))
     (nreverse (face-list)))
    list))

(defun facemenu-iterate (func iterate-list)
  "Apply FUNC to each element of LIST until one returns non-nil.
Returns the non-nil value it found, or nil if all were nil."
  (while (and iterate-list (not (funcall func (car iterate-list))))
    (setq iterate-list (cdr iterate-list)))
  (car iterate-list))

(facemenu-update)
(add-hook 'menu-bar-final-items 'Face)
(add-hook 'after-change-functions 'facemenu-after-change)

;;; facemenu.el ends here

