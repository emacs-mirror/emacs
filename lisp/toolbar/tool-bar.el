;;; tool-bar.el --- Setting up the tool bar
;;
;; Copyright (C) 2000 Free Software Foundation, Inc.
;;
;; Author: Dave Love <fx@gnu.org>
;; Keywords: mouse frames

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

;; Provides `tool-bar-mode' to control display of the tool -bar and
;; bindings for the global tool bar with convenience functions
;; `tool-bar-add-item' and `tool-bar-add-item-from-menu'.

;;; Code:

;;;###autoload
(define-minor-mode tool-bar-mode
  "Toggle use of the tool bar.
With ARG, display the tool bar if and only if ARG is positive.

See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  nil nil nil
  :global t
  :group 'mouse
  :group 'frames
  (let ((lines (if tool-bar-mode 1 0)))
    ;; Alter existing frames...
    (mapc (lambda (frame)
	    (modify-frame-parameters frame
				     (list (cons 'tool-bar-lines lines))))
	  (frame-list))
    ;; ...and future ones.
    (let ((elt (assq 'tool-bar-lines default-frame-alist)))
      (if elt
	  (setcdr elt lines)
	(add-to-list 'default-frame-alist (cons 'tool-bar-lines lines))))))

(defvar tool-bar-global-map (let ((map (make-sparse-keymap)))
			     (global-set-key [tool-bar] map))
  "Keymap for the tool bar in the global map.")

;;;###autoload
(defun tool-bar-add-item (icon def key &optional map &rest props)
  "Add an item to the tool bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  MAP is the tool bar
keymap in which to define the item; it defaults to
`tool-bar-global-map'.  Remaining arguments PROPS are additional items
to add to the menu item specification.  See Info node `(elisp)Tool
Bar'.  Items are added from left to right.

ICON is the base name of a file cnntaining the image to use.  The
function will try to use first ICON.xpm, then ICON.xbm using
`find-image'.  If PROPS contains `:enable', a `disabled' version of
the icon is generated automatically using the Laplace algorithm (see
Info node `(elisp)Image Descriptors')."
  (let ((image (find-image `((:type xbm :file ,(concat icon ".xbm"))
			     (:type xpm :file ,(concat icon ".xpm"))))))
    (when image
      (unless (image-mask-p image)
	(setq image (append image '(:mask heuristic))))
      (define-key-after (or map tool-bar-global-map) (vector key)
	`(menu-item ,(symbol-name key) ,def :image ,image ,@props)))))

(defun tool-bar-add-item-from-menu (command icon &optional map &rest props)
  "Define tool bar binding for COMMAND using the given ICON in keymap MAP.
The binding of COMMAND is looked up in the menu bar in MAP (default
`global-map') and modified to add an image specification for ICON, which
is looked for as by `tool-bar-add-item'.
MAP must contain appropriate keymaps bound to `[menu-bar]' and
`[tool-bar]'.
PROPS is a list of additional properties to add to the binding."
  (unless map
    (setq map global-map))
  (let* ((menu-bar-map (lookup-key map [menu-bar]))
	 (keys (where-is-internal command menu-bar-map))
	 (tb-map (key-binding [tool-bar] map))
	 (image (find-image `((:type xpm :file ,(concat icon ".xpm"))
			      (:type xbm :file ,(concat icon ".xbm")))))
	 submap key)
    (when image
      ;; We'll pick up the last valid entry in the list of keys if
      ;; there's more than one.
      (dolist (k keys)
	;; We're looking for a binding of the command in a submap of
	;; the menu bar map, so the key sequence must be two or more
	;; long.
	(if (and (vectorp k)
		 (> (length k) 1))
	    (let ((m (lookup-key menu-bar-map (substring k 0 -1)))
		  ;; Last element in the bound key sequence:
		  (kk (aref k (1- (length k)))))
	      (if (and (keymapp m)
		       (symbolp kk))
		  (setq submap m
			key kk)))))
      (when (and (symbolp submap) (boundp submap))
	(setq submap (eval submap)))
      (unless (image-mask-p image)
	(setq image (append image '(:mask heuristic))))
      (define-key-after tb-map (vector key)
	(append (cdr (assq key (cdr submap))) (list :image image) props)))))

;;; Set up some global items.  Additions/deletions up for grabs.

(tool-bar-add-item-from-menu 'save-buffers-kill-emacs "exit")
(tool-bar-add-item-from-menu 'find-file "new")
(tool-bar-add-item-from-menu 'dired "open")
(tool-bar-add-item-from-menu 'kill-this-buffer "close")
(tool-bar-add-item-from-menu 'save-buffer "save" nil
			     :visible '(not (eq 'special (get major-mode
							      'mode-class))))
(tool-bar-add-item-from-menu 'write-file "saveas" nil
			     :visible '(not (eq 'special (get major-mode
							      'mode-class))))
(tool-bar-add-item-from-menu 'undo "undo" nil
			     :visible '(not (eq 'special (get major-mode
							      'mode-class))))
(tool-bar-add-item-from-menu 'kill-region "cut" nil
			     :visible '(not (eq 'special (get major-mode
							      'mode-class))))
(tool-bar-add-item-from-menu 'menu-bar-kill-ring-save "copy")
(tool-bar-add-item-from-menu 'yank "paste" nil
			     :visible '(not (eq 'special (get major-mode
							      'mode-class))))
(tool-bar-add-item-from-menu 'nonincremental-search-forward "search")
;;(tool-bar-add-item-from-menu 'ispell-buffer "spell")

;; There's no icon appropriate for News and we need a command rather
;; than a lambda for Read Mail.
;;(tool-bar-add-item-from-menu 'compose-mail "mail_compose")

(tool-bar-add-item-from-menu 'print-buffer "print")
(tool-bar-add-item "preferences" 'customize 'customize nil
		   :help "Edit preferences (customize)")
(tool-bar-add-item "help"
		   (lambda ()
		     (interactive)
		     (let ((p (mouse-position)))
		       (x-popup-menu (list (list (cadr p) (cddr p)) (car p))
				     menu-bar-help-menu)))
		   'help nil :help "Pop up the Help menu")

(provide 'tool-bar)

;;; tool-bar.el ends here
