;;; tool-bar.el --- setting up the tool bar
;;
;; Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

;; The normal global binding for [tool-bar] (below) uses the value of
;; `tool-bar-map' as the actual keymap to define the tool bar.  Modes
;; may either bind items under the [tool-bar] prefix key of the local
;; map to add to the global bar or may set `tool-bar-map'
;; buffer-locally to override it.  (Some items are removed from the
;; global bar in modes which have `special' as their `mode-class'
;; property.)

;; Todo: Somehow make tool bars easily customizable by the naive?

;;; Code:

;;;###autoload
(define-minor-mode tool-bar-mode
  "Toggle use of the tool bar.
With numeric ARG, display the tool bar if and only if ARG is positive.

See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  :init-value nil
  :global t
  :group 'mouse
  :group 'frames
  (and (display-images-p)
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
	     (add-to-list 'default-frame-alist (cons 'tool-bar-lines lines)))))
       (if (and tool-bar-mode
		(display-graphic-p)
		(= 1 (length (default-value 'tool-bar-map)))) ; not yet setup
	   (tool-bar-setup))))

;;;###autoload
;; We want to pretend the toolbar by standard is on, as this will make
;; customize consider disabling the toolbar a customization, and save
;; that.  We could do this for real by setting :init-value above, but
;; that would turn on the toolbar in MS Windows where it is currently
;; useless, and it would overwrite disabling the tool bar from X
;; resources.  If anyone want to implement this in a cleaner way,
;; please do so.
;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-21.
(put 'tool-bar-mode 'standard-value '(t))

(defvar tool-bar-map (make-sparse-keymap)
  "Keymap for the tool bar.
Define this locally to override the global tool bar.")

(global-set-key [tool-bar]
		'(menu-item "tool bar" ignore
			    :filter (lambda (ignore) tool-bar-map)))

;;;###autoload
(defun tool-bar-add-item (icon def key &rest props)
  "Add an item to the tool bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item'."
  (apply 'tool-bar-local-item icon def key tool-bar-map props))

;;;###autoload
(defun tool-bar-local-item (icon def key map &rest props)
  "Add an item to the tool bar in map MAP.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'."
  (let* ((fg (face-attribute 'tool-bar :foreground))
	 (bg (face-attribute 'tool-bar :background))
	 (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			(if (eq bg 'unspecified) nil (list :background bg))))
	 (image (find-image
		(if (display-color-p)
		    (list (list :type 'xpm :file (concat icon ".xpm"))
			  (append (list :type 'pbm :file (concat icon ".pbm"))
				  colors)
			  (append (list :type 'xbm :file (concat icon ".xbm"))
				  colors))
		  (list (append (list :type 'pbm :file (concat icon ".pbm"))
				colors)
			(append (list :type 'xbm :file (concat icon ".xbm"))
				colors)
			(list :type 'xpm :file (concat icon ".xpm")))))))
    (when (and (display-images-p) image)
      (unless (image-mask-p image)
	(setq image (append image '(:mask heuristic))))
      (define-key-after map (vector key)
	`(menu-item ,(symbol-name key) ,def :image ,image ,@props)))))

;;;###autoload
(defun tool-bar-add-item-from-menu (command icon &optional map &rest props)
  "Define tool bar binding for COMMAND using the given ICON in keymap MAP.
This makes a binding for COMMAND in `tool-bar-map', copying its
binding from the menu bar in MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

MAP must contain appropriate binding for `[menu-bar]' which holds a keymap.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item'."
  (apply 'tool-bar-local-item-from-menu command icon tool-bar-map map props))

;;;###autoload
(defun tool-bar-local-item-from-menu (command icon in-map &optional from-map &rest props)
  "Define tool bar binding for COMMAND using the given ICON in keymap MAP.
This makes a binding for COMMAND in IN-MAP, copying its binding from
the menu bar in FROM-MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

MAP must contain appropriate binding for `[menu-bar]' which holds a keymap."
  (unless from-map
    (setq from-map global-map))
  (let* ((menu-bar-map (lookup-key from-map [menu-bar]))
	 (keys (where-is-internal command menu-bar-map))
	 (fg (face-attribute 'tool-bar :foreground))
	 (bg (face-attribute 'tool-bar :background))
	 (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			(if (eq bg 'unspecified) nil (list :background bg))))
	 (spec (if (display-color-p)
		   (list (list :type 'xpm :file (concat icon ".xpm"))
			 (append (list :type 'pbm :file (concat icon ".pbm"))
				       colors)
			 (append (list :type 'xbm :file (concat icon ".xbm"))
				       colors))
		 (list (append (list :type 'pbm :file (concat icon ".pbm"))
				     colors)
		       (append (list :type 'xbm :file (concat icon ".xbm"))
				     colors)
		       (list :type 'xpm :file (concat icon ".xpm")))))
	 (image (find-image spec))
	 submap key)
    (when (and (display-images-p) image)
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
      (let ((defn (assq key (cdr submap))))
	(if (eq (cadr defn) 'menu-item)
	    (define-key-after in-map (vector key)
	      (append (cdr defn) (list :image image) props))
	  (setq defn (cdr defn))
	  (define-key-after in-map (vector key)
	    (append `(menu-item ,(car defn) ,(cddr defn))
		    (list :image image) props)))))))

;;; Set up some global items.  Additions/deletions up for grabs.

(defun tool-bar-setup ()
  ;; People say it's bad to have EXIT on the tool bar, since users
  ;; might inadvertently click that button.
  ;;(tool-bar-add-item-from-menu 'save-buffers-kill-emacs "exit")
  (tool-bar-add-item-from-menu 'find-file "new")
  (tool-bar-add-item-from-menu 'dired "open")
  (tool-bar-add-item-from-menu 'kill-this-buffer "close")
  (tool-bar-add-item-from-menu 'save-buffer "save" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
  (tool-bar-add-item-from-menu 'write-file "saveas" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
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
  (tool-bar-add-item "preferences" 'customize 'customize
		     :help "Edit preferences (customize)")

  (tool-bar-add-item "help" (lambda ()
			      (interactive)
			      (popup-menu menu-bar-help-menu))
		     'help
		     :help "Pop up the Help menu")
  )

(provide 'tool-bar)

;;; tool-bar.el ends here
