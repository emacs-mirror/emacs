;;; tool-bar.el --- setting up the tool bar
;;
;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;;
;; Author: Dave Love <fx@gnu.org>
;; Keywords: mouse frames

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `tool-bar-mode' to control display of the tool-bar and
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

;; The autoload cookie doesn't work when preloading.
;; Deleting it means invoking this command won't work
;; when you are on a tty.  I hope that won't cause too much trouble -- rms.
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
       (modify-all-frames-parameters (list (cons 'tool-bar-lines
						 (if tool-bar-mode 1 0))))
       (if (and tool-bar-mode
		(display-graphic-p))
	   (tool-bar-setup))))

;;;###autoload
;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tool-bar-mode-from-frame (&optional arg)
  "Toggle tool bar on or off, based on the status of the current frame.
See `tool-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tool-bar-mode (if (> (frame-parameter nil 'tool-bar-lines) 0) 0 1))
    (tool-bar-mode arg)))

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
			    :filter tool-bar-make-keymap))

(defun tool-bar-make-keymap (&optional ignore)
  "Generate an actual keymap from `tool-bar-map'.
Its main job is to figure out which images to use based on the display's
color capability and based on the available image libraries."
  (mapcar (lambda (bind)
            (let (image-exp)
              (when (and (eq (car-safe (cdr-safe bind)) 'menu-item)
                         (setq image-exp (plist-get bind :image))
                         (consp image-exp)
                         (not (eq (car image-exp) 'image))
                         (fboundp (car image-exp)))
                (if (not (display-images-p))
                    (setq bind nil)
                  (let ((image (eval image-exp)))
                    (unless (image-mask-p image)
                      (setq image (append image '(:mask heuristic))))
                    (setq bind (copy-sequence bind))
                    (plist-put bind :image image))))
              bind))
	  tool-bar-map))

(defconst tool-bar-find-image-cache (make-hash-table :weakness t :test 'equal))

(defun tool-bar-find-image (specs)
  "Like `find-image' but with caching."
  (or (gethash specs tool-bar-find-image-cache)
      (puthash specs (find-image specs) tool-bar-find-image-cache)))

;;;###autoload
(defun tool-bar-add-item (icon def key &rest props)
  "Add an item to the tool bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if display-color-cells
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
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
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'."
  (let* ((fg (face-attribute 'tool-bar :foreground))
	 (bg (face-attribute 'tool-bar :background))
	 (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			(if (eq bg 'unspecified) nil (list :background bg))))
	 (xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
	 (xpm-lo-spec (if (> (display-color-cells) 256)
			  nil
			(list :type 'xpm :file
                              (concat "low-color/" icon ".xpm"))))
	 (pbm-spec (append (list :type 'pbm :file
                                 (concat icon ".pbm")) colors))
	 (xbm-spec (append (list :type 'xbm :file
                                 (concat icon ".xbm")) colors))
	 (image-exp `(tool-bar-find-image
                      (if (display-color-p)
                          ',(list xpm-lo-spec xpm-spec pbm-spec xbm-spec)
                        ',(list pbm-spec xbm-spec xpm-lo-spec xpm-spec)))))

    (define-key-after map (vector key)
      `(menu-item ,(symbol-name key) ,def :image ,image-exp ,@props))))

;;;###autoload
(defun tool-bar-add-item-from-menu (command icon &optional map &rest props)
  "Define tool bar binding for COMMAND in keymap MAP using the given ICON.
This makes a binding for COMMAND in `tool-bar-map', copying its
binding from the menu bar in MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

MAP must contain appropriate binding for `[menu-bar]' which holds a keymap.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item-from-menu'."
  (apply 'tool-bar-local-item-from-menu command icon
	 (default-value 'tool-bar-map) map props))

;;;###autoload
(defun tool-bar-local-item-from-menu (command icon in-map &optional from-map &rest props)
  "Define local tool bar binding for COMMAND using the given ICON.
This makes a binding for COMMAND in IN-MAP, copying its binding from
the menu bar in FROM-MAP (which defaults to `global-map'), but
modifies the binding by adding an image specification for ICON.  It
finds ICON just like `tool-bar-add-item'.  PROPS are additional
properties to add to the binding.

FROM-MAP must contain appropriate binding for `[menu-bar]' which
holds a keymap."
  (unless from-map
    (setq from-map global-map))
  (let* ((menu-bar-map (lookup-key from-map [menu-bar]))
	 (keys (where-is-internal command menu-bar-map))
	 (fg (face-attribute 'tool-bar :foreground))
	 (bg (face-attribute 'tool-bar :background))
	 (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			(if (eq bg 'unspecified) nil (list :background bg))))
	 (xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
	 (xpm-lo-spec (if (> (display-color-cells) 256)
			  nil
			(list :type 'xpm :file
                              (concat "low-color/" icon ".xpm"))))
	 (pbm-spec (append (list :type 'pbm :file
                                 (concat icon ".pbm")) colors))
	 (xbm-spec (append (list :type 'xbm :file
                                 (concat icon ".xbm")) colors))
	 (image-exp `(tool-bar-find-image
                      (if (display-color-p)
                          ',(list xpm-lo-spec xpm-spec pbm-spec xbm-spec)
                        ',(list pbm-spec xbm-spec xpm-lo-spec xpm-spec))))
	 submap key)
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
    (let ((defn (assq key (cdr submap))))
      (if (eq (cadr defn) 'menu-item)
          (define-key-after in-map (vector key)
            (append (cdr defn) (list :image image-exp) props))
        (setq defn (cdr defn))
        (define-key-after in-map (vector key)
          (let ((rest (cdr defn)))
            ;; If the rest of the definition starts
            ;; with a list of menu cache info, get rid of that.
            (if (and (consp rest) (consp (car rest)))
                (setq rest (cdr rest)))
            (append `(menu-item ,(car defn) ,rest)
                    (list :image image-exp) props)))))))

;;; Set up some global items.  Additions/deletions up for grabs.

(defvar tool-bar-setup nil
  "Set to t if the tool-bar has been set up by `tool-bar-setup'.")

(defun tool-bar-setup (&optional frame)
  (unless tool-bar-setup
    (with-selected-frame (or frame (selected-frame))
      ;; People say it's bad to have EXIT on the tool bar, since users
      ;; might inadvertently click that button.
      ;;(tool-bar-add-item-from-menu 'save-buffers-kill-emacs "exit")
      (tool-bar-add-item-from-menu 'find-file "new")
      (tool-bar-add-item-from-menu 'menu-find-file-existing "open")
      (tool-bar-add-item-from-menu 'dired "diropen")
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
      (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [cut])
				   "cut" nil
				   :visible '(not (eq 'special (get major-mode
								    'mode-class))))
      (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [copy])
				   "copy")
      (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [paste])
				   "paste" nil
				   :visible '(not (eq 'special (get major-mode
								    'mode-class))))
      (tool-bar-add-item-from-menu 'nonincremental-search-forward "search")
      ;;(tool-bar-add-item-from-menu 'ispell-buffer "spell")

      ;; There's no icon appropriate for News and we need a command rather
      ;; than a lambda for Read Mail.
      ;;(tool-bar-add-item-from-menu 'compose-mail "mail/compose")

      (tool-bar-add-item-from-menu 'print-buffer "print")

      ;; tool-bar-add-item-from-menu itself operates on
      ;; (default-value 'tool-bar-map), but when we don't use that function,
      ;; we must explicitly operate on the default value.

      (let ((tool-bar-map (default-value 'tool-bar-map)))
        (tool-bar-add-item "preferences" 'customize 'customize
                           :help "Edit preferences (customize)")

        (tool-bar-add-item "help" (lambda ()
                                    (interactive)
                                    (popup-menu menu-bar-help-menu))
                           'help
                           :help "Pop up the Help menu"))
      (setq tool-bar-setup t))))


(provide 'tool-bar)
;; arch-tag: 15f30f0a-d0d7-4d50-bbb7-f48fd0c8582f
;;; tool-bar.el ends here
