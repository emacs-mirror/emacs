;;; tool-bar.el --- setting up the tool bar  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2026 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: mouse frames
;; Package: emacs

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
  "Toggle the tool bar in all graphical frames (Tool Bar mode).

See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  :init-value t
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable tool-bar-mode
  (let ((val (if tool-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'tool-bar-lines val))
    ;; If the user has given `default-frame-alist' a `tool-bar-lines'
    ;; parameter, replace it.
    (if (assq 'tool-bar-lines default-frame-alist)
	(setq default-frame-alist
	      (cons (cons 'tool-bar-lines val)
		    (assq-delete-all 'tool-bar-lines
				     default-frame-alist)))))
  (and tool-bar-mode
       (= 1 (length (default-value 'tool-bar-map))) ; not yet setup
       (tool-bar-setup)))

;;;###autoload
;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tool-bar-mode-from-frame (&optional arg)
  "Toggle tool bar on or off, based on the status of the current frame.
See `tool-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tool-bar-mode (if (> (frame-parameter nil 'tool-bar-lines) 0) 0 1))
    (tool-bar-mode arg)))

(defvar-keymap tool-bar-map
  :doc "Keymap for the tool bar.

To override the global tool bar, define this variable
buffer-locally and add the items you want to it with
`tool-bar-add-item', `tool-bar-add-item-from-menu' and related
functions.")

(defvar secondary-tool-bar-map nil
  "Optional secondary keymap for the tool bar.

If non-nil, tool bar items defined within this map are displayed
in a line below the tool bar if the `tool-bar-position' frame
parameter is set to `top', and above the tool bar it is set to
`bottom'.")

(global-set-key [tool-bar]
                '(menu-item "tool bar" ignore
                            :filter tool-bar-make-keymap))

(declare-function image-mask-p "image.c" (spec &optional frame))

(defconst tool-bar-keymap-cache (make-hash-table :test #'equal))

(defsubst tool-bar--cache-key ()
  (cons (frame-terminal)
        (sxhash-eq (if tool-bar-always-show-default (default-value 'tool-bar-map)
                     tool-bar-map))))

(defsubst tool-bar--secondary-cache-key ()
  (cons (frame-terminal) (sxhash-eq secondary-tool-bar-map)))

(defun tool-bar--flush-cache ()
  "Remove all cached entries that refer to the current `tool-bar-map'."
  (let ((id (sxhash-eq tool-bar-map))
        (secondary-id (and secondary-tool-bar-map
                           (sxhash-eq secondary-tool-bar-map)))
        (entries nil))
    (maphash (lambda (k _)
               (when (or (equal (cdr k) id)
                         (equal (cdr k) secondary-id))
                 (push k entries)))
             tool-bar-keymap-cache)
    (dolist (k entries)
      (remhash k tool-bar-keymap-cache))))

(defun tool-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tool-bar-map'.
If `secondary-tool-bar-map' is non-nil, take it into account as well.
Its main job is to figure out which images to use based on the display's
color capability and based on the available image libraries."
  (let* ((key (tool-bar--cache-key))
         (base-keymap
          (or (gethash key tool-bar-keymap-cache)
              (setf (gethash key tool-bar-keymap-cache)
                    (tool-bar-make-keymap-1))))
        (secondary-keymap
         (and secondary-tool-bar-map
              (or (gethash (tool-bar--secondary-cache-key)
                           tool-bar-keymap-cache)
                  (setf (gethash (tool-bar--secondary-cache-key)
                                 tool-bar-keymap-cache)
                        (tool-bar-make-keymap-1
                         secondary-tool-bar-map))))))
    (if secondary-keymap
        (or (ignore-errors
              (progn
                ;; Determine the value of the `tool-bar-position' frame
                ;; parameter.
                (let ((position (frame-parameter nil 'tool-bar-position)))
                  (cond ((eq position 'top)
                         ;; Place `base-keymap' above `secondary-keymap'.
                         (append base-keymap (list (list (gensym)
                                                         'menu-item
                                                         "" 'ignore
                                                         :wrap t))
                                 (cdr secondary-keymap)))
                        ((eq position 'bottom)
                         ;; Place `secondary-keymap' above `base-keymap'.
                         (append secondary-keymap (list (list (gensym)
                                                              'menu-item
                                                              "" 'ignore
                                                              :wrap t))
                                 (cdr base-keymap)))
                        ;; If the tool bar position isn't known, don't
                        ;; display the secondary keymap at all.
                        (t base-keymap)))))
            ;; If combining both keymaps fails, return the base
            ;; keymap.
            base-keymap)
      base-keymap)))

;; This function should return binds even if images can not be
;; displayed so the tool bar can still be displayed on terminals.
(defun tool-bar-make-keymap-1 (&optional map)
  "Generate an actual keymap from `tool-bar-map', without caching.
MAP is either a keymap to use as a source for menu items, or nil,
in which case the value of `tool-bar-map' is used instead."
  (mapcar (lambda (bind)
            (let (image-exp plist)
              (when (and (eq (car-safe (cdr-safe bind)) 'menu-item)
			 ;; For the format of menu-items, see node
			 ;; `Extended Menu Items' in the Elisp manual.
			 (setq plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					     bind))
			 (setq image-exp (plist-get plist :image))
			 (consp image-exp)
			 (not (eq (car image-exp) 'image))
			 (fboundp (car image-exp)))
		(let ((image (and (display-images-p)
                                  (eval image-exp))))
		  (unless (and image (image-mask-p image))
		    (setq image (append image '(:mask heuristic))))
		  (setq bind (copy-sequence bind)
			plist (nthcdr (if (consp (nth 4 bind)) 5 4)
				      bind))
		  (plist-put plist :image image)))
	      bind))
	  (or map
              (if tool-bar-always-show-default (default-value 'tool-bar-map)
                tool-bar-map))))

;;;###autoload
(defun tool-bar-add-item (icon def key &rest props)
  "Add an item to the tool bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tool Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'.

Use this function only to make bindings in the global value of `tool-bar-map'.
To define items in any other map, use `tool-bar-local-item'."
  (apply #'tool-bar-local-item icon def key tool-bar-map props)
  (tool-bar--flush-cache))

(defun tool-bar--image-expression (icon)
  "Return an expression that evaluates to an image spec for ICON."
  (let* ((fg (face-attribute 'tool-bar :foreground))
	 (bg (face-attribute 'tool-bar :background))
	 (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			(if (eq bg 'unspecified) nil (list :background bg))))
	 (xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
	 (xpm-lo-spec (list :type 'xpm :file
			    (concat "low-color/" icon ".xpm")))
	 (pbm-spec (append (list :type 'pbm :file
                                 (concat icon ".pbm")) colors))
	 (xbm-spec (append (list :type 'xbm :file
                                 (concat icon ".xbm")) colors)))
    `(find-image (cond ((not (display-color-p))
			',(list pbm-spec xbm-spec xpm-lo-spec xpm-spec))
		       ((< (display-color-cells) 256)
			',(list xpm-lo-spec xpm-spec pbm-spec xbm-spec))
		       (t
			',(list xpm-spec pbm-spec xbm-spec)))
                 t)))

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
  (let* ((image-exp (tool-bar--image-expression icon)))
    (define-key-after map (vector key)
      `(menu-item ,(symbol-name key) ,def :image ,image-exp ,@props))
    (tool-bar--flush-cache)
    (force-mode-line-update)))

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
  (apply #'tool-bar-local-item-from-menu command icon
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
	 (image-exp (tool-bar--image-expression icon))
	 submap key)
    ;; We'll pick up the last valid entry in the list of keys if
    ;; there's more than one.
    ;; FIXME: Aren't they *all* "valid"??  --Stef
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
                    (list :image image-exp) props))))
      (tool-bar--flush-cache)
      (force-mode-line-update))))

;;; Set up some global items.  Additions/deletions up for grabs.

(defun tool-bar-setup ()
  (setq tool-bar-separator-image-expression
	(tool-bar--image-expression "separator"))
  (tool-bar-add-item-from-menu 'find-file "new" nil :label "New File"
			       :vert-only t)
  (tool-bar-add-item-from-menu 'menu-find-file-existing "open" nil
			       :label "Open" :vert-only t)
  (tool-bar-add-item-from-menu 'dired "diropen" nil :vert-only t)
  (tool-bar-add-item-from-menu 'kill-this-buffer "close" nil :vert-only t)
  (tool-bar-add-item-from-menu 'save-buffer "save" nil
			       :label "Save")
  (define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)
  (tool-bar-add-item-from-menu 'undo "undo" nil)
  (define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			       "cut" nil :vert-only t)
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			       "copy" nil :vert-only t)
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			       "paste" nil :vert-only t)
  (define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)
  (tool-bar-add-item-from-menu 'isearch-forward "search"
			       nil :label "Search" :vert-only t)
  ;;(tool-bar-add-item-from-menu 'ispell-buffer "spell")

  ;; There's no icon appropriate for News and we need a command rather
  ;; than a lambda for Read Mail.
  ;;(tool-bar-add-item-from-menu 'compose-mail "mail/compose")

  ;; Help button on a tool bar is rather non-standard...
  ;; (let ((tool-bar-map (default-value 'tool-bar-map)))
  ;;   (tool-bar-add-item "help" (lambda ()
  ;; 				(interactive)
  ;; 				(popup-menu menu-bar-help-menu))
  ;; 		       'help
  ;; 		       :help "Pop up the Help menu"))
)

(if (featurep 'move-toolbar)
    (defcustom tool-bar-position 'top
      "Specify on which side the tool bar shall be.
Possible values are `top' (tool bar on top), `bottom' (tool bar at
bottom), `left' (tool bar on left) and `right' (tool bar on right).
This option takes effect only on graphical frames, the values `left' and
`right' only if Emacs was built with GTK, and `bottom' only on systems
besides Nextstep.  Customize `tool-bar-mode' if you want to show or hide
the tool bar."
      :version "24.1"
      :type '(choice (const top)
		     (const bottom)
		     (const left)
		     (const right))
      :group 'frames
      :initialize 'custom-initialize-default
      :set (lambda (sym val)
	     (set-default sym val)
	     (modify-all-frames-parameters
	      (list (cons 'tool-bar-position val))))))

(defcustom tool-bar-always-show-default nil
  "If non-nil, `tool-bar-mode' only shows the default tool bar.
This works well when also using `global-window-tool-bar-mode' to
display buffer-specific tool bars."
  :type 'boolean
  :group 'frames
  :group 'mouse
  :version "30.1")



;; Modifier bar mode.
;; This displays a small tool bar containing modifier keys
;; above or below the main tool bar itself.

(defvar modifier-bar-modifier-list nil
  "List of modifiers that are currently applied.
Each symbol in this list represents a modifier button that has
been pressed as part of decoding this key sequence.")

(declare-function set-text-conversion-style "textconv.c")

;; These functions are very similar to their counterparts in
;; simple.el, but allow combining multiple modifier buttons together.

(defun tool-bar-apply-modifiers (event modifiers)
  "Apply the specified list of MODIFIERS to EVENT.
MODIFIERS must be a list containing only the symbols `alt',
`super', `hyper', `shift', `control' and `meta'.
Return EVENT with the specified modifiers applied."
  (dolist (modifier modifiers)
    (cond
     ((eq modifier 'alt)
      (setq event (event-apply-modifier event 'alt 22 "A-")))
     ((eq modifier 'super)
      (setq event (event-apply-modifier event 'super 23 "s-")))
     ((eq modifier 'hyper)
      (setq event (event-apply-modifier event 'hyper 24 "H-")))
     ((eq modifier 'shift)
      (setq event (event-apply-modifier event 'shift 25 "S-")))
     ((eq modifier 'control)
      (setq event (event-apply-modifier event 'control 26 "C-")))
     ((eq modifier 'meta)
      (setq event (event-apply-modifier event 'meta 27 "M-")))))
  event)

(defvar overriding-text-conversion-style)

(defun modifier-bar-button (init-modifier-list)
  "Decode the key sequence associated with a modifier bar button.
INIT-MODIFIER-LIST is a list of one symbol describing the button
being pressed.

Bind `modifier-bar-modifier-list' to INIT-MODIFIER-LIST.  Read
events, adding each subsequent modifier bar event's associated
modifier to that list while updating the tool bar to disable
buttons that were pressed.  Return any other event read with all
modifier keys read applied.

Temporarily disable text conversion and display the on screen
keyboard while doing so."
  ;; Save the previously used text conversion style.
  (let ((old-text-conversion-style text-conversion-style)
        ;; Clear the list of modifiers currently pressed.
        (modifier-bar-modifier-list init-modifier-list))
    ;; Disable text conversion.
    (when (fboundp 'set-text-conversion-style)
      (set-text-conversion-style nil))
    (unwind-protect
        (progn
          ;; Display the on screen keyboard.
          (frame-toggle-on-screen-keyboard nil nil)
          ;; Update the tool bar to disable this modifier key.
          (force-mode-line-update)
          (let* ((modifiers init-modifier-list) event1
                 (overriding-text-conversion-style nil)
                 (event (read-event)))
            ;; Combine any more modifier key presses.
            (while (eq event 'tool-bar)
              (setq event1 (event-basic-type (read-event)))
              ;; Reject unknown tool bar events.
              (unless (memq event1 '(alt super hyper shift control meta))
                (user-error "Unknown tool-bar event %s" event1))
              ;; If `event' is the name of a modifier key, apply that
              ;; modifier key as well.
              (unless (memq event1 modifiers)
                (push event1 modifiers)
                ;; This list is used to check which tool bar buttons
                ;; need to be enabled.
                (push event1 modifier-bar-modifier-list))
              ;; Update the tool bar to disable the modifier button
              ;; that was read.
              (force-mode-line-update)
              (redisplay)
              ;; Read another event.
              (setq event (read-event)))
            ;; EVENT is a keyboard event to which the specified list of
            ;; modifier keys should be applied.
            (vector (tool-bar-apply-modifiers event modifiers))))
      ;; Re-enable text conversion if necessary.
      (unless (or (not (fboundp 'set-text-conversion-style))
                  (eq old-text-conversion-style text-conversion-style))
        (set-text-conversion-style old-text-conversion-style t))
      ;; Re-enable all modifier bar buttons which may have been
      ;; disabled.
      (force-mode-line-update))))

(defun tool-bar-event-apply-alt-modifier (_ignore-prompt)
  "Like `event-apply-alt-modifier'.
However, take additional modifier tool bar items into account;
apply any extra modifiers bound to subsequent `tool-bar' events."
  (modifier-bar-button '(alt)))

(defun tool-bar-event-apply-super-modifier (_ignore-prompt)
  "Like `event-apply-super-modifier'.
However, take additional modifier tool bar items into account;
apply any extra modifiers bound to subsequent `tool-bar' events."
  (modifier-bar-button '(super)))

(defun tool-bar-event-apply-hyper-modifier (_ignore-prompt)
  "Like `event-apply-hyper-modifier'.
However, take additional modifier tool bar items into account;
apply any extra modifiers bound to subsequent `tool-bar' events."
  (modifier-bar-button '(hyper)))

(defun tool-bar-event-apply-shift-modifier (_ignore-prompt)
  "Like `event-apply-shift-modifier'.
However, take additional modifier tool bar items into account;
apply any extra modifiers bound to subsequent `tool-bar' events."
  (modifier-bar-button '(shift)))

(defun tool-bar-event-apply-control-modifier (_ignore-prompt)
  "Like `event-apply-control-modifier'.
However, take additional modifier tool bar items into account;
apply any extra modifiers bound to subsequent `tool-bar' events."
  (modifier-bar-button '(control)))

(defun tool-bar-event-apply-meta-modifier (_ignore-prompt)
  "Like `event-apply-meta-modifier'.
However, take additional modifier tool bar items into account;
apply any extra modifiers bound to subsequent `tool-bar' events."
  (modifier-bar-button '(meta)))

(defun modifier-bar-available-p (modifier)
  "Return whether the modifier button for MODIFIER should be enabled.
Return t if MODIFIER has not yet been selected as part of
decoding the current key sequence, nil otherwise."
  (not (memq modifier modifier-bar-modifier-list)))

(define-minor-mode modifier-bar-mode
  "Toggle display of the key-modifier tool bar.

When enabled, a small tool bar will be displayed in addition to the
regular tool bar, containing buttons for key modifiers such as
Ctrl, Shift, Alt, etc.  This is useful on terminals whose keyboard
has no keys for these modifiers, such as smartphones and other
devices with small keyboards."
  :init-value nil
  :global t
  :group 'tool-bar
  (if modifier-bar-mode
      (progn
        (setq secondary-tool-bar-map
              ;; The commands specified in the menu items here are not
              ;; used.  Instead, Emacs relies on each of the tool bar
              ;; events being specified in `input-decode-map'.
              `(keymap (control menu-item "Control Key"
                                event-apply-control-modifier
                                :help "Add Control modifier to the following event"
                                :image ,(tool-bar--image-expression "ctrl")
                                :enable (modifier-bar-available-p 'control))
                       (shift menu-item "Shift Key"
                              event-apply-shift-modifier
                              :help "Add Shift modifier to the following event"
                              :image ,(tool-bar--image-expression "shift")
                              :enable (modifier-bar-available-p 'shift))
                       (meta menu-item "Meta Key"
                             event-apply-meta-modifier
                             :help "Add Meta modifier to the following event"
                             :image ,(tool-bar--image-expression "meta")
                             :enable (modifier-bar-available-p 'meta))
                       (alt menu-item "Alt Key"
                            event-apply-alt-modifier
                            :help "Add Alt modifier to the following event"
                            :image ,(tool-bar--image-expression "alt")
                            :enable (modifier-bar-available-p 'alt))
                       (super menu-item "Super Key"
                              event-apply-super-modifier
                              :help "Add Super modifier to the following event"
                              :image ,(tool-bar--image-expression "super")
                              :enable (modifier-bar-available-p 'super))
                       (hyper menu-item "Hyper Key"
                              event-apply-hyper-modifier
                              :help "Add Hyper modifier to the following event"
                              :image ,(tool-bar--image-expression "hyper")
                              :enable (modifier-bar-available-p 'hyper))))
        (define-key input-decode-map [tool-bar control]
                    #'tool-bar-event-apply-control-modifier)
        (define-key input-decode-map [tool-bar shift]
                    #'tool-bar-event-apply-shift-modifier)
        (define-key input-decode-map [tool-bar meta]
                    #'tool-bar-event-apply-meta-modifier)
        (define-key input-decode-map [tool-bar alt]
                    #'tool-bar-event-apply-alt-modifier)
        (define-key input-decode-map [tool-bar super]
                    #'tool-bar-event-apply-super-modifier)
        (define-key input-decode-map [tool-bar hyper]
                    #'tool-bar-event-apply-hyper-modifier))
    (setq secondary-tool-bar-map nil))
  ;; Update the mode line now.
  (force-mode-line-update t))

(provide 'tool-bar)

;;; tool-bar.el ends here
