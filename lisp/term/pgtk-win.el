;;; pgtk-win.el --- parse relevant switches and set up for Pure-GTK  -*- lexical-binding: t -*-

;; Copyright (C) 1995, 2001-2020, 2022-2024 Free Software Foundation,
;; Inc.

;; Author: FSF
;; Keywords: terminals

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

;;; Code:

(eval-when-compile (require 'cl-lib))
(unless (featurep 'pgtk)
  (error "%s: Loading pgtk-win.el but not compiled with PGTK."
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'term/common-win)
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)
(require 'pgtk-dnd)

(defvar x-invocation-args)
(defvar x-command-line-resources)
(defvar pgtk-input-file)
(defvar pgtk-use-im-context-on-new-connection)

(declare-function pgtk-use-im-context "pgtkim.c")

(defun pgtk-drag-n-drop (event &optional new-frame force-text)
  "Edit the files listed in the drag-n-drop EVENT.
Switch to a buffer editing the last file dropped."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (arg (car (cdr (cdr event))))
         (type (car arg))
         (data (car (cdr arg)))
         (url-or-string (cond ((eq type 'file)
                               (concat "file:" data))
                              (t data))))
    (set-frame-selected-window nil window)
    (when new-frame
      (select-frame (make-frame)))
    (raise-frame)
    (setq window (selected-window))
    (if force-text
        (dnd-insert-text window 'private data)
      (dnd-handle-one-url window 'private url-or-string))))

(defun pgtk-drag-n-drop-other-frame (event)
  "Edit the files listed in the drag-n-drop EVENT, in other frames.
May create new frames, or reuse existing ones.  The frame editing
the last file dropped is selected."
  (interactive "e")
  (pgtk-drag-n-drop event t))

(defun pgtk-drag-n-drop-as-text (event)
  "Drop the data in EVENT as text."
  (interactive "e")
  (pgtk-drag-n-drop event nil t))

(defun pgtk-drag-n-drop-as-text-other-frame (event)
  "Drop the data in EVENT as text in a new frame."
  (interactive "e")
  (pgtk-drag-n-drop event t t))

(global-set-key [drag-n-drop] 'pgtk-drag-n-drop)

(defun pgtk-suspend-error ()
  "Don't allow suspending if any of the frames are PGTK frames."
  (if (memq 'pgtk (mapcar 'window-system (frame-list)))
      (error "Cannot suspend Emacs while a PGTK GUI frame exists")))

(defvar pgtk-initialized nil
  "Non-nil if pure-GTK windowing has been initialized.")

(declare-function x-handle-args "common-win" (args))
(declare-function x-open-connection "pgtkfns.c"
                  (display &optional xrm-string must-succeed))
(declare-function pgtk-set-resource "pgtkfns.c" (attribute value))

;; Do the actual window system setup here; the above code just defines
;; functions and variables that we use now.
(cl-defmethod window-system-initialization (&context (window-system pgtk)
                                            &optional display)
  "Initialize the PGTK window system.
WINDOW-SYSTEM is, aptly, `pgtk'.
DISPLAY is the name of the display Emacs should connect to."
  (cl-assert (not pgtk-initialized))

  ;; PENDING: not needed?
  (setq command-line-args (x-handle-args command-line-args))

  ;; Make sure we have a valid resource name.
  (when (boundp 'x-resource-name)
    (unless (stringp x-resource-name)
      (let (i)
	(setq x-resource-name (copy-sequence invocation-name))

	;; Change any . or * characters in x-resource-name to hyphens,
	;; so as not to choke when we use it in X resource queries.
	(while (setq i (string-match "[.*]" x-resource-name))
	  (aset x-resource-name i ?-)))))

  ;; Setup the default fontset.
  (create-default-fontset)
  ;; Create the standard fontset.
  (condition-case err
      (create-fontset-from-fontset-spec standard-fontset-spec t)
    (error (display-warning
            'initialization
            (format "Creation of the standard fontset failed: %s" err)
            :error)))

  (x-open-connection (or display
                         x-display-name)
		     x-command-line-resources
		     ;; Exit Emacs with fatal error if this fails and we
		     ;; are the initial display.
                     (= (length (frame-list)) 0))

  (x-apply-session-resources)

  ;; Don't let Emacs suspend under PGTK.
  (add-hook 'suspend-hook 'pgtk-suspend-error)

  (setq pgtk-initialized t))

;; Any display name is OK.
(add-to-list 'display-format-alist '(".*" . pgtk))

(cl-defmethod handle-args-function (args &context (window-system pgtk))
  (x-handle-args args))

(cl-defmethod frame-creation-function (params &context (window-system pgtk))
  (x-create-frame-with-faces params))

(declare-function pgtk-own-selection-internal "pgtkselect.c" (selection value &optional frame))
(declare-function pgtk-disown-selection-internal "pgtkselect.c" (selection &optional terminal))
(declare-function pgtk-selection-owner-p "pgtkselect.c" (&optional selection terminal))
(declare-function pgtk-selection-exists-p "pgtkselect.c" (&optional selection terminal))
(declare-function pgtk-get-selection-internal "pgtkselect.c" (selection-symbol target-type &optional terminal))

(cl-defmethod gui-backend-set-selection (selection value
                                         &context (window-system pgtk))
  (if value (pgtk-own-selection-internal selection value)
    (pgtk-disown-selection-internal selection)))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system pgtk))
  (pgtk-selection-owner-p selection))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system pgtk))
  (pgtk-selection-exists-p selection))

(cl-defmethod gui-backend-get-selection (selection-symbol target-type
                                         &context (window-system pgtk))
  (pgtk-get-selection-internal selection-symbol target-type))


(defvar pgtk-preedit-overlay nil)

(defun pgtk-preedit-text (event)
  "An internal function to display preedit text from input method.

EVENT is a `preedit-text' event."
  (interactive "e")
  (when pgtk-preedit-overlay
    (delete-overlay pgtk-preedit-overlay))
  (setq pgtk-preedit-overlay nil)

  (let ((ovstr "")
        (idx 0)
        atts ov str color face-name)
    (dolist (part (nth 1 event))
      (setq str (car part))
      (setq face-name (intern (format "pgtk-im-%d" idx)))
      (eval
       `(defface ,face-name nil "face of input method preedit"))
      (setq atts nil)
      (when (setq color (cdr-safe (assq 'fg (cdr part))))
        (setq atts (append atts `(:foreground ,color))))
      (when (setq color (cdr-safe (assq 'bg (cdr part))))
        (setq atts (append atts `(:background ,color))))
      (when (setq color (cdr-safe (assq 'ul (cdr part))))
        (setq atts (append atts `(:underline ,color))))
      (face-spec-set face-name `((t . ,atts)))
      (add-text-properties 0 (length str) `(face ,face-name) str)
      (setq ovstr (concat ovstr str))
      (setq idx (1+ idx)))

    (setq ov (make-overlay (point) (point)))
    (overlay-put ov 'before-string ovstr)
    (setq pgtk-preedit-overlay ov)))

(define-key special-event-map [preedit-text] 'pgtk-preedit-text)

(defun pgtk-use-im-context-handler ()
  "Set up input context usage after Emacs initialization."
  (when (eq window-system 'pgtk)
    (pgtk-use-im-context pgtk-use-im-context-on-new-connection)))

(add-hook 'after-init-hook #'pgtk-use-im-context-handler)

(defcustom x-gtk-stock-map
  (mapcar (lambda (arg)
	    (cons (purecopy (car arg)) (purecopy (cdr arg))))
  '(
    ("etc/images/new" . ("document-new" "gtk-new"))
    ("etc/images/open" . ("document-open" "gtk-open"))
    ("etc/images/diropen" . "gtk-directory")
    ("etc/images/close" . ("window-close" "gtk-close"))
    ("etc/images/save" . ("document-save" "gtk-save"))
    ("etc/images/saveas" . ("document-save-as" "gtk-save-as"))
    ("etc/images/undo" . ("edit-undo" "gtk-undo"))
    ("etc/images/cut" . ("edit-cut" "gtk-cut"))
    ("etc/images/copy" . ("edit-copy" "gtk-copy"))
    ("etc/images/paste" . ("edit-paste" "gtk-paste"))
    ("etc/images/search" . ("edit-find" "gtk-find"))
    ("etc/images/print" . ("document-print" "gtk-print"))
    ("etc/images/preferences" . ("preferences-system" "gtk-preferences"))
    ("etc/images/help" . ("help-browser" "gtk-help"))
    ("etc/images/left-arrow" . ("go-previous" "gtk-go-back"))
    ("etc/images/right-arrow" . ("go-next" "gtk-go-forward"))
    ("etc/images/home" . ("go-home" "gtk-home"))
    ("etc/images/jump-to" . ("go-jump" "gtk-jump-to"))
    ("etc/images/index" . ("gtk-search" "gtk-index"))
    ("etc/images/exit" . ("application-exit" "gtk-quit"))
    ("etc/images/cancel" . "gtk-cancel")
    ("etc/images/info" . ("dialog-information" "gtk-info"))
    ("etc/images/bookmark_add" . "n:bookmark_add")
    ;; Used in Gnus and/or MH-E:
    ("etc/images/attach" . ("mail-attachment" "gtk-attach"))
    ("etc/images/connect" . "gtk-connect")
    ("etc/images/contact" . "gtk-contact")
    ("etc/images/delete" . ("edit-delete" "gtk-delete"))
    ("etc/images/describe" . ("document-properties" "gtk-properties"))
    ("etc/images/disconnect" . "gtk-disconnect")
    ;; ("etc/images/exit" . "gtk-exit")
    ("etc/images/lock-broken" . "gtk-lock_broken")
    ("etc/images/lock-ok" . "gtk-lock_ok")
    ("etc/images/lock" . "gtk-lock")
    ("etc/images/next-page" . "gtk-next-page")
    ("etc/images/refresh" . ("view-refresh" "gtk-refresh"))
    ("etc/images/search-replace" . "edit-find-replace")
    ("etc/images/sort-ascending" . ("view-sort-ascending" "gtk-sort-ascending"))
    ("etc/images/sort-column-ascending" . "gtk-sort-column-ascending")
    ("etc/images/sort-criteria" . "gtk-sort-criteria")
    ("etc/images/sort-descending" . ("view-sort-descending"
				     "gtk-sort-descending"))
    ("etc/images/sort-row-ascending" . "gtk-sort-row-ascending")
    ("etc/images/spell" . ("tools-check-spelling" "gtk-spell-check"))
    ("images/gnus/toggle-subscription" . "gtk-task-recurring")
    ("images/mail/compose" . ("mail-message-new" "gtk-mail-compose"))
    ("images/mail/copy" . "gtk-mail-copy")
    ("images/mail/forward" . "gtk-mail-forward")
    ("images/mail/inbox" . "gtk-inbox")
    ("images/mail/move" . "gtk-mail-move")
    ("images/mail/not-spam" . "gtk-not-spam")
    ("images/mail/outbox" . "gtk-outbox")
    ("images/mail/reply-all" . "gtk-mail-reply-to-all")
    ("images/mail/reply" . "gtk-mail-reply")
    ("images/mail/save-draft" . "gtk-mail-handling")
    ("images/mail/send" . ("mail-send" "gtk-mail-send"))
    ("images/mail/spam" . "gtk-spam")
    ;; Used for GDB Graphical Interface
    ("images/gud/break" . "gtk-no")
    ("images/gud/recstart" . ("media-record" "gtk-media-record"))
    ("images/gud/recstop" . ("media-playback-stop" "gtk-media-stop"))
    ;; No themed versions available:
    ;; mail/preview (combining stock_mail and stock_zoom)
    ;; mail/save    (combining stock_mail, stock_save and stock_convert)
    ))
  "How icons for tool bars are mapped to Gtk+ stock items.
Emacs must be compiled with the Gtk+ toolkit for this to have any effect.
A value that begins with n: denotes a named icon instead of a stock icon."
  :version "22.2"
  :type '(choice (repeat
		  (choice symbol
			  (cons (string :tag "Emacs icon")
				(choice (group (string :tag "Named")
					       (string :tag "Stock"))
					(string :tag "Stock/named"))))))
  :group 'pgtk)

(defcustom icon-map-list '(x-gtk-stock-map)
  "A list of alists that map icon file names to stock/named icons.
The alists are searched in the order they appear.  The first match is used.
The keys in the alists are file names without extension and with two directory
components.  For example, to map /usr/share/emacs/22.1.1/etc/images/open.xpm
to stock item gtk-open, use:

  (\"etc/images/open\" . \"gtk-open\")

Themes also have named icons.  To map to one of those, use n: before the name:

  (\"etc/images/diropen\" . \"n:system-file-manager\")

The list elements are either the symbol name for the alist or the
alist itself.

If you don't want stock icons, set the variable to nil."
  :version "22.2"
  :type '(choice (const :tag "Don't use stock icons" nil)
		 (repeat (choice symbol
				 (cons (string :tag "Emacs icon")
				       (string :tag "Stock/named")))))
  :group 'pgtk)

(defconst x-gtk-stock-cache (make-hash-table :weakness t :test 'equal))

(defun x-gtk-map-stock (file)
  "Map icon with file name FILE to a Gtk+ stock name.
This uses `icon-map-list' to map icon file names to stock icon names."
  (when (stringp file)
    (or (gethash file x-gtk-stock-cache)
	(puthash
	 file
	 (save-match-data
	   (let* ((file-sans (file-name-sans-extension file))
		  (key (and (string-match "/\\([^/]+/[^/]+/[^/]+$\\)"
					  file-sans)
			    (match-string 1 file-sans)))
		  (icon-map icon-map-list)
		  elem value)
	     (while (and (null value) icon-map)
	       (setq elem (car icon-map)
		     value (assoc-string (or key file-sans)
					 (if (symbolp elem)
					     (symbol-value elem)
					   elem))
		     icon-map (cdr icon-map)))
	     (and value (cdr value))))
	 x-gtk-stock-cache))))

(declare-function accelerate-menu "pgtkmenu.c" (&optional frame) t)

(defun pgtk-menu-bar-open (&optional frame)
  "Open the menu bar if it is shown.
`popup-menu' is used if it is off."
  (interactive "i")
  (cond
   ((and (not (zerop (or (frame-parameter nil 'menu-bar-lines) 0)))
	 (fboundp 'accelerate-menu))
    (accelerate-menu frame))
   (t
    (popup-menu (mouse-menu-bar-map) last-nonmenu-event))))

(defun pgtk-device-class (name)
  "Return the device class of NAME.
Users should not call this function; see `device-class' instead."
  (cond
   ((not name) nil)
   ((string-match-p "XTEST" name) 'test)
   ((string= "Virtual core pointer" name) 'core-pointer)
   ((string= "Virtual core keyboard" name) 'core-keyboard)
   (t (let ((number (ignore-errors
                      (string-to-number name))))
        (when number
          (cl-case number
            (0 'mouse)
            (1 'pen)
            (2 'eraser)
            (3 'puck)
            (4 'keyboard)
            (5 'touchscreen)
            (6 'touchpad)
            (7 'trackpoint)
            (8 'pad)))))))

(defvaralias 'x-gtk-use-system-tooltips 'use-system-tooltips)


(define-key special-event-map [drag-n-drop] #'pgtk-dnd-handle-drag-n-drop-event)
(add-hook 'after-make-frame-functions #'pgtk-dnd-init-frame)

(provide 'pgtk-win)
(provide 'term/pgtk-win)

;;; pgtk-win.el ends here
