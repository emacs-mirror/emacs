;;; haiku-win.el --- set up windowing on Haiku -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

;; Support for using Haiku's BeOS derived windowing system.

;;; Code:

(eval-when-compile (require 'cl-lib))
(unless (featurep 'haiku)
  (error "%s: Loading haiku-win without having Haiku"
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(add-to-list 'display-format-alist '(".*" . haiku))

;;;; Command line argument handling.

(defvar x-invocation-args)
(defvar x-command-line-resources)

(defvar haiku-initialized)
(defvar haiku-signal-invalid-refs)
(defvar haiku-drag-track-function)
(defvar haiku-allowed-ui-colors)

(defvar haiku-dnd-selection-value nil
  "The local value of the special `XdndSelection' selection.")

(defvar haiku-dnd-selection-converters '((STRING . haiku-dnd-convert-string)
                                         (FILE_NAME . haiku-dnd-convert-file-name)
                                         (text/uri-list . haiku-dnd-convert-text-uri-list))
  "Alist of X selection types to functions that act as selection converters.
The functions should accept a single argument VALUE, describing
the value of the drag-and-drop selection, and return a list of
two elements TYPE and DATA, where TYPE is a string containing the
MIME type of DATA, and DATA is a unibyte string, or nil if the
data could not be converted.

DATA may also be a list of items; that means to add every
individual item in DATA to the serialized message, instead of
DATA in its entirety.

DATA can optionally have a text property `type', which specifies
the type of DATA inside the system message (see the doc string of
`haiku-drag-message' for more details).  If DATA is a list, then
that property is obtained from the first element of DATA.")

(defvar haiku-normal-selection-encoders '(haiku-select-encode-xstring
                                          haiku-select-encode-utf-8-string
                                          haiku-select-encode-file-name)
  "List of functions which act as selection encoders.
These functions accept two arguments SELECTION and VALUE, and
return an association appropriate for a serialized system
message (or nil if VALUE is not applicable to the encoder) that
will be put into the system selection SELECTION.  VALUE is the
content that is being put into the selection by
`gui-set-selection'.  See the doc string of `haiku-drag-message'
for more details on the structure of the associations.")

;; This list has to be set correctly, otherwise Emacs will crash upon
;; encountering an invalid color.
(setq haiku-allowed-ui-colors
      ["B_PANEL_BACKGROUND_COLOR" "B_MENU_BACKGROUND_COLOR"
       "B_WINDOW_TAB_COLOR" "B_KEYBOARD_NAVIGATION_COLOR"
       "B_DESKTOP_COLOR" "B_MENU_SELECTED_BACKGROUND_COLOR"
       "B_MENU_ITEM_TEXT_COLOR" "B_MENU_SELECTED_ITEM_TEXT_COLOR"
       "B_MENU_SELECTED_BORDER_COLOR" "B_PANEL_TEXT_COLOR"
       "B_DOCUMENT_BACKGROUND_COLOR" "B_DOCUMENT_TEXT_COLOR"
       "B_CONTROL_BACKGROUND_COLOR" "B_CONTROL_TEXT_COLOR"
       "B_CONTROL_BORDER_COLOR" "B_CONTROL_HIGHLIGHT_COLOR"
       "B_NAVIGATION_PULSE_COLOR" "B_SHINE_COLOR"
       "B_SHADOW_COLOR" "B_TOOLTIP_BACKGROUND_COLOR"
       "B_TOOLTIP_TEXT_COLOR" "B_WINDOW_TEXT_COLOR"
       "B_WINDOW_INACTIVE_TAB_COLOR" "B_WINDOW_INACTIVE_TEXT_COLOR"
       "B_WINDOW_BORDER_COLOR" "B_WINDOW_INACTIVE_BORDER_COLOR"
       "B_CONTROL_MARK_COLOR" "B_LIST_BACKGROUND_COLOR"
       "B_LIST_SELECTED_BACKGROUND_COLOR" "B_LIST_ITEM_TEXT_COLOR"
       "B_LIST_SELECTED_ITEM_TEXT_COLOR" "B_SCROLL_BAR_THUMB_COLOR"
       "B_LINK_TEXT_COLOR" "B_LINK_HOVER_COLOR"
       "B_LINK_VISITED_COLOR" "B_LINK_ACTIVE_COLOR"
       "B_STATUS_BAR_COLOR" "B_SUCCESS_COLOR" "B_FAILURE_COLOR"])

(defvar x-colors)
;; Also update `x-colors' to take that into account.
(setq x-colors (append haiku-allowed-ui-colors x-colors))

(defun haiku-selection-bounds (value)
  "Return bounds of selection value VALUE.
The return value is a list (BEG END BUF) if VALUE is a cons of
two markers or an overlay.  Otherwise, it is nil."
  (cond ((bufferp value)
	 (with-current-buffer value
	   (when (mark t)
	     (list (mark t) (point) value))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (when (and (marker-buffer (car value))
		    (buffer-name (marker-buffer (car value)))
		    (eq (marker-buffer (car value))
			(marker-buffer (cdr value))))
	   (list (marker-position (car value))
		 (marker-position (cdr value))
		 (marker-buffer (car value)))))
	((overlayp value)
	 (when (overlay-buffer value)
	   (list (overlay-start value)
		 (overlay-end value)
		 (overlay-buffer value))))))

(defun haiku-dnd-convert-string (value)
  "Convert VALUE to a UTF-8 string and appropriate MIME type.
Return a list of the appropriate MIME type, and UTF-8 data of
VALUE as a unibyte string, or nil if VALUE was not a string."
  (unless (stringp value)
    (when-let ((bounds (haiku-selection-bounds value)))
      (setq value (ignore-errors
                    (with-current-buffer (nth 2 bounds)
                      (buffer-substring (nth 0 bounds)
                                        (nth 1 bounds)))))))
  (when (stringp value)
    (list "text/plain" (string-to-unibyte
                        (encode-coding-string value 'utf-8)))))

(defun haiku-dnd-convert-file-name (value)
  "Convert VALUE to a file system reference if it is a file name."
  (cond ((and (stringp value)
              (not (file-remote-p value))
              (file-exists-p value))
         (list "refs" (propertize (expand-file-name value)
                                  'type 'ref)))
        ((vectorp value)
         (list "refs"
               (cl-loop for item across value
                        collect (propertize (expand-file-name item)
                                            'type 'ref))))))

(defun haiku-dnd-convert-text-uri-list (value)
  "Convert VALUE to a list of URLs."
  (cond
   ((stringp value) (list "text/uri-list"
                          (concat (url-encode-url value) "\n")))
   ((vectorp value) (list "text/uri-list"
                          (with-temp-buffer
                            (cl-loop for tem across value
                                     do (progn
                                          (insert (url-encode-url tem))
                                          (insert "\n")))
                            (buffer-string))))))

(declare-function x-open-connection "haikufns.c")
(declare-function x-handle-args "common-win")
(declare-function haiku-selection-data "haikuselect.c")
(declare-function haiku-selection-put "haikuselect.c")
(declare-function haiku-selection-owner-p "haikuselect.c")
(declare-function haiku-put-resource "haikufns.c")
(declare-function haiku-drag-message "haikuselect.c")

(defun haiku--handle-x-command-line-resources (command-line-resources)
  "Handle command line X resources specified with the option `-xrm'.
The resources should be a list of strings in COMMAND-LINE-RESOURCES."
  (dolist (s command-line-resources)
    (let ((components (split-string s ":")))
      (when (car components)
        (haiku-put-resource (car components)
                            (string-trim-left
                             (mapconcat #'identity (cdr components) ":")))))))

(cl-defmethod window-system-initialization (&context (window-system haiku)
                                                     &optional display)
  "Set up the window system.  WINDOW-SYSTEM must be HAIKU.
DISPLAY may be set to the name of a display that will be initialized."
  (cl-assert (not haiku-initialized))
  (create-default-fontset)
  (when x-command-line-resources
    (haiku--handle-x-command-line-resources
     (split-string x-command-line-resources "\n")))
  (x-open-connection (or display "be") x-command-line-resources t)
  (setq haiku-initialized t))

(cl-defmethod frame-creation-function (params &context (window-system haiku))
  (x-create-frame-with-faces params))

(cl-defmethod handle-args-function (args &context (window-system haiku))
  (x-handle-args args))

(defun haiku--selection-type-to-mime (type)
  "Convert symbolic selection type TYPE to its MIME equivalent.
If TYPE is nil, return \"text/plain\"."
  (cond
   ((eq type 'STRING) "text/plain;charset=iso-8859-1")
   ((eq type 'UTF8_STRING) "text/plain")
   ((stringp type) type)
   ((symbolp type) (symbol-name type))
   (t "text/plain")))

(defun haiku-selection-targets (clipboard)
  "Find the types of data available from CLIPBOARD.
CLIPBOARD should be the symbol `PRIMARY', `SECONDARY' or
`CLIPBOARD'.  Return the available types as a list of strings."
  (mapcar #'car (haiku-selection-data clipboard nil)))

(defun haiku-select-encode-xstring (_selection value)
  "Convert VALUE to a system message association.
VALUE will be encoded as Latin-1 (like on X Windows) and stored
under the type `text/plain;charset=iso-8859-1'."
  (unless (stringp value)
    (when-let ((bounds (haiku-selection-bounds value)))
      (setq value (ignore-errors
                    (with-current-buffer (nth 2 bounds)
                      (buffer-substring (nth 0 bounds)
                                        (nth 1 bounds)))))))
  (when (and (stringp value) (not (string-empty-p value)))
    (list "text/plain;charset=iso-8859-1" 1296649541
          (encode-coding-string value 'iso-latin-1))))

(defun haiku-select-encode-utf-8-string (_selection value)
  "Convert VALUE to a system message association.
VALUE will be encoded as UTF-8 and stored under the type
`text/plain'."
  (unless (stringp value)
    (when-let ((bounds (haiku-selection-bounds value)))
      (setq value (ignore-errors
                    (with-current-buffer (nth 2 bounds)
                      (buffer-substring (nth 0 bounds)
                                        (nth 1 bounds)))))))
  (when (and (stringp value) (not (string-empty-p value)))
    (list "text/plain" 1296649541
          (encode-coding-string value 'utf-8-unix))))

(defun haiku-select-encode-file-name (_selection value)
  "Convert VALUE to a system message association.
This takes the file name of VALUE's buffer (if it is an overlay
or a pair of markers) and turns it into a file system reference."
  (when (setq value (xselect--selection-bounds value))
    (list "refs" 'ref (buffer-file-name (nth 2 value)))))

(cl-defmethod gui-backend-get-selection (type data-type
                                              &context (window-system haiku))
  (if (eq data-type 'TARGETS)
      (apply #'vector (mapcar #'intern
                              (haiku-selection-targets type)))
    (if (eq type 'XdndSelection)
        haiku-dnd-selection-value
      (haiku-selection-data type (haiku--selection-type-to-mime data-type)))))

(cl-defmethod gui-backend-set-selection (type value
                                              &context (window-system haiku))
  (if (eq type 'XdndSelection)
      (setq haiku-dnd-selection-value value)
    (let ((message nil))
      (dolist (encoder haiku-normal-selection-encoders)
        (let ((result (funcall encoder type value)))
          (when result
            (push result message))))
      (haiku-selection-put type message))))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system haiku))
  (haiku-selection-data selection "text/plain"))

(cl-defmethod gui-backend-selection-owner-p (selection &context (window-system haiku))
  (haiku-selection-owner-p selection))

(declare-function haiku-read-file-name "haikufns.c")

(defun x-file-dialog (prompt dir &optional default-filename mustmatch only-dir-p)
  "SKIP: real doc in xfns.c."
  (if (eq (framep-on-display (selected-frame)) 'haiku)
      (haiku-read-file-name (if (not (string-suffix-p ": " prompt))
                                prompt
                              (substring prompt 0 (- (length prompt) 2)))
                            (selected-frame)
                            (or dir (and default-filename
                                         (file-name-directory default-filename)))
                            mustmatch only-dir-p
                            (and default-filename
                                 (file-name-nondirectory default-filename)))
    (error "x-file-dialog on a tty frame")))

(defun haiku-drag-and-drop (event)
  "Handle specified drag-n-drop EVENT."
  (interactive "e")
  (let* ((string (caddr event))
	 (window (posn-window (event-start event))))
    (if (eq string 'lambda) ; This means the mouse moved.
        (dnd-handle-movement (event-start event))
      (cond
       ((assoc "refs" string)
        (with-selected-window window
          (raise-frame)
          (dolist (filename (cddr (assoc "refs" string)))
            (dnd-handle-one-url window 'private
                                (concat "file:" filename)))))
       ((assoc "text/uri-list" string)
        (dolist (text (cddr (assoc "text/uri-list" string)))
          (let ((uri-list (split-string text "[\0\r\n]" t)))
            (dolist (bf uri-list)
              (dnd-handle-one-url window 'private bf)))))
       ((assoc "text/plain" string)
        (with-selected-window window
          (raise-frame)
          (dolist (text (cddr (assoc "text/plain" string)))
            (goto-char (posn-point (event-start event)))
            (dnd-insert-text window 'private
                             (if (multibyte-string-p text)
                                 text
                               (decode-coding-string text 'undecided))))))
       ((not (eq (cdr (assq 'type string))
                 3003)) ; Type of the placeholder message Emacs uses
                        ; to cancel a drop on C-g.
        (message "Don't know how to drop any of: %s"
                 (mapcar #'car string)))))))

(define-key special-event-map [drag-n-drop] 'haiku-drag-and-drop)

(defvaralias 'haiku-use-system-tooltips 'use-system-tooltips)

(defun haiku-use-system-tooltips-watcher (&rest _ignored)
  "Variable watcher to force a menu bar update when `use-system-tooltip' changes.
This is necessary because on Haiku `use-system-tooltip' doesn't
take effect on menu items until the menu bar is updated again."
  (force-mode-line-update t))

;; Note that `mouse-position' can't return the actual frame the mouse
;; pointer is under, so this only works for the frame where the drop
;; started.
(defun haiku-dnd-drag-handler ()
  "Handle mouse movement during drag-and-drop."
  (let ((track-mouse 'drag-source)
        (mouse-position (mouse-pixel-position)))
    (when (car mouse-position)
      (dnd-handle-movement (posn-at-x-y (cadr mouse-position)
                                        (cddr mouse-position)
                                        (car mouse-position)))
      (redisplay))))

(setq haiku-drag-track-function #'haiku-dnd-drag-handler)

(defun x-begin-drag (targets &optional action frame _return-frame allow-current-frame)
  "SKIP: real doc in xfns.c."
  (unless haiku-dnd-selection-value
    (error "No local value for XdndSelection"))
  (let ((message nil)
        (mouse-highlight nil)
        (haiku-signal-invalid-refs nil))
    (dolist (target targets)
      (let* ((target-atom (intern target))
             (selection-converter (cdr (assoc target-atom
                                              haiku-dnd-selection-converters)))
             (value (if (stringp haiku-dnd-selection-value)
                        (or (get-text-property 0 target-atom
                                               haiku-dnd-selection-value)
                            haiku-dnd-selection-value)
                      haiku-dnd-selection-value)))
        (when selection-converter
          (let ((selection-result (funcall selection-converter value)))
            (when selection-result
              (let* ((field (cdr (assoc (car selection-result) message)))
                     (maybe-string (if (stringp (cadr selection-result))
                                       (cadr selection-result)
                                     (caadr selection-result))))
                (unless (cadr field)
                  ;; Add B_MIME_TYPE to the message if the type was not
                  ;; previously specified, or the type if it was.
                  (push (or (get-text-property 0 'type maybe-string)
                            1296649541)
                        (alist-get (car selection-result) message
                                   nil nil #'equal))))
              (if (not (consp (cadr selection-result)))
                  (push (cadr selection-result)
                        (cdr (alist-get (car selection-result) message
                                        nil nil #'equal)))
                (dolist (tem (cadr selection-result))
                  (push tem
                        (cdr (alist-get (car selection-result) message
                                        nil nil #'equal))))))))))
    (prog1 (or (and (symbolp action)
                    action)
               'XdndActionCopy)
      (haiku-drag-message (or frame (selected-frame))
                          message allow-current-frame))))

(add-variable-watcher 'use-system-tooltips #'haiku-use-system-tooltips-watcher)


;;;; Session management.

(declare-function haiku-save-session-reply "haikufns.c")

(defun emacs-session-save ()
  "SKIP: real doc in x-win.el."
  (with-temp-buffer ; Saving sessions is not yet supported.
    (condition-case nil
	;; A return of t means cancel the shutdown.
	(run-hook-with-args-until-success
	 'emacs-save-session-functions)
      (error t))))

(defun handle-save-session (_event)
  "SKIP: real doc in xsmfns.c."
  (interactive "e")
  (let ((cancel-shutdown t))
    (unwind-protect
        (setq cancel-shutdown (emacs-session-save))
      (haiku-save-session-reply (not cancel-shutdown)))
    ;; The App Server will kill Emacs after receiving the reply, but
    ;; the Deskbar will not, so kill ourself here.
    (unless cancel-shutdown (kill-emacs))))


;;;; Cursors.

;; We use the same interface as X, but the cursor numbers are
;; different, and there are also less cursors.

(defconst x-pointer-X-cursor 5)			; B_CURSOR_ID_CROSS_HAIR
(defconst x-pointer-arrow 1)			; B_CURSOR_ID_SYSTEM_DEFAULT
(defconst x-pointer-bottom-left-corner 22)	; B_CURSOR_ID_RESIZE_SOUTH_WEST
(defconst x-pointer-bottom-right-corner 21)	; B_CURSOR_ID_RESIZE_SOUTH_EAST
(defconst x-pointer-bottom-side 17)		; B_CURSOR_ID_RESIZE_SOUTH
(defconst x-pointer-clock 14)			; B_CURSOR_ID_PROGRESS
(defconst x-pointer-cross 5)			; B_CURSOR_ID_CROSS_HAIR
(defconst x-pointer-cross-reverse 5)		; B_CURSOR_ID_CROSS_HAIR
(defconst x-pointer-crosshair 5)		; B_CURSOR_ID_CROSS_HAIR
(defconst x-pointer-diamond-cross 5)		; B_CURSOR_ID_CROSS_HAIR
(defconst x-pointer-hand1 7)			; B_CURSOR_ID_GRAB
(defconst x-pointer-hand2 8)			; B_CURSOR_ID_GRABBING
(defconst x-pointer-left-side 18)		; B_CURSOR_ID_RESIZE_WEST
(defconst x-pointer-right-side 16)		; B_CURSOR_ID_RESIZE_EAST
(defconst x-pointer-sb-down-arrow 17)		; B_CURSOR_ID_RESIZE_SOUTH
(defconst x-pointer-sb-left-arrow 18)		; B_CURSOR_ID_RESIZE_WEST
(defconst x-pointer-sb-right-arrow 16)		; B_CURSOR_ID_RESIZE_EAST
(defconst x-pointer-sb-up-arrow 16)		; B_CURSOR_ID_RESIZE_NORTH
(defconst x-pointer-target 5)			; B_CURSOR_ID_CROSS_HAIR
(defconst x-pointer-top-left-corner 20)		; B_CURSOR_ID_RESIZE_NORTH_WEST
(defconst x-pointer-top-right-corner 19)	; B_CURSOR_ID_RESIZE_NORTH_EAST
(defconst x-pointer-top-side 16)		; B_CURSOR_ID_RESIZE_NORTH
(defconst x-pointer-watch 14)			; B_CURSOR_ID_PROGRESS
(defconst x-pointer-invisible 12)		; B_CURSOR_ID_NO_CURSOR

(provide 'haiku-win)
(provide 'term/haiku-win)

;;; haiku-win.el ends here
