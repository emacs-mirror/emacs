;;; x-dnd.el --- drag and drop support for X  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2026 Free Software Foundation, Inc.

;; Author: Jan Djärv <jan.h.d@swipnet.se>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: window, drag, drop
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

;; This file provides the receiving side of the XDND and Motif
;; protocols, and both the receiving and initiating ends of the old
;; KDE (OffiX) and new OffiX protocols.

;;; Code:

(require 'dnd)
;; For when building a --without-x configuration, where this is not
;; preloaded.
(eval-when-compile (require 'mwheel))

;;; Customizable variables
(defcustom x-dnd-test-function #'x-dnd-default-test-function
  "Function to be used by drag-and-drop to determine whether to accept a drop.
The function takes three arguments: WINDOW, ACTION, and TYPES.
WINDOW is where the window under the mouse is when the function is called.
WINDOW may be a frame if the mouse isn't over a real window (e.g., menu
bar, tool bar, scroll bar, etc.).
ACTION is the suggested action from the drag and drop source, one of the
symbols `move', `copy', `link' or `ask'.
TYPES is a vector of available types for the drop.
Each element of TYPES should either be a string (containing the
name of the type's X atom), or a symbol, whose name will be used.

The function shall return nil to reject the drop or a cons with
two values, the wanted action as `car' and the wanted type as `cdr'.
The wanted action can be `copy', `move', `link', `ask' or `private'.

The default value for this variable is `x-dnd-default-test-function'."
  :version "22.1"
  :type 'symbol
  :group 'x)

(defcustom x-dnd-types-alist
  '(("text/uri-list" . x-dnd-handle-uri-list)
    ("text/x-moz-url" . x-dnd-handle-moz-url)
    ("_NETSCAPE_URL" . x-dnd-handle-uri-list)
    ("FILE_NAME" . x-dnd-handle-file-name)
    ("UTF8_STRING" . x-dnd-insert-utf8-text)
    ("text/plain;charset=UTF-8" . x-dnd-insert-utf8-text)
    ("text/plain;charset=utf-8" . x-dnd-insert-utf8-text)
    ("text/unicode" . x-dnd-insert-utf16-text)
    ("text/plain" . dnd-insert-text)
    ("COMPOUND_TEXT" . x-dnd-insert-ctext)
    ("STRING" . dnd-insert-text)
    ("TEXT"   . dnd-insert-text)
    ("DndTypeFile" . x-dnd-handle-offix-file)
    ("DndTypeFiles" . x-dnd-handle-offix-files)
    ("DndTypeText" . dnd-insert-text))
  "Functions to call to handle drag-and-drop of known types.
If the type of the drop is not present in the alist, or the
function corresponding to the type is nil, the drop of that
type will be rejected.

Each function takes three arguments: WINDOW, ACTION, and DATA.
WINDOW is the window where the drop occurred.
ACTION is the action for this drop (`copy', `move', `link', `private'
or `ask'), as determined by a previous call to `x-dnd-test-function'.
DATA is the drop data.
The function shall return the action it used (one of the above,
excluding `ask') if drop is successful, nil if not."
  :version "22.1"
  :type 'alist
  :group 'x)

(defcustom x-dnd-known-types
  '("XdndDirectSave0"
    "text/uri-list"
    "text/x-moz-url"
    "_NETSCAPE_URL"
    "FILE_NAME"
    "UTF8_STRING"
    "text/plain;charset=UTF-8"
    "text/plain;charset=utf-8"
    "text/unicode"
    "text/plain"
    "COMPOUND_TEXT"
    "STRING"
    "TEXT"
    "DndTypeFile"
    "DndTypeText")
  "The types accepted by default for dropped data.
The types are chosen in the order they appear in the list."
  :version "22.1"
  :type '(repeat string)
  :group 'x)

(defcustom x-dnd-use-offix-drop 'files
  "If non-nil, use the OffiX protocol to drop files and text.
This allows dropping (via `dired-mouse-drag-files' or
`mouse-drag-and-drop-region-cross-program') on some old Java
applets and old KDE programs.  Turning this off allows dropping
only text on some other programs such as xterm and urxvt.

If the symbol `files', use the OffiX protocol when dropping
files, and the fallback drop method (which is used with programs
like xterm) for text."
  :version "29.1"
  :type '(choice (const :tag "Don't use the OffiX protocol for drag-and-drop" nil)
                 (const :tag "Only use the OffiX protocol to drop files" files)
                 (const :tag "Use the OffiX protocol for both files and text" t))
  :group 'x)

(defcustom x-dnd-direct-save-function #'x-dnd-save-direct
  "Function called when a file is dropped via XDS protocol.
The value should be a function of two arguments that supports
the X Direct Save (XDS) protocol.  The function will be called
twice during the protocol execution.

When the function is called with the first argument non-nil,
it should return an absolute file name whose base name is
the value of the second argument, a string.  The return value
is the file name for the dragged file to be saved.  The function
can also return nil if saving the file should be refused for some
reason; in that case the drop will be canceled.

When the function is called with the first argument nil, the
second argument specifies the file name where the file was saved;
the function should then do whatever is appropriate when such a
file is saved, like show the file in the Dired buffer or visit
the file."
  :version "29.1"
  :type '(choice (const :tag "Prompt for file name to save"
                        x-dnd-save-direct)
                 (const :tag "Save in `default-directory' without prompting"
                        x-dnd-save-direct-immediately)
                 (function :tag "Other function"))
  :group 'x)

(defcustom x-dnd-copy-types '("chromium/x-renderer-taint")
  "List of data types offered by programs that don't support `private'.
Some programs (such as Chromium) do not support
`XdndActionPrivate'.  The default `x-dnd-test-function' will
always return `copy' instead, for programs offering one of the
data types in this list."
  :version "29.1"
  :type '(repeat string)
  :group 'x)

;; Internal variables

(defvar x-dnd-debug-errors nil
  "Whether or not to signal protocol errors during drag-and-drop.
This is useful for debugging errors in the DND code, but makes
drag-and-drop much slower over network connections with high
latency.")

(defvar x-dnd-current-state nil
  "The current state for a drop.
This is an alist with one entry for each display.  The value for each display
is a vector that contains the state for drag and drop for that display.
Elements in the vector are:
Last buffer drag was in,
last window drag was in,
types available for drop,
the action suggested by the source,
the type we want for the drop,
the action we want for the drop,
any protocol specific data.")

(declare-function x-get-selection-internal "xselect.c"
		  (selection-symbol target-type &optional time-stamp terminal))
(declare-function x-display-set-last-user-time "xfns.c")

(defconst x-dnd-xdnd-to-action
  '(("XdndActionPrivate" . private)
    ("XdndActionCopy" . copy)
    ("XdndActionMove" . move)
    ("XdndActionLink" . link)
    ("XdndActionAsk" . ask)
    ("XdndActionDirectSave" . direct-save))
  "Mapping from XDND action types to Lisp symbols.")

(defvar x-dnd-empty-state [nil nil nil nil nil nil nil])

(declare-function x-register-dnd-atom "xselect.c")

(defvar x-fast-protocol-requests)

(defun x-dnd-init-frame (&optional frame)
  "Setup drag and drop for FRAME (i.e. create appropriate properties)."
  (when (and (eq 'x (window-system frame))
             (not (frame-parameter frame 'tooltip)))
    (let ((x-fast-protocol-requests (not x-dnd-debug-errors)))
      (x-register-dnd-atom "DndProtocol" frame)
      (x-register-dnd-atom "_MOTIF_DRAG_AND_DROP_MESSAGE" frame)
      (x-register-dnd-atom "XdndEnter" frame)
      (x-register-dnd-atom "XdndPosition" frame)
      (x-register-dnd-atom "XdndLeave" frame)
      (x-register-dnd-atom "XdndDrop" frame)
      (x-register-dnd-atom "_DND_PROTOCOL" frame)
      (x-dnd-init-xdnd-for-frame frame)
      (x-dnd-init-motif-for-frame frame))))

(defun x-dnd-get-state-cons-for-frame (frame-or-window)
  "Return the entry in `x-dnd-current-state' for a frame or window."
  (let* ((frame (if (framep frame-or-window) frame-or-window
		  (window-frame frame-or-window)))
	 (display (frame-parameter frame 'display)))
    (if (not (assoc display x-dnd-current-state))
	(push (cons display (copy-sequence x-dnd-empty-state))
	      x-dnd-current-state))
    (assoc display x-dnd-current-state)))

(defun x-dnd-get-state-for-frame (frame-or-window)
  "Return the state in `x-dnd-current-state' for a frame or window."
  (cdr (x-dnd-get-state-cons-for-frame frame-or-window)))

(defun x-dnd-default-test-function (_window _action types)
  "The default test function for drag-and-drop.
WINDOW is where the mouse is when this function is called.  It
may be a frame if the mouse is over the menu bar, scroll bar or
tool bar.  ACTION is the suggested action from the source, and
TYPES are the types the drop data can have.  This function only
accepts drops with types in `x-dnd-known-types'.  It always
returns the action `private', unless `types' contains a value
inside `x-dnd-copy-types', in which case it may return `copy'."
  (let ((type (x-dnd-choose-type types)))
    (when type (let ((list x-dnd-copy-types))
                 (catch 'out
                   (while t
                     (if (not list)
                         (throw 'out (cons 'private type))
                       (if (x-dnd-find-type (car list) types)
                           (throw 'out (cons 'copy type))
                         (setq list (cdr list))))))))))

(defun x-dnd-current-type (frame-or-window)
  "Return the type we want the DND data to be in for the current drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (aref (x-dnd-get-state-for-frame frame-or-window) 4))

(defun x-dnd-forget-drop (frame-or-window)
  "Remove all state for the last drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (setcdr (x-dnd-get-state-cons-for-frame frame-or-window)
	  (copy-sequence x-dnd-empty-state)))

(defun x-dnd-find-type (target types)
  "Find the type TARGET in an array of types TYPES.
TARGET must be a string, but TYPES can contain either symbols or
strings."
  (catch 'done
    (dotimes (i (length types))
      (let* ((type (aref types i))
	     (typename (if (symbolp type)
			   (symbol-name type) type)))
	(when (equal target typename)
	  (throw 'done t))))
    nil))

(defun x-dnd-maybe-call-test-function (window action &optional xdnd)
  "Call `x-dnd-test-function' if something has changed.
WINDOW is the window the mouse is over.  ACTION is the suggested
action from the source.  If nothing has changed, return the last
action and type we got from `x-dnd-test-function'.

XDND means the XDND protocol is being used."
  (let ((buffer (when (window-live-p window)
		  (window-buffer window)))
	(current-state (x-dnd-get-state-for-frame window)))
    (if (and xdnd (x-dnd-find-type "XdndDirectSave0"
                                   (aref current-state 2)))
        (setq current-state
              (x-dnd-save-state window 'direct-save
                                '(direct-save . "XdndDirectSave0")))
      (unless (and (equal buffer (aref current-state 0))
                   (equal window (aref current-state 1))
                   (equal action (aref current-state 3)))
        (save-current-buffer
	  (when buffer (set-buffer buffer))
	  (let* ((action-type (funcall x-dnd-test-function
				       window
				       action
				       (aref current-state 2)))
	         (handler (cdr (assoc (cdr action-type) x-dnd-types-alist))))
	    ;; Ignore action-type if we have no handler.
	    (setq current-state
		  (x-dnd-save-state window
				    action
				    (when handler action-type))))))))
  (let ((current-state (x-dnd-get-state-for-frame window)))
    (cons (aref current-state 5)
	  (aref current-state 4))))

(defun x-dnd-save-state (window action action-type &optional types extra-data)
  "Save the state of the current drag and drop.
WINDOW is the window the mouse is over.  ACTION is the action suggested
by the source.  ACTION-TYPE is the result of calling `x-dnd-test-function'.
If given, TYPES are the types for the drop data that the source supports.
EXTRA-DATA is data needed for a specific protocol."
  (let ((current-state (x-dnd-get-state-for-frame window)))
    (aset current-state 5 (car action-type))
    (aset current-state 4 (cdr action-type))
    (aset current-state 3 action)
    (when types (aset current-state 2 types))
    (when extra-data (aset current-state 6 extra-data))
    (aset current-state 1 window)
    (aset current-state 0 (and (window-live-p window) (window-buffer window)))
    (setcdr (x-dnd-get-state-cons-for-frame window) current-state)))


(defun x-dnd-handle-moz-url (window action data)
  "Handle one item of type text/x-moz-url.
WINDOW is the window where the drop happened.  ACTION is ignored.
DATA is the moz-url, which is formatted as two strings separated by \\r\\n.
The first string is the URL, the second string is the title of that URL.
DATA is encoded in utf-16.  Decode the URL and call `x-dnd-handle-uri-list'."
  ;; Mozilla and applications based on it use text/unicode, but it is
  ;; impossible to tell if it is le or be.  Use what the machine Emacs
  ;; runs on uses.  This loses if dropping between machines
  ;; with different endian-ness, but it is the best we can do.
  (let* ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le))
	 (string (decode-coding-string data coding))
	 (strings (split-string string "[\r\n]" t))
	 ;; Can one drop more than one moz-url ??  Assume not.
	 (url (car strings)))
    (x-dnd-handle-uri-list window action url)))

(defun x-dnd-insert-utf8-text (window action text)
  "Decode the UTF-8 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (dnd-insert-text window action (decode-coding-string text 'utf-8)))

(defun x-dnd-insert-utf16-text (window action text)
  "Decode the UTF-16 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  ;; See comment in x-dnd-handle-moz-url about coding.
  (let ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le)))
    (dnd-insert-text window action (decode-coding-string text coding))))

(defun x-dnd-insert-ctext (window action text)
  "Decode the compound text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (dnd-insert-text window action
		   (decode-coding-string text
					 'compound-text-with-extensions)))

(defun x-dnd-handle-uri-list (window action string)
  "Split an uri-list into separate URIs and call `dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the uri-list as a string.  The URIs are separated by \\r\\n."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	retval)
    (let ((did-action (dnd-handle-multiple-urls window uri-list
                                                action)))
      (when did-action (setq retval did-action)))
    retval))

(defun x-dnd-handle-file-name (window action string)
  "Convert file names to URLs and call `dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the file names as a string, separated by nulls."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	(coding (or file-name-coding-system
		    default-file-name-coding-system))
	retval)
    (let ((did-action
           (dnd-handle-multiple-urls
            window
            (mapcar
             (lambda (item)
               (when coding
                 (setq item (encode-coding-string item
                                                  coding)))
               (concat "file://"
                       (mapconcat 'url-hexify-string
                                  (split-string item "/")
                                  "/")))
             uri-list)
            action)))
      (when did-action (setq retval did-action)))
    retval))

(defun x-dnd-choose-type (types &optional known-types)
  "Choose which type we want to receive for the drop.
TYPES are the types the source of the drop offers, a vector of type names
as strings or symbols.  Select among the types in `x-dnd-known-types' or
KNOWN-TYPES if given, and return that type name.
If no suitable type is found, return nil."
  (let* ((known-list (or known-types x-dnd-known-types))
	 (first-known-type (car known-list))
	 (types-array types)
	 (found (when first-known-type
		  (catch 'done
		    (dotimes (i (length types-array))
		      (let* ((type (aref types-array i))
			     (typename (if (symbolp type)
					   (symbol-name type) type)))
			(when (equal first-known-type typename)
			  (throw 'done first-known-type))))
		    nil))))

    (if (and (not found) (cdr known-list))
	(x-dnd-choose-type types (cdr known-list))
      found)))

(defun x-dnd-drop-data (event frame window data type)
  "Drop one data item onto a frame.
EVENT is the client message for the drop, FRAME is the frame the drop
occurred on.  WINDOW is the window of FRAME where the drop happened.
DATA is the data received from the source, and type is the type for DATA,
see `x-dnd-types-alist').

Returns the action used (move, copy, link, private) if drop was successful,
nil if not."
  (let* ((type-info (assoc type x-dnd-types-alist))
	 (handler (cdr type-info))
	 (state (x-dnd-get-state-for-frame frame))
	 (action (aref state 5))
	 (w (posn-window (event-start event))))
    (when handler
      (if (and (window-live-p w)
	       (not (window-minibuffer-p w))
	       (not (window-dedicated-p w)))
	  ;; If dropping in an ordinary window which we could use,
	  ;; let dnd-open-file-other-window specify what to do.
	  (progn
	    (when (and (not mouse-yank-at-point)
                       ;; If dropping on top of the mode line, insert
                       ;; the text at point instead.
                       (posn-point (event-start event)))
	      (goto-char (posn-point (event-start event))))
	    (funcall handler window action data))
	;; If we can't display the file here,
	;; make a new window for it.
	(let ((dnd-open-file-other-window t))
	  (select-frame frame)
	  (funcall handler window action data))))))

(defun x-dnd-handle-drag-n-drop-event (event)
  "Receive drag and drop events (X client messages).
Currently XDND, Motif and old KDE 1.x protocols are recognized."
  (interactive "e")
  (let* ((client-message (car (cdr (cdr event))))
         (x-fast-protocol-requests (not x-dnd-debug-errors))
	 (window (posn-window (event-start event))))
    (if (eq (and (consp client-message)
                 (car client-message))
            'XdndSelection)
        ;; This is an internal Emacs message caused by something being
        ;; dropped on top of a frame.
        (progn
          (let ((action (cdr (assoc (symbol-name (cadr client-message))
                                    x-dnd-xdnd-to-action)))
                (targets (cdddr client-message))
                (local-value (nth 2 client-message)))
            (when (windowp window)
              (select-window window))
            ;; Remove XdndDirectSave0 from this list--Emacs does not
            ;; support this protocol for internal drops.
            (setq targets (delete 'XdndDirectSave0 targets))
            (x-dnd-save-state window nil nil (apply #'vector targets))
            (x-dnd-maybe-call-test-function window action nil)
            (unwind-protect
                (x-dnd-drop-data event (if (framep window) window
                                         (window-frame window))
                                 window
                                 (x-get-local-selection
                                  local-value
                                  (intern (x-dnd-current-type window)))
                                 (x-dnd-current-type window))
              (x-dnd-forget-drop window))))
      (let ((message-atom (aref client-message 0))
	    (frame (aref client-message 1))
	    (format (aref client-message 2))
	    (data (aref client-message 3)))
        (cond ((equal "DndProtocol" message-atom)	; Old KDE 1.x.
	       (x-dnd-handle-old-kde event frame window message-atom format data))
              ((equal "_DND_PROTOCOL" message-atom) ; OffiX protocol.
               (x-dnd-handle-offix event frame window message-atom format data))
	      ((equal "_MOTIF_DRAG_AND_DROP_MESSAGE" message-atom)	; Motif
	       (x-dnd-handle-motif event frame window message-atom format data))

	      ((and (> (length message-atom) 4)	; XDND protocol.
		    (equal "Xdnd" (substring message-atom 0 4)))
	       (x-dnd-handle-xdnd event frame window message-atom format data)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Old KDE protocol.

(declare-function x-window-property "xfns.c"
		  (prop &optional frame type source delete-p vector-ret-p))

(defvar x-dnd-offix-old-kde-to-name '((-1 . DndTypeInvalid)
                                      (0 . DndTypeUnknown)
                                      (1 . DndTypeRawData)
                                      (2 . DndTypeFile)
                                      (3 . DndTypeFiles)
                                      (4 . DndTypeText)
                                      (5 . DndTypeDir)
                                      (6 . DndTypeLink)
                                      (7 . DndTypeExe)
                                      (8 . DndTypeUrl))
  "Alist of old KDE data types to their names.")

(defun x-dnd-handle-old-kde (event frame window _message _format data)
  "Handle an old KDE (OffiX) drop.
EVENT, FRAME, WINDOW and DATA mean the same thing they do in
`x-dnd-handle-offix.'"
  (let ((proto (aref data 4)))
    ;; If PROTO > 0, this is an old KDE drop emulated by a program
    ;; supporting a newer version of the OffiX protocol, so we should
    ;; wait for the corresponding modern event instead.
    (when (zerop proto)
      (let ((type (cdr (assq (aref data 0) x-dnd-offix-old-kde-to-name)))
            (data (x-window-property "DndSelection" frame nil 0 t)))
        ;; First save state.
        (x-dnd-save-state window nil nil (vector type) nil)
        ;; Now call the test function to decide what action to perform.
        (x-dnd-maybe-call-test-function window 'private)
        (unwind-protect
            (when (windowp window)
              (select-window window))
            (x-dnd-drop-data event frame window data
                             (symbol-name type))
          (x-dnd-forget-drop window))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New OffiX protocol.

(defvar x-dnd-offix-id-to-name '((-1 . DndTypeInvalid)
                                 (0 . DndTypeUnknown)
                                 (1 . DndTypeRawData)
                                 (2 . DndTypeFile)
                                 (3 . DndTypeFiles)
                                 (4 . DndTypeText)
                                 (5 . DndTypeDir)
                                 (6 . DndTypeLink)
                                 (7 . DndTypeExe)
                                 (8 . DndTypeUrl)
                                 (9 . DndTypeMime)
                                 (10 . DndTypePixmap))
  "Alist of OffiX data types to their names.")

(defun x-dnd-handle-offix-file (window action string)
  "Convert OffiX file name to a regular file name.
Then, call `x-dnd-handle-file-name'.

WINDOW and ACTION mean the same as in `x-dnd-handle-file-name'.
STRING is the raw OffiX file name data."
  (x-dnd-handle-file-name window action
                          (replace-regexp-in-string "\0$" "" string)))

(defun x-dnd-handle-offix-files (window action string)
  "Convert OffiX file name list to a URI list.
Then, call `x-dnd-handle-file-name'.

WINDOW and ACTION mean the same as in `x-dnd-handle-file-name'.
STRING is the raw OffiX file name data."
  (x-dnd-handle-file-name window action
                          ;; OffiX file name lists contain one extra
                          ;; NULL byte at the end.
                          (if (string-suffix-p "\0\0" string)
                              (substring string 0 (1- (length string)))
                            string)))

(defun x-dnd-handle-offix (event frame window _message-atom _format data)
  "Handle OffiX drop event EVENT.
FRAME is the frame where the drop happened.
WINDOW is the window where the drop happened.
_MESSAGE-ATOM and _FORMAT are unused.
DATA is the vector containing the contents of the client
message (format 32) that caused EVENT to be generated."
  (let ((type (cdr (assq (aref data 0) x-dnd-offix-id-to-name)))
        (data (x-window-property "_DND_SELECTION" frame nil 0 t)))
    ;; First save state.
    (x-dnd-save-state window nil nil (vector type) nil)
    ;; Now call the test function to decide what action to perform.
    (x-dnd-maybe-call-test-function window 'private)
    (unwind-protect
        (when (windowp window)
          (select-window window))
        (x-dnd-drop-data event frame window data
                         (symbol-name type))
      (x-dnd-forget-drop window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  XDND protocol.

(declare-function x-change-window-property "xfns.c"
		  (prop value &optional frame type format outer-P window-id))
(declare-function x-translate-coordinates "xfns.c")

(defun x-dnd-init-xdnd-for-frame (frame)
  "Set the XdndAware property for FRAME to indicate that we do XDND."
  (x-change-window-property "XdndAware"
			    '(5)	;; The version of XDND we support.
			    frame "ATOM" 32 t))

(defun x-dnd-after-move-frame (frame)
  "Handle FRAME moving to a different position.
Clear any cached root window position."
  (and (frame-live-p frame)
       (set-frame-parameter frame 'dnd-root-window-position
                            nil)))

(add-hook 'move-frame-functions #'x-dnd-after-move-frame)

(defun x-dnd-compute-root-window-position (frame)
  "Return the position of FRAME's edit widget relative to the root window.
The value is a cons of (X . Y), describing the position of
FRAME's edit widget (inner window) relative to the root window of
its screen."
  (or (frame-parameter frame 'dnd-root-window-position)
      (let* ((result (x-translate-coordinates frame))
             (param (cons (car result) (cadr result))))
        (unless result
          (error "Frame isn't on the same screen as its root window"))
        (prog1 param
          (set-frame-parameter frame 'dnd-root-window-position param)))))

(defun x-dnd-get-window-rectangle (window)
  "Return the bounds of WINDOW as a rectangle.
The coordinates in the rectangle are relative to its frame's root
window.  Return the bounds as a list of (X Y WIDTH HEIGHT)."
  (let* ((frame (window-frame window))
         (frame-pos (x-dnd-compute-root-window-position frame))
         (edges (window-inside-pixel-edges window)))
    (list (+ (car frame-pos) (nth 0 edges))
          (+ (cdr frame-pos) (nth 1 edges))
          (- (nth 2 edges) (nth 0 edges))
          (- (nth 3 edges) (nth 1 edges)))))

(defun x-dnd-intersect-rectangles (r1 r2)
  "Return the intersection of R1 and R2, both rectangles."
  (let ((left (if (< (car r1) (car r2)) r1 r2))
        (right (if (> (car r2) (car r1)) r2 r1))
        (upper (if (< (cadr r1) (cadr r2)) r1 r2))
        (lower (if (> (cadr r2) (cadr r1)) r2 r1))
        (result (list 0 0 0 0)))
    (when (<= (car right) (+ (car left) (nth 2 left)))
      (setcar result (car right))
      (setcar (nthcdr 2 result)
              (- (min (+ (car left) (nth 2 left))
                      (+ (car right) (nth 2 right)))
                 (car result)))
      (when (<= (cadr lower) (+ (cadr upper) (nth 3 upper)))
        (setcar (cdr result) (cadr lower))
        (setcar (nthcdr 3 result)
                (- (min (+ (cadr lower) (nth 3 lower))
                        (+ (cadr upper) (nth 3 upper)))
                   (cadr result)))))
    result))

(defun x-dnd-get-object-rectangle (window posn)
  "Return the rectangle of the object (character or image) under POSN.
WINDOW is the window POSN represents.  The rectangle is returned
with coordinates relative to the root window."
  (if (posn-point posn)
      (with-selected-window window
        (if-let* ((new-posn (posn-at-point (posn-point posn)))
                  (posn-x-y (posn-x-y new-posn))
                  (object-width-height (posn-object-width-height new-posn))
                  (edges (window-inside-pixel-edges window))
                  (frame-pos (x-dnd-compute-root-window-position
                              (window-frame window))))
            (list (+ (car frame-pos) (car posn-x-y)
                     (car edges))
                  (+ (cdr frame-pos) (cdr posn-x-y)
                     (cadr edges))
                  (car object-width-height)
                  (cdr object-width-height))
          '(0 0 0 0)))
    '(0 0 0 0)))

(defun x-dnd-get-drop-rectangle (window posn)
  "Return the drag-and-drop rectangle at POSN on WINDOW."
  (if (or dnd-scroll-margin
          (not (windowp window))
          ;; Drops on the scroll bar aren't allowed, but the mouse
          ;; rectangle can be set while still on the scroll bar,
          ;; causing the drag initiator to never send an XdndPosition
          ;; event that will an XdndStatus message with the accept
          ;; flag set to be set, even after the mouse enters the
          ;; window text area.  To prevent that, simply don't generate
          ;; a mouse rectangle when an area is set.
          (posn-area posn))
      '(0 0 0 0)
    (let ((window-rectangle (x-dnd-get-window-rectangle window))
          object-rectangle)
      (when dnd-indicate-insertion-point
        (setq object-rectangle (x-dnd-get-object-rectangle window posn)
              window-rectangle (x-dnd-intersect-rectangles object-rectangle
                                                           window-rectangle)))
      window-rectangle)))

(declare-function x-get-atom-name "xselect.c" (value &optional frame))
(declare-function x-send-client-message "xselect.c"
		  (display dest from message-type format values))

(defun x-dnd-version-from-flags (flags)
  "Return the version byte from the 32 bit FLAGS in an XDndEnter message."
  (ash flags -24))

(defun x-dnd-more-than-3-from-flags (flags)
  "Return the nmore-than3 bit from the 32 bit FLAGS in an XDndEnter message."
  (logand flags 1))

(declare-function x-get-modifier-masks "xfns.c")

(defun x-dnd-modifier-mask (mods)
  "Return the X modifier mask for the Emacs modifier state MODS.
MODS is a single symbol, or a list of symbols such as `shift' or
`control'."
  (let ((virtual-modifiers (x-get-modifier-masks))
        (mask 0))
    (unless (consp mods)
      (setq mods (list mods)))
    (dolist (modifier mods)
      (cond ((eq modifier 'shift)
             (setq mask (logior mask 1))) ; ShiftMask
            ((eq modifier 'control)
             (setq mask (logior mask 4))) ; ControlMask
            ((eq modifier 'meta)
             (setq mask (logior mask (nth 4 virtual-modifiers))))
            ((eq modifier 'hyper)
             (setq mask (car virtual-modifiers)))
            ((eq modifier 'super)
             (setq mask (cadr virtual-modifiers)))
            ((eq modifier 'alt)
             (setq mask (nth 2 virtual-modifiers)))))
    mask))

(defun x-dnd-get-modifiers ()
  "Obtain an X modifier mask containing all modifiers.
Value is an X modifier mask containing all modifiers that can
modify an Emacs keyboard or mouse event."
  (let ((mods (x-get-modifier-masks))
        (mask 5)) ; ShiftMask | ControlMask
    (dolist (mod mods)
      (setq mask (logior mask mod)))
    mask))

(defun x-dnd-wheel-modifier-type (flags)
  "Return the modifier type of an X modifier mask.
FLAGS is the X modifier mask of a turn of the mouse wheel."
  (let ((modifiers (x-dnd-get-modifiers)))
    (catch 'type
      (dolist (modifier mouse-wheel-scroll-amount)
        (when (and (consp modifier)
                   (eq (x-dnd-modifier-mask (car modifier))
                       (logand flags modifiers)))
          (throw 'type (cdr modifier))))
      nil)))

(defvar x-dnd-click-count nil
  "Alist of button numbers to click counters during drag-and-drop.
The cdr of each association's cdr is the timestamp of the last
button press event for the given button, and the car is the
number of clicks in quick succession currently received.")

(defun x-dnd-note-click (button timestamp)
  "Note that button BUTTON was pressed at TIMESTAMP during drag-and-drop.
Return the number of clicks that were made in quick succession."
  (if (not (integerp double-click-time))
      1
    (let ((cell (cdr (assq button x-dnd-click-count))))
      (unless cell
        (setq cell (cons 0 timestamp))
        (push (cons button cell)
              x-dnd-click-count))
      (when (< (cdr cell) (- timestamp double-click-time))
        (setcar cell 0))
      (setcar cell (1+ (car cell)))
      (setcdr cell timestamp)
      (car cell))))

(defun x-dnd-mwheel-scroll (button count modifiers)
  "Call the appropriate wheel scrolling function for BUTTON.
Use MODIFIERS, an X modifier mask, to determine if any
alternative operation (such as scrolling horizontally) should be
taken.  COUNT is the number of times in quick succession BUTTON
has been pressed."
  (let* ((type (x-dnd-wheel-modifier-type modifiers))
         (hscroll (eq type 'hscroll))
         (amt (or (and (not mouse-wheel-progressive-speed) 1)
                  (* 1 count))))
    (unless (and (not mouse-wheel-tilt-scroll)
                 (or (eq button 6) (eq button 7)))
      (let ((function (cond ((eq type 'text-scale)
                             #'text-scale-adjust)
                            ((eq type 'global-text-scale)
                             #'global-text-scale-adjust)
                            ((eq button 4)
                             (if hscroll
                                 mwheel-scroll-right-function
                               mwheel-scroll-down-function))
                            ((eq button 5)
                             (if hscroll
                                 mwheel-scroll-left-function
                               mwheel-scroll-up-function))
                            ((eq button 6)
                             (if mouse-wheel-flip-direction
                                 mwheel-scroll-right-function
                               mwheel-scroll-left-function))
                            ((eq button 7)
                             (if mouse-wheel-flip-direction
                                 mwheel-scroll-left-function
                               mwheel-scroll-right-function)))))
        ;; Button5 should decrease the text scale, not increase it.
        (when (and (memq type '(text-scale global-text-scale))
                   (eq button 5))
          (setq amt (- amt)))
        (when function
          (condition-case nil
              ;; Don't overwrite any echo-area message that might
              ;; already be shown, since this can be called from
              ;; `x-begin-drag'.
              (let ((inhibit-message t))
                (funcall function amt))
            ;; Do not error at buffer limits.  Show a message instead.
            ;; This is especially important here because signaling an
            ;; error will mess up the drag-and-drop operation.
            (beginning-of-buffer
             (message (error-message-string '(beginning-of-buffer))))
            (end-of-buffer
             (message (error-message-string '(end-of-buffer))))))))))

(defun x-dnd-handle-xdnd (event frame window message _format data)
  "Receive one XDND event (client message) and send the appropriate reply.
EVENT is the client message.  FRAME is where the mouse is now.
WINDOW is the window within FRAME where the mouse is now.
DATA is the vector containing the data of the client message as a
vector of cardinals.
MESSAGE is the type of the ClientMessage that was sent."
  (cond ((equal "XdndEnter" message)
	 (let* ((flags (aref data 1))
		(version (x-dnd-version-from-flags flags))
		(more-than-3 (x-dnd-more-than-3-from-flags flags))
		(dnd-source (aref data 0)))
	   (when version  ;; If flags is bad, version will be nil.
	     (x-dnd-save-state
	      window nil nil
	      (if (> more-than-3 0)
		  (x-window-property "XdndTypeList"
				     frame "AnyPropertyType"
				     dnd-source nil t)
		(vector (x-get-atom-name (aref data 2))
			(x-get-atom-name (aref data 3))
			(x-get-atom-name (aref data 4))))
              version))))

	((equal "XdndPosition" message)
         ;; If (flags >> 10) & 1, then Emacs should scroll according
         ;; to the button passed in bits 8 and 9, and the state passed
         ;; in bits 0 to 7.
         (let ((state (x-dnd-get-state-for-frame window)))
           (when (windowp (posn-window (event-start event)))
             (let ((flags (aref data 1))
                   (version (aref state 6)))
               (when (not (zerop (logand (ash flags -10) 1)))
                 (let* ((button (+ 4 (logand (ash flags -8) #x3)))
                        (count (or (and (>= version 1)
                                        (x-dnd-note-click button
                                                          (aref data 3)))
                                   1))
                        (state (logand flags #xff)))
                   (with-selected-window (posn-window (event-start event))
                     (x-dnd-mwheel-scroll button count state)
                     (let ((old-x-y (posn-x-y (event-start event))))
                       (setcar (cdr event)
                               (posn-at-x-y (max (car old-x-y) 0)
                                            (max (cdr old-x-y) 0)))))))))
	   (let* ((version (aref state 6))
                  (action (if (< version 2) 'copy ; `copy' is the default action.
                            (x-get-atom-name (aref data 4))))
		  (dnd-source (aref data 0))
		  (action-type (x-dnd-maybe-call-test-function
			        window
			        (cdr (assoc action x-dnd-xdnd-to-action)) t))
		  (reply-action (car (rassoc
                                      ;; Mozilla and some other programs
                                      ;; support XDS, but only if we
                                      ;; reply with `copy'.  We can
                                      ;; recognize these broken programs
                                      ;; by checking to see if
                                      ;; `XdndActionDirectSave' was
                                      ;; originally specified.
                                      (if (and (eq (car action-type)
                                                   'direct-save)
                                               (not (eq action 'direct-save)))
                                          'copy
                                        (car action-type))
				      x-dnd-xdnd-to-action)))
		  (accept ;; 1 = accept, 0 = reject
		   (if (and reply-action action-type
                            ;; Only allow drops on the text area of a
                            ;; window.
                            (not (posn-area (event-start event))))
                       1 0))
                  (rect (x-dnd-get-drop-rectangle window
                                                  (event-start event)))
		  (list-to-send
		   (list (string-to-number
			  (frame-parameter frame 'outer-window-id))
                         ;; 1 = accept, 0 = reject.  2 = "want position
                         ;; updates even for movement inside the given
                         ;; widget bounds".
		         accept
		         (cons (car rect) (cadr rect))
		         (cons (nth 2 rect) (nth 3 rect))
                         ;; The no-toolkit Emacs build can actually
                         ;; receive drops from programs that speak
                         ;; versions of XDND earlier than 3 (such as
                         ;; GNUstep), since the toplevel window is the
                         ;; innermost window.
		         (if (>= version 2)
                             (or reply-action 0)
                           0))))
	     (x-send-client-message
	      frame dnd-source frame "XdndStatus" 32 list-to-send)
             (dnd-handle-movement (event-start event)))))

	((equal "XdndLeave" message)
	 (x-dnd-forget-drop window))

	((equal "XdndDrop" message)
	 (if (windowp window) (select-window window))
	 (let* ((state (x-dnd-get-state-for-frame frame))
                (version (aref state 6))
                (dnd-source (aref data 0))
		(timestamp (aref data 2))
                (current-action (aref state 5))
                (current-type (aref state 4))
		success action value)
           (x-display-set-last-user-time timestamp)
           (if (and (eq current-action 'direct-save)
                    (equal current-type "XdndDirectSave0"))
               (x-dnd-handle-xds-drop event window dnd-source version)
             (setq value (and (x-dnd-current-type window)
			      (x-get-selection-internal
			       'XdndSelection
			       (intern (x-dnd-current-type window))
			       timestamp)))
             (unwind-protect
                 (setq action (if value
			          (condition-case info
				      (x-dnd-drop-data
                                       event frame window value
				       (x-dnd-current-type window))
			            (error
			             (message "Error: %s" info)
			             nil))))
	       (setq success (if action 1 0))
               (when (>= version 2)
	         (x-send-client-message
	          frame dnd-source frame "XdndFinished" 32
	          (list (string-to-number
                         (frame-parameter frame 'outer-window-id))
		        (if (>= version 5) success 0) ;; 1 = Success, 0 = Error
		        (if (or (not action) (< version 5)) 0
                          (or (car (rassoc action
                                           x-dnd-xdnd-to-action))
                              0)))))
	       (x-dnd-forget-drop window)))))

	(t (error "Unknown XDND message %s %s" message data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Motif protocol.

(defun x-dnd-init-motif-for-frame (frame)
  "Set _MOTIF_DRAG_RECEIVER_INFO for FRAME to indicate that we do Motif DND."
  (x-change-window-property "_MOTIF_DRAG_RECEIVER_INFO"
			    (list
			     (byteorder)
			     0			; The Motif DND version.
			     5			; We want drag dynamic.
			     0 0 0 0 0 0 0
			     0 0 0 0 0 0)	; Property must be 16 bytes.
			    frame "_MOTIF_DRAG_RECEIVER_INFO" 8 t))

(defun x-dnd-get-motif-value (data offset size byteorder)
  (cond ((eq size 2)
	 (if (eq byteorder ?l)
	     (+ (ash (aref data (1+ offset)) 8)
		(aref data offset))
	   (+ (ash (aref data offset) 8)
	      (aref data (1+ offset)))))

	((eq size 4)
	 (if (eq byteorder ?l)
	     (+ (ash (aref data (+ 3 offset)) 24)
		(ash (aref data (+ 2 offset)) 16)
		(ash (aref data (1+ offset)) 8)
		(aref data offset))
	   (+ (ash (aref data offset) 24)
	      (ash (aref data (1+ offset)) 16)
	      (ash (aref data (+ 2 offset)) 8)
	      (aref data (+ 3 offset)))))))

(defun x-dnd-motif-value-to-list (value size byteorder)
  (let ((bytes (cond ((eq size 2)
		      (list (logand (ash value -8) ?\xff)
			    (logand value ?\xff)))

		     ((eq size 4)
		      (list (logand (ash value -24) ?\xff)
			    (logand (ash value -16) ?\xff)
			    (logand (ash value -8) ?\xff)
			    (logand value ?\xff))))))
    (if (eq byteorder ?l)
	(reverse bytes)
      bytes)))

(defun x-dnd-xm-unpack-targets-table-header (data)
  "Decode the header of DATA, a Motif targets table.
Return a list of the following fields with the given types:

    Field name        Type
  - BYTE_ORDER        BYTE
  - PROTOCOL          BYTE
  - TARGET_LIST_COUNT CARD16
  - TOTAL_DATA_SIZE   CARD32"
  (let* ((byte-order (aref data 0))
         (protocol (aref data 1))
         (target-list-count (x-dnd-get-motif-value
                             data 2 2 byte-order))
         (total-data-size (x-dnd-get-motif-value
                           data 4 4 byte-order)))
    (list byte-order protocol target-list-count
          total-data-size)))

(defun x-dnd-xm-read-single-rec (data i)
  "Read a single rec from DATA, a Motif targets table.
I is the offset into DATA to begin reading at.  Return a list
of (CONSUMED NTARGETS TARGETS), where CONSUMED is the number of
bytes read from DATA, NTARGETS is the total number of targets
inside the current rec, and TARGETS is a vector of atoms
describing the selection targets in the current rec."
  (let* ((byte-order (aref data 0))
         (n-targets (x-dnd-get-motif-value
                     data i 2 byte-order))
         (targets (make-vector n-targets nil))
         (consumed 0))
    (while (< consumed n-targets)
      (aset targets consumed (x-dnd-get-motif-value
                              data (+ i 2 (* consumed 4))
                              4 byte-order))
      (setq consumed (1+ consumed)))
    (list (+ 2 (* consumed 4)) n-targets targets)))

(defun x-dnd-xm-read-targets-table (frame)
  "Read the Motif targets table on FRAME.
Return a vector of vectors of numbers, which are the atoms of the
available selection targets for each index into the selection
table."
  (let* ((drag-window (x-window-property "_MOTIF_DRAG_WINDOW"
                                         frame "WINDOW" 0 nil t))
         (targets-data (x-window-property "_MOTIF_DRAG_TARGETS"
                                          frame "_MOTIF_DRAG_TARGETS"
                                          drag-window nil t))
         (header (x-dnd-xm-unpack-targets-table-header targets-data))
         (vec (make-vector (nth 2 header) nil))
         (current-byte 8)
         (i 0))
    (unless (stringp targets-data)
      (error "Expected format 8, got %s" (type-of targets-data)))
    (prog1 vec
      (while (< i (nth 2 header))
        (let ((rec (x-dnd-xm-read-single-rec targets-data
                                             current-byte)))
          (aset vec i (nth 2 rec))
          (setq current-byte (+ current-byte (car rec)))
          (setq i (1+ i))))
      (unless (eq current-byte (nth 3 header))
        (error "Targets table header says size is %d, but it is actually %d"
               (nth 3 header) current-byte)))))

(defun x-dnd-xm-read-targets (frame window selection)
  "Read targets of SELECTION on FRAME from the targets table.
WINDOW should be the drag-and-drop operation's initiator.
Return a vector of atoms containing the selection targets."
  (let* ((targets-table (x-dnd-xm-read-targets-table frame))
         (initiator-info (x-window-property selection frame
                                            "_MOTIF_DRAG_INITIATOR_INFO"
                                            window nil nil))
         (byte-order (aref initiator-info 0))
         (idx (x-dnd-get-motif-value initiator-info
                                     2 2 byte-order))
         (vector (aref targets-table idx))
         (i 0))
    (prog1 vector
      (while (< i (length vector))
        (aset vector i
              (intern (x-get-atom-name (aref vector i))))
        (setq i (1+ i))))))

(defvar x-dnd-motif-message-types
  '((0 . XmTOP_LEVEL_ENTER)
    (1 . XmTOP_LEVEL_LEAVE)
    (2 . XmDRAG_MOTION)
    (3 . XmDROP_SITE_ENTER)
    (4 . XmDROP_SITE_LEAVE)
    (5 . XmDROP_START)
    (6 . XmDROP_FINISH)
    (7 . XmDRAG_DROP_FINISH)
    (8 . XmOPERATION_CHANGED))
  "Mapping from numbers to Motif DND message types.")

(defvar x-dnd-motif-to-action
  '((1 . move)
    (2 . copy)
    (3 . link)	; Both 3 and 4 has been seen as link.
    (4 . link)
    (2 . private)) ; Motif does not have private, so use copy for private.
  "Mapping from number to operation for Motif DND.")

(defun x-dnd-handle-motif (event frame window _message-atom _format data)
  (let* ((message-type (cdr (assoc (logand (aref data 0) #x3f)
                                   x-dnd-motif-message-types)))
         (initiator-p (eq (ash (aref data 0) -7) 0))
	 (source-byteorder (aref data 1))
	 (my-byteorder (byteorder))
	 (source-flags (x-dnd-get-motif-value data 2 2 source-byteorder))
	 (source-action (cdr (assoc (logand ?\xF source-flags)
				    x-dnd-motif-to-action))))

    (when initiator-p
      (cond ((eq message-type 'XmTOP_LEVEL_ENTER)
	     (let* ((dnd-source (x-dnd-get-motif-value
			         data 8 4 source-byteorder))
		    (selection-atom (x-dnd-get-motif-value
				     data 12 4 source-byteorder))
                    (atom-name (x-get-atom-name selection-atom))
		    (types (x-dnd-xm-read-targets frame dnd-source
                                                  atom-name)))
	       (x-dnd-forget-drop frame)
	       (when types (x-dnd-save-state window nil nil
					     types dnd-source))))

	    ;; Can not forget drop here, LEAVE comes before DROP_START and
	    ;; we need the state in DROP_START.
	    ((eq message-type 'XmTOP_LEVEL_LEAVE)
	     nil)

	    ((eq message-type 'XmDRAG_MOTION)
	     (let* ((state (x-dnd-get-state-for-frame frame))
		    (timestamp (x-dnd-motif-value-to-list
			        (x-dnd-get-motif-value data 4 4
						       source-byteorder)
			        4 my-byteorder))
		    (x (x-dnd-motif-value-to-list
		        (x-dnd-get-motif-value data 8 2 source-byteorder)
		        2 my-byteorder))
		    (y (x-dnd-motif-value-to-list
		        (x-dnd-get-motif-value data 10 2 source-byteorder)
		        2 my-byteorder))
		    (dnd-source (aref state 6))
		    (first-move (not (aref state 3)))
		    (action-type (x-dnd-maybe-call-test-function
				  window
				  source-action))
		    (reply-action (car (rassoc (car action-type)
					       x-dnd-motif-to-action)))
		    (reply-flags
                     (if (posn-area (event-start event))
                         (x-dnd-motif-value-to-list ?\x20 ; 20: invalid drop site
                                                    2 my-byteorder)
		       (x-dnd-motif-value-to-list
		        (if reply-action
			    (+ reply-action
			       ?\x30                      ; 30:  valid drop site
			       ?\x700)                    ; 700: can do copy, move or link
		          ?\x30)                          ; 30:  drop site, but noop.
		        2 my-byteorder)))
		    (reply (append
			    (list
			     (+ ?\x80	; 0x80 indicates a reply.
			        (if first-move
				    3	; First time, reply is SITE_ENTER.
				  2))	; Not first time, reply is DRAG_MOTION.
			     my-byteorder)
			    reply-flags
			    timestamp
			    x
			    y)))
               (x-display-set-last-user-time timestamp)
	       (x-send-client-message frame
				      dnd-source
				      frame
				      "_MOTIF_DRAG_AND_DROP_MESSAGE"
				      8
				      reply)
               (dnd-handle-movement (event-start event))))

	    ((eq message-type 'XmOPERATION_CHANGED)
	     (let* ((state (x-dnd-get-state-for-frame frame))
		    (timestamp (x-dnd-motif-value-to-list
			        (x-dnd-get-motif-value data 4 4 source-byteorder)
			        4 my-byteorder))
		    (dnd-source (aref state 6))
		    (action-type (x-dnd-maybe-call-test-function
				  window
				  source-action))
		    (reply-action (car (rassoc (car action-type)
					       x-dnd-motif-to-action)))
		    (reply-flags
		     (if (posn-area (event-start event))
                         (x-dnd-motif-value-to-list ?\x20 ; 20: invalid drop site
                                                    2 my-byteorder)
		       (x-dnd-motif-value-to-list
		        (if reply-action
			    (+ reply-action
			       ?\x30   ; 30:  valid drop site
			       ?\x700) ; 700: can do copy, move or link
		          ?\x30)       ; 30:  drop site, but noop.
		        2 my-byteorder)))
		    (reply (append
			    (list
			     (+ ?\x80	; 0x80 indicates a reply.
			        8)	; 8 is OPERATION_CHANGED
			     my-byteorder)
			    reply-flags
			    timestamp)))
               (x-display-set-last-user-time timestamp)
	       (x-send-client-message frame
				      dnd-source
				      frame
				      "_MOTIF_DRAG_AND_DROP_MESSAGE"
				      8
				      reply)))

	    ((eq message-type 'XmDROP_START)
             (when (windowp window)
               (select-window window))
	     (let* ((x (x-dnd-motif-value-to-list
		        (x-dnd-get-motif-value data 8 2 source-byteorder)
		        2 my-byteorder))
		    (y (x-dnd-motif-value-to-list
		        (x-dnd-get-motif-value data 10 2 source-byteorder)
		        2 my-byteorder))
		    (selection-atom (x-dnd-get-motif-value
				     data 12 4 source-byteorder))
		    (atom-name (x-get-atom-name selection-atom))
                    (dnd-source (x-dnd-get-motif-value
			         data 16 4 source-byteorder)))

               ;; This might be a drop from a program that doesn't use
               ;; the Motif drag protocol.  Compute all the necessary
               ;; state here if that is true.
               (unless (and (x-dnd-get-state-for-frame frame)
                            (aref (x-dnd-get-state-for-frame frame) 2))
                 (x-dnd-forget-drop frame)
                 (let ((types (x-dnd-xm-read-targets frame dnd-source
                                                     atom-name)))
                   (x-dnd-save-state window nil nil types dnd-source)))

               (let* ((action-type (x-dnd-maybe-call-test-function
			            window
			            source-action))
		      (reply-action (and (not (posn-area (event-start event)))
                                         (car (rassoc (car action-type)
					              x-dnd-motif-to-action))))
		      (reply-flags
		       (x-dnd-motif-value-to-list
                        (if (posn-area (event-start event))
                            (+ ?\x20     ; 20: invalid drop site
                               ?\x200)   ; 200: drop cancel
		          (if reply-action
			      (+ reply-action
			         ?\x30   ; 30:  valid drop site
			         ?\x700) ; 700: can do copy, move or link
		            (+ ?\x30     ; 30:  drop site, but noop.
			       ?\x200))) ; 200: drop cancel.
		        2 my-byteorder))
		      (reply (append
			      (list
			       (+ ?\x80	; 0x80 indicates a reply.
			          5)	; DROP_START.
			       my-byteorder)
			      reply-flags
			      x y))
		      (timestamp (x-dnd-get-motif-value
			          data 4 4 source-byteorder))
		      action)
                 (x-display-set-last-user-time timestamp)
	         (x-send-client-message frame
				        dnd-source
				        frame
				        "_MOTIF_DRAG_AND_DROP_MESSAGE"
				        8
				        reply)
	         (unwind-protect
                     (setq action
		           (when (and reply-action atom-name)
		             (let* ((value (x-get-selection-internal
				            (intern atom-name)
				            (intern (x-dnd-current-type window))
                                            timestamp)))
		               (when value
			         (condition-case info
			             (x-dnd-drop-data event frame window value
					              (x-dnd-current-type window))
			           (error
			            (message "Error: %s" info)
			            nil))))))
	           (x-get-selection-internal
	            (intern atom-name)
	            (if action 'XmTRANSFER_SUCCESS 'XmTRANSFER_FAILURE)
	            timestamp)
	           (x-dnd-forget-drop frame)))))

            (t (message "Unknown Motif drag-and-drop message: %s"
                        (logand (aref data 0) #x3f)))))))


;;;



;;; Handling drops.

(defvar x-treat-local-requests-remotely)
(declare-function x-get-local-selection "xfns.c")

(defun x-dnd-convert-to-offix (targets local-selection)
  "Convert local selection data to OffiX data.
TARGETS should be the list of targets currently available in
`XdndSelection'.  Return a list of an OffiX type, and data
suitable for passing to `x-change-window-property', or nil if the
data could not be converted.
LOCAL-SELECTION should be the local selection data describing the
selection data to convert."
  (let ((x-treat-local-requests-remotely t)
        file-name-data string-data)
    (cond
     ((and (member "FILE_NAME" targets)
           (setq file-name-data
                 (x-get-local-selection local-selection 'FILE_NAME)))
      (if (string-match-p "\0" file-name-data)
          ;; This means there are multiple file names in
          ;; XdndSelection.  Convert the file name data to a format
          ;; that OffiX understands.
          (cons 'DndTypeFiles (concat file-name-data "\0\0"))
        (cons 'DndTypeFile (concat file-name-data "\0"))))
     ((and (member "STRING" targets)
           (setq string-data
                 (x-get-local-selection local-selection 'STRING)))
      (cons 'DndTypeText (encode-coding-string string-data
                                               'latin-1))))))

(defun x-dnd-do-offix-drop (targets x y frame window-id contents)
  "Perform an OffiX drop on WINDOW-ID with the given selection contents.
Return non-nil if the drop succeeded, or nil if it did not
happen, which can happen if TARGETS didn't contain anything that
the OffiX protocol can represent.

X and Y are the root window coordinates of the drop.  TARGETS is
the list of targets CONTENTS can be converted to, and CONTENTS is
the local selection data to drop onto the target window.

FRAME is the frame that will act as a source window for the
drop."
  (if-let* ((data (x-dnd-convert-to-offix targets contents))
            (type-id (car (rassq (car data)
                                 x-dnd-offix-id-to-name)))
            (source-id (string-to-number
                        (frame-parameter frame 'window-id)))
            (message-data (list type-id           ; l[0] = DataType
                                0                 ; l[1] = event->xbutton.state
                                source-id         ; l[2] = window
                                (+ x (* 65536 y)) ; l[3] = drop_x + 65536 * drop_y
                                1)))              ; l[4] = protocol version
    (prog1 t
      ;; Send a legacy (old KDE) message first.  Newer clients will
      ;; ignore it, since the protocol version is 1.
      (x-change-window-property "DndSelection"
                                (cdr data) frame
                                "STRING" 8 nil 0)
      (x-send-client-message frame window-id
                             frame "DndProtocol"
                             32 message-data)
      ;; Now send a modern _DND_PROTOCOL message.
      (x-change-window-property "_DND_SELECTION"
                                (cdr data) frame
                                "STRING" 8 nil 0)
      (x-send-client-message frame window-id
                             frame "_DND_PROTOCOL"
                             32 message-data))))

(defun x-dnd-handle-unsupported-drop (targets x y action window-id frame _time local-selection-data)
  "Return non-nil if the drop described by TARGETS and ACTION should not proceed.
X and Y are the root window coordinates of the drop.
FRAME is the frame the drop originated on.
WINDOW-ID is the X window the drop should happen to.
LOCAL-SELECTION-DATA is the local selection data of the drop."
  (let ((chosen-action nil))
    (not (and (or (eq action 'XdndActionCopy)
                  (eq action 'XdndActionMove))
              (not (and x-dnd-use-offix-drop local-selection-data
                        (or (not (eq x-dnd-use-offix-drop 'files))
                            (member "FILE_NAME" targets))
                        (when (x-dnd-do-offix-drop targets x
                                                   y frame window-id
                                                   local-selection-data)
                          (setq chosen-action 'XdndActionCopy))))
              (let ((delegate-p (or (member "STRING" targets)
                                    (member "UTF8_STRING" targets)
                                    (member "COMPOUND_TEXT" targets)
                                    (member "TEXT" targets))))
                (prog1 delegate-p
                  ;; A string will avoid the drop emulation done in C
                  ;; code, but won't be returned from `x-begin-drag'.
                  (setq chosen-action (unless delegate-p ""))))))
    chosen-action))

(defvar x-dnd-targets-list)
(defvar x-dnd-native-test-function)

(defun x-dnd-handle-native-drop (pos action)
  "Compute the action for a drop at POS.
Return the appropriate drag-and-drop action for a local drop at POS.
ACTION is the action given to `x-begin-drag'."
  (let ((state (funcall x-dnd-test-function
                        (posn-window pos)
                        (cdr (assoc (symbol-name action)
                                    x-dnd-xdnd-to-action))
                        (apply #'vector x-dnd-targets-list))))
    (when state
      (intern (car (rassq (car state) x-dnd-xdnd-to-action))))))

(setq x-dnd-native-test-function #'x-dnd-handle-native-drop)

;;; XDS protocol support.

(declare-function x-begin-drag "xfns.c")
(declare-function x-delete-window-property "xfns.c")
(defvar selection-converter-alist)

(defvar x-dnd-xds-current-file nil
  "The file name for which a direct save is currently being performed.")

(defvar x-dnd-xds-source-frame nil
  "The frame from which a direct save is currently being performed.")

(defvar x-dnd-xds-performed nil
  "Whether or not the drop target made a request for `XdndDirectSave0'.")

(defvar x-dnd-disable-motif-protocol)
(defvar x-dnd-use-unsupported-drop)

(defvar x-dnd-xds-testing nil
  "Whether or not XDS is being tested from ERT.
When non-nil, throw errors from the `XdndDirectSave0' converters
instead of returning \"E\".")

(defun x-dnd-handle-direct-save (_selection _type _value)
  "Handle a selection request for `XdndDirectSave'."
  (setq x-dnd-xds-performed t)
  (let* ((uri (x-window-property "XdndDirectSave0"
                                 x-dnd-xds-source-frame
                                 "AnyPropertyType" nil t))
         (local-file-uri (if (and (string-match "^file://\\([^/]*\\)" uri)
                                  (not (equal (match-string 1 uri) "")))
                             (dnd-get-local-file-uri uri)
                           uri))
         (local-name (and local-file-uri
                          (dnd-get-local-file-name local-file-uri))))
    (if (not local-name)
        '(STRING . "F")
      ;; We want errors to be signaled immediately during ERT
      ;; testing, instead of being silently handled.  (bug#56712)
      (if x-dnd-xds-testing
          (prog1 '(STRING . "S")
            (copy-file x-dnd-xds-current-file
                       local-name t)
            (when (equal x-dnd-xds-current-file
                         dnd-last-dragged-remote-file)
              (dnd-remove-last-dragged-remote-file)))
        (condition-case nil
            (progn
              (copy-file x-dnd-xds-current-file
                         local-name t)
              (when (equal x-dnd-xds-current-file
                           dnd-last-dragged-remote-file)
                (dnd-remove-last-dragged-remote-file)))
          (:success '(STRING . "S"))
          (error '(STRING . "E")))))))

(defun x-dnd-handle-octet-stream (_selection _type _value)
  "Handle a selection request for `application/octet-stream'.
Return the contents of the XDS file."
  (cons 'application/octet-stream
        (ignore-errors
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (setq buffer-file-coding-system 'binary)
            (insert-file-contents-literally x-dnd-xds-current-file)
            (buffer-substring-no-properties (point-min)
                                            (point-max))))))

(defun x-dnd-do-direct-save (file name frame allow-same-frame)
  "Perform a direct save operation on FILE, from FRAME.
FILE is the file containing the contents to drop.
NAME is the name that should be given to the file after dropping.
FRAME is the frame from which the drop will originate.
ALLOW-SAME-FRAME means whether or not dropping will be allowed
on FRAME.

Return the action taken by the drop target, or nil if no action
was taken, or the direct save failed."
  (dnd-remove-last-dragged-remote-file)
  (let ((file-name file)
        (original-file-name file)
        (selection-converter-alist
         (append '((XdndDirectSave0 . x-dnd-handle-direct-save)
                   (application/octet-stream . x-dnd-handle-octet-stream))
                 selection-converter-alist))
        (x-dnd-xds-current-file nil)
        (x-dnd-xds-source-frame frame)
        (x-dnd-xds-performed nil)
        ;; The XDS protocol is built on top of XDND, and cannot
        ;; possibly work with Motif or OffiX programs.
        (x-dnd-disable-motif-protocol t)
        (x-dnd-use-offix-drop nil)
        (x-dnd-use-unsupported-drop nil)
        (prop-deleted nil)
        encoded-name)
    (unwind-protect
        (progn
          (when (file-remote-p file)
            (setq file-name (file-local-copy file))
            (setq dnd-last-dragged-remote-file file-name)
            (add-hook 'kill-emacs-hook
                      #'dnd-remove-last-dragged-remote-file))
          (setq encoded-name
                (encode-coding-string name
                                      (or file-name-coding-system
                                          default-file-name-coding-system)))
          (setq x-dnd-xds-current-file file-name)
          (x-change-window-property "XdndDirectSave0" encoded-name
                                    frame "text/plain" 8 nil)
          (gui-set-selection 'XdndSelection (concat "file://" file-name))
          ;; FIXME: this does not work with GTK file managers,
          ;; since they always reach for `text/uri-list' first,
          ;; contrary to the spec.
          (let ((action (x-begin-drag '("XdndDirectSave0" "text/uri-list"
                                        "application/octet-stream")
                                      'XdndActionDirectSave
                                      frame nil allow-same-frame)))
            (if (not x-dnd-xds-performed)
                action
              (let ((property (x-window-property "XdndDirectSave0" frame
                                                 "AnyPropertyType" nil t)))
                (setq prop-deleted t)
                ;; "System-G" deletes the property upon success.
                (and (or (null property)
                         (and (stringp property)
                              (not (equal property ""))))
                     action)))))
      (unless prop-deleted
        (x-delete-window-property "XdndDirectSave0" frame))
      ;; Delete any remote copy that was made.
      (when (and (not (equal file-name original-file-name))
                 x-dnd-xds-performed)
        (delete-file file-name)))))

(defun x-dnd-save-direct (need-name filename)
  "Handle dropping a file FILENAME that should be saved first, asking the user.
NEED-NAME non-nil means the caller requests the full absolute
file name of FILENAME under which to save it; FILENAME is just
the base name in that case.  The function then prompts the user
for where to save to file and returns the result to the caller.

NEED-NAME nil means the file was saved as FILENAME (which should
be the full absolute file name in that case).  The function then
refreshes the Dired display, if the current buffer is in Dired
mode, or visits the file otherwise.

This function is intended to be the value of `x-dnd-direct-save-function',
which see."
  (if need-name
      (let ((file-name (read-file-name "Write file: "
                                       default-directory
                                       nil nil filename)))
        (when (file-exists-p file-name)
          (unless (y-or-n-p (format-message
                             "File `%s' exists; overwrite? " file-name))
            (setq file-name nil)))
        file-name)
    ;; TODO: move this to dired.el once a platform-agonistic
    ;; interface can be found.
    (if (derived-mode-p 'dired-mode)
        (revert-buffer)
      (find-file filename))))

(defun x-dnd-save-direct-immediately (need-name filename)
  "Handle dropping a file FILENAME that should be saved first.
Like `x-dnd-save-direct', but do not prompt for the file name;
instead, return its absolute file name for saving in the current
directory.

This function is intended to be the value of `x-dnd-direct-save-function',
which see."
  (if need-name
      (let ((file-name (expand-file-name filename)))
        (when (file-exists-p file-name)
          (unless (y-or-n-p (format-message
                             "File `%s' exists; overwrite? " file-name))
            (setq file-name nil)))
        file-name)
    ;; TODO: move this to dired.el once a platform-agonistic
    ;; interface can be found.
    (if (derived-mode-p 'dired-mode)
        (revert-buffer)
      (find-file filename))))

(defun x-dnd-handle-octet-stream-for-drop (save-to)
  "Save the contents of the XDS selection to SAVE-TO.
Return non-nil if successful, nil otherwise."
  (ignore-errors
    (let ((coding-system-for-write 'raw-text)
          (data (x-get-selection-internal 'XdndSelection
                                          'application/octet-stream)))
      (when data
        (write-region data nil save-to)
        t))))

(defun x-dnd-handle-xds-drop (event window source version)
  "Handle an XDS (X Direct Save) protocol drop.
EVENT is the drag-n-drop event containing the drop.
WINDOW is the window on top of which the drop is supposed to happen.
SOURCE is the X window that sent the drop.
VERSION is the version of the XDND protocol understood by SOURCE."
  (if (not (windowp window))
      ;; We can't perform an XDS drop if there's no window from which
      ;; to determine the current directory.
      (let* ((start (event-start event))
             (frame (posn-window start)))
        (x-send-client-message frame source frame
                               "XdndFinished" 32
                               (list (string-to-number
                                      (frame-parameter frame
                                                       'outer-window-id)))))
    (let ((desired-name (x-window-property "XdndDirectSave0"
                                           (window-frame window)
                                           ;; We currently don't handle
                                           ;; any alternative character
                                           ;; encodings.
                                           "text/plain" source))
          (frame (window-frame window))
          (success nil) save-to save-to-remote hostname)
      (unwind-protect
          (when (stringp desired-name)
            (setq desired-name (decode-coding-string
                                desired-name
                                (or file-name-coding-system
                                    default-file-name-coding-system)))
            (let ((name (expand-file-name
                         (funcall x-dnd-direct-save-function
                                  t desired-name))))
              (setq save-to name save-to-remote name))
            (when save-to
              (if (file-remote-p save-to)
                  (setq hostname (file-remote-p save-to 'host)
                        save-to (file-local-name save-to))
                (setq hostname (system-name)))
              (with-selected-window window
                (let ((uri (format "file://%s%s" hostname save-to)))
                  (x-change-window-property "XdndDirectSave0"
                                            (encode-coding-string
                                             (url-encode-url uri) 'ascii)
                                            frame "text/plain" 8 nil source)
                  (let ((result (x-get-selection-internal 'XdndSelection
                                                          'XdndDirectSave0)))
                    (cond ((equal result "F")
                           (setq success
                                 (x-dnd-handle-octet-stream-for-drop save-to-remote))
                           (unless success
                             (x-change-window-property "XdndDirectSave0" ""
                                                       frame "text/plain" 8
                                                       nil source)))
                          ((equal result "S")
                           (setq success t))
                          ((equal result "E")
                           (setq success nil))
                          (t (error "Broken implementation of XDS: got %s in reply"
                                    result)))
                    (when success
                      (funcall x-dnd-direct-save-function nil save-to-remote)))))))
        ;; We assume XDS always comes from a client supporting version 2
        ;; or later, since custom actions aren't present before.
        (x-send-client-message frame source frame
                               "XdndFinished" 32
                               (list (string-to-number
                                      (frame-parameter frame
                                                       'outer-window-id))
                                     (if (>= version 5)
                                         (if success 1 0)
                                       0)
                                     (if (or (not success)
                                             (< version 5))
                                         0
                                       "XdndActionDirectSave")))))))

;; Internal wheel movement.

(defvar x-dnd-wheel-function)

(defun x-dnd-note-wheel-movement (position button state time)
  "Note wheel movement at POSITION.
POSITION is a mouse position list describing the position of the
wheel movement.
BUTTON is the wheel button that was pressed.
STATE is the X modifier state at the time of the wheel movement.
TIME is the X server time at which the wheel moved."
  (when (posn-window position)
    (with-selected-window (posn-window position)
      (let ((count (x-dnd-note-click button time)))
        (x-dnd-mwheel-scroll button count state)))))

(setq x-dnd-wheel-function #'x-dnd-note-wheel-movement)

(provide 'x-dnd)

;;; x-dnd.el ends here
