;;; pgtk-dnd.el --- drag and drop support for GDK -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: window, drag, drop
;; Package: emacs

;; Significant portions taken from x-dnd.el.

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

;; This file provides the receiving side of the GDK drag and drop
;; mechanism.

;;; Code:

(require 'dnd)

;;; Customizable variables
(defcustom pgtk-dnd-test-function #'pgtk-dnd-default-test-function
  "The function drag and drop uses to determine if to accept or reject a drop.
The function takes three arguments, WINDOW, ACTION and TYPES.
WINDOW is where the mouse is when the function is called.  WINDOW
may be a frame if the mouse isn't over a real window (i.e. menu
bar, tool bar or scroll bar).  ACTION is the suggested action
from the drag and drop source, one of the symbols move, copy,
link or ask.  TYPES is a vector of available types for the drop.

Each element of TYPE should either be a string (containing the
name of the type's X atom), or a symbol, whose name will be used.

The function shall return nil to reject the drop or a cons with
two values, the wanted action as car and the wanted type as cdr.
The wanted action can be copy, move, link, ask or private.

The default value for this variable is `pgtk-dnd-default-test-function'."
  :version "22.1"
  :type 'symbol
  :group 'pgtk)

(defcustom pgtk-dnd-types-alist
  `((,(purecopy "text/uri-list") . pgtk-dnd-handle-uri-list)
    (,(purecopy "FILE_NAME") . pgtk-dnd-handle-file-name)
    (,(purecopy "UTF8_STRING") . pgtk-dnd-insert-utf8-text)
    (,(purecopy "text/plain;charset=UTF-8") . pgtk-dnd-insert-utf8-text)
    (,(purecopy "text/plain;charset=utf-8") . pgtk-dnd-insert-utf8-text)
    (,(purecopy "text/plain") . dnd-insert-text)
    (,(purecopy "COMPOUND_TEXT") . pgtk-dnd-insert-ctext)
    (,(purecopy "STRING") . dnd-insert-text)
    (,(purecopy "TEXT")   . dnd-insert-text))
  "Which function to call to handle a drop of that type.
If the type for the drop is not present, or the function is nil,
the drop is rejected.  The function takes three arguments, WINDOW, ACTION
and DATA.  WINDOW is where the drop occurred, ACTION is the action for
this drop (copy, move, link, private or ask) as determined by a previous
call to `pgtk-dnd-test-function'.  DATA is the drop data.
The function shall return the action used (copy, move, link or private)
if drop is successful, nil if not."
  :version "22.1"
  :type 'alist
  :group 'pgtk)

(defcustom pgtk-dnd-known-types
  (mapcar 'purecopy '("text/uri-list"
                      "FILE_NAME"
                      "UTF8_STRING"
                      "text/plain;charset=UTF-8"
                      "text/plain;charset=utf-8"
                      "text/plain"
                      "COMPOUND_TEXT"
                      "STRING"
                      "TEXT"))
  "The types accepted by default for dropped data.
The types are chosen in the order they appear in the list."
  :version "22.1"
  :type '(repeat string)
  :group 'pgtk)

;; Internal variables

(defvar pgtk-dnd-current-state nil
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

(declare-function pgtk-get-selection-internal "pgtkselect.c")
(declare-function pgtk-register-dnd-targets "pgtkselect.c")

(defvar pgtk-dnd-empty-state [nil nil nil nil nil nil nil])

(defun pgtk-dnd-init-frame (&optional frame)
  "Setup drag and drop for FRAME (i.e. create appropriate properties)."
  (when (eq 'pgtk (window-system frame))
    (pgtk-register-dnd-targets frame pgtk-dnd-known-types)))

(defun pgtk-dnd-get-state-cons-for-frame (frame-or-window)
  "Return the entry in `pgtk-dnd-current-state' for a frame or window."
  (let* ((frame (if (framep frame-or-window) frame-or-window
		  (window-frame frame-or-window)))
	 (display (frame-parameter frame 'display)))
    (if (not (assoc display pgtk-dnd-current-state))
	(push (cons display (copy-sequence pgtk-dnd-empty-state))
	      pgtk-dnd-current-state))
    (assoc display pgtk-dnd-current-state)))

(defun pgtk-dnd-get-state-for-frame (frame-or-window)
  "Return the state in `pgtk-dnd-current-state' for a frame or window."
  (cdr (pgtk-dnd-get-state-cons-for-frame frame-or-window)))

(defun pgtk-dnd-default-test-function (_window _action types)
  "The default test function for drag and drop.
WINDOW is where the mouse is when this function is called.  It may be
a frame if the mouse is over the menu bar, scroll bar or tool bar.
ACTION is the suggested action from the source, and TYPES are the
types the drop data can have.  This function only accepts drops with
types in `pgtk-dnd-known-types'.  It always returns the action `copy'."
  (let ((type (pgtk-dnd-choose-type types)))
    (when type (cons 'copy type))))

(defun pgtk-dnd-current-type (frame-or-window)
  "Return the type we want the DND data to be in for the current drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (aref (pgtk-dnd-get-state-for-frame frame-or-window) 4))

(defun pgtk-dnd-forget-drop (frame-or-window)
  "Remove all state for the last drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (setcdr (pgtk-dnd-get-state-cons-for-frame frame-or-window)
	  (copy-sequence pgtk-dnd-empty-state)))

(defun pgtk-dnd-maybe-call-test-function (window action)
  "Call `pgtk-dnd-test-function' if something has changed.
WINDOW is the window the mouse is over.  ACTION is the suggested
action from the source.  If nothing has changed, return the last
action and type we got from `pgtk-dnd-test-function'."
  (let ((buffer (when (window-live-p window)
		  (window-buffer window)))
	(current-state (pgtk-dnd-get-state-for-frame window)))
    (unless (and (equal buffer (aref current-state 0))
                 (equal window (aref current-state 1))
                 (equal action (aref current-state 3)))
      (save-current-buffer
	(when buffer (set-buffer buffer))
	(let* ((action-type (funcall pgtk-dnd-test-function
				     window
				     action
				     (aref current-state 2)))
	       (handler (cdr (assoc (cdr action-type) pgtk-dnd-types-alist))))
	  ;; Ignore action-type if we have no handler.
	  (setq current-state
		(pgtk-dnd-save-state window
				  action
				  (when handler action-type)))))))
  (let ((current-state (pgtk-dnd-get-state-for-frame window)))
    (cons (aref current-state 5)
	  (aref current-state 4))))

(defun pgtk-dnd-save-state (window action action-type &optional types extra-data)
  "Save the state of the current drag and drop.
WINDOW is the window the mouse is over.  ACTION is the action suggested
by the source.  ACTION-TYPE is the result of calling `pgtk-dnd-test-function'.
If given, TYPES are the types for the drop data that the source supports.
EXTRA-DATA is data needed for a specific protocol."
  (let ((current-state (pgtk-dnd-get-state-for-frame window)))
    (aset current-state 5 (car action-type))
    (aset current-state 4 (cdr action-type))
    (aset current-state 3 action)
    (when types (aset current-state 2 types))
    (when extra-data (aset current-state 6 extra-data))
    (aset current-state 1 window)
    (aset current-state 0 (and (window-live-p window) (window-buffer window)))
    (setcdr (pgtk-dnd-get-state-cons-for-frame window) current-state)))


(defun pgtk-dnd-handle-moz-url (window action data)
  "Handle one item of type text/x-moz-url.
WINDOW is the window where the drop happened.  ACTION is ignored.
DATA is the moz-url, which is formatted as two strings separated by \\r\\n.
The first string is the URL, the second string is the title of that URL.
DATA is encoded in utf-16.  Decode the URL and call `pgtk-dnd-handle-uri-list'."
  ;; Mozilla and applications based on it use text/unicode, but it is
  ;; impossible to tell if it is le or be.  Use what the machine Emacs
  ;; runs on uses.  This loses if dropping between machines
  ;; with different endian-ness, but it is the best we can do.
  (let* ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le))
	 (string (decode-coding-string data coding))
	 (strings (split-string string "[\r\n]" t))
	 ;; Can one drop more than one moz-url ??  Assume not.
	 (url (car strings)))
    (pgtk-dnd-handle-uri-list window action url)))

(defun pgtk-dnd-insert-utf8-text (window action text)
  "Decode the UTF-8 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (dnd-insert-text window action (decode-coding-string text 'utf-8)))

(defun pgtk-dnd-insert-utf16-text (window action text)
  "Decode the UTF-16 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  ;; See comment in pgtk-dnd-handle-moz-url about coding.
  (let ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le)))
    (dnd-insert-text window action (decode-coding-string text coding))))

(defun pgtk-dnd-insert-ctext (window action text)
  "Decode the compound text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (dnd-insert-text window action
		   (decode-coding-string text
					 'compound-text-with-extensions)))

(defun pgtk-dnd-handle-uri-list (window action string)
  "Split an uri-list into separate URIs and call `dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the uri-list as a string.  The URIs are separated by \\r\\n."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	retval)
    (dolist (bf uri-list)
      ;; If one URL is handled, treat as if the whole drop succeeded.
      (let ((did-action (dnd-handle-one-url window action bf)))
	(when did-action (setq retval did-action))))
    retval))

(defun pgtk-dnd-handle-file-name (window action string)
  "Convert file names to URLs and call `dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the file names as a string, separated by nulls."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	(coding (or file-name-coding-system
		    default-file-name-coding-system))
	retval)
    (dolist (bf uri-list)
      ;; If one URL is handled, treat as if the whole drop succeeded.
      (if coding (setq bf (encode-coding-string bf coding)))
      (let* ((file-uri (concat "file://"
			       (mapconcat 'url-hexify-string
					  (split-string bf "/") "/")))
	     (did-action (dnd-handle-one-url window action file-uri)))
	(when did-action (setq retval did-action))))
    retval))


(defun pgtk-dnd-choose-type (types &optional known-types)
  "Choose which type we want to receive for the drop.
TYPES are the types the source of the drop offers, a vector of type names
as strings or symbols.  Select among the types in `pgtk-dnd-known-types' or
KNOWN-TYPES if given, and return that type name.
If no suitable type is found, return nil."
  (let* ((known-list (or known-types pgtk-dnd-known-types))
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
	(pgtk-dnd-choose-type types (cdr known-list))
      found)))

(defun pgtk-dnd-drop-data (event frame window data type)
  "Drop one data item onto a frame.
EVENT is the client message for the drop, FRAME is the frame the drop
occurred on.  WINDOW is the window of FRAME where the drop happened.
DATA is the data received from the source, and type is the type for DATA,
see `pgtk-dnd-types-alist').

Returns the action used (move, copy, link, private) if drop was successful,
nil if not."
  (let* ((type-info (assoc type pgtk-dnd-types-alist))
	 (handler (cdr type-info))
	 (state (pgtk-dnd-get-state-for-frame frame))
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

(defun pgtk-dnd-handle-drag-n-drop-event (event)
  "Receive drag and drop events (X client messages).
Currently XDND, Motif and old KDE 1.x protocols are recognized."
  (interactive "e")
  (let* ((client-message (car (cdr (cdr event))))
	 (window (posn-window (event-start event)))
         (frame (if (framep window)
                    window
                  (window-frame window))))
    (pgtk-dnd-handle-gdk event frame window client-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  GDK protocol.

(declare-function pgtk-update-drop-status "pgtkselect.c")
(declare-function pgtk-drop-finish "pgtkselect.c")

(defvar pgtk-dnd-clear-data-on-motion nil
  "Whether or not to obtain the new list of targets upon the next drag motion.
For more details, see the function `pgtk-dnd-handle-gdk'.")

(defun pgtk-dnd-handle-gdk (event frame window client-message)
  "Handle drag-n-drop EVENT on FRAME.
WINDOW should be the window the event happened on top of.
CLIENT-MESSAGE is the detailed description of the drag-and-drop
message."
  (cond
   ;; We can't handle `drag-leave' immediately, since that signal is
   ;; also sent right before `drag-drop', and there is no reliable way
   ;; to distinguish a signal sent because the source left from one
   ;; sent prior to a drop.  Instead, set a flag that tells Emacs to
   ;; clear the drag-and-drop state if anything other than a drop is
   ;; received.
   ((not client-message) ; drag-leave
    (setq pgtk-dnd-clear-data-on-motion t))
   ((eq (car client-message) 'lambda) ; drag-motion
    (let ((state (pgtk-dnd-get-state-for-frame frame)))
      (unless (and (aref state 0) ;; This is actually an entry.
                   (not pgtk-dnd-clear-data-on-motion))
        (setq pgtk-dnd-clear-data-on-motion nil)
        ;; Forget the drop first, or else the list of targets will not
        ;; be cleared if it is nil.
        (pgtk-dnd-forget-drop window)
        (pgtk-dnd-save-state window nil nil
                             (pgtk-get-selection-internal
                              (nth 1 client-message) 'TARGETS)
                             t)
        (setq state (pgtk-dnd-get-state-for-frame frame)))
      (let* ((action (nth 3 client-message))
             (time (nth 2 client-message))
             (action-type (pgtk-dnd-maybe-call-test-function window
                                                             action)))
        ;; Get the selection contents now.  GdkWaylandSelection
        ;; becomes unavailable immediately after `drag-drop' is sent.
        (let* ((current-type (pgtk-dnd-current-type window))
               (current-action-type (car-safe (aref state 6))))
          (when (and current-type
                     (not (equal current-action-type action-type)))
            (aset state 6 (cons action-type
                                (pgtk-get-selection-internal
                                 (nth 1 client-message)
                                 (intern current-type))))))
        (pgtk-update-drop-status (car action-type) time)
        (dnd-handle-movement (event-start event)))))
   ((eq (car client-message) 'quote) ; drag-drop
    (let* ((state (pgtk-dnd-get-state-for-frame frame))
           (timestamp (nth 2 client-message))
           (value (and (pgtk-dnd-current-type window)
                       (or (cdr-safe (aref state 6))
                           (pgtk-get-selection-internal
                            (nth 1 client-message)
                            (intern (pgtk-dnd-current-type window))
                            timestamp))))
           action)
      (unwind-protect
          (setq action (when value
                         (condition-case info
			     (pgtk-dnd-drop-data
                              event frame window value
			      (pgtk-dnd-current-type window))
			   (error
			    (message "Error: %s" info)
			    nil))))
        (pgtk-drop-finish action timestamp (eq action 'move))
        (pgtk-dnd-forget-drop window))))))

(provide 'pgtk-dnd)

;;; pgtk-dnd.el ends here
