;;; x-dnd.el --- drag and drop support for X.

;; Copyright (C) 2004
;;  Free Software Foundation, Inc.

;; Author: Jan Dj,Ad(Brv <jan.h.d@swipnet.se>
;; Maintainer: FSF
;; Keywords: window, drag, drop

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

;; This file provides the drop part only.  Currently supported protocols
;; are XDND and the old KDE 1.x protocol.

;;; Code:

;;; Customizable variables


(defcustom x-dnd-test-function 'x-dnd-default-test-function
  "The function drag and drop uses to determine if to accept or reject a drop.
The function takes three arguments, WINDOW ACTION and TYPES.
WINDOW is where the mouse is when the function is called.  WINDOW may be a
frame if the mouse isn't over a real window (i.e. menu bar, tool bar or
scroll bar).  ACTION is the suggested action from the drag and drop source,
one of the symbols move, copy link or ask.  TYPES is a list of available types
for the drop.

The function shall return nil to reject the drop or a cons with two values,
the wanted action as car and the wanted type as cdr.  The wanted action
can be copy, move, link, ask or private.
The default value for this variable is `x-dnd-default-test-function'."
  :type 'symbol
  :group 'x)

(defcustom x-dnd-protocol-alist
  '(
    ("^file:///" . x-dnd-open-local-file)	; XDND format.
    ("^file://"  . x-dnd-open-file)		; URL with host
    ("^file:"    . x-dnd-open-local-file)	; Old KDE, Motif, Sun
    )

  "The functions to call for different protocols when a drop is made.
This variable is used by `x-dnd-handle-uri-list' and `x-dnd-handle-moz-url'.
The list contains of (REGEXP . FUNCTION) pairs.
The functions shall take two arguments, URL, which is the URL dropped and
ACTION which is the action to be performed for the drop (move, copy, link,
private or ask).
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
Insertion of text is not handeled by these functions, see `x-dnd-types-alist'
for that.
The function shall return the action done (move, copy, link or private)
if some action was made, or nil if the URL is ignored."
  :type 'alist
  :group 'x)


(defcustom x-dnd-types-alist
  '(
    ("text/uri-list" . x-dnd-handle-uri-list)
    ("text/x-moz-url" . x-dnd-handle-moz-url)
    ("FILE_NAME" . x-dnd-handle-uri-list)
    ("_NETSCAPE_URL" . x-dnd-handle-uri-list)
    ("UTF8_STRING" . x-dnd-insert-utf8-text)
    ("text/plain;charset=UTF-8" . x-dnd-insert-utf8-text)
    ("text/plain;charset=utf-8" . x-dnd-insert-utf8-text)
    ("text/unicode" . x-dnd-insert-utf16-text)
    ("text/plain" . x-dnd-insert-text)
    ("STRING" . x-dnd-insert-text)
    ("TEXT"   . x-dnd-insert-text)
    )
  "Which function to call to handle a drop of that type.
If the type for the drop is not present, or the function is nil,
the drop is rejected.  The function takes three arguments, WINDOW, ACTION
and DATA.  WINDOW is where the drop occured, ACTION is the action for
this drop (copy, move, link, private or ask) as determined by a previous
call to `x-dnd-test-function'.  DATA is the drop data.
The function shall return the action used (copy, move, link or private) if drop
is successful, nil if not."
  :type 'alist
  :group 'x)

(defcustom x-dnd-open-file-other-window nil
  "If non-nil, always use find-file-other-window to open dropped files."
  :type 'boolean
  :group 'x)

;; Internal variables

(defvar x-dnd-known-types
  '("text/uri-list"
    "text/x-moz-url"
    "FILE_NAME"
    "_NETSCAPE_URL"
    "UTF8_STRING"
    "text/plain;charset=UTF-8"
    "text/plain;charset=utf-8"
    "text/unicode"
    "text/plain"
    "STRING"
    "TEXT"
    )
  "The types accepted by default for dropped data.
The types are chosen in the order they appear in the list.")

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
the action we want for the drop.")

(defvar x-dnd-empty-state [nil nil nil nil nil nil])



(defun x-dnd-init-frame (&optional frame)
  "Setup drag and drop for FRAME (i.e. create appropriate properties)."
  (x-dnd-init-xdnd-for-frame frame))

(defun x-dnd-get-state-cons-for-frame (frame-or-window)
  "Return the entry in x-dnd-current-state for a frame or window."
  (let* ((frame (if (framep frame-or-window) frame-or-window
		  (window-frame frame-or-window)))
	 (display (frame-parameter frame 'display)))
    (if (not (assoc display x-dnd-current-state))
	(push (cons display x-dnd-empty-state) x-dnd-current-state))
    (assoc display x-dnd-current-state)))

(defun x-dnd-get-state-for-frame (frame-or-window)
  "Return the state in x-dnd-current-state for a frame or window."
  (cdr (x-dnd-get-state-cons-for-frame frame-or-window)))

(defun x-dnd-default-test-function (window action types)
  "The default test function for drag and drop.
WINDOW is where the mouse is when this function is called.  It may be a frame
if the mouse is over the menu bar, scroll bar or tool bar.
ACTION is the suggested action from the source, and TYPES are the
types the drop data can have.  This function only accepts drops with
types in `x-dnd-known-types'.  It always returns the action private."
  (let ((type (x-dnd-choose-type types)))
    (when type (cons 'private type))))


(defun x-dnd-current-type (frame-or-window)
  "Return the type we want the DND data to be in for the current drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (aref (x-dnd-get-state-for-frame frame-or-window) 4))

(defun x-dnd-forget-drop (frame-or-window)
  "Remove all state for the last drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (setcdr (x-dnd-get-state-cons-for-frame frame-or-window) x-dnd-empty-state))

(defun x-dnd-maybe-call-test-function (window action)
  "Call `x-dnd-test-function' if something has changed.
WINDOW is the window the mouse is over.  ACTION is the suggested
action from the source.  If nothing has changed, return the last
action and type we got from `x-dnd-test-function'."
  (let ((buffer (when (and (windowp window) (window-live-p window))
		  (window-buffer window)))
	(current-state (x-dnd-get-state-for-frame window)))
    (when (or (not (equal buffer (aref current-state 0)))
	      (not (equal window (aref current-state 1)))
	      (not (equal action (aref current-state 3))))
      (save-excursion
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
				  (when handler action-type)))))))
  (let ((current-state (x-dnd-get-state-for-frame window)))
    (cons (aref current-state 5)
	  (aref current-state 4))))

(defun x-dnd-save-state (window action action-type &optional types)
  "Save the state of the current drag and drop.
WINDOW is the window the mouse is over.  ACTION is the action suggested
by the source.  ACTION-TYPE is the result of calling `x-dnd-test-function'.
If given, TYPES are the types for the drop data that the source supports."
  (let ((current-state (x-dnd-get-state-for-frame window)))
    (aset current-state 5 (car action-type))
    (aset current-state 4 (cdr action-type))
    (aset current-state 3 action)
    (if types (aset current-state 2 types))
    (aset current-state 1 window)
    (aset current-state 0 (if (and (windowp window)
				   (window-live-p window))
			      (window-buffer window) nil))
    (setcdr (x-dnd-get-state-cons-for-frame window) current-state)))


(defun x-dnd-test-and-save-state (window action types)
  "Test if drop shall be accepted, and save the state for future reference.
ACTION is the suggested action by the source.
TYPES is a list of types the source supports."
  (x-dnd-save-state window
		    action
		    (x-dnd-maybe-call-test-function window action)
		    types))

(defun x-dnd-handle-one-url (window action arg)
  "Handle one dropped url by calling the appropriate handler.
The handler is first localted by looking at `x-dnd-protocol-alist'.
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
If no match is found, just call `x-dnd-insert-text'.
WINDOW is where the drop happend, ACTION is the action for the drop,
ARG is the URL that has been dropped.
Returns ACTION."
  (require 'browse-url)
  (let* ((uri (replace-regexp-in-string
	       "%[A-Z0-9][A-Z0-9]"
	       (lambda (arg)
		 (format "%c" (string-to-number (substring arg 1) 16)))
	       arg))
	 ret)
    (or
     (catch 'done
       (dolist (bf x-dnd-protocol-alist)
	 (when (string-match (car bf) uri)
	   (setq ret (funcall (cdr bf) uri action))
	   (throw 'done t)))
       nil)
     (when (not (functionp browse-url-browser-function))
       (catch 'done
	 (dolist (bf browse-url-browser-function)
	   (when (string-match (car bf) uri)
	     (setq ret 'private)
	     (funcall (cdr bf) uri action)
	     (throw 'done t)))
	 nil))
     (x-dnd-insert-text window action uri))
    ret))


(defun x-dnd-get-local-file-uri (uri)
  "Return an uri converted to file:/// syntax if uri is a local file.
Return nil if URI is not a local file."

  ;; The hostname may be our hostname, in that case, convert to a local
  ;; file.  Otherwise return nil.  TODO:  How about an IP-address as hostname?
  (let ((hostname (when (string-match "^file://\\([^/]*\\)" uri)
		      (downcase (match-string 1 uri))))
	(system-name-no-dot
	 (downcase (if (string-match "^[^\\.]+" system-name)
		       (match-string 0 system-name)
		     system-name))))
    (when (and hostname
	     (or (string-equal "localhost" hostname)
		 (string-equal (downcase system-name) hostname)
		 (string-equal system-name-no-dot hostname)))
	(concat "file://" (substring uri (+ 7 (length hostname)))))))

(defun x-dnd-get-local-file-name (uri &optional must-exist)
  "Return file name converted from file:/// or file: syntax.
URI is the uri for the file.  If MUST-EXIST is given and non-nil,
only return non-nil if the file exists.
Return nil if URI is not a local file."
  (let ((f (cond ((string-match "^file:///" uri)	; XDND format.
		  (substring uri (1- (match-end 0))))
		 ((string-match "^file:" uri)		; Old KDE, Motif, Sun
		  (substring uri (match-end 0)))
		 nil)))
    (when (and f must-exist)
      (let* ((decoded-f (decode-coding-string 
			 f
			 (or file-name-coding-system
			     default-file-name-coding-system)))
	     (try-f (if (file-readable-p decoded-f) decoded-f f)))
	(when (file-readable-p try-f) try-f)))))
	

(defun x-dnd-open-local-file (uri action)
  "Open a local file.
The file is opened in the current window, or a new window if
`x-dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file:file-name or file:///file-name.
The last / in file:/// is part of the file name.  ACTION is ignored."

  (let* ((f (x-dnd-get-local-file-name uri t)))
    (when f
      (if (file-readable-p f)
	  (progn
	    (if x-dnd-open-file-other-window
		(find-file-other-window f)
	      (find-file f))
	    'private)
	(error "Can not read %s (%s)" f uri)))))

(defun x-dnd-open-file (uri action)
  "Open a local or remote file.
The file is opened in the current window, or a new window if
`x-dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file://hostname/file-name.  ACTION is ignored.
The last / in file://hostname/ is part of the file name."

  ;; The hostname may be our hostname, in that case, convert to a local
  ;; file.  Otherwise return nil.
  (let ((local-file (x-dnd-get-local-file-uri uri)))
    (when local-file (x-dnd-open-local-file local-file action))))


(defun x-dnd-handle-moz-url (window action data)
  "Handle one item of type text/x-moz-url.
WINDOW is the window where the drop happened.  ACTION is ignored.
DATA is the moz-url, which is formatted as two strings separated by \r\n.
The first string is the URL, the second string is the title of that URL.
DATA is encoded in utf-16.  Decode the URL and call `x-dnd-handle-uri-list'."
  (let* ((string (decode-coding-string data 'utf-16le))  ;; ALWAYS LE???
	 (strings (split-string string "[\r\n]" t))
	 ;; Can one drop more than one moz-url ??  Assume not.
	 (url (car strings))
	 (title (car (cdr strings))))
    (x-dnd-handle-uri-list window action url)))

(defun x-dnd-insert-utf8-text (window action text)
  "Decode the UTF-8 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (x-dnd-insert-text window action (decode-coding-string text 'utf-8)))

(defun x-dnd-insert-utf16-text (window action text)
  "Decode the UTF-16 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (x-dnd-insert-text window action (decode-coding-string text 'utf-16le)))

(defun x-dnd-insert-text (window action text)
  "Insert text at point or push to the kill ring if buffer is read only.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (if (or buffer-read-only
	  (not (windowp window)))
      (progn
	(kill-new text)
	(message
	 (substitute-command-keys
	  "The dropped text can be accessed with \\[yank]")))
    (insert text))
  action)

(defun x-dnd-handle-uri-list (window action string)
  "Split an uri-list into separate URIs and call `x-dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the uri-list as a string.  The URIs are separated by \r\n."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	retval)
    (dolist (bf uri-list)
      ;; If one URL is handeled, treat as if the whole drop succeeded.
      (let ((did-action (x-dnd-handle-one-url window action bf)))
	(when did-action (setq retval did-action))))
    retval))


(defun x-dnd-choose-type (types &optional known-types)
  "Choose which type we want to receive for the drop.
TYPES are the types the source of the drop offers, a vector of type names
as strings or symbols.  Select among the types in `x-dnd-known-types' or
KNOWN-TYPES if given,  and return that type name.
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
EVENT is the client message for the drop, FRAME is the frame the drop occurred
on.  WINDOW is the window of FRAME where the drop happened.  DATA is the data
received from the source, and type is the type for DATA, see
`x-dnd-types-alist').

Returns the action used (move, copy, link, private) if drop was successful,
nil if not."
  (let* ((type-info (assoc type x-dnd-types-alist))
	 (handler (cdr type-info))
	 (state (x-dnd-get-state-for-frame frame))
	 (action (aref state 5))
	 (w (posn-window (event-start event))))
    (when handler
      (if (and (windowp w) (window-live-p w))
	  ;; If dropping in a window, open files in that window rather
	  ;; than in a new widow.
	  (let ((x-dnd-open-file-other-window nil))
	    (goto-char (posn-point (event-start event)))
	    (funcall handler window action data))
	(let ((x-dnd-open-file-other-window t))  ;; Dropping on non-window.
	  (select-frame frame)
	  (funcall handler window action data))))))

(defun x-dnd-handle-drag-n-drop-event (event)
  "Receive drag and drop events (X client messages).
Currently XDND and old KDE 1.x protocols are recognized.
TODO: Add Motif and OpenWindows."
  (interactive "e")
  (let* ((client-message (car (cdr (cdr event))))
	 (window (posn-window (event-start event)))
	 (message-atom (aref client-message 0))
	 (frame (aref client-message 1))
	 (format (aref client-message 2))
	 (data (aref client-message 3)))

    (cond ((equal "DndProtocol" message-atom)	;; Old KDE 1.x.
	   (x-dnd-handle-old-kde event frame window message-atom format data))

	  ((and (> (length message-atom) 4)	;; XDND protocol.
		(equal "Xdnd" (substring message-atom 0 4)))
	   (x-dnd-handle-xdnd event frame window message-atom format data))

	  (t (error "Unknown DND atom: %s" message-atom)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Old KDE protocol.  Only dropping of files.

(defun x-dnd-handle-old-kde (event frame window message format data)
  "Open the files in a KDE 1.x drop."
  (let ((values (x-window-property "DndSelection" frame nil 0 t)))
    (x-dnd-handle-uri-list window 'private
			   (replace-regexp-in-string "\0$" "" values))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  XDND protocol.

(defvar x-dnd-xdnd-to-action
  '(("XdndActionPrivate" . private)
    ("XdndActionCopy" . copy)
    ("XdndActionMove" . move)
    ("XdndActionLink" . link)
    ("XdndActionAsk" . ask))
  "Mapping from XDND action types to lisp symbols.")

(defun x-dnd-init-xdnd-for-frame (frame)
  "Set the XdndAware for FRAME to indicate that we do XDND."
  (x-change-window-property "XdndAware"
			    '(5)	;; The version of XDND we support.
			    frame "ATOM" 32 t))

(defun x-dnd-get-drop-width-height (frame w accept)
  "Return the widht/height to be sent in a XDndStatus message.
FRAME is the frame and W is the window where the drop happened.
If ACCEPT is nil return 0 (empty rectangle),
otherwise if W is a window, return its widht/height,
otherwise return the frame width/height."
  (if accept
      (if (windowp w)   ;; w is not a window if dropping on the menu bar,
			;; scroll bar or tool bar.
	  (let ((edges (window-inside-pixel-edges w)))
	    (cons
	     (- (nth 2 edges) (nth 0 edges))	;; right - left
	     (- (nth 3 edges) (nth 1 edges))))	;; bottom - top
	(cons (frame-pixel-width frame)
	      (frame-pixel-height frame)))
    0))

(defun x-dnd-get-drop-x-y (frame w)
  "Return the x/y coordinates to be sent in a XDndStatus message.
Coordinates are required to be absolute.
FRAME is the frame and W is the window where the drop happened.
If W is a window, return its absolute corrdinates,
otherwise return the frame coordinates."
  (let* ((frame-left (frame-parameter frame 'left))
	 ;; If the frame is outside the display, frame-left looks like
	 ;; '(0 -16).  Extract the -16.
	 (frame-real-left (if (consp frame-left) (car (cdr frame-left))
			    frame-left))
	 (frame-top (frame-parameter frame 'top))
	 (frame-real-top (if (consp frame-top) (car (cdr frame-top))
			   frame-top)))
    (if (windowp w)
	(let ((edges (window-inside-pixel-edges w)))
	  (cons
	   (+ frame-real-left (nth 0 edges))
	   (+ frame-real-top (nth 1 edges))))
      (cons frame-real-left frame-real-top))))

(defun x-dnd-handle-xdnd (event frame window message format data)
  "Receive one XDND event (client message) and send the appropriate reply.
EVENT is the client message.  FRAME is where the mouse is now.
WINDOW is the window within FRAME where the mouse is now.
FORMAT is 32 (not used).  MESSAGE is the data part of an XClientMessageEvent."
  (cond ((equal "XdndEnter" message)
	 (let ((version (ash (car (aref data 1)) -8))
	       (more-than-3 (cdr (aref data 1)))
	       (dnd-source (aref data 0)))
	   (x-dnd-save-state 
	    window nil nil
	    (if (> more-than-3 0)
		(x-window-property "XdndTypeList"
				   frame "AnyPropertyType"
				   dnd-source nil t)
	      (vector (x-get-atom-name (aref data 2))
		      (x-get-atom-name (aref data 3))
		      (x-get-atom-name (aref data 4)))))))

	((equal "XdndPosition" message)
	 (let* ((x (car (aref data 2)))
		(y (cdr (aref data 2)))
		(action (x-get-atom-name (aref data 4)))
		(dnd-source (aref data 0))
		(dnd-time (aref data 3))
		(action-type (x-dnd-maybe-call-test-function
			      window
			      (cdr (assoc action x-dnd-xdnd-to-action))))
		(reply-action (car (rassoc (car action-type)
					   x-dnd-xdnd-to-action)))
		(accept ;; 1 = accept, 0 = reject
		 (if (and reply-action action-type) 1 0))
		(list-to-send
		 (list (string-to-number
			(frame-parameter frame 'outer-window-id))
		       accept ;; 1 = Accept, 0 = reject.
		       (x-dnd-get-drop-x-y frame window)
		       (x-dnd-get-drop-width-height 
			frame window (eq accept 1))
		       (or reply-action 0)
		       )))
	   (x-send-client-message
	    frame dnd-source frame "XdndStatus" 32 list-to-send)
	   ))

	((equal "XdndLeave" message)
	 (x-dnd-forget-drop window))

	((equal "XdndDrop" message)
	 (if (windowp window) (select-window window))
	 (let* ((dnd-source (aref data 0))
		(value (and (x-dnd-current-type window)
			    ;; Get selection with target DELETE if move.
			    (x-get-selection-internal
			     'XdndSelection
			     (intern (x-dnd-current-type window)))))
		success action ret-action)

	   (setq action (if value
			    (condition-case info
				(x-dnd-drop-data event frame window value 
						 (x-dnd-current-type window))
			      (error 
			       (message "Error: %s" info)
			       nil))))

	   (setq success (if action 1 0))
	   (setq ret-action
		 (if (eq success 1)
		     (or (car (rassoc action x-dnd-xdnd-to-action))
			 "XdndActionPrivate")
		   0))

	   (x-send-client-message
	    frame dnd-source frame "XdndFinished" 32
	    (list (string-to-number (frame-parameter frame 'outer-window-id))
		  success	;; 1 = Success, 0 = Error
		  (if success "XdndActionPrivate" 0)
		  ))
	   (x-dnd-forget-drop window)))

	(t (error "Unknown XDND message %s %s" message data))))

(provide 'x-dnd)

;;; arch-tag: b621fb7e-50da-4323-850b-5fc71ae64621
;;; x-dnd.el ends here
