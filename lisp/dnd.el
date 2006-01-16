;;; dnd.el --- drag and drop support.

;; Copyright (C) 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides the generic handling of the drop part only.
;; Different DND backends (X11, W32, etc.) that handle the platform
;; specific DND parts call the functions here to do final delivery of
;; a drop.

;;; Code:

;;; Customizable variables


;;;###autoload
(defcustom dnd-protocol-alist
  '(
    ("^file:///" . dnd-open-local-file)	; XDND format.
    ("^file://"  . dnd-open-file)	; URL with host
    ("^file:"    . dnd-open-local-file)	; Old KDE, Motif, Sun
    )

  "The functions to call for different protocols when a drop is made.
This variable is used by `dnd-handle-one-url' and `dnd-handle-file-name'.
The list contains of (REGEXP . FUNCTION) pairs.
The functions shall take two arguments, URL, which is the URL dropped and
ACTION which is the action to be performed for the drop (move, copy, link,
private or ask).
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
If no match is found, the URL is inserted as text by calling `dnd-insert-text'.
The function shall return the action done (move, copy, link or private)
if some action was made, or nil if the URL is ignored."
  :version "22.1"
  :type '(repeat (cons (regexp) (function)))
  :group 'dnd)



(defcustom dnd-open-file-other-window nil
  "If non-nil, always use find-file-other-window to open dropped files."
  :version "22.1"
  :type 'boolean
  :group 'dnd)


;; Functions

(defun dnd-handle-one-url (window action arg)
  "Handle one dropped url by calling the appropriate handler.
The handler is first located by looking at `dnd-protocol-alist'.
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
If no match is found, just call `dnd-insert-text'.
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
       (dolist (bf dnd-protocol-alist)
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
     (progn
       (dnd-insert-text window action uri)
       (setq ret 'private)))
    ret))


(defun dnd-get-local-file-uri (uri)
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

(defun dnd-get-local-file-name (uri &optional must-exist)
  "Return file name converted from file:/// or file: syntax.
URI is the uri for the file.  If MUST-EXIST is given and non-nil,
only return non-nil if the file exists.
Return nil if URI is not a local file."
  (let ((f (cond ((string-match "^file:///" uri)	; XDND format.
		  (substring uri (1- (match-end 0))))
		 ((string-match "^file:" uri)		; Old KDE, Motif, Sun
		  (substring uri (match-end 0))))))
    (when (and f must-exist)
      (let* ((decoded-f (decode-coding-string
			 f
			 (or file-name-coding-system
			     default-file-name-coding-system)))
	     (try-f (if (file-readable-p decoded-f) decoded-f f)))
	(when (file-readable-p try-f) try-f)))))


(defun dnd-open-local-file (uri action)
  "Open a local file.
The file is opened in the current window, or a new window if
`dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file:file-name or file:///file-name.
The last / in file:/// is part of the file name.  ACTION is ignored."

  (let* ((f (dnd-get-local-file-name uri t)))
    (if (and f (file-readable-p f))
	(progn
	  (if dnd-open-file-other-window
	      (find-file-other-window f)
	    (find-file f))
	  'private)
      (error "Can not read %s" uri))))

(defun dnd-open-file (uri action)
  "Open a local or remote file.
The file is opened in the current window, or a new window if
`dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file://hostname/file-name.  ACTION is ignored.
The last / in file://hostname/ is part of the file name."

  ;; The hostname may be our hostname, in that case, convert to a local
  ;; file.  Otherwise return nil.
  (let ((local-file (dnd-get-local-file-uri uri)))
    (if local-file (dnd-open-local-file local-file action)
      (error "Remote files not supported"))))


(defun dnd-insert-text (window action text)
  "Insert text at point or push to the kill ring if buffer is read only.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (if (or buffer-read-only
	  (not (windowp window)))
      (progn
	(kill-new text)
	(message "%s"
	 (substitute-command-keys
	  "The dropped text can be accessed with \\[yank]")))
    (insert text))
  action)


(provide 'dnd)

;; arch-tag: 0472f6a5-2e8f-4304-9e44-1a0877c771b7
;;; dnd.el ends here
