;;; dnd.el --- drag and drop support  -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2026 Free Software Foundation, Inc.

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

;; This file provides the generic handling of the drop part only.
;; Different DND backends (X11, W32, etc.) that handle the platform
;; specific DND parts call the functions here to do final delivery of
;; a drop.

;;; Code:

;;; Customizable variables

(eval-when-compile
  (require 'cl-lib))

(defgroup dnd nil
  "Handling data from drag and drop."
  :group 'environment)

;;;###autoload
(defcustom dnd-protocol-alist
  '(("^file:///"    . dnd-open-local-file) ; XDND format.
    ("^file://[^/]" . dnd-open-file)       ; URL with host
    ("^file:/[^/]"  . dnd-open-local-file) ; Old KDE, Motif, Sun
    ("^file:[^/]"   . dnd-open-local-file) ; MS-Windows
    ("^\\(https?\\|ftp\\|nfs\\)://" . dnd-open-file))
  "The functions to call for different protocols when a drop is made.
This variable is used by `dnd-handle-multiple-urls'.
The list contains of (REGEXP . FUNCTION) pairs.
The functions shall take two arguments, URL, which is the URL dropped and
ACTION which is the action to be performed for the drop (move, copy, link,
private or ask).
If a function's `dnd-multiple-handler' property is set, it is provided
a list of each URI dropped instead.
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
If no match is found, the URL is inserted as text by calling `dnd-insert-text'.
The function shall return the action done (move, copy, link or private)
if some action was made, or nil if the URL is ignored."
  :version "30.1"
  :type '(repeat (cons (regexp) (function)))
  :group 'dnd)


(defcustom dnd-open-remote-file-function
  (if (eq system-type 'windows-nt)
      #'dnd-open-local-file
    #'dnd-open-remote-url)
  "The function to call when opening a file on a remote machine.
The function will be called with two arguments, URI and ACTION.
See `dnd-open-file' for details.
If nil, then dragging remote files into Emacs will result in an error.
Predefined functions are `dnd-open-local-file' and `dnd-open-remote-url'.
`dnd-open-local-file' attempts to open a remote file using its UNC name and
is the default on MS-Windows.  `dnd-open-remote-url' uses `url-handler-mode'
and is the default except for MS-Windows."
  :version "22.1"
  :type 'function
  :group 'dnd)


(defcustom dnd-open-file-other-window nil
  "If non-nil, always use `find-file-other-window' to open dropped files."
  :version "22.1"
  :type 'boolean
  :group 'dnd)

(defcustom dnd-scroll-margin nil
  "The scroll margin inside a window underneath the cursor during drag-and-drop.
If the mouse moves this many lines close to the top or bottom of
a window while dragging text, then that window will be scrolled
down and up respectively."
  :type '(choice (const :tag "Don't scroll during mouse movement")
                 (integer :tag "This many lines from window top or bottom"))
  :version "29.1"
  :group 'dnd)

(defcustom dnd-indicate-insertion-point nil
  "Whether or not point should follow the position of the mouse.
If non-nil, the point of the window underneath the mouse will be
adjusted to reflect where any text will be inserted upon drop
when the mouse moves while receiving a drop from another
program."
  :type 'boolean
  :version "29.1"
  :group 'dnd)

(defcustom dnd-direct-save-remote-files 'x
  "Whether or not to perform a direct save of remote files.
This is compatible with fewer programs, but means dropped files
will be saved with their actual file names, and not a temporary
file name provided by TRAMP.

This defaults to `x', which means to save that way only on X
Windows."
  :type '(choice (const :tag "Only use direct save on X Windows" x)
                 (const :tag "Use direct save everywhere" t)
                 (const :tag "Don't use direct save")))

;; Functions

(defun dnd-handle-movement (posn)
  "Handle mouse movement to POSN when receiving a drop from another program."
  (when (windowp (posn-window posn))
    (with-selected-window (posn-window posn)
      (when (and dnd-scroll-margin
                 ;; TODO: handle scroll bars reasonably.
                 (not (posn-area posn)))
        (ignore-errors
          (let* ((row (cdr (posn-col-row posn)))
                 (window (when (windowp (posn-window posn))
                           (posn-window posn)))
                 (text-height (window-text-height window))
                 ;; Make sure it's possible to scroll both up
                 ;; and down if the margin is too large for the
                 ;; window.
                 (margin (min (/ text-height 3) dnd-scroll-margin)))
            ;; At 2 lines, the window becomes too small for any
            ;; meaningful scrolling.
            (unless (<= text-height 2)
              (cond
               ;; Inside the bottom scroll margin, scroll up.
               ((> row (- text-height margin))
                (with-selected-window window
                  (scroll-up 1)))
               ;; Inside the top scroll margin, scroll down.
               ((< row margin)
                (with-selected-window window
                  (scroll-down 1))))))))
      (when dnd-indicate-insertion-point
        (let ((pos (posn-point posn)))
          ;; We avoid errors here, since on some systems this runs
          ;; when waiting_for_input is non-zero, and that aborts on
          ;; error.
          (if (and pos (<= (point-min) pos (point-max)))
              (goto-char pos)
            pos))))))

(defun dnd-handle-one-url (window action url)
  "Handle one dropped url by calling the appropriate handler.
The handler is first located by looking at `dnd-protocol-alist'.
If no match is found here, `browse-url-handlers' and
`browse-url-default-handlers' are searched for a match.
If no match is found, just call `dnd-insert-text'.  WINDOW is
where the drop happened, ACTION is the action for the drop, URL
is what has been dropped.  Returns ACTION.

This function has been obsolete since Emacs 30.1; it has been
supplanted by `dnd-handle-multiple-urls'."
  (let (ret)
    (or
     (catch 'done
       (dolist (bf dnd-protocol-alist)
	 (when (string-match (car bf) url)
	   (setq ret (funcall (cdr bf) url action))
	   (throw 'done t)))
       nil)
     (catch 'done
       (let ((browser (browse-url-select-handler url 'internal)))
         (when browser
           (setq ret 'private)
           (funcall browser url action)
           (throw 'done t)))
       nil)
     (progn
       (dnd-insert-text window action url)
       (setq ret 'private)))
    ret))

(make-obsolete 'dnd-handle-one-url 'dnd-handle-multiple-urls "30.1")

(defun dnd-handle-multiple-urls (window urls action)
  "Select a handler for, then open, each element of URLS.
The argument ACTION is the action which must be taken, much as
that to `dnd-begin-file-drag'.

Assign and give each URL to one of the \"DND handler\" functions
listed in the variable `dnd-protocol-alist'.  When multiple
handlers matching the same subset of URLs exist, give precedence
to the handler assigned the greatest number of URLs.

If a handler is a symbol with the property
`dnd-multiple-handler', call it with ACTION and a list of every
URL it is assigned.  Otherwise, call it once for each URL
assigned with ACTION and the URL in question.

Subsequently open URLs that don't match any handlers opened with
any handler selected by `browse-url-select-handler', and failing
even that, insert them with `dnd-insert-text'.

Return a symbol designating the actions taken by each DND handler
called.  If all DND handlers called return the same symbol,
return that symbol; otherwise, or if no DND handlers are called,
return `private'.

Do not rely on the contents of URLS after calling this function,
for it will be modified."
  (let ((list nil) (return-value nil))
    (with-selected-window window
      (dolist (handler dnd-protocol-alist)
        (let ((pattern (car handler))
              (handler (cdr handler)))
          (dolist (uri urls)
            (when (string-match pattern uri)
              (let ((cell (or (cdr (assq handler list))
                              (let ((cell (cons handler nil)))
                                (push cell list)
                                cell))))
                (unless (memq uri cell)
                  (setcdr cell (cons uri (cdr cell)))))))))
      (setq list (nreverse list))
      ;; While unassessed handlers still exist...
      (while list
        ;; Sort list by the number of URLs assigned to each handler.
        (setq list (sort list (lambda (first second)
                                (> (length (cdr first))
                                   (length (cdr second))))))
        ;; Call the handler in its car before removing each URL from
        ;; URLs.
        (let ((handler (caar list))
              (entry-urls (cdar list)))
          (setq list (cdr list))
          (when entry-urls
            (if (and (symbolp handler)
                     (get handler 'dnd-multiple-handler))
                (progn
                  (let ((value (funcall handler entry-urls action)))
                    (if (or (not return-value)
                            (eq return-value value))
                        (setq return-value value)
                      (setq return-value 'private)))
                  (dolist (url entry-urls)
                    (setq urls (delq url urls))
                    ;; And each handler-URL list after this.
                    (dolist (item list)
                      (setcdr item (delq url (cdr item))))))
              (dolist (url entry-urls)
                (let ((value (funcall handler url action)))
                  (if (or (not return-value) (eq return-value value))
                      (setq return-value value)
                    (setq return-value 'private)))
                (setq urls (delq url urls))
                ;; And each handler-URL list after this.
                (dolist (item list)
                  (setcdr item (delq url (cdr item)))))))))
      ;; URLS should now incorporate only those which haven't been
      ;; assigned their own handlers.
      (dolist (leftover urls)
        (setq return-value 'private)
        (if-let* ((handler (browse-url-select-handler leftover
                                                      'internal)))
            (funcall handler leftover action)
          (dnd-insert-text window action leftover)))
      (or return-value 'private))))

(defun dnd-get-local-file-uri (uri)
  "Return an uri converted to file:/// syntax if uri is a local file.
Return nil if URI is not a local file."

  ;; The hostname may be our hostname, in that case, convert to a local
  ;; file.  Otherwise return nil.  TODO:  How about an IP-address as hostname?
  (let ((sysname (system-name)))
    (let ((hostname (when (string-match "^file://\\([^/]*\\)" uri)
		      (downcase (match-string 1 uri))))
	  (sysname-no-dot
	   (downcase (if (string-match "^[^\\.]+" sysname)
			 (match-string 0 sysname)
		       sysname))))
      (when (and hostname
                 (not (eq system-type 'windows-nt))
		 (or (string-equal "localhost" hostname)
		     (string-equal (downcase sysname) hostname)
		     (string-equal sysname-no-dot hostname)))
	(concat "file://" (substring uri (+ 7 (length hostname))))))))

(defvar dnd-unescape-file-uris t
  "Whether to unescape file: URIs before they are opened.
Bind this to nil when providing `dnd-get-local-file-name' with a
file name that may incorporate URI escape sequences.")

(defun dnd--unescape-uri (uri)
  ;; Merge with corresponding code in URL library.
  (replace-regexp-in-string
   "%[[:xdigit:]][[:xdigit:]]"
   (lambda (arg)
     (let ((str (make-string 1 0)))
       (aset str 0 (string-to-number (substring arg 1) 16))
       str))
   uri t t))

;; https://lists.gnu.org/r/emacs-devel/2006-05/msg01060.html
(defun dnd-get-local-file-name (uri &optional must-exist)
  "Return file name converted from file:/// or file: syntax.
URI is the uri for the file.  If MUST-EXIST is given and non-nil,
only return non-nil if the file exists.
Return nil if URI is not a local file."
  (let ((f (cond ((string-match "^file:///" uri)	; XDND format.
		  (substring uri (1- (match-end 0))))
		 ((string-match "^file:" uri)		; Old KDE, Motif, Sun
		  (substring uri (match-end 0)))))
	(coding (if (equal system-type 'windows-nt)
		    ;; W32 pretends that file names are UTF-8 encoded.
		    'utf-8
		  (or file-name-coding-system
		      default-file-name-coding-system))))
    (and f (setq f (decode-coding-string
                    (if dnd-unescape-file-uris
                        (dnd--unescape-uri f) f)
                    coding)))
    (when (and f must-exist (not (file-readable-p f)))
      (setq f nil))
    f))

(defun dnd-open-local-file (uri _action)
  "Open a local file.
The file is opened in the current window, or a new window if
`dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file:file-name or file:///file-name.
The last / in file:/// is part of the file name.  If the system
natively supports unc file names, then remote urls of the form
file://server-name/file-name will also be handled by this function.
An alternative for systems that do not support unc file names is
`dnd-open-remote-url'.  ACTION is ignored."

  (let* ((f (dnd-get-local-file-name uri t)))
    (if (and f (file-readable-p f))
	(progn
	  (if dnd-open-file-other-window
	      (find-file-other-window f)
	    (find-file f))
          (file-name-history--add f)
	  'private)
      (error "Can not read %s" uri))))

(defun dnd-open-remote-url (uri _action)
  "Open a remote file with `find-file' and `url-handler-mode'.
Turns `url-handler-mode' on if not on before.  The file is opened in the
current window, or a new window if `dnd-open-file-other-window' is set.
URI is the url for the file.  ACTION is ignored."
  (progn
    (require 'url-handlers)
    (or url-handler-mode (url-handler-mode))
    (if dnd-open-file-other-window
	(find-file-other-window uri)
      (find-file uri))
    'private))


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
      (if dnd-open-remote-file-function
	  (funcall dnd-open-remote-file-function uri action)
	(error "Remote files not supported")))))


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


;;; Functions for dragging stuff to other programs.  These build upon
;;; the lower-level `x-begin-drag' interface, but take care of data
;;; types and abstracting around the different return values.

(defvar dnd-last-dragged-remote-file nil
  "If non-nil, the name of a local copy of the last remote file that was dragged.
This may also be a list of files, if multiple files were dragged.
It can't be removed immediately after the drag-and-drop operation
completes, since there is no way to determine when the drop
target has finished opening it.  So instead, this file is removed
when Emacs exits or the user drags another file.")

(defun dnd-remove-last-dragged-remote-file ()
  "Remove the local copy of the last remote file to be dragged.
If `dnd-last-dragged-remote-file' is a list, remove all the files
in that list instead."
  (when dnd-last-dragged-remote-file
    (unwind-protect
        (if (consp dnd-last-dragged-remote-file)
            (mapc #'delete-file dnd-last-dragged-remote-file)
          (delete-file dnd-last-dragged-remote-file))
      (setq dnd-last-dragged-remote-file nil)))
  (remove-hook 'kill-emacs-hook
               #'dnd-remove-last-dragged-remote-file))

(declare-function x-begin-drag "xfns.c")

(defun dnd-begin-text-drag (text &optional frame action allow-same-frame)
  "Begin dragging TEXT from FRAME.
Initiate a drag-and-drop operation allowing the user to drag text
from Emacs to another program (the drop target), then block until
the drop is completed or is canceled.

If the drop completed, return the action that the drop target
actually performed, which can be one of the following symbols:

  - `copy', which means TEXT was inserted by the drop target.

  - `move', which means TEXT was inserted, and the caller should
    additionally delete TEXT from its source (such as the buffer
    where it originated).

  - `private', which means the drop target chose to perform an
    unspecified action.

Return nil if the drop was canceled.

TEXT is a string containing text that will be inserted by the
program where the drop happened.  FRAME is the frame where the
mouse is currently held down, or nil, which stands for the
current frame.  ACTION is one of the symbols `copy' or `move',
where `copy' means that the text should be inserted by the drop
target, and `move' means the same as `copy', but in addition
the caller might have to delete TEXT from its source after this
function returns.  If ALLOW-SAME-FRAME is nil, ignore any drops
on FRAME itself.

This function might return immediately if no mouse buttons are
currently being held down.  It should only be called upon a
`down-mouse-1' (or similar) event.

This function is only supported on X Windows, macOS/GNUstep, and Haiku;
on all other platforms it will signal an error."
  (unless (fboundp 'x-begin-drag)
    (error "Dragging text from Emacs is not supported by this window system"))
  (gui-set-selection 'XdndSelection text)
  (unless action
    (setq action 'copy))
  (let ((return-value
         (x-begin-drag '(;; Traditional X selection targets used by GTK, the
                         ;; Motif drag-and-drop protocols, and programs like
                         ;; Xterm.  `STRING' is also used on NS and Haiku.
                         "STRING" "TEXT" "COMPOUND_TEXT" "UTF8_STRING"
                         ;; Used by Xdnd clients that strictly comply with
                         ;; the standard (i.e. Qt programs).
                         "text/plain" "text/plain;charset=utf-8")
                       (cl-ecase action
                         (copy 'XdndActionCopy)
                         (move 'XdndActionMove))
                       frame nil allow-same-frame)))
    (cond
     ((eq return-value 'XdndActionCopy) 'copy)
     ((eq return-value 'XdndActionMove) 'move)
     ((not return-value) nil)
     (t 'private))))

(defun dnd-begin-file-drag (file &optional frame action allow-same-frame)
  "Begin dragging FILE from FRAME.
Initiate a drag-and-drop operation allowing the user to drag a file
from Emacs to another program (the drop target), then block until
the drop happens or is canceled.

Return the action that the drop target actually performed, which
can be one of the following symbols:

  - `copy', which means FILE was opened by the drop target.

  - `move', which means FILE was moved to another location by the
    drop target.

  - `link', which means a symbolic link was created to FILE by
    the drop target, usually a file manager.

  - `private', which means the drop target chose to perform an
    unspecified action.

Return nil if the drop was canceled.

FILE is the file name that will be sent to the program where the
drop happened.  If it is a remote file, Emacs will make a
temporary copy and pass that.  FRAME is the frame where the mouse
is currently held down, or nil (which means to use the current
frame).  ACTION is one of the symbols `copy', `move' or `link',
where `copy' means that the file should be opened or copied by
the drop target, `move' means the drop target should move the
file to another location, and `link' means the drop target should
create a symbolic link to FILE.  It is an error to specify `link'
as the action if FILE is a remote file.  If ALLOW-SAME-FRAME is
nil, any drops on FRAME itself will be ignored.

This function might return immediately if no mouse buttons are
currently being held down.  It should only be called upon a
`down-mouse-1' (or similar) event.

This function is only supported on X Windows, macOS/GNUstep, and Haiku;
on all other platforms it will signal an error."
  (unless (fboundp 'x-begin-drag)
    (error "Dragging files from Emacs is not supported by this window system"))
  (dnd-remove-last-dragged-remote-file)
  (unless action
    (setq action 'copy))
  (if (and (or (and (eq dnd-direct-save-remote-files 'x)
                    (eq (framep (or frame
                                    (selected-frame)))
                        'x))
               (and dnd-direct-save-remote-files
                    (not (eq dnd-direct-save-remote-files 'x))))
           (eq action 'copy)
           (file-remote-p file))
      (dnd-direct-save file (file-name-nondirectory file)
                       frame allow-same-frame)
    (let ((original-file file))
      (when (file-remote-p file)
        (if (eq action 'link)
            (error "Cannot create symbolic link to remote file")
          (setq file (file-local-copy file))
          (setq dnd-last-dragged-remote-file file)
          (add-hook 'kill-emacs-hook
                    #'dnd-remove-last-dragged-remote-file)))
      (gui-set-selection 'XdndSelection
                         (propertize (expand-file-name file) 'text/uri-list
                                     (concat "file://"
                                             (expand-file-name file))))
      (let ((return-value
             (x-begin-drag '(;; Xdnd types used by GTK, Qt, and most other
                             ;; modern programs that expect filenames to
                             ;; be supplied as URIs.
                             "text/uri-list" "text/x-xdnd-username"
                             ;; Traditional X selection targets used by
                             ;; programs supporting the Motif
                             ;; drag-and-drop protocols.  Also used by NS
                             ;; and Haiku.
                             "FILE_NAME" "FILE" "HOST_NAME"
                             ;; ToolTalk filename.  Mostly used by CDE
                             ;; programs.
                             "_DT_NETFILE")
                           (cl-ecase action
                             (copy 'XdndActionCopy)
                             (move 'XdndActionMove)
                             (link 'XdndActionLink))
                           frame nil allow-same-frame)))
        (cond
         ((eq return-value 'XdndActionCopy) 'copy)
         ((eq return-value 'XdndActionMove)
          (prog1 'move
            ;; If original-file is a remote file, delete it from the
            ;; remote as well.
            (when (file-remote-p original-file)
              (ignore-errors
                (delete-file original-file)))))
         ((eq return-value 'XdndActionLink) 'link)
         ((not return-value) nil)
         (t 'private))))))

(defun dnd-begin-drag-files (files &optional frame action allow-same-frame)
  "Begin dragging FILES from FRAME.
This is like `dnd-begin-file-drag', except with multiple files.
FRAME, ACTION and ALLOW-SAME-FRAME mean the same as in
`dnd-begin-file-drag'.

FILES is a list of files that will be dragged.  If the drop
target doesn't support dropping multiple files, the first file in
FILES will be dragged.

This function is only supported on X Windows, macOS/GNUstep, and Haiku;
on all other platforms it will signal an error."
  (unless (fboundp 'x-begin-drag)
    (error "Dragging files from Emacs is not supported by this window system"))
  (dnd-remove-last-dragged-remote-file)
  (let* ((new-files (copy-sequence files))
         (tem new-files))
    (while tem
      (setcar tem (expand-file-name (car tem)))
      (when (file-remote-p (car tem))
        (when (eq action 'link)
          (error "Cannot create symbolic link to remote file"))
        (condition-case error
            (progn (setcar tem (file-local-copy (car tem)))
                   (push (car tem) dnd-last-dragged-remote-file))
          (error (message "Failed to download file: %s" error)
                 (setcar tem nil))))
      (setq tem (cdr tem)))
    (when dnd-last-dragged-remote-file
      (add-hook 'kill-emacs-hook
                #'dnd-remove-last-dragged-remote-file))
    ;; Remove any files that failed to download from a remote host.
    (setq new-files (delq nil new-files))
    (unless new-files
      (error "No files were specified or no remote file could be downloaded"))
    (unless action
      (setq action 'copy))
    (gui-set-selection 'XdndSelection
                       (propertize (car new-files)
                                   'text/uri-list
                                   (cl-loop for file in new-files
                                            collect (concat "file://" file)
                                            into targets finally return
                                            (apply #'vector targets))
                                   'FILE_NAME (apply #'vector new-files)))
    (let ((return-value
           (x-begin-drag '(;; Xdnd types used by GTK, Qt, and most other
                           ;; modern programs that expect filenames to
                           ;; be supplied as URIs.
                           "text/uri-list" "text/x-xdnd-username"
                           ;; Traditional X selection targets used by
                           ;; programs supporting the Motif
                           ;; drag-and-drop protocols.  Also used by NS
                           ;; and Haiku.
                           "FILE_NAME" "HOST_NAME")
                         (cl-ecase action
                           (copy 'XdndActionCopy)
                           (move 'XdndActionMove)
                           (link 'XdndActionLink))
                         frame nil allow-same-frame)))
      (cond
       ((eq return-value 'XdndActionCopy) 'copy)
       ((eq return-value 'XdndActionMove)
        (prog1 'move
          ;; If original-file is a remote file, delete it from the
          ;; remote as well.
          (dolist (original-file files)
            (when (file-remote-p original-file)
              (ignore-errors
                (delete-file original-file))))))
       ((eq return-value 'XdndActionLink) 'link)
       ((not return-value) nil)
       (t 'private)))))

(declare-function x-dnd-do-direct-save "x-dnd.el")

(defun dnd-direct-save (file name &optional frame allow-same-frame)
  "Drag FILE from FRAME, but do not treat it as an actual file.
Instead, ask the target window to insert the file with NAME.
File managers will create a file in the displayed directory with
the contents of FILE and the name NAME, while text editors will
insert the contents of FILE in a new document named
NAME.

ALLOW-SAME-FRAME means the same as in `dnd-begin-file-drag'.
Return `copy' if the drop was successful, else nil."
  (setq file (expand-file-name file))
  (cond ((eq window-system 'x)
         (when (x-dnd-do-direct-save file name frame
                                     allow-same-frame)
           'copy))
        ;; Avoid infinite recursion.
        (t (let ((dnd-direct-save-remote-files nil))
             (dnd-begin-file-drag file frame nil allow-same-frame)))))

(provide 'dnd)

;;; dnd.el ends here
