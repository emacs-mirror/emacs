;;; mh-utils.el --- mh-e code needed for both sending and reading
;; Time-stamp: <95/02/10 14:20:14 gildea>

;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.

;; This file is part of mh-e.

;; mh-e is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mh-e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mh-e; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Internal support for mh-e package.

;;; Change Log:

;; $Id: mh-utils.el,v 1.8 95/03/02 04:54:00 gildea Exp $

;;; Code:

;;; Set for local environment:
;;; mh-progs and mh-lib used to be set in paths.el, which tried to
;;; figure out at build time which of several possible directories MH
;;; was installed into.  But if you installed MH after building Emacs,
;;; this would almost certainly be wrong, so now we do it at run time.

(defvar mh-progs nil
  "Directory containing MH commands, such as inc, repl, and rmm.")

(defvar mh-lib nil
  "Directory containing the MH library.
This directory contains, among other things,
the mhl program and the components file.")

;;;###autoload
(put 'mh-progs 'risky-local-variable t)
;;;###autoload
(put 'mh-lib 'risky-local-variable t)

;;; User preferences:

(defvar mh-auto-folder-collect t
  "*Whether to start collecting MH folder names immediately in the background.
Non-nil means start a background process collecting the names of all
folders as soon as mh-e is loaded.")

(defvar mh-recursive-folders nil
  "*If non-nil, then commands which operate on folders do so recursively.")

(defvar mh-clean-message-header nil
  "*Non-nil means clean headers of messages that are displayed or inserted.
The variables `mh-visible-headers' and `mh-invisible-headers' control what
is removed.")

(defvar mh-visible-headers nil
  "*If non-nil, contains a regexp specifying the headers to keep when cleaning.
Only used if `mh-clean-message-header' is non-nil.  Setting this variable
overrides `mh-invisible-headers'.")

(defvar mh-invisible-headers
  "^Received: \\|^Message-Id: \\|^Remailed-\\|^Via: \\|^Mail-from: \\|^Return-Path: \\|^Delivery-Date: \\|^In-Reply-To: \\|^Resent-"
  "Regexp matching lines in a message header that are not to be shown.
If `mh-visible-headers' is non-nil, it is used instead to specify what
to keep.")

(defvar mh-bury-show-buffer t
  "*Non-nil means that the displayed show buffer for a folder is buried.")

(defvar mh-summary-height 4
  "*Number of lines in MH-Folder window (including the mode line).")

(defvar mh-msg-number-regexp "^ *\\([0-9]+\\)"
  "Regexp to find the number of a message in a scan line.
The message's number must be surrounded with \\( \\)")

(defvar mh-msg-search-regexp "^[^0-9]*%d[^0-9]"
  "Format string containing a regexp matching the scan listing for a message.
The desired message's number will be an argument to format.")

(defvar mhl-formfile nil
  "*Name of format file to be used by mhl to show and print messages.
A value of T means use the default format file.
Nil means don't use mhl to format messages when showing; mhl is still used,
with the default format file, to format messages when printing them.
The format used should specify a non-zero value for overflowoffset so
the message continues to conform to RFC 822 and mh-e can parse the headers.")

(defvar mh-default-folder-for-message-function nil
  "Function to select a default folder for refiling or Fcc.
If set to a function, that function is called with no arguments by
`\\[mh-refile-msg]' and `\\[mh-to-fcc]' to get a default when
prompting the user for a folder.  The function is called from within a
save-excursion, with point at the start of the message.  It should
return the folder to offer as the refile or Fcc folder, as a string
with a leading `+' sign.  It can also return an empty string to use no
default, or NIL to calculate the default the usual way.
NOTE: This variable is not an ordinary hook;
It may not be a list of functions.")

(defvar mh-find-path-hook nil
  "Invoked by mh-find-path while reading the user's MH profile.")

(defvar mh-folder-list-change-hook nil
  "Invoked whenever the cached folder list `mh-folder-list' is changed.")

(defvar mh-show-buffer-mode-line-buffer-id "{show-%s} %d"
  "Format string to produce `mode-line-buffer-identification' for show buffers.
First argument is folder name.  Second is message number.")

(defvar mh-cmd-note 4
  "Offset to insert notation.")

(defvar mh-note-seq "%"
  "String whose first character is used to notate messages in a sequence.")

;;; Internal bookkeeping variables:

;; The value of `mh-folder-list-change-hook' is called whenever
;; mh-folder-list variable is set.
(defvar mh-folder-list nil)		;List of folder names for completion.

;; Cached value of the `Path:' component in the user's MH profile.
(defvar mh-user-path nil)		;User's mail folder directory.

;; An mh-draft-folder of NIL means do not use a draft folder.
;; Cached value of the `Draft-Folder:' component in the user's MH profile.
(defvar mh-draft-folder nil)		;Name of folder containing draft messages.

;; Cached value of the `Unseen-Sequence:' component in the user's MH profile.
(defvar mh-unseen-seq nil)		;Name of the Unseen sequence.

;; Cached value of the `Previous-Sequence:' component in the user's MH profile.
(defvar mh-previous-seq nil)		;Name of the Previous sequence.

;; Cached value of the `Inbox:' component in the user's MH profile,
;; or "+inbox" if no such component.
(defvar mh-inbox nil)			;Name of the Inbox folder.

(defconst mh-temp-buffer " *mh-temp*")	;Name of mh-e scratch buffer.

(defvar mh-previous-window-config nil)	;Window configuration before mh-e command.

;;; Internal variables local to a folder.

(defvar mh-current-folder nil)		;Name of current folder, a string.

(defvar mh-show-buffer nil)		;Buffer that displays message for this folder.

(defvar mh-folder-filename nil)		;Full path of directory for this folder.

(defvar mh-showing nil)			;If non-nil, show the message in a separate window.

;;; This holds a documentation string used by describe-mode.
(defun mh-showing ()
  "When moving to a new message in the Folder window,
also show it in a separate Show window."
  nil)

(defvar mh-seq-list nil)		;The sequences of this folder.  An alist of (seq . msgs).

(defvar mh-seen-list nil)		;List of displayed messages to be removed from the Unseen sequence.

;; If non-nil, show buffer contains message with all headers.
;; If nil, show buffer contains message processed normally.
(defvar mh-showing-with-headers nil)	;Showing message with headers or normally.


;;; mh-e macros

(defmacro with-mh-folder-updating (save-modification-flag-p &rest body)
  ;; Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG-P) &body BODY).
  ;; Execute BODY, which can modify the folder buffer without having to
  ;; worry about file locking or the read-only flag, and return its result.
  ;; If SAVE-MODIFICATION-FLAG-P is non-nil, the buffer's modification
  ;; flag is unchanged, otherwise it is cleared.
  (setq save-modification-flag-p (car save-modification-flag-p)) ; CL style
  (` (prog1
	 (let ((mh-folder-updating-mod-flag (buffer-modified-p))
	       (buffer-read-only nil)
	       (buffer-file-name nil))	;don't let the buffer get locked
	   (prog1
	       (progn
		 (,@ body))
	     (mh-set-folder-modified-p mh-folder-updating-mod-flag)))
       (,@ (if (not save-modification-flag-p)
	       '((mh-set-folder-modified-p nil)))))))

(put 'with-mh-folder-updating 'lisp-indent-hook 1)

(defmacro mh-in-show-buffer (show-buffer &rest body)
  ;; Format is (mh-in-show-buffer (SHOW-BUFFER) &body BODY).
  ;; Display buffer SHOW-BUFFER in other window and execute BODY in it.
  ;; Stronger than save-excursion, weaker than save-window-excursion.
  (setq show-buffer (car show-buffer))	; CL style
  (` (let ((mh-in-show-buffer-saved-window (selected-window)))
       (switch-to-buffer-other-window (, show-buffer))
       (if mh-bury-show-buffer (bury-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (,@ body))
	 (select-window mh-in-show-buffer-saved-window)))))

(put 'mh-in-show-buffer 'lisp-indent-hook 1)

(defmacro mh-make-seq (name msgs) (list 'cons name msgs))

(defmacro mh-seq-name (pair) (list 'car pair))

(defmacro mh-seq-msgs (pair) (list 'cdr pair))


;;; Ensure new buffers won't get this mode if default-major-mode is nil.
(put 'mh-show-mode 'mode-class 'special)

(defun mh-show-mode ()
  "Major mode for showing messages in mh-e.
The value of mh-show-mode-hook is called when a new message is displayed."
  (kill-all-local-variables)
  (setq major-mode 'mh-show-mode)
  (mh-set-mode-name "MH-Show")
  (run-hooks 'mh-show-mode-hook))


(defun mh-maybe-show (&optional msg)
  ;; If in showing mode, then display the message pointed to by the cursor.
  (if mh-showing (mh-show msg)))

(defun mh-show (&optional message)
  "Show MESSAGE (default: message at cursor).
Force a two-window display with the folder window on top (size
mh-summary-height) and the show buffer below it.
If the message is already visible, display the start of the message.

Display of the message is controlled by setting the variables
`mh-clean-message-header' and `mhl-formfile'.  The default behavior is
to scroll uninteresting headers off the top of the window.
Type \"\\[mh-header-display]\" to see the message with all its headers."
  (interactive)
  (and mh-showing-with-headers
       (or mhl-formfile mh-clean-message-header)
       (mh-invalidate-show-buffer))
  (mh-show-msg message))


(defun mh-show-msg (msg)
  (if (not msg)
      (setq msg (mh-get-msg-num t)))
  (setq mh-showing t)
  (let ((folder mh-current-folder)
	(clean-message-header mh-clean-message-header)
	(show-window (get-buffer-window mh-show-buffer)))
    (if (not (eql (next-window (minibuffer-window)) (selected-window)))
	(delete-other-windows))		; force ourself to the top window
    (mh-in-show-buffer (mh-show-buffer)
      (if (and show-window
	       (equal (mh-msg-filename msg folder) buffer-file-name))
	  (progn			;just back up to start
	    (goto-char (point-min))
	    (if (not clean-message-header)
		(mh-start-of-uncleaned-message)))
	(mh-display-msg msg folder))))
  (if (not (= (1+ (window-height)) (screen-height))) ;not horizontally split
      (shrink-window (- (window-height) mh-summary-height)))
  (mh-recenter nil)
  (if (not (memq msg mh-seen-list)) (setq mh-seen-list (cons msg mh-seen-list)))
  (run-hooks 'mh-show-hook))


(defun mh-display-msg (msg-num folder)
  ;; Display message NUMBER of FOLDER.
  ;; Sets the current buffer to the show buffer.
  (set-buffer folder)
  ;; Bind variables in folder buffer in case they are local
  (let ((formfile mhl-formfile)
	(clean-message-header mh-clean-message-header)
	(invisible-headers mh-invisible-headers)
	(visible-headers mh-visible-headers)
	(msg-filename (mh-msg-filename msg-num))
	(show-buffer mh-show-buffer))
    (if (not (file-exists-p msg-filename))
	(error "Message %d does not exist" msg-num))
    (set-buffer show-buffer)
    (cond ((not (equal msg-filename buffer-file-name))
	   (mh-unvisit-file)
	   (erase-buffer)
	   ;; Changing contents, so this hook needs to be reinitialized.
	   ;; pgp.el uses this.
	   (if (boundp 'write-contents-hooks) ;Emacs 19
	       (setq write-contents-hooks nil))
	   (if formfile
	       (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
				       (if (stringp formfile)
					   (list "-form" formfile))
				       msg-filename)
	     (insert-file-contents msg-filename))
	   (goto-char (point-min))
	   (cond (clean-message-header
		  (mh-clean-msg-header (point-min)
				       invisible-headers
				       visible-headers)
		  (goto-char (point-min)))
		 (t
		  (mh-start-of-uncleaned-message)))
	   ;; the parts of visiting we want to do (no locking)
	   (or (eq buffer-undo-list t)	;don't save undo info for prev msgs
	       (setq buffer-undo-list nil))
	   (set-buffer-modified-p nil)
	   (set-buffer-auto-saved)
	   ;; the parts of set-visited-file-name we want to do (no locking)
	   (setq buffer-file-name msg-filename)
	   (setq buffer-backed-up nil)
	   (auto-save-mode 1)
	   (set-mark nil)
	   (mh-show-mode)
	   (setq mode-line-buffer-identification
		 (list (format mh-show-buffer-mode-line-buffer-id
			       folder msg-num)))
	   (set-buffer folder)
	   (setq mh-showing-with-headers nil)))))

(defun mh-start-of-uncleaned-message ()
  ;; position uninteresting headers off the top of the window
  (let ((case-fold-search t))
    (re-search-forward
     "^To:\\|^Cc:\\|^From:\\|^Subject:\\|^Date:" nil t)
    (beginning-of-line)
    (mh-recenter 0)))


(defun mh-invalidate-show-buffer ()
  ;; Invalidate the show buffer so we must update it to use it.
  (if (get-buffer mh-show-buffer)
      (save-excursion
	(set-buffer mh-show-buffer)
	(mh-unvisit-file))))


(defun mh-unvisit-file ()
  ;; Separate current buffer from the message file it was visiting.
  (or (not (buffer-modified-p))
      (null buffer-file-name)		;we've been here before
      (yes-or-no-p (format "Message %s modified; flush changes? "
			   (file-name-nondirectory buffer-file-name)))
      (error "Flushing changes not confirmed"))
  (clear-visited-file-modtime)
  (unlock-buffer)
  (setq buffer-file-name nil))

  
(defun mh-get-msg-num (error-if-no-message)
  ;; Return the message number of the displayed message.  If the argument
  ;; ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is not
  ;; pointing to a message.
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at mh-msg-number-regexp)
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-filename (msg &optional folder)
  ;; Return the file name of MESSAGE in FOLDER (default current folder).
  (expand-file-name (int-to-string msg)
		    (if folder
			(mh-expand-file-name folder)
			mh-folder-filename)))


(defun mh-clean-msg-header (start invisible-headers visible-headers)
  ;; Flush extraneous lines in a message header, from the given POINT to the
  ;; end of the message header.  If VISIBLE-HEADERS is non-nil, it contains a
  ;; regular expression specifying the lines to display, otherwise
  ;; INVISIBLE-HEADERS contains a regular expression specifying lines to
  ;; delete from the header.
  (let ((case-fold-search t))
    (save-restriction
      (goto-char start)
      (if (search-forward "\n\n" nil 'move)
	  (backward-char 1))
      (narrow-to-region start (point))
      (goto-char (point-min))
      (if visible-headers
	  (while (< (point) (point-max))
	    (cond ((looking-at visible-headers)
		   (forward-line 1)
		   (while (looking-at "[ \t]") (forward-line 1)))
		  (t
		    (mh-delete-line 1)
		    (while (looking-at "[ \t]")
		      (mh-delete-line 1)))))
	  (while (re-search-forward invisible-headers nil t)
	    (beginning-of-line)
	    (mh-delete-line 1)
	    (while (looking-at "[ \t]")
	      (mh-delete-line 1))))
      (unlock-buffer))))


(defun mh-recenter (arg)
  ;; Like recenter but with two improvements: nil arg means recenter,
  ;; and only does anything if the current buffer is in the selected
  ;; window.  (Commands like save-some-buffers can make this false.)
  (if (eql (get-buffer-window (current-buffer))
	   (selected-window))
      (recenter (if arg arg '(t)))))


(defun mh-delete-line (lines)
  ;; Delete version of kill-line.
  (delete-region (point) (save-excursion (forward-line lines) (point))))


(defun mh-notate (msg notation offset)
  ;; Marks MESSAGE with the character NOTATION at position OFFSET.
  ;; Null MESSAGE means the message that the cursor points to.
  (save-excursion
    (if (or (null msg)
	    (mh-goto-msg msg t t))
	(with-mh-folder-updating (t)
	  (beginning-of-line)
	  (forward-char offset)
	  (delete-char 1)
	  (insert notation)))))


(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Position the cursor at message NUMBER.
Optional non-nil second argument means return nil instead of
signaling an error if message does not exist.
Non-nil third argument means not to show the message."
  (interactive "NGo to message: ")
  (setq number (prefix-numeric-value number)) ;Emacs 19
  (let ((cur-msg (mh-get-msg-num nil))
	(starting-place (point))
	(msg-pattern (mh-msg-search-pat number)))
    (cond ((cond ((and cur-msg (= cur-msg number)) t)
		 ((and cur-msg
		       (< cur-msg number)
		       (re-search-forward msg-pattern nil t)) t)
		 ((and cur-msg
		       (> cur-msg number)
		       (re-search-backward msg-pattern nil t)) t)
		 (t			; Do thorough search of buffer
		  (goto-char (point-max))
		  (re-search-backward msg-pattern nil t)))
	    (beginning-of-line)
	    (if (not dont-show) (mh-maybe-show number))
	    t)
	  (t
	   (goto-char starting-place)
	   (if (not no-error-if-no-message)
	       (error "No message %d" number))
	   nil))))

(defun mh-msg-search-pat (n)
  ;; Return a search pattern for message N in the scan listing.
  (format mh-msg-search-regexp n))


(defun mh-get-profile-field (field)
  ;; Find and return the value of FIELD in the current buffer.
  ;; Returns NIL if the field is not in the buffer.
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^%s" field) nil t)) nil)
	  ((looking-at "[\t ]*$") nil)
	  (t
	   (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
	   (let ((start (match-beginning 1)))
	     (end-of-line)
	     (buffer-substring start (point)))))))


(defun mh-find-path ()
  ;; Set mh-progs and mh-lib.
  ;; (This step is necessary if MH was installed after this Emacs was dumped.)
  ;; From profile file, set mh-user-path, mh-draft-folder,
  ;; mh-unseen-seq, mh-previous-seq, mh-inbox.
  (mh-find-progs)
  (save-excursion
    ;; Be sure profile is fully expanded before switching buffers
    (let ((profile (expand-file-name (or (getenv "MH") "~/.mh_profile"))))
      (set-buffer (get-buffer-create mh-temp-buffer))
      (setq buffer-offer-save nil)	;for people who set default to t
      (erase-buffer)
      (condition-case err
	  (insert-file-contents profile)
	(file-error
	 (mh-install profile err)))
      (setq mh-user-path (mh-get-profile-field "Path:"))
      (if (not mh-user-path)
	  (setq mh-user-path "Mail"))
      (setq mh-user-path
	    (file-name-as-directory
	     (expand-file-name mh-user-path (expand-file-name "~"))))
      (setq mh-draft-folder (mh-get-profile-field "Draft-Folder:"))
      (if mh-draft-folder
	  (progn
	    (if (not (mh-folder-name-p mh-draft-folder))
		(setq mh-draft-folder (format "+%s" mh-draft-folder)))
	    (if (not (file-exists-p (mh-expand-file-name mh-draft-folder)))
		(error "Draft folder \"%s\" not found.  Create it and try again."
		       (mh-expand-file-name mh-draft-folder)))))
      (setq mh-inbox (mh-get-profile-field "Inbox:"))
      (cond ((not mh-inbox)
	     (setq mh-inbox "+inbox"))
	    ((not (mh-folder-name-p mh-inbox))
	     (setq mh-inbox (format "+%s" mh-inbox))))
      (setq mh-unseen-seq (mh-get-profile-field "Unseen-Sequence:"))
      (if mh-unseen-seq
	  (setq mh-unseen-seq (intern mh-unseen-seq))
	(setq mh-unseen-seq 'unseen))	;old MH default?
      (setq mh-previous-seq (mh-get-profile-field "Previous-Sequence:"))
      (if mh-previous-seq
	  (setq mh-previous-seq (intern mh-previous-seq)))
      (run-hooks 'mh-find-path-hook))))

(defun mh-find-progs ()
  (or (file-exists-p (expand-file-name "inc" mh-progs))
      (setq mh-progs
	    (or (mh-path-search exec-path "inc")
		(mh-path-search '("/usr/local/bin/mh/"
				  "/usr/local/mh/"
				  "/usr/bin/mh/" ;Ultrix 4.2
				  "/usr/new/mh/" ;Ultrix <4.2
				  "/usr/contrib/mh/bin/" ;BSDI
				  "/usr/local/bin/"
				  )
				"inc")
		mh-progs
		"/usr/local/bin/")))
  (or (file-exists-p (expand-file-name "mhl" mh-lib))
      (setq mh-lib
	    (or (mh-path-search '("/usr/local/lib/mh/"
				  "/usr/local/mh/lib/"
				  "/usr/local/bin/mh/"
				  "/usr/lib/mh/" ;Ultrix 4.2
				  "/usr/new/lib/mh/" ;Ultrix <4.2
				  "/usr/contrib/mh/lib/" ;BSDI
				  )
				"mhl")
		(mh-path-search exec-path "mhl") ;unlikely
		mh-lib
		"/usr/local/lib/mh/"))))

(defun mh-path-search (path file)
  ;; Search PATH, a list of directory names, for FILE.
  ;; Returns the element of PATH that contains FILE, or nil if not found.
  (while (and path
	      (not (file-exists-p (expand-file-name file (car path)))))
    (setq path (cdr path)))
  (car path))

(defun mh-install (profile error-val)
  ;; Called to do error recovery if we fail to read the profile file.
  ;; If possible, initialize the MH environment.
  (if (or (getenv "MH")
	  (file-exists-p profile))
      (error "Cannot read MH profile \"%s\": %s"
	     profile (car (cdr (cdr error-val)))))
  ;; The "install-mh" command will output a short note which
  ;; mh-exec-cmd will display to the user.
  ;; The MH 5 version of install-mh might try prompt the user
  ;; for information, which would fail here.
  (mh-exec-cmd (expand-file-name "install-mh" mh-lib) "-auto")
  ;; now try again to read the profile file
  (erase-buffer)
  (condition-case err
      (insert-file-contents profile)
    (file-error
     (error "Cannot read MH profile \"%s\": %s"
	    profile (car (cdr (cdr err)))))))


(defun mh-set-folder-modified-p (flag)
  ;; Mark current folder as modified or unmodified according to FLAG.
  (set-buffer-modified-p flag))


(defun mh-find-seq (name) (assoc name mh-seq-list))

(defun mh-seq-to-msgs (seq)
  ;; Return a list of the messages in SEQUENCE.
  (mh-seq-msgs (mh-find-seq seq)))


(defun mh-add-msgs-to-seq (msgs seq &optional internal-flag)
  ;; Add MESSAGE(s) to the SEQUENCE.  If optional FLAG is non-nil, do not mark
  ;; the message in the scan listing or inform MH of the addition.
  (let ((entry (mh-find-seq seq)))
    (if (and msgs (atom msgs)) (setq msgs (list msgs)))
    (if (null entry)
	(setq mh-seq-list (cons (mh-make-seq seq msgs) mh-seq-list))
	(if msgs (setcdr entry (append msgs (mh-seq-msgs entry)))))
    (cond ((not internal-flag)
	   (mh-add-to-sequence seq msgs)
	   (mh-notate-seq seq mh-note-seq (1+ mh-cmd-note))))))

(autoload 'mh-add-to-sequence "mh-seq")
(autoload 'mh-notate-seq "mh-seq")
(autoload 'mh-read-seq-default "mh-seq")
(autoload 'mh-map-to-seq-msgs "mh-seq")


(defun mh-set-mode-name (mode-name-string)
  ;; Set the mode-name and ensure that the mode line is updated.
  (setq mode-name mode-name-string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p)))


(defun mh-prompt-for-folder (prompt default can-create)
  ;; Prompt for a folder name with PROMPT.  Returns the folder's name as a
  ;; string.  DEFAULT is used if the folder exists and the user types return.
  ;; If the CAN-CREATE flag is t, then a non-existent folder is made.
  (if (null default)
      (setq default ""))
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 read-name folder-name)
    (if (null mh-folder-list)
	(mh-set-folder-list))
    (while (and (setq read-name (completing-read prompt mh-folder-list
					    nil nil "+"))
		(equal read-name "")
		(equal default "")))
    (cond ((or (equal read-name "") (equal read-name "+"))
	   (setq read-name default))
	  ((not (mh-folder-name-p read-name))
	   (setq read-name (format "+%s" read-name))))
    (setq folder-name read-name)
    (cond ((and (> (length folder-name) 0)
		(eql (aref folder-name (1- (length folder-name))) ?/))
	   (setq folder-name (substring folder-name 0 -1))))
    (let ((new-file-p (not (file-exists-p (mh-expand-file-name folder-name)))))
      (cond ((and new-file-p
		  (y-or-n-p
		   (format "Folder %s does not exist.  Create it? " folder-name)))
	     (message "Creating %s" folder-name)
	     (call-process "mkdir" nil nil nil (mh-expand-file-name folder-name))
	     (message "Creating %s...done" folder-name)
	     (setq mh-folder-list (cons (list read-name) mh-folder-list))
	     (run-hooks 'mh-folder-list-change-hook))
	    (new-file-p
	     (error "Folder %s is not created" folder-name))
	    ((and (null (assoc read-name mh-folder-list))
		  (null (assoc (concat read-name "/") mh-folder-list)))
	     (setq mh-folder-list (cons (list read-name) mh-folder-list))
	     (run-hooks 'mh-folder-list-change-hook))))
    folder-name))


(defvar mh-make-folder-list-process nil) ;The background process collecting the folder list.

(defvar mh-folder-list-temp nil)	;mh-folder-list as it is being built.

(defvar mh-folder-list-partial-line "")	;Start of last incomplete line from folder process.

(defun mh-set-folder-list ()
  ;; Sets mh-folder-list correctly.
  ;; A useful function for the command line or for when you need to
  ;; sync by hand.  Format is in a form suitable for completing read.
  (message "Collecting folder names...")
  (if (not mh-make-folder-list-process)
      (mh-make-folder-list-background))
  (while (eq (process-status mh-make-folder-list-process) 'run)
    (accept-process-output mh-make-folder-list-process))
  (setq mh-folder-list mh-folder-list-temp)
  (run-hooks 'mh-folder-list-change-hook)
  (setq mh-folder-list-temp nil)
  (delete-process mh-make-folder-list-process)
  (setq mh-make-folder-list-process nil)
  (message "Collecting folder names...done"))

(defun mh-make-folder-list-background ()
  ;; Start a background process to compute a list of the user's folders.
  ;; Call mh-set-folder-list to wait for the result.
  (cond
   ((not mh-make-folder-list-process)
    (mh-find-progs)
    (let ((process-connection-type nil))
      (setq mh-make-folder-list-process
	    (start-process "folders" nil (expand-file-name "folders" mh-progs)
			   "-fast"
			   (if mh-recursive-folders
			       "-recurse"
			     "-norecurse")))
      (set-process-filter mh-make-folder-list-process
			  'mh-make-folder-list-filter)
      (process-kill-without-query mh-make-folder-list-process)))))

(defun mh-make-folder-list-filter (process output)
  ;; parse output from "folders -fast"
  (let ((position 0)
	(line-end t)
	new-folder)
    (while line-end
      (setq line-end (string-match "\n" output position))
      (cond
       (line-end			;make sure got complete line
	(setq new-folder (format "+%s%s"
				 mh-folder-list-partial-line
				 (substring output position line-end)))
	(setq mh-folder-list-partial-line "")
	;; is new folder a subfolder of previous?
	(if (and mh-folder-list-temp
		 (string-match (regexp-quote
				(concat (car (car mh-folder-list-temp)) "/"))
			       new-folder))
	    ;; append slash to parent folder for better completion
	    ;; (undone by mh-prompt-for-folder)
	    (setq mh-folder-list-temp
		  (cons (list new-folder)
			(cons
			 (list (concat (car (car mh-folder-list-temp)) "/"))
			 (cdr mh-folder-list-temp))))
	  (setq mh-folder-list-temp
		(cons (list new-folder)
		      mh-folder-list-temp)))
	(setq position (1+ line-end)))))
    (setq mh-folder-list-partial-line (substring output position))))


(defun mh-folder-name-p (name)
  ;; Return non-NIL if NAME is possibly the name of a folder.
  ;; A name (a string or symbol) can be a folder name if it begins with "+".
  (if (symbolp name)
      (eql (aref (symbol-name name) 0) ?+)
    (and (> (length name) 0)
	 (eql (aref name 0) ?+))))


;;; Issue commands to MH.


(defun mh-exec-cmd (command &rest args)
  ;; Execute mh-command COMMAND with ARGS.
  ;; The side effects are what is desired.
  ;; Any output is assumed to be an error and is shown to the user.
  ;; The output is not read or parsed by mh-e.
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer)
    (apply 'call-process
	   (expand-file-name command mh-progs) nil t nil
	   (mh-list-to-string args))
    (if (> (buffer-size) 0)
	(save-window-excursion
	  (switch-to-buffer-other-window mh-temp-buffer)
	  (sit-for 5)))))


(defun mh-exec-cmd-error (env command &rest args)
  ;; In environment ENV, execute mh-command COMMAND with args ARGS.
  ;; ENV is nil or a string of space-separated "var=value" elements.
  ;; Signals an error if process does not complete successfully.
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer)
    (let ((status
	   (if env
	       ;; the shell hacks necessary here shows just how broken Unix is
	       (apply 'call-process "/bin/sh" nil t nil "-c"
		      (format "%s %s ${1+\"$@\"}"
			      env 
			      (expand-file-name command mh-progs))
		      command
		      (mh-list-to-string args))
	       (apply 'call-process
		      (expand-file-name command mh-progs) nil t nil
		      (mh-list-to-string args)))))
      (mh-handle-process-error command status))))


(defun mh-exec-cmd-daemon (command &rest args)
  ;; Execute MH command COMMAND with ARGS in the background.
  ;; Any output from command is displayed in an asynchronous pop-up window.
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer))
  (let* ((process-connection-type nil)
	 (process (apply 'start-process
			 command nil
			 (expand-file-name command mh-progs)
			 (mh-list-to-string args))))
    (set-process-filter process 'mh-process-daemon)))

(defun mh-process-daemon (process output)
  ;; Process daemon that puts output into a temporary buffer.
  (set-buffer (get-buffer-create mh-temp-buffer))
  (insert-before-markers output)
  (display-buffer mh-temp-buffer))


(defun mh-exec-cmd-quiet (raise-error command &rest args)
  ;; Args are RAISE-ERROR, COMMANDS, ARGS....
  ;; Execute MH command COMMAND with ARGS.  ARGS is a list of strings.
  ;; Return at start of mh-temp buffer, where output can be parsed and used.
  ;; Returns value of call-process, which is 0 for success,
  ;; unless RAISE-ERROR is non-nil, in which case an error is signaled
  ;; if call-process returns non-0.
  (set-buffer (get-buffer-create mh-temp-buffer))
  (erase-buffer)
  (let ((value
	 (apply 'call-process
		(expand-file-name command mh-progs) nil t nil
		args)))
    (goto-char (point-min))
    (if raise-error
	(mh-handle-process-error command value)
      value)))


(defun mh-exec-cmd-output (command display &rest args)
  ;; Execute MH command COMMAND with DISPLAY flag and ARGS.
  ;; Put the output into buffer after point.  Set mark after inserted text.
  ;; Output is expected to be shown to user, not parsed by mh-e.
  (push-mark (point) t)
  (apply 'call-process
	 (expand-file-name command mh-progs) nil t display
	 (mh-list-to-string args))
  (exchange-point-and-mark))


(defun mh-exec-lib-cmd-output (command &rest args)
  ;; Execute MH library command COMMAND with ARGS.
  ;; Put the output into buffer after point.  Set mark after inserted text.
  (apply 'mh-exec-cmd-output (expand-file-name command mh-lib) nil args))


(defun mh-handle-process-error (command status)
  ;; Raise error if COMMAND returned non-0 STATUS, otherwise return STATUS.
  ;; STATUS is return value from call-process.
  ;; Program output is in current buffer.
  ;; If output is too long to include in error message, display the buffer.
  (cond ((eql status 0)			;success
	 status)
	((stringp status)		;kill string
	 (error (format "%s: %s" command status)))
	(t				;exit code
	 (cond
	  ((= (buffer-size) 0)		;program produced no error message
	   (error (format "%s: exit code %d" command status)))
	  (t
	   ;; will error message fit on one line?
	   (goto-line 2)
	   (if (and (< (buffer-size) (screen-width))
		    (eobp))
	       (error (buffer-substring 1 (progn (goto-char 1)
						 (end-of-line)
						 (point))))
	     (display-buffer (current-buffer))
	     (error (format
		     "%s failed with status %d.  See error message in other window."
		     command status))))))))


(defun mh-expand-file-name (filename &optional default)
  ;; Just like `expand-file-name', but also handles MH folder names.
  ;; Assumes that any filename that starts with '+' is a folder name.
   (if (mh-folder-name-p filename)
       (expand-file-name (substring filename 1) mh-user-path)
     (expand-file-name filename default)))


(defun mh-list-to-string (l)
  ;; Flattens the list L and makes every element of the new list into a string.
  (nreverse (mh-list-to-string-1 l)))

(defun mh-list-to-string-1 (l)
  (let ((new-list nil))
    (while l
      (cond ((null (car l)))
	    ((symbolp (car l))
	     (setq new-list (cons (symbol-name (car l)) new-list)))
	    ((numberp (car l))
	     (setq new-list (cons (int-to-string (car l)) new-list)))
	    ((equal (car l) ""))
	    ((stringp (car l)) (setq new-list (cons (car l) new-list)))
	    ((listp (car l))
	     (setq new-list (nconc (mh-list-to-string-1 (car l))
				   new-list)))
	    (t (error "Bad element in mh-list-to-string: %s" (car l))))
      (setq l (cdr l)))
    new-list))

(provide 'mh-utils)

(and (not noninteractive)
     mh-auto-folder-collect
     (mh-make-folder-list-background))

;;; mh-utils.el ends here
