;;; view.el --- peruse file or buffer without editing.

;; Copyright (C) 1985, 1989, 1994, 1995 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF

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

;; This package provides the `view' minor mode documented in the Emacs
;; user's manual.

;;; Code:

;;;###autoload
(defvar view-highlight-face 'highlight
   "*The overlay face used for highlighting the match found by View mode search.")

(defvar view-mode nil "Non-nil if View mode is enabled.")
(make-variable-buffer-local 'view-mode)

(defvar view-mode-auto-exit nil
  "Non-nil means scrolling past the end of buffer exits View mode.
Some commands, such as \\[view-file], set this to t locally;
the only way to override that is to set it to nil using `view-mode-hook'.")

(make-variable-buffer-local 'view-mode-auto-exit)

(defvar view-old-buffer-read-only nil)
(make-variable-buffer-local 'view-old-buffer-read-only)
(defvar view-old-Helper-return-blurb)
(make-variable-buffer-local 'view-old-Helper-return-blurb)

(defvar view-scroll-size nil)
(make-variable-buffer-local 'view-scroll-size)

(defvar view-last-regexp nil)
(make-variable-buffer-local 'view-last-regexp)

(defvar view-exit-action nil)
(make-variable-buffer-local 'view-exit-action)
(defvar view-return-here nil)
(make-variable-buffer-local 'view-return-here)
(defvar view-exit-position nil)
(make-variable-buffer-local 'view-exit-position)

(defvar view-overlay nil
  "Overlay used to display where a search operation found its match.
This is local in each buffer, once it is used.")
(make-variable-buffer-local 'view-overlay)

(or (assq 'view-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(view-mode " View") minor-mode-alist)))

(defvar view-mode-map nil)
(if view-mode-map
    nil
  (setq view-mode-map (make-keymap))
  ;; We used to call suppress-keymap here, but that isn't good in a minor mode.
  ;; Self-inserting characters will beep anyway, since the buffer is read-only,
  ;; and we should not interfere with letters that serve as useful commands.
  (define-key view-mode-map "q" 'view-exit)
  (define-key view-mode-map "<" 'beginning-of-buffer)
  (define-key view-mode-map ">" 'end-of-buffer)
  (define-key view-mode-map "\ev" 'View-scroll-lines-backward)
  (define-key view-mode-map "\C-v" 'View-scroll-lines-forward)
  (define-key view-mode-map " " 'View-scroll-lines-forward)
  (define-key view-mode-map "\C-?" 'View-scroll-lines-backward)
  (define-key view-mode-map "\n" 'View-scroll-one-more-line)
  (define-key view-mode-map "\r" 'View-scroll-one-more-line)
  (define-key view-mode-map "z" 'View-scroll-lines-forward-set-scroll-size)
  (define-key view-mode-map "g" 'View-goto-line)
  (define-key view-mode-map "=" 'what-line)
  (define-key view-mode-map "." 'set-mark-command)
  (define-key view-mode-map "'" 'View-back-to-mark)
  (define-key view-mode-map "@" 'View-back-to-mark)  
  (define-key view-mode-map "x" 'exchange-point-and-mark)
  (define-key view-mode-map "h" 'describe-mode)
  (define-key view-mode-map "?" 'describe-mode)
  (define-key view-mode-map "s" 'isearch-forward)
  (define-key view-mode-map "r" 'isearch-backward)
  (define-key view-mode-map "/" 'View-search-regexp-forward)
  (define-key view-mode-map "\\" 'View-search-regexp-backward)
  ;; This conflicts with the standard binding of isearch-regexp-forward
  (define-key view-mode-map "\e\C-s" 'View-search-regexp-forward)
  (define-key view-mode-map "\e\C-r" 'View-search-regexp-backward)  
  (define-key view-mode-map "n" 'View-search-last-regexp-forward)
  (define-key view-mode-map "p" 'View-search-last-regexp-backward)
  )

(or (assq 'view-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'view-mode view-mode-map) minor-mode-map-alist)))


;;;###autoload
(defun view-file (file-name)
  "View FILE in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

This command runs the normal hook `view-mode-hook'."
  (interactive "fView file: ")
  (let ((old-buf (current-buffer))
	(had-a-buf (get-file-buffer file-name))
	(buf-to-view (find-file-noselect file-name)))
    ;; This used to pass t as second argument,
    ;; but then the buffer did not show up in the Buffers menu.
    (switch-to-buffer buf-to-view had-a-buf)
    (view-mode-enter old-buf
		     (and (not had-a-buf) (not (buffer-modified-p buf-to-view))
			  'kill-buffer))))

;;;###autoload
(defun view-file-other-window (file-name)
  "View FILE in View mode in other window.
Return to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

This command runs the normal hook `view-mode-hook'."
  (interactive "fView file: ")
  (let ((old-arrangement (current-window-configuration))
	(had-a-buf (get-file-buffer file-name))
	(buf-to-view (find-file-noselect file-name)))
    (switch-to-buffer-other-window buf-to-view)
    (view-mode-enter old-arrangement
		     (and (not had-a-buf) (not (buffer-modified-p buf-to-view))
			  'kill-buffer))))

;;;###autoload
(defun view-buffer (buffer-name)
  "View BUFFER in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

This command runs the normal hook `view-mode-hook'."
  (interactive "bView buffer: ")
  (let ((old-buf (current-buffer)))
    (switch-to-buffer buffer-name t)
    (view-mode-enter old-buf nil)))

;;;###autoload
(defun view-buffer-other-window (buffer-name not-return)
  "View BUFFER in View mode in another window.
Return to previous buffer when done, unless NOT-RETURN is non-nil.

The usual Emacs commands are not available in View mode; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

This command runs the normal hook `view-mode-hook'."
  (interactive "bView buffer:\nP")
  (let ((return-to (and not-return (current-window-configuration))))
    (switch-to-buffer-other-window buffer-name)
    (view-mode-enter return-to)))

;;;###autoload
(defun view-mode (&optional arg)
  "Toggle View mode.
With a prefix argument, turn View mode on if the argument is >= zero
and off if it is not.

If you use this function to turn on View mode, then subsequently
\"exiting\" View mode does nothing except turn View mode off.  The
other way to turn View mode on is by calling `view-mode-enter';
that is what Lisp programs usually use.

Letters do not insert themselves.  Instead these commands are provided.
Most commands take prefix arguments.  Commands dealing with lines
default to \"scroll size\" lines (initially size of window).
Search commands default to a repeat count of one.

M-< or <	move to beginning of buffer.
M-> or >	move to end of buffer.
C-v or Space	scroll forward lines.
M-v or DEL	scroll backward lines.
CR or LF	scroll forward one line (backward with prefix argument).
z		like Space except set number of lines for further
		   scrolling commands to scroll by.
C-u and Digits	provide prefix arguments.  `-' denotes negative argument.
=		prints the current line number.
g		goes to line given by prefix argument.
/ or M-C-s	searches forward for regular expression
\\ or M-C-r	searches backward for regular expression.
n		searches forward for last regular expression.
p		searches backward for last regular expression.
C-@ or .	set the mark.
x		exchanges point and mark.
C-s or s	do forward incremental search.
C-r or r	do reverse incremental search.
@ or '		return to mark and pops mark ring.
		  Mark ring is pushed at start of every
		  successful search and when jump to line to occurs.
		  The mark is set on jump to buffer start or end.
? or h		provide help message (list of commands).
\\[help-command]		provides help (list of commands or description of a command).
C-n		moves down lines vertically.
C-p		moves upward lines vertically.
C-l		recenters the screen.
q		exit view-mode and return to previous buffer."
  (interactive "P")
  (setq view-mode
	(if (null arg)
	    (not view-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defun view-mode-enter (&optional prev-buffer action)
  "Enter View mode, a Minor mode for viewing text but not editing it.
See the function `view-mode' for more details.

This function runs the normal hook `view-mode-hook'.

\\{view-mode-map}"
;  Not interactive because dangerous things happen
;  if you call it without passing a buffer as argument
;  and they are not easy to fix.
;  (interactive)
  (setq view-old-buffer-read-only buffer-read-only)
  (setq view-old-Helper-return-blurb
	(and (boundp 'Helper-return-blurb) Helper-return-blurb))

  ;; Enable view-exit to make use of the data we just saved
  ;; and to perform the exit action.
  (setq view-mode-auto-exit t)

  (setq buffer-read-only t)
  (setq view-mode t)
  (setq Helper-return-blurb
	(format "continue viewing %s"
		(if (buffer-file-name)
		    (file-name-nondirectory (buffer-file-name))
		    (buffer-name))))

  (setq view-exit-action action)
  (setq view-return-here prev-buffer)
  (setq view-exit-position (point-marker))

  (beginning-of-line)
  (setq goal-column nil)

  (run-hooks 'view-mode-hook)
  (message "%s"
     (substitute-command-keys
      "Type \\[help-command] for help, \\[describe-mode] for commands, \\[view-exit] to quit.")))

(defun view-exit ()
  "Exit from view-mode.
If you viewed an existing buffer, that buffer returns to its previous mode.
If you viewed a file that was not present in Emacs, its buffer is killed."
  (interactive)
  (setq view-mode nil)
  (and view-overlay (delete-overlay view-overlay))
  (force-mode-line-update)
  (cond (view-mode-auto-exit
	 (setq buffer-read-only view-old-buffer-read-only)
	 (setq view-mode-auto-exit nil)

	 (goto-char view-exit-position)
	 (set-marker view-exit-position nil)

	 ;; Now do something to the buffer that we were viewing
	 ;; (such as kill it).
	 (let ((viewed-buffer (current-buffer))
	       (action view-exit-action))
	   (cond
	    ((bufferp view-return-here)
	     (switch-to-buffer view-return-here))
	    ((window-configuration-p view-return-here)
	     (set-window-configuration view-return-here)))
	   (if action (funcall action viewed-buffer))))))

(defun view-window-size () (1- (window-height)))

(defun view-scroll-size ()
  (min (view-window-size) (or view-scroll-size (view-window-size))))

(defvar view-mode-hook nil
  "Normal hook run when starting to view a buffer or file.")

;(defun view-last-command (&optional who what)
;  (setq view-last-command-entry this-command)
;  (setq view-last-command who)
;  (setq view-last-command-argument what))

;(defun View-repeat-last-command ()
;  "Repeat last command issued in View mode."
;  (interactive)
;  (if (and view-last-command
;	   (eq view-last-command-entry last-command))
;      (funcall view-last-command view-last-command-argument))
;  (setq this-command view-last-command-entry))

(defun View-goto-line (line)
  "Move to line LINE in View mode.
Display is centered at LINE.  Sets mark at starting position and pushes
mark ring."
  (interactive "p")
  (push-mark)
  (goto-line line)
  (recenter (/ (view-window-size) 2)))

(defun View-scroll-lines-forward (&optional lines)
  "Scroll forward in View mode, or exit if end of text is visible.
No arg means whole window full, or number of lines set by \\[View-scroll-lines-forward-set-scroll-size].
Arg is number of lines to scroll."
  (interactive "P")
  (setq lines
	(if lines (prefix-numeric-value lines)
	  (view-scroll-size)))
  (if (and (pos-visible-in-window-p (point-max))
	   ;; Allow scrolling backward at the end of the buffer.
	   (> lines 0)
	   view-mode-auto-exit)
      (view-exit)
    ;; (view-last-command 'View-scroll-lines-forward lines)
    (if (>= lines (view-window-size))
	(scroll-up nil)
      (if (>= (- lines) (view-window-size))
	  (scroll-down nil)
	(scroll-up lines)))
    (cond ((pos-visible-in-window-p (point-max))
	   (goto-char (point-max))
	   (message "%s"
		    (substitute-command-keys
		     "End.  Type \\[view-exit] to quit viewing."))))
    (move-to-window-line -1)
    (beginning-of-line)))

(defun View-scroll-lines-forward-set-scroll-size (&optional lines)
  "Scroll forward LINES lines in View mode, setting the \"scroll size\".
This is the number of lines which \\[View-scroll-lines-forward] and \\[View-scroll-lines-backward] scroll by default.
The absolute value of LINES is used, so this command can be used to scroll
backwards (but \"scroll size\" is always positive).  If LINES is greater than
window height or omitted, then window height is assumed.  If LINES is less
than window height then scrolling context is provided from previous screen."
  (interactive "P")
  (if (not lines)
      (setq view-scroll-size (view-window-size))
    (setq lines (prefix-numeric-value lines))
    (setq view-scroll-size
	  (min (if (> lines 0) lines (- lines)) (view-window-size))))
  (View-scroll-lines-forward lines))

(defun View-scroll-one-more-line (&optional arg)
  "Scroll one more line up in View mode.
With ARG scroll one line down."
  (interactive "P")
  (View-scroll-lines-forward (if (not arg) 1 -1)))

(defun View-scroll-lines-backward (&optional lines)
  "Scroll backward in View mode.
No arg means whole window full, or number of lines set by \\[View-scroll-lines-forward-set-scroll-size].
Arg is number of lines to scroll."
  (interactive "P")
  (View-scroll-lines-forward (if lines
				 (- (prefix-numeric-value lines))
			       (- (view-scroll-size)))))
  
(defun View-search-regexp-forward (n regexp)
  "Search forward for Nth occurrence of REGEXP.
Displays line found at center of window.  REGEXP is remembered for
searching with \\[View-search-last-regexp-forward] and \\[View-search-last-regexp-backward].  Sets mark at starting position and pushes mark ring.

The variable `view-highlight-face' controls the face that is used
for highlighting the match that is found."
  (interactive "p\nsSearch forward (regexp): ")
;;;(view-last-command 'View-search-last-regexp-forward n)
  (view-search n (if (equal regexp "") view-last-regexp regexp)))

(defun View-search-regexp-backward (n regexp)
  "Search backward from window start for Nth instance of REGEXP.
Displays line found at center of window.  REGEXP is remembered for
searching with \\[View-search-last-regexp-forward] and \\[View-search-last-regexp-backward].  Sets mark at starting position and pushes mark ring.

The variable `view-highlight-face' controls the face that is used
for highlighting the match that is found."
  (interactive "p\nsSearch backward (regexp): ")
  (View-search-regexp-forward (- n)
			      (if (equal regexp "") view-last-regexp regexp)))

(defun View-search-last-regexp-forward (n)
  "Search forward from window end for Nth instance of last regexp.
Displays line found at center of window.  Sets mark at starting position
and pushes mark ring.

The variable `view-highlight-face' controls the face that is used
for highlighting the match that is found."
  (interactive "p")
  (if view-last-regexp
      (View-search-regexp-forward n view-last-regexp)
    (error "No previous View-mode search")))

(defun View-search-last-regexp-backward (n)
  "Search backward from window start for Nth instance of last regexp.
Displays line found at center of window.  Sets mark at starting position and
pushes mark ring.

The variable `view-highlight-face' controls the face that is used
for highlighting the match that is found."
  (interactive "p")
  (if view-last-regexp
      (View-search-regexp-backward n view-last-regexp)
    (error "No previous View-mode search")))

(defun View-back-to-mark (&optional ignore)
  "Return to last mark set in View mode, else beginning of file.
Displays line at center of window.  Pops mark ring so successive
invocations return to earlier marks."
  (interactive)
  (goto-char (or (mark t) (point-min)))
  (pop-mark)
  (recenter (/ (view-window-size) 2)))
	     
(defun view-search (times regexp)
  (setq view-last-regexp regexp)
  (let (where)
    (save-excursion
      (move-to-window-line (if (< times 0) 0 -1))
      (if (re-search-forward regexp nil t times)
	  (setq where (point))))
    (if where
	(progn
	  (push-mark)
	  (goto-char where)
	  (if view-overlay
	      (move-overlay view-overlay (match-beginning 0) (match-end 0))
	    (setq view-overlay
		  (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put view-overlay 'face view-highlight-face)
	  (beginning-of-line)
	  (recenter (/ (view-window-size) 2)))
      (message "Can't find occurrence %d of %s" times regexp)
      (sit-for 4))))


(provide 'view)

;;; view.el ends here
