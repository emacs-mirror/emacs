;;; buff-menu.el --- buffer menu main function and support functions.

;; Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Edit, delete, or change attributes of all currently active Emacs
;; buffers from a list summarizing their state.  A good way to browse
;; any special or scratch buffers you have loaded, since you can't find
;; them by filename.  The single entry point is `Buffer-menu-mode',
;; normally bound to C-x C-b.

;;; Change Log:

;; Merged by esr with recent mods to Emacs 19 buff-menu, 23 Mar 1993
;;
;; Modified by Bob Weiner, Motorola, Inc., 4/14/89
;;
;; Added optional backup argument to 'Buffer-menu-unmark' to make it undelete
;; current entry and then move to previous one.
;;
;; Based on FSF code dating back to 1985.

;;; Code:
 
;;;Not needed, now that q is now just quit and Buffer-menu-select is v.
;;;(defvar Buffer-menu-window-config nil
;;;  "Window configuration saved from entry to `buffer-menu'.")

; Put buffer *Buffer List* into proper mode right away
; so that from now on even list-buffers is enough to get a buffer menu.

(defvar Buffer-menu-buffer-column nil)

(defvar Buffer-menu-mode-map nil "")

(if Buffer-menu-mode-map
    ()
  (setq Buffer-menu-mode-map (make-keymap))
  (suppress-keymap Buffer-menu-mode-map t)
  (define-key Buffer-menu-mode-map "q" 'Buffer-menu-quit)
  (define-key Buffer-menu-mode-map "v" 'Buffer-menu-select)
  (define-key Buffer-menu-mode-map "2" 'Buffer-menu-2-window)
  (define-key Buffer-menu-mode-map "1" 'Buffer-menu-1-window)
  (define-key Buffer-menu-mode-map "f" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "\C-m" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "o" 'Buffer-menu-other-window)
  (define-key Buffer-menu-mode-map "\C-o" 'Buffer-menu-switch-other-window)
  (define-key Buffer-menu-mode-map "s" 'Buffer-menu-save)
  (define-key Buffer-menu-mode-map "d" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "\C-d" 'Buffer-menu-delete-backwards)
  (define-key Buffer-menu-mode-map "\C-k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "x" 'Buffer-menu-execute)
  (define-key Buffer-menu-mode-map " " 'next-line)
  (define-key Buffer-menu-mode-map "n" 'next-line)
  (define-key Buffer-menu-mode-map "p" 'previous-line)
  (define-key Buffer-menu-mode-map "\177" 'Buffer-menu-backup-unmark)
  (define-key Buffer-menu-mode-map "~" 'Buffer-menu-not-modified)
  (define-key Buffer-menu-mode-map "?" 'describe-mode)
  (define-key Buffer-menu-mode-map "u" 'Buffer-menu-unmark)
  (define-key Buffer-menu-mode-map "m" 'Buffer-menu-mark)
  (define-key Buffer-menu-mode-map "t" 'Buffer-menu-visit-tags-table)
  (define-key Buffer-menu-mode-map "%" 'Buffer-menu-toggle-read-only)
  (define-key Buffer-menu-mode-map "g" 'revert-buffer)
  (define-key Buffer-menu-mode-map [mouse-2] 'Buffer-menu-mouse-select)
)

;; Buffer Menu mode is suitable only for specially formatted data.
(put 'Buffer-menu-mode 'mode-class 'special)

(defun Buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<Buffer-menu-mode-map>
\\[Buffer-menu-mouse-select] -- select buffer you click on, in place of the buffer menu.
\\[Buffer-menu-this-window] -- select current line's buffer in place of the buffer menu.
\\[Buffer-menu-other-window] -- select that buffer in another window,
  so the buffer menu buffer remains visible in its window.
\\[Buffer-menu-switch-other-window] -- make another window display that buffer.
\\[Buffer-menu-mark] -- mark current line's buffer to be displayed.
\\[Buffer-menu-select] -- select current line's buffer.
  Also show buffers marked with m, in other windows.
\\[Buffer-menu-1-window] -- select that buffer in full-frame window.
\\[Buffer-menu-2-window] -- select that buffer in one window,
  together with buffer selected before this one in another window.
\\[Buffer-menu-visit-tags-table] -- visit-tags-table this buffer.
\\[Buffer-menu-not-modified] -- clear modified-flag on that buffer.
\\[Buffer-menu-save] -- mark that buffer to be saved, and move down.
\\[Buffer-menu-delete] -- mark that buffer to be deleted, and move down.
\\[Buffer-menu-delete-backwards] -- mark that buffer to be deleted, and move up.
\\[Buffer-menu-execute] -- delete or save marked buffers.
\\[Buffer-menu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[Buffer-menu-backup-unmark] -- back up a line and remove marks.
\\[Buffer-menu-toggle-read-only] -- toggle read-only status of buffer on this line."
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (save-excursion
    (goto-char (point-min))
    (search-forward "Buffer")
    (backward-word 1)
    (setq Buffer-menu-buffer-column (current-column))
    (forward-line 2)
    (while (not (eobp))
      (let ((where (Buffer-menu-buffer-name-position)))
	(put-text-property (car where) (cdr where) 'mouse-face 'highlight))
      (forward-line 1)))
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'Buffer-menu-revert-function)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'buffer-menu-mode-hook))

(defun Buffer-menu-revert-function (ignore1 ignore2)
  (list-buffers))

(defun Buffer-menu-buffer (error-if-non-existent-p)
  "Return buffer described by this line of buffer menu."
  (let* ((where (Buffer-menu-buffer-name-position))
	 (string (buffer-substring (car where) (cdr where))))
    (set-text-properties 0 (length string) nil string)
    (or (get-buffer string)
	(if error-if-non-existent-p
	    (error "No buffer named \"%s\"" string)
	  nil))))

;; Find the start and end positions of the buffer name on this line.
;; Returns a cons (START . END).
(defun Buffer-menu-buffer-name-position ()
  (save-excursion
    (beginning-of-line)
    (forward-char Buffer-menu-buffer-column)
    (let ((start (point)))
      (re-search-forward "\t\\|  ")
      (skip-chars-backward " \t")
      (cons start (point)))))

(defun buffer-menu (&optional arg)
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away and to restore
previous window configuration."
  (interactive "P")
;;;  (setq Buffer-menu-window-config (current-window-configuration))
  (list-buffers arg)
  (let ((newpoint (save-excursion (set-buffer "*Buffer List*")
				  (point))))
    (pop-to-buffer "*Buffer List*")
    (goto-char newpoint))
  (message
   "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun Buffer-menu-quit ()
  "Quit the buffer menu."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; Restore previous window configuration before displaying
    ;; selected buffers.
    (switch-to-buffer (other-buffer))
    (bury-buffer buffer)))

(defun Buffer-menu-mark ()
  "Mark buffer on this line for being displayed by \\<Buffer-menu-mode-map>\\[Buffer-menu-select] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil))
      (delete-char 3)
      (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
  (forward-line (if backup -1 1)))

(defun Buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above."
  (interactive)
  (forward-line -1)
  (Buffer-menu-unmark)
  (forward-line -1))

(defun Buffer-menu-delete (&optional arg)
  "Mark buffer on this line to be deleted by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command.
Prefix arg is how many buffers to delete.
Negative arg means delete backwards."
  (interactive "p")
  (beginning-of-line)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (if (or (null arg) (= arg 0))
	  (setq arg 1))
      (while (> arg 0)
	(delete-char 1)
	(insert ?D)
	(forward-line 1)
	(setq arg (1- arg)))
      (while (< arg 0)
	(delete-char 1)
	(insert ?D)
	(forward-line -1)
	(setq arg (1+ arg))))))

(defun Buffer-menu-delete-backwards (&optional arg)
  "Mark buffer on this line to be deleted by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command
and then move up one line.  Prefix arg means move that many lines."
  (interactive "p")
  (Buffer-menu-delete (- (or arg 1)))
  (while (looking-at " [-M]")
    (forward-line 1)))

(defun Buffer-menu-save ()
  "Mark buffer on this line to be saved by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (forward-char 1)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?S)
      (forward-line 1))))

(defun Buffer-menu-not-modified (&optional arg)
  "Mark buffer on this line as unmodified (no changes to save)."
  (interactive "P")
  (save-excursion
    (set-buffer (Buffer-menu-buffer t))
    (set-buffer-modified-p arg))
  (save-excursion
   (beginning-of-line)
   (forward-char 1)
   (if (= (char-after (point)) (if arg ?  ?*))
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert (if arg ?* ? ))))))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-save] or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^.S" nil t)
      (let ((modp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (save-buffer)
	  (setq modp (buffer-modified-p)))
	(let ((buffer-read-only nil))
	  (delete-char -1)
	  (insert (if modp ?* ? ))))))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
	  (buffer-read-only nil))
      (while (search-forward "\nD" nil t)
	(forward-char -1)
	(let ((buf (Buffer-menu-buffer nil)))
	  (or (eq buf nil)
	      (eq buf buff-menu-buffer)
	      (save-excursion (kill-buffer buf))))
	(if (Buffer-menu-buffer nil)
	    (progn (delete-char 1)
		   (insert ? ))
	  (delete-region (point) (progn (forward-line 1) (point)))
 	  (forward-char -1))))))

(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with the \\<Buffer-menu-mode-map>\\[Buffer-menu-mark] command."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))	      
	(others ())
	tem)
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil))
	(delete-char -1)
	(insert ?\ ))
      (or (eq tem buff) (memq tem others) (setq others (cons tem others))))
    (setq others (nreverse others)
	  tem (/ (1- (frame-height)) (1+ (length others))))
    (delete-other-windows)
    (switch-to-buffer buff)
    (or (eq menu buff)
	(bury-buffer menu))
    (if (equal (length others) 0)
	(progn
;;;	  ;; Restore previous window configuration before displaying
;;;	  ;; selected buffers.
;;;	  (if Buffer-menu-window-config
;;;	      (progn
;;;		(set-window-configuration Buffer-menu-window-config)
;;;		(setq Buffer-menu-window-config nil)))
	  (switch-to-buffer buff))
      (while others
	(split-window nil tem)
	(other-window 1)
	(switch-to-buffer (car others))
	(setq others (cdr others)))
      (other-window 1)  			;back to the beginning!
)))


(defun Buffer-menu-visit-tags-table ()
  "Visit the tags table in the buffer on this line.  See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (Buffer-menu-buffer t))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun Buffer-menu-1-window ()
  "Select this line's buffer, alone, in full frame."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t))
  (bury-buffer (other-buffer))
  (delete-other-windows))

(defun Buffer-menu-mouse-select (event)
  "Select the buffer whose line you click on."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq buffer (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (if (and (window-dedicated-p (selected-window))
	     (eq (selected-window) (frame-root-window)))
	(switch-to-buffer-other-frame buffer)
      (switch-to-buffer buffer))))

(defun Buffer-menu-this-window ()
  "Select this line's buffer in this window."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t)))

(defun Buffer-menu-other-window ()
  "Select this line's buffer in other window, leaving buffer menu visible."
  (interactive)
  (switch-to-buffer-other-window (Buffer-menu-buffer t)))

(defun Buffer-menu-switch-other-window ()
  "Make the other window select this line's buffer.
The current window remains selected."
  (interactive)
  (display-buffer (Buffer-menu-buffer t)))

(defun Buffer-menu-2-window ()
  "Select this line's buffer, with previous buffer in second window."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(pop-up-windows t))
    (delete-other-windows)
    (switch-to-buffer (other-buffer))
    (pop-to-buffer buff)
    (bury-buffer menu)))

(defun Buffer-menu-toggle-read-only ()
  "Toggle read-only status of buffer on this line, perhaps via version control."
  (interactive)
  (let (char)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (vc-toggle-read-only)
      (setq char (if buffer-read-only ?% ? )))
    (save-excursion
      (beginning-of-line)
      (forward-char 2)
      (if (/= (following-char) char)
          (let (buffer-read-only)
            (delete-char 1)
            (insert char))))))



(define-key ctl-x-map "\C-b" 'list-buffers)

(defun list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive "P")
  (let ((old-buffer (current-buffer))
	(blist-buffer (get-buffer-create "*Buffer List*"))
	(desired-point nil))
    (with-output-to-temp-buffer "*Buffer List*"
      (set-buffer standard-output)
      (princ "\
 MR Buffer           Size  Mode         File
 -- ------           ----  ----         ----
")
      (let ((bl (buffer-list)))
	(while bl
	  (let* ((buffer (car bl))
		 (name (buffer-name buffer))
		 (file (buffer-file-name buffer)))
	    (cond
	     ;; Don't mention internal buffers.
	     ((string= (substring name 0 1) " "))
	     ;; Maybe don't mention buffers without files.
	     ((and files-only (not file)))
	     ;; Otherwise output info.
	     (t
	      ;; Identify current buffer.
	      (if (eq buffer old-buffer)
		  (progn
		    (setq desired-point (point))
		    (princ "."))
		(princ " "))
	      ;; Identify modified buffers.
	      (princ (if (buffer-modified-p buffer) "*" " "))
	      ;; Handle readonly status.  The output buffer is special
	      ;; cased to be readonly; it is actually made so at a later
	      ;; date.
	      (princ (if (or (eq buffer standard-output) buffer-read-only)
			 "% "
		       "  "))
	      (princ name)
	      (indent-to 17 2)
	      (let (size
		    mode
		    (excess (- (current-column) 17)))
		(save-excursion
		  (set-buffer buffer)
		  (setq size (format "%8d" (buffer-size)))
		  ;; Ack -- if looking at the *Buffer List* buffer,
		  ;; always use "Buffer Menu" mode.  Otherwise the
		  ;; first time the buffer is created, the mode will
		  ;; be wrong.
		  (setq mode (if (eq buffer standard-output)
				 "Buffer Menu"
			       mode-name))
		  (while (and (> excess 0) (= (aref size 0) ?\ ))
		    (setq size (substring size 1))
		    (setq excess (1- excess))))
		(princ size)
		(indent-to 27 1)
		(princ mode))
	      (indent-to 40 1)
	      (or file
		  ;; No visited file.  Check local value of
		  ;; list-buffers-directory.
		  (save-excursion
		    (set-buffer buffer)
		    (if (and (boundp 'list-buffers-directory)
			     list-buffers-directory)
			(setq file list-buffers-directory))))
	      (if file
		  (princ file))
	      (princ "\n"))))
	  (setq bl (cdr bl)))))
    ;; DESIRED-POINT doesn't have to be set; it is not when the
    ;; current buffer is not displayed for some reason.
    (save-excursion
      (set-buffer blist-buffer)
      (Buffer-menu-mode)
      (and desired-point
	   (goto-char desired-point)))))

;;; buff-menu.el ends here
