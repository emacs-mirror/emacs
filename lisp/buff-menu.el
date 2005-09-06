;;; buff-menu.el --- buffer menu main function and support functions -*- coding:utf-8 -*-

;; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: convenience

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

;; Edit, delete, or change attributes of all currently active Emacs
;; buffers from a list summarizing their state.  A good way to browse
;; any special or scratch buffers you have loaded, since you can't find
;; them by filename.  The single entry point is `list-buffers',
;; normally bound to C-x C-b.

;;; Change Log:

;; Buffer-menu-view: New function
;; Buffer-menu-view-other-window: New function

;; Merged by esr with recent mods to Emacs 19 buff-menu, 23 Mar 1993
;;
;; Modified by Bob Weiner, Motorola, Inc., 4/14/89
;;
;; Added optional backup argument to 'Buffer-menu-unmark' to make it undelete
;; current entry and then move to previous one.
;;
;; Based on FSF code dating back to 1985.

;;; Code:

;;Trying to preserve the old window configuration works well in
;;simple scenarios, when you enter the buffer menu, use it, and exit it.
;;But it does strange things when you switch back to the buffer list buffer
;;with C-x b, later on, when the window configuration is different.
;;The choice seems to be, either restore the window configuration
;;in all cases, or in no cases.
;;I decided it was better not to restore the window config at all. -- rms.

;;But since then, I changed buffer-menu to use the selected window,
;;so q now once again goes back to the previous window configuration.

;;(defvar Buffer-menu-window-config nil
;;  "Window configuration saved from entry to `buffer-menu'.")

;; Put buffer *Buffer List* into proper mode right away
;; so that from now on even list-buffers is enough to get a buffer menu.

(defgroup Buffer-menu nil
  "Show a menu of all buffers in a buffer."
  :group 'tools
  :group 'convenience)

(defcustom Buffer-menu-use-header-line t
  "*Non-nil means to use an immovable header-line."
  :type 'boolean
  :group 'Buffer-menu)

(defface Buffer-menu-buffer
  '((t (:weight bold)))
  "Face used to highlight buffer name."
  :group 'Buffer-menu
  :group 'font-lock-highlighting-faces)

(defcustom Buffer-menu-buffer+size-width 26
  "*How wide to jointly make the buffer name and size columns."
  :type 'number
  :group 'Buffer-menu)

(defcustom Buffer-menu-mode-width 16
  "*How wide to make the mode name column."
  :type 'number
  :group 'Buffer-menu)

;; This should get updated & resorted when you click on a column heading
(defvar Buffer-menu-sort-column nil
  "*2 for sorting by buffer names.  5 for sorting by file names.
nil for default sorting by visited order.")

(defconst Buffer-menu-buffer-column 4)

(defvar Buffer-menu-mode-map nil
  "Local keymap for `Buffer-menu-mode' buffers.")

(defvar Buffer-menu-files-only nil
  "Non-nil if the current buffer-menu lists only file buffers.
This variable determines whether reverting the buffer lists only
file buffers.  It affects both manual reverting and reverting by
Auto Revert Mode.")

(make-variable-buffer-local 'Buffer-menu-files-only)

(if Buffer-menu-mode-map
    ()
  (setq Buffer-menu-mode-map (make-keymap))
  (suppress-keymap Buffer-menu-mode-map t)
  (define-key Buffer-menu-mode-map "q" 'quit-window)
  (define-key Buffer-menu-mode-map "v" 'Buffer-menu-select)
  (define-key Buffer-menu-mode-map "2" 'Buffer-menu-2-window)
  (define-key Buffer-menu-mode-map "1" 'Buffer-menu-1-window)
  (define-key Buffer-menu-mode-map "f" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "e" 'Buffer-menu-this-window)
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
  (define-key Buffer-menu-mode-map "b" 'Buffer-menu-bury)
  (define-key Buffer-menu-mode-map "g" 'Buffer-menu-revert)
  (define-key Buffer-menu-mode-map "V" 'Buffer-menu-view)
  (define-key Buffer-menu-mode-map "T" 'Buffer-menu-toggle-files-only)
  (define-key Buffer-menu-mode-map [mouse-2] 'Buffer-menu-mouse-select)
  (define-key Buffer-menu-mode-map [follow-link] 'mouse-face)
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
\\[Buffer-menu-view] -- select current line's buffer, but in view-mode.
\\[Buffer-menu-view-other-window] -- select that buffer in
  another window, in view-mode.
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
\\[Buffer-menu-toggle-read-only] -- toggle read-only status of buffer on this line.
\\[Buffer-menu-revert] -- update the list of buffers.
\\[Buffer-menu-toggle-files-only] -- toggle whether the menu displays only file buffers.
\\[Buffer-menu-bury] -- bury the buffer listed on this line."
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (set (make-local-variable 'revert-buffer-function)
       'Buffer-menu-revert-function)
  (set (make-local-variable 'buffer-stale-function)
       #'(lambda (&optional noconfirm) 'fast))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-mode-hooks 'buffer-menu-mode-hook))

;; This function exists so we can make the doc string of Buffer-menu-mode
;; look nice.
(defun Buffer-menu-revert ()
  "Update the list of buffers."
  (interactive)
  (revert-buffer))

(defun Buffer-menu-revert-function (ignore1 ignore2)
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  ;; We can not use save-excursion here.  The buffer gets erased.
  (let ((opoint (point))
	(eobp (eobp))
	(ocol (current-column))
	(oline (progn (move-to-column 4)
		      (get-text-property (point) 'buffer)))
	(prop (point-min))
	;; do not make undo records for the reversion.
	(buffer-undo-list t))
    (list-buffers-noselect Buffer-menu-files-only)
    (if oline
	(while (setq prop (next-single-property-change prop 'buffer))
	  (when (eq (get-text-property prop 'buffer) oline)
	    (goto-char prop)
	    (move-to-column ocol)))
      (goto-char (if eobp (point-max) opoint)))))

(defun Buffer-menu-toggle-files-only (arg)
  "Toggle whether the current buffer-menu displays only file buffers.
With a positive ARG display only file buffers.  With zero or
negative ARG, display other buffers as well."
  (interactive "P")
  (setq Buffer-menu-files-only
	(cond ((not arg) (not Buffer-menu-files-only))
	      ((> (prefix-numeric-value arg) 0) t)))
  (revert-buffer))


(defun Buffer-menu-buffer (error-if-non-existent-p)
  "Return buffer described by this line of buffer menu."
  (let* ((where (save-excursion
		  (beginning-of-line)
		  (+ (point) Buffer-menu-buffer-column)))
	 (name (and (not (eobp)) (get-text-property where 'buffer-name)))
	 (buf (and (not (eobp)) (get-text-property where 'buffer))))
    (if name
	(or (get-buffer name)
	    (and buf (buffer-name buf) buf)
	    (if error-if-non-existent-p
		(error "No buffer named `%s'" name)
	      nil))
      (or (and buf (buffer-name buf) buf)
	  (if error-if-non-existent-p
	      (error "No buffer on this line")
	    nil)))))

(defun buffer-menu (&optional arg)
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q to remove the buffer menu from the display.

The first column shows `>' for a buffer you have
marked to be displayed, `D' for one you have marked for
deletion, and `.' for the current buffer.

The C column has a `.' for the buffer from which you came.
The R column has a `%' if the buffer is read-only.
The M column has a `*' if it is modified,
or `S' if you have marked it for saving.
After this come the buffer name, its size in characters,
its major mode, and the visited file name (if any)."
  (interactive "P")
;;;  (setq Buffer-menu-window-config (current-window-configuration))
  (switch-to-buffer (list-buffers-noselect arg))
  (message
   "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun buffer-menu-other-window (&optional arg)
  "Display a list of buffers in another window.
With the buffer list buffer, you can save, delete or select the buffers.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q to remove the buffer menu from the display.
For more information, see the function `buffer-menu'."
  (interactive "P")
;;;  (setq Buffer-menu-window-config (current-window-configuration))
  (switch-to-buffer-other-window (list-buffers-noselect arg))
  (message
   "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(defun Buffer-menu-no-header ()
  (beginning-of-line)
  (if (or Buffer-menu-use-header-line
	  (not (eq (char-after) ?C)))
      t
    (ding)
    (forward-line 1)
    nil))

(defun Buffer-menu-mark ()
  "Mark buffer on this line for being displayed by \\<Buffer-menu-mode-map>\\[Buffer-menu-select] command."
  (interactive)
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional prefix arg means move up."
  (interactive "P")
  (when (Buffer-menu-no-header)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil))
      (delete-char 3)
      (insert (if readonly (if mod " %*" " % ") (if mod "  *" "   ")))))
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
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (if (or (null arg) (= arg 0))
	  (setq arg 1))
      (while (> arg 0)
	(delete-char 1)
	(insert ?D)
	(forward-line 1)
	(setq arg (1- arg)))
      (while (and (< arg 0)
		  (Buffer-menu-no-header))
	(delete-char 1)
	(insert ?D)
	(forward-line -1)
	(setq arg (1+ arg))))))

(defun Buffer-menu-delete-backwards (&optional arg)
  "Mark buffer on this line to be deleted by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command
and then move up one line.  Prefix arg means move that many lines."
  (interactive "p")
  (Buffer-menu-delete (- (or arg 1))))

(defun Buffer-menu-save ()
  "Mark buffer on this line to be saved by \\<Buffer-menu-mode-map>\\[Buffer-menu-execute] command."
  (interactive)
  (when (Buffer-menu-no-header)
    (let ((buffer-read-only nil))
      (forward-char 2)
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
   (forward-char 2)
   (if (= (char-after) (if arg ?\s ?*))
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert (if arg ?* ?\s))))))

(defun Buffer-menu-beginning ()
  (goto-char (point-min))
  (unless Buffer-menu-use-header-line
    (forward-line)))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-save] or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (save-excursion
    (Buffer-menu-beginning)
    (while (re-search-forward "^..S" nil t)
      (let ((modp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (save-buffer)
	  (setq modp (buffer-modified-p)))
	(let ((buffer-read-only nil))
	  (delete-char -1)
	  (insert (if modp ?* ?\s))))))
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buff-menu-buffer (current-buffer))
	  (buffer-read-only nil))
      (while (re-search-forward "^D" nil t)
	(forward-char -1)
	(let ((buf (Buffer-menu-buffer nil)))
	  (or (eq buf nil)
	      (eq buf buff-menu-buffer)
	      (save-excursion (kill-buffer buf)))
	  (if (and buf (buffer-name buf))
	    (progn (delete-char 1)
		   (insert ?\s))
	  (delete-region (point) (progn (forward-line 1) (point)))
	    (unless (bobp)
	      (forward-char -1))))))))

(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with the \\<Buffer-menu-mode-map>\\[Buffer-menu-mark] command.
This command deletes and replaces all the previously existing windows
in the selected frame."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(others ())
	tem)
    (Buffer-menu-beginning)
    (while (re-search-forward "^>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil))
	(delete-char -1)
	(insert ?\s))
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
  (let ((pop-up-windows t)
	same-window-buffer-names
	same-window-regexps)
    (display-buffer (Buffer-menu-buffer t))))

(defun Buffer-menu-2-window ()
  "Select this line's buffer, with previous buffer in second window."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(pop-up-windows t)
	same-window-buffer-names
	same-window-regexps)
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
      (setq char (if buffer-read-only ?% ?\s)))
    (save-excursion
      (beginning-of-line)
      (forward-char 1)
      (if (/= (following-char) char)
          (let (buffer-read-only)
            (delete-char 1)
            (insert char))))))

(defun Buffer-menu-bury ()
  "Bury the buffer listed on this line."
  (interactive)
  (when (Buffer-menu-no-header)
    (save-excursion
      (beginning-of-line)
      (bury-buffer (Buffer-menu-buffer t))
      (let ((line (buffer-substring (point) (progn (forward-line 1) (point))))
            (buffer-read-only nil))
        (delete-region (point) (progn (forward-line -1) (point)))
        (goto-char (point-max))
        (insert line))
      (message "Buried buffer moved to the end"))))


(defun Buffer-menu-view ()
  "View this line's buffer in View mode."
  (interactive)
  (view-buffer (Buffer-menu-buffer t)))


(defun Buffer-menu-view-other-window ()
  "View this line's buffer in View mode in another window."
  (interactive)
  (view-buffer-other-window (Buffer-menu-buffer t)))


(define-key ctl-x-map "\C-b" 'list-buffers)

(defun list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

For more information, see the function `buffer-menu'."
  (interactive "P")
  (display-buffer (list-buffers-noselect files-only)))

(defun Buffer-menu-buffer+size (name size &optional name-props size-props)
  (if (> (+ (length name) (length size) 2) Buffer-menu-buffer+size-width)
      (setq name
	    (if (string-match "<[0-9]+>$" name)
		(concat (substring name 0
				   (- Buffer-menu-buffer+size-width
				      (max (length size) 3)
				      (match-end 0)
				      (- (match-beginning 0))
				      2))
			":"		; narrow ellipsis
			(match-string 0 name))
	      (concat (substring name 0
				 (- Buffer-menu-buffer+size-width
				    (max (length size) 3)
				    2))
		      ":")))		; narrow ellipsis
    ;; Don't put properties on (buffer-name).
    (setq name (copy-sequence name)))
  (add-text-properties 0 (length name) name-props name)
  (add-text-properties 0 (length size) size-props size)
  (concat name
	  (make-string (- Buffer-menu-buffer+size-width
			  (length name)
			  (length size))
		       ?\s)
	  size))

(defun Buffer-menu-sort (column)
  "Sort the buffer menu by COLUMN."
  (interactive "P")
  (when column
    (setq column (prefix-numeric-value column))
    (if (< column 2) (setq column 2))
    (if (> column 5) (setq column 5)))
  (setq Buffer-menu-sort-column column)
  (let (buffer-read-only l buf m1 m2)
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
	(when (buffer-live-p (setq buf (get-text-property (+ (point) 4) 'buffer)))
	  (setq m1 (char-after)
		m1 (if (memq m1 '(?> ?D)) m1)
		m2 (char-after (+ (point) 2))
		m2 (if (eq m2 ?S) m2))
	  (if (or m1 m2)
	      (push (list buf m1 m2) l)))
	(forward-line)))
    (Buffer-menu-revert)
    (setq buffer-read-only)
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
	(when (setq buf (assq (get-text-property (+ (point) 4) 'buffer) l))
	  (setq m1 (cadr buf)
		m2 (cadr (cdr buf)))
	  (when m1
	    (delete-char 1)
	    (insert m1)
	    (backward-char 1))
	  (when m2
	    (forward-char 2)
	    (delete-char 1)
	    (insert m2)))
	(forward-line)))))

(defun Buffer-menu-make-sort-button (name column)
  (if (equal column Buffer-menu-sort-column) (setq column nil))
  (let* ((downname (downcase name))
         (map (make-sparse-keymap))
         (fun `(lambda (&optional e)
                 ,(concat "Sort the buffer menu by " downname ".")
                 (interactive (list last-input-event))
                 (if e (mouse-select-window e))
                 (Buffer-menu-sort ,column)))
         (sym (intern (format "Buffer-menu-sort-by-%s-%s" name column))))
    ;; Use a symbol rather than an anonymous function, to make the output of
    ;; C-h k less intimidating.
    (fset sym fun)
    (setq fun sym)
    ;; This keymap handles both nil and non-nil
    ;; values for Buffer-menu-use-header-line.
    (define-key map [header-line mouse-1] fun)
    (define-key map [header-line mouse-2] fun)
    (define-key map [mouse-2] fun)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" fun)
    (propertize name
                'help-echo (concat
                            (if Buffer-menu-use-header-line
                                "mouse-1, mouse-2: sort by "
                              "mouse-2, RET: sort by ")
                            ;; No clue what this is for, but I preserved the
                            ;; behavior, just in case.  --Stef
                            (if column downname "visited order"))
                'mouse-face 'highlight
                'keymap map)))

(defun list-buffers-noselect (&optional files-only buffer-list)
  "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

If BUFFER-LIST is non-nil, it should be a list of buffers;
it means list those buffers and no others.

For more information, see the function `buffer-menu'."
  (let* ((old-buffer (current-buffer))
	 (standard-output standard-output)
	 (mode-end (make-string (- Buffer-menu-mode-width 2) ?\s))
	 (header (concat "CRM "
			 (Buffer-menu-buffer+size
			  (Buffer-menu-make-sort-button "Buffer" 2)
			  (Buffer-menu-make-sort-button "Size" 3))
			 "  "
			 (Buffer-menu-make-sort-button "Mode" 4) mode-end
			 (Buffer-menu-make-sort-button "File" 5) "\n"))
	 list desired-point)
    (when Buffer-menu-use-header-line
      (let ((pos 0))
	;; Turn spaces in the header into stretch specs so they work
	;; regardless of the header-line face.
	(while (string-match "[ \t]+" header pos)
	  (setq pos (match-end 0))
	  (put-text-property (match-beginning 0) pos 'display
			     ;; Assume fixed-size chars in the buffer.
			     (list 'space :align-to pos)
			     header)))
      ;; Try to better align the one-char headers.
      (put-text-property 0 3 'face 'fixed-pitch header)
      ;; Add a "dummy" leading space to align the beginning of the header
      ;; line with the beginning of the text (rather than with the left
      ;; scrollbar or the left fringe).  –-Stef
      (setq header (concat (propertize " " 'display '(space :align-to 0))
			   header)))
    (with-current-buffer (get-buffer-create "*Buffer List*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (unless Buffer-menu-use-header-line
	(let ((underline (if (char-displayable-p ?—) ?— ?-)))
	  (insert header
		  (apply 'string
			 (mapcar (lambda (c)
				   (if (memq c '(?\n ?\s)) c underline))
				 header)))))
      ;; Collect info for every buffer we're interested in.
      (dolist (buffer (or buffer-list (buffer-list)))
	(with-current-buffer buffer
	  (let ((name (buffer-name))
		(file buffer-file-name))
	    (unless (and (not buffer-list)
			 (or
			  ;; Don't mention internal buffers.
			  (and (string= (substring name 0 1) " ") (null file))
			  ;; Maybe don't mention buffers without files.
			  (and files-only (not file))
			  (string= name "*Buffer List*")))
	      ;; Otherwise output info.
	      (let ((mode (concat (format-mode-line mode-name nil nil buffer)
				  (if mode-line-process
				      (format-mode-line mode-line-process
							nil nil buffer))))
		    (bits (string
			   (if (eq buffer old-buffer) ?. ?\s)
			   ;; Handle readonly status.  The output buffer
			   ;; is special cased to appear readonly; it is
			   ;; actually made so at a later date.
			   (if (or (eq buffer standard-output)
				   buffer-read-only)
			       ?% ?\s)
			   ;; Identify modified buffers.
			   (if (buffer-modified-p) ?* ?\s)
			   ;; Space separator.
			   ?\s)))
		(unless file
		  ;; No visited file.  Check local value of
		  ;; list-buffers-directory.
		  (when (and (boundp 'list-buffers-directory)
			     list-buffers-directory)
		    (setq file list-buffers-directory)))
		(push (list buffer bits name (buffer-size) mode file)
		      list))))))
      ;; Preserve the original buffer-list ordering, just in case.
      (setq list (nreverse list))
      ;; Place the buffers's info in the output buffer, sorted if necessary.
      (dolist (buffer
	       (if Buffer-menu-sort-column
		   (sort list
			 (if (eq Buffer-menu-sort-column 3)
			     (lambda (a b)
			       (< (nth Buffer-menu-sort-column a)
				  (nth Buffer-menu-sort-column b)))
			   (lambda (a b)
			     (string< (nth Buffer-menu-sort-column a)
				      (nth Buffer-menu-sort-column b)))))
		 list))
	(if (eq (car buffer) old-buffer)
	    (setq desired-point (point)))
	(insert (cadr buffer)
		;; Put the buffer name into a text property
		;; so we don't have to extract it from the text.
		;; This way we avoid problems with unusual buffer names.
		(Buffer-menu-buffer+size (nth 2 buffer)
					 (int-to-string (nth 3 buffer))
					 `(buffer-name ,(nth 2 buffer)
					   buffer ,(car buffer)
					   font-lock-face Buffer-menu-buffer
					   mouse-face highlight
					   help-echo "mouse-2: select this buffer"))
		"  "
		(if (> (length (nth 4 buffer)) Buffer-menu-mode-width)
		    (substring (nth 4 buffer) 0 Buffer-menu-mode-width)
		  (nth 4 buffer)))
	(when (nth 5 buffer)
	  (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-width
			Buffer-menu-mode-width 4) 1)
	  (princ (abbreviate-file-name (nth 5 buffer))))
	(princ "\n"))
      (Buffer-menu-mode)
      (when Buffer-menu-use-header-line
	(setq header-line-format header))
      ;; DESIRED-POINT doesn't have to be set; it is not when the
      ;; current buffer is not displayed for some reason.
      (and desired-point
	   (goto-char desired-point))
      (setq Buffer-menu-files-only files-only)
      (set-buffer-modified-p nil)
      (current-buffer))))

;; arch-tag: e7dfcfc9-6cb2-46e4-bf55-8ef1936d83c6
;;; buff-menu.el ends here
