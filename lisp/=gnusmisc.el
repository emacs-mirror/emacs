;;; gnusmisc.el --- miscellaneous commands for GNUS newsreader

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Last-Modified: 10 Jun 1992

;; $Header: gnusmisc.el,v 1.2 90/03/23 13:25:04 umerin Locked $

;; Copyright (C) 1989, 1990 Free Software Foundation, Inc.

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

;;; Code:

(require 'gnus)

;;;
;;; GNUS Browse-Killed Mode
;;;

;; Some ideas are due to roland@wheaties.ai.mit.edu (Roland McGrath).
;; I'd like to thank him very much.

(defvar gnus-Browse-killed-mode-hook nil
  "*A hook for GNUS Browse-Killed Mode.")

(defvar gnus-Browse-killed-buffer "*Killed Newsgroup*")
(defvar gnus-Browse-killed-mode-map nil)
(defvar gnus-winconf-browse-killed nil)

(put 'gnus-Browse-killed-mode 'mode-class 'special)

;; Make the buffer to be managed by GNUS.

(or (memq gnus-Browse-killed-buffer gnus-buffer-list)
    (setq gnus-buffer-list
	  (cons gnus-Browse-killed-buffer gnus-buffer-list)))

(if gnus-Browse-killed-mode-map
    nil
  (setq gnus-Browse-killed-mode-map (make-keymap))
  (suppress-keymap gnus-Browse-killed-mode-map t)
  (define-key gnus-Browse-killed-mode-map " " 'gnus-Group-next-group)
  (define-key gnus-Browse-killed-mode-map "\177" 'gnus-Group-prev-group)
  (define-key gnus-Browse-killed-mode-map "\C-n" 'gnus-Group-next-group)
  (define-key gnus-Browse-killed-mode-map "\C-p" 'gnus-Group-prev-group)
  (define-key gnus-Browse-killed-mode-map "n" 'gnus-Group-next-group)
  (define-key gnus-Browse-killed-mode-map "p" 'gnus-Group-prev-group)
  (define-key gnus-Browse-killed-mode-map "y" 'gnus-Browse-killed-yank)
  (define-key gnus-Browse-killed-mode-map "\C-y" 'gnus-Browse-killed-yank)
  (define-key gnus-Browse-killed-mode-map "l" 'gnus-Browse-killed-groups)
  (define-key gnus-Browse-killed-mode-map "q" 'gnus-Browse-killed-exit)
  (define-key gnus-Browse-killed-mode-map "\C-c\C-c" 'gnus-Browse-killed-exit)
  (define-key gnus-Browse-killed-mode-map "\C-c\C-i" 'gnus-Info-find-node))

(defun gnus-Browse-killed-mode ()
  "Major mode for browsing the killed newsgroups.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-Browse-killed-mode-map}

The killed newsgroups are saved in the quick startup file \".newsrc.el\"
unless disabled inthe options line of the startup file \".newsrc\".

Entry to this mode calls `gnus-Browse-killed-mode-hook' with no arguments
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  ;; Gee.  Why don't you upgrade?
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format)))))
	(t
	 (setq mode-line-format
	       "--- GNUS: Killed Newsgroups  %[(%m)%]----%3p-%-")))
  (setq major-mode 'gnus-Browse-killed-mode)
  (setq mode-name "Browse-Killed")
  (setq mode-line-buffer-identification	"GNUS: Killed Newsgroups")
  (use-local-map gnus-Browse-killed-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-Browse-killed-mode-hook))

(defun gnus-Browse-killed-groups ()
  "Browse the killed newsgroups.
\\<gnus-Browse-killed-mode-map>\\[gnus-Browse-killed-yank] yanks the newsgroup on the current line into the Newsgroups buffer."
  (interactive)
  (or gnus-killed-assoc
      (error "No killed newsgroups"))
  ;; Save current window configuration if this is first invocation..
  (or (get-buffer-window gnus-Browse-killed-buffer)
      (setq gnus-winconf-browse-killed
	    (current-window-configuration)))
  ;; Prepare browsing buffer.
  (pop-to-buffer (get-buffer-create gnus-Browse-killed-buffer))
  (gnus-Browse-killed-mode)
  (let ((buffer-read-only nil)
	(killed-assoc gnus-killed-assoc))
    (erase-buffer)
    (while killed-assoc
      (insert (gnus-Group-prepare-line (car killed-assoc)))
      (setq killed-assoc (cdr killed-assoc)))
    (goto-char (point-min))
    ))

(defun gnus-Browse-killed-yank ()
  "Yank current newsgroup to Newsgroup buffer."
  (interactive)
  (let ((group (gnus-Group-group-name)))
    (if group
	(let* ((buffer-read-only nil)
	       (killed (assoc group gnus-killed-assoc)))
	  (pop-to-buffer gnus-Group-buffer) ;Needed to adjust point.
	  (if killed
	      (gnus-Group-insert-group killed))
	  (pop-to-buffer gnus-Browse-killed-buffer)
	  (beginning-of-line)
	  (delete-region (point)
			 (progn (forward-line 1) (point)))
	  )))
  (gnus-Browse-killed-check-buffer))

(defun gnus-Browse-killed-check-buffer ()
  "Exit if the buffer is empty by deleting the window and killing the buffer."
  (and (null gnus-killed-assoc)
       (get-buffer gnus-Browse-killed-buffer)
       (gnus-Browse-killed-exit)))

(defun gnus-Browse-killed-exit ()
  "Exit this mode by deleting the window and killing the buffer."
  (interactive)
  (and (get-buffer-window gnus-Browse-killed-buffer)
       (delete-window (get-buffer-window gnus-Browse-killed-buffer)))
  (kill-buffer gnus-Browse-killed-buffer)
  ;; Restore previous window configuration if available.
  (and gnus-winconf-browse-killed
       (set-window-configuration gnus-winconf-browse-killed))
  (setq gnus-winconf-browse-killed nil))


;;;
;;; kill/yank newsgroup commands of GNUS Group Mode
;;;

(defun gnus-Group-kill-group (n)
  "Kill newsgroup on current line, repeated prefix argument N times.
The killed newsgroups can be yanked by using \\[gnus-Group-yank-group]."
  (interactive "p")
  (let ((buffer-read-only nil)
	(group nil))
    (while (> n 0)
      (setq group (gnus-Group-group-name))
      (or group
	  (signal 'end-of-buffer nil))
      (beginning-of-line)
      (delete-region (point)
		     (progn (forward-line 1) (point)))
      (gnus-kill-newsgroup group)
      (setq n (1- n))
      ;; Add to killed newsgroups in the buffer if exists.
      (if (get-buffer gnus-Browse-killed-buffer)
	  (save-excursion
	    (set-buffer gnus-Browse-killed-buffer)
	    (let ((buffer-read-only nil))
	      (goto-char (point-min))
	      (insert (gnus-Group-prepare-line (car gnus-killed-assoc)))
	      )))
      )
    (search-forward ":" nil t)
    ))

(defun gnus-Group-yank-group ()
  "Yank the last newsgroup killed with \\[gnus-Group-kill-group],
inserting it before the newsgroup on the line containging point."
  (interactive)
  (gnus-Group-insert-group (car gnus-killed-assoc))
  ;; Remove killed newsgroups from the buffer if exists.
  (if (get-buffer gnus-Browse-killed-buffer)
      (save-excursion
	(set-buffer gnus-Browse-killed-buffer)
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (delete-region (point-min)
			 (progn (forward-line 1) (point)))
	  )))
  (gnus-Browse-killed-check-buffer))

(defun gnus-Group-insert-group (info)
  "Insert newsgroup at current line using `gnus-newsrc-assoc' INFO."
  (if (null gnus-killed-assoc)
      (error "No killed newsgroups"))
  (if (not gnus-have-all-newsgroups)
      (error
       (substitute-command-keys
	"Not all newsgroups are displayed.  Type \\[gnus-Group-list-all-groups] to display all newsgroups.")))
  (let ((buffer-read-only nil)
	(group (gnus-Group-group-name)))
    (gnus-insert-newsgroup info group)
    (beginning-of-line)
    (insert (gnus-Group-prepare-line info))
    (forward-line -1)
    (search-forward ":" nil t)
    ))

(provide 'gnusmisc)

;;; gnusmisc.el ends here
