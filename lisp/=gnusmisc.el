;;; gnusmisc.el --- miscellaneous commands for GNUS newsreader

;; Copyright (C) 1989, 1990, 1993 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news

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

(defvar gnus-browse-killed-mode-hook nil
  "*A hook for GNUS Browse-Killed Mode.")

(defvar gnus-browse-killed-buffer "*Killed Newsgroup*")
(defvar gnus-browse-killed-mode-map nil)
(defvar gnus-winconf-browse-killed nil)

(autoload 'timezone-make-date-arpa-standard "timezone")

(put 'gnus-browse-killed-mode 'mode-class 'special)


;;;
;;; GNUS Browse-Killed Mode
;;;

;; Some ideas are due to roland@wheaties.ai.mit.edu (Roland McGrath).
;; I'd like to thank him very much.

;; Make the buffer to be managed by GNUS.

(or (memq gnus-browse-killed-buffer gnus-buffer-list)
    (setq gnus-buffer-list
	  (cons gnus-browse-killed-buffer gnus-buffer-list)))

(if gnus-browse-killed-mode-map
    nil
  (setq gnus-browse-killed-mode-map (make-keymap))
  (suppress-keymap gnus-browse-killed-mode-map t)
  (define-key gnus-browse-killed-mode-map " " 'gnus-group-next-group)
  (define-key gnus-browse-killed-mode-map "\177" 'gnus-group-prev-group)
  (define-key gnus-browse-killed-mode-map "\C-n" 'gnus-group-next-group)
  (define-key gnus-browse-killed-mode-map "\C-p" 'gnus-group-prev-group)
  (define-key gnus-browse-killed-mode-map "n" 'gnus-group-next-group)
  (define-key gnus-browse-killed-mode-map "p" 'gnus-group-prev-group)
  (define-key gnus-browse-killed-mode-map "y" 'gnus-browse-killed-yank)
  (define-key gnus-browse-killed-mode-map "\C-y" 'gnus-browse-killed-yank)
  (define-key gnus-browse-killed-mode-map "l" 'gnus-list-killed-groups)
  (define-key gnus-browse-killed-mode-map "q" 'gnus-browse-killed-exit)
  (define-key gnus-browse-killed-mode-map "\C-c\C-c" 'gnus-browse-killed-exit)
  (define-key gnus-browse-killed-mode-map "\C-c\C-i" 'gnus-info-find-node))

(defun gnus-browse-killed-mode ()
  "Major mode for browsing the killed newsgroups.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-browse-killed-mode-map}

The killed newsgroups are saved in the quick startup file (.newsrc.el)
unless it against the options line in the startup file (.newsrc).

Entry to this mode calls gnus-browse-killed-mode-hook with no arguments,
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
  (setq major-mode 'gnus-browse-killed-mode)
  (setq mode-name "Browse-Killed")
  (setq mode-line-buffer-identification	"GNUS: Killed Newsgroups")
  (use-local-map gnus-browse-killed-mode-map)
  (buffer-flush-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-browse-killed-mode-hook))

(defun gnus-list-killed-groups ()
  "List the killed newsgroups.
The keys y and C-y yank the newsgroup on the current line into the
Newsgroups buffer."
  (interactive)
  (or gnus-killed-assoc
      (error "No killed newsgroups"))
  ;; Save current window configuration if this is first invocation..
  (or (get-buffer-window gnus-browse-killed-buffer)
      (setq gnus-winconf-browse-killed
	    (current-window-configuration)))
  ;; Prepare browsing buffer.
  (pop-to-buffer (get-buffer-create gnus-browse-killed-buffer))
  (gnus-browse-killed-mode)
  (let ((buffer-read-only nil)
	(killed-assoc gnus-killed-assoc))
    (erase-buffer)
    (while killed-assoc
      (insert (gnus-group-prepare-line (car killed-assoc)))
      (setq killed-assoc (cdr killed-assoc)))
    (goto-char (point-min))
    ))

(defun gnus-browse-killed-yank ()
  "Yank current newsgroup to Newsgroup buffer."
  (interactive)
  (let ((group (gnus-group-group-name)))
    (if group
	(let* ((buffer-read-only nil)
	       (killed (gnus-gethash group gnus-killed-hashtb)))
	  (pop-to-buffer gnus-group-buffer) ;Needed to adjust point.
	  (if killed
	      (gnus-group-insert-group killed))
	  (pop-to-buffer gnus-browse-killed-buffer)
	  (beginning-of-line)
	  (delete-region (point)
			 (progn (forward-line 1) (point)))
	  )))
  (gnus-browse-killed-check-buffer))

(defun gnus-browse-killed-check-buffer ()
  "Exit if the buffer is empty by deleting the window and killing the buffer."
  (and (null gnus-killed-assoc)
       (get-buffer gnus-browse-killed-buffer)
       (gnus-browse-killed-exit)))

(defun gnus-browse-killed-exit ()
  "Exit this mode by deleting the window and killing the buffer."
  (interactive)
  (and (get-buffer-window gnus-browse-killed-buffer)
       (delete-window (get-buffer-window gnus-browse-killed-buffer)))
  (kill-buffer gnus-browse-killed-buffer)
  ;; Restore previous window configuration if available.
  (and gnus-winconf-browse-killed
       (set-window-configuration gnus-winconf-browse-killed))
  (setq gnus-winconf-browse-killed nil))


;;;
;;; kill/yank newsgroup commands of GNUS Group Mode
;;;

(defun gnus-group-transpose-groups (arg)
  "Exchange current newsgroup and previous newsgroup.
With argument ARG, takes previous newsgroup and moves it past ARG newsgroup."
  (interactive "p")
  ;; BUG: last newsgroup and the last but one cannot be transposed
  ;; since gnus-group-search-forward does not move forward beyond the
  ;; last.  If we instead use forward-line, no problem, but I don't
  ;; want to use it for later extension.
  (while (> arg 0)
    (gnus-group-search-forward t t)
    (gnus-group-kill-group 1)
    (gnus-group-search-forward nil t)
    (gnus-group-yank-group)
    (gnus-group-search-forward nil t)
    (setq arg (1- arg))
    ))

(defun gnus-group-kill-region (begin end)
  "Kill newsgroups in current region (excluding current point).
The killed newsgroups can be yanked by using \\[gnus-group-yank-group]."
  (interactive "r")
  (let ((lines
	 ;; Exclude a line where current point is on.
	 (1-
	  ;; Count lines.
	  (save-excursion
	    (count-lines
	     (progn
	       (goto-char begin)
	       (beginning-of-line)
	       (point))
	     (progn
	       (goto-char end)
	       (end-of-line)
	       (point)))))))
    (goto-char begin)
    (beginning-of-line)			;Important when LINES < 1
    (gnus-group-kill-group lines)))

(defun gnus-group-kill-group (n)
  "Kill newsgroup on current line, repeated prefix argument N times.
The killed newsgroups can be yanked by using \\[gnus-group-yank-group]."
  (interactive "p")
  (let ((buffer-read-only nil)
	(group nil))
    (while (> n 0)
      (setq group (gnus-group-group-name))
      (or group
	  (signal 'end-of-buffer nil))
      (beginning-of-line)
      (delete-region (point)
		     (progn (forward-line 1) (point)))
      (gnus-kill-newsgroup group)
      (setq n (1- n))
      ;; Add to killed newsgroups in the buffer if exists.
      (if (get-buffer gnus-browse-killed-buffer)
	  (save-excursion
	    (set-buffer gnus-browse-killed-buffer)
	    (let ((buffer-read-only nil))
	      (goto-char (point-min))
	      (insert (gnus-group-prepare-line (car gnus-killed-assoc)))
	      )))
      )
    (search-forward ":" nil t)
    ))

(defun gnus-group-yank-group ()
  "Yank the last newsgroup killed with \\[gnus-group-kill-group],
inserting it before the newsgroup on the line containing point."
  (interactive)
  (gnus-group-insert-group (car gnus-killed-assoc))
  ;; Remove killed newsgroups from the buffer if exists.
  (if (get-buffer gnus-browse-killed-buffer)
      (save-excursion
	(set-buffer gnus-browse-killed-buffer)
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (delete-region (point-min)
			 (progn (forward-line 1) (point)))
	  )))
  (gnus-browse-killed-check-buffer))

(defun gnus-group-insert-group (info)
  "Insert newsgroup at current line using gnus-newsrc-assoc INFO."
  (if (null gnus-killed-assoc)
      (error "No killed newsgroups"))
  ;; Huuum.  It this right?
  ;;(if (not gnus-have-all-newsgroups)
  ;;    (error
  ;;     (substitute-command-keys
  ;;	"Not all newsgroups are displayed.  Type \\[gnus-group-list-all-groups] to display all newsgroups.")))
  (let ((buffer-read-only nil)
	(group (gnus-group-group-name)))
    (gnus-insert-newsgroup info group)
    (beginning-of-line)
    (insert (gnus-group-prepare-line info))
    (forward-line -1)
    (search-forward ":" nil t)
    ))


;;; Rewrite Date: field in GMT to local

(defun gnus-gmt-to-local ()
  "Rewrite Date: field described in GMT to local in current buffer.
The variable gnus-local-timezone is used for local time zone.
Intended to be used with gnus-article-prepare-hook."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (goto-char (point-min))
      (if (re-search-forward "^Date:[ \t]\\(.*\\)$" nil t)
	  (let ((buffer-read-only nil)
		(date (buffer-substring (match-beginning 1) (match-end 1))))
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert
	     (timezone-make-date-arpa-standard date nil gnus-local-timezone))
	    ))
      )))

(provide 'gnusmisc)

;;; gnusmisc.el ends here
