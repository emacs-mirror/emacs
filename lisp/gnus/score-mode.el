;;; score-mode.el --- mode for editing Gnus score files
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news, mail

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

;;; Code:

(require 'easymenu)
(require 'timezone)
(eval-when-compile (require 'cl))

(defvar gnus-score-mode-hook nil
  "*Hook run in score mode buffers.")

(defvar gnus-score-menu-hook nil
  "*Hook run after creating the score mode menu.")

(defvar gnus-score-edit-exit-function nil
  "Function run on exit from the score buffer.")

(defvar gnus-score-mode-map nil)
(unless gnus-score-mode-map
  (setq gnus-score-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key gnus-score-mode-map "\C-c\C-c" 'gnus-score-edit-exit)
  (define-key gnus-score-mode-map "\C-c\C-d" 'gnus-score-edit-insert-date)
  (define-key gnus-score-mode-map "\C-c\C-p" 'gnus-score-pretty-print))

;;;###autoload
(defun gnus-score-mode ()
  "Mode for editing Gnus score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-score-mode-map)
  (gnus-score-make-menu-bar)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'gnus-score-mode)
  (setq mode-name "Score")
  (lisp-mode-variables nil)
  (make-local-variable 'gnus-score-edit-exit-function)
  (run-hooks 'emacs-lisp-mode-hook 'gnus-score-mode-hook))

(defun gnus-score-make-menu-bar ()
  (unless (boundp 'gnus-score-menu)
    (easy-menu-define
     gnus-score-menu gnus-score-mode-map ""
     '("Score"
       ["Exit" gnus-score-edit-exit t]
       ["Insert date" gnus-score-edit-insert-date t]
       ["Format" gnus-score-pretty-print t]))
    (run-hooks 'gnus-score-menu-hook)))

(defun gnus-score-edit-insert-date ()
  "Insert date in numerical format."
  (interactive)
  (princ (gnus-score-day-number (current-time)) (current-buffer)))

(defun gnus-score-pretty-print ()
  "Format the current score file."
  (interactive)
  (goto-char (point-min))
  (let ((form (read (current-buffer))))
    (erase-buffer)
    (pp form (current-buffer)))
  (goto-char (point-min)))

(defun gnus-score-edit-exit ()
  "Stop editing the score file."
  (interactive)
  (unless (file-exists-p (file-name-directory (buffer-file-name)))
    (make-directory (file-name-directory (buffer-file-name)) t))
  (save-buffer)
  (bury-buffer (current-buffer))
  (let ((buf (current-buffer)))
    (when gnus-score-edit-exit-function
      (funcall gnus-score-edit-exit-function))
    (when (eq buf (current-buffer))
      (switch-to-buffer (other-buffer (current-buffer))))))

(defun gnus-score-day-number (time)
  (let ((dat (decode-time time)))
    (timezone-absolute-from-gregorian
     (nth 4 dat) (nth 3 dat) (nth 5 dat))))

(provide 'score-mode)

;;; score-mode.el ends here
