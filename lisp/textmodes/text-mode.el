;;; text-mode.el --- text mode, and its idiosyncratic commands.

;; Copyright (C) 1985, 1992, 1994 Free Software Foundation, Inc.

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

;; This package provides the fundamental text mode documented in the
;; Emacs user's manual.

;;; Code:

(defvar text-mode-syntax-table nil
  "Syntax table used while in text mode.")

(defvar text-mode-abbrev-table nil
  "Abbrev table used while in text mode.")
(define-abbrev-table 'text-mode-abbrev-table ())

(if text-mode-syntax-table
    ()
  (setq text-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " text-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " text-mode-syntax-table)
  (modify-syntax-entry ?' "w   " text-mode-syntax-table))

(defvar text-mode-map nil
  "Keymap for Text mode.
Many other modes, such as Mail mode, Outline mode and Indented Text mode,
inherit all the commands defined in this map.")

(if text-mode-map
    ()
  (setq text-mode-map (make-sparse-keymap))
  (define-key text-mode-map "\e\t" 'ispell-complete-word)
  (define-key text-mode-map "\t" 'tab-to-tab-stop)
  (define-key text-mode-map "\es" 'center-line)
  (define-key text-mode-map "\eS" 'center-paragraph))


;(defun non-saved-text-mode ()
;  "Like text-mode, but delete auto save file when file is saved for real."
;  (text-mode)
;  (make-local-variable 'delete-auto-save-files)
;  (setq delete-auto-save-files t))

(defun text-mode ()
  "Major mode for editing text intended for humans to read.
Special commands:
\\{text-mode-map}
Turning on Text mode calls the value of the variable `text-mode-hook',
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (setq mode-name "Text")
  (setq major-mode 'text-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'text-mode-hook))

(defvar indented-text-mode-map ()
  "Keymap for Indented Text mode.
All the commands defined in Text mode are inherited unless overridden.")

(if indented-text-mode-map
    ()
  ;; Make different definition for TAB before the one in text-mode-map, but
  ;; share the rest.
  (let ((newmap (make-sparse-keymap)))
    (define-key newmap "\t" 'indent-relative)
    (setq indented-text-mode-map (nconc newmap text-mode-map))))

(defun indented-text-mode ()
  "Major mode for editing text with indented paragraphs.
In this mode, paragraphs are delimited only by blank lines.
You can thus get the benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{indented-text-mode-map}
Turning on `indented-text-mode' calls the value of the variable
`text-mode-hook', if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (use-local-map indented-text-mode-map)
  (setq mode-name "Indented Text")
  (setq major-mode 'indented-text-mode)
  (run-hooks 'text-mode-hook 'indented-text-mode-hook))

(defun center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (center-region (point) end))))

(defun center-region (from to)
  "Center each nonblank line starting in the region.
See `center-line' for more info."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(or (save-excursion (skip-chars-forward " \t") (eolp))
	    (center-line))
	(forward-line 1)))))

(defun center-line ()
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals
the distance between the end of the text and `fill-column'."
  (interactive)
  (save-excursion
    (let ((lm (current-left-margin))
	  line-length)
      (beginning-of-line)
      (delete-horizontal-space)
      (end-of-line)
      (delete-horizontal-space)
      (setq line-length (current-column))
      (indent-line-to 
	(+ lm (/ (- (fill-column) lm line-length) 2))))))

;;; text-mode.el ends here
