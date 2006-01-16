;;; tabify.el --- tab conversion commands for Emacs

;; Copyright (C) 1985, 1994, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Commands to optimize spaces to tabs or expand tabs to spaces in a region
;; (`tabify' and `untabify').  The variable tab-width does the obvious.

;;; Code:

;;;###autoload
(defun untabify (start end)
  "Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) end)
      (goto-char start)
      (while (search-forward "\t" nil t)	; faster than re-search
	(forward-char -1)
	(let ((tab-beg (point))
	      (indent-tabs-mode nil)
	      column)
	  (skip-chars-forward "\t")
	  (setq column (current-column))
	  (delete-region tab-beg (point))
	  (indent-to column))))))

(defvar tabify-regexp "[ \t][ \t]+"
  "Regexp matching whitespace that tabify should consider.
Usually this will be \"[ \\t][ \\t]+\" to match two or more spaces or tabs.
\"^[ \\t]+\" is also useful, for tabifying only initial whitespace.")

;;;###autoload
(defun tabify (start end)
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Include the beginning of the line in the narrowing
      ;; since otherwise it will throw off current-column.
      (goto-char start)
      (beginning-of-line)
      (narrow-to-region (point) end)
      (goto-char start)
      (while (re-search-forward tabify-regexp nil t)
	(let ((column (current-column))
	      (indent-tabs-mode t))
	  (delete-region (match-beginning 0) (point))
	  (indent-to column))))))

(provide 'tabify)

;;; arch-tag: c83893b1-e0cc-4e57-8a09-73fd03466416
;;; tabify.el ends here
