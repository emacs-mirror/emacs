;;; underline.el --- insert/remove underlining (done by overstriking) in Emacs

;; Copyright (C) 1985 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp

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

;; This package deals with the primitive form of underlining
;; consisting of prefixing each character with "_\^h".  The entry
;; point `underline-region' performs such underlining on a region.
;; The entry point `ununderline-region' removes it.

;;; Code:

;;;###autoload
(defun underline-region (start end)
  "Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "*r")
  (save-excursion
   (let ((end1 (make-marker)))
     (move-marker end1 (max start end))
     (goto-char (min start end))
     (while (< (point) end1)
       (or (looking-at "[_\^@- ]")
	   (insert "_\b"))
       (forward-char 1)))))

;;;###autoload
(defun ununderline-region (start end)
  "Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "*r")
  (save-excursion
   (let ((end1 (make-marker)))
     (move-marker end1 (max start end))
     (goto-char (min start end))
     (while (re-search-forward "_\b\\|\b_" end1 t)
       (delete-char -2)))))

(provide 'underline)

;;; arch-tag: e7b48582-c3ea-4386-987a-87415f3c372a
;;; underline.el ends here
