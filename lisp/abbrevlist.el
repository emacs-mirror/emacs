;;; abbrevlist.el --- list one abbrev table alphabetically ordered

;; Copyright (C) 1986, 1992, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;; Suggested by a previous version by Gildea.

;; Maintainer: FSF
;; Keywords: abbrev

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Code:

;;;###autoload
(defun list-one-abbrev-table (abbrev-table output-buffer)
  "Display alphabetical listing of ABBREV-TABLE in buffer OUTPUT-BUFFER."
  (with-output-to-temp-buffer output-buffer
    (save-excursion
      (let ((abbrev-list nil) (first-column 0))
	(set-buffer standard-output)
	(mapatoms
	  (function (lambda (abbrev)
		      (setq abbrev-list (cons abbrev abbrev-list))))
	  abbrev-table)
	(setq abbrev-list (sort abbrev-list 'string-lessp))
	(while abbrev-list
	  (if (> (+ first-column 40) (window-width))
	      (progn
		(insert "\n")
		(setq first-column 0)))
	  (indent-to first-column)
	  (insert (symbol-name (car abbrev-list)))
	  (indent-to (+ first-column 8))
	  (insert (symbol-value (car abbrev-list)))
	  (setq first-column (+ first-column 40))
	  (setq abbrev-list (cdr abbrev-list)))))))

(provide 'abbrevlist)

;; arch-tag: 178f0638-6597-4c16-bcee-576c3d8e9217
;;; abbrevlist.el ends here
