;;; nxml-maint.el --- commands for maintainers of nxml-*.el  -*- lexical-binding:t -*-

;; Copyright (C) 2003, 2007-2017 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: wp, hypermedia, languages, XML

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Parsing target repertoire files from ucs-fonts.
;; This is for converting the TARGET? files in
;; http://www.cl.cam.ac.uk/~mgk25/download/ucs-fonts.tar.gz
;; into a glyph set.

(defun nxml-insert-target-repertoire-glyph-set (file var)
  (interactive "fTarget file: \nSVariable name: ")
  (let (lst head)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (while (re-search-forward "^ *\\([a-FA-F0-9]\\{2\\}\\)[ \t]+" nil t)
	(let ((row (match-string 1))
	      (eol (line-end-position)))
	  (while (re-search-forward "\\([a-FA-F0-9]\\{2\\}\\)-\\([a-FA-F0-9]\\{2\\}\\)\\|\\([a-FA-F0-9]\\{2\\}\\)" eol t)
	    (setq lst
		  (cons (if (match-beginning 3)
			    (concat "#x" row (match-string 3))
			(concat "(#x" row (match-string 1)
				" . #x" row (match-string 2) ")"))
			lst))))))
    (setq lst (nreverse lst))
    (insert (format "(defconst %s\n  [" var))
    (while lst
      (setq head (car lst))
      (setq lst (cdr lst))
      (insert head)
      (when (= (length head) 6)
	(while (and lst (= (length (car lst)) 6))
	  (insert " ")
	  (insert (car lst))
	  (setq lst (cdr lst))))
      (when lst (insert "\n   ")))
    (insert "])\n")))

(provide 'nxml-maint)

;;; nxml-maint.el ends here
