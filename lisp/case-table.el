;;; case-table.el --- code to extend the character set and support case tables.

;; Copyright (C) 1988, 1994 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n

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

;; Written by:
;; TN/ETX/TX/UMG Howard Gayle        UUCP : seismo!enea!erix!howard
;; Telefonaktiebolaget L M Ericsson  Phone: +46 8 719 55 65
;; Ericsson Telecom     	     Telex: 14910 ERIC S
;; S-126 25 Stockholm                FAX  : +46 8 719 64 82
;; Sweden

;;; Code:

;;;###autoload
(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((vector (make-vector 256 nil))
	(ch 0))
    (while (< ch 256)
      (aset vector ch
	    (cond ((/= ch (downcase ch))
		   (concat "uppercase, matches "
			   (char-to-string (downcase ch))))
		  ((/= ch (upcase ch))
		   (concat "lowercase, matches "
			   (char-to-string (upcase ch))))
		  (t "case-invariant")))
      (setq ch (1+ ch)))
    (save-excursion
     (with-output-to-temp-buffer "*Help*"
       (set-buffer standard-output)
       (describe-vector vector)))))

;;;###autoload
(defun set-case-syntax-delims (l r table)
  "Make characters L and R a matching pair of non-case-converting delimiters.
This sets the entries for L and R in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to
indicate left and right delimiters."
  (aset table l l)
  (aset table r r)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       (standard-syntax-table)))

;;;###autoload
(defun set-case-syntax-pair (uc lc table)
  "Make characters UC and LC a pair of inter-case-converting letters.
This sets the entries for characters UC and LC in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to give them the syntax of
word constituents."
  (aset table uc lc)
  (aset table lc lc)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

;;;###autoload
(defun set-case-syntax (c syntax table)
  "Make characters C case-invariant with syntax SYNTAX.
This sets the entries for character C in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table'.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (aset table c c)
  (modify-syntax-entry c syntax (standard-syntax-table)))

(provide 'case-table)

;;; case-table.el ends here
