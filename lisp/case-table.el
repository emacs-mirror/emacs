;;; case-table.el --- code to extend the character set and support case tables.

;; Copyright (C) 1988 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i14n

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

(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((vector (make-vector 256 nil))
	(case-table (current-case-table))
	(i 0))
    (while (< i 256)
      (aset vector i 
	    (cond ((/= ch (downcase ch))
		   (concat "uppercase, matches "
			   (text-char-description (downcase ch))))
		  ((/= ch (upcase ch))
		   (concat "lowercase, matches "
			   (text-char-description (upcase ch))))
		  (t "case-invariant")))
      (setq i (1+ i)))
    (with-output-to-temp-buffer "*Help*"
      (describe-vector vector))))

(defun set-case-syntax-delims (l r string)
  "Make characters L and R a matching pair of non-case-converting delimiters.
Sets the entries for L and R in STRING, which is a downcasing table.
Also modifies `standard-syntax-table', and `text-mode-syntax-table' to
indicate left and right delimiters."
  (aset string l l)
  (aset string r r)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       text-mode-syntax-table)
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       text-mode-syntax-table))

(defun set-case-syntax-pair (uc lc string)
  "Make characters UC and LC a pair of inter-case-converting letters.
Sets the entries for characters UC and LC in STRING, which is a downcasing table.
Also modify `standard-syntax-table' and `text-mode-syntax-table' to indicate an
(uppercase, lowercase) pair of letters."
  (aset string uc lc)
  (aset (car (cdr (standard-case-table))) lc uc)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry lc "w   " text-mode-syntax-table)
  (modify-syntax-entry uc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " text-mode-syntax-table))

(defun set-case-syntax (c syntax string)
  "Make characters C case-invariant with syntax SYNTAX.
Sets the entries for character C in STRING, which is the downcasing table.
Also modify `standard-syntax-table' and `text-mode-syntax-table'.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (aset string c c)
  (modify-syntax-entry c syntax (standard-syntax-table))
  (modify-syntax-entry c syntax text-mode-syntax-table))

(provide 'case-table)

;;; case-table.el ends here
