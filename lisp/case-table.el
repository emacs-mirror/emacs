;;; case-table.el --- code to extend the character set and support case tables

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar set-case-syntax-set-multibyte nil)

(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((description (make-char-table 'case-table)))
    (map-char-table
     (function (lambda (key value)
		 (aset
		  description key
		  (cond ((not (natnump value))
			 "case-invariant")
			((/= key (downcase key))
			 (concat "uppercase, matches "
				 (char-to-string (downcase key))))
			((/= key (upcase key))
			 (concat "lowercase, matches "
				 (char-to-string (upcase key))))
			(t "case-invariant")))))
     (current-case-table))
    (save-excursion
     (with-output-to-temp-buffer "*Help*"
       (set-buffer standard-output)
       (describe-vector description)
       (help-mode)))))

(defun copy-case-table (case-table)
  (let ((copy (copy-sequence case-table)))
    ;; Clear out the extra slots so that they will be
    ;; recomputed from the main (downcase) table.
    (set-char-table-extra-slot copy 0 nil)
    (set-char-table-extra-slot copy 1 nil)
    (set-char-table-extra-slot copy 2 nil)
    copy))

(defun set-case-syntax-delims (l r table)
  "Make characters L and R a matching pair of non-case-converting delimiters.
This sets the entries for L and R in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to
indicate left and right delimiters."
  (aset table l l)
  (aset table r r)
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table.
  (set-char-table-extra-slot table 0 nil)
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       (standard-syntax-table)))

(defun set-case-syntax-pair (uc lc table)
  "Make characters UC and LC a pair of inter-case-converting letters.
This sets the entries for characters UC and LC in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to give them the syntax of
word constituents."
  (let ((lu (length (string-as-unibyte (string uc))))
	(ll (length (string-as-unibyte (string lc)))))
    (unless (= lu ll)
      (error "Can't casify chars with different `charset-bytes' values")))
  (aset table uc lc)
  (aset table lc lc)
  (set-char-table-extra-slot table 0 nil)
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " (standard-syntax-table)))

(defun set-case-syntax (c syntax table)
  "Make character C case-invariant with syntax SYNTAX.
This sets the entry for character C in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table'.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (aset table c c)
  (set-char-table-extra-slot table 0 nil)
  (set-char-table-extra-slot table 1 nil)
  (set-char-table-extra-slot table 2 nil)
  (modify-syntax-entry c syntax (standard-syntax-table)))

(provide 'case-table)

;;; case-table.el ends here
