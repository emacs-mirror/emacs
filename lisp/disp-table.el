;;; disp-table.el --- functions for dealing with char tables.

;; Copyright (C) 1987 Free Software Foundation, Inc.

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

;;; Code:

(defun describe-display-table (dt)
  "Describe the display table DT in a help buffer."
  (with-output-to-temp-buffer "*Help*"
    (princ "\nTruncation glyph: ")
    (prin1 (aref dt 256))
    (princ "\nWrap glyph: ")
    (prin1 (aref dt 257))
    (princ "\nEscape glyph: ")
    (prin1 (aref dt 258))
    (princ "\nCtrl glyph: ")
    (prin1 (aref dt 259))
    (princ "\nSelective display glyph sequence: ")
    (prin1 (aref dt 260))
    (princ "\nCharacter display glyph sequences:\n")
    (save-excursion
      (set-buffer standard-output)
      (let ((vector (make-vector 256 nil))
	    (i 0))
	(while (< i 256)
	  (aset vector i (aref dt i))
	  (setq i (1+ i)))
	(describe-vector vector)))
    (print-help-return-message)))

;;;###autoload
(defun describe-current-display-table ()
  "Describe the display table in use in the selected window and buffer."
  (interactive)
  (let ((disptab
	 (or (window-display-table (selected-window))
	     buffer-display-table
	     standard-display-table)))
    (if disptab
	(describe-display-table disptab)
      (message "No display table"))))

;;;###autoload
(defun make-display-table ()
  "Return a new, empty display table."
  (make-vector 261 nil))

;;;###autoload
(defun standard-display-8bit (l h)
  "Display characters in the range L to H literally."
  (while (<= l h)
    (if (and (>= l ?\ ) (< l 127))
	(if standard-display-table (aset standard-display-table l nil))
      (or standard-display-table
	  (setq standard-display-table (make-vector 261 nil)))
      (aset standard-display-table l (vector l)))
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-default (l h)
  "Display characters in the range L to H using the default notation."
  (while (<= l h)
    (if (and (>= l ?\ ) (< l 127))
	(if standard-display-table (aset standard-display-table l nil))
      (or standard-display-table
	  (setq standard-display-table (make-vector 261 nil)))
      (aset standard-display-table l nil))
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-ascii (c s)
  "Display character C using string S."
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table c (apply 'vector (append s nil))))

;;;###autoload
(defun standard-display-g1 (c sc)
  "Display character C as character SC in the g1 character set."
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table c
	(vector (create-glyph (concat "\016" (char-to-string sc) "\017")))))

;;;###autoload
(defun standard-display-graphic (c gc)
  "Display character C as character GC in graphics character set."
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table c
	(vector (create-glyph (concat "\e(0" (char-to-string gc) "\e(B")))))

;;;###autoload
(defun standard-display-underline (c uc)
  "Display character C as character UC plus underlining."
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table c
	(vector (create-glyph (concat "\e[4m" (char-to-string uc) "\e[m")))))

;; Allocate a glyph code to display by sending STRING to the terminal.
;;;###autoload
(defun create-glyph (string)
  (if (= (length glyph-table) 65536)
      (error "No free glyph codes remain"))
  (setq glyph-table (vconcat glyph-table (list string)))
  (1- (length glyph-table)))

;;;###autoload
(defun standard-display-european (arg)
  "Toggle display of European characters encoded with ISO 8859.
When enabled, characters in the range of 160 to 255 display not
as octal escapes, but as accented characters.
With prefix argument, enable European character display iff arg is positive."
  (interactive "P")
  (if (or (< (prefix-numeric-value arg) 0)
	  (and (null arg)
	       (vectorp standard-display-table)
	       (>= (length standard-display-table) 161)
	       (equal (aref standard-display-table 160) [160])))
      (standard-display-default 160 255)
    (standard-display-8bit 160 255)))

(provide 'disp-table)

;;; disp-table.el ends here
