;;; rot13.el --- display a buffer in ROT13  -*- lexical-binding: t -*-

;; Copyright (C) 1988, 2001-2023 Free Software Foundation, Inc.

;; Author: Howard Gayle
;;         Simon Josefsson
;; Maintainer: emacs-devel@gnu.org

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

;;   "ROT13 ('rotate by 13 places') is a simple letter substitution
;;   cipher that replaces a letter with the 13th letter after it in
;;   the alphabet.  ROT13 is a special case of the Caesar cipher
;;   which was developed in ancient Rome.
;;
;;   Because there are 26 letters (2×13) in the basic Latin
;;   alphabet, ROT13 is its own inverse; that is, to undo ROT13, the
;;   same algorithm is applied, so the same action can be used for
;;   encoding and decoding.  The algorithm provides virtually no
;;   cryptographic security, and is often cited as a canonical
;;   example of weak encryption.
;;
;;   ROT13 is used in online forums as a means of hiding spoilers,
;;   punchlines, puzzle solutions, and offensive materials from the
;;   casual glance."                      - Wikipedia article on ROT13
;;
;; The entry points, `rot13', `rot13-string', and `rot13-region' performs ROT13
;; encoding/decoding on buffers and strings.  The entry point
;; `rot13-other-window' performs a ROT13 encoding/decoding on the current
;; buffer and displays the result in another window.

;;; Code:

(defconst rot13-display-table
  (let ((table (make-display-table)))
    (dotimes (i 26)
      (aset table (+ i ?a) (vector (+ (% (+ i 13) 26) ?a)))
      (aset table (+ i ?A) (vector (+ (% (+ i 13) 26) ?A))))
    table)
  "Char table for ROT13 display.")

(put 'plain-char-table 'char-table-extra-slots 0)

(defconst rot13-translate-table
  (let ((table (make-char-table 'translation-table)))
    (dotimes (i 26)
      (aset table (+ i ?a) (+ (% (+ i 13) 26) ?a))
      (aset table (+ i ?A) (+ (% (+ i 13) 26) ?A)))
    table)
  "Char table for ROT13 translation.")

;;;###autoload
(defun rot13 (object &optional start end)
  "ROT13 encrypt OBJECT, a buffer or string.
If OBJECT is a buffer, encrypt the region between START and END.
If OBJECT is a string, encrypt it in its entirety, ignoring START
and END, and return the encrypted string."
  (if (bufferp object)
      (with-current-buffer object
	(rot13-region start end))
    (rot13-string object)))

;;;###autoload
(defun rot13-string (string)
  "Return ROT13 encryption of STRING."
  (with-temp-buffer
    (insert string)
    (rot13-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun rot13-region (start end)
  "ROT13 encrypt the region between START and END in current buffer.
If invoked interactively and the buffer is read-only, a message
will be printed instead."
  (interactive "r")
  (condition-case nil
      (translate-region start end rot13-translate-table)
    (buffer-read-only
     (when (called-interactively-p 'interactive)
       (let ((dec (rot13-string (buffer-substring start end))))
         (message "Buffer is read-only:\n%s" (string-trim dec)))))))

;;;###autoload
(defun rot13-other-window ()
  "Display current buffer in ROT13 in another window.
The text itself is not modified, only the way it is displayed is affected.

To terminate the ROT13 display, delete that window.  As long as that window
is not deleted, any buffer displayed in it will become instantly encoded
in ROT13.

See also `toggle-rot13-mode'."
  (interactive)
  (let ((w (display-buffer (current-buffer) t)))
    (set-window-display-table w rot13-display-table)))

;;;###autoload
(defun toggle-rot13-mode ()
  "Toggle the use of ROT13 encoding for the current window."
  (interactive)
  (if (eq (window-display-table) rot13-display-table)
      (set-window-display-table (selected-window) nil)
    (if (null (window-display-table))
	(set-window-display-table (selected-window) rot13-display-table))))

(provide 'rot13)

;;; rot13.el ends here
