;;; shorthands.el --- Read code considering Elisp shorthands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: lisp

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

;; Basic helpers for loading files with Shorthands.

;;; Code:
(require 'files)

(defun hack-elisp-shorthands (fullname)
  "Return value of `elisp-shorthands' file-local variable in FULLNAME.
FULLNAME is the absolute file name of an Elisp .el file which
potentially specifies a file-local value for `elisp-shorthands'.
The Elisp code in FULLNAME isn't read or evaluated in any way,
except for extraction of the buffer-local value of
`elisp-shorthands'."
  (let* ((size (nth 7 (file-attributes fullname)))
         (from (max 0 (- size 3000)))
         (to size))
    (with-temp-buffer
      (while (and (< (buffer-size) 3000) (>= from 0))
        (insert-file-contents fullname nil from to)
        (setq to from from (- from 100)))
      ;; FIXME: relies on the `hack-local-variables--find-variables'
      ;; detail of files.el.  That function should be exported,
      ;; possibly be refactored into two parts, since we're only
      ;; interested in basic "Local Variables" parsing.
      (alist-get 'elisp-shorthands (hack-local-variables--find-variables)))))

(defun load-with-shorthands-and-code-conversion (fullname file noerror nomessage)
  "Like `load-with-code-conversion', but also consider Elisp shorthands.
This function uses shorthands defined in the file FULLNAME's local
value of `elisp-shorthands', when it processes that file's Elisp code."
  (let ((elisp-shorthands (hack-elisp-shorthands fullname)))
    (load-with-code-conversion fullname file noerror nomessage)))

;;; shorthands.el ends here
