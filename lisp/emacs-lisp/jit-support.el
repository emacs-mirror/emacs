;;; jit-support.el --- helper functions for JIT compilation -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Keywords: lisp
;; Package: emacs

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

;;;###autoload
(defun jit-disassemble (func)
  (interactive "aDisassemble function: ")
  (when (symbolp func)
    (setf func (symbol-function func)))
  (let ((str (jit-disassemble-to-string func)))
    (with-current-buffer (get-buffer-create "*JIT*")
      (erase-buffer)
      (save-excursion
        (insert str))
      (pop-to-buffer (current-buffer)))))

(provide 'jit-support)

;;; jit-support.el ends here
