;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

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

;;; Code:

(require 'disass)
(eval-when-compile (require 'cl-lib))

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defun comp-recuparate-lap (fun)
  "Compile FUN if necessary and recuparate its LAP rapresentation."
  (byte-compile-close-variables
   (byte-compile-top-level (byte-compile-preprocess fun))
   byte-compile-lap-output))

(defun comp-compute-blocks (obj)
  "Split OBJ in basic blocks."
  obj)

(defun native-compile (fun)
  "FUN is the function definition to be compiled to native code."
  (if-let ((f (symbol-function fun)))
      (comp-recuparate-lap f)
    (error "Trying to native compile not a function")))

(provide 'comp)

;;; comp.el ends here
