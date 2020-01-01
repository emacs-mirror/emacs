;;; test-fmt.el --- test semantic tag formatting

;;; Copyright (C) 2012, 2019-2020 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;;
;;

;;; Code:
(require 'semantic)
;;
;; ## name "semantic"
;; ## abbreviate "semantic<>"
;; ## summarize "Requires: semantic"

(defun test-fmt-1 (a)
  "Function with 1 arg.")
;;
;; ## name "test-fmt-1"
;; ## abbreviate "(test-fmt-1)"
;; ## summarize "Defuns: (test-fmt-1 a)"
;; ## short-doc "Function with 1 arg."
;; ## uml-prototype "(test-fmt-1 a)"   <-- That is probably wrong.

(defvar test-fmt-var nil
  "Variable test.")
;;
;; ## name "test-fmt-var"
;; ## abbreviate "test-fmt-var"
;; ## summarize "Variables: test-fmt-var"
;; ## short-doc "Variable test."
;; ## uml-prototype "test-fmt-var"

(defclass test-fmt-class ()
  ((slot1 :initarg :slot1))
  "Class for testing.")
;;
;; ## name "test-fmt-class"
;; ## abbreviate "test-fmt-class{}"
;; ## summarize "Types: class test-fmt-class {}"
;; ## short-doc "Class for testing."
;; ## uml-prototype "class test-fmt-class {}"



(provide 'test-fmt)

;;; test-fmt.el ends here
