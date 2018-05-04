;;; ede-tests.el --- Some tests for the Emacs Development Environment

;; Copyright (C) 2008-2018 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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

;; Extracted from ede-locate.el in the CEDET distribution.

;;; Code:

;;; From ede-locate:

(require 'ede/locate)

;;; TESTS
;;
;; Some testing routines.
(defun ede-locate-test-locate (file)
  "Test EDE Locate on FILE using LOCATE type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-locate
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-global (file)
  "Test EDE Locate on FILE using GNU Global type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-global
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-idutils (file)
  "Test EDE Locate on FILE using ID Utils type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-idutils
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-cscope (file)
  "Test EDE Locate on FILE using CScope type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-cscope
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

;;; ede-test.el ends here
