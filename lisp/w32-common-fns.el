;;; w32-common-fns.el --- Lisp routines for Windows and Cygwin-w32

;; Copyright (C) 1994, 2001-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This file contains functions that are used by both native NT Emacs
;;; and Cygwin Emacs compiled to use the native Windows widget
;;; library.

(declare-function x-server-version "w32fns.c" (&optional terminal))

(defun w32-version ()
  "Return the MS-Windows version numbers.
The value is a list of three integers: the major and minor version
numbers, and the build number."
  (x-server-version))

(defun w32-using-nt ()
  "Return non-nil if running on a Windows NT descendant.
That includes all Windows systems except for 9X/Me."
  (getenv "SystemRoot"))

(declare-function w32-get-clipboard-data "w32select.c")
(declare-function w32-set-clipboard-data "w32select.c")
(declare-function x-server-version "w32fns.c" (&optional display))

;;; Fix interface to (X-specific) mouse.el
(gui-method-define gui-own-selection w32
                   (lambda (type value)
                     (put 'x-selections (or type 'PRIMARY) data)))

(gui-method-define gui-disown-selection w32
                   (lambda (type &optional _time-object _frame)
                     (put 'x-selections (or type 'PRIMARY) nil)))

(gui-method-define gui-get-selection w32
                   (lambda (&optional type _data-type)
                     (get 'x-selections (or type 'PRIMARY))))

;; gui-selection-owner-p is used in simple.el
(gui-method-define gui-selection-owner-p w32
                   (lambda (selection)
                     (and (memq selection '(nil PRIMARY SECONDARY))
                          (get 'x-selections (or selection 'PRIMARY)))))

;; The "Windows" keys on newer keyboards bring up the Start menu
;; whether you want it or not - make Emacs ignore these keystrokes
;; rather than beep.
(global-set-key [lwindow] 'ignore)
(global-set-key [rwindow] 'ignore)

(defvar w32-charset-info-alist)		; w32font.c


;;;; Selections

(defun w32-get-selection-value ()
  "Return the value of the current selection.
Consult the selection.  Treat empty strings as if they were unset."
  (if gui-select-enable-clipboard
      ;; Don't die if x-get-selection signals an error.
      (with-demoted-errors "w32-get-clipboard-data:%S"
        (w32-get-clipboard-data))))

;; Arrange for the kill and yank functions to set and check the clipboard.
(gui-method-define gui-selection-value w32 #'w32-get-selection-value)

(provide 'w32-common-fns)
