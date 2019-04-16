;;; wid-edit-tests.el --- tests for wid-edit.el -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

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

(require 'ert)
(require 'wid-edit)

(ert-deftest widget-at ()
  "Test `widget-at' behavior."
  (with-temp-buffer
    (should-not (widget-at))
    (let ((marco (widget-create 'link "link widget"))
          (polo  (widget-at (1- (point)))))
      (should (widgetp polo))
      (should (eq marco polo)))
    ;; Buttons and widgets are incompatible (bug#34506).
    (insert-text-button "text button")
    (should-not (widget-at (1- (point))))
    (insert-button "overlay button")
    (should-not (widget-at (1- (point))))))

;;; wid-edit-tests.el ends here
