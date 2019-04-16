;;; button-tests.el --- tests for button.el -*- lexical-binding: t -*-

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

(ert-deftest button-at ()
  "Test `button-at' behavior."
  (with-temp-buffer
    (should-not (button-at (point)))
    (let ((button (insert-text-button "text button"))
          (marker (button-at (1- (point)))))
      (should (markerp marker))
      (should (= (button-end button) (button-end marker) (point))))
    (let ((button  (insert-button "overlay button"))
          (overlay (button-at (1- (point)))))
      (should (overlayp overlay))
      (should (eq button overlay)))
    ;; Buttons and widgets are incompatible (bug#34506).
    (widget-create 'link "link widget")
    (should-not (button-at (1- (point))))))

;;; button-tests.el ends here
