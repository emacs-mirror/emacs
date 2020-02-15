;;; font-lock-tests.el --- Test suite for font-lock. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(ert-deftest font-lock-test-append-anonymous-face ()
  "Ensure `font-lock-append-text-property' does not splice anonymous faces."
  (with-temp-buffer
    (insert "foo")
    (add-text-properties 1 3 '(face italic))
    (font-lock-append-text-property 1 3 'face '(:strike-through t))
    (should (equal (get-text-property 1 'face (current-buffer))
                   '(italic (:strike-through t))))))

(ert-deftest font-lock-test-prepend-anonymous-face ()
  "Ensure `font-lock-prepend-text-property' does not splice anonymous faces."
  (with-temp-buffer
    (insert "foo")
    (add-text-properties 1 3 '(face italic))
    (font-lock-prepend-text-property 1 3 'face '(:strike-through t))
    (should (equal (get-text-property 1 'face (current-buffer))
                   '((:strike-through t) italic)))))

;; font-lock-tests.el ends here
