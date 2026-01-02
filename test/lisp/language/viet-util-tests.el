;;; viet-util-tests.el --- unit tests for viet-util.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

;; Unit tests for lisp/language/viet-util.el.

;;; Code:

(ert-deftest viet-util-test-viqr ()
  "Test bug#80039."
  (let ((viqr-text "O^ng te^n gi`\\? To^i te^n la` Tra^`n Va(n Hie^'u\\.")
        (viet-text "Ông tên gì? Tôi tên là Trần Văn Hiếu."))
    (with-temp-buffer
      (insert viqr-text)
      (viet-decode-viqr-region (point-min) (point-max))
      (should (equal (buffer-string) viet-text))
      (viet-encode-viqr-region (point-min) (point-max))
      (should (equal (buffer-string) viqr-text)))))

;;; viet-util--tests.el ends here
