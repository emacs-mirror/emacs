;;; rust-ts-mode-tests.el --- Tests for rust-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
(require 'ert-font-lock)
(require 'ert-x)
(require 'treesit)
(require 'rust-ts-mode)

(ert-deftest rust-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'rust))
  (let ((treesit-font-lock-level 4)
        (rust-ts-mode-fontify-number-suffix-as-type nil))
    (ert-font-lock-test-file (ert-resource-file "font-lock.rs")
                             'rust-ts-mode)))

(ert-deftest rust-ts-test-font-lock-number ()
  (skip-unless (treesit-ready-p 'rust))
  (let ((treesit-font-lock-level 4)
        (rust-ts-mode-fontify-number-suffix-as-type t))
    (ert-font-lock-test-file (ert-resource-file "font-lock-number.rs")
                             'rust-ts-mode)))

(ert-deftest rust-ts-test-no-parent ()
  (skip-unless (treesit-ready-p 'rust))
  (let ((treesit-font-lock-level 4)
        (rust-ts-mode-fontify-number-suffix-as-type t))
    (ert-font-lock-test-file (ert-resource-file "font-lock-no-parent.rs")
                             'rust-ts-mode)))

(provide 'rust-ts-mode-tests)

;;; rust-ts-mode-tests.el ends here
