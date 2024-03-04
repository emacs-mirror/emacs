;;; lua-ts-mode-tests.el --- Tests for lua-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

(ert-deftest lua-ts-test-indentation ()
  (skip-unless (treesit-ready-p 'lua))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest lua-ts-test-movement ()
  (skip-unless (treesit-ready-p 'lua))
  (ert-test-erts-file (ert-resource-file "movement.erts")))

(ert-deftest lua-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'lua))
  (let ((treesit-font-lock-level 4))
    (ert-font-lock-test-file (ert-resource-file "font-lock.lua") 'lua-ts-mode)))

(provide 'lua-ts-mode-tests)

;;; lua-ts-mode-tests.el ends here
