;;; go-ts-mode-tests.el --- Tests for Tree-sitter-based Go mode         -*- lexical-binding: t; -*-

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
(require 'ert-x)
(require 'treesit)

;; go-ts-mode

(ert-deftest go-ts-mode-test-indentation ()
  (skip-unless (treesit-ready-p 'go))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest go-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'go))
  (let ((treesit-font-lock-level 4))
    (ert-font-lock-test-file (ert-resource-file "font-lock.go") 'go-ts-mode)))

;; go-mod-ts-mode

(ert-deftest go-mod-ts-mode-test-indentation ()
  (skip-unless (treesit-ready-p 'gomod))
  (ert-test-erts-file (ert-resource-file "indent-mod.erts")))

(ert-deftest go-mod-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'gomod))
  (let ((treesit-font-lock-level 4))
    (ert-font-lock-test-file (ert-resource-file "font-lock-package.go") 'go-mod-ts-mode)))

;; go-work-ts-mode

(ert-deftest go-work-ts-mode-test-indentation ()
  (skip-unless (treesit-ready-p 'gowork))
  (ert-test-erts-file (ert-resource-file "indent-work.erts")))

(ert-deftest go-work-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'gowork))
  (let ((treesit-font-lock-level 4))
    (ert-font-lock-test-file (ert-resource-file "font-lock-package.go") 'go-work-ts-mode)))

(provide 'go-ts-mode-tests)
;;; go-ts-mode-tests.el ends here
