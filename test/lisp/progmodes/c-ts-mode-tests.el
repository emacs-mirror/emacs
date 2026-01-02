;;; c-ts-mode-tests.el --- Tests for Tree-sitter-based C mode         -*- lexical-binding: t; -*-

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

(ert-deftest c-ts-mode-test-indentation ()
  (skip-unless (treesit-ready-p 'c))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest c-ts-mode-test-indentation-preproc ()
  (skip-unless (treesit-ready-p 'c))
  (ert-test-erts-file (ert-resource-file "indent-preproc.erts")))

(ert-deftest c-ts-mode-test-indentation-bsd ()
  (skip-unless (treesit-ready-p 'c))
  (ert-test-erts-file (ert-resource-file "indent-bsd.erts")))

(ert-deftest c-ts-mode-test-filling ()
  (skip-unless (treesit-ready-p 'c))
  (ert-test-erts-file (ert-resource-file "filling.erts")))

(provide 'c-ts-mode-tests)
;;; c-ts-mode-tests.el ends here
