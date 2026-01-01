;;; which-func-tests.el --- tests for which-func     -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Spencer Baugh <sbaugh@catern.com>

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'ert)
(require 'which-func)

(ert-deftest which-func-tests-toggle ()
  (let ((which-func-display 'mode-and-header) buf-code buf-not)
    (setq buf-code (find-file-noselect "which-func-tests.el"))
    (setq buf-not (get-buffer-create "fundamental"))
    (with-current-buffer buf-code
      (should-not which-func-mode) (should-not header-line-format))
    (with-current-buffer buf-not
      (should-not which-func-mode) (should-not header-line-format))
    (which-function-mode 1)
    (with-current-buffer buf-code
      (should which-func-mode) (should header-line-format))
    (with-current-buffer buf-not
      (should-not which-func-mode) (should-not header-line-format))
    (which-function-mode -1)
    ;; which-func-mode stays set even when which-function-mode is off.
    (with-current-buffer buf-code
      (should which-func-mode) (should-not header-line-format))
    (with-current-buffer buf-not
      (should-not which-func-mode) (should-not header-line-format))
    (kill-buffer buf-code)
    (kill-buffer buf-not)
    (which-function-mode 1)
    (setq buf-code (find-file-noselect "which-func-tests.el"))
    (setq buf-not (get-buffer-create "fundamental"))
    (with-current-buffer buf-code
      (should which-func-mode) (should header-line-format))
    (with-current-buffer buf-not
      (should-not which-func-mode) (should-not header-line-format))))

(provide 'which-func-tests)
;;; which-func-tests.el ends here
