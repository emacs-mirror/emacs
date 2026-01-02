;;; lua-ts-mode-tests.el --- Tests for lua-ts-mode -*- lexical-binding: t; -*-

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
(require 'hideshow)
(require 'treesit)
(require 'which-func)
(require 'elec-pair)

(ert-deftest lua-ts-test-indentation ()
  (skip-unless (treesit-ready-p 'lua t))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest lua-ts-test-movement ()
  (skip-unless (treesit-ready-p 'lua t))
  (ert-test-erts-file (ert-resource-file "movement.erts")))

(ert-deftest lua-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'lua t))
  (let ((treesit-font-lock-level 4))
    (ert-font-lock-test-file (ert-resource-file "font-lock.lua") 'lua-ts-mode)))

(ert-deftest lua-ts-test-which-function ()
  (skip-unless (treesit-ready-p 'lua t))
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "which-function.lua"))
    (lua-ts-mode)
    (which-function-mode)
    (goto-char (point-min))
    (should (equal "f" (which-function)))
    (which-function-mode -1)))

(ert-deftest lua-ts-test-hideshow ()
  (skip-unless (treesit-ready-p 'lua t))
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "hide-show.lua"))
    (lua-ts-mode)
    (hs-minor-mode)
    (hs-hide-all)
    (should (= 11 (length (overlays-in (point-min) (point-max)))))
    (hs-show-all)
    (should (= 0 (length (overlays-in (point-min) (point-max)))))
    (hs-minor-mode -1)))

;; from: electric-tests.el
(defun call-with-saved-electric-modes (fn)
  (let ((saved-electric (if electric-pair-mode 1 -1))
        (saved-layout (if electric-layout-mode 1 -1))
        (saved-indent (if electric-indent-mode 1 -1))
        (blink-paren-function nil))
    (electric-pair-mode -1)
    (electric-layout-mode -1)
    (electric-indent-mode -1)
    (unwind-protect
        (funcall fn)
      (electric-pair-mode saved-electric)
      (electric-indent-mode saved-indent)
      (electric-layout-mode saved-layout))))

;; from: electric-tests.el
(defmacro save-electric-modes (&rest body)
  (declare (indent defun) (debug t))
  `(call-with-saved-electric-modes (lambda () ,@body)))

(ert-deftest lua-ts-test-auto-close-block-comments ()
  (skip-unless (treesit-ready-p 'lua t))
  (save-electric-modes
   (with-temp-buffer
     (dlet ((lua-ts-auto-close-block-comments t))
       (electric-pair-mode 1)
       (lua-ts-mode)
       (insert "--")
       (let ((last-command-event ?\[))
         (ert-simulate-command '(self-insert-command 1))
         (ert-simulate-command '(self-insert-command 1)))
       (should (equal "--[[\n]]" (buffer-string)))))))

(provide 'lua-ts-mode-tests)

;;; lua-ts-mode-tests.el ends here
