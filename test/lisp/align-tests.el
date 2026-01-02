;;; align-tests.el --- Test suite for aligns  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'align)

;;;; align

(defun test-align-transform-fun (function)
  (lambda ()
    (funcall function)
    (align (point-min) (point-max))))

(ert-deftest align-c ()
  (ert-test-erts-file (ert-resource-file "c-mode.erts")
                      (test-align-transform-fun #'c-mode)))

(ert-deftest align-css ()
  (let ((indent-tabs-mode nil))
    (ert-test-erts-file (ert-resource-file "css-mode.erts")
                        (test-align-transform-fun #'css-mode))))

(ert-deftest align-java ()
  (ert-test-erts-file (ert-resource-file "java-mode.erts")
                      (test-align-transform-fun #'java-mode)))

(ert-deftest align-latex ()
  (ert-test-erts-file (ert-resource-file "latex-mode.erts")
                      (test-align-transform-fun #'latex-mode)))

(autoload 'treesit-ready-p "treesit")

(ert-deftest align-ts-lua ()
  (skip-unless (treesit-ready-p 'lua t))
  (let ((comment-column 20)
        (indent-tabs-mode nil))
    (ert-test-erts-file (ert-resource-file "lua-ts-mode.erts")
                        (test-align-transform-fun #'lua-ts-mode))))

(ert-deftest align-python ()
  (ert-test-erts-file (ert-resource-file "python-mode.erts")
                      (test-align-transform-fun #'python-mode)))

(ert-deftest align-toml ()
  (let ((indent-tabs-mode nil))
    (ert-test-erts-file (ert-resource-file "conf-toml-mode.erts")
                        (test-align-transform-fun #'conf-toml-mode))))

;;;; align-regexp

(ert-deftest align-regexp ()
  (let ((indent-tabs-mode nil))
    (ert-test-erts-file (ert-resource-file "align-regexp.erts")
                        (lambda ()
                          (align-regexp (point-min) (point-max)
                                        "\\(\\s-*\\)(")))))

(provide 'align-tests)

;;; align-tests.el ends here
