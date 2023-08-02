;;; align-tests.el --- Test suite for aligns  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023 Free Software Foundation, Inc.

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

(ert-deftest align-toml ()
  (let ((indent-tabs-mode nil))
    (ert-test-erts-file (ert-resource-file "conf-toml-mode.erts")
                        (test-align-transform-fun #'conf-toml-mode))))

(provide 'align-tests)

;;; align-tests.el ends here
