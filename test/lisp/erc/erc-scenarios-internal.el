;;; erc-scenarios-internal.el --- Proxy file for erc-d tests -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

(require 'ert-x)
(eval-and-compile
  (when (and (getenv "EMACS_TEST_DIRECTORY")
             (getenv "EMACS_TEST_JUNIT_REPORT"))
    (setq ert-load-file-name (or (macroexp-file-name) buffer-file-name)))
  (let ((load-path (cons (expand-file-name "erc-d" (ert-resource-directory))
                         load-path)))
    (load "erc-d-tests" nil 'silent)))

;;; erc-scenarios-internal.el ends here
