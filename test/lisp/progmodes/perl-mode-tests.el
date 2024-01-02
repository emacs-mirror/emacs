;;; perl-mode-tests.el --- Test for perl-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

(require 'perl-mode)

(ert-deftest perl-test-lock ()
  (with-temp-buffer
    (perl-mode)
    (insert "$package = foo;")
    (font-lock-ensure (point-min) (point-max))
    (should (equal (get-text-property 4 'face) 'font-lock-variable-name-face))))

;;;; Reuse cperl-mode tests

(defvar cperl-test-mode)
(setq cperl-test-mode #'perl-mode)
(load-file (expand-file-name "cperl-mode-tests.el"
                             (file-truename
                              (file-name-directory (or load-file-name
                                                       buffer-file-name)))))

(setq ert-load-file-name load-file-name)

;;; perl-mode-tests.el ends here
