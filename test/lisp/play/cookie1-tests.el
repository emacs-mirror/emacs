;;; cookie1-tests.el --- Tests for cookie1.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
(require 'cookie1)

(ert-deftest cookie1-tests-cookie ()
  (let ((fortune-file (ert-resource-file "cookies")))
    (should (string-match "\\`This fortune"
                          (cookie fortune-file)))))

(ert-deftest cookie1-testss-cookie-apropos ()
  (let ((fortune-file (ert-resource-file "cookies")))
    (should (string-match "\\`This fortune"
                          (car (cookie-apropos "false" fortune-file))))
    (should (= (length (cookie-apropos "false" fortune-file)) 1))))

(provide 'fortune-tests)
;;; cookie1-tests.el ends here
