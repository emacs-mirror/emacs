;;; ps-mode-tests.el --- Test suite for ps-mode

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(require 'ps-mode)

(ert-deftest ps-mode-test-octal-region-unibyte ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "foo" #x90 #x91 #x92 "bar")
    (ps-mode-octal-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "foo\\220\\221\\222bar"))))

(ert-deftest ps-mode-test-octal-region-multibyte ()
  (with-temp-buffer
    (insert "foo"
            (unibyte-char-to-multibyte #x90)
            (unibyte-char-to-multibyte #x91)
            (unibyte-char-to-multibyte #x92)
            "bar")
    (ps-mode-octal-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "foo\\220\\221\\222bar"))))

(provide 'ps-mode-tests)

;;; ps-mode-tests.el ends here
