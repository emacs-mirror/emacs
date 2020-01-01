;;; studly-tests.el --- Tests for studly.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'studly)

(ert-deftest studly-tests-studlify-region ()
  (with-temp-buffer
    (insert "Studlify this string of text")
    (studlify-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "StudliFy this StrinG of tExt"))))

(ert-deftest studly-tests-studlify-word ()
  (with-temp-buffer
    (insert "normal studlified normal")
    (goto-char 8)
    (studlify-word 1)
    (should (equal (buffer-string)
                   "normal stUdlIfIed normal"))))

(ert-deftest studly-tests-nato-region ()
  (with-temp-buffer
    (insert "Studlify\n this\n buffer")
    (studlify-buffer)
    (should (equal (buffer-string)
                   "STuDlify\n This\n buffer"))))

(provide 'studly-tests)
;;; studly-tests.el ends here
