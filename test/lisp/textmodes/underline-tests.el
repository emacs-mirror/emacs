;;; underline-tests.el --- Tests for underline.el  -*- lexical-binding: t; -*-

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
(require 'underline)

(ert-deftest underline-tests-underline-region ()
  (with-temp-buffer
    (insert "foo bar baz")
    (underline-region 5 8)
    (should (equal (buffer-string) "foo _\C-hb_\C-ha_\C-hr baz"))))

(ert-deftest underline-tests-ununderline-region ()
  (with-temp-buffer
    (insert "foo _\C-hb_\C-ha_\C-hr baz")
    (ununderline-region 5 13)
    (should (equal (buffer-string) "foo bar baz"))))

(provide 'underline-tests)
;;; underline-tests.el ends here
