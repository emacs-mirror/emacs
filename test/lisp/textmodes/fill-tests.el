;;; fill-test.el --- ERT tests for fill.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author:     Marcin Borkowski <mbork@mbork.pl>
;; Keywords:   text, wp

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

;; This package defines tests for the filling feature, specifically
;; the `fill-polish-nobreak-p' function.

;;; Code:

(require 'ert)

(ert-deftest fill-test-no-fill-polish-nobreak-p nil
  "Tests of the `fill-polish-nobreak-p' function."
  (with-temp-buffer
    (insert "Abc d efg (h ijk).")
    (setq fill-column 8)
    (setq-local fill-nobreak-predicate '())
    (fill-paragraph)
    (should (string= (buffer-string) "Abc d\nefg (h\nijk).")))
  (with-temp-buffer
    (insert "Abc d efg (h ijk).")
    (setq fill-column 8)
    (setq-local fill-nobreak-predicate '(fill-polish-nobreak-p))
    (fill-paragraph)
    (should (string= (buffer-string) "Abc\nd efg\n(h ijk)."))))


(provide 'fill-tests)

;;; fill-tests.el ends here
