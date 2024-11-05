;;; yank-media-tests.el --- Tests for yank-media.el  -*- lexical-binding: t; -*-

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

;;; Commentary:

;;

;;; Code:

(require 'yank-media)
(require 'ert)
(require 'ert-x)

(ert-deftest test-utf-16 ()
  (should-not (yank-media--utf-16-p "f"))
  (should-not (yank-media--utf-16-p "fo"))
  (should-not (yank-media--utf-16-p "\000ofo"))
  (should (eq (yank-media--utf-16-p "\000o\000o") 'utf-16-be))
  (should (eq (yank-media--utf-16-p "o\000o\000") 'utf-16-le))
  (should-not (yank-media--utf-16-p "o\000\000o")))

;;; yank-media-tests.el ends here
