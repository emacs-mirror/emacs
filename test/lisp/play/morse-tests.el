;;; morse-tests.el --- Tests for morse.el  -*- lexical-binding: t -*-

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
(require 'morse)

(ert-deftest morse-tests-morse-region ()
  (with-temp-buffer
    (insert "Morse encoded")
    (morse-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "--/---/.-./.../. ./-./-.-./---/-.././-.."))))

(ert-deftest morse-tests-unmorse-region ()
  (with-temp-buffer
    (insert "--/---/.-./.../. ./-./-.-./---/-.././-..")
    (unmorse-region (point-min) (point-max))
    (should (equal (buffer-string) "morse encoded"))))

(ert-deftest morse-tests-nato-region ()
  (with-temp-buffer
    (insert "Nato encoded")
    (nato-region (point-min) (point-max))
    (should (equal (buffer-string)
                   (concat
                    "November-Alfa-Tango-Oscar Echo-November"
                    "-Charlie-Oscar-Delta-Echo-Delta")))))

(ert-deftest morse-tests-unnato-region ()
  (with-temp-buffer
    (insert (concat
             "November-Alfa-Tango-Oscar Echo-November"
             "-Charlie-Oscar-Delta-Echo-Delta"))
    (denato-region (point-min) (point-max))
    (should (equal (buffer-string) "nato encoded"))))

(provide 'morse-tests)
;;; morse-tests.el ends here
