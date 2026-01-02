;;; range-tests.el --- Tests for range.el  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'range)
(require 'ert)
(require 'ert-x)

(ert-deftest ranges ()
  (should (equal (range-compress-list '(2 3 4 5 9 11 12 13))
                 '((2 . 5) 9 (11 . 13))))
  (should (equal (range-uncompress '((2 . 5) 9 (11 . 13)))
                 '(2 3 4 5 9 11 12 13)))
  (should (equal (range-normalize '(1 . 2))
                 '((1 . 2))))
  (should (equal (range-difference '((1 . 10))
                                   '((2 . 7)))
                 '(1 (8 . 10))))
  (should (equal (range-intersection '((2 . 5) 9 (11 . 13))
                                     '((5 . 12)))
                 '(5 9 (11 . 12))))
  (should (equal (range-add-list '((2 . 5) 9 (11 . 13))
                                 '(10 11 12 15 16 17))
                 '((2 . 5) (9 . 10) (11 . 13) (15 . 17))))
  (should (equal (range-remove (copy-tree '((2 . 5) 9 (11 . 13)))
                               '((5 . 9)))
                 '((2 . 4) (11 . 13))))
  (should (range-member-p 9 '((2 . 5) 9 (11 . 13))))
  (should (range-member-p 12 '((2 . 5) 9 (11 . 13))))
  (should (equal (range-list-intersection
                  '(4 5 6 7 8 9)
                  '((2 . 5) 9 (11 . 13)))
                 '(4 5 9)))
  (should (equal (range-list-difference
                  '(4 5 6 7 8 9)
                  '((2 . 5) 9 (11 . 13)))
                 '(6 7 8)))
  (should (equal (range-length '((2 . 5) 9 (11 . 13)))
                 8))
  (should (equal (range-concat '((2 . 5) 9 (11 . 13))
                               '(6 (12 . 15)))
                 '((2 . 6) 9 (11 . 15)))))

;;; range-tests.el ends here
