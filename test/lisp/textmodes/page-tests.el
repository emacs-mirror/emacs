;;; page-tests.el --- Tests for page.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

(require 'ert)
(require 'page)

(ert-deftest page-tests-forward-page ()
  (with-temp-buffer
    (insert "foo\n\nbar\n\nbaz")
    (forward-page -1)
    (should (looking-at-p "\nbaz"))
    (forward-page -2)
    (should (= (point) (point-min)))
    (forward-page 1)
    (should (looking-at-p "\nbar"))
    (forward-page)
    (should (looking-at-p "\nbaz"))
    (forward-page 1)
    (should (= (point) (point-max)))))

(ert-deftest page-tests-backward-page ()
  (with-temp-buffer
    (insert "foo\n\nbar\n\nbaz")
    (backward-page 1)
    (should (looking-at-p "\nbaz"))
    (backward-page)
    (should (looking-at-p "\nbar"))
    (backward-page 1)
    (should (= (point) (point-min)))
    (backward-page -1)
    (should (looking-at-p "\nbar"))
    (backward-page -2)
    (should (= (point) (point-max)))))

(defun page-tests--region-string ()
  "Return the contents of the region as a string."
  (buffer-substring (region-beginning) (region-end)))

(ert-deftest page-tests-mark-page ()
  (with-temp-buffer
    (insert "foo\n\nbar\n\nbaz")
    (mark-page)
    (should (equal (page-tests--region-string) "\nbaz"))
    (mark-page -2)
    (should (equal (page-tests--region-string) "foo\n"))
    (mark-page 1)
    (should (equal (page-tests--region-string) "\nbar\n"))))

(ert-deftest page-tests-narrow-to-page ()
  (with-temp-buffer
    (insert "foo\n\nbar\n\nbaz")
    (goto-char (point-min))
    (narrow-to-page)
    (should (equal (buffer-string) "foo\n"))
    (narrow-to-page 2)
    (should (equal (buffer-string) "baz"))
    (narrow-to-page -1)
    (should (equal (buffer-string) "bar\n"))))

(ert-deftest page-tests-count-lines-page ()
  (with-temp-buffer
    (insert "foo\n\nbar\n\nbaz")
    (goto-char (point-min))
    (should (equal (page--count-lines-page) '(1 0 1)))
    (goto-char (point-max))
    (should (equal (page--count-lines-page) '(2 2 0)))))

(ert-deftest page-tests-what-page ()
  (with-temp-buffer
    (insert "foo\n\nbar\n\nbaz")
    (goto-char (point-min))
    (should (equal (page--what-page) '(1 1)))
    (forward-page)
    (should (equal (page--what-page) '(2 2)))
    (forward-page)
    (should (equal (page--what-page) '(3 4)))))

;;; page-tests.el ends here
