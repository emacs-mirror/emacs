;;; footnote-tests.el --- Tests for footnote-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'footnote)

(ert-deftest footnote-tests-same-place ()
  (with-temp-buffer
    (footnote-mode 1)
    (insert "hello world")
    (beginning-of-line) (forward-word)
    (footnote-add-footnote)
    (insert "footnote")
    (footnote-back-to-message)
    (should (equal (buffer-substring (point-min) (point))
                   "hello[1]"))
    (beginning-of-line) (forward-word)
    (footnote-add-footnote)
    (insert "other footnote")
    (footnote-back-to-message)
    (should (equal (buffer-substring (point-min) (point))
                   "hello[1]"))
    (should (equal (buffer-substring (point-min) (line-end-position))
                   "hello[1][2] world"))))

(provide 'footnote-tests)
;;; footnote-tests.el ends here
