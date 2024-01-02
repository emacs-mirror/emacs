;;; mh-limit-tests.el --- tests for mh-limit.el -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'mh-limit)

(ert-deftest mh-pick-args-list ()
  "Test `mh-pick-args-list'."
  (should (equal '() (mh-pick-args-list "")))
  (should (equal '("-subject" "a") (mh-pick-args-list "-subject a")))
  (should (equal '("-subject" "a") (mh-pick-args-list "  -subject   a  ")))
  (should (equal '("-subject" "a" "-from" "b")
                 (mh-pick-args-list "-subject a -from b")))
  (should (equal '("-subject" "a b" "-from" "c d")
                 (mh-pick-args-list "-subject a b -from c d"))))

;;; mh-limit-tests.el ends here
