;;; bibtex-tests.el --- Test suite for bibtex.  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2024 Free Software Foundation, Inc.

;; Keywords: bibtex

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
(require 'bibtex)

(ert-deftest bibtex-test-set-dialect ()
  "Tests if `bibtex-set-dialect' is executed."
  (with-temp-buffer
    (insert "@article{someID,
  author = {some author},
  title = {some title},
}")
    (bibtex-mode)
    (should-not (null bibtex-dialect))
    (should-not (null bibtex-entry-type))
    (should-not (null bibtex-entry-head))
    (should-not (null bibtex-reference-key))
    (should-not (null bibtex-entry-head))
    (should-not (null bibtex-entry-maybe-empty-head))
    (should-not (null bibtex-any-valid-entry-type))))

(ert-deftest bibtex-test-parse-buffers-stealthily ()
  "Tests if `bibtex-parse-buffers-stealthily' can be executed."
  (with-temp-buffer
    (insert "@article{someID,
  author = {some author},
  title = {some title},
}")
    (bibtex-mode)
    (should (progn (bibtex-parse-buffers-stealthily) t))))

(provide 'bibtex-tests)

;;; bibtex-tests.el ends here
