;;; regexp-tests.el --- Test suite for regular expression handling.

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:       internal
;; Human-Keywords: internal

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

(require 'regexp-opt)

(ert-deftest regexp-test-regexp-opt ()
  "Test the `compilation-error-regexp-alist' regexps.
The test data is in `compile-tests--test-regexps-data'."
  (should (string-match (regexp-opt-charset '(?^)) "a^b")))

;;; regexp-tests.el ends here.
