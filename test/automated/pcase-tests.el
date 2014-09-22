;;; pcase-tests.el --- Test suite for pcase macro.

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest pcase-tests-behavior ()
  "Test pcase code."
  (should (equal (pcase '(1 . 2) ((app car '2) 6) ((app car '1) 5)) 5)))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pcase-tests.el ends here.
