;;; inversion-tests.el --- Tests for inversion.el  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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

;; Moved here from test/manual/cedet/cedet-utests.el

;;; Code:

(require 'inversion)
(require 'ert)

(ert-deftest inversion-unit-test ()
  "Test inversion to make sure it can identify different version strings."
  (let ((c1 (inversion-package-version 'inversion))
        (c1i (inversion-package-incompatibility-version 'inversion))
        (c2 (inversion-decode-version  "1.3alpha2"))
        (c3 (inversion-decode-version  "1.3beta4"))
        (c4 (inversion-decode-version  "1.3 beta5"))
        (c5 (inversion-decode-version  "1.3.4"))
        (c6 (inversion-decode-version  "2.3alpha"))
        (c7 (inversion-decode-version  "1.3"))
        (c8 (inversion-decode-version  "1.3pre1"))
        (c9 (inversion-decode-version  "2.4 (patch 2)"))
        (c10 (inversion-decode-version "2.4 (patch 3)"))
        (c11 (inversion-decode-version "2.4.2.1"))
        (c12 (inversion-decode-version "2.4.2.2")))
    (should (inversion-= c1 c1))
    (should (inversion-< c1i c1))
    (should (inversion-< c2 c3))
    (should (inversion-< c3 c4))
    (should (inversion-< c4 c5))
    (should (inversion-< c5 c6))
    (should (inversion-< c2 c4))
    (should (inversion-< c2 c5))
    (should (inversion-< c2 c6))
    (should (inversion-< c3 c5))
    (should (inversion-< c3 c6))
    (should (inversion-< c7 c6))
    (should (inversion-< c4 c7))
    (should (inversion-< c2 c7))
    (should (inversion-< c8 c6))
    (should (inversion-< c8 c7))
    (should (inversion-< c4 c8))
    (should (inversion-< c2 c8))
    (should (inversion-< c9 c10))
    (should (inversion-< c10 c11))
    (should (inversion-< c11 c12))
    ;; Negatives
    (should-not (inversion-< c3 c2))
    (should-not (inversion-< c4 c3))
    (should-not (inversion-< c5 c4))
    (should-not (inversion-< c6 c5))
    (should-not (inversion-< c7 c2))
    (should-not (inversion-< c7 c8))
    (should-not (inversion-< c12 c11))
    ;; Test the tester on inversion
    (should-not (inversion-test 'inversion inversion-version))
    (should (stringp (inversion-test 'inversion "0.0.0")))
    (should (stringp (inversion-test 'inversion "1000.0")))))

;;; inversion-tests.el ends here
