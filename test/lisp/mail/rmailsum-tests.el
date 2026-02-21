;;; rmailsum-tests.el --- tests for rmailsum.el  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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
(require 'rmailsum)

(ert-deftest rmailsum-tests-name-or-address-prefers-name ()
  (let ((from "Foo Bar <foo@example.test>"))
    (should (equal (rmail-summary-name-or-address from)
                   "Foo Bar"))))

(ert-deftest rmailsum-tests-name-or-address-fallback-to-address ()
  (let ((from "<foo@example.test>"))
    (should (equal (rmail-summary-name-or-address from)
                   "foo@example.test"))))

(ert-deftest rmailsum-tests-recipient-strip-quoted-names-first-line ()
  (let ((to "Foo Bar <foo@example.test>,\n Baz Quux <baz@example.test>"))
    (should (equal (rmail-summary-recipient-strip-quoted-names to)
                   "foo@example.test,"))))

(ert-deftest rmailsum-tests-recipient-names-folded ()
  (let ((to "Foo Bar <foo@example.test>,\n Baz Quux <baz@example.test>"))
    (should (equal (rmail-summary-recipient-names to)
                   "Foo Bar, Baz Quux"))))

(ert-deftest rmailsum-tests-recipient-names-fallback-to-address ()
  (let ((to "Foo Bar <foo@example.test>,\n <baz@example.test>"))
    (should (equal (rmail-summary-recipient-names to)
                   "Foo Bar, baz@example.test"))))

(provide 'rmailsum-tests)
;;; rmailsum-tests.el ends here
