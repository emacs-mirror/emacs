;;; erc-tests.el --- Tests for erc.  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
(require 'erc)

(ert-deftest erc--read-time-period ()
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should (equal (erc--read-time-period "foo: ") nil)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should (equal (erc--read-time-period "foo: ") nil)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " 432 ")))
    (should (equal (erc--read-time-period "foo: ") 432)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "432")))
    (should (equal (erc--read-time-period "foo: ") 432)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1h")))
    (should (equal (erc--read-time-period "foo: ") 3600)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1h10s")))
    (should (equal (erc--read-time-period "foo: ") 3610)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1d")))
    (should (equal (erc--read-time-period "foo: ") 86400))))
