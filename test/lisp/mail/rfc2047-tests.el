;;; rfc2047-tests.el --- tests for rfc2047.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(require 'rfc2047)

(defun test-rfc2047 (before after)
  (with-temp-buffer
    (insert before)
    (goto-char (point-min))
    (rfc2047-fold-field)
    (should (equal (buffer-string) after))))

(ert-deftest test-rfc2047-fold-short ()
  (test-rfc2047
   "Organization: Lots Of Short Words Here Lots Of Short Words Here Lots Of Short Words Here\n"

   "Organization: Lots Of Short Words Here Lots Of Short Words Here Lots Of
 Short Words Here
"))

(ert-deftest test-rfc2047-fold-encoded ()
  (test-rfc2047
   "Subject: This is =?utf-8?Q?=C3=A1?= long subject that's =?utf-8?Q?v=C3=A9ry?= long and =?utf-8?Q?ver=C3=BD?= encoded yes indeed it =?utf-8?Q?=C3=ADs?=\n"
   "Subject: This is =?utf-8?Q?=C3=A1?= long subject that's
 =?utf-8?Q?v=C3=A9ry?= long and =?utf-8?Q?ver=C3=BD?= encoded yes indeed it
 =?utf-8?Q?=C3=ADs?=
"))

(ert-deftest test-rfc2047-fold-long ()
  (test-rfc2047
   "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword and then\n"
   "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword
 and then
"))

(ert-deftest test-rfc2047-fold-long-short ()
  (test-rfc2047
   "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword\n"
   "Organization: verylongverylongverylongverylongverylongverylongverylongverylongverylongword\n"))

;;; rfc2047-tests.el ends here
