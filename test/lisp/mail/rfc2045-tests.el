;;; rfc2045-tests.el --- Tests for rfc2045.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'rfc2045)

(ert-deftest rfc2045-test-encode-string ()
  (should (equal (rfc2045-encode-string "foo" "bar") "foo=bar"))
  (should (equal (rfc2045-encode-string "foo" "bar-baz") "foo=bar-baz"))
  (should (equal (rfc2045-encode-string "foo" "bar baz") "foo=\"bar baz\""))
  (should (equal (rfc2045-encode-string "foo" "bar\tbaz") "foo=\"bar\tbaz\""))
  (should (equal (rfc2045-encode-string "foo" "bar\nbaz") "foo=\"bar\nbaz\"")))

(provide 'rfc2045-tests)
;;; rfc2045-tests.el ends here
