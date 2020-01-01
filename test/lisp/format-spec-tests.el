;;; format-spec-tests.el --- tests for format-spec.el -*- lexical-binding: t -*-

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
(require 'format-spec)

(ert-deftest test-format-spec ()
  (should (equal (format-spec "foo %b zot" `((?b . "bar")))
                 "foo bar zot"))
  (should (equal (format-spec "foo %-10b zot" '((?b . "bar")))
                 "foo bar        zot"))
  (should (equal (format-spec "foo %10b zot" '((?b . "bar")))
                 "foo        bar zot")))

(ert-deftest test-format-unknown ()
  (should-error (format-spec "foo %b %z zot" '((?b . "bar"))))
  (should (equal (format-spec "foo %b %z zot" '((?b . "bar")) t)
                 "foo bar %z zot"))
  (should (equal (format-spec "foo %b %z %% zot" '((?b . "bar")) t)
                 "foo bar %z %% zot")))

(ert-deftest test-format-modifiers ()
  (should (equal (format-spec "foo %10b zot" '((?b . "bar")))
                 "foo        bar zot"))
  (should (equal (format-spec "foo % 10b zot" '((?b . "bar")))
                 "foo        bar zot"))
  (should (equal (format-spec "foo %-010b zot" '((?b . "bar")))
                 "foo bar0000000 zot"))
  (should (equal (format-spec "foo %0-10b zot" '((?b . "bar")))
                 "foo bar0000000 zot"))
  (should (equal (format-spec "foo %^10b zot" '((?b . "bar")))
                 "foo        BAR zot"))
  (should (equal (format-spec "foo %_10b zot" '((?b . "BAR")))
                 "foo        bar zot"))
  (should (equal (format-spec "foo %<4b zot" '((?b . "longbar")))
                 "foo gbar zot"))
  (should (equal (format-spec "foo %>4b zot" '((?b . "longbar")))
                 "foo long zot")))

;;; format-spec-tests.el ends here
