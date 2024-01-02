;;; format-spec-tests.el --- tests for format-spec.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

(ert-deftest format-spec-make ()
  "Test `format-spec-make'."
  (should-not (format-spec-make))
  (should-error (format-spec-make ?b))
  (should (equal (format-spec-make ?b "b") '((?b . "b"))))
  (should-error (format-spec-make ?b "b" ?a))
  (should (equal (format-spec-make ?b "b" ?a 'a)
                 '((?b . "b")
                   (?a . a)))))

(ert-deftest format-spec-parse-flags ()
  "Test `format-spec--parse-flags'."
  (should-not (format-spec--parse-flags nil))
  (should-not (format-spec--parse-flags ""))
  (should (equal (format-spec--parse-flags "-") '(:pad-right)))
  (should (equal (format-spec--parse-flags " 0") '(:pad-zero)))
  (should (equal (format-spec--parse-flags " -x0y< >^_z ")
                 '(:pad-right :pad-zero :chop-left :chop-right
                              :upcase :downcase))))

(ert-deftest format-spec-do-flags ()
  "Test `format-spec--do-flags'."
  (should (equal (format-spec--do-flags "" () nil nil) ""))
  (dolist (flag '(:pad-zero :pad-right :upcase :downcase
                            :chop-left :chop-right))
    (should (equal (format-spec--do-flags "" (list flag) nil nil) "")))
  (should (equal (format-spec--do-flags "FOOBAR" '(:downcase :chop-right) 5 2)
                 "   fo"))
  (should (equal (format-spec--do-flags
                  "foobar" '(:pad-zero :pad-right :upcase :chop-left) 5 2)
                 "AR000")))

(ert-deftest format-spec-do-flags-truncate ()
  "Test `format-spec--do-flags' truncation."
  (let ((flags nil))
    (should (equal (format-spec--do-flags "" flags nil 0) ""))
    (should (equal (format-spec--do-flags "" flags nil 1) ""))
    (should (equal (format-spec--do-flags "a" flags nil 0) ""))
    (should (equal (format-spec--do-flags "a" flags nil 1) "a"))
    (should (equal (format-spec--do-flags "a" flags nil 2) "a"))
    (should (equal (format-spec--do-flags "asd" flags nil 0) ""))
    (should (equal (format-spec--do-flags "asd" flags nil 1) "a")))
  (let ((flags '(:chop-left)))
    (should (equal (format-spec--do-flags "" flags nil 0) ""))
    (should (equal (format-spec--do-flags "" flags nil 1) ""))
    (should (equal (format-spec--do-flags "a" flags nil 0) ""))
    (should (equal (format-spec--do-flags "a" flags nil 1) "a"))
    (should (equal (format-spec--do-flags "a" flags nil 2) "a"))
    (should (equal (format-spec--do-flags "asd" flags nil 0) ""))
    (should (equal (format-spec--do-flags "asd" flags nil 1) "d"))))

(ert-deftest format-spec-do-flags-pad ()
  "Test `format-spec--do-flags' padding."
  (let ((flags nil))
    (should (equal (format-spec--do-flags "" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "" flags 1 nil) " "))
    (should (equal (format-spec--do-flags "a" flags 0 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 1 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 2 nil) " a")))
  (let ((flags '(:pad-zero)))
    (should (equal (format-spec--do-flags "" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "" flags 1 nil) "0"))
    (should (equal (format-spec--do-flags "a" flags 0 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 1 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 2 nil) "0a")))
  (let ((flags '(:pad-right)))
    (should (equal (format-spec--do-flags "" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "" flags 1 nil) " "))
    (should (equal (format-spec--do-flags "a" flags 0 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 1 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 2 nil) "a ")))
  (let ((flags '(:pad-right :pad-zero)))
    (should (equal (format-spec--do-flags "" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "" flags 1 nil) "0"))
    (should (equal (format-spec--do-flags "a" flags 0 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 1 nil) "a"))
    (should (equal (format-spec--do-flags "a" flags 2 nil) "a0"))))

(ert-deftest format-spec-do-flags-chop ()
  "Test `format-spec--do-flags' chopping."
  (let ((flags '(:chop-left)))
    (should (equal (format-spec--do-flags "a" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "a" flags 1 nil) "a"))
    (should (equal (format-spec--do-flags "asd" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "asd" flags 1 nil) "d")))
  (let ((flags '(:chop-right)))
    (should (equal (format-spec--do-flags "a" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "a" flags 1 nil) "a"))
    (should (equal (format-spec--do-flags "asd" flags 0 nil) ""))
    (should (equal (format-spec--do-flags "asd" flags 1 nil) "a"))))

(ert-deftest format-spec-do-flags-case ()
  "Test `format-spec--do-flags' case fiddling."
  (dolist (flag '(:pad-zero :pad-right :chop-left :chop-right))
    (let ((flags (list flag)))
      (should (equal (format-spec--do-flags "a" flags nil nil) "a"))
      (should (equal (format-spec--do-flags "A" flags nil nil) "A")))
    (let ((flags (list flag :downcase)))
      (should (equal (format-spec--do-flags "a" flags nil nil) "a"))
      (should (equal (format-spec--do-flags "A" flags nil nil) "a")))
    (let ((flags (list flag :upcase)))
      (should (equal (format-spec--do-flags "a" flags nil nil) "A"))
      (should (equal (format-spec--do-flags "A" flags nil nil) "A")))))

(ert-deftest format-spec ()
  (should (equal (format-spec "" ()) ""))
  (should (equal (format-spec "a" ()) "a"))
  (should (equal (format-spec "b" '((?b . "bar"))) "b"))
  (should (equal (format-spec "%%%b%%b%b%%" '((?b . "bar"))) "%bar%bbar%"))
  (should (equal (format-spec "foo %b zot" `((?b . "bar")))
                 "foo bar zot"))
  (should (equal (format-spec "foo %-10b zot" '((?b . "bar")))
                 "foo bar        zot"))
  (should (equal (format-spec "foo %10b zot" '((?b . "bar")))
                 "foo        bar zot"))
  (should (equal-including-properties
           (format-spec (propertize "a" 'a 'b) '((?a . "foo")))
           #("a" 0 1 (a b))))
  (let ((fmt (concat (propertize "%a" 'a 'b)
                     (propertize "%%" 'c 'd)
                     "%b"
                     (propertize "%b" 'e 'f))))
    (should (equal-including-properties
             (format-spec fmt '((?b . "asd") (?a . "fgh")))
             #("fgh%asdasd" 0 3 (a b) 3 4 (c d) 7 10 (e f))))))

(ert-deftest format-spec/function ()
  (let* (called
         (spec `((?a . "foo")
                 (?f . ,(lambda ()
                          (setq called t)
                          "bar")))))
    (should (equal (format-spec "%a" spec) "foo"))
    (should-not called)
    (should (equal (format-spec "%f" spec) "bar"))
    (should called)))

(ert-deftest format-spec-unknown ()
  (should-error (format-spec "foo %b %z zot" '((?b . "bar"))))
  (should-error (format-spec "foo %b %%%z zot" '((?b . "bar"))))
  (should (equal (format-spec "foo %b %z zot" '((?b . "bar")) t)
                 "foo bar %z zot"))
  (should (equal (format-spec "foo %4b %%%4z %%4 zot" '((?b . "bar")) t)
                 "foo  bar %%%4z %%4 zot"))
  (should (equal (format-spec "foo %4b %%%4z %%4 zot" '((?b . "bar")) 'ignore)
                 "foo  bar %%4z %4 zot"))
  (should (equal (format-spec "foo %4b %%%4z %%4 zot" '((?b . "bar")) 'delete)
                 "foo  bar % %4 zot")))

(ert-deftest format-spec-flags ()
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

(ert-deftest format-spec-split ()
  (should (equal (format-spec "foo %b bar" '((?b . "zot")) nil t)
                 '("foo " "zot" " bar")))
  (should (equal (format-spec "%b bar" '((?b . "zot")) nil t)
                 '("zot" " bar")))
  (should (equal (format-spec "%b" '((?b . "zot")) nil t)
                 '("zot")))
  (should (equal (format-spec "foo %b" '((?b . "zot")) nil t)
                 '("foo " "zot"))))

;;; format-spec-tests.el ends here
