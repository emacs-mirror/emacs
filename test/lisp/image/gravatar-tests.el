;;; gravatar-tests.el --- tests for gravatar.el -*- lexical-binding: t -*-

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
(require 'gravatar)

(ert-deftest gravatar-hash ()
  "Test `gravatar-hash'."
  (should (equal (gravatar-hash "") "d41d8cd98f00b204e9800998ecf8427e"))
  (let ((hash "acbd18db4cc2f85cedef654fccc4a4d8"))
    (should (equal (gravatar-hash "foo") hash))
    (should (equal (gravatar-hash "foo ") hash))
    (should (equal (gravatar-hash " foo") hash))
    (should (equal (gravatar-hash " foo ") hash))))

(ert-deftest gravatar-size ()
  "Test query strings for `gravatar-size'."
  (let ((gravatar-default-image nil)
        (gravatar-force-default nil))
    (let ((gravatar-size 2048))
      (should (equal (gravatar--query-string) "r=g&s=2048")))
    (let ((gravatar-size nil))
      (should (equal (gravatar--query-string) "r=g")))))

(ert-deftest gravatar-default-image ()
  "Test query strings for `gravatar-default-image'."
  (let ((gravatar-force-default nil)
        (gravatar-size nil))
    (let ((gravatar-default-image nil))
      (should (equal (gravatar--query-string) "r=g")))
    (let ((gravatar-default-image "404"))
      (should (equal (gravatar--query-string) "r=g&d=404")))
    (let ((gravatar-default-image "https://foo/bar.png"))
      (should (equal (gravatar--query-string)
                     "r=g&d=https%3A%2F%2Ffoo%2Fbar.png")))))

(ert-deftest gravatar-force-default ()
  "Test query strings for `gravatar-force-default'."
  (let ((gravatar-default-image nil)
        (gravatar-size nil))
    (let ((gravatar-force-default nil))
      (should (equal (gravatar--query-string) "r=g")))
    (let ((gravatar-force-default t))
      (should (equal (gravatar--query-string) "r=g&f=y")))))

(ert-deftest gravatar-build-url ()
  "Test `gravatar-build-url'."
  (let ((gravatar-default-image nil)
        (gravatar-force-default nil)
        (gravatar-size nil))
    (should (equal (gravatar-build-url "foo") "\
https://www.gravatar.com/avatar/acbd18db4cc2f85cedef654fccc4a4d8?r=g"))))

;;; gravatar-tests.el ends here
