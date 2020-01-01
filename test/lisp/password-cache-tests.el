;;; password-cache-tests.el --- Tests for password-cache.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(require 'password-cache)

(ert-deftest password-cache-tests-add-and-remove ()
  (let ((password-data (copy-hash-table password-data)))
    (password-cache-add "foo" "bar")
    (should (eq (password-in-cache-p "foo") t))
    (password-cache-remove "foo")
    (should (not (password-in-cache-p "foo")))))

(ert-deftest password-cache-tests-read-from-cache ()
  (let ((password-data (copy-hash-table password-data)))
    (password-cache-add "foo" "bar")
    (should (equal (password-read-from-cache "foo") "bar"))
    (should (not (password-read-from-cache nil)))))

(ert-deftest password-cache-tests-in-cache-p ()
  (let ((password-data (copy-hash-table password-data)))
    (password-cache-add "foo" "bar")
    (should (password-in-cache-p "foo"))
    (should (not (password-read-from-cache nil)))))

(ert-deftest password-cache-tests-read ()
  (let ((password-data (copy-hash-table password-data)))
    (password-cache-add "foo" "bar")
    (should (equal (password-read nil "foo") "bar"))))

(ert-deftest password-cache-tests-reset ()
  (let ((password-data (copy-hash-table password-data)))
    (password-cache-add "foo" "bar")
    (password-reset)
    (should (not (password-in-cache-p "foo")))))

(ert-deftest password-cache-tests-add/expires-key ()
  :tags '(:expensive-test)
  (let ((password-data (copy-hash-table password-data))
        (password-cache-expiry 0.01))
    (password-cache-add "foo" "bar")
    (sit-for 0.1)
    (should (not (password-in-cache-p "foo")))))

(ert-deftest password-cache-tests-no-password-cache ()
  (let ((password-data (copy-hash-table password-data))
        (password-cache nil))
    (password-cache-add "foo" "bar")
    (should (not (password-in-cache-p "foo")))
    (should (not (password-read-from-cache "foo")))))

(provide 'password-cache-tests)
;;; password-cache-tests.el ends here
