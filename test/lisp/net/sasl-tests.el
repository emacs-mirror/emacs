;;; sasl-tests.el --- tests for sasl.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
(require 'sasl)

(ert-deftest sasl-test-make-client ()
  (let ((client (sasl-make-client 'foo 'bar 'baz 'zut)))
    (should (eq (sasl-client-mechanism client) 'foo))
    (should (eq (sasl-client-name client) 'bar))
    (should (eq (sasl-client-service client) 'baz))
    (should (eq (sasl-client-server client) 'zut))))

(ert-deftest sasl-test-client-set-properties ()
  (let ((client (sasl-make-client 'foo 'bar 'baz 'zut)))
    (sasl-client-set-property client 'foo 'bar)
    (should (eq (sasl-client-property client 'foo) 'bar))))

(ert-deftest sasl-test-step-data ()
  (let ((step [nil nil]))
    (sasl-step-set-data step "foo")
    (should (equal (sasl-step-data step) "foo"))))

(ert-deftest sasl-test-unique-id ()
  (should (stringp (sasl-unique-id)))
  (should-not (equal (sasl-unique-id) (sasl-unique-id))))

(ert-deftest sasl-test-find-mechanism ()
  (should (sasl-find-mechanism '("ANONYMOUS")))
  (should-not (sasl-find-mechanism '("nonexistent mechanism"))))

(ert-deftest sasl-test-mechanism-name ()
  (let ((mechanism (sasl-find-mechanism '("ANONYMOUS"))))
    (should (equal (sasl-mechanism-name mechanism) "ANONYMOUS"))))

(provide 'sasl-tests)
;;; sasl-tests.el ends here
