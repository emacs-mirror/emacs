;;; warnings-tests.el --- tests for warnings.el  -*- lexical-binding: t; -*-

;; Author: Stefan Kangas <stefankangas@gmail.com>

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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
(require 'warnings)

(ert-deftest test-warning-suppress-p ()
  (should (warning-suppress-p 'foo '((foo))))
  (should (warning-suppress-p '(foo bar) '((foo bar))))
  (should (warning-suppress-p '(foo bar baz) '((foo bar))))
  (should-not (warning-suppress-p '(foo bar baz) '((foo bax))))
  (should-not (warning-suppress-p 'foobar nil)))

(ert-deftest test-display-warning ()
  (dolist (level '(:emergency :error :warning))
    (with-temp-buffer
      (display-warning '(foo) "Hello123" level (current-buffer))
      (should (string-match "foo" (buffer-string)))
      (should (string-match "Hello123" (buffer-string))))
    (with-current-buffer "*Messages*"
      (should (string-match "Hello123" (buffer-string))))))

(ert-deftest test-display-warning/warning-minimum-level ()
  ;; This test only works interactively:
  :expected-result :failed
  (let ((warning-minimum-level :emergency))
    (with-temp-buffer
      (display-warning '(foo) "baz" :warning (current-buffer)))
    (with-current-buffer "*Messages*"
      (should-not (string-match "baz" (buffer-string))))))

(ert-deftest test-display-warning/warning-minimum-log-level ()
  (let ((warning-minimum-log-level :error))
    (with-temp-buffer
      (display-warning '(foo) "hello" :warning (current-buffer))
      (should-not (string-match "hello" (buffer-string))))))

(provide 'warnings-tests)

;;; warnings-tests.el ends here
