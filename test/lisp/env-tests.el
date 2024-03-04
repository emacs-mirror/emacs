;;; env-tests.el --- Tests for env.el  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

(require 'env)
(require 'ert)

(ert-deftest test-substitute-env-in-file-name ()
  (should (equal (substitute-env-in-file-name "foo_${HOME}_bar")
                 (concat "foo_" (getenv "HOME") "_bar"))))

(ert-deftest test-getenv-setenv ()
  (should (equal (setenv "EMACS_ENV_EL_TEST_VAR" "foobar") "foobar"))
  (should (equal (getenv "EMACS_ENV_EL_TEST_VAR") "foobar"))
  (should-not (getenv "LIKELY_TO_BE_NON_EXISTENT_FOO_BAR_BAZ")))

(ert-deftest test-with-environment-variables ()
  (let ((A "TEST") (B "/foo/bar"))
    (with-environment-variables ((A B))
      (should (equal (getenv A) B)))))

(provide 'env-tests)
;;; env-tests.el ends here
