;;; test-custom-deps.el --- Test custom deps  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;; The command `cus-test-deps' loads all (!) custom dependencies and
;; reports about load errors.

;;; Code:

(require 'ert)

(defconst custom-test-admin-cus-test
  (expand-file-name "admin/cus-test.el" source-directory))

(declare-function cus-test-deps custom-test-admin-cus-test)
(defvar cus-test-deps-errors)  ; from admin/cus-tests.el

(ert-deftest test-custom-deps ()
  :tags '(:expensive-test)
  (skip-unless (file-readable-p custom-test-admin-cus-test))
  (load custom-test-admin-cus-test)
  (cus-test-deps)
  (should-not cus-test-deps-errors))

;;; test-custom-deps.el ends here
