;;; test-custom-opts.el --- Test custom opts  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

;; The command `cus-test-opts' tests many (all?) custom options.

;;; Code:

(require 'ert)

(defconst custom-test-admin-cus-test
  (expand-file-name "admin/cus-test.el" source-directory))

(declare-function cus-test-opts custom-test-admin-cus-test)

(ert-deftest check-for-wrong-custom-opts ()
  :tags '(:expensive-test)
  (skip-unless (file-readable-p custom-test-admin-cus-test))
  (load custom-test-admin-cus-test)
  (should (null (cus-test-opts t))))

;;; test-custom-opts.el ends here
