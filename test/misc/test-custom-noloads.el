;;; test-custom-noloads.el --- Test custom noloads  -*- lexical-binding:t -*-

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

;; The command `cus-test-noloads' returns a list of variables which
;; are somewhere declared as custom options, but not loaded by
;; `custom-load-symbol'.

;;; Code:

(require 'ert)

(defconst custom-test-admin-cus-test
  (expand-file-name "admin/cus-test.el" source-directory))

(declare-function cus-test-noloads custom-test-admin-cus-test)
(defvar cus-test-vars-not-cus-loaded)  ; from admin/cus-tests.el

;; FIXME: Multiple failures here.
(ert-deftest custom-test-load ()
  :tags '(:expensive-test :unstable)
  :expected-result :failed ; FIXME: See above.
  (skip-unless (file-readable-p custom-test-admin-cus-test))
  (load custom-test-admin-cus-test)
  (cus-test-noloads)
  (should-not cus-test-vars-not-cus-loaded))

;;; test-custom-noloads.el ends here
