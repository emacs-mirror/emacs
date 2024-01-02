;;; test-custom-libs.el --- Test custom loads  -*- lexical-binding:t -*-

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

;; This file runs for all libraries with autoloads separate Emacs
;; processes of the form "emacs -batch -l LIB".

;;; Code:

(require 'ert)

(defconst custom-test-admin-cus-test
  (expand-file-name "admin/cus-test.el" source-directory))

(declare-function cus-test-libs custom-test-admin-cus-test)
(defvar cus-test-libs-errors)  ; from admin/cus-tests.el

;; FIXME: Currently fails for:
;;        - lisp/term/ns-win.el
;;        - lisp/org/org-num.el
(ert-deftest test-custom-libs ()
  :tags '(:expensive-test)
  :expected-result :failed ; FIXME: See above.
  ;; This test is very slow, and IMO not worth the time it takes.
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (skip-unless (file-readable-p custom-test-admin-cus-test))
  (load custom-test-admin-cus-test)
  (cus-test-libs t)
  (should-not cus-test-libs-errors))

;;; test-custom-libs.el ends here
