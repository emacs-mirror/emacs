;;; org-tests.el --- tests for org/org.el

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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

(ert-deftest org-package-version ()
  "Test Version: header is present and correct.
Ref <https://debbugs.gnu.org/30310>."
  (should (require 'org-version nil t))
  (should (equal (version-to-list (org-release))
                 (cdr (assq 'org package--builtin-versions)))))
