;;; grep-tests.el --- Test suite for grep.el  -*- lexical-binding:t -*-

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

;;; Code:

(require 'ert)
(require 'grep)

(defconst grep-tests--ellipsis (if (char-displayable-p ?…) "[…]" "[...]")
  "The form that the ellipsis takes in `grep-find-abbreviate-properties'.")

(defun grep-tests--get-rgrep-abbreviation ()
  "Get the `display' property of the excessive part of the rgrep command."
  (with-temp-buffer
    (grep-compute-defaults)
    (insert (rgrep-default-command "search" "*" nil))
    (grep-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (re-search-forward "find ")
    (get-text-property (point) 'display)))

(defun grep-tests--check-rgrep-abbreviation ()
  "Check that the excessive part of the rgrep command is abbreviated iff
`grep-find-abbreviate' is non-nil."
  (let ((grep-find-abbreviate t))
    (should (equal (grep-tests--get-rgrep-abbreviation)
                   grep-tests--ellipsis)))
  (let ((grep-find-abbreviate nil))
    (should-not (grep-tests--get-rgrep-abbreviation))))

(ert-deftest grep-tests--rgrep-abbreviate-properties-gnu-linux ()
  (let ((system-type 'gnu/linux))
    (grep-tests--check-rgrep-abbreviation)))

(ert-deftest grep-tests--rgrep-abbreviate-properties-darwin ()
  (let ((system-type 'darwin))
    (grep-tests--check-rgrep-abbreviation)))

(ert-deftest grep-tests--rgrep-abbreviate-properties-windows-nt-dos-semantics ()
  (let ((system-type 'windows-nt))
    (cl-letf (((symbol-function 'w32-shell-dos-semantics) #'always))
      (grep-tests--check-rgrep-abbreviation))))

(ert-deftest grep-tests--rgrep-abbreviate-properties-windows-nt-sh-semantics ()
  (let ((system-type 'windows-nt))
    (cl-letf (((symbol-function 'w32-shell-dos-semantics) #'ignore))
      (grep-tests--check-rgrep-abbreviation))))

;;; grep-tests.el ends here
