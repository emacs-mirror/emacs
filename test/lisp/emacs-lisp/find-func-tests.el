;;; find-func-tests.el --- Unit tests for find-func.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert-x)                        ;For `ert-run-keys'.

(ert-deftest find-func-tests--library-completion () ;bug#43393
  ;; FIXME: How can we make this work in batch (see also
  ;; `mule-cmds--test-universal-coding-system-argument')?
  ;; (skip-unless (not noninteractive))
  ;; Check that `partial-completion' works when completing library names.
  (should (equal "org/org"
                 (ert-simulate-keys
                     (kbd "o / o r g TAB RET")
                   (read-library-name))))
  ;; Check that absolute file names also work.
  (should (equal (expand-file-name "nxml/" data-directory)
                 (ert-simulate-keys
                     (concat data-directory (kbd "n x / TAB RET"))
                   (read-library-name)))))

(provide 'find-func-tests)
;;; find-func-tests.el ends here
