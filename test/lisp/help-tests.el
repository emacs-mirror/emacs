;;; help-tests.el --- Tests for help.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Juanma Barranquero <lekktu@gmail.com>
;; Keywords: help, internal

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

(require 'ert)

(ert-deftest help-split-fundoc-SECTION ()
  "Test new optional arg SECTION."
  (let* ((doc "Doc first line.\nDoc second line.")
         (usg "\n\n(fn ARG1 &optional ARG2)")
         (full (concat doc usg))
         (usage "(t ARG1 &optional ARG2)"))
    ;; Docstring has both usage and doc
    (should (equal (help-split-fundoc full t nil)    `(,usage . ,doc)))
    (should (equal (help-split-fundoc full t t)      `(,usage . ,doc)))
    (should (equal (help-split-fundoc full t 'usage)  usage))
    (should (equal (help-split-fundoc full t 'doc)    doc))
    ;; Docstring has no usage, only doc
    (should (equal (help-split-fundoc doc t nil)      nil))
    (should (equal (help-split-fundoc doc t t)       `(nil . ,doc)))
    (should (equal (help-split-fundoc doc t 'usage)  nil))
    (should (equal (help-split-fundoc doc t 'doc)    doc))
    ;; Docstring is only usage, no doc
    (should (equal (help-split-fundoc usg t nil)     `(,usage . nil)))
    (should (equal (help-split-fundoc usg t t)       `(,usage . nil)))
    (should (equal (help-split-fundoc usg t 'usage)  usage))
    (should (equal (help-split-fundoc usg t 'doc)    nil))
    ;; Docstring is null
    (should (equal (help-split-fundoc nil t nil)     nil))
    (should (equal (help-split-fundoc nil t t)       '(nil)))
    (should (equal (help-split-fundoc nil t 'usage)  nil))
    (should (equal (help-split-fundoc nil t 'doc)    nil))))

(provide 'help-tests)

;;; help-tests.el ends here
