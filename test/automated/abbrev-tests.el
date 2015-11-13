;;; abbrev-tests.el --- Test suite for abbrevs.

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>
;; Keywords: abbrevs

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'abbrev)

(ert-deftest copy-abbrev-table-test ()
  (defvar foo-abbrev-table nil)         ; Avoid compiler warning
  (define-abbrev-table 'foo-abbrev-table
    '())
  (should (abbrev-table-p foo-abbrev-table))
  ;; Bug 21828
  (let ((new-foo-abbrev-table
         (condition-case nil
             (copy-abbrev-table foo-abbrev-table)
           (error nil))))
    (should (abbrev-table-p new-foo-abbrev-table)))
  (should-not (string-equal (buffer-name) "*Backtrace*")))

(provide 'abbrev-tests)

;;; abbrev-tests.el ends here
