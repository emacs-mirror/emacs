;;; replace-tests.el --- tests for replace.el.

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

(ert-deftest query-replace--split-string-tests ()
  (let ((sep (propertize "\0" 'separator t)))
    (dolist (before '("" "b"))
      (dolist (after '("" "a"))
        (should (equal
                 (query-replace--split-string (concat before sep after))
                 (cons before after)))
        (should (equal
                 (query-replace--split-string (concat before "\0" after))
                 (concat before "\0" after)))))))

;;; replace-tests.el ends here
