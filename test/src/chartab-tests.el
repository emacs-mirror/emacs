;;; chartab-tests.el --- Tests for char-tab.c

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>

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

;;; Code:

(require 'ert)

(defun chartab-set-and-test (rng)
  (let ((tbl (make-char-table nil nil))
        (from (car rng))
        (to (cdr rng)))
    (set-char-table-range tbl rng t)
    (should (eq (aref tbl from) t))
    (should (eq (aref tbl to) t))
    (should (eq (aref tbl (/ (+ from to) 2)) t))
    (when (< to (max-char))
      (should-not (eq (aref tbl (1+ to)) t)))
    (when (> from 0)
      (should-not (eq (aref tbl (1- from)) t)))))

(ert-deftest chartab-test-range-setting ()
  (mapc (lambda (elt)
          (chartab-set-and-test elt))
        '((0 . 127)
          (128 . 256)
          (#x1000 . #x1fff)
          (#x1001 . #x2000)
          (#x10000 . #x20000)
          (#x10001 . #x1ffff)
          (#x20000 . #x30000)
          (#xe0e00 . #xe0ef6)
          )))

(provide 'chartab-tests)
;;; chartab-tests.el ends here
