;;; isearch-tests.el --- Tests for isearch.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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

(ert-deftest isearch--test-update ()
  (with-temp-buffer
    (setq isearch--current-buffer (current-buffer)))
  (with-temp-buffer
    (isearch-update)
    (should (equal isearch--current-buffer (current-buffer)))))

(ert-deftest isearch--test-done ()
  ;; Normal operation.
  (isearch-update)
  (isearch-done)
  (should-not isearch--current-buffer)
  ;; Bug #21091: let `isearch-done' work without `isearch-update'.
  (isearch-done))

(provide 'isearch-tests)
;;; isearch-tests.el ends here
