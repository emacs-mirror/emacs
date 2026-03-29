;;; map-ynp-tests.el --- Tests for map-ynp.el        -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Spencer Baugh <sbaugh@catern.com>
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

;; Tests for map-ynp.el.

;;; Code:

(require 'ert)

(defun map-ynp-tests-simple-call ()
  (map-y-or-n-p ""  #'ignore '(1)))

(ert-deftest test-map-ynp-kmacro ()
  "Test that `map-y-or-n-p' in a kmacro terminates on end of input."
  (let ((eval-expression-debug-on-error nil)) ;; bug#67836
    (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET y"))
    (should-error
     (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET")))
    (unless noninteractive
      (let ((noninteractive t))
        (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET y"))
        (should-error
         (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET")))))))

(provide 'map-ynp-tests)
;;; map-ynp-tests.el ends here
