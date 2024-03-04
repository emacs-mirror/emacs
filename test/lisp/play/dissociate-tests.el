;;; dissociate-tests.el --- Tests for dissociate.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'dissociate)

(ert-deftest dissociate-tests-dissociated-press ()
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
            ((symbol-function 'random)  (lambda (_) 10)))
    (save-window-excursion
      (with-temp-buffer
        (insert "Lorem ipsum dolor sit amet")
        (dissociated-press)
        (should (string-match-p "dolor sit ametdolor sit amdolor sit amdolor sit am"
                                (buffer-string)))))))

(provide 'dissociate-tests)
;;; dissociate-tests.el ends here
