;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

;;; Code:

(require 'smerge-mode)

(ert-deftest smerge-mode-test-empty-hunk ()
  "Regression test for bug #25555"
  (with-temp-buffer
    (insert "<<<<<<< one\n")
    (save-excursion
      (insert "=======\nLLL\n>>>>>>> end\n"))
    (smerge-mode)
    (smerge-keep-current)
    (should (equal (buffer-substring (point-min) (point-max)) ""))))

(provide 'smerge-mode-tests)
