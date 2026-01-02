;;; life-tests.el --- Tests for life.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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
(require 'life)

(ert-deftest test-life ()
  (let ((life--max-width 5)
        (life--max-height 3)
        (life-patterns [(" @ "
                     "  @"
                     "@@@")])
        (generations '("

  @
   @
 @@@
" "


 @ @
  @@
  @
" "


   @
 @ @
  @@
" "


  @
   @@
  @@
" "


   @
    @
  @@@
"
)))
    (life-setup)
    ;; Test initial state.
    (goto-char (point-min))
    (dolist (generation generations)
      ;; Hack to test buffer contents without trailing whitespace,
      ;; while also not modifying the "*Life*" buffer.
      (let ((str (buffer-string))
            (delete-trailing-lines t))
        (with-temp-buffer
          (insert str)
          (delete-trailing-whitespace)
          (should (equal (buffer-string) generation))))
      (life--tick))))

(provide 'life-tests)

;;; life-tests.el ends here
