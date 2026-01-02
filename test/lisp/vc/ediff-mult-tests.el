;;; ediff-mult-tests.el --- Tests for ediff-mult.el  -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ediff-mult)

(ert-deftest ediff-test-bug3348 ()
  "After saving `ediff-meta-diff-buffer' to a file, we should not reuse it."
  (let ((test-dir
         (expand-file-name "bug-3348-testdir" temporary-file-directory)))
    (make-directory test-dir t)
    (cd test-dir)

    (make-directory "dir-a" t)
    (make-directory "dir-b" t)

    (with-temp-file "dir-a/file"
      (insert "aaa\n"))
    (with-temp-file "dir-b/file"
      (insert "bbb\n"))

    (ediff-directories "dir-a" "dir-b" nil)
    (switch-to-buffer "*Ediff Session Group Panel*")

    (ediff-next-meta-item 1)
    (ediff-mark-for-operation-at-pos nil)
    (ediff-collect-custom-diffs)

    (with-current-buffer "*Ediff Multifile Diffs*"
      (write-file "foo.patch"))

    (with-temp-file "dir-b/file"
      (insert "BBB\n"))
    (ediff-collect-custom-diffs)

    (should-not (equal ediff-meta-diff-buffer (get-buffer "foo.patch")))))
