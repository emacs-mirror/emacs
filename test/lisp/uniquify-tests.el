;;; uniquify-tests.el --- Tests for uniquify         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Spencer Baugh <sbaugh@janestreet.com>

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)

(ert-deftest uniquify-basic ()
  (let (bufs old-names)
    (cl-flet ((names-are (current-names &optional nosave)
                (should (equal (mapcar #'buffer-name bufs) current-names))
                (unless nosave (push current-names old-names))))
      (should (eq (get-buffer "z") nil))
      (push (find-file-noselect "a/b/z") bufs)
      (names-are '("z"))
      (push (find-file-noselect "a/b/c/z") bufs)
      (names-are '("z<c>" "z<b>"))
      (push (find-file-noselect "a/b/d/z") bufs)
      (names-are '("z<d>" "z<c>" "z<b>"))
      (push (find-file-noselect "e/b/z") bufs)
      (names-are '("z<e/b>" "z<d>" "z<c>" "z<a/b>"))
      ;; buffers without a buffer-file-name don't get uniquified by uniquify
      (push (generate-new-buffer "z") bufs)
      (names-are '("z" "z<e/b>" "z<d>" "z<c>" "z<a/b>"))
      ;; but they do get uniquified by the C code which uses <n>
      (push (generate-new-buffer "z") bufs)
      (names-are '("z<2>" "z" "z<e/b>" "z<d>" "z<c>" "z<a/b>"))
      (save-excursion
        ;; uniquify will happily work with file-visiting buffers whose names don't match buffer-file-name
        (find-file "f/y")
        (push (current-buffer) bufs)
        (rename-buffer "z" t)
        (names-are '("z<f>" "z<2>" "z" "z<e/b>" "z<d>" "z<c>" "z<a/b>") 'nosave)
        ;; somewhat confusing behavior results if a buffer is renamed to match an already-uniquified buffer
        (rename-buffer "z<a/b>" t)
        (names-are '("z<a/b><f>" "z<2>" "z" "z<e/b>" "z<d>" "z<c>" "z<a/b>") 'nosave))
      (while bufs
        (kill-buffer (pop bufs))
        (names-are (pop old-names) 'nosave)))))

(ert-deftest uniquify-dirs ()
  "Check strip-common-suffix and trailing-separator-p work together; bug#47132"
  (ert-with-temp-directory root
    (let ((a-path (file-name-concat root "a/x/y/dir"))
          (b-path (file-name-concat root "b/x/y/dir")))
      (make-directory a-path 'parents)
      (make-directory b-path 'parents)
      (let ((uniquify-buffer-name-style 'forward)
            (uniquify-strip-common-suffix t)
            (uniquify-trailing-separator-flag nil))
        (let ((bufs (list (find-file-noselect a-path)
                          (find-file-noselect b-path))))
          (should (equal (mapcar #'buffer-name bufs)
                         '("a/dir" "b/dir")))
          (mapc #'kill-buffer bufs)))
      (let ((uniquify-buffer-name-style 'forward)
            (uniquify-strip-common-suffix nil)
            (uniquify-trailing-separator-flag t))
        (let ((bufs (list (find-file-noselect a-path)
                          (find-file-noselect b-path))))
          (should (equal (mapcar #'buffer-name bufs)
                         '("a/x/y/dir/" "b/x/y/dir/")))
          (mapc #'kill-buffer bufs)))
      (let ((uniquify-buffer-name-style 'forward)
            (uniquify-strip-common-suffix t)
            (uniquify-trailing-separator-flag t))
        (let ((bufs (list (find-file-noselect a-path)
                          (find-file-noselect b-path))))
          (should (equal (mapcar #'buffer-name bufs)
                         '("a/dir/" "b/dir/")))
          (mapc #'kill-buffer bufs))))))

(ert-deftest uniquify-rename-to-dir ()
  "Giving a buffer a name which matches a directory doesn't rename the buffer"
  (let ((uniquify-buffer-name-style 'forward)
        (uniquify-trailing-separator-flag t))
      (save-excursion
        (find-file "../README")
        (rename-buffer "lisp" t)
        (should (equal (buffer-name) "lisp"))
        (kill-buffer))))

(ert-deftest uniquify-separator-style-reverse ()
  (let ((uniquify-buffer-name-style 'reverse)
        (uniquify-trailing-separator-flag t))
    (save-excursion
      (should (file-directory-p "../lib-src"))
      (find-file "../lib-src")
      (should (equal (buffer-name) "\\lib-src"))
      (kill-buffer))))

(ert-deftest uniquify-separator-ignored ()
  "If `uniquify-buffer-name-style' isn't forward or reverse,
`uniquify-trailing-separator-flag' is ignored."
  (let ((uniquify-buffer-name-style 'post-forward-angle-brackets)
        (uniquify-trailing-separator-flag t))
    (save-excursion
      (should (file-directory-p "../lib-src"))
      (find-file "../lib-src")
      (should (equal (buffer-name) "lib-src"))
      (kill-buffer))))

(ert-deftest uniquify-space-prefix ()
  "If a buffer starts with a space, | is added at the start"
  (save-excursion
    (find-file " foo")
    (should (equal (buffer-name) "| foo"))
    (kill-buffer)))

(require 'project)
(ert-deftest uniquify-project-transform ()
  "`project-uniquify-dirname-transform' works"
  (skip-unless (project-current nil source-directory))
  (let ((uniquify-dirname-transform #'project-uniquify-dirname-transform)
        (project-vc-name "foo1/bar")
        bufs)
    (save-excursion
      (let ((default-directory (expand-file-name "test/" source-directory)))
        (should (file-exists-p "../README"))
        (push (find-file-noselect "../README") bufs)
        (push (find-file-noselect "other/README") bufs)
        (should (equal (mapcar #'buffer-name bufs)
                       '("README<other>" "README<bar>")))
        (push (find-file-noselect "foo2/bar/README") bufs)
        (should (equal (mapcar #'buffer-name bufs)
                       '("README<foo2/bar>" "README<other>"
                         "README<foo1/bar>")))
        (while bufs
          (kill-buffer (pop bufs)))))))

(provide 'uniquify-tests)
;;; uniquify-tests.el ends here
