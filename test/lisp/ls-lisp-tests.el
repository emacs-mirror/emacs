;;; ls-lisp-tests.el --- tests for ls-lisp.el  -*- lexical-binding: t-*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Keywords:

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
(require 'ls-lisp)

(ert-deftest ls-lisp-unload ()
  "Test for https://debbugs.gnu.org/xxxxx ."
  (should (advice-member-p 'ls-lisp--insert-directory 'insert-directory))
  (unload-feature 'ls-lisp 'force)
  (should-not (advice-member-p 'ls-lisp--insert-directory 'insert-directory))
  (require 'ls-lisp))

(ert-deftest ls-lisp-test-bug27762 ()
  "Test for https://debbugs.gnu.org/27762 ."
  (let* ((dir source-directory)
         (default-directory dir)
         (files (mapcar (lambda (f) (concat "src/" f))
                        (directory-files
                         (expand-file-name "src") nil "\\.*\\.c\\'")))
         ls-lisp-use-insert-directory-program buf)
    (unwind-protect
        (let ((file1 "src/cygw32.c")
              (file2 "src/atimer.c"))
          (setq buf (dired (nconc (list dir) files)))
          (dired-goto-file (expand-file-name file2 default-directory))
          (should-not (looking-at "^   -")) ; Must be 2 spaces not 3.
          (setq files (cons file1 (delete file1 files)))
          (kill-buffer buf)
          (setq buf (dired (nconc (list dir) files)))
          (should (looking-at "src"))
          (next-line) ; File names must be aligned.
          (should (looking-at "src")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest ls-lisp-test-bug27631 ()
  "Test for https://debbugs.gnu.org/27631 ."
  (let* ((dir (make-temp-file "bug27631" 'dir))
         (dir1 (expand-file-name "dir1" dir))
         (dir2 (expand-file-name "dir2" dir))
         (default-directory dir)
         ls-lisp-use-insert-directory-program buf)
    (unwind-protect
        (progn
          (make-directory dir1)
          (make-directory dir2)
          (with-temp-file (expand-file-name "a.txt" dir1))
          (with-temp-file (expand-file-name "b.txt" dir2))
          (setq buf (dired (expand-file-name "dir*/*.txt" dir)))
          (dired-toggle-marks)
          (should (cdr (dired-get-marked-files))))
      (delete-directory dir 'recursive)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest ls-lisp-test-bug27693 ()
  "Test for https://debbugs.gnu.org/27693 ."
  (let ((dir (expand-file-name "lisp" source-directory))
        (size "")
        ls-lisp-use-insert-directory-program buf)
    (unwind-protect
        (progn
          (setq buf (dired (list dir "simple.el" "subr.el"))
                size (number-to-string
                      (file-attribute-size
                       (file-attributes (dired-get-filename)))))
          (search-backward-regexp size nil t)
          (should (looking-back "[[:space:]]" (1- (point)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'ls-lisp-tests)
;;; ls-lisp-tests.el ends here
