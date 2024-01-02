;;; ls-lisp-tests.el --- tests for ls-lisp.el  -*- lexical-binding: t-*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'ls-lisp)
(require 'dired)

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
          (with-suppressed-warnings ((interactive-only next-line))
            (next-line)) ; File names must be aligned.
          (should (looking-at "src")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest ls-lisp-test-bug27631 ()
  "Test for https://debbugs.gnu.org/27631 ."
  (ert-with-temp-directory dir
    :suffix "bug27631"
    (let* ((dir1 (expand-file-name "dir1" dir))
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
        (when (buffer-live-p buf) (kill-buffer buf))))))

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

(ert-deftest ls-lisp-test-bug55787 ()
  "Test proper sorting by version."
  (let ((files1 (vector "34 klmn-300dpi.jpg"
                        "34 klmn-300dpi.png"
                        "054_xyz.jpg"
                        "054_xyz.png"
                        "91 opqrs.jpg"
                        "91 opqrs.png"
                        "0717-abcd.jpg"
                        "0717-abcd.png"
                        "1935 uv.jpg"
                        "1935 uv.png"
                        "FFFF_fghk.jpg"
                        "FFFF_fghk.png"
                        "hhhh.jpg"
		        "hhhh.png"))
        (files2 (vector "01.0" "10" "010" "01.2")))
    (should (equal (sort files1
                         (lambda (x y)
                           (ls-lisp-version-lessp x y)))
                   '["0717-abcd.jpg"
                     "0717-abcd.png"
                     "054_xyz.jpg"
                     "054_xyz.png"
                     "34 klmn-300dpi.jpg"
                     "34 klmn-300dpi.png"
                     "91 opqrs.jpg"
                     "91 opqrs.png"
                     "1935 uv.jpg"
                     "1935 uv.png"
                     "FFFF_fghk.jpg"
                     "FFFF_fghk.png"
                     "hhhh.jpg"
                     "hhhh.png"]))
    (should (equal (sort files2
                         (lambda (x y)
                           (ls-lisp-version-lessp x y)))
                   '["01.0" "01.2" "010" "10"]))))

(provide 'ls-lisp-tests)
;;; ls-lisp-tests.el ends here
