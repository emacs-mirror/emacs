;;; tests/em-ls-tests.el --- em-ls test suite

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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
(require 'em-ls)
(require 'dired)

(ert-deftest em-ls-test-bug27631 ()
  "Test for https://debbugs.gnu.org/27631 ."
  (let* ((dir (make-temp-file "bug27631" 'dir))
         (dir1 (expand-file-name "dir1" dir))
         (dir2 (expand-file-name "dir2" dir))
         (default-directory dir)
         (orig eshell-ls-use-in-dired)
         buf)
    (unwind-protect
        (progn
          (customize-set-value 'eshell-ls-use-in-dired t)
          (make-directory dir1)
          (make-directory dir2)
          (with-temp-file (expand-file-name "a.txt" dir1))
          (with-temp-file (expand-file-name "b.txt" dir2))
          (setq buf (dired (expand-file-name "dir*/*.txt" dir)))
          (dired-toggle-marks)
          (should (cdr (dired-get-marked-files))))
      (customize-set-variable 'eshell-ls-use-in-dired orig)
      (delete-directory dir 'recursive)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest em-ls-test-bug27817 ()
  "Test for https://debbugs.gnu.org/27817 ."
  (let ((orig eshell-ls-use-in-dired)
        (dired-use-ls-dired 'unspecified)
        buf insert-directory-program)
    (unwind-protect
        (progn
          (customize-set-variable 'eshell-ls-use-in-dired t)
          (should (setq buf (dired source-directory))))
      (customize-set-variable 'eshell-ls-use-in-dired orig)
      (and (buffer-live-p buf) (kill-buffer)))))

(ert-deftest em-ls-test-bug27843 ()
  "Test for https://debbugs.gnu.org/27843 ."
  (let ((orig eshell-ls-use-in-dired)
        (dired-use-ls-dired 'unspecified)
        buf insert-directory-program)
    (unwind-protect
        (progn
          (customize-set-variable 'eshell-ls-use-in-dired t)
          (setq buf (dired (list source-directory "lisp")))
          (dired-toggle-marks)
          (should-not (cdr (dired-get-marked-files))))
      (customize-set-variable 'eshell-ls-use-in-dired orig)
      (and (buffer-live-p buf) (kill-buffer)))))

(ert-deftest em-ls-test-bug27844 ()
  "Test for https://debbugs.gnu.org/27844 ."
  ;; FIXME: it would be better to use something other than source-directory
  ;; in this test.
  (skip-unless (and source-directory
                    (file-exists-p
                     (expand-file-name "lisp/subr.el" source-directory))))
  (let ((orig eshell-ls-use-in-dired)
        (dired-use-ls-dired 'unspecified)
        buf insert-directory-program)
    (unwind-protect
        (progn
          (customize-set-variable 'eshell-ls-use-in-dired t)
          (setq buf (dired (expand-file-name "lisp/*.el" source-directory)))
          (dired-toggle-marks)
          (should (cdr (dired-get-marked-files)))
          (kill-buffer buf)
          ;; Eshell's default format duplicates the year for non-recent files,
          ;; eg "2015-05-06  2015", which doesn't make a lot of sense,
          ;; and causes this portion of the test to fail if subr.el
          ;; is non-recent (eg if building from a tarfile unpacked
          ;; with a fixed early timestamp for reproducibility).  Bug#33734.
          (let ((eshell-ls-date-format "%b %e"))
            (setq buf (dired (expand-file-name "lisp/subr.el"
                                               source-directory))))
          (should (looking-at "subr\\.el")))
      (customize-set-variable 'eshell-ls-use-in-dired orig)
      (and (buffer-live-p buf) (kill-buffer)))))


(provide 'em-ls-test)

;;; em-ls-tests.el ends here
