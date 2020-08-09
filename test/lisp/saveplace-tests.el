;;; saveplace-tests.el --- Tests for saveplace.el  -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

;;; Commentary:

(require 'ert)
(require 'saveplace)

(defvar saveplace-tests-dir
  (file-truename
   (expand-file-name "saveplace-resources"
                     (file-name-directory (or load-file-name
                                              buffer-file-name)))))

(ert-deftest saveplace-test-save-place-to-alist/dir ()
  (save-place-mode)
  (let* ((save-place-alist nil)
         (save-place-loaded t)
         (loc saveplace-tests-dir))
    (save-window-excursion
      (dired loc)
      (save-place-to-alist)
      (should (equal save-place-alist
                     `((,(concat loc "/")
                        (dired-filename . ,(concat loc "/saveplace")))))))))

(ert-deftest saveplace-test-save-place-to-alist/file ()
  (save-place-mode)
  (let* ((tmpfile (make-temp-file "emacs-test-saveplace-"))
         (save-place-alist nil)
         (save-place-loaded t)
         (loc tmpfile)
         (pos 4))
    (unwind-protect
        (save-window-excursion
          (find-file loc)
          (insert "abc") ; must insert something
          (save-place-to-alist)
          (should (equal save-place-alist (list (cons tmpfile pos)))))
      (delete-file tmpfile))))

(ert-deftest saveplace-test-forget-unreadable-files ()
  (save-place-mode)
  (let* ((save-place-loaded t)
         (tmpfile (make-temp-file "emacs-test-saveplace-"))
         (alist-orig (list (cons "/this/file/does/not/exist" 10)
                           (cons tmpfile 1917)))
         (save-place-alist alist-orig))
    (unwind-protect
        (progn
          (save-place-forget-unreadable-files)
          (should (equal save-place-alist (cdr alist-orig))))
      (delete-file tmpfile))))

(ert-deftest saveplace-test-place-alist-to-file ()
  (save-place-mode)
  (let* ((tmpfile (make-temp-file "emacs-test-saveplace-"))
         (tmpfile2 (make-temp-file "emacs-test-saveplace-"))
         (save-place-file tmpfile)
         (save-place-alist (list (cons tmpfile2 99))))
    (unwind-protect
        (progn (save-place-alist-to-file)
               (setq save-place-alist nil)
               (save-window-excursion
                 (find-file save-place-file)
                 (unwind-protect
                     (should (string-match tmpfile2 (buffer-string)))
                   (kill-buffer))))
      (delete-file tmpfile)
      (delete-file tmpfile2))))

(ert-deftest saveplace-test-load-alist-from-file ()
  (save-place-mode)
  (let ((save-place-loaded nil)
        (save-place-file
         (expand-file-name "saveplace" saveplace-tests-dir))
        (save-place-alist nil))
    (load-save-place-alist-from-file)
    (should (equal save-place-alist
                   '(("/home/skangas/.emacs.d/cache/recentf" . 1306)
                     ("/home/skangas/wip/emacs/"
                      (dired-filename . "/home/skangas/wip/emacs/COPYING")))))))

(provide 'saveplace-tests)
;;; saveplace-tests.el ends here
