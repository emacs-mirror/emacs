;;; dired-tests.el --- Tests for directory-files in dired.c  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords:

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

;; These tests check mostly for correct behaviour with COUNT argument.

;;; Code:
(require 'ert)

(ert-deftest directory-files-tests ()
  (let ((testdir (expand-file-name "directory-files-test"
                                (temporary-file-directory)))
        (nod directory-files-no-dot-files-regexp))
    (unwind-protect
        (progn
          (when (file-directory-p testdir)
            (delete-directory testdir t))

          (make-directory testdir)
          (when (file-directory-p testdir)
            ;; directory-empty-p: test non-existent dir
            (should-not (directory-empty-p "some-imaginary-dir"))
            (should (= 2 (length (directory-files testdir))))
            ;; directory-empty-p: test empty dir
            (should (directory-empty-p testdir))
            (should-not (directory-files testdir nil nod t 1))
            (dolist (file '(a b c d))
              (make-empty-file (expand-file-name (symbol-name file) testdir)))
            (should (= 6 (length (directory-files testdir))))
            (should (equal "abcd" (mapconcat 'identity (directory-files
                                                        testdir nil nod) "")))
            (should (= 2 (length (directory-files testdir nil "[bc]"))))
            (should (= 3 (length (directory-files testdir nil nod nil 3))))
            (dolist (file '(5 4 3 2 1))
              (make-empty-file (expand-file-name (number-to-string
                                                  file) testdir)))
            ;;(should (= 0 (length (directory-files testdir nil "[0-9]" t -1))))
            (should (= 5 (length (directory-files testdir nil "[0-9]" t))))
            (should (= 5 (length (directory-files testdir nil "[0-9]" t 50))))
            (should-not (directory-empty-p testdir)))

          (delete-directory testdir t)))))

(ert-deftest directory-files-and-attributes-tests ()
  (let ((testdir (expand-file-name "directory-files-test"
                                (temporary-file-directory)))
        (nod directory-files-no-dot-files-regexp))

    (unwind-protect
        (progn
          (when (file-directory-p testdir)
            (delete-directory testdir t))

          (make-directory testdir)
          (when (file-directory-p testdir)
            (should (= 2 (length (directory-files testdir))))
            (should-not (directory-files-and-attributes testdir t nod t 1))
            (dolist (file '(a b c d))
              (make-directory (expand-file-name (symbol-name file) testdir)))
            (should (= 6 (length (directory-files-and-attributes testdir))))
            (dolist (dir (directory-files-and-attributes testdir t nod))
              (should (file-directory-p (car dir)))
              (should-not (file-regular-p (car dir))))
            (should (= 2 (length
                          (directory-files-and-attributes testdir nil
                                                          "[bc]"))))
            (should (= 3 (length
                          (directory-files-and-attributes testdir nil nod
                                                          nil nil 3))))
            (dolist (file '(5 4 3 2 1))
              (make-empty-file (expand-file-name (number-to-string file)
                                                 testdir)))
            ;; (should (= 0 (length (directory-files-and-attributes testdir nil
            ;;                                                      "[0-9]" t
            ;;                                                      nil -1))))
            (should (= 5 (length
                          (directory-files-and-attributes testdir nil
                                                          "[0-9]" t))))
            (should (= 5 (length
                          (directory-files-and-attributes testdir nil
                                                          "[0-9]" t
                                                          nil 50))))))
      (when (file-directory-p testdir)
        (delete-directory testdir t)))))

(provide 'dired-tests)
;;; dired-tests.el ends here
