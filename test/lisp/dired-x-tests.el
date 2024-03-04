;;; dired-x-tests.el --- Test suite for dired-x. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'dired-x)


(ert-deftest dired-test-bug25942 ()
  "Test for https://debbugs.gnu.org/25942 ."
  (let* ((dirs (list "Public" "Music"))
         (files (list ".bashrc" "bar.c" "foo.c" "c" ".c"))
         (all-but-c
          (sort
           (append (copy-sequence dirs)
                   (delete "c" (copy-sequence files)))
           #'string<))
         (extension "c"))
    (ert-with-temp-directory dir
      (dolist (d dirs)
        (make-directory (expand-file-name d dir)))
      (dolist (f files)
        (write-region nil nil (expand-file-name f dir)))
      (dired dir)
      (dired-mark-extension extension)
      (should (equal '("bar.c" "foo.c")
                     (sort (dired-get-marked-files 'local) #'string<)))
      (dired-unmark-all-marks)
      (dired-mark-suffix extension)
      (should (equal all-but-c
                     (sort (dired-get-marked-files 'local) #'string<))))))

(ert-deftest dired-x--string-to-number ()
  (should (= (dired-x--string-to-number "2.4K") 2457.6))
  (should (= (dired-x--string-to-number "2400") 2400))
  (should (= (dired-x--string-to-number "123.4M") 129394278.4))
  (should (= (dired-x--string-to-number "123.40000M") 129394278.4))
  (should (= (dired-x--string-to-number "4.134") 4134))
  (should (= (dired-x--string-to-number "4,134") 4134))
  (should (= (dired-x--string-to-number "4 134") 4134))
  (should (= (dired-x--string-to-number "41,52,134") 4152134)))

(provide 'dired-x-tests)
;;; dired-x-tests.el ends here
