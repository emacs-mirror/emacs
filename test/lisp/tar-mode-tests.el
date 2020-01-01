;;; tar-mode-tests.el --- Test suite for tar-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
(require 'tar-mode)

(defvar tar-mode-tests-data-directory
  (expand-file-name "test/data/decompress" source-directory))

(ert-deftest tar-mode-test-tar-grind-file-mode ()
  (let ((alist (list (cons 448 "rwx------")
                     (cons 420 "rw-r--r--")
                     (cons 292 "r--r--r--")
                     (cons 512 "--------T")
                     (cons 1024 "-----S---"))))
    (dolist (x alist)
      (should (equal (cdr x) (tar-grind-file-mode (car x)))))))

(ert-deftest tar-mode-test-tar-extract-gz ()
  (skip-unless (executable-find "gzip"))
  (let* ((tar-file (expand-file-name "tg.tar.gz" tar-mode-tests-data-directory))
         tar-buffer gz-buffer)
    (unwind-protect
        (with-current-buffer (setq tar-buffer (find-file-noselect tar-file))
          (setq gz-buffer (tar-extract))
          (should (equal (char-after) ?\N{SNOWFLAKE})))
      (when (buffer-live-p tar-buffer) (kill-buffer tar-buffer))
      (when (buffer-live-p gz-buffer) (kill-buffer gz-buffer)))))

(provide 'tar-mode-tests)

;; tar-mode-tests.el ends here
