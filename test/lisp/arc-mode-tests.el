;;; arc-mode-tests.el --- Test suite for arc-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2023 Free Software Foundation, Inc.

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
(require 'arc-mode)

(defvar arc-mode-tests-data-directory
  (expand-file-name "test/data/decompress" source-directory))

(ert-deftest arc-mode-test-archive-int-to-mode ()
  (let ((alist (list (cons 448 "-rwx------")
                     (cons 420 "-rw-r--r--")
                     (cons 292 "-r--r--r--")
                     (cons 512 "---------T")
                     (cons 1024 "------S---") ; Bug#28092
                     (cons 2048 "---S------"))))
    (dolist (x alist)
      (should (equal (cdr x) (file-modes-number-to-symbolic (car x)))))))

(ert-deftest arc-mode-test-zip-extract-gz ()
  (skip-unless (and archive-zip-extract (executable-find (car archive-zip-extract))))
  (skip-unless (executable-find "gzip"))
  (let* ((zip-file (expand-file-name "zg.zip" arc-mode-tests-data-directory))
         zip-buffer gz-buffer)
    (unwind-protect
        (with-current-buffer (setq zip-buffer (find-file-noselect zip-file))
          (setq gz-buffer (archive-extract))
          (should (equal (char-after) ?\N{SNOWFLAKE})))
      (when (buffer-live-p zip-buffer) (kill-buffer zip-buffer))
      (when (buffer-live-p gz-buffer) (kill-buffer gz-buffer)))))

(ert-deftest arc-mode-test-zip-ensure-ext ()
  "Regression test for bug#61326."
  (skip-unless (executable-find "zip"))
  (let* ((default-directory arc-mode-tests-data-directory)
         (base-zip-1 "base-1.zip")
         (base-zip-2 "base-2.zip")
         (content-1 '("1" "2"))
         (content-2 '("3" "4"))
         (make-file (lambda (name)
                      (with-temp-buffer
                        (insert name)
                        (write-file name))))
         (make-zip
          (lambda (zip files)
            (delete-file zip nil)
            (funcall (archive--act-files '("zip") files) zip)))
         (update-fn
          (lambda (zip-nonempty)
            (with-current-buffer (find-file-noselect zip-nonempty)
              (save-excursion
                (goto-char archive-file-list-start)
                (save-current-buffer
                  (archive-extract)
                  (save-excursion
                    (goto-char (point-max))
                    (insert ?a)
                    (save-buffer))
                  (kill-buffer (current-buffer)))
                (archive-extract)
                ;; [2] must be ?a; [3] must be (eobp)
                (should (eq (char-after 2) ?a))
                (should (eq (point-max) 3))))))
         (delete-fn
          (lambda (zip-nonempty)
            (with-current-buffer (find-file-noselect zip-nonempty)
              ;; mark delete and expunge first entry
              (save-excursion
                (goto-char archive-file-list-start)
                (should (length= archive-files 2))
                (archive-flag-deleted 1)
                (archive--expunge-maybe-force t)
                (should (length= archive-files 1))))))
         (test-modify
          (lambda (zip mod-fn)
            (let ((zip-base (concat zip ".zip"))
                  (tag (gensym)))
              (copy-file base-zip-1 zip t)
              (copy-file base-zip-2 zip-base t)
              (file-has-changed-p zip tag)
              (file-has-changed-p zip-base tag)
              (funcall mod-fn zip)
              (should-not (file-has-changed-p zip-base tag))
              (should (file-has-changed-p zip tag))))))
    ;; setup: make two zip files with different contents
    (mapc make-file (append content-1 content-2))
    (mapc (lambda (args) (apply make-zip args))
          (list (list base-zip-1 content-1)
                (list base-zip-2 content-2)))
    ;; test 1: with "test-update" and "test-update.zip", update
    ;; "test-update": (1) ensure only "test-update" is modified, (2)
    ;; ensure the contents of the new member is expected.
    (funcall test-modify "test-update" update-fn)
    ;; test 2: with "test-delete" and "test-delete.zip", delete entry
    ;; from "test-delete": (1) ensure only "test-delete" is modified,
    ;; (2) ensure the file list is reduced as expected.
    (funcall test-modify "test-delete" delete-fn)))

(provide 'arc-mode-tests)

;;; arc-mode-tests.el ends here
