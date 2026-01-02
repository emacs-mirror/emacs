;;; arc-mode-tests.el --- Test suite for arc-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

(declare-function tar-extract "tar-mode")
(ert-deftest arc-mode-test-zip-extract-tar-and-gz ()
  (skip-unless (and archive-zip-extract (executable-find (car archive-zip-extract))))
  (skip-unless (executable-find "gzip"))
  (require 'tar-mode)
  (let* ((zip-file (expand-file-name "ztg.zip" arc-mode-tests-data-directory))
         zip-buffer tar-buffer gz-buffer)
    (unwind-protect
        (with-current-buffer (setq zip-buffer (find-file-noselect zip-file))
          (with-current-buffer (setq tar-buffer (archive-extract))
            (setq gz-buffer (tar-extract))
            (should (equal (char-after) ?\N{SNOWFLAKE}))))
      (when (buffer-live-p zip-buffer) (kill-buffer zip-buffer))
      (when (buffer-live-p tar-buffer) (kill-buffer tar-buffer))
      (when (buffer-live-p gz-buffer) (kill-buffer gz-buffer)))))

(defun arc-mode-test-make-file (name)
  "Create file NAME in default directory with content NAME.
Return NAME."
  (with-temp-buffer
    (insert name)
    (write-file name))
  name)

(defun arc-mode-test-make-archive (command arc files)
  "Call COMMAND to create archive ARC containing FILES.
Return a cons (ARC . EXIT-STATUS)."
  (unless (listp command)
    (setq command (list command)))
  (delete-file arc nil)
  (cons arc (funcall (archive--act-files command files) arc)))

(defmacro define-arc-mode-test-on-type (name command extension type)
  "Define a test that tests function `archive-find-type'.
Name the test based on NAME.  The generated test first calls

  (call-process (car COMMAND) nil nil nil
                (append COMMAND (list ARCHIVE MEMBER)))

to create file ARCHIVE with extension EXTENSION and containing a single
member MEMBER.  Then the test finds ARCHIVE and ensures that function
`archive-find-type' detects it as an archive having type TYPE."
  (let* ((command (eval command))
         (argv0   (car command))
         (type    (eval type)))
    `(ert-deftest ,(intern (format "arc-mode-test-type-%s" name)) ()
       (skip-unless (executable-find ,argv0))
       (let ((default-directory arc-mode-tests-data-directory)
             (member nil) (archive nil) (buffer nil)
             result exit-status type)
         (unwind-protect
             (progn
               (setq member (arc-mode-test-make-file "member")
                     result (arc-mode-test-make-archive
                             (quote ,command) ,(format "arc.%s" extension) (list member))
                     archive (car result)
                     exit-status (cdr result))
               ;; do not count archiver errors as test failures
               (skip-unless (eq exit-status 0))
               (with-current-buffer
                   (setq buffer (find-file-literally archive))
                 (setq type (condition-case err
                                (archive-find-type)
                              (error
                               ;; turn the most likely error into a nice
                               ;; and self-explaining symbol that can be
                               ;; compared in a `should'
                               (if (string= (cadr err) "Buffer format not recognized")
                                   'signature-not-recognized
                                 (signal (car err) (cdr err))))))
                 (should (eq type (quote ,type)))))
           (when buffer (kill-buffer buffer))
           (dolist (file (list member archive))
             (when file (ignore-errors (delete-file file)))))))))

(define-arc-mode-test-on-type "zip" '("zip") "zip" 'zip)

(define-arc-mode-test-on-type "split-zip" '("zip" "-s1") "zip" 'zip)

(define-arc-mode-test-on-type "arc" '("arc" "a") "arc" 'arc)

(define-arc-mode-test-on-type "lha" '("lha" "a") "lzh" 'lzh)

(define-arc-mode-test-on-type "rar" '("rar" "a") "rar" 'rar)

(define-arc-mode-test-on-type "ar" '("ar" "q") "a" 'ar)

;; prefer executable "7z" to "7za", since the former seems be supported
;; on a broader range of ports
(define-arc-mode-test-on-type "7z" '("7z" "a") "7z" '7z)

(ert-deftest arc-mode-test-zip-ensure-ext ()
  "Regression test for bug#61326."
  (skip-unless (executable-find "zip"))
  (let* ((default-directory arc-mode-tests-data-directory)
         (created-files nil)
         (base-zip-1 "base-1.zip")
         (base-zip-2 "base-2.zip")
         (content-1 '("1" "2"))
         (content-2 '("3" "4"))
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
              (push zip created-files)
              (copy-file base-zip-1 zip t)
              (push zip-base created-files)
              (copy-file base-zip-2 zip-base t)
              (file-has-changed-p zip tag)
              (file-has-changed-p zip-base tag)
              (funcall mod-fn zip)
              (should-not (file-has-changed-p zip-base tag))
              (should (file-has-changed-p zip tag))))))
    (unwind-protect
        (progn
          ;; setup: make two zip files with different contents
          (dolist (file (append content-1 content-2))
            (push (arc-mode-test-make-file file) created-files))
          (push (car (arc-mode-test-make-archive "zip" base-zip-1 content-1))
                created-files)
          (push (car (arc-mode-test-make-archive "zip" base-zip-2 content-2))
                created-files)

          ;; test 1: with "test-update" and "test-update.zip", update
          ;; "test-update": (1) ensure only "test-update" is modified, (2)
          ;; ensure the contents of the new member is expected.
          (funcall test-modify "test-update" update-fn)

          ;; test 2: with "test-delete" and "test-delete.zip", delete entry
          ;; from "test-delete": (1) ensure only "test-delete" is modified,
          ;; (2) ensure the file list is reduced as expected.
          (funcall test-modify "test-delete" delete-fn))

      ;; Clean up created files.
      (dolist (file created-files)
        (ignore-errors (delete-file file))))))

(provide 'arc-mode-tests)

;;; arc-mode-tests.el ends here
