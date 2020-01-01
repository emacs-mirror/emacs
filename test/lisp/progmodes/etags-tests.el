;;; etags-tests.el --- Test suite for etags.el.

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>

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
(require 'etags)
(eval-when-compile (require 'cl-lib))

(defvar his-masters-voice t)

(defconst etags-tests--test-dir
  (or (getenv "EMACS_TEST_DIRECTORY")
      (expand-file-name "../../.."
                        (or load-file-name buffer-file-name))))

(defun y-or-n-p (_prompt)
  "Replacement for `y-or-n-p' that returns what we tell it to."
  his-masters-voice)

(ert-deftest etags-bug-158 ()
  "Test finding tags with local and global tags tables."
  (let ((buf-with-global-tags (get-buffer-create "*buf-global*"))
        (buf-with-local-tags (get-buffer-create "*buf-local*"))
        xref-buf)
    (set-buffer buf-with-global-tags)
    (setq default-directory (expand-file-name "."))
    (visit-tags-table
     (expand-file-name "manual/etags/ETAGS.good_1" etags-tests--test-dir))
    ;; Check that tags in ETAGS.good_1 are recognized.
    (setq xref-buf (xref-find-definitions "LL_Task_Procedure_Access/t"))
    (should (bufferp xref-buf))
    (kill-buffer xref-buf)
    (setq xref-buf (xref-find-definitions "PrintAdd"))
    (should (bufferp xref-buf))
    (kill-buffer xref-buf)
    ;; Check that tags not in ETAGS.good_1, but in ETAGS.good_3, are
    ;; NOT recognized.
    (should-error (xref-find-definitions "intNumber") :type 'user-error)
    (kill-buffer xref-buf)
    (set-buffer buf-with-local-tags)
    (setq default-directory (expand-file-name "."))
    (let (his-masters-voice)
      (visit-tags-table
       (expand-file-name "manual/etags/ETAGS.good_3" etags-tests--test-dir)
       t))
    ;; Check that tags in ETAGS.good_1 are recognized.
    (setq xref-buf (xref-find-definitions "LL_Task_Procedure_Access/t"))
    (should (bufferp xref-buf))
    (kill-buffer xref-buf)
    (setq xref-buf (xref-find-definitions "PrintAdd"))
    (should (bufferp xref-buf))
    (kill-buffer xref-buf)
    ;; Check that tags in ETAGS.good_3 are recognized.  This is a test
    ;; for bug#158.
    (setq xref-buf (xref-find-definitions "intNumber"))
    (should (or (null xref-buf)
                (bufferp xref-buf)))
    ;; Bug #17326
    (should (string= (file-name-nondirectory
                      (buffer-local-value 'tags-file-name buf-with-local-tags))
                     "ETAGS.good_3"))
    (should (string= (file-name-nondirectory
                      (default-value 'tags-file-name))
                     "ETAGS.good_1"))
    (if (bufferp xref-buf) (kill-buffer xref-buf))))

(ert-deftest etags-bug-23164 ()
  "Test that setting a local value of tags table doesn't signal errors."
  (set-buffer (get-buffer-create "*foobar*"))
  (fundamental-mode)
  (visit-tags-table
   (expand-file-name "manual/etags/ETAGS.good_3" etags-tests--test-dir)
   t)
  (should (equal (should-error (xref-find-definitions "foobar123"))
                 '(user-error "No definitions found for: foobar123"))))

(ert-deftest etags-buffer-local-tags-table-list ()
  "Test that a buffer-local value of `tags-table-list' is used."
  (let ((file (make-temp-file "etag-test-tmpfile")))
    (unwind-protect
        (progn
          (set-buffer (find-file-noselect file))
          (fundamental-mode)
          (setq-local tags-table-list
                      (list (expand-file-name "manual/etags/ETAGS.good_3"
                                              etags-tests--test-dir)))
          (cl-letf ((tag-tables tags-table-list)
                    (tags-file-name nil)
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _)
                       (error "We should not prompt the user"))))
            (should (visit-tags-table-buffer))
            (should (equal tags-file-name (car tag-tables)))))
      (delete-file file))))
