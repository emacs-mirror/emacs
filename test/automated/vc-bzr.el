;;; vc-bzr.el --- tests for vc/vc-bzr.el

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Glenn Morris <rgm@gnu.org>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'vc-bzr)
(require 'vc-dir)

;; FIXME it would be better to skip all these tests if there is no
;; bzr installed.  We could just put everything inside an IF
;; statement, but it would be nice if ERT had a "skipped" facility (?).

(ert-deftest vc-bzr-test-bug9726 ()
  "Test for http://debbugs.gnu.org/9726 ."
  :expected-result (if (executable-find vc-bzr-program) :passed :failed)
  (should (executable-find vc-bzr-program))
  (let* ((tempdir (make-temp-file "vc-bzr-test" t))
         (ignored-dir (expand-file-name "ignored-dir" tempdir))
         (default-directory (file-name-as-directory tempdir)))
    (unwind-protect
        (progn
          (make-directory ignored-dir)
          (with-temp-buffer
            (insert (file-name-nondirectory ignored-dir))
            (write-region nil nil (expand-file-name ".bzrignore" tempdir)
                          nil 'silent))
          (call-process vc-bzr-program nil nil nil "init")
          (call-process vc-bzr-program nil nil nil "add")
          (call-process vc-bzr-program nil nil nil "commit" "-m" "Commit 1")
          (with-temp-buffer
            (insert "unregistered file")
            (write-region nil nil (expand-file-name "testfile2" ignored-dir)
                          nil 'silent))
          (vc-dir ignored-dir)
          (while (vc-dir-busy)
            (sit-for 0.1))
          ;; FIXME better to explicitly test for error from process sentinel.
          (with-current-buffer "*vc-dir*"
            (goto-char (point-min))
            (should (search-forward "unregistered" nil t))))
      (delete-directory tempdir t))))

;; Not specific to bzr.
(ert-deftest vc-bzr-test-bug9781 ()
  "Test for http://debbugs.gnu.org/9781 ."
  :expected-result (if (executable-find vc-bzr-program) :passed :failed)
  (should (executable-find vc-bzr-program))
  (let* ((tempdir (make-temp-file "vc-bzr-test" t))
         (subdir (expand-file-name "subdir" tempdir))
         (file (expand-file-name "file" tempdir))
         (default-directory (file-name-as-directory tempdir)))
    (unwind-protect
        (progn
          (call-process vc-bzr-program nil nil nil "init")
          (make-directory subdir)
          (with-temp-buffer
            (insert "text")
            (write-region nil nil file nil 'silent)
            (write-region nil nil (expand-file-name "subfile" subdir)
                          nil 'silent))
          (call-process vc-bzr-program nil nil nil "add")
          (call-process vc-bzr-program nil nil nil "commit" "-m" "Commit 1")
          (call-process vc-bzr-program nil nil nil "remove" subdir)
          (with-temp-buffer
            (insert "different text")
            (write-region nil nil file nil 'silent))
          (vc-dir tempdir)
          (while (vc-dir-busy)
            (sit-for 0.1))
          (vc-dir-mark-all-files t)
          (let ((f (symbol-function 'y-or-n-p)))
            (unwind-protect
                (progn
                  (fset 'y-or-n-p (lambda (prompt) t))
                  (vc-next-action nil))
              (fset 'y-or-n-p f)))
          (should (get-buffer "*vc-log*")))
      (delete-directory tempdir t))))

;;; vc-bzr.el ends here
