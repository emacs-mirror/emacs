;;; vc-bzr.el --- tests for vc/vc-bzr.el

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Glenn Morris <rgm@gnu.org>
;; Maintainer: emacs-devel@gnu.org

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
(require 'vc-bzr)
(require 'vc-dir)

(ert-deftest vc-bzr-test-bug9726 ()
  "Test for https://debbugs.gnu.org/9726 ."
  (skip-unless (executable-find vc-bzr-program))
  ;; Bzr wants to access HOME, e.g. to write ~/.bzr.log.
  ;; This is a problem on hydra, where HOME is non-existent.
  ;; You can disable logging with BZR_LOG=/dev/null, but then some
  ;; commands (eg `bzr status') want to access ~/.bazaar, and will
  ;; abort if they cannot.  I could not figure out how to stop bzr
  ;; doing that, so just give it a temporary homedir for the duration.
  ;; http://bugs.launchpad.net/bzr/+bug/137407 ?
  (let* ((homedir (make-temp-file "vc-bzr-test" t))
         (bzrdir (expand-file-name "bzr" homedir))
         (ignored-dir (progn
                        (make-directory bzrdir)
                        (expand-file-name "ignored-dir" bzrdir)))
         (default-directory (file-name-as-directory bzrdir))
         (process-environment (cons (format "BZR_HOME=%s" homedir)
                                    process-environment)))
    (unwind-protect
        (progn
          (make-directory ignored-dir)
          (with-temp-buffer
            (insert (file-name-nondirectory ignored-dir))
            (write-region nil nil (expand-file-name ".bzrignore" bzrdir)
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
      (delete-directory homedir t))))

;; Not specific to bzr.
(ert-deftest vc-bzr-test-bug9781 ()
  "Test for https://debbugs.gnu.org/9781 ."
  (skip-unless (executable-find vc-bzr-program))
  (let* ((homedir (make-temp-file "vc-bzr-test" t))
         (bzrdir (expand-file-name "bzr" homedir))
         (subdir (progn
                   (make-directory bzrdir)
                   (expand-file-name "subdir" bzrdir)))
         (file (expand-file-name "file" bzrdir))
         (default-directory (file-name-as-directory bzrdir))
         (process-environment (cons (format "BZR_HOME=%s" homedir)
                                    process-environment)))
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
          (vc-dir bzrdir)
          (while (vc-dir-busy)
            (sit-for 0.1))
          (vc-dir-mark-all-files t)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (vc-next-action nil))
          (should (get-buffer "*vc-log*")))
      (delete-directory homedir t))))

;; https://lists.gnu.org/r/help-gnu-emacs/2012-04/msg00145.html
(ert-deftest vc-bzr-test-faulty-bzr-autoloads ()
  "Test we can generate autoloads in a bzr directory when bzr is faulty."
  (skip-unless (executable-find vc-bzr-program))
  (let* ((homedir (make-temp-file "vc-bzr-test" t))
         (bzrdir (expand-file-name "bzr" homedir))
         (file (progn
                 (make-directory bzrdir)
                 (expand-file-name "foo.el" bzrdir)))
         (default-directory (file-name-as-directory bzrdir))
         (generated-autoload-file (expand-file-name "loaddefs.el" bzrdir))
         (process-environment (cons (format "BZR_HOME=%s" homedir)
                                    process-environment)))
    (unwind-protect
        (progn
          (call-process vc-bzr-program nil nil nil "init")
          (with-temp-buffer
            (insert ";;;###autoload
\(defun foo () \"foo\" (interactive) (message \"foo!\"))")
            (write-region nil nil file nil 'silent))
          (call-process vc-bzr-program nil nil nil "add")
          (call-process vc-bzr-program nil nil nil "commit" "-m" "Commit 1")
          ;; Deleting dirstate ensures both that vc-bzr's status heuristic
          ;; fails, so it has to call the external bzr status, and
          ;; causes bzr status to fail.  This simulates a broken bzr
          ;; installation.
          (delete-file ".bzr/checkout/dirstate")
          (should (progn (update-directory-autoloads default-directory)
                         t)))
      (delete-directory homedir t))))

;;; vc-bzr.el ends here
