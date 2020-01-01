;;; ediff-ptch-tests.el --- Tests for ediff-ptch.el

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'ediff-ptch)

(ert-deftest ediff-ptch-test-bug25010 ()
  "Test for https://debbugs.gnu.org/25010 ."
  (with-temp-buffer
    (insert "diff --git a/lisp/vc/ediff-ptch.el b/lisp/vc/ediff-ptch.el
index 6a07f80..6e8e947 100644
--- a/lisp/vc/ediff-ptch.el
+++ b/lisp/vc/ediff-ptch.el
@@ -120,11 +120,12 @@ ediff-patch-default-directory
")
    (goto-char (point-min))
    (let ((filename
           (progn
             (re-search-forward ediff-context-diff-label-regexp nil t)
             (match-string 1))))
      (should-not (string-suffix-p "@@" filename)))))


(ert-deftest ediff-ptch-test-bug26084 ()
  "Test for https://debbugs.gnu.org/26084 ."
  (skip-unless (executable-find "git"))
  (skip-unless (executable-find ediff-patch-program))
  (let* ((tmpdir (make-temp-file "ediff-ptch-test" t))
         (default-directory (file-name-as-directory tmpdir))
         (patch (make-temp-file "ediff-ptch-test"))
         (qux (expand-file-name "qux.txt" tmpdir))
         (bar (expand-file-name "bar.txt" tmpdir))
         (git-program (executable-find "git")))
    ;; Create repository.
    (with-temp-buffer
      (insert "qux here\n")
      (write-region nil nil qux nil 'silent)
      (erase-buffer)
      (insert "bar here\n")
      (write-region nil nil bar nil 'silent))
    (call-process git-program nil nil nil "init")
    (call-process git-program nil nil nil "add" ".")
    (call-process git-program nil nil nil "commit" "-m" "Test repository.")
    ;; Update repo., save the diff and reset to initial state.
    (with-temp-buffer
      (insert "foo here\n")
      (write-region nil nil qux nil 'silent)
      (write-region nil nil bar nil 'silent))
    (call-process git-program nil `(:file ,patch) nil "diff")
    (call-process git-program nil nil nil "reset" "--hard" "HEAD")
    ;; Visit the diff file i.e., patch; extract from it the parts
    ;; affecting just each of the files: store in patch-bar the part
    ;; affecting 'bar', and in patch-qux the part affecting 'qux'.
    (find-file patch)
    (unwind-protect
        (let* ((info
                (progn (ediff-map-patch-buffer (current-buffer)) ediff-patch-map))
               (patch-bar
                (buffer-substring-no-properties
                 (car (nth 3 (car info)))
                 (car (nth 4 (car info)))))
               (patch-qux
                (buffer-substring-no-properties
                 (car (nth 3 (cadr info)))
                 (car (nth 4 (cadr info))))))
          ;; Apply both patches.
          (dolist (x (list (cons patch-bar bar) (cons patch-qux qux)))
            (with-temp-buffer
              ;; Some windows variants require the option '--binary'
              ;; in order to 'patch' create backup files.
              (let ((opts (format "--backup%s"
                                  (if (memq system-type '(windows-nt ms-dos))
                                      " --binary" ""))))
                (insert (car x))
                (call-process-region (point-min)
                                     (point-max)
                                     ediff-patch-program
                                     nil nil nil
                                     opts (cdr x)))))
          ;; Check backup files were saved correctly; in Bug#26084 some
          ;; of the backup files are overwritten with the actual content
          ;; of the updated file.  To ensure that the bug is fixed we just
          ;; need to check that every backup file produced has different
          ;; content that the current updated file.
          (dolist (x (list qux bar))
            (let ((backup
                   (car
                    (directory-files
                     tmpdir 'full
                     (concat (file-name-nondirectory x) ".")))))
              ;; Compare files only if the backup has being created.
              (when backup
                (should-not
                 (string= (with-temp-buffer
                            (insert-file-contents x)
                            (buffer-string))
                          (with-temp-buffer
                            (insert-file-contents backup)
                            (buffer-string)))))))
          (delete-directory tmpdir 'recursive)
          (delete-file patch)))))


(provide 'ediff-ptch-tests)
;;; ediff-ptch-tests.el ends here
