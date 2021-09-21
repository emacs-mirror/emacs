;;; dired-aux-tests.el --- Test suite for dired-aux. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021 Free Software Foundation, Inc.

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
(require 'dired-aux)
(eval-when-compile (require 'cl-lib))

(ert-deftest dired-test-bug27496 ()
  "Test for https://debbugs.gnu.org/27496 ."
  (skip-unless (executable-find shell-file-name))
  (let* ((foo (make-temp-file "foo"))
         (files (list foo)))
    (unwind-protect
        (cl-letf (((symbol-function 'read-char-from-minibuffer) 'error))
          (dired temporary-file-directory)
          (dired-goto-file foo)
          ;; `dired-do-shell-command' returns nil on success.
          (should-error (dired-do-shell-command "ls ? ./?" nil files))
          (should-error (dired-do-shell-command "ls ./? ?" nil files))
          (should-not (dired-do-shell-command "ls ? ?" nil files))
          (should-error (dired-do-shell-command "ls * ./*" nil files))
          (should-not (dired-do-shell-command "ls * *" nil files))
          (should-not (dired-do-shell-command "ls ? ./`?`" nil files)))
      (delete-file foo))))

;; Auxiliary macro for `dired-test-bug28834': it binds
;; `dired-create-destination-dirs' to CREATE-DIRS and execute BODY.
;; If YES-OR-NO is non-nil, it binds `yes-or-no-p' to
;; to avoid the prompt.
(defmacro with-dired-bug28834-test (create-dirs yes-or-no &rest body)
  (declare (debug (form symbolp body)))
  (let ((foo (make-symbol "foo")))
    `(let* ((,foo (make-temp-file "foo" 'dir))
            (dired-create-destination-dirs ,create-dirs))
       (setq from (make-temp-file "from"))
       (setq to-cp
             (expand-file-name
              "foo-cp" (file-name-as-directory (expand-file-name "bar" ,foo))))
       (setq to-mv
             (expand-file-name
              "foo-mv" (file-name-as-directory (expand-file-name "qux" ,foo))))
       (unwind-protect
           (if ,yes-or-no
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (_prompt) (eq ,yes-or-no 'yes))))
                 ,@body)
             ,@body)
         ;; clean up
         (delete-directory ,foo 'recursive)
         (delete-file from)))))

(ert-deftest dired-test-bug28834 ()
  "test for https://debbugs.gnu.org/28834 ."
  (let (from to-cp to-mv)
    ;; `dired-create-destination-dirs' set to 'always.
    (with-dired-bug28834-test
     'always nil
     (dired-copy-file-recursive from to-cp nil)
     (should (file-exists-p to-cp))
     (dired-rename-file from to-mv nil)
     (should (file-exists-p to-mv)))
    ;; `dired-create-destination-dirs' set to nil.
    (with-dired-bug28834-test
     nil nil
     (should-error (dired-copy-file-recursive from to-cp nil))
     (should-error (dired-rename-file from to-mv nil)))
    ;; `dired-create-destination-dirs' set to 'ask.
    (with-dired-bug28834-test
     'ask 'yes ; Answer `yes'
     (dired-copy-file-recursive from to-cp nil)
     (should (file-exists-p to-cp))
     (dired-rename-file from to-mv nil)
     (should (file-exists-p to-mv)))
    (with-dired-bug28834-test
     'ask 'no ; Answer `no'
     (should-error (dired-copy-file-recursive from to-cp nil))
     (should-error (dired-rename-file from to-mv nil)))))

(ert-deftest dired-test-bug30624 ()
  "test for https://debbugs.gnu.org/30624 ."
  (cl-letf* ((target-dir (make-temp-file "target" 'dir))
             ((symbol-function 'dired-mark-read-file-name)
              (lambda (&rest _) target-dir))
             (inhibit-message t))
    ;; Delete target-dir: `dired-do-create-files' must recreate it.
    (delete-directory target-dir)
    (let ((file1 (make-temp-file "bug30624_file1"))
          (file2 (make-temp-file "bug30624_file2"))
          (dired-create-destination-dirs 'always)
          (buf (dired temporary-file-directory)))
      (unwind-protect
          (progn
            (dired-revert)
            (dired-mark-files-regexp "bug30624_file")
            (should (dired-do-create-files 'copy 'dired-copy-file "Copy" nil)))
        (delete-directory target-dir 'recursive)
        (mapc #'delete-file `(,file1 ,file2))
        (kill-buffer buf)))))

(defun dired-test--check-highlighting (command positions)
  (let ((start 1))
    (dolist (pos positions)
      (should-not (text-property-not-all start (1- pos) 'face nil command))
      (should (equal 'warning (get-text-property pos 'face command)))
      (setq start (1+ pos)))
    (should-not (text-property-not-all
                 start (length command) 'face nil command))))

(ert-deftest dired-test-highlight-metachar ()
  "Check that non-isolated meta-characters are highlighted."
  (let* ((command "sed -r -e 's/oo?/a/' -e 's/oo?/a/' ? `?`")
         (markers "               ^             ^")
         (result (dired--highlight-no-subst-chars
                  (dired--need-confirm-positions command "?")
                  command
                  t))
         (lines (split-string result "\n")))
    (should (= (length lines) 2))
    (should (string-match (regexp-quote command) (nth 0 lines)))
    (should (string-match (regexp-quote markers) (nth 1 lines)))
    (dired-test--check-highlighting (nth 0 lines) '(15 29)))
  ;; Note that `?` is considered isolated, but `*` is not.
  (let* ((command "sed -e 's/o*/a/' -e 's/o`*` /a/'")
         (markers "           ^             ^")
         (result (dired--highlight-no-subst-chars
                  (dired--need-confirm-positions command "*")
                  command
                  t))
         (lines (split-string result "\n")))
    (should (= (length lines) 2))
    (should (string-match (regexp-quote command) (nth 0 lines)))
    (should (string-match (regexp-quote markers) (nth 1 lines)))
    (dired-test--check-highlighting (nth 0 lines) '(11 25)))
  (let* ((command "sed 's/\\?/!/'")
         (result (dired--highlight-no-subst-chars
                  (dired--need-confirm-positions command "?")
                  command
                  nil))
         (lines (split-string result "\n")))
    (should (= (length lines) 1))
    (should (string-match (regexp-quote command) (nth 0 lines)))
    (dired-test--check-highlighting (nth 0 lines) '(8))))

(ert-deftest dired-test-bug47058-tar ()
  "test for https://debbugs.gnu.org/47058 ."
  (skip-unless (and (executable-find "tar")
                    (executable-find "gzip")))
  (dired-test-bug47058-fn "tar -cf - %i | gzip -c9 > %o"
                           "gzip -dc %i | tar -xf - -C %c"
                           ".tar.gz"))

(ert-deftest dired-test-bug47058-zip ()
  "test for https://debbugs.gnu.org/47058 ."
  (skip-unless (and (executable-find "zip")
                    (executable-find "unzip")))
  (dired-test-bug47058-fn "zip %o -r --filesync %i"
                           "unzip -o -d %o %i"
                           ".zip"))

(defun dired-test-bug47058-fn (compress-cmd uncompress-cmd extension)
  "helper fn for testing https://debbugs.gnu.org/47058 ."
  (let* ((base-file (make-temp-file "dired-test-47058-"))
         (archive-file (concat base-file extension))
         (file1 (make-temp-file "a"))
         (file2 (make-temp-file "b"))
         (file3 (make-temp-file "c"))
         (filelist (list file1 file2 file3))
         (comprcmd (replace-regexp-in-string
                  "%c" (shell-quote-argument temporary-file-directory)
                  (replace-regexp-in-string
                   "%i" (mapconcat 'identity filelist " ")
                   (replace-regexp-in-string
                    "%o" (shell-quote-argument archive-file)
                    compress-cmd)))))
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _) base-file)))
      (dired-delete-file base-file)
      (should-not (file-exists-p base-file))
      (should-not (file-exists-p archive-file))
      (dired-shell-command comprcmd)
      (should (file-exists-p archive-file))
      (mapcar (lambda (f) (should (file-exists-p f)))
              filelist)
      (mapcar (lambda (f) (delete-file f))
              filelist)
      (mapcar (lambda (f) (should-not (file-exists-p f)))
              filelist)
      (should (string-equal
               (dired-uncompress-file archive-file
                                      base-file
                                      uncompress-cmd)
               (file-name-as-directory base-file)))
      (mapcar (lambda (f)
                (should (file-exists-p
                         (concat (file-name-as-directory base-file) f))))
              filelist)
      (dired-delete-file base-file 'always' nil)
      (dired-delete-file archive-file 'always' nil)
      (should-not (file-exists-p base-file))
      (should-not (file-exists-p archive-file)))))

(provide 'dired-aux-tests)
;; dired-aux-tests.el ends here
