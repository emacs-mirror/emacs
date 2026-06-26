;;; vc-test-misc.el --- backend-agnostic VC tests  -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>

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

(require 'ert-x)
(require 'vc)
(require 'vc-git)
(require 'vc-dir)
(require 'log-edit)

(require 'vc-tests-helpers
         (ert-resource-file "vc-tests-helpers"))

(defvar vc-hg-global-switches)

(ert-deftest vc-test-buffer-sync-fileset ()
  "Test `vc-buffer-sync-fileset'."
  (cl-flet ((test-it (&rest args)
              (let (buffers)
                (cl-letf (((symbol-function 'vc-buffer-sync)
                           (lambda (&rest _)
                             (push (current-buffer) buffers))))
                  (apply #'vc-buffer-sync-fileset args)
                  (sort buffers)))))
    (ert-with-temp-directory temp
      (let* ((default-directory temp)
             (present (find-file-noselect "present"))
             (missing (find-file-noselect "missing"))
             (only-present (list present))
             (only-missing (list missing))
             (missing+present (list missing present)))
        (with-current-buffer present (basic-save-buffer))
        (with-temp-file "unvisited")
        ;; Regular behavior for files.
        (should (equal (test-it `(Git ("missing")))
                       only-missing))
        (should (equal (test-it `(Git ("present" "missing")))
                       missing+present))
        ;; Regular behavior for directories.
        (should (equal (test-it `(Git (,temp)))
                       only-present))
        ;; Two ways to override regular behavior for directories.
        (should (equal (test-it `(Git (,temp)) nil t)
                       missing+present))
        (should (equal (test-it `(Git (,temp "missing")))
                       missing+present))
        ;; Doesn't sync PRESENT twice.
        (should (equal (test-it `(Git ("present" ,temp)))
                       only-present))
        (should (equal (test-it `(Git ("missing" ,temp "present")))
                       missing+present))))))

(defmacro vc-test--exec-after-wait ()
  '(progn
     (while (process-live-p proc)
       (when (input-pending-p)
         (discard-input))
       (should (memq success '(nil ignore)))
       (sit-for 0.05))
     (sit-for 0.05)))

(ert-deftest vc-test-exec-after-1 ()
  "Test `vc-exec-after' adding a sentinel."
  (with-temp-buffer
    (let ((proc (start-process-shell-command "test" (current-buffer)
                                             (if (eq system-type 'windows-nt)
                                                 "sleep 1 & echo hello"
                                               "sleep 0.2; echo hello")))
          success)
      (vc-exec-after (lambda () (setq success t)))
      (should-not (eq (process-sentinel proc)
                      #'internal-default-process-sentinel))
      (vc-test--exec-after-wait)
      (should success))))

(ert-deftest vc-test-exec-after-2 ()
  "Test `vc-exec-after' executing the code immediately."
  (with-temp-buffer
    (let ((proc (start-process-shell-command "test" (current-buffer)
                                             (if (eq system-type 'windows-nt)
                                                 "sleep 1 & echo hello"
                                               "sleep 0.2; echo hello")))
          success)
      (vc-test--exec-after-wait)
      (vc-exec-after (lambda () (setq success t)))
      (should (eq (process-sentinel proc)
                  #'internal-default-process-sentinel))
      (should success))))

(ert-deftest vc-test-exec-after-3 ()
  "Test OKSTATUS argument to `vc-exec-after'."
  (with-temp-buffer
    (let ((proc (start-process-shell-command "test" (current-buffer) "true"))
          success)
      (vc-exec-after (lambda () (setq success t)) 0)
      (vc-test--exec-after-wait)
      (should success)))

  (with-temp-buffer
    (let ((proc (start-process-shell-command "test" (current-buffer) "false"))
          success)
      (vc-exec-after (lambda () (setq success t)) 0)
      (vc-test--exec-after-wait)
      (should-not success))))

(ert-deftest vc-test-exec-after-4 ()
  "Test `vc-exec-after' handling the process mark."
  (with-temp-buffer
    (let ((proc (start-process-shell-command "test" (current-buffer)
                                             (if (eq system-type 'windows-nt)
                                                 "echo hello there & sleep 1"
                                               "echo hello there; sleep 0.2")))
          (success 'ignore))
      ;; Disable the default output, which further moves point.
      (set-process-sentinel proc #'ignore)

      (vc-exec-after (lambda ()
                       (goto-char (point-min))
                       (should (looking-at "hello"))))
      (vc-exec-after (lambda ()
                       (forward-word 1)
                       (should (looking-at " there"))))
      (accept-process-output proc)
      (let ((opoint (point)))
        (vc-test--exec-after-wait)
        (should (eq (point) opoint))))))

(defvar vc-sentinel-movepoint)

(ert-deftest vc-test-exec-after-5 ()
  "Test `vc-exec-after' with `vc-sentinel-movepoint' variable."
  (with-temp-buffer
    (let ((proc (start-process-shell-command "test" (current-buffer)
                                             (if (eq system-type 'windows-nt)
                                                 "echo hello there & sleep 1"
                                               "echo hello there; sleep 0.2")))
          (success 'ignore))
      ;; Disable the default output, which further moves point.
      (set-process-sentinel proc #'ignore)

      (vc-exec-after (lambda () (setq vc-sentinel-movepoint (point-min))))
      (accept-process-output proc)
      (should-not (eq (point) (point-min)))
      (vc-test--exec-after-wait)
      (should (eq (point) (point-min))))))

(ert-deftest vc-test-do-command-1 ()
  "Test `vc-run-command' synchronous, discarding stderr."
  (with-temp-buffer
    (vc-do-command '(t nil) 0 "sh" nil "-c" "echo foo; echo >&2 bar")
    (should (equal (buffer-string) "foo\n"))))

(ert-deftest vc-test-do-command-2 ()
  "Test `vc-run-command' synchronous, keeping stderr."
  (with-temp-buffer
    (vc-do-command t 0 "sh" nil "-c" "echo foo; echo >&2 bar")
    (goto-char (point-min))
    (should (save-excursion (re-search-forward "foo" nil t)))
    (should (save-excursion (re-search-forward "bar" nil t)))))

(ert-deftest vc-test-do-command-3 ()
  "Test `vc-run-command' synchronous, discarding both."
  (with-temp-buffer
    (vc-do-command '(nil t) 0 "sh" nil "-c" "echo foo; echo >&2 bar")
    (should (bobp))))

(ert-deftest vc-test-do-command-4 ()
  "Test `vc-run-command' asynchronous, discarding stderr."
  (with-temp-buffer
    (let ((proc (vc-do-command '(t nil) 'async "sh" nil
                               "-c" "echo foo; echo >&2 bar"))
          (success 'ignore))
      (vc-test--exec-after-wait)
      (should (equal (buffer-string) "foo\n")))))

(ert-deftest vc-test-do-command-5 ()
  "Test `vc-run-command' asynchronous, keeping stderr."
  (with-temp-buffer
    (let ((proc (vc-do-command t 'async "sh" nil
                               "-c" "echo foo; echo >&2 bar"))
          (success 'ignore))
      (vc-test--exec-after-wait)
      (goto-char (point-min))
      (should (save-excursion (re-search-forward "foo" nil t)))
      (should (save-excursion (re-search-forward "bar" nil t))))))

(ert-deftest vc-test-do-command-6 ()
  "Test `vc-run-command' asynchronous, discarding both."
  (with-temp-buffer
    (let ((proc (vc-do-command '(nil t) 'async "sh" nil
                               "-c" "echo foo; echo >&2 bar"))
          (success 'ignore))
      (vc-test--exec-after-wait)
      (should (bobp)))))

(ert-deftest vc-test-do-command-7 ()
  "Test `vc-run-command' setting up the buffer."
  (let ((buf (generate-new-buffer " *temp*" t)))
    (unwind-protect
        (progn
          (vc-do-command (list buf nil) 0 "sh" nil
                         "-c" "echo foo; echo >&2 bar")
          (with-current-buffer buf
            (should (equal (buffer-string) "foo\n"))))
      (kill-buffer buf))))

(ert-deftest vc-test-match-branch-name-regexps ()
  "Test `vc--match-branch-name-regexps'."
  (let ((vc-trunk-branch-regexps '("master" "main")))
    (let ((vc-topic-branch-regexps '("m.*")))
      (should-error (vc--match-branch-name-regexps "master")))
    (let ((vc-topic-branch-regexps '("f" "o")))
      (should (eq (vc--match-branch-name-regexps "master") 'trunk))
      (should (null (vc--match-branch-name-regexps "foo"))))
    (let ((vc-topic-branch-regexps '("f.*" "o")))
      (should (eq (vc--match-branch-name-regexps "master") 'trunk))
      (should (eq (vc--match-branch-name-regexps "foo") 'topic)))
    (let (vc-topic-branch-regexps)
      (should (eq (vc--match-branch-name-regexps "master") 'trunk))
      (should (null (vc--match-branch-name-regexps "foo"))))
    (let ((vc-topic-branch-regexps t))
      (should (eq (vc--match-branch-name-regexps "master") 'trunk))
      (should (eq (vc--match-branch-name-regexps "foo") 'topic))))
  (let ((vc-trunk-branch-regexps '(not "master")))
    (let (vc-topic-branch-regexps)
      (should (null (vc--match-branch-name-regexps "master")))
      (should (eq (vc--match-branch-name-regexps "foo") 'trunk)))
    (let ((vc-topic-branch-regexps t))
      (should (eq (vc--match-branch-name-regexps "master") 'topic))
      (should (eq (vc--match-branch-name-regexps "foo") 'trunk)))))

(ert-deftest vc-test-vc-dir-on-symlink ()
  "Test VC-Dir on a symlink to a repository.
See bug#80803 and bug#80967."
  ;; Git for Windows could fail in a symlinked tree.
  (skip-when (eq system-type 'windows-nt))
  (skip-unless (executable-find vc-git-program))
  (vc-test--with-author-identity 'Git
    (let ((vc-handled-backends '(Git)))
      (ert-with-temp-directory tempdir
        (let* ((default-directory tempdir)
               (src (expand-file-name "src/" tempdir))
               (dest (expand-file-name "dest/" tempdir))
               (file (expand-file-name "foo" dest))
               file-buf truename-dir symlink-dir)
          (make-directory dest)
          (let ((default-directory dest)
                vc-async-checkin)
            (vc-test--create-repo-function 'Git)
            (write-region "foo\n" nil file nil 'nomessage)
            (with-current-buffer (setq file-buf (find-file-noselect file))
              (vc-register `(Git (,file)))
              (vc-checkin (list file) 'Git)
              (insert "Initial commit")
              (let (vc-async-checkin)
                (log-edit-done))))
          (make-symbolic-link dest src)
          ;; Emulate an interactive call to `vc-dir'.
          (vc-dir (file-truename src) 'Git)
          (while (vc-dir-busy) (sit-for 0.05))
          (should (equal default-directory dest))
          (setq truename-dir (current-buffer))
          ;; Now a `vc-dir' pointed at the symlink, which is unlike an
          ;; interactive call to `vc-dir'.
          (vc-dir src 'Git)
          (while (vc-dir-busy) (sit-for 0.05))
          (should (equal default-directory src))
          (setq symlink-dir (current-buffer))
          (with-current-buffer file-buf
            (insert "bar")
            (basic-save-buffer))
          (dolist (buf (list truename-dir symlink-dir))
            (with-current-buffer buf
              (let ((data (ewoc-data (ewoc-nth vc-ewoc 1))))
                (should (equal (vc-dir-fileinfo->name data)
                               (file-name-nondirectory file)))
                (should (equal (vc-dir-fileinfo->state data)
                               'edited))))))))))

(ert-deftest vc-test-vc-dir-next/previous () ; bug#81248
  "Test navigating with `vc-dir-{next,previous}-{line,directory}'."
  (skip-unless (executable-find vc-git-program))
  (vc-test--with-author-identity 'Git
    (let ((vc-handled-backends '(Git)))
      (ert-with-temp-directory tempdir
        (let ((default-directory tempdir)
              (n 0)
              vc-dir-buf)
          (vc-test--create-repo-function 'Git)
          (dolist (file '("file01" "dir1/file11"))
            (make-empty-file file t))
          (vc-dir default-directory 'Git)
          (while (vc-dir-busy) (sit-for 0.05))
          (setq vc-dir-buf (current-buffer))
          (should (bobp))
          (while (vc-dir--before-dotname-p)
            (vc-dir-next-line 1)
            (should (and (looking-at "\\./$") (looking-back "^ +" (pos-bol))))
            (incf n 1)
            (goto-char (point-min))
            (forward-line n))
          (vc-dir-next-line 1)
          (should (looking-at "file01$"))
          (vc-dir-next-line 1)
          (should (looking-at "dir1/$"))
          (vc-dir-next-line 1)
          (should (looking-at "dir1/file11$"))
          (vc-dir-next-line 1)
          (should (looking-at "^$"))
          (let ((end (point)))
            (vc-dir-next-line 1)
            (should (equal (point) end)))
          (goto-char (point-min))
          (vc-dir-next-directory)
          (should (and (looking-at "\\./$") (looking-back "^ +" (pos-bol))))
          (vc-dir-next-directory)
          (should (and (looking-at "dir1/$") (looking-back "^ +" (pos-bol))))
          (vc-dir-next-directory)
          (should (and (looking-at "dir1/$") (looking-back "^ +" (pos-bol))))
          (goto-char (point-max))
          (vc-dir-previous-line 1)
          (should (looking-at "dir1/file11$"))
          (vc-dir-previous-line 1)
          (should (and (looking-at "dir1/$") (looking-back "^ +" (pos-bol))))
          (vc-dir-previous-line 1)
          (should (looking-at "file01$"))
          (vc-dir-previous-line 1)
          (should (and (looking-at "\\./$") (looking-back "^ +" (pos-bol))))
          (vc-dir-previous-line 1)
          (should (and (looking-at "\\./$") (looking-back "^ +" (pos-bol))))
          (goto-char (point-max))
          (vc-dir-previous-directory)
          (should (and (looking-at "dir1/$") (looking-back "^ +" (pos-bol))))
          (vc-dir-previous-directory)
          (should (and (looking-at "\\./$") (looking-back "^ +" (pos-bol))))
          (vc-dir-previous-directory)
          (should (and (looking-at "\\./$") (looking-back "^ +" (pos-bol))))
          (kill-buffer vc-dir-buf))))))

(ert-deftest vc-test-vc-dir-mark/unmark-all-dir-entry () ; bug#81249
  "Test `vc-dir-{un}mark-all' called on directory entry."
  (skip-unless (executable-find vc-git-program))
  (vc-test--with-author-identity 'Git
    (let ((vc-handled-backends '(Git)))
      (ert-with-temp-directory tempdir
        (let ((default-directory tempdir)
              (files '("file01" "file02" "dir1/file11" "dir1/file12"
                       "dir2/file21" "dir2/file22"))
              vc-dir-buf
              dir-children)
          (vc-test--create-repo-function 'Git)
          (dolist (file files)
            (make-empty-file file t))
          (vc-dir default-directory 'Git)
          (while (vc-dir-busy) (sit-for 0.05))
          (setq vc-dir-buf (current-buffer))
          ;; Put point on line of "./" entry.
          (goto-char (ewoc-location (ewoc-nth vc-ewoc 0)))
          ;; Cumulatively mark file entries directory-wise.
          (let ((next t))
            (while next
             (vc-dir-mark-all-files nil)
             ;; Can't use this to set dir-children because it returns
             ;; all files below directory entry, so loop over file
             ;; entries until next directory entry.
             ;; (vc-dir-find-child-files
             ;;  (expand-file-name
             ;;   (vc-dir-fileinfo->name
             ;;    (ewoc-data (ewoc-locate vc-ewoc)))))
             (catch 'done
               (while t
                 (vc-dir-next-line 1)
                 (cond ((vc-dir-fileinfo->directory
                          (ewoc-data (ewoc-locate vc-ewoc)))
                        (throw 'done nil))
                         ;; After last entry.
                       ((looking-at "^$")
                        (throw 'done (setq next nil)))
                       (t
                        (push (expand-file-name
                               (vc-dir-fileinfo->name
                                (ewoc-data (ewoc-locate vc-ewoc))))
                              dir-children)))))
             (should (seq-set-equal-p (vc-dir-marked-files) dir-children))))
          (goto-char (ewoc-location (ewoc-nth vc-ewoc 0)))
          ;; Cumulatively unmark file entries directory-wise.
          (let ((next t))
            (while next
              (vc-dir-unmark-all-files nil)
              (catch 'done
                (while t
                  (vc-dir-next-line 1)
                  (cond ((vc-dir-fileinfo->directory
                           (ewoc-data (ewoc-locate vc-ewoc)))
                         (throw 'done nil))
                          ;; After last entry.
                        ((looking-at "^$")
                         (throw 'done (setq next nil)))
                        (t
                         (setq dir-children
                               (delete (expand-file-name
                                        (vc-dir-fileinfo->name
                                         (ewoc-data (ewoc-locate vc-ewoc))))
                                       dir-children))))))
              (should (seq-set-equal-p (vc-dir-marked-files) dir-children))))
          (kill-buffer vc-dir-buf))))))

(ert-deftest vc-test-vc-dir-mark-all-with-marked-directory () ; bug#81277
  "Test `vc-dir-mark-all' called on a marked directory entry."
  (skip-unless (executable-find vc-git-program))
  (vc-test--with-author-identity 'Git
    (let ((vc-handled-backends '(Git)))
      (ert-with-temp-directory tempdir
        (let ((default-directory tempdir)
              (files '("dir1/file11" "dir1/file12"))
              vc-dir-buf)
          (vc-test--create-repo-function 'Git)
          (dolist (file files)
            (make-empty-file file t))
          (vc-dir default-directory 'Git)
          (while (vc-dir-busy) (sit-for 0.05))
          (setq vc-dir-buf (current-buffer))
          ;; Move point to "dir1/" entry line.
          (goto-char (ewoc-location (ewoc-nth vc-ewoc 1)))
          ;; Mark "dir1/".
          (vc-dir-mark-file)
          ;; Move back to "dir1/" entry.
          (vc-dir-previous-line 1)
          ;; Test that it's marked.
          (should (vc-dir-fileinfo->marked (ewoc-data (ewoc-locate vc-ewoc))))
          (vc-dir-mark-all-files nil)
          ;; Now it should have been unmarked.
          (should-not
           (vc-dir-fileinfo->marked (ewoc-data (ewoc-locate vc-ewoc))))
          ;; All its children should be marked.
          (let ((dir-children (vc-dir-find-child-files
                               (expand-file-name
                                (vc-dir-fileinfo->name
                                 (ewoc-data (ewoc-nth vc-ewoc 1)))))))
            (should (seq-set-equal-p (vc-dir-marked-files) dir-children)))
          (kill-buffer vc-dir-buf))))))

(defun vc-test--vc-dir-unmark-file ()
  "Execute `vc-dir-unmark-file' assuming \"y\" at `y-or-n-p' prompt."
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (_prompt) t)))
    (vc-dir-unmark-file)))

(ert-deftest vc-test-vc-dir-unmark-file-with-marked-directory () ; bug#81277
  "Test `vc-dir-unmark-file' with a marked ancestor directory."
  (skip-unless (executable-find vc-git-program))
  (vc-test--with-author-identity 'Git
    (let ((vc-handled-backends '(Git)))
      (ert-with-temp-directory tempdir
        (let ((default-directory tempdir)
              (files '("dir1/file11" "dir1/file12"
                       "dir1/dir2/file21" "dir1/dir2/file22"
                       "dir1/dir3/file31" "dir1/dir3/file32"))
              vc-dir-buf unmarked directories)
          (vc-test--create-repo-function 'Git)
          (dolist (file files)
            (make-empty-file file t))
          (vc-dir default-directory 'Git)
          (while (vc-dir-busy) (sit-for 0.05))
          (setq vc-dir-buf (current-buffer))
          ;; Move point to "dir1/" entry line.
          (goto-char (ewoc-location (ewoc-nth vc-ewoc 1)))
          ;; Mark "dir1/".
          (vc-dir-mark-file)
          ;; On next entry simulate invoking `vc-dir-unmark-file' and
          ;; answering "y" to `y-or-n-p' prompt.
          (vc-test--vc-dir-unmark-file)
          ;; Move back to that entry and test that it's unmarked.
          (vc-dir-previous-line 1)
          (should-not
           (vc-dir-fileinfo->marked (ewoc-data (ewoc-locate vc-ewoc))))
          (push (expand-file-name
                 (vc-dir-fileinfo->name
                  (ewoc-data (ewoc-locate vc-ewoc))))
                unmarked)
          ;; All other non-directory descendents of "dir1/" should be
          ;; marked.
          (let* ((dir-children (vc-dir-find-child-files
                                (expand-file-name
                                 (vc-dir-fileinfo->name
                                  (ewoc-data (ewoc-nth vc-ewoc 1))))))
                 (rest-children (seq-difference dir-children unmarked)))
            (should (seq-set-equal-p (vc-dir-marked-files) rest-children)))
          ;; All directory entries should be unmarked.
          (goto-char (point-max))
          (while (not (bobp))
            (vc-dir-previous-directory)
            (push (expand-file-name
                   (vc-dir-fileinfo->name
                    (ewoc-data (ewoc-locate vc-ewoc))))
                  directories)
            (when (equal
                   (vc-dir-fileinfo->name (ewoc-data (ewoc-locate vc-ewoc)))
                   (vc-dir-fileinfo->name (ewoc-data (ewoc-nth vc-ewoc 1))))
              (goto-char (point-min))))
          (should-not (seq-intersection directories (vc-dir-marked-files)))
          (kill-buffer vc-dir-buf))))))

(provide 'vc-test-misc)
;;; vc-test-misc.el ends here
