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

(provide 'vc-test-misc)
;;; vc-test-misc.el ends here
