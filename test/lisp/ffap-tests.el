;;; ffap-tests.el --- Test suite for ffap.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2024 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'ffap)

(ert-deftest ffap-replace-file-component ()
  (should (equal
           (ffap-replace-file-component "/ftp:who@foo.com:/whatever" "/new")
           "/ftp:who@foo.com:/new")))

(ert-deftest ffap-file-remote-p ()
  (dolist (test '(("/user@foo.bar.com:/pub" .
                   "/user@foo.bar.com:/pub")
                  ("/cssun.mathcs.emory.edu://dir" .
                   "/cssun.mathcs.emory.edu:/dir")
                  ("/ffap.el:80" .
                   "/ffap.el:80")))
    (let ((A (car test))
          (B (cdr test)))
      (should (equal (ffap-file-remote-p A) B)))))

(ert-deftest ffap-machine-p ()
  (should-not (ffap-machine-p "ftp"))
  (should-not (ffap-machine-p "nonesuch"))
  (should (eq (ffap-machine-p "ftp.mathcs.emory.edu") 'accept))
  (should-not (ffap-machine-p "mathcs" 5678))
  (should-not (ffap-machine-p "foo.bonk"))
  (should (eq (ffap-machine-p "foo.bonk.com") 'accept)))

(ert-deftest ffap-tests-25243 ()
  "Test for https://debbugs.gnu.org/25243 ."
  (ert-with-temp-file file
    :suffix "-bug25243"
    (let ((str "diff --git b/lisp/ffap.el a/lisp/ffap.el
index 3d7cebadcf..ad4b70d737 100644
--- b/lisp/ffap.el
+++ a/lisp/ffap.el
@@ -203,6 +203,9 @@ ffap-foo-at-bar-prefix
"))
      (transient-mark-mode 1)
      (when (natnump ffap-max-region-length)
        (insert
         (concat
          str
          (make-string ffap-max-region-length #xa)
          (format "%s ENDS HERE" file)))
        (call-interactively 'mark-whole-buffer)
        (should (equal "" (ffap-string-at-point)))
        (should (equal '(1 1) ffap-string-at-point-region))))))

(ert-deftest ffap-gopher-at-point ()
  (with-temp-buffer
    (insert "\
Type = 1
Name = foo
Path = /the/path
Port = 7070
Host = example.com\n")
    (should-not (ffap-gopher-at-point))
    (goto-char (point-min))
    (should (equal (ffap-gopher-at-point)
                   "gopher://example.com:7070/1/the/path"))
    (should (equal ffap-string-at-point-region
                   (list (point-min) (point-max))))
    (let ((ffap-gopher-regexp nil))
      (should-not (ffap-gopher-at-point)))))

(ert-deftest ffap-other-window--bug-25352 ()
  "Test for Bug#25352.  Checks that the window configuration is
left alone when opening a URL in an external browser."
  (cl-letf* ((old (current-window-configuration))
             (urls nil)
             (ffap-url-fetcher (lambda (url) (push url urls) nil)))
    (should-not (ffap-other-window "https://www.gnu.org"))
    (should (window-configuration-equal-p (current-window-configuration) old))
    (should (equal urls '("https://www.gnu.org")))))

(defun ffap-test-string (space string)
  (let ((ffap-file-name-with-spaces space))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (forward-char 10)
      (ffap-string-at-point))))

(ert-deftest ffap-test-with-spaces ()
  (should
   (equal
    (ffap-test-string
     t "c:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program here.txt")
    "/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program here.txt"))
  (should
   (equal
    (ffap-test-string
     nil "c:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program here.txt")
    "c:/Program"))
  (should
   (equal
    (ffap-test-string
     t "c:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program Files/Hummingbird/")
    "/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program Files/Hummingbird/"))
  (should
   (equal
    (ffap-test-string
     t "c:\\Program Files\\Open Text Evaluation Media\\Open Text Exceed 14 x86\\Program Files\\Hummingbird\\")
    "\\Program Files\\Open Text Evaluation Media\\Open Text Exceed 14 x86\\Program Files\\Hummingbird\\"))
  (should
   (equal
    (ffap-test-string
     t "c:\\Program Files\\Freescale\\CW for MPC55xx and MPC56xx 2.10\\PowerPC_EABI_Tools\\Command_Line_Tools\\CLT_Usage_Notes.txt")
    "\\Program Files\\Freescale\\CW for MPC55xx and MPC56xx 2.10\\PowerPC_EABI_Tools\\Command_Line_Tools\\CLT_Usage_Notes.txt"))
  (should
   (equal
    (ffap-test-string
     t "C:\\temp\\program.log on Windows or /var/log/program.log on Unix.")
    "\\temp\\program.log")))

(ert-deftest ffap-test-no-newlines ()
  (should-not
   (with-temp-buffer
     (save-excursion (insert "type="))
     (ffap-guess-file-name-at-point))))

(ert-deftest ffap-ido-mode ()
  (require 'ido)
  (with-temp-buffer
    (let ((ido-mode t)
          (read-file-name-function read-file-name-function)
          (read-buffer-function read-buffer-function))
      ;; Says ert-deftest:
      ;; Macros in BODY are expanded when the test is defined, not when it
      ;; is run.  If a macro (possibly with side effects) is to be tested,
      ;; it has to be wrapped in `(eval (quote ...))'.
      (eval (quote (ido-everywhere)) t)
      (let ((read-file-name-function (lambda (&rest args)
                                       (expand-file-name
                                        (nth 4 args)
                                        (nth 1 args)))))
        (save-excursion (insert "ffap-tests.el"))
        (let (kill-buffer-query-functions)
          (kill-buffer (call-interactively #'find-file-at-point)))))))

(ert-deftest ffap-test-path ()
  (skip-unless (file-exists-p "/bin"))
  (skip-unless (file-exists-p "/usr/bin"))
  (with-temp-buffer
    (insert "/usr/bin:/bin")
    (goto-char (point-min))
    (should (equal (ffap-file-at-point) "/usr/bin")))
  (with-temp-buffer
    (insert "/usr/bin:/bin")
    (goto-char (point-min))
    (search-forward ":")
    (should (equal (ffap-file-at-point) "/bin")))
  (with-temp-buffer
    (insert ":/bin")
    (goto-char (point-min))
    (should (equal (ffap-file-at-point) nil))))

(provide 'ffap-tests)

;;; ffap-tests.el ends here
