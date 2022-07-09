;;; esh-io-tests.el --- esh-io test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'esh-mode)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

(defvar eshell-test-value nil)

(defun eshell-test-file-string (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun eshell/test-output ()
  "Write some test output separately to stdout and stderr."
  (eshell-printn "stdout")
  (eshell-errorn "stderr"))

;;; Tests:


;; Basic redirection

(ert-deftest esh-io-test/redirect-file/overwrite ()
  "Check that redirecting to a file in overwrite mode works."
  (ert-with-temp-file temp-file
    :text "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new > %s" temp-file)))
    (should (equal (eshell-test-file-string temp-file) "new"))))

(ert-deftest esh-io-test/redirect-file/append ()
  "Check that redirecting to a file in append mode works."
  (ert-with-temp-file temp-file
    :text "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new >> %s" temp-file)))
    (should (equal (eshell-test-file-string temp-file) "oldnew"))))

(ert-deftest esh-io-test/redirect-file/insert ()
  "Check that redirecting to a file in insert works."
  (ert-with-temp-file temp-file
    :text "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new >>> %s" temp-file)))
    (should (equal (eshell-test-file-string temp-file) "newold"))))

(ert-deftest esh-io-test/redirect-buffer/overwrite ()
  "Check that redirecting to a buffer in overwrite mode works."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new > #<%s>" bufname)))
    (should (equal (buffer-string) "new"))))

(ert-deftest esh-io-test/redirect-buffer/append ()
  "Check that redirecting to a buffer in append mode works."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-insert-command (format "echo new >> #<%s>" bufname)))
    (should (equal (buffer-string) "oldnew"))))

(ert-deftest esh-io-test/redirect-buffer/insert ()
  "Check that redirecting to a buffer in insert mode works."
  (eshell-with-temp-buffer bufname "old"
    (goto-char (point-min))
    (with-temp-eshell
     (eshell-insert-command (format "echo new >>> #<%s>" bufname)))
    (should (equal (buffer-string) "newold"))))

(ert-deftest esh-io-test/redirect-buffer/escaped ()
  "Check that redirecting to a buffer with escaped characters works."
  (with-temp-buffer
    (rename-buffer "eshell\\temp\\buffer" t)
    (let ((bufname (buffer-name)))
      (with-temp-eshell
       (eshell-insert-command (format "echo hi > #<%s>"
                                      (string-replace "\\" "\\\\" bufname))))
      (should (equal (buffer-string) "hi")))))

(ert-deftest esh-io-test/redirect-symbol/overwrite ()
  "Check that redirecting to a symbol in overwrite mode works."
  (let ((eshell-test-value "old"))
    (with-temp-eshell
     (eshell-insert-command "echo new > #'eshell-test-value"))
    (should (equal eshell-test-value "new"))))

(ert-deftest esh-io-test/redirect-symbol/append ()
  "Check that redirecting to a symbol in append mode works."
  (let ((eshell-test-value "old"))
    (with-temp-eshell
     (eshell-insert-command "echo new >> #'eshell-test-value"))
    (should (equal eshell-test-value "oldnew"))))

(ert-deftest esh-io-test/redirect-marker ()
  "Check that redirecting to a marker works."
  (with-temp-buffer
    (let ((eshell-test-value (point-marker)))
      (with-temp-eshell
       (eshell-insert-command "echo hi > $eshell-test-value"))
      (should (equal (buffer-string) "hi")))))

(ert-deftest esh-io-test/redirect-multiple ()
  "Check that redirecting to multiple targets works."
  (let ((eshell-test-value "old"))
    (eshell-with-temp-buffer bufname "old"
     (with-temp-eshell
      (eshell-insert-command (format "echo new > #<%s> > #'eshell-test-value"
                                     bufname)))
     (should (equal (buffer-string) "new"))
     (should (equal eshell-test-value "new")))))

(ert-deftest esh-io-test/redirect-multiple/repeat ()
  "Check that redirecting to multiple targets works when repeating a target."
  (let ((eshell-test-value "old"))
    (eshell-with-temp-buffer bufname "old"
     (with-temp-eshell
      (eshell-insert-command
       (format "echo new > #<%s> > #'eshell-test-value > #<%s>"
               bufname bufname)))
     (should (equal (buffer-string) "new"))
     (should (equal eshell-test-value "new")))))


;; Redirecting specific handles

(ert-deftest esh-io-test/redirect-stdout ()
  "Check that redirecting to stdout doesn't redirect stderr."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output > #<%s>" bufname)
                                  "stderr\n"))
    (should (equal (buffer-string) "stdout\n")))
  ;; Also check explicitly specifying the stdout fd.
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 1> #<%s>" bufname)
                                  "stderr\n"))
    (should (equal (buffer-string) "stdout\n"))))

(ert-deftest esh-io-test/redirect-stderr/overwrite ()
  "Check that redirecting to stderr doesn't redirect stdout."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2> #<%s>" bufname)
                                  "stdout\n"))
    (should (equal (buffer-string) "stderr\n"))))

(ert-deftest esh-io-test/redirect-stderr/append ()
  "Check that redirecting to stderr doesn't redirect stdout."
  (eshell-with-temp-buffer bufname "old"
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2>> #<%s>" bufname)
                                  "stdout\n"))
    (should (equal (buffer-string) "oldstderr\n"))))

(ert-deftest esh-io-test/redirect-stderr/insert ()
  "Check that redirecting to stderr doesn't redirect stdout."
  (eshell-with-temp-buffer bufname "old"
    (goto-char (point-min))
    (with-temp-eshell
     (eshell-match-command-output (format "test-output 2>>> #<%s>" bufname)
                                  "stdout\n"))
    (should (equal (buffer-string) "stderr\nold"))))

(ert-deftest esh-io-test/redirect-stdout-and-stderr ()
  "Check that redirecting to both stdout and stderr works."
  (eshell-with-temp-buffer bufname-1 "old"
    (eshell-with-temp-buffer bufname-2 "old"
      (with-temp-eshell
       (eshell-match-command-output (format "test-output > #<%s> 2> #<%s>"
                                            bufname-1 bufname-2)
                                    "\\`\\'"))
      (should (equal (buffer-string) "stderr\n")))
    (should (equal (buffer-string) "stdout\n"))))


;; Virtual targets

(ert-deftest esh-io-test/virtual-dev-eshell ()
  "Check that redirecting to /dev/eshell works."
  (with-temp-eshell
   (eshell-match-command-output "echo hi > /dev/eshell" "hi")))

(ert-deftest esh-io-test/virtual-dev-kill ()
  "Check that redirecting to /dev/kill works."
  (with-temp-eshell
   (eshell-insert-command "echo one > /dev/kill")
   (should (equal (car kill-ring) "one"))
   (eshell-insert-command "echo two > /dev/kill")
   (should (equal (car kill-ring) "two"))
   (eshell-insert-command "echo three >> /dev/kill")
   (should (equal (car kill-ring) "twothree"))))

;;; esh-io-tests.el ends here
