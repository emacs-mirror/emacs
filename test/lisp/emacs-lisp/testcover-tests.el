;;; testcover-tests.el --- Testcover test suite   -*- lexical-binding:t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

;; Author: Gemini Lasswell

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

;; Testcover test suite.
;; * All the test cases are in testcover-resources/testcover-cases.el.
;;   See that file for an explanation of the test case format.
;; * `testcover-tests-define-tests', which is run when this file is
;;   loaded, reads testcover-resources/testcover-cases.el and defines
;;   ERT tests for each test case.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'testcover)
(require 'skeleton)

;; Convert Testcover's overlays to plain text.

(eval-and-compile
  (defun testcover-tests-markup-region (beg end &rest optargs)
    "Mark up test code within region between BEG and END.
Convert Testcover's tan and red splotches to %%% and !!! for
testcases.el.  This can be used to create test cases if Testcover
is working correctly on a code sample.  OPTARGS are optional
arguments for `testcover-start'."
    (interactive "r")
    (ert-with-temp-file tempfile
      :suffix ".el"
      (let ((find-file-suppress-same-file-warnings t)
            (code (buffer-substring beg end))
            (marked-up-code))
        (unwind-protect
            (progn
              (with-temp-file tempfile
                (insert code))
              (save-current-buffer
                (let ((buf (find-file-noselect tempfile)))
                  (set-buffer buf)
                  (apply 'testcover-start (cons tempfile optargs))
                  (testcover-mark-all buf)
                  (dolist (overlay (overlays-in (point-min) (point-max)))
                    (let ((ov-face (overlay-get overlay 'face)))
                      (goto-char (overlay-end overlay))
                      (cond
                       ((eq ov-face 'testcover-nohits) (insert "!!!"))
                       ((eq ov-face 'testcover-1value) (insert "%%%"))
                       (t nil))))
                  (setq marked-up-code (buffer-string)))
                (set-buffer-modified-p nil)))
          (ignore-errors (kill-buffer (find-file-noselect tempfile))))

        ;; Now replace the original code with the marked up code.
        (delete-region beg end)
        (insert marked-up-code)))))

(eval-and-compile
  (defun testcover-tests-unmarkup-region (beg end)
    "Remove the markup used in testcases.el between BEG and END."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward "!!!\\|%%%" nil t)
          (replace-match ""))))))

(define-skeleton testcover-tests-skeleton
  "Write a testcase for testcover-tests.el."
  "Enter name of test: "
  ";; ==== "  str " ====\n"
  "\"docstring\"\n"
  ";; Directives for ERT should go here, if any.\n"
  ";; ====\n"
  ";; Replace this line with annotated test code.\n")

;; Check a test case.

(eval-and-compile
  (defun testcover-tests-run-test-case (marked-up-code)
    "Test the operation of Testcover on the string MARKED-UP-CODE."
    (ert-with-temp-file tempfile
      :suffix ".el"
      (let ((find-file-suppress-same-file-warnings t))
        (unwind-protect
            (progn
              (with-temp-file tempfile
                (insert marked-up-code))
              ;; Remove the marks and mark the code up again. The original
              ;; and recreated versions should match.
              (save-current-buffer
                (set-buffer (find-file-noselect tempfile))
                ;; Fail the test if the debugger tries to become active,
                ;; which can happen if Testcover fails to attach itself
                ;; correctly. Note that this will prevent debugging
                ;; these tests using Edebug.
                (cl-letf (((symbol-function #'edebug-default-enter)
                           (lambda (&rest _args)
                             (ert-fail "Debugger invoked during test run"))))
                  (dolist (byte-compile '(t nil))
                    (testcover-tests-unmarkup-region (point-min) (point-max))
                    (unwind-protect
                        (testcover-tests-markup-region (point-min) (point-max) byte-compile)
                      (set-buffer-modified-p nil))
                    (should (string= marked-up-code
                                     (buffer-string)))))))
          (ignore-errors (kill-buffer (find-file-noselect tempfile))))))))

;; Convert test case file to ert-defmethod.

(eval-and-compile
  (defun testcover-tests-build-test-cases ()
    "Parse the test case file and return a list of ERT test definitions.
Construct and return a list of `ert-deftest' forms.  See testcases.el
for documentation of the test definition format."
    (let (results)
      (with-temp-buffer
        (insert-file-contents (ert-resource-file "testcases.el"))
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^;; ==== \\([^ ]+?\\) ====\n"
                        "\\(\\(?:.*\n\\)*?\\)"
                        ";; ====\n"
                        "\\(\\(?:.*\n\\)*?\\)"
                        "\\(\\'\\|;; ====\\)")
                nil t)
          (let ((name (match-string 1))
                (splice (car (read-from-string
                              (format "(%s)" (match-string 2)))))
                (code (match-string 3)))
            (push
             `(ert-deftest ,(intern (concat "testcover-tests-" name)) ()
                ,@splice
                (testcover-tests-run-test-case ,code))
             results))
          (beginning-of-line)))
      results)))

;; Define all the tests.

(defmacro testcover-tests-define-tests ()
  "Construct and define ERT test methods using the test case file."
  (let* ((test-cases (testcover-tests-build-test-cases)))
    `(progn ,@test-cases)))

(testcover-tests-define-tests)

(provide 'testcover-tests)

;;; testcover-tests.el ends here
