;;; em-prompt-tests.el --- em-prompt test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;; Tests for Eshell's prompt support.

;;; Code:

(require 'ert)
(require 'eshell)
(require 'em-prompt)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defmacro em-prompt-test--with-multiline (&rest body)
  "Execute BODY with a multiline Eshell prompt."
  `(let ((eshell-prompt-function (lambda () "multiline prompt\n$ ")))
     ,@body))

;;; Tests:


;; Prompt output

(ert-deftest em-prompt-test/field-properties ()
  "Check that field properties are properly set on Eshell output/prompts."
  (with-temp-eshell
   (eshell-insert-command "echo hello")
   (let ((last-prompt (field-string (1- eshell-last-input-start)))
         (last-input  (field-string (1+ eshell-last-input-start)))
         (last-output (field-string (1+ eshell-last-input-end))))
     (should (equal-including-properties
              last-prompt
              (propertize
               (format "%s %s " (directory-file-name default-directory)
                       (if (= (file-user-uid) 0) "#" "$"))
               'read-only t
               'field 'prompt
               'font-lock-face 'eshell-prompt
               'front-sticky '(read-only font-lock-face field)
               'rear-nonsticky '(read-only font-lock-face field))))
     (should (equal last-input "echo hello\n"))
     (should (equal-including-properties
              last-output
              (apply #'propertize "hello\n"
                     eshell-command-output-properties))))))

(ert-deftest em-prompt-test/field-properties/no-highlight ()
  "Check that field properties are properly set on Eshell output/prompts.
This tests the case when `eshell-highlight-prompt' is nil."
  (let ((eshell-highlight-prompt nil))
    (with-temp-eshell
     (eshell-insert-command "echo hello")
     (let ((last-prompt (field-string (1- eshell-last-input-start)))
           (last-input  (field-string (1+ eshell-last-input-start)))
           (last-output (field-string (1+ eshell-last-input-end))))
       (should (equal-including-properties
                last-prompt
                (propertize
                 (format "%s %s " (directory-file-name default-directory)
                         (if (= (file-user-uid) 0) "#" "$"))
                 'field 'prompt
                 'front-sticky '(field)
                 'rear-nonsticky '(field))))
       (should (equal last-input "echo hello\n"))
       (should (equal-including-properties
                last-output
                (apply #'propertize "hello\n"
                       eshell-command-output-properties)))))))

(ert-deftest em-prompt-test/field-properties/merge-stickiness ()
  "Check that stickiness properties are properly merged on Eshell prompts."
  (let ((eshell-prompt-function
         (lambda ()
           (concat (propertize (eshell/pwd)
                               'front-sticky '(front) 'rear-nonsticky t)
                   (propertize "$ "
                               'front-sticky t 'rear-nonsticky '(rear))))))
    (with-temp-eshell
     (eshell-insert-command "echo hello")
     (let ((last-prompt (field-string (1- eshell-last-input-start))))
       (should (equal-including-properties
                last-prompt
                (concat
                 (propertize
                  (directory-file-name default-directory)
                  'read-only t
                  'field 'prompt
                  'font-lock-face 'eshell-prompt
                  'front-sticky '(front read-only font-lock-face field)
                  'rear-nonsticky t)
                 (propertize
                  "$ "
                  'read-only t
                  'field 'prompt
                  'font-lock-face 'eshell-prompt
                  'front-sticky t
                  'rear-nonsticky '(rear read-only font-lock-face field)))))))))

(ert-deftest em-prompt-test/after-failure ()
  "Check that current prompt shows the exit code of the last failed command."
  (with-temp-eshell
   (let ((debug-on-error nil))
     (eshell-insert-command "(zerop \"foo\")"))
   (let ((current-prompt (field-string (1- (point)))))
     (should (equal-including-properties
              current-prompt
              (propertize
               (concat (directory-file-name default-directory)
                       (unless (eshell-exit-success-p)
                         (format " [%d]" eshell-last-command-status))
                       (if (= (file-user-uid) 0) " # " " $ "))
               'read-only t
               'field 'prompt
               'font-lock-face 'eshell-prompt
               'front-sticky '(read-only font-lock-face field)
               'rear-nonsticky '(read-only font-lock-face field)))))))


;; Prompt navigation

(defun em-prompt-test/next-previous-prompt-1 ()
  "Helper for checking forward/backward navigation of old prompts."
  (with-temp-eshell
   (eshell-insert-command "echo one")
   (eshell-insert-command "echo two")
   (eshell-insert-command "echo three")
   (let ((debug-on-error nil))          ; A failed command.
     (eshell-insert-command "(zerop \"foo\")"))
   (insert "echo fou")                  ; A partially-entered command.
   (ert-info ("Go back one prompt")
     (eshell-previous-prompt)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "(zerop \"foo\")\n")))
   (ert-info ("Go back three prompts, starting from the end of the input")
     (end-of-line)
     (eshell-previous-prompt 3)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo one\n")))
   (ert-info ("Go to the current prompt, starting from the end of the input")
     (end-of-line)
     (eshell-previous-prompt 0)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo one\n")))
   (ert-info ("Go forward one prompt")
     (eshell-next-prompt)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo two\n")))
   (ert-info ("Go forward three prompts")
     (eshell-next-prompt 3)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo fou")))
   (ert-info ("Go back one prompt, starting from the beginning of the line")
     (forward-line 0)
     (eshell-previous-prompt 1)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "(zerop \"foo\")\n")))
   (ert-info ("Go back one prompt, starting from the previous prompt's output")
     (forward-line -1)
     (eshell-previous-prompt 1)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo three\n")))))

(ert-deftest em-prompt-test/next-previous-prompt ()
  "Check that navigating forward/backward through old prompts works correctly."
  (em-prompt-test/next-previous-prompt-1))

(ert-deftest em-prompt-test/next-previous-prompt/multiline ()
  "Check old prompt forward/backward navigation for multiline prompts."
  (em-prompt-test--with-multiline
   (em-prompt-test/next-previous-prompt-1)))

(defun em-prompt-test/forward-backward-paragraph-1 ()
  "Helper for checking forward/backward navigation by paragraphs."
  (with-temp-eshell
    (cl-flet ((at-prompt-for-command-p (command)
                (and (equal (point) (field-beginning))
                     (equal (get-text-property (point) 'field) 'prompt)
                     (save-excursion
                       (goto-char (field-end))
                       (equal (field-string) command)))))
      (eshell-insert-command "echo 'high five'")
      (eshell-insert-command "echo 'up high\n\ndown low'")
      (eshell-insert-command "echo 'too slow'")
      (insert "echo good")            ; A partially-entered command.
      (ert-info ("Go back to the last prompt")
        (eshell-backward-paragraph)
        (should (at-prompt-for-command-p "echo good")))
      (ert-info ("Go back to the paragraph break")
        (eshell-backward-paragraph 2)
        (should (looking-at "\ndown low\n")))
      (ert-info ("Go forward to the third prompt")
        (eshell-forward-paragraph)
        (should (at-prompt-for-command-p "echo 'too slow'\n")))
      (ert-info ("Go backward to before the first prompt")
        (eshell-backward-paragraph 5)
        (should (looking-back "Welcome to the Emacs shell\n")))
      (ert-info ("Go backward to the beginning of the buffer")
        (eshell-backward-paragraph)
        (should (bobp)))
      (ert-info ("Go forward to the second prompt")
        (eshell-forward-paragraph 3)
        (should (at-prompt-for-command-p "echo 'up high\n\ndown low'\n"))))))

(ert-deftest em-prompt-test/forward-backward-paragraph ()
  "Check that navigating forward/backward through paragraphs works correctly."
  (em-prompt-test/forward-backward-paragraph-1))

(ert-deftest em-prompt-test/forward-backward-paragraph/multiline ()
  "Check paragraph forward/backward navigation for multiline prompts."
  (em-prompt-test--with-multiline
   (em-prompt-test/forward-backward-paragraph-1)))

(defun em-prompt-test/forward-backward-matching-input-1 ()
  "Helper for checking forward/backward navigation via regexps."
  (with-temp-eshell
   (eshell-insert-command "echo one")
   (eshell-insert-command "printnl something else")
   (eshell-insert-command "echo two")
   (eshell-insert-command "echo three")
   (let ((debug-on-error nil))          ; A failed command.
     (eshell-insert-command "(zerop \"foo\")"))
   (insert "echo fou")                  ; A partially-entered command.
   (ert-info ("Search for \"echo\", back one prompt")
     (eshell-backward-matching-input "echo" 1)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo three\n")))
   (ert-info ((concat "Search for \"echo\", back two prompts, "
                      "starting from the end of this line"))
     (end-of-line)
     (eshell-backward-matching-input "echo" 2)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo one\n")))
   (ert-info ("Search for \"echo\", forward three prompts")
     (eshell-forward-matching-input "echo" 3)
     (should (equal (point) (field-beginning)))
     (should (equal (field-string) "echo fou")))))

(ert-deftest em-prompt-test/forward-backward-matching-input ()
  "Check that navigating forward/backward via regexps works correctly."
  (em-prompt-test/forward-backward-matching-input-1))

(ert-deftest em-prompt-test/forward-backward-matching-input-multiline ()
  "Check forward/backward regexp navigation for multiline prompts."
  (em-prompt-test--with-multiline
   (em-prompt-test/forward-backward-matching-input-1)))

;;; em-prompt-tests.el ends here
