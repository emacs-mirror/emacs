;;; em-prompt-tests.el --- em-prompt test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

;;; Tests:

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
               (format "%s $ " (directory-file-name default-directory))
               'read-only t
               'field 'prompt
               'font-lock-face 'eshell-prompt
               'front-sticky '(read-only field font-lock-face)
               'rear-nonsticky '(read-only field font-lock-face))))
     (should (equal last-input "echo hello\n"))
     (should (equal-including-properties
              last-output
              (propertize "hello\n" 'rear-nonsticky '(field)
                          'field 'command-output))))))

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
                 (format "%s $ " (directory-file-name default-directory))
                 'field 'prompt
                 'front-sticky '(field)
                 'rear-nonsticky '(field))))
       (should (equal last-input "echo hello\n"))
       (should (equal-including-properties
                last-output
                (propertize "hello\n" 'rear-nonsticky '(field)
                            'field 'command-output)))))))

;;; em-prompt-tests.el ends here
