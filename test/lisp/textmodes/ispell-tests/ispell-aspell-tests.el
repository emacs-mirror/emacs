;;; ispell-aspell-tests.el --- Test ispell.el aspell backend.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lockywolf

;; Author: Lockywolf <for_emacs_1@lockywolf.net>
;; Keywords: languages, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for ispell.el's aspell integration.

;;; Code:

(require 'ispell)
(require 'ert)
(require 'ert-x)

(eval-and-compile
  (let ((load-path (cons (file-name-directory
                          (or (macroexp-file-name) load-file-name))
                         load-path)))
    (require 'ispell-tests-common)))

(ert-deftest ispell/aspell/ispell-check-version/works ()
  "Test that aspell is correctly detected."
  (skip-unless (and (executable-find "aspell")
                    (with-temp-buffer
                      (call-process "aspell" nil t nil "-vv")
                      (search-backward "but really Aspell"))))
  (ispell-tests--letopt ((ispell-program-name "aspell"))
    (setq ispell-last-program-name (time-to-seconds))
    (setf ispell-program-name "aspell")
    (should (stringp ispell-really-aspell)))
  'passed)

(ert-deftest ispell/aspell/ispell-check-version/version-lowlow ()
  "Test that low-version aspell is correctly detected and rejected."
  (skip-unless (progn
                 (let ((fake-aspell-path (expand-file-name
                                          "./fake-aspell.bash"
                                          ispell-tests--data-directory)))
                   (chmod fake-aspell-path #o770)
                   (call-process fake-aspell-path nil nil nil))))
  (let ((fake-aspell-path (expand-file-name
                           "./fake-aspell.bash"
                           ispell-tests--data-directory)))
    (ispell-tests--letopt ((ispell-program-name "aspell"))
      (setq ispell-last-program-name (time-to-seconds))
      (should-error
       (progn
         (setopt ispell-program-name fake-aspell-path)
         (ispell-check-version t))))
    'passed)
  )

(ert-deftest ispell/aspell/ispell-word/english/correct ()
  "This test checks that Russian spellchecking works for Aspell."
  (skip-unless (executable-find "aspell"))
  (skip-unless (equal
                0
                (call-process "aspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "test")
                  (call-process-region
                   nil
                   nil
                   "aspell" nil t nil "-a" "-denglish"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt ((ispell-program-name "aspell")
                             (ispell-dictionary "english"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/english/correct-one "\n")
          (goto-char 1)
          (ispell-change-dictionary "english")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word)
              (should (string-match "is correct" lres))
              )))))
    'passed)
  )

(ert-deftest ispell/aspell/ispell-word/english/incorrect ()
  "This test checks that English spellchecking works for Aspell."
  (skip-unless (executable-find "aspell"))
  (skip-unless (equal
                0
                (call-process "aspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert ispell-tests--constants/english/correct-one)
                  (call-process-region
                   nil
                   nil
                   "aspell" nil t nil "-a" "-denglish"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt ((ispell-program-name "aspell")
                             (ispell-dictionary "english"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ;; there is no such a word in English, I swear.
           ispell-tests--constants/english/wrong"\n"
           )
          (goto-char 1)
          (ispell-change-dictionary "english")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres))))
        'passed
        )))
  )

(ert-deftest ispell/aspell/ispell-word/english/wrong-language ()
  "This test checks that English spellchecking works for Aspell."
  :expected-result :failed
  (skip-unless (executable-find "aspell"))
  (skip-unless (equal
                0
                (call-process "aspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert ispell-tests--constants/english/correct-one)
                  (call-process-region nil nil "aspell" nil '("*scratch*" t) nil "-a" "-denglish"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt
          ((ispell-program-name "aspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ;; giving Aspell a wrong language should not fail
           ispell-tests--constants/russian/correct "\n"
           )
          (goto-char 1)
          (ispell-change-dictionary "english")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (not (string-match "Error" lres)))))
        'passed)))
  )

(provide 'ispell-aspell-tests)

;;; ispell-aspell-tests.el ends here
