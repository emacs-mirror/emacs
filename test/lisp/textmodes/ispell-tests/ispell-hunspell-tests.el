;;; ispell-hunspell-tests.el --- Test ispell.el Hunspell backend.  -*- lexical-binding: t; -*-

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

;; Tests for ispell.el's cooperation with Hunspell

;;; Code:

(require 'ispell)
(require 'ert)
(require 'ert-x)

(eval-and-compile
  (let ((load-path (cons (file-name-directory
                          (or (macroexp-file-name) load-file-name))
                         load-path)))
    (require 'ispell-tests-common)))

(defun ispell-tests-hunspell--hunspell-working ()
  "Check that Hunspell can be called from Emacs."
  (if (not (equal
            0
            (call-process "hunspell" nil nil nil "-a")))
      (progn
        (message "Hunspell installation is broken and does not support a default dictionary!")
        nil)
    t))

(ert-deftest ispell/hunspell/ispell-word/english/check-only ()
  "This test checks that English spellchecking works for Hunspell."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (ispell-tests-hunspell--hunspell-working))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "привет")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d en_US"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/english/correct-one "\n")
          (goto-char 1)
          (ispell-change-dictionary "en_US")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres))))
        (with-temp-buffer
          (insert
           ispell-tests--constants/english/wrong "\n"
           )
          (goto-char 1)
          (ispell-change-dictionary "en_US")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres))))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/correct "\n"
           )
          (goto-char 1)
          (ispell-change-dictionary "en_US")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres))))
        )))
  )

(ert-deftest ispell/hunspell/ispell-word/russian/check-only ()
  "This test checks that Russian spellchecking works Hunspell.
With UTF-8."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (ispell-tests-hunspell--hunspell-working))
  (skip-unless (equal
                0
                (let ((retval
                       (with-temp-buffer
                         (insert ispell-tests--constants/russian/correct)
                         (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
                  retval)))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/correct "\n")
          (goto-char 1)
          (ispell-change-dictionary "ru_RU")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres))))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/wrong "\n"
           )
          (goto-char 1)
          (ispell-change-dictionary "ru_RU")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres))))
        )))
  )

(ert-deftest ispell/hunspell/ispell-word/language-switch/check-only ()
  "This test checks that language switching works for Hunspell."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (ispell-tests-hunspell--hunspell-working))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct "\n")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt
          ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/correct "\n")
          (goto-char 1)
          (ispell-change-dictionary "ru_RU")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres)))
          (goto-char (buffer-end 1))
          (insert
           "\n" ispell-tests--constants/russian/wrong "\n"
           )
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres)))
          (goto-char (buffer-end 1))
          (ispell-change-dictionary "en_US")
          (insert
           "\n" ispell-tests--constants/english/correct-one "\n"
           )
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres)))
          ))))
  )

(ert-deftest ispell/hunspell/ispell-word/russian/check-only/wrong-language ()
  "If we give Russian-checking Hunspell an english word, it should still process it gracefully.
It will not say correct/incorrect, but it should at least not crash or something."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (ispell-tests-hunspell--hunspell-working))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct)
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt
          ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/english/correct-one "\n")
          (goto-char 1)
          (ispell-change-dictionary "ru_RU")
          (let ((ispell-check-only t))
            ;; should not fail
            (ispell-word))
          (insert "\n" ispell-tests--constants/russian/correct "\n")
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres)))))))
  )

(ert-deftest ispell/hunspell/ispell-word/multilang ()
  "Hunspell is able to check two languages at once."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (ispell-tests-hunspell--hunspell-working))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct)
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct)
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "en_US"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory)
          (multidict "en_US,ru_RU"))
      (ispell-tests--letopt
          ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (ispell-hunspell-add-multi-dic multidict)
        (with-temp-buffer
          (insert
           ispell-tests--constants/english/correct-one "\n")
          (goto-char 1)
          (ispell-change-dictionary multidict)
          (insert "\n" ispell-tests--constants/russian/correct "\n")
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres)))
          (insert "\n" ispell-tests--constants/english/correct-one "\n")
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres)))
          (insert "\n" ispell-tests--constants/russian/wrong "\n")
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres)))
          (insert "\n" ispell-tests--constants/english/wrong "\n")
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres)))))))
  )

(provide 'ispell-hunspell-tests)
;;; ispell-hunspell-tests.el ends here
