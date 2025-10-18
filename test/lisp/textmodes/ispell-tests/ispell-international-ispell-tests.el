;;; ispell-international-ispell-tests.el --- Test ispell.el International Ispell backend.  -*- lexical-binding: t; -*-

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

;; Tests for ispell.el's cooperation with International Ispell.

;;; Code:

(require 'ispell)
(require 'ert)
(require 'ert-x)

(eval-and-compile
  (let ((load-path (cons (file-name-directory
                          (or (macroexp-file-name) load-file-name))
                         load-path)))
    (require 'ispell-tests-common)))

(ert-deftest ispell/international-ispell/ispell-word/russian/check-only ()
  "This test checks that Russian spellchecking works for International Ispell with UTF-8."
  (skip-unless (executable-find "ispell"))
  (skip-unless (equal
                0
                (call-process "ispell" nil nil nil "-vv")))
  (skip-unless (string-equal
                "
*

привет
"
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct "\n")
                  (forward-line -1)
                  (call-process-region nil nil "ispell" nil t nil "-a" "-d" "russian")
                  (goto-char 1)
                  (kill-line)
                  (buffer-string))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt
          ((ispell-program-name "ispell")
           (ispell-local-dictionary-alist
            '((
               "russian"
               "[A-Za-zабвгдеёжзиклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ]"
               "[^A-Za-zабвгдеёжзиклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ]"
               ""
               nil
               nil
               nil
               nil
               ))))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/correct "\n")
          (goto-char 1)
          (ispell-change-dictionary "russian")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres))))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/correct "\n")
          (goto-char 1)
          (ispell-change-dictionary "russian")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres))))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/wrong "\n"
           )
          (goto-char 1)
          (ispell-change-dictionary "russian")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres))))
        )))
  )

(ert-deftest ispell/international-ispell/ispell-word/language-switch/check-only ()
  "This test checks that Russian spellchecking works for International Ispell with UTF-8 when switching the language."
  (skip-unless (executable-find "ispell"))
  (skip-unless (equal
                0
                (call-process "ispell" nil nil nil "-vv")))
  (skip-unless (string-equal
                "
*

привет
"
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct "\n")
                  (forward-line -1)
                  (call-process-region nil nil "ispell" nil t nil "-a" "-d" "russian")
                  (goto-char 1)
                  (kill-line)
                  (buffer-string))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt
          ((ispell-program-name "ispell")
           (ispell-local-dictionary-alist
            '((
               "russian"
               "[A-Za-zабвгдеёжзиклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ]"
               "[^A-Za-zабвгдеёжзиклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ]"
               ""
               nil
               nil
               nil
               nil
               ))))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/russian/correct "\n")
          (goto-char 1)
          (ispell-change-dictionary "russian")
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
          (ispell-change-dictionary "english")
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

(ert-deftest ispell/international-ispell/ispell-word/russian/check-only/wrong-language ()
  "If we give Russian-checking Ispell an english word, it should still process it gracefully.
It will not say correct/incorrect, but it should at least not crash or something."
  (skip-unless (executable-find "ispell"))
  (skip-unless (equal
                0
                (call-process "ispell" nil nil nil "-vv")))
  (skip-unless (string-equal
                (seq-concatenate 'string
"
*

" ispell-tests--constants/russian/correct "
")
                (with-temp-buffer
                  (insert ispell-tests--constants/russian/correct "\n")
                  (forward-line -1)
                  (call-process-region nil nil "ispell" nil t nil "-a" "-d" "russian")
                  (goto-char 1)
                  (kill-line)
                  (buffer-string))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (ispell-tests--letopt
          ((ispell-program-name "ispell")
           (ispell-local-dictionary-alist
            '((
               "russian"
               "[A-Za-zабвгдеёжзиклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ]"
               "[^A-Za-zабвгдеёжзиклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ]"
               ""
               nil
               nil
               nil
               nil
               ))))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ispell-tests--constants/english/correct-one "\n")
          (goto-char 1)
          (ispell-change-dictionary "russian")
          (let ((ispell-check-only t))
            (ispell-word))
          (insert "\n" ispell-tests--constants/russian/correct "\n")
          (forward-line -1)
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is correct" lres)))))))
  )

(provide 'ispell-international-ispell-tests)
;;; ispell-international-ispell-tests.el ends here
