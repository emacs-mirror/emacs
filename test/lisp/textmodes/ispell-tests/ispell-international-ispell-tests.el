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
"This test checks that Russian spellchecking works for.
International Ispell with UTF-8."
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
                  (insert "привет\n")
                  (forward-line -1)
                  (call-process-region nil nil "ispell" nil t nil "-a" "-d" "russian")
                  (goto-char 0)
                  (kill-line)
                  (buffer-string))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "ispell")
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
           "привет\n")
          (goto-char 0)
          (ispell-change-dictionary "russian")
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is correct" nil t)
                         current-point)))
            ))
        (with-temp-buffer
          (insert
           "ёлка\n")
          (goto-char 0)
          (ispell-change-dictionary "russian")
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is correct" nil t)
                         current-point)))
            ))
        (with-temp-buffer
          (insert
           ;; there is no such a word in Russian, I swear.
           "ыфаывфафыввпфывафывафывафывавы\n"
           )
          (goto-char 0)
          (ispell-change-dictionary "russian")
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is incorrect" nil t)
                         current-point)))
            ))
        )))
  )

(ert-deftest ispell/international-ispell/ispell-word/language-switch/check-only ()
  "This test checks that Russian spellchecking works for
 International Ispell with UTF-8."
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
                  (insert "привет\n")
                  (forward-line -1)
                  (call-process-region nil nil "ispell" nil t nil "-a" "-d" "russian")
                  (goto-char 0)
                  (kill-line)
                  (buffer-string))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "ispell")
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
           "привет\n")
          (goto-char 0)
          (ispell-change-dictionary "russian")
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is correct" nil t)
                         current-point)))
            )
          (goto-char (buffer-end 1))
          (insert
           ;; there is no such a word in Russian, I swear.
           "\nыфаывфафыввпфывафывафывафывавы\n"
           )
          (forward-line -1)
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is incorrect" nil t)
                         current-point)))
            )
          (goto-char (buffer-end 1))
          (ispell-change-dictionary "english")
          (insert
            "\nhello\n"
           )
          (forward-line -1)
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is correct" nil t)
                         current-point)))
            )
          ))))
)

(ert-deftest ispell/international-ispell/ispell-word/russian/check-only/wrong-language ()
"If we give Russian-checking Ispell an english word, it should.
Still process it gracefully.  It will not say correct/incorrect, but
it should at least not crash or something."
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
                  (insert "привет\n")
                  (forward-line -1)
                  (call-process-region nil nil "ispell" nil t nil "-a" "-d" "russian")
                  (goto-char 0)
                  (kill-line)
                  (buffer-string))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "ispell")
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
           "hello\n")
          (goto-char 0)
          (ispell-change-dictionary "russian")
          (let ((ispell-check-only t))
            (ispell-word))
          (insert "\nпривет\n")
          (forward-line -1)
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is correct" nil t)
                         current-point)))
            )))))
)

(provide 'ispell-international-ispell-tests)
;;; ispell-international-ispell-tests.el ends here
