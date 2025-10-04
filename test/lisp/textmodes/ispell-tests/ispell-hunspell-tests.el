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

(ert-deftest ispell/hunspell/ispell-word/english/check-only ()
"This test checks that Russian spellchecking works for Hunspell."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "привет")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d en_US"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "hello\n")
          (goto-char 0)
          (ispell-change-dictionary "en_US")
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
           ;; there is no such a word in English, I swear.
           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"
           )
          (goto-char 0)
          (ispell-change-dictionary "en_US")
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
        (with-temp-buffer
          (insert
           ;; giving Hunspell a wrong language should not fail
           "привет\n"
           )
          (goto-char 0)
          (ispell-change-dictionary "en_US")
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

(ert-deftest ispell/hunspell/ispell-word/russian/check-only ()
  "This test checks that Russian spellchecking works Hunspell.
With UTF-8."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (let ((retval (with-temp-buffer
                               (insert "привет")
                               (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
                  (message "lwf:hunspell-test-call=%s" retval)
                  retval )))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "привет\n")
          (goto-char 0)
          (ispell-change-dictionary "ru_RU")
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
          (ispell-change-dictionary "ru_RU")
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

(ert-deftest ispell/hunspell/ispell-word/language-switch/check-only ()
  "This test checks that Russian spellchecking works Hunspell."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "привет")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "привет\n")
          (goto-char 0)
          (ispell-change-dictionary "ru_RU")
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
          (ispell-change-dictionary "en_US")
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

(ert-deftest ispell/hunspell/ispell-word/russian/check-only/wrong-language ()
  "If we give Russian-checking Hunspell an english word, it should.
Still process it gracefully.  It will not say correct/incorrect, but.
It should at least not crash or something."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "привет")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "hello\n")
          (goto-char 0)
          (ispell-change-dictionary "ru_RU")
          (let ((ispell-check-only t))
            ;; should not fail
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

(ert-deftest ispell/hunspell/ispell-word/multilang ()
  "Hunspell is able to check two languages at once."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal
                0
                (call-process "hunspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "привет")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "ru_RU"))))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "привет")
                  (call-process-region nil nil "hunspell" nil '("*scratch*" t) nil "-d" "en_US"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory)
          (multidict "en_US,ru_RU"))
      (letopt ((ispell-program-name "hunspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (ispell-hunspell-add-multi-dic multidict)
        (with-temp-buffer
          (insert
           "hello\n")
          (goto-char 0)
          (ispell-change-dictionary multidict)
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
            )
          (insert "\nhello\n")
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
          (insert "\nывафываываыфвавыафывавыфафывафываыва\n")
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
          (insert "\nhelooooooooo\n")
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
          ))))
)



(provide 'ispell-hunspell-tests)
;;; ispell-hunspell-tests.el ends here
