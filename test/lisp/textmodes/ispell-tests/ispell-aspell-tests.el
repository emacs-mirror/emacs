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
  (should (stringp
           (let ((test-saved-ispell-program-name ispell-program-name))
             (unwind-protect
                 (let ()
                   (setq ispell-last-program-name (time-to-seconds))
                   (setf ispell-program-name "aspell")
                   ispell-really-aspell)
               (setf ispell-program-name test-saved-ispell-program-name))))))

(ert-deftest ispell/aspell/ispell-check-version/version-lowlow ()
  "Test that aspell is correctly detected."
  (skip-unless (progn
                 (let ((fake-aspell-path (expand-file-name
                                          "./fake-aspell.bash"
                                          tests-ispell-data-directory)))
                   (chmod fake-aspell-path 504)
                   (call-process fake-aspell-path nil nil nil))))
  (let ((fake-aspell-path (expand-file-name
                           "./fake-aspell.bash"
                           tests-ispell-data-directory)))
    (let ((test-saved-ispell-program-name ispell-program-name)
          (test-saved-ispell-last-program-name ispell-last-program-name))
      (unwind-protect
          (progn
            (setq ispell-last-program-name (time-to-seconds))
            (should-error
             (progn
               (setopt ispell-program-name fake-aspell-path)
               (ispell-check-version t)))
            ispell-really-aspell)
        (set-variable 'ispell-program-name test-saved-ispell-program-name)
        (set-variable 'ispell-last-program-name
                      test-saved-ispell-last-program-name)))))

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
      (letopt ((ispell-program-name "aspell")
               (ispell-dictionary "english"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "hello\n")
          (goto-char 0)
          (ispell-change-dictionary "english")
          (let ((debugmessage ""))
            (ert-with-message-capture lres
              (let ((ispell-check-only t))
                (ispell-word)
                (setf debugmessage lres)
                ;;(should (string-match "is correct" lres))
                ))
            (message "lwf:lres=%s" debugmessage)))
        'passed
        )))
  )

(ert-deftest ispell/aspell/ispell-word/english/incorrect ()
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
      (letopt ((ispell-program-name "aspell")
               (ispell-dictionary "english"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ;; there is no such a word in English, I swear.
           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"
           )
          (goto-char 0)
          (ispell-change-dictionary "english")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (string-match "is incorrect" lres))))
        'passed
        )))
  )

(ert-deftest ispell/aspell/ispell-word/english/wrong-language ()
  "This test checks that Russian spellchecking works for Aspell."
  :expected-result :failed
  (skip-unless (executable-find "aspell"))
  (skip-unless (equal
                0
                (call-process "aspell" nil nil nil "-vv")))
  (skip-unless (equal
                0
                (with-temp-buffer
                  (insert "test")
                  (call-process-region nil nil "aspell" nil '("*scratch*" t) nil "-a" "-denglish"))))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name "aspell"))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           ;; giving Aspell a wrong language should not fail
           "привет\n"
           )
          (goto-char 0)
          (ispell-change-dictionary "english")
          (ert-with-message-capture lres
            (let ((ispell-check-only t))
              (ispell-word))
            (should (not (string-match "Error" lres)))))
        'passed
        )))
  )

(provide 'tests-ispell-aspell)
;;; tests-ispell-aspell.el ends here
