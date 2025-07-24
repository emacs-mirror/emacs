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

(load (expand-file-name "test/lisp/textmodes/ispell-tests/ispell-tests-common.el" source-directory))

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


(provide 'tests-ispell-aspell)
;;; tests-ispell-aspell.el ends here
