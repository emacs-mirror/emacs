;;; term-tests.el --- tests for term.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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


;;; Code:
(require 'ert)
(require 'term)
(eval-when-compile (require 'cl-lib))

(defvar term-height)                    ; Number of lines in window.
(defvar term-width)                     ; Number of columns in window.

(defun term-test-screen-from-input (width height input &optional return-var)
  (with-temp-buffer
    (term-mode)
    ;; Keep dimensions independent from window size.
    (remove-function (local 'window-adjust-process-window-size-function)
                     'term-maybe-reset-size)
    (term-exec (current-buffer) "test" "cat" nil nil)
    (term-char-mode)
    (setq term-width width)
    (setq term-height height)
    ;; Pass input directly to `term-emulate-terminal', it's easier to
    ;; control chunking, and we don't have to worry about wrestling
    ;; with stty settings.
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Don't get stuck when we close the buffer.
      (set-process-query-on-exit-flag proc nil)
      (if (consp input)
                (mapc (lambda (input) (term-emulate-terminal proc input)) input)
              (term-emulate-terminal proc input))
      (if return-var (buffer-local-value return-var (current-buffer))
        (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest term-simple-lines ()
  (let ((str "\
first line\r
next line\r\n"))
    (should (equal (term-test-screen-from-input 40 12 str)
                   (replace-regexp-in-string "\r" "" str)))))

(ert-deftest term-carriage-return ()
  (let ((str "\
first line\r_next line\r\n"))
    (should (equal (term-test-screen-from-input 40 12 str)
                   "_next line\n"))))

(ert-deftest term-line-wrap ()
  (should (string-match-p
           ;; Don't be strict about trailing whitespace.
           "\\`a\\{40\\}\na\\{20\\} *\\'"
           (term-test-screen-from-input 40 12 (make-string 60 ?a))))
  ;; Again, but split input into chunks.
  (should (string-match-p
           "\\`a\\{40\\}\na\\{20\\} *\\'"
           (term-test-screen-from-input 40 12 (let ((str (make-string 30 ?a)))
                                                (list str str))))))

(ert-deftest term-cursor-movement ()
  ;; Absolute positioning.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (concat "\e[2;2Hd"
                                "\e[2;1Hc"
                                "\e[1;2Hb"
                                "\e[1;1Ha"))))
  ;; Send one byte at a time.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (split-string (concat "\e[2;2Hd"
                                              "\e[2;1Hc"
                                              "\e[1;2Hb"
                                              "\e[1;1Ha") "" t))))
  ;; Relative positioning.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (concat "\e[B\e[Cd"
                                "\e[D\e[Dc"
                                "\e[Ab"
                                "\e[D\e[Da")))))

(ert-deftest term-scrolling-region ()
  (should (equal "\
line3
line4
line5
line6
"
                 (term-test-screen-from-input
                  40 12 "\e[1;5r\
line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
"))))

(ert-deftest term-set-directory ()
  (let ((term-ansi-at-user (user-real-login-name)))
    (should (equal (term-test-screen-from-input
                    40 12 "\eAnSiTc /foo/\n" 'default-directory)
                   "/foo/"))
    ;; Split input (Bug#17231).
    (should (equal (term-test-screen-from-input
                    40 12 (list "\eAnSiTc /f" "oo/\n") 'default-directory)
                   "/foo/"))))

(provide 'term-tests)

;;; term-tests.el ends here
