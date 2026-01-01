;;; dabbrev-tests.el --- Test suite for dabbrev.  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Alan Third <alan@idiocy.org>
;; Keywords: dabbrev

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
(require 'ert-x)
(require 'dabbrev)

(ert-deftest dabbrev-expand-test ()
  "Test for bug#1948.
When `dabbrev-eliminate-newlines' is non-nil (the default),
repeated calls to `dabbrev-expand' can result in the source of
first expansion being replaced rather than the destination."
  (with-temp-buffer
   (insert "ab  x\na\nab  y")
   (goto-char 8)
   (save-window-excursion
     (set-window-buffer nil (current-buffer))
     (execute-kbd-macro (kbd "M-/ SPC M-/ M-/")))
   (should (string= (buffer-string) "ab  x\nab y\nab  y"))))

(ert-deftest dabbrev-completion-test ()
  "Test for bug#17899.
dabbrev-completion should not look for expansions in other
buffers unless a prefix argument is used."
  (with-temp-buffer
    (insert "axy")
    (with-temp-buffer
      (insert "abc\na")
      (goto-char 6)
      (save-window-excursion
        (set-window-buffer nil (current-buffer))
        (execute-kbd-macro (kbd "C-M-/")))
      (should (string= (buffer-string) "abc\nabc")))))

(ert-deftest dabbrev-completion-test-with-argument ()
  "Test for bug#17899.
dabbrev-completion should not complete because it has found
multiple expansions."
  (with-temp-buffer
    (insert "axy")
    (with-temp-buffer
      (insert "abc\na")
      (goto-char 6)
      (save-window-excursion
        (set-window-buffer nil (current-buffer))
        (execute-kbd-macro (kbd "C-u C-u C-M-/")))
      (should (string= (buffer-string) "abc\na")))))

(defmacro with-dabbrev-test (&rest body)
  "Set up an isolated `dabbrev' test environment."
  (declare (debug (body)))
  `(ert-with-temp-directory dabbrev-test-home
     (let* (;; Since we change HOME, clear this to avoid a conflict
            ;; e.g. if Emacs runs within the user's home directory.
            (abbreviated-home-dir nil)
            (process-environment (cons (format "HOME=%s" dabbrev-test-home)
                                       process-environment))
            (dabbrev-directory (ert-resource-directory)))
       (unwind-protect
           (progn ,@body)
         ;; Restore pre-test-run state of test files.
         (dolist (f (directory-files dabbrev-directory))
           (let ((buf (get-file-buffer f)))
             (when buf
               (with-current-buffer buf
                 (restore-buffer-modified-p nil)
                 (kill-buffer)))))
         (dabbrev--reset-global-variables)))))

(ert-deftest dabbrev-expand-test-same-buffer-1 ()
  "Test expanding a string twice within a single buffer.
The first expansion should expand the input (a prefix-string) to a
string in the buffer containing no whitespace character, the second
expansion, after adding a space to the first expansion, should extend
the string with the following string in the buffer up to the next
whitespace character."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (goto-char (point-max))
   (terpri)
   (execute-kbd-macro (kbd "Ind M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic"))
   (execute-kbd-macro (kbd "SPC M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic and"))))

(ert-deftest dabbrev-expand-test-same-buffer-2 ()
  "Test expanding a string plus space twice within a single buffer.
Each expansion should extend the string with the following string in the
buffer up to the next whitespace character."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (goto-char (point-max))
   (terpri)
   (execute-kbd-macro (kbd "Indic SPC M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic and"))
   (execute-kbd-macro (kbd "SPC M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic and Khmer"))))

(ert-deftest dabbrev-expand-test-same-buffer-3 ()
  "Test replacing an expansion within a single buffer."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (goto-char (point-max))
   (terpri)
   (insert-file-contents (ert-resource-file "dabbrev-expand.el"))
   (goto-char (point-max))
   (terpri)
   (execute-kbd-macro (kbd "Ind M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indicate"))
   (kill-whole-line)
   (execute-kbd-macro (kbd "Ind M-/ M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic"))
   (execute-kbd-macro (kbd "SPC M-/"))
   (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic and"))))

(ert-deftest dabbrev-expand-test-same-buffer-4 ()
  "Test expanding a string in a narrowed-region."
  (with-dabbrev-test
   (let (disabled-command-function) ; Enable narrow-to-region.
     (find-file (ert-resource-file "INSTALL_BEGIN"))
     (goto-char (point-min))
     (execute-kbd-macro (kbd "C-s Ind M-a C-SPC M-} C-x n n"))
     (goto-char (point-max))
     (terpri)
     (execute-kbd-macro (kbd "Ind M-/"))
     (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic"))
     (execute-kbd-macro (kbd "SPC M-/"))
     (should (string= (buffer-substring (pos-bol) (pos-eol)) "Indic and")))))

(ert-deftest dabbrev-expand-test-other-buffer-1 ()
  "Test expanding a prefix string to a string from another buffer."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (switch-to-buffer (get-buffer-create "a" t))
   (execute-kbd-macro (kbd "Ind M-/"))
   (should (string= (buffer-string) "Indic"))
   (execute-kbd-macro (kbd "SPC M-/"))
   (should (string= (buffer-string) "Indic and"))
   (kill-buffer "a")))

(ert-deftest dabbrev-expand-test-other-buffer-2 ()
  "Test expanding a string + space to a string from another buffer."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (switch-to-buffer (get-buffer-create "a" t))
   (execute-kbd-macro (kbd "Indic SPC M-/"))
   (should (string= (buffer-string) "Indic and"))
   (execute-kbd-macro (kbd "SPC M-/"))
   (should (string= (buffer-string) "Indic and Khmer"))
   (kill-buffer "a")))

(ert-deftest dabbrev-expand-test-other-buffer-3 ()
  "Test replacing an expansion with three different buffers.
A prefix string in a buffer should find the first expansion in a
different buffer and then find a replacement expansion is yet another
buffer."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (find-file (ert-resource-file "dabbrev-expand.el"))
   (switch-to-buffer (get-buffer-create "a" t))
   (emacs-lisp-mode)
   (execute-kbd-macro (kbd "Ind M-/"))
   (should (string= (buffer-string) "Indicate"))
   (erase-buffer)
   (execute-kbd-macro (kbd "Ind M-/ M-/"))
   (should (string= (buffer-string) "Indic"))
   (execute-kbd-macro (kbd "SPC M-/"))
   (should (string= (buffer-string) "Indic and"))
   (kill-buffer "a")))

(ert-deftest dabbrev-expand-test-other-buffer-4 ()
  "Test expanding a string using another narrowed buffer."
  (with-dabbrev-test
   (let (disabled-command-function) ; Enable narrow-to-region.
     (find-file (ert-resource-file "INSTALL_BEGIN"))
     (goto-char (point-min))
     (execute-kbd-macro (kbd "C-s Ind M-a C-SPC M-} C-x n n"))
     (switch-to-buffer (get-buffer-create "a" t))
     (execute-kbd-macro (kbd "Ind M-/"))
     (should (string= (buffer-string) "Indic"))
     (execute-kbd-macro (kbd "SPC M-/"))
     (should (string= (buffer-string) "Indic and"))
     (kill-buffer "a"))))

(ert-deftest dabbrev-expand-test-minibuffer-1 ()
  "Test expanding a prefix string twice in the minibuffer.
Both expansions should come from the buffer from which the minibuffer
was entered."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (with-selected-window (minibuffer-window)
     (insert "Ind")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic"))
     (insert " ")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic and"))
     (delete-minibuffer-contents))))

(ert-deftest dabbrev-expand-test-minibuffer-2 ()
  "Test expanding a string + space in the minibuffer.
The expansions should come from the buffer from which the minibuffer was
entered."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (with-selected-window (minibuffer-window)
     (insert "Indic ")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic and"))
     (insert " ")
     (dabbrev-expand nil)
     (should (string= (buffer-string) "Indic and Khmer"))
     (delete-minibuffer-contents))))

;; FIXME: Why is dabbrev--reset-global-variables needed here?
(ert-deftest dabbrev-expand-test-minibuffer-3 ()
  "Test replacing an expansion in the minibuffer using two buffers.
The first expansion should be found in the buffer from which the
minibuffer was entered, the replacement should found in another buffer."
  (with-dabbrev-test
   (find-file (ert-resource-file "INSTALL_BEGIN"))
   (find-file (ert-resource-file "dabbrev-expand.el"))
   (with-selected-window (minibuffer-window)
     (insert "Ind")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indicate"))
     (kill-whole-line)
     (dabbrev--reset-global-variables)
     (insert "Ind")
     (dabbrev-expand nil)
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic"))
     (dabbrev--reset-global-variables)
     (insert " ")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic and"))
     (delete-minibuffer-contents))))

(ert-deftest dabbrev-expand-test-minibuffer-4 ()
  "Test expansion in the minibuffer using another narrowed buffer."
  (with-dabbrev-test
   (let (disabled-command-function) ; Enable narrow-to-region.
     (find-file (ert-resource-file "INSTALL_BEGIN"))
     (goto-char (point-min))
     (execute-kbd-macro (kbd "C-s Ind M-a C-SPC M-} C-x n n")))
   (with-selected-window (minibuffer-window)
     (insert "Ind")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic"))
     (insert " ")
     (dabbrev-expand nil)
     (should (string= (minibuffer-contents) "Indic and"))
     (delete-minibuffer-contents))))

(ert-deftest dabbrev-expand-after-killing-buffer ()
  "Test expansion after killing buffer containing first expansion.
Finding successive expansions in another live buffer should succeed, but
after killing the buffer, expansion should fail with a user-error,
leaving the unexpanded string in the buffer." ; See bug#74090.
  (with-dabbrev-test
   (with-current-buffer (get-buffer-create "foo")
     (insert "abc abd"))
   (switch-to-buffer "*scratch*")
   (erase-buffer)
   (execute-kbd-macro (kbd "ab M-/"))
   (should (string= (buffer-string) "abc"))
   (execute-kbd-macro (kbd "SPC ab M-/"))
   (should (string= (buffer-string) "abc abc"))
   (erase-buffer)
   (execute-kbd-macro (kbd "abc SPC ab M-/ M-/"))
   (should (string= (buffer-string) "abc abd"))
   (kill-buffer "foo")
   (erase-buffer)
   ;; In batch runs of this file, the user-error message contains curved
   ;; quotes, but grave quotes when running `make check' (so here was
   ;; evidently not passed to `substitute-command-keys'), so use grave
   ;; quotes so the test succeeds in both modes of execution.
   (let* ((text-quoting-style 'grave)
          (msg (cadr (should-error (execute-kbd-macro (kbd "abc SPC ab M-/ M-/"))
                                  :type 'user-error))))
     (should (string= (buffer-string) "abc ab"))
     (should (string= msg "No further dynamic expansion for `ab' found")))))

;;; dabbrev-tests.el ends here
