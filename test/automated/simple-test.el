;;; simple-test.el --- Tests for simple.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(defmacro simple-test--dummy-buffer (&rest body)
  (declare (indent 0)
           (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (setq indent-tabs-mode nil)
     (insert "(a b")
     (save-excursion (insert " c d)"))
     ,@body
     (cons (buffer-substring (point-min) (point))
           (buffer-substring (point) (point-max)))))



;;; `newline'
(ert-deftest newline ()
  (should-error (newline -1))
  (should (equal (simple-test--dummy-buffer (newline 1))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-mode -1)
                   (call-interactively #'newline))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((current-prefix-arg 5))
                     (call-interactively #'newline)))
                 '("(a b\n\n\n\n\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer (newline 5))
                 '("(a b\n\n\n\n\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (newline 1))
                 '("(a b \n" . "c d)"))))

(ert-deftest newline-indent ()
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 1))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 1 'interactive))
                 '("(a b\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg nil))
                     (call-interactively #'newline)
                     (call-interactively #'newline)))
                 '("(a b\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 5 'interactive))
                 '("(a b\n\n\n\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg 5))
                     (call-interactively #'newline)))
                 '("(a b\n\n\n\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (electric-indent-local-mode 1)
                   (newline 1 'interactive))
                 '("(a b\n   " . "c d)"))))


;;; `open-line'
(ert-deftest open-line ()
  (should-error (open-line -1))
  (should-error (open-line))
  (should (equal (simple-test--dummy-buffer (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-mode -1)
                   (call-interactively #'open-line))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((current-prefix-arg 5))
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer (open-line 5))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (open-line 1))
                 '("(a b " . "\nc d)"))))

(ert-deftest open-line-margin-and-prefix ()
  (should (equal (simple-test--dummy-buffer
                   (let ((left-margin 10))
                     (open-line 3)))
                 '("(a b" . "\n\n\n          c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-line 0)
                   (let ((left-margin 2))
                     (open-line 1)))
                 '("  " . "\n  (a b c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((fill-prefix "- - "))
                     (open-line 1)))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-line 0)
                   (let ((fill-prefix "- - "))
                     (open-line 1)))
                 '("- - " . "\n(a b c d)"))))

(ert-deftest open-line-indent ()
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 1 'interactive))
                 '("(a b" . "\n   c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg nil))
                     (call-interactively #'open-line)
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n   c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 5 'interactive))
                 '("(a b" . "\n\n\n\n\n   c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg 5))
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n\n\n\n   c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (electric-indent-local-mode 1)
                   (open-line 1 'interactive))
                 '("(a b" . "\n   c d)"))))

(ert-deftest open-line-hook ()
  (let* ((x 0)
         (inc (lambda () (setq x (1+ x)))))
    (simple-test--dummy-buffer
      (add-hook 'post-self-insert-hook inc nil 'local)
      (open-line 1))
    (should (= x 0))
    (simple-test--dummy-buffer
      (add-hook 'post-self-insert-hook inc nil 'local)
      (open-line 1 'interactive))
    (should (= x 1))

    (unwind-protect
        (progn
          (add-hook 'post-self-insert-hook inc)
          (simple-test--dummy-buffer
            (open-line 1))
          (should (= x 1))
          (simple-test--dummy-buffer
            (open-line 10 'interactive))
          (should (= x 2)))
      (remove-hook 'post-self-insert-hook inc))))


;;; `delete-trailing-whitespace'
(ert-deftest simple-delete-trailing-whitespace ()
  "Test bug#21766: delete-whitespace sometimes deletes non-whitespace."
  (defvar python-indent-guess-indent-offset)  ; to avoid a warning
  (let ((python (featurep 'python))
        (python-indent-guess-indent-offset nil)
        (delete-trailing-lines t))
    (unwind-protect
        (with-temp-buffer
          (python-mode)
          (insert (concat "query = \"\"\"WITH filtered AS \n"
                          "WHERE      \n"
                          "\"\"\".format(fv_)\n"
                          "\n"
                          "\n"))
          (delete-trailing-whitespace)
          (should (equal (count-lines (point-min) (point-max)) 3)))
      ;; Let's clean up if running interactive
      (unless (or noninteractive python)
        (unload-feature 'python)))))

(provide 'simple-test)
;;; simple-test.el ends here
