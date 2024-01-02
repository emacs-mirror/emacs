;;; macroexp-tests.el --- Tests for macroexp.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

(require 'macroexp)
(require 'ert-x)

(ert-deftest macroexp--tests-fgrep ()
  (should (equal (macroexp--fgrep '((x) (y)) '([x] z ((u))))
                 '((x))))
  (should (equal (macroexp--fgrep '((x) (y)) '#2=([y] ((y #2#))))
                 '((y))))
  (should (equal (macroexp--fgrep '((x) (y)) '#2=([r] ((a x)) a b c d . #2#))
                 '((x)))))

(defconst macroexp--tests-filename (macroexp-file-name))

(defmacro macroexp--test-get-file-name () (macroexp-file-name))

(ert-deftest macroexp--tests-file-name ()
  (should (string-match
           "\\`macroexp-tests.elc?\\'"
           (file-name-nondirectory macroexp--tests-filename)))
  (let ((rsrc-dir (expand-file-name
                   "macroexp-resources"
                   (file-name-directory macroexp--tests-filename))))
    (with-current-buffer
        (find-file-noselect (expand-file-name "m1.el" rsrc-dir))
      (defvar macroexp--m1-tests-filename)
      (declare-function macroexp--m1-tests-file-name "m1" ())
      ;; `macroexp-file-name' should work with `eval-buffer'.
      (eval-buffer)
      (should (equal "m1.el"
                     (file-name-nondirectory macroexp--m1-tests-filename)))
      (should (equal "m1.el"
                     (file-name-nondirectory (macroexp--m1-tests-file-name))))
      (search-forward "macroexp--m1-tests-filename")
      (makunbound 'macroexp--m1-tests-filename)
      ;; `macroexp-file-name' should also work with `eval-defun'.
      (eval-defun nil)
      (should (equal "m1.el"
                     (file-name-nondirectory macroexp--m1-tests-filename))))

    ;; Test the case where we load a file which byte-compiles another.
    (defvar macroexp--m1-tests-comp-filename)
    (makunbound 'macroexp--m1-tests-comp-filename)
    (load (expand-file-name "m2.el" rsrc-dir))
    (should (equal "m1.el"
                   (file-name-nondirectory macroexp--m1-tests-comp-filename)))))

(defun macroexp-tests--run-emacs (&rest args)
  "Run Emacs in batch mode with ARGS, return output."
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((res (apply #'call-process emacs nil t nil
                        "-Q" "--batch" args))
            (output (buffer-string)))
        (unless (equal res 0)
          (message "%s" output)
          (error "Inferior Emacs exited with status %S" res))
        output))))

(defun macroexp-tests--eval-in-subprocess (file expr)
  (let ((output (macroexp-tests--run-emacs
                 "-l" file (format "--eval=(print %S)" expr))))
    (car (read-from-string output))))

(defun macroexp-tests--byte-compile-in-subprocess (file)
  "Byte-compile FILE using a subprocess to avoid contaminating the lisp state."
  (let ((output (macroexp-tests--run-emacs "-f" "batch-byte-compile" file)))
    (when output
      (message "%s" output))))

(ert-deftest macroexp--tests-dynamic-variable-p ()
  "Test `macroexp--dynamic-variable-p'."
  (let* ((vk-el (ert-resource-file "vk.el"))
         (vk-elc (concat vk-el "c"))
         (expr '(list (vk-f1 0)
                      (vk-f2 0)
                      vk-val3
                      (funcall vk-f4 0)
                      (funcall vk-f5 0)
                      (vk-f6)
                      (vk-f7))))
    ;; We compile and run the test in separate processes for complete
    ;; isolation between test cases.
    (should (equal (macroexp-tests--eval-in-subprocess vk-el expr)
                   '((dyn dyn dyn dyn lex lex)
                     (dyn dyn lex lex)
                     (dyn dyn dyn dyn lex lex)
                     (dyn dyn dyn dyn dyn)
                     (dyn dyn dyn lex lex)
                     (dyn dyn dyn dyn)
                     (dyn dyn dyn lex))))
    (macroexp-tests--byte-compile-in-subprocess vk-el)
    (should (equal (macroexp-tests--eval-in-subprocess vk-elc expr)
                   '((dyn dyn dyn dyn lex lex)
                     (dyn dyn lex lex)
                     (dyn dyn dyn dyn lex lex)
                     (dyn dyn dyn dyn dyn)
                     (dyn dyn dyn lex lex)
                     (dyn dyn dyn dyn)
                     (dyn dyn dyn lex))))))

;;; macroexp-tests.el ends here
