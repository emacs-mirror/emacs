;;; lisp-mode-tests.el --- Test Lisp editing commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lisp-mode)

(defconst lisp-mode-tests--correctly-indented-sexp "\
\(a
 (prog1
     (prog1
         1
       2)
   2)
 (fun arg1

      arg2)
 (1
  \"string
noindent\" (\"string2
noindent\" 3
4)
  2)                                    ; comment
 ;; comment
 b)")

(ert-deftest indent-sexp ()
  "Test basics of \\[indent-sexp]."
  (with-temp-buffer
    (insert lisp-mode-tests--correctly-indented-sexp)
    (goto-char (point-min))
    (let ((indent-tabs-mode nil)
          (correct lisp-mode-tests--correctly-indented-sexp))
      (dolist (mode '(fundamental-mode emacs-lisp-mode))
        (funcall mode)
        (indent-sexp)
        ;; Don't mess up correctly indented code.
        (should (string= (buffer-string) correct))
        ;; Correctly add indentation.
        (save-excursion
          (while (not (eobp))
            (delete-horizontal-space)
            (forward-line)))
        (indent-sexp)
        (should (equal (buffer-string) correct))
        ;; Correctly remove indentation.
        (save-excursion
          (let ((n 0))
            (while (not (eobp))
              (unless (looking-at "noindent\\|^[[:blank:]]*$")
                (insert (make-string n ?\s)))
              (cl-incf n)
              (forward-line))))
        (indent-sexp)
        (should (equal (buffer-string) correct))))))

(ert-deftest indent-subsexp ()
  "Make sure calling `indent-sexp' inside a sexp works."
  (with-temp-buffer
    (insert "\
\(d1 xx
    (d2 yy
	zz)
    11)")
    (let ((correct (buffer-string)))
      (search-backward "d2")
      (up-list -1)
      (indent-sexp)
      (should (equal (buffer-string) correct))
      (backward-sexp)
      (end-of-line)
      (indent-sexp)
      (should (equal (buffer-string) correct)))))

(ert-deftest indent-sexp-in-string ()
  "Make sure calling `indent-sexp' inside a string works."
  ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21343.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\";\"")
    (let ((correct (buffer-string)))
      (search-backward ";")
      (indent-sexp)
      (should (equal (buffer-string) correct)))))

(ert-deftest indent-sexp-stop ()
  "Make sure `indent-sexp' stops at the end of the sexp."
  ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26878.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a ()\n)")
    (let ((original (buffer-string)))
      (search-backward "a ")
      (goto-char (match-end 0))
      (indent-sexp)
      ;; The final paren should not be indented, because the sexp
      ;; we're indenting ends on the previous line.
      (should (equal (buffer-string) original)))))

(ert-deftest lisp-indent-region ()
  "Test basics of `lisp-indent-region'."
  (with-temp-buffer
    (insert lisp-mode-tests--correctly-indented-sexp)
    (goto-char (point-min))
    (let ((indent-tabs-mode nil)
          (correct lisp-mode-tests--correctly-indented-sexp))
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      ;; Don't mess up correctly indented code.
      (should (string= (buffer-string) correct))
      ;; Correctly add indentation.
      (save-excursion
        (while (not (eobp))
          (delete-horizontal-space)
          (forward-line)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) correct))
      ;; Correctly remove indentation.
      (save-excursion
        (let ((n 0))
          (while (not (eobp))
            (unless (looking-at "noindent\\|^[[:blank:]]*$")
              (insert (make-string n ?\s)))
            (cl-incf n)
            (forward-line))))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) correct)))))


(ert-deftest lisp-indent-region-defun-with-docstring ()
  "Test Bug#26619."
  (with-temp-buffer
    (insert "\
\(defun test ()
  \"This is a test.
Test indentation in emacs-lisp-mode\"
  (message \"Hi!\"))")
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) correct)))))

(ert-deftest lisp-indent-region-open-paren ()
  (with-temp-buffer
    (insert "\
\(with-eval-after-load 'foo
  (setq bar `(
              baz)))")
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) correct)))))

(ert-deftest lisp-indent-region-in-sexp ()
  (with-temp-buffer
    (insert "\
\(when t
  (when t
    (list 1 2 3)
    'etc)
  (quote etc)
  (quote etc))")
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (emacs-lisp-mode)
      (search-backward "1")
      (indent-region (point) (point-max))
      (should (equal (buffer-string) correct)))))

(ert-deftest lisp-indent-region-after-string-literal ()
  (with-temp-buffer
    (insert "\
\(user-error \"Unexpected initialization file: `%s'
Expected initialization file: `%s'\"
            (abbreviate-file-name user-init-file)
            (abbreviate-file-name this-init-file))")
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) correct)))))

(ert-deftest lisp-comment-indent-1 ()
  (with-temp-buffer
    (insert "\
\(let (                                  ;sf
      (x 3))
  4)")
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (comment-indent)
      (should (equal (buffer-string) correct)))))

(ert-deftest lisp-comment-indent-2 ()
  (with-temp-buffer
    (insert "\
\(let (;;sf
      (x 3))
  4)")
    (let ((indent-tabs-mode nil)
          (correct (buffer-string)))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (comment-indent)
      (should (equal (buffer-string) correct)))))


(provide 'lisp-mode-tests)
;;; lisp-mode-tests.el ends here
