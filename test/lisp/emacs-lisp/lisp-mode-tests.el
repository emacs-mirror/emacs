;;; lisp-mode-tests.el --- Test Lisp editing commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lisp-mode)
(require 'faceup)


;;; Indentation

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
              (incf n)
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

(ert-deftest indent-sexp-go ()
  "Make sure `indent-sexp' doesn't stop after #s."
  ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31984.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "#s(foo\nbar)\n")
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string) "\
#s(foo
   bar)\n"))))

(ert-deftest indent-sexp-cant-go ()
  "`indent-sexp' shouldn't error before a sexp."
  ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31984#32.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(())")
    (goto-char (1+ (point-min)))
    ;; Paredit calls `indent-sexp' from this position.
    (indent-sexp)
    (should (equal (buffer-string) "(())"))))

(ert-deftest indent-sexp-stop-before-eol-comment ()
  "`indent-sexp' shouldn't look for more sexps after an eol comment."
  ;; See https://debbugs.gnu.org/35286.
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((str "() ;;\n  x"))
      (insert str)
      (goto-char (point-min))
      (indent-sexp)
      ;; The "x" is in the next sexp, so it shouldn't get indented.
      (should (equal (buffer-string) str)))))

(ert-deftest indent-sexp-stop-before-eol-non-lisp ()
  "`indent-sexp' shouldn't be too aggressive in non-Lisp modes."
  ;; See https://debbugs.gnu.org/35286#13.
  (with-temp-buffer
    (prolog-mode)
    (let ((str "\
x(H) -->
    {y(H)}.
a(A) -->
    b(A)."))
      (insert str)
      (search-backward "{")
      (indent-sexp)
      ;; There's no line-spanning sexp, so nothing should be indented.
      (should (equal (buffer-string) str)))))

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
            (incf n)
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

(ert-deftest lisp-indent-with-read-only-field ()
  "Test indentation on line with read-only field (Bug#32014)."
  (with-temp-buffer
    (insert (propertize "prompt> " 'field 'output 'read-only t
                        'rear-nonsticky t 'front-sticky '(read-only)))
    (insert " foo")
    (lisp-indent-line)
    (should (equal (buffer-string) "prompt> foo"))))

(ert-deftest lisp-indent-unfinished-string ()
  "Don't infloop on unfinished string (Bug#37045)."
  (with-temp-buffer
    (insert "\"\n")
    (lisp-indent-region (point-min) (point-max))))

(ert-deftest lisp-indent-defun ()
  (with-temp-buffer
    (lisp-mode)
    (let ((orig "(defun x ()
  (print (quote ( thingy great
		  stuff)))
  (print (quote (thingy great
			stuff))))"))
      (insert orig)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) orig)))))


;;; Filling

(ert-deftest lisp-fill-paragraph-docstring-boundaries ()
  "Test bug#28937, ensuring filling the docstring filled is properly
bounded."
  (with-temp-buffer
    (insert "\
(defun test ()
  \"This is a test docstring.
Here is some more text.\"
  1
  2
  3
  4
  5)")
    (let ((correct (buffer-string)))
      (emacs-lisp-mode)
      (search-backward "This is a test docstring")
      (fill-paragraph)                  ;function under test
      (should (equal (buffer-string) correct)))))

(ert-deftest lisp-fill-paragraph-as-displayed ()
  "Test bug#56197 -- more specifically, validate that a leading indentation
for a string is preserved in the filled string."
  (let ((lisp-fill-paragraphs-as-doc-string nil) ;variable under test
        ;; The following is a contrived example that demonstrates the
        ;; fill-column problem when the string to fill is indented.
        (source "\
'(description \"This is a very long string which is indented by a considerable value, causing it to
protrude from the configured `fill-column' since
lisp-fill-paragraph was refactored in version 28.\")"))
    (with-temp-buffer
      (insert source)
      (emacs-lisp-mode)
      (search-backward "This is a very long string")
      (fill-paragraph)                  ;function under test
      (goto-char (point-min))
      (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
      (let ((i 1)
            (lines-count (count-lines (point-min) (point-max))))
        (while (< i lines-count)
          (beginning-of-line i)
          (end-of-line)
          (should (<= (current-column) fill-column))
          (setq i (1+ i)))))))


;;; Fontification

(ert-deftest lisp-fontify-confusables ()
  "Unescaped 'smart quotes' should be fontified in `font-lock-warning-face'."
  (with-temp-buffer
    (dolist (ch
             '(#x2018 ;; LEFT SINGLE QUOTATION MARK
               #x2019 ;; RIGHT SINGLE QUOTATION MARK
               #x201B ;; SINGLE HIGH-REVERSED-9 QUOTATION MARK
               #x201C ;; LEFT DOUBLE QUOTATION MARK
               #x201D ;; RIGHT DOUBLE QUOTATION MARK
               #x201F ;; DOUBLE HIGH-REVERSED-9 QUOTATION MARK
               #x301E ;; DOUBLE PRIME QUOTATION MARK
               #xFF02 ;; FULLWIDTH QUOTATION MARK
               #xFF07 ;; FULLWIDTH APOSTROPHE
               ))
      (insert (format "«w:%c»foo \\%cfoo\n" ch ch)))
    (let ((faceup (buffer-string)))
      (faceup-clean-buffer)
      (should (faceup-test-font-lock-buffer 'emacs-lisp-mode faceup)))))

(ert-deftest test-lisp-current-defun-name ()
  (require 'edebug)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo ()\n'bar)\n")
    (goto-char 5)
    (should (equal (lisp-current-defun-name) "foo")))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(define-flabbergast-test zot ()\n'bar)\n")
    (goto-char 5)
    (should (equal (lisp-current-defun-name) "zot")))
  ;; These tests should probably work after bug#49592 has been fixed.
  ;; (with-temp-buffer
  ;;   (emacs-lisp-mode)
  ;;   (insert "(progn\n ;; comment\n ;; about that\n (define-key ...)\n )")
  ;;   (goto-char 5)
  ;;   (should (equal (lisp-current-defun-name) "progn")))
  ;; (with-temp-buffer
  ;;   (emacs-lisp-mode)
  ;;   (insert "(defblarg \"a\" 'b)")
  ;;   (goto-char 5)
  ;;   (should (equal (lisp-current-defun-name) "defblarg")))
  )

(ert-deftest test-font-lock-keywords ()
  "Keywords should be fontified in `font-lock-keyword-face`."
  (with-temp-buffer
    (emacs-lisp-mode)
    (mapc (lambda (el-keyword)
            (erase-buffer)
            (insert (format "(%s some-symbol () \"hello\"" el-keyword))
            (font-lock-ensure)
            ;; Verify face property throughout the keyword
            (let* ((begin (1+ (point-min)))
                   (end (1- (+ begin (length el-keyword)))))
              (mapc (lambda (pos)
                      (should (equal (get-text-property pos 'face)
                                     'font-lock-keyword-face)))
                    (number-sequence begin end))))
          '("defsubst" "cl-defsubst" "define-inline"
            "define-advice" "defadvice" "defalias"
            "define-derived-mode" "define-minor-mode"
            "define-generic-mode"
            "define-globalized-minor-mode" "define-skeleton"
            "define-widget" "ert-deftest" "defconst" "defcustom"
            "defvaralias" "defvar-local" "defface" "define-error"))))

(provide 'lisp-mode-tests)
;;; lisp-mode-tests.el ends here
