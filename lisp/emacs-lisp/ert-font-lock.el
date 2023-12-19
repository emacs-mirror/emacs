;;; ert-font-lock.el --- ERT Font Lock   -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Vladimir Kazanov
;; Keywords: lisp, tools

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
;;
;; ERT Font Lock is an extension to the Emacs Lisp Regression Test
;; library (ERT) providing a convenient way to check syntax
;; highlighting provided by font-lock.
;;
;; ert-font-lock entry points are functions
;; `ert-font-lock-test-string' and `ert-font-lock-test-file' and
;; convenience macros: `ert-font-lock-deftest' and
;; `ert-font-lock-deftest-file'.
;;
;; See unit tests in ert-font-lock-tests.el for usage examples.

;;; Code:

(require 'ert)
(require 'newcomment)
(require 'pcase)

(defconst ert-font-lock--assertion-re
  (rx
   ;; column specifiers
   (group (or "^" "<-"))
   (one-or-more " ")
   ;; optional negation of the face specification
   (group (optional "!"))
   ;; face symbol name
   (group (one-or-more (or alphanumeric "-" "_" "."))))
  "An ert-font-lock assertion regex.")

(defun ert-font-lock--validate-major-mode (mode)
  "Validate if MODE is a valid major mode."
  (unless (functionp mode)
    (error "Invalid major mode: %S. Please specify a valid major mode for
 syntax highlighting tests" mode)))

(defun ert-font-lock--test-body-str (mode str test-name)
  "Run assertions from STR.
Argument MODE - major mode to test.
Argument TEST-NAME - name of the currently running ert test."
  (ert-font-lock--validate-major-mode mode)
  (with-temp-buffer
    (insert str)
    (funcall mode)
    (font-lock-ensure)
    (let ((tests (ert-font-lock--parse-comments)))
      (ert-font-lock--check-faces tests)))
  test-name)

(defun ert-font-lock--test-body-file (mode file test-name)
  "Run assertions from FILE.
Argument MODE - major mode to test.
Argument TEST-NAME - name of the currently running ert test."
  (ert-font-lock--validate-major-mode mode)
  (ert-font-lock-test-file file mode)
  test-name)

(defun ert-font-lock--parse-macro-args (doc-keys-mode-arg)
  "Parse DOC-KEYS-MODE-ARG macro argument list."
  (let (doc doc-p mode arg)

    (when (stringp (car doc-keys-mode-arg))
      (setq doc (pop doc-keys-mode-arg)
            doc-p t))

    (pcase-let
        ((`(,keys ,mode-arg)
          (ert--parse-keys-and-body doc-keys-mode-arg)))

      (unless (symbolp (car mode-arg))
        (error "A major mode symbol expected: %S" (car mode-arg)))
      (setq mode (pop mode-arg))

      (unless (stringp (car mode-arg))
        (error "A string or file with assertions expected: %S" (car mode-arg)))
      (setq arg (pop mode-arg))

      (list doc doc-p keys mode arg))))

;;;###autoload
(defmacro ert-font-lock-deftest (name &rest docstring-keys-mode-and-str)
  "Define test NAME (a symbol) using assertions from TEST-STR.

Other than MAJOR-MODE and TEST-STR parameters, this macro accepts
the same parameters and keywords as `ert-deftest' and is intended
to be used through `ert'.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] MAJOR-MODE TEST-STR)"
  (declare (debug (&define [&name "test@" symbolp]
                           sexp [&optional stringp]
                           [&rest keywordp sexp]
                           symbolp
                           stringp))
           (doc-string 3)
           (indent 2))
  (pcase-let ((`(,documentation
                 ,documentation-supplied-p
                 ,keys ,mode ,arg)
               (ert-font-lock--parse-macro-args docstring-keys-mode-and-str)))

    `(ert-set-test ',name
                   (make-ert-test
                    :name ',name
                    ,@(when documentation-supplied-p
                        `(:documentation ,documentation))
                    ,@(when (map-contains-key keys :expected-result)
                        `(:expected-result-type ,(map-elt keys :expected-result)))
                    ,@(when (map-contains-key keys :tags)
                        `(:tags ,(map-elt keys :tags)))
                    :body (lambda () (ert-font-lock--test-body-str ',mode ,arg ',name))

                    :file-name ,(or (macroexp-file-name) buffer-file-name)))))

;;;###autoload
(defmacro ert-font-lock-deftest-file (name &rest docstring-keys-mode-and-file)
  "Define test NAME (a symbol) using assertions from FILE.

FILE - path to a file with assertions in ERT resource director as
return by `ert-resource-directory'.

Other than MAJOR-MODE and FILE parameters, this macro accepts the
same parameters and keywords as `ert-deftest' and is intended to
be used through `ert'.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] MAJOR-MODE FILE)"
  (declare (debug (&define [&name "test@" symbolp]
                           sexp [&optional stringp]
                           [&rest keywordp sexp]
                           symbolp
                           stringp))
           (doc-string 3)
           (indent 2))

  (pcase-let ((`(,documentation
                 ,documentation-supplied-p
                 ,keys ,mode ,arg)
               (ert-font-lock--parse-macro-args docstring-keys-mode-and-file)))

    `(ert-set-test ',name
                   (make-ert-test
                    :name ',name
                    ,@(when documentation-supplied-p
                        `(:documentation ,documentation))
                    ,@(when (map-contains-key keys :expected-result)
                        `(:expected-result-type ,(map-elt keys :expected-result)))
                    ,@(when (map-contains-key keys :tags)
                        `(:tags ,(map-elt keys :tags)))
                    :body (lambda () (ert-font-lock--test-body-file
                                 ',mode (ert-resource-file ,arg) ',name))
                    :file-name ,(or (macroexp-file-name) buffer-file-name)))))

(defun ert-font-lock--in-comment-p ()
  "Check if the current point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun ert-font-lock--comment-start-p ()
  "Check if the current point starts a comment."
  (or
   ;; regexps use syntax tables so let's check that first
   (looking-at "\\s<")

   ;; check newcomment.el facilities
   (and comment-start (looking-at (regexp-quote comment-start)))
   (and comment-start-skip (looking-at comment-start-skip))

   ;; sometimes comment syntax is just hardcoded
   (and (derived-mode-p '(c-mode c++-mode java-mode))
        (looking-at-p "//"))))

(defun ert-font-lock--line-comment-p ()
  "Return t if the current line is a comment-only line."
  (syntax-ppss)
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward " ")
    ;; skip empty lines
    (unless (eolp)
      (or
       ;; multiline comments
       (ert-font-lock--in-comment-p)

       ;; single line comments
       (ert-font-lock--comment-start-p)))))

(defun ert-font-lock--line-assertion-p ()
  "Return t if the current line contains an assertion."
  (syntax-ppss)
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward " ")
    (re-search-forward ert-font-lock--assertion-re
                       (line-end-position) t 1)))

(defun ert-font-lock--goto-first-char ()
  "Move the point to the first character."
  (beginning-of-line)
  (skip-syntax-forward " "))

(defun ert-font-lock--get-first-char-column ()
  "Get the position of the first non-empty char in the current line."
  (save-excursion
    (ert-font-lock--goto-first-char)
    (- (point) (line-beginning-position))))

(defun ert-font-lock--parse-comments ()
  "Read test assertions from comments in the current buffer."
  (let ((tests '())
        (curline 1)
        (linetocheck -1))

    (goto-char (point-min))

    ;; Go through all lines, for comments check if there are
    ;; assertions. For non-comment and comment/non-assert lines
    ;; remember the last line seen.
    (while (not (eobp))
      (catch 'nextline

        ;; Not a comment? remember the line, move to the next one
        (unless (ert-font-lock--line-comment-p)
          (setq linetocheck curline)
          (throw 'nextline t))

        ;; A comment. Not an assertion? remember the line to be
        ;; checked, move to the next line
        (unless (ert-font-lock--line-assertion-p)
          (setq linetocheck curline)
          (throw 'nextline t))


        ;; Collect the assertion
        (when (re-search-forward ert-font-lock--assertion-re
                                 (line-end-position) t 1)

          (unless (> linetocheck -1)
            (user-error "Invalid test comment syntax at line %d. Expected a line to test before the comment line" curline))

          ;; construct a test
          (let* (;; either comment start char column (for arrows) or
                 ;; caret column
                 (column-checked (if (equal (match-string-no-properties 1) "^")
                                     (- (match-beginning 1) (line-beginning-position))
                                   (ert-font-lock--get-first-char-column)))
                 ;; negate the face?
                 (negation (string-equal (match-string-no-properties 2) "!"))
                 ;; the face that is supposed to be in the position specified
                 (face (match-string-no-properties 3)))

            (push (list :line-checked linetocheck
                        :line-assert curline
                        :column-checked column-checked
                        :face face
                        :negation negation)
                  tests))))

      ;; next line
      (setq curline (1+ curline))
      (forward-line 1))

    (reverse tests)))

(defun ert-font-lock--point-at-line-and-column (line column)
  "Get the buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun ert-font-lock--get-line (line-number)
  "Return the content of the line specified by LINE-NUMBER."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ert-font-lock--check-faces (tests)
  "Check if the current buffer is fontified correctly.
TESTS - tests to run.

The function is meant to be run from within an ERT test."
  (dolist (test tests)
    (let* ((line-checked (plist-get test :line-checked))
           (line-assert (plist-get test :line-assert))
           (column-checked (plist-get test :column-checked))
           (expected-face (intern (plist-get test :face)))
           (negation (plist-get test :negation))

           (actual-face (get-text-property (ert-font-lock--point-at-line-and-column line-checked column-checked) 'face))
           (line-str (ert-font-lock--get-line line-checked))
           (line-assert-str (ert-font-lock--get-line line-assert)))

      (when (not (eq actual-face expected-face))
        (ert-fail
         (list (format "Expected face %S, got %S on line %d column %d"
                       expected-face actual-face line-checked column-checked)
               :line line-str
               :assert line-assert-str)))

      (when (and negation (eq actual-face expected-face))
        (ert-fail
         (list (format "Did not expect face %S face on line %d, column %d"
                       actual-face line-checked column-checked)
               :line line-str
               :assert line-assert-str))))))

;;;###autoload
(defun ert-font-lock-test-string (test-string mode)
  "Check font faces in TEST-STRING set by MODE.

The function is meant to be run from within an ERT test."
  (ert-font-lock--validate-major-mode mode)
  (with-temp-buffer
    (insert test-string)
    (funcall mode)
    (font-lock-ensure)

    (ert-font-lock--check-faces (ert-font-lock--parse-comments)))

  (ert-pass))

;;;###autoload
(defun ert-font-lock-test-file (filename mode)
  "Check font faces in FILENAME set by MODE.

The function is meant to be run from within an ERT test."
  (ert-font-lock--validate-major-mode mode)
  (with-temp-buffer
    (insert-file-contents filename)
    (funcall mode)
    (font-lock-ensure)

    (ert-font-lock--check-faces (ert-font-lock--parse-comments)))

  (ert-pass))


(provide 'ert-font-lock)

;;; ert-font-lock.el ends here
