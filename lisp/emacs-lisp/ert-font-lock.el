;;; ert-font-lock.el --- ERT Font Lock   -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
;; `ert-font-lock-test-string' and `ert-font-lock-test-file', and
;; convenience macros `ert-font-lock-deftest' and
;; `ert-font-lock-deftest-file'.
;;
;; See unit tests in ert-font-lock-tests.el for usage examples.

;;; Code:

(require 'ert)
(require 'ert-x)

(defconst ert-font-lock--face-symbol-re
  (rx (+ (or alphanumeric "-" "_" "." "/")))
  "A face symbol matching regex.
The regexp cannot use character classes as these can be redefined by the
major mode of the host language.")

(defconst ert-font-lock--face-symbol-list-re
  (rx "("
      (* whitespace)
      (? (regexp ert-font-lock--face-symbol-re))
      (* (+ whitespace)
         (regexp ert-font-lock--face-symbol-re))
      (* whitespace)
      ")")
  "A face symbol list matching regex.")

(defconst ert-font-lock--assertion-line-re
  (rx
   ;; leading column assertion (arrow/caret)
   (group-n 1 (or "^" "<-"))
   (* whitespace)
   ;; possible to have many carets on an assertion line
   (group-n 2 (* "^" (* whitespace)))
   ;; optional negation of the face specification
   (group-n 3 (optional "!"))
   (* whitespace)
   ;; face symbol name or a list of symbols
   (group-n 4 (or (regexp ert-font-lock--face-symbol-re)
                  (regexp ert-font-lock--face-symbol-list-re))))
  "An ert-font-lock assertion line regex.")

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
  (let (doc mode arg)

    (when (stringp (car doc-keys-mode-arg))
      (setq doc (pop doc-keys-mode-arg)))

    (pcase-let
        ((`(,keys ,mode-arg)
          (ert--parse-keys-and-body doc-keys-mode-arg)))

      (unless (symbolp (car mode-arg))
        (error "Expected a major mode symbol: %S" (car mode-arg)))
      (setq mode (pop mode-arg))

      (unless (stringp (car mode-arg))
        (error "Expected a string or file with assertions: %S" (car mode-arg)))
      (setq arg (pop mode-arg))

      (list doc keys mode arg))))

;;;###autoload
(defmacro ert-font-lock-deftest (name &rest docstring-keys-mode-and-str)
  "Define test NAME (a symbol) using assertions from TEST-STR.

The MAJOR-MODE symbol determines the syntax and font lock of TEST-STR.

Except for the MAJOR-MODE and TEST-STR parameters, this macro accepts
the same arguments and keywords as `ert-deftest' and is intended to be
used through `ert'.

\(fn NAME [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] MAJOR-MODE TEST-STR)"
  (declare (debug (&define [&name "test@" symbolp]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           symbolp
                           stringp))
           (doc-string 2)
           (indent 1))
  (pcase-let ((`(,documentation ,keys ,mode ,arg)
               (ert-font-lock--parse-macro-args docstring-keys-mode-and-str)))

    `(ert-set-test ',name
                   (make-ert-test
                    :name ',name
                    ,@(when documentation
                        `(:documentation ,documentation))
                    ,@(when (map-contains-key keys :expected-result)
                        `(:expected-result-type ,(map-elt keys :expected-result)))
                    ,@(when (map-contains-key keys :tags)
                        `(:tags ,(map-elt keys :tags)))
                    :body (lambda ()
                            (ert-font-lock--test-body-str ',mode ,arg ',name))
                    :file-name ,(or (macroexp-file-name) buffer-file-name)))))

;;;###autoload
(defmacro ert-font-lock-deftest-file (name &rest docstring-keys-mode-and-file)
  "Define test NAME (a symbol) using assertions from FILE.

FILE names a file with assertions in the ERT resource directory, as
returned by `ert-resource-directory'.  The MAJOR-MODE symbol determines
the syntax and font lock of FILE's contents.

Except for the MAJOR-MODE and FILE parameters, this macro accepts the
same arguments and keywords as `ert-deftest' and is intended to be used
through `ert'.

\(fn NAME [DOCSTRING] [:expected-result RESULT-TYPE] \
[:tags \\='(TAG...)] MAJOR-MODE FILE)"
  (declare (debug (&define [&name "test@" symbolp]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           symbolp
                           stringp))
           (doc-string 2)
           (indent 1))
  (pcase-let ((`(,documentation ,keys ,mode ,arg)
               (ert-font-lock--parse-macro-args docstring-keys-mode-and-file)))

    `(ert-set-test ',name
                   (make-ert-test
                    :name ',name
                    ,@(when documentation
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
    (re-search-forward ert-font-lock--assertion-line-re
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


        ;; Collect the first line assertion (caret or arrow)
        (when (re-search-forward ert-font-lock--assertion-line-re
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
                 (negation (string-equal (match-string-no-properties 3) "!"))
                 ;; the face that is supposed to be in the position specified
                 (face (read (match-string-no-properties 4))))

            ;; Collect the first assertion on the line
            (push (list :line-checked linetocheck
                        :line-assert curline
                        :column-checked column-checked
                        :face face
                        :negation negation)
                  tests)

            ;; Collect all the other line carets (if present)
            (goto-char (match-beginning 2))
            (while (equal (following-char) ?^)
              (setq column-checked (- (point) (line-beginning-position)))
              (push (list :line-checked linetocheck
                          :line-assert curline
                          :column-checked column-checked
                          :face face
                          :negation negation)
                    tests)
              (forward-char)
              (skip-syntax-forward " ")))))

      ;; next line
      (setq curline (1+ curline))
      (forward-line 1))

    (unless tests
      (user-error "No test assertions found"))

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
           (expected-face (plist-get test :face))
           (negation (plist-get test :negation))

           (actual-face (get-text-property (ert-font-lock--point-at-line-and-column line-checked column-checked) 'face))
           (line-str (ert-font-lock--get-line line-checked))
           (line-assert-str (ert-font-lock--get-line line-assert)))

      ;; normalize both expected and resulting face - these can be
      ;; either symbols, nils or lists of symbols
      (setq actual-face (ensure-list actual-face))
      (setq expected-face (ensure-list expected-face))

      ;; fail when lists are not 'equal and the assertion is *not negated*
      (when (and (not negation) (not (equal actual-face expected-face)))
        (ert-fail
         (list (format "Expected face %S, got %S on line %d column %d"
                       expected-face actual-face line-checked column-checked)
               :line line-str
               :assert line-assert-str)))

      ;; fail when lists are 'equal and the assertion is *negated*
      (when (and negation (equal actual-face expected-face))
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
