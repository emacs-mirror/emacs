;;; electric-tests.el --- tests for electric.el

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords:

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

;; Tests for Electric Pair mode.
;; TODO: Add tests for other Electric-* functionality

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'electric)
(require 'elec-pair)
(require 'cl-lib)

(defun call-with-saved-electric-modes (fn)
  (let ((saved-electric (if electric-pair-mode 1 -1))
        (saved-layout (if electric-layout-mode 1 -1))
        (saved-indent (if electric-indent-mode 1 -1)))
    (electric-pair-mode -1)
    (electric-layout-mode -1)
    (electric-indent-mode -1)
    (unwind-protect
        (funcall fn)
      (electric-pair-mode saved-electric)
      (electric-indent-mode saved-indent)
      (electric-layout-mode saved-layout))))

(defmacro save-electric-modes (&rest body)
  (declare (indent defun) (debug t))
  `(call-with-saved-electric-modes #'(lambda () ,@body)))

(defun electric-pair-test-for (fixture where char expected-string
                                       expected-point mode bindings fixture-fn)
  (with-temp-buffer
    (funcall mode)
    (insert fixture)
    (save-electric-modes
      (let ((last-command-event char)
            (transient-mark-mode 'lambda))
        (goto-char where)
        (funcall fixture-fn)
        (cl-progv
            (mapcar #'car bindings)
            (mapcar #'cdr bindings)
          (call-interactively (key-binding `[,last-command-event])))))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   expected-string))
    (should (equal (point)
                   expected-point))))

(eval-when-compile
  (defun electric-pair-define-test-form (name fixture
                                              char
                                              pos
                                              expected-string
                                              expected-point
                                              skip-pair-string
                                              prefix
                                              suffix
                                              extra-desc
                                              mode
                                              bindings
                                              fixture-fn)
    (let* ((expected-string-and-point
            (if skip-pair-string
                (with-temp-buffer
                  (cl-progv
                      ;; FIXME: avoid `eval'
                      (mapcar #'car (eval bindings))
                      (mapcar #'cdr (eval bindings))
                    (funcall mode)
                    (insert fixture)
                    (goto-char (1+ pos))
                    (insert char)
                    (cond ((eq (aref skip-pair-string pos)
                               ?p)
                           (insert (cadr (electric-pair-syntax-info char)))
                           (backward-char 1))
                          ((eq (aref skip-pair-string pos)
                               ?s)
                           (delete-char -1)
                           (forward-char 1)))
                    (list
                     (buffer-substring-no-properties (point-min) (point-max))
                     (point))))
              (list expected-string expected-point)))
           (expected-string (car expected-string-and-point))
           (expected-point (cadr expected-string-and-point))
           (fixture (format "%s%s%s" prefix fixture suffix))
           (expected-string (format "%s%s%s" prefix expected-string suffix))
           (expected-point (+ (length prefix) expected-point))
           (pos (+ (length prefix) pos)))
      `(ert-deftest ,(intern (format "electric-pair-%s-at-point-%s-in-%s%s"
                                     name
                                     (1+ pos)
                                     mode
                                     extra-desc))
           ()
         ,(format "Electricity test in a `%s' buffer.\n
Start with point at %d in a %d-char-long buffer
like this one:

  |%s|   (buffer start and end are denoted by `|')
%s
%s
Now press the key for: %c

The buffer's contents should %s:

  |%s|

, and point should be at %d."
                  mode
                  (1+ pos)
                  (length fixture)
                  fixture
                  (if fixture-fn (format "\nNow call this:\n\n%s"
                                         (pp-to-string fixture-fn)) "")
                  (if bindings (format "\nEnsure the following bindings:\n\n%s"
                                       (pp-to-string bindings)) "")
                  char
                  (if (string= fixture expected-string) "stay" "become")
                  (replace-regexp-in-string "\n" "\\\\n" expected-string)
                  expected-point)
         (electric-pair-test-for ,fixture
                                 ,(1+ pos)
                                 ,char
                                 ,expected-string
                                 ,expected-point
                                 ',mode
                                 ,bindings
                                 ,fixture-fn)))))

(cl-defmacro define-electric-pair-test
    (name fixture
          input
          &key
          skip-pair-string
          expected-string
          expected-point
          bindings
          (modes '(quote (ruby-mode js-mode)))
          (test-in-comments t)
          (test-in-strings t)
          (test-in-code t)
          (fixture-fn #'(lambda ()
                          (electric-pair-mode 1))))
  `(progn
     ,@(cl-loop
        for mode in (eval modes) ;FIXME: avoid `eval'
        append
        (cl-loop
         for (prefix suffix extra-desc) in
         (append (if test-in-comments
                     `((,(with-temp-buffer
                           (funcall mode)
                           (insert "z")
                           (comment-region (point-min) (point-max))
                           (buffer-substring-no-properties (point-min)
                                                           (1- (point-max))))
                        ""
                        "-in-comments")))
                 (if test-in-strings
                     '(("\"" "\"" "-in-strings")))
                 (if test-in-code
                     '(("" "" ""))))
         append
         (cl-loop
          for char across input
          for pos from 0
          unless (eq char ?-)
          collect (electric-pair-define-test-form
                   name
                   fixture
                   (aref input pos)
                   pos
                   expected-string
                   expected-point
                   skip-pair-string
                   prefix
                   suffix
                   extra-desc
                   mode
                   bindings
                   fixture-fn))))))

;;; Basic pairs and skips
;;;
(define-electric-pair-test balanced-situation
  " (())  " "(((((((" :skip-pair-string "ppppppp"
  :modes '(ruby-mode))

(define-electric-pair-test too-many-openings
  " ((()) " "(((((((" :skip-pair-string "ppppppp")

(define-electric-pair-test too-many-closings
  " (())) " "(((((((" :skip-pair-string "------p")

(define-electric-pair-test too-many-closings-2
  "()   ) " "---(---" :skip-pair-string "-------")

(define-electric-pair-test too-many-closings-3
  ")()    " "(------" :skip-pair-string "-------")

(define-electric-pair-test balanced-autoskipping
  " (())  " "---))--" :skip-pair-string "---ss--")

(define-electric-pair-test too-many-openings-autoskipping
  " ((()) " "----))-" :skip-pair-string "-------")

(define-electric-pair-test too-many-closings-autoskipping
  " (())) " "---)))-" :skip-pair-string "---sss-")


;;; Mixed parens
;;;
(define-electric-pair-test mixed-paren-1
  "  ()]  " "-(-(---" :skip-pair-string "-p-p---")

(define-electric-pair-test mixed-paren-2
  "  [()  " "-(-()--" :skip-pair-string "-p-ps--")

(define-electric-pair-test mixed-paren-3
  "  (])  " "-(-()--" :skip-pair-string "---ps--")

(define-electric-pair-test mixed-paren-4
  "  ()]  " "---)]--" :skip-pair-string "---ss--")

(define-electric-pair-test mixed-paren-5
  "  [()  " "----(--" :skip-pair-string "----p--")

(define-electric-pair-test find-matching-different-paren-type
  "  ()]  " "-[-----" :skip-pair-string "-------")

(define-electric-pair-test find-matching-different-paren-type-inside-list
  "( ()]) " "-[-----" :skip-pair-string "-------")

(define-electric-pair-test ignore-different-nonmatching-paren-type
  "( ()]) " "-(-----" :skip-pair-string "-p-----")

(define-electric-pair-test autopair-keep-least-amount-of-mixed-unbalance
  "( ()]  " "-(-----" :skip-pair-string "-p-----")

(define-electric-pair-test dont-autopair-to-resolve-mixed-unbalance
  "( ()]  " "-[-----" :skip-pair-string "-------")

(define-electric-pair-test autopair-so-as-not-to-worsen-unbalance-situation
  "( (])  " "-[-----" :skip-pair-string "-p-----")

(define-electric-pair-test skip-over-partially-balanced
  " [([])   " "-----)---" :skip-pair-string "-----s---")

(define-electric-pair-test only-skip-over-at-least-partially-balanced-stuff
  " [([())  " "-----))--" :skip-pair-string "-----s---")




;;; Quotes
;;;
(define-electric-pair-test pair-some-quotes-skip-others
  " \"\"      " "-\"\"-----" :skip-pair-string "-ps------"
  :test-in-strings nil
  :bindings `((electric-pair-text-syntax-table
               . ,prog-mode-syntax-table)))

(define-electric-pair-test skip-single-quotes-in-ruby-mode
  " '' " "--'-" :skip-pair-string "--s-"
  :modes '(ruby-mode)
  :test-in-comments nil
  :test-in-strings nil
  :bindings `((electric-pair-text-syntax-table
               . ,prog-mode-syntax-table)))

(define-electric-pair-test leave-unbalanced-quotes-alone
  " \"' " "-\"'-" :skip-pair-string "----"
  :modes '(ruby-mode)
  :test-in-strings nil
  :bindings `((electric-pair-text-syntax-table
               . ,prog-mode-syntax-table)))

(define-electric-pair-test leave-unbalanced-quotes-alone-2
  " \"\\\"' " "-\"--'-" :skip-pair-string "------"
  :modes '(ruby-mode)
  :test-in-strings nil
  :bindings `((electric-pair-text-syntax-table
               . ,prog-mode-syntax-table)))

(define-electric-pair-test leave-unbalanced-quotes-alone-3
  " foo\\''" "'------" :skip-pair-string "-------"
  :modes '(ruby-mode)
  :test-in-strings nil
  :bindings `((electric-pair-text-syntax-table
               . ,prog-mode-syntax-table)))

(define-electric-pair-test inhibit-if-strings-mismatched
  "\"foo\"\"bar" "\""
  :expected-string "\"\"foo\"\"bar"
  :expected-point 2
  :test-in-strings nil
  :bindings `((electric-pair-text-syntax-table
               . ,prog-mode-syntax-table)))

(define-electric-pair-test inhibit-in-mismatched-string-inside-ruby-comments
  "foo\"\"
#
#    \"bar\"
#    \"   \"
#    \"
#
baz\"\""
  "\""
  :modes '(ruby-mode)
  :test-in-strings nil
  :test-in-comments nil
  :expected-point 19
  :expected-string
  "foo\"\"
#
#    \"bar\"\"
#    \"   \"
#    \"
#
baz\"\""
  :fixture-fn #'(lambda () (goto-char (point-min)) (search-forward "bar")))

(define-electric-pair-test inhibit-in-mismatched-string-inside-c-comments
  "foo\"\"/*
    \"bar\"
    \"   \"
    \"
*/baz\"\""
  "\""
  :modes '(c-mode)
  :test-in-strings nil
  :test-in-comments nil
  :expected-point 18
  :expected-string
  "foo\"\"/*
    \"bar\"\"
    \"   \"
    \"
*/baz\"\""
  :fixture-fn #'(lambda () (goto-char (point-min)) (search-forward "bar")))


;;; More quotes, but now don't bind `electric-pair-text-syntax-table'
;;; to `prog-mode-syntax-table'. Use the defaults for
;;; `electric-pair-pairs' and `electric-pair-text-pairs'.
;;;
(define-electric-pair-test pairing-skipping-quotes-in-code
  " \"\"      " "-\"\"-----" :skip-pair-string "-ps------"
  :test-in-strings nil
  :test-in-comments nil)

(define-electric-pair-test skipping-quotes-in-comments
  " \"\"      " "--\"-----" :skip-pair-string "--s------"
  :test-in-strings nil)


;;; Skipping over whitespace
;;;
(define-electric-pair-test whitespace-jumping
  " (    )  " "--))))---" :expected-string " (    )  " :expected-point 8
  :bindings '((electric-pair-skip-whitespace . t)))

(define-electric-pair-test whitespace-chomping
  " (    )  " "--)------" :expected-string " ()  " :expected-point 4
  :bindings '((electric-pair-skip-whitespace . chomp)))

(define-electric-pair-test whitespace-chomping-2
  " ( \n\t\t\n  )  " "--)------" :expected-string " ()  " :expected-point 4
  :bindings '((electric-pair-skip-whitespace . chomp))
  :test-in-comments nil)

(ert-deftest electric-pair-whitespace-chomping-2-at-point-4-in-c++-mode-in-strings nil
  "Check if whitespace chomping works in `c++' unterminated strings."
  (electric-pair-test-for
   "\" ( \n		\n  )  \"" 4 41 "\" ()  \"" 5 'c++-mode
   '((electric-pair-skip-whitespace . chomp))
   (lambda () (electric-pair-mode 1))))

(define-electric-pair-test whitespace-chomping-dont-cross-comments
  " ( \n\t\t\n  )  " "--)------" :expected-string " () \n\t\t\n  )  "
  :expected-point 4
  :bindings '((electric-pair-skip-whitespace . chomp))
  :test-in-strings nil
  :test-in-code nil
  :test-in-comments t)

(define-electric-pair-test whitespace-skipping-for-quotes-not-outside
  "  \"  \"" "\"-----" :expected-string "\"\"  \"  \""
  :expected-point 2
  :bindings '((electric-pair-skip-whitespace . chomp))
  :test-in-strings nil
  :test-in-code t
  :test-in-comments nil)

(define-electric-pair-test whitespace-skipping-for-quotes-only-inside
  "  \"  \"" "---\"--" :expected-string "  \"\""
  :expected-point 5
  :bindings '((electric-pair-skip-whitespace . chomp))
  :test-in-strings nil
  :test-in-code t
  :test-in-comments nil)

(define-electric-pair-test whitespace-skipping-quotes-not-without-proper-syntax
  "  \"  \"" "---\"--" :expected-string "  \"\"\"  \""
  :expected-point 5
  :modes '(text-mode)
  :bindings '((electric-pair-skip-whitespace . chomp))
  :test-in-strings nil
  :test-in-code t
  :test-in-comments nil)


;;; Pairing arbitrary characters
;;;
(define-electric-pair-test angle-brackets-everywhere
  "<>" "<>" :skip-pair-string "ps"
  :bindings '((electric-pair-pairs . ((?\< . ?\>)))))

(define-electric-pair-test angle-brackets-everywhere-2
  "(<>" "-<>" :skip-pair-string "-ps"
  :bindings '((electric-pair-pairs . ((?\< . ?\>)))))

(defvar electric-pair-test-angle-brackets-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?\< "(>" table)
    (modify-syntax-entry ?\> ")<`" table)
    table))

(define-electric-pair-test angle-brackets-pair
  "<>" "<" :expected-string "<><>" :expected-point 2
  :test-in-code nil
  :bindings `((electric-pair-text-syntax-table
               . ,electric-pair-test-angle-brackets-table)))

(define-electric-pair-test angle-brackets-skip
  "<>" "->" :expected-string "<>" :expected-point 3
  :test-in-code nil
  :bindings `((electric-pair-text-syntax-table
               . ,electric-pair-test-angle-brackets-table)))

(define-electric-pair-test pair-backtick-and-quote-in-comments
  ";; " "---`" :expected-string ";; `'" :expected-point 5
  :test-in-comments nil
  :test-in-strings nil
  :modes '(emacs-lisp-mode)
  :bindings '((electric-pair-text-pairs . ((?\` . ?\')))))

(define-electric-pair-test skip-backtick-and-quote-in-comments
  ";; `foo'" "-------'" :expected-string ";; `foo'" :expected-point 9
  :test-in-comments nil
  :test-in-strings nil
  :modes '(emacs-lisp-mode)
  :bindings '((electric-pair-text-pairs . ((?\` . ?\')))))

(define-electric-pair-test pair-backtick-and-quote-in-strings
  "\"\"" "-`" :expected-string "\"`'\"" :expected-point 3
  :test-in-comments nil
  :test-in-strings nil
  :modes '(emacs-lisp-mode)
  :bindings '((electric-pair-text-pairs . ((?\` . ?\')))))

(define-electric-pair-test skip-backtick-and-quote-in-strings
  "\"`'\"" "--'" :expected-string "\"`'\"" :expected-point 4
  :test-in-comments nil
  :test-in-strings nil
  :modes '(emacs-lisp-mode)
  :bindings '((electric-pair-text-pairs . ((?\` . ?\')))))

(define-electric-pair-test skip-backtick-and-quote-in-strings-2
  "  \"`'\"" "----'" :expected-string "  \"`'\"" :expected-point 6
  :test-in-comments nil
  :test-in-strings nil
  :modes '(emacs-lisp-mode)
  :bindings '((electric-pair-text-pairs . ((?\` . ?\')))))


;;; `js-mode' has `electric-layout-rules' for '{ and '}
;;;
(define-electric-pair-test js-mode-braces
  "" "{" :expected-string "{}" :expected-point 2
  :modes '(js-mode)
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)))


(define-electric-pair-test js-mode-braces-with-layout
  "" "{" :expected-string "{\n\n}" :expected-point 3
  :modes '(js-mode)
  :test-in-comments nil
  :test-in-strings nil
  :fixture-fn #'(lambda ()
                  (electric-layout-mode 1)
                  (electric-pair-mode 1)))

(define-electric-pair-test js-mode-braces-with-layout-and-indent
  "" "{" :expected-string "{\n    \n}" :expected-point 7
  :modes '(js-mode)
  :test-in-comments nil
  :test-in-strings nil
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (electric-indent-mode 1)
                  (electric-layout-mode 1)))

(define-electric-pair-test js-mode-braces-with-layout-and-indent
  "" "{" :expected-string "{\n    \n}" :expected-point 7
  :modes '(js-mode)
  :test-in-comments nil
  :test-in-strings nil
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (electric-indent-mode 1)
                  (electric-layout-mode 1)))


;;; Backspacing
;;; TODO: better tests
;;;
(ert-deftest electric-pair-backspace-1 ()
  (save-electric-modes
    (with-temp-buffer
      (insert "()")
      (goto-char 2)
      (electric-pair-delete-pair 1)
      (should (equal "" (buffer-string))))))


;;; Electric newlines between pairs
;;; TODO: better tests
(ert-deftest electric-pair-open-extra-newline ()
  (save-electric-modes
    (with-temp-buffer
      (c-mode)
      (electric-pair-mode 1)
      (electric-indent-mode 1)
      (insert "int main {}")
      (backward-char 1)
      (let ((c-basic-offset 4))
        (newline 1 t)
        (should (equal "int main {\n    \n}"
                       (buffer-string)))
        (should (equal (point) (- (point-max) 2)))))))



;;; Autowrapping
;;;
(define-electric-pair-test autowrapping-1
  "foo" "(" :expected-string "(foo)" :expected-point 2
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (mark-sexp 1)))

(define-electric-pair-test autowrapping-2
  "foo" ")" :expected-string "(foo)" :expected-point 6
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (mark-sexp 1)))

(define-electric-pair-test autowrapping-3
  "foo" ")" :expected-string "(foo)" :expected-point 6
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (goto-char (point-max))
                  (skip-chars-backward "\"")
                  (mark-sexp -1)))

(define-electric-pair-test autowrapping-4
  "foo" "(" :expected-string "(foo)" :expected-point 2
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (goto-char (point-max))
                  (skip-chars-backward "\"")
                  (mark-sexp -1)))

(define-electric-pair-test autowrapping-5
  "foo" "\"" :expected-string "\"foo\"" :expected-point 2
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (mark-sexp 1)))

(define-electric-pair-test autowrapping-6
  "foo" "\"" :expected-string "\"foo\"" :expected-point 6
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (goto-char (point-max))
                  (skip-chars-backward "\"")
                  (mark-sexp -1)))

(define-electric-pair-test autowrapping-7
  "foo" "\"" :expected-string "``foo''" :expected-point 8
  :modes '(tex-mode)
  :test-in-comments nil
  :fixture-fn #'(lambda ()
                  (electric-pair-mode 1)
                  (goto-char (point-max))
                  (skip-chars-backward "\"")
                  (mark-sexp -1)))


;;; Electric quotes
(define-electric-pair-test electric-quote-string
  "" "'" :expected-string "'" :expected-point 2
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-string . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-opening-single
  "" "`" :expected-string "‘" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-closing-single
  "" "'" :expected-string "’" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-opening-double
  "‘" "-`" :expected-string "“" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-closing-double
  "’" "-'" :expected-string "”" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-disabled
  "" "\"" :expected-string "\"" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-backtick
  "" "`" :expected-string "`" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-bob-single
  "" "'" :expected-string "‘" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-bob-double
  "‘" "-'" :expected-string "“" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-bob
  "" "\"" :expected-string "“" :expected-point 2
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-bol-single
  "a\n" "--'" :expected-string "a\n‘" :expected-point 4
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-bol-double
  "a\n‘" "---'" :expected-string "a\n“" :expected-point 4
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-bol
  "a\n" "--\"" :expected-string "a\n“" :expected-point 4
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-after-space-single
  " " "-'" :expected-string " ‘" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-after-space-double
  " ‘" "--'" :expected-string " “" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-after-space
  " " "-\"" :expected-string " “" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-after-letter-single
  "a" "-'" :expected-string "a’" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-after-letter-double
  "a’" "--'" :expected-string "a”" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-after-letter
  "a" "-\"" :expected-string "a”" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-after-paren-single
  "(" "-'" :expected-string "(‘" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-context-sensitive-after-paren-double
  "(‘" "--'" :expected-string "(“" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-context-sensitive . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-after-paren
  "(" "-\"" :expected-string "(“" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-no-context-single
  " " "-'" :expected-string " ’" :expected-point 3
  :modes '(text-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-replace-double-escaped-open
  "foo \\" "-----\"" :expected-string "foo \\“"
  :expected-point 7 :modes '(emacs-lisp-mode c-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t)
              (electric-quote-comment . t)
              (electric-quote-string . t))
  :test-in-comments t :test-in-strings t :test-in-code nil)

(define-electric-pair-test electric-quote-replace-double-escaped-close
  "foo \\“foo\\" "----------\"" :expected-string "foo \\“foo\\”"
  :expected-point 12 :modes '(emacs-lisp-mode c-mode)
  :fixture-fn #'electric-quote-local-mode
  :bindings '((electric-quote-replace-double . t)
              (electric-quote-comment . t)
              (electric-quote-string . t))
  :test-in-comments t :test-in-strings t :test-in-code nil)

;; Simulate ‘markdown-mode’: it sets both ‘comment-start’ and
;; ‘comment-use-syntax’, but derives from ‘text-mode’.
(define-electric-pair-test electric-quote-markdown-in-text
  "" "'" :expected-string "’" :expected-point 2
  :modes '(text-mode)
  :fixture-fn (lambda ()
                (electric-quote-local-mode)
                (add-hook 'electric-quote-inhibit-functions
                          (lambda ()
                            (save-excursion (search-backward "`" nil t)))
                          nil :local))
  :bindings '((comment-start . "<!--") (comment-use-syntax . t))
  :test-in-comments nil :test-in-strings nil)

(define-electric-pair-test electric-quote-markdown-in-code
  "`a`" "-'" :expected-string "`'a`" :expected-point 3
  :modes '(text-mode)
  :fixture-fn (lambda ()
                (electric-quote-local-mode)
                (add-hook 'electric-quote-inhibit-functions
                          (lambda ()
                            (save-excursion (search-backward "`" nil t)))
                          nil :local))
  :bindings '((comment-start . "<!--") (comment-use-syntax . t))
  :test-in-comments nil :test-in-strings nil)


;;; tests for `electric-layout-mode'

(define-derived-mode plainer-c-mode c-mode "pC"
  "A plainer/saner C-mode with no internal electric machinery."
  (c-toggle-electric-state -1)
  (setq-local electric-indent-local-mode-hook nil)
  (setq-local electric-indent-mode-hook nil)
  (electric-indent-local-mode 1)
  (dolist (key '(?\" ?\' ?\{ ?\} ?\( ?\) ?\[ ?\]))
    (local-set-key (vector key) 'self-insert-command)))

(defun electric-layout-for-c-style-du-jour (inserted)
  "A function to use in `electric-layout-rules'"
  (when (memq inserted '(?{ ?}))
    (save-excursion
      (backward-char 2) (c-point-syntax) (forward-char) ; silly, but needed
      (c-brace-newlines (c-point-syntax)))))

(ert-deftest electric-layout-plainer-c-mode-use-c-style ()
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode 1)
    (electric-pair-local-mode 1)
    (electric-indent-local-mode 1)
    (setq-local electric-layout-rules
                '(electric-layout-for-c-style-du-jour))
    (insert "int main () ")
    (let ((last-command-event ?\{))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main ()\n{\n  \n}\n"))))

(ert-deftest electric-layout-int-main-kernel-style ()
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode 1)
    (electric-pair-local-mode 1)
    (electric-indent-local-mode 1)
    (setq-local electric-layout-rules
                '((?\{ . (after))
                  (?\} . (before))))
    (insert "int main () ")
    (let ((last-command-event ?\{))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main () {\n  \n}"))))

(ert-deftest electric-layout-control-reindentation ()
  "Same as `e-l-int-main-kernel-style', but checking Bug#35254."
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode 1)
    (electric-pair-local-mode 1)
    (electric-indent-local-mode 1)
    (setq-local electric-layout-rules
                '((?\{ . (after))
                  (?\} . (before))))
    (insert "int main () ")
    (let ((last-command-event ?\{))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main () {\n  \n}"))
    ;; insert an additional newline and check indentation and
    ;; reindentation
    (call-interactively 'newline)
    (should (equal (buffer-string) "int main () {\n\n  \n}"))))

(ert-deftest electric-modes-int-main-allman-style ()
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode 1)
    (electric-pair-local-mode 1)
    (electric-indent-local-mode 1)
    (setq-local electric-layout-rules
                '((?\{ . (before after))
                  (?\} . (before))))
    (insert "int main () ")
    (let ((last-command-event ?\{))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main ()\n{\n  \n}"))))

(ert-deftest electric-pair-mode-newline-between-parens ()
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode -1) ;; ensure e-l-m mode is off
    (electric-pair-local-mode 1)
    (insert-before-markers "int main () {}")
    (backward-char 1)
    (let ((last-command-event ?\r))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main () {\n  \n}"))))

(ert-deftest electric-layout-mode-newline-between-parens-without-e-p-m ()
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode 1)
    (electric-pair-local-mode -1) ;; ensure e-p-m mode is off
    (electric-indent-local-mode 1)
    (setq-local electric-layout-rules
                '((?\n
                   .
                   (lambda ()
                     (when (eq (save-excursion
                                 (skip-chars-backward "\t\s")
                                 (char-before (1- (point))))
                               (matching-paren (char-after)))
                       '(after-stay))))))
    (insert "int main () {}")
    (backward-char 1)
    (let ((last-command-event ?\r))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main () {\n  \n}"))))

(ert-deftest electric-layout-mode-newline-between-parens-without-e-p-m-2 ()
  (ert-with-test-buffer ()
    (plainer-c-mode)
    (electric-layout-local-mode 1)
    (electric-pair-local-mode -1) ;; ensure e-p-m mode is off
    (electric-indent-local-mode 1)
    (setq-local electric-layout-rules
                '((lambda (char)
                    (when (and
                           (eq char ?\n)
                           (eq (save-excursion
                                 (skip-chars-backward "\t\s")
                                 (char-before (1- (point))))
                               (matching-paren (char-after))))
                       '(after-stay)))))
    (insert "int main () {}")
    (backward-char 1)
    (let ((last-command-event ?\r))
      (call-interactively (key-binding `[,last-command-event])))
    (should (equal (buffer-string) "int main () {\n  \n}"))))

(provide 'electric-tests)
;;; electric-tests.el ends here
