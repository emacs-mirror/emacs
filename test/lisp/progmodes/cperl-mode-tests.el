;;; cperl-mode-tests.el --- Test for cperl-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords: internal
;; URL: https://github.com/HaraldJoerg/cperl-mode

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

;; This is a collection of tests for CPerl-mode.
;; The maintainer would like to use this test file with cperl-mode.el
;; also in older Emacs versions (currently: Emacs 26.1): Please don't
;; use Emacs features which are not available in that version (unless
;; they're already used in existing tests).

;;; Code:

(defvar cperl-test-mode #'cperl-mode)

(require 'cperl-mode)
(require 'ert)
(require 'ert-x)

;;; Utilities

(defun cperl-test-ppss (text regexp)
  "Return the `syntax-ppss' after the last character matched by REGEXP in TEXT."
  (interactive)
  (with-temp-buffer
    (insert text)
    (funcall cperl-test-mode)
    (goto-char (point-min))
    (re-search-forward regexp)
    (syntax-ppss)))

(defmacro cperl--run-test-cases (file &rest body)
  "Run all test cases in FILE with BODY.
This macro helps with tests which reformat Perl code, e.g. when
indenting or rearranging flow control.  It extracts source code
snippets and corresponding expected results from a resource file,
runs BODY on the snippets, and compares the resulting buffer with
the expected results.

Test cases in FILE are formatted like this:

# -------- NAME: input --------
Your input to the test case comes here.
Both input and expected output may span several lines.
# -------- NAME: expected output --------
The expected output from running BODY on the input goes here.
# -------- NAME: end --------

You can have many of these blocks in one test file.  You can
chose a NAME for each block, which is passed to the `should'
clause for easy identification of the first test case that
failed (if any).  Text outside these the blocks is ignored by the
tests, so you can use it to document the test cases if you wish."
  `(with-temp-buffer
     (insert-file-contents ,file)
     (goto-char (point-min))
     (while (re-search-forward
             (concat "^# ?-+ \\_<\\(?1:.+?\\)\\_>: input ?-+\n"
                     "\\(?2:\\(?:.*\n\\)+?\\)"
                     "# ?-+ \\1: expected output ?-+\n"
                     "\\(?3:\\(?:.*\n\\)+?\\)"
                     "# ?-+ \\1: end ?-+")
             nil t)
       (let ((name (match-string 1))
             (code (match-string 2))
             (expected (match-string 3))
             got)
         (with-temp-buffer
           (insert code)
           (goto-char (point-min))
           (funcall cperl-test-mode)
           ,@body
           (setq expected (concat "test case " name ":\n" expected))
           (setq got (concat "test case " name ":\n" (buffer-string)))
           (should (equal got expected)))))))

;;; Indentation tests

(ert-deftest cperl-test-indent-exp ()
  "Run various tests for `cperl-indent-exp' edge cases.
These exercise some standard blocks and also the special
treatment for Perl expressions where a closing paren isn't the
end of the statement."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (cperl--run-test-cases
   (ert-resource-file "cperl-indent-exp.pl")
   (cperl-indent-exp))) ; here we go!

(ert-deftest cperl-test-indent-styles ()
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (cperl--run-test-cases
   (ert-resource-file "cperl-indent-styles.pl")
   (cperl-file-style "PBP")
   (indent-region (point-min) (point-max)))) ; here we go!

;;; Fontification tests

(ert-deftest cperl-test-fontify-punct-vars ()
  "Test fontification of Perl's punctuation variables.
Perl has variable names containing unbalanced quotes for the list
separator $\" and pre- and postmatch $` and $'.  A reference to
these variables, for example \\$\", should not cause the dollar
to be escaped, which would then start a string beginning with the
quote character.  This used to be broken in cperl-mode at some
point in the distant past, and is still broken in perl-mode. "
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((file (ert-resource-file "fontify-punctuation-vars.pl")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (while (search-forward "##" nil t)
        ;; The third element of syntax-ppss is true if in a string,
        ;; which would indicate bad interpretation of the quote.  The
        ;; fourth element is true if in a comment, which should be the
        ;; case.
        (should (equal (nth 3 (syntax-ppss)) nil))
        (should (equal (nth 4 (syntax-ppss)) t))))))

(ert-deftest cperl-test-fontify-declarations ()
  "Test that declarations and package usage use consistent fontification."
  (with-temp-buffer
    (funcall cperl-test-mode)
    (insert "package Foo::Bar;\n")
    (insert "use Fee::Fie::Foe::Foo\n;")
    (insert "my $xyzzy = 'PLUGH';\n")
    (goto-char (point-min))
    (font-lock-ensure)
    (search-forward "Bar")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   'font-lock-function-name-face))
    (search-forward "use") ; This was buggy in perl-mode
    (should (equal (get-text-property (match-beginning 0) 'face)
                   'font-lock-keyword-face))
    (search-forward "my")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   'font-lock-keyword-face))))

(ert-deftest cperl-test-fontify-attrs-and-signatures ()
  "Test fontification of the various combinations of subroutine
attributes, prototypes and signatures."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((file (ert-resource-file "proto-and-attrs.pl")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (font-lock-ensure)

      ;; Named subroutines
      (while (search-forward-regexp "\\_<sub_[[:digit:]]+" nil t)
        (should (equal (get-text-property (match-beginning 0) 'face)
                       'font-lock-function-name-face))
        (let ((start-of-sub (match-beginning 0))
              (end-of-sub (save-excursion (search-forward "}\n") (point))))

          ;; Prototypes are shown as strings
          (when (search-forward-regexp " ([$%@*]*) " end-of-sub t)
            (should (equal (get-text-property (1+ (match-beginning 0)) 'face)
                           'font-lock-string-face)))
          (goto-char start-of-sub)
          ;; Attributes with their optional parameters
          (when (search-forward-regexp "\\(:[a-z]+\\)\\((.*?)\\)?" end-of-sub t)
            (should (equal (get-text-property (match-beginning 1) 'face)
                           'font-lock-constant-face))
            (when (match-beginning 2)
              (should (equal (get-text-property (match-beginning 2) 'face)
                             'font-lock-string-face))))
          ;; Subroutine signatures
          (goto-char start-of-sub)
          (when (search-forward "$bar" end-of-sub t)
            (should (equal (get-text-property (match-beginning 0) 'face)
                           'font-lock-variable-name-face)))
          (goto-char end-of-sub)))
      ;; Anonymous subroutines
      (while (search-forward-regexp "= sub" nil t)
        (let ((start-of-sub (match-beginning 0))
              (end-of-sub (save-excursion (search-forward "}") (point))))

          ;; Prototypes are shown as strings
          (when (search-forward-regexp " ([$%@*]*) " end-of-sub t)
            (should (equal (get-text-property (1+ (match-beginning 0)) 'face)
                           'font-lock-string-face)))
          (goto-char start-of-sub)
          (when (search-forward-regexp "\\(:[a-z]+\\)\\((.*?)\\)?" end-of-sub t)
            (should (equal (get-text-property (match-beginning 1) 'face)
                           'font-lock-constant-face))
            (when (match-beginning 2)
              (should (equal (get-text-property (match-beginning 2) 'face)
                             'font-lock-string-face))))
          ;; Subroutine signatures
          (goto-char start-of-sub)
          (when (search-forward "$bar" end-of-sub t)
            (should (equal (get-text-property (match-beginning 0) 'face)
                           'font-lock-variable-name-face)))
          (goto-char end-of-sub))))))

(ert-deftest cperl-test-fontify-builtin-constants ()
  "Test fontification of the floating point constants \"nan\" and \"inf\"."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((constants '("my $min=-builtin::inf;"
                     "my $unknown = builtin::nan;"
                     "if ($big == inf) {"
                     "my $with_ampersand = &inf")))
    (dolist (code constants)
      (with-temp-buffer
        (insert code)
        (goto-char (point-min))
        (funcall cperl-test-mode)
        (font-lock-ensure)
        (search-forward-regexp "&?\\(builtin::\\)?\\(inf\\|nan\\)")
        (should (equal (get-text-property (match-beginning 0) 'face)
                       'font-lock-constant-face)))))
  ;; Also, test some things that are not these constants
  (let ((lookalikes '(("sub inf { ... }" . font-lock-function-name-face)
                      ("my $inf = 1E6;"  . font-lock-variable-name-face)
                      ("$object->inf;"   . cperl-method-call))))
    (dolist (doppelganger lookalikes)
      (let ((code (car doppelganger))
            (face (cdr doppelganger)))
        (with-temp-buffer
          (insert code)
          (goto-char (point-min))
          (funcall cperl-test-mode)
          (font-lock-ensure)
          (search-forward-regexp "&?\\(builtin::\\)?\\(inf\\|nan\\)")
          (should (equal (get-text-property (match-beginning 0) 'face)
                         face)))))))


(ert-deftest cperl-test-fontify-class ()
  "Test fontification of the various elements in a Perl class."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((file (ert-resource-file "perl-class.pl")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (font-lock-ensure)

      ;; The class name
      (while (search-forward-regexp "class " nil t)
        (should (equal (get-text-property (point) 'face)
                       'font-lock-function-name-face)))
      ;; The attributes (class and method)
      (while (search-forward-regexp " : " nil t)
        (should (equal (get-text-property (point) 'face)
                       'font-lock-constant-face)))
      ;; The signature
      (goto-char (point-min))
      (search-forward-regexp "\\(\\$top\\),\\(\\$down\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-variable-name-face))
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-variable-name-face))
      ;; Fields
      (goto-char (point-min))
      (search-forward-regexp "\\(field\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-keyword-face))
      (search-forward-regexp "\\(decorated\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-variable-name-face))
      (search-forward-regexp "\\(:param\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-constant-face))
      (search-forward-regexp "\\(get_decoration\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-string-face))
      ;; Initializers are no attributes
      (search-forward-regexp "\\(not_an\\)")
      (should-not (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-constant-face))
)))

(ert-deftest cperl-test-fontify-special-variables ()
  "Test fontification of variables like $^T or ${^ENCODING}.
These can occur as \"local\" aliases."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (with-temp-buffer
    (insert "local ($^I, ${^UNICODE});\n")
    (goto-char (point-min))
    (funcall cperl-test-mode)
    (font-lock-ensure)
    (search-forward "$")
    (should (equal (get-text-property (point) 'face)
                   'font-lock-variable-name-face))
    (search-forward "$")
    (should (equal (get-text-property (point) 'face)
                   'font-lock-variable-name-face))))

(ert-deftest cperl-test-fontify-sub-names ()
    "Test fontification of subroutines named like builtins.
On declaration, they should look like other used defined
functions.  When called, they should not be fontified.  In
comments and POD they should be fontified as POD."
  (let ((file (ert-resource-file "sub-names.pl")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (font-lock-ensure)
      ;; The declaration
      (search-forward-regexp "sub \\(m\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-function-name-face))
      ;; calling as a method
      (search-forward-regexp "C->new->\\(m\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     (if (equal cperl-test-mode 'perl-mode) nil
                       'cperl-method-call)))
      ;; POD
      (search-forward-regexp "\\(method\\) \\(name\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-comment-face))
      (should (equal (get-text-property (match-beginning 2) 'face)
                     'font-lock-comment-face))
      ;; comment
      (search-forward-regexp "\\(method\\) \\(name\\)")
      (should (equal (get-text-property (match-beginning 1) 'face)
                     'font-lock-comment-face))
      (should (equal (get-text-property (match-beginning 2) 'face)
                     'font-lock-comment-face)))))

(ert-deftest cperl-test-identify-heredoc ()
  "Test whether a construct containing \"<<\" followed by a
  bareword is properly identified for a here-document if
  appropriate."
  (let ((here-docs
         '("$text .= <<DELIM;"          ; mutator concatenating a here-doc
           "func($arg) . <<DELIM;"      ; concatenating a return value
           "func 1, <<DELIM;"           ; a function taking two arguments
           ))
        ;; There forms are currently mishandled in `perl-mode' :-(
        (here-docs-cperl
         '("print {a} <<DELIM;"         ; printing to a file handle
           "system $prog <<DELIM;"      ; lie about the program's name
           ))
        (_undecidable
         '("foo <<bar")                 ; could be either "foo() <<bar"
                                        ; or "foo(<<bar)"
         ))
    (dolist (code (append here-docs (if (eq cperl-test-mode #'cperl-mode)
                                        here-docs-cperl)))
      (with-temp-buffer
        (insert code "\n\nDELIM\n")
        (funcall cperl-test-mode)
        (goto-char (point-min))
        (forward-line 1)
        ;; We should now be within a here-doc.
        (let ((ppss (syntax-ppss)))
          (should (and (nth 8 ppss) (nth 4 ppss))))
        ))))

(ert-deftest cperl-test-identify-no-heredoc ()
  "Test whether a construct containing \"<<\" which is not a
  here-document is properly rejected."
  (let (
        (not-here-docs
         '("while (<<>>) {"             ; double angle bracket operator
           "expr <<func();"             ; left shift by a return value
           "$var <<func;"               ; left shift by a return value
           "($var+1) <<func;"           ; same for an expression
           "$hash{key} <<func;"         ; same for a hash element
           "or $var <<func;"            ; same for an expression
           "sorted $by <<func"          ; _not_ a call to sort
           ))
        (_undecidable
         '("foo <<bar"                  ; could be either "foo() <<bar"
                                        ; or "foo(<<bar)"
           "$foo = <<;")                ; empty delim forbidden since 5.28
         ))
    (dolist (code not-here-docs)
      (with-temp-buffer
        (insert code "\n\n")
        (funcall cperl-test-mode)
        (goto-char (point-min))
        (forward-line 1)
        ;; Point is not within a here-doc (nor string nor comment).
        (let ((ppss (syntax-ppss)))
          (should-not (nth 8 ppss)))
        ))))

(ert-deftest cperl-test-here-doc-missing-end ()
  "Verify that a missing here-document terminator gives a message.
This message prints the terminator which wasn't found and is only
issued by CPerl mode."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (ert-with-message-capture collected-messages
    (with-temp-buffer
      (insert "my $foo = <<HERE\n")
      (insert "some text here\n")
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (cperl-find-pods-heres)
      (should (string-match "End of here-document [‘'`]HERE[’']"
                            collected-messages))))
  (ert-with-message-capture collected-messages
    (with-temp-buffer
      (insert "my $foo = <<HERE . <<'THERE'\n")
      (insert "some text here\n")
      (insert "HERE\n")
      (insert "more text here\n")
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (cperl-find-pods-heres)
      (should (string-match "End of here-document [‘'`]THERE[’']"
                            collected-messages)))))

(defvar perl-continued-statement-offset)
(defvar perl-indent-level)
(defvar perl-indent-parens-as-block)

(defconst cperl--tests-heredoc-face
  (if (equal cperl-test-mode 'perl-mode) 'perl-heredoc
    'font-lock-string-face))
(defconst cperl--tests-heredoc-delim-face
  (if (equal cperl-test-mode 'perl-mode) 'perl-heredoc
    'font-lock-constant-face))

(ert-deftest cperl-test-heredocs ()
  "Test that HERE-docs are fontified with the appropriate face."
  (require 'perl-mode)
  (let ((file (ert-resource-file "here-docs.pl"))
        (cperl-continued-statement-offset perl-continued-statement-offset)
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (funcall cperl-test-mode)
      (indent-region (point-min) (point-max))
      (font-lock-ensure (point-min) (point-max))
      (while (search-forward "## test case" nil t)
        (save-excursion
          (while (search-forward "look-here" nil t)
            (should (equal
                     (get-text-property (match-beginning 0) 'face)
                     cperl--tests-heredoc-face))
            (beginning-of-line)
            (should (null (looking-at "[ \t]")))
            (forward-line 1)))
        (should (re-search-forward
                 (concat "^\\([ \t]*\\)" ; the actual indentation amount
                         "\\([^ \t\n].*?\\)\\(no\\)?indent")
                 nil t))
        (should (equal (- (match-end 1) (match-beginning 1))
                       (if (match-beginning 3) 0
                         perl-indent-level)))))))

;;; Grammar based tests: unit tests

(defun cperl-test--validate-regexp (regexp valid &optional invalid)
  "Runs tests for elements of VALID and INVALID lists against REGEXP.
Tests with elements from VALID must match, tests with elements
from INVALID must not match.  The match string must be equal to
the whole string."
  (funcall cperl-test-mode)
  (dolist (string valid)
    (should (string-match regexp string))
    (should (string= (match-string 0 string) string)))
  (when invalid
    (dolist (string invalid)
       (should-not
       (and (string-match regexp string)
	    (string= (match-string 0 string) string))))))

(ert-deftest cperl-test-ws-rx ()
  "Tests capture of very simple regular expressions (yawn)."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
	 '(" " "\t" "\n"))
	(invalid
	 '("a" "  " "")))
    (cperl-test--validate-regexp (rx (eval cperl--ws-rx))
				 valid invalid)))

(ert-deftest cperl-test-ws+-rx ()
  "Tests sequences of whitespace and comment lines."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
	 `(" " "\t#\n" "\n# \n"
	   ,(concat "# comment\n" "# comment\n" "\n" "#comment\n")))
	(invalid
	 '("=head1 NAME\n" )))
    (cperl-test--validate-regexp (rx (eval cperl--ws+-rx))
				 valid invalid)))

(ert-deftest cperl-test-version-regexp ()
  "Tests the regexp for recommended syntax of versions in Perl."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
	 '("1" "1.1" "1.1_1" "5.032001"
	   "v120.100.103"))
	(invalid
	 '("alpha" "0." ".123" "1E2"
	   "v1.1" ; a "v" version string needs at least 3 components
	   ;; bad examples from "Version numbers should be boring"
	   ;; by xdg AKA David A. Golden
	   "1.20alpha" "2.34beta2" "2.00R3")))
    (cperl-test--validate-regexp cperl--version-regexp
				 valid invalid)))

(ert-deftest cperl-test-package-regexp ()
  "Tests the regular expression of Perl package and class names with versions.
Also includes valid cases with whitespace in strange places."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
	 '("package Foo"
	   "package Foo::Bar"
	   "package Foo::Bar v1.2.3"
	   "package Foo::Bar::Baz 1.1"
	   "class O3D::Sphere"          ; since Perl 5.38
	   "package \nFoo::Bar\n 1.00"))
	(invalid
	 '("package Foo;"          ; semicolon must not be included
	   "package Foo 1.1 {"     ; nor the opening brace
	   "packageFoo"            ; not a package declaration
	   "package Foo1.1")))     ; invalid package name
    (cperl-test--validate-regexp (rx (eval cperl--package-rx))
				 valid invalid)))

(ert-deftest cperl-test-identifier-rx ()
  "Test valid and invalid identifiers (no sigils)."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
         '("foo" "FOO" "f_oo" "a123"
           "manĝis"))                   ; Unicode is allowed!
        (invalid
         '("$foo"                       ; no sigils allowed (yet)
           "Foo::bar"                   ; no package qualifiers allowed
           "lots_of_€")))               ; € is not alphabetic
    (cperl-test--validate-regexp (rx (eval cperl--basic-identifier-rx))
                                 valid invalid)))

(ert-deftest cperl-test-attribute-rx ()
  "Test attributes and attribute lists"
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
         '("foo" "bar()" "baz(quux)"))
        (invalid
         '("+foo"                       ; not an identifier
           "foo::bar"                   ; no package qualifiers allowed
           "(no-identifier)"            ; no attribute name
           "baz (quux)")))              ; no space allowed before "("
    (cperl-test--validate-regexp (rx (eval cperl--single-attribute-rx))
                                 valid invalid)))

(ert-deftest cperl-test-attribute-list-rx ()
  "Test attributes and attribute lists."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
         '(":" ":foo" ": bar()" ":baz(quux):"
           ":_" ":_foo"
           ":isa(Foo) does(Bar)" ":isa(Foo):does(Bar)"
           ":isa(Foo):does(Bar):"
           ":  isa(Foo::Bar) : does(Bar)"))
        (invalid
         '(":foo + bar"                ; not an identifier
           "::foo"                     ; not an attribute list
           ": foo(bar : : baz"         ; too many colons
           ": foo(bar)baz"             ; need a separator
           ": baz (quux)")))           ; no space allowed before "("
    (cperl-test--validate-regexp (rx (eval cperl--attribute-list-rx))
                                 valid invalid)))

(ert-deftest cperl-test-field-declaration-rx ()
  "Test field declarations with and without attributes."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
         '("field $fold"
           "field @many"
           "field %ofStrawberries"
           "field $required :param"
           "field $renamed :param(alias)"
           "field $readable : param reader(get_readable)"))
        (invalid
         '("field name"                 ; missing sigil
           "field $else::where"         ; invalid qualification
           "field &code")))             ; invalid sigil
    (cperl-test--validate-regexp (rx (eval cperl--field-declaration-rx))
                                 valid invalid)))



         (ert-deftest cperl-test-prototype-rx ()
  "Test subroutine prototypes"
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((valid
         ;; Examples from perldoc perlsub
         '("($$)" "($$$)" "($$;$)" "($$$;$)" "(@)" "($@)" "(\\@)" "(\\@$$@)"
           "(\\[%@])" "(*;$)" "(**)" "(&@)" "(;$)" "()"))
        (invalid
         '("$"                   ; missing paren
           "($self)"             ; a variable, -> subroutine signature
           "(!$)"                ; not all punctuation is permitted
           "{$$}")))             ; wrong type of paren
    (cperl-test--validate-regexp (rx (eval cperl--prototype-rx))
                                 valid invalid)))

(ert-deftest cperl-test-signature-rx ()
   "Test subroutine signatures."
   (skip-unless (eq cperl-test-mode #'cperl-mode))
   (let ((valid
          '("()" "( )" "($self, %params)" "(@params)" "($first,$)"))
        (invalid
         '("$self"               ; missing paren
           "($!)"                ; globals not permitted in a signature
           "(@par,%options)"     ; two slurpy parameters
           "{$self}")))          ; wrong type of paren
    (cperl-test--validate-regexp (rx (eval cperl--signature-rx))
                                 valid invalid)))

(ert-deftest cperl-test-autogenerated-reader-rx ()
  (let ((code-examples '("field $name :reader;"
                         "field $field :reader(name);"
                         "field $name :param :reader;"
                         "field $field :param :reader(name);"
                         "field $field :reader(name) :param;"
                         "field $field :reader(name) = 'value';"
                         "field $field :writer = 'value';"
                         "field $field :writer(write_f)")))
    (dolist (code code-examples)
      (with-temp-buffer
        (insert code)
        (goto-char (point-min))
        (search-forward-regexp (rx (eval cperl--sub-name-generated-rx)))
        (should (string= (match-string 1) "field"))
        (should (string-match ":\\(reader\\|writer\\)" (match-string 2)))))))

;;; Test unicode identifier in various places

(defun cperl--test-unicode-setup (code string)
  "Insert CODE, prepare it for tests, and find STRING.
Invoke the appropriate major mode, ensure fontification, and set
point after the first occurrence of STRING (no regexp!)."
  (insert code)
  (funcall cperl-test-mode)
  (font-lock-ensure)
  (goto-char (point-min))
  (search-forward string))

(ert-deftest cperl-test-unicode-labels ()
  "Verify that non-ASCII labels are processed correctly."
  (with-temp-buffer
    (cperl--test-unicode-setup "LABEł: for ($manĝi) { say; }" "LAB")
    (should (equal (get-text-property (point) 'face)
                   'font-lock-constant-face))))

(ert-deftest cperl-test-unicode-sub ()
  (with-temp-buffer
    (cperl--test-unicode-setup
     (concat "use strict;\n"            ; distinguish bob from b-o-f
             "sub ℏ {\n"
             "  6.62607015e-34\n"
             "};")
     "sub ")                            ; point is before "ℏ"

    ;; Testing fontification
    ;; FIXME 2021-09-10: This tests succeeds because cperl-mode
    ;; accepts almost anything as a sub name for fontification.  For
    ;; example, it fontifies "sub @ {...;}" which is a syntax error in
    ;; Perl.  I let this pass for the moment.
    (should (equal (get-text-property (point) 'face)
                   'font-lock-function-name-face))

    ;; Testing `beginning-of-defun'.  Not available in perl-mode,
    ;; where it jumps to the beginning of the buffer.
    (when (eq cperl-test-mode #'cperl-mode)
      (goto-char (point-min))
      (search-forward "-34")
      (beginning-of-defun)
      (should (looking-at "sub")))))

(ert-deftest cperl-test-unicode-varname ()
  (with-temp-buffer
    (cperl--test-unicode-setup
     (concat "use strict;\n"
             "my $π = 3.1415926535897932384626433832795028841971;\n"
             "\n"
             "my $manĝi = $π;\n"
             "__END__\n")
     "my $") ; perl-mode doesn't fontify the sigil, so include it here

    ;; Testing fontification
    ;; FIXME 2021-09-10: This test succeeds in cperl-mode because the
    ;; π character is "not ASCII alphabetic", so it treats $π as a
    ;; punctuation variable.  The following two `should' forms with a
    ;; longer variable name were added for stronger verification.
    (should (equal (get-text-property (point) 'face)
                   'font-lock-variable-name-face))
    ;; Test both ends of a longer variable name
    (search-forward "my $")             ; again skip the sigil
    (should (equal (get-text-property (point) 'face)
                   'font-lock-variable-name-face))
    (search-forward "manĝi")
    (should (equal (get-text-property (1- (match-end 0)) 'face)
                   'font-lock-variable-name-face))))

(ert-deftest cperl-test-unicode-varname-list ()
  "Verify that all elements of a variable list are fontified."

  (let ((hash-face (if (eq cperl-test-mode #'perl-mode)
                       'perl-non-scalar-variable
                     'cperl-hash-face))
        (array-face (if (eq cperl-test-mode #'perl-mode)
                        'perl-non-scalar-variable
                      'cperl-array-face)))
    (with-temp-buffer
      (cperl--test-unicode-setup
       "my (%äsh,@ärräy,$scâlâr);" "%")
      (should (equal (get-text-property (point) 'face)
                     hash-face))
      (search-forward "@")
      (should (equal (get-text-property (point) 'face)
                     array-face))
      (search-forward "scâlâr")
      (should (equal (get-text-property (match-beginning 0) 'face)
                     'font-lock-variable-name-face))
      (should (equal (get-text-property (1- (match-end 0)) 'face)
                     'font-lock-variable-name-face)))

      ;; Now with package-qualified variables
    (with-temp-buffer
      (cperl--test-unicode-setup
       "local (%Søme::äsh,@Søme::ärräy,$Søme::scâlâr);" "%")
      (should (equal (get-text-property (point) 'face)
                     hash-face))
      (search-forward "Søme::")         ; test basic identifier
      (should (equal (get-text-property (point) 'face)
                     hash-face))
      (search-forward "@")              ; test package name
      (should (equal (get-text-property (point) 'face)
                     array-face))
      (search-forward "Søme::")         ; test basic identifier
      (should (equal (get-text-property (point) 'face)
                     array-face))
      (search-forward "Søme")           ; test package name
      (should (equal (get-text-property (match-beginning 0) 'face)
                     'font-lock-variable-name-face))
      (should (equal (get-text-property (1- (match-end 0)) 'face)
                     'font-lock-variable-name-face))
      (search-forward "scâlâr")         ; test basic identifier
      (should (equal (get-text-property (match-beginning 0) 'face)
                     'font-lock-variable-name-face))
      (should (equal (get-text-property (1- (match-end 0)) 'face)
                     'font-lock-variable-name-face)))))

(ert-deftest cperl-test-unicode-arrays ()
  "Test fontification of array access."
  ;; Perl mode just looks at the sigil, for element access
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  ;; simple array element
  (with-temp-buffer
    (cperl--test-unicode-setup
     "$ärräy[1] = 7;" "$")
    (should (equal (get-text-property (point) 'face)
                   'cperl-array-face)))
  ;; array slice
  (with-temp-buffer
    (cperl--test-unicode-setup
     "@ärräy[(1..3)] = (4..6);" "@")
    (should (equal (get-text-property (point) 'face)
                     'cperl-array-face)))
  ;; array max index
  (with-temp-buffer
    (cperl--test-unicode-setup
     "$#ärräy = 1;" "$")
    (should (equal (get-text-property (point) 'face)
                   'cperl-array-face)))
  ;; array dereference
  (with-temp-buffer
    (cperl--test-unicode-setup
     "@$ärräy = (1,2,3)" "@")
    (should (equal (get-text-property (1- (point)) 'face)
                   'cperl-array-face))
    (should (equal (get-text-property (1+ (point)) 'face)
                   'font-lock-variable-name-face))))

(ert-deftest cperl-test-unicode-hashes ()
  "Test fontification of hash access."
  ;; Perl mode just looks at the sigil, for element access
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  ;; simple hash element
  (with-temp-buffer
    (cperl--test-unicode-setup
     "$häsh{'a'} = 7;" "$")
    (should (equal (get-text-property (point) 'face)
                   'cperl-hash-face)))
  ;; hash array slice
  (with-temp-buffer
    (cperl--test-unicode-setup
     "@häsh{(1..3)} = (4..6);" "@")
    (should (equal (get-text-property (point) 'face)
                     'cperl-hash-face)))
  ;; hash subset
  (with-temp-buffer
    (cperl--test-unicode-setup
     "my %hash = %häsh{'a',2,3};" "= %")
    (should (equal (get-text-property (point) 'face)
                   'cperl-hash-face)))
  ;; hash dereference
  (with-temp-buffer
    (cperl--test-unicode-setup
     "%$äsh = (key => 'value');" "%")
    (should (equal (get-text-property (1- (point)) 'face)
                   'cperl-hash-face))
    (should (equal (get-text-property (1+ (point)) 'face)
                   'font-lock-variable-name-face))))

(ert-deftest cperl-test-unicode-hashref ()
  "Verify that a hashref access disambiguates {s}.
CPerl mode takes the token \"s\" as a substitution unless
detected otherwise.  Not for perl-mode: it doesn't stringify
bareword hash keys and doesn't recognize a substitution
\"s}foo}bar}\""
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (with-temp-buffer
    (cperl--test-unicode-setup "$häshref->{s} # }}" "{")
    (should (equal (get-text-property (point) 'face)
            'font-lock-string-face))
    (should (equal (get-text-property (1+ (point)) 'face)
            nil))))

(ert-deftest cperl-test-unicode-proto ()
  ;; perl-mode doesn't fontify prototypes at all
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (with-temp-buffer
    (cperl--test-unicode-setup
     (concat "sub prötötyped ($) {\n"
             "  ...;"
             "}\n")
     "prötötyped (")

    (should (equal (get-text-property (point) 'face)
                   'font-lock-string-face))))

(ert-deftest cperl-test-unicode-fhs ()
  (with-temp-buffer
    (cperl--test-unicode-setup
     (concat "while (<BAREWÖRD>) {\n"
             "    ...;)\n"
             "}\n")
     "while (<") ; point is before the first char of the handle
    ;; Testing fontification
    ;; FIXME 2021-09-10: perl-mode.el and cperl-mode.el handle these
    ;; completely differently.  perl-mode interprets barewords as
    ;; constants, cperl-mode does not fontify them.  Both treat
    ;; non-barewords as globs, which are not fontified by perl-mode,
    ;; but fontified as strings in cperl-mode.  We keep (and test)
    ;; that behavior "as is" because both bareword filehandles and
    ;; <glob> syntax are no longer recommended.
    (let ((bareword-face
           (if (equal cperl-test-mode 'perl-mode) 'font-lock-constant-face
             nil)))
            (should (equal (get-text-property (point) 'face)
                     bareword-face)))))

(ert-deftest cperl-test-unicode-hashkeys ()
  "Test stringification of bareword hash keys.  Not in perl-mode.
perl-mode generally does not stringify bareword hash keys."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  ;; Plain hash key
  (with-temp-buffer
    (cperl--test-unicode-setup
     "$häsh { kéy }" "{ ")
    (should (equal (get-text-property (point) 'face)
                   'font-lock-string-face)))
  ;; Nested hash key
  (with-temp-buffer
    (cperl--test-unicode-setup
     "$häsh { kéy } { kèy }" "} { ")
    (should (equal (get-text-property (point) 'face)
                   'font-lock-string-face)))
  ;; Key => value
  (with-temp-buffer
    (cperl--test-unicode-setup
     "( kéy => 'value'," "( ")
    (should (equal (get-text-property (point) 'face)
                   'font-lock-string-face))))

(ert-deftest cperl-test-word-at-point ()
  "Test whether the function captures non-ASCII words."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((words '("rôle" "café" "ångström"
                 "Data::Dump::dump"
                 "_underscore")))
    (dolist (word words)
      (with-temp-buffer
        (insert " + ")                  ; this will be the suffix
        (beginning-of-line)
        (insert ")")                    ; A non-word char
        (insert word)
        (should (string= word (cperl-word-at-point-hard)))))))

(ert-deftest cperl-test-extra-delimiters ()
  "Test whether cperl-mode can process unicode delimiters.
The minor mode `cperl-extra-paired-delimiters-mode' controls whether we
have extra paired delimiters."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "extra-delimiters.pl"))
    (funcall cperl-test-mode)
    (cperl-extra-paired-delimiters-mode t)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward-regexp "\\(label:\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-constant-face))
    (search-forward-regexp "\\(comment\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-comment-face))
    (search-forward-regexp "\\(sanity\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-variable-name-face))
    ;; Now switch off the minor mode and redo
    (cperl-extra-paired-delimiters-mode -1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward-regexp "\\(label:\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-string-face))
    (search-forward-regexp "\\(comment\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-string-face))
    (search-forward-regexp "\\(sanity\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-variable-name-face))))


;;; Function test: Building an index for imenu

(ert-deftest cperl-test-imenu-index ()
  "Test index creation for imenu.
This test relies on the specific layout of the index alist as
created by CPerl mode, so skip it for Perl mode."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "grammar.pl"))
    (cperl-mode)
    (let ((index (cperl-imenu--create-perl-index))
          current-list)
      (setq current-list (assoc-string "+Unsorted List+..." index))
      (should current-list)
      (let ((expected '("(main)::outside"
                        "Package::in_package"
                        "Shoved::elsewhere"
                        "Package::prototyped"
                        "Versioned::Package::versioned"
                        "Block::attr"
                        "Versioned::Package::outer"
                        "lexical"
                        "Versioned::Block::signatured"
                        "Package::in_package_again"
                        "Erdős::Number::erdős_number"
                        "Class::Class::init"
                        "Class::Inner::init_again"
                        "With::Accessors->auto_reader"
                        "With::Accessors->named"
                        "With::Accessors->set_auto_writer"
                        "With::Accessors->read_all"
                        "With::Accessors->set_auto_all")))
        (dolist (sub expected)
          (should (assoc-string sub index))))
      (should-not (assoc-string "_false" index)))))

;;; Tests for issues reported in the Bug Tracker

(ert-deftest cperl-test-bug-997 ()
  "Test that we distinguish a regexp match when there's nothing before it."
  (let ((code "# some comment\n\n/fontify me/;\n"))
    (with-temp-buffer
      (funcall cperl-test-mode)
      (insert code)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "/f")
      (should (equal (get-text-property (point) 'face)
                     'font-lock-string-face)))))

(defun cperl-test--run-bug-10483 ()
  "Runs a short program, intended to be under timer scrutiny.
This function is intended to be used by an Emacs subprocess in
batch mode.  The message buffer is used to report the result of
running `cperl-indent-exp' for a very simple input.  The result
is expected to be different from the input, to verify that
indentation actually takes place.."
  (let ((code "poop ('foo', \n'bar')")) ; see the bug report
    (message "Test Bug#10483 started")
    (with-temp-buffer
      (insert code)
      (funcall cperl-test-mode)
      (goto-char (point-min))
      (search-forward "poop")
      (cperl-indent-exp)
      (message "%s" (buffer-string)))))

(ert-deftest cperl-test-bug-10483 ()
  "Check that indenting certain perl code does not loop forever.
This verifies that indenting a piece of code that ends in a paren
without a statement terminator on the same line does not loop
forever.  The test starts an asynchronous Emacs batch process
under timeout control."
  :tags '(:expensive-test)
  (skip-unless (not (getenv "EMACS_HYDRA_CI"))) ; FIXME times out
  (skip-unless (not (< emacs-major-version 28))) ; times out in older Emacsen
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let* ((emacs (concat invocation-directory invocation-name))
         (test-function 'cperl-test--run-bug-10483)
         (test-function-name (symbol-name test-function))
         (test-file (symbol-file test-function 'defun))
         (ran-out-of-time nil)
         (process-connection-type nil)
         runner)
    (with-temp-buffer
      (with-timeout (2
                     (delete-process runner)
                     (setq ran-out-of-time t))
        (setq runner (start-process "speedy"
                                    (current-buffer)
                                    emacs
                                    "-batch"
                                    "--quick"
                                    "--load" test-file
                                    "--funcall" test-function-name))
        (while (accept-process-output runner)))
      (should (equal ran-out-of-time nil))
      (goto-char (point-min))
      ;; just a very simple test for indentation: This should
      ;; be rather robust with regard to indentation defaults
      (should (string-match
               "poop ('foo', \n      'bar')" (buffer-string))))))

(ert-deftest cperl-test-bug-11733 ()
  "Verify indentation of braces after newline and non-labels."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-11733.pl")
   (goto-char (point-min))
   (while (null (eobp))
     (cperl-indent-command)
     (forward-line 1))))


(ert-deftest cperl-test-bug-11996 ()
  "Verify that we give the right syntax property to a backslash operator."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-11996.pl"))
    (funcall cperl-test-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (re-search-forward "\\(\\\\(\\)")
    (save-excursion
      (goto-char (match-beginning 1))
      (should (equal (syntax-after (point)) (string-to-syntax ".")))
      ;; `forward-sexp' shouldn't complain.
      (forward-sexp)
      (should (char-equal (char-after) ?\;)))
    (re-search-forward "\\(\\\\\"\\)")
    (save-excursion
      (goto-char (match-beginning 1))
      (should (equal (syntax-after (point)) (string-to-syntax "\\")))
      (should (equal (get-text-property (point) 'face) 'font-lock-string-face)))
    (re-search-forward "\\(\\\\\"\\)")
    (save-excursion
      (goto-char (match-beginning 1))
      (should (equal (syntax-after (point)) (string-to-syntax "\\"))))
    (re-search-forward "\\(\\\\\"\\)")
    (save-excursion
      (goto-char (match-beginning 1))
      (should (equal (syntax-after (point)) (string-to-syntax ".")))
      (should (equal (get-text-property (1+ (point)) 'face)
                     'font-lock-string-face)))))

(ert-deftest cperl-test-bug-14343 ()
  "Verify that inserting text into a HERE-doc string with Elisp
does not break fontification."
  (with-temp-buffer
    (insert "my $string = <<HERE;\n"
            "One line of text.\n"
            "Last line of this string.\n"
            "HERE\n")
    (funcall cperl-test-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "One line")
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-face))
    (beginning-of-line)
    (insert "Another line if text.\n")
    (font-lock-ensure)
    (forward-line -1)
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-face))
    (search-forward "HERE")
    (beginning-of-line)
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-delim-face)))
  ;; insert into an empty here-document
  (with-temp-buffer
    (insert "print <<HERE;\n"
            "HERE\n")
    (funcall cperl-test-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (forward-line)
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-delim-face))
    ;; Insert a newline into the empty here-document
    (goto-char (point-min))
    (forward-line)
    (insert "\n")
    (search-forward "HERE")
    (beginning-of-line)
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-delim-face))
    ;; Insert text at the beginning of the here-doc
    (goto-char (point-min))
    (forward-line)
    (insert "text")
    (font-lock-ensure)
    (search-backward "text")
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-face))
    (search-forward "HERE")
    (beginning-of-line)
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-delim-face))
    ;; Insert a new line immediately before the delimiter
    ;; (That's where the point is anyway)
    (insert "A new line\n")
    (font-lock-ensure)
    ;; The delimiter is still the delimiter
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-delim-face))
    (forward-line -1)
    ;; The new line has been "added" to the here-document
    (should (equal (get-text-property (point) 'face)
                   cperl--tests-heredoc-face))))

(ert-deftest cperl-test-bug-16368 ()
  "Verify that `cperl-forward-group-in-re' doesn't hide errors."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((code "/(\\d{4})(?{2}/;")     ; the regex from the bug report
        (result))
    (with-temp-buffer
      (insert code)
      (goto-char 9)
      (setq result (cperl-forward-group-in-re))
      (should (equal (car result) 'scan-error))
      (should (equal (nth 1 result) "Unbalanced parentheses"))
      (should (= (point) 9))))        ; point remains unchanged on error
  (let ((code "/(\\d{4})(?{2})/;")    ; here all parens are balanced
        (result))
    (with-temp-buffer
      (insert code)
      (goto-char 9)
      (setq result (cperl-forward-group-in-re))
      (should (equal result nil))
      (should (= (point) 15)))))      ; point has skipped the group

(ert-deftest cperl-test-bug-19709 ()
  "Verify that indentation of closing paren works as intended.
Note that Perl mode has no setting for close paren offset, per
documentation it does the right thing anyway."
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-19709.pl")
   ;; settings from the bug report
   (setq-local cperl-indent-level 4)
   (setq-local cperl-indent-parens-as-block t)
   (setq-local  cperl-close-paren-offset -4)
   ;; same, adapted for per-mode
   (setq-local perl-indent-level 4)
   (setq-local perl-indent-parens-as-block t)
   (while (null (eobp))
     (cperl-indent-command)
     (forward-line 1))))

(ert-deftest cperl-test-bug-22355 ()
  "Verify that substitutions are fontified directly after \"|&\".
Regular expressions are strings in both perl-mode and cperl-mode."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-22355.pl"))
    (funcall cperl-test-mode)
    (goto-char (point-min))
    ;; Just check for the start of the string
    (search-forward "{")
    (should (nth 3 (syntax-ppss)))))

(ert-deftest cperl-test-bug-23992 ()
  "Verify that substitutions are fontified directly after \"|&\".
Regular expressions are strings in both perl-mode and cperl-mode."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-23992.pl"))
    (funcall cperl-test-mode)
    (goto-char (point-min))
    ;; "or" operator, with spaces
    (search-forward "RIGHT")
    (should (nth 3 (syntax-ppss)))
    ;; "or" operator, without spaces
    (search-forward "RIGHT")
    (should (nth 3 (syntax-ppss)))
    ;; "and" operator, with spaces
    (search-forward "RIGHT")
    (should (nth 3 (syntax-ppss)))
    ;; "and" operator, without spaces
    (search-forward "RIGHT")
    (should (nth 3 (syntax-ppss)))))

(ert-deftest cperl-test-bug-25098 ()
  "Verify that a quotelike operator is recognized after a fat comma \"=>\".
Related, check that calling a method named q is not mistaken as a
quotelike operator."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-25098.pl"))
    (funcall cperl-test-mode)
    (goto-char (point-min))
    ;; good example from the bug report, with a space
    (search-forward "q{")
    (should (nth 3 (syntax-ppss)))
    ;; bad (but now fixed) example from the bug report, without space
    (search-forward "q{")
    (should (nth 3 (syntax-ppss)))
    ;; calling a method "q" (parens instead of braces to make it valid)
    (search-forward "q(")
    (should-not (nth 3 (syntax-ppss)))))

(ert-deftest cperl-test-bug-28650 ()
  "Verify that regular expressions are recognized after 'return'.
The test uses the syntax property \"inside a string\" for the
text in regular expressions, which is non-nil for both cperl-mode
and perl-mode."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-26850.pl"))
    (goto-char (point-min))
    (re-search-forward "sub interesting {[^}]*}")
    (should-not (equal (nth 3 (cperl-test-ppss (match-string 0) "Today"))
                       nil))
    (re-search-forward "sub boring {[^}]*}")
    (should-not (equal (nth 3 (cperl-test-ppss (match-string 0) "likes\\?"))
                       nil))))

(ert-deftest cperl-test-bug-30393 ()
  "Verify that indentation is not disturbed by an open paren in col 0.
Perl is not Lisp: An open paren in column 0 does not start a function."
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-30393.pl")
   (while (null (eobp))
     (cperl-indent-command)
     (forward-line 1))))

(ert-deftest cperl-test-bug-35925 ()
  "Check that indentation is correct after a terminating format declaration."
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-35925.pl")
   (cperl-file-style "PBP") ; Make cperl-mode use the same settings as perl-mode.
   (let ((tab-function
          (if (equal cperl-test-mode 'perl-mode)
              #'indent-for-tab-command
            #'cperl-indent-command)))
     (goto-char (point-max))
     (forward-line -2)
     (funcall tab-function))))

(ert-deftest cperl-test-bug-37127 ()
  "Verify that closing a paren in a regex goes without a message.
Also check that the message is issued if the regex terminator is
missing."
  ;; The actual fix for this bug is in simple.el, which is not
  ;; backported to older versions of Emacs.  Therefore we skip this
  ;; test if we're running Emacs 27 or older.
  (skip-unless (< 27 emacs-major-version))
  ;; Part one: Regex is ok, no messages
  (ert-with-message-capture collected-messages
    (with-temp-buffer
      (insert "$_ =~ /(./;")
      (funcall cperl-test-mode)
      (goto-char (point-min))
      (search-forward ".")
      (let ((last-command-event ?\))
            ;; Don't emit "Matches ..." even if not visible (e.g. in batch).
            (blink-matching-paren 'jump-offscreen))
        (self-insert-command 1)
        ;; `self-insert-command' doesn't call `blink-matching-open' in
        ;; batch mode, so we need to call it explicitly.
        (blink-matching-open))
      (syntax-propertize (point-max)))
    (should (string-equal collected-messages "")))
  ;; part two: Regex terminator missing -> message
  (when (eq cperl-test-mode #'cperl-mode)
    ;; This test is only run in `cperl-mode' because only cperl-mode
    ;; emits a message to warn about such unclosed REs.
    (ert-with-message-capture collected-messages
      (with-temp-buffer
        (insert "$_ =~ /(..;")
        (goto-char (point-min))
        (funcall cperl-test-mode)
        (search-forward ".")
        (let ((last-command-event ?\)))
          (self-insert-command 1))
        (syntax-propertize (point-max)))
      (should (string-match "^End of .* string/RE"
                            collected-messages)))))

(ert-deftest cperl-test-bug-42168 ()
  "Verify that '/' is a division after ++ or --, not a regexp.
Reported in https://github.com/jrockway/cperl-mode/issues/45.
If seen as regular expression, then the slash is displayed using
font-lock-constant-face.  If seen as a division, then it doesn't
have a face property."
  :tags '(:fontification)
  ;; The next two Perl expressions have divisions.  The slash does not
  ;; start a string.
  (let ((code "{ $a++ / $b }"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) nil)))
  (let ((code "{ $a-- / $b }"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) nil)))
  ;; The next two Perl expressions have regular expressions. The slash
  ;; starts a string.
  (let ((code "{ $a+ / $b } # /"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) 7)))
  (let ((code "{ $a- / $b } # /"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) 7))))

(ert-deftest cperl-test-bug-45255 ()
  "Verify that \"<<>>\" is recognized as not starting a HERE-doc."
  (let ((code (concat "while (<<>>) {\n"
                      "   ...;\n"
                      "}\n")))
    ;; The yadda-yadda operator should not be in a string.
    (should (equal (nth 8 (cperl-test-ppss code "\\.")) nil))))

(ert-deftest cperl-test-bug-47112 ()
  "Check that in a bareword starting with a quote-like operator
followed by an underscore is not interpreted as that quote-like
operator.  Also check that a quote-like operator followed by a
colon (which is, like ?_, a symbol in CPerl mode) _is_ identified
as that quote like operator."
  (with-temp-buffer
    (funcall cperl-test-mode)
    (insert "sub y_max { q:bar:; y _bar_foo_; }")
    (goto-char (point-min))
    (syntax-propertize (point-max))
    (font-lock-ensure)
    (search-forward "max")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   'font-lock-function-name-face))
    (search-forward "bar")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   'font-lock-string-face))
    ;; perl-mode doesn't highlight
    (when (eq cperl-test-mode #'cperl-mode)
      (search-forward "_")
      (should (equal (get-text-property (match-beginning 0) 'face)
                     (if (eq cperl-test-mode #'cperl-mode)
                         'font-lock-constant-face
                       'font-lock-string-face))))))

(ert-deftest cperl-test-hyperactive-electric-else ()
  "Demonstrate cperl-electric-else behavior.
If `cperl-electric-keywords' is true, keywords like \"else\" and
\"continue\" are expanded by a following empty block, with the
cursor in the appropriate position to write that block.  This,
however, must not happen when the keyword occurs in a variable
\"$else\" or \"$continue\"."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  ;; `self-insert-command' takes a second argument only since Emacs 27
  (skip-unless (not (< emacs-major-version 27)))
  (with-temp-buffer
    (setq cperl-electric-keywords t)
    (cperl-mode)
    (insert "continue")
    (self-insert-command 1 ?\ )
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    ;; cperl-mode creates a block here
    (should (search-forward-regexp "continue {\n[[:blank:]]+\n}")))
  (with-temp-buffer
    (setq cperl-electric-keywords t)
    (cperl-mode)
    (insert "$continue")
    (self-insert-command 1 ?\ )
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    ;; No block should have been created here
    (should-not (search-forward-regexp "{" nil t))))

(ert-deftest cperl-test-bug-47598 ()
  "Check that a file test followed by ? is no longer interpreted
as a regex."
  ;; Testing the text from the bug report
  (with-temp-buffer
    (insert "my $f = -f ? 'file'\n")
    (insert "      : -l ? [readlink]\n")
    (insert "      : -d ? 'dir'\n")
    (insert "      : 'unknown';\n")
    (funcall cperl-test-mode)
    ;; Perl mode doesn't highlight file tests as functions, so we
    ;; can't test for the function's face.  But we can verify that the
    ;; function is not a string.
    (goto-char (point-min))
    (search-forward "?")
    (should-not (nth 3 (syntax-ppss (point)))))
  ;; Testing the actual targets for the regexp: m?foo? (still valid)
  ;; and ?foo? (invalid since Perl 5.22)
  (with-temp-buffer
    (insert "m?foo?;")
    (funcall cperl-test-mode)
    (should (nth 3 (syntax-ppss 3))))
  (with-temp-buffer
    (insert " ?foo?;")
    (funcall cperl-test-mode)
    (should-not (nth 3 (syntax-ppss 3)))))

(ert-deftest cperl-test-bug-64190 ()
  "Verify correct fontification of multiline declarations"
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (let ((file (ert-resource-file "cperl-bug-64190.pl")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (cperl-mode)
      (font-lock-ensure)
      ;; Example 1
      (while (search-forward "var" nil t)
        (should (equal (get-text-property (point) 'face)
                       'font-lock-variable-name-face)))
      ;; Example 2
      (search-forward "package F")
      (should (equal (get-text-property (point) 'face)
                     'font-lock-function-name-face))

      ;; Example 3 and 4 can't be directly tested because jit-lock and
      ;; batch tests don't play together well.  But we can approximate
      ;; the behavior by calling the fontification for the same
      ;; region which would be used by jit-lock.
      ;; Example 3
      (search-forward "sub do_stuff")
      (let ((start-change (point)))
        (insert "\n{")
        (cperl-font-lock-fontify-region-function start-change
                                                 (point-max)
                                                 nil) ; silent
        (font-lock-ensure start-change (point-max))
        (goto-char (1- start-change)) ; between the "ff" in "stuff"
        (should (equal (get-text-property (point) 'face)
                       'font-lock-function-name-face))
        (search-forward "{")
        (insert "}")) ; make it legal again

      ;; Example 4
      (search-forward "$param2")
      (beginning-of-line)
      (let ((start-change (point)))
        (insert " ")
        (cperl-font-lock-fontify-region-function start-change
                                                 (point-max)
                                                 nil) ; silent
        (font-lock-ensure start-change (point-max))
        (goto-char (1+ start-change))
        (should (equal (get-text-property (point) 'face)
                       'font-lock-variable-name-face))
        (re-search-forward (rx (group "sub") " " (group "oops")))
        (should (equal (get-text-property (match-beginning 1) 'face)
                       'font-lock-keyword-face))
        (should (equal (get-text-property (match-beginning 2) 'face)
                       'font-lock-function-name-face))))))

(ert-deftest cperl-test-bug-64364 ()
  "Check that multi-line subroutine declarations indent correctly."
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-64364.pl")
   (cperl-file-style "PBP") ; make cperl-mode use the same settings as perl-mode
   (indent-region (point-min) (point-max)))
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-64364.pl")
   (cperl-file-style "PBP") ; make cperl-mode use the same settings as perl-mode
   (let ((tab-function
          (if (equal cperl-test-mode 'perl-mode)
              #'indent-for-tab-command
            #'cperl-indent-command)))
     (goto-char (point-min))
     (while (null (eobp))
       (funcall tab-function)
       (forward-line 1)))))

(ert-deftest cperl-test-bug-65834 ()
  "Verify that CPerl mode identifies a left-shift operator.
Left-shift and here-documents both use the \"<<\" operator.
In the code provided by this bug report, it needs to be
detected as left-shift operator."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-65834.pl"))
    (funcall cperl-test-mode)
    (font-lock-ensure)
    (search-forward "retur")             ; leaves point before the "n"
    (should (equal (get-text-property (point) 'face)
                   'font-lock-keyword-face))
    (search-forward "# comm")           ; leaves point before "ent"
    (should (equal (get-text-property (point) 'face)
                   'font-lock-comment-face))))

(ert-deftest cperl-test-bug-66145 ()
  "Verify that hashes and arrays are only fontified in code.
In strings, comments and POD the syntaxified faces should
prevail.  The tests exercise all combinations of sigils $@% and
parenthesess [{ for comments, POD, strings and HERE-documents.
Fontification in code for `cperl-mode' is done in the tests
beginning with `cperl-test-unicode`."
  (let ((types '("array" "hash" "key"))
        (faces `(("string"  . font-lock-string-face)
                 ("comment" . font-lock-comment-face)
                 ("here"    . ,(if (equal cperl-test-mode 'perl-mode)
                                   'perl-heredoc
                                 'font-lock-string-face)))))
    (with-temp-buffer
      (insert-file-contents (ert-resource-file "cperl-bug-66145.pl"))
      (funcall cperl-test-mode)
      (font-lock-ensure)
      (dolist (type types)
        (goto-char (point-min))
        (while (re-search-forward (concat type "_\\([a-z]+\\)") nil t)
          (should (equal (get-text-property (match-beginning 1) 'face)
                         (cdr (assoc (match-string-no-properties 1)
                                     faces)))))))))

(ert-deftest cperl-test-bug-66161 ()
  "Verify that text after \"__END__\" is fontified as comment.
For `cperl-mode', this needs the custom variable
`cperl-fontify-trailer' to be set to `comment'.  Per default,
cperl-mode fontifies text after the delimiter as Perl code."
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "cperl-bug-66161.pl"))
    (setq cperl-fontify-trailer 'comment)
    (funcall cperl-test-mode)
    (font-lock-ensure)
    (search-forward "TODO")             ; leaves point before the colon
    (should (equal (get-text-property (point) 'face)
                   'font-lock-comment-face))))

(ert-deftest cperl-test-bug-69604 ()
  "Verify that $\" in a double-quoted string does not end the string.
Both `perl-mode' and `cperl-mode' treat ?$ as a quoting/escaping char to
avoid issues with punctuation variables.  In a string, however, this is
not appropriate."
  (let ((strings
         '("\"$\\\"      in string ---\"; # \"" ; $ must not quote \
           "$\"     . \" in string ---\"; # \"" ; $ must quote \
           "\"\\$\" . \" in string ---\"; # \""))) ; \$ must not quote
    (dolist (string strings)
      (with-temp-buffer
        (insert string)
        (funcall cperl-test-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "in string")
        (should (equal (get-text-property (point) 'face)
                       'font-lock-string-face))))))

(ert-deftest cperl-test-bug-72296 ()
  "Verify that the perl modes correctly handle the flip-flop operator.
Two successive dots are an operator.  A slash immediately following them
starts a regular expression, if there's another term between the dots
and the slash, then we have a division."
  :tags '(:fontification)
  ;; Code from the bug report.  The slash is a division.  The following
  ;; number is not a string.
  (let ((code "for (2..$n/2) { ...; }"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) nil)))
  ;; This is what the test for two successive dots wants to catch: The
  ;; flip-flop operator.  Here, the number is part of a regexp, seen as
  ;; a string.
  (let ((code "for (2../2/) { ...; }"))
    (should (equal (nth 8 (cperl-test-ppss code "/")) 9)))
  )

(ert-deftest cperl-test-bug-74245 ()
  "Verify that a bare \"$\" can appear at the end of a subroutine signature.
It must not be mistaken for \"$)\"."
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (cperl--run-test-cases
   (ert-resource-file "cperl-bug-74245.pl")
   (while (null (eobp))
     (cperl-indent-command)
     (forward-line 1))))

(ert-deftest test-indentation ()
  ;; The erts file explicitly invokes cperl-mode, so skip in perl-mode.
  ;; Indentation defaults are different, so it won't pass in perl-mode
  (skip-unless (eq cperl-test-mode #'cperl-mode))
  (ert-test-erts-file (ert-resource-file "cperl-indents.erts")))

;;; cperl-mode-tests.el ends here
