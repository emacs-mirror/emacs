;;; regex-tests.el --- tests for regex.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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

(defvar regex-tests--resources-dir
  (concat (concat (file-name-directory (or load-file-name buffer-file-name))
                  "/regex-resources/"))
  "Path to regex-resources directory next to the \"regex-tests.el\" file.")

(ert-deftest regex-word-cc-fallback-test ()
  "Test that \"[[:cc:]]*x\" matches \"x\" (bug#24020).

Test that a regex of the form \"[[:cc:]]*x\" where CC is
a character class which matches a multibyte character X, matches
string \"x\".

For example, \"[[:word:]]*\u2620\" regex (note: \u2620 is a word
character) must match a string \"\u2420\"."
  (dolist (class '("[[:word:]]" "\\sw"))
    (dolist (repeat '("*" "+"))
      (dolist (suffix '("" "b" "bar" "\u2620"))
        (dolist (string '("" "foo"))
          (when (not (and (string-equal repeat "+")
                          (string-equal string "")))
            (should (string-match (concat "^" class repeat suffix "$")
                                  (concat string suffix)))))))))

(defun regex--test-cc (name matching not-matching)
  (let (case-fold-search)
    (should (string-match-p (concat "^[[:" name ":]]*$") matching))
    (should (string-match-p (concat "^[[:" name ":]]*?\u2622$")
                            (concat matching "\u2622")))
    (should (string-match-p (concat "^[^[:" name ":]]*$") not-matching))
    (should (string-match-p (concat "^[^[:" name ":]]*\u2622$")
                            (concat not-matching "\u2622")))
    (with-temp-buffer
      (insert matching)
      (let ((p (point)))
        (insert not-matching)
        (goto-char (point-min))
        (skip-chars-forward (concat "[:" name ":]"))
        (should (equal (point) p))
        (skip-chars-forward (concat "^[:" name ":]"))
        (should (equal (point) (point-max)))
        (goto-char (point-min))
        (skip-chars-forward (concat "[:" name ":]\u2622"))
        (should (or (equal (point) p) (equal (point) (1+ p))))))))

(dolist (test '(("alnum" "abcABC012łąka" "-, \t\n")
                ("alpha" "abcABCłąka" "-,012 \t\n")
                ("digit" "012" "abcABCłąka-, \t\n")
                ("xdigit" "0123aBc" "łąk-, \t\n")
                ("upper" "ABCŁĄKA" "abc012-, \t\n")
                ("lower" "abcłąka" "ABC012-, \t\n")

                ("word" "abcABC012\u2620" "-, \t\n")

                ("punct" ".,-" "abcABC012\u2620 \t\n")
                ("cntrl" "\1\2\t\n" ".,-abcABC012\u2620 ")
                ("graph" "abcłąka\u2620-," " \t\n\1")
                ("print" "abcłąka\u2620-, " "\t\n\1")

                ("space" " \t\n\u2001" "abcABCł0123")
                ("blank" " \t\u2001" "\n")

                ("ascii" "abcABC012 \t\n\1" "łą\u2620")
                ("nonascii" "łą\u2622" "abcABC012 \t\n\1")
                ("unibyte" "abcABC012 \t\n\1" "łą\u2622")
                ("multibyte" "łą\u2622" "abcABC012 \t\n\1")))
  (let ((name (intern (concat "regex-tests-" (car test) "-character-class")))
        (doc (concat "Perform sanity test of regexes using " (car test)
                     " character class.

Go over all the supported character classes and test whether the
classes and their inversions match what they are supposed to
match.  The test is done using `string-match-p' as well as
`skip-chars-forward'.")))
    (eval `(ert-deftest ,name () ,doc ,(cons 'regex--test-cc test)) t)))


(defmacro regex-tests-generic-line (comment-char test-file whitelist &rest body)
  "Reads a line of the test file TEST-FILE, skipping
comments (defined by COMMENT-CHAR), and evaluates the tests in
this line as defined in the BODY.  Line numbers in the WHITELIST
are known failures, and are skipped."

  `(with-temp-buffer
    (modify-syntax-entry ?_ "w;; ") ; tests expect _ to be a word
    (insert-file-contents (concat regex-tests--resources-dir ,test-file))
    (let ((case-fold-search nil)
          (line-number 1)
          (whitelist-idx 0))

      (goto-char (point-min))

      (while (not (eobp))
        (let ((start (point)))
          (end-of-line)
          (narrow-to-region start (point))

          (goto-char (point-min))

          (when
              (and
               ;; ignore comments
               (save-excursion
                 (re-search-forward ,(concat "^[^" (string comment-char) "]") nil t))

               ;; skip lines in the whitelist
               (let ((whitelist-next
                      (condition-case nil
                          (aref ,whitelist whitelist-idx) (args-out-of-range nil))))
                 (cond
                  ;; whitelist exhausted. do process this line
                  ((null whitelist-next) t)

                  ;; we're not yet at the next whitelist element. do
                  ;; process this line
                  ((< line-number whitelist-next) t)

                  ;; we're past the next whitelist element. This
                  ;; shouldn't happen
                  ((> line-number whitelist-next)
                   (error
                    (format
                     "We somehow skipped the next whitelist element: line %d" whitelist-next)))

                  ;; we're at the next whitelist element. Skip this
                  ;; line, and advance the whitelist index
                  (t
                   (setq whitelist-idx (1+ whitelist-idx)) nil))))
            ,@body)

          (widen)
          (forward-line)
          (beginning-of-line)
          (setq line-number (1+ line-number)))))))

(defun regex-tests-compare (string what-failed bounds-ref &optional substring-ref)
  "I just ran a search, looking at STRING.  WHAT-FAILED describes
what failed, if anything; valid values are 'search-failed,
'compilation-failed and nil.  I compare the beginning/end of each
group with their expected values.  This is done with either
BOUNDS-REF or SUBSTRING-REF; one of those should be non-nil.
BOUNDS-REF is a sequence \[start-ref0 end-ref0 start-ref1
end-ref1 ....] while SUBSTRING-REF is the expected substring
obtained by indexing the input string by start/end-ref.

If the search was supposed to fail then start-ref0/substring-ref0
is 'search-failed.  If the search wasn't even supposed to compile
successfully, then start-ref0/substring-ref0 is
'compilation-failed.  If I only care about a match succeeding,
this can be set to t.

This function returns a string that describes the failure, or nil
on success"

  (when (or
         (and bounds-ref substring-ref)
         (not (or bounds-ref substring-ref)))
    (error "Exactly one of bounds-ref and bounds-ref should be non-nil"))

  (let ((what-failed-ref (car (or bounds-ref substring-ref))))

    (cond
     ((eq what-failed 'search-failed)
      (cond
       ((eq what-failed-ref 'search-failed)
        nil)
       ((eq what-failed-ref 'compilation-failed)
        "Expected pattern failure; but no match")
       (t
        "Expected match; but no match")))

     ((eq what-failed 'compilation-failed)
      (cond
       ((eq what-failed-ref 'search-failed)
        "Expected no match; but pattern failure")
       ((eq what-failed-ref 'compilation-failed)
        nil)
       (t
        "Expected match; but pattern failure")))

     ;; The regex match succeeded
     ((eq what-failed-ref 'search-failed)
      "Expected no match; but match")
     ((eq what-failed-ref 'compilation-failed)
      "Expected pattern failure; but match")

     ;; The regex match succeeded, as expected. I now check all the
     ;; bounds
     (t
      (let ((idx 0)
            msg
            ref next-ref-function compare-ref-function mismatched-ref-function)

        (if bounds-ref
            (setq ref bounds-ref
                  next-ref-function (lambda (x) (cddr x))
                  compare-ref-function (lambda (ref start-pos end-pos)
                                         (or (eq (car ref) t)
                                             (and (eq start-pos (car ref))
                                                  (eq end-pos   (cadr ref)))))
                  mismatched-ref-function (lambda (ref start-pos end-pos)
                                            (format
                                             "beginning/end positions: %d/%s and %d/%s"
                                             start-pos (car ref) end-pos (cadr ref))))
          (setq ref substring-ref
                next-ref-function (lambda (x) (cdr x))
                compare-ref-function (lambda (ref start-pos end-pos)
                                       (or (eq (car ref) t)
                                           (string= (substring string start-pos end-pos) (car ref))))
                mismatched-ref-function (lambda (ref start-pos end-pos)
                                          (format
                                           "beginning/end positions: %d/%s and %d/%s"
                                           start-pos (car ref) end-pos (cadr ref)))))

        (while (not (or (null ref) msg))

          (let ((start (match-beginning idx))
                (end   (match-end       idx)))

            (when (not (funcall compare-ref-function ref start end))
              (setq msg
                    (format
                     "Have expected match, but mismatch in group %d: %s" idx (funcall mismatched-ref-function ref start end))))

            (setq ref (funcall next-ref-function ref)
                  idx (1+ idx))))

        (or msg
            nil))))))



(defun regex-tests-match (pattern string bounds-ref &optional substring-ref)
  "I match the given STRING against PATTERN.  I compare the
beginning/end of each group with their expected values.
BOUNDS-REF is a sequence [start-ref0 end-ref0 start-ref1 end-ref1
....].

If the search was supposed to fail then start-ref0 is
'search-failed.  If the search wasn't even supposed to compile
successfully, then start-ref0 is 'compilation-failed.

This function returns a string that describes the failure, or nil
on success"

  (if (string-match "\\[\\([\\.=]\\)..?\\1\\]" pattern)
      ;; Skipping test: [.x.] and [=x=] forms not supported by emacs
      nil

    (regex-tests-compare
     string
     (condition-case nil
         (if (string-match pattern string) nil 'search-failed)
       ('invalid-regexp 'compilation-failed))
     bounds-ref substring-ref)))


(defconst regex-tests-re-even-escapes
  "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*"
  "Regex that matches an even number of \\ characters")

(defconst regex-tests-re-odd-escapes
  (concat regex-tests-re-even-escapes "\\\\")
  "Regex that matches an odd number of \\ characters")


(defun regex-tests-unextend (pattern)
  "Basic conversion from extended regexes to emacs ones.  This is
mostly a hack that adds \\ to () and | and {}, and removes it if
it already exists.  We also change \\S (and \\s) to \\S- (and
\\s-) because extended regexes see the former as whitespace, but
emacs requires an extra symbol character"

  (with-temp-buffer
    (insert pattern)
    (goto-char (point-min))

    (while (re-search-forward "[()|{}]" nil t)
      ;; point is past special character. If it is escaped, unescape
      ;; it

      (if (save-excursion
            (re-search-backward (concat regex-tests-re-odd-escapes ".\\=") nil t))

          ;; This special character is preceded by an odd number of \,
          ;; so I unescape it by removing the last one
          (progn
            (forward-char -2)
            (delete-char 1)
            (forward-char 1))

        ;; This special character is preceded by an even (possibly 0)
        ;; number of \. I add an escape
        (forward-char -1)
        (insert "\\")
        (forward-char 1)))

    ;; convert \s to \s-
    (goto-char (point-min))
    (while (re-search-forward (concat regex-tests-re-odd-escapes "[Ss]") nil t)
      (insert "-"))

    (buffer-string)))

(defun regex-tests-BOOST-frob-escapes (s ispattern)
  "Mangle \\ the way it is done in frob_escapes() in
regex-tests-BOOST.c in glibc: \\t, \\n, \\r are interpreted;
\\\\, \\^, \{, \\|, \} are unescaped for the string (not
pattern)"

  ;; this is all similar to (regex-tests-unextend)
  (with-temp-buffer
    (insert s)

    (let ((interpret-list (list "t" "n" "r")))
      (while interpret-list
        (goto-char (point-min))
        (while (re-search-forward
                (concat "\\(" regex-tests-re-even-escapes "\\)"
                        "\\\\" (car interpret-list))
                nil t)
          (replace-match (concat "\\1" (car (read-from-string
                                             (concat "\"\\" (car interpret-list) "\""))))))

        (setq interpret-list (cdr interpret-list))))

    (when (not ispattern)
      ;; unescape \\, \^, \{, \|, \}
      (let ((unescape-list (list "\\\\" "^" "{" "|" "}")))
        (while unescape-list
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\(" regex-tests-re-even-escapes "\\)"
                          "\\\\" (car unescape-list))
                  nil t)
            (replace-match (concat "\\1" (car unescape-list))))

          (setq unescape-list (cdr unescape-list))))
      )
    (buffer-string)))




(defconst regex-tests-BOOST-whitelist
  [
   ;; emacs is more stringent with regexes involving unbalanced )
   63 65 69

   ;; in emacs, regex . doesn't match \n
   91

   ;; emacs is more forgiving with * and ? that don't apply to
   ;; characters
   107 108 109 122 123 124 140 141 142

   ;; emacs accepts regexes with {}
   161

   ;; emacs doesn't fail on bogus ranges such as [3-1] or [1-3-5]
   222 223

   ;; emacs doesn't match (ab*)[ab]*\1 greedily: only 4 chars of
   ;; ababaaa match
   284 294

   ;; ambiguous groupings are ambiguous
   443 444 445 446 448 449 450

   ;; emacs doesn't know how to handle weird ranges such as [a-Z] and
   ;; [[:alpha:]-a]
   539 580 581

   ;; emacs matches non-greedy regex ab.*? non-greedily
   639 677 712
   ]
  "Line numbers in the boost test that should be skipped.  These
are false-positive test failures that represent known/benign
differences in behavior.")

;; - Format
;;   - Comments are lines starting with ;
;;   - Lines starting with - set options passed to regcomp() and regexec():
;;     - if no "REG_BASIC" is found, with have an extended regex
;;     - These set a flag:
;;       - REG_ICASE
;;       - REG_NEWLINE  (ignored by this function)
;;       - REG_NOTBOL
;;       - REG_NOTEOL
;;
;;   - Test lines are
;;     pattern string start0 end0 start1 end1 ...
;;
;;   - pattern, string can have escapes
;;   - string can have whitespace if enclosed in ""
;;   - if string is "!", then the pattern is supposed to fail compilation
;;   - start/end are of group0, group1, etc. group 0 is the full match
;;   - start<0 indicates "no match"
;;   - start is the 0-based index of the first character
;;   - end   is the 0-based index of the first character past the group
(defun regex-tests-BOOST ()
  (let (failures
        basic icase notbol noteol)
    (regex-tests-generic-line
     ?\; "BOOST.tests" regex-tests-BOOST-whitelist
     (if (save-excursion (re-search-forward "^-" nil t))
         (setq basic   (save-excursion (re-search-forward "REG_BASIC" nil t))
               icase   (save-excursion (re-search-forward "REG_ICASE" nil t))
               notbol  (save-excursion (re-search-forward "REG_NOTBOL" nil t))
               noteol  (save-excursion (re-search-forward "REG_NOTEOL" nil t)))

       (save-excursion
         (or (re-search-forward "\\(\\S-+\\)\\s-+\"\\(.*\\)\"\\s-+?\\(.+\\)" nil t)
             (re-search-forward "\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+?\\(.+\\)"  nil t)
             (re-search-forward "\\(\\S-+\\)\\s-+\\(!\\)"                    nil t)))

       (let* ((pattern-raw   (match-string 1))
              (string-raw    (match-string 2))
              (positions-raw (match-string 3))
              (pattern (regex-tests-BOOST-frob-escapes pattern-raw t))
              (string  (regex-tests-BOOST-frob-escapes string-raw  nil))
              (positions
               (if (string= string "!")
                   (list 'compilation-failed 0)
                 (mapcar
                  (lambda (x)
                    (let ((x (string-to-number x)))
                      (if (< x 0) nil x)))
                  (split-string positions-raw)))))

         (when (null (car positions))
           (setcar positions 'search-failed))

         (when (not basic)
           (setq pattern (regex-tests-unextend pattern)))

         ;; great. I now have all the data parsed. Let's use it to do
         ;; stuff
         (let* ((case-fold-search icase)
                (msg (regex-tests-match pattern string positions)))

           (if (and
                ;; Skipping test: notbol/noteol not supported
                (not notbol) (not noteol)

                msg)

               ;; store failure
               (setq failures
                     (cons (format "line number %d: Regex '%s': %s"
                                   line-number pattern msg)
                           failures)))))))

    failures))

(defconst regex-tests-PCRE-whitelist
  [
   ;; ambiguous groupings are ambiguous
   610 611 1154 1157 1160 1168 1171 1176 1179 1182 1185 1188 1193 1196 1203
  ]
  "Line numbers in the PCRE test that should be skipped.  These
are false-positive test failures that represent known/benign
differences in behavior.")

;; - Format
;;
;;  regex
;;  input_string
;;  group_num: group_match | "No match"
;;  input_string
;;  group_num: group_match | "No match"
;;  input_string
;;  group_num: group_match | "No match"
;;  input_string
;;  group_num: group_match | "No match"
;;  ...
(defun regex-tests-PCRE ()
  (let (failures
        pattern icase string what-failed matches-observed)
    (regex-tests-generic-line
     ?# "PCRE.tests" regex-tests-PCRE-whitelist

     (cond

      ;; pattern
      ((save-excursion (re-search-forward "^/\\(.*\\)/\\(.*i?\\)$" nil t))
       (setq icase (string= "i" (match-string 2))
             pattern (regex-tests-unextend (match-string 1))))

      ;; string. read it in, match against pattern, and save all the results
      ((save-excursion (re-search-forward "^    \\(.*\\)" nil t))
       (let ((case-fold-search icase))
         (setq string (match-string 1)

               ;; the regex match under test
               what-failed
               (condition-case nil
                   (if (string-match pattern string) nil 'search-failed)
                 ('invalid-regexp 'compilation-failed))

               matches-observed
               (cl-loop for x from 0 to 20
                        collect (and (not what-failed)
                                     (or (match-string x string) "<unset>")))))
       nil)

      ;; verification line: failed match
      ((save-excursion (re-search-forward "^No match" nil t))
       (unless what-failed
         (setq failures
               (cons (format "line number %d: Regex '%s': Expected no match; but match"
                             line-number pattern)
                     failures))))

      ;; verification line: succeeded match
      ((save-excursion (re-search-forward "^ *\\([0-9]+\\): \\(.*\\)" nil t))
       (let* ((match-ref (match-string 2))
              (idx       (string-to-number (match-string 1))))

         (if what-failed
             "Expected match; but no match"
           (unless (string= match-ref (elt matches-observed idx))
             (setq failures
                   (cons (format "line number %d: Regex '%s': Have expected match, but group %d is wrong: '%s'/'%s'"
                                 line-number pattern
                                 idx match-ref (elt matches-observed idx))
                         failures))))))

      ;; reset
      (t (setq pattern nil) nil)))

    failures))

(defconst regex-tests-PTESTS-whitelist
  [
   ;; emacs doesn't barf on weird ranges such as [b-a], but simply
   ;; fails to match
   138

   ;; emacs doesn't see DEL (0x78) as a [:cntrl:] character
   168
  ]
  "Line numbers in the PTESTS test that should be skipped.  These
are false-positive test failures that represent known/benign
differences in behavior.")

;; - Format
;;   - fields separated by ¦ (note: this is not a |)
;;   - start¦end¦pattern¦string
;;   - start is the 1-based index of the first character
;;   - end   is the 1-based index of the last  character
(defun regex-tests-PTESTS ()
  (let (failures)
    (regex-tests-generic-line
     ?# "PTESTS" regex-tests-PTESTS-whitelist
     (let* ((fields (split-string (buffer-string) "¦"))

            ;; string has 1-based index of first char in the
            ;; match. -1 means "no match". -2 means "invalid
            ;; regex".
            ;;
            ;; start-ref is 0-based index of first char in the
            ;; match
            ;;
            ;; string==0 is a special case, and I have to treat
            ;; it as start-ref = 0
            (start-ref (let ((raw (string-to-number (elt fields 0))))
                         (cond
                          ((= raw -2) 'compilation-failed)
                          ((= raw -1) 'search-failed)
                          ((= raw  0) 0)
                          (t          (1- raw)))))

            ;; string has 1-based index of last char in the
            ;; match. end-ref is 0-based index of first char past
            ;; the match
            (end-ref   (string-to-number (elt fields 1)))
            (pattern   (elt fields 2))
            (string    (elt fields 3)))

       (let ((msg (regex-tests-match pattern string (list start-ref end-ref))))
         (when msg
           (setq failures
                 (cons (format "line number %d: Regex '%s': %s"
                               line-number pattern msg)
                       failures))))))
    failures))

(defconst regex-tests-TESTS-whitelist
  [
   ;; emacs doesn't barf on weird ranges such as [b-a], but simply
   ;; fails to match
   42

   ;; emacs is more forgiving with * and ? that don't apply to
   ;; characters
   57 58 59 60

   ;; emacs is more stringent with regexes involving unbalanced )
   67
  ]
  "Line numbers in the TESTS test that should be skipped.  These
are false-positive test failures that represent known/benign
differences in behavior.")

;; - Format
;;   - fields separated by :. Watch for [\[:xxx:]]
;;   - expected:pattern:string
;;
;;   expected:
;;   | 0 | successful match      |
;;   | 1 | failed match          |
;;   | 2 | regcomp() should fail |
(defun regex-tests-TESTS ()
  (let (failures)
    (regex-tests-generic-line
     ?# "TESTS" regex-tests-TESTS-whitelist
     (if (save-excursion (re-search-forward "^\\([^:]+\\):\\(.*\\):\\([^:]*\\)$" nil t))
         (let* ((what-failed
                 (let ((raw (string-to-number (match-string 1))))
                   (cond
                    ((= raw 2) 'compilation-failed)
                    ((= raw 1) 'search-failed)
                    (t         t))))
                (string  (match-string 3))
                (pattern (regex-tests-unextend (match-string 2))))

           (let ((msg (regex-tests-match pattern string nil (list what-failed))))
             (when msg
               (setq failures
                     (cons (format "line number %d: Regex '%s': %s"
                                   line-number pattern msg)
                           failures)))))

       (error "Error parsing TESTS file line: '%s'" (buffer-string))))
    failures))

(ert-deftest regex-tests-BOOST ()
  "Tests of the regular expression engine.
This evaluates the BOOST test cases from glibc."
  (should-not (regex-tests-BOOST)))

(ert-deftest regex-tests-PCRE ()
  "Tests of the regular expression engine.
This evaluates the PCRE test cases from glibc."
  (should-not (regex-tests-PCRE)))

(ert-deftest regex-tests-PTESTS ()
  "Tests of the regular expression engine.
This evaluates the PTESTS test cases from glibc."
  (should-not (regex-tests-PTESTS)))

(ert-deftest regex-tests-TESTS ()
  "Tests of the regular expression engine.
This evaluates the TESTS test cases from glibc."
  (should-not (regex-tests-TESTS)))

(ert-deftest regex-repeat-limit ()
  "Test the #xFFFF repeat limit."
  (should (string-match "\\`x\\{65535\\}" (make-string 65535 ?x)))
  (should-not (string-match "\\`x\\{65535\\}" (make-string 65534 ?x)))
  (should-error (string-match "\\`x\\{65536\\}" "X") :type 'invalid-regexp))

;;; regex-tests.el ends here
