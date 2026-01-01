;;; minibuffer-tests.el --- Tests for completion functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'ert)
(require 'ert-x)

(eval-when-compile (require 'cl-lib))

(ert-deftest completion-test1 ()
  (with-temp-buffer
    (cl-flet* ((test/completion-table (string pred action)
                 (let ((completion-ignore-case t))
                   (complete-with-action action '("test: ") string pred)))
               (test/completion-at-point ()
                 (list (copy-marker (point-min))
                       (copy-marker (point))
                       #'test/completion-table)))
      (let ((completion-at-point-functions (list #'test/completion-at-point)))
        (insert "TEST")
        (completion-at-point)
        (should (equal (buffer-string)
                       "test: "))))))

(ert-deftest completion-table-with-predicate-test ()
  (let ((full-collection
         '("apple"                      ; Has A.
           "beet"                       ; Has B.
           "banana"                     ; Has A & B.
           "cherry"                     ; Has neither.
           ))
        (no-A (lambda (x) (not (string-match-p "a" x))))
        (no-B (lambda (x) (not (string-match-p "b" x)))))
    (should
     (member "cherry"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    (should-not
     (member "banana"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    ;; "apple" should still match when strict is nil.
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil)
                   no-B)))
    ;; "apple" should still match when strict is nil and pred2 is nil
    ;; (Bug#27841).
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil))))))

(ert-deftest completion-table-subvert-test ()
  (let* ((origtable '("A-hello" "A-there"))
         (subvtable (completion-table-subvert origtable "B" "A")))
    (should (equal (try-completion "B-hel" subvtable)
                   "B-hello"))
    (should (equal (all-completions "B-hel" subvtable) '("-hello")))
    (should (test-completion "B-hello" subvtable))
    (should (equal (completion-boundaries "B-hel" subvtable
                                          nil "suffix")
                   '(1 . 6)))))

(ert-deftest completion-table-test-quoting ()
  (let ((process-environment
         `("CTTQ1=ed" "CTTQ2=et/" ,@process-environment))
        (default-directory (ert-resource-directory)))
    (pcase-dolist (`(,input ,output)
                   '(
                     ;; Test that $ in files is properly $$ quoted.
                     ("data/m-cttq" "data/minibuffer-test-cttq$$tion")
                     ;; Test that $$ in input is properly unquoted.
                     ("data/m-cttq$$t" "data/minibuffer-test-cttq$$tion")
                     ;; Test that env-vars are preserved.
                     ("lisp/c${CTTQ1}et/se-u-c" "lisp/c${CTTQ1}et/semantic-utest-c.test")
                     ("lisp/ced${CTTQ2}se-u-c" "lisp/ced${CTTQ2}semantic-utest-c.test")
                     ;; Test that env-vars don't prevent partial-completion.
                     ("lis/c${CTTQ1}/se-u-c" "lisp/c${CTTQ1}et/semantic-utest-c.test")
                     ))
      (should (equal (completion-try-completion input
                                                #'completion--file-name-table
                                                nil (length input))
                     (cons output (length output)))))
    ;; Everything also works with `completion-ignore-case'.
    (let ((completion-ignore-case t))
      (pcase-dolist (`(,input ,output)
                     '(
                       ("data/M-CTTQ" "data/minibuffer-test-cttq$$tion")
                       ("data/M-CTTQ$$t" "data/minibuffer-test-cttq$$tion")
                       ;; When an env var is in the completion bounds, try-completion
                       ;; won't change letter case.
                       ("lisp/c${CTTQ1}E" "lisp/c${CTTQ1}Et/")
                       ("lisp/ced${CTTQ2}SE-U-c" "lisp/ced${CTTQ2}SEmantic-utest-c.test")
                       ;; If the env var is before the completion bounds, try-completion
                       ;; *will* change letter case.
                       ("lisp/c${CTTQ1}et/SE-U-c" "lisp/c${CTTQ1}et/semantic-utest-c.test")
                       ("lis/c${CTTQ1}/SE-U-c" "lisp/c${CTTQ1}et/semantic-utest-c.test")
                       ))
        (should (equal (car (completion-try-completion input
                                                       #'completion--file-name-table
                                                       nil (length input)))
                       output))))))

(ert-deftest completion--insert-strings-faces ()
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "suffix1")))
    (should (equal (get-text-property 12 'face) '(completions-annotations))))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" #("suffix1" 0 7 (face shadow)))))
    (should (equal (get-text-property 12 'face) 'shadow)))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "prefix1" "suffix1")))
    (should (equal (get-text-property 19 'face) nil)))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "prefix1" #("suffix1" 0 7 (face shadow)))))
    (should (equal (get-text-property 19 'face) 'shadow))))

(ert-deftest completion-pcm--optimize-pattern ()
  (should (equal (completion-pcm--optimize-pattern '("buf" point "f"))
                 '("buf" point "f")))
  (should (equal (completion-pcm--optimize-pattern '(any "" any))
                 '(any))))

(defun test-completion-all-sorted-completions (base def history-var history-list)
  (with-temp-buffer
    (insert base)
    (cl-letf (((symbol-function #'minibufferp) #'always))
      (let ((completion-styles '(basic))
            (completion-category-defaults nil)
            (completion-category-overrides nil)
            (minibuffer-history-variable history-var)
            (minibuffer-history history-list)
            (minibuffer-default def)
            (minibuffer-completion-table
             (lambda (str pred action)
               (pcase action
                 (`(boundaries . ,_) `(boundaries ,(length base) . 0))
                 (_ (complete-with-action
                     action
                     '("epsilon" "alpha" "gamma" "beta" "delta")
                     (substring str (length base)) pred))))))
        (completion-all-sorted-completions)))))

(ert-deftest completion-all-sorted-completions ()
  ;; No base, disabled history, no default
  (should (equal (test-completion-all-sorted-completions
                  "" nil t nil)
                 `("beta" "alpha" "delta" "gamma" "epsilon" . 0)))
  ;; No base, disabled history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" t nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, empty history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" 'minibuffer-history nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, empty history, default list
  (should (equal (test-completion-all-sorted-completions
                  "" '("gamma" "zeta") 'minibuffer-history nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" 'minibuffer-history '("other" "epsilon" "delta"))
                 `("gamma" "epsilon" "delta" "beta" "alpha"  . 0)))
  ;; Base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "base/" "base/gamma" 'minibuffer-history
                  '("some/alpha" "base/epsilon" "base/delta"))
                 `("gamma" "epsilon" "delta" "beta" "alpha"  . 5)))
  ;; Base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "base/" "gamma" 'minibuffer-history
                  '("some/alpha" "base/epsilon" "base/delta"))
                 `("epsilon" "delta" "beta" "alpha" "gamma"  . 5))))

(defun completion--pcm-score (comp)
  "Get `completion-score' from COMP."
  ;; FIXME, uses minibuffer.el implementation details
  (completion--flex-score comp completion-pcm--regexp))

(defun completion--pcm-first-difference-pos (comp)
  "Get `completions-first-difference' from COMP."
  (cl-loop for pos = (next-single-property-change 0 'face comp)
           then (next-single-property-change pos 'face comp)
           while pos
           when (eq (get-text-property pos 'face comp)
                    'completions-first-difference)
           return pos))

(ert-deftest completion-test--pcm-bug38458 ()
  (should (equal (let ((completion-ignore-case t))
                   (completion-pcm--merge-try '("tes" point "ing")
                                              '("Testing" "testing")
                                              "" ""))
                 '("testing" . 7)))
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm-try-completion "tes" '("Testing" "testing") nil 3))
           '("testing" . 7))))

(ert-deftest completion-pcm-test-1 ()
  ;; Point is at end, this does not match anything
  (should (null
           (completion-pcm-all-completions
            "foo" '("hello" "world" "barfoobar") nil 3))))

(ert-deftest completion-pcm-test-2 ()
  ;; Point is at beginning, this matches "barfoobar"
  (should (equal
           (car (completion-pcm-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 0))
           "barfoobar")))

(ert-deftest completion-pcm-test-3 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-pcm-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-pcm-test-4 ()
  ;; One fourth of a match and no match due to point being at the end
  (should (eql
           (completion--pcm-score
            (car (completion-pcm-all-completions
                  "RO" '("RaOb") nil 1)))
           (/ 1.0 4.0)))
  (should (null
           (completion-pcm-all-completions
            "RO" '("RaOb") nil 2))))

(ert-deftest completion-pcm-test-5 ()
  ;; Since point is at the beginning, there is nothing that can really
  ;; be typed anymore
  (should (null
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "f" '("few" "many") nil 0)))))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "a*" '("ab" "ac") nil 2)))
           1))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "a*" '("ab" "ac") nil 1)))
           1))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "a*x" '("abx" "acx") nil 2)))
           1))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "a*x" '("abxd" "acxe") nil 3)))
           ;; FIXME: the highlighting should start at the 4th character
           ;; rather than the third.
           3)))

(ert-deftest completion-pcm-test-6 ()
  ;; Wildcards and delimiters work
  (should (equal
           (car (completion-pcm-all-completions
                 "li-pac*" '("list-packages") nil 7))
           "list-packages"))
  (should (null
           (car (completion-pcm-all-completions
                 "li-pac*" '("do-not-list-packages") nil 7)))))

(ert-deftest completion-pcm-test-7 ()
  ;; Wildcards are preserved even when right before a delimiter.
  (should (equal
           (completion-pcm-try-completion
            "x*/"
            '("x1/y1" "x2/y2")
            nil 3)
           '("x*/y" . 4)))
  ;; Or around point.
  (should (equal
           (completion-pcm--merge-try
            '(point star "foo") '("xxfoo" "xyfoo") "" "")
           '("x*foo" . 1)))
  (should (equal
           (completion-pcm--merge-try
            '(star point "foo") '("xxfoo" "xyfoo") "" "")
           '("x*foo" . 2)))
  ;; This is important if the wildcard is at the start of a component.
  (should (equal
           (completion-pcm-try-completion
            "*/minibuf"
            '("lisp/minibuffer.el" "src/minibuf.c")
            nil 9)
           '("*/minibuf" . 9)))
  ;; A series of wildcards is preserved (for now), along with point's position.
  (should (equal
           (completion-pcm--merge-try
            '(star star point star "foo") '("xxfoo" "xyfoo") "" "")
           '("x***foo" . 3)))
  ;; The series of wildcards is considered together; if any of them wants the common suffix, it's generated.
  (should (equal
           (completion-pcm--merge-try
            '(prefix any) '("xfoo" "yfoo") "" "")
           '("foo" . 0)))
  ;; We consider each series of wildcards separately: if one series
  ;; wants the common suffix, but the next one does not, it doesn't get
  ;; the common suffix.
  (should (equal
           (completion-pcm--merge-try
            '(prefix any "bar" any) '("xbarxfoo" "ybaryfoo") "" "")
           '("bar" . 3))))

(ert-deftest completion-pcm-test-8 ()
  ;; try-completion inserts the common prefix and suffix at point.
  (should (equal (completion-pcm-try-completion
                  "r" '("fooxbar" "fooybar") nil 0)
                 '("foobar" . 3)))
  ;; Even if point is at the end of the minibuffer.
  (should (equal (completion-pcm-try-completion
                  "" '("fooxbar" "fooybar") nil 0)
                 '("foobar" . 3))))

(ert-deftest completion-pcm-test-anydelim ()
  ;; After each delimiter is a special wildcard which matches any
  ;; sequence of delimiters.
  (should (equal (completion-pcm-try-completion
                  "-x" '("-_.x" "-__x") nil 2)
                 '("-_x" . 3))))

(ert-deftest completion-pcm-test-pattern->regex ()
  (should (equal (completion-pcm--pattern->regex
                  '("A" any prefix "B" point "C"))
                 "\\`A[^z-a]*?B[^z-a]*?C"))
  (should (equal (completion-pcm--pattern->regex
                  '(any any-delim prefix "A" "B" "C"))
                 "\\`[^z-a]*?ABC"))
  (should (equal (completion-pcm--pattern->regex
                  '(any-delim "A" "B" star "C"))
                 "\\`[-_./:| *]*?AB[^z-a]*?C"))
  (should (equal (completion-pcm--pattern->regex
                  '(any "A" any-delim "B" any-delim "C" any))
                 "\\`[^z-a]*?A[-_./:| *]*?B[-_./:| *]*?C[^z-a]*?")))

(ert-deftest completion-pcm--test-zerowidth-delim ()
  (let ((completion-pcm--delim-wild-regex ; Let "u8" complete to "utf-8".
         (concat "\\(?:" completion-pcm--delim-wild-regex
                 "\\|\\([[:alpha:]]\\)[[:digit:]]\\)")))
    (should (equal (car (completion-try-completion
                         "u8-d" '("utf-8-dos" "utf-8-foo-dos" "utf-8-unix")
                         nil 4))
                   "utf-8-dos"))))

(ert-deftest completion-pcm-bug4219 ()
  ;; With `completion-ignore-case', try-completion should change the
  ;; case of existing text when the completions have different casing.
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm-try-completion "a" '("ABC" "ABD") nil 1))
           '("AB" . 2)))
  ;; Even when the text isn't growing.
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm-try-completion "ab" '("ABC" "ABD") nil 2))
           '("AB" . 2)))
  ;; Or when point is in the middle of the region changing case.
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm-try-completion "ab" '("ABC" "ABD") nil 1))
           '("AB" . 2)))
  ;; Even when the existing minibuffer contents has mixed case.
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm-try-completion "Ab" '("ABC" "ABD") nil 1))
           '("AB" . 2)))
  ;; But not if the completions don't actually all have the same case.
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm-try-completion "Ab" '("abc" "ABD") nil 1))
           '("Ab" . 2)))
  ;; We don't change case if it doesn't match all of the completions, though.
  (should (equal
           (let ((completion-ignore-case t)) (try-completion "a" '("ax" "Ay")))
           "a"))
  (should (equal
           (let ((completion-ignore-case t)) (try-completion "a" '("Ay" "ax")))
           "a")))

(ert-deftest completion-substring-test-1 ()
  ;; One third of a match!
  (should (equal
           (car (completion-substring-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 3))
           "barfoobar"))
  (should (eql
           (completion--pcm-score
            (car (completion-substring-all-completions
                  "foo" '("hello" "world" "barfoobar") nil 3)))
           (/ 1.0 3.0))))

(ert-deftest completion-substring-test-2 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-substring-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-substring-test-3 ()
  ;; Substring match
  (should (equal
           (car (completion-substring-all-completions
                 "custgroup" '("customize-group") nil 4))
           "customize-group"))
  (should (null
           (car (completion-substring-all-completions
                 "custgroup" '("customize-group") nil 5)))))

(ert-deftest completion-substring-test-4 ()
  ;; `completions-first-difference' should be at the right place
  (should (eql
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjobstabby" "many") nil 1)))
           4))
  (should (null
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 1)))))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 3)))
           6)))

(ert-deftest completion-substring-test-5 ()
  ;; Normally a `prefix' wildcard ignores the common prefix to its
  ;; left, since it only grows the common suffix; but if that common
  ;; prefix is also a common suffix, it should be included.
  (should (equal
           (completion-pcm--merge-try '(prefix "b") '("ab" "sab") "" "")
           '("ab" . 2)))
  (should (equal
           (completion-pcm--merge-try '(prefix "b") '("ab" "ab") "" "")
           '("ab" . 2)))
  ;; When there's a fixed string before `prefix', that fixed string
  ;; should always be included.
  (should (equal
           (completion-pcm--merge-try '("a" prefix "b") '("axb" "ayb") "" "")
           '("ab" . 2)))
  ;; Letter-casing from the completions on the common prefix is still applied.
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm--merge-try '("a" prefix "b") '("Axb" "Ayb") "" ""))
           '("Ab" . 2)))
  (should (equal
           (let ((completion-ignore-case t))
             (completion-pcm--merge-try '("a" prefix "b") '("AAxb" "AAyb") "" ""))
           '("Ab" . 2)))
  ;; substring completion should successfully complete the entire string
  (should (equal
           (completion-substring-try-completion "b" '("ab" "ab") nil 0)
           '("ab" . 2))))

(ert-deftest completion-flex-test-1 ()
  ;; Fuzzy match
  (should (equal
           (car (completion-flex-all-completions
                 "foo" '("hello" "world" "fabrobazo") nil 3))
           "fabrobazo")))

(ert-deftest completion-flex-test-2 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-flex-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-flex-test-3 ()
  ;; Another fuzzy match, but more of a "substring" one
  (should (equal
           (car (completion-flex-all-completions
                 "custgroup" '("customize-group-other-window") nil 4))
           "customize-group-other-window"))
  ;; `completions-first-difference' should be at the right place
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 4)))
           4))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 9)))
           15)))


(defmacro with-minibuffer-setup (completing-read &rest body)
  (declare (indent 1) (debug t))
  `(catch 'result
     (minibuffer-with-setup-hook
         (lambda ()
           (let ((redisplay-skip-initial-frame nil)
                 (executing-kbd-macro nil)) ; Don't skip redisplay
             (throw 'result (progn . ,body))))
       (let ((executing-kbd-macro t)) ; Force the real minibuffer
         ,completing-read))))

(defmacro completing-read-with-minibuffer-setup (collection &rest body)
  (declare (indent 1) (debug t))
  `(catch 'result
     (minibuffer-with-setup-hook
         (lambda ()
           (let ((redisplay-skip-initial-frame nil)
                 (executing-kbd-macro nil)) ; Don't skip redisplay
             (throw 'result (progn . ,body))))
       (let ((executing-kbd-macro t)) ; Force the real minibuffer
         (completing-read "Prompt: " ,collection)))))

(ert-deftest completion-auto-help-test ()
  (let (messages)
    (cl-letf* (((symbol-function 'minibuffer-message)
                (lambda (message &rest args)
                  (push (apply #'format-message message args) messages))))
      (let ((completion-auto-help nil))
        (completing-read-with-minibuffer-setup
            '("a" "ab" "ac")
          (execute-kbd-macro (kbd "a TAB TAB"))
          (should (equal (car messages) "Complete, but not unique"))
          (should-not (minibuffer--completions-visible))
          (execute-kbd-macro (kbd "b TAB"))
          (should (equal (car messages) "Sole completion"))))
      (let ((completion-auto-help t))
        (completing-read-with-minibuffer-setup
            '("a" "ab" "ac")
          (execute-kbd-macro (kbd "a TAB TAB"))
          (should (minibuffer--completions-visible))
          (execute-kbd-macro (kbd "b TAB"))
          (should (equal (car messages) "Sole completion"))))
      (let ((completion-auto-help 'visible))
        (completing-read-with-minibuffer-setup
         '("a" "ab" "ac" "achoo")
         (execute-kbd-macro (kbd "a TAB TAB"))
         (should (minibuffer--completions-visible))
         (execute-kbd-macro (kbd "ch TAB"))
         (should (equal (car messages) "Sole completion")))))))

(ert-deftest completion-auto-select-test ()
  (let ((completion-auto-select t))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (execute-kbd-macro (kbd "a TAB"))
      (should (and (minibuffer--completions-visible)
                   (eq (current-buffer) (get-buffer "*Completions*"))))
      (execute-kbd-macro (kbd "TAB TAB TAB"))
      (should (and (minibuffer--completions-visible)
                   (eq (current-buffer) (get-buffer " *Minibuf-1*"))))
      (execute-kbd-macro (kbd "S-TAB"))
      (should (and (minibuffer--completions-visible)
                   (eq (current-buffer) (get-buffer "*Completions*"))))))
  (let ((completion-auto-select 'second-tab))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (execute-kbd-macro (kbd "a TAB"))
      (should (and (minibuffer--completions-visible)
                   (not (eq (current-buffer) (get-buffer "*Completions*")))))
      (execute-kbd-macro (kbd "TAB TAB"))
      (should (eq (current-buffer) (get-buffer "*Completions*"))))))

(ert-deftest completion-auto-wrap-test ()
  (let ((completion-auto-wrap nil))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#54374
      (next-completion 5)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (previous-completion 5)
      (should (equal "aa" (get-text-property (point) 'completion--string)))

      (first-completion)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-line-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-line-completion 5)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (previous-line-completion 5)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (goto-char (point-min))
      (next-line-completion 5)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (goto-char (point-min))
      (previous-line-completion 5)
      (should (equal "aa" (get-text-property (point) 'completion--string)))))
  (let ((completion-auto-wrap t))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-completion 1)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (previous-completion 1)
      (should (equal "ac" (get-text-property (point) 'completion--string)))

      (first-completion)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-line-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-line-completion 1)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (previous-line-completion 1)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (goto-char (point-min))
      (next-line-completion 4)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (goto-char (point-min))
      (previous-line-completion 4)
      (should (equal "ac" (get-text-property (point) 'completion--string))))))

(ert-deftest completion-next-line-multline-test ()
  (let ((completion-auto-wrap t))
    (completing-read-with-minibuffer-setup
     '("a\na" "a\nb" "ac")
     (insert "a")
     (minibuffer-completion-help)
     (switch-to-completions)
     (goto-char (point-min))
     (next-line-completion 5)
     (should (equal "a\nb" (get-text-property (point) 'completion--string)))
     (goto-char (point-min))
     (previous-line-completion 5)
     (should (equal "a\nb" (get-text-property (point) 'completion--string))))))

(ert-deftest completions-header-format-test ()
  (let ((completion-show-help nil)
        (minibuffer-completion-auto-choose t)
        (completions-header-format nil))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      ;; Fixed in bug#55430
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (previous-completion 2)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#54374
      (previous-completion 1)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-completion 1)
      (should (equal "aa" (get-text-property (point) 'completion--string)))

      (next-line-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (previous-line-completion 2)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (previous-line-completion 1)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-line-completion 1)
      (should (equal "aa" (get-text-property (point) 'completion--string)))

      ;; Fixed in bug#55430
      (execute-kbd-macro (kbd "C-u RET"))
      (should (equal (minibuffer-contents) "aa")))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      ;; Fixed in bug#55289
      (execute-kbd-macro (kbd "a M-<up> M-<down>"))
      (should (equal (minibuffer-contents) "aa")))))

(ert-deftest completions-affixation-navigation-test ()
  (let ((completion-extra-properties
         `(:affixation-function
           ,(lambda (completions)
              (mapcar (lambda (c)
                        (list c "prefix " " suffix"))
                      completions)))))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      (should (equal 'highlight (get-text-property (point) 'mouse-face)))
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (let ((completion-auto-wrap t))
        (next-completion 3))
      (should (equal 'highlight (get-text-property (point) 'mouse-face)))
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (let ((completion-auto-wrap nil))
        (next-completion 3))
      (should (equal 'highlight (get-text-property (point) 'mouse-face)))
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#54374
      (goto-char (1- (point-max)))
      (should-not (equal 'highlight (get-text-property (point) 'mouse-face)))

      (first-completion)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (let ((completion-auto-wrap t))
        (next-line-completion 3))
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (let ((completion-auto-wrap nil))
        (next-line-completion 3))
      (should (equal "ac" (get-text-property (point) 'completion--string)))

      (execute-kbd-macro (kbd "C-u RET"))
      (should (equal (minibuffer-contents) "ac")))))

(ert-deftest completions-group-navigation-test ()
  (completing-read-with-minibuffer-setup
      (lambda (string pred action)
	(if (eq action 'metadata)
	    `(metadata
	      (group-function
	       . ,(lambda (name transform)
                    (if transform
                        name
                      (pcase name
                        (`"aa1" "Group 1")
                        (`"aa2" "Group 1")
                        (`"aa3" "Group 1")
                        (`"aa4" "Group 1")
                        (`"ab1" "Group 2")
                        (`"ac1" "Group 3")
                        (`"ac2" "Group 3")))))
	      (category . unicode-name))
	  (complete-with-action action '("aa1" "aa2" "aa3" "aa4" "ab1" "ac1" "ac2")
                                string pred)))
    (insert "a")
    (minibuffer-completion-help)
    (switch-to-completions)
    (should (equal "aa1" (get-text-property (point) 'completion--string)))
    (let ((completion-auto-wrap t))
      (next-completion 7))
    (should (equal "aa1" (get-text-property (point) 'completion--string)))
    (let ((completion-auto-wrap nil))
      (next-completion 7))
    (should (equal "ac2" (get-text-property (point) 'completion--string)))

    (let ((completion-auto-wrap t))
      ;; First column
      (first-completion)
      (next-line-completion 1)
      (should (equal "aa4" (get-text-property (point) 'completion--string)))
      (next-line-completion 3)
      (should (equal "aa1" (get-text-property (point) 'completion--string)))
      (previous-line-completion 2)
      (should (equal "ab1" (get-text-property (point) 'completion--string)))

      ;; Second column
      (first-completion)
      (next-completion 1)
      (should (equal "aa2" (get-text-property (point) 'completion--string)))
      (next-line-completion 1)
      (should (equal "ac2" (get-text-property (point) 'completion--string)))
      (next-line-completion 1)
      (should (equal "aa2" (get-text-property (point) 'completion--string)))
      (previous-line-completion 1)
      (should (equal "ac2" (get-text-property (point) 'completion--string)))
      (previous-line-completion 1)
      (should (equal "aa2" (get-text-property (point) 'completion--string)))

      ;; Third column
      (first-completion)
      (next-completion 2)
      (should (equal "aa3" (get-text-property (point) 'completion--string)))
      (next-line-completion 1)
      (should (equal "aa3" (get-text-property (point) 'completion--string))))

    (let ((completion-auto-wrap nil))
      (first-completion)
      (next-line-completion 7)
      (should (equal "ac1" (get-text-property (point) 'completion--string)))
      (previous-line-completion 7)
      (should (equal "aa1" (get-text-property (point) 'completion--string))))))

(defun completions-format-navigation--tests (n)
  "Make tests for navigating buffer of N completion candidate.
The tests check expected results of navigating with and without wrapping
for combinations of the values of `completion-auto-wrap' and
`completions-format' (see bug#78959 for motivation and discussion of the
expected behavior).  The tests are actually run by calling this
function, with specific values of N (> 1 to have a \"*Completions*\"
buffer), from functions defined by `ert-deftest.'"
  (let* (
         ;; Make list of N unique completions.
         (letters (mapcar 'string (number-sequence 97 122)))
         (gen-compl (lambda (x)
                      (let (comps)
                        (dotimes (_ x)
                          (push (concat (car comps) (pop letters)) comps))
                        (nreverse comps))))
         (completions (funcall gen-compl n))

         ;; Navigation tests.
         ;; (i) For both horizontal and vertical formats.
         (all-completions
          (lambda (type)
            (let ((next-fn (pcase type
                             ('any 'next-completion)
                             ('column 'next-column-completion)
                             ('line 'next-line-completion)))
                  (prev-fn (pcase type
                             ('any 'previous-completion)
                             ('column 'previous-column-completion)
                             ('line 'previous-line-completion))))
              (completing-read-with-minibuffer-setup completions
                (insert (car completions))
                (minibuffer-completion-help)
                (switch-to-completions)
                ;; Sanity check that we're on first completion candidate.
                (should
                 (equal (car completions)
                        (get-text-property (point) 'completion--string)))
                ;; Double check.
                (first-completion)
                (should
                 (equal (car completions)
                        (get-text-property (point) 'completion--string)))
                ;; Test moving from first to Ith next completion
                ;; candidate (0<I<N-1).
                (dolist (i (number-sequence 1 (1- n)))
                  (funcall next-fn i)
                  (should
                   (equal (nth i completions)
                          (get-text-property (point) 'completion--string)))
                  (if (< i (1- n))
                      (first-completion)
                    (last-completion)
                    (should
                     (equal (nth i completions)
                            (get-text-property (point) 'completion--string)))
                    (funcall next-fn 1)
                    (if completion-auto-wrap
                        ;; Wrap around to first completion candidate.
                        (should
                         (equal (car completions)
                                (get-text-property (point) 'completion--string)))
                      ;; No wrapping.
                      (should
                       (equal (nth i completions)
                              (get-text-property (point) 'completion--string))))))
                (last-completion)
                ;; Test moving from last to Ith previous completion
                ;; candidate (0<I<N-1).
                (dolist (i (number-sequence 1 (1- n)))
                  (funcall prev-fn i)
                  (should
                   (equal (nth (- n i 1) completions)
                          (get-text-property (point) 'completion--string)))
                  (if (< i (1- n))
                      (last-completion)
                    (first-completion)
                    (should
                     (equal (car completions)
                            (get-text-property (point) 'completion--string)))
                    (funcall prev-fn 1)
                    (if completion-auto-wrap
                        ;; Wrap around to last completion candidate.
                        (should
                         (equal (nth (1- n) completions)
                                (get-text-property (point) 'completion--string)))
                      ;; No wrapping.
                      (should
                       (equal (car completions)
                              (get-text-property (point)
                                                 'completion--string))))))))))

         ;; (ii) Only for horizontal format.
         (within-column
          (lambda ()
            (completing-read-with-minibuffer-setup completions
              (insert (car completions))
              (minibuffer-completion-help)
              (switch-to-completions)
              (while (not (eolp))
                (completion--move-to-candidate-start)
                (let* ((first (get-text-property (point) 'completion--string))
                       (pos (point))
                       (i 0)
                       last1 last2)
                  ;; Keep moving to next completion candidate in this
                  ;; column until we reach the last one, and then wrap
                  ;; back to the first candidate, if
                  ;; `completion-auto-wrap' is non-nil, otherwise stay
                  ;; on the last one.
                  (while (or (and completion-auto-wrap
                                  (not (equal last1 first)))
                             (not (equal last1
                                         (get-text-property
                                          (point) 'completion--string))))
                    (setq last2 last1)
                    (next-line-completion 1)
                    (incf i)
                    ;; Set `last1' to either the first or last
                    ;; candidate, depending on the value of
                    ;; `completion-auto-wrap'.
                    (setq last1 (get-text-property (point) 'completion--string)))
                  (setq last1 last2
                        last2 nil)
                  (decf i)
                  (when completion-auto-wrap
                    (should (equal (get-text-property (point) 'completion--string)
                                   first))
                    ;; Test wrapping from last to first line in this column.
                    (next-line-completion i) ; Move to last candidate.
                    (should (equal (get-text-property (point) 'completion--string)
                                   last1)))
                  ;; Now keeping move from last to first completion
                  ;; candidate in this column.
                  (while (or (not (equal last2 first))
                             (not (equal last2
                                         (get-text-property
                                          (point) 'completion--string))))
                    (previous-line-completion 1)
                    (setq last2 (get-text-property (point) 'completion--string)))
                  ;; Test wrapping from first to last line in this column.
                  (when completion-auto-wrap
                    (should (equal (get-text-property (point) 'completion--string)
                                   first))
                    (next-line-completion i)
                    (should (equal (get-text-property (point) 'completion--string)
                                   last1)))
                  ;; Move to first candidate in next column to continue loop
                  (completion--move-to-candidate-end)
                  (unless (eolp)
                    (goto-char pos)
                    (next-column-completion 1)))))))

         ;; (iii) Only for vertical format.
         (within-line
          (lambda ()
            (completing-read-with-minibuffer-setup completions
              (insert (car completions))
              (minibuffer-completion-help)
              (switch-to-completions)
              (let ((one-col (save-excursion
                                (first-completion)
                                (completion--move-to-candidate-end)
                                (eolp))))
                (while (not (eobp))
                  (let ((first (get-text-property (point) 'completion--string))
                         last pos)
                    ;; Test moving to next column in this line.
                    (while (not (eolp))
                      (next-column-completion 1)
                      (let ((next (get-text-property (point) 'completion--string)))
                        (should
                         ;; FIXME: tautology?
                         (equal (nth (seq-position completions next) completions)
                                next)))
                      ;; If this is the last completion in this line,
                      ;; exit the loop.
                      (when (or (> (current-column) 0)
                                (save-excursion (and (forward-line) (eobp)))
                                (unless one-col
                                  (save-excursion
                                    (and (progn
                                           (completion--move-to-candidate-start)
                                           (bolp))
                                         (progn
                                           (completion--move-to-candidate-end)
                                           (eolp))))))
                        (completion--move-to-candidate-end)))
                    (backward-char)
                    (completion--move-to-candidate-start)
                    (setq last (get-text-property (point) 'completion--string)
                          pos (point))
                    ;; We're on the last column, so next move either
                    ;; wraps or stays put.
                    (next-column-completion 1)
                    (if completion-auto-wrap
                        ;; We wrapped around to first candidate in this line.
                        (progn
                          (should (bolp))
                          (should
                           (equal (get-text-property (point) 'completion--string)
                                  first))
                          ;; Go back to last completion in this line for next test.
                          (goto-char (if one-col pos (pos-eol)))
                          (backward-char))
                      (should
                       (equal (get-text-property (point) 'completion--string)
                              last)))
                    ;; Test moving to previous column in this line.
                    (while (if one-col
                               (save-excursion
                                 (forward-line -1)
                                 (get-text-property (point) 'completion--string))
                             (not (bolp)))
                      (previous-column-completion 1)
                      (let ((prev (get-text-property (point) 'completion--string)))
                        (should
                         ;; FIXME: tautology?
                         (equal (nth (seq-position completions prev) completions)
                                prev))))
                    ;; We're on the first column, so next move either
                    ;; wraps or stay put.
                    (previous-column-completion 1)
                    (if completion-auto-wrap
                        ;; We wrapped around to last candidate in this line.
                        (progn
                          (completion--move-to-candidate-end)
                          (should (eolp))
                          (backward-char)
                          (should
                           (equal (get-text-property (point) 'completion--string)
                                  last)))
                      ;; We stayed on the first candidate.
                      (should
                       (equal (get-text-property (point) 'completion--string)
                              first)))
                    (if one-col
                        (goto-char (point-max))
                      (forward-line)))))))))

    ;; Run tests.
    ;; Test navigation with wrapping...
    (let ((completion-auto-wrap t))
      ;; ...in horizontal format,
      (let ((completions-format 'horizontal))
        (funcall all-completions 'any)
        (funcall all-completions 'column)
        (funcall within-column))
      ;; ...in vertical format.
      (let ((completions-format 'vertical))
        (funcall all-completions 'any)
        (funcall all-completions 'line)
        (funcall within-line)))
    ;; Test navigation without wrapping...
    (let ((completion-auto-wrap nil))
      ;; ...in horizontal format,
      (let ((completions-format 'horizontal))
        (funcall all-completions 'any)
        (funcall all-completions 'column)
        (funcall within-column))
      ;; ...in vertical format.
      (let ((completions-format 'vertical))
        (funcall all-completions 'any)
        (funcall all-completions 'line)
        (funcall within-line)))))

;; (ert-deftest completions-format-navigation-test-1 ()
;;   (completions-format-navigation--tests 1))

(ert-deftest completions-format-navigation-test-2 ()
  (completions-format-navigation--tests 2))

(ert-deftest completions-format-navigation-test-3 ()
  (completions-format-navigation--tests 3))

(ert-deftest completions-format-navigation-test-4 ()
  (completions-format-navigation--tests 4))

(ert-deftest completions-format-navigation-test-5 ()
  (completions-format-navigation--tests 5))

(ert-deftest completions-format-navigation-test-9 ()
  (completions-format-navigation--tests 9))

(ert-deftest completions-format-navigation-test-10 ()
  (completions-format-navigation--tests 10))

(ert-deftest completions-format-navigation-test-15 ()
  (completions-format-navigation--tests 15))

(ert-deftest completions-format-navigation-test-16 ()
  (completions-format-navigation--tests 16))

(ert-deftest completion-cycle ()
  (completing-read-with-minibuffer-setup '("aaa" "bbb" "ccc")
    (let ((completion-cycle-threshold t))
      (execute-kbd-macro (kbd "TAB TAB TAB"))
      (should (equal (minibuffer-contents) "ccc")))))

(ert-deftest minibuffer-next-completion ()
  (let ((default-directory (ert-resource-directory))
        (minibuffer-completion-auto-choose t))
    (completing-read-with-minibuffer-setup #'read-file-name-internal
      (insert "d/")
      (execute-kbd-macro (kbd "M-<down> M-<down> M-<down>"))
      (should (equal "data/minibuffer-test-cttq$$tion" (minibuffer-contents))))))

(ert-deftest minibuffer-completion-RET-prefix ()
  ;; REQUIRE-MATCH=nil
  (with-minibuffer-setup
      (completing-read ":" '("aaa" "bbb" "ccc") nil nil)
    (execute-kbd-macro (kbd "M-<down> M-<down> C-u RET"))
    (should (equal "bbb" (minibuffer-contents))))
  ;; REQUIRE-MATCH=t
  (with-minibuffer-setup
   (completing-read ":" '("aaa" "bbb" "ccc") nil t)
   (execute-kbd-macro (kbd "M-<down> M-<down> C-u RET"))
   (should (equal "bbb" (minibuffer-contents)))))

(defun test/completion-at-point ()
  (list (point-min) (point) '("test:a" "test:b")))

(ert-deftest completion-in-region-next-completion ()
  (with-current-buffer (get-buffer-create "*test*")
    ;; Put this buffer in the selected window so
    ;; `minibuffer--completions-visible' works.
    (pop-to-buffer (current-buffer))
    (setq-local completion-at-point-functions (list #'test/completion-at-point))
    (insert "test:")
    (completion-help-at-point)
    (should (minibuffer--completions-visible))
    ;; C-u RET and RET have basically the same behavior for
    ;; completion-in-region-mode, since they both dismiss *Completions*
    ;; while leaving completion-in-region-mode still active.
    (execute-kbd-macro (kbd "M-<down>"))
    (should (equal (completion--selected-candidate) "test:a"))
    (execute-kbd-macro (kbd "C-u RET"))
    (should (equal (buffer-string) "test:a"))
    (delete-char -1)
    (completion-help-at-point)
    (execute-kbd-macro (kbd "M-<down> M-<down>"))
    (should (equal (completion--selected-candidate) "test:b"))
    (execute-kbd-macro (kbd "RET"))
    (should (equal (buffer-string) "test:b"))))

(ert-deftest completion-category-inheritance ()
  (let ((completion-category-defaults
         '((cat (display-sort-function . foo))
           (dog (display-sort-function . bar)
                (group-function        . baz)
                (annotation-function   . qux)
                (styles                basic))
           (fel (annotation-function   . spam))
           (testtesttest (styles substring))))
        (origt (get 'testtesttest 'completion-category-parents))
        (origc (get 'cat 'completion-category-parents)))
    (unwind-protect
        (progn
          (put 'testtesttest 'completion-category-parents '(cat dog))
          (put 'cat 'completion-category-parents '(fel))
          ;; Value from first parent.
          (should (eq
                   (completion-metadata-get '((category . testtesttest))
                                            'display-sort-function)
                   'foo))
          ;; Value from parent of first parent, not from second parent.
          (should (eq
                   (completion-metadata-get '((category . testtesttest))
                                            'annotation-function)
                   'spam))
          ;; Value from second parent.
          (should (eq
                   (completion-metadata-get '((category . testtesttest))
                                            'group-function)
                   'baz))
          ;; Property specified directly, takes precedence.
          (should (equal
                   (completion-metadata-get '((category . testtesttest))
                                            'styles)
                   '(substring)))
          ;; Not specified and not inherited.
          (should-not (completion-metadata-get '((category . testtesttest))
                                               'affixation-function)))
      (put 'cat 'completion-category-parents origc)
      (put 'testtesttest 'completion-category-parents origt))))

(provide 'minibuffer-tests)
;;; minibuffer-tests.el ends here
