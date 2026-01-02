;;; replace-tests.el --- tests for replace.el.  -*- lexical-binding:t -*-

;; Copyright (C) 2010-2026 Free Software Foundation, Inc.

;; Author: Nicolas Richard <youngfrog@members.fsf.org>
;; Author: Juri Linkov <juri@jurta.org>

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
(eval-when-compile (require 'subr-x))

(ert-deftest query-replace--split-string-tests ()
  (let ((sep (propertize "\0" 'separator t)))
    (dolist (before '("" "b"))
      (dolist (after '("" "a"))
        (should (equal
                 (query-replace--split-string (concat before sep after))
                 (cons before after)))
        (should (equal
                 (query-replace--split-string (concat before "\0" after))
                 (concat before "\0" after)))))))

(defconst replace-occur-tests
  '(
    ;; * Test one-line matches (at bob, eob, bol, eol).
    ("x" 0 "\
xa
b
cx
xd
xex
fx
" "\
6 matches in 5 lines for \"x\" in buffer:  *test-occur*
      1:xa
      3:cx
      4:xd
      5:xex
      6:fx
")
    ;; * Test multi-line matches, this is the first test from
    ;; https://lists.gnu.org/r/emacs-devel/2005-06/msg01008.html
    ;; where numbers are replaced with letters.
    ("a\na" 0 "\
a
a
a
a
a
" "\
2 matches for \"a\na\" in buffer:  *test-occur*
      1:a
       :a
      3:a
       :a
")
    ;; * Test multi-line matches, this is the second test from
    ;; https://lists.gnu.org/r/emacs-devel/2005-06/msg01008.html
    ;; where numbers are replaced with letters.
    ("a\nb" 0 "\
a
b
c
a
b
" "\
2 matches for \"a\nb\" in buffer:  *test-occur*
      1:a
       :b
      4:a
       :b
")
    ;; * Test line numbers for multi-line matches with empty last match line.
    ("a\n" 0 "\
a

c
a

" "\
2 matches for \"a\n\" in buffer:  *test-occur*
      1:a
       :
      4:a
       :
")
    ;; * Test multi-line matches with 3 match lines.
    ("x\n.x\n" 0 "\
ax
bx
c
d
ex
fx
" "\
2 matches for \"x\n.x\n\" in buffer:  *test-occur*
      1:ax
       :bx
       :c
      5:ex
       :fx
       :
")
    ;; * Test non-overlapping context lines with matches at bob/eob.
    ("x" 1 "\
ax
b
c
d
ex
f
g
hx
" "\
3 matches for \"x\" in buffer:  *test-occur*
      1:ax
       :b
-------
       :d
      5:ex
       :f
-------
       :g
      8:hx
")
    ;; * Test non-overlapping context lines with matches not at bob/eob.
    ("x" 1 "\
a
bx
c
d
ex
f
" "\
2 matches for \"x\" in buffer:  *test-occur*
       :a
      2:bx
       :c
-------
       :d
      5:ex
       :f
")
    ;; * Test overlapping context lines with matches at bob/eob.
    ("x" 2 "\
ax
bx
c
dx
e
f
gx
h
i
j
kx
" "\
5 matches for \"x\" in buffer:  *test-occur*
      1:ax
      2:bx
       :c
      4:dx
       :e
       :f
      7:gx
       :h
       :i
       :j
     11:kx
")
    ;; * Test overlapping context lines with matches not at bob/eob.
    ("x" 2 "\
a
b
cx
d
e
f
gx
h
i
" "\
2 matches for \"x\" in buffer:  *test-occur*
       :a
       :b
      3:cx
       :d
       :e
       :f
      7:gx
       :h
       :i
")
    ;; * Test overlapping context lines with empty first and last line..
    ("x" 2 "\

b
cx
d
e
f
gx
h

" "\
2 matches for \"x\" in buffer:  *test-occur*
       :
       :b
      3:cx
       :d
       :e
       :f
      7:gx
       :h
       :
")
    ;; * Test multi-line overlapping context lines.
    ("x\n.x" 2 "\
ax
bx
c
d
ex
fx
g
h
i
jx
kx
" "\
3 matches for \"x\n.x\" in buffer:  *test-occur*
      1:ax
       :bx
       :c
       :d
      5:ex
       :fx
       :g
       :h
       :i
     10:jx
       :kx
")
    ;; * Test multi-line non-overlapping context lines.
    ("x\n.x" 2 "\
ax
bx
c
d
e
f
gx
hx
" "\
2 matches for \"x\n.x\" in buffer:  *test-occur*
      1:ax
       :bx
       :c
       :d
-------
       :e
       :f
      7:gx
       :hx
")
    ;; * Test non-overlapping negative (before-context) lines.
    ("x" -2 "\
a
bx
c
d
e
fx
g
h
ix
" "\
3 matches for \"x\" in buffer:  *test-occur*
       :a
      2:bx
-------
       :d
       :e
      6:fx
-------
       :g
       :h
      9:ix
")
    ;; * Test overlapping negative (before-context) lines.
    ("x" -3 "\
a
bx
c
dx
e
f
gx
h
" "\
3 matches for \"x\" in buffer:  *test-occur*
       :a
      2:bx
       :c
      4:dx
       :e
       :f
      7:gx
")

)
  "List of tests for `occur'.
Each element has the format:
\(REGEXP NLINES INPUT-BUFFER-STRING OUTPUT-BUFFER-STRING).")

(defun replace-occur-test-case (test)
  (let ((regexp (nth 0 test))
        (nlines (nth 1 test))
        (input-buffer-string (nth 2 test))
        (temp-buffer (get-buffer-create " *test-occur*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert input-buffer-string)
            (occur regexp nlines)
            (with-current-buffer "*Occur*"
              (buffer-substring-no-properties (point-min) (point-max)))))
      (and (buffer-name temp-buffer)
           (kill-buffer temp-buffer)))))

(defun replace-occur-test-create (n)
  "Create a test for element N of the `replace-occur-tests' constant."
  (let ((testname (intern (format "occur-test-%.2d" n)))
        (testdoc (format "Test element %d of `replace-occur-tests'." n)))
    (eval
     `(ert-deftest ,testname ()
        ,testdoc
        (let (replace-occur-hook)
          (should (equal (replace-occur-test-case (nth ,n replace-occur-tests))
                         (nth 3 (nth ,n replace-occur-tests)))))))))

(dotimes (i (length replace-occur-tests))
  (replace-occur-test-create i))

(ert-deftest replace-occur-revert-bug32543 ()
  "Test `occur-revert' with non-nil `list-matching-lines-jump-to-current-line'."
  (let ((temp-buffer (get-buffer-create " *test-occur*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer temp-buffer
            (erase-buffer)
            (setq list-matching-lines-jump-to-current-line t)
            (insert
";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

")
            (occur "and")
            (with-current-buffer "*Occur*"
              (revert-buffer)
              (goto-char (point-min))
              (should (string-match "\\`2 matches for \"and\" in buffer: "
                                    (buffer-substring-no-properties
                                     (point) (pos-eol)))))))
      (and (buffer-name temp-buffer)
           (kill-buffer temp-buffer)))))

(ert-deftest replace-occur-revert-bug32987 ()
  "Test `occur-revert' with non-nil `list-matching-lines-jump-to-current-line'."
  (let ((temp-buffer (get-buffer-create " *test-occur*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer temp-buffer
            (erase-buffer)
            (setq list-matching-lines-jump-to-current-line nil)
            (insert
";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

")
            (occur "and")
            (with-current-buffer "*Occur*"
              (revert-buffer)
              (goto-char (point-min))
              (should (string-match "\\`2 matches for \"and\" in buffer: "
                                    (buffer-substring-no-properties
                                     (point) (pos-eol)))))))
      (and (buffer-name temp-buffer)
           (kill-buffer temp-buffer)))))


;;; General tests for `query-replace' and `query-replace-regexp'.

(defconst query-replace-tests
  '(
    ;; query-replace
    ("aaa" "M-% a RET 1 RET !" "111")
    ("aaa" "M-% a RET 1 RET y n y" "1a1")
    ;; Empty inputs
    ("aaa" "M-% a RET RET !" "")
    ("aaa" "M-% RET 1 RET !" "1a1a1a")
    ("aaa" "M-% RET RET !" "aaa")
    ;; Reuse the previous default
    ("aaa" "M-% a RET 1 RET . M-% RET !" "111")

    ;; query-replace-regexp
    ("aaa" "C-M-% a* RET 1 RET !" "1")
    ;; Empty inputs
    ("aaa" "C-M-% a* RET RET !" "")
    ("aaa" "C-M-% RET 1 RET !" "1a1a1a")
    ("aaa" "C-M-% RET RET !" "aaa")
    ;; Empty matches
    ("aaa" "C-M-% b* RET 1 RET !" "1a1a1a")
    ;; Complete matches
    ("aaa" "C-M-% .* RET 1 RET !" "1")
    ;; Adjacent non-empty matches
    ("abaab" "C-M-% ab* RET 12 RET !" "121212")
    ;; Adjacent non-empty and empty matches
    ("abab" "C-M-% a* RET 1 RET !" "1b1b")
    ("abab" "C-M-% b* RET 1 RET !" "1a1a1")
    ;; Test case from commit 5632eb272c7
    ("a a a " "C-M-% \\ba SPC RET c RET !" "ccc") ; not "ca c"
    ;; Backup ('^') with calculated replacement ('\,') (bug#79811)
    ("abc" "C-M-% \\([abc]\\) RET [\\1,\\#] RET y n ^ y y" "[a,0][b,1][c,2]")
    ("abc" "C-M-% \\([abc]\\) RET [\\,\\1,\\#] RET y n ^ y y" "[a,0][b,1][c,2]")
    ))

(defun query-replace--run-tests (tests)
  (with-temp-buffer
    (save-window-excursion
      ;; `execute-kbd-macro' is applied to window only
      (set-window-buffer nil (current-buffer))
      (dolist (case tests)
        ;; Ensure empty input means empty string to replace:
        (setq query-replace-defaults nil)
        (delete-region (point-min) (point-max))
        (insert (nth 0 case))
        (goto-char (point-min))
        (execute-kbd-macro (kbd (nth 1 case)))
        (should (equal (buffer-string) (nth 2 case)))))))

(ert-deftest query-replace-tests ()
  (query-replace--run-tests query-replace-tests))

(ert-deftest query-replace-search-function-tests ()
  (let* ((replace-re-search-function #'re-search-forward))
    (query-replace--run-tests query-replace-tests))

  (let* ((pairs '((1 . 2) (3 . 4)))
         (replace-re-search-function
          (lambda (string &optional _bound noerror count)
            (let (found)
              (while (and (not found) pairs)
                (goto-char (caar pairs))
                (when (re-search-forward string (cdar pairs) noerror count)
                  (setq found t))
                (pop pairs))
              found)))
         (tests
          '(
            ;; FIXME: this test should pass after fixing bug#54733:
            ;; ("aaaa" "C-M-% .* RET 1 RET !" "1a1a")
            )))
    (query-replace--run-tests tests)))


;;; General tests for `perform-replace'.

(defconst perform-replace-tests
  '(
    ;; Test case from commit 5632eb272c7
    ("a a a " "\\ba " "c" nil t nil nil nil nil nil nil nil "ccc") ; not "ca c"
    ;; The same with region inside the second match
    ;; FIXME: this test should pass after fixing bug#54733:
    ;; ("a a a " "\\ba " "c" nil t nil nil nil 1 4 nil nil "ca a ")
    ))

(defun perform-replace--run-tests (tests)
  (with-temp-buffer
    (dolist (case tests)
      (delete-region (point-min) (point-max))
      (insert (pop case))
      (goto-char (point-min))
      (apply 'perform-replace (butlast case))
      (should (equal (buffer-string) (car (last case)))))))

(ert-deftest perform-replace-tests ()
  (perform-replace--run-tests perform-replace-tests))


;;; Tests for `query-replace' undo feature.

(defvar replace-tests-bind-read-string nil
  "A string to bind `read-string' and avoid the prompt.")

(defvar replace-tests-perform-replace-regexp-flag t
  "Value for regexp-flag argument passed to `perform-replace' in undo tests.")

(defmacro replace-tests-with-undo (input from to char-nums def-chr &rest body)
  "Helper to test `query-replace' undo feature.
INPUT is a string to insert in a temporary buffer.
FROM is the string to match and replace.
TO is the replacement string.
CHAR-NUMS is a list of elements (CHAR . NUMS), where CHAR is
one of the characters `,', `?\\s', `u', `U', `E' or `q'
and NUMS a list of integers.
DEF-CHAR is the character `?\\s' or `q'.
BODY is a list of forms to evaluate.

Use CHAR-NUMS and DEF-CHAR to temporary bind the function value of
`read-event', thus avoiding the prompt.
For instance, if CHAR-NUMS is the lists ((?\\s . (1 2 3)) (?u . (4))),
then replace 3 matches of FROM with TO, and undo the last replacement.

Return the last evalled form in BODY."
  (declare (indent 5) (debug (stringp stringp stringp form characterp body)))
  (cl-with-gensyms (text count)
    `(let* ((,text ,input)
            (,count 0)
            (inhibit-message t))
       (with-temp-buffer
         (insert ,text)
         (goto-char 1)
         ;; Bind `read-event' to simulate user input.
         ;; If `replace-tests-bind-read-string' is non-nil, then
         ;; bind `read-string' as well.
         (cl-letf (((symbol-function 'read-key)
                    (lambda (&rest _args)
                      (incf ,count)
                      (pcase ,count ; Build the clauses from CHAR-NUMS
                        ,@(append
                           (delq nil
                                 (mapcar
                                  (lambda (chr)
                                    (when-let* ((it (alist-get chr char-nums)))
                                      (if (cdr it)
                                          `(,(cons 'or it) ,chr)
                                        `(,(car it) ,chr))))
                                  '(?, ?\s ?u ?U ?E ?q)))
                           `((_ ,def-chr))))))
                   ((symbol-function 'read-string)
                    (if replace-tests-bind-read-string
                        (lambda (&rest _args) replace-tests-bind-read-string)
                      (symbol-function 'read-string)))
                   ;; Emulate replace-highlight clobbering match-data via
                   ;; isearch-lazy-highlight-new-loop and sit-for (bug#36328)
                   ((symbol-function 'replace-highlight)
                    (lambda (&rest _args)
                      (string-match "[A-Z ]" "ForestGreen")))
                   ;; Override `sit-for' and `ding' so that we don't have
                   ;; to wait and listen to bells when running the test.
                   ((symbol-function 'sit-for)
                    (lambda (&rest _args) (redisplay)))
                   ((symbol-function 'ding) 'ignore))
           (perform-replace ,from ,to t replace-tests-perform-replace-regexp-flag nil))
         ,@body))))

(defun replace-tests--query-replace-undo (&optional comma)
  (let ((input "111"))
    (if comma
        (should
         (replace-tests-with-undo
          input "1" "2" ((?, . (2)) (?u . (3)) (?q . (4))) ?\s (buffer-string)))
      (should
       (replace-tests-with-undo
        input "1" "2" ((?\s . (2)) (?u . (3)) (?q . (4))) ?\s (buffer-string))))))

(ert-deftest query-replace--undo ()
  (should (string= "211" (replace-tests--query-replace-undo)))
  (should (string= "211" (replace-tests--query-replace-undo 'comma))))

(ert-deftest query-replace-undo-bug31073 ()
  "Test for https://debbugs.gnu.org/31073 ."
  (let ((input "aaa aaa"))
    (should
     (replace-tests-with-undo
      input "a" "B" ((?\s . (1 2 3)) (?U . (4))) ?q
      (string= input (buffer-string))))))

(ert-deftest query-replace-undo-bug31492 ()
  "Test for https://debbugs.gnu.org/31492 ."
  (let ((input "a\nb\nc\n"))
    (should
     (replace-tests-with-undo
      input "^\\|\b\\|$" "foo" ((?\s . (1 2)) (?U . (3))) ?q
      (string= input (buffer-string))))))

(ert-deftest query-replace-undo-bug31538 ()
  "Test for https://debbugs.gnu.org/31538 ."
  (let ((input "aaa aaa")
        (replace-tests-bind-read-string "Bfoo"))
    (should
     (replace-tests-with-undo
      input "a" "B" ((?\s . (1 2 3)) (?E . (4)) (?U . (5))) ?q
      (string= input (buffer-string))))))

(ert-deftest query-replace-undo-bug37073 ()
  "Test for https://debbugs.gnu.org/37073 ."
  (let ((input "theorem 1\ntheorem 2\ntheorem 3"))
    (should
     (replace-tests-with-undo
         input "theorem \\([0-9]+\\)"
         '(replace-eval-replacement
           replace-quote
           (format "theorem \\\\ref{theo_%d}" (1+ (string-to-number (match-string 1)))))
         ((?\s . (1 2)) (?U . (3)))
         ?q
       (string= input (buffer-string)))))
  ;; Now run a test with regexp-flag arg in `perform-replace' set to nil
  (let ((input " ^theorem$ 1\n ^theorem$ 2\n ^theorem$ 3")
        (replace-tests-perform-replace-regexp-flag nil)
        (expected " theo 1\n ^theorem$ 2\n ^theorem$ 3"))
    (should
     (replace-tests-with-undo
         input "^theorem$"
         "theo"
         ((?\s . (1 2 4)) (?U . (3)))
         ?q
       (string= expected (buffer-string))))))

(ert-deftest query-replace-undo-bug37287 ()
  "Test for https://debbugs.gnu.org/37287 ."
  (let ((input "foo-1\nfoo-2\nfoo-3")
        (expected "foo-2\nfoo-2\nfoo-3"))
    (should
     (replace-tests-with-undo
      input "\\([0-9]\\)"
      '(replace-eval-replacement
        replace-quote
        (format "%d" (1+ (string-to-number (match-string 1)))))
      ((?\s . (1 2 4)) (?U . (3)))
      ?q
      (string= expected (buffer-string))))))

(defmacro replace-tests-with-highlighted-occurrence (highlight-locus &rest body)
  "Helper macro to test the highlight of matches when navigating occur buffer.

Eval BODY with `next-error-highlight' and `next-error-highlight-no-select'
bound to HIGHLIGHT-LOCUS."
  (declare (indent 1) (debug (form body)))
  `(let ((regexp "foo")
         (next-error-highlight ,highlight-locus)
         (next-error-highlight-no-select ,highlight-locus)
         (buffer (generate-new-buffer "test"))
         (inhibit-message t))
     (unwind-protect
         ;; Local bind to disable the deletion of `occur-highlight-overlay'
         (cl-letf (((symbol-function 'occur-goto-locus-delete-o) (lambda ())))
           (with-current-buffer buffer (dotimes (_ 3) (insert regexp ?\n)))
           (pop-to-buffer buffer)
           (occur regexp)
           (pop-to-buffer "*Occur*")
           (occur-next)
           ,@body)
       (kill-buffer buffer)
       (kill-buffer "*Occur*"))))

(ert-deftest occur-highlight-occurrence ()
  "Test for https://debbugs.gnu.org/39121 ."
  (let ((alist '((nil . nil) (0.5 . t) (t . t) (fringe-arrow . nil)))
        (check-overlays
         (lambda (has-ov)
           (eq has-ov (not (null (overlays-in (point-min) (point-max))))))))
    (pcase-dolist (`(,highlight-locus . ,has-overlay) alist)
      ;; Visiting occurrences
      (replace-tests-with-highlighted-occurrence highlight-locus
        (occur-mode-goto-occurrence)
        (should (funcall check-overlays has-overlay)))
      ;; Displaying occurrences
      (replace-tests-with-highlighted-occurrence highlight-locus
        (occur-mode-display-occurrence)
        (with-current-buffer (marker-buffer
                              (caar (get-text-property (point) 'occur-target)))
          (should (funcall check-overlays has-overlay)))))))

(ert-deftest replace-regexp-bug45973 ()
  "Test for https://debbugs.gnu.org/45973 ."
  (let ((before "1RB 1LC 1RC 1RB 1RD 0LE 1LA 1LD 1RH 0LA")
        (after  "1LB 1RC 1LC 1LB 1LD 0RE 1RA 1RD 1LH 0RA"))
    (with-temp-buffer
      (insert before)
      (goto-char (point-min))
      (with-suppressed-warnings ((interactive-only replace-regexp))
        (replace-regexp
         "\\(\\(L\\)\\|\\(R\\)\\)"
         '(replace-eval-replacement
           replace-quote
           (if (match-string 2) "R" "L"))))
      (should (equal (buffer-string) after)))))

(ert-deftest test-count-matches ()
  (with-temp-buffer
    (insert "oooooooooo")
    (goto-char (point-min))
    (should (= (count-matches "oo") 5))
    (should (= (count-matches "o+") 1)))
  (with-temp-buffer
    (insert "o\n\n\n\no\n\n")
    (goto-char (point-min))
    (should (= (count-matches "^$") 4))))

;;; replace-tests.el ends here
