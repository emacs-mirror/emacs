;;; replace-tests.el --- tests for replace.el.

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

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

(defun replace-tests--query-replace-undo (&optional comma)
  (with-temp-buffer
    (insert "111")
    (goto-char 1)
    (let ((count 0))
      ;; Don't wait for user input.
      (cl-letf (((symbol-function 'read-event)
                 (lambda (&rest args)
                   (cl-incf count)
                   (let ((val (pcase count
                                ('2 (if comma ?, ?\s)) ; replace and: ',' no move; '\s' go next
                                ('3 ?u) ; undo
                                ('4 ?q) ; exit
                                (_ ?\s)))) ; replace current and go next
                     val))))
        (perform-replace "1" "2" t nil nil)))
    (buffer-string)))

(ert-deftest query-replace--undo ()
  (should (string= "211" (replace-tests--query-replace-undo)))
  (should (string= "211" (replace-tests--query-replace-undo 'comma))))

;;; replace-tests.el ends here
