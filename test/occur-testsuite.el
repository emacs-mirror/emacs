;;; occur-testsuite.el --- Test suite for occur.

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@jurta.org>
;; Keywords: matching, internal

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Type M-x test-occur RET to test the functionality of `occur'.

;;; Code:

(defconst occur-tests
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
5 matches for \"x\" in buffer:  *temp*
      1:xa
      3:cx
      4:xd
      5:xex
      6:fx
")
    ;; * Test multi-line matches, this is the first test from
    ;; http://lists.gnu.org/archive/html/emacs-devel/2005-06/msg01008.html
    ;; where numbers are replaced with letters.
    ("a\na" 0 "\
a
a
a
a
a
" "\
2 matches for \"a^Ja\" in buffer:  *temp*
      1:a
       :a
      3:a
       :a
")
    ;; * Test multi-line matches, this is the second test from
    ;; http://lists.gnu.org/archive/html/emacs-devel/2005-06/msg01008.html
    ;; where numbers are replaced with letters.
    ("a\nb" 0 "\
a
b
c
a
b
" "\
2 matches for \"a^Jb\" in buffer:  *temp*
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
2 matches for \"a^J\" in buffer:  *temp*
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
2 matches for \"x^J.x^J\" in buffer:  *temp*
      1:ax
       :bx
       :c
      5:ex
       :fx
       :
")
    )
  "List of tests for `occur'.
Each element has the format:
\(REGEXP NLINES INPUT-BUFFER-STRING OUTPUT-BUFFER-STRING).")

(defun test-occur ()
  (interactive)
  (let ((count 1)
        failed
        (occur-hook nil))
    (dolist (test occur-tests)
      (let ((regexp (nth 0 test))
            (nlines (nth 1 test))
            (input-buffer-string (nth 2 test))
            (output-buffer-string (nth 3 test)))
        (save-excursion
          (with-temp-buffer
            (insert input-buffer-string)
            (occur regexp nlines)
            (unless (equal output-buffer-string
                           (with-current-buffer "*Occur*"
                             (buffer-string)))
              (setq failed (cons count failed))))))
      (setq count (1+ count)))
    (if failed
        (message "FAILED TESTS: %S" (reverse failed))
      (message "SUCCESS"))))

(provide 'occur-testsuite)

;;; occur-testsuite.el ends here
