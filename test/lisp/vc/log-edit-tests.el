;;; log-edit-tests.el --- Unit tests for log-edit.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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

;; Unit tests for lisp/vc/log-edit.el.

;;; Code:

(require 'log-edit)
(require 'ert)

(ert-deftest log-edit-fill-entry ()
  (with-temp-buffer
    (insert "\
* dir/file.ext (fun1):
\(fun2):
\(fun3):
* file2.txt (fun4):
\(fun5):
\(fun6):
\(fun7): Some prose.
\(fun8): A longer description of a complicated change.\
  Spread over a couple of sentences.\
  Long enough to be filled for several lines.
\(fun9): Etc.")
    (goto-char (point-min))
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1, fun2, fun3):
* file2.txt (fun4, fun5, fun6, fun7): Some prose.
\(fun8): A longer description of a complicated change.  Spread over a
couple of sentences.  Long enough to be filled for several lines.
\(fun9): Etc."))
    (let ((fill-column 20)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1)
\(fun2, fun3):
* file2.txt (fun4)
\(fun5, fun6, fun7):
Some prose.
\(fun8): A longer
description of a
complicated change.
Spread over a couple
of sentences.  Long
enough to be filled
for several lines.
\(fun9): Etc."))
    (let ((fill-column 40)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1, fun2, fun3):
* file2.txt (fun4, fun5, fun6, fun7):
Some prose.
\(fun8): A longer description of a
complicated change.  Spread over a
couple of sentences.  Long enough to be
filled for several lines.
\(fun9): Etc."))))

(ert-deftest log-edit-fill-entry-indented-func-entries ()
  ;; Indenting function entries is a typical mistake caused by using a
  ;; misconfigured or non-ChangeLog specific fill function.
  (with-temp-buffer
    (insert "\
* dir/file.ext (fun1):
  (fun2):
  (fun3):
* file2.txt (fun4):
  (fun5):
  (fun6):
  (fun7): Some prose.
  (fun8): A longer description of a complicated change.\
  Spread over a couple of sentences.\
  Long enough to be filled for several lines.
  (fun9): Etc.")
    (goto-char (point-min))
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1, fun2, fun3):
* file2.txt (fun4, fun5, fun6, fun7): Some prose.
\(fun8): A longer description of a complicated change.  Spread over a
couple of sentences.  Long enough to be filled for several lines.
\(fun9): Etc."))))

(ert-deftest log-edit-fill-entry-trailing-prose ()
  (with-temp-buffer
    (insert "\
* dir/file.ext (fun1): A longer description of a complicated change.\
  Spread over a couple of sentences.\
  Long enough to be filled for several lines.")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* dir/file.ext (fun1): A longer description of a complicated change.
Spread over a couple of sentences.  Long enough to be filled for several
lines."))))

(ert-deftest log-edit-fill-entry-joining ()
  ;; Join short enough function names on the same line.
  (with-temp-buffer
    (insert "* dir/file.ext (fun1):\n(fun2):")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "* dir/file.ext (fun1, fun2):")))
  ;; Don't combine them if they're too long.
  (with-temp-buffer
    (insert "* dir/long-file-name.ext (a-really-long-function-name):
\(another-very-long-function-name):")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "* dir/long-file-name.ext (a-really-long-function-name)
\(another-very-long-function-name):")))
  ;; Put function name on next line, if the file name is too long.
  (with-temp-buffer
    (insert "\
* a-very-long-directory-name/another-long-directory-name/and-a-long-file-name.ext\
 (a-really-long-function-name):")
    (let ((fill-column 72)) (log-edit-fill-entry))
    (should (equal (buffer-string) "\
* a-very-long-directory-name/another-long-directory-name/and-a-long-file-name.ext
\(a-really-long-function-name):"))))

(ert-deftest log-edit-fill-entry-confinement ()
  (let (string string1 string2 string3 string4)
    (setq string
          ;; This entry is precisely 65 columns in length;
          ;; log-edit-fill-column should leave it unmodified.
          "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10, fun1134):"
          string1
          ;; This entry is 66 columns in length, and must be filled.
          "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10, fun11345):"
          string2
          ;; The first line of this entry totals 65 columns in length,
          ;; and should be preserved intact.
          "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10, fun11345)
(fun11356):"
          string3
          ;; The first defun in this entry is a file name that brings
          ;; the total to 40 columns in length and should be preserved
          ;; intact.
          "* file2.txt (abcdefghijklmnopqrstuvwxyz)
(ABC):"
          string4
          ;; The first defun brings that total to 41, and should be
          ;; placed on the next line.
          "* file2.txt (abcdefghijklmnopqrstuvwxyz):")
    (with-temp-buffer
      (insert string)
      (let ((fill-column 64)) (log-edit-fill-entry))
      (should (equal (buffer-string) string))
      (erase-buffer)
      (insert string1)
      (let ((fill-column 64)) (log-edit-fill-entry))
      (should (equal (buffer-string)
                     "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10)
(fun11345):"))
      (erase-buffer)
      (insert string2)
      (let ((fill-column 64)) (log-edit-fill-entry))
      (should (equal (buffer-string) string2))
      (erase-buffer)
      (insert string3)
      (let ((fill-column 39)) (log-edit-fill-entry))
      (should (equal (buffer-string) string3))
      (erase-buffer)
      (insert string4)
      (let ((fill-column 39)) (log-edit-fill-entry))
      (should (equal (buffer-string)
                     "* file2.txt\s
(abcdefghijklmnopqrstuvwxyz):")))))

(ert-deftest log-edit-fill-entry-space-substitution ()
  ;; This test verifies that filling the paragraph surrounding the
  ;; last line of defuns does not break between defun lists with
  ;; spaces in identifiers.
  (let (string wanted)
    (setq string "
* src/sfnt.c (xmalloc, xrealloc): Improve behavior upon allocation
failures during test.
(sfnt_table_names): Add prep.
(sfnt_transform_coordinates): Allow applying offsets during
coordinate transform.
(sfnt_decompose_compound_glyph): Defer offset computation until
any component compound glyph is loaded, then apply it during the
transform process.
(sfnt_multiply_divide): Make available everywhere.  Implement on
64 bit systems.
(sfnt_multiply_divide_signed): New function.
(sfnt_mul_fixed): Fix division overflow.
(sfnt_curve_to_and_build_1, sfnt_build_glyph_outline): Remove
outdated comment.
(sfnt_build_outline_edges): Fix coding style.
(sfnt_lookup_glyph_metrics): Allow looking up metrics without
scaling.
(struct sfnt_cvt_table): Fix type of cvt values.
(struct sfnt_prep_table): New structure.
(sfnt_read_cvt_table): Read cvt values in terms of fwords, not
longs (as Apple's doc seems to say).
(sfnt_read_fpgm_table): Fix memory allocation for font program
table.
(sfnt_read_prep_table): New function.
(struct sfnt_interpreter_zone): New structure.
(struct sfnt_interpreter_graphics_state): New fields `project',
`move', `vector_dot_product'.  Rename to `sfnt_graphics_state'.
(struct sfnt_interpreter, sfnt_mul_f26dot6): Stop doing rounding
division.
(sfnt_init_graphics_state, sfnt_make_interpreter, MOVE, SSW, RAW)
(SDS, ADD, SUB, ABS, NEG, WCVTF, _MIN, S45ROUND, SVTCAx)
(sfnt_set_srounding_state, sfnt_skip_code)
(sfnt_interpret_unimplemented, sfnt_interpret_fdef)
(sfnt_interpret_idef, sfnt_interpret_if, sfnt_interpret_else)
(sfnt_round_none, sfnt_round_to_grid, sfnt_round_to_double_grid)
"
          wanted "
* src/sfnt.c\s
(xmalloc, xrealloc):
Improve behavior
upon allocation
failures during
test.
(sfnt_table_names):
Add prep.
(sfnt_transform_coordinates):
Allow applying
offsets during
coordinate
transform.
(sfnt_decompose_compound_glyph):
Defer offset
computation until
any component
compound glyph is
loaded, then apply
it during the
transform process.
(sfnt_multiply_divide):
Make available
everywhere.
Implement on 64 bit
systems.
(sfnt_multiply_divide_signed):
New function.
(sfnt_mul_fixed):
Fix division
overflow.
(sfnt_curve_to_and_build_1)
(sfnt_build_glyph_outline):
Remove outdated
comment.
(sfnt_build_outline_edges):
Fix coding style.
(sfnt_lookup_glyph_metrics):
Allow looking up
metrics without
scaling.
(struct sfnt_cvt_table):
Fix type of cvt
values.
(struct sfnt_prep_table):
New structure.
(sfnt_read_cvt_table):
Read cvt values in
terms of fwords, not
longs (as Apple's
doc seems to say).
(sfnt_read_fpgm_table):
Fix memory
allocation for font
program table.
(sfnt_read_prep_table):
New function.
(struct sfnt_interpreter_zone):
New structure.
(struct sfnt_interpreter_graphics_state):
New fields
`project', `move',
`vector_dot_product'.
Rename to
`sfnt_graphics_state'.
(struct sfnt_interpreter)
(sfnt_mul_f26dot6):
Stop doing rounding
division.
(sfnt_init_graphics_state)
(sfnt_make_interpreter)
(MOVE, SSW, RAW, SDS)
(ADD, SUB, ABS, NEG)
(WCVTF, _MIN)
(S45ROUND, SVTCAx)
(sfnt_set_srounding_state)
(sfnt_skip_code)
(sfnt_interpret_unimplemented)
(sfnt_interpret_fdef)
(sfnt_interpret_idef)
(sfnt_interpret_if)
(sfnt_interpret_else)
(sfnt_round_none)
(sfnt_round_to_grid)
(sfnt_round_to_double_grid):
")
    (with-temp-buffer
      (insert string)
      (let ((fill-column 20)) (log-edit-fill-entry))
      (should (equal (buffer-string) wanted)))))

(ert-deftest log-edit-fill-entry-initial-wrapping ()
  ;; This test verifies that a newline is inserted before a defun
  ;; itself longer than the fill column when such a defun is being
  ;; inserted after a file name, and not otherwise.
  (let (string wanted)
    (setq string "
* src/sfnt.c (long_entry_1): This entry should be placed on a
new line.
(but_this_entry_should_not): With the prose displaced to the
next line instead."
          wanted "
* src/sfnt.c\s
(long_entry_1): This
entry should be
placed on a new
line.
(but_this_entry_should_not):
With the prose
displaced to the
next line instead.")
    (with-temp-buffer
      (insert string)
      (let ((fill-column 20)) (log-edit-fill-entry))
      (should (equal (buffer-string) wanted)))))

(ert-deftest log-edit-fill-entry-no-defun-list-wrapping ()
  ;; This test verifies that the opening defun list of an entry is never
  ;; broken, even in the event its length in total exceeds the fill
  ;; column.
  (let (string wanted)
    (setq string "
* src/androidfns.c (Fxw_display_color_p):
(Fx_display_grayscale_p): Report color and/or grayscale properly.
"
          wanted "
* src/androidfns.c (Fxw_display_color_p, Fx_display_grayscale_p):
Report color and/or grayscale properly.
")
    (with-temp-buffer
      (insert string)
      (let ((fill-column 64)) (log-edit-fill-entry))
      (should (equal (buffer-string) wanted)))))

(defun log-edit-done-strip-cvs-lines-helper (initial-text wanted vc-backend)
  "Helper function for the log-edit-done-strip-cvs-lines tests.
Tests that running log-edit-done-strip-cvs-lines as a log-edit-done-hook
produces the WANTED string when run on INITIAL-TEXT with
\\='log-edit-vc-backend' set to VC-BACKEND.\""
  (with-temp-buffer
      (let ((log-edit-done-hook 'log-edit-done-strip-cvs-lines)
            (log-edit-vc-backend vc-backend))
        (setq-local log-edit-callback #'(lambda () (interactive) nil))
        (insert initial-text)
        (log-edit-done)
        (should (equal (buffer-string) wanted)))))

(ert-deftest log-edit-done-strip-cvs-lines-cvs ()
  "Strip lines beginning with \"CVS:\" when using CVS as VC backend."
  (let (string wanted)
    (setq string "summary line
first line
CVS: Please evaluate your changes and consider the following.
CVS: Abort checkin if you answer no.
"
          wanted "summary line
first line
")
    (log-edit-done-strip-cvs-lines-helper string wanted 'CVS)))

(ert-deftest log-edit-done-strip-cvs-lines-non-cvs ()
  "Do not strip lines beginning with \"CVS:\" when not using CVS as VC backend."
  (let (string)
    (setq string "summary line
first line
CVS: Please evaluate your changes and consider the following.
CVS: Abort checkin if you answer no.
")
  (log-edit-done-strip-cvs-lines-helper string string nil)))

(ert-deftest log-edit-done-strip-cvs-lines-only-cvs-colon-blank ()
  "Strip lines that contain solely \"CVS: \" when using CVS as VC backend."
  (let (string wanted)
    (setq string "CVS: \n"
          wanted "")
    (log-edit-done-strip-cvs-lines-helper string wanted 'CVS)))

(ert-deftest log-edit-done-strip-cvs-lines-only-cvs-colon ()
  "Strip lines that contain solely \"CVS:\" when using CVS as VC backend."
  ;; This test verifies that lines consisting only of "CVS:" (no blank
  ;; after the colon) are stripped from the commit message.
  ;; CVS does this to accommodate editors that delete trailing whitespace.
  (let (string wanted)
    (setq string "CVS:\n"
          wanted "")
    (log-edit-done-strip-cvs-lines-helper string wanted 'CVS)))

;;; log-edit-tests.el ends here
