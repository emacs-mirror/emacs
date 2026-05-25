;;; visual-wrap-test.el --- Manual tests for visual-wrap-prefix-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;; Manual test suite for `visual-wrap-prefix-mode'.  Each test opens a
;; buffer named *visual-wrap-test-NNN* with an explanatory banner at
;; the top, followed by sample lines.  The banner describes what to
;; look for; the code below carries no parallel documentation.
;;
;; Run from `emacs -Q':
;;
;;   emacs -Q -l test/manual/visual-wrap-test.el \
;;             --eval "(visual-wrap-test-001)"
;;
;; Append `-nw' to the same invocation to repeat each test in a TTY
;; frame.  `string-pixel-width' adapts to the frame, so GUI and TTY
;; runs share the same expectations modulo test 004 (variable-pitch),
;; which degrades silently on a TTY.
;;
;; Tests:
;;   001   Visible fixed-pitch prefix (baseline / regression check).
;;   002   Fully invisible prefix (the original bug).
;;   003   Partially invisible prefix.
;;   004a  Variable-pitch narrow prefix `;;; ' (GUI only).
;;   004b  Variable-pitch wide prefix   `%%% ' (GUI only).
;;   005   Non-zero `visual-wrap-extra-indent'.
;;   006   markdown-ts-mode + `markdown-ts-hide-markup' (real-world repro).
;;   007   org-table-style `|' prefix (regression check for bug#73882).

;;; Code:

(defconst visual-wrap-test--long
  "The quick brown fox jumps over the lazy dog, repeatedly, with great enthusiasm, again and again, until the moon comes up and the cows come home, and then some more for good measure."
  "A line long enough to overflow any reasonable window.")

(defun visual-wrap-test--prepare (name)
  "Create or reset buffer NAME, plain `text-mode', return it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (kill-all-local-variables)
      (text-mode))
    buf))

(defun visual-wrap-test--show (buf)
  "Enable `visual-wrap-prefix-mode' in BUF and switch to it."
  (with-current-buffer buf
    (goto-char (point-min))
    (visual-wrap-prefix-mode 1))
  (switch-to-buffer buf))

(defun visual-wrap-test--insert-invisible (text)
  "Insert TEXT and mark its character range invisible via text properties.
Uses `invisible t', which is matched by the default
`buffer-invisibility-spec'."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'invisible t)))

(defun visual-wrap-test-001 ()
  "Baseline: visible fixed-pitch prefix.  See banner in the test buffer."
  (interactive)
  (let ((buf (visual-wrap-test--prepare "*visual-wrap-test-001*")))
    (with-current-buffer buf
      (insert "\
visual-wrap-test-001 — visible fixed-pitch prefix (baseline)
============================================================

Mode: `text-mode'.  The paragraph below starts with `> ', which
the default `adaptive-fill-regexp' matches as a paragraph prefix.

`visual-wrap-prefix-mode' is enabled in this buffer.  Narrow the
window until the long paragraph wraps onto several visual lines.

Expected:
  * Line 1 is not shifted; `> ' renders at its natural width.
  * Continuation visual lines align horizontally with the first
    character that follows `> ' on line 1.

This case worked correctly before the patch (bug#81039).  The test confirms
the redesign (bug#81039) has not broken the common fixed-pitch fixed-width
case while fixing the invisible-prefix and variable-pitch cases.

Sample line:

> ")
      (insert visual-wrap-test--long "\n"))
    (visual-wrap-test--show buf)))

(defun visual-wrap-test-002 ()
  "Fully invisible prefix.  See banner in the test buffer."
  (interactive)
  (let ((buf (visual-wrap-test--prepare "*visual-wrap-test-002*")))
    (with-current-buffer buf
      (insert "\
visual-wrap-test-002 — fully invisible prefix
=============================================

Mode: `text-mode'.  The paragraph below starts with `### ', and
all four of those characters carry `invisible t' as a text
property.  The default `buffer-invisibility-spec' includes t, so
the display engine renders them at zero pixels.

The original bug reported in bug#81039: `visual-wrap--content-prefix'
used `string-width' to derive a column count, which ignores
invisibility.  It therefore reserved four columns of `min-width' on
line 1 and shifted the visible content rightward.

`visual-wrap-prefix-mode' is enabled.  Narrow the window so the
paragraph wraps.

Expected with the redesign (bug#81039):
  * Line 1 is NOT shifted; the visible content starts at column 0
    (the `### ' has zero rendered width).
  * Continuation visual lines also start at column 0, since the
    natural pixel width of the prefix is zero.

Sample line:

")
      (visual-wrap-test--insert-invisible "### ")
      (insert visual-wrap-test--long "\n"))
    (visual-wrap-test--show buf)))

(defun visual-wrap-test-003 ()
  "Partially invisible prefix.  See banner in the test buffer."
  (interactive)
  (let ((buf (visual-wrap-test--prepare "*visual-wrap-test-003*")))
    (with-current-buffer buf
      (insert "\
visual-wrap-test-003 — partially invisible prefix
=================================================

Mode: `text-mode'.  The paragraph below begins with `### '
\(four characters: three hashes and a space).  The first two
hashes carry `invisible t'; the third hash and the space remain
visible.  The visible portion of the prefix is therefore `# ',
two columns wide.

`visual-wrap-prefix-mode' is enabled.  Narrow the window so the
paragraph wraps.

Expected with the redesign (bug#81039):
  * Line 1 shows `# ' at column 0, followed by the paragraph
    text — no extra padding to compensate for the hidden hashes.
  * Continuation visual lines align with the first character
    after the visible `# ' on line 1 (i.e. two columns in).

If line 1's content begins past column 2, or continuations land
elsewhere than two columns in, the natural-width computation is
not honoring per-character invisibility.

Sample line:

")
      (visual-wrap-test--insert-invisible "##")
      (insert "# ")
      (insert visual-wrap-test--long "\n"))
    (visual-wrap-test--show buf)))

(defun visual-wrap-test--variable-pitch (name prefix narrow-or-wide)
  "Set up a variable-pitch test buffer named NAME with PREFIX.
NARROW-OR-WIDE is the string \"narrow\" or \"wide\", used only in
the banner."
  (let ((buf (visual-wrap-test--prepare name)))
    (with-current-buffer buf
      (when (display-graphic-p)
        (variable-pitch-mode 1))
      (unless (display-graphic-p)
        (insert "\
NOTE: this Emacs frame is a TTY.  `variable-pitch-mode' has no
effect; every glyph is exactly one column wide.  The test below
therefore degenerates to a visible fixed-pitch prefix (similar to
test 001).  Re-run inside a GUI frame to actually test the
variable-pitch path.

"))
      (insert (format "\
visual-wrap-test — variable-pitch %s prefix `%s'
=================================================

Mode: `text-mode' + `variable-pitch-mode' (GUI only).  The
paragraph below starts with `%s', whose natural pixel width in
a proportional font is %s than the same number of monospace
columns.

This is the case Jim Porter's 2024 commit was designed to handle:
under the old `(max string-width (ceiling pixel/avg-space))'
formula, the column-rounded `min-width' on line 1 over-padded the
prefix.  Under the redesign (bug#81039), the continuation `wrap-prefix' uses
the prefix's pixel width directly, so no rounding occurs.

`visual-wrap-prefix-mode' is enabled.  Narrow the window so the
paragraph wraps.

Expected with the redesign (bug#81039):
  * Line 1 renders `%s' at its natural pixel width.
  * Continuation visual lines align with the first character that
    follows `%s' on line 1, in pixels — no visible jitter
    between line 1 and the wrapped lines.

To compare against the pre-bug#81039 behavior, re-run this test
without `--load'ing the patched `visual-wrap.el' (bug#81039) (i.e. let
the built-in version handle the buffer).  You should see a small
but real horizontal gap between the prefix end on line 1 and the
start of continuation lines.

Sample line:

%s" narrow-or-wide prefix prefix narrow-or-wide prefix prefix prefix))
      (insert visual-wrap-test--long "\n\n"
              (format "\

=== Appendix: artifact in banner text (out of scope of bug#81039) ===

On close inspection in GUI, the bullet-prefix line

  * Continuation...

above sits a few pixels right of its follow-on buffer lines that start
with four spaces

    follows `%s' on...

This is an artifact due to the specific content in the banner.  Because
the prefix width is computed for each physical line separately, in the
case of a variable-pitch font we end up having slightly different widths
(in this example, the differences are barely visible by human eye).  In
fact, ` * ' and `    ' are separate adaptive-fill prefixes on independent
buffer lines, and `visual-wrap.el' processes them independently: it has
no notion of \"these lines belong to one logical bullet\".  Under the
old code the step was an estimated ~17 pixels (column-rounded
`min-width' on ` * ' line, no processing on `    ' lines).  The
redesign (bug#81039) drops it to an estimated ~2 pixels (natural pixel
width of ` * ' is slightly larger than that of `    ' in a proportional
font), small enough to look aligned.
" prefix)))
    (visual-wrap-test--show buf)))

(defun visual-wrap-test-004a ()
  "Variable-pitch narrow prefix `;;; '.  See banner in the test buffer."
  (interactive)
  (visual-wrap-test--variable-pitch
   "*visual-wrap-test-004a*" ";;; " "narrower"))

(defun visual-wrap-test-004b ()
  "Variable-pitch wide prefix `%%% '.  See banner in the test buffer."
  (interactive)
  (visual-wrap-test--variable-pitch
   "*visual-wrap-test-004b*" "%%% " "wider"))

(defun visual-wrap-test-005 ()
  "Non-zero `visual-wrap-extra-indent'.  See banner in the test buffer."
  (interactive)
  (let ((buf (visual-wrap-test--prepare "*visual-wrap-test-005*")))
    (with-current-buffer buf
      (setq-local visual-wrap-extra-indent 4)
      (insert "\
visual-wrap-test-005 — non-zero `visual-wrap-extra-indent'
=========================================================

Mode: `text-mode'.  `visual-wrap-extra-indent' is set buffer-local
to 4.

`visual-wrap--adjust-prefix' must now convert four canonical-char
columns to pixels before adding them to the prefix's pixel width
(since `visual-wrap--content-prefix' returns a pixel count under
the redesign (bug#81039)).

`visual-wrap-prefix-mode' is enabled.  Narrow the window so the
paragraph wraps.

Expected:
  * Continuation visual lines start four canonical-character
    columns to the right of the `> ' prefix on line 1.
  * Line 1 itself is not shifted.

If continuations land at zero columns past the prefix end, the
column-to-pixel conversion in `visual-wrap--adjust-prefix' is not
firing.  If they land somewhere fractional or wrong, the unit
conversion is wrong.

Sample line:

> ")
      (insert visual-wrap-test--long "\n"))
    (visual-wrap-test--show buf)))

(defun visual-wrap-test-006 ()
  "markdown-ts-mode + `markdown-ts-hide-markup'.  See banner in the test buffer."
  (interactive)
  (let ((have-mode (fboundp 'markdown-ts-mode))
        (buf (get-buffer-create "*visual-wrap-test-006*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (kill-all-local-variables)
      (unless have-mode
        (insert "\
WARNING: this Emacs build does not expose `markdown-ts-mode`.
Test 006 cannot run.  Use Emacs 31 or newer.

"))
      (insert "\
visual-wrap-test-006 — markdown-ts-mode + hide-markup (real-world repro)
=======================================================================

Mode: `markdown-ts-mode` with `markdown-ts-hide-markup` enabled.  This
is the case that originally exposed the bug reported in bug#81039.  The
ATX heading marker `### ` carries `invisible markdown-ts--markup`, which
is in `buffer-invisibility-spec` while hide-markup is on.

Default `adaptive-fill-regexp` matches `### ` as a paragraph
prefix.  Under the old code, hidden hashes were still counted by
`string-width`, so the heading text shifted right by four columns
the moment `visual-wrap-prefix-mode` came on — visible even
without any wrapping happening.

Two cases are demonstrated below: a short heading (no wrap
needed, but the shift was visible) and a long heading (wraps,
and the continuation must align with the visible heading text).

Expected with the redesign (bug#81039):
* Short heading: not shifted; reads as `A short heading`.
* Long heading: line 1 not shifted; continuation visual lines
  align with the start of the visible heading text.

To compare, revert the patch (bug#81039), restart, and re-run; you
should see the heading text on line 1 shift right by four columns.

Case 1 — short heading (no wrap; the shift was visible without
wrapping under the old code):

### A short heading

Case 2 — long heading (wraps; continuation visual lines must
align with the start of the visible heading text):

### ")
      (insert visual-wrap-test--long "\n")
      (when have-mode
        (markdown-ts-mode)
        ;; Enable hide-markup directly.  `markdown-ts-toggle-hide-markup'
        ;; is not autoloaded, so it is not yet bound at the time the
        ;; enclosing `let' captures `fboundp' on entry.
        (setq markdown-ts-hide-markup t)
        (add-to-invisibility-spec 'markdown-ts--markup)
        (font-lock-flush))
      (goto-char (point-min))
      (visual-wrap-prefix-mode 1))
    (switch-to-buffer buf)))

(defun visual-wrap-test-007 ()
  "Org-table-style `|' prefix.  See banner in the test buffer."
  (interactive)
  (let ((buf (visual-wrap-test--prepare "*visual-wrap-test-007*")))
    (with-current-buffer buf
      (insert "\
visual-wrap-test-007 — org-table-style `|' prefix (bug#73882 regression)
========================================================================

Mode: `text-mode'.  The buffer below contains a pre-aligned org-style
table.  `|' is in the default `adaptive-fill-regexp', so each table
row is treated as a logical line with `| ' as its first-line prefix.

Original bug: with `global-visual-wrap-prefix-mode' enabled, the table
cells in the first column got misaligned because `min-width' from a
prior fontification of the same `|' character accumulated on each
pass, inflating the width past one space.  Reporter:
Arthur Elsenaar, 2024-10-19.  Fixed by Jim Porter as 81a5beb8af0
\(strip prior `min-width' before measuring the prefix).

The redesign (bug#81039) supersedes that fix at a lower level: no
`min-width' display property is installed at all, so there is nothing
that can accumulate across fontification passes.

`visual-wrap-prefix-mode' is enabled in this buffer.

Expected:
  * The table cells stay aligned.  Each `|' character in every column
    sits at the same horizontal position from row to row.
  * No `min-width' property appears anywhere on the table text.

To compare against the pre-Jim-Porter behavior, you would need to
revert his commit and ours; this is purely a regression check today.

You may also want to run `M-x org-mode' in this buffer and verify
that the table remains properly aligned.

Sample table:

| head   | 1 | 2 | 3 | 4 |
|--------+---+---+---+---|
| apple  |   |   |   |   |
| orange |   |   |   |   |
| pear   |   |   |   |   |
| banana |   |   |   |   |
"))
    (visual-wrap-test--show buf)))

(provide 'visual-wrap-test)

;;; visual-wrap-test.el ends here
