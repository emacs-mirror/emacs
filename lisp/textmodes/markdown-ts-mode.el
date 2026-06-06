;;; markdown-ts-mode.el --- tree sitter support for Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author           : Rahul Martim Juliato <rahul.juliato@gmail.com>
;;                  : Stéphane Marks <shipmints@gmail.com>
;; Maintainer       : Rahul Martim Juliato <rahul.juliato@gmail.com>
;; Created          : April 2024
;; Version          : 1.0
;; Package-Requires : ((emacs "31.1"))
;; Keywords         : markdown md text edit languages tree-sitter

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

;;;; Tree-sitter Language Versions
;;
;; markdown-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-markdown: v0.4.1
;; - tree-sitter-markdown-inline: v0.4.1
;;
;; We try our best to make built-in modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work.  Send
;; us a bug report if it doesn't.
;;
;; Bidirectional Text Considerations
;;
;; Text with markup may need an extra newline before bidirectional text
;; for it to show correctly.  This is a limitation in the Emacs display
;; engine.
;;
;; Code Block Language Mode Considerations
;;
;; Fenced code block language modes are derived from the table
;; `markdown-ts-code-block-modes' and heuristics adding "-ts-mode" and
;; "-mode" to the language name.  If your language's mode is not
;; properly recognized, add it to `markdown-ts-code-block-modes', which
;; see.
;;
;; Each language's major mode is enabled once per `markdown-ts-mode'
;; buffer in a temporary buffer to extract its default font-lock and
;; tree-sitter settings.  In conventional non-treesit code blocks, the
;; major mode is enabled each time the block is fontified.
;;
;; NOTE: Major mode hooks are not run and font-lock, treesit, indent,
;; comment recognition, etc. customizations that you might have in your
;; hooks are not applied.  As a result, your code blocks might appear
;; different in `markdown-ts-mode' mode than in native major mode
;; buffers.  Not running hooks avoids the cost of each mode hook and
;; avoids potential recursive treesit callback issues.
;;
;;;; Code Block Commands
;;
;; Some common commands will operate within a code block and in its
;; mode.  These operate in an indirect buffer and some commands may
;; operate slightly differently to the same commands invoked in a native
;; mode buffer.  `indent-for-tab-command' may invoke
;; `completion-at-point' under the covers and may return results from
;; the surrounding Markdown buffer instead of the code block's context.
;; You can invoke `completion-at-point' directly by using its key
;; binding, `C-M-i`.
;;
;;;; Pipe Tables
;;
;; These are a GitHub Flavored Markdown (GFM) extension and are a de
;; facto standard given their popularity and implementations across
;; products.  They might as well be folded into the CommonMark standard.
;;
;; A pipe table is a rectangle of text surrounded to the east and west
;; by pipe symbols and where pipes separate columns.  It has a header
;; row immediately followed by header alignment row, both of which must
;; contain the same number of cells.  Body rows are optional.  A table
;; ends with a blank line or at the start of a new Markdown block
;; element.
;;
;; The header alignment row column must have a minimum of three hyphens
;; which indicates default alignment (i.e., whatever a renderer
;; chooses).  To indicate left alignment, prefix the hyphen string with
;; a colon like this :---.  For center alignment, prefix and suffix with
;; colons like this :---:.  For right alignment, suffix with a colon
;; like this ---:.
;;
;; Each table header or body cell can contain the usual Markdown inline
;; style indicators.  A cell cannot contain a block element such as a
;; headings, thematic breaks, block quotes, fenced code blocks.
;;
;; Table cells do not need to align visually; i.e., the pipe symbols do
;; not need to line up vertically.  The tree-sitter parser and renderers
;; detect cell boundaries using pipe symbols, not their relative alignment.
;;
;; If you want to include what looks like code, you can use backticks to
;; wrap such text ala `this is code`.  This mode will not fontify such
;; code.
;;
;; If you want to include a pipe symbol in a cell, escape it thusly \|.
;;
;; Technically, body rows do not need to contain the same number of
;; cells as the header has columns and rows do not need to share the
;; same number of cells among themselves.  Note: Many renderers get
;; confused if the table is "ragged" with an uneven number of columns
;; among rows.  Some renderers will insert empty cells on a row that
;; contains fewer cells than the header has columns.  Some elide cells
;; that exceed the number of columns in the header.
;;
;;;; Pipe Table Recommendations
;;
;; - Always use pipe symbols at the start and end of each table line.
;; - Use a uniform number of columns spanning the table.
;; - If you detect a parsing error which presents as different
;;   fontification and which is often caused by an empty first cell on the
;;   final row, try putting some characters in that cell.
;; - Renderers often exclude certain empty cells such as an "empty" final
;;   cell in a table.  Follow the next item to avoid this.
;; - If you need that cell to appear blank and are converting to HTML,
;;   try using a non-printing HTML entity, such as a non-breaking space
;;   "&nbsp;", which parse as concrete characters yet render as blank.
;; - HTML comments <!-- --> can also be used as a non-blank character
;;   string that does not get rendered.  These are considered cell text
;;   and when placed at the end of a row, that row's number of columns is
;;   increased and might exceed the number of header columns.
;; - There are tree-sitter parser quirks.  Commands such as
;;   `markdown-ts-table-delete-column' and `markdown-ts-table-move-column'
;;   follow the parser tree and can lead to unexpected results so follow
;;   these recommendations and table operations should be as expected.
;;
;;;; Tree Sitter Bugs
;;
;; `markdown-ts-mode' relies on the underlying tree-sitter library in
;; Emacs (chosen at its build time), and language grammars you have
;; installed.  There are known and reported bugs which negatively affect
;; certain features.  This mode should benefit as these bugs are fixed
;; or worked around.
;;
;; - The Markdown grammar inserts block_continuation nodes as children
;;   of code_fence_content, which confuses both the inspector and the
;;   embedded parser.  This affects code blocks inside block quotes.
;;
;; - HTML block type 4 (`<!' followed by an uppercase letter, e.g.,
;;   `<!DOCTYPE html>') causes the parser to consume all subsequent
;;   content.  Lowercase `<!doctype html>' works as a workaround.
;;   See <https://github.com/tree-sitter-grammars/tree-sitter-markdown/issues/233>.
;;
;; - The grammar parses solo tildes, incorrectly applying strikethrough.
;;   For example, writing:
;;
;;     I see ~approximately four lights.
;;     I do not see ~approximately five lights.
;;
;;   Results in strikethrough incorrectly starting at the first
;;   ~approximately and extending till the tilde at the second
;;   ~approximately.
;;   See <https://github.com/tree-sitter-grammars/tree-sitter-markdown/issues/236>.
;;
;; - Superscript (`^text^') and subscript (`~text~') syntax is not
;;   supported by the grammar.  No EXTENSION_ build flag exists for
;;   this.  This is Pandoc / PHP Markdown Extra syntax, not CommonMark
;;   or GFM.
;;
;; - Ordered (numbered) lists do not nest by indentation.  Indenting
;;   a `1.' item under another ordered item does not produce a nested
;;   list node; the parser either treats it as a flat sibling or
;;   absorbs it into the parent item's paragraph as a
;;   block_continuation.  Unordered (`-', `*', `+') lists nest
;;   correctly.  Demote/promote of ordered list items is therefore
;;   disabled.
;;
;; - Renumbering ordered lists (`markdown-ts-renumber-list') may only
;;   affect items from point downward if the parser splits a single
;;   list into separate `list' nodes, or may continue numbering across
;;   two separate lists if the parser merges them into one node.
;;
;; - Empty lines following an `indented_code_block' may be claimed by
;;   the parser as continuation lines of that block, rather than being
;;   treated as blank separators.
;;
;; - Pipe tables are inconsistently parsed.  Whitespace is correctly
;;   trimmed at the start of a cell content but trailing whitespace is
;;   incorrectly included.  Empty cells can contain uneven "ragged" row
;;   column configurations that can confuse the parser.
;;
;;   Markdown pipe tables with parsing issues:
;;
;;   |Column 1|Column 2|Column 3|
;;   |--------|--------|--------|
;;   |        |        |        |
;;   |        |        |        | <-- parse error
;;
;;   |Column 1|Column 2|Column 3|
;;   |--------|--------|--------|
;;   |        |        |        |
;;   | xxx    |        |        | <-- parsed correctly
;;
;;   |Column 1|Column 2| <-- 2 columns correct
;;   |--------|--------| <-- 2 columns correct
;;            |        | <-- 1 column incorrect
;;            |        | <-- parse error
;;   |        |        | <-- not a row after the error, above
;;
;;   See <https://github.com/tree-sitter-grammars/tree-sitter-markdown/issues/241>
;;       <https://github.com/tree-sitter-grammars/tree-sitter-markdown/issues/242>
;;
;; - The grammar's external scanner has a buffer overflow in its
;;   `serialize' function: when the parser state exceeds the
;;   serialization buffer provided by tree-sitter, `memcpy' writes past
;;   the end and can corrupt the stack.  Triggered in practice while
;;   parsing Markdown in Emacs.
;;   See <https://github.com/tree-sitter-grammars/tree-sitter-markdown/issues/243>.
;;
;;;; Batch Fontification
;;
;; Some downstream packages fontify multiple unrelated Markdown
;; fragments by joining them in a single buffer with a separator and
;; running `markdown-ts-mode' over the whole thing.  Choice of separator
;; matters: the tree-sitter-markdown grammar does not list the NUL byte
;; (`\0') as extra/whitespace, so a NUL separator yields an ERROR node
;; and the parser enters error recovery, which can leak inline faces
;; (e.g., strikethrough) across fragment boundaries.  Regex-based modes
;; tolerate NUL silently because they do not parse structure; this is a
;; behavioral difference, not a regression.
;;
;; A form feed (`^L', `\f') flanked by blank lines works as a clean
;; drop-in separator: the parser treats it as a paragraph break and
;; inline state does not bleed across fragments.

;;; Code:

(require 'treesit)
(require 'subr-x)
(require 'seq)
(require 'outline)
(require 'goto-addr)
(require 'xref)
(require 'icons)

;;; Customizations:

(defgroup markdown-ts nil
  "Major mode for viewing and editing Markdown buffers."
  :prefix "markdown-ts-"
  :group 'text
  :group 'editing
  :version "31.1")

(defcustom markdown-ts-hide-markup nil
  "Non-nil means hide Markdown markup delimiters in this buffer."
  :type 'boolean
  :local t
  :safe #'booleanp
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-ellipsis nil
  "The ellipsis to use in folded headings.
When nil, use the standard three dots.  When a non-empty string,
use that string instead."
  :type '(choice (const  :tag "Default (...)" nil)
                 (string :tag "String" :value " ⌄"))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-menu-bar-show t
  "Non-nil means show the Markdown menu in the menu bar."
  :type 'boolean
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-default-folding 'show-all
  "Default heading folding level."
  :type '(choice
          (const :tag "Show everything" show-all)
          (const :tag "Fold everything, showing top-level headings" fold-all)
          (const :tag "Fold headings, showing all heading levels" fold-headings))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-inline-images nil
  "Non-nil means display inline images below image links."
  :type 'boolean
  :local t
  :safe #'booleanp
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-image-max-width 'window
  "Maximum width of inline images in pixels.
When `window', use the window body width.  When a number, use
that as the maximum pixel width."
  :type '(choice (const  :tag "Window width" window)
                 (natnum :tag "Pixel width"))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-display-remote-inline-images nil
  "How to display remote inline images.
When nil, do not display remote images.  When `download', fetch the
image into a temporary buffer and display it.

Remote images are skipped by default for security."
  :type '(choice (const :tag "Skip remote images" nil)
                 (const :tag "Download remote images" download))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-unordered-list-marker '(("● " . "- ")
                                               ("○ " . "- ")
                                               ("◼ " . "- ")
                                               ("• " . "- "))
  "If markup is hidden, display these for an unordered list marker.
Each list item marker's depth in its list controls its selected string
starting at the first element and cycling through the others for deeper
items.  The list will be cycle around back to the beginning if there are
insufficient strings to represent deep levels.

Note that the default strings have trailing spaces.

Value forms:
 - (list (cons (PREFERRED . FALLBACK)) ...): where PREFERRED is used if
   its first character passes `char-displayable-p', otherwise FALLBACK.
 - nil: display the raw markup."
  :type '(choice (repeat (cons (string :tag "Preferred (GUI)")
                               (string :tag "Fallback (TTY)")))
                 (const :tag "Display original markup" nil))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-checked-checkbox '("☑" . "+")
  "If markup is hidden, display this for a checked task list marker.
Value forms:
 - cons (PREFERRED . FALLBACK): PREFERRED is used if its first character
   passes `char-displayable-p', otherwise FALLBACK.
 - symbol `icon': defer to the icons.el icon
   `markdown-ts-checked-checkbox-icon'; customize via
   \\[customize-icon].
 - nil: display the raw markup."
  :type '(choice (cons (string :tag "Preferred (GUI)")
                       (string :tag "Fallback (TTY)"))
                 (const :tag "Use icons.el icon" icon)
                 (const :tag "Display original markup" nil))
  :version "31.1"
  :package-version "1.0")

(define-icon markdown-ts-checked-checkbox-icon nil
  '((image "checked.svg" :height line :ascent center)
    (symbol "☑")
    (text "+"))
  "Icon used for a checked task list marker when markup is hidden.
Consulted only when `markdown-ts-checked-checkbox' is the symbol
`icon'."
  :version "31.1")

(defcustom markdown-ts-unchecked-checkbox '("☐" . "-")
  "If markup is hidden, display this for an unchecked task list marker.
Value forms:
 - cons (PREFERRED . FALLBACK): PREFERRED is used if its first character
   passes `char-displayable-p', otherwise FALLBACK.
 - symbol `icon': defer to the icons.el icon
   `markdown-ts-unchecked-checkbox-icon'; customize via
   \\[customize-icon].
 - nil: display the raw markup."
  :type '(choice (cons (string :tag "Preferred (GUI)")
                       (string :tag "Fallback (TTY)"))
                 (const :tag "Use icons.el icon" icon)
                 (const :tag "Display original markup" nil))
  :version "31.1"
  :package-version "1.0")

(define-icon markdown-ts-unchecked-checkbox-icon nil
  '((image "unchecked.svg" :height line :ascent center)
    (symbol "☐")
    (text "-"))
  "Icon used for an unchecked task list marker when markup is hidden.
Consulted only when `markdown-ts-unchecked-checkbox' is the symbol
`icon'."
  :version "31.1")

(defcustom markdown-ts-thematic-break-character '(?─ . ?-)
  "If markup is hidden, display this character for thematic breaks.
It is repeated to fill the window width.  This assumes a static window
width.
You may prefer an `:extend' attribute on the
`markdown-ts-thematic-break' which will span window width dynamically
using an underline, in which case this character is ignored.
The value is a cons (PREFERRED . FALLBACK): PREFERRED is used if it passes
`char-displayable-p', otherwise FALLBACK is used.
Use nil to display the raw markup."
  :type '(choice (cons (character :tag "Preferred (GUI)")
                       (character :tag "Fallback (TTY)"))
                 (const :tag "Display original markup" nil))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-hard-line-break-backslash '(?¶ . ?|)
  "If markup is hidden, display this character for a backslash hard line break.
The value is a cons (PREFERRED . FALLBACK): PREFERRED is used if it passes
`char-displayable-p', otherwise FALLBACK is used.
nil keeps the raw markup."
  :type '(choice (cons (character :tag "Preferred (GUI)")
                       (character :tag "Fallback (TTY)"))
                 (const :tag "Display original markup" nil)
                 (const :tag "Hide markup" hide))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-hard-line-break-space
  (lambda (n)
    (let* ((val (markdown-ts--resolve-display-value
                 markdown-ts-hard-line-break-backslash))
           (ch (if (characterp val) val ?|)))
      (make-string n ch)))
  "If markup is hidden, display this for a trailing-spaces hard line break.
The value can be:
- a character or string: shown once at the start of the trailing spaces,
  with no repetition (the remaining spaces stay invisible);
- a function: called with one argument, the number of trailing spaces,
  and must return the string to display in place of the run;
- nil: keep the raw markup."
  :type '(choice (character :tag "Display specified character (no repetition)")
                 (string :tag "Display specified string (no repetition)")
                 (function :tag "Function from count to display string")
                 (const :tag "Display original markup" nil)
                 (const :tag "Hide markup" hide))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-fontify-code-blocks-natively t
  "Non-nil means fontify code block contents using the language's mode.
When non-nil, fenced code blocks are highlighted with syntax of the
embedded language (via tree-sitter for languages with a tree-sitter
mode, or via conventional font-lock for the rest).  When nil, code
blocks keep only the `markdown-ts-code-block' background face.  Toggling
for the ts-embedded path takes effect after restarting
`markdown-ts-mode' in the buffer."
  :type 'boolean
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-enable-code-block-context-mode t
  "Non-nil automatically enables `markdown-ts-code-block-context-mode'.
If non-nil and `point' is in a fenced code block, this runs
`indent-for-tab-command', `newline', et.al., in the mode of the code
block."
  :type 'boolean
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-code-block-in-context-mode-lighter " [code]"
  "Minor mode `markdown-ts-code-block-context-mode' lighter string.
Set to nil to disable the lighter."
  :type 'string
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-inhibit-code-block-mode-warnings t
  "If non-nil, inhibit code-block major-mode messages and warnings.
Some fenced code-block major modes produce messages or warnings which
have little relevance to Markdown buffers.  Use this option to inhibit
them.  Major modes may be enabled for fontification or editing in the
mode's context."
  :type 'boolean
  :version "31.1"
  :package-version "1.0")

(defvar warning-minimum-level)

(defmacro markdown-ts--inhibit-messages-and-warnings (var &rest body)
  "Execute BODY with messages and warnings inhibited.
VAR is a variable's symbol; e.g.,
\\='markdown-ts-inhibit-code-block-mode-warnings.  If the symbol's value
is non-nil, do what it says on the tin.
Inhibit messages in the echo area.
Inhibit messages in the log.
Inhibit warnings with level < :error."
  (declare (indent defun))
  `(let* ((val (symbol-value ,var))
          (inhibit-message val)
         ;; Also inhibit logging to *Messages*
         (message-log-max (if val nil message-log-max))
         (warning-minimum-level
          (if val :error warning-minimum-level)))
     (progn ,@body)))

(defcustom markdown-ts-default-code-block-mode 'text-mode
  "Default mode for anonymous code blocks."
  :type '(choice
          (const  :tag "Text mode" text-mode)
          (const  :tag "Fundamental mode" fundamental-mode)
          (symbol :tag "Other mode symbol"
                  :validate (lambda (wid)
                              (let ((val (widget-value wid)))
                                (unless (and val (eq 'symbol (type-of val)))
                                  (widget-put wid :error "Must be a non-nil symbol")
                                  wid)))))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-table-default-column-width 5
  "Column width for new columns.
You may make this wider, but smaller than 5 is not recommended."
  :type 'integer
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-enable-table-mode t
  "Non-nil automatically enables `markdown-ts-table-mode'.
If non-nil, enable `markdown-ts-table-mode' if `point' is in a Markdown
table."
  :type 'boolean
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-table-auto-align '(cell-navigation transpose)
  "Automatically align the table at point during these operations.
If t, trigger on all supported operations.
If nil, do not auto align.
Otherwise, it is a list of symbols representing table operations."
  :type '(choice
          (const :tag "Do not automatically align" nil)
          (const :tag "Always automatically align" t)
          (set
           (const :tag "Cell navigation" cell-navigation)
           (const :tag "Transpose" transpose)))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-table-align-features '(justify-cells)
  "Use these optional features when aligning a table.
Aligning a table aligns each column to the width of its widest cell.  It
can also justify each cell according to the table's delimiter line left,
center, or right-hand justification rules.

Customizing this user option is most useful when the user option
`markdown-ts-table-auto-align' is enabled and to avoid having to use a
prefix argument before every auto-alignment operation.

If t, enable all optional alignment operations.
If nil, operations are enabled beyond basic column alignment.
Otherwise, it is a list of symbols."
  :type '(choice
          (const :tag "No optional features" nil)
          (const :tag "All optional features" t)
          (set
           (const :tag "Justify cells" justify-cells)))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-in-table-mode-lighter " [table]"
  "Minor mode `markdown-ts-in-table-mode' lighter string.
Set to nil to disable the lighter."
  :type 'string
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-view-mode-pre-init-hook (list #'markdown-ts-add-final-newline)
  "Hooks run before `markdown-ts-view-mode` initialization.
Functions on this list are intended to amend buffer content for
`markdown-ts-view-mode' and tree-sitter Markdown grammar compatibility.

For example, `markdown-ts-add-final-newline' ensures the grammar
correctly parses markup at the end of the buffer that depends on a final
newline."
  :type '(hook)
  :version "31.1"
  :package-version "1.0")

;;; Faces:

(defgroup markdown-ts-faces nil
  "Faces used by Markdown-TS."
  :group 'markdown-ts-faces
  :group 'faces)

(defface markdown-ts-delimiter '((t (:inherit shadow :slant normal :weight normal)))
  "Face for the # before Markdown headings."
  :version "31.1")

(defface markdown-ts-heading-1 '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for first level Markdown headings."
  :version "31.1")

(defface markdown-ts-setext-heading '((t (:inherit markdown-ts-heading-1)))
  "Face for setext Markdown headings (headings underlined by === or ---)."
  :version "31.1")

(defface markdown-ts-heading-2 '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for second level Markdown headings."
  :version "31.1")

(defface markdown-ts-heading-3 '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for third level Markdown headings."
  :version "31.1")

(defface markdown-ts-heading-4 '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for fourth level Markdown headings."
  :version "31.1")

(defface markdown-ts-heading-5 '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for fifth level Markdown headings."
  :version "31.1")

(defface markdown-ts-heading-6 '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for sixth level Markdown headings."
  :version "31.1")

(defface markdown-ts-emphasis '((t (:inherit italic)))
  "Face for Markdown emphasis (italic) text."
  :version "31.1")

(defface markdown-ts-bold '((t (:inherit bold)))
  "Face for Markdown strong emphasis (bold) text."
  :version "31.1")

(defface markdown-ts-strikethrough '((t (:strike-through t)))
  "Face for Markdown strikethrough text."
  :version "31.1")

(defface markdown-ts-block-quote '((t (:inherit font-lock-doc-face)))
  "Face for Markdown block quotes."
  :version "31.1")

(defface markdown-ts-link '((t (:inherit link)))
  "Face for Markdown link text and image descriptions."
  :version "31.1")

(defface markdown-ts-link-destination '((t (:inherit font-lock-string-face)))
  "Face for Markdown link destinations (URLs)."
  :version "31.1")

(defface markdown-ts-code-span '((t (:inherit (markdown-ts-code-block font-lock-constant-face))))
  "Face for Markdown inline code spans."
  :version "31.1")

(defface markdown-ts-code-block '((t (:inherit fixed-pitch :extend t)))
  "Face for Markdown fenced code block content.
Alter this face to add a `:background' for a visually distinct
code block region, e.g.:
  (set-face-attribute \\='markdown-ts-code-block nil :background \"gray95\")"
  :version "31.1")

(defface markdown-ts-in-code-block '((t (:extend t)))
  "Face for when point is in a Markdown code block.
See `markdown-ts-code-block-in-context-mode'.
Alter this face to add a `:background' for a visually distinct table
region, e.g.:
  (set-face-attribute \\='markdown-ts-in-code-block :background \"gray95\")"
  :version "31.1")

(defface markdown-ts-code-block-markup-hidden
  '((((background light)) (:background "gray95" :extend t))
    (((background dark))  (:background "gray15" :extend t)))
  "Face for Markdown fenced code block content when markup is hidden.
Used instead of `markdown-ts-code-block' when `markdown-ts-hide-markup'
is non-nil."
  :version "31.1")

(defface markdown-ts-indented-code-block '((t (:inherit (markdown-ts-code-block font-lock-constant-face))))
  "Face for Markdown indented code blocks."
  :version "31.1")

(defface markdown-ts-html-tag '((t (:inherit font-lock-type-face)))
  "Face for inline HTML tags in Markdown."
  :version "31.1")

(defface markdown-ts-html-block '((t (:inherit font-lock-type-face)))
  "Face for HTML blocks in Markdown."
  :version "31.1")

(defface markdown-ts-thematic-break '((t (:inherit markdown-ts-delimiter :extend t)))
  "Face for Markdown thematic breaks (horizontal rules).
Customize this face to add a :background for a full-width visual rule."
  :version "31.1")

(defface markdown-ts-entity-reference '((t (:inherit font-lock-variable-name-face)))
  "Face for named HTML entity references like &amp; and &copy;."
  :version "31.1")

(defface markdown-ts-numeric-character-reference
  '((t (:inherit font-lock-variable-name-face)))
  "Face for numeric character references like &#65; and &#x41;."
  :version "31.1")

(defface markdown-ts-latex '((t (:inherit font-lock-string-face)))
  "Face for LaTeX / math content in Markdown ($...$ and $$...$$)."
  :version "31.1")

(defface markdown-ts-table-header '((t (:inherit markdown-ts-table)))
  "Face for Markdown pipe table header cells."
  :version "31.1")

(defface markdown-ts-table-cell '((t (:inherit markdown-ts-table)))
  "Face for Markdown pipe table data cells."
  :version "31.1")

(defface markdown-ts-table-delimiter-cell '((t (:inherit markdown-ts-table)))
  "Face for Markdown pipe table delimiter cells (--- separators)."
  :version "31.1")

(defface markdown-ts-table '((t (:inherit (markdown-ts-code-block) :extend t)))
  "Face for Markdown table.
Alter this face to add a `:background' for a visually distinct table
region, e.g.:
  (set-face-attribute \\='markdown-ts-table nil :background \"gray95\")"
  :version "31.1")

(defface markdown-ts-in-table '((t (:extend t)))
  "Face for Markdown `markdown-ts-in-table-mode' when point is in a table.
Alter this face to add a `:background' for a visually distinct table
region, e.g.:
  (set-face-attribute \\='markdown-ts-in-table nil :background \"gray95\")"
  :version "31.1")

(defface markdown-ts-language-keyword '((t (:inherit font-lock-type-face)))
  "Face for the language keyword for Markdown code blocks."
  :version "31.1")

(defface markdown-ts-list-marker '((t (:inherit shadow :slant normal :weight normal)))
  "Face for Markdown list markers like - and *."
  :version "31.1")

(defface markdown-ts-hard-line-break-backslash
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for Markdown hard line breaks introduced by a trailing backslash."
  :version "31.1")

(defface markdown-ts-hard-line-break-backslash-hidden
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for trailing-backslash hard line break when markup is hidden."
  :version "31.1")

(defface markdown-ts-hard-line-break-space
  '((((background light)) (:background "gray70" :weight bold))
    (((background dark))  (:background "gray40" :weight bold)))
  "Face for Markdown hard line breaks introduced by two trailing spaces.
The trailing spaces are otherwise invisible, so they are shown as a
shadow-colored block."
  :version "31.1")

(defface markdown-ts-hard-line-break-space-hidden
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for trailing-spaces hard line break when markup is hidden."
  :version "31.1")

(defface markdown-ts-task-unchecked '((t (:inherit font-lock-builtin-face)))
  "Face for Markdown unchecked task list markers."
  :version "31.1")

(defface markdown-ts-task-checked '((t (:inherit font-lock-builtin-face)))
  "Face for Markdown checked task list markers."
  :version "31.1")

(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(markdown
   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
   :commit "413285231ce8fa8b11e7074bbe265b48aa7277f9"
   :source-dir "tree-sitter-markdown/src")
 t)

(add-to-list
 'treesit-language-source-alist
 '(markdown-inline
   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
   :commit "413285231ce8fa8b11e7074bbe265b48aa7277f9"
   :source-dir "tree-sitter-markdown-inline/src")
 t)

;;; Variables:

(defvar markdown-ts--set-up-inline nil
  "Let bind this to non-nil for inline `markdown-ts-mode' buffers.")

(defvar markdown-ts-code-block-modes
  `((sh bash-ts-mode)
    (shell bash-ts-mode)
    (,(intern "c#") csharp-ts-mode)
    (cpp c++-ts-mode)
    (el emacs-lisp-mode)
    (elisp emacs-lisp-mode)
    (golang go-ts-mode)
    (gomod go-mod-ts-mode)
    (gowork go-work-ts-mode)
    (javascript js-ts-mode)
    (md markdown-ts-mode)
    (py python-ts-mode)
    (rb ruby-ts-mode)
    (rs rust-ts-mode)
    (tex latex-mode)
    (ts typescript-ts-mode)
    (yml yaml-ts-mode))
  "Extra mappings from code block language tags to major modes.
Entries here are only needed when the language tag in a fenced code
block does NOT match the conventional mode name derivation, e.g. the
user writes \\=`\\=`\\=`ts instead of \\=`\\=`\\=`typescript, or
\\=`\\=`\\=`py instead of \\=`\\=`\\=`python.  For tags that already
resolve via the standard \"-ts-mode\"/\"-mode\" heuristics there is no
need to add an entry.

The alist is of the form (LANGUAGE MAJOR-MODE). MAJOR-MODE can be a
tree-sitter or a conventional mode.

If you prefer a conventional mode over its tree-sitter variant, add or
replace relevant entries.  For example:

    (add-to-list \\='markdown-ts-code-block-modes
      \\='(python python-mode))
    (setf (alist-get \\='py markdown-ts-code-block-modes)
          \\='(python-mode))

Code blocks for that non tree-sitter modes will be fontified using the
mode's conventional font-lock rules.")

(defvar markdown-ts-code-block-force-conventional-modes
  '(markdown-ts-mode
    php-ts-mode)
  "Treat these code block tree-sitter modes as conventional.
Some modes that embed multiple `treesit' parsers need to be treated with
conventional font-lock.  `markdown-ts-mode' itself is one of them.")

;;; Font-lock:

(defun markdown-ts--fontify-delimiter (node override start end &rest _)
  "Fontify delimiter NODE and optionally hide its markup.
NODE is the tree-sitter node representing the delimiter.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'markdown-ts-delimiter override start end)
  (when markdown-ts-hide-markup
    (if (and (derived-mode-p 'markdown-ts-view-mode)
             (equal (treesit-node-type node) "fenced_code_block_delimiter"))
        ;; In view-mode only, hide the whole line containing the fence
        ;; (including its terminating newline) so Eldoc/LSP markdown
        ;; snippets render without stray blank lines around the code
        ;; block.  Restricted to view-mode because hide-markup while
        ;; editing already has UX hazards (point movement, backspace
        ;; across invisible regions) and we should not tune rendering
        ;; for that mode.  Restricted to fenced_code_block_delimiter
        ;; because the same handler is shared by inline delimiters
        ;; (emphasis, code span, link brackets) where munching
        ;; surrounding whitespace would collapse word separators.
        (save-excursion
          (goto-char (treesit-node-start node))
          (let ((bol (pos-bol))
                (eol+1 (progn (goto-char (treesit-node-end node))
                              (min (point-max) (1+ (pos-eol))))))
            (put-text-property bol eol+1
                               'invisible 'markdown-ts--markup)))
      (put-text-property (treesit-node-start node) (treesit-node-end node)
                         'invisible 'markdown-ts--markup))))

(defun markdown-ts--fontify-atx-delimiter (node override start end &rest _)
  "Fontify atx_heading delimiter NODE and optionally hide its markup.
NODE is the tree-sitter node representing the delimiter.
Leading whitespace between the delimiter and the heading text is hidden
along with the delimiter when hiding markup.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'markdown-ts-delimiter override start end)
  (when markdown-ts-hide-markup
    (put-text-property (treesit-node-start node)
                       (save-excursion
                         (goto-char (treesit-node-end node))
                         (re-search-forward "[^[:blank:]]" (pos-eol) 'no-error)
                         (if (eq (point) (pos-eol))
                             (point)
                           (1- (point))))
                       'invisible 'markdown-ts--markup)))

(defvar url-mail-command) ; url/url-vars.el

(defun markdown-ts--make-link-button (beg end url)
  "Make the region from BEG to END a clickable button for URL.
For mailto: URIs, use `url-mail-command'.  For other schemes
\(e.g., http, ftp), open with `browse-url'.  Otherwise, treat as
a relative file path and open with `find-file'.

Do not pass `face' to `make-text-button': the link face is already
applied by `markdown-ts--fontify-link-node' via
`treesit-fontify-with-override' (with `:override append'), and
`add-text-properties' would otherwise replace the appended face
list with a single `markdown-ts-link', clobbering an enclosing
heading face."
  ;; NOTE: URI scheme and host name are case-insensitive per RFC 3986
  ;; and RFC 7230.
  (let ((case-fold-search nil))
    (make-text-button beg end
                      'action (lambda (_button)
                                (cond
                                 ((string-prefix-p "#" url)
                                  (markdown-ts--follow-fragment
                                   (substring url 1)))
                                 ((string-match-p "\\`mailto:" url)
                                  (funcall url-mail-command
                                           (replace-regexp-in-string
                                            "\\`mailto:" "" url)))
                                 ((string-match-p "\\`[a-z]+:" url)
                                  (browse-url url))
                                 (t (find-file url))))
                      'help-echo url)))

(defun markdown-ts--fontify-link-destination (node override start end &rest _)
  "Fontify link destination NODE and hide it when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'markdown-ts-link-destination override start end)
  (when markdown-ts-hide-markup
    (put-text-property (treesit-node-start node) (treesit-node-end node)
                       'invisible 'markdown-ts--markup)))

(defvar-local markdown-ts--link-ref-cache nil
  "Cached alist of (LABEL . URL) from link reference definitions.
Each entry is a cons cell mapping a downcased label string to its
destination URL.")

(defvar-local markdown-ts--link-ref-cache-tick nil
  "Value of `buffer-chars-modified-tick' when the cache was built.")

(defun markdown-ts--link-ref-definitions ()
  "Return an alist of (LABEL . URL) from link reference definitions.
LABEL is downcased for case-insensitive matching.  Results are
cached and rebuilt only when the buffer changes."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq tick markdown-ts--link-ref-cache-tick)
      (let* ((root (treesit-buffer-root-node 'markdown))
             (matches (treesit-query-capture
                       root
                       '((link_reference_definition
                          (link_label) @label
                          (link_destination) @dest))))
             defs)
        (while matches
          (let* ((label-pair (pop matches))
                 (dest-pair (pop matches))
                 (lbl-text (string-trim (treesit-node-text (cdr label-pair) t)
                                        "\\[" "\\]")))
            (push (cons (downcase lbl-text)
                        (treesit-node-text (cdr dest-pair) t))
                  defs)))
        (setq markdown-ts--link-ref-cache (nreverse defs)
              markdown-ts--link-ref-cache-tick tick)))
    markdown-ts--link-ref-cache))

(defun markdown-ts--resolve-link-ref (label)
  "Resolve LABEL to a URL via link reference definitions.
LABEL should be the text without brackets.  Matching is
case-insensitive per CommonMark spec section 4.7."
  (alist-get (downcase label) (markdown-ts--link-ref-definitions)
             nil nil #'equal))

;;; Fragment links:

(defconst markdown-ts--slug-github-strip-re
  "[][!\"#$%&'()*+,./:;<=>?@\\^`{|}~]"
  "Punctuation stripped by GitHub's gh-slugger before hyphenation.")

(defun markdown-ts--slug-github (text)
  "Return the GitHub-flavor slug for heading TEXT.
Mirrors the gh-slugger algorithm: lowercase, strip a fixed
punctuation set, replace spaces with hyphens."
  (let* ((lc (downcase text))
         (stripped (replace-regexp-in-string
                    markdown-ts--slug-github-strip-re "" lc)))
    (replace-regexp-in-string "[[:blank:]]" "-" stripped)))

(defun markdown-ts--slug-pandoc (text)
  "Return the Pandoc-flavor auto-id slug for heading TEXT.
Mirrors Pandoc's algorithm: drop everything before the first
letter, keep alphanumerics and `_-.', collapse whitespace runs to
hyphens, lowercase.  Falls back to \"section\" if empty."
  (let* ((lc (downcase text))
         (from-letter (if (string-match "[[:alpha:]]" lc)
                          (substring lc (match-beginning 0))
                        ""))
         (kept (replace-regexp-in-string
                "[^[:alnum:]_.\n\t[:blank:]-]" "" from-letter))
         (hyphenated (replace-regexp-in-string "[[:blank:]\t\n]+" "-" kept))
         (trimmed (replace-regexp-in-string "-+\\'" "" hyphenated)))
    (if (string-empty-p trimmed) "section" trimmed)))

(defconst markdown-ts--explicit-id-re
  "[ \t]*{[ \t]*#\\([^} \t]+\\)[ \t]*}[ \t]*\\'"
  "Match a Pandoc-style trailing `{#id}' on a heading line.
Capture group 1 is the explicit id.")

(defun markdown-ts--heading-text-and-id (raw)
  "Split RAW heading text into (VISIBLE . EXPLICIT-ID).
EXPLICIT-ID is non-nil if RAW ends with a Pandoc-style `{#id}',
in which case VISIBLE is RAW with that suffix removed."
  (if (string-match markdown-ts--explicit-id-re raw)
      (cons (substring raw 0 (match-beginning 0))
            (match-string 1 raw))
    (cons raw nil)))

(defvar-local markdown-ts--heading-id-cache nil
  "Cached hash table mapping heading slug strings to buffer positions.
Each value is the buffer position of the start of the matching
`atx_heading' or `setext_heading' node.  Built lazily by
`markdown-ts--heading-ids', invalidated by buffer-tick change.")

(defvar-local markdown-ts--heading-id-cache-tick nil
  "Value of `buffer-chars-modified-tick' when the heading-id cache was built.")

(defun markdown-ts--build-heading-ids ()
  "Walk all headings in the buffer and return a hash table.
Which maps each slug to the buffer position of its heading.  Each
heading contributes its GitHub slug, its Pandoc slug, and its explicit
`{#id}' if present.  Both slug algorithms are stored in the same buffer
because a markdown file is often previewed by several renderers (GitHub
web UI, Pandoc, mdBook, Hugo, and so on) and the same source should
resolve regardless of which one the author wrote the link for.  For most
headings the two algorithms produce the same slug anyway.
Duplicate slugs (within one algorithm) are disambiguated by appending
`-1', `-2', and so on, in document order, matching GitHub's behavior.
When two distinct headings would otherwise share a slug, the first
occurrence in document order wins."
  (let ((table (make-hash-table :test #'equal))
        ;; Per-algorithm dedupe counters: how many times this base slug
        ;; has been seen so far in document order.
        (gh-counts (make-hash-table :test #'equal))
        (pd-counts (make-hash-table :test #'equal))
        (root (treesit-buffer-root-node 'markdown)))
    (dolist (cap (treesit-query-capture
                  root
                  '(((atx_heading) @h)
                    ((setext_heading) @h))))
      (let* ((node (cdr cap))
             (pos (treesit-node-start node))
             (raw (string-trim (treesit-node-text node t)))
             ;; Strip the leading marker (### or ====/----).
             ;; For atx, drop leading #'s and following space.
             ;; For setext, drop the trailing underline line.
             (text (cond
                    ((string-match "\\`#+[ \t]*\\(.*?\\)[ \t]*#*[ \t]*\\'" raw)
                     (match-string 1 raw))
                    ((string-match "\\`\\(.*?\\)\n[=-]+[ \t]*\\'" raw)
                     (match-string 1 raw))
                    (t raw)))
             (split (markdown-ts--heading-text-and-id text))
             (visible (car split))
             (explicit (cdr split)))
        ;; Explicit {#id} wins outright; first occurrence keeps it.
        (when (and explicit (not (gethash explicit table)))
          (puthash explicit pos table))
        ;; Auto slugs (GitHub + Pandoc), deduped per algorithm.
        (let* ((gh-base (markdown-ts--slug-github visible))
               (pd-base (markdown-ts--slug-pandoc visible))
               (gh-n (gethash gh-base gh-counts 0))
               (pd-n (gethash pd-base pd-counts 0))
               (gh-id (if (zerop gh-n) gh-base
                        (format "%s-%d" gh-base gh-n)))
               (pd-id (if (zerop pd-n) pd-base
                        (format "%s-%d" pd-base pd-n))))
          (puthash gh-base (1+ gh-n) gh-counts)
          (puthash pd-base (1+ pd-n) pd-counts)
          ;; First-writer-wins so document order tiebreaks collisions.
          (unless (gethash gh-id table) (puthash gh-id pos table))
          (unless (gethash pd-id table) (puthash pd-id pos table)))))
    table))

(defun markdown-ts--heading-ids ()
  "Return the heading-id hash table for the current buffer.
Cache is rebuilt only when `buffer-chars-modified-tick' has advanced
since the last call (mirrors `markdown-ts--link-ref-cache')."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq tick markdown-ts--heading-id-cache-tick)
      (setq markdown-ts--heading-id-cache (markdown-ts--build-heading-ids)
            markdown-ts--heading-id-cache-tick tick))
    markdown-ts--heading-id-cache))

(defun markdown-ts--follow-fragment (id)
  "Jump to the heading whose slug matches ID (without leading `#').
Pushes the mark before moving so `C-u C-SPC' returns.  Signals
`user-error' if no heading matches."
  (if-let* ((pos (gethash id (markdown-ts--heading-ids))))
      (progn
        (push-mark)
        (goto-char pos)
        (recenter))
    (user-error "No heading for fragment: #%s" id)))

(defun markdown-ts--fontify-atx-heading (node _override _start _end &rest _)
  "Apply the heading face across an atx_heading NODE.
Layer the face on top of child sub-nodes (e.g. an inline link) so
their own faces are preserved.  Strip any prior copy of the face
first so it does not accumulate when the heading is refontified or
its level/type changes during editing.
Do not fontify the header's trailing newline.
Elide trailing whitespace when hiding markup.
Fontify any optional trailing closing-`#' sequence as a delimiter.  The
tree-sitter grammar does not produce a separate node for these; per
CommonMark they are decorative and must be preceded by a space or tab."
  (let* ((n-start (treesit-node-start node))
         (n-end   (treesit-node-end node))
         (face (let ((marker (treesit-node-child node 0)))
                 (intern
                  (format "markdown-ts-heading-%d"
                          (progn
                            (string-match "[[:blank:]]*\\([#]+\\)"
                                          (treesit-node-text marker t))
                            (- (match-end 1) (match-beginning 1))))))))
    (font-lock--remove-face-from-text-property n-start n-end 'face face)
    (font-lock-append-text-property n-start (1- n-end) 'face face)
    (save-excursion
      (goto-char n-end)
      (skip-chars-backward "[:space:]" n-start)
      (let ((trailing-end (point)))
        (skip-chars-backward "#" n-start)
        (let ((trailing-start (point)))
          (cond ((and (< trailing-start trailing-end)
                      (> trailing-start n-start)
                      (memq (char-before trailing-start) '(?\s ?\t)))
                 ;; Identify the optional trailing closing-# sequence,
                 ;; fontify it as a delimiter, and remove whitespace
                 ;; between the heading text and the delimiter.  The
                 ;; grammar omits a node for this run despite CommonMark.
                 (font-lock--remove-face-from-text-property
                  trailing-start trailing-end
                  'face 'markdown-ts-delimiter)
                 (font-lock-prepend-text-property
                  trailing-start trailing-end
                  'face 'markdown-ts-delimiter)
                 (when markdown-ts-hide-markup
                   (let ((hide-start (save-excursion
                                       (goto-char trailing-start)
                                       (skip-chars-backward "[:space:]" n-start)
                                       (point))))
                     (put-text-property hide-start (pos-eol)
                                        'invisible 'markdown-ts--markup))))
                (markdown-ts-hide-markup
                 ;; Hide trailing whitespace in the nominal case.
                 (put-text-property trailing-end (pos-eol)
                                    'invisible 'markdown-ts--markup))))))))

(defun markdown-ts--fontify-setext-heading (node _override _start _end &rest _)
  "Apply the heading face across a setext NODE.
Layer the face on top of child sub-nodes (e.g. an inline link) so
their own faces are preserved.  Strip any prior copy of the face
first so it does not accumulate when the heading is refontified or
its level/type changes during editing.
Apply the face to the setext heading_content separately from the
underline rather than treat them as a single range.  This avoids putting
the face on the heading_content newline.  If `markdown-ts-hide-markup'
is non-nil, hide the underline line entirely by setting its line-height
text property to 0.
Elide trailing whitespace when hiding markup."
  (let* ((n-start (treesit-node-start node))
         (n-end (treesit-node-end node))
         (content (treesit-node-child node 0 'named))
         (content-start (treesit-node-start content))
         (content-end (treesit-node-end content))
         (underline (treesit-node-child node 1 'named))
         (underline-start (treesit-node-start underline))
         (underline-end (treesit-node-end underline))
         (face 'markdown-ts-setext-heading))
    (font-lock--remove-face-from-text-property n-start n-end 'face face)
    ;; 1- content-end avoids the newline so it hides correctly.
    (font-lock-append-text-property content-start (1- content-end) 'face face)
    (font-lock-append-text-property underline-start underline-end 'face face)
    (when markdown-ts-hide-markup
      ;; Hide heading_content trailing spaces.
      (put-text-property (save-excursion
                           (goto-char content-end)
                           (skip-chars-backward "[:space:]" content-start)
                           (point))
                         content-end
                         'invisible 'markdown-ts--markup)
      (put-text-property underline-start underline-end 'line-height 0))))

(defun markdown-ts--fontify-link-node (node override start end &rest _)
  "Fontify link or image text NODE as a clickable button.
Works for inline links, reference links, shortcut links, and
image descriptions.  The URL is taken from a sibling
`link_destination' node when present, or resolved from a
link reference definition, or the node text itself is used as
fallback.  OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let ((parent (treesit-node-parent node)))
    ;; When NODE is a link_label inside a full_reference_link that
    ;; also has a link_text, skip it, link_text handles that link.
    (unless (and (string= (treesit-node-type node) "link_label")
                 (treesit-search-subtree parent "\\`link_text\\'"))
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'markdown-ts-link override start end)
      (let* ((dest (treesit-search-subtree parent "\\`link_destination\\'"))
             (url (if dest
                      (treesit-node-text dest t)
                    ;; Reference links: resolve via link_label or link_text.
                    (let* ((label-node (treesit-search-subtree
                                        parent "\\`link_label\\'"))
                           (label (if label-node
                                      (string-trim (treesit-node-text
                                                    label-node t)
                                                   "\\[" "\\]")
                                    (treesit-node-text node t))))
                      (or (markdown-ts--resolve-link-ref label)
                          label)))))
        (markdown-ts--make-link-button
         (treesit-node-start node) (treesit-node-end node) url)))))

(defun markdown-ts--fontify-autolink (node override start end &rest _)
  "Fontify autolink NODE (URI or email) as a clickable button.
For email autolinks, the URL is prefixed with \"mailto:\".
Angle bracket delimiters are hidden when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (inner (buffer-substring-no-properties
                 (1+ node-start) (1- node-end)))
         (url (if (string= (treesit-node-type node) "email_autolink")
                  (concat "mailto:" inner)
                inner)))
    (treesit-fontify-with-override
     node-start node-end 'markdown-ts-link override start end)
    (markdown-ts--make-link-button node-start node-end url)
    (when markdown-ts-hide-markup
      (put-text-property node-start (1+ node-start)
                         'invisible 'markdown-ts--markup)
      (put-text-property (1- node-end) node-end
                         'invisible 'markdown-ts--markup))))

(defun markdown-ts--fontify-link-ref-label (node override start end &rest _)
  "Fontify link reference definition label NODE as a clickable button.
The brackets and colon are hidden when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (parent (treesit-node-parent node))
         (dest-node (treesit-search-subtree parent "\\`link_destination\\'"))
         (url (when dest-node (treesit-node-text dest-node t))))
    ;; Fontify the label text (inside brackets).
    (treesit-fontify-with-override
     (1+ node-start) (1- node-end)
     'markdown-ts-link override start end)
    ;; Make it a clickable button.
    (when url
      (markdown-ts--make-link-button (1+ node-start) (1- node-end) url))
    ;; Hide "[" before label and "]: " between label and destination.
    (when markdown-ts-hide-markup
      (put-text-property node-start (1+ node-start)
                         'invisible 'markdown-ts--markup)
      (let ((colon-end (if dest-node (treesit-node-start dest-node)
                         (1+ node-end))))
        (put-text-property (1- node-end) colon-end
                           'invisible 'markdown-ts--markup)))))

(defun markdown-ts--fontify-link-ref-destination (node override start end
                                                       &rest _)
  "Fontify link reference destination NODE and hide when markup hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'markdown-ts-link-destination override start end)
  (when markdown-ts-hide-markup
    (put-text-property (treesit-node-start node) (treesit-node-end node)
                       'invisible 'markdown-ts--markup)))

(defun markdown-ts--latex-block-valid-p (node)
  "Return non-nil if latex block NODE is within a single inline scope.
The global inline parser can create false `latex_block' matches
spanning across paragraph boundaries.  Check that the opening and
closing delimiters share the same markdown `inline' ancestor."
  (let ((node-start (treesit-node-start node))
        (node-end (1- (treesit-node-end node))))
    (when-let* ((md-start (treesit-node-at node-start 'markdown))
                (md-end (treesit-node-at node-end 'markdown))
                (inline-start (treesit-parent-until
                               md-start
                               (lambda (n)
                                 (equal (treesit-node-type n) "inline"))))
                (inline-end (treesit-parent-until
                             md-end
                             (lambda (n)
                               (equal (treesit-node-type n) "inline")))))
      (eq (treesit-node-start inline-start)
          (treesit-node-start inline-end)))))

(defun markdown-ts--fontify-latex-block (node override start end &rest _)
  "Fontify latex block NODE and hide its delimiters when markup is hidden.
Skip fontification for false matches that span across paragraph boundaries.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (when (markdown-ts--latex-block-valid-p node)
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'markdown-ts-latex override start end)
    (when markdown-ts-hide-markup
      (dotimes (i (treesit-node-child-count node))
        (let ((child (treesit-node-child node i)))
          (when (equal (treesit-node-type child) "latex_span_delimiter")
            (put-text-property (treesit-node-start child)
                               (treesit-node-end child)
                               'invisible 'markdown-ts--markup)))))))

(defun markdown-ts--fontify-backslash-escape (node override start end &rest _)
  "Fontify backslash escape NODE, hiding the backslash when markup is hidden.
Skip hiding inside `latex_block' where backslashes are LaTeX syntax.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let ((node-start (treesit-node-start node))
        (in-latex (equal (treesit-node-type (treesit-node-parent node))
                         "latex_block")))
    (treesit-fontify-with-override
     node-start (1+ node-start)
     'markdown-ts-delimiter override start end)
    (when (and markdown-ts-hide-markup (not in-latex))
      (put-text-property node-start (1+ node-start)
                         'invisible 'markdown-ts--markup))))

(defvar sgml-char-names)
(declare-function org-entity-get "org/org-entities.el")

(defun markdown-ts--decode-entity (text)
  "Decode HTML entity TEXT (e.g., \"&amp;\") to its character string.
Return the decoded string, or nil if the entity is unknown."
  (cond
   ;; Numeric hex &#x1F4A9;
   ((string-match "\\`&#x\\([[:xdigit:]]+\\);\\'" text)
    (char-to-string (string-to-number (match-string 1 text) 16)))
   ;; Numeric decimal &#123;
   ((string-match "\\`&#\\([[:digit:]]+\\);\\'" text)
    (char-to-string (string-to-number (match-string 1 text))))
   ;; Named &amp; &copy; via org-entities, fall back to sgml-char-names
   ((string-match "\\`&\\([[:alnum:]]+\\);\\'" text)
    (let ((name (match-string 1 text)))
      (require 'org-entities)
      (if-let* ((entry (org-entity-get name))
                (utf8 (nth 6 entry)))
          utf8
        (require 'sgml-mode)
        (when-let* ((code (seq-position sgml-char-names name #'equal)))
          (char-to-string code)))))))

(defun markdown-ts--fontify-entity (node override start end &rest _)
  "Fontify entity NODE and show its decoded value when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node))
        (face (if (equal (treesit-node-type node) "entity_reference")
                  'markdown-ts-entity-reference
                'markdown-ts-numeric-character-reference)))
    (treesit-fontify-with-override node-start node-end face
                                   override start end)
    (if markdown-ts-hide-markup
        (when-let* ((decoded (markdown-ts--decode-entity
                              (treesit-node-text node t)))
                    ((char-displayable-p (aref decoded 0))))
          (put-text-property node-start node-end 'display decoded))
      (remove-text-properties node-start node-end '(display nil)))))

(defun markdown-ts--resolve-display-value (val)
  "Resolve VAL, a cons (PREFERRED . FALLBACK), to a displayable value.
PREFERRED and FALLBACK can be a character or a string.  Return PREFERRED
if it, or its first character, is `char-displayable-p', otherwise return
FALLBACK.
If VAL is not a cons or is nil, return VAL."
  (if (consp val)
      (let* ((preferred (car val))
             (ch (if (characterp preferred)
                     preferred
                   (aref preferred 0))))
        (if (char-displayable-p ch)
            (car val)
          (cdr val)))
    val))

(defun markdown-ts--list-item-depth (node)
  "Compute the depth of list NODE relative to its parents.
NODE can be a list, list_item, or one of the list_marker_'s.
If NODE is not in a list, return -1."
  (let ((depth -1))
    (while (and node
                (not (equal (treesit-node-type node) "section")))
      (when (equal (treesit-node-type node) "list")
        (setq depth (1+ depth)))
      (setq node (treesit-node-parent node)))
    depth))

(defun markdown-ts--fontify-unordered-list-marker (node override start end &rest _)
  "Fontify unordered list marker NODE, show a symbol when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  ;; The tree-sitter markdown grammar includes the leading indentation
  ;; in the first list_marker_minus/plus/star node of a list, so skip
  ;; over any leading whitespace to avoid overwriting the indent with
  ;; the replacement glyph.
  (let* ((raw-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (node-start (save-excursion
                       (goto-char raw-start)
                       (skip-chars-forward " \t" node-end)
                       (point)))
         (face 'markdown-ts-list-marker))
    (treesit-fontify-with-override node-start node-end face
                                   override start end)
    (cond (markdown-ts-hide-markup
           (let* ((depth (markdown-ts--list-item-depth node))
                  (value (if markdown-ts-unordered-list-marker
                             (nth (mod depth (length markdown-ts-unordered-list-marker))
                                  markdown-ts-unordered-list-marker)
                           nil))
                  (display-spec (markdown-ts--resolve-display-value value)))
             (put-text-property node-start node-end 'display display-spec)))
          (t
           (remove-text-properties node-start node-end '(display nil))))))

(defun markdown-ts--fontify-checkbox (node override start end &rest _)
  "Fontify task list checkbox NODE, show a Unicode symbol when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (checked (equal (treesit-node-type node) "task_list_marker_checked"))
         (face (if checked 'markdown-ts-task-checked
                 'markdown-ts-task-unchecked))
         (value (if checked
                    markdown-ts-checked-checkbox
                  markdown-ts-unchecked-checkbox))
         (replacement
          (cond ((eq value 'icon)
                 (icon-string (if checked
                                  'markdown-ts-checked-checkbox-icon
                                'markdown-ts-unchecked-checkbox-icon)))
                (t (markdown-ts--resolve-display-value value))))
         ;; If `icon-string' returned an image, its `display' property
         ;; on char 0 holds the actual image spec; nested `display'
         ;; props are not honored when a string is the value of a
         ;; `display' text-property, so apply the spec directly.
         (display-spec
          (or (and (eq value 'icon) replacement
                   (get-text-property 0 'display replacement))
              replacement)))
    (treesit-fontify-with-override node-start node-end face
                                   override start end)
    (if (and markdown-ts-hide-markup replacement
             (or (eq value 'icon)
                 (char-displayable-p (aref replacement 0))))
        (put-text-property node-start node-end 'display display-spec)
      (remove-text-properties node-start node-end '(display nil)))))

(defun markdown-ts--fontify-hard-line-break (node override start end &rest _)
  "Fontify hard line break NODE; show a Unicode symbol when markup is hidden.
A backslash break gets `markdown-ts-hard-line-break-backslash' (or its
`-hidden' variant when markup is hidden); a trailing-spaces break gets
`markdown-ts-hard-line-break-space' (or `-hidden').  When hidden, a
backslash break is replaced by a single `markdown-ts-hard-line-break'
glyph; a trailing-spaces break replaces each space with the glyph, so the run
of pilcrows fills the line up to the newline.
If `markdown-ts-hard-line-break-backslash' or
`markdown-ts-hard-line-break-space' are the symbol `hide', hide the
markup entirely.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let* ((node-start (treesit-node-start node))
         (node-end   (treesit-node-end node))
         (text       (treesit-node-text node t))
         (backslash  (and (> (length text) 0) (eq (aref text 0) ?\\)))
         ;; Determine the actual range to fontify and replace.  The
         ;; hard_line_break node may not cover the full run of trailing
         ;; spaces, so walk back from end-of-line ourselves for the
         ;; spaces variant.
         (region     (if backslash
                         (cons node-start
                               (if (and (> node-end node-start)
                                        (eq (char-before node-end) ?\n))
                                   (1- node-end)
                                 node-end))
                       (save-excursion
                         (goto-char node-start)
                         (let ((eol (line-end-position)))
                           (cons (save-excursion
                                   (goto-char eol)
                                   (skip-chars-backward
                                    " " (line-beginning-position))
                                   (point))
                                 eol)))))
         (region-start (car region))
         (region-end   (cdr region))
         (face       (cond
                      ((and backslash markdown-ts-hide-markup)
                       'markdown-ts-hard-line-break-backslash-hidden)
                      (backslash 'markdown-ts-hard-line-break-backslash)
                      (markdown-ts-hide-markup
                       'markdown-ts-hard-line-break-space-hidden)
                      (t 'markdown-ts-hard-line-break-space))))
    (treesit-fontify-with-override region-start region-end face
                                   override start end)
    ;; Always start by clearing any stale `display' property.  We never
    ;; span the trailing newline with `display' (it confuses
    ;; redisplay's cursor placement), and we only paint a single
    ;; combined string onto the first character of the run, leaving
    ;; the rest of the markup alone.
    (remove-text-properties region-start region-end '(display nil))
    (when markdown-ts-hide-markup
      (let* ((spec (if backslash
                       (markdown-ts--resolve-display-value
                        markdown-ts-hard-line-break-backslash)
                     markdown-ts-hard-line-break-space))
             (str (cond
                   ((null spec) nil)
                   ((characterp spec) (char-to-string spec))
                   ((stringp spec) spec)
                   ((functionp spec)
                    (funcall spec (- region-end region-start))))))
        (if (eq spec 'hide)
            (put-text-property region-start region-end
                               'invisible 'markdown-ts--markup)
          (when (and (stringp str)
                     (> (length str) 0)
                     (char-displayable-p (aref str 0)))
            (put-text-property region-start (1+ region-start)
                               'display str)
            ;; For the trailing-spaces variant, hide the remaining
            ;; spaces in the run so the line doesn't end with leftover
            ;; whitespace after the substituted glyph.  Each position
            ;; gets its own empty-string `display' so cursor placement
            ;; stays unambiguous.
            (unless backslash
              (let ((i (1+ region-start)))
                (while (< i region-end)
                  (put-text-property i (1+ i) 'display "")
                  (setq i (1+ i)))))))))))

(defun markdown-ts--fontify-thematic-break (node override start end &rest _)
  "Fontify thematic break NODE and show a line when markup is hidden.
OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (treesit-fontify-with-override node-start node-end
                                   'markdown-ts-thematic-break
                                   override start end)
    (if markdown-ts-hide-markup
        (cond
         ((and (display-supports-face-attributes-p '(:extend t))
               (face-attribute 'markdown-ts-thematic-break
                               :extend nil 'default))
          (put-text-property node-start node-end
                             'display
                             (propertize "\n" 'face '(:extend t :underline t))))
         (t
          (when-let* ((char (markdown-ts--resolve-display-value
                             markdown-ts-thematic-break-character))
                      (_ (char-displayable-p char)))
            (let* ((col (save-excursion (goto-char node-start)
                                        (current-column)))
                   (span-length (max 12 (- (window-body-width) col))))
              (put-text-property node-start node-end
                                 'display
                                 (concat
                                  (make-string span-length char)
                                  "\n"))))))
      (remove-text-properties node-start node-end '(display nil)))))

(defun markdown-ts--fontify-code-block (node _override _start _end &rest _)
  "Fontify code block content NODE with a background overlay.
Use `markdown-ts-code-block-markup-hidden' when markup is hidden,
`markdown-ts-code-block' otherwise.  Use an overlay so the
background layers itself behind language-specific fontification.
The overlay also stores the code block language and mode as
properties `markdown-ts-code-block-language' and
`markdown-ts-code-block-mode', accessible via
`markdown-ts-code-block-language-at'."
  (let* ((node-start (save-excursion
                       (goto-char (treesit-node-start node))
                       (line-beginning-position)))
         (node-end (save-excursion
                     (goto-char (treesit-node-end node))
                     (skip-chars-backward " \t")
                     (point)))
         (face (if markdown-ts-hide-markup
                   'markdown-ts-code-block-markup-hidden
                 'markdown-ts-code-block))
         (lang (markdown-ts--language-at-node node))
         (mode (when lang
                 (markdown-ts--code-block-language-mode lang)))
         (existing (seq-find (lambda (ov)
                               (overlay-get ov 'markdown-ts-code-block))
                             (overlays-at node-start))))
    (if existing
        (progn
          (move-overlay existing node-start node-end)
          (overlay-put existing 'face face)
          (overlay-put existing 'markdown-ts-code-block-language lang)
          (overlay-put existing 'markdown-ts-code-block-mode mode))
      (let ((ov (make-overlay node-start node-end nil t nil)))
        ;; Markers need to be set only once.
        (overlay-put ov 'markdown-ts-code-beg-marker (set-marker (make-marker)
                                                                 node-start))
        (overlay-put ov 'markdown-ts-code-end-marker (set-marker (make-marker)
                                                                 node-end))
        (overlay-put ov 'markdown-ts-code-block t)
        (overlay-put ov 'face face)
        (overlay-put ov 'priority '(nil . 10))
        (overlay-put ov 'markdown-ts-code-block-language lang)
        (overlay-put ov 'markdown-ts-code-block-mode mode)
        (overlay-put ov 'evaporate t)))))

(defun markdown-ts-at-code-block-p (&optional pos)
  "Return non nil if point is in a code block.
If POS is nil, use point."
  (get-char-property (or pos (point)) 'markdown-ts-code-block))

(defun markdown-ts-code-block-language-at (&optional pos)
  "Return the language symbol of the code block at POS.
If POS is nil, use point.  Returns nil if POS is not inside a fenced
code block.  This works regardless of whether a guest tree-sitter parser
is active, since the language is stored on the code block overlay by the
host parser's fontification."
  (get-char-property (or pos (point)) 'markdown-ts-code-block-language))

(defun markdown-ts-code-block-mode-at (&optional pos)
  "Return the major mode for the code block at POS.
If POS is nil, use point.  Returns nil if POS is not inside a fenced
code block or if the language has no recognized mode."
  (when (markdown-ts-at-code-block-p pos)
    (or (get-char-property (or pos (point)) 'markdown-ts-code-block-mode)
        markdown-ts-default-code-block-mode)))

(defun markdown-ts--host-ranges-notifier (ranges _parser)
  "Prune stale code block overlays after the host parser reparses.
RANGES is a list of (START . END) cons cells marking regions where
the markdown parse tree changed.  For each `markdown-ts-code-block'
overlay intersecting a changed range, verify that a containing
`fenced_code_block' node still exists in the host tree.  If not,
the fences were deleted, so delete the overlay to avoid running
commands in a stale block context."
  (dolist (range ranges)
    (let ((beg (car range))
          (end (cdr range)))
      (dolist (ov (overlays-in beg end))
        (when (overlay-get ov 'markdown-ts-code-block)
          (let* ((ov-start (overlay-start ov))
                 (node (and ov-start
                            (treesit-node-at ov-start 'markdown))))
            (unless (and node
                         (treesit-parent-until
                          node "\\`fenced_code_block\\'" t))
              (delete-overlay ov))))))))

;;; Image handling:

(defun markdown-ts--image-alone-on-line-p (node)
  "Return non-nil if image NODE is the only content on its line.
Whitespace, block quote markers (`>'), and list markers
\(`-', `*', `+', `1.') before the image are ignored."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (save-excursion
      (goto-char node-start)
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        ;; Before the image: only block quote markers, whitespace,
        ;; and an optional list marker are allowed.
        (and (string-match-p
              (rx bos
                  (zero-or-more (any "> \t"))
                  (optional (or (seq (any "-*+") " ")
                                (seq (one-or-more digit) (any ".)" ) " ")))
                  eos)
              (buffer-substring-no-properties bol node-start))
             ;; After the image: only trailing whitespace allowed.
             (string-match-p
              (rx bos (zero-or-more (any " \t")) eos)
              (buffer-substring-no-properties node-end eol)))))))

(defun markdown-ts--fontify-image (node _override _start _end &rest _)
  "Show an inline image at NODE.
When `markdown-ts-inline-images' is non-nil, display the image.
If the image link is the only content on its line, display the
image below the link.  If the image is inline within a paragraph,
display it right after the link text.
Remote images are controlled by
`markdown-ts-display-remote-inline-images'."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (eol (save-excursion (goto-char node-end)
                              (line-end-position)))
         (search-end (min (1+ eol) (point-max))))
    ;; Always clear stale overlays for this node's range.
    (dolist (ov (overlays-in node-start search-end))
      (when (and (overlay-get ov 'markdown-ts-image)
                 (>= (overlay-start ov) node-start)
                 (<= (overlay-end ov) search-end))
        (delete-overlay ov)))
    (when (and markdown-ts-inline-images
               (display-images-p)
               ;; Don't create image overlays for nodes inside
               ;; folded (outline-invisible) headings, since the
               ;; images wouldn't be visible and could interfere
               ;; with the folded display.
               (not (markdown-ts--outline-invisible-p node-start)))
      (let* ((dest (treesit-search-subtree node "\\`link_destination\\'"))
             (url (and dest (treesit-node-text dest t)))
             (remotep (and url (string-match-p "\\`https?://" url)))
             (displayable
              (when url
                (if remotep
                    (when (eq markdown-ts-display-remote-inline-images
                              'download)
                      (ignore-errors
                        (require 'url-handlers)
                        (with-work-buffer
                          (set-buffer-multibyte nil)
                          (url-insert-file-contents url)
                          (buffer-string))))
                  (let ((file (expand-file-name url)))
                    (and (not (file-remote-p file))
                         (file-exists-p file)
                         (image-supported-file-p file)
                         file)))))
             (max-w (and displayable
                         (if (eq markdown-ts-image-max-width 'window)
                             (window-body-width nil t)
                           markdown-ts-image-max-width)))
             (img (and max-w
                       (create-image displayable nil remotep
                                     :max-width max-w
                                     :scale 1))))
        (when img
          (let* ((alone (markdown-ts--image-alone-on-line-p node))
                 (str (if alone
                          (concat "\n" (propertize " " 'display img))
                        (propertize " " 'display img)))
                 (ov (make-overlay (1- node-end) node-end nil t nil)))
            (overlay-put ov 'markdown-ts-image t)
            (overlay-put ov 'after-string str)
            (overlay-put ov 'evaporate t)))))))

;;; URL/URI handling:

(defvar markdown-ts--bare-url-regexp goto-address-url-regexp
  "Regexp matching bare URLs not wrapped in angle brackets or link syntax.")

(defvar markdown-ts--bare-email-uri-regexp goto-address-mail-regexp
  "Regexp matching bare email addresses not wrapped in angle brackets.")

(defun markdown-ts--fontify-bare-uri (start end)
  "Fontify bare URL or email URI between START and END.
Skip matches already inside tree-sitter link or autolink nodes."
  (dolist (re (list markdown-ts--bare-url-regexp
                    markdown-ts--bare-email-uri-regexp))
    (goto-char start)
    (while (re-search-forward re end t)
      (let* ((uri-start (match-beginning 0))
             (uri-end (match-end 0))
             (uri (match-string 0))
             (node (treesit-node-at uri-start 'markdown-inline))
             (parent (and node (treesit-node-parent node)))
             (parent-type (and parent (treesit-node-type parent))))
        (unless (or (member parent-type
                            '("inline_link" "full_reference_link"
                              "collapsed_reference_link" "shortcut_link"
                              "image" "uri_autolink" "email_autolink"
                              "link_destination"))
                    (member (and node (treesit-node-type node))
                            '("uri_autolink" "email_autolink"
                              "link_destination" "code_span"
                              "code_fence_content" "info_string"))
                    (get-text-property uri-start 'button))
          (markdown-ts--make-link-button
           uri-start uri-end
           (if (eq uri-start 0)
               uri
             (concat "mailto:" uri))))))))

;;; treesit settings:

(defvar markdown-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)

   :language 'markdown-inline
   :override t
   :feature 'delimiter
   '((inline_link [ "[" "]" "(" ")" ] @markdown-ts--fontify-delimiter)
     (full_reference_link [ "[" "]" ] @markdown-ts--fontify-delimiter)
     (collapsed_reference_link [ "[" "]" ] @markdown-ts--fontify-delimiter)
     (shortcut_link [ "[" "]" ] @markdown-ts--fontify-delimiter)
     (image [ "!" "[" "]" "(" ")" ] @markdown-ts--fontify-delimiter))

   :language 'markdown
   :feature 'heading
   '(((atx_heading)    @markdown-ts--fontify-atx-heading)
     ((setext_heading) @markdown-ts--fontify-setext-heading))

   :language 'markdown
   :feature 'heading
   :override 'prepend
   '((atx_h1_marker) @markdown-ts--fontify-atx-delimiter
     (atx_h2_marker) @markdown-ts--fontify-atx-delimiter
     (atx_h3_marker) @markdown-ts--fontify-atx-delimiter
     (atx_h4_marker) @markdown-ts--fontify-atx-delimiter
     (atx_h5_marker) @markdown-ts--fontify-atx-delimiter
     (atx_h6_marker) @markdown-ts--fontify-atx-delimiter
     (setext_h1_underline) @markdown-ts--fontify-delimiter
     (setext_h2_underline) @markdown-ts--fontify-delimiter)

   :language 'markdown
   :feature 'paragraph
   '(((thematic_break) @markdown-ts--fontify-thematic-break)
     ((html_block) @markdown-ts-html-block)
     ((indented_code_block) @markdown-ts-indented-code-block)
     (list_item (list_marker_star) @markdown-ts--fontify-unordered-list-marker)
     (list_item (list_marker_plus) @markdown-ts--fontify-unordered-list-marker)
     (list_item (list_marker_minus) @markdown-ts--fontify-unordered-list-marker)
     (list_item (list_marker_dot) @markdown-ts-list-marker)
     (list_item (list_marker_parenthesis) @markdown-ts-list-marker)
     (list_item (task_list_marker_unchecked) @markdown-ts--fontify-checkbox)
     (list_item (task_list_marker_checked) @markdown-ts--fontify-checkbox)
     (link_reference_definition
      (link_label) @markdown-ts--fontify-link-ref-label)
     (link_reference_definition
      (link_destination) @markdown-ts--fontify-link-ref-destination)
     (link_reference_definition
      (link_title) @markdown-ts-link-destination))

   :language 'markdown
   :feature 'paragraph
   :override 'prepend
   '((pipe_table_header "|" @markdown-ts-table-header)
     (pipe_table_header (pipe_table_cell) @markdown-ts-table-header)
     (pipe_table_row "|" @markdown-ts-table-delimiter-cell)
     (pipe_table_row (pipe_table_cell) @markdown-ts-table-cell)
     (pipe_table_delimiter_row "|" @markdown-ts-table-delimiter-cell)
     ((pipe_table) @markdown-ts-table)
     (pipe_table_delimiter_row (pipe_table_delimiter_cell)
                               @markdown-ts-table-delimiter-cell))

   :language 'markdown
   :feature 'paragraph
   :override 'prepend
   '((block_quote) @markdown-ts-block-quote
     (block_quote_marker) @markdown-ts--fontify-delimiter
     (fenced_code_block_delimiter) @markdown-ts--fontify-delimiter
     (fenced_code_block
      (info_string (language) @markdown-ts-language-keyword)
      @markdown-ts--fontify-delimiter)
     (block_quote
      (block_quote_marker) @markdown-ts--fontify-delimiter
      (paragraph (inline (block_continuation) @markdown-ts--fontify-delimiter))))

   :language 'markdown
   :feature 'paragraph
   :override 'append
   '((fenced_code_block
      (info_string (language))
      (code_fence_content) @markdown-ts--fontify-non-ts-code-block))

   :language 'markdown
   :feature 'paragraph
   :override 'append
   '((fenced_code_block (code_fence_content) @markdown-ts--fontify-code-block))

   :language 'markdown-inline
   :override 'prepend
   :feature 'paragraph-inline
   '(((code_span) @markdown-ts-code-span)
     ((code_span_delimiter) @markdown-ts--fontify-delimiter))

   :language 'markdown-inline
   :override 'append
   :feature 'paragraph-inline
   '(((link_destination) @markdown-ts--fontify-link-destination)
     ((emphasis) @markdown-ts-emphasis)
     ((strong_emphasis) @markdown-ts-bold)
     ((strikethrough) @markdown-ts-strikethrough)
     (inline_link (link_text) @markdown-ts--fontify-link-node)
     (full_reference_link (link_text) @markdown-ts--fontify-link-node)
     (full_reference_link (link_label) @markdown-ts--fontify-link-node)
     (collapsed_reference_link (link_text) @markdown-ts--fontify-link-node)
     (shortcut_link (link_text) @markdown-ts--fontify-link-node)
     (image (image_description) @markdown-ts--fontify-link-node)
     ((uri_autolink) @markdown-ts--fontify-autolink)
     ((email_autolink) @markdown-ts--fontify-autolink)
     (inline_link (link_title) @markdown-ts-link-destination)
     ((backslash_escape) @markdown-ts--fontify-backslash-escape)
     ((entity_reference) @markdown-ts--fontify-entity)
     ((numeric_character_reference) @markdown-ts--fontify-entity)
     ((html_tag) @markdown-ts-html-tag)
     ((hard_line_break) @markdown-ts--fontify-hard-line-break)
     ((latex_block) @markdown-ts--fontify-latex-block))

   :language 'markdown-inline
   :feature 'paragraph-inline
   :override 'append
   '((emphasis_delimiter) @markdown-ts--fontify-delimiter)

   :language 'markdown-inline
   :feature 'image-preview
   :override 'append
   '((image) @markdown-ts--fontify-image)))

;;; Imenu:

(defun markdown-ts--imenu-heading-node-p (node)
  "Check if NODE is a valid entry to imenu."
  (and (equal (treesit-node-type node) "inline")
       (equal (treesit-node-type (treesit-node-parent node))
              "atx_heading")))

(defun markdown-ts--imenu-heading-name-function (node)
  "Return an imenu entry if NODE is a valid header."
  (let ((name (treesit-node-text node)))
    (if (markdown-ts--imenu-heading-node-p node)
        (car (split-string (treesit-node-text
                           (treesit-node-parent node))
                          "\n" t " "))
      name)))

(defun markdown-ts--imenu-code-block-node-p (node)
  "Check if NODE is an info_string with descriptive text after the language."
  (and (equal (treesit-node-type node) "info_string")
       (when-let* ((lang-node (treesit-search-subtree node "language"))
                   ((< (treesit-node-end lang-node)
                       (treesit-node-end node))))
         (not (string-blank-p
               (buffer-substring-no-properties
                (treesit-node-end lang-node)
                (treesit-node-end node)))))))

(defun markdown-ts--imenu-code-block-name-function (node)
  "Return an imenu entry name for a code block info_string NODE.
Uses the descriptive text after the language name."
  (when-let* ((lang-node (treesit-search-subtree node "language"))
              (desc (string-trim
                     (buffer-substring-no-properties
                      (treesit-node-end lang-node)
                      (treesit-node-end node)))))
    (format "%s: %s" (treesit-node-text lang-node) desc)))

(defun markdown-ts--outline-predicate (node)
  "Match NODE if it is a hierarchical section that has a heading."
  (and (equal (treesit-node-type node) "section")
       (when-let* ((child (treesit-node-child node 0)))
         (equal (treesit-node-type child) "atx_heading"))))

;;; Heading manipulation:

(defconst markdown-ts--parser-heading-max-level 6
  "Maximum ATX heading level in Markdown (h1-h6).
Defined by the CommonMark spec and the tree-sitter grammar.")

(defun markdown-ts--heading-at-point ()
  "Return the atx_heading node at or before point, or nil."
  (when-let* ((node (treesit-node-at (point) 'markdown)))
    (if (equal (treesit-node-type node) "atx_heading")
        node
      (treesit-parent-until
       node (lambda (n) (equal (treesit-node-type n) "atx_heading"))))))

(defun markdown-ts--heading-level (heading)
  "Return HEADING level (range 1 to `markdown-ts--parser-heading-max-level')."
  (let ((marker (treesit-node-child heading 0)))
    (length (treesit-node-text marker t))))

(defun markdown-ts--section-at-point ()
  "Return the section node containing point."
  (when-let* ((node (treesit-node-at (point) 'markdown)))
    (treesit-parent-until
     node (lambda (n) (equal (treesit-node-type n) "section")))))

(defun markdown-ts-promote ()
  "Promote the heading or list item at point.
For headings, decrease the level (e.g., ## to #).
For list items, decrease nesting (dedent).
With `transient-mark-mode' on and mark active, promote all
headings in the region."
  (interactive)
  (cond
   ((and transient-mark-mode mark-active)
    (markdown-ts--promote-or-demote-region -1))
   ((markdown-ts--list-item-at-point)
    (markdown-ts--list-promote-or-demote nil))
   (t (markdown-ts--promote-or-demote -1))))

(defun markdown-ts-demote ()
  "Demote the heading or list item at point.
For headings, increase the level (e.g., # to ##).
For list items, increase nesting (indent).
With `transient-mark-mode' on and mark active, demote all headings
in the region."
  (interactive)
  (cond
   ((and transient-mark-mode mark-active)
    (markdown-ts--promote-or-demote-region 1))
   ((markdown-ts--list-item-at-point)
    (markdown-ts--list-promote-or-demote t))
   (t (markdown-ts--promote-or-demote 1))))

(defun markdown-ts--promote-or-demote (delta)
  "Change the heading at point by DELTA levels.
Negative DELTA promotes (fewer #), positive demotes (more #).
If the heading has an optional trailing closing-`#' sequence, that
sequence is resized to match the new level."
  (when-let* ((heading (markdown-ts--heading-at-point))
              (marker (treesit-node-child heading 0))
              (level (length (treesit-node-text marker t)))
              (new-level (+ level delta)))
    (when (and (>= new-level 1)
               (<= new-level markdown-ts--parser-heading-max-level))
      (let* ((opener-beg (treesit-node-start marker))
             (opener-end (treesit-node-end marker))
             (line-end (save-excursion
                         (goto-char opener-end)
                         (line-end-position)))
             ;; Detect a CommonMark closing-`#' sequence: optional
             ;; trailing whitespace, a run of `#', and at least one
             ;; space/tab between it and the heading text.
             (closer (save-excursion
                       (goto-char line-end)
                       (skip-chars-backward " \t" opener-end)
                       (let ((end (point)))
                         (skip-chars-backward "#" opener-end)
                         (let ((beg (point)))
                           (when (and (< beg end)
                                      (> beg opener-end)
                                      (memq (char-before beg)
                                            '(?\s ?\t)))
                             (cons beg end)))))))
        (save-excursion
          ;; Resize the closer first so it doesn't shift the opener.
          (when closer
            (delete-region (car closer) (cdr closer))
            (goto-char (car closer))
            (insert (make-string new-level ?#)))
          (delete-region opener-beg opener-end)
          (goto-char opener-beg)
          (insert (make-string new-level ?#)))))))

(defun markdown-ts--promote-or-demote-region (delta)
  "Change all headings in the active region by DELTA levels.
Negative DELTA promotes (fewer #), positive demotes (more #)."
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when (markdown-ts--heading-at-point)
          (markdown-ts--promote-or-demote delta))
        (forward-line 1)))))

(defun markdown-ts-move-subtree-up ()
  "Move the current section or list item up past the previous sibling."
  (interactive)
  (if (markdown-ts--list-item-at-point)
      (markdown-ts--list-move t)
    (markdown-ts--move-subtree-up-or-down t)))

(defun markdown-ts-move-subtree-down ()
  "Move the current section or list item down past the next sibling."
  (interactive)
  (if (markdown-ts--list-item-at-point)
      (markdown-ts--list-move nil)
    (markdown-ts--move-subtree-up-or-down nil)))

(defun markdown-ts--section-folded-p (section)
  "Return non-nil if SECTION's heading is folded.
Checks whether the end of the heading line has an outline
invisible overlay."
  (let ((heading (treesit-node-child section 0)))
    (when (and heading
               (equal (treesit-node-type heading) "atx_heading"))
      (save-excursion
        (goto-char (treesit-node-start heading))
        (outline-invisible-p (pos-eol))))))

(defun markdown-ts--move-subtree-up-or-down (up)
  "Move the current section subtree.
If UP is non-nil, move past the previous sibling; otherwise move down.
Preserves the folding state of both sections."
  (when-let* ((section (markdown-ts--section-at-point))
              (sibling (if up
                           (treesit-node-prev-sibling section)
                         (treesit-node-next-sibling section))))
    (when (equal (treesit-node-type sibling) "section")
      (let* ((first (if up sibling section))
             (second (if up section sibling))
             (first-folded (markdown-ts--section-folded-p first))
             (second-folded (markdown-ts--section-folded-p second))
             (first-start (treesit-node-start first))
             (second-start (treesit-node-start second))
             (second-end (treesit-node-end second))
             ;; Extract separator between items and trailing whitespace.
             (first-raw (buffer-substring first-start second-start))
             (first-text (string-trim-right first-raw))
             (separator (substring first-raw (length first-text)))
             (second-raw (buffer-substring second-start second-end))
             (second-text (string-trim-right second-raw))
             (trailing (substring second-raw (length second-text)))
             (line-offset (- (line-number-at-pos (point))
                             (line-number-at-pos (treesit-node-start section)))))
        (delete-region first-start second-end)
        (goto-char first-start)
        (insert second-text separator first-text trailing)
        ;; Restore folding state
        (save-excursion
          (goto-char first-start)
          (when (outline-on-heading-p)
            (if second-folded
                (outline-hide-subtree)
              (outline-show-subtree)))
          (outline-next-heading)
          (when (outline-on-heading-p)
            (if first-folded
                (outline-hide-subtree)
              (outline-show-subtree))))
        (goto-char (if up
                       first-start
                     (+ first-start (length second-text)
                        (length separator))))
        (with-suppressed-warnings ((interactive-only next-line))
          (next-line line-offset))))))

;;; List manipulation:

(defun markdown-ts-toggle-checkbox ()
  "Toggle the task list checkbox on the current line.
Switches between `[ ]' and `[x]'."
  (interactive)
  (when-let* ((pos (save-excursion (back-to-indentation) (point)))
              (node (treesit-node-at pos 'markdown))
              (item (treesit-parent-until
                     node (lambda (n)
                            (equal (treesit-node-type n) "list_item")))))
    (when-let* ((marker (seq-find
                         (lambda (child)
                           (member (treesit-node-type child)
                                   '("task_list_marker_checked"
                                     "task_list_marker_unchecked")))
                         (treesit-node-children item))))
      (let ((beg (treesit-node-start marker))
            (end (treesit-node-end marker))
            (checked (equal (treesit-node-type marker)
                            "task_list_marker_checked")))
        (save-excursion
          (goto-char beg)
          (delete-region beg end)
          (insert (if checked "[ ]" "[x]")))))))

(defun markdown-ts--list-item-at-point ()
  "Return the innermost list_item node containing point, or nil.
Uses the position after `back-to-indentation' so that point in
a line's leading whitespace resolves to the item on that line,
not to a preceding item whose node spans the whitespace.
Inside block quotes, also try from the content position past
the `>' markers."
  (when-let* ((pos (save-excursion (back-to-indentation) (point)))
              (node (treesit-node-at pos 'markdown))
              (bol (line-beginning-position))
              (eol (line-end-position)))
    (or (let ((item (treesit-parent-until
                     node (lambda (n)
                            (equal (treesit-node-type n) "list_item")))))
          ;; Verify the current line is within the item's range.
          ;; `treesit-node-at' can return a node inside a list_item
          ;; even when point is on a preceding line.
          (when (and item
                     (<= (treesit-node-start item) eol)
                     (>= (treesit-node-end item) bol))
            item))
        ;; When back-to-indentation lands on block quote markers,
        ;; skip past them and try from the content position.
        (let ((content-pos (save-excursion
                             (beginning-of-line)
                             (skip-chars-forward "> \t")
                             (point))))
          (when (> content-pos pos)
            (when-let* ((cnode (treesit-node-at content-pos 'markdown)))
              (let ((item (treesit-parent-until
                           cnode
                           (lambda (n)
                             (equal (treesit-node-type n) "list_item")))))
                (when (and item
                           (<= (treesit-node-start item) eol)
                           (>= (treesit-node-end item) bol))
                  item))))))))

(defun markdown-ts--list-marker-width (item)
  "Return the width of ITEM's list marker including trailing space."
  (let ((marker (treesit-node-child item 0)))
    (- (treesit-node-end marker) (treesit-node-start marker))))

(defun markdown-ts--list-item-region (item)
  "Return the (BEG . END) region for ITEM, clamped to full lines.
BEG starts at the beginning of the line containing the item.
END is clamped so it does not extend into the next line's
indentation, which tree-sitter may include in the node."
  (let ((beg (save-excursion
               (goto-char (treesit-node-start item))
               (line-beginning-position)))
        (end (save-excursion
               (goto-char (treesit-node-end item))
               (if (bolp) (point) (line-beginning-position)))))
    (cons beg end)))

(defun markdown-ts--list-ordered-item-p (item)
  "Return non-nil if ITEM is an ordered (numbered) list item."
  (let ((marker (treesit-node-child item 0)))
    (member (treesit-node-type marker)
            '("list_marker_dot" "list_marker_parenthesis"))))

(defun markdown-ts--list-promote-or-demote (demote)
  "Change nesting of the list item at point.
If DEMOTE is non-nil, demote (indent); otherwise promote (dedent).
Ordered (numbered) list items are skipped because the grammar does
not support nesting them by indentation."
  (when-let* ((item (markdown-ts--list-item-at-point))
              (region (markdown-ts--list-item-region item))
              (beg (car region))
              (end (cdr region)))
    (when (markdown-ts--list-ordered-item-p item)
      (user-error "Ordered list items cannot be nested (grammar limitation)"))
    (if demote
        ;; Demote: indent by marker width, only if there is a previous
        ;; sibling to nest under (like org-mode).
        (when (treesit-node-prev-sibling item)
          (indent-rigidly beg end (markdown-ts--list-marker-width item)))
      ;; Promote: dedent to grandparent level.
      (let* ((parent-list (treesit-node-parent item))
             (grandparent (and parent-list (treesit-node-parent parent-list)))
             (item-col (save-excursion
                         (goto-char beg) (current-indentation))))
        (when (and grandparent
                   (equal (treesit-node-type grandparent) "list_item")
                   (> item-col 0))
          (let ((gp-col (save-excursion
                          (goto-char (treesit-node-start grandparent))
                          (current-indentation))))
            (indent-rigidly beg end (- gp-col item-col))))))))

(defun markdown-ts--list-node-bol (node)
  "Return the beginning of the line containing NODE's start."
  (save-excursion
    (goto-char (treesit-node-start node))
    (line-beginning-position)))

(defun markdown-ts--list-move (up)
  "Move the list item at point.
If UP is non-nil, move past the previous sibling; otherwise move down."
  (when-let* ((item (markdown-ts--list-item-at-point))
              (sibling (if up
                           (treesit-node-prev-sibling item)
                         (treesit-node-next-sibling item))))
    (when (equal (treesit-node-type sibling) "list_item")
      (let* ((first (if up sibling item))
             (second (if up item sibling))
             (first-start (markdown-ts--list-node-bol first))
             (second-start (markdown-ts--list-node-bol second))
             (second-end (treesit-node-end second))
             ;; Extract separator between items and trailing whitespace.
             (first-raw (buffer-substring first-start second-start))
             (first-text (string-trim-right first-raw))
             (separator (substring first-raw (length first-text)))
             (second-raw (buffer-substring second-start second-end))
             (second-text (string-trim-right second-raw))
             (trailing (substring second-raw (length second-text)))
             (line-offset (- (line-number-at-pos (point))
                             (line-number-at-pos
                              (treesit-node-start item)))))
        (delete-region first-start second-end)
        (goto-char first-start)
        (insert second-text separator first-text trailing)
        (goto-char (if up
                       first-start
                     (+ first-start (length second-text)
                        (length separator))))
        (with-suppressed-warnings ((interactive-only next-line))
          (next-line line-offset))))))

(defun markdown-ts-renumber-list (&optional start)
  "Renumber the ordered list at point.
Numbers are assigned sequentially starting from the first item's
number.  With a prefix argument START, start numbering from that
value instead.  Does nothing for unordered lists."
  (interactive "P")
  (when-let* ((item (markdown-ts--list-item-at-point))
              (list-node (treesit-node-parent item)))
    (when (equal (treesit-node-type list-node) "list")
      (let* ((children (treesit-node-children list-node))
             (items (seq-filter (lambda (n)
                                  (equal (treesit-node-type n) "list_item"))
                                children))
             (first-marker (treesit-node-child (car items) 0))
             (marker-type (treesit-node-type first-marker)))
        (when (member marker-type '("list_marker_dot" "list_marker_parenthesis"))
          (let* ((base (if start
                           (prefix-numeric-value start)
                         (string-to-number
                          (treesit-node-text first-marker t))))
                 (count (length items))
                 ;; Start from the last number and work backwards so
                 ;; that earlier buffer positions remain valid.
                 (num (+ base count -1)))
            (dolist (it (reverse items))
              (let* ((marker (treesit-node-child it 0))
                     (marker-end (treesit-node-end marker))
                     (bol (save-excursion
                            (goto-char (treesit-node-start marker))
                            (line-beginning-position)))
                     ;; Full text from bol to end of marker includes
                     ;; any leading whitespace, the number, and the
                     ;; separator (e.g., ". " or ".\t").
                     (full-text (buffer-substring-no-properties
                                 bol marker-end))
                     (indent-str
                      (if (string-match
                           "\\`\\([[:blank:]]*\\)[0-9]" full-text)
                          (match-string 1 full-text)
                        ""))
                     (suffix-str
                      (if (string-match
                           "\\`[[:blank:]]*[0-9]+\\(.*\\)\\'" full-text)
                          (match-string 1 full-text)
                        ". "))
                     (new-text (concat indent-str
                                       (number-to-string num)
                                       suffix-str)))
                (save-excursion
                  (delete-region bol marker-end)
                  (goto-char bol)
                  (insert new-text)))
              (cl-decf num))
            (message "Renumbered %d items" count)))))))

(defun markdown-ts--list-item-new-marker (item)
  "Return the marker string for a new item following ITEM.
For unordered items, reuse the same marker character.
For ordered items, increment the number."
  (let* ((marker (treesit-node-child item 0))
         (type (treesit-node-type marker))
         (indent (save-excursion
                   (goto-char (treesit-node-start item))
                   (current-indentation)))
         (prefix (make-string indent ?\s)))
    (pcase type
      ("list_marker_minus" (concat prefix "- "))
      ("list_marker_plus"  (concat prefix "+ "))
      ("list_marker_star"  (concat prefix "* "))
      ("list_marker_dot"
       (let ((num (string-to-number (treesit-node-text marker t))))
         (concat prefix (number-to-string (1+ num)) ". ")))
      ("list_marker_parenthesis"
       (let ((num (string-to-number (treesit-node-text marker t))))
         (concat prefix (number-to-string (1+ num)) ") ")))
      (_ (concat prefix "- ")))))

(defun markdown-ts--new-marker-for-line (item)
  "Return the marker string for a new item following ITEM.
When the current line has a list marker pattern at a different
indentation than ITEM (e.g., a nested marker that the grammar did
not recognize as a list_item), use the current line's marker.
Otherwise, fall back to `markdown-ts--list-item-new-marker'."
  (let ((marker-node (treesit-node-child item 0)))
    (if (= (line-number-at-pos)
            (line-number-at-pos (treesit-node-start marker-node)))
        ;; Item's marker is on the current line: use it normally.
        (markdown-ts--list-item-new-marker item)
      ;; Item's marker is on a different line.  The parser may have
      ;; failed to recognize a nested marker (e.g., a lone `  - '
      ;; parsed as a setext heading).  Read the current line.
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (if (string-match
             "\\`\\([[:blank:]]*\\)\\([-*+] \\|\\([0-9]+\\)\\([.)] \\)\\)"
             line)
            (let ((indent (match-string 1 line))
                  (num (match-string 3 line))
                  (suffix (match-string 4 line))
                  (unordered (match-string 2 line)))
              (if num
                  (concat indent (number-to-string
                                  (1+ (string-to-number num)))
                          suffix)
                (concat indent unordered)))
          ;; No marker pattern on current line: use item's marker.
          (markdown-ts--list-item-new-marker item))))))

(defun markdown-ts--line-block-quote-depth (&optional pos)
  "Count the number of `>' block quote markers on the line at POS."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (let ((count 0))
      (while (and (not (eolp))
                  (memq (char-after) '(?> ?\s ?\t)))
        (when (eq (char-after) ?>)
          (setq count (1+ count)))
        (forward-char 1))
      count)))

(defun markdown-ts-newline ()
  "Insert a newline, continuing the current context.
Inside a list item, the new line is indented to the item's text
column so the paragraph continues.  A second RET on a blank
continuation line removes the indentation, dropping point to
column 0 so the tree-sitter grammar ends the list.
Inside a block quote, the new line includes the quote prefix.
Otherwise, insert a plain newline."
  (interactive)
  (let* ((node (treesit-node-at
                (save-excursion (back-to-indentation) (point))
                'markdown))
         ;; Tree-sitter may report a node inside a block_quote even
         ;; when point is on a blank line past the quote (the node
         ;; span can extend beyond the `>' lines).  Only treat the
         ;; line as inside a block quote if it actually starts with
         ;; a `>' marker.
         (in-bq (and (treesit-parent-until node "\\`block_quote\\'")
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p "[ \t]*>"))))
         (item (markdown-ts--list-item-at-point)))
    (cond
     (item
      (let* ((col (markdown-ts--list-item-text-column item))
             (bq-prefix (when in-bq (markdown-ts--block-quote-prefix)))
             ;; `col' is the absolute column of the item's text,
             ;; which inside a block quote includes the "> " prefix
             ;; width.  Since we insert `bq-prefix' separately, we
             ;; must subtract its length to avoid doubling it.
             (indent (- col (length (or bq-prefix ""))))
             ;; The current line is "blank" if it contains only
             ;; whitespace (or quote prefix + whitespace) and no
             ;; actual text from a previous RET's continuation indent.
             (blank-line-p (save-excursion
                             (beginning-of-line)
                             (looking-at-p
                              "^[> \t]*$")))
             ;; Point is before the item's text (at or before the
             ;; marker).  A plain newline is appropriate here, adding
             ;; continuation indent would shift the existing content
             ;; to the right.
             (before-text-p (<= (current-column) col)))
        (cond
         (blank-line-p
          ;; The user pressed RET on an already-blank continuation
          ;; line.  Instead of adding yet another indented blank
          ;; line, remove the indentation and insert a plain
          ;; newline.  This drops point to column 0, which is the
          ;; only way the tree-sitter markdown grammar ends a list:
          ;; blank lines alone do NOT end a list, only
          ;; non-indented content does.  So this gives the user a
          ;; natural "RET RET to exit the list" workflow.
          (delete-region (line-beginning-position) (line-end-position))
          (newline))
         (before-text-p
          ;; Point is on or before the list marker; just insert a
          ;; plain newline so the item is pushed down unchanged.
          (newline))
         (t
          (delete-horizontal-space)
          (newline)
          (when bq-prefix (insert bq-prefix))
          (insert (make-string indent ?\s))))))
     (in-bq
      (let ((bq-prefix (markdown-ts--block-quote-prefix))
            ;; A line with only quote markers and whitespace (e.g.,
            ;; "> ") is "blank" inside the quote.  RET here exits the
            ;; quote, same as RET on a blank continuation line exits a
            ;; list: remove the prefix and drop to column 0.
            (blank-line-p (save-excursion
                            (beginning-of-line)
                            (looking-at-p "^[> \t]*$"))))
        (if blank-line-p
            (progn
              (delete-region (line-beginning-position) (line-end-position))
              (newline))
          (delete-horizontal-space)
          (newline)
          (insert bq-prefix))))
     ;; Default: plain newline.  This also handles empty list markers
     ;; (e.g., "- " with no text) that the grammar parses as ERROR
     ;; nodes rather than list_item nodes.
     ;;
     ;; When the current line is blank (whitespace only), it is a
     ;; continuation-indent line left by a previous RET that
     ;; tree-sitter no longer considers part of a list_item.  Clear
     ;; the whitespace so no trailing spaces remain, and insert a
     ;; plain newline at column 0 to exit the list context.
     (t
      (when (save-excursion
              (beginning-of-line)
              (looking-at-p "^[ \t]+$"))
        (delete-region (line-beginning-position) (line-end-position)))
      (newline)))))

(defun markdown-ts-insert-list-item ()
  "Insert a new list item, splitting text at point.
Text after point moves to the new item.  At the beginning of a line,
an empty item is inserted above and the current content is pushed down.
Inside a block quote, the new line includes the quote prefix.
For ordered lists, the number is incremented.
When not inside a list, fall back to `markdown-ts-newline'."
  (interactive)
  (let* ((node (treesit-node-at
                (save-excursion (back-to-indentation) (point))
                'markdown))
         (in-bq (and (treesit-parent-until node "\\`block_quote\\'")
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p "[ \t]*>"))))
         (bq-prefix (when in-bq (markdown-ts--block-quote-prefix)))
         (item (markdown-ts--list-item-at-point)))
    (cond
     ((and item
           (or (not in-bq)
               (>= (markdown-ts--line-block-quote-depth)
                   (markdown-ts--line-block-quote-depth
                    (treesit-node-start (treesit-node-child item 0))))))
      (let* ((new-marker (markdown-ts--new-marker-for-line item))
             (at-bol (<= (current-column)
                         (save-excursion
                           (back-to-indentation)
                           (current-column))))
             (tail (when (and (not at-bol)
                              (not (looking-at-p "[ \t]*$")))
                     (delete-and-extract-region
                      (point) (line-end-position)))))
        (if at-bol
            ;; At BOL: insert empty item above, push current line down.
            (progn
              (beginning-of-line)
              (when bq-prefix (insert bq-prefix))
              (insert new-marker "\n")
              (back-to-indentation))
          ;; Mid-line or EOL: new item below with tail text.
          (delete-horizontal-space)
          (newline)
          (when bq-prefix (insert bq-prefix))
          (insert new-marker)
          (when tail (save-excursion (insert (string-trim-left tail)))))))

     ;; Not in a list: fall back to newline behavior.
     (t (markdown-ts-newline)))))

;;; Filling:

;; TODO: Remove the pipe_table block if we implement pipe_table fill.
(defconst markdown-ts--fill-unfillable-block-query
  '([(fenced_code_block) (indented_code_block) (pipe_table)] @block)
  "Tree-sitter query matching blocks where filling is inhibited.")

(defun markdown-ts--list-item-text-column (item)
  "Return the column where the text of list ITEM starts.
This is the column of the first paragraph node inside ITEM, or
the width of the list marker if no paragraph is found."
  (let ((paragraph (treesit-search-subtree item "\\`paragraph\\'")))
    (if paragraph
        (save-excursion
          (goto-char (treesit-node-start paragraph))
          (current-column))
      (let ((marker (treesit-node-child item 0)))
        (- (treesit-node-end marker)
           (treesit-node-start marker))))))

(defun markdown-ts--fill-list-item (item justify)
  "Fill the contents of list ITEM, preserving its indentation.
Narrows to ITEM's own paragraph content (excluding nested lists)
and sets `fill-prefix' to align continuation lines with the start
of the item's text.  JUSTIFY is as in `fill-paragraph'."
  (let* ((fill-prefix (make-string (markdown-ts--list-item-text-column item)
                                   ?\s))
         ;; Find the item's own paragraph (first paragraph child).
         ;; If the item has nested lists, narrow to just the
         ;; paragraph so we don't merge nested items.
         (para (treesit-search-subtree item "\\`paragraph\\'" nil nil 1))
         (beg (treesit-node-start item))
         (end (if para
                  (treesit-node-end para)
                (treesit-node-end item)))
         ;; Use default paragraph motion inside the narrowed region
         ;; so that the inner `fill-paragraph' does not re-enter our
         ;; custom paragraph-finding logic.
         (fill-forward-paragraph-function #'forward-paragraph))
    (save-restriction
      (narrow-to-region beg end)
      (fill-paragraph justify))
    ;; Return non-nil to signal that filling has been handled,
    ;; as per the contract of `fill-paragraph-function'.
    t))

(defun markdown-ts--adaptive-fill ()
  "Return the fill prefix for the current line in Markdown.
When inside a list item, return spaces matching the column where
the item's text starts."
  (and-let* ((node (treesit-node-at
                    (save-excursion (back-to-indentation) (point))
                    'markdown))
             (item (treesit-parent-until node "\\`list_item\\'")))
    (make-string (markdown-ts--list-item-text-column item) ?\s)))

(defun markdown-ts--fill-forward-paragraph (arg)
  "Move forward by ARG paragraphs, respecting Markdown structure.
List items are treated as individual paragraphs.  Blocks matched
by `markdown-ts--fill-unfillable-block-query' are skipped: both
forward and backward motion move to the end of the block, so
`fill-region' sees a zero-length region and leaves the block
unfilled."
  (let ((direction (if (> arg 0) 1 -1))
        (count (abs arg))
        (moved 0))
    (dotimes (_ count)
      ;; For backward motion, skip back over whitespace to find
      ;; the item we are leaving, not the next one.
      (let* ((pos (if (< direction 0)
                      (save-excursion
                        (skip-chars-backward " \t\n")
                        (max (point-min) (1- (point))))
                    (point)))
             (block (car (treesit-query-capture
                          (treesit-buffer-root-node 'markdown)
                          markdown-ts--fill-unfillable-block-query
                          pos (1+ pos))))
             (indented-pos (save-excursion
                             (goto-char pos)
                             (back-to-indentation)
                             (point)))
             (node (treesit-node-at indented-pos 'markdown))
             (item (treesit-parent-until node "\\`list_item\\'")))
        ;; When moving forward from whitespace between list items,
        ;; skip to the next non-blank position and check again.
        (when (and (not item) (not block) (> direction 0))
          (let ((next-pos (save-excursion
                            (skip-chars-forward " \t\n")
                            (point))))
            (setq node (treesit-node-at next-pos 'markdown))
            (setq item (treesit-parent-until node "\\`list_item\\'"))))
        (cond
         ;; Inside an unfillable block: skip over it entirely.
         (block
          (goto-char (treesit-node-end (cdr block)))
          (setq moved (1+ moved)))
         ;; Inside a list item: treat as paragraph boundary.
         (item
          (if (> direction 0)
              (goto-char (treesit-node-end item))
            (goto-char (treesit-node-start item)))
          (setq moved (1+ moved)))
         ;; Default: use standard paragraph motion.
         (t
          (forward-paragraph direction)
          (setq moved (1+ moved))))))
    ;; Return the number of paragraphs left to move (0 = all done).
    (- count moved)))

(defun markdown-ts--fill-paragraph (&optional justify)
  "Fill the current paragraph, respecting Markdown block structure.
This function prevents filling inside blocks matched by
`markdown-ts--fill-unfillable-block-query', and fills within list
items without merging adjacent items.  JUSTIFY is as in
`fill-paragraph'."
  (cond*
   ;; Don't fill inside unfillable blocks.  Use a query against the
   ;; root node because `treesit-node-at' may not return a node
   ;; inside the block when point is on anonymous (unnamed) text.
   ((treesit-query-capture
     (treesit-buffer-root-node 'markdown)
     markdown-ts--fill-unfillable-block-query
     (point) (min (1+ (point)) (point-max)))
    t)
   ;; Fill within the enclosing list item.  Use
   ;; `markdown-ts--list-item-at-point' which handles block quote
   ;; markers.  When the list item is inside a block quote, delegate
   ;; to the block quote filler which handles `> ' prefixes correctly.
   ((bind-and* (item (markdown-ts--list-item-at-point)))
    (if (treesit-parent-until item "\\`block_quote\\'")
        (markdown-ts--fill-block-quote justify)
      (markdown-ts--fill-list-item item justify)))
   ;; Point is on a blank line before a list.  If the next
   ;; non-whitespace position falls inside a list item, fill that item.
   ((save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*$"))
    (and-let* ((next-pos (save-excursion
                           (skip-chars-forward " \t\n")
                           (point)))
               (next-node (treesit-node-at next-pos 'markdown))
               (item (treesit-parent-until
                      next-node "\\`list_item\\'")))
      (markdown-ts--fill-list-item item justify)))
   ;; Fill within a block quote.  Narrow to the paragraph node at
   ;; point's nesting level so lines with different `> ' depths
   ;; are not merged.
   ((treesit-parent-until
     (treesit-node-at
      (save-excursion (back-to-indentation) (point))
      'markdown)
     "\\`block_quote\\'")
    (markdown-ts--fill-block-quote justify))
   ;; Fill within an HTML comment.  The node type is "html_block"
   ;; in grammar v0.4.x and "comment" in some other versions.
   ((bind-and* (block (let ((n (treesit-node-at (point) 'markdown)))
                        (or (treesit-parent-until
                             n "\\`\\(?:html_block\\|comment\\)\\'")
                            (and (member (treesit-node-type n)
                                         '("html_block" "comment"))
                                 n)))))
    (markdown-ts--fill-html-comment block justify))
   ;; Default: let fill-paragraph handle it.
   (t nil)))

(defun markdown-ts--fill-html-comment (node justify)
  "Fill HTML comment NODE, aligning continuation lines.
Return t if NODE is a comment and was filled, nil otherwise.
JUSTIFY is as in `fill-paragraph'."
  (when (save-excursion
          (goto-char (treesit-node-start node))
          (looking-at-p "<!--"))
    (let ((fill-prefix (save-excursion
                         (goto-char (treesit-node-start node))
                         (make-string (+ (current-column)
                                         (length "<!-- "))
                                      ?\s)))
          (adaptive-fill-function nil)
          (fill-forward-paragraph-function #'forward-paragraph)
          (fill-paragraph-function nil))
      (save-restriction
        (narrow-to-region (treesit-node-start node)
                          (treesit-node-end node))
        (fill-paragraph justify))
      t)))

(defun markdown-ts--block-quote-prefix ()
  "Return the block quote prefix string from the current line.
Read the actual `>' markers (with whatever spacing the user wrote)
from the beginning of the line so that fill preserves the existing
style (e.g., `>>> ' stays `>>> ', `> > > ' stays `> > > ')."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\([> \t]*>\\)[ \t]*")
        (concat (match-string 0))
      "> ")))

(defun markdown-ts--fill-block-quote (justify)
  "Fill the paragraph at point inside a block quote.
Find the paragraph node at point within the block quote and fill
only that paragraph, preserving the `> ' prefix.  When the
paragraph is inside a list item, align continuation lines with
the item's text column.  JUSTIFY is as in `fill-paragraph'."
  ;; Skip past `> ' markers and any list marker to land inside the
  ;; content, where the paragraph node lives.
  (let* ((content-pos (save-excursion
                        (beginning-of-line)
                        (skip-chars-forward "> \t")
                        ;; If we land on a list marker, skip past it.
                        (when (looking-at "[-*+] \\|[0-9]+[.)]\s")
                          (goto-char (match-end 0)))
                        (min (1+ (point)) (point-max))))
         (node (treesit-node-at content-pos 'markdown))
         (para (treesit-parent-until
                node (lambda (n)
                       (equal (treesit-node-type n) "paragraph")))))
    (when para
      ;; The grammar may include trailing block_continuation nodes
      ;; (blank `> >' lines) as children of the paragraph.  Use the
      ;; end of the last inline child so fill does not merge across
      ;; what should be a paragraph boundary.
      (let* ((last-inline
              (seq-find (lambda (child)
                          (equal (treesit-node-type child) "inline"))
                        (reverse (treesit-node-children para))))
             (para-end (if last-inline
                           (treesit-node-end last-inline)
                         (treesit-node-end para)))
             (bq-prefix (markdown-ts--block-quote-prefix))
             ;; When inside a list item, extend the prefix with spaces
             ;; so continuation lines align with the item's text.
             (list-item (treesit-parent-until para "\\`list_item\\'"))
             (prefix (if list-item
                         (let* ((text-col (markdown-ts--list-item-text-column
                                           list-item))
                                (extra (max 0 (- text-col (length bq-prefix)))))
                           (concat bq-prefix (make-string extra ?\s)))
                       bq-prefix))
             (adaptive-fill-function nil)
             (fill-forward-paragraph-function #'forward-paragraph)
             (fill-paragraph-function nil))
        (save-restriction
          ;; Narrow to the full lines of the paragraph so that the
          ;; `> ' markers are included in the fill region.
          (narrow-to-region
           (save-excursion
             (goto-char (treesit-node-start para))
             (beginning-of-line)
             (point))
           para-end)
          (let ((fill-prefix prefix))
            (fill-paragraph justify))))))
  t)

;;; Code blocks:

(defvar-local markdown-ts--code-block-languages nil
  "Alist mapping language node names to ts language.
If a language is not a ts language or is not mapped to one, it will be
in `markdown-ts--code-block-non-ts-modes'.")

(defvar-local markdown-ts--code-block-non-ts-modes nil
  "Alist mapping languages to non-tree-sitter modes for code blocks.
Populated by `markdown-ts--code-block-ts-language' to identify a
code-block language mode which is not a tree-sitter mode or for which
the tree-sitter grammar is unavailable.")

(defun markdown-ts--harvest-mode-treesit-configuration (mode)
  "Harvest tree-sitter configuration from MODE.
Return a plist with the following keys and values:

    :font-lock (from `treesit-font-lock-settings')
    :simple-indent (from `treesit-simple-indent-rules')
    :range (from `treesit-range-settings')"
  (with-work-buffer
    (markdown-ts--inhibit-messages-and-warnings
      'markdown-ts-inhibit-code-block-mode-warnings
      (delay-mode-hooks (funcall mode)))
    (list :language (treesit-language-at (point-min))
          :font-lock treesit-font-lock-settings
          :simple-indent treesit-simple-indent-rules
          :range treesit-range-settings)))

(defun markdown-ts--configure-current-buffer (configuration)
  "Add CONFIGURATION to the current buffer.
CONFIGURATION includes font-lock and indent rules.  For font-lock rules,
use the same features enabled in MODE."
  (setq treesit-font-lock-settings
        (append treesit-font-lock-settings
                ;; Get all the font-lock settings, including ones that
                ;; don't pertain to MODE.  This way, we get jsdoc
                ;; from js-ts-mode, for example.
                (plist-get configuration :font-lock)))
  (setq treesit-simple-indent-rules
        (append treesit-simple-indent-rules
                ;; Similarly, get all indent rules.
                (plist-get configuration :simple-indent)))
    (setq treesit-range-settings
          (append treesit-range-settings
                  ;; Filter out function queries, because they are
                  ;; usually some hack and might escape the code block.
                  ;; Case in point: c-ts-mode's range setting.
                  (seq-filter (lambda (setting)
                                (not (functionp (car setting))))
                              (plist-get configuration :range))))
  (setq-local indent-line-function #'treesit-indent)
  (setq-local indent-region-function #'treesit-indent-region))

(defun markdown-ts--language-at-node (node)
  "Return the language symbol for code_fence_content NODE."
  (when-let* ((parent (treesit-node-parent node))
              (lang-node (when parent
                           (treesit-search-subtree parent "\\`language\\'"))))
    (intern (treesit-node-text lang-node t))))

(defvar-local markdown-ts--non-ts-fontify-cache (make-hash-table :test #'equal)
  "Cache for non-tree-sitter code block fontification.
Keys are (LANG . CONTENT-HASH) cons cells, values are lists of
\(OFFSET-START OFFSET-END FACE) triples.")

(defvar-local markdown-ts--non-ts-fontify-cache-tick nil
  "For blunt `buffer-chars-modified-tick' cache invalidation.")

(defun markdown-ts--fontify-non-ts-code-block (node override start end &rest _)
  "Fontify code_fence_content NODE using a non-tree-sitter mode.
Apply the code-block mode's conventional font-lock in a temporary
buffer.  OVERRIDE, START, and END are passed through to
`treesit-fontify-with-override'.

Cache results to avoid fontification of unchanged code blocks.

NODE should already have passed through
`markdown-ts--code-block-ts-language' which may have classified this
node as a non-ts mode."
  (when-let* ((_ markdown-ts-fontify-code-blocks-natively)
              (lang (markdown-ts--language-at-node node))
              (mode (alist-get lang markdown-ts--code-block-non-ts-modes))
              (tick (buffer-chars-modified-tick))
              (block-start (treesit-node-start node))
              ;; Cannot use markers 'markdown-ts-code-beg-marker
              ;; 'markdown-ts-code-end-marker they are set after this
              ;; function runs.
              (node-start (save-excursion
                            (goto-char (treesit-node-start node))
                            (line-beginning-position)))
              (node-end (save-excursion
                          (goto-char (treesit-node-end node))
                          (skip-chars-backward " \t")
                          (point))))
    (when (> node-end node-start)
      (unless (eq tick markdown-ts--non-ts-fontify-cache-tick)
        (clrhash markdown-ts--non-ts-fontify-cache))
      (let* ((cache-key (cons lang (secure-hash
                                    'sha256
                                    (current-buffer) node-start node-end)))
             (props
              (or (gethash cache-key
                           markdown-ts--non-ts-fontify-cache)
                  (let ((result
                         (markdown-ts--fontify-non-ts-collect-faces
                          mode node-start node-end)))
                    (puthash cache-key result
                             markdown-ts--non-ts-fontify-cache)
                    (setq markdown-ts--non-ts-fontify-cache-tick tick)
                    result))))
        (dolist (range props)
          (treesit-fontify-with-override
           (nth 0 range)
           (nth 1 range)
           (nth 2 range)
           override start end))))))

(defun markdown-ts--fontify-non-ts-collect-faces (mode beg end)
  "Run MODE on the BEG..END region's text and harvest face properties.
Return a list of (BUF-BEG BUF-END FACE) triples in base-buffer
coordinates.

For non-tree-sitter modes use an indirect buffer narrowed to BEG..END:
regex/syntactic font-lock honors narrowing and the indirect buffer
shares text with the base, so this is less expensive and copy-free.

For ts modes in `markdown-ts-code-block-force-conventional-modes', fall
back to a temp buffer.  A parser created in the source buffer does not
react to narrowing in an indirect buffer derived from it (only parsers
created in that indirect buffer would honor its narrowing), so reusing
the host parser via an indirect buffer is not an option here.  And in
the host buffer itself, the inner content of a fenced code block is
just opaque text inside a `code_fence_content' node, never re-parsed
as a fresh document.  A temp buffer with a fresh parser sees the inner
content as a standalone markdown document, which is what we want."
  (cond ((memq mode markdown-ts-code-block-force-conventional-modes)
         (let ((content (buffer-substring-no-properties beg end))
               res)
           (with-work-buffer
             (insert content)
             (markdown-ts--inhibit-messages-and-warnings
               'markdown-ts-inhibit-code-block-mode-warnings
               (delay-mode-hooks
                 (let ((markdown-ts--set-up-inline t))
                   (markdown-ts-mode))))
             (font-lock-ensure)
             (let ((pos (point-min)))
               (while (< pos (point-max))
                 (let ((next (next-single-property-change
                              pos 'face nil (point-max)))
                       (face (get-text-property pos 'face)))
                   (when face
                     ;; Translate temp-buffer offsets back to base-buffer
                     ;; positions: temp `point-min' corresponds to BEG.
                     (push (list (+ beg (1- pos)) (+ beg (1- next)) face)
                           res))
                   (setq pos next)))))
           (nreverse res)))
        (t
         (let ((indirect-buffer (make-indirect-buffer
                                 (current-buffer)
                                 (generate-new-buffer-name (buffer-name))
                                 nil 'inhibit-buffer-hooks))
               res)
           (unwind-protect
               (with-current-buffer indirect-buffer
                 (markdown-ts--inhibit-messages-and-warnings
                   'markdown-ts-inhibit-code-block-mode-warnings
                   (delay-mode-hooks (funcall mode)))
                 (narrow-to-region beg end)
                 (font-lock-ensure)
                 (let ((pos (point-min)))
                   (while (< pos (point-max))
                     (let ((next (next-single-property-change
                                  pos 'face nil (point-max)))
                           (face (get-text-property pos 'face)))
                       (when face
                         (push (list pos next face) res))
                       (setq pos next)))))
             (kill-buffer indirect-buffer))
           (nreverse res)))))

(defun markdown-ts--code-block-language-mode (lang)
  "Compute and cache a mode symbol from LANG, a symbol.
Consult the `markdown-ts-code-block-modes' cache, or consult
`treesit-major-mode-remap-alist' and `major-mode-remap-alist', or brute
force mode probe.  Return a valid mode symbol or nil."
  (if-let* ((mapped-mode (car (alist-get lang markdown-ts-code-block-modes))))
      mapped-mode
    (let* ((lang-string (symbol-name lang))
           (lang-mode (concat lang-string "-mode"))
           (mode))
      (if (setq mode (alist-get lang-mode treesit-major-mode-remap-alist))
          mode
        (if (setq mode (alist-get lang-mode major-mode-remap-alist))
            mode
          (catch :mode
            (dolist (mode
                     (list
                      ;; Try a treesit mode using the raw string.
                      (concat lang-string "-ts-mode")
                      ;; Try a conventional mode using the raw string.
                      lang-mode
                      ;; Try a treesit mode using the downcased string.
                      (concat (downcase lang-string) "-ts-mode")
                      ;; Try a conventional mode using the downcased string.
                      (concat (downcase lang-string) "-mode")))
              (setq mode (intern mode))
              (when (fboundp mode)
                (push (list lang mode) markdown-ts-code-block-modes)
                (throw :mode mode)))))))))

(defun markdown-ts--code-block-ts-language (node)
  "Convert NODE to a language for the code block."
  (let* ((lang (intern (treesit-node-text node)))
         (mode (markdown-ts--code-block-language-mode lang))
         (ts-language (alist-get lang markdown-ts--code-block-languages))
         (non-ts-language (alist-get lang markdown-ts--code-block-non-ts-modes)))
    (cond
     ((fboundp mode)
      (cond
       ;; Return known treesit language.
       (ts-language ts-language)
       ;; Return nil for known non-treesit language for font-lock.
       (non-ts-language nil)
       ;; Markdown itself needs two parsers (markdown +
       ;; markdown-inline), so treesit embedding (which creates only
       ;; one) cannot fully fontify it.  Route through the non-ts
       ;; path so the temp buffer runs the full mode with both
       ;; parsers.
       ((provided-mode-derived-p mode 'markdown-ts-mode)
        (unless (assq lang markdown-ts--code-block-non-ts-modes)
          (push (cons lang mode) markdown-ts--code-block-non-ts-modes))
        nil)
       (t
        (let* ((configuration (markdown-ts--harvest-mode-treesit-configuration
                               mode))
               (ts-language (plist-get configuration :language)))
          (cond
           ((and ts-language
                 (plist-get configuration :font-lock))
            (markdown-ts--configure-current-buffer configuration)
            (unless (assq lang markdown-ts--code-block-languages)
              (push (cons lang ts-language) markdown-ts--code-block-languages))
            ts-language)
           ;; Otherwise, classify mode as non-tree-sitter.
           (t
            (unless (assq lang markdown-ts--code-block-non-ts-modes)
              (push (cons lang mode) markdown-ts--code-block-non-ts-modes))
            nil))))))
     (t nil))))

;;; Code block minor mode support and code-block context commands:

(defvar markdown-ts-code-block-commands '(indent-for-tab-command
                                          electric-newline-and-maybe-indent
                                          completion-at-point
                                          complete-symbol
                                          newline
                                          comment-dwim
                                          comment-line
                                          comment-or-uncomment-region
                                          markdown-ts--code-block-fill-paragraph
                                          prog-fill-reindent-defun)
  "Commands to execute in a code-block context.
See `markdown-ts--run-command-in-code-block'.")

(defvar markdown-ts-code-block-thing-commands '(xref-find-definitions)
  "Commands that need a \"thing\" at point in a code-block context.
See `markdown-ts--run-command-in-code-block'.")

(defvar markdown-ts-code-block-ignore-output-commands '(xref-find-definitions)
  "Commands whose output to ignore when executed in a code-block context.
See `markdown-ts--run-command-in-code-block'.")

(defvar markdown-ts-code-block-region-commands '(comment-or-uncomment-region)
  "Commands that need a region in a code-block context.
See `markdown-ts--run-command-in-code-block'.")

(defun markdown-ts--enable-code-block-in-context-mode ()
  "Enable `markdown-ts-code-block-in-context-mode' if in a fenced code block."
  (markdown-ts-code-block-in-context-mode
   (if (markdown-ts-at-code-block-p) 1 -1)))

;; NOTE: Do not add this command to `markdown-ts-code-block-commands'.
(defun markdown-ts--code-block-newline (&optional arg interactive)
  "Insert a newline, and move to left margin of the new line.
With prefix argument ARG, insert that many newlines.

If `electric-indent-mode' is enabled, this indents the final new line
that it adds, and reindents the preceding line.  To just insert
a newline, use \\[electric-indent-just-newline].

If `auto-fill-mode' is enabled, this may cause automatic line
breaking of the preceding line.  A non-nil ARG inhibits this.

If variable `use-hard-newlines' is enabled, the newline is marked with
the text-property `hard'.

A non-nil INTERACTIVE argument means to run the `post-self-insert-hook'."
  (interactive "*P\np")
  (if-let* ((block-mode (markdown-ts-code-block-mode-at))
            ((fboundp block-mode)))
      (markdown-ts--run-command-in-code-block block-mode
                                              #'newline
                                              arg
                                              interactive)
    (funcall-interactively #'newline arg interactive)))

(defun markdown-ts--code-block-fill-paragraph (&optional justify)
  "Refill or reindent the markdown content that contains point.
If the point is in a string or a comment, fill the paragraph that
contains point or follows point.

Otherwise, reindent the function definition that contains point or
follows point.

If JUSTIFY is non-nil (interactively, with prefix argument), justify as
well."
  (interactive "P")
  (cond ((derived-mode-p 'prog-mode)
         (call-interactively #'prog-fill-reindent-defun justify))
        (t
         (call-interactively #'fill-paragraph justify))))

;; NOTE: Do not add this command to `markdown-ts-code-block-commands'.
(defun markdown-ts--code-block-xref-find-definitions (&rest args)
  "Helper command for `xref-find-definitions' in a code-block context.
Find thing at point.
Adjust the references on the xref stack to the base buffer.
Pass through ARGS if not in a code block with an available mode."
  (interactive)
  (if-let* ((block-mode (markdown-ts-code-block-mode-at))
            ((fboundp block-mode)))
      (condition-case err
          (unwind-protect
              (progn
                ;; Record a placeholder xref marker in the base buffer.  This
                ;; will be retained if the command succeeds to push a marker in
                ;; the code-block buffer which we will pop.
                (xref-push-marker-stack)
                ;; `xref-find-definitions' is a thing command in
                ;; `markdown-ts-code-block-thing-commands'.
                (markdown-ts--run-command-in-code-block block-mode
                                                        #'xref-find-definitions
                                                        args))
            ;; Pop the top xref marker.  If the command succeeded, the
            ;; top marker will be the code-block buffer marker.  If it
            ;; failed, we'll pop the placeholder which is now of no
            ;; value.
            ;; TODO: Propose an `xref' pop function that doesn't "go
            ;; back".
            (let ((history (xref--get-history)))
              (unless (null (car history))
                (pop (car history)))))
        (error
         ;; Propagate the signal.
         (signal (car err) (cdr err))))
    ;; Not in a code block context.  This should really never happen.
    (funcall-interactively #'xref-find-definitions args)))

(defun markdown-ts--maybe-run-command-in-code-block ()
  "Helper function to wrap a command for a code-block context.
If `this-command' is a member of `markdown-ts-code-block-commands' and
point is within a code block with an available mode, it will run in that
code block's mode in a buffer narrowed to its content.  Otherwise, the
command will run in the context of the `markdown-ts-mode' buffer."
  (when (memq this-command markdown-ts-code-block-commands)
    (when-let* ((command this-command)
                (block-mode (markdown-ts-code-block-mode-at))
                ((fboundp block-mode)))
      (setq this-command (lambda (&rest args)
                           (interactive)
                           (apply #'markdown-ts--run-command-in-code-block
                                  block-mode
                                  command
                                  args))))))

(defun markdown-ts--run-command-in-code-block (block-mode command &rest args)
  "Run COMMAND in BLOCK-MODE.
ARGS are captured by `markdown-ts--maybe-run-command-in-code-block'."
  (when-let* ((beg (get-char-property (point) 'markdown-ts-code-beg-marker))
              (end (get-char-property (point) 'markdown-ts-code-end-marker))
              (str (buffer-substring-no-properties beg end)))
    ;; Use a temp (or work) buffer because treesit currently confuses
    ;; nodes in an indirect buffer even if the indirect buffer is not
    ;; narrowed.
    (let* ((temp-deactivate-mark)
           (orig-point (point))
           (orig-mark (mark t))
           (orig-mark-active mark-active)
           (region-beg (use-region-beginning))
           (region-end (use-region-end))
           (adj-point (1+ (- orig-point beg)))
           (adj-mark (when orig-mark (1+ (- orig-mark beg))))
           (adj-region-beg (when region-beg (1+ (- orig-point region-beg))))
           (adj-region-end (when region-end (1+ (- orig-point region-end))))
           (point-delta 0)
           (ignore-output
            (memq command markdown-ts-code-block-ignore-output-commands))
           (source-buffer (current-buffer)))
      (with-work-buffer
        (insert str)
        (goto-char adj-point)
        ;; Propagate mark (and region).
        (when orig-mark-active
          (set-mark adj-mark))
        (markdown-ts--inhibit-messages-and-warnings
          'markdown-ts-inhibit-code-block-mode-warnings
          (delay-mode-hooks
            (let ((markdown-ts--set-up-inline
                   (provided-mode-derived-p block-mode 'markdown-ts-mode)))
              (funcall block-mode))))
        (let ((point (point))
              (arity (cdr (func-arity command))))
          (cond
           ((memq command markdown-ts-code-block-thing-commands)
            (when-let* ((thing (thing-at-point 'symbol)))
              (funcall-interactively command thing)))
           ((memq command markdown-ts-code-block-region-commands)
            (when (and adj-region-beg adj-region-end)
              (apply #'funcall-interactively command
                     adj-region-beg adj-region-end args)))
           ((zerop arity)
            (funcall-interactively command))
           ((eq 1 arity)
            (funcall-interactively command (car args)))
           (t
            (apply #'funcall-interactively command args)))
          (unless ignore-output
            (setq str (buffer-substring-no-properties (point-min) (point-max)))
            (setq temp-deactivate-mark deactivate-mark)
            (setq point-delta (- (point) point))))
        (unless ignore-output
          (let ((work-buffer (current-buffer)))
            (with-current-buffer source-buffer
              (replace-region-contents beg end work-buffer)
              ;; Propagate mark deactivation to the source buffer.
              (setq deactivate-mark temp-deactivate-mark)
              ;; Move point if it moved in the temp buffer.
              (goto-char (+ orig-point point-delta))
              ;; This helps maintain discrete command actions.
              (undo-boundary)
              ;; Make sure the originating region is refontified.
              (font-lock-flush beg end))))
        ;; Record the original command.
        (setq this-command command)))))

(defun markdown-ts--find-code-block-delimiter (pos &optional backward)
  "Return the next or previous fenced_code_block_delimiter node, or nil.
Search starting at POS.
Search backward if BACKWARD is non-nil."
  (treesit-search-forward (treesit-node-at pos 'markdown)
                          (lambda (node)
                            (string= (treesit-node-type node)
                                     "fenced_code_block_delimiter"))
                          backward))

(defun markdown-ts--find-next-code-block-delimiter (&optional
                                                    pos backward remain)
  "Return the next or previous fenced_code_block_delimiter node, or nil.
Search starting at POS or `point', if POS is nil.
Search backward if BACKWARD is non-nil.
If REMAIN is non-nil, move to the top or bottom of the current code
block, if in one."
  (setq pos (or pos (point)))
  (and-let* ((node (markdown-ts--find-code-block-delimiter pos backward)))
    (let ((in-block (markdown-ts-at-code-block-p pos)))
      (cond
       ((and remain in-block)
        node)
       (remain
        nil)
       (t
        (while (and node
                    ;; If backward, skip the current block's
                    ;; starting delimiter.
                    (or (and backward in-block (treesit-node-next-sibling node))
                        (not (treesit-node-next-sibling node))))
          (setq in-block (markdown-ts-at-code-block-p pos))
          (setq pos (if backward
                        (treesit-node-start node)
                      (treesit-node-end node)))
          (setq node (markdown-ts--find-code-block-delimiter pos backward)))
        node)))))

(defun markdown-ts-move-to-next-code-block (arg)
  "Move point to the start of the next code block.
With the prefix argument ARG, remain within the current code block."
  (interactive "P")
  (when-let* ((node
               (markdown-ts--find-next-code-block-delimiter nil nil arg)))
    (goto-char (treesit-node-start node))))

(defun markdown-ts-move-to-previous-code-block (arg)
  "Move point to the start of the previous code block.
With the prefix argument ARG, remain within the current code block."
  (interactive "P")
  (when-let* ((node
               (markdown-ts--find-next-code-block-delimiter nil 'backward arg)))
    (goto-char (treesit-node-start node))))

;;; Tables aka pipe tables:

;;;; Utilities:

(defconst markdown-ts-table-export-buffer
  "*markdown-ts-table-export*")

;; In frequency matching order.
(defconst markdown-ts--table-row-types
  '("pipe_table_row"
    "pipe_table_delimiter_row"
    "pipe_table_header"))

;; In frequency matching order.
(defconst markdown-ts--table-cell-types
  '("pipe_table_cell"
    "pipe_table_delimiter_cell"))

(defconst markdown-ts--table-delimiter-cell-types
  '("pipe_table_delimiter_cell"
    "pipe_table_align_left"
    "pipe_table_align_right"))

(defconst markdown-ts--table-delimiter-cell-subtypes
  '("pipe_table_align_left"
    "pipe_table_align_right"))

(defun markdown-ts--table-abutting-pos (pos)
  "Adjust POS to abut its closest text.
Return pos adjusted to the position of the nearest non-blank character.
Otherwise, return nil, for example, if the line is empty."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward "[[:blank:]]" (pos-eol))
    (if (eq pos (pos-eol))
        (progn
          (goto-char pos)
          (skip-chars-backward "[[:blank:]]" (pos-bol))
          (unless (eq pos (pos-bol))
            (max (point-min) (1- (point)))))
      (min (point-max) (1+ (point))))))

(defun markdown-ts--table-node-cell (&optional node pos abutting)
  "Compute table cell from named NODE at POS.
If NODE is nil, use the node at POS.
If POS is nil, use `point'.
If NODE is a cell type, return it.
If NODE is a subtype, promote it.
If ABUTTING is non-nil, adjust POS to the nearest non-blank character.
Otherwise, return nil."
  ;; We could sanity check via `treesit-node-named' and error if not.
  (setq pos (or pos (point)))
  (when abutting
    (setq pos (or (markdown-ts--table-abutting-pos pos) pos)))
  (when-let* ((node (or node
                        (treesit-node-at pos 'markdown 'named)))
              (type (treesit-node-type node)))
    (cond ((member type markdown-ts--table-cell-types)
           node)
          ((member type markdown-ts--table-delimiter-cell-subtypes)
           (treesit-node-parent node))
          ((member type markdown-ts--table-row-types)
           (cond ((eq abutting 'left)
                  (treesit-node-child node 0 'named))
                 ;; Account for 'right or t.
                 (t
                  (treesit-node-child node -1 'named)))))))

(defun markdown-ts--table-node-row (&optional node pos)
  "Compute table row from named NODE at POS.
If NODE is nil, use the node at POS.
If POS is nil, use `point'.
If NODE is a row type, return it.
If NODE is a table cell type, return its row.
Otherwise, return nil."
  ;; We could sanity check via `treesit-node-named' and error if not.
  (when-let* ((node (or node
                        (treesit-node-at (or pos (point)) 'markdown 'named)))
              (type (treesit-node-type node)))
    (if (member type markdown-ts--table-row-types)
        node
       (treesit-node-parent (markdown-ts--table-node-cell node)))))

(defun markdown-ts--table-parse-error-p (&optional node pos)
  "Return non-nil if named NODE at POS is in an \"ERROR\" subtree.
NODE must be a table element; i.e., its type prefix is \"pipe_table\".
If NODE is a table, return nil (it shouldn't be in ERROR).
If NODE is not a table element, return nil.
If NODE's nearby ancestor is an \"ERROR\" node, return t.
Otherwise, return nil."
  ;; We could sanity check via `treesit-node-named' and error if not.
  (when-let* ((row (markdown-ts--table-node-row node pos))
              (type (treesit-node-type row))
              (parent (treesit-node-parent row))
              (parent-type (treesit-node-type parent)))
    (cond ((equal type "pipe_table")
           nil)
          ((equal parent-type "pipe_table")
           nil)
          ((equal parent-type "ERROR")
           t)
          (t
           (error "Should never happen")))))

(defun markdown-ts-at-table-p (&optional pos abutting)
  "Return non-nil if POS is at or in a pipe table.
If POS is nil, use point.
Return cons (pos . table-node) if POS is within a table, otherwise
return nil.
if POS is within a table but with parse errors, signal an error.
If ABUTTING is non-nil, adjust POS to the nearest non-blank character.
If the parsed table is in ERROR, return nil."
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    ;; If point is on an empty line, it is never a table.
    (unless (and (eolp) (bolp))
      ;; If point is bol or eol it might be abutting a pipe symbol so
      ;; look forwards or backwards one character to test if that pos
      ;; is a table.
      (when abutting
        (setq pos (or (markdown-ts--table-abutting-pos pos) pos)))
      (when-let* ((row (markdown-ts--table-node-row nil pos)))
        (if (markdown-ts--table-parse-error-p row)
            ;; Do not use error or warning here.  Errors interfere with
            ;; the command loop.  Warnings are annoying.
            (progn
              (message "Incorrectly formatted Markdown table (parser error)")
              nil)
          (cons pos (treesit-search-forward
                     row
                     "\\`pipe_table\\'" 'backward)))))))

(defun markdown-ts--enable-in-table-mode ()
  "Enable `markdown-ts-in-table-mode' if in a table.
A table with a parser ERROR is ineligible."
  (markdown-ts-in-table-mode
   (if (markdown-ts-at-table-p nil t) 1 -1)))

(defun markdown-ts--table-body-row-near-pos (&optional pos abutting)
  "Return the row near POS.
Return nil if not at a table or there is no row.
Otherwise return cons (row . in-header) where in-header is nil or t.
This is useful to insert a new row just below the header instead of just
below the first row.
If POS is nil use `point'.
If ABUTTING is non-nil, use (1- POS) if POS is immediately after a pipe
symbol."
  (setq pos (or pos (point)))
  (let (in-header)
    (when-let* ((at-table (markdown-ts-at-table-p pos abutting))
                (pos (car at-table))
                (table (cdr at-table))
                (cell (markdown-ts--table-node-cell nil pos abutting))
                (cell-type (treesit-node-type cell))
                (parent (treesit-node-parent cell))
                (parent-type (treesit-node-type parent))
                (lineage (list cell-type parent-type))
                ;; Find the first row after the header or the row at POS is
                ;; on and in the same table.
                (row (cond ((or (member "pipe_table_header" lineage)
                                (member "pipe_table_delimiter_row" lineage))
                            (setq in-header t)
                            (treesit-search-forward cell "\\`pipe_table_row\\'"))
                           ((equal cell-type "pipe_table_row")
                            cell)
                           ((equal parent-type "pipe_table_row")
                            parent))))
      (when (treesit-node-eq table (treesit-search-forward
                                    row
                                    "\\`pipe_table\\'" 'backward))
        (cons row in-header)))))

(defun markdown-ts--table-compute-node-column (row node)
  "Compute NODE's column in ROW.
Return nil if NODE is not in ROW (this should not happen).
NODE must be a node in ROW.
ROW must be a pipe_table_header, pipe_table_delimiter_row, or
pipe_table_row."
  (catch :column
    (let ((count 0))
      (dolist (cell (treesit-node-children row 'named))
        (when (treesit-node-eq node cell)
          (throw :column count))
        (setq count (1+ count))))))

(defun markdown-ts-table--goto-column (column &optional no-error)
  "Move point on the current row to COLUMN.
COLUMN is a 0-based index.
Return non-nil if successful.
If NO-ERROR is non-nil, return nil if the current row does not extend to
COLUMN, otherwise signal an error.
Signal an error if point is not at a table."
  (if-let* ((cell (markdown-ts--table-node-cell nil nil t))
            (row (markdown-ts--table-node-row cell))
            (cols (treesit-node-children row 'named)))
      (if-let* ((target-cell (nth column cols)))
          (goto-char (treesit-node-start target-cell))
        (unless no-error
          (error "Column %S not found" column)))
    (error "Not at a table")))

(defun markdown-ts--table-aligners (table)
  "Extract the header line aligners from node TABLE.
Return a list of lists of the form:
  (:beg xxx :end xxx :align left)
:align is one of `left' `center' `right' `unspecified'."
  (let* ((delimiter-row (treesit-search-subtree
                         table
                         "\\`pipe_table_delimiter_row\\'"))
         (aligners
          (let (res)
            (dolist (elt (treesit-node-children delimiter-row 'named))
              (let ((align-beg (treesit-node-type
                                (treesit-node-child elt 0 'name)))
                    (align-end (treesit-node-type
                                (treesit-node-child elt -1 'name))))
                (push (list
                       :beg (treesit-node-start elt)
                       :end (treesit-node-end elt)
                       :align
                       (cond ((and
                               (equal align-beg "pipe_table_align_left")
                               (equal align-end "pipe_table_align_left"))
                              'left)
                             ((and
                               (equal align-beg "pipe_table_align_left")
                               (equal align-end "pipe_table_align_right"))
                              'center)
                             ((and
                               (equal align-beg "pipe_table_align_right")
                               (equal align-end "pipe_table_align_right"))
                              'right)
                             (t 'unspecified)))
                      res)))
            (nreverse res))))
    aligners))

(defun markdown-ts--table-make-aligner (width align)
  "Make a Markdown column aligner.
WIDTH is the column width.
ALIGN is one of `left' `center' `right' `unspecified'."
  (pcase align
    ('left
     (concat ":" (make-string (1- width) ?-)))
    ('center
     (concat ":" (make-string (- width 2) ?-) ":"))
    ('right
     (concat (make-string (1- width) ?-) ":"))
    (_
     (concat (make-string width ?-)))))

(defun markdown-ts--table-align-cell (text width align)
  "Return TEXT padded to WIDTH aligned using ALIGN.
Truncate TEXT wider than WIDTH.  It is the caller's responsibility to
ensure text will fit.
ALIGN is one of `left' `center' `right' `unspecified'."
  (let* ((strlen (length text))
         (strlen0 (min width strlen))
         (text0 (substring text 0 strlen0))
         (pad (- width strlen0)))
    (pcase align
      ('center
       (let* ((half-pad (make-string (floor (/ pad 2)) ?\s))
              (s (concat
                  half-pad
                  text0
                  half-pad)))
         (string-pad
          (substring s 0 (min (length s) width))
          width)))
      ('right
       (string-pad text width nil 'start))
      (_
       (string-pad text width)))))

;;;; Commands:

(defun markdown-ts-table-insert-table (&optional nrows ncols headings body)
  "Insert a Markdown pipe table.
NROWS is the number of rows in the new table and defaults to 3.

NCOLS is the number of columns and defaults to 3.

HEADINGS is a list of headings and defaults to left-aligned synthesized
column names.  If NCOLS is nil, use HEADINGS length.  If both NCOLS and
HEADINGS are non-nil, use up to NCOLS headings, synthesizing the
remainder.

Each heading in the list can be a string column name or a cons of the
form (name . alignment) where alignment is one of the symbols `left',
`right', or `center'.  If a string, default alignment is unspecified.

If BODY is non-nil, it is a list of NROWS rows where each row is a list
of NCOLS body cells.

For convenience and aesthetics, pad each body cell to the width of its
column's header, and accommodate alignment indicators.  If a BODY cell
is longer than its column header, insert it unchanged.

Note: A blank line is needed before the table for it to properly render.
If the line preceding `point' is not blank, insert one.

Note: Due to an issue with the tree-sitter Markdown grammar, a row of
empty cells causes a parsing error.  To avoid this, an empty row will
contain a single period in its first cell."
  (interactive)
  (setq nrows (or nrows 3))
  (setq ncols (or ncols (and headings (length headings)) 3))
  (setq headings
        (let (res)
          (dotimes (x ncols)
            (push (or (nth x headings)
                      (format "Column %d" (1+ x)))
                  res))
          (nreverse res)))
  (let ((heading-widths
         (let (res)
           (dolist (elt headings)
             (let ((str (if (consp elt) (car elt) elt)))
               (push (max markdown-ts-table-default-column-width
                          (length str))
                     res)))
           (nreverse res))))
    ;; If point is not at bol, insert a newline.
    (unless (eq (point) (pos-bol))
      (insert "\n"))
    ;; Insert an empty line if needed.
    (unless (save-excursion
              (forward-line -1)
              (eq (pos-bol) (pos-eol)))
      (insert "\n"))
    ;; Insert padded heading text.
    (insert "|")
    (dotimes (x ncols)
      (let* ((elt (nth x headings))
             (str (if (consp elt) (car elt) elt))
             (width (max 3 (nth x heading-widths)))
             (pad-width (- width (length str))))
        (insert str (make-string pad-width ?\s) "|")))
    (insert "\n")
    ;; Insert padded heading aligners.
    (insert "|")
    (dotimes (x ncols)
      (let* ((elt (nth x headings))
             (align (if (consp elt) (cdr elt) 'unspecified))
             (width (max 3 (nth x heading-widths))))
        (insert (markdown-ts--table-make-aligner width align)))
      (insert "|"))
    (insert "\n")
    ;; Insert BODY cells or empty cells.
    (dotimes (y nrows)
      (insert "|")
      (let ((row (nth y body)))
        (dotimes (x ncols)
          ;; TODO: Remove the placeholder character if the tree-sitter
          ;; grammar is repaired.
          (let* ((str (or (nth x row) (and (eq x 0) ".") ""))
                 (strlen (length str))
                 (width (nth x heading-widths))
                 (pad-width (max 0 (- width strlen))))
            (insert str
                    (make-string pad-width ?\s)
                    "|")))
        (insert "\n")))))

(defun markdown-ts-table-delete-table ()
  "Remove the Markdown table at point.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-delete-table)
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (table (cdr at-table)))
    ;; TODO: Remove after bug#23903 Undo after kb differs from after M-x
    (push (point) buffer-undo-list)
    (delete-region (treesit-node-start table) (treesit-node-end table))))

(defun markdown-ts-table-previous-row ()
  "Move to the previous row in the table.
If not in a table, do nothing."
  (interactive)
  (markdown-ts-table-next-row -1))

(defun markdown-ts-table-next-row (&optional n)
  "Move to the next row in the table.
If N is negative, move to the previous row.
If point is on the last row of the table, insert a new row below and
move into it.  With a prefix argument, clone the final row into the new
one.
If not in a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-next-row)
  (let ((backwards (and n (< n 0))))
    ;; Don't use `markdown-ts--table-body-row-near-pos' because we want
    ;; to operate in the header.
    (when-let* ((at-table (markdown-ts-at-table-p nil t))
                (pos (car at-table))
                (table (cdr at-table))
                (row (markdown-ts--table-node-row nil pos)))
      ;; If no next or prev sibling, we're on the last/first row.
      (let ((current-column (current-column))
            (next-row (if backwards
                          (treesit-node-prev-sibling row 'named)
                        (treesit-node-next-sibling row 'named))))
        (if next-row
            (goto-char (treesit-node-start next-row))
          (unless backwards
            (markdown-ts-table-insert-row-below current-prefix-arg)))
        (move-to-column current-column)))))

(defun markdown-ts-table-previous-cell ()
  "Move to the previous cell in the table.
If not in a table, do nothing."
  (interactive)
  (markdown-ts-table-next-cell -1))

(defun markdown-ts-table-next-cell (&optional n)
  "Move to the next cell in the table.
If N is negative, move to the previous cell.
If not in a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-next-cell)
  (let ((backwards (and n (< n 0))))
    (when-let* ((at-table (markdown-ts-at-table-p nil t))
                (pos (car at-table))
                (table (cdr at-table))
                (node (treesit-node-at pos 'markdown 'named))
                (cell (markdown-ts--table-node-cell node))
                (cell-type (treesit-node-type node))
                (row (markdown-ts--table-node-row cell))
                (row-type (treesit-node-type row))
                (next-cell
                 (cond
                  ;; If in a pipe_table_delimiter_row and not the first
                  ;; or final cell in the row, stay within the row
                  ;; otherwise, the default condition should match.
                  ((and (equal row-type "pipe_table_delimiter_row")
                        (if backwards
                            (treesit-node-prev-sibling cell 'named)
                          (treesit-node-next-sibling cell 'named)))
                   (if backwards
                       (treesit-node-prev-sibling cell 'named)
                     (treesit-node-next-sibling cell 'named)))
                  ;; If on the first pipe_table_row's first cell moving
                  ;; backwards, move into the final
                  ;; pipe_table_delimiter_cell.
                  ((and (equal cell-type "pipe_table_cell")
                        (equal (treesit-node-type
                                (treesit-node-prev-sibling row 'named))
                               "pipe_table_delimiter_row")
                        (not (treesit-node-prev-sibling cell 'named))
                        backwards)
                   (treesit-search-forward
                    cell
                    "\\`pipe_table_delimiter_cell\\'" backwards))
                  ;; If on the first pipe_table_header's final cell
                  ;; moving forwards, move into the first
                  ;; pipe_table_delimiter_cell.
                  ((and (equal cell-type "pipe_table_cell")
                        (equal (treesit-node-type
                                (treesit-node-next-sibling row 'named))
                               "pipe_table_delimiter_row")
                        (not (treesit-node-next-sibling cell 'named))
                        (not backwards))
                   (treesit-search-forward
                    cell
                    "\\`pipe_table_delimiter_cell\\'" backwards))
                  (t
                   (treesit-search-forward
                    cell
                    "\\`pipe_table_cell\\'" backwards)))))
      ;; Keep point within the current table.
      (when (treesit-node-eq table (treesit-search-forward
                                    next-cell
                                    "\\`pipe_table\\'" 'backward))
        (goto-char (treesit-node-start next-cell)))
      (when (or (eq markdown-ts-table-auto-align t)
                (memq 'cell-navigation markdown-ts-table-auto-align))
        (markdown-ts-table-align-table)))))

(defun markdown-ts-table-insert-row-below (&optional clone)
  "Insert a table row below point.
If point is in the header, insert below the first row of the table.
If CLONE is non-nil, or with a prefix argument, clone the current row.
If point is not at a table, do nothing."
  (interactive "P")
  (setq clone (or clone current-prefix-arg))
    (markdown-ts-table-insert-row 'below clone))

(defun markdown-ts-table-insert-row-above (&optional clone)
  "Insert a table row above point.
If point is in the header, insert above the first row of the table.
If CLONE is non-nil, or with a prefix argument, clone the current row.
If point is not at a table, do nothing."
  (interactive "P")
  (setq clone (or clone current-prefix-arg))
    (markdown-ts-table-insert-row 'above clone))

(defun markdown-ts-table-clone-row-below ()
  "Insert a table row below point.
If point is in the header, insert below the first row of the table.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-insert-row-below 'clone))

(defun markdown-ts-table-clone-row-above ()
  "Insert a table row above point.
If point is in the header, insert above the first row of the table.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-insert-row-above 'clone))

(defun markdown-ts-table-insert-row (&optional above clone)
  "Insert a table row below point.
If point is in the header, insert below the first row of the table.
If ABOVE is non-nil and is not `below', or with a prefix argument,
insert the row above point.
If CLONE is non-nil, clone the current row, otherwise insert empty
cells.
If point is not at a table, do nothing."
  (interactive "P")
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-insert-row)
  (setq above (cond ((eq above 'below) nil)
                    (above above)
                    (current-prefix-arg t)))
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (pos (car at-table))
              (table (cdr at-table))
              (delimiter-row (treesit-search-subtree
                              table
                              "\\`pipe_table_delimiter_row\\'"))
              (ncols (treesit-node-child-count delimiter-row 'named))
              (heading-widths
               (let (res)
                 (dolist (elt (treesit-node-children delimiter-row 'named))
                   (push (length (treesit-node-text elt)) res))
                 (nreverse res))))
    (let* ((near (markdown-ts--table-body-row-near-pos pos))
           (row (car near))
           (current-column (current-column)))
      ;; If row is nil, there are zero rows so add one below the
      ;; delimiter-row.
      (unless row
        (setq clone nil)
        (setq row delimiter-row)
        (setq above nil))
      (save-excursion
        (cond (above
               (goto-char (treesit-node-start row)))
              (t
               (goto-char (treesit-node-end row))
               (insert "\n")))
        (cond (clone
               ;; The clone strategy is WYSIWYG.  Just copy the text
               ;; and don't fuss with cell content and pipe symbols.
               ;; The grammar includes the final newline (and
               ;; sometimes not).
               (insert (string-trim-right
                        (treesit-node-text row 'no-property) "\n")))
              (t
               (insert "|")
               (dotimes (x ncols)
                 (let ((pad-width (nth x heading-widths)))
                   ;; TODO: Remove the placeholder character if the
                   ;; tree-sitter grammar is repaired.
                   (insert (if (eq x 0) "." "")
                           (make-string (- pad-width (if (eq x 0) 1 0)) ?\s)
                           "|")))))
        (when above
          (insert "\n")))
      (cond (above
             (when (> current-column 0)
               (with-suppressed-warnings ((interactive-only previous-line))
                 (previous-line))))
            (t (with-suppressed-warnings ((interactive-only next-line))
                 (next-line)))))))

(defun markdown-ts-table-delete-row ()
  "Delete the table row at point.
If point is on the header or delimiter row, do nothing.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-delete-row)
  (when-let* ((near (markdown-ts--table-body-row-near-pos nil t))
              ;; Bail if point is on the header or delimiter row — do
              ;; not silently delete a body row the user did not select.
              ((not (cdr near)))
              (row (car near)))
    (let ((current-column (current-column)))
      (delete-region (treesit-node-start row)
                     (min (1+ (treesit-node-end row)) (point-max)))
      (move-to-column current-column))))

(defun markdown-ts-table-move-row-up ()
  "Move the row at point up one line.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-move-row))

(defun markdown-ts-table-move-row-down ()
  "Move the row at point down one line.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-move-row 'down))

(defun markdown-ts-table-move-row (&optional down)
  "Move the row at point up one line.
If DOWN is non-nil, or with a prefix argument, move the row down one
line.
If point is on the header or delimiter row, do nothing.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-move-row)
  (setq down (or down current-prefix-arg))
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (pos (car at-table))
              (table (cdr at-table))
              (near (markdown-ts--table-body-row-near-pos pos))
              ;; Bail if point is on the header or delimiter row — do
              ;; not silently move a body row the user did not select.
              ((not (cdr near)))
              (row (car near)))
    ;; Do not move above the pipe_table_delimiter_row.
    ;; Do not move below the bottom of the table.
    (unless (or (and (not down)
                     (equal (treesit-node-type
                             (treesit-node-prev-sibling row 'named))
                            "pipe_table_delimiter_row"))
                (and down
                     (not (treesit-node-next-sibling row 'named))))
      (let ((row2 (if down
                      (treesit-node-next-sibling row 'named)
                    (treesit-node-prev-sibling row 'named)))
            (row-end (treesit-node-end row)))
        ;; TODO: Remove after bug#23903 Undo after kb differs from after M-x
        (push (point) buffer-undo-list)
        ;; If row point is outside its source region, e.g., at eol, move
        ;; it inside the region for stability.
        (when (>= (point) row-end)
          (goto-char (max (point-min) (1- row-end))))
        (transpose-regions (treesit-node-start row)
                           (treesit-node-end row)
                           (treesit-node-start row2)
                           (treesit-node-end row2))))))

(defun markdown-ts-table-insert-column-left (&optional clone)
  "Insert a table column to the left of point's column.
If CLONE is non-nil, or with a prefix argument, clone the current
column, otherwise insert empty cells.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive "P")
  (markdown-ts-table-insert-column 'left clone))

(defun markdown-ts-table-insert-column-right (&optional clone)
  "Insert a table column to the right of point's column.
If CLONE is non-nil, or with a prefix argument, clone the current
column, otherwise insert empty cells.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive "P")
  (markdown-ts-table-insert-column 'right clone))

(defun markdown-ts-table-clone-column-left ()
  "Clone the current column to its left.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-insert-column-left 'clone))

(defun markdown-ts-table-clone-column-right ()
  "Clone the current column to its right.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-insert-column-right 'clone))

(defun markdown-ts-table-insert-column (&optional left clone)
  "Insert a table column after point's current column.
If LEFT is non-nil and is not `right', or with a prefix argument,
insert the column to the left of the current column.
If CLONE is non-nil, clone the current column, otherwise insert empty
cells.
If CLONE is a list, its first element is target-column-number.  It is
the caller's responsibility that this value is valid for the table,
though it may be invalid for a non-uniform ragged row.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-insert-column)
  (setq left (cond ((eq left 'right) nil)
                   (left left)
                   (current-prefix-arg t)))
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (pos (car at-table))
              (table (cdr at-table))
              (delimiter-row (treesit-search-subtree
                              table "\\`pipe_table_delimiter_row\\'"))
              ;; Column pos for inserting missing leading pipes.
              (delim0-pipe-col
               (save-excursion
                 (let* ((delim-cell (treesit-node-child
                                     delimiter-row 0 'named))
                        (beg (treesit-node-start delim-cell)))
                   (goto-char beg)
                   (goto-char (pos-bol))
                   (if (search-forward "|" beg t)
                       (1- (current-column))
                     (goto-char beg)
                     (current-column)))))
              (node (treesit-node-at pos 'markdown 'named))
              (cell (markdown-ts--table-node-cell node))
              (point-row (markdown-ts--table-node-row cell)))
    (let ((table-column (markdown-ts--table-compute-node-column point-row cell)))
      (unless table-column
        (error "Could not compute the table column"))
      ;; NOTE: GFM tables allow non-uniform table rows.  The current row
      ;; could have a differing number of columns from other rows.  We
      ;; silently do nothing to a row that does not extend to the
      ;; computed column.
      ;;
      ;; NOTE: The grammar annoyingly ignores cell leading whitespace
      ;; but includes trailing whitespace, so cells are bounded by the
      ;; first graph character up to the last character just before the
      ;; pipe symbol, including whitespace.  The result is the range
      ;; does not start immediately after the optional leading pipe
      ;; symbol yet inconsistently continues until just before the
      ;; trailing pipe.
      (let* ((adj 0) ; Adjust node position offsets by inserted text.
             (width markdown-ts-table-default-column-width)
             (empty (make-string width ?\s))
             ;; TODO: Remove the placeholder character if the tree-sitter
             ;; grammar is repaired.
             (placeholder (concat "." (make-string (1- width) ?\s)))
             (delim (make-string width ?-))
             (rows (treesit-node-children table 'named))
             (nrows (length rows))
             (target-column-number (and (listp clone) (car clone)))
             ;; Position to move point to after all rows have been
             ;; updated.  Deterministic placement avoids depending on
             ;; tree-sitter navigation through freshly-inserted empty
             ;; cells, which the grammar does not always reparse as
             ;; pipe_table_cell nodes.
             (target-pos))
        ;; TODO: Remove after bug#23903 Undo after kb differs from after M-x
        (push (point) buffer-undo-list)
        (save-excursion
          (without-restriction
            (dotimes (x nrows)
              (when-let* ((row (nth x rows))
                          (row-type (treesit-node-type row))
                          (cols (treesit-node-children row 'named))
                          (ncols (length cols))
                          ;; source-cell will be nil if table-column is beyond this
                          ;; row's column set and that row will be skipped.
                          (source-cell (nth table-column cols))
                          ;; If cloning with a specified target-column-number,
                          ;; target-cell will be nil and the row will be skipped.
                          (target-cell (if target-column-number
                                          (nth target-column-number cols)
                                        source-cell))
                          ;; Compute the insertion point.
                          (beg (cond
                                ;; If cloning to a specified target, use
                                ;; target-cell's end.
                                (target-column-number
                                 (+ adj
                                    (treesit-node-end target-cell)))
                                ;; If inserting to the left of the first
                                ;; column, use source cell's start
                                ;; adjusted for its optional pipe to
                                ;; capture its whitespace.  If there is
                                ;; no pipe, use bol.
                                ((and left (eq table-column 0))
                                 (let ((p (+ adj
                                             (treesit-node-start source-cell))))
                                   (goto-char p)
                                   (goto-char (pos-bol))
                                   (if (search-forward "|" p t)
                                       (setq p (1- (point)))
                                     (setq p (pos-bol)))
                                   p))
                                ;; If inserting left and any other
                                ;; column, use the end of the prior
                                ;; cell.
                                (left
                                 (+ adj
                                    (1+ (treesit-node-end
                                         (nth (1- table-column) cols)))))
                                ;; If inserting right, use the end of
                                ;; the current cell.
                                (t
                                 (+ adj
                                    (treesit-node-end source-cell))))))
                (goto-char beg)
                (let* ((orig-point (point))
                       (missing-leading-pipe
                        (when (eq table-column 0)
                          (save-excursion
                            (goto-char (pos-bol))
                            (unless (search-forward
                                     "|"
                                     (+ adj (treesit-node-start source-cell)) t)
                              "|"))))
                       (text (cond
                              ;; If cloning, retrieve the source cell text.
                              (clone (buffer-substring-no-properties
                                      ;; Back up to bol or the pipe
                                      ;; to capture the whitespace
                                      ;; the grammar leaves out.
                                      (save-excursion
                                        (goto-char
                                         (+ adj
                                            (treesit-node-start source-cell)))
                                        (skip-chars-backward "[[:blank:]]"
                                                             (pos-bol))
                                        (point))
                                      (+ adj (treesit-node-end source-cell))))
                              ;; If new column, and this is the
                              ;; delimiter row, insert a default
                              ;; delimiter.
                              ((equal row-type "pipe_table_delimiter_row")
                               delim)
                              ;; If new column, and this is the a header
                              ;; or body row, insert a default empty
                              ;; string.
                              (t
                               empty))))
                  (cond
                   (clone
                    (cond
                    ;; Clone the first column to the left.  If there is
                    ;; no pipe before the cell content, insert one.
                     ((and left (eq table-column 0))
                        (insert "|"
                                text
                                (or missing-leading-pipe "")))
                    ;; Clone the first column to the right.
                     ((and (not left) (eq table-column 0))
                      (insert "|"
                              text)
                      ;; For a positive user experience given grammar
                      ;; bugs, add a missing leading pipe to the first
                      ;; cell at a pleasing position after cloning.
                      (when missing-leading-pipe
                        (save-excursion
                          (move-to-column delim0-pipe-col)
                          (insert "|"))))
                    ;; Clone the final column to the left.
                     ((and left (eq table-column (1- ncols)))
                      (insert text
                              "|"))
                     ;; Clone a middle column to the left.
                     (left
                      (insert text
                              "|"))
                     ;; Clone a middle or final column to the right.
                     (t
                      (insert "|"
                              text))))

                   ;; Not cloning, insert a new column.
                   ((and left (eq table-column 0) (eq x (1- nrows))
                         (not (equal row-type "pipe_table_delimiter_row")))
                   ;; TODO: remove placeholder logic.
                    (insert
                     "|"
                     placeholder
                     (or missing-leading-pipe "")))
                   ((and left (eq table-column 0))
                      (insert
                       "|"
                       text
                       (or missing-leading-pipe "")))
                   (left
                    (insert
                     text
                     "|"))
                   (t
                    (insert
                     "|"
                     text)))

                  ;; Capture the start of the new cell's content in
                  ;; the row that originally contained point.
                  (when (and (not target-pos)
                             (treesit-node-eq row point-row))
                    (setq target-pos
                          (cond
                           (target-column-number
                            (treesit-node-start target-cell))
                           (t
                            (if (and left (not (eq table-column 0)))
                                ;; "text |" inserted at beg.
                                orig-point
                              ;; All other branches insert "|" first.
                              (1+ orig-point))))))

                  ;; Adjust offset for deleted text.
                  (setq adj (+ adj (- (point) orig-point))
                        orig-point (point)))))))
        (when target-pos
          (goto-char target-pos))))))

(defun markdown-ts-table-delete-column ()
  "Delete the table column at point.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-delete-column)
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (pos (car at-table))
              (table (cdr at-table))
              (cell (markdown-ts--table-node-cell nil pos))
              (row (markdown-ts--table-node-row cell))
              (cols (treesit-node-children row 'named))
              (ncols (length cols)))
    (let ((table-column (markdown-ts--table-compute-node-column row cell)))
      (unless table-column
        (error "Could not compute the table column"))
      ;; NOTE: GFM tables allow non-uniform table rows.  The current row
      ;; could have a differing number of columns from other rows.  We
      ;; silently do nothing to a row that does not extend to the
      ;; computed column.
      ;;
      ;; NOTE: The grammar annoyingly ignores cell leading whitespace
      ;; but does include trailing whitespace so cells are bounded by
      ;; the first graph character up to the last character just before
      ;; the pipe symbol, including whitespace.
      ;;
      ;; The grammar also considers the trailing pipe symbol to be a
      ;; part of its preceding cell so that's what we delete when we
      ;; delete a column's cell; i.e., if there is a pipe before the
      ;; first column, we delete it.
      (let* ((adj 0) ; Adjust node position offsets by deleted text.
             (rows (treesit-node-children table 'named))
             (nrows (length rows)))
        (save-excursion
          (without-restriction
            (dotimes (x nrows)
              ;; If cell is nil, it is beyond the table-column and its
              ;; row is ignored.
              (when-let* ((row (nth x rows))
                          (cols (treesit-node-children row 'named))
                          (ncols (length cols))
                          (cell (nth table-column cols))
                          (cell-beg (treesit-node-start cell))
                          (cell-end (treesit-node-end cell))
                          ;; Adjust beg to trim the grammar
                          ;; unaccounted-for leading whitespace.
                          (beg (cond
                                ;; If this is the sole column, delete from bol.
                                ((eq ncols 1)
                                 (goto-char (- cell-beg adj))
                                 (pos-bol))
                                ;; If this is the first column, delete
                                ;; from the pipe, if there is one,
                                ;; otherwise cell-beg.
                                ((eq table-column 0)
                                 (let ((p (- cell-beg adj)))
                                   (goto-char p)
                                   (goto-char (pos-bol))
                                   (if (search-forward "|" p t)
                                       (1- (point))
                                     p)))
                                ;; If this is the final column, delete from the prior pipe symbol.
                                ((eq table-column (1- ncols))
                                 (let ((p (- (treesit-node-end
                                              (nth (1- table-column) cols)) adj)))
                                   (goto-char p)
                                   (if (search-forward "|" (- cell-beg adj) t)
                                       (point)
                                     p)))
                                ;; Otherwise, start at the end of the
                                ;; prior cell.
                                (t
                                 (- (treesit-node-end
                                         (nth (1- table-column) cols))
                                    adj))))
                          ;; Adjust end to trim the trailing pipe symbol
                          ;; considered by the grammar to be part of its
                          ;; preceding cell.
                          (end (cond
                                ;; If this is the sole column, delete until eol.
                                ((eq ncols 1)
                                 (goto-char (- cell-beg adj))
                                 (pos-eol))
                                ;; If this is the final column, delete until eol.
                                ((eq table-column (1- ncols))
                                 (goto-char (- cell-end adj))
                                 (pos-eol))
                                ;; Otherwise, look forward from cell-end
                                ;; (which includes the cell's trailing
                                ;; whitespace) for a pipe symbol.
                                (t
                                 (goto-char (- cell-end adj))
                                 (if (re-search-forward "|+?" (pos-eol) t)
                                     (1- (point))
                                   cell-end)))))
                (delete-region beg end)
                (setq adj (+ adj (- end beg)))))))))))

(defun markdown-ts-table-move-column-left ()
  "Move the table column at point to its left.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-move-column 'left))

(defun markdown-ts-table-move-column-right ()
  "Move the table column at point to its right.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts-table-move-column 'right))

(defun markdown-ts-table-move-column (&optional left)
  "Move the table column at point right or left.
If LEFT is non-nil and is not `right', or with a prefix argument,
move the column to its left.
Do nothing if a row being moved is not within the columns of the header.
Do nothing if moving left and the current column is the first column of
the current row.
Do nothing if moving right and the current column is the final column of
the current row.
Point can be in the table header or body.
If point is not at a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-move-column)
  (setq left (cond ((eq left 'right) nil)
                   (left left)
                   (current-prefix-arg t)))
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (pos (car at-table))
              (table (cdr at-table))
              (node (treesit-node-at pos 'markdown 'named))
              (cell (markdown-ts--table-node-cell node))
              (row (markdown-ts--table-node-row node))
              (header-row (treesit-search-subtree
                              table
                              "\\`pipe_table_header\\'"))
              (nheaders (length (treesit-node-children header-row 'named)))
              (cols (treesit-node-children row 'named))
              (ncols (length cols)))
    (let ((table-column (markdown-ts--table-compute-node-column row cell)))
      (unless table-column
        (error "Could not compute the table column"))
      (if (>= table-column nheaders)
          (message "Ragged/uneven column cannot be moved")
        ;; A B C D -> B A C D
        ;; Move A left: do nothing
        ;; Move D right: do nothing
        ;; Move current column left:
        ;;   source column = t - 1
        ;; Move current column right:
        ;;   clone t to t+1 if the final column, otherwise t+2
        ;;   delete t
        (cond ((and left (eq table-column 0))
               (message "Leftmost column cannot be moved left")
               nil)
              ((and (not left) (eq table-column (1- ncols)))
               (message "Rightmost column cannot be moved right")
               nil)
              (t
               (let ((source-column (if left
                                        (1- table-column)
                                      table-column))
                     (target-column (if left
                                        table-column
                                      (1+ table-column))))
                 (save-excursion
                   (markdown-ts-table--goto-column source-column)
                   (markdown-ts-table-insert-column
                    'right (list
                            target-column)))
                 (markdown-ts-table--goto-column source-column)
                 (markdown-ts-table-delete-column)
                 ;; Empty cells can confuse the grammar, so no-error here.
                 (if left
                     (markdown-ts-table--goto-column
                      source-column 'no-error)
                   (markdown-ts-table--goto-column
                    target-column 'no-error)))))))))

(defun markdown-ts-table-align-column-left ()
  "Align the table column at point to the left.
Do the work with `markdown-ts-table-align-column', which see for more
details."
  (interactive)
  (markdown-ts-table-align-column 'left))

(defun markdown-ts-table-align-column-center ()
  "Align the table column at point to the center.
Do the work with `markdown-ts-table-align-column', which see for more
details."
  (interactive)
  (markdown-ts-table-align-column 'center))

(defun markdown-ts-table-align-column-right ()
  "Align the table column at point to the right.
Do the work with `markdown-ts-table-align-column', which see for more
details."
  (interactive)
  (markdown-ts-table-align-column 'right))

(defun markdown-ts-table-align-column (&optional align)
  "Alter the table column alignment at point.
Alter the column point is in.

Note: To compute the column, point must be within the column and cannot
be on the leading or trailing whitespace or on a column delimiter.

ALIGN can be one of the symbols `left', `center', `right' or nil for
unspecified, or the characters l, c, or r.

If ALIGN is nil, assume unspecified.  Make the alignment string a
minimum of 5 characters to accommodate Markdown conventions.

If point is not at a table, do nothing."
  (interactive
   (list (car (read-multiple-choice
               "Align column" '((?l "left")
                                (?c "center")
                                (?r "right")
                                (?u "unspecified"))))))
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-align-column)
  (setq align (if (characterp align)
                  (pcase align (?l 'left) (?c 'center) (?r 'right))
                align))
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (pos (car at-table))
              (table (cdr at-table))
              (node (treesit-node-at pos 'markdown 'named))
              (cell (markdown-ts--table-node-cell node))
              (row (markdown-ts--table-node-row cell))
              (aligners (markdown-ts--table-aligners table)))
    (let ((table-column (markdown-ts--table-compute-node-column row cell)))
      (unless table-column
        (error "Could not compute the table column"))
      ;; NOTE: GFM tables allow non-uniform table rows.  The current row
      ;; could have a differing number of columns from the header.  We
      ;; silently do nothing if we can't find a matching header column.
      (when-let* ((aligner (nth table-column aligners))
                  (beg (plist-get aligner :beg))
                  (end (plist-get aligner :end))
                  (str (buffer-substring-no-properties beg end))
                  (strlen (max 5 (length str))))
        (delete-region beg end)
        (save-excursion
          (goto-char beg)
          (insert (markdown-ts--table-make-aligner strlen align)))))))

(defun markdown-ts-table-align-table (&optional align-cells)
  "Align the Markdown table at point.
Adjust each column's width to accommodate its widest constituent cell.
If ALIGN-CELLS is non-nil, or with a prefix argument, apply the
delimiter row's alignment to cell content.  ALIGN-CELLS is non-nil if
the user option `markdown-ts-table-align-features' includes
`justify-cells'.
Anchor the columns to the delimiter row.  Ignore rows that contain
columns beyond the delimiter row.
If not in a table, do nothing."
  (interactive "P")
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-align-table)
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (table (cdr at-table))
              (beg (treesit-node-start table))
              (end (treesit-node-end table))
              (_ (markdown-ts--table-tick-stale-p beg))
              (rows (treesit-node-children table 'named))
              (nrows (length rows))
              (delimiter-row (treesit-search-subtree
                         table
                         "\\`pipe_table_delimiter_row\\'"))
              (aligners (markdown-ts--table-aligners table))
              (cols (treesit-node-children delimiter-row 'named))
              (ncols (length cols))
              (col-widths (make-hash-table :size ncols :test 'equal))
              (cell-text (make-hash-table :size (* nrows ncols) :test 'equal)))
    (setq align-cells (or align-cells
                          current-prefix-arg
                          (memq 'justify-cells markdown-ts-table-align-features)))
    (without-restriction
      ;; Collect cell text and compute max column widths.
      ;; Do not use the delimiter row for width computation.
      (dotimes (x nrows)
        (let* ((row (nth x rows))
               (cells (treesit-node-children row 'named)))
          (dotimes (y ncols)
            ;; Skip nil cells--the parser got confused.
            (when-let* ((cell (nth y cells))
                        (text (string-trim-right
                               (treesit-node-text cell 'no-property)))
                        (w (length text))
                        (w0 (or (gethash y col-widths) 0)))
              ;; Elide delimiter widths.
              (unless (eq x 1)
                (when (> w w0)
                  (puthash y w col-widths)))
              (puthash (cons x y) text cell-text)))))

      (let ((source-buffer (current-buffer)))
        (with-temp-buffer
          (dotimes (x nrows)
            (dotimes (y ncols)
              (let* ((width (or (gethash y col-widths)
                                markdown-ts-table-default-column-width))
                     (align (or (plist-get (nth y aligners) :align)
                                'unspecified))
                     ;; Confused cells have nil text.
                     (text (or (gethash (cons x y) cell-text) ""))
                     (text0 (markdown-ts--table-align-cell
                             text width
                             (if align-cells
                                 align
                               'unspecified))))
                (cond
                 ;; Header row.
                 ((eq x 0)
                  (insert "|" text0))
                 ;; Delimiter row.  Use unaligned text.
                 ((eq x 1)
                  (insert "|"
                          (markdown-ts--table-make-aligner width align)))
                 ;; Body row.
                 (t
                  (insert "|" text0)))))
            (insert "|" "\n"))
          (let ((temp-buffer (current-buffer)))
            (with-current-buffer source-buffer
              (replace-region-contents beg end temp-buffer))))))
    (markdown-ts--table-tick-update beg)))

(defun markdown-ts-table-transpose-table ()
  "Transpose the rows and columns of the Markdown table at point.
Use unspecified column alignment in the transposed table.
If table's rows are not uniform with those on the delimiter row, signal
an error.
If not in a table, do nothing."
  (interactive)
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-transpose-table)
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (table (cdr at-table))
              (beg (treesit-node-start table))
              (end (treesit-node-end table))
              (rows (treesit-node-children table 'named))
              (nrows (length rows))
              (delimiter-row (treesit-search-subtree
                         table
                         "\\`pipe_table_delimiter_row\\'"))
              (aligners (markdown-ts--table-aligners table))
              (cols (treesit-node-children delimiter-row 'named))
              (ncols (length cols))
              (cell-text (make-hash-table :size (* nrows ncols) :test 'equal)))
    (without-restriction
      ;; Collect cell text and invert coordinates.
      (dotimes (x nrows)
        (let* ((row (nth x rows))
               (cells (treesit-node-children row 'named)))
          (dotimes (y ncols)
            ;; Skip nil cells--the parser got confused.
            (when-let* ((cell (nth y cells))
                        (text (string-trim-right
                               (treesit-node-text cell 'no-property))))
              (puthash (cons y x) text cell-text)))))

      (let ((source-buffer (current-buffer)))
        (with-temp-buffer
          (dotimes (x ncols)
            ;; Synthesize a delimiter row.
            (when (eq x 1)
              (dotimes (_y (1- nrows))
                (insert "|"
                        (markdown-ts--table-make-aligner
                         (max 5 markdown-ts-table-default-column-width)
                         'unspecified)))
              (insert "|" "\n"))
            (dotimes (y nrows)
              ;; Skip the original delimiter row.
              (unless (eq y 1)
                ;; Confused cells have nil text.
                (let ((text (or (gethash (cons x y) cell-text) "")))
                  (insert "|" text))))
            (insert "|" "\n"))
          (let ((temp-buffer (current-buffer)))
            (with-current-buffer source-buffer
              (replace-region-contents beg end temp-buffer))))))
    (when (or (eq markdown-ts-table-auto-align t)
              (memq 'transpose markdown-ts-table-auto-align))
      (markdown-ts-table-align-table))))

(defun markdown-ts-table-convert-csv-region ( beg end
                                              &optional
                                              header-line replace)
  "Convert the comma-separated region BEG to END to a Markdown table.
If HEADER-LINE is non-nil, use the first line of the region as the table
column header.  If nil, infer the number of columns and synthesize
column names from the first line.

Note: Body columns beyond the provided or inferred number of columns are
dropped.

If REPLACE is non-nil, overwrite the region BEG END with the Markdown
table, otherwise insert the table after END.

With a single prefix argument, HEADER-LINE is non-nil.
With a double prefix argument, REPLACE is non-nil.
With a triple prefix argument, both are non-nil."
  (interactive "R")
  (markdown-ts-table-convert-region beg end header-line replace 'csv))

(defun markdown-ts-table-convert-tsv-region ( beg end
                                              &optional
                                              header-line replace)
  "Convert the tab-separated region BEG to END to a Markdown table.
If HEADER-LINE is non-nil, use the first line of the region as the table
column header.  If nil, infer the number of columns and synthesize
column names from the first line.

Note: Body columns beyond the provided or inferred number of columns are
dropped.

If REPLACE is non-nil, overwrite the region BEG END with the Markdown
table, otherwise insert the table after END.

With a single prefix argument, HEADER-LINE is non-nil.
With a double prefix argument, REPLACE is non-nil.
With a triple prefix argument, both are non-nil."
  (interactive "R")
  (markdown-ts-table-convert-region beg end header-line replace 'tsv))

(defun markdown-ts-table-convert-region ( beg end
                                          &optional
                                          header-line replace format)
  "Convert the comma-separated region BEG to END to a Markdown table.
If HEADER-LINE is non-nil, use the first line of the region as the table
column header.  If nil, infer the number of columns and synthesize
column names from the first line.

Note: Body columns beyond the provided or inferred number of columns are
dropped.

If REPLACE is non-nil, overwrite the region BEG END with the Markdown
table, otherwise insert the table after END.

FORMAT is one of the symbols `csv' for comma-separated values, or `tsv'
for tab-separated values.  If any other value, use `csv'.

With a single prefix argument, HEADER-LINE is non-nil.
With a double prefix argument, REPLACE is non-nil.
With a triple prefix argument, both are non-nil."
  (interactive "R")
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-convert-region)

  (cond ((equal current-prefix-arg '(4))
         (setq header-line t))
        ((equal current-prefix-arg '(16))
         (setq replace t))
        ((equal current-prefix-arg '(64))
         (setq header-line t
               replace t)))
  (let (rows)
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (pcase format
          ('tsv
             (push
              (split-string
               (buffer-substring-no-properties (point) (pos-eol)) "\t")
              rows))
           (_
            (let (row)
              (while (re-search-forward
                      "\"\\(?:[^\"]\\|\"\"\\)*\"\\|[^,]+\\|\"\""
                      (pos-eol) t)
                (push
                 (replace-regexp-in-string
                  "\"\"" "\""
                  (replace-regexp-in-string
                   "\\`\"\\(\.*\\)\"\\'"
                   "\\1"
                   (substring-no-properties (match-string 0))))
                 row))
              (push (nreverse row) rows))))
        (forward-line 1))
        (setq rows (nreverse rows))
        (cond (replace (delete-region beg end))
              (t (goto-char end)))
        (if header-line
            (markdown-ts-table-insert-table (1- (length rows))
                                            (length (car rows))
                                            (car rows)
                                            (cdr rows))
          (markdown-ts-table-insert-table (length rows)
                                          (length (car rows))
                                          nil
                                          rows)))))

(defun markdown-ts-table-export-table-csv (&optional display)
  "Export the Markdown table at point to comma-separated in a buffer.
If DISPLAY is non-nil, or with a prefix argument, display the export
buffer after exporting.
If point is not at a table, do nothing."
  (interactive "P")
  (markdown-ts-table-export-table display 'csv))

(defun markdown-ts-table-export-table-tsv (&optional display)
  "Export the Markdown table at point to tab-separated in a buffer.
If DISPLAY is non-nil, or with a prefix argument, display the export
buffer after exporting.
If point is not at a table, do nothing."
  (interactive "P")
  (markdown-ts-table-export-table display 'tsv))

(defun markdown-ts-table-export-table (&optional display format)
  "Export the Markdown table at point to CSV in a buffer.
If DISPLAY is non-nil, or with a prefix argument, display the export
buffer after exporting.
FORMAT is one of the symbols `csv' for comma-separated values aka CSV,
or `tsv' for tab-separated values aka TSV.  If any other value, use
`csv'.  If TSV, replace any tabs within a field with a space.
Do not export the table's header delimiter row.
If point is not at a table, do nothing."
  (interactive "P")
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-export-table)
  (when-let* ((at-table (markdown-ts-at-table-p nil t))
              (table (cdr at-table))
              (rows (treesit-node-children table 'named))
              (header (treesit-search-subtree table "\\`pipe_table_header\\'"))
              (delimiter-row (treesit-search-subtree
                              table "\\`pipe_table_delimiter_row\\'"))
              (export-buffer (get-buffer-create
                              markdown-ts-table-export-buffer)))
    (with-current-buffer export-buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (widen)
        (erase-buffer)
        (dolist (row rows)
          (unless (equal row delimiter-row)
            (let* ((elts (treesit-node-children row 'named))
                   (len (length elts)))
              (dotimes (x len)
                (let ((s (string-trim (treesit-node-text (nth x elts)
                                                         'no-property))))
                  (pcase format
                    ('tsv
                     (insert (replace-regexp-in-string "\t" "\s" s))
                     (when (< x (1- len))
                       (insert "\t")))
                    (_
                     (insert
                      (if (string-match "[\",]" s)
                          (concat "\""
                                  (mapconcat
                                   'identity (split-string s "\"") "\"\"")
                                  "\"")
                        s))
                     (when (< x (1- len))
                       (insert ","))))))
              (insert "\n"))))
        (goto-char (point-min))))
    (when display
      (display-buffer export-buffer))))

;;; Helpers:

(defun markdown-ts--outline-invisible-p (pos)
  "Return non-nil if POS is inside an outline-folded region."
  (cl-some (lambda (ov) (eq (overlay-get ov 'invisible) 'outline))
           (overlays-at pos)))

(defun markdown-ts--range-settings ()
  "Return range settings for `markdown-ts-mode'."
  (apply
   #'treesit-range-rules
   `( :embed markdown-inline
      :host markdown
      :local t
     ((inline) @markdown-inline)
     ,@(when markdown-ts-fontify-code-blocks-natively
         '( :embed markdown-ts--code-block-ts-language
            :host markdown
            :local t
            ((fenced_code_block (info_string (language) @language)
                                (code_fence_content) @content)))))))

(defun markdown-ts--remove-image-overlays ()
  "Remove all inline image overlays from the current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'markdown-ts-image)
      (delete-overlay ov))))

(defun markdown-ts--outline-view-change ()
  "Update image overlays after outline fold/unfold.
Removes image overlays in folded regions and triggers
refontification so that images in unfolded regions are recreated."
  (when markdown-ts-inline-images
    (markdown-ts--remove-image-overlays)
    (font-lock-flush)))

(defun markdown-ts--set-hide-markup (value)
  "Set hiding of Markdown markup delimiters in the current buffer.
VALUE non-nil hides markup, nil shows it."
  (if value
      (add-to-invisibility-spec 'markdown-ts--markup)
    (remove-from-invisibility-spec 'markdown-ts--markup))
  (when markdown-ts-inline-images
    (markdown-ts--remove-image-overlays))
  (font-lock-flush))

(defun markdown-ts-toggle-hide-markup ()
  "Toggle hiding of Markdown markup delimiters in the current buffer."
  (interactive)
  (setq markdown-ts-hide-markup (not markdown-ts-hide-markup))
  (markdown-ts--set-hide-markup markdown-ts-hide-markup))

(defun markdown-ts--set-inline-images (value)
  "Set display of inline images in the current buffer.
VALUE non-nil displays images, nil removes them."
  (if value
      (treesit-font-lock-recompute-features '(image-preview))
    (treesit-font-lock-recompute-features nil '(image-preview)))
  (markdown-ts--remove-image-overlays)
  (font-lock-flush))

(defun markdown-ts-toggle-inline-images ()
  "Toggle display of inline images in the current buffer."
  (interactive)
  (setq markdown-ts-inline-images (not markdown-ts-inline-images))
  (markdown-ts--set-inline-images markdown-ts-inline-images)
  (message "Inline images %s" (if markdown-ts-inline-images
                                  "enabled" "disabled")))

;;; Emphasis:

(defvar markdown-ts-emphasis-alist
  '((?b . "**")
    (?B . "__")
    (?i . "*")
    (?I . "_")
    (?s . "~~")
    (?c . "`")
    (?a . "***"))
  "Alist of emphasis markers for `markdown-ts-emphasize'.
Each entry is (KEY . MARKER) where KEY is a character (not a string) and
MARKER is the Markdown emphasized text prefix/suffix.")

(defun markdown-ts--emphasis-node-at-point ()
  "Return the emphasis node at point, or nil.
Finds the innermost `emphasis', `strong_emphasis',
`strikethrough', or `code_span' node."
  (when-let* ((node (treesit-node-at (point) 'markdown-inline)))
    (treesit-parent-until
     node (lambda (n)
            (member (treesit-node-type n)
                    '("emphasis" "strong_emphasis"
                      "strikethrough" "code_span"))))))

(defun markdown-ts-remove-emphasis ()
  "Remove emphasis around point or region.
With an active region, strip the outermost emphasis markers from
the selected text.  Without a region, find the emphasis node at
point using tree-sitter and remove its markers."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (buffer-substring beg end))
             (stripped nil))
        (cl-loop for (_key . m) in (sort (copy-sequence
                                          markdown-ts-emphasis-alist)
                                         (lambda (a b)
                                           (> (length (cdr a))
                                              (length (cdr b)))))
                 for re = (concat "\\`" (regexp-quote m)
                                  "\\(\\(?:.\\|\n\\)*\\)"
                                  (regexp-quote m) "\\'")
                 when (string-match re text)
                 do (setq stripped (match-string 1 text))
                 and return nil)
        (when stripped
          (delete-region beg end)
          (insert stripped)))
    ;; Find the outermost emphasis node at point.
    (when-let* ((inner (markdown-ts--emphasis-node-at-point))
                (node (let ((n inner))
                        (while (let ((parent (treesit-node-parent n)))
                                 (when (and parent
                                            (member (treesit-node-type parent)
                                                    '("emphasis"
                                                      "strong_emphasis"
                                                      "strikethrough"
                                                      "code_span")))
                                   (setq n parent))))
                        n))
                (start (treesit-node-start node))
                (end (treesit-node-end node))
                (text (treesit-node-text node t))
                (type (treesit-node-type node))
                (mlen (pcase type
                        ("strong_emphasis"
                         (if (string-prefix-p "_" text) 2 2))
                        ("emphasis"
                         (if (string-prefix-p "_" text) 1 1))
                        ("strikethrough" 2)
                        ("code_span" 1)))
                (inner-text (substring text mlen (- (length text) mlen))))
      (let ((offset (- (point) start mlen)))
        (delete-region start end)
        (goto-char start)
        (insert inner-text)
        (goto-char (+ start (max 0 offset)))))))

(defun markdown-ts-emphasize (&optional char)
  "Insert or change emphasis on text.
If there is an active region, wrap it with emphasis markers.
If there is no region, insert marker pairs and place point between
them.  CHAR selects the emphasis type:

  b   **bold**       B  __bold__
  i   *italic*       I  _italic_
  a   ***bold+italic***
  s   ~~strikethrough~~
  c   `code`
  SPC remove emphasis at point or region"
  (interactive
   "cEmphasis [b]old [B]old_ [i]talic [I]talic_ [a]ll [s]trike [c]ode SPC remove:")
  (if (eq char ?\s)
      (markdown-ts-remove-emphasis)
    (if-let* ((marker (cdr (assq char markdown-ts-emphasis-alist))))
        (if (use-region-p)
            (let ((beg (region-beginning))
                  (end (copy-marker (region-end))))
              (save-excursion
                (goto-char end)
                (insert marker)
                (goto-char beg)
                (insert marker)))
          (if-let* ((bounds (bounds-of-thing-at-point 'word)))
              (save-excursion
                (goto-char (cdr bounds))
                (insert marker)
                (goto-char (car bounds))
                (insert marker))
            (insert marker marker)
            (backward-char (length marker))))
      (user-error "No such emphasis marker: %c" char))))

;;; Block structure:

(defun markdown-ts-insert-structure (&optional char)
  "Insert a block structure.
If there is an active region, wrap it.  Otherwise, insert an empty
block and place point inside.  CHAR selects the structure type:

  `  fenced code block (```)
  ~  tilde fenced code block (~~~)
  q  block quote (> )
  d  horizontal divider/rule (---)
  t  table"
  (interactive
   "cStructure [`]back-tick code block [~]tilde code block [q]uote [d]ivider [t]able:")
  (pcase char
    (?` (markdown-ts--insert-code-block ?`))
    (?~ (markdown-ts--insert-code-block ?~))
    (?q (markdown-ts--insert-block-quote))
    (?d (markdown-ts--insert-divider))
    (?t (markdown-ts-table-insert-table))
    (_ (user-error "No such structure: %c" char))))

(defun markdown-ts--insert-code-block (char &optional language)
  "Insert a fenced code block using a CHAR sequence.
CHAR should be one of a backtick or a tilde.
LANGUAGE is a whitespace free language string."
  (setq language
        (or language
            (replace-regexp-in-string
             "[[:blank:]]" ""
             (let ((completion-ignore-case nil)
                   (candidates
                    (seq-map
                     #'symbol-name
                     (seq-uniq
                      (append (mapcar #'car markdown-ts--code-block-languages)
                              (mapcar #'car markdown-ts--code-block-non-ts-modes)
                              (mapcar #'car markdown-ts-code-block-modes))))))
               (completing-read
                "Code block language: "
                candidates
                nil
                'confirm ; Prompt if the language is not on our list.
                nil
                'markdown-ts-language-history)))))
  (let ((fence-string (make-string 3 char)))
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (unless (bolp) (insert "\n"))
            (insert fence-string "\n")
            (goto-char beg)
            (insert fence-string language "\n")))
      (insert fence-string language "\n\n" fence-string)
      (forward-line -1))))

(defun markdown-ts--insert-block-quote ()
  "Insert a block quote."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (insert "> ")
            (forward-line 1))))
    (insert "> ")))

(defun markdown-ts--insert-divider ()
  "Insert a horizontal rule."
  (unless (bolp) (insert "\n"))
  (insert "\n---\n\n"))

(defun markdown-ts--apply-ellipsis ()
  "Apply `markdown-ts-ellipsis' to the current buffer's display table."
  (if (and (stringp markdown-ts-ellipsis)
           (not (string-empty-p markdown-ts-ellipsis)))
      (let ((table (or buffer-display-table
                       (make-display-table))))
        (set-display-table-slot
         table 4
         (vconcat (mapcar (lambda (c) (make-glyph-code c 'shadow))
                          markdown-ts-ellipsis)))
        (setq buffer-display-table table))
    (when buffer-display-table
      (set-display-table-slot buffer-display-table 4 nil))))

;;; Major mode:

(defun markdown-ts-outline-cycle ()
  "Cycle visibility of the heading at point.
On a heading, call `outline-cycle'.  Otherwise do nothing."
  (interactive)
  (when (outline-on-heading-p)
    (outline-cycle)))

(defvar-keymap markdown-ts-mode-map
  :doc "Keymap for `markdown-ts-mode'."
  "M-<left>"    #'markdown-ts-promote
  "M-<right>"   #'markdown-ts-demote
  "M-<up>"      #'markdown-ts-move-subtree-up
  "M-<down>"    #'markdown-ts-move-subtree-down
  "C-c C-n"     #'outline-next-heading
  "C-c C-p"     #'outline-previous-heading
  "C-c C-u"     #'outline-up-heading
  "C-c C-f"     #'outline-forward-same-level
  "C-c C-b"     #'outline-backward-same-level
  "C-c C-x C-f" #'markdown-ts-emphasize
  "C-c C-x C-m" #'markdown-ts-toggle-hide-markup
  "C-c C-x C-v" #'markdown-ts-toggle-inline-images
  "C-c C-c"     #'markdown-ts-toggle-checkbox
  "C-c C-r"     #'markdown-ts-renumber-list
  "C-c C-,"     #'markdown-ts-insert-structure
  "C-c C-v n"   #'markdown-ts-move-to-next-code-block
  "C-c C-v p"   #'markdown-ts-move-to-previous-code-block
  "RET"         #'markdown-ts-newline
  "M-RET"       #'markdown-ts-insert-list-item
  "TAB"         #'markdown-ts-outline-cycle)

(defvar-keymap markdown-ts-view-mode-map
  :doc "Keymap for `markdown-ts-view-mode'."
  :parent special-mode-map
  :menu nil
  "g"           #'ignore ; Override special-mode-map #'revert-buffer
  "C-c C-n"     #'outline-next-heading
  "n"           #'outline-next-heading
  "C-c C-p"     #'outline-previous-heading
  "p"           #'outline-previous-heading
  "C-c C-u"     #'outline-up-heading
  "u"           #'outline-up-heading
  "C-c C-f"     #'outline-forward-same-level
  "f"           #'outline-forward-same-level
  "C-c C-b"     #'outline-backward-same-level
  "b"           #'outline-backward-same-level
  "C-c C-x C-m" #'markdown-ts-toggle-hide-markup
  "C-c C-x C-v" #'markdown-ts-toggle-inline-images
  "C-c C-v n"   #'markdown-ts-move-to-next-code-block
  "C-c C-v p"   #'markdown-ts-move-to-previous-code-block
  "TAB"         #'markdown-ts-outline-cycle)

(defvar-keymap markdown-ts-code-block-in-context-mode-map
  :doc "Keymap for `markdown-ts-code-block-in-context-mode'.
These override keys in `markdown-ts-mode-map' to support executing their
commands in a code-block context."
  :parent markdown-ts-mode-map
  :menu nil
  "M-."         #'markdown-ts--code-block-xref-find-definitions
  "TAB"         #'indent-for-tab-command
  "RET"         #'markdown-ts--code-block-newline
  "C-j"         #'markdown-ts--code-block-newline
  "M-RET"       #'markdown-ts--code-block-newline
  "M-q"         #'markdown-ts--code-block-fill-paragraph)

(defvar-keymap markdown-ts-in-table-mode-map
  :doc "Keymap for `markdown-ts-in-table-mode'.
These override keys in `markdown-ts-mode-map' to support executing their
commands in a table context."
  :parent markdown-ts-mode-map
  :menu nil
  "<return>"    #'markdown-ts-table-next-row
  "S-<return>"  #'markdown-ts-table-previous-row
  "<tab>"       #'markdown-ts-table-next-cell
  "<backtab>"   #'markdown-ts-table-previous-cell
  "M-<up>"      #'markdown-ts-table-move-row-up
  "M-<down>"    #'markdown-ts-table-move-row-down
  "M-<left>"    #'markdown-ts-table-move-column-left
  "M-<right>"   #'markdown-ts-table-move-column-right
  "M-S-<up>"    #'markdown-ts-table-insert-row-above
  "M-S-<down>"  #'markdown-ts-table-delete-row
  "M-S-<right>" #'markdown-ts-table-insert-column-left
  "M-S-<left>"  #'markdown-ts-table-delete-column
  "C-c C-t a"   #'markdown-ts-table-align-column
  "C-c C-c"     #'markdown-ts-table-align-table
  "C-c C-t t"   #'markdown-ts-table-transpose-table)

(easy-menu-define markdown-ts-mode-menu markdown-ts-mode-map
  "`markdown-ts-mode' menu."
  '("Markdown" :visible markdown-ts-menu-bar-show
    "--"
    ["Cycle Outline Visibility"  markdown-ts-outline-cycle              :help "Cycle heading visibility from point"]
    ["Toggle Hide Markup"        markdown-ts-toggle-hide-markup         :help "Toggle display of markup characters" :style toggle :selected markdown-ts-hide-markup]
    ["Toggle Inline Images"      markdown-ts-toggle-inline-images       :help "Toggle display of inline images below links" :style toggle :selected markdown-ts-inline-images]
    "--"
    ["Up"                        outline-up-heading                     :help "Move to the parent heading"]
    ["Next"                      outline-next-heading                   :help "Move to the next heading"]
    ["Previous"                  outline-previous-heading               :help "Move to the previous heading"]
    ["Next Same Level"           outline-forward-same-level             :help "Move to the next heading at the same level"]
    ["Previous Same Level"       outline-backward-same-level            :help "Move to the previous heading at the same level"]
    "--"
    ["Jump"                      imenu                                  :help "Jump to a heading via completion"]
    "--"
    ["Move Subtree Up"           markdown-ts-move-subtree-up            :help "Move the current section or list item up"]
    ["Move Subtree Down"         markdown-ts-move-subtree-down          :help "Move the current section or list item down"]
    ["Promote"                   markdown-ts-promote                    :help "Promote heading or list item"]
    ["Demote"                    markdown-ts-demote                     :help "Demote heading or list item"]
    "--"
    ["Emphasis..."               markdown-ts-emphasize                  :help "Add or change emphasis on region"]
    ["Add Block Structure"       markdown-ts-insert-structure           :help "Insert a code block, quote, or other structure"]
    "--"
    ["Toggle Checkbox"           markdown-ts-toggle-checkbox            :help "Toggle task list checkbox at point"]
    ["New List Item"             markdown-ts-insert-list-item           :help "Insert a new list item after the current one"]
    ["Renumber List"             markdown-ts-renumber-list              :help "Renumber the ordered list at point"]
    "--"
    ("Table"
     ["Insert table"             markdown-ts-table-insert-table         :help "Insert an empty table at point"]
     ["Delete table"             markdown-ts-table-delete-table         :help "Delete the table at point"]
     "--"
     ["Next cell"                markdown-ts-table-next-cell            :help "Move point to the next cell"]
     ["Previous cell"            markdown-ts-table-previous-cell        :help "Move point to the previous cell"]
     "--"
     ["Next row"                 markdown-ts-table-next-row             :help "Move point to the next row"]
     ["Previous row"             markdown-ts-table-previous-row         :help "Move point to the previous row"]
     ["Insert row below"         markdown-ts-table-insert-row-below     :help "Insert an empty row below point"]
     ["Insert row above"         markdown-ts-table-insert-row-above     :help "Insert an empty row above point"]
     ["Clone row below"          markdown-ts-table-clone-row-below      :help "Clone the current row below it"]
     ["Clone row above"          markdown-ts-table-clone-row-above      :help "Clone the current row above it"]
     ["Move row up"              markdown-ts-table-move-row-up          :help "Move the row at point up"]
     ["Move row down"            markdown-ts-table-move-row-down        :help "Move the row at point down"]
     ["Delete row"               markdown-ts-table-delete-row           :help "Delete the row at point"]
     "--"
     ["Insert column left"       markdown-ts-table-insert-column-left   :help "Insert an empty column to the left of point"]
     ["Insert column right"      markdown-ts-table-insert-column-right  :help "Insert an empty column to the right of point"]
     ["Clone column left"        markdown-ts-table-clone-column-left    :help "Insert an empty column to the left of point"]
     ["Clone column right"       markdown-ts-table-clone-column-right   :help "Insert an empty column to the right of point"]
     ["Move column left"         markdown-ts-table-move-column-left     :help "Move the current column to its left"]
     ["Move column right"        markdown-ts-table-move-column-right    :help "Move the current column to its right"]
     ["Align column..."          markdown-ts-table-align-column         :help "Align column at point to the left, center, or right"]
     ["Delete column"            markdown-ts-table-delete-column        :help "Delete the column at point"]
     "--"
     ["Align table columns"      markdown-ts-table-align-table          :help "Align the table at point in an easy-to-edit format"]
     ["Transpose table"          markdown-ts-table-transpose-table      :help "Transpose the table at point swapping its rows and columns"]
     "--"
     ["Convert CSV to table"     markdown-ts-table-convert-csv-region   :help "Convert a range of comma-separated values to a new table at point"]
     ["Convert TSV to table"     markdown-ts-table-convert-tsv-region   :help "Convert a range of tab-separated values to a new table at point"]
     ["Export table to CSV"      markdown-ts-table-export-table-csv     :help "Export the table at point to a comma-separated values buffer"]
     ["Export table to TSV"      markdown-ts-table-export-table-tsv     :help "Export the table at point to a tab-separated values buffer"])))

(defun markdown-ts--set-up ()
  "Set up the buffer for `markdown-ts-mode'.
If `markdown-ts--set-up-inline' is non-nil, use a lightweight set up for
embedded inline `markdown-ts-mode' buffers.

NOTE: Call this function only when the treesit `markdown' and
`markdown-inline' parsers are available."

  ;; Set these up for both master and inline for code-block buffers.
  (setq-local comment-start "<!-- "
              comment-end " -->"
              comment-start-skip "<!--[ \t]*"
              comment-end-skip "[ \t]*-->"
              comment-use-syntax nil)

  (setq-local fill-paragraph-function
              #'markdown-ts--fill-paragraph
              fill-forward-paragraph-function
              #'markdown-ts--fill-forward-paragraph)

  ;; `adaptive-fill-function' takes precedence over
  ;; `adaptive-fill-regexp'; the default regexp is fine as a
  ;; fallback for plain (non-list) paragraphs.
  (setq-local adaptive-fill-function #'markdown-ts--adaptive-fill)

  ;; Create and configure the parsers.
  (setq treesit-primary-parser
        (treesit-parser-create 'markdown))

  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-settings markdown-ts--treesit-settings)
  (setq-local treesit-font-lock-feature-list '((delimiter heading)
                                               (paragraph)
                                               (paragraph-inline)
                                               (image-preview error)))

  (cond (markdown-ts--set-up-inline
         (treesit-parser-create 'markdown-inline)
         (setq-local treesit-range-settings
                     (treesit-range-rules
                      :embed 'markdown-inline
                      :host 'markdown
                      '((inline) @markdown-inline))))
        (t
         ;; Range settings differ in the master buffer vs. inline above.
         (setq-local treesit-range-settings (markdown-ts--range-settings))

         ;; Configure features needed only in the master buffer.

         ;; Imenu support.
         (setq-local treesit-simple-imenu-settings
                     `(("Headings" ,#'markdown-ts--imenu-heading-node-p
                        nil ,#'markdown-ts--imenu-heading-name-function)
                       ("Code Blocks" ,#'markdown-ts--imenu-code-block-node-p
                        nil ,#'markdown-ts--imenu-code-block-name-function)))

         ;; Outline support.
         (setq-local treesit-outline-predicate #'markdown-ts--outline-predicate)
         (setq-local outline-minor-mode-cycle t)
         (outline-minor-mode 1)
         (markdown-ts--apply-ellipsis)

         ;; NOTE: `outline-view-change-hook' was obsoleted in 29.1 (commit
         ;; 53b1e6f96cb) on the grounds that only lazy-lock used it, but no
         ;; replacement was provided and outline.el itself still runs the hook
         ;; in 10+ places.
         (with-suppressed-warnings ((obsolete outline-view-change-hook))
           (add-hook 'outline-view-change-hook
                     #'markdown-ts--outline-view-change nil t))

         (progn
           (make-local-variable 'font-lock-extra-managed-props)
           (dolist (prop '(invisible display button category action help-echo))
             (add-to-list 'font-lock-extra-managed-props prop)))

         (when (treesit-ready-p 'html t)
           (treesit-parser-create 'html)
           (require 'html-ts-mode)
           (defvar html-ts-mode--font-lock-settings)
           (defvar html-ts-mode--treesit-font-lock-feature-list)
           (setq-local treesit-font-lock-settings
                       (append treesit-font-lock-settings
                               html-ts-mode--font-lock-settings))
           (setq-local treesit-font-lock-feature-list
                       (treesit-merge-font-lock-feature-list
                        treesit-font-lock-feature-list
                        html-ts-mode--treesit-font-lock-feature-list))
           (setq-local treesit-range-settings
                       (append treesit-range-settings
                               (treesit-range-rules
                                :embed 'html
                                :host 'markdown
                                :local t
                                '((html_block) @html)

                                :embed 'html
                                :host 'markdown-inline
                                '((html_tag) @html)))))

         (when (treesit-ready-p 'yaml t)
           (require 'yaml-ts-mode)
           (defvar yaml-ts-mode--font-lock-settings)
           (defvar yaml-ts-mode--font-lock-feature-list)
           (setq-local treesit-font-lock-settings
                       (append treesit-font-lock-settings
                               yaml-ts-mode--font-lock-settings))
           (setq-local treesit-font-lock-feature-list
                       (treesit-merge-font-lock-feature-list
                        treesit-font-lock-feature-list
                        yaml-ts-mode--font-lock-feature-list))
           (setq-local treesit-range-settings
                       (append treesit-range-settings
                               (treesit-range-rules
                                :embed 'yaml
                                :host 'markdown
                                :local t
                                '((minus_metadata) @yaml)))))

         (when (treesit-ready-p 'toml t)
           (require 'toml-ts-mode)
           (defvar toml-ts-mode--font-lock-settings)
           (defvar toml-ts-mode--font-lock-feature-list)
           (setq treesit-font-lock-settings
                 (append treesit-font-lock-settings
                         toml-ts-mode--font-lock-settings))
           (setq-local treesit-font-lock-feature-list
                       (treesit-merge-font-lock-feature-list
                        treesit-font-lock-feature-list
                        toml-ts-mode--font-lock-feature-list))
           (setq-local treesit-range-settings
                       (append treesit-range-settings
                               (treesit-range-rules
                                :embed 'toml
                                :host 'markdown
                                :local t
                                '((plus_metadata) @toml)))))

         ;; Support for executing commands in a code-block context.
         (when markdown-ts-enable-code-block-context-mode
           (markdown-ts-code-block-context-mode))

         ;; Support for table mode.
         (when markdown-ts-enable-table-mode
           (markdown-ts-table-mode))))

  (treesit-major-mode-setup)

  ;; Do not enable `jit-lock-mode' in indirect buffers such as the one
  ;; we use for code block commands.
  (unless (buffer-base-buffer)
    (jit-lock-register #'markdown-ts--fontify-bare-uri))

  (unless markdown-ts--set-up-inline
    ;; Order matters: `markdown-ts--set-hide-markup' calls `font-lock-flush'
    ;; (only meaningful once `treesit-major-mode-setup' has wired up
    ;; font-lock), and `markdown-ts-default-folding' calls outline
    ;; commands that rely on `outline-search-function', which
    ;; `treesit-major-mode-setup' installs from `treesit-outline-predicate'.
    (markdown-ts--set-hide-markup markdown-ts-hide-markup)
    ;; Respect the user's default outline folding.
    (pcase markdown-ts-default-folding
      ('show-all (ignore))
      ('fold-all (outline-hide-sublevels 1))
      ('fold-headings (outline-show-all)
                      (outline-hide-region-body (point-min) (point-max))))))

(defun markdown-ts-mode-install-parsers (arg)
  "Install `markdown-ts-mode' tree-sitter language parsers.
If needed, install the required parsers for `markdown' and `markdown-inline'.
With a prefix argument, ARG, if needed, install parsers for `html',
`yaml', and `toml'."
  (interactive "P")
  (unless (treesit-language-available-p 'markdown)
    (treesit-install-language-grammar 'markdown))
  (unless (treesit-language-available-p 'markdown-inline)
    (treesit-install-language-grammar 'markdown-inline))
  (when arg
    (unless (treesit-language-available-p 'html)
      (require 'html-ts-mode)
      (treesit-install-language-grammar 'html))
    (unless (treesit-language-available-p 'yaml)
      (require 'yaml-ts-mode)
      (treesit-install-language-grammar 'yaml))
    (unless (treesit-language-available-p 'toml)
      (require 'toml-ts-mode)
      (treesit-install-language-grammar 'toml))))

(defun markdown-ts-mode--initialize ()
  "Invoke this from major mode definitions after local variable set up."
  (treesit-ensure-installed 'markdown)
  (treesit-ensure-installed 'markdown-inline)
  ;; Bypass `treesit-max-buffer-size' so the mode activates in large
  ;; buffers instead of refusing.  `treesit-ready-p' would otherwise
  ;; refuse and emit a misleading "parsers not found" message even when
  ;; they are installed.  Revisit if `treesit-parser-create' gains its
  ;; own buffer-size guard (see bug#80909).
  (let ((treesit-max-buffer-size most-positive-fixnum))
    (cond ((treesit-ready-p '(markdown markdown-inline) t)
           (markdown-ts--set-up))
          (t
           (warn "markdown-ts-mode cannot be set up; using text-mode.
%s."
                 (if (treesit-available-p)
                     "The tree-sitter parsers `markdown' and `markdown-inline' were not found.
Use the command `markdown-ts-mode-install-parsers' to install them.
With a prefix argument, it can also install optional parsers"
                   "Emacs was built without Tree-sitter support, or could not load Tree-sitter"))
           (text-mode)))))

;;;###autoload
(define-derived-mode markdown-ts-mode text-mode "Markdown"
  "Major mode for editing Markdown using tree-sitter grammar.
NOTE: See `markdown-ts--set-up-inline'."
  (markdown-ts-mode--initialize))

(derived-mode-add-parents 'markdown-ts-mode '(markdown-mode))

;;; View mode:

;;;###autoload
(define-derived-mode markdown-ts-view-mode
  nil ; Intentionally left blank.
  "Markdown View"
  "Major mode for read-only viewing Markdown using tree-sitter grammar."
  ;; NOTE: `markdown-ts-mode' is manually added as a parent to avoid
  ;; invoking its initialization before we set override variables.
  (setq-local markdown-ts-menu-bar-show nil)
  (setq-local markdown-ts-hide-markup t)
  (setq-local markdown-ts-inline-images t)
  (setq-local markdown-ts-hard-line-break-backslash 'hide)
  (setq-local markdown-ts-hard-line-break-space 'hide)
  (setq-local markdown-ts-fontify-code-blocks-natively t)
  (setq-local markdown-ts-enable-code-block-context-mode nil)
  (setq-local markdown-ts-enable-table-mode nil)
  (run-hooks 'markdown-ts-view-mode-pre-init-hook)
  (markdown-ts-mode--initialize)
  (setq buffer-read-only t))

(derived-mode-add-parents 'markdown-ts-view-mode '(markdown-ts-mode special-mode))

;;; Mode utilities:

;;;###autoload
(defun markdown-ts-buffer-string ()
  "Like `buffer-string', and convert overlay properties to text properties."
  (let ((str (buffer-string)))
    (dolist (ov (overlays-in (point-min) (point-max)) str)
      (when-let* ((face (overlay-get ov 'face)))
        (font-lock-append-text-property
         (overlay-start ov) (overlay-end ov) 'face face str)))))

(defun markdown-ts--barf-if-not-mode (&optional context)
  "Signal an error if the current buffer is not a `markdown-ts-mode' buffer.
Prefix the error message with CONTEXT."
  (unless (derived-mode-p 'markdown-ts-mode)
    (user-error "%sis valid only in `markdown-ts-mode' buffers"
                (if context (format "%s: " context) ""))))

(defun markdown-ts-add-final-newline ()
  "Add a final newline to the current buffer, if necessary."
  ;; Inspired by files.el.
  (let ((inhibit-read-only t))
    (when (or (eq (buffer-size) 0)
              (and (/= (char-after (1- (point-max))) ?\n)
                   (not (and (eq selective-display t)
                             (= (char-after (1- (point-max))) ?\r)))))
      (save-excursion
        (goto-char (point-max))
        (insert ?\n)))))

(define-minor-mode markdown-ts-code-block-in-context-mode
  "Minor mode enabled if point is within a fenced code block.
This enables the keymap `markdown-ts-code-block-in-context-mode-map'."
  :group 'markdown-ts
  :lighter markdown-ts-code-block-in-context-mode-lighter
  :interactive nil
  (markdown-ts--code-block-in-context-mode-update-ov))

(defvar-local markdown-ts--code-block-in-context-mode-ov nil)

(defun markdown-ts--code-block-in-context-mode-update-ov ()
  "Manage `markdown-ts--code-block-in-context-mode-ov'."
  (cond (markdown-ts-code-block-in-context-mode
         (let ((beg (get-char-property (point) 'markdown-ts-code-beg-marker))
               (end (get-char-property (point) 'markdown-ts-code-end-marker)))
           (if markdown-ts--code-block-in-context-mode-ov
               (move-overlay markdown-ts--code-block-in-context-mode-ov beg end)
             (setq markdown-ts--code-block-in-context-mode-ov
                   (make-overlay beg end nil t nil)))
           (overlay-put markdown-ts--code-block-in-context-mode-ov
                        'markdown-ts-in-code-block t)
           (overlay-put markdown-ts--code-block-in-context-mode-ov
                        'evaporate t)
           (overlay-put markdown-ts--code-block-in-context-mode-ov
                        'priority '(nil . 20))
           (overlay-put markdown-ts--code-block-in-context-mode-ov
                        'face 'markdown-ts-in-code-block)))
        (t
         (when markdown-ts--code-block-in-context-mode-ov
           (delete-overlay markdown-ts--code-block-in-context-mode-ov)))))

(define-minor-mode markdown-ts-code-block-context-mode
  "Minor mode to enable commands in fenced code block context.
If non-nil and `point' is in a fenced code block, run `indent-for-tab-command',
`newline', et.al., in the mode of the code block."
  :group 'markdown-ts
  :interactive nil
  (markdown-ts--barf-if-not-mode 'markdown-ts-code-block-context-mode)
  (cond (markdown-ts-code-block-context-mode
         ;; Enable the minor mode `markdown-ts-code-block-in-context-mode' and
         ;; its keymap when point is within a code block.
         (add-hook 'post-command-hook
                   #'markdown-ts--enable-code-block-in-context-mode nil 'local)
         ;; If `save-place-mode' or similar is used, point could start within
         ;; a code block so initialize from that state.
         (run-with-timer 0.01 nil
                         #'markdown-ts--enable-code-block-in-context-mode)
         ;; For each eligible command, execute it in a code-block context,
         ;; otherwise in the `markdown-ts-mode' buffer's context.
         (add-hook 'pre-command-hook
                   #'markdown-ts--maybe-run-command-in-code-block nil 'local)
         ;; Prune stale code block overlays when the host parse tree
         ;; changes (e.g., when fenced code block delimiters are
         ;; deleted but the overlay from a prior fontification remains).
         (treesit-parser-add-notifier
          treesit-primary-parser
          #'markdown-ts--host-ranges-notifier))
        (t
         (remove-hook 'post-command-hook
                      #'markdown-ts--enable-code-block-in-context-mode 'local)
         (remove-hook 'pre-command-hook
                      #'markdown-ts--maybe-run-command-in-code-block 'local)
         (treesit-parser-remove-notifier
          treesit-primary-parser
          #'markdown-ts--host-ranges-notifier))))

(define-minor-mode markdown-ts-in-table-mode
  "Minor mode enabled if point is within a table.
This enables the keymap `markdown-ts-in-table-mode-map'."
  :group 'markdown-ts
  :lighter markdown-ts-in-table-mode-lighter
  :interactive nil
  (markdown-ts--in-table-mode-update-ov))

(defvar-local markdown-ts--in-table-mode-ov nil)

(defun markdown-ts--in-table-mode-get-ov (pos)
  "Return the `markdown-ts-in-table-mode' overlay at POS."
  (seq-find (lambda (ov)
              (when (overlay-get ov 'markdown-ts-in-table)
                ov))
            (overlays-at pos)))

(defun markdown-ts--table-tick-update (pos)
  "Update the `markdown-ts-in-table-mode' overlay tick at POS.
If the overlay is not found, do nothing."
  (when-let* ((ov (markdown-ts--in-table-mode-get-ov pos)))
    (overlay-put ov
                 'markdown-ts-in-table-tick
                 (buffer-chars-modified-tick))))

(defun markdown-ts--table-tick-stale-p (pos)
  "Return non-nil if the table at POS might have changed.
It is up to this function's callers to call
`markdown-ts--table-tick-update'."
  (when-let* ((ov (markdown-ts--in-table-mode-get-ov pos)))
    (not (eq (overlay-get ov 'markdown-ts-in-table-tick)
             (buffer-chars-modified-tick)))))

(defun markdown-ts--in-table-mode-update-ov ()
  "Manage `markdown-ts--in-table-mode-ov'."
  (cond (markdown-ts-in-table-mode
         (when-let* ((at-table (markdown-ts-at-table-p nil t))
                     (table (cdr at-table))
                     (beg (treesit-node-start table))
                     (end (treesit-node-end table)))
           (if markdown-ts--in-table-mode-ov
               ;; Move the overlay, if needed, and reset the tick if so.
               (when (not (eq (overlay-start markdown-ts--in-table-mode-ov)
                              beg))
                 (move-overlay markdown-ts--in-table-mode-ov beg end)
                 (overlay-put markdown-ts--in-table-mode-ov
                              'markdown-ts-in-table-tick nil))
             (setq markdown-ts--in-table-mode-ov
                   (make-overlay beg end nil t nil)))
           (overlay-put markdown-ts--in-table-mode-ov
                        'markdown-ts-in-table t)
           (overlay-put markdown-ts--in-table-mode-ov
                        'evaporate t)
           (overlay-put markdown-ts--in-table-mode-ov
                        'priority '(nil . 20))
           (overlay-put markdown-ts--in-table-mode-ov
                        'face 'markdown-ts-in-table)
           ))
        (t
         (when markdown-ts--in-table-mode-ov
           (delete-overlay markdown-ts--in-table-mode-ov)))))

(define-minor-mode markdown-ts-table-mode
  "Minor mode to enable commands in tables.
If non-nil and `point' is in a table, enable
`markdown-ts-in-table-mode'."
  :group 'markdown-ts
  :interactive nil
  (markdown-ts--barf-if-not-mode 'markdown-ts-table-mode)
  (cond (markdown-ts-table-mode
         ;; Enable the minor mode `markdown-ts-in-table-mode' and
         ;; its keymap when point is within a code block.
         (add-hook 'post-command-hook
                   #'markdown-ts--enable-in-table-mode nil 'local)
         ;; If `save-place-mode' or similar is used, point could start within
         ;; a table, so initialize from that state.
         (run-with-timer 0.01 nil
                         #'markdown-ts--enable-in-table-mode))
        (t
         (remove-hook 'post-command-hook
                      #'markdown-ts--enable-in-table-mode 'local))))

;;;###autoload
(defun markdown-ts-mode-maybe ()
  "Enable `markdown-ts-mode' when its grammars are available.
Also propose to install the grammars when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
          (eq treesit-enabled-modes t)
          (memq 'markdown-ts-mode treesit-enabled-modes))
      (markdown-ts-mode)
    (text-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist '("\\.md\\'"        . markdown-ts-mode-maybe))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'"  . markdown-ts-mode-maybe))
  (add-to-list 'auto-mode-alist '("\\.mdx\\'"       . markdown-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(markdown-mode . markdown-ts-mode)))

(provide 'markdown-ts-mode)
;;; markdown-ts-mode.el ends here
