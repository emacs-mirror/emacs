;;; c-ts-common.el --- Utilities for C like Languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Maintainer : 付禹安 (Yuan Fu) <casouri@gmail.com>
;; Package    : emacs
;; Keywords   : c c++ java javascript rust languages tree-sitter

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
;; This file contains functions that can be shared by C-like language
;; major modes, like indenting and filling "/* */" block comments.
;;
;; For indenting and filling comments:
;;
;; - Use `c-ts-common-comment-setup' to setup comment variables and
;;   filling.
;;
;; - Use simple-indent matcher `c-ts-common-looking-at-star' and
;;   anchor `c-ts-common-comment-start-after-first-star' for indenting
;;   block comments.  See `c-ts-mode--indent-styles' for example.
;;
;; For indenting statements:
;;
;; - Set `c-ts-common-indent-offset',
;;   `c-ts-common-indent-block-type-regexp', and
;;   `c-ts-common-indent-bracketless-type-regexp', then use simple-indent
;;   offset `c-ts-common-statement-offset' in
;;   `treesit-simple-indent-rules'.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")

;;; Comment indentation and filling

(defun c-ts-common-looking-at-star (_n _p bol &rest _)
  "A tree-sitter simple indent matcher.
Matches if there is a \"*\" after BOL."
  (eq (char-after bol) ?*))

(defun c-ts-common-comment-start-after-first-star (_n parent &rest _)
  "A tree-sitter simple indent anchor.
Finds the \"/*\" and returns the point after the \"*\".
Assumes PARENT is a comment node."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (if (looking-at (rx "/*"))
        (match-end 0)
      (point))))

(defun c-ts-common-comment-2nd-line-matcher (_n parent &rest _)
  "Matches if point is at the second line of a block comment.
PARENT should be a comment node."
  (and (equal (treesit-node-type parent) "comment")
       (save-excursion
         (forward-line -1)
         (back-to-indentation)
         (eq (point) (treesit-node-start parent)))))

(defun c-ts-common-comment-2nd-line-anchor (_n _p bol &rest _)
  "Return appropriate anchor for the second line of a comment.

If the first line is /* alone, return the position right after
the star; if the first line is /* followed by some text, return
the position right before the text minus 1.

Use an offset of 1 with this anchor.  BOL is the beginning of
non-whitespace characters of the current line."
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (when (looking-at comment-start-skip)
      (goto-char (match-end 0))
      (if (looking-at (rx (* (or " " "\t")) eol))
          ;; Only /* at the first line.
          (progn (skip-chars-backward " \t")
                 (if (save-excursion
                       (goto-char bol)
                       (looking-at (rx "*")))
                     ;; The common case.  Checked by "Multiline Block
                     ;; Comments 4".
                     (point)
                   ;; The "Multiline Block Comments 2" test in
                   ;; c-ts-common-resources/indent.erts checks this.
                   (1- (point))))
        ;; There is something after /* at the first line.  The
        ;; "Multiline Block Comments 3" test checks this.
        (1- (point))))))

(defvar c-ts-common--comment-regexp
  ;; These covers C/C++, Java, JavaScript, TypeScript, Rust, C#.
  (rx (or "comment" "line_comment" "block_comment"))
  "Regexp pattern that matches a comment in C-like languages.")

(defun c-ts-common--fill-paragraph (&optional arg)
  "Filling function for `c-ts-common'.
ARG is passed to `fill-paragraph'."
  (interactive "*P")
  (save-restriction
    (widen)
    (let ((node (treesit-node-at (point))))
      (when (string-match-p c-ts-common--comment-regexp
                            (treesit-node-type node))
        (if (save-excursion
              (goto-char (treesit-node-start node))
              (looking-at "//"))
            (fill-comment-paragraph arg)
          (c-ts-common--fill-block-comment arg)))
      ;; Return t so `fill-paragraph' doesn't attempt to fill by
      ;; itself.
      t)))

(defun c-ts-common--fill-block-comment (&optional arg)
  "Filling function for block comments.
ARG is passed to `fill-paragraph'.  Assume point is in a block
comment."
  (let* ((node (treesit-node-at (point)))
         (start (treesit-node-start node))
         (end (treesit-node-end node))
         ;; Bind to nil to avoid infinite recursion.
         (fill-paragraph-function nil)
         (orig-point (point-marker))
         (start-marker (point-marker))
         (end-marker nil)
         (end-len 0))
    (move-marker start-marker start)
    ;; We mask "/*" and the space before "*/" like
    ;; `c-fill-paragraph' does.
    (atomic-change-group
      ;; Mask "/*".
      (goto-char start)
      (when (looking-at (rx (* (syntax whitespace))
                            (group "/") "*"))
        (goto-char (match-beginning 1))
        (move-marker start-marker (point))
        (replace-match " " nil nil nil 1))

      ;; Include whitespaces before /*.
      (goto-char start)
      (beginning-of-line)
      (setq start (point))

      ;; Mask spaces before "*/" if it is attached at the end
      ;; of a sentence rather than on its own line.
      (goto-char end)
      (when (looking-back (rx (not (syntax whitespace))
                              (group (+ (syntax whitespace)))
                              "*/")
                          (line-beginning-position))
        (goto-char (match-beginning 1))
        (setq end-marker (point-marker))
        (setq end-len (- (match-end 1) (match-beginning 1)))
        (replace-match (make-string end-len ?x)
                       nil nil nil 1))

      ;; If "*/" is on its own line, don't included it in the
      ;; filling region.
      (when (not end-marker)
        (goto-char end)
        (when (looking-back (rx "*/") 2)
          (backward-char 2)
          (skip-syntax-backward "-")
          (setq end (point))))

      ;; Let `fill-paragraph' do its thing.
      (goto-char orig-point)
      (narrow-to-region start end)
      (let (para-start para-end)
        (forward-paragraph 1)
        (setq para-end (point))
        (forward-paragraph -1)
        (setq para-start (point))
        ;; We don't want to fill the region between START and
        ;; START-MARKER, otherwise the filling function might delete
        ;; some spaces there.  Also, we only fill the current
        ;; paragraph.
        (fill-region (max start-marker para-start) (min end para-end) arg))

      ;; Unmask.
      (when start-marker
        (goto-char start-marker)
        (delete-char 1)
        (insert "/"))
      (when end-marker
        (goto-char end-marker)
        (delete-region (point) (+ end-len (point)))
        (insert (make-string end-len ?\s)))
      (goto-char orig-point))))

(defun c-ts-common-comment-setup ()
  "Set up local variables for C-like comment.

Set up:
 - `comment-start'
 - `comment-end'
 - `comment-start-skip'
 - `comment-end-skip'
 - `adaptive-fill-mode'
 - `adaptive-fill-first-line-regexp'
 - `paragraph-start'
 - `paragraph-separate'
 - `fill-paragraph-function'"
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                         (seq "/" (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/")))))
  (setq-local adaptive-fill-mode t)
  ;; This matches (1) empty spaces (the default), (2) "//", (3) "*",
  ;; but do not match "/*", because we don't want to use "/*" as
  ;; prefix when filling.  (Actually, it doesn't matter, because
  ;; `comment-start-skip' matches "/*" which will cause
  ;; `fill-context-prefix' to use "/*" as a prefix for filling, that's
  ;; why we mask the "/*" in `c-ts-common--fill-paragraph'.)
  (setq-local adaptive-fill-regexp
              (concat (rx (* (syntax whitespace))
                          (group (or (seq "/" (+ "/")) (* "*"))))
                      adaptive-fill-regexp))
  ;; Note the missing * comparing to `adaptive-fill-regexp'.  The
  ;; reason for its absence is a bit convoluted to explain.  Suffice
  ;; to say that without it, filling a single line paragraph that
  ;; starts with /* doesn't insert * at the beginning of each
  ;; following line, and filling a multi-line paragraph whose first
  ;; two lines start with * does insert * at the beginning of each
  ;; following line.  If you know how does adaptive filling works, you
  ;; know what I mean.
  (setq-local adaptive-fill-first-line-regexp
              (rx bos
                  (seq (* (syntax whitespace))
                       (group (seq "/" (+ "/")))
                       (* (syntax whitespace)))
                  eos))
  ;; Same as `adaptive-fill-regexp'.
  (setq-local paragraph-start
              (rx (or (seq (* (syntax whitespace))
                           (group (or (seq "/" (+ "/")) (* "*")))
                           (* (syntax whitespace))
                           ;; Add this eol so that in
                           ;; `fill-context-prefix', `paragraph-start'
                           ;; doesn't match the prefix.
                           eol)
                      "\f")))
  (setq-local paragraph-separate paragraph-start)
  (setq-local fill-paragraph-function #'c-ts-common--fill-paragraph))

;;; Statement indent

(defvar c-ts-common-indent-offset nil
  "Indent offset used by `c-ts-common' indent functions.

This should be the symbol of the indent offset variable for the
particular major mode.  This cannot be nil for `c-ts-common'
statement indent functions to work.")

(defvar c-ts-common-indent-type-regexp-alist nil
  "An alist of node type regexps.

Each key in the alist is one of `if', `else', `do', `while',
`for', `block', `close-bracket'.  Each value in the alist
is the regexp matching the type of that kind of node.  Most of
these types are self-explanatory, e.g., `if' corresponds to
\"if_statement\" in C.  `block' corresponds to the {} block.

Some types, specifically `else', is usually not identified by a
standalone node, but a child under the \"if_statement\", under a
field name like \"alternative\", etc.  In that case, use a
cons (TYPE . FIELD-NAME) as the value, where TYPE is the node's
parent's type, and FIELD-NAME is the field name of the node.

If the language doesn't have a particular type, it is fine to
omit it.")

(defun c-ts-common--node-is (node &rest types)
  "Return non-nil if NODE is any one of the TYPES.

TYPES can be any of `if', `else', `while', `do', `for', and
`block'.

If NODE is nil, return nil."
  (declare (indent 2))
  (catch 'ret
    (when (null node)
      (throw 'ret nil))
    (dolist (type types)
      (let ((regexp (alist-get
                     type c-ts-common-indent-type-regexp-alist))
            (parent (treesit-node-parent node)))
        (when (and regexp
                   (if (consp regexp)
                       (and parent
                            (string-match-p (car regexp)
                                            (treesit-node-type parent))
                            (treesit-node-field-name node)
                            (string-match-p (cdr regexp)
                                            (treesit-node-field-name
                                             node)))
                     (string-match-p regexp (treesit-node-type node))))
          (throw 'ret t))))
    nil))

(defun c-ts-common-statement-offset (node parent &rest _)
  "Return an indent offset for a statement inside a block.

Assumes the anchor is (point-min), i.e., the 0th column.

This function basically counts the number of block nodes (i.e.,
brackets) (defined by `c-ts-common-indent-block-type-regexp')
between NODE and the root node (not counting NODE itself), and
multiply that by `c-ts-common-indent-offset'.

To support GNU style, on each block level, this function also
checks whether the opening bracket { is on its own line, if so,
it adds an extra level, except for the top-level.

It also has special handling for bracketless statements and
else-if statements, which see.

PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters on the current line."
  (let ((level 0))
    ;; If NODE is a opening/closing bracket on its own line, take off
    ;; one level because the code below assumes NODE is a statement
    ;; _inside_ a {} block.
    (when (c-ts-common--node-is node 'block 'close-bracket)
      (cl-decf level))
    ;; If point is on an empty line, NODE would be nil, but we pretend
    ;; there is a statement node.
    (when (null node)
      (setq node t))
    ;; Go up the tree and compute indent level.
    (while (if (eq node t)
               (setq node parent)
             node)
      (let ((parent (treesit-node-parent node)))
        ;; Increment level for every bracket (with exception).
        (when (c-ts-common--node-is node 'block)
          (cl-incf level)
          (save-excursion
            (goto-char (treesit-node-start node))
            ;; Add an extra level if the opening bracket is on its own
            ;; line, except (1) it's at top-level, or (2) it's immediate
            ;; parent is another block.
            (cond ((bolp) nil) ; Case (1).
                  ((c-ts-common--node-is parent 'block) ; Case (2).
                   nil)
                  ;; Add a level.
                  ((looking-back (rx bol (* whitespace))
                                 (line-beginning-position))
                   (cl-incf level)))))
        ;; Fix bracketless statements.
        (when (and (c-ts-common--node-is parent
                       'if 'do 'while 'for)
                   (not (c-ts-common--node-is node 'block)))
          (cl-incf level))
        ;; Flatten "else if" statements.
        (when (and (c-ts-common--node-is node 'else)
                   (c-ts-common--node-is node 'if)
                   ;; But if the "if" is on it's own line, still
                   ;; indent a level.
                   (not (save-excursion
                          (goto-char (treesit-node-start node))
                          (looking-back (rx bol (* whitespace))
                                        (line-beginning-position)))))
          (cl-decf level)))
      ;; Go up the tree.
      (setq node (treesit-node-parent node)))
    (* level (symbol-value c-ts-common-indent-offset))))

(provide 'c-ts-common)

;;; c-ts-common.el ends here
