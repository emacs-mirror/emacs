;;; markdown-ts-mode.el --- tree sitter support for Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author     : Rahul Martim Juliato <rahul.juliato@gmail.com>
;; Maintainer : Rahul Martim Juliato <rahul.juliato@gmail.com>
;; Created    : April 2024
;; Keywords   : markdown md languages tree-sitter

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

(require 'treesit)
(require 'subr-x)

(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")

(defvar markdown-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'markdown
   :override t
   :feature 'delimiter
   '([ "[" "]" "(" ")" ] @shadow)


   :language 'markdown
   :feature 'paragraph
   '([((setext_heading) @font-lock-keyword-face)
      ((atx_heading) @font-lock-keyword-face)
      ((thematic_break) @shadow)
      ((indented_code_block) @font-lock-string-face)
      (list_item (list_marker_star) @font-lock-keyword-face)
      (list_item (list_marker_plus) @font-lock-keyword-face)
      (list_item (list_marker_minus) @font-lock-keyword-face)
      (list_item (list_marker_dot) @font-lock-keyword-face)
      (fenced_code_block (fenced_code_block_delimiter) @font-lock-doc-face)
      (fenced_code_block (code_fence_content) @font-lock-string-face)
      ((block_quote_marker) @font-lock-string-face)
      (block_quote (paragraph) @font-lock-string-face)
      (block_quote (block_quote_marker) @font-lock-string-face)
      ])

   :language 'markdown-inline
   :feature 'paragraph-inline
   '([
      ((image_description) @link)
      ((link_destination) @font-lock-string-face)
      ((code_span) @font-lock-string-face)
      ((emphasis) @underline)
      ((strong_emphasis) @bold)
      (inline_link (link_text) @link)
      (inline_link (link_destination) @font-lock-string-face)
      (shortcut_link (link_text) @link)])))

(defun markdown-ts-imenu-node-p (node)
  "Check if NODE is a valid entry to imenu."
  (equal (treesit-node-type (treesit-node-parent node))
         "atx_heading"))

(defun markdown-ts-imenu-name-function (node)
  "Return an imenu entry if NODE is a valid header."
  (let ((name (treesit-node-text node)))
    (if (markdown-ts-imenu-node-p node)
	(thread-first (treesit-node-parent node)(treesit-node-text))
      name)))

(defun markdown-ts-setup ()
  "Setup treesit for `markdown-ts-mode'."
  (setq-local treesit-font-lock-settings markdown-ts--treesit-settings)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode markdown-ts-mode text-mode "Markdown"
  "Major mode for editing Markdown using tree-sitter grammar."
  (setq-local font-lock-defaults nil
	          treesit-font-lock-feature-list '((delimiter)
					           (paragraph)
					           (paragraph-inline)))

  (setq-local treesit-simple-imenu-settings
              `(("Headings" markdown-ts-imenu-node-p nil markdown-ts-imenu-name-function)))
  (setq-local treesit-outline-predicate "section")

  (when (treesit-ready-p 'markdown)
    (treesit-parser-create 'markdown-inline)
    (treesit-parser-create 'markdown)
    (markdown-ts-setup)))

(derived-mode-add-parents 'markdown-ts-mode '(markdown-mode))

(if (treesit-ready-p 'markdown)
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode)))

(provide 'markdown-ts-mode)
;;; markdown-ts-mode.el ends here
