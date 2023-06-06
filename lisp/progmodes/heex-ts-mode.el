;;; heex-ts-mode.el --- Major mode for Heex with tree-sitter support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Wilhelm H Kirschbaum <wkirschbaum@gmail.com>
;; Created: November 2022
;; Keywords: elixir languages tree-sitter

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
;; This package provides `heex-ts-mode' which is a major mode for editing
;; HEEx files that uses Tree Sitter to parse the language.
;;
;; This package is compatible with and was tested against the tree-sitter grammar
;; for HEEx found at https://github.com/phoenixframework/tree-sitter-heex.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")

(defgroup heex-ts nil
  "Major mode for editing HEEx code."
  :prefix "heex-ts-"
  :group 'langauges)

(defcustom heex-ts-indent-offset 2
  "Indentation of HEEx statements."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'heex-ts)

(defconst heex-ts--sexp-regexp
  (rx bol
      (or "directive" "tag" "component" "slot"
          "attribute" "attribute_value" "quoted_attribute_value")
      eol))

;; There seems to be no parent directive block for tree-sitter-heex,
;; so we ignore them for now until we learn how to query them.
;; https://github.com/phoenixframework/tree-sitter-heex/issues/28
(defvar heex-ts--indent-rules
  (let ((offset heex-ts-indent-offset))
    `((heex
       ((parent-is "fragment")
        (lambda (node parent &rest _)
          ;; If HEEx is embedded indent to parent
          ;; otherwise indent to the bol.
          (if (eq (treesit-language-at (point-min)) 'heex)
              (point-min)
            (save-excursion
              (goto-char (treesit-node-start parent))
              (back-to-indentation)
              (point))
            )) 0)
       ((node-is "end_tag") parent-bol 0)
       ((node-is "end_component") parent-bol 0)
       ((node-is "end_slot") parent-bol 0)
       ((node-is "/>") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((parent-is "comment") prev-adaptive-prefix 0)
       ((parent-is "component") parent-bol ,offset)
       ((parent-is "tag") parent-bol ,offset)
       ((parent-is "start_tag") parent-bol ,offset)
       ((parent-is "component") parent-bol ,offset)
       ((parent-is "start_component") parent-bol ,offset)
       ((parent-is "slot") parent-bol ,offset)
       ((parent-is "start_slot") parent-bol ,offset)
       ((parent-is "self_closing_tag") parent-bol ,offset)
       (no-node parent-bol ,offset)))))

(defvar heex-ts--font-lock-settings
  (when (treesit-available-p)
    (treesit-font-lock-rules
     :language 'heex
     :feature 'heex-comment
     '((comment) @font-lock-comment-face)
     :language 'heex
     :feature 'heex-doctype
     '((doctype) @font-lock-doc-face)
     :language 'heex
     :feature 'heex-tag
     `([(tag_name) (slot_name)] @font-lock-function-name-face)
     :language 'heex
     :feature 'heex-attribute
     `((attribute_name) @font-lock-variable-name-face)
     :language 'heex
     :feature 'heex-keyword
     `((special_attribute_name) @font-lock-keyword-face)
     :language 'heex
     :feature 'heex-string
     `([(attribute_value) (quoted_attribute_value)] @font-lock-constant-face)
     :language 'heex
     :feature 'heex-component
     `([
        (component_name) @font-lock-function-name-face
        (module) @font-lock-keyword-face
        (function) @font-lock-keyword-face
        "." @font-lock-keyword-face
        ])))
  "Tree-sitter font-lock settings.")

(defun heex-ts--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (pcase (treesit-node-type node)
    ((or "component" "slot" "tag")
     (string-trim
      (treesit-node-text
       (treesit-node-child (treesit-node-child node 0) 1) nil)))
    (_ nil)))

(defun heex-ts--forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With ARG, do it many times.  Negative ARG means move backward."
  (or arg (setq arg 1))
  (funcall
   (if (> arg 0) #'treesit-end-of-thing #'treesit-beginning-of-thing)
   heex-ts--sexp-regexp
   (abs arg)))

;;;###autoload
(define-derived-mode heex-ts-mode html-mode "HEEx"
  "Major mode for editing HEEx, powered by tree-sitter."
  :group 'heex-ts

  (when (treesit-ready-p 'heex)
    (treesit-parser-create 'heex)

    ;; Comments
    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment" "text")))

    (setq-local forward-sexp-function #'heex-ts--forward-sexp)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (rx bol (or "component" "tag" "slot") eol))
    (setq-local treesit-defun-name-function #'heex-ts--defun-name)

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                '(("Component" "\\`component\\'" nil nil)
                  ("Slot" "\\`slot\\'" nil nil)
                  ("Tag" "\\`tag\\'" nil nil)))

    (setq-local treesit-font-lock-settings heex-ts--font-lock-settings)

    (setq-local treesit-simple-indent-rules heex-ts--indent-rules)

    (setq-local treesit-font-lock-feature-list
                '(( heex-comment heex-keyword heex-doctype )
                  ( heex-component heex-tag heex-attribute heex-string )
                  () ()))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'heex)
    ;; Both .heex and the deprecated .leex files should work
    ;; with the tree-sitter-heex grammar.
    (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-ts-mode)))

(provide 'heex-ts-mode)
;;; heex-ts-mode.el ends here
