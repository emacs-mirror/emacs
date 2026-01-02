;;; heex-ts-mode.el --- Major mode for Heex with tree-sitter support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Tree-sitter language versions
;;
;; heex-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-heex: v0.7.0
;; - tree-sitter-elixir: v0.3.3
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

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
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(heex "https://github.com/phoenixframework/tree-sitter-heex"
        :commit "f6b83f305a755cd49cf5f6a66b2b789be93dc7b9")
 t)
(add-to-list
 'treesit-language-source-alist
 '(elixir "https://github.com/elixir-lang/tree-sitter-elixir"
          :commit "02a6f7fd4be28dd94ee4dd2ca19cb777053ea74e")
 t)

(defgroup heex-ts nil
  "Major mode for editing HEEx code."
  :prefix "heex-ts-"
  :group 'languages)

(defcustom heex-ts-indent-offset 2
  "Indentation of HEEx statements."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'heex-ts)

;; There seems to be no parent directive block for tree-sitter-heex,
;; so we ignore them for now until we learn how to query them.
;; https://github.com/phoenixframework/tree-sitter-heex/issues/28
(defvar heex-ts--indent-rules
  (let ((offset heex-ts-indent-offset))
    `((heex
       ((parent-is "fragment")
        (lambda (_node _parent bol &rest _)
          ;; If HEEx is embedded indent to parent
          ;; otherwise indent to the bol.
          (if (eq (treesit-language-at (point-min)) 'heex)
              (point-min)
            (save-excursion
              (goto-char (treesit-node-start
                          (treesit-node-at bol 'elixir)))
              (back-to-indentation)
              (point))
            ))
        0)
       ((node-is "end_tag") parent-bol 0)
       ((node-is "end_component") parent-bol 0)
       ((node-is "end_slot") parent-bol 0)
       ((node-is "/>") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((node-is "}") parent-bol 0)
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
     `([(attribute_value) (quoted_attribute_value)] @font-lock-string-face)
     :language 'heex
     :feature 'heex-component
     `([
        (component_name) @font-lock-function-name-face
        (module) @font-lock-keyword-face
        (function) @font-lock-keyword-face
        "." @font-lock-keyword-face
        ])))
  "Tree-sitter font-lock settings.")

(defvar heex-ts--font-lock-feature-list
  '(( heex-comment heex-keyword heex-doctype )
    ( heex-component heex-tag heex-attribute heex-string )
    () ())
  "Tree-sitter font-lock feature list.")

(defun heex-ts--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (pcase (treesit-node-type node)
    ((or "component" "slot" "tag")
     (string-trim
      (treesit-node-text
       (treesit-node-child (treesit-node-child node 0) 1) nil)))
    (_ nil)))

(defvar heex-ts--thing-settings
  `((defun ,(rx bos (or "component" "tag" "slot") eos))
    (sexp
     (not (or (and named
                   ,(rx bos (or "fragment" "comment") eos))
              (and anonymous
                   ,(rx (or "<!" "<" ">" "{" "}"))))))
    (list
     ,(rx bos (or "doctype"
                  "tag"
                  "component"
                  "slot"
                  "expression"
                  "directive"
                  "comment")
          eos))
    (sentence
     ,(rx bos (or "tag_name"
                  "component_name"
                  "attribute")
          eos))
    (text
     ,(rx bos (or "comment" "text") eos)))
  "`treesit-thing-settings' for HEEx.")

(defvar heex-ts--range-rules
  (when (treesit-available-p)
    (treesit-range-rules
     :embed 'elixir
     :host 'heex
     '((directive [(partial_expression_value)
                   (ending_expression_value)]
                  @cap))

     :embed 'elixir
     :host 'heex
     :local t
     '((directive (expression_value) @cap)
       (expression (expression_value) @cap)))))

(defvar elixir-ts--font-lock-settings)
(defvar elixir-ts--font-lock-feature-list)
(defvar elixir-ts--indent-rules)
(defvar elixir-ts--thing-settings)

;;;###autoload
(define-derived-mode heex-ts-mode html-mode "HEEx"
  "Major mode for editing HEEx, powered by tree-sitter."
  :group 'heex-ts

  (when (treesit-ensure-installed 'heex)
    (setq treesit-primary-parser (treesit-parser-create 'heex))

    ;; Comments
    (setq-local treesit-thing-settings
                `((heex ,@heex-ts--thing-settings)))

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (rx bos (or "component" "tag" "slot") eos))
    (setq-local treesit-defun-name-function #'heex-ts--defun-name)

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                '(("Component" "\\`component\\'" nil nil)
                  ("Slot" "\\`slot\\'" nil nil)
                  ("Tag" "\\`tag\\'" nil nil)))

    ;; Outline minor mode
    ;; `heex-ts-mode' inherits from `html-mode' that sets
    ;; regexp-based outline variables.  So need to restore
    ;; the default values of outline variables to be able
    ;; to use `treesit-outline-predicate' derived
    ;; from `treesit-simple-imenu-settings' above.
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)

    (setq-local treesit-font-lock-settings heex-ts--font-lock-settings)

    (setq-local treesit-simple-indent-rules heex-ts--indent-rules)

    (setq-local treesit-font-lock-feature-list
                heex-ts--font-lock-feature-list)

    (when (treesit-ensure-installed 'elixir)
      (require 'elixir-ts-mode)
      (treesit-parser-create 'elixir)

      (setq-local treesit-range-settings heex-ts--range-rules)

      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          elixir-ts--font-lock-settings))
      (setq-local treesit-font-lock-feature-list
                  (treesit-merge-font-lock-feature-list
                   treesit-font-lock-feature-list
                   elixir-ts--font-lock-feature-list))

      (setq-local treesit-simple-indent-rules
                  (append treesit-simple-indent-rules
                          elixir-ts--indent-rules))

      (setq-local treesit-thing-settings
                  (append treesit-thing-settings
                          `((elixir ,@elixir-ts--thing-settings)))))

    (treesit-major-mode-setup)

    ;; Enable the 'sexp' navigation by default
    (setq-local forward-sexp-function #'treesit-forward-sexp
                treesit-sexp-thing 'sexp
                hs-treesit-things '(or defun list))))

(derived-mode-add-parents 'heex-ts-mode '(heex-mode))

;;;###autoload
(defun heex-ts-mode-maybe ()
  "Enable `heex-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'heex)
          (eq treesit-enabled-modes t)
          (memq 'heex-ts-mode treesit-enabled-modes))
      (heex-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  ;; Both .heex and the deprecated .leex files should work
  ;; with the tree-sitter-heex grammar.
  (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(heex-mode . heex-ts-mode)))

(provide 'heex-ts-mode)
;;; heex-ts-mode.el ends here
