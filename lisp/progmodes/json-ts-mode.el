;;; json-ts-mode.el --- tree-sitter support for JSON  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : json languages tree-sitter

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
;; json-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-json: v0.24.8-1-g4d770d3
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'rx)
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(json "https://github.com/tree-sitter/tree-sitter-json"
        :commit "4d770d31f732d50d3ec373865822fbe659e47c75")
 t)

(defcustom json-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `json-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'json)

(defvar json-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table for `json-ts-mode'.")


(defvar json-ts--indent-rules
  `((json
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "object") parent-bol json-ts-mode-indent-offset)
     ((parent-is "array") parent-bol json-ts-mode-indent-offset))))

(defvar json-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'json
   :feature 'comment
   '((comment) @font-lock-comment-face)
   :language 'json
   :feature 'bracket
   '((["[" "]" "{" "}"]) @font-lock-bracket-face)
   :language 'json
   :feature 'constant
   '([(null) (true) (false)] @font-lock-constant-face)
   :language 'json
   :feature 'delimiter
   '((["," ":"]) @font-lock-delimiter-face)
   :language 'json
   :feature 'number
   '((number) @font-lock-number-face)
   :language 'json
   :feature 'string
   '((string) @font-lock-string-face)
   :language 'json
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)
   :language 'json
   :feature 'pair
   :override t ; Needed for overriding string face on keys.
   '((pair key: (_) @font-lock-property-use-face))
   :language 'json
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Font-lock settings for JSON.")

(defun json-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "pair" "object")
     (string-trim (treesit-node-text
                   (treesit-node-child-by-field-name
                    node "key")
                   t)
                  "\"" "\""))))

(defun json-ts--get-path-at-node (node)
  "Get the path from the root of the JSON tree to NODE.
Return a list of keys (strings) and indices (numbers).
NODE is a tree-sitter node."
  (let ((path nil)
        (parent nil))
    (while (setq parent (treesit-node-parent node))
      (let ((type (treesit-node-type parent)))
        (cond
         ((equal type "array")
          (push (treesit-node-index node t) path))
         ((equal type "pair")
          (let ((key (treesit-node-child-by-field-name parent "key")))
             (push (treesit-node-text key t) path)))))
      (setq node parent))
    path))

(defun json-ts--path-to-jq (path)
  "Convert PATH list to a jq-style path string.
PATH is a list of keys (strings) and indices (numbers)."
  (mapconcat
   (lambda (x)
     (cond
      ((numberp x) (format "[%d]" x))
      ((stringp x)
       (let ((key (string-trim x "\"" "\"")))
         (if (string-match-p (rx bos (any alpha "_") (* (any alnum "_")) eos) key)
             (format ".%s" key)
           (format "[%S]" key))))
      (t "")))
   path
   ""))

(defun json-ts--path-to-python (path)
  "Convert PATH list to a Python-style path string.
PATH is a list of keys (strings) and indices (numbers)."
  (mapconcat
   (lambda (x)
     (cond
      ((numberp x) (format "[%d]" x))
      ((stringp x) (format "[\"%s\"]" x))
      (t "")))
   path
   ""))

(defun json-ts-jq-path-at-point ()
  "Show the JSON path at point in jq format."
  (interactive)
  (if-let* ((node (treesit-node-at (point))))
      (let ((path (json-ts--path-to-jq (json-ts--get-path-at-node node))))
        (kill-new path)
        (message "%s" path))
    (user-error "No JSON node at point")))

;;;###autoload
(define-derived-mode json-ts-mode prog-mode "JSON"
  "Major mode for editing JSON, powered by tree-sitter."
  :group 'json
  :syntax-table json-ts-mode--syntax-table

  (unless (treesit-ensure-installed 'json)
    (error "Tree-sitter for JSON isn't available"))

  (setq treesit-primary-parser (treesit-parser-create 'json))

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Indent.
  (setq-local treesit-simple-indent-rules json-ts--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx (or "pair" "object")))
  (setq-local treesit-defun-name-function #'json-ts-mode--defun-name)

  (setq-local treesit-thing-settings
              `((json
                 (list ,(rx (or "object" "array")))
                 (sentence "pair"))))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings json-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment constant number pair string)
                (escape-sequence)
                (bracket delimiter error)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '((nil "\\`pair\\'" nil nil)))

  (treesit-major-mode-setup)

  ;; Disable outlines since they are created for 'pair' from
  ;; 'treesit-simple-imenu-settings' almost on every line:
  (kill-local-variable 'outline-search-function)
  (kill-local-variable 'outline-level))

(derived-mode-add-parents 'json-ts-mode '(json-mode))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(js-json-mode . json-ts-mode)))

(provide 'json-ts-mode)

;;; json-ts-mode.el ends here
