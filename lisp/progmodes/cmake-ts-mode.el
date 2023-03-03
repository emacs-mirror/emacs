;;; cmake-ts-mode.el --- tree-sitter support for CMake  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : cmake languages tree-sitter

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
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom cmake-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `cmake-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'cmake)

(defvar cmake-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?$  "'" table)
    table)
  "Syntax table for `cmake-ts-mode'.")

(defvar cmake-ts-mode--indent-rules
  `((cmake
     ((node-is ")") parent-bol 0)
     ((node-is "else_command") parent-bol 0)
     ((node-is "elseif_command") parent-bol 0)
     ((node-is "endforeach_command") parent-bol 0)
     ((node-is "endfunction_command") parent-bol 0)
     ((node-is "endif_command") parent-bol 0)
     ((parent-is "foreach_loop") parent-bol cmake-ts-mode-indent-offset)
     ((parent-is "function_def") parent-bol cmake-ts-mode-indent-offset)
     ((parent-is "if_condition") parent-bol cmake-ts-mode-indent-offset)
     ((parent-is "normal_command") parent-bol cmake-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `cmake-ts-mode'.")

(defvar cmake-ts-mode--constants
  '("1" "ON" "TRUE" "YES" "Y" "0" "OFF" "FALSE" "NO" "N" "IGNORE"
    "NOTFOUND")
  "CMake constants for tree-sitter font-locking.")

(defvar cmake-ts-mode--keywords
  '((else) (elseif) (endforeach) (endfunction) (endif) (endmacro)
    (endwhile) (foreach) (function) (if) (macro) (while))
  "CMake keywords for tree-sitter font-locking.")

(defvar cmake-ts-mode--foreach-options
  '("IN" "ITEMS" "LISTS" "RANGE" "ZIP_LISTS")
  "CMake foreach options for tree-sitter font-locking.")

(defvar cmake-ts-mode--if-conditions
  '("AND" "COMMAND" "DEFINED" "EQUAL" "EXISTS" "GREATER"
    "GREATER_EQUAL" "LESS" "LESS_EQUAL" "MATCHES" "NOT" "OR"
    "PATH_EQUAL" "STREQUAL" "STRGREATER" "STRGREATER_EQUAL" "STRLESS"
    "STRLESS_EQUAL" "VERSION_EQUAL" "VERSION_GREATER"
    "VERSION_GREATER_EQUAL" "VERSION_LESS" "VERSION_LESS_EQUAL")
  "CMake if conditions for tree-sitter font-locking.")

(defvar cmake-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'cmake
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)

   :language 'cmake
   :feature 'builtin
   `(((foreach_command
       ((argument) @font-lock-constant-face
        (:match ,(rx-to-string
                  `(seq bol
                        (or ,@cmake-ts-mode--foreach-options)
                        eol))
                @font-lock-constant-face))))
     ((if_command
       ((argument) @font-lock-constant-face
        (:match ,(rx-to-string
                  `(seq bol
                        (or ,@cmake-ts-mode--if-conditions)
                        eol))
                @font-lock-constant-face)))))

   :language 'cmake
   :feature 'comment
   '([(bracket_comment) (line_comment)] @font-lock-comment-face)

   :language 'cmake
   :feature 'constant
   `(((argument) @font-lock-constant-face
      (:match ,(rx-to-string
                `(seq bol
                      (or ,@cmake-ts-mode--constants)
                      eol))
              @font-lock-constant-face)))

   :language 'cmake
   :feature 'function
   '((normal_command (identifier) @font-lock-function-call-face))

   :language 'cmake
   :feature 'keyword
   `([,@cmake-ts-mode--keywords] @font-lock-keyword-face)

   :language 'cmake
   :feature 'number
   '(((unquoted_argument) @font-lock-number-face
      (:match "^[[:digit:]]*\\.?[[:digit:]]*\\.?[[:digit:]]+$" @font-lock-number-face)))

   :language 'cmake
   :feature 'string
   '([(bracket_argument) (quoted_argument)] @font-lock-string-face)

   :language 'cmake
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'cmake
   :feature 'misc-punctuation
   ;; Don't override strings.
   :override 'nil
   '((["$" "{" "}" "<" ">"]) @font-lock-misc-punctuation-face)

   :language 'cmake
   :feature 'variable
   :override t
   '((variable) @font-lock-variable-use-face)

   :language 'cmake
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `cmake-ts-mode'.")

(defun cmake-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (func-tree (treesit-induce-sparse-tree
                     node "function_def" nil 1000))
         (func-index (cmake-ts-mode--imenu-1 func-tree)))
    (append
     (when func-index `(("Function" . ,func-index))))))

(defun cmake-ts-mode--imenu-1 (node)
  "Helper for `cmake-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'cmake-ts-mode--imenu-1
                           children))
         (name (when ts-node
                 (pcase (treesit-node-type ts-node)
                   ("function_def"
                    (treesit-node-text
                     (treesit-node-child (treesit-node-child ts-node 0) 2) t)))))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((or (null ts-node) (null name)) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

;;;###autoload
(define-derived-mode cmake-ts-mode prog-mode "CMake"
  "Major mode for editing CMake files, powered by tree-sitter."
  :group 'cmake
  :syntax-table cmake-ts-mode--syntax-table

  (when (treesit-ready-p 'cmake)
    (treesit-parser-create 'cmake)

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "#" (* (syntax whitespace))))

    ;; Imenu.
    (setq-local imenu-create-index-function #'cmake-ts-mode--imenu)
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local treesit-simple-indent-rules cmake-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings cmake-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  ;; 'function' and 'variable' here play slightly
                  ;; different roles than in other ts modes, so we
                  ;; kept them at level 3.
                  (builtin constant escape-sequence function number variable)
                  (bracket error misc-punctuation)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'cmake)
    (add-to-list 'auto-mode-alist
                 '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)))

(provide 'cmake-ts-mode)

;;; cmake-ts-mode.el ends here
