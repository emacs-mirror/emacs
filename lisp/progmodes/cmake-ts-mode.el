;;; cmake-ts-mode.el --- tree-sitter support for CMake  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Tree-sitter language versions
;;
;; cmake-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-cmake: v0.5.0-5-ge409ae3
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(cmake "https://github.com/uyha/tree-sitter-cmake"
         :commit "e409ae33f00e04cde30f2bcffb979caf1a33562a")
 t)

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

(defun cmake-ts-mode--indent-rules ()
  "Return tree-sitter indent rules for `cmake-ts-mode'."
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
     ((parent-is "normal_command") parent-bol cmake-ts-mode-indent-offset)
     ;; Release v0.4.0 wraps arguments in an argument_list node.
     ,@(ignore-errors
         (treesit-query-capture 'cmake '((argument_list) @capture))
         `(((parent-is "argument_list") grand-parent cmake-ts-mode-indent-offset)))
     ;; Release v0.3.0 wraps the body of commands into a body node.
     ,@(ignore-errors
         (treesit-query-capture 'cmake '((body) @capture))
         `(((parent-is "body") grand-parent cmake-ts-mode-indent-offset))))))

(defvar cmake-ts-mode--constants
  '("ON" "TRUE" "YES" "Y" "OFF" "FALSE" "NO" "N" "IGNORE" "NOTFOUND")
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

(defun cmake-ts-mode--font-lock-compatibility-fe9b5e0 ()
  "Font lock helper, to handle different releases of tree-sitter-cmake.
Check if a node type is available, then return the right font lock rules."
  ;; handle commit fe9b5e0
  (condition-case nil
      (progn (treesit-query-capture 'cmake '((argument_list) @capture))
             `(((foreach_command
                 ((argument_list) @font-lock-constant-face
                  (:match ,(rx-to-string
                            `(seq bol
                                  (or ,@cmake-ts-mode--foreach-options)
                                  eol))
                          @font-lock-constant-face))))
               ((if_command
                 ((argument_list) @font-lock-constant-face
                  (:match ,(rx-to-string
                            `(seq bol
                                  (or ,@cmake-ts-mode--if-conditions)
                                  eol))
                          @font-lock-constant-face))))))
    (error
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
                  @font-lock-constant-face))))))))

(defun cmake-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for `cmake-ts-mode'."
  (treesit-font-lock-rules
   :language 'cmake
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)

   :language 'cmake
   :feature 'builtin
   (cmake-ts-mode--font-lock-compatibility-fe9b5e0)

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
      (:match "\\`-?[[:digit:]]*\\.?[[:digit:]]*\\.?[[:digit:]]+\\'"
              @font-lock-number-face)))

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
   '((["$" "{" "}"]) @font-lock-misc-punctuation-face)

   :language 'cmake
   :feature 'variable
   :override t
   '((variable) @font-lock-variable-use-face)

   :language 'cmake
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)))

(defun cmake-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "function_def" "macro_def")
     (treesit-node-text
      (treesit-search-subtree node "^argument$" nil nil 3)
      t))))

;;;###autoload
(define-derived-mode cmake-ts-mode prog-mode "CMake"
  "Major mode for editing CMake files, powered by tree-sitter."
  :group 'cmake
  :syntax-table cmake-ts-mode--syntax-table

  (when (treesit-ensure-installed 'cmake)
    (setq treesit-primary-parser (treesit-parser-create 'cmake))

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "#" (* (syntax whitespace))))

    ;; Defuns.
    (setq-local treesit-defun-type-regexp (rx (or "function" "macro")
                                              "_def"))
    (setq-local treesit-defun-name-function #'cmake-ts-mode--defun-name)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Function" "^function_def$")
                  ("Macro" "^macro_def$")))
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local treesit-simple-indent-rules (cmake-ts-mode--indent-rules))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings (cmake-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  ;; 'function' and 'variable' here play slightly
                  ;; different roles than in other ts modes, so we
                  ;; kept them at level 3.
                  (builtin constant escape-sequence function number variable)
                  (bracket error misc-punctuation)))

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'cmake-ts-mode '(cmake-mode))

;;;###autoload
(defun cmake-ts-mode-maybe ()
  "Enable `cmake-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'cmake)
          (eq treesit-enabled-modes t)
          (memq 'cmake-ts-mode treesit-enabled-modes))
      (cmake-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist
               '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(cmake-mode . cmake-ts-mode)))

(provide 'cmake-ts-mode)

;;; cmake-ts-mode.el ends here
