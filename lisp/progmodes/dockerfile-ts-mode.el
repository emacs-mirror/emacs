;;; dockerfile-ts-mode.el --- tree-sitter support for Dockerfiles  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : dockerfile languages tree-sitter

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
;; dockerfile-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-dockerfile: v0.2.0-1-g087daa2
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
 '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"
              :commit "087daa20438a6cc01fa5e6fe6906d77c869d19fe")
 t)

(defvar dockerfile-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `dockerfile-ts-mode'.")

(defvar dockerfile-ts-mode--indent-rules
  `((dockerfile
     ((parent-is "copy_instruction") (nth-sibling 1) 0)
     ((parent-is "env_instruction") (nth-sibling 1) 0)
     ((parent-is "expose_instruction") (nth-sibling 1) 0)
     ((parent-is "label_instruction") (nth-sibling 1) 0)
     ((parent-is "shell_command") first-sibling 0)
     ((parent-is "string_array") first-sibling 1)
     ((dockerfile-ts-mode--line-continuation-p) dockerfile-ts-mode--line-continuation-anchor 0)))
  "Tree-sitter indent rules.")

(defun dockerfile-ts-mode--line-continuation-p ()
  "Return t if the current node is a line continuation node."
  (lambda (node _ _ &rest _)
    (string= (treesit-node-type node) "\n")))

(defun dockerfile-ts-mode--line-continuation-anchor (_ _ &rest _)
  "This anchor is used to align any nodes that are part of a line
continuation to the previous entry."
  (save-excursion
    (forward-line -1)
    (let ((prev-node (treesit-node-at (point))))
      (if (string= (treesit-node-type prev-node) "\\\n")
          (back-to-indentation)
        (forward-word)
        (forward-char))
      (+ 1 (- (point) (pos-bol))))))

(defvar dockerfile-ts-mode--keywords
  '("ADD" "ARG" "AS" "CMD" "COPY" "CROSS_BUILD" "ENTRYPOINT" "ENV"
    "EXPOSE" "FROM" "HEALTHCHECK" "LABEL" "MAINTAINER" "ONBUILD" "RUN"
    "SHELL" "STOPSIGNAL" "USER" "VOLUME" "WORKDIR")
  "Dockerfile keywords for tree-sitter font-locking.")

(defvar dockerfile-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'dockerfile
   :feature 'bracket
   '((["[" "]"]) @font-lock-bracket-face)

   :language 'dockerfile
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'dockerfile
   :feature 'delimiter
   '(([","]) @font-lock-delimiter-face)

   :language 'dockerfile
   :feature 'image-spec
   '((image_name)  @font-lock-function-name-face
     (image_tag)   @font-lock-function-name-face
     (image_alias) @font-lock-function-name-face)

   :language 'dockerfile
   :feature 'keyword
   `([,@dockerfile-ts-mode--keywords] @font-lock-keyword-face)

   :language 'dockerfile
   :feature 'number
   '((expose_port) @font-lock-number-face)

   :language 'dockerfile
   :feature 'operator
   '((["="]) @font-lock-operator-face)

   :language 'dockerfile
   :feature 'string
   '((single_quoted_string) @font-lock-string-face
     (double_quoted_string) @font-lock-string-face
     (json_string) @font-lock-string-face
     (path) @font-lock-string-face
     (arg_instruction
      default: (unquoted_string) @font-lock-string-face)
     (env_pair
      value: (unquoted_string) @font-lock-string-face))

   :language 'dockerfile
   :feature 'string-expansion
   :override t
   '((expansion
      (["$" "{" "}"] @font-lock-variable-name-face))
     (expansion
      (variable) @font-lock-variable-name-face))

   :language 'dockerfile
   :feature 'identifiers
   '((arg_instruction
      name: (unquoted_string) @font-lock-variable-name-face)
     (env_pair
      name: (unquoted_string) @font-lock-variable-name-face))

   :language 'dockerfile
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings.")

(defun dockerfile-ts-mode--stage-name (node)
  "Return the stage name of NODE.
Return nil if there is no name or if NODE is not a stage node."
  (pcase (treesit-node-type node)
    ("from_instruction"
     (treesit-node-text
      (or (treesit-node-child-by-field-name node "as")
          (treesit-node-child node 1))
      t))))

;;;###autoload
(define-derived-mode dockerfile-ts-mode prog-mode "Dockerfile"
  "Major mode for editing Dockerfiles, powered by tree-sitter."
  :group 'dockerfile
  :syntax-table dockerfile-ts-mode--syntax-table

  (when (treesit-ensure-installed 'dockerfile)
    (setq treesit-primary-parser (treesit-parser-create 'dockerfile))

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "#" (* (syntax whitespace))))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Stage" "\\`from_instruction\\'" nil dockerfile-ts-mode--stage-name)))
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local treesit-simple-indent-rules
                dockerfile-ts-mode--indent-rules)

    ;; Navigation
    (setq-local treesit-thing-settings
                `((dockerfile
                   (sentence "instruction"))))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                dockerfile-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string string-expansion identifiers)
                  (image-spec number)
                  (bracket delimiter error operator)))

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'dockerfile-ts-mode '(dockerfile-mode))

;;;###autoload
(defun dockerfile-ts-mode-maybe ()
  "Enable `dockerfile-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'dockerfile)
          (eq treesit-enabled-modes t)
          (memq 'dockerfile-ts-mode treesit-enabled-modes))
      (dockerfile-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist
               ;; NOTE: We can't use `rx' here, as it breaks bootstrap.
               ;; (rx (or (and (or "Dockerfile" "Containerfile")
               ;;              (? "." (* nonl)))
               ;;         (and "." (and (any "Dd") "ocker") "file"))
               ;;     eos)
               '("\\(?:\\(?:\\(?:Contain\\|Dock\\)erfile\\)\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
                 . dockerfile-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(dockerfile-mode . dockerfile-ts-mode)))

(provide 'dockerfile-ts-mode)

;;; dockerfile-ts-mode.el ends here
