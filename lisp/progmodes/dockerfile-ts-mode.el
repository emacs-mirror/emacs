;;; dockerfile-ts-mode.el --- tree-sitter support for Dockerfiles  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

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
   '((image_spec) @font-lock-constant-face)

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
   '((double_quoted_string) @font-lock-string-face)

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

  (when (treesit-ready-p 'dockerfile)
    (treesit-parser-create 'dockerfile)

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

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
                dockerfile-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  (image-spec number)
                  (bracket delimiter error operator)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'dockerfile)
    (add-to-list 'auto-mode-alist
                 ;; NOTE: We can't use `rx' here, as it breaks bootstrap.
                 '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
                   . dockerfile-ts-mode)))

(provide 'dockerfile-ts-mode)

;;; dockerfile-ts-mode.el ends here
