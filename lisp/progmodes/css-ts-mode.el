;;; css-ts-mode.el --- tree-sitter support for CSS  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : November 2022
;; Keywords   : css languages tree-sitter

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'rx)
(require 'css-mode)

(defcustom css-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'css)

(defvar css-ts-mode--indent-rules
  `((css
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)

     ((parent-is "block") parent-bol css-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol css-ts-mode-indent-offset)
     ((parent-is "declaration") parent-bol css-ts-mode-indent-offset))))

(defvar css-ts-mode--settings
  (treesit-font-lock-rules
   :language 'css
   :feature 'basic
   :override t
   `((unit) @font-lock-constant-face
     (integer_value) @font-lock-builtin-face
     (float_value) @font-lock-builtin-face
     (plain_value) @font-lock-variable-name-face
     (comment) @font-lock-comment-face
     (class_selector) @css-selector
     (child_selector) @css-selector
     (id_selector) @css-selector
     (tag_name) @css-selector
     (property_name) @css-property
     (class_name) @css-selector
     (function_name) @font-lock-function-name-face)))

(defun css-ts-mode--imenu-1 (node)
  "Helper for `css-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (subtrees (mapcan #'css-ts-mode--imenu-1 (cdr node)))
         (name (when ts-node
                 (if (equal (treesit-node-type ts-node) "tag_name")
                     (treesit-node-text ts-node)
                   (treesit-node-text (treesit-node-child ts-node 1) t))))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

(defun css-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (tree (treesit-induce-sparse-tree
                node (rx (or "class_selector"
                             "id_selector"
                             "tag_name")))))
    (css-ts-mode--imenu-1 tree)))

(define-derived-mode css-ts-mode prog-mode "CSS"
  "Major mode for editing CSS."
  :group 'css
  :syntax-table css-mode-syntax-table

  (unless (treesit-ready-p nil 'css)
    (error "Tree-sitter for CSS isn't available"))

  (treesit-parser-create 'css)

  ;; Comments
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  ;; Indent.
  (setq-local treesit-simple-indent-rules css-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "rule_set")
  ;; Font-lock.
  (setq-local treesit-font-lock-settings css-ts-mode--settings)
  (setq treesit-font-lock-feature-list '((basic) () ()))

  ;; Imenu.
  (setq-local imenu-create-index-function #'css-ts-mode--imenu)
  (setq-local which-func-functions nil) ;; Piggyback on imenu

  (treesit-major-mode-setup))

(provide 'css-ts-mode)

;;; css-ts-mode.el ends here
