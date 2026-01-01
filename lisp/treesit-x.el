;;; treesit-x.el --- tree-sitter extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: treesit, tree-sitter, languages, generic, font-lock
;; Package: emacs

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

;; This file contains a collection of treesit generic modes.
;;
;; INSTALLATION:
;;
;; Add this line to your init file:
;;
;;   (require 'treesit-x)
;;
;; Then visiting a file that has a tree-sitter grammar will ask you
;; for confirmation before installing that grammar.  Afterwards it
;; will highlight text according to syntax defined by the grammar.

;;; Code:

(require 'treesit)

(treesit-declare-unavailable-functions)

;;; Define treesit generic mode

;;;###autoload
(defmacro define-treesit-generic-mode (mode &optional docstring &rest body)
  "Create a new treesit generic mode MODE.

A \"treesit\" mode is a simple major mode with basic support for
Font Lock mode, but otherwise does not have any special keystrokes
or functionality available.

MODE is the name of the command for the treesit generic mode; don't
quote it.  The optional DOCSTRING is the documentation for the mode
command.  If you do not supply it, `define-treesit-generic-mode'
uses a default documentation string instead.

KEYWORD-ARGS are optional arguments in the form of pairs of keyword
and value. The following keyword arguments are currently supported:

  :lang is a language symbol of the corresponding tree-sitter grammar.

  :source is either a string for the URL or a list in the same format
  as for elements in `treesit-language-source-alist', i.e.
  (URL REVISION SOURCE-DIR CC C++ COMMIT).

  :auto-mode is a regular expression or a list of regular expressions
  to add to `auto-mode-alist'.  These regular expressions are added
  when Emacs runs the macro expansion.

  :parent is the name of the command for the parent mode.

  :name is a string that will appear in the mode line.

BODY are forms to execute just before running the
hooks for the new mode.  Do not use `interactive' here.
These forms do some additional setup.  The mode command calls
these functions just before it runs `treesit-major-mode-setup'
and the mode hook `MODE-hook'.

See at the bottom of the file treesit-x.el for some examples
of `define-treesit-generic-mode'.

\(fn MODE [DOCSTRING] [KEYWORD-ARGS...] &rest BODY)"
  (declare (debug (&define name [&optional stringp]
                           [&rest keywordp sexp] def-body))
           (doc-string 2)
           (indent defun))

  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))

  (let* ((mode-name (symbol-name mode))
         (pretty-name (capitalize (replace-regexp-in-string
                                   "-mode\\'" "" mode-name)))
         lang source auto-mode parent name)

    ;; Process the keyword args.
    (while (keywordp (car body))
      (pcase (pop body)
        (:lang (setq lang (pop body)))
        (:source (setq source (pop body)))
        (:auto-mode (setq auto-mode (pop body)))
        (:parent (setq parent (pop body)))
        (:name (setq name (pop body)))
        (_ (pop body))))

    (when (stringp source)
      (setq source (list 'quote (list source :copy-queries t))))
    (when (stringp auto-mode)
      (setq auto-mode (list 'quote (ensure-list auto-mode))))

    `(progn
       ;; Add lang and source to source-alist.
       (add-to-list 'treesit-language-source-alist (cons ,lang ,source) t)

       ;; Add it to auto-mode-alist
       (dolist (re ,auto-mode)
         (add-to-list 'auto-mode-alist (cons re ',mode)))

       (define-derived-mode ,mode
         ,(or (if (eq (car-safe parent) 'quote) (cadr parent) parent)
              'fundamental-mode)
         ,(or name pretty-name)
         ,(or docstring
              (concat (or name pretty-name) " mode.\n"
                      "This a tree-sitter mode defined with `define-treesit-generic-mode'."))
         (treesit-generic-mode-setup ,lang)
         ,@body
         (treesit-major-mode-setup)))))

;;;###autoload
(defun treesit-generic-mode-setup (lang)
  "Go into the treesit generic mode MODE."
  (when (treesit-ensure-installed lang)
    (setq treesit-primary-parser (treesit-parser-create lang))

    (when-let* ((query (treesit-generic-mode-font-lock-query lang)))
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          (treesit-font-lock-rules
                           :language lang
                           :feature 'highlights
                           query)))
      (setq-local treesit-font-lock-feature-list
                  (treesit-merge-font-lock-feature-list
                   treesit-font-lock-feature-list
                   '((highlights)))))))

;;; Generic font-lock handling

(defvar treesit-generic-mode-font-lock-map
  '(
    ("@attribute"             . "@font-lock-preprocessor-face")
    ("@boolean"               . "@font-lock-constant-face")
    ("@comment"               . "@font-lock-comment-face")
    ("@constructor"           . "@font-lock-type-face")
    ("@constant"              . "@font-lock-constant-face")
    ("@constant.builtin"      . "@font-lock-builtin-face")
    ("@delimiter"             . "@font-lock-delimiter-face")
    ("@error"                 . "@font-lock-warning-face")
    ("@escape"                . "@font-lock-escape-face")
    ("@function"              . "@font-lock-function-name-face")
    ("@function.builtin"      . "@font-lock-builtin-face")
    ("@function.call"         . "@font-lock-function-call-face")
    ("@keyword"               . "@font-lock-keyword-face")
    ("@keyword.operator"      . "@font-lock-operator-face")
    ("@module"                . "@font-lock-keyword-face")
    ("@number"                . "@font-lock-number-face")
    ("@operator"              . "@font-lock-operator-face")
    ("@property"              . "@font-lock-property-name-face")
    ("@punctuation.bracket"   . "@font-lock-bracket-face")
    ("@punctuation.delimiter" . "@font-lock-delimiter-face")
    ("@punctuation.special"   . "@font-lock-misc-punctuation-face")
    ("@string"                . "@font-lock-string-face")
    ("@string.regexp"         . "@font-lock-regexp-face")
    ("@string.special"        . "@font-lock-string-face")
    ("@tag"                   . "@font-lock-function-name-face")
    ("@tag.delimiter"         . "@font-lock-delimiter-face")
    ("@text.reference"        . "@font-lock-doc-face")
    ("@type"                  . "@font-lock-type-face")
    ("@type.builtin"          . "@font-lock-builtin-face")
    ("@variable"              . "@font-lock-variable-name-face")
    ("@variable.builtin"      . "@font-lock-builtin-face")
    ("@variable.parameter"    . "@font-lock-variable-name-face")
    )
  "A mapping from default capture names to font-lock faces.")

(defun treesit-generic-mode-font-lock-query (lang)
  "Find the file highlights.scm and return its queries as a string."
  (let* ((file (expand-file-name
                (format "queries/%s/highlights.scm" lang)
                (locate-user-emacs-file "tree-sitter")))
         (query (when (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-substring-no-properties (point-min) (point-max))))))
    (when query
      (setq query (replace-regexp-in-string "(#set! [^)]+)" "" query nil nil))
      (pcase-dolist (`(,from . ,to) treesit-generic-mode-font-lock-map)
        (setq query (replace-regexp-in-string from to query nil t)))
      query)))

;;; Default treesit generic modes

(define-treesit-generic-mode gitattributes-generic-ts-mode
  "Tree-sitter generic mode for .gitattributes files."
  :lang 'gitattributes
  :source '("https://github.com/tree-sitter-grammars/tree-sitter-gitattributes"
            :commit "5425944fd61bf2b3bad2c17c2dc9f53172b0f01d"
            :copy-queries t)
  :auto-mode "gitattributes\\'"
  :name "Git-Attributes"
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

(define-treesit-generic-mode liquid-generic-ts-mode
  "Tree-sitter generic mode for Liquid templates."
  :lang 'liquid
  :source '("https://github.com/hankthetank27/tree-sitter-liquid"
            :commit "d6ebde3974742cd1b61b55d1d94aab1dacb41056"
            :copy-queries t)
  :auto-mode "\\.liquid\\'"
  :name "Liquid"
  :parent 'mhtml-ts-mode

  (setq-local treesit-range-settings
              (append treesit-range-settings
                      (treesit-range-rules
                       :embed 'html
                       :host 'liquid
                       '(((template_content) @cap)))))

  (setq-local treesit-thing-settings
              (append treesit-thing-settings
                      `((liquid (sexp (not ,(rx bos "program" eos)))
                                (list ,(rx bos (or "range"
                                                   "if_statement"
                                                   "for_loop_statement"
                                                   "case_statement"
                                                   "unless_statement"
                                                   "capture_statement"
                                                   "form_statement"
                                                   "tablerow_statement"
                                                   "paginate_statement")
                                           eos))))))

  (setq-local treesit-aggregated-outline-predicate
              (append treesit-aggregated-outline-predicate
                      `((liquid . ,(rx bos (or "if_statement"
                                               "for_loop_statement")
                                       eos)))))

  (when (treesit-ready-p 'yaml t)
    (require 'yaml-ts-mode)
    (defvar yaml-ts-mode--font-lock-settings)
    (defvar yaml-ts-mode--font-lock-feature-list)
    (setq-local treesit-range-settings
                (append treesit-range-settings
                        (treesit-range-rules
                         :embed 'yaml
                         :host 'liquid
                         :local t
                         '(((front_matter) @cap)))))
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        yaml-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                (treesit-merge-font-lock-feature-list
                 treesit-font-lock-feature-list
                 yaml-ts-mode--font-lock-feature-list))))

(defvar alpinejs-generic-ts-attr-regexp
  (rx bos (or "x-" ":" "@"))
  "Regexp for HTML attributes handled by Alpine.js")

(defun alpinejs-generic-ts-attr-not-match (node)
  (not (string-match-p alpinejs-generic-ts-attr-regexp
                       (treesit-node-text node t))))

(defun alpinejs-generic-ts-setup ()
  "Tree-sitter generic setup for Alpine.js framework.
It uses JavaScript highlighting inside a limited set of HTML attributes.
Intended to be used in combination with such major modes as
`mhtml-ts-mode' and `liquid-generic-ts-mode'.  For example:

\(add-hook \\='mhtml-ts-mode-hook \\='alpinejs-generic-ts-setup)
\(add-hook \\='liquid-generic-ts-mode-hook \\='alpinejs-generic-ts-setup)"

  ;; Use JavaScript highlighting for Alpinejs HTML attributes
  (setq-local treesit-range-settings
              (append treesit-range-settings
                      (treesit-range-rules
                       :embed 'javascript
                       :host 'html
                       :local t
                       `((attribute
                          (attribute_name) @_name
                          (:match ,alpinejs-generic-ts-attr-regexp @_name)
                          (quoted_attribute_value
                           (attribute_value) @cap))))))

  ;; Highlight only non-Alpinejs HTML attributes
  (setq-local treesit-font-lock-settings
              (treesit-replace-font-lock-feature-settings
               (treesit-font-lock-rules
                :language 'html
                :override t
                :feature 'string
                `((attribute
                   (attribute_name) @_name
                   (:pred alpinejs-generic-ts-attr-not-match @_name)
                   (quoted_attribute_value) @font-lock-string-face)))
               treesit-font-lock-settings))

  ;; Highlight only quotes for Alpinejs HTML attributes
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language 'html
                       :override t
                       :feature 'string
                       `((attribute
                          (attribute_name) @_name
                          (:match ,alpinejs-generic-ts-attr-regexp @_name)
                          (quoted_attribute_value "\"" @font-lock-string-face))))))

  (treesit-major-mode-setup)

  (run-mode-hooks 'alpinejs-generic-ts-setup-hook))

(provide 'treesit-x)

;;; treesit-x.el ends here
