;;; treesit-x.el --- tree-sitter extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

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

;;; Define treesit generic mode

;;;###autoload
(defvar treesit-generic-mode-list nil
  "A list of mode names for `treesit-generic-mode'.
Do not add entries to this list directly; use `define-treesit-generic-mode'
instead (which see).")

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
      (setq source (list 'quote (ensure-list source))))
    (when (stringp auto-mode)
      (setq auto-mode (list 'quote (ensure-list auto-mode))))

    `(progn
       ;; Add lang and source to source-alist.
       (add-to-list 'treesit-language-source-alist (cons ,lang ,source))

       ;; Add a new entry.
       (add-to-list 'treesit-generic-mode-list ,mode-name)

       ;; Add it to auto-mode-alist
       (dolist (re ,auto-mode)
         (add-to-list 'auto-mode-alist (cons re ',mode)))

       (define-derived-mode ,mode
         ,(or parent 'fundamental-mode)
         ,(or name pretty-name)
         ,(or docstring
              (concat (or name pretty-name) " mode.\n"
                      "This a tree-sitter mode defined with `define-treesit-generic-mode'.\n"
                      "It runs `" mode-name "-hook' as the last thing it does."))
         (treesit-generic-mode-setup ,lang ,source)
         ,@body
         (treesit-major-mode-setup)))))

;;;###autoload
(defun treesit-generic-mode-setup (lang source)
  "Go into the treesit generic mode MODE."
  (unless (treesit-ready-p lang t)
    (when (y-or-n-p (format "Install grammar for %s?" lang))
      (apply
       #'treesit--install-language-grammar-1
       (locate-user-emacs-file "tree-sitter")
       lang source)))

  (when (treesit-ready-p lang)
    (setq treesit-primary-parser (treesit-parser-create lang))

    (when-let* ((query (treesit-generic-mode-font-lock-query lang)))
      (setq-local treesit-font-lock-settings
                  (treesit-font-lock-rules
                   :language lang
                   :feature 'highlights
                   query))
      (setq-local treesit-font-lock-feature-list '((highlights))))))

;;;###autoload
(defun treesit-generic-mode (mode)
  "Enter treesit generic mode MODE.

Treesit generic modes provide basic font-lock functionality for
tree-sitter grammars.  (Files which are too small to warrant their
own mode, but have comments, keywords, and the like.)

To define a generic mode, use the function `define-treesit-generic-mode'.
Some treesit generic modes are defined in `treesit-x.el'."
  (interactive
   (list (completing-read "Treesit generic mode: " treesit-generic-mode-list nil t)))
  (funcall (intern mode)))

;;; Generic font-lock handling

(defvar treesit-generic-mode-font-lock-map
  '(
    ("@boolean"               . "@font-lock-constant-face")
    ("@comment"               . "@font-lock-comment-face")
    ("@constant"              . "@font-lock-constant-face")
    ("@error"                 . "@font-lock-warning-face")
    ("@escape"                . "@font-lock-escape-face")
    ("@keyword"               . "@font-lock-keyword-face")
    ("@operator"              . "@font-lock-operator-face")
    ("@property"              . "@font-lock-property-use-face")
    ("@punctuation.bracket"   . "@font-lock-bracket-face")
    ("@punctuation.delimiter" . "@font-lock-delimiter-face")
    ("@punctuation.special"   . "@font-lock-misc-punctuation-face")
    ("@string.regexp"         . "@font-lock-regexp-face")
    ("@string.special"        . "@font-lock-string-face")
    ("@string"                . "@font-lock-string-face")
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
  :source "https://github.com/tree-sitter-grammars/tree-sitter-gitattributes"
  :auto-mode "gitattributes\\'"
  :name "Git-Attributes"
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

(provide 'treesit-x)

;;; treesit-x.el ends here
