;;; editorconfig-conf-mode.el --- Major mode for editing .editorconfig files  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

;; This file is part of EditorConfig Emacs Plugin.

;; EditorConfig Emacs Plugin is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; EditorConfig Emacs Plugin is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; EditorConfig Emacs Plugin. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing .editorconfig files.

;;; Code:

(require 'conf-mode)

(defvar editorconfig-conf-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    table)
  "Syntax table in use in `editorconfig-conf-mode' buffers.")

(defvar editorconfig-conf-mode-abbrev-table nil
  "Abbrev table in use in `editorconfig-conf-mode' buffers.")
(define-abbrev-table 'editorconfig-conf-mode-abbrev-table ())

;;;###autoload
(define-derived-mode editorconfig-conf-mode conf-unix-mode "Conf[EditorConfig]"
  "Major mode for editing .editorconfig files."
  (set-variable 'indent-line-function 'indent-relative)
  (let ((key-property-list
         '("charset"
           "end_of_line"
           "file_type_emacs"
           "file_type_ext"
           "indent_size"
           "indent_style"
           "insert_final_newline"
           "max_line_length"
           "root"
           "tab_width"
           "trim_trailing_whitespace"))
        (key-value-list
         '("unset"
           "true"
           "false"
           "lf"
           "cr"
           "crlf"
           "space"
           "tab"
           "latin1"
           "utf-8"
           "utf-8-bom"
           "utf-16be"
           "utf-16le"))
        (font-lock-value
         '(("^[ \t]*\\[\\(.+?\\)\\]" 1 font-lock-type-face)
           ("^[ \t]*\\(.+?\\)[ \t]*[=:]" 1 font-lock-variable-name-face))))

    ;; Highlight all key values
    (dolist (key-value key-value-list)
      (push `(,(format "[=:][ \t]*\\(%s\\)\\([ \t]\\|$\\)" key-value)
              1 font-lock-constant-face)
            font-lock-value))
    ;; Highlight all key properties
    (dolist (key-property key-property-list)
      (push `(,(format "^[ \t]*\\(%s\\)[ \t]*[=:]" key-property)
              1 font-lock-builtin-face)
            font-lock-value))

    (conf-mode-initialize "#" font-lock-value)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.editorconfig\\'" . editorconfig-conf-mode))

(provide 'editorconfig-conf-mode)
;;; editorconfig-conf-mode.el ends here
