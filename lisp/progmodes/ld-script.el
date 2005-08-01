;;; ld-script.el --- GNU linker script editing mode for Emacs

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Masatake YAMATO<jet@gyve.org>
;; Keywords: languages, faces

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Major mode for editing GNU linker (ld) scripts.

;;; Code:

;; Custom
(defgroup ld-script nil
  "GNU linker script code editing commands for Emacs."
  :prefix "ld-script-"
  :group 'languages)

(defvar ld-script-location-counter-face 'ld-script-location-counter)
(defface ld-script-location-counter
  '((t (:weight bold :inherit font-lock-builtin-face)))
  "Face for location counter in GNU ld script."
  :group 'ld-script)
;; backward-compatibility alias
(put 'ld-script-location-counter-face 'face-alias 'ld-script-location-counter)

;; Syntax rules
(defvar ld-script-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\  "-"   st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?_ "w"   st)
    (modify-syntax-entry ?. "_"   st)
    (modify-syntax-entry ?\\  "\\" st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?? "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?~ "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table used while in `ld-script-mode'.")

;; Font lock keywords
(defvar ld-script-keywords
  '("ENTRY" "INCLUDE" "INPUT" "GROUP"
    "OUTPUT" "SEARCH_DIR" "STARTUP"
    "OUTPUT_FORMAT" "TARGET"
    "ASSERT" "EXTERN" "FORCE_COMMON_ALLOCATION" "NOCROSSREFS" "OUTPUT_ARCH"
    "PROVIDE"
    "SECTIONS" "SORT" "COMMON" "KEEP"
    "BYTE" "SHORT" "LONG" "QUAD" "SQAD"
    "FILL"
    "CREATE_OBJECT_SYMBOLS"
    "CONSTRUCTORS"
    "NOLOAD" "DSECT" "COPY" "INFO" "OVERLAY"
    "AT"
    "MEMORY"
    "PHDRS" "FILEHDR" "FLAGS"
    "PT_NULL" "PT_LOAD" "PT_DYNAMIC" "PT_INTERP" "PT_NONE" "PT_SHLIB" "PT_PHDR"
    "VERSION")
  "Keywords used of GNU ld script.")

(defvar ld-script-builtins
  '("ABSOLUTE"
    "ADDR"
    "ALIGN"
    "BLOCK"
    "DEFINED"
    "LOADADDR"
    "MAX"
    "MIN"
    "NEXT"
    "SIZEOF"
    "SIZEOF_HEADERS"
    "sizeof_headers")
  "Builtin functions of GNU ld script.")

(defvar ld-script-font-lock-keywords
  `((,(regexp-opt ld-script-keywords 'words)
     1 font-lock-keyword-face)
    (,(regexp-opt ld-script-builtins 'words)
     1 font-lock-builtin-face)
    ("/DISCARD/" . font-lock-warning-face)
    ("##\\|#[^#\n]+$"  . font-lock-preprocessor-face)
    ("\\W\\(\\.\\)\\W" 1 ld-script-location-counter-face)
    )
  "Default font-lock-keywords for `ld-script-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lds" . ld-script-mode))

;;;###autoload
(define-derived-mode ld-script-mode nil "LD-Script"
   "A major mode to edit GNU ld script files"
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end)   " */")
  (set (make-local-variable 'indent-line-function) #'indent-relative)
  (set (make-local-variable 'font-lock-defaults) '(ld-script-font-lock-keywords nil)))

(provide 'ld-script)

;;; arch-tag: 83280b6b-e6fc-4d00-a630-922d7aec5593
;;; ld-script.el ends here
