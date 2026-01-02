;;; editorconfig.el --- EditorConfig Plugin  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2026 Free Software Foundation, Inc.

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.11.0
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience editorconfig

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors or
;; https://github.com/editorconfig/editorconfig-emacs/blob/master/CONTRIBUTORS
;; for the list of contributors.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; EditorConfig helps developers define and maintain consistent
;; coding styles between different editors and IDEs.

;; The EditorConfig project consists of a file format for defining
;; coding styles and a collection of text editor plugins that enable
;; editors to read the file format and adhere to defined styles.
;; EditorConfig files are easily readable and they work nicely with
;; version control systems.

;;; News:

;; - In `editorconfig-indentation-alist', if a mode is associated to a function
;;   that function should not set the vars but should instead *return* them.
;; - New var `editorconfig-indent-size-vars' for major modes to set.
;; - New hook `editorconfig-get-local-variables-functions' to support
;;   additional settings.

;;; Code:

(require 'cl-lib)

(eval-when-compile (require 'subr-x))

(require 'editorconfig-core)

(defgroup editorconfig nil
  "EditorConfig Emacs Plugin.

EditorConfig helps developers define and maintain consistent
coding styles between different editors and IDEs."
  :tag "EditorConfig"
  :prefix "editorconfig-"
  :group 'tools)

(when (< emacs-major-version 30)
  (define-obsolete-variable-alias
    'edconf-custom-hooks
    'editorconfig-after-apply-functions
    "0.5")
  (define-obsolete-variable-alias
    'editorconfig-custom-hooks
    'editorconfig-after-apply-functions
    "0.7.14")
  (defcustom editorconfig-after-apply-functions ()
    "A list of functions after loading common EditorConfig settings.

Each element in this list is a hook function.  This hook function
takes one parameter, which is a property hash table.  The value
of properties can be obtained through gethash function.

The hook does not have to be coding style related; you can add
whatever functionality you want.  For example, the following is
an example to add a new property emacs_linum to decide whether to
show line numbers on the left:

  (add-hook \\='editorconfig-after-apply-functions
    \\='(lambda (props)
       (let ((show-line-num (gethash \\='emacs_linum props)))
         (cond ((equal show-line-num \"true\") (linum-mode 1))
           ((equal show-line-num \"false\") (linum-mode 0))))))

This hook will be run even when there are no matching sections in
\".editorconfig\", or no \".editorconfig\" file was found at all."
    :type 'hook))

(when (< emacs-major-version 30)
  (defcustom editorconfig-hack-properties-functions ()
    "A list of function to alter property values before applying them.

These functions will be run after loading \".editorconfig\" files and before
applying them to current buffer, so that you can alter some properties from
\".editorconfig\" before they take effect.
\(Since 2021/08/30 (v0.9.0): Buffer coding-systems are set before running
this functions, so this variable cannot be used to change coding-systems.)

For example, Makefiles always use tab characters for indentation: you can
overwrite \"indent_style\" property when current `major-mode' is a
`makefile-mode' with following code:

  (add-hook \\='editorconfig-hack-properties-functions
            \\='(lambda (props)
               (when (derived-mode-p \\='makefile-mode)
                 (puthash \\='indent_style \"tab\" props))))

This hook will be run even when there are no matching sections in
\".editorconfig\", or no \".editorconfig\" file was found at all."
    :type 'hook))
(make-obsolete-variable 'editorconfig-hack-properties-functions
                        'editorconfig-get-local-variables-functions
                        "2024")

(define-obsolete-variable-alias
  'edconf-indentation-alist
  'editorconfig-indentation-alist
  "0.5")
(defcustom editorconfig-indentation-alist
  ;; For contributors: Sort modes in alphabetical order
  '((awk-mode c-basic-offset)
    (bash-ts-mode sh-basic-offset)
    (bpftrace-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c++-ts-mode c-basic-offset
                 c-ts-mode-indent-offset)
    (c-ts-mode c-basic-offset
               c-ts-mode-indent-offset)
    (cmake-mode cmake-tab-width)
    (cmake-ts-mode cmake-tab-width
                   cmake-ts-mode-indent-offset)
    (coffee-mode coffee-tab-width)
    (csharp-mode c-basic-offset)
    (csharp-ts-mode c-basic-offset
                    csharp-ts-mode-indent-offset)
    (css-ts-mode css-indent-offset)
    (d-mode c-basic-offset)
    (elixir-ts-mode elixir-ts-indent-offset)
    (emacs-lisp-mode . editorconfig--get-indentation-lisp-mode)
    (f90-mode f90-associate-indent
              f90-continuation-indent
              f90-critical-indent
              f90-do-indent
              f90-if-indent
              f90-program-indent
              f90-type-indent)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (fsharp-mode fsharp-continuation-offset
                 fsharp-indent-level
                 fsharp-indent-offset)
    (go-ts-mode go-ts-mode-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  haskell-indentation-layout-offset
                  haskell-indentation-left-offset
                  haskell-indentation-starter-offset
                  haskell-indentation-where-post-offset
                  haskell-indentation-where-pre-offset
                  shm-indent-spaces)
    (haxor-mode haxor-tab-width)
    (html-ts-mode html-ts-mode-indent-offset)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (java-ts-mode c-basic-offset
                  java-ts-mode-indent-offset)
    (js-ts-mode js-indent-level)
    (js-jsx-mode js-indent-level sgml-basic-offset)
    (js2-jsx-mode js2-basic-offset sgml-basic-offset)
    (json-mode js-indent-level)
    (json-ts-mode json-ts-mode-indent-offset)
    (jsonian-mode jsonian-default-indentation)
    (kotlin-mode kotlin-tab-width)
    (kotlin-ts-mode kotlin-ts-mode-indent-offset)
    (latex-mode . editorconfig--get-indentation-latex-mode)
    (lisp-mode . editorconfig--get-indentation-lisp-mode)
    (livescript-mode livescript-tab-width)
    (magik-ts-mode magik-indent-level)
    (meson-mode meson-indent-basic)
    (mips-mode mips-tab-width)
    (nxml-mode . editorconfig--get-indentation-nxml-mode)
    (objc-mode c-basic-offset)
    (octave-mode octave-block-offset)
    ;; No need to change `php-mode-coding-style' value for php-mode
    ;; since we run editorconfig later than it resets `c-basic-offset'.
    ;; See https://github.com/editorconfig/editorconfig-emacs/issues/116
    ;; for details.
    (php-mode c-basic-offset)
    (php-ts-mode php-ts-mode-indent-offset)
    (pike-mode c-basic-offset)
    (protobuf-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (python-mode . editorconfig--get-indentation-python-mode)
    (python-ts-mode . editorconfig--get-indentation-python-mode)
    (rjsx-mode js-indent-level sgml-basic-offset)
    (ruby-ts-mode ruby-indent-level)
    (rust-ts-mode rust-indent-offset
                  rust-ts-mode-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (swift-mode swift-mode:basic-offset)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (toml-ts-mode toml-ts-mode-indent-offset)
    (typescript-ts-base-mode typescript-ts-mode-indent-offset)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode . editorconfig--get-indentation-web-mode)
    (yaml-ts-mode yaml-indent-offset)
    )
  "Alist of indentation setting methods by modes.

This is a fallback used for those modes which don't set
`editorconfig-indent-size-vars'.

Each element should look like (MODE . SETTING) where SETTING
should obey the same rules as `editorconfig-indent-size-vars',
i.e. be either a list of variable names or a function returning
a list of settings in the form (VARIABLE . VALUE)."
  :type '(alist :key-type symbol
                :value-type (choice function
                                    (repeat
                                     (choice symbol
                                             (cons symbol integer)))))
  :version "30.1"
  :risky t)

(defcustom editorconfig-trim-whitespaces-mode #'delete-trailing-whitespace-mode
  "Buffer local minor-mode to use to trim trailing whitespaces.

If set, enable that mode when `trim_trailing_whitespace` is set to true.
Otherwise, use `delete-trailing-whitespace'."
  :version "30.1"
  :type 'function)

(defvar-local editorconfig-properties-hash nil
  "Hash object of EditorConfig properties that was enabled for current buffer.")
(put 'editorconfig-properties-hash 'permanent-local t)

(defvar editorconfig-lisp-use-default-indent nil
  "Selectively ignore the value of indent_size for Lisp files.
Prevents `lisp-indent-offset' from being set selectively.

nil - `lisp-indent-offset' is always set normally.
t   - `lisp-indent-offset' is never set normally
       (always use default indent for lisps).
number - `lisp-indent-offset' is not set only if indent_size is
         equal to this number.  For example, if this is set to 2,
         `lisp-indent-offset' will not be set only if indent_size is 2.")

(define-error 'editorconfig-error
              "Error thrown from editorconfig lib")

(defun editorconfig-error (&rest args)
  "Signal an `editorconfig-error'.
Make a message by passing ARGS to `format-message'."
  (signal 'editorconfig-error (list (apply #'format-message args))))

(defun editorconfig-string-integer-p (string)
  "Return non-nil if STRING represents integer."
  (and (stringp string)
       (string-match-p "\\`[0-9]+\\'" string)))

(defun editorconfig--get-indentation-web-mode (size)
  `((web-mode-indent-style . 2)
    (web-mode-attr-indent-offset       . ,size)
    (web-mode-attr-value-indent-offset . ,size)
    (web-mode-code-indent-offset       . ,size)
    (web-mode-css-indent-offset        . ,size)
    (web-mode-markup-indent-offset     . ,size)
    (web-mode-sql-indent-offset        . ,size)
    (web-mode-block-padding            . ,size)
    (web-mode-script-padding           . ,size)
    (web-mode-style-padding            . ,size)))

(defun editorconfig--get-indentation-python-mode (size)
  "Vars to set `python-mode' indent size to SIZE."
  `((python-indent-offset . ,size)      ;For python.el
    (py-indent-offset . ,size)))        ;For python-mode.el

(defun editorconfig--get-indentation-latex-mode (size)
  "Vars to set `latex-mode' indent size to SIZE."
  `((tex-indent-basic . ,size)
    (tex-indent-item . ,size)
    (tex-indent-arg . ,(* 2 size))
    ;; For AUCTeX
    (TeX-brace-indent-level . ,size)
    (LaTeX-indent-level . ,size)
    (LaTeX-item-indent . ,(- size))))

(defun editorconfig--get-indentation-nxml-mode (size)
  "Vars to set `nxml-mode' indent size to SIZE."
  `((nxml-child-indent . ,size)
    (nxml-attribute-indent . ,(* 2 size))))

(defun editorconfig--get-indentation-lisp-mode (size)
  "Set indent size to SIZE for Lisp mode(s)."
  (when (cond ((null editorconfig-lisp-use-default-indent)  t)
              ((eql t editorconfig-lisp-use-default-indent) nil)
              ((numberp editorconfig-lisp-use-default-indent)
               (not (eql size editorconfig-lisp-use-default-indent)))
              (t t))
    `((lisp-indent-offset . ,size))))

(cl-defun editorconfig--should-set (symbol)
  "Determine if editorconfig should set SYMBOL."
  (display-warning '(editorconfig editorconfig--should-set)
                   (format "symbol: %S"
                           symbol)
                   :debug)
  (when (assq symbol file-local-variables-alist)
    (cl-return-from editorconfig--should-set
      nil))

  (when (assq symbol dir-local-variables-alist)
    (cl-return-from editorconfig--should-set
      nil))

  t)

(defvar editorconfig-indent-size-vars
  #'editorconfig--default-indent-size-function
  "Rule to use to set a given `indent_size'.
This should hold the list of variables that need to be set to SIZE
to tell the indentation code of the current major mode to use a basic
indentation step of size SIZE.
It can also take the form of a function, in which case we call it with
a single SIZE argument (an integer) and it should return a list
of (VAR . VAL) pairs indicating the variables to set and the values to
set them to.
Major modes are expected to set this buffer-locally.")

(defun editorconfig--default-indent-size-function (size)
 "Guess which variables to set to for the indentation step to have size SIZE.
This relies on `editorconfig-indentation-alist' supplemented with a crude
heuristic for those modes not found there."
  (let ((parents (if (fboundp 'derived-mode-all-parents) ;Emacs-30
                     (derived-mode-all-parents major-mode)
                   (let ((modes nil)
                         (mode major-mode))
                     (while mode
                       (push mode modes)
                       (setq mode (get mode 'derived-mode--parent)))
                     (nreverse modes))))
        entry)
    (let ((parents parents))
      (while (and parents (not entry))
        (setq entry (assq (pop parents) editorconfig-indentation-alist))))
    (or
     (when entry
       (let ((rule (cdr entry)))
         ;; Filter out settings of unknown vars.
         (delq nil
               (mapcar (lambda (elem)
                         (let ((v (car elem)))
                           (cond
                            ((not (symbolp v))
                             (message "Unsupported element in `editorconfig-indentation-alist': %S" elem))
                            ((or (eq 'eval v) (boundp v)) elem))))
                       (if (functionp rule)
                           (funcall rule size)
                         (mapcar (lambda (elem) `(,elem . ,size)) rule))))))
     ;; Fallback, let's try and guess.
     (let ((suffixes '("-indent-level" "-basic-offset" "-indent-offset"))
           (guess ()))
       (while (and parents (not guess))
         (let* ((mode (pop parents))
                (modename (symbol-name mode))
                (name (substring modename 0
                                 (string-match "-mode\\'" modename))))
           (dolist (suffix suffixes)
             (let ((sym (intern-soft (concat name suffix))))
               (when (and sym (boundp sym))
                 (setq guess sym))))))
       (when guess `((,guess . ,size))))
     (and (local-variable-p 'smie-rules-function)
          `((smie-indent-basic . ,size))))))

(defun editorconfig--get-indentation (props)
  "Get indentation vars according to STYLE, SIZE, and TAB_WIDTH."
  (let ((style (gethash 'indent_style props))
        (size (gethash 'indent_size props))
        (tab_width (gethash 'tab_width props)))
    (cond
     (tab_width (setq tab_width (string-to-number tab_width)))
     ;; The EditorConfig spec is excessively eager to set `tab-width'
     ;; even when not explicitly requested (bug#73991).
     ;; As a trade-off, we accept `indent_style=tab' as a good enough hint.
     ((and (equal style "tab") (editorconfig-string-integer-p size))
      (setq tab_width (string-to-number size))))

    ;; When users choose `indent_size=tab', they most likely prefer
    ;; `indent_style=tab' as well.
    (when (and (null style) (equal size "tab"))
      (setq style "tab"))

    (setq size
          (cond ((editorconfig-string-integer-p size)
                 (string-to-number size))
                ((equal size "tab")
                 (or tab_width tab-width))
                (t
                 nil)))
    `(,@(if tab_width `((tab-width . ,tab_width)))

      ,@(pcase style
          ("space" `((indent-tabs-mode . nil)))
          ("tab" `((indent-tabs-mode . t))))

      ,@(when (and size (featurep 'evil))
          `((evil-shift-width . ,size)))

      ,@(cond
         ((null size) nil)
         ((functionp editorconfig-indent-size-vars)
          (funcall editorconfig-indent-size-vars size))
         (t (mapcar (lambda (v) `(,v . ,size)) editorconfig-indent-size-vars))))))

(defvar-local editorconfig--apply-coding-system-currently nil
  "Used internally.")
(put 'editorconfig--apply-coding-system-currently 'permanent-local t)

(defun editorconfig-merge-coding-systems (end-of-line charset)
  "Return merged coding system symbol of END-OF-LINE and CHARSET."
  (let ((eol (cond
              ((equal end-of-line "lf") 'undecided-unix)
              ((equal end-of-line "cr") 'undecided-mac)
              ((equal end-of-line "crlf") 'undecided-dos)))
        (cs (cond
             ((equal charset "latin1") 'iso-latin-1)
             ((equal charset "utf-8") 'utf-8)
             ((equal charset "utf-8-bom") 'utf-8-with-signature)
             ((equal charset "utf-16be") 'utf-16be-with-signature)
             ((equal charset "utf-16le") 'utf-16le-with-signature))))
    (if (and eol cs)
        (merge-coding-systems cs eol)
      (or eol cs))))

(cl-defun editorconfig-set-coding-system-revert (end-of-line charset)
  "Set buffer coding system by END-OF-LINE and CHARSET.

This function will revert buffer when the coding-system has been changed."
  ;; `editorconfig--advice-find-file-noselect' does not use this function
  (let ((coding-system (editorconfig-merge-coding-systems end-of-line
                                                          charset)))
    (display-warning '(editorconfig editorconfig-set-coding-system-revert)
                     (format "editorconfig-set-coding-system-revert: buffer-file-name: %S | buffer-file-coding-system: %S | coding-system: %S | apply-currently: %S"
                             buffer-file-name
                             buffer-file-coding-system
                             coding-system
                             editorconfig--apply-coding-system-currently)
                     :debug)
    (when (memq coding-system '(nil undecided))
      (cl-return-from editorconfig-set-coding-system-revert))
    (when (and buffer-file-coding-system
               (memq buffer-file-coding-system
                     (coding-system-aliases (merge-coding-systems coding-system
                                                                  buffer-file-coding-system))))
      (cl-return-from editorconfig-set-coding-system-revert))
    (unless (file-readable-p buffer-file-name)
      (set-buffer-file-coding-system coding-system)
      (cl-return-from editorconfig-set-coding-system-revert))
    (unless (memq coding-system
                  (coding-system-aliases editorconfig--apply-coding-system-currently))
      ;; Revert functions might call `editorconfig-apply' again
      ;; FIXME: I suspect `editorconfig--apply-coding-system-currently'
      ;; gymnastics is not needed now that we hook into `find-auto-coding'.
      (unwind-protect
          (progn
            (setq editorconfig--apply-coding-system-currently coding-system)
            ;; Revert without query if buffer is not modified
            (let ((revert-without-query '(".")))
              (revert-buffer-with-coding-system coding-system)))
        (setq editorconfig--apply-coding-system-currently nil)))))

(defun editorconfig--get-trailing-nl (props)
  "Get the vars to require final newline according to PROPS."
  (pcase (gethash 'insert_final_newline props)
    ("true"
     ;; Keep prefs around how/when the nl is added, if set.
     `((require-final-newline
        . ,(or require-final-newline mode-require-final-newline t))))
    ("false"
     `((require-final-newline . nil)))))

(defun editorconfig--delete-trailing-whitespace ()
  "Call `delete-trailing-whitespace' unless the buffer is read-only."
  (unless buffer-read-only (delete-trailing-whitespace)))

(defun editorconfig--get-trailing-ws (props)
  "Get vars to trim of trailing whitespace according to PROPS."
  (let ((fun (or editorconfig-trim-whitespaces-mode
                 #'delete-trailing-whitespace-mode)))
    (pcase (gethash 'trim_trailing_whitespace props)
      ("true" `((eval . (,fun 1))))
      ("false"
       ;; Just do it right away rather than return a (VAR . VAL), which
       ;; would be probably more trouble than it's worth.
       (funcall fun 0)
       nil))))

(defun editorconfig--get-line-length (props)
  "Get the max line length (`fill-column') to PROPS."
  (let ((length (gethash 'max_line_length props)))
    (when (and (editorconfig-string-integer-p length)
               (> (string-to-number length) 0))
      `((fill-column . ,(string-to-number length))))))

(defun editorconfig-call-get-properties-function (filename)
  "Call `editorconfig-core-get-properties-hash' with FILENAME and return result.
This function also removes `unset' properties."
  (if (stringp filename)
      (setq filename (expand-file-name filename))
    (editorconfig-error "Invalid argument: %S" filename))
  (let ((props nil))
    (condition-case-unless-debug err
        (setq props (editorconfig-core-get-properties-hash filename))
      (error
       (editorconfig-error "Error from editorconfig-core-get-properties-hash: %S"
                           err)))
    (cl-loop for k being the hash-keys of props using (hash-values v)
             when (equal v "unset") do (remhash k props))
    ;; E.g. for `editorconfig-display-current-properties'.
    ;; FIXME: Use it for memoization as well to avoid the duplicate
    ;; calls to `editorconfig-core-get-properties-hash' (one for
    ;; `editorconfig--get-coding-system' and one for
    ;; `editorconfig--get-dir-local-variables')?
    (setq editorconfig-properties-hash props)
    props))

(defvar editorconfig-get-local-variables-functions
  '(editorconfig--get-indentation
    editorconfig--get-trailing-nl
    editorconfig--get-trailing-ws
    editorconfig--get-line-length)
  "Special hook run to convert EditorConfig settings to their Emacs equivalent.
Every function is called with one argument, a hash-table indexed by
EditorConfig settings represented as symbols and whose corresponding value
is represented as a string.  It should return a list of (VAR . VAL) settings
where VAR is a Lisp variable and VAL is the value to which it should be set.")

(defun editorconfig--get-local-variables (props)
  "Get variables settings according to EditorConfig PROPS."
  (let ((alist ()))
    (run-hook-wrapped 'editorconfig-get-local-variables-functions
                      (lambda (fun props)
                        (setq alist (append (funcall fun props) alist))
                        nil)
                      props)
    alist))

(defun editorconfig-set-local-variables (props)
  "Set buffer variables according to EditorConfig PROPS."
  (pcase-dolist (`(,var . ,val) (editorconfig--get-local-variables props))
    (if (eq 'eval var)
        (eval val t)
      (when (editorconfig--should-set var)
        (set (make-local-variable var) val)))))

(defun editorconfig-major-mode-hook ()
  "Function to run when `major-mode' has been changed.

This functions does not reload .editorconfig file, just sets local variables
again.  Changing major mode can reset these variables.

This function also executes `editorconfig-after-apply-functions' functions."
  (display-warning '(editorconfig editorconfig-major-mode-hook)
                   (format "editorconfig-major-mode-hook: editorconfig-mode: %S, major-mode: %S, -properties-hash: %S"
                           (and (boundp 'editorconfig-mode)
                                editorconfig-mode)
                           major-mode
                           editorconfig-properties-hash)
                   :debug)
  (when (and (bound-and-true-p editorconfig-mode)
             editorconfig-properties-hash)
    (editorconfig-set-local-variables editorconfig-properties-hash)
    (condition-case err
        (run-hook-with-args 'editorconfig-after-apply-functions editorconfig-properties-hash)
      (error
       (display-warning '(editorconfig editorconfig-major-mode-hook)
                        (format "Error while running `editorconfig-after-apply-functions': %S"
                                err))))))

(defun editorconfig--advice-find-auto-coding (filename &rest _args)
  "Consult `charset' setting of EditorConfig."
  (let ((cs (dlet ((auto-coding-file-name filename))
              (editorconfig--get-coding-system))))
    (when cs (cons cs 'EditorConfig))))

(defun editorconfig--advice-find-file-noselect (f filename &rest args)
  "Get EditorConfig properties and apply them to buffer to be visited.

This function should be added as an advice function to `find-file-noselect'.
F is that function, and FILENAME and ARGS are arguments passed to F."
  (let ((props nil)
        (ret nil))
    (condition-case err
        (when (stringp filename)
          (setq props (editorconfig-call-get-properties-function filename)))
      (error
       (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                        (format "Failed to get properties, styles will not be applied: %S"
                                err)
                        :warning)))

    (setq ret (apply f filename args))

    (condition-case err
        (with-current-buffer ret
          (when props

            ;; NOTE: hack-properties-functions cannot affect coding-system value,
            ;; because it has to be set before initializing buffers.
            (condition-case err
                (run-hook-with-args 'editorconfig-hack-properties-functions props)
              (error
               (display-warning '(editorconfig editorconfig-hack-properties-functions)
                                (format "Error while running editorconfig-hack-properties-functions, abort running hook: %S"
                                        err)
                                :warning)))
            (setq editorconfig-properties-hash props)
            ;; When initializing buffer, `editorconfig-major-mode-hook'
            ;; will be called before setting `editorconfig-properties-hash', so
            ;; execute this explicitly here.
            (editorconfig-set-local-variables props)

            (condition-case err
                (run-hook-with-args 'editorconfig-after-apply-functions props)
              (error
               (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                                (format "Error while running `editorconfig-after-apply-functions': %S"
                                        err))))))
      (error
       (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                        (format "Error while setting variables from EditorConfig: %S" err))))
    ret))

(defvar editorconfig--getting-coding-system nil)

(defun editorconfig--get-coding-system (&optional _size)
  "Return the coding system to use according to EditorConfig.
Meant to be used on `auto-coding-functions'."
  (defvar auto-coding-file-name) ;; Emacs≥30
  ;; Not only we don't want that an error in the `.editorconfig' file
  ;; prevents opening a file but we don't want an error to be dropped on
  ;; the floor by some `ignore-errors' higher up.
  (with-demoted-errors "EditorConfig: %S"
    (when (and (stringp auto-coding-file-name)
               (file-name-absolute-p auto-coding-file-name)
	       ;; Don't recurse infinitely.
	       (not (member auto-coding-file-name
	                    editorconfig--getting-coding-system)))
      (let* ((editorconfig--getting-coding-system
              (cons auto-coding-file-name editorconfig--getting-coding-system))
             (props (editorconfig-call-get-properties-function
                     auto-coding-file-name)))
        (editorconfig-merge-coding-systems (gethash 'end_of_line props)
                                           (gethash 'charset props))))))

(defun editorconfig--get-dir-local-variables ()
  "Return the directory local variables specified via EditorConfig.
Meant to be used on `hack-dir-local-get-variables-functions'."
  (when (stringp buffer-file-name)
    (let* ((props (editorconfig-call-get-properties-function buffer-file-name))
           (alist (if (< 0 (hash-table-count props))
                      (editorconfig--get-local-variables props))))
      ;; FIXME: If there's `/foo/.editorconfig', `/foo/bar/.dir-locals.el',
      ;; and `/foo/bar/baz/.editorconfig', it would be nice to return two
      ;; pairs here, so that hack-dir-local can give different priorities
      ;; to the `/foo/.editorconfig' settings compared to those of
      ;; `/foo/bar/baz/.editorconfig', but we can't just convert the
      ;; settings from each file individually and let hack-dir-local merge
      ;; them because hack-dir-local doesn't have the notion of "unset",
      ;; and because the conversion of `indent_size' depends on `tab_width'.
      (when alist
        (cons
         (file-name-directory (editorconfig-core-get-nearest-editorconfig
                               buffer-file-name))
         alist)))))

;;;###autoload
(define-minor-mode editorconfig-mode
  "Toggle EditorConfig feature."
  :global t
  (if (boundp 'hack-dir-local-get-variables-functions) ;Emacs≥30
      (if editorconfig-mode
          (progn
            (add-hook 'hack-dir-local-get-variables-functions
                      ;; Give them slightly lower precedence than settings from
                      ;; `dir-locals.el'.
                      #'editorconfig--get-dir-local-variables t)
            ;; `auto-coding-functions' also exists in Emacs<30 but without
            ;; access to the file's name via `auto-coding-file-name'.
            (add-hook 'auto-coding-functions
                      #'editorconfig--get-coding-system))
        (remove-hook 'hack-dir-local-get-variables-functions
                     #'editorconfig--get-dir-local-variables)
        (remove-hook 'auto-coding-functions
                     #'editorconfig--get-coding-system))
    ;; Emacs<30
    (let ((modehooks '(prog-mode-hook
                       text-mode-hook
                       ;; Some modes call `kill-all-local-variables' in their init
                       ;; code, which clears some values set by editorconfig.
                       ;; For those modes, editorconfig-apply need to be called
                       ;; explicitly through their hooks.
                       rpm-spec-mode-hook)))
      (if editorconfig-mode
          (progn
            (advice-add 'find-file-noselect :around #'editorconfig--advice-find-file-noselect)
            (advice-add 'find-auto-coding :after-until
                        #'editorconfig--advice-find-auto-coding)
            (dolist (hook modehooks)
              (add-hook hook
                        #'editorconfig-major-mode-hook
                        t)))
        (advice-remove 'find-file-noselect #'editorconfig--advice-find-file-noselect)
        (advice-remove 'find-auto-coding
                       #'editorconfig--advice-find-auto-coding)
        (dolist (hook modehooks)
          (remove-hook hook #'editorconfig-major-mode-hook))))))


;; (defconst editorconfig--version
;;   (eval-when-compile
;;     (require 'lisp-mnt)
;;     (declare-function lm-version "lisp-mnt" nil)
;;     (lm-version))
;;   "EditorConfig version.")

;; ;;;###autoload
;; (defun editorconfig-version (&optional show-version)
;;   "Get EditorConfig version as string.
;;
;; If called interactively or if SHOW-VERSION is non-nil, show the
;; version in the echo area and the messages buffer."
;;   (interactive (list t))
;;   (let ((version-full
;;          (if (fboundp 'package-get-version)
;;              (package-get-version)
;;            (let* ((version
;;                    (with-temp-buffer
;;                      (require 'find-func)
;;                      (declare-function find-library-name "find-func" (library))
;;                      (insert-file-contents (find-library-name "editorconfig"))
;;                      (require 'lisp-mnt)
;;                      (declare-function lm-version "lisp-mnt" nil)
;;                      (lm-version)))
;;                   (pkg (and (eval-and-compile (require 'package nil t))
;;                             (cadr (assq 'editorconfig
;;                                         package-alist))))
;;                   (pkg-version (and pkg (package-version-join
;;                                          (package-desc-version pkg)))))
;;              (if (and pkg-version
;;                       (not (string= version pkg-version)))
;;                  (concat version "-" pkg-version)
;;                version)))))
;;     (when show-version
;;       (message "EditorConfig Emacs v%s" version-full))
;;     version-full))

(provide 'editorconfig)
;;; editorconfig.el ends here
