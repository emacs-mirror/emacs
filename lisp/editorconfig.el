;;; editorconfig.el --- EditorConfig Emacs Plugin  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024  Free Software Foundation, Inc.

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.11.0
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme
;; Package-Requires: ((emacs "26.1") (nadvice "0.3"))
;; Keywords: convenience editorconfig

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

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

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'nadvice)

(eval-when-compile
  (require 'rx)
  (require 'subr-x)
  (defvar tex-indent-basic)
  (defvar tex-indent-item)
  (defvar tex-indent-arg)
  (defvar evil-shift-width))

(require 'editorconfig-core)

(defgroup editorconfig nil
  "EditorConfig Emacs Plugin.

EditorConfig helps developers define and maintain consistent
coding styles between different editors and IDEs."
  :tag "EditorConfig"
  :prefix "editorconfig-"
  :group 'tools)

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
  :type 'hook)

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
  :type 'hook)
(make-obsolete-variable 'editorconfig-hack-properties-functions
                        "Using `editorconfig-after-apply-functions' instead is recommended,
    because since 2021/08/30 (v0.9.0) this variable cannot support all properties:
    charset values will be referenced before running this hook."
                        "v0.9.0")

(define-obsolete-variable-alias
  'edconf-indentation-alist
  'editorconfig-indentation-alist
  "0.5")
(defcustom editorconfig-indentation-alist
  ;; For contributors: Sort modes in alphabetical order
  `((apache-mode apache-indent-level)
    (bash-ts-mode sh-basic-offset
                  sh-indentation)
    (bpftrace-mode c-basic-offset)
    (c++-ts-mode c-basic-offset
                 c-ts-mode-indent-offset)
    (c-ts-mode c-basic-offset
               c-ts-mode-indent-offset)
    (cmake-mode cmake-tab-width)
    (cmake-ts-mode cmake-tab-width
                   cmake-ts-mode-indent-offset)
    (csharp-mode c-basic-offset)
    (csharp-ts-mode c-basic-offset
                    csharp-ts-mode-indent-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (ess-mode ess-indent-offset)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (gdscript-mode gdscript-indent-offset)
    (go-ts-mode go-ts-mode-indent-offset)
    (hcl-mode hcl-indent-level)
    (html-ts-mode html-ts-mode-indent-offset)
    (java-ts-mode c-basic-offset
                  java-ts-mode-indent-offset)
    (js-mode js-indent-level)
    (jsonian-mode jsonian-default-indentation)
    (latex-mode . editorconfig-set-indentation-latex-mode)
    (lisp-mode lisp-indent-offset)
    (matlab-mode matlab-indent-level)
    (octave-mode octave-block-offset)
    ;; No need to change `php-mode-coding-style' value for php-mode
    ;; since we run editorconfig later than it resets `c-basic-offset'.
    ;; See https://github.com/editorconfig/editorconfig-emacs/issues/116
    ;; for details.
    (php-mode c-basic-offset)
    (php-ts-mode php-ts-mode-indent-offset)
    (ps-mode ps-mode-tab)
    (ruby-mode ruby-indent-level)
    (rust-ts-mode rust-indent-offset
                  rust-ts-mode-indent-offset)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-indentation)
    (svelte-mode svelte-basic-offset)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (toml-ts-mode toml-ts-mode-indent-offset)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode (web-mode-indent-style . ,(lambda (size) 2))
              web-mode-markup-indent-offset
              web-mode-css-indent-offset
              web-mode-code-indent-offset)
    (zig-mode zig-indent-offset)
    )
  "Alist of indentation setting methods by modes."
  :risky t)

(defcustom editorconfig-trim-whitespaces-mode nil
  "Buffer local minor-mode to use to trim trailing whitespaces.

If set, enable that mode when `trim_trailing_whitespace` is set to true.
Otherwise, use `delete-trailing-whitespace'."
  :type 'symbol)

(defvar editorconfig-properties-hash nil
  "Hash object of EditorConfig properties that was enabled for current buffer.
Set by `editorconfig-apply' and nil if that is not invoked in
current buffer yet.")
(make-variable-buffer-local 'editorconfig-properties-hash)
(put 'editorconfig-properties-hash
     'permanent-local
     t)

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

(defun editorconfig-set-indentation-latex-mode (size)
  "Set `latex-mode' indent size to SIZE."
  (setq-local LaTeX-indent-level size)
  (setq-local LaTeX-item-indent size)
  (setq-local TeX-brace-indent-level size))

(cl-defun editorconfig--should-set (symbol &optional size)
  "Determine if editorconfig should set SYMBOL.

Optional arg SIZE is used when symbol is `lisp-indent-offset'.
See `editorconfig-lisp-use-default-indent' for details."
  (display-warning '(editorconfig editorconfig--should-set)
                   (format "symbol: %S | size: %S"
                           symbol
                           size)
                   :debug)
  (when (assq symbol file-local-variables-alist)
    (cl-return-from editorconfig--should-set
      nil))

  (when (assq symbol dir-local-variables-alist)
    (cl-return-from editorconfig--should-set
      nil))

  (when (eq symbol 'lisp-indent-offset)
    (cl-return-from editorconfig--should-set
      (cond ((null editorconfig-lisp-use-default-indent)  t)
            ((eql t editorconfig-lisp-use-default-indent) nil)
            ((numberp editorconfig-lisp-use-default-indent)
             (not (eql size editorconfig-lisp-use-default-indent)))
            (t t))))

  t)

(defun editorconfig-set-indentation (style &optional size tab_width)
  "Set indentation type from STYLE, SIZE and TAB_WIDTH."
  (setq size
        (cond ((editorconfig-string-integer-p size)
               (string-to-number size))
              ((equal size "tab")
               "tab")
              (t
               nil)))
  (cond ((not (editorconfig--should-set 'tab-width))
         nil)
   )

  (cond ((not (editorconfig--should-set 'indent-tabs-mode))
         nil)
   )

  (when (editorconfig--should-set 'evil-shift-width)
    )
  )

(defvar-local editorconfig--apply-coding-system-currently nil
  "Used internally.")
(put 'editorconfig--apply-coding-system-currently
     'permanent-local
     t)

(defun editorconfig-merge-coding-systems (end-of-line charset)
  "Return merged coding system symbol of END-OF-LINE and CHARSET."
  (let ((eol (cond
              ((equal end-of-line "lf") 'undecided-unix)
              ((equal end-of-line "cr") 'undecided-mac)
              ((equal end-of-line "crlf") 'undecided-dos)
              (t 'undecided)))
        (cs (cond
             ((equal charset "latin1") 'iso-latin-1)
             ((equal charset "utf-8") 'utf-8)
             ((equal charset "utf-8-bom") 'utf-8-with-signature)
             ((equal charset "utf-16be") 'utf-16be-with-signature)
             ((equal charset "utf-16le") 'utf-16le-with-signature)
             (t 'undecided))))
    (merge-coding-systems cs eol)))

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
    (when (eq coding-system 'undecided)
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
      ;; Revert functions might call editorconfig-apply again
      (unwind-protect
          (progn
            (setq editorconfig--apply-coding-system-currently coding-system)
            ;; Revert without query if buffer is not modified
            (let ((revert-without-query '(".")))
              (revert-buffer-with-coding-system coding-system)))
        (setq editorconfig--apply-coding-system-currently nil)))))

(defun editorconfig-set-trailing-nl (final-newline)
  "Set up requiring final newline by FINAL-NEWLINE.

This function will set `require-final-newline' and `mode-require-final-newline'
to non-nil when FINAL-NEWLINE is true."
  (pcase final-newline
   ("true"
    )
   ("false"
    )))

(defun editorconfig-set-trailing-ws (trim-trailing-ws)
    (if editorconfig-trim-whitespaces-mode
        (funcall editorconfig-trim-whitespaces-mode 1)
      ...)
    (when editorconfig-trim-whitespaces-mode
      (funcall editorconfig-trim-whitespaces-mode 0))
    ...)

(defun editorconfig-set-line-length (length)
  "Set the max line length (`fill-column') to LENGTH."
  (when (and (editorconfig-string-integer-p length)
             (> (string-to-number length) 0))
    (setq fill-column (string-to-number length))))

(defun editorconfig-call-get-properties-function (filename)
  "Call `editorconfig-core-get-properties-hash' with FILENAME and return result.

This function also removes `unset' properties and calls
`editorconfig-hack-properties-functions'."
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
    props))

(defun editorconfig-set-local-variables (props)
  "Set buffer variables according to EditorConfig PROPS."
  (editorconfig-set-indentation (gethash 'indent_style props)
                                (gethash 'indent_size props)
                                (gethash 'tab_width props))
  (editorconfig-set-trailing-nl (gethash 'insert_final_newline props))
  (editorconfig-set-trailing-ws (gethash 'trim_trailing_whitespace props))
  (editorconfig-set-line-length (gethash 'max_line_length props)))


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

(defvar editorconfig--filename-codingsystem-hash (make-hash-table :test 'equal)
  "Used interally.

`editorconfig--advice-find-file-noselect' will put value to this hash, and
`editorconfig--advice-insert-file-contents' will use the value to set
`coding-system-for-read' value.")

(defun editorconfig--advice-insert-file-contents (f filename &rest args)
  "Set `coding-system-for-read'.

This function should be added as an advice function to `insert-file-contents'.
F is that function, and FILENAME and ARGS are arguments passed to F."
  ;; This function uses `editorconfig--filename-codingsystem-hash' to decide what coding-system
  ;; should be used, which will be set by `editorconfig--advice-find-file-noselect'.
  (display-warning '(editorconfig editorconfig--advice-insert-file-contents)
                   (format "editorconfig--advice-insert-file-contents: filename: %S args: %S codingsystem: %S bufferfilename: %S"
                           filename args
                           editorconfig--filename-codingsystem-hash
                           buffer-file-name)
                   :debug)
  (let ((coding-system (and (stringp filename)
                            (gethash (expand-file-name filename)
                                     editorconfig--filename-codingsystem-hash))))
    (if (and coding-system
             (not (eq coding-system
                      'undecided)))
        (let ((coding-system-for-read coding-system))
          (apply f filename args))
      (apply f filename args))))

(defun editorconfig--advice-find-file-noselect (f filename &rest args)
  "Get EditorConfig properties and apply them to buffer to be visited.

This function should be added as an advice function to `find-file-noselect'.
F is that function, and FILENAME and ARGS are arguments passed to F."
  (let ((props nil)
        (coding-system nil)
        (ret nil))
    (condition-case err
        (when (stringp filename)
          (setq props (editorconfig-call-get-properties-function filename))
          (setq coding-system
                (editorconfig-merge-coding-systems (gethash 'end_of_line props)
                                                   (gethash 'charset props)))
          (puthash (expand-file-name filename)
                   coding-system
                   editorconfig--filename-codingsystem-hash))
      (error
       (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                        (format "Failed to get properties, styles will not be applied: %S"
                                err)
                        :warning)))

    (setq ret (apply f filename args))
    (clrhash editorconfig--filename-codingsystem-hash)

    (condition-case err
        (with-current-buffer ret
          (when props

            ;; When file path indicates it is a remote file and it actually
            ;; does not exists, `buffer-file-coding-system' will not be set.
            ;; (Seems `insert-file-contents' will not be called)
            ;; For this case, explicitly set this value so that saving will be done
            ;; with expected coding system.
            (when (and (file-remote-p filename)
                       (not (local-variable-p 'buffer-file-coding-system))
                       (not (file-exists-p filename))
                       coding-system
                       (not (eq coding-system
                                'undecided)))
              (set-buffer-file-coding-system coding-system))

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


;;;###autoload
(define-minor-mode editorconfig-mode
  "Toggle EditorConfig feature."
  :global t
  (let ((modehooks '(prog-mode-hook
                     text-mode-hook
                     read-only-mode-hook
                     ;; Some modes call `kill-all-local-variables' in their init
                     ;; code, which clears some values set by editorconfig.
                     ;; For those modes, editorconfig-apply need to be called
                     ;; explicitly through their hooks.
                     rpm-spec-mode-hook)))
    (if editorconfig-mode
        (progn
          (advice-add 'find-file-noselect :around 'editorconfig--advice-find-file-noselect)
          (advice-add 'insert-file-contents :around 'editorconfig--advice-insert-file-contents)
          (dolist (hook modehooks)
            (add-hook hook
                      'editorconfig-major-mode-hook
                      t)))
      (advice-remove 'find-file-noselect 'editorconfig--advice-find-file-noselect)
      (advice-remove 'insert-file-contents 'editorconfig--advice-insert-file-contents)
      (dolist (hook modehooks)
        (remove-hook hook 'editorconfig-major-mode-hook)))))


;; (defconst editorconfig--version
;;   (eval-when-compile
;;     (require 'lisp-mnt)
;;     (declare-function lm-version "lisp-mnt" nil)
;;     (lm-version))
;;   "EditorConfig version.")

(declare-function find-library-name "find-func" (library))
(declare-function lm-version "lisp-mnt" nil)

;;;###autoload
(defun editorconfig-version (&optional show-version)
  "Get EditorConfig version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer."
  (interactive (list t))
  (let* ((version (with-temp-buffer
                    (require 'find-func)
                    (insert-file-contents (find-library-name "editorconfig"))
                    (require 'lisp-mnt)
                    (lm-version)))
         (pkg (and (eval-and-compile (require 'package nil t))
                   (cadr (assq 'editorconfig
                               package-alist))))
         (pkg-version (and pkg
                           (package-version-join (package-desc-version pkg))))
         (version-full (if (and pkg-version
                                (not (string= version pkg-version)))
                           (concat version "-" pkg-version)
                         version)))
    (when show-version
      (message "EditorConfig Emacs v%s" version-full))
    version-full))

(provide 'editorconfig)
;;; editorconfig.el ends here
