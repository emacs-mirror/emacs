;;; less-css-mode.el --- Major mode for editing Less CSS files  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Steve Purcell <steve@sanityinc.com>
;; Maintainer: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords: hypermedia

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

;; This mode provides syntax highlighting for Less CSS files
;; (http://lesscss.org/), plus optional support for compilation of
;; .less files to .css files at the time they are saved: use
;; `less-css-compile-at-save' to enable this.
;;
;; Command line utility "lessc" is required if setting
;; `less-css-compile-at-save' to t.  To install "lessc" using the
;; Node.js package manager, run "npm install less".
;;
;; Also make sure the "lessc" executable is in Emacs' PATH, example:
;; (push (expand-file-name "~/.gem/ruby/1.8/bin") exec-path)
;; or customize `less-css-lessc-command' to point to your "lessc"
;; executable.
;;
;; We target lessc >= 1.4.0, and thus use the `--no-color' flag by
;; default.  You may want to adjust `less-css-lessc-options' for
;; compatibility with older versions.
;;
;; `less-css-mode' is derived from `css-mode', and indentation of
;; nested blocks may not work correctly with versions of `css-mode'
;; other than that bundled with recent Emacs.
;;
;; You can specify per-file values for `less-css-compile-at-save',
;; `less-css-output-file-name' or `less-css-output-directory' using a
;; variables header at the top of your .less file, e.g.:
;;
;; // -*- less-css-compile-at-save: t; less-css-output-directory: "../css" -*-
;;
;; Alternatively, you can use directory local variables to set the
;; default value of `less-css-output-directory' for your project.
;;
;; In the case of files which are included in other .less files, you
;; may want to trigger the compilation of a "master" .less file on
;; save: you can accomplish this with `less-css-input-file-name',
;; which is probably best set using directory local variables.
;;
;; If you don't need CSS output but would like to be warned of any
;; syntax errors in your .less source, consider using `flymake-less':
;; https://github.com/purcell/flymake-less.

;;; Credits

;; The original code for this mode was, in large part, written using
;; Anton Johansson's scss-mode as a template -- thanks Anton!
;; https://github.com/antonj

;;; Code:

(require 'compile)
(require 'css-mode)
(require 'derived)
(eval-when-compile (require 'subr-x))

(defgroup less-css nil
  "Less CSS mode."
  :prefix "less-css-"
  :group 'css)

(defcustom less-css-lessc-command "lessc"
  "Command used to compile Less files.
Should be \"lessc\" or the complete path to your lessc
executable, e.g.: \"~/.gem/ruby/1.8/bin/lessc\"."
  :type 'file)

(defcustom less-css-compile-at-save nil
  "If non-nil, Less buffers are compiled to CSS after each save."
  :type 'boolean)
;;;###autoload
(put 'less-css-compile-at-save 'safe-local-variable 'booleanp)

(defcustom less-css-lessc-options '("--no-color")
  "Command line options for Less executable.
Use \"-x\" to minify output."
  :type '(repeat string))
;;;###autoload
(put 'less-css-lessc-options 'safe-local-variable t)

(defcustom less-css-output-directory nil
  "Directory in which to save CSS, or nil to use the Less file's directory.
This path is expanded relative to the directory of the Less file
using `expand-file-name', so both relative and absolute paths
will work as expected."
  :type 'directory)
;;;###autoload
(put 'less-css-output-directory 'safe-local-variable 'stringp)

(defcustom less-css-output-file-name nil
  "File name in which to save CSS, or nil to use <name>.css for <name>.less.
This can be also be set to a full path, or a relative path.  If
the path is relative, it will be relative to the value of
`less-css-output-dir', if set, or the current directory by
default."
  :type 'file)
(make-variable-buffer-local 'less-css-output-file-name)

(defcustom less-css-input-file-name nil
  "File name which will be compiled to CSS.
When the current buffer is saved `less-css-input-file-name' file
will be compiled to CSS instead of the current file.

Set this in order to trigger compilation of a \"master\" .less
file which includes the current file.  The best way to set this
variable in most cases is likely to be via directory local
variables.

This can be also be set to a full path, or a relative path.  If
the path is relative, it will be relative to the current
directory by default."
  :type 'file)
;;;###autoload
(put 'less-css-input-file-name 'safe-local-variable 'stringp)
(make-variable-buffer-local 'less-css-input-file-name)

(defconst less-css-default-error-regex
  "^\\(?:\e\\[31m\\)?\\([^\e\n]*\\|FileError:.*\n\\)\\(?:\e\\[39m\e\\[31m\\)? in \\(?:\e\\[39m\\)?\\([^ \r\n\t\e]+\\)\\(?:\e\\[90m\\)?\\(?::\\| on line \\)\\([0-9]+\\)\\(?::\\|, column \\)\\([0-9]+\\):?\\(?:\e\\[39m\\)?")

;;; Compilation to CSS

(add-to-list 'compilation-error-regexp-alist-alist
             (list 'less-css less-css-default-error-regex 2 3 4 nil 1))
(add-to-list 'compilation-error-regexp-alist 'less-css)

(defun less-css-compile-maybe ()
  "Run `less-css-compile' if `less-css-compile-at-save' is non-nil."
  (when less-css-compile-at-save
    (less-css-compile)))

(defun less-css--output-path ()
  "Return the path to use for the compiled CSS file."
  (expand-file-name
   (or less-css-output-file-name
       (concat
        (file-name-nondirectory
         (file-name-sans-extension buffer-file-name))
        ".css"))
   (or less-css-output-directory default-directory)))

(defun less-css-compile ()
  "Compile the current buffer to CSS using `less-css-lessc-command'."
  (interactive)
  (message "Compiling Less to CSS")
  (let ((compilation-buffer-name-function
         (lambda (_) "*less-css-compilation*")))
    (save-window-excursion
      (with-current-buffer
          (compile
           (string-join
            (append
             (list less-css-lessc-command)
             (mapcar #'shell-quote-argument less-css-lessc-options)
             (list (shell-quote-argument
                    (or less-css-input-file-name buffer-file-name))
                   (shell-quote-argument (less-css--output-path))))
            " "))
        (add-hook 'compilation-finish-functions
                  (lambda (buf msg)
                    (unless (string-match-p "^finished" msg)
                      (display-buffer buf)))
                  nil
                  t)))))

;;; Major mode

;; TODO:
;; - interpolation ("@{val}")
;; - escaped values (~"...")
;; - JS eval (~`...`)
;; - custom faces.
(defconst less-css-font-lock-keywords
  '(;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-variable-name-face)
    ("&" . font-lock-preprocessor-face)
    ;; Mixins
    ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*;" .
     (1 font-lock-keyword-face))))

(defvar less-css-mode-syntax-table
  (let ((st (make-syntax-table css-mode-syntax-table)))
    ;; C++-style comments.
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    ;; Special chars that sometimes come at the beginning of words.
    (modify-syntax-entry ?. "'" st)
    st))

(defvar less-css-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'less-css-compile)
    map))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
;;;###autoload
(define-derived-mode less-css-mode css-mode "Less"
  "Major mode for editing Less files (http://lesscss.org/).
Special commands:
\\{less-css-mode-map}"
  (font-lock-add-keywords nil less-css-font-lock-keywords)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-continue " *")
  (setq-local comment-start-skip "/[*/]+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*+/\\)")
  (add-hook 'after-save-hook 'less-css-compile-maybe nil t))

(provide 'less-css-mode)
;;; less-css-mode.el ends here
