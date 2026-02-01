;;; elisp-mode.el --- Emacs Lisp mode  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1986, 1999-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp, languages
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

;; The major mode for editing Emacs Lisp code.
;; This mode is documented in the Emacs manual.

;;; Code:

(require 'cl-generic)
(require 'lisp-mode)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table ()
  "Abbrev table for Emacs Lisp mode.
It has `lisp-mode-abbrev-table' as its parent."
  :parents (list lisp-mode-abbrev-table))

(defvar emacs-lisp-mode-syntax-table
  (let ((table (make-syntax-table lisp-data-mode-syntax-table)))
    ;; Remove the "p" flag from the entry of `@' because we use instead
    ;; `syntax-propertize' to take care of `,@', which is more precise.
    ;; FIXME: We should maybe do the same in other Lisp modes?  (bug#24542)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table used in `emacs-lisp-mode'.")

(defvar-keymap emacs-lisp-mode-map
  :doc "Keymap for Emacs Lisp mode.
All commands in `lisp-mode-shared-map' are inherited by this map."
  :parent lisp-mode-shared-map
  "M-TAB" #'completion-at-point
  "C-M-x" #'eval-defun
  "C-c C-e" #'elisp-eval-region-or-buffer
  "C-c C-f" #'elisp-byte-compile-file
  "C-c C-b" #'elisp-byte-compile-buffer
  "C-M-q" #'indent-pp-sexp)

(easy-menu-define emacs-lisp-mode-menu emacs-lisp-mode-map
  "Menu for Emacs Lisp mode."
  '("Emacs-Lisp"
    ["Indent Line" lisp-indent-line]
    ["Indent Region" indent-region
     :help "Indent each nonblank line in the region"
     :active mark-active]
    ["Comment Out Region" comment-region
     :help "Comment or uncomment each line in the region"
     :active mark-active]
    "---"
    ["Evaluate Last S-expression" eval-last-sexp
     :help "Evaluate sexp before point; print value in echo area"]
    ["Evaluate Region" eval-region
     :help "Execute the region as Lisp code"
     :active mark-active]
    ["Evaluate Buffer" eval-buffer
     :help "Execute the current buffer as Lisp code"]
    ["Interactive Expression Evaluation" ielm
     :help "Interactively evaluate Emacs Lisp expressions"]
    "---"
    ["Byte-compile This File" emacs-lisp-byte-compile
     :help "Byte compile the file containing the current buffer"]
    ["Byte-compile and Load" emacs-lisp-byte-compile-and-load
     :help "Byte-compile the current file (if it has changed), then load compiled code"]
    ["Byte-recompile Directory..." byte-recompile-directory
     :help "Recompile every `.el' file in DIRECTORY that needs recompilation"]
    ["Native-compile This File" emacs-lisp-native-compile
     :help "Compile this buffer's file to native code"
     :active (native-comp-available-p)]
    ["Native-compile and Load" emacs-lisp-native-compile-and-load
     :help "Compile this buffer's file to native code, then load compiled native code"
     :active (native-comp-available-p)]
    ["Disassemble Byte Compiled Object..." disassemble
     :help "Print disassembled code for OBJECT in a buffer"]
    "---"
    ["Instrument Function for Debugging" edebug-defun
     :help "Evaluate the top level form point is in, stepping through with Edebug"
     :keys "C-u C-M-x"]
    ("Navigation"
     ["Forward Sexp" forward-sexp
      :help "Go to the next s-expression"]
     ["Backward Sexp" backward-sexp
      :help "Go to the previous s-expression"]
     ["Beginning Of Defun" beginning-of-defun
      :help "Go to the start of the current function definition"]
     ["Up List" up-list
      :help "Go one level up and forward"])
    ("Profiling"
     ;; Maybe this should be in a separate submenu from the ELP stuff?
     ["Start Native Profiler..." profiler-start
      :help "Start recording profiling information"]
     ["Show Profiler Report" profiler-report
      :help "Show the current profiler report"
      :active (and (featurep 'profiler)
                   (profiler-running-p))]
     ["Stop Native Profiler" profiler-stop
      :help "Stop recording profiling information"
      :active (and (featurep 'profiler)
                   (profiler-running-p))]
     "---"
     ["Instrument Function..." elp-instrument-function
      :help "Instrument a function for profiling"]
     ["Instrument Package..." elp-instrument-package
      :help "Instrument for profiling all function that start with a prefix"]
     ["Show Profiling Results" elp-results
      :help "Display current profiling results"]
     ["Reset Counters for Function..." elp-reset-function
      :help "Reset the profiling information for a function"]
     ["Reset Counters for All Functions" elp-reset-all
      :help "Reset the profiling information for all functions being profiled"]
     "---"
     ["Remove Instrumentation for All Functions" elp-restore-all
      :help "Restore the original definitions of all functions being profiled"]
     ["Remove Instrumentation for Function..." elp-restore-function
      :help "Restore an instrumented function to its original definition"])
    ("Tracing"
     ["Trace Function..." trace-function
      :help "Trace the function given as an argument"]
     ["Trace Function Quietly..." trace-function-background
      :help "Trace the function with trace output going quietly to a buffer"]
     "---"
     ["Untrace All" untrace-all
      :help "Untrace all currently traced functions"]
     ["Untrace Function..." untrace-function
      :help "Untrace function, and possibly activate all remaining advice"])
    ["Construct Regexp" re-builder
     :help "Construct a regexp interactively"]
    ["Check Documentation Strings" checkdoc
     :help "Check documentation strings for style requirements"]
    ["Auto-Display Documentation Strings" eldoc-mode
     :help "Display the documentation string for the item under cursor"
     :style toggle
     :selected (bound-and-true-p eldoc-mode)]))

(defun elisp-context-menu (menu click)
  "Populate MENU with symbol help commands at CLICK."
  (when (thing-at-mouse click 'symbol)
    (define-key-after menu [elisp-separator] menu-bar-separator
      'middle-separator)

    (let* ((string (thing-at-mouse click 'symbol t))
           (symbol (when (stringp string) (intern string)))
           (title (cond
                   ((not (symbolp symbol)) nil)
                   ((and (facep symbol) (not (fboundp symbol)))
                    "Face")
                   ((and (fboundp symbol)
                         (not (or (boundp symbol) (facep symbol))))
                    "Function")
                   ((and (boundp symbol)
                         (not (or (fboundp symbol) (facep symbol))))
                    "Variable")
                   ((or (fboundp symbol) (boundp symbol) (facep symbol))
                    "Symbol"))))
      (when title
        (define-key-after menu [info-lookup-symbol]
          `(menu-item "Look up in Manual"
                      (lambda (_click) (interactive "e")
                        (info-lookup-symbol ',symbol))
                      :help ,(format "Find `%s' in relevant manual" symbol))
          'elisp-separator)
        (define-key-after menu [describe-symbol]
          `(menu-item (format "Describe %s" ,title)
                      (lambda (_click) (interactive "e")
                        (describe-symbol ',symbol))
                      :help ,(format "Display the documentation of `%s'" symbol))
          'elisp-separator))))
  menu)

(defun emacs-lisp-byte-compile ()
  "Byte-compile the current buffer's file."
  (interactive nil emacs-lisp-mode)
  (if buffer-file-name
      (byte-compile-file buffer-file-name)
    (error "The buffer must be saved in a file first")))

(defun emacs-lisp--before-compile-buffer ()
  "Make sure the buffer is saved before compiling."
  (or buffer-file-name
      (error "The buffer must be saved in a file first"))
  ;; Recompile if file or buffer has changed since last compilation.
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "Save buffer %s first? " (buffer-name))))
      (save-buffer)))

(defun emacs-lisp-byte-compile-and-load ()
  "Byte-compile the current file (if it has changed), then load compiled code."
  (interactive nil emacs-lisp-mode)
  (emacs-lisp--before-compile-buffer)
  (require 'bytecomp)
  (byte-recompile-file buffer-file-name nil 0)
  (load (byte-compile-dest-file buffer-file-name)))

(declare-function native-compile "comp")
(declare-function comp--write-bytecode-file "comp")

(defun emacs-lisp-native-compile ()
  "Native-compile the current buffer's file (if it has changed).
This invokes a synchronous native-compilation of the file that is
visited by the current buffer."
  (interactive nil emacs-lisp-mode)
  (emacs-lisp--before-compile-buffer)
  (let* ((byte+native-compile t)
         (byte-to-native-output-buffer-file nil)
         (eln (native-compile buffer-file-name)))
    (when eln
      (comp--write-bytecode-file eln))))

(defun emacs-lisp-native-compile-and-load ()
  "Native-compile the current buffer's file (if it has changed), then load it.
This invokes a synchronous native-compilation of the file that is
visited by the current buffer, then loads the compiled native code
when the compilation is finished.

Use `emacs-lisp-byte-compile-and-load' in combination with
`native-comp-jit-compilation' set to t to achieve asynchronous
native compilation of the current buffer's file."
  (interactive nil emacs-lisp-mode)
  (when-let* ((byte-file (emacs-lisp-native-compile)))
    (load (file-name-sans-extension byte-file))))

(defun emacs-lisp-macroexpand ()
  "Macroexpand the form after point.
Comments in the form will be lost."
  (interactive)
  (let* ((start (point))
         (exp (read (current-buffer)))
         ;; Compute it before, since it may signal errors.
         (new (macroexpand-1 exp)))
    (if (equal exp new)
        (message "Not a macro call, nothing to expand")
      (delete-region start (point))
      (pp new (current-buffer))
      (if (bolp) (delete-char -1))
      (indent-region start (point)))))

(defun elisp-mode-syntax-propertize (start end)
  (goto-char start)
  (let ((case-fold-search nil))
    (funcall
     (syntax-propertize-rules
      ;; Empty symbol.
      ("##" (0 (unless (nth 8 (syntax-ppss))
                 (string-to-syntax "_"))))
      ;; Prevent the @ from becoming part of a following symbol.
      (",@" (0 (unless (nth 8 (syntax-ppss))
                 (string-to-syntax "'"))))
      ;; Unicode character names.  (The longest name is 88 characters
      ;; long.)
      ("\\?\\\\N{[-A-Za-z0-9 ]\\{,100\\}}"
       (0 (unless (nth 8 (syntax-ppss))
            (string-to-syntax "_"))))
      ((rx "#" (or (seq (group-n 1 "&" (+ digit)) ?\") ; Bool-vector.
                   (seq (group-n 1 "s") "(")           ; Record.
                   (seq (group-n 1 (+ "^")) "[")))     ; Char-table.
       (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
            (string-to-syntax "'")))))
     start end)))

(defgroup elisp nil "Emacs Lisp editing support." :version "31.1" :group 'lisp)

(defcustom elisp-fontify-semantically nil
  "Whether to highlight symbols according to their semantic meaning.

If this is non-nil, `emacs-lisp-mode' uses code analysis to determine
the role of each symbol and highlight it accordingly.  We call this kind
of highlighting \"semantic highlighting\".

Semantic highlighting works best when you keep your code syntactically
correct while editing it, for example by using `electric-pair-mode'.

In trusted buffers (see `trusted-content-p'), the code analysis may
expand some macro calls in your code to analyze the expanded forms.  In
untrusted buffers, for security reasons, macro-expansion is restricted
to safe macros only (see `elisp-scope-safe-macro-p').  Hence in
untrusted buffers the arguments of some macros might not be analyzed,
and therefore will not be semantically highlighted.

See the function `elisp-scope-analyze-form' for more details about the
code analysis."
  :type 'boolean
  :version "31.1")

(defface elisp-symbol-at-mouse
  '((((background light)) :background "#fff6d8")
    (((background dark))  :background "#00422a"))
  "Face for highlighting the symbol at mouse in Emacs Lisp code."
  :version "31.1")

(defface elisp-free-variable
  '((t :underline t :foreground reset :inherit font-lock-variable-use-face))
  "Face for highlighting free (special) variables in Emacs Lisp code."
  :version "31.1")

(defface elisp-special-variable-declaration '((t :inherit elisp-free-variable))
  "Face for highlighting free variable declarations in Emacs Lisp code."
  :version "31.1")

(defface elisp-condition '((t :foreground "red"))
  "Face for highlighting `condition-case' conditions in Emacs Lisp code."
  :version "31.1")

(defface elisp-major-mode-name
  '((((background light)) :foreground "#006400")
    (((background dark))  :foreground "#4ade80"))
  "Face for highlighting major mode names in Emacs Lisp code.")

(defface elisp-face '((t :inherit font-lock-type-face))
  "Face for highlighting face names in Emacs Lisp code."
  :version "31.1")

(defface elisp-symbol-role
  '((default :inherit font-lock-function-call-face)
    (((background light)) :foreground "#00008b")
    (((background dark))  :foreground "#5c9cff"))
  "Face for highlighting symbol role names in Emacs Lisp code."
  :version "31.1")

(defface elisp-symbol-role-definition
  '((default :inherit font-lock-function-name-face)
    (((background light)) :foreground "#00008b")
    (((background dark))  :foreground "#5c9cff"))
  "Face for highlighting symbol role definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-function '((t :inherit font-lock-function-call-face))
  "Face for highlighting function calls in Emacs Lisp code."
  :version "31.1")

(defface elisp-non-local-exit '((t :inherit elisp-function :underline "red"))
  "Face for highlighting calls to functions that do not return."
  :version "31.1")

(defface elisp-unknown-call
  '((default :inherit elisp-function)
    (((background light)) :foreground "#2f4f4f")
    (((background dark))  :foreground "#7fa9a9"))
  "Face for highlighting unknown functions/macros in Emacs Lisp code."
  :version "31.1")

(defface elisp-macro '((t :inherit font-lock-keyword-face))
  "Face for highlighting macro calls in Emacs Lisp code."
  :version "31.1")

(defface elisp-special-form '((t :inherit elisp-macro))
  "Face for highlighting special forms in Emacs Lisp code."
  :version "31.1")

(defface elisp-throw-tag '((t :inherit font-lock-constant-face))
  "Face for highlighting `catch'/`throw' tags in Emacs Lisp code."
  :version "31.1")

(defface elisp-feature '((t :inherit font-lock-constant-face))
  "Face for highlighting feature names in Emacs Lisp code."
  :version "31.1")

(defface elisp-rx
  '((((background light)) :foreground "#00008b")
    (((background dark))  :foreground "#5c9cff"))
  "Face for highlighting `rx' constructs in Emacs Lisp code."
  :version "31.1")

(defface elisp-theme '((t :inherit font-lock-constant-face))
  "Face for highlighting custom theme names in Emacs Lisp code."
  :version "31.1")

(defface elisp-binding-variable
  '((t :slant italic :inherit font-lock-variable-name-face))
  "Face for highlighting binding occurrences of variables in Emacs Lisp code."
  :version "31.1")

(defface elisp-bound-variable
  '((t :slant italic :foreground reset :inherit font-lock-variable-use-face))
  "Face for highlighting bound occurrences of variables in Emacs Lisp code."
  :version "31.1")

(defface elisp-shadowing-variable
  '((t :inherit elisp-binding-variable :underline t))
  "Face for highlighting local bindings that shadow special variables."
  :version "31.1")

(defface elisp-shadowed-variable
  '((t :inherit elisp-bound-variable :underline t))
  "Face for highlighting special variables that are shadowed by a local binding."
  :version "31.1")

(defface elisp-variable-at-point '((t :inherit bold))
  "Face for highlighting (all occurrences of) the variable at point."
  :version "31.1")

(defface elisp-warning-type '((t :inherit font-lock-type-face))
  "Face for highlighting byte-compilation warning type names in Emacs Lisp."
  :version "31.1")

(defface elisp-function-property-declaration '((t :inherit font-lock-variable-use-face))
  "Face for highlighting function/macro property declaration type names."
  :version "31.1")

(defface elisp-thing '((t :inherit font-lock-type-face))
  "Face for highlighting `thing-at-point' \"thing\" names in Emacs Lisp."
  :version "31.1")

(defface elisp-slot '((t :inherit font-lock-builtin-face))
  "Face for highlighting EIEIO slot names."
  :version "31.1")

(defface elisp-widget-type '((t :inherit font-lock-type-face))
  "Face for highlighting widget type names in Emacs Lisp code."
  :version "31.1")

(defface elisp-type '((t :inherit font-lock-type-face))
  "Face for highlighting object type names in Emacs Lisp code."
  :version "31.1")

(defface elisp-group '((t :inherit font-lock-type-face))
  "Face for highlighting customization group names in Emacs Lisp code."
  :version "31.1")

(defface elisp-nnoo-backend '((t :inherit font-lock-type-face))
  "Face for highlighting `nnoo' backend names in Emacs Lisp code."
  :version "31.1")

(defface elisp-ampersand '((t :inherit font-lock-type-face))
  "Face for highlighting argument list markers, such as `&optional'."
  :version "31.1")

(defface elisp-constant '((t :inherit font-lock-builtin-face))
  "Face for highlighting self-evaluating symbols in Emacs Lisp code."
  :version "31.1")

(defface elisp-defun '((t :inherit font-lock-function-name-face))
  "Face for highlighting function definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-defmacro '((t :inherit elisp-defun))
  "Face for highlighting macro definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-defvar '((t :inherit font-lock-variable-name-face))
  "Face for highlighting variable definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-defface '((t :inherit font-lock-variable-name-face))
  "Face for highlighting face definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-icon '((t :inherit font-lock-type-face))
  "Face for highlighting icon names in Emacs Lisp code."
  :version "31.1")

(defface elisp-deficon '((t :inherit elisp-icon))
  "Face for highlighting icon definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-oclosure '((t :inherit font-lock-type-face))
  "Face for highlighting OClosure type names in Emacs Lisp code."
  :version "31.1")

(defface elisp-defoclosure '((t :inherit elisp-oclosure))
  "Face for highlighting OClosure type definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-coding '((t :inherit font-lock-type-face))
  "Face for highlighting coding system names in Emacs Lisp code."
  :version "31.1")

(defface elisp-defcoding '((t :inherit elisp-coding))
  "Face for highlighting coding system definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-charset '((t :inherit font-lock-type-face))
  "Face for highlighting charset names in Emacs Lisp code."
  :version "31.1")

(defface elisp-defcharset '((t :inherit elisp-charset))
  "Face for highlighting charset definitions in Emacs Lisp code."
  :version "31.1")

(defface elisp-completion-category '((t :inherit font-lock-type-face))
  "Face for highlighting completion category names in Emacs Lisp code."
  :version "31.1")

(defface elisp-completion-category-definition
  '((t :inherit elisp-completion-category))
  "Face for highlighting completion category definitions in Emacs Lisp code."
  :version "31.1")

(defun elisp-local-references (pos)
  "Return references to local variable at POS as (BEG . LEN) cons cells."
  (let (all cur)
    (save-excursion
      (goto-char pos)
      (beginning-of-defun)
      (elisp-scope-analyze-form
       (lambda (_role beg _sym id &optional _def)
         (let* ((end (progn (goto-char beg) (read (current-buffer)) (point)))
                (len (- end beg)))
           (when (<= beg pos end) (setq cur id))
           (when id (setf (alist-get beg all) (list len id)))))))
    (seq-keep
     (pcase-lambda (`(,beg ,len ,id)) (when (equal id cur) (cons beg len)))
     all)))

(defun elisp-highlight-variable (pos)
  "Highlight variable at POS along with its co-occurrences."
  (pcase-dolist (`(,beg . ,len) (elisp-local-references pos))
    (let ((ov (make-overlay beg (+ beg len))))
      (overlay-put ov 'face 'elisp-variable-at-point)
      (overlay-put ov 'elisp-highlight-variable t))))

(defun elisp-unhighlight-variable (pos)
  "Remove variable highlighting across top-level form at POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-defun)
    (remove-overlays (point) (progn (end-of-defun) (point))
                     'elisp-highlight-variable t)))

(defun elisp-cursor-sensor (pos)
  "Return `cursor-sensor-functions' for Emacs Lisp symbol at POS."
  (list
   (lambda (_win old dir)
     (cl-case dir
       (entered (elisp-highlight-variable pos))
       (left (elisp-unhighlight-variable old))))))

(defun elisp--function-help-echo (sym &rest _)
  (when (fboundp sym)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (insert "`" (symbol-name sym) "' is ")
        (describe-function-1 sym))
      (buffer-string))))

(defun elisp--help-echo (prop str sym &rest _)
  (if-let* ((doc (documentation-property sym prop t)))
      (format "%s `%S'.\n\n%s" str sym doc)
    str))

(defcustom elisp-add-help-echo t
  "Whether to add `help-echo' property to symbols while highlighting them.
This option has effect only if `elisp-fontify-semantically' is non-nil."
  :version "31.1"
  :type 'boolean)

(defun elisp--annotate-symbol-with-help-echo (role beg end sym)
  (put-text-property
   beg end 'help-echo
   (when-let* ((hlp (elisp-scope-get-symbol-role-property role :help)))
     ;; HLP is either a string, or a function that takes SYM as an
     ;; additional argument on top of the usual WINDOW, OBJECT and POS
     ;; that `help-echo' functions takes.
     (if (stringp hlp) hlp (apply-partially hlp sym)))))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun elisp-extend-region-to-whole-defuns ()
  (when elisp-fontify-semantically
    (let (changed)
      (when-let* ((new-beg (syntax-ppss-toplevel-pos (syntax-ppss font-lock-beg))))
        (setq font-lock-beg new-beg changed t))
      (when-let* ((beg-of-end (syntax-ppss-toplevel-pos (syntax-ppss font-lock-end)))
                  (new-end (ignore-error scan-error (scan-sexps beg-of-end 1))))
        (setq font-lock-end new-end changed t))
      changed)))

(defcustom elisp-fontify-symbol-precedence-function #'ignore
  "Function that determines the precedence of semantic highlighting.

The function takes two arguments, BEG and END, which are the beginning
and end positions in the current buffer of a symbol that is about to be
fontified during semantic highlighting.  The function is called after
`font-lock-keywords' were already applied.  If the function returns nil,
then semantic highlighting takes precedence, otherwise the highlighting
that `font-lock-keywords' applied takes precedence, if any.  By default,
semantic highlighting takes precedence."
  :type '(choice
          (function-item :tag "Prioritize semantic highlighting" ignore)
          (function-item :tag "Prioritize `font-lock-keywords'" always)
          (function :tag "Custom function"))
  :version "31.1")

(defun elisp-fontify-symbol (role beg sym id &optional _def)
  "Fontify symbol SYM starting at position BEG according to its ROLE.

If `elisp-add-help-echo' is non-nil, also annotate the symbol with the
`help-echo' text property.  If `cursor-sensor-mode' is enabled and ID is
non-nil, also annotate the symbol with `cursor-sensor-functions'."
  (let ((end (progn (goto-char beg) (read (current-buffer)) (point))))
    (let ((face (elisp-scope-get-symbol-role-property role :face)))
      (add-face-text-property
       beg end face
       (cl-case elisp-fontify-symbol-precedence-function
         (ignore nil)
         (always t)
         (otherwise (funcall elisp-fontify-symbol-precedence-function beg end))))
      (when elisp-add-help-echo
        (elisp--annotate-symbol-with-help-echo role beg end sym)
        (put-text-property beg end 'mouse-face `(,face elisp-symbol-at-mouse)))
      (when (and id (bound-and-true-p cursor-sensor-mode))
        (put-text-property beg (1+ end) 'cursor-sensor-functions
                           ;; Get a fresh list with SYM hardcoded,
                           ;; so that the value is distinguishable
                           ;; from the value in adjacent regions.
                           (elisp-cursor-sensor beg))))))

(defun elisp-fontify-symbols (end)
  "Fontify symbols from point to END according to their role in the code."
  (when elisp-fontify-semantically
    (while (< (point) end)
      (ignore-errors (elisp-scope-analyze-form #'elisp-fontify-symbol)))))

(defun elisp-outline-search (&optional bound move backward looking-at)
  "Don't use leading parens in strings for outline headings."
  (if looking-at
      (and (looking-at outline-regexp)
           (save-excursion (not (nth 8 (syntax-ppss (match-beginning 0))))))
    (let ((search-success nil))
      (while (and (setq search-success
                        (funcall (if backward #'re-search-backward
                                   #'re-search-forward)
                                 (concat "^\\(?:" outline-regexp "\\)")
                                 bound (if move 'move t)))
                  (save-excursion
                    (save-match-data
                      (nth 8 (syntax-ppss (match-beginning 0)))))))
      search-success)))

(defcustom emacs-lisp-mode-hook nil
  "Hook run when entering Emacs Lisp mode."
  :options '(eldoc-mode imenu-add-menubar-index checkdoc-minor-mode)
  :type 'hook
  :group 'lisp)

(defun emacs-lisp-set-electric-text-pairs ()
  "Set `electric-pair-text-pairs' for all `emacs-lisp-mode' buffers."
  (defvar electric-pair-text-pairs)
  (let ((elisp-pairs (append '((?\` . ?\') (?‘ . ?’))
                             electric-pair-text-pairs)))
    (save-current-buffer
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (derived-mode-p 'emacs-lisp-mode)
          (setq-local electric-pair-text-pairs elisp-pairs)))))
  (remove-hook 'electric-pair-mode-hook #'emacs-lisp-set-electric-text-pairs))

(defun elisp-enable-lexical-binding (&optional interactive)
  "Make the current buffer use `lexical-binding'.
With a prefix argument \\[universal-argument], make the buffer use
dynamic binding instead.
In addition to setting the value of `lexical-binding' in the buffer,
this function adds the lexbind cookie to the first line of the buffer,
if it is not already there, so that saving the buffer to its file
will cause Emacs to use the specified value of `lexical-binding'
when the file is loaded henceforth.
INTERACTIVE non-nil means ask the user for confirmation; this
happens in interactive invocations.
When calling from Lisp, use nil or a positive number as the value
of INTERACTIVE to enable `lexical-binding', a negative number to
disable it."
  (interactive "p")
  (let* ((disable-lexbind (or (and (numberp interactive)
                                   (< interactive 0))
                              (if current-prefix-arg t)))
         (required-value (not disable-lexbind)))
    (if (and (local-variable-p 'lexical-binding)
             (null (xor required-value lexical-binding)))
        (when interactive
          (message "lexical-binding already %s!"
                   (if disable-lexbind "disabled" "enabled"))
          (ding))
      (when (or (not interactive)
                (y-or-n-p (format "%s lexical-binding in this %s? "
                                  (if disable-lexbind "Disable" "Enable")
                                  (if buffer-file-name "file" "buffer"))))
        (setq-local lexical-binding required-value)
        (add-file-local-variable-prop-line 'lexical-binding required-value
                                           interactive)))))

(defvar-keymap elisp--dynlex-modeline-map
  "<mode-line> <mouse-1>" #'elisp-enable-lexical-binding)

(defconst elisp-semantic-font-lock-keywords
  (append lisp-el-font-lock-keywords-2 '((elisp-fontify-symbols))))

;;;###autoload
(define-derived-mode emacs-lisp-mode lisp-data-mode
  `("Elisp"
    (lexical-binding (:propertize "/l"
                      help-echo "Using lexical-binding mode")
                     (:propertize "/d"
                      help-echo "Using old dynamic scoping mode\n\
mouse-1: Enable lexical-binding mode"
		      face warning
		      mouse-face mode-line-highlight
                      local-map ,elisp--dynlex-modeline-map)))
  "Major mode for editing Lisp code to run in Emacs.
\\<emacs-lisp-mode-map>
- \\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
- Blank lines separate paragraphs.
- Semicolons start comments.

When editing Lisp data (as opposed to code), `lisp-data-mode' can
be used instead.

\\{emacs-lisp-mode-map}"
  :group 'lisp
  (defvar project-vc-external-roots-function)
  (setcar font-lock-defaults
          '(lisp-el-font-lock-keywords
            lisp-el-font-lock-keywords-1
            lisp-el-font-lock-keywords-2
            elisp-semantic-font-lock-keywords))
  (dolist (prop '(cursor-sensor-functions help-echo mouse-face))
    (cl-pushnew prop
                (alist-get 'font-lock-extra-managed-props
                           (nthcdr 5 font-lock-defaults))))
  (setf (nth 2 font-lock-defaults) nil)
  (add-hook 'font-lock-extend-region-functions
            #'elisp-extend-region-to-whole-defuns nil t)
  (add-hook 'after-load-functions #'elisp--font-lock-flush-elisp-buffers)
  (if (boundp 'electric-pair-text-pairs)
      (setq-local electric-pair-text-pairs
                  (append '((?\` . ?\') (?\‘ . ?\’))
                          electric-pair-text-pairs))
    (add-hook 'electric-pair-mode-hook #'emacs-lisp-set-electric-text-pairs))
  (add-hook 'eldoc-documentation-functions
            #'elisp-eldoc-funcall nil t)
  (add-hook 'eldoc-documentation-functions
            #'elisp-eldoc-var-docstring nil t)
  (add-hook 'xref-backend-functions #'elisp--xref-backend nil t)
  (setq-local project-vc-external-roots-function #'elisp-load-path-roots)
  (setq-local syntax-propertize-function #'elisp-mode-syntax-propertize)
  (setq-local outline-search-function #'elisp-outline-search)
  (add-hook 'completion-at-point-functions
            #'elisp-completion-at-point nil 'local)
  (add-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc nil t)
  (add-hook 'flymake-diagnostic-functions
            #'elisp-flymake-byte-compile nil t)
  (add-hook 'context-menu-functions #'elisp-context-menu 10 t))

;; Font-locking support.

(defun elisp--font-lock-shorthand (_limit)
  ;; Add faces on shorthands between point and LIMIT.
  ;; ...
  ;; Return nil to tell font-lock, that there's nothing left to do.
  nil)

(defun elisp--font-lock-flush-elisp-buffers (&optional file)
  ;; We're only ever called from after-load-functions, load-in-progress can
  ;; still be t in case of nested loads.
  (when (or (not load-in-progress) file)
    ;; FIXME: If the loaded file did not define any macros, there shouldn't
    ;; be any need to font-lock-flush all the Elisp buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (derived-mode-p 'emacs-lisp-mode)
          ;; So as to take into account new macros that may have been defined
          ;; by the just-loaded file.
	  (font-lock-flush))))))

;;; Completion at point for Elisp

(defun elisp--local-variables-1 (vars sexp)
  "Return VARS locally bound around the witness, or nil if not found."
  (let (res)
    (while
        (unless
            (setq res
                  (pcase sexp
                    (`(,(or 'let 'let*) ,bindings)
                     (let ((vars vars))
                       (when (eq 'let* (car sexp))
                         (dolist (binding (cdr (reverse bindings)))
                           (push (or (car-safe binding) binding) vars)))
                       (elisp--local-variables-1
                        vars (car (cdr-safe (car (last bindings)))))))
                    (`(,(or 'let 'let*) ,bindings . ,body)
                     (let ((vars vars))
                       (dolist (binding bindings)
                         (push (or (car-safe binding) binding) vars))
                       (elisp--local-variables-1 vars (car (last body)))))
                    (`(lambda ,_args)
                     ;; FIXME: Look for the witness inside `args'.
                     (setq sexp nil))
                    (`(lambda ,args . ,body)
                     (elisp--local-variables-1
                      (let ((args (if (listp args) args)))
                        ;; FIXME: Exit the loop if witness is in args.
                        (append (remq '&optional (remq '&rest args)) vars))
                      (car (last body))))
                    (`(condition-case ,_ ,e) (elisp--local-variables-1 vars e))
                    (`(condition-case ,v ,_ . ,catches)
                     (elisp--local-variables-1
                      (cons v vars) (cdr (car (last catches)))))
                    (`(quote . ,_)
                     ;; FIXME: Look for the witness inside sexp.
                     (setq sexp nil))
                    ;; FIXME: Handle `cond'.
                    (`(,_ . ,_)
                     (elisp--local-variables-1 vars (car (last sexp))))
                    ('elisp--witness--lisp (or vars '(nil)))
                    (_ nil)))
          ;; We didn't find the witness in the last element so we try to
          ;; backtrack to the last-but-one.
          (setq sexp (ignore-errors (butlast sexp)))))
    res))

(defvar warning-minimum-log-level)

(defvar elisp--local-macroenv
  `((cl-eval-when . ,(lambda (&rest args) `(progn . ,(cdr args))))
    (eval-when-compile . ,(lambda (&rest args) `(progn . ,args)))
    (eval-and-compile . ,(lambda (&rest args) `(progn . ,args))))
  "Environment to use while tentatively expanding macros.
This is used to try and avoid the most egregious problems linked to the
use of `macroexpand-all' as a way to find the \"underlying raw code\".")

(defvar elisp--macroexpand-untrusted-warning t)

(defun elisp--safe-macroexpand-all (sexp)
  (if (not (trusted-content-p))
      ;; FIXME: We should try and do better here, either using a notion
      ;; of "safe" macros, or with `bwrap', or ...
      (progn
        (when elisp--macroexpand-untrusted-warning
          (setq-local elisp--macroexpand-untrusted-warning nil) ;Don't spam!
          (let ((inhibit-message t))      ;Only log.
            (message "Completion of local vars is disabled in %s (untrusted content)"
                     (buffer-name))))
        sexp)
    (let ((macroexpand-advice
           (lambda (expander form &rest args)
             (condition-case err
                 (apply expander form args)
               (error
                (message "Ignoring macroexpansion error: %S" err) form)))))
      (unwind-protect
          ;; Silence any macro expansion errors when
          ;; attempting completion at point (bug#58148).
          (let ((inhibit-message t)
                (macroexp-inhibit-compiler-macros t)
                (warning-minimum-log-level :emergency))
            (advice-add 'macroexpand-1 :around macroexpand-advice)
            (macroexpand-all sexp elisp--local-macroenv))
        (advice-remove 'macroexpand-1 macroexpand-advice)))))

(defun elisp--local-variables ()
  "Return a list of locally let-bound variables at point."
  (save-excursion
    (skip-syntax-backward "w_")
    (let* ((ppss (syntax-ppss))
           (txt (buffer-substring-no-properties (or (car (nth 9 ppss)) (point))
                                                (or (nth 8 ppss) (point))))
           (closer ()))
      (dolist (p (nth 9 ppss))
        (push (cdr (syntax-after p)) closer))
      (setq closer (apply #'string closer))
      (let* ((sexp (condition-case nil
                       (car (read-from-string
                             (concat txt "elisp--witness--lisp" closer)))
                     ((invalid-read-syntax end-of-file) nil)))
             (vars (elisp--local-variables-1
                    nil (elisp--safe-macroexpand-all sexp))))
        (delq nil
              (mapcar (lambda (var)
                        (and (symbolp var)
                             (not (string-match (symbol-name var) "\\`[&_]"))
                             ;; Eliminate uninterned vars.
                             (intern-soft var)
                             var))
                      vars))))))

(defconst elisp--local-variables-completion-table
  (let ((lastpos nil) (lastvars nil))
    (letrec ((hookfun (lambda ()
                        (setq lastpos nil)
                        (remove-hook 'post-command-hook hookfun))))
      (completion-table-dynamic
       (lambda (_string)
         (save-excursion
           (skip-syntax-backward "_w")
           (let ((newpos (cons (point) (current-buffer))))
             (unless (equal lastpos newpos)
               (add-hook 'post-command-hook hookfun)
               (setq lastpos newpos)
               (setq lastvars
                     (mapcar #'symbol-name (elisp--local-variables))))))
         lastvars)))))

(defun elisp--expect-function-p (pos)
  "Return non-nil if the symbol at position POS is expected to be a function."
  (or
   (and (eq (char-before pos) ?')
        (eq (char-before (1- pos)) ?#))
   (save-excursion
     (let ((parent (nth 1 (syntax-ppss pos))))
       (when parent
         (goto-char parent)
         (and
          (looking-at (concat "(\\(cl-\\)?"
                              (regexp-opt '("declare-function"
                                            "function" "defadvice"
                                            "callf" "callf2"
                                            "defsetf"))
                              "[ \t\r\n]+"))
          (eq (match-end 0) pos)))))))

(defun elisp--form-quoted-p (pos)
  "Return non-nil if the form at POS is not evaluated.
It can be quoted, or be inside a quoted form."
  ;; FIXME: Do some macro expansion maybe.
  (save-excursion
    (let ((state (syntax-ppss pos)))
      (or (nth 8 state)   ; Code inside strings usually isn't evaluated.
          ;; FIXME: The 9th element is undocumented.
          (let ((nesting (cons (point) (reverse (nth 9 state))))
                res)
            (while (and nesting (not res))
              (goto-char (pop nesting))
              (cond
               ((or (eq (char-after) ?\[)
                    (progn
                      (skip-chars-backward " ")
                      (memq (char-before) '(?' ?` ?‘))))
                (setq res t))
               ((eq (char-before) ?,)
                (setq nesting nil))))
            res)))))

;; FIXME: Support for Company brings in features which straddle eldoc.
;; We should consolidate this, so that major modes can provide all that
;; data all at once:
;; - a function to extract "the reference at point" (may be more complex
;;     than a mere string, to distinguish various namespaces).
;; - a function to jump to such a reference.
;; - a function to show the signature/interface of such a reference.
;; - a function to build a help-buffer about that reference.
;; FIXME: Those functions should also be used by the normal completion code in
;; the *Completions* buffer.

(defun elisp--company-doc-buffer (str)
  (let ((symbol (intern-soft str)))
    ;; FIXME: we really don't want to "display-buffer and then undo it".
    (save-window-excursion
      ;; Make sure we don't display it in another frame, otherwise
      ;; save-window-excursion won't be able to undo it.
      (let ((display-buffer-overriding-action
             '(nil . ((inhibit-switch-frame . t)))))
        (ignore-errors
          (cond
           ((fboundp symbol) (describe-function symbol))
           ((boundp symbol) (describe-variable symbol))
           ((featurep symbol) (describe-package symbol))
           ((facep symbol) (describe-face symbol))
           (t (signal 'user-error nil)))
          (help-buffer))))))

(defun elisp--company-doc-string (str)
  (let* ((symbol (intern-soft str))
         (doc (if (fboundp symbol)
                  (documentation symbol t)
                (documentation-property symbol 'variable-documentation t))))
    (and (stringp doc)
         (string-match ".*$" doc)
         (match-string 0 doc))))

;; can't (require 'find-func) in a preloaded file
(declare-function find-library-name "find-func" (library))
(declare-function find-function-library "find-func" (function &optional l-o v))

(defun elisp--company-location (str)
  (let ((sym (intern-soft str)))
    (cond
     ((fboundp sym) (find-definition-noselect sym nil))
     ((boundp sym) (find-definition-noselect sym 'defvar))
     ((featurep sym)
      (require 'find-func)
      (cons (find-file-noselect (find-library-name
                                 (symbol-name sym)))
            0))
     ((facep sym) (find-definition-noselect sym 'defface)))))

(defvar obarray-cache nil
  "If non-nil, a hash table of cached obarray-related information.
The cache holds information specific to the current state of the
Elisp obarray.  If the obarray is modified by any means (such as
interning or uninterning a symbol), this variable is set to nil.")

(defun elisp--completion-local-symbols ()
  "Compute collections of all Elisp symbols for completion purposes.
The return value is compatible with the COLLECTION form described
in `completion-at-point-functions' (which see)."
  (cl-flet ((obarray-plus-shorthands ()
              (let (retval)
                (mapatoms
                 (lambda (s)
                   (push s retval)
                   (cl-loop
                    for (shorthand . longhand) in read-symbol-shorthands
                    for full-name = (symbol-name s)
                    when (string-prefix-p longhand full-name)
                    do (let ((sym (make-symbol
                                   (concat shorthand
                                           (substring full-name
                                                      (length longhand))))))
                         (put sym 'shorthand t)
                         (push sym retval)
                         retval))))
                retval)))
    (cond ((null read-symbol-shorthands) obarray)
          ((and obarray-cache
                (gethash (cons (current-buffer) read-symbol-shorthands)
                         obarray-cache)))
          (obarray-cache
            (puthash (cons (current-buffer) read-symbol-shorthands)
                     (obarray-plus-shorthands)
                     obarray-cache))
          (t
            (setq obarray-cache (make-hash-table :test #'equal))
            (puthash (cons (current-buffer) read-symbol-shorthands)
                     (obarray-plus-shorthands)
                     obarray-cache)))))

(defun elisp--shorthand-aware-fboundp (sym)
  (fboundp (intern-soft (symbol-name sym))))

(defun elisp--shorthand-aware-boundp (sym)
  (boundp (intern-soft (symbol-name sym))))

(defun elisp-completion-at-point ()
  "Function used for `completion-at-point-functions' in `emacs-lisp-mode'.
If the context at point allows only a certain category of
symbols (e.g. functions, or variables) then the returned
completions are restricted to that category.  In contexts where
any symbol is possible (following a quote, for example),
functions are annotated with \"<f>\" via the
`:annotation-function' property."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let* ((pos (point))
	   (beg (condition-case nil
		    (save-excursion
		      (backward-sexp 1)
		      (skip-chars-forward "`',‘#")
		      (min (point) pos))
		  (scan-error pos)))
	   (end
	    (cond
	     ((and (< beg (point-max))
		   (memq (char-syntax (char-after beg))
		                   '(?w ?\\ ?_)))
	      (condition-case nil
		  (save-excursion
		    (goto-char beg)
		    (forward-sexp 1)
                    (skip-chars-backward "'’")
		    (when (>= (point) pos)
		      (point)))
		(scan-error pos)))
             ((or (>= beg (point-max))
                  (memq (char-syntax (char-after beg))
		        '(?\) ?\s)))
              beg)))
           ;; t if in function position.
           (funpos (eq (char-before beg) ?\())
           (quoted (elisp--form-quoted-p beg))
           (is-ignore-error
            (condition-case nil
                (save-excursion
                  (up-list -1)
                  (forward-char 1)
                  (looking-at-p "ignore-error\\>"))
              (error nil))))
      (when (and end (or (not (nth 8 (syntax-ppss)))
                         (memq (char-before beg) '(?` ?‘))))
        (let ((table-etc
               (if (or (not funpos) quoted)
                   (cond
                    ;; FIXME: We could look at the first element of
                    ;; the current form and use it to provide a more
                    ;; specific completion table in more cases.
                    (is-ignore-error
                     (list t (elisp--completion-local-symbols)
                           :predicate (lambda (sym)
                                        (get sym 'error-conditions))))
                    ((elisp--expect-function-p beg)
                     (list nil (elisp--completion-local-symbols)
                           :predicate
                           #'elisp--shorthand-aware-fboundp
                           :company-kind #'elisp--company-kind
                           :company-doc-buffer #'elisp--company-doc-buffer
                           :company-docsig #'elisp--company-doc-string
                           :company-location #'elisp--company-location
                           :company-deprecated #'elisp--company-deprecated))
                    (quoted
                     (list nil (elisp--completion-local-symbols)
                           ;; Don't include all symbols (bug#16646).
                           :predicate (lambda (sym)
                                        ;; shorthand-aware
                                        (let ((sym (intern-soft (symbol-name sym))))
                                          (or (boundp sym)
                                              (fboundp sym)
                                              (featurep sym)
                                              (symbol-plist sym))))
                           :annotation-function
                           (lambda (str) (if (fboundp (intern-soft str)) " <f>"))
                           :company-kind #'elisp--company-kind
                           :company-doc-buffer #'elisp--company-doc-buffer
                           :company-docsig #'elisp--company-doc-string
                           :company-location #'elisp--company-location
                           :company-deprecated #'elisp--company-deprecated))
                    (t
                     (list nil (completion-table-merge
                                elisp--local-variables-completion-table
                                (apply-partially #'completion-table-with-predicate
                                                 (elisp--completion-local-symbols)
                                                 #'elisp--shorthand-aware-boundp
                                                 'strict))
                           :company-kind
                           (lambda (s)
                             (if (test-completion s elisp--local-variables-completion-table)
                                 'value
                               'variable))
                           :company-doc-buffer #'elisp--company-doc-buffer
                           :company-docsig #'elisp--company-doc-string
                           :company-location #'elisp--company-location
                           :company-deprecated #'elisp--company-deprecated)))
                 ;; Looks like a funcall position.  Let's double check.
                 (save-excursion
                   (goto-char (1- beg))
                   (let ((parent
                          (condition-case nil
                              (progn (up-list -1) (forward-char 1)
                                     (let ((c (char-after)))
                                       (if (eq c ?\() ?\(
                                         (if (memq (char-syntax c) '(?w ?_))
                                             (let ((pt (point)))
                                               (forward-sexp)
                                               (intern-soft
                                                (buffer-substring pt (point))))))))
                            (error nil))))
                     (pcase parent
                       ;; FIXME: Rather than hardcode special cases here,
                       ;; we should use something like a symbol-property.
                       ('declare
                        (list t (mapcar (lambda (x) (symbol-name (car x)))
                                        (delete-dups
                                         ;; FIXME: We should include some
                                         ;; docstring with each entry.
                                         (append macro-declarations-alist
                                                 defun-declarations-alist
                                                 nil))))) ; Copy both alists.
                       ((and (or 'condition-case 'condition-case-unless-debug)
                             (guard (save-excursion
                                      (ignore-errors
                                        (forward-sexp 2)
                                        (< (point) beg)))))
                        (list t (elisp--completion-local-symbols)
                              :predicate (lambda (sym) (get sym 'error-conditions))))
                       ;; `ignore-error' with a list CONDITION parameter.
                       ('ignore-error
                        (list t (elisp--completion-local-symbols)
                              :predicate (lambda (sym)
                                           (get sym 'error-conditions))))
                       ((and (or ?\( 'let 'let* 'cond 'cond* 'bind*)
                             (guard (save-excursion
                                      (goto-char (1- beg))
                                      (when (eq parent ?\()
                                        (up-list -1))
                                      (skip-syntax-backward " w_")
                                      (or
                                       (looking-at
                                        "\\_<\\(let\\*?\\|bind\\*\\)\\_>")
                                       (and (not (eq parent ?\())
                                            (looking-at
                                             "\\_<cond\\*?\\_>"))))))
                        (list t (elisp--completion-local-symbols)
                              :predicate #'elisp--shorthand-aware-boundp
                              :company-kind (lambda (_) 'variable)
                              :company-doc-buffer #'elisp--company-doc-buffer
                              :company-docsig #'elisp--company-doc-string
                              :company-location #'elisp--company-location
                              :company-deprecated #'elisp--company-deprecated))
                       (_ (list nil (elisp--completion-local-symbols)
                                :predicate #'elisp--shorthand-aware-fboundp
                                :company-kind #'elisp--company-kind
                                :company-doc-buffer #'elisp--company-doc-buffer
                                :company-docsig #'elisp--company-doc-string
                                :company-location #'elisp--company-location
                                :company-deprecated #'elisp--company-deprecated
                                ))))))))
          (nconc (list beg end)
                 (if (null (car table-etc))
                     (cdr table-etc)
                   (cons
                    (if (memq (char-syntax (or (char-after end) ?\s))
                              '(?\s ?>))
                        (cadr table-etc)
                      (apply-partially 'completion-table-with-terminator
                                       " " (cadr table-etc)))
                    (cddr table-etc)))))))))

(defun elisp--company-kind (str)
  (let ((sym (intern-soft str)))
    (cond
     ((or (macrop sym) (special-form-p sym)) 'keyword)
     ((fboundp sym) 'function)
     ((boundp sym) 'variable)
     ((featurep sym) 'module)
     ((facep sym) 'color)
     (t 'text))))

(defun elisp--company-deprecated (str)
  (let ((sym (intern-soft str)))
    (or (get sym 'byte-obsolete-variable)
        (get sym 'byte-obsolete-info))))

(defun lisp-completion-at-point (&optional _predicate)
  (declare (obsolete elisp-completion-at-point "25.1"))
  (elisp-completion-at-point))

;;; Xref backend

(declare-function xref-make "progmodes/xref" (summary location))
(declare-function xref-item-location "progmodes/xref" (this))

(defun elisp--xref-backend () 'elisp)

(defconst elisp--xref-format
  #("(%S %S)"
    1 3 (face font-lock-keyword-face)
    4 6 (face font-lock-function-name-face)))

(defconst elisp--xref-format-extra
  #("(%S %S %S)"
    1 3 (face font-lock-keyword-face)
    4 6 (face font-lock-function-name-face)))

(defvar find-feature-regexp);; in find-func.el

(defun elisp--xref-make-xref (type symbol file &optional summary)
  "Return an xref for TYPE SYMBOL in FILE.
TYPE must be a type in `find-function-regexp-alist' (use nil for
`defun').  If SUMMARY is non-nil, use it for the summary;
otherwise build the summary from TYPE and SYMBOL."
  (xref-make (or summary
		 (format elisp--xref-format (or type 'defun) symbol))
	     (xref-make-elisp-location symbol type file)))

(defvar elisp-xref-find-def-functions nil
  "List of functions run from `elisp--xref-find-definitions' to add more xrefs.
Called with one arg; the symbol whose definition is desired.
Each function should return a list of xrefs, or nil; the first
non-nil result supersedes the xrefs produced by
`elisp--xref-find-definitions'.")

(defun elisp--xref-list-index ()
  "Return the list index of the form at point, moving to the start.
If the buffer start was reached, return nil."
  (let ((i 0))
    (while (condition-case nil
               (let ((pt (point)))
                 (backward-sexp)
                 (< (point) pt))
             (scan-error nil))
      (setq i (1+ i)))
    (and (not (bobp)) i)))

(defun elisp--xref-infer-namespace (pos)
  "Find the likely namespace of the identifier at POS.
Return one of `function', `variable' `maybe-variable', `feature', `face', or
`any' (indicating any namespace).  `maybe-variable' indicates a variable
namespace but with lower confidence."
  (save-excursion
    (goto-char pos)
    (cl-flet ((looking-at-sym ()
                (let ((val (save-excursion
                             (ignore-errors (read (current-buffer))))))
                  (and (symbolp val) val))))
      (cond
       ((and (eq (char-before pos) ?\')
             (eq (char-before (1- pos)) ?#))
        ;; #'IDENT
        'function)
       ((memq (char-before pos) '(?\' ?`))
        ;; 'IDENT or `IDENT -- try to disambiguate.
        (backward-char)                 ; Step over '
        (let ((i (elisp--xref-list-index))
              (sym (looking-at-sym)))
          (cond
           ((eql i 1)
            (cond
             ((memq sym '( featurep require provide))
              'feature)
             ((memq sym
                    '(
                      ;; We are mostly interested in functions that take a
                      ;; function symbol as argument:
                      fboundp symbol-function fset
                      ;; ... but we include some common higher-order functions
                      ;; as well, even though the argument really should
                      ;; be #'-quoted:
                      function-get function-put
                      func-arity functionp
                      funcall funcall-interactively
                      apply mapcar mapc mapcan mapconcat
                      apply-partially
                      substitute-key-definition))
              'function)
             ((memq sym
                    '(
                      ;; Functions taking a variable symbol as first argument.
                      ;; More of these could be added for greater precision.
                      boundp set symbol-value
                      special-variable-p local-variable-p
                      local-variable-if-set-p
                      make-variable-buffer-local
                      default-value set-default make-local-variable
                      buffer-local-value))
              'variable)
             ((memq sym
                    '(
                      ;; FIXME: Add more functions taking a face
                      ;; symbol for greater precision.
                      facep face-name face-id))
              'face)
             (t 'any)))
           ((and (eql i 2)
                 (memq sym '( global-set-key local-set-key
                              substitute-key-definition
                              add-hook)))
            'function)
           ((and (eql i 3)
                 (memq sym '( define-key add-function)))
            'function)
           (t 'any))))
       ((or (and (eq (char-before (1- pos)) ?,)
                 (eq (char-before pos) ?@))
            (eq (char-before pos) ?,))
        ;; ,IDENT or ,@IDENT
        'variable)
       (t
        ;; Unquoted name -- look at the context.  General scheme:
        ;; (K-HEAD ... (J-HEAD ... (I-HEAD ... IDENT
        ;;             ^ index K   ^ index J   ^ index I
        (let* ((i (elisp--xref-list-index))
               (i-head (looking-at-sym))
               (i-paren (and i (eq (char-before) ?\()
                             (progn (backward-char) t)))
               (i-quoted (and i-paren (memq (char-before) '(?\' ?`))))
               (j (and i-paren (elisp--xref-list-index)))
               (j-head (and j (looking-at-sym)))
               (j-paren (and j (eq (char-before) ?\()
                             (progn (backward-char) t)))
               (j-quoted (and j-paren (memq (char-before) '(?\' ?`))))
               (k (and j-paren (elisp--xref-list-index)))
               (k-head (and k (looking-at-sym)))
               (k-paren (and k (eq (char-before) ?\()
                             (progn (backward-char) t)))
               (k-quoted (and k-paren (memq (char-before) '(?\' ?`)))))
          (cond
           ((or i-quoted j-quoted k-quoted)
            ;; '(... IDENT or '(... (... IDENT or '(... (... (... IDENT
            'any)
           ((and (eql j 1)
                 (memq j-head '( let let* letrec dlet lambda)))
            ;; (let (... IDENT
            'variable)
           ((and (eql j 2)
                 (memq j-head '( defun defmacro defsubst
                                 define-inline declare-function
                                 defadvice
                                 cl-defmethod cl-defgeneric)))
            ;; (defun FUNC (... IDENT
            'variable)
           ((and (eql j 2)
                 (eq j-head 'defclass))
            ;; (defclass CLASS (... IDENT
            'function)
           ((eq j-head 'cond)
            ;; (cond ... (... IDENT
            'variable)
           ((and (eql k 1)
                 (memq k-head '( let let* letrec dlet )))
            ;; (let (... (... IDENT
            'variable)
           ((eql i 0)
            ;; (IDENT ...
            'function)
           ((functionp i-head)
            ;; (FUNC ... IDENT
            'variable)
           ((and (eql i 1)
                 (cond
                  ((memq i-head '( function
                                   defun defmacro defsubst
                                   define-inline declare-function
                                   defadvice
                                   cl-defmethod cl-defgeneric))
                   'function)
                  ((memq i-head '( defvar defvar-local defconst defcustom))
                   'variable)
                  ((eq i-head 'defface)
                   'face))))
           ((memq i-head '( if while and or when unless progn prog1
                            let let* lambda defun defsubst defvar defconst))
            ;; arg to some common non-function forms
            'variable)
           ;; Anything else: probably a variable, but since i-head may be
           ;; a macro we cannot be sure.
           (t 'maybe-variable))))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'elisp)))
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (and bounds
         (let ((ident (buffer-substring-no-properties
                       (car bounds) (cdr bounds))))
           ;; Use a property to transport the location of the identifier.
           (propertize ident 'pos (car bounds))))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'elisp)) identifier)
  (require 'find-func)
  (let ((sym (intern-soft identifier)))
    (when sym
      (let* ((pos (get-text-property 0 'pos identifier))
             (namespace (if (and pos
                                 ;; Reusing it in Help Mode.
                                 (derived-mode-p 'emacs-lisp-mode))
                            (elisp--xref-infer-namespace pos)
                          'any))
             (defs (elisp--xref-find-definitions sym)))
        (if (eq namespace 'maybe-variable)
            (or (elisp--xref-filter-definitions defs 'variable sym)
                (elisp--xref-filter-definitions defs 'any sym))
          (elisp--xref-filter-definitions defs namespace sym))))))

(defun elisp--xref-filter-definitions (definitions namespace symbol)
  (if (eq namespace 'any)
      (if (memq symbol minor-mode-list)
          ;; The symbol is a minor mode. These should be defined by
          ;; "define-minor-mode", which means the variable and the
          ;; function are declared in the same place. So we return only
          ;; the function, arbitrarily.
          ;;
          ;; There is an exception, when the variable is defined in C
          ;; code, as for abbrev-mode.
          (cl-loop for d in definitions
                   for loc = (xref-item-location d)
                   for file = (xref-elisp-location-file loc)
                   when (or (not (eq (xref-elisp-location-type loc) 'defvar))
                            (null file)
                            (string-prefix-p "src/" file))
                   collect d)
        definitions)
    (let ((expected-types
           (pcase-exhaustive namespace
             ('function '( nil defalias define-type
                           cl-defgeneric cl-defmethod))
             ('variable '(defvar))
             ('face '(defface))
             ('feature '(feature)))))
      (cl-loop for d in definitions
               when (memq
                     (xref-elisp-location-type (xref-item-location d))
                     expected-types)
               collect d))))

(defun elisp--xref-find-definitions (symbol)
  ;; The file name is not known when `symbol' is defined via interactive eval.
  (let (xrefs)
    (let ((temp elisp-xref-find-def-functions))
      (while (and (null xrefs)
                  temp)
        (setq xrefs (append xrefs (funcall (pop temp) symbol)))))

    (unless xrefs
      ;; alphabetical by result type symbol

      ;; FIXME: advised function; list of advice functions
      ;; FIXME: aliased variable

      ;; Coding system symbols do not appear in ‘load-history’,
      ;; so we can’t get a location for them.

      (when (and (symbolp symbol)
                 (symbol-function symbol)
                 (symbolp (symbol-function symbol)))
        ;; aliased function
        (let* ((alias-symbol symbol)
               (alias-file (symbol-file alias-symbol))
               (real-symbol  (symbol-function symbol))
               (real-file (find-lisp-object-file-name real-symbol 'defun)))

          (when real-file
            (push (elisp--xref-make-xref nil real-symbol real-file) xrefs))

          (when alias-file
            (push (elisp--xref-make-xref 'defalias alias-symbol alias-file) xrefs))))

      (when (facep symbol)
        (let ((file (find-lisp-object-file-name symbol 'defface)))
          (when file
            (push (elisp--xref-make-xref 'defface symbol file) xrefs))))

      (when (fboundp symbol)
        (let ((file (find-lisp-object-file-name symbol (symbol-function symbol)))
              generic doc)
          (when file
            (cond
             ((eq file 'C-source)
              ;; First call to find-lisp-object-file-name for an object
              ;; defined in C; the doc strings from the C source have
              ;; not been loaded yet.  Second call will return "src/*.c"
              ;; in file; handled by t case below.
              (push (elisp--xref-make-xref
                     nil symbol (help-C-file-name (symbol-function symbol)
                                                  'subr))
                    xrefs))

             ((and (setq doc (documentation symbol t))
                   ;; This doc string is defined in cl-macs.el cl-defstruct
                   ;; FIXME: This is hideously brittle!
                   (string-match "Constructor for objects of type `\\(.*\\)'"
                                 doc))
              ;; `symbol' is a name for the default constructor created by
              ;; cl-defstruct, so return the location of the cl-defstruct.
              (let* ((type-name (match-string 1 doc))
                     (type-symbol (intern type-name))
                     (file (find-lisp-object-file-name
                            type-symbol 'define-type))
                     (summary (format elisp--xref-format-extra
                                      'cl-defstruct type-symbol
                                      `(:constructor ,symbol))))
                (push (elisp--xref-make-xref 'define-type type-symbol
                                             file summary)
                      xrefs)))

             ((setq generic (cl--generic symbol))
              ;; FIXME: move this to elisp-xref-find-def-functions, in cl-generic.el
              ;; XXX: How are we going to support using newer xref
              ;; with older versions of Emacs, though?

              ;; A generic function. If there is a default method, it
              ;; will appear in the method table, with no
              ;; specializers.
              ;;
              ;; If the default method is declared by the cl-defgeneric
              ;; declaration, it will have the same location as the
              ;; cl-defgeneric, so we want to exclude it from the
              ;; result. In this case, it will have a null doc
              ;; string. User declarations of default methods may also
              ;; have null doc strings, but we hope that is
              ;; rare. Perhaps this heuristic will discourage that.
              (dolist (method (cl--generic-method-table generic))
                (let* ((info (cl--generic-method-info method));; qual-string combined-args doconly
                       (specializers (cl--generic-method-specializers method))
                       (non-default nil)
                       (met-name (cl--generic-load-hist-format
                                  symbol
                                  (cl--generic-method-qualifiers method)
                                  specializers))
                       (file (find-lisp-object-file-name met-name 'cl-defmethod)))
                  (dolist (item specializers)
                    ;; Default method has all t in specializers.
                    (setq non-default (or non-default (not (equal t item)))))

                  ;; Assuming only co-located default has null doc string
                  (when (and file (or non-default (nth 2 info)))
                    (if specializers
                        (let ((summary (format elisp--xref-format-extra
                                               'cl-defmethod symbol
                                               (nth 1 info))))
                          (push (elisp--xref-make-xref 'cl-defmethod met-name
                                                       file summary)
                                xrefs))

                      (let ((summary (format elisp--xref-format-extra
                                             'cl-defmethod symbol ())))
                        (push (elisp--xref-make-xref 'cl-defmethod met-name
                                                     file summary)
                              xrefs))))
                  ))

              ;; FIXME: We rely on the fact that `cl-defgeneric' sets
              ;; a `function-documentation' property (via the third arg of
              ;; `defalias'), whereas implicit declaration of a generic via
              ;; `cl-defmethod' doesn't.
              (if (null (get symbol 'function-documentation))
                  ;; This symbol is an implicitly defined defgeneric, so
                  ;; don't return it.
                  nil
                (push (elisp--xref-make-xref 'cl-defgeneric symbol file) xrefs))
              )

             (t
              (push (elisp--xref-make-xref nil symbol file) xrefs))
             ))))

      (when (boundp symbol)
        ;; A variable
        (let ((file (find-lisp-object-file-name symbol 'defvar)))
          (when file
            (cond
             ((eq file 'C-source)
              ;; The doc strings from the C source have not been loaded
              ;; yet; help-C-file-name does that.  Second call will
              ;; return "src/*.c" in file; handled below.
              (push (elisp--xref-make-xref 'defvar symbol (help-C-file-name symbol 'var)) xrefs))

             (t
              (push (elisp--xref-make-xref 'defvar symbol file) xrefs))

             ))))

      (when (featurep symbol)
        (let ((file (ignore-errors
                      (find-library-name (symbol-name symbol)))))
          (when file
            (push (elisp--xref-make-xref 'feature symbol file) xrefs))))
      );; 'unless xrefs'

    xrefs))

(declare-function xref-apropos-regexp "xref" (pattern))

(cl-defmethod xref-backend-apropos ((_backend (eql 'elisp)) pattern)
  (apply #'nconc
         (let ((regexp (xref-apropos-regexp pattern))
               lst)
           (dolist (sym (apropos-internal regexp))
             (push (elisp--xref-find-definitions sym) lst))
           (nreverse lst))))

(defvar elisp--xref-identifier-completion-table
  (apply-partially #'completion-table-with-predicate
                   obarray
                   (lambda (sym)
                     (or (boundp sym)
                         (fboundp sym)
                         (featurep sym)
                         (facep sym)))
                   'strict))

(cl-defmethod xref-backend-identifier-completion-table ((_backend
                                                         (eql 'elisp)))
  elisp--xref-identifier-completion-table)

(cl-defstruct (xref-elisp-location
               (:constructor xref-make-elisp-location (symbol type file)))
  "Location of an Emacs Lisp symbol definition."
  symbol type file)

(cl-defmethod xref-location-marker ((l xref-elisp-location))
  (pcase-let (((cl-struct xref-elisp-location symbol type file) l))
    (let ((buffer-point (find-function-search-for-symbol symbol type file)))
      (with-current-buffer (car buffer-point)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (or (cdr buffer-point) (point-min)))
            (point-marker)))))))

(cl-defmethod xref-location-group ((l xref-elisp-location))
  (let ((file (xref-elisp-location-file l)))
    (defvar find-function-C-source-directory)
    (if (and find-function-C-source-directory
             (string-match-p "\\`src/" file))
        (concat find-function-C-source-directory
                (substring file 3))
      file)))

(defun elisp-load-path-roots ()
  (if (boundp 'package-user-dir)
      (cons package-user-dir load-path)
    load-path))

;;; Elisp Interaction mode

(defvar-keymap lisp-interaction-mode-map
  :doc "Keymap for Lisp Interaction mode.
All commands in `lisp-mode-shared-map' are inherited by this map."
  :parent lisp-mode-shared-map
  "C-M-x" #'eval-defun
  "C-M-q" #'indent-pp-sexp
  "C-c C-e" #'elisp-eval-region-or-buffer
  "C-c C-b" #'elisp-byte-compile-buffer
  "M-TAB" #'completion-at-point
  "C-j"   #'eval-print-last-sexp)

(easy-menu-define lisp-interaction-mode-menu lisp-interaction-mode-map
  "Menu for Lisp Interaction mode."
  '("Lisp-Interaction"
    ["Complete Lisp Symbol" completion-at-point
     :help "Perform completion on Lisp symbol preceding point"]
    ["Indent or Pretty-Print" indent-pp-sexp
     :help "Indent each line of the list starting just after point, or prettyprint it"]
    ["Instrument Function for Debugging" edebug-defun
     :help "Evaluate the top level form point is in, stepping through with Edebug"
     :keys "C-u C-M-x"]
    ["Evaluate and Print" eval-print-last-sexp
     :help "Evaluate sexp before point; print value into current buffer"]
    ["Evaluate Defun" eval-defun
     :help "Evaluate the top-level form containing point, or after point"]))

(define-derived-mode lisp-interaction-mode emacs-lisp-mode "Lisp Interaction"
  "Major mode for typing and evaluating Lisp forms.
Like Lisp mode except that \\[eval-print-last-sexp] evals the Lisp expression
before point, and prints its value into the buffer, advancing point.
Note that printing is controlled by `eval-expression-print-length'
and `eval-expression-print-level'.
\\<lisp-interaction-mode-map>
- \\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
- Paragraphs are separated only by blank lines.
- Semicolons start comments.

\\{lisp-interaction-mode-map}"
  :abbrev-table nil
  (setq-local lexical-binding t))

;;; Emacs Lisp Byte-Code mode

(eval-and-compile
  (defconst emacs-lisp-byte-code-comment-re
    (concat "\\(#\\)@\\([0-9]+\\) "
            ;; Make sure it's a docstring and not a lazy-loaded byte-code.
            "\\(?:[^(]\\|([^\"]\\)")
    "Regular expression matching a dynamic doc string comment."))

(defun elisp--byte-code-comment (end &optional _point)
  "Try to syntactically mark the #@NNN ....^_ docstrings in byte-code files."
  (let ((ppss (syntax-ppss)))
    (when (and (nth 4 ppss)
               (eq (char-after (nth 8 ppss)) ?#))
      (let* ((n (save-excursion
                  (goto-char (nth 8 ppss))
                  (when (looking-at emacs-lisp-byte-code-comment-re)
                    (string-to-number (match-string 2)))))
             ;; `maxdiff' tries to make sure the loop below terminates.
             (maxdiff n))
        (when n
          (let* ((bchar (match-end 2))
                 (b (position-bytes bchar)))
            (goto-char (+ b n))
            (while (let ((diff (- (position-bytes (point)) b n)))
                     (unless (zerop diff)
                       (when (> diff maxdiff) (setq diff maxdiff))
                       (forward-char (- diff))
                       (setq maxdiff (if (> diff 0) diff
                                       (max (1- maxdiff) 1)))
                       t))))
          (if (<= (point) end)
              (put-text-property (1- (point)) (point)
                                 'syntax-table
                                 (string-to-syntax "> b"))
            (goto-char end)))))))

(defun elisp-byte-code-syntax-propertize (start end)
  (goto-char start)
  (elisp--byte-code-comment end (point))
  (funcall
   (syntax-propertize-rules
    (emacs-lisp-byte-code-comment-re
     (1 (prog1 "< b"
          (goto-char (match-end 2))
          (elisp--byte-code-comment end (point))))))
   start end))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elc\\'" . elisp-byte-code-mode))
;;;###autoload
(define-derived-mode elisp-byte-code-mode emacs-lisp-mode
  "Elisp-Byte-Code"
  "Major mode for *.elc files."
  ;; TODO: Add way to disassemble byte-code under point.
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local syntax-propertize-function
              #'elisp-byte-code-syntax-propertize))


;;; Globally accessible functionality

(defun eval-print-last-sexp (&optional eval-last-sexp-arg-internal)
  "Evaluate sexp before point; print value into current buffer.

Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix numeric argument.
Normally, this function truncates long output according to the value
of the variables `eval-expression-print-length' and
`eval-expression-print-level'.  But if EVAL-LAST-SEXP-ARG-INTERNAL is zero,
there is no such truncation, and integers are printed in several additional
formats (octal, hexadecimal, and character).

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger."
  (interactive "P")
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp (or eval-last-sexp-arg-internal t))
    (terpri)))


(defun last-sexp-setup-props (beg end value alt1 alt2)
  "Set up text properties for the output of `elisp--eval-last-sexp'.
BEG and END are the start and end of the output in current buffer.
VALUE is the Lisp value printed, ALT1 and ALT2 are strings for the
alternative printed representations that can be displayed."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'elisp-last-sexp-toggle-display)
    (define-key map [down-mouse-2] #'mouse-set-point)
    (define-key map [mouse-2] #'elisp-last-sexp-toggle-display)
    (add-text-properties
     beg end
     `(printed-value (,value ,alt1 ,alt2)
		     mouse-face highlight
		     keymap ,map
		     help-echo "RET, mouse-2: toggle abbreviated display"
		     rear-nonsticky (mouse-face keymap help-echo
						printed-value)))))


(defun elisp-last-sexp-toggle-display (&optional _arg)
  "Toggle between abbreviated and unabbreviated printed representations."
  (interactive "P")
  (save-restriction
    (widen)
    (let ((value (get-text-property (point) 'printed-value)))
      (when value
	(let ((beg (or (previous-single-property-change (min (point-max) (1+ (point)))
							'printed-value)
		       (point)))
	      (end (or (next-single-char-property-change (point) 'printed-value) (point)))
	      (standard-output (current-buffer))
	      (point (point)))
	  (delete-region beg end)
	  (insert (nth 1 value))
	  (or (= beg point)
	      (setq point (1- (point))))
	  (last-sexp-setup-props beg (point)
				 (nth 0 value)
				 (nth 2 value)
				 (nth 1 value))
	  (goto-char (min (point-max) point)))))))

(defun prin1-char (char)                ;FIXME: Move it, e.g. to simple.el.
  "Return a string representing CHAR as a character rather than as an integer.
If CHAR is not a character, return nil."
  (and (integerp char)
       (eventp char)
       (let ((c (event-basic-type char))
	     (mods (event-modifiers char))
	     string)
	 ;; Prevent ?A from turning into ?\S-a.
	 (if (and (memq 'shift mods)
		  (zerop (logand char ?\S-\^@))
		  (not (let ((case-fold-search nil))
			 (char-equal c (upcase c)))))
	     (setq c (upcase c) mods nil))
	 ;; What string are we considering using?
	 (condition-case nil
	     (setq string
		   (concat
		    "?"
		    (mapconcat
		     (lambda (modif)
		       (cond ((eq modif 'super) "\\s-")
			     (t (string ?\\ (upcase (aref (symbol-name modif) 0)) ?-))))
		     mods)
		    (cond
		     ((memq c '(?\; ?\( ?\) ?\{ ?\} ?\[ ?\] ?\" ?\' ?\\)) (string ?\\ c))
		     ((eq c 127) "\\C-?")
		     (t
		      (string c)))))
	   (error nil))
	 ;; Verify the string reads a CHAR, not to some other character.
	 ;; If it doesn't, return nil instead.
	 (and string
	      (= (car (read-from-string string)) char)
	      string))))

(defun elisp--preceding-sexp ()
  "Return sexp before the point."
  (let ((opoint (point))
	(left-quote ?‘)
	expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
	;; If this sexp appears to be enclosed in `...' or ‘...’
	;; then ignore the surrounding quotes.
	(cond ((eq (preceding-char) ?’)
	       (progn (forward-char -1) (setq opoint (point))))
	      ((or (eq (following-char) ?\')
		   (eq (preceding-char) ?\'))
	       (setq left-quote ?\`)))

        ;; When after a named character literal, skip over the entire
        ;; literal, not only its last word.
        (when (= (preceding-char) ?})
          (let ((begin (save-excursion
                         (backward-char)
                         (skip-syntax-backward "w-")
                         (backward-char 3)
                         (when (looking-at-p "\\\\N{") (point)))))
            (when begin (goto-char begin))))

	(forward-sexp -1)
	;; If we were after `?\e' (or similar case),
	;; use the whole thing, not just the `e'.
	(when (eq (preceding-char) ?\\)
	  (forward-char -1)
	  (when (eq (preceding-char) ??)
	    (forward-char -1)))

	;; Skip over hash table read syntax.
	(and (> (point) (1+ (point-min)))
	     (looking-back "#s" (- (point) 2))
	     (forward-char -2))

	;; Skip over `#N='s.
	(when (eq (preceding-char) ?=)
	  (let (labeled-p)
	    (save-excursion
	      (skip-chars-backward "0-9#=")
	      (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
	    (when labeled-p
	      (forward-sexp -1))))

	(save-restriction
	  (if (eq (following-char) left-quote)
              ;; vladimir@cs.ualberta.ca 30-Jul-1997: Skip ` in `variable' so
              ;; that the value is returned, not the name.
	      (forward-char))
          (when (looking-at ",@?") (goto-char (match-end 0)))
	  (narrow-to-region (point-min) opoint)
	  (setq expr (read (current-buffer)))
          ;; If it's an (interactive ...) form, it's more useful to show how an
          ;; interactive call would use it.
          ;; FIXME: Is it really the right place for this?
          (when (eq (car-safe expr) 'interactive)
	    (setq expr
                  `(call-interactively
                    (lambda (&rest args) ,expr args))))
	  expr)))))
(define-obsolete-function-alias 'preceding-sexp #'elisp--preceding-sexp "25.1")

(defun elisp--eval-last-sexp (eval-last-sexp-arg-internal)
  "Evaluate sexp before point; print value in the echo area.
If EVAL-LAST-SEXP-ARG-INTERNAL is non-nil, print output into
current buffer.  If EVAL-LAST-SEXP-ARG-INTERNAL is `0', print
output with no limit on the length and level of lists, and
include additional formats for integers \(octal, hexadecimal, and
character)."
  (pcase-let*
      ((`(,insert-value ,no-truncate ,char-print-limit)
        (eval-expression-get-print-arguments eval-last-sexp-arg-internal)))
    ;; The expression might change to a different buffer, so record the
    ;; desired output stream now.
    (let ((output (if insert-value (current-buffer) t)))
      ;; Setup the lexical environment if lexical-binding is enabled.
      (elisp--eval-last-sexp-print-value
       (eval (macroexpand-all
              (eval-sexp-add-defvars
               (elisp--eval-defun-1 (macroexpand (elisp--preceding-sexp)))))
             lexical-binding)
       output no-truncate char-print-limit))))

(defun elisp--eval-last-sexp-print-value
    (value output &optional no-truncate char-print-limit)
  (let* ((unabbreviated (let ((print-length nil) (print-level nil))
                          (prin1-to-string value)))
         (eval-expression-print-maximum-character char-print-limit)
         (print-length (unless no-truncate eval-expression-print-length))
         (print-level  (unless no-truncate eval-expression-print-level))
         (beg (point))
         end)
    (prog1
	(prin1 value output)
      (let ((str (and char-print-limit (eval-expression-print-format value))))
	(if str (princ str output)))
      (setq end (point))
      (when (and (bufferp output)
		 (or (not (null print-length))
		     (not (null print-level)))
		 (not (string= unabbreviated
			       (buffer-substring-no-properties beg end))))
	(last-sexp-setup-props beg end value
			       unabbreviated
			       (buffer-substring-no-properties beg end))
	))))

(defun eval-sexp-add-defvars (exp &optional pos)
  "Prepend EXP with all the `defvar's that precede it in the buffer.
POS specifies the starting position where EXP was found and defaults to point."
  (if (not lexical-binding)
      exp
    (save-excursion
      (unless pos (setq pos (point)))
      (let ((vars ()))
        (goto-char (point-min))
        (while (re-search-forward
                "(def\\(?:var\\|const\\|custom\\)[ \t\n]+\\([^; '()\n\t]+\\)"
                pos t)
          (let ((var (intern (match-string 1))))
            (unless (or (special-variable-p var)
                        (syntax-ppss-toplevel-pos
                         (save-excursion
                           (syntax-ppss (match-beginning 0)))))
              (push var vars))))
        `(progn ,@(mapcar (lambda (v) `(defvar ,v)) vars) ,exp)))))

(defun eval-last-sexp (eval-last-sexp-arg-internal)
  "Evaluate sexp before point; print value in the echo area.
Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.
With a non `-' prefix argument, print output into current buffer.

This commands handles `defvar', `defcustom' and `defface' the
same way that `eval-defun' does.  See the doc string of that
function for details.

Normally, this function truncates long output according to the
value of the variables `eval-expression-print-length' and
`eval-expression-print-level'.  With a prefix argument of zero,
however, there is no such truncation.
Integer values are printed in several formats (decimal, octal,
and hexadecimal).  When the prefix argument is -1 or the value
doesn't exceed `eval-expression-print-maximum-character', an
integer value is also printed as a character of that codepoint.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger."
  (interactive "P")
  (funcall
   ;; Not sure why commit 4428c27c1ae7d stored into `values' only when
   ;; `eval-expression-debug-on-error' was nil, but let's preserve that.
   (if eval-expression-debug-on-error #'identity #'values--store-value)
   (handler-bind ((error (if eval-expression-debug-on-error
                             #'eval-expression--debug #'ignore)))
     (elisp--eval-last-sexp eval-last-sexp-arg-internal))))

(defun elisp--eval-defun-1 (form)
  "Treat some expressions in FORM specially.
Reset the `defvar' and `defcustom' variables to the initial value.
\(For `defcustom', use the :set function if there is one.)
Reinitialize the face according to the `defface' specification."
  ;; The code in edebug-defun should be consistent with this, but not
  ;; the same, since this gets a macroexpanded form.
  (cond ((not (listp form))
	 form)
	((and (eq (car form) 'defvar)
	      (cdr-safe (cdr-safe form))
	      (boundp (cadr form)))
	 ;; Force variable to be re-set.
	 `(progn (defvar ,(nth 1 form) nil ,@(nthcdr 3 form))
		 (setq-default ,(nth 1 form) ,(nth 2 form))))
	;; `defcustom' is now macroexpanded to
	;; `custom-declare-variable' with a quoted value arg.
	((and (eq (car form) 'custom-declare-variable)
	      (default-boundp (eval (nth 1 form) lexical-binding)))
	 ;; Force variable to be bound, using :set function if specified.
	 (let ((setfunc (memq :set form)))
	   (when setfunc
	     (setq setfunc (car-safe (cdr-safe setfunc)))
	     (or (functionp setfunc) (setq setfunc nil)))
	   (funcall (or setfunc 'set-default)
		    (eval (nth 1 form) lexical-binding)
		    ;; The second arg is an expression that evaluates to
		    ;; an expression.  The second evaluation is the one
		    ;; normally performed not by normal execution but by
		    ;; custom-initialize-set (for example).
		    (eval (eval (nth 2 form) lexical-binding) t)))
	 form)
	;; `defface' is macroexpanded to `custom-declare-face'.
	((eq (car form) 'custom-declare-face)
	 ;; Reset the face.
	 (let ((face-symbol (eval (nth 1 form) lexical-binding)))
	   (remhash face-symbol face--new-frame-defaults)
	   (put face-symbol 'face-defface-spec nil)
	   (put face-symbol 'face-override-spec nil))
	 form)
	((eq (car form) 'progn)
	 (cons 'progn (mapcar #'elisp--eval-defun-1 (cdr form))))
	(t form)))

(defun elisp--eval-defun ()
  "Evaluate defun that point is in or before.
The value is displayed in the echo area.
If the current defun is actually a call to `defvar',
then reset the variable using the initial value expression
even if the variable already has some other value.
\(Normally `defvar' does not change the variable's value
if it already has a value.)

Return the result of evaluation."
  ;; FIXME: the print-length/level bindings should only be applied while
  ;; printing, not while evaluating.
  (defvar elisp--eval-defun-result)
  (let ((edebugging edebug-all-defs)
        elisp--eval-defun-result)
    (save-excursion
      ;; Arrange for eval-region to "read" the (possibly) altered form.
      ;; eval-region handles recording which file defines a function or
      ;; variable.
      (let ((standard-output t)
            beg end form)
        ;; Read the form from the buffer, and record where it ends.
        (save-excursion
          (end-of-defun)
          (beginning-of-defun)
          (setq beg (point))
          (setq form (funcall load-read-function (current-buffer)))
          (setq end (point)))
        ;; Alter the form if necessary.  We bind `print-level' (etc.)
        ;; in the form itself, because we want evalling the form to
        ;; use the original values, while we want the printing to use
        ;; `eval-expression-print-length' (etc.).
        (let ((form `(let ((print-level ,print-level)
                           (print-length ,print-length))
                       ,(eval-sexp-add-defvars
                         (elisp--eval-defun-1
                          (macroexpand form)))))
	      (print-length eval-expression-print-length)
	      (print-level eval-expression-print-level)
              (should-print (if (not edebugging) standard-output)))
          (eval-region beg end should-print
                       (lambda (_ignore)
                         ;; Skipping to the end of the specified region
                         ;; will make eval-region return.
                         (goto-char end)
                         ;; This `setq' needs to be added *after* passing
                         ;; form through `elisp--eval-defun-1' since it
                         ;; would otherwise "hide" forms like `defvar's and
                         ;; thus defeat their special treatment.
                         `(setq elisp--eval-defun-result ,form))))))
    (let ((str (eval-expression-print-format elisp--eval-defun-result)))
      (if str (princ str)))
    elisp--eval-defun-result))

(defun eval-defun (edebug-it)
  "Evaluate top-level form around point and instrument it if EDEBUG-IT is non-nil.
Interactively, EDEBUG-IT is the prefix argument.
If `edebug-all-defs' is non-nil, that inverts the meaning of EDEBUG-IT
and the prefix argument: this function will instrument the form
unless EDEBUG-IT is non-nil.  The command `edebug-all-defs' toggles
the value of the variable `edebug-all-defs'.

If point isn't in a top-level form, evaluate the first top-level
form after point.  If there is no top-level form after point,
evaluate the first preceding top-level form.

If the current defun is actually a call to `defvar' or `defcustom',
evaluating it this way resets the variable using its initial value
expression (using the defcustom's :set function if there is one), even
if the variable already has some other value.  (Normally `defvar' and
`defcustom' do not alter the value if there already is one.)  In an
analogous way, evaluating a `defface' overrides any customizations of
the face, so that it becomes defined exactly as the `defface' expression
says.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger.

If acting on a `defun' for FUNCTION, and the function was
instrumented, `Edebug: FUNCTION' is printed in the echo area.  If not
instrumented, just FUNCTION is printed.

If not acting on a `defun', the result of evaluation is displayed in
the echo area.  This display is controlled by the variables
`eval-expression-print-length' and `eval-expression-print-level',
which see."
  (interactive "P")
  (cond (edebug-it
	 (require 'edebug)
	 (defvar edebug-all-defs)
	 (eval-defun (not edebug-all-defs)))
	(t
	 (handler-bind ((error (if eval-expression-debug-on-error
	                           #'eval-expression--debug #'ignore)))
	   (elisp--eval-defun)))))

;;; ElDoc Support

(defvar elisp--eldoc-last-data (make-vector 3 nil)
  "Bookkeeping.
Elements are as follows:
  0 - contains the last symbol read from the buffer.
  1 - contains the string last displayed in the echo area for variables,
      or argument string for functions.
  2 - `function' if function args, `variable' if variable documentation.")

(defun elisp--documentation-one-liner ()
  (let* (str
         (callback (lambda (doc &rest plist)
                     (when doc
                       (setq str
                             (format "%s: %s"
                                     (propertize (prin1-to-string
                                                  (plist-get plist :thing))
                                                 'face (plist-get plist :face))
                                     doc))))))
    (or (progn (elisp-eldoc-var-docstring callback) str)
        (progn (elisp-eldoc-funcall callback) str))))

(defalias 'elisp-eldoc-documentation-function #'elisp--documentation-one-liner
  "Return Elisp documentation for the thing at point as one-line string.
This is meant as a backward compatibility aide to the \"old\"
Elisp eldoc behavior.  Consider variable docstrings and function
signatures only, in this order.  If none applies, returns nil.
Changes to `eldoc-documentation-functions' and
`eldoc-documentation-strategy' are _not_ reflected here.  As such
it is preferable to use ElDoc's interfaces directly.")

(make-obsolete 'elisp-eldoc-documentation-function
               "use ElDoc's interfaces instead." "28.1")

(defun elisp-eldoc-funcall (callback &rest _ignored)
  "Document function call at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see)."
  (let* ((sym-info (elisp--fnsym-in-current-sexp))
         (fn-sym (car sym-info)))
    (when fn-sym
      (funcall callback (apply #'elisp-get-fnsym-args-string sym-info)
               :thing fn-sym
               :face (if (functionp fn-sym)
                         'font-lock-function-name-face
                       'font-lock-keyword-face)))))

(defcustom elisp-eldoc-docstring-length-limit 1000
  "Maximum length of doc strings displayed by elisp ElDoc functions."
  :type 'natnum
  :version "31.1")

(defcustom elisp-eldoc-funcall-with-docstring-length 'short
  "Control length of doc string shown by `elisp-eldoc-funcall-with-docstring'.
If set to `short', only show the first sentence of the doc string.
Otherwise if set to `full', display full doc string."
  :type '(choice
          (const :tag "Short" short)
          (const :tag "Full" full))
  :version "31.1")

(defun elisp-eldoc-funcall-with-docstring (callback &rest _ignored)
  "Document function call at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see).
Compared to `elisp-eldoc-funcall', this also includes the
current function doc string, doc string length depends on
`elisp-eldoc-funcall-with-docstring-length'."
  (when-let* ((sym-info (elisp--fnsym-in-current-sexp))
              (fn-sym (car sym-info))
              ((fboundp fn-sym))
              (fn-doc (or (cdr (help-split-fundoc
                                (condition-case nil (documentation fn-sym t)
                                  (invalid-function nil))
                                fn-sym))
                          "Undocumented."))
              (more (- (length fn-doc) elisp-eldoc-docstring-length-limit))
              (doc (concat
                      (propertize
                       (string-limit fn-doc elisp-eldoc-docstring-length-limit)
                       'face 'font-lock-doc-face)
                      (when (> more 0)
                        (format "[%sc more]" more)))))
    (funcall callback
               (concat (apply #'elisp-get-fnsym-args-string sym-info)
                       ;; Ensure not display the docstring in the
                       ;; mode-line.
                       (when (not (minibufferp))
                         (concat
                          "\n"
                          (pcase elisp-eldoc-funcall-with-docstring-length
                            ('full doc)
                            ('short
                             (save-match-data
                               (when (string-match "\\." doc)
                                 (concat "\n" (substring doc 0 (match-end 0))))))))))
               :thing fn-sym
               :face (if (functionp fn-sym)
                         'font-lock-function-name-face
                       'font-lock-keyword-face))))

(defun elisp-eldoc-var-docstring (callback &rest _ignored)
  "Document variable at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see).
Also see `elisp-eldoc-var-docstring-with-value'."
  (let* ((sym (elisp--current-symbol))
         (docstring (and sym (elisp-get-var-docstring sym))))
    (when docstring
      (funcall callback docstring
               :thing sym
               :face 'font-lock-variable-name-face))))

(defun elisp-eldoc-var-docstring-with-value (callback &rest _)
  "Document variable at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see).
Compared to `elisp-eldoc-var-docstring', this also includes the
current variable value and a bigger chunk of the docstring."
  (when-let* ((cs (elisp--current-symbol)))
    (when (and (boundp cs)
	       ;; nil and t are boundp!
	       (not (null cs))
	       (not (eq cs t)))
      (funcall callback
	       (format "%.100S %s"
		       (symbol-value cs)
		       (let* ((doc (documentation-property
                                    cs 'variable-documentation t))
			      (more (- (length doc) elisp-eldoc-docstring-length-limit)))
			 (concat (propertize
				  (string-limit (if (string= doc "nil")
						    "Undocumented."
						  doc)
					        elisp-eldoc-docstring-length-limit)
				  'face 'font-lock-doc-face)
				 (when (> more 0)
				   (format "[%sc more]" more)))))
	       :thing cs
	       :face 'font-lock-variable-name-face))))

(defun elisp-get-fnsym-args-string (sym &optional index)
  "Return a string containing the parameter list of the function SYM.
INDEX is the index of the parameter in the returned string to highlight.
If SYM is a subr and no arglist is obtainable from the docstring
or elsewhere, return a 1-line docstring."
  (let ((argstring
	 (cond
	  ((not (and sym (symbolp sym) (fboundp sym))) nil)
	  ((and (eq sym (aref elisp--eldoc-last-data 0))
		(eq 'function (aref elisp--eldoc-last-data 2)))
	   (aref elisp--eldoc-last-data 1))
	  (t
	   (let* ((advertised (get-advertised-calling-convention
                               (indirect-function sym)))
                  doc
		  (args
		   (cond
		    ((listp advertised) advertised)
		    ((setq doc (help-split-fundoc
				(condition-case nil (documentation sym t)
				  (invalid-function nil))
				sym))
		     (substitute-command-keys (car doc)))
		    (t (help-function-arglist sym)))))
             ;; Stringify, and store before highlighting, downcasing, etc.
	     (elisp--last-data-store sym (elisp-function-argstring args)
                                     'function))))))
    ;; Highlight
    (if argstring
	(elisp--highlight-function-argument
         sym argstring index))))

(defun elisp--highlight-function-argument (sym args index)
  "Highlight the argument of function SYM whose index is INDEX.
ARGS is the argument list of function SYM."
  ;; FIXME: This should probably work on the list representation of `args'
  ;; rather than its string representation.
  ;; FIXME: This function is much too long, we need to split it up!
  (let* ((start          nil)
         (end            0)
         (argument-face  'eldoc-highlight-function-argument)
         (args-lst (mapcar (lambda (x)
                             (replace-regexp-in-string
                              "\\`[(]\\|[)]\\'" "" x))
                           (split-string args)))
         (args-lst-ak (cdr (member "&key" args-lst))))
    ;; Find the current argument in the argument string.  We need to
    ;; handle `&rest' and informal `...' properly.
    ;;
    ;; FIXME: What to do with optional arguments, like in
    ;;        (defun NAME ARGLIST [DOCSTRING] BODY...) case?
    ;;        The problem is there is no robust way to determine if
    ;;        the current argument is indeed a docstring.

    ;; When `&key' is used finding position based on `index'
    ;; would be wrong, so find the arg at point and determine
    ;; position in ARGS based on this current arg.
    (when (and args-lst-ak
               (>= index (- (length args-lst) (length args-lst-ak))))
      (let* (case-fold-search
             key-have-value
             (sym-name (symbol-name sym))
             (cur-w (current-word t))
             (limit (save-excursion
                      (when (re-search-backward sym-name nil t)
                        (match-end 0))))
             (cur-a (if (and cur-w (string-match ":\\([^ ()]*\\)" cur-w))
                        (substring cur-w 1)
                      (save-excursion
                        (let (split)
                          (when (re-search-backward ":\\([^ ()\n]*\\)" limit t)
                            (setq split (split-string (match-string 1) " " t))
                            (prog1 (car split)
                              (when (cdr split)
                                (setq key-have-value t))))))))
             ;; If `cur-a' is not one of `args-lst-ak'
             ;; assume user is entering an unknown key
             ;; referenced in last position in signature.
             (other-key-arg (and (stringp cur-a)
                                 args-lst-ak
                                 (not (member (upcase cur-a) args-lst-ak))
                                 (upcase (car (last args-lst-ak))))))
        (unless (or (null cur-w) (string= cur-w sym-name))
          ;; The last keyword have already a value
          ;; i.e :foo a b and cursor is at b.
          ;; If signature have also `&rest'
          ;; (assume it is after the `&key' section)
          ;; go to the arg after `&rest'.
          (if (and key-have-value
                   (save-excursion
                     (not (re-search-forward ":.*" (line-end-position) t)))
                   (string-match "&rest \\([^ ()]*\\)" args))
              (setq index nil ; Skip next block based on positional args.
                    start (match-beginning 1)
                    end   (match-end 1))
            ;; If `cur-a' is nil probably cursor is on a positional arg
            ;; before `&key', in this case, exit this block and determine
            ;; position with `index'.
            (when (and cur-a     ; A keyword arg (dot removed) or nil.
                       (or (string-match
                            (concat "\\_<" (upcase cur-a) "\\_>") args)
                           (string-match
                            (concat "\\_<" other-key-arg "\\_>") args)))
              (setq index nil ; Skip next block based on positional args.
                    start (match-beginning 0)
                    end   (match-end 0)))))))
    ;; Handle now positional arguments.
    (while (and index (>= index 1))
      (if (string-match "[^ ()]+" args end)
	  (progn
	    (setq start (match-beginning 0)
		  end   (match-end 0))
	    (let ((argument (match-string 0 args)))
	      (cond ((string= argument "&rest")
		     ;; All the rest arguments are the same.
		     (setq index 1))
		    ((string= argument "&optional"))         ; Skip.
                    ((string= argument "&allow-other-keys")) ; Skip.
                    ;; Back to index 0 in ARG1 ARG2 ARG2 ARG3 etc...
                    ;; like in `setq'.
		    ((or (and (string-match-p "\\.\\.\\.\\'" argument)
                              (string= argument (car (last args-lst))))
                         (and (string-match-p "\\.\\.\\.\\'"
                                              (substring args 1 (1- (length args))))
                              (= (length (remove "..." args-lst)) 2)
                              (> index 1) (oddp index)))
                     (setq index 0))
		    (t
		     (setq index (1- index))))))
	(setq end           (length args)
	      start         (1- end)
	      argument-face 'font-lock-warning-face
	      index         0)))
    (let ((doc args))
      (when start
	(setq doc (copy-sequence args))
	(add-text-properties start end (list 'face argument-face) doc))
      doc)))

;; Return a string containing a brief (one-line) documentation string for
;; the variable.
(defun elisp-get-var-docstring (sym)
  (cond ((not sym) nil)
        ((and (eq sym (aref elisp--eldoc-last-data 0))
              (eq 'variable (aref elisp--eldoc-last-data 2)))
         (aref elisp--eldoc-last-data 1))
        (t
         (let ((doc (documentation-property sym 'variable-documentation t)))
           (when doc
             (let ((doc (elisp--docstring-first-line doc)))
               (elisp--last-data-store sym doc 'variable)))))))

(defun elisp--last-data-store (symbol doc type)
  (aset elisp--eldoc-last-data 0 symbol)
  (aset elisp--eldoc-last-data 1 doc)
  (aset elisp--eldoc-last-data 2 type)
  doc)

;; Note that any leading `*' in the docstring (which indicates the variable
;; is a user option) is removed.
(defun elisp--docstring-first-line (doc)
  (and (stringp doc)
       (substitute-command-keys
        (save-match-data
	  ;; Don't use "^" in the regexp below since it may match
	  ;; anywhere in the doc-string.
	  (let ((start (if (string-match "\\`\\*" doc) (match-end 0) 0)))
            (cond ((string-match "\n" doc)
                   (substring doc start (match-beginning 0)))
                  ((zerop start) doc)
                  (t (substring doc start))))))))

;; Return a list of current function name and argument index.
(defun elisp--fnsym-in-current-sexp ()
  (save-excursion
    (unless (nth 8 (syntax-ppss))
      (let ((argument-index (1- (elisp--beginning-of-sexp))))
        ;; If we are at the beginning of function name, this will be -1.
        (when (< argument-index 0)
          (setq argument-index 0))
        (list (elisp--current-symbol) argument-index)))))

;; Move to the beginning of current sexp.  Return the number of nested
;; sexp the point was over or after.
(defun elisp--beginning-of-sexp ()
  (let ((parse-sexp-ignore-comments t)
	(num-skipped-sexps 0))
    (condition-case _
	(progn
	  ;; First account for the case the point is directly over a
	  ;; beginning of a nested sexp.
	  (condition-case _
	      (let ((p (point)))
		(forward-sexp -1)
		(forward-sexp 1)
		(when (< (point) p)
		  (setq num-skipped-sexps 1)))
	    (error))
	  (while
	      (let ((p (point)))
		(forward-sexp -1)
		(when (< (point) p)
		  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

;; returns nil unless current word is an interned symbol.
(defun elisp--current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (intern-soft (current-word)))))

(defun elisp-function-argstring (arglist)
  "Return ARGLIST as a string enclosed by ().
ARGLIST is either a string, or a list of strings or symbols."
  (let ((str (cond ((stringp arglist) arglist)
                   ((not (listp arglist)) nil)
                   (t (substitute-command-keys
                       (help--make-usage-docstring 'toto arglist))))))
    (if (and str (string-match "\\`([^ )]+ ?" str))
        (replace-match "(" t t str)
      str)))

;;; Flymake support

;; Don't require checkdoc, but forward declare these checkdoc special
;; variables. Autoloading them on `checkdoc-current-buffer' is too
;; late, they won't be bound dynamically.
(defvar checkdoc-create-error-function)
(defvar checkdoc-autofix-flag)
(defvar checkdoc-generate-compile-warnings-flag)
(defvar checkdoc-diagnostic-buffer)

;;;###autoload
(defun elisp-flymake-checkdoc (report-fn &rest _args)
  "A Flymake backend for `checkdoc'.
Calls REPORT-FN directly."
  (let (collected)
    (let* ((checkdoc-create-error-function
            (lambda (text start end &optional unfixable)
              (push (list text start end unfixable) collected)
              nil))
           (checkdoc-autofix-flag nil)
           (checkdoc-generate-compile-warnings-flag nil)
           (checkdoc-diagnostic-buffer
            (generate-new-buffer " *checkdoc-temp*")))
      (unwind-protect
          (save-excursion
            ;; checkdoc-current-buffer can error if there are
            ;; unbalanced parens, for example, but this shouldn't
            ;; disable the backend (bug#29176).
            (ignore-errors
              (checkdoc-current-buffer t)))
        (kill-buffer checkdoc-diagnostic-buffer)))
    (funcall report-fn
             (cl-loop for (text start end _unfixable) in
                      collected
                      collect
                      (flymake-make-diagnostic
                       (current-buffer)
                       (or start 1) (or end (1+ (or start 1)))
                       :note text)))
    collected))

(defun elisp-flymake--byte-compile-done (report-fn
                                         source-buffer
                                         output-buffer)
  (with-current-buffer
      source-buffer
    (save-excursion
      (save-restriction
        (widen)
        (funcall
         report-fn
         (cl-loop with data =
                  (with-current-buffer output-buffer
                    (goto-char (point-min))
                    (search-forward ":elisp-flymake-output-start")
                    (read (point-marker)))
                  for (string pos _fill level) in data
                  do (goto-char pos)
                  for beg = (if (< (point) (point-max))
                                (point)
                              (line-beginning-position))
                  for end = (min
                             (line-end-position)
                             (or (cdr
                                  (bounds-of-thing-at-point 'sexp))
                                 (point-max)))
                  collect (flymake-make-diagnostic
                           (current-buffer)
                           (if (= beg end)
                               (max (1- beg) (point-min))
                             beg)
                           (if (= beg end)
                               (min (max beg (1+ (point-min)))
                                    (point-max))
                             end)
                           level
                           string)))))))

(defvar-local elisp-flymake--byte-compile-process nil
  "Buffer-local process started for byte-compiling the buffer.")

(defvar elisp-flymake-byte-compile-load-path (list "./")
  "Like `load-path' but used by `elisp-flymake-byte-compile'.
The default value contains just \"./\" which includes the default
directory of the buffer being compiled, and nothing else.")

(put 'elisp-flymake-byte-compile-load-path 'safe-local-variable
     (lambda (x) (and (listp x) (catch 'tag
                                  (dolist (path x t) (unless (stringp path)
                                                       (throw 'tag nil)))))))

(defvar bytecomp--inhibit-lexical-cookie-warning)

(defcustom elisp-flymake-byte-compile-executable nil
  "The Emacs executable to use for Flymake byte compilation.

If non-nil, this should be an absolute or relative file name of an Emacs
executable to use for byte compilation by Flymake.  If it's a relative
file name, it should be relative to the root directory of the project
containing the file being compiled, as determined by `project-current'.

If nil, or if the file named by this does not exist, Flymake will
use the same executable as the running Emacs, as specified by the
variables `invocation-name' and `invocation-directory'."
  :type '(choice
          (const :tag "Current session's executable" nil)
          (file :tag "Specific Emacs executable"))
  :group 'lisp
  :version "31.1")

(declare-function project-root "project" (project))
(defun elisp-flymake-byte-compile--executable ()
  "Return absolute file name of the Emacs executable for flymake byte-compilation."
  (cond
   ((null elisp-flymake-byte-compile-executable)
    (expand-file-name invocation-name invocation-directory))
   ((not (stringp elisp-flymake-byte-compile-executable))
    (error "Invalid `elisp-flymake-byte-compile-executable': %s"
           elisp-flymake-byte-compile-executable))
   ((file-name-absolute-p elisp-flymake-byte-compile-executable)
    elisp-flymake-byte-compile-executable)
   (t ; relative file name
    (let ((filename (file-name-concat (project-root (project-current))
                                      elisp-flymake-byte-compile-executable)))
      (if (file-executable-p filename)
          filename
        ;; The user might not have built Emacs yet, so just fall back.
        (message "`elisp-flymake-byte-compile-executable' (%s) doesn't exist"
                 elisp-flymake-byte-compile-executable)
        (expand-file-name invocation-name invocation-directory))))))

;;;###autoload
(defun elisp-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (unless (trusted-content-p)
    ;; FIXME: Use `bwrap' and friends to compile untrusted content.
    ;; FIXME: We emit a message *and* signal an error, because by default
    ;; Flymake doesn't display the warning it puts into "*flymake log*".
    (message "Disabling elisp-flymake-byte-compile in %s (untrusted content)"
             (buffer-name))
    (user-error "Disabling elisp-flymake-byte-compile in %s (untrusted content)"
                (buffer-name)))
  (when elisp-flymake--byte-compile-process
    (when (process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process)))
  (let ((temp-file (make-temp-file "elisp-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer (generate-new-buffer " *elisp-flymake-byte-compile*"))
           ;; Hack: suppress warning about missing lexical cookie in
           ;; *scratch* buffers.
           (warning-suppression-opt
            (and (derived-mode-p 'lisp-interaction-mode)
                 '("--eval"
                   "(setq bytecomp--inhibit-lexical-cookie-warning t)"))))
      (setq
       elisp-flymake--byte-compile-process
       (make-process
        :name "elisp-flymake-byte-compile"
        :buffer output-buffer
        :command `(,(elisp-flymake-byte-compile--executable)
                   "-Q"
                   "--batch"
                   ;; "--eval" "(setq load-prefer-newer t)" ; for testing
                   ,@(mapcan (lambda (path) (list "-L" path))
                             elisp-flymake-byte-compile-load-path)
                   ,@warning-suppression-opt
                   "-f" "elisp-flymake--batch-compile-for-flymake"
                   ,temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond
                 ((not (and (buffer-live-p source-buffer)
                            (eq proc (with-current-buffer source-buffer
                                       elisp-flymake--byte-compile-process))))
                  (flymake-log :warning
                               "byte-compile process %s obsolete" proc))
                 ((zerop (process-exit-status proc))
                  (elisp-flymake--byte-compile-done report-fn
                                                    source-buffer
                                                    output-buffer))
                 (t
                  (funcall report-fn
                           :panic
                           :explanation
                           (format "byte-compile process %s died" proc))))
              (ignore-errors (delete-file temp-file))
              (kill-buffer output-buffer))))
        :stderr " *stderr of elisp-flymake-byte-compile*"
        :noquery t)))))

(defun elisp-flymake--batch-compile-for-flymake (&optional file)
  "Helper for `elisp-flymake-byte-compile'.
Runs in a batch-mode Emacs.  Interactively use variable
`buffer-file-name' for FILE."
  (interactive (list buffer-file-name))
  (let* ((file (or file
                   (car command-line-args-left)))
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8)
         (byte-compile-log-buffer
          (generate-new-buffer " *dummy-byte-compile-log-buffer*"))
         (byte-compile-dest-file-function #'ignore)
         (collected)
         (byte-compile-log-warning-function
          (lambda (string &optional position fill level)
            (push (list string position fill level)
                  collected)
            t)))
    (unwind-protect
        (byte-compile-file file)
      (ignore-errors
        (kill-buffer byte-compile-log-buffer)))
    (prin1 :elisp-flymake-output-start)
    (terpri)
    (pp collected)))

(defun elisp-eval-region-or-buffer ()
  "Evaluate the forms in the active region or the whole current buffer.
In Transient Mark mode when the mark is active, call `eval-region'.
Otherwise, call `eval-buffer'."
  (interactive)
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (eval-buffer))
  (message "Evaluated the %s%s buffer"
           (if (use-region-p) "region in the " "")
           (buffer-name)))

(defun elisp-byte-compile-file (&optional load)
  "Byte compile the file the current buffer is visiting.
If LOAD is non-nil, load the resulting .elc file.  When called
interactively, this is the prefix argument."
  (interactive "P")
  (unless buffer-file-name
    (error "This buffer is not visiting a file"))
  (byte-compile-file buffer-file-name)
  (when load
    (load (funcall byte-compile-dest-file-function buffer-file-name))))

(defun elisp-byte-compile-buffer (&optional load)
  "Byte compile the current buffer, but don't write a file.
If LOAD is non-nil, load byte-compiled data.  When called
interactively, this is the prefix argument."
  (interactive "P")
  (let ((bfn buffer-file-name)
        file elc)
    (require 'bytecomp)
    (unwind-protect
        (progn
          (setq file (make-temp-file "compile" nil ".el")
                elc (funcall byte-compile-dest-file-function file))
          (write-region (point-min) (point-max) file nil 'silent)
          (let ((set-message-function
                 (lambda (message)
                   (when (string-match-p "\\`Wrote " message)
                     'ignore)))
                (byte-compile-log-warning-function
                 (lambda (string position &optional fill level)
                   (if bfn
                       ;; Massage the warnings to that they point to
                       ;; this file, not the one in /tmp.
                       (let ((byte-compile-current-file bfn)
                             (byte-compile-root-dir (file-name-directory bfn)))
                         (byte-compile--log-warning-for-byte-compile
                          string position fill level))
                     ;; We don't have a file name, so the warnings
                     ;; will point to a file that doesn't exist.  This
                     ;; should be fixed in some way.
                     (byte-compile--log-warning-for-byte-compile
                      string position fill level)))))
            (byte-compile-file file))
          (when (and bfn (get-buffer "*Compile-Log*"))
            (with-current-buffer "*Compile-Log*"
              (setq default-directory (file-name-directory bfn))))
          (if load
              (load elc)
            (message "Byte-compiled the current buffer")))
      (when file
        (when (file-exists-p file)
          (delete-file file))
        (when (file-exists-p elc)
          (delete-file elc))))))


(put 'read-symbol-shorthands 'safe-local-variable #'consp)

(provide 'elisp-mode)
;;; elisp-mode.el ends here
