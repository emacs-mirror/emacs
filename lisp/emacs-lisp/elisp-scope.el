;;; elisp-scope.el --- Semantic analysis for Elisp symbols  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: lisp, languages

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

;; This library implements an analysis that determines the role of each
;; symbol in Emacs Lisp code.

;; The analysis assigns to each symbol a "symbol role", such as
;; `function', `bound-variable', `binding-variable', `face', etc.  Each
;; symbol role has associated properties, such as the `:face' property,
;; which specifies a face that is applied to symbols with that role when
;; using semantic highlighting with `elisp-fontify-semantically'.
;; To define new symbol roles, see `elisp-scope-define-symbol-role'.
;;
;; The entry point of the analysis in the function
;; `elisp-scope-analyze-form'.  It takes a caller-provided callback
;; function which will be called to report the information we find about
;; each analyzed symbol: the callback gets the position and length of
;; the analyzed symbol, along with its inferred role and, for
;; locally-bound variables, the position of the binder.
;; `elisp-scope-analyze-form' reads a form from the current buffer,
;; starting from point, using `read-positioning-symbols' to attach
;; position information to symbols.  It then recursively analyzes the
;; form, reporting information about each symbol it encounters via the
;; caller-provided callback function.
;;
;; The core of the analysis that `elisp-scope-analyze-form' performs is
;; implemented in the recursive function `elisp-scope-1', which analyzes
;; an sexp as an evaluated form, propagating contextual information such
;; as local variable bindings down to analyzed sub-forms.
;; `elisp-scope-1' takes two arguments: FORM, which is the form to
;; analyze, and OUTSPEC, which is a specification of the expected
;; value of FORM used to analyze quoted data.  The analysis proceeds
;; as follows:
;;
;; - If FORM is a symbol, `elisp-scope-1' reports it as a variable.
;;
;; - If FORM is a cons cell (HEAD . ARGS), then the analysis depends
;;   on HEAD.  HEAD can have a bespoke "analyzer function" AF,
;;   which is called as (AF HEAD . ARGS) and is responsible for
;;   (recursively) analyzing FORM.  The analyzer function can be
;;   associated to HEAD either locally, as an alist entry in
;;   `elisp-scope-local-definitions', or globally, via the symbol
;;   property `elisp-scope-analyzer'.
;;
;;   An analyzer may use the functions `elisp-scope-report-s',
;;   `elisp-scope-1' and `elisp-scope-n' to analyze its arguments, and
;;   it can consult the variable `elisp-scope-output-spec' to obtain the
;;   expected output spec of the analyzed form.  For example, the
;;   following is a suitable analyzer for the `identity' function:
;;
;;     (lambda (fsym arg)
;;       (elisp-scope-report-s fsym 'function)
;;       (elisp-scope-1 arg elisp-scope-output-spec))
;;
;;   In particular, the analyzer function of `quote' analyzes its
;;   argument according to `elisp-scope-output-spec', which is bound to
;;   the value of the `outspec' argument passed to `elisp-scope-1'.
;;
;; - If HEAD is a macro, normally it is expanded, and then the
;;   expanded form is analyzed recursively.  Since macro-expansion may
;;   involve arbitrary code execution, only "safe" macro invocations are
;;   expanded: if HEAD is one of the macros in
;;   `elisp-scope-unsafe-macros', then it is never considered safe.
;;   Otherwise, HEAD is safe if it specified in the variable
;;   `elisp-scope-safe-macros'; or if it has a non-nil `safe-macro'
;;   symbol property; or if the current buffer is trusted according to
;;   `trusted-content-p'.  If a macro HEAD is not safe to expand (and
;;   has no associated analyzer function), then the macro arguments
;;   ARGS are not analyzed.
;;
;; - If HEAD is a function, it is reported as such, and ARGS are
;;   recursively analyzed as evaluated forms.
;;
;; - Otherwise, if HEAD has no associated analyzer function, and it is
;;   not a known macro or function, then it is reported with the `unknown'
;;   symbol role.  If the variable `elisp-scope-assume-func' is non-nil,
;;   then unknown HEAD is assumed to be a function call, and thus ARGS
;;   are analyzed as evaluated forms; otherwise ARGS are not analyzed.
;;
;; When `elisp-scope-1' encounters a variable reference VAR, it checks
;; whether VAR has a local binding in `elisp-scope-local-bindings', and
;; whether VAR is a known special variable.  If VAR is a locally-bound
;; special variable, `elisp-scope-1' reports the role `shadowed-variable'.
;; If VAR is locally-bound and not a special variable, it gets the role
;; `bound-variable'.  Lastly, if it not locally-bound, then it gets the
;; role `free-variable'.
;;
;; When analyzer functions invoke `elisp-scope-1/n' to analyze some
;; sub-forms, they specify the OUTSPEC argument to convey information
;; but the expected value of the evaluated sub-form(s), so
;; `elisp-scope-1/n' will know what to do with a sub-form that is just
;; (quoted) data.  For example, the analyzer function for
;; `face-attribute' calls `elisp-scope-1' to analyze its first argument
;; with an OUTSPEC which says that a quoted symbol in this position
;; refers to a face name.
;; That way, in a form such as (face-attribute 'default :foreground),
;; the symbol `default' is reported as a face reference (`face' role).
;; Moreover, the OUTSPEC is passed down as appropriate through various
;; predefined analyzers, so every quoted symbol in a "tail position" of
;; the first argument to `face-attribute' will also be recognized as a
;; face.  For instance, in the following form, both `success' and
;; `error' are reported as face references:
;;
;;   (face-attribute (if (something-p)
;;                       'success
;;                     (message "oops")
;;                     'error)
;;                   :foreground)
;;
;; See also the docstring of `elisp-scope-1' for details about the
;; format of the `outspec' argument.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun elisp-scope--define-symbol-role (name parents props)
  (put name 'elisp-scope-parent-roles parents)
  (put name 'elisp-scope-role-properties props))

(defmacro elisp-scope-define-symbol-role (name parents &rest props)
  "Define NAME as the name of a symbol role that inherits from PARENTS.

A symbol role is a symbol that Emacs uses to describe the role
of (other) symbols in Emacs Lisp source code.  For example, the symbol
role `face' characterizes symbols that are face names.

PROPS is a plist specifying the properties of the new symbol role NAME.
NAME inherits properties that do not appear in PROPS from its PARENTS.

Common symbol role properties are:

- `:doc': short documentation string describing this symbol role.
- `:face': face for highlighting symbols with this role.
- `:help': `help-echo' text for symbols with this role.

See also `elisp-scope-get-symbol-role-property' and
`elisp-scope-set-symbol-role-property' for getting and setting values of
symbol role properties."
  (declare (indent defun))
  `(elisp-scope--define-symbol-role ',name ',parents ,(when props `(list ,@props))))

;;;###autoload
(defun elisp-scope-get-symbol-role-property (role prop)
  "Return value of property PROP for symbol role ROLE."
  (seq-some
   (lambda (c) (plist-get (get c 'elisp-scope-role-properties) prop))
   (elisp-scope--all-reachable-symbol-roles role)))

(defvar elisp-scope--all-reachable-symbol-roles-cache (make-hash-table))

(defun elisp-scope--all-reachable-symbol-roles (symbol-role)
  (with-memoization (gethash symbol-role elisp-scope--all-reachable-symbol-roles-cache)
    (cons symbol-role
          (let* ((parents (get symbol-role 'elisp-scope-parent-roles))
                 (aps (mapcar #'elisp-scope--all-reachable-symbol-roles parents)))
            (if (cdr aps)
                (merge-ordered-lists (nconc aps (list parents)))
              (car aps))))))

;;;###autoload
(defun elisp-scope-set-symbol-role-property (role prop value)
  "Set value of property PROP for symbol role ROLE to VALUE."
  (put role 'elisp-scope-role-properties
       (plist-put (get role 'elisp-scope-role-properties) prop value)))

;;;###autoload
(defun elisp-scope-symbol-role-p (sym)
  "Check whether a symbol SYM is the name of a \"symbol role\"."
  (or (get sym 'elisp-scope-parent-roles) (get sym 'elisp-scope-role-properties)))

(defvar elisp-scope-read-symbol-role-history nil)

(defun elisp-scope-read-symbol-role (prompt &optional default)
  "Prompt with PROMPT for a symbol role.
DEFAULT is the minibuffer default argument."
  (completing-read
   (format-prompt prompt default)
   obarray #'elisp-scope-symbol-role-p 'confirm
   nil 'elisp-scope-read-symbol-role-history default))

(defvar describe-symbol-backends)

;;;###autoload
(defun elisp-scope-add-symbol-roles-to-describe-symbol ()
  (require 'help-mode)
  (setf
   (alist-get "symbol-role" describe-symbol-backends nil nil #'equal)
   `(,#'elisp-scope-symbol-role-p ,#'elisp-scope-describe-symbol-role)))

;;;###autoload
(defun elisp-scope-describe-symbol-role (role &rest _)
  "Describe ROLE of a symbol.
Interactively, prompt for ROLE."
  (interactive (list (elisp-scope-read-symbol-role
                      "Describe symbol role"
                      (when-let* ((def (symbol-at-point))
                                  ((elisp-scope-symbol-role-p def)))
                        def))))
  (when (stringp role) (setq role (intern role)))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'elisp-scope-describe-symbol-role role)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert "Symbol role "
                (substitute-quotes (concat "`" (symbol-name role) "'"))
                ":\n\n"
                (substitute-quotes
                 (or (elisp-scope-get-symbol-role-property role :doc)
                     "Undocumented.")))
        (when-let* ((parents (get role 'elisp-scope-parent-roles)))
          (insert "\n\nParent roles: "
                  (mapconcat (lambda (parent)
                               (let ((name (symbol-name parent)))
                                 (substitute-quotes
                                  (concat
                                   "`"
                                   (buttonize
                                    name #'elisp-scope-describe-symbol-role name
                                    "mouse-2, RET: describe this symbol role")
                                   "'"))))
                             parents ", ")))
        ;; Return the text we displayed for `describe-symbol-backends'.
        (buffer-string)))))

(elisp-scope-define-symbol-role symbol-role ()
  :doc "Symbol role names."
  :face 'elisp-symbol-role
  :help "Symbol role")

(elisp-scope-define-symbol-role symbol-role-definition (symbol-role)
  :doc "Symbol role name definitions."
  :face 'elisp-symbol-role-definition
  :help "Symbol role definition")

(elisp-scope-define-symbol-role variable ()
  :doc "Abstract symbol role of variables.")

(elisp-scope-define-symbol-role free-variable (variable)
  :doc "Variable names."
  :face 'elisp-free-variable
  :help (lambda (sym &rest _)
          (let ((val (if (boundp sym) (truncate-string-to-width (prin1-to-string (symbol-value sym)) 60 nil nil t) "#<unbound>")))
            (if-let* ((doc (documentation-property sym 'variable-documentation t)))
                (format "Special variable `%S'.\n\nValue: %s\n\n%s" sym val doc)
              (format "Special variable `%S'.\n\nValue: %s" sym val)))))

(elisp-scope-define-symbol-role bound-variable (variable)
  :doc "Local variable names."
  :face 'elisp-bound-variable
  :help "Local variable")

(elisp-scope-define-symbol-role binding-variable (bound-variable)
  :doc "Local variable definitions."
  :face 'elisp-binding-variable
  :help "Local variable binding")

(elisp-scope-define-symbol-role shadowed-variable (variable)
  :doc "Locally shadowed variable names."
  :face 'elisp-shadowed-variable
  :help "Locally shadowed variable")

(elisp-scope-define-symbol-role shadowing-variable (shadowed-variable)
  :doc "Locally shadowing variables."
  :face 'elisp-shadowing-variable
  :help "Local variable shadowing")

(elisp-scope-define-symbol-role face ()
  :doc "Face names."
  :face 'elisp-face
  :help (apply-partially #'elisp--help-echo 'face-documentation "Face"))

(elisp-scope-define-symbol-role callable ()
  :doc "Abstract symbol role of function-like symbols.")

(elisp-scope-define-symbol-role function (callable)
  :doc "Function names."
  :face 'elisp-function
  :help #'elisp--function-help-echo)

(elisp-scope-define-symbol-role command (function)
  :doc "Command names.")

(elisp-scope-define-symbol-role unknown (function)
  :doc "Unknown symbols at function position."
  :face 'elisp-unknown-call
  :help "Unknown callable")

(elisp-scope-define-symbol-role non-local-exit (function)
  :doc "Functions that do not return."
  :face 'elisp-non-local-exit)

(elisp-scope-define-symbol-role macro (callable)
  :doc "Macro names."
  :face 'elisp-macro
  :help #'elisp--function-help-echo)

(elisp-scope-define-symbol-role special-form (callable)
  :doc "Special form names."
  :face 'elisp-special-form
  :help #'elisp--function-help-echo)

(elisp-scope-define-symbol-role throw-tag ()
  :doc "Symbols used as `throw'/`catch' tags."
  :face 'elisp-throw-tag
  :help "`throw'/`catch' tag")

(elisp-scope-define-symbol-role warning-type ()
  :doc "Byte-compilation warning types."
  :face 'elisp-warning-type
  :help "Warning type")

(elisp-scope-define-symbol-role feature ()
  :doc "Feature names."
  :face 'elisp-feature
  :help "Feature")

(elisp-scope-define-symbol-role deffeature (feature)
  :doc "Feature definitions."
  :help "Feature definition")

(elisp-scope-define-symbol-role function-property-declaration ()
  :doc "Function/macro property declaration types."
  :face 'elisp-function-property-declaration
  :help "Function/macro property declaration")

(elisp-scope-define-symbol-role rx-construct ()
  :doc "`rx' constructs."
  :face 'elisp-rx
  :help "`rx' construct")

(elisp-scope-define-symbol-role theme ()
  :doc "Custom theme names."
  :face 'elisp-theme
  :help "Theme")

(elisp-scope-define-symbol-role deftheme (theme)
  :doc "Custom theme definitions."
  :help "Theme definition")

(elisp-scope-define-symbol-role thing ()
  :doc "`thing-at-point' \"thing\" identifiers."
  :face 'elisp-thing
  :help "Thing (text object)")

(elisp-scope-define-symbol-role slot ()
  :doc "EIEIO slots."
  :face 'elisp-slot
  :help "Slot")

(elisp-scope-define-symbol-role widget-type ()
  :doc "Widget types."
  :face 'elisp-widget-type
  :help "Widget type")

(elisp-scope-define-symbol-role widget-type-definition (widget-type)
  :doc "Widget type definitions."
  :help "Widget type definition")

(elisp-scope-define-symbol-role type ()
  :doc "Elisp object type names."
  :face 'elisp-type
  :help "Type")

(elisp-scope-define-symbol-role deftype (type)
  :doc "Elisp object type definitions."
  :help "Type definition")

(elisp-scope-define-symbol-role group ()
  :doc "Customization groups."
  :face 'elisp-group
  :help "Customization group")

(elisp-scope-define-symbol-role defgroup (group)
  :doc "Customization group definitions."
  :help "Customization group definition")

(elisp-scope-define-symbol-role nnoo-backend ()
  :doc "`nnoo' backend names."
  :face 'elisp-nnoo-backend
  :help "`nnoo' backend")

(elisp-scope-define-symbol-role condition ()
  :doc "`condition-case' conditions."
  :face 'elisp-condition
  :help (lambda (sym &rest _)
          (let ((msg (get sym 'error-message)))
            (apply #'concat
                   "`condition-case' condition"
                   (when (and msg (not (string-empty-p msg)))
                     `(": " ,msg))))))

(elisp-scope-define-symbol-role defcondition (condition)
  :doc "`condition-case' condition definitions."
  :help "`condition-case' condition definition")

(elisp-scope-define-symbol-role ampersand ()
  :doc "Argument list markers, such as `&optional' and `&rest'."
  :face 'elisp-ampersand
  :help "Arguments separator")

(elisp-scope-define-symbol-role constant ()
  :doc "Self-evaluating symbols."
  :face 'elisp-constant
  :help "Constant")

(elisp-scope-define-symbol-role defun ()
  :doc "Function definitions."
  :face 'elisp-defun
  :help "Function definition")

(elisp-scope-define-symbol-role defmacro ()
  :doc "Macro definitions."
  :face 'elisp-defmacro
  :help "Macro definition")

(elisp-scope-define-symbol-role defcmd (defun)
  :doc "Command definitions."
  :help "Command definition")

(elisp-scope-define-symbol-role defvar ()
  :doc "Variable definitions."
  :face 'elisp-defvar
  :help "Special variable definition")

(elisp-scope-define-symbol-role special-variable-declaration ()
  :doc "Special variable declarations."
  :face 'elisp-special-variable-declaration
  :help "Special variable declaration")

(elisp-scope-define-symbol-role defface ()
  :doc "Face definitions."
  :face 'elisp-defface
  :help "Face definition")

(elisp-scope-define-symbol-role major-mode ()
  :doc "Major mode names."
  :face 'elisp-major-mode-name
  :help (lambda (sym &rest _)
          (if-let* ((doc (documentation sym)))
              (format "Major mode `%S'.\n\n%s" sym doc)
            "Major mode")))

(elisp-scope-define-symbol-role major-mode-definition (major-mode)
  :doc "Major mode definitions."
  :help "Major mode definition")

(elisp-scope-define-symbol-role block ()
  :doc "`cl-block' block names."
  :help "Block")

(elisp-scope-define-symbol-role icon ()
  :doc "Icon names."
  :face 'elisp-icon
  :help "Icon")

(elisp-scope-define-symbol-role deficon ()
  :doc "Icon definitions."
  :face 'elisp-deficon
  :help "Icon definition")

(elisp-scope-define-symbol-role oclosure ()
  :doc "OClosure type names."
  :face 'elisp-oclosure
  :help (lambda (sym &rest _)
          (if-let* ((doc (oclosure--class-docstring (get sym 'cl--class))))
              (format "OClosure type `%S'.\n\n%s" sym doc)
            "OClosure type")))

(elisp-scope-define-symbol-role defoclosure ()
  :doc "OClosure type definitions."
  :face 'elisp-defoclosure
  :help "OClosure type definition")

(elisp-scope-define-symbol-role coding ()
  :doc "Coding-system names."
  :face 'elisp-coding
  :help (lambda (sym &rest _)
          (if-let* ((doc (coding-system-doc-string sym)))
              (format "Coding-system `%S'.\n\n%s" sym doc)
            "Coding-system")))

(elisp-scope-define-symbol-role defcoding ()
  :doc "Coding-system definitions."
  :face 'elisp-defcoding
  :help "Coding-system definition")

(elisp-scope-define-symbol-role charset ()
  :doc "Character set names."
  :face 'elisp-charset
  :help (lambda (sym &rest _)
          (if-let* ((doc (charset-description sym)))
              (format "Character set `%S'.\n\n%s" sym doc)
            "Character set")))

(elisp-scope-define-symbol-role defcharset ()
  :doc "Character set definitions."
  :face 'elisp-defcharset
  :help "Character set definition")

(elisp-scope-define-symbol-role completion-category ()
  :doc "Completion categories."
  :face 'elisp-completion-category
  :help (lambda (sym &rest _)
          (if-let* ((doc (get sym 'completion-category-documentation)))
              (format "Completion category `%S'.\n\n%s" sym doc)
            "Completion category")))

(elisp-scope-define-symbol-role completion-category-definition ()
  :doc "Completion category definitions."
  :face 'elisp-completion-category-definition
  :help "Completion category definition")

(defvar elisp-scope--counter nil)

(defvar elisp-scope-local-bindings nil
  "Alist of locally bound variables.

This is a list of cons cells (BOUND . BINDER), where BOUND is a symbol
which has a local variable binding in the current context, and BINDER
uniquely identifies the value that BOUND is bound to.  Usually, BINDER
is the buffer position in which BOUND is bound, such as a surrounding
`let' or `lambda' form.")

(defvar elisp-scope-output-spec nil
  "Output spec of the form currently analyzed, or nil if unknown.
See `elisp-scope-1' for possible values.")

(defvar elisp-scope--callback #'ignore
  "Function to call to report information about each analyzed symbol.")

(defsubst elisp-scope--local-new (sym pos &optional local)
  "Return new local context with SYM bound at POS.

Optional argument LOCAL is a local context to extend."
  (cons (cons sym (or pos (cons 'gen (incf elisp-scope--counter)))) local))

(defsubst elisp-scope--sym-pos (sym)
  (when (symbol-with-pos-p sym) (symbol-with-pos-pos sym)))

(defsubst elisp-scope--sym-bare (sym)
  (cond
   ((symbolp sym) sym)
   ((symbol-with-pos-p sym) (bare-symbol sym))))

(defvar elisp-scope--quoted nil)

(defsubst elisp-scope--report (role beg sym &optional id def)
  (funcall elisp-scope--callback role beg sym id (or def (and (numberp id) id))))

(defvar elisp-scope-special-variables nil
  "List of symbols that are special variables in the current analysis context.")

(defun elisp-scope--special-variable-p (sym)
  "Check whether SYM is a special variable in the current analysis context."
  (or (memq sym elisp-scope-special-variables) (special-variable-p sym)))

(defun elisp-scope--variable (sym beg id)
  (elisp-scope--report
   (if id (if (elisp-scope--special-variable-p sym) 'shadowed-variable 'bound-variable) 'free-variable)
   beg sym id))

(defun elisp-scope--binding (sym beg)
  (elisp-scope--report
   (if (elisp-scope--special-variable-p sym) 'shadowing-variable 'binding-variable)
   beg sym beg))

(defun elisp-scope--symbol (sym)
  "Analyze and report symbol SYM as a variable reference."
  (when-let* ((beg (elisp-scope--sym-pos sym))
              (bare (bare-symbol sym)))
    (if (keywordp bare) (elisp-scope--report 'constant beg bare)
      (unless (eq bare t) ; Do not report t as a variable.
        (elisp-scope--variable
         bare beg (alist-get bare elisp-scope-local-bindings))))))

(defun elisp-scope--let-1 (local bindings body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (elisp-scope--sym-bare sym))
             (beg (elisp-scope--sym-pos sym)))
        (when beg (elisp-scope--binding bare beg))
        (elisp-scope-1 (cadr binding))
        (elisp-scope--let-1 (if bare (elisp-scope--local-new bare beg local) local)
                            (cdr bindings) body))
    (let ((elisp-scope-local-bindings local))
      (elisp-scope-n body elisp-scope-output-spec))))

(defun elisp-scope-let (bindings body)
  "Analyze BINDINGS and BODY of a `let' form (let BINDINGS . BODY)."
  (elisp-scope--let-1 elisp-scope-local-bindings bindings body))

(defun elisp-scope-let* (bindings body)
  "Analyze BINDINGS and BODY of a `let*' form (let* BINDINGS . BODY)."
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (bare-symbol sym))
             (beg (elisp-scope--sym-pos sym)))
        (when beg (elisp-scope--binding bare beg))
        (elisp-scope-1 (cadr binding))
        (let ((elisp-scope-local-bindings (elisp-scope--local-new bare beg elisp-scope-local-bindings)))
          (elisp-scope-let* (cdr bindings) body)))
    (elisp-scope-n body elisp-scope-output-spec)))

(defun elisp-scope-interactive (intr spec modes)
  (when (symbol-with-pos-p intr)
    (elisp-scope--report 'special-form (symbol-with-pos-pos intr) (bare-symbol intr)))
  (elisp-scope-1 spec)
  (mapc #'elisp-scope-major-mode-name modes))

(defun elisp-scope-lambda (args body &optional outspec)
  (let ((l elisp-scope-local-bindings))
    (when (listp args)
      (dolist (arg args)
        (when-let* ((bare (bare-symbol arg))
                    (beg (elisp-scope--sym-pos arg)))
          (unless (memq bare '(&optional &rest))
            (setq l (elisp-scope--local-new bare beg l))))))
    ;; Handle docstring.
    (cond
     ((and (consp (car body))
           (or (symbol-with-pos-p (caar body))
               (symbolp (caar body)))
           (eq (bare-symbol (caar body)) :documentation))
      (elisp-scope--symbol (caar body))
      (elisp-scope-1 (cadar body))
      (setq body (cdr body)))
     ((stringp (car body)) (setq body (cdr body))))
    ;; Handle `declare'.
    (when-let* ((form (car body))
                (decl (car-safe form))
                ((or (symbol-with-pos-p decl)
                     (symbolp decl)))
                (bare (bare-symbol decl))
                ((eq bare 'declare)))
      (when (symbol-with-pos-p decl)
        (elisp-scope--report 'macro (symbol-with-pos-pos decl) bare))
      (dolist (spec (cdr form))
        (when-let* ((head (car-safe spec))
                    (bare (elisp-scope--sym-bare head)))
          (when (symbol-with-pos-p head)
            (elisp-scope--report 'function-property-declaration (symbol-with-pos-pos head) bare))
          (cl-case bare
            (completion (elisp-scope-sharpquote (cadr spec)))
            (interactive-only
             (when-let* ((bare (elisp-scope--sym-bare (cadr spec)))
                         ((not (eq bare t))))
               (elisp-scope-sharpquote (cadr spec))))
            (obsolete
             (when-let* ((bare (elisp-scope--sym-bare (cadr spec))))
               (elisp-scope-sharpquote (cadr spec))))
            ((compiler-macro gv-expander gv-setter)
             ;; Use the extended lexical environment `l'.
             (let ((elisp-scope-local-bindings l))
               (elisp-scope-sharpquote (cadr spec))))
            (modes (mapc #'elisp-scope-major-mode-name (cdr spec)))
            (interactive-args
             (dolist (arg-form (cdr spec))
               (when-let* ((arg (car-safe arg-form)))
                 (let ((elisp-scope-local-bindings l)) (elisp-scope--symbol arg))
                 (when (consp (cdr arg-form))
                   (elisp-scope-1 (cadr arg-form)))))))))
      (setq body (cdr body)))
    ;; Handle `interactive'.
    (when-let* ((form (car body))
                (intr (car-safe form))
                ((or (symbol-with-pos-p intr)
                     (symbolp intr)))
                ((eq (bare-symbol intr) 'interactive)))
      (elisp-scope-interactive intr (cadar body) (cddar body))
      (setq body (cdr body)))
    ;; Handle ARGS.
    (when (listp args)
      (dolist (arg args)
        (and (symbol-with-pos-p arg)
             (let* ((beg (symbol-with-pos-pos arg))
                    (bare (bare-symbol arg)))
               (when (and beg (not (eq bare '_)))
                 (if (memq bare '(&optional &rest))
                     (elisp-scope--report 'ampersand beg bare)
                   (elisp-scope--report 'binding-variable beg bare beg)))))))
    ;; Handle BODY.
    (let ((elisp-scope-local-bindings l)) (elisp-scope-n body outspec))))

(defun elisp-scope-defun (name args body)
  (when-let* ((beg (elisp-scope--sym-pos name))
              (bare (elisp-scope--sym-bare name)))
    (elisp-scope--report
     (let ((tmp body))
       (when (stringp (car-safe tmp)) (pop tmp))
       (when (eq 'declare (elisp-scope--sym-bare (car-safe (car-safe tmp)))) (pop tmp))
       (if (eq 'interactive (elisp-scope--sym-bare (car-safe (car-safe tmp))))
           'defcmd
         'defun))
     beg bare))
  (elisp-scope-lambda args body))

(defun elisp-scope-setq (args) (elisp-scope-n args elisp-scope-output-spec))

(defvar elisp-scope-local-definitions nil
  "Alist associating (local) analyzer functions to function/macro names.")

(defmacro elisp-scope-with-local-definition (sym af &rest body)
  "Execute BODY with analyzer function AF associated to function/macro SYM."
  (declare (indent 2) (debug t))
  `(let ((elisp-scope-local-definitions
          (cons (cons ,sym ,af) elisp-scope-local-definitions)))
     ,@body))

(defun elisp-scope-flet (defs body outspec)
  (if defs
      (let* ((def (car defs))
             (func (car def))
             (exps (cdr def))
             (beg (elisp-scope--sym-pos func))
             (bare (bare-symbol func)))
        (when beg
          ;; TODO: Use a bespoke 'local-function-definition' role.
          (elisp-scope--report 'function beg bare beg))
        (if (cdr exps)
            ;; def is (FUNC ARGLIST BODY...)
            (elisp-scope-cl-lambda (car exps) (cdr exps))
          ;; def is (FUNC EXP)
          (elisp-scope-1 (car exps)))
        (let ((pos (or beg (cons 'gen (incf elisp-scope--counter)))))
          (elisp-scope-with-local-definition bare
              (elisp-scope--local-function-analyzer pos)
            (elisp-scope-flet (cdr defs) body outspec))))
    (elisp-scope-n body outspec)))

(defun elisp-scope--local-function-analyzer (pos)
  (lambda (f &rest args)
    (when (symbol-with-pos-p f)
      (elisp-scope--report 'function (symbol-with-pos-pos f) (bare-symbol f) pos))
    (elisp-scope-n args)))

(defun elisp-scope-labels (defs forms outspec)
  (if defs
      (let* ((def (car defs))
             (func (car def))
             (args (cadr def))
             (body (cddr def))
             (beg (elisp-scope--sym-pos func))
             (bare (bare-symbol func)))
        (when beg
          (elisp-scope--report 'function beg bare beg))
        (let ((pos (or beg (cons 'gen (incf elisp-scope--counter)))))
          (elisp-scope-with-local-definition bare
              (elisp-scope--local-function-analyzer pos)
            (elisp-scope-lambda args body)
            (elisp-scope-flet (cdr defs) forms outspec))))
    (elisp-scope-n forms outspec)))

(defvar elisp-scope-block-alist nil)

(defun elisp-scope-block (name body)
  (if name
      (let* ((beg (elisp-scope--sym-pos name))
             (bare (bare-symbol name)))
        (when beg
          (elisp-scope--report 'block beg bare beg))
        (let ((elisp-scope-block-alist (elisp-scope--local-new bare beg elisp-scope-block-alist)))
          (elisp-scope-n body)))
    (elisp-scope-n body)))

(defun elisp-scope-return-from (name result)
  (when-let* ((bare (and (symbol-with-pos-p name) (bare-symbol name)))
              (pos (alist-get bare elisp-scope-block-alist)))
    (elisp-scope--report 'block (symbol-with-pos-pos name) bare pos))
  (elisp-scope-1 result))

(defvar elisp-scope-assume-func nil)

(defun elisp-scope-sharpquote (arg)
  (cond
   ((or (symbol-with-pos-p arg) (symbolp arg))
    (let ((bare (bare-symbol arg)))
      (cond
       ((or (functionp bare)
            (assq bare elisp-scope-local-definitions)
            elisp-scope-assume-func)
        (elisp-scope-report-s arg 'function))
       (t (elisp-scope-report-s arg 'unknown)))))
   ((consp arg) (elisp-scope-1 arg))))

(defun elisp-scope-loop-for-and (rest)
  (if (eq (elisp-scope--sym-bare (car rest)) 'and)
      (elisp-scope-loop-for elisp-scope-local-bindings (cadr rest) (cddr rest))
    (elisp-scope-loop rest)))

(defun elisp-scope-loop-for-by (local expr rest)
  (elisp-scope-1 expr)
  (let ((elisp-scope-local-bindings local))
    (elisp-scope-loop-for-and rest)))

(defun elisp-scope-loop-for-to (local expr rest)
  (elisp-scope-1 expr)
  (when-let* ((bare (elisp-scope--sym-bare (car rest)))
              (more (cdr rest)))
    (cond
     ((eq bare 'by)
      (elisp-scope-loop-for-by local (car more) (cdr more)))
     (t (let ((elisp-scope-local-bindings local))
          (elisp-scope-loop-for-and rest))))))

(defun elisp-scope-loop-for-from (local expr rest)
  (elisp-scope-1 expr)
  (when-let* ((bare (elisp-scope--sym-bare (car rest)))
              (more (cdr rest)))
    (cond
     ((memq bare '(to upto downto below above))
      (elisp-scope-loop-for-to local (car more) (cdr more)))
     ((eq bare 'by)
      (elisp-scope-loop-for-by local (car more) (cdr more)))
     (t (let ((elisp-scope-local-bindings local))
          (elisp-scope-loop-for-and rest))))))

(defun elisp-scope-loop-for-= (local expr rest)
  (elisp-scope-1 expr)
  (when-let* ((bare (elisp-scope--sym-bare (car rest)))
              (more (cdr rest)))
    (cond
     ((eq bare 'then)
      (elisp-scope-loop-for-by local (car more) (cdr more)))
     (t (let ((elisp-scope-local-bindings local))
          (elisp-scope-loop-for-and rest))))))

(defun elisp-scope-loop-for-being-the-hash-keys-of-using (form rest)
  (let* ((var (cadr form))
         (bare (elisp-scope--sym-bare var))
         (beg (elisp-scope--sym-pos var)))
    (when beg (elisp-scope--binding bare beg))
    (let ((elisp-scope-local-bindings (elisp-scope--local-new bare beg elisp-scope-local-bindings)))
      (elisp-scope-loop-for-and rest))))

(defun elisp-scope-loop-for-being-the-hash-keys-of (local expr rest)
  (elisp-scope-1 expr)
  (when-let* ((bare (elisp-scope--sym-bare (car rest)))
              (more (cdr rest)))
    (let ((elisp-scope-local-bindings local))
      (cond
       ((eq bare 'using)
        (elisp-scope-loop-for-being-the-hash-keys-of-using (car more) (cdr more)))
       (t (elisp-scope-loop-for-and rest))))))

(defun elisp-scope-loop-for-being-the-hash-keys (local word rest)
  (when-let* ((bare (elisp-scope--sym-bare word)))
    (cond
     ((eq bare 'of)
      (elisp-scope-loop-for-being-the-hash-keys-of local (car rest) (cdr rest))))))

(defun elisp-scope-loop-for-being-the (local word rest)
  (when-let* ((bare (elisp-scope--sym-bare word)))
    (cond
     ((memq bare '(buffer buffers))
      (let ((elisp-scope-local-bindings local))
        (elisp-scope-loop-for-and rest)))
     ((memq bare '( hash-key hash-keys
                    hash-value hash-values
                    key-code key-codes
                    key-binding key-bindings))
      (elisp-scope-loop-for-being-the-hash-keys local (car rest) (cdr rest))))))

(defun elisp-scope-loop-for-being (local next rest)
  (elisp-scope-loop-for-being-the
   local (car rest)
   (if (memq (elisp-scope--sym-bare next) '(the each)) (cdr rest) rest)))

(defun elisp-scope-loop-for (local vars rest)
  (if vars
      ;; FIXME: var need not be a symbol, see
      ;; `cl-macs-loop-destructure-cons' test in cl-macs-tests.el.
      (let* ((var (car (ensure-list vars)))
             (bare (bare-symbol var))
             (beg (elisp-scope--sym-pos var)))
        (when beg (elisp-scope--binding bare beg))
        (elisp-scope-loop-for (elisp-scope--local-new bare beg local) (cdr-safe vars) rest))
    (when-let* ((bare (elisp-scope--sym-bare (car rest)))
                (more (cdr rest)))
      (cond
       ((memq bare '(from upfrom downfrom))
        (elisp-scope-loop-for-from local (car more) (cdr more)))
       ((memq bare '( to upto downto below above
                      in on in-ref))
        (elisp-scope-loop-for-to local (car more) (cdr more)))
       ((memq bare '(by
                     across across-ref))
        (elisp-scope-loop-for-by local (car more) (cdr more)))
       ((eq bare '=)
        (elisp-scope-loop-for-= local (car more) (cdr more)))
       ((eq bare 'being)
        (elisp-scope-loop-for-being local (car more) (cdr more)))))))

(defun elisp-scope-loop-repeat (form rest)
  (elisp-scope-1 form)
  (elisp-scope-loop rest))

(defvar elisp-scope-loop-into-vars nil)

(defun elisp-scope-loop-collect (expr rest)
  (elisp-scope-1 expr)
  (let ((bw (elisp-scope--sym-bare (car rest)))
        (more (cdr rest)))
    (if (eq bw 'into)
        (let* ((var (car more))
               (bare (elisp-scope--sym-bare var))
               (beg (elisp-scope--sym-pos var)))
          (if (memq bare elisp-scope-loop-into-vars)
              (progn
                (elisp-scope--symbol var)
                (elisp-scope-loop (cdr more)))
            (when beg (elisp-scope--binding bare beg))
            (let ((elisp-scope-loop-into-vars (cons bare elisp-scope-loop-into-vars))
                  (elisp-scope-local-bindings (elisp-scope--local-new bare beg elisp-scope-local-bindings)))
              (elisp-scope-loop (cdr more)))))
      (elisp-scope-loop rest))))

(defun elisp-scope-loop-with-and (rest)
  (if (eq (elisp-scope--sym-bare (car rest)) 'and)
      (elisp-scope-loop-with (cadr rest) (cddr rest))
    (elisp-scope-loop rest)))

(defun elisp-scope-loop-with (var rest)
  (let* ((bare (elisp-scope--sym-bare var))
         (beg (symbol-with-pos-pos var))
         (l (elisp-scope--local-new bare beg elisp-scope-local-bindings))
         (eql (car rest)))
    (when beg (elisp-scope--binding bare beg))
    (if (eq (elisp-scope--sym-bare eql) '=)
        (let* ((val (cadr rest)) (more (cddr rest)))
          (elisp-scope-1 val)
          (let ((elisp-scope-local-bindings l))
            (elisp-scope-loop-with-and more)))
      (let ((elisp-scope-local-bindings l))
        (elisp-scope-loop-with-and rest)))))

(defun elisp-scope-loop-do (form rest)
  (elisp-scope-1 form)
  (if (consp (car rest))
      (elisp-scope-loop-do (car rest) (cdr rest))
    (elisp-scope-loop rest)))

(defun elisp-scope-loop-named (name rest)
  (let* ((beg (elisp-scope--sym-pos name))
         (bare (elisp-scope--sym-bare name)))
    (when beg
      (elisp-scope--report 'block beg bare beg))
    (let ((elisp-scope-block-alist (elisp-scope--local-new bare beg elisp-scope-block-alist)))
      (elisp-scope-loop rest))))

(defun elisp-scope-loop-finally (next rest)
  (if-let* ((bare (elisp-scope--sym-bare next)))
      (cond
       ((eq bare 'do)
        (elisp-scope-loop-do (car rest) (cdr rest)))
       ((eq bare 'return)
        (elisp-scope-1 (car rest))
        (elisp-scope-loop (cdr rest))))
    (if (eq (elisp-scope--sym-bare (car-safe next)) 'return)
        (progn
          (elisp-scope-1 (cadr next))
          (elisp-scope-loop (cdr rest)))
      (elisp-scope-loop-do next rest))))

(defun elisp-scope-loop-initially (next rest)
  (if (eq (elisp-scope--sym-bare next) 'do)
      (elisp-scope-loop-do (car rest) (cdr rest))
    (elisp-scope-loop-do next rest)))

(defvar elisp-scope-loop-if-depth 0)

(defun elisp-scope-loop-if (keyword condition rest)
  (elisp-scope-1 condition)
  (let ((elisp-scope-loop-if-depth (1+ elisp-scope-loop-if-depth))
        (elisp-scope-local-bindings
         ;; `if' binds `it'.
         (elisp-scope--local-new 'it (elisp-scope--sym-pos keyword) elisp-scope-local-bindings)))
    (elisp-scope-loop rest)))

(defun elisp-scope-loop-end (rest)
  (let ((elisp-scope-loop-if-depth (1- elisp-scope-loop-if-depth)))
    (unless (minusp elisp-scope-loop-if-depth)
      (elisp-scope-loop rest))))

(defun elisp-scope-loop-and (rest)
  (when (plusp elisp-scope-loop-if-depth) (elisp-scope-loop rest)))

(defun elisp-scope-loop (forms)
  (when forms
    (let* ((kw (car forms))
           (bare (elisp-scope--sym-bare kw))
           (rest (cdr forms)))
      (cond
       ((memq bare '(for as))
        (elisp-scope-loop-for elisp-scope-local-bindings (car rest) (cdr rest)))
       ((memq bare '( repeat while until always never thereis iter-by
                      return))
        (elisp-scope-loop-repeat (car rest) (cdr rest)))
       ((memq bare '(collect append nconc concat vconcat count sum maximize minimize))
        (elisp-scope-loop-collect (car rest) (cdr rest)))
       ((memq bare '(with))
        (elisp-scope-loop-with (car rest) (cdr rest)))
       ((memq bare '(do)) (elisp-scope-loop-do (car rest) (cdr rest)))
       ((memq bare '(named)) (elisp-scope-loop-named (car rest) (cdr rest)))
       ((memq bare '(finally)) (elisp-scope-loop-finally (car rest) (cdr rest)))
       ((memq bare '(initially)) (elisp-scope-loop-initially (car rest) (cdr rest)))
       ((memq bare '(if when unless)) (elisp-scope-loop-if kw (car rest) (cdr rest)))
       ((memq bare '(end)) (elisp-scope-loop-end rest))
       ((memq bare '(and else)) (elisp-scope-loop-and rest))))))

(defun elisp-scope-named-let (name bindings body &optional outspec)
  (let ((bare (elisp-scope--sym-bare name))
        (beg (elisp-scope--sym-pos name)))
    (when beg
      (elisp-scope--report 'function beg bare beg))
    (dolist (binding bindings)
      (let* ((sym (car (ensure-list binding)))
             (beg (symbol-with-pos-pos sym))
             (bare (bare-symbol sym)))
        (when beg (elisp-scope--binding bare beg))
        (elisp-scope-1 (cadr binding))))
    (let ((l elisp-scope-local-bindings))
      (dolist (binding bindings)
        (when-let* ((sym (car (ensure-list binding)))
                    (bare (elisp-scope--sym-bare sym)))
          (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos sym) l))))
      (let ((pos (or beg (cons 'gen (incf elisp-scope--counter)))))
        (elisp-scope-with-local-definition bare
            (elisp-scope--local-function-analyzer pos)
          (let ((elisp-scope-local-bindings l)) (elisp-scope-n body outspec)))))))

(defun elisp-scope-rx (regexps)
  (dolist (regexp regexps) (elisp-scope-rx-1 regexp)))

(defvar elisp-scope-rx-alist nil)

(defun elisp-scope-rx-1 (regexp)
  (if (consp regexp)
      (let* ((head (car regexp))
             (bare (elisp-scope--sym-bare head)))
        (when (and bare (symbol-with-pos-p head))
          (elisp-scope--report 'rx-construct (symbol-with-pos-pos head) bare (alist-get bare elisp-scope-rx-alist)))
        (cond
         ((memq bare '(literal regex regexp eval))
          (elisp-scope-1 (cadr regexp)))
         ((memq bare '( seq sequence and :
                        or |
                        zero-or-more 0+ * *?
                        one-or-more 1+ + +?
                        zero-or-one optional opt \? \??
                        = >= ** repeat
                        minimal-match maximal-match
                        group submatch
                        group-n submatch-n))
          (elisp-scope-rx (cdr regexp)))))
    (when-let* (((symbol-with-pos-p regexp))
                (bare (elisp-scope--sym-bare regexp)))
      (elisp-scope--report 'rx-construct (symbol-with-pos-pos regexp) bare (alist-get bare elisp-scope-rx-alist)))))

(defun elisp-scope-rx-define (name rest)
  (when-let* ((bare (elisp-scope--sym-bare name)))
    (elisp-scope--report 'rx-construct (symbol-with-pos-pos name) bare))
  (if (not (cdr rest))
      (elisp-scope-rx-1 (car rest))
    (let ((l elisp-scope-rx-alist)
          (args (car rest))
          (rx (cadr rest)))
      (dolist (arg args)
        (and (symbol-with-pos-p arg)
             (when-let* ((beg (symbol-with-pos-pos arg))
                         (bare (bare-symbol arg)))
               (if (memq bare '(&optional &rest _))
                   (elisp-scope--report 'ampersand beg bare)
                 (elisp-scope--report 'rx-construct beg bare beg)))))
      (dolist (arg args)
        (when-let* ((bare (bare-symbol arg))
                    (beg (elisp-scope--sym-pos arg)))
          (unless (memq bare '(&optional &rest))
            (setq l (elisp-scope--local-new bare beg l)))))
      (let ((elisp-scope-rx-alist l))
        (elisp-scope-rx-1 rx)))))

(defun elisp-scope-rx-let (bindings body)
  (if-let* ((binding (car bindings)))
      (let ((name (car binding)) (rest (cdr binding)))
        (when-let* ((bare (elisp-scope--sym-bare name))
                    (beg (symbol-with-pos-pos name)))
          (elisp-scope--report 'rx-construct beg bare beg))
        (if (cdr rest)
            (let ((l elisp-scope-rx-alist)
                  (args (car rest))
                  (rx (cadr rest)))
              (dolist (arg args)
                (and (symbol-with-pos-p arg)
                     (when-let* ((beg (symbol-with-pos-pos arg))
                                 (bare (bare-symbol arg)))
                       (if (memq bare '(&optional &rest _))
                           (elisp-scope--report 'ampersand beg bare)
                         (elisp-scope--report 'rx-construct beg bare beg)))))
              (dolist (arg args)
                (when-let* ((bare (bare-symbol arg))
                            (beg (elisp-scope--sym-pos arg)))
                  (unless (memq bare '(&optional &rest))
                    (setq l (elisp-scope--local-new bare beg l)))))
              (let ((elisp-scope-rx-alist l))
                (elisp-scope-rx-1 rx))
              (let ((elisp-scope-rx-alist (elisp-scope--local-new (elisp-scope--sym-bare name)
                                                                  (elisp-scope--sym-pos name)
                                                                  elisp-scope-rx-alist)))
                (elisp-scope-rx-let (cdr bindings) body)))
          (elisp-scope-rx-1 (car rest))
          (let ((elisp-scope-rx-alist (elisp-scope--local-new (elisp-scope--sym-bare name)
                                                              (elisp-scope--sym-pos name)
                                                              elisp-scope-rx-alist)))
            (elisp-scope-rx-let (cdr bindings) body))))
    (elisp-scope-n body)))

(defun elisp-scope-gv-define-expander (name handler)
  (when-let* ((beg (elisp-scope--sym-pos name)) (bare (elisp-scope--sym-bare name)))
    (elisp-scope--report 'defun beg bare))
  (elisp-scope-1 handler))

(defun elisp-scope-gv-define-simple-setter (name setter rest)
  (when-let* ((beg (elisp-scope--sym-pos name)) (bare (elisp-scope--sym-bare name)))
    (elisp-scope--report 'defun beg bare))
  (when-let* ((beg (elisp-scope--sym-pos setter)) (bare (elisp-scope--sym-bare setter)))
    (elisp-scope--report 'function beg bare))
  (elisp-scope-n rest))

(defun elisp-scope-deftype (name args body)
  (when-let* ((beg (elisp-scope--sym-pos name)) (bare (elisp-scope--sym-bare name)))
    (elisp-scope--report 'deftype beg bare))
  (elisp-scope-lambda args body))

(defun elisp-scope-defmethod-1 (local args body)
  (if args
      (let ((arg (car args)) (bare nil))
        (cond
         ((consp arg)
          (let* ((var (car arg))
                 (spec (cadr arg)))
            (cond
             ((setq bare (elisp-scope--sym-bare var))
              (when-let* ((beg (elisp-scope--sym-pos var)))
                (elisp-scope--binding bare beg))
              (cond
               ((consp spec)
                (let ((head (car spec)) (form (cadr spec)))
                  (and (eq 'eql (elisp-scope--sym-bare head))
                       (not (or (symbolp form) (symbol-with-pos-p form)))
                       (elisp-scope-1 form))))
               ((symbol-with-pos-p spec)
                (when-let* ((beg (symbol-with-pos-pos spec))
                            (bare (bare-symbol spec)))
                  (elisp-scope--report 'type beg bare))))
              (elisp-scope-defmethod-1 (elisp-scope--local-new bare (elisp-scope--sym-pos var) local)
                                       (cdr args) body)))))
         ((setq bare (elisp-scope--sym-bare arg))
          (cond
           ((memq bare '(&optional &rest &body _))
            (when-let* ((beg (elisp-scope--sym-pos arg)))
              (elisp-scope--report 'ampersand beg bare))
            (elisp-scope-defmethod-1 local (cdr args) body))
           ((eq bare '&context)
            (let* ((expr-type (cadr args))
                   (expr (car expr-type))
                   (spec (cadr expr-type))
                   (more (cddr args)))
              (when-let* ((beg (elisp-scope--sym-pos arg)))
                (elisp-scope--report 'ampersand beg bare))
              (elisp-scope-1 expr)
              (cond
               ((consp spec)
                (let ((head (car spec)) (form (cadr spec)))
                  (and (eq 'eql (elisp-scope--sym-bare head))
                       (not (or (symbolp form) (symbol-with-pos-p form)))
                       (elisp-scope-1 form))))
               ((symbol-with-pos-p spec)
                (when-let* ((beg (symbol-with-pos-pos spec))
                            (bare (bare-symbol spec)))
                  (elisp-scope--report 'type beg bare beg))))
              (elisp-scope-defmethod-1 local more body)))
           (t
            (when-let* ((beg (elisp-scope--sym-pos arg)))
              (elisp-scope--binding bare beg))
            (elisp-scope-defmethod-1 (elisp-scope--local-new bare (elisp-scope--sym-pos arg) local)
                                     (cdr args) body))))))
    (let ((elisp-scope-local-bindings local))
      (elisp-scope-n body))))

;; (defun elisp-scope-defmethod (local name rest)
;;   (when (and (symbol-with-pos-p (car rest))
;;              (eq (bare-symbol (car rest)) :extra))
;;     (setq rest (cddr rest)))
;;   (when (and (symbol-with-pos-p (car rest))
;;              (memq (bare-symbol (car rest)) '(:before :after :around)))
;;     (setq rest (cdr rest)))
;;   (elisp-scope-defmethod-1 local local name (car rest)
;;                      (if (stringp (cadr rest)) (cddr rest) (cdr rest))))

(defun elisp-scope-defmethod (name rest)
  (when-let* ((beg (elisp-scope--sym-pos name)) (bare (elisp-scope--sym-bare name)))
    (elisp-scope--report 'defun beg bare))
  ;; [EXTRA]
  (when (eq (elisp-scope--sym-bare (car rest)) :extra)
    (elisp-scope--symbol (car rest))
    (setq rest (cddr rest)))
  ;; [QUALIFIER]
  (when (keywordp (elisp-scope--sym-bare (car rest)))
    (elisp-scope--symbol (car rest))
    (setq rest (cdr rest)))
  ;; ARGUMENTS
  (elisp-scope-defmethod-1 elisp-scope-local-bindings (car rest) (cdr rest)))

(defun elisp-scope-cl-defun (name arglist body)
  (let ((beg (elisp-scope--sym-pos name))
        (bare (elisp-scope--sym-bare name)))
    (when beg (elisp-scope--report 'defun beg bare))
    (let ((elisp-scope-block-alist (elisp-scope--local-new bare beg elisp-scope-block-alist)))
      (elisp-scope-cl-lambda arglist body))))

(defun elisp-scope-cl-lambda (arglist body)
  (elisp-scope-cl-lambda-1 arglist nil body))

(defun elisp-scope-cl-lambda-1 (arglist more body)
  (cond
   (arglist
    (if (consp arglist)
        (let ((head (car arglist)))
          (if (consp head)
              (elisp-scope-cl-lambda-1 head (cons (cdr arglist) more) body)
            (let ((bare (elisp-scope--sym-bare head)))
              (if (memq bare '(&optional &rest &body &key &aux &whole &cl-defs &cl-quote))
                  (progn
                    (when-let* ((beg (elisp-scope--sym-pos head)))
                      (elisp-scope--report 'ampersand beg bare))
                    (cl-case bare
                      (&optional (elisp-scope-cl-lambda-optional (cadr arglist) (cddr arglist) more body))
                      (&cl-defs (elisp-scope-cl-lambda-defs (cadr arglist) (cddr arglist) more body))
                      ((&rest &body) (elisp-scope-cl-lambda-rest (cadr arglist) (cddr arglist) more body))
                      (&key (elisp-scope-cl-lambda-key (cadr arglist) (cddr arglist) more body))
                      (&aux (elisp-scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body))
                      (&whole (elisp-scope-cl-lambda-1 (cdr arglist) more body))))
                (when-let* ((beg (elisp-scope--sym-pos head)))
                  (elisp-scope--binding bare beg))
                (let ((elisp-scope-local-bindings
                       (elisp-scope--local-new bare (elisp-scope--sym-pos head)
                                               elisp-scope-local-bindings)))
                  (elisp-scope-cl-lambda-1 (cdr arglist) more body))))))
      (elisp-scope-cl-lambda-1 (list '&rest arglist) more body)))
   (more (elisp-scope-cl-lambda-1 (car more) (cdr more) body))
   (t (elisp-scope-lambda nil body))))

(defun elisp-scope-cl-lambda-defs (arg arglist more body)
  (when (consp arg)
    (let ((def (car arg))
          (defs (cdr arg)))
      (elisp-scope-1 def)
      (dolist (d defs) (elisp-scope-n (cdr-safe d)))))
  (elisp-scope-cl-lambda-1 arglist more body))

(defun elisp-scope-cl-lambda-optional (arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l elisp-scope-local-bindings)
         (init (cadr a))
         (svar (caddr a)))
    (elisp-scope-1 init)
    (if (consp var)
        (let ((elisp-scope-local-bindings l))
          (elisp-scope-cl-lambda-1 var (cons (append (when svar (list svar))
                                                     (cons '&optional arglist))
                                             more)
                                   body))
      (when-let* ((bare (elisp-scope--sym-bare svar)))
        (when-let* ((beg (elisp-scope--sym-pos svar)))
          (elisp-scope--binding bare beg))
        (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos svar) l)))
      (when-let* ((bare (elisp-scope--sym-bare var)))
        (when-let* ((beg (elisp-scope--sym-pos var)))
          (elisp-scope--binding bare beg))
        (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let* ((bare (elisp-scope--sym-bare head))
                    ((memq bare '(&rest &body &key &aux))))
              (progn
                (when-let* ((beg (elisp-scope--sym-pos head)))
                  (elisp-scope--report 'ampersand beg bare))
                (cl-case bare
                  ((&rest &body)
                   (let ((elisp-scope-local-bindings l))
                     (elisp-scope-cl-lambda-rest (cadr arglist) (cddr arglist) more body)))
                  (&key (let ((elisp-scope-local-bindings l))
                          (elisp-scope-cl-lambda-key (cadr arglist) (cddr arglist) more body)))
                  (&aux (let ((elisp-scope-local-bindings l))
                          (elisp-scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body)))))
            (let ((elisp-scope-local-bindings l))
              (elisp-scope-cl-lambda-optional head (cdr arglist) more body)))))
       (more
        (let ((elisp-scope-local-bindings l))
          (elisp-scope-cl-lambda-1 (car more) (cdr more) body)))
       (t (let ((elisp-scope-local-bindings l)) (elisp-scope-lambda nil body)))))))

(defun elisp-scope-cl-lambda-rest (var arglist more body)
  (let* ((l elisp-scope-local-bindings))
    (if (consp var)
        (elisp-scope-cl-lambda-1 var (cons arglist more) body)
      (when-let* ((bare (elisp-scope--sym-bare var)))
        (when-let* ((beg (elisp-scope--sym-pos var)))
          (elisp-scope--binding bare beg))
        (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos var) l)))
      (cond
       (arglist
        (let* ((head (car arglist))
               (bare (elisp-scope--sym-bare head)))
          (if (memq bare '(&key &aux))
              (progn
                (when-let* ((beg (elisp-scope--sym-pos head)))
                  (elisp-scope--report 'ampersand beg bare))
                (cl-case bare
                  (&key
                   (let ((elisp-scope-local-bindings l))
                     (elisp-scope-cl-lambda-key (cadr arglist) (cddr arglist) more body)))
                  (&aux
                   (let ((elisp-scope-local-bindings l))
                     (elisp-scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body)))))
            (let ((elisp-scope-local-bindings l))
              (elisp-scope-cl-lambda-1 (car more) (cdr more) body)))))
       (more (let ((elisp-scope-local-bindings l))
               (elisp-scope-cl-lambda-1 (car more) (cdr more) body)))
       (t (let ((elisp-scope-local-bindings l))
            (elisp-scope-lambda nil body)))))))

(defun elisp-scope-cl-lambda-key (arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l elisp-scope-local-bindings)
         (init (cadr a))
         (svar (caddr a))
         (kw (car-safe var)))
    (elisp-scope-1 init)
    (and kw (or (symbolp kw) (symbol-with-pos-p kw))
         (cadr var)
         (not (cddr var))
         ;; VAR is (KEYWORD VAR)
         (setq var (cadr var)))
    (when-let* ((bare (elisp-scope--sym-bare kw))
                ((keywordp bare)))
      (when-let* ((beg (elisp-scope--sym-pos kw)))
        (elisp-scope--report 'constant beg bare))
      (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos svar) l)))
    (if (consp var)
        (let ((elisp-scope-local-bindings l))
          (elisp-scope-cl-lambda-1 var (cons (append (when svar (list svar))
                                                     (cons '&key arglist))
                                             more)
                                   body))
      (when-let* ((bare (elisp-scope--sym-bare svar)))
        (when-let* ((beg (elisp-scope--sym-pos svar)))
          (elisp-scope--binding bare beg))
        (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos svar) l)))
      (when-let* ((bare (elisp-scope--sym-bare var)))
        (when-let* ((beg (elisp-scope--sym-pos var)))
          (elisp-scope--binding bare beg))
        (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos var) l)))
      (cond
       (arglist
        (let* ((head (car arglist))
               (bare (elisp-scope--sym-bare head)))
          (if (memq bare '(&aux &allow-other-keys))
              (progn
                (when-let* ((beg (elisp-scope--sym-pos head)))
                  (elisp-scope--report 'ampersand beg bare))
                (cl-case bare
                  (&aux
                   (let ((elisp-scope-local-bindings l))
                     (elisp-scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body)))
                  (&allow-other-keys
                   (let ((elisp-scope-local-bindings l))
                     (elisp-scope-cl-lambda-1 (car more) (cdr more) body)))))
            (let ((elisp-scope-local-bindings l))
              (elisp-scope-cl-lambda-key head (cdr arglist) more body)))))
       (more (let ((elisp-scope-local-bindings l))
               (elisp-scope-cl-lambda-1 (car more) (cdr more) body)))
       (t (let ((elisp-scope-local-bindings l))
            (elisp-scope-lambda nil body)))))))

(defun elisp-scope-cl-lambda-aux (arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l elisp-scope-local-bindings)
         (init (cadr a)))
    (elisp-scope-1 init)
    (if (consp var)
        (let ((elisp-scope-local-bindings l))
          (elisp-scope-cl-lambda-1 var (cons arglist more) body))
      (when-let* ((bare (elisp-scope--sym-bare var)))
        (when-let* ((beg (elisp-scope--sym-pos var)))
          (elisp-scope--binding bare beg))
        (setq l (elisp-scope--local-new bare (elisp-scope--sym-pos var) l)))
      (let ((elisp-scope-local-bindings l))
        (cond
         (arglist (elisp-scope-cl-lambda-aux (car arglist) (cdr arglist) more body))
         (more (elisp-scope-cl-lambda-1 (car more) (cdr more) body))
         (t (elisp-scope-lambda nil body)))))))

(defun elisp-scope-cl-macrolet (bindings body outspec)
  (if-let* ((b (car bindings)))
      (let ((name (car b))
            (arglist (cadr b))
            (mbody (cddr b)))
        (elisp-scope-cl-lambda arglist mbody)
        (when-let* ((bare (elisp-scope--sym-bare name)))
          (let ((beg (elisp-scope--sym-pos name)))
            ;; TODO: Use a bespoke 'local-macro-definition' role.
            (when beg (elisp-scope--report 'macro beg bare beg))
            (let ((pos (or beg (cons 'gen (incf elisp-scope--counter)))))
              (elisp-scope-with-local-definition bare
                  (lambda (f &rest _)
                    (when (symbol-with-pos-p f)
                      (elisp-scope--report 'macro (symbol-with-pos-pos f) bare pos)))
                (elisp-scope-cl-macrolet (cdr bindings) body outspec))))))
    (elisp-scope-n body outspec)))

(defun elisp-scope-define-minor-mode (mode _doc body)
  (let ((explicit-var nil) (command t))
    (while-let ((kw (car-safe body))
                (bkw (elisp-scope--sym-bare kw))
                ((keywordp bkw)))
      (when-let* ((beg (elisp-scope--sym-pos kw)))
        (elisp-scope--report 'constant beg bkw))
      (cl-case bkw
        ((:init-value :keymap :after-hook :initialize)
         (elisp-scope-1 (cadr body)))
        (:lighter (elisp-scope-mode-line-construct (cadr body)))
        ((:interactive)
         (let ((val (cadr body)))
           (when (consp val) (mapc #'elisp-scope-major-mode-name val))
           (setq command val)))
        ((:variable)
         (let* ((place (cadr body))
                (tail (cdr-safe place)))
           (if (and tail (let ((symbols-with-pos-enabled t))
                           (or (symbolp tail) (functionp tail))))
               (progn
                 (elisp-scope-1 (car place))
                 (elisp-scope-sharpquote tail))
             (elisp-scope-1 place)))
         (setq explicit-var t))
        ((:group) (elisp-scope-1 (cadr body) '(symbol . group)))
        ((:predicate)                   ;For globalized minor modes.
         (elisp-scope-global-minor-mode-predicate (cadr body)))
        ((:on :off)
         (let ((obod (cdr body)))
           (while (and obod (not (keywordp (elisp-scope--sym-bare (car obod)))))
             (elisp-scope-1 (pop obod)))
           (setq body (cons bkw (cons nil obod))))))
      (setq body (cddr body)))
    (when-let* ((bare (elisp-scope--sym-bare mode)) (beg (elisp-scope--sym-pos mode))
                (typ (if command 'defcmd 'defun)))
      (elisp-scope--report typ beg bare)
      (unless explicit-var
        (elisp-scope--report 'defvar beg bare)))
    (elisp-scope-n body)))

(defun elisp-scope-global-minor-mode-predicate (pred)
  (if (consp pred)
      (if (eq 'not (elisp-scope--sym-bare (car pred)))
          (mapc #'elisp-scope-global-minor-mode-predicate (cdr pred))
        (mapc #'elisp-scope-global-minor-mode-predicate pred))
    (elisp-scope-major-mode-name pred)))

(defun elisp-scope-major-mode-name (mode)
  (when-let* ((beg (elisp-scope--sym-pos mode))
              (bare (bare-symbol mode))
              ((not (booleanp bare))))
    (elisp-scope--report 'major-mode beg bare)))

(defun elisp-scope-mode-line-construct (format)
  (elisp-scope-mode-line-construct-1 format))

(defun elisp-scope-mode-line-construct-1 (format)
  (cond
   ((symbol-with-pos-p format)
    (elisp-scope--report 'free-variable (symbol-with-pos-pos format) (bare-symbol format)))
   ((consp format)
    (let ((head (car format)))
      (cond
       ((or (stringp head) (consp head) (integerp head))
        (mapc #'elisp-scope-mode-line-construct-1 format))
       ((or (symbolp head) (symbol-with-pos-p head))
        (elisp-scope--symbol head)
        (cl-case (bare-symbol head)
          (:eval
           (elisp-scope-1 (cadr format)))
          (:propertize
           (elisp-scope-mode-line-construct-1 (cadr format))
           (when-let* ((props (cddr format))
                       (symbols-with-pos-enabled t)
                       (val-form (plist-get props 'face)))
             (elisp-scope-quote val-form 'face)))
          (otherwise
           (elisp-scope-mode-line-construct-1 (cadr format))
           (elisp-scope-mode-line-construct-1 (caddr format))))))))))

(defcustom elisp-scope-safe-macros nil
  "Specify which macros are safe to expand during code analysis.

If this is t, macros are considered safe by default.  Otherwise, this is
a (possibly empty) list of safe macros; nil means no macros are safe.

Some macros are never safe, see `elisp-scope-safe-macro-p' for details.

Note that this option only affects analysis of untrusted code, for
trusted code macro expansion is always safe."
  :type '(choice (const :tag "Trust all macros" t)
                 (repeat :tag "Trust these macros" symbol))
  :version "31.1"
  :group 'lisp)

(defvar elisp-scope-unsafe-macros
  '( static-if static-when static-unless
     cl-eval-when eval-when-compile eval-and-compile let-when-compile
     rx cl-macrolet nnoo-define-basics))

(defun elisp-scope-safe-macro-p (macro)
  "Check whether it is safe to expand MACRO, return non-nil iff so.

If MACRO is one of the macros in `elisp-scope-unsafe-macros', then it is
never considered safe.  Otherwise, MACRO is safe if it specified in
`elisp-scope-safe-macros', or if it has a non-nil `safe-macro' symbol
property, or if the current buffer is trusted (see `trusted-content-p')."
  (and (not (memq macro elisp-scope-unsafe-macros))
       (or (eq elisp-scope-safe-macros t)
           (memq macro elisp-scope-safe-macros)
           (get macro 'safe-macro)
           (trusted-content-p))))

(defvar warning-minimum-log-level)

(defmacro elisp-scope-define-analyzer (fsym args &rest body)
  "Define an analyzer function for function/macro FSYM.
ARGS is the arguments list of the analyzer function, and BODY is its body."
  (declare (indent defun))
  (let ((analyzer (intern (concat "elisp-scope--analyze-" (symbol-name fsym)))))
    `(progn
       (defun ,analyzer ,args ,@body)
       (put ',fsym 'elisp-scope-analyzer #',analyzer))))

(defmacro elisp-scope--define-function-analyzer (fsym args role &rest body)
  (declare (indent defun))
  (let ((helper (intern (concat "elisp-scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (elisp-scope-define-analyzer ,fsym (f &rest args)
         (elisp-scope-report-s f ',role)
         (apply #',helper args)))))

(defmacro elisp-scope-define-function-analyzer (fsym args &rest body)
  "Define an analyzer function for function FSYM.
The analyzer function analyzes occurrences of FSYM as a function call,
and it analyzes the arguments in calls to FSYM by executing BODY with
ARGS bound to the analyzed arguments."
  (declare (indent defun))
  `(elisp-scope--define-function-analyzer ,fsym ,args function ,@body))

(defmacro elisp-scope-define-macro-analyzer (fsym args &rest body)
  "Define an analyzer function for macro FSYM.
The analyzer function analyzes occurrences of FSYM as a macro call, and
it analyzes the arguments in calls to FSYM by executing BODY with ARGS
bound to the analyzed arguments."
  (declare (indent defun))
  (let ((helper (intern (concat "elisp-scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (elisp-scope-define-analyzer ,fsym (f &rest args)
         (elisp-scope-report-s f 'macro)
         (apply #',helper args)))))

(defmacro elisp-scope-define-special-form-analyzer (fsym args &rest body)
  "Define an analyzer function for special form FSYM.
The analyzer function analyzes occurrences of FSYM as a special form,
and it analyzes the arguments in calls to FSYM by executing BODY with
ARGS bound to the analyzed arguments."
  (declare (indent defun))
  (let ((helper (intern (concat "elisp-scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (elisp-scope-define-analyzer ,fsym (f &rest args)
         (elisp-scope-report-s f 'special-form)
         (apply #',helper args)))))

(defun elisp-scope--unquote (form)
  (when (memq (elisp-scope--sym-bare (car-safe form)) '(quote function \`))
    (cadr form)))

(elisp-scope-define-analyzer with-suppressed-warnings (f warnings &rest body)
  (elisp-scope-report-s f 'macro)
  (dolist (warning warnings)
    (when-let* ((wsym (car-safe warning)))
      (elisp-scope-report-s wsym 'warning-type)))
  (elisp-scope-n body))

(elisp-scope-define-analyzer eval (f form &optional lexical)
  (elisp-scope-report-s f 'function)
  ;; TODO: Use elisp-scope-1 with outspec `code' in the next line.
  ;; Difficulty: that would analyze the quoted code as if it is
  ;; evaluated in an unrelated local environment, so local variables
  ;; wouldn't be recognized correctly etc.  We can solve that by adding
  ;; some `code-evaled-here' outspec.
  (elisp-scope-1 (or (elisp-scope--unquote form) form))
  (elisp-scope-1 lexical))

(elisp-scope-define-function-analyzer funcall (&optional f &rest args)
  (elisp-scope-1 f '(symbol . function))
  (elisp-scope-n args))

(put 'apply 'elisp-scope-analyzer #'elisp-scope--analyze-funcall)

(elisp-scope-define-function-analyzer defalias (&optional sym def docstring)
  (elisp-scope-1 sym '(symbol . defun))
  (elisp-scope-1 def '(symbol . defun))
  (elisp-scope-1 docstring))

(elisp-scope-define-function-analyzer oclosure--define
  (&optional name docstring parent-names slots &rest props)
  (elisp-scope-1 name '(symbol . defoclosure))
  (elisp-scope-1 docstring)
  (elisp-scope-1 parent-names '(repeat . (symbol . oclosure)))
  (elisp-scope-1 slots
                 '(repeat .
                          (or (symbol . slot)
                              (cons (symbol . slot) .
                                    (plist (:type . cl-type))))))
  (while-let ((kw (car-safe props))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (elisp-scope-1 (cadr props) (when (eq bkw :predicate) '(symbol . defun)))
    (setq props (cddr props)))
  (when props (elisp-scope-n props)))

(elisp-scope-define-function-analyzer define-charset
  (&optional name docstring &rest props)
  (elisp-scope-1 name '(symbol . defcharset))
  (elisp-scope-1 docstring)
  (elisp-scope-n props))

(elisp-scope-define-function-analyzer define-charset-alias
  (&optional alias charset)
  (elisp-scope-1 alias '(symbol . defcharset))
  (elisp-scope-1 charset '(symbol . charset)))

(elisp-scope-define-function-analyzer charset-chars
  (&optional charset &rest rest)
  (elisp-scope-1 charset '(symbol . charset))
  (elisp-scope-n rest))

(dolist (sym '(charset-description charset-info charset-iso-final-char
                                   charset-long-name charset-plist
                                   charset-short-name
                                   get-charset-property put-charset-property
                                   list-charset-chars
                                   set-charset-plist
                                   set-charset-priority
                                   unify-charset
                                   locale-charset-to-coding-system))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-charset-chars))

(elisp-scope-define-function-analyzer define-coding-system
  (&optional name &rest rest)
  (elisp-scope-1 name '(symbol . defcoding))
  (mapc #'elisp-scope-1 rest))

(elisp-scope-define-function-analyzer define-coding-system-alias
  (&optional alias coding-system)
  (elisp-scope-1 alias '(symbol . defcoding))
  (elisp-scope-1 coding-system '(symbol . coding)))

(elisp-scope-define-function-analyzer decode-coding-region
  (&optional start end coding-system &rest rest)
  (elisp-scope-1 start)
  (elisp-scope-1 end)
  (elisp-scope-1 coding-system '(symbol . coding))
  (elisp-scope-n rest))

(put 'encode-coding-region 'elisp-scope-analyzer #'elisp-scope--analyze-decode-coding-region)

(elisp-scope-define-function-analyzer decode-coding-string
  (&optional string coding-system &rest rest)
  (elisp-scope-1 string)
  (elisp-scope-1 coding-system '(symbol . coding))
  (elisp-scope-n rest))

(dolist (sym '(encode-coding-char encode-coding-string))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-decode-coding-string))

(elisp-scope-define-function-analyzer coding-system-mnemonic
  (&optional coding-system &rest rest)
  (elisp-scope-1 coding-system '(symbol . coding))
  (elisp-scope-n rest))

(dolist (sym '(add-to-coding-system-list
               check-coding-system
               coding-system-aliases
               coding-system-base
               coding-system-category
               coding-system-change-eol-conversion
               coding-system-change-text-conversion
               coding-system-charset-list
               coding-system-doc-string
               coding-system-eol-type
               coding-system-eol-type-mnemonic
               coding-system-get
               coding-system-plist
               coding-system-post-read-conversion
               coding-system-pre-write-conversion
               coding-system-put
               coding-system-translation-table-for-decode
               coding-system-translation-table-for-encode
               coding-system-type
               describe-coding-system
               prefer-coding-system
               print-coding-system
               print-coding-system-briefly
               revert-buffer-with-coding-system
               set-buffer-file-coding-system
               set-clipboard-coding-system
               set-coding-system-priority
               set-default-coding-systems
               set-file-name-coding-system
               set-keyboard-coding-system
               set-next-selection-coding-system
               set-selection-coding-system
               set-terminal-coding-system
               universal-coding-system-argument))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-coding-system-mnemonic))

(elisp-scope-define-function-analyzer thing-at-point (&optional thing no-props)
  (elisp-scope-1 thing '(symbol . thing))
  (elisp-scope-1 no-props))

(dolist (sym '( forward-thing
                beginning-of-thing
                end-of-thing
                bounds-of-thing-at-point))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-thing-at-point))

(elisp-scope-define-function-analyzer bounds-of-thing-at-mouse (&optional event thing)
  (elisp-scope-1 event)
  (elisp-scope-1 thing '(symbol . thing)))

(elisp-scope-define-function-analyzer thing-at-mouse (&optional event thing no-props)
  (elisp-scope-1 event)
  (elisp-scope-1 thing '(symbol . thing))
  (elisp-scope-1 no-props))

(elisp-scope-define-function-analyzer custom-declare-variable (sym default doc &rest args)
  (elisp-scope-1 sym '(symbol . defvar))
  (elisp-scope-1 default)
  (elisp-scope-1 doc)
  (while-let ((kw (car-safe args))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (elisp-scope-1 (cadr args)
                   (cl-case bkw
                     (:type  'widget-type)
                     (:group '(symbol . group))))
    (setq args (cddr args)))
  (when args (elisp-scope-n args)))

(elisp-scope-define-function-analyzer custom-declare-group (sym members doc &rest args)
  (elisp-scope-1 sym '(symbol . defgroup))
  (elisp-scope-1 members)
  (elisp-scope-1 doc '(symbol . defgroup))
  (while-let ((kw (car-safe args))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (elisp-scope-1 (cadr args) (when (eq bkw :group) '(symbol . group)))
    (setq args (cddr args)))
  (when args (elisp-scope-n args)))

(elisp-scope-define-function-analyzer custom-declare-face (face spec doc &rest args)
  (elisp-scope-1 face '(symbol . defface))
  (elisp-scope-1 spec '(repeat . (cons t . (plist (:inherit . (symbol . face))))))
  (elisp-scope-1 doc)
  (while-let ((kw (car-safe args))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (elisp-scope-1 (cadr args) (when (eq bkw :group) '(symbol . group)))
    (setq args (cddr args)))
  (when args (elisp-scope-n args)))

(elisp-scope-define-function-analyzer cl-typep (val type)
  (elisp-scope-1 val)
  (elisp-scope-1 type 'cl-type))

(elisp-scope-define-function-analyzer pulse-momentary-highlight-region (start end &optional face)
  (elisp-scope-1 start)
  (elisp-scope-1 end)
  (elisp-scope-1 face '(symbol . face)))

(elisp-scope--define-function-analyzer throw (&optional tag val) non-local-exit
  (elisp-scope-1 tag '(symbol . throw-tag))
  (elisp-scope-1 val))

(elisp-scope--define-function-analyzer signal (&optional error-symbol data) non-local-exit
  (elisp-scope-1 error-symbol '(symbol . condition))
  (elisp-scope-1 data))

(elisp-scope--define-function-analyzer kill-emacs (&rest rest) non-local-exit
  (elisp-scope-n rest))

(dolist (sym '( abort-recursive-edit top-level exit-recursive-edit
                tty-frame-restack error user-error
                minibuffer-quit-recursive-edit exit-minibuffer))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-kill-emacs))

(elisp-scope-define-function-analyzer run-hooks (&rest hooks)
  (dolist (hook hooks) (elisp-scope-1 hook '(symbol . free-variable))))

(elisp-scope-define-function-analyzer fboundp (&optional symbol)
  (elisp-scope-1 symbol '(symbol . function)))

(elisp-scope-define-function-analyzer overlay-put (&optional ov prop val)
  (elisp-scope-1 ov)
  (elisp-scope-1 prop)                  ;TODO: Recognize overlay props.
  (elisp-scope-1
   val
   (let* ((q (elisp-scope--unquote prop)))
     (when (memq (elisp-scope--sym-bare q) '(face mouse-face))
       'face))))

(elisp-scope-define-function-analyzer add-face-text-property (&optional start end face &rest rest)
  (elisp-scope-1 start)
  (elisp-scope-1 end)
  (elisp-scope-1 face 'face)
  (elisp-scope-n rest))

(elisp-scope-define-function-analyzer facep (&optional face &rest rest)
  (elisp-scope-1 face '(symbol . face))
  (elisp-scope-n rest))

(dolist (sym '( check-face face-id face-differs-from-default-p
                face-name face-all-attributes face-attribute
                face-foreground face-background face-stipple
                face-underline-p face-inverse-video-p face-bold-p
                face-italic-p face-extend-p face-documentation
                set-face-documentation set-face-attribute
                set-face-font set-face-background set-face-foreground
                set-face-stipple set-face-underline set-face-inverse-video
                set-face-bold set-face-italic set-face-extend))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-facep))

(elisp-scope-define-function-analyzer boundp (&optional var &rest rest)
  (elisp-scope-1 var '(symbol . free-variable))
  (elisp-scope-n rest))

(dolist (sym '( set symbol-value define-abbrev-table
                special-variable-p local-variable-p
                local-variable-if-set-p add-variable-watcher
                get-variable-watchers remove-variable-watcher
                default-value set-default make-local-variable
                buffer-local-value add-to-list add-to-history find-buffer
                customize-set-variable set-variable
                add-hook remove-hook run-hook-with-args run-hook-wrapped))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-boundp))

(elisp-scope-define-function-analyzer defvaralias (new base &optional docstring)
  (elisp-scope-1 new '(symbol . defvar))
  (elisp-scope-1 base '(symbol . free-variable))
  (elisp-scope-1 docstring))

(elisp-scope-define-function-analyzer define-error (&optional name message parent)
  (elisp-scope-1 name '(symbol . defcondition))
  (elisp-scope-1 message)
  (elisp-scope-1 parent '(or (symbol . condition)
                             (repeat . (symbol . condition)))))

(elisp-scope-define-function-analyzer featurep (feature &rest rest)
  (elisp-scope-1 feature '(symbol . feature))
  (elisp-scope-n rest))

(put 'require 'elisp-scope-analyzer #'elisp-scope--analyze-featurep)

(elisp-scope-define-function-analyzer provide (feature &rest rest)
  (elisp-scope-1 feature '(symbol . deffeature))
  (elisp-scope-n rest))

(elisp-scope-define-function-analyzer put-text-property (&optional beg end prop val obj)
  (elisp-scope-1 beg)
  (elisp-scope-1 end)
  (elisp-scope-1 prop)
  (elisp-scope-1
   val
   (let* ((q (elisp-scope--unquote prop)))
     (when (memq (elisp-scope--sym-bare q) '(face mouse-face))
       'face)))
  (elisp-scope-1 obj))

(put 'remove-overlays 'elisp-scope-analyzer #'elisp-scope--analyze-put-text-property)

(elisp-scope-define-function-analyzer propertize (string &rest props)
  (elisp-scope-1 string)
  (while props
    (elisp-scope-1 (car props))
    (elisp-scope-1
     (cadr props)
     (let* ((q (elisp-scope--unquote (car props))))
       (when (memq (elisp-scope--sym-bare q) '(face mouse-face))
         'face)))
    (setq props (cddr props)))
  (when props (elisp-scope-n props)))

(elisp-scope-define-function-analyzer eieio-defclass-internal
  (&optional name superclasses slots options)
  (elisp-scope-1 name '(symbol . deftype))
  (elisp-scope-1 superclasses '(repeat . (symbol . type)))
  (elisp-scope-1 slots
                 '(repeat
                   cons
                   (symbol . slot)
                   plist
                   (:initform   . code)
                   (:initarg    . (symbol . constant))
                   (:accessor   . (symbol . defun))
                   (:allocation . code)
                   (:writer     . (symbol . function))
                   (:reader     . (symbol . function))
                   (:type       . cl-type)
                   ;; TODO: add (:custom  . custom-type)
                   ))
  (elisp-scope-1 options))

(elisp-scope-define-function-analyzer cl-struct-define
  (&optional name doc parent type named slots children tag print)
  (elisp-scope-1 name '(symbol . deftype))
  (elisp-scope-1 doc)
  (elisp-scope-1 parent '(symbol . type))
  (elisp-scope-1 type)
  (elisp-scope-1 named)
  (elisp-scope-1 slots)           ;TODO: Specify type of `slots'.
  (elisp-scope-1 children)
  (elisp-scope-1 tag)
  (elisp-scope-1 print))

(elisp-scope-define-function-analyzer define-widget (name class doc &rest args)
  (elisp-scope-1 name '(symbol . widget-type-definition))
  (elisp-scope-1 class '(symbol . widget-type))
  (elisp-scope-1 doc)
  (while-let ((kw (car-safe args))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (elisp-scope-1 (cadr args)
                   (cl-case bkw
                     (:type 'widget-type)
                     (:args '(repeat . widget-type))))
    (setq args (cddr args)))
  (when args (elisp-scope-n args)))

(elisp-scope-define-function-analyzer provide-theme (name &rest rest)
  (elisp-scope-1 name '(symbol . theme))
  (elisp-scope-n rest))

(dolist (sym '(enable-theme disable-theme load-theme custom-theme-p))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-provide-theme))

(elisp-scope-define-function-analyzer custom-theme-set-variables (theme &rest args)
  (elisp-scope-1 theme '(symbol . theme))
  (dolist (arg args)
    (elisp-scope-1
     arg
     '(cons (symbol . free-variable) .
            (cons code .
                  (or (cons t .
                            (cons (repeat . (symbol . feature)) .
                                  t))
                      t))))))

(elisp-scope-define-function-analyzer custom-declare-theme (name &rest rest)
  (elisp-scope-1 name '(symbol . deftheme))
  (elisp-scope-n rest))

(elisp-scope-define-function-analyzer eieio-oref (obj slot)
  (elisp-scope-1 obj)
  (elisp-scope-1 slot '(symbol . slot)))

(dolist (fun '(slot-boundp slot-makeunbound slot-exists-p eieio-oref-default))
  (put fun 'elisp-scope-analyzer #'elisp-scope--analyze-eieio-oref))

(elisp-scope-define-function-analyzer eieio-oset (obj slot value)
  (elisp-scope-1 obj)
  (elisp-scope-1 slot '(symbol . slot))
  (elisp-scope-1 value))

(put 'eieio-oset-default 'elisp-scope-analyzer #'elisp-scope--analyze-eieio-oset)

(elisp-scope-define-function-analyzer derived-mode-p (modes &rest rest)
  (elisp-scope-1 modes '(or (repeat . (symbol . major-mode))
                            (symbol . major-mode)))
  (dolist (mode rest) (elisp-scope-1 mode '(symbol . major-mode))))

(elisp-scope-define-function-analyzer derived-mode-set-parent (&optional mode parent)
  (elisp-scope-1 mode '(symbol . major-mode))
  (elisp-scope-1 parent '(symbol . major-mode)))

(elisp-scope-define-function-analyzer elisp-scope-report (role &rest args)
  (elisp-scope-1 role '(symbol . symbol-role))
  (mapc #'elisp-scope-1 args))

(elisp-scope-define-function-analyzer elisp-scope-report-s (&optional sym role)
  (elisp-scope-1 sym)
  (elisp-scope-1 role '(symbol . symbol-role)))

(elisp-scope-define-function-analyzer elisp-scope-1 (&optional form outspec)
  (elisp-scope-1 form)
  (elisp-scope-1 outspec 'spec))

(elisp-scope-define-function-analyzer icons--register (&optional name parent spec doc kws)
  (elisp-scope-1 name '(symbol . deficon))
  (elisp-scope-1 parent '(symbol . icon))
  (elisp-scope-1 spec)                  ;TODO: Specify spec of `spec'.
  (elisp-scope-1 doc)
  (if-let* ((q (elisp-scope--unquote kws)))
      (progn
        (while-let ((kw (car-safe q))
                    (bkw (elisp-scope--sym-bare kw))
                    ((keywordp bkw)))
          (elisp-scope-report-s kw 'constant)
          (elisp-scope-1 (cadr q) (when (eq bkw :group) '(symbol . group)))
          (setq q (cddr q)))
        (when q (elisp-scope-n q)))
    (elisp-scope-1 kws)))

(elisp-scope-define-function-analyzer setopt--set (&optional var val)
  (elisp-scope-1 var '(symbol . free-variable))
  (elisp-scope-1 val elisp-scope-output-spec))

(elisp-scope-define-function-analyzer autoload (&optional func file doc int type)
  (elisp-scope-1 func '(symbol . function))
  (elisp-scope-1 file)
  (elisp-scope-1 doc)
  (elisp-scope-1 int '(repeat . (symbol . major-mode)))
  (elisp-scope-1 type))

(elisp-scope-define-function-analyzer define-completion-category (&optional name parents &rest rest)
  (elisp-scope-1 name '(symbol . completion-category-definition))
  (elisp-scope-1 parents '(repeat . (symbol . completion-category)))
  (elisp-scope-n rest))

(elisp-scope-define-function-analyzer completion-table-with-category (&optional category table)
  (elisp-scope-1 category '(symbol . completion-category))
  (elisp-scope-1 table))

(defun elisp-scope--easy-menu-do-define-menu (menu)
  (let ((items (cdr menu)))
    (while-let ((kw (car-safe items))
                (bkw (elisp-scope--sym-bare kw))
                ((keywordp bkw)))
      (elisp-scope-report-s kw 'constant)
      (cl-case bkw
        ((:active :label :visible) (elisp-scope-1 (cadr items)))
        ((:filter) (elisp-scope-sharpquote (cadr items))))
      (setq items (cddr items)))
    (dolist (item items)
      (cond
       ((vectorp item)
        (when (length> item 2)
          (elisp-scope-sharpquote (aref item 1))
          (let ((it (cddr (append item nil))))
            (elisp-scope-1 (car it))
            (while-let ((kw (car-safe it))
                        (bkw (elisp-scope--sym-bare kw))
                        ((keywordp bkw)))
              (elisp-scope-report-s kw 'constant)
              (cl-case bkw
                ((:active :enable :label :visible :suffix :selected) (elisp-scope-1 (cadr it))))
              (setq it (cddr it))))))
       ((consp item) (elisp-scope--easy-menu-do-define-menu item))))))

(elisp-scope-define-function-analyzer easy-menu-do-define (&optional symbol maps doc menu)
  (elisp-scope-1 symbol)
  (elisp-scope-1 maps)
  (elisp-scope-1 doc)
  (if-let* ((q (elisp-scope--unquote menu)))
      ;; TODO: Use `elisp-scope-1' with an appropriate outspec.
      (elisp-scope--easy-menu-do-define-menu q)
    (elisp-scope-1 menu)))

(elisp-scope-define-function-analyzer define-key (&optional keymap key def remove)
  (elisp-scope-1 keymap)
  (elisp-scope-1 key)
  (if-let* ((q (elisp-scope--unquote def)))
      ;; TODO: Use `elisp-scope-1' with an appropriate outspec.
      (cond
       ((eq (elisp-scope--sym-bare (car-safe q)) 'menu-item)
        (let ((fn (caddr q)) (it (cdddr q)))
          (elisp-scope-sharpquote fn)
          (while-let ((kw (car-safe it))
                      (bkw (elisp-scope--sym-bare kw))
                      ((keywordp bkw)))
            (elisp-scope-report-s kw 'constant)
            (cl-case bkw
              ((:active :enable :label :visible :suffix :selected) (elisp-scope-1 (cadr it)))
              ((:filter) (elisp-scope-sharpquote (cadr it))))
            (setq it (cddr it)))))
       ((or (symbolp q) (symbol-with-pos-p q))
        (elisp-scope-report-s q 'function)))
    (elisp-scope-1 def))
  (elisp-scope-1 remove))

(elisp-scope-define-function-analyzer eval-after-load (&optional file form)
  (elisp-scope-1 file '(symbol . feature))
  (elisp-scope-1 form 'code))

;; We use a bespoke analyzer for `if-let*' instead of letting
;; `elisp-scope-1' expand it because `if-let*' expands to a form that
;; uses each binding symbol also as a bound symbol, and hence after
;; macro-expansion, we would analyze the same symbol(-with-position)
;; first as a `binding-variable' and then as `bound-variable'.  With
;; this bespoke analyzer, we only analyze it as a `binding-variable'.
(elisp-scope-define-macro-analyzer if-let* (&optional varlist then &rest else)
  (elisp-scope-if-let varlist then else elisp-scope-output-spec))

(defun elisp-scope-if-let (bindings then else outspec)
  (if (consp bindings)
      (let* ((binding (car bindings))
             (sym (if (consp binding)
                      (when (cdr binding) (car binding))
                    binding))
             (form (when (consp binding)
                     (if (cdr binding) (cadr binding) (car binding))))
             (bare (elisp-scope--sym-bare sym))
             (beg (elisp-scope--sym-pos sym)))
        (when beg (elisp-scope--binding bare beg))
        (when form (elisp-scope-1 form))
        (let ((elisp-scope-local-bindings
               (elisp-scope--local-new bare beg elisp-scope-local-bindings)))
          (elisp-scope-if-let (cdr bindings) then else outspec)))
    (elisp-scope-1 then outspec)
    (elisp-scope-n else outspec)))

(elisp-scope-define-macro-analyzer define-globalized-minor-mode (global mode turn-on &rest body)
  (elisp-scope-report-s mode 'function)
  (elisp-scope-report-s turn-on 'function)
  (elisp-scope-define-minor-mode global nil body))

(elisp-scope-define-macro-analyzer define-derived-mode (&optional child parent name &rest body)
  (elisp-scope-report-s child 'major-mode-definition)
  (elisp-scope-report-s parent 'major-mode)
  (elisp-scope-mode-line-construct name)
  (when (stringp (car body)) (pop body))
  (while-let ((kw (car-safe body))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (cl-case bkw
      (:group (elisp-scope-1 (cadr body) '(symbol . group)))
      ((:syntax-table :abbrev-table :after-hook) (elisp-scope-1 (cadr body))))
    (setq body (cddr body)))
  (elisp-scope-n body))

(elisp-scope-define-macro-analyzer lambda (args &rest body)
  (elisp-scope-lambda args body))

(defun elisp-scope-oclosure-lambda-1 (local bindings args body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (elisp-scope--sym-bare sym))
             (beg (elisp-scope--sym-pos sym)))
        (when beg (elisp-scope--binding bare beg))
        (elisp-scope-1 (cadr binding))
        (elisp-scope-oclosure-lambda-1
         (if bare (elisp-scope--local-new bare beg local) local)
         (cdr bindings) args body))
    (let ((elisp-scope-local-bindings local))
      (elisp-scope-lambda args body))))

(defun elisp-scope-oclosure-lambda (spec args body)
  (let ((type (car-safe spec)))
    (elisp-scope-report-s type 'oclosure))
  (elisp-scope-oclosure-lambda-1 elisp-scope-local-bindings (cdr-safe spec) args body))

(elisp-scope-define-macro-analyzer oclosure-lambda (&optional spec args &rest body)
  (elisp-scope-oclosure-lambda spec args body))

(elisp-scope-define-macro-analyzer cl-loop (&rest clauses)
  (elisp-scope-loop clauses))

(elisp-scope-define-macro-analyzer named-let (name bindings &rest body)
  (elisp-scope-named-let name bindings body elisp-scope-output-spec))

(elisp-scope-define-macro-analyzer cl-flet (bindings &rest body)
  (elisp-scope-flet bindings body elisp-scope-output-spec))

(elisp-scope-define-macro-analyzer cl-labels (bindings &rest body)
  (elisp-scope-labels bindings body elisp-scope-output-spec))

(elisp-scope-define-macro-analyzer with-slots (spec-list object &rest body)
  (elisp-scope-1 object)
  (elisp-scope-let spec-list body))

(elisp-scope-define-macro-analyzer cl-defmethod (name &rest rest)
  (elisp-scope-defmethod name rest))

(elisp-scope-define-macro-analyzer cl-destructuring-bind (args expr &rest body)
  (elisp-scope-1 expr)
  (elisp-scope-cl-lambda args body))

(elisp-scope-define-macro-analyzer declare-function (&optional fn _file arglist _fileonly)
  (elisp-scope-report-s fn 'function)
  (elisp-scope-lambda (and (listp arglist) arglist) nil))

(elisp-scope-define-macro-analyzer cl-block (name &rest body)
  (elisp-scope-block name body))

(elisp-scope-define-macro-analyzer cl-return-from (name &optional result)
  (elisp-scope-return-from name result))

(elisp-scope-define-macro-analyzer rx (&rest regexps)
  ;; Unsafe macro!
  (elisp-scope-rx regexps))

(elisp-scope-define-macro-analyzer cl-tagbody (&rest body)
  (let (labels statements)
    (while body
      (let ((head (pop body)))
        (if (consp head)
            (push head statements)
          (push head labels))))
    (elisp-scope-cl-tagbody (nreverse labels) (nreverse statements))))

(defvar elisp-scope-label-alist nil)

(defun elisp-scope-cl-tagbody (labels statements)
  (if labels
      (let* ((label (car labels))
             (bare (elisp-scope--sym-bare label)))
        (when-let* ((beg (elisp-scope--sym-pos label)))
          (elisp-scope--report 'label beg bare beg))
        (let ((elisp-scope-label-alist
               (if bare
                   (elisp-scope--local-new bare (elisp-scope--sym-pos label) elisp-scope-label-alist)
                 elisp-scope-label-alist)))
          (elisp-scope-cl-tagbody (cdr labels) statements)))
    (elisp-scope-n statements)))

(elisp-scope-define-macro-analyzer go (label)
  ;; TODO: Change to a local macro definition induced by `cl-tagbody'.
  (when-let* ((bare (elisp-scope--sym-bare label))
              (pos (alist-get bare elisp-scope-label-alist))
              (beg (elisp-scope--sym-pos label)))
    (elisp-scope--report 'label beg bare pos)))

(elisp-scope-define-macro-analyzer rx-define (name &rest rest)
  (elisp-scope-rx-define name rest))

(elisp-scope-define-macro-analyzer rx-let (bindings &rest body)
  (elisp-scope-rx-let bindings body))

(elisp-scope-define-macro-analyzer let-when-compile (bindings &rest body)
  ;; Unsafe macro!
  (elisp-scope-let* bindings body))

(elisp-scope-define-macro-analyzer cl-eval-when (_when &rest body)
  ;; Unsafe macro!
  (elisp-scope-n body))

(elisp-scope-define-macro-analyzer cl-macrolet (bindings &rest body)
  ;; Unsafe macro!
  (elisp-scope-cl-macrolet bindings body elisp-scope-output-spec))

(elisp-scope-define-macro-analyzer cl-symbol-macrolet (bindings &rest body)
  ;; Unsafe macro!
  (elisp-scope-let* bindings body))

(elisp-scope-define-macro-analyzer nnoo-define-basics (&optional backend)
  ;; Unsafe macro!
  (when-let* ((beg (elisp-scope--sym-pos backend)))
    (elisp-scope--report 'nnoo-backend beg (bare-symbol backend))))

(elisp-scope-define-macro-analyzer gv-define-expander (name handler)
  (elisp-scope-gv-define-expander name handler))

(elisp-scope-define-macro-analyzer gv-define-simple-setter (name setter &rest rest)
  (elisp-scope-gv-define-simple-setter name setter rest))

(elisp-scope-define-macro-analyzer cl-deftype (name arglist &rest body)
  (elisp-scope-deftype name arglist body))

(elisp-scope-define-macro-analyzer define-minor-mode (&optional mode doc &rest body)
  (when mode (elisp-scope-define-minor-mode mode doc body)))

(elisp-scope-define-macro-analyzer setq-local (&rest args)
  (elisp-scope-setq args))

(put 'setq-default 'elisp-scope-analyzer #'elisp-scope--analyze-setq-local)

(elisp-scope-define-macro-analyzer cl-defun (name arglist &rest body)
  (elisp-scope-cl-defun name arglist body))

(put 'cl-defmacro 'elisp-scope-analyzer #'elisp-scope--analyze-cl-defun)

(elisp-scope-define-macro-analyzer defun (&optional name arglist &rest body)
  (when name (elisp-scope-defun name arglist body)))

(elisp-scope-define-macro-analyzer defmacro (&optional name arglist &rest body)
  (elisp-scope-report-s name 'defmacro)
  (elisp-scope-lambda arglist body))

(put 'ert-deftest 'elisp-scope-analyzer #'elisp-scope--analyze-defun)

(elisp-scope-define-macro-analyzer elisp-scope-define-symbol-role (&optional name parents &rest props)
  (elisp-scope-report-s name 'symbol-role-definition)
  (dolist (parent parents) (elisp-scope-report-s parent 'symbol-role))
  (while-let ((kw (car-safe props))
              (bkw (elisp-scope--sym-bare kw))
              ((keywordp bkw)))
    (elisp-scope-report-s kw 'constant)
    (elisp-scope-1 (cadr props) (when (eq bkw :face) 'face))
    (setq props (cddr props))))

(elisp-scope-define-macro-analyzer cl-letf (bindings &rest body)
  (let ((l elisp-scope-local-bindings))
    (dolist (binding bindings)
      (let ((place (car binding)))
        (if (or (symbol-with-pos-p place) (symbolp place))
            (let* ((bare (bare-symbol place))
                   (beg (elisp-scope--sym-pos place)))
              (when beg (elisp-scope--binding bare beg))
              (setq l (elisp-scope--local-new bare beg l)))
          (elisp-scope-1 place))
        (elisp-scope-1 (cadr binding))))
    (let ((elisp-scope-local-bindings l)) (elisp-scope-n body elisp-scope-output-spec))))

(elisp-scope-define-macro-analyzer setf (&rest args) (elisp-scope-setq args))

(elisp-scope-define-macro-analyzer pop (&optional place) (elisp-scope-1 place))

(elisp-scope-define-macro-analyzer push (&optional newelt place)
  (elisp-scope-1 newelt)
  (elisp-scope-1 place))

(elisp-scope-define-macro-analyzer with-memoization (&optional place &rest body)
  (elisp-scope-1 place)
  (elisp-scope-n body elisp-scope-output-spec))

(elisp-scope-define-macro-analyzer cl-pushnew (&rest args)
  (mapc #'elisp-scope-1 args))

(dolist (sym '(incf decf))
  (put sym 'elisp-scope-analyzer #'elisp-scope--analyze-cl-pushnew))

(elisp-scope-define-macro-analyzer static-if (&optional test then &rest else)
  (elisp-scope-1 test)
  (elisp-scope-1 then elisp-scope-output-spec)
  (elisp-scope-n else elisp-scope-output-spec))

(elisp-scope-define-macro-analyzer static-when (&optional test &rest body)
  (elisp-scope-1 test)
  (elisp-scope-n body elisp-scope-output-spec))

(put 'static-unless 'elisp-scope-analyzer #'elisp-scope--analyze-static-when)

(elisp-scope-define-macro-analyzer eval-when-compile (&rest body)
  (elisp-scope-n body elisp-scope-output-spec))

(put 'eval-and-compile 'elisp-scope-analyzer #'elisp-scope--analyze-eval-when-compile)

(elisp-scope-define-macro-analyzer cl-callf (&rest args)
  (elisp-scope-sharpquote (car args))
  (elisp-scope-n (cdr args)))

(put 'cl-callf2 'elisp-scope-analyzer #'elisp-scope--analyze-cl-callf)

(elisp-scope-define-macro-analyzer seq-let (args sequence &rest body)
  (elisp-scope-1 sequence)
  (let ((l elisp-scope-local-bindings))
    (dolist (arg args)
      (let* ((bare (elisp-scope--sym-bare arg))
             (beg (elisp-scope--sym-pos arg)))
        (if (eq bare '&rest)
            (elisp-scope--report 'ampersand beg bare)
          (when beg (elisp-scope--binding bare beg))
          (setq l (elisp-scope--local-new bare beg l)))))
    (let ((elisp-scope-local-bindings l)) (elisp-scope-n body))))

(elisp-scope-define-macro-analyzer define-obsolete-face-alias (&optional obs cur when)
  (when-let* ((q (elisp-scope--unquote obs))) (elisp-scope-report-s q 'defface))
  (when-let* ((q (elisp-scope--unquote cur))) (elisp-scope-report-s q 'face))
  (elisp-scope-1 when))

(elisp-scope-define-macro-analyzer backquote (&optional structure)
  (elisp-scope-backquote structure elisp-scope-output-spec))

(defvar elisp-scope-backquote-depth 0)

(defun elisp-scope-backquote (structure &optional outspec)
  (let ((elisp-scope-backquote-depth (1+ elisp-scope-backquote-depth)))
    (elisp-scope-backquote-1 structure outspec)))

(defun elisp-scope-backquote-1 (structure &optional outspec)
  (cond
   ((vectorp structure)
    (dotimes (i (length structure))
      (elisp-scope-backquote-1 (aref structure i))))
   ((atom structure) (elisp-scope-quote structure outspec))
   ((or (eq (car structure) backquote-unquote-symbol)
        (eq (car structure) backquote-splice-symbol))
    (if (= elisp-scope-backquote-depth 1)
        (elisp-scope-1 (cadr structure) outspec)
      (let ((elisp-scope-backquote-depth (1- elisp-scope-backquote-depth)))
        (elisp-scope-backquote-1 (cadr structure)))))
   (t
    (while (consp structure) (elisp-scope-backquote-1 (pop structure)))
    (when structure (elisp-scope-backquote-1 structure)))))

(elisp-scope-define-special-form-analyzer let (bindings &rest body)
  (elisp-scope-let bindings body))

(elisp-scope-define-special-form-analyzer let* (bindings &rest body)
  (elisp-scope-let* bindings body))

(elisp-scope-define-special-form-analyzer cond (&rest clauses)
  (dolist (clause clauses) (elisp-scope-n clause elisp-scope-output-spec)))

(elisp-scope-define-special-form-analyzer setq (&rest args)
  (elisp-scope-setq args))

(elisp-scope-define-special-form-analyzer defvar (&rest args)
  (elisp-scope-report-s
   (car args)
   (if (cdr args) 'defvar 'special-variable-declaration))
  (elisp-scope-1 (cadr args)))

(elisp-scope-define-special-form-analyzer defconst (&optional sym init _doc)
  (elisp-scope-report-s sym 'defvar)
  (elisp-scope-1 init))

(elisp-scope-define-special-form-analyzer condition-case (var bodyform &rest handlers)
  (let* ((bare (bare-symbol var))
         (beg (when (symbol-with-pos-p var) (symbol-with-pos-pos var)))
         (l (elisp-scope--local-new bare beg elisp-scope-local-bindings)))
    (when beg (elisp-scope--binding bare beg))
    (elisp-scope-1 bodyform elisp-scope-output-spec)
    (dolist (handler handlers)
      (dolist (cond-name (ensure-list (car-safe handler)))
        (when-let* ((cbeg (elisp-scope--sym-pos cond-name))
                    (cbare (elisp-scope--sym-bare cond-name))
                    (clen (length (symbol-name cbare))))
          (cond
           ((booleanp cbare))
           ((keywordp cbare) (elisp-scope--report 'constant cbeg cbare))
           (t                (elisp-scope--report 'condition cbeg cbare)))))
      (let ((elisp-scope-local-bindings l))
        (elisp-scope-n (cdr handler) elisp-scope-output-spec)))))

(elisp-scope-define-special-form-analyzer function (&optional arg)
  (when arg (elisp-scope-sharpquote arg)))

(elisp-scope-define-special-form-analyzer quote (arg)
  (elisp-scope-quote arg elisp-scope-output-spec))

(elisp-scope-define-special-form-analyzer interactive (&rest _)
  ;; Out-of-place `interactive' call, do nothing.
  )

(elisp-scope-define-special-form-analyzer if (&optional test then &rest else)
  (elisp-scope-1 test)
  (elisp-scope-1 then elisp-scope-output-spec)
  (elisp-scope-n else elisp-scope-output-spec))

(elisp-scope-define-special-form-analyzer and (&rest forms)
  (elisp-scope-n forms elisp-scope-output-spec))

(elisp-scope-define-special-form-analyzer or (&rest forms)
  (dolist (form forms) (elisp-scope-1 form elisp-scope-output-spec)))

(defun elisp-scope-quote (arg &optional outspec)
  (when outspec
    (when-let* ((spec (elisp-scope--match-spec-to-arg outspec arg)))
      (elisp-scope--handle-quoted spec arg))))

(cl-defgeneric elisp-scope--handle-quoted (spec arg))

(cl-defmethod elisp-scope--handle-quoted ((_spec (eql t)) _arg)
  ;; Do nothing.
  )

(cl-defmethod elisp-scope--handle-quoted ((_spec (eql 'code)) arg)
  (let ((elisp-scope-local-bindings nil)
        (elisp-scope-local-definitions nil)
        (elisp-scope-block-alist nil)
        (elisp-scope-label-alist nil)
        (elisp-scope-rx-alist nil)
        (elisp-scope--quoted t))
    (elisp-scope-1 arg)))

(cl-defmethod elisp-scope--handle-quoted ((spec (head symbol)) arg)
  (when-let* ((role (cdr spec))) (elisp-scope-report-s arg role)))

(cl-defmethod elisp-scope--handle-quoted ((spec (head list)) arg)
  (let ((specs (cdr spec)))
    (while specs (elisp-scope--handle-quoted (pop specs) (pop arg)))))

(cl-defmethod elisp-scope--handle-quoted ((spec (head cons)) arg)
  (elisp-scope--handle-quoted (cadr spec) (car arg))
  (elisp-scope--handle-quoted (cddr spec) (cdr arg)))

(cl-defgeneric elisp-scope--match-spec-to-arg (spec arg))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (eql t)) _arg) spec)

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (eql 'code)) _arg) spec)

(cl-defmethod elisp-scope--match-spec-to-arg ((_spec (eql 'spec)) arg)
  (elisp-scope--match-spec-to-arg
   ;; Unfold `spec'.
   '(or (symbol)
        (cons (member symbol) . (symbol . symbol-role))
        (cons (member repeat) . spec)
        (cons (member list)   . spec)
        (cons (member or)     . (repeat . spec))
        (cons (member and)    . (repeat . spec))
        (cons (member cons)   . (cons spec . spec))
        (cons (member member) . t)
        (cons (member plist)  . (repeat . (cons (symbol . constant) . spec)))
        (cons (member plist-and-then) . (repeat . (cons (symbol . constant) . spec))))
   arg))

(cl-defmethod elisp-scope--match-spec-to-arg ((_spec (eql 'face)) arg)
  (elisp-scope--match-spec-to-arg
   (if (consp arg)
       (if (keywordp (elisp-scope--sym-bare (car arg)))
           ;; One face, given as a plist of face attributes.
           '(plist (:inherit . (symbol . face)))
         ;; Multiple faces.
         '(repeat . (or (symbol . face)
                        (plist (:inherit . (symbol . face))))))
     '(symbol . face))
   arg))

(cl-defmethod elisp-scope--match-spec-to-arg ((_spec (eql 'cl-type)) arg)
  (elisp-scope--match-spec-to-arg
   ;; Unfold `cl-type'.
   '(or (member t)
        (symbol . type)
        (cons (member integer float real number) . t)
        (cons (member or and not)                . (repeat . cl-type))
        (cons (member member cl-member)          . (repeat . t))
        (cons (member satisfies)                 . (cons (or (symbol . function) code) . t)))
   arg))

(cl-defmethod elisp-scope--match-spec-to-arg ((_spec (eql 'widget-type)) arg)
  (elisp-scope--match-spec-to-arg
   (let ((kws
          '((:key-type . widget-type)
            (:value-type . widget-type)
            (:action . (symbol . function))
            (:match . (symbol . function))
            (:match-inline . (symbol . function))
            (:validate . (symbol . function))
            (:args . (repeat . widget-type)))))
     `(or (symbol . widget-type)
          (cons (and (member cons group vector choice radio set repeat checklist)
                     (symbol . widget-type))
                . (plist-and-then ,@kws (t . (repeat . widget-type))))
          (cons (and (member function-item)
                     (symbol . widget-type))
                . (plist-and-then ,@kws (t . (list (symbol . function)))))
          (cons (and (member variable-item)
                     (symbol . widget-type))
                . (plist-and-then ,@kws (t . (list (symbol . free-variable)))))
          (cons (symbol . widget-type)  ;Fallback.
                . (plist-and-then ,@kws (t . (repeat . t))))))
   arg))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head symbol)) arg)
  (when (or (symbolp arg) (symbol-with-pos-p arg)) spec))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head repeat)) arg)
  (when (listp arg)
    (named-let loop ((args arg) (acc nil))
      (if args
          (when-let* ((res (elisp-scope--match-spec-to-arg (cdr spec) (car args))))
            (loop (cdr args) (cons res acc)))
        (cons 'list (nreverse acc))))))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head or)) arg)
  (named-let loop ((specs (cdr spec)))
    (when specs
      (if-let* ((res (elisp-scope--match-spec-to-arg (car specs) arg))) res
        (loop (cdr specs))))))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head and)) arg)
  (let ((specs (cdr spec)))
    (if (null specs) t
      (let ((go t))
        (while (and (cdr specs) (setq go (elisp-scope--match-spec-to-arg
                                          (car specs) arg)))
          (pop specs))
        (when go (elisp-scope--match-spec-to-arg (car specs) arg))))))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head cons)) arg)
  (when (consp arg)
    (let ((car-spec (cadr spec))
          (cdr-spec (cddr spec)))
      (when-let* ((car-res (elisp-scope--match-spec-to-arg car-spec (car arg)))
                  (cdr-res (elisp-scope--match-spec-to-arg cdr-spec (cdr arg))))
        (cons 'cons (cons car-res cdr-res))))))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head member)) arg)
  (let ((symbols-with-pos-enabled t)) (and (member arg (cdr spec)) t)))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head plist)) arg)
  (when (listp arg)
    (let ((res nil) (go t))
      (while (and arg go)
        (let* ((key (car arg))
               (bkw (elisp-scope--sym-bare key))
               (val (cadr arg)))
          (push (if (keywordp bkw) '(symbol . constant) t) res)
          (push (setq go (elisp-scope--match-spec-to-arg (alist-get bkw (cdr spec) t) val)) res))
        (setq arg (cddr arg)))
      (when go (cons 'list (nreverse res))))))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head list)) arg)
  (when (listp arg)
    (let ((specs (cdr spec)) (go t) res)
      (while (and specs (setq go (elisp-scope--match-spec-to-arg (pop specs) (pop arg))))
        (push go res))
      (when go (cons 'list (nreverse res))))))

(cl-defmethod elisp-scope--match-spec-to-arg ((spec (head plist-and-then)) arg)
  (cond
   ((consp arg)
    (let ((val-spec-alist (cdr spec))
          (res nil)
          (go t)
          bkw)
      (while (and go (keywordp (setq bkw (elisp-scope--sym-bare (car arg)))))
        (push '(symbol . constant) res)
        (setq go (elisp-scope--match-spec-to-arg (alist-get bkw val-spec-alist t) (cadr arg)))
        (push go res)
        (setq arg (cddr arg)))
      (when go
        (let ((rest-res (elisp-scope--match-spec-to-arg (alist-get t val-spec-alist t) arg)))
          (when (eq (car rest-res) 'list)
            (setq rest-res (cdr rest-res))
            (dolist (s res) (push s rest-res))
            (cons 'list rest-res))))))
   ((null arg) t)))

(elisp-scope-define-special-form-analyzer catch (&optional tag &rest body)
  (elisp-scope-1 tag '(symbol . throw-tag))
  (elisp-scope-n body elisp-scope-output-spec))

(elisp-scope-define-special-form-analyzer progn (&rest body)
  (elisp-scope-n body elisp-scope-output-spec))

(put 'inline 'elisp-scope-analyzer #'elisp-scope--analyze-progn)
(put 'save-current-buffer 'elisp-scope-analyzer #'elisp-scope--analyze-progn)
(put 'save-excursion 'elisp-scope-analyzer #'elisp-scope--analyze-progn)
(put 'save-restriction 'elisp-scope-analyzer #'elisp-scope--analyze-progn)

(elisp-scope-define-special-form-analyzer while (&rest rest)
  (mapc #'elisp-scope-1 rest))

(elisp-scope-define-special-form-analyzer prog1 (&rest body)
  (when (consp body) (elisp-scope-1 (pop body) elisp-scope-output-spec))
  (elisp-scope-n body))

(put 'unwind-protect 'elisp-scope-analyzer #'elisp-scope--analyze-prog1)

(defun elisp-scope-report-s (sym role)
  "Report that symbol SYM has role ROLE.

If SYM is not a symbol with position information, do nothing."
  (when-let* ((beg (elisp-scope--sym-pos sym)) (bare (bare-symbol sym)))
    (elisp-scope--report role beg bare)))

(defvar-local elisp-scope-buffer-file-name nil)

(defun elisp-scope-1 (form &optional outspec)
  "Analyze FORM as an evaluated form with expected output spec OUTSPEC.

If OUTSPEC is non-nil, it specifies FORM's expected \"output spec\".
This guides the analysis of quoted (sub)forms.
OUTSPEC can be one the following:

- t: FORM evaluates to an arbitrary object.
  In other words, OUTSPEC of t conveys no information about FORM.

- `code': FORM evaluates to a form to be evaluated elsewhere.
  The quoted output of FORM will again be analyzed as an evaluated form,
  in a \"clean\" local environment.

- (symbol . ROLE): FORM evaluates to a symbol with role ROLE.
  See `elisp-scope-define-symbol-role' for more information about
  defining new symbol roles.

- (repeat . SPEC): FORM evaluates to a list with elements of spec SPEC.

- (cons CARSPEC . CDRSPEC): FORM evaluates to a cons cell whose `car'
  has spec CARSPEC and whose `cdr' has spec CDRSPEC.

- (list . SPECS): FORM evaluates to a list of the same length as SPECS,
  in which the `i'th element matches the `i'th spec in SPECS.

- (member . VALS): FORM evaluates to a `member' of VALS.

- (plist . VALSPECS): FORM evaluates to a plist.  VALSPECS is an alist
  associating value specs to properties in the plist.  For example, an
  entry (:face . (symbol . face)) in VALSPECS says that the value of the
  property `:face' in the plist is a face name.

- (or . SPECS): FORM evaluates to a value that matches one of SPECS.

- (and . SPECS): FORM evaluates to a value that matches all of SPECS.
  The last spec in SPECS determines how to analyze FORM if it matches.

For example, to analyze a FORM that evaluates to either a list of major
mode names or just to a single major mode name, use OUTSPEC as follows:

  (elisp-scope-1 FORM \\='(or (repeat . (symbol . major-mode))
                           (symbol . major-mode)))

If FORM in this example is (if (something-p) \\='foo \\='(bar baz)),
then all of `foo', `bar' and `baz' will be analyzed as major mode names.

See also `elisp-scope-analyze-form' for an details about how subforms
are analyzed."
  (cond
   ((consp form)
    (let* ((f (car form)) (bare (elisp-scope--sym-bare f))
           (forms (cdr form)) (this nil))
      (when bare
        (cond
         ((setq this (or (alist-get bare elisp-scope-local-definitions)
                         (function-get bare 'elisp-scope-analyzer)))
          (let ((elisp-scope-output-spec outspec)) (apply this form)))
         ((macrop bare) (elisp-scope-report-s f 'macro)
          (cond
           ((elisp-scope-safe-macro-p bare)
            (elisp-scope-1
             (let* ((warning-minimum-log-level :emergency)
                   (macroexp-inhibit-compiler-macros t)
                   (symbols-with-pos-enabled t)
                   (message-log-max nil)
                   (inhibit-message t)
                   (macroexpand-all-environment
                    (append (mapcar #'list elisp-scope-unsafe-macros) macroexpand-all-environment)))
               (ignore-errors (macroexpand-1 form macroexpand-all-environment)))
             outspec))
           ((eq (get bare 'edebug-form-spec) t) (elisp-scope-n forms))))
         ((functionp bare)
          (elisp-scope-report-s f 'function) (elisp-scope-n forms))
         (t
          (elisp-scope-report-s f 'unknown)
          (when elisp-scope-assume-func (elisp-scope-n forms)))))))
   ((symbol-with-pos-p form) (elisp-scope--symbol form))))

(defun elisp-scope-n (forms &optional outspec)
  "Analyze FORMS as evaluated forms.

OUTSPEC is the expected output spec of the last form in FORMS, if any.
It is passed to `elisp-scope-1', which see."
  (while (cdr-safe forms) (elisp-scope-1 (pop forms)))
  (when-let* ((form (car-safe forms))) (elisp-scope-1 form outspec)))

;;;###autoload
(defun elisp-scope-analyze-form (callback &optional stream)
  "Read and analyze code from STREAM, reporting findings via CALLBACK.

Call CALLBACK for each analyzed symbol SYM with arguments ROLE, POS,
SYM, ID and DEF, where ROLE is a symbol that specifies the semantics of
SYM; POS is the position of SYM in STREAM; ID is an object that uniquely
identifies (co-)occurrences of SYM in the current defun; and DEF is the
position in which SYM is locally defined, or nil.  If SYM is itself a
binding occurrence, then POS and DEF are equal.  If SYM is not lexically
bound, then DEF is nil.

If STREAM is nil, it defaults to the current buffer.  When reading from
the current buffer, this function leaves point at the end of the form.

This function recursively analyzes Lisp forms (HEAD . TAIL), usually
starting with a top-level form, by inspecting HEAD at each level:

- If HEAD is a symbol with a non-nil `elisp-scope-analyzer' symbol
  property, then the value of that property specifies a bespoke analyzer
  function, AF, that is called as (AF HEAD . TAIL) to analyze the form.
  See more details about writing analyzer functions below.

- If HEAD satisfies `functionp', which means it is a function in the
  running Emacs session, analyze the form as a function call.

- If HEAD is a safe macro (see `elisp-scope-safe-macro-p'), expand it
  and analyze the resulting form.

- If HEAD is unknown, then the arguments in TAIL are ignored, unless
  `elisp-scope-assume-func' is non-nil, in which case they are analyzed
  as evaluated forms (i.e. HEAD is assumed to be a function).

An analyzer (function specified via the `elisp-scope-analyzer' property)
can use the functions `elisp-scope-report-s', `elisp-scope-1' and
`elisp-scope-n' to analyze its arguments, and it can consult the
variable `elisp-scope-output-spec' to obtain the expected output spec of
the analyzed form.  For example, the following is a suitable analyzer
for the `identity' function:

  (lambda (fsym arg)
    (elisp-scope-report-s fsym \\='function)
    (elisp-scope-1 arg elisp-scope-output-spec))"
  (let* ((stream (or stream (current-buffer)))
         (form (read-positioning-symbols stream))
         (elisp-scope--counter 0)
         (elisp-scope--callback callback)
         (max-lisp-eval-depth 32768))
    (if (eq stream (current-buffer))
        ;; `save-excursion' so CALLBACK can change point freely.
        (save-excursion (elisp-scope-1 form))
      (elisp-scope-1 form))))

(provide 'elisp-scope)
;;; elisp-scope.el ends here
