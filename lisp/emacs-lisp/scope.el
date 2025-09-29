;;; scope.el --- Semantic analysis for ELisp symbols  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: lisp, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an analysis that determines the role of each
;; symbol in ELisp code.  The entry point for the analysis is the
;; function `scope', see its docstring for usage information.

;;; Code:

(require 'cl-lib)

(defvar scope--symbol-type-property-cache (make-hash-table))

(defun scope--define-symbol-type (name parents props)
  (clrhash scope--symbol-type-property-cache)
  (put name 'scope-parent-types parents)
  (put name 'scope-type-properties props))

;;;###autoload
(defmacro scope-define-symbol-type (name parents &rest props)
  (declare (indent defun))
  `(scope--define-symbol-type ',name ',parents ,(when props `(list ,@props))))

;;;###autoload
(defun scope-get-symbol-type-property (type prop)
  (with-memoization (alist-get prop (gethash type scope--symbol-type-property-cache))
    (named-let loop ((current type)
                     (parents (get type 'scope-parent-types))
                     (more nil)
                     (done nil))
      (or (plist-get (get current 'scope-type-properties) prop)
          (when-let* ((next (car parents)))
            (loop (car parents) (get next 'scope-parent-types) (append (cdr parents) more) done))
          (when-let* ((next (car more)))
            (loop next (let (res)
                         (dolist (per (get next 'scope-parent-types))
                           (unless (memq per done)
                             (push per res)))
                         (nreverse res))
                  (cdr more) done))))))

;;;###autoload
(defun scope-set-symbol-type-property (type prop value)
  (clrhash scope--symbol-type-property-cache)
  (put type 'scope-type-properties
       (plist-put (get type 'scope-type-properties) prop value)))

;;;###autoload
(defun scope-symbol-type-p (sym)
  (or (get sym 'scope-parent-types) (get sym 'scope-type-properties)))

(defvar scope-read-symbol-type-history nil)

(defun scope-read-symbol-type (prompt &optional default)
  (completing-read
   (format-prompt prompt default)
   obarray #'scope-symbol-type-p 'confirm
   nil 'scope-read-symbol-type-history default))

(defvar help-mode--current-data)

;;;###autoload
(defun scope-describe-symbol-type (type)
  (interactive (list (scope-read-symbol-type
                      "Describe symbol type"
                      (when-let* ((def (symbol-at-point))
                                  ((scope-symbol-type-p def)))
                        def))))
  (when (stringp type) (setq type (intern type)))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'scope-describe-symbol-type type)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert "Symbol type "
                (substitute-quotes (concat "`" (symbol-name type) "'"))
                ":\n\n"
                (substitute-quotes
                 (or (scope-get-symbol-type-property type :doc)
                     "Undocumented.")))
        (when-let* ((parents (get type 'scope-parent-types)))
          (insert "\n\nParent types: "
                  (mapconcat (lambda (parent)
                               (let ((name (symbol-name parent)))
                                 (substitute-quotes
                                  (concat
                                   "`"
                                   (buttonize
                                    name #'scope-describe-symbol-type name
                                    "mouse-2, RET: describe this symbol type")
                                   "'"))))
                             parents ", ")))
        (setq help-mode--current-data
              (list :symbol type :type 'define-symbol-type
                    :file (find-lisp-object-file-name type 'define-symbol-type)))))))

(scope-define-symbol-type symbol-type ()
  :doc "Symbol type names."
  :definition 'symbol-type-definition
  :face 'elisp-symbol-type
  :help (cl-constantly "Symbol type")
  :namespace 'symbol-type)

(scope-define-symbol-type symbol-type-definition (symbol-type)
  :doc "Symbol type name definitions."
  :face 'elisp-symbol-type-definition
  :help (cl-constantly "Symbol type definition")
  :imenu "Symbol Type"
  :namespace 'symbol-type)

(scope-define-symbol-type variable ()
  :doc "Variable names."
  :definition 'defvar
  :face 'elisp-free-variable
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (let ((val (if (boundp sym) (truncate-string-to-width (prin1-to-string (symbol-value sym)) 60 nil nil t) "#<unbound>")))
                  (if-let* ((doc (documentation-property sym 'variable-documentation t)))
                      (format "Special variable `%S'.\n\nValue: %s\n\n%s" sym val doc)
                    (format "Special variable `%S'.\n\nValue: %s" sym val))))
            "Special variable"))
  :namespace 'variable)

(scope-define-symbol-type bound-variable (variable)
  :doc "Local variable names."
  :face 'elisp-bound-variable
  :help (cl-constantly "Local variable"))

(scope-define-symbol-type binding-variable (bound-variable)
  :doc "Local variable definitions."
  :face 'elisp-binding-variable
  :help (cl-constantly "Local variable binding"))

(scope-define-symbol-type shadowed-variable (variable)
  :doc "Locally shadowed variable names."
  :face 'elisp-shadowed-variable
  :help (cl-constantly "Locally shadowed variable"))

(scope-define-symbol-type shadowing-variable (shadowed-variable)
  :doc "Local variable definitions."
  :face 'elisp-shadowing-variable
  :help (cl-constantly "Local variable shadowing"))

(scope-define-symbol-type face ()
  :doc "Face names."
  :definition 'defface
  :face 'elisp-face
  :help (lambda (beg end _def)
          (elisp--help-echo beg end 'face-documentation "Face"))
  :namespace 'face)

(scope-define-symbol-type callable ()
  :doc "Abstract symbol type of function-like symbols."
  :namespace 'function)

(scope-define-symbol-type function (callable)
  :doc "Function names."
  :definition '(defun defcmd)
  :face 'elisp-function-reference
  :help (lambda (beg end def)
          (cond ((equal beg def) "Local function definition")
                (def             "Local function call")
                (t (if-let* ((sym (intern-soft (buffer-substring-no-properties beg end))))
                       (apply-partially #'elisp--function-help-echo sym)
                     "Function call")))))

(scope-define-symbol-type command (function)
  :doc "Command names.")

(scope-define-symbol-type unknown (function)
  :doc "Unknown symbols at function position."
  :face 'elisp-unknown-call
  :help (cl-constantly "Unknown callable"))

(scope-define-symbol-type non-local-exit (function)
  :doc "Functions that do not return."
  :face 'elisp-non-local-exit
  :help (lambda (beg end _def)
          (if-let* ((sym (intern-soft (buffer-substring-no-properties beg end))))
              (apply-partially #'elisp--function-help-echo sym)
            "Non-local exit")))

(scope-define-symbol-type macro (callable)
  :doc "Macro names."
  :definition 'defmacro
  :face 'elisp-macro-call
  :help (lambda (beg end _def)
          (if-let* ((sym (intern-soft (buffer-substring-no-properties beg end))))
              (apply-partially #'elisp--function-help-echo sym)
            "Macro call")))

(scope-define-symbol-type undefined-macro (macro)
  :doc "Known macro names whose definition is unknown."
  :help (cl-constantly "Call to macro with unknown definition"))

(scope-define-symbol-type special-form (callable)
  :doc "Special form names."
  :face 'elisp-special-form
  :help (lambda (beg end _def)
          (if-let* ((sym (intern-soft (buffer-substring-no-properties beg end))))
              (apply-partially #'elisp--function-help-echo sym)
            "Special form")))

(scope-define-symbol-type throw-tag ()
  :doc "Symbols used as `throw'/`catch' tags."
  :face 'elisp-throw-tag
  :help (cl-constantly "`throw'/`catch' tag"))

(scope-define-symbol-type warning-type ()
  :doc "Byte-compilation warning types."
  :face 'elisp-warning-type
  :help (cl-constantly "Warning type"))

(scope-define-symbol-type feature ()
  :doc "Feature names."
  :definition 'deffeature
  :face 'elisp-feature
  :help (cl-constantly "Feature")
  :namespace 'feature)

(scope-define-symbol-type deffeature (feature)
  :doc "Feature definitions."
  :imenu "Feature"
  :help (cl-constantly "Feature definition"))

(scope-define-symbol-type declaration ()
  :doc "Function attribute declaration types."
  :face 'elisp-declaration
  :help (cl-constantly "Declaration"))

(scope-define-symbol-type rx-construct ()
  :doc "`rx' constructs."
  :face 'elisp-rx
  :help (cl-constantly "`rx' construct"))

(scope-define-symbol-type theme ()
  :doc "Custom theme names."
  :definition 'deftheme
  :face 'elisp-theme
  :help (cl-constantly "Theme"))

(scope-define-symbol-type deftheme (theme)
  :doc "Custom theme definitions."
  :imenu "Theme"
  :help (cl-constantly "Theme definition"))

(scope-define-symbol-type thing ()
  :doc "`thing-at-point' \"thing\" identifiers."
  :face 'elisp-thing
  :help (cl-constantly "Thing (text object)"))

(scope-define-symbol-type slot ()
  :doc "EIEIO slots."
  :face 'elisp-slot
  :help (cl-constantly "Slot"))

(scope-define-symbol-type widget-type ()
  :doc "Widget types."
  :definition 'widget-type-definition
  :face 'elisp-widget-type
  :help (cl-constantly "Widget type")
  :namespace 'widget-type)

(scope-define-symbol-type widget-type-definition (widget-type)
  :doc "Widget type definitions."
  :imenu "Widget"
  :help (cl-constantly "Widget type definition"))

(scope-define-symbol-type type ()
  :doc "ELisp object type names."
  :face 'elisp-type
  :help (cl-constantly "Type"))

(scope-define-symbol-type deftype (type)
  :doc "ELisp object type definitions."
  :imenu "Type"
  :help (cl-constantly "Type definition"))

(scope-define-symbol-type group ()
  :doc "Customization groups."
  :definition 'defgroup
  :face 'elisp-group
  :help (cl-constantly "Customization group"))

(scope-define-symbol-type defgroup (group)
  :doc "Customization group definitions."
  :imenu "Group"
  :help (cl-constantly "Customization group definition"))

(scope-define-symbol-type nnoo-backend ()
  :doc "`nnoo' backend names."
  :face 'elisp-nnoo-backend
  :help (cl-constantly "`nnoo' backend"))

(scope-define-symbol-type condition ()
  :doc "`condition-case' conditions."
  :definition 'defcondition
  :face 'elisp-condition
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (let ((msg (get sym 'error-message)))
                  (apply #'concat
                         "`condition-case' condition"
                         (when (and msg (not (string-empty-p msg)))
                           `(": " ,msg)))))
            "`condition-case' condition"))
  :namespace 'condition)

(scope-define-symbol-type defcondition (condition)
  :doc "`condition-case' condition definitions."
  :definition 'defcondition
  :help (cl-constantly "`condition-case' condition definition"))

(scope-define-symbol-type ampersand ()
  :doc "Argument list markers, such as `&optional' and `&rest'."
  :face 'elisp-ampersand
  :help (cl-constantly "Arguments separator"))

(scope-define-symbol-type constant ()
  :doc "Self-evaluating symbols."
  :face 'elisp-constant
  :help (cl-constantly "Constant"))

(scope-define-symbol-type defun ()
  :doc "Function definitions."
  :definition 'defun
  :face 'elisp-defun
  :help (cl-constantly "Function definition")
  :imenu "Function"
  :namespace 'function)

(scope-define-symbol-type defmacro ()
  :doc "Macro definitions."
  :definition 'defmacro
  :face 'elisp-defmacro
  :help (cl-constantly "Macro definition")
  :imenu "Macro"
  :namespace 'function)

(scope-define-symbol-type defcmd (defun)
  :doc "Command definitions."
  :definition 'defcmd
  :help (cl-constantly "Command definition")
  :imenu "Command")

(scope-define-symbol-type defvar ()
  :doc "Variable definitions."
  :definition 'defvar
  :face 'elisp-defvar
  :help (cl-constantly "Special variable definition")
  :imenu "Variable"
  :namespace 'variable)

(scope-define-symbol-type defface ()
  :doc "Face definitions."
  :definition 'defface
  :face 'elisp-defface
  :help (cl-constantly "Face definition")
  :imenu "Face"
  :namespace 'face)

(scope-define-symbol-type major-mode ()
  :doc "Major mode names."
  :definition 'major-mode-definition
  :face 'elisp-major-mode-name
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let* ((doc (documentation sym)))
                    (format "Major mode `%S'.\n\n%s" sym doc)
                  "Major mode"))
            "Major mode"))
  :namespace 'function)

(scope-define-symbol-type major-mode-definition (major-mode)
  :doc "Major mode definitions."
  :help (cl-constantly "Major mode definition")
  :imenu "Major Mode")

(scope-define-symbol-type block ()
  :doc "`cl-block' block names."
  :help (lambda (beg _end def)
          (if (equal beg def) "Block definition" "Block")))

(scope-define-symbol-type icon ()
  :doc "Icon names."
  :definition 'deficon
  :face 'elisp-icon
  :help (cl-constantly "Icon")
  :namespace 'icon)

(scope-define-symbol-type deficon ()
  :doc "Icon definitions."
  :definition 'deficon
  :face 'elisp-deficon
  :help (cl-constantly "Icon definition")
  :imenu "Icon"
  :namespace 'icon)

(scope-define-symbol-type oclosure ()
  :doc "OClosure type names."
  :definition 'defoclosure
  :face 'elisp-oclosure
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let* ((doc (oclosure--class-docstring (get sym 'cl--class))))
                    (format "OClosure type `%S'.\n\n%s" sym doc)
                  "OClosure type"))
            "OClosure type"))
  :namespace 'oclosure)

(scope-define-symbol-type defoclosure ()
  :doc "OClosure type definitions."
  :definition 'defoclosure
  :face 'elisp-defoclosure
  :help (cl-constantly "OClosure type definition")
  :imenu "OClosure type"
  :namespace 'oclosure)

(scope-define-symbol-type coding ()
  :doc "Coding system names."
  :definition 'defcoding
  :face 'elisp-coding
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let* ((doc (coding-system-doc-string sym)))
                    (format "Coding system `%S'.\n\n%s" sym doc)
                  "Coding system"))
            "Coding system"))
  :namespace 'coding)

(scope-define-symbol-type defcoding ()
  :doc "Coding system definitions."
  :definition 'defcoding
  :face 'elisp-defcoding
  :help (cl-constantly "Coding system definition")
  :imenu "Coding system"
  :namespace 'coding)

(scope-define-symbol-type charset ()
  :doc "Charset names."
  :definition 'defcharset
  :face 'elisp-charset
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let* ((doc (charset-description sym)))
                    (format "Charset `%S'.\n\n%s" sym doc)
                  "Charset"))
            "Charset"))
  :namespace 'charset)

(scope-define-symbol-type defcharset ()
  :doc "Charset definitions."
  :definition 'defcharset
  :face 'elisp-defcharset
  :help (cl-constantly "Charset definition")
  :imenu "Charset"
  :namespace 'charset)

(scope-define-symbol-type completion-category ()
  :doc "Completion categories."
  :definition 'completion-category-definition
  :face 'elisp-completion-category
  :help (lambda (beg end _def)
          (if-let* ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let* ((doc (get sym 'completion-category-documentation)))
                    (format "Completion category `%S'.\n\n%s" sym doc)
                  "Completion category"))
            "Completion category"))
  :namespace 'completion-category)

(scope-define-symbol-type completion-category-definition ()
  :doc "Completion category definitions."
  :definition 'completion-category-definition
  :face 'elisp-completion-category-definition
  :help (cl-constantly "Completion category definition")
  :imenu "Completion category"
  :namespace 'completion-category)

(defvar scope-counter nil)

(defvar scope-local-functions nil)

(defvar scope--local nil)

(defvar scope--output-type nil)

(defvar scope-callback #'ignore)

(defvar scope-current-let-alist-form nil)

(defvar scope-gen-id-alist nil)

(defsubst scope-local-new (sym pos &optional local)
  "Return new local context with SYM bound at POS.

Optional argument LOCAL is a local context to extend."
  (cons (cons sym (or pos (cons 'gen (incf scope-counter)))) local))

(defsubst scope-sym-pos (sym)
  (when (symbol-with-pos-p sym) (symbol-with-pos-pos sym)))

(defsubst scope-sym-bare (sym)
  (cond
   ((symbolp sym) sym)
   ((symbol-with-pos-p sym) (bare-symbol sym))))

(defvar scope--quoted nil)

(defsubst scope-report (type beg len &optional id def)
  (funcall scope-callback type beg len id (or def (and (numberp id) id))))

(defvar scope-special-variables nil)

(defun scope-special-variable-p (sym)
  (or (memq sym scope-special-variables) (special-variable-p sym)))

(defun scope-variable (sym beg len id)
  (scope-report
   (if id (if (scope-special-variable-p sym) 'shadowed-variable 'bound-variable) 'variable)
   beg len id))

(defun scope-binding (sym beg len)
  (scope-report
   (if (scope-special-variable-p sym) 'shadowing-variable 'binding-variable)
   beg len beg))

(defun scope-s (sym)
  (let* ((beg (scope-sym-pos sym))
         (bare (scope-sym-bare sym))
         (name (symbol-name bare))
         (len (length name)))
    (when (and beg (not (booleanp bare)))
      (cond
       ((keywordp bare) (scope-report 'constant beg len))
       ((and scope-current-let-alist-form (= (aref name 0) ?.))
        (if (and (length> name 1) (= (aref name 1) ?.))
            ;; Double dot escapes `let-alist'.
            (let* ((unescaped (intern (substring name 1))))
              (scope-variable unescaped beg len (alist-get unescaped scope--local)))
          (scope-report 'bound-variable beg len
                        (list 'let-alist (car scope-current-let-alist-form) bare)
                        (cdr scope-current-let-alist-form))))
       (t (scope-variable bare beg len (alist-get bare scope--local)))))))

(defun scope-let-1 (local bindings body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (scope-sym-bare sym))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos sym)))
        (when beg (scope-binding bare beg len))
        (scope-1 (cadr binding))
        (scope-let-1 (if bare (scope-local-new bare beg local) local)
                     (cdr bindings) body))
    (let ((scope--local local))
      (scope-n body scope--output-type))))

(defun scope-let (bindings body)
  (scope-let-1 scope--local bindings body))

(defun scope-let* (bindings body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (bare-symbol sym))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos sym)))
        (when beg (scope-binding bare beg len))
        (scope-1 (cadr binding))
        (let ((scope--local (scope-local-new bare beg scope--local)))
          (scope-let* (cdr bindings) body)))
    (scope-n body scope--output-type)))

(defun scope-interactive (intr spec modes)
  (when (symbol-with-pos-p intr)
    (scope-report 'special-form
                  (symbol-with-pos-pos intr)
                  (length (symbol-name (scope-sym-bare intr)))))
  (scope-1 spec)
  (mapc #'scope-major-mode-name modes))

(defun scope-lambda (args body &optional outtype)
  (let ((l scope--local))
    (when (listp args)
      (dolist (arg args)
        (when-let* ((bare (bare-symbol arg))
                    (beg (scope-sym-pos arg)))
          (unless (memq bare '(&optional &rest))
            (setq l (scope-local-new bare beg l))))))
    ;; Handle docstring.
    (cond
     ((and (consp (car body))
           (or (symbol-with-pos-p (caar body))
               (symbolp (caar body)))
           (eq (bare-symbol (caar body)) :documentation))
      (scope-s (caar body))
      (scope-1 (cadar body))
      (setq body (cdr body)))
     ((stringp (car body)) (setq body (cdr body))))
    ;; Handle `declare'.
    (when-let* ((form (car body))
                (decl (car-safe form))
                ((or (symbol-with-pos-p decl)
                     (symbolp decl)))
                ((eq (bare-symbol decl) 'declare)))
      (when (symbol-with-pos-p decl)
        (scope-report 'macro
                      (symbol-with-pos-pos decl)
                      (length (symbol-name (bare-symbol decl)))))
      (dolist (spec (cdr form))
        (when-let* ((head (car-safe spec))
                    (bare (scope-sym-bare head)))
          (when (symbol-with-pos-p head)
            (scope-report 'declaration
                          (symbol-with-pos-pos head)
                          (length (symbol-name bare))))
          (cl-case bare
            (completion (scope-sharpquote (cadr spec)))
            (interactive-only
             (when-let* ((bare (scope-sym-bare (cadr spec)))
                         ((not (eq bare t))))
               (scope-sharpquote (cadr spec))))
            (obsolete
             (when-let* ((bare (scope-sym-bare (cadr spec))))
               (scope-sharpquote (cadr spec))))
            ((compiler-macro gv-expander gv-setter)
             ;; Use the extended lexical environment `l'.
             (let ((scope--local l))
               (scope-sharpquote (cadr spec))))
            (modes (mapc #'scope-major-mode-name (cdr spec)))
            (interactive-args
             (dolist (arg-form (cdr spec))
               (when-let* ((arg (car-safe arg-form)))
                 (let ((scope--local l)) (scope-s arg))
                 (when (consp (cdr arg-form))
                   (scope-1 (cadr arg-form)))))))))
      (setq body (cdr body)))
    ;; Handle `interactive'.
    (when-let* ((form (car body))
                (intr (car-safe form))
                ((or (symbol-with-pos-p intr)
                     (symbolp intr)))
                ((eq (bare-symbol intr) 'interactive)))
      (scope-interactive intr (cadar body) (cddar body))
      (setq body (cdr body)))
    ;; Handle ARGS.
    (when (listp args)
      (dolist (arg args)
        (and (symbol-with-pos-p arg)
             (let* ((beg (symbol-with-pos-pos arg))
                    (bare (bare-symbol arg))
                    (len (length (symbol-name bare))))
               (when (and beg (not (eq bare '_)))
                 (if (memq bare '(&optional &rest))
                     (scope-report 'ampersand beg len)
                   (scope-report 'binding-variable beg len beg)))))))
    ;; Handle BODY.
    (let ((scope--local l)) (scope-n body outtype))))

(defun scope-defun (name args body)
  (when-let* ((beg (scope-sym-pos name))
              (bare (scope-sym-bare name)))
    (scope-report
     (let ((tmp body))
       (when (stringp (car-safe tmp)) (pop tmp))
       (when (eq 'declare (scope-sym-bare (car-safe (car-safe tmp)))) (pop tmp))
       (if (eq 'interactive (scope-sym-bare (car-safe (car-safe tmp))))
           'defcmd
         'defun))
     beg (length (symbol-name bare))))
  (scope-lambda args body))

(defun scope-setq (args) (scope-n args scope--output-type))

(defvar scope-flet-alist nil)

(defun scope-flet (defs body)
  (if defs
      (let* ((def (car defs))
             (func (car def))
             (exps (cdr def))
             (beg (scope-sym-pos func))
             (bare (bare-symbol func)))
        (when beg
          (scope-report 'function beg (length (symbol-name bare)) beg))
        (if (cdr exps)
            ;; def is (FUNC ARGLIST BODY...)
            (scope-cl-lambda (car exps) (cdr exps))
          ;; def is (FUNC EXP)
          (scope-1 (car exps)))
        (let ((scope-flet-alist (scope-local-new bare beg scope-flet-alist)))
          (scope-flet (cdr defs) body)))
    (scope-n body)))

(defun scope-labels (defs forms)
  (if defs
      (let* ((def (car defs))
             (func (car def))
             (args (cadr def))
             (body (cddr def))
             (beg (scope-sym-pos func))
             (bare (bare-symbol func)))
        (when beg
          (scope-report 'function beg (length (symbol-name bare)) beg))
        (let ((scope-flet-alist (scope-local-new bare beg scope-flet-alist)))
          (scope-lambda args body)
          (scope-flet (cdr defs) forms)))
    (scope-n forms)))

(defvar scope-block-alist nil)

(defun scope-block (name body)
  (if name
      (let* ((beg (scope-sym-pos name))
             (bare (bare-symbol name)))
        (when beg
          (scope-report 'block beg (length (symbol-name bare)) beg))
        (let ((scope-block-alist (scope-local-new bare beg scope-block-alist)))
          (scope-n body)))
    (scope-n body)))

(defun scope-return-from (name result)
  (when-let* ((bare (and (symbol-with-pos-p name) (bare-symbol name)))
              (pos (alist-get bare scope-block-alist)))
    (scope-report 'block
                  (symbol-with-pos-pos name) (length (symbol-name bare)) pos))
  (scope-1 result))

(defvar scope-assume-func nil)

(defun scope-sharpquote (arg)
  (cond
   ((or (symbol-with-pos-p arg) (symbolp arg))
    (let ((bare (bare-symbol arg)))
      (cond
       ((or (functionp bare) (memq bare scope-local-functions) (assq bare scope-flet-alist) scope-assume-func)
        (scope-report-s arg 'function))
       (t (scope-report-s arg 'unknown)))))
   ((consp arg) (scope-1 arg))))

(defun scope-loop-for-and (rest)
  (if (eq (scope-sym-bare (car rest)) 'and)
      (scope-loop-for scope--local (cadr rest) (cddr rest))
    (scope-loop rest)))

(defun scope-loop-for-by (local expr rest)
  (scope-1 expr)
  (let ((scope--local local))
    (scope-loop-for-and rest)))

(defun scope-loop-for-to (local expr rest)
  (scope-1 expr)
  (when-let* ((bare (scope-sym-bare (car rest)))
              (more (cdr rest)))
    (cond
     ((eq bare 'by)
      (scope-loop-for-by local (car more) (cdr more)))
     (t (let ((scope--local local))
          (scope-loop-for-and rest))))))

(defun scope-loop-for-from (local expr rest)
  (scope-1 expr)
  (when-let* ((bare (scope-sym-bare (car rest)))
              (more (cdr rest)))
    (cond
     ((memq bare '(to upto downto below above))
      (scope-loop-for-to local (car more) (cdr more)))
     ((eq bare 'by)
      (scope-loop-for-by local (car more) (cdr more)))
     (t (let ((scope--local local))
          (scope-loop-for-and rest))))))

(defun scope-loop-for-= (local expr rest)
  (scope-1 expr)
  (when-let* ((bare (scope-sym-bare (car rest)))
              (more (cdr rest)))
    (cond
     ((eq bare 'then)
      (scope-loop-for-by local (car more) (cdr more)))
     (t (let ((scope--local local))
          (scope-loop-for-and rest))))))

(defun scope-loop-for-being-the-hash-keys-of-using (form rest)
  (let* ((var (cadr form))
         (bare (scope-sym-bare var))
         (beg (scope-sym-pos var)))
    (when beg (scope-binding bare beg (length (symbol-name bare))))
    (let ((scope--local (scope-local-new bare beg scope--local)))
      (scope-loop-for-and rest))))

(defun scope-loop-for-being-the-hash-keys-of (local expr rest)
  (scope-1 expr)
  (when-let* ((bare (scope-sym-bare (car rest)))
              (more (cdr rest)))
    (let ((scope--local local))
      (cond
       ((eq bare 'using)
        (scope-loop-for-being-the-hash-keys-of-using (car more) (cdr more)))
       (t (scope-loop-for-and rest))))))

(defun scope-loop-for-being-the-hash-keys (local word rest)
  (when-let* ((bare (scope-sym-bare word)))
    (cond
     ((eq bare 'of)
      (scope-loop-for-being-the-hash-keys-of local (car rest) (cdr rest))))))

(defun scope-loop-for-being-the (local word rest)
  (when-let* ((bare (scope-sym-bare word)))
    (cond
     ((memq bare '(buffer buffers))
      (let ((scope--local local))
        (scope-loop-for-and rest)))
     ((memq bare '( hash-key hash-keys
                    hash-value hash-values
                    key-code key-codes
                    key-binding key-bindings))
      (scope-loop-for-being-the-hash-keys local (car rest) (cdr rest))))))

(defun scope-loop-for-being (local next rest)
  (scope-loop-for-being-the
   local (car rest)
   (if (memq (scope-sym-bare next) '(the each)) (cdr rest) rest)))

(defun scope-loop-for (local vars rest)
  (if vars
      ;; FIXME: var need not be a symbol, see
      ;; `cl-macs-loop-destructure-cons' test in cl-macs-tests.el.
      (let* ((var (car (ensure-list vars)))
             (bare (bare-symbol var))
             (beg (scope-sym-pos var)))
        (when beg (scope-binding bare beg (length (symbol-name bare))))
        (scope-loop-for (scope-local-new bare beg local) (cdr-safe vars) rest))
    (when-let* ((bare (scope-sym-bare (car rest)))
                (more (cdr rest)))
      (cond
       ((memq bare '(from upfrom downfrom))
        (scope-loop-for-from local (car more) (cdr more)))
       ((memq bare '( to upto downto below above
                      in on in-ref))
        (scope-loop-for-to local (car more) (cdr more)))
       ((memq bare '(by
                     across across-ref))
        (scope-loop-for-by local (car more) (cdr more)))
       ((eq bare '=)
        (scope-loop-for-= local (car more) (cdr more)))
       ((eq bare 'being)
        (scope-loop-for-being local (car more) (cdr more)))))))

(defun scope-loop-repeat (form rest)
  (scope-1 form)
  (scope-loop rest))

(defvar scope-loop-into-vars nil)

(defun scope-loop-collect (expr rest)
  (scope-1 expr)
  (let ((bw (scope-sym-bare (car rest)))
        (more (cdr rest)))
    (if (eq bw 'into)
        (let* ((var (car more))
               (bare (scope-sym-bare var))
               (beg (scope-sym-pos var)))
          (if (memq bare scope-loop-into-vars)
              (progn
                (scope-s var)
                (scope-loop (cdr more)))
            (when beg (scope-binding bare beg (length (symbol-name bare))))
            (let ((scope-loop-into-vars (cons bare scope-loop-into-vars))
                  (scope--local (scope-local-new bare beg scope--local)))
              (scope-loop (cdr more)))))
      (scope-loop rest))))

(defun scope-loop-with-and (rest)
  (if (eq (scope-sym-bare (car rest)) 'and)
      (scope-loop-with (cadr rest) (cddr rest))
    (scope-loop rest)))

(defun scope-loop-with (var rest)
  (let* ((bare (scope-sym-bare var))
         (beg (symbol-with-pos-pos var))
         (l (scope-local-new bare beg scope--local))
         (eql (car rest)))
    (when beg (scope-binding bare beg (length (symbol-name bare))))
    (if (eq (scope-sym-bare eql) '=)
        (let* ((val (cadr rest)) (more (cddr rest)))
          (scope-1 val)
          (let ((scope--local l))
            (scope-loop-with-and more)))
      (let ((scope--local l))
        (scope-loop-with-and rest)))))

(defun scope-loop-do (form rest)
  (scope-1 form)
  (if (consp (car rest))
      (scope-loop-do (car rest) (cdr rest))
    (scope-loop rest)))

(defun scope-loop-named (name rest)
  (let* ((beg (scope-sym-pos name))
         (bare (scope-sym-bare name)))
    (when beg
      (scope-report 'block beg (length (symbol-name bare)) beg))
    (let ((scope-block-alist (scope-local-new bare beg scope-block-alist)))
      (scope-loop rest))))

(defun scope-loop-finally (next rest)
  (if-let* ((bare (scope-sym-bare next)))
      (cond
       ((eq bare 'do)
        (scope-loop-do (car rest) (cdr rest)))
       ((eq bare 'return)
        (scope-1 (car rest))
        (scope-loop (cdr rest))))
    (if (eq (scope-sym-bare (car-safe next)) 'return)
        (progn
          (scope-1 (cadr next))
          (scope-loop (cdr rest)))
      (scope-loop-do next rest))))

(defun scope-loop-initially (next rest)
  (if (eq (scope-sym-bare next) 'do)
      (scope-loop-do (car rest) (cdr rest))
    (scope-loop-do next rest)))

(defvar scope-loop-if-depth 0)

(defun scope-loop-if (keyword condition rest)
  (scope-1 condition)
  (let ((scope-loop-if-depth (1+ scope-loop-if-depth))
        (scope--local
         ;; `if' binds `it'.
         (scope-local-new 'it (scope-sym-pos keyword) scope--local)))
    (scope-loop rest)))

(defun scope-loop-end (rest)
  (let ((scope-loop-if-depth (1- scope-loop-if-depth)))
    (unless (minusp scope-loop-if-depth)
      (scope-loop rest))))

(defun scope-loop-and (rest)
  (when (plusp scope-loop-if-depth) (scope-loop rest)))

(defun scope-loop (forms)
  (when forms
    (let* ((kw (car forms))
           (bare (scope-sym-bare kw))
           (rest (cdr forms)))
      (cond
       ((memq bare '(for as))
        (scope-loop-for scope--local (car rest) (cdr rest)))
       ((memq bare '( repeat while until always never thereis iter-by
                      return))
        (scope-loop-repeat (car rest) (cdr rest)))
       ((memq bare '(collect append nconc concat vconcat count sum maximize minimize))
        (scope-loop-collect (car rest) (cdr rest)))
       ((memq bare '(with))
        (scope-loop-with (car rest) (cdr rest)))
       ((memq bare '(do)) (scope-loop-do (car rest) (cdr rest)))
       ((memq bare '(named)) (scope-loop-named (car rest) (cdr rest)))
       ((memq bare '(finally)) (scope-loop-finally (car rest) (cdr rest)))
       ((memq bare '(initially)) (scope-loop-initially (car rest) (cdr rest)))
       ((memq bare '(if when unless)) (scope-loop-if kw (car rest) (cdr rest)))
       ((memq bare '(end)) (scope-loop-end rest))
       ((memq bare '(and else)) (scope-loop-and rest))))))

(defun scope-named-let (name bindings body &optional outtype)
  (let ((bare (scope-sym-bare name))
        (beg (scope-sym-pos name)))
    (when beg
      (scope-report 'function beg (length (symbol-name bare)) beg))
    (dolist (binding bindings)
      (let* ((sym (car (ensure-list binding)))
             (beg (symbol-with-pos-pos sym))
             (bare (bare-symbol sym)))
        (when beg (scope-binding bare beg (length (symbol-name bare))))
        (scope-1 (cadr binding))))
    (let ((l scope--local))
      (dolist (binding bindings)
        (when-let* ((sym (car (ensure-list binding)))
                    (bare (scope-sym-bare sym)))
          (setq l (scope-local-new bare (scope-sym-pos sym) l))))
      (let ((scope-flet-alist (scope-local-new bare beg scope-flet-alist))
            (scope--local l))
        (scope-n body outtype)))))

(defun scope-with-slots (spec-list object body)
  (scope-1 object)
  (scope-let spec-list body))

(defun scope-rx (regexps)
  (dolist (regexp regexps) (scope-rx-1 regexp)))

(defvar scope-rx-alist nil)

(defun scope-rx-1 (regexp)
  (if (consp regexp)
      (let* ((head (car regexp))
             (bare (scope-sym-bare head)))
        (when (and bare (symbol-with-pos-p head))
          (scope-report 'rx-construct
                        (symbol-with-pos-pos head) (length (symbol-name bare))
                        (alist-get bare scope-rx-alist)))
        (cond
         ((memq bare '(literal regex regexp eval))
          (scope-1 (cadr regexp)))
         ((memq bare '( seq sequence and :
                        or |
                        zero-or-more 0+ * *?
                        one-or-more 1+ + +?
                        zero-or-one optional opt \? \??
                        = >= ** repeat
                        minimal-match maximal-match
                        group submatch
                        group-n submatch-n))
          (scope-rx (cdr regexp)))))
    (when-let* (((symbol-with-pos-p regexp))
                (bare (scope-sym-bare regexp)))
      (scope-report 'rx-construct
                    (symbol-with-pos-pos regexp) (length (symbol-name bare))
                    (alist-get bare scope-rx-alist)))))

(defun scope-rx-define (name rest)
  (when-let* ((bare (scope-sym-bare name)))
    (scope-report 'rx-construct
                  (symbol-with-pos-pos name) (length (symbol-name bare)) nil))
  (if (not (cdr rest))
      (scope-rx-1 (car rest))
    (let ((l scope-rx-alist)
          (args (car rest))
          (rx (cadr rest)))
      (dolist (arg args)
        (and (symbol-with-pos-p arg)
             (let* ((beg (symbol-with-pos-pos arg))
                    (bare (bare-symbol arg))
                    (len (length (symbol-name bare))))
               (when beg
                 (if (memq (bare-symbol arg) '(&optional &rest _))
                     (scope-report 'ampersand beg len)
                   (scope-report 'rx-construct beg len beg))))))
      (dolist (arg args)
        (when-let* ((bare (bare-symbol arg))
                    (beg (scope-sym-pos arg)))
          (unless (memq bare '(&optional &rest))
            (setq l (scope-local-new bare beg l)))))
      (let ((scope-rx-alist l))
        (scope-rx-1 rx)))))

(defun scope-rx-let (bindings body)
  (if-let* ((binding (car bindings)))
      (let ((name (car binding)) (rest (cdr binding)))
        (when-let* ((bare (scope-sym-bare name))
                    (beg (symbol-with-pos-pos name)))
          (scope-report 'rx-construct
                        beg (length (symbol-name bare)) beg))
        (if (cdr rest)
            (let ((l scope-rx-alist)
                  (args (car rest))
                  (rx (cadr rest)))
              (dolist (arg args)
                (and (symbol-with-pos-p arg)
                     (let* ((beg (symbol-with-pos-pos arg))
                            (bare (bare-symbol arg))
                            (len (length (symbol-name bare))))
                       (when beg
                         (if (memq (bare-symbol arg) '(&optional &rest _))
                             (scope-report 'ampersand beg len)
                           (scope-report 'rx-construct beg len beg))))))
              (dolist (arg args)
                (when-let* ((bare (bare-symbol arg))
                            (beg (scope-sym-pos arg)))
                  (unless (memq bare '(&optional &rest))
                    (setq l (scope-local-new bare beg l)))))
              (let ((scope-rx-alist l))
                (scope-rx-1 rx))
              (let ((scope-rx-alist (scope-local-new (scope-sym-bare name)
                                                     (scope-sym-pos name)
                                                     scope-rx-alist)))
                (scope-rx-let (cdr bindings) body)))
          (scope-rx-1 (car rest))
          (let ((scope-rx-alist (scope-local-new (scope-sym-bare name)
                                                 (scope-sym-pos name)
                                                 scope-rx-alist)))
            (scope-rx-let (cdr bindings) body))))
    (scope-n body)))

(defun scope-gv-define-expander (name handler)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'defun beg (length (symbol-name bare))))
  (scope-1 handler))

(defun scope-gv-define-simple-setter (name setter rest)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'defun beg (length (symbol-name bare))))
  (when-let* ((beg (scope-sym-pos setter)) (bare (scope-sym-bare setter)))
    (scope-report 'function beg (length (symbol-name bare))))
  (scope-n rest))

(defun scope-face (face)
  (if (or (scope-sym-bare face)
          (keywordp (scope-sym-bare (car-safe face))))
      (scope-face-1 face)
    (mapc #'scope-face-1 face)))

(defun scope-face-1 (face)
  (cond
   ((symbol-with-pos-p face)
    (when-let* ((beg (scope-sym-pos face)) (bare (scope-sym-bare face)))
      (scope-report 'face beg (length (symbol-name bare)))))
   ((keywordp (scope-sym-bare (car-safe face)))
    (let ((l face))
      (while l
        (let ((kw (car l))
              (vl (cadr l)))
          (setq l (cddr l))
          (when-let* ((bare (scope-sym-bare kw))
                      ((keywordp bare)))
            (when-let* ((beg (scope-sym-pos kw))
                        (len (length (symbol-name bare))))
              (scope-report 'constant beg len))
            (when (eq bare :inherit)
              (when-let* ((beg (scope-sym-pos vl)) (fbare (scope-sym-bare vl)))
                (scope-report 'face beg (length (symbol-name fbare))))))))))))

(defun scope-deftype (name args body)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'deftype beg (length (symbol-name bare))))
  (scope-lambda args body))

(defun scope-widget-type (form)
  (when-let* (((memq (scope-sym-bare (car-safe form)) '(quote \`)))
              (type (cadr form)))
    (scope-widget-type-1 type)))

(defun scope-widget-type-1 (type)
  (cond
   ((symbol-with-pos-p type)
    (when-let* ((beg (scope-sym-pos type)) (bare (scope-sym-bare type)))
      (scope-report 'widget-type
                    (symbol-with-pos-pos type)
                    (length (symbol-name (bare-symbol type))))))
   ((consp type)
    (let ((head (car type)))
      (when-let* ((beg (scope-sym-pos head)) (bare (scope-sym-bare head)))
        (scope-report 'widget-type beg (length (symbol-name bare))))
      (when-let* ((bare (scope-sym-bare head)))
        (scope-widget-type-arguments bare (cdr type)))))))

(defun scope-widget-type-keyword-arguments (head kw args)
  (when-let* ((beg (scope-sym-pos kw))
              (len (length (symbol-name (bare-symbol kw)))))
    (scope-report 'constant beg len))
  (cond
   ((and (memq head '(plist alist))
         (memq kw   '(:key-type :value-type)))
    (scope-widget-type-1 (car args)))
   ((memq kw '(:action :match :match-inline :validate))
    (when-let* ((fun (car args))
                (beg (scope-sym-pos fun))
                (bare (scope-sym-bare fun)))
      (scope-report 'function beg (length (symbol-name bare)))))
   ((memq kw '(:args))
    (mapc #'scope-widget-type-1 (car args))))
  ;; TODO: (restricted-sexp :match-alternatives CRITERIA)
  (scope-widget-type-arguments head (cdr args)))

(defun scope-widget-type-arguments (head args)
  (let* ((arg (car args))
         (bare (scope-sym-bare arg)))
    (if (keywordp bare)
        (scope-widget-type-keyword-arguments head bare (cdr args))
      (scope-widget-type-arguments-1 head args))))

(defun scope-widget-type-arguments-1 (head args)
  (cl-case head
    ((list cons group vector choice radio set repeat checklist)
     (mapc #'scope-widget-type-1 args))
    ((function-item)
     (when-let* ((fun (car args))
                 (beg (scope-sym-pos fun))
                 (bare (scope-sym-bare fun)))
       (scope-report 'function beg (length (symbol-name bare)))))
    ((variable-item)
     (when-let* ((var (car args))
                 (beg (scope-sym-pos var))
                 (bare (scope-sym-bare var)))
       (scope-report 'variable beg (length (symbol-name bare)))))))

(defun scope-quoted-group (sym-form)
  (when-let* (((eq (scope-sym-bare (car-safe sym-form)) 'quote))
              (sym (cadr sym-form))
              (beg (scope-sym-pos sym))
              (bare (scope-sym-bare sym)))
    (scope-report 'group beg (length (symbol-name bare)))))

(defun scope-defmethod-1 (local args body)
  (if args
      (let ((arg (car args)) (bare nil))
        (cond
         ((consp arg)
          (let* ((var (car arg))
                 (spec (cadr arg)))
            (cond
             ((setq bare (scope-sym-bare var))
              (when-let* ((beg (scope-sym-pos var))
                          (len (length (symbol-name bare))))
                (scope-binding bare beg len))
              (cond
               ((consp spec)
                (let ((head (car spec)) (form (cadr spec)))
                  (and (eq 'eql (scope-sym-bare head))
                       (not (or (symbolp form) (symbol-with-pos-p form)))
                       (scope-1 form))))
               ((symbol-with-pos-p spec)
                (when-let* ((beg (symbol-with-pos-pos spec))
                            (bare (bare-symbol spec))
                            (len (length (symbol-name bare))))
                  (scope-report 'type beg len))))
              (scope-defmethod-1 (scope-local-new bare (scope-sym-pos var) local)
                                 (cdr args) body)))))
         ((setq bare (scope-sym-bare arg))
          (cond
           ((memq bare '(&optional &rest &body _))
            (when-let* ((beg (scope-sym-pos arg)))
              (scope-report 'ampersand beg (length (symbol-name bare))))
            (scope-defmethod-1 local (cdr args) body))
           ((eq bare '&context)
            (let* ((expr-type (cadr args))
                   (expr (car expr-type))
                   (spec (cadr expr-type))
                   (more (cddr args)))
              (when-let* ((beg (scope-sym-pos arg)))
                (scope-report 'ampersand beg (length (symbol-name bare))))
              (scope-1 expr)
              (cond
               ((consp spec)
                (let ((head (car spec)) (form (cadr spec)))
                  (and (eq 'eql (scope-sym-bare head))
                       (not (or (symbolp form) (symbol-with-pos-p form)))
                       (scope-1 form))))
               ((symbol-with-pos-p spec)
                (when-let* ((beg (symbol-with-pos-pos spec))
                            (bare (bare-symbol spec))
                            (len (length (symbol-name bare))))
                  (scope-report 'type beg len beg))))
              (scope-defmethod-1 local more body)))
           (t
            (when-let* ((beg (scope-sym-pos arg))
                        (len (length (symbol-name bare))))
              (scope-binding bare beg len))
            (scope-defmethod-1 (scope-local-new bare (scope-sym-pos arg) local)
                               (cdr args) body))))))
    (let ((scope--local local))
      (scope-n body))))

;; (defun scope-defmethod (local name rest)
;;   (when (and (symbol-with-pos-p (car rest))
;;              (eq (bare-symbol (car rest)) :extra))
;;     (setq rest (cddr rest)))
;;   (when (and (symbol-with-pos-p (car rest))
;;              (memq (bare-symbol (car rest)) '(:before :after :around)))
;;     (setq rest (cdr rest)))
;;   (scope-defmethod-1 local local name (car rest)
;;                      (if (stringp (cadr rest)) (cddr rest) (cdr rest))))

(defun scope-defmethod (name rest)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'defun beg (length (symbol-name bare))))
  ;; [EXTRA]
  (when (eq (scope-sym-bare (car rest)) :extra)
    (scope-s (car rest))
    (setq rest (cddr rest)))
  ;; [QUALIFIER]
  (when (keywordp (scope-sym-bare (car rest)))
    (scope-s (car rest))
    (setq rest (cdr rest)))
  ;; ARGUMENTS
  (scope-defmethod-1 scope--local (car rest) (cdr rest)))

(defun scope-cl-defun (name arglist body)
  (let ((beg (scope-sym-pos name))
        (bare (scope-sym-bare name)))
    (when beg (scope-report 'defun beg (length (symbol-name bare))))
    (let ((scope-block-alist (scope-local-new bare beg scope-block-alist)))
      (scope-cl-lambda arglist body))))

(defun scope-cl-lambda (arglist body)
  (scope-cl-lambda-1 arglist nil body))

(defun scope-cl-lambda-1 (arglist more body)
  (cond
   (arglist
    (if (consp arglist)
        (let ((head (car arglist)))
          (if (consp head)
              (scope-cl-lambda-1 head (cons (cdr arglist) more) body)
            (let ((bare (scope-sym-bare head)))
              (if (memq bare '(&optional &rest &body &key &aux &whole &cl-defs &cl-quote))
                  (progn
                    (when-let* ((beg (scope-sym-pos head)))
                      (scope-report 'ampersand beg (length (symbol-name bare))))
                    (cl-case bare
                      (&optional (scope-cl-lambda-optional (cadr arglist) (cddr arglist) more body))
                      (&cl-defs (scope-cl-lambda-defs (cadr arglist) (cddr arglist) more body))
                      ((&rest &body) (scope-cl-lambda-rest (cadr arglist) (cddr arglist) more body))
                      (&key (scope-cl-lambda-key (cadr arglist) (cddr arglist) more body))
                      (&aux (scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body))
                      (&whole (scope-cl-lambda-1 (cdr arglist) more body))))
                (when-let* ((beg (scope-sym-pos head)))
                  (scope-binding bare beg (length (symbol-name bare))))
                (let ((scope--local (scope-local-new bare (scope-sym-pos head) scope--local)))
                  (scope-cl-lambda-1 (cdr arglist) more body))))))
      (scope-cl-lambda-1 (list '&rest arglist) more body)))
   (more (scope-cl-lambda-1 (car more) (cdr more) body))
   (t (scope-lambda nil body))))

(defun scope-cl-lambda-defs (arg arglist more body)
  (when (consp arg)
    (let ((def (car arg))
          (defs (cdr arg)))
      (scope-1 def)
      (dolist (d defs) (scope-n (cdr-safe d)))))
  (scope-cl-lambda-1 arglist more body))

(defun scope-cl-lambda-optional (arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l scope--local)
         (init (cadr a))
         (svar (caddr a)))
    (scope-1 init)
    (if (consp var)
        (let ((scope--local l))
          (scope-cl-lambda-1 var (cons (append (when svar (list svar))
                                               (cons '&optional arglist))
                                       more)
                             body))
      (when-let* ((bare (scope-sym-bare svar)))
        (when-let* ((beg (scope-sym-pos svar)))
          (scope-binding bare beg (length (symbol-name bare))))
        (setq l (scope-local-new bare (scope-sym-pos svar) l)))
      (when-let* ((bare (scope-sym-bare var)))
        (when-let* ((beg (scope-sym-pos var)))
          (scope-binding bare beg (length (symbol-name bare))))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let* ((bare (scope-sym-bare head))
                    ((memq bare '(&rest &body &key &aux))))
              (progn
                (when-let* ((beg (scope-sym-pos head)))
                  (scope-report 'ampersand beg (length (symbol-name bare))))
                (cl-case bare
                  ((&rest &body)
                   (let ((scope--local l))
                     (scope-cl-lambda-rest (cadr arglist) (cddr arglist) more body)))
                  (&key (let ((scope--local l))
                          (scope-cl-lambda-key (cadr arglist) (cddr arglist) more body)))
                  (&aux (let ((scope--local l))
                          (scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body)))))
            (let ((scope--local l))
              (scope-cl-lambda-optional head (cdr arglist) more body)))))
       (more
        (let ((scope--local l))
          (scope-cl-lambda-1 (car more) (cdr more) body)))
       (t (let ((scope--local l)) (scope-lambda nil body)))))))

(defun scope-cl-lambda-rest (var arglist more body)
  (let* ((l scope--local))
    (if (consp var)
        (scope-cl-lambda-1 var (cons arglist more) body)
      (when-let* ((bare (scope-sym-bare var)))
        (when-let* ((beg (scope-sym-pos var)))
          (scope-binding bare beg (length (symbol-name bare))))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let* ((bare (scope-sym-bare head))
                    ((memq bare '(&key &aux))))
              (progn
                (when-let* ((beg (scope-sym-pos head)))
                  (scope-report 'ampersand beg (length (symbol-name bare))))
                (cl-case bare
                  (&key
                   (let ((scope--local l))
                     (scope-cl-lambda-key (cadr arglist) (cddr arglist) more body)))
                  (&aux
                   (let ((scope--local l))
                     (scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body)))))
            (let ((scope--local l))
              (scope-cl-lambda-1 (car more) (cdr more) body)))))
       (more (let ((scope--local l))
               (scope-cl-lambda-1 (car more) (cdr more) body)))
       (t (let ((scope--local l))
            (scope-lambda nil body)))))))

(defun scope-cl-lambda-key (arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l scope--local)
         (init (cadr a))
         (svar (caddr a))
         (kw (car-safe var)))
    (scope-1 init)
    (and kw (or (symbolp kw) (symbol-with-pos-p kw))
         (cadr var)
         (not (cddr var))
         ;; VAR is (KEYWORD VAR)
         (setq var (cadr var)))
    (when-let* ((bare (scope-sym-bare kw))
                ((keywordp bare)))
      (when-let* ((beg (scope-sym-pos kw)))
        (scope-report 'constant beg (length (symbol-name bare))))
      (setq l (scope-local-new bare (scope-sym-pos svar) l)))
    (if (consp var)
        (let ((scope--local l))
          (scope-cl-lambda-1 var (cons (append (when svar (list svar))
                                               (cons '&key arglist))
                                       more)
                             body))
      (when-let* ((bare (scope-sym-bare svar)))
        (when-let* ((beg (scope-sym-pos svar)))
          (scope-binding bare beg (length (symbol-name bare))))
        (setq l (scope-local-new bare (scope-sym-pos svar) l)))
      (when-let* ((bare (scope-sym-bare var)))
        (when-let* ((beg (scope-sym-pos var)))
          (scope-binding bare beg (length (symbol-name bare))))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let* ((bare (scope-sym-bare head))
                    ((memq bare '(&aux &allow-other-keys))))
              (progn
                (when-let* ((beg (scope-sym-pos head)))
                  (scope-report 'ampersand beg (length (symbol-name bare))))
                (cl-case bare
                  (&aux
                   (let ((scope--local l))
                     (scope-cl-lambda-aux (cadr arglist) (cddr arglist) more body)))
                  (&allow-other-keys
                   (let ((scope--local l))
                     (scope-cl-lambda-1 (car more) (cdr more) body)))))
            (let ((scope--local l))
              (scope-cl-lambda-key head (cdr arglist) more body)))))
       (more (let ((scope--local l))
               (scope-cl-lambda-1 (car more) (cdr more) body)))
       (t (let ((scope--local l))
            (scope-lambda nil body)))))))

(defun scope-cl-lambda-aux (arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l scope--local)
         (init (cadr a)))
    (scope-1 init)
    (if (consp var)
        (let ((scope--local l))
          (scope-cl-lambda-1 var (cons arglist more) body))
      (when-let* ((bare (scope-sym-bare var)))
        (when-let* ((beg (scope-sym-pos var)))
          (scope-binding bare beg (length (symbol-name bare))))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (let ((scope--local l))
        (cond
         (arglist (scope-cl-lambda-aux (car arglist) (cdr arglist) more body))
         (more (scope-cl-lambda-1 (car more) (cdr more) body))
         (t (scope-lambda nil body)))))))

(defvar scope-macrolet-alist nil)

(defun scope-cl-macrolet (bindings body)
  (if-let* ((b (car bindings)))
      (let ((name (car b))
            (arglist (cadr b))
            (mbody (cddr b)))
        (scope-cl-lambda arglist mbody)
        (when-let* ((bare (scope-sym-bare name)))
          (when-let* ((beg (scope-sym-pos name)))
            (scope-report 'macro beg (length (symbol-name bare)) beg))
          (let ((scope-macrolet-alist (scope-local-new bare (scope-sym-pos name) scope-macrolet-alist)))
            (scope-cl-macrolet (cdr bindings) body))))
    (scope-n body)))

(defun scope-define-minor-mode (mode _doc body)
  (let ((explicit-var nil) (command t))
    (while-let ((kw (car-safe body))
                (bkw (scope-sym-bare kw))
                ((keywordp bkw)))
      (when-let* ((beg (scope-sym-pos kw)))
        (scope-report 'constant beg (length (symbol-name bkw))))
      (cl-case bkw
        ((:init-value :keymap :after-hook :initialize)
         (scope-1 (cadr body)))
        (:lighter (scope-mode-line-construct (cadr body)))
        ((:interactive)
         (let ((val (cadr body)))
           (when (consp val) (mapc #'scope-major-mode-name val))
           (setq command val)))
        ((:variable)
         (let* ((place (cadr body))
                (tail (cdr-safe place)))
           (if (and tail (let ((symbols-with-pos-enabled t))
                           (or (symbolp tail) (functionp tail))))
               (progn
                 (scope-1 (car place))
                 (scope-sharpquote tail))
             (scope-1 place)))
         (setq explicit-var t))
        ((:group)
         (scope-quoted-group (cadr body)))
        ((:predicate)                   ;For globalized minor modes.
         (scope-global-minor-mode-predicate (cadr body)))
        ((:on :off)
         (let ((obod (cdr body)))
           (while (and obod (not (keywordp (scope-sym-bare (car obod)))))
             (scope-1 (pop obod)))
           (setq body (cons bkw (cons nil obod))))))
      (setq body (cddr body)))
    (when-let* ((bare (scope-sym-bare mode)) (beg (scope-sym-pos mode))
                (typ (if command 'defcmd 'defun)))
      (scope-report typ beg (length (symbol-name bare)))
      (unless explicit-var
        (scope-report 'defvar beg (length (symbol-name bare)))))
    (scope-n body)))

(defun scope-global-minor-mode-predicate (pred)
  (if (consp pred)
      (if (eq 'not (scope-sym-bare (car pred)))
          (mapc #'scope-global-minor-mode-predicate (cdr pred))
        (mapc #'scope-global-minor-mode-predicate pred))
    (scope-major-mode-name pred)))

(defun scope-major-mode-name (mode)
  (when-let* ((beg (scope-sym-pos mode))
              (bare (bare-symbol mode))
              ((not (booleanp bare)))
              (len (length (symbol-name bare))))
    (scope-report 'major-mode beg len)))

(defun scope-mode-line-construct (format)
  (scope-mode-line-construct-1 format))

(defun scope-mode-line-construct-1 (format)
  (cond
   ((symbol-with-pos-p format)
    (scope-report 'variable
                  (symbol-with-pos-pos format)
                  (length (symbol-name (bare-symbol format)))))
   ((consp format)
    (let ((head (car format)))
      (cond
       ((or (stringp head) (consp head) (integerp head))
        (mapc #'scope-mode-line-construct-1 format))
       ((or (symbolp head) (symbol-with-pos-p head))
        (scope-s head)
        (cl-case (bare-symbol head)
          (:eval
           (scope-1 (cadr format)))
          (:propertize
           (scope-mode-line-construct-1 (cadr format))
           (when-let* ((props (cdr format))
                       (symbols-with-pos-enabled t)
                       (val-form (plist-get props 'face)))
             (scope-face-1 val-form)))
          (otherwise
           (scope-mode-line-construct-1 (cadr format))
           (scope-mode-line-construct-1 (caddr format))))))))))

(defcustom scope-safe-macros nil
  "Specify which macros are safe to expand during code analysis.

If this is t, macros are considered safe by default.  Otherwise, this is
a (possibly empty) list of safe macros.

Note that this option only affects analysis of untrusted code, for
trusted code macro expansion is always safe."
  :type '(choice (const :tag "Trust all macros" t)
                 (repeat :tag "Trust these macros" symbol))
  :group 'lisp)

(defvar scope-unsafe-macros
  '( static-if static-when static-unless
     cl-eval-when eval-when-compile eval-and-compile let-when-compile
     rx cl-macrolet nnoo-define-basics))

(defun scope-safe-macro-p (macro)
  (and (not (memq macro scope-unsafe-macros))
       (or (eq scope-safe-macros t)
           (memq macro scope-safe-macros)
           (get macro 'safe-macro)
           (trusted-content-p))))

(defvar warning-minimum-log-level)

(defmacro scope-define-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let ((analyzer (intern (concat "scope--analyze-" (symbol-name fsym)))))
    `(progn
       (defun ,analyzer ,args ,@body)
       (put ',fsym 'scope-analyzer #',analyzer))))

(defmacro scope--define-function-analyzer (fsym args type &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (f &rest args)
         (scope-report-s f ',type)
         (apply #',helper args)
         (scope-n args)))))

(defmacro scope-define-function-analyzer (fsym args &rest body)
  (declare (indent defun))
  `(scope--define-function-analyzer ,fsym ,args function ,@body)
  ;; (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
  ;;   `(progn
  ;;      (defun ,helper ,args ,@body)
  ;;      (scope-define-analyzer ,fsym (l f &rest args)
  ;;        (scope-report-s f 'function)
  ;;        (apply #',helper args)
  ;;        (scope-n l args))))
  )

(defmacro scope-define-func-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (f &rest args)
         (scope-report-s f 'function)
         (apply #',helper args)))))

(defmacro scope-define-macro-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (f &rest args)
         (scope-report-s f 'macro)
         (apply #',helper args)))))

(defmacro scope-define-special-form-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (f &rest args)
         (scope-report-s f 'macro)
         (apply #',helper args)))))

(defun scope--unquote (form)
  (when (memq (scope-sym-bare (car-safe form)) '(quote function \`))
    (cadr form)))

(scope-define-analyzer with-suppressed-warnings (f warnings &rest body)
  (scope-report-s f 'macro)
  (dolist (warning warnings)
    (when-let* ((wsym (car-safe warning)))
      (scope-report-s wsym 'warning-type)))
  (scope-n body))

(scope-define-analyzer eval (f form &optional lexical)
  (scope-report-s f 'function)
  (if-let* ((quoted (scope--unquote form)))
      (scope-1 quoted)
    (scope-1 form))
  (scope-1 lexical))

(scope-define-func-analyzer funcall (&optional f &rest args)
  (scope-1 f '(symbol . function))
  (dolist (arg args) (scope-1 arg)))

(put 'apply 'scope-analyzer #'scope--analyze-funcall)

(scope-define-func-analyzer defalias (&optional sym def docstring)
  (scope-1 sym '(symbol . defun))
  (scope-1 def '(symbol . defun))
  (scope-1 docstring))

(scope-define-function-analyzer oclosure--define
  (&optional name _docstring parent-names _slots &rest props)
  (when-let* ((quoted (scope--unquote name))) (scope-report-s quoted 'defoclosure))
  (when-let* ((qs (scope--unquote parent-names)))
    (dolist (q qs)
      (scope-report-s q 'oclosure)))
  (while-let ((kw (car-safe props))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (scope-report-s kw 'constant)
    (cl-case bkw
      (:predicate
       (when-let* ((q (scope--unquote (cadr props)))) (scope-report-s q 'defun))))
    (setq props (cddr props))))

(scope-define-function-analyzer define-charset
  (&optional name _docstring &rest _props)
  (when-let* ((quoted (scope--unquote name))) (scope-report-s quoted 'defcharset)))

(scope-define-function-analyzer define-charset-alias
  (&optional alias charset)
  (when-let* ((quoted (scope--unquote alias))) (scope-report-s quoted 'defcharset))
  (when-let* ((quoted (scope--unquote charset))) (scope-report-s quoted 'charset)))

(scope-define-func-analyzer charset-chars
  (&optional charset &rest rest)
  (scope-1 charset '(symbol . charset))
  (mapc #'scope-1 rest))

(dolist (sym '(charset-description charset-info charset-iso-final-char
                                   charset-long-name charset-plist
                                   charset-short-name
                                   get-charset-property put-charset-property
                                   list-charset-chars
                                   set-charset-plist
                                   set-charset-priority
                                   unify-charset
                                   locale-charset-to-coding-system))
  (put sym 'scope-analyzer #'scope--analyze-charset-chars))

(scope-define-func-analyzer define-coding-system
  (&optional name &rest rest)
  (scope-1 name '(symbol . defcoding))
  (mapc #'scope-1 rest))

(scope-define-func-analyzer define-coding-system-alias
  (&optional alias coding-system)
  (scope-1 alias '(symbol . defcoding))
  (scope-1 coding-system '(symbol . coding)))

(scope-define-function-analyzer decode-coding-region
  (&optional _start _end coding-system &rest _)
  (when-let* ((quoted (scope--unquote coding-system))) (scope-report-s quoted 'coding)))

(put 'encode-coding-region 'scope-analyzer #'scope--analyze-decode-coding-region)

(scope-define-function-analyzer decode-coding-string
  (&optional _string coding-system &rest _)
  (when-let* ((quoted (scope--unquote coding-system))) (scope-report-s quoted 'coding)))

(dolist (sym '(encode-coding-char encode-coding-string))
  (put sym 'scope-analyzer #'scope--analyze-decode-coding-string))

(scope-define-function-analyzer coding-system-mnemonic
  (&optional coding-system &rest _)
  (when-let* ((quoted (scope--unquote coding-system))) (scope-report-s quoted 'coding)))

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
  (put sym 'scope-analyzer #'scope--analyze-coding-system-mnemonic))

(scope-define-func-analyzer thing-at-point (&optional thing no-props)
  (scope-1 thing '(symbol . thing))
  (scope-1 no-props))

(dolist (sym '( forward-thing
                beginning-of-thing
                end-of-thing
                bounds-of-thing-at-point))
  (put sym 'scope-analyzer #'scope--analyze-thing-at-point))

(scope-define-func-analyzer bounds-of-thing-at-mouse (&optional event thing)
  (scope-1 event)
  (scope-1 thing '(symbol . thing)))

(scope-define-func-analyzer thing-at-mouse (&optional event thing no-props)
  (scope-1 event)
  (scope-1 thing '(symbol . thing))
  (scope-1 no-props))

(scope-define-function-analyzer custom-declare-variable (sym _default _doc &rest args)
  (when-let* ((quoted (scope--unquote sym))) (scope-report-s quoted 'defvar))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:type
       (when-let* ((quoted (scope--unquote (cadr args)))) (scope-widget-type-1 quoted)))
      (:group
       (when-let* ((quoted (scope--unquote (cadr args)))) (scope-report-s quoted 'group))))
    (setq args (cddr args))))

(scope-define-function-analyzer custom-declare-group (sym _members _doc &rest args)
  (when-let* ((quoted (scope--unquote sym))) (scope-report-s quoted 'defgroup))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:group
       (when-let* ((quoted (scope--unquote (cadr args)))) (scope-report-s quoted 'group))))
    (setq args (cddr args))))

(scope-define-function-analyzer custom-declare-face (face spec _doc &rest args)
  (when-let* ((q (scope--unquote face))) (scope-report-s q 'defface))
  (when-let* ((q (scope--unquote spec)))
    (when (consp q) (dolist (s q) (scope-face (cdr s)))))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:group
       (when-let* ((q (scope--unquote (cadr args)))) (scope-report-s q 'group))))
    (setq args (cddr args))))

(defun scope-typep (type)
  (cond
   ((or (symbolp type) (symbol-with-pos-p type))
    (unless (booleanp (scope-sym-bare type))
      (scope-report-s type 'type)))
   ((consp   type)
    (cond
     ((memq (scope-sym-bare (car type)) '(and or not))
      (mapc #'scope-typep (cdr type)))
     ((eq (scope-sym-bare (car type)) 'satisfies)
      (scope-report-s (cadr type) 'function))))))

(scope-define-function-analyzer cl-typep (_val type)
  (when-let* ((q (scope--unquote type)))
    (scope-typep q)))

(scope-define-function-analyzer pulse-momentary-highlight-region (_start _end &optional face)
  (when-let* ((q (scope--unquote face))) (scope-face q)))

(scope--define-function-analyzer throw (tag _value) non-local-exit
  (when-let* ((q (scope--unquote tag))) (scope-report-s q 'throw-tag)))

(scope--define-function-analyzer signal (error-symbol &optional _data) non-local-exit
  (when-let* ((q (scope--unquote error-symbol))) (scope-report-s q 'condition)))

(scope--define-function-analyzer kill-emacs                     (&rest _) non-local-exit)
(scope--define-function-analyzer abort-recursive-edit           (&rest _) non-local-exit)
(scope--define-function-analyzer top-level                      (&rest _) non-local-exit)
(scope--define-function-analyzer exit-recursive-edit            (&rest _) non-local-exit)
(scope--define-function-analyzer tty-frame-restack              (&rest _) non-local-exit)
(scope--define-function-analyzer error                          (&rest _) non-local-exit)
(scope--define-function-analyzer user-error                     (&rest _) non-local-exit)
(scope--define-function-analyzer minibuffer-quit-recursive-edit (&rest _) non-local-exit)
(scope--define-function-analyzer exit-minibuffer                (&rest _) non-local-exit)

(scope-define-func-analyzer run-hooks (&rest hooks)
  (dolist (hook hooks) (scope-1 hook '(symbol . variable))))

(scope-define-func-analyzer fboundp (&optional symbol)
  (scope-1 symbol '(symbol . function)))

(scope-define-function-analyzer overlay-put (&optional _ov prop val)
  (when-let* ((q (scope--unquote prop))
              ((eq (scope-sym-bare q) 'face))
              (face (scope--unquote val)))
    (scope-face face)))

(scope-define-function-analyzer add-face-text-property (&optional _start _end face &rest _)
  (when-let* ((q (scope--unquote face))) (scope-face q)))

(scope-define-function-analyzer facep (&optional face &rest _)
  (when-let* ((q (scope--unquote face))) (scope-report-s q 'face)))

(dolist (sym '( check-face face-id face-differs-from-default-p
                face-name face-all-attributes face-attribute
                face-foreground face-background face-stipple
                face-underline-p face-inverse-video-p face-bold-p
                face-italic-p face-extend-p face-documentation
                set-face-documentation set-face-attribute
                set-face-font set-face-background set-face-foreground
                set-face-stipple set-face-underline set-face-inverse-video
                set-face-bold set-face-italic set-face-extend))
  (put sym 'scope-analyzer #'scope--analyze-facep))

(scope-define-func-analyzer boundp (&optional var &rest rest)
  (scope-1 var '(symbol . variable))
  (mapc #'scope-1 rest))

(dolist (sym '( set symbol-value define-abbrev-table
                special-variable-p local-variable-p
                local-variable-if-set-p add-variable-watcher
                get-variable-watchers remove-variable-watcher
                default-value set-default make-local-variable
                buffer-local-value add-to-list add-to-history find-buffer
                customize-set-variable set-variable
                add-hook remove-hook run-hook-with-args run-hook-wrapped))
  (put sym 'scope-analyzer #'scope--analyze-boundp))

(scope-define-function-analyzer defvaralias (new base &optional _docstring)
  (when-let* ((q (scope--unquote new))) (scope-report-s q 'defvar))
  (when-let* ((q (scope--unquote base))) (scope-report-s q 'variable)))

(scope-define-func-analyzer define-error (&optional name message parent)
  (scope-1 name '(symbol . defcondition))
  (scope-1 message)
  (scope-1 parent '(or (symbol . condition)
                       (repeat . (symbol . condition)))))

(scope-define-function-analyzer featurep (feature &rest _)
  (when-let* ((q (scope--unquote feature))) (scope-report-s q 'feature)))

(put 'require 'scope-analyzer #'scope--analyze-featurep)

(scope-define-function-analyzer provide (feature &rest _)
  (when-let* ((q (scope--unquote feature))) (scope-report-s q 'deffeature)))

(scope-define-function-analyzer put-text-property (&optional _ _ prop val _)
  (when (memq (scope-sym-bare (scope--unquote prop)) '(mouse-face face))
    (when-let* ((q (scope--unquote val))) (scope-face q))))

(put 'remove-overlays 'scope-analyzer #'scope--analyze-put-text-property)

(scope-define-function-analyzer propertize (_string &rest props)
  (while props
    (cl-case (scope-sym-bare (scope--unquote (car props)))
      ((face mouse-face)
       (when-let* ((q (scope--unquote (cadr props)))) (scope-face q))))
    (setq props (cddr props))))

(scope-define-function-analyzer eieio-defclass-internal (name superclasses _ _)
  (when-let* ((q (scope--unquote name))) (scope-report-s q 'deftype))
  (when-let* ((q (scope--unquote superclasses)))
    (dolist (sup q) (scope-report-s sup 'type))))

(scope-define-function-analyzer cl-struct-define
  (name _doc parent _type _named _slots _children _tab _print)
  (when-let* ((q (scope--unquote name)))   (scope-report-s q 'deftype))
  (when-let* ((q (scope--unquote parent))) (scope-report-s q 'type)))

(scope-define-function-analyzer define-widget (name class _doc &rest args)
  (when-let* ((q (scope--unquote name)))  (scope-report-s q 'widget-type))
  (when-let* ((q (scope--unquote class))) (scope-report-s q 'widget-type))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:type
       (when-let* ((q (scope--unquote (cadr args)))) (scope-widget-type-1 q)))
      (:args
       (when-let* ((q (scope--unquote (cadr args)))) (mapc #'scope-widget-type-1 q))))
    (setq args (cddr args))))

(scope-define-function-analyzer provide-theme (name &rest _)
  (when-let* ((q (scope--unquote name))) (scope-report-s q 'theme)))

(dolist (sym '(enable-theme disable-theme load-theme custom-theme-p))
  (put sym 'scope-analyzer #'scope--analyze-provide-theme))

(scope-define-function-analyzer custom-theme-set-variables (theme &rest args)
  (when-let* ((q (scope--unquote theme))) (scope-report-s q 'theme))
  (dolist (arg args)
    (when-let* ((q (scope--unquote arg)))
      (when (consp q)
        (scope-report-s (pop q) 'variable)
        (when (consp q)
          (scope-1 (pop q))
          (dolist (request (car (cdr-safe q)))
            (scope-report-s request 'feature)))))))

(scope-define-function-analyzer custom-declare-theme (name &rest _)
  (when-let* ((q (scope--unquote name))) (scope-report-s q 'deftheme)))

(scope-define-function-analyzer eieio-oref (_obj slot)
  (when-let* ((q (scope--unquote slot))) (scope-report-s q 'slot)))

(dolist (fun '(slot-boundp slot-makeunbound slot-exists-p eieio-oref-default))
  (put fun 'scope-analyzer #'scope--analyze-eieio-oref))

(scope-define-function-analyzer eieio-oset (_obj slot _value)
  (when-let* ((q (scope--unquote slot))) (scope-report-s q 'slot)))

(put 'eieio-oset-default 'scope-analyzer #'scope--analyze-eieio-oset)

(scope-define-function-analyzer derived-mode-p (modes &rest _obsolete)
  (when-let* ((q (scope--unquote modes))) (scope-report-s q 'major-mode)))

(scope-define-func-analyzer derived-mode-set-parent (&optional mode parent)
  (scope-1 mode '(symbol . major-mode))
  (scope-1 parent '(symbol . major-mode)))

(scope-define-func-analyzer scope-report (type &rest args)
  (scope-1 type '(symbol . symbol-type))
  (mapc #'scope-1 args))

(scope-define-func-analyzer scope-report-s (&optional sym type)
  (scope-1 sym)
  (scope-1 type '(symbol . symbol-type)))

(scope-define-func-analyzer scope-1 (&optional form outtype)
  (scope-1 form)
  (scope-1 outtype 'type))

(scope-define-function-analyzer icons--register (&optional name parent _spec _doc kws)
  (when-let* ((q (scope--unquote name))) (scope-report-s q 'deficon))
  (when-let* ((q (scope--unquote parent))) (scope-report-s q 'icon))
  (when-let* ((q (scope--unquote kws)))
    (while-let ((kw (car-safe q))
                (bkw (scope-sym-bare kw))
                ((keywordp bkw)))
      (scope-report-s kw 'constant)
      (cl-case bkw
        (:group (scope-report-s (cadr q) 'group)))
      (setq q (cddr q)))))

(scope-define-function-analyzer setopt--set (&optional var _val)
  (when-let* ((q (scope--unquote var))) (scope-report-s q 'variable)))

(scope-define-function-analyzer autoload (&optional func _file _doc int &rest _)
  (when-let* ((q (scope--unquote func))) (scope-report-s q 'function))
  (when-let* ((q (scope--unquote int)) ((listp q)))
    (dolist (mode q) (scope-report-s mode 'major-mode))))

(scope-define-function-analyzer minibuffer--define-completion-category (&optional name parents &rest _)
  (when-let* ((q (scope--unquote name))) (scope-report-s q 'completion-category-definition))
  (when-let* ((q (scope--unquote parents)))
    (dolist (p (ensure-list q)) (scope-report-s p 'completion-category))))

;; (scope-define-macro-analyzer define-completion-category (l &optional name parent &rest rest)
;;   (scope-report-s name 'completion-category-definition)
;;   (scope-report-s parent 'completion-category)
;;   (scope-n l rest))

(scope-define-func-analyzer completion-table-with-category (&optional category table)
  (scope-1 category '(symbol . completion-category))
  (scope-1 table))

(defun scope--easy-menu-do-define-menu (menu)
  (let ((items (cdr menu)))
    (while-let ((kw (car-safe items))
                (bkw (scope-sym-bare kw))
                ((keywordp bkw)))
      (scope-report-s kw 'constant)
      (cl-case bkw
        ((:active :label :visible) (scope-1 (cadr items)))
        ((:filter) (scope-sharpquote (cadr items))))
      (setq items (cddr items)))
    (dolist (item items)
      (cond
       ((vectorp item)
        (when (length> item 2)
          (scope-sharpquote (aref item 1))
          (let ((it (cddr (append item nil))))
            (scope-1 (car it))
            (while-let ((kw (car-safe it))
                        (bkw (scope-sym-bare kw))
                        ((keywordp bkw)))
              (scope-report-s kw 'constant)
              (cl-case bkw
                ((:active :enable :label :visible :suffix :selected) (scope-1 (cadr it))))
              (setq it (cddr it))))))
       ((consp item) (scope--easy-menu-do-define-menu item))))))

(scope-define-function-analyzer easy-menu-do-define (&optional _symbol _maps _doc menu)
  (when-let* ((q (scope--unquote menu)))
    (scope--easy-menu-do-define-menu q)))

(scope-define-function-analyzer define-key (&optional _keymaps _key def _remove)
  (when-let* ((q (scope--unquote def)))
    (cond
     ((eq (scope-sym-bare (car-safe q)) 'menu-item)
      (let ((fn (caddr q)) (it (cdddr q)))
        (scope-sharpquote fn)
        (while-let ((kw (car-safe it))
                    (bkw (scope-sym-bare kw))
                    ((keywordp bkw)))
          (scope-report-s kw 'constant)
          (cl-case bkw
            ((:active :enable :label :visible :suffix :selected) (scope-1 (cadr it)))
            ((:filter) (scope-sharpquote (cadr it))))
          (setq it (cddr it)))))
     ((or (symbolp q) (symbol-with-pos-p q))
      (scope-report-s q 'function)))))

(scope-define-function-analyzer eval-after-load (&optional file form)
  (when-let* ((q (scope--unquote file))) (scope-report-s q 'feature))
  (when-let* ((q (scope--unquote form))) (scope-1 q)))

(scope-define-macro-analyzer define-globalized-minor-mode (global mode turn-on &rest body)
  (scope-report-s mode 'function)
  (scope-report-s turn-on 'function)
  (scope-define-minor-mode global nil body))

(scope-define-macro-analyzer define-derived-mode (&optional child parent name &rest body)
  (scope-report-s child 'major-mode-definition)
  (scope-report-s parent 'major-mode)
  (scope-mode-line-construct name)
  (when (stringp (car body)) (pop body))
  (while-let ((kw (car-safe body))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (scope-report-s kw 'constant)
    (cl-case bkw
      (:group (scope-quoted-group (cadr body)))
      ((:syntax-table :abbrev-table :after-hook) (scope-1 (cadr body))))
    (setq body (cddr body)))
  (scope-n body))

(scope-define-macro-analyzer lambda (args &rest body)
  (scope-lambda args body))

(defun scope-oclosure-lambda-1 (local bindings args body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (scope-sym-bare sym))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos sym)))
        (when beg (scope-binding bare beg len))
        (scope-1 (cadr binding))
        (scope-oclosure-lambda-1
         (if bare (scope-local-new bare beg local) local)
         (cdr bindings) args body))
    (let ((scope--local local))
      (scope-lambda args body))))

(defun scope-oclosure-lambda (spec args body)
  (let ((type (car-safe spec)))
    (scope-report-s type 'oclosure))
  (scope-oclosure-lambda-1 scope--local (cdr-safe spec) args body))

(scope-define-macro-analyzer oclosure-lambda (&optional spec args &rest body)
  (scope-oclosure-lambda spec args body))

(scope-define-macro-analyzer cl-loop (&rest clauses)
  (scope-loop clauses))

(scope-define-macro-analyzer named-let (name bindings &rest body)
  (scope-named-let name bindings body scope--output-type))

(scope-define-macro-analyzer cl-flet (bindings &rest body)
  (scope-flet bindings body))

(scope-define-macro-analyzer cl-labels (bindings &rest body)
  (scope-labels bindings body))

(scope-define-macro-analyzer with-slots (spec-list object &rest body)
  (scope-with-slots spec-list object body))

(scope-define-macro-analyzer cl-defmethod (name &rest rest)
  (scope-defmethod name rest))

(scope-define-macro-analyzer cl-destructuring-bind (args expr &rest body)
  (scope-1 expr)
  (scope-cl-lambda args body))

(scope-define-macro-analyzer declare-function (&optional fn _file arglist _fileonly)
  (scope-report-s fn 'function)
  (scope-lambda (and (listp arglist) arglist) nil))

(scope-define-macro-analyzer cl-block (name &rest body)
  (scope-block name body))

(scope-define-macro-analyzer cl-return-from (name &optional result)
  (scope-return-from name result))

(scope-define-macro-analyzer rx (&rest regexps)
  ;; Unsafe macro!
  (scope-rx regexps))

(scope-define-macro-analyzer cl-tagbody (&rest body)
  (let (labels statements)
    (while body
      (let ((head (pop body)))
        (if (consp head)
            (push head statements)
          (push head labels))))
    (scope-cl-tagbody (nreverse labels) (nreverse statements))))

(defvar scope-label-alist nil)

(defun scope-cl-tagbody (labels statements)
  (if labels
      (let* ((label (car labels))
             (bare (scope-sym-bare label)))
        (when-let* ((beg (scope-sym-pos label)))
          (scope-report 'label beg (length (symbol-name bare)) beg))
        (let ((scope-label-alist
               (if bare
                   (scope-local-new bare (scope-sym-pos label) scope-label-alist)
                 scope-label-alist)))
          (scope-cl-tagbody (cdr labels) statements)))
    (scope-n statements)))

(scope-define-macro-analyzer go (label)
  ;; TODO: Change to a local macro defintion induced by `cl-tagbody'.
  (when-let* ((bare (scope-sym-bare label))
              (pos (alist-get bare scope-label-alist))
              (beg (scope-sym-pos label)))
    (scope-report 'label beg (length (symbol-name bare)) pos)))

(scope-define-macro-analyzer rx-define (name &rest rest)
  (scope-rx-define name rest))

(scope-define-macro-analyzer rx-let (bindings &rest body)
  (scope-rx-let bindings body))

(scope-define-macro-analyzer let-when-compile (bindings &rest body)
  ;; Unsafe macro!
  (scope-let* bindings body))

(scope-define-macro-analyzer cl-eval-when (_when &rest body)
  ;; Unsafe macro!
  (scope-n body))

(scope-define-macro-analyzer cl-macrolet (bindings &rest body)
  ;; Unsafe macro!
  (scope-cl-macrolet bindings body))

(scope-define-macro-analyzer cl-symbol-macrolet (bindings &rest body)
  ;; Unsafe macro!
  (scope-let* bindings body))

(scope-define-macro-analyzer nnoo-define-basics (&optional backend)
  ;; Unsafe macro!
  (let* ((bare (bare-symbol backend))
         (len (length (symbol-name bare)))
         (beg (scope-sym-pos backend)))
    (when beg (scope-report 'nnoo-backend beg len))))

(scope-define-macro-analyzer gv-define-expander (name handler)
  (scope-gv-define-expander name handler))

(scope-define-macro-analyzer gv-define-simple-setter (name setter &rest rest)
  (scope-gv-define-simple-setter name setter rest))

(scope-define-macro-analyzer cl-deftype (name arglist &rest body)
  (scope-deftype name arglist body))

(scope-define-macro-analyzer define-minor-mode (&optional mode doc &rest body)
  (when mode (scope-define-minor-mode mode doc body)))

(scope-define-macro-analyzer setq-local (&rest args)
  (scope-setq args))

(put 'setq-default 'scope-analyzer #'scope--analyze-setq-local)

(scope-define-macro-analyzer cl-defun (name arglist &rest body)
  (scope-cl-defun name arglist body))

(put 'cl-defmacro 'scope-analyzer #'scope--analyze-cl-defun)

(scope-define-macro-analyzer defun (&optional name arglist &rest body)
  (when name (scope-defun name arglist body)))

(scope-define-macro-analyzer defmacro (&optional name arglist &rest body)
  (scope-report-s name 'defmacro)
  (scope-lambda arglist body))

(put 'ert-deftest 'scope-analyzer #'scope--analyze-defun)

(scope-define-macro-analyzer scope-define-symbol-type (&optional name parents &rest props)
  (scope-report-s name 'symbol-type-definition)
  (dolist (parent parents) (scope-report-s parent 'symbol-type))
  (while-let ((kw (car-safe props))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (scope-report-s kw 'constant)
    (cl-case bkw
      (:face
       (if-let* ((q (scope--unquote (cadr props)))) (scope-face-1 q)
         (scope-1 (cadr props))))
      (:definition
       (if-let* ((q (scope--unquote (cadr props))))
           (dolist (st (ensure-list q)) (scope-report-s st 'symbol-type))
         (scope-1 (cadr props))))
      (otherwise (scope-1 (cadr props))))
    (setq props (cddr props))))

(scope-define-macro-analyzer cl-letf (bindings &rest body)
  (let ((l scope--local))
    (dolist (binding bindings)
      (let ((place (car binding)))
        (if (or (symbol-with-pos-p place) (symbolp place))
            (let* ((bare (bare-symbol place))
                   (len (length (symbol-name bare)))
                   (beg (scope-sym-pos place)))
              (when beg (scope-binding bare beg len))
              (setq l (scope-local-new bare beg l)))
          (scope-1 place))
        (scope-1 (cadr binding))))
    (let ((scope--local l)) (scope-n body scope--output-type))))

(scope-define-macro-analyzer setf (&rest args) (scope-setq args))

(scope-define-macro-analyzer pop (&optional place) (scope-1 place))

(scope-define-macro-analyzer push (&optional newelt place)
  (scope-1 newelt)
  (scope-1 place))

(scope-define-macro-analyzer with-memoization (&optional place &rest body)
  (scope-1 place)
  (scope-n body scope--output-type))

(scope-define-macro-analyzer cl-pushnew (&rest args)
  (mapc #'scope-1 args))

(dolist (sym '(incf decf))
  (put sym 'scope-analyzer #'scope--analyze-cl-pushnew))

(scope-define-macro-analyzer static-if (&optional test then &rest else)
  (scope-1 test)
  (scope-1 then scope--output-type)
  (scope-n else scope--output-type))

(scope-define-macro-analyzer static-when (&optional test &rest body)
  (scope-1 test)
  (scope-n body scope--output-type))

(put 'static-unless 'scope-analyzer #'scope--analyze-static-when)

(scope-define-macro-analyzer eval-when-compile (&rest body)
  (scope-n body scope--output-type))

(put 'eval-and-compile 'scope-analyzer #'scope--analyze-eval-when-compile)

(scope-define-macro-analyzer cl-callf (&rest args)
  (scope-sharpquote (car args))
  (scope-n (cdr args)))

(put 'cl-callf2 'scope-analyzer #'scope--analyze-cl-callf)

(scope-define-macro-analyzer seq-let (args sequence &rest body)
  (scope-1 sequence)
  (let ((l scope--local))
    (dolist (arg args)
      (let* ((bare (scope-sym-bare arg))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos arg)))
        (if (eq bare '&rest)
            (scope-report 'ampersand beg len)
          (when beg (scope-binding bare beg len))
          (setq l (scope-local-new bare beg l)))))
    (let ((scope--local l)) (scope-n body))))

(scope-define-analyzer let-alist (f alist &rest body)
  (scope-report-s f 'macro)
  (scope-1 alist)
  (let ((scope-current-let-alist-form
         (cons (or (scope-sym-pos f) (cons 'gen (incf scope-counter)))
               (scope-sym-pos f))))
    (scope-n body)))

(scope-define-macro-analyzer define-obsolete-face-alias (&optional obs cur when)
  (when-let* ((q (scope--unquote obs))) (scope-report-s q 'defface))
  (when-let* ((q (scope--unquote cur))) (scope-report-s q 'face))
  (scope-1 when))

(scope-define-macro-analyzer backquote (&optional structure)
  (scope-backquote structure scope--output-type))

(defvar scope-backquote-depth 0)

(defun scope-backquote (structure &optional outtype)
  (let ((scope-backquote-depth (1+ scope-backquote-depth)))
    (scope-backquote-1 structure outtype)))

(defun scope-backquote-1 (structure &optional outtype)
  (cond
   ((vectorp structure)
    (dotimes (i (length structure))
      (scope-backquote-1 (aref structure i))))
   ((atom structure) (scope-quote structure outtype))
   ((or (eq (car structure) backquote-unquote-symbol)
        (eq (car structure) backquote-splice-symbol))
    (if (= scope-backquote-depth 1)
        (scope-1 (cadr structure) outtype)
      (let ((scope-backquote-depth (1- scope-backquote-depth)))
        (scope-backquote-1 (cadr structure)))))
   (t
    (while (consp structure) (scope-backquote-1 (pop structure)))
    (when structure (scope-backquote-1 structure)))))

(scope-define-special-form-analyzer let (bindings &rest body)
  (scope-let bindings body))

(scope-define-special-form-analyzer let* (bindings &rest body)
  (scope-let* bindings body))

(scope-define-special-form-analyzer cond (&rest clauses)
  (dolist (clause clauses) (scope-n clause scope--output-type)))

(scope-define-special-form-analyzer setq (&rest args)
  (scope-setq args))

(scope-define-special-form-analyzer defvar (&optional sym init _doc)
  (scope-report-s sym 'defvar)
  (scope-1 init))

(put 'defconst 'scope-analyzer #'scope--analyze-defvar)

(defun scope-condition-case (var bodyform handlers)
  (let* ((bare (bare-symbol var))
         (beg (when (symbol-with-pos-p var) (symbol-with-pos-pos var)))
         (l (scope-local-new bare beg scope--local)))
    (when beg (scope-binding bare beg (length (symbol-name bare))))
    (scope-1 bodyform scope--output-type)
    (dolist (handler handlers)
      (dolist (cond-name (ensure-list (car-safe handler)))
        (when-let* ((cbeg (scope-sym-pos cond-name))
                    (cbare (scope-sym-bare cond-name))
                    (clen (length (symbol-name cbare))))
          (cond
           ((booleanp cbare))
           ((keywordp cbare) (scope-report 'constant cbeg clen))
           (t                (scope-report 'condition cbeg clen)))))
      (let ((scope--local l))
        (scope-n (cdr handler) scope--output-type)))))

(scope-define-special-form-analyzer condition-case (var bodyform &rest handlers)
  (scope-condition-case var bodyform handlers))

(scope-define-macro-analyzer condition-case-unless-debug (var bodyform &rest handlers)
  (scope-condition-case var bodyform handlers))

(scope-define-special-form-analyzer function (&optional arg)
  (when arg (scope-sharpquote arg)))

(scope-define-special-form-analyzer quote (arg)
  (scope-quote arg scope--output-type))

(scope-define-special-form-analyzer if (&optional test then &rest else)
  (scope-1 test)
  (scope-1 then scope--output-type)
  (scope-n else scope--output-type))

(scope-define-special-form-analyzer and (&rest forms)
  (scope-n forms scope--output-type))

(scope-define-special-form-analyzer or (&rest forms)
  (dolist (form forms) (scope-1 form scope--output-type)))

(defun scope-quote (arg &optional outtype)
  (when outtype
    (when-let* ((type (scope--match-type-to-arg outtype arg)))
      (scope--handle-quoted type arg))))

(cl-defgeneric scope--handle-quoted (type arg))

(cl-defmethod scope--handle-quoted ((_type (eql t)) _arg)
  ;; Do nothing.
  )

(cl-defmethod scope--handle-quoted ((_type (eql 'code)) arg)
  (let ((scope--local nil)
        (scope-current-let-alist-form nil)
        (scope-flet-alist nil)
        (scope-block-alist nil)
        (scope-macrolet-alist nil)
        (scope-label-alist nil)
        (scope-rx-alist nil)
        (scope--quoted t))
    (scope-1 arg)))

(cl-defmethod scope--handle-quoted ((type (head symbol)) arg)
  (scope-report-s arg (cdr type)))

(cl-defmethod scope--handle-quoted ((type (head list)) arg)
  (let ((types (cdr type)))
    (while types (scope--handle-quoted (pop types) (pop arg)))))

(cl-defmethod scope--handle-quoted ((type (head cons)) arg)
  (scope--handle-quoted (cadr type) (car arg))
  (scope--handle-quoted (cddr type) (cdr arg)))

(cl-defgeneric scope--match-type-to-arg (type arg))

(cl-defmethod scope--match-type-to-arg ((type (eql 'code)) _arg) type)

(cl-defmethod scope--match-type-to-arg ((_type (eql 'type)) arg)
  (scope--match-type-to-arg
   ;; Unfold `type'.
   '(or (equal . code)
        (equal . type)
        (cons (equal . symbol) . (symbol . symbol-type))
        (cons (equal . repeat) . type)
        (cons (equal . or)     . (repeat . type))
        (cons (equal . cons)   . (cons type . type))
        (cons (equal . equal)  . t))
   arg))

(cl-defmethod scope--match-type-to-arg ((type (head symbol)) arg)
  (when (or (symbolp arg) (symbol-with-pos-p arg)) type))

(cl-defmethod scope--match-type-to-arg ((type (head repeat)) arg)
  (when (listp arg)
    (named-let loop ((args arg) (acc nil))
      (if args
          (when-let* ((res (scope--match-type-to-arg (cdr type) (car args))))
            (loop (cdr args) (cons res acc)))
        (cons 'list (nreverse acc))))))

(cl-defmethod scope--match-type-to-arg ((type (head or)) arg)
  (named-let loop ((types (cdr type)))
    (when types
      (if-let* ((res (scope--match-type-to-arg (car types) arg))) res
        (loop (cdr types))))))

(cl-defmethod scope--match-type-to-arg ((type (head cons)) arg)
  (when (consp arg)
    (let ((car-type (cadr type))
          (cdr-type (cddr type)))
      (when-let* ((car-res (scope--match-type-to-arg car-type (car arg)))
                  (cdr-res (scope--match-type-to-arg cdr-type (cdr arg))))
        (cons 'cons (cons car-res cdr-res))))))

(cl-defmethod scope--match-type-to-arg ((type (head equal)) arg)
  (equal (cdr type) arg))

(scope--match-type-to-arg '(repeat .
                                   (or (cons (equal . foo) . (symbol footype))
                                       (cons (equal . bar) . (symbol bartype))))
                          '((bar . spambar) (foo . spamfoo)))

(scope-define-special-form-analyzer catch (&optional tag &rest body)
  (scope-1 tag '(symbol . throw-tag))
  (scope-n body scope--output-type))

(scope-define-special-form-analyzer progn (&rest body)
  (scope-n body scope--output-type))

(put 'inline 'scope-analyzer #'scope--analyze-progn)
(put 'save-current-buffer 'scope-analyzer #'scope--analyze-progn)
(put 'save-excursion 'scope-analyzer #'scope--analyze-progn)
(put 'save-restriction 'scope-analyzer #'scope--analyze-progn)

(scope-define-special-form-analyzer while (&rest rest)
  (mapc #'scope-1 rest))

(scope-define-special-form-analyzer prog1 (&rest body)
  (when (consp body) (scope-1 (pop body) scope--output-type))
  (scope-n body))

(put 'unwind-protect 'scope-analyzer #'scope--analyze-prog1)

(defun scope-report-s (sym type)
  (when-let* ((beg (scope-sym-pos sym)) (bare (bare-symbol sym)))
    (scope-report type beg (length (symbol-name bare)))))

(defvar-local scope-buffer-file-name nil)

(defun scope-1 (form &optional outtype)
  (cond
   ((consp form)
    (let* ((f (car form)) (bare (scope-sym-bare f))
           (forms (cdr form)) (this nil))
      (when bare
        (cond
         ((setq this (assq bare scope-flet-alist))
          (scope-report
           'function (symbol-with-pos-pos f) (length (symbol-name bare)) (cdr this))
          (scope-n forms))
         ((setq this (assq bare scope-macrolet-alist))
          (when (symbol-with-pos-p f)
            (scope-report
             'macro (symbol-with-pos-pos f) (length (symbol-name bare)) (cdr this)))
          ;; Local macros can be unsafe, so we do not expand them.
          ;; Hence we cannot interpret their arguments.
          )
         ((setq this (function-get bare 'scope-analyzer))
          (let ((scope--output-type outtype)) (apply this form)))
         ((special-form-p bare) (scope-report-s f 'special-form) (scope-n forms))
         ((macrop bare) (scope-report-s f 'macro)
          (cond
           ((eq (get bare 'edebug-form-spec) t) (scope-n forms))
           ((scope-safe-macro-p bare)
            (let* ((warning-minimum-log-level :emergency)
                   (macroexp-inhibit-compiler-macros t)
                   (symbols-with-pos-enabled t)
                   (message-log-max nil)
                   (inhibit-message t)
                   (macroexpand-all-environment
                    (append (mapcar #'list scope-unsafe-macros) macroexpand-all-environment))
                   (expanded (ignore-errors (macroexpand-1 form macroexpand-all-environment))))
              (scope-1 expanded)))))
         ((or (functionp bare) (memq bare scope-local-functions))
          (scope-report-s f 'function) (scope-n forms))
         (t
          (scope-report-s f 'unknown)
          (when scope-assume-func (scope-n forms)))))))
   ((symbol-with-pos-p form) (scope-s form))))

(defun scope-n (body &optional outtype)
  (while (cdr-safe body) (scope-1 (pop body)))
  (when-let* ((form (car-safe body))) (scope-1 form outtype)))

;;;###autoload
(defun scope (callback &optional stream)
  "Read and analyze code from STREAM, reporting findings via CALLBACK.

Call CALLBACK for each analyzed symbol SYM with arguments TYPE, POS,
LEN, ID and DEF, where TYPE is a symbol that specifies the semantics of
SYM; POS is the position of SYM in STREAM; LEN is SYM's length; ID is an
object that uniquely identifies (co-)occurrences of SYM in the current
defun; and DEF is the position in which SYM is locally defined, or nil.
If SYM is itself a binding occurrence, then POS and BINDER are equal.
If SYM is not lexically bound, then BINDER is nil.  This function
ignores `read-symbol-shorthands', so SYM and LEN always correspond to
the symbol as it appears in STREAM.

If STREAM is nil, it defaults to the current buffer.

This function recursively analyzes Lisp forms (HEAD . TAIL), usually
starting with a top-level form, by inspecting HEAD at each level."
  (let ((scope-counter 0)
        (scope-callback callback)
        (read-symbol-shorthands nil)
        (max-lisp-eval-depth 32768))
    (scope-1 (read-positioning-symbols (or stream (current-buffer))))))

(provide 'scope)
;;; scope.el ends here
