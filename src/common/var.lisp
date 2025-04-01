(defpackage :lem/common/var
  (:use :cl)
  (:export
   :editor-variable
   :editor-variable-value
   :editor-variable-local-indicator
   :define-editor-variable
   :clear-editor-local-variables
   :variable-value-aux
   :variable-value
   :variable-documentation
   :find-editor-variable
   :editor-variables
   :with-global-variable-value))
(in-package :lem/common/var)

(defvar *editor-variables* '())

(defstruct editor-variable
  value
  documentation
  local-indicator
  change-value-hook)

(setf (documentation 'editor-variable 'type)
      "`editor-variable` is a variable used within the editor.
It is used to manage variables with both local and global values.
The scope of the variable is decided by the optional `scope`
Parameter to `variable-value`.
The `changed-value-hook` is only called on a global change.")

(defmacro define-editor-variable (var &optional value documentation change-value-hook)
  "Define editor-variable `var`
`value` is the global value of the variable.
`documentation` is the doc-string of the Variable
`change-value-hook` is a hook function called when the global value changes"
  (check-type var symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (get ',var 'editor-variable)
       (defvar ,var)
       (pushnew ',var *editor-variables*)
       (setf (get ',var 'editor-variable)
             (make-editor-variable :value ,value
                                   :documentation ,documentation
                                   :local-indicator (gensym ,(string var))
                                   :change-value-hook ,change-value-hook))
       t)))

(defun editor-variable-error (symbol)
  (error "~A is not editor variable" symbol))

(defun check-editor-variable (symbol)
  "Errors if `symbol` is not an editor-variable."
  (unless (editor-variable-p (get symbol 'editor-variable))
    (editor-variable-error symbol)))

(defgeneric variable-value-aux (var scope &optional where)
  (:documentation
   "Generic funtion to get the value of the editor-variable `var`.
Can be specialized for differend `scope`s."))
(defgeneric (setf variable-value-aux) (value var scope &optional where)
  (:documentation
   "Generic function to set the value of the editor-variable `var`.
Can be specialized for differend `scope`s"))

(defmethod variable-value-aux ((var editor-variable) (scope (eql :global)) &optional (where nil wherep))
  (declare (ignore where wherep))
  (editor-variable-value var))

(defun variable-value (symbol &optional (scope :default) (where nil wherep))
  "Get the value of the editor-variable `symbol`.
See `variable-value-aux`."
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (if wherep
        (variable-value-aux var scope where)
        (variable-value-aux var scope))))

(defmethod (setf variable-value-aux) (value (var editor-variable) (scope (eql :global)) &optional (where nil wherep))
  (declare (ignore where wherep))
  (let ((fn (editor-variable-change-value-hook var)))
    (when fn
      (funcall fn value)))
  (setf (editor-variable-value var) value))

(defun (setf variable-value) (value symbol &optional (scope :default) (where nil wherep))
  "Set the value of the editor-variable `symbol`.
See `setf variable-value-aux`."
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (if wherep
        (setf (variable-value-aux var scope where) value)
        (setf (variable-value-aux var scope) value))))

(defun variable-documentation (symbol)
  "Returns the doc-string of the editor variable `symbol`."
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (editor-variable-documentation var)))

(defun editor-variables ()
  "Returns a list of all defined editor-variables."
  *editor-variables*)

(defun find-editor-variable (var)
  "Find the editor-variable with name (string) `var`."
  (find var (editor-variables) :test 'string-equal))


;; for test
(defun call-with-global-variable-value (var value function)
  (let* ((editor-variable (get var 'editor-variable))
         (save-value (editor-variable-value editor-variable)))
    (setf (editor-variable-value editor-variable) value)
    (unwind-protect (funcall function)
      (setf (editor-variable-value editor-variable) save-value))))

(defmacro with-global-variable-value ((var value) &body body)
  `(call-with-global-variable-value ',var ,value (lambda () ,@body)))
