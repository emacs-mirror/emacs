;;; eieio-compat.el --- Compatibility with Older EIEIO versions  -*- lexical-binding:t -*-

;; Copyright (C) 1995-1996, 1998-2017 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: OO, lisp
;; Package: eieio

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

;; Backward compatibility definition of old EIEIO functions in
;; terms of newer equivalent.

;; The main elements are the old EIEIO `defmethod' and `defgeneric' which are
;; now implemented on top of cl-generic.  The differences we have to
;; accommodate are:
;; - EIEIO's :static methods (turned into a new `eieio--static' specializer).
;; - EIEIO's support for `call-next-method' and `next-method-p' instead of
;;   `cl-next-method-p' and `cl-call-next-method' (simple matter of renaming).
;; - Different errors are signaled.
;; - EIEIO's defgeneric does not reset the function.
;; - EIEIO's no-next-method and no-applicable-method can't be aliases of
;;   cl-generic's namesakes since they have different calling conventions,
;;   which means that packages that (defmethod no-next-method ..) don't work.
;; - EIEIO's `call-next-method' and `next-method-p' had dynamic scope whereas
;;   cl-generic's `cl-next-method-p' and `cl-call-next-method' are lexically
;;   scoped.

;;; Code:

(require 'eieio-core)
(require 'cl-generic)

(put 'eieio--defalias 'byte-hunk-handler
     #'byte-compile-file-form-defalias) ;;(get 'defalias 'byte-hunk-handler)
;;;###autoload
(defun eieio--defalias (name body)
  "Like `defalias', but with less side-effects.
More specifically, it has no side-effects at all when the new function
definition is the same (`eq') as the old one."
  (cl-assert (not (symbolp body)))
  (while (and (fboundp name) (symbolp (symbol-function name)))
    ;; Follow aliases, so methods applied to obsolete aliases still work.
    (setq name (symbol-function name)))
  (unless (and (fboundp name)
               (eq (symbol-function name) body))
    (defalias name body)))

;;;###autoload
(defmacro defgeneric (method args &optional doc-string)
  "Create a generic function METHOD.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as its purpose is to decide which method body
is appropriate to use.  Uses `defmethod' to create methods, and calls
`defgeneric' for you.  With this implementation the ARGS are
currently ignored.  You can use `defgeneric' to apply specialized
top level documentation to a method."
  (declare (doc-string 3) (obsolete cl-defgeneric "25.1"))
  `(eieio--defalias ',method
                    (eieio--defgeneric-init-form
                     ',method
                     ,(if doc-string (help-add-fundoc-usage doc-string args)))))

;;;###autoload
(defmacro defmethod (method &rest args)
  "Create a new METHOD through `defgeneric' with ARGS.

The optional second argument KEY is a specifier that
modifies how the method is called, including:
   :before  - Method will be called before the :primary
   :primary - The default if not specified
   :after   - Method will be called after the :primary
   :static  - First arg could be an object or class
The next argument is the ARGLIST.  The ARGLIST specifies the arguments
to the method as with `defun'.  The first argument can have a type
specifier, such as:
  ((VARNAME CLASS) ARG2 ...)
where VARNAME is the name of the local variable for the method being
created.  The CLASS is a class symbol for a class made with `defclass'.
A DOCSTRING comes after the ARGLIST, and is optional.
All the rest of the args are the BODY of the method.  A method will
return the value of the last form in the BODY.

Summary:

 (defmethod mymethod [:before | :primary | :after | :static]
                     ((typearg class-name) arg2 &optional opt &rest rest)
    \"doc-string\"
     body)"
  (declare (doc-string 3) (obsolete cl-defmethod "25.1")
           (debug
            (&define                    ; this means we are defining something
             [&or name ("setf" name :name setf)]
             ;; ^^ This is the methods symbol
             [ &optional symbolp ]                ; this is key :before etc
             cl-generic-method-args               ; arguments
             [ &optional stringp ]                ; documentation string
             def-body                             ; part to be debugged
             )))
  (let* ((key (if (keywordp (car args)) (pop args)))
	 (params (car args))
	 (arg1 (car params))
         (fargs (if (consp arg1)
                   (cons (car arg1) (cdr params))
                 params))
	 (class (if (consp arg1) (nth 1 arg1)))
         (code `(lambda ,fargs ,@(cdr args))))
    `(progn
       ;; Make sure there is a generic and the byte-compiler sees it.
       (defgeneric ,method ,args)
       (eieio--defmethod ',method ',key ',class #',code))))

(defun eieio--generic-static-symbol-specializers (tag &rest _)
  (cl-assert (or (null tag) (eieio--class-p tag)))
  (when (eieio--class-p tag)
    (let ((superclasses (eieio--generic-subclass-specializers tag))
	  (specializers ()))
      (dolist (superclass superclasses)
	(push superclass specializers)
	(push `(eieio--static ,(cadr superclass)) specializers))
      (nreverse specializers))))

(cl-generic-define-generalizer eieio--generic-static-symbol-generalizer
  ;; Give it a slightly higher priority than `subclass' so that the
  ;; interleaved list comes before subclass's non-interleaved list.
  61 (lambda (name &rest _) `(and (symbolp ,name) (cl--find-class ,name)))
  #'eieio--generic-static-symbol-specializers)
(cl-generic-define-generalizer eieio--generic-static-object-generalizer
  ;; Give it a slightly higher priority than `class' so that the
  ;; interleaved list comes before the class's non-interleaved list.
  51 #'cl--generic-struct-tag
  (lambda (tag &rest _)
    (and (symbolp tag) (setq tag (cl--find-class tag))
         (eieio--class-p tag)
         (let ((superclasses (eieio--class-precedence-list tag))
               (specializers ()))
           (dolist (superclass superclasses)
             (setq superclass (eieio--class-name superclass))
             (push superclass specializers)
             (push `(eieio--static ,superclass) specializers))
           (nreverse specializers)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head eieio--static)))
  (list eieio--generic-static-symbol-generalizer
        eieio--generic-static-object-generalizer))

;;;###autoload
(defun eieio--defgeneric-init-form (method doc-string)
  (if doc-string (put method 'function-documentation doc-string))
  (if (memq method '(no-next-method no-applicable-method))
      (symbol-function method)
    (let ((generic (cl-generic-ensure-function method)))
      (or (symbol-function (cl--generic-name generic))
          (cl--generic-make-function generic)))))

;;;###autoload
(defun eieio--defmethod (method kind argclass code)
  (setq kind (intern (downcase (symbol-name kind))))
  (let* ((specializer (if (not (eq kind :static))
                          (or argclass t)
                        (setq kind nil)
                        `(eieio--static ,argclass)))
         (uses-cnm (not (memq kind '(:before :after))))
         (specializers `((arg ,specializer)))
         (code
          ;; Backward compatibility for `no-next-method' and
          ;; `no-applicable-method', which have slightly different calling
          ;; convention than their cl-generic counterpart.
          (pcase method
            (`no-next-method
             (setq method 'cl-no-next-method)
             (setq specializers `(generic method ,@specializers))
             (lambda (_generic _method &rest args) (apply code args)))
            (`no-applicable-method
             (setq method 'cl-no-applicable-method)
             (setq specializers `(generic ,@specializers))
             (lambda (generic arg &rest args)
               (apply code arg (cl--generic-name generic) (cons arg args))))
            (_ code))))
    (cl-generic-define-method
     method (unless (memq kind '(nil :primary)) (list kind))
     specializers uses-cnm
     (if uses-cnm
         (let* ((docstring (documentation code 'raw))
                (args (help-function-arglist code 'preserve-names))
                (doc-only (if docstring
                              (let ((split (help-split-fundoc docstring nil)))
                                (if split (cdr split) docstring)))))
           (lambda (cnm &rest args)
             (:documentation
              (help-add-fundoc-usage doc-only (cons 'cl-cnm args)))
             (cl-letf (((symbol-function 'call-next-method) cnm)
                       ((symbol-function 'next-method-p)
                        (lambda () (cl--generic-isnot-nnm-p cnm))))
               (apply code args))))
       code))
    ;; The old EIEIO code did not signal an error when there are methods
    ;; applicable but only of the before/after kind.  So if we add a :before
    ;; or :after, make sure there's a matching dummy primary.
    (when (and (memq kind '(:before :after))
               ;; FIXME: Use `cl-find-method'?
               (not (cl-find-method method ()
                                    (mapcar (lambda (arg)
                                              (if (consp arg) (nth 1 arg) t))
                                            specializers))))
      (cl-generic-define-method method () specializers t
                                (lambda (cnm &rest args)
                                  (if (cl--generic-isnot-nnm-p cnm)
                                      (apply cnm args)))))
    method))

;; Compatibility with code which tries to catch `no-method-definition' errors.
(push 'no-method-definition (get 'cl-no-applicable-method 'error-conditions))

(defun generic-p (fname) (not (null (cl--generic fname))))

(defun no-next-method (&rest args)
  (declare (obsolete cl-no-next-method "25.1"))
  (apply #'cl-no-next-method 'unknown nil args))

(defun no-applicable-method (object method &rest args)
  (declare (obsolete cl-no-applicable-method "25.1"))
  (apply #'cl-no-applicable-method method object args))

(define-obsolete-function-alias 'call-next-method 'cl-call-next-method "25.1")
(defun next-method-p ()
  (declare (obsolete cl-next-method-p "25.1"))
  ;; EIEIO's `next-method-p' just returned nil when called in an
  ;; invalid context.
  (message "next-method-p called outside of a primary or around method")
  nil)

;;;###autoload
(defun eieio-defmethod (method args)
  "Obsolete work part of an old version of the `defmethod' macro."
  (declare (obsolete cl-defmethod "24.1"))
  (eval `(defmethod ,method ,@args))
  method)

;;;###autoload
(defun eieio-defgeneric (method doc-string)
  "Obsolete work part of an old version of the `defgeneric' macro."
  (declare (obsolete cl-defgeneric "24.1"))
  (eval `(defgeneric ,method (x) ,@(if doc-string `(,doc-string))))
  ;; Return the method
  'method)

;;;###autoload
(defun eieio-defclass (cname superclasses slots options)
  (declare (obsolete eieio-defclass-internal "25.1"))
  (eval `(defclass ,cname ,superclasses ,slots ,@options)))


;; Local Variables:
;; generated-autoload-file: "eieio-loaddefs.el"
;; End:

(provide 'eieio-compat)

;;; eieio-compat.el ends here
