(defpackage :lem-lsp-base/type
  (:use :cl)
  (:export :+null+
           :+true+
           :+false+
           :json-type-error
           :required-argument-error
           :lsp-uri
           :lsp-document-uri
           :lsp-integer
           :lsp-uinteger
           :lsp-decimal
           :lsp-regexp
           :lsp-string
           :lsp-boolean
           :lsp-null
           :lsp-array
           :lsp-map
           :lsp-tuple
           :lsp-interface
           :protocol-object
           :protocol-class
           :define-enum
           :define-type-alias
           :define-class
           :request-message
           :request-message-deprecated
           :request-message-documentation
           :request-message-error-data
           :request-message-message-direction
           :request-message-method
           :request-message-params
           :request-message-partial-result
           :request-message-proposed
           :request-message-registration-method
           :request-message-registration-options
           :request-message-result
           :request-message-since
           :notification-message
           :notification-message-deprecated
           :notification-message-documentation
           :notification-message-message-direction
           :notification-message-method
           :notification-message-params
           :notification-message-proposed
           :notification-message-registration-method
           :notification-message-registration-options
           :notification-message-since
           :define-request-message
           :define-notification-message
           :protocol-class-slots
           :pascal-to-lisp-case
           :lisp-to-pascal-case
           :make-lsp-map
           :make-lsp-array
           :get-map
           :lsp-array-p
           :lsp-null-p))
(in-package :lem-lsp-base/type)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defconstant +null+ :null)
(defconstant +true+ t)
(defconstant +false+ nil)

(define-condition json-type-error ()
  ((type :initarg :type)
   (value :initarg :value)
   (context :initarg :context :initform nil))
  (:report (lambda (c s)
             (with-slots (value type context) c
               (if context
                   (format s "~S is not a ~S in ~S" value type context)
                   (format s "~S is not a ~S" value type))))))

(define-condition required-argument-error (json-type-error)
  ((slot-name :initarg :slot-name)
   (class-name :initarg :class-name))
  (:report (lambda (condition stream)
             (with-slots (slot-name class-name) condition
               (format stream
                       "Required argument ~A missing for ~A."
                       slot-name
                       class-name)))))

(deftype lsp-uri () 'string)
(deftype lsp-document-uri () 'string)
(deftype lsp-integer () 'integer)
(deftype lsp-uinteger () '(integer 0 *))
(deftype lsp-decimal () 'integer)
(deftype lsp-regexp () 'string)
(deftype lsp-string (&optional (string nil stringp))
  (if stringp
      (labels ((f (value)
                 (equal value string)))
        (let ((g (gensym)))
          (setf (symbol-function g) #'f)
          `(satisfies ,g)))
      'string))
(deftype lsp-boolean () 'boolean)
(deftype lsp-null () '(eql :null))

(deftype lsp-array (&optional element-type)
  (declare (ignore element-type))
  'vector)

(deftype lsp-map (key value)
  (declare (ignore key value))
  'hash-table)

(deftype lsp-tuple (&rest types)
  (declare (ignore types))
  'vector)

(deftype lsp-interface (properties &key &allow-other-keys)
  (declare (ignore properties))
  'hash-table)

(defclass protocol-class (c2mop:standard-class)
  ((deprecated :initarg :deprecated
               :reader protocol-class-deprecated)
   (proposed :initarg :proposed
             :reader protocol-class-proposed)
   (since :initarg :since
          :reader protocol-class-since)))

(defmethod c2mop:validate-superclass ((class protocol-class)
                                      (super c2mop:standard-class))
  t)

(defmethod c2mop:validate-superclass ((class c2mop:standard-class)
                                      (super protocol-class))
  t)

(defclass protocol-slot (c2mop:standard-direct-slot-definition)
  ((optional
    :initarg :optional
    :initform nil
    :reader protocol-slot-optional-p)
   (deprecated
    :initarg :deprecated
    :initform nil
    :reader protocol-slot-deprecated)
   (proposed
    :initarg :proposed
    :reader protocol-slot-proposed)
   (since
    :initarg :since
    :reader protocol-slot-since)))

(defmethod c2mop:direct-slot-definition-class ((class protocol-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'protocol-slot))

(defclass protocol-object () ()
  (:metaclass protocol-class))

(defun protocol-class-slots (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (loop :with base := (find-class 'protocol-class)
        :for superclass :in (c2mop:class-precedence-list class)
        :while (eq base (class-of superclass))
        :append (c2mop:class-direct-slots superclass)))

(defun check-argument-is-required (class slot)
  (unless (protocol-slot-optional-p slot)
    (error 'required-argument-error
           :slot-name (c2mop:slot-definition-name slot)
           :class-name (class-name class))))

(defun check-initargs (protocol-object)
  (loop :with class := (class-of protocol-object)
        :for slot :in (protocol-class-slots class)
        :for slot-name := (c2mop:slot-definition-name slot)
        :do (cond ((slot-boundp protocol-object slot-name)
                   (let ((value (slot-value protocol-object slot-name))
                         (expected-type (c2mop:slot-definition-type slot)))
                     (unless (typep value expected-type)
                       (if (eq value :null)
                           ;; If null, treat as no argument
                           (check-argument-is-required class slot)
                           (error 'json-type-error
                                  :type expected-type
                                  :value value
                                  :context slot-name)))))
                  (t
                   (check-argument-is-required class slot)))))

(defmethod initialize-instance ((instance protocol-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (check-initargs instance)
    instance))

#+(or)
(defmacro define-enum (name (&rest fields) &body options)
  (declare (ignore options))
  (alexandria:with-unique-names (f x anon-name)
    (let ((field-values (mapcar #'second fields)))
      `(progn
         (deftype ,name ()
           (labels ((,f (,x) (member ,x ',field-values :test #'equal)))
             (let ((,anon-name (gensym)))
               (setf (symbol-function ,anon-name) #',f)
               `(satisfies ,,anon-name))))
         ,@(loop :for (field-name value) :in fields
                 :for variable := (intern (format nil "~A-~A" name field-name))
                 :collect `(defparameter ,variable ,value))
         ',name))))

(defmacro define-enum (name (&rest fields) &body options)
  (declare (ignore options))
  `(progn
     (deftype ,name ()
       t)
     ,@(loop :for (field-name value) :in fields
             :for variable := (intern (format nil "~A-~A" name field-name))
             :collect `(defparameter ,variable ,value))
     ',name))

(defmacro define-type-alias (name type &body options)
  (let ((doc (second (assoc :documentation options))))
    `(deftype ,name () ,@(when `(,doc)) ',type)))

(defmacro define-class (name superclasses &body args)
  `(defclass ,name ,(if (null superclasses)
                        '(protocol-object)
                        superclasses)
     ,@args
     (:metaclass protocol-class)))

(defclass request-message ()
  ((deprecated
    :initarg :deprecated
    :reader request-message-deprecated)
   (documentation
    :initarg :documentation
    :reader request-message-documentation)
   (error-data
    :initarg :error-data
    :reader request-message-error-data)
   (message-direction
    :initarg :message-direction
    :reader request-message-message-direction)
   (method
    :initarg :method
    :reader request-message-method)
   (params
    :initarg :params
    :reader request-message-params)
   (partial-result
    :initarg :partial-result
    :reader request-message-partial-result)
   (proposed
    :initarg :proposed
    :reader request-message-proposed)
   (registration-method
    :initarg :registration-method
    :reader request-message-registration-method)
   (registration-options
    :initarg :registration-options
    :reader request-message-registration-options)
   (result
    :initarg :result
    :reader request-message-result)
   (since
    :initarg :since
    :reader request-message-since)))

(defclass notification-message ()
  ((deprecated
    :initarg :deprecated
    :reader notification-message-deprecated)
   (documentation
    :initarg :documentation
    :reader notification-message-documentation)
   (message-direction
    :initarg :message-direction
    :reader notification-message-message-direction)
   (method
    :initarg :method
    :reader notification-message-method)
   (params
    :initarg :params
    :reader notification-message-params)
   (proposed
    :initarg :proposed
    :reader notification-message-proposed)
   (registration-method
    :initarg :registration-method
    :reader notification-message-registration-method)
   (registration-options
    :initarg :registration-options
    :reader notification-message-registration-options)
   (since
    :initarg :since
    :reader notification-message-since)))

(defmacro define-request-message (name () &body default-initargs)
  `(defclass ,name (request-message)
     ()
     (:default-initargs ,@default-initargs)))

(defmacro define-notification-message (name () &body default-initargs)
  `(defclass ,name (notification-message)
     ()
     (:default-initargs ,@default-initargs)))

(defun param-case (string)
  (format nil "~{~A~^/~}"
          (loop :for string-part :in (split-sequence:split-sequence #\/ string)
                :collect (cl-change-case:param-case string-part))))

(defun pascal-to-lisp-case (string)
  (string-upcase
   (if (alexandria:starts-with-subseq "_" string)
       (uiop:strcat "_" (param-case string))
       (param-case string))))

(defun lisp-to-pascal-case (string)
  (if (alexandria:starts-with-subseq "_" string)
      (uiop:strcat "_" (cl-change-case:camel-case string))
      (cl-change-case:camel-case string)))

(defun make-lsp-map (&rest key-value-pairs)
  (let ((hash-table (make-hash-table :test 'equal)))
    (loop :for (key value) :on key-value-pairs :by #'cddr
          :do (let ((key (etypecase key
                           (string key)
                           (keyword (lisp-to-pascal-case (string key))))))
                (setf (gethash key hash-table) value)))
    hash-table))

(defun make-lsp-array (&rest args)
  (apply #'vector args))

(defun get-map (lsp-map key &optional default)
  (gethash key lsp-map default))

(defun lsp-array-p (value)
  (typep value 'lsp-array))

(defun lsp-null-p (value)
  (eq value +null+))
