(defpackage :lem-lsp-base/converter
  (:use :cl :lem-lsp-base/type)
  (:export :convert-from-json
           :convert-to-json))
(in-package :lem-lsp-base/converter)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; yason options: json-nulls-as-keyword = t json-arrays-as-vectors = t

(define-condition missing-value ()
  ((key :initarg :key)
   (value :initarg :value)
   (type :initarg :type)))

(defun assert-type (value type)
  (unless (typep value type)
    (log:warn "type mismatch: expected: ~A actual: ~A" type value)
    (error 'json-type-error :value value :type type))
  (values))

(defun exist-key-p (hash-table key)
  (let ((default '#:default))
    (not (eq default (gethash key hash-table default)))))

(defun typexpand (type)
  #+sbcl
  (sb-ext:typexpand type)
  #+ccl
  (ccl::type-expand type))

(defun convert-json-to-protocol-object (hash-table class)
  (assert-type hash-table 'hash-table)
  (let ((initargs (loop :with default := '#:default
                        :for slot :in (protocol-class-slots class)
                        :for slot-name := (c2mop:slot-definition-name slot)
                        :for type := (c2mop:slot-definition-type slot)
                        :for key := (lisp-to-pascal-case (string slot-name))
                        :for value := (gethash key hash-table default)
                        :unless (eq value default)
                        :append (list (alexandria:make-keyword slot-name)
                                      (convert-from-json value type key)))))
    (apply #'make-instance class initargs)))

(defun protocol-class-p (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (and (not (typep class 'c2mop:built-in-class))
       (not (null (member 'protocol-object
                          (c2mop:class-precedence-list class)
                          :key #'class-name)))))

(defun convert-from-json (value type &optional context)
  (declare (ignorable context))
  (trivia:match type
    ('lsp-boolean
     (cond ((eq value :null)
            ;; If null, treat as false
            nil)
           (t
            (assert-type value type)
            value)))
    ((or 'lsp-uri
         'lsp-document-uri
         'lsp-integer
         'lsp-uinteger
         'lsp-decimal
         'lsp-regexp
         'lsp-string
         'lsp-null)
     (assert-type value type)
     value)
    ((list 'lsp-array element-type)
     (assert-type value 'vector)
     (map 'vector
          (lambda (element)
            (convert-from-json element element-type))
          value))
    ((list 'lsp-map key-type value-type)
     (assert-type value 'hash-table)
     (let ((hash-table (make-hash-table :test 'equal)))
       (maphash (lambda (key value)
                  (setf (gethash (convert-from-json key key-type) hash-table)
                        (convert-from-json value value-type)))
                value)
       hash-table))
    ((cons 'lsp-tuple types)
     (assert-type value 'vector)
     (unless (alexandria:length= value types)
       (error 'json-type-error :type type :value value))
     (map 'vector #'convert-from-json value types))
    ((list 'lsp-interface properties)
     (assert-type value 'hash-table)
     (loop :with hash-table := (make-hash-table :test 'equal)
           :for (name . options) :in properties
           :do (destructuring-bind (&key (initform nil initform-p) type &allow-other-keys)
                   options
                 (declare (ignore initform))
                 (let ((key (lisp-to-pascal-case (string name))))
                   (cond ((exist-key-p value key)
                          (setf (gethash key hash-table)
                                (convert-from-json (gethash key value) type)))
                         (initform-p ; is not optional
                          (error 'missing-value
                                 :key key
                                 :value value
                                 :type type)))))
           :finally (let* ((src-keys (alexandria:hash-table-keys value))
                           (dst-keys (alexandria:hash-table-keys hash-table))
                           (additional-keys (set-difference src-keys dst-keys :test #'equal)))
                      (loop :for key :in additional-keys
                            :do (setf (gethash key hash-table)
                                      (gethash key value)))
                      (return hash-table))))
    ((cons 'or types)
     (dolist (type types (error 'json-type-error :type type :value value))
       (handler-case
           (return (convert-from-json value type))
         (json-type-error ()))))
    (otherwise
     (let ((class (and (symbolp type)
                       (find-class type nil))))
       (if (and class (protocol-class-p class))
           (convert-json-to-protocol-object value class)
           (multiple-value-bind (type expanded)
               (typexpand type)
             (cond (expanded
                    (convert-from-json value type))
                   (t
                    ;; (assert-type value type)
                    value))))))))

(defmethod convert-to-json ((object protocol-object))
  (loop :with hash-table := (make-hash-table :test 'equal)
        :for slot :in (protocol-class-slots (class-of object))
        :for slot-name := (c2mop:slot-definition-name slot)
        :when (slot-boundp object slot-name)
        :do (let ((value (slot-value object slot-name))
                  (key (lisp-to-pascal-case (string slot-name))))
              (setf (gethash key hash-table)
                    (convert-to-json value)))
        :finally (return hash-table)))

(defmethod convert-to-json ((object string))
  object)

(defmethod convert-to-json ((object vector))
  (map 'vector #'convert-to-json object))

(defmethod convert-to-json ((object hash-table))
  (let ((hash-table (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (setf (gethash k hash-table)
                     (convert-to-json v)))
             object)
    hash-table))

(defmethod convert-to-json (object)
  object)
