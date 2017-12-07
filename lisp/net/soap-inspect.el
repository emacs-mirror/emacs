;;; soap-inspect.el --- Interactive WSDL inspector    -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

;; Author: Alexandru Harsanyi <AlexHarsanyi@gmail.com>
;; Created: October 2010
;; Keywords: soap, web-services, comm, hypermedia
;; Package: soap-client
;; Homepage: https://github.com/alex-hhh/emacs-soap-client

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
;;
;; This package provides an inspector for a WSDL document loaded with
;; `soap-load-wsdl' or `soap-load-wsdl-from-url'.  To use it, evaluate:
;;
;; (soap-inspect *wsdl*)
;;
;; This will pop-up the inspector buffer.  You can click on ports, operations
;; and types to explore the structure of the wsdl document.
;;


;;; Code:

(require 'cl-lib)
(require 'soap-client)

;;; sample-value

(defun soap-sample-value (type)
  "Provide a sample value for TYPE, a WSDL type.
A sample value is a LISP value which soap-client.el will accept
for encoding it using TYPE when making SOAP requests.

This is a generic function, depending on TYPE a specific function
will be called."
  (let ((sample-value (get (aref type 0) 'soap-sample-value)))
    (if sample-value
        (funcall sample-value type)
      (error "Cannot provide sample value for type %s" (aref type 0)))))

(defun soap-sample-value-for-xs-basic-type (type)
  "Provide a sample value for TYPE, an xs-basic-type.
This is a specialization of `soap-sample-value' for xs-basic-type
objects."
  (cl-case (soap-xs-basic-type-kind type)
    (string "a string")
    (anyURI "an URI")
    (QName "a QName")
    (dateTime "a time-value-p or string")
    (boolean "t or nil")
    ((long int integer byte unsignedInt) 42)
    ((float double) 3.14)
    (base64Binary "a string")
    (t (format "%s" (soap-xs-basic-type-kind type)))))

(defun soap-sample-value-for-xs-element (element)
  "Provide a sample value for ELEMENT, a WSDL element.
This is a specialization of `soap-sample-value' for xs-element
objects."
  (if (soap-xs-element-name element)
      (cons (intern (soap-xs-element-name element))
            (soap-sample-value (soap-xs-element-type element)))
    (soap-sample-value (soap-xs-element-type element))))

(defun soap-sample-value-for-xs-attribute (attribute)
  "Provide a sample value for ATTRIBUTE, a WSDL attribute.
This is a specialization of `soap-sample-value' for
soap-xs-attribute objects."
  (if (soap-xs-attribute-name attribute)
      (cons (intern (soap-xs-attribute-name attribute))
            (soap-sample-value (soap-xs-attribute-type attribute)))
    (soap-sample-value (soap-xs-attribute-type attribute))))

(defun soap-sample-value-for-xs-attribute-group (attribute-group)
  "Provide a sample value for ATTRIBUTE-GROUP, a WSDL attribute group.
This is a specialization of `soap-sample-value' for
soap-xs-attribute objects."
  (let ((sample-values nil))
    (dolist (attribute (soap-xs-attribute-group-attributes attribute-group))
      (if (soap-xs-attribute-name attribute)
          (setq sample-values
                (append sample-values
                        (cons (intern (soap-xs-attribute-name attribute))
                              (soap-sample-value (soap-xs-attribute-type
                                                  attribute)))))
        (setq sample-values
              (append sample-values
                      (soap-sample-value
                       (soap-xs-attribute-type attribute))))))))

(defun soap-sample-value-for-xs-simple-type (type)
  "Provide a sample value for TYPE, a `soap-xs-simple-type'.
This is a specialization of `soap-sample-value' for
`soap-xs-simple-type' objects."
  (append
   (mapcar 'soap-sample-value-for-xs-attribute
           (soap-xs-type-attributes type))
   (cond
    ((soap-xs-simple-type-enumeration type)
     (let ((enumeration (soap-xs-simple-type-enumeration type)))
       (nth (random (length enumeration)) enumeration)))
    ((soap-xs-simple-type-pattern type)
     (format "a string matching %s" (soap-xs-simple-type-pattern type)))
    ((soap-xs-simple-type-length-range type)
     (cl-destructuring-bind (low . high) (soap-xs-simple-type-length-range type)
       (cond
        ((and low high)
         (format "a string between %d and %d chars long" low high))
        (low (format "a string at least %d chars long" low))
        (high (format "a string at most %d chars long" high))
        (t (format "a string OOPS")))))
    ((soap-xs-simple-type-integer-range type)
     (cl-destructuring-bind (min . max) (soap-xs-simple-type-integer-range type)
       (cond
        ((and min max) (+ min (random (- max min))))
        (min (+ min (random 10)))
        (max (random max))
        (t (random 100)))))
    ((consp (soap-xs-simple-type-base type)) ; an union of values
     (let ((base (soap-xs-simple-type-base type)))
       (soap-sample-value (nth (random (length base)) base))))
    ((soap-xs-basic-type-p (soap-xs-simple-type-base type))
     (soap-sample-value (soap-xs-simple-type-base type))))))

(defun soap-sample-value-for-xs-complex-type (type)
  "Provide a sample value for TYPE, a `soap-xs-complex-type'.
This is a specialization of `soap-sample-value' for
`soap-xs-complex-type' objects."
  (append
   (mapcar 'soap-sample-value-for-xs-attribute
           (soap-xs-type-attributes type))
   (cl-case (soap-xs-complex-type-indicator type)
     (array
      (let* ((element-type (soap-xs-complex-type-base type))
             (sample1 (soap-sample-value element-type))
             (sample2 (soap-sample-value element-type)))
        ;; Our sample value is a vector of two elements, but any number of
        ;; elements are permissible
        (vector sample1 sample2 '&etc)))
     ((sequence choice all)
      (let ((base (soap-xs-complex-type-base type)))
        (let ((value (append (and base (soap-sample-value base))
                             (mapcar #'soap-sample-value
                                     (soap-xs-complex-type-elements type)))))
          (if (eq (soap-xs-complex-type-indicator type) 'choice)
              (cons '***choice-of*** value)
            value)))))))

(defun soap-sample-value-for-message (message)
  "Provide a sample value for a WSDL MESSAGE.
This is a specialization of `soap-sample-value' for
`soap-message' objects."
  ;; NOTE: parameter order is not considered.
  (let (sample-value)
    (dolist (part (soap-message-parts message))
      (push (soap-sample-value (cdr part)) sample-value))
    (nreverse sample-value)))

(progn
  ;; Install soap-sample-value methods for our types
  (put (aref (make-soap-xs-basic-type) 0)
       'soap-sample-value
       'soap-sample-value-for-xs-basic-type)

  (put (aref (make-soap-xs-element) 0)
       'soap-sample-value
       'soap-sample-value-for-xs-element)

  (put (aref (make-soap-xs-attribute) 0)
       'soap-sample-value
       'soap-sample-value-for-xs-attribute)

  (put (aref (make-soap-xs-attribute) 0)
       'soap-sample-value
       'soap-sample-value-for-xs-attribute-group)

  (put (aref (make-soap-xs-simple-type) 0)
       'soap-sample-value
       'soap-sample-value-for-xs-simple-type)

  (put (aref (make-soap-xs-complex-type) 0)
       'soap-sample-value
       'soap-sample-value-for-xs-complex-type)

  (put (aref (make-soap-message) 0)
       'soap-sample-value
       'soap-sample-value-for-message))



;;; soap-inspect

(defvar soap-inspect-previous-items nil
  "A stack of previously inspected items in the *soap-inspect* buffer.
Used to implement the BACK button.")

(defvar soap-inspect-current-item nil
  "The current item being inspected in the *soap-inspect* buffer.")

(progn
  (make-variable-buffer-local 'soap-inspect-previous-items)
  (make-variable-buffer-local 'soap-inspect-current-item))

(defun soap-inspect (element)
  "Inspect a SOAP ELEMENT in the *soap-inspect* buffer.
The buffer is populated with information about ELEMENT with links
to its sub elements.  If ELEMENT is the WSDL document itself, the
entire WSDL can be inspected."
  (let ((inspect (get (aref element 0) 'soap-inspect)))
    (unless inspect
      (error "Soap-inspect: no inspector for element"))

    (with-current-buffer (get-buffer-create "*soap-inspect*")
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)

        (when soap-inspect-current-item
          (push soap-inspect-current-item
                soap-inspect-previous-items))
        (setq soap-inspect-current-item element)

        (funcall inspect element)

        (unless (null soap-inspect-previous-items)
          (insert "\n\n")
          (insert-text-button
           "[back]"
           'type 'soap-client-describe-back-link
           'item element)
          (insert "\n"))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))


(define-button-type 'soap-client-describe-link
  'face 'link
  'help-echo "mouse-2, RET: describe item"
  'follow-link t
  'action (lambda (button)
            (let ((item (button-get button 'item)))
              (soap-inspect item)))
  'skip t)

(define-button-type 'soap-client-describe-back-link
  'face 'link
  'help-echo "mouse-2, RET: browse the previous item"
  'follow-link t
  'action (lambda (_button)
            (let ((item (pop soap-inspect-previous-items)))
              (when item
                (setq soap-inspect-current-item nil)
                (soap-inspect item))))
  'skip t)

(defun soap-insert-describe-button (element)
  "Insert a button to inspect ELEMENT when pressed."
  (insert-text-button
   (soap-element-fq-name element)
   'type 'soap-client-describe-link
   'item element))

(defun soap-inspect-xs-basic-type (type)
  "Insert information about TYPE, a soap-xs-basic-type, in the current buffer."
  (insert "Basic type: " (soap-element-fq-name type))
  (insert "\nSample value:\n")
  (pp (soap-sample-value type) (current-buffer)))

(defun soap-inspect-xs-element (element)
  "Insert information about ELEMENT, a soap-xs-element, in the current buffer."
  (insert "Element: " (soap-element-fq-name element))
  (insert "\nType: ")
  (soap-insert-describe-button (soap-xs-element-type element))
  (insert "\nAttributes:")
  (when (soap-xs-element-optional? element)
    (insert " optional"))
  (when (soap-xs-element-multiple? element)
    (insert " multiple"))
  (insert "\nSample value:\n")
  (pp (soap-sample-value element) (current-buffer)))

(defun soap-inspect-xs-attribute (attribute)
  "Insert information about ATTRIBUTE in the current buffer.
ATTRIBUTE is a soap-xs-attribute."
  (insert "Attribute: " (soap-element-fq-name attribute))
  (insert "\nType: ")
  (soap-insert-describe-button (soap-xs-attribute-type attribute))
  (insert "\nSample value:\n")
  (pp (soap-sample-value attribute) (current-buffer)))

(defun soap-inspect-xs-attribute-group (attribute-group)
  "Insert information about ATTRIBUTE-GROUP in the current buffer.
ATTRIBUTE is a soap-xs-attribute-group."
  (insert "Attribute group: " (soap-element-fq-name attribute-group))
  (insert "\nSample values:\n")
  (pp (soap-sample-value attribute-group) (current-buffer)))

(defun soap-inspect-xs-simple-type (type)
  "Insert information about TYPE, a soap-xs-simple-type, in the current buffer."
  (insert "Simple type: " (soap-element-fq-name type))
  (insert "\nBase: " )
  (if (listp (soap-xs-simple-type-base type))
      (let ((first-time t))
        (dolist (b (soap-xs-simple-type-base type))
          (unless first-time
            (insert ", ")
            (setq first-time nil))
          (soap-insert-describe-button b)))
    (soap-insert-describe-button (soap-xs-simple-type-base type)))
  (insert "\nAttributes: ")
  (dolist (attribute (soap-xs-simple-type-attributes type))
    (let ((name (or (soap-xs-attribute-name attribute) "*inline*"))
          (type (soap-xs-attribute-type attribute)))
      (insert "\n\t")
      (insert name)
      (insert "\t")
      (soap-insert-describe-button type)))
  (when (soap-xs-simple-type-enumeration type)
    (insert "\nEnumeration values: ")
    (dolist (e (soap-xs-simple-type-enumeration type))
      (insert "\n\t")
      (pp e)))
  (when (soap-xs-simple-type-pattern type)
    (insert "\nPattern: " (soap-xs-simple-type-pattern type)))
  (when (car (soap-xs-simple-type-length-range type))
    (insert "\nMin length: "
            (number-to-string (car (soap-xs-simple-type-length-range type)))))
  (when (cdr (soap-xs-simple-type-length-range type))
    (insert "\nMin length: "
            (number-to-string (cdr (soap-xs-simple-type-length-range type)))))
  (when (car (soap-xs-simple-type-integer-range type))
    (insert "\nMin value: "
            (number-to-string (car (soap-xs-simple-type-integer-range type)))))
  (when (cdr (soap-xs-simple-type-integer-range type))
    (insert "\nMin value: "
            (number-to-string (cdr (soap-xs-simple-type-integer-range type)))))
  (insert "\nSample value:\n")
  (pp (soap-sample-value type) (current-buffer)))

(defun soap-inspect-xs-complex-type (type)
  "Insert information about TYPE in the current buffer.
TYPE is a `soap-xs-complex-type'"
  (insert "Complex type: " (soap-element-fq-name type))
  (insert "\nKind: ")
  (cl-case (soap-xs-complex-type-indicator type)
    ((sequence all)
     (insert "a sequence ")
     (when (soap-xs-complex-type-base type)
       (insert "extending ")
       (soap-insert-describe-button (soap-xs-complex-type-base type)))
     (insert "\nAttributes: ")
     (dolist (attribute (soap-xs-complex-type-attributes type))
       (let ((name (or (soap-xs-attribute-name attribute) "*inline*"))
             (type (soap-xs-attribute-type attribute)))
         (insert "\n\t")
         (insert name)
         (insert "\t")
         (soap-insert-describe-button type)))
     (insert "\nElements: ")
     (let ((name-width 0)
           (type-width 0))
       (dolist (element (soap-xs-complex-type-elements type))
         (let ((name (or (soap-xs-element-name element) "*inline*"))
               (type (soap-xs-element-type element)))
           (setq name-width (max name-width (length name)))
           (setq type-width
                 (max type-width (length (soap-element-fq-name type))))))
       (setq name-width (+ name-width 2))
       (setq type-width (+ type-width 2))
       (dolist (element (soap-xs-complex-type-elements type))
         (let ((name (or (soap-xs-element-name element) "*inline*"))
               (type (soap-xs-element-type element)))
           (insert "\n\t")
           (insert name)
           (insert (make-string (- name-width (length name)) ?\ ))
           (soap-insert-describe-button type)
           (insert
            (make-string
             (- type-width (length (soap-element-fq-name type))) ?\ ))
           (when (soap-xs-element-multiple? element)
             (insert " multiple"))
           (when (soap-xs-element-optional? element)
             (insert " optional"))))))
    (choice
     (insert "a choice ")
     (when (soap-xs-complex-type-base type)
       (insert "extending ")
       (soap-insert-describe-button (soap-xs-complex-type-base type)))
     (insert "\nElements: ")
     (dolist (element (soap-xs-complex-type-elements type))
       (insert "\n\t")
       (soap-insert-describe-button element)))
    (array
     (insert "an array of ")
     (soap-insert-describe-button (soap-xs-complex-type-base type))))
  (insert "\nSample value:\n")
  (pp (soap-sample-value type) (current-buffer)))


(defun soap-inspect-message (message)
  "Insert information about MESSAGE into the current buffer."
  (insert "Message name: " (soap-element-fq-name message) "\n")
  (insert "Parts:\n")
  (dolist (part (soap-message-parts message))
    (insert "\t" (symbol-name (car part))
            " type: ")
    (soap-insert-describe-button (cdr part))
    (insert "\n")))

(defun soap-inspect-operation (operation)
  "Insert information about OPERATION into the current buffer."
  (insert "Operation name: " (soap-element-fq-name operation) "\n")
  (let ((input (soap-operation-input operation)))
    (insert "\tInput: " (symbol-name (car input)) " (" )
    (soap-insert-describe-button (cdr input))
    (insert ")\n"))
  (let ((output (soap-operation-output operation)))
    (insert "\tOutput: " (symbol-name (car output)) " (")
    (soap-insert-describe-button (cdr output))
    (insert ")\n"))

  (insert "\n\nSample invocation:\n")
  (let ((sample-message-value
         (soap-sample-value (cdr (soap-operation-input operation))))
        (funcall (list 'soap-invoke '*WSDL* "SomeService"
                       (soap-element-name operation))))
    (let ((sample-invocation
           (append funcall (mapcar 'cdr sample-message-value))))
      (pp sample-invocation (current-buffer)))))

(defun soap-inspect-port-type (port-type)
  "Insert information about PORT-TYPE into the current buffer."
  (insert "Port-type name: " (soap-element-fq-name port-type) "\n")
  (insert "Operations:\n")
  (cl-loop for o being the hash-values of
           (soap-namespace-elements (soap-port-type-operations port-type))
           do (progn
                (insert "\t")
                (soap-insert-describe-button (car o)))))

(defun soap-inspect-binding (binding)
  "Insert information about BINDING into the current buffer."
  (insert "Binding: " (soap-element-fq-name binding) "\n")
  (insert "\n")
  (insert "Bound operations:\n")
  (let* ((ophash (soap-binding-operations binding))
         (operations (cl-loop for o being the hash-keys of ophash
                              collect o))
         op-name-width)

    (setq operations (sort operations 'string<))

    (setq op-name-width (cl-loop for o in operations maximizing (length o)))

    (dolist (op operations)
      (let* ((bound-op (gethash op ophash))
             (soap-action (soap-bound-operation-soap-action bound-op))
             (use (soap-bound-operation-use bound-op)))
        (unless soap-action
          (setq soap-action ""))
        (insert "\t")
        (soap-insert-describe-button (soap-bound-operation-operation bound-op))
        (when (or use (not (equal soap-action "")))
          (insert (make-string (- op-name-width (length op)) ?\s))
          (insert " (")
          (insert soap-action)
          (when use
            (insert " " (symbol-name use)))
          (insert ")"))
        (insert "\n")))))

(defun soap-inspect-port (port)
  "Insert information about PORT into the current buffer."
  (insert "Port name:   " (soap-element-name port) "\n"
          "Service URL: " (soap-port-service-url port) "\n"
          "Binding:     ")
  (soap-insert-describe-button (soap-port-binding port)))

(defun soap-inspect-wsdl (wsdl)
  "Insert information about WSDL into the current buffer."
  (insert "WSDL Origin: " (soap-wsdl-origin wsdl) "\n")
  (insert "Ports:")
  (dolist (p (soap-wsdl-ports wsdl))
    (insert "\n--------------------\n")
    ;; (soap-insert-describe-button p)
    (soap-inspect-port p))
  (insert "\n--------------------\nNamespace alias table:\n")
  (dolist (a (soap-wsdl-alias-table wsdl))
    (insert "\t" (car a) " => " (cdr a) "\n")))

(progn
  ;; Install the soap-inspect methods for our types

  (put (aref (make-soap-xs-basic-type) 0) 'soap-inspect
       'soap-inspect-xs-basic-type)

  (put (aref (make-soap-xs-element) 0) 'soap-inspect
       'soap-inspect-xs-element)

  (put (aref (make-soap-xs-simple-type) 0) 'soap-inspect
       'soap-inspect-xs-simple-type)

  (put (aref (make-soap-xs-complex-type) 0) 'soap-inspect
       'soap-inspect-xs-complex-type)

  (put (aref (make-soap-xs-attribute) 0) 'soap-inspect
       'soap-inspect-xs-attribute)

  (put (aref (make-soap-xs-attribute-group) 0) 'soap-inspect
       'soap-inspect-xs-attribute-group)

  (put (aref (make-soap-message) 0) 'soap-inspect
       'soap-inspect-message)
  (put (aref (make-soap-operation) 0) 'soap-inspect
       'soap-inspect-operation)

  (put (aref (make-soap-port-type) 0) 'soap-inspect
       'soap-inspect-port-type)

  (put (aref (make-soap-binding) 0) 'soap-inspect
       'soap-inspect-binding)

  (put (aref (make-soap-port) 0) 'soap-inspect
       'soap-inspect-port)

  (put (aref (soap-make-wsdl "origin") 0) 'soap-inspect
       'soap-inspect-wsdl))

(provide 'soap-inspect)
;;; soap-inspect.el ends here
