;;; soap-client.el --- Access SOAP web services       -*- lexical-binding: t -*-

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

;; Author: Alexandru Harsanyi <AlexHarsanyi@gmail.com>
;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Created: December, 2009
;; Version: 3.2.0
;; Keywords: soap, web-services, comm, hypermedia
;; Package: soap-client
;; URL: https://github.com/alex-hhh/emacs-soap-client
;; Package-Requires: ((cl-lib "0.6.1"))
;;FIXME: Put in `Package-Requires:' the Emacs version we expect.

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
;; To use the SOAP client, you first need to load the WSDL document for the
;; service you want to access, using `soap-load-wsdl-from-url'.  A WSDL
;; document describes the available operations of the SOAP service, how their
;; parameters and responses are encoded.  To invoke operations, you use the
;; `soap-invoke' method passing it the WSDL, the service name, the operation
;; you wish to invoke and any required parameters.
;;
;; Ideally, the service you want to access will have some documentation about
;; the operations it supports.  If it does not, you can try using
;; `soap-inspect' to browse the WSDL document and see the available operations
;; and their parameters.
;;

;;; Code:

(require 'cl-lib)

(require 'xml)
(require 'xsd-regexp)
(require 'rng-xsd)
(require 'rng-dt)
(require 'warnings)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'url-vars)
(require 'mm-decode)

(defsubst soap-warning (message &rest args)
  "Display a warning MESSAGE with ARGS, using the `soap-client' warning type."
  ;; Do not use #'format-message, to support older Emacs versions.
  (display-warning 'soap-client (apply #'format message args) :warning))

(defgroup soap-client nil
  "Access SOAP web services from Emacs."
  :version "24.1"
  :group 'tools)

;;;; Support for parsing XML documents with namespaces

;; XML documents with namespaces are difficult to parse because the names of
;; the nodes depend on what "xmlns" aliases have been defined in the document.
;; To work with such documents, we introduce a translation layer between a
;; "well known" namespace tag and the local namespace tag in the document
;; being parsed.

(defconst soap-well-known-xmlns
  '(("apachesoap" . "http://xml.apache.org/xml-soap")
    ("soapenc" . "http://schemas.xmlsoap.org/soap/encoding/")
    ("wsdl" . "http://schemas.xmlsoap.org/wsdl/")
    ("wsdlsoap" . "http://schemas.xmlsoap.org/wsdl/soap/")
    ("xsd" . "http://www.w3.org/2001/XMLSchema")
    ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")
    ("wsa" . "http://www.w3.org/2005/08/addressing")
    ("wsaw" . "http://www.w3.org/2006/05/addressing/wsdl")
    ("soap" . "http://schemas.xmlsoap.org/soap/envelope/")
    ("soap12" . "http://schemas.xmlsoap.org/wsdl/soap12/")
    ("http" . "http://schemas.xmlsoap.org/wsdl/http/")
    ("mime" . "http://schemas.xmlsoap.org/wsdl/mime/")
    ("xml" . "http://www.w3.org/XML/1998/namespace"))
  "A list of well known xml namespaces and their aliases.")

(defvar soap-local-xmlns
  '(("xml" . "http://www.w3.org/XML/1998/namespace"))
  "A list of local namespace aliases.
This is a dynamically bound variable, controlled by
`soap-with-local-xmlns'.")

(defvar soap-default-xmlns nil
  "The default XML namespaces.
Names in this namespace will be unqualified.  This is a
dynamically bound variable, controlled by
`soap-with-local-xmlns'")

(defvar soap-target-xmlns nil
  "The target XML namespace.
New XSD elements will be defined in this namespace, unless they
are fully qualified for a different namespace.  This is a
dynamically bound variable, controlled by
`soap-with-local-xmlns'")

(defvar soap-current-wsdl nil
  "The current WSDL document used when decoding the SOAP response.
This is a dynamically bound variable.")

(defun soap-wk2l (well-known-name)
  "Return local variant of WELL-KNOWN-NAME.
This is done by looking up the namespace in the
`soap-well-known-xmlns' table and resolving the namespace to
the local name based on the current local translation table
`soap-local-xmlns'.  See also `soap-with-local-xmlns'."
  (let ((wk-name-1 (if (symbolp well-known-name)
                       (symbol-name well-known-name)
                     well-known-name)))
    (cond
     ((string-match "^\\(.*\\):\\(.*\\)$" wk-name-1)
      (let ((ns (match-string 1 wk-name-1))
            (name (match-string 2 wk-name-1)))
        (let ((namespace (cdr (assoc ns soap-well-known-xmlns))))
          (cond ((equal namespace soap-default-xmlns)
                 ;; Name is unqualified in the default namespace
                 (if (symbolp well-known-name)
                     (intern name)
                   name))
                (t
                 (let* ((local-ns (car (rassoc namespace soap-local-xmlns)))
                        (local-name (concat local-ns ":" name)))
                   (if (symbolp well-known-name)
                       (intern local-name)
                     local-name)))))))
     (t well-known-name))))

(defun soap-l2wk (local-name)
  "Convert LOCAL-NAME into a well known name.
The namespace of LOCAL-NAME is looked up in the
`soap-well-known-xmlns' table and a well known namespace tag is
used in the name.

nil is returned if there is no well-known namespace for the
namespace of LOCAL-NAME."
  (let ((l-name-1 (if (symbolp local-name)
                      (symbol-name local-name)
                    local-name))
        namespace name)
    (cond
     ((string-match "^\\(.*\\):\\(.*\\)$" l-name-1)
      (setq name (match-string 2 l-name-1))
      (let ((ns (match-string 1 l-name-1)))
        (setq namespace (cdr (assoc ns soap-local-xmlns)))
        (unless namespace
          (error "Soap-l2wk(%s): no namespace for alias %s" local-name ns))))
     (t
      (setq name l-name-1)
      (setq namespace soap-default-xmlns)))

    (if namespace
        (let ((well-known-ns (car (rassoc namespace soap-well-known-xmlns))))
          (if well-known-ns
              (let ((well-known-name (concat well-known-ns ":" name)))
                (if (symbolp local-name)
                    (intern well-known-name)
                  well-known-name))
            nil))
      ;; if no namespace is defined, just return the unqualified name
      name)))


(defun soap-l2fq (local-name &optional use-tns)
  "Convert LOCAL-NAME into a fully qualified name.
A fully qualified name is a cons of the namespace name and the
name of the element itself.  For example \"xsd:string\" is
converted to (\"http://www.w3.org/2001/XMLSchema\" . \"string\").

The USE-TNS argument specifies what to do when LOCAL-NAME has no
namespace tag.  If USE-TNS is non-nil, the `soap-target-xmlns'
will be used as the element's namespace, otherwise
`soap-default-xmlns' will be used.

This is needed because different parts of a WSDL document can use
different namespace aliases for the same element."
  (let ((local-name-1 (if (symbolp local-name)
                          (symbol-name local-name)
                        local-name)))
    (cond ((string-match "^\\(.*\\):\\(.*\\)$" local-name-1)
           (let ((ns (match-string 1 local-name-1))
                 (name (match-string 2 local-name-1)))
             (let ((namespace (cdr (assoc ns soap-local-xmlns))))
               (if namespace
                   (cons namespace name)
                 (error "Soap-l2fq(%s): unknown alias %s" local-name ns)))))
          (t
           (cons (if use-tns
                     soap-target-xmlns
                   soap-default-xmlns)
                 local-name-1)))))

(defun soap-name-p (name)
  "Return t if NAME is a valid name for XMLSchema types.
A valid name is either a string or a cons of (NAMESPACE . NAME)."
  (or (stringp name)
      (and (consp name)
           (stringp (car name))
           (stringp (cdr name)))))

(defun soap-extract-xmlns (node &optional xmlns-table)
  "Return a namespace alias table for NODE by extending XMLNS-TABLE."
  (let (xmlns default-ns target-ns)
    (dolist (a (xml-node-attributes node))
      (let ((name (symbol-name (car a)))
            (value (cdr a)))
        (cond ((string= name "targetNamespace")
               (setq target-ns value))
              ((string= name "xmlns")
               (setq default-ns value))
              ((string-match "^xmlns:\\(.*\\)$" name)
               (push (cons (match-string 1 name) value) xmlns)))))

    (let ((tns (assoc "tns" xmlns)))
      (cond ((and tns target-ns)
             ;; If a tns alias is defined for this node, it must match
             ;; the target namespace.
             (unless (equal target-ns (cdr tns))
               (soap-warning
                "soap-extract-xmlns(%s): tns alias and targetNamespace mismatch"
                (xml-node-name node))))
            ((and tns (not target-ns))
             (setq target-ns (cdr tns)))))

    (list default-ns target-ns (append xmlns xmlns-table))))

(defmacro soap-with-local-xmlns (node &rest body)
  "Install a local alias table from NODE and execute BODY."
  (declare (debug (form &rest form)) (indent 1))
  (let ((xmlns (make-symbol "xmlns")))
    `(let ((,xmlns (soap-extract-xmlns ,node soap-local-xmlns)))
       (let ((soap-default-xmlns (or (nth 0 ,xmlns) soap-default-xmlns))
             (soap-target-xmlns (or (nth 1 ,xmlns) soap-target-xmlns))
             (soap-local-xmlns (nth 2 ,xmlns)))
         ,@body))))

(defun soap-get-target-namespace (node)
  "Return the target namespace of NODE.
This is the namespace in which new elements will be defined."
  (or (xml-get-attribute-or-nil node 'targetNamespace)
      (cdr (assoc "tns"  soap-local-xmlns))
      soap-target-xmlns))

(defun soap-xml-get-children1 (node child-name)
  "Return the children of NODE named CHILD-NAME.
This is the same as `xml-get-children', but CHILD-NAME can have
namespace tag."
  (let (result)
    (dolist (c (xml-node-children node))
      (when (and (consp c)
                 (soap-with-local-xmlns c
                   ;; We use `ignore-errors' here because we want to silently
                   ;; skip nodes when we cannot convert them to a well-known
                   ;; name.
                   (eq (ignore-errors (soap-l2wk (xml-node-name c)))
                       child-name)))
        (push c result)))
    (nreverse result)))

(defun soap-xml-node-find-matching-child (node set)
  "Return the first child of NODE whose name is a member of SET."
  (catch 'found
    (dolist (child (xml-node-children node))
      (when (and (consp child)
                 (memq (soap-l2wk (xml-node-name child)) set))
        (throw 'found child)))))

(defun soap-xml-get-attribute-or-nil1 (node attribute)
  "Return the NODE's ATTRIBUTE, or nil if it does not exist.
This is the same as `xml-get-attribute-or-nil', but ATTRIBUTE can
be tagged with a namespace tag."
  (catch 'found
    (soap-with-local-xmlns node
      (dolist (a (xml-node-attributes node))
        ;; We use `ignore-errors' here because we want to silently skip
        ;; attributes for which we cannot convert them to a well-known name.
        (when (eq (ignore-errors (soap-l2wk (car a))) attribute)
          (throw 'found (cdr a)))))))


;;;; XML namespaces

;; An element in an XML namespace, "things" stored in soap-xml-namespaces will
;; be derived from this object.

(cl-defstruct soap-element
  name
  ;; The "well-known" namespace tag for the element.  For example, while
  ;; parsing XML documents, we can have different tags for the XMLSchema
  ;; namespace, but internally all our XMLSchema elements will have the "xsd"
  ;; tag.
  namespace-tag)

(defun soap-element-fq-name (element)
  "Return a fully qualified name for ELEMENT.
A fq name is the concatenation of the namespace tag and the
element name."
  (cond ((soap-element-namespace-tag element)
         (concat (soap-element-namespace-tag element)
                 ":" (soap-element-name element)))
        ((soap-element-name element)
         (soap-element-name element))
        (t
         "*unnamed*")))

;; a namespace link stores an alias for an object in once namespace to a
;; "target" object possibly in a different namespace

(cl-defstruct (soap-namespace-link (:include soap-element))
  target)

;; A namespace is a collection of soap-element objects under a name (the name
;; of the namespace).

(cl-defstruct soap-namespace
  (name nil :read-only t)               ; e.g "http://xml.apache.org/xml-soap"
  (elements (make-hash-table :test 'equal) :read-only t))

(defun soap-namespace-put (element ns)
  "Store ELEMENT in NS.
Multiple elements with the same name can be stored in a
namespace.  When retrieving the element you can specify a
discriminant predicate to `soap-namespace-get'."
  (let ((name (soap-element-name element)))
    (push element (gethash name (soap-namespace-elements ns)))))

(defun soap-namespace-put-link (name target ns)
  "Store a link from NAME to TARGET in NS.
TARGET can be either a SOAP-ELEMENT or a string denoting an
element name into another namespace.

If NAME is nil, an element with the same name as TARGET will be
added to the namespace."

  (unless (and name (not (equal name "")))
    ;; if name is nil, use TARGET as a name...
    (cond ((soap-element-p target)
           (setq name (soap-element-name target)))
          ((consp target)               ; a fq name: (namespace . name)
           (setq name (cdr target)))
          ((stringp target)
           (cond ((string-match "^\\(.*\\):\\(.*\\)$" target)
                  (setq name (match-string 2 target)))
                 (t
                  (setq name target))))))

  ;; by now, name should be valid
  (cl-assert (and name (not (equal name "")))
             nil
             "Cannot determine name for namespace link")
  (push (make-soap-namespace-link :name name :target target)
        (gethash name (soap-namespace-elements ns))))

(defun soap-namespace-get (name ns &optional discriminant-predicate)
  "Retrieve an element with NAME from the namespace NS.
If multiple elements with the same name exist,
DISCRIMINANT-PREDICATE is used to pick one of them.  This allows
storing elements of different types (like a message type and a
binding) but the same name."
  (cl-assert (stringp name))
  (let ((elements (gethash name (soap-namespace-elements ns))))
    (cond (discriminant-predicate
           (catch 'found
             (dolist (e elements)
               (when (funcall discriminant-predicate e)
                 (throw 'found e)))))
          ((= (length elements) 1) (car elements))
          ((> (length elements) 1)
           (error
            "Soap-namespace-get(%s): multiple elements, discriminant needed"
            name))
          (t
           nil))))


;;;; XML Schema

;; SOAP WSDL documents use XML Schema to define the types that are part of the
;; message exchange.  We include here an XML schema model with a parser and
;; serializer/deserializer.

(cl-defstruct (soap-xs-type (:include soap-element))
  id
  attributes
  attribute-groups)

;;;;; soap-xs-basic-type

(cl-defstruct (soap-xs-basic-type (:include soap-xs-type))
  ;; Basic types are "built in" and we know how to handle them directly.
  ;; Other type definitions reference basic types, so we need to create them
  ;; in a namespace (see `soap-make-xs-basic-types')

  ;; a symbol of: string, dateTime, long, int, etc
  kind
  )

(defun soap-make-xs-basic-types (namespace-name &optional namespace-tag)
  "Construct NAMESPACE-NAME containing the XMLSchema basic types.
An optional NAMESPACE-TAG can also be specified."
  (let ((ns (make-soap-namespace :name namespace-name)))
    (dolist (type '("string" "language" "ID" "IDREF"
                    "dateTime" "time" "date" "boolean"
                    "gYearMonth" "gYear" "gMonthDay" "gDay" "gMonth"
                    "long" "short" "int" "integer" "nonNegativeInteger"
                    "unsignedLong" "unsignedShort" "unsignedInt"
                    "decimal" "duration"
                    "byte" "unsignedByte"
                    "float" "double"
                    "base64Binary" "anyType" "anyURI" "QName" "Array" "byte[]"))
      (soap-namespace-put
       (make-soap-xs-basic-type :name type
                                :namespace-tag namespace-tag
                                :kind (intern type))
       ns))
    ns))

(defun soap-encode-xs-basic-type-attributes (value type)
  "Encode the XML attributes for VALUE according to TYPE.
The xsi:type and an optional xsi:nil attributes are added.  The
attributes are inserted in the current buffer at the current
position.

This is a specialization of `soap-encode-attributes' for
`soap-xs-basic-type' objects."
  (let ((xsi-type (soap-element-fq-name type))
        (basic-type (soap-xs-basic-type-kind type)))
    ;; try to classify the type based on the value type and use that type when
    ;; encoding
    (when (eq basic-type 'anyType)
      (cond ((stringp value)
             (setq xsi-type "xsd:string" basic-type 'string))
            ((integerp value)
             (setq xsi-type "xsd:int" basic-type 'int))
            ((memq value '(t nil))
             (setq xsi-type "xsd:boolean" basic-type 'boolean))
            (t
             (error "Cannot classify anyType value"))))

    (insert " xsi:type=\"" xsi-type "\"")
    ;; We have some ambiguity here, as a nil value represents "false" when the
    ;; type is boolean, we will never have a "nil" boolean type...
    (unless (or value (eq basic-type 'boolean))
      (insert " xsi:nil=\"true\""))))

(defun soap-encode-xs-basic-type (value type)
  "Encode the VALUE according to TYPE.
The data is inserted in the current buffer at the current
position.

This is a specialization of `soap-encode-value' for
`soap-xs-basic-type' objects."
  (let ((kind (soap-xs-basic-type-kind type))
        ;; Handle conversions of this form:
        ;; (Element (AttrA . "A") (AttrB . "B") "Value here")
        ;; to:
        ;; <ns:Element AttrA="A" AttrB="B">Value here</ns:Element>
        ;; by assuming that if this is a list, it must have attributes
        ;; preceding the basic value.
        (value (if (listp value) (progn (car (last value))) value)))
    (when (eq kind 'anyType)
      (cond ((stringp value)
             (setq kind 'string))
            ((integerp value)
             (setq kind 'int))
            ((memq value '(t nil))
             (setq kind 'boolean))
            (t
             (error "Cannot classify anyType value"))))

    ;; NOTE: a nil value is not encoded, as an xsi:nil="true" attribute was
    ;; encoded for it.  However, we have some ambiguity here, as a nil value
    ;; also represents "false" when the type is boolean...

    (when (or value (eq kind 'boolean))
      (let ((value-string
             (cl-case kind
               ((string anyURI QName ID IDREF language)
                (unless (stringp value)
                  (error "Not a string value: %s" value))
                (url-insert-entities-in-string value))
               ((dateTime time date gYearMonth gYear gMonthDay gDay gMonth)
                (cond ((consp value)
                       ;; Value is a (current-time) style value,
                       ;; convert to the ISO 8601-inspired XSD
                       ;; string format in UTC.
                       (format-time-string
                        (concat
                         (cl-ecase kind
                           (dateTime "%Y-%m-%dT%H:%M:%S")
                           (time "%H:%M:%S")
                           (date "%Y-%m-%d")
                           (gYearMonth "%Y-%m")
                           (gYear "%Y")
                           (gMonthDay "--%m-%d")
                           (gDay "---%d")
                           (gMonth "--%m"))
                         ;; Internal time is always in UTC.
                         "Z")
                        value t))
                      ((stringp value)
                       ;; Value is a string in the ISO 8601-inspired XSD
                       ;; format.  Validate it.
                       (soap-decode-date-time value kind)
                       (url-insert-entities-in-string value))
                      (t
                       (error "Invalid date-time format"))))
               (boolean
                (unless (memq value '(t nil))
                  (error "Not a boolean value"))
                (if value "true" "false"))

               ((long short int integer byte unsignedInt unsignedLong
                      unsignedShort nonNegativeInteger decimal duration)
                (unless (integerp value)
                  (error "Not an integer value"))
                (when (and (memq kind '(unsignedInt unsignedLong
                                                    unsignedShort
                                                    nonNegativeInteger))
                           (< value 0))
                  (error "Not a positive integer"))
                (number-to-string value))

               ((float double)
                (unless (numberp value)
                  (error "Not a number"))
                (number-to-string value))

               (base64Binary
                (unless (stringp value)
                  (error "Not a string value for base64Binary"))
                (base64-encode-string value))

               (otherwise
                (error "Don't know how to encode %s for type %s"
                       value (soap-element-fq-name type))))))
        (soap-validate-xs-basic-type value-string type)
        (insert value-string)))))

(defun soap-decode-date-time (date-time-string &optional datatype)
  "Decode DATE-TIME-STRING as DATATYPE.
DATE-TIME-STRING should be in ISO 8601 basic or extended format.
DATATYPE can be omitted, or one of the symbols dateTime, time,
date, gYearMonth, gYear, gMonthDay, gDay, or gMonth.  If Emacs is
a version that supports fractional seconds, DATATYPE can also be
dateTime-subsecond, or time-subsecond.  On older versions of
Emacs (prior to 27.1), which do not support fractional seconds,
leaving DATATYPE nil means that subseconds in DATE-TIME-STRING
will be ignored.

Return a list in a format identical or similar to that returned
by `decode-time'.  The returned format is always compatible with
`encode-time'.  If DATATYPE is omitted or nil, this function will
return a list that has exactly the same format as that returned
by `decode-time'.

Note that on versions of Emacs that predate support for
fractional seconds, `encode-time' will not notice the SUBSECOND
field so it must be handled specially.

The formats returned by this function are as follows, where _
means \"should be ignored\":

 DATATYPE   | Return format
------------+----------------------------------------------------------------
 nil        | (SECOND MINUTE HOUR DAY MONTH YEAR DOW       DST        UTCOFF)
 dateTime   | (SECOND MINUTE HOUR DAY MONTH YEAR SUBSECOND dateTime   UTCOFF)
 time       | (SECOND MINUTE HOUR _   _     _    SUBSECOND time       _)
 date       | (_      _      _    DAY MONTH YEAR _         date       _)
 gYearMonth | (_      _      _    _   MONTH YEAR _         gYearMonth _)
 gYear      | (_      _      _    _   _     YEAR _         gYear      _)
 gMonthDay  | (_      _      _    DAY MONTH _    _         gMonthDay  _)
 gDay       | (_      _      _    DAY _     _    _         gDay       _)
 gMonth     | (_      _      _    _   MONTH _    _         gMonth     _)

When DATATYPE is dateTime or time, the DOW (day-of-week) field is
replaced with SUBSECOND, a float representing the fractional
seconds, and the DST (daylight savings time) field is replaced
with DATATYPE, a symbol representing the XSD primitive datatype.
This symbol can be used to determine which fields apply and which
do not, when it is not already clear from context.  For example a
datatype of `time' means the year, month, day and time zone
fields should be ignored.

New code that depends on Emacs 27.1 or newer anyway, and that
wants dateTime or time but with the first argument with subsecond
resolution, i.e., (TICKS . HZ), can set DATATYPE to
dateTime-subsecond or time-subsecond respectively.  This function
throws an error if dateTime-subsecond or time-subsecond is
specified when Emacs does not support subsecond resolution.

This function throws an error if DATE-TIME-STRING represents a
leap second, since the XML Schema 1.1 standard does not support
representing leap seconds."
  (let* ((new-decode-time (condition-case nil
                              (not (null
                                    (with-no-warnings (decode-time nil nil t))))
                            (wrong-number-of-arguments)))
         (new-decode-time-second nil)
         (no-support "This Emacs version does not support %s")
         (datetime-regexp-type
          (cl-case datatype
            ((dateTime-subsecond time-subsecond)
             (if new-decode-time
                 (intern (replace-regexp-in-string
                          "-subsecond" "" (symbol-name datatype)))
               (error (format no-support (symbol-name datatype)))))
            ((nil) 'dateTime)
            (otherwise datatype)))
         (datetime-regexp (cadr (get datetime-regexp-type 'rng-xsd-convert)))
         (year-sign (progn
                      (string-match datetime-regexp date-time-string)
                      (match-string 1 date-time-string)))
         (year (match-string 2 date-time-string))
         (month (match-string 3 date-time-string))
         (day (match-string 4 date-time-string))
         (hour (match-string 5 date-time-string))
         (minute (match-string 6 date-time-string))
         (second (match-string 7 date-time-string))
         (second-fraction (match-string 8 date-time-string))
         (time-zone nil)
         (has-time-zone (match-string 9 date-time-string))
         (time-zone-sign (match-string 10 date-time-string))
         (time-zone-hour (match-string 11 date-time-string))
         (time-zone-minute (match-string 12 date-time-string)))
    (setq year-sign (if year-sign -1 1))
    (setq year
          (if year
              (* year-sign
                 (string-to-number year))
            ;; By defaulting to the epoch date, a time value can be treated as
            ;; a relative number of seconds.
            1970))
    (setq month
          (if month (string-to-number month) 1))
    (setq day
          (if day (string-to-number day) 1))
    (setq hour
          (if hour (string-to-number hour) 0))
    (setq minute
          (if minute (string-to-number minute) 0))
    (when new-decode-time
      (setq new-decode-time-second
            (if second
                (if second-fraction
                    (let* ((second-fraction-significand
                            (string-replace "." "" second-fraction))
                           (hertz
                            (expt 10 (length second-fraction-significand)))
                           (ticks (+ (* hertz (string-to-number second))
                                     (string-to-number
                                      second-fraction-significand))))
                      (cons ticks hertz))
                  (cons second 1)))))
    (setq second
          (if second (string-to-number second) 0))
    (setq second-fraction
          (if second-fraction
              (progn
                (when (and (not datatype) (not new-decode-time))
                  (message
                   "soap-decode-date-time: Discarding fractional seconds"))
                (float (string-to-number second-fraction)))
            0.0))
    (setq has-time-zone (and has-time-zone t))
    (setq time-zone-sign
          (if (equal time-zone-sign "-") -1 1))
    (setq time-zone-hour
          (if time-zone-hour (string-to-number time-zone-hour) 0))
    (setq time-zone-minute
          (if time-zone-minute (string-to-number time-zone-minute) 0))
    (setq time-zone (if has-time-zone
                        (* (rng-xsd-time-to-seconds
                            time-zone-hour
                            time-zone-minute
                            0)
                           time-zone-sign)
                      ;; UTC.
                      0))
    (unless (and
             ;; XSD does not allow year 0.
             (> year 0)
             (>= month 1) (<= month 12)
             (>= day 1) (<= day (rng-xsd-days-in-month year month))
             (>= hour 0) (<= hour 23)
             (>= minute 0) (<= minute 59)
             ;; 60 represents a leap second, but leap seconds are explicitly
             ;; disallowed by the XML Schema 1.1 specification.  This agrees
             ;; with typical Emacs installations, which don't count leap
             ;; seconds in time values.
             (>= second 0) (<= second 59)
             (>= time-zone-hour 0)
             (<= time-zone-hour 23)
             (>= time-zone-minute 0)
             (<= time-zone-minute 59))
      (error "Invalid or unsupported time: %s" date-time-string))
    ;; Return a value in a format identical or similar to that
    ;; returned by decode-time, and always suitable for (apply
    ;; #'encode-time ...).
    (if datatype
        (list (if (memq datatype '(dateTime-subsecond time-subsecond))
                  new-decode-time-second
                second)
              minute hour day month year second-fraction datatype time-zone)
      (let ((time
             (apply
              #'encode-time (list
                             (if new-decode-time new-decode-time-second second)
                             minute hour day month year nil nil time-zone))))
        (if new-decode-time
            (with-no-warnings (decode-time time nil t))
          (decode-time time))))))

(defun soap-decode-xs-basic-type (type node)
  "Use TYPE, a `soap-xs-basic-type', to decode the contents of NODE.
A LISP value is returned based on the contents of NODE and the
type-info stored in TYPE.

This is a specialization of `soap-decode-type' for
`soap-xs-basic-type' objects."
  (let ((contents (xml-node-children node))
        (kind (soap-xs-basic-type-kind type))
        (attributes (xml-node-attributes node))
        (validate-type type)
        (is-nil nil))

    (dolist (attribute attributes)
      (let ((attribute-type (soap-l2fq (car attribute)))
            (attribute-value (cdr attribute)))
        ;; xsi:type can override an element's expected type.
        (when (equal attribute-type (soap-l2fq "xsi:type"))
          (setq validate-type
                (soap-wsdl-get attribute-value soap-current-wsdl)))
        ;; xsi:nil can specify that an element is nil in which case we don't
        ;; validate it.
        (when (equal attribute-type (soap-l2fq "xsi:nil"))
          (setq is-nil (string= (downcase attribute-value) "true")))))

    (unless is-nil
      ;; For validation purposes, when xml-node-children returns nil, treat it
      ;; as the empty string.
      (soap-validate-xs-basic-type (car (or contents (list ""))) validate-type))

    (if (null contents)
        nil
      (cl-ecase kind
        ((string anyURI QName ID IDREF language) (car contents))
        ((dateTime time date gYearMonth gYear gMonthDay gDay gMonth)
         (car contents))
        ((long short int integer
               unsignedInt unsignedLong unsignedShort nonNegativeInteger
               decimal byte float double duration)
         (string-to-number (car contents)))
        (boolean (string= (downcase (car contents)) "true"))
        (base64Binary (base64-decode-string (car contents)))
        (anyType (soap-decode-any-type node))
        (Array (soap-decode-array node))))))

(defalias 'soap-type-of
  ;; FIXME: Once we drop support for Emacs<25, use generic functions
  ;; via `cl-defmethod' instead of our own ad-hoc version of it.
  (if (eq 'soap-xs-basic-type (type-of (make-soap-xs-basic-type)))
      ;; `type-of' in Emacs ≥ 26 already does what we need.
      #'type-of
    ;; For Emacs < 26, use our own function.
    (lambda (element)
      "Return the type of ELEMENT."
      (if (vectorp element)
          (aref element 0)            ;Assume this vector is actually a struct!
        ;; This should never happen.
        (type-of element)))))

;; Register methods for `soap-xs-basic-type'
(let ((tag (soap-type-of (make-soap-xs-basic-type))))
  (put tag 'soap-attribute-encoder #'soap-encode-xs-basic-type-attributes)
  (put tag 'soap-encoder #'soap-encode-xs-basic-type)
  (put tag 'soap-decoder #'soap-decode-xs-basic-type))

;;;;; soap-xs-element

(cl-defstruct (soap-xs-element (:include soap-element))
  ;; NOTE: we don't support exact number of occurrences via minOccurs,
  ;; maxOccurs.  Instead we support optional? and multiple?

  id
  type^               ; note: use soap-xs-element-type to retrieve this member
  optional?
  multiple?
  reference
  substitution-group
  ;; contains a list of elements who point to this one via their
  ;; substitution-group slot
  alternatives
  is-group)

(defun soap-xs-element-type (element)
  "Retrieve the type of ELEMENT.
This is normally stored in the TYPE^ slot, but if this element
contains a reference, retrieve the type of the reference."
  (if (soap-xs-element-reference element)
      (soap-xs-element-type (soap-xs-element-reference element))
    (soap-xs-element-type^ element)))

(defun soap-node-optional (node)
  "Return t if NODE specifies an optional element."
  (or (equal (xml-get-attribute-or-nil node 'nillable) "true")
      (let ((e (xml-get-attribute-or-nil node 'minOccurs)))
        (and e (equal e "0")))))

(defun soap-node-multiple (node)
  "Return t if NODE permits multiple elements."
  (let* ((e (xml-get-attribute-or-nil node 'maxOccurs)))
    (and e (not (equal e "1")))))

(defun soap-xs-parse-element (node)
  "Construct a `soap-xs-element' from NODE."
  (let ((name (xml-get-attribute-or-nil node 'name))
        (id (xml-get-attribute-or-nil node 'id))
        (type (xml-get-attribute-or-nil node 'type))
        (optional? (soap-node-optional node))
        (multiple? (soap-node-multiple node))
        (ref (xml-get-attribute-or-nil node 'ref))
        (substitution-group (xml-get-attribute-or-nil node 'substitutionGroup))
        (node-name (soap-l2wk (xml-node-name node))))
    (cl-assert (memq node-name '(xsd:element xsd:group))
               "expecting xsd:element or xsd:group, got %s" node-name)

    (when type
      (setq type (soap-l2fq type 'tns)))

    (when ref
      (setq ref (soap-l2fq ref 'tns)))

    (when substitution-group
      (setq substitution-group (soap-l2fq substitution-group 'tns)))

    (unless (or ref type)
      ;; no type specified and this is not a reference.  Must be a type
      ;; defined within this node.
      (let ((simple-type (soap-xml-get-children1 node 'xsd:simpleType)))
        (if simple-type
            (setq type (soap-xs-parse-simple-type (car simple-type)))
          ;; else
          (let ((complex-type (soap-xml-get-children1 node 'xsd:complexType)))
            (if complex-type
                (setq type (soap-xs-parse-complex-type (car complex-type)))
              ;; else
              (error "soap-xs-parse-element: Missing type or ref"))))))

    (make-soap-xs-element :name name
                          ;; Use the full namespace name for now, we will
                          ;; convert it to a nstag in
                          ;; `soap-resolve-references-for-xs-element'
                          :namespace-tag soap-target-xmlns
                          :id id :type^ type
                          :optional? optional? :multiple? multiple?
                          :reference ref
                          :substitution-group substitution-group
                          :is-group (eq node-name 'xsd:group))))

(defun soap-resolve-references-for-xs-element (element wsdl)
  "Replace names in ELEMENT with the referenced objects in the WSDL.
This is a specialization of `soap-resolve-references' for
`soap-xs-element' objects.

See also `soap-wsdl-resolve-references'."

  (let ((namespace (soap-element-namespace-tag element)))
    (when namespace
      (let ((nstag (car (rassoc namespace (soap-wsdl-alias-table wsdl)))))
        (when nstag
          (setf (soap-element-namespace-tag element) nstag)))))

  (let ((type (soap-xs-element-type^ element)))
    (cond ((soap-name-p type)
           (setf (soap-xs-element-type^ element)
                 (soap-wsdl-get type wsdl 'soap-xs-type-p)))
          ((soap-xs-type-p type)
           ;; an inline defined type, this will not be reached from anywhere
           ;; else, so we must resolve references now.
           (soap-resolve-references type wsdl))))
  (let ((reference (soap-xs-element-reference element)))
    (when (and (soap-name-p reference)
               ;; xsd:group reference nodes will be converted to inline types
               ;; by soap-resolve-references-for-xs-complex-type, so skip them
               ;; here.
               (not (soap-xs-element-is-group element)))
      (setf (soap-xs-element-reference element)
            (soap-wsdl-get reference wsdl 'soap-xs-element-p))))

  (let ((subst (soap-xs-element-substitution-group element)))
    (when (soap-name-p subst)
      (let ((target (soap-wsdl-get subst wsdl)))
        (if target
            (push element (soap-xs-element-alternatives target))
          (soap-warning "No target found for substitution-group" subst))))))

(defun soap-encode-xs-element-attributes (value element)
  "Encode the XML attributes for VALUE according to ELEMENT.
Currently no attributes are needed.

This is a specialization of `soap-encode-attributes' for
`soap-xs-basic-type' objects."
  ;; Use the variables to suppress checkdoc and compiler warnings.
  (list value element)
  nil)

(defun soap-should-encode-value-for-xs-element (value element)
  "Return t if VALUE should be encoded for ELEMENT, nil otherwise."
  (cond
   ;; if value is not nil, attempt to encode it
   (value t)

   ;; value is nil, but the element's type is a boolean, so nil in this case
   ;; means "false".  We need to encode it.
   ((let ((type (soap-xs-element-type element)))
      (and (soap-xs-basic-type-p type)
           (eq (soap-xs-basic-type-kind type) 'boolean))))

   ;; This is not an optional element.  Force encoding it (although this
   ;; might fail at the validation step, but this is what we intend.

   ;; value is nil, but the element's type has some attributes which supply a
   ;; default value.  We need to encode it.

   ((let ((type (soap-xs-element-type element)))
      (catch 'found
        (dolist (a (soap-xs-type-attributes type))
          (when (soap-xs-attribute-default a)
            (throw 'found t))))))

   ;; otherwise, we don't need to encode it
   (t nil)))

(defun soap-type-is-array? (type)
  "Return t if TYPE defines an ARRAY."
  (and (soap-xs-complex-type-p type)
       (eq (soap-xs-complex-type-indicator type) 'array)))

(defvar soap-encoded-namespaces nil
  "A list of namespace tags used during encoding a message.
This list is populated by `soap-encode-value' and used by
`soap-create-envelope' to add aliases for these namespace to the
XML request.

This variable is dynamically bound in `soap-create-envelope'.")

(defun soap-encode-xs-element (value element)
  "Encode the VALUE according to ELEMENT.
The data is inserted in the current buffer at the current
position.

This is a specialization of `soap-encode-value' for
`soap-xs-basic-type' objects."
  (let ((fq-name (soap-element-fq-name element))
        (type (soap-xs-element-type element)))
    ;; Only encode the element if it has a name.  NOTE: soap-element-fq-name
    ;; will return *unnamed* for such elements
    (if (soap-element-name element)
        ;; Don't encode this element if value is nil.  However, even if value
        ;; is nil we still want to encode this element if it has any attributes
        ;; with default values.
        (when (soap-should-encode-value-for-xs-element value element)
          (progn
            (insert "<" fq-name)
            (soap-encode-attributes value type)
            ;; If value is nil and type is boolean encode the value as "false".
            ;; Otherwise don't encode the value.
            (if (or value (and (soap-xs-basic-type-p type)
                               (eq (soap-xs-basic-type-kind type) 'boolean)))
                (progn (insert ">")
                       ;; ARRAY's need special treatment, as each element of
                       ;; the array is encoded with the same tag as the
                       ;; current element...
                       (if (soap-type-is-array? type)
                           (let ((new-element (copy-soap-xs-element element)))
                             (when (soap-element-namespace-tag type)
                               (add-to-list 'soap-encoded-namespaces
                                            (soap-element-namespace-tag type)))
                             (setf (soap-xs-element-type^ new-element)
                                   (soap-xs-complex-type-base type))
                             (cl-loop for i below (length value)
                                      do (soap-encode-xs-element
                                          (aref value i) new-element)))
                         (soap-encode-value value type))
                       (insert "</" fq-name ">\n"))
              ;; else
              (insert "/>\n"))))
      (when (soap-should-encode-value-for-xs-element value element)
        (soap-encode-value value type)))))

(defun soap-decode-xs-element (element node)
  "Use ELEMENT, a `soap-xs-element', to decode the contents of NODE.
A LISP value is returned based on the contents of NODE and the
type-info stored in ELEMENT.

This is a specialization of `soap-decode-type' for
`soap-xs-basic-type' objects."
  (let ((type (soap-xs-element-type element)))
    (soap-decode-type type node)))

;; Register methods for `soap-xs-element'
(let ((tag (soap-type-of (make-soap-xs-element))))
  (put tag 'soap-resolve-references #'soap-resolve-references-for-xs-element)
  (put tag 'soap-attribute-encoder #'soap-encode-xs-element-attributes)
  (put tag 'soap-encoder #'soap-encode-xs-element)
  (put tag 'soap-decoder #'soap-decode-xs-element))

;;;;; soap-xs-attribute

(cl-defstruct (soap-xs-attribute (:include soap-element))
  type                                  ; a simple type or basic type
  default                               ; the default value, if any
  reference)

(cl-defstruct (soap-xs-attribute-group (:include soap-xs-type))
  reference)

(defun soap-xs-parse-attribute (node)
  "Construct a `soap-xs-attribute' from NODE."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'xsd:attribute)
             "expecting xsd:attribute, got %s" (soap-l2wk (xml-node-name node)))
  (let* ((name (xml-get-attribute-or-nil node 'name))
         (type (soap-l2fq (xml-get-attribute-or-nil node 'type)))
         (default (xml-get-attribute-or-nil node 'fixed))
         (attribute (xml-get-attribute-or-nil node 'ref))
         (ref (when attribute (soap-l2fq attribute))))
    (unless (or type ref)
      (setq type (soap-xs-parse-simple-type
                  (soap-xml-node-find-matching-child
                   node '(xsd:restriction xsd:list xsd:union)))))
    (make-soap-xs-attribute
     :name name :type type :default default :reference ref)))

(defun soap-xs-parse-attribute-group (node)
  "Construct a `soap-xs-attribute-group' from NODE."
  (let ((node-name (soap-l2wk (xml-node-name node))))
    (cl-assert (eq node-name 'xsd:attributeGroup)
               "expecting xsd:attributeGroup, got %s" node-name)
    (let ((name (xml-get-attribute-or-nil node 'name))
          (id (xml-get-attribute-or-nil node 'id))
          (ref (xml-get-attribute-or-nil node 'ref))
          attribute-group)
      (when (and name ref)
        (soap-warning "name and ref set for attribute group %s" node-name))
      (setq attribute-group
            (make-soap-xs-attribute-group :id id
                                          :name name
                                          :reference (and ref (soap-l2fq ref))))
      (when (not ref)
        (dolist (child (xml-node-children node))
          ;; Ignore whitespace.
          (unless (stringp child)
            ;; Ignore optional annotation.
            ;; Ignore anyAttribute nodes.
            (cl-case (soap-l2wk (xml-node-name child))
              (xsd:attribute
               (push (soap-xs-parse-attribute child)
                     (soap-xs-type-attributes attribute-group)))
              (xsd:attributeGroup
               (push (soap-xs-parse-attribute-group child)
                     (soap-xs-attribute-group-attribute-groups
                      attribute-group)))))))
      attribute-group)))

(defun soap-resolve-references-for-xs-attribute (attribute wsdl)
  "Replace names in ATTRIBUTE with the referenced objects in the WSDL.
This is a specialization of `soap-resolve-references' for
`soap-xs-attribute' objects.

See also `soap-wsdl-resolve-references'."
  (let* ((type (soap-xs-attribute-type attribute))
         (reference (soap-xs-attribute-reference attribute))
         (predicate 'soap-xs-element-p)
         (xml-reference
          (and (soap-name-p reference)
               (equal (car reference) "http://www.w3.org/XML/1998/namespace"))))
    (cond (xml-reference
           ;; Convert references to attributes defined by the XML
           ;; schema (xml:base, xml:lang, xml:space and xml:id) to
           ;; xsd:string, to avoid needing to bundle and parse
           ;; xml.xsd.
           (setq reference '("http://www.w3.org/2001/XMLSchema" . "string"))
           (setq predicate 'soap-xs-basic-type-p))
          ((soap-name-p type)
           (setf (soap-xs-attribute-type attribute)
                 (soap-wsdl-get type wsdl
                                (lambda (type)
                                  (or (soap-xs-basic-type-p type)
                                      (soap-xs-simple-type-p type))))))
          ((soap-xs-type-p type)
           ;; an inline defined type, this will not be reached from anywhere
           ;; else, so we must resolve references now.
           (soap-resolve-references type wsdl)))
    (when (soap-name-p reference)
      (setf (soap-xs-attribute-reference attribute)
            (soap-wsdl-get reference wsdl predicate)))))

(put (soap-type-of (make-soap-xs-attribute))
     'soap-resolve-references #'soap-resolve-references-for-xs-attribute)

(defun soap-resolve-references-for-xs-attribute-group (attribute-group wsdl)
  "Set slots in ATTRIBUTE-GROUP to the referenced objects in the WSDL.
This is a specialization of `soap-resolve-references' for
`soap-xs-attribute-group' objects.

See also `soap-wsdl-resolve-references'."
  (let ((reference (soap-xs-attribute-group-reference attribute-group)))
    (when (soap-name-p reference)
      (let ((resolved (soap-wsdl-get reference wsdl
                                     'soap-xs-attribute-group-p)))
        (dolist (attribute (soap-xs-attribute-group-attributes resolved))
          (soap-resolve-references attribute wsdl))
        (setf (soap-xs-attribute-group-name attribute-group)
              (soap-xs-attribute-group-name resolved))
        (setf (soap-xs-attribute-group-id attribute-group)
              (soap-xs-attribute-group-id resolved))
        (setf (soap-xs-attribute-group-reference attribute-group) nil)
        (setf (soap-xs-attribute-group-attributes attribute-group)
              (soap-xs-attribute-group-attributes resolved))
        (setf (soap-xs-attribute-group-attribute-groups attribute-group)
              (soap-xs-attribute-group-attribute-groups resolved))))))

(put (soap-type-of (make-soap-xs-attribute-group))
     'soap-resolve-references #'soap-resolve-references-for-xs-attribute-group)

;;;;; soap-xs-simple-type

(cl-defstruct (soap-xs-simple-type (:include soap-xs-type))
  ;; A simple type is an extension on the basic type to which some
  ;; restrictions can be added.  For example we can define a simple type based
  ;; off "string" with the restrictions that only the strings "one", "two" and
  ;; "three" are valid values (this is an enumeration).

  base              ; can be a single type, or a list of types for union types
  enumeration       ; nil, or list of permitted values for the type
  pattern           ; nil, or value must match this pattern
  length-range      ; a cons of (min . max) length, inclusive range.
                                        ; For exact length, use (l, l).
                                        ; nil means no range,
                                        ; (nil . l) means no min range,
                                        ; (l . nil) means no max range.
  integer-range     ; a pair of (min, max) integer values, inclusive range,
                                        ; same meaning as `length-range'
  is-list           ; t if this is an xs:list, nil otherwise
  )

(defun soap-xs-parse-simple-type (node)
  "Construct an `soap-xs-simple-type' object from the XML NODE."
  (cl-assert (memq (soap-l2wk (xml-node-name node))
                   '(xsd:simpleType xsd:simpleContent))
             nil
             "expecting xsd:simpleType or xsd:simpleContent node, got %s"
             (soap-l2wk (xml-node-name node)))

  ;; NOTE: name can be nil for inline types.  Such types cannot be added to a
  ;; namespace.
  (let ((name (xml-get-attribute-or-nil node 'name))
        (id (xml-get-attribute-or-nil node 'id)))

    (let ((type (make-soap-xs-simple-type
                 :name name :namespace-tag soap-target-xmlns :id id))
          (def (soap-xml-node-find-matching-child
                node '(xsd:restriction xsd:extension xsd:union xsd:list))))
      (cl-ecase (soap-l2wk (xml-node-name def))
        (xsd:restriction (soap-xs-add-restriction def type))
        (xsd:extension (soap-xs-add-extension def type))
        (xsd:union (soap-xs-add-union def type))
        (xsd:list (soap-xs-add-list def type)))

      type)))

(defun soap-xs-add-restriction (node type)
  "Add restrictions defined in XML NODE to TYPE, an `soap-xs-simple-type'."

  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'xsd:restriction)
             nil
             "expecting xsd:restriction node, got %s"
             (soap-l2wk (xml-node-name node)))

  (setf (soap-xs-simple-type-base type)
        (soap-l2fq (xml-get-attribute node 'base)))

  (dolist (r (xml-node-children node))
    (unless (stringp r)                 ; skip the white space
      (let ((value (xml-get-attribute r 'value)))
        (cl-case (soap-l2wk (xml-node-name r))
          (xsd:enumeration
           (push value (soap-xs-simple-type-enumeration type)))
          (xsd:pattern
           (setf (soap-xs-simple-type-pattern type)
                 (concat "\\`" (xsdre-translate value) "\\'")))
          (xsd:length
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-length-range type)
                   (cons value value))))
          (xsd:minLength
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-length-range type)
                   (if (soap-xs-simple-type-length-range type)
                       (cons value
                             (cdr (soap-xs-simple-type-length-range type)))
                     ;; else
                     (cons value nil)))))
          (xsd:maxLength
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-length-range type)
                   (if (soap-xs-simple-type-length-range type)
                       (cons (car (soap-xs-simple-type-length-range type))
                             value)
                     ;; else
                     (cons nil value)))))
          (xsd:minExclusive
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-integer-range type)
                   (if (soap-xs-simple-type-integer-range type)
                       (cons (1+ value)
                             (cdr (soap-xs-simple-type-integer-range type)))
                     ;; else
                     (cons (1+ value) nil)))))
          (xsd:maxExclusive
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-integer-range type)
                   (if (soap-xs-simple-type-integer-range type)
                       (cons (car (soap-xs-simple-type-integer-range type))
                             (1- value))
                     ;; else
                     (cons nil (1- value))))))
          (xsd:minInclusive
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-integer-range type)
                   (if (soap-xs-simple-type-integer-range type)
                       (cons value
                             (cdr (soap-xs-simple-type-integer-range type)))
                     ;; else
                     (cons value nil)))))
          (xsd:maxInclusive
           (let ((value (string-to-number value)))
             (setf (soap-xs-simple-type-integer-range type)
                   (if (soap-xs-simple-type-integer-range type)
                       (cons (car (soap-xs-simple-type-integer-range type))
                             value)
                     ;; else
                     (cons nil value))))))))))

(defun soap-xs-add-union (node type)
  "Add union members defined in XML NODE to TYPE, an `soap-xs-simple-type'."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'xsd:union)
             nil
             "expecting xsd:union node, got %s"
             (soap-l2wk (xml-node-name node)))

  (setf (soap-xs-simple-type-base type)
        (mapcar #'soap-l2fq
                (split-string
                 (or (xml-get-attribute-or-nil node 'memberTypes) ""))))

  ;; Additional simple types can be defined inside the union node.  Add them
  ;; to the base list.  The "memberTypes" members will have to be resolved by
  ;; the "resolve-references" method, the inline types will not.
  (let (result)
    (dolist (simple-type (soap-xml-get-children1 node 'xsd:simpleType))
      (push (soap-xs-parse-simple-type simple-type) result))
    (setf (soap-xs-simple-type-base type)
          (append (soap-xs-simple-type-base type) (nreverse result)))))

(defun soap-xs-add-list (node type)
  "Add list defined in XML NODE to TYPE, a `soap-xs-simple-type'."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'xsd:list)
             nil
             "expecting xsd:list node, got %s" (soap-l2wk (xml-node-name node)))

  ;; A simple type can be defined inline inside the list node or referenced by
  ;; the itemType attribute, in which case it will be resolved by the
  ;; resolve-references method.
  (let* ((item-type (xml-get-attribute-or-nil node 'itemType))
         (children (soap-xml-get-children1 node 'xsd:simpleType)))
    (if item-type
        (if (= (length children) 0)
            (setf (soap-xs-simple-type-base type) (soap-l2fq item-type))
          (soap-warning
           "xsd:list node with itemType has more than zero children: %s"
           (soap-xs-type-name type)))
      (if (= (length children) 1)
          (setf (soap-xs-simple-type-base type)
                (soap-xs-parse-simple-type
                 (car (soap-xml-get-children1 node 'xsd:simpleType))))
        (soap-warning "xsd:list node has more than one child %s"
                      (soap-xs-type-name type))))
    (setf (soap-xs-simple-type-is-list type) t)))

(defun soap-xs-add-extension (node type)
  "Add the extended type defined in XML NODE to TYPE, an `soap-xs-simple-type'."
  (setf (soap-xs-simple-type-base type)
        (soap-l2fq (xml-get-attribute node 'base)))
  (dolist (attribute (soap-xml-get-children1 node 'xsd:attribute))
    (push (soap-xs-parse-attribute attribute)
          (soap-xs-type-attributes type)))
  (dolist (attribute-group (soap-xml-get-children1 node 'xsd:attributeGroup))
    (push (soap-xs-parse-attribute-group attribute-group)
          (soap-xs-type-attribute-groups type))))

(defun soap-validate-xs-basic-type (value type)
  "Validate VALUE against the basic type TYPE."
  (let* ((kind (soap-xs-basic-type-kind type)))
    (cl-case kind
      ((anyType Array byte[])
       value)
      (t
       (let ((convert (get kind 'rng-xsd-convert)))
         (if convert
             (if (rng-dt-make-value convert value)
                 value
               (error "Invalid %s: %s" (symbol-name kind) value))
           (error "Don't know how to convert %s" kind)))))))

(defun soap-validate-xs-simple-type (value type)
  "Validate VALUE against the restrictions of TYPE."

  (let* ((base-type (soap-xs-simple-type-base type))
         (messages nil))
    (if (listp base-type)
        (catch 'valid
          (dolist (base base-type)
            (condition-case error-object
                (cond ((soap-xs-simple-type-p base)
                       (throw 'valid
                              (soap-validate-xs-simple-type value base)))
                      ((soap-xs-basic-type-p base)
                       (throw 'valid
                              (soap-validate-xs-basic-type value base))))
              (error (push (cadr error-object) messages))))
          (when messages
            (error (mapconcat #'identity (nreverse messages) "; and: "))))
      (cl-labels ((fail-with-message (format value)
                                     (push (format format value) messages)
                                     (throw 'invalid nil)))
        (catch 'invalid
          (let ((enumeration (soap-xs-simple-type-enumeration type)))
            (when (and (> (length enumeration) 1)
                       (not (member value enumeration)))
              (fail-with-message "bad value, should be one of %s" enumeration)))

          (let ((pattern (soap-xs-simple-type-pattern type)))
            (when (and pattern (not (string-match-p pattern value)))
              (fail-with-message "bad value, should match pattern %s" pattern)))

          (let ((length-range (soap-xs-simple-type-length-range type)))
            (when length-range
              (unless (stringp value)
                (fail-with-message
                 "bad value, should be a string with length range %s"
                 length-range))
              (when (car length-range)
                (unless (>= (length value) (car length-range))
                  (fail-with-message "short string, should be at least %s chars"
                                     (car length-range))))
              (when (cdr length-range)
                (unless (<= (length value) (cdr length-range))
                  (fail-with-message "long string, should be at most %s chars"
                                     (cdr length-range))))))

          (let ((integer-range (soap-xs-simple-type-integer-range type)))
            (when integer-range
              (unless (numberp value)
                (fail-with-message "bad value, should be a number with range %s"
                                   integer-range))
              (when (car integer-range)
                (unless (>= value (car integer-range))
                  (fail-with-message "small value, should be at least %s"
                                     (car integer-range))))
              (when (cdr integer-range)
                (unless (<= value (cdr integer-range))
                  (fail-with-message "big value, should be at most %s"
                                     (cdr integer-range))))))))
      (when messages
        (error "Xs-simple-type(%s, %s): %s"
               value (or (soap-xs-type-name type) (soap-xs-type-id type))
               (car messages)))))
  ;; Return the validated value.
  value)

(defun soap-resolve-references-for-xs-simple-type (type wsdl)
  "Replace names in TYPE with the referenced objects in the WSDL.
This is a specialization of `soap-resolve-references' for
`soap-xs-simple-type' objects.

See also `soap-wsdl-resolve-references'."

  (let ((namespace (soap-element-namespace-tag type)))
    (when namespace
      (let ((nstag (car (rassoc namespace (soap-wsdl-alias-table wsdl)))))
        (when nstag
          (setf (soap-element-namespace-tag type) nstag)))))

  (let ((base (soap-xs-simple-type-base type)))
    (cond
     ((soap-name-p base)
      (setf (soap-xs-simple-type-base type)
            (soap-wsdl-get base wsdl 'soap-xs-type-p)))
     ((soap-xs-type-p base)
      (soap-resolve-references base wsdl))
     ((listp base)
      (setf (soap-xs-simple-type-base type)
            (mapcar (lambda (type)
                      (cond ((soap-name-p type)
                             (soap-wsdl-get type wsdl 'soap-xs-type-p))
                            ((soap-xs-type-p type)
                             (soap-resolve-references type wsdl)
                             type)
                            (t     ; signal an error?
                             type)))
                    base)))
     (t (error "Oops"))))
  (dolist (attribute (soap-xs-type-attributes type))
    (soap-resolve-references attribute wsdl))
  (dolist (attribute-group (soap-xs-type-attribute-groups type))
    (soap-resolve-references attribute-group wsdl)))

(defun soap-encode-xs-simple-type-attributes (value type)
  "Encode the XML attributes for VALUE according to TYPE.
The attributes are inserted in the current buffer at the current
position.  If TYPE has no attributes, the xsi:type attribute and
an optional xsi:nil attribute are added.

This is a specialization of `soap-encode-attributes' for
`soap-xs-simple-type' objects."
  (let ((attributes (soap-get-xs-attributes type)))
    (dolist (a attributes)
      (let ((element-name (soap-element-name a)))
        (if (soap-xs-attribute-default a)
            (insert " " element-name
                    "=\"" (soap-xs-attribute-default a) "\"")
          (dolist (value-pair value)
            (when (equal element-name (symbol-name (car-safe value-pair)))
              (insert " " element-name
                      "=\"" (cdr value-pair) "\""))))))
    (unless attributes
      (insert " xsi:type=\"" (soap-element-fq-name type) "\"")
      (unless value (insert " xsi:nil=\"true\"")))))

(defun soap-encode-xs-simple-type (value type)
  "Encode the VALUE according to TYPE.
The data is inserted in the current buffer at the current
position.

This is a specialization of `soap-encode-value' for
`soap-xs-simple-type' objects."
  (soap-validate-xs-simple-type value type)
  (if (soap-xs-simple-type-is-list type)
      (progn
        (dolist (v (butlast value))
          (soap-encode-value v (soap-xs-simple-type-base type))
          (insert " "))
        (soap-encode-value (car (last value)) (soap-xs-simple-type-base type)))
    (soap-encode-value value (soap-xs-simple-type-base type))))

(defun soap-decode-xs-simple-type (type node)
  "Use TYPE, a `soap-xs-simple-type', to decode the contents of NODE.
A LISP value is returned based on the contents of NODE and the
type-info stored in TYPE.

This is a specialization of `soap-decode-type' for
`soap-xs-simple-type' objects."
  (if (soap-xs-simple-type-is-list type)
      ;; Technically, we could construct fake XML NODEs and pass them to
      ;; soap-decode-value...
      (split-string (car (xml-node-children node)))
    (let ((value (soap-decode-type (soap-xs-simple-type-base type) node)))
      (soap-validate-xs-simple-type value type))))

;; Register methods for `soap-xs-simple-type'
(let ((tag (soap-type-of (make-soap-xs-simple-type))))
  (put tag 'soap-resolve-references
       #'soap-resolve-references-for-xs-simple-type)
  (put tag 'soap-attribute-encoder #'soap-encode-xs-simple-type-attributes)
  (put tag 'soap-encoder #'soap-encode-xs-simple-type)
  (put tag 'soap-decoder #'soap-decode-xs-simple-type))

;;;;; soap-xs-complex-type

(cl-defstruct (soap-xs-complex-type (:include soap-xs-type))
  indicator                             ; sequence, choice, all, array
  base
  elements
  optional?
  multiple?
  is-group)

(defun soap-xs-parse-complex-type (node)
  "Construct a `soap-xs-complex-type' by parsing the XML NODE."
  (let ((name (xml-get-attribute-or-nil node 'name))
        (id (xml-get-attribute-or-nil node 'id))
        (node-name (soap-l2wk (xml-node-name node)))
        type
        attributes
        attribute-groups)
    (cl-assert (memq node-name '(xsd:complexType xsd:complexContent xsd:group))
               nil "unexpected node: %s" node-name)

    (dolist (def (xml-node-children node))
      (when (consp def)                 ; skip text nodes
        (cl-case (soap-l2wk (xml-node-name def))
          (xsd:attribute (push (soap-xs-parse-attribute def) attributes))
          (xsd:attributeGroup
           (push (soap-xs-parse-attribute-group def)
                 attribute-groups))
          (xsd:simpleContent (setq type (soap-xs-parse-simple-type def)))
          ((xsd:sequence xsd:all xsd:choice)
           (setq type (soap-xs-parse-sequence def)))
          (xsd:complexContent
           (dolist (def (xml-node-children def))
             (when (consp def)
               (cl-case (soap-l2wk (xml-node-name def))
                 (xsd:attribute
                  (push (soap-xs-parse-attribute def) attributes))
                 (xsd:attributeGroup
                  (push (soap-xs-parse-attribute-group def)
                        attribute-groups))
                 ((xsd:extension xsd:restriction)
                  (setq type
                        (soap-xs-parse-extension-or-restriction def)))
                 ((xsd:sequence xsd:all xsd:choice)
                  (soap-xs-parse-sequence def)))))))))
    (unless type
      ;; the type has not been built, this is a shortcut for a simpleContent
      ;; node
      (setq type (make-soap-xs-complex-type)))

    (setf (soap-xs-type-name type) name)
    (setf (soap-xs-type-namespace-tag type) soap-target-xmlns)
    (setf (soap-xs-type-id type) id)
    (setf (soap-xs-type-attributes type)
          (append attributes (soap-xs-type-attributes type)))
    (setf (soap-xs-type-attribute-groups type)
          (append attribute-groups (soap-xs-type-attribute-groups type)))
    (when (soap-xs-complex-type-p type)
      (setf (soap-xs-complex-type-is-group type)
            (eq node-name 'xsd:group)))
    type))

(defun soap-xs-parse-sequence (node)
  "Parse a sequence definition from XML NODE.
Returns a `soap-xs-complex-type'."
  (cl-assert (memq (soap-l2wk (xml-node-name node))
                   '(xsd:sequence xsd:choice xsd:all))
             nil
             "unexpected node: %s" (soap-l2wk (xml-node-name node)))

  (let ((type (make-soap-xs-complex-type)))

    (setf (soap-xs-complex-type-indicator type)
          (cl-ecase (soap-l2wk (xml-node-name node))
            (xsd:sequence 'sequence)
            (xsd:all 'all)
            (xsd:choice 'choice)))

    (setf (soap-xs-complex-type-optional? type) (soap-node-optional node))
    (setf (soap-xs-complex-type-multiple? type) (soap-node-multiple node))

    (dolist (r (xml-node-children node))
      (unless (stringp r)                 ; skip the white space
        (cl-case (soap-l2wk (xml-node-name r))
          ((xsd:element xsd:group)
           (push (soap-xs-parse-element r)
                 (soap-xs-complex-type-elements type)))
          ((xsd:sequence xsd:choice xsd:all)
           ;; an inline sequence, choice or all node
           (let ((choice (soap-xs-parse-sequence r)))
             (push (make-soap-xs-element :name nil :type^ choice)
                   (soap-xs-complex-type-elements type))))
          (xsd:attribute
           (push (soap-xs-parse-attribute r)
                 (soap-xs-type-attributes type)))
          (xsd:attributeGroup
           (push (soap-xs-parse-attribute-group r)
                 (soap-xs-type-attribute-groups type))))))

    (setf (soap-xs-complex-type-elements type)
          (nreverse (soap-xs-complex-type-elements type)))

    type))

(defun soap-xs-parse-extension-or-restriction (node)
  "Parse an extension or restriction definition from XML NODE.
Return a `soap-xs-complex-type'."
  (cl-assert (memq (soap-l2wk (xml-node-name node))
                   '(xsd:extension xsd:restriction))
             nil
             "unexpected node: %s" (soap-l2wk (xml-node-name node)))
  (let (type
        attributes
        attribute-groups
        array?
        (base (xml-get-attribute-or-nil node 'base)))

    ;; Array declarations are recognized specially, it is unclear to me how
    ;; they could be treated generally...
    (setq array?
          (and (eq (soap-l2wk (xml-node-name node)) 'xsd:restriction)
               (equal base (soap-wk2l "soapenc:Array"))))

    (dolist (def (xml-node-children node))
      (when (consp def)                 ; skip text nodes
        (cl-case (soap-l2wk (xml-node-name def))
          ((xsd:sequence xsd:choice xsd:all)
           (setq type (soap-xs-parse-sequence def)))
          (xsd:attribute
           (if array?
               (let ((array-type
                      (soap-xml-get-attribute-or-nil1 def 'wsdl:arrayType)))
                 (when (and array-type
                            (string-match "^\\(.*\\)\\[\\]$" array-type))
                   ;; Override
                   (setq base (match-string 1 array-type))))
             ;; else
             (push (soap-xs-parse-attribute def) attributes)))
          (xsd:attributeGroup
           (push (soap-xs-parse-attribute-group def) attribute-groups)))))

    (unless type
      (setq type (make-soap-xs-complex-type))
      (when array?
        (setf (soap-xs-complex-type-indicator type) 'array)))

    (setf (soap-xs-complex-type-base type) (soap-l2fq base))
    (setf (soap-xs-complex-type-attributes type) attributes)
    (setf (soap-xs-complex-type-attribute-groups type) attribute-groups)
    type))

(defun soap-resolve-references-for-xs-complex-type (type wsdl)
  "Replace names in TYPE with the referenced objects in the WSDL.
This is a specialization of `soap-resolve-references' for
`soap-xs-complex-type' objects.

See also `soap-wsdl-resolve-references'."

  (let ((namespace (soap-element-namespace-tag type)))
    (when namespace
      (let ((nstag (car (rassoc namespace (soap-wsdl-alias-table wsdl)))))
        (when nstag
          (setf (soap-element-namespace-tag type) nstag)))))

  (let ((base (soap-xs-complex-type-base type)))
    (cond ((soap-name-p base)
           (setf (soap-xs-complex-type-base type)
                 (soap-wsdl-get base wsdl 'soap-xs-type-p)))
          ((soap-xs-type-p base)
           (soap-resolve-references base wsdl))))
  (let (all-elements)
    (dolist (element (soap-xs-complex-type-elements type))
      (if (soap-xs-element-is-group element)
          ;; This is an xsd:group element that references an xsd:group node,
          ;; which we treat as a complex type.  We replace the reference
          ;; element by inlining the elements of the referenced xsd:group
          ;; (complex type) node.
          (let ((type (soap-wsdl-get
                       (soap-xs-element-reference element)
                       wsdl (lambda (type)
                              (and
                               (soap-xs-complex-type-p type)
                               (soap-xs-complex-type-is-group type))))))
            (dolist (element (soap-xs-complex-type-elements type))
              (soap-resolve-references element wsdl)
              (push element all-elements)))
        ;; This is a non-xsd:group node so just add it directly.
        (soap-resolve-references element wsdl)
        (push element all-elements)))
    (setf (soap-xs-complex-type-elements type) (nreverse all-elements)))
  (dolist (attribute (soap-xs-type-attributes type))
    (soap-resolve-references attribute wsdl))
  (dolist (attribute-group (soap-xs-type-attribute-groups type))
    (soap-resolve-references attribute-group wsdl)))

(defun soap-encode-xs-complex-type-attributes (value type)
  "Encode the XML attributes for encoding VALUE according to TYPE.
The xsi:type and optional xsi:nil attributes are added, plus
additional attributes needed for arrays types, if applicable.  The
attributes are inserted in the current buffer at the current
position.

This is a specialization of `soap-encode-attributes' for
`soap-xs-complex-type' objects."
  (if (eq (soap-xs-complex-type-indicator type) 'array)
      (let ((element-type (soap-xs-complex-type-base type)))
        (insert " xsi:type=\"soapenc:Array\"")
        (insert " soapenc:arrayType=\""
                (soap-element-fq-name element-type)
                "[" (format "%s" (length value)) "]" "\""))
    ;; else
    (progn
      (dolist (a (soap-get-xs-attributes type))
        (let ((element-name (soap-element-name a)))
          (if (soap-xs-attribute-default a)
              (insert " " element-name
                      "=\"" (soap-xs-attribute-default a) "\"")
            (dolist (value-pair value)
              (when (equal element-name (symbol-name (car value-pair)))
                (insert " " element-name
                        "=\"" (cdr value-pair) "\""))))))
      ;; If this is not an empty type, and we have no value, mark it as nil
      (when (and (soap-xs-complex-type-indicator type) (null value))
        (insert " xsi:nil=\"true\"")))))

(defun soap-get-candidate-elements (element)
  "Return a list of elements that are compatible with ELEMENT.
The returned list includes ELEMENT's references and
alternatives."
  (let ((reference (soap-xs-element-reference element)))
    ;; If the element is a reference, append the reference and its
    ;; alternatives...
    (if reference
        (append (list reference)
                (soap-xs-element-alternatives reference))
      ;; ...otherwise append the element itself and its alternatives.
      (append (list element)
              (soap-xs-element-alternatives element)))))

(defun soap-encode-xs-complex-type (value type)
  "Encode the VALUE according to TYPE.
The data is inserted in the current buffer at the current
position.

This is a specialization of `soap-encode-value' for
`soap-xs-complex-type' objects."
  (cl-case (soap-xs-complex-type-indicator type)
    (array
     (error "Arrays of type soap-encode-xs-complex-type are handled elsewhere"))
    ((sequence choice all nil)
     (let ((type-list (list type))
           (type-elements '()))

       ;; Collect all base types
       (let ((base (soap-xs-complex-type-base type)))
         (while base
           (push base type-list)
           (setq base (soap-xs-complex-type-base base))))

       ;; Collect type elements, eliminating duplicates from the type
       ;; hierarchy.
       (dolist (type type-list)
         (dolist (element (soap-xs-complex-type-elements type))
           (unless (member element type-elements)
             (setq type-elements (append type-elements (list element))))))

       (dolist (element type-elements)
         (catch 'done
           (let ((instance-count 0))
             (dolist (candidate (soap-get-candidate-elements element))
               (let ((e-name (soap-xs-element-name candidate)))
                 (if e-name
                     (let ((e-name (intern e-name)))
                       (dolist (v value)
                         (when (equal (car v) e-name)
                           (cl-incf instance-count)
                           (soap-encode-value (cdr v) candidate))))
                   (if (soap-xs-complex-type-indicator type)
                       (let ((current-point (point)))
                         ;; Check if encoding happened by checking if
                         ;; characters were inserted in the buffer.
                         (soap-encode-value value candidate)
                         (when (not (equal current-point (point)))
                           (cl-incf instance-count)))
                     (dolist (v value)
                       (let ((current-point (point)))
                         (soap-encode-value v candidate)
                         (when (not (equal current-point (point)))
                           (cl-incf instance-count))))))))
             ;; Do some sanity checking
             (let* ((indicator (soap-xs-complex-type-indicator type))
                    (element-type (soap-xs-element-type element))
                    (reference (soap-xs-element-reference element))
                    (e-name (or (soap-xs-element-name element)
                                (and reference
                                     (soap-xs-element-name reference)))))
               (cond ((and (eq indicator 'choice)
                           (> instance-count 0))
                      ;; This was a choice node and we encoded
                      ;; one instance.
                      (throw 'done t))
                     ((and (not (eq indicator 'choice))
                           (= instance-count 0)
                           (not (soap-xs-element-optional? element))
                           (not (soap-xs-complex-type-optional? type))
                           (and (soap-xs-complex-type-p element-type)
                                (not (soap-xs-complex-type-optional-p
                                      element-type))))
                      (soap-warning
                       "While encoding %s: missing non-nillable slot %s"
                       value e-name))
                     ((and (> instance-count 1)
                           (not (soap-xs-element-multiple? element))
                           (and (soap-xs-complex-type-p element-type)
                                (not (soap-xs-complex-type-multiple-p
                                      element-type))))
                      (soap-warning
                       (concat  "While encoding %s: expected single,"
                                " found multiple elements for slot %s")
                       value e-name)))))))))
    (t
     (error "Don't know how to encode complex type: %s"
            (soap-xs-complex-type-indicator type)))))

(defun soap-xml-get-children-fq (node child-name)
  "Return the children of NODE named CHILD-NAME.
This is the same as `xml-get-children1', but NODE's local
namespace is used to resolve the children's namespace tags."
  (let (result)
    (dolist (c (xml-node-children node))
      (when (and (consp c)
                 (soap-with-local-xmlns node
                   ;; We use `ignore-errors' here because we want to silently
                   ;; skip nodes for which we cannot convert them to a
                   ;; well-known name.
                   (equal (ignore-errors
                            (soap-l2fq (xml-node-name c)))
                          child-name)))
        (push c result)))
    (nreverse result)))

(defun soap-xs-element-get-fq-name (element wsdl)
  "Return ELEMENT's fully-qualified name using WSDL's alias table.
Return nil if ELEMENT does not have a name."
  (let* ((ns-aliases (soap-wsdl-alias-table wsdl))
         (ns-name (cdr (assoc
                        (soap-element-namespace-tag element)
                        ns-aliases))))
    (when ns-name
      (cons ns-name (soap-element-name element)))))

(defun soap-xs-complex-type-optional-p (type)
  "Return t if TYPE or any of TYPE's ancestor types is optional.
Return nil otherwise."
  (when type
    (or (soap-xs-complex-type-optional? type)
        (and (soap-xs-complex-type-p type)
             (soap-xs-complex-type-optional-p
              (soap-xs-complex-type-base type))))))

(defun soap-xs-complex-type-multiple-p (type)
  "Return t if TYPE or any of TYPE's ancestor types permits multiple elements.
Return nil otherwise."
  (when type
    (or (soap-xs-complex-type-multiple? type)
        (and (soap-xs-complex-type-p type)
             (soap-xs-complex-type-multiple-p
              (soap-xs-complex-type-base type))))))

(defun soap-get-xs-attributes-from-groups (attribute-groups)
  "Return a list of attributes from all ATTRIBUTE-GROUPS."
  (let (attributes)
    (dolist (group attribute-groups)
      (let ((sub-groups (soap-xs-attribute-group-attribute-groups group)))
        (setq attributes (append attributes
                                 (soap-get-xs-attributes-from-groups sub-groups)
                                 (soap-xs-attribute-group-attributes group)))))
    attributes))

(defun soap-get-xs-attributes (type)
  "Return a list of all of TYPE's and TYPE's ancestors' attributes."
  (let* ((base (and (soap-xs-complex-type-p type)
                    (soap-xs-complex-type-base type)))
         (attributes (append (soap-xs-type-attributes type)
                             (soap-get-xs-attributes-from-groups
                              (soap-xs-type-attribute-groups type)))))
    (if base
        (append attributes (soap-get-xs-attributes base))
      attributes)))

(defun soap-decode-xs-attributes (type node)
  "Use TYPE, a `soap-xs-complex-type', to decode the attributes of NODE."
  (let (result)
    (dolist (attribute (soap-get-xs-attributes type))
      (let* ((name (soap-xs-attribute-name attribute))
             (attribute-type (soap-xs-attribute-type attribute))
             (symbol (intern name))
             (value (xml-get-attribute-or-nil node symbol)))
        ;; We don't support attribute uses: required, optional, prohibited.
        (cond
         ((soap-xs-basic-type-p attribute-type)
          ;; Basic type values are validated by xml.el.
          (when value
            (push (cons symbol
                        ;; Create a fake XML node to satisfy the
                        ;; soap-decode-xs-basic-type API.
                        (soap-decode-xs-basic-type attribute-type
                                                   (list symbol nil value)))
                  result)))
         ((soap-xs-simple-type-p attribute-type)
          (when value
            (push (cons symbol
                        (soap-validate-xs-simple-type value attribute-type))
                  result)))
         (t
          (error (concat "Attribute %s is of type %s which is"
                         " not a basic or simple type")
                 name (soap-name-p attribute))))))
    result))

(defun soap-decode-xs-complex-type (type node)
  "Use TYPE, a `soap-xs-complex-type', to decode the contents of NODE.
A LISP value is returned based on the contents of NODE and the
type-info stored in TYPE.

This is a specialization of `soap-decode-type' for
`soap-xs-basic-type' objects."
  (cl-case (soap-xs-complex-type-indicator type)
    (array
     (let ((result nil)
           (element-type (soap-xs-complex-type-base type)))
       (dolist (node (xml-node-children node))
         (when (consp node)
           (push (soap-decode-type element-type node) result)))
       (nreverse result)))
    ((sequence choice all nil)
     (let ((result nil)
           (base (soap-xs-complex-type-base type)))
       (when base
         (setq result (nreverse (soap-decode-type base node))))
       (catch 'done
         (dolist (element (soap-xs-complex-type-elements type))
           (let* ((instance-count 0)
                  (e-name (soap-xs-element-name element))
                  ;; Heuristic: guess if we need to decode using local
                  ;; namespaces.
                  (use-fq-names (string-search ":" (symbol-name (car node))))
                  (children (if e-name
                                (if use-fq-names
                                    ;; Find relevant children
                                    ;; using local namespaces by
                                    ;; searching for the element's
                                    ;; fully-qualified name.
                                    (soap-xml-get-children-fq
                                     node
                                     (soap-xs-element-get-fq-name
                                      element soap-current-wsdl))
                                  ;; No local namespace resolution
                                  ;; needed so use the element's
                                  ;; name unqualified.
                                  (xml-get-children node (intern e-name)))
                              ;; e-name is nil so a) we don't know which
                              ;; children to operate on, and b) we want to
                              ;; re-use soap-decode-xs-complex-type, which
                              ;; expects a node argument with a complex
                              ;; type; therefore we need to operate on the
                              ;; entire node.  We wrap node in a list so
                              ;; that it will carry through as "node" in the
                              ;; loop below.
                              ;;
                              ;; For example:
                              ;;
                              ;; Element Type:
                              ;; <xs:complexType name="A">
                              ;;  <xs:sequence>
                              ;;   <xs:element name="B" type="t:BType"/>
                              ;;   <xs:choice>
                              ;;    <xs:element name="C" type="xs:string"/>
                              ;;    <xs:element name="D" type="t:DType"/>
                              ;;   </xs:choice>
                              ;;  </xs:sequence>
                              ;; </xs:complexType>
                              ;;
                              ;; Node:
                              ;; <t:A>
                              ;;   <t:B tag="b"/>
                              ;;   <t:C>1</C>
                              ;; </t:A>
                              ;;
                              ;; soap-decode-type will be called below with:
                              ;;
                              ;; element =
                              ;;   <xs:choice>
                              ;;     <xs:element name="C" type="xs:string"/>
                              ;;     <xs:element name="D" type="t:DType"/>
                              ;;   </xs:choice>
                              ;; node =
                              ;;   <t:A>
                              ;;     <t:B tag="b"/>
                              ;;     <t:C>1</C>
                              ;;   </t:A>
                              (list node)))
                  (element-type (soap-xs-element-type element)))
             (dolist (node children)
               (cl-incf instance-count)
               (let* ((attributes
                       (soap-decode-xs-attributes element-type node))
                      ;; Attributes may specify xsi:type override.
                      (element-type
                       (if (soap-xml-get-attribute-or-nil1 node 'xsi:type)
                           (soap-wsdl-get
                            (soap-l2fq
                             (soap-xml-get-attribute-or-nil1 node
                                                             'xsi:type))
                            soap-current-wsdl 'soap-xs-type-p t)
                         element-type))
                      (decoded-child (soap-decode-type element-type node)))
                 (if e-name
                     (push (cons (intern e-name)
                                 (append attributes decoded-child)) result)
                   ;; When e-name is nil we don't want to introduce an extra
                   ;; level of nesting, so we splice the decoding into
                   ;; result.
                   (setq result (append decoded-child result)))))
             (cond ((and (eq (soap-xs-complex-type-indicator type) 'choice)
                         ;; Choices can allow multiple values.
                         (not (soap-xs-complex-type-multiple-p type))
                         (> instance-count 0))
                    ;; This was a choice node, and we decoded one value.
                    (throw 'done t))

                   ;; Do some sanity checking
                   ((and (not (eq (soap-xs-complex-type-indicator type)
                                  'choice))
                         (= instance-count 0)
                         (not (soap-xs-element-optional? element))
                         (and (soap-xs-complex-type-p element-type)
                              (not (soap-xs-complex-type-optional-p
                                    element-type))))
                    (soap-warning "missing non-nillable slot %s" e-name))
                   ((and (> instance-count 1)
                         (not (soap-xs-complex-type-multiple-p type))
                         (not (soap-xs-element-multiple? element))
                         (and (soap-xs-complex-type-p element-type)
                              (not (soap-xs-complex-type-multiple-p
                                    element-type))))
                    (soap-warning "expected single %s slot, found multiple"
                                  e-name))))))
       (nreverse result)))
    (t
     (error "Don't know how to decode complex type: %s"
            (soap-xs-complex-type-indicator type)))))

;; Register methods for `soap-xs-complex-type'
(let ((tag (soap-type-of (make-soap-xs-complex-type))))
  (put tag 'soap-resolve-references
       #'soap-resolve-references-for-xs-complex-type)
  (put tag 'soap-attribute-encoder #'soap-encode-xs-complex-type-attributes)
  (put tag 'soap-encoder #'soap-encode-xs-complex-type)
  (put tag 'soap-decoder #'soap-decode-xs-complex-type))

;;;; WSDL documents
;;;;; WSDL document elements


(cl-defstruct (soap-message (:include soap-element))
  parts                                 ; ALIST of NAME => WSDL-TYPE name
  )

(cl-defstruct (soap-operation (:include soap-element))
  parameter-order
  input                                 ; (NAME . MESSAGE)
  output                                ; (NAME . MESSAGE)
  faults                                ; a list of (NAME . MESSAGE)
  input-action                          ; WS-addressing action string
  output-action)                        ; WS-addressing action string

(cl-defstruct (soap-port-type (:include soap-element))
  operations)                           ; a namespace of operations

;; A bound operation is an operation which has a soap action and a use
;; method attached -- these are attached as part of a binding and we
;; can have different bindings for the same operations.
(cl-defstruct soap-bound-operation
  operation                             ; SOAP-OPERATION
  soap-action                           ; value for SOAPAction HTTP header
  soap-headers                          ; list of (message part use)
  soap-body                             ;  message parts present in the body
  use                                   ; 'literal or 'encoded, see
                                        ; https://www.w3.org/TR/wsdl#_soap:body
  )

(cl-defstruct (soap-binding (:include soap-element))
  port-type
  (operations (make-hash-table :test 'equal) :readonly t))

(cl-defstruct (soap-port (:include soap-element))
  service-url
  binding)


;;;;; The WSDL document

;; The WSDL data structure used for encoding/decoding SOAP messages
(cl-defstruct (soap-wsdl
               ;; NOTE: don't call this constructor, see `soap-make-wsdl'
               (:constructor soap-make-wsdl^)
               (:copier soap-copy-wsdl))
  origin                         ; file or URL from which this wsdl was loaded
  current-file                   ; most-recently fetched file or URL
  xmlschema-imports              ; a list of schema imports
  ports                          ; a list of SOAP-PORT instances
  alias-table                    ; a list of namespace aliases
  namespaces                     ; a list of namespaces
  )

(defun soap-make-wsdl (origin)
  "Create a new WSDL document, loaded from ORIGIN, and initialize it."
  (let ((wsdl (soap-make-wsdl^ :origin origin)))

    ;; Add the XSD types to the wsdl document
    (let ((ns (soap-make-xs-basic-types
               ;; The following string is a name and not an URL, so
               ;; the "http:" should not be changed.
               "http://www.w3.org/2001/XMLSchema" "xsd")))
      (soap-wsdl-add-namespace ns wsdl)
      (soap-wsdl-add-alias "xsd" (soap-namespace-name ns) wsdl))

    ;; Add the soapenc types to the wsdl document
    (let ((ns (soap-make-xs-basic-types
               "http://schemas.xmlsoap.org/soap/encoding/" "soapenc")))
      (soap-wsdl-add-namespace ns wsdl)
      (soap-wsdl-add-alias "soapenc" (soap-namespace-name ns) wsdl))

    wsdl))

(defun soap-wsdl-add-alias (alias name wsdl)
  "Add a namespace ALIAS for NAME to the WSDL document."
  (let ((existing (assoc alias (soap-wsdl-alias-table wsdl))))
    (if existing
        (unless (equal (cdr existing) name)
          (warn "Redefining alias %s from %s to %s"
                alias (cdr existing) name)
          (push (cons alias name) (soap-wsdl-alias-table wsdl)))
      (push (cons alias name) (soap-wsdl-alias-table wsdl)))))

(defun soap-wsdl-find-namespace (name wsdl)
  "Find a namespace by NAME in the WSDL document."
  (catch 'found
    (dolist (ns (soap-wsdl-namespaces wsdl))
      (when (equal name (soap-namespace-name ns))
        (throw 'found ns)))))

(defun soap-wsdl-add-namespace (ns wsdl)
  "Add the namespace NS to the WSDL document.
If a namespace by this name already exists in WSDL, individual
elements will be added to it."
  (let ((existing (soap-wsdl-find-namespace (soap-namespace-name ns) wsdl)))
    (if existing
        ;; Add elements from NS to EXISTING, replacing existing values.
        (maphash (lambda (_key value)
                   (dolist (v value)
                     (soap-namespace-put v existing)))
                 (soap-namespace-elements ns))
      (push ns (soap-wsdl-namespaces wsdl)))))

(defun soap-wsdl-get (name wsdl &optional predicate use-local-alias-table)
  "Retrieve element NAME from the WSDL document.

PREDICATE is used to differentiate between elements when NAME
refers to multiple elements.  A typical value for this would be a
structure predicate for the type of element you want to retrieve.
For example, to retrieve a message named \"foo\" when other
elements named \"foo\" exist in the WSDL you could use:

  (soap-wsdl-get \"foo\" WSDL \\='soap-message-p)

If USE-LOCAL-ALIAS-TABLE is not nil, `soap-local-xmlns' will be
used to resolve the namespace alias."
  (let ((alias-table (soap-wsdl-alias-table wsdl))
        namespace element-name element)

    (when (symbolp name)
      (setq name (symbol-name name)))

    (when use-local-alias-table
      (setq alias-table (append soap-local-xmlns alias-table)))

    (cond ((consp name) ; a fully qualified name, as returned by `soap-l2fq'
           (setq element-name (cdr name))
           (when (symbolp element-name)
             (setq element-name (symbol-name element-name)))
           (setq namespace (soap-wsdl-find-namespace (car name) wsdl))
           (unless namespace
             (error "Soap-wsdl-get(%s): unknown namespace: %s" name namespace)))

          ((string-match "^\\(.*\\):\\(.*\\)$" name)
           (setq element-name (match-string 2 name))

           (let* ((ns-alias (match-string 1 name))
                  (ns-name (cdr (assoc ns-alias alias-table))))
             (unless ns-name
               (error "Soap-wsdl-get(%s): cannot find namespace alias %s"
                      name ns-alias))

             (setq namespace (soap-wsdl-find-namespace ns-name wsdl))
             (unless namespace
               (error
                "Soap-wsdl-get(%s): unknown namespace %s, referenced as %s"
                name ns-name ns-alias))))
          (t
           (error "Soap-wsdl-get(%s): bad name" name)))

    (setq element (soap-namespace-get
                   element-name namespace
                   (if predicate
                       (lambda (e)
                         (or (funcall 'soap-namespace-link-p e)
                             (funcall predicate e)))
                     nil)))

    (unless element
      (error "Soap-wsdl-get(%s): cannot find element" name))

    (if (soap-namespace-link-p element)
        ;; NOTE: don't use the local alias table here
        (soap-wsdl-get (soap-namespace-link-target element) wsdl predicate)
      element)))

;;;;; soap-parse-schema

(defun soap-parse-schema (node wsdl)
  "Parse a schema NODE, placing the results in WSDL.
Return a SOAP-NAMESPACE containing the elements."
  (soap-with-local-xmlns node
    (cl-assert (eq (soap-l2wk (xml-node-name node)) 'xsd:schema)
               nil
               "expecting an xsd:schema node, got %s"
               (soap-l2wk (xml-node-name node)))

    (let ((ns (make-soap-namespace :name (soap-get-target-namespace node))))

      (dolist (def (xml-node-children node))
        (unless (stringp def)           ; skip text nodes
          (cl-case (soap-l2wk (xml-node-name def))
            (xsd:import
             ;; Imports will be processed later
             ;; NOTE: we should expand the location now!
             (let ((location (or
                              (xml-get-attribute-or-nil def 'schemaLocation)
                              (xml-get-attribute-or-nil def 'location))))
               (when location
                 (push location (soap-wsdl-xmlschema-imports wsdl)))))
            (xsd:element
             (soap-namespace-put (soap-xs-parse-element def) ns))
            (xsd:attribute
             (soap-namespace-put (soap-xs-parse-attribute def) ns))
            (xsd:attributeGroup
             (soap-namespace-put (soap-xs-parse-attribute-group def) ns))
            (xsd:simpleType
             (soap-namespace-put (soap-xs-parse-simple-type def) ns))
            ((xsd:complexType xsd:group)
             (soap-namespace-put (soap-xs-parse-complex-type def) ns)))))
      ns)))

;;;;; Resolving references for wsdl types

;; See `soap-wsdl-resolve-references', which is the main entry point for
;; resolving references

(defun soap-resolve-references (element wsdl)
  "Replace names in ELEMENT with the referenced objects in the WSDL.
This is a generic function which invokes a specific resolver
function depending on the type of the ELEMENT.

If ELEMENT has no resolver function, it is silently ignored."
  (let ((resolver (get (soap-type-of element) 'soap-resolve-references)))
    (when resolver
      (funcall resolver element wsdl))))

(defun soap-resolve-references-for-message (message wsdl)
  "Replace names in MESSAGE with the referenced objects in the WSDL.
This is a generic function, called by `soap-resolve-references',
you should use that function instead.

See also `soap-wsdl-resolve-references'."
  (let (resolved-parts)
    (dolist (part (soap-message-parts message))
      (let ((name (car part))
            (element (cdr part)))
        (when (stringp name)
          (setq name (intern name)))
        (if (soap-name-p element)
            (setq element (soap-wsdl-get
                           element wsdl
                           (lambda (x)
                             (or (soap-xs-type-p x) (soap-xs-element-p x)))))
          ;; else, inline element, resolve recursively, as the element
          ;; won't be reached.
          (soap-resolve-references element wsdl)
          (unless (soap-element-namespace-tag element)
            (setf (soap-element-namespace-tag element)
                  (soap-element-namespace-tag message))))
        (push (cons name element) resolved-parts)))
    (setf (soap-message-parts message) (nreverse resolved-parts))))

(defun soap-resolve-references-for-operation (operation wsdl)
  "Resolve references for an OPERATION type using the WSDL document.
See also `soap-resolve-references' and
`soap-wsdl-resolve-references'"

  (let ((namespace (soap-element-namespace-tag operation)))
    (when namespace
      (let ((nstag (car (rassoc namespace (soap-wsdl-alias-table wsdl)))))
        (when nstag
          (setf (soap-element-namespace-tag operation) nstag)))))

  (let ((input (soap-operation-input operation))
        (counter 0))
    (let ((name (car input))
          (message (cdr input)))
      ;; Name this part if it was not named
      (when (or (null name) (equal name ""))
        (setq name (format "in%d" (cl-incf counter))))
      (when (soap-name-p message)
        (setf (soap-operation-input operation)
              (cons (intern name)
                    (soap-wsdl-get message wsdl 'soap-message-p))))))

  (let ((output (soap-operation-output operation))
        (counter 0))
    (let ((name (car output))
          (message (cdr output)))
      (when (or (null name) (equal name ""))
        (setq name (format "out%d" (cl-incf counter))))
      (when (soap-name-p message)
        (setf (soap-operation-output operation)
              (cons (intern name)
                    (soap-wsdl-get message wsdl 'soap-message-p))))))

  (let ((resolved-faults nil)
        (counter 0))
    (dolist (fault (soap-operation-faults operation))
      (let ((name (car fault))
            (message (cdr fault)))
        (when (or (null name) (equal name ""))
          (setq name (format "fault%d" (cl-incf counter))))
        (if (soap-name-p message)
            (push (cons (intern name)
                        (soap-wsdl-get message wsdl 'soap-message-p))
                  resolved-faults)
          (push fault resolved-faults))))
    (setf (soap-operation-faults operation) resolved-faults))

  (when (= (length (soap-operation-parameter-order operation)) 0)
    (setf (soap-operation-parameter-order operation)
          (mapcar #'car (soap-message-parts
                         (cdr (soap-operation-input operation))))))

  (setf (soap-operation-parameter-order operation)
        (mapcar (lambda (p)
                  (if (stringp p)
                      (intern p)
                    p))
                (soap-operation-parameter-order operation))))

(defun soap-resolve-references-for-binding (binding wsdl)
  "Resolve references for a BINDING type using the WSDL document.
See also `soap-resolve-references' and
`soap-wsdl-resolve-references'"
  (when (soap-name-p (soap-binding-port-type binding))
    (setf (soap-binding-port-type binding)
          (soap-wsdl-get (soap-binding-port-type binding)
                         wsdl 'soap-port-type-p)))

  (let ((port-ops (soap-port-type-operations (soap-binding-port-type binding))))
    (maphash (lambda (k v)
               (setf (soap-bound-operation-operation v)
                     (soap-namespace-get k port-ops 'soap-operation-p))
               (let (resolved-headers)
                 (dolist (h (soap-bound-operation-soap-headers v))
                   (push (list (soap-wsdl-get (nth 0 h) wsdl)
                               (intern (nth 1 h))
                               (nth 2 h))
                         resolved-headers))
                 (setf (soap-bound-operation-soap-headers v)
                       (nreverse resolved-headers))))
             (soap-binding-operations binding))))

(defun soap-resolve-references-for-port (port wsdl)
  "Replace names in PORT with the referenced objects in the WSDL.
This is a generic function, called by `soap-resolve-references',
you should use that function instead.

See also `soap-wsdl-resolve-references'."
  (when (soap-name-p (soap-port-binding port))
    (setf (soap-port-binding port)
          (soap-wsdl-get (soap-port-binding port) wsdl 'soap-binding-p))))

;; Install resolvers for our types
(progn
  (put (soap-type-of (make-soap-message)) 'soap-resolve-references
       #'soap-resolve-references-for-message)
  (put (soap-type-of (make-soap-operation)) 'soap-resolve-references
       #'soap-resolve-references-for-operation)
  (put (soap-type-of (make-soap-binding)) 'soap-resolve-references
       #'soap-resolve-references-for-binding)
  (put (soap-type-of (make-soap-port)) 'soap-resolve-references
       #'soap-resolve-references-for-port))

(defun soap-wsdl-resolve-references (wsdl)
  "Resolve all references inside the WSDL structure.

When the WSDL elements are created from the XML document, they
refer to each other by name.  For example, the ELEMENT-TYPE slot
of an SOAP-ARRAY-TYPE will contain the name of the element and
the user would have to call `soap-wsdl-get' to obtain the actual
element.

After the entire document is loaded, we resolve all these
references to the actual elements they refer to so that at
runtime, we don't have to call `soap-wsdl-get' each time we
traverse an element tree."
  (let ((nprocessed 0)
        (nstag-id 0)
        (alias-table (soap-wsdl-alias-table wsdl)))
    (dolist (ns (soap-wsdl-namespaces wsdl))
      (let ((nstag (car-safe (rassoc (soap-namespace-name ns) alias-table))))
        (unless nstag
          ;; If this namespace does not have an alias, create one for it.
          (catch 'done
            (while t
              (setq nstag (format "ns%d" (cl-incf nstag-id)))
              (unless (assoc nstag alias-table)
                (soap-wsdl-add-alias nstag (soap-namespace-name ns) wsdl)
                (throw 'done t)))))

        (maphash (lambda (_name element)
                   (cond ((soap-element-p element) ; skip links
                          (cl-incf nprocessed)
                          (soap-resolve-references element wsdl))
                         ((listp element)
                          (dolist (e element)
                            (when (soap-element-p e)
                              (cl-incf nprocessed)
                              (soap-resolve-references e wsdl))))))
                 (soap-namespace-elements ns)))))
  wsdl)

;;;;; Loading WSDL from XML documents

(defun soap-parse-server-response ()
  "Error-check and parse the XML contents of the current buffer."
  (let ((mime-part (mm-dissect-buffer t t)))
    (when (and
           (equal (mm-handle-media-type mime-part) "multipart/related")
           (equal (get-text-property 0 'type (mm-handle-media-type mime-part))
                  "text/xml"))
      (setq mime-part
            (mm-make-handle
             (get-text-property 0 'buffer (mm-handle-media-type mime-part))
             `(,(get-text-property 0 'type (mm-handle-media-type mime-part))))))
    (unless mime-part
      (error "Failed to decode response from server"))
    (unless (equal (car (mm-handle-type mime-part)) "text/xml")
      (error "Server response is not an XML document"))
    (with-temp-buffer
      (mm-insert-part mime-part)
      (prog1
          (car (xml-parse-region (point-min) (point-max)))
        (kill-buffer)
        (mm-destroy-part mime-part)))))

(defvar url-http-response-status)

(defun soap-fetch-xml-from-url (url wsdl)
  "Load an XML document from URL and return it.
The previously parsed URL is read from WSDL."
  (message "Fetching from %s" url)
  (let ((current-file (url-expand-file-name url (soap-wsdl-current-file wsdl)))
        (url-request-method "GET")
        (url-package-name "soap-client.el")
        (url-package-version "1.0")
        (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
        (url-http-attempt-keepalives t))
    (setf (soap-wsdl-current-file wsdl) current-file)
    (let ((buffer (url-retrieve-synchronously current-file)))
      (with-current-buffer buffer
        (if (> url-http-response-status 299)
            (error "Error retrieving WSDL: %s" url-http-response-status))
        (soap-parse-server-response)))))

(defun soap-fetch-xml-from-file (file wsdl)
  "Load an XML document from FILE and return it.
The previously parsed file is read from WSDL."
  (let* ((current-file (soap-wsdl-current-file wsdl))
         (expanded-file (expand-file-name file
                                          (if current-file
                                              (file-name-directory current-file)
                                            default-directory))))
    (setf (soap-wsdl-current-file wsdl) expanded-file)
    (with-temp-buffer
      (insert-file-contents expanded-file)
      (car (xml-parse-region (point-min) (point-max))))))

(defun soap-fetch-xml (file-or-url wsdl)
  "Load an XML document from FILE-OR-URL and return it.
The previously parsed file or URL is read from WSDL."
  (let ((current-file (or (soap-wsdl-current-file wsdl) file-or-url)))
    (if (or (and current-file (file-exists-p current-file))
            (file-exists-p file-or-url))
        (soap-fetch-xml-from-file file-or-url wsdl)
      (soap-fetch-xml-from-url file-or-url wsdl))))

(defun soap-load-wsdl (file-or-url &optional wsdl)
  "Load a document from FILE-OR-URL and return it.
Build on WSDL if it is provided."
  (let* ((wsdl (or wsdl (soap-make-wsdl file-or-url)))
         (xml (soap-fetch-xml file-or-url wsdl)))
    (soap-wsdl-resolve-references (soap-parse-wsdl xml wsdl))
    wsdl))

(defalias 'soap-load-wsdl-from-url #'soap-load-wsdl)

(defun soap-parse-wsdl-phase-validate-node (node)
  "Assert that NODE is valid."
  (soap-with-local-xmlns node
    (let ((node-name (soap-l2wk (xml-node-name node))))
      (cl-assert (eq node-name 'wsdl:definitions)
                 nil
                 "expecting wsdl:definitions node, got %s" node-name))))

(defun soap-parse-wsdl-phase-fetch-imports (node wsdl)
  "Fetch and load files imported by NODE into WSDL."
  (soap-with-local-xmlns node
    (dolist (node (soap-xml-get-children1 node 'wsdl:import))
      (let ((location (xml-get-attribute-or-nil node 'location)))
        (when location
          (soap-load-wsdl location wsdl))))))

(defun soap-parse-wsdl-phase-parse-schema (node wsdl)
  "Load types found in NODE into WSDL."
  (soap-with-local-xmlns node
    ;; Find all the 'xsd:schema nodes which are children of wsdl:types nodes and
    ;; build our type-library.
    (let ((types (car (soap-xml-get-children1 node 'wsdl:types))))
      (dolist (node (xml-node-children types))
        ;; We cannot use (xml-get-children node (soap-wk2l 'xsd:schema)) because
        ;; each node can install its own alias type so the schema nodes might
        ;; have a different prefix.
        (when (consp node)
          (soap-with-local-xmlns
              node
            (when (eq (soap-l2wk (xml-node-name node)) 'xsd:schema)
              (soap-wsdl-add-namespace (soap-parse-schema node wsdl)
                                       wsdl))))))))

(defun soap-parse-wsdl-phase-fetch-schema (node wsdl)
  "Fetch and load schema imports defined by NODE into WSDL."
  (soap-with-local-xmlns node
    (while (soap-wsdl-xmlschema-imports wsdl)
      (let* ((import (pop (soap-wsdl-xmlschema-imports wsdl)))
             (xml (soap-fetch-xml import wsdl)))
        (soap-wsdl-add-namespace (soap-parse-schema xml wsdl) wsdl)))))

(defun soap-parse-wsdl-phase-finish-parsing (node wsdl)
  "Finish parsing NODE into WSDL."
  (soap-with-local-xmlns node
    (let ((ns (make-soap-namespace :name (soap-get-target-namespace node))))
      (dolist (node (soap-xml-get-children1 node 'wsdl:message))
        (soap-namespace-put (soap-parse-message node) ns))

      (dolist (node (soap-xml-get-children1 node 'wsdl:portType))
        (let ((port-type (soap-parse-port-type node)))
          (soap-namespace-put port-type ns)
          (soap-wsdl-add-namespace
           (soap-port-type-operations port-type) wsdl)))

      (dolist (node (soap-xml-get-children1 node 'wsdl:binding))
        (soap-namespace-put (soap-parse-binding node) ns))

      (dolist (node (soap-xml-get-children1 node 'wsdl:service))
        (dolist (node (soap-xml-get-children1 node 'wsdl:port))
          (let ((name (xml-get-attribute node 'name))
                (binding (xml-get-attribute node 'binding))
                (url (let ((n (car (soap-xml-get-children1
                                    node 'wsdlsoap:address))))
                       (xml-get-attribute n 'location))))
            (let ((port (make-soap-port
                         :name name :binding (soap-l2fq binding 'tns)
                         :service-url url)))
              (soap-namespace-put port ns)
              (push port (soap-wsdl-ports wsdl))))))

      (soap-wsdl-add-namespace ns wsdl))))

(defun soap-parse-wsdl (node wsdl)
  "Construct from NODE a WSDL structure, which is an XML document."
  ;; Break this into phases to allow for asynchronous parsing.
  (soap-parse-wsdl-phase-validate-node node)
  ;; Makes synchronous calls.
  (soap-parse-wsdl-phase-fetch-imports node wsdl)
  (soap-parse-wsdl-phase-parse-schema node wsdl)
  ;; Makes synchronous calls.
  (soap-parse-wsdl-phase-fetch-schema node wsdl)
  (soap-parse-wsdl-phase-finish-parsing node wsdl)
  wsdl)

(defun soap-parse-message (node)
  "Parse NODE as a wsdl:message and return the corresponding type."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:message)
             nil
             "expecting wsdl:message node, got %s"
             (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        parts)
    (dolist (p (soap-xml-get-children1 node 'wsdl:part))
      (let ((name (xml-get-attribute-or-nil p 'name))
            (type (xml-get-attribute-or-nil p 'type))
            (element (xml-get-attribute-or-nil p 'element)))

        (when type
          (setq type (soap-l2fq type 'tns)))

        (if element
            (setq element (soap-l2fq element 'tns))
          ;; else
          (setq element (make-soap-xs-element
                         :name name
                         :namespace-tag soap-target-xmlns
                         :type^ type)))

        (push (cons name element) parts)))
    (make-soap-message :name name :parts (nreverse parts))))

(defun soap-parse-port-type (node)
  "Parse NODE as a wsdl:portType and return the corresponding port."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:portType)
             nil
             "expecting wsdl:portType node got %s"
             (soap-l2wk (xml-node-name node)))
  (let* ((soap-target-xmlns (concat "urn:" (xml-get-attribute node 'name)))
         (ns (make-soap-namespace :name soap-target-xmlns)))
    (dolist (node (soap-xml-get-children1 node 'wsdl:operation))
      (let ((o (soap-parse-operation node)))

        (let ((other-operation (soap-namespace-get
                                (soap-element-name o) ns 'soap-operation-p)))
          (if other-operation
              ;; Unfortunately, the Confluence WSDL defines two operations
              ;; named "search" which differ only in parameter names...
              (soap-warning "Discarding duplicate operation: %s"
                            (soap-element-name o))

            (progn
              (soap-namespace-put o ns)

              ;; link all messages from this namespace, as this namespace
              ;; will be used for decoding the response.
              (cl-destructuring-bind (name . message) (soap-operation-input o)
                (soap-namespace-put-link name message ns))

              (cl-destructuring-bind (name . message) (soap-operation-output o)
                (soap-namespace-put-link name message ns))

              (dolist (fault (soap-operation-faults o))
                (cl-destructuring-bind (name . message) fault
                  (soap-namespace-put-link name message ns)))

              )))))

    (make-soap-port-type :name (xml-get-attribute node 'name)
                         :operations ns)))

(defun soap-parse-operation (node)
  "Parse NODE as a wsdl:operation and return the corresponding type."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:operation)
             nil
             "expecting wsdl:operation node, got %s"
             (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute node 'name))
        (parameter-order (split-string
                          (xml-get-attribute node 'parameterOrder)))
        input output faults input-action output-action)
    (dolist (n (xml-node-children node))
      (when (consp n)                 ; skip string nodes which are whitespace
        (let ((node-name (soap-l2wk (xml-node-name n))))
          (cond
           ((eq node-name 'wsdl:input)
            (let ((message (xml-get-attribute n 'message))
                  (name (xml-get-attribute n 'name))
                  (action (soap-xml-get-attribute-or-nil1 n 'wsaw:Action)))
              (setq input (cons name (soap-l2fq message 'tns)))
              (setq input-action action)))
           ((eq node-name 'wsdl:output)
            (let ((message (xml-get-attribute n 'message))
                  (name (xml-get-attribute n 'name))
                  (action (soap-xml-get-attribute-or-nil1 n 'wsaw:Action)))
              (setq output (cons name (soap-l2fq message 'tns)))
              (setq output-action action)))
           ((eq node-name 'wsdl:fault)
            (let ((message (xml-get-attribute n 'message))
                  (name (xml-get-attribute n 'name)))
              (push (cons name (soap-l2fq message 'tns)) faults)))))))
    (make-soap-operation
     :name name
     :namespace-tag soap-target-xmlns
     :parameter-order parameter-order
     :input input
     :output output
     :faults (nreverse faults)
     :input-action input-action
     :output-action output-action)))

(defun soap-parse-binding (node)
  "Parse NODE as a wsdl:binding and return the corresponding type."
  (cl-assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:binding)
             nil
             "expecting wsdl:binding node, got %s"
             (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute node 'name))
        (type (xml-get-attribute node 'type)))
    (let ((binding (make-soap-binding :name name
                                      :port-type (soap-l2fq type 'tns))))
      (dolist (wo (soap-xml-get-children1 node 'wsdl:operation))
        (let ((name (xml-get-attribute wo 'name))
              soap-action
              soap-headers
              soap-body
              use)
          (dolist (so (soap-xml-get-children1 wo 'wsdlsoap:operation))
            (setq soap-action (xml-get-attribute-or-nil so 'soapAction)))

          ;; Search a wsdlsoap:body node and find a "use" tag.  The
          ;; same use tag is assumed to be present for both input and
          ;; output types (although the WDSL spec allows separate
          ;; "use"-s for each of them...

          (dolist (i (soap-xml-get-children1 wo 'wsdl:input))

            ;; There can be multiple headers ...
            (dolist (h (soap-xml-get-children1 i 'wsdlsoap:header))
              (let ((message (soap-l2fq (xml-get-attribute-or-nil h 'message)))
                    (part (xml-get-attribute-or-nil h 'part))
                    (use (xml-get-attribute-or-nil h 'use)))
                (when (and message part)
                  (push (list message part use) soap-headers))))

            ;; ... but only one body
            (let ((body (car (soap-xml-get-children1 i 'wsdlsoap:body))))
              (setq soap-body (xml-get-attribute-or-nil body 'parts))
              (when soap-body
                (setq soap-body
                      (mapcar #'intern (split-string soap-body
                                                     nil
                                                     'omit-nulls))))
              (setq use (xml-get-attribute-or-nil body 'use))))

          (unless use
            (dolist (i (soap-xml-get-children1 wo 'wsdl:output))
              (dolist (b (soap-xml-get-children1 i 'wsdlsoap:body))
                (setq use (or use
                              (xml-get-attribute-or-nil b 'use))))))

          (puthash name (make-soap-bound-operation
                         :operation name
                         :soap-action soap-action
                         :soap-headers (nreverse soap-headers)
                         :soap-body soap-body
                         :use (and use (intern use)))
                   (soap-binding-operations binding))))
      binding)))

;;;; SOAP type decoding

(defvar soap-multi-refs nil
  "The list of multi-ref nodes in the current SOAP response.
This is a dynamically bound variable used during decoding the
SOAP response.")

(defvar soap-decoded-multi-refs nil
  "List of decoded multi-ref nodes in the current SOAP response.
This is a dynamically bound variable used during decoding the
SOAP response.")

(defun soap-decode-type (type node)
  "Use TYPE (an xsd type) to decode the contents of NODE.

NODE is an XML node, representing some SOAP encoded value or a
reference to another XML node (a multiRef).  This function will
resolve the multiRef reference, if any, than call a TYPE specific
decode function to perform the actual decoding."
  (let ((href (xml-get-attribute-or-nil node 'href)))
    (cond (href
           (catch 'done
             ;; NODE is actually a HREF, find the target and decode that.
             ;; Check first if we already decoded this multiref.

             (let ((decoded (cdr (assoc href soap-decoded-multi-refs))))
               (when decoded
                 (throw 'done decoded)))

             (unless (string-match "^#\\(.*\\)$" href)
               (error "Invalid multiRef: %s" href))

             (let ((id (match-string 1 href)))
               (dolist (mr soap-multi-refs)
                 (let ((mrid (xml-get-attribute mr 'id)))
                   (when (equal id mrid)
                     ;; recurse here, in case there are multiple HREF's
                     (let ((decoded (soap-decode-type type mr)))
                       (push (cons href decoded) soap-decoded-multi-refs)
                       (throw 'done decoded)))))
               (error "Cannot find href %s" href))))
          (t
           (soap-with-local-xmlns node
             (if (equal (soap-xml-get-attribute-or-nil1 node 'xsi:nil) "true")
                 nil
               ;; Handle union types.
               (cond ((listp type)
                      (catch 'done
                        (dolist (union-member type)
                          (let* ((decoder (get (soap-type-of union-member)
                                               'soap-decoder))
                                 (result (ignore-errors
                                           (funcall decoder
                                                    union-member node))))
                            (when result (throw 'done result))))))
                     (t
                      (let ((decoder (get (soap-type-of type) 'soap-decoder)))
                        (cl-assert decoder nil
                                   "no soap-decoder for %s type"
                                   (soap-type-of type))
                        (funcall decoder type node))))))))))

(defun soap-decode-any-type (node)
  "Decode NODE using type information inside it."
  ;; If the NODE has type information, we use that...
  (let ((type (soap-xml-get-attribute-or-nil1 node 'xsi:type)))
    (when type
      (setq type (soap-l2fq type)))
    (if type
        (let ((wtype (soap-wsdl-get type soap-current-wsdl 'soap-xs-type-p)))
          (if wtype
              (soap-decode-type wtype node)
            ;; The node has type info encoded in it, but we don't know how
            ;; to decode it...
            (error "Node has unknown type: %s" type)))

      ;; No type info in the node...

      (let ((contents (xml-node-children node)))
        (if (and (= (length contents) 1) (stringp (car contents)))
            ;; contents is just a string
            (car contents)

          ;; we assume the NODE is a sequence with every element a
          ;; structure name
          (let (result)
            (dolist (element contents)
              ;; skip any string contents, assume they are whitespace
              (unless (stringp element)
                (let ((key (xml-node-name element))
                      (value (soap-decode-any-type element)))
                  (push (cons key value) result))))
            (nreverse result)))))))

(defun soap-decode-array (node)
  "Decode NODE as an Array using type information inside it."
  (let ((type (soap-xml-get-attribute-or-nil1 node 'soapenc:arrayType))
        (wtype nil)
        (contents (xml-node-children node))
        result)
    (when type
      ;; Type is in the format "someType[NUM]" where NUM is the number of
      ;; elements in the array.  We discard the [NUM] part.
      (setq type (replace-regexp-in-string "\\[[0-9]+\\]\\'" "" type))
      (setq wtype (soap-wsdl-get (soap-l2fq type)
                                 soap-current-wsdl 'soap-xs-type-p))
      (unless wtype
        ;; The node has type info encoded in it, but we don't know how to
        ;; decode it...
        (error "soap-decode-array: Node has unknown type: %s" type)))
    (dolist (e contents)
      (when (consp e)
        (push (if wtype
                  (soap-decode-type wtype e)
                (soap-decode-any-type e))
              result)))
    (nreverse result)))

;;;; Soap Envelope parsing

(if (fboundp 'define-error)
    (define-error 'soap-error "SOAP error")
  ;; Support Emacs<24.4 that do not have define-error, so
  ;; that soap-client can remain unchanged in GNU ELPA.
  (put 'soap-error
       'error-conditions
       '(error soap-error))
  (put 'soap-error 'error-message "SOAP error"))

(defun soap-parse-envelope (node operation wsdl)
  "Parse the SOAP envelope in NODE and return the response.
OPERATION is the WSDL operation for which we expect the response,
WSDL is used to decode the NODE."
  (soap-with-local-xmlns node
    (cl-assert (eq (soap-l2wk (xml-node-name node)) 'soap:Envelope)
               nil
               "expecting soap:Envelope node, got %s"
               (soap-l2wk (xml-node-name node)))
    (let ((headers (soap-xml-get-children1 node 'soap:Header))
          (body (car (soap-xml-get-children1 node 'soap:Body))))

      (let ((fault (car (soap-xml-get-children1 body 'soap:Fault))))
        (when fault
          (let ((fault-code (let ((n (car (xml-get-children
                                           fault 'faultcode))))
                              (car-safe (xml-node-children n))))
                (fault-string (let ((n (car (xml-get-children
                                             fault 'faultstring))))
                                (car-safe (xml-node-children n))))
                (detail (xml-get-children fault 'detail)))
            (while t
              (signal 'soap-error (list fault-code fault-string detail))))))

      ;; First (non string) element of the body is the root node of he
      ;; response
      (let ((response (if (eq (soap-bound-operation-use operation) 'literal)
                          ;; For 'literal uses, the response is the actual body
                          body
                        ;; ...otherwise the first non string element
                        ;; of the body is the response
                        (catch 'found
                          (dolist (n (xml-node-children body))
                            (when (consp n)
                              (throw 'found n)))))))
        (soap-parse-response response operation wsdl headers body)))))

(defun soap-parse-response (response-node operation wsdl soap-headers soap-body)
  "Parse RESPONSE-NODE and return the result as a LISP value.
OPERATION is the WSDL operation for which we expect the response,
WSDL is used to decode the NODE.

SOAP-HEADERS is a list of the headers of the SOAP envelope or nil
if there are no headers.

SOAP-BODY is the body of the SOAP envelope (of which
RESPONSE-NODE is a sub-node).  It is used in case RESPONSE-NODE
reference multiRef parts which are external to RESPONSE-NODE."
  (let* ((soap-current-wsdl wsdl)
         (op (soap-bound-operation-operation operation))
         (use (soap-bound-operation-use operation))
         (message (cdr (soap-operation-output op))))

    (soap-with-local-xmlns response-node

      (when (eq use 'encoded)
        (let* ((received-message-name (soap-l2fq (xml-node-name response-node)))
               (received-message (soap-wsdl-get
                                  received-message-name wsdl 'soap-message-p)))
          (unless (eq received-message message)
            (error "Unexpected message: got %s, expecting %s"
                   received-message-name
                   (soap-element-name message)))))

      (let ((decoded-parts nil)
            (soap-multi-refs (xml-get-children soap-body 'multiRef))
            (soap-decoded-multi-refs nil))

        (dolist (part (soap-message-parts message))
          (let ((tag (car part))
                (type (cdr part))
                node)

            (setq node
                  (cond
                   ((eq use 'encoded)
                    (car (xml-get-children response-node tag)))

                   ((eq use 'literal)
                    (catch 'found
                      (let* ((ns-aliases (soap-wsdl-alias-table wsdl))
                             (ns-name (cdr (assoc
                                            (soap-element-namespace-tag type)
                                            ns-aliases)))
                             (fqname (cons ns-name (soap-element-name type))))
                        (dolist (c (append (mapcar (lambda (header)
                                                     (car (xml-node-children
                                                           header)))
                                                   soap-headers)
                                           (xml-node-children response-node)))
                          (when (consp c)
                            (soap-with-local-xmlns c
                              (when (equal (soap-l2fq (xml-node-name c))
                                           fqname)
                                (throw 'found c))))))))))

            (unless node
              (error "Soap-parse-response(%s): cannot find message part %s"
                     (soap-element-name op) tag))
            (let ((decoded-value (soap-decode-type type node)))
              (when decoded-value
                (push decoded-value decoded-parts)))))

        decoded-parts))))

;;;; SOAP type encoding

(defun soap-encode-attributes (value type)
  "Encode XML attributes for VALUE according to TYPE.
This is a generic function which determines the attribute encoder
for the type and calls that specialized function to do the work.

Attributes are inserted in the current buffer at the current
position."
  (let ((attribute-encoder (get (soap-type-of type) 'soap-attribute-encoder)))
    (cl-assert attribute-encoder nil
               "no soap-attribute-encoder for %s type" (soap-type-of type))
    (funcall attribute-encoder value type)))

(defun soap-encode-value (value type)
  "Encode the VALUE using TYPE.
The resulting XML data is inserted in the current buffer
at (point)/

TYPE is one of the soap-*-type structures which defines how VALUE
is to be encoded.  This is a generic function which finds an
encoder function based on TYPE and calls that encoder to do the
work."
  (let ((encoder (get (soap-type-of type) 'soap-encoder)))
    (cl-assert encoder nil "no soap-encoder for %s type" (soap-type-of type))
    (funcall encoder value type))
  (when (soap-element-namespace-tag type)
    (add-to-list 'soap-encoded-namespaces (soap-element-namespace-tag type))))

(defun soap-encode-body (operation parameters &optional service-url)
  "Create the body of a SOAP request for OPERATION in the current buffer.
PARAMETERS is a list of parameters supplied to the OPERATION.

The OPERATION and PARAMETERS are encoded according to the WSDL
document.  SERVICE-URL should be provided when WS-Addressing is
being used."
  (let* ((op (soap-bound-operation-operation operation))
         (use (soap-bound-operation-use operation))
         (message (cdr (soap-operation-input op)))
         (parameter-order (soap-operation-parameter-order op))
         (param-table (cl-loop for formal in parameter-order
                               for value in parameters
                               collect (cons formal value))))

    (unless (= (length parameter-order) (length parameters))
      (error "Wrong number of parameters for %s: expected %d, got %s"
             (soap-element-name op)
             (length parameter-order)
             (length parameters)))

    (let ((headers (soap-bound-operation-soap-headers operation))
          (input-action (soap-operation-input-action op)))
      (when headers
        (insert "<soap:Header>\n")
        (when input-action
          (add-to-list 'soap-encoded-namespaces "wsa")
          (insert "<wsa:Action>" input-action "</wsa:Action>\n")
          (insert "<wsa:To>" service-url "</wsa:To>\n"))
        (dolist (h headers)
          (let* ((message (nth 0 h))
                 (part (assq (nth 1 h) (soap-message-parts message)))
                 (value (cdr (assoc (car part) (car parameters))))
                 (use (nth 2 h))
                 (element (cdr part)))
            (when (eq use 'encoded)
              (when (soap-element-namespace-tag element)
                (add-to-list 'soap-encoded-namespaces
                             (soap-element-namespace-tag element)))
              (insert "<" (soap-element-fq-name element) ">\n"))
            (soap-encode-value value element)
            (when (eq use 'encoded)
              (insert "</" (soap-element-fq-name element) ">\n"))))
        (insert "</soap:Header>\n")))

    (insert "<soap:Body>\n")
    (when (eq use 'encoded)
      (when (soap-element-namespace-tag op)
        (add-to-list 'soap-encoded-namespaces (soap-element-namespace-tag op)))
      (insert "<" (soap-element-fq-name op) ">\n"))

    (dolist (part (soap-message-parts message))
      (let* ((param-name (car part))
             (element (cdr part))
             (value (cdr (assoc param-name param-table))))
        (when (or (null (soap-bound-operation-soap-body operation))
                  (member param-name
                          (soap-bound-operation-soap-body operation)))
          (soap-encode-value value element))))

    (when (eq use 'encoded)
      (insert "</" (soap-element-fq-name op) ">\n"))
    (insert "</soap:Body>\n")))

(defun soap-create-envelope (operation parameters wsdl &optional service-url)
  "Create a SOAP request envelope for OPERATION using PARAMETERS.
WSDL is the wsdl document used to encode the PARAMETERS.
SERVICE-URL should be provided when WS-Addressing is being used."
  (with-temp-buffer
    (let ((soap-encoded-namespaces '("xsi" "soap" "soapenc"))
          (use (soap-bound-operation-use operation)))

      ;; Create the request body
      (soap-encode-body operation parameters service-url)

      ;; Put the envelope around the body
      (goto-char (point-min))
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<soap:Envelope\n")
      (when (eq use 'encoded)
        (insert "    soapenc:encodingStyle=\"\
http://schemas.xmlsoap.org/soap/encoding/\"\n"))
      (dolist (nstag soap-encoded-namespaces)
        (insert "    xmlns:" nstag "=\"")
        (let ((nsname (cdr (assoc nstag soap-well-known-xmlns))))
          (unless nsname
            (setq nsname (cdr (assoc nstag (soap-wsdl-alias-table wsdl)))))
          (insert nsname)
          (insert "\"\n")))
      (insert ">\n")
      (goto-char (point-max))
      (insert "</soap:Envelope>\n"))

    (buffer-string)))

;;;; invoking soap methods

(defcustom soap-debug nil
  "When t, enable some debugging facilities."
  :type 'boolean)

(defun soap-find-port (wsdl service)
  "Return the WSDL port having SERVICE name.
Signal an error if not found."
  (or (catch 'found
        (dolist (p (soap-wsdl-ports wsdl))
          (when (equal service (soap-element-name p))
            (throw 'found p))))
      (error "Unknown SOAP service: %s" service)))

(defun soap-find-operation (port operation-name)
  "Inside PORT, find OPERATION-NAME, a `soap-port-type'.
Signal an error if not found."
  (let* ((binding (soap-port-binding port))
         (op (gethash operation-name (soap-binding-operations binding))))
    (or op
        (error "No operation %s for SOAP service %s"
               operation-name (soap-element-name port)))))

(defun soap-operation-arity (wsdl service operation-name)
  "Return the number of arguments required by a soap operation.
WSDL, SERVICE, OPERATION-NAME and PARAMETERS are as described in
`soap-invoke'."
  (let* ((port (soap-find-port wsdl service))
         (op (soap-find-operation port operation-name))
         (bop (soap-bound-operation-operation op)))
    (length (soap-operation-parameter-order bop))))

(defun soap-invoke-internal (callback cbargs wsdl service operation-name
                                      &rest parameters)
  "Implement `soap-invoke' and `soap-invoke-async'.
If CALLBACK is non-nil, operate asynchronously, then call CALLBACK as (apply
CALLBACK RESPONSE CBARGS), where RESPONSE is the SOAP invocation result.
If CALLBACK is nil, operate synchronously.  WSDL, SERVICE,
OPERATION-NAME and PARAMETERS are as described in `soap-invoke'."
  (let* ((port (soap-find-port wsdl service))
         (operation (soap-find-operation port operation-name)))
    (let ((url-request-method "POST")
          (url-package-name "soap-client.el")
          (url-package-version "1.0")
          (url-request-data
           ;; url-request-data expects a unibyte string already encoded...
           (encode-coding-string
            (soap-create-envelope operation parameters wsdl
                                  (soap-port-service-url port))
            'utf-8))
          (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
          (url-http-attempt-keepalives t)
          (url-request-extra-headers
           (list
            (cons "SOAPAction"
                  (concat "\"" (encode-coding-string
                                (soap-bound-operation-soap-action
                                 operation)
                                'utf-8)
                          "\""))
            (cons "Content-Type"
                  "text/xml; charset=utf-8"))))
      (if callback
          (url-retrieve
           (soap-port-service-url port)
           (lambda (status)
             (let ((data-buffer (current-buffer)))
               (unwind-protect
                   (let ((error-status (plist-get status :error)))
                     (if error-status
                         (signal (car error-status) (cdr error-status))
                       (apply callback
                              (soap-parse-envelope
                               (soap-parse-server-response)
                               operation wsdl)
                              cbargs)))
                 ;; Ensure the url-retrieve buffer is not leaked.
                 (and (buffer-live-p data-buffer)
                      (kill-buffer data-buffer))))))
        (let ((buffer (url-retrieve-synchronously
                       (soap-port-service-url port))))
          (condition-case err
              (with-current-buffer buffer
                (if (null url-http-response-status)
                    (error "No HTTP response from server"))
                (if (and soap-debug (> url-http-response-status 299))
                    ;; This is a warning because some SOAP errors come
                    ;; back with a HTTP response 500 (internal server
                    ;; error)
                    (warn "Error in SOAP response: HTTP code %s"
                          url-http-response-status))
                (soap-parse-envelope (soap-parse-server-response)
                                     operation wsdl))
            (soap-error
             ;; Propagate soap-errors -- they are error replies of the
             ;; SOAP protocol and don't indicate a communication
             ;; problem or a bug in this code.
             (signal (car err) (cdr err)))
            (error
             (when soap-debug
               (pop-to-buffer buffer))
             (error (error-message-string err)))))))))

(defun soap-invoke (wsdl service operation-name &rest parameters)
  "Invoke a SOAP operation and return the result.

WSDL is used for encoding the request and decoding the response.
It also contains information about the WEB server address that
will service the request.

SERVICE is the SOAP service to invoke.

OPERATION-NAME is the operation to invoke.

PARAMETERS -- the remaining parameters are used as parameters for
the SOAP request.

NOTE: The SOAP service provider should document the available
operations and their parameters for the service.  You can also
use the `soap-inspect' function to browse the available
operations in a WSDL document.

NOTE: `soap-invoke' base64-decodes xsd:base64Binary return values
into unibyte strings; these byte-strings require further
interpretation by the caller."
  (apply #'soap-invoke-internal nil nil wsdl service operation-name parameters))

(defun soap-invoke-async (callback cbargs wsdl service operation-name
                                   &rest parameters)
  "Like `soap-invoke', but call CALLBACK asynchronously with response.
CALLBACK is called as (apply CALLBACK RESPONSE CBARGS), where
RESPONSE is the SOAP invocation result.  WSDL, SERVICE,
OPERATION-NAME and PARAMETERS are as described in `soap-invoke'."
  (unless callback
    (error "Callback argument is nil"))
  (apply #'soap-invoke-internal callback cbargs wsdl service operation-name
         parameters))

(provide 'soap-client)


;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;;;+"
;; End:

;;; soap-client.el ends here
