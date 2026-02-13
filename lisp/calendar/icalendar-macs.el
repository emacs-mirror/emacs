;;; icalendar-macs.el --- Macros for iCalendar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: October 2024
;; Keywords: calendar
;; Human-Keywords: calendar, iCalendar

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines the macros `ical:define-type', `ical:define-param',
;; `ical:define-property' and `ical:define-component', used in
;; icalendar-parser.el to define the particular value types, parameters,
;; properties and components in the standard as type symbols.

;; TODOs:
;;   - in the define* macros, :default needs rethinking.
;;     I had made this a string because otherwise you can't distinguish
;;     an unspecified default from an explicit "FALSE" for icalendar-boolean
;;     But this might not be true/might not matter anyway, and it's a pain
;;     to have to read the default value where you need it.  Probably
;;     should just change these to be the value as read.


;;; Code:

(eval-when-compile (require 'cl-lib))

(declare-function ical:ast-node-p "icalendar-ast")
(declare-function ical:ast-node-type "icalendar-ast")
(declare-function ical:ast-node-value "icalendar-ast")
(declare-function ical:type-symbol-p "icalendar-ast")
(declare-function ical:value-type-symbol-p "icalendar-ast")
(declare-function ical:expects-list-of-values-p "icalendar-ast")

;; Some utilities:

(defun ical:format-child-spec (child-spec)
  "Format CHILD-SPEC as a table for use in symbol documentation."
  (concat
   (format "%-30s%6s\n" "Type" "Number")
   (make-string 36 ?-) "\n"
   (mapconcat
    (lambda (type) (format "%-30s%-6s\n" (format "`%s'" type) "1"))
    (plist-get child-spec :one))
   (mapconcat
    (lambda (type) (format "%-30s%-6s\n" (format "`%s'" type) "1+"))
    (plist-get child-spec :one-or-more))
   (mapconcat
    (lambda (type) (format "%-30s%-6s\n" (format "`%s'" type) "0-1"))
    (plist-get child-spec :zero-or-one))
   (mapconcat
    (lambda (type) (format "%-30s%-6s\n" (format "`%s'" type) "0+"))
    (plist-get child-spec :zero-or-more))))


;; Define value types:
(cl-defmacro ical:define-type (symbolic-name print-name doc specifier matcher
                               &key link
                                    (reader #'identity)
                                    (printer #'identity))
  "Define an iCalendar value type named SYMBOLIC-NAME.

PRINT-NAME should be the string used to represent this type in
the value of an `icalendar-valuetypeparam' property parameter, or
nil if this is not a type that should be specified there.  DOC
should be a documentation string for the type.  SPECIFIER should
be a type specifier in the sense of `cl-deftype'.  MATCHER should
be an RX definition body (see `rx-define'; argument lists are not
supported).

Before the type is defined with `cl-deftype', a function will be
defined named `icalendar-match-PRINT-NAME-value'
\(or `icalendar-match-OTHER-value', if PRINT-NAME is nil, where
OTHER is derived from SYMBOLIC-NAME by removing any prefix
\"icalendar-\" and suffix \"value\").  This function takes a
string argument and matches it against MATCHER.  This function may
thus occur in SPECIFIER (e.g. in a (satisfies ...) clause).

See the functions `icalendar-read-value-node',
`icalendar-parse-value-node', and `icalendar-print-value-node' to
convert values defined with this macro to and from their text
representation in iCalendar format.

The following keyword arguments are accepted:

:reader - a function to read data of this type.  It will be passed
  a string matching MATCHER and should return an Elisp data structure.
  Its name does not need to be quoted.  (default: identity)

:printer - a function to convert an Elisp data structure of this
  type to a string.  Its name does not need to be quoted.
  (default: identity)

:link - a string containing a URL for further documentation of this type"
  (declare (doc-string 2))
  (let* (;; Related functions:
         (type-dname (if print-name
                         (downcase print-name)
                       (string-trim
                        (symbol-name symbolic-name)
                        "icalendar-" "value")))
         (matcher-name (intern (concat "icalendar-match-" type-dname "-value")))
         ;; Documentation:
         (header "It names a value type defined by `icalendar-define-type'.")
         (matcher-doc (format
"Strings representing values of this type can be matched with
`%s'.\n" matcher-name))
         (reader-doc (format "They can be read with `%s'\n" reader))
         (printer-doc (format "and printed with `%s'." printer))
         (full-doc (concat header "\n\n" doc "\n\n"
                           matcher-doc reader-doc printer-doc "\n\n"
"A syntax node of this type can be read with
`icalendar-read-value-node' or parsed with `icalendar-parse-value-node',
and printed with `icalendar-print-value-node'.")))

    `(progn
       ;; Type metadata needs to be available at both compile time and
       ;; run time.  In particular, `ical:value-type-symbol-p' needs to
       ;; work at compile time.
       (eval-and-compile
         (setplist (quote ,symbolic-name)
                   (list
                    'ical:is-type t
                    'ical:is-value t
                    'ical:matcher (function ,matcher-name)
                    'ical:value-rx (quote ,symbolic-name)
                    'ical:value-reader (function ,reader)
                    'ical:value-printer (function ,printer)
                    'ical:type-documentation ,full-doc
                    'ical:link ,link)))

       (rx-define ,symbolic-name
         ,matcher)

       (defun ,matcher-name (s)
         ,(format "Match string S against rx `%s'." symbolic-name)
         (string-match (rx ,symbolic-name) s))

       (cl-deftype ,symbolic-name () ,specifier)

       ;; Store the association between the print name and the type
       ;; symbol in ical:value-types.  The check against print name
       ;; here allows us to also define value types that aren't
       ;; "really" types according to the standard, like
       ;; `ical:geo-coordinates'.  Only types that have a
       ;; print-name can be specified in a VALUE parameter.
       (when ,print-name
         (push (cons ,print-name (quote ,symbolic-name)) ical:value-types)))))

;; TODO: not sure this is needed.  I've only used it once in the parser.
(cl-defmacro ical:define-keyword-type (symbolic-name print-name doc matcher
                                       &key link
                                            (reader 'intern)
                                            (printer 'symbol-name))
  "Like `icalendar-define-type', for types represented by strings.
String values matching MATCHER are assumed to be type-specific keywords
that should be interned as symbols when read.  (Thus no type specifier
is necessary: it is always just \\='symbol.) Their printed
representation is their symbol name."
  `(ical:define-type ,symbolic-name ,print-name ,doc
                     'symbol
                     ,matcher
                     :link ,link
                     :reader ,reader
                     :printer ,printer))


;; Define parameters:
(cl-defmacro ical:define-param (symbolic-name param-name doc value
                                &key quoted
                                     list-sep
                                     default
                                     (unrecognized default)
                                     ((:name-face name-face)
                                      'ical:parameter-name nondefault-name-face)
                                     ((:value-face value-face)
                                      'ical:parameter-value nondefault-value-face)
                                     ((:warn-face warn-face)
                                      'ical:warning nondefault-warn-face)
                                     extra-faces
                                     link)
  "Define iCalendar parameter PARAM-NAME under the symbol SYMBOLIC-NAME.
PARAM-NAME should be the parameter name as it should appear in
iCalendar data.

VALUE should either be a symbol for a value type defined with
`icalendar-define-type', or an `rx' regular expression.  If it is
a type symbol, the regex, reader and printer functions associated
with that type will be used when parsing and serializing values.
If it is a regular expression, it is assumed that the values of
this parameter are strings which match that regular expression.

An `rx' regular expression named SYMBOLIC-NAME which matches the
parameter is defined:
  Group 1 of this regex matches PARAM-NAME
    (or any valid parameter name, if PARAM-NAME is nil).
  Group 2 matches VALUE, which specifies a correct value
    for this parameter according to RFC5545.
  Group 3, if matched, contains any parameter value which does
    *not* match VALUE, and is incorrect according to the standard.

This regex matches the entire string representing this parameter,
from \";\" to the end of its value.  Another regular expression
named `SYMBOLIC-NAME-value' is also defined to match just the
value part, after \";PARAM-NAME=\", with groups 2 and 3 as above.

A function to match the complete parameter expression called
`icalendar-match-PARAM-NAME-param' is defined
\(or `icalendar-match-OTHER-param-value' if PARAM-NAME is nil,
where OTHER is derived from SYMBOLIC-NAME by removing any prefix
`icalendar-' and suffix `param').  This function is used
to provide syntax highlighting in `icalendar-mode'.

See the functions `icalendar-read-param-value',
`icalendar-parse-param-value', `icalendar-parse-params' and
`icalendar-print-param-node' to convert parameters defined with
this macro to and from their text representation in iCalendar
format.

The following keyword arguments are accepted:

:default - a (string representing the) default value, if the
  parameter is not specified on a given property.

:unrecognized - a (string representing the) value which must be
  substituted for values that are not recognized but syntactically
  correct according to RFC5545.  Unrecognized values must be in match
  group 5 of the regex determined by VALUE.  An unrecognized value will
  be preserved in the syntax tree metadata and printed instead of this
  value when the node is printed.  Defaults to any value specified for
  :default.

:quoted - non-nil if values of this parameter must always be surrounded
  by (double-)quotation marks when printed, according to RFC5545.

:list-sep - if the parameter accepts a list of values, this should be a
  string which separates the values (typically \",\").  If :list-sep is
  non-nil, the value string will first be split on the separator, then
  if :quoted is non-nil, the individual values will be unquoted, then
  each value will be read according to VALUE and collected into a list
  when parsing.  When printing, the inverse happens: values are quoted
  if :quoted is non-nil, then joined with :list-sep.  Passing this
  argument marks SYMBOLIC-NAME as a type that accepts a list of values
  for `icalendar-expects-list-of-values-p'.

:name-face - a face symbol for highlighting the property name
  (default: `icalendar-parameter-name')

:value-face - a face symbol for highlighting valid property values
  (default: `icalendar-parameter-value')

:warn-face - a face symbol for highlighting invalid property values
  (default: `icalendar-warning')

:extra-faces - a list of the form accepted for HIGHLIGHT in
  `font-lock-keywords'.  In particular,
    ((GROUPNUM FACENAME [OVERRIDE [LAXMATCH]]) ...)
  can be used to apply different faces to different
  match subgroups.

:link - a string containing a URL for documentation of this parameter.
  The URL will be provided in the documentation shown by
  `describe-symbol' for SYMBOLIC-NAME."
  (declare (doc-string 2))
  (let* (;; Related function names:
         (param-dname (if param-name
                          (downcase param-name)
                        (string-trim (symbol-name symbolic-name)
                                     "icalendar-" "param")))
         (matcher-name (intern (concat "icalendar-match-" param-dname "-param")))
         (type-predicate-name
          (intern (concat "icalendar-" param-dname "-param-p")))
         ;; Value regexes:
         (qvalue-rx (if quoted `(seq ?\" ,value ?\") value))
         (values-rx (when list-sep
                     `(seq ,qvalue-rx (zero-or-more ,list-sep ,qvalue-rx))))
         (full-value-rx-name
          (intern (concat (symbol-name symbolic-name) "-value")))
         ;; Faces:
         (has-faces (or nondefault-name-face nondefault-value-face
                        nondefault-warn-face extra-faces))
         ;; Documentation:
         (header "It names a parameter type defined by `icalendar-define-param'.")
         (val-list (if list-sep (concat "VAL1" list-sep "VAL2" list-sep "...")
                     "VAL"))
         (s (if list-sep "s" "")) ; to make plurals
         (val-doc (concat "VAL" s " "
                          "must be " (unless list-sep "a ") (when quoted "quoted ")
                          (if (ical:value-type-symbol-p value)
                              (format "`%s' value%s" (symbol-name value) s)
                            (format "string%s matching rx `%S'" s value))))
         (syntax-doc (format "Syntax: %s=%s\n%s"
                             (or param-name "(NAME)") val-list val-doc))
         (full-doc (concat header "\n\n" doc "\n\n" syntax-doc)))

    `(progn
       ;; Type metadata needs to be available at both compile time and
       ;; run time.  In particular, `ical:value-type-symbol-p' needs to
       ;; work at compile time.
       (eval-and-compile
         (setplist (quote ,symbolic-name)
                   (list
                    'ical:is-type t
                    'ical:is-param t
                    'ical:matcher (function ,matcher-name)
                    'ical:default-value ,default
                    'ical:is-quoted ,quoted
                    'ical:list-sep ,list-sep
                    'ical:substitute-value ,unrecognized
                    'ical:matcher (function ,matcher-name)
                    'ical:value-type
                    (when (ical:value-type-symbol-p (quote ,value))
                      (quote ,value))
                    'ical:value-rx (quote ,value)
                    'ical:values-rx (quote ,values-rx)
                    'ical:full-value-rx (quote ,full-value-rx-name)
                    'ical:type-documentation ,full-doc
                    'ical:link ,link)))

       ;; Regex which matches just the value of the parameter:
       ;; Group 2: correct values of the parameter, and
       ;; Group 3: incorrect values up to the next parameter
       (rx-define ,full-value-rx-name
         (or (group-n 2 ,(or values-rx qvalue-rx))
             (group-n 3 ical:param-value)))

       ;; Regex which matches the full parameter:
       ;; Group 1: the parameter name,
       ;; Group 2: correct values of the parameter, and
       ;; Group 3: incorrect values up to the next parameter
       (rx-define ,symbolic-name
         (seq ";"
              ;; if the parameter name has no printed form, the best we
              ;; can do is match ical:param-name:
              (group-n 1 ,(or param-name 'ical:param-name))
              "="
              ,full-value-rx-name))

       ;; CL-type to represent syntax nodes for this parameter:
       (defun ,type-predicate-name (node)
         ,(format "Return non-nil if NODE represents a %s parameter." param-name)
         (and (ical:ast-node-p node)
              (eq (ical:ast-node-type node) (quote ,symbolic-name))))

       (cl-deftype ,symbolic-name () '(satisfies ,type-predicate-name))

       ;; Matcher for the full param string, for syntax highlighting:
       (defun ,matcher-name (limit)
         ,(concat (format "Matcher for %s parameter.\n" param-name)
                  "(Defined by `icalendar-define-param'.)")
         (re-search-forward (rx ,symbolic-name) limit t))

       ;; Entry for font-lock-keywords in icalendar-mode:
       (when ,has-faces
         ;; Avoid circular load of icalendar-mode.el in
         ;; icalendar-parser.el (which does not use the *-face
         ;; keywords), while still allowing external code to add to
         ;; font-lock-keywords dynamically:
         (require 'icalendar-mode)
         (push (quote (,matcher-name
                       (1 (quote ,name-face) t t)
                       (2 (quote ,value-face) t t)
                       (3 (quote ,warn-face) t t)
                       ,@extra-faces))
               ical:font-lock-keywords))

       ;; Associate the print name with the type symbol for
       ;; `ical:parse-params' and `ical:print-param':
       (when ,param-name
         (push (cons ,param-name (quote ,symbolic-name)) ical:param-types)))))


;; Define properties:
(cl-defmacro ical:define-property (symbolic-name property-name doc value
                                   &key default
                                        (unrecognized default)
                                        (default-type
                                         (if (ical:value-type-symbol-p value)
                                             value
                                           'ical:text))
                                        other-types
                                        list-sep
                                        child-spec
                                        other-validator
                                        ((:name-face name-face)
                                         'ical:property-name nondefault-name-face)
                                        ((:value-face value-face)
                                         'ical:property-value nondefault-value-face)
                                        ((:warn-face warn-face)
                                         'ical:warning nondefault-warn-face)
                                        extra-faces
                                        link)
  "Define iCalendar property PROPERTY-NAME under SYMBOLIC-NAME.
PROPERTY-NAME should be the property name as it should appear in
iCalendar data.

VALUE should either be a symbol for a value type defined with
`icalendar-define-type', or an `rx' regular expression.  If it is
a type symbol, the regex, reader and printer functions associated
with that type will be used when parsing and serializing the
property's value.  If it is a regular expression, it is assumed
that the values are strings of type `icalendar-text' which match
that regular expression.

An `rx' regular expression named SYMBOLIC-NAME is defined to
match the property:
  Group 1 of this regex matches PROPERTY-NAME.
  Group 2 matches VALUE.
  Group 3, if matched, contains any property value which does
   *not* match VALUE, and is incorrect according to the standard.
  Group 4, if matched, contains the (unparsed) property parameters;
   its boundaries can be used for parsing these.

This regex matches the entire string representing this property,
from the beginning of the content line to the end of its value.
Another regular expression named `SYMBOLIC-NAME-value' is also
defined to match just the value part, after the separating colon,
with groups 2 and 3 as above.

A function to match the complete property expression called
`icalendar-match-PROPERTY-NAME-property' is defined.  This
function is used to provide syntax highlighting in
`icalendar-mode'.

See the functions `icalendar-read-property-value',
`icalendar-parse-property-value', `icalendar-parse-property', and
`icalendar-print-property-node' to convert properties defined
with this macro to and from their text representation in
iCalendar format.

The following keyword arguments are accepted:

:default - a (string representing the) default value, if
  the property is not specified in a given component.

:unrecognized - a (string representing the) value which must be
  substituted for values that are not recognized but
  syntactically correct according to RFC5545.  Unrecognized values
  must be in match group 5 of the regex determined by VALUE.  An
  unrecognized value will be preserved in the syntax tree
  metadata and printed instead of this value when the node is
  printed.  Defaults to any value specified for :default.

:default-type - a type symbol naming the default type of the
  property's value.  If the property's value differs from this
  type, an `icalendar-valuetypeparam' parameter will be added to
  the property's syntax node and printed when the node is
  printed.  Default is VALUE if VALUE is a value type symbol,
  otherwise the type `icalendar-text'.

:other-types - a list of type symbols naming value types other
  than :default-type.  These represent alternative types for the
  property's value.  If parsing the property's value under its
  default type fails, these types will be tried in turn, and only
  if the property's value matches none of them will an error be
  signaled.

:list-sep - if the property accepts a list of values, this should
  be a string which separates the values (typically \",\").  If
  :list-sep is non-nil, the value string will first be split on
  the separator, then each value will be read according to VALUE
  and collected into a list when parsing.  When printing, the
  inverse happens: values are printed individually and then
  joined with :list-sep.  Passing this argument marks
  SYMBOLIC-NAME as a type that accepts a list of values for
  `icalendar-expects-list-of-values-p'.

:child-spec - a plist mapping the following keywords to lists
of type symbols:
  :one           - parameters that must appear exactly once
  :one-or-more   - parameters that must appear at least once and
                   may appear more than once
  :zero-or-one   - parameters that must appear at most once
  :zero-or-more  - parameters that may appear more than once
  :allow-others  - if non-nil, other parameters besides those listed in
                   the above are allowed to appear.  (In this case, a
                   :zero-or-more clause is redundant.)

:other-validator - a function to perform any additional validation of
  the component, beyond what `icalendar-ast-node-valid-p' checks.
  This function should accept one argument, a syntax node.  It
  should return non-nil if the node is valid, or signal an
  `icalendar-validation-error' if it is not.  Its name does not
  need to be quoted.

:name-face - a face symbol for highlighting the property name
  (default: `icalendar-property-name')

:value-face - a face symbol for highlighting valid property values
  (default: `icalendar-property-value')

:warn-face - a face symbol for highlighting invalid property values
  (default: `icalendar-warning')

:extra-faces - a list of the form for HIGHLIGHT in `font-lock-keywords'.
  In particular, ((GROUPNUM FACENAME [OVERRIDE [LAXMATCH]])...)
  can be used to apply different faces to different match subgroups.

:link - a string containing a URL for documentation of this property"
  (declare (doc-string 2))
  (let* (;; Value RX:
        (full-value-rx-name
         (intern (concat (symbol-name symbolic-name) "-property-value")))
        (values-rx (when list-sep
                    `(seq ,value (zero-or-more ,list-sep ,value))))
        ;; Related functions:
        (property-dname (if property-name
                            (downcase property-name)
                          (string-trim (symbol-name symbolic-name)
                                       "icalendar-" "-property")))
        (matcher-name
         (intern (concat "icalendar-match-" property-dname "-property")))
        (type-predicate-name
         (intern (concat "icalendar-" property-dname "-property-p")))
        ;; Faces:
        (has-faces (or nondefault-name-face nondefault-value-face
                       nondefault-warn-face extra-faces))
        ;; Documentation:
        (header "It names a property type defined by `icalendar-define-property'.")
        (val-list (if list-sep (concat "VAL1" list-sep "VAL2" list-sep "...")
                    "VAL"))
        (default-doc (if default (format "The default value is: \"%s\"\n" default)
                       ""))
        (s (if list-sep "s" "")) ; to make plurals
        (val-doc (concat "VAL" s " "
                         "must be " (unless list-sep "a ")
                         (format "value%s of one of the following types:\n" s)
                         (string-join
                          (cons
                           (format "`%s' (default)" default-type)
                           (mapcar (lambda (type) (format "`%s'" type))
                                   other-types))
                          "\n")
                         default-doc))
        (name-doc (if property-name "" "NAME must match rx `icalendar-name'"))
        (syntax-doc (format "Syntax: %s[;PARAM...]:%s\n%s\n%s\n"
                            (or property-name "NAME") val-list name-doc val-doc))
        (child-doc
         (concat
          "The following parameters are required or allowed\n"
          "as children in syntax nodes of this type:\n\n"
          (ical:format-child-spec child-spec)
          (when (plist-get child-spec :allow-others)
            "\nOther parameters of any type are also allowed.\n")))
        (full-doc (concat header "\n\n" doc "\n\n" syntax-doc "\n\n" child-doc)))

    `(progn
       ;; Type metadata needs to be available at both compile time and
       ;; run time.  In particular, `ical:value-type-symbol-p' needs to
       ;; work at compile time.
       (eval-and-compile
         (setplist (quote ,symbolic-name)
                   (list
                    'ical:is-type t
                    'ical:is-property t
                    'ical:matcher (function ,matcher-name)
                    'ical:default-value ,default
                    'ical:default-type (quote ,default-type)
                    'ical:other-types (quote ,other-types)
                    'ical:list-sep ,list-sep
                    'ical:substitute-value ,unrecognized
                    'ical:value-type
                    (when (ical:value-type-symbol-p (quote ,value))
                      (quote ,value))
                    'ical:value-rx (quote ,value)
                    'ical:values-rx (quote ,values-rx)
                    'ical:full-value-rx (quote ,full-value-rx-name)
                    'ical:child-spec (quote ,child-spec)
                    'ical:other-validator (function ,other-validator)
                    'ical:type-documentation ,full-doc
                    'ical:link ,link)))

       ;; Value regex which matches:
       ;; Group 2: correct values of the property, and
       ;; Group 3: incorrect values up to end-of-line (for syntax warnings)
       (rx-define ,full-value-rx-name
         (or (group-n 2 ,(or values-rx value))
             (group-n 3 (zero-or-more not-newline))))

       ;; Full property regex which matches:
       ;; Group 1: the property name,
       ;; Group 2: correct values of the property, and
       ;; Group 3: incorrect values up to end-of-line (for syntax warnings)
       (rx-define ,symbolic-name
         (seq line-start
              (group-n 1 ,(or property-name 'ical:name))
              (group-n 4 (zero-or-more ical:other-param-safe))
              ":"
              ,full-value-rx-name
              line-end))

       ;; Matcher:
       (defun ,matcher-name (limit)
         ,(concat (format "Matcher for `%s' property.\n" symbolic-name)
                  "(Defined by icalendar-define-property.)")
         (re-search-forward (rx ,symbolic-name) limit t))

       ;; CL-type to represent syntax nodes for this property:
       (defun ,type-predicate-name (node)
         ,(format "Return non-nil if NODE represents a %s property." property-name)
         (and (ical:ast-node-p node)
              (eq (ical:ast-node-type node) (quote ,symbolic-name))))

       (cl-deftype ,symbolic-name () '(satisfies ,type-predicate-name))

       ;; Associate the print name with the type symbol for
       ;; `icalendar-parse-property', `icalendar-print-property-node', etc.:
       (when ,property-name
         (push (cons ,property-name (quote ,symbolic-name)) ical:property-types))

       ;; Generate an entry for font-lock-keywords in icalendar-mode:
       (when ,has-faces
         ;; Avoid circular load of icalendar-mode.el in
         ;; icalendar-parser.el (which does not use the *-face
         ;; keywords), while still allowing external code to add to
         ;; font-lock-keywords dynamically:
         (require 'icalendar-mode)
         (push (quote (,matcher-name
                       (1 (quote ,name-face) t t)
                       (2 (quote ,value-face) t t)
                       (3 (quote ,warn-face) t t)
                       ,@extra-faces))
               ical:font-lock-keywords)))))


;; Define components:
(cl-defmacro ical:define-component (symbolic-name component-name doc
                                    &key
                                    ((:keyword-face keyword-face)
                                     'ical:keyword nondefault-keyword-face)
                                    ((:name-face name-face)
                                     'ical:component-name nondefault-name-face)
                                    child-spec
                                    other-validator
                                    link)
  "Define iCalendar component COMPONENT-NAME under SYMBOLIC-NAME.
COMPONENT-NAME should be the name of the component as it should
appear in iCalendar data.

Regular expressions to match the component boundaries are defined
named `COMPONENT-NAME-begin' and `COMPONENT-NAME-end' (or
`OTHER-begin' and `OTHER-end', where `OTHER' is derived from
SYMBOLIC-NAME by removing any prefix `icalendar-' and suffix
`-component' if COMPONENT-NAME is nil).
  Group 1 of these regexes matches the \"BEGIN\" or \"END\"
    keyword that marks a component boundary.
  Group 2 matches the component name.

A function to match the component boundaries is defined called
`icalendar-match-COMPONENT-NAME-component' (or
`icalendar-match-OTHER-component', with OTHER as above).  This
function is used to provide syntax highlighting in
`icalendar-mode'.

The following keyword arguments are accepted:

:child-spec - a plist mapping the following keywords to lists
of type symbols:
  :one           - properties or components that must appear exactly once
  :one-or-more   - properties or components that must appear at least once and
                   may appear more than once
  :zero-or-one   - properties or components that must appear at most once
  :zero-or-more  - properties or components that may appear more than once
  :allow-others  - if non-nil, other children besides those listed in the above
                   are allowed to appear.  (In this case, a :zero-or-more
                   clause is redundant.)

:other-validator - a function to perform any additional validation of
  the component, beyond what `icalendar-ast-node-valid-p' checks.
  This function should accept one argument, a syntax node.  It
  should return non-nil if the node is valid, or signal an
  `icalendar-validation-error' if it is not.  Its name does not
  need to be quoted.

:keyword-face - a face symbol for highlighting the BEGIN/END keyword
  (default: `icalendar-keyword')

:name-face - a face symbol for highlighting the component name
  (default: `icalendar-component-name')

:link - a string containing a URL for documentation of this component"
  (declare (doc-string 2))
  (let* (;; Regexes:
         (name-rx (or component-name 'ical:name))
         (component-dname (if component-name
                              (downcase component-name)
                            (string-trim (symbol-name symbolic-name)
                                         "icalendar-" "-component")))
         (begin-rx-name (intern (concat "icalendar-" component-dname "-begin")))
         (end-rx-name (intern (concat "icalendar-" component-dname "-end")))
         ;; Related functions:
         (matcher-name
          (intern (concat "icalendar-match-" component-dname "-component")))
         (type-predicate-name
          (intern (concat "icalendar-" component-dname "-component-p")))
         ;; Faces:
         (has-faces (or nondefault-name-face nondefault-keyword-face))
         ;; Documentation:
         (header "It names a component type defined by
`icalendar-define-component'.")
         (name-doc (if (not component-name)
                       "\nNAME must match rx `icalendar-name'"
                     ""))
         (syntax-doc (format "Syntax:\nBEGIN:%s\n[contentline ...]\nEND:%1$s%s"
                             (or component-name "NAME")
                             name-doc))
         (child-doc
          (concat
           "The following properties and components are required or "
           "allowed\nas children in syntax nodes of this type:\n\n"
           (ical:format-child-spec child-spec)
           (when (plist-get child-spec :allow-others)
             "\nOther properties and components of any type are also allowed.\n")))
         (full-doc (concat header "\n\n" doc "\n\n" syntax-doc "\n\n" child-doc)))

    `(progn
       ;; Type metadata needs to be available at both compile time and
       ;; run time.  In particular, `ical:value-type-symbol-p' needs to
       ;; work at compile time.
       (eval-and-compile
         (setplist (quote ,symbolic-name)
                   (list
                    'ical:is-type t
                    'ical:is-component t
                    'ical:matcher (function ,matcher-name)
                    'ical:begin-rx (quote ,begin-rx-name)
                    'ical:end-rx (quote ,end-rx-name)
                    'ical:child-spec (quote ,child-spec)
                    'ical:other-validator (function ,other-validator)
                    'ical:type-documentation ,full-doc
                    'ical:link ,link)))

       ;; Regexes which match:
       ;; Group 1: BEGIN or END, and
       ;; Group 2: the component name
       (rx-define ,begin-rx-name
         (seq line-start
              (group-n 1 "BEGIN")
              ":"
              (group-n 2 ,name-rx)
              line-end))

       (rx-define ,end-rx-name
         (seq line-start
              (group-n 1  "END")
              ":"
              (group-n 2 ,name-rx)
              line-end))

       (defun ,matcher-name (limit)
         ,(concat (format "Matcher for %s component boundaries.\n"
                          (or component-name "unrecognized"))
                  "(Defined by `icalendar-define-component'.)")
           (re-search-forward (rx (or ,begin-rx-name ,end-rx-name)) limit t))

       ;; CL-type to represent syntax nodes for this component:
       (defun ,type-predicate-name (node)
         ,(format "Return non-nil if NODE represents a %s component."
                  (or component-name "unrecognized"))
         (and (ical:ast-node-p node)
              (eq (ical:ast-node-type node) (quote ,symbolic-name))))

       (cl-deftype ,symbolic-name () '(satisfies ,type-predicate-name))

       ;; Generate an entry for font-lock-keywords in icalendar-mode:
       (when ,has-faces
         ;; Avoid circular load of icalendar-mode.el in
         ;; icalendar-parser.el (which does not use the *-face
         ;; keywords), while still allowing external code to add to
         ;; font-lock-keywords dynamically:
         (require 'icalendar-mode)
         (push (quote (,matcher-name
                       (1 (quote ,keyword-face) t t)
                       (2 (quote ,name-face) t t)))
               ical:font-lock-keywords))

       ;; Associate the print name with the type symbol for
       ;; `icalendar-parse-component', `icalendar-print-component' etc.:
       (when ,component-name
         (push (cons ,component-name (quote ,symbolic-name))
               ical:component-types)))))


;; Macros for destructuring and binding AST nodes

(defmacro ical:with-node-children (node bindings &rest body)
  "Execute BODY with BINDINGS to children in NODE.
NODE should be an iCalendar syntax node representing a component or
property.

Each binding in BINDINGS should be a list of one of the following forms:

\(TYPE VAR)
  TYPE should be a type symbol for an iCalendar property or component
  which can be a child of COMPONENT.  The first child node of TYPE, if
  any, will be bound to VAR in BODY.

\(TYPE KEY1 VAR1 ...)
  For each KEY present, the corresponding VAR will be bound as follows:
   :all - a list of all child nodes of TYPE.  If this keyword is present,
     none of the others are allowed.
   :first - the first child node of TYPE
   :default - the default value, if any, for TYPE
   :value-node - the value of the node in :first
   :value-type - the type of the node in :value-node (if it is a node).
   :value - the value of the node in :value-node, if it is a node,
     or :value-node itself, if it is not.
  If TYPE expects a list of values, you should use the following keywords
  instead of the previous three:
   :value-nodes - the values of the node in :first
   :value-types - a list of the types of the nodes in :value-nodes.
   :values - a list of the values of the nodes in :value-nodes (if they are
     nodes), or the :value-nodes themselves (if they are not).
  It is a compile-time error to use the singular keywords with a TYPE that
  takes multiple values, or the plural keywords with a TYPE that does not."
  (declare (debug (form form &rest form))
           (indent 2))
  ;; Static checks on the bindings prevent various annoying bugs:
  (dolist (b bindings)
    (let ((type (car b))
          (kwargs (cdr b)))
      (unless (ical:type-symbol-p type)
        (error "Not an iCalendar type symbol: %s" type))
      (when (and (plist-member kwargs :all)
                 (> 2 (length kwargs)))
        (error ":all may not be combined with other bindings"))
      (if (ical:expects-list-of-values-p type)
            (when (or (plist-member kwargs :value-node)
                      (plist-member kwargs :value-type)
                      (plist-member kwargs :value))
              (error "Type `%s' expects a list of values" type))
        (when (or (plist-member kwargs :value-nodes)
                  (plist-member kwargs :value-types)
                  (plist-member kwargs :values))
              (error "Type `%s' does not expect a list of values" type)))))

  (let ((nd (gensym "icalendar-node")))
    `(let* ((,nd ,node)
            ,@(mapcan
               (lambda (tv)
                 (let ((type (car tv))
                       (vars (cdr tv)))
                   (when (and (symbolp (car vars)) (null (cdr vars)))
                     ;; the simple (TYPE VAR) case:
                     (setq vars (list :first (car vars))))

                   (let ((first-var (or (plist-get vars :first)
                                        (gensym "first")))
                         (default-var (or (plist-get vars :default)
                                          (gensym "default")))
                         (vnode-var (or (plist-get vars :value-node)
                                        (gensym "value-node")))
                         (vtype-var (or (plist-get vars :value-type)
                                        (gensym "value-type")))
                         (vval-var (or (plist-get vars :value)
                                       (gensym "value")))

                         (vnodes-var (or (plist-get vars :value-nodes)
                                         (gensym "value-nodes")))
                         (vtypes-var (or (plist-get vars :value-types)
                                         (gensym "value-types")))
                         (vvals-var (or (plist-get vars :values)
                                        (gensym "values")))

                         (all-var (or (plist-get vars :all)
                                      (gensym "all")))
                         ;; The corresponding vars for :all are mostly
                         ;; too complicated to be useful, I think, so
                         ;; not implementing them for now.
                         ;; TODO: but it *would* be helpful to have an
                         ;; :all-values clause especially for RDATE and
                         ;; EXDATE, since they both accept lists, and
                         ;; can also occur multiple times.
                         ;; I've found myself needing to write
                         ;; (mapcar #'ical:ast-node-value
                         ;;   (apply #'append
                         ;;     (mapcar #'ical:ast-node-value rdate-nodes))
                         ;; a bit too often.
                         )
                     (delq nil
                           (list
                            (when (plist-member vars :all)
                              `(,all-var (ical:ast-node-children-of
                                          (quote ,type) ,nd)))
                            (when (not (plist-member vars :all))
                              `(,first-var (ical:ast-node-first-child-of
                                            (quote ,type) ,nd)))
                            (when (plist-member vars :default)
                              `(,default-var (get (quote ,type)
                                                  'ical:default-value)))
                            ;; Single value:
                            (when (or (plist-member vars :value-node)
                                      (plist-member vars :value-type)
                                      (plist-member vars :value))
                              `(,vnode-var (when (ical:ast-node-p ,first-var)
                                             (ical:ast-node-value ,first-var))))
                            (when (plist-member vars :value-type)
                              `(,vtype-var
                                (when ,vnode-var
                                  (ical:ast-node-type ,vnode-var))))
                            (when (plist-member vars :value)
                              `(,vval-var
                                (when ,vnode-var
                                  (if (ical:ast-node-p ,vnode-var)
                                      (ical:ast-node-value ,vnode-var)
                                    ,vnode-var))))

                            ;; List of values:
                            (when (or (plist-member vars :value-nodes)
                                      (plist-member vars :value-types)
                                      (plist-member vars :values))
                              `(,vnodes-var
                                (when (ical:ast-node-p ,first-var)
                                  (ical:ast-node-value ,first-var))))
                            (when (plist-member vars :value-types)
                              `(,vtypes-var
                                (when ,vnodes-var
                                  (mapcar #'ical:ast-node-type ,vnodes-var))))
                            (when (plist-member vars :values)
                              `(,vvals-var
                                (when ,vnodes-var
                                  (if (ical:ast-node-p (car ,vnodes-var))
                                      (mapcar #'ical:ast-node-value
                                              ,vnodes-var)
                                    ,vnodes-var)))))))))

               bindings))
       ,@body)))

(defalias 'ical:with-component #'ical:with-node-children
    "Execute BODY with properties of NODE bound as in BINDINGS.

NODE should be an iCalendar syntax node representing an iCalendar
component: `icalendar-vevent', `icalendar-vtodo', `icalendar-vjournal',
`icalendar-vtimezone', `icalendar-vfreebusy', `icalendar-standard',
`icalendar-daylight'.  It may also be an entire `icalendar-vcalendar'.

Each binding in BINDINGS should be a list of one of the following forms:

(TYPE VAR)
  TYPE should be a type symbol for an iCalendar property or component
  which can be a child of COMPONENT.  The first child node of TYPE, if
  any, will be bound to VAR in BODY.

(TYPE KEY1 VAR1 ...)
  For each KEY present, the corresponding VAR will be bound as follows:
   :all - a list of all child nodes of TYPE.  If this keyword is present,
     none of the others are allowed.
   :default - the default value, if any, for TYPE
   :first - the first child node of TYPE
   :value-node - the value (which is itself a node) of the node in :first
   :value-type - the type of the node in :value-node.
   :value - the value of the node in :value-node.
  If TYPE expects a list of values, you should use the following keywords
  instead of the previous three:
   :value-nodes - the values (which are themselves nodes) of the node in :first
   :value-types - a list of the types of the nodes in :value-nodes.
   :values - a list of the values of the node in :value-node.
  It is a compile-time error to use the singular keywords with a TYPE that
  takes multiple values, or the plural keywords with a TYPE that does not.")

(defmacro ical:with-node-value (node &optional bindings &rest body)
  "Execute BODY with bindings in BINDINGS taken from NODE and its children.

NODE should be an iCalendar syntax node representing a property or
parameter.  If NODE is not a syntax node, this form evalutes to nil
without binding the variables in BINDINGS and without executing BODY.

Within BODY, if NODE's value is itself a syntax node, the symbol
`value-node' will be bound to the syntax node for NODE's value,
`value-type' will be bound to `value-node's type, and `value' will be
bound to `value-node's value.

If NODE's value is a list of syntax nodes, then within BODY,
`value-nodes' will be bound to those value nodes, `value-types' will be
bound to a list of their types, and `values' will be bound to their
values.

If NODE's value is not a syntax node, then `value' is instead bound
directly to NODE's value, and `value-type' and `value-node' are bound to
nil.

If BODY is nil, it is assumed to be the symbol `value'; thus
  (icalendar-with-node-value some-node)
is equivalent to
  (icalendar-with-node-value some-node nil value)

BINDINGS are passed on to `icalendar-with-node-children' and will be
available in BODY; see its docstring for their form."
  (declare (debug (form &optional form &rest form))
           (indent 2))
  (let ((vn (gensym "icalendar-node"))
        (val (gensym "icalendar-value"))
        (is-list (gensym "is-list")))
    `(let ((,vn ,node))
       (when (ical:ast-node-p ,vn)
         (let* ((,val (ical:ast-node-value ,vn))
                (value-node (when (ical:ast-node-p ,val) ,val))
                (value-type (when (ical:ast-node-p value-node)
                              (ical:ast-node-type value-node)))
                (value (if (ical:ast-node-p value-node)
                           (ical:ast-node-value value-node)
                         ,val))
                (,is-list (ical:expects-list-of-values-p (ical:ast-node-type ,vn)))
                (value-nodes (when ,is-list
                               (seq-filter #'ical:ast-node-p ,val)))
                (value-types (when ,is-list
                               (mapcar #'ical:ast-node-type value-nodes)))
                (values (when ,is-list
                          (mapcar #'ical:ast-node-value value-nodes))))
           (ignore value-type ; Silence the byte compiler when
                   value      ; one of these goes unused
                   value-types
                   values)
           (ical:with-node-children ,vn ,bindings ,@(or body (list 'value))))))))

(defalias 'ical:with-property #'ical:with-node-value
    "Execute BODY with BINDINGS taken from the value and parameters in NODE.

NODE should be an iCalendar syntax node representing a property.  If NODE
is not a syntax node, this form evalutes to nil without binding the
variables in BINDINGS and without executing BODY.

Within BODY, if NODE's value is itself a syntax node, the symbol
`value-node' will be bound to the syntax node for NODE's value,
`value-type' will be bound to `value-node's type, and `value' will be
bound to `value-node's value.

If NODE's value is a list of syntax nodes, then within BODY,
`value-nodes' will be bound to those value nodes, `value-types' will be
bound to a list of their types, and `values' will be bound to their
values.

If NODE's value is not a syntax node, then `value' is bound directly to
NODE's value, and `value-type' and `value-node' are bound to nil.

BINDINGS are passed on to `icalendar-with-node-children' and will be
available in BODY; see its docstring for their form.")

(defmacro ical:with-param (parameter &rest body)
  "Bind the value in PARAMETER and execute BODY.

PARAMETER should be an iCalendar syntax node representing a
parameter.  If PARAMETER is nil, this form evalutes to nil without
executing BODY.

Within BODY, if PARAMETER's value is a syntax node, the symbol
`value-node' will be bound to that syntax node, `value-type' will be
bound to the value node's type, and `value' will be bound to the value
node's value.

If PARAMETER's value is not a syntax node, then `value' is bound
directly to PARAMETER's value, and `value-type' and `value-node' are
bound to nil."
  (declare (debug (form &rest form))
           (indent 1))
  `(ical:with-node-value ,parameter nil ,@body))

(defmacro ical:with-child-of (node type &optional bindings &rest body)
  "Like `icalendar-with-node-value', but for the relevant node's parent.

Find the first child node of type TYPE in NODE, bind that
child node's value and any of its children in BINDINGS and execute BODY
with these bindings.  If there is no such node, this form evalutes to
nil without executing BODY.

Within BODY, the symbols `value-node', `value-type', and `value' will be
bound as in `icalendar-with-node-value'.
If BODY is nil, it is assumed to be the symbol `value'; thus
  (icalendar-with-child-of some-node some-type)
is equivalent to
  (icalendar-with-child-of some-node some-type nil value)

See `icalendar-with-node-children' for the form of BINDINGS."
  (declare (debug (form form &optional form &rest form))
           (indent 3))
  (let ((child (gensym "icalendar-node")))
    `(let ((,child (ical:ast-node-first-child-of ,type ,node)))
       (ical:with-node-value ,child ,bindings ,@body))))

(defalias 'ical:with-property-of #'ical:with-child-of
  "Like `icalendar-with-property', but for components containing that property.

Find the first property node of type TYPE in NODE and execute BODY.

Within BODY, the symbols `value-node', `value-type', and `value' will be
bound to the property's value node, type and value as in
`icalendar-with-node-value'.  If BODY is nil, it is assumed to be the
symbol `value'; thus
  (icalendar-with-property-of some-component some-type)
is equivalent to
  (icalendar-with-property-of some-component some-type nil value)

BINDINGS can be used to bind the property's parameters; see
`icalendar-with-node-children' for the form of BINDINGS.")

(defmacro ical:with-param-of (node type &rest body)
  "Like `icalendar-with-param', but for properties containing that param.

Find the first parameter node of TYPE in NODE and execute BODY.

Within BODY, the symbols `value-node', `value-type', and `value' will be
bound to the parameter's value node, type and value as in
`icalendar-with-node-value'.  If BODY is nil, it is assumed to be the
symbol `value'; thus
  (icalendar-with-param-of some-property some-type)
is equivalent to
  (icalendar-with-param-of some-property some-type nil value)"
  (declare (debug (form form &rest form))
           (indent 2))
  `(ical:with-child-of ,node ,type nil ,@body))

(provide 'icalendar-macs)
;; Local Variables:
;; read-symbol-shorthands: (("ical:" . "icalendar-"))
;; End:
;;; icalendar-macs.el ends here
