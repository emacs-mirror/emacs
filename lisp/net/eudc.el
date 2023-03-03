;;; eudc.el --- Emacs Unified Directory Client  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2023 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;;         Pavel Janík <Pavel@Janik.cz>
;; Maintainer: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: comm

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
;;    This package provides a common interface to query directory servers using
;;    different protocols such as LDAP, CCSO PH/QI or BBDB.  Queries can be
;;    made through an interactive form or inline.  Inline query strings in
;;    buffers are expanded with appropriately formatted query results
;;    (especially used to expand email addresses in message buffers).  EUDC
;;    also interfaces with the BBDB package to let you register query results
;;    into your own BBDB database.

;;; Usage:
;;    EUDC comes with an extensive documentation, please refer to it.
;;
;;    The main entry points of EUDC are:
;;      `eudc-query-form': Query a directory server from a query form
;;      `eudc-expand-inline': Query a directory server for the e-mail address
;;                            of the name before cursor and insert it in the
;;                            buffer
;;      `eudc-get-phone': Get a phone number from a directory server
;;      `eudc-get-email': Get an e-mail address from a directory server
;;      `eudc-customize': Customize various aspects of EUDC

;;; Code:

(require 'wid-edit)
(require 'cl-lib)
(require 'eudc-vars)

;;{{{      Internal cooking

;;{{{      Internal variables and compatibility tricks

(defvar eudc-form-widget-list nil)

(defvar-keymap eudc-mode-map
  :parent widget-keymap
  "q" #'kill-current-buffer
  "x" #'kill-current-buffer
  "f" #'eudc-query-form
  "b" #'eudc-try-bbdb-insert
  "n" #'eudc-move-to-next-record
  "p" #'eudc-move-to-previous-record)

(defvar mode-popup-menu)

;; List of variables that have server- or protocol-local bindings
(defvar eudc-local-vars nil)

;; Protocol local. Query function
(defvar eudc-query-function nil)

;; Protocol local.  A function that retrieves a list of valid attribute names
(defvar eudc-list-attributes-function nil)

;; Protocol local. A mapping between EUDC attribute names and corresponding
;; protocol specific names.  The following names are defined by EUDC and may be
;; included in that list: `name' , `firstname', `email', `phone'
(defvar eudc-protocol-attributes-translation-alist nil)

;; Protocol local. Mapping between protocol attribute names and BBDB field
;; names
(defvar eudc-bbdb-conversion-alist nil)

;; Protocol/Server local. Hook called upon switching to that server
(defvar eudc-switch-to-server-hook nil)

;; Protocol/Server local. Hook called upon switching from that server
(defvar eudc-switch-from-server-hook nil)

;; Protocol local. Whether the protocol supports queries with no specified
;; attribute name
(defvar eudc-protocol-has-default-query-attributes nil)

(defvar bbdb-version)

(defun eudc--using-bbdb-3-or-newer-p ()
  "Return non-nil if BBDB version is 3 or greater."
  (or
   ;; MELPA versions of BBDB may have a bad package version, but
   ;; they're all version 3 or later.
   (equal bbdb-version "@PACKAGE_VERSION@")
   ;; Development versions of BBDB can have the format "X.YZ devo".
   ;; Split the string just in case.
   (version<= "3" (car (split-string bbdb-version)))))

(defun eudc--plist-member (plist prop &optional predicate)
  "Like `plist-member', but signal on invalid PLIST."
  (or (plistp plist)
      (signal 'wrong-type-argument `(plistp ,plist)))
  (plist-member plist prop predicate))

(defun eudc-plist-member (plist prop)
  "Return t if PROP has a value specified in PLIST.
Signal an error if PLIST is not a valid property list."
  (and (eudc--plist-member plist prop) t))

;; Emacs's `plist-get' lacks a default parameter, and CL-Lib's
;; `cl-getf' doesn't accept a predicate or signal an error.
(defun eudc-plist-get (plist prop &optional default)
  "Extract the value of PROP in property list PLIST.
PLIST is a list of the form (PROP1 VALUE1 PROP2 VALUE2...).
This function returns the first value corresponding to the given
PROP, or DEFAULT if PROP is not one of the properties in the
list.  The comparison with PROP is done using `eq'.  If PLIST is
not a valid property list, this function signals an error."
  (let ((tail (eudc--plist-member plist prop)))
    (if tail (cadr tail) default)))

(defun eudc-lax-plist-get (plist prop &optional default)
  "Extract the value of PROP from lax property list PLIST.
PLIST is a list of the form (PROP1 VALUE1 PROP2 VALUE2...), where
comparisons between properties are done using `equal' instead of
`eq'.  This function returns the first value corresponding to
PROP, or DEFAULT if PROP is not one of the properties in the
list.  If PLIST is not a valid property list, this function
signals an error."
  (let ((tail (eudc--plist-member plist prop #'equal)))
    (if tail (cadr tail) default)))

(defun eudc-replace-in-string (str regexp newtext)
  "Replace all matches in STR for REGEXP with NEWTEXT.
Value is the new string."
  (let ((rtn-str "")
	(start 0)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat rtn-str
		    (substring str prev-start match)
		    newtext)))
    (concat rtn-str (substring str start))))


(defconst eudc-rfc5322-atext-token "[:alpha:][:digit:]!#$%&'*+/=?^_`{|}~-"
  "Printable US-ASCII characters not including specials.  Used for atoms.")

(defconst eudc-rfc5322-wsp-token " \t"
  "Non-folding white space.")

(defconst eudc-rfc5322-fwsp-token
  (concat eudc-rfc5322-wsp-token "\n")
  "Folding white space.")

(defconst eudc-rfc5322-cctext-token "\u005D-\u007E\u002A-\u005B\u0021-\u0027"
  "Printable US-ASCII characters not including \"(\", \")\", or \"\\\".")

(defun eudc-rfc5322-quote-phrase (string)
  "Quote STRING if it needs quoting as a phrase in a header."
  (if (string-match
       (concat "[^" eudc-rfc5322-wsp-token eudc-rfc5322-atext-token "]")
       string)
      (concat "\"" string "\"")
    string))

(defun eudc-rfc5322-valid-comment-p (string)
  "Check if STRING can be used as comment in a header."
  (if (string-match
       (concat "[^" eudc-rfc5322-cctext-token eudc-rfc5322-fwsp-token "]")
       string)
      nil
    t))

(defun eudc-rfc5322-make-address (address &optional firstname name comment)
  "Create a valid address specification according to RFC5322.
RFC5322 address specifications are used in message header fields
to indicate senders and recipients of messages.  They generally
have one of the forms:

ADDRESS
ADDRESS (COMMENT)
PHRASE <ADDRESS>
PHRASE <ADDRESS> (COMMENT)

The arguments FIRSTNAME and NAME are combined to form PHRASE.
PHRASE is enclosed in double quotes if necessary.

COMMENT is omitted if it contains any symbols outside the
permitted set `eudc-rfc5322-cctext-token'."
  (if (and address
           (not (string-blank-p address)))
      (let ((result address)
            (name-given (and name
                             (not (string-blank-p name))))
            (firstname-given (and firstname
                                  (not (string-blank-p firstname))))
            (valid-comment-given (and comment
                                      (not (string-blank-p comment))
                                      (eudc-rfc5322-valid-comment-p comment))))
        (if (or name-given firstname-given)
            (let ((phrase (string-trim (concat firstname " " name))))
              (setq result
                    (concat
                     (eudc-rfc5322-quote-phrase phrase)
                     " <" result ">"))))
        (if valid-comment-given
            (setq result
                  (concat result " (" comment ")")))
        result)
    ;; nil or empty address, nothing to return
    nil))

;;}}}

;;{{{ Server and Protocol Variable Routines

(defun eudc-server-local-variable-p (var)
  "Return non-nil if VAR has server-local bindings."
  (eudc-plist-member (get var 'eudc-locals) 'server))

(defun eudc-protocol-local-variable-p (var)
  "Return non-nil if VAR has protocol-local bindings."
  (eudc-plist-member (get var 'eudc-locals) 'protocol))

(defun eudc-default-set (var val)
  "Set the EUDC default value of VAR to VAL.
The current binding of VAR is not changed."
  (put var 'eudc-locals
       (plist-put (get var 'eudc-locals) 'default val))
  (add-to-list 'eudc-local-vars var))

(defun eudc-protocol-set (var val &optional protocol)
  "Set the PROTOCOL-local binding of VAR to VAL.
If omitted PROTOCOL defaults to the current value of `eudc-protocol'.
The current binding of VAR is changed only if PROTOCOL is omitted."
  (if (eq 'unbound (eudc-variable-default-value var))
      (eudc-default-set var (symbol-value var)))
  (let* ((eudc-locals (get var 'eudc-locals))
	 (protocol-locals (eudc-plist-get eudc-locals 'protocol)))
    (setq protocol-locals (plist-put protocol-locals (or protocol
							 eudc-protocol) val))
    (setq eudc-locals
	  (plist-put eudc-locals 'protocol protocol-locals))
    (put var 'eudc-locals eudc-locals)
    (add-to-list 'eudc-local-vars var)
    (unless protocol
      (eudc-update-variable var))))

(defun eudc-server-set (var val &optional server)
  "Set the SERVER-local binding of VAR to VAL.
If omitted SERVER defaults to the current value of `eudc-server'.
The current binding of VAR is changed only if SERVER is omitted."
  (if (eq 'unbound (eudc-variable-default-value var))
      (eudc-default-set var (symbol-value var)))
  (let* ((eudc-locals (get var 'eudc-locals))
	 (server-locals (eudc-plist-get eudc-locals 'server)))
    (setq server-locals (plist-put server-locals (or server
						     eudc-server) val))
    (setq eudc-locals
	  (plist-put eudc-locals 'server server-locals))
    (put var 'eudc-locals eudc-locals)
    (add-to-list 'eudc-local-vars var)
    (unless server
      (eudc-update-variable var))))


(defun eudc-set (var val)
  "Set the most local (server, protocol or default) binding of VAR to VAL.
The current binding of VAR is also set to VAL."
  (cond
   ((not (eq 'unbound (eudc-variable-server-value var)))
    (eudc-server-set var val))
   ((not (eq 'unbound (eudc-variable-protocol-value var)))
    (eudc-protocol-set var val))
   (t
    (eudc-default-set var val)))
  (set var val))

(defun eudc-variable-default-value (var)
  "Return the default binding of VAR.
Return `unbound' if VAR has no EUDC default value."
  (let ((eudc-locals (get var 'eudc-locals)))
    (if (and (boundp var)
	     eudc-locals)
	(eudc-plist-get eudc-locals 'default 'unbound)
      'unbound)))

(defun eudc-variable-protocol-value (var &optional protocol)
  "Return the value of VAR local to PROTOCOL.
Return `unbound' if VAR has no value local to PROTOCOL.
PROTOCOL defaults to `eudc-protocol'."
  (let* ((eudc-locals (get var 'eudc-locals))
	 protocol-locals)
    (if (not (and  (boundp var)
		   eudc-locals
		   (eudc-plist-member eudc-locals 'protocol)))
	'unbound
      (setq protocol-locals (eudc-plist-get eudc-locals 'protocol))
      (eudc-lax-plist-get protocol-locals
			  (or protocol
			      eudc-protocol) 'unbound))))

(defun eudc-variable-server-value (var &optional server)
  "Return the value of VAR local to SERVER.
Return `unbound' if VAR has no value local to SERVER.
SERVER defaults to `eudc-server'."
  (let* ((eudc-locals (get var 'eudc-locals))
	 server-locals)
    (if (not (and (boundp var)
		  eudc-locals
		  (eudc-plist-member eudc-locals 'server)))
	'unbound
      (setq server-locals (eudc-plist-get eudc-locals 'server))
      (eudc-lax-plist-get server-locals
			  (or server
			      eudc-server) 'unbound))))

(defun eudc-update-variable (var)
  "Set the value of VAR according to its locals.
If the VAR has a server- or protocol-local value corresponding
to the current `eudc-server' and `eudc-protocol' then it is set
accordingly.  Otherwise it is set to its EUDC default binding."
  (let (val)
    (cond
     ((not (eq 'unbound (setq val (eudc-variable-server-value var))))
      (set var val))
     ((not (eq 'unbound (setq val (eudc-variable-protocol-value var))))
      (set var val))
     ((not (eq 'unbound (setq val (eudc-variable-default-value var))))
      (set var val)))))

(defun eudc-update-local-variables ()
  "Update all EUDC variables according to their local settings."
  (interactive)
  (mapcar #'eudc-update-variable eudc-local-vars))

(eudc-default-set 'eudc-query-function nil)
(eudc-default-set 'eudc-list-attributes-function nil)
(eudc-default-set 'eudc-protocol-attributes-translation-alist nil)
(eudc-default-set 'eudc-bbdb-conversion-alist nil)
(eudc-default-set 'eudc-switch-to-server-hook nil)
(eudc-default-set 'eudc-switch-from-server-hook nil)
(eudc-default-set 'eudc-protocol-has-default-query-attributes nil)
(eudc-default-set 'eudc-attribute-display-method-alist nil)

;;}}}


(defun eudc-register-protocol (protocol)
  "Add PROTOCOL to the list of supported protocols."
  (unless (memq protocol eudc-supported-protocols)
    (setq eudc-supported-protocols
	  (cons protocol eudc-supported-protocols))
    (put 'eudc-protocol 'custom-type
	 `(choice :menu-tag "Protocol"
		  ,@(mapcar (lambda (s)
			      (list 'string ':tag (symbol-name s)))
			    eudc-supported-protocols))))
  (or (memq protocol eudc-known-protocols)
      (setq eudc-known-protocols
	    (cons protocol eudc-known-protocols))))


(defun eudc-translate-query (query &optional reverse)
  "Translate attribute names of QUERY.
The translation is done according to
`eudc-protocol-attributes-translation-alist'.

When REVERSE is nil or omitted, the attribute names are
translated from EUDC generic names to protocol-specific
names. When REVERSE is non-nil, the translation is from
protocol-specific names back to EUDC generic names."
  (if eudc-protocol-attributes-translation-alist
      (mapcar (lambda (attribute)
                (let ((trans
                       (if reverse
                           (rassq (car attribute)
                                  (symbol-value eudc-protocol-attributes-translation-alist))
                         (assq (car attribute)
                               (symbol-value eudc-protocol-attributes-translation-alist)))))
                  (if trans
                      (cons (if reverse (car trans) (cdr trans))
                            (cdr attribute))
                    attribute)))
	      query)
    query))

(defun eudc-translate-attribute-list (list &optional reverse)
  "Translate a list of attribute names LIST.
The translation is done according to
`eudc-protocol-attributes-translation-alist'.

When REVERSE is nil or omitted, the attribute names are
translated from EUDC generic names to protocol-specific
names. When REVERSE is non-nil, the translation is from
protocol-specific names back to EUDC generic names."
  (if eudc-protocol-attributes-translation-alist
      (let (trans)
	(mapcar (lambda (attribute)
		  (setq trans
                        (if reverse
                            (rassq attribute
				   (symbol-value eudc-protocol-attributes-translation-alist))
                          (assq attribute
				(symbol-value eudc-protocol-attributes-translation-alist))))
		  (if trans
		      (if reverse (car trans) (cdr trans))
		    attribute))
		list))
    list))

(defun eudc-select (choices beg end)
  "Choose one from CHOICES using a completion.
BEG and END delimit the text which is to be replaced."
  (let ((replacement))
   (setq replacement
	 (completing-read "Multiple matches found; choose one: "
			  (mapcar #'list choices)))
   (delete-region beg end)
   (insert replacement)))

(defun eudc-query (query &optional return-attributes no-translation)
   "Query the current directory server with QUERY.
QUERY is a list of cons cells (ATTR . VALUE) where ATTR is an attribute
name and VALUE the corresponding value.
If NO-TRANSLATION is non-nil, ATTR is translated according to
`eudc-protocol-attributes-translation-alist'.
RETURN-ATTRIBUTES is a list of attributes to return defaulting to
`eudc-default-return-attributes'."
   (unless eudc-query-function
     (error "Don't know how to perform the query"))
   (if no-translation
       (funcall eudc-query-function query (or return-attributes
					      eudc-default-return-attributes))

     (funcall eudc-query-function
	      (eudc-translate-query query)
	      (cond
	       (return-attributes
		(eudc-translate-attribute-list return-attributes))
	       ((listp eudc-default-return-attributes)
		(eudc-translate-attribute-list eudc-default-return-attributes))
	       (t
		eudc-default-return-attributes)))))

(defun eudc-format-attribute-name-for-display (attribute)
  "Format a directory attribute name for display.
ATTRIBUTE is looked up in `eudc-user-attribute-names-alist' and replaced
by the corresponding user name if any.  Otherwise it is capitalized and
underscore characters are replaced by spaces."
  (let ((match (assq attribute eudc-user-attribute-names-alist)))
    (if match
	(cdr match)
      (capitalize
       (mapconcat #'identity
		  (split-string (symbol-name attribute) "_")
		  " ")))))

(defun eudc-print-attribute-value (field)
  "Insert the value of the directory FIELD at point.
The directory attribute name in car of FIELD is looked up in
`eudc-attribute-display-method-alist' and the corresponding method,
if any, is called to print the value in cdr of FIELD."
  (let ((match (assoc (downcase (car field))
		      eudc-attribute-display-method-alist))
	(col (current-column))
	(val (cdr field)))
    (if match
	(progn
	  (funcall (cdr match) val)
	  (insert "\n"))
      (mapc
       (lambda (val-elem)
         (indent-to col)
         (insert val-elem "\n"))
       (cond
	((listp val) val)
	((stringp val) (split-string val "\n"))
	((null val) '(""))
	(t (list val)))))))

(defun eudc-print-record-field (field column-width)
  "Print the record field FIELD.
FIELD is a list (ATTR VALUE1 VALUE2 ...) or cons-cell (ATTR . VAL)
COLUMN-WIDTH is the width of the first display column containing the
attribute name ATTR."
  (let ((field-beg (point)))
;; The record field that is passed to this function has already been processed
;; by `eudc-format-attribute-name-for-display' so we don't need to call it
;; again to display the attribute name
    (insert (format (concat "%" (int-to-string column-width) "s: ")
		    (car field)))
    (put-text-property field-beg (point) 'face 'bold)
    (indent-to (+ 2 column-width))
    (eudc-print-attribute-value field)))

(defun eudc-display-records (records &optional raw-attr-names)
  "Display the record list RECORDS in a formatted buffer.
If RAW-ATTR-NAMES is non-nil, the raw attribute names are displayed
otherwise they are formatted according to `eudc-user-attribute-names-alist'."
  (let (inhibit-read-only
	precords
	(width 0)
	beg
	first-record
	attribute-name)
    (with-output-to-temp-buffer "*Directory Query Results*"
      (with-current-buffer standard-output
	(setq buffer-read-only t)
	(setq inhibit-read-only t)
	(erase-buffer)
	(insert "Directory Query Result\n")
	(insert "======================\n\n\n")
	(if (null records)
	    (insert "No match found.\n"
		    (if eudc-strict-return-matches
			"Try setting `eudc-strict-return-matches' to nil or change `eudc-default-return-attributes'.\n"
		      ""))
	  ;; Replace field names with user names, compute max width
	  (setq precords
		(mapcar
                 (lambda (record)
                   (mapcar
                    (lambda (field)
                      (setq attribute-name
                            (if raw-attr-names
                                (symbol-name (car field))
                              (eudc-format-attribute-name-for-display (car field))))
                      (if (> (length attribute-name) width)
                          (setq width (length attribute-name)))
                      (cons attribute-name (cdr field)))
                    record))
		 records))
	  ;; Display the records
	  (setq first-record (point))
	  (mapc
           (lambda (record)
             (setq beg (point))
             ;; Map over the record fields to print the attribute/value pairs
             (mapc (lambda (field)
                     (eudc-print-record-field field width))
                   record)
             ;; Store the record internal format in some convenient place
             (overlay-put (make-overlay beg (point))
                          'eudc-record
                          (car records))
             (setq records (cdr records))
             (insert "\n"))
	   precords))
	(insert "\n")
	(widget-create 'push-button
		       :notify (lambda (&rest _ignore)
				 (eudc-query-form))
		       "New query")
	(widget-insert " ")
	(widget-create 'push-button
		       :notify (lambda (&rest _ignore)
				 (kill-this-buffer))
		       "Quit")
	(eudc-mode)
	(widget-setup)
	(if first-record
	    (goto-char first-record))))))

(defun eudc-process-form ()
  "Process the query form in current buffer and display the results."
  (let (query-alist
	value)
    (if (not (and (boundp 'eudc-form-widget-list)
		  eudc-form-widget-list))
	(error "Not in a directory query form buffer")
      (mapc (lambda (wid-field)
              (setq value (widget-value (cdr wid-field)))
              (if (not (string= value ""))
                  (setq query-alist (cons (cons (car wid-field) value)
                                          query-alist))))
	    eudc-form-widget-list)
      (kill-buffer (current-buffer))
      (eudc-display-records (eudc-query query-alist) eudc-use-raw-directory-names))))


(defun eudc-filter-duplicate-attributes (record)
  "Filter RECORD according to `eudc-duplicate-attribute-handling-method'."
  (let ((rec record)
	unique
	duplicates
	result)

    ;; Search for multiple records
    (while (and rec
		(not (listp (cdar rec))))
      (setq rec (cdr rec)))

    (if (null (cdar rec))
	(list record)			; No duplicate attrs in this record
      (mapc (lambda (field)
              (if (listp (cdr field))
                  (setq duplicates (cons field duplicates))
                (setq unique (cons field unique))))
	    record)
      (setq result (list unique))
      ;; Map over the record fields that have multiple values
      (mapc
       (lambda (field)
         (let ((method (if (consp eudc-duplicate-attribute-handling-method)
                           (cdr
                            (assq
                             (or
                              (car
                               (rassq
                                (car field)
                                (symbol-value
                                 eudc-protocol-attributes-translation-alist)))
                              (car field))
                             eudc-duplicate-attribute-handling-method))
                         eudc-duplicate-attribute-handling-method)))
           (cond
            ((or (null method) (eq 'list method))
             (setq result
                   (eudc-add-field-to-records field result)))
            ((eq 'first method)
             (setq result
                   (eudc-add-field-to-records (cons (car field)
                                                (cadr field))
                                          result)))
            ((eq 'concat method)
             (setq result
                   (eudc-add-field-to-records (cons (car field)
                                                (mapconcat
                                                 #'identity
                                                 (cdr field)
                                                 "\n"))
                                          result)))
            ((eq 'duplicate method)
             (setq result
                   (eudc-distribute-field-on-records field result))))))
       duplicates)
      result)))

(defun eudc-filter-partial-records (records attrs)
  "Eliminate records that do not contain all ATTRS from RECORDS."
  (delq nil
	(mapcar
         (lambda (rec)
           (if (cl-every (lambda (attr)
                           (consp (assq attr rec)))
                         attrs)
               rec))
	 records)))

(defun eudc-add-field-to-records (field records)
  "Add FIELD to each individual record in RECORDS and return the resulting list."
  (mapcar (lambda (r)
            (cons field r))
	  records))

(defun eudc-distribute-field-on-records (field records)
  "Duplicate each individual record in RECORDS according to value of FIELD.
Each copy is added a new field containing one of the values of FIELD."
  (let (result)
    (dolist (value (delete-dups (cdr field))) ;; Uniquify values first.
      (setq result (nconc (eudc-add-field-to-records
			   (cons (car field) value)
			   records)
                          result)))
    result))

(define-derived-mode eudc-mode special-mode "EUDC"
  "Major mode used in buffers displaying the results of directory queries.
There is no sense in calling this command from a buffer other than
one containing the results of a directory query.

These are the special commands of EUDC mode:
    q -- Kill this buffer.
    f -- Display a form to query the current directory server.
    n -- Move to next record.
    p -- Move to previous record.
    b -- Insert record at point into the BBDB database."
  (easy-menu-define eudc-emacs-menu eudc-mode-map "" (eudc-menu)))

;;}}}

;;{{{      High-level interfaces (interactive functions)

(defun eudc-customize ()
  "Customize the EUDC package."
  (interactive)
  (customize-group 'eudc))

;;;###autoload
(defun eudc-set-server (server protocol &optional no-save)
  "Set the directory server to SERVER using PROTOCOL.
Unless NO-SAVE is non-nil, the server is saved as the default
server for future sessions."
  (interactive (list
		(read-from-minibuffer "Directory Server: ")
		(intern (completing-read "Protocol: "
					 (mapcar (lambda (elt)
						    (cons (symbol-name elt)
							  elt))
						 eudc-known-protocols)))))
  (unless (or (null protocol)
	      (member protocol
		      eudc-supported-protocols)
	      (load (concat "eudcb-" (symbol-name protocol)) t))
    (error "Unsupported protocol: %s" protocol))
  (run-hooks 'eudc-switch-from-server-hook)
  (setq eudc-protocol protocol)
  (setq eudc-server server)
  (eudc-update-local-variables)
  (run-hooks 'eudc-switch-to-server-hook)
  (if (called-interactively-p 'interactive)
      (message "Current directory server is now %s (%s)" eudc-server eudc-protocol))
  (if (null no-save)
      (when (not eudc-ignore-options-file)
	(eudc-save-options))))

;;;###autoload
(defun eudc-get-email (name &optional error)
  "Get the email field of NAME from the directory server.
If ERROR is non-nil, report an error if there is none."
  (interactive "sSurname: \np")
  (or eudc-server
      (call-interactively 'eudc-set-server))
  (let ((result (eudc-query (list (cons 'name name)) '(email)))
	email)
    (if (null (cdr result))
        (setq email (cdaar result))
      (error "Multiple match--use the query form"))
    (if error
	(if email
	    (message "%s" email)
	  (error "No record matching %s" name)))
    email))

;;;###autoload
(defun eudc-get-phone (name &optional error)
  "Get the phone field of NAME from the directory server.
If ERROR is non-nil, report an error if there is none."
  (interactive "sSurname: \np")
  (or eudc-server
      (call-interactively 'eudc-set-server))
  (let ((result (eudc-query (list (cons 'name name)) '(phone)))
	phone)
    (if (null (cdr result))
        (setq phone (cdaar result))
      (error "Multiple match--use the query form"))
    (if error
	(if phone
	    (message "%s" phone)
	  (error "No record matching %s" name)))
    phone))

(defun eudc-get-attribute-list ()
  "Return a list of valid attributes for the current server.
When called interactively the list is formatted in a dedicated buffer
otherwise a list of symbols is returned."
  (interactive)
  (if eudc-list-attributes-function
      (let ((entries (funcall eudc-list-attributes-function
			      (called-interactively-p 'interactive))))
	(if entries
	    (if (called-interactively-p 'interactive)
		(eudc-display-records entries t)
	      entries)))
    (error "The %s protocol has no support for listing attributes" eudc-protocol)))

(defun eudc-format-query (words format)
  "Use FORMAT to build a EUDC query from WORDS."
  (let (query
	query-alist
	key val cell)
    (if format
	(progn
	  (while (and words format)
	    (setq query-alist (cons (cons (car format) (car words))
				    query-alist))
	    (setq words (cdr words)
		  format (cdr format)))
	  ;; If the same attribute appears more than once, merge
	  ;; the corresponding values
	  (while query-alist
	    (setq key (caar query-alist)
		  val (cdar query-alist)
		  cell (assq key query))
	    (if cell
		(setcdr cell (concat (cdr cell) " " val))
	      (setq query (cons (car query-alist) query)))
	    (setq query-alist (cdr query-alist)))
	  query)
      (if eudc-protocol-has-default-query-attributes
	  (mapconcat #'identity words " ")
	(list (cons 'name (mapconcat #'identity words " ")))))))

(defun eudc-extract-n-word-formats (format-list n)
  "Extract a list of N-long formats from FORMAT-LIST.
If none try N - 1 and so forth."
  (let (formats)
    (while (and (null formats)
		(> n 0))
      (setq formats
	    (delq nil
		  (mapcar (lambda (format)
			     (if (= n
				    (length format))
				 format
			       nil))
			  format-list)))
      (setq n (1- n)))
    formats))

;;;###autoload
(defun eudc-expand-try-all (&optional try-all-servers)
  "Wrap `eudc-expand-inline' with a prefix argument.
If TRY-ALL-SERVERS -- the prefix argument when called
interactively -- is non-nil, collect results from all servers.
If TRY-ALL-SERVERS is nil, do not try subsequent servers after
one server returns any match."
  (interactive "P")
  (eudc-expand-inline (not eudc-expansion-save-query-as-kill) try-all-servers))

;;;###autoload
(defun eudc-expand-inline (&optional save-query-as-kill try-all-servers)
  "Query the directory server, and expand the query string before point.
The query string consists of the buffer substring from the point back to
the preceding comma, colon or beginning of line.
The variable `eudc-inline-query-format' controls how to associate the
individual inline query words with directory attribute names.
After querying the server for the given string, the expansion specified by
`eudc-inline-expansion-format' is inserted in the buffer at point.
If SAVE-QUERY-AS-KILL is non-nil, then save the pre-expansion
text to the kill ring.  `eudc-expansion-save-query-as-kill' being
non-nil inverts the meaning of SAVE-QUERY-AS-KILL.
Multiple servers can be tried with the same query until one finds a match,
see `eudc-inline-expansion-servers'.  If TRY-ALL-SERVERS is
non-nil, collect results from all servers."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(if (re-search-backward "\\([:,]\\|^\\)[ \t]*"
                                        (line-beginning-position) 'move)
		    (goto-char (match-end 0)))
		(point)))
	 (query-words (split-string (buffer-substring-no-properties beg end)
				    "[ \t]+"))
	 (response-strings (eudc-query-with-words query-words try-all-servers)))
    (if (null response-strings)
        (error "No match")

      (if (or
	   (and save-query-as-kill (not eudc-expansion-save-query-as-kill))
	   (and (not save-query-as-kill) eudc-expansion-save-query-as-kill))
	  (kill-ring-save beg end))
      (cond
       ((or (= (length response-strings) 1)
	    (null eudc-multiple-match-handling-method)
	    (eq eudc-multiple-match-handling-method 'first))
	(delete-region beg end)
	(insert (car response-strings)))
       ((eq eudc-multiple-match-handling-method 'select)
	(eudc-select response-strings beg end))
       ((eq eudc-multiple-match-handling-method 'all)
	(delete-region beg end)
	(insert (mapconcat #'identity response-strings ", ")))
       ((eq eudc-multiple-match-handling-method 'abort)
	(error "There is more than one match for the query"))))))

;;;###autoload
(defun eudc-format-inline-expansion-result (res query-attrs)
  "Format a query result according to `eudc-inline-expansion-format'."
  (cond
   ;; format string
   ((consp eudc-inline-expansion-format)
    (string-trim (apply #'format
	                (car eudc-inline-expansion-format)
	                (mapcar
	                 (lambda (field)
	                   (or (cdr (assq field res))
		               ""))
	                 (eudc-translate-attribute-list
	                  (cdr eudc-inline-expansion-format))))))

   ;; formatting function
   ((functionp eudc-inline-expansion-format)
    (let ((addr (cdr (assq (nth 2 query-attrs) res)))
          (ucontent (funcall eudc-inline-expansion-format res)))
      (if (and ucontent
               (listp ucontent))
          (let* ((phrase (car ucontent))
                 (comment (cadr ucontent))
                 (phrase-given
                  (and phrase
                       (stringp phrase)
                       (not (string-blank-p phrase))))
                 (valid-comment-given
                  (and comment
                       (stringp comment)
                       (not (string-blank-p comment))
                       (eudc-rfc5322-valid-comment-p
                        comment))))
            (eudc-rfc5322-make-address
             addr nil
             (if phrase-given phrase nil)
             (if valid-comment-given comment nil)))
        (progn
          (error "Error: the function referenced by \
`eudc-inline-expansion-format' is expected to return a list.")
          nil))))

   ;; fallback behavior (nil function, or non-matching type)
   (t
    (let ((fname (cdr (assq (nth 0 query-attrs) res)))
          (lname (cdr (assq (nth 1 query-attrs) res)))
          (addr (cdr (assq (nth 2 query-attrs) res))))
      (eudc-rfc5322-make-address addr fname lname)))))

;;;###autoload
(defun eudc-query-with-words (query-words &optional try-all-servers)
  "Query the directory server, and return the matching responses.
The variable `eudc-inline-query-format' controls how to associate the
individual QUERY-WORDS with directory attribute names.
After querying the server for the given string, the expansion
specified by `eudc-inline-expansion-format' is applied to the
matches before returning them.
Multiple servers can be tried with the same query until one finds a match,
see `eudc-inline-expansion-servers'.   When TRY-ALL-SERVERS is non-nil,
keep collecting results from subsequent servers after the first match."
  (cond
   ((eq eudc-inline-expansion-servers 'current-server)
    (or eudc-server
	(call-interactively 'eudc-set-server)))
   ((eq eudc-inline-expansion-servers 'server-then-hotlist)
    (or eudc-server
	;; Allow server to be nil if hotlist is set.
	eudc-server-hotlist
	(call-interactively 'eudc-set-server)))
   ((eq eudc-inline-expansion-servers 'hotlist)
    (or eudc-server-hotlist
	(error "No server in the hotlist")))
   (t
    (error "Wrong value for `eudc-inline-expansion-servers': %S"
	   eudc-inline-expansion-servers)))
  (let* (query-formats
	 response-strings
	 (eudc-former-server eudc-server)
	 (eudc-former-protocol eudc-protocol)
	 ;; Prepare the list of servers to query
	 (servers
	  (cond
	   ((eq eudc-inline-expansion-servers 'hotlist)
	    eudc-server-hotlist)
	   ((eq eudc-inline-expansion-servers 'server-then-hotlist)
	    (if eudc-server
		(cons (cons eudc-server eudc-protocol)
		      (delete (cons eudc-server eudc-protocol)
			      (copy-sequence eudc-server-hotlist)))
	      eudc-server-hotlist))
	   ((eq eudc-inline-expansion-servers 'current-server)
	    (list (cons eudc-server eudc-protocol))))))

    (if (and eudc-max-servers-to-query
	     (> (length servers) eudc-max-servers-to-query))
	(setcdr (nthcdr (1- eudc-max-servers-to-query) servers) nil))

    (unwind-protect
	(cl-flet
	    ((run-query
              (query-formats)
              (let* ((query-attrs (eudc-translate-attribute-list
                                        (if (consp eudc-inline-expansion-format)
                                            (cdr eudc-inline-expansion-format)
                                          '(firstname name email))))
                     (response
                      (eudc-query
                       (eudc-format-query query-words (car query-formats))
                       query-attrs)))
                (when response
                  ;; Format response.
                  (dolist (r response)
                    (let ((response-string
                           (eudc-format-inline-expansion-result r query-attrs)))
                      (if response-string
                          (cl-pushnew response-string response-strings
                                      :test #'equal))))
                  (when (not try-all-servers)
                    (throw 'found nil))))))
	  (catch 'found
	    ;; Loop on the servers.
	    (dolist (server servers)
	      (eudc-set-server (car server) (cdr server) t)

	      ;; Determine which formats apply in the query-format list.
	      (setq query-formats
		    (or
		     (eudc-extract-n-word-formats eudc-inline-query-format
						  (length query-words))
		     (if (null eudc-protocol-has-default-query-attributes)
			 '(name))))

	      ;; Loop on query-formats.
	      (while query-formats
		(run-query query-formats)
		(setq query-formats (cdr query-formats))))
	    ;; No more servers to try... no match found.
	    nil)
	  response-strings)
      (or (and (equal eudc-server eudc-former-server)
	       (equal eudc-protocol eudc-former-protocol))
	  (eudc-set-server eudc-former-server eudc-former-protocol t)))))

;;;###autoload
(defun eudc-query-form (&optional get-fields-from-server)
  "Display a form to query the directory server.
If given a non-nil argument GET-FIELDS-FROM-SERVER, the function first
queries the server for the existing fields and displays a corresponding form."
  (interactive "P")
  (let ((fields (or (and get-fields-from-server
			 (eudc-get-attribute-list))
		    eudc-query-form-attributes))
	(buffer (get-buffer-create "*Directory Query Form*"))
	prompts
	widget
	(width 0)
	pt)
    (switch-to-buffer buffer)
    (let ((inhibit-read-only t))
    (remove-hook 'after-change-functions 'widget-after-change t)
    (delete-all-overlays)
    (erase-buffer)
    (add-hook 'after-change-functions 'widget-after-change nil t)
    (kill-all-local-variables)
    (make-local-variable 'eudc-form-widget-list)
    (widget-insert "Directory Query Form\n")
    (widget-insert "====================\n\n")
    (widget-insert "Current server is: " (or eudc-server
					     (progn
					       (call-interactively 'eudc-set-server)
					       eudc-server))
					     "\n")
    (widget-insert "Protocol         : " (symbol-name eudc-protocol) "\n")
    ;; Build the list of prompts
    (setq prompts (if eudc-use-raw-directory-names
		      (mapcar #'symbol-name (eudc-translate-attribute-list fields))
                    (mapcar (lambda (field)
                              (or (cdr (assq field eudc-user-attribute-names-alist))
                                  (capitalize (symbol-name field))))
			    fields)))
    ;; Loop over prompt strings to find the longest one
    (mapc (lambda (prompt)
            (if (> (length prompt) width)
                (setq width (length prompt))))
	  prompts)
    ;; Insert the first widget out of the mapcar to leave the cursor
    ;; in the first field
    (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
    (setq pt (point))
    (setq widget (widget-create 'editable-field :size 15))
    (setq eudc-form-widget-list (cons (cons (car fields) widget)
				      eudc-form-widget-list))
    (setq fields (cdr fields))
    (setq prompts (cdr prompts))
    (mapc (lambda (field)
            (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
            (setq widget (widget-create 'editable-field
                                        :size 15))
            (setq eudc-form-widget-list (cons (cons field widget)
                                          eudc-form-widget-list))
            (setq prompts (cdr prompts)))
	  fields)
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
			     (eudc-process-form))
		   "Query Server")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
			     (eudc-query-form))
		   "Reset Form")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
			     (kill-this-buffer))
		   "Quit")
    (goto-char pt)
    (use-local-map widget-keymap)
    (widget-setup)))
  )

(defun eudc-bookmark-server (server protocol)
  "Add SERVER using PROTOCOL to the EUDC `servers' hotlist."
  (interactive "sDirectory server: \nsProtocol: ")
  (if (member (cons server protocol) eudc-server-hotlist)
      (error "%s:%s is already in the hotlist" protocol server)
    (setq eudc-server-hotlist (cons (cons server protocol) eudc-server-hotlist))
    (eudc-install-menu)
    (if eudc-ignore-options-file
	(warn "Not saving bookmark due to `eudc-ignore-options-file'\
 customization. Instead, customize `eudc-server-hotlist' to include %s:%s"
	      protocol server)
      (eudc-save-options))))

(defun eudc-bookmark-current-server ()
  "Add current server to the EUDC `servers' hotlist."
  (interactive)
  (eudc-bookmark-server eudc-server eudc-protocol))

(defun eudc-save-options ()
  "Save options to `eudc-options-file'."
  (interactive)
  (when eudc-ignore-options-file
    (error "EUDC is configured to ignore the deprecated options file;\
 see `eudc-ignore-options-file'"))
  (with-current-buffer (find-file-noselect eudc-options-file t)
    (goto-char (point-min))
    ;; delete the previous setq
    (let ((standard-output (current-buffer))
	  provide-p
	  set-hotlist-p
	  set-server-p)
      (catch 'found
	(while t
	  (let ((sexp (condition-case nil
			  (read (current-buffer))
			(end-of-file (throw 'found nil)))))
	    (if (listp sexp)
		(cond
		 ((eq (car sexp)  'eudc-set-server)
		  (delete-region (save-excursion
				   (backward-sexp)
				   (point))
				 (point))
		  (setq set-server-p t))
		 ((and (eq (car sexp)  'setq)
		       (eq (cadr sexp) 'eudc-server-hotlist))
		  (delete-region (save-excursion
				   (backward-sexp)
				   (point))
				 (point))
		  (setq set-hotlist-p t))
		 ((and (eq (car sexp)  'provide)
		       (equal (cadr sexp) '(quote eudc-options-file)))
		  (setq provide-p t)))
	      (if (and provide-p
		       set-hotlist-p
		       set-server-p)
		  (throw 'found t))))))
      (if (eq (point-min) (point-max))
	  (princ ";; This file was automatically generated by eudc.el.\n\n"))
      (or provide-p
	  (princ "(provide 'eudc-options-file)\n"))
      (or (bolp)
	  (princ "\n"))
      (delete-blank-lines)
      (princ "(eudc-set-server ")
      (prin1 eudc-server)
      (princ " '")
      (prin1 eudc-protocol)
      (princ " t)\n")
      (princ "(setq eudc-server-hotlist '")
      (prin1 eudc-server-hotlist)
      (princ ")\n")
      (save-buffer))))

(defun eudc-move-to-next-record ()
  "Move to next record, in a buffer displaying directory query results."
  (interactive)
  (if (not (derived-mode-p 'eudc-mode))
      (error "Not in a EUDC buffer")
    (let ((pt (next-overlay-change (point))))
      (if (< pt (point-max))
	  (goto-char (1+ pt))
	(error "No more records after point")))))

(defun eudc-move-to-previous-record ()
  "Move to previous record, in a buffer displaying directory query results."
  (interactive)
  (if (not (derived-mode-p 'eudc-mode))
      (error "Not in a EUDC buffer")
    (let ((pt (previous-overlay-change (point))))
      (if (> pt (point-min))
	  (goto-char pt)
	(error "No more records before point")))))

;;}}}

;;{{{      Menus and keymaps

(defconst eudc-custom-generated-menu (cdr (custom-menu-create 'eudc)))

(defconst eudc-tail-menu
  `(["---" nil nil]
    ["Query with Form" eudc-query-form
     :help "Display a form to query the directory server"]
    ["Expand Inline Query Trying All Servers" eudc-expand-try-all
     :help "Query all directory servers and expand the query string before point"]
    ["Expand Inline Query" eudc-expand-inline
     :help "Query the directory server, and expand the query string before point"]
    ["Insert Record into BBDB" eudc-insert-record-at-point-into-bbdb
     (and (or (featurep 'bbdb)
	      (prog1 (locate-library "bbdb") (message "")))
	  (overlays-at (point))
	  (overlay-get (car (overlays-at (point))) 'eudc-record))
     :help "Insert record at point into the BBDB database"]
    ["Insert All Records into BBDB" eudc-batch-export-records-to-bbdb
     (and (derived-mode-p 'eudc-mode)
	  (or (featurep 'bbdb)
	      (prog1 (locate-library "bbdb") (message ""))))
     :help "Insert all the records returned by a directory query into BBDB"]
    ["---" nil nil]
    ["Get Email" eudc-get-email
     :help "Get the email field of NAME from the directory server"]
    ["Get Phone" eudc-get-phone
     :help "Get the phone field of name from the directory server"]
    ["List Valid Attribute Names" eudc-get-attribute-list
     :help "Return a list of valid attributes for the current server"]
    ["---" nil nil]
    ,(cons "Customize" eudc-custom-generated-menu)))


(defconst eudc-server-menu
  '(["---" nil nil]
    ["Bookmark Current Server" eudc-bookmark-current-server
     :help "Add current server to the EUDC `servers' hotlist"]
    ["Edit Server List" eudc-edit-hotlist
     :help "Edit the hotlist of directory servers in a specialized buffer"]
    ["New Server" eudc-set-server
     :help "Set the directory server to SERVER using PROTOCOL"]))

(defun eudc-menu ()
  "Return easy menu for EUDC."
  (let (command)
    (append '("Directory Servers")
	    (list
	     (append
	      '("Server")
	      (mapcar
               (lambda (servspec)
                 (let* ((server (car servspec))
                        (protocol (cdr servspec))
                        (proto-name (symbol-name protocol)))
                   (setq command (intern (concat "eudc-set-server-"
                                                 server
                                                 "-"
                                                 proto-name)))
                   (if (not (fboundp command))
                       (fset command
                             (lambda ()
                               (interactive)
                               (eudc-set-server server protocol)
                               (message "Selected directory server is now %s (%s)"
                                        server
                                        proto-name))))
                   (vector (format "%s (%s)" server proto-name)
                           command
                           :style 'radio
                           :selected `(equal eudc-server ,server))))
	       eudc-server-hotlist)
	      eudc-server-menu))
	    eudc-tail-menu)))

(defun eudc-install-menu ()
  "Install EUDC menu."
  (define-key
    global-map
    [menu-bar tools directory-search]
    (cons "Directory Servers"
	  (easy-menu-create-menu "Directory Servers" (cdr (eudc-menu))))))

;;}}}

;;{{{ Load time initializations

;; Load the options file
(let ((library-file-path (locate-library eudc-options-file)))
  (if (and (not noninteractive)
	   (and library-file-path
	        (progn (message "") t))   ; Remove mode line message
	   (not (featurep 'eudc-options-file))
	   (not eudc-ignore-options-file))
      (load eudc-options-file)))

;; Install the full menu
(unless (featurep 'infodock)
  (eudc-install-menu))


;; The following installs a short menu for EUDC at Emacs startup.

;;;###autoload
(defun eudc-load-eudc ()
  "Load the Emacs Unified Directory Client.
This does nothing except loading eudc by autoload side-effect."
  (interactive)
  ;; FIXME: By convention, loading a file should "do nothing significant"
  ;; since Emacs may occasionally load a file for "frivolous" reasons
  ;; (e.g. to find a docstring), so having a function which just loads
  ;; the file doesn't seem very useful.
  nil)

;;;###autoload
(progn
  (defvar eudc-tools-menu
    (let ((map (make-sparse-keymap "Directory Servers")))
      (define-key map [phone]
	`(menu-item ,(purecopy "Get Phone") eudc-get-phone
		    :help ,(purecopy "Get the phone field of name from the directory server")))
      (define-key map [email]
	`(menu-item ,(purecopy "Get Email") eudc-get-email
		    :help ,(purecopy "Get the email field of NAME from the directory server")))
      (define-key map [separator-eudc-email] menu-bar-separator)
      (define-key map [expand-inline]
	`(menu-item ,(purecopy "Expand Inline Query") eudc-expand-inline
		    :help ,(purecopy "Query the directory server, and expand the query string before point")))
      (define-key map [query]
	`(menu-item ,(purecopy "Query with Form") eudc-query-form
		    :help ,(purecopy "Display a form to query the directory server")))
      (define-key map [separator-eudc-query] menu-bar-separator)
      (define-key map [new]
	`(menu-item ,(purecopy "New Server") eudc-set-server
		    :help ,(purecopy "Set the directory server to SERVER using PROTOCOL")))
      (define-key map [load]
	`(menu-item ,(purecopy "Load Hotlist of Servers") eudc-load-eudc
		    :help ,(purecopy "Load the Emacs Unified Directory Client")))
      map))
  (fset 'eudc-tools-menu (symbol-value 'eudc-tools-menu)))

;;}}}

(provide 'eudc)

;;; eudc.el ends here
