;;; eudc-vars.el --- Emacs Unified Directory Client

;; Copyright (C) 1998-2018 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;;         Pavel Janík <Pavel@Janik.cz>
;; Maintainer: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: comm
;; Package: eudc

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

;;; Code:

(require 'custom)

;;{{{      EUDC Main Custom Group

(defgroup eudc nil
  "Emacs Unified Directory Client."
  :version "21.1"
  :link '(info-link "(eudc)")
  :group 'mail
  :group 'comm)

(defcustom eudc-server nil
  "The name or IP address of the directory server.
A port number may be specified by appending a colon and a
number to the name of the server.  Use `localhost' if the directory
server resides on your computer (BBDB backend).

To specify multiple servers, customize eudc-server-hotlist
instead."
  :type  '(choice (string :tag "Server") (const :tag "None" nil)))

;; Known protocols (used in completion)
;; Not to be mistaken with `eudc-supported-protocols'
(defvar eudc-known-protocols '(bbdb ldap))

(defcustom eudc-server-hotlist nil
  "Directory servers to query.
This is an alist of the form (SERVER . PROTOCOL).  SERVER is the
host name or URI of the server, PROTOCOL is a symbol representing
the EUDC backend with which to access the server.

The BBDB backend ignores SERVER; `localhost' can be used as a
placeholder string."
  :tag   "Directory Servers to Query"
  :type  `(repeat (cons :tag "Directory Server"
			(string :tag "Server Host Name or URI")
			(choice :tag "Protocol"
				:menu-tag "Protocol"
				,@(mapcar (lambda (s)
					    (list 'const
						  ':tag (symbol-name s) s))
					  eudc-known-protocols)
				(const :tag "None" nil))))
  :version "25.1")

(defvar eudc-supported-protocols nil
  "Protocols currently supported by EUDC.
This variable is updated when protocol-specific libraries
are loaded, *do not change manually*.")

(defcustom eudc-protocol nil
  "The directory protocol to use to query the server.
Supported protocols are specified by `eudc-supported-protocols'."
  :type  `(choice :menu-tag "Protocol"
		  ,@(mapcar (lambda (s)
			      (list 'const ':tag (symbol-name s) s))
			    eudc-known-protocols)
		  (const :tag "None" nil)))


(defcustom eudc-strict-return-matches t
  "Ignore or allow entries not containing all requested return attributes.
If non-nil, such entries are ignored."
  :type  'boolean)

(defcustom eudc-default-return-attributes nil
  "A list of default attributes to extract from directory entries.
If set to the symbol `all', return all attributes.
A value of nil means return the default attributes as configured in the
server."
  :type  '(choice :menu-tag "Return Attributes"
		  (const :menu-tag "Server defaults (nil)" nil)
		  (const :menu-tag "All" all)
		  (repeat :menu-tag "Attribute list"
			  :tag "Attribute name"
			  :value (nil)
			  (symbol :tag "Attribute name"))))

(defcustom eudc-multiple-match-handling-method 'select
  "What to do when multiple entries match an inline expansion query.
Possible values are:
`first' (equivalent to nil) which means keep the first match only,
`select' pop-up a selection buffer,
`all' expand to all matches,
`abort' the operation is aborted, an error is signaled."
  :type  '(choice :menu-tag "Method"
		  (const :menu-tag "Use First"
			 :tag "Use First"  first)
		  (const :menu-tag "Select Interactively"
			 :tag "Select Interactively" select)
		  (const :menu-tag "Use All"
			 :tag "Use All"    all)
		  (const :menu-tag "Abort Operation"
			 :tag "Abort Operation"  abort)
		  (const :menu-tag "Default (Use First)"
			 :tag "Default (Use First)" nil)))

(defcustom eudc-duplicate-attribute-handling-method '((email . duplicate))
  "A method to handle entries containing duplicate attributes.
This is either an alist (ATTR . METHOD) or a symbol METHOD.
The alist form of the variable associates a method to an individual attribute,
the second form specifies a method applicable to all attributes.
Available methods are:
`list' or nil lets the value of the attribute be a list of values,
`first' keeps the first value and discards the others,
`concat' concatenates the values into a single multiline string,
`duplicate' duplicates the entire entry into as many instances as
different values."
  :type '(choice (const :menu-tag "List" list)
		 (const :menu-tag "First" first)
		 (const :menu-tag "Concat" concat)
		 (const :menu-tag "Duplicate" duplicate)
		 (repeat :menu-tag "Per Attribute Specification"
			 :tag "Per Attribute Specification"
			 (cons :tag "Attribute/Method"
			       :value (nil . list)
			       (symbol :tag "Attribute name")
			       (choice :tag "Method"
				       :menu-tag "Method"
				       (const :menu-tag "List" list)
				       (const :menu-tag "First" first)
				       (const :menu-tag "Concat" concat)
				       (const :menu-tag "Duplicate" duplicate))))))

(defcustom eudc-inline-query-format '((email)
				      (firstname)
				      (firstname name))
  "Format of an inline expansion query.
This is a list of FORMATs.  A FORMAT is itself a list of one or more
EUDC attribute names.  A FORMAT applies if it contains as many attributes as
there are individual words in the inline query string.
If several FORMATs apply then they are tried in order until a match
is found.
If nil, all the words are mapped onto the default server or protocol
attribute name.

The attribute names in FORMATs are not restricted to EUDC attribute names
but can also be protocol/server specific names.  In this case, this variable
must be set in a protocol/server-local fashion, see `eudc-server-set' and
`eudc-protocol-set'."
  :tag "Format of Inline Expansion Queries"
  :type  '(repeat
	   (repeat
	    :menu-tag "Format"
	    :tag "Format"
	    (choice
	     :tag "Attribute"
	     (const :menu-tag "First Name" :tag "First Name" firstname)
	     (const :menu-tag "Surname" :tag "Surname" name)
	     (const :menu-tag "Email Address" :tag "Email Address" email)
	     (const :menu-tag "Phone" :tag "Phone" phone)
	     (symbol :menu-tag "Other" :tag "Attribute name"))))
  :version "25.1")

;; Default to nil so that the most common use of eudc-expand-inline,
;; where replace is nil, does not affect the kill ring.
(defcustom eudc-expansion-overwrites-query nil
  "If non-nil, expanding a query overwrites the query string."
  :type  'boolean
  :version "25.1")

(defcustom eudc-inline-expansion-format '("%s %s <%s>" firstname name email)
  "A list specifying the format of the expansion of inline queries.
This variable controls what `eudc-expand-inline' actually inserts in
the buffer.  First element is a string passed to `format'.  Remaining
elements are symbols indicating attribute names; the corresponding values
are passed as additional arguments to `format'."
  :type  '(list
	   (string :tag "Format String")
	   (repeat :inline t
		   :tag "Attributes"
		   (choice
		    :tag "Attribute"
		    (const :menu-tag "First Name" :tag "First Name" firstname)
		    (const :menu-tag "Surname" :tag "Surname" name)
		    (const :menu-tag "Email Address" :tag "Email Address" email)
		    (const :menu-tag "Phone" :tag "Phone" phone)
		    (symbol :menu-tag "Other")
		    (symbol :tag "Attribute name"))))
  :version "25.1")

(defcustom eudc-inline-expansion-servers 'server-then-hotlist
  "Which servers to contact for the expansion of inline queries.
Possible values are:
  `current-server': the EUDC current server.
  `hotlist': the servers of the hotlist in the order they appear,
  `server-then-hotlist': the current server and then the servers of
  the hotlist."
  :type '(choice :tag "Servers"
		 :menu-tag "Servers"
		 (const :menu-tag "Current server" current-server)
		 (const :menu-tag "Servers in the hotlist" hotlist)
		 (const :menu-tag "Current server then hotlist" server-then-hotlist)))

(defcustom eudc-max-servers-to-query nil
  "Maximum number of servers to query for an inline expansion.
If nil, query all servers available from `eudc-inline-expansion-servers'."
  :tag "Max Number of Servers to Query"
  :type '(choice :tag "Max. Servers"
		 :menu-tag "Max. Servers"
		 (const :menu-tag "No limit" nil)
		 (const :menu-tag "1" 1)
		 (const :menu-tag "2" 2)
		 (const :menu-tag "3" 3)
		 (const :menu-tag "4" 4)
		 (const :menu-tag "5" 5)
		 (integer :menu-tag "Set")))

(defcustom eudc-query-form-attributes '(name firstname email phone)
  "A list of attributes presented in the query form."
  :tag   "Attributes in Query Forms"
  :type  '(repeat
	   (choice
	    :tag "Attribute"
	    (const :menu-tag "First Name" :tag "First Name" firstname)
	    (const :menu-tag "Surname" :tag "Surname" name)
	    (const :menu-tag "Email Address" :tag "Email Address" email)
	    (const :menu-tag "Phone" :tag "Phone" phone)
	    (symbol :menu-tag "Other" :tag "Attribute name"))))

(defcustom eudc-user-attribute-names-alist '((url . "URL")
					     (callsign . "HAM Call Sign")
					     (id . "ID")
					     (email . "E-Mail")
					     (firstname . "First Name")
					     (cn . "Full Name")
					     (sn . "Surname")
					     (givenname . "First Name")
					     (ou . "Unit")
					     (labeledurl . "URL")
					     (postaladdress . "Address")
					     (postalcode . "Postal Code")
					     (l . "Location")
					     (c . "Country")
					     (o . "Organization")
					     (roomnumber . "Office")
					     (telephonenumber . "Phone")
					     (uniqueidentifier . "ID")
					     (objectclass . "Object Class"))
  "Alist of user-defined names for directory attributes.
These names are used as prompt strings in query/response forms
instead of the raw directory attribute names.
Prompt strings for attributes that are not listed here
are derived by splitting the attribute name
at `_' characters and capitalizing the individual words."
  :tag   "User-defined Names of Directory Attributes"
  :type  '(repeat (cons :tag "Field"
			(symbol :tag "Directory attribute")
			(string :tag "User friendly name "))))

(defcustom eudc-use-raw-directory-names nil
  "If non-nil, use attributes names as defined in the directory.
Otherwise, directory query/response forms display the user attribute
names defined in `eudc-user-attribute-names-alist'."
  :type  'boolean)

(defcustom eudc-attribute-display-method-alist nil
  "An alist specifying methods to display attribute values.
Each member of the list is of the form (NAME . FUNC) where NAME is a lowercased
string naming a directory attribute (translated according to
`eudc-user-attribute-names-alist' if `eudc-use-raw-directory-names' is
non-nil) and FUNC a function that will be passed the corresponding
attribute values for display."
  :tag "Attribute Decoding Functions"
  :type '(repeat (cons :tag "Attribute"
		       (symbol :tag "Name")
		       (symbol :tag "Display Function"))))

(defcustom eudc-external-viewers '(("ImageMagick" "display" "-")
				   ("ShowAudio" "showaudio"))
  "A list of viewer program specifications.
Viewers are programs which can be piped a directory attribute value for
display or arbitrary processing.  Each specification is a list whose
first element is a string naming the viewer.  The second element is the
executable program which should be invoked, and following elements are
arguments that should be passed to the program."
  :tag "External Viewer Programs"
  :type '(repeat (list :tag "Viewer"
		       (string :tag "Name")
		       (string :tag "Executable program")
		       (repeat
			:tag "Arguments"
			:inline t
			(string :tag "Argument")))))

(defcustom eudc-options-file
  (locate-user-emacs-file "eudc-options" ".eudc-options")
  "A file where the `servers' hotlist is stored."
  :type '(file :Tag "File Name:")
  :version "25.1")

(defcustom eudc-mode-hook nil
  "Normal hook run on entry to EUDC mode."
  :type 'hook)

;;}}}

;;{{{ PH Custom Group

(defgroup eudc-ph nil
  "Emacs Unified Directory Client - CCSO PH/QI Backend."
  :group 'eudc)

(defcustom eudc-ph-bbdb-conversion-alist
  '((name . name)
    (net . email)
    (address . (eudc-bbdbify-address address "Address"))
    (phone . ((eudc-bbdbify-phone phone "Phone")
	      (eudc-bbdbify-phone office_phone "Office Phone"))))
  "A mapping from BBDB to PH/QI fields.
This is a list of cons cells (BBDB-FIELD . SPEC-OR-LIST) where
BBDB-FIELD is the name of a field that must be defined in your BBDB
environment (standard field names are `name', `company', `net', `phone',
`address' and `notes').  SPEC-OR-LIST is either a single SPEC or a list
of SPECs.  Lists of specs are valid only for the `phone' and `address'
BBDB fields.  SPECs are sexps which are evaluated:
  a string evaluates to itself,
  a symbol evaluates to the symbol value.  Symbols naming PH/QI fields
    present in the record evaluate to the value of the field in the record,
  a form is evaluated as a function.  The argument list may contain PH/QI
    field names which eval to the corresponding values in the
    record.  The form evaluation should return something appropriate for
    the particular BBDB-FIELD (see `bbdb-create-internal').
    `eudc-bbdbify-phone' and `eudc-bbdbify-address' are provided as convenience
    functions to parse phones and addresses."
  :tag "BBDB to PH Field Name Mapping"
  :type '(repeat (cons :tag "Field Name"
		       (symbol :tag "BBDB Field")
		       (sexp :tag "Conversion Spec"))))

(make-obsolete-variable 'eudc-ph-bbdb-conversion-alist
                        "the EUDC PH/QI backend is obsolete."
                        "25.1")

;;}}}

;;{{{ LDAP Custom Group

(defgroup eudc-ldap nil
  "Emacs Unified Directory Client - LDAP Backend."
  :group 'eudc)

(defcustom eudc-ldap-bbdb-conversion-alist
  '((name . cn)
    (net . mail)
    (address . (eudc-bbdbify-address postaladdress "Address"))
    (phone . (eudc-bbdbify-phone telephonenumber "Phone"))
    (company . (eudc-bbdbify-company o)))
  "A mapping from BBDB to LDAP attributes.
This is a list of cons cells (BBDB-FIELD . SPEC-OR-LIST) where
BBDB-FIELD is the name of a field that must be defined in your BBDB
environment (standard field names are `name', `company', `net', `phone',
`address' and `notes').  SPEC-OR-LIST is either a single SPEC or a list
of SPECs.  Lists of specs are valid only for the `phone' and `address'
BBDB fields.  SPECs are sexps which are evaluated:
  a string evaluates to itself,
  a symbol evaluates to the symbol value.  Symbols naming LDAP attributes
    present in the record evaluate to the value of the field in the record,
  a form is evaluated as a function.  The argument list may contain LDAP
    field names which eval to the corresponding values in the
    record.  The form evaluation should return something appropriate for
    the particular BBDB-FIELD (see `bbdb-create-internal').
    `eudc-bbdbify-phone' and `eudc-bbdbify-address' are provided as convenience
    functions to parse phones and addresses."
  :tag "BBDB to LDAP Attribute Names Mapping"
  :type '(repeat (cons :tag "Field Name"
		       (symbol :tag "BBDB Field")
		       (sexp :tag "Conversion Spec"))))

;;}}}

;;{{{ BBDB Custom Group

(defgroup eudc-bbdb nil
  "Emacs Unified Directory Client - BBDB Backend."
  :group 'eudc)

(defcustom eudc-bbdb-use-locations-as-attribute-names t
  "If non-nil, BBDB address and phone locations are used as attribute names.
This has no effect on queries (you can't search for a specific location)
but influences the way records are displayed."
  :type 'boolean)

(defcustom eudc-bbdb-enable-substring-matches t
  "If non-nil, authorize substring match in the same way BBDB does.
Otherwise records must match queries exactly."
  :type 'boolean)

;;}}}


(provide 'eudc-vars)

;;; eudc-vars.el ends here
