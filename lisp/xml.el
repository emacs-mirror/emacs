;;; xml.el --- XML parser

;; Copyright (C) 2000, 01, 03, 2004  Free Software Foundation, Inc.

;; Author: Emmanuel Briot  <briot@gnat.com>
;; Maintainer: Mark A. Hershberger <mah@everybody.org>
;; Keywords: xml, data

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains a somewhat incomplete non-validating XML parser.  It
;; parses a file, and returns a list that can be used internally by
;; any other Lisp libraries.

;;; FILE FORMAT

;; The document type declaration may either be ignored or (optionally)
;; parsed, but currently the parsing will only accept element
;; declarations.  The XML file is assumed to be well-formed.  In case
;; of error, the parsing stops and the XML file is shown where the
;; parsing stopped.
;;
;; It also knows how to ignore comments and processing instructions.
;;
;; The XML file should have the following format:
;;    <node1 attr1="name1" attr2="name2" ...>value
;;       <node2 attr3="name3" attr4="name4">value2</node2>
;;       <node3 attr5="name5" attr6="name6">value3</node3>
;;    </node1>
;; Of course, the name of the nodes and attributes can be anything.  There can
;; be any number of attributes (or none), as well as any number of children
;; below the nodes.
;;
;; There can be only top level node, but with any number of children below.

;;; LIST FORMAT

;; The functions `xml-parse-file', `xml-parse-region' and
;; `xml-parse-tag' return a list with the following format:
;;
;;    xml-list   ::= (node node ...)
;;    node       ::= (qname attribute-list . child_node_list)
;;    child_node_list ::= child_node child_node ...
;;    child_node ::= node | string
;;    qname      ::= (:namespace-uri . "name") | "name"
;;    attribute_list ::= ((qname . "value") (qname . "value") ...)
;;                       | nil
;;    string     ::= "..."
;;
;; Some macros are provided to ease the parsing of this list.
;; Whitespace is preserved.  Fixme: There should be a tree-walker that
;; can remove it.

;; TODO:
;;  * xml:base, xml:space support
;;  * more complete DOCTYPE parsing
;;  * pi support

;;; Code:

;; Note that {buffer-substring,match-string}-no-properties were
;; formerly used in several places, but that removes composition info.

;;*******************************************************************
;;**
;;**  Macros to parse the list
;;**
;;*******************************************************************

(defsubst xml-node-name (node)
  "Return the tag associated with NODE.
Without namespace-aware parsing, the tag is a symbol.

With namespace-aware parsing, the tag is a cons of a string
representing the uri of the namespace with the local name of the
tag.  For example,

    <foo>

would be represented by

    '(\"\" . \"foo\")."

  (car node))

(defsubst xml-node-attributes (node)
  "Return the list of attributes of NODE.
The list can be nil."
  (nth 1 node))

(defsubst xml-node-children (node)
  "Return the list of children of NODE.
This is a list of nodes, and it can be nil."
  (cddr node))

(defun xml-get-children (node child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should match the value returned by `xml-node-name'."
  (let ((match ()))
    (dolist (child (xml-node-children node))
      (if (and (listp child)
               (equal (xml-node-name child) child-name))
          (push child match)))
    (nreverse match)))

(defun xml-get-attribute-or-nil (node attribute)
  "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found.

See also `xml-get-attribute'."
  (cdr (assoc attribute (xml-node-attributes node))))

(defsubst xml-get-attribute (node attribute)
  "Get from NODE the value of ATTRIBUTE.
An empty string is returned if the attribute was not found.

See also `xml-get-attribute-or-nil'."
  (or (xml-get-attribute-or-nil node attribute) ""))

;;*******************************************************************
;;**
;;**  Creating the list
;;**
;;*******************************************************************

;;;###autoload
(defun xml-parse-file (file &optional parse-dtd parse-ns)
  "Parse the well-formed XML file FILE.
If FILE is already visited, use its buffer and don't kill it.
Returns the top node with all its children.
If PARSE-DTD is non-nil, the DTD is parsed rather than skipped.
If PARSE-NS is non-nil, then QNAMES are expanded."
  (let ((keep))
    (if (get-file-buffer file)
	(progn
	  (set-buffer (get-file-buffer file))
	  (setq keep (point)))
      (let (auto-mode-alist)		; no need for xml-mode
	(find-file file)))

    (let ((xml (xml-parse-region (point-min)
				 (point-max)
				 (current-buffer)
				 parse-dtd parse-ns)))
      (if keep
	  (goto-char keep)
	(kill-buffer (current-buffer)))
      xml)))

;; Note that this is setup so that we can do whitespace-skipping with
;; `(skip-syntax-forward " ")', inter alia.  Previously this was slow
;; compared with `re-search-forward', but that has been fixed.  Also
;; note that the standard syntax table contains other characters with
;; whitespace syntax, like NBSP, but they are invalid in contexts in
;; which we might skip whitespace -- specifically, they're not
;; NameChars [XML 4].

(defvar xml-syntax-table
  (let ((table (make-syntax-table)))
    ;; Get space syntax correct per XML [3].
    (dotimes (c 31)
      (modify-syntax-entry c "." table)) ; all are space in standard table
    (dolist (c '(?\t ?\n ?\r))		; these should be space
      (modify-syntax-entry c " " table))
    ;; For skipping attributes.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Non-alnum name chars should be symbol constituents (`-' and `_'
    ;; are OK by default).
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?: "_" table)
    ;; XML [89]
    (dolist (c '(#x00B7 #x02D0 #x02D1 #x0387 #x0640 #x0E46 #x0EC6 #x3005
		 #x3031 #x3032 #x3033 #x3034 #x3035 #x309D #x309E #x30FC
		 #x30FD #x30FE))
      (modify-syntax-entry (decode-char 'ucs c) "w" table))
    ;; Fixme: rest of [4]
    table)
  "Syntax table used by `xml-parse-region'.")

;; XML [5]
;; Note that [:alpha:] matches all multibyte chars with word syntax.
(eval-and-compile
  (defconst xml-name-regexp "[[:alpha:]_:][[:alnum:]._:-]*"))

;; Fixme:  This needs re-writing to deal with the XML grammar properly, i.e.
;;   document    ::=    prolog element Misc*
;;   prolog    ::=    XMLDecl? Misc* (doctypedecl Misc*)?

;;;###autoload
(defun xml-parse-region (beg end &optional buffer parse-dtd parse-ns)
  "Parse the region from BEG to END in BUFFER.
If BUFFER is nil, it defaults to the current buffer.
Returns the XML list for the region, or raises an error if the region
is not well-formed XML.
If PARSE-DTD is non-nil, the DTD is parsed rather than skipped,
and returned as the first element of the list.
If PARSE-NS is non-nil, then QNAMES are expanded."
  (save-restriction
    (narrow-to-region beg end)
    ;; Use fixed syntax table to ensure regexp char classes and syntax
    ;; specs DTRT.
    (with-syntax-table (standard-syntax-table)
      (let ((case-fold-search nil)	; XML is case-sensitive.
	    xml result dtd)
	(save-excursion
	  (if buffer
	      (set-buffer buffer))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (search-forward "<" nil t)
		(progn
		  (forward-char -1)
		  (setq result (xml-parse-tag parse-dtd parse-ns))
		  (if (and xml result)
		      ;;  translation of rule [1] of XML specifications
		      (error "XML files can have only one toplevel tag")
		    (cond
		     ((null result))
		     ((and (listp (car result))
			   parse-dtd)
		      (setq dtd (car result))
		      (if (cdr result)	; possible leading comment
			  (add-to-list 'xml (cdr result))))
		     (t
		      (add-to-list 'xml result)))))
	      (goto-char (point-max))))
	  (if parse-dtd
	      (cons dtd (nreverse xml))
	    (nreverse xml)))))))

(defun xml-maybe-do-ns (name default xml-ns)
  "Perform any namespace expansion.
NAME is the name to perform the expansion on.
DEFAULT is the default namespace.  XML-NS is a cons of namespace
names to uris.  When namespace-aware parsing is off, then XML-NS
is nil.

During namespace-aware parsing, any name without a namespace is
put into the namespace identified by DEFAULT.  nil is used to
specify that the name shouldn't be given a namespace."
  (if (consp xml-ns)
      (let* ((nsp (string-match ":" name))
	     (lname (if nsp (substring name (match-end 0)) name))
	     (prefix (if nsp (substring name 0 (match-beginning 0)) default))
	     (special (and (string-equal lname "xmlns") (not prefix)))
             ;; Setting default to nil will insure that there is not
             ;; matching cons in xml-ns.  In which case we
	     (ns (or (cdr (assoc (if special "xmlns" prefix)
                                 xml-ns))
                     :)))
        (cons ns (if special "" lname)))
    (intern name)))

(defun xml-parse-tag (&optional parse-dtd parse-ns)
  "Parse the tag at point.
If PARSE-DTD is non-nil, the DTD of the document, if any, is parsed and
returned as the first element in the list.
If PARSE-NS is non-nil, then QNAMES are expanded.
Returns one of:
 - a list : the matching node
 - nil    : the point is not looking at a tag.
 - a pair : the first element is the DTD, the second is the node."
  (let ((xml-ns (if (consp parse-ns)
		    parse-ns
		  (if parse-ns
		      (list
                       ;; Default for empty prefix is no namespace
                       (cons ""      :)
		       ;; "xml" namespace
		       (cons "xml"   :http://www.w3.org/XML/1998/namespace)
		       ;; We need to seed the xmlns namespace
		       (cons "xmlns" :http://www.w3.org/2000/xmlns/))))))
    (cond
     ;; Processing instructions (like the <?xml version="1.0"?> tag at the
     ;; beginning of a document).
     ((looking-at "<\\?")
      (search-forward "?>")
      (skip-syntax-forward " ")
      (xml-parse-tag parse-dtd xml-ns))
     ;;  Character data (CDATA) sections, in which no tag should be interpreted
     ((looking-at "<!\\[CDATA\\[")
      (let ((pos (match-end 0)))
	(unless (search-forward "]]>" nil t)
	  (error "CDATA section does not end anywhere in the document"))
	(buffer-substring pos (match-beginning 0))))
     ;;  DTD for the document
     ((looking-at "<!DOCTYPE")
      (let (dtd)
	(if parse-dtd
	    (setq dtd (xml-parse-dtd))
	  (xml-skip-dtd))
      (skip-syntax-forward " ")
      (if dtd
	  (cons dtd (xml-parse-tag nil xml-ns))
	(xml-parse-tag nil xml-ns))))
     ;;  skip comments
     ((looking-at "<!--")
      (search-forward "-->")
      nil)
     ;;  end tag
     ((looking-at "</")
      '())
     ;;  opening tag
     ((looking-at "<\\([^/>[:space:]]+\\)")
      (goto-char (match-end 1))

      ;; Parse this node
      (let* ((node-name (match-string 1))
             ;; Parse the attribute list.
             (attrs (xml-parse-attlist xml-ns))
             children pos)

        ;; add the xmlns:* attrs to our cache
        (when (consp xml-ns)
	  (dolist (attr attrs)
	    (when (and (consp (car attr))
		       (eq :http://www.w3.org/2000/xmlns/
			   (caar attr)))
	      (push (cons (cdar attr) (intern (concat ":" (cdr attr))))
		    xml-ns))))

        (setq children (list attrs (xml-maybe-do-ns node-name "" xml-ns)))

	;; is this an empty element ?
	(if (looking-at "/>")
	(progn
	  (forward-char 2)
	  (nreverse children))

	;; is this a valid start tag ?
	(if (eq (char-after) ?>)
	    (progn
	      (forward-char 1)
	      ;;  Now check that we have the right end-tag. Note that this
	      ;;  one might contain spaces after the tag name
	      (let ((end (concat "</" node-name "\\s-*>")))
		(while (not (looking-at end))
		  (cond
		   ((looking-at "</")
		    (error "XML: Invalid end tag (expecting %s) at pos %d"
			   node-name (point)))
		   ((= (char-after) ?<)
		    (let ((tag (xml-parse-tag nil xml-ns)))
		      (when tag
			(push tag children))))
		   (t
		    (setq pos (point))
		    (search-forward "<")
		    (forward-char -1)
		    (let ((string (buffer-substring pos (point)))
			  (pos 0))

		      ;; Clean up the string.  As per XML
		      ;; specifications, the XML processor should
		      ;; always pass the whole string to the
		      ;; application.  But \r's should be replaced:
		      ;; http://www.w3.org/TR/2000/REC-xml-20001006#sec-line-ends
		      (while (string-match "\r\n?" string pos)
			(setq string (replace-match "\n" t t string))
			(setq pos (1+ (match-beginning 0))))

		      (setq string (xml-substitute-special string))
		      (setq children
			    (if (stringp (car children))
				;; The two strings were separated by a comment.
				(cons (concat (car children) string)
				      (cdr children))
			      (cons string children))))))))

	      (goto-char (match-end 0))
	      (nreverse children))
	  ;;  This was an invalid start tag
	  (error "XML: Invalid attribute list")))))
     (t	;; This is not a tag.
      (error "XML: Invalid character")))))

(defun xml-parse-attlist (&optional xml-ns)
  "Return the attribute-list after point.
Leave point at the first non-blank character after the tag."
  (let ((attlist ())
	end-pos name)
    (skip-syntax-forward " ")
    (while (looking-at (eval-when-compile
			 (concat "\\(" xml-name-regexp "\\)\\s-*=\\s-*")))
      (setq end-pos (match-end 0))
      (setq name (xml-maybe-do-ns (match-string 1) nil xml-ns))
      (goto-char end-pos)

      ;; See also: http://www.w3.org/TR/2000/REC-xml-20001006#AVNormalize

      ;; Do we have a string between quotes (or double-quotes),
      ;;  or a simple word ?
      (if (looking-at "\"\\([^\"]*\\)\"")
	  (setq end-pos (match-end 0))
	(if (looking-at "'\\([^']*\\)'")
	    (setq end-pos (match-end 0))
	  (error "XML: Attribute values must be given between quotes")))

      ;; Each attribute must be unique within a given element
      (if (assoc name attlist)
	  (error "XML: each attribute must be unique within an element"))

      ;; Multiple whitespace characters should be replaced with a single one
      ;; in the attributes
      (let ((string (match-string 1))
	    (pos 0))
	(replace-regexp-in-string "\\s-\\{2,\\}" " " string)
	(push (cons name (xml-substitute-special string)) attlist))

      (goto-char end-pos)
      (skip-syntax-forward " "))
    (nreverse attlist)))

;;*******************************************************************
;;**
;;**  The DTD (document type declaration)
;;**  The following functions know how to skip or parse the DTD of
;;**  a document
;;**
;;*******************************************************************

;; Fixme: This fails at least if the DTD contains conditional sections.

(defun xml-skip-dtd ()
  "Skip the DTD at point.
This follows the rule [28] in the XML specifications."
  (forward-char (length "<!DOCTYPE"))
  (if (looking-at "\\s-*>")
      (error "XML: invalid DTD (excepting name of the document)"))
  (condition-case nil
      (progn
	(forward-sexp)
	(skip-syntax-forward " ")
	(if (looking-at "\\[")
	    (re-search-forward "]\\s-*>")
	  (search-forward ">")))
    (error (error "XML: No end to the DTD"))))

(defun xml-parse-dtd ()
  "Parse the DTD at point."
  (forward-char (eval-when-compile (length "<!DOCTYPE")))
  (skip-syntax-forward " ")
  (if (looking-at ">")
      (error "XML: invalid DTD (excepting name of the document)"))

  ;;  Get the name of the document
  (looking-at xml-name-regexp)
  (let ((dtd (list (match-string 0) 'dtd))
	type element end-pos)
    (goto-char (match-end 0))

    (skip-syntax-forward " ")
    ;; XML [75]
    (cond ((looking-at "PUBLIC\\s-+")
	   (goto-char (match-end 0))
	   (unless (or (re-search-forward
			"\\=\"\\([[:space:][:alnum:]-'()+,./:=?;!*#@$_%]*\\)\""
			nil t)
		       (re-search-forward
			"\\='\\([[:space:][:alnum:]-()+,./:=?;!*#@$_%]*\\)'"
			nil t))
	     (error "XML: missing public id"))
	   (let ((pubid (match-string 1)))
	     (unless (or (re-search-forward "\\='\\([^']*\\)'" nil t)
			 (re-search-forward "\\=\"\\([^\"]*\\)\"" nil t))
	       (error "XML: missing system id"))
	     (push (list pubid (match-string 1) 'public) dtd)))
	  ((looking-at "SYSTEM\\s-+")
	   (goto-char (match-end 0))
	   (unless (or (re-search-forward "\\='\\([^']*\\)'" nil t)
		       (re-search-forward "\\=\"\\([^\"]*\\)\"" nil t))
	     (error "XML: missing system id"))
	   (push (list (match-string 1) 'system) dtd)))
    (skip-syntax-forward " ")
    (if (eq ?> (char-after))
	(forward-char)
      (skip-syntax-forward " ")
      (if (not (eq (char-after) ?\[))
	  (error "XML: bad DTD")
	(forward-char)
	;;  Parse the rest of the DTD
	;;  Fixme: Deal with ENTITY, ATTLIST, NOTATION, PIs.
	(while (not (looking-at "\\s-*\\]"))
	  (skip-syntax-forward " ")
	  (cond

	   ;;  Translation of rule [45] of XML specifications
	   ((looking-at
	     "<!ELEMENT\\s-+\\([[:alnum:].%;]+\\)\\s-+\\([^>]+\\)>")

	    (setq element (match-string 1)
		  type    (match-string-no-properties 2))
	    (setq end-pos (match-end 0))

	    ;;  Translation of rule [46] of XML specifications
	    (cond
	     ((string-match "^EMPTY[ \t\n\r]*$" type) ;; empty declaration
	      (setq type 'empty))
	     ((string-match "^ANY[ \t\n\r]*$" type) ;; any type of contents
	      (setq type 'any))
	     ((string-match "^(\\(.*\\))[ \t\n\r]*$" type) ;; children ([47])
	      (setq type (xml-parse-elem-type (match-string 1 type))))
	     ((string-match "^%[^;]+;[ \t\n\r]*$" type)	;; substitution
	      nil)
	     (t
	      (error "XML: Invalid element type in the DTD")))

	    ;;  rule [45]: the element declaration must be unique
	    (if (assoc element dtd)
		(error "XML: element declarations must be unique in a DTD (<%s>)"
		       element))

	    ;;  Store the element in the DTD
	    (push (list element type) dtd)
	    (goto-char end-pos))
	   ((looking-at "<!--")
	    (search-forward "-->"))

	   (t
	    (error "XML: Invalid DTD item")))

	  ;;  Skip the end of the DTD
	  (search-forward ">"))))
    (nreverse dtd)))

(defun xml-parse-elem-type (string)
  "Convert element type STRING into a Lisp structure."

  (let (elem modifier)
    (if (string-match "(\\([^)]+\\))\\([+*?]?\\)" string)
	(progn
	  (setq elem     (match-string 1 string)
		modifier (match-string 2 string))
	  (if (string-match "|" elem)
	      (setq elem (cons 'choice
			       (mapcar 'xml-parse-elem-type
				       (split-string elem "|"))))
	    (if (string-match "," elem)
		(setq elem (cons 'seq
				 (mapcar 'xml-parse-elem-type
					 (split-string elem ",")))))))
      (if (string-match "[ \t\n\r]*\\([^+*?]+\\)\\([+*?]?\\)" string)
	  (setq elem	 (match-string 1 string)
		modifier (match-string 2 string))))

    (if (and (stringp elem) (string= elem "#PCDATA"))
	(setq elem 'pcdata))

    (cond
     ((string= modifier "+")
      (list '+ elem))
     ((string= modifier "*")
      (list '* elem))
     ((string= modifier "?")
      (list '\? elem))
     (t
      elem))))

;;*******************************************************************
;;**
;;**  Substituting special XML sequences
;;**
;;*******************************************************************

(eval-when-compile
  (defvar str))		       ; dynamic from replace-regexp-in-string

;; Fixme:  Take declared entities from the DTD when they're available.
(defun xml-substitute-entity (match)
  "Subroutine of `xml-substitute-special'."
  (save-match-data
    (let ((match1 (match-string 1 str)))
      (cond ((string= match1 "lt") "<")
	    ((string= match1 "gt") ">")
	    ((string= match1 "apos") "'")
	    ((string= match1 "quot") "\"")
	    ((string= match1 "amp") "&")
	    ((and (string-match "#\\([0-9]+\\)" match1)
		  (let ((c (decode-char
			    'ucs
			    (string-to-number (match-string 1 match1)))))
		    (if c (string c))))) ; else unrepresentable
	    ((and (string-match "#x\\([[:xdigit:]]+\\)" match1)
		  (let ((c (decode-char
			    'ucs
			    (string-to-number (match-string 1 match1) 16))))
		    (if c (string c)))))
	    ;; Default to asis.  Arguably, unrepresentable code points
	    ;; might be best replaced with U+FFFD.
	    (t match)))))

(defun xml-substitute-special (string)
  "Return STRING, after subsituting entity references."
  ;; This originally made repeated passes through the string from the
  ;; beginning, which isn't correct, since then either "&amp;amp;" or
  ;; "&#38;amp;" won't DTRT.
  (replace-regexp-in-string "&\\([^;]+\\);"
			    #'xml-substitute-entity string t t))

;;*******************************************************************
;;**
;;**  Printing a tree.
;;**  This function is intended mainly for debugging purposes.
;;**
;;*******************************************************************

(defun xml-debug-print (xml)
  (dolist (node xml)
    (xml-debug-print-internal node "")))

(defun xml-debug-print-internal (xml indent-string)
  "Outputs the XML tree in the current buffer.
The first line is indented with INDENT-STRING."
  (let ((tree xml)
	attlist)
    (insert indent-string ?< (symbol-name (xml-node-name tree)))

    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert ?\  (symbol-name (caar attlist)) "=\"" (cdar attlist) ?\")
      (setq attlist (cdr attlist)))

    (insert ?>)

    (setq tree (xml-node-children tree))

    ;;  output the children
    (dolist (node tree)
      (cond
       ((listp node)
	(insert ?\n)
	(xml-debug-print-internal node (concat indent-string "  ")))
       ((stringp node) (insert node))
       (t
	(error "Invalid XML tree"))))

    (insert ?\n indent-string
	    ?< ?/ (symbol-name (xml-node-name xml)) ?>)))

(provide 'xml)

;;; arch-tag: 5864b283-5a68-4b59-a20d-36a72b353b9b
;;; xml.el ends here
