;;; xml.el --- XML parser

;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Emmanuel Briot  <briot@gnat.com>
;; Maintainer: Emmanuel Briot <briot@gnat.com>
;; Keywords: xml

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

;; This file contains a full XML parser. It parses a file, and returns a list
;; that can be used internally by any other lisp file.
;; See some example in todo.el

;;; FILE FORMAT

;; It does not parse the DTD, if present in the XML file, but knows how to
;; ignore it. The XML file is assumed to be well-formed. In case of error, the
;; parsing stops and the XML file is shown where the parsing stopped.
;;
;; It also knows how to ignore comments, as well as the special ?xml? tag
;; in the XML file.
;;
;; The XML file should have the following format:
;;    <node1 attr1="name1" attr2="name2" ...>value
;;       <node2 attr3="name3" attr4="name4">value2</node2>
;;       <node3 attr5="name5" attr6="name6">value3</node3>
;;    </node1>
;; Of course, the name of the nodes and attributes can be anything. There can
;; be any number of attributes (or none), as well as any number of children
;; below the nodes.
;;
;; There can be only top level node, but with any number of children below.

;;; LIST FORMAT

;; The functions `xml-parse-file' and `xml-parse-tag' return a list with
;; the following format:
;;
;;    xml-list   ::= (node node ...)
;;    node       ::= (tag_name attribute-list . child_node_list)
;;    child_node_list ::= child_node child_node ...
;;    child_node ::= node | string
;;    tag_name   ::= string
;;    attribute_list ::= (("attribute" . "value") ("attribute" . "value") ...)
;;                       | nil
;;    string     ::= "..."
;;
;; Some macros are provided to ease the parsing of this list

;;; Code:

;;*******************************************************************
;;**
;;**  Macros to parse the list
;;**
;;*******************************************************************

(defsubst xml-node-name (node)
  "Return the tag associated with NODE.
The tag is a lower-case symbol."
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
CHILD-NAME should be a lower case symbol."
  (let ((match ()))
    (dolist (child (xml-node-children node))
      (if child
	  (if (equal (xml-node-name child) child-name)
	      (push child match))))
    (nreverse match)))

(defun xml-get-attribute (node attribute)
  "Get from NODE the value of ATTRIBUTE.
An empty string is returned if the attribute was not found."
  (if (xml-node-attributes node)
      (let ((value (assoc attribute (xml-node-attributes node))))
	(if value
	    (cdr value)
	  ""))
    ""))

;;*******************************************************************
;;**
;;**  Creating the list
;;**
;;*******************************************************************

(defun xml-parse-file (file &optional parse-dtd)
  "Parse the well-formed XML FILE.
If FILE is already edited, this will keep the buffer alive.
Returns the top node with all its children.
If PARSE-DTD is non-nil, the DTD is parsed rather than skipped."
  (let ((keep))
    (if (get-file-buffer file)
	(progn
	  (set-buffer (get-file-buffer file))
	  (setq keep (point)))
      (find-file file))
    
    (let ((xml (xml-parse-region (point-min)
				 (point-max)
				 (current-buffer)
				 parse-dtd)))
      (if keep
	  (goto-char keep)
	(kill-buffer (current-buffer)))
      xml)))

(defun xml-parse-region (beg end &optional buffer parse-dtd)
  "Parse the region from BEG to END in BUFFER.
If BUFFER is nil, it defaults to the current buffer.
Returns the XML list for the region, or raises an error if the region
is not a well-formed XML file.
If PARSE-DTD is non-nil, the DTD is parsed rather than skipped,
and returned as the first element of the list"
  (let (xml result dtd)
    (save-excursion
      (if buffer
	  (set-buffer buffer))
      (goto-char beg)
      (while (< (point) end)
	(if (search-forward "<" end t)
	    (progn
	      (forward-char -1)
	      (if (null xml)
		  (progn
		    (setq result (xml-parse-tag end parse-dtd))
		    (cond
		     ((null result))
		     ((listp (car result))
		      (setq dtd (car result))
		      (add-to-list 'xml (cdr result)))
		     (t
		      (add-to-list 'xml result))))

		;;  translation of rule [1] of XML specifications
		(error "XML files can have only one toplevel tag")))
	  (goto-char end)))
      (if parse-dtd
	  (cons dtd (reverse xml))
	(reverse xml)))))


(defun xml-parse-tag (end &optional parse-dtd)
  "Parse the tag that is just in front of point.
The end tag must be found before the position END in the current buffer.
If PARSE-DTD is non-nil, the DTD of the document, if any, is parsed and
returned as the first element in the list.
Returns one of:
   - a list : the matching node
   - nil    : the point is not looking at a tag.
   - a cons cell: the first element is the DTD, the second is the node"
  (cond
   ;; Processing instructions (like the <?xml version="1.0"?> tag at the
   ;; beginning of a document)
   ((looking-at "<\\?")
    (search-forward "?>" end)
    (skip-chars-forward " \t\n")
    (xml-parse-tag end))
   ;;  Character data (CDATA) sections, in which no tag should be interpreted
   ((looking-at "<!\\[CDATA\\[")
    (let ((pos (match-end 0)))
      (unless (search-forward "]]>" end t)
	(error "CDATA section does not end anywhere in the document"))
      (buffer-substring-no-properties pos (match-beginning 0))))
   ;;  DTD for the document
   ((looking-at "<!DOCTYPE")
    (let (dtd)
      (if parse-dtd
	  (setq dtd (xml-parse-dtd end))
	(xml-skip-dtd end))
      (skip-chars-forward " \t\n")
      (if dtd
	  (cons dtd (xml-parse-tag end))
	(xml-parse-tag end))))
   ;;  skip comments
   ((looking-at "<!--")
    (search-forward "-->" end)
    nil)
   ;;  end tag
   ((looking-at "</")
    '())
   ;;  opening tag
   ((looking-at "<\\([^/> \t\n]+\\)")
    (goto-char (match-end 1))
    (let* ((case-fold-search nil) ;; XML is case-sensitive.
	   (node-name (match-string 1))
	   ;; Parse the attribute list.
	   (children (list (xml-parse-attlist end) (intern node-name)))
	   pos)

      ;; is this an empty element ?
      (if (looking-at "/>")
	  (progn
	    (forward-char 2)
	    (nreverse (cons '("") children)))

	;; is this a valid start tag ?
	(if (eq (char-after) ?>)
	    (progn
	      (forward-char 1)
	      ;;  Now check that we have the right end-tag. Note that this
	      ;;  one might contain spaces after the tag name
	      (while (not (looking-at (concat "</" node-name "[ \t\n]*>")))
		(cond
		 ((looking-at "</")
		  (error (concat
			  "XML: invalid syntax -- invalid end tag (expecting "
			  node-name
			  ") at pos " (number-to-string (point)))))
		 ((= (char-after) ?<)
		  (let ((tag (xml-parse-tag end)))
		    (when tag
		      (push tag children))))
		 (t
		  (setq pos (point))
		  (search-forward "<" end)
		  (forward-char -1)
		  (let ((string (buffer-substring-no-properties pos (point)))
			(pos 0))
		    
		    ;; Clean up the string (no newline characters)
		    ;; Not done, since as per XML specifications, the XML processor
		    ;; should always pass the whole string to the application.
		    ;; 	    (while (string-match "\\s +" string pos)
		    ;; 	      (setq string (replace-match " " t t string))
		    ;; 	      (setq pos (1+ (match-beginning 0))))

		    (setq string (xml-substitute-special string))
		    (setq children
			  (if (stringp (car children))
			      ;; The two strings were separated by a comment.
			      (cons (concat (car children) string)
				    (cdr children))
			    (cons string children)))))))
	      (goto-char (match-end 0))
	      (if (> (point) end)
		  (error "XML: End tag for %s not found before end of region"
			 node-name))
	      (nreverse children))

	  ;;  This was an invalid start tag
	  (error "XML: Invalid attribute list")
	  ))))
   (t ;; This is not a tag.
    (error "XML: Invalid character"))
   ))

(defun xml-parse-attlist (end)
  "Return the attribute-list that point is looking at.
The search for attributes end at the position END in the current buffer.
Leaves the point on the first non-blank character after the tag."
  (let ((attlist ())
	name)
    (skip-chars-forward " \t\n")
    (while (looking-at "\\([a-zA-Z_:][-a-zA-Z0-9._:]*\\)[ \t\n]*=[ \t\n]*")
      (setq name (intern (match-string 1)))
      (goto-char (match-end 0))

      ;; Do we have a string between quotes (or double-quotes),
      ;;  or a simple word ?
      (unless (looking-at "\"\\([^\"]*\\)\"")
	(unless (looking-at "'\\([^']*\\)'")
	  (error "XML: Attribute values must be given between quotes")))

      ;; Each attribute must be unique within a given element
      (if (assoc name attlist)
	  (error "XML: each attribute must be unique within an element"))
      
      (push (cons name (match-string-no-properties 1)) attlist)
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n")
      (if (> (point) end)
	  (error "XML: end of attribute list not found before end of region"))
      )
    (nreverse attlist)))

;;*******************************************************************
;;**
;;**  The DTD (document type declaration)
;;**  The following functions know how to skip or parse the DTD of
;;**  a document
;;**
;;*******************************************************************

(defun xml-skip-dtd (end)
  "Skip the DTD that point is looking at.
The DTD must end before the position END in the current buffer.
The point must be just before the starting tag of the DTD.
This follows the rule [28] in the XML specifications."
  (forward-char (length "<!DOCTYPE"))
  (if (looking-at "[ \t\n]*>")
      (error "XML: invalid DTD (excepting name of the document)"))
  (condition-case nil
      (progn
	(forward-word 1)  ;; name of the document
	(skip-chars-forward " \t\n")
	(if (looking-at "\\[")
	    (re-search-forward "\\][ \t\n]*>" end)
	  (search-forward ">" end)))
    (error (error "XML: No end to the DTD"))))

(defun xml-parse-dtd (end)
  "Parse the DTD that point is looking at.
The DTD must end before the position END in the current buffer."
  (forward-char (length "<!DOCTYPE"))
  (skip-chars-forward " \t\n")
  (if (looking-at ">")
      (error "XML: invalid DTD (excepting name of the document)"))
  
  ;;  Get the name of the document
  (looking-at "\\sw+")
  (let ((dtd (list (match-string-no-properties 0) 'dtd))
	type element end-pos)
    (goto-char (match-end 0))

    (skip-chars-forward " \t\n")

    ;;  External DTDs => don't know how to handle them yet
    (if (looking-at "SYSTEM")
	(error "XML: Don't know how to handle external DTDs"))
    
    (if (not (= (char-after) ?\[))
	(error "XML: Unknown declaration in the DTD"))

    ;;  Parse the rest of the DTD
    (forward-char 1)
    (while (and (not (looking-at "[ \t\n]*\\]"))
		(<= (point) end))
      (cond

       ;;  Translation of rule [45] of XML specifications
       ((looking-at
	 "[\t \n]*<!ELEMENT[ \t\n]+\\([a-zA-Z0-9.%;]+\\)[ \t\n]+\\([^>]+\\)>")

	(setq element (intern (match-string-no-properties 1))
	      type    (match-string-no-properties 2))
	(setq end-pos (match-end 0))
	
	;;  Translation of rule [46] of XML specifications
	(cond
	 ((string-match "^EMPTY[ \t\n]*$" type)     ;; empty declaration
	  (setq type 'empty))
	 ((string-match "^ANY[ \t\n]*$" type)       ;; any type of contents
	  (setq type 'any))
	 ((string-match "^(\\(.*\\))[ \t\n]*$" type) ;; children ([47])
	  (setq type (xml-parse-elem-type (match-string-no-properties 1 type))))
	 ((string-match "^%[^;]+;[ \t\n]*$" type)   ;; substitution
	  nil)
	 (t
	  (error "XML: Invalid element type in the DTD")))

	;;  rule [45]: the element declaration must be unique
	(if (assoc element dtd)
	    (error "XML: elements declaration must be unique in a DTD (<%s>)"
		   (symbol-name element)))
	
	;;  Store the element in the DTD
	(push (list element type) dtd)
	(goto-char end-pos))


       (t
	(error "XML: Invalid DTD item"))
       )
      )

    ;;  Skip the end of the DTD
    (search-forward ">" end)
    (nreverse dtd)))


(defun xml-parse-elem-type (string)
  "Convert a STRING for an element type into an elisp structure."

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
					 (split-string elem ","))))
	      )))
      (if (string-match "[ \t\n]*\\([^+*?]+\\)\\([+*?]?\\)" string)
	  (setq elem     (match-string 1 string)
		modifier (match-string 2 string))))

    (if (and (stringp elem) (string= elem "#PCDATA"))
	(setq elem 'pcdata))
    
    (cond
     ((string= modifier "+")
      (list '+ elem))
     ((string= modifier "*")
      (list '* elem))
     ((string= modifier "?")
      (list '? elem))
     (t
      elem))))


;;*******************************************************************
;;**
;;**  Substituting special XML sequences
;;**
;;*******************************************************************

(defun xml-substitute-special (string)
  "Return STRING, after subsituting special XML sequences."
  (while (string-match "&amp;" string)
    (setq string (replace-match "&"  t nil string)))
  (while (string-match "&lt;" string)
    (setq string (replace-match "<"  t nil string)))
  (while (string-match "&gt;" string)
    (setq string (replace-match ">"  t nil string)))
  (while (string-match "&apos;" string)
    (setq string (replace-match "'"  t nil string)))
  (while (string-match "&quot;" string)
    (setq string (replace-match "\"" t nil string)))
  string)

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
The first line indented with INDENT-STRING."
  (let ((tree xml)
	attlist)
    (insert indent-string "<" (symbol-name (xml-node-name tree)))
    
    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert " ")
      (insert (symbol-name (caar attlist)) "=\"" (cdar attlist) "\"")
      (setq attlist (cdr attlist)))
    
    (insert ">")
    
    (setq tree (xml-node-children tree))

    ;;  output the children
    (dolist (node tree)
      (cond
       ((listp node)
	(insert "\n")
	(xml-debug-print-internal node (concat indent-string "  ")))
       ((stringp node) (insert node))
       (t
	(error "Invalid XML tree"))))

    (insert "\n" indent-string
	    "</" (symbol-name (xml-node-name xml)) ">")))

(provide 'xml)

;;; xml.el ends here
