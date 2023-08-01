;;; org-element.el --- Parser for Org Syntax         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

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
;; See <https://orgmode.org/worg/dev/org-syntax.html> for details about
;; Org syntax.
;;
;; Lisp-wise, a syntax object can be represented as a list.
;; It follows the pattern (TYPE PROPERTIES CONTENTS), where:
;;   TYPE is a symbol describing the object.
;;   PROPERTIES is the property list attached to it.  See docstring of
;;              appropriate parsing function to get an exhaustive list.
;;   CONTENTS is a list of syntax objects or raw strings contained
;;            in the current object, when applicable.
;;
;; For the whole document, TYPE is `org-data' and PROPERTIES is nil.
;;
;; The first part of this file defines constants for the Org syntax,
;; while the second one provide accessors and setters functions.
;;
;; The next part implements a parser and an interpreter for each
;; element and object type in Org syntax.
;;
;; The following part creates a fully recursive buffer parser.  It
;; also provides a tool to map a function to elements or objects
;; matching some criteria in the parse tree.  Functions of interest
;; are `org-element-parse-buffer', `org-element-map' and, to a lesser
;; extent, `org-element-parse-secondary-string'.
;;
;; The penultimate part is the cradle of an interpreter for the
;; obtained parse tree: `org-element-interpret-data'.
;;
;; The library ends by furnishing `org-element-at-point' function, and
;; a way to give information about document structure around point
;; with `org-element-context'.  A cache mechanism is also provided for
;; these functions.


;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'avl-tree)
(require 'ring)
(require 'cl-lib)
(require 'ol)
(require 'org)
(require 'org-persist)
(require 'org-compat)
(require 'org-entities)
(require 'org-footnote)
(require 'org-list)
(require 'org-macs)
(require 'org-table)
(require 'org-fold-core)

(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-escape-code-in-string "org-src" (s))
(declare-function org-macro-escape-arguments "org-macro" (&rest args))
(declare-function org-macro-extract-arguments "org-macro" (s))
(declare-function org-reduced-level "org" (l))
(declare-function org-unescape-code-in-string "org-src" (s))
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())
(declare-function outline-next-heading "outline" ())
(declare-function outline-previous-heading "outline" ())

(defvar org-complex-heading-regexp)
(defvar org-done-keywords)
(defvar org-edit-src-content-indentation)
(defvar org-match-substring-regexp)
(defvar org-odd-levels-only)
(defvar org-property-drawer-re)
(defvar org-property-format)
(defvar org-property-re)
(defvar org-src-preserve-indentation)
(defvar org-tags-column)
(defvar org-todo-regexp)
(defvar org-ts-regexp-both)


;;; Definitions And Rules
;;
;; Define elements, greater elements and specify recursive objects,
;; along with the affiliated keywords recognized.  Also set up
;; restrictions on recursive objects combinations.
;;
;; `org-element-update-syntax' builds proper syntax regexps according
;; to current setup.

(defconst org-element-archive-tag "ARCHIVE"
  "Tag marking a substree as archived.")

(defconst org-element-citation-key-re
  (rx "@" (group (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
  "Regexp matching a citation key.
Key is located in match group 1.")

(defconst org-element-citation-prefix-re
  (rx "[cite"
      (opt "/" (group (one-or-more (any "/_-" alnum)))) ;style
      ":"
      (zero-or-more (any "\t\n ")))
  "Regexp matching a citation prefix.
Style, if any, is located in match group 1.")

(defconst org-element-clock-line-re
  (rx-to-string
   `(seq
     line-start (0+ (or ?\t ?\s))
     "CLOCK: "
     (regexp ,org-ts-regexp-inactive)
     (opt "--"
          (regexp ,org-ts-regexp-inactive)
          (1+ (or ?\t ?\s)) "=>" (1+ (or ?\t ?\s))
          (1+ digit) ":" digit digit)
     (0+ (or ?\t ?\s))
     line-end))
  "Regexp matching a clock line.")

(defconst org-element-comment-string "COMMENT"
  "String marker for commented headlines.")

(defconst org-element-closed-keyword "CLOSED:"
  "Keyword used to close TODO entries.")

(defconst org-element-deadline-keyword "DEADLINE:"
  "Keyword used to mark deadline entries.")

(defconst org-element-scheduled-keyword "SCHEDULED:"
  "Keyword used to mark scheduled entries.")

(defconst org-element-planning-keywords-re
  (regexp-opt (list org-element-closed-keyword
                    org-element-deadline-keyword
                    org-element-scheduled-keyword))
  "Regexp matching any planning line keyword.")

(defconst org-element-planning-line-re
  (rx-to-string
   `(seq line-start (0+ (any ?\s ?\t))
         (group (regexp ,org-element-planning-keywords-re))))
  "Regexp matching a planning line.")

(defconst org-element-drawer-re
  (rx line-start (0+ (any ?\s ?\t))
      ":" (group (1+ (any ?- ?_ word))) ":"
      (0+ (any ?\s ?\t)) line-end)
  "Regexp matching opening or closing line of a drawer.
Drawer's name is located in match group 1.")

(defconst org-element-dynamic-block-open-re
  (rx line-start (0+ (any ?\s ?\t))
      "#+BEGIN:" (0+ (any ?\s ?\t))
      (group (1+ word))
      (opt
       (1+ (any ?\s ?\t))
       (group (1+ nonl))))
  "Regexp matching the opening line of a dynamic block.
Dynamic block's name is located in match group 1.
Parameters are in match group 2.")

(defconst org-element-headline-re
  (rx line-start (1+ "*") " ")
  "Regexp matching a headline.")

(defvar org-element-paragraph-separate nil
  "Regexp to separate paragraphs in an Org buffer.
In the case of lines starting with \"#\" and \":\", this regexp
is not sufficient to know if point is at a paragraph ending.  See
`org-element-paragraph-parser' for more information.")

(defvar org-element--object-regexp nil
  "Regexp possibly matching the beginning of an object.
This regexp allows false positives.  Dedicated parser (e.g.,
`org-element-bold-parser') will take care of further filtering.
Radio links are not matched by this regexp, as they are treated
specially in `org-element--object-lex'.")

(defun org-element--set-regexps ()
  "Build variable syntax regexps."
  (setq org-element-paragraph-separate
	(concat "^\\(?:"
		;; Headlines, inlinetasks.
		"\\*+ " "\\|"
		;; Footnote definitions.
		"\\[fn:[-_[:word:]]+\\]" "\\|"
		;; Diary sexps.
		"%%(" "\\|"
		"[ \t]*\\(?:"
		;; Empty lines.
		"$" "\\|"
		;; Tables (any type).
		"|" "\\|"
		"\\+\\(?:-+\\+\\)+[ \t]*$" "\\|"
		;; Comments, keyword-like or block-like constructs.
		;; Blocks and keywords with dual values need to be
		;; double-checked.
		"#\\(?: \\|$\\|\\+\\(?:"
		"BEGIN_\\S-+" "\\|"
		"\\S-+\\(?:\\[.*\\]\\)?:[ \t]*\\)\\)"
		"\\|"
		;; Drawers (any type) and fixed-width areas.  Drawers
		;; need to be double-checked.
		":\\(?: \\|$\\|[-_[:word:]]+:[ \t]*$\\)" "\\|"
		;; Horizontal rules.
		"-\\{5,\\}[ \t]*$" "\\|"
		;; LaTeX environments.
		"\\\\begin{\\([A-Za-z0-9*]+\\)}" "\\|"
		;; Clock lines.
		org-element-clock-line-re "\\|"
		;; Lists.
		(let ((term (pcase org-plain-list-ordered-item-terminator
			      (?\) ")") (?. "\\.") (_ "[.)]")))
		      (alpha (and org-list-allow-alphabetical "\\|[A-Za-z]")))
		  (concat "\\(?:[-+*]\\|\\(?:[0-9]+" alpha "\\)" term "\\)"
			  "\\(?:[ \t]\\|$\\)"))
		"\\)\\)")
	org-element--object-regexp
	(mapconcat #'identity
		   (let ((link-types (regexp-opt (org-link-types))))
		     (list
		      ;; Sub/superscript.
		      "\\(?:[_^][-{(*+.,[:alnum:]]\\)"
		      ;; Bold, code, italic, strike-through, underline
		      ;; and verbatim.
                      (rx (or "*" "~" "=" "+" "_" "/") (not space))
		      ;; Plain links.
		      (concat "\\<" link-types ":")
		      ;; Objects starting with "[": citations,
		      ;; footnote reference, statistics cookie,
		      ;; timestamp (inactive) and regular link.
		      (format "\\[\\(?:%s\\)"
			      (mapconcat
			       #'identity
			       (list "cite[:/]"
				     "fn:"
				     "\\(?:[0-9]\\|\\(?:%\\|/[0-9]*\\)\\]\\)"
				     "\\[")
			       "\\|"))
		      ;; Objects starting with "@": export snippets.
		      "@@"
		      ;; Objects starting with "{": macro.
		      "{{{"
		      ;; Objects starting with "<" : timestamp
		      ;; (active, diary), target, radio target and
		      ;; angular links.
		      (concat "<\\(?:%%\\|<\\|[0-9]\\|" link-types "\\)")
		      ;; Objects starting with "$": latex fragment.
		      "\\$"
		      ;; Objects starting with "\": line break,
		      ;; entity, latex fragment.
		      "\\\\\\(?:[a-zA-Z[(]\\|\\\\[ \t]*$\\|_ +\\)"
		      ;; Objects starting with raw text: inline Babel
		      ;; source block, inline Babel call.
		      "\\(?:call\\|src\\)_"))
		   "\\|")))

(org-element--set-regexps)

;;;###autoload
(defun org-element-update-syntax ()
  "Update parser internals."
  (interactive)
  (org-element--set-regexps)
  (org-element-cache-reset 'all))

(defconst org-element-all-elements
  '(babel-call center-block clock comment comment-block diary-sexp drawer
	       dynamic-block example-block export-block fixed-width
	       footnote-definition headline horizontal-rule inlinetask item
	       keyword latex-environment node-property paragraph plain-list
	       planning property-drawer quote-block section
	       special-block src-block table table-row verse-block)
  "Complete list of element types.")

(defconst org-element-greater-elements
  '(center-block drawer dynamic-block footnote-definition headline inlinetask
		 item plain-list property-drawer quote-block section
		 special-block table org-data)
  "List of recursive element types aka Greater Elements.")

(defconst org-element-all-objects
  '(bold citation citation-reference code entity export-snippet
	 footnote-reference inline-babel-call inline-src-block italic line-break
	 latex-fragment link macro radio-target statistics-cookie strike-through
	 subscript superscript table-cell target timestamp underline verbatim)
  "Complete list of object types.")

(defconst org-element-recursive-objects
  '(bold citation footnote-reference italic link subscript radio-target
	 strike-through superscript table-cell underline)
  "List of recursive object types.")

(defconst org-element-object-containers
  (append org-element-recursive-objects '(paragraph table-row verse-block))
  "List of object or element types that can directly contain objects.")

(defconst org-element-affiliated-keywords
  '("CAPTION" "DATA" "HEADER" "HEADERS" "LABEL" "NAME" "PLOT" "RESNAME" "RESULT"
    "RESULTS" "SOURCE" "SRCNAME" "TBLNAME")
  "List of affiliated keywords as strings.
By default, all keywords setting attributes (e.g., \"ATTR_LATEX\")
are affiliated keywords and need not to be in this list.")

(defconst org-element-keyword-translation-alist
  '(("DATA" . "NAME")  ("LABEL" . "NAME") ("RESNAME" . "NAME")
    ("SOURCE" . "NAME") ("SRCNAME" . "NAME") ("TBLNAME" . "NAME")
    ("RESULT" . "RESULTS") ("HEADERS" . "HEADER"))
  "Alist of usual translations for keywords.
The key is the old name and the value the new one.  The property
holding their value will be named after the translated name.")

(defconst org-element-multiple-keywords '("CAPTION" "HEADER")
  "List of affiliated keywords that can occur more than once in an element.

Their value will be consed into a list of strings, which will be
returned as the value of the property.

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.

By default, all keywords setting attributes (e.g., \"ATTR_LATEX\")
allow multiple occurrences and need not to be in this list.")

(defconst org-element-parsed-keywords '("CAPTION")
  "List of affiliated keywords whose value can be parsed.

Their value will be stored as a secondary string: a list of
strings and objects.

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element--parsed-properties-alist
  (mapcar (lambda (k) (cons k (intern (concat ":" (downcase k)))))
	  org-element-parsed-keywords)
  "Alist of parsed keywords and associated properties.
This is generated from `org-element-parsed-keywords', which
see.")

(defconst org-element-dual-keywords '("CAPTION" "RESULTS")
  "List of affiliated keywords which can have a secondary value.

In Org syntax, they can be written with optional square brackets
before the colons.  For example, RESULTS keyword can be
associated to a hash value with the following:

  #+RESULTS[hash-string]: some-source

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element--affiliated-re
  (format "[ \t]*#\\+\\(?:%s\\):[ \t]*"
	  (concat
	   ;; Dual affiliated keywords.
	   (format "\\(?1:%s\\)\\(?:\\[\\(.*\\)\\]\\)?"
		   (regexp-opt org-element-dual-keywords))
	   "\\|"
	   ;; Regular affiliated keywords.
	   (format "\\(?1:%s\\)"
		   (regexp-opt
		    (cl-remove-if
		     (lambda (k) (member k org-element-dual-keywords))
		     org-element-affiliated-keywords)))
	   "\\|"
	   ;; Export attributes.
	   "\\(?1:ATTR_[-_A-Za-z0-9]+\\)"))
  "Regexp matching any affiliated keyword.

Keyword name is put in match group 1.  Moreover, if keyword
belongs to `org-element-dual-keywords', put the dual value in
match group 2.

Don't modify it, set `org-element-affiliated-keywords' instead.")

(defconst org-element-object-restrictions
  (let* ((minimal-set '(bold code entity italic latex-fragment strike-through
			     subscript superscript underline verbatim))
	 (standard-set
	  (remq 'citation-reference (remq 'table-cell org-element-all-objects)))
	 (standard-set-no-line-break (remq 'line-break standard-set)))
    `((bold ,@standard-set)
      (citation citation-reference)
      (citation-reference ,@minimal-set)
      (footnote-reference ,@standard-set)
      (headline ,@standard-set-no-line-break)
      (inlinetask ,@standard-set-no-line-break)
      (italic ,@standard-set)
      (item ,@standard-set-no-line-break)
      (keyword ,@(remq 'footnote-reference standard-set))
      ;; Ignore all links in a link description.  Also ignore
      ;; radio-targets and line breaks.
      (link export-snippet inline-babel-call inline-src-block macro
	    statistics-cookie ,@minimal-set)
      (paragraph ,@standard-set)
      ;; Remove any variable object from radio target as it would
      ;; prevent it from being properly recognized.
      (radio-target ,@minimal-set)
      (strike-through ,@standard-set)
      (subscript ,@standard-set)
      (superscript ,@standard-set)
      ;; Ignore inline babel call and inline source block as formulas
      ;; are possible.  Also ignore line breaks and statistics
      ;; cookies.
      (table-cell citation export-snippet footnote-reference link macro
                  radio-target target timestamp ,@minimal-set)
      (table-row table-cell)
      (underline ,@standard-set)
      (verse-block ,@standard-set)))
  "Alist of objects restrictions.

key is an element or object type containing objects and value is
a list of types that can be contained within an element or object
of such type.

This alist also applies to secondary string.  For example, an
`headline' type element doesn't directly contain objects, but
still has an entry since one of its properties (`:title') does.")

(defconst org-element-secondary-value-alist
  '((citation :prefix :suffix)
    (headline :title)
    (inlinetask :title)
    (item :tag)
    (citation-reference :prefix :suffix))
  "Alist between element types and locations of secondary values.")

(defconst org-element--pair-round-table
  (let ((table (make-char-table 'syntax-table '(2))))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    table)
  "Table used internally to pair only round brackets.")

(defconst org-element--pair-square-table
  (let ((table (make-char-table 'syntax-table '(2))))
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Table used internally to pair only square brackets.")

(defconst org-element--pair-curly-table
  (let ((table (make-char-table 'syntax-table '(2))))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "Table used internally to pair only curly brackets.")

(defun org-element--parse-paired-brackets (char)
  "Parse paired brackets at point.
CHAR is the opening bracket to consider, as a character.  Return
contents between brackets, as a string, or nil.  Also move point
past the brackets."
  (when (eq char (char-after))
    (let ((syntax-table (pcase char
			  (?\{ org-element--pair-curly-table)
			  (?\[ org-element--pair-square-table)
			  (?\( org-element--pair-round-table)
			  (_ nil)))
	  (pos (point)))
      (when syntax-table
	(with-syntax-table syntax-table
	  (let ((end (ignore-errors (scan-lists pos 1 0))))
	    (when end
	      (goto-char end)
	      (buffer-substring-no-properties (1+ pos) (1- end)))))))))


;;; Accessors and Setters
;;
;; Provide four accessors: `org-element-type', `org-element-property'
;; `org-element-contents' and `org-element-restriction'.
;;
;; Setter functions allow modification of elements by side effect.
;; There is `org-element-put-property', `org-element-set-contents'.
;; These low-level functions are useful to build a parse tree.
;;
;; `org-element-adopt-elements', `org-element-set-element',
;; `org-element-extract-element' and `org-element-insert-before' are
;; high-level functions useful to modify a parse tree.
;;
;; `org-element-secondary-p' is a predicate used to know if a given
;; object belongs to a secondary string.  `org-element-class' tells if
;; some parsed data is an element or an object, handling pseudo
;; elements and objects.  `org-element-copy' returns an element or
;; object, stripping its parent property in the process.

(defsubst org-element-type (element)
  "Return type of ELEMENT.

The function returns the type of the element or object provided.
It can also return the following special value:
  `plain-text'       for a string
  `org-data'         for a complete document
  nil                in any other case."
  (cond
   ((not (consp element)) (and (stringp element) 'plain-text))
   ((symbolp (car element)) (car element))))

(defsubst org-element-property (property element)
  "Extract the value from the PROPERTY of an ELEMENT."
  (if (stringp element) (get-text-property 0 property element)
    (plist-get (nth 1 element) property)))

(defsubst org-element-contents (element)
  "Extract contents from an ELEMENT."
  (cond ((not (consp element)) nil)
	((symbolp (car element)) (nthcdr 2 element))
	(t element)))

(defsubst org-element-restriction (element)
  "Return restriction associated to ELEMENT.
ELEMENT can be an element, an object or a symbol representing an
element or object type."
  (cdr (assq (if (symbolp element) element (org-element-type element))
	     org-element-object-restrictions)))

(defsubst org-element-put-property (element property value)
  "In ELEMENT set PROPERTY to VALUE.
Return modified element."
  (if (stringp element) (org-add-props element nil property value)
    (setcar (cdr element) (plist-put (nth 1 element) property value))
    element))

(defsubst org-element-set-contents (element &rest contents)
  "Set ELEMENT's contents to CONTENTS.
Return ELEMENT."
  (cond ((null element) contents)
	((not (symbolp (car element))) contents)
	((cdr element) (setcdr (cdr element) contents) element)
	(t (nconc element contents))))

(defun org-element-secondary-p (object)
  "Non-nil when OBJECT directly belongs to a secondary string.
Return value is the property name, as a keyword, or nil."
  (let* ((parent (org-element-property :parent object))
	 (properties (cdr (assq (org-element-type parent)
				org-element-secondary-value-alist))))
    (catch 'exit
      (dolist (p properties)
	(and (memq object (org-element-property p parent))
	     (throw 'exit p))))))

(defsubst org-element-class (datum &optional parent)
  "Return class for ELEMENT, as a symbol.
Class is either `element' or `object'.  Optional argument PARENT
is the element or object containing DATUM.  It defaults to the
value of DATUM `:parent' property."
  (let ((type (org-element-type datum))
	(parent (or parent (org-element-property :parent datum))))
    (cond
     ;; Trivial cases.
     ((memq type org-element-all-objects) 'object)
     ((memq type org-element-all-elements) 'element)
     ;; Special cases.
     ((eq type 'org-data) 'element)
     ((eq type 'plain-text) 'object)
     ((not type) 'object)
     ;; Pseudo object or elements.  Make a guess about its class.
     ;; Basically a pseudo object is contained within another object,
     ;; a secondary string or a container element.
     ((not parent) 'element)
     (t
      (let ((parent-type (org-element-type parent)))
	(cond ((not parent-type) 'object)
	      ((memq parent-type org-element-object-containers) 'object)
	      ((org-element-secondary-p datum) 'object)
	      (t 'element)))))))

(defsubst org-element-adopt-elements (parent &rest children)
  "Append elements to the contents of another element.

PARENT is an element or object.  CHILDREN can be elements,
objects, or a strings.

The function takes care of setting `:parent' property for CHILD.
Return parent element."
  (declare (indent 1))
  (if (not children) parent
    ;; Link every child to PARENT. If PARENT is nil, it is a secondary
    ;; string: parent is the list itself.
    (dolist (child children)
      (when child
        (org-element-put-property child :parent (or parent children))))
    ;; Add CHILDREN at the end of PARENT contents.
    (when parent
      (apply #'org-element-set-contents
	     parent
	     (nconc (org-element-contents parent) children)))
    ;; Return modified PARENT element.
    (or parent children)))

(defun org-element-extract-element (element)
  "Extract ELEMENT from parse tree.
Remove element from the parse tree by side-effect, and return it
with its `:parent' property stripped out."
  (let ((parent (org-element-property :parent element))
	(secondary (org-element-secondary-p element)))
    (if secondary
        (org-element-put-property
	 parent secondary
	 (delq element (org-element-property secondary parent)))
      (apply #'org-element-set-contents
	     parent
	     (delq element (org-element-contents parent))))
    ;; Return ELEMENT with its :parent removed.
    (org-element-put-property element :parent nil)))

(defun org-element-insert-before (element location)
  "Insert ELEMENT before LOCATION in parse tree.
LOCATION is an element, object or string within the parse tree.
Parse tree is modified by side effect."
  (let* ((parent (org-element-property :parent location))
	 (property (org-element-secondary-p location))
	 (siblings (if property (org-element-property property parent)
		     (org-element-contents parent)))
	 ;; Special case: LOCATION is the first element of an
	 ;; independent secondary string (e.g. :title property).  Add
	 ;; ELEMENT in-place.
	 (specialp (and (not property)
			(eq siblings parent)
			(eq (car parent) location))))
    ;; Install ELEMENT at the appropriate LOCATION within SIBLINGS.
    (cond (specialp)
	  ((or (null siblings) (eq (car siblings) location))
	   (push element siblings))
	  ((null location) (nconc siblings (list element)))
	  (t
	   (let ((index (cl-position location siblings)))
	     (unless index (error "No location found to insert element"))
	     (push element (cdr (nthcdr (1- index) siblings))))))
    ;; Store SIBLINGS at appropriate place in parse tree.
    (cond
     (specialp (setcdr parent (copy-sequence parent)) (setcar parent element))
     (property (org-element-put-property parent property siblings))
     (t (apply #'org-element-set-contents parent siblings)))
    ;; Set appropriate :parent property.
    (org-element-put-property element :parent parent)))

(defconst org-element--cache-element-properties
  '(:cached
    :org-element--cache-sync-key)
  "List of element properties used internally by cache.")

(defun org-element-set-element (old new)
  "Replace element or object OLD with element or object NEW.
The function takes care of setting `:parent' property for NEW."
  ;; Ensure OLD and NEW have the same parent.
  (org-element-put-property new :parent (org-element-property :parent old))
  (dolist (p org-element--cache-element-properties)
    (when (org-element-property p old)
      (org-element-put-property new p (org-element-property p old))))
  (if (or (memq (org-element-type old) '(plain-text nil))
	  (memq (org-element-type new) '(plain-text nil)))
      ;; We cannot replace OLD with NEW since one of them is not an
      ;; object or element.  We take the long path.
      (progn (org-element-insert-before new old)
	     (org-element-extract-element old))
    ;; Since OLD is going to be changed into NEW by side-effect, first
    ;; make sure that every element or object within NEW has OLD as
    ;; parent.
    (dolist (blob (org-element-contents new))
      (org-element-put-property blob :parent old))
    ;; Transfer contents.
    (apply #'org-element-set-contents old (org-element-contents new))
    ;; Overwrite OLD's properties with NEW's.
    (setcar (cdr old) (nth 1 new))
    ;; Transfer type.
    (setcar old (car new))))

(defun org-element-create (type &optional props &rest children)
  "Create a new element of type TYPE.
Optional argument PROPS, when non-nil, is a plist defining the
properties of the element.  CHILDREN can be elements, objects or
strings."
  (apply #'org-element-adopt-elements (list type props) children))

(defun org-element-copy (datum)
  "Return a copy of DATUM.
DATUM is an element, object, string or nil.  `:parent' property
is cleared and contents are removed in the process."
  (when datum
    (let ((type (org-element-type datum)))
      (pcase type
	(`org-data (list 'org-data nil))
	(`plain-text (substring-no-properties datum))
	(`nil (copy-sequence datum))
	(_
         (let ((element-copy (list type (plist-put (copy-sequence (nth 1 datum)) :parent nil))))
           ;; We cannot simply return the copies property list.  When
           ;; DATUM is i.e. a headline, it's property list (`:title'
           ;; in case of headline) can contain parsed objects.  The
           ;; objects will contain `:parent' property set to the DATUM
           ;; itself.  When copied, these inner `:parent' property
           ;; values will contain incorrect object decoupled from
           ;; DATUM.  Changes to the DATUM copy will not longer be
           ;; reflected in the `:parent' properties.  So, we need to
           ;; reassign inner `:parent' properties to the DATUM copy
           ;; explicitly.
           (org-element-map element-copy (cons 'plain-text org-element-all-objects)
             (lambda (obj) (when (equal datum (org-element-property :parent obj))
                        (org-element-put-property obj :parent element-copy))))
           element-copy))))))



;;; Greater elements
;;
;; For each greater element type, we define a parser and an
;; interpreter.
;;
;; A parser returns the element or object as the list described above.
;; Most of them accepts no argument.  Though, exceptions exist.  Hence
;; every element containing a secondary string (see
;; `org-element-secondary-value-alist') will accept an optional
;; argument to toggle parsing of these secondary strings.  Moreover,
;; `item' parser requires current list's structure as its first
;; element.
;;
;; An interpreter accepts two arguments: the list representation of
;; the element or object, and its contents.  The latter may be nil,
;; depending on the element or object considered.  It returns the
;; appropriate Org syntax, as a string.
;;
;; Parsing functions must follow the naming convention:
;; org-element-TYPE-parser, where TYPE is greater element's type, as
;; defined in `org-element-greater-elements'.
;;
;; Similarly, interpreting functions must follow the naming
;; convention: org-element-TYPE-interpreter.
;;
;; With the exception of `headline' and `item' types, greater elements
;; cannot contain other greater elements of their own type.
;;
;; Beside implementing a parser and an interpreter, adding a new
;; greater element requires tweaking `org-element--current-element'.
;; Moreover, the newly defined type must be added to both
;; `org-element-all-elements' and `org-element-greater-elements'.


;;;; Center Block

(defun org-element-center-block-parser (limit affiliated)
  "Parse a center block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `center-block' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_CENTER[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(let* ((begin (car affiliated))
	       (post-affiliated (point))
	       ;; Empty blocks have no contents.
	       (contents-begin (progn (forward-line)
				      (and (< (point) block-end-line)
					   (point))))
	       (contents-end (and contents-begin block-end-line))
	       (pos-before-blank (progn (goto-char block-end-line)
					(forward-line)
					(point)))
	       (end (save-excursion
		      (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
	  (list 'center-block
		(nconc
		 (list :begin begin
		       :end end
		       :contents-begin contents-begin
		       :contents-end contents-end
		       :post-blank (count-lines pos-before-blank end)
		       :post-affiliated post-affiliated)
		 (cdr affiliated))))))))

(defun org-element-center-block-interpreter (_ contents)
  "Interpret a center-block element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+begin_center\n%s#+end_center" contents))


;;;; Drawer

(defun org-element-drawer-parser (limit affiliated)
  "Parse a drawer.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `drawer' and CDR is a plist containing
`:drawer-name', `:begin', `:end', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at beginning of drawer."
  (let ((case-fold-search t))
    (if (not (save-excursion
               (goto-char (min limit (line-end-position)))
               (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)))
	;; Incomplete drawer: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (save-excursion
	(let* ((drawer-end-line (match-beginning 0))
	       (name
                (progn
                  (looking-at org-element-drawer-re)
		  (match-string-no-properties 1)))
	       (begin (car affiliated))
	       (post-affiliated (point))
	       ;; Empty drawers have no contents.
	       (contents-begin (progn (forward-line)
				      (and (< (point) drawer-end-line)
					   (point))))
	       (contents-end (and contents-begin drawer-end-line))
	       (pos-before-blank (progn (goto-char drawer-end-line)
					(forward-line)
					(point)))
	       (end (progn (skip-chars-forward " \r\t\n" limit)
			   (if (eobp) (point) (line-beginning-position)))))
	  (list 'drawer
		(nconc
		 (list :begin begin
		       :end end
		       :drawer-name name
		       :contents-begin contents-begin
		       :contents-end contents-end
		       :post-blank (count-lines pos-before-blank end)
		       :post-affiliated post-affiliated)
		 (cdr affiliated))))))))

(defun org-element-drawer-interpreter (drawer contents)
  "Interpret DRAWER element as Org syntax.
CONTENTS is the contents of the element."
  (format ":%s:\n%s:END:"
	  (org-element-property :drawer-name drawer)
	  contents))


;;;; Dynamic Block

(defun org-element-dynamic-block-parser (limit affiliated)
  "Parse a dynamic block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `dynamic-block' and CDR is a plist
containing `:block-name', `:begin', `:end', `:contents-begin',
`:contents-end', `:arguments', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at beginning of dynamic block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END:?[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(save-excursion
	  (let* ((name (progn
                         (looking-at org-element-dynamic-block-open-re)
			 (match-string-no-properties 1)))
		 (arguments (match-string-no-properties 2))
		 (begin (car affiliated))
		 (post-affiliated (point))
		 ;; Empty blocks have no contents.
		 (contents-begin (progn (forward-line)
					(and (< (point) block-end-line)
					     (point))))
		 (contents-end (and contents-begin block-end-line))
		 (pos-before-blank (progn (goto-char block-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'dynamic-block
		  (nconc
		   (list :begin begin
			 :end end
			 :block-name name
			 :arguments arguments
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-dynamic-block-interpreter (dynamic-block contents)
  "Interpret DYNAMIC-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+begin: %s%s\n%s#+end:"
	  (org-element-property :block-name dynamic-block)
	  (let ((args (org-element-property :arguments dynamic-block)))
	    (if args (concat " " args) ""))
	  contents))


;;;; Footnote Definition

(defconst org-element--footnote-separator
  (concat org-element-headline-re "\\|"
	  org-footnote-definition-re "\\|"
	  "^\\([ \t]*\n\\)\\{2,\\}")
  "Regexp used as a footnote definition separator.")

(defun org-element-footnote-definition-parser (limit affiliated)
  "Parse a footnote definition.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `footnote-definition' and CDR is
a plist containing `:label', `:begin' `:end', `:contents-begin',
`:contents-end', `:pre-blank',`:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the footnote definition."
  (save-excursion
    (let* ((label (progn (looking-at org-footnote-definition-re)
			 (match-string-no-properties 1)))
	   (begin (car affiliated))
	   (post-affiliated (point))
	   (end
	    (save-excursion
	      (end-of-line)
	      (cond
	       ((not
		 (re-search-forward org-element--footnote-separator limit t))
		limit)
	       ((eq ?\[ (char-after (match-beginning 0)))
		;; At a new footnote definition, make sure we end
		;; before any affiliated keyword above.
		(forward-line -1)
		(while (and (> (point) post-affiliated)
			    (looking-at-p org-element--affiliated-re))
		  (forward-line -1))
		(line-beginning-position 2))
	       ((eq ?* (char-after (match-beginning 0))) (match-beginning 0))
	       (t (skip-chars-forward " \r\t\n" limit)
		  (if (= limit (point)) limit (line-beginning-position))))))
	   (pre-blank 0)
	   (contents-begin
	    (progn (search-forward "]")
		   (skip-chars-forward " \r\t\n" end)
		   (cond ((= (point) end) nil)
			 ((= (line-beginning-position) post-affiliated) (point))
			 (t
			  (setq pre-blank
				(count-lines (line-beginning-position) begin))
			  (line-beginning-position)))))
	   (contents-end
	    (progn (goto-char end)
		   (skip-chars-backward " \r\t\n")
		   (line-beginning-position 2))))
      (list 'footnote-definition
	    (nconc
	     (list :label label
		   :begin begin
		   :end end
		   :contents-begin contents-begin
		   :contents-end (and contents-begin contents-end)
		   :pre-blank pre-blank
		   :post-blank (count-lines contents-end end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-footnote-definition-interpreter (footnote-definition contents)
  "Interpret FOOTNOTE-DEFINITION element as Org syntax.
CONTENTS is the contents of the footnote-definition."
  (let ((pre-blank
	 (min (or (org-element-property :pre-blank footnote-definition)
		  ;; 0 is specific to paragraphs at the beginning of
		  ;; the footnote definition, so we use 1 as
		  ;; a fall-back value, which is more universal.
		  1)
	      ;; Footnote ends after more than two consecutive empty
	      ;; lines: limit ourselves to 2 newline characters.
	      2)))
    (concat (format "[fn:%s]" (org-element-property :label footnote-definition))
	    (if (= pre-blank 0) (concat " " (org-trim contents))
	      (concat (make-string pre-blank ?\n) contents)))))

;;;; Headline

(defun org-element--get-node-properties (&optional at-point-p?)
  "Return node properties for headline or property drawer at point.
Upcase property names.  It avoids confusion between properties
obtained through property drawer and default properties from the
parser (e.g. `:end' and :END:).  Return value is a plist.

When AT-POINT-P? is nil, assume that point as at a headline.  Otherwise
parse properties for property drawer at point."
  (save-excursion
    (unless at-point-p?
      (forward-line)
      (when (looking-at-p org-element-planning-line-re) (forward-line)))
    (when (looking-at org-property-drawer-re)
      (forward-line)
      (let ((end (match-end 0)) properties)
	(while (< (line-end-position) end)
	  (looking-at org-property-re)
          (let* ((property-name (concat ":" (upcase (match-string 2))))
                 (property-name-symbol (intern property-name))
                 (property-value (match-string-no-properties 3)))
            (cond
             ((and (plist-member properties property-name-symbol)
                   (string-match-p "\\+$" property-name))
              (let ((val (plist-get properties property-name-symbol)))
                (if (listp val)
                    (setq properties
                          (plist-put properties
                                     property-name-symbol
                                     (append (plist-get properties property-name-symbol)
                                             (list property-value))))
                  (plist-put properties property-name-symbol (list val property-value)))))
             (t (setq properties (plist-put properties property-name-symbol property-value)))))
	  (forward-line))
	properties))))

(defun org-element--get-time-properties ()
  "Return time properties associated to headline at point.
Return value is a plist."
  (save-excursion
    (when (progn (forward-line) (looking-at org-element-planning-line-re))
      (let ((end (line-end-position))
            plist)
	(while (re-search-forward org-element-planning-keywords-re end t)
	  (skip-chars-forward " \t")
	  (let ((keyword (match-string 0))
		(time (org-element-timestamp-parser)))
	    (cond ((equal keyword org-element-scheduled-keyword)
		   (setq plist (plist-put plist :scheduled time)))
		  ((equal keyword org-element-deadline-keyword)
		   (setq plist (plist-put plist :deadline time)))
		  (t (setq plist (plist-put plist :closed time))))))
	plist))))

(defun org-element-headline-parser (&optional _ raw-secondary-p)
  "Parse a headline.

Return a list whose CAR is `headline' and CDR is a plist
containing `:raw-value', `:title', `:begin', `:end',
`:pre-blank', `:contents-begin' and `:contents-end', `:level',
`:priority', `:tags', `:todo-keyword', `:todo-type', `:scheduled',
`:deadline', `:closed', `:archivedp', `:commentedp'
`:footnote-section-p', `:post-blank' and `:post-affiliated'
keywords.

The plist also contains any property set in the property drawer,
with its name in upper cases and colons added at the
beginning (e.g., `:CUSTOM_ID').

When RAW-SECONDARY-P is non-nil, headline's title will not be
parsed as a secondary string, but as a plain string instead.

Assume point is at beginning of the headline."
  (save-excursion
    (let* ((begin (point))
           (true-level (prog1 (skip-chars-forward "*")
                         (skip-chars-forward " \t")))
	   (level (org-reduced-level true-level))
	   (todo (and org-todo-regexp
		      (let (case-fold-search) (looking-at (concat org-todo-regexp " ")))
		      (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")
			     (match-string 1))))
	   (todo-type
	    (and todo (if (member todo org-done-keywords) 'done 'todo)))
	   (priority (and (looking-at "\\[#.\\][ \t]*")
			  (progn (goto-char (match-end 0))
				 (aref (match-string 0) 2))))
	   (commentedp
	    (and (let ((case-fold-search nil))
                   (looking-at org-element-comment-string))
		 (goto-char (match-end 0))
                 (when (looking-at-p "\\(?:[ \t]\\|$\\)")
                   (point))))
	   (title-start (prog1 (point)
                          (unless (or todo priority commentedp)
                            ;; Headline like "* :tag:"
                            (skip-chars-backward " \t"))))
	   (tags (when (re-search-forward
			"[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
			(line-end-position)
			'move)
		   (goto-char (match-beginning 0))
		   (org-split-string (match-string 1) ":")))
	   (title-end (point))
	   (raw-value (org-trim
		       (buffer-substring-no-properties title-start title-end)))
	   (archivedp (member org-element-archive-tag tags))
	   (footnote-section-p (and org-footnote-section
				    (string= org-footnote-section raw-value)))
	   (standard-props (org-element--get-node-properties))
	   (time-props (org-element--get-time-properties))
	   (end
            (save-excursion
              (let ((re (rx-to-string
                         `(seq line-start (** 1 ,true-level "*") " "))))
                (if (re-search-forward re nil t)
                    (line-beginning-position)
                  (point-max)))))
	   (contents-begin (save-excursion
			     (forward-line)
			     (skip-chars-forward " \r\t\n" end)
			     (and (/= (point) end) (line-beginning-position))))
	   (contents-end (and contents-begin
			      (progn (goto-char end)
				     (skip-chars-backward " \r\t\n")
				     (line-beginning-position 2))))
           (robust-begin (and contents-begin
                              (progn (goto-char contents-begin)
                                     (when (looking-at-p org-element-planning-line-re)
                                       (forward-line))
                                     (when (looking-at org-property-drawer-re)
                                       (goto-char (match-end 0)))
                                     ;; If there is :pre-blank, we
                                     ;; need to be careful about
                                     ;; robust beginning.
                                     (max (if (< (+ 2 contents-begin) contents-end)
                                              (+ 2 contents-begin)
                                            0)
                                          (point)))))
           (robust-end (and robust-begin
                            (when (> (- contents-end 2) robust-begin)
                              (- contents-end 2)))))
      (unless robust-end (setq robust-begin nil))
      (let ((headline
	     (list 'headline
		   (nconc
		    (list :raw-value raw-value
			  :begin begin
			  :end end
			  :pre-blank
			  (if (not contents-begin) 0
			    (1- (count-lines begin contents-begin)))
			  :contents-begin contents-begin
			  :contents-end contents-end
                          :robust-begin robust-begin
                          :robust-end robust-end
			  :level level
			  :priority priority
			  :tags tags
			  :todo-keyword todo
			  :todo-type todo-type
			  :post-blank
			  (if contents-end
			      (count-lines contents-end end)
			    (1- (count-lines begin end)))
			  :footnote-section-p footnote-section-p
			  :archivedp archivedp
			  :commentedp commentedp
			  :post-affiliated begin)
		    time-props
		    standard-props))))
	(org-element-put-property
	 headline :title
	 (if raw-secondary-p raw-value
	   (org-element--parse-objects
	    (progn (goto-char title-start)
		   (skip-chars-forward " \t")
		   (point))
	    (progn (goto-char title-end)
		   (skip-chars-backward " \t")
		   (point))
	    nil
	    (org-element-restriction 'headline)
	    headline)))))))

(defun org-element-headline-interpreter (headline contents)
  "Interpret HEADLINE element as Org syntax.
CONTENTS is the contents of the element."
  (let* ((level (org-element-property :level headline))
	 (todo (org-element-property :todo-keyword headline))
	 (priority (org-element-property :priority headline))
	 (title (org-element-interpret-data
		 (org-element-property :title headline)))
	 (tags (let ((tag-list (org-element-property :tags headline)))
		 (and tag-list
		      (format ":%s:" (mapconcat #'identity tag-list ":")))))
	 (commentedp (org-element-property :commentedp headline))
	 (pre-blank (or (org-element-property :pre-blank headline) 0))
	 (heading
	  (concat (make-string (if org-odd-levels-only (1- (* level 2)) level)
			       ?*)
		  (and todo (concat " " todo))
		  (and commentedp (concat " " org-element-comment-string))
		  (and priority (format " [#%c]" priority))
		  " "
		  (if (and org-footnote-section
			   (org-element-property :footnote-section-p headline))
		      org-footnote-section
		    title))))
    (concat
     heading
     ;; Align tags.
     (when tags
       (cond
	((zerop org-tags-column) (format " %s" tags))
	((< org-tags-column 0)
	 (concat
	  (make-string
	   (max (- (+ org-tags-column (length heading) (length tags))) 1)
	   ?\s)
	  tags))
	(t
	 (concat
	  (make-string (max (- org-tags-column (length heading)) 1) ?\s)
	  tags))))
     (make-string (1+ pre-blank) ?\n)
     contents)))

;;;; org-data

(defun org-element--get-global-node-properties ()
  "Return node properties associated with the whole Org buffer.
Upcase property names.  It avoids confusion between properties
obtained through property drawer and default properties from the
parser (e.g. `:end' and :END:).  Return value is a plist."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (and (org-at-comment-p) (bolp)) (forward-line))
   (org-element--get-node-properties t)))


(defvar org-element-org-data-parser--recurse nil)
(defun org-element-org-data-parser (&optional _)
  "Parse org-data."
  (org-with-wide-buffer
   (let* ((begin 1)
          (contents-begin (progn
                            (goto-char 1)
                            (org-skip-whitespace)
                            (beginning-of-line)
                            (point)))
	  (end (point-max))
	  (pos-before-blank (progn (goto-char (point-max))
                                   (skip-chars-backward " \r\t\n")
                                   (line-beginning-position 2)))
          (robust-end (when (> (- pos-before-blank 2) contents-begin)
                        (- pos-before-blank 2)))
          (robust-begin (when (and robust-end
                                   (< (+ 2 contents-begin) pos-before-blank))
                          (or
                           (org-with-wide-buffer
                            (goto-char (point-min))
                            (while (and (org-at-comment-p) (bolp)) (forward-line))
                            (when (looking-at org-property-drawer-re)
                              (goto-char (match-end 0))
                              (skip-chars-backward " \t")
                              (min robust-end (point))))
                           (+ 2 contents-begin))))
          (category (cond ((null org-category)
		           (when (org-with-base-buffer nil
                                   buffer-file-name)
		             (file-name-sans-extension
		              (file-name-nondirectory
                               (org-with-base-buffer nil
                                 buffer-file-name)))))
		          ((symbolp org-category) (symbol-name org-category))
		          (t org-category)))
          (category (catch 'buffer-category
                      (unless org-element-org-data-parser--recurse
                        (org-with-point-at end
                          ;; Avoid recursive calls from
                          ;; `org-element-at-point-no-context'.
                          (let ((org-element-org-data-parser--recurse t))
	                    (while (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t)
                              (org-element-with-disabled-cache
	                        (let ((element (org-element-at-point-no-context)))
	                          (when (eq (org-element-type element) 'keyword)
		                    (throw 'buffer-category
		                           (org-element-property :value element)))))))))
	              category))
          (properties (org-element--get-global-node-properties)))
     (unless (plist-get properties :CATEGORY)
       (setq properties (plist-put properties :CATEGORY category)))
     (list 'org-data
           (nconc
            (list :begin begin
                  :contents-begin contents-begin
                  :contents-end pos-before-blank
                  :end end
                  :robust-begin robust-begin
                  :robust-end robust-end
                  :post-blank (count-lines pos-before-blank end)
                  :post-affiliated begin
                  :path (buffer-file-name)
                  :mode 'org-data)
            properties)))))

(defun org-element-org-data-interpreter (_ contents)
  "Interpret ORG-DATA element as Org syntax.
CONTENTS is the contents of the element."
  contents)

;;;; Inlinetask

(defun org-element-inlinetask-parser (limit &optional raw-secondary-p)
  "Parse an inline task.

Return a list whose CAR is `inlinetask' and CDR is a plist
containing `:title', `:begin', `:end', `:pre-blank',
`:contents-begin' and `:contents-end', `:level', `:priority',
`:raw-value', `:tags', `:todo-keyword', `:todo-type',
`:scheduled', `:deadline', `:closed', `:post-blank' and
`:post-affiliated' keywords.

The plist also contains any property set in the property drawer,
with its name in upper cases and colons added at the
beginning (e.g., `:CUSTOM_ID').

When optional argument RAW-SECONDARY-P is non-nil, inline-task's
title will not be parsed as a secondary string, but as a plain
string instead.

Assume point is at beginning of the inline task."
  (save-excursion
    (let* ((begin (point))
	   (level (prog1 (org-reduced-level (skip-chars-forward "*"))
		    (skip-chars-forward " \t")))
	   (todo (and org-todo-regexp
		      (let (case-fold-search) (looking-at org-todo-regexp))
		      (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")
			     (match-string 0))))
	   (todo-type (and todo
			   (if (member todo org-done-keywords) 'done 'todo)))
	   (priority (and (looking-at "\\[#.\\][ \t]*")
			  (progn (goto-char (match-end 0))
				 (aref (match-string 0) 2))))
           (commentedp
	    (and (let ((case-fold-search nil))
                   (looking-at org-element-comment-string))
		 (goto-char (match-end 0))
                 (when (looking-at-p "\\(?:[ \t]\\|$\\)")
                   (point))))
	   (title-start (prog1 (point)
                          (unless (or todo priority commentedp)
                            ;; Headline like "* :tag:"
                            (skip-chars-backward " \t"))))
	   (tags (when (re-search-forward
			"[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
			(line-end-position)
			'move)
		   (goto-char (match-beginning 0))
		   (org-split-string (match-string 1) ":")))
	   (title-end (point))
	   (raw-value (org-trim
		       (buffer-substring-no-properties title-start title-end)))
           (archivedp (member org-element-archive-tag tags))
	   (task-end (save-excursion
		       (end-of-line)
		       (and (re-search-forward org-element-headline-re limit t)
			    (looking-at-p "[ \t]*END[ \t]*$")
			    (line-beginning-position))))
	   (standard-props (and task-end (org-element--get-node-properties)))
	   (time-props (and task-end (org-element--get-time-properties)))
	   (contents-begin (and task-end
				(< (point) task-end)
				(progn
				  (forward-line)
				  (skip-chars-forward " \t\n")
				  (line-beginning-position))))
	   (contents-end (and contents-begin task-end))
	   (end (progn (when task-end (goto-char task-end))
		       (forward-line)
		       (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position))))
	   (inlinetask
	    (list 'inlinetask
		  (nconc
		   (list :raw-value raw-value
			 :begin begin
			 :end end
			 :pre-blank
			 (if (not contents-begin) 0
			   (1- (count-lines begin contents-begin)))
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :level level
			 :priority priority
			 :tags tags
			 :todo-keyword todo
			 :todo-type todo-type
			 :post-blank (1- (count-lines (or task-end begin) end))
			 :post-affiliated begin
                         :archivedp archivedp
			 :commentedp commentedp)
		   time-props
		   standard-props))))
      (org-element-put-property
       inlinetask :title
       (if raw-secondary-p raw-value
	 (org-element--parse-objects
	  (progn (goto-char title-start)
		 (skip-chars-forward " \t")
		 (point))
	  (progn (goto-char title-end)
		 (skip-chars-backward " \t")
		 (point))
	  nil
	  (org-element-restriction 'inlinetask)
	  inlinetask))))))

(defun org-element-inlinetask-interpreter (inlinetask contents)
  "Interpret INLINETASK element as Org syntax.
CONTENTS is the contents of inlinetask."
  (let* ((level (org-element-property :level inlinetask))
	 (todo (org-element-property :todo-keyword inlinetask))
	 (priority (org-element-property :priority inlinetask))
	 (title (org-element-interpret-data
		 (org-element-property :title inlinetask)))
	 (tags (let ((tag-list (org-element-property :tags inlinetask)))
		 (and tag-list
		      (format ":%s:" (mapconcat 'identity tag-list ":")))))
	 (task (concat (make-string level ?*)
		       (and todo (concat " " todo))
		       (and priority (format " [#%c]" priority))
		       (and title (concat " " title)))))
    (concat task
	    ;; Align tags.
	    (when tags
	      (cond
	       ((zerop org-tags-column) (format " %s" tags))
	       ((< org-tags-column 0)
		(concat
		 (make-string
		  (max (- (+ org-tags-column (length task) (length tags))) 1)
		  ?\s)
		 tags))
	       (t
		(concat
		 (make-string (max (- org-tags-column (length task)) 1) ?\s)
		 tags))))
	    ;; Prefer degenerate inlinetasks when there are no
	    ;; contents.
	    (when contents
	      (concat "\n"
		      contents
		      (make-string level ?*) " end")))))


;;;; Item

(defun org-element-item-parser (_ struct &optional raw-secondary-p)
  "Parse an item.

STRUCT is the structure of the plain list.

Return a list whose CAR is `item' and CDR is a plist containing
`:bullet', `:begin', `:end', `:contents-begin', `:contents-end',
`:checkbox', `:counter', `:tag', `:structure', `:pre-blank',
`:post-blank' and `:post-affiliated' keywords.

When optional argument RAW-SECONDARY-P is non-nil, item's tag, if
any, will not be parsed as a secondary string, but as a plain
string instead.

Assume point is at the beginning of the item."
  (save-excursion
    (beginning-of-line)
    (looking-at org-list-full-item-re)
    (let* ((begin (point))
	   (bullet (match-string-no-properties 1))
	   (checkbox (let ((box (match-string 3)))
		       (cond ((equal "[ ]" box) 'off)
			     ((equal "[X]" box) 'on)
			     ((equal "[-]" box) 'trans))))
	   (counter (let ((c (match-string 2)))
		      (save-match-data
			(cond
			 ((not c) nil)
			 ((string-match "[A-Za-z]" c)
			  (- (string-to-char (upcase (match-string 0 c)))
			     64))
			 ((string-match "[0-9]+" c)
			  (string-to-number (match-string 0 c)))))))
	   (end (progn (goto-char (nth 6 (assq (point) struct)))
		       (if (bolp) (point) (line-beginning-position 2))))
	   (pre-blank 0)
	   (contents-begin
	    (progn
	      (goto-char
	       ;; Ignore tags in un-ordered lists: they are just
	       ;; a part of item's body.
	       (if (and (match-beginning 4)
			(save-match-data (string-match "[.)]" bullet)))
		   (match-beginning 4)
		 (match-end 0)))
	      (skip-chars-forward " \r\t\n" end)
	      (cond ((= (point) end) nil)
		    ;; If first line isn't empty, contents really
		    ;; start at the text after item's meta-data.
		    ((= (line-beginning-position) begin) (point))
		    (t
		     (setq pre-blank
			   (count-lines (line-beginning-position) begin))
		     (line-beginning-position)))))
	   (contents-end (and contents-begin
			      (progn (goto-char end)
				     (skip-chars-backward " \r\t\n")
				     (line-beginning-position 2))))
	   (item
	    (list 'item
		  (list :bullet bullet
			:begin begin
			:end end
			:contents-begin contents-begin
			:contents-end contents-end
			:checkbox checkbox
			:counter counter
			:structure struct
			:pre-blank pre-blank
			:post-blank (count-lines (or contents-end begin) end)
			:post-affiliated begin))))
      (org-element-put-property
       item :tag
       (let ((raw (org-list-get-tag begin struct)))
	 (when raw
	   (if raw-secondary-p raw
	     (org-element--parse-objects
	      (match-beginning 4) (match-end 4) nil
	      (org-element-restriction 'item)
	      item))))))))

(defun org-element-item-interpreter (item contents)
  "Interpret ITEM element as Org syntax.
CONTENTS is the contents of the element."
  (let ((tag (pcase (org-element-property :tag item)
	       (`nil nil)
	       (tag (format "%s :: " (org-element-interpret-data tag)))))
	(bullet
	 (org-list-bullet-string
	  (cond
	   ((not (string-match-p "[0-9a-zA-Z]"
				 (org-element-property :bullet item))) "- ")
	   ((eq org-plain-list-ordered-item-terminator ?\)) "1)")
	   (t "1.")))))
    (concat
     bullet
     (pcase (org-element-property :counter item)
       (`nil nil)
       (counter (format "[@%d] " counter)))
     (pcase (org-element-property :checkbox item)
       (`on "[X] ")
       (`off "[ ] ")
       (`trans "[-] ")
       (_ nil))
     tag
     (when contents
       (let* ((ind (make-string (if tag 5 (length bullet)) ?\s))
	      (pre-blank
	       (min (or (org-element-property :pre-blank item)
			;; 0 is specific to paragraphs at the
			;; beginning of the item, so we use 1 as
			;; a fall-back value, which is more universal.
			1)
		    ;; Lists ends after more than two consecutive
		    ;; empty lines: limit ourselves to 2 newline
		    ;; characters.
		    2))
	      (contents (replace-regexp-in-string
			 "\\(^\\)[ \t]*\\S-" ind contents nil nil 1)))
	 (if (= pre-blank 0) (org-trim contents)
	   (concat (make-string pre-blank ?\n) contents)))))))


;;;; Plain List

(defun org-element--list-struct (limit)
  ;; Return structure of list at point.  Internal function.  See
  ;; `org-list-struct' for details.
  (let ((case-fold-search t)
	(top-ind limit)
	(item-re (org-item-re))
	(inlinetask-re (and (featurep 'org-inlinetask)
                            (boundp 'org-inlinetask-min-level)
                            (boundp 'org-inlinetask-max-level)
                            (format "^\\*\\{%d,%d\\}+ "
                                    org-inlinetask-min-level
                                    org-inlinetask-max-level)))
	items struct)
    (save-excursion
      (catch :exit
	(while t
	  (cond
	   ;; At limit: end all items.
	   ((>= (point) limit)
	    (let ((end (progn (skip-chars-backward " \r\t\n")
			      (line-beginning-position 2))))
	      (dolist (item items) (setcar (nthcdr 6 item) end)))
	    (throw :exit (sort (nconc items struct) #'car-less-than-car)))
	   ;; At list end: end all items.
	   ((looking-at org-list-end-re)
	    (dolist (item items) (setcar (nthcdr 6 item) (point)))
	    (throw :exit (sort (nconc items struct) #'car-less-than-car)))
	   ;; At a new item: end previous sibling.
	   ((looking-at item-re)
	    (let ((ind (save-excursion (skip-chars-forward " \t")
				       (org-current-text-column))))
	      (setq top-ind (min top-ind ind))
	      (while (and items (<= ind (nth 1 (car items))))
		(let ((item (pop items)))
		  (setcar (nthcdr 6 item) (point))
		  (push item struct)))
	      (push (progn (looking-at org-list-full-item-re)
			   (let ((bullet (match-string-no-properties 1)))
			     (list (point)
				   ind
				   bullet
				   (match-string-no-properties 2) ; counter
				   (match-string-no-properties 3) ; checkbox
				   ;; Description tag.
				   (and (save-match-data
					  (string-match "[-+*]" bullet))
					(match-string-no-properties 4))
				   ;; Ending position, unknown so far.
				   nil)))
		    items))
	    (forward-line))
	   ;; Skip empty lines.
	   ((looking-at "^[ \t]*$") (forward-line))
	   ;; Skip inline tasks and blank lines along the way.
	   ((and inlinetask-re (looking-at inlinetask-re))
	    (forward-line)
	    (let ((origin (point)))
	      (when (re-search-forward inlinetask-re limit t)
		(if (looking-at-p "END[ \t]*$") (forward-line)
		  (goto-char origin)))))
	   ;; At some text line.  Check if it ends any previous item.
	   (t
	    (let ((ind (save-excursion
			 (skip-chars-forward " \t")
			 (org-current-text-column)))
		  (end (save-excursion
			 (skip-chars-backward " \r\t\n")
			 (line-beginning-position 2))))
	      (while (<= ind (nth 1 (car items)))
		(let ((item (pop items)))
		  (setcar (nthcdr 6 item) end)
		  (push item struct)
		  (unless items
		    (throw :exit (sort struct #'car-less-than-car))))))
	    ;; Skip blocks (any type) and drawers contents.
	    (cond
	     ((and (looking-at "[ \t]*#\\+BEGIN\\(:\\|_\\S-+\\)")
		   (re-search-forward
		    (format "^[ \t]*#\\+END%s[ \t]*$" (match-string 1))
		    limit t)))
	     ((and (looking-at org-element-drawer-re)
		   (re-search-forward "^[ \t]*:END:[ \t]*$" limit t))))
	    (forward-line))))))))

(defun org-element-plain-list-parser (limit affiliated structure)
  "Parse a plain list.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.  STRUCTURE is the structure of the plain list being
parsed.

Return a list whose CAR is `plain-list' and CDR is a plist
containing `:type', `:begin', `:end', `:contents-begin' and
`:contents-end', `:structure', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the list."
  (save-excursion
    (let* ((struct (or structure (org-element--list-struct limit)))
	   (type (cond ((looking-at-p "[ \t]*[A-Za-z0-9]") 'ordered)
		       ((nth 5 (assq (point) struct)) 'descriptive)
		       (t 'unordered)))
	   (contents-begin (point))
	   (begin (car affiliated))
	   (contents-end (let* ((item (assq contents-begin struct))
				(ind (nth 1 item))
				(pos (nth 6 item)))
			   (while (and (setq item (assq pos struct))
				       (= (nth 1 item) ind))
			     (setq pos (nth 6 item)))
			   pos))
	   (end (progn (goto-char contents-end)
		       (skip-chars-forward " \r\t\n" limit)
		       (if (= (point) limit) limit (line-beginning-position)))))
      ;; Return value.
      (list 'plain-list
	    (nconc
	     (list :type type
		   :begin begin
		   :end end
		   :contents-begin contents-begin
		   :contents-end contents-end
		   :structure struct
		   :post-blank (count-lines contents-end end)
		   :post-affiliated contents-begin)
	     (cdr affiliated))))))

(defun org-element-plain-list-interpreter (_ contents)
  "Interpret plain-list element as Org syntax.
CONTENTS is the contents of the element."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (org-list-repair)
    (buffer-string)))


;;;; Property Drawer

(defun org-element-property-drawer-parser (limit)
  "Parse a property drawer.

LIMIT bounds the search.

Return a list whose car is `property-drawer' and cdr is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the property drawer."
  (save-excursion
    (let ((case-fold-search t)
	  (begin (point))
	  (contents-begin (line-beginning-position 2)))
      (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)
      (let ((contents-end (and (> (match-beginning 0) contents-begin)
			       (match-beginning 0)))
	    (before-blank (progn (forward-line) (point)))
	    (end (progn (skip-chars-forward " \r\t\n" limit)
			(if (eobp) (point) (line-beginning-position)))))
	(list 'property-drawer
	      (list :begin begin
		    :end end
		    :contents-begin (and contents-end contents-begin)
		    :contents-end contents-end
		    :post-blank (count-lines before-blank end)
		    :post-affiliated begin))))))

(defun org-element-property-drawer-interpreter (_ contents)
  "Interpret property-drawer element as Org syntax.
CONTENTS is the properties within the drawer."
  (format ":PROPERTIES:\n%s:END:" contents))


;;;; Quote Block

(defun org-element-quote-block-parser (limit affiliated)
  "Parse a quote block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `quote-block' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_QUOTE[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 ;; Empty blocks have no contents.
		 (contents-begin (progn (forward-line)
					(and (< (point) block-end-line)
					     (point))))
		 (contents-end (and contents-begin block-end-line))
		 (pos-before-blank (progn (goto-char block-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'quote-block
		  (nconc
		   (list :begin begin
			 :end end
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-quote-block-interpreter (_ contents)
  "Interpret quote-block element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+begin_quote\n%s#+end_quote" contents))


;;;; Section

(defun org-element-section-parser (_)
  "Parse a section.

Return a list whose CAR is `section' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `contents-end',
`:post-blank' and `:post-affiliated' keywords."
  (save-excursion
    ;; Beginning of section is the beginning of the first non-blank
    ;; line after previous headline.
    (let* ((begin (point))
	   (end (progn (org-with-limited-levels (outline-next-heading))
		       (point)))
	   (pos-before-blank (progn (skip-chars-backward " \r\t\n")
				    (line-beginning-position 2)))
           (robust-end (when (> (- pos-before-blank 2) begin)
                         (- pos-before-blank 2)))
           (robust-begin (when robust-end begin))
           )
      (list 'section
	    (list :begin begin
		  :end end
		  :contents-begin begin
		  :contents-end pos-before-blank
                  :robust-begin robust-begin
                  :robust-end robust-end
		  :post-blank (count-lines pos-before-blank end)
		  :post-affiliated begin)))))

(defun org-element-section-interpreter (_ contents)
  "Interpret section element as Org syntax.
CONTENTS is the contents of the element."
  contents)


;;;; Special Block

(defun org-element-special-block-parser (limit affiliated)
  "Parse a special block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `special-block' and CDR is a plist
containing `:type', `:parameters', `:begin', `:end',
`:contents-begin', `:contents-end', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let* ((case-fold-search t)
	 (type (progn (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)[ \t]*\\(.*\\)[ \t]*$")
		      (match-string-no-properties 1)))
	 (parameters (match-string-no-properties 2)))
    (if (not (save-excursion
	       (re-search-forward
		(format "^[ \t]*#\\+END_%s[ \t]*$" (regexp-quote type))
		limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 ;; Empty blocks have no contents.
		 (contents-begin (progn (forward-line)
					(and (< (point) block-end-line)
					     (point))))
		 (contents-end (and contents-begin block-end-line))
		 (pos-before-blank (progn (goto-char block-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'special-block
		  (nconc
		   (list :type type
			 :parameters (and (org-string-nw-p parameters)
					  (org-trim parameters))
			 :begin begin
			 :end end
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-special-block-interpreter (special-block contents)
  "Interpret SPECIAL-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (let ((block-type (org-element-property :type special-block))
        (parameters (org-element-property :parameters special-block)))
    (format "#+begin_%s%s\n%s#+end_%s" block-type
            (if parameters (concat " " parameters) "")
            (or contents "") block-type)))



;;; Elements
;;
;; For each element, a parser and an interpreter are also defined.
;; Both follow the same naming convention used for greater elements.
;;
;; Also, as for greater elements, adding a new element type is done
;; through the following steps: implement a parser and an interpreter,
;; tweak `org-element--current-element' so that it recognizes the new
;; type and add that new type to `org-element-all-elements'.


;;;; Babel Call

(defun org-element-babel-call-parser (limit affiliated)
  "Parse a babel call.

LIMIT bounds the search.  AFFILIATED is a list of which car is
the buffer position at the beginning of the first affiliated
keyword and cdr is a plist of affiliated keywords along with
their value.

Return a list whose car is `babel-call' and cdr is a plist
containing `:call', `:inside-header', `:arguments',
`:end-header', `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' as keywords."
  (save-excursion
    (let* ((begin (car affiliated))
	   (post-affiliated (point))
	   (before-blank (line-beginning-position 2))
	   (value (progn (search-forward ":" before-blank t)
			 (skip-chars-forward " \t")
			 (org-trim
			  (buffer-substring-no-properties
			   (point) (line-end-position)))))
	   (call
	    (or (org-string-nw-p
		 (buffer-substring-no-properties
		  (point) (progn (skip-chars-forward "^[]()" before-blank)
				 (point))))))
	   (inside-header (org-element--parse-paired-brackets ?\[))
	   (arguments (org-string-nw-p
		       (org-element--parse-paired-brackets ?\()))
	   (end-header
	    (org-string-nw-p
	     (org-trim
	      (buffer-substring-no-properties (point) (line-end-position)))))
	   (end (progn (forward-line)
		       (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'babel-call
	    (nconc
	     (list :call call
		   :inside-header inside-header
		   :arguments arguments
		   :end-header end-header
		   :begin begin
		   :end end
		   :value value
		   :post-blank (count-lines before-blank end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-babel-call-interpreter (babel-call _)
  "Interpret BABEL-CALL element as Org syntax."
  (concat "#+call: "
	  (org-element-property :call babel-call)
	  (let ((h (org-element-property :inside-header babel-call)))
	    (and h (format "[%s]" h)))
	  (concat "(" (org-element-property :arguments babel-call) ")")
	  (let ((h (org-element-property :end-header babel-call)))
	    (and h (concat " " h)))))


;;;; Clock

(defun org-element-clock-parser (limit)
  "Parse a clock.

LIMIT bounds the search.

Return a list whose CAR is `clock' and CDR is a plist containing
`:status', `:value', `:time', `:begin', `:end', `:post-blank' and
`:post-affiliated' as keywords."
  (save-excursion
    (let* ((case-fold-search nil)
	   (begin (point))
	   (value (progn (search-forward "CLOCK:" (line-end-position) t)
			 (skip-chars-forward " \t")
			 (org-element-timestamp-parser)))
	   (duration (and (search-forward " => " (line-end-position) t)
			  (progn (skip-chars-forward " \t")
				 (looking-at "\\(\\S-+\\)[ \t]*$"))
			  (match-string-no-properties 1)))
	   (status (if duration 'closed 'running))
	   (post-blank (let ((before-blank (progn (forward-line) (point))))
			 (skip-chars-forward " \r\t\n" limit)
			 (skip-chars-backward " \t")
			 (unless (bolp) (end-of-line))
			 (count-lines before-blank (point))))
	   (end (point)))
      (list 'clock
	    (list :status status
		  :value value
		  :duration duration
		  :begin begin
		  :end end
		  :post-blank post-blank
		  :post-affiliated begin)))))

(defun org-element-clock-interpreter (clock _)
  "Interpret CLOCK element as Org syntax."
  (concat "CLOCK: "
	  (org-element-timestamp-interpreter
	   (org-element-property :value clock) nil)
	  (let ((duration (org-element-property :duration clock)))
	    (and duration
		 (concat " => "
			 (apply 'format
				"%2s:%02s"
				(org-split-string duration ":")))))))


;;;; Comment

(defun org-element-comment-parser (limit)
  "Parse a comment.

LIMIT bounds the search.

Return a list whose CAR is `comment' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank',
`:post-affiliated' keywords.

Assume point is at comment beginning."
  (save-excursion
    (let* ((begin (point))
	   (value (prog2 (looking-at "[ \t]*# ?")
		      (buffer-substring-no-properties
		       (match-end 0) (line-end-position))
		    (forward-line)))
	   (com-end
	    ;; Get comments ending.
	    (progn
	      (while (and (< (point) limit) (looking-at "[ \t]*#\\( \\|$\\)"))
		;; Accumulate lines without leading hash and first
		;; whitespace.
		(setq value
		      (concat value
			      "\n"
			      (buffer-substring-no-properties
			       (match-end 0) (line-end-position))))
		(forward-line))
	      (point)))
	   (end (progn (goto-char com-end)
		       (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'comment
	    (list :begin begin
		  :end end
		  :value value
		  :post-blank (count-lines com-end end)
		  :post-affiliated begin)))))

(defun org-element-comment-interpreter (comment _)
  "Interpret COMMENT element as Org syntax.
CONTENTS is nil."
  (replace-regexp-in-string "^" "# " (org-element-property :value comment)))


;;;; Comment Block

(defun org-element-comment-block-parser (limit affiliated)
  "Parse an export block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `comment-block' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at comment block beginning."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_COMMENT[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 (contents-begin (progn (forward-line) (point)))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position))))
		 (value (buffer-substring-no-properties
			 contents-begin contents-end)))
	    (list 'comment-block
		  (nconc
		   (list :begin begin
			 :end end
			 :value value
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-comment-block-interpreter (comment-block _)
  "Interpret COMMENT-BLOCK element as Org syntax."
  (format "#+begin_comment\n%s#+end_comment"
	  (org-element-normalize-string
	   (org-remove-indentation
	    (org-element-property :value comment-block)))))


;;;; Diary Sexp

(defun org-element-diary-sexp-parser (limit affiliated)
  "Parse a diary sexp.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `diary-sexp' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords."
  (save-excursion
    (let ((begin (car affiliated))
	  (post-affiliated (point))
	  (value (progn (looking-at "\\(%%(.*\\)[ \t]*$")
			(match-string-no-properties 1)))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'diary-sexp
	    (nconc
	     (list :value value
		   :begin begin
		   :end end
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-diary-sexp-interpreter (diary-sexp _)
  "Interpret DIARY-SEXP as Org syntax."
  (org-element-property :value diary-sexp))


;;;; Example Block

(defun org-element-example-block-parser (limit affiliated)
  "Parse an example block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `example-block' and CDR is a plist
containing `:begin', `:end', `:number-lines', `:preserve-indent',
`:retain-labels', `:use-labels', `:label-fmt', `:switches',
`:value', `:post-blank' and `:post-affiliated' keywords."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_EXAMPLE[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((switches
		  (progn
		    (looking-at "^[ \t]*#\\+BEGIN_EXAMPLE\\(?: +\\(.*\\)\\)?")
		    (match-string-no-properties 1)))
		 ;; Switches analysis.
		 (number-lines
		  (and switches
		       (string-match "\\([-+]\\)n\\(?: *\\([0-9]+\\)\\)?\\>"
				     switches)
		       (cons
			(if (equal (match-string 1 switches) "-")
			    'new
			  'continued)
			(if (not (match-end 2)) 0
			  ;; Subtract 1 to give number of lines before
			  ;; first line.
			  (1- (string-to-number (match-string 2 switches)))))))
		 (preserve-indent
		  (and switches (string-match "-i\\>" switches)))
		 ;; Should labels be retained in (or stripped from) example
		 ;; blocks?
		 (retain-labels
		  (or (not switches)
		      (not (string-match "-r\\>" switches))
		      (and number-lines (string-match "-k\\>" switches))))
		 ;; What should code-references use - labels or
		 ;; line-numbers?
		 (use-labels
		  (or (not switches)
		      (and retain-labels
			   (not (string-match "-k\\>" switches)))))
		 (label-fmt
		  (and switches
		       (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
		       (match-string 1 switches)))
		 ;; Standard block parsing.
		 (begin (car affiliated))
		 (post-affiliated (point))
		 (contents-begin (line-beginning-position 2))
		 (value (org-unescape-code-in-string
			 (buffer-substring-no-properties
			  contents-begin contents-end)))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'example-block
		  (nconc
		   (list :begin begin
			 :end end
			 :value value
			 :switches switches
			 :number-lines number-lines
			 :preserve-indent preserve-indent
			 :retain-labels retain-labels
			 :use-labels use-labels
			 :label-fmt label-fmt
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-example-block-interpreter (example-block _)
  "Interpret EXAMPLE-BLOCK element as Org syntax."
  (let ((switches (org-element-property :switches example-block))
	(value
	 (let ((val (org-element-property :value example-block)))
	   (cond
	    ((or org-src-preserve-indentation
		 (org-element-property :preserve-indent example-block))
	     val)
	    ((= 0 org-edit-src-content-indentation)
	     (org-remove-indentation val))
	    (t
	     (let ((ind (make-string org-edit-src-content-indentation ?\s)))
	       (replace-regexp-in-string "^[ \t]*\\S-"
					 (concat ind "\\&")
					 (org-remove-indentation val))))))))
    (concat "#+begin_example" (and switches (concat " " switches)) "\n"
	    (org-element-normalize-string (org-escape-code-in-string value))
	    "#+end_example")))


;;;; Export Block

(defun org-element-export-block-parser (limit affiliated)
  "Parse an export block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `export-block' and CDR is a plist
containing `:begin', `:end', `:type', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at export-block beginning."
  (let* ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_EXPORT[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (save-excursion
	(let* ((contents-end (match-beginning 0))
	       (backend
		(progn
		  (looking-at
		   "[ \t]*#\\+BEGIN_EXPORT\\(?:[ \t]+\\(\\S-+\\)\\)?[ \t]*$")
		  (match-string-no-properties 1)))
	       (begin (car affiliated))
	       (post-affiliated (point))
	       (contents-begin (progn (forward-line) (point)))
	       (pos-before-blank (progn (goto-char contents-end)
					(forward-line)
					(point)))
	       (end (progn (skip-chars-forward " \r\t\n" limit)
			   (if (eobp) (point) (line-beginning-position))))
	       (value (org-unescape-code-in-string
		       (buffer-substring-no-properties contents-begin
						       contents-end))))
	  (list 'export-block
		(nconc
		 (list :type (and backend (upcase backend))
		       :begin begin
		       :end end
		       :value value
		       :post-blank (count-lines pos-before-blank end)
		       :post-affiliated post-affiliated)
		 (cdr affiliated))))))))

(defun org-element-export-block-interpreter (export-block _)
  "Interpret EXPORT-BLOCK element as Org syntax."
  (format "#+begin_export %s\n%s#+end_export"
	  (org-element-property :type export-block)
	  (org-element-property :value export-block)))


;;;; Fixed-width

(defun org-element-fixed-width-parser (limit affiliated)
  "Parse a fixed-width section.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `fixed-width' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the fixed-width area."
  (save-excursion
    (let* ((begin (car affiliated))
	   (post-affiliated (point))
	   (end-area
	    (progn
	      (while (and (< (point) limit)
			  (looking-at "[ \t]*:\\( \\|$\\)"))
		(forward-line))
	      (if (bolp) (line-end-position 0) (point))))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'fixed-width
	    (nconc
	     (list :begin begin
		   :end end
		   :value (replace-regexp-in-string
			   "^[ \t]*: ?" ""
			   (buffer-substring-no-properties post-affiliated
							   end-area))
		   :post-blank (count-lines end-area end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-fixed-width-interpreter (fixed-width _)
  "Interpret FIXED-WIDTH element as Org syntax."
  (let ((value (org-element-property :value fixed-width)))
    (and value
         (if (string-empty-p value) ":\n"
           (replace-regexp-in-string "^" ": " value)))))


;;;; Horizontal Rule

(defun org-element-horizontal-rule-parser (limit affiliated)
  "Parse an horizontal rule.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `horizontal-rule' and CDR is a plist
containing `:begin', `:end', `:post-blank' and `:post-affiliated'
keywords."
  (save-excursion
    (let ((begin (car affiliated))
	  (post-affiliated (point))
	  (post-hr (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'horizontal-rule
	    (nconc
	     (list :begin begin
		   :end end
		   :post-blank (count-lines post-hr end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-horizontal-rule-interpreter (&rest _)
  "Interpret HORIZONTAL-RULE element as Org syntax."
  "-----")


;;;; Keyword

(defun org-element-keyword-parser (limit affiliated)
  "Parse a keyword at point.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is a normalized `keyword' (uppercase) and
CDR is a plist containing `:key', `:value', `:begin', `:end',
`:post-blank' and `:post-affiliated' keywords."
  (save-excursion
    ;; An orphaned affiliated keyword is considered as a regular
    ;; keyword.  In this case AFFILIATED is nil, so we take care of
    ;; this corner case.
    (let ((begin (or (car affiliated) (point)))
	  (post-affiliated (point))
	  (key (progn (looking-at "[ \t]*#\\+\\(\\S-*\\):")
		      (upcase (match-string-no-properties 1))))
	  (value (org-trim (buffer-substring-no-properties
                            (match-end 0) (line-end-position))))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'keyword
	    (nconc
	     (list :key key
		   :value value
		   :begin begin
		   :end end
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-keyword-interpreter (keyword _)
  "Interpret KEYWORD element as Org syntax."
  (format "#+%s: %s"
	  (downcase (org-element-property :key keyword))
	  (org-element-property :value keyword)))


;;;; LaTeX Environment

(defconst org-element--latex-begin-environment
  "^[ \t]*\\\\begin{\\([A-Za-z0-9*]+\\)}"
  "Regexp matching the beginning of a LaTeX environment.
The environment is captured by the first group.

See also `org-element--latex-end-environment'.")

(defconst org-element--latex-end-environment
  "\\\\end{%s}[ \t]*$"
  "Format string matching the ending of a LaTeX environment.
See also `org-element--latex-begin-environment'.")

(defun org-element-latex-environment-parser (limit affiliated)
  "Parse a LaTeX environment.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `latex-environment' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the latex environment."
  (save-excursion
    (let ((case-fold-search t)
	  (code-begin (point)))
      (looking-at org-element--latex-begin-environment)
      (if (not (re-search-forward (format org-element--latex-end-environment
					  (regexp-quote (match-string 1)))
				  limit t))
	  ;; Incomplete latex environment: parse it as a paragraph.
	  (org-element-paragraph-parser limit affiliated)
	(let* ((code-end (progn (forward-line) (point)))
	       (begin (car affiliated))
	       (value (buffer-substring-no-properties code-begin code-end))
	       (end (progn (skip-chars-forward " \r\t\n" limit)
			   (if (eobp) (point) (line-beginning-position)))))
	  (list 'latex-environment
		(nconc
		 (list :begin begin
		       :end end
		       :value value
		       :post-blank (count-lines code-end end)
		       :post-affiliated code-begin)
		 (cdr affiliated))))))))

(defun org-element-latex-environment-interpreter (latex-environment _)
  "Interpret LATEX-ENVIRONMENT element as Org syntax."
  (org-element-property :value latex-environment))


;;;; Node Property

(defun org-element-node-property-parser (limit)
  "Parse a node-property at point.

LIMIT bounds the search.

Return a list whose CAR is `node-property' and CDR is a plist
containing `:key', `:value', `:begin', `:end', `:post-blank' and
`:post-affiliated' keywords."
  (looking-at org-property-re)
  (let ((case-fold-search t)
	(begin (point))
	(key   (match-string-no-properties 2))
	(value (match-string-no-properties 3))
	(end (save-excursion
	       (end-of-line)
	       (if (re-search-forward org-property-re limit t)
		   (line-beginning-position)
		 limit))))
    (list 'node-property
	  (list :key key
		:value value
		:begin begin
		:end end
		:post-blank 0
		:post-affiliated begin))))

(defun org-element-node-property-interpreter (node-property _)
  "Interpret NODE-PROPERTY element as Org syntax."
  (format org-property-format
	  (format ":%s:" (org-element-property :key node-property))
	  (or (org-element-property :value node-property) "")))


;;;; Paragraph

(defun org-element-paragraph-parser (limit affiliated)
  "Parse a paragraph.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `paragraph' and CDR is a plist
containing `:begin', `:end', `:contents-begin' and
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the paragraph."
  (save-excursion
    (let* ((begin (car affiliated))
	   (contents-begin (point))
	   (before-blank
	    (let ((case-fold-search t))
	      (end-of-line)
	      ;; A matching `org-element-paragraph-separate' is not
	      ;; necessarily the end of the paragraph.  In particular,
	      ;; drawers, blocks or LaTeX environments opening lines
	      ;; must be closed.  Moreover keywords with a secondary
	      ;; value must belong to "dual keywords".
	      (while (not
		      (cond
		       ((not (and (re-search-forward
				   org-element-paragraph-separate limit 'move)
				  (progn (beginning-of-line) t))))
		       ((looking-at org-element-drawer-re)
			(save-excursion
			  (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)))
		       ((looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)")
			(save-excursion
			  (re-search-forward
			   (format "^[ \t]*#\\+END_%s[ \t]*$"
				   (regexp-quote (match-string 1)))
			   limit t)))
		       ((looking-at org-element--latex-begin-environment)
			(save-excursion
			  (re-search-forward
			   (format org-element--latex-end-environment
				   (regexp-quote (match-string 1)))
			   limit t)))
		       ((looking-at "[ \t]*#\\+\\(\\S-+\\)\\[.*\\]:")
			(member-ignore-case (match-string 1)
					    org-element-dual-keywords))
		       ;; Everything else is unambiguous.
		       (t)))
		(end-of-line))
	      (if (= (point) limit) limit
		(goto-char (line-beginning-position)))))
	   (contents-end (save-excursion
			   (skip-chars-backward " \r\t\n" contents-begin)
			   (line-beginning-position 2)))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'paragraph
	    (nconc
	     (list :begin begin
		   :end end
		   :contents-begin contents-begin
		   :contents-end contents-end
		   :post-blank (count-lines before-blank end)
		   :post-affiliated contents-begin)
	     (cdr affiliated))))))

(defun org-element-paragraph-interpreter (_ contents)
  "Interpret paragraph element as Org syntax.
CONTENTS is the contents of the element."
  contents)


;;;; Planning

(defun org-element-planning-parser (limit)
  "Parse a planning.

LIMIT bounds the search.

Return a list whose CAR is `planning' and CDR is a plist
containing `:closed', `:deadline', `:scheduled', `:begin',
`:end', `:post-blank' and `:post-affiliated' keywords."
  (save-excursion
    (let* ((case-fold-search nil)
	   (begin (point))
	   (post-blank (let ((before-blank (progn (forward-line) (point))))
			 (skip-chars-forward " \r\t\n" limit)
			 (skip-chars-backward " \t")
			 (unless (bolp) (end-of-line))
			 (count-lines before-blank (point))))
	   (end (point))
	   closed deadline scheduled)
      (goto-char begin)
      (while (re-search-forward org-element-planning-keywords-re end t)
	(skip-chars-forward " \t" end)
	(let ((keyword (match-string 0))
	      (time (org-element-timestamp-parser)))
	  (cond
           ((equal keyword org-element-closed-keyword) (setq closed time))
	   ((equal keyword org-element-deadline-keyword) (setq deadline time))
	   (t (setq scheduled time)))))
      (list 'planning
	    (list :closed closed
		  :deadline deadline
		  :scheduled scheduled
		  :begin begin
		  :end end
		  :post-blank post-blank
		  :post-affiliated begin)))))

(defun org-element-planning-interpreter (planning _)
  "Interpret PLANNING element as Org syntax."
  (mapconcat
   #'identity
   (delq nil
	 (list (let ((deadline (org-element-property :deadline planning)))
		 (when deadline
		   (concat org-element-deadline-keyword " "
			   (org-element-timestamp-interpreter deadline nil))))
	       (let ((scheduled (org-element-property :scheduled planning)))
		 (when scheduled
		   (concat org-element-scheduled-keyword " "
			   (org-element-timestamp-interpreter scheduled nil))))
	       (let ((closed (org-element-property :closed planning)))
		 (when closed
		   (concat org-element-closed-keyword " "
			   (org-element-timestamp-interpreter closed nil))))))
   " "))


;;;; Src Block

(defun org-element-src-block-parser (limit affiliated)
  "Parse a source block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `src-block' and CDR is a plist
containing `:language', `:switches', `:parameters', `:begin',
`:end', `:number-lines', `:retain-labels', `:use-labels',
`:label-fmt', `:preserve-indent', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion (re-search-forward "^[ \t]*#\\+END_SRC[ \t]*$"
						limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 ;; Get language as a string.
		 (language
		  (progn
		    (looking-at
		     "^[ \t]*#\\+BEGIN_SRC\
\\(?: +\\(\\S-+\\)\\)?\
\\(\\(?: +\\(?:-\\(?:l \".+\"\\|[ikr]\\)\\|[-+]n\\(?: *[0-9]+\\)?\\)\\)+\\)?\
\\(.*\\)[ \t]*$")
		    (match-string-no-properties 1)))
		 ;; Get switches.
		 (switches (match-string-no-properties 2))
		 ;; Get parameters.
		 (parameters (match-string-no-properties 3))
		 ;; Switches analysis.
		 (number-lines
		  (and switches
		       (string-match "\\([-+]\\)n\\(?: *\\([0-9]+\\)\\)?\\>"
				     switches)
		       (cons
			(if (equal (match-string 1 switches) "-")
			    'new
			  'continued)
			(if (not (match-end 2)) 0
			  ;; Subtract 1 to give number of lines before
			  ;; first line.
			  (1- (string-to-number (match-string 2 switches)))))))
		 (preserve-indent (and switches
				       (string-match "-i\\>" switches)))
		 (label-fmt
		  (and switches
		       (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
		       (match-string 1 switches)))
		 ;; Should labels be retained in (or stripped from)
		 ;; source blocks?
		 (retain-labels
		  (or (not switches)
		      (not (string-match "-r\\>" switches))
		      (and number-lines (string-match "-k\\>" switches))))
		 ;; What should code-references use - labels or
		 ;; line-numbers?
		 (use-labels
		  (or (not switches)
		      (and retain-labels
			   (not (string-match "-k\\>" switches)))))
		 ;; Retrieve code.
		 (value (org-unescape-code-in-string
			 (buffer-substring-no-properties
			  (line-beginning-position 2) contents-end)))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 ;; Get position after ending blank lines.
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'src-block
		  (nconc
		   (list :language language
			 :switches (and (org-string-nw-p switches)
					(org-trim switches))
			 :parameters (and (org-string-nw-p parameters)
					  (org-trim parameters))
			 :begin begin
			 :end end
			 :number-lines number-lines
			 :preserve-indent preserve-indent
			 :retain-labels retain-labels
			 :use-labels use-labels
			 :label-fmt label-fmt
			 :value value
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-src-block-interpreter (src-block _)
  "Interpret SRC-BLOCK element as Org syntax."
  (let ((lang (org-element-property :language src-block))
	(switches (org-element-property :switches src-block))
	(params (org-element-property :parameters src-block))
	(value
	 (let ((val (org-element-property :value src-block)))
	   (cond
	    ((or org-src-preserve-indentation
		 (org-element-property :preserve-indent src-block))
	     val)
	    ((zerop org-edit-src-content-indentation)
	     (org-remove-indentation val))
	    (t
	     (let ((ind (make-string org-edit-src-content-indentation ?\s)))
	       (replace-regexp-in-string "^[ \t]*\\S-"
					 (concat ind "\\&")
					 (org-remove-indentation val))))))))
    (format "#+begin_src%s\n%s#+end_src"
	    (concat (and lang (concat " " lang))
		    (and switches (concat " " switches))
		    (and params (concat " " params)))
	    (org-element-normalize-string (org-escape-code-in-string value)))))


;;;; Table

(defun org-element-table-parser (limit affiliated)
  "Parse a table at point.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `table' and CDR is a plist containing
`:begin', `:end', `:tblfm', `:type', `:contents-begin',
`:contents-end', `:value', `:post-blank' and `:post-affiliated'
keywords.

Assume point is at the beginning of the table."
  (save-excursion
    (let* ((case-fold-search t)
	   (table-begin (point))
	   (type (if (looking-at "[ \t]*|") 'org 'table.el))
           (end-re (format "^[ \t]*\\($\\|[^| \t%s]\\)"
			   (if (eq type 'org) "" "+")))
	   (begin (car affiliated))
	   (table-end
	    (if (re-search-forward end-re limit 'move)
		(goto-char (match-beginning 0))
	      (point)))
	   (tblfm (let (acc)
		    (while (looking-at "[ \t]*#\\+TBLFM: +\\(.*\\)[ \t]*$")
		      (push (match-string-no-properties 1) acc)
		      (forward-line))
		    acc))
	   (pos-before-blank (point))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'table
	    (nconc
	     (list :begin begin
		   :end end
		   :type type
		   :tblfm tblfm
		   ;; Only `org' tables have contents.  `table.el' tables
		   ;; use a `:value' property to store raw table as
		   ;; a string.
		   :contents-begin (and (eq type 'org) table-begin)
		   :contents-end (and (eq type 'org) table-end)
		   :value (and (eq type 'table.el)
			       (buffer-substring-no-properties
				table-begin table-end))
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated table-begin)
	     (cdr affiliated))))))

(defun org-element-table-interpreter (table contents)
  "Interpret TABLE element as Org syntax.
CONTENTS is a string, if table's type is `org', or nil."
  (if (eq (org-element-property :type table) 'table.el)
      (org-remove-indentation (org-element-property :value table))
    (concat (with-temp-buffer (insert contents)
			      (org-table-align)
			      (buffer-string))
	    (mapconcat (lambda (fm) (concat "#+TBLFM: " fm))
		       (reverse (org-element-property :tblfm table))
		       "\n"))))


;;;; Table Row

(defun org-element-table-row-parser (_)
  "Parse table row at point.

Return a list whose CAR is `table-row' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:type', `:post-blank' and `:post-affiliated' keywords."
  (save-excursion
    (let* ((type (if (looking-at "^[ \t]*|-") 'rule 'standard))
	   (begin (point))
	   ;; A table rule has no contents.  In that case, ensure
	   ;; CONTENTS-BEGIN matches CONTENTS-END.
	   (contents-begin (and (eq type 'standard) (search-forward "|")))
	   (contents-end (and (eq type 'standard)
			      (progn
				(end-of-line)
				(skip-chars-backward " \t")
				(point))))
	   (end (line-beginning-position 2)))
      (list 'table-row
	    (list :type type
		  :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank 0
		  :post-affiliated begin)))))

(defun org-element-table-row-interpreter (table-row contents)
  "Interpret TABLE-ROW element as Org syntax.
CONTENTS is the contents of the table row."
  (if (eq (org-element-property :type table-row) 'rule) "|-"
    (concat "|" contents)))


;;;; Verse Block

(defun org-element-verse-block-parser (limit affiliated)
  "Parse a verse block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `verse-block' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:post-blank' and `:post-affiliated' keywords.

Assume point is at beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_VERSE[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 (contents-begin (progn (forward-line) (point)))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'verse-block
		  (nconc
		   (list :begin begin
			 :end end
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-verse-block-interpreter (_ contents)
  "Interpret verse-block element as Org syntax.
CONTENTS is verse block contents."
  (format "#+begin_verse\n%s#+end_verse" contents))



;;; Objects
;;
;; Unlike to elements, raw text can be found between objects.  Hence,
;; `org-element--object-lex' is provided to find the next object in
;; buffer.
;;
;; Some object types (e.g., `italic') are recursive.  Restrictions on
;; object types they can contain will be specified in
;; `org-element-object-restrictions'.
;;
;; Creating a new type of object requires to alter
;; `org-element--object-regexp' and `org-element--object-lex', add the
;; new type in `org-element-all-objects', and possibly add
;; restrictions in `org-element-object-restrictions'.

;;;; Bold

(defun org-element--parse-generic-emphasis (mark type)
  "Parse emphasis object at point, if any.

MARK is the delimiter string used.  TYPE is a symbol among
`bold', `code', `italic', `strike-through', `underline', and
`verbatim'.

Assume point is at first MARK."
  (save-excursion
    (let ((origin (point)))
      (unless (bolp) (forward-char -1))
      (let ((opening-re
             (rx-to-string
              `(seq (or line-start (any space ?- ?\( ?' ?\" ?\{))
                    ,mark
                    (not space)))))
        (when (looking-at opening-re)
          (goto-char (1+ origin))
          (let ((closing-re
                 (rx-to-string
                  `(seq
                    (not space)
                    (group ,mark)
                    (or (any space ?- ?. ?, ?\; ?: ?! ?? ?' ?\" ?\) ?\} ?\\ ?\[)
                        line-end)))))
            (when (re-search-forward closing-re nil t)
              (let ((closing (match-end 1)))
                (goto-char closing)
                (let* ((post-blank (skip-chars-forward " \t"))
                       (contents-begin (1+ origin))
                       (contents-end (1- closing)))
                  (list type
                        (append
                         (list :begin origin
                               :end (point)
                               :post-blank post-blank)
                         (if (memq type '(code verbatim))
                             (list :value
                                   (and (memq type '(code verbatim))
                                        (buffer-substring
                                         contents-begin contents-end)))
                           (list :contents-begin contents-begin
                                 :contents-end contents-end)))))))))))))

(defun org-element-bold-parser ()
  "Parse bold object at point, if any.

When at a bold object, return a list whose car is `bold' and cdr
is a plist with `:begin', `:end', `:contents-begin' and
`:contents-end' and `:post-blank' keywords.  Otherwise, return
nil.

Assume point is at the first star marker."
  (org-element--parse-generic-emphasis "*" 'bold))

(defun org-element-bold-interpreter (_ contents)
  "Interpret bold object as Org syntax.
CONTENTS is the contents of the object."
  (format "*%s*" contents))


;;;; Citation

(defun org-element-citation-parser ()
  "Parse citation object at point, if any.

When at a citation object, return a list whose car is `citation'
and cdr is a plist with `:style', `:prefix', `:suffix', `:begin',
`:end', `:contents-begin', `:contents-end', and `:post-blank'
keywords.  Otherwise, return nil.

Assume point is at the beginning of the citation."
  (when (looking-at org-element-citation-prefix-re)
    (let* ((begin (point))
	   (style (and (match-end 1)
		       (match-string-no-properties 1)))
	   ;; Ignore blanks between cite type and prefix or key.
	   (start (match-end 0))
	   (closing (with-syntax-table org-element--pair-square-table
		      (ignore-errors (scan-lists begin 1 0)))))
      (save-excursion
	(when (and closing
		   (re-search-forward org-element-citation-key-re closing t))
	  ;; Find prefix, if any.
	  (let ((first-key-end (match-end 0))
		(types (org-element-restriction 'citation-reference))
                (cite
		 (list 'citation
		       (list :style style
			     :begin begin
			     :post-blank (progn
					   (goto-char closing)
					   (skip-chars-forward " \t"))
			     :end (point)))))
	    ;; `:contents-begin' depends on the presence of
	    ;; a non-empty common prefix.
	    (goto-char first-key-end)
	    (if (not (search-backward ";" start t))
		(org-element-put-property cite :contents-begin start)
	      (when (< start (point))
		(org-element-put-property
                 cite :prefix
                 (org-element--parse-objects start (point) nil types cite)))
	      (forward-char)
	      (org-element-put-property cite :contents-begin (point)))
	    ;; `:contents-end' depends on the presence of a non-empty
	    ;; common suffix.
	    (goto-char (1- closing))
	    (skip-chars-backward " \r\t\n")
	    (let ((end (point)))
	      (if (or (not (search-backward ";" first-key-end t))
		      (re-search-forward org-element-citation-key-re end t))
		  (org-element-put-property cite :contents-end end)
                (forward-char)
		(when (< (point) end)
		  (org-element-put-property
                   cite :suffix
                   (org-element--parse-objects (point) end nil types cite)))
		(org-element-put-property cite :contents-end (point))))
	    cite))))))

(defun org-element-citation-interpreter (citation contents)
  "Interpret CITATION object as Org syntax.
CONTENTS is the contents of the object, as a string."
  (let ((prefix (org-element-property :prefix citation))
        (suffix (org-element-property :suffix citation))
        (style (org-element-property :style citation)))
    (concat "[cite"
            (and style (concat "/" style))
            ":"
            (and prefix (concat (org-element-interpret-data prefix) ";"))
            (if suffix
                (concat contents (org-element-interpret-data suffix))
              ;; Remove spurious semicolon.
              (substring contents nil -1))
            "]")))


;;;; Citation Reference

(defun org-element-citation-reference-parser ()
  "Parse citation reference object at point, if any.

When at a reference, return a list whose car is
`citation-reference', and cdr is a plist with `:key',
`:prefix', `:suffix', `:begin', `:end', and `:post-blank' keywords.

Assume point is at the beginning of the reference."
  (save-excursion
    (let ((begin (point)))
      (when (re-search-forward org-element-citation-key-re nil t)
        (let* ((key (match-string-no-properties 1))
	       (key-start (match-beginning 0))
	       (key-end (match-end 0))
	       (separator (search-forward ";" nil t))
               (end (or separator (point-max)))
               (suffix-end (if separator (1- end) end))
               (types (org-element-restriction 'citation-reference))
	       (reference
	        (list 'citation-reference
		      (list :key key
			    :begin begin
			    :end end
			    :post-blank 0))))
	  (when (< begin key-start)
	    (org-element-put-property
	     reference :prefix
             (org-element--parse-objects begin key-start nil types reference)))
	  (when (< key-end suffix-end)
	    (org-element-put-property
	     reference :suffix
             (org-element--parse-objects key-end suffix-end nil types reference)))
	  reference)))))

(defun org-element-citation-reference-interpreter (citation-reference _)
  "Interpret CITATION-REFERENCE object as Org syntax."
  (concat (org-element-interpret-data
           (org-element-property :prefix citation-reference))
	  "@" (org-element-property :key citation-reference)
	  (org-element-interpret-data
           (org-element-property :suffix citation-reference))
          ";"))


;;;; Code

(defun org-element-code-parser ()
  "Parse code object at point, if any.

When at a code object, return a list whose car is `code' and cdr
is a plist with `:value', `:begin', `:end' and `:post-blank'
keywords.  Otherwise, return nil.

Assume point is at the first tilde marker."
  (org-element--parse-generic-emphasis "~" 'code))

(defun org-element-code-interpreter (code _)
  "Interpret CODE object as Org syntax."
  (format "~%s~" (org-element-property :value code)))


;;;; Entity

(defun org-element-entity-parser ()
  "Parse entity at point, if any.

When at an entity, return a list whose car is `entity' and cdr
a plist with `:begin', `:end', `:latex', `:latex-math-p',
`:html', `:latin1', `:utf-8', `:ascii', `:use-brackets-p' and
`:post-blank' as keywords.  Otherwise, return nil.

Assume point is at the beginning of the entity."
  (catch 'no-object
    (when (looking-at "\\\\\\(?:\\(?1:_ +\\)\\|\\(?1:there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\(?2:$\\|{}\\|[^[:alpha:]]\\)\\)")
      (save-excursion
	(let* ((value (or (org-entity-get (match-string 1))
			  (throw 'no-object nil)))
	       (begin (match-beginning 0))
	       (bracketsp (string= (match-string 2) "{}"))
	       (post-blank (progn (goto-char (match-end 1))
				  (when bracketsp (forward-char 2))
				  (skip-chars-forward " \t")))
	       (end (point)))
	  (list 'entity
		(list :name (car value)
		      :latex (nth 1 value)
		      :latex-math-p (nth 2 value)
		      :html (nth 3 value)
		      :ascii (nth 4 value)
		      :latin1 (nth 5 value)
		      :utf-8 (nth 6 value)
		      :begin begin
		      :end end
		      :use-brackets-p bracketsp
		      :post-blank post-blank)))))))

(defun org-element-entity-interpreter (entity _)
  "Interpret ENTITY object as Org syntax."
  (concat "\\"
	  (org-element-property :name entity)
	  (when (org-element-property :use-brackets-p entity) "{}")))


;;;; Export Snippet

(defun org-element-export-snippet-parser ()
  "Parse export snippet at point.

When at an export snippet, return a list whose car is
`export-snippet' and cdr a plist with `:begin', `:end',
`:back-end', `:value' and `:post-blank' as keywords.  Otherwise,
return nil.

Assume point is at the beginning of the snippet."
  (save-excursion
    (let (contents-end)
      (when (and (looking-at "@@\\([-A-Za-z0-9]+\\):")
		 (setq contents-end
		       (save-match-data (goto-char (match-end 0))
                                        (when
					    (re-search-forward "@@" nil t)
					  (match-beginning 0)))))
	(let* ((begin (match-beginning 0))
	       (back-end (match-string-no-properties 1))
	       (value (buffer-substring-no-properties
		       (match-end 0) contents-end))
	       (post-blank (skip-chars-forward " \t"))
	       (end (point)))
	  (list 'export-snippet
		(list :back-end back-end
		      :value value
		      :begin begin
		      :end end
		      :post-blank post-blank)))))))

(defun org-element-export-snippet-interpreter (export-snippet _)
  "Interpret EXPORT-SNIPPET object as Org syntax."
  (format "@@%s:%s@@"
	  (org-element-property :back-end export-snippet)
	  (org-element-property :value export-snippet)))


;;;; Footnote Reference

(defun org-element-footnote-reference-parser ()
  "Parse footnote reference at point, if any.

When at a footnote reference, return a list whose car is
`footnote-reference' and cdr a plist with `:label', `:type',
`:begin', `:end', `:contents-begin', `:contents-end' and
`:post-blank' as keywords.  Otherwise, return nil."
  (when (looking-at org-footnote-re)
    (let ((closing (with-syntax-table org-element--pair-square-table
		     (ignore-errors (scan-lists (point) 1 0)))))
      (when closing
	(save-excursion
	  (let* ((begin (point))
		 (label (match-string-no-properties 1))
		 (inner-begin (match-end 0))
		 (inner-end (1- closing))
		 (type (if (match-end 2) 'inline 'standard))
		 (post-blank (progn (goto-char closing)
				    (skip-chars-forward " \t")))
		 (end (point)))
	    (list 'footnote-reference
		  (list :label label
			:type type
			:begin begin
			:end end
			:contents-begin (and (eq type 'inline) inner-begin)
			:contents-end (and (eq type 'inline) inner-end)
			:post-blank post-blank))))))))

(defun org-element-footnote-reference-interpreter (footnote-reference contents)
  "Interpret FOOTNOTE-REFERENCE object as Org syntax.
CONTENTS is its definition, when inline, or nil."
  (format "[fn:%s%s]"
	  (or (org-element-property :label footnote-reference) "")
	  (if contents (concat ":" contents) "")))


;;;; Inline Babel Call

(defun org-element-inline-babel-call-parser ()
  "Parse inline babel call at point, if any.

When at an inline babel call, return a list whose car is
`inline-babel-call' and cdr a plist with `:call',
`:inside-header', `:arguments', `:end-header', `:begin', `:end',
`:value' and `:post-blank' as keywords.  Otherwise, return nil.

Assume point is at the beginning of the babel call."
  (save-excursion
    (catch :no-object
      (when (let ((case-fold-search nil))
	      (looking-at "\\<call_\\([^ \t\n[(]+\\)[([]"))
	(goto-char (match-end 1))
	(let* ((begin (match-beginning 0))
	       (call (match-string-no-properties 1))
	       (inside-header
		(let ((p (org-element--parse-paired-brackets ?\[)))
		  (and (org-string-nw-p p)
		       (replace-regexp-in-string "\n[ \t]*" " " (org-trim p)))))
	       (arguments (org-string-nw-p
			   (or (org-element--parse-paired-brackets ?\()
			       ;; Parenthesis are mandatory.
			       (throw :no-object nil))))
	       (end-header
		(let ((p (org-element--parse-paired-brackets ?\[)))
		  (and (org-string-nw-p p)
		       (replace-regexp-in-string "\n[ \t]*" " " (org-trim p)))))
	       (value (buffer-substring-no-properties begin (point)))
	       (post-blank (skip-chars-forward " \t"))
	       (end (point)))
	  (list 'inline-babel-call
		(list :call call
		      :inside-header inside-header
		      :arguments arguments
		      :end-header end-header
		      :begin begin
		      :end end
		      :value value
		      :post-blank post-blank)))))))

(defun org-element-inline-babel-call-interpreter (inline-babel-call _)
  "Interpret INLINE-BABEL-CALL object as Org syntax."
  (concat "call_"
	  (org-element-property :call inline-babel-call)
	  (let ((h (org-element-property :inside-header inline-babel-call)))
	    (and h (format "[%s]" h)))
	  "(" (org-element-property :arguments inline-babel-call) ")"
	  (let ((h (org-element-property :end-header inline-babel-call)))
	    (and h (format "[%s]" h)))))


;;;; Inline Src Block

(defun org-element-inline-src-block-parser ()
  "Parse inline source block at point, if any.

When at an inline source block, return a list whose car is
`inline-src-block' and cdr a plist with `:begin', `:end',
`:language', `:value', `:parameters' and `:post-blank' as
keywords.  Otherwise, return nil.

Assume point is at the beginning of the inline source block."
  (save-excursion
    (catch :no-object
      (when (let ((case-fold-search nil))
	      (looking-at "\\<src_\\([^ \t\n[{]+\\)[{[]"))
	(goto-char (match-end 1))
	(let ((begin (match-beginning 0))
	      (language (match-string-no-properties 1))
	      (parameters
	       (let ((p (org-element--parse-paired-brackets ?\[)))
		 (and (org-string-nw-p p)
		      (replace-regexp-in-string "\n[ \t]*" " " (org-trim p)))))
	      (value (or (org-element--parse-paired-brackets ?\{)
			 (throw :no-object nil)))
	      (post-blank (skip-chars-forward " \t")))
	  (list 'inline-src-block
		(list :language language
		      :value value
		      :parameters parameters
		      :begin begin
		      :end (point)
		      :post-blank post-blank)))))))

(defun org-element-inline-src-block-interpreter (inline-src-block _)
  "Interpret INLINE-SRC-BLOCK object as Org syntax."
  (let ((language (org-element-property :language inline-src-block))
	(arguments (org-element-property :parameters inline-src-block))
	(body (org-element-property :value inline-src-block)))
    (format "src_%s%s{%s}"
	    language
	    (if arguments (format "[%s]" arguments) "")
	    body)))

;;;; Italic

(defun org-element-italic-parser ()
  "Parse italic object at point, if any.

When at an italic object, return a list whose car is `italic' and
cdr is a plist with `:begin', `:end', `:contents-begin' and
`:contents-end' and `:post-blank' keywords.  Otherwise, return
nil.

Assume point is at the first slash marker."
  (org-element--parse-generic-emphasis "/" 'italic))

(defun org-element-italic-interpreter (_ contents)
  "Interpret italic object as Org syntax.
CONTENTS is the contents of the object."
  (format "/%s/" contents))


;;;; LaTeX Fragment

(defun org-element-latex-fragment-parser ()
  "Parse LaTeX fragment at point, if any.

When at a LaTeX fragment, return a list whose car is
`latex-fragment' and cdr a plist with `:value', `:begin', `:end',
and `:post-blank' as keywords.  Otherwise, return nil.

Assume point is at the beginning of the LaTeX fragment."
  (catch 'no-object
    (save-excursion
      (let* ((begin (point))
	     (after-fragment
	      (cond
	       ((not (eq ?$ (char-after)))
		(pcase (char-after (1+ (point)))
		  (?\( (search-forward "\\)" nil t))
		  (?\[ (search-forward "\\]" nil t))
		  (_
		   ;; Macro.
		   (and (looking-at "\\\\[a-zA-Z]+\\*?\\(\\(\\[[^][\n{}]*\\]\\)\
\\|\\({[^{}\n]*}\\)\\)*")
			(match-end 0)))))
	       ((eq ?$ (char-after (1+ (point))))
		(search-forward "$$" nil t 2))
	       (t
		(and (not (eq ?$ (char-before)))
		     (not (memq (char-after (1+ (point)))
				'(?\s ?\t ?\n ?, ?. ?\;)))
		     (search-forward "$" nil t 2)
		     (not (memq (char-before (match-beginning 0))
				'(?\s ?\t ?\n ?, ?.)))
		     (looking-at-p
		      "\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|'\\|$\\)")
		     (point)))))
	     (post-blank
	      (if (not after-fragment) (throw 'no-object nil)
		(goto-char after-fragment)
		(skip-chars-forward " \t")))
	     (end (point)))
	(list 'latex-fragment
	      (list :value (buffer-substring-no-properties begin after-fragment)
		    :begin begin
		    :end end
		    :post-blank post-blank))))))

(defun org-element-latex-fragment-interpreter (latex-fragment _)
  "Interpret LATEX-FRAGMENT object as Org syntax."
  (org-element-property :value latex-fragment))

;;;; Line Break

(defun org-element-line-break-parser ()
  "Parse line break at point, if any.

When at a line break, return a list whose car is `line-break',
and cdr a plist with `:begin', `:end' and `:post-blank' keywords.
Otherwise, return nil.

Assume point is at the beginning of the line break."
  (when (and (looking-at-p "\\\\\\\\[ \t]*$")
	     (not (eq (char-before) ?\\)))
    (list 'line-break
	  (list :begin (point)
		:end (line-beginning-position 2)
		:post-blank 0))))

(defun org-element-line-break-interpreter (&rest _)
  "Interpret LINE-BREAK object as Org syntax."
  "\\\\\n")


;;;; Link

(defun org-element-link-parser ()
  "Parse link at point, if any.

When at a link, return a list whose car is `link' and cdr a plist
with `:type', `:path', `:format', `:raw-link', `:application',
`:search-option', `:begin', `:end', `:contents-begin',
`:contents-end' and `:post-blank' as keywords.  Otherwise, return
nil.

Assume point is at the beginning of the link."
  (catch 'no-object
    (let ((begin (point))
	  end contents-begin contents-end link-end post-blank path type format
	  raw-link search-option application)
      (cond
       ;; Type 1: Text targeted from a radio target.
       ((and org-target-link-regexp
	     (save-excursion (or (bolp) (backward-char))
			     (looking-at org-target-link-regexp)))
	(setq type "radio")
	(setq format 'plain)
	(setq link-end (match-end 1))
	(setq path (match-string-no-properties 1))
	(setq contents-begin (match-beginning 1))
	(setq contents-end (match-end 1)))
       ;; Type 2: Standard link, i.e. [[https://orgmode.org][website]]
       ((looking-at org-link-bracket-re)
	(setq format 'bracket)
	(setq contents-begin (match-beginning 2))
	(setq contents-end (match-end 2))
	(setq link-end (match-end 0))
	;; RAW-LINK is the original link.  Decode any encoding.
	;; Expand any abbreviation in it.
	;;
	;; Also treat any newline character and associated
	;; indentation as a single space character.  This is not
	;; compatible with RFC 3986, which requires to ignore
	;; them altogether.  However, doing so would require
	;; users to encode spaces on the fly when writing links
	;; (e.g., insert [[shell:ls%20*.org]] instead of
	;; [[shell:ls *.org]], which defeats Org's focus on
	;; simplicity.
	(setq raw-link (org-link-expand-abbrev
			(org-link-unescape
			 (replace-regexp-in-string
			  "[ \t]*\n[ \t]*" " "
			  (match-string-no-properties 1)))))
	;; Determine TYPE of link and set PATH accordingly.  According
	;; to RFC 3986, remove whitespaces from URI in external links.
	;; In internal ones, treat indentation as a single space.
	(cond
	 ;; File type.
	 ((or (file-name-absolute-p raw-link)
	      (string-match "\\`\\.\\.?/" raw-link))
	  (setq type "file")
	  (setq path raw-link))
	 ;; Explicit type (http, irc, bbdb...).
	 ((string-match org-link-types-re raw-link)
	  (setq type (match-string 1 raw-link))
	  (setq path (substring raw-link (match-end 0))))
	 ;; Code-ref type: PATH is the name of the reference.
	 ((and (string-match-p "\\`(" raw-link)
	       (string-match-p ")\\'" raw-link))
	  (setq type "coderef")
	  (setq path (substring raw-link 1 -1)))
	 ;; Custom-id type: PATH is the name of the custom id.
	 ((= (string-to-char raw-link) ?#)
	  (setq type "custom-id")
	  (setq path (substring raw-link 1)))
	 ;; Fuzzy type: Internal link either matches a target, an
	 ;; headline name or nothing.  PATH is the target or
	 ;; headline's name.
	 (t
	  (setq type "fuzzy")
	  (setq path raw-link))))
       ;; Type 3: Plain link, e.g., https://orgmode.org
       ((looking-at org-link-plain-re)
	(setq format 'plain)
	(setq raw-link (match-string-no-properties 0))
	(setq type (match-string-no-properties 1))
	(setq link-end (match-end 0))
	(setq path (match-string-no-properties 2)))
       ;; Type 4: Angular link, e.g., <https://orgmode.org>.  Unlike to
       ;; bracket links, follow RFC 3986 and remove any extra
       ;; whitespace in URI.
       ((looking-at org-link-angle-re)
	(setq format 'angle)
	(setq type (match-string-no-properties 1))
	(setq link-end (match-end 0))
	(setq raw-link
	      (buffer-substring-no-properties
	       (match-beginning 1) (match-end 2)))
	(setq path (replace-regexp-in-string
		    "[ \t]*\n[ \t]*" "" (match-string-no-properties 2))))
       (t (throw 'no-object nil)))
      ;; In any case, deduce end point after trailing white space from
      ;; LINK-END variable.
      (save-excursion
	(setq post-blank
	      (progn (goto-char link-end) (skip-chars-forward " \t")))
	(setq end (point)))
      ;; Special "file"-type link processing.  Extract opening
      ;; application and search option, if any.  Also normalize URI.
      (when (string-match "\\`file\\(?:\\+\\(.+\\)\\)?\\'" type)
	(setq application (match-string 1 type))
	(setq type "file")
	(when (string-match "::\\(.*\\)\\'" path)
	  (setq search-option (match-string 1 path))
	  (setq path (replace-match "" nil nil path)))
	(setq path (replace-regexp-in-string "\\`///*\\(.:\\)?/" "\\1/" path)))
      ;; Translate link, if `org-link-translation-function' is set.
      (let ((trans (and (functionp org-link-translation-function)
			(funcall org-link-translation-function type path))))
	(when trans
	  (setq type (car trans))
	  (setq path (cdr trans))))
      (list 'link
	    (list :type type
		  :path path
		  :format format
		  :raw-link (or raw-link path)
		  :application application
		  :search-option search-option
		  :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-link-interpreter (link contents)
  "Interpret LINK object as Org syntax.
CONTENTS is the contents of the object, or nil."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (if (string= type "radio") path
      (let ((fmt (pcase (org-element-property :format link)
		   ;; Links with contents and internal links have to
		   ;; use bracket syntax.  Ignore `:format' in these
		   ;; cases.  This is also the default syntax when the
		   ;; property is not defined, e.g., when the object
		   ;; was crafted by the user.
		   ((guard contents)
		    (format "[[%%s][%s]]"
			    ;; Since this is going to be used as
			    ;; a format string, escape percent signs
			    ;; in description.
			    (replace-regexp-in-string "%" "%%" contents)))
		   ((or `bracket
			`nil
			(guard (member type '("coderef" "custom-id" "fuzzy"))))
		    "[[%s]]")
		   ;; Otherwise, just obey to `:format'.
		   (`angle "<%s>")
		   (`plain "%s")
		   (f (error "Wrong `:format' value: %s" f)))))
	(format fmt
		(pcase type
		  ("coderef" (format "(%s)" path))
		  ("custom-id" (concat "#" path))
		  ("file"
		   (let ((app (org-element-property :application link))
			 (opt (org-element-property :search-option link)))
		     (concat type (and app (concat "+" app)) ":"
			     path
			     (and opt (concat "::" opt)))))
		  ("fuzzy" path)
		  (_ (concat type ":" path))))))))


;;;; Macro

(defun org-element-macro-parser ()
  "Parse macro at point, if any.

When at a macro, return a list whose car is `macro' and cdr
a plist with `:key', `:args', `:begin', `:end', `:value' and
`:post-blank' as keywords.  Otherwise, return nil.

Assume point is at the macro."
  (save-excursion
    (when (looking-at "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\((\\([^\000]*?\\))\\)?}}}")
      (let ((begin (point))
	    (key (downcase (match-string-no-properties 1)))
	    (value (match-string-no-properties 0))
	    (post-blank (progn (goto-char (match-end 0))
			       (skip-chars-forward " \t")))
	    (end (point))
	    (args (pcase (match-string-no-properties 3)
		    (`nil nil)
		    (a (org-macro-extract-arguments
			(replace-regexp-in-string
			 "[ \t\r\n]+" " " (org-trim a)))))))
	(list 'macro
	      (list :key key
		    :value value
		    :args args
		    :begin begin
		    :end end
		    :post-blank post-blank))))))

(defun org-element-macro-interpreter (macro _)
  "Interpret MACRO object as Org syntax."
  (format "{{{%s%s}}}"
	  (org-element-property :key macro)
	  (pcase (org-element-property :args macro)
	    (`nil "")
	    (args (format "(%s)" (apply #'org-macro-escape-arguments args))))))


;;;; Radio-target

(defun org-element-radio-target-parser ()
  "Parse radio target at point, if any.

When at a radio target, return a list whose car is `radio-target'
and cdr a plist with `:begin', `:end', `:contents-begin',
`:contents-end', `:value' and `:post-blank' as keywords.
Otherwise, return nil.

Assume point is at the radio target."
  (save-excursion
    (when (looking-at org-radio-target-regexp)
      (let ((begin (point))
	    (contents-begin (match-beginning 1))
	    (contents-end (match-end 1))
	    (value (match-string-no-properties 1))
	    (post-blank (progn (goto-char (match-end 0))
			       (skip-chars-forward " \t")))
	    (end (point)))
	(list 'radio-target
	      (list :begin begin
		    :end end
		    :contents-begin contents-begin
		    :contents-end contents-end
		    :post-blank post-blank
		    :value value))))))

(defun org-element-radio-target-interpreter (_ contents)
  "Interpret target object as Org syntax.
CONTENTS is the contents of the object."
  (concat "<<<" contents ">>>"))


;;;; Statistics Cookie

(defun org-element-statistics-cookie-parser ()
  "Parse statistics cookie at point, if any.

When at a statistics cookie, return a list whose car is
`statistics-cookie', and cdr a plist with `:begin', `:end',
`:value' and `:post-blank' keywords.  Otherwise, return nil.

Assume point is at the beginning of the statistics-cookie."
  (save-excursion
    (when (looking-at "\\[[0-9]*\\(%\\|/[0-9]*\\)\\]")
      (let* ((begin (point))
	     (value (buffer-substring-no-properties
		     (match-beginning 0) (match-end 0)))
	     (post-blank (progn (goto-char (match-end 0))
				(skip-chars-forward " \t")))
	     (end (point)))
	(list 'statistics-cookie
	      (list :begin begin
		    :end end
		    :value value
		    :post-blank post-blank))))))

(defun org-element-statistics-cookie-interpreter (statistics-cookie _)
  "Interpret STATISTICS-COOKIE object as Org syntax."
  (org-element-property :value statistics-cookie))


;;;; Strike-Through

(defun org-element-strike-through-parser ()
  "Parse strike-through object at point, if any.

When at a strike-through object, return a list whose car is
`strike-through' and cdr is a plist with `:begin', `:end',
`:contents-begin' and `:contents-end' and `:post-blank' keywords.
Otherwise, return nil.

Assume point is at the first plus sign marker."
  (org-element--parse-generic-emphasis "+" 'strike-through))

(defun org-element-strike-through-interpreter (_ contents)
  "Interpret strike-through object as Org syntax.
CONTENTS is the contents of the object."
  (format "+%s+" contents))


;;;; Subscript

(defun org-element-subscript-parser ()
  "Parse subscript at point, if any.

When at a subscript object, return a list whose car is
`subscript' and cdr a plist with `:begin', `:end',
`:contents-begin', `:contents-end', `:use-brackets-p' and
`:post-blank' as keywords.  Otherwise, return nil.

Assume point is at the underscore."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (looking-at org-match-substring-regexp)
      (let ((bracketsp (match-beginning 4))
	    (begin (match-beginning 2))
	    (contents-begin (or (match-beginning 4)
				(match-beginning 3)))
	    (contents-end (or (match-end 4) (match-end 3)))
	    (post-blank (progn (goto-char (match-end 0))
			       (skip-chars-forward " \t")))
	    (end (point)))
	(list 'subscript
	      (list :begin begin
		    :end end
		    :use-brackets-p bracketsp
		    :contents-begin contents-begin
		    :contents-end contents-end
		    :post-blank post-blank))))))

(defun org-element-subscript-interpreter (subscript contents)
  "Interpret SUBSCRIPT object as Org syntax.
CONTENTS is the contents of the object."
  (format
   (if (org-element-property :use-brackets-p subscript) "_{%s}" "_%s")
   contents))


;;;; Superscript

(defun org-element-superscript-parser ()
  "Parse superscript at point, if any.

When at a superscript object, return a list whose car is
`superscript' and cdr a plist with `:begin', `:end',
`:contents-begin', `:contents-end', `:use-brackets-p' and
`:post-blank' as keywords.  Otherwise, return nil.

Assume point is at the caret."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (looking-at org-match-substring-regexp)
      (let ((bracketsp (match-beginning 4))
	    (begin (match-beginning 2))
	    (contents-begin (or (match-beginning 4)
				(match-beginning 3)))
	    (contents-end (or (match-end 4) (match-end 3)))
	    (post-blank (progn (goto-char (match-end 0))
			       (skip-chars-forward " \t")))
	    (end (point)))
	(list 'superscript
	      (list :begin begin
		    :end end
		    :use-brackets-p bracketsp
		    :contents-begin contents-begin
		    :contents-end contents-end
		    :post-blank post-blank))))))

(defun org-element-superscript-interpreter (superscript contents)
  "Interpret SUPERSCRIPT object as Org syntax.
CONTENTS is the contents of the object."
  (format
   (if (org-element-property :use-brackets-p superscript) "^{%s}" "^%s")
   contents))


;;;; Table Cell

(defun org-element-table-cell-parser ()
  "Parse table cell at point.
Return a list whose car is `table-cell' and cdr is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end'
and `:post-blank' keywords."
  (looking-at "[ \t]*\\(.*?\\)[ \t]*\\(?:|\\|$\\)")
  (let* ((begin (match-beginning 0))
	 (end (match-end 0))
	 (contents-begin (match-beginning 1))
	 (contents-end (match-end 1)))
    (list 'table-cell
	  (list :begin begin
		:end end
		:contents-begin contents-begin
		:contents-end contents-end
		:post-blank 0))))

(defun org-element-table-cell-interpreter (_ contents)
  "Interpret table-cell element as Org syntax.
CONTENTS is the contents of the cell, or nil."
  (concat  " " contents " |"))


;;;; Target

(defun org-element-target-parser ()
  "Parse target at point, if any.

When at a target, return a list whose car is `target' and cdr
a plist with `:begin', `:end', `:value' and `:post-blank' as
keywords.  Otherwise, return nil.

Assume point is at the target."
  (save-excursion
    (when (looking-at org-target-regexp)
      (let ((begin (point))
	    (value (match-string-no-properties 1))
	    (post-blank (progn (goto-char (match-end 0))
			       (skip-chars-forward " \t")))
	    (end (point)))
	(list 'target
	      (list :begin begin
		    :end end
		    :value value
		    :post-blank post-blank))))))

(defun org-element-target-interpreter (target _)
  "Interpret TARGET object as Org syntax."
  (format "<<%s>>" (org-element-property :value target)))


;;;; Timestamp

(defconst org-element--timestamp-regexp
  (concat org-ts-regexp-both
	  "\\|"
	  "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
	  "\\|"
	  "\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
  "Regexp matching any timestamp type object.")

(defun org-element-timestamp-parser ()
  "Parse time stamp at point, if any.

When at a time stamp, return a list whose car is `timestamp', and
cdr a plist with `:type', `:raw-value', `:year-start',
`:month-start', `:day-start', `:hour-start', `:minute-start',
`:year-end', `:month-end', `:day-end', `:hour-end',
`:minute-end', `:repeater-type', `:repeater-value',
`:repeater-unit', `:warning-type', `:warning-value',
`:warning-unit', `:begin', `:end' and `:post-blank' keywords.
Otherwise, return nil.

Assume point is at the beginning of the timestamp."
  (when (looking-at-p org-element--timestamp-regexp)
    (save-excursion
      (let* ((begin (point))
	     (activep (eq (char-after) ?<))
	     (raw-value
	      (progn
		(looking-at (concat "\\([<[]\\(%%\\)?.*?\\)[]>]\\(?:--\\("
                                    org-ts-regexp-both
                                    "\\)\\)?"))
		(match-string-no-properties 0)))
	     (date-start (match-string-no-properties 1))
	     (date-end (match-string 3))
	     (diaryp (match-beginning 2))
	     (post-blank (progn (goto-char (match-end 0))
				(skip-chars-forward " \t")))
	     (end (point))
	     (time-range
	      (and (not diaryp)
		   (string-match
		    "[012]?[0-9]:[0-5][0-9]\\(-\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)"
		    date-start)
		   (cons (string-to-number (match-string 2 date-start))
			 (string-to-number (match-string 3 date-start)))))
	     (type (cond (diaryp 'diary)
			 ((and activep (or date-end time-range)) 'active-range)
			 (activep 'active)
			 ((or date-end time-range) 'inactive-range)
			 (t 'inactive)))
	     (repeater-props
	      (and (not diaryp)
		   (string-match "\\([.+]?\\+\\)\\([0-9]+\\)\\([hdwmy]\\)"
				 raw-value)
		   (list
		    :repeater-type
		    (let ((type (match-string 1 raw-value)))
		      (cond ((equal "++" type) 'catch-up)
			    ((equal ".+" type) 'restart)
			    (t 'cumulate)))
		    :repeater-value (string-to-number (match-string 2 raw-value))
		    :repeater-unit
		    (pcase (string-to-char (match-string 3 raw-value))
		      (?h 'hour) (?d 'day) (?w 'week) (?m 'month) (_ 'year)))))
	     (warning-props
	      (and (not diaryp)
		   (string-match "\\(-\\)?-\\([0-9]+\\)\\([hdwmy]\\)" raw-value)
		   (list
		    :warning-type (if (match-string 1 raw-value) 'first 'all)
		    :warning-value (string-to-number (match-string 2 raw-value))
		    :warning-unit
		    (pcase (string-to-char (match-string 3 raw-value))
		      (?h 'hour) (?d 'day) (?w 'week) (?m 'month) (_ 'year)))))
	     year-start month-start day-start hour-start minute-start year-end
	     month-end day-end hour-end minute-end)
	;; Parse date-start.
	(unless diaryp
	  (let ((date (org-parse-time-string date-start t)))
	    (setq year-start (nth 5 date)
		  month-start (nth 4 date)
		  day-start (nth 3 date)
		  hour-start (nth 2 date)
		  minute-start (nth 1 date))))
	;; Compute date-end.  It can be provided directly in time-stamp,
	;; or extracted from time range.  Otherwise, it defaults to the
	;; same values as date-start.
	(unless diaryp
	  (let ((date (and date-end (org-parse-time-string date-end t))))
	    (setq year-end (or (nth 5 date) year-start)
		  month-end (or (nth 4 date) month-start)
		  day-end (or (nth 3 date) day-start)
		  hour-end (or (nth 2 date) (car time-range) hour-start)
		  minute-end (or (nth 1 date) (cdr time-range) minute-start))))
	(list 'timestamp
	      (nconc (list :type type
			   :raw-value raw-value
			   :year-start year-start
			   :month-start month-start
			   :day-start day-start
			   :hour-start hour-start
			   :minute-start minute-start
			   :year-end year-end
			   :month-end month-end
			   :day-end day-end
			   :hour-end hour-end
			   :minute-end minute-end
			   :begin begin
			   :end end
			   :post-blank post-blank)
		     repeater-props
		     warning-props))))))

(defun org-element-timestamp-interpreter (timestamp _)
  "Interpret TIMESTAMP object as Org syntax."
  (let* ((repeat-string
	  (concat
	   (pcase (org-element-property :repeater-type timestamp)
	     (`cumulate "+") (`catch-up "++") (`restart ".+"))
	   (let ((val (org-element-property :repeater-value timestamp)))
	     (and val (number-to-string val)))
	   (pcase (org-element-property :repeater-unit timestamp)
	     (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
	 (warning-string
	  (concat
	   (pcase (org-element-property :warning-type timestamp)
	     (`first "--") (`all "-"))
	   (let ((val (org-element-property :warning-value timestamp)))
	     (and val (number-to-string val)))
	   (pcase (org-element-property :warning-unit timestamp)
	     (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
	 (build-ts-string
	  ;; Build an Org timestamp string from TIME.  ACTIVEP is
	  ;; non-nil when time stamp is active.  If WITH-TIME-P is
	  ;; non-nil, add a time part.  HOUR-END and MINUTE-END
	  ;; specify a time range in the timestamp.  REPEAT-STRING is
	  ;; the repeater string, if any.
	  (lambda (time activep &optional with-time-p hour-end minute-end)
	    (let ((ts (format-time-string
                       (org-time-stamp-format with-time-p)
		       time)))
	      (when (and hour-end minute-end)
		(string-match "[012]?[0-9]:[0-5][0-9]" ts)
		(setq ts
		      (replace-match
		       (format "\\&-%02d:%02d" hour-end minute-end)
		       nil nil ts)))
	      (unless activep (setq ts (format "[%s]" (substring ts 1 -1))))
	      (dolist (s (list repeat-string warning-string))
		(when (org-string-nw-p s)
		  (setq ts (concat (substring ts 0 -1)
				   " "
				   s
				   (substring ts -1)))))
	      ;; Return value.
	      ts)))
	 (type (org-element-property :type timestamp)))
    (pcase type
      ((or `active `inactive)
       (let* ((minute-start (org-element-property :minute-start timestamp))
	      (minute-end (org-element-property :minute-end timestamp))
	      (hour-start (org-element-property :hour-start timestamp))
	      (hour-end (org-element-property :hour-end timestamp))
	      (time-range-p (and hour-start hour-end minute-start minute-end
				 (or (/= hour-start hour-end)
				     (/= minute-start minute-end)))))
	 (funcall
	  build-ts-string
	  (org-encode-time 0
                           (or minute-start 0)
                           (or hour-start 0)
                           (org-element-property :day-start timestamp)
                           (org-element-property :month-start timestamp)
                           (org-element-property :year-start timestamp))
	  (eq type 'active)
	  (and hour-start minute-start)
	  (and time-range-p hour-end)
	  (and time-range-p minute-end))))
      ((or `active-range `inactive-range)
       (let ((minute-start (org-element-property :minute-start timestamp))
	     (minute-end (org-element-property :minute-end timestamp))
	     (hour-start (org-element-property :hour-start timestamp))
	     (hour-end (org-element-property :hour-end timestamp)))
	 (concat
	  (funcall
	   build-ts-string (org-encode-time
			    0
			    (or minute-start 0)
			    (or hour-start 0)
			    (org-element-property :day-start timestamp)
			    (org-element-property :month-start timestamp)
			    (org-element-property :year-start timestamp))
	   (eq type 'active-range)
	   (and hour-start minute-start))
	  "--"
	  (funcall build-ts-string
		   (org-encode-time
                    0
                    (or minute-end 0)
                    (or hour-end 0)
                    (org-element-property :day-end timestamp)
                    (org-element-property :month-end timestamp)
                    (org-element-property :year-end timestamp))
		   (eq type 'active-range)
		   (and hour-end minute-end)))))
      (_ (org-element-property :raw-value timestamp)))))


;;;; Underline

(defun org-element-underline-parser ()
  "Parse underline object at point, if any.

When at an underline object, return a list whose car is
`underline' and cdr is a plist with `:begin', `:end',
`:contents-begin' and `:contents-end' and `:post-blank' keywords.
Otherwise, return nil.

Assume point is at the first underscore marker."
  (org-element--parse-generic-emphasis "_" 'underline))

(defun org-element-underline-interpreter (_ contents)
  "Interpret underline object as Org syntax.
CONTENTS is the contents of the object."
  (format "_%s_" contents))


;;;; Verbatim

(defun org-element-verbatim-parser ()
  "Parse verbatim object at point, if any.

When at a verbatim object, return a list whose car is `verbatim'
and cdr is a plist with `:value', `:begin', `:end' and
`:post-blank' keywords.  Otherwise, return nil.

Assume point is at the first equal sign marker."
  (org-element--parse-generic-emphasis "=" 'verbatim))

(defun org-element-verbatim-interpreter (verbatim _)
  "Interpret VERBATIM object as Org syntax."
  (format "=%s=" (org-element-property :value verbatim)))



;;; Parsing Element Starting At Point
;;
;; `org-element--current-element' is the core function of this section.
;; It returns the Lisp representation of the element starting at
;; point.

(defvar org-element--cache-sync-requests); Declared later
(defun org-element--current-element (limit &optional granularity mode structure add-to-cache)
  "Parse the element starting at point.

Return value is a list like (TYPE PROPS) where TYPE is the type
of the element and PROPS a plist of properties associated to the
element.

Possible types are defined in `org-element-all-elements'.

LIMIT bounds the search.

Optional argument GRANULARITY determines the depth of the
recursion.  Allowed values are `headline', `greater-element',
`element', `object' or nil.  When it is broader than `object' (or
nil), secondary values will not be parsed, since they only
contain objects.

Optional argument MODE, when non-nil, can be either
`first-section', `item', `node-property', `planning',
`property-drawer', `section', `table-row', or `top-comment'.


If STRUCTURE isn't provided but MODE is set to `item', it will be
computed.

Optional argument ADD-TO-CACHE, when non-nil, and when cache is active,
will also add current element to cache if it is not yet there.  Use
this argument with care, as validity of the element in parse tree is
not checked.

This function assumes point is always at the beginning of the
element it has to parse."
  (let* ((element (and (not (buffer-narrowed-p))
                       (org-element--cache-active-p)
                       (not org-element--cache-sync-requests)
                       (org-element--cache-find (point) t)))
         (element (progn (while (and element
                                     (not (and (eq (point) (org-element-property :begin element))
                                               (eq mode (org-element-property :mode element)))))
                           (setq element (org-element-property :parent element)))
                         element))
         (old-element element)
         (element (when
                      (pcase (org-element-property :granularity element)
                        (`nil t)
                        (`object t)
                        (`element (not (memq granularity '(nil object))))
                        (`greater-element (not (memq granularity '(nil object element))))
                        (`headline (eq granularity 'headline)))
                    element)))
    (if element
        element
      (save-excursion
        (let ((case-fold-search t)
	      ;; Determine if parsing depth allows for secondary strings
	      ;; parsing.  It only applies to elements referenced in
	      ;; `org-element-secondary-value-alist'.
	      (raw-secondary-p (and granularity (not (eq granularity 'object))))
              result)
          (setq
           result
           (cond
            ;; Item.
            ((eq mode 'item)
	     (org-element-item-parser limit structure raw-secondary-p))
            ;; Table Row.
            ((eq mode 'table-row) (org-element-table-row-parser limit))
            ;; Node Property.
            ((eq mode 'node-property) (org-element-node-property-parser limit))
            ;; Headline.
            ((org-with-limited-levels (looking-at-p org-outline-regexp-bol))
             (org-element-headline-parser limit raw-secondary-p))
            ;; Sections (must be checked after headline).
            ((eq mode 'section) (org-element-section-parser limit))
            ((eq mode 'first-section)
	     (org-element-section-parser
	      (or (save-excursion (org-with-limited-levels (outline-next-heading)))
	          limit)))
            ;; Comments.
            ((looking-at "^[ \t]*#\\(?: \\|$\\)")
	     (org-element-comment-parser limit))
            ;; Planning.
            ((and (eq mode 'planning)
	          (eq ?* (char-after (line-beginning-position 0)))
	          (looking-at org-element-planning-line-re))
	     (org-element-planning-parser limit))
            ;; Property drawer.
            ((and (pcase mode
	            (`planning (eq ?* (char-after (line-beginning-position 0))))
	            ((or `property-drawer `top-comment)
		     (save-excursion
		       (beginning-of-line 0)
		       (not (looking-at "[[:blank:]]*$"))))
	            (_ nil))
	          (looking-at org-property-drawer-re))
	     (org-element-property-drawer-parser limit))
            ;; When not at bol, point is at the beginning of an item or
            ;; a footnote definition: next item is always a paragraph.
            ((not (bolp)) (org-element-paragraph-parser limit (list (point))))
            ;; Clock.
            ((looking-at org-element-clock-line-re)
             (org-element-clock-parser limit))
            ;; Inlinetask.
            ((looking-at "^\\*+ ")
	     (org-element-inlinetask-parser limit raw-secondary-p))
            ;; From there, elements can have affiliated keywords.
            (t (let ((affiliated (org-element--collect-affiliated-keywords
			          limit (memq granularity '(nil object)))))
	         (cond
	          ;; Jumping over affiliated keywords put point off-limits.
	          ;; Parse them as regular keywords.
	          ((and (cdr affiliated) (>= (point) limit))
	           (goto-char (car affiliated))
	           (org-element-keyword-parser limit nil))
	          ;; LaTeX Environment.
	          ((looking-at org-element--latex-begin-environment)
	           (org-element-latex-environment-parser limit affiliated))
	          ;; Drawer.
	          ((looking-at org-element-drawer-re)
	           (org-element-drawer-parser limit affiliated))
	          ;; Fixed Width
	          ((looking-at "[ \t]*:\\( \\|$\\)")
	           (org-element-fixed-width-parser limit affiliated))
	          ;; Inline Comments, Blocks, Babel Calls, Dynamic Blocks and
	          ;; Keywords.
	          ((looking-at "[ \t]*#\\+")
	           (goto-char (match-end 0))
	           (cond
	            ((looking-at "BEGIN_\\(\\S-+\\)")
		     (beginning-of-line)
		     (funcall (pcase (upcase (match-string 1))
			        ("CENTER"  #'org-element-center-block-parser)
			        ("COMMENT" #'org-element-comment-block-parser)
			        ("EXAMPLE" #'org-element-example-block-parser)
			        ("EXPORT"  #'org-element-export-block-parser)
			        ("QUOTE"   #'org-element-quote-block-parser)
			        ("SRC"     #'org-element-src-block-parser)
			        ("VERSE"   #'org-element-verse-block-parser)
			        (_         #'org-element-special-block-parser))
			      limit
			      affiliated))
	            ((looking-at "CALL:")
		     (beginning-of-line)
		     (org-element-babel-call-parser limit affiliated))
	            ((save-excursion
                       (beginning-of-line)
                       (looking-at org-element-dynamic-block-open-re))
		     (beginning-of-line)
		     (org-element-dynamic-block-parser limit affiliated))
	            ((looking-at "\\S-+:")
		     (beginning-of-line)
		     (org-element-keyword-parser limit affiliated))
	            (t
		     (beginning-of-line)
		     (org-element-paragraph-parser limit affiliated))))
	          ;; Footnote Definition.
	          ((looking-at org-footnote-definition-re)
	           (org-element-footnote-definition-parser limit affiliated))
	          ;; Horizontal Rule.
	          ((looking-at "[ \t]*-\\{5,\\}[ \t]*$")
	           (org-element-horizontal-rule-parser limit affiliated))
	          ;; Diary Sexp.
	          ((looking-at "%%(")
	           (org-element-diary-sexp-parser limit affiliated))
	          ;; Table.
	          ((or (looking-at "[ \t]*|")
		       ;; There is no strict definition of a table.el
		       ;; table.  Try to prevent false positive while being
		       ;; quick.
		       (let ((rule-regexp
			      (rx (zero-or-more (any " \t"))
			          "+"
			          (one-or-more (one-or-more "-") "+")
			          (zero-or-more (any " \t"))
			          eol))
			     (non-table.el-line
			      (rx bol
			          (zero-or-more (any " \t"))
			          (or eol (not (any "+| \t")))))
			     (next (line-beginning-position 2)))
		         ;; Start with a full rule.
		         (and
		          (looking-at rule-regexp)
		          (< next limit) ;no room for a table.el table
		          (save-excursion
		            (end-of-line)
		            (cond
			     ;; Must end with a full rule.
			     ((not (re-search-forward non-table.el-line limit 'move))
			      (if (bolp) (forward-line -1) (beginning-of-line))
			      (looking-at rule-regexp))
			     ;; Ignore pseudo-tables with a single
			     ;; rule.
			     ((= next (line-beginning-position))
			      nil)
			     ;; Must end with a full rule.
			     (t
			      (forward-line -1)
			      (looking-at rule-regexp)))))))
	           (org-element-table-parser limit affiliated))
	          ;; List.
	          ((looking-at (org-item-re))
	           (org-element-plain-list-parser
	            limit affiliated
	            (or structure (org-element--list-struct limit))))
	          ;; Default element: Paragraph.
	          (t (org-element-paragraph-parser limit affiliated)))))))
          (when result
            (org-element-put-property result :mode mode)
            (org-element-put-property result :granularity granularity))
          (when (and (not (buffer-narrowed-p))
                     (org-element--cache-active-p)
                     (not org-element--cache-sync-requests)
                     add-to-cache)
            (if (not old-element)
                (setq result (org-element--cache-put result))
              (org-element-set-element old-element result)
              (setq result old-element)))
          result)))))


;; Most elements can have affiliated keywords.  When looking for an
;; element beginning, we want to move before them, as they belong to
;; that element, and, in the meantime, collect information they give
;; into appropriate properties.  Hence the following function.

(defun org-element--collect-affiliated-keywords (limit parse)
  "Collect affiliated keywords from point down to LIMIT.

Return a list whose CAR is the position at the first of them and
CDR a plist of keywords and values and move point to the
beginning of the first line after them.

As a special case, if element doesn't start at the beginning of
the line (e.g., a paragraph starting an item), CAR is current
position of point and CDR is nil.

When PARSE is non-nil, values from keywords belonging to
`org-element-parsed-keywords' are parsed as secondary strings."
  (if (not (bolp)) (list (point))
    (let ((case-fold-search t)
	  (origin (point))
	  ;; RESTRICT is the list of objects allowed in parsed
	  ;; keywords value.  If PARSE is nil, no object is allowed.
	  (restrict (and parse (org-element-restriction 'keyword)))
	  output)
      (while (and (< (point) limit) (looking-at org-element--affiliated-re))
	(let* ((raw-kwd (upcase (match-string 1)))
	       ;; Apply translation to RAW-KWD.  From there, KWD is
	       ;; the official keyword.
	       (kwd (or (cdr (assoc raw-kwd
				    org-element-keyword-translation-alist))
			raw-kwd))
	       ;; PARSED? is non-nil when keyword should have its
	       ;; value parsed.
	       (parsed? (member kwd org-element-parsed-keywords))
	       ;; Find main value for any keyword.
	       (value
		(let ((beg (match-end 0))
		      (end (save-excursion
			     (end-of-line)
			     (skip-chars-backward " \t")
			     (point))))
		  (if parsed?
		      (save-match-data
			(org-element--parse-objects beg end nil restrict))
		    (org-trim (buffer-substring-no-properties beg end)))))
	       ;; If KWD is a dual keyword, find its secondary value.
	       ;; Maybe parse it.
	       (dual? (member kwd org-element-dual-keywords))
	       (dual-value
		(and dual?
		     (let ((sec (match-string-no-properties 2)))
		       (cond
			((and sec parsed?)
			 (save-match-data
			   (org-element--parse-objects
			    (match-beginning 2) (match-end 2) nil restrict)))
			(sec sec)))))
	       ;; Attribute a property name to KWD.
	       (kwd-sym (and kwd (intern (concat ":" (downcase kwd))))))
	  ;; Now set final shape for VALUE.
	  (when dual?
	    (setq value (and (or value dual-value) (cons value dual-value))))
	  (when (or (member kwd org-element-multiple-keywords)
		    ;; Attributes can always appear on multiple lines.
		    (string-match "^ATTR_" kwd))
	    (setq value (cons value (plist-get output kwd-sym))))
	  ;; Eventually store the new value in OUTPUT.
	  (setq output (plist-put output kwd-sym value))
	  ;; Move to next keyword.
	  (forward-line)))
      ;; If affiliated keywords are orphaned: move back to first one.
      ;; They will be parsed as a paragraph.
      (when (looking-at "[ \t]*$") (goto-char origin) (setq output nil))
      ;; Return value.
      (cons origin output))))



;;; The Org Parser
;;
;; The two major functions here are `org-element-parse-buffer', which
;; parses Org syntax inside the current buffer, taking into account
;; region, narrowing, or even visibility if specified, and
;; `org-element-parse-secondary-string', which parses objects within
;; a given string.
;;
;; The (almost) almighty `org-element-map' allows applying a function
;; on elements or objects matching some type, and accumulating the
;; resulting values.  In an export situation, it also skips unneeded
;; parts of the parse tree.

(defun org-element-parse-buffer (&optional granularity visible-only)
  "Recursively parse the buffer and return structure.
If narrowing is in effect, only parse the visible part of the
buffer.

Optional argument GRANULARITY determines the depth of the
recursion.  It can be set to the following symbols:

`headline'          Only parse headlines.
`greater-element'   Don't recurse into greater elements except
		    headlines and sections.  Thus, elements
		    parsed are the top-level ones.
`element'           Parse everything but objects and plain text.
`object'            Parse the complete buffer (default).

When VISIBLE-ONLY is non-nil, don't parse contents of hidden
elements.

An element or object is represented as a list with the
pattern (TYPE PROPERTIES CONTENTS), where :

  TYPE is a symbol describing the element or object.  See
  `org-element-all-elements' and `org-element-all-objects' for an
  exhaustive list of such symbols.  One can retrieve it with
  `org-element-type' function.

  PROPERTIES is the list of attributes attached to the element or
  object, as a plist.  Although most of them are specific to the
  element or object type, all types share `:begin', `:end',
  `:post-blank' and `:parent' properties, which respectively
  refer to buffer position where the element or object starts,
  ends, the number of white spaces or blank lines after it, and
  the element or object containing it.  Properties values can be
  obtained by using `org-element-property' function.

  CONTENTS is a list of elements, objects or raw strings
  contained in the current element or object, when applicable.
  One can access them with `org-element-contents' function.

The Org buffer has `org-data' as type and nil as properties.
`org-element-map' function can be used to find specific elements
or objects within the parse tree.

This function assumes that current major mode is `org-mode'."
  (save-excursion
    (goto-char (point-min))
    (let ((org-data (org-element-org-data-parser))
          (gc-cons-threshold #x40000000))
      (org-skip-whitespace)
      (org-element--parse-elements
       (line-beginning-position) (point-max)
       ;; Start in `first-section' mode so text before the first
       ;; headline belongs to a section.
       'first-section nil granularity visible-only org-data))))

(defun org-element-parse-secondary-string (string restriction &optional parent)
  "Recursively parse objects in STRING and return structure.

RESTRICTION is a symbol limiting the object types that will be
looked after.

Optional argument PARENT, when non-nil, is the element or object
containing the secondary string.  It is used to set correctly
`:parent' property within the string.

If STRING is the empty string or nil, return nil."
  (cond
   ((not string) nil)
   ((equal string "") nil)
   (t (let ((local-variables (buffer-local-variables)))
	(with-temp-buffer
	  (dolist (v local-variables)
	    (ignore-errors
	      (if (symbolp v) (makunbound v)
		;; Don't set file name to avoid mishandling hooks (bug#44524)
		(unless (memq (car v) '(buffer-file-name buffer-file-truename))
		  (set (make-local-variable (car v)) (cdr v))))))
	  ;; Transferring local variables may put the temporary buffer
	  ;; into a read-only state.  Make sure we can insert STRING.
	  (let ((inhibit-read-only t)) (insert string))
	  ;; Prevent "Buffer *temp* modified; kill anyway?".
	  (restore-buffer-modified-p nil)
	  (org-element--parse-objects
	   (point-min) (point-max) nil restriction parent))))))

(defun org-element-map
    (data types fun &optional info first-match no-recursion with-affiliated)
  "Map a function on selected elements or objects.

DATA is a parse tree (for example, returned by
`org-element-parse-buffer'), an element, an object, a string, or a
list of such constructs.  TYPES is a symbol or list of symbols of
elements or object types (see `org-element-all-elements' and
`org-element-all-objects' for a complete list of types).  FUN is the
function called on the matching element or object.  It has to accept
one argument: the element or object itself.

When optional argument INFO is non-nil, it should be a plist
holding export options.  In that case, parts of the parse tree
not exportable according to that property list will be skipped.

When optional argument FIRST-MATCH is non-nil, stop at the first
match for which FUN doesn't return nil, and return that value.

Optional argument NO-RECURSION is a symbol or a list of symbols
representing elements or objects types.  `org-element-map' won't
enter any recursive element or object whose type belongs to that
list.  Though, FUN can still be applied on them.

When optional argument WITH-AFFILIATED is non-nil, FUN will also
apply to matching objects within parsed affiliated keywords (see
`org-element-parsed-keywords').

Nil values returned from FUN do not appear in the results.


Examples:
---------

Assuming TREE is a variable containing an Org buffer parse tree,
the following example will return a flat list of all `src-block'
and `example-block' elements in it:

  (setq tree (org-element-parse-buffer))
  (org-element-map tree \\='(example-block src-block) #\\='identity)

The following snippet will find the first headline with a level
of 1 and a \"phone\" tag, and will return its beginning position:

  (org-element-map tree \\='headline
   (lambda (hl)
     (and (= (org-element-property :level hl) 1)
          (member \"phone\" (org-element-property :tags hl))
          (org-element-property :begin hl)))
   nil t)

The next example will return a flat list of all `plain-list' type
elements in TREE that are not a sub-list themselves:

  (org-element-map tree \\='plain-list #\\='identity nil nil \\='plain-list)

Eventually, this example will return a flat list of all `bold'
type objects containing a `latex-snippet' type object, even
looking into captions:

  (org-element-map tree \\='bold
   (lambda (b)
     (and (org-element-map b \\='latex-snippet #\\='identity nil t) b))
   nil nil nil t)"
  (declare (indent 2))
  ;; Ensure TYPES and NO-RECURSION are a list, even of one element.
  (let* ((types (if (listp types) types (list types)))
	 (no-recursion (if (listp no-recursion) no-recursion
			 (list no-recursion)))
	 ;; Recursion depth is determined by --CATEGORY.
	 (--category
	  (catch :--found
	    (let ((category 'greater-elements)
		  (all-objects (cons 'plain-text org-element-all-objects)))
	      (dolist (type types category)
		(cond ((memq type all-objects)
		       ;; If one object is found, the function has
		       ;; to recurse into every object.
		       (throw :--found 'objects))
		      ((not (memq type org-element-greater-elements))
		       ;; If one regular element is found, the
		       ;; function has to recurse, at least, into
		       ;; every element it encounters.
		       (and (not (eq category 'elements))
			    (setq category 'elements))))))))
         (--ignore-list (plist-get info :ignore-list))
	 --acc)
    (letrec ((--walk-tree
	      (lambda (--data)
		;; Recursively walk DATA.  INFO, if non-nil, is a plist
		;; holding contextual information.
		(let ((--type (org-element-type --data)))
		  (cond
		   ((not --data))
		   ;; Ignored element in an export context.
		   ((and info (memq --data --ignore-list)))
		   ;; List of elements or objects.
		   ((not --type) (mapc --walk-tree --data))
		   ;; Unconditionally enter parse trees.
		   ((eq --type 'org-data)
		    (mapc --walk-tree (org-element-contents --data)))
		   (t
		    ;; Check if TYPE is matching among TYPES.  If so,
		    ;; apply FUN to --DATA and accumulate return value
		    ;; into --ACC (or exit if FIRST-MATCH is non-nil).
		    (when (memq --type types)
		      (let ((result (funcall fun --data)))
			(cond ((not result))
			      (first-match (throw :--map-first-match result))
			      (t (push result --acc)))))
		    ;; If --DATA has a secondary string that can contain
		    ;; objects with their type among TYPES, look inside.
		    (when (and (eq --category 'objects) (not (stringp --data)))
		      (dolist (p (cdr (assq --type
					    org-element-secondary-value-alist)))
			(funcall --walk-tree (org-element-property p --data))))
		    ;; If --DATA has any parsed affiliated keywords and
		    ;; WITH-AFFILIATED is non-nil, look for objects in
		    ;; them.
		    (when (and with-affiliated
			       (eq --category 'objects)
			       (eq (org-element-class --data) 'element))
		      (dolist (kwd-pair org-element--parsed-properties-alist)
			(let ((kwd (car kwd-pair))
			      (value (org-element-property (cdr kwd-pair) --data)))
			  ;; Pay attention to the type of parsed
			  ;; keyword.  In particular, preserve order for
			  ;; multiple keywords.
			  (cond
			   ((not value))
			   ((member kwd org-element-dual-keywords)
			    (if (member kwd org-element-multiple-keywords)
				(dolist (line (reverse value))
				  (funcall --walk-tree (cdr line))
				  (funcall --walk-tree (car line)))
			      (funcall --walk-tree (cdr value))
			      (funcall --walk-tree (car value))))
			   ((member kwd org-element-multiple-keywords)
			    (mapc --walk-tree (reverse value)))
			   (t (funcall --walk-tree value))))))
		    ;; Determine if a recursion into --DATA is possible.
		    (cond
		     ;; --TYPE is explicitly removed from recursion.
		     ((memq --type no-recursion))
		     ;; --DATA has no contents.
		     ((not (org-element-contents --data)))
		     ;; Looking for greater elements but --DATA is
		     ;; simply an element or an object.
		     ((and (eq --category 'greater-elements)
			   (not (memq --type org-element-greater-elements))))
		     ;; Looking for elements but --DATA is an object.
		     ((and (eq --category 'elements)
			   (eq (org-element-class --data) 'object)))
		     ;; In any other case, map contents.
		     (t (mapc --walk-tree (org-element-contents --data))))))))))
      (catch :--map-first-match
	(funcall --walk-tree data)
	;; Return value in a proper order.
	(nreverse --acc)))))

;; The following functions are internal parts of the parser.
;;
;; The first one, `org-element--parse-elements' acts at the element's
;; level.
;;
;; The second one, `org-element--parse-objects' applies on all objects
;; of a paragraph or a secondary string.  It calls
;; `org-element--object-lex' to find the next object in the current
;; container.

(defsubst org-element--next-mode (mode type parent?)
  "Return next mode according to current one.

MODE is a symbol representing the expectation about the next
element or object.  Meaningful values are `first-section',
`item', `node-property', `planning', `property-drawer',
`section', `table-row', `top-comment', and nil.

TYPE is the type of the current element or object.

If PARENT? is non-nil, assume the next element or object will be
located inside the current one."
  (if parent?
      (pcase type
	(`headline 'section)
	((and (guard (eq mode 'first-section)) `section) 'top-comment)
        ((and (guard (eq mode 'org-data)) `org-data) 'first-section)
        ((and (guard (not mode)) `org-data) 'first-section)
	(`inlinetask 'planning)
	(`plain-list 'item)
	(`property-drawer 'node-property)
	(`section 'planning)
	(`table 'table-row))
    (pcase mode
      (`item 'item)
      (`node-property 'node-property)
      ((and `planning (guard (eq type 'planning))) 'property-drawer)
      (`table-row 'table-row)
      ((and `top-comment (guard (eq type 'comment))) 'property-drawer))))

(defun org-element--parse-elements
    (beg end mode structure granularity visible-only acc)
  "Parse elements between BEG and END positions.

MODE prioritizes some elements over the others.  It can be set to
`first-section', `item', `node-property', `planning',
`property-drawer', `section', `table-row', `top-comment', or nil.

When value is `item', STRUCTURE will be used as the current list
structure.

GRANULARITY determines the depth of the recursion.  See
`org-element-parse-buffer' for more information.

When VISIBLE-ONLY is non-nil, don't parse contents of hidden
elements.

Elements are accumulated into ACC."
  (save-excursion
    (goto-char beg)
    ;; When parsing only headlines, skip any text before first one.
    (when (and (eq granularity 'headline) (not (org-at-heading-p)))
      (org-with-limited-levels (outline-next-heading)))
    (let (elements)
      (while (< (point) end)
	;; Visible only: skip invisible parts due to folding.
	(if (and visible-only (org-invisible-p nil t))
	    (progn
	      (goto-char (org-find-visible))
	      (when (and (eolp) (not (eobp))) (forward-char)))
	  ;; Find current element's type and parse it accordingly to
	  ;; its category.
	  (let* ((element (org-element-copy
                           ;; `org-element--current-element' may return cached
                           ;; elements.  Below code reassigns
                           ;; `:parent' property of the element and
                           ;; may interfere with cache
                           ;; synchronization if parent element is not
                           ;; yet in cache.  Moreover, the returned
                           ;; structure may be altered by caller code
                           ;; arbitrarily.  Hence, we return a copy of
                           ;; the potentially cached element to make
                           ;; potential modifications safe for element
                           ;; cache.
                           (org-element--current-element
			    end granularity mode structure)))
		 (type (org-element-type element))
		 (cbeg (org-element-property :contents-begin element)))
	    (goto-char (org-element-property :end element))
	    ;; Fill ELEMENT contents by side-effect.
	    (cond
	     ;; If element has no contents, don't modify it.
	     ((not cbeg))
	     ;; Greater element: parse it between `contents-begin' and
	     ;; `contents-end'.  Ensure GRANULARITY allows recursion,
	     ;; or ELEMENT is a headline, in which case going inside
	     ;; is mandatory, in order to get sub-level headings.
	     ((and (memq type org-element-greater-elements)
		   (or (memq granularity '(element object nil))
		       (and (eq granularity 'greater-element)
			    (eq type 'section))
		       (eq type 'headline)))
	      (org-element--parse-elements
	       cbeg (org-element-property :contents-end element)
	       ;; Possibly switch to a special mode.
	       (org-element--next-mode mode type t)
	       (and (memq type '(item plain-list))
		    (org-element-property :structure element))
	       granularity visible-only element))
	     ;; ELEMENT has contents.  Parse objects inside, if
	     ;; GRANULARITY allows it.
	     ((memq granularity '(object nil))
	      (org-element--parse-objects
	       cbeg (org-element-property :contents-end element) element
	       (org-element-restriction type))))
	    (push (org-element-put-property element :parent acc) elements)
	    ;; Update mode.
	    (setq mode (org-element--next-mode mode type nil)))))
      ;; Return result.
      (org-element-put-property acc :granularity granularity)
      (apply #'org-element-set-contents acc (nreverse elements)))))

(defun org-element--object-lex (restriction)
  "Return next object in current buffer or nil.
RESTRICTION is a list of object types, as symbols, that should be
looked after.  This function assumes that the buffer is narrowed
to an appropriate container (e.g., a paragraph)."
  (cond
   ((memq 'table-cell restriction) (org-element-table-cell-parser))
   ((memq 'citation-reference restriction)
    (org-element-citation-reference-parser))
   (t
    (let* ((start (point))
	   (limit
	    ;; Object regexp sometimes needs to have a peek at
	    ;; a character ahead.  Therefore, when there is a hard
	    ;; limit, make it one more than the true beginning of the
	    ;; radio target.
	    (save-excursion
	      (cond ((not org-target-link-regexp) nil)
		    ((not (memq 'link restriction)) nil)
		    ((progn
		       (unless (bolp) (forward-char -1))
		       (not (re-search-forward org-target-link-regexp nil t)))
		     nil)
		    ;; Since we moved backward, we do not want to
		    ;; match again an hypothetical 1-character long
		    ;; radio link before us.  Realizing that this can
		    ;; only happen if such a radio link starts at
		    ;; beginning of line, we prevent this here.
		    ((and (= start (1+ (line-beginning-position)))
			  (= start (match-end 1)))
		     (and (re-search-forward org-target-link-regexp nil t)
			  (1+ (match-beginning 1))))
		    (t (1+ (match-beginning 1))))))
	   found)
      (save-excursion
	(while (and (not found)
		    (re-search-forward org-element--object-regexp limit 'move))
	  (goto-char (match-beginning 0))
	  (let ((result (match-string 0)))
	    (setq found
		  (cond
		   ((string-prefix-p "call_" result t)
		    (and (memq 'inline-babel-call restriction)
			 (org-element-inline-babel-call-parser)))
		   ((string-prefix-p "src_" result t)
		    (and (memq 'inline-src-block restriction)
			 (org-element-inline-src-block-parser)))
		   (t
		    (pcase (char-after)
		      (?^ (and (memq 'superscript restriction)
			       (org-element-superscript-parser)))
		      (?_ (or (and (memq 'subscript restriction)
				   (org-element-subscript-parser))
			      (and (memq 'underline restriction)
				   (org-element-underline-parser))))
		      (?* (and (memq 'bold restriction)
			       (org-element-bold-parser)))
		      (?/ (and (memq 'italic restriction)
			       (org-element-italic-parser)))
		      (?~ (and (memq 'code restriction)
			       (org-element-code-parser)))
		      (?= (and (memq 'verbatim restriction)
			       (org-element-verbatim-parser)))
		      (?+ (and (memq 'strike-through restriction)
			       (org-element-strike-through-parser)))
		      (?@ (and (memq 'export-snippet restriction)
			       (org-element-export-snippet-parser)))
		      (?{ (and (memq 'macro restriction)
			       (org-element-macro-parser)))
		      (?$ (and (memq 'latex-fragment restriction)
			       (org-element-latex-fragment-parser)))
		      (?<
		       (if (eq (aref result 1) ?<)
			   (or (and (memq 'radio-target restriction)
				    (org-element-radio-target-parser))
			       (and (memq 'target restriction)
				    (org-element-target-parser)))
			 (or (and (memq 'timestamp restriction)
				  (org-element-timestamp-parser))
			     (and (memq 'link restriction)
				  (org-element-link-parser)))))
		      (?\\
		       (if (eq (aref result 1) ?\\)
			   (and (memq 'line-break restriction)
				(org-element-line-break-parser))
			 (or (and (memq 'entity restriction)
				  (org-element-entity-parser))
			     (and (memq 'latex-fragment restriction)
				  (org-element-latex-fragment-parser)))))
		      (?\[
		       (pcase (aref result 1)
			 ((and ?\[
			       (guard (memq 'link restriction)))
			  (org-element-link-parser))
			 ((and ?f
			       (guard (memq 'footnote-reference restriction)))
			  (org-element-footnote-reference-parser))
			 ((and ?c
			       (guard (memq 'citation restriction)))
			  (org-element-citation-parser))
			 ((and (or ?% ?/)
			       (guard (memq 'statistics-cookie restriction)))
			  (org-element-statistics-cookie-parser))
			 (_
			  (or (and (memq 'timestamp restriction)
				   (org-element-timestamp-parser))
			      (and (memq 'statistics-cookie restriction)
				   (org-element-statistics-cookie-parser))))))
		      ;; This is probably a plain link.
		      (_ (and (memq 'link restriction)
			      (org-element-link-parser)))))))
	    (or (eobp) (forward-char))))
	(cond (found)
	      (limit (forward-char -1)
		     (org-element-link-parser))	;radio link
	      (t nil)))))))

(defun org-element--parse-objects (beg end acc restriction &optional parent)
  "Parse objects between BEG and END and return recursive structure.

Objects are accumulated in ACC.  RESTRICTION is a list of object
successors which are allowed in the current object.

ACC becomes the parent for all parsed objects.  However, if ACC
is nil (i.e., a secondary string is being parsed) and optional
argument PARENT is non-nil, use it as the parent for all objects.
Eventually, if both ACC and PARENT are nil, the common parent is
the list of objects itself."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (next-object contents)
	(while (and (not (eobp))
		    (setq next-object (org-element--object-lex restriction)))
	  ;; Text before any object.
	  (let ((obj-beg (org-element-property :begin next-object)))
	    (unless (= (point) obj-beg)
	      (let ((text (buffer-substring-no-properties (point) obj-beg)))
		(push (if acc (org-element-put-property text :parent acc) text)
		      contents))))
	  ;; Object...
	  (let ((obj-end (org-element-property :end next-object))
		(cont-beg (org-element-property :contents-begin next-object)))
	    (when acc (org-element-put-property next-object :parent acc))
	    (push (if cont-beg
		      ;; Fill contents of NEXT-OBJECT if possible.
		      (org-element--parse-objects
		       cont-beg
		       (org-element-property :contents-end next-object)
		       next-object
		       (org-element-restriction next-object))
		    next-object)
		  contents)
	    (goto-char obj-end)))
	;; Text after last object.
	(unless (eobp)
	  (let ((text (buffer-substring-no-properties (point) end)))
	    (push (if acc (org-element-put-property text :parent acc) text)
		  contents)))
	;; Result.  Set appropriate parent.
	(if acc (apply #'org-element-set-contents acc (nreverse contents))
	  (let* ((contents (nreverse contents))
		 (parent (or parent contents)))
	    (dolist (datum contents contents)
	      (org-element-put-property datum :parent parent))))))))



;;; Towards A Bijective Process
;;
;; The parse tree obtained with `org-element-parse-buffer' is really
;; a snapshot of the corresponding Org buffer.  Therefore, it can be
;; interpreted and expanded into a string with canonical Org syntax.
;; Hence `org-element-interpret-data'.
;;
;; The function relies internally on
;; `org-element--interpret-affiliated-keywords'.

;;;###autoload
(defun org-element-interpret-data (data)
  "Interpret DATA as Org syntax.
DATA is a parse tree, an element, an object or a secondary string
to interpret.  Return Org syntax as a string."
  (letrec ((fun
	    (lambda (data parent)
	      (let* ((type (org-element-type data))
		     ;; Find interpreter for current object or
		     ;; element.  If it doesn't exist (e.g. this is
		     ;; a pseudo object or element), return contents,
		     ;; if any.
		     (interpret
		      (let ((fun (intern
				  (format "org-element-%s-interpreter" type))))
			(if (fboundp fun) fun (lambda (_ contents) contents))))
		     (results
		      (cond
		       ;; Secondary string.
		       ((not type)
			(mapconcat (lambda (obj) (funcall fun obj parent))
				   data
				   ""))
		       ;; Full Org document.
		       ((eq type 'org-data)
			(mapconcat (lambda (obj) (funcall fun obj parent))
				   (org-element-contents data)
				   ""))
		       ;; Plain text: return it.
		       ((stringp data) data)
		       ;; Element or object without contents.
		       ((not (org-element-contents data))
			(funcall interpret data nil))
		       ;; Element or object with contents.
		       (t
			(funcall
			 interpret
			 data
			 ;; Recursively interpret contents.
			 (mapconcat
			  (lambda (datum) (funcall fun datum data))
			  (org-element-contents
			   (if (not (memq type '(paragraph verse-block)))
			       data
			     ;; Fix indentation of elements containing
			     ;; objects.  We ignore `table-row'
			     ;; elements as they are one line long
			     ;; anyway.
			     (org-element-normalize-contents
			      data
			      ;; When normalizing first paragraph of
			      ;; an item or a footnote-definition,
			      ;; ignore first line's indentation.
			      (and (eq type 'paragraph)
				   (memq (org-element-type parent)
					 '(footnote-definition item))
				   (eq data (car (org-element-contents parent)))
				   (eq (org-element-property :pre-blank parent)
				       0)))))
			  ""))))))
		(if (memq type '(org-data nil)) results
		  ;; Build white spaces.  If no `:post-blank' property
		  ;; is specified, assume its value is 0.
		  (let ((blank (or (org-element-property :post-blank data) 0)))
		    (if (eq (org-element-class data parent) 'object)
			(concat results (make-string blank ?\s))
		      (concat (org-element--interpret-affiliated-keywords data)
			      (org-element-normalize-string results)
			      (make-string blank ?\n)))))))))
    (funcall fun data nil)))

(defun org-element--interpret-affiliated-keywords (element)
  "Return ELEMENT's affiliated keywords as Org syntax.
If there is no affiliated keyword, return the empty string."
  (let ((keyword-to-org
	 (lambda (key value)
	   (let (dual)
	     (when (member key org-element-dual-keywords)
	       (setq dual (cdr value) value (car value)))
	     (concat "#+" (downcase key)
		     (and dual
			  (format "[%s]" (org-element-interpret-data dual)))
		     ": "
		     (if (member key org-element-parsed-keywords)
			 (org-element-interpret-data value)
		       value)
		     "\n")))))
    (mapconcat
     (lambda (prop)
       (let ((value (org-element-property prop element))
	     (keyword (upcase (substring (symbol-name prop) 1))))
	 (when value
	   (if (or (member keyword org-element-multiple-keywords)
		   ;; All attribute keywords can have multiple lines.
		   (string-match "^ATTR_" keyword))
	       (mapconcat (lambda (line) (funcall keyword-to-org keyword line))
			  (reverse value)
			  "")
	     (funcall keyword-to-org keyword value)))))
     ;; List all ELEMENT's properties matching an attribute line or an
     ;; affiliated keyword, but ignore translated keywords since they
     ;; cannot belong to the property list.
     (cl-loop for prop in (nth 1 element) by 'cddr
	      when (let ((keyword (upcase (substring (symbol-name prop) 1))))
		     (or (string-match "^ATTR_" keyword)
			 (and
			  (member keyword org-element-affiliated-keywords)
			  (not (assoc keyword
				      org-element-keyword-translation-alist)))))
	      collect prop)
     "")))

;; Because interpretation of the parse tree must return the same
;; number of blank lines between elements and the same number of white
;; space after objects, some special care must be given to white
;; spaces.
;;
;; The first function, `org-element-normalize-string', ensures any
;; string different from the empty string will end with a single
;; newline character.
;;
;; The second function, `org-element-normalize-contents', removes
;; global indentation from the contents of the current element.

(defun org-element-normalize-string (s)
  "Ensure string S ends with a single newline character.

If S isn't a string return it unchanged.  If S is the empty
string, return it.  Otherwise, return a new string with a single
newline character at its end."
  (cond
   ((not (stringp s)) s)
   ((string= "" s) "")
   (t (and (string-match "\\(\n[ \t]*\\)*\\'" s)
	   (replace-match "\n" nil nil s)))))

(defun org-element-normalize-contents (element &optional ignore-first)
  "Normalize plain text in ELEMENT's contents.

ELEMENT must only contain plain text and objects.

If optional argument IGNORE-FIRST is non-nil, ignore first line's
indentation to compute maximal common indentation.

Return the normalized element that is element with global
indentation removed from its contents."
  (letrec ((find-min-ind
	    ;; Return minimal common indentation within BLOB.  This is
	    ;; done by walking recursively BLOB and updating MIN-IND
	    ;; along the way.  FIRST-FLAG is non-nil when the next
	    ;; object is expected to be a string that doesn't start
	    ;; with a newline character.  It happens for strings at
	    ;; the beginnings of the contents or right after a line
	    ;; break.
	    (lambda (blob first-flag min-ind)
	      (dolist (datum (org-element-contents blob) min-ind)
		(when first-flag
		  (setq first-flag nil)
		  (cond
		   ;; Objects cannot start with spaces: in this
		   ;; case, indentation is 0.
		   ((not (stringp datum)) (throw :zero 0))
		   ((not (string-match
			  "\\`\\([ \t]+\\)\\([^ \t\n]\\|\n\\|\\'\\)" datum))
		    (throw :zero 0))
		   ((equal (match-string 2 datum) "\n")
		    (put-text-property
		     (match-beginning 1) (match-end 1) 'org-ind 'empty datum))
		   (t
		    (let ((i (string-width (match-string 1 datum))))
		      (put-text-property
		       (match-beginning 1) (match-end 1) 'org-ind i datum)
		      (setq min-ind (min i min-ind))))))
		(cond
		 ((stringp datum)
		  (let ((s 0))
		    (while (string-match
			    "\n\\([ \t]*\\)\\([^ \t\n]\\|\n\\|\\'\\)" datum s)
		      (setq s (match-end 1))
		      (cond
		       ((equal (match-string 1 datum) "")
			(unless (member (match-string 2 datum) '("" "\n"))
			  (throw :zero 0)))
		       ((equal (match-string 2 datum) "\n")
			(put-text-property (match-beginning 1) (match-end 1)
					   'org-ind 'empty datum))
		       (t
			(let ((i (string-width (match-string 1 datum))))
			  (put-text-property (match-beginning 1) (match-end 1)
					     'org-ind i datum)
			  (setq min-ind (min i min-ind))))))))
		 ((eq (org-element-type datum) 'line-break)
		  (setq first-flag t))
		 ((memq (org-element-type datum) org-element-recursive-objects)
		  (setq min-ind
			(funcall find-min-ind datum first-flag min-ind)))))))
	   (min-ind
	    (catch :zero
	      (funcall find-min-ind
		       element (not ignore-first) most-positive-fixnum))))
    (if (or (zerop min-ind) (= min-ind most-positive-fixnum)) element
      ;; Build ELEMENT back, replacing each string with the same
      ;; string minus common indentation.
      (letrec ((build
		(lambda (datum)
		  ;; Return DATUM with all its strings indentation
		  ;; shortened from MIN-IND white spaces.
		  (setcdr
		   (cdr datum)
		   (mapcar
		    (lambda (object)
		      (cond
		       ((stringp object)
			(with-temp-buffer
			  (insert object)
			  (let ((s (point-min)))
			    (while (setq s (text-property-not-all
					    s (point-max) 'org-ind nil))
			      (goto-char s)
			      (let ((i (get-text-property s 'org-ind)))
				(delete-region s (progn
						   (skip-chars-forward " \t")
						   (point)))
				(when (integerp i) (indent-to (- i min-ind))))))
			  (buffer-string)))
		       ((memq (org-element-type object)
			      org-element-recursive-objects)
			(funcall build object))
		       (t object)))
		    (org-element-contents datum)))
		  datum)))
	(funcall build element)))))



;;; Cache
;;
;; Implement a caching mechanism for `org-element-at-point', `org-element-context', and for
;; fast mapping across Org elements in `org-element-cache-map', which see.
;;
;; When cache is enabled, the elements returned by `org-element-at-point' and
;; `org-element-context' are returned by reference.  Altering these elements will
;; also alter their cache representation.  The same is true for
;; elements passed to mapping function in `org-element-cache-map'.
;;
;; Public functions are: `org-element-cache-reset', `org-element-cache-refresh', and
;; `org-element-cache-map'.
;;
;; Cache can be controlled using `org-element-use-cache' and `org-element-cache-persistent'.
;;  `org-element-cache-sync-idle-time', `org-element-cache-sync-duration' and
;; `org-element-cache-sync-break' can be tweaked to control caching behavior.
;;
;; Internally, parsed elements are stored in an AVL tree,
;; `org-element--cache'.  This tree is updated lazily: whenever
;; a change happens to the buffer, a synchronization request is
;; registered in `org-element--cache-sync-requests' (see
;; `org-element--cache-submit-request').  During idle time, requests
;; are processed by `org-element--cache-sync'.  Synchronization also
;; happens when an element is required from the cache.  In this case,
;; the process stops as soon as the needed element is up-to-date.
;;
;; A synchronization request can only apply on a synchronized part of
;; the cache.  Therefore, the cache is updated at least to the
;; location where the new request applies.  Thus, requests are ordered
;; from left to right and all elements starting before the first
;; request are correct.  This property is used by functions like
;; `org-element--cache-find' to retrieve elements in the part of the
;; cache that can be trusted.
;;
;; A request applies to every element, starting from its original
;; location (or key, see below).  When a request is processed, it
;; moves forward and may collide the next one.  In this case, both
;; requests are merged into a new one that starts from that element.
;; As a consequence, the whole synchronization complexity does not
;; depend on the number of pending requests, but on the number of
;; elements the very first request will be applied on.
;;
;; Elements cannot be accessed through their beginning position, which
;; may or may not be up-to-date.  Instead, each element in the tree is
;; associated to a key, obtained with `org-element--cache-key'.  This
;; mechanism is robust enough to preserve total order among elements
;; even when the tree is only partially synchronized.
;;
;; The cache code debugging is fairly complex because cache request
;; state is often hard to reproduce.  An extensive diagnostics
;; functionality is built into the cache code to assist hunting bugs.
;; See `org-element--cache-self-verify', `org-element--cache-self-verify-frequency',
;; `org-element--cache-diagnostics', `org-element--cache-diagnostics-level',
;; `org-element--cache-diagnostics-ring-size', `org-element--cache-map-statistics',
;; `org-element--cache-map-statistics-threshold'.

;;;###autoload
(defvar org-element-use-cache t
  "Non-nil when Org parser should cache its results.")

(defvar org-element-cache-persistent t
  "Non-nil when cache should persist between Emacs sessions.")

(defvar org-element-cache-sync-idle-time 0.6
  "Length, in seconds, of idle time before syncing cache.")

(defvar org-element-cache-sync-duration 0.04
  "Maximum duration, as a time value, for a cache synchronization.
If the synchronization is not over after this delay, the process
pauses and resumes after `org-element-cache-sync-break'
seconds.")

(defvar org-element-cache-sync-break 0.3
  "Duration, as a time value, of the pause between synchronizations.
See `org-element-cache-sync-duration' for more information.")

(defvar org-element--cache-self-verify nil
  "Activate extra consistency checks for the cache.

This may cause serious performance degradation depending on the value
of `org-element--cache-self-verify-frequency'.

When set to symbol `backtrace', record and display backtrace log if
any inconsistency is detected.")

(defvar org-element--cache-self-verify-frequency 0.03
  "Frequency of cache element verification.

This number is a probability to check an element requested from cache
to be correct.  Setting this to a value less than 0.0001 is useless.")

(defvar org-element--cache-diagnostics nil
  "Print detailed diagnostics of cache processing.")

(defvar org-element--cache-map-statistics nil
  "Print statistics for `org-element-cache-map'.")

(defvar org-element--cache-map-statistics-threshold 0.1
  "Time threshold in seconds to log statistics for `org-element-cache-map'.")

(defvar org-element--cache-diagnostics-level 2
  "Detail level of the diagnostics.")

(defvar-local org-element--cache-diagnostics-ring nil
  "Ring containing last `org-element--cache-diagnostics-ring-size'
cache process log entries.")

(defvar org-element--cache-diagnostics-ring-size 5000
  "Size of `org-element--cache-diagnostics-ring'.")

;;;; Data Structure

(defvar-local org-element--cache nil
  "AVL tree used to cache elements.
Each node of the tree contains an element.  Comparison is done
with `org-element--cache-compare'.  This cache is used in
`org-element-at-point'.")

(defvar-local org-element--headline-cache nil
  "AVL tree used to cache headline and inlinetask elements.
Each node of the tree contains an element.  Comparison is done
with `org-element--cache-compare'.  This cache is used in
`org-element-cache-map'.")

(defconst org-element--cache-hash-size 16
  "Cache size for recent cached calls to `org-element--cache-find'.

This extra caching is based on the following paper:
Pugh [Information Processing Letters] (1990) Slow optimally balanced
 search strategies vs. cached fast uniformly balanced search
 strategies.  http://dx.doi.org/10.1016/0020-0190(90)90130-P

Also, see `org-element--cache-hash-left' and `org-element--cache-hash-right'.")
(defvar-local org-element--cache-hash-left nil
  "Cached elements from `org-element--cache' for fast O(1) lookup.
When non-nil, it should be a vector representing POS arguments of
`org-element--cache-find' called with nil SIDE argument.
Also, see `org-element--cache-hash-size'.")
(defvar-local org-element--cache-hash-right nil
  "Cached elements from `org-element--cache' for fast O(1) lookup.
When non-nil, it should be a vector representing POS arguments of
`org-element--cache-find' called with non-nil, non-`both' SIDE argument.
Also, see `org-element--cache-hash-size'.")

(defvar org-element--cache-hash-statistics '(0 . 0)
  "Cons cell storing how Org makes use of `org-element--cache-find' caching.
The car is the number of successful uses and cdr is the total calls to
`org-element--cache-find'.")
(defvar org-element--cache-hash-nocache 0
  "Number of calls to `org-element--cache-has' with `both' SIDE argument.
These calls are not cached by hash.  See `org-element--cache-hash-size'.")

(defvar-local org-element--cache-size 0
  "Size of the `org-element--cache'.

Storing value is variable is faster because `avl-tree-size' is O(N).")

(defvar-local org-element--headline-cache-size 0
  "Size of the `org-element--headline-cache'.

Storing value is variable is faster because `avl-tree-size' is O(N).")

(defvar-local org-element--cache-sync-requests nil
  "List of pending synchronization requests.

A request is a vector with the following pattern:

 [NEXT BEG END OFFSET PARENT PHASE]

Processing a synchronization request consists of three phases:

  0. Delete modified elements,
  1. Fill missing area in cache,
  2. Shift positions and re-parent elements after the changes.

During phase 0, NEXT is the key of the first element to be
removed, BEG and END is buffer position delimiting the
modifications.  Elements starting between them (inclusive) are
removed.  So are elements whose parent is removed.  PARENT, when
non-nil, is the common parent of all the elements between BEG and END.

It is guaranteed that only a single phase 0 request exists at any
moment of time.  If it does, it must be the first request in the list.

During phase 1, NEXT is the key of the next known element in
cache and BEG its beginning position.  Parse buffer between that
element and the one before it in order to determine the parent of
the next element.  Set PARENT to the element containing NEXT.

During phase 2, NEXT is the key of the next element to shift in
the parse tree.  All elements starting from this one have their
properties relative to buffer positions shifted by integer
OFFSET and, if they belong to element PARENT, are adopted by it.

PHASE specifies the phase number, as an integer.

For any synchronization request, all the later requests in the cache
must not start at or before END.  See `org-element--cache-submit-request'.")

(defvar-local org-element--cache-sync-timer nil
  "Timer used for cache synchronization.")

(defvar-local org-element--cache-sync-keys-value nil
  "Id value used to identify keys during synchronization.
See `org-element--cache-key' for more information.")

(defvar-local org-element--cache-change-tic nil
  "Last `buffer-chars-modified-tick' for registered changes.")

(defvar-local org-element--cache-last-buffer-size nil
  "Last value of `buffer-size' for registered changes.")

(defvar org-element--cache-non-modifying-commands
  '(org-agenda
    org-agenda-redo
    org-sparse-tree
    org-occur
    org-columns
    org-columns-redo
    org-columns-new
    org-columns-delete
    org-columns-compute
    org-columns-insert-dblock
    org-agenda-columns
    org-ctrl-c-ctrl-c)
  "List of commands that are not expected to change the cache state.

This variable is used to determine when re-parsing buffer is not going
to slow down the command.

If the commands end up modifying the cache, the worst case scenario is
performance drop.  So, advicing these commands is safe.  Yet, it is
better to remove the commands advised in such a way from this list.")

(defmacro org-element--request-key (request)
  "Get NEXT part of a `org-element--cache-sync-requests' REQUEST."
  `(aref ,request 0))

(defmacro org-element--request-beg (request)
  "Get BEG part of a `org-element--cache-sync-requests' REQUEST."
  `(aref ,request 1))

(defmacro org-element--request-end (request)
  "Get END part of a `org-element--cache-sync-requests' REQUEST."
  `(aref ,request 2))

(defmacro org-element--request-offset (request)
  "Get OFFSET part of a `org-element--cache-sync-requests' REQUEST."
  `(aref ,request 3))

(defmacro org-element--request-parent (request)
  "Get PARENT part of a `org-element--cache-sync-requests' REQUEST."
  `(aref ,request 4))

(defmacro org-element--request-phase (request)
  "Get PHASE part of a `org-element--cache-sync-requests' REQUEST."
  `(aref ,request 5))

(defmacro org-element--format-element (element)
  "Format ELEMENT for printing in diagnostics."
  `(let ((print-length 50)
         (print-level 5))
     (prin1-to-string ,element)))

(defmacro org-element--cache-log-message (format-string &rest args)
  "Add a new log message for org-element-cache."
  `(when (or org-element--cache-diagnostics
             (eq org-element--cache-self-verify 'backtrace))
     (let* ((format-string (concat (format "org-element-cache diagnostics(%s): "
                                           (buffer-name (current-buffer)))
                                   ,format-string))
            (format-string (funcall #'format format-string ,@args)))
       (if org-element--cache-diagnostics
           (display-warning 'org-element-cache format-string)
         (unless org-element--cache-diagnostics-ring
           (setq org-element--cache-diagnostics-ring
                 (make-ring org-element--cache-diagnostics-ring-size)))
         (ring-insert org-element--cache-diagnostics-ring format-string)))))

(defmacro org-element--cache-warn (format-string &rest args)
  "Raise warning for org-element-cache."
  `(let* ((format-string (funcall #'format ,format-string ,@args))
          (format-string
           (if (or (not org-element--cache-diagnostics-ring)
                   (not (eq 'backtrace org-element--cache-self-verify)))
               format-string
             (prog1
                 (concat (format "Warning(%s): "
                                 (buffer-name (current-buffer)))
                         format-string
                         "\nBacktrace:\n  "
                         (mapconcat #'identity
                                    (ring-elements org-element--cache-diagnostics-ring)
                                    "\n  "))
               (setq org-element--cache-diagnostics-ring nil)))))
     (if (and (boundp 'org-batch-test) org-batch-test)
         (error "%s" (concat "org-element--cache: " format-string))
       (display-warning 'org-element-cache
                        (concat "org-element--cache: " format-string)))))

(defsubst org-element--cache-key (element)
  "Return a unique key for ELEMENT in cache tree.

Keys are used to keep a total order among elements in the cache.
Comparison is done with `org-element--cache-key-less-p'.

When no synchronization is taking place, a key is simply the
beginning position of the element, or that position plus one in
the case of an first item (respectively row) in
a list (respectively a table).  They key of a section is its beginning
position minus one.

During a synchronization, the key is the one the element had when
the cache was synchronized for the last time.  Elements added to
cache during the synchronization get a new key generated with
`org-element--cache-generate-key'.

Such keys are stored inside the element property
`:org-element--cache-sync-key'.  The property is a cons containing
current `org-element--cache-sync-keys-value' and the element key."
  (or (when (eq org-element--cache-sync-keys-value (car (org-element-property :org-element--cache-sync-key element)))
        (cdr (org-element-property :org-element--cache-sync-key element)))
      (let* ((begin (org-element-property :begin element))
	     ;; Increase beginning position of items (respectively
	     ;; table rows) by one, so the first item can get
	     ;; a different key from its parent list (respectively
	     ;; table).
	     (key (if (memq (org-element-type element) '(item table-row))
		      (1+ begin)
                    ;; Decrease beginning position of sections by one,
                    ;; so that the first element of the section get
                    ;; different key from the parent section.
                    (if (eq (org-element-type element) 'section)
                        (1- begin)
                      (if (eq (org-element-type element) 'org-data)
                          (- begin 2)
		        begin)))))
        (when org-element--cache-sync-requests
	  (org-element-put-property
           element
           :org-element--cache-sync-key
           (cons org-element--cache-sync-keys-value key)))
        key)))

(defun org-element--cache-generate-key (lower upper)
  "Generate a key between LOWER and UPPER.

LOWER and UPPER are fixnums or lists of same, possibly empty.

If LOWER and UPPER are equals, return LOWER.  Otherwise, return
a unique key, as an integer or a list of integers, according to
the following rules:

  - LOWER and UPPER are compared level-wise until values differ.

  - If, at a given level, LOWER and UPPER differ from more than
    2, the new key shares all the levels above with LOWER and
    gets a new level.  Its value is the mean between LOWER and
    UPPER:

      (1 2) + (1 4) --> (1 3)

  - If LOWER has no value to compare with, it is assumed that its
    value is `most-negative-fixnum'.  E.g.,

      (1 1) + (1 1 2)

    is equivalent to

      (1 1 m) + (1 1 2)

    where m is `most-negative-fixnum'.  Likewise, if UPPER is
    short of levels, the current value is `most-positive-fixnum'.

  - If they differ from only one, the new key inherits from
    current LOWER level and fork it at the next level.  E.g.,

      (2 1) + (3 3)

    is equivalent to

      (2 1) + (2 M)

    where M is `most-positive-fixnum'.

  - If the key is only one level long, it is returned as an
    integer:

      (1 2) + (3 2) --> 2

When they are not equals, the function assumes that LOWER is
lesser than UPPER, per `org-element--cache-key-less-p'."
  (if (equal lower upper) lower
    (let ((lower (if (integerp lower) (list lower) lower))
	  (upper (if (integerp upper) (list upper) upper))
          skip-upper key)
      (catch 'exit
	(while t
	  (let ((min (or (car lower) most-negative-fixnum))
		(max (cond (skip-upper most-positive-fixnum)
                           ((car upper))
                           (t most-positive-fixnum))))
            (if (< (1+ min) max)
		(let ((mean (+ (ash min -1) (ash max -1) (logand min max 1))))
		  (throw 'exit (if key (nreverse (cons mean key)) mean)))
	      (when (and (< min max) (not skip-upper))
		;; When at a given level, LOWER and UPPER differ from
		;; 1, ignore UPPER altogether.  Instead create a key
		;; between LOWER and the greatest key with the same
		;; prefix as LOWER so far.
		(setq skip-upper t))
	      (push min key)
	      (setq lower (cdr lower) upper (cdr upper)))))))))

(defsubst org-element--cache-key-less-p (a b)
  "Non-nil if key A is less than key B.
A and B are either integers or lists of integers, as returned by
`org-element--cache-key'.

Note that it is not reliable to compare buffer position with the cache
keys.  They keys may be larger compared to actual element :begin
position."
  (if (integerp a) (if (integerp b) (< a b) (<= a (car b)))
    (if (integerp b) (< (car a) b)
      (catch 'exit
	(while (and a b)
	  (cond ((car-less-than-car a b) (throw 'exit t))
		((car-less-than-car b a) (throw 'exit nil))
		(t (setq a (cdr a) b (cdr b)))))
	;; If A is empty, either keys are equal (B is also empty) and
	;; we return nil, or A is lesser than B (B is longer) and we
	;; return a non-nil value.
	;;
	;; If A is not empty, B is necessarily empty and A is greater
	;; than B (A is longer).  Therefore, return nil.
	(and (null a) b)))))

(defun org-element--cache-compare (a b)
  "Non-nil when element A is located before element B."
  (org-element--cache-key-less-p (org-element--cache-key a) (org-element--cache-key b)))

(defsubst org-element--cache-root ()
  "Return root value in `org-element--cache' .
This function assumes `org-element--cache' is a valid AVL tree."
  (avl-tree--node-left (avl-tree--dummyroot org-element--cache)))

(defsubst org-element--headline-cache-root ()
  "Return root value in `org-element--headline-cache' .
This function assumes `org-element--headline-cache' is a valid AVL tree."
  (avl-tree--node-left (avl-tree--dummyroot org-element--headline-cache)))

;;;; Tools

;; FIXME: Ideally, this should be inlined to avoid overheads, but
;; inlined functions should be declared before the code that uses them
;; and some code above does use `org-element--cache-active-p'.  Moving this
;; declaration on top would require restructuring the whole cache
;; section.
(defun org-element--cache-active-p (&optional called-from-cache-change-func-p)
  "Non-nil when cache is active in current buffer."
  (org-with-base-buffer nil
    (and org-element-use-cache
         org-element--cache
         (or called-from-cache-change-func-p
             (eq org-element--cache-change-tic (buffer-chars-modified-tick))
             (and
              ;; org-num-mode calls some Org structure analysis functions
              ;; that can trigger cache update in the middle of changes.  See
              ;; `org-num--verify' calling `org-num--skip-value' calling
              ;; `org-entry-get' that uses cache.
              ;; Forcefully disable cache when called from inside a
              ;; modification hook, where `inhibit-modification-hooks' is set
              ;; to t.
              (not inhibit-modification-hooks)
              ;; `combine-change-calls' sets `after-change-functions' to
              ;; nil.  We need not to use cache inside
              ;; `combine-change-calls' because the buffer is potentially
              ;; changed without notice (the change will be registered
              ;; after exiting the `combine-change-calls' body though).
              (catch :inhibited
                (org-fold-core-cycle-over-indirect-buffers
                  (unless (memq #'org-element--cache-after-change after-change-functions)
                    (throw :inhibited nil)))
                t))))))

;; FIXME: Remove after we establish that hashing is effective.
(defun org-element-cache-hash-show-statistics ()
  "Display efficiency of O(1) query cache for `org-element--cache-find'.

This extra caching is based on the following paper:
Pugh [Information Processing Letters] (1990) Slow optimally balanced
 search strategies vs. cached fast uniformly balanced search
 strategies.  http://dx.doi.org/10.1016/0020-0190(90)90130-P

Also, see `org-element--cache-size'."
  (interactive)
  (message "%.2f%% of cache searches hashed, %.2f%% non-hashable."
	   (* 100
	      (/ (float (car org-element--cache-hash-statistics))
		 (cdr org-element--cache-hash-statistics)))
	   (* 100
	      (/ (float org-element--cache-hash-nocache)
		 (cdr org-element--cache-hash-statistics)))))

(defun org-element--cache-find (pos &optional side)
  "Find element in cache starting at POS or before.

POS refers to a buffer position.

When optional argument SIDE is non-nil, the function checks for
elements starting at or past POS instead.  If SIDE is `both', the
function returns a cons cell where car is the first element
starting at or before POS and cdr the first element starting
after POS.

The function can only find elements in the synchronized part of
the cache."
  (org-with-base-buffer nil
    (let* ((limit (and org-element--cache-sync-requests
                       (org-element--request-key (car org-element--cache-sync-requests))))
	   (node (org-element--cache-root))
           (hash-pos (unless (eq side 'both)
                       (mod (org-knuth-hash pos)
                            org-element--cache-hash-size)))
           (hashed (if (not side)
                       (aref org-element--cache-hash-left hash-pos)
                     (unless (eq side 'both)
                       (aref org-element--cache-hash-right hash-pos))))
	   lower upper)
      ;; `org-element--cache-key-less-p' does not accept markers.
      (when (markerp pos) (setq pos (marker-position pos)))
      (cl-incf (cdr org-element--cache-hash-statistics))
      (when (eq side 'both) (cl-incf org-element--cache-hash-nocache))
      (if (and hashed (not (eq side 'both))
               (or (not limit)
                   ;; Limit can be a list key.
                   (org-element--cache-key-less-p
                    (org-element--cache-key hashed)
                    limit))
               (= pos (org-element-property :begin hashed))
               ;; We cannot rely on element :begin for elements with
               ;; children starting at the same pos.
               (not (memq (org-element-type hashed)
                        '(section org-data table)))
               (org-element-property :cached hashed))
          (progn
            (cl-incf (car org-element--cache-hash-statistics))
            hashed)
        (while node
          (let* ((element (avl-tree--node-data node))
	         (begin (org-element-property :begin element)))
	    (cond
	     ((and limit
	           (not (org-element--cache-key-less-p
	               (org-element--cache-key element) limit)))
	      (setq node (avl-tree--node-left node)))
	     ((> begin pos)
	      (setq upper element
		    node (avl-tree--node-left node)))
	     ((or (< begin pos)
                  ;; If the element is section or org-data, we also need
                  ;; to check the following element.
                  (memq (org-element-type element) '(section org-data)))
	      (setq lower element
		    node (avl-tree--node-right node)))
	     ;; We found an element in cache starting at POS.  If `side'
	     ;; is `both' we also want the next one in order to generate
	     ;; a key in-between.
	     ;;
	     ;; If the element is the first row or item in a table or
	     ;; a plain list, we always return the table or the plain
	     ;; list.
	     ;;
	     ;; In any other case, we return the element found.
	     ((eq side 'both)
	      (setq lower element)
	      (setq node (avl-tree--node-right node)))
	     ((and (memq (org-element-type element) '(item table-row))
	           (let ((parent (org-element-property :parent element)))
		     (and (= (org-element-property :begin element)
			     (org-element-property :contents-begin parent))
		          (setq node nil
			        lower parent
			        upper parent)))))
	     (t
	      (setq node nil
		    lower element
		    upper element)))))
        (if (not side)
            (aset org-element--cache-hash-left hash-pos lower)
          (unless (eq side 'both)
            (aset org-element--cache-hash-right hash-pos lower)))
        (pcase side
          (`both (cons lower upper))
          (`nil lower)
          (_ upper))))))

(defun org-element--cache-put (element)
  "Store ELEMENT in current buffer's cache, if allowed."
  (org-with-base-buffer nil
    (when (org-element--cache-active-p)
      (when org-element--cache-sync-requests
        ;; During synchronization, first build an appropriate key for
        ;; the new element so `avl-tree-enter' can insert it at the
        ;; right spot in the cache.
        (let* ((keys (org-element--cache-find
		      (org-element-property :begin element) 'both))
               (new-key (org-element--cache-generate-key
		         (and (car keys) (org-element--cache-key (car keys)))
		         (cond ((cdr keys) (org-element--cache-key (cdr keys)))
			       (org-element--cache-sync-requests
			        (org-element--request-key (car org-element--cache-sync-requests)))))))
          (org-element-put-property
           element
           :org-element--cache-sync-key
           (cons org-element--cache-sync-keys-value new-key))))
      (when (>= org-element--cache-diagnostics-level 2)
        (org-element--cache-log-message
         "Added new element with %S key: %S"
         (org-element-property :org-element--cache-sync-key element)
         (org-element--format-element element)))
      (org-element-put-property element :cached t)
      (when (memq (org-element-type element) '(headline inlinetask))
        (cl-incf org-element--headline-cache-size)
        (avl-tree-enter org-element--headline-cache element))
      (cl-incf org-element--cache-size)
      (avl-tree-enter org-element--cache element))))

(defsubst org-element--cache-remove (element)
  "Remove ELEMENT from cache.
Assume ELEMENT belongs to cache and that a cache is active."
  (org-with-base-buffer nil
    (org-element-put-property element :cached nil)
    (cl-decf org-element--cache-size)
    ;; Invalidate contents of parent.
    (when (and (org-element-property :parent element)
               (org-element-contents (org-element-property :parent element)))
      (org-element-set-contents (org-element-property :parent element) nil))
    (when (memq (org-element-type element) '(headline inlinetask))
      (cl-decf org-element--headline-cache-size)
      (avl-tree-delete org-element--headline-cache element))
    (org-element--cache-log-message
     "Decreasing cache size to %S"
     org-element--cache-size)
    (when (< org-element--cache-size 0)
      (org-element--cache-warn
       "Cache grew to negative size in %S when deleting %S at %S.  Cache key: %S.
If this warning appears regularly, please report the warning text to Org mode mailing list (M-x org-submit-bug-report)."
       (org-element-type element)
       (current-buffer)
       (org-element-property :begin element)
       (org-element-property :org-element--cache-sync-key element))
      (org-element-cache-reset)
      (throw 'quit nil))
    (or (avl-tree-delete org-element--cache element)
        (progn
          ;; This should not happen, but if it is, would be better to know
          ;; where it happens.
          (org-element--cache-warn
           "Failed to delete %S element in %S at %S. The element cache key was %S.
If this warning appears regularly, please report the warning text to Org mode mailing list (M-x org-submit-bug-report)."
           (org-element-type element)
           (current-buffer)
           (org-element-property :begin element)
           (org-element-property :org-element--cache-sync-key element))
          (org-element-cache-reset)
          (throw 'quit nil)))))

;;;; Synchronization

(defsubst org-element--cache-set-timer (buffer)
  "Set idle timer for cache synchronization in BUFFER."
  (when org-element--cache-sync-timer
    (cancel-timer org-element--cache-sync-timer))
  (setq org-element--cache-sync-timer
	(run-with-idle-timer
	 (let ((idle (current-idle-time)))
	   (if idle (time-add idle org-element-cache-sync-break)
	     org-element-cache-sync-idle-time))
	 nil
	 #'org-element--cache-sync
	 buffer)))

(defsubst org-element--cache-interrupt-p (time-limit)
  "Non-nil when synchronization process should be interrupted.
TIME-LIMIT is a time value or nil."
  (and time-limit
       (or (input-pending-p)
	   (time-less-p time-limit nil))))

(defsubst org-element--cache-shift-positions (element offset &optional props)
  "Shift ELEMENT properties relative to buffer positions by OFFSET.

Properties containing buffer positions are `:begin', `:end',
`:contents-begin', `:contents-end' and `:structure'.  When
optional argument PROPS is a list of keywords, only shift
properties provided in that list.

Properties are modified by side-effect."
  (let ((properties (nth 1 element)))
    ;; Shift `:structure' property for the first plain list only: it
    ;; is the only one that really matters and it prevents from
    ;; shifting it more than once.
    (when (and (or (not props) (memq :structure props))
	       (eq (org-element-type element) 'plain-list)
	       (not (eq (org-element-type (plist-get properties :parent)) 'item)))
      (dolist (item (plist-get properties :structure))
	(cl-incf (car item) offset)
	(cl-incf (nth 6 item) offset)))
    (dolist (key '( :begin :contents-begin :contents-end :end
                    :post-affiliated :robust-begin :robust-end))
      (let ((value (and (or (not props) (memq key props))
			(plist-get properties key))))
	(and value (plist-put properties key (+ offset value)))))))

(defvar org-element--cache-interrupt-C-g t
  "When non-nil, allow the user to abort `org-element--cache-sync'.
The execution is aborted upon pressing `\\[keyboard-quit]'
`org-element--cache-interrupt-C-g-max-count' times.")
(defvar org-element--cache-interrupt-C-g-max-count 5
  "`\\[keyboard-quit]' count to interrupt `org-element--cache-sync'.
See `org-element--cache-interrupt-C-g'.")
(defvar org-element--cache-interrupt-C-g-count 0
  "Current number of `org-element--cache-sync' calls.
See `org-element--cache-interrupt-C-g'.")

(defvar org-element--cache-change-warning nil
  "Non-nil when a sensitive line is about to be changed.
It is a symbol among nil, t, or a number representing smallest level of
modified headline.  The level considers headline levels both before
and after the modification.")

(defun org-element--cache-sync (buffer &optional threshold future-change offset)
  "Synchronize cache with recent modification in BUFFER.

When optional argument THRESHOLD is non-nil, do the
synchronization for all elements starting before or at threshold,
then exit.  Otherwise, synchronize cache for as long as
`org-element-cache-sync-duration' or until Emacs leaves idle
state.

FUTURE-CHANGE, when non-nil, is a buffer position where changes
not registered yet in the cache are going to happen.  OFFSET is the
change offset.  It is used in `org-element--cache-submit-request',
where cache is partially updated before current modification are
actually submitted."
  (when (buffer-live-p buffer)
    (org-with-base-buffer buffer
      ;; Do not sync when, for example, in the middle of
      ;; `combine-change-calls'.  See the commentary inside
      ;; `org-element--cache-active-p'.
      (when (and org-element--cache-sync-requests (org-element--cache-active-p))
        ;; Check if the buffer have been changed outside visibility of
        ;; `org-element--cache-before-change' and `org-element--cache-after-change'.
        (if (/= org-element--cache-last-buffer-size (buffer-size))
            (progn
              (org-element--cache-warn
               "Unregistered buffer modifications detected (%S != %S). Resetting.
If this warning appears regularly, please report the warning text to Org mode mailing list (M-x org-submit-bug-report).
The buffer is: %s\n Current command: %S\n Backtrace:\n%S"
               org-element--cache-last-buffer-size
               (buffer-size)
               (buffer-name (current-buffer))
               this-command
               (when (and (fboundp 'backtrace-get-frames)
                          (fboundp 'backtrace-to-string))
                 (backtrace-to-string (backtrace-get-frames 'backtrace))))
              (org-element-cache-reset))
          (let ((inhibit-quit t) request next)
            (setq org-element--cache-interrupt-C-g-count 0)
	    (when org-element--cache-sync-timer
	      (cancel-timer org-element--cache-sync-timer))
            (let ((time-limit (time-add nil org-element-cache-sync-duration)))
	      (catch 'org-element--cache-interrupt
                (when org-element--cache-sync-requests
                  (org-element--cache-log-message "Syncing down to %S-%S" (or future-change threshold) threshold))
	        (while org-element--cache-sync-requests
	          (setq request (car org-element--cache-sync-requests)
		        next (nth 1 org-element--cache-sync-requests))
	          (org-element--cache-process-request
	           request
	           (when next (org-element--request-key next))
	           threshold
	           (unless threshold time-limit)
	           future-change
                   offset)
                  ;; Re-assign current and next requests.  It could have
                  ;; been altered during phase 1.
                  (setq request (car org-element--cache-sync-requests)
		        next (nth 1 org-element--cache-sync-requests))
	          ;; Request processed.  Merge current and next offsets and
	          ;; transfer ending position.
	          (when next
                    ;; The following requests can only be either phase 1
                    ;; or phase 2 requests.  We need to let them know
                    ;; that additional shifting happened ahead of them.
	            (cl-incf (org-element--request-offset next) (org-element--request-offset request))
                    (org-element--cache-log-message
                     "Updating next request offset to %S: %s"
                     (org-element--request-offset next)
                     (let ((print-length 10) (print-level 3)) (prin1-to-string next)))
                    ;; FIXME: END part of the request only matters for
                    ;; phase 0 requests.  However, the only possible
                    ;; phase 0 request must be the first request in the
                    ;; list all the time.  END position should be
                    ;; unused.
                    (setf (org-element--request-end next) (org-element--request-end request)))
	          (setq org-element--cache-sync-requests
		        (cdr org-element--cache-sync-requests)))))
	    ;; If more requests are awaiting, set idle timer accordingly.
	    ;; Otherwise, reset keys.
	    (if org-element--cache-sync-requests
	        (org-element--cache-set-timer buffer)
              (setq org-element--cache-change-warning nil)
              (setq org-element--cache-sync-keys-value (1+ org-element--cache-sync-keys-value)))))))))

(defun org-element--cache-process-request
    (request next-request-key threshold time-limit future-change offset)
  "Process synchronization REQUEST for all entries before NEXT.

REQUEST is a vector, built by `org-element--cache-submit-request'.

NEXT-REQUEST-KEY is a cache key of the next request, as returned by
`org-element--cache-key'.

When non-nil, THRESHOLD is a buffer position.  Synchronization
stops as soon as a shifted element begins after it.

When non-nil, TIME-LIMIT is a time value.  Synchronization stops
after this time or when Emacs exits idle state.

When non-nil, FUTURE-CHANGE is a buffer position where changes not
registered yet in the cache are going to happen.  OFFSET is the
changed text length.  See `org-element--cache-submit-request' for more
information.

Throw `org-element--cache-interrupt' if the process stops before
completing the request."
  (org-with-base-buffer nil
    (org-element--cache-log-message
     "org-element-cache: Processing request %s up to %S-%S, next: %S"
     (let ((print-length 10) (print-level 3)) (prin1-to-string request))
     future-change
     threshold
     next-request-key)
    (catch 'org-element--cache-quit
      (when (= (org-element--request-phase request) 0)
        ;; Phase 0.
        ;;
        ;; Delete all elements starting after beginning of the element
        ;; with request key NEXT, but not after buffer position END.
        ;;
        ;; At each iteration, we start again at tree root since
        ;; a deletion modifies structure of the balanced tree.
        (org-element--cache-log-message "Phase 0")
        (catch 'org-element--cache-end-phase
          (let ((deletion-count 0))
            (while t
	      (when (org-element--cache-interrupt-p time-limit)
                (org-element--cache-log-message "Interrupt: time limit")
	        (throw 'org-element--cache-interrupt nil))
	      (let ((request-key (org-element--request-key request))
		    (end (org-element--request-end request))
		    (node (org-element--cache-root))
		    data data-key)
	        ;; Find first element in cache with key REQUEST-KEY or
	        ;; after it.
	        (while node
	          (let* ((element (avl-tree--node-data node))
		         (key (org-element--cache-key element)))
		    (cond
		     ((org-element--cache-key-less-p key request-key)
		      (setq node (avl-tree--node-right node)))
		     ((org-element--cache-key-less-p request-key key)
		      (setq data element
			    data-key key
			    node (avl-tree--node-left node)))
		     (t (setq data element
			      data-key key
			      node nil)))))
	        (if data
                    ;; We found first element in cache starting at or
                    ;; after REQUEST-KEY.
		    (let ((pos (org-element-property :begin data)))
                      ;; FIXME: Maybe simply (< pos end)?
		      (if (<= pos end)
                          (progn
                            (org-element--cache-log-message "removing %S::%S"
                                                 (org-element-property :org-element--cache-sync-key data)
                                                 (org-element--format-element data))
                            (cl-incf deletion-count)
                            (org-element--cache-remove data)
                            (when (and (> (log org-element--cache-size 2) 10)
                                       (> deletion-count
                                          (/ org-element--cache-size (log org-element--cache-size 2))))
                              (org-element--cache-log-message "Removed %S>N/LogN(=%S/%S) elements.  Resetting cache to prevent performance degradation"
                                                   deletion-count
                                                   org-element--cache-size
                                                   (log org-element--cache-size 2))
                              (org-element-cache-reset)
                              (throw 'org-element--cache-quit t)))
                        ;; Done deleting everything starting before END.
                        ;; DATA-KEY is the first known element after END.
                        ;; Move on to phase 1.
                        (org-element--cache-log-message
                         "found element after %S: %S::%S"
                         end
                         (org-element-property :org-element--cache-sync-key data)
                         (org-element--format-element data))
                        (setf (org-element--request-key request) data-key)
                        (setf (org-element--request-beg request) pos)
                        (setf (org-element--request-phase request) 1)
		        (throw 'org-element--cache-end-phase nil)))
	          ;; No element starting after modifications left in
	          ;; cache: further processing is futile.
                  (org-element--cache-log-message
                   "Phase 0 deleted all elements in cache after %S!"
                   request-key)
	          (throw 'org-element--cache-quit t)))))))
      (when (= (org-element--request-phase request) 1)
        ;; Phase 1.
        ;;
        ;; Phase 0 left a hole in the cache.  Some elements after it
        ;; could have parents within.  For example, in the following
        ;; buffer:
        ;;
        ;;   - item
        ;;
        ;;
        ;;     Paragraph1
        ;;
        ;;     Paragraph2
        ;;
        ;; if we remove a blank line between "item" and "Paragraph1",
        ;; everything down to "Paragraph2" is removed from cache.  But
        ;; the paragraph now belongs to the list, and its `:parent'
        ;; property no longer is accurate.
        ;;
        ;; Therefore we need to parse again elements in the hole, or at
        ;; least in its last section, so that we can re-parent
        ;; subsequent elements, during phase 2.
        ;;
        ;; Note that we only need to get the parent from the first
        ;; element in cache after the hole.
        ;;
        ;; When next key is lesser or equal to the current one, current
        ;; request is inside a to-be-shifted part of the cache.  It is
        ;; fine because the order of elements will not be altered by
        ;; shifting.  However, we cannot know the real position of the
        ;; unshifted NEXT element in the current request.  So, we need
        ;; to sort the request list according to keys and re-start
        ;; processing from the new leftmost request.
        (org-element--cache-log-message "Phase 1")
        (let ((key (org-element--request-key request)))
	  (when (and next-request-key (not (org-element--cache-key-less-p key next-request-key)))
            ;; In theory, the only case when requests are not
            ;; ordered is when key of the next request is either the
            ;; same with current key or it is a key for a removed
            ;; element. Either way, we can simply merge the two
            ;; requests.
	    (let ((next-request (nth 1 org-element--cache-sync-requests)))
              (org-element--cache-log-message "Phase 1: Unorderered requests. Merging: %S\n%S\n"
                                   (let ((print-length 10) (print-level 3)) (prin1-to-string request))
                                   (let ((print-length 10) (print-level 3)) (prin1-to-string next-request)))
	      (setf (org-element--request-key next-request) key)
              (setf (org-element--request-beg next-request) (org-element--request-beg request))
	      (setf (org-element--request-phase next-request) 1)
              (throw 'org-element--cache-quit t))))
        ;; Next element will start at its beginning position plus
        ;; offset, since it hasn't been shifted yet.  Therefore, LIMIT
        ;; contains the real beginning position of the first element to
        ;; shift and re-parent.
        (let ((limit (+ (org-element--request-beg request) (org-element--request-offset request)))
              cached-before)
	  (cond ((and threshold (> limit threshold))
                 (org-element--cache-log-message "Interrupt: position %S after threshold %S" limit threshold)
                 (throw 'org-element--cache-interrupt nil))
	        ((and future-change (>= limit future-change))
	         ;; Changes happened around this element and they will
	         ;; trigger another phase 1 request.  Skip re-parenting
	         ;; and simply proceed with shifting (phase 2) to make
	         ;; sure that followup phase 0 request for the recent
	         ;; changes can operate on the correctly shifted cache.
                 (org-element--cache-log-message "position %S after future change %S" limit future-change)
                 (setf (org-element--request-parent request) nil)
                 (setf (org-element--request-phase request) 2))
	        (t
                 (when future-change
                   ;; Changes happened, but not yet registered after
                   ;; this element.  However, we a not yet safe to look
                   ;; at the buffer and parse elements in the cache gap.
                   ;; Some of the parents to be added to cache may end
                   ;; after the changes.  Parsing this parents will
                   ;; assign the :end correct value for cache state
                   ;; after future-change.  Then, when the future change
                   ;; is going to be processed, such parent boundary
                   ;; will be altered unnecessarily.  To avoid this,
                   ;; we alter the new parents by -OFFSET.
                   ;; For now, just save last known cached element and
                   ;; then check all the parents below.
                   (setq cached-before (org-element--cache-find (1- limit) nil)))
                 ;; No relevant changes happened after submitting this
                 ;; request.  We are safe to look at the actual Org
                 ;; buffer and calculate the new parent.
	         (let ((parent (org-element--parse-to (1- limit) nil time-limit)))
                   (when future-change
                     ;; Check all the newly added parents to not
                     ;; intersect with future change.
                     (let ((up parent))
                       (while (and up
                                   (or (not cached-before)
                                       (> (org-element-property :begin up)
                                          (org-element-property :begin cached-before))))
                         (when (> (org-element-property :end up) future-change)
                           ;; Offset future cache request.
                           (org-element--cache-shift-positions
                            up (- offset)
                            (if (and (org-element-property :robust-begin up)
                                     (org-element-property :robust-end up))
                                '(:contents-end :end :robust-end)
                              '(:contents-end :end))))
                         (setq up (org-element-property :parent up)))))
                   (org-element--cache-log-message
                    "New parent at %S: %S::%S"
                    limit
                    (org-element-property :org-element--cache-sync-key parent)
                    (org-element--format-element parent))
                   (setf (org-element--request-parent request) parent)
		   (setf (org-element--request-phase request) 2))))))
      ;; Phase 2.
      ;;
      ;; Shift all elements starting from key START, but before NEXT, by
      ;; OFFSET, and re-parent them when appropriate.
      ;;
      ;; Elements are modified by side-effect so the tree structure
      ;; remains intact.
      ;;
      ;; Once THRESHOLD, if any, is reached, or once there is an input
      ;; pending, exit.  Before leaving, the current synchronization
      ;; request is updated.
      (org-element--cache-log-message "Phase 2")
      (let ((start (org-element--request-key request))
	    (offset (org-element--request-offset request))
	    (parent (org-element--request-parent request))
	    (node (org-element--cache-root))
	    (stack (list nil))
	    (leftp t)
	    exit-flag continue-flag)
        ;; No re-parenting nor shifting planned: request is over.
        (when (and (not parent) (zerop offset))
          (org-element--cache-log-message "Empty offset. Request completed.")
          (throw 'org-element--cache-quit t))
        (while node
	  (let* ((data (avl-tree--node-data node))
	         (key (org-element--cache-key data)))
            ;; Traverse the cache tree.  Ignore all the elements before
            ;; START.  Note that `avl-tree-stack' would not bypass the
            ;; elements before START and thus would have been less
            ;; efficient.
	    (if (and leftp (avl-tree--node-left node)
		     (not (org-element--cache-key-less-p key start)))
	        (progn (push node stack)
		       (setq node (avl-tree--node-left node)))
              ;; Shift and re-parent when current node starts at or
              ;; after START, but before NEXT.
	      (unless (org-element--cache-key-less-p key start)
	        ;; We reached NEXT.  Request is complete.
	        (when (and next-request-key
                           (not (org-element--cache-key-less-p key next-request-key)))
                  (org-element--cache-log-message "Reached next request.")
                  (let ((next-request (nth 1 org-element--cache-sync-requests)))
                    (unless (and (org-element-property :cached (org-element--request-parent next-request))
                                 (org-element-property :begin (org-element--request-parent next-request))
                                 parent
                                 (> (org-element-property :begin (org-element--request-parent next-request))
                                    (org-element-property :begin parent)))
                      (setf (org-element--request-parent next-request) parent)))
                  (throw 'org-element--cache-quit t))
	        ;; Handle interruption request.  Update current request.
	        (when (or exit-flag (org-element--cache-interrupt-p time-limit))
                  (org-element--cache-log-message "Interrupt: %s" (if exit-flag "threshold" "time limit"))
                  (setf (org-element--request-key request) key)
                  (setf (org-element--request-parent request) parent)
                  (throw 'org-element--cache-interrupt nil))
	        ;; Shift element.
	        (unless (zerop offset)
                  (when (>= org-element--cache-diagnostics-level 3)
                    (org-element--cache-log-message "Shifting positions (𝝙%S) in %S::%S"
                                         offset
                                         (org-element-property :org-element--cache-sync-key data)
                                         (org-element--format-element data)))
		  (org-element--cache-shift-positions data offset))
	        (let ((begin (org-element-property :begin data)))
		  ;; Update PARENT and re-parent DATA, only when
		  ;; necessary.  Propagate new structures for lists.
		  (while (and parent
			      (<= (org-element-property :end parent) begin))
		    (setq parent (org-element-property :parent parent)))
		  (cond ((and (not parent) (zerop offset)) (throw 'org-element--cache-quit nil))
                        ;; Consider scenario when DATA lays within
                        ;; sensitive lines of PARENT that was found
                        ;; during phase 2.  For example:
                        ;;
                        ;; #+ begin_quote
                        ;; Paragraph
                        ;; #+end_quote
                        ;;
                        ;; In the above source block, remove space in
                        ;; the first line will trigger re-parenting of
                        ;; the paragraph and "#+end_quote" that is also
                        ;; considered paragraph before the modification.
                        ;; However, the paragraph element stored in
                        ;; cache must be deleted instead.
                        ((and parent
                              (or (not (memq (org-element-type parent) org-element-greater-elements))
                                  (and (org-element-property :contents-begin parent)
                                       (< (org-element-property :begin data) (org-element-property :contents-begin parent)))
                                  (and (org-element-property :contents-end parent)
                                       (>= (org-element-property :begin data) (org-element-property :contents-end parent)))
                                  (> (org-element-property :end data) (org-element-property :end parent))
                                  (and (org-element-property :contents-end data)
                                       (> (org-element-property :contents-end data) (org-element-property :contents-end parent)))))
                         (org-element--cache-log-message "org-element-cache: Removing obsolete element with key %S::%S"
                                              (org-element-property :org-element--cache-sync-key data)
                                              (org-element--format-element data))
                         (org-element--cache-remove data)
                         ;; We altered the tree structure.  The tree
                         ;; traversal needs to be restarted.
                         (setf (org-element--request-key request) key)
                         (setf (org-element--request-parent request) parent)
                         ;; Restart tree traversal.
                         (setq node (org-element--cache-root)
	                       stack (list nil)
	                       leftp t
                               begin -1
                               continue-flag t))
		        ((and parent
                              (not (eq parent data))
			      (let ((p (org-element-property :parent data)))
			        (or (not p)
				    (< (org-element-property :begin p)
				       (org-element-property :begin parent))
                                    (unless (eq p parent)
                                      (not (org-element-property :cached p))
                                      ;; (not (avl-tree-member-p org-element--cache p))
                                      ))))
                         (org-element--cache-log-message
                          "Updating parent in %S\n Old parent: %S\n New parent: %S"
                          (org-element--format-element data)
                          (org-element--format-element (org-element-property :parent data))
                          (org-element--format-element parent))
                         (when (and (eq 'org-data (org-element-type parent))
                                    (not (eq 'headline (org-element-type data))))
                           ;; FIXME: This check is here to see whether
                           ;; such error happens within
                           ;; `org-element--cache-process-request' or somewhere
                           ;; else.
                           (org-element--cache-warn
                            "Added org-data parent to non-headline element: %S
If this warning appears regularly, please report the warning text to Org mode mailing list (M-x org-submit-bug-report)."
                            data)
                           (org-element-cache-reset)
                           (throw 'org-element--cache-quit t))
		         (org-element-put-property data :parent parent)
		         (let ((s (org-element-property :structure parent)))
			   (when (and s (org-element-property :structure data))
			     (org-element-put-property data :structure s)))))
		  ;; Cache is up-to-date past THRESHOLD.  Request
		  ;; interruption.
		  (when (and threshold (> begin threshold))
                    (org-element--cache-log-message "Reached threshold %S: %S"
                                         threshold
                                         (org-element--format-element data))
                    (setq exit-flag t))))
              (if continue-flag
                  (setq continue-flag nil)
	        (setq node (if (setq leftp (avl-tree--node-right node))
			       (avl-tree--node-right node)
			     (pop stack)))))))
        ;; We reached end of tree: synchronization complete.
        t))
    (org-element--cache-log-message
     "org-element-cache: Finished process. The cache size is %S. The remaining sync requests: %S"
     org-element--cache-size
     (let ((print-level 2)) (prin1-to-string org-element--cache-sync-requests)))))

(defsubst org-element--open-end-p (element)
  "Check if ELEMENT in current buffer contains extra blank lines after
it and does not have closing term.

Examples of such elements are: section, headline, org-data,
and footnote-definition."
  (and (org-element-property :contents-end element)
       (= (org-element-property :contents-end element)
          (save-excursion
            (goto-char (org-element-property :end element))
            (skip-chars-backward " \r\n\t")
            (line-beginning-position 2)))))

(defun org-element--parse-to (pos &optional syncp time-limit)
  "Parse elements in current section, down to POS.

Start parsing from the closest between the last known element in
cache or headline above.  Return the smallest element containing
POS.

When optional argument SYNCP is non-nil, return the parent of the
element containing POS instead.  In that case, it is also
possible to provide TIME-LIMIT, which is a time value specifying
when the parsing should stop.  The function throws
`org-element--cache-interrupt' if the process stopped before finding
the expected result."
  (catch 'exit
    (save-match-data
      (org-with-base-buffer nil
        (org-with-wide-buffer
         (goto-char pos)
         (save-excursion
           (end-of-line)
           (skip-chars-backward " \r\t\n")
           ;; Within blank lines at the beginning of buffer, return nil.
           (when (bobp) (throw 'exit nil)))
         (let* ((cached (and (org-element--cache-active-p)
			     (org-element--cache-find pos nil)))
                (mode (org-element-property :mode cached))
                element next)
           (cond
            ;; Nothing in cache before point: start parsing from first
            ;; element in buffer down to POS or from the beginning of the
            ;; file.
            ((and (not cached) (org-element--cache-active-p))
             (setq element (org-element-org-data-parser))
             (unless (org-element-property :begin element)
               (org-element--cache-warn "Error parsing org-data. Got %S\nPlease report to Org mode mailing list (M-x org-submit-bug-report)." element))
             (org-element--cache-log-message
              "Nothing in cache. Adding org-data: %S"
              (org-element--format-element element))
             (org-element--cache-put element)
             (goto-char (org-element-property :contents-begin element))
	     (setq mode 'org-data))
            ;; Nothing in cache before point because cache is not active.
            ;; Parse from previous heading to avoid re-parsing the whole
            ;; buffer above.  This comes at the cost of not calculating
            ;; `:parent' property for headings.
            ((not cached)
             (if (org-with-limited-levels (outline-previous-heading))
                 (progn
                   (setq element (org-element-headline-parser nil 'fast))
	           (setq mode 'planning)
	           (forward-line))
               (setq element (org-element-org-data-parser))
	       (setq mode 'org-data))
             (org-skip-whitespace)
             (beginning-of-line))
            ;; Check if CACHED or any of its ancestors contain point.
            ;;
            ;; If there is such an element, we inspect it in order to know
            ;; if we return it or if we need to parse its contents.
            ;; Otherwise, we just start parsing from location, which is
            ;; right after the top-most element containing CACHED but
            ;; still before POS.
            ;;
            ;; As a special case, if POS is at the end of the buffer, we
            ;; want to return the innermost element ending there.
            ;;
            ;; Also, if we find an ancestor and discover that we need to
            ;; parse its contents, make sure we don't start from
            ;; `:contents-begin', as we would otherwise go past CACHED
            ;; again.  Instead, in that situation, we will resume parsing
            ;; from NEXT, which is located after CACHED or its higher
            ;; ancestor not containing point.
            (t
             (let ((up cached)
                   (pos (if (= (point-max) pos) (1- pos) pos)))
               (while (and up (<= (org-element-property :end up) pos))
                 (goto-char (org-element-property :end up))
                 (setq element up
                       mode (org-element--next-mode (org-element-property :mode element) (org-element-type element) nil)
                       up (org-element-property :parent up)
                       next (point)))
               (when up (setq element up)))))
           ;; Parse successively each element until we reach POS.
           (let ((end (or (org-element-property :end element) (point-max)))
	         (parent (org-element-property :parent element)))
             (while t
	       (when (org-element--cache-interrupt-p time-limit)
                 (throw 'org-element--cache-interrupt nil))
               (when (and inhibit-quit org-element--cache-interrupt-C-g quit-flag)
                 (when quit-flag
	           (cl-incf org-element--cache-interrupt-C-g-count)
                   (setq quit-flag nil))
                 (when (>= org-element--cache-interrupt-C-g-count
                           org-element--cache-interrupt-C-g-max-count)
                   (setq quit-flag t)
                   (setq org-element--cache-interrupt-C-g-count 0)
                   (org-element-cache-reset)
                   (error "org-element: Parsing aborted by user.  Cache has been cleared.
If you observe Emacs hangs frequently, please report this to Org mode mailing list (M-x org-submit-bug-report)."))
                 (message (substitute-command-keys
                           "`org-element--parse-buffer': Suppressed `\\[keyboard-quit]'.  Press `\\[keyboard-quit]' %d more times to force interruption.")
                          (- org-element--cache-interrupt-C-g-max-count
                             org-element--cache-interrupt-C-g-count)))
	       (unless element
                 ;; Do not try to parse within blank at EOB.
                 (unless (save-excursion
                           (org-skip-whitespace)
                           (eobp))
                   (org-element-with-disabled-cache
                     (setq element (org-element--current-element
			            end 'element mode
			            (org-element-property :structure parent)))))
                 ;; Make sure that we return referenced element in cache
                 ;; that can be altered directly.
                 (if element
                     (setq element (or (org-element--cache-put element) element))
                   ;; Nothing to parse (i.e. empty file).
                   (throw 'exit parent))
                 (unless (or (not (org-element--cache-active-p)) parent)
                   (org-element--cache-warn
                    "Got empty parent while parsing. Please report it to Org mode mailing list (M-x org-submit-bug-report).\n Backtrace:\n%S"
                    (when (and (fboundp 'backtrace-get-frames)
                               (fboundp 'backtrace-to-string))
                      (backtrace-to-string (backtrace-get-frames 'backtrace))
                      (org-element-cache-reset)
                      (error "org-element--cache: Emergency exit"))))
	         (org-element-put-property element :parent parent))
	       (let ((elem-end (org-element-property :end element))
	             (type (org-element-type element)))
	         (cond
	          ;; Skip any element ending before point.  Also skip
	          ;; element ending at point (unless it is also the end of
	          ;; buffer) since we're sure that another element begins
	          ;; after it.
	          ((and (<= elem-end pos) (/= (point-max) elem-end))
                   ;; Avoid parsing headline siblings above.
                   (goto-char elem-end)
                   (when (eq type 'headline)
                     (save-match-data
                       (unless (when (and (/= 1 (org-element-property :level element))
                                          (re-search-forward
                                           (rx-to-string
                                            `(and bol (repeat 1 ,(1- (let ((level (org-element-property :level element)))
                                                                       (if org-odd-levels-only (1- (* level 2)) level)))
                                                              "*")
                                                  " "))
                                           pos t))
                                 (beginning-of-line)
                                 t)
                         ;; There are headings with lower level than
                         ;; ELEMENT between ELEM-END and POS.  Siblings
                         ;; may exist though.  Parse starting from the
                         ;; last sibling or from ELEM-END if there are
                         ;; no other siblings.
                         (goto-char pos)
                         (unless
                             (re-search-backward
                              (rx-to-string
                               `(and bol (repeat ,(let ((level (org-element-property :level element)))
                                                    (if org-odd-levels-only (1- (* level 2)) level))
                                                 "*")
                                     " "))
                              elem-end t)
                           ;; Roll-back to normal parsing.
                           (goto-char elem-end)))))
	           (setq mode (org-element--next-mode mode type nil)))
	          ;; A non-greater element contains point: return it.
	          ((not (memq type org-element-greater-elements))
	           (throw 'exit (if syncp parent element)))
	          ;; Otherwise, we have to decide if ELEMENT really
	          ;; contains POS.  In that case we start parsing from
	          ;; contents' beginning.
	          ;;
	          ;; If POS is at contents' beginning but it is also at
	          ;; the beginning of the first item in a list or a table.
	          ;; In that case, we need to create an anchor for that
	          ;; list or table, so return it.
	          ;;
	          ;; Also, if POS is at the end of the buffer, no element
	          ;; can start after it, but more than one may end there.
	          ;; Arbitrarily, we choose to return the innermost of
	          ;; such elements.
	          ((let ((cbeg (org-element-property :contents-begin element))
		         (cend (org-element-property :contents-end element)))
	             (when (and cbeg cend
			        (or (< cbeg pos)
			            (and (= cbeg pos)
				         (not (memq type '(plain-list table)))))
			        (or (> cend pos)
                                    ;; When we are at cend or within blank
                                    ;; lines after, it is a special case:
                                    ;; 1. At the end of buffer we return
                                    ;; the innermost element.
                                    ;; 2. At cend of element with return
                                    ;; that element.
                                    ;; 3. At the end of element, we would
                                    ;; return in the earlier cond form.
                                    ;; 4. Within blank lines after cend,
                                    ;; when element does not have a
                                    ;; closing keyword, we return that
                                    ;; outermost element, unless the
                                    ;; outermost element is a non-empty
                                    ;; headline.  In the latter case, we
                                    ;; return the outermost element inside
                                    ;; the headline section.
			            (and (org-element--open-end-p element)
                                         (or (= (org-element-property :end element) (point-max))
                                             (and (>= pos (org-element-property :contents-end element))
                                                  (memq (org-element-type element) '(org-data section headline)))))))
		       (goto-char (or next cbeg))
		       (setq mode (if next mode (org-element--next-mode mode type t))
                             next nil
		             parent element
		             end (if (org-element--open-end-p element)
                                     (org-element-property :end element)
                                   (org-element-property :contents-end element))))))
	          ;; Otherwise, return ELEMENT as it is the smallest
	          ;; element containing POS.
	          (t (throw 'exit (if syncp parent element)))))
	       (setq element nil)))))))))

;;;; Staging Buffer Changes

(defconst org-element--cache-sensitive-re
  (concat
   "^\\*+ " "\\|"
   "\\\\end{[A-Za-z0-9*]+}[ \t]*$" "\\|"
   "^[ \t]*\\(?:"
   "#\\+END\\(?:_\\|:?[ \t]*$\\)" "\\|"
   org-list-full-item-re "\\|"
   ":\\(?: \\|$\\)" "\\|"
   ":\\(?:\\w\\|[-_]\\)+:[ \t]*$"
   "\\)")
  "Regexp matching a sensitive line, structure wise.
A sensitive line is a headline, inlinetask, block, drawer, or
latex-environment boundary.  When such a line is modified,
structure changes in the document may propagate in the whole
section, possibly making cache invalid.")

(defun org-element--cache-before-change (beg end)
  "Detect modifications in sensitive parts of Org buffer.
BEG and END are the beginning and end of the range of changed
text.  See `before-change-functions' for more information.

The function returns the new value of `org-element--cache-change-warning'."
  (org-with-base-buffer nil
    (when (org-element--cache-active-p t)
      (org-with-wide-buffer
       (setq org-element--cache-change-tic (buffer-chars-modified-tick))
       (setq org-element--cache-last-buffer-size (buffer-size))
       (goto-char beg)
       (beginning-of-line)
       (let ((bottom (save-excursion
                       (goto-char end)
                       (if (and (bolp)
                                ;; When beg == end, still extent to eol.
                                (> (point) beg))
                           ;; FIXME: Potential pitfall.
                           ;; We are appending to an element end.
                           ;; Unless the last inserted char is not
                           ;; newline, the next element is not broken
                           ;; and does not need to be purged from the
                           ;; cache.
                           end
                         (line-end-position)))))
         (prog1
             ;; Use the worst change warning to not miss important edits.
             ;; This function is called before edit and after edit by
             ;; `org-element--cache-after-change'.  Before the edit, we still
             ;; want to use the old value if it comes from previous
             ;; not yet processed edit (they may be merged by
             ;; `org-element--cache-submit-request').  After the edit, we want to
             ;; look if there was a sensitive removed during edit.
             ;; FIXME: This is not the most efficient way and we now
             ;; have to delete more elements than needed in some
             ;; cases.  A better approach may be storing the warning
             ;; in the modification request itself.
             (let ((org-element--cache-change-warning-before org-element--cache-change-warning)
                   (org-element--cache-change-warning-after))
               (setq org-element--cache-change-warning-after
	             (save-match-data
                       (let ((case-fold-search t))
                         (when (re-search-forward
		                org-element--cache-sensitive-re bottom t)
                           (goto-char beg)
                           (beginning-of-line)
                           (let (min-level)
                             (cl-loop while (re-search-forward
                                             (rx-to-string
                                              (if (and min-level
                                                       (> min-level 1))
                                                  `(and bol (repeat 1 ,(1- min-level) "*") " ")
                                                `(and bol (+ "*") " ")))
                                             bottom t)
                                      do (setq min-level (1- (length (match-string 0))))
                                      until (= min-level 1))
                             (goto-char beg)
                             (beginning-of-line)
                             (or (and min-level (org-reduced-level min-level))
                                 (when (looking-at-p "^[ \t]*#\\+CATEGORY:")
                                   'org-data)
                                 t))))))
               (setq org-element--cache-change-warning
                     (cond
                      ((and (numberp org-element--cache-change-warning-before)
                            (numberp org-element--cache-change-warning-after))
                       (min org-element--cache-change-warning-after
                            org-element--cache-change-warning-before))
                      ((numberp org-element--cache-change-warning-before)
                       org-element--cache-change-warning-before)
                      ((numberp org-element--cache-change-warning-after)
                       org-element--cache-change-warning-after)
                      (t (or org-element--cache-change-warning-after
                             org-element--cache-change-warning-before)))))
           (org-element--cache-log-message
            "%S is about to modify text: warning %S"
            this-command
            org-element--cache-change-warning)))))))

(defun org-element--cache-after-change (beg end pre)
  "Update buffer modifications for current buffer.
BEG and END are the beginning and end of the range of changed
text, and the length in bytes of the pre-change text replaced by
that range.  See `after-change-functions' for more information."
  (org-with-base-buffer nil
    (when (org-element--cache-active-p t)
      (when (not (eq org-element--cache-change-tic (buffer-chars-modified-tick)))
        (org-element--cache-log-message "After change")
        (setq org-element--cache-change-warning (org-element--cache-before-change beg end))
        ;; If beg is right after spaces in front of an element, we
        ;; risk affecting previous element, so move beg to bol, making
        ;; sure that we capture preceding element.
        (setq beg (save-excursion
                    (goto-char beg)
                    (cl-incf pre (- beg (line-beginning-position)))
                    (line-beginning-position)))
        ;; Store synchronization request.
        (let ((offset (- end beg pre)))
          (save-match-data
            (org-element--cache-submit-request beg (- end offset) offset)))
        ;; Activate a timer to process the request during idle time.
        (org-element--cache-set-timer (current-buffer))))))

(defun org-element--cache-setup-change-functions ()
  "Setup `before-change-functions' and `after-change-functions'."
  (when (and (derived-mode-p 'org-mode) org-element-use-cache)
    (add-hook 'before-change-functions
	      #'org-element--cache-before-change nil t)
    ;; Run `org-element--cache-after-change' early to handle cases
    ;; when other `after-change-functions' require element cache.
    (add-hook 'after-change-functions
	      #'org-element--cache-after-change -1 t)))

(defvar org-element--cache-avoid-synchronous-headline-re-parsing nil
  "This variable controls how buffer changes are handled by the cache.

By default (when this variable is nil), cache re-parses modified
headlines immediately after modification preserving all the unaffected
elements inside the headline.

The default behavior works best when users types inside Org buffer of
when buffer modifications are mixed with cache requests.  However,
large automated edits inserting/deleting many headlines are somewhat
slower by default (as in `org-archive-subtree').  Let-binding this
variable to non-nil will reduce cache latency after every singular edit
(`after-change-functions') at the cost of slower cache queries.")
(defun org-element--cache-for-removal (beg end offset)
  "Return first element to remove from cache.

BEG and END are buffer positions delimiting buffer modifications.
OFFSET is the size of the changes.

Returned element is usually the first element in cache containing
any position between BEG and END.  As an exception, greater
elements around the changes that are robust to contents
modifications are preserved and updated according to the
changes.  In the latter case, the returned element is the outermost
non-robust element affected by the changes.  Note that the returned
element may end before END position in which case some cached element
starting after the returned may still be affected by the changes.

Also, when there are no elements in cache before BEG, return first
known element in cache (it may start after END)."
  (let* ((elements (org-element--cache-find (1- beg) 'both))
	 (before (car elements))
	 (after (cdr elements)))
    (if (not before) after
      ;; If BEFORE is a keyword, it may need to be removed to become
      ;; an affiliated keyword.
      (when (eq 'keyword (org-element-type before))
        (let ((prev before))
          (while (eq 'keyword (org-element-type prev))
            (setq before prev
                  beg (org-element-property :begin prev))
            (setq prev (org-element--cache-find (1- (org-element-property :begin before)))))))
      (let ((up before)
	    (robust-flag t))
	(while up
	  (if (let ((type (org-element-type up)))
                (or (and (memq type '( center-block dynamic-block
                                       quote-block special-block
                                       drawer))
                         (or (not (eq type 'drawer))
                             (not (string= "PROPERTIES" (org-element-property :drawer-name up))))
                         ;; Sensitive change.  This is
                         ;; unconditionally non-robust change.
                         (not org-element--cache-change-warning)
		         (let ((cbeg (org-element-property :contents-begin up))
                               (cend (org-element-property :contents-end up)))
		           (and cbeg
                                (<= cbeg beg)
			        (or (> cend end)
                                    (and (= cend end)
                                         (= (+ end offset) (point-max)))))))
                    (and (memq type '(headline section org-data))
		         (let ((rbeg (org-element-property :robust-begin up))
                               (rend (org-element-property :robust-end up)))
		           (and rbeg rend
                                (<= rbeg beg)
                                (or (> rend end)
                                    (and (= rend end)
                                         (= (+ end offset) (point-max))))))
                         (pcase type
                           ;; Sensitive change in section.  Need to
                           ;; re-parse.
                           (`section (not org-element--cache-change-warning))
                           ;; Headline might be inserted.  This is non-robust
                           ;; change when `up' is a `headline' or `section'
                           ;; with `>' level compared to the inserted headline.
                           ;;
                           ;; Also, planning info/property drawer
                           ;; could have been inserted.  It is not
                           ;; robust change then.
                           (`headline
                            (and
                             (or (not (numberp org-element--cache-change-warning))
                                 (> org-element--cache-change-warning
                                    (org-element-property :level up)))
                             (org-with-point-at (org-element-property :contents-begin up)
                               (unless
                                   (save-match-data
                                     (when (looking-at-p org-element-planning-line-re)
                                       (forward-line))
                                     (when (looking-at org-property-drawer-re)
                                       (< beg (match-end 0))))
                                 'robust))))
                           (`org-data (and (not (eq org-element--cache-change-warning 'org-data))
                                           ;; Property drawer could
                                           ;; have been inserted.  It
                                           ;; is not robust change
                                           ;; then.
                                           (org-with-wide-buffer
                                            (goto-char (point-min))
                                            (while (and (org-at-comment-p) (bolp)) (forward-line))
                                            ;; Should not see property
                                            ;; drawer within changed
                                            ;; region.
                                            (save-match-data
                                              (or (not (looking-at org-property-drawer-re))
                                                  (> beg (match-end 0)))))))
                           (_ 'robust)))))
	      ;; UP is a robust greater element containing changes.
	      ;; We only need to extend its ending boundaries.
              (progn
	        (org-element--cache-shift-positions
                 up offset
                 (if (and (org-element-property :robust-begin up)
                          (org-element-property :robust-end up))
                     '(:contents-end :end :robust-end)
                   '(:contents-end :end)))
                (org-element--cache-log-message
                 "Shifting end positions of robust parent: %S"
                 (org-element--format-element up)))
            (unless (or
                     ;; UP is non-robust.  Yet, if UP is headline, flagging
                     ;; everything inside for removal may be to
                     ;; costly.  Instead, we should better re-parse only the
                     ;; headline itself when possible.  If a headline is still
                     ;; starting from old :begin position, we do not care that
                     ;; its boundaries could have extended to shrunk - we
                     ;; will re-parent and shift them anyway.
                     (and (eq 'headline (org-element-type up))
                          (not org-element--cache-avoid-synchronous-headline-re-parsing)
                          ;; The change is not inside headline.  Not
                          ;; updating here.
                          (not (<= beg (org-element-property :begin up)))
                          (not (> end (org-element-property :end up)))
                          (let ((current (org-with-point-at (org-element-property :begin up)
                                           (org-element-with-disabled-cache
                                             (and (looking-at-p org-element-headline-re)
                                                  (org-element-headline-parser))))))
                            (when (eq 'headline (org-element-type current))
                              (org-element--cache-log-message
                               "Found non-robust headline that can be updated individually: %S"
                               (org-element--format-element current))
                              (org-element-set-element up current)
                              t)))
                     ;; If UP is org-data, the situation is similar to
                     ;; headline case.  We just need to re-parse the
                     ;; org-data itself, unless the change is made
                     ;; within blank lines at BOB (that could
                     ;; potentially alter first-section).
                     (when (and (eq 'org-data (org-element-type up))
                                (>= beg (org-element-property :contents-begin up)))
                       (org-element-set-element up (org-with-point-at 1 (org-element-org-data-parser)))
                       (org-element--cache-log-message
                        "Found non-robust change invalidating org-data. Re-parsing: %S"
                        (org-element--format-element up))
                       t))
              (org-element--cache-log-message
               "Found non-robust element: %S"
               (org-element--format-element up))
              (setq before up)
	      (when robust-flag (setq robust-flag nil))))
          (unless (or (org-element-property :parent up)
                      (eq 'org-data (org-element-type up)))
            (org-element--cache-warn "Got element without parent. Please report it to Org mode mailing list (M-x org-submit-bug-report).\n%S" up)
            (org-element-cache-reset)
            (error "org-element--cache: Emergency exit"))
	  (setq up (org-element-property :parent up)))
        ;; We're at top level element containing ELEMENT: if it's
        ;; altered by buffer modifications, it is first element in
        ;; cache to be removed.  Otherwise, that first element is the
        ;; following one.
        ;;
        ;; As a special case, do not remove BEFORE if it is a robust
        ;; container for current changes.
        (if (or (< (org-element-property :end before) beg) robust-flag) after
	  before)))))

(defun org-element--cache-submit-request (beg end offset)
  "Submit a new cache synchronization request for current buffer.
BEG and END are buffer positions delimiting the minimal area
where cache data should be removed.  OFFSET is the size of the
change, as an integer."
  (org-element--cache-log-message
   "Submitting new synchronization request for [%S..%S]𝝙%S"
   beg end offset)
  (org-with-base-buffer nil
    (let ((next (car org-element--cache-sync-requests))
	  delete-to delete-from)
      (if (and next
               ;; First existing sync request is in phase 0.
	       (= 0 (org-element--request-phase next))
               ;; Current changes intersect with the first sync request.
	       (> (setq delete-to (+ (org-element--request-end next)
                                     (org-element--request-offset next)))
                  end)
	       (<= (setq delete-from (org-element--request-beg next))
                  end))
	  ;; Current changes can be merged with first sync request: we
	  ;; can save a partial cache synchronization.
	  (progn
            (org-element--cache-log-message "Found another phase 0 request intersecting with current")
            ;; Update OFFSET of the existing request.
	    (cl-incf (org-element--request-offset next) offset)
	    ;; If last change happened within area to be removed, extend
	    ;; boundaries of robust parents, if any.  Otherwise, find
	    ;; first element to remove and update request accordingly.
	    (if (> beg delete-from)
                ;; The current modification is completely inside NEXT.
                ;; We already added the current OFFSET to the NEXT
                ;; request.  However, the robust elements around
                ;; modifications also need to be shifted.  Moreover, the
                ;; new modification may also have non-nil
                ;; `org-element--cache-change-warning'.  In the latter case, we
                ;; also need to update the request.
                (let ((first (org-element--cache-for-removal delete-from end offset) ; Shift as needed.
                             ))
                  (org-element--cache-log-message
                   "Current request is inside next. Candidate parent: %S"
                   (org-element--format-element first))
                  (when
                      ;; Non-robust element is now before NEXT.  Need to
                      ;; update.
                      (and first
                           (org-element--cache-key-less-p
                            (org-element--cache-key first)
                            (org-element--request-key next)))
                    (org-element--cache-log-message
                     "Current request is inside next. New parent: %S"
                     (org-element--format-element first))
                    (setf (org-element--request-key next)
                          (org-element--cache-key first))
                    (setf (org-element--request-beg next)
                          (org-element-property :begin first))
                    (setf (org-element--request-end next)
                          (max (org-element-property :end first)
                               (org-element--request-end next)))
                    (setf (org-element--request-parent next)
                          (org-element-property :parent first))))
              ;; The current and NEXT modifications are intersecting
              ;; with current modification starting before NEXT and NEXT
              ;; ending after current.  We need to update the common
              ;; non-robust parent for the new extended modification
              ;; region.
	      (let ((first (org-element--cache-for-removal beg delete-to offset)))
                (org-element--cache-log-message
                 "Current request intersects with next. Candidate parent: %S"
                 (org-element--format-element first))
	        (when (and first
                           (org-element--cache-key-less-p
                            (org-element--cache-key first)
                            (org-element--request-key next)))
                  (org-element--cache-log-message
                   "Current request intersects with next. Updating. New parent: %S"
                   (org-element--format-element first))
                  (setf (org-element--request-key next) (org-element--cache-key first))
                  (setf (org-element--request-beg next) (org-element-property :begin first))
                  (setf (org-element--request-end next)
                        (max (org-element-property :end first)
                             (org-element--request-end next)))
                  (setf (org-element--request-parent next) (org-element-property :parent first))))))
        ;; Ensure cache is correct up to END.  Also make sure that NEXT,
        ;; if any, is no longer a 0-phase request, thus ensuring that
        ;; phases are properly ordered.  We need to provide OFFSET as
        ;; optional parameter since current modifications are not known
        ;; yet to the otherwise correct part of the cache (i.e, before
        ;; the first request).
        (org-element--cache-log-message "Adding new phase 0 request")
        (when next (org-element--cache-sync (current-buffer) end beg offset))
        (let ((first (org-element--cache-for-removal beg end offset)))
	  (if first
	      (push (let ((first-beg (org-element-property :begin first))
			  (key (org-element--cache-key first)))
		      (cond
		       ;; When changes happen before the first known
		       ;; element, re-parent and shift the rest of the
		       ;; cache.
		       ((> first-beg end)
                        (org-element--cache-log-message "Changes are before first known element. Submitting phase 1 request")
                        (vector key first-beg nil offset nil 1))
		       ;; Otherwise, we find the first non robust
		       ;; element containing END.  All elements between
		       ;; FIRST and this one are to be removed.
                       ;;
                       ;; The current modification is completely inside
                       ;; FIRST.  Clear and update cached elements in
                       ;; region containing FIRST.
		       ((let ((first-end (org-element-property :end first)))
			  (when (> first-end end)
                            (org-element--cache-log-message "Extending to non-robust element %S" (org-element--format-element first))
			    (vector key first-beg first-end offset (org-element-property :parent first) 0))))
		       (t
                        ;; Now, FIRST is the first element after BEG or
                        ;; non-robust element containing BEG.  However,
                        ;; FIRST ends before END and there might be
                        ;; another ELEMENT before END that spans beyond
                        ;; END.  If there is such element, we need to
                        ;; extend the region down to end of the common
                        ;; parent of FIRST and everything inside
                        ;; BEG..END.
		        (let* ((element (org-element--cache-find end))
			       (element-end (org-element-property :end element))
			       (up element))
			  (while (and (not (eq up first))
                                      (setq up (org-element-property :parent up))
				      (>= (org-element-property :begin up) first-beg))
                            ;; Note that UP might have been already
                            ;; shifted if it is a robust element.  After
                            ;; deletion, it can put it's end before yet
                            ;; unprocessed ELEMENT.
			    (setq element-end (max (org-element-property :end up) element-end)
				  element up))
                          ;; Extend region to remove elements between
                          ;; beginning of first and the end of outermost
                          ;; element starting before END but after
                          ;; beginning of first.
                          ;; of the FIRST.
                          (org-element--cache-log-message
                           "Extending to all elements between:\n 1: %S\n 2: %S"
                           (org-element--format-element first)
                           (org-element--format-element element))
			  (vector key first-beg element-end offset up 0)))))
		    org-element--cache-sync-requests)
	    ;; No element to remove.  No need to re-parent either.
	    ;; Simply shift additional elements, if any, by OFFSET.
	    (if org-element--cache-sync-requests
                (progn
                  (org-element--cache-log-message
                   "Nothing to remove. Updating offset of the next request by 𝝙%S: %S"
                   offset
                   (let ((print-level 3))
                     (car org-element--cache-sync-requests)))
	          (cl-incf (org-element--request-offset (car org-element--cache-sync-requests))
		           offset))
              (org-element--cache-log-message
               "Nothing to remove. No elements in cache after %S. Terminating."
               end))))))
    (setq org-element--cache-change-warning nil)))

(defun org-element--cache-verify-element (element)
  "Verify correctness of ELEMENT when `org-element--cache-self-verify' is non-nil.

Return non-nil when verification failed."
  (let ((org-element--cache-self-verify
         (or org-element--cache-self-verify
             (and (boundp 'org-batch-test) org-batch-test)))
        (org-element--cache-self-verify-frequency
         (if (and (boundp 'org-batch-test) org-batch-test)
             1
           org-element--cache-self-verify-frequency)))
    ;; Verify correct parent for the element.
    (unless (or (not org-element--cache-self-verify)
                (org-element-property :parent element)
                (eq 'org-data (org-element-type element)))
      (org-element--cache-warn "Got element without parent (cache active?: %S). Please report it to Org mode mailing list (M-x org-submit-bug-report).\n%S" (org-element--cache-active-p)  element)
      (org-element-cache-reset))
    (when (and org-element--cache-self-verify
               (org-element--cache-active-p)
               (eq 'headline (org-element-type element))
               ;; Avoid too much slowdown
               (< (random 1000) (* 1000 org-element--cache-self-verify-frequency)))
      (org-with-point-at (org-element-property :begin element)
        (org-element-with-disabled-cache (org-up-heading-or-point-min))
        (unless (or (= (point) (org-element-property :begin (org-element-property :parent element)))
                    (eq (point) (point-min)))
          (org-element--cache-warn
           "Cached element has wrong parent in %s. Resetting.
If this warning appears regularly, please report the warning text to Org mode mailing list (M-x org-submit-bug-report).
The element is: %S\n The parent is: %S\n The real parent is: %S"
           (buffer-name (current-buffer))
           (org-element--format-element element)
           (org-element--format-element (org-element-property :parent element))
           (org-element--format-element (org-element--current-element (org-element-property :end (org-element-property :parent element)))))
          (org-element-cache-reset))
        (org-element--cache-verify-element (org-element-property :parent element))))
    ;; Verify the element itself.
    (when (and org-element--cache-self-verify
               (org-element--cache-active-p)
               element
               (not (memq (org-element-type element) '(section org-data)))
               ;; Avoid too much slowdown
               (< (random 1000) (* 1000 org-element--cache-self-verify-frequency)))
      (let ((real-element (let (org-element-use-cache)
                            (org-element--parse-to
                             (if (memq (org-element-type element) '(table-row item))
                                 (1+ (org-element-property :begin element))
                               (org-element-property :begin element))))))
        (unless (and (eq (org-element-type real-element) (org-element-type element))
                     (eq (org-element-property :begin real-element) (org-element-property :begin element))
                     (eq (org-element-property :end real-element) (org-element-property :end element))
                     (eq (org-element-property :contents-begin real-element) (org-element-property :contents-begin element))
                     (eq (org-element-property :contents-end real-element) (org-element-property :contents-end element))
                     (or (not (org-element-property :ID real-element))
                         (string= (org-element-property :ID real-element) (org-element-property :ID element))))
          (org-element--cache-warn "(%S) Cached element is incorrect in %s. (Cache tic up to date: %S) Resetting.
If this warning appears regularly, please report the warning text to Org mode mailing list (M-x org-submit-bug-report).
The element is: %S\n The real element is: %S\n Cache around :begin:\n%S\n%S\n%S"
                                   this-command
                                   (buffer-name (current-buffer))
                                   (if (/= org-element--cache-change-tic
                                          (buffer-chars-modified-tick))
                                       "no" "yes")
                                   (org-element--format-element element)
                                   (org-element--format-element real-element)
                                   (org-element--cache-find (1- (org-element-property :begin real-element)))
                                   (car (org-element--cache-find (org-element-property :begin real-element) 'both))
                                   (cdr (org-element--cache-find (org-element-property :begin real-element) 'both)))
          (org-element-cache-reset))))))

;;; Cache persistence

(defun org-element--cache-persist-before-write (container &optional associated)
  "Sync cache before saving."
  (when (equal container '(elisp org-element--cache))
    (if (and org-element-use-cache
             (plist-get associated :file)
             (get-file-buffer (plist-get associated :file))
             org-element-cache-persistent)
        (with-current-buffer (get-file-buffer (plist-get associated :file))
          (if (and (derived-mode-p 'org-mode)
                   org-element--cache)
              (org-with-wide-buffer
               (org-element--cache-sync (current-buffer) (point-max))
               ;; Cleanup cache request keys to avoid collisions during next
               ;; Emacs session.
               (avl-tree-mapc
                (lambda (el)
                  (org-element-put-property el :org-element--cache-sync-key nil))
                org-element--cache)
               nil)
            'forbid))
      'forbid)))

(defun org-element--cache-persist-before-read (container &optional associated)
  "Avoid reading cache before Org mode is loaded."
  (when (equal container '(elisp org-element--cache))
    (if (not (and (plist-get associated :file)
                (get-file-buffer (plist-get associated :file))))
        'forbid
      (with-current-buffer (get-file-buffer (plist-get associated :file))
        (unless (and org-element-use-cache
                     org-element-cache-persistent
                     (derived-mode-p 'org-mode)
                     (equal (secure-hash 'md5 (current-buffer))
                            (plist-get associated :hash)))
          'forbid)))))

(defun org-element--cache-persist-after-read (container &optional associated)
  "Setup restored cache."
  (when (and (plist-get associated :file)
             (get-file-buffer (plist-get associated :file)))
    (with-current-buffer (get-file-buffer (plist-get associated :file))
      (when (and org-element-use-cache org-element-cache-persistent)
        (when (and (equal container '(elisp org-element--cache)) org-element--cache)
          (setq-local org-element--cache-size (avl-tree-size org-element--cache)))
        (when (and (equal container '(elisp org-element--headline-cache)) org-element--headline-cache)
          (setq-local org-element--headline-cache-size (avl-tree-size org-element--headline-cache)))))))

(add-hook 'org-persist-before-write-hook #'org-element--cache-persist-before-write)
(add-hook 'org-persist-before-read-hook #'org-element--cache-persist-before-read)
(add-hook 'org-persist-after-read-hook #'org-element--cache-persist-after-read)

;;;; Public Functions

(defvar-local org-element--cache-gapless nil
  "An alist containing (granularity . `org-element--cache-change-tic') elements.
Each element indicates the latest `org-element--cache-change-tic' when
change did not contain gaps.")

;;;###autoload
(defun org-element-cache-reset (&optional all no-persistence)
  "Reset cache in current buffer.
When optional argument ALL is non-nil, reset cache in all Org
buffers.
When optional argument NO-PERSISTENCE is non-nil, do not try to update
the cache persistence in the buffer."
  (interactive "P")
  (dolist (buffer (if all (buffer-list) (list (current-buffer))))
    (org-with-base-buffer buffer
      (when (and org-element-use-cache (derived-mode-p 'org-mode))
        ;; Only persist cache in file buffers.
        (when (and (buffer-file-name) (not no-persistence))
          (when (not org-element-cache-persistent)
            (org-persist-unregister 'org-element--headline-cache (current-buffer))
            (org-persist-unregister 'org-element--cache (current-buffer)))
          (when (and org-element-cache-persistent
                     (buffer-file-name (current-buffer)))
            (org-persist-register 'org-element--cache (current-buffer))
            (org-persist-register 'org-element--headline-cache
                                  (current-buffer)
                                  :inherit 'org-element--cache)))
        (setq-local org-element--cache-change-tic (buffer-chars-modified-tick))
        (setq-local org-element--cache-last-buffer-size (buffer-size))
        (setq-local org-element--cache-gapless nil)
	(setq-local org-element--cache
		    (avl-tree-create #'org-element--cache-compare))
        (setq-local org-element--headline-cache
		    (avl-tree-create #'org-element--cache-compare))
        (setq-local org-element--cache-hash-left (make-vector org-element--cache-hash-size nil))
        (setq-local org-element--cache-hash-right (make-vector org-element--cache-hash-size nil))
        (setq-local org-element--cache-size 0)
        (setq-local org-element--headline-cache-size 0)
	(setq-local org-element--cache-sync-keys-value 0)
	(setq-local org-element--cache-change-warning nil)
	(setq-local org-element--cache-sync-requests nil)
	(setq-local org-element--cache-sync-timer nil)
        (org-element--cache-setup-change-functions)
        ;; Make sure that `org-element--cache-after-change' and
        ;; `org-element--cache-before-change' are working inside properly created
        ;; indirect buffers.  Note that `clone-indirect-buffer-hook'
        ;; will not work inside indirect buffers not created by
        ;; calling `clone-indirect-buffer'.  We consider that the code
        ;; not using `clone-indirect-buffer' to be written with
        ;; awareness about possible consequences.
        (add-hook 'clone-indirect-buffer-hook
                  #'org-element--cache-setup-change-functions)))))

;;;###autoload
(defun org-element-cache-refresh (pos)
  "Refresh cache at position POS."
  (when (org-element--cache-active-p)
    (org-element--cache-sync (current-buffer) pos)
    (org-element--cache-submit-request pos pos 0)
    (org-element--cache-set-timer (current-buffer))))

(defvar warning-minimum-log-level) ; Defined in warning.el

(defvar org-element-cache-map-continue-from nil
  "Position from where mapping should continue.
This variable can be set by called function, especially when the
function modified the buffer.")
;;;###autoload
(cl-defun org-element-cache-map (func &key (granularity 'headline+inlinetask) restrict-elements
                                      next-re fail-re from-pos (to-pos (point-max-marker)) after-element limit-count
                                      narrow)
  "Map all elements in current buffer with FUNC according to
GRANULARITY.  Collect non-nil return values into result list.

FUNC should accept a single argument - the element.

FUNC can modify the buffer, but doing so may reduce performance.  If
buffer is modified, the mapping will continue from an element starting
after the last mapped element.  If the last mapped element is deleted,
the subsequent element will be skipped as it cannot be distinguished
deterministically from a changed element.  If FUNC is expected to
delete the element, it should directly set the value of
`org-element-cache-map-continue-from' to force `org-element-cache-map'
continue from the right point in buffer.

If some elements are not yet in cache, they will be added.

GRANULARITY can be `headline', `headline+inlinetask'
`greater-element', or `element'.  The default is
`headline+inlinetask'.  `object' granularity is not supported.

RESTRICT-ELEMENTS is a list of element types to be mapped over.

NEXT-RE is a regexp used to search next candidate match when FUNC
returns non-nil and to search the first candidate match.  FAIL-RE is a
regexp used to search next candidate match when FUNC returns nil.  The
mapping will continue starting from headline at the RE match.

FROM-POS and TO-POS are buffer positions.  When non-nil, they bound the
mapped elements to elements starting at of after FROM-POS but before
TO-POS.

AFTER-ELEMENT, when non-nil, bounds the mapping to all the elements
after AFTER-ELEMENT (i.e. if AFTER-ELEMENT is a headline section, we
map all the elements starting from first element inside section, but
not including the section).

LIMIT-COUNT limits mapping to that many first matches where FUNC
returns non-nil.

NARROW controls whether current buffer narrowing should be preserved.

This function does a subset of what `org-element-map' does, but with
much better performance.  Cached elements are supplied as the single
argument of FUNC.  Changes to elements made in FUNC will also alter
the cache."
  (unless (org-element--cache-active-p)
    (error "Cache must be active."))
  (unless (memq granularity '( headline headline+inlinetask
                               greater-element element))
    (error "Unsupported granularity: %S" granularity))
  ;; Make TO-POS marker.  Otherwise, buffer edits may garble the the
  ;; process.
  (unless (markerp to-pos)
    (let ((mk (make-marker)))
      (set-marker mk to-pos)
      (setq to-pos mk)))
  (let (;; Bind variables used inside loop to avoid memory
        ;; re-allocation on every iteration.
        ;; See https://emacsconf.org/2021/talks/faster/
        tmpnext-start tmpparent tmpelement)
    (save-excursion
      (save-restriction
        (unless narrow (widen))
        ;; Synchronize cache up to the end of mapped region.
        (org-element-at-point to-pos)
        (cl-macrolet ((cache-root
                        ;; Use the most optimal version of cache available.
                        () `(org-with-base-buffer nil
                              (if (memq granularity '(headline headline+inlinetask))
                                  (org-element--headline-cache-root)
                                (org-element--cache-root))))
                      (cache-size
                        ;; Use the most optimal version of cache available.
                        () `(org-with-base-buffer nil
                              (if (memq granularity '(headline headline+inlinetask))
                                  org-element--headline-cache-size
                                org-element--cache-size)))
                      (cache-walk-restart
                        ;; Restart tree traversal after AVL tree re-balance.
                        () `(when node
                              (org-element-at-point (point-max))
                              (setq node (cache-root)
		                    stack (list nil)
		                    leftp t
		                    continue-flag t)))
                      (cache-walk-abort
                        ;; Abort tree traversal.
                        () `(setq continue-flag t
                                  node nil))
                      (element-match-at-point
                        ;; Returning the first element to match around point.
                        ;; For example, if point is inside headline and
                        ;; granularity is restricted to headlines only, skip
                        ;; over all the child elements inside the headline
                        ;; and return the first parent headline.
                        ;; When we are inside a cache gap, calling
                        ;; `org-element-at-point' also fills the cache gap down to
                        ;; point.
                        () `(progn
                              ;; Parsing is one of the performance
                              ;; bottlenecks.  Make sure to optimize it as
                              ;; much as possible.
                              ;;
                              ;; Avoid extra staff like timer cancels et al
                              ;; and only call `org-element--cache-sync-requests' when
                              ;; there are pending requests.
                              (org-with-base-buffer nil
                                (when org-element--cache-sync-requests
                                  (org-element--cache-sync (current-buffer))))
                              ;; Call `org-element--parse-to' directly avoiding any
                              ;; kind of `org-element-at-point' overheads.
                              (if restrict-elements
                                  ;; Search directly instead of calling
                                  ;; `org-element-lineage' to avoid funcall overheads
                                  ;; and making sure that we do not go all
                                  ;; the way to `org-data' as `org-element-lineage'
                                  ;; does.
                                  (progn
                                    (setq tmpelement (org-element--parse-to (point)))
                                    (while (and tmpelement (not (memq (org-element-type tmpelement) restrict-elements)))
                                      (setq tmpelement (org-element-property :parent tmpelement)))
                                    tmpelement)
                                (org-element--parse-to (point)))))
                      ;; Starting from (point), search RE and move START to
                      ;; the next valid element to be matched according to
                      ;; restriction.  Abort cache walk if no next element
                      ;; can be found.  When RE is nil, just find element at
                      ;; point.
                      (move-start-to-next-match
                        (re) `(save-match-data
                                (if (or (not ,re)
                                        (if org-element--cache-map-statistics
                                            (progn
                                              (setq before-time (float-time))
                                              (re-search-forward (or (car-safe ,re) ,re) nil 'move)
                                              (cl-incf re-search-time
                                                       (- (float-time)
                                                          before-time)))
                                          (re-search-forward (or (car-safe ,re) ,re) nil 'move)))
                                    (unless (or (< (point) (or start -1))
                                                (and data
                                                     (< (point) (org-element-property :begin data))))
                                      (if (cdr-safe ,re)
                                          ;; Avoid parsing when we are 100%
                                          ;; sure that regexp is good enough
                                          ;; to find new START.
                                          (setq start (match-beginning 0))
                                        (setq start (max (or start -1)
                                                         (or (org-element-property :begin data) -1)
                                                         (or (org-element-property :begin (element-match-at-point)) -1))))
                                      (when (>= start to-pos) (cache-walk-abort))
                                      (when (eq start -1) (setq start nil)))
                                  (cache-walk-abort))))
                      ;; Find expected begin position of an element after
                      ;; DATA.
                      (next-element-start
                        () `(progn
                              (setq tmpnext-start nil)
                              (if (memq granularity '(headline headline+inlinetask))
                                  (setq tmpnext-start (or (when (memq (org-element-type data) '(headline org-data))
                                                            (org-element-property :contents-begin data))
                                                          (org-element-property :end data)))
		                (setq tmpnext-start (or (when (memq (org-element-type data) org-element-greater-elements)
                                                          (org-element-property :contents-begin data))
                                                        (org-element-property :end data))))
                              ;; DATA end may be the last element inside
                              ;; i.e. source block.  Skip up to the end
                              ;; of parent in such case.
                              (setq tmpparent data)
		              (catch :exit
                                (when (eq tmpnext-start (org-element-property :contents-end tmpparent))
			          (setq tmpnext-start (org-element-property :end tmpparent)))
			        (while (setq tmpparent (org-element-property :parent tmpparent))
			          (if (eq tmpnext-start (org-element-property :contents-end tmpparent))
			              (setq tmpnext-start (org-element-property :end tmpparent))
                                    (throw :exit t))))
                              tmpnext-start))
                      ;; Check if cache does not have gaps.
                      (cache-gapless-p
                        () `(org-with-base-buffer nil
                              (eq org-element--cache-change-tic
                                  (alist-get granularity org-element--cache-gapless)))))
          ;; The core algorithm is simple walk along binary tree.  However,
          ;; instead of checking all the tree elements from first to last
          ;; (like in `avl-tree-mapcar'), we begin from FROM-POS skipping
          ;; the elements before FROM-POS efficiently: O(logN) instead of
          ;; O(Nbefore).
          ;;
          ;; Later, we may also not check every single element in the
          ;; binary tree after FROM-POS.  Instead, we can find position of
          ;; next candidate elements by means of regexp search and skip the
          ;; binary tree branches that are before the next candidate:
          ;; again, O(logN) instead of O(Nbetween).
          ;;
          ;; Some elements might not yet be in the tree.  So, we also parse
          ;; the empty gaps in cache as needed making sure that we do not
          ;; miss anything.
          (let* (;; START is always beginning of an element.  When there is
                 ;; no element in cache at START, we are inside cache gap
                 ;; and need to fill it.
                 (start (and from-pos
                             (progn
                               (goto-char from-pos)
                               (org-element-property :begin (element-match-at-point)))))
                 ;; Some elements may start at the same position, so we
                 ;; also keep track of the last processed element and make
                 ;; sure that we do not try to search it again.
                 (prev after-element)
                 (node (cache-root))
                 data
                 (stack (list nil))
                 (leftp t)
                 result
                 ;; Whether previous element matched FUNC (FUNC
                 ;; returned non-nil).
                 (last-match t)
                 continue-flag
                 ;; Generic regexp to search next potential match.  If it
                 ;; is a cons of (regexp . 'match-beg), we are 100% sure
                 ;; that the match beginning is the existing element
                 ;; beginning.
                 (next-element-re (pcase granularity
                                    ((or `headline
                                         (guard (equal '(headline)
                                                       restrict-elements)))
                                     (cons
                                      (org-with-limited-levels
                                       org-element-headline-re)
                                      'match-beg))
                                    (`headline+inlinetask
                                     (cons
                                      (if (equal '(inlinetask) restrict-elements)
                                          (org-inlinetask-outline-regexp)
                                        org-element-headline-re)
                                      'match-beg))
                                    ;; TODO: May add other commonly
                                    ;; searched elements as needed.
                                    (_)))
                 ;; Make sure that we are not checking the same regexp twice.
                 (next-re (unless (and next-re
                                       (string= next-re
                                                (or (car-safe next-element-re)
                                                    next-element-re)))
                            next-re))
                 (fail-re (unless (and fail-re
                                       (string= fail-re
                                                (or (car-safe next-element-re)
                                                    next-element-re)))
                            fail-re))
                 (restrict-elements (or restrict-elements
                                        (pcase granularity
                                          (`headline
                                           '(headline))
                                          (`headline+inlinetask
                                           '(headline inlinetask))
                                          (`greater-element
                                           org-element-greater-elements)
                                          (_ nil))))
                 ;; Statistics
                 (time (float-time))
                 (predicate-time 0)
                 (pre-process-time 0)
                 (re-search-time 0)
                 (count-predicate-calls-match 0)
                 (count-predicate-calls-fail 0)
                 ;; Bind variables used inside loop to avoid memory
                 ;; re-allocation on every iteration.
                 ;; See https://emacsconf.org/2021/talks/faster/
                 cache-size before-time modified-tic)
            ;; Skip to first element within region.
            (goto-char (or start (point-min)))
            (move-start-to-next-match next-element-re)
            (unless (and start (>= start to-pos))
              (while node
                (setq data (avl-tree--node-data node))
                (if (and leftp (avl-tree--node-left node) ; Left branch.
                         ;; Do not move to left branch when we are before
                         ;; PREV.
		         (or (not prev)
		             (not (org-element--cache-key-less-p
		                   (org-element--cache-key data)
			           (org-element--cache-key prev))))
                         ;; ... or when we are before START.
                         (or (not start)
                             (not (> start (org-element-property :begin data)))))
	            (progn (push node stack)
		           (setq node (avl-tree--node-left node)))
                  ;; The whole tree left to DATA is before START and
                  ;; PREV.  DATA may still be before START (i.e. when
                  ;; DATA is the root or when START moved), at START, or
                  ;; after START.
                  ;;
                  ;; If DATA is before start, skip it over and move to
                  ;; subsequent elements.
                  ;; If DATA is at start, run FUNC if necessary and
                  ;; update START according and NEXT-RE, FAIL-RE,
                  ;; NEXT-ELEMENT-RE.
                  ;; If DATA is after start, we have found a cache gap
                  ;; and need to fill it.
                  (unless (or (and start (< (org-element-property :begin data) start))
		              (and prev (not (org-element--cache-key-less-p
				              (org-element--cache-key prev)
				              (org-element--cache-key data)))))
                    ;; DATA is at of after START and PREV.
	            (if (or (not start) (= (org-element-property :begin data) start))
                        ;; DATA is at START.  Match it.
                        ;; In the process, we may alter the buffer,
                        ;; so also keep track of the cache state.
                        (progn
                          (setq modified-tic
                                (org-with-base-buffer nil
                                  org-element--cache-change-tic))
                          (setq cache-size (cache-size))
                          ;; When NEXT-RE/FAIL-RE is provided, skip to
                          ;; next regexp match after :begin of the current
                          ;; element.
                          (when (if last-match next-re fail-re)
                            (goto-char (org-element-property :begin data))
                            (move-start-to-next-match
                             (if last-match next-re fail-re)))
                          (when (and (or (not start) (eq (org-element-property :begin data) start))
                                     (< (org-element-property :begin data) to-pos))
                            ;; Calculate where next possible element
                            ;; starts and update START if needed.
		            (setq start (next-element-start))
                            (goto-char start)
                            ;; Move START further if possible.
                            (when (and next-element-re
                                       ;; Do not move if we know for
                                       ;; sure that cache does not
                                       ;; contain gaps.  Regexp
                                       ;; searches are not cheap.
                                       (not (cache-gapless-p)))
                              (move-start-to-next-match next-element-re)
                              ;; Make sure that point is at START
                              ;; before running FUNC.
                              (goto-char start))
                            ;; Try FUNC if DATA matches all the
                            ;; restrictions.  Calculate new START.
                            (when (or (not restrict-elements)
                                      (memq (org-element-type data) restrict-elements))
                              ;; DATA matches restriction.  FUNC may
                              ;;
                              ;; Call FUNC.  FUNC may move point.
                              (setq org-element-cache-map-continue-from nil)
                              (if (org-with-base-buffer nil org-element--cache-map-statistics)
                                  (progn
                                    (setq before-time (float-time))
                                    (push (funcall func data) result)
                                    (cl-incf predicate-time
                                             (- (float-time)
                                                before-time))
                                    (if (car result)
                                        (cl-incf count-predicate-calls-match)
                                      (cl-incf count-predicate-calls-fail)))
                                (push (funcall func data) result)
                                (when (car result) (cl-incf count-predicate-calls-match)))
                              ;; Set `last-match'.
                              (setq last-match (car result))
                              ;; If FUNC moved point forward, update
                              ;; START.
                              (when org-element-cache-map-continue-from
                                (goto-char org-element-cache-map-continue-from))
                              (when (> (point) start)
                                (move-start-to-next-match nil)
                                ;; (point) inside matching element.
                                ;; Go further.
                                (when (> (point) start)
                                  (setq data (element-match-at-point))
                                  (if (not data)
                                      (cache-walk-abort)
                                    (goto-char (next-element-start))
                                    (move-start-to-next-match next-element-re))))
                              ;; Drop nil.
                              (unless (car result) (pop result)))
                            ;; If FUNC did not move the point and we
                            ;; know for sure that cache does not contain
                            ;; gaps, do not try to calculate START in
                            ;; advance but simply loop to the next cache
                            ;; element.
                            (when (and (cache-gapless-p)
                                       (eq (next-element-start)
                                           start))
                              (setq start nil))
                            ;; Check if the buffer has been modified.
                            (unless (org-with-base-buffer nil
                                      (and (eq modified-tic org-element--cache-change-tic)
                                           (eq cache-size (cache-size))))
                              ;; START may no longer be valid, update
                              ;; it to beginning of real element.
                              ;; Upon modification, START may lay
                              ;; inside an element.  We want to move
                              ;; it to real beginning then despite
                              ;; START being larger.
                              (setq start nil)
                              (let ((data nil)) ; data may not be valid. ignore it.
                                (move-start-to-next-match nil))
                              ;; The new element may now start before
                              ;; or at already processed position.
                              ;; Make sure that we continue from an
                              ;; element past already processed
                              ;; place.
                              (when (and start
                                         (<= start (org-element-property :begin data))
                                         (not org-element-cache-map-continue-from))
                                (goto-char start)
                                (setq data (element-match-at-point))
                                ;; If DATA is nil, buffer is
                                ;; empty. Abort.
                                (when data
                                  (goto-char (next-element-start))
                                  (move-start-to-next-match next-element-re)))
                              (org-element-at-point to-pos)
                              (cache-walk-restart))
                            ;; Reached LIMIT-COUNT.  Abort.
                            (when (and limit-count
                                       (>= count-predicate-calls-match
                                           limit-count))
                              (cache-walk-abort))
                            (if (org-element-property :cached data)
		                (setq prev data)
                              (setq prev nil))))
                      ;; DATA is after START.  Fill the gap.
                      (if (memq (org-element-type (org-element--parse-to start)) '(plain-list table))
                          ;; Tables and lists are special, we need a
                          ;; trickery to make items/rows be populated
                          ;; into cache.
                          (org-element--parse-to (1+ start)))
                      ;; Restart tree traversal as AVL tree is
                      ;; re-balanced upon adding elements.  We can no
                      ;; longer trust STACK.
                      (cache-walk-restart)))
                  ;; Second, move to the right branch of the tree or skip
                  ;; it altogether.
                  (if continue-flag
	              (setq continue-flag nil)
	            (setq node (if (and (car stack)
                                        ;; If START advanced beyond stack parent, skip the right branch.
                                        (or (and start (< (org-element-property :begin (avl-tree--node-data (car stack))) start))
		                            (and prev (org-element--cache-key-less-p
				                       (org-element--cache-key (avl-tree--node-data (car stack)))
                                                       (org-element--cache-key prev)))))
                                   (progn
                                     (setq leftp nil)
                                     (pop stack))
                                 ;; Otherwise, move ahead into the right
                                 ;; branch when it exists.
                                 (if (setq leftp (avl-tree--node-right node))
		                     (avl-tree--node-right node)
		                   (pop stack))))))))
            (when (and org-element--cache-map-statistics
                       (or (not org-element--cache-map-statistics-threshold)
                           (> (- (float-time) time) org-element--cache-map-statistics-threshold)))
              (message "Mapped over elements in %S. %d/%d predicate matches. Total time: %f sec. Pre-process time: %f sec. Predicate time: %f sec. Re-search time: %f sec.
       Calling parameters: :granularity %S :restrict-elements %S :next-re %S :fail-re %S :from-pos %S :to-pos %S :limit-count %S :after-element %S"
                       (current-buffer)
                       count-predicate-calls-match
                       (+ count-predicate-calls-match
                          count-predicate-calls-fail)
                       (- (float-time) time)
                       pre-process-time
                       predicate-time
                       re-search-time
                       granularity restrict-elements next-re fail-re from-pos to-pos limit-count after-element))
            ;; Return result.
            (nreverse result)))))))




;;; The Toolbox
;;
;; The first move is to implement a way to obtain the smallest element
;; containing point.  This is the job of `org-element-at-point'.  It
;; basically jumps back to the beginning of section containing point
;; and proceed, one element after the other, with
;; `org-element--current-element' until the container is found.  Note:
;; When using `org-element-at-point', secondary values are never
;; parsed since the function focuses on elements, not on objects.
;;
;; At a deeper level, `org-element-context' lists all elements and
;; objects containing point.
;;
;; `org-element-nested-p' and `org-element-swap-A-B' may be used
;; internally by navigation and manipulation tools.


;;;###autoload
(defun org-element-at-point (&optional pom cached-only)
  "Determine closest element around point or POM.

Only check cached element when CACHED-ONLY is non-nil and return nil
unconditionally when element at POM is not in cache.

Return value is a list like (TYPE PROPS) where TYPE is the type
of the element and PROPS a plist of properties associated to the
element.

Possible types are defined in `org-element-all-elements'.
Properties depend on element or object type, but always include
`:begin', `:end', and `:post-blank' properties.

As a special case, if point is at the very beginning of the first
item in a list or sub-list, returned element will be that list
instead of the item.  Likewise, if point is at the beginning of
the first row of a table, returned element will be the table
instead of the first row.

When point is at the end of the buffer, return the innermost
element ending there."
  (setq pom (or pom (point)))
  ;; Allow re-parsing when the command can benefit from it.
  (when (and cached-only
             (memq this-command org-element--cache-non-modifying-commands))
    (setq cached-only nil))
  (let (element)
    (when (org-element--cache-active-p)
      (if (not org-element--cache) (org-element-cache-reset)
        (unless cached-only (org-element--cache-sync (current-buffer) pom))))
    (setq element (if cached-only
                      (when (and (org-element--cache-active-p)
                                 (or (not org-element--cache-sync-requests)
                                     (< pom
                                        (org-element--request-beg
                                         (car org-element--cache-sync-requests)))))
                        (org-element--cache-find pom))
                    (condition-case err
                        (org-element--parse-to pom)
                      (error
                       (org-element--cache-warn
                        "Org parser error in %s::%S. Resetting.\n The error was: %S\n Backtrace:\n%S\n Please report this to Org mode mailing list (M-x org-submit-bug-report)."
                        (buffer-name (current-buffer))
                        pom
                        err
                        (when (and (fboundp 'backtrace-get-frames)
                                   (fboundp 'backtrace-to-string))
                          (backtrace-to-string (backtrace-get-frames 'backtrace))))
                       (org-element-cache-reset)
                       (org-element--parse-to pom)))))
    (when (and (org-element--cache-active-p)
               element
               (org-element--cache-verify-element element))
      (setq element (org-element--parse-to pom)))
    (unless (eq 'org-data (org-element-type element))
      (unless (and cached-only
                   (not (and element
                             (or (= pom (org-element-property :begin element))
                                 (and (not (memq (org-element-type element) org-element-greater-elements))
                                      (>= pom (org-element-property :begin element))
                                      (< pom (org-element-property :end element)))
                                 (and (org-element-property :contents-begin element)
                                      (>= pom (org-element-property :begin element))
                                      (< pom (org-element-property :contents-begin element)))
                                 (and (not (org-element-property :contents-end element))
                                      (>= pom (org-element-property :begin element))
                                      (< pom (org-element-property :end element)))))))
        (if (not (eq (org-element-type element) 'section))
            element
          (org-element-at-point (1+ pom) cached-only))))))

;;;###autoload
(defsubst org-element-at-point-no-context (&optional pom)
  "Quickly find element at point or POM.

It is a faster version of `org-element-at-point' that is not
guaranteed to return correct `:parent' properties even when cache is
enabled."
  (or (org-element-at-point pom 'cached-only)
      (let (org-element-use-cache) (org-element-at-point pom))))

;;;###autoload
(defun org-element-context (&optional element)
  "Return smallest element or object around point.

Return value is a list like (TYPE PROPS) where TYPE is the type
of the element or object and PROPS a plist of properties
associated to it.

Possible types are defined in `org-element-all-elements' and
`org-element-all-objects'.  Properties depend on element or
object type, but always include `:begin', `:end', `:parent' and
`:post-blank'.

As a special case, if point is right after an object and not at
the beginning of any other object, return that object.

Optional argument ELEMENT, when non-nil, is the closest element
containing point, as returned by `org-element-at-point'.
Providing it allows for quicker computation."
  (save-match-data
    (catch 'objects-forbidden
      (org-with-wide-buffer
       (let* ((pos (point))
	      (element (or element (org-element-at-point)))
	      (type (org-element-type element))
	      (post (org-element-property :post-affiliated element)))
         ;; If point is inside an element containing objects or
         ;; a secondary string, narrow buffer to the container and
         ;; proceed with parsing.  Otherwise, return ELEMENT.
         (cond
	  ;; At a parsed affiliated keyword, check if we're inside main
	  ;; or dual value.
	  ((and post (< pos post))
	   (beginning-of-line)
	   (let ((case-fold-search t)) (looking-at org-element--affiliated-re))
	   (cond
	    ((not (member-ignore-case (match-string 1)
				    org-element-parsed-keywords))
	     (throw 'objects-forbidden element))
	    ((< (match-end 0) pos)
	     (narrow-to-region (match-end 0) (line-end-position)))
	    ((and (match-beginning 2)
		  (>= pos (match-beginning 2))
		  (< pos (match-end 2)))
	     (narrow-to-region (match-beginning 2) (match-end 2)))
	    (t (throw 'objects-forbidden element)))
	   ;; Also change type to retrieve correct restrictions.
	   (setq type 'keyword))
	  ;; At an item, objects can only be located within tag, if any.
	  ((eq type 'item)
	   (let ((tag (org-element-property :tag element)))
	     (if (or (not tag) (/= (line-beginning-position) post))
	         (throw 'objects-forbidden element)
	       (beginning-of-line)
	       (search-forward tag (line-end-position))
	       (goto-char (match-beginning 0))
	       (if (and (>= pos (point)) (< pos (match-end 0)))
		   (narrow-to-region (point) (match-end 0))
	         (throw 'objects-forbidden element)))))
	  ;; At an headline or inlinetask, objects are in title.
	  ((memq type '(headline inlinetask))
	   (let ((case-fold-search nil))
	     (goto-char (org-element-property :begin element))
	     (looking-at org-complex-heading-regexp)
	     (let ((end (match-end 4)))
	       (if (not end) (throw 'objects-forbidden element)
	         (goto-char (match-beginning 4))
	         (when (looking-at org-element-comment-string)
		   (goto-char (match-end 0)))
	         (if (>= (point) end) (throw 'objects-forbidden element)
		   (narrow-to-region (point) end))))))
	  ;; At a paragraph, a table-row or a verse block, objects are
	  ;; located within their contents.
	  ((memq type '(paragraph table-row verse-block))
	   (let ((cbeg (org-element-property :contents-begin element))
	         (cend (org-element-property :contents-end element)))
	     ;; CBEG is nil for table rules.
	     (if (and cbeg cend (>= pos cbeg)
		      (or (< pos cend) (and (= pos cend) (eobp))))
	         (narrow-to-region cbeg cend)
	       (throw 'objects-forbidden element))))
	  (t (throw 'objects-forbidden element)))
         (goto-char (point-min))
         (let ((restriction (org-element-restriction type))
	       (parent element)
	       last)
	   (catch 'exit
	     (while t
	       (let ((next (org-element--object-lex restriction)))
	         (when next (org-element-put-property next :parent parent))
	         ;; Process NEXT, if any, in order to know if we need to
	         ;; skip it, return it or move into it.
	         (if (or (not next) (> (org-element-property :begin next) pos))
		     (throw 'exit (or last parent))
		   (let ((end (org-element-property :end next))
		         (cbeg (org-element-property :contents-begin next))
		         (cend (org-element-property :contents-end next)))
		     (cond
		      ;; Skip objects ending before point.  Also skip
		      ;; objects ending at point unless it is also the
		      ;; end of buffer, since we want to return the
		      ;; innermost object.
		      ((and (<= end pos) (/= (point-max) end))
		       (goto-char end)
		       ;; For convenience, when object ends at POS,
		       ;; without any space, store it in LAST, as we
		       ;; will return it if no object starts here.
		       (when (and (= end pos)
				  (not (memq (char-before) '(?\s ?\t))))
		         (setq last next)))
		      ;; If POS is within a container object, move into
		      ;; that object.
		      ((and cbeg cend
			    (>= pos cbeg)
			    (or (< pos cend)
			        ;; At contents' end, if there is no
			        ;; space before point, also move into
			        ;; object, for consistency with
			        ;; convenience feature above.
			        (and (= pos cend)
				     (or (= (point-max) pos)
				         (not (memq (char-before pos)
					            '(?\s ?\t)))))))
		       (goto-char cbeg)
		       (narrow-to-region (point) cend)
		       (setq parent next)
		       (setq restriction (org-element-restriction next)))
		      ;; Otherwise, return NEXT.
		      (t (throw 'exit next))))))))))))))

(defun org-element-lineage (datum &optional types with-self)
  "List all ancestors of a given element or object.

DATUM is an object or element.

Return ancestors from the closest to the farthest.  When optional
argument TYPES is a list of symbols, return the first element or
object in the lineage whose type belongs to that list instead.

When optional argument WITH-SELF is non-nil, lineage includes
DATUM itself as the first element, and TYPES, if provided, also
apply to it.

When DATUM is obtained through `org-element-context' or
`org-element-at-point', only ancestors from its section can be
found.  There is no such limitation when DATUM belongs to a full
parse tree."
  (let ((up (if with-self datum (org-element-property :parent datum)))
	ancestors)
    (while (and up (not (memq (org-element-type up) types)))
      (unless types (push up ancestors))
      (setq up (org-element-property :parent up)))
    (if types up (nreverse ancestors))))

(defun org-element-nested-p (elem-A elem-B)
  "Non-nil when elements ELEM-A and ELEM-B are nested."
  (let ((beg-A (org-element-property :begin elem-A))
	(beg-B (org-element-property :begin elem-B))
	(end-A (org-element-property :end elem-A))
	(end-B (org-element-property :end elem-B)))
    (or (and (>= beg-A beg-B) (<= end-A end-B))
	(and (>= beg-B beg-A) (<= end-B end-A)))))

(defun org-element-swap-A-B (elem-A elem-B)
  "Swap elements ELEM-A and ELEM-B.
Assume ELEM-B is after ELEM-A in the buffer.  Leave point at the
end of ELEM-A."
  (goto-char (org-element-property :begin elem-A))
  ;; There are two special cases when an element doesn't start at bol:
  ;; the first paragraph in an item or in a footnote definition.
  (let ((specialp (not (bolp))))
    ;; Only a paragraph without any affiliated keyword can be moved at
    ;; ELEM-A position in such a situation.  Note that the case of
    ;; a footnote definition is impossible: it cannot contain two
    ;; paragraphs in a row because it cannot contain a blank line.
    (when (and specialp
	       (or (not (eq (org-element-type elem-B) 'paragraph))
		   (/= (org-element-property :begin elem-B)
		      (org-element-property :contents-begin elem-B))))
      (error "Cannot swap elements"))
    ;; Preserve folding state when `org-fold-core-style' is set to
    ;; `text-properties'.
    (org-fold-core-ignore-modifications
      ;; In a special situation, ELEM-A will have no indentation.  We'll
      ;; give it ELEM-B's (which will in, in turn, have no indentation).
      (let* ((ind-B (when specialp
		      (goto-char (org-element-property :begin elem-B))
		      (current-indentation)))
	     (beg-A (org-element-property :begin elem-A))
	     (end-A (save-excursion
		      (goto-char (org-element-property :end elem-A))
		      (skip-chars-backward " \r\t\n")
		      (line-end-position)))
	     (beg-B (org-element-property :begin elem-B))
	     (end-B (save-excursion
		      (goto-char (org-element-property :end elem-B))
		      (skip-chars-backward " \r\t\n")
		      (line-end-position)))
	     ;; Store inner folds responsible for visibility status.
	     (folds
	      (cons
               (org-fold-core-get-regions :from beg-A :to end-A :relative t)
               (org-fold-core-get-regions :from beg-B :to end-B :relative t)))
	     ;; Get contents.
	     (body-A (buffer-substring beg-A end-A))
	     (body-B (buffer-substring beg-B end-B)))
        ;; Clear up the folds.
        (org-fold-region beg-A end-A nil)
        (org-fold-region beg-B end-B nil)
        (delete-region beg-B end-B)
        (goto-char beg-B)
        (when specialp
	  (setq body-B (replace-regexp-in-string "\\`[ \t]*" "" body-B))
	  (indent-to-column ind-B))
        (insert body-A)
        ;; Restore ex ELEM-A folds.
        (org-fold-core-regions (car folds) :relative beg-B)
	(goto-char beg-A)
	(delete-region beg-A end-A)
	(insert body-B)
        ;; Restore ex ELEM-A folds.
        (org-fold-core-regions (cdr folds) :relative beg-A)
        (goto-char (org-element-property :end elem-B))))))

(provide 'org-element)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-element.el ends here
