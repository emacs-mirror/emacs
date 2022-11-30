;;; org-lint.el --- Linting for Org documents        -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library implements linting for Org syntax.  The process is
;; started by calling `org-lint' command, which see.

;; New checkers are added by `org-lint-add-checker' function.
;; Internally, all checks are listed in `org-lint--checkers'.

;; Results are displayed in a special "*Org Lint*" buffer with
;; a dedicated major mode, derived from `tabulated-list-mode'.
;; In addition to the usual key-bindings inherited from it, "C-j" and
;; "TAB" display problematic line reported under point whereas "RET"
;; jumps to it.  Also, "h" hides all reports similar to the current
;; one.  Additionally, "i" removes them from subsequent reports.

;; Checks currently implemented report the following:

;; - duplicates CUSTOM_ID properties,
;; - duplicate NAME values,
;; - duplicate targets,
;; - duplicate footnote definitions,
;; - orphaned affiliated keywords,
;; - obsolete affiliated keywords,
;; - deprecated export block syntax,
;; - deprecated Babel header syntax,
;; - missing language in source blocks,
;; - missing back-end in export blocks,
;; - invalid Babel call blocks,
;; - NAME values with a colon,
;; - wrong babel headers,
;; - invalid value in babel headers,
;; - misuse of CATEGORY keyword,
;; - "coderef" links with unknown destination,
;; - "custom-id" links with unknown destination,
;; - "fuzzy" links with unknown destination,
;; - "id" links with unknown destination,
;; - links to non-existent local files,
;; - SETUPFILE keywords with non-existent file parameter,
;; - INCLUDE keywords with misleading link parameter,
;; - obsolete markup in INCLUDE keyword,
;; - unknown items in OPTIONS keyword,
;; - spurious macro arguments or invalid macro templates,
;; - special properties in properties drawers,
;; - obsolete syntax for properties drawers,
;; - invalid duration in EFFORT property,
;; - missing definition for footnote references,
;; - missing reference for footnote definitions,
;; - non-footnote definitions in footnote section,
;; - probable invalid keywords,
;; - invalid blocks,
;; - misplaced planning info line,
;; - probable incomplete drawers,
;; - probable indented diary-sexps,
;; - obsolete QUOTE section,
;; - obsolete "file+application" link,
;; - obsolete escape syntax in links,
;; - spurious colons in tags,
;; - invalid bibliography file,
;; - missing "print_bibliography" keyword,
;; - invalid value for "cite_export" keyword,
;; - incomplete citation object.


;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ob)
(require 'oc)
(require 'ol)
(require 'org-attach)
(require 'org-macro)
(require 'org-fold)
(require 'ox)
(require 'seq)


;;; Checkers structure

(cl-defstruct (org-lint-checker (:copier nil))
  name summary function trust categories)

(defvar org-lint--checkers nil
  "List of all available checkers.
This list is populated by `org-lint-add-checker' function.")

;;;###autoload
(defun org-lint-add-checker (name summary fun &rest props)
  "Add a new checker for linter.

NAME is a unique check identifier, as a non-nil symbol.  SUMMARY
is a short description of the check, as a string.

The check is done calling the function FUN with one mandatory
argument, the parse tree describing the current Org buffer.  Such
function calls are wrapped within a `save-excursion' and point is
always at `point-min'.  Its return value has to be an
alist (POSITION MESSAGE) where POSITION refer to the buffer
position of the error, as an integer, and MESSAGE is a one-line
string describing the error.

Optional argument PROPS provides additional information about the
checker.  Currently, two properties are supported:

  `:categories'

     Categories relative to the check, as a list of symbol.  They
     are used for filtering when calling `org-lint'.  Checkers
     not explicitly associated to a category are collected in the
     `default' one.

  `:trust'

    The trust level one can have in the check.  It is either
    `low' or `high', depending on the heuristics implemented and
    the nature of the check.  This has an indicative value only
    and is displayed along reports."
  (declare (indent 1))
  ;; Sanity checks.
  (pcase name
    (`nil (error "Name field is mandatory for checkers"))
    ((pred symbolp) nil)
    (_ (error "Invalid type for name field")))
  (unless (functionp fun)
    (error "Checker field is expected to be a valid function"))
  ;; Install checker in `org-lint--checkers'; uniquify by name.
  (setq org-lint--checkers
        (cons (apply #'make-org-lint-checker
                     :name name
                     :summary summary
                     :function fun
                     props)
              (seq-remove (lambda (c) (eq name (org-lint-checker-name c)))
                          org-lint--checkers))))


;;; Reports UI

(defvar org-lint--report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'org-lint--jump-to-source)
    (define-key map (kbd "TAB") 'org-lint--show-source)
    (define-key map (kbd "C-j") 'org-lint--show-source)
    (define-key map (kbd "h") 'org-lint--hide-checker)
    (define-key map (kbd "i") 'org-lint--ignore-checker)
    map)
  "Local keymap for `org-lint--report-mode' buffers.")

(define-derived-mode org-lint--report-mode tabulated-list-mode "OrgLint"
  "Major mode used to display reports emitted during linting.
\\{org-lint--report-mode-map}"
  (setf tabulated-list-format
	`[("Line" 6
	   (lambda (a b)
	     (< (string-to-number (aref (cadr a) 0))
		(string-to-number (aref (cadr b) 0))))
	   :right-align t)
	  ("Trust" 5 t)
	  ("Warning" 0 t)])
  (tabulated-list-init-header))

(defun org-lint--generate-reports (buffer checkers)
  "Generate linting report for BUFFER.

CHECKERS is the list of checkers used.

Return an alist (ID [LINE TRUST DESCRIPTION CHECKER]), suitable
for `tabulated-list-printer'."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((ast (org-element-parse-buffer))
	    (id 0)
	    (last-line 1)
	    (last-pos 1))
	;; Insert unique ID for each report.  Replace buffer positions
	;; with line numbers.
	(mapcar
	 (lambda (report)
	   (list
	    (cl-incf id)
	    (apply #'vector
		   (cons
		    (progn
		      (goto-char (car report))
		      (beginning-of-line)
		      (prog1 (number-to-string
			      (cl-incf last-line
				       (count-lines last-pos (point))))
			(setf last-pos (point))))
		    (cdr report)))))
	 ;; Insert trust level in generated reports.  Also sort them
	 ;; by buffer position in order to optimize lines computation.
	 (sort (cl-mapcan
		(lambda (c)
		  (let ((trust (symbol-name (org-lint-checker-trust c))))
		    (mapcar
		     (lambda (report)
		       (list (car report) trust (nth 1 report) c))
		     (save-excursion
		       (funcall (org-lint-checker-function c)
			        ast)))))
		checkers)
	       #'car-less-than-car))))))

(defvar-local org-lint--source-buffer nil
  "Source buffer associated to current report buffer.")

(defvar-local org-lint--local-checkers nil
  "List of checkers used to build current report.")

(defun org-lint--refresh-reports ()
  (setq tabulated-list-entries
	(org-lint--generate-reports org-lint--source-buffer
				    org-lint--local-checkers))
  (tabulated-list-print))

(defun org-lint--current-line ()
  "Return current report line, as a number."
  (string-to-number (aref (tabulated-list-get-entry) 0)))

(defun org-lint--current-checker (&optional entry)
  "Return current report checker.
When optional argument ENTRY is non-nil, use this entry instead
of current one."
  (aref (if entry (nth 1 entry) (tabulated-list-get-entry)) 3))

(defun org-lint--display-reports (source checkers)
  "Display linting reports for buffer SOURCE.
CHECKERS is the list of checkers used."
  (let ((buffer (get-buffer-create "*Org Lint*")))
    (with-current-buffer buffer
      (org-lint--report-mode)
      (setf org-lint--source-buffer source)
      (setf org-lint--local-checkers checkers)
      (org-lint--refresh-reports)
      (add-hook 'tabulated-list-revert-hook #'org-lint--refresh-reports nil t))
    (pop-to-buffer buffer)))

(defun org-lint--jump-to-source ()
  "Move to source line that generated the report at point."
  (interactive)
  (let ((l (org-lint--current-line)))
    (switch-to-buffer-other-window org-lint--source-buffer)
    (org-goto-line l)
    (org-fold-show-set-visibility 'local)
    (recenter)))

(defun org-lint--show-source ()
  "Show source line that generated the report at point."
  (interactive)
  (let ((buffer (current-buffer)))
    (org-lint--jump-to-source)
    (switch-to-buffer-other-window buffer)))

(defun org-lint--hide-checker ()
  "Hide all reports from checker that generated the report at point."
  (interactive)
  (let ((c (org-lint--current-checker)))
    (setf tabulated-list-entries
	  (cl-remove-if (lambda (e) (equal c (org-lint--current-checker e)))
			tabulated-list-entries))
    (tabulated-list-print)))

(defun org-lint--ignore-checker ()
  "Ignore all reports from checker that generated the report at point.
Checker will also be ignored in all subsequent reports."
  (interactive)
  (setf org-lint--local-checkers
	(remove (org-lint--current-checker) org-lint--local-checkers))
  (org-lint--hide-checker))


;;; Main function

;;;###autoload
(defun org-lint (&optional arg)
  "Check current Org buffer for syntax mistakes.

By default, run all checkers.  With a `\\[universal-argument]' prefix ARG, \
select one
category of checkers only.  With a `\\[universal-argument] \
\\[universal-argument]' prefix, run one precise
checker by its name.

ARG can also be a list of checker names, as symbols, to run."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (when (called-interactively-p 'any)
    (message "Org linting process starting..."))
  (let ((checkers
	 (pcase arg
	   (`nil org-lint--checkers)
	   (`(4)
	    (let ((category
		   (completing-read
		    "Checker category: "
		    (mapcar #'org-lint-checker-categories org-lint--checkers)
		    nil t)))
	      (cl-remove-if-not
	       (lambda (c)
		 (assoc-string category (org-lint-checker-categories c)))
	       org-lint--checkers)))
	   (`(16)
	    (list
	     (let ((name (completing-read
			  "Checker name: "
			  (mapcar #'org-lint-checker-name org-lint--checkers)
			  nil t)))
	       (catch 'exit
		 (dolist (c org-lint--checkers)
		   (when (string= (org-lint-checker-name c) name)
		     (throw 'exit c)))))))
	   ((pred consp)
	    (cl-remove-if-not (lambda (c) (memq (org-lint-checker-name c) arg))
			      org-lint--checkers))
	   (_ (user-error "Invalid argument `%S' for `org-lint'" arg)))))
    (if (not (called-interactively-p 'any))
	(org-lint--generate-reports (current-buffer) checkers)
      (org-lint--display-reports (current-buffer) checkers)
      (message "Org linting process completed"))))


;;; Checker functions

(defun org-lint--collect-duplicates
    (ast type extract-key extract-position build-message)
  "Helper function to collect duplicates in parse tree AST.

EXTRACT-KEY is a function extracting key.  It is called with
a single argument: the element or object.  Comparison is done
with `equal'.

EXTRACT-POSITION is a function returning position for the report.
It is called with two arguments, the object or element, and the
key.

BUILD-MESSAGE is a function creating the report message.  It is
called with one argument, the key used for comparison."
  (let* (keys
	 originals
	 reports
	 (make-report
	  (lambda (position value)
	    (push (list position (funcall build-message value)) reports))))
    (org-element-map ast type
      (lambda (datum)
	(let ((key (funcall extract-key datum)))
	  (cond
	   ((not key))
	   ((assoc key keys) (cl-pushnew (assoc key keys) originals)
	    (funcall make-report (funcall extract-position datum key) key))
	   (t (push (cons key (funcall extract-position datum key)) keys))))))
    (dolist (e originals reports) (funcall make-report (cdr e) (car e)))))

(defun org-lint-duplicate-custom-id (ast)
  (org-lint--collect-duplicates
   ast
   'node-property
   (lambda (property)
     (and (org-string-equal-ignore-case
           "CUSTOM_ID" (org-element-property :key property))
	  (org-element-property :value property)))
   (lambda (property _) (org-element-property :begin property))
   (lambda (key) (format "Duplicate CUSTOM_ID property \"%s\"" key))))

(defun org-lint-duplicate-name (ast)
  (org-lint--collect-duplicates
   ast
   org-element-all-elements
   (lambda (datum) (org-element-property :name datum))
   (lambda (datum name)
     (goto-char (org-element-property :begin datum))
     (re-search-forward
      (format "^[ \t]*#\\+[A-Za-z]+:[ \t]*%s[ \t]*$" (regexp-quote name)))
     (match-beginning 0))
   (lambda (key) (format "Duplicate NAME \"%s\"" key))))

(defun org-lint-duplicate-target (ast)
  (org-lint--collect-duplicates
   ast
   'target
   (lambda (target) (split-string (org-element-property :value target)))
   (lambda (target _) (org-element-property :begin target))
   (lambda (key)
     (format "Duplicate target <<%s>>" (mapconcat #'identity key " ")))))

(defun org-lint-duplicate-footnote-definition (ast)
  (org-lint--collect-duplicates
   ast
   'footnote-definition
   (lambda (definition)  (org-element-property :label definition))
   (lambda (definition _) (org-element-property :post-affiliated definition))
   (lambda (key) (format "Duplicate footnote definition \"%s\"" key))))

(defun org-lint-orphaned-affiliated-keywords (ast)
  ;; Ignore orphan RESULTS keywords, which could be generated from
  ;; a source block returning no value.
  (let ((keywords (cl-set-difference org-element-affiliated-keywords
				     '("RESULT" "RESULTS")
				     :test #'equal)))
    (org-element-map ast 'keyword
      (lambda (k)
	(let ((key (org-element-property :key k)))
	  (and (or (let ((case-fold-search t))
		     (string-match-p "\\`ATTR_[-_A-Za-z0-9]+\\'" key))
		   (member key keywords))
	       (list (org-element-property :post-affiliated k)
		     (format "Orphaned affiliated keyword: \"%s\"" key))))))))

(defun org-lint-obsolete-affiliated-keywords (_)
  (let ((regexp (format "^[ \t]*#\\+%s:"
			(regexp-opt '("DATA" "LABEL" "RESNAME" "SOURCE"
				      "SRCNAME" "TBLNAME" "RESULT" "HEADERS")
				    t)))
	reports)
    (while (re-search-forward regexp nil t)
      (let ((key (upcase (match-string-no-properties 1))))
	(when (< (point)
		 (org-element-property :post-affiliated (org-element-at-point)))
	  (push
	   (list (line-beginning-position)
		 (format
		  "Obsolete affiliated keyword: \"%s\".  Use \"%s\" instead"
		  key
		  (pcase key
		    ("HEADERS" "HEADER")
		    ("RESULT" "RESULTS")
		    (_ "NAME"))))
	   reports))))
    reports))

(defun org-lint-deprecated-export-blocks (ast)
  (let ((deprecated '("ASCII" "BEAMER" "HTML" "LATEX" "MAN" "MARKDOWN" "MD"
		      "ODT" "ORG" "TEXINFO")))
    (org-element-map ast 'special-block
      (lambda (b)
	(let ((type (org-element-property :type b)))
	  (when (member-ignore-case type deprecated)
	    (list
	     (org-element-property :post-affiliated b)
	     (format
	      "Deprecated syntax for export block.  Use \"BEGIN_EXPORT %s\" \
instead"
	      type))))))))

(defun org-lint-deprecated-header-syntax (ast)
  (let* ((deprecated-babel-properties
	  ;; DIR is also used for attachments.
	  (delete "dir"
		  (mapcar (lambda (arg) (downcase (symbol-name (car arg))))
			  org-babel-common-header-args-w-values)))
	 (deprecated-re
	  (format "\\`%s[ \t]" (regexp-opt deprecated-babel-properties t))))
    (org-element-map ast '(keyword node-property)
      (lambda (datum)
	(let ((key (org-element-property :key datum)))
	  (pcase (org-element-type datum)
	    (`keyword
	     (let ((value (org-element-property :value datum)))
	       (and (string= key "PROPERTY")
		    (string-match deprecated-re value)
		    (list (org-element-property :begin datum)
			  (format "Deprecated syntax for \"%s\".  \
Use header-args instead"
				  (match-string-no-properties 1 value))))))
	    (`node-property
	     (and (member-ignore-case key deprecated-babel-properties)
		  (list
		   (org-element-property :begin datum)
		   (format "Deprecated syntax for \"%s\".  \
Use :header-args: instead"
			   key))))))))))

(defun org-lint-missing-language-in-src-block (ast)
  (org-element-map ast 'src-block
    (lambda (b)
      (unless (org-element-property :language b)
	(list (org-element-property :post-affiliated b)
	      "Missing language in source block")))))

(defun org-lint-missing-backend-in-export-block (ast)
  (org-element-map ast 'export-block
    (lambda (b)
      (unless (org-element-property :type b)
	(list (org-element-property :post-affiliated b)
	      "Missing back-end in export block")))))

(defun org-lint-invalid-babel-call-block (ast)
  (org-element-map ast 'babel-call
    (lambda (b)
      (cond
       ((not (org-element-property :call b))
	(list (org-element-property :post-affiliated b)
	      "Invalid syntax in babel call block"))
       ((let ((h (org-element-property :end-header b)))
	  (and h (string-match-p "\\`\\[.*\\]\\'" h)))
	(list
	 (org-element-property :post-affiliated b)
	 "Babel call's end header must not be wrapped within brackets"))))))

(defun org-lint-deprecated-category-setup (ast)
  (org-element-map ast 'keyword
    (let (category-flag)
      (lambda (k)
	(cond
	 ((not (string= (org-element-property :key k) "CATEGORY")) nil)
	 (category-flag
	  (list (org-element-property :post-affiliated k)
		"Spurious CATEGORY keyword.  Set :CATEGORY: property instead"))
	 (t (setf category-flag t) nil))))))

(defun org-lint-invalid-coderef-link (ast)
  (let ((info (list :parse-tree ast)))
    (org-element-map ast 'link
      (lambda (link)
	(let ((ref (org-element-property :path link)))
	  (and (equal (org-element-property :type link) "coderef")
	       (not (ignore-errors (org-export-resolve-coderef ref info)))
	       (list (org-element-property :begin link)
		     (format "Unknown coderef \"%s\"" ref))))))))

(defun org-lint-invalid-custom-id-link (ast)
  (let ((info (list :parse-tree ast)))
    (org-element-map ast 'link
      (lambda (link)
	(and (equal (org-element-property :type link) "custom-id")
	     (not (ignore-errors (org-export-resolve-id-link link info)))
	     (list (org-element-property :begin link)
		   (format "Unknown custom ID \"%s\""
			   (org-element-property :path link))))))))

(defun org-lint-invalid-fuzzy-link (ast)
  (let ((info (list :parse-tree ast)))
    (org-element-map ast 'link
      (lambda (link)
	(and (equal (org-element-property :type link) "fuzzy")
	     (not (ignore-errors (org-export-resolve-fuzzy-link link info)))
	     (list (org-element-property :begin link)
		   (format "Unknown fuzzy location \"%s\""
			   (let ((path (org-element-property :path link)))
			     (if (string-prefix-p "*" path)
				 (substring path 1)
			       path)))))))))

(defun org-lint-invalid-id-link (ast)
  (org-element-map ast 'link
    (lambda (link)
      (let ((id (org-element-property :path link)))
	(and (equal (org-element-property :type link) "id")
	     (not (org-id-find id))
	     (list (org-element-property :begin link)
		   (format "Unknown ID \"%s\"" id)))))))

(defun org-lint-special-property-in-properties-drawer (ast)
  (org-element-map ast 'node-property
    (lambda (p)
      (let ((key (org-element-property :key p)))
	(and (member-ignore-case key org-special-properties)
	     (list (org-element-property :begin p)
		   (format
		    "Special property \"%s\" found in a properties drawer"
		    key)))))))

(defun org-lint-obsolete-properties-drawer (ast)
  (org-element-map ast 'drawer
    (lambda (d)
      (when (equal (org-element-property :drawer-name d) "PROPERTIES")
	(let ((headline? (org-element-lineage d '(headline)))
	      (before
	       (mapcar #'org-element-type
		       (assq d (reverse (org-element-contents
					 (org-element-property :parent d)))))))
	  (list (org-element-property :post-affiliated d)
		(if (or (and headline? (member before '(nil (planning))))
			(and (null headline?) (member before '(nil (comment)))))
		    "Incorrect contents for PROPERTIES drawer"
		  "Incorrect location for PROPERTIES drawer")))))))

(defun org-lint-invalid-effort-property (ast)
  (org-element-map ast 'node-property
    (lambda (p)
      (when (equal "EFFORT" (org-element-property :key p))
	(let ((value (org-element-property :value p)))
	  (and (org-string-nw-p value)
	       (not (org-duration-p value))
	       (list (org-element-property :begin p)
		     (format "Invalid effort duration format: %S" value))))))))

(defun org-lint-link-to-local-file (ast)
  (org-element-map ast 'link
    (lambda (l)
      (let ((type (org-element-property :type l)))
	(pcase type
	  ((or "attachment" "file")
	   (let* ((path (org-element-property :path l))
		  (file (if (string= type "file")
			    path
                          (org-with-point-at (org-element-property :begin l)
			    (org-attach-expand path)))))
	     (and (not (file-remote-p file))
		  (not (file-exists-p file))
		  (list (org-element-property :begin l)
			(format (if (org-element-lineage l '(link))
				    "Link to non-existent image file %S \
in description"
				  "Link to non-existent local file %S")
                                file)))))
	  (_ nil))))))

(defun org-lint-non-existent-setupfile-parameter (ast)
  (org-element-map ast 'keyword
    (lambda (k)
      (when (equal (org-element-property :key k) "SETUPFILE")
	(let ((file (org-unbracket-string
			"\"" "\""
		      (org-element-property :value k))))
	  (and (not (org-url-p file))
	       (not (file-remote-p file))
	       (not (file-exists-p file))
	       (list (org-element-property :begin k)
		     (format "Non-existent setup file %S" file))))))))

(defun org-lint-wrong-include-link-parameter (ast)
  (org-element-map ast 'keyword
    (lambda (k)
      (when (equal (org-element-property :key k) "INCLUDE")
        (let* ((value (org-element-property :value k))
               (path
                (and (string-match "^\\(\".+?\"\\|\\S-+\\)[ \t]*" value)
                     (save-match-data
                       (org-strip-quotes (match-string 1 value))))))
          (if (not path)
              (list (org-element-property :post-affiliated k)
                    "Missing location argument in INCLUDE keyword")
            (let* ((file (org-string-nw-p
                          (if (string-match "::\\(.*\\)\\'" path)
                              (substring path 0 (match-beginning 0))
                            path)))
                   (search (and (not (equal file path))
                                (org-string-nw-p (match-string 1 path)))))
              (unless (org-url-p file)
                (if (and file
                         (not (file-remote-p file))
                         (not (file-exists-p file)))
                    (list (org-element-property :post-affiliated k)
                          "Non-existent file argument in INCLUDE keyword")
                  (let* ((visiting (if file (find-buffer-visiting file)
                                     (current-buffer)))
                         (buffer (or visiting (find-file-noselect file)))
                         (org-link-search-must-match-exact-headline t))
                    (unwind-protect
                        (with-current-buffer buffer
                          (when (and search
                                     (not (ignore-errors
                                            (org-link-search search nil t))))
                            (list (org-element-property :post-affiliated k)
                                  (format
                                   "Invalid search part \"%s\" in INCLUDE keyword"
                                   search))))
                      (unless visiting (kill-buffer buffer)))))))))))))

(defun org-lint-obsolete-include-markup (ast)
  (let ((regexp (format "\\`\\(?:\".+\"\\|\\S-+\\)[ \t]+%s"
			(regexp-opt
			 '("ASCII" "BEAMER" "HTML" "LATEX" "MAN" "MARKDOWN" "MD"
			   "ODT" "ORG" "TEXINFO")
			 t))))
    (org-element-map ast 'keyword
      (lambda (k)
	(when (equal (org-element-property :key k) "INCLUDE")
	  (let ((case-fold-search t)
		(value (org-element-property :value k)))
	    (when (string-match regexp value)
	      (let ((markup (match-string-no-properties 1 value)))
		(list (org-element-property :post-affiliated k)
		      (format "Obsolete markup \"%s\" in INCLUDE keyword.  \
Use \"export %s\" instead"
			      markup
			      markup))))))))))

(defun org-lint-unknown-options-item (ast)
  (let ((allowed (delq nil
		       (append
			(mapcar (lambda (o) (nth 2 o)) org-export-options-alist)
			(cl-mapcan
			 (lambda (b)
			   (mapcar (lambda (o) (nth 2 o))
				   (org-export-backend-options b)))
			 org-export-registered-backends))))
	reports)
    (org-element-map ast 'keyword
      (lambda (k)
	(when (string= (org-element-property :key k) "OPTIONS")
	  (let ((value (org-element-property :value k))
		(start 0))
	    (while (string-match "\\(.+?\\):\\((.*?)\\|\\S-+\\)?[ \t]*"
				 value
				 start)
	      (setf start (match-end 0))
	      (let ((item (match-string 1 value)))
		(unless (member item allowed)
		  (push (list (org-element-property :post-affiliated k)
			      (format "Unknown OPTIONS item \"%s\"" item))
			reports))
                (unless (match-string 2 value)
                  (push (list (org-element-property :post-affiliated k)
                              (format "Missing value for option item %S" item))
                        reports))))))))
    reports))

(defun org-lint-invalid-macro-argument-and-template (ast)
  (let* ((reports nil)
         (extract-placeholders
	  (lambda (template)
	    (let ((start 0)
		  args)
	      (while (string-match "\\$\\([1-9][0-9]*\\)" template start)
	        (setf start (match-end 0))
	        (push (string-to-number (match-string 1 template)) args))
	      (sort (org-uniquify args) #'<))))
         (check-arity
          (lambda (arity macro)
            (let* ((name (org-element-property :key macro))
                   (pos (org-element-property :begin macro))
                   (args (org-element-property :args macro))
                   (l (length args)))
              (cond
               ((< l (1- (car arity)))
                (push (list pos (format "Missing arguments in macro %S" name))
                      reports))
               ((< l (car arity))
                (push (list pos (format "Missing argument in macro %S" name))
                      reports))
               ((> l (1+ (cdr arity)))
                (push (let ((spurious-args (nthcdr (cdr arity) args)))
                        (list pos
                              (format "Spurious arguments in macro %S: %s"
                                      name
                                      (mapconcat #'org-trim spurious-args ", "))))
                      reports))
               ((> l (cdr arity))
                (push (list pos
                            (format "Spurious argument in macro %S: %s"
                                    name
                                    (org-last args)))
                      reports))
               (t nil))))))
    ;; Check arguments for macro templates.
    (org-element-map ast 'keyword
      (lambda (k)
	(when (string= (org-element-property :key k) "MACRO")
	  (let* ((value (org-element-property :value k))
		 (name (and (string-match "^\\S-+" value)
			    (match-string 0 value)))
		 (template (and name
				(org-trim (substring value (match-end 0))))))
	    (cond
	     ((not name)
	      (push (list (org-element-property :post-affiliated k)
			  "Missing name in MACRO keyword")
		    reports))
	     ((not (org-string-nw-p template))
	      (push (list (org-element-property :post-affiliated k)
			  "Missing template in macro \"%s\"" name)
		    reports))
	     (t
	      (unless (let ((args (funcall extract-placeholders template)))
			(equal (number-sequence 1 (or (org-last args) 0)) args))
		(push (list (org-element-property :post-affiliated k)
			    (format "Unused placeholders in macro \"%s\""
				    name))
		      reports))))))))
    ;; Check arguments for macros.
    (org-macro-initialize-templates)
    (let ((templates (append
		      (mapcar (lambda (m) (cons m "$1"))
			      '("author" "date" "email" "title" "results"))
		      org-macro-templates)))
      (org-element-map ast 'macro
	(lambda (macro)
	  (let* ((name (org-element-property :key macro))
		 (template (cdr (assoc-string name templates t))))
            (pcase template
              (`nil
               (push (list (org-element-property :begin macro)
			   (format "Undefined macro %S" name))
		     reports))
              ((guard (string= name "keyword"))
               (funcall check-arity '(1 . 1) macro))
              ((guard (string= name "modification-time"))
               (funcall check-arity '(1 . 2) macro))
              ((guard (string= name "n"))
               (funcall check-arity '(0 . 2) macro))
              ((guard (string= name "property"))
               (funcall check-arity '(1 . 2) macro))
              ((guard (string= name "time"))
               (funcall check-arity '(1 . 1) macro))
              ((pred functionp))        ;ignore (eval ...) templates
              (_
               (let* ((arg-numbers (funcall extract-placeholders template))
                      (arity (if (null arg-numbers)
                                 '(0 . 0)
                               (let ((m (apply #'max arg-numbers)))
                                 (cons m m)))))
                 (funcall check-arity arity macro))))))))
    reports))

(defun org-lint-undefined-footnote-reference (ast)
  (let ((definitions
          (org-element-map ast '(footnote-definition footnote-reference)
	    (lambda (f)
              (and (or (eq 'footnote-definition (org-element-type f))
                       (eq 'inline (org-element-property :type f)))
                   (org-element-property :label f))))))
    (org-element-map ast 'footnote-reference
      (lambda (f)
	(let ((label (org-element-property :label f)))
	  (and (eq 'standard (org-element-property :type f))
	       (not (member label definitions))
	       (list (org-element-property :begin f)
		     (format "Missing definition for footnote [%s]"
			     label))))))))

(defun org-lint-unreferenced-footnote-definition (ast)
  (let ((references (org-element-map ast 'footnote-reference
		      (lambda (f) (org-element-property :label f)))))
    (org-element-map ast 'footnote-definition
      (lambda (f)
	(let ((label (org-element-property :label f)))
	  (and label
	       (not (member label references))
	       (list (org-element-property :post-affiliated f)
		     (format "No reference for footnote definition [%s]"
			     label))))))))

(defun org-lint-colon-in-name (ast)
  (org-element-map ast org-element-all-elements
    (lambda (e)
      (let ((name (org-element-property :name e)))
	(and name
	     (string-match-p ":" name)
	     (list (progn
		     (goto-char (org-element-property :begin e))
		     (re-search-forward
		      (format "^[ \t]*#\\+\\w+: +%s *$" (regexp-quote name)))
		     (match-beginning 0))
		   (format
		    "Name \"%s\" contains a colon; Babel cannot use it as input"
		    name)))))))

(defun org-lint-misplaced-planning-info (_)
  (let ((case-fold-search t)
	reports)
    (while (re-search-forward org-planning-line-re nil t)
      (unless (memq (org-element-type (org-element-at-point))
		    '(comment-block example-block export-block planning
				    src-block verse-block))
	(push (list (line-beginning-position) "Misplaced planning info line")
	      reports)))
    reports))

(defun org-lint-incomplete-drawer (_)
  (let (reports)
    (while (re-search-forward org-drawer-regexp nil t)
      (let ((name (org-trim (match-string-no-properties 0)))
	    (element (org-element-at-point)))
	(pcase (org-element-type element)
	  (`drawer
	   ;; Find drawer opening lines within non-empty drawers.
	   (let ((end (org-element-property :contents-end element)))
	     (when end
	       (while (re-search-forward org-drawer-regexp end t)
		 (let ((n (org-trim (match-string-no-properties 0))))
		   (push (list (line-beginning-position)
			       (format "Possible misleading drawer entry %S" n))
			 reports))))
	     (goto-char (org-element-property :end element))))
	  (`property-drawer
	   (goto-char (org-element-property :end element)))
	  ((or `comment-block `example-block `export-block `src-block
	       `verse-block)
	   nil)
	  (_
	   ;; Find drawer opening lines outside of any drawer.
	   (push (list (line-beginning-position)
		       (format "Possible incomplete drawer %S" name))
		 reports)))))
    reports))

(defun org-lint-indented-diary-sexp (_)
  (let (reports)
    (while (re-search-forward "^[ \t]+%%(" nil t)
      (unless (memq (org-element-type (org-element-at-point))
		    '(comment-block diary-sexp example-block export-block
				    src-block verse-block))
	(push (list (line-beginning-position) "Possible indented diary-sexp")
	      reports)))
    reports))

(defun org-lint-invalid-block (_)
  (let ((case-fold-search t)
	(regexp "^[ \t]*#\\+\\(BEGIN\\|END\\)\\(?::\\|_[^[:space:]]*\\)?[ \t]*")
	reports)
    (while (re-search-forward regexp nil t)
      (let ((name (org-trim (buffer-substring-no-properties
			     (line-beginning-position) (line-end-position)))))
	(cond
	 ((and (string-prefix-p "END" (match-string 1) t)
	       (not (eolp)))
	  (push (list (line-beginning-position)
		      (format "Invalid block closing line \"%s\"" name))
		reports))
	 ((not (memq (org-element-type (org-element-at-point))
		     '(center-block comment-block dynamic-block example-block
				    export-block quote-block special-block
				    src-block verse-block)))
	  (push (list (line-beginning-position)
		      (format "Possible incomplete block \"%s\""
			      name))
		reports)))))
    reports))

(defun org-lint-invalid-keyword-syntax (_)
  (let ((regexp "^[ \t]*#\\+\\([^[:space:]:]*\\)\\(?: \\|$\\)")
	(exception-re
	 (format "[ \t]*#\\+%s\\(\\[.*\\]\\)?:\\(?: \\|$\\)"
		 (regexp-opt org-element-dual-keywords)))
	reports)
    (while (re-search-forward regexp nil t)
      (let ((name (match-string-no-properties 1)))
	(unless (or (string-prefix-p "BEGIN" name t)
		    (string-prefix-p "END" name t)
		    (save-excursion
		      (beginning-of-line)
		      (let ((case-fold-search t)) (looking-at exception-re))))
	  (push (list (match-beginning 0)
		      (format "Possible missing colon in keyword \"%s\"" name))
		reports))))
    reports))

(defun org-lint-extraneous-element-in-footnote-section (ast)
  (org-element-map ast 'headline
    (lambda (h)
      (and (org-element-property :footnote-section-p h)
	   (org-element-map (org-element-contents h)
	       (cl-remove-if
		(lambda (e)
		  (memq e '(comment comment-block footnote-definition
				    property-drawer section)))
		org-element-all-elements)
	     (lambda (e)
	       (not (and (eq (org-element-type e) 'headline)
			 (org-element-property :commentedp e))))
	     nil t '(footnote-definition property-drawer))
	   (list (org-element-property :begin h)
		 "Extraneous elements in footnote section are not exported")))))

(defun org-lint-quote-section (ast)
  (org-element-map ast '(headline inlinetask)
    (lambda (h)
      (let ((title (org-element-property :raw-value h)))
	(and (or (string-prefix-p "QUOTE " title)
		 (string-prefix-p (concat org-comment-string " QUOTE ") title))
	     (list (org-element-property :begin h)
		   "Deprecated QUOTE section"))))))

(defun org-lint-file-application (ast)
  (org-element-map ast 'link
    (lambda (l)
      (let ((app (org-element-property :application l)))
	(and app
	     (list (org-element-property :begin l)
		   (format "Deprecated \"file+%s\" link type" app)))))))

(defun org-lint-percent-encoding-link-escape (ast)
  (org-element-map ast 'link
    (lambda (l)
      (when (eq 'bracket (org-element-property :format l))
	(let* ((uri (org-element-property :path l))
	       (start 0)
	       (obsolete-flag
		(catch :obsolete
		  (while (string-match "%\\(..\\)?" uri start)
		    (setq start (match-end 0))
		    (unless (member (match-string 1 uri) '("25" "5B" "5D" "20"))
		      (throw :obsolete nil)))
		  (string-match-p "%" uri))))
	  (when obsolete-flag
	    (list (org-element-property :begin l)
		  "Link escaped with obsolete percent-encoding syntax")))))))

(defun org-lint-wrong-header-argument (ast)
  (let* ((reports)
	 (verify
	  (lambda (datum language headers)
	    (let ((allowed
		   ;; If LANGUAGE is specified, restrict allowed
		   ;; headers to both LANGUAGE-specific and default
		   ;; ones.  Otherwise, accept headers from any loaded
		   ;; language.
		   (append
		    org-babel-header-arg-names
		    (cl-mapcan
		     (lambda (l)
		       (let ((v (intern (format "org-babel-header-args:%s" l))))
			 (and (boundp v) (mapcar #'car (symbol-value v)))))
		     (if language (list language)
		       (mapcar #'car org-babel-load-languages))))))
	      (dolist (header headers)
		(let ((h (symbol-name (car header)))
		      (p (or (org-element-property :post-affiliated datum)
			     (org-element-property :begin datum))))
		  (cond
		   ((not (string-prefix-p ":" h))
		    (push
		     (list p
			   (format "Missing colon in header argument \"%s\"" h))
		     reports))
		   ((assoc-string (substring h 1) allowed))
		   (t (push (list p (format "Unknown header argument \"%s\"" h))
			    reports)))))))))
    (org-element-map ast '(babel-call inline-babel-call inline-src-block keyword
				      node-property src-block)
      (lambda (datum)
	(pcase (org-element-type datum)
	  ((or `babel-call `inline-babel-call)
	   (funcall verify
		    datum
		    nil
		    (cl-mapcan #'org-babel-parse-header-arguments
			       (list
				(org-element-property :inside-header datum)
				(org-element-property :end-header datum)))))
	  (`inline-src-block
	   (funcall verify
		    datum
		    (org-element-property :language datum)
		    (org-babel-parse-header-arguments
		     (org-element-property :parameters datum))))
	  (`keyword
	   (when (string= (org-element-property :key datum) "PROPERTY")
	     (let ((value (org-element-property :value datum)))
	       (when (or (string-match "\\`header-args\\(?::\\(\\S-+\\)\\)?\\+ *"
				       value)
                         (string-match "\\`header-args\\(?::\\(\\S-+\\)\\)? *"
				       value))
		 (funcall verify
			  datum
			  (match-string 1 value)
			  (org-babel-parse-header-arguments
			   (substring value (match-end 0))))))))
	  (`node-property
	   (let ((key (org-element-property :key datum)))
	     (when (let ((case-fold-search t))
		     (or (string-match "\\`HEADER-ARGS\\(?::\\(\\S-+\\)\\)?\\+"
				       key)
                         (string-match "\\`HEADER-ARGS\\(?::\\(\\S-+\\)\\)?"
				       key)))
	       (funcall verify
			datum
			(match-string 1 key)
			(org-babel-parse-header-arguments
			 (org-element-property :value datum))))))
	  (`src-block
	   (funcall verify
		    datum
		    (org-element-property :language datum)
		    (cl-mapcan #'org-babel-parse-header-arguments
			       (cons (org-element-property :parameters datum)
				     (org-element-property :header datum))))))))
    reports))

(defun org-lint-wrong-header-value (ast)
  (let (reports)
    (org-element-map ast
	'(babel-call inline-babel-call inline-src-block src-block)
      (lambda (datum)
	(let* ((type (org-element-type datum))
	       (language (org-element-property :language datum))
	       (allowed-header-values
		(append (and language
			     (let ((v (intern (concat "org-babel-header-args:"
						      language))))
			       (and (boundp v) (symbol-value v))))
			org-babel-common-header-args-w-values))
	       (datum-header-values
		(org-babel-parse-header-arguments
		 (org-trim
		  (pcase type
		    (`src-block
		     (mapconcat
		      #'identity
		      (cons (org-element-property :parameters datum)
			    (org-element-property :header datum))
		      " "))
		    (`inline-src-block
		     (or (org-element-property :parameters datum) ""))
		    (_
		     (concat
		      (org-element-property :inside-header datum)
		      " "
		      (org-element-property :end-header datum))))))))
	  (dolist (header datum-header-values)
	    (let ((allowed-values
		   (cdr (assoc-string (substring (symbol-name (car header)) 1)
				      allowed-header-values))))
	      (unless (memq allowed-values '(:any nil))
		(let ((values (cdr header))
		      groups-alist)
		  (dolist (v (if (stringp values) (split-string values)
			       (list values)))
		    (let ((valid-value nil))
		      (catch 'exit
			(dolist (group allowed-values)
			  (cond
			   ((not (funcall
				  (if (stringp v) #'assoc-string #'assoc)
				  v group))
			    (when (memq :any group)
			      (setf valid-value t)
			      (push (cons group v) groups-alist)))
			   ((assq group groups-alist)
			    (push
			     (list
			      (or (org-element-property :post-affiliated datum)
				  (org-element-property :begin datum))
			      (format
			       "Forbidden combination in header \"%s\": %s, %s"
			       (car header)
			       (cdr (assq group groups-alist))
			       v))
			     reports)
			    (throw 'exit nil))
			   (t (push (cons group v) groups-alist)
			      (setf valid-value t))))
			(unless valid-value
			  (push
			   (list
			    (or (org-element-property :post-affiliated datum)
				(org-element-property :begin datum))
			    (format "Unknown value \"%s\" for header \"%s\""
				    v
				    (car header)))
			   reports))))))))))))
    reports))

(defun org-lint-spurious-colons (ast)
  (org-element-map ast '(headline inlinetask)
    (lambda (h)
      (when (member "" (org-element-property :tags h))
	(list (org-element-property :begin h)
	      "Tags contain a spurious colon")))))

(defun org-lint-non-existent-bibliography (ast)
  (org-element-map ast 'keyword
    (lambda (k)
      (when (equal "BIBLIOGRAPHY" (org-element-property :key k))
        (let ((file (org-strip-quotes (org-element-property :value k))))
          (and (not (file-remote-p file))
	       (not (file-exists-p file))
	       (list (org-element-property :begin k)
		     (format "Non-existent bibliography %S" file))))))))

(defun org-lint-missing-print-bibliography (ast)
  (and (org-element-map ast 'citation #'identity nil t)
       (not (org-element-map ast 'keyword
              (lambda (k)
                (equal "PRINT_BIBLIOGRAPHY" (org-element-property :key k)))
              nil t))
       (list
        (list (point-max) "Possibly missing \"PRINT_BIBLIOGRAPHY\" keyword"))))

(defun org-lint-invalid-cite-export-declaration (ast)
  (org-element-map ast 'keyword
    (lambda (k)
      (when (equal "CITE_EXPORT" (org-element-property :key k))
        (let ((value (org-element-property :value k))
              (source (org-element-property :begin k)))
          (if (equal value "")
              (list source "Missing export processor name")
            (condition-case _
                (pcase (org-cite-read-processor-declaration value)
                  (`(,(and (pred symbolp) name)
                     ,(pred string-or-null-p)
                     ,(pred string-or-null-p))
                   (unless (org-cite-get-processor name)
                     (list source "Unknown cite export processor %S" name)))
                  (_
                   (list source "Invalid cite export processor declaration")))
              (error
               (list source "Invalid cite export processor declaration")))))))))

(defun org-lint-incomplete-citation (ast)
  (org-element-map ast 'plain-text
    (lambda (text)
      (and (string-match-p org-element-citation-prefix-re text)
           ;; XXX: The code below signals the error at the beginning
           ;; of the paragraph containing the faulty object.  It is
           ;; not very accurate but may be enough for now.
           (list (org-element-property :contents-begin
                                       (org-element-property :parent text))
                 "Possibly incomplete citation markup")))))


;;; Checkers declaration

(org-lint-add-checker 'duplicate-custom-id
  "Report duplicates CUSTOM_ID properties"
  #'org-lint-duplicate-custom-id
  :categories '(link))

(org-lint-add-checker 'duplicate-name
  "Report duplicate NAME values"
  #'org-lint-duplicate-name
  :categories '(babel 'link))

(org-lint-add-checker 'duplicate-target
  "Report duplicate targets"
  #'org-lint-duplicate-target
  :categories '(link))

(org-lint-add-checker 'duplicate-footnote-definition
  "Report duplicate footnote definitions"
  #'org-lint-duplicate-footnote-definition
  :categories '(footnote))

(org-lint-add-checker 'orphaned-affiliated-keywords
  "Report orphaned affiliated keywords"
  #'org-lint-orphaned-affiliated-keywords
  :trust 'low)

(org-lint-add-checker 'obsolete-affiliated-keywords
  "Report obsolete affiliated keywords"
  #'org-lint-obsolete-affiliated-keywords
  :categories '(obsolete))

(org-lint-add-checker 'deprecated-export-blocks
  "Report deprecated export block syntax"
  #'org-lint-deprecated-export-blocks
  :trust 'low :categories '(obsolete export))

(org-lint-add-checker 'deprecated-header-syntax
  "Report deprecated Babel header syntax"
  #'org-lint-deprecated-header-syntax
  :trust 'low :categories '(obsolete babel))

(org-lint-add-checker 'missing-language-in-src-block
  "Report missing language in source blocks"
  #'org-lint-missing-language-in-src-block
  :categories '(babel))

(org-lint-add-checker 'missing-backend-in-export-block
  "Report missing back-end in export blocks"
  #'org-lint-missing-backend-in-export-block
  :categories '(export))

(org-lint-add-checker 'invalid-babel-call-block
  "Report invalid Babel call blocks"
  #'org-lint-invalid-babel-call-block
  :categories '(babel))

(org-lint-add-checker 'colon-in-name
  "Report NAME values with a colon"
  #'org-lint-colon-in-name
  :categories '(babel))

(org-lint-add-checker 'wrong-header-argument
  "Report wrong babel headers"
  #'org-lint-wrong-header-argument
  :categories '(babel))

(org-lint-add-checker 'wrong-header-value
  "Report invalid value in babel headers"
  #'org-lint-wrong-header-value
  :categories '(babel) :trust 'low)

(org-lint-add-checker 'deprecated-category-setup
  "Report misuse of CATEGORY keyword"
  #'org-lint-deprecated-category-setup
  :categories '(obsolete))

(org-lint-add-checker 'invalid-coderef-link
  "Report \"coderef\" links with unknown destination"
  #'org-lint-invalid-coderef-link
  :categories '(link))

(org-lint-add-checker 'invalid-custom-id-link
  "Report \"custom-id\" links with unknown destination"
  #'org-lint-invalid-custom-id-link
  :categories '(link))

(org-lint-add-checker 'invalid-fuzzy-link
  "Report \"fuzzy\" links with unknown destination"
  #'org-lint-invalid-fuzzy-link
  :categories '(link))

(org-lint-add-checker 'invalid-id-link
  "Report \"id\" links with unknown destination"
  #'org-lint-invalid-id-link
  :categories '(link))

(org-lint-add-checker 'link-to-local-file
  "Report links to non-existent local files"
  #'org-lint-link-to-local-file
  :categories '(link) :trust 'low)

(org-lint-add-checker 'non-existent-setupfile-parameter
  "Report SETUPFILE keywords with non-existent file parameter"
  #'org-lint-non-existent-setupfile-parameter
  :trust 'low)

(org-lint-add-checker 'wrong-include-link-parameter
  "Report INCLUDE keywords with misleading link parameter"
  #'org-lint-wrong-include-link-parameter
  :categories '(export) :trust 'low)

(org-lint-add-checker 'obsolete-include-markup
  "Report obsolete markup in INCLUDE keyword"
  #'org-lint-obsolete-include-markup
  :categories '(obsolete export) :trust 'low)

(org-lint-add-checker 'unknown-options-item
  "Report unknown items in OPTIONS keyword"
  #'org-lint-unknown-options-item
  :categories '(export) :trust 'low)

(org-lint-add-checker 'invalid-macro-argument-and-template
  "Report spurious macro arguments or invalid macro templates"
  #'org-lint-invalid-macro-argument-and-template
  :categories '(export) :trust 'low)

(org-lint-add-checker 'special-property-in-properties-drawer
  "Report special properties in properties drawers"
  #'org-lint-special-property-in-properties-drawer
  :categories '(properties))

(org-lint-add-checker 'obsolete-properties-drawer
  "Report obsolete syntax for properties drawers"
  #'org-lint-obsolete-properties-drawer
  :categories '(obsolete properties))

(org-lint-add-checker 'invalid-effort-property
  "Report invalid duration in EFFORT property"
  #'org-lint-invalid-effort-property
  :categories '(properties))

(org-lint-add-checker 'undefined-footnote-reference
  "Report missing definition for footnote references"
  #'org-lint-undefined-footnote-reference
  :categories '(footnote))

(org-lint-add-checker 'unreferenced-footnote-definition
  "Report missing reference for footnote definitions"
  #'org-lint-unreferenced-footnote-definition
  :categories '(footnote))

(org-lint-add-checker 'extraneous-element-in-footnote-section
  "Report non-footnote definitions in footnote section"
  #'org-lint-extraneous-element-in-footnote-section
  :categories '(footnote))

(org-lint-add-checker 'invalid-keyword-syntax
  "Report probable invalid keywords"
  #'org-lint-invalid-keyword-syntax
  :trust 'low)

(org-lint-add-checker 'invalid-block
  "Report invalid blocks"
  #'org-lint-invalid-block
  :trust 'low)

(org-lint-add-checker 'misplaced-planning-info
  "Report misplaced planning info line"
  #'org-lint-misplaced-planning-info
  :trust 'low)

(org-lint-add-checker 'incomplete-drawer
  "Report probable incomplete drawers"
  #'org-lint-incomplete-drawer
  :trust 'low)

(org-lint-add-checker 'indented-diary-sexp
  "Report probable indented diary-sexps"
  #'org-lint-indented-diary-sexp
  :trust 'low)

(org-lint-add-checker 'quote-section
  "Report obsolete QUOTE section"
  #'org-lint-quote-section
  :categories '(obsolete) :trust 'low)

(org-lint-add-checker 'file-application
  "Report obsolete \"file+application\" link"
  #'org-lint-file-application
  :categories '(link obsolete))

(org-lint-add-checker 'percent-encoding-link-escape
  "Report obsolete escape syntax in links"
  #'org-lint-percent-encoding-link-escape
  :categories '(link obsolete) :trust 'low)

(org-lint-add-checker 'spurious-colons
  "Report spurious colons in tags"
  #'org-lint-spurious-colons
  :categories '(tags))

(org-lint-add-checker 'non-existent-bibliography
  "Report invalid bibliography file"
  #'org-lint-non-existent-bibliography
  :categories '(cite))

(org-lint-add-checker 'missing-print-bibliography
  "Report missing \"print_bibliography\" keyword"
  #'org-lint-missing-print-bibliography
  :categories '(cite))

(org-lint-add-checker 'invalid-cite-export-declaration
  "Report invalid value for \"cite_export\" keyword"
  #'org-lint-invalid-cite-export-declaration
  :categories '(cite))

(org-lint-add-checker 'incomplete-citation
  "Report incomplete citation object"
  #'org-lint-incomplete-citation
  :categories '(cite) :trust 'low)

(provide 'org-lint)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-lint.el ends here
