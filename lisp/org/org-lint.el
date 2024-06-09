;;; org-lint.el --- Linting for Org documents        -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Keywords: outlines, hypermedia, calendar, text

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
;; - missing backend in export blocks,
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
;; - invalid ID property with a double colon,
;; - missing definition for footnote references,
;; - missing reference for footnote definitions,
;; - non-footnote definitions in footnote section,
;; - probable invalid keywords,
;; - invalid blocks,
;; - mismatched repeaters in planning info line,
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
      (let ((ast (org-element-parse-buffer nil nil 'defer))
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
		      (forward-line 0)
		      (prog1 (propertize
                              (number-to-string
			       (cl-incf last-line
				        (count-lines last-pos (point))))
                              'org-lint-marker (car report))
			(setf last-pos (point))))
		    (cdr report)))))
	 ;; Insert trust level in generated reports.  Also sort them
	 ;; by buffer position in order to optimize lines computation.
	 (sort (cl-mapcan
		(lambda (c)
		  (let ((trust (symbol-name (org-lint-checker-trust c))))
		    (mapcar
		     (lambda (report)
		       (list (copy-marker (car report)) trust (nth 1 report) c))
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

(defun org-lint--current-marker ()
  "Return current report marker."
  (get-text-property 0 'org-lint-marker (aref (tabulated-list-get-entry) 0)))

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
  (let ((mk (org-lint--current-marker)))
    (switch-to-buffer-other-window org-lint--source-buffer)
    (unless (<= (point-min) mk (point-max)) (widen))
    (goto-char mk)
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

(defun org-lint-misplaced-heading (ast)
  "Check for accidentally misplaced heading lines."
  (org-with-point-at ast
    (goto-char (point-min))
    (let (result)
      ;; Heuristics for 2+ level heading not at bol.
      (while (re-search-forward (rx (not (any "*\n\r ,")) ;; Not a bol; not escaped ,** heading; not " *** words"
                                    "*" (1+ "*") " ") nil t)
        (unless (org-at-block-p) ; Inside a block, where the chances to have heading a slim.
          (push (list (match-beginning 0) "Possibly misplaced heading line") result)))
      result)))

(defun org-lint-duplicate-custom-id (ast)
  (org-lint--collect-duplicates
   ast
   'node-property
   (lambda (property)
     (and (org-string-equal-ignore-case
           "CUSTOM_ID" (org-element-property :key property))
	  (org-element-property :value property)))
   (lambda (property _) (org-element-begin property))
   (lambda (key) (format "Duplicate CUSTOM_ID property \"%s\"" key))))

(defun org-lint-duplicate-name (ast)
  (org-lint--collect-duplicates
   ast
   org-element-all-elements
   (lambda (datum) (org-element-property :name datum))
   (lambda (datum name)
     (goto-char (org-element-begin datum))
     (re-search-forward
      (format "^[ \t]*#\\+[A-Za-z]+:[ \t]*%s[ \t]*$" (regexp-quote name)))
     (match-beginning 0))
   (lambda (key) (format "Duplicate NAME \"%s\"" key))))

(defun org-lint-duplicate-target (ast)
  (org-lint--collect-duplicates
   ast
   'target
   (lambda (target) (split-string (org-element-property :value target)))
   (lambda (target _) (org-element-begin target))
   (lambda (key)
     (format "Duplicate target <<%s>>" (mapconcat #'identity key " ")))))

(defun org-lint-duplicate-footnote-definition (ast)
  (org-lint--collect-duplicates
   ast
   'footnote-definition
   (lambda (definition)  (org-element-property :label definition))
   (lambda (definition _) (org-element-post-affiliated definition))
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
	       (list (org-element-post-affiliated k)
		     (format "Orphaned affiliated keyword: \"%s\"" key))))))))

(defun org-lint-regular-keyword-before-affiliated (ast)
  (org-element-map ast 'keyword
    (lambda (keyword)
      (when (= (org-element-post-blank keyword) 0)
        (let ((next-element (org-with-point-at (org-element-end keyword)
                              (org-element-at-point))))
          (when (< (org-element-begin next-element) (org-element-post-affiliated next-element))
            ;; A keyword followed without blank lines by an element with affiliated keywords.
            ;; The keyword may be confused with affiliated keywords.
            (list (org-element-begin keyword)
                  (format "Independent keyword %s may be confused with affiliated keywords below"
                          (org-element-property :key keyword)))))))))

(defun org-lint-obsolete-affiliated-keywords (_)
  (let ((regexp (format "^[ \t]*#\\+%s:"
			(regexp-opt '("DATA" "LABEL" "RESNAME" "SOURCE"
				      "SRCNAME" "TBLNAME" "RESULT" "HEADERS")
				    t)))
	reports)
    (while (re-search-forward regexp nil t)
      (let ((key (upcase (match-string-no-properties 1))))
	(when (< (point)
		 (org-element-post-affiliated (org-element-at-point)))
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
	     (org-element-post-affiliated b)
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
		    (list (org-element-begin datum)
			  (format "Deprecated syntax for \"%s\".  \
Use header-args instead"
				  (match-string-no-properties 1 value))))))
	    (`node-property
	     (and (member-ignore-case key deprecated-babel-properties)
		  (list
		   (org-element-begin datum)
		   (format "Deprecated syntax for \"%s\".  \
Use :header-args: instead"
			   key))))))))))

(defun org-lint-missing-language-in-src-block (ast)
  (org-element-map ast 'src-block
    (lambda (b)
      (unless (org-element-property :language b)
	(list (org-element-post-affiliated b)
	      "Missing language in source block")))))

(defun org-lint-suspicious-language-in-src-block (ast)
  (org-element-map ast 'src-block
    (lambda (b)
      (when-let ((lang (org-element-property :language b)))
        (unless (or (functionp (intern (format "org-babel-execute:%s" lang)))
                    ;; No babel backend, but there is corresponding
                    ;; major mode.
                    (fboundp (org-src-get-lang-mode lang)))
	  (list (org-element-property :post-affiliated b)
	        (format "Unknown source block language: '%s'" lang)))))))

(defun org-lint-missing-backend-in-export-block (ast)
  (org-element-map ast 'export-block
    (lambda (b)
      (unless (org-element-property :type b)
	(list (org-element-post-affiliated b)
	      "Missing backend in export block")))))

(defun org-lint-invalid-babel-call-block (ast)
  (org-element-map ast 'babel-call
    (lambda (b)
      (cond
       ((not (org-element-property :call b))
	(list (org-element-post-affiliated b)
	      "Invalid syntax in babel call block"))
       ((let ((h (org-element-property :end-header b)))
	  (and h (string-match-p "\\`\\[.*\\]\\'" h)))
	(list
	 (org-element-post-affiliated b)
	 "Babel call's end header must not be wrapped within brackets"))))))

(defun org-lint-deprecated-category-setup (ast)
  (org-element-map ast 'keyword
    (let (category-flag)
      (lambda (k)
	(cond
	 ((not (string= (org-element-property :key k) "CATEGORY")) nil)
	 (category-flag
	  (list (org-element-post-affiliated k)
		"Spurious CATEGORY keyword.  Set :CATEGORY: property instead"))
	 (t (setf category-flag t) nil))))))

(defun org-lint-invalid-coderef-link (ast)
  (let ((info (list :parse-tree ast)))
    (org-element-map ast 'link
      (lambda (link)
	(let ((ref (org-element-property :path link)))
	  (and (equal (org-element-property :type link) "coderef")
	       (not (ignore-errors (org-export-resolve-coderef ref info)))
	       (list (org-element-begin link)
		     (format "Unknown coderef \"%s\"" ref))))))))

(defun org-lint-invalid-custom-id-link (ast)
  (let ((info (list :parse-tree ast)))
    (org-element-map ast 'link
      (lambda (link)
	(and (equal (org-element-property :type link) "custom-id")
	     (not (ignore-errors (org-export-resolve-id-link link info)))
	     (list (org-element-begin link)
		   (format "Unknown custom ID \"%s\""
			   (org-element-property :path link))))))))

(defun org-lint-invalid-fuzzy-link (ast)
  (let ((info (list :parse-tree ast)))
    (org-element-map ast 'link
      (lambda (link)
	(and (equal (org-element-property :type link) "fuzzy")
	     (not (ignore-errors (org-export-resolve-fuzzy-link link info)))
	     (list (org-element-begin link)
		   (format "Unknown fuzzy location \"%s\""
			   (let ((path (org-element-property :path link)))
			     (if (string-prefix-p "*" path)
				 (substring path 1)
			       path)))))))))

(defun org-lint-invalid-id-link (ast)
  (let ((id-locations-updated nil))
    (org-element-map ast 'link
      (lambda (link)
        (let ((id (org-element-property :path link)))
	  (and (equal (org-element-property :type link) "id")
               (progn
                 (unless id-locations-updated
                   (org-id-update-id-locations nil t)
                   (setq id-locations-updated t))
                 t)
               ;; The locations are up-to-date with file changes after
               ;; the call to `org-id-update-id-locations'.  We do not
               ;; need to double-check if recorded ID is still present
               ;; in the file.
	       (not (org-id-find-id-file id))
	       (list (org-element-begin link)
		     (format "Unknown ID \"%s\"" id))))))))

(defun org-lint-confusing-brackets (ast)
  (org-element-map ast 'link
    (lambda (link)
      (org-with-wide-buffer
       (when (eq (char-after (org-element-end link)) ?\])
         (list (org-element-begin link)
	       (format "Trailing ']' after link end")))))))

(defun org-lint-brackets-inside-description (ast)
  (org-element-map ast 'link
    (lambda (link)
      (when (org-element-contents-begin link)
        (org-with-point-at link
          (goto-char (org-element-contents-begin link))
          (let ((count 0))
            (while (re-search-forward (rx (or ?\] ?\[)) (org-element-contents-end link) t)
              (if (equal (match-string 0) "[") (cl-incf count) (cl-decf count)))
            (when (> count 0)
              (list (org-element-begin link)
	            (format "No closing ']' matches '[' in link description: %s"
                            (buffer-substring-no-properties
                             (org-element-contents-begin link)
                             (org-element-contents-end link)))))))))))

(defun org-lint-special-property-in-properties-drawer (ast)
  (org-element-map ast 'node-property
    (lambda (p)
      (let ((key (org-element-property :key p)))
	(and (member-ignore-case key org-special-properties)
	     (list (org-element-begin p)
		   (format
		    "Special property \"%s\" found in a properties drawer"
		    key)))))))

(defun org-lint-obsolete-properties-drawer (ast)
  (org-element-map ast 'drawer
    (lambda (d)
      (when (equal (org-element-property :drawer-name d) "PROPERTIES")
	(let ((headline? (org-element-lineage d 'headline))
	      (before
	       (mapcar #'org-element-type
		       (assq d (reverse (org-element-contents
					 (org-element-parent d)))))))
	  (list (org-element-post-affiliated d)
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
	       (list (org-element-begin p)
		     (format "Invalid effort duration format: %S" value))))))))

(defun org-lint-invalid-id-property (ast)
  (org-element-map ast 'node-property
    (lambda (p)
      (when (equal "ID" (org-element-property :key p))
	(let ((value (org-element-property :value p)))
	  (and (org-string-nw-p value)
               (string-match-p "::" value)
	       (list (org-element-begin p)
		     (format "IDs should not include \"::\": %S" value))))))))

(defun org-lint-link-to-local-file (ast)
  (org-element-map ast 'link
    (lambda (l)
      (let ((type (org-element-property :type l)))
	(pcase type
	  ((or "attachment" "file")
	   (let* ((path (org-element-property :path l))
		  (file (if (string= type "file")
			    path
                          (org-with-point-at (org-element-begin l)
			    (org-attach-expand path)))))
             (setq file (substitute-env-in-file-name file))
	     (and (not (file-remote-p file))
		  (not (file-exists-p file))
		  (list (org-element-begin l)
			(format (if (org-element-lineage l 'link)
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
	       (list (org-element-begin k)
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
              (list (org-element-post-affiliated k)
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
                    (list (org-element-post-affiliated k)
                          "Non-existent file argument in INCLUDE keyword")
                  (let* ((visiting (if file (find-buffer-visiting file)
                                     (current-buffer)))
                         (buffer (or visiting (find-file-noselect file)))
                         (org-link-search-must-match-exact-headline t))
                    (unwind-protect
                        (with-current-buffer buffer
                          (org-with-wide-buffer
                           (when (and search
                                      (not (ignore-errors
                                           (org-link-search search nil t))))
                             (list (org-element-post-affiliated k)
                                   (format
                                    "Invalid search part \"%s\" in INCLUDE keyword"
                                    search)))))
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
		(list (org-element-post-affiliated k)
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
		  (push (list (org-element-post-affiliated k)
			      (format "Unknown OPTIONS item \"%s\"" item))
			reports))
                (unless (match-string 2 value)
                  (push (list (org-element-post-affiliated k)
                              (format "Missing value for option item %S" item))
                        reports))))))))
    reports))

(defun org-lint-export-option-keywords (ast)
  "Check for options keyword properties without EXPORT in AST."
  (require 'ox)
  (let (options reports common-options options-alist)
    (dolist (opt org-export-options-alist)
      (when (stringp (nth 1 opt))
        (cl-pushnew (nth 1 opt) common-options :test #'equal)))
    (dolist (backend org-export-registered-backends)
      (dolist (opt (org-export-backend-options backend))
        (when (stringp (nth 1 opt))
          (cl-pushnew (or (org-export-backend-name backend) 'anonymous)
                      (alist-get (nth 1 opt) options-alist nil nil #'equal))
	  (cl-pushnew (nth 1 opt) options :test #'equal))))
    (setq options-alist (nreverse options-alist))
    (org-element-map ast 'node-property
      (lambda (node)
        (let ((prop (org-element-property :key node)))
          (when (and (or (member prop options) (member prop common-options))
                     (not (member prop org-default-properties)))
            (push (list (org-element-post-affiliated node)
                        (format "Potentially misspelled %sexport option \"%s\"%s.  Consider \"EXPORT_%s\"."
                                (when (member prop common-options)
                                  "global ")
                                prop
                                (if-let ((backends
                                          (and (not (member prop common-options))
                                               (cdr (assoc-string prop options-alist)))))
                                    (format
                                     " in %S export %s"
                                     (if (= 1 (length backends)) (car backends) backends)
                                     (if (> (length backends) 1) "backends" "backend"))
                                  "")
                                prop))
                  reports)))))
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
                   (pos (org-element-begin macro))
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
	      (push (list (org-element-post-affiliated k)
			  "Missing name in MACRO keyword")
		    reports))
	     ((not (org-string-nw-p template))
	      (push (list (org-element-post-affiliated k)
			  "Missing template in macro \"%s\"" name)
		    reports))
	     (t
	      (unless (let ((args (funcall extract-placeholders template)))
			(equal (number-sequence 1 (or (org-last args) 0)) args))
		(push (list (org-element-post-affiliated k)
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
               (push (list (org-element-begin macro)
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
             (and (or (org-element-type-p f 'footnote-definition)
                      (eq 'inline (org-element-property :type f)))
                  (org-element-property :label f))))))
    (org-element-map ast 'footnote-reference
      (lambda (f)
	(let ((label (org-element-property :label f)))
	  (and (eq 'standard (org-element-property :type f))
	       (not (member label definitions))
	       (list (org-element-begin f)
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
	       (list (org-element-post-affiliated f)
		     (format "No reference for footnote definition [%s]"
			     label))))))))

(defun org-lint-mismatched-planning-repeaters (ast)
  (org-element-map ast 'planning
    (lambda (e)
      (let* ((scheduled (org-element-property :scheduled e))
             (deadline (org-element-property :deadline e))
             (scheduled-repeater-type (org-element-property
                                       :repeater-type scheduled))
             (deadline-repeater-type (org-element-property
                                      :repeater-type deadline))
             (scheduled-repeater-value (org-element-property
                                        :repeater-value scheduled))
             (deadline-repeater-value (org-element-property
                                       :repeater-value deadline)))
        (when (and scheduled deadline
                   (memq scheduled-repeater-type '(cumulate catch-up))
                   (memq deadline-repeater-type '(cumulate catch-up))
                   (> scheduled-repeater-value 0)
                   (> deadline-repeater-value 0)
                   (not
                    (and
                     (eq scheduled-repeater-type deadline-repeater-type)
                     (eq (org-element-property :repeater-unit scheduled)
                         (org-element-property :repeater-unit deadline))
                     (eql scheduled-repeater-value deadline-repeater-value))))
          (list
           (org-element-property :begin e)
           "Different repeaters in SCHEDULED and DEADLINE timestamps."))))))

(defun org-lint-misplaced-planning-info (_)
  (let ((case-fold-search t)
	reports)
    (while (re-search-forward org-planning-line-re nil t)
      (unless (org-element-type-p
               (org-element-at-point)
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
	   (let ((end (org-element-contents-end element)))
	     (when end
	       (while (re-search-forward org-drawer-regexp end t)
		 (let ((n (org-trim (match-string-no-properties 0))))
		   (push (list (line-beginning-position)
			       (format "Possible misleading drawer entry %S" n))
			 reports))))
	     (goto-char (org-element-end element))))
	  (`property-drawer
	   (goto-char (org-element-end element)))
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
      (unless (org-element-type-p
               (org-element-at-point)
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
	 ((not (org-element-type-p
              (org-element-at-point)
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
		      (forward-line 0)
		      (let ((case-fold-search t)) (looking-at exception-re))))
	  (push (list (match-beginning 0)
		      (format "Possible missing colon in keyword \"%s\"" name))
		reports))))
    reports))

(defun org-lint-invalid-image-alignment (ast)
  (apply
   #'nconc
   (org-element-map ast 'paragraph
     (lambda (p)
       (let ((center-re ":center[[:space:]]+\\(\\S-+\\)")
             (align-re ":align[[:space:]]+\\(\\S-+\\)")
             (keyword-string
              (car-safe (org-element-property :attr_org p)))
             reports)
         (when keyword-string
           (when (and (string-match align-re keyword-string)
                      (not (member (match-string 1 keyword-string)
                                   '("left" "center" "right"))))
             (push
              (list (org-element-begin p)
                    (format
                     "\"%s\" not a supported value for #+ATTR_ORG keyword attribute \":align\"."
                     (match-string 1 keyword-string)))
              reports))
           (when (and (string-match center-re keyword-string)
                      (not (equal (match-string 1 keyword-string) "t")))
             (push
              (list (org-element-begin p)
                    (format
                     "\"%s\" not a supported value for #+ATTR_ORG keyword attribute \":center\"."
                     (match-string 1 keyword-string)))
              reports)))
         reports)))))

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
	       (not (and (org-element-type-p e 'headline)
		       (org-element-property :commentedp e))))
	     nil t '(footnote-definition property-drawer))
	   (list (org-element-begin h)
		 "Extraneous elements in footnote section are not exported")))))

(defun org-lint-quote-section (ast)
  (org-element-map ast '(headline inlinetask)
    (lambda (h)
      (let ((title (org-element-property :raw-value h)))
	(and (or (string-prefix-p "QUOTE " title)
		 (string-prefix-p (concat org-comment-string " QUOTE ") title))
	     (list (org-element-begin h)
		   "Deprecated QUOTE section"))))))

(defun org-lint-file-application (ast)
  (org-element-map ast 'link
    (lambda (l)
      (let ((app (org-element-property :application l)))
	(and app
	     (list (org-element-begin l)
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
	    (list (org-element-begin l)
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
		      (p (or (org-element-post-affiliated datum)
			     (org-element-begin datum))))
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

(defun org-lint-empty-header-argument (ast)
  (let* (reports)
    (org-element-map ast '(babel-call inline-babel-call inline-src-block src-block)
      (lambda (datum)
        (let ((headers
	       (pcase (org-element-type datum)
	         ((or `babel-call `inline-babel-call)
		  (cl-mapcan
                   (lambda (header) (org-babel-parse-header-arguments header 'no-eval))
		   (list
		    (org-element-property :inside-header datum)
		    (org-element-property :end-header datum))))
	         (`inline-src-block
		  (org-babel-parse-header-arguments
		   (org-element-property :parameters datum)
                   'no-eval))
	         (`src-block
		  (cl-mapcan
                   (lambda (header) (org-babel-parse-header-arguments header 'no-eval))
		   (cons (org-element-property :parameters datum)
			 (org-element-property :header datum)))))))
          (dolist (header headers)
            (when (not (cdr header))
              (push
	       (list
                (or (org-element-post-affiliated datum)
		    (org-element-begin datum))
		(format "Empty value in header argument \"%s\"" (symbol-name (car header))))
	       reports))))))
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
			      (or (org-element-post-affiliated datum)
				  (org-element-begin datum))
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
			    (or (org-element-post-affiliated datum)
				(org-element-begin datum))
			    (format "Unknown value \"%s\" for header \"%s\""
				    v
				    (car header)))
			   reports))))))))))))
    reports))

(defun org-lint-named-result (ast)
  (org-element-map ast org-element-all-elements
    (lambda (el)
      (when-let* ((result (org-element-property :results el))
                  (result-name (org-element-property :name el))
                  (origin-block
                   (if (org-string-nw-p (car result))
                       (condition-case _
                           (org-export-resolve-link (car result) `(:parse-tree ,ast))
                         (org-link-broken nil))
                     (org-export-get-previous-element el nil))))
        (when (org-element-type-p origin-block 'src-block)
          (list (org-element-begin el)
                (format "Links to \"%s\" will not be valid during export unless the parent source block has :exports results or both" result-name)))))))

(defun org-lint-spurious-colons (ast)
  (org-element-map ast '(headline inlinetask)
    (lambda (h)
      (when (member "" (org-element-property :tags h))
	(list (org-element-begin h)
	      "Tags contain a spurious colon")))))

(defun org-lint-non-existent-bibliography (ast)
  (org-element-map ast 'keyword
    (lambda (k)
      (when (equal "BIBLIOGRAPHY" (org-element-property :key k))
        (let ((file (org-strip-quotes (org-element-property :value k))))
          (and (not (file-remote-p file))
	       (not (file-exists-p file))
	       (list (org-element-begin k)
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
              (source (org-element-begin k)))
          (if (equal value "")
              (list source "Missing export processor name")
            (condition-case _
                (pcase (org-cite-read-processor-declaration value)
                  (`(,(and (pred symbolp) name)
                     ,(pred string-or-null-p)
                     ,(pred string-or-null-p))
                   (unless (or (org-cite-get-processor name)
                               (progn
                                 (org-cite-try-load-processor name)
                                 (org-cite-get-processor name)))
                     (list source (format "Unknown cite export processor %S" name))))
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
           (list (org-element-contents-begin
                                       (org-element-parent text))
                 "Possibly incomplete citation markup")))))

(defun org-lint-item-number (ast)
  (org-element-map ast 'item
    (lambda (item)
      (unless (org-element-property :counter item)
        (when-let* ((bullet (org-element-property :bullet item))
                    (bullet-number
                     (cond
                      ((string-match "[A-Za-z]" bullet)
		       (- (string-to-char (upcase (match-string 0 bullet)))
			  64))
                      ((string-match "[0-9]+" bullet)
		       (string-to-number (match-string 0 bullet)))))
                    (true-number
                     (org-list-get-item-number
                      (org-element-begin item)
                      (org-element-property :structure item)
                      (org-list-prevs-alist (org-element-property :structure item))
                      (org-list-parents-alist (org-element-property :structure item)))))
          (unless (equal bullet-number (car (last true-number)))
            (list
             (org-element-begin item)
             (format "Bullet counter \"%s\" is not the same with item position %d.  Consider adding manual [@%d] counter."
                     bullet (car (last true-number)) bullet-number))))))))

(defun org-lint-LaTeX-$ (ast)
  "Report semi-obsolete $...$ LaTeX fragments.
AST is the buffer parse tree."
  (org-element-map ast 'latex-fragment
    (lambda (fragment)
      (and (string-match-p "^[$][^$]" (org-element-property :value fragment))
           (list (org-element-begin fragment)
                 "Potentially confusing LaTeX fragment format.  Prefer using more reliable \\(...\\)")))))
(defun org-lint-LaTeX-$-ambiguous (_)
  "Report LaTeX fragment-like text.
AST is the buffer parse tree."
  (org-with-wide-buffer
   (let ((ambiguous-latex-re (rx "$." digit))
         report context)
     (while (re-search-forward ambiguous-latex-re nil t)
       (setq context (org-element-context))
       (when (or (eq 'latex-fragment (org-element-type context))
                 (memq 'latex-fragment (org-element-restriction context)))
         (push
          (list
           (point)
           "$ symbol potentially matching LaTeX fragment boundary.  Consider using \\dollar entity.")
          report)))
     report)))
(defun org-lint-timestamp-syntax (ast)
  "Report malformed timestamps.
AST is the buffer parse tree."
  (org-element-map ast 'timestamp
    (lambda (timestamp)
      (let ((expected (org-element-interpret-data timestamp))
            (actual (buffer-substring-no-properties
                     (org-element-property :begin timestamp)
                     (org-element-property :end timestamp))))
        (unless (equal expected actual)
          (list (org-element-property :begin timestamp)
                (format "Potentially malformed timestamp %s.  Parsed as: %s" actual expected)))))))
(defun org-lint-inactive-planning (ast)
  "Report inactive timestamp in SCHEDULED/DEADLINE.
AST is the buffer parse tree."
  (org-element-map ast 'planning
    (lambda (planning)
      (let ((scheduled (org-element-property :scheduled planning))
            (deadline (org-element-property :deadline planning)))
        (cond
         ((memq (org-element-property :type scheduled) '(inactive inactive-range))
          (list (org-element-begin planning) "Inactive timestamp in SCHEDULED will not appear in agenda."))
         ((memq (org-element-property :type deadline) '(inactive inactive-range))
          (list (org-element-begin planning) "Inactive timestamp in DEADLINE will not appear in agenda."))
         (t nil))))))

(defvar org-beamer-frame-environment) ; defined in ox-beamer.el
(defun org-lint-beamer-frame (ast)
  "Check for occurrences of begin or end frame."
  (require 'ox-beamer)
  (org-with-point-at ast
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward
              (concat "\\\\\\(begin\\|end\\){" org-beamer-frame-environment "}") nil t)
        (push (list (match-beginning 0) "Beamer frame name may cause error when exporting.  Consider customizing `org-beamer-frame-environment'.") result))
      result)))


;;; Checkers declaration

(org-lint-add-checker 'misplaced-heading
  "Report accidentally misplaced heading lines."
  #'org-lint-misplaced-heading :trust 'low)

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

(org-lint-add-checker 'combining-keywords-with-affiliated
  "Report independent keywords preceding affiliated keywords."
  #'org-lint-regular-keyword-before-affiliated
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

(org-lint-add-checker 'suspicious-language-in-src-block
  "Report suspicious language in source blocks"
  #'org-lint-suspicious-language-in-src-block
  :trust 'low :categories '(babel))

(org-lint-add-checker 'missing-backend-in-export-block
  "Report missing backend in export blocks"
  #'org-lint-missing-backend-in-export-block
  :categories '(export))

(org-lint-add-checker 'invalid-babel-call-block
  "Report invalid Babel call blocks"
  #'org-lint-invalid-babel-call-block
  :categories '(babel))

(org-lint-add-checker 'wrong-header-argument
  "Report wrong babel headers"
  #'org-lint-wrong-header-argument
  :categories '(babel))

(org-lint-add-checker 'wrong-header-value
  "Report invalid value in babel headers"
  #'org-lint-wrong-header-value
  :categories '(babel) :trust 'low)

(org-lint-add-checker 'named-result
  "Report results evaluation with #+name keyword."
  #'org-lint-named-result
  :categories '(babel) :trust 'high)

(org-lint-add-checker 'empty-header-argument
  "Report empty values in babel headers"
  #'org-lint-empty-header-argument
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

(org-lint-add-checker 'trailing-bracket-after-link
  "Report potentially confused trailing ']' after link."
  #'org-lint-confusing-brackets
  :categories '(link) :trust 'low)

(org-lint-add-checker 'unclosed-brackets-in-link-description
  "Report potentially confused trailing ']' after link."
  #'org-lint-brackets-inside-description
  :categories '(link) :trust 'low)

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

(org-lint-add-checker 'misspelled-export-option
  "Report potentially misspelled export options in properties."
  #'org-lint-export-option-keywords
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

(org-lint-add-checker 'invalid-id-property
  "Report search string delimiter \"::\" in ID property"
  #'org-lint-invalid-id-property
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

(org-lint-add-checker 'invalid-image-alignment
  "Report unsupported align attribute for keyword"
  #'org-lint-invalid-image-alignment
  :trust 'high)

(org-lint-add-checker 'invalid-block
  "Report invalid blocks"
  #'org-lint-invalid-block
  :trust 'low)

(org-lint-add-checker 'mismatched-planning-repeaters
  "Report mismatched repeaters in planning info line"
  #'org-lint-mismatched-planning-repeaters
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

(org-lint-add-checker 'item-number
  "Report inconsistent item numbers in lists"
  #'org-lint-item-number
  :categories '(plain-list))

(org-lint-add-checker 'LaTeX-$
  "Report potentially confusing $...$ LaTeX markup."
  #'org-lint-LaTeX-$
  :categories '(markup))
(org-lint-add-checker 'LaTeX-$
  "Report $ that might be treated as LaTeX fragment boundary."
  #'org-lint-LaTeX-$-ambiguous
  :categories '(markup) :trust 'low)
(org-lint-add-checker 'beamer-frame
  "Report that frame text contains beamer frame environment."
  #'org-lint-beamer-frame
  :categories '(export) :trust 'low)
(org-lint-add-checker 'timestamp-syntax
  "Report malformed timestamps."
  #'org-lint-timestamp-syntax
  :categories '(timestamp) :trust 'low)
(org-lint-add-checker 'planning-inactive
  "Report inactive timestamps in SCHEDULED/DEADLINE."
  #'org-lint-inactive-planning
  :categories '(timestamp) :trust 'high)

(provide 'org-lint)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-lint.el ends here
