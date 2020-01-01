;;; nxml-rap.el --- low-level support for random access parsing for nXML mode  -*- lexical-binding:t -*-

;; Copyright (C) 2003-2004, 2007-2020 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: wp, hypermedia, languages, XML

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

;; This uses xmltok.el to do XML parsing. The fundamental problem is
;; how to handle changes. We don't want to maintain a complete parse
;; tree.  We also don't want to reparse from the start of the document
;; on every keystroke.  However, it is not possible in general to
;; parse an XML document correctly starting at a random point in the
;; middle.  The main problems are comments, CDATA sections and
;; processing instructions: these can all contain things that are
;; indistinguishable from elements. Literals in the prolog are also a
;; problem.  Attribute value literals are not a problem because
;; attribute value literals cannot contain less-than signs.
;;
;; Our strategy is to keep track of just the problematic things.
;; Specifically, we keep track of all comments, CDATA sections and
;; processing instructions in the instance.  We do this by marking
;; the first character of these with the generic string syntax by setting
;; a 'syntax-table' text property in `sgml-syntax-propertize'.
;;
;; Thus to parse some random point in the file we first ensure that we
;; have scanned up to that point.  Then we search backwards for a <.
;; Then we check whether the < has the generic string syntax.  If it
;; does we go backwards to first character of the generic string (this
;; character must be a <).  Then we start parsing forward from the <
;; we have found.
;;
;; The prolog has to be parsed specially, so we also keep track of the
;; end of the prolog in `nxml-prolog-end'. The prolog is reparsed on
;; every change to the prolog.  This won't work well if people try to
;; edit huge internal subsets. Hopefully that will be rare.
;;
;; We rely on the `syntax-propertize-function' machinery to keep track
;; of the changes in the buffer.  Fontification also relies on correct
;; `syntax-table' properties.  This means that scanning for these
;; constructs had better be quick.  Fortunately it is. Firstly, the
;; typical proportion of comments, CDATA sections and processing
;; instructions is small relative to other things.  Secondly, to scan
;; we just search for the regexp <[!?].

;;; Code:

(require 'xmltok)
(require 'nxml-util)
(require 'sgml-mode)

(defvar-local nxml-prolog-end nil
  "Integer giving position following end of the prolog.")

(defsubst nxml-get-inside (pos)
  "Return non-nil if inside comment, CDATA, or PI."
  (let ((ppss (save-excursion (syntax-ppss pos))))
    (or
     ;; Inside comment.
     (nth 4 ppss)
     ;; Inside "generic" string which is used for CDATA, and PI.
     ;; "Normal" double and single quoted strings are used for
     ;; attribute values.
     (eq t (nth 3 ppss)))))

(defun nxml-inside-end (pos)
  "Return the end of the inside region containing POS.
Return nil if the character at POS is not inside."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (when (nth 8 ppss)
        (goto-char (nth 8 ppss))
        (with-syntax-table sgml-tag-syntax-table
          (if (nth 3 ppss)
              (progn (forward-comment 1) (point))
            (or (scan-sexps (point) 1) (point-max))))))))

(defun nxml-inside-start (pos)
  "Return the start of the inside region containing POS.
Return nil if the character at POS is not inside."
  (save-excursion (nth 8 (syntax-ppss pos))))

;;; Change management

;; n-s-p only called from nxml-mode.el, where this variable is defined.
(defvar nxml-prolog-regions)

(defun nxml-scan-prolog ()
  (goto-char (point-min))
  (let (xmltok-dtd
	xmltok-errors)
    (setq nxml-prolog-regions (xmltok-forward-prolog))
    (setq nxml-prolog-end (point))))

;;; Random access parsing

(defun nxml-token-after ()
  "Return the position after the token containing the char after point.
Sets up the variables `xmltok-type', `xmltok-start',
`xmltok-name-end', `xmltok-name-colon', `xmltok-attributes',
`xmltok-namespace-attributes' in the same was as does
`xmltok-forward'.  The prolog will be treated as a single token with
type `prolog'."
  (let ((pos (point)))
    (if (< pos nxml-prolog-end)
	(progn
	  (setq xmltok-type 'prolog
		xmltok-start (point-min))
	  (min nxml-prolog-end (point-max)))
      (nxml-ensure-scan-up-to-date)
      (if (nxml-get-inside pos)
	  (save-excursion
	    (nxml-move-outside-backwards)
	    (xmltok-forward)
	    (point))
	(save-excursion
	  (if (or (eq (char-after) ?<)
		      (search-backward "<"
				       (max (point-min) nxml-prolog-end)
				       t))
	      (nxml-move-outside-backwards)
	    (goto-char (if (<= (point-min) nxml-prolog-end)
			   nxml-prolog-end
			 (or (nxml-inside-end (point-min))
			     (point-min)))))
	  (while (and (nxml-tokenize-forward)
		      (<= (point) pos)))
	  (point))))))

(defun nxml-token-before ()
  "Return the position after the token containing the char before point.
Sets variables like `nxml-token-after'."
  (if (/= (point-min) (point))
      (save-excursion
	(goto-char (1- (point)))
	(nxml-token-after))
    (setq xmltok-start (point))
    (setq xmltok-type nil)
    (point)))

(defun nxml-tokenize-forward ()
  (let (xmltok-errors)
    (xmltok-forward)
    xmltok-type))

(defun nxml-move-tag-backwards (bound)
  "Move point backwards outside any “inside” regions or tags.
Point will not move past `nxml-prolog-end'.
Point will either be at BOUND or a `<' character starting a tag
outside any “inside” regions.
As a precondition, point must be >= BOUND."
  (nxml-move-outside-backwards)
  (when (not (equal (char-after) ?<))
    (if (search-backward "<" bound t)
        (progn
          (nxml-move-outside-backwards)
          (when (not (equal (char-after) ?<))
            (search-backward "<" bound t)))
      (goto-char bound))))

(defun nxml-move-outside-backwards ()
  "Move point to first character of the containing special thing.
Leave point unmoved if it is not inside anything special."
  (let ((start (nxml-inside-start (point))))
    (when start
      (goto-char start)
      (when (nxml-get-inside (point))
	(error "Char before inside-start at %s is still \"inside\"" (point))))))

(defun nxml-ensure-scan-up-to-date ()
  (syntax-propertize (point)))

;;; Element scanning

(defun nxml-scan-element-forward (from &optional up)
  "Scan forward from FROM over a single balanced element.
Point must be between tokens.  Return the position of the end of
the tag that ends the element. `xmltok-start' will contain the
position of the start of the tag.  If UP is non-nil, then scan
past end-tag of element containing point.  If no element is
found, return nil.  If a well-formedness error prevents scanning,
signal an `nxml-scan-error'.  Point is not moved."
  (let ((open-tags (and up t))
	found)
    (save-excursion
      (goto-char from)
      (while (cond ((not (nxml-tokenize-forward))
		    (when (consp open-tags)
		      (nxml-scan-error (cadr open-tags)
				       "Start-tag has no end-tag"))
		    nil)
		   ((eq xmltok-type 'start-tag)
		    (setq open-tags
			  (cons (xmltok-start-tag-qname)
				(cons xmltok-start
				      open-tags)))
		    t)
		   ((eq xmltok-type 'end-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags)) (setq found (point)) nil)
			  ((not (string= (car open-tags)
					 (xmltok-end-tag-qname)))
			   (nxml-scan-error (+ 2 xmltok-start)
					    "Mismatched end-tag; \
expected `%s'"
					    (car open-tags)))
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found (point)) nil)))
		   ((memq xmltok-type '(empty-element
					partial-empty-element))
		    (if open-tags
			t
		      (setq found (point))
		      nil))
		   ((eq xmltok-type 'partial-end-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags)) (setq found (point)) nil)
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found (point)) nil)))
		   ((eq xmltok-type 'partial-start-tag)
		    (nxml-scan-error xmltok-start
				     "Missing `>'"))
		   (t t))))
    found))

(defun nxml-scan-element-backward (from &optional up bound)
  "Scan backward from FROM over a single balanced element.
Point must be between tokens.  Return the position of the end of
the tag that starts the element. `xmltok-start' will contain the
position of the start of the tag.  If UP is non-nil, then scan
past start-tag of element containing point.  If BOUND is non-nil,
then don't scan back past BOUND.  If no element is found, return
nil.  If a well-formedness error prevents scanning, signal an
`nxml-scan-error'.  Point is not moved."
  (let ((open-tags (and up t))
	token-end found)
    (save-excursion
      (goto-char from)
      (while (cond ((or (< (point) nxml-prolog-end)
			(not (search-backward "<"
					      (max (or bound 0)
						   nxml-prolog-end)
					      t)))
		    (when (and (consp open-tags) (not bound))
		      (nxml-scan-error (cadr open-tags)
				       "End-tag has no start-tag"))
		    nil)
		   ((progn
		      (nxml-move-outside-backwards)
		      (save-excursion
			(nxml-tokenize-forward)
			(setq token-end (point)))
		      (eq xmltok-type 'end-tag))
		    (setq open-tags
			  (cons (xmltok-end-tag-qname)
				(cons xmltok-start open-tags)))
		    t)
		   ((eq xmltok-type 'start-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags))
			   (setq found token-end)
			   nil)
			  ((and (car open-tags)
				(not (string= (car open-tags)
					      (xmltok-start-tag-qname))))
			   (nxml-scan-error (1+ xmltok-start)
					    "Mismatched start-tag; \
expected `%s'"
					    (car open-tags)))
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found token-end) nil)))
		   ((memq xmltok-type '(empty-element
					partial-empty-element))
		    (if open-tags
			t
		      (setq found token-end)
		      nil))
		   ((eq xmltok-type 'partial-end-tag)
		    (setq open-tags
			  (cons nil (cons xmltok-start open-tags)))
		    t)
		   ((eq xmltok-type 'partial-start-tag)
		    ;; if we have only a partial-start-tag
		    ;; then it's unlikely that there's a matching
		    ;; end-tag, so it's probably not helpful
		    ;; to treat it as a complete start-tag
		    (nxml-scan-error xmltok-start
				     "Missing `>'"))
		   (t t))))
    found))

(defun nxml-scan-error (&rest args)
  (signal 'nxml-scan-error args))

(define-error 'nxml-scan-error
  "Scan over element that is not well-formed" 'nxml-error)

(provide 'nxml-rap)

;;; nxml-rap.el ends here
