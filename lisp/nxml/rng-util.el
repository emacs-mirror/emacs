;;; rng-util.el --- utility functions for RELAX NG library  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2007-2024 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: wp, hypermedia, languages, XML, RelaxNG

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

(defun rng-make-datatypes-uri (uri)
  (if (string-equal uri "")
      ;; The spec doesn't say to do this, but it's perfectly conformant
      ;; and better than using nil, I think.
      'http://relaxng.org/ns/structure/1.0
    (intern uri)))

(defconst rng-xsd-datatypes-uri
  (rng-make-datatypes-uri "http://www.w3.org/2001/XMLSchema-datatypes"))

(defconst rng-builtin-datatypes-uri (rng-make-datatypes-uri ""))

(defun rng-substq (new old list)
  "Replace first member of LIST (if any) that is `eq' to OLD by NEW.
LIST is not modified."
  (cond ((null list) nil)
	((eq (car list) old)
	 (cons new (cdr list)))
	(t
	 (let ((tail (cons (car list)
			   nil))
	       (rest (cdr list)))
	   (setq list tail)
	   (while rest
	     (let ((item (car rest)))
	       (setq rest (cdr rest))
	       (cond ((eq item old)
		      (setcdr tail
			      (cons new rest))
		      (setq rest nil))
		     (t
		      (setq tail
			    (setcdr tail
				    (cons item nil))))))))
	 list)))

(defun rng-escape-string (s)
  (replace-regexp-in-string "[&\"<>]"
			    (lambda (match)
			      (cdr (assoc match
					  '(("&" . "&amp;")
					    ("\"" . "&quot;")
					    (">" . "&gt;")
					    ("<" . "&lt;")))))
			    s
			    t))

(define-error 'rng-error nil)

(defun rng-uniquify-eq (list)
  (declare (obsolete seq-uniq "28.1"))
  (seq-uniq list #'eq))

(define-obsolete-function-alias 'rng-uniquify-equal #'seq-uniq "28.1")
(define-obsolete-function-alias 'rng-blank-p #'string-blank-p "29.1")
(define-obsolete-function-alias 'rng-collapse-space #'string-clean-whitespace "29.1")

(provide 'rng-util)

;;; rng-util.el ends here
