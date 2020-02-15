;;; rng-util.el --- utility functions for RELAX NG library

;; Copyright (C) 2003, 2007-2020 Free Software Foundation, Inc.

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

(defun rng-uniquify-eq (list)
  "Destructively remove `eq' duplicates from LIST."
  (and list
       (let ((head list))
	 (while (cdr head)
	   (if (eq (car head) (cadr head))
	       (setcdr head (cddr head)))
	   (setq head (cdr head)))
	 list)))

(defun rng-uniquify-equal (list)
  "Destructively remove `equal' duplicates from LIST."
  (and list
       (let ((head list))
	 (while (cdr head)
	   (if (equal (car head) (cadr head))
	       (setcdr head (cddr head)))
	   (setq head (cdr head)))
	 list)))

(defun rng-blank-p (str) (string-match "\\`[ \t\n\r]*\\'" str))

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

(defun rng-collapse-space (string)
  (setq string
	(replace-regexp-in-string "[ \t\r\n]+" " " string t t))
  (when (string-match "\\` " string)
    (setq string (substring string 1)))
  (when (string-match " \\'" string)
    (setq string (substring string 0 -1)))
  string)

(define-error 'rng-error nil)

(provide 'rng-util)

;;; rng-util.el ends here
