;;; ob-lob.el --- functions supporting the Library of Babel

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte, Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.4

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the online documentation for more information
;; 
;;   http://orgmode.org/worg/org-contrib/babel/

;;; Code:
(require 'ob)
(require 'ob-table)

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.
This is an association list.  Populate the library by adding
files to `org-babel-lob-files'.")

(defcustom org-babel-lob-files '()
  "Files used to populate the `org-babel-library-of-babel'.
To add files to this list use the `org-babel-lob-ingest' command."
  :group 'org-babel
  :type 'list)

;;;###autoload
(defun org-babel-lob-ingest (&optional file)
  "Add all named source-blocks defined in FILE to
`org-babel-library-of-babel'."
  (interactive "f")
  (let ((lob-ingest-count 0))
    (org-babel-map-src-blocks file
      (let* ((info (org-babel-get-src-block-info 'light))
	     (source-name (nth 4 info)))
	(when source-name
	  (setq source-name (intern source-name)
		org-babel-library-of-babel
		(cons (cons source-name info)
		      (assq-delete-all source-name org-babel-library-of-babel))
		lob-ingest-count (1+ lob-ingest-count)))))
    (message "%d src block%s added to Library of Babel"
	     lob-ingest-count (if (> lob-ingest-count 1) "s" ""))
    lob-ingest-count))

(defconst org-babel-lob-call-aliases '("lob" "call")
  "Aliases to call a source block function.
If you change the value of this variable then your files may
  become unusable by other org-babel users, and vice versa.")

(defconst org-babel-lob-one-liner-regexp
  (concat
   "^\\([ \t]*\\)#\\+\\(?:"
   (mapconcat #'regexp-quote org-babel-lob-call-aliases "\\|")
   "\\):[ \t]+\\([^\(\)\n]+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)"
   "\(\\([^\n]*\\)\)\\(\\[.+\\]\\|\\)[ \t]*\\([^\n]*\\)")
  "Regexp to match calls to predefined source block functions.")

;; functions for executing lob one-liners
;;;###autoload
(defun org-babel-lob-execute-maybe ()
  "Execute a Library of Babel source block, if appropriate.
Detect if this is context for a Library Of Babel source block and
if so then run the appropriate source block from the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (if (nth 0 info) (progn (org-babel-lob-execute info) t) nil)))

;;;###autoload
(defun org-babel-lob-get-info ()
  "Return a Library of Babel function call as a string."
  (let ((case-fold-search t))
    (save-excursion
      (beginning-of-line 1)
      (if (looking-at org-babel-lob-one-liner-regexp)
          (append
	   (mapcar #'org-babel-clean-text-properties 
		   (list
		    (format "%s%s(%s)%s"
			    (match-string 2)
			    (if (match-string 4)
				(concat "[" (match-string 4) "]") "")
			    (or (match-string 6) "") (match-string 7))
		    (match-string 8)))
	   (list (length (match-string 1))))))))
  
(defun org-babel-lob-execute (info)
  "Execute the lob call specified by INFO."
  (let ((params (org-babel-process-params
		 (org-babel-merge-params
		  org-babel-default-header-args
		  (org-babel-params-from-buffer)
		  (org-babel-params-from-properties)
		  (org-babel-parse-header-arguments
		   (org-babel-clean-text-properties
		    (concat ":var results="
			    (mapconcat #'identity (butlast info) " "))))))))
    (org-babel-execute-src-block
     nil (list "emacs-lisp" "results" params nil nil (nth 2 info)))))

(provide 'ob-lob)

;; arch-tag: ce0712c9-2147-4019-ba3f-42341b8b474b

;;; ob-lob.el ends here
