;;; ob-exp.el --- Exportation of org-babel source blocks

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
(require 'org-exp-blocks)
(eval-when-compile
  (require 'cl))

(defvar obe-marker nil)
(defvar org-current-export-file)
(defvar org-babel-lob-one-liner-regexp)
(defvar org-babel-ref-split-regexp)
(declare-function org-babel-lob-get-info "ob-lob" ())
(declare-function org-babel-eval-wipe-error-buffer "ob-eval" ())
(add-to-list 'org-export-interblocks '(src org-babel-exp-inline-src-blocks))
(add-to-list 'org-export-interblocks '(lob org-babel-exp-lob-one-liners))
(add-hook 'org-export-blocks-postblock-hook 'org-exp-res/src-name-cleanup)

(org-export-blocks-add-block '(src org-babel-exp-src-blocks nil))

(defcustom org-export-babel-evaluate t
  "Switch controlling code evaluation during export.
When set to nil no code will be evaluated as part of the export
process."
  :group 'org-babel
  :type 'boolean)
(put 'org-export-babel-evaluate 'safe-local-variable (lambda (x) (eq x nil)))

(defvar org-babel-function-def-export-keyword "function"
  "The keyword to substitute for the source name line on export.
When exporting a source block function, this keyword will
appear in the exported version in the place of source name
line. A source block is considered to be a source block function
if the source name is present and is followed by a parenthesized
argument list. The parentheses may be empty or contain
whitespace. An example is the following which generates n random
\(uniform) numbers.

#+source: rand(n)
#+begin_src R
  runif(n)
#+end_src")

(defvar org-babel-function-def-export-indent 4
  "Number of characters to indent a source block on export.
When exporting a source block function, the block contents will
be indented by this many characters. See
`org-babel-function-def-export-name' for the definition of a
source block function.")

(defmacro org-babel-exp-in-export-file (&rest body)
  `(let* ((lang-headers (intern (concat "org-babel-default-header-args:" lang)))
	  (heading (nth 4 (ignore-errors (org-heading-components))))
	  (link (when org-current-export-file
		  (org-make-link-string
		   (if heading
		       (concat org-current-export-file "::" heading)
		     org-current-export-file))))
	  (export-buffer (current-buffer)) results)
     (when link
       ;; resolve parameters in the original file so that
       ;; headline and file-wide parameters are included, attempt
       ;; to go to the same heading in the original file
       (set-buffer (get-file-buffer org-current-export-file))
       (save-restriction
	 (condition-case nil
	     (org-open-link-from-string link)
	   (error (when heading
		    (goto-char (point-min))
		    (re-search-forward (regexp-quote heading) nil t))))
	 (setq results ,@body))
       (set-buffer export-buffer)
       results)))

(defun org-babel-exp-src-blocks (body &rest headers)
  "Process source block for export.
Depending on the 'export' headers argument in replace the source
code block with...

both ---- display the code and the results

code ---- the default, display the code inside the block but do
          not process

results - just like none only the block is run on export ensuring
          that it's results are present in the org-mode buffer

none ----- do not display either code or results upon export"
  (interactive)
  (message "org-babel-exp processing...")
  (save-excursion
    (goto-char (match-beginning 0))
    (let* ((info (org-babel-get-src-block-info 'light))
	   (lang (nth 0 info))
	   (raw-params (nth 2 info)))
      ;; bail if we couldn't get any info from the block
      (when info
	(org-babel-exp-in-export-file
	 (setf (nth 2 info)
	       (org-babel-merge-params
		org-babel-default-header-args
		(org-babel-params-from-buffer)
		(org-babel-params-from-properties lang)
		(if (boundp lang-headers) (eval lang-headers) nil)
		raw-params)))
	;; expand noweb references in the original file
	(setf (nth 1 info)
	      (if (and (cdr (assoc :noweb (nth 2 info)))
		       (string= "yes" (cdr (assoc :noweb (nth 2 info)))))
		  (org-babel-expand-noweb-references
		   info (get-file-buffer org-current-export-file))
		(nth 1 info)))
	(org-babel-exp-do-export info 'block)))))

(defun org-babel-exp-inline-src-blocks (start end)
  "Process inline source blocks between START and END for export.
See `org-babel-exp-src-blocks' for export options, currently the
options and are taken from `org-babel-default-inline-header-args'."
  (interactive)
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward org-babel-inline-src-block-regexp end t))
      (let* ((info (save-match-data (org-babel-parse-inline-src-block-match)))
	     (params (nth 2 info))
	     (replacement
	      (save-match-data
		(if (org-babel-in-example-or-verbatim)
		    (buffer-substring (match-beginning 0) (match-end 0))
		  ;; expand noweb references in the original file
		  (setf (nth 1 info)
			(if (and (cdr (assoc :noweb params))
				 (string= "yes" (cdr (assoc :noweb params))))
			    (org-babel-expand-noweb-references
			     info (get-file-buffer org-current-export-file))
			  (nth 1 info)))
		  (org-babel-exp-do-export info 'inline)))))
	(setq end (+ end (- (length replacement) (length (match-string 1)))))
	(replace-match replacement t t nil 1)))))

(defun org-exp-res/src-name-cleanup ()
  "Clean up #+results and #+srcname lines for export.
This function should only be called after all block processing
has taken place."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-re-search-forward-unprotected
	    (concat
	     "\\("org-babel-src-name-regexp"\\|"org-babel-result-regexp"\\)")
	    nil t)
      (delete-region
       (progn (beginning-of-line) (point))
       (progn (end-of-line) (+ 1 (point)))))))

(defun org-babel-in-example-or-verbatim ()
  "Return true if point is in example or verbatim code.
Example and verbatim code include escaped portions of
an org-mode buffer code that should be treated as normal
org-mode text."
  (or (org-in-indented-comment-line) 
      (save-excursion
	(save-match-data
	  (goto-char (point-at-bol))
	  (looking-at "[ \t]*:[ \t]")))
      (org-in-regexps-block-p "^[ \t]*#\\+begin_src" "^[ \t]*#\\+end_src")))

(defun org-babel-exp-lob-one-liners (start end)
  "Process Library of Babel calls between START and END for export.
See `org-babel-exp-src-blocks' for export options. Currently the
options are taken from `org-babel-default-header-args'."
  (interactive)
  (let (replacement)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (re-search-forward org-babel-lob-one-liner-regexp nil t))
	(setq replacement
	      (let ((lob-info (org-babel-lob-get-info)))
		(save-match-data
		  (org-babel-exp-do-export
		   (list "emacs-lisp" "results"
			 (org-babel-merge-params
			  org-babel-default-header-args
			  (org-babel-params-from-buffer)
			  (org-babel-params-from-properties)
			  (org-babel-parse-header-arguments
			   (org-babel-clean-text-properties
			    (concat ":var results="
				    (mapconcat #'identity
					       (butlast lob-info) " ")))))
			 (car (last lob-info)))
		   'lob))))
	(setq end (+ end (- (length replacement) (length (match-string 0)))))
	(replace-match replacement t t)))))

(defun org-babel-exp-do-export (info type)
  "Return a string with the exported content of a code block.
The function respects the value of the :exports header argument."
  (flet ((silently () (let ((session (cdr (assoc :session (nth 2 info)))))
			(when (and session
				   (not (equal "none" session)))
			  (org-babel-exp-results info type 'silent))))
	 (clean () (org-babel-remove-result info)))
    (case (intern (or (cdr (assoc :exports (nth 2 info))) "code"))
      ('none (silently) (clean) "")
      ('code (silently) (clean) (org-babel-exp-code info type))
      ('results (org-babel-exp-results info type))
      ('both (concat (org-babel-exp-code info type)
		     "\n\n"
		     (org-babel-exp-results info type))))))

(defvar backend)
(defun org-babel-exp-code (info type)
  "Prepare and return code in the current code block for export.
Code is prepared in a manner suitable for export by
org-mode.  This function is called by `org-babel-exp-do-export'.
The code block is not evaluated."
  (let ((lang (nth 0 info))
        (body (nth 1 info))
        (switches (nth 3 info))
        (name (nth 4 info))
        (args (mapcar #'cdr (org-babel-get-header (nth 2 info) :var))))
    (case type
      ('inline (format "=%s=" body))
      ('block
	  (let ((str
		 (format "#+BEGIN_SRC %s %s\n%s%s#+END_SRC\n" lang switches body
			 (if (and body (string-match "\n$" body))
			     "" "\n"))))
	    (when name
	      (add-text-properties
	       0 (length str)
	       (list 'org-caption
		     (format "%s(%s)"
			     name
			     (mapconcat #'identity args ", ")))
	       str))
	    str))
      ('lob
       (let ((call-line (and (string-match "results=" (car args))
			     (substring (car args) (match-end 0)))))
	 (cond
	  ((eq backend 'html)
	   (format "\n#+HTML: <label class=\"org-src-name\">%s</label>\n"
		   call-line))
	  ((format ": %s\n" call-line))))))))

(defun org-babel-exp-results (info type &optional silent)
  "Evaluate and return the results of the current code block for export.
Results are prepared in a manner suitable for export by org-mode.
This function is called by `org-babel-exp-do-export'.  The code
block will be evaluated.  Optional argument SILENT can be used to
inhibit insertion of results into the buffer."
  (or
   (when org-export-babel-evaluate
     (let ((lang (nth 0 info))
	   (body (nth 1 info)))
       (setf (nth 2 info) (org-babel-exp-in-export-file
			   (org-babel-process-params (nth 2 info))))
       ;; skip code blocks which we can't evaluate
       (when (fboundp (intern (concat "org-babel-execute:" lang)))
	 (org-babel-eval-wipe-error-buffer)
	 (if (equal type 'inline)
	     (let ((raw (org-babel-execute-src-block
			 nil info '((:results . "silent"))))
		   (result-params (split-string
				   (cdr (assoc :results (nth 2 info))))))
	       (unless silent
		 (cond ;; respect the value of the :results header argument
		  ((member "file" result-params)
		   (org-babel-result-to-file raw))
		  ((or (member "raw" result-params)
		       (member "org" result-params))
		   (format "%s" raw))
		  ((member "code" result-params)
		   (format "src_%s{%s}" lang raw))
		  (t
		   (if (stringp raw)
		       (if (= 0 (length raw)) "=(no results)="
			 (format "%s" raw))
		     (format "%S" raw))))))
	   (prog1 nil
	     (setf (nth 2 info)
		   (org-babel-merge-params
		    (nth 2 info)
		    `((:results . ,(if silent "silent" "replace")))))
	     (cond
	      ((equal type 'block) (org-babel-execute-src-block nil info))
	      ((equal type 'lob)
	       (save-excursion
		 (re-search-backward org-babel-lob-one-liner-regexp nil t)
		 (org-babel-execute-src-block nil info)))))))))
   ""))

(provide 'ob-exp)

;; arch-tag: 523abf4c-76d1-44ed-9f27-e3bddf34bf0f

;;; ob-exp.el ends here
