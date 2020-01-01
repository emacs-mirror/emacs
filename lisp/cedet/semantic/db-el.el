;;; semantic/db-el.el --- Semantic database extensions for Emacs Lisp

;;; Copyright (C) 2002-2020 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags

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
;; There are a lot of Emacs Lisp functions and variables available for
;; the asking.  This adds on to the semanticdb programming interface to
;; allow all loaded Emacs Lisp functions to be queried via semanticdb.
;;
;; This allows you to use programs written for Semantic using the database
;; to also work in Emacs Lisp with no compromises.
;;

(require 'semantic/db)
(require 'eieio-opt)

(declare-function semantic-elisp-desymbolify "semantic/bovine/el")
(declare-function semantic-tag-similar-p "semantic/tag-ls")

;;; Code:

;;; Classes:
(defclass semanticdb-table-emacs-lisp (semanticdb-abstract-table)
  ((major-mode :initform emacs-lisp-mode)
   )
  "A table for returning search results from Emacs.")

(cl-defmethod semanticdb-refresh-table ((_obj semanticdb-table-emacs-lisp) &optional _force)
  "Do not refresh Emacs Lisp table.
It does not need refreshing."
  nil)

(cl-defmethod semanticdb-needs-refresh-p ((_obj semanticdb-table-emacs-lisp))
  "Return nil, we never need a refresh."
  nil)

(cl-defmethod semanticdb-debug-info ((obj semanticdb-table-emacs-lisp))
  (list "(proxy)"))

(cl-defmethod cl-print-object ((obj semanticdb-table-emacs-lisp) stream)
  "Pretty printer extension for `semanticdb-table-emacs-lisp'."
  (princ (eieio-object-name obj (semanticdb-debug-info obj))
         stream))

(defclass semanticdb-project-database-emacs-lisp
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-emacs-lisp
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Emacs core.")

(cl-defmethod semanticdb-debug-info ((obj
                                      semanticdb-project-database-emacs-lisp))
  (let ((count 0))
    (mapatoms (lambda (_sym) (setq count (1+ count))))
    (append (cl-call-next-method obj)
            (list (format "(%d known syms)" count)))))

(cl-defmethod cl-print-object ((obj semanticdb-project-database-emacs-lisp)
                               stream)
  "Pretty printer extension for `semanticdb-table-emacs-lisp'.
Adds the number of tags in this file to the object print name."
  (princ (eieio-object-name obj (semanticdb-debug-info obj))
         stream))

;; Create the database, and add it to searchable databases for Emacs Lisp mode.
(defvar-mode-local emacs-lisp-mode semanticdb-project-system-databases
  (list
   (make-instance 'semanticdb-project-database-emacs-lisp))
  "Search Emacs core for symbols.")

(defvar-mode-local emacs-lisp-mode semanticdb-find-default-throttle
  '(project omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")

;;; Filename based methods
;;
(cl-defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-emacs-lisp))
  "For an Emacs Lisp database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (make-instance 'semanticdb-table-emacs-lisp)))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (cl-call-next-method))

(cl-defmethod semanticdb-file-table ((obj semanticdb-project-database-emacs-lisp) _filename)
  "From OBJ, return FILENAME's associated table object.
For Emacs Lisp, creates a specialized table."
  (car (semanticdb-get-database-tables obj))
  )

(cl-defmethod semanticdb-get-tags ((_table semanticdb-table-emacs-lisp ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time.
  nil)

(cl-defmethod semanticdb-equivalent-mode ((_table semanticdb-table-emacs-lisp) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by the `semantic-equivalent-major-modes'
local variable."
  (with-current-buffer buffer
    (eq (or mode-local-active-mode major-mode) 'emacs-lisp-mode)))

(cl-defmethod semanticdb-full-filename ((_obj semanticdb-table-emacs-lisp))
  "Fetch the full filename that OBJ refers to.
For Emacs Lisp system DB, there isn't one."
  nil)

;;; Conversion
;;
(cl-defmethod semanticdb-normalize-tags ((obj semanticdb-table-emacs-lisp) tags)
  "Convert tags, originating from Emacs OBJ, into standardized form."
  (let ((newtags nil))
    (dolist (T tags)
      (let* ((ot (semanticdb-normalize-one-tag obj T))
	     (tag (cdr ot)))
	(setq newtags (cons tag newtags))))
    ;; There is no promise to have files associated.
    (nreverse newtags)))

(cl-defmethod semanticdb-normalize-one-tag ((obj semanticdb-table-emacs-lisp) tag)
  "Convert one TAG, originating from Emacs OBJ, into standardized form.
If Emacs cannot resolve this symbol to a particular file, then return nil."
  ;; Here's the idea.  For each tag, get the name, then use
  ;; Emacs's `symbol-file' to get the source.  Once we have that,
  ;; we can use more typical semantic searching techniques to
  ;; get a regularly parsed tag.
  (let* ((type (cond ((semantic-tag-of-class-p tag 'function)
		      'defun)
		     ((semantic-tag-of-class-p tag 'variable)
		      'defvar)
		     ))
	 (sym (intern (semantic-tag-name tag)))
	 (file (condition-case nil
		   (symbol-file sym type)
		 ;; Older [X]Emacs don't have a 2nd argument.
		 (error (symbol-file sym))))
	 )
    (if (or (not file) (not (file-exists-p file)))
	;; The file didn't exist.  Return nil.
	;; We can't normalize this tag.  Fake it out.
	(cons obj tag)
      (when (string-match "\\.elc" file)
	(setq file (concat (file-name-sans-extension file)
			   ".el"))
	(when (and (not (file-exists-p file))
		   (file-exists-p (concat file ".gz")))
	  ;; Is it a .gz file?
	  (setq file (concat file ".gz"))))

      (let* ((tab (semanticdb-file-table-object file))
	     (newtags (when tab (semanticdb-find-tags-by-name-method
				 tab (semantic-tag-name tag))))
	     (match nil))
	;; We might not have a parsed tag in this file, because it
	;; might be generated through a macro like defstruct.
	(if (null newtags)
	    (setq match tag)
	  ;; Find the best match.
	  (dolist (T newtags)
	    (when (semantic-tag-similar-p T tag)
	      (setq match T)))
	  ;; Backup system.
	  (when (not match)
	    (setq match (car newtags))))
	;; Return it.
	(when tab (cons tab match))))))

(autoload 'help-function-arglist "help-fns")
(defalias 'semanticdb-elisp-sym-function-arglist 'help-function-arglist)
(make-obsolete 'semanticdb-elisp-sym-function-arglist
	       'help-function-arglist "CEDET 1.1")

(defun semanticdb-elisp-sym->tag (sym &optional toktype)
  "Convert SYM into a semantic tag.
TOKTYPE is a hint to the type of tag desired."
  (if (stringp sym)
      (setq sym (intern-soft sym)))
  (when sym
    (cond ((and (eq toktype 'function) (fboundp sym))
	   (require 'semantic/bovine/el)
	   (let ((arglist (help-function-arglist sym)))
	     (when (not (listp arglist))
	       ;; Function might be autoloaded, in which case
	       ;; the arglist is not available yet.
	       (setq arglist nil))
	     (semantic-tag-new-function
	      (symbol-name sym)
	      nil	;; return type
	      (semantic-elisp-desymbolify arglist)
	      :user-visible-flag (condition-case nil
				     (interactive-form sym)
				   (error nil)))))
	  ((and (eq toktype 'variable) (boundp sym))
	   (semantic-tag-new-variable
	    (symbol-name sym)
	    nil	;; type
	    nil	;; value - ignore for now
	    ))
	  ((and (eq toktype 'type) (class-p sym))
	   (semantic-tag-new-type
	    (symbol-name sym)
	    "class"
	    (semantic-elisp-desymbolify
	     (let ((class (find-class sym)))
	       (if (fboundp 'eieio--class-public-a) ; Emacs < 25.1
		   (eieio--class-public-a class)
		 (mapcar #'eieio-slot-descriptor-name
			 (eieio-class-slots class)))))
	    (semantic-elisp-desymbolify (eieio-class-parents sym)) ;; parents
	    ))
	  ((not toktype)
	   ;; Figure it out on our own.
	   (cond ((class-p sym)
		  (semanticdb-elisp-sym->tag sym 'type))
		 ((fboundp sym)
		  (semanticdb-elisp-sym->tag sym 'function))
		 ((boundp sym)
		  (semanticdb-elisp-sym->tag sym 'variable))
		 (t nil))
	   )
	  (t nil))))

;;; Search Overrides
;;
(defvar semanticdb-elisp-mapatom-collector nil
  "Variable used to collect `mapatoms' output.")

(cl-defmethod semanticdb-find-tags-by-name-method
  ((_table semanticdb-table-emacs-lisp) name &optional tags)
  "Find all tags named NAME in TABLE.
Uses `intern-soft' to match NAME to Emacs symbols.
Return a list of tags."
  (if tags (cl-call-next-method)
    ;; No need to search.  Use `intern-soft' which does the same thing for us.
    (let* ((sym (intern-soft name))
	   (fun (semanticdb-elisp-sym->tag sym 'function))
	   (var (semanticdb-elisp-sym->tag sym 'variable))
	   (typ (semanticdb-elisp-sym->tag sym 'type))
	   (taglst nil)
	   )
      (when (or fun var typ)
	;; If the symbol is any of these things, build the search table.
	(when var	(setq taglst (cons var taglst)))
	(when typ	(setq taglst (cons typ taglst)))
	(when fun	(setq taglst (cons fun taglst)))
	taglst
	))))

(cl-defmethod semanticdb-find-tags-by-name-regexp-method
  ((_table semanticdb-table-emacs-lisp) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Uses `apropos-internal' to find matches.
Return a list of tags."
  (if tags (cl-call-next-method)
    (delq nil (mapcar #'semanticdb-elisp-sym->tag
		      (apropos-internal regex)))))

(cl-defmethod semanticdb-find-tags-for-completion-method
  ((_table semanticdb-table-emacs-lisp) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (cl-call-next-method)
    (delq nil (mapcar #'semanticdb-elisp-sym->tag
		      (all-completions prefix obarray)))))

(cl-defmethod semanticdb-find-tags-by-class-method
  ((_table semanticdb-table-emacs-lisp) _class &optional tags)
  "In TABLE, find all occurrences of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (cl-call-next-method)
    ;; We could implement this, but it could be messy.
    nil))

;;; Deep Searches
;;
;; For Emacs Lisp deep searches are like top level searches.
(cl-defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-emacs-lisp) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for Emacs Lisp."
  (semanticdb-find-tags-by-name-method table name tags))

(cl-defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-emacs-lisp) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for Emacs Lisp."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(cl-defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-emacs-lisp) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for Emacs Lisp."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(cl-defmethod semanticdb-find-tags-external-children-of-type-method
  ((_table semanticdb-table-emacs-lisp) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (cl-call-next-method)
    ;; EIEIO is the only time this matters
    (when (featurep 'eieio)
      (let* ((class (intern-soft type))
	     (taglst (when class
		       (delq nil
			     (mapcar #'semanticdb-elisp-sym->tag
				     ;; Fancy eieio function that knows all about
				     ;; built in methods belonging to CLASS.
				     (cl-generic-all-functions class)))))
	     )
	taglst))))

(provide 'semantic/db-el)

;;; semantic/db-el.el ends here
