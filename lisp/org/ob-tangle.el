;;; ob-tangle.el --- Extract Source Code From Org Files -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; Extract the code from source blocks out into raw source-code files.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'org-src)
(require 'org-macs)
(require 'ol)

(declare-function make-directory "files" (dir &optional parents))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-babel-update-block-body "ob-core" (new-body))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-element--cache-active-p "org-element" ())
(declare-function org-element-lineage "org-element" (datum &optional types with-self))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-type "org-element" (element))
(declare-function org-heading-components "org" ())
(declare-function org-in-commented-heading-p "org" (&optional no-inheritance))
(declare-function org-in-archived-heading-p "org" (&optional no-inheritance))
(declare-function outline-previous-heading "outline" ())
(defvar org-id-link-to-org-use-id) ; Dynamically scoped

(defgroup org-babel-tangle nil
  "Options for extracting source code from code blocks."
  :tag "Org Babel Tangle"
  :group 'org-babel)

(defcustom org-babel-tangle-lang-exts
  '(("emacs-lisp" . "el")
    ("elisp" . "el"))
  "Alist mapping languages to their file extensions.
The key is the language name, the value is the string that should
be inserted as the extension commonly used to identify files
written in this language.  If no entry is found in this list,
then the name of the language is used."
  :group 'org-babel-tangle
  :version "24.1"
  :type '(repeat
	  (cons
	   (string "Language name")
	   (string "File Extension"))))

(defcustom org-babel-tangle-use-relative-file-links t
  "Use relative path names in links from tangled source back the Org file."
  :group 'org-babel-tangle
  :type 'boolean)

(defcustom org-babel-post-tangle-hook nil
  "Hook run in code files tangled by `org-babel-tangle'."
  :group 'org-babel-tangle
  :version "24.1"
  :type 'hook)

(defcustom org-babel-pre-tangle-hook '(save-buffer)
  "Hook run at the beginning of `org-babel-tangle' in the original buffer."
  :group 'org-babel-tangle
  :version "24.1"
  :type 'hook)

(defcustom org-babel-tangle-body-hook nil
  "Hook run over the contents of each code block body."
  :group 'org-babel-tangle
  :version "24.1"
  :type 'hook)

(defcustom org-babel-tangle-finished-hook nil
  "Hook run at the very end of `org-babel-tangle' in the original buffer.
In this way, it is the counterpart to `org-babel-pre-tangle-hook'."
  :group 'org-babel-tangle
  :package-version '(Org . "9.6")
  :type 'hook)

(defcustom org-babel-tangle-comment-format-beg "[[%link][%source-name]]"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org style link to the code block
%source-name -- name of the code block

Upon insertion the formatted comment will be commented out, and
followed by a newline.  To inhibit this post-insertion processing
set the `org-babel-tangle-uncomment-comments' variable to a
non-nil value.

Whether or not comments are inserted during tangling is
controlled by the :comments header argument."
  :group 'org-babel-tangle
  :version "24.1"
  :type 'string)

(defcustom org-babel-tangle-comment-format-end "%source-name ends here"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org style link to the code block
%source-name -- name of the code block

Upon insertion the formatted comment will be commented out, and
followed by a newline.  To inhibit this post-insertion processing
set the `org-babel-tangle-uncomment-comments' variable to a
non-nil value.

Whether or not comments are inserted during tangling is
controlled by the :comments header argument."
  :group 'org-babel-tangle
  :version "24.1"
  :type 'string)

(defcustom org-babel-tangle-uncomment-comments nil
  "Inhibits automatic commenting and addition of trailing newline
of tangle comments.  Use `org-babel-tangle-comment-format-beg'
and `org-babel-tangle-comment-format-end' to customize the format
of tangled comments."
  :group 'org-babel-tangle
  :type 'boolean)

(defcustom org-babel-process-comment-text 'org-remove-indentation
  "Function called to process raw Org text collected to be
inserted as comments in tangled source-code files.  The function
should take a single string argument and return a string
result.  The default value is `org-remove-indentation'."
  :group 'org-babel-tangle
  :version "24.1"
  :type 'function)

(defcustom org-babel-tangle-default-file-mode #o644
  "The default mode used for tangled files, as an integer.
The default value 420 correspands to the octal #o644, which is
read-write permissions for the user, read-only for everyone else."
  :group 'org-babel-tangle
  :package-version '(Org . "9.6")
  :type 'integer)

(defun org-babel-find-file-noselect-refresh (file)
  "Find file ensuring that the latest changes on disk are
represented in the file."
  (find-file-noselect file 'nowarn)
  (with-current-buffer (get-file-buffer file)
    (revert-buffer t t t)))

(defmacro org-babel-with-temp-filebuffer (file &rest body)
  "Open FILE into a temporary buffer execute BODY there like
`progn', then kill the FILE buffer returning the result of
evaluating BODY."
  (declare (indent 1) (debug t))
  (let ((temp-path (make-symbol "temp-path"))
	(temp-result (make-symbol "temp-result"))
	(temp-file (make-symbol "temp-file"))
	(visited-p (make-symbol "visited-p")))
    `(let* ((,temp-path ,file)
	    (,visited-p (get-file-buffer ,temp-path))
	    ,temp-result ,temp-file)
       (org-babel-find-file-noselect-refresh ,temp-path)
       (setf ,temp-file (get-file-buffer ,temp-path))
       (with-current-buffer ,temp-file
	 (setf ,temp-result (progn ,@body)))
       (unless ,visited-p (kill-buffer ,temp-file))
       ,temp-result)))

;;;###autoload
(defun org-babel-tangle-file (file &optional target-file lang-re)
  "Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.

Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.

Optional argument LANG-RE can be used to limit the exported
source code blocks by languages matching a regular expression.

Return list of the tangled file names."
  (interactive "fFile to tangle: \nP")
  (let* ((visited (find-buffer-visiting file))
         (buffer (or visited (find-file-noselect file))))
    (prog1
        (with-current-buffer buffer
          (org-with-wide-buffer
           (mapcar #'expand-file-name
                   (org-babel-tangle nil target-file lang-re))))
      (unless visited (kill-buffer buffer)))))

(defun org-babel-tangle-publish (_ filename pub-dir)
  "Tangle FILENAME and place the results in PUB-DIR."
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (setq pub-dir (file-name-as-directory pub-dir))
  (mapc (lambda (el) (copy-file el pub-dir t)) (org-babel-tangle-file filename)))

;;;###autoload
(defun org-babel-tangle (&optional arg target-file lang-re)
  "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.  Return the list of files.
With one universal prefix argument, only tangle the block at point.
When two universal prefix arguments, only tangle blocks for the
tangle file of the block at point.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG-RE can
be used to limit the exported source code blocks by languages
matching a regular expression."
  (interactive "P")
  (run-hooks 'org-babel-pre-tangle-hook)
  ;; Possibly Restrict the buffer to the current code block
  (save-restriction
    (save-excursion
      (when (equal arg '(4))
	(let ((head (org-babel-where-is-src-block-head)))
	  (if head
	      (goto-char head)
	    (user-error "Point is not in a source code block"))))
      (let ((block-counter 0)
	    (org-babel-default-header-args
	     (if target-file
		 (org-babel-merge-params org-babel-default-header-args
					 (list (cons :tangle target-file)))
	       org-babel-default-header-args))
	    (tangle-file
	     (when (equal arg '(16))
	       (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'no-eval))))
		   (user-error "Point is not in a source code block"))))
	    path-collector)
	(mapc ;; map over file-names
	 (lambda (by-fn)
	   (let ((file-name (car by-fn)))
	     (when file-name
               (let ((lspecs (cdr by-fn))
		     (fnd (file-name-directory file-name))
		     modes make-dir she-banged lang)
	         ;; drop source-blocks to file
	         ;; We avoid append-to-file as it does not work with tramp.
	         (with-temp-buffer
		   (mapc
		    (lambda (lspec)
		      (let* ((block-lang (car lspec))
			     (spec (cdr lspec))
			     (get-spec (lambda (name) (cdr (assq name (nth 4 spec)))))
			     (she-bang (let ((sheb (funcall get-spec :shebang)))
				         (when (> (length sheb) 0) sheb)))
			     (tangle-mode (funcall get-spec :tangle-mode)))
		        (unless (string-equal block-lang lang)
			  (setq lang block-lang)
			  (let ((lang-f (org-src-get-lang-mode lang)))
			    (when (fboundp lang-f) (ignore-errors (funcall lang-f)))))
		        ;; if file contains she-bangs, then make it executable
		        (when she-bang
			  (unless tangle-mode (setq tangle-mode #o755)))
		        (when tangle-mode
			  (add-to-list 'modes (org-babel-interpret-file-mode tangle-mode)))
		        ;; Possibly create the parent directories for file.
		        (let ((m (funcall get-spec :mkdirp)))
			  (and m fnd (not (string= m "no"))
			       (setq make-dir t)))
		        ;; Handle :padlines unless first line in file
		        (unless (or (string= "no" (funcall get-spec :padline))
				    (= (point) (point-min)))
			  (insert "\n"))
		        (when (and she-bang (not she-banged))
			  (insert (concat she-bang "\n"))
			  (setq she-banged t))
		        (org-babel-spec-to-string spec)
		        (setq block-counter (+ 1 block-counter))))
		    lspecs)
		   (when make-dir
		     (make-directory fnd 'parents))
                   (unless
                       (and (file-exists-p file-name)
                            (let ((tangle-buf (current-buffer)))
                              (with-temp-buffer
                                (insert-file-contents file-name)
                                (and
                                 (equal (buffer-size)
                                        (buffer-size tangle-buf))
                                 (= 0
                                    (let (case-fold-search)
                                      (compare-buffer-substrings
                                       nil nil nil
                                       tangle-buf nil nil)))))))
                     ;; erase previous file
                     (when (file-exists-p file-name)
                       (delete-file file-name))
		     (write-region nil nil file-name)
		     (mapc (lambda (mode) (set-file-modes file-name mode)) modes))
                   (push file-name path-collector))))))
	 (if (equal arg '(4))
	     (org-babel-tangle-single-block 1 t)
	   (org-babel-tangle-collect-blocks lang-re tangle-file)))
	(message "Tangled %d code block%s from %s" block-counter
		 (if (= block-counter 1) "" "s")
		 (file-name-nondirectory
		  (buffer-file-name
		   (or (buffer-base-buffer)
                       (current-buffer)
                       (and (org-src-edit-buffer-p)
                            (org-src-source-buffer))))))
	;; run `org-babel-post-tangle-hook' in all tangled files
	(when org-babel-post-tangle-hook
	  (mapc
	   (lambda (file)
	     (org-babel-with-temp-filebuffer file
	       (run-hooks 'org-babel-post-tangle-hook)))
	   path-collector))
        (run-hooks 'org-babel-tangle-finished-hook)
	path-collector))))

(defun org-babel-interpret-file-mode (mode)
  "Determine the integer representation of a file MODE specification.
The following forms are currently recognized:
- an integer (returned without modification)
- \"o755\" (chmod style octal)
- \"rwxrw-r--\" (ls style specification)
- \"a=rw,u+x\" (chmod style) *

* The interpretation of these forms relies on `file-modes-symbolic-to-number',
  and uses `org-babel-tangle-default-file-mode' as the base mode."
  (cond
   ((integerp mode)
    (if (string-match-p "^[0-7][0-7][0-7]$" (format "%o" mode))
        mode
      (user-error "%1$o is not a valid file mode octal. \
Did you give the decimal value %1$d by mistake?" mode)))
   ((not (stringp mode))
    (error "File mode %S not recognized as a valid format." mode))
   ((string-match-p "^o0?[0-7][0-7][0-7]$" mode)
    (string-to-number (replace-regexp-in-string "^o" "" mode) 8))
   ((string-match-p "^[ugoa]*\\(?:[+-=][rwxXstugo]*\\)+\\(,[ugoa]*\\(?:[+-=][rwxXstugo]*\\)+\\)*$" mode)
    ;; Match regexp taken from `file-modes-symbolic-to-number'.
    (file-modes-symbolic-to-number mode org-babel-tangle-default-file-mode))
   ((string-match-p "^[r-][w-][xs-][r-][w-][xs-][r-][w-][x-]$" mode)
    (file-modes-symbolic-to-number (concat "u="  (delete ?- (substring mode 0 3))
                                           ",g=" (delete ?- (substring mode 3 6))
                                           ",o=" (delete ?- (substring mode 6 9)))
                                   0))
   (t (error "File mode %S not recognized as a valid format. See `org-babel-interpret-file-mode'." mode))))

(defun org-babel-tangle-clean ()
  "Remove comments inserted by `org-babel-tangle'.
Call this function inside of a source-code file generated by
`org-babel-tangle' to remove all comments inserted automatically
by `org-babel-tangle'.  Warning, this comment removes any lines
containing constructs which resemble Org file links or noweb
references."
  (interactive)
  (goto-char (point-min))
  (while (or (re-search-forward "\\[\\[file:.*\\]\\[.*\\]\\]" nil t)
             (re-search-forward (org-babel-noweb-wrap) nil t))
    (delete-region (save-excursion (beginning-of-line 1) (point))
                   (save-excursion (end-of-line 1) (forward-char 1) (point)))))

(defun org-babel-spec-to-string (spec)
  "Insert SPEC into the current file.

Insert the source-code specified by SPEC into the current source
code file.  This function uses `comment-region' which assumes
that the appropriate major-mode is set.  SPEC has the form:

  (start-line file link source-name params body comment)"
  (pcase-let*
      ((`(,start ,file ,link ,source ,info ,body ,comment) spec)
       (comments (cdr (assq :comments info)))
       (link? (or (string= comments "both") (string= comments "link")
		  (string= comments "yes") (string= comments "noweb")))
       (link-data `(("start-line" . ,(number-to-string start))
		    ("file" . ,file)
		    ("link" . ,link)
		    ("source-name" . ,source)))
       (insert-comment (lambda (text)
			 (when (and comments
				    (not (string= comments "no"))
				    (org-string-nw-p text))
			   (if org-babel-tangle-uncomment-comments
			       ;; Plain comments: no processing.
			       (insert text)
			     ;; Ensure comments are made to be
			     ;; comments, and add a trailing newline.
			     ;; Also ignore invisible characters when
			     ;; commenting.
			     (comment-region
			      (point)
			      (progn (insert (org-no-properties text))
				     (point)))
			     (end-of-line)
			     (insert "\n"))))))
    (when comment (funcall insert-comment comment))
    (when link?
      (funcall insert-comment
	       (org-fill-template
		org-babel-tangle-comment-format-beg link-data)))
    (insert body "\n")
    (when link?
      (funcall insert-comment
	       (org-fill-template
		org-babel-tangle-comment-format-end link-data)))))

(defun org-babel-effective-tangled-filename (buffer-fn src-lang src-tfile)
  "Return effective tangled filename of a source-code block.
BUFFER-FN is the name of the buffer, SRC-LANG the language of the
block and SRC-TFILE is the value of the :tangle header argument,
as computed by `org-babel-tangle-single-block'."
  (let ((base-name (cond
                    ((string= "yes" src-tfile)
                     ;; Use the buffer name
                     (file-name-sans-extension buffer-fn))
                    ((string= "no" src-tfile) nil)
                    ((> (length src-tfile) 0) src-tfile)))
        (ext (or (cdr (assoc src-lang org-babel-tangle-lang-exts)) src-lang)))
    (when base-name
      ;; decide if we want to add ext to base-name
      (if (and ext (string= "yes" src-tfile))
          (concat base-name "." ext) base-name))))

(defun org-babel-tangle-collect-blocks (&optional lang-re tangle-file)
  "Collect source blocks in the current Org file.
Return an association list of language and source-code block
specifications of the form used by `org-babel-spec-to-string'
grouped by tangled file name.

Optional argument LANG-RE can be used to limit the collected
source code blocks by languages matching a regular expression.

Optional argument TANGLE-FILE can be used to limit the collected
code blocks by target file."
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
             (if (org-element--cache-active-p)
                 (or (org-element-property :begin (org-element-lineage (org-element-at-point) '(headline) t)) 1)
	       (org-with-wide-buffer
	        (org-with-limited-levels (outline-previous-heading))))))
	(if (eq last-heading-pos current-heading-pos) (cl-incf counter)
	  (setq counter 1)
	  (setq last-heading-pos current-heading-pos)))
      (unless (or (org-in-commented-heading-p)
		  (org-in-archived-heading-p))
	(let* ((info (org-babel-get-src-block-info 'no-eval))
	       (src-lang (nth 0 info))
	       (src-tfile (cdr (assq :tangle (nth 2 info)))))
	  (unless (or (string= src-tfile "no")
		      (and tangle-file (not (equal tangle-file src-tfile)))
		      (and lang-re (not (string-match-p lang-re src-lang))))
	    ;; Add the spec for this block to blocks under its tangled
	    ;; file name.
	    (let* ((block (org-babel-tangle-single-block counter))
                   (src-tfile (cdr (assq :tangle (nth 4 block))))
		   (file-name (org-babel-effective-tangled-filename
                               (nth 1 block) src-lang src-tfile))
		   (by-fn (assoc file-name blocks)))
	      (if by-fn (setcdr by-fn (cons (cons src-lang block) (cdr by-fn)))
		(push (cons file-name (list (cons src-lang block))) blocks)))))))
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b))))
	    (nreverse blocks))))

(defun org-babel-tangle--unbracketed-link (params)
  "Get a raw link to the src block at point, without brackets.

The PARAMS are the 3rd element of the info for the same src block."
  (unless (string= "no" (cdr (assq :comments params)))
    (save-match-data
      (let* (;; The created link is transient.  Using ID is not necessary,
             ;; but could have side-effects if used.  An ID property may
             ;; be added to existing entries thus creating unexpected file
             ;; modifications.
             (org-id-link-to-org-use-id nil)
             (l (org-no-properties
                 (cl-letf (((symbol-function 'org-store-link-functions)
                            (lambda () nil)))
                   (org-store-link nil))))
             (bare (and l
                        (string-match org-link-bracket-re l)
                        (match-string 1 l))))
        (when bare
          (if (and org-babel-tangle-use-relative-file-links
                   (string-match org-link-types-re bare)
                   (string= (match-string 1 bare) "file"))
              (concat "file:"
                      (file-relative-name (substring bare (match-end 0))
                                          (file-name-directory
                                           (cdr (assq :tangle params)))))
            bare))))))

(defvar org-outline-regexp) ; defined in lisp/org.el
(defun org-babel-tangle-single-block (block-counter &optional only-this-block)
  "Collect the tangled source for current block.
Return the list of block attributes needed by
`org-babel-tangle-collect-blocks'.  When ONLY-THIS-BLOCK is
non-nil, return the full association list to be used by
`org-babel-tangle' directly."
  (let* ((info (org-babel-get-src-block-info))
	 (start-line
	  (save-restriction (widen)
			    (+ 1 (line-number-at-pos (point)))))
	 (file (buffer-file-name (buffer-base-buffer)))
	 (src-lang (nth 0 info))
	 (params (nth 2 info))
	 (extra (nth 3 info))
         (coderef (nth 6 info))
	 (cref-regexp (org-src-coderef-regexp coderef))
	 (link (org-babel-tangle--unbracketed-link params))
	 (source-name
	  (or (nth 4 info)
	      (format "%s:%d"
		      (or (ignore-errors (nth 4 (org-heading-components)))
			  "No heading")
		      block-counter)))
	 (expand-cmd (intern (concat "org-babel-expand-body:" src-lang)))
	 (assignments-cmd
	  (intern (concat "org-babel-variable-assignments:" src-lang)))
	 (body
	  ;; Run the tangle-body-hook.
          (let ((body (if (org-babel-noweb-p params :tangle)
                          (if (string= "strip-tangle" (cdr (assq :noweb (nth 2 info))))
                            (replace-regexp-in-string (org-babel-noweb-wrap) "" (nth 1 info))
			    (org-babel-expand-noweb-references info))
			(nth 1 info))))
	    (with-temp-buffer
	      (insert
	       ;; Expand body in language specific manner.
	       (cond ((assq :no-expand params) body)
		     ((fboundp expand-cmd) (funcall expand-cmd body params))
		     (t
		      (org-babel-expand-body:generic
		       body params (and (fboundp assignments-cmd)
					(funcall assignments-cmd params))))))
	      (when (string-match "-r" extra)
		(goto-char (point-min))
		(while (re-search-forward cref-regexp nil t)
		  (replace-match "")))
	      (run-hooks 'org-babel-tangle-body-hook)
	      (buffer-string))))
	 (comment
	  (when (or (string= "both" (cdr (assq :comments params)))
		    (string= "org" (cdr (assq :comments params))))
	    ;; From the previous heading or code-block end
	    (funcall
	     org-babel-process-comment-text
	     (buffer-substring
	      (max (condition-case nil
		       (save-excursion
			 (org-back-to-heading t)
			 (re-search-forward org-outline-regexp))
		     (error (point-min)))
		   (save-excursion
		     (if (re-search-backward
			  org-babel-src-block-regexp nil t)
			 (match-end 0)
		       (point-min))))
	      (point)))))
         (src-tfile (cdr (assq :tangle params)))
	 (result
	  (list start-line
		(if org-babel-tangle-use-relative-file-links
		    (file-relative-name file)
		  file)
		link
		source-name
		params
		(if org-src-preserve-indentation
		    (org-trim body t)
		  (org-trim (org-remove-indentation body)))
		comment)))
    (if only-this-block
        (let* ((file-name (org-babel-effective-tangled-filename
                           (nth 1 result) src-lang src-tfile)))
          (list (cons file-name (list (cons src-lang result)))))
      result)))

(defun org-babel-tangle-comment-links (&optional info)
  "Return a list of begin and end link comments for the code block at point.
INFO, when non nil, is the source block information, as returned
by `org-babel-get-src-block-info'."
  (let ((link-data (pcase (or info (org-babel-get-src-block-info 'no-eval))
		     (`(,_ ,_ ,params ,_ ,name ,start ,_)
		      `(("start-line" . ,(org-with-point-at start
					   (number-to-string
					    (line-number-at-pos))))
			("file" . ,(buffer-file-name))
			("link" . ,(org-babel-tangle--unbracketed-link params))
			("source-name" . ,name))))))
    (list (org-fill-template org-babel-tangle-comment-format-beg link-data)
	  (org-fill-template org-babel-tangle-comment-format-end link-data))))

;; de-tangling functions
(defun org-babel-detangle (&optional source-code-file)
  "Propagate changes in source file back original to Org file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found."
  (interactive)
  (save-excursion
    (when source-code-file (find-file source-code-file))
    (goto-char (point-min))
    (let ((counter 0) new-body end)
      (while (re-search-forward org-link-bracket-re nil t)
        (if (and (match-string 2)
		 (re-search-forward
		  (concat " " (regexp-quote (match-string 2)) " ends here") nil t))
	    (progn (setq end (match-end 0))
		   (forward-line -1)
		   (save-excursion
		     (when (setq new-body (org-babel-tangle-jump-to-org))
		       (org-babel-update-block-body new-body)))
		   (setq counter (+ 1 counter)))
	  (setq end (point)))
        (goto-char end))
      (prog1 counter (message "Detangled %d code blocks" counter)))))

(defun org-babel-tangle-jump-to-org ()
  "Jump from a tangled code file to the related Org mode file."
  (interactive)
  (let ((mid (point))
	start body-start end target-buffer target-char link block-name body)
    (save-window-excursion
      (save-excursion
	(while (and (re-search-backward org-link-bracket-re nil t)
		    (not ; ever wider searches until matching block comments
		     (and (setq start (line-beginning-position))
			  (setq body-start (line-beginning-position 2))
			  (setq link (match-string 0))
			  (setq block-name (match-string 2))
			  (save-excursion
			    (save-match-data
			      (re-search-forward
			       (concat " " (regexp-quote block-name)
				       " ends here")
			       nil t)
			      (setq end (line-beginning-position))))))))
	(unless (and start (< start mid) (< mid end))
	  (error "Not in tangled code"))
        (setq body (buffer-substring body-start end)))
      ;; Go to the beginning of the relative block in Org file.
      ;; Explicitly allow fuzzy search even if user customized
      ;; otherwise.
      (let (org-link-search-must-match-exact-headline)
        (org-link-open-from-string link))
      (setq target-buffer (current-buffer))
      (if (string-match "[^ \t\n\r]:\\([[:digit:]]+\\)" block-name)
          (let ((n (string-to-number (match-string 1 block-name))))
	    (if (org-before-first-heading-p) (goto-char (point-min))
	      (org-back-to-heading t))
	    ;; Do not skip the first block if it begins at point min.
	    (cond ((or (org-at-heading-p)
		       (not (eq (org-element-type (org-element-at-point))
				'src-block)))
		   (org-babel-next-src-block n))
		  ((= n 1))
		  (t (org-babel-next-src-block (1- n)))))
        (org-babel-goto-named-src-block block-name))
      (goto-char (org-babel-where-is-src-block-head))
      (forward-line 1)
      ;; Try to preserve location of point within the source code in
      ;; tangled code file.
      (let ((offset (- mid body-start)))
	(when (> end (+ offset (point)))
	  (forward-char offset)))
      (setq target-char (point)))
    (org-src-switch-to-buffer target-buffer t)
    (goto-char target-char)
    body))

(provide 'ob-tangle)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-tangle.el ends here
