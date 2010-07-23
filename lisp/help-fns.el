;;; help-fns.el --- Complex help functions

;; Copyright (C) 1985, 1986, 1993, 1994, 1998, 1999, 2000, 2001, 2002,
;;   2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal

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

;; This file contains those help commands which are complicated, and
;; which may not be used in every session.  For example
;; `describe-function' will probably be heavily used when doing elisp
;; programming, but not if just editing C files.  Simpler help commands
;; are in help.el

;;; Code:

;; Functions

;;;###autoload
(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t nil nil
				(and fn (symbol-name fn))))
     (list (if (equal val "")
	       fn (intern val)))))
  (if (null function)
      (message "You didn't specify a function")
    (help-setup-xref (list #'describe-function function)
		     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
	(prin1 function)
	;; Use " is " instead of a colon so that
	;; it is easier to get out the function name using forward-sexp.
	(princ " is ")
	(describe-function-1 function)
	(with-current-buffer standard-output
	  ;; Return the text we displayed.
	  (buffer-string))))))

(defun help-split-fundoc (docstring def)
  "Split a function DOCSTRING into the actual doc and the usage info.
Return (USAGE . DOC) or nil if there's no usage info.
DEF is the function whose usage we're looking for in DOCSTRING."
  ;; Functions can get the calling sequence at the end of the doc string.
  ;; In cases where `function' has been fset to a subr we can't search for
  ;; function's name in the doc string so we use `fn' as the anonymous
  ;; function name instead.
  (when (and docstring (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring))
    (cons (format "(%s%s"
		  ;; Replace `fn' with the actual function name.
		  (if (consp def) "anonymous" def)
		  (match-string 1 docstring))
	  (substring docstring 0 (match-beginning 0)))))

(defun help-add-fundoc-usage (docstring arglist)
  "Add the usage info to DOCSTRING.
If DOCSTRING already has a usage info, then just return it unchanged.
The usage info is built from ARGLIST.  DOCSTRING can be nil.
ARGLIST can also be t or a string of the form \"(FUN ARG1 ARG2 ...)\"."
  (unless (stringp docstring) (setq docstring "Not documented"))
  (if (or (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring) (eq arglist t))
      docstring
    (concat docstring
	    (if (string-match "\n?\n\\'" docstring)
		(if (< (- (match-end 0) (match-beginning 0)) 2) "\n" "")
	      "\n\n")
	    (if (and (stringp arglist)
		     (string-match "\\`([^ ]+\\(.*\\))\\'" arglist))
		(concat "(fn" (match-string 1 arglist) ")")
	      (format "%S" (help-make-usage 'fn arglist))))))

(defun help-function-arglist (def)
  ;; Handle symbols aliased to other symbols.
  (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
  ;; If definition is a macro, find the function inside it.
  (if (eq (car-safe def) 'macro) (setq def (cdr def)))
  (cond
   ((byte-code-function-p def) (aref def 0))
   ((eq (car-safe def) 'lambda) (nth 1 def))
   ((and (eq (car-safe def) 'autoload) (not (eq (nth 4 def) 'keymap)))
    "[Arg list not available until function definition is loaded.]")
   (t t)))

(defun help-make-usage (function arglist)
  (cons (if (symbolp function) function 'anonymous)
	(mapcar (lambda (arg)
		  (if (not (symbolp arg))
		      (if (and (consp arg) (symbolp (car arg)))
			  ;; CL style default values for optional args.
			  (cons (intern (upcase (symbol-name (car arg))))
				(cdr arg))
			arg)
		    (let ((name (symbol-name arg)))
		      (if (string-match "\\`&" name) arg
			(intern (upcase name))))))
		arglist)))

;; Could be this, if we make symbol-file do the work below.
;; (defun help-C-file-name (subr-or-var kind)
;;   "Return the name of the C file where SUBR-OR-VAR is defined.
;; KIND should be `var' for a variable or `subr' for a subroutine."
;;   (symbol-file (if (symbolp subr-or-var) subr-or-var
;; 		 (subr-name subr-or-var))
;; 	       (if (eq kind 'var) 'defvar 'defun)))
;;;###autoload
(defun help-C-file-name (subr-or-var kind)
  "Return the name of the C file where SUBR-OR-VAR is defined.
KIND should be `var' for a variable or `subr' for a subroutine."
  (let ((docbuf (get-buffer-create " *DOC*"))
	(name (if (eq 'var kind)
		  (concat "V" (symbol-name subr-or-var))
		(concat "F" (subr-name subr-or-var)))))
    (with-current-buffer docbuf
      (goto-char (point-min))
      (if (eobp)
	  (insert-file-contents-literally
	   (expand-file-name internal-doc-file-name doc-directory)))
      (let ((file (catch 'loop
		    (while t
		      (let ((pnt (search-forward (concat "" name "\n"))))
			(re-search-backward "S\\(.*\\)")
			(let ((file (match-string 1)))
			  (if (member file build-files)
			      (throw 'loop file)
			    (goto-char pnt))))))))
	(if (string-match "^ns.*\\(\\.o\\|obj\\)\\'" file)
	    (setq file (replace-match ".m" t t file 1))
	  (if (string-match "\\.\\(o\\|obj\\)\\'" file)
	      (setq file (replace-match ".c" t t file))))
	(if (string-match "\\.\\(c\\|m\\)\\'" file)
	    (concat "src/" file)
	  file)))))

(defcustom help-downcase-arguments nil
  "If non-nil, argument names in *Help* buffers are downcased."
  :type 'boolean
  :group 'help
  :version "23.2")

(defun help-highlight-arg (arg)
  "Highlight ARG as an argument name for a *Help* buffer.
Return ARG in face `help-argument-name'; ARG is also downcased
if the variable `help-downcase-arguments' is non-nil."
  (propertize (if help-downcase-arguments (downcase arg) arg)
	      'face 'help-argument-name))

(defun help-do-arg-highlight (doc args)
  (with-syntax-table (make-syntax-table emacs-lisp-mode-syntax-table)
    (modify-syntax-entry ?\- "w")
    (dolist (arg args doc)
      (setq doc (replace-regexp-in-string
                 ;; This is heuristic, but covers all common cases
                 ;; except ARG1-ARG2
                 (concat "\\<"                   ; beginning of word
                         "\\(?:[a-z-]*-\\)?"     ; for xxx-ARG
                         "\\("
                         (regexp-quote arg)
                         "\\)"
                         "\\(?:es\\|s\\|th\\)?"  ; for ARGth, ARGs
                         "\\(?:-[a-z0-9-]+\\)?"  ; for ARG-xxx, ARG-n
                         "\\(?:-[{([<`\"].*?\\)?"; for ARG-{x}, (x), <x>, [x], `x'
                         "\\>")                  ; end of word
                 (help-highlight-arg arg)
                 doc t t 1)))))

(defun help-highlight-arguments (usage doc &rest args)
  (when usage
    (with-temp-buffer
      (insert usage)
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (next (not (or args (looking-at "\\["))))
            (opt nil))
        ;; Make a list of all arguments
        (skip-chars-forward "^ ")
        (while next
          (or opt (not (looking-at " &")) (setq opt t))
          (if (not (re-search-forward " \\([\\[(]*\\)\\([^] &)\.]+\\)" nil t))
              (setq next nil)
            (setq args (cons (match-string 2) args))
            (when (and opt (string= (match-string 1) "("))
              ;; A pesky CL-style optional argument with default value,
              ;; so let's skip over it
              (search-backward "(")
              (goto-char (scan-sexps (point) 1)))))
        ;; Highlight aguments in the USAGE string
        (setq usage (help-do-arg-highlight (buffer-string) args))
        ;; Highlight arguments in the DOC string
        (setq doc (and doc (help-do-arg-highlight doc args))))))
  ;; Return value is like the one from help-split-fundoc, but highlighted
  (cons usage doc))

;; The following function was compiled from the former functions
;; `describe-simplify-lib-file-name' and `find-source-lisp-file' with
;; some excerpts from `describe-function-1' and `describe-variable'.
;; The only additional twists provided are (1) locate the defining file
;; for autoloaded functions, and (2) give preference to files in the
;; "install directory" (directories found via `load-path') rather than
;; to files in the "compile directory" (directories found by searching
;; the loaddefs.el file).  We autoload it because it's also used by
;; `describe-face' (instead of `describe-simplify-lib-file-name').

;;;###autoload
(defun find-lisp-object-file-name (object type)
  "Guess the file that defined the Lisp object OBJECT, of type TYPE.
OBJECT should be a symbol associated with a function, variable, or face;
  alternatively, it can be a function definition.
If TYPE is `defvar', search for a variable definition.
If TYPE is `defface', search for a face definition.
If TYPE is the value returned by `symbol-function' for a function symbol,
 search for a function definition.

The return value is the absolute name of a readable file where OBJECT is
defined.  If several such files exist, preference is given to a file
found via `load-path'.  The return value can also be `C-source', which
means that OBJECT is a function or variable defined in C.  If no
suitable file is found, return nil."
  (let* ((autoloaded (eq (car-safe type) 'autoload))
	 (file-name (or (and autoloaded (nth 1 type))
			(symbol-file
			 object (if (memq type (list 'defvar 'defface))
				    type
				  'defun)))))
    (cond
     (autoloaded
      ;; An autoloaded function: Locate the file since `symbol-function'
      ;; has only returned a bare string here.
      (setq file-name
	    (locate-file file-name load-path '(".el" ".elc") 'readable)))
     ((and (stringp file-name)
	   (string-match "[.]*loaddefs.el\\'" file-name))
      ;; An autoloaded variable or face.  Visit loaddefs.el in a buffer
      ;; and try to extract the defining file.  The following form is
      ;; from `describe-function-1' and `describe-variable'.
      (let ((location
	     (condition-case nil
		 (find-function-search-for-symbol object nil file-name)
	       (error nil))))
	(when (cdr location)
	  (with-current-buffer (car location)
	    (goto-char (cdr location))
	    (when (re-search-backward
		   "^;;; Generated autoloads from \\(.*\\)" nil t)
	      (setq file-name
		    (locate-file
		     (file-name-sans-extension
		      (match-string-no-properties 1))
		     load-path '(".el" ".elc") 'readable))))))))

    (cond
     ((and (not file-name) (subrp type))
      ;; A built-in function.  The form is from `describe-function-1'.
      (if (get-buffer " *DOC*")
	  (help-C-file-name type 'subr)
	'C-source))
     ((and (not file-name) (symbolp object)
	   (integerp (get object 'variable-documentation)))
      ;; A variable defined in C.  The form is from `describe-variable'.
      (if (get-buffer " *DOC*")
	  (help-C-file-name object 'var)
	'C-source))
     ((not (stringp file-name))
      ;; If we don't have a file-name string by now, we lost.
      nil)
     ((let ((lib-name
	     (if (string-match "[.]elc\\'" file-name)
		 (substring-no-properties file-name 0 -1)
	       file-name)))
	;; When the Elisp source file can be found in the install
	;; directory return the name of that file - `file-name' should
	;; have become an absolute file name ny now.
	(or (and (file-readable-p lib-name) lib-name)
	    ;; The library might be compressed.
	    (and (file-readable-p (concat lib-name ".gz")) lib-name))))
     ((let* ((lib-name (file-name-nondirectory file-name))
	     ;; The next form is from `describe-simplify-lib-file-name'.
	     (file-name
	      ;; Try converting the absolute file name to a library
	      ;; name, convert that back to a file name and see if we
	      ;; get the original one.  If so, they are equivalent.
	      (if (equal file-name (locate-file lib-name load-path '("")))
		  (if (string-match "[.]elc\\'" lib-name)
		      (substring-no-properties lib-name 0 -1)
		    lib-name)
		file-name))
	     ;; The next three forms are from `find-source-lisp-file'.
	     (elc-file (locate-file
			(concat file-name
				(if (string-match "\\.el\\'" file-name)
				    "c"
				  ".elc"))
			load-path nil 'readable))
	     (str (when elc-file
		    (with-temp-buffer
		      (insert-file-contents-literally elc-file nil 0 256)
		      (buffer-string))))
	     (src-file (and str
			    (string-match ";;; from file \\(.*\\.el\\)" str)
			    (match-string 1 str))))
	(and src-file (file-readable-p src-file) src-file))))))

(declare-function ad-get-advice-info "advice" (function))

;;;###autoload
(defun describe-function-1 (function)
  (let* ((advised (and (symbolp function) (featurep 'advice)
		       (ad-get-advice-info function)))
	 ;; If the function is advised, use the symbol that has the
	 ;; real definition, if that symbol is already set up.
	 (real-function
	  (or (and advised
		   (let ((origname (cdr (assq 'origname advised))))
		     (and (fboundp origname) origname)))
	      function))
	 ;; Get the real definition.
	 (def (if (symbolp real-function)
		  (symbol-function real-function)
		function))
	 file-name string
	 (beg (if (commandp def) "an interactive " "a "))
         (pt1 (with-current-buffer (help-buffer) (point)))
	 errtype)
    (setq string
	  (cond ((or (stringp def)
		     (vectorp def))
		 "a keyboard macro")
		((subrp def)
		 (if (eq 'unevalled (cdr (subr-arity def)))
		     (concat beg "special form")
		   (concat beg "built-in function")))
		((byte-code-function-p def)
		 (concat beg "compiled Lisp function"))
		((symbolp def)
		 (while (and (fboundp def)
			     (symbolp (symbol-function def)))
		   (setq def (symbol-function def)))
		 ;; Handle (defalias 'foo 'bar), where bar is undefined.
		 (or (fboundp def) (setq errtype 'alias))
		 (format "an alias for `%s'" def))
		((eq (car-safe def) 'lambda)
		 (concat beg "Lisp function"))
		((eq (car-safe def) 'macro)
		 "a Lisp macro")
		((eq (car-safe def) 'autoload)
		 (format "%s autoloaded %s"
			 (if (commandp def) "an interactive" "an")
			 (if (eq (nth 4 def) 'keymap) "keymap"
			   (if (nth 4 def) "Lisp macro" "Lisp function"))))
                ((keymapp def)
                 (let ((is-full nil)
                       (elts (cdr-safe def)))
                   (while elts
                     (if (char-table-p (car-safe elts))
                         (setq is-full t
                               elts nil))
                     (setq elts (cdr-safe elts)))
                   (if is-full
                       "a full keymap"
                     "a sparse keymap")))
		(t "")))
    (princ string)
    (if (eq errtype 'alias)
	(princ ",\nwhich is not defined.  Please make a bug report.")
      (with-current-buffer standard-output
	(save-excursion
	  (save-match-data
	    (when (re-search-backward "alias for `\\([^`']+\\)'" nil t)
	      (help-xref-button 1 'help-function def)))))

      (setq file-name (find-lisp-object-file-name function def))
      (when file-name
	(princ " in `")
	;; We used to add .el to the file name,
	;; but that's completely wrong when the user used load-file.
	(princ (if (eq file-name 'C-source)
		   "C source code"
		 (file-name-nondirectory file-name)))
	(princ "'")
	;; Make a hyperlink to the library.
	(with-current-buffer standard-output
	  (save-excursion
	    (re-search-backward "`\\([^`']+\\)'" nil t)
	    (help-xref-button 1 'help-function-def function file-name))))
      (princ ".")
      (with-current-buffer (help-buffer)
	(fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point))
				  (point)))
      (terpri)(terpri)
      (when (commandp function)
	(let ((pt2 (with-current-buffer (help-buffer) (point)))
	      (remapped (command-remapping function)))
	  (unless (memq remapped '(ignore undefined))
	    (let ((keys (where-is-internal
			 (or remapped function) overriding-local-map nil nil))
		  non-modified-keys)
	      (if (and (eq function 'self-insert-command)
		       (vectorp (car-safe keys))
		       (consp (aref (car keys) 0)))
		  (princ "It is bound to many ordinary text characters.\n")
		;; Which non-control non-meta keys run this command?
		(dolist (key keys)
		  (if (member (event-modifiers (aref key 0)) '(nil (shift)))
		      (push key non-modified-keys)))
		(when remapped
		  (princ "It is remapped to `")
		  (princ (symbol-name remapped))
		  (princ "'"))

		(when keys
		  (princ (if remapped ", which is bound to " "It is bound to "))
		  ;; If lots of ordinary text characters run this command,
		  ;; don't mention them one by one.
		  (if (< (length non-modified-keys) 10)
		      (princ (mapconcat 'key-description keys ", "))
		    (dolist (key non-modified-keys)
		      (setq keys (delq key keys)))
		    (if keys
			(progn
			  (princ (mapconcat 'key-description keys ", "))
			  (princ ", and many ordinary text characters"))
		      (princ "many ordinary text characters"))))
		(when (or remapped keys non-modified-keys)
		  (princ ".")
		  (terpri)))))

	  (with-current-buffer (help-buffer)
	    (fill-region-as-paragraph pt2 (point))
	    (unless (looking-back "\n\n")
	      (terpri)))))
      ;; Note that list* etc do not get this property until
      ;; cl-hack-byte-compiler runs, after bytecomp is loaded.
      (when (and (symbolp function)
                 (eq (get function 'byte-compile)
                     'cl-byte-compile-compiler-macro))
	(princ "This function has a compiler macro")
	(let ((lib (get function 'compiler-macro-file)))
	  (when (stringp lib)
	    (princ (format " in `%s'" lib))
	    (with-current-buffer standard-output
	      (save-excursion
		(re-search-backward "`\\([^`']+\\)'" nil t)
		(help-xref-button 1 'help-function-cmacro function lib)))))
	(princ ".\n\n"))
      (let* ((advertised (gethash def advertised-signature-table t))
	     (arglist (if (listp advertised)
			  advertised (help-function-arglist def)))
	     (doc (documentation function))
	     (usage (help-split-fundoc doc function)))
	(with-current-buffer standard-output
	  ;; If definition is a keymap, skip arglist note.
	  (unless (keymapp function)
	    (if usage (setq doc (cdr usage)))
	    (let* ((use (cond
			 ((and usage (not (listp advertised))) (car usage))
			 ((listp arglist)
			  (format "%S" (help-make-usage function arglist)))
			 ((stringp arglist) arglist)
			 ;; Maybe the arglist is in the docstring of a symbol
			 ;; this one is aliased to.
			 ((let ((fun real-function))
			    (while (and (symbolp fun)
					(setq fun (symbol-function fun))
					(not (setq usage (help-split-fundoc
							  (documentation fun)
							  function)))))
			    usage)
			  (car usage))
			 ((or (stringp def)
			      (vectorp def))
			  (format "\nMacro: %s" (format-kbd-macro def)))
			 (t "[Missing arglist.  Please make a bug report.]")))
		   (high (help-highlight-arguments use doc)))
	      (let ((fill-begin (point)))
		(insert (car high) "\n")
		(fill-region fill-begin (point)))
	      (setq doc (cdr high))))
	  (let* ((obsolete (and
			    ;; function might be a lambda construct.
			    (symbolp function)
			    (get function 'byte-obsolete-info)))
		 (use (car obsolete)))
	    (when obsolete
	      (princ "\nThis function is obsolete")
	      (when (nth 2 obsolete)
		(insert (format " since %s" (nth 2 obsolete))))
	      (insert (cond ((stringp use) (concat ";\n" use))
			    (use (format ";\nuse `%s' instead." use))
			    (t "."))
		      "\n"))
	    (insert "\n"
		    (or doc "Not documented."))))))))


;; Variables

;;;###autoload
(defun variable-at-point (&optional any-symbol)
  "Return the bound variable symbol found at or before point.
Return 0 if there is no such symbol.
If ANY-SYMBOL is non-nil, don't insist the symbol be bound."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
	    (save-excursion
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) (boundp obj) obj)))
          (error nil))
        (let* ((str (find-tag-default))
               (sym (if str (intern-soft str))))
          (if (and sym (or any-symbol (boundp sym)))
              sym
            (save-match-data
              (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                (setq sym (intern-soft (match-string 1 str)))
                (and (or any-symbol (boundp sym)) sym)))))
        0)))

(defun describe-variable-custom-version-info (variable)
  (let ((custom-version (get variable 'custom-version))
	(cpv (get variable 'custom-package-version))
	(output nil))
    (if custom-version
	(setq output
	      (format "This variable was introduced, or its default value was changed, in\nversion %s of Emacs.\n"
		      custom-version))
      (when cpv
	(let* ((package (car-safe cpv))
	       (version (if (listp (cdr-safe cpv))
			    (car (cdr-safe cpv))
			  (cdr-safe cpv)))
	       (pkg-versions (assq package customize-package-emacs-version-alist))
	       (emacsv (cdr (assoc version pkg-versions))))
	  (if (and package version)
	      (setq output
		    (format (concat "This variable was introduced, or its default value was changed, in\nversion %s of the %s package"
				    (if emacsv
					(format " that is part of Emacs %s" emacsv))
				    ".\n")
			    version package))))))
    output))

;;;###autoload
(defun describe-variable (variable &optional buffer frame)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also.
If VARIABLE has a buffer-local value in BUFFER or FRAME
\(default to the current buffer and current frame),
it is displayed along with the global value."
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if (symbolp v)
				    (format
				     "Describe variable (default %s): " v)
				  "Describe variable: ")
				obarray
				'(lambda (vv)
				   (or (boundp vv)
				       (get vv 'variable-documentation)))
				t nil nil
				(if (symbolp v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (let (file-name)
    (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
    (unless (frame-live-p frame) (setq frame (selected-frame)))
    (if (not (symbolp variable))
	(message "You did not specify a variable")
      (save-excursion
	(let ((valvoid (not (with-current-buffer buffer (boundp variable))))
	      val val-start-pos locus)
	  ;; Extract the value before setting up the output buffer,
	  ;; in case `buffer' *is* the output buffer.
	  (unless valvoid
	    (with-selected-frame frame
	      (with-current-buffer buffer
		(setq val (symbol-value variable)
		      locus (variable-binding-locus variable)))))
	  (help-setup-xref (list #'describe-variable variable buffer)
			   (called-interactively-p 'interactive))
	  (with-help-window (help-buffer)
	    (with-current-buffer buffer
	      (prin1 variable)
	      (setq file-name (find-lisp-object-file-name variable 'defvar))

	      (if file-name
		  (progn
		    (princ " is a variable defined in `")
		    (princ (if (eq file-name 'C-source)
			       "C source code"
			     (file-name-nondirectory file-name)))
		    (princ "'.\n")
		    (with-current-buffer standard-output
		      (save-excursion
			(re-search-backward "`\\([^`']+\\)'" nil t)
			(help-xref-button 1 'help-variable-def
					  variable file-name)))
		    (if valvoid
			(princ "It is void as a variable.")
		      (princ "Its ")))
		(if valvoid
		    (princ " is void as a variable.")
		  (princ "'s "))))
	    (if valvoid
		nil
	      (with-current-buffer standard-output
		(setq val-start-pos (point))
		(princ "value is ")
		(terpri)
		(let ((from (point)))
		  (pp val)
		  ;; Hyperlinks in variable's value are quite frequently
		  ;; inappropriate e.g C-h v <RET> features <RET>
		  ;; (help-xref-on-pp from (point))
		  (if (< (point) (+ from 20))
		      (delete-region (1- from) from)))))
	    (terpri)

	    (when locus
	      (if (bufferp locus)
		  (princ (format "%socal in buffer %s; "
				 (if (get variable 'permanent-local)
				     "Permanently l" "L")
				 (buffer-name)))
		(princ (format "It is a frame-local variable; ")))
	      (if (not (default-boundp variable))
		  (princ "globally void")
		(let ((val (default-value variable)))
		  (with-current-buffer standard-output
		    (princ "global value is ")
		    (terpri)
		    ;; Fixme: pp can take an age if you happen to
		    ;; ask for a very large expression.  We should
		    ;; probably print it raw once and check it's a
		    ;; sensible size before prettyprinting.  -- fx
		    (let ((from (point)))
		      (pp val)
		      ;; See previous comment for this function.
		      ;; (help-xref-on-pp from (point))
		      (if (< (point) (+ from 20))
			  (delete-region (1- from) from))))))
              (terpri))

	    ;; If the value is large, move it to the end.
	    (with-current-buffer standard-output
	      (when (> (count-lines (point-min) (point-max)) 10)
		;; Note that setting the syntax table like below
		;; makes forward-sexp move over a `'s' at the end
		;; of a symbol.
		(set-syntax-table emacs-lisp-mode-syntax-table)
		(goto-char val-start-pos)
		;; The line below previously read as
		;; (delete-region (point) (progn (end-of-line) (point)))
		;; which suppressed display of the buffer local value for
		;; large values.
		(when (looking-at "value is") (replace-match ""))
		(save-excursion
		  (insert "\n\nValue:")
		  (set (make-local-variable 'help-button-cache)
		       (point-marker)))
		(insert "value is shown ")
		(insert-button "below"
			       'action help-button-cache
			       'follow-link t
			       'help-echo "mouse-2, RET: show value")
		(insert ".\n")))
            (terpri)

            (let* ((alias (condition-case nil
                              (indirect-variable variable)
                            (error variable)))
                   (obsolete (get variable 'byte-obsolete-variable))
		   (use (car obsolete))
		   (safe-var (get variable 'safe-local-variable))
                   (doc (or (documentation-property variable 'variable-documentation)
                            (documentation-property alias 'variable-documentation)))
                   (extra-line nil))
              ;; Add a note for variables that have been make-var-buffer-local.
              (when (and (local-variable-if-set-p variable)
                         (or (not (local-variable-p variable))
                             (with-temp-buffer
                               (local-variable-if-set-p variable))))
                (setq extra-line t)
                (princ "  Automatically becomes buffer-local when set in any fashion.\n"))

              ;; Mention if it's an alias
              (unless (eq alias variable)
                (setq extra-line t)
                (princ (format "  This variable is an alias for `%s'.\n" alias)))

              (when obsolete
                (setq extra-line t)
                (princ "  This variable is obsolete")
                (if (cdr obsolete) (princ (format " since %s" (cdr obsolete))))
		(princ (cond ((stringp use) (concat ";\n  " use))
			     (use (format ";\n  use `%s' instead." (car obsolete)))
			     (t ".")))
                (terpri))

	      (when (member (cons variable val) file-local-variables-alist)
		(setq extra-line t)
		(if (member (cons variable val) dir-local-variables-alist)
		    (let ((file (and (buffer-file-name)
				     (not (file-remote-p (buffer-file-name)))
				     (dir-locals-find-file (buffer-file-name)))))
		      (princ "  This variable is a directory local variable")
		      (when file
			(princ (concat "\n  from the file \""
				       (if (consp file)
					   (car file)
					 file)
				       "\"")))
		      (princ ".\n"))
		  (princ "  This variable is a file local variable.\n")))

	      (when (memq variable ignored-local-variables)
		(setq extra-line t)
		(princ "  This variable is ignored when used as a file local \
variable.\n"))

	      ;; Can be both risky and safe, eg auto-fill-function.
	      (when (risky-local-variable-p variable)
		(setq extra-line t)
		(princ "  This variable is potentially risky when used as a \
file local variable.\n")
		(when (assq variable safe-local-variable-values)
		  (princ "  However, you have added it to \
`safe-local-variable-values'.\n")))

	      (when safe-var
                (setq extra-line t)
		(princ "  This variable is safe as a file local variable ")
		(princ "if its value\n  satisfies the predicate ")
		(princ (if (byte-code-function-p safe-var)
			   "which is byte-compiled expression.\n"
			 (format "`%s'.\n" safe-var))))

              (if extra-line (terpri))
	      (princ "Documentation:\n")
	      (with-current-buffer standard-output
		(insert (or doc "Not documented as a variable."))))

	    ;; Make a link to customize if this variable can be customized.
	    (when (custom-variable-p variable)
	      (let ((customize-label "customize"))
		(terpri)
		(terpri)
		(princ (concat "You can " customize-label " this variable."))
		(with-current-buffer standard-output
		  (save-excursion
		    (re-search-backward
		     (concat "\\(" customize-label "\\)") nil t)
		    (help-xref-button 1 'help-customize-variable variable))))
	      ;; Note variable's version or package version
	      (let ((output (describe-variable-custom-version-info variable)))
		(when output
		  (terpri)
		  (terpri)
		  (princ output))))

	    (with-current-buffer standard-output
	      ;; Return the text we displayed.
	      (buffer-string))))))))


;;;###autoload
(defun describe-syntax (&optional buffer)
  "Describe the syntax specifications in the syntax table of BUFFER.
The descriptions are inserted in a help buffer, which is then displayed.
BUFFER defaults to the current buffer."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (help-setup-xref (list #'describe-syntax buffer)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (let ((table (with-current-buffer buffer (syntax-table))))
      (with-current-buffer standard-output
	(describe-vector table 'internal-describe-syntax-value)
	(while (setq table (char-table-parent table))
	  (insert "\nThe parent syntax table is:")
	  (describe-vector table 'internal-describe-syntax-value))))))

(defun help-describe-category-set (value)
  (insert (cond
	   ((null value) "default")
	   ((char-table-p value) "deeper char-table ...")
	   (t (condition-case err
		  (category-set-mnemonics value)
		(error "invalid"))))))

;;;###autoload
(defun describe-categories (&optional buffer)
  "Describe the category specifications in the current category table.
The descriptions are inserted in a buffer, which is then displayed.
If BUFFER is non-nil, then describe BUFFER's category table instead.
BUFFER should be a buffer or a buffer name."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (help-setup-xref (list #'describe-categories buffer)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (let* ((table (with-current-buffer buffer (category-table)))
	   (docs (char-table-extra-slot table 0)))
      (if (or (not (vectorp docs)) (/= (length docs) 95))
	  (error "Invalid first extra slot in this category table\n"))
      (with-current-buffer standard-output
	(insert "Legend of category mnemonics (see the tail for the longer description)\n")
	(let ((pos (point)) (items 0) lines n)
	  (dotimes (i 95)
	    (if (aref docs i) (setq items (1+ items))))
	  (setq lines (1+ (/ (1- items) 4)))
	  (setq n 0)
	  (dotimes (i 95)
	    (let ((elt (aref docs i)))
	      (when elt
		(string-match ".*" elt)
		(setq elt (match-string 0 elt))
		(if (>= (length elt) 17)
		    (setq elt (concat (substring elt 0 14) "...")))
		(if (< (point) (point-max))
		    (move-to-column (* 20 (/ n lines)) t))
		(insert (+ i ?\s) ?: elt)
		(if (< (point) (point-max))
		    (forward-line 1)
		  (insert "\n"))
		(setq n (1+ n))
		(if (= (% n lines) 0)
		    (goto-char pos))))))
	(goto-char (point-max))
	(insert "\n"
		"character(s)\tcategory mnemonics\n"
		"------------\t------------------")
	(describe-vector table 'help-describe-category-set)
	(insert "Legend of category mnemonics:\n")
	(dotimes (i 95)
	  (let ((elt (aref docs i)))
	    (when elt
	      (if (string-match "\n" elt)
		  (setq elt (substring elt (match-end 0))))
	      (insert (+ i ?\s) ": " elt "\n"))))
	(while (setq table (char-table-parent table))
	  (insert "\nThe parent category table is:")
	  (describe-vector table 'help-describe-category-set))))))

(provide 'help-fns)

;; arch-tag: 9e10331c-ae81-4d13-965d-c4819aaab0b3
;;; help-fns.el ends here
