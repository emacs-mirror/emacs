;;; help-fns.el --- Complex help functions

;; Copyright (C) 1985, 86, 93, 94, 98, 1999, 2000, 01, 02, 03, 2004
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains those help commands which are complicated, and
;; which may not be used in every session.  For example
;; `describe-function' will probably be heavily used when doing elisp
;; programming, but not if just editing C files.  Simpler help commands
;; are in help.el

;;; Code:

(require 'help-mode)


;;;###autoload
(defun help-with-tutorial (&optional arg)
  "Select the Emacs learn-by-doing tutorial.
If there is a tutorial version written in the language
of the selected language environment, that version is used.
If there's no tutorial in that language, `TUTORIAL' is selected.
With ARG, you are asked to choose which language."
  (interactive "P")
  (let ((lang (if arg
                  (let ((minibuffer-setup-hook minibuffer-setup-hook))
                    (add-hook 'minibuffer-setup-hook
                              'minibuffer-completion-help)
                    (read-language-name 'tutorial "Language: " "English"))
		(if (get-language-info current-language-environment 'tutorial)
		    current-language-environment
		  "English")))
	file filename)
    (setq filename (get-language-info lang 'tutorial))
    (setq file (expand-file-name (concat "~/" filename)))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name filename data-directory))
      (hack-local-variables)
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      ;; Convert the <<...>> line to the proper [...] line,
      ;; or just delete the <<...>> line if a [...] line follows.
      (cond ((save-excursion
	       (forward-line 1)
	       (looking-at "\\["))
	     (delete-region (point) (progn (forward-line 1) (point))))
	    ((looking-at "<<Blank lines inserted.*>>")
	     (replace-match "[Middle of page left blank for didactic purposes.   Text continues below]"))
	    (t
	     (looking-at "<<")
	     (replace-match "[")
	     (search-forward ">>")
	     (replace-match "]")))
      (beginning-of-line)
      (let ((n (- (window-height (selected-window))
		  (count-lines (point-min) (point))
		  6)))
	(if (< n 8)
	    (progn
	      ;; For a short gap, we don't need the [...] line,
	      ;; so delete it.
	      (delete-region (point) (progn (end-of-line) (point)))
	      (newline n))
	  ;; Some people get confused by the large gap.
	  (newline (/ n 2))

	  ;; Skip the [...] line (don't delete it).
	  (forward-line 1)
	  (newline (- n (/ n 2)))))
      (goto-char (point-min))
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil))))

;;;###autoload
(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `\\[load-library]'
to find the file that `\\[load-library] RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normaly returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (completing-read "Locate library: "
				      'locate-file-completion
				      (cons load-path load-suffixes))
		     nil nil
		     t))
  (let ((file (locate-file library
			   (or path load-path)
			   (append (unless nosuffix load-suffixes) '("")))))
    (if interactive-call
	(if file
	    (message "Library is file %s" (abbreviate-file-name file))
	  (message "No library %s in search path" library)))
    file))


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
				obarray 'fboundp t nil nil (symbol-name fn)))
     (list (if (equal val "")
	       fn (intern val)))))
  (if (null function)
      (message "You didn't specify a function")
    (help-setup-xref (list #'describe-function function) (interactive-p))
    (save-excursion
      (with-output-to-temp-buffer (help-buffer)
	(prin1 function)
	;; Use " is " instead of a colon so that
	;; it is easier to get out the function name using forward-sexp.
	(princ " is ")
	(describe-function-1 function)
	(print-help-return-message)
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
  ;; and do the same for interpreted closures
  (if (eq (car-safe def) 'closure) (setq def (cddr def)))
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
	(if (string-match "\\.\\(o\\|obj\\)\\'" file)
	    (setq file (replace-match ".c" t t file)))
	(if (string-match "\\.c\\'" file)
	    (concat "src/" file)
	  file)))))

;;;###autoload
(defface help-argument-name '((((supports :slant italic)) :inherit italic))
  "Face to highlight argument names in *Help* buffers."
  :group 'help)

(defun help-default-arg-highlight (arg)
  "Default function to highlight arguments in *Help* buffers.
It returns ARG in face `help-argument-name'; ARG is also
downcased if it displays differently than the default
face (according to `face-differs-from-default-p')."
  (propertize (if (face-differs-from-default-p 'help-argument-name)
                  (downcase arg)
                arg)
              'face 'help-argument-name))

(defun help-do-arg-highlight (doc args)
  (with-syntax-table (make-syntax-table emacs-lisp-mode-syntax-table)
    (modify-syntax-entry ?\- "w")
    (while args
      (let ((arg (prog1 (car args) (setq args (cdr args)))))
        (setq doc (replace-regexp-in-string
                   ;; This is heuristic, but covers all common cases
                   ;; except ARG1-ARG2
                   (concat "\\<"                   ; beginning of word
                           "\\(?:[a-z-]*-\\)?"     ; for xxx-ARG
                           "\\("
                           (regexp-quote arg)
                           "\\)"
                           "\\(?:es\\|s\\|th\\)?"  ; for ARGth, ARGs
                           "\\(?:-[a-z-]+\\)?"     ; for ARG-xxx
                           "\\>")                  ; end of word
                   (help-default-arg-highlight arg)
                   doc t t 1))))
    doc))

(defun help-highlight-arguments (usage doc &rest args)
  (when (and usage (string-match "^(" usage))
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

;;;###autoload
(defun describe-function-1 (function)
  (let* ((def (if (symbolp function)
		  (symbol-function function)
		function))
	 file-name string
	 (beg (if (commandp def) "an interactive " "a ")))
    (setq string
	  (cond ((or (stringp def) (vectorp def))
		 "a keyboard macro")
		((subrp def)
		 (if (eq 'unevalled (cdr (subr-arity def)))
		     (concat beg "special form")
		   (concat beg "built-in function")))
		((byte-code-function-p def)
		 (concat beg "compiled Lisp function"))
		((and (funvecp def) (eq (aref def 0) 'curry))
		 (if (symbolp (aref def 1))
		     (format "a curried function calling `%s'" (aref def 1))
		   "a curried function"))
		((funvecp def)
		 (format "a function-vector (funvec) of type `%s'"
			 (aref def 0)))
		((symbolp def)
		 (while (symbolp (symbol-function def))
		   (setq def (symbol-function def)))
		 (format "an alias for `%s'" def))
		((eq (car-safe def) 'lambda)
		 (concat beg "Lisp function"))
		((eq (car-safe def) 'macro)
		 "a Lisp macro")
		((eq (car-safe def) 'closure)
		 (concat beg "Lisp closure"))
		((eq (car-safe def) 'autoload)
		 (setq file-name (nth 1 def))
		 (format "%s autoloaded %s"
			 (if (commandp def) "an interactive" "an")
			 (if (eq (nth 4 def) 'keymap) "keymap"
			   (if (nth 4 def) "Lisp macro" "Lisp function"))
			 ))
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
    (with-current-buffer standard-output
      (save-excursion
	(save-match-data
	  (if (re-search-backward "alias for `\\([^`']+\\)'" nil t)
	      (help-xref-button 1 'help-function def)))))
    (or file-name
	(setq file-name (symbol-file function 'defun)))
    (when (equal file-name "loaddefs.el")
      ;; Find the real def site of the preloaded function.
      ;; This is necessary only for defaliases.
      (let ((location
	     (condition-case nil
		 (find-function-search-for-symbol function nil "loaddefs.el")
	       (error nil))))
	(when location
	  (with-current-buffer (car location)
	    (goto-char (cdr location))
	    (when (re-search-backward
		   "^;;; Generated autoloads from \\(.*\\)" nil t)
	      (setq file-name (match-string 1)))))))
    (when (and (null file-name) (subrp def))
      ;; Find the C source file name.
      (setq file-name (if (get-buffer " *DOC*")
			  (help-C-file-name def 'subr)
			'C-source)))
    (when file-name
      (princ " in `")
      ;; We used to add .el to the file name,
      ;; but that's completely wrong when the user used load-file.
      (princ (if (eq file-name 'C-source) "C source code" file-name))
      (princ "'")
      ;; Make a hyperlink to the library.
      (with-current-buffer standard-output
        (save-excursion
	  (re-search-backward "`\\([^`']+\\)'" nil t)
	  (help-xref-button 1 'help-function-def function file-name))))
    (princ ".")
    (terpri)
    (when (commandp function)
      (let* ((remapped (command-remapping function))
	     (keys (where-is-internal
		    (or remapped function) overriding-local-map nil nil))
	     non-modified-keys)
	;; Which non-control non-meta keys run this command?
	(dolist (key keys)
	  (if (member (event-modifiers (aref key 0)) '(nil (shift)))
	      (push key non-modified-keys)))
	(when remapped
	  (princ "It is remapped to `")
	  (princ (symbol-name remapped))
	  (princ "'"))

	(when keys
	  (princ (if remapped " which is bound to " "It is bound to "))
	  ;; FIXME: This list can be very long (f.ex. for self-insert-command).
	  ;; If there are many, remove them from KEYS.
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
	  (terpri))))
    (let* ((arglist (help-function-arglist def))
	   (doc (documentation function))
	   (usage (help-split-fundoc doc function)))
      (with-current-buffer standard-output
        ;; If definition is a keymap, skip arglist note.
        (unless (keymapp def)
          (let* ((use (cond
                        (usage (setq doc (cdr usage)) (car usage))
                        ((listp arglist)
                         (format "%S" (help-make-usage function arglist)))
                        ((stringp arglist) arglist)
                        ;; Maybe the arglist is in the docstring of the alias.
                        ((let ((fun function))
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
			((and (funvecp def) (eq (aref def 0) 'curry))
			 ;; Describe a curried-function's function and args
			 (let ((slot 0))
			   (mapconcat (lambda (arg)
					(setq slot (1+ slot))
					(cond
					 ((= slot 1) "")
					 ((= slot 2)
					  (format "  Function: %S" arg))
					 (t
					  (format "Argument %d: %S"
						  (- slot 3) arg))))
				      def
				      "\n")))
			((funvecp def) nil)
                        (t "[Missing arglist.  Please make a bug report.]")))
                 (high (help-highlight-arguments use doc)))
	    (when (car high)
	      (insert (car high) "\n"))
            (setq doc (cdr high))))
        (let ((obsolete (and
                         ;; function might be a lambda construct.
                         (symbolp function)
                         (get function 'byte-obsolete-info))))
          (when obsolete
            (princ "\nThis function is obsolete")
            (when (nth 2 obsolete)
              (insert (format " since %s" (nth 2 obsolete))))
            (insert ";\n"
                    (if (stringp (car obsolete)) (car obsolete)
                      (format "use `%s' instead." (car obsolete)))
                    "\n"))
          (insert "\n"
                  (or doc "Not documented.")))))))


;; Variables

;;;###autoload
(defun variable-at-point (&optional any-symbol)
  "Return the bound variable symbol found around point.
Return 0 if there is no such symbol.
If ANY-SYMBOL is non-nil, don't insist the symbol be bound."
  (or (condition-case ()
	  (with-syntax-table emacs-lisp-mode-syntax-table
	    (save-excursion
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) (boundp obj) obj))))
	(error nil))
      (let* ((str (find-tag-default))
	     (sym (if str (intern-soft str))))
	(if (and sym (or any-symbol (boundp sym)))
	    sym
	  (save-match-data
	    (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
	      (setq sym (intern-soft (match-string 1 str)))
	      (and (or any-symbol (boundp sym)) sym)))))
      0))

;;;###autoload
(defun describe-variable (variable &optional buffer)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also.
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value."
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if (symbolp v)
				    (format
				     "Describe variable (default %s): " v)
				  "Describe variable: ")
				obarray 'boundp t nil nil
				(if (symbolp v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
  (if (not (symbolp variable))
      (message "You did not specify a variable")
    (save-excursion
      (let* ((valvoid (not (with-current-buffer buffer (boundp variable))))
	     ;; Extract the value before setting up the output buffer,
	     ;; in case `buffer' *is* the output buffer.
	     (val (unless valvoid (buffer-local-value variable buffer))))
	(help-setup-xref (list #'describe-variable variable buffer)
			 (interactive-p))
	(with-output-to-temp-buffer (help-buffer)
	  (with-current-buffer buffer
	    (prin1 variable)
	    (if valvoid
		(princ " is void")
	      (with-current-buffer standard-output
		(princ "'s value is ")
		(terpri)
		(let ((from (point)))
		  (pp val)
		  ;; Hyperlinks in variable's value are quite frequently
		  ;; inappropriate e.g C-h v <RET> features <RET>
		  ;; (help-xref-on-pp from (point))
		  (if (< (point) (+ from 20))
		      (delete-region (1- from) from)))))
	    (terpri)
	    (when (local-variable-p variable)
	      (princ (format "%socal in buffer %s; "
			     (if (get variable 'permanent-local)
				 "Permanently l" "L")
			     (buffer-name)))
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
	    (terpri)
	    (with-current-buffer standard-output
	      (when (> (count-lines (point-min) (point-max)) 10)
		;; Note that setting the syntax table like below
		;; makes forward-sexp move over a `'s' at the end
		;; of a symbol.
		(set-syntax-table emacs-lisp-mode-syntax-table)
		(goto-char (point-min))
		(if valvoid
		    (forward-line 1)
		  (forward-sexp 1)
		  (delete-region (point) (progn (end-of-line) (point)))
		  (save-excursion
		    (insert "\n\nValue:")
		    (set (make-local-variable 'help-button-cache)
			 (point-marker)))
		  (insert " value is shown ")
		  (insert-button "below"
				 'action help-button-cache
				 'follow-link t
				 'help-echo "mouse-2, RET: show value")
		  (insert ".\n\n")))
	      ;; Add a note for variables that have been make-var-buffer-local.
	      (when (and (local-variable-if-set-p variable)
			 (or (not (local-variable-p variable))
			     (with-temp-buffer
			       (local-variable-if-set-p variable))))
		(save-excursion
		  (forward-line -1)
		  (insert "Automatically becomes buffer-local when set in any fashion.\n"))))
 	    ;; Mention if it's an alias
            (let* ((alias (condition-case nil
                             (indirect-variable variable)
                           (error variable)))
                   (obsolete (get variable 'byte-obsolete-variable))
                   (doc (or (documentation-property variable 'variable-documentation)
                            (documentation-property alias 'variable-documentation))))
              (unless (eq alias variable)
                (princ (format "This variable is an alias for `%s'." alias))
                (terpri)
                (terpri))
              (when obsolete
                (princ "This variable is obsolete")
                (if (cdr obsolete) (princ (format " since %s" (cdr obsolete))))
                (princ ";") (terpri)
                (princ (if (stringp (car obsolete)) (car obsolete)
                         (format "use `%s' instead." (car obsolete))))
                (terpri)
                (terpri))
              (princ (or doc "Not documented as a variable.")))
	    ;; Make a link to customize if this variable can be customized.
	    (if (custom-variable-p variable)
		(let ((customize-label "customize"))
		  (terpri)
		  (terpri)
		  (princ (concat "You can " customize-label " this variable."))
		  (with-current-buffer standard-output
		    (save-excursion
		      (re-search-backward
		       (concat "\\(" customize-label "\\)") nil t)
		      (help-xref-button 1 'help-customize-variable variable)))))
	    ;; Make a hyperlink to the library if appropriate.  (Don't
	    ;; change the format of the buffer's initial line in case
	    ;; anything expects the current format.)
	    (let ((file-name (symbol-file variable 'defvar)))
	      (when (equal file-name "loaddefs.el")
		;; Find the real def site of the preloaded variable.
		(let ((location
		       (condition-case nil
			   (find-variable-noselect variable file-name)
			 (error nil))))
		  (when location
		    (with-current-buffer (car location)
		      (goto-char (cdr location))
		      (when (re-search-backward
			     "^;;; Generated autoloads from \\(.*\\)" nil t)
			(setq file-name (match-string 1)))))))
	      (when (and (null file-name)
			 (integerp (get variable 'variable-documentation)))
		;; It's a variable not defined in Elisp but in C.
		(setq file-name
		      (if (get-buffer " *DOC*")
			  (help-C-file-name variable 'var)
			'C-source)))
	      (when file-name
		(princ "\n\nDefined in `")
		(princ (if (eq file-name 'C-source) "C source code" file-name))
		(princ "'.")
		(with-current-buffer standard-output
		  (save-excursion
		    (re-search-backward "`\\([^`']+\\)'" nil t)
		    (help-xref-button 1 'help-variable-def
				      variable file-name)))))

	    (print-help-return-message)
	    (save-excursion
	      (set-buffer standard-output)
	      ;; Return the text we displayed.
	      (buffer-string))))))))


;;;###autoload
(defun describe-syntax (&optional buffer)
  "Describe the syntax specifications in the syntax table of BUFFER.
The descriptions are inserted in a help buffer, which is then displayed.
BUFFER defaults to the current buffer."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (help-setup-xref (list #'describe-syntax buffer) (interactive-p))
  (with-output-to-temp-buffer (help-buffer)
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
  (help-setup-xref (list #'describe-categories buffer) (interactive-p))
  (with-output-to-temp-buffer (help-buffer)
    (let ((table (with-current-buffer buffer (category-table))))
      (with-current-buffer standard-output
	(describe-vector table 'help-describe-category-set)
	(let ((docs (char-table-extra-slot table 0)))
	  (if (or (not (vectorp docs)) (/= (length docs) 95))
	      (insert "Invalid first extra slot in this char table\n")
	    (insert "Meanings of mnemonic characters are:\n")
	    (dotimes (i 95)
	      (let ((elt (aref docs i)))
		(when elt
		  (insert (+ i ?\ ) ": " elt "\n"))))
	    (while (setq table (char-table-parent table))
	      (insert "\nThe parent category table is:")
	      (describe-vector table 'help-describe-category-set))))))))

(provide 'help-fns)

;; arch-tag: 9e10331c-ae81-4d13-965d-c4819aaab0b3
;;; help-fns.el ends here
