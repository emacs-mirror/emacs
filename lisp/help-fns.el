;;; help-fns.el --- Complex help functions

;; Copyright (C) 1985, 1986, 1993, 1994, 1998, 1999, 2000, 2001
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
With arg, you are asked to choose which language."
  (interactive "P")
  (let ((lang (if arg
		  (read-language-name 'tutorial "Language: " "English")
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
      (set-buffer-modified-p nil))))

;;;###autoload
(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normaly returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (read-string "Locate library: ")
		     nil nil
		     t))
  (catch 'answer
    (dolist (dir (or path load-path))
      (dolist (suf (append (unless nosuffix load-suffixes) '("")))
	(let ((try (expand-file-name (concat library suf) dir)))
	  (and (file-readable-p try)
	       (null (file-directory-p try))
	       (progn
		 (if interactive-call
		     (message "Library is file %s" (abbreviate-file-name try)))
		 (throw 'answer try))))))
    (if interactive-call
	(message "No library %s in search path" library))
    nil))


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

;;;###autoload
(defun describe-function-1 (function)
  (let* ((def (if (symbolp function)
		  (symbol-function function)
		function))
	 file-name string
	 (beg (if (commandp def) "an interactive " "a ")))
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
		 (while (symbolp (symbol-function def))
		   (setq def (symbol-function def)))
		 (format "an alias for `%s'" def))
		((eq (car-safe def) 'lambda)
		 (concat beg "Lisp function"))
		((eq (car-safe def) 'macro)
		 "a Lisp macro")
		((eq (car-safe def) 'autoload)
		 (setq file-name (nth 1 def))
		 (format "%s autoloaded %s"
			 (if (commandp def) "an interactive" "an")
			 (if (eq (nth 4 def) 'keymap) "keymap"
			   (if (nth 4 def) "Lisp macro" "Lisp function"))
			 ))
                ;; perhaps use keymapp here instead
                ((eq (car-safe def) 'keymap)
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
	(setq file-name (symbol-file function)))
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
    (cond
     (file-name
      (princ " in `")
      ;; We used to add .el to the file name,
      ;; but that's completely wrong when the user used load-file.
      (princ file-name)
      (princ "'")
      ;; Make a hyperlink to the library.
      (with-current-buffer standard-output
	(save-excursion
	  (re-search-backward "`\\([^`']+\\)'" nil t)
	  (help-xref-button 1 'help-function-def function file-name)))))
    (princ ".")
    (terpri)
    (when (commandp function)
      (let* ((remapped (remap-command function))
	     (keys (where-is-internal
		   (or remapped function) overriding-local-map nil nil)))
	(when remapped
	  (princ "It is remapped to `")
	  (princ (symbol-name remapped))
	  (princ "'"))
	(when keys
	  (princ (if remapped " which is bound to " "It is bound to "))
	  ;; FIXME: This list can be very long (f.ex. for self-insert-command).
	  (princ (mapconcat 'key-description keys ", ")))
	(when (or remapped keys)
	  (princ ".")
	  (terpri))))
    ;; Handle symbols aliased to other symbols.
    (setq def (indirect-function def))
    ;; If definition is a macro, find the function inside it.
    (if (eq (car-safe def) 'macro)
	(setq def (cdr def)))
    (let ((arglist (cond ((byte-code-function-p def)
			  (car (append def nil)))
			 ((eq (car-safe def) 'lambda)
			  (nth 1 def))
			 ((and (eq (car-safe def) 'autoload)
			       (not (eq (nth 4 def) 'keymap)))
			  (concat "[Arg list not available until "
				  "function definition is loaded.]"))
			 (t t))))
      (cond ((listp arglist)
	     (princ (cons (if (symbolp function) function "anonymous")
			  (mapcar (lambda (arg)
				    (if (memq arg '(&optional &rest))
					arg
				      (intern (upcase (symbol-name arg)))))
				  arglist)))
	     (terpri))
	    ((stringp arglist)
	     (princ arglist)
	     (terpri))))
    (let ((doc (documentation function)))
      (if doc
	  (progn (terpri)
		 (princ doc)
		 (if (subrp def)
		     (with-current-buffer standard-output
		       (beginning-of-line)
		       ;; Builtins get the calling sequence at the end of
		       ;; the doc string.  Move it to the same place as
		       ;; for other functions.

		       ;; In cases where `function' has been fset to a
		       ;; subr we can't search for function's name in
		       ;; the doc string.  Kluge round that using the
		       ;; printed representation.  The arg list then
		       ;; shows the wrong function name, but that
		       ;; might be a useful hint.
		       (let* ((rep (prin1-to-string def))
			      (name (progn
				      (string-match " \\([^ ]+\\)>$" rep)
				      (match-string 1 rep))))
			 (if (looking-at (format "(%s[ )]" (regexp-quote name)))
			     (let ((start (point-marker)))
			       (goto-char (point-min))
			       (forward-paragraph)
			       (insert-buffer-substring (current-buffer) start)
			       (insert ?\n)
			       (delete-region (1- start) (point-max)))
			   (goto-char (point-min))
			   (forward-paragraph)
			   (insert
			    "[Missing arglist.  Please make a bug report.]\n")))
		       (goto-char (point-max)))))
	(princ "not documented")))))


;; Variables

;;;###autoload
(defun variable-at-point ()
  "Return the bound variable symbol found around point.
Return 0 if there is no such symbol."
  (condition-case ()
      (with-syntax-table emacs-lisp-mode-syntax-table
	(save-excursion
	  (or (not (zerop (skip-syntax-backward "_w")))
	      (eq (char-syntax (following-char)) ?w)
	      (eq (char-syntax (following-char)) ?_)
	      (forward-sexp -1))
	  (skip-chars-forward "'")
	  (let ((obj (read (current-buffer))))
	    (or (and (symbolp obj) (boundp obj) obj)
		0))))
    (error 0)))

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
  (unless (bufferp buffer) (setq buffer (current-buffer)))
  (if (not (symbolp variable))
      (message "You did not specify a variable")
    (save-excursion
      (let (valvoid)
	(help-setup-xref (list #'describe-variable variable buffer)
			 (interactive-p))
	(with-output-to-temp-buffer (help-buffer)
	  (with-current-buffer buffer
	    (prin1 variable)
	    (if (not (boundp variable))
		(progn
		  (princ " is void")
		  (setq valvoid t))
	      (let ((val (symbol-value variable)))
		(with-current-buffer standard-output
		  (princ "'s value is ")
		  (terpri)
		  (let ((from (point)))
		    (pp val)
		    (help-xref-on-pp from (point))
		    (if (< (point) (+ from 20))
			(save-excursion
			  (goto-char from)
			  (delete-char -1)))))))
	    (terpri)
	    (when (local-variable-p variable)
	      (princ (format "Local in buffer %s; " (buffer-name)))
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
		      (help-xref-on-pp from (point))
		      (if (< (point) (+ from 20))
			  (save-excursion
			    (goto-char from)
			    (delete-char -1)))))))
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
		  (insert " value is shown below.\n\n")
		  (save-excursion
		    (insert "\n\nValue:"))))
	      ;; Add a note for variables that have been make-var-buffer-local.
	      (when (and (local-variable-if-set-p variable)
			 (or (not (local-variable-p variable))
			     (with-temp-buffer
			       (local-variable-if-set-p variable))))
		(save-excursion
		  (forward-line -1)
		  (insert "Automatically becomes buffer-local when set in any fashion.\n"))))
	    (princ "Documentation:")
	    (terpri)
	    (let ((doc (documentation-property variable 'variable-documentation)))
	      (princ (or doc "not documented as a variable.")))
	  
	    ;; Make a link to customize if this variable can be customized.
	    ;; Note, it is not reliable to test only for a custom-type property
	    ;; because those are only present after the var's definition
	    ;; has been loaded.
	    (if (or (get variable 'custom-type) ; after defcustom
		    (get variable 'custom-loads) ; from loaddefs.el
		    (get variable 'standard-value)) ; from cus-start.el
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
	    (let ((file-name (symbol-file variable)))
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
	      (when file-name
		(princ "\n\nDefined in `")
		(princ file-name)
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
The descriptions are inserted in a buffer, which is then displayed."
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

;;; help-fns.el ends here
