;;; help.el --- help commands for Emacs

;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(defvar help-map (make-sparse-keymap)
  "Keymap for characters following the Help key.")

(define-key global-map (char-to-string help-char) 'help-command)
(fset 'help-command help-map)

(define-key help-map (char-to-string help-char) 'help-for-help)
(define-key help-map "?" 'help-for-help)

(define-key help-map "\C-c" 'describe-copying)
(define-key help-map "\C-d" 'describe-distribution)
(define-key help-map "\C-w" 'describe-no-warranty)
(define-key help-map "a" 'command-apropos)

(define-key help-map "b" 'describe-bindings)

(define-key help-map "c" 'describe-key-briefly)
(define-key help-map "k" 'describe-key)

(define-key help-map "d" 'describe-function)
(define-key help-map "f" 'describe-function)

(define-key help-map "i" 'info)
(define-key help-map "\C-f" 'Info-goto-emacs-command-node)
(define-key help-map "\C-k" 'Info-goto-emacs-key-command-node)

(define-key help-map "l" 'view-lossage)

(define-key help-map "m" 'describe-mode)

(define-key help-map "\C-n" 'view-emacs-news)
(define-key help-map "n" 'view-emacs-news)

(define-key help-map "s" 'describe-syntax)

(define-key help-map "t" 'help-with-tutorial)

(define-key help-map "w" 'where-is)

(define-key help-map "v" 'describe-variable)

(defun help-with-tutorial ()
  "Select the Emacs learn-by-doing tutorial."
  (interactive)
  (let ((file (expand-file-name "~/TUTORIAL")))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name "TUTORIAL" data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
      (newline (- (window-height (selected-window))
		  (count-lines (point-min) (point))
		  6))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

(defun describe-key-briefly (key)
  "Print the name of the function KEY invokes.  KEY is a string."
  (interactive "kDescribe key briefly: ")
  (let ((defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (message "%s runs the command %s"
	       (key-description key)
	       (if (symbolp defn) defn (prin1-to-string defn))))))

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the optional argument FUNCTION to it.
If FUNCTION is nil, applies `message' to it, thus printing it."
  (and (not (get-buffer-window standard-output))
       (funcall (or function 'message)
		(concat
		 (substitute-command-keys
		  (if (one-window-p t)
		      (if pop-up-windows
			  "Type \\[delete-other-windows] to remove help window."
			"Type \\[switch-to-buffer] RET to remove help window.")
		    "Type \\[switch-to-buffer-other-window] RET to restore the other window."))
		 (substitute-command-keys
		  "  \\[scroll-other-window] to scroll the help.")))))

(defun describe-key (key)
  "Display documentation of the function invoked by KEY.  KEY is a string."
  (interactive "kDescribe key: ")
  (let ((defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (with-output-to-temp-buffer "*Help*"
	(prin1 defn)
	(princ ":\n")
	(if (documentation defn)
	    (princ (documentation defn))
	  (princ "not documented"))
	(print-help-return-message)))))

(defun describe-mode (&optional minor)
  "Display documentation of current major mode.
If optional MINOR is non-nil (or prefix argument is given if interactive),
display documentation of active minor modes as well.
For this to work correctly for a minor mode, the mode's indicator variable
(listed in `minor-mode-alist') must also be a function whose documentation
describes the minor mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " Mode:\n")
    (princ (documentation major-mode))
    (let ((minor-modes minor-mode-alist)
	  (locals (buffer-local-variables)))
      (while minor-modes
	(let* ((minor-mode (car (car minor-modes)))
	       (indicator (car (cdr (car minor-modes))))
	       (local-binding (assq minor-mode locals)))
	  ;; Document a minor mode if it is listed in minor-mode-alist,
	  ;; bound locally in this buffer, non-nil, and has a function
	  ;; definition.
	  (if (and local-binding
		   (cdr local-binding)
		   (fboundp minor-mode))
	      (progn
		(princ (format "\n\n\n%s minor mode (indicator%s):\n"
			       minor-mode indicator))
		(princ (documentation minor-mode)))))
	(setq minor-modes (cdr minor-modes))))
    (print-help-return-message)))

;; So keyboard macro definitions are documented correctly
(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "DISTRIB" data-directory)))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "COPYING" data-directory))
  (goto-char (point-min)))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty Emacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "NO WARRANTY")
    (recenter 0)))

(defun view-emacs-news ()
  "Display info on recent changes to Emacs."
  (interactive)
  (find-file-read-only (expand-file-name "NEWS" data-directory)))

(defun view-lossage ()
  "Display last 100 input keystrokes."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (key-description (recent-keys)))
    (save-excursion
      (set-buffer standard-output)
      (goto-char (point-min))
      (while (progn (move-to-column 50) (not (eobp)))
	(search-forward " " nil t)
	(insert "\n")))
    (print-help-return-message)))

(defun help-for-help ()
  "You have typed \\[help-for-help], the help character.  Type a Help option:

A  command-apropos.   Give a substring, and see a list of commands
              (functions interactively callable) that contain
	      that substring.  See also the  apropos  command.
B  describe-bindings.  Display table of all key bindings.
C  describe-key-briefly.  Type a command key sequence;
	      it prints the function name that sequence runs.
F  describe-function.  Type a function name and get documentation of it.
I  info. The  info  documentation reader.
K  describe-key.  Type a command key sequence;
	      it displays the full documentation.
L  view-lossage.  Shows last 100 characters you typed.
M  describe-mode.  Print documentation of current major mode,
	      which describes the commands peculiar to it.
N  view-emacs-news.  Shows emacs news file.
S  describe-syntax.  Display contents of syntax table, plus explanations
T  help-with-tutorial.  Select the Emacs learn-by-doing tutorial.
V  describe-variable.  Type name of a variable;
	      it displays the variable's documentation and value.
W  where-is.  Type command name; it prints which keystrokes
	      invoke that command.
C-c print Emacs copying permission (General Public License).
C-d print Emacs ordering information.
C-n print news of recent Emacs changes.
C-w print information on absence of warranty for GNU Emacs."
  (interactive)
  (message (substitute-command-keys
 "A B C F I K L M N S T V W C-c C-d C-n C-w.  Type \\[help-for-help] again for more help: "))
  (let ((char (read-char)))
    (if (or (= char help-char) (= char ??))
	(save-window-excursion
	  (switch-to-buffer "*Help*")
	  (delete-other-windows)
	  (erase-buffer)
	  (insert (documentation 'help-for-help))
	  (goto-char (point-min))
	  (while (memq char (cons help-char '(?? ?\C-v ?\  ?\177 ?\M-v)))
	    (if (memq char '(?\C-v ?\ ))
		(scroll-up))
	    (if (memq char '(?\177 ?\M-v))
		(scroll-down))
	    (message "A B C F I K L M N S T V W C-c C-d C-n C-w%s: "
		     (if (pos-visible-in-window-p (point-max))
			 "" " or Space to scroll"))
	    (let ((cursor-in-echo-area t))
	      (setq char (read-char))))))
    (let ((defn (cdr (assq (downcase char) (cdr help-map)))))
      (if defn (call-interactively defn) (ding)))))


;; Return a function which is called by the list containing point.
;; If that gives no function, return a function whose name is around point.
;; If that doesn't give a function, return nil.
(defun function-called-at-point ()
  (or (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	      (backward-up-list 1)
	      (forward-char 1)
	      (let (obj)
		(setq obj (read (current-buffer)))
		(and (symbolp obj) (fboundp obj) obj))))
	(error nil))
      (condition-case ()
	  (save-excursion
	    (forward-sexp -1)
	    (skip-chars-forward "'")
	    (let ((obj (read (current-buffer))))
	      (and (symbolp obj) (fboundp obj) obj)))
	(error nil))))

(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t))
     (list (if (equal val "")
	       fn (intern val)))))
  (with-output-to-temp-buffer "*Help*"
    (prin1 function)
    (princ ": ")
    (let* ((def (symbol-function function))
	   (beg (if (commandp def) "an interactive " "a ")))
      (princ (cond ((stringp def) "a keyboard macro.")
		   ((subrp def)
		    (concat beg "built-in function."))
		   ((byte-code-function-p def)
		    (concat beg "compiled Lisp function."))
		   ((symbolp def)
		    (format "alias for `%s'." def))
		   ((eq (car-safe def) 'lambda)
		    (concat beg "Lisp function."))
		   ((eq (car-safe def) 'macro)
		    "a Lisp macro.")
		   ((eq (car-safe def) 'mocklisp)
		    "a mocklisp function.")
		   ((eq (car-safe def) 'autoload)
		    (format "%s autoloaded Lisp %s."
			    (if (commandp def) "an interactive" "an")
			    (if (nth 4 def) "macro" "function")
;;; Including the file name made this line too long.
;;;			    (nth 1 def)
			    ))
		   (t "")))
      (terpri))
    (if (documentation function)
        (princ (documentation function))
      (princ "not documented"))
    (print-help-return-message)
    ;; Return the text we displayed.
    (save-excursion (set-buffer standard-output) (buffer-string))))

(defun variable-at-point ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) (boundp obj) obj)))
    (error nil)))

(defun describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also."
  (interactive 
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if v
				    (format "Describe variable (default %s): " v)
				  "Describe variable: ")
				obarray 'boundp t))
     (list (if (equal val "")
	       v (intern val)))))
  (with-output-to-temp-buffer "*Help*"
    (prin1 variable)
    (princ "'s value is ")
    (if (not (boundp variable))
        (princ "void.")
      (prin1 (symbol-value variable)))
    (terpri) (terpri)
    (princ "Documentation:")
    (terpri)
    (let ((doc (documentation-property variable 'variable-documentation)))
      (if doc
	  (princ (substitute-command-keys doc))
	(princ "not documented as a variable.")))
    (print-help-return-message)
    ;; Return the text we displayed.
    (save-excursion (set-buffer standard-output) (buffer-string))))

(defun command-apropos (string)
  "Like apropos but lists only symbols that are names of commands
\(interactively callable functions).  Argument REGEXP is a regular expression
that is matched against command symbol names.  Returns list of symbols and
documentation found."
  (interactive "sCommand apropos (regexp): ")
  (let ((message
	 (let ((standard-output (get-buffer-create "*Help*")))
	   (print-help-return-message 'identity))))
    (apropos string t 'commandp)
    (and message (message message))))

(defun locate-library (library &optional nosuffix)
  "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
  (interactive "sLocate library: ")
  (catch 'answer
    (mapcar
     '(lambda (dir)
	(mapcar
	 '(lambda (suf)
	    (let ((try (expand-file-name (concat library suf) dir)))
	      (and (file-readable-p try)
		   (null (file-directory-p try))
		   (progn
		     (message "Library is file %s" try)
		     (throw 'answer try)))))
	 (if nosuffix '("") '(".elc" ".el" ""))))
     load-path)
    (message "No library %s in search path" library)
    nil))

;;; help.el ends here
