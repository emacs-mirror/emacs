;;; help.el --- help commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1986, 1993-1994, 1998-2022 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: help, internal
;; Package: emacs

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

;; This code implements GNU Emacs's built-in help system, the one invoked by
;; `M-x help-for-help'.

;;; Code:

;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'help-macro))

;; This makes `with-output-to-temp-buffer' buffers use `help-mode'.
(add-hook 'temp-buffer-setup-hook 'help-mode-setup)
(add-hook 'temp-buffer-show-hook 'help-mode-finish)

;; `help-window-point-marker' is a marker you can move to a valid
;; position of the buffer shown in the help window in order to override
;; the standard positioning mechanism (`point-min') chosen by
;; `with-output-to-temp-buffer' and `with-temp-buffer-window'.
;; `with-help-window' has this point nowhere before exiting.  Currently
;; used by `view-lossage' to assert that the last keystrokes are always
;; visible.
(defvar help-window-point-marker (make-marker)
  "Marker to override default `window-point' in help windows.")

(defvar help-window-old-frame nil
  "Frame selected at the time `with-help-window' is invoked.")

(defvar help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (char-to-string help-char) 'help-for-help)
    (define-key map [help] 'help-for-help)
    (define-key map [f1] 'help-for-help)
    (define-key map "." 'display-local-help)
    (define-key map "?" 'help-for-help)

    (define-key map "\C-a" 'about-emacs)
    (define-key map "\C-c" 'describe-copying)
    (define-key map "\C-d" 'view-emacs-debugging)
    (define-key map "\C-e" 'view-external-packages)
    (define-key map "\C-f" 'view-emacs-FAQ)
    (define-key map "\C-m" 'view-order-manuals)
    (define-key map "\C-n" 'view-emacs-news)
    (define-key map "\C-o" 'describe-distribution)
    (define-key map "\C-p" 'view-emacs-problems)
    (define-key map "\C-s" 'search-forward-help-for-help)
    (define-key map "\C-t" 'view-emacs-todo)
    (define-key map "\C-w" 'describe-no-warranty)

    ;; This does not fit the pattern, but it is natural given the C-\ command.
    (define-key map "\C-\\" 'describe-input-method)

    (define-key map "C" 'describe-coding-system)
    (define-key map "F" 'Info-goto-emacs-command-node)
    (define-key map "I" 'describe-input-method)
    (define-key map "K" 'Info-goto-emacs-key-command-node)
    (define-key map "L" 'describe-language-environment)
    (define-key map "S" 'info-lookup-symbol)

    (define-key map "a" 'apropos-command)
    (define-key map "b" 'describe-bindings)
    (define-key map "c" 'describe-key-briefly)
    (define-key map "d" 'apropos-documentation)
    (define-key map "e" 'view-echo-area-messages)
    (define-key map "f" 'describe-function)
    (define-key map "g" 'describe-gnu-project)
    (define-key map "h" 'view-hello-file)

    (define-key map "i" 'info)
    (define-key map "4i" 'info-other-window)

    (define-key map "k" 'describe-key)
    (define-key map "l" 'view-lossage)
    (define-key map "m" 'describe-mode)
    (define-key map "o" 'describe-symbol)
    (define-key map "n" 'view-emacs-news)
    (define-key map "p" 'finder-by-keyword)
    (define-key map "P" 'describe-package)
    (define-key map "r" 'info-emacs-manual)
    (define-key map "R" 'info-display-manual)
    (define-key map "s" 'describe-syntax)
    (define-key map "t" 'help-with-tutorial)
    (define-key map "v" 'describe-variable)
    (define-key map "w" 'where-is)
    (define-key map "x" 'describe-command)
    (define-key map "q" 'help-quit)
    map)
  "Keymap for characters following the Help key.")

(define-key global-map (char-to-string help-char) 'help-command)
(define-key global-map [help] 'help-command)
(define-key global-map [f1] 'help-command)
(fset 'help-command help-map)

;; insert-button makes the action nil if it is not store somewhere
(defvar help-button-cache nil)


(defun help-quit ()
  "Just exit from the Help command's command loop."
  (interactive)
  nil)

(defvar help-return-method nil
  "What to do to \"exit\" the help buffer.
This is a list
 (WINDOW . t)              delete the selected window (and possibly its frame,
                           see `quit-window'), go to WINDOW.
 (WINDOW . quit-window)    do quit-window, then select WINDOW.
 (WINDOW BUF START POINT)  display BUF at START, POINT, then select WINDOW.")

(defun help-print-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
This function assumes that `standard-output' is the help buffer.
It computes a message, and applies the optional argument FUNCTION to it.
If FUNCTION is nil, it applies `message', thus displaying the message.
In addition, this function sets up `help-return-method', which see, that
specifies what to do when the user exits the help buffer.

Do not call this in the scope of `with-help-window'."
  (and (not (get-buffer-window standard-output))
       (let ((first-message
	      (cond ((or
		      pop-up-frames
		      (special-display-p (buffer-name standard-output)))
		     (setq help-return-method (cons (selected-window) t))
		     ;; If the help output buffer is a special display buffer,
		     ;; don't say anything about how to get rid of it.
		     ;; First of all, the user will do that with the window
		     ;; manager, not with Emacs.
		     ;; Secondly, the buffer has not been displayed yet,
		     ;; so we don't know whether its frame will be selected.
		     nil)
		    ((not (one-window-p t))
		     (setq help-return-method
			   (cons (selected-window) 'quit-window))
		     "Type \\[display-buffer] RET to restore the other window.")
		    (pop-up-windows
		     (setq help-return-method (cons (selected-window) t))
		     "Type \\[delete-other-windows] to remove help window.")
		    (t
		     (setq help-return-method
			   (list (selected-window) (window-buffer)
				 (window-start) (window-point)))
		     "Type \\[switch-to-buffer] RET to remove help window."))))
	 (funcall (or function 'message)
		  (concat
		   (if first-message
		       (substitute-command-keys first-message))
		   (if first-message "  ")
		   ;; If the help buffer will go in a separate frame,
		   ;; it's no use mentioning a command to scroll, so don't.
		   (if (or pop-up-windows
			   (special-display-p (buffer-name standard-output)))
		       nil
		     (if (same-window-p (buffer-name standard-output))
			 ;; Say how to scroll this window.
			 (substitute-command-keys
                          "\\[scroll-up-command] to scroll the help.")
		       ;; Say how to scroll some other window.
		       (substitute-command-keys
			"\\[scroll-other-window] to scroll the help."))))))))

;; So keyboard macro definitions are documented correctly
(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))


;;; Help for help.  (a.k.a. `C-h C-h')

(defvar help-for-help-buffer-name " *Metahelp*"
  "Name of the `help-for-help' buffer.")

(defface help-for-help-header '((t :height 1.26))
  "Face used for headers in the `help-for-help' buffer."
  :group 'help)

(defun help--for-help-make-commands (commands)
  "Create commands for `help-for-help' screen from COMMANDS."
  (mapconcat
   (lambda (cmd)
     (if (listp cmd)
         (let ((name (car cmd)) (desc (cadr cmd)))
           (concat
            "   "
            (if (string-match (rx string-start "C-" word string-end) name)
                ;; `help--key-description-fontified' would convert "C-m" to
                ;; "RET" so we can't use it here.
                (propertize name 'face 'help-key-binding)
              (concat "\\[" name "]"))
            " " ; ensure we have some whitespace before the description
            (propertize "\t" 'display '(space :align-to 8))
            desc))
       ""))
   commands "\n"))

(defun help--for-help-make-sections (sections)
  "Create sections for `help-for-help' screen from SECTIONS."
  (mapconcat
   (lambda (section)
     (let ((title (car section)) (commands (cdr section)))
       (concat
        "\n\n"
        (propertize title 'face 'help-for-help-header)
        "\n\n"
        (help--for-help-make-commands commands))))
   sections ""))

(defalias 'help 'help-for-help)
(make-help-screen help-for-help
  (purecopy "Type a help option: [abcCdefFgiIkKlLmnprstvw.] C-[cdefmnoptw] or ?")
  (concat
   "(Type "
   (help--key-description-fontified (kbd "<PageDown>"))
   " or "
   (help--key-description-fontified (kbd "<PageUp>"))
   " to scroll, "
   (help--key-description-fontified "\C-s")
   " to search, or \\<help-map>\\[help-quit] to exit.)"
   (help--for-help-make-sections
    `(("Commands, Keys and Functions"
       ("describe-mode"
        "Show help for current major and minor modes and their commands")
       ("describe-bindings" "Show all key bindings")
       ("describe-key" "Show help for key")
       ("describe-key-briefly" "Show help for key briefly")
       ("where-is" "Show which key runs a specific command")
       ""
       ("apropos-command"
        "Search for commands (see also \\[apropos])")
       ("apropos-documentation"
        "Search documentation of functions, variables, and other items")
       ("describe-command" "Show help for command")
       ("describe-function" "Show help for function")
       ("describe-variable" "Show help for variable")
       ("describe-symbol" "Show help for function or variable"))
      ("Manuals"
       ("info-emacs-manual" "Show Emacs manual")
       ("Info-goto-emacs-command-node"
        "Show Emacs manual section for command")
       ("Info-goto-emacs-key-command-node"
        "Show Emacs manual section for a key sequence")
       ("info" "Show all installed manuals")
       ("info-display-manual" "Show a specific manual")
       ("info-lookup-symbol" "Show description of symbol in pertinent manual"))
      ("Other Help Commands"
       ("view-external-packages"
        "Extending Emacs with external packages")
       ("finder-by-keyword"
        "Search for Emacs packages (see also \\[list-packages])")
       ("describe-package" "Describe a specific Emacs package")
       ""
       ("help-with-tutorial" "Start the Emacs tutorial")
       ("view-echo-area-messages"
        "Show recent messages (from echo area)")
       ("view-lossage" ,(format "Show last %d input keystrokes (lossage)"
                                (lossage-size)))
       ("display-local-help" "Show local help at point"))
      ("Miscellaneous"
       ("about-emacs" "About Emacs")
       ("view-emacs-FAQ" "Emacs FAQ")
       ("C-n" "News of recent changes")
       ("view-emacs-problems" "Known problems")
       ("view-emacs-debugging" "Debugging Emacs")
       ""
       ("describe-gnu-project" "About the GNU project")
       ("describe-copying"
        "Emacs copying permission (GNU General Public License)")
       ("describe-distribution"
        "Emacs ordering and distribution information")
       ("C-m" "Order printed manuals")
       ("view-emacs-todo" "Emacs TODO")
       ("describe-no-warranty"
        "Information on absence of warranty"))
      ("Internationalization and Coding Systems"
       ("describe-input-method" "Describe input method")
       ("describe-coding-system" "Describe coding system")
       ("describe-language-environment"
        "Describe language environment")
       ("describe-syntax" "Show current syntax table")
       ("view-hello-file"
        "Display the HELLO file illustrating various scripts"))))
   "\n")
  help-map
  help-for-help-buffer-name)



(defun function-called-at-point ()
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
            (save-excursion
              (or (not (zerop (skip-syntax-backward "_w")))
                  (eq (char-syntax (following-char)) ?w)
                  (eq (char-syntax (following-char)) ?_)
                  (forward-sexp -1))
              (skip-chars-forward "'")
              (let ((obj (read (current-buffer))))
                (and (symbolp obj) (fboundp obj) obj)))
          (error nil))
        (condition-case ()
            (save-excursion
              (save-restriction
                (let ((forward-sexp-function nil)) ;Use elisp-mode's value
                  (narrow-to-region (max (point-min)
                                         (- (point) 1000))
                                    (point-max))
                  ;; Move up to surrounding paren, then after the open.
                  (backward-up-list 1)
                  (forward-char 1)
                  ;; If there is space here, this is probably something
                  ;; other than a real Lisp function call, so ignore it.
                  (if (looking-at "[ \t]")
                      (error "Probably not a Lisp function call"))
                  (let ((obj (read (current-buffer))))
                    (and (symbolp obj) (fboundp obj) obj)))))
          (error nil))
        (let* ((str (find-tag-default))
               (sym (if str (intern-soft str))))
          (if (and sym (fboundp sym))
              sym
            (save-match-data
              (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                (setq sym (intern-soft (match-string 1 str)))
                (and (fboundp sym) sym))))))))


;;; `User' help functions

(defun view-help-file (file &optional dir)
  (view-file (expand-file-name file (or dir data-directory)))
  (goto-address-mode 1)
  (goto-char (point-min)))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (view-help-file "DISTRIB"))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (view-help-file "COPYING"))

;; Maybe this command should just be removed.
(defun describe-gnu-project ()
  "Browse online information on the GNU project."
  (interactive)
  (browse-url "https://www.gnu.org/gnu/thegnuproject.html"))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty Emacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "Disclaimer of Warranty")
    (forward-line 0)
    (recenter 0)))

(defun describe-prefix-bindings ()
  "Describe the bindings of the prefix used to reach this command.
The prefix described consists of all but the last event
of the key sequence that ran this command."
  (interactive)
  (let ((key (this-command-keys)))
    (describe-bindings
     (if (stringp key)
	 (substring key 0 (1- (length key)))
       (let ((prefix (make-vector (1- (length key)) nil))
	     (i 0))
	 (while (< i (length prefix))
	   (aset prefix i (aref key i))
	   (setq i (1+ i)))
	 prefix)))))
;; Make C-h after a prefix, when not specifically bound,
;; run describe-prefix-bindings.
(setq prefix-help-command 'describe-prefix-bindings)

(defun view-emacs-news (&optional version)
  "Display info on recent changes to Emacs.
With argument, display info only for the selected version."
  (interactive "P")
  (unless version
    (setq version emacs-major-version))
  (when (consp version)
    (let* ((all-versions
	    (let (res)
	      (mapc
	       (lambda (file)
		 (with-temp-buffer
		   (insert-file-contents
		    (expand-file-name file data-directory))
		   (while (re-search-forward
			   (if (member file '("NEWS.18" "NEWS.1-17"))
			       "Changes in \\(?:Emacs\\|version\\)?[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)"
			     "^\\* [^0-9\n]*\\([0-9]+\\.[0-9]+\\)") nil t)
		     (setq res (cons (match-string-no-properties 1) res)))))
	       (cons "NEWS"
		     (directory-files data-directory nil
				      "\\`NEWS\\.[0-9][-0-9]*\\'" nil)))
	      (sort (delete-dups res) #'string>)))
	   (current (car all-versions)))
      (setq version (completing-read
		     (format-prompt "Read NEWS for the version" current)
		     all-versions nil nil nil nil current))
      (if (integerp (string-to-number version))
	  (setq version (string-to-number version))
	(unless (or (member version all-versions)
		    (<= (string-to-number version) (string-to-number current)))
	  (error "No news about version %s" version)))))
  (when (integerp version)
    (cond ((<= version 12)
	   (setq version (format "1.%d" version)))
	  ((<= version 18)
	   (setq version (format "%d" version)))
	  ((> version emacs-major-version)
	   (error "No news about Emacs %d (yet)" version))))
  (let* ((vn (if (stringp version)
		 (string-to-number version)
	       version))
	 (file (cond
		((>= vn emacs-major-version) "NEWS")
		((< vn 18) "NEWS.1-17")
		(t (format "NEWS.%d" vn))))
	 res)
    (view-file (expand-file-name file data-directory))
    (widen)
    (goto-char (point-min))
    (when (stringp version)
      (when (re-search-forward
	     (concat (if (< vn 19)
			 "Changes in Emacs[ \t]*"
		       "^\\* [^0-9\n]*") version "$")
	     nil t)
	(beginning-of-line)
	(narrow-to-region
	 (point)
	 (save-excursion
	   (while (and (setq res
			     (re-search-forward
			      (if (< vn 19)
				  "Changes in \\(?:Emacs\\|version\\)?[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)"
				"^\\* [^0-9\n]*\\([0-9]+\\.[0-9]+\\)") nil t))
		       (equal (match-string-no-properties 1) version)))
	   (or res (goto-char (point-max)))
	   (beginning-of-line)
	   (point)))))))

(defun view-emacs-todo (&optional _arg)
  "Display the Emacs TODO list."
  (interactive "P")
  (view-help-file "TODO"))

(defun view-echo-area-messages ()
  "View the log of recent echo-area messages: the `*Messages*' buffer.
The number of messages retained in that buffer is specified by
the variable `message-log-max'."
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (let ((win (display-buffer (current-buffer))))
      ;; If the buffer is already displayed, we need to forcibly set
      ;; the window point to scroll to the end of the buffer.
      (set-window-point win (point))
      win)))

(defun view-order-manuals ()
  "Display information on how to buy printed copies of Emacs manuals."
  (interactive)
  (info "(emacs)Printed Books"))

(defun view-emacs-FAQ ()
  "Display the Emacs Frequently Asked Questions (FAQ) file."
  (interactive)
  (info "(efaq)"))

(defun view-emacs-problems ()
  "Display info on known problems with Emacs and possible workarounds."
  (interactive)
  (view-help-file "PROBLEMS"))

(defun view-emacs-debugging ()
  "Display info on how to debug Emacs problems."
  (interactive)
  (view-help-file "DEBUG"))

;; This used to visit a plain text file etc/MORE.STUFF;
;; maybe this command should just be removed.
(defun view-external-packages ()
  "Display info on where to get more Emacs packages."
  (interactive)
  (info "(efaq)Packages that do not come with Emacs"))

(defun view-lossage ()
  "Display last few input keystrokes and the commands run.
For convenience this uses the same format as
`edit-last-kbd-macro'.
See `lossage-size' to update the number of recorded keystrokes.

To record all your input, use `open-dribble-file'."
  (interactive)
  (help-setup-xref (list #'view-lossage)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ " ")
    (princ (mapconcat (lambda (key)
			(cond
			 ((and (consp key) (null (car key)))
			  (format ";; %s\n" (if (symbolp (cdr key)) (cdr key)
					      "anonymous-command")))
			 ((or (integerp key) (symbolp key) (listp key))
			  (single-key-description key))
			 (t
			  (prin1-to-string key nil))))
		      (recent-keys 'include-cmds)
		      " "))
    (with-current-buffer standard-output
      (goto-char (point-min))
      (let ((comment-start ";; ")
            (comment-column 24))
        (while (not (eobp))
          (comment-indent)
	  (forward-line 1)))
      ;; Show point near the end of "lossage", as we did in Emacs 24.
      (set-marker help-window-point-marker (point)))))


;; Key bindings

(defun help--key-description-fontified (keys &optional prefix)
  "Like `key-description' but add face for \"*Help*\" buffers."
  ;; We add both the `font-lock-face' and `face' properties here, as this
  ;; seems to be the only way to get this to work reliably in any
  ;; buffer.
  (propertize (key-description keys prefix)
              'font-lock-face 'help-key-binding
              'face 'help-key-binding))

(defcustom describe-bindings-outline nil
  "Non-nil enables outlines in the output buffer of `describe-bindings'."
  :type 'boolean
  :group 'help
  :version "28.1")

(defun describe-bindings (&optional prefix buffer)
  "Display a buffer showing a list of all defined keys, and their definitions.
The keys are displayed in order of precedence.

The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument BUFFER specifies which buffer's bindings
to display (default, the current buffer).  BUFFER can be a buffer
or a buffer name."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (help-setup-xref (list #'describe-bindings prefix buffer)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    ;; Be aware that `describe-buffer-bindings' puts its output into
    ;; the current buffer.
    (with-current-buffer (help-buffer)
      (describe-buffer-bindings buffer prefix)

      (when describe-bindings-outline
        (setq-local outline-regexp ".*:$")
        (setq-local outline-heading-end-regexp ":\n")
        (setq-local outline-level (lambda () 1))
        (setq-local outline-minor-mode-cycle t
                    outline-minor-mode-highlight t)
        (outline-minor-mode 1)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (insert (substitute-command-keys
                     (concat "\\<outline-minor-mode-cycle-map>Type "
                             "\\[outline-cycle] or \\[outline-cycle-buffer] "
                             "on headings to cycle their visibility.\n\n")))
            ;; Hide the longest body
            (when (and (re-search-forward "Key translations" nil t)
                       (fboundp 'outline-cycle))
              (outline-cycle))))))))

(defun where-is (definition &optional insert)
  "Print message listing key sequences that invoke the command DEFINITION.
Argument is a command definition, usually a symbol with a function definition.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (format-prompt "Where is command" fn)
		                obarray 'commandp t nil nil
		                (and fn (symbol-name fn))))
     (list (unless (equal val "") (intern val))
	   current-prefix-arg)))
  (unless definition (error "No command"))
  (let ((func (indirect-function definition))
        (defs nil)
        (standard-output (if insert (current-buffer) standard-output)))
    ;; In DEFS, find all symbols that are aliases for DEFINITION.
    (mapatoms (lambda (symbol)
		(and (fboundp symbol)
		     (not (eq symbol definition))
		     (eq func (condition-case ()
				  (indirect-function symbol)
				(error symbol)))
		     (push symbol defs))))
    ;; Look at all the symbols--first DEFINITION,
    ;; then its aliases.
    (dolist (symbol (cons definition defs))
      (let* ((remapped (command-remapping symbol))
	     (keys (where-is-internal
		    symbol overriding-local-map nil nil remapped))
             (keys (mapconcat #'help--key-description-fontified
                              keys ", "))
	     string)
	(setq string
	      (if insert
		  (if (> (length keys) 0)
		      (if remapped
			  (format "%s (%s) (remapped from %s)"
				  keys remapped symbol)
			(format "%s (%s)" keys symbol))
		    (format "M-x %s RET" symbol))
		(if (> (length keys) 0)
		    (if remapped
			(format "%s is remapped to %s which is on %s"
				symbol remapped keys)
		      (format "%s is on %s" symbol keys))
		  ;; If this is the command the user asked about,
		  ;; and it is not on any key, say so.
		  ;; For other symbols, its aliases, say nothing
		  ;; about them unless they are on keys.
		  (if (eq symbol definition)
		      (format "%s is not on any key" symbol)))))
	(when string
	  (unless (eq symbol definition)
	    (princ ";\n its alias "))
	  (princ string)))))
  nil)

(defun help-key-description (key untranslated)
  (let ((string (help--key-description-fontified key)))
    (if (or (not untranslated)
	    (and (eq (aref untranslated 0) ?\e) (not (eq (aref key 0) ?\e))))
	string
      (let ((otherstring (help--key-description-fontified untranslated)))
	(if (equal string otherstring)
	    string
	  (format "%s (translated from %s)" string otherstring))))))

(defun help--binding-undefined-p (defn)
  (or (null defn) (integerp defn) (equal defn 'undefined)))

(defun help--analyze-key (key untranslated &optional buffer)
  "Get information about KEY its corresponding UNTRANSLATED events.
Returns a list of the form (BRIEF-DESC DEFN EVENT MOUSE-MSG).
When BUFFER is nil, it defaults to the buffer displayed
in the selected window."
  (if (numberp untranslated)
      (error "Missing `untranslated'!"))
  (let* ((event (when (> (length key) 0)
                  (aref key (if (and (symbolp (aref key 0))
		                     (> (length key) 1)
		                     (consp (aref key 1)))
                                ;; Look at the second event when the first
                                ;; is a pseudo-event like `mode-line' or
                                ;; `left-fringe'.
		                1
	                      0))))
	 (modifiers (event-modifiers event))
	 (mouse-msg (if (or (memq 'click modifiers) (memq 'down modifiers)
			    (memq 'drag modifiers))
                        " at that spot" ""))
         ;; Use `posn-set-point' to handle the case when a menu item
         ;; is selected from the context menu that should describe KEY
         ;; at the position of mouse click that opened the context menu.
         ;; When no mouse was involved, don't use `posn-set-point'.
         (defn (if buffer
                   (key-binding key t)
                 (save-excursion (posn-set-point (event-end event))
                                 (key-binding key t)))))
    ;; Handle the case where we faked an entry in "Select and Paste" menu.
    (when (and (eq defn nil)
	       (stringp (aref key (1- (length key))))
	       (eq (key-binding (substring key 0 -1)) 'yank-menu))
      (setq defn 'menu-bar-select-yank))
    ;; Don't bother user with strings from (e.g.) the select-paste menu.
    (when (stringp (aref key (1- (length key))))
      (aset key (1- (length key)) "(any string)"))
    (when (and untranslated
               (stringp (aref untranslated (1- (length untranslated)))))
      (aset untranslated (1- (length untranslated)) "(any string)"))
    (list
     ;; Now describe the key, perhaps as changed.
     (let ((key-desc (help-key-description key untranslated)))
       (if (help--binding-undefined-p defn)
           (format "%s%s is undefined" key-desc mouse-msg)
         (format "%s%s runs the command %S" key-desc mouse-msg defn)))
     defn event mouse-msg)))

(defun help--filter-info-list (info-list i)
  "Drop the undefined keys."
  (or
   ;; Remove all `undefined' keys.
   (delq nil (mapcar (lambda (x)
                       (unless (help--binding-undefined-p (nth i x)) x))
                     info-list))
   ;; If nothing left, then keep one (the last one).
   (last info-list)))

(defun describe-key-briefly (&optional key-list insert buffer)
  "Print the name of the functions KEY-LIST invokes.
KEY-LIST is a list of pairs (SEQ . RAW-SEQ) of key sequences, where
RAW-SEQ is the untranslated form of the key sequence SEQ.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer.

While reading KEY-LIST interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them.

BUFFER is the buffer in which to lookup those keys; it defaults to the
current buffer."
  (interactive
   ;; Ignore mouse movement events because it's too easy to miss the
   ;; message while moving the mouse.
   (let ((key-list (help--read-key-sequence 'no-mouse-movement)))
     `(,key-list ,current-prefix-arg)))
  (when (arrayp key-list)
    ;; Old calling convention, changed
    (setq key-list (list (cons key-list nil))))
  (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
    (let* ((info-list (mapcar (lambda (kr)
                                (help--analyze-key (car kr) (cdr kr) buffer))
                              key-list))
           (msg (mapconcat #'car (help--filter-info-list info-list 1) "\n")))
      (if insert (insert msg) (message "%s" msg)))))

(defun help--key-binding-keymap (key &optional accept-default no-remap position)
  "Return a keymap holding a binding for KEY within current keymaps.
The effect of the arguments KEY, ACCEPT-DEFAULT, NO-REMAP and
POSITION is as documented in the function `key-binding'."
  (let* ((active-maps (current-active-maps t position))
         map found)
    ;; We loop over active maps like key-binding does.
    (while (and
            (not found)
            (setq map (pop active-maps)))
      (setq found (lookup-key map key accept-default))
      (when (integerp found)
        ;; The first `found' characters of KEY were found but not the
        ;; whole sequence.
        (setq found nil)))
    (when found
      (if (and (symbolp found)
               (not no-remap)
               (command-remapping found))
          ;; The user might want to know in which map the binding is
          ;; found, or in which map the remapping is found.  The
          ;; default is to show the latter.
          (help--key-binding-keymap (vector 'remap found))
        map))))

(defun help--binding-locus (key position)
  "Describe in which keymap KEY is defined.
Return a symbol pointing to that keymap if one exists ; otherwise
return nil.  The argument POSITION is as documented in the
function `key-binding'."
  (let ((map (help--key-binding-keymap key t nil position)))
    (when map
      (catch 'found
        (let ((advertised-syms (nconc
                                (list 'overriding-terminal-local-map
                                      'overriding-local-map)
                                (delq nil
                                      (mapcar
                                       (lambda (mode-and-map)
                                         (let ((mode (car mode-and-map)))
                                           (when (symbol-value mode)
                                             (intern-soft
                                              (format "%s-map" mode)))))
                                       minor-mode-map-alist))
                                (list 'global-map
                                      (intern-soft (format "%s-map" major-mode))))))
          ;; Look into these advertised symbols first.
          (dolist (sym advertised-syms)
            (when (and
                   (boundp sym)
                   (eq map (symbol-value sym)))
              (throw 'found sym)))
          ;; Only look in other symbols otherwise.
          (mapatoms
           (lambda (x)
             (when (and (boundp x)
                        ;; Avoid let-bound symbols.
                        (special-variable-p x)
                        (eq (symbol-value x) map))
               (throw 'found x))))
          nil)))))

(defun help--read-key-sequence (&optional no-mouse-movement)
  "Read a key sequence from the user.
Usually reads a single key sequence, except when that sequence might
hide another one (e.g. a down event, where the user is interested
in getting info about the up event, or a click event, where the user
wants to get info about the double click).
Return a list of elements of the form (SEQ . RAW-SEQ), where SEQ is a key
sequence, and RAW-SEQ is its untranslated form.
If NO-MOUSE-MOVEMENT is non-nil, ignore key sequences starting
with `mouse-movement' events."
  (let ((enable-disabled-menus-and-buttons t)
        (cursor-in-echo-area t)
        (side-event nil)
        saved-yank-menu)
    (unwind-protect
        (let (last-modifiers key-list)
          ;; If yank-menu is empty, populate it temporarily, so that
          ;; "Select and Paste" menu can generate a complete event.
          (when (null (cdr yank-menu))
            (setq saved-yank-menu (copy-sequence yank-menu))
            (menu-bar-update-yank-menu "(any string)" nil))
          (while
              ;; Read at least one key-sequence.
              (or (null key-list)
                  ;; After a down event, also read the (presumably) following
                  ;; up-event.
                  (memq 'down last-modifiers)
                  ;; After a click, see if a double click is on the way.
                  (and (memq 'click last-modifiers)
                       (not (sit-for (/ double-click-time 1000.0) t))))
            (let* ((seq (read-key-sequence "\
Describe the following key, mouse click, or menu item: "
                                           nil nil 'can-return-switch-frame))
                   (raw-seq (this-single-command-raw-keys))
                   (keyn (when (> (length seq) 0)
                           (aref seq (1- (length seq)))))
                   (base (event-basic-type keyn))
                   (modifiers (event-modifiers keyn)))
              (cond
               ((zerop (length seq)))   ;FIXME: Can this happen?
               ((and no-mouse-movement (eq base 'mouse-movement)) nil)
               ((memq base '(mouse-movement switch-frame select-window))
                ;; Mostly ignore these events since it's sometimes difficult to
                ;; generate the event you care about without also generating
                ;; these side-events along the way.
                (setq side-event (cons seq raw-seq)))
               ((eq base 'help-echo) nil)
               (t
                (setq last-modifiers modifiers)
                (push (cons seq raw-seq) key-list)))))
          (if side-event
              (cons side-event (nreverse key-list))
            (nreverse key-list)))
      ;; Put yank-menu back as it was, if we changed it.
      (when saved-yank-menu
        (setq yank-menu (copy-sequence saved-yank-menu))
        (fset 'yank-menu (cons 'keymap yank-menu))))))

(defun describe-key (&optional key-list buffer up-event)
  "Display documentation of the function invoked by KEY-LIST.
KEY-LIST can be any kind of a key sequence; it can include keyboard events,
mouse events, and/or menu events.  When calling from a program,
pass KEY-LIST as a list of elements (SEQ . RAW-SEQ) where SEQ is
a key-sequence and RAW-SEQ is its untranslated form.

While reading KEY-LIST interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them.

BUFFER is the buffer in which to lookup those keys; it defaults to the
current buffer."
  (declare (advertised-calling-convention (key-list &optional buffer) "27.1"))
  (interactive (list (help--read-key-sequence)))
  (when (arrayp key-list)
    ;; Compatibility with old calling convention.
    (setq key-list (cons (list key-list) (if up-event (list up-event))))
    (when buffer
      (let ((raw (if (numberp buffer) (this-single-command-raw-keys) buffer)))
        (setf (cdar (last key-list)) raw)))
    (setq buffer nil))
  (let* ((buf (or buffer (current-buffer)))
         (on-link
          (mapcar (lambda (kr)
                    (let ((raw (cdr kr)))
                      (and (not (memq mouse-1-click-follows-link '(nil double)))
                           (> (length raw) 0)
                           (eq (car-safe (aref raw 0)) 'mouse-1)
                           (with-current-buffer buf
                             (mouse-on-link-p (event-start (aref raw 0)))))))
                  key-list))
         (info-list
          (help--filter-info-list
           (with-current-buffer buf
             (mapcar (lambda (x)
                       (pcase-let* ((`(,seq . ,raw-seq) x)
                                    (`(,brief-desc ,defn ,event ,_mouse-msg)
                                     (help--analyze-key seq raw-seq buffer))
                                    (locus
                                     (help--binding-locus
                                      seq (event-start event))))
                         `(,seq ,brief-desc ,defn ,locus)))
                     key-list))
           2)))
    (help-setup-xref (list (lambda (key-list buf)
                             (describe-key key-list
                                           (if (buffer-live-p buf) buf)))
                           key-list buf)
		     (called-interactively-p 'interactive))
    (if (and (<= (length info-list) 1)
             (help--binding-undefined-p (nth 2 (car info-list))))
        (message "%s" (nth 1 (car info-list)))
      (with-help-window (help-buffer)
        (when (> (length info-list) 1)
          ;; FIXME: Make this into clickable hyperlinks.
          (princ "There were several key-sequences:\n\n")
          (princ (mapconcat (lambda (info)
                              (pcase-let ((`(,_seq ,brief-desc ,_defn ,_locus)
                                           info))
                                (concat "  " brief-desc)))
                            info-list
                            "\n"))
          (when (delq nil on-link)
            (princ "\n\nThose are influenced by `mouse-1-click-follows-link'"))
          (princ "\n\nThey're all described below."))
        (pcase-dolist (`(,_seq ,brief-desc ,defn ,locus)
                       info-list)
          (when defn
            (when (> (length info-list) 1)
              (with-current-buffer standard-output
                (insert "\n\n" (make-separator-line) "\n")))

            (princ brief-desc)
            (when locus
              (princ (format " (found in %s)" locus)))
            (princ ", which is ")
	    (describe-function-1 defn)))))))

(defun search-forward-help-for-help ()
  "Search forward in the help-for-help window.
This command is meant to be used after issuing the `C-h C-h' command."
  (interactive)
  (unless (get-buffer help-for-help-buffer-name)
    (error "No %s buffer; use `C-h C-h' first" help-for-help-buffer-name))
  ;; Move cursor to the "help window".
  (pop-to-buffer help-for-help-buffer-name)
  ;; Do incremental search forward.
  (isearch-forward nil t))

(defun describe-minor-mode (minor-mode)
  "Display documentation of a minor mode given as MINOR-MODE.
MINOR-MODE can be a minor mode symbol or a minor mode indicator string
appeared on the mode-line."
  (interactive (list (completing-read
		      "Minor mode: "
			      (nconc
			       (describe-minor-mode-completion-table-for-symbol)
			       (describe-minor-mode-completion-table-for-indicator)
			       ))))
  (if (symbolp minor-mode)
      (setq minor-mode (symbol-name minor-mode)))
  (let ((symbols (describe-minor-mode-completion-table-for-symbol))
	(indicators (describe-minor-mode-completion-table-for-indicator)))
    (cond
     ((member minor-mode symbols)
      (describe-minor-mode-from-symbol (intern minor-mode)))
     ((member minor-mode indicators)
      (describe-minor-mode-from-indicator minor-mode))
     (t
      (error "No such minor mode: %s" minor-mode)))))

;; symbol
(defun describe-minor-mode-completion-table-for-symbol ()
  ;; In order to list up all minor modes, minor-mode-list
  ;; is used here instead of minor-mode-alist.
  (delq nil (mapcar 'symbol-name minor-mode-list)))

(defun describe-minor-mode-from-symbol (symbol)
  "Display documentation of a minor mode given as a symbol, SYMBOL."
  (interactive (list (intern (completing-read
			      "Minor mode symbol: "
			      (describe-minor-mode-completion-table-for-symbol)))))
  (if (fboundp symbol)
      (describe-function symbol)
    (describe-variable symbol)))

;; indicator
(defun describe-minor-mode-completion-table-for-indicator ()
  (delq nil
	(mapcar (lambda (x)
		  (let ((i (format-mode-line x)))
		    ;; remove first space if existed
		    (cond
		     ((= 0 (length i))
		      nil)
		     ((eq (aref i 0) ?\s)
		      (substring i 1))
		     (t
		      i))))
		minor-mode-alist)))

(defun describe-minor-mode-from-indicator (indicator)
  "Display documentation of a minor mode specified by INDICATOR.
If you call this function interactively, you can give indicator which
is currently activated with completion."
  (interactive (list
		(completing-read
		 "Minor mode indicator: "
		 (describe-minor-mode-completion-table-for-indicator))))
  (let ((minor-mode (lookup-minor-mode-from-indicator indicator)))
    (if minor-mode
	(describe-minor-mode-from-symbol minor-mode)
      (error "Cannot find minor mode for `%s'" indicator))))

(defun lookup-minor-mode-from-indicator (indicator)
  "Return a minor mode symbol from its indicator on the mode line."
  ;; remove first space if existed
  (if (and (< 0 (length indicator))
	   (eq (aref indicator 0) ?\s))
      (setq indicator (substring indicator 1)))
  (let ((minor-modes minor-mode-alist)
	result)
    (while minor-modes
      (let* ((minor-mode (car (car minor-modes)))
	     (anindicator (format-mode-line
			   (car (cdr (car minor-modes))))))
	;; remove first space if existed
	(if (and (stringp anindicator)
		 (> (length anindicator) 0)
		 (eq (aref anindicator 0) ?\s))
	    (setq anindicator (substring anindicator 1)))
	(if (equal indicator anindicator)
	    (setq result minor-mode
		  minor-modes nil)
	  (setq minor-modes (cdr minor-modes)))))
    result))


(defun substitute-command-keys (string &optional no-face)
  "Substitute key descriptions for command names in STRING.
Each substring of the form \\\\=[COMMAND] is replaced by either a
keystroke sequence that invokes COMMAND, or \"M-x COMMAND\" if COMMAND
is not on any keys.  Keybindings will use the face `help-key-binding',
unless the optional argument NO-FACE is non-nil.

Each substring of the form \\\\={MAPVAR} is replaced by a summary of
the value of MAPVAR as a keymap.  This summary is similar to the one
produced by ‘describe-bindings’.  The summary ends in two newlines
(used by the helper function ‘help-make-xrefs’ to find the end of the
summary).

Each substring of the form \\\\=<MAPVAR> specifies the use of MAPVAR
as the keymap for future \\\\=[COMMAND] substrings.

Each grave accent \\=` is replaced by left quote, and each apostrophe \\='
is replaced by right quote.  Left and right quote characters are
specified by ‘text-quoting-style’.

\\\\== quotes the following character and is discarded; thus, \\\\==\\\\== puts \\\\==
into the output, \\\\==\\[ puts \\[ into the output, and \\\\==\\=` puts \\=` into the
output.

Return the original STRING if no substitutions are made.
Otherwise, return a new string."
  (when (not (null string))
    ;; KEYMAP is either nil (which means search all the active
    ;; keymaps) or a specified local map (which means search just that
    ;; and the global map).  If non-nil, it might come from
    ;; overriding-local-map, or from a \\<mapname> construct in STRING
    ;; itself.
    (let ((keymap overriding-local-map)
          (inhibit-modification-hooks t)
          (orig-buf (current-buffer)))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((orig-point (point))
                end-point active-maps
                close generate-summary)
            (cond
             ;; 1. Handle all sequences starting with "\"
             ((= (following-char) ?\\)
              (ignore-errors
                (forward-char 1))
              (cond
               ;; 1A. Ignore \= at end of string.
               ((and (= (+ (point) 1) (point-max))
                     (= (following-char) ?=))
                (forward-char 1))
               ;; 1B. \= quotes the next character; thus, to put in \[
               ;;     without its special meaning, use \=\[.
               ((= (following-char) ?=)
                (goto-char orig-point)
                (delete-char 2)
                (ignore-errors
                  (forward-char 1)))
               ;; 1C. \[foo] is replaced with the keybinding.
               ((and (= (following-char) ?\[)
                     (save-excursion
                       (prog1 (search-forward "]" nil t)
                         (setq end-point (- (point) 2)))))
                (goto-char orig-point)
                (delete-char 2)
                (let* ((fun (intern (buffer-substring (point) (1- end-point))))
                       (key (with-current-buffer orig-buf
                              (where-is-internal fun keymap t))))
                  ;; If this a command remap, we need to follow it.
                  (when (and (vectorp key)
                             (> (length key) 1)
                             (eq (aref key 0) 'remap)
                             (symbolp (aref key 1)))
                    (setq fun (aref key 1))
                    (setq key (with-current-buffer orig-buf
                                (where-is-internal fun keymap t))))
                  (if (not key)
                      ;; Function is not on any key.
                      (let ((op (point)))
                        (insert "M-x ")
                        (goto-char (+ end-point 3))
                        (or no-face
                            (add-text-properties
                             op (point)
                             '( face help-key-binding
                                font-lock-face help-key-binding)))
                        (delete-char 1))
                    ;; Function is on a key.
                    (delete-char (- end-point (point)))
                    (insert (if no-face
                                (key-description key)
                              (help--key-description-fontified key))))))
               ;; 1D. \{foo} is replaced with a summary of the keymap
               ;;            (symbol-value foo).
               ;;     \<foo> just sets the keymap used for \[cmd].
               ((and (or (and (= (following-char) ?{)
                              (setq close "}")
                              (setq generate-summary t))
                         (and (= (following-char) ?<)
                              (setq close ">")))
                     (or (save-excursion
                           (prog1 (search-forward close nil t)
                             (setq end-point (- (point) 2))))))
                (goto-char orig-point)
                (delete-char 2)
                (let* ((name (intern (buffer-substring (point) (1- end-point))))
                       this-keymap)
                  (delete-char (- end-point (point)))
                  ;; Get the value of the keymap in TEM, or nil if
                  ;; undefined. Do this in the user's current buffer
                  ;; in case it is a local variable.
                  (with-current-buffer orig-buf
                    ;; This is for computing the SHADOWS arg for
                    ;; describe-map-tree.
                    (setq active-maps (current-active-maps))
                    (when (boundp name)
                      (setq this-keymap (and (keymapp (symbol-value name))
                                             (symbol-value name)))))
                  (cond
                   ((null this-keymap)
                    (insert "\nUses keymap "
                            (substitute-command-keys "`")
                            (symbol-name name)
                            (substitute-command-keys "'")
                            ", which is not currently defined.\n")
                    (unless generate-summary
                      (setq keymap nil)))
                   ((not generate-summary)
                    (setq keymap this-keymap))
                   (t
                    ;; Get the list of active keymaps that precede this one.
                    ;; If this one's not active, get nil.
                    (let ((earlier-maps (cdr (memq this-keymap (reverse active-maps)))))
                      (describe-map-tree this-keymap t (nreverse earlier-maps)
                                         nil nil t nil nil t))))))))
             ;; 2. Handle quotes.
             ((and (eq (text-quoting-style) 'curve)
                   (or (and (= (following-char) ?\`)
                            (prog1 t (insert "‘")))
                       (and (= (following-char) ?')
                            (prog1 t (insert "’")))))
              (delete-char 1))
             ((and (eq (text-quoting-style) 'straight)
                   (= (following-char) ?\`))
              (insert "'")
              (delete-char 1))
             ;; 3. Nothing to do -- next character.
             (t (forward-char 1)))))
        (buffer-string)))))

(defvar help--keymaps-seen nil)
(defun describe-map-tree (startmap partial shadow prefix title no-menu
                                   transl always-title mention-shadow)
  "Insert a description of the key bindings in STARTMAP.
This is followed by the key bindings of all maps reachable
through STARTMAP.

If PARTIAL is non-nil, omit certain uninteresting commands
\(such as `undefined').

If SHADOW is non-nil, it is a list of maps; don't mention keys
which would be shadowed by any of them.

If PREFIX is non-nil, mention only keys that start with PREFIX.

If TITLE is non-nil, is a string to insert at the beginning.
TITLE should not end with a colon or a newline; we supply that.

If NOMENU is non-nil, then omit menu-bar commands.

If TRANSL is non-nil, the definitions are actually key
translations so print strings and vectors differently.

If ALWAYS-TITLE is non-nil, print the title even if there are no
maps to look through.

If MENTION-SHADOW is non-nil, then when something is shadowed by
SHADOW, don't omit it; instead, mention it but say it is
shadowed.

Any inserted text ends in two newlines (used by
`help-make-xrefs')."
  (let* ((amaps (accessible-keymaps startmap prefix))
         (orig-maps (if no-menu
                        (progn
                          ;; Delete from MAPS each element that is for
                          ;; the menu bar.
                          (let* ((tail amaps)
                                 result)
                            (while tail
                              (let ((elem (car tail)))
                                (when (not (and (>= (length (car elem)) 1)
                                                (eq (elt (car elem) 0) 'menu-bar)))
                                  (setq result (append result (list elem)))))
                              (setq tail (cdr tail)))
                            result))
                      amaps))
         (maps orig-maps)
         (print-title (or maps always-title)))
    ;; Print title.
    (when print-title
      (insert (concat (if title
                          (concat title
                                  (if prefix
                                      (concat " Starting With "
                                              (help--key-description-fontified prefix)))
                                  ":\n"))
                      "key             binding\n"
                      "---             -------\n")))
    ;; Describe key bindings.
    (setq help--keymaps-seen nil)
    (while (consp maps)
      (let* ((elt (car maps))
             (elt-prefix (car elt))
             (sub-shadows (lookup-key shadow elt-prefix t)))
        (when (if (natnump sub-shadows)
                  (prog1 t (setq sub-shadows nil))
                ;; Describe this map iff elt_prefix is bound to a
                ;; keymap, since otherwise it completely shadows this
                ;; map.
                (or (keymapp sub-shadows)
                    (null sub-shadows)
                    (and (consp sub-shadows)
                         (keymapp (car sub-shadows)))))
          ;; Maps we have already listed in this loop shadow this map.
          (let ((tail orig-maps))
            (while (not (equal tail maps))
              (when (equal (car (car tail)) elt-prefix)
                (setq sub-shadows (cons (cdr (car tail)) sub-shadows)))
              (setq tail (cdr tail))))
          (describe-map (cdr elt) elt-prefix transl partial
                        sub-shadows no-menu mention-shadow)))
      (setq maps (cdr maps)))
    (when print-title
      (insert "\n"))))

(defun help--shadow-lookup (keymap key accept-default remap)
  "Like `lookup-key', but with command remapping.
Return nil if the key sequence is too long."
  ;; Converted from shadow_lookup in keymap.c.
  (let ((value (lookup-key keymap key accept-default)))
    (cond ((and (fixnump value) (<= 0 value)))
          ((and value remap (symbolp value))
           (or (command-remapping value nil keymap)
               value))
          (t value))))

(defvar help--previous-description-column 0)
(defun help--describe-command (definition)
  ;; Converted from describe_command in keymap.c.
  ;; If column 16 is no good, go to col 32;
  ;; but don't push beyond that--go to next line instead.
  (let* ((column (current-column))
         (description-column (cond ((> column 30)
                                    (insert "\n")
                                    32)
                                   ((or (> column 14)
                                        (and (> column 10)
                                             (= help--previous-description-column 32)))
                                    32)
                                   (t 16))))
    ;; Avoid using the `help-keymap' face.
    (let ((op (point)))
      (indent-to description-column 1)
      (set-text-properties op (point) '( face nil
                                         font-lock-face nil)))
    (setq help--previous-description-column description-column)
    (cond ((symbolp definition)
           (insert (symbol-name definition) "\n"))
          ((or (stringp definition) (vectorp definition))
           (insert "Keyboard Macro\n"))
          ((keymapp definition)
           (insert "Prefix Command\n"))
          (t (insert "??\n")))))

(defun help--describe-translation (definition)
  ;; Converted from describe_translation in keymap.c.
  ;; Avoid using the `help-keymap' face.
  (let ((op (point)))
    (indent-to 16 1)
    (set-text-properties op (point) '( face nil
                                      font-lock-face nil)))
  (cond ((symbolp definition)
         (insert (symbol-name definition) "\n"))
        ((or (stringp definition) (vectorp definition))
         (insert (key-description definition nil) "\n"))
        ((keymapp definition)
         (insert "Prefix Command\n"))
        (t (insert "??\n"))))

(defun help--describe-map-compare (a b)
  (let ((a (car a))
        (b (car b)))
    (cond ((and (fixnump a) (fixnump b)) (< a b))
          ;; ((and (not (fixnump a)) (fixnump b)) nil) ; not needed
          ((and (fixnump a) (not (fixnump b))) t)
          ((and (symbolp a) (symbolp b))
           ;; Sort the keystroke names in the "natural" way, with (for
           ;; instance) "<f2>" coming between "<f1>" and "<f11>".
           (string-version-lessp (symbol-name a) (symbol-name b)))
          (t nil))))

(defun describe-map (map prefix transl partial shadow nomenu mention-shadow)
  "Describe the contents of keymap MAP.
Assume that this keymap itself is reached by the sequence of
prefix keys PREFIX (a string or vector).

TRANSL, PARTIAL, SHADOW, NOMENU, MENTION-SHADOW are as in
`describe-map-tree'."
  ;; Converted from describe_map in keymap.c.
  (let* ((suppress (and partial 'suppress-keymap))
         (map (keymap-canonicalize map))
         (tail map)
         (first t)
         (describer (if transl
                        #'help--describe-translation
                      #'help--describe-command))
         done vect)
    (while (and (consp tail) (not done))
      (cond ((or (vectorp (car tail)) (char-table-p (car tail)))
             (help--describe-vector (car tail) prefix describer partial
                                shadow map mention-shadow))
            ((consp (car tail))
             (let ((event (caar tail))
                   definition this-shadowed)
               ;; Ignore bindings whose "prefix" are not really
               ;; valid events. (We get these in the frames and
               ;; buffers menu.)
               (and (or (symbolp event) (fixnump event))
                    (not (and nomenu (eq event 'menu-bar)))
                    ;; Don't show undefined commands or suppressed
                    ;; commands.
                    (setq definition (keymap--get-keyelt (cdr (car tail)) nil))
                    (or (not (symbolp definition))
                        (null (get definition suppress)))
                    ;; Don't show a command that isn't really
                    ;; visible because a local definition of the
                    ;; same key shadows it.
                    (or (not shadow)
                        (let ((tem (help--shadow-lookup shadow (vector event) t nil)))
                          (cond ((null tem) t)
                                ;; If both bindings are keymaps,
                                ;; this key is a prefix key, so
                                ;; don't say it is shadowed.
                                ((and (keymapp definition) (keymapp tem)) t)
                                ;; Avoid generating duplicate
                                ;; entries if the shadowed binding
                                ;; has the same definition.
                                ((and mention-shadow (not (eq tem definition)))
                                 (setq this-shadowed t))
                                (t nil))))
                    (eq definition (lookup-key tail (vector event) t))
                    (push (list event definition this-shadowed) vect))))
            ((eq (car tail) 'keymap)
             ;; The same keymap might be in the structure twice, if
             ;; we're using an inherited keymap.  So skip anything
             ;; we've already encountered.
             (let ((tem (assq tail help--keymaps-seen)))
               (if (and (consp tem)
                        (equal (car tem) prefix))
                   (setq done t)
                 (push (cons tail prefix) help--keymaps-seen)))))
      (setq tail (cdr tail)))
    ;; If we found some sparse map events, sort them.
    (let ((vect (sort vect 'help--describe-map-compare)))
      ;; Now output them in sorted order.
      (while vect
        (let* ((elem (car vect))
               (start (car elem))
               (definition (cadr elem))
               (shadowed (caddr elem))
               (end start))
          (when first
            (setq help--previous-description-column 0)
            (insert "\n")
            (setq first nil))
          ;; Find consecutive chars that are identically defined.
          (when (fixnump start)
            (while (and (cdr vect)
                        (let ((this-event (caar vect))
                              (this-definition (cadar vect))
                              (this-shadowed (caddar vect))
                              (next-event (caar (cdr vect)))
                              (next-definition (cadar (cdr vect)))
                              (next-shadowed (caddar (cdr vect))))
                          (and (eq next-event (1+ this-event))
                               (equal next-definition this-definition)
                               (eq this-shadowed next-shadowed))))
              (setq vect (cdr vect))
              (setq end (caar vect))))
          ;; Now START .. END is the range to describe next.
          ;; Insert the string to describe the event START.
          (insert (help--key-description-fontified (vector start) prefix))
          (when (not (eq start end))
            (insert " .. " (help--key-description-fontified (vector end) prefix)))
          ;; Print a description of the definition of this character.
          ;; Called function will take care of spacing out far enough
          ;; for alignment purposes.
          (if transl
              (help--describe-translation definition)
            (help--describe-command definition))
          ;; Print a description of the definition of this character.
          ;; elt_describer will take care of spacing out far enough for
          ;; alignment purposes.
          (when shadowed
            (goto-char (max (1- (point)) (point-min)))
            (insert "\n  (this binding is currently shadowed)")
            (goto-char (min (1+ (point)) (point-max)))))
        ;; Next item in list.
        (setq vect (cdr vect))))))

;;;; This Lisp version is 100 times slower than its C equivalent:
;;
;; (defun help--describe-vector
;;     (vector prefix transl partial shadow entire-map mention-shadow)
;;   "Insert in the current buffer a description of the contents of VECTOR.
;;
;; PREFIX a prefix key which leads to the keymap that this vector is
;; in.
;;
;; If PARTIAL, it means do not mention suppressed commands
;; (that assumes the vector is in a keymap).
;;
;; SHADOW is a list of keymaps that shadow this map.  If it is
;; non-nil, look up the key in those maps and don't mention it if it
;; is defined by any of them.
;;
;; ENTIRE-MAP is the vector in which this vector appears.
;; If the definition in effect in the whole map does not match
;; the one in this vector, we ignore this one."
;;   ;; Converted from describe_vector in keymap.c.
;;   (let* ((first t)
;;          (idx 0))
;;     (while (< idx (length vector))
;;       (let* ((val (aref vector idx))
;;              (definition (keymap--get-keyelt val nil))
;;              (start-idx idx)
;;              this-shadowed
;;              found-range)
;;         (when (and definition
;;                    ;; Don't mention suppressed commands.
;;                    (not (and partial
;;                              (symbolp definition)
;;                              (get definition 'suppress-keymap)))
;;                    ;; If this binding is shadowed by some other map,
;;                    ;; ignore it.
;;                    (not (and shadow
;;                              (help--shadow-lookup shadow (vector start-idx) t nil)
;;                              (if mention-shadow
;;                                  (prog1 nil (setq this-shadowed t))
;;                                t)))
;;                    ;; Ignore this definition if it is shadowed by an earlier
;;                    ;; one in the same keymap.
;;                    (not (and entire-map
;;                              (not (eq (lookup-key entire-map (vector start-idx) t)
;;                                       definition)))))
;;           (when first
;;             (insert "\n")
;;             (setq first nil))
;;           (when (and prefix (> (length prefix) 0))
;;             (insert (format "%s" prefix)))
;;           (insert (help--key-description-fontified (vector start-idx) prefix))
;;           ;; Find all consecutive characters or rows that have the
;;           ;; same definition.
;;           (while (equal (keymap--get-keyelt (aref vector (1+ idx)) nil)
;;                         definition)
;;             (setq found-range t)
;;             (setq idx (1+ idx)))
;;           ;; If we have a range of more than one character,
;;           ;; print where the range reaches to.
;;           (when found-range
;;             (insert " .. ")
;;             (when (and prefix (> (length prefix) 0))
;;               (insert (format "%s" prefix)))
;;             (insert (help--key-description-fontified (vector idx) prefix)))
;;           (if transl
;;               (help--describe-translation definition)
;;             (help--describe-command definition))
;;           (when this-shadowed
;;             (goto-char (1- (point)))
;;             (insert "  (binding currently shadowed)")
;;             (goto-char (1+ (point))))))
;;       (setq idx (1+ idx)))))


(declare-function x-display-pixel-height "xfns.c" (&optional terminal))
(declare-function x-display-pixel-width "xfns.c" (&optional terminal))

;;; Automatic resizing of temporary buffers.
(defcustom temp-buffer-max-height
  (lambda (_buffer)
    (if (and (display-graphic-p) (eq (selected-window) (frame-root-window)))
	(/ (x-display-pixel-height) (frame-char-height) 2)
      (/ (- (frame-height) 2) 2)))
  "Maximum height of a window displaying a temporary buffer.
This is effective only when Temp Buffer Resize mode is enabled.
The value is the maximum height (in lines) which
`resize-temp-buffer-window' will give to a window displaying a
temporary buffer.  It can also be a function to be called to
choose the height for such a buffer.  It gets one argument, the
buffer, and should return a positive integer.  At the time the
function is called, the window to be resized is selected."
  :type '(choice integer function)
  :group 'help
  :version "24.3")

(defcustom temp-buffer-max-width
  (lambda (_buffer)
    (if (and (display-graphic-p) (eq (selected-window) (frame-root-window)))
	(/ (x-display-pixel-width) (frame-char-width) 2)
      (/ (- (frame-width) 2) 2)))
  "Maximum width of a window displaying a temporary buffer.
This is effective only when Temp Buffer Resize mode is enabled.
The value is the maximum width (in columns) which
`resize-temp-buffer-window' will give to a window displaying a
temporary buffer.  It can also be a function to be called to
choose the width for such a buffer.  It gets one argument, the
buffer, and should return a positive integer.  At the time the
function is called, the window to be resized is selected."
  :type '(choice integer function)
  :group 'help
  :version "24.4")

(define-minor-mode temp-buffer-resize-mode
  "Toggle auto-resizing temporary buffer windows (Temp Buffer Resize Mode).

When Temp Buffer Resize mode is enabled, the windows in which we
show a temporary buffer are automatically resized in height to
fit the buffer's contents, but never more than
`temp-buffer-max-height' nor less than `window-min-height'.

A window is resized only if it has been specially created for the
buffer.  Windows that have shown another buffer before are not
resized.  A frame is resized only if `fit-frame-to-buffer' is
non-nil.

This mode is used by `help', `apropos' and `completion' buffers,
and some others."
  :global t :group 'help
  (if temp-buffer-resize-mode
      ;; `help-make-xrefs' may add a `back' button and thus increase the
      ;; text size, so `resize-temp-buffer-window' must be run *after* it.
      (add-hook 'temp-buffer-show-hook 'resize-temp-buffer-window 'append)
    (remove-hook 'temp-buffer-show-hook 'resize-temp-buffer-window)))

(defun resize-temp-buffer-window (&optional window)
  "Resize WINDOW to fit its contents.
WINDOW must be a live window and defaults to the selected one.
Do not resize if WINDOW was not created by `display-buffer'.

If WINDOW is part of a vertical combination, restrain its new
size by `temp-buffer-max-height' and do not resize if its minimum
accessible position is scrolled out of view.  If WINDOW is part
of a horizontal combination, restrain its new size by
`temp-buffer-max-width'.  In both cases, the value of the option
`fit-window-to-buffer-horizontally' can inhibit resizing.

If WINDOW is the root window of its frame, resize the frame
provided `fit-frame-to-buffer' is non-nil.

This function may call `preserve-window-size' to preserve the
size of WINDOW."
  (setq window (window-normalize-window window t))
  (let ((height (if (functionp temp-buffer-max-height)
		    (with-selected-window window
		      (funcall temp-buffer-max-height (window-buffer)))
		  temp-buffer-max-height))
	(width (if (functionp temp-buffer-max-width)
		   (with-selected-window window
		     (funcall temp-buffer-max-width (window-buffer)))
		 temp-buffer-max-width))
	(quit-cadr (cadr (window-parameter window 'quit-restore))))
    ;; Resize WINDOW iff it was made by `display-buffer'.
    (when (or (and (eq quit-cadr 'window)
		   (or (and (window-combined-p window)
			    (not (eq fit-window-to-buffer-horizontally
				     'only))
			    (pos-visible-in-window-p (point-min) window))
		       (and (window-combined-p window t)
			    fit-window-to-buffer-horizontally)))
	      (and (eq quit-cadr 'frame)
                   fit-frame-to-buffer
                   (eq window (frame-root-window window))))
	(fit-window-to-buffer window height nil width nil t))))

;;; Help windows.
(defcustom help-window-select nil
  "Non-nil means select help window for viewing.
Choices are:

 never (nil) Select help window only if there is no other window
             on its frame.

 other       Select help window if and only if it appears on the
             previously selected frame, that frame contains at
             least two other windows and the help window is
             either new or showed a different buffer before.

 always (t)  Always select the help window.

If this option is non-nil and the help window appears on another
frame, then give that frame input focus too.  Note also that if
the help window appears on another frame, it may get selected and
its frame get input focus even if this option is nil.

This option has effect if and only if the help window was created
by `with-help-window'."
  :type '(choice (const :tag "never (nil)" nil)
		 (const :tag "other" other)
		 (const :tag "always (t)" t))
  :group 'help
  :version "23.1")

(define-obsolete-variable-alias 'help-enable-auto-load
  'help-enable-autoload "27.1")

(defcustom help-enable-autoload t
  "Whether Help commands can perform autoloading.
If non-nil, whenever \\[describe-function] is called for an
autoloaded function whose docstring contains any key substitution
construct (see `substitute-command-keys'), the library is loaded,
so that the documentation can show the right key bindings."
  :type 'boolean
  :group 'help
  :version "24.3")

(defun help-window-display-message (quit-part window &optional scroll)
  "Display message telling how to quit and scroll help window.
QUIT-PART is a string telling how to quit the help window WINDOW.
Optional argument SCROLL non-nil means tell how to scroll WINDOW.
SCROLL equal `other' means tell how to scroll the \"other\"
window."
  (let ((scroll-part
	 (cond
	  ;; If we don't have QUIT-PART we probably reuse a window
	  ;; showing the same buffer so we don't show any message.
	  ((not quit-part) nil)
	  ((pos-visible-in-window-p
	    (with-current-buffer (window-buffer window)
	      (point-max)) window t)
	   ;; Buffer end is at least partially visible, no need to talk
	   ;; about scrolling.
	   ".")
	  ((eq scroll 'other)
	   ", \\[scroll-other-window] to scroll help.")
          (scroll ", \\[scroll-up-command] to scroll help."))))
    (message "%s"
     (substitute-command-keys (concat quit-part scroll-part)))))

(defun help-window-setup (window &optional value)
  "Set up help window WINDOW for `with-help-window'.
WINDOW is the window used for displaying the help buffer.
Return VALUE."
  (let* ((help-buffer (when (window-live-p window)
			(window-buffer window)))
	 (help-setup (when (window-live-p window)
		       (car (window-parameter window 'quit-restore))))
	 (frame (window-frame window)))

    (when help-buffer
      ;; Handle `help-window-point-marker'.
      (when (eq (marker-buffer help-window-point-marker) help-buffer)
	(set-window-point window help-window-point-marker)
	;; Reset `help-window-point-marker'.
	(set-marker help-window-point-marker nil))

      ;; If the help window appears on another frame, select it if
      ;; `help-window-select' is non-nil and give that frame input focus
      ;; too.  See also Bug#19012.
      (when (and help-window-select
		 (frame-live-p help-window-old-frame)
		 (not (eq frame help-window-old-frame)))
	(select-window window)
	(select-frame-set-input-focus frame))

      (cond
       ((or (eq window (selected-window))
	    ;; If the help window is on the selected frame, select
	    ;; it if `help-window-select' is t or `help-window-select'
	    ;; is 'other, the frame contains at least three windows, and
	    ;; the help window did show another buffer before.  See also
	    ;; Bug#11039.
	    (and (eq frame (selected-frame))
		 (or (eq help-window-select t)
		     (and (eq help-window-select 'other)
			  (> (length (window-list nil 'no-mini)) 2)
			  (not (eq help-setup 'same))))
		 (select-window window)))
	;; The help window is or gets selected ...
	(help-window-display-message
	 (cond
	  ((eq help-setup 'window)
	   ;; ... and is new, ...
	   "Type \"q\" to delete help window")
	  ((eq help-setup 'frame)
	   ;; ... on a new frame, ...
	   "Type \"q\" to quit the help frame")
	  ((eq help-setup 'other)
	   ;; ... or displayed some other buffer before.
	   "Type \"q\" to restore previous buffer"))
	 window t))
       ((and (eq (window-frame window) help-window-old-frame)
	     (= (length (window-list nil 'no-mini)) 2))
	;; There are two windows on the help window's frame and the
	;; other one is the selected one.
	(help-window-display-message
	 (cond
	  ((eq help-setup 'window)
	   "Type \\[delete-other-windows] to delete the help window")
	  ((eq help-setup 'other)
	   "Type \"q\" in help window to restore its previous buffer"))
	 window 'other))
       (t
	;; The help window is not selected ...
	(help-window-display-message
	 (cond
	  ((eq help-setup 'window)
	   ;; ... and is new, ...
	   "Type \"q\" in help window to delete it")
	  ((eq help-setup 'other)
	   ;; ... or displayed some other buffer before.
	   "Type \"q\" in help window to restore previous buffer"))
	 window))))
    ;; Return VALUE.
    value))

;; `with-help-window' is a wrapper for `with-temp-buffer-window'
;; providing the following additional twists:

;; (1) It puts the buffer in `help-mode' (via `help-mode-setup') and
;;     adds cross references (via `help-mode-finish').

;; (2) It issues a message telling how to scroll and quit the help
;;     window (via `help-window-setup').

;; (3) An option (customizable via `help-window-select') to select the
;;     help window automatically.

;; (4) A marker (`help-window-point-marker') to move point in the help
;;     window to an arbitrary buffer position.
(defmacro with-help-window (buffer-or-name &rest body)
  "Evaluate BODY, send output to BUFFER-OR-NAME and show in a help window.
This construct is like `with-temp-buffer-window', which see, but unlike
that, it puts the buffer specified by BUFFER-OR-NAME in `help-mode' and
displays a message about how to delete the help window when it's no
longer needed.  The help window will be selected if
`help-window-select' is non-nil.
Most of this is done by `help-window-setup', which see."
  (declare (indent 1) (debug t))
  `(progn
     ;; Make `help-window-point-marker' point nowhere.  The only place
     ;; where this should be set to a buffer position is within BODY.
     (set-marker help-window-point-marker nil)
     (let ((temp-buffer-window-setup-hook
	    (cons 'help-mode-setup temp-buffer-window-setup-hook))
	   (temp-buffer-window-show-hook
	    (cons 'help-mode-finish temp-buffer-window-show-hook)))
       (setq help-window-old-frame (selected-frame))
       (with-temp-buffer-window
	,buffer-or-name nil 'help-window-setup (progn ,@body)))))

;; Called from C, on encountering `help-char' when reading a char.
;; Don't print to *Help*; that would clobber Help history.
(defun help-form-show ()
  "Display the output of a non-nil `help-form'."
  (let ((msg (eval help-form)))
    (if (stringp msg)
	(with-output-to-temp-buffer " *Char Help*"
	  (princ msg)))))


(defun help--docstring-quote (string)
  "Return a doc string that represents STRING.
The result, when formatted by `substitute-command-keys', should equal STRING."
  (replace-regexp-in-string "['\\`‘’]" "\\\\=\\&" string))

;; The following functions used to be in help-fns.el, which is not preloaded.
;; But for various reasons, they are more widely needed, so they were
;; moved to this file, which is preloaded.  https://debbugs.gnu.org/17001

(defun help-split-fundoc (docstring def &optional section)
  "Split a function DOCSTRING into the actual doc and the usage info.
Return (USAGE . DOC), where USAGE is a string describing the argument
list of DEF, such as \"(apply FUNCTION &rest ARGUMENTS)\".
DEF is the function whose usage we're looking for in DOCSTRING.
With SECTION nil, return nil if there is no usage info; conversely,
SECTION t means to return (USAGE . DOC) even if there's no usage info.
When SECTION is \\='usage or \\='doc, return only that part."
  ;; Functions can get the calling sequence at the end of the doc string.
  ;; In cases where `function' has been fset to a subr we can't search for
  ;; function's name in the doc string so we use `fn' as the anonymous
  ;; function name instead.
  (let* ((found (and docstring
                     (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring)))
         (doc (if found
                  (and (memq section '(t nil doc))
                       (not (zerop (match-beginning 0)))
                       (substring docstring 0 (match-beginning 0)))
                docstring))
         (usage (and found
                     (memq section '(t nil usage))
                     (let ((tail (match-string 1 docstring)))
                       (format "(%s%s"
                               ;; Replace `fn' with the actual function name.
                               (if (and (symbolp def) def)
                                   (help--docstring-quote (format "%S" def))
                                 'anonymous)
                               tail)))))
    (pcase section
      (`nil (and usage (cons usage doc)))
      (`t (cons usage doc))
      (`usage usage)
      (`doc doc))))

(defun help-add-fundoc-usage (docstring arglist)
  "Add the usage info to DOCSTRING.
If DOCSTRING already has a usage info, then just return it unchanged.
The usage info is built from ARGLIST.  DOCSTRING can be nil.
ARGLIST can also be t or a string of the form \"(FUN ARG1 ARG2 ...)\"."
  (unless (stringp docstring) (setq docstring ""))
  (if (or (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring)
          (eq arglist t))
      docstring
    (concat docstring
	    (if (string-match "\n?\n\\'" docstring)
		(if (< (- (match-end 0) (match-beginning 0)) 2) "\n" "")
	      "\n\n")
	    (if (stringp arglist)
                (if (string-match "\\`[^ ]+\\(.*\\))\\'" arglist)
                    (concat "(fn" (match-string 1 arglist) ")")
                  (error "Unrecognized usage format"))
	      (help--make-usage-docstring 'fn arglist)))))

(declare-function subr-native-lambda-list "data.c")

(defun help-function-arglist (def &optional preserve-names)
  "Return a formal argument list for the function DEF.
If PRESERVE-NAMES is non-nil, return a formal arglist that uses
the same names as used in the original source code, when possible."
  ;; Handle symbols aliased to other symbols.
  (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
  ;; Advice wrappers have "catch all" args, so fetch the actual underlying
  ;; function to find the real arguments.
  (while (advice--p def) (setq def (advice--cdr def)))
  ;; If definition is a macro, find the function inside it.
  (if (eq (car-safe def) 'macro) (setq def (cdr def)))
  (cond
   ((and (byte-code-function-p def) (listp (aref def 0))) (aref def 0))
   ((eq (car-safe def) 'lambda) (nth 1 def))
   ((eq (car-safe def) 'closure) (nth 2 def))
   ((and (featurep 'native-compile)
         (subrp def)
         (listp (subr-native-lambda-list def)))
    (subr-native-lambda-list def))
   ((or (and (byte-code-function-p def) (integerp (aref def 0)))
        (subrp def) (module-function-p def))
    (or (when preserve-names
          (let* ((doc (condition-case nil (documentation def) (error nil)))
                 (docargs (if doc (car (help-split-fundoc doc nil))))
                 (arglist (if docargs
                              (cdar (read-from-string (downcase docargs)))))
                 (valid t))
            ;; Check validity.
            (dolist (arg arglist)
              (unless (and (symbolp arg)
                           (let ((name (symbol-name arg)))
                             (if (eq (aref name 0) ?&)
                                 (memq arg '(&rest &optional))
                               (not (string-search "." name)))))
                (setq valid nil)))
            (when valid arglist)))
        (let* ((arity (func-arity def))
               (max (cdr arity))
               (min (car arity))
               (arglist ()))
          (dotimes (i min)
            (push (intern (concat "arg" (number-to-string (1+ i)))) arglist))
          (when (and (integerp max) (> max min))
            (push '&optional arglist)
            (dotimes (i (- max min))
              (push (intern (concat "arg" (number-to-string (+ 1 i min))))
                    arglist)))
          (unless (integerp max) (push '&rest arglist) (push 'rest arglist))
          (nreverse arglist))))
   ((and (autoloadp def) (not (eq (nth 4 def) 'keymap)))
    "[Arg list not available until function definition is loaded.]")
   (t t)))

(defun help--make-usage (function arglist)
  (cons (if (symbolp function) function 'anonymous)
	(mapcar (lambda (arg)
		  (cond
                   ;; Parameter name.
                   ((symbolp arg)
		    (let ((name (symbol-name arg)))
		      (cond
                       ((string-match "\\`&" name) arg)
                       ((string-match "\\`_." name)
                        (intern (upcase (substring name 1))))
                       (t (intern (upcase name))))))
                   ;; Parameter with a default value (from
                   ;; cl-defgeneric etc).
                   ((and (consp arg)
                         (symbolp (car arg)))
                    (cons (intern (upcase (symbol-name (car arg)))) (cdr arg)))
                   ;; Something else.
                   (t arg)))
		arglist)))

(define-obsolete-function-alias 'help-make-usage 'help--make-usage "25.1")

(defun help--make-usage-docstring (fn arglist)
  (let ((print-escape-newlines t))
    (help--docstring-quote (format "%S" (help--make-usage fn arglist)))))



;; Just some quote-like characters for now.  TODO: generate this stuff
;; from official Unicode data.
(defconst help-uni-confusables
  '((#x2018 . "'") ;; LEFT SINGLE QUOTATION MARK
    (#x2019 . "'") ;; RIGHT SINGLE QUOTATION MARK
    (#x201B . "'") ;; SINGLE HIGH-REVERSED-9 QUOTATION MARK
    (#x201C . "\"") ;; LEFT DOUBLE QUOTATION MARK
    (#x201D . "\"") ;; RIGHT DOUBLE QUOTATION MARK
    (#x201F . "\"") ;; DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    (#x301E . "\"") ;; DOUBLE PRIME QUOTATION MARK
    (#xFF02 . "'") ;; FULLWIDTH QUOTATION MARK
    (#xFF07 . "'") ;; FULLWIDTH APOSTROPHE
    )
  "An alist of confusable characters to give hints about.
Each alist element is of the form (CHAR . REPLACEMENT), where
CHAR is the potentially confusable character, and REPLACEMENT is
the suggested string to use instead.  See
`help-uni-confusable-suggestions'.")

(defconst help-uni-confusables-regexp
  (concat "[" (mapcar #'car help-uni-confusables) "]")
  "Regexp matching any character listed in `help-uni-confusables'.")

(defun help-uni-confusable-suggestions (string)
  "Return a message describing confusables in STRING."
  (let ((i 0)
        (confusables nil))
    (while (setq i (string-match help-uni-confusables-regexp string i))
      (let ((replacement (alist-get (aref string i) help-uni-confusables)))
        (push (aref string i) confusables)
        (setq string (replace-match replacement t t string))
        (setq i (+ i (length replacement)))))
    (when confusables
      (format-message
       (ngettext
        "Found confusable character: %s, perhaps you meant: `%s'?"
        "Found confusable characters: %s; perhaps you meant: `%s'?"
        (length confusables))
       (mapconcat (lambda (c) (format-message "`%c'" c))
                  confusables ", ")
       string))))

(defun help-command-error-confusable-suggestions (data _context _signal)
  (pcase data
    (`(void-variable ,var)
     (let ((suggestions (help-uni-confusable-suggestions
                         (symbol-name var))))
       (when suggestions
         (princ (concat "\n  " suggestions) t))))
    (_ nil)))

(add-function :after command-error-function
              #'help-command-error-confusable-suggestions)

(define-obsolete-function-alias 'help-for-help-internal #'help-for-help "28.1")


(provide 'help)

;;; help.el ends here
