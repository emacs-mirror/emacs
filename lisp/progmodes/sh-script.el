;;; sh-script.el --- shell-script editing commands for Emacs
;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer, fax (+49 69) 75 88 529, c/o <bonhoure@cict.fr>
;; Maintainer: FSF
;; Keywords: shell programming

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

;;; Commentary:

;; Major mode for editing shell scripts.  Currently sh, ksh, bash and csh,
;; tcsh are supported.  Structured statements can be inserted with one
;; command.

;; Autoloading of these functions is currently turned off
;; because it's not clear whether this mode is really desirable to use.
;; -- rms

;;; Code:

;; page 1:	variables and settings
;; page 2:	mode-command and utility functions
;; page 3:	statement syntax-commands for various shells
;; page 4:	various other commands


;;;###dont-autoload
(setq auto-mode-alist
      ;; matches files
      ;;	- whose path contains /bin/, but not directories
      (cons '("/bin/" . sh-or-other-mode)
	    ;;	- that have a suffix .sh or .shar (shell archive)
	    ;;	- that contain resources for the various shells
	    ;;	- startup files for X11
	    (cons '("\\.sh\\'\\|\\.shar\\'\\|/\\.\\(profile\\|bash_profile\\|login\\|bash_login\\|logout\\|bash_logout\\|bashrc\\|t?cshrc\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
		  auto-mode-alist)))


(defvar sh-mode-syntax-table
  (let ((table (copy-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\^l ">#" table)
    (modify-syntax-entry ?\n ">#" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?\' "\"'" table)
    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?^ "_" table)
    (modify-syntax-entry ?~ "_" table)
    table)
  "Syntax table in use in Shell-Script mode.")



(defvar sh-use-prefix nil
  "If non-nil when loading, `$' and `<' will be  C-c $  and  C-c < .")

(defvar sh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c(" 'sh-function)
    (define-key map "\C-c\C-w" 'sh-while)
    (define-key map "\C-c\C-u" 'sh-until)
    (define-key map "\C-c\C-s" 'sh-select)
    (define-key map "\C-c\C-l" 'sh-indexed-loop)
    (define-key map "\C-c\C-i" 'sh-if)
    (define-key map "\C-c\C-f" 'sh-for)
    (define-key map "\C-c\C-c" 'sh-case)
    
    (define-key map (if sh-use-prefix "\C-c$" "$")
      'sh-query-for-variable)
    (define-key map "=" 'sh-assignment)
    (define-key map "\C-c+" 'sh-add)
    (define-key map (if sh-use-prefix "\C-c<" "<")
      'sh-maybe-here-document)
    (define-key map "(" 'pair-insert-maybe)
    (define-key map "{" 'pair-insert-maybe)
    (define-key map "[" 'pair-insert-maybe)
    (define-key map "'" 'pair-insert-maybe)
    (define-key map "`" 'pair-insert-maybe)
    (define-key map "\"" 'pair-insert-maybe)

    (define-key map "\t" 'sh-indent-line)
    (substitute-key-definition 'complete-tag 'comint-dynamic-complete-filename
			       map (current-global-map))
    (substitute-key-definition 'newline-and-indent 'sh-newline-and-indent
			       map (current-global-map))
    ;; Now that tabs work properly, this might be unwanted.
    (substitute-key-definition 'delete-backward-char
			       'backward-delete-char-untabify
			       map (current-global-map))
    (define-key map "\C-c:" 'sh-set-shell)
    (substitute-key-definition 'beginning-of-defun
			       'sh-beginning-of-compound-command
			       map (current-global-map))
    (substitute-key-definition 'backward-sentence 'sh-beginning-of-command
			       map (current-global-map))
    (substitute-key-definition 'forward-sentence 'sh-end-of-command
			       map (current-global-map))
    (substitute-key-definition 'manual-entry 'sh-manual-entry
			       map (current-global-map))
    (define-key map [menu-bar insert] 
      (cons "Insert" (make-sparse-keymap "Insert")))
    (define-key map [menu-bar insert sh-while] 
      '("While Loop" . sh-while))
    (define-key map [menu-bar insert sh-until] 
      '("Until Loop" . sh-until))
    (define-key map [menu-bar insert sh-select] 
      '("Select Statement" . sh-select))
    (define-key map [menu-bar insert sh-indexed-loop] 
      '("Indexed Loop" . sh-indexed-loop))
    (define-key map [menu-bar insert sh-if] 
      '("If Statement" . sh-if))
    (define-key map [menu-bar insert sh-for] 
      '("For Loop" . sh-for))
    (define-key map [menu-bar insert sh-case] 
      '("Case Statement" . sh-case))
    map)
  "Keymap used in Shell-Script mode.")



(defvar sh-find-file-modifies t
  "*What to do when newly found file has no magic number:
	nil	do nothing
	t	insert magic number
	other	insert magic number, but mark as unmodified.")


(defvar sh-query-for-magic t
  "*If non-nil, ask user before changing or inserting magic number.")


(defvar sh-magicless-file-regexp "/\\.[^/]+$"
  "*On files with this kind of name no magic is inserted or changed.")


;; someone who understands /etc/magic better than me should beef this up
;; this currently covers only SCO Unix and Sinix executables
;; the elegant way would be to read /etc/magic
(defvar magic-number-alist '(("L\^a\^h\\|\^?ELF" . hexl-mode)
			     ("#!.*perl" . perl-mode))
  "A regexp to match the magic number of a found file.
Currently this is only used by function `sh-or-other-mode'.")


(defvar sh-executable ".* is \\([^ \t]*\\)\n"
  "*Regexp to match the output of sh builtin `type' command on your machine.
The regexp must match the whole output, and must contain a \\(something\\)
construct which matches the actual executable.")



(defvar sh-chmod-argument "+x"
  "*After saving, if the file is not executable, set this mode.
The mode can be absolute, such as \"777\", or relative, such as \"+x\".
Do nothing if this is nil.")


(defvar sh-shell-path (or (getenv "SHELL") "/bin/sh")
  "*The executable of the shell being programmed.")

(defvar sh-shell-argument nil
  "*A single argument for the magic number, or nil.")

(defvar sh-shell nil
  "The shell being programmed.  This is set by \\[sh-set-shell].")

(defvar sh-shell-is-csh nil
  "The shell being programmed.  This is set by \\[sh-set-shell].")

(defvar sh-tab-width 4
  "The default value for `tab-width' in Shell-Script mode.
This is the width of tab stops after the indentation of the preceeding line.")

(defvar sh-remember-variable-min 3
  "*Don't remember variables less than this length for completing reads.")


(defvar sh-beginning-of-command
  "\\([;({`|&]\\|^\\)[ \t]*\\([/~:a-zA-Z0-9]\\)"
  "*Regexp to determine the beginning of a shell command.
The actual command starts at the beginning of the second \\(grouping\\).")

(defvar sh-end-of-command
  "\\([/~:a-zA-Z0-9]\\)[ \t]*\\([;#)}`|&]\\|$\\)"
  "*Regexp to determine the end of a shell command.
The actual command ends at the end of the first \\(grouping\\).")



(defvar sh-assignment-space '(csh tcsh)
  "List of shells that allow spaces around the assignment =.")

(defvar sh-here-document-word "+"
  "Word to delimit here documents.")


;process-environment
(defvar sh-variables
  '(("addsuffix" tcsh)			("allow_null_glob_expansion" bash)
    ("ampm" tcsh)			("argv" csh tcsh) 
    ("autocorrect" tcsh)		("autoexpand" tcsh)	
    ("autolist" tcsh)			("autologout" tcsh)
    ("auto_resume" bash)		("BASH" bash)
    ("BASH_VERSION" bash)		("cdable_vars" bash)
    ("cdpath" csh tcsh)			("CDPATH" sh ksh bash) 
    ("chase_symlinks" tcsh)		("child" csh tcsh) 
    ("COLUMNS" ksh tcsh)		("correct" tcsh) 
    ("dextract" tcsh)			("echo" csh tcsh) 
    ("edit" tcsh)			("EDITOR") 
    ("el" tcsh)				("ENV" ksh bash)	
    ("ERRNO" ksh)			("EUID" bash)
    ("FCEDIT" ksh bash)			("FIGNORE" bash)
    ("fignore" tcsh)			("FPATH" ksh) 
    ("gid" tcsh)			("glob_dot_filenames" bash)
    ("histchars" bash csh tcsh)		("HISTFILE" ksh bash)
    ("HISTFILESIZE" bash)		("histlit" tcsh) 
    ("history" csh tcsh)		("history_control" bash)
    ("HISTSIZE" bash)			("home" csh tcsh) 
    ("HOME")				("HOST" tcsh) 
    ("hostname_completion_file" bash)	("HOSTTYPE" bash tcsh)
    ("HPATH" tcsh)			("HUSHLOGIN")
    ("IFS" sh ksh bash)			("ignoreeof" bash csh tcsh)
    ("IGNOREEOF" bash)			("ignore_symlinks" tcsh) 
    ("LANG")				("LC_COLLATE") 
    ("LC_CTYPE")			("LC_MESSAGES") 
    ("LC_MONETARY")			("LC_NUMERIC") 
    ("LC_TIME")				("LINENO" ksh bash)
    ("LINES" ksh tcsh)			("listjobs" tcsh) 
    ("listlinks" tcsh)			("listmax" tcsh) 
    ("LOGNAME")				("mail" csh tcsh) 
    ("MAIL")				("MAILCHECK") 
    ("MAILPATH")			("MAIL_WARNING" bash)
    ("matchbeep" tcsh)			("nobeep" tcsh)
    ("noclobber" bash csh tcsh)		("noglob" csh tcsh)
    ("nolinks" bash)			("nonomatch" csh tcsh) 
    ("NOREBIND" tcsh)			("notify" bash)
    ("no_exit_on_failed_exec" bash)	("NO_PROMPT_VARS" bash)
    ("oid" tcsh)			("OLDPWD" ksh bash)
    ("OPTARG" sh ksh bash)		("OPTERR" bash)
    ("OPTIND" sh ksh bash)		("PAGER") 
    ("path" csh tcsh)			("PATH") 
    ("PPID" ksh bash)			("printexitvalue" tcsh) 
    ("prompt" csh tcsh)			("prompt2" tcsh) 
    ("prompt3" tcsh)			("PROMPT_COMMAND" bash)
    ("PS1" sh ksh bash)			("PS2" sh ksh bash)
    ("PS3" ksh)				("PS4" ksh bash)
    ("pushdsilent" tcsh)		("pushdtohome" tcsh)
    ("pushd_silent" bash)		("PWD" ksh bash)
    ("RANDOM" ksh bash)			("recexact" tcsh) 
    ("recognize_only_executables" tcsh)	("REPLY" ksh bash)
    ("rmstar" tcsh)			("savehist" tcsh) 
    ("SECONDS" ksh bash)		("shell" csh tcsh) 
    ("SHELL")				("SHLVL" bash tcsh) 
    ("showdots" tcsh)			("sl" tcsh)	
    ("status" csh tcsh)			("SYSTYPE" tcsh) 
    ("tcsh" tcsh)			("term" tcsh) 
    ("TERM")				("TERMCAP")	
    ("time" csh tcsh)			("TMOUT" ksh bash) 
    ("tperiod" tcsh)			("tty" tcsh) 
    ("UID" bash)			("uid" tcsh)
    ("verbose" csh tcsh)		("version" tcsh)
    ("visiblebell" tcsh)		("VISUAL")
    ("watch" tcsh)			("who" tcsh)
    ("wordchars" tcsh))
  "Alist of all environment and shell variables used for completing read.
Variables only understood by some shells are associated to a list of those.")



(defvar sh-font-lock-keywords nil
  ;; This is done syntactically:
  ;'(("[ \t]\\(#.*\\)" 1 font-lock-comment-face)
  ;  ("\"[^`]*\"\\|'.*'\\|\\\\[^\nntc]" . font-lock-string-face))
  "*Rules for highlighting shell scripts.
This variable is included into the various variables
`sh-SHELL-font-lock-keywords'.  If no such variable exists for some shell,
this one is used.")


(defvar sh-sh-font-lock-keywords
  (append sh-font-lock-keywords
	  '(("\\(^\\|[^-._a-z0-9]\\)\\(case\\|do\\|done\\|elif\\|else\\|esac\\|fi\\|for\\|if\\|in\\|then\\|until\\|while\\)\\($\\|[^-._a-z0-9]\\)" 2 font-lock-keyword-face t)))
  "*Rules for highlighting Bourne shell scripts.")

(defvar sh-ksh-font-lock-keywords
  (append sh-sh-font-lock-keywords
	  '(("\\(^\\|[^-._a-z0-9]\\)\\(function\\|select\\)\\($\\|[^-._a-z0-9]\\)" 2 font-lock-keyword-face t)))
  "*Rules for highlighting Korn shell scripts.")

(defvar sh-bash-font-lock-keywords
  (append sh-sh-font-lock-keywords
	  '(("\\(^\\|[^-._a-z0-9]\\)\\(function\\)\\($\\|[^-._a-z0-9]\\)" 2 font-lock-keyword-face t)))
  "*Rules for highlighting Bourne again shell scripts.")


(defvar sh-csh-font-lock-keywords
  (append sh-font-lock-keywords
	  '(("\\(^\\|[^-._a-z0-9]\\)\\(breaksw\\|case\\|default\\|else\\|end\\|endif\\|foreach\\|if\\|switch\\|then\\|while\\)\\($\\|[^-._a-z0-9]\\)" 2 font-lock-keyword-face t)))
  "*Rules for highlighting C shell scripts.")

(defvar sh-tcsh-font-lock-keywords sh-csh-font-lock-keywords
  "*Rules for highlighting Toronto C shell scripts.")



;; mode-command and utility functions

;;;###dont-autoload
(defun sh-or-other-mode ()
  "Decide whether this is a compiled executable or a script.
Usually the file-names of scripts and binaries cannot be automatically
distinguished, so the presence of an executable's magic number is used."
  (funcall (or (let ((l magic-number-alist))
		 (while (and l
			     (not (looking-at (car (car l)))))
		   (setq l (cdr l)))
		 (cdr (car l)))
	       'sh-mode)))


;;;###dont-autoload
(defun sh-mode ()
  "Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

The syntax of the statements varies with the shell being used.  The syntax of
statements can be modified by putting a property on the command or new ones
defined with `define-sh-skeleton'.  For example

    (put 'sh-until 'ksh '(() \"until \" _ \\n > \"do\" \\n \"done\"))
or
    (put 'sh-if 'smush '(\"What? \" \"If ya got ( \" str \" ) ya betta { \" _ \" }\"))

where `sh-until' or `sh-if' have been or will be defined by `define-sh-skeleton'.

The following commands are available, based on the current shell's syntax:

\\[sh-case]	 case statement
\\[sh-for]	 for loop
\\[sh-function]	 function definition
\\[sh-if]	 if statement
\\[sh-indexed-loop]	 indexed loop from 1 to n
\\[sh-select]	 select statement
\\[sh-until]	 until loop
\\[sh-while]	 while loop

\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[sh-newline-and-indent]	 Delete unquoted space and indent new line same as this one.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-manual-entry]	 Display the Unix manual entry for the current command or shell.

\\[sh-query-for-variable]	 Unless quoted with \\, query for a variable with completions offered.
\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.
{, (, [, ', \", `
	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sh-mode-syntax-table)
  (use-local-map sh-mode-map)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'after-save-hook)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'sh-shell-path)
  (make-local-variable 'sh-shell)
  (make-local-variable 'sh-shell-is-csh)
  (make-local-variable 'pair-alist)
  (make-local-variable 'pair-filter)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'sh-variables)
  (setq major-mode 'sh-mode
	mode-name "Shell-script"
	;; Why can't Emacs have one standard function with some parameters?
	;; Only few modes actually analyse the previous line's contents
	indent-line-function 'sh-indent-line
	comment-start "# "
	after-save-hook 'sh-chmod
	tab-width sh-tab-width
	;; C shells do
	require-final-newline t
	pair-alist '((?` _ ?`))
	pair-filter 'sh-quoted-p)
  ;; parse or insert magic number for exec
  (save-excursion
    (goto-char (point-min))
    (sh-set-shell
     (if (looking-at "#![\t ]*\\([^\t\n ]+\\)")
	 (buffer-substring (match-beginning 1) (match-end 1))
       sh-shell-path)))
  ;; find-file is set by `normal-mode' when called by `after-find-file'
  (and (boundp 'find-file) find-file
       (or (eq sh-find-file-modifies t)
	   (set-buffer-modified-p nil)))
  (run-hooks 'sh-mode-hook))
;;;###dont-autoload
(defalias 'shell-script-mode 'sh-mode)



(defmacro define-sh-skeleton (command documentation &rest definitions)
  "Define COMMAND with [DOCSTRING] to insert statements as in DEFINITION ...
Prior definitions (e.g. from ~/.emacs) are maintained.
Each definition is built up as (SHELL PROMPT ELEMENT ...).  Alternately
a synonym definition can be (SHELL . PREVIOUSLY-DEFINED-SHELL).

For the meaning of (PROMPT ELEMENT ...) see `skeleton-insert'.
Each DEFINITION is actually stored as
	(put COMMAND SHELL (PROMPT ELEMENT ...)),
which you can also do yourself."
  (or (stringp documentation)
      (setq definitions (cons documentation definitions)
	    documentation ""))
  ;; The compiled version doesn't.
  (require 'backquote)
  (`(progn
      (let ((definitions '(, definitions)))
	(while definitions
	  ;; skeleton need not be loaded to define these
	  (or (and (not (if (boundp 'skeleton-debug) skeleton-debug))
		   (get '(, command) (car (car definitions))))
	      (put '(, command) (car (car definitions))
		   (if (symbolp (cdr (car definitions)))
		       (get '(, command) (cdr (car definitions)))
		     (cdr (car definitions)))))
	  (setq definitions (cdr definitions))))
      (put '(, command) 'menu-enable '(get '(, command) sh-shell))
      (defun (, command) ()
	(, documentation)
	(interactive)
	(skeleton-insert
	 (or (get '(, command) sh-shell)
	     (error "%s statement syntax not defined for shell %s."
		    '(, command) sh-shell)))))))



(defun sh-indent-line ()
  "Indent as far as preceding line, then by steps of `tab-width'.
If previous line starts with a comment, it's considered empty."
  (interactive)
  (let ((previous (save-excursion
		    (line-move -1)
		    (back-to-indentation)
		    (if (looking-at comment-start-skip)
			0
		      (current-column)))))
    (save-excursion
      (indent-to (if (eq this-command 'newline-and-indent)
		     previous
		   (if (< (current-column)
			  (progn (back-to-indentation)
				 (current-column)))
		       (if (eolp) previous 0)
		     (if (eolp)
			 (max previous (* (1+ (/ (current-column) tab-width))
					  tab-width))
		       (* (1+ (/ (current-column) tab-width)) tab-width))))))
      (if (< (current-column) (current-indentation))
	  (skip-chars-forward " \t"))))


(defun sh-remember-variable (var)
  "Make VARIABLE available for future completing reads in this buffer."
  (or (< (length var) sh-remember-variable-min)
      (assoc var sh-variables)
      (setq sh-variables (cons (list var) sh-variables)))
  var)


;; Augment the standard variables by those found in the environment.
(if (boundp 'process-environment)(let ((l process-environment))
  (while l
    (sh-remember-variable (substring (car l)
				     0 (string-match "=" (car l))))
    (setq l (cdr l)))))



(defun sh-quoted-p ()
  "Is point preceded by an odd number of backslashes?"
  (eq 1 (% (- (point) (save-excursion
			(skip-chars-backward "\\\\")
			(point)))
	   2)))



(defun sh-executable (command)
  "If COMMAND is an executable in $PATH its full name is returned.  Else nil."
  (let ((point (point))
	(buffer-modified-p (buffer-modified-p))
	buffer-read-only after-change-function)
    (call-process "sh" nil t nil "-c" (concat "type " command))
    (setq point (prog1 (point)
		  (goto-char point)))
    (prog1
	(and (looking-at sh-executable)
	     (eq point (match-end 0))
	     (buffer-substring (match-beginning 1) (match-end 1)))
      (delete-region (point) point)
      (set-buffer-modified-p buffer-modified-p))))



(defun sh-chmod ()
  "This gets called after saving a file to assure that it be executable.
You can set the absolute or relative mode with `sh-chmod-argument'."
  (if sh-chmod-argument
       (or (file-executable-p buffer-file-name)
	   (shell-command (concat "chmod " sh-chmod-argument
				  " " buffer-file-name)))))

;; statement syntax-commands for various shells

;; You are welcome to add the syntax or even completely new statements as
;; appropriate for your favorite shell.

(define-sh-skeleton sh-case
  "Insert a case/switch statement in the current shell's syntax."
  (sh "expression: "
      "case " str " in" \n
      > (read-string "pattern: ") ?\) \n
      > _ \n
      ";;" \n
      ( "other pattern, %s: "
	< str ?\) \n
	> \n
	";;" \n)
      < "*)" \n
      > \n
      resume:
      < < "esac")
  (ksh . sh)
  (bash . sh)
  (csh "expression: "
       "switch( " str " )" \n
       > "case " (read-string "pattern: ") ?: \n
       > _ \n
       "breaksw" \n
       ( "other pattern, %s: "
	 < "case " str ?: \n
	 > \n
	 "breaksw" \n)
       < "default:" \n
       > \n
       resume:
       < < "endsw")
  (tcsh . csh))



(define-sh-skeleton sh-for
  "Insert a for loop in the current shell's syntax."
  (sh "Index variable: "
      "for " str " in " _ "; do" \n
      > ?$ (sh-remember-variable str) \n
      < "done")
  (ksh . sh)
  (bash . sh)
  (csh "Index variable: "
       "foreach " str " ( " _ " )" \n
       > ?$ (sh-remember-variable str) \n
       < "end")
  (tcsh . csh))



(define-sh-skeleton sh-indexed-loop
  "Insert an indexed loop from 1 to n in the current shell's syntax."
  (sh "Index variable: "
      str "=1" \n
      "while [ $" str " -le "
      (read-string "upper limit: ")
      " ]; do" \n
      > _ ?$ str \n
      str ?= (sh-add (sh-remember-variable str) 1) \n
      < "done")
  (ksh . sh)
  (bash . sh)
  (csh "Index variable: "
       "@ " str " = 1" \n
       "while( $" str " <= "
       (read-string "upper limit: ")
       " )" \n
       > _ ?$ (sh-remember-variable str) \n
       "@ " str "++" \n
       < "end")
  (tcsh . csh))



(defun sh-add (var delta)
  "Insert an addition of VAR and prefix DELTA for Bourne type shells."
  (interactive
   (list (sh-remember-variable
	  (completing-read "Variable: " sh-variables
			   (lambda (element)
			     (or (not (cdr element))
				 (memq sh-shell (cdr element))))))
	 (prefix-numeric-value current-prefix-arg)))
  (setq delta (concat (if (< delta 0) " - " " + ")
		      (abs delta)))
  (skeleton-insert
   (assq sh-shell
	 '((sh "`expr $" var delta "`")
	   (ksh "$(( $" var delta " ))")
	   (bash "$[ $" var delta " ]")))
   t))



(define-sh-skeleton sh-function
  "Insert a function definition in the current shell's syntax."
  (sh ()
      "() {" \n
      > _ \n
      < "}")
  (ksh "name: "
       "function " str " {" \n
       > _ \n
       < "}")
  (bash "name: "
       "function " str "() {" \n
       > _ \n
       < "}"))



(define-sh-skeleton sh-if
  "Insert an if statement in the current shell's syntax."
  (sh "condition: "
      "if [ " str " ]; then" \n
      > _ \n
      ( "other condition, %s: "
	< "elif [ " str " ]; then" \n
	> \n)
      < "else" \n
      > \n
      resume:
      < "fi")
  (ksh . sh)
  (bash . sh)
  (csh "condition: "
       "if( " str " ) then" \n
       > _ \n
       ( "other condition, %s: "
	 < "else if ( " str " ) then" \n
	 > \n)
       < "else" \n
       > \n
       resume:
       < "endif")
  (tcsh . csh))



(define-sh-skeleton sh-select
  "Insert a select statement in the current shell's syntax."
  (ksh "Index variable: "
       "select " str " in " _ "; do" \n
       > ?$ str \n
       < "done"))
(put 'sh-select 'menu-enable '(get 'sh-select sh-shell))



(define-sh-skeleton sh-until
  "Insert an until loop in the current shell's syntax."
  (sh "condition: "
      "until [ " str " ]; do" \n
      > _ \n
      < "done")
  (ksh . sh)
  (bash . sh))
(put 'sh-until 'menu-enable '(get 'sh-until sh-shell))


(define-sh-skeleton sh-while
  "Insert a while loop in the current shell's syntax."
  (sh "condition: "
      "while [ " str " ]; do" \n
      > _ \n
      < "done")
  (ksh . sh)
  (bash . sh)
  (csh "condition: "
       "while( " str " )" \n
       > _ \n
       < "end")
  (tcsh . csh))



(defun sh-query-for-variable (arg)
  "Unless quoted with `\\', query for variable-name with completions.
Prefix arg 0 means don't insert `$' before the variable.
Prefix arg 2 or more means only do self-insert that many times.
  If { is pressed as the first character, it will surround the variable name."
  (interactive "*p")
  (or (prog1 (or (> arg 1)
		 (sh-quoted-p))
	(self-insert-command arg))
    (let (completion-ignore-case
	  (minibuffer-local-completion-map
	   (or (get 'sh-query-for-variable 'keymap)
	       (put 'sh-query-for-variable 'keymap
		    (copy-keymap minibuffer-local-completion-map))))
	  (buffer (current-buffer)))
      ;; local function that depends on `arg' and `buffer'
      (define-key minibuffer-local-completion-map "{"
	(lambda () (interactive)
	  (if (or arg (> (point) 1))
	      (beep)
	    (save-window-excursion
	      (setq arg t)
	      (switch-to-buffer-other-window buffer)
	      (insert "{}")))))
      (insert
       (prog1
	   (sh-remember-variable
	    (completing-read "Variable: " sh-variables
			     (lambda (element)
			       (or (not (cdr element))
				   (memq sh-shell (cdr element))))))
	 (if (eq t arg) (forward-char 1))))
      (if (eq t arg) (forward-char 1)))))



(defun sh-assignment (arg)
  "Insert self.  Remember previous identifier for future completing read."
  (interactive "p")
  (if (eq arg 1)
      (sh-remember-variable
       (save-excursion
	 (buffer-substring
	  (progn
	    (if (memq sh-shell sh-assignment-space)
		(skip-chars-backward " \t"))
	    (point))
	  (progn
	    (skip-chars-backward "a-zA-Z0-9_")
	    (point))))))
  (self-insert-command arg))



(defun sh-maybe-here-document (arg)
  "Inserts self.  Without prefix, following unquoted `<' inserts here document.
The document is bounded by `sh-here-document-word'."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (or arg
      (not (eq (char-after (- (point) 2)) last-command-char))
      (save-excursion
	(goto-char (- (point) 2))
	(sh-quoted-p))
      (progn
	(insert sh-here-document-word)
	(or (looking-at "[ \t\n]") (insert ? ))
	(end-of-line 1)
	(newline)
	(save-excursion (insert ?\n sh-here-document-word)))))


;; various other commands

(autoload 'comint-dynamic-complete-filename "comint"
  "Dynamically complete the filename at point." t)



(defun sh-newline-and-indent (&optional arg)
  "Strip unquoted whitespace, insert newline, and indent like current line.
Unquoted whitespace is stripped from the current line's end, unless a
prefix ARG is given."
  (interactive "*P")
  (let ((previous (current-indentation))
	(end-of-line (point)))
    (if arg ()
      (skip-chars-backward " \t") 
      (and (< (point) end-of-line)
	   (sh-quoted-p)
	   (forward-char 1))
      (delete-region (point) end-of-line))
    (newline)
    (indent-to previous)))



(defun sh-set-shell (shell)
  "Set this buffer's shell to SHELL (a string).
Calls the value of `sh-set-shell-hook' if set."
  (interactive "sName or path of shell: ")
  (save-excursion
    (goto-char (point-min))
    (setq sh-shell-path (if (file-name-absolute-p shell)
				      shell
				    (or (sh-executable shell)
					(error "Cannot find %s." shell)))
	  sh-shell (intern (file-name-nondirectory sh-shell-path))
	  sh-shell-is-csh (memq sh-shell '(csh tcsh))
	  font-lock-defaults
	  (let ((keywords (intern-soft (format "sh-%s-font-lock-keywords"
					       sh-shell))))
	    (list (if (and keywords (boundp keywords))
		      keywords
		    'sh-font-lock-keywords)))
	  comment-start-skip (if sh-shell-is-csh
				 "\\(^\\|[^$]\\|\\$[^{]\\)#+[\t ]*"
			       "\\(^\\|[^$]\\|\\$[^{]\\)\\B#+[\t ]*")
	  mode-line-process (format ": %s" sh-shell)
	  shell (concat sh-shell-path
			(and sh-shell-argument " ")
			sh-shell-argument))
    (and (not buffer-read-only)
	 (not (if buffer-file-name
		  (string-match sh-magicless-file-regexp buffer-file-name)))
	 ;; find-file is set by `normal-mode' when called by `after-find-file'
	 (if (and (boundp 'find-file) find-file) sh-find-file-modifies t)
	 (if (looking-at "#!")
	     (and (skip-chars-forward "#! \t")
		  (not (string= shell
				(buffer-substring (point)
						  (save-excursion (end-of-line)
								  (point)))))
		  (if sh-query-for-magic
		      (y-or-n-p (concat "Replace magic number by ``#! "
					shell "''? "))
		    (message "Magic number ``%s'' replaced."
			     (buffer-substring (point-min) (point))))
		  (not (delete-region (point) (progn (end-of-line) (point))))
		  (insert shell))
	   (if (if sh-query-for-magic
		   (y-or-n-p (concat "Add ``#! " shell "''? "))
		 t)
	       (insert "#! " shell ?\n)))))
  (run-hooks 'sh-set-shell-hook))



(defun sh-beginning-of-command ()
  "Move point to successive beginnings of commands."
  (interactive)
  (if (re-search-backward sh-beginning-of-command nil t)
      (goto-char (match-beginning 2))))



(defun sh-end-of-command ()
  "Move point to successive ends of commands."
  (interactive)
  (if (re-search-forward sh-end-of-command nil t)
      (goto-char (match-end 1))))



(defun sh-manual-entry (arg)
  "Display the Unix manual entry for the current command or shell.
Universal argument ARG, is passed to `Man-getpage-in-background'."
  (interactive "P")
  (let ((command (save-excursion
		   (sh-beginning-of-command)
		   (sh-executable
		    (buffer-substring (point)
				      (progn (forward-sexp) (point)))))))
    (setq command (read-input (concat "Manual entry (default "
				      (symbol-name sh-shell)
				      "): ")
			      (if command
				  (file-name-nondirectory command))))
    (manual-entry (if (string= command "")
		      (symbol-name sh-shell)
		    command)
		  arg)))

;; sh-script.el ends here
