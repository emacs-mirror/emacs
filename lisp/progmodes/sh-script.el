;;; sh-script.el --- shell-script editing commands for Emacs

;; Copyright (C) 1993, 94, 95, 96, 97, 1999 by Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Version: 2.0e
;; Maintainer: FSF
;; Keywords: languages, unix

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

;; Major mode for editing shell scripts.  Bourne, C and rc shells as well
;; as various derivatives are supported and easily derived from.  Structured
;; statements can be inserted with one command or abbrev.  Completion is
;; available for filenames, variables known from the script, the shell and
;; the environment as well as commands.

;;; Known Bugs:

;; - In Bourne the keyword `in' is not anchored to case, for, select ...
;; - Variables in `"' strings aren't fontified because there's no way of
;;   syntactically distinguishing those from `'' strings.

;;; Code:

;; page 1:	variables and settings
;; page 2:	mode-command and utility functions
;; page 3:	statement syntax-commands for various shells
;; page 4:	various other commands

(require 'executable)

(defvar sh-mode-hook nil
  "*Hook run by `sh-mode'.")

(defvar sh-set-shell-hook nil
  "*Hook run by `sh-set-shell'.")

(defgroup sh nil
  "Shell programming utilities"
  :group 'unix
  :group 'languages)

(defgroup sh-script nil
  "Shell script mode"
  :group 'sh
  :prefix "sh-")


(defcustom sh-ancestor-alist
  '((ash . sh)
    (bash . jsh)
    (dtksh . ksh)
    (es . rc)
    (itcsh . tcsh)
    (jcsh . csh)
    (jsh . sh)
    (ksh . ksh88)
    (ksh88 . jsh)
    (oash . sh)
    (pdksh . ksh88)
    (posix . sh)
    (tcsh . csh)
    (wksh . ksh88)
    (wsh . sh)
    (zsh . ksh88)
    (rpm . sh))
  "*Alist showing the direct ancestor of various shells.
This is the basis for `sh-feature'.  See also `sh-alias-alist'.
By default we have the following three hierarchies:

csh		C Shell
  jcsh		C Shell with Job Control
  tcsh		Toronto C Shell
    itcsh	? Toronto C Shell
rc		Plan 9 Shell
  es		Extensible Shell
sh		Bourne Shell
  ash		? Shell
  jsh		Bourne Shell with Job Control
    bash	GNU Bourne Again Shell
    ksh88	Korn Shell '88
      ksh	Korn Shell '93
	dtksh	CDE Desktop Korn Shell
      pdksh	Public Domain Korn Shell
      wksh	Window Korn Shell
      zsh	Z Shell
  oash		SCO OA (curses) Shell
  posix		IEEE 1003.2 Shell Standard
  wsh		? Shell"
  :type '(repeat (cons symbol symbol))
  :group 'sh-script)


(defcustom sh-alias-alist
  (nconc (if (eq system-type 'gnu/linux)
	     '((csh . tcsh)
	       (ksh . pdksh)))
	 ;; for the time being
	 '((ksh . ksh88)
	   (sh5 . sh)))
  "*Alist for transforming shell names to what they really are.
Use this where the name of the executable doesn't correspond to the type of
shell it really is."
  :type '(repeat (cons symbol symbol))
  :group 'sh-script)


(defcustom sh-shell-file
  (or
   ;; On MSDOS and Windows, collapse $SHELL to lower-case and remove
   ;; the executable extension, so comparisons with the list of
   ;; known shells work.
   (and (memq system-type '(ms-dos windows-nt))
	(let* ((shell (getenv "SHELL"))
	       (shell-base
		(and shell (file-name-nondirectory shell))))
	  ;; shell-script mode doesn't support DOS/Windows shells,
	  ;; so use the default instead.
	  (if (or (null shell)
		  (member (downcase shell-base)
			  '("command.com" "cmd.exe" "4dos.com" "ndos.com"
			    "cmdproxy.exe")))
	      "/bin/sh"
	    (file-name-sans-extension (downcase shell)))))
   (getenv "SHELL")
   "/bin/sh")
  "*The executable file name for the shell being programmed."
  :type 'string
  :group 'sh-script)


(defcustom sh-shell-arg
  ;; bash does not need any options when run in a shell script,
  '((bash)
    (csh . "-f")
    (pdksh)
    ;; Bill_Mann@praxisint.com says -p with ksh can do harm.
    (ksh88)
    ;; -p means don't initialize functions from the environment.
    (rc . "-p")
    ;; Someone proposed -motif, but we don't want to encourage
    ;; use of a non-free widget set.
    (wksh)
    ;; -f means don't run .zshrc.
    (zsh . "-f"))
  "*Single argument string for the magic number.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (const :tag "No Arguments" nil)
			       (string :tag "Arguments")
			       (cons :format "Evaluate: %v"
				     (const :format "" eval)
				     sexp))))
  :group 'sh-script)

(defcustom sh-imenu-generic-expression
  (list
   (cons 'sh
	 (concat
	  "\\(^\\s-*function\\s-+[A-Za-z_][A-Za-z_0-9]*\\)"
	  "\\|"
	  "\\(^\\s-*[A-Za-z_][A-Za-z_0-9]*\\s-*()\\)")))
  "*Regular expression for recognizing shell function definitions.
See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       regexp))
  :group 'sh-script
  :version "20.3")

(defvar sh-shell-variables nil
  "Alist of shell variable names that should be included in completion.
These are used for completion in addition to all the variables named
in `process-environment'.  Each element looks like (VAR . VAR), where
the car and cdr are the same symbol.")

(defvar sh-shell-variables-initialized nil
  "Non-nil if `sh-shell-variables' is initialized.")

(defun sh-canonicalize-shell (shell)
  "Convert a shell name SHELL to the one we should handle it as."
  (if (string-match "\\.exe\\'" shell)
      (setq shell (substring shell 0 (match-beginning 0))))
  (or (symbolp shell)
      (setq shell (intern shell)))
  (or (cdr (assq shell sh-alias-alist))
      shell))

(defvar sh-shell (sh-canonicalize-shell (file-name-nondirectory sh-shell-file))
  "The shell being programmed.  This is set by \\[sh-set-shell].")

;;; I turned off this feature because it doesn't permit typing commands
;;; in the usual way without help.
;;;(defvar sh-abbrevs
;;;  '((csh eval sh-abbrevs shell
;;;	 "switch" 'sh-case
;;;	 "getopts" 'sh-while-getopts)

;;;    (es eval sh-abbrevs shell
;;;	"function" 'sh-function)

;;;    (ksh88 eval sh-abbrevs sh
;;;	   "select" 'sh-select)

;;;    (rc eval sh-abbrevs shell
;;;	"case" 'sh-case
;;;	"function" 'sh-function)

;;;    (sh eval sh-abbrevs shell
;;;	"case" 'sh-case
;;;	"function" 'sh-function
;;;	"until" 'sh-until
;;;	"getopts" 'sh-while-getopts)

;;;    ;; The next entry is only used for defining the others
;;;    (shell "for" sh-for
;;;	   "loop" sh-indexed-loop
;;;	   "if" sh-if
;;;	   "tmpfile" sh-tmp-file
;;;	   "while" sh-while)

;;;    (zsh eval sh-abbrevs ksh88
;;;	 "repeat" 'sh-repeat))
;;;  "Abbrev-table used in Shell-Script mode.  See `sh-feature'.
;;;Due to the internal workings of abbrev tables, the shell name symbol is
;;;actually defined as the table for the like of \\[edit-abbrevs].")



(defvar sh-mode-syntax-table
  '((sh eval sh-mode-syntax-table ()
	?\# "<"
	?\^l ">#"
	?\n ">#"
	?\" "\"\""
	?\' "\"'"
	?\` "\"`"
	?! "_"
	?% "_"
	?: "_"
	?. "_"
	?^ "_"
	?~ "_")
    (csh eval identity sh)
    (rc eval identity sh))
  "Syntax-table used in Shell-Script mode.  See `sh-feature'.")



(defvar sh-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Insert")))
    (define-key map "\C-c(" 'sh-function)
    (define-key map "\C-c\C-w" 'sh-while)
    (define-key map "\C-c\C-u" 'sh-until)
    (define-key map "\C-c\C-t" 'sh-tmp-file)
    (define-key map "\C-c\C-s" 'sh-select)
    (define-key map "\C-c\C-r" 'sh-repeat)
    (define-key map "\C-c\C-o" 'sh-while-getopts)
    (define-key map "\C-c\C-l" 'sh-indexed-loop)
    (define-key map "\C-c\C-i" 'sh-if)
    (define-key map "\C-c\C-f" 'sh-for)
    (define-key map "\C-c\C-c" 'sh-case)

    (define-key map "=" 'sh-assignment)
    (define-key map "\C-c+" 'sh-add)
    (define-key map "\C-\M-x" 'sh-execute-region)
    (define-key map "\C-c\C-x" 'executable-interpret)
    (define-key map "<" 'sh-maybe-here-document)
    (define-key map "(" 'skeleton-pair-insert-maybe)
    (define-key map "{" 'skeleton-pair-insert-maybe)
    (define-key map "[" 'skeleton-pair-insert-maybe)
    (define-key map "'" 'skeleton-pair-insert-maybe)
    (define-key map "`" 'skeleton-pair-insert-maybe)
    (define-key map "\"" 'skeleton-pair-insert-maybe)

    (define-key map "\t" 'sh-indent-line)
    (substitute-key-definition 'complete-tag 'comint-dynamic-complete
			       map (current-global-map))
    (substitute-key-definition 'newline-and-indent 'sh-newline-and-indent
			       map (current-global-map))
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
    (define-key map [menu-bar insert] (cons "Insert" menu-map))
    (define-key menu-map [sh-while]	'("While Loop" . sh-while))
    (define-key menu-map [sh-until]	'("Until Loop" . sh-until))
    (define-key menu-map [sh-tmp-file]	'("Temporary File" . sh-tmp-file))
    (define-key menu-map [sh-select]	'("Select Statement" . sh-select))
    (define-key menu-map [sh-repeat]	'("Repeat Loop" . sh-repeat))
    (define-key menu-map [sh-while-getopts]
					'("Options Loop" . sh-while-getopts))
    (define-key menu-map [sh-indexed-loop]
					'("Indexed Loop" . sh-indexed-loop))
    (define-key menu-map [sh-if]	'("If Statement" . sh-if))
    (define-key menu-map [sh-for]	'("For Loop" . sh-for))
    (define-key menu-map [sh-case]	'("Case Statement" . sh-case))
    map)
  "Keymap used in Shell-Script mode.")



(defcustom sh-dynamic-complete-functions
  '(shell-dynamic-complete-environment-variable
    shell-dynamic-complete-command
    comint-dynamic-complete-filename)
  "*Functions for doing TAB dynamic completion."
  :type '(repeat function)
  :group 'sh-script)


(defcustom sh-require-final-newline
  '((csh . t)
    (pdksh . t)
    (rc eval . require-final-newline)
    (sh eval . require-final-newline))
  "*Value of `require-final-newline' in Shell-Script mode buffers.
See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (const :tag "require" t)
			       (cons :format "Evaluate: %v"
				     (const :format "" eval)
				     sexp))))
  :group 'sh-script)


(defcustom sh-assignment-regexp
  '((csh . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.+\\]\\)?[ \t]*[-+*/%^]?=")
    ;; actually spaces are only supported in let/(( ... ))
    (ksh88 . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.+\\]\\)?[ \t]*\\([-+*/%&|~^]\\|<<\\|>>\\)?=")
    (rc . "\\<\\([a-zA-Z0-9_*]+\\)[ \t]*=")
    (sh . "\\<\\([a-zA-Z0-9_]+\\)="))
  "*Regexp for the variable name and what may follow in an assignment.
First grouping matches the variable name.  This is upto and including the `='
sign.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice regexp
			       (cons :format "Evaluate: %v"
				     (const :format "" eval)
				     sexp))))
  :group 'sh-script)


(defcustom sh-indentation 4
  "The width for further indentation in Shell-Script mode."
  :type 'integer
  :group 'sh-script)


(defcustom sh-remember-variable-min 3
  "*Don't remember variables less than this length for completing reads."
  :type 'integer
  :group 'sh-script)


(defvar sh-header-marker nil
  "When non-`nil' is the end of header for prepending by \\[sh-execute-region].
That command is also used for setting this variable.")


(defcustom sh-beginning-of-command
  "\\([;({`|&]\\|\\`\\|[^\\]\n\\)[ \t]*\\([/~a-zA-Z0-9:]\\)"
  "*Regexp to determine the beginning of a shell command.
The actual command starts at the beginning of the second \\(grouping\\)."
  :type 'regexp
  :group 'sh-script)


(defcustom sh-end-of-command
  "\\([/~a-zA-Z0-9:]\\)[ \t]*\\([;#)}`|&]\\|$\\)"
  "*Regexp to determine the end of a shell command.
The actual command ends at the end of the first \\(grouping\\)."
  :type 'regexp
  :group 'sh-script)



(defvar sh-here-document-word "EOF"
  "Word to delimit here documents.")

(defvar sh-test
  '((sh "[  ]" . 3)
    (ksh88 "[[  ]]" . 4))
  "Initial input in Bourne if, while and until skeletons.  See `sh-feature'.")


;; customized this out of sheer bravado.  not for the faint of heart.
;; but it *did* have an asterisk in the docstring!
(defcustom sh-builtins
  '((bash eval sh-append posix
	  "alias" "bg" "bind" "builtin" "declare" "dirs" "enable" "fc" "fg"
	  "help" "history" "jobs" "kill" "let" "local" "popd" "pushd" "source"
	  "suspend" "typeset" "unalias")

    ;; The next entry is only used for defining the others
    (bourne eval sh-append shell
	    "eval" "export" "getopts" "newgrp" "pwd" "read" "readonly"
	    "times" "ulimit")

    (csh eval sh-append shell
	 "alias" "chdir" "glob" "history" "limit" "nice" "nohup" "rehash"
	 "setenv" "source" "time" "unalias" "unhash")

    (dtksh eval identity wksh)

    (es "access" "apids" "cd" "echo" "eval" "false" "let" "limit" "local"
	"newpgrp" "result" "time" "umask" "var" "vars" "wait" "whatis")

    (jsh eval sh-append sh
	 "bg" "fg" "jobs" "kill" "stop" "suspend")

    (jcsh eval sh-append csh
	 "bg" "fg" "jobs" "kill" "notify" "stop" "suspend")

    (ksh88 eval sh-append bourne
	   "alias" "bg" "false" "fc" "fg" "jobs" "kill" "let" "print" "time"
	   "typeset" "unalias" "whence")

    (oash eval sh-append sh
	  "checkwin" "dateline" "error" "form" "menu" "newwin" "oadeinit"
	  "oaed" "oahelp" "oainit" "pp" "ppfile" "scan" "scrollok" "wattr"
	  "wclear" "werase" "win" "wmclose" "wmmessage" "wmopen" "wmove"
	  "wmtitle" "wrefresh")

    (pdksh eval sh-append ksh88
	   "bind")

    (posix eval sh-append sh
	   "command")

    (rc "builtin" "cd" "echo" "eval" "limit" "newpgrp" "shift" "umask" "wait"
	"whatis")

    (sh eval sh-append bourne
	"hash" "test" "type")

    ;; The next entry is only used for defining the others
    (shell "cd" "echo" "eval" "set" "shift" "umask" "unset" "wait")

    (wksh eval sh-append ksh88
	  "Xt[A-Z][A-Za-z]*")

    (zsh eval sh-append ksh88
	 "autoload" "bindkey" "builtin" "chdir" "compctl" "declare" "dirs"
	 "disable" "disown" "echotc" "enable" "functions" "getln" "hash"
	 "history" "integer" "limit" "local" "log" "popd" "pushd" "r"
	 "readonly" "rehash" "sched" "setopt" "source" "suspend" "true"
	 "ttyctl" "type" "unfunction" "unhash" "unlimit" "unsetopt" "vared"
	 "which"))
  "*List of all shell builtins for completing read and fontification.
Note that on some systems not all builtins are available or some are
implemented as aliases.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (cons :format "Evaluate: %v"
				     (const :format "" eval)
				     sexp))))
  :group 'sh-script)



(defcustom sh-leading-keywords
  '((csh "else")

    (es "true" "unwind-protect" "whatis")

    (rc "else")

    (sh "do" "elif" "else" "if" "then" "trap" "type" "until" "while"))
  "*List of keywords that may be immediately followed by a builtin or keyword.
Given some confusion between keywords and builtins depending on shell and
system, the distinction here has been based on whether they influence the
flow of control or syntax.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (cons :format "Evaluate: %v"
				     (const :format "" eval)
				     sexp))))
  :group 'sh-script)


(defcustom sh-other-keywords
  '((bash eval sh-append bourne
	  "bye" "logout")

    ;; The next entry is only used for defining the others
    (bourne eval sh-append sh
	    "function")

    (csh eval sh-append shell
	 "breaksw" "default" "end" "endif" "endsw" "foreach" "goto"
	 "if" "logout" "onintr" "repeat" "switch" "then" "while")

    (es "break" "catch" "exec" "exit" "fn" "for" "forever" "fork" "if"
	"return" "throw" "while")

    (ksh88 eval sh-append bourne
	   "select")

    (rc "break" "case" "exec" "exit" "fn" "for" "if" "in" "return" "switch"
	"while")

    (sh eval sh-append shell
	"done" "esac" "fi" "for" "in" "return")

    ;; The next entry is only used for defining the others
    (shell "break" "case" "continue" "exec" "exit")

    (zsh eval sh-append bash
	 "select"))
  "*List of keywords not in `sh-leading-keywords'.
See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (cons :format "Evaluate: %v"
				     (const :format "" eval)
				     sexp))))
  :group 'sh-script)



(defvar sh-variables
  '((bash eval sh-append sh
	  "allow_null_glob_expansion" "auto_resume" "BASH" "BASH_VERSION"
	  "cdable_vars" "ENV" "EUID" "FCEDIT" "FIGNORE" "glob_dot_filenames"
	  "histchars" "HISTFILE" "HISTFILESIZE" "history_control" "HISTSIZE"
	  "hostname_completion_file" "HOSTTYPE" "IGNOREEOF" "ignoreeof"
	  "LINENO" "MAIL_WARNING" "noclobber" "nolinks" "notify"
	  "no_exit_on_failed_exec" "NO_PROMPT_VARS" "OLDPWD" "OPTERR" "PPID"
	  "PROMPT_COMMAND" "PS4" "pushd_silent" "PWD" "RANDOM" "REPLY"
	  "SECONDS" "SHLVL" "TMOUT" "UID")

    (csh eval sh-append shell
	 "argv" "cdpath" "child" "echo" "histchars" "history" "home"
	 "ignoreeof" "mail" "noclobber" "noglob" "nonomatch" "path" "prompt"
	 "shell" "status" "time" "verbose")

    (es eval sh-append shell
	"apid" "cdpath" "CDPATH" "history" "home" "ifs" "noexport" "path"
	"pid" "prompt" "signals")

    (jcsh eval sh-append csh
	 "notify")

    (ksh88 eval sh-append sh
	   "ENV" "ERRNO" "FCEDIT" "FPATH" "HISTFILE" "HISTSIZE" "LINENO"
	   "OLDPWD" "PPID" "PS3" "PS4" "PWD" "RANDOM" "REPLY" "SECONDS"
	   "TMOUT")

    (oash eval sh-append sh
	  "FIELD" "FIELD_MAX" "LAST_KEY" "OALIB" "PP_ITEM" "PP_NUM")

    (rc eval sh-append shell
	"apid" "apids" "cdpath" "CDPATH" "history" "home" "ifs" "path" "pid"
	"prompt" "status")

    (sh eval sh-append shell
	"CDPATH" "IFS" "OPTARG" "OPTIND" "PS1" "PS2")

    ;; The next entry is only used for defining the others
    (shell "COLUMNS" "EDITOR" "HOME" "HUSHLOGIN" "LANG" "LC_COLLATE"
	   "LC_CTYPE" "LC_MESSAGES" "LC_MONETARY" "LC_NUMERIC" "LC_TIME"
	   "LINES" "LOGNAME" "MAIL" "MAILCHECK" "MAILPATH" "PAGER" "PATH"
	   "SHELL" "TERM" "TERMCAP" "TERMINFO" "VISUAL")

    (tcsh eval sh-append csh
	  "addsuffix" "ampm" "autocorrect" "autoexpand" "autolist"
	  "autologout" "chase_symlinks" "correct" "dextract" "edit" "el"
	  "fignore" "gid" "histlit" "HOST" "HOSTTYPE" "HPATH"
	  "ignore_symlinks" "listjobs" "listlinks" "listmax" "matchbeep"
	  "nobeep" "NOREBIND" "oid" "printexitvalue" "prompt2" "prompt3"
	  "pushdsilent" "pushdtohome" "recexact" "recognize_only_executables"
	  "rmstar" "savehist" "SHLVL" "showdots" "sl" "SYSTYPE" "tcsh" "term"
	  "tperiod" "tty" "uid" "version" "visiblebell" "watch" "who"
	  "wordchars")

    (zsh eval sh-append ksh88
	 "BAUD" "bindcmds" "cdpath" "DIRSTACKSIZE" "fignore" "FIGNORE" "fpath"
	 "HISTCHARS" "hostcmds" "hosts" "HOSTS" "LISTMAX" "LITHISTSIZE"
	 "LOGCHECK" "mailpath" "manpath" "NULLCMD" "optcmds" "path" "POSTEDIT"
	 "prompt" "PROMPT" "PROMPT2" "PROMPT3" "PROMPT4" "psvar" "PSVAR"
	 "READNULLCMD" "REPORTTIME" "RPROMPT" "RPS1" "SAVEHIST" "SPROMPT"
	 "STTY" "TIMEFMT" "TMOUT" "TMPPREFIX" "varcmds" "watch" "WATCH"
	 "WATCHFMT" "WORDCHARS" "ZDOTDIR"))
  "List of all shell variables available for completing read.
See `sh-feature'.")



(defvar sh-font-lock-keywords
  '((csh eval sh-append shell
	 '("\\${?[#?]?\\([A-Za-z_][A-Za-z0-9_]*\\|0\\)" 1
	   font-lock-variable-name-face))

    (es eval sh-append executable-font-lock-keywords
	'("\\$#?\\([A-Za-z_][A-Za-z0-9_]*\\|[0-9]+\\)" 1
	  font-lock-variable-name-face))

    (rc eval identity es)

    (sh eval sh-append shell
	;; Variable names.
	'("\\$\\({#?\\)?\\([A-Za-z_][A-Za-z0-9_]*\\|[-#?@!]\\)" 2
	  font-lock-variable-name-face)
	;; Function names.
	'("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
	'("\\<\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
	  (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t)))

    ;; The next entry is only used for defining the others
    (shell eval sh-append executable-font-lock-keywords
	   '("\\\\[^A-Za-z0-9]" 0 font-lock-string-face)
	   '("\\${?\\([A-Za-z_][A-Za-z0-9_]*\\|[0-9]+\\|[$*_]\\)" 1
	     font-lock-variable-name-face))
    (rpm eval sh-append rpm2
	 '("%{?\\(\\sw+\\)"  1 font-lock-keyword-face))
    (rpm2 eval sh-append shell
	  '("^\\(\\sw+\\):"  1 font-lock-variable-name-face)))
  "Default expressions to highlight in Shell Script modes.  See `sh-feature'.")

(defvar sh-font-lock-keywords-1
  '((sh "[ \t]in\\>"))
  "Subdued level highlighting for Shell Script modes.")

(defvar sh-font-lock-keywords-2 ()
  "Gaudy level highlighting for Shell Script modes.")

(defconst sh-font-lock-syntactic-keywords
  ;; Mark a `#' character as having punctuation syntax in a variable reference.
  ;; Really we should do this properly.  From Chet Ramey and Brian Fox:
  ;; "A `#' begins a comment when it is unquoted and at the beginning of a
  ;; word.  In the shell, words are separated by metacharacters."
  ;; To do this in a regexp would be slow as it would be anchored to the right.
  ;; But I can't be bothered to write a function to do it properly and
  ;; efficiently.  So we only do it properly for `#' in variable references and
  ;; do it efficiently by anchoring the regexp to the left.
  '(("\\${?[^}#\n\t ]*\\(##?\\)" 1 (1 . nil))))

;; mode-command and utility functions

;;;###autoload
(put 'sh-mode 'mode-class 'special)

;;;###autoload
(defun sh-mode ()
  "Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

This mode adapts to the variations between shells (see `sh-set-shell') by
means of an inheritance based feature lookup (see `sh-feature').  This
mechanism applies to all variables (including skeletons) that pertain to
shell-specific features.

The default style of this mode is that of Rosenblatt's Korn shell book.
The syntax of the statements varies with the shell being used.  The
following commands are available, based on the current shell's syntax:

\\[sh-case]	 case statement
\\[sh-for]	 for loop
\\[sh-function]	 function definition
\\[sh-if]	 if statement
\\[sh-indexed-loop]	 indexed loop from 1 to n
\\[sh-while-getopts]	 while getopts loop
\\[sh-repeat]	 repeat loop
\\[sh-select]	 select loop
\\[sh-until]	 until loop
\\[sh-while]	 while loop

\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[sh-newline-and-indent]	 Delete unquoted space and indent new line same as this one.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-execute-region]	 Have optional header and region be executed in a subshell.

\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.
{, (, [, ', \", `
	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``.

If you generally program a shell different from your login shell you can
set `sh-shell-file' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sh-mode-map)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'skeleton-end-hook)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'sh-header-marker)
  (make-local-variable 'sh-shell-file)
  (make-local-variable 'sh-shell)
  (make-local-variable 'skeleton-pair-alist)
  (make-local-variable 'skeleton-pair-filter)
  (make-local-variable 'comint-dynamic-complete-functions)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'skeleton-filter)
  (make-local-variable 'skeleton-newline-indent-rigidly)
  (make-local-variable 'sh-shell-variables)
  (make-local-variable 'sh-shell-variables-initialized)
  (make-local-variable 'imenu-generic-expression)
  (setq major-mode 'sh-mode
	mode-name "Shell-script"
	indent-line-function 'sh-indent-line
	;; not very clever, but enables wrapping skeletons around regions
	indent-region-function (lambda (b e)
				 (save-excursion
				   (goto-char b)
				   (skip-syntax-backward "-")
				   (setq b (point))
				   (goto-char e)
				   (skip-syntax-backward "-")
				   (indent-rigidly b (point) sh-indentation)))
	skeleton-end-hook (lambda ()
			    (or (eolp) (newline) (indent-relative)))
	paragraph-start (concat page-delimiter "\\|$")
	paragraph-separate paragraph-start
	comment-start "# "
	comint-dynamic-complete-functions sh-dynamic-complete-functions
	;; we can't look if previous line ended with `\'
	comint-prompt-regexp "^[ \t]*"
	font-lock-defaults
	'((sh-font-lock-keywords
	   sh-font-lock-keywords-1 sh-font-lock-keywords-2)
	  nil nil
	  ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
	  (font-lock-syntactic-keywords . sh-font-lock-syntactic-keywords))
	skeleton-pair-alist '((?` _ ?`))
	skeleton-pair-filter 'sh-quoted-p
	skeleton-further-elements '((< '(- (min sh-indentation
						(current-column)))))
	skeleton-filter 'sh-feature
	skeleton-newline-indent-rigidly t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Parse or insert magic number for exec, and set all variables depending
  ;; on the shell thus determined.
  (let ((interpreter
	 (save-excursion
	   (goto-char (point-min))
	   (cond ((looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)")
		  (match-string 2))
		 ((and buffer-file-name
		       (string-match "\\.m?spec$" buffer-file-name))
		  "rpm")))))
    (if interpreter
	(sh-set-shell interpreter nil nil)
      (progn
        ;; If we don't know the shell for this file, set the syntax
        ;; table anyway, for the user's normal choice of shell.
	(set-syntax-table (or (sh-feature sh-mode-syntax-table)
			      (standard-syntax-table)))
        ;; And avoid indent-new-comment-line (at least) losing.
        (setq comment-start-skip "#+[\t ]*"))))
  (run-hooks 'sh-mode-hook))
;;;###autoload
(defalias 'shell-script-mode 'sh-mode)


(defun sh-font-lock-keywords (&optional keywords)
  "Function to get simple fontification based on `sh-font-lock-keywords'.
This adds rules for comments and assignments."
  (sh-feature sh-font-lock-keywords
	      (when (stringp (sh-feature sh-assignment-regexp))
		(lambda (list)
		  `((,(sh-feature sh-assignment-regexp)
		     1 font-lock-variable-name-face)
		    ,@keywords
		    ,@list)))))

(defun sh-font-lock-keywords-1 (&optional builtins)
  "Function to get better fontification including keywords."
  (let ((keywords (concat "\\([;(){}`|&]\\|^\\)[ \t]*\\(\\(\\("
			  (mapconcat 'identity
				     (sh-feature sh-leading-keywords)
				     "\\|")
			  "\\)[ \t]+\\)?\\("
			  (mapconcat 'identity
				     (append (sh-feature sh-leading-keywords)
					     (sh-feature sh-other-keywords))
				     "\\|")
			  "\\)")))
    (sh-font-lock-keywords
     `(,@(if builtins
	     `((,(concat keywords "[ \t]+\\)?\\("
			 (mapconcat 'identity (sh-feature sh-builtins) "\\|")
			 "\\)\\>")
		(2 font-lock-keyword-face nil t)
		(6 font-lock-builtin-face))
	       ,@(sh-feature sh-font-lock-keywords-2)))
	 (,(concat keywords "\\)\\>")
	  2 font-lock-keyword-face)
	 ,@(sh-feature sh-font-lock-keywords-1)))))

(defun sh-font-lock-keywords-2 ()
  "Function to get better fontification including keywords and builtins."
  (sh-font-lock-keywords-1 t))


(defun sh-set-shell (shell &optional no-query-flag insert-flag)
  "Set this buffer's shell to SHELL (a string).
Makes this script executable via `executable-set-magic', and sets up the
proper starting #!-line, if INSERT-FLAG is non-nil.
Calls the value of `sh-set-shell-hook' if set."
  (interactive (list (completing-read "Name or path of shell: "
				      interpreter-mode-alist
				      (lambda (x) (eq (cdr x) 'sh-mode)))
		     (eq executable-query 'function)
		     t))
  (if (string-match "\\.exe\\'" shell)
      (setq shell (substring shell 0 (match-beginning 0))))
  (setq sh-shell (intern (file-name-nondirectory shell))
	sh-shell (or (cdr (assq sh-shell sh-alias-alist))
		     sh-shell))
  (if insert-flag
      (setq sh-shell-file
	    (executable-set-magic shell (sh-feature sh-shell-arg)
				  no-query-flag insert-flag)))
  (setq require-final-newline (sh-feature sh-require-final-newline)
;;;	local-abbrev-table (sh-feature sh-abbrevs)
;; Packages should not need to set these variables directly.  sm.
;	font-lock-keywords nil		; force resetting
;	font-lock-syntax-table nil
	comment-start-skip "#+[\t ]*"
	mode-line-process (format "[%s]" sh-shell)
	sh-shell-variables nil
	sh-shell-variables-initialized nil
	imenu-generic-expression (sh-feature sh-imenu-generic-expression)
	imenu-case-fold-search nil)
  (set-syntax-table (or (sh-feature sh-mode-syntax-table)
			(standard-syntax-table)))
  (let ((vars (sh-feature sh-variables)))
    (while vars
      (sh-remember-variable (car vars))
      (setq vars (cdr vars))))
;; Packages should not need to toggle Font Lock mode.  sm.
;  (and (boundp 'font-lock-mode)
;       font-lock-mode
;       (font-lock-mode (font-lock-mode 0)))
  (run-hooks 'sh-set-shell-hook))



(defun sh-feature (list &optional function)
  "Index ALIST by the current shell.
If ALIST isn't a list where every element is a cons, it is returned as is.
Else indexing follows an inheritance logic which works in two ways:

  - Fall back on successive ancestors (see `sh-ancestor-alist') as long as
    the alist contains no value for the current shell.
    The ultimate default is always `sh'.

  - If the value thus looked up is a list starting with `eval' its `cdr' is
    first evaluated.  If that is also a list and the first argument is a
    symbol in ALIST it is not evaluated, but rather recursively looked up in
    ALIST to allow the function called to define the value for one shell to be
    derived from another shell.  While calling the function, is the car of the
    alist element is the current shell.
    The value thus determined is physically replaced into the alist.

Optional FUNCTION is applied to the determined value and the result is cached
in ALIST."
  (or (if (consp list)
	  (let ((l list))
	    (while (and l (consp (car l)))
	      (setq l (cdr l)))
	    (if l list)))
      (if function
	  (cdr (assoc (setq function (cons sh-shell function)) list)))
      (let ((sh-shell sh-shell)
	    elt val)
	(while (and sh-shell
		    (not (setq elt (assq sh-shell list))))
	  (setq sh-shell (cdr (assq sh-shell sh-ancestor-alist))))
	;; If the shell is not known, treat it as sh.
	(unless elt
	  (setq elt (assq 'sh list)))
	(if (and (consp (setq val (cdr elt)))
		 (eq (car val) 'eval))
	    (setcdr elt
		    (setq val
			  (eval (if (consp (setq val (cdr val)))
				    (let ((sh-shell (car (cdr val)))
					  function)
				      (if (assq sh-shell list)
					  (setcar (cdr val)
						  (list 'quote
							(sh-feature list))))
				      val)
				  val)))))
	(if function
	    (nconc list
		   (list (cons function
			       (setq sh-shell (car function)
				     val (funcall (cdr function) val))))))
	val)))



;;; I commented this out because nobody calls it -- rms.
;;;(defun sh-abbrevs (ancestor &rest list)
;;;  "Iff it isn't, define the current shell as abbrev table and fill that.
;;;Abbrev table will inherit all abbrevs from ANCESTOR, which is either an abbrev
;;;table or a list of (NAME1 EXPANSION1 ...).  In addition it will define abbrevs
;;;according to the remaining arguments NAMEi EXPANSIONi ...
;;;EXPANSION may be either a string or a skeleton command."
;;;  (or (if (boundp sh-shell)
;;;	  (symbol-value sh-shell))
;;;      (progn
;;;	(if (listp ancestor)
;;;	    (nconc list ancestor))
;;;	(define-abbrev-table sh-shell ())
;;;	(if (vectorp ancestor)
;;;	    (mapatoms (lambda (atom)
;;;			(or (eq atom 0)
;;;			    (define-abbrev (symbol-value sh-shell)
;;;			      (symbol-name atom)
;;;			      (symbol-value atom)
;;;			      (symbol-function atom))))
;;;		      ancestor))
;;;	(while list
;;;	  (define-abbrev (symbol-value sh-shell)
;;;	    (car list)
;;;	    (if (stringp (car (cdr list)))
;;;		(car (cdr list))
;;;	      "")
;;;	    (if (symbolp (car (cdr list)))
;;;		(car (cdr list))))
;;;	  (setq list (cdr (cdr list)))))
;;;      (symbol-value sh-shell)))


(defun sh-mode-syntax-table (table &rest list)
  "Copy TABLE and set syntax for successive CHARs according to strings S."
  (setq table (copy-syntax-table table))
  (while list
    (modify-syntax-entry (car list) (car (cdr list)) table)
    (setq list (cdr (cdr list))))
  table)


(defun sh-append (ancestor &rest list)
  "Return list composed of first argument (a list) physically appended to rest."
  (nconc list ancestor))


(defun sh-modify (skeleton &rest list)
  "Modify a copy of SKELETON by replacing I1 with REPL1, I2 with REPL2 ..."
  (setq skeleton (copy-sequence skeleton))
  (while list
    (setcar (or (nthcdr (car list) skeleton)
		(error "Index %d out of bounds" (car list)))
	    (car (cdr list)))
    (setq list (nthcdr 2 list)))
  skeleton)


(defun sh-indent-line ()
  "Indent a line for Sh mode (shell script mode).
Indent as far as preceding non-empty line, then by steps of `sh-indentation'.
Lines containing only comments are considered empty."
  (interactive)
  (let ((previous (save-excursion
		    (while (and (progn (beginning-of-line)
				       (not (bobp)))
				(progn
				  (forward-line -1)
				  (back-to-indentation)
				  (or (eolp)
				      (eq (following-char) ?#)))))
		    (current-column)))
	current)
    (save-excursion
      (indent-to (if (eq this-command 'newline-and-indent)
		     previous
		   (if (< (current-column)
			  (setq current (progn (back-to-indentation)
					       (current-column))))
		       (if (eolp) previous 0)
		     (delete-region (point)
				    (progn (beginning-of-line) (point)))
		     (if (eolp)
			 (max previous (* (1+ (/ current sh-indentation))
					  sh-indentation))
		       (* (1+ (/ current sh-indentation)) sh-indentation))))))
    (if (< (current-column) (current-indentation))
	(skip-chars-forward " \t"))))


(defun sh-execute-region (start end &optional flag)
  "Pass optional header and region to a subshell for noninteractive execution.
The working directory is that of the buffer, and only environment variables
are already set which is why you can mark a header within the script.

With a positive prefix ARG, instead of sending region, define header from
beginning of buffer to point.  With a negative prefix ARG, instead of sending
region, clear header."
  (interactive "r\nP")
  (if flag
      (setq sh-header-marker (if (> (prefix-numeric-value flag) 0)
				 (point-marker)))
    (if sh-header-marker
	(save-excursion
	  (let (buffer-undo-list)
	    (goto-char sh-header-marker)
	    (append-to-buffer (current-buffer) start end)
	    (shell-command-on-region (point-min)
				     (setq end (+ sh-header-marker
						  (- end start)))
				     sh-shell-file)
	    (delete-region sh-header-marker end)))
      (shell-command-on-region start end (concat sh-shell-file " -")))))


(defun sh-remember-variable (var)
  "Make VARIABLE available for future completing reads in this buffer."
  (or (< (length var) sh-remember-variable-min)
      (getenv var)
      (assoc var sh-shell-variables)
      (setq sh-shell-variables (cons (cons var var) sh-shell-variables)))
  var)



(defun sh-quoted-p ()
  "Is point preceded by an odd number of backslashes?"
  (eq -1 (% (save-excursion (skip-chars-backward "\\\\")) 2)))

;; statement syntax-commands for various shells

;; You are welcome to add the syntax or even completely new statements as
;; appropriate for your favorite shell.

(define-skeleton sh-case
  "Insert a case/switch statement.  See `sh-feature'."
  (csh "expression: "
       "switch( " str " )" \n
       > "case " (read-string "pattern: ") ?: \n
       > _ \n
       "breaksw" \n
       ( "other pattern, %s: "
	 < "case " str ?: \n
	 > _ \n
	 "breaksw" \n)
       < "default:" \n
       > _ \n
       resume:
       < < "endsw")
  (es)
  (rc "expression: "
      "switch( " str " ) {" \n
      > "case " (read-string "pattern: ") \n
      > _ \n
      ( "other pattern, %s: "
	< "case " str \n
	> _ \n)
      < "case *" \n
      > _ \n
      resume:
      < < ?})
  (sh "expression: "
      "case " str " in" \n
      > (read-string "pattern: ") ?\) \n
      > _ \n
      ";;" \n
      ( "other pattern, %s: "
	< str ?\) \n
	> _ \n
	";;" \n)
      < "*)" \n
      > _ \n
      resume:
      < < "esac"))

(define-skeleton sh-for
  "Insert a for loop.  See `sh-feature'."
  (csh eval sh-modify sh
       1 "foreach "
       3 " ( "
       5 " )"
       15 "end")
  (es eval sh-modify rc
      3 " = ")
  (rc eval sh-modify sh
      1 "for( "
      5 " ) {"
      15 ?})
  (sh "Index variable: "
      "for " str " in " _ "; do" \n
      > _ | ?$ & (sh-remember-variable str) \n
      < "done"))



(define-skeleton sh-indexed-loop
  "Insert an indexed loop from 1 to n.  See `sh-feature'."
  (bash eval identity posix)
  (csh "Index variable: "
       "@ " str " = 1" \n
       "while( $" str " <= " (read-string "upper limit: ") " )" \n
       > _ ?$ str \n
       "@ " str "++" \n
       < "end")
  (es eval sh-modify rc
      3 " =")
  (ksh88 "Index variable: "
	 "integer " str "=0" \n
	 "while (( ( " str " += 1 ) <= "
	 (read-string "upper limit: ")
	 " )); do" \n
	 > _ ?$ (sh-remember-variable str) \n
	 < "done")
  (posix "Index variable: "
	 str "=1" \n
	 "while [ $" str " -le "
	 (read-string "upper limit: ")
	 " ]; do" \n
	 > _ ?$ str \n
	 str ?= (sh-add (sh-remember-variable str) 1) \n
	 < "done")
  (rc "Index variable: "
      "for( " str " in" " `{awk 'BEGIN { for( i=1; i<="
      (read-string "upper limit: ")
      "; i++ ) print i }'}) {" \n
      > _ ?$ (sh-remember-variable str) \n
      < ?})
  (sh "Index variable: "
      "for " str " in `awk 'BEGIN { for( i=1; i<="
      (read-string "upper limit: ")
      "; i++ ) print i }'`; do" \n
      > _ ?$ (sh-remember-variable str) \n
      < "done"))


(defun sh-shell-initialize-variables ()
  "Scan the buffer for variable assignments.
Add these variables to `sh-shell-variables'."
  (message "Scanning buffer `%s' for variable assignments..." (buffer-name))
  (save-excursion
    (goto-char (point-min))
    (setq sh-shell-variables-initialized t)
    (while (search-forward "=" nil t)
      (sh-assignment 0)))
  (message "Scanning buffer `%s' for variable assignments...done"
	   (buffer-name)))

(defvar sh-add-buffer)

(defun sh-add-completer (string predicate code)
  "Do completion using `sh-shell-variables', but initialize it first.
This function is designed for use as the \"completion table\",
so it takes three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda'.
nil means to return the best completion of STRING, or nil if there is none.
t means to return a list of all possible completions of STRING.
`lambda' means to return t if STRING is a valid completion as it stands."
  (let ((sh-shell-variables
	 (save-excursion
	   (set-buffer sh-add-buffer)
	   (or sh-shell-variables-initialized
	       (sh-shell-initialize-variables))
	   (nconc (mapcar (lambda (var)
			    (let ((name
				   (substring var 0 (string-match "=" var))))
			      (cons name name)))
			  process-environment)
		  sh-shell-variables))))
    (cond ((null code)
	   (try-completion string sh-shell-variables predicate))
	  ((eq code t)
	   (all-completions string sh-shell-variables predicate))
	  ((eq code 'lambda)
	   (assoc string sh-shell-variables)))))

(defun sh-add (var delta)
  "Insert an addition of VAR and prefix DELTA for Bourne (type) shell."
  (interactive
   (let ((sh-add-buffer (current-buffer)))
     (list (completing-read "Variable: " 'sh-add-completer)
	   (prefix-numeric-value current-prefix-arg))))
  (insert (sh-feature '((bash . "$[ ")
			(ksh88 . "$(( ")
			(posix . "$(( ")
			(rc . "`{expr $")
			(sh . "`expr $")
			(zsh . "$[ ")))
	  (sh-remember-variable var)
	  (if (< delta 0) " - " " + ")
	  (number-to-string (abs delta))
	  (sh-feature '((bash . " ]")
			(ksh88 . " ))")
			(posix . " ))")
			(rc . "}")
			(sh . "`")
			(zsh . " ]")))))



(define-skeleton sh-function
  "Insert a function definition.  See `sh-feature'."
  (bash eval sh-modify ksh88
	3 "() {")
  (ksh88 "name: "
	 "function " str " {" \n
	 > _ \n
	 < "}")
  (rc eval sh-modify ksh88
	1 "fn ")
  (sh ()
      "() {" \n
      > _ \n
      < "}"))



(define-skeleton sh-if
  "Insert an if statement.  See `sh-feature'."
  (csh "condition: "
       "if( " str " ) then" \n
       > _ \n
       ( "other condition, %s: "
	 < "else if( " str " ) then" \n
	 > _ \n)
       < "else" \n
       > _ \n
       resume:
       < "endif")
  (es "condition: "
      "if { " str " } {" \n
       > _ \n
       ( "other condition, %s: "
	 < "} { " str " } {" \n
	 > _ \n)
       < "} {" \n
       > _ \n
       resume:
       < ?})
  (rc eval sh-modify csh
      3 " ) {"
      8 '( "other condition, %s: "
	   < "} else if( " str " ) {" \n
	   > _ \n)
      10 "} else {"
      17 ?})
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      "if " str "; then" \n
      > _ \n
      ( "other condition, %s: "
	< "elif " str "; then" \n
	> _ \n)
      < "else" \n
      > _ \n
      resume:
      < "fi"))



(define-skeleton sh-repeat
  "Insert a repeat loop definition.  See `sh-feature'."
  (es nil
      "forever {" \n
      > _ \n
      < ?})
  (zsh "factor: "
      "repeat " str "; do"\n
      > _ \n
      < "done"))
;;;(put 'sh-repeat 'menu-enable '(sh-feature sh-repeat))



(define-skeleton sh-select
  "Insert a select statement.  See `sh-feature'."
  (ksh88 "Index variable: "
	 "select " str " in " _ "; do" \n
	 > ?$ str \n
	 < "done"))
;;;(put 'sh-select 'menu-enable '(sh-feature sh-select))



(define-skeleton sh-tmp-file
  "Insert code to setup temporary file handling.  See `sh-feature'."
  (bash eval identity ksh88)
  (csh (file-name-nondirectory (buffer-file-name))
       "set tmp = /tmp/" str ".$$" \n
       "onintr exit" \n _
       (and (goto-char (point-max))
	    (not (bolp))
	    ?\n)
       "exit:\n"
       "rm $tmp* >&/dev/null" >)
  (es (file-name-nondirectory (buffer-file-name))
      "local( signals = $signals sighup sigint; tmp = /tmp/" str ".$pid ) {" \n
      > "catch @ e {" \n
      > "rm $tmp^* >[2]/dev/null" \n
      "throw $e" \n
      < "} {" \n
      > _ \n
      < ?} \n
      < ?})
  (ksh88 eval sh-modify sh
	 6 "EXIT")
  (rc (file-name-nondirectory (buffer-file-name))
       "tmp = /tmp/" str ".$pid" \n
       "fn sigexit { rm $tmp^* >[2]/dev/null }")
  (sh (file-name-nondirectory (buffer-file-name))
      "TMP=${TMPDIR:-/tmp}/" str ".$$" \n
      "trap \"rm $TMP* 2>/dev/null\" " ?0))



(define-skeleton sh-until
  "Insert an until loop.  See `sh-feature'."
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      "until " str "; do" \n
      > _ \n
      < "done"))
;;;(put 'sh-until 'menu-enable '(sh-feature sh-until))



(define-skeleton sh-while
  "Insert a while loop.  See `sh-feature'."
  (csh eval sh-modify sh
       2 "while( "
       4 " )"
       10 "end")
  (es eval sh-modify rc
      2 "while { "
      4 " } {")
  (rc eval sh-modify csh
      4 " ) {"
      10 ?})
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      "while " str "; do" \n
      > _ \n
      < "done"))



(define-skeleton sh-while-getopts
  "Insert a while getopts loop.  See `sh-feature'.
Prompts for an options string which consists of letters for each recognized
option followed by a colon `:' if the option accepts an argument."
  (bash eval sh-modify sh
	18 "${0##*/}")
  (csh nil
       "while( 1 )" \n
       > "switch( \"$1\" )" \n
       '(setq input '("- x" . 2))
       > >
       ( "option, %s: "
	 < "case " '(eval str)
	 '(if (string-match " +" str)
	      (setq v1 (substring str (match-end 0))
		    str (substring str 0 (match-beginning 0)))
	    (setq v1 nil))
	 str ?: \n
	 > "set " v1 & " = $2" | -4 & _ \n
	 (if v1 "shift") & \n
	 "breaksw" \n)
       < "case --:" \n
       > "shift" \n
       < "default:" \n
       > "break" \n
       resume:
       < < "endsw" \n
       "shift" \n
       < "end")
  (ksh88 eval sh-modify sh
	 16 "print"
	 18 "${0##*/}"
	 36 "OPTIND-1")
  (posix eval sh-modify sh
	 18 "$(basename $0)")
  (sh "optstring: "
      "while getopts :" str " OPT; do" \n
      > "case $OPT in" \n
      > >
      '(setq v1 (append (vconcat str) nil))
      ( (prog1 (if v1 (char-to-string (car v1)))
	  (if (eq (nth 1 v1) ?:)
	      (setq v1 (nthcdr 2 v1)
		    v2 "\"$OPTARG\"")
	    (setq v1 (cdr v1)
		  v2 nil)))
	< str "|+" str ?\) \n
	> _ v2 \n
	";;" \n)
      < "*)" \n
      > "echo" " \"usage: " "`basename $0`"
      " [+-" '(setq v1 (point)) str
      '(save-excursion
	 (while (search-backward ":" v1 t)
	   (replace-match " ARG] [+-" t t)))
      (if (eq (preceding-char) ?-) -5)
      "] [--] ARGS...\"" \n
      "exit 2" \n
      < < "esac" \n
      < "done" \n
      "shift " (sh-add "OPTIND" -1)))



(defun sh-assignment (arg)
  "Remember preceding identifier for future completion and do self-insert."
  (interactive "p")
  (self-insert-command arg)
  (if (<= arg 1)
      (sh-remember-variable
       (save-excursion
	 (if (re-search-forward (sh-feature sh-assignment-regexp)
				(prog1 (point)
				  (beginning-of-line 1))
				t)
	     (match-string 1))))))



(defun sh-maybe-here-document (arg)
  "Inserts self.  Without prefix, following unquoted `<' inserts here document.
The document is bounded by `sh-here-document-word'."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (or arg
      (not (eq (char-after (- (point) 2)) last-command-char))
      (save-excursion
	(backward-char 2)
	(sh-quoted-p))
      (progn
	(insert sh-here-document-word)
	(or (eolp) (looking-at "[ \t]") (insert ? ))
	(end-of-line 1)
	(while
	    (sh-quoted-p)
	  (end-of-line 2))
	(newline)
	(save-excursion (insert ?\n sh-here-document-word)))))


;; various other commands

(autoload 'comint-dynamic-complete "comint"
  "Dynamically perform completion at point." t)

(autoload 'shell-dynamic-complete-command "shell"
  "Dynamically complete the command at point." t)

(autoload 'comint-dynamic-complete-filename "comint"
  "Dynamically complete the filename at point." t)

(autoload 'shell-dynamic-complete-environment-variable "shell"
  "Dynamically complete the environment variable at point." t)



(defun sh-newline-and-indent ()
  "Strip unquoted whitespace, insert newline, and indent like current line."
  (interactive "*")
  (indent-to (prog1 (current-indentation)
	       (delete-region (point)
			      (progn
				(or (zerop (skip-chars-backward " \t"))
				    (if (sh-quoted-p)
					(forward-char)))
				(point)))
	       (newline))))



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

(provide 'sh-script)

;; sh-script.el ends here
