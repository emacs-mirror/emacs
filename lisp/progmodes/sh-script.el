;;; sh-script.el --- shell-script editing commands for Emacs
;; Copyright (C) 1993, 1994, 1995 by Free Software Foundation, Inc.

;; Author: Daniel.Pfeiffer@Informatik.START.dbp.de, fax (+49 69) 7588-2389
;; Version: 2.0d
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Major mode for editing shell scripts.  Bourne, C and rc shells as well
;; as various derivatives are supported and easily derived from.  Structured
;; statements can be inserted with one command or abbrev.  Completion is
;; available for filenames, variables known from the script, the shell and
;; the environment as well as commands.

;;; Known Bugs:

;; - Since GNU Emacs' syntax can't handle the context-sensitive meanings of
;;   the variable/number base/comment symbol `#', that has to be fontified by
;;   regexp.  This alas means that a quote `'' or `"' in a comment will
;;   fontify VERY badly.  The alternative is to have these frequent constructs
;;   with `#' fontify as comments.  Or maybe we intoduce a 'syntax text-
;;   property?
;; - Variables in `"' strings aren't fontified because there's no way of
;;   syntactically distinguishing those from `'' strings.

;;; Code:

;; page 1:	variables and settings
;; page 2:	mode-command and utility functions
;; page 3:	statement syntax-commands for various shells
;; page 4:	various other commands

(require 'executable)


;;;###autoload
(or (assoc "sh" interpreter-mode-alist)
    (setq auto-mode-alist
	  ;; matches files
	  ;;	- that have a suffix .sh, .csh or .shar (shell archive)
	  ;;	- that contain ressources for the various shells
	  ;;	- startup files for X11
	  (cons '("\\.c?sh\\'\\|\\.shar\\'\\|/\\.\\(z?profile\\|bash_profile\\|z?login\\|bash_login\\|z?logout\\|bash_logout\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\|rcrc\\|[kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
		auto-mode-alist)
	  interpreter-mode-alist
	  (nconc '(("ash" . sh-mode)
		   ("bash" . sh-mode)
		   ("csh" . sh-mode)
		   ("dtksh" . sh-mode)
		   ("es" . sh-mode)
		   ("itcsh" . sh-mode)
		   ("jsh" . sh-mode)
		   ("ksh" . sh-mode)
		   ("oash" . sh-mode)
		   ("pdksh" . sh-mode)
		   ("rc" . sh-mode)
		   ("sh" . sh-mode)
		   ("sh5" . sh-mode)
		   ("tcsh" . sh-mode)
		   ("wksh" . sh-mode)
		   ("wsh" . sh-mode)
		   ("zsh" . sh-mode))
		 interpreter-mode-alist)))


(defvar sh-ancestor-alist
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
    (zsh . ksh88))
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
  wsh		? Shell")


(defvar sh-alias-alist
  (nconc (if (eq system-type 'linux)
	     '((csh . tcsh)
	       (ksh . pdksh)
	       (sh . bash)))
	 ;; for the time being
	 '((ksh . ksh88)
	   (sh5 . sh)))
  "*Alist for transforming shell names to what they really are.
Use this where the name of the executable doesn't correspond to the type of
shell it really is.")


(defvar sh-shells
  '(("ash") ("bash") ("csh") ("dtksh") ("es") ("itcsh") ("jsh") ("ksh")
    ("oash") ("pdksh") ("rc") ("sh") ("tcsh") ("wksh") ("wsh") ("zsh"))
  "*Alist of shells available for completing read in `sh-set-shell'.")


(defvar sh-shell-path (or (getenv "SHELL") "/bin/sh")
  "*The executable of the shell being programmed.")


(defvar sh-shell-arg
  '((bash . "-norc")
    (csh . "-f")
    (ksh88 eval progn nil (if (file-exists-p "/etc/suid_profile") nil "-p"))
    (pdksh)
    (rc . "-p")
    (wksh . "-motif")
    (zsh . "-f"))
  "*Single argument string for the magic number.  See `sh-feature'.")



(defvar sh-shell (or (cdr (assq (intern (file-name-nondirectory sh-shell-path))
				sh-alias-alist))
		     (intern (file-name-nondirectory sh-shell-path)))
  "The shell being programmed.  This is set by \\[sh-set-shell].")



(defvar sh-abbrevs
  '((csh eval sh-abbrevs shell
	 "switch" 'sh-case)

    (es eval sh-abbrevs shell
	"function" 'sh-function)

    (ksh88 eval sh-abbrevs sh
	   "select" 'sh-select)

    (rc eval sh-abbrevs shell
	"case" 'sh-case
	"function" 'sh-function)

    (sh eval sh-abbrevs shell
	"case" 'sh-case
	"function" 'sh-function
	"until" 'sh-until
	"getopts" 'sh-while-getopts)

    ;; The next entry is only used for defining the others
    (shell "for" sh-for
	   "loop" sh-indexed-loop
	   "if" sh-if
	   "tmpfile" sh-tmp-file
	   "while" sh-while)

    (zsh eval sh-abbrevs ksh88
	 "repeat" 'sh-repeat))
  "Abbrev-table used in Shell-Script mode.  See `sh-feature'.
Due to the internal workings of abbrev tables, the shell name symbol is
actually defined as the table for the like of \\[edit-abbrevs].")



(defvar sh-mode-syntax-table
  '((csh eval identity sh)
    (sh eval sh-mode-syntax-table ()
	;; #'s meanings depend on context which can't be expressed here
	;; ?\# "<"
	;; ?\^l ">#"
	;; ?\n ">#"
	?\" "\"\""
	?\' "\"'"
	?\` ".`"
	?$ "_"
	?! "_"
	?% "_"
	?: "_"
	?. "_"
	?^ "_"
	?~ "_")
    (rc eval sh-mode-syntax-table sh
	?\" "_"
	?\` "."))
  "Syntax-table used in Shell-Script mode.  See `sh-feature'.")



(defvar sh-mode-map
  (let ((map (make-sparse-keymap)))
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
    (define-key map "\C-c|" 'sh-execute-region)
    (define-key map "\C-c!" 'executable-interpret)
    (define-key map "<" 'sh-maybe-here-document)
    (define-key map "(" 'pair-insert-maybe)
    (define-key map "{" 'pair-insert-maybe)
    (define-key map "[" 'pair-insert-maybe)
    (define-key map "'" 'pair-insert-maybe)
    (define-key map "`" 'pair-insert-maybe)
    (define-key map "\"" 'pair-insert-maybe)

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
    (define-key map [menu-bar insert] 
      (cons "Insert" (make-sparse-keymap "Insert")))
    (define-key map [menu-bar insert sh-while] 
      '("While loop" . sh-while))
    (define-key map [menu-bar insert sh-until] 
      '("Until loop" . sh-until))
    (define-key map [menu-bar insert sh-tmp-file] 
      '("Temporary file" . sh-tmp-file))
    (define-key map [menu-bar insert sh-select] 
      '("Select statement" . sh-select))
    (define-key map [menu-bar insert sh-repeat] 
      '("Repeat loop" . sh-repeat))
    (define-key map [menu-bar insert sh-while-getopts] 
      '("Options loop" . sh-while-getopts))
    (define-key map [menu-bar insert sh-indexed-loop] 
      '("Indexed loop" . sh-indexed-loop))
    (define-key map [menu-bar insert sh-if] 
      '("If statement" . sh-if))
    (define-key map [menu-bar insert sh-for] 
      '("For loop" . sh-for))
    (define-key map [menu-bar insert sh-case] 
      '("Case statement" . sh-case))
    map)
  "Keymap used in Shell-Script mode.")



(defvar sh-dynamic-complete-functions
  '(shell-dynamic-complete-environment-variable
    shell-dynamic-complete-command
    comint-dynamic-complete-filename)
  "*Functions for doing TAB dynamic completion.")


(defvar sh-require-final-newline
  '((csh . t)
    (pdksh . t)
    (rc eval . require-final-newline)
    (sh eval . require-final-newline))
  "*Value of `require-final-newline' in Shell-Script mode buffers.
See `sh-feature'.")


(defvar sh-comment-prefix
  '((csh . "\\(^\\|[^$]\\|\\$[^{]\\)")
    (rc eval identity csh)
    (sh . "\\(^\\|[ \t|&;()]\\)"))
  "*Regexp matching what may come before a comment `#'.
This must contain one \\(grouping\\) since it is the basis for fontifying
comments as well as for `comment-start-skip'.
See `sh-feature'.")


(defvar sh-assignment-regexp
  '((csh . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.+\\]\\)?[ \t]*[-+*/%^]?=")
    ;; actually spaces are only supported in let/(( ... ))
    (ksh88 . "\\<\\([a-zA-Z0-9_]+\\)\\(\\[.+\\]\\)?[ \t]*\\([-+*/%&|~^]\\|<<\\|>>\\)?=")
    (rc . "\\<\\([a-zA-Z0-9_*]+\\)[ \t]*=")
    (sh . "\\<\\([a-zA-Z0-9_]+\\)="))
  "*Regexp for the variable name and what may follow in an assignment.
First grouping matches the variable name.  This is upto and including the `='
sign.  See `sh-feature'.")


(defvar sh-indentation 4
  "The width for further indentation in Shell-Script mode.")


(defvar sh-remember-variable-min 3
  "*Don't remember variables less than this length for completing reads.")


(defvar sh-header-marker nil
  "When non-`nil' is the end of header for prepending by \\[sh-execute-region].
That command is also used for setting this variable.")


(defvar sh-beginning-of-command
  "\\([;({`|&]\\|^\\)[ \t]*\\([/~:a-zA-Z0-9]\\)"
  "*Regexp to determine the beginning of a shell command.
The actual command starts at the beginning of the second \\(grouping\\).")


(defvar sh-end-of-command
  "\\([/~:a-zA-Z0-9]\\)[ \t]*\\([;#)}`|&]\\|$\\)"
  "*Regexp to determine the end of a shell command.
The actual command ends at the end of the first \\(grouping\\).")



(defvar sh-here-document-word "EOF"
  "Word to delimit here documents.")


(defvar sh-builtins
  '((bash eval sh-append sh
	  "alias" "bg" "bind" "builtin" "bye" "command" "declare" "dirs"
	  "enable" "fc" "fg" "function" "help" "history" "jobs" "kill" "let"
	  "local" "logout" "popd" "pushd" "source" "suspend" "typeset"
	  "unalias")

    ;; The next entry is only used for defining the others
    (bourne eval sh-append shell
	    "do" "done" "elif" "esac" "export" "fi" "for" "getopts" "in"
	    "newgrp" "pwd" "read" "readonly" "return" "times" "trap" "ulimit"
	    "until")

    (csh eval sh-append shell
	 "alias" "breaksw" "chdir" "default" "end" "endif" "endsw" "foreach"
	 "glob" "goto" "history" "limit" "logout" "nice" "nohup" "onintr"
	 "rehash" "repeat" "setenv" "source" "switch" "time" "unalias"
	 "unhash")

    (es "access" "apids" "break" "catch" "cd" "echo" "eval" "exec" "exit"
	"false" "fn" "for" "forever" "fork" "if" "let" "limit" "local"
	"newpgrp" "result" "return" "throw" "time" "true" "umask"
	"unwind-protect" "var" "vars" "wait" "whatis" "while")

    (jsh eval sh-append sh
	 "bg" "fg" "jobs" "kill" "stop" "suspend")

    (jcsh eval sh-append csh
	 "bg" "fg" "jobs" "kill" "notify" "stop" "suspend")

    (ksh88 eval sh-append bourne
	   "alias" "bg" "false" "fc" "fg" "function" "jobs" "kill" "let"
	   "print" "select" "time" "typeset" "unalias" "whence")

    (oash eval sh-append sh
	  "checkwin" "dateline" "error" "form" "menu" "newwin" "oadeinit"
	  "oaed" "oahelp" "oainit" "pp" "ppfile" "scan" "scrollok" "wattr"
	  "wclear" "werase" "win" "wmclose" "wmmessage" "wmopen" "wmove"
	  "wmtitle" "wrefresh")

    (pdksh eval sh-append ksh88
	   "bind")

    (posix eval sh-append sh
	   "command")

    (rc "break" "builtin" "case" "cd" "echo" "else" "eval" "exec" "exit" "fn"
	"for" "if" "in" "limit" "newpgrp" "return" "shift" "switch" "umask"
	"wait" "whatis" "while")

    (sh eval sh-append bourne
	"hash" "test" "type")

    ;; The next entry is only used for defining the others
    (shell "break" "case" "cd" "continue" "echo" "else" "eval" "exec" "exit"
	   "if" "set" "shift" "then" "umask" "unset" "wait" "while")

    (zsh eval sh-append ksh88
	 "autoload" "bindkey" "builtin" "bye" "chdir" "compctl" "declare"
	 "dirs" "disable" "disown" "echotc" "enable" "functions" "getln"
	 "hash" "history" "integer" "limit" "local" "log" "logout" "popd"
	 "pushd" "r" "readonly" "rehash" "sched" "setopt" "source" "suspend"
	 "true" "ttyctl" "type" "unfunction" "unhash" "unlimit" "unsetopt"
	 "vared" "which"))
  "*List of all shell builtins for completing read and fontification.
Note that on some systems not all builtins are available or some are
implemented as aliases.  See `sh-feature'.")


(defvar sh-leading-keywords
  '((bash eval sh-append sh
	  "builtin" "command" "enable")

    ;; The next entry is only used for defining the others
    (bourne "do" "elif" "else" "eval" "if" "then" "trap" "until" "while")

    (csh "else")

    (es "eval" "time" "true" "umask"
	"unwind-protect" "whatis")

    (ksh88 eval sh-append bourne
	   "time" "whence")

    (posix eval sh-append sh
	   "command")

    (rc "builtin" "else" "eval" "whatis")

    (sh eval sh-append bourne
	"type")

    (zsh eval sh-append ksh88
	 "builtin" "disable" "enable" "type" "unhash" "which"))
  "*List of keywords that may be immediately followed by a command(-name).
See `sh-feature'.")



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
	   font-lock-function-name-face)
	'("\\(^\\|[ \t]\\)\\(default\\):" 2
	  font-lock-keyword-face t))

    (dtksh eval identity wksh)

    (es eval sh-append executable-font-lock-keywords
	'("\\$#?\\([A-Za-z_][A-Za-z0-9_]*\\|[0-9]+\\)" 1
	  font-lock-function-name-face))

    (rc eval sh-append es
	'("\\(^\\|[ \t]\\)\\(else\\( if\\)?\\)\\>" 2
	  font-lock-keyword-face t))

    (sh eval sh-append shell
	'("\\$\\({#?\\)?\\([A-Za-z_][A-Za-z0-9_]*\\|[-#?@!]\\)" 2
	  font-lock-function-name-face)
	" in\\([ \t]\\|$\\)")

    ;; The next entry is only used for defining the others
    (shell eval sh-append executable-font-lock-keywords
	   '("\\\\." 0 font-lock-string-face)
	   '("\\${?\\([A-Za-z_][A-Za-z0-9_]*\\|[0-9]+\\|[$*_]\\)" 1
	     font-lock-function-name-face))

    (wksh eval sh-append ksh88
	  '("\\(^\\|[^-._A-Za-z0-9]\\)\\(Xt[A-Z][A-Za-z]*\\)\\($\\|[^-._A-Za-z0-9]\\)" 2 font-lock-keyword-face)))
  "*Rules for highlighting shell scripts.  See `sh-feature'.")


;; mode-command and utility functions

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
set `sh-shell-path' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sh-mode-map)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'sh-header-marker)
  (make-local-variable 'sh-shell-path)
  (make-local-variable 'sh-shell)
  (make-local-variable 'pair-alist)
  (make-local-variable 'pair-filter)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'comint-dynamic-complete-functions)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (make-local-variable 'skeleton-filter)
  (make-local-variable 'process-environment)
  (setq major-mode 'sh-mode
	mode-name "Shell-script"
	indent-line-function 'sh-indent-line
	;; not very clever, but enables wrapping skeletons around regions
	indent-region-function (lambda (b e)
				 (indent-rigidly b e sh-indentation))
	paragraph-start "^$\\|^"
	paragraph-separate paragraph-start
	comment-start "# "
	font-lock-keywords-case-fold-search nil
	comint-dynamic-complete-functions sh-dynamic-complete-functions
	;; we can't look if previous line ended with `\'
	comint-prompt-regexp "^[ \t]*"
	pair-alist '((?` _ ?`))
	pair-filter 'sh-quoted-p
	skeleton-further-elements '((< '(- (min sh-indentation
						(current-column)))))
	skeleton-filter 'sh-feature)
  ;; parse or insert magic number for exec() and set all variables depending
  ;; on the shell thus determined
  (goto-char (point-min))
  (sh-set-shell
   (if (looking-at "#![\t ]*\\([^\t\n ]+\\)")
       (buffer-substring (match-beginning 1) (match-end 1))
     sh-shell-path))
  (run-hooks 'sh-mode-hook))
;;;###autoload
(defalias 'shell-script-mode 'sh-mode)



(defun sh-set-shell (shell)
  "Set this buffer's shell to SHELL (a string).
Makes this script executable via `executable-set-magic'.
Calls the value of `sh-set-shell-hook' if set."
  (interactive (list (completing-read "Name or path of shell: " sh-shells)))
  (if (eq this-command 'sh-set-shell)
      ;; prevent querying
      (setq this-command 'executable-set-magic))
  (setq sh-shell (intern (file-name-nondirectory shell))
	sh-shell (or (cdr (assq sh-shell sh-alias-alist))
		     sh-shell)
	sh-shell-path (executable-set-magic shell (sh-feature sh-shell-arg))
	local-abbrev-table (sh-feature sh-abbrevs)
	require-final-newline (sh-feature sh-require-final-newline)
	font-lock-keywords
	  (sh-feature
	   sh-font-lock-keywords
	   (lambda (list)
	     (sh-append list
			(cons (concat (sh-feature sh-comment-prefix)
				      "\\(#.*\\)")
			      '(2 font-lock-comment-face t))
			;; first grouping should include all keywords such
			;; as while, do ... that may be followed by a
			;; command, but neither t nor keep will fontify it
			(cons (concat "\\(^\\|[|&;()]\\)[ \t]*\\(\\(\\("
				      (mapconcat 'identity
						 (sh-feature
						  sh-leading-keywords)
						 "\\|")
				      "\\)[ \t]+\\)?\\("
				      (mapconcat 'identity
						 (sh-feature sh-builtins)
						 "\\|")
				      "\\)\\)\\($\\|[ \t|&;()]\\)")
			      '(2 font-lock-keyword-face 'keep))
			(cons (sh-feature sh-assignment-regexp)
			      '(1 font-lock-function-name-face)))))
	comment-start-skip (concat (sh-feature sh-comment-prefix) "#+[\t ]*")
	mode-line-process (format "[%s]" sh-shell)
	process-environment (default-value 'process-environment)
	shell (sh-feature sh-variables))
  (set-syntax-table (sh-feature sh-mode-syntax-table))
  (save-excursion
    (while (search-forward "=" nil t)
      (sh-assignment 0)))
  (while shell
    (sh-remember-variable (car shell))
    (setq shell (cdr shell)))
  (and (boundp 'font-lock-mode)
       font-lock-mode
       (font-lock-mode (font-lock-mode 0)))
  (run-hooks 'sh-set-shell-hook))



(defun sh-feature (list &optional function)
  "Index ALIST by the current shell.
If ALIST isn't a list where every element is a cons, it is returned as is.
Else indexing follows an inheritance logic which works in two ways:

  - Fall back on successive ancestors (see `sh-ancestor-alist') as long as
    the alist contains no value for the current shell.

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



(defun sh-abbrevs (ancestor &rest list)
  "Iff it isn't, define the current shell as abbrev table and fill that.
Abbrev table will inherit all abbrevs from ANCESTOR, which is either an abbrev
table or a list of (NAME1 EXPANSION1 ...).  In addition it will define abbrevs
according to the remaining arguments NAMEi EXPANSIONi ...
EXPANSION may be either a string or a skeleton command."
  (or (if (boundp sh-shell)
	  (symbol-value sh-shell))
      (progn
	(if (listp ancestor)
	    (nconc list ancestor))
	(define-abbrev-table sh-shell ())
	(if (vectorp ancestor)
	    (mapatoms (lambda (atom)
			(or (eq atom 0)
			    (define-abbrev (symbol-value sh-shell)
			      (symbol-name atom)
			      (symbol-value atom)
			      (symbol-function atom))))
		      ancestor))
	(while list
	  (define-abbrev (symbol-value sh-shell)
	    (car list)
	    (if (stringp (car (cdr list)))
		(car (cdr list))
	      "")
	    (if (symbolp (car (cdr list)))
		(car (cdr list))))
	  (setq list (cdr (cdr list)))))
      (symbol-value sh-shell)))


(defun sh-mode-syntax-table (table &rest list)
  "Copy TABLE and set syntax for succesive CHARs according to strings S."
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
  "Indent as far as preceding non-empty line, then by steps of `sh-indentation'.
Lines containing only comments are considered empty."
  (interactive)
  (let ((previous (save-excursion
		    (while (progn
			     (line-move -1)
			     (back-to-indentation)
			     (or (eolp)
				 (eq (following-char) ?#))))
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
				     sh-shell-path)
	    (delete-region sh-header-marker end)))
      (shell-command-on-region start end (concat sh-shell-path " -")))))


(defun sh-remember-variable (var)
  "Make VARIABLE available for future completing reads in this buffer."
  (or (< (length var) sh-remember-variable-min)
      (getenv var)
      (setq process-environment (cons (concat var "=0") process-environment)))
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


(defun sh-add (var delta)
  "Insert an addition of VAR and prefix DELTA for Bourne (type) shell."
  (interactive
   (list (completing-read "Variable: "
			  (mapcar (lambda (var)
				    (substring var 0 (string-match "=" var)))
				  process-environment))
	 (prefix-numeric-value current-prefix-arg)))
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
      "if [ " str " ]; then" \n
      > _ \n
      ( "other condition, %s: "
	< "elif [ " str " ]; then" \n
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
(put 'sh-repeat 'menu-enable '(sh-feature sh-repeat))



(define-skeleton sh-select
  "Insert a select statement.  See `sh-feature'."
  (ksh88 "Index variable: "
	 "select " str " in " _ "; do" \n
	 > ?$ str \n
	 < "done"))
(put 'sh-select 'menu-enable '(sh-feature sh-select))



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
      "TMP=/tmp/" str ".$$" \n
      "trap \"rm $TMP* 2>/dev/null\" " ?0))



(define-skeleton sh-until
  "Insert an until loop.  See `sh-feature'."
  (sh "condition: "
      "until [ " str " ]; do" \n
      > _ \n
      < "done"))
(put 'sh-until 'menu-enable '(sh-feature sh-until))



(define-skeleton sh-while
  "Insert a while loop.  See `sh-feature'."
  (csh eval sh-modify sh
       1 "while( "
       3 " )"
       9 "end")
  (es eval sh-modify rc
      1 "while { "
      3 " } {")
  (rc eval sh-modify csh
      3 " ) {"
      9 ?})
  (sh "condition: "
      "while [ " str " ]; do" \n
      > _ \n
      < "done"))



(define-skeleton sh-while-getopts
  "Insert a while getopts loop.  See `sh-feature'.
Prompts for an options string which consists of letters for each recognized
option followed by a colon `:' if the option accepts an argument."
  (bash eval sh-modify sh
	18 "${0##*/}")
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
      "[ +-" '(setq v1 (point)) str
      '(save-excursion
	 (while (search-backward ":" v1 t)
	   (replace-match " arg][ +-" t t)))
      (if (eq (preceding-char) ?-) -5)
      "][ --] args\"" \n
      "exit 2" \n
      < < "esac" \n
      < "done" \n
      "shift " (sh-add "OPTIND" -1)))
(put 'sh-while-getopts 'menu-enable '(sh-feature sh-while-getopts))



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
	     (buffer-substring (match-beginning 1) (match-end 1)))))))



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



(defun sh-newline-and-indent (&optional arg)
  "Strip unquoted whitespace, insert newline, and indent like current line.
Unquoted whitespace is stripped from the current line's end, unless a
prefix ARG is given."
  (interactive "*P")
  (let ((previous (current-indentation)))
    (if arg ()
      (just-one-space)
      (backward-char)
      (if (sh-quoted-p)
	  (forward-char)
	(delete-char 1)))
    (newline)
    (indent-to previous)))



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

;; sh-script.el ends here
