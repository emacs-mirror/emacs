;;; sh-script.el --- shell-script editing commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1993-1997, 1999, 2001-2022 Free Software Foundation,
;; Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Old-Version: 2.0f
;; Maintainer: emacs-devel@gnu.org
;; Keywords: languages, unix

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

;; Major mode for editing shell scripts.  Bourne, C and rc shells as well
;; as various derivatives are supported and easily derived from.  Structured
;; statements can be inserted with one command or abbrev.  Completion is
;; available for filenames, variables known from the script, the shell and
;; the environment as well as commands.

;;; Known Bugs:

;; - In Bourne the keyword `in' is not anchored to case, for, select ...
;; - Variables in `"' strings aren't fontified because there's no way of
;;   syntactically distinguishing those from `'' strings.

;;		Indentation
;;	 	===========
;; Indentation for rc and es modes is very limited, but for Bourne shells
;; and its derivatives it is quite customizable.
;;
;; The following description applies to sh and derived shells (bash,
;; zsh, ...).
;;
;; There are various customization variables which allow tailoring to
;; a wide variety of styles.  Most of these variables are named
;; sh-indent-for-XXX and sh-indent-after-XXX.  For example.
;; sh-indent-after-if controls the indenting of a line following
;; an if statement, and sh-indent-for-fi controls the indentation
;; of the line containing the fi.
;;
;; You can set each to a numeric value, but it is often more convenient
;; to a symbol such as `+' which uses the value of variable `sh-basic-offset'.
;; By changing this one variable you can increase or decrease how much
;; indentation there is.  Valid symbols:
;;
;; 	+   Indent right by sh-basic-offset
;; 	-   Indent left  by sh-basic-offset
;; 	++  Indent right twice sh-basic-offset
;; 	--  Indent left  twice sh-basic-offset
;; 	*   Indent right half sh-basic-offset
;; 	/   Indent left  half sh-basic-offset.
;;
;; 	Saving indentation values
;; 	-------------------------
;; After you've learned the values in a buffer, how to you remember them?
;; There is a minimal way of being able to save indentation values and
;; to reload them in another buffer or at another point in time.
;;
;; Use `sh-name-style' to give a name to the indentation settings of
;; 	the current buffer.
;; Use `sh-load-style' to load indentation settings for the current
;; 	buffer from a specific style.
;; Use `sh-save-styles-to-buffer' to write all the styles to a buffer
;; 	in lisp code.  You can then store it in a file and later use
;; 	`load-file' to load it.
;;
;; 	Indentation variables - buffer local or global?
;; 	----------------------------------------------
;; I think that often having them buffer-local makes sense,
;; especially if one is using `smie-config-guess'.  However, if
;; a user sets values using customization, these changes won't appear
;; to work if the variables are already local!
;;
;; To get round this, there is a variable `sh-make-vars-local' and 2
;; functions: `sh-make-vars-local' and `sh-reset-indent-vars-to-global-values'.
;;
;; If `sh-make-vars-local' is non-nil, then these variables become
;; buffer local when the mode is established.
;; If this is nil, then the variables are global.  At any time you
;; can make them local with the command `sh-make-vars-local'.
;; Conversely, to update with the global values you can use the
;; command `sh-reset-indent-vars-to-global-values'.
;;
;; This may be awkward, but the intent is to cover all cases.
;;
;; 	Awkward things, pitfalls
;; 	------------------------
;; Indentation for a sh script is complicated for a number of reasons:
;;
;; 1. You can't format by simply looking at symbols, you need to look
;;    at keywords.  [This is not the case for rc and es shells.]
;; 2. The character ")" is used both as a matched pair "(" ... ")" and
;;    as a stand-alone symbol (in a case alternative).  This makes
;;    things quite tricky!
;; 3. Here-documents in a script should be treated "as is", and when
;;    they terminate we want to revert to the indentation of the line
;;    containing the "<<" symbol.
;; 4. A line may be continued using the "\".
;; 5. The character "#" (outside a string) normally starts a comment,
;;    but it doesn't in the sequence "$#"!
;;
;; To try and address points 2 3 and 5 I used a feature that cperl mode
;; uses, that of a text's syntax property.  This, however, has 2
;; disadvantages:
;; 1. We need to scan the buffer to find which ")" symbols belong to a
;;    case alternative, to find any here documents, and handle "$#".
;;
;; 	Bugs
;; 	----
;; - Indenting many lines is slow.  It currently does each line
;;   independently, rather than saving state information.
;;
;; - "echo $z in ps | head)" the last ) is mis-identified as being part of
;;   a case-pattern.  You need to put the "in" between quotes to coerce
;;   sh-script into doing the right thing.
;;
;; Richard Sharman <rsharman@pobox.com>  June 1999.

;;; Code:

;; page 1:	variables and settings
;; page 2:	indentation stuff
;; page 3:	mode-command and utility functions
;; page 4:	statement syntax-commands for various shells
;; page 5:	various other commands

(eval-when-compile
  (require 'skeleton)
  (require 'cl-lib)
  (require 'comint))
(require 'executable)

(autoload 'comint-completion-at-point "comint")
(autoload 'comint-filename-completion "comint")
(autoload 'comint-send-string "comint")
(autoload 'shell-command-completion "shell")
(autoload 'shell-environment-variable-completion "shell")

(defvar font-lock-comment-face)
(defvar font-lock-set-defaults)
(defvar font-lock-string-face)


(defgroup sh nil
  "Shell programming utilities."
  :group 'languages)

(defgroup sh-script nil
  "Shell script mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'sh
  :prefix "sh-")


(defcustom sh-ancestor-alist
  '((ash . sh)
    (bash . jsh)
    (bash2 . jsh)
    (dash . ash)
    (dtksh . ksh)
    (es . rc)
    (itcsh . tcsh)
    (jcsh . csh)
    (jsh . sh)
    (ksh . ksh88)
    (ksh88 . jsh)
    (oash . sh)
    (pdksh . ksh88)
    (mksh . pdksh)
    (posix . sh)
    (tcsh . csh)
    (wksh . ksh88)
    (wsh . sh)
    (zsh . ksh88)
    (rpm . sh))
  "Alist showing the direct ancestor of various shells.
This is the basis for `sh-feature'.  See also `sh-alias-alist'.
By default we have the following three hierarchies:

csh		C Shell
  jcsh		C Shell with Job Control
  tcsh		TENEX C Shell
    itcsh	Ian's TENEX C Shell
rc		Plan 9 Shell
  es		Extensible Shell
sh		Bourne Shell
  ash		Almquist Shell
    dash	Debian Almquist Shell
  jsh		Bourne Shell with Job Control
    bash	GNU Bourne Again Shell
    ksh88	Korn Shell '88
      ksh	Korn Shell '93
	dtksh	CDE Desktop Korn Shell
      pdksh	Public Domain Korn Shell
        mksh    MirOS BSD Korn Shell
      wksh	Window Korn Shell
      zsh	Z Shell
  oash		SCO OA (curses) Shell
  posix		IEEE 1003.2 Shell Standard
  wsh		? Shell"
  :type '(repeat (cons symbol symbol))
  :version "24.4"                       ; added dash
  :group 'sh-script)

(defcustom sh-alias-alist
  (append (if (eq system-type 'gnu/linux)
	     '((csh . tcsh)
	       (ksh . pdksh)))
	 ;; for the time being
	 '((ksh . ksh88)
           (bash2 . bash)
	   (sh5 . sh)
           ;; Android's system shell
           ("^/system/bin/sh$" . mksh)))
  "Alist for transforming shell names to what they really are.
Use this where the name of the executable doesn't correspond to
the type of shell it really is.  Keys are regular expressions
matched against the full path of the interpreter.  (For backward
compatibility, keys may also be symbols, which are matched
against the interpreter's basename.  The values are symbols
naming the shell."
  :type '(repeat (cons (radio
                        (regexp :tag "Regular expression")
                        (symbol :tag "Basename"))
                       (symbol :tag "Shell")))
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
  "The executable file name for the shell being programmed."
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
  "Single argument string for the magic number.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (const :tag "No Arguments" nil)
			       (string :tag "Arguments")
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)

(defcustom sh-imenu-generic-expression
  '((sh
     . ((nil
	 ;; function FOO
	 ;; function FOO()
         "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
         1)
	;; FOO()
	(nil
	 "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
	 1)
	)))
  "Alist of regular expressions for recognizing shell function definitions.
See `sh-feature' and `imenu-generic-expression'."
  :type '(alist :key-type (symbol :tag "Shell")
		:value-type (alist :key-type (choice :tag "Title"
						     string
						     (const :tag "None" nil))
				   :value-type
				   (repeat :tag "Regexp, index..." sexp)))
  :group 'sh-script
  :version "20.4")

(defun sh-current-defun-name ()
  "Find the name of function or variable at point.
For use in `add-log-current-defun-function'."
  (save-excursion
    (end-of-line)
    (when (re-search-backward
	   (concat "\\(?:"
		   ;; function FOO
		   ;; function FOO()
		   "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
		   "\\)\\|\\(?:"
		   ;; FOO()
		   "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
		   "\\)\\|\\(?:"
		   ;; FOO=
		   "^\\([[:alpha:]_][[:alnum:]_]*\\)="
		   "\\)")
	   nil t)
      (or (match-string-no-properties 1)
	  (match-string-no-properties 2)
	  (match-string-no-properties 3)))))

(defvar sh-shell-variables nil
  "Alist of shell variable names that should be included in completion.
These are used for completion in addition to all the variables named
in `process-environment'.  Each element looks like (VAR . VAR), where
the car and cdr are the same symbol.")

(defvar sh-shell-variables-initialized nil
  "Non-nil if `sh-shell-variables' is initialized.")

(defun sh-canonicalize-shell (shell)
  "Convert a shell name SHELL to the one we should handle it as.
SHELL is a full path to the shell interpreter; return a shell
name symbol."
  (cl-loop
     with shell = (cond ((string-match "\\.exe\\'" shell)
                         (substring shell 0 (match-beginning 0)))
                        (t shell))
     with shell-base = (intern (file-name-nondirectory shell))
     for (key . value) in sh-alias-alist
     if (and (stringp key) (string-match key shell)) return value
     if (eq key shell-base) return value
     finally return shell-base))

(defvar sh-shell (sh-canonicalize-shell sh-shell-file)
  "The shell being programmed.  This is set by \\[sh-set-shell].")
;;;###autoload(put 'sh-shell 'safe-local-variable 'symbolp)

(define-abbrev-table 'sh-mode-abbrev-table ())


(defun sh-mode-syntax-table (table &rest list)
  "Copy TABLE and set syntax for successive CHARs according to strings S."
  (setq table (copy-syntax-table table))
  (while list
    (modify-syntax-entry (pop list) (pop list) table))
  table)

(defvar sh-mode-syntax-table
  (sh-mode-syntax-table ()
	?\# "<"
	?\n ">#"
	?\" "\"\""
	?\' "\"'"
	?\` "\"`"
	;; ?$ might also have a ". p" syntax. Both "'" and ". p" seem
	;; to work fine. This is needed so that dabbrev-expand
	;; $VARNAME works.
	?$ "'"
	?! "_"
	?% "_"
	?: "_"
	?. "_"
	?^ "_"
	?~ "_"
	?, "_"
	?= "."
        ?/ "."
	?\; "."
	?| "."
	?& "."
	?< "."
	?> ".")
  "The syntax table to use for Shell-Script mode.
This is buffer-local in every such buffer.")

(defvar sh-mode-syntax-table-input
  `((sh . nil)
    ;; Treat ' as punctuation rather than a string delimiter, as RPM
    ;; files often contain prose with apostrophes.
    (rpm . (,sh-mode-syntax-table ?\' ".")))
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
    (define-key map "\C-c?" #'smie-config-show-indent)
    (define-key map "\C-c=" #'smie-config-set-indent)
    (define-key map "\C-c<" #'smie-config-set-indent)
    (define-key map "\C-c>" #'smie-config-guess)
    (define-key map "\C-c\C-\\" 'sh-backslash-region)

    (define-key map "\C-c+" 'sh-add)
    (define-key map "\C-\M-x" 'sh-execute-region)
    (define-key map "\C-c\C-x" 'executable-interpret)
    (define-key map "\C-c\C-n" 'sh-send-line-or-region-and-step)
    (define-key map "\C-c\C-d" 'sh-cd-here)
    (define-key map "\C-c\C-z" 'sh-show-shell)

    (define-key map [remap delete-backward-char]
      'backward-delete-char-untabify)
    (define-key map "\C-c:" 'sh-set-shell)
    (define-key map [remap backward-sentence] 'sh-beginning-of-command)
    (define-key map [remap forward-sentence] 'sh-end-of-command)
    map)
  "Keymap used in Shell-Script mode.")

(easy-menu-define sh-mode-menu sh-mode-map
  "Menu for Shell-Script mode."
  '("Sh-Script"
    ["Backslash region" sh-backslash-region
     :help "Insert, align, or delete end-of-line backslashes on the lines in the region."]
    ["Set shell type..." sh-set-shell
     :help "Set this buffer's shell to SHELL (a string)"]
    ["Execute script..." executable-interpret
     :help "Run script with user-specified args, and collect output in a buffer"]
    ["Execute region" sh-execute-region
     :help "Pass optional header and region to a subshell for noninteractive execution"]
    "---"
    ;; Insert
    ["Case Statement" sh-case
     :help "Insert a case/switch statement"]
    ["For Loop" sh-for
     :help "Insert a for loop"]
    ["If Statement" sh-if
     :help "Insert an if statement"]
    ["Select Statement" sh-select
     :help "Insert a select statement "]
    ["Indexed Loop" sh-indexed-loop
     :help "Insert an indexed loop from 1 to n."]
    ["Options Loop" sh-while-getopts
     :help "Insert a while getopts loop."]
    ["While Loop" sh-while
     :help "Insert a while loop"]
    ["Repeat Loop" sh-repeat
     :help "Insert a repeat loop definition"]
    ["Until Loop" sh-until
     :help "Insert an until loop"]
    ["Addition..." sh-add
     :help "Insert an addition of VAR and prefix DELTA for Bourne (type) shell"]
    ["Function..." sh-function
     :help "Insert a function definition"]
    "---"
    ;; Other
    ["Insert braces and quotes in pairs" electric-pair-mode
     :style toggle
     :selected (bound-and-true-p electric-pair-mode)
     :help "Inserting a brace or quote automatically inserts the matching pair"]
    ["Set indentation" smie-config-set-indent
     :help "Set the indentation for the current line"]
    ["Show indentation" smie-config-show-indent
     :help "Show the how the current line would be indented"]
    ["Learn buffer indentation" smie-config-guess
     :help "Learn how to indent the buffer the way it currently is."]))

(defvar sh-skeleton-pair-default-alist '((?\( _ ?\)) (?\))
				      (?\[ ?\s _ ?\s ?\]) (?\])
				      (?{ _ ?}) (?\}))
  "Value to use for `skeleton-pair-default-alist' in Shell-Script mode.")

(defcustom sh-dynamic-complete-functions
  '(shell-environment-variable-completion
    shell-command-completion
    comint-filename-completion)
  "Functions for doing TAB dynamic completion."
  :type '(repeat function)
  :group 'sh-script)

(defcustom sh-assignment-regexp
  ;; The "\\[.+\\]" matches the "[index]" in "arrayvar[index]=value".
  `((csh . "\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?[ \t]*[-+*/%^]?=")
    ;; actually spaces are only supported in let/(( ... ))
    (ksh88 . ,(concat "\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?"
                      "[ \t]*\\(?:[-+*/%&|~^]\\|<<\\|>>\\)?="))
    (bash . "\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?\\+?=")
    (rc . "\\<\\([[:alnum:]_*]+\\)[ \t]*=")
    (sh . "\\<\\([[:alnum:]_]+\\)="))
  "Regexp for the variable name and what may follow in an assignment.
First grouping matches the variable name.  This is upto and including the `='
sign.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice regexp
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)

(define-obsolete-variable-alias 'sh-indentation 'sh-basic-offset "26.1")
(put 'sh-indentation 'safe-local-variable 'integerp)

(defcustom sh-remember-variable-min 3
  "Don't remember variables less than this length for completing reads."
  :type 'integer
  :group 'sh-script)


(defvar-local sh-header-marker nil
  "When non-nil is the end of header for prepending by \\[sh-execute-region].
That command is also used for setting this variable.")

(defcustom sh-beginning-of-command
  "\\([;({`|&]\\|\\`\\|[^\\]\n\\)[ \t]*\\([/~[:alnum:]:]\\)"
  "Regexp to determine the beginning of a shell command.
The actual command starts at the beginning of the second \\(grouping\\)."
  :type 'regexp
  :group 'sh-script)


(defcustom sh-end-of-command
  "\\([/~[:alnum:]:]\\)[ \t]*\\([;#)}`|&]\\|$\\)"
  "Regexp to determine the end of a shell command.
The actual command ends at the end of the first \\(grouping\\)."
  :type 'regexp
  :group 'sh-script)



(defcustom sh-here-document-word "EOF"
  "Word to delimit here documents.
If the first character of this string is \"-\", this is taken as
part of the redirection operator, rather than part of the
word (that is, \"<<-\" instead of \"<<\").  This is a feature
used by some shells (for example Bash) to indicate that leading
tabs inside the here document should be ignored.  In this case,
Emacs indents the initial body and end of the here document with
tabs, to the same level as the start (note that apart from this
there is no support for indentation of here documents).  This
will only work correctly if `sh-basic-offset' is a multiple of
`tab-width'.

Any quote characters or leading whitespace in the word are
removed when closing the here document."
  :type 'string
  :group 'sh-script)


(defvar sh-test
  '((sh "[  ]" . 3)
    (ksh88 "[[  ]]" . 4))
  "Initial input in Bourne if, while and until skeletons.  See `sh-feature'.")


;; customized this out of sheer bravado.  not for the faint of heart.
;; but it *did* have an asterisk in the docstring!
(defcustom sh-builtins
  '((bash sh-append posix
	  "." "alias" "bg" "bind" "builtin" "caller" "compgen" "complete"
          "declare" "dirs" "disown" "enable" "fc" "fg" "help" "history"
          "jobs" "kill" "let" "local" "popd" "printf" "pushd" "shopt"
          "source" "suspend" "typeset" "unalias"
          ;; bash4
          "mapfile" "readarray" "coproc")

    ;; The next entry is only used for defining the others
    (bourne sh-append shell
	    "eval" "export" "getopts" "newgrp" "pwd" "read" "readonly"
	    "times" "ulimit")

    (csh sh-append shell
	 "alias" "chdir" "glob" "history" "limit" "nice" "nohup" "rehash"
	 "setenv" "source" "time" "unalias" "unhash")

    (dtksh sh-append wksh)

    (es "access" "apids" "cd" "echo" "eval" "false" "let" "limit" "local"
	"newpgrp" "result" "time" "umask" "var" "vars" "wait" "whatis")

    (jsh sh-append sh
	 "bg" "fg" "jobs" "kill" "stop" "suspend")

    (jcsh sh-append csh
	  "bg" "fg" "jobs" "kill" "notify" "stop" "suspend")

    (ksh88 sh-append bourne
	   "alias" "bg" "false" "fc" "fg" "jobs" "kill" "let" "print" "time"
	   "typeset" "unalias" "whence")

    (oash sh-append sh
	  "checkwin" "dateline" "error" "form" "menu" "newwin" "oadeinit"
	  "oaed" "oahelp" "oainit" "pp" "ppfile" "scan" "scrollok" "wattr"
	  "wclear" "werase" "win" "wmclose" "wmmessage" "wmopen" "wmove"
	  "wmtitle" "wrefresh")

    (pdksh sh-append ksh88
	   "bind")

    (posix sh-append sh
	   "command")

    (rc "builtin" "cd" "echo" "eval" "limit" "newpgrp" "shift" "umask" "wait"
	"whatis")

    (sh sh-append bourne
	"hash" "test" "type")

    ;; The next entry is only used for defining the others
    (shell "cd" "echo" "eval" "set" "shift" "umask" "unset" "wait")

    (wksh sh-append ksh88)

    (zsh sh-append ksh88
	 "autoload" "bindkey" "builtin" "chdir" "compctl" "declare" "dirs"
	 "disable" "disown" "echotc" "enable" "functions" "getln" "hash"
	 "history" "integer" "limit" "local" "log" "popd" "pushd" "r"
	 "readonly" "rehash" "sched" "setopt" "source" "suspend" "true"
	 "ttyctl" "type" "unfunction" "unhash" "unlimit" "unsetopt" "vared"
	 "which"))
  "List of all shell builtins for completing read and fontification.
Note that on some systems not all builtins are available or some are
implemented as aliases.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (sexp :format "Evaluate: %v"))))
  :version "24.4"                       ; bash4 additions
  :group 'sh-script)



(defcustom sh-leading-keywords
  '((bash sh-append sh
          "time")

    (csh "else")

    (es "true" "unwind-protect" "whatis")

    (rc "else")

    (sh "!" "do" "elif" "else" "if" "then" "trap" "type" "until" "while"))
  "List of keywords that may be immediately followed by a builtin or keyword.
Given some confusion between keywords and builtins depending on shell and
system, the distinction here has been based on whether they influence the
flow of control or syntax.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)


(defcustom sh-other-keywords
  '((bash sh-append bourne
	  "bye" "logout" "select")

    ;; The next entry is only used for defining the others
    (bourne sh-append sh
	    "function")

    (csh sh-append shell
	 "breaksw" "default" "end" "endif" "endsw" "foreach" "goto"
	 "if" "logout" "onintr" "repeat" "switch" "then" "while")

    (es "break" "catch" "exec" "exit" "fn" "for" "forever" "fork" "if"
	"return" "throw" "while")

    (ksh88 sh-append bourne
	   "select")

    (rc "break" "case" "exec" "exit" "fn" "for" "if" "in" "return" "switch"
	"while")

    (sh sh-append shell
	"done" "esac" "fi" "for" "in" "return")

    ;; The next entry is only used for defining the others
    (shell "break" "case" "continue" "exec" "exit")

    (zsh sh-append bash
	 "select" "foreach"))
  "List of keywords not in `sh-leading-keywords'.
See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)



(defvar sh-variables
  '((bash sh-append sh
	  "allow_null_glob_expansion" "auto_resume" "BASH" "BASH_ENV"
	  "BASH_VERSINFO" "BASH_VERSION" "cdable_vars" "COMP_CWORD"
	  "COMP_LINE" "COMP_POINT" "COMP_WORDS" "COMPREPLY" "DIRSTACK"
	  "ENV" "EUID" "FCEDIT" "FIGNORE" "FUNCNAME"
	  "glob_dot_filenames" "GLOBIGNORE" "GROUPS" "histchars"
	  "HISTCMD" "HISTCONTROL" "HISTFILE" "HISTFILESIZE"
	  "HISTIGNORE" "history_control" "HISTSIZE"
	  "hostname_completion_file" "HOSTFILE" "HOSTTYPE" "IGNOREEOF"
	  "ignoreeof" "INPUTRC" "LINENO" "MACHTYPE" "MAIL_WARNING"
	  "noclobber" "nolinks" "notify" "no_exit_on_failed_exec"
	  "NO_PROMPT_VARS" "OLDPWD" "OPTERR" "OSTYPE" "PIPESTATUS"
	  "PPID" "POSIXLY_CORRECT" "PROMPT_COMMAND" "PS3" "PS4"
	  "pushd_silent" "PWD" "RANDOM" "REPLY" "SECONDS" "SHELLOPTS"
	  "SHLVL" "TIMEFORMAT" "TMOUT" "UID")

    (csh sh-append shell
	 "argv" "cdpath" "child" "echo" "histchars" "history" "home"
	 "ignoreeof" "mail" "noclobber" "noglob" "nonomatch" "path" "prompt"
	 "shell" "status" "time" "verbose")

    (es sh-append shell
	"apid" "cdpath" "CDPATH" "history" "home" "ifs" "noexport" "path"
	"pid" "prompt" "signals")

    (jcsh sh-append csh
	  "notify")

    (ksh88 sh-append sh
	   "ENV" "ERRNO" "FCEDIT" "FPATH" "HISTFILE" "HISTSIZE" "LINENO"
	   "OLDPWD" "PPID" "PS3" "PS4" "PWD" "RANDOM" "REPLY" "SECONDS"
	   "TMOUT")

    (oash sh-append sh
	  "FIELD" "FIELD_MAX" "LAST_KEY" "OALIB" "PP_ITEM" "PP_NUM")

    (rc sh-append shell
	"apid" "apids" "cdpath" "CDPATH" "history" "home" "ifs" "path" "pid"
	"prompt" "status")

    (sh sh-append shell
	"CDPATH" "IFS" "OPTARG" "OPTIND" "PS1" "PS2")

    ;; The next entry is only used for defining the others
    (shell "COLUMNS" "EDITOR" "HOME" "HUSHLOGIN" "LANG" "LC_COLLATE"
	   "LC_CTYPE" "LC_MESSAGES" "LC_MONETARY" "LC_NUMERIC" "LC_TIME"
	   "LINES" "LOGNAME" "MAIL" "MAILCHECK" "MAILPATH" "PAGER" "PATH"
	   "SHELL" "TERM" "TERMCAP" "TERMINFO" "VISUAL")

    (tcsh sh-append csh
	  "addsuffix" "ampm" "autocorrect" "autoexpand" "autolist"
	  "autologout" "chase_symlinks" "correct" "dextract" "edit" "el"
	  "fignore" "gid" "histlit" "HOST" "HOSTTYPE" "HPATH"
	  "ignore_symlinks" "listjobs" "listlinks" "listmax" "matchbeep"
	  "nobeep" "NOREBIND" "oid" "printexitvalue" "prompt2" "prompt3"
	  "pushdsilent" "pushdtohome" "recexact" "recognize_only_executables"
	  "rmstar" "savehist" "SHLVL" "showdots" "sl" "SYSTYPE" "tcsh" "term"
	  "tperiod" "tty" "uid" "version" "visiblebell" "watch" "who"
	  "wordchars")

    (zsh sh-append ksh88
	 "BAUD" "bindcmds" "cdpath" "DIRSTACKSIZE" "fignore" "FIGNORE" "fpath"
	 "HISTCHARS" "hostcmds" "hosts" "HOSTS" "LISTMAX" "LITHISTSIZE"
	 "LOGCHECK" "mailpath" "manpath" "NULLCMD" "optcmds" "path" "POSTEDIT"
	 "prompt" "PROMPT" "PROMPT2" "PROMPT3" "PROMPT4" "psvar" "PSVAR"
	 "READNULLCMD" "REPORTTIME" "RPROMPT" "RPS1" "SAVEHIST" "SPROMPT"
	 "STTY" "TIMEFMT" "TMOUT" "TMPPREFIX" "varcmds" "watch" "WATCH"
	 "WATCHFMT" "WORDCHARS" "ZDOTDIR"))
  "List of all shell variables available for completing read.
See `sh-feature'.")


;; Font-Lock support

(defface sh-heredoc
  '((((min-colors 88) (class color)
      (background dark))
     (:foreground "yellow1" :weight bold))
    (((class color)
      (background dark))
     (:foreground "yellow" :weight bold))
    (((class color)
      (background light))
     (:foreground "tan1" ))
    (t
     (:weight bold)))
  "Face to show a here-document."
  :group 'sh-indentation)

;; These colors are probably icky.  It's just a placeholder though.
(defface sh-quoted-exec
  '((((class color) (background dark))
     (:foreground "salmon"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t
     (:weight bold)))
  "Face to show quoted execs like \\=`blabla\\=`."
  :group 'sh-indentation)

(defface sh-escaped-newline '((t :inherit font-lock-string-face))
  "Face used for (non-escaped) backslash at end of a line in Shell-script mode."
  :group 'sh-script
  :version "22.1")

(defvar sh-font-lock-keywords-var
  '((csh sh-append shell
	 ("\\${?[#?]?\\([[:alpha:]_][[:alnum:]_]*\\|0\\)" 1
          font-lock-variable-name-face))

    (es sh-append executable-font-lock-keywords
	("\\$#?\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\)" 1
         font-lock-variable-name-face))

    (rc sh-append es)
    (bash sh-append sh ("\\$(\\([^)\n]+\\)" (1 'sh-quoted-exec t) ))
    (sh sh-append shell
	;; Variable names.
	("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)" 2
	  font-lock-variable-name-face)
	;; Function names.
	("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
	("\\<\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
	  (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
	("\\(?:^\\s *\\|[[();&|]\\s *\\|\\(?:\\s +-[ao]\\|if\\|else\\|then\\|while\\|do\\)\\s +\\)\\(!\\)"
	 1 font-lock-negation-char-face))

    ;; The next entry is only used for defining the others
    (shell
           ;; Using font-lock-string-face here confuses sh-get-indent-info.
           ("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" 3 'sh-escaped-newline)
	   ("\\\\[^[:alnum:]]" 0 font-lock-string-face)
	   ("\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\|[$*_]\\)" 1
	     font-lock-variable-name-face))
    (rpm sh-append rpm2
	 ("%{?\\(\\sw+\\)"  1 font-lock-keyword-face))
    (rpm2 sh-append shell
	  ("^Summary:\\(.*\\)$" (1 font-lock-doc-face t))
	  ("^\\(\\sw+\\):"  1 font-lock-variable-name-face)))
  "Default expressions to highlight in Shell Script modes.  See `sh-feature'.")

(defvar sh-font-lock-keywords-var-1
  '((sh "[ \t]in\\>"))
  "Subdued level highlighting for Shell Script modes.")

(defvar sh-font-lock-keywords-var-2 ()
  "Gaudy level highlighting for Shell Script modes.")

;; These are used for the syntax table stuff (derived from cperl-mode).
;; Note: parse-sexp-lookup-properties must be set to t for it to work.
(defconst sh-st-punc (string-to-syntax "."))
(defconst sh-here-doc-syntax (string-to-syntax "|")) ;; generic string

(eval-and-compile
  (defconst sh-escaped-line-re
    ;; Should match until the real end-of-continued-line, but if that is not
    ;; possible (because we bump into EOB or the search bound), then we should
    ;; match until the search bound.
    "\\(?:\\(?:.*[^\\\n]\\)?\\(?:\\\\\\\\\\)*\\\\\n\\)*.*")

  (defconst sh-here-doc-open-re
    (concat "[^<]<<-?\\s-*\\\\?\\(\\(?:['\"][^'\"]+['\"]\\|\\sw\\|[-/~._]\\)+\\)"
            sh-escaped-line-re "\\(\n\\)")))

(defun sh--inside-noncommand-expression (pos)
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (when (nth 1 ppss)
        (goto-char (nth 1 ppss))
        (or
         (pcase (char-after)
           ;; ((...)) or $((...)) or $[...] or ${...}. Nested
           ;; parenthesis can occur inside the first of these forms, so
           ;; parse backward recursively.
           (?\( (eq ?\( (char-before)))
           ((or ?\{ ?\[) (eq ?\$ (char-before))))
         (sh--inside-noncommand-expression (1- (point))))))))

(defun sh-font-lock-open-heredoc (start string eol)
  "Determine the syntax of the \\n after a <<EOF.
START is the position of <<.
STRING is the actual word used as delimiter (e.g. \"EOF\").
INDENTED is non-nil if the here document's content (and the EOF mark) can
be indented (i.e. a <<- was used rather than just <<).
Point is at the beginning of the next line."
  (unless (or (memq (char-before start) '(?< ?>))
	      (sh-in-comment-or-string start)
              (sh--inside-noncommand-expression start))
    ;; We're looking at <<STRING, so we add "^STRING$" to the syntactic
    ;; font-lock keywords to detect the end of this here document.
    (let ((str (replace-regexp-in-string "['\"]" "" string))
          (ppss (save-excursion (syntax-ppss eol))))
      (if (nth 4 ppss)
          ;; The \n not only starts the heredoc but also closes a comment.
          ;; Let's close the comment just before the \n.
          (put-text-property (1- eol) eol 'syntax-table '(12))) ;">"
      (if (or (nth 5 ppss) (> (count-lines start eol) 1))
          ;; If the sh-escaped-line-re part of sh-here-doc-open-re has matched
          ;; several lines, make sure we refontify them together.
          ;; Furthermore, if (nth 5 ppss) is non-nil (i.e. the \n is
          ;; escaped), it means the right \n is actually further down.
          ;; Don't bother fixing it now, but place a multiline property so
          ;; that when jit-lock-context-* refontifies the rest of the
          ;; buffer, it also refontifies the current line with it.
          (put-text-property start (1+ eol) 'syntax-multiline t))
      (put-text-property eol (1+ eol) 'sh-here-doc-marker str)
      (prog1 sh-here-doc-syntax
        (goto-char (+ 2 start))))))

(defun sh-syntax-propertize-here-doc (end)
  (let ((ppss (syntax-ppss)))
    (when (eq t (nth 3 ppss))
      (let ((key (get-text-property (nth 8 ppss) 'sh-here-doc-marker))
            (case-fold-search nil))
        (when (re-search-forward
               (concat "^\\([ \t]*\\)" (regexp-quote key) "\\(\n\\)")
               end 'move)
          (let ((eol (match-beginning 2)))
            (put-text-property eol (1+ eol)
                               'syntax-table sh-here-doc-syntax)))))))

(defun sh-font-lock-quoted-subshell (limit)
  "Search for a subshell embedded in a string.
Find all the unescaped \" characters within said subshell, remembering that
subshells can nest."
  (when (eq ?\" (nth 3 (syntax-ppss))) ; Check we matched an opening quote.
    ;; bingo we have a $( or a ` inside a ""
    (let (;; `state' can be: double-quote, backquote, code.
          (state (if (eq (char-before) ?`) 'backquote 'code))
          (startpos (point))
          ;; Stacked states in the context.
          (states '(double-quote)))
      (while (and state (progn (skip-chars-forward "^'\\\\\"`$()" limit)
                               (< (point) limit)))
        ;; unescape " inside a $( ... ) construct.
        (pcase (char-after)
          (?\' (pcase state
                 ('double-quote nil)
                 (_ (forward-char 1)
                    ;; FIXME: mark skipped double quotes as punctuation syntax.
                    (let ((spos (point)))
                      (skip-chars-forward "^'" limit)
                      (save-excursion
                        (let ((epos (point)))
                          (goto-char spos)
                          (while (search-forward "\"" epos t)
                            (put-text-property (point) (1- (point))
                                            'syntax-table '(1)))))))))
          (?\\ (forward-char 1))
          (?\" (pcase state
                 ('double-quote (setq state (pop states)))
                 (_ (push state states) (setq state 'double-quote)))
               (if state (put-text-property (point) (1+ (point))
                                            'syntax-table '(1))))
          (?\` (pcase state
                 ('backquote (setq state (pop states)))
                 (_ (push state states) (setq state 'backquote))))
          (?\$ (if (not (eq (char-after (1+ (point))) ?\())
                   nil
                 (forward-char 1)
                 (pcase state
                   (_ (push state states) (setq state 'code)))))
          (?\( (pcase state
                 ('double-quote nil)
                 (_ (push state states) (setq state 'code))))
          (?\) (pcase state
                 ('double-quote nil)
                 (_ (setq state (pop states)))))
          (_ (error "Internal error in sh-font-lock-quoted-subshell")))
        (forward-char 1))
      (when (< startpos (line-beginning-position))
        (put-text-property startpos (point) 'syntax-multiline t)
        (add-hook 'syntax-propertize-extend-region-functions
                  #'syntax-propertize-multiline nil t))
      )))


(defun sh-is-quoted-p (pos)
  (and (eq (char-before pos) ?\\)
       (not (sh-is-quoted-p (1- pos)))))

(defun sh-font-lock-paren (start)
  (unless (nth 8 (syntax-ppss))
    (save-excursion
      (let ((open nil))
        (goto-char start)
        ;; Skip through all patterns
        (while
            (progn
              (while
                  (progn
                    (forward-comment (- (point-max)))
                    (when (and (eolp) (sh-is-quoted-p (point)))
                      (forward-char -1)
                      t)))
              ;; Skip through one pattern
              (while
                  (or (/= 0 (skip-syntax-backward "w_"))
                      (/= 0 (skip-chars-backward "-$=?[]*@/\\\\"))
                      (and (sh-is-quoted-p (1- (point)))
                           (goto-char (- (point) 2)))
                      (when (memq (char-before) '(?\" ?\' ?\}))
                        (condition-case nil (progn (backward-sexp 1) t)
                          (error nil)))))
              ;; Patterns can be preceded by an open-paren (bug#1320).
              (when (eq (char-before (point)) ?\()
                (backward-char 1)
                (setq open (point)))
              (while (progn
                       (forward-comment (- (point-max)))
                       ;; Maybe we've bumped into an escaped newline.
                       (sh-is-quoted-p (point)))
                (backward-char 1))
              (when (eq (char-before) ?|)
                (backward-char 1) t)))
        (and (> (point) (1+ (point-min)))
             (progn (backward-char 2)
                    (if (> start (line-end-position))
                        (put-text-property (point) (1+ start)
                                           'syntax-multiline t))
                    ;; FIXME: The `in' may just be a random argument to
                    ;; a normal command rather than the real `in' keyword.
                    ;; I.e. we should look back to try and find the
                    ;; corresponding `case'.
                    (and (looking-at ";[;&]\\|\\_<in")
                         ;; ";; esac )" is a case that looks
                         ;; like a case-pattern but it's really just a close
                         ;; paren after a case statement.  I.e. if we skipped
                         ;; over `esac' just now, we're not looking
                         ;; at a case-pattern.
                         (not (looking-at "..[ \t\n]+esac[^[:word:]_]"))))
             (progn
               (when open
                 (put-text-property open (1+ open) 'syntax-table sh-st-punc))
               sh-st-punc))))))

(defun sh-font-lock-backslash-quote ()
  (if (eq (save-excursion (nth 3 (syntax-ppss (match-beginning 0)))) ?\')
      ;; In a '...' the backslash is not escaping.
      sh-st-punc
    nil))

(defun sh-syntax-propertize-function (start end)
  (goto-char start)
  (sh-syntax-propertize-here-doc end)
  (funcall
   (syntax-propertize-rules
    (sh-here-doc-open-re
     (2 (sh-font-lock-open-heredoc
         (1+ (match-beginning 0)) (match-string 1) (match-beginning 2))))
    ("\\s|" (0 (prog1 nil (sh-syntax-propertize-here-doc end))))
    ;; A `#' begins a comment when it is unquoted and at the
    ;; beginning of a word.  In the shell, words are separated by
    ;; metacharacters.  The list of special chars is taken from
    ;; the single-unix spec of the shell command language (under
    ;; `quoting') but with `$' removed.  Also -- if there's something like
    ;; \ #foo, then that's not a comment, unless the backslash itself
    ;; is backslashed.
    ("\\(?:[^|&;<>(`\\\"' \t\n]\\|\\${\\|\\(?:[^\\]\\|^\\)\\\\\\(?:\\\\\\\\\\)*.\\)\\(#+\\)" (1 "_"))
    ;; In addition, `#' at the beginning of closed parentheses
    ;; does not start a comment if the parentheses are not isolated
    ;; by metacharacters, excluding [()].
    ;; (e.g. `foo(#q/)' and `(#b)foo' in zsh)
    ("[^|&;<>(`\\\"' \t\n](\\(#+\\)" (1 "_"))
    ("(\\(#\\)[^)]+?)[^|&;<>)`\\\"' \t\n]" (1 "_"))
    ;; In a '...' the backslash is not escaping.
    ("\\(\\\\\\)'" (1 (sh-font-lock-backslash-quote)))
    ;; Make sure $@ and $? are correctly recognized as sexps.
    ("\\$\\([?@]\\)" (1 "_"))
    ;; Distinguish the special close-paren in `case'.
    (")" (0 (sh-font-lock-paren (match-beginning 0))))
    ;; Highlight (possibly nested) subshells inside "" quoted
    ;; regions correctly.
    ("\"\\(?:[^\\\"]\\|\\\\.\\)*?\\(\\$(\\|`\\)"
     (1 (ignore
         (if (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
             (goto-char (1+ (match-beginning 0)))
           ;; Save excursion because we want to also apply other
           ;; syntax-propertize rules within the affected region.
           (save-excursion
             (sh-font-lock-quoted-subshell end)))))))
   (point) end))
(defun sh-font-lock-syntactic-face-function (state)
  (let ((q (nth 3 state)))
    (if q
        (if (characterp q)
            (if (eq q ?\`) 'sh-quoted-exec font-lock-string-face)
          'sh-heredoc)
      font-lock-comment-face)))

(defgroup sh-indentation nil
  "Variables controlling indentation in shell scripts.

Note: customizing these variables will not affect existing buffers if
`sh-make-vars-local' is non-nil.  See the documentation for
variable `sh-make-vars-local', command `sh-make-vars-local'
and command `sh-reset-indent-vars-to-global-values'."
  :group 'sh-script)


(defcustom sh-set-shell-hook nil
  "Hook run by `sh-set-shell'."
  :type 'hook
  :group 'sh-script)

(defcustom sh-mode-hook '(sh-electric-here-document-mode)
  "Hook run by `sh-mode'."
  :type 'hook
  :options '(sh-electric-here-document-mode)
  :group 'sh-script)

(defcustom sh-popup-occur-buffer nil
  "Controls when  `smie-config-guess' pops the `*indent*' buffer.
If t it is always shown.  If nil, it is shown only when there
are conflicts."
  :type '(choice
	  (const :tag "Only when there are conflicts." nil)
	  (const :tag "Always"  t))
  :group 'sh-indentation)

(defcustom sh-first-lines-indent 0
  "The indentation of the first non-blank non-comment line.
Usually 0 meaning first column.
Can be set to a number, or to nil which means leave it as is."
  :type '(choice
	  (const :tag "Leave as is"	nil)
	  (integer :tag "Column number"
		   :menu-tag "Indent to this col (0 means first col)" ))
  :group 'sh-indentation)


(defcustom sh-basic-offset 4
  "The default indentation increment.
This value is used for the `+' and `-' symbols in an indentation variable."
  :type 'integer
  :group 'sh-indentation)
(put 'sh-basic-offset 'safe-local-variable 'integerp)

(defcustom sh-indent-comment t
  "How a comment line is to be indented.
nil means leave it as it is;
t  means indent it as a normal line, aligning it to previous non-blank
   non-comment line;
a number means align to that column, e.g. 0 means first column."
  :type '(choice
	  (const :tag "Leave as is." nil)
	  (const :tag "Indent as a normal line."  t)
	  (integer :menu-tag "Indent to this col (0 means first col)."
		   :tag "Indent to column number.") )
  :version "24.3"
  :group 'sh-indentation)


(defvar sh-debug nil
  "Enable lots of debug messages - if function `sh-debug' is enabled.")


;; Uncomment this defun and comment the defmacro for debugging.
;; (defun sh-debug (&rest args)
;;   "For debugging:  display message ARGS if variable SH-DEBUG is non-nil."
;;   (if sh-debug
;;       (apply 'message args)))
(defmacro sh-debug (&rest _args))

(defconst sh-symbol-list
  '((const :tag "+ "  :value +
	   :menu-tag "+   Indent right by sh-basic-offset")
    (const :tag "- "  :value -
	   :menu-tag "-   Indent left  by sh-basic-offset")
    (const :tag "++"  :value  ++
	   :menu-tag "++  Indent right twice sh-basic-offset")
    (const :tag "--"  :value --
	   :menu-tag "--  Indent left  twice sh-basic-offset")
    (const :tag "* " :value *
	   :menu-tag "*   Indent right half sh-basic-offset")
    (const :tag "/ " :value /
	   :menu-tag "/   Indent left  half sh-basic-offset")))

(defcustom sh-indent-for-else 0
  "How much to indent an `else' relative to its `if'.  Usually 0."
  :type `(choice
	  (integer :menu-tag "A number (positive=>indent right)"
		   :tag "A number")
	  (const :tag "--") ;; separator!
	  ,@ sh-symbol-list
	  )
  :group 'sh-indentation)

(defconst sh-number-or-symbol-list
  (append '((integer :menu-tag "A number (positive=>indent right)"
		     :tag "A number")
	    (const :tag "--"))		; separator
	  sh-symbol-list))

(defcustom sh-indent-for-fi 0
  "How much to indent a `fi' relative to its `if'.  Usually 0."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-for-done 0
  "How much to indent a `done' relative to its matching stmt.  Usually 0."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-after-else '+
  "How much to indent a statement after an `else' statement."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-after-if '+
  "How much to indent a statement after an `if' statement.
This includes lines after `else' and `elif' statements, too, but
does not affect the `else', `elif' or `fi' statements themselves."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-for-then 0
  "How much to indent a `then' relative to its `if'."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-for-do 0
  "How much to indent a `do' statement.
This is relative to the statement before the `do', typically a
`while', `until', `for', `repeat' or `select' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-do '+
  "How much to indent a line after a `do' statement.
This is used when the `do' is the first word of the line.
This is relative to the statement before the `do', typically a
`while', `until', `for', `repeat' or `select' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-loop-construct '+
  "How much to indent a statement after a loop construct.

This variable is used when the keyword `do' is on the same line as the
loop statement (e.g., `until', `while' or `for').
If the `do' is on a line by itself, then `sh-indent-after-do' is used instead."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)


(defcustom sh-indent-after-done 0
  "How much to indent a statement after a `done' keyword.
Normally this is 0, which aligns the `done' to the matching
looping construct line.
Setting it non-zero allows you to have the `do' statement on a line
by itself and align the done under to do."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-for-case-label '+
  "How much to indent a case label statement.
This is relative to the line containing the `case' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-for-case-alt '++
  "How much to indent statements after the case label.
This is relative to the line containing the `case' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)


(defcustom sh-indent-for-continuation '+
  "How much to indent for a continuation statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-open '+
  "How much to indent after a line with an opening parenthesis or brace.
For an open paren after a function, `sh-indent-after-function' is used."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-function '+
  "How much to indent after a function line."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

;; These 2 are for the rc shell:

(defcustom sh-indent-after-switch '+
  "How much to indent a `case' statement relative to the `switch' statement.
This is for the rc shell."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-case '+
  "How much to indent a statement relative to the `case' statement.
This is for the rc shell."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-backslash-column 48
  "Column in which `sh-backslash-region' inserts backslashes."
  :type 'integer
  :group 'sh)

(defcustom sh-backslash-align t
  "If non-nil, `sh-backslash-region' will align backslashes."
  :type 'boolean
  :group 'sh)

;; Internal use - not designed to be changed by the user:

(defun sh-mkword-regexpr (word)
  "Make a regexp which matches WORD as a word.
This specifically excludes an occurrence of WORD followed by
punctuation characters like `-'."
  (concat word "\\([^-[:alnum:]_]\\|$\\)"))

(defconst sh-re-done (sh-mkword-regexpr "done"))


(defconst sh-kws-for-done
  '((sh .  ( "while" "until" "for" ) )
    (bash . ( "while" "until" "for" "select"  ) )
    (ksh88 . ( "while" "until" "for" "select"  ) )
    (zsh .  ( "while" "until" "for" "repeat" "select" ) ) )
  "Which keywords can match the word `done' in this shell.")


(defconst sh-indent-supported
  '((sh . sh)
    (csh . nil)
    (rc . rc))
  "Indentation rule set to use for each shell type.")

(defvar sh-indent-supported-here nil
  "Non-nil if we support indentation for the current buffer's shell type.")

(defconst sh-var-list
  '(
    sh-basic-offset sh-first-lines-indent sh-indent-after-case
    sh-indent-after-do sh-indent-after-done
    sh-indent-after-else
    sh-indent-after-if
    sh-indent-after-loop-construct
    sh-indent-after-open
    sh-indent-comment
    sh-indent-for-case-alt
    sh-indent-for-case-label
    sh-indent-for-continuation
    sh-indent-for-do
    sh-indent-for-done
    sh-indent-for-else
    sh-indent-for-fi
    sh-indent-for-then
    )
  "A list of variables used by script mode to control indentation.
This list is used when switching between buffer-local and global
values of variables, and for the commands using indentation styles.")

(defvar sh-make-vars-local t
  "Controls whether indentation variables are local to the buffer.
If non-nil, indentation variables are made local initially.
If nil, you can later make the variables local by invoking
command `sh-make-vars-local'.
The default is t because I assume that in one Emacs session one is
frequently editing existing scripts with different styles.")


;; inferior shell interaction
;; TODO: support multiple interactive shells
(defvar-local sh-shell-process nil
  "The inferior shell process for interaction.")

(defvar explicit-shell-file-name)

(defun sh-shell-process (force)
  "Get a shell process for interaction.
If FORCE is non-nil and no process found, create one."
  (if (process-live-p sh-shell-process)
      sh-shell-process
    (setq sh-shell-process
          (let ((found nil) proc
                (procs (process-list)))
            (while (and (not found) procs
                        (process-live-p (setq proc (pop procs)))
                        (process-command proc))
              (when (string-equal sh-shell (file-name-nondirectory
                                            (car (process-command proc))))
                (setq found proc)))
            (or found
                (and force
                     (get-buffer-process
                      (let ((explicit-shell-file-name sh-shell-file)
                            (display-buffer-overriding-action
                             '(nil . ((inhibit-same-window . t)))))
                        ;; We must prevent this `(shell)' call from
                        ;; switching buffers, so that the variable
                        ;; `sh-shell-process' is set locally in the
                        ;; correct buffer.
                        (save-current-buffer
                          (shell))))))))))

(defun sh-show-shell ()
  "Pop the shell interaction buffer."
  (interactive)
  (pop-to-buffer (process-buffer (sh-shell-process t))))

(defun sh-send-text (text)
  "Send the text to the `sh-shell-process'."
  (comint-send-string (sh-shell-process t) (concat text "\n")))

(defun sh-cd-here ()
  "Change directory in the current interaction shell to the current one."
  (interactive)
  (sh-send-text (concat "cd " default-directory)))

(defun sh-send-line-or-region-and-step ()
  "Send the current line to the inferior shell and step to the next line.
When the region is active, send the region instead."
  (interactive)
  (let (from to end)
    (if (use-region-p)
        (setq from (region-beginning)
              to (region-end)
              end to)
      (setq from (line-beginning-position)
            to (line-end-position)
            end (1+ to)))
    (sh-send-text (buffer-substring-no-properties from to))
    (goto-char end)))


;; mode-command and utility functions

(defun sh-after-hack-local-variables ()
  (when (assq 'sh-shell file-local-variables-alist)
    (sh-set-shell (if (symbolp sh-shell)
                      (symbol-name sh-shell)
                    sh-shell))))

;;;###autoload
(define-derived-mode sh-mode prog-mode "Shell-script"
  "Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

This mode adapts to the variations between shells (see `sh-set-shell') by
means of an inheritance based feature lookup (see `sh-feature').  This
mechanism applies to all variables (including skeletons) that pertain to
shell-specific features.  Shell script files can use the `sh-shell' local
variable to indicate the shell variant to be used for the file.

The default style of this mode is that of Rosenblatt's Korn shell book.
The syntax of the statements varies with the shell being used.  The
following commands are available, based on the current shell's syntax:
\\<sh-mode-map>
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

For sh and rc shells indentation commands are:
\\[smie-config-show-indent]	Show the rules controlling this line's indentation.
\\[smie-config-set-indent]	Change the rules controlling this line's indentation.
\\[smie-config-guess]  Try to tweak the indentation rules so the
buffer indents as it currently is indented.


\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-execute-region]	 Have optional header and region be executed in a subshell.

`sh-electric-here-document-mode' controls whether insertion of two
unquoted < insert a here document.  You can control this behavior by
modifying `sh-mode-hook'.

If you generally program a shell different from your login shell you can
set `sh-shell-file' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle."
  (make-local-variable 'sh-shell-file)
  (make-local-variable 'sh-shell)

  (setq-local skeleton-pair-default-alist
	      sh-skeleton-pair-default-alist)

  (setq-local paragraph-start (concat page-delimiter "\\|$"))
  (setq-local paragraph-separate (concat paragraph-start "\\|#!/"))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local local-abbrev-table sh-mode-abbrev-table)
  (setq-local comint-dynamic-complete-functions
	      sh-dynamic-complete-functions)
  (add-hook 'completion-at-point-functions #'comint-completion-at-point nil t)
  ;; we can't look if previous line ended with `\'
  (setq-local comint-prompt-regexp "^[ \t]*")
  (setq-local imenu-case-fold-search nil)
  (setq font-lock-defaults
	`((sh-font-lock-keywords
	   sh-font-lock-keywords-1 sh-font-lock-keywords-2)
	  nil nil
	  ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
	  (font-lock-syntactic-face-function
	   . ,#'sh-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'sh-syntax-propertize-function)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (setq-local skeleton-pair-alist '((?` _ ?`)))
  (setq-local skeleton-pair-filter-function #'sh-quoted-p)
  (setq-local skeleton-further-elements
	      '((< '(- (min sh-basic-offset (current-column))))))
  (setq-local skeleton-filter-function #'sh-feature)
  (setq-local skeleton-newline-indent-rigidly t)
  (setq-local defun-prompt-regexp
              (concat
               "^\\("
               "\\(function[ \t]\\)?[ \t]*[[:alnum:]_]+[ \t]*([ \t]*)"
               "\\|"
               "function[ \t]+[[:alnum:]_]+[ \t]*\\(([ \t]*)\\)?"
               "\\)[ \t]*"))
  (setq-local add-log-current-defun-function #'sh-current-defun-name)
  (add-hook 'completion-at-point-functions
            #'sh-completion-at-point-function nil t)
  (setq-local outline-regexp "###")
  ;; Parse or insert magic number for exec, and set all variables depending
  ;; on the shell thus determined.
  (sh-set-shell
   (cond ((save-excursion
            (goto-char (point-min))
            (looking-at auto-mode-interpreter-regexp))
          (match-string 2))
         ((not buffer-file-name) sh-shell-file)
         ;; Checks that use `buffer-file-name' follow.
         ((string-match "\\.m?spec\\'" buffer-file-name) "rpm")
         ((string-match "[.]sh\\>"     buffer-file-name) "sh")
         ((string-match "[.]bash\\>"   buffer-file-name) "bash")
         ((string-match "[.]ksh\\>"    buffer-file-name) "ksh")
         ((string-match "[.]mkshrc\\>" buffer-file-name) "mksh")
         ((string-match "[.]t?csh\\(rc\\)?\\>" buffer-file-name) "csh")
         ((string-match "[.]zsh\\(rc\\|env\\)?\\>" buffer-file-name) "zsh")
	 ((equal (file-name-nondirectory buffer-file-name) ".profile") "sh")
         (t sh-shell-file))
   nil nil)
  (add-hook 'hack-local-variables-hook
    #'sh-after-hack-local-variables nil t))

;;;###autoload
(defalias 'shell-script-mode 'sh-mode)


(defun sh-font-lock-keywords (&optional keywords)
  "Function to get simple fontification based on `sh-font-lock-keywords'.
This adds rules for comments and assignments."
  (sh-feature sh-font-lock-keywords-var
	      (when (stringp (sh-feature sh-assignment-regexp))
		(lambda (list)
		  `((,(sh-feature sh-assignment-regexp)
		     1 font-lock-variable-name-face)
		    ,@keywords
		    ,@list
		    ,@executable-font-lock-keywords)))))

(defun sh-font-lock-keywords-1 (&optional builtins)
  "Function to get better fontification including keywords."
  (let ((keywords (concat "\\([;(){}`|&]\\|^\\)[ \t]*\\(\\("
			  (regexp-opt (sh-feature sh-leading-keywords) t)
			  "[ \t]+\\)?"
			  (regexp-opt (append (sh-feature sh-leading-keywords)
					      (sh-feature sh-other-keywords))
				      t))))
    (sh-font-lock-keywords
     `(,@(if builtins
	     `((,(concat keywords "[ \t]+\\)?"
			 (regexp-opt (sh-feature sh-builtins) t)
			 "\\>")
		(2 font-lock-keyword-face nil t)
		(6 font-lock-builtin-face))
	       ,@(sh-feature sh-font-lock-keywords-var-2)))
	 (,(concat keywords "\\)\\>")
	  2 font-lock-keyword-face)
	 ,@(sh-feature sh-font-lock-keywords-var-1)))))

(defun sh-font-lock-keywords-2 ()
  "Function to get better fontification including keywords and builtins."
  (sh-font-lock-keywords-1 t))

;;; Completion

(defvar sh--completion-keywords '("if" "while" "until" "for"))

(defun sh--vars-before-point ()
  (save-excursion
    (let ((vars ()))
      (while (re-search-backward "^[ \t]*\\([[:alnum:]_]+\\)=" nil t)
        (push (match-string 1) vars))
      vars)))

;; (defun sh--var-completion-table (string pred action)
;;   (complete-with-action action (sh--vars-before-point) string pred))

(defun sh--cmd-completion-table (string pred action)
  (let ((cmds
         (append (when (fboundp 'imenu--make-index-alist)
                   (mapcar #'car
                           (condition-case nil
                               (imenu--make-index-alist)
                             (imenu-unavailable nil))))
                 (mapcar (lambda (v) (concat v "="))
                         (sh--vars-before-point))
                 (locate-file-completion-table
                  exec-path exec-suffixes string pred t)
                 sh--completion-keywords)))
    (complete-with-action action cmds string pred)))

(defun sh-completion-at-point-function ()
  (save-excursion
    (skip-chars-forward "[:alnum:]_")
    (let ((end (point))
          (_ (skip-chars-backward "[:alnum:]_"))
          (start (point)))
      (cond
       ((eq (char-before) ?$)
        (list start end (sh--vars-before-point)
              :company-kind (lambda (_) 'variable)))
       ((sh-smie--keyword-p)
        (list start end #'sh--cmd-completion-table
              :company-kind
              (lambda (s)
                (cond
                 ((member s sh--completion-keywords) 'keyword)
                 ((string-suffix-p "=" s) 'variable)
                 (t 'function)))
              ))))))

;;; Indentation and navigation with SMIE.

(require 'smie)

(defun sh-smie--keyword-p ()
  "Non-nil if we're at a keyword position.
A keyword position is one where if we're looking at something that looks
like a keyword, then it is a keyword."
  (let ((prev (funcall smie-backward-token-function)))
    (if (zerop (length prev))
        (looking-back "\\`\\|\\s(" (1- (point)))
      (assoc prev smie-grammar))))

(defun sh-smie--newline-semi-p (&optional tok)
  "Return non-nil if a newline should be treated as a semi-colon.
Here we assume that a newline should be treated as a semi-colon unless it
comes right after a special keyword.
This function does not pay attention to line-continuations.
If TOK is nil, point should be before the newline; otherwise, TOK is the token
before the newline and in that case point should be just before the token."
  (save-excursion
    (unless tok
      (setq tok (funcall smie-backward-token-function)))
    (if (and (zerop (length tok))
             (looking-back "\\s(" (1- (point))))
        nil
      (not (numberp (nth 2 (assoc tok smie-grammar)))))))

;;;; SMIE support for `sh'.

(defconst sh-smie-sh-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp)                    ;A constant, or a $var, or a sequence of them...
      (cmd ("case" exp "in" branches "esac")
           ("if" cmd "then" cmd "fi")
           ("if" cmd "then" cmd "else" cmd "fi")
           ("if" cmd "then" cmd "elif" cmd "then" cmd "fi")
           ("if" cmd "then" cmd "elif" cmd "then" cmd "else" cmd "fi")
           ("if" cmd "then" cmd "elif" cmd "then" cmd
                                "elif" cmd "then" cmd "else" cmd "fi")
           ("while" cmd "do" cmd "done")
           ("until" cmd "do" cmd "done")
           ("for" exp "in" cmd "do" cmd "done")
           ("for" exp "do" cmd "done")
           ("select" exp "in" cmd "do" cmd "done")   ;bash&zsh&ksh88.
           ("repeat" exp "do" cmd "done")            ;zsh.
           (exp "always" exp)                        ;zsh.
           (cmd "|" cmd) (cmd "|&" cmd)
           (cmd "&&" cmd) (cmd "||" cmd)
           (cmd ";" cmd) (cmd "&" cmd))
      (rpattern (rpattern "|" rpattern))
      (pattern (rpattern) ("case-(" rpattern))
      (branches (branches ";;" branches)
                (branches ";&" branches) (branches ";;&" branches) ;bash.
                (pattern "case-)" cmd)))
    '((assoc ";;" ";&" ";;&"))
    '((assoc ";" "&") (assoc "&&" "||") (assoc "|" "|&")))))

(defconst sh-smie--sh-operators
  (delq nil (mapcar (lambda (x)
                      (setq x (car x))
                      (and (stringp x)
                           (not (string-match "\\`[a-z]" x))
                           x))
                    sh-smie-sh-grammar)))

(defconst sh-smie--sh-operators-re (regexp-opt sh-smie--sh-operators))
(defconst sh-smie--sh-operators-back-re
  (concat "\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*"
          "\\(" sh-smie--sh-operators-re "\\)"))

(defun sh-smie--sh-keyword-in-p ()
  "Assuming we're looking at \"in\", return non-nil if it's a keyword.
Does not preserve point."
  (let ((forward-sexp-function nil)
        (words nil)                     ;We've seen words.
        (newline nil)                   ;We've seen newlines after the words.
        (res nil)
        prev)
    (while (not res)
      (setq prev (funcall smie-backward-token-function))
      (cond
       ((zerop (length prev))
	(cond
	 (newline (cl-assert words) (setq res 'word))
	 ((bobp) (setq res 'word))
	 (t
          (setq words t)
          (condition-case nil
              (forward-sexp -1)
            (scan-error (setq res 'unknown))))))
       ((equal prev ";")
        (if words (setq newline t)
          (setq res 'keyword)))
       ((member prev '("case" "for" "select")) (setq res 'keyword))
       ((assoc prev smie-grammar) (setq res 'word))
       (t
        (if newline
            (progn (cl-assert words) (setq res 'word))
          (setq words t)))))
    (eq res 'keyword)))

(defun sh-smie--sh-keyword-p (tok)
  "Non-nil if TOK (at which we're looking) really is a keyword."
  (cond
   ((looking-at "[[:alnum:]_]+=") nil)
   ((equal tok "in") (sh-smie--sh-keyword-in-p))
   (t (sh-smie--keyword-p))))

(defun sh-smie--default-forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-forward "."))
              (while (progn (skip-syntax-forward "w_'")
                            (looking-at "\\\\"))
                (forward-char 2)))
          (point))))

(defun sh-smie--default-backward-token ()
  (forward-comment (- (point)))
  (let ((pos (point))
        (n (skip-syntax-backward ".")))
    (if (or (zerop n)
            (and (eq n -1)
                 (let ((p (point)))
                   (if (eq -1 (% (skip-syntax-backward "\\") 2))
                       t
                     (goto-char p)
                     nil))))
        (while
            (progn (skip-syntax-backward ".w_'")
                   (or (not (zerop (skip-syntax-backward "\\")))
                       (when (eq ?\\ (char-before (1- (point))))
                         (let ((p (point)))
                           (forward-char -1)
                           (if (eq -1 (% (skip-syntax-backward "\\") 2))
                               t
                             (goto-char p)
                             nil))))))
      (goto-char (- (point) (% (skip-syntax-backward "\\") 2))))
    (buffer-substring-no-properties (point) pos)))

(defun sh-smie-sh-forward-token ()
  (if (and (looking-at "[ \t]*\\(?:#\\|\\(\\s|\\)\\|$\\)")
           (save-excursion
             (skip-chars-backward " \t")
             (not (bolp))))
      (if (and (match-end 1) (not (nth 3 (syntax-ppss))))
          ;; Right before a here-doc.
          (let ((forward-sexp-function nil))
            (forward-sexp 1)
            ;; Pretend the here-document is a "newline representing a
            ;; semi-colon", since the here-doc otherwise covers the newline(s).
            ";")
        (unless (eobp)
          (let ((semi (sh-smie--newline-semi-p)))
            (forward-line 1)
            (if (or semi (eobp)) ";"
              (sh-smie-sh-forward-token)))))
    (forward-comment (point-max))
    (cond
     ((looking-at "\\\\\n") (forward-line 1) (sh-smie-sh-forward-token))
     ((looking-at sh-smie--sh-operators-re)
      (goto-char (match-end 0))
      (let ((tok (match-string-no-properties 0)))
        (if (and (memq (aref tok (1- (length tok))) '(?\; ?\& ?\|))
                 (looking-at "[ \t]*\\(?:#\\|$\\)"))
            (forward-line 1))
        tok))
     (t
      (let* ((pos (point))
             (tok (sh-smie--default-forward-token)))
        (cond
         ((equal tok ")") "case-)")
         ((equal tok "(") "case-(")
         ((and tok (string-match "\\`[a-z]" tok)
               (assoc tok smie-grammar)
               (not
                (save-excursion
                  (goto-char pos)
                  (sh-smie--sh-keyword-p tok))))
          " word ")
         (t tok)))))))

(defun sh-smie--looking-back-at-continuation-p ()
  (save-excursion
    (and (if (eq (char-before) ?\n) (progn (forward-char -1) t) (eolp))
         (looking-back "\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\\\"
                       (line-beginning-position)))))

(defun sh-smie-sh-backward-token ()
  (let ((bol (line-beginning-position)))
    (forward-comment (- (point)))
    (cond
     ((and (bolp) (not (bobp))
           (equal (syntax-after (1- (point))) (string-to-syntax "|"))
           (not (nth 3 (syntax-ppss))))
      ;; Right after a here-document.
      (let ((forward-sexp-function nil))
        (forward-sexp -1)
        ;; Pretend the here-document is a "newline representing a
        ;; semi-colon", since the here-doc otherwise covers the newline(s).
        ";"))
     ((< (point) bol)
      (cond
       ((sh-smie--looking-back-at-continuation-p)
        (forward-char -1)
        (funcall smie-backward-token-function))
       ((sh-smie--newline-semi-p) ";")
       (t (funcall smie-backward-token-function))))
     ((looking-back sh-smie--sh-operators-back-re
                    (line-beginning-position) 'greedy)
      (goto-char (match-beginning 1))
      (match-string-no-properties 1))
     (t
      (let ((tok (sh-smie--default-backward-token)))
        (cond
         ((equal tok ")") "case-)")
         ((equal tok "(") "case-(")
         ((and tok (string-match "\\`[a-z]" tok)
               (assoc tok smie-grammar)
               (not (save-excursion (sh-smie--sh-keyword-p tok))))
          " word ")
         (t tok)))))))

(defcustom sh-indent-after-continuation t
  "If non-nil, indent relative to the continued line's beginning.
Continued lines can either be indented as \"one long wrapped line\" without
paying attention to the actual syntactic structure, as in:

   for f \\
       in a; do \\
       toto; \\
       done

or as lines that just don't have implicit semi-colons between them, as in:

   for f \\
   in a; do \\
       toto; \\
   done

With `always' you get the former behavior whereas with nil you get the latter.
With t, you get the latter as long as that would indent the continuation line
deeper than the initial line."
  :version "25.1"
  :type '(choice
          (const nil :tag "Never")
          (const t   :tag "Only if needed to make it deeper")
          (const always :tag "Always"))
  :group 'sh-indentation)

(defun sh-smie--continuation-start-indent ()
  "Return the initial indentation of a continued line.
May return nil if the line should not be treated as continued."
  (save-excursion
    (forward-line -1)
    (unless (sh-smie--looking-back-at-continuation-p)
      (current-indentation))))

(defun sh-smie--indent-continuation ()
  (cond
   ((not (and sh-indent-after-continuation
              (save-excursion
                (ignore-errors
                  (skip-chars-backward " \t")
                  (sh-smie--looking-back-at-continuation-p)))))
    nil)
   ((eq sh-indent-after-continuation 'always)
    (save-excursion
      (forward-line -1)
      (if (sh-smie--looking-back-at-continuation-p)
          (current-indentation)
        (+ (current-indentation) sh-basic-offset))))
   (t
    ;; Just make sure a line-continuation is indented deeper.
    (save-excursion
      (let ((indent (let ((sh-indent-after-continuation nil))
                      (smie-indent-calculate)))
            (max most-positive-fixnum))
        (if (not (numberp indent)) indent
          (while (progn
                   (forward-line -1)
                   (let ((ci (current-indentation)))
                     (cond
                      ;; Previous line is less indented, we're good.
                      ((< ci indent) nil)
                      ((sh-smie--looking-back-at-continuation-p)
                       (setq max (min max ci))
                       ;; Previous line is itself a continuation.
                       ;; If it's indented like us, we're good, otherwise
                       ;; check the line before that one.
                       (> ci indent))
                      (t ;Previous line is the beginning of the continued line.
                       (setq indent (min (+ ci sh-basic-offset) max))
                       nil)))))
          indent))))))

(defun sh-smie-sh-rules (kind token)
  (pcase (cons kind token)
    ('(:elem . basic) sh-basic-offset)
    ('(:after . "case-)") (- (sh-var-value 'sh-indent-for-case-alt)
                             (sh-var-value 'sh-indent-for-case-label)))
    (`(:before . ,(or "(" "{" "[" "while" "if" "for" "case"))
     (cond
      ((and (equal token "{") (smie-rule-parent-p "for"))
       (let ((data (smie-backward-sexp "in")))
         (when (equal (nth 2 data) "for")
           `(column . ,(smie-indent-virtual)))))
      ((not (smie-rule-prev-p "&&" "||" "|"))
       (when (smie-rule-hanging-p)
         (smie-rule-parent)))
      (t
       (unless (smie-rule-bolp)
	 (while (equal "|" (nth 2 (smie-backward-sexp 'halfexp))))
	 `(column . ,(smie-indent-virtual))))))
    ;; FIXME: Maybe this handling of ;; should be made into
    ;; a smie-rule-terminator function that takes the substitute ";" as arg.
    (`(:before . ,(or ";;" ";&" ";;&"))
     (if (and (smie-rule-bolp) (looking-at ";;?&?[ \t]*\\(#\\|$\\)"))
         (cons 'column (smie-indent-keyword ";"))
       (smie-rule-separator kind)))
    (`(:after . ,(or ";;" ";&" ";;&"))
     (with-demoted-errors
       (smie-backward-sexp token)
       (cons 'column
             (if (or (smie-rule-bolp)
                     (save-excursion
                       (and (member (funcall smie-backward-token-function)
                                    '("in" ";;"))
                            (smie-rule-bolp))))
                 (current-column)
               (smie-indent-calculate)))))
    (`(:before . ,(or "|" "&&" "||"))
     (unless (smie-rule-parent-p token)
       (smie-backward-sexp token)
       `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                      (smie-indent-virtual)))))

    ;; Attempt at backward compatibility with the old config variables.
    ('(:before . "fi") (sh-var-value 'sh-indent-for-fi))
    ('(:before . "done") (sh-var-value 'sh-indent-for-done))
    ('(:after . "else") (sh-var-value 'sh-indent-after-else))
    ('(:after . "if") (sh-var-value 'sh-indent-after-if))
    ('(:before . "then") (sh-var-value 'sh-indent-for-then))
    ('(:before . "do") (sh-var-value 'sh-indent-for-do))
    ('(:after . "do")
     (sh-var-value (if (smie-rule-hanging-p)
                       'sh-indent-after-loop-construct 'sh-indent-after-do)))
    ;; sh-indent-after-done: aligned completely differently.
    ('(:after . "in") (sh-var-value 'sh-indent-for-case-label))
    ;; sh-indent-for-continuation: Line continuations are handled differently.
    (`(:after . ,(or "(" "{" "["))
     (if (not (looking-at ".[ \t]*[^\n \t#]"))
         (sh-var-value 'sh-indent-after-open)
       (goto-char (1- (match-end 0)))
       `(column . ,(current-column))))
    ;; sh-indent-after-function: we don't handle it differently.
    ))

;; (defconst sh-smie-csh-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;;     '((exp)              ;A constant, or a $var, or a sequence of them...
;;       (elseifcmd (cmd)
;;                  (cmd "else" "else-if" exp "then" elseifcmd))
;;       (cmd ("switch" branches "endsw")
;;            ("if" exp)
;;            ("if" exp "then" cmd "endif")
;;            ("if" exp "then" cmd "else" cmd "endif")
;;            ("if" exp "then" elseifcmd "endif")
;;            ;; ("if" exp "then" cmd "else" cmd "endif")
;;            ;; ("if" exp "then" cmd "else" "if" exp "then" cmd "endif")
;;            ;; ("if" exp "then" cmd "else" "if" exp "then" cmd
;;            ;;                      "else" cmd "endif")
;;            ;; ("if" exp "then" cmd "else" "if" exp "then" cmd
;;            ;;                      "else" "if" exp "then" cmd "endif")
;;            ("while" cmd "end")
;;            ("foreach" cmd "end")
;;            (cmd "|" cmd) (cmd "|&" cmd)
;;            (cmd "&&" cmd) (cmd "||" cmd)
;;            (cmd ";" cmd) (cmd "&" cmd))
;;       ;; This is a lie, but (combined with the corresponding disambiguation
;;       ;; rule) it makes it more clear that `case' and `default' are the key
;;       ;; separators and the `:' is a secondary tokens.
;;       (branches (branches "case" branches)
;;                 (branches "default" branches)
;;                 (exp ":" branches)))
;;     '((assoc "else" "then" "endif"))
;;     '((assoc "case" "default") (nonassoc ":"))
;;     '((assoc ";;" ";&" ";;&"))
;;     '((assoc ";" "&") (assoc "&&" "||") (assoc "|" "|&")))))

;;;; SMIE support for `rc'.

(defconst sh-smie-rc-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp)                    ;A constant, or a $var, or a sequence of them...
      (cmd (cmd "case" cmd)
           ("if" exp)
           ("switch" exp)
           ("for" exp) ("while" exp)
           (cmd "|" cmd) (cmd "|&" cmd)
           (cmd "&&" cmd) (cmd "||" cmd)
           (cmd ";" cmd) (cmd "&" cmd))
      (pattern (pattern "|" pattern))
      (branches (branches ";;" branches)
                (branches ";&" branches) (branches ";;&" branches) ;bash.
                (pattern "case-)" cmd)))
    '((assoc ";;" ";&" ";;&"))
    '((assoc "case") (assoc ";" "&") (assoc "&&" "||") (assoc "|" "|&")))))

(defun sh-smie--rc-after-special-arg-p ()
  "Check if we're after the first arg of an if/while/for/... construct.
Returns the construct's token and moves point before it, if so."
  (forward-comment (- (point)))
  (when (looking-back ")\\|\\_<not" (- (point) 3))
    (ignore-errors
      (let ((forward-sexp-function nil))
        (forward-sexp -1)
        (car (member (funcall smie-backward-token-function)
                     '("if" "for" "switch" "while")))))))

(defun sh-smie--rc-newline-semi-p ()
  "Return non-nil if a newline should be treated as a semi-colon.
Point should be before the newline."
  (save-excursion
    (let ((tok (funcall smie-backward-token-function)))
      (if (or (when (equal tok "not") (forward-word-strictly 1) t)
              (and (zerop (length tok)) (eq (char-before) ?\))))
          (not (sh-smie--rc-after-special-arg-p))
        (sh-smie--newline-semi-p tok)))))

(defun sh-smie-rc-forward-token ()
  ;; FIXME: Code duplication with sh-smie-sh-forward-token.
  (if (and (looking-at "[ \t]*\\(?:#\\|\\(\\s|\\)\\|$\\)")
           (save-excursion
             (skip-chars-backward " \t")
             (not (bolp))))
      (if (and (match-end 1) (not (nth 3 (syntax-ppss))))
          ;; Right before a here-doc.
          (let ((forward-sexp-function nil))
            (forward-sexp 1)
            ;; Pretend the here-document is a "newline representing a
            ;; semi-colon", since the here-doc otherwise covers the newline(s).
            ";")
        (let ((semi (sh-smie--rc-newline-semi-p)))
          (forward-line 1)
          (if (or semi (eobp)) ";"
            (sh-smie-rc-forward-token))))
    (forward-comment (point-max))
    (cond
     ((looking-at "\\\\\n") (forward-line 1) (sh-smie-rc-forward-token))
     ;; ((looking-at sh-smie--rc-operators-re)
     ;;  (goto-char (match-end 0))
     ;;  (let ((tok (match-string-no-properties 0)))
     ;;    (if (and (memq (aref tok (1- (length tok))) '(?\; ?\& ?\|))
     ;;             (looking-at "[ \t]*\\(?:#\\|$\\)"))
     ;;        (forward-line 1))
     ;;    tok))
     (t
      (let* ((pos (point))
             (tok (sh-smie--default-forward-token)))
        (cond
         ;; ((equal tok ")") "case-)")
         ((and tok (string-match "\\`[a-z]" tok)
               (assoc tok smie-grammar)
               (not
                (save-excursion
                  (goto-char pos)
                  (sh-smie--keyword-p))))
          " word ")
         (t tok)))))))

(defun sh-smie-rc-backward-token ()
  ;; FIXME: Code duplication with sh-smie-sh-backward-token.
  (let ((bol (line-beginning-position)))
    (forward-comment (- (point)))
    (cond
     ((and (bolp) (not (bobp))
           (equal (syntax-after (1- (point))) (string-to-syntax "|"))
           (not (nth 3 (syntax-ppss))))
      ;; Right after a here-document.
      (let ((forward-sexp-function nil))
        (forward-sexp -1)
        ;; Pretend the here-document is a "newline representing a
        ;; semi-colon", since the here-doc otherwise covers the newline(s).
        ";"))
     ((< (point) bol)                   ;We skipped over a newline.
      (cond
       ;; A continued line.
       ((and (eolp)
             (looking-back "\\(?:^\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\\\"
                           (line-beginning-position)))
        (forward-char -1)
        (funcall smie-backward-token-function))
       ((sh-smie--rc-newline-semi-p) ";")
       (t (funcall smie-backward-token-function))))
     ;; ((looking-back sh-smie--sh-operators-back-re
     ;;                (line-beginning-position) 'greedy)
     ;;  (goto-char (match-beginning 1))
     ;;  (match-string-no-properties 1))
     (t
      (let ((tok (sh-smie--default-backward-token)))
        (cond
         ;; ((equal tok ")") "case-)")
         ((and tok (string-match "\\`[a-z]" tok)
               (assoc tok smie-grammar)
               (not (save-excursion (sh-smie--keyword-p))))
          " word ")
         (t tok)))))))

(defun sh-smie-rc-rules (kind token)
  (pcase (cons kind token)
    ('(:elem . basic) sh-basic-offset)
    ;; (`(:after . "case") (or sh-basic-offset smie-indent-basic))
    ('(:after . ";")
     (if (smie-rule-parent-p "case")
         (smie-rule-parent (sh-var-value 'sh-indent-after-case))))
    ('(:before . "{")
     (save-excursion
       (when (sh-smie--rc-after-special-arg-p)
         `(column . ,(current-column)))))
    (`(:before . ,(or "(" "{" "["))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    ;; FIXME: SMIE parses "if (exp) cmd" as "(if ((exp) cmd))" so "cmd" is
    ;; treated as an arg to (exp) by default, which indents it all wrong.
    ;; To handle it right, we should extend smie-indent-exps so that the
    ;; preceding keyword can give special rules.  Currently the only special
    ;; rule we have is the :list-intro hack, which we use here to align "cmd"
    ;; with "(exp)", which is rarely the right thing to do, but is better
    ;; than nothing.
    (`(:list-intro . ,(or "for" "if" "while")) t)
    ;; sh-indent-after-switch: handled implicitly by the default { rule.
    ))

;;; End of SMIE code.

(defvar sh-regexp-for-done nil
  "A buffer-local regexp to match opening keyword for done.")


(defun sh-set-shell (shell &optional no-query-flag insert-flag)
  "Set this buffer's shell to SHELL (a string).
When used interactively, insert the proper starting #!-line,
and make the visited file executable via `executable-set-magic',
perhaps querying depending on the value of `executable-query'.
(If given a prefix (i.e., `\\[universal-argument]') don't insert any starting #!
line.)

When this function is called noninteractively, INSERT-FLAG (the third
argument) controls whether to insert a #!-line and think about making
the visited file executable, and NO-QUERY-FLAG (the second argument)
controls whether to query about making the visited file executable.

Calls the value of `sh-set-shell-hook' if set.

Shell script files can cause this function be called automatically
when the file is visited by having a `sh-shell' file-local variable
whose value is the shell name (don't quote it)."
  (interactive (list (completing-read
                      (format-prompt "Shell" sh-shell-file)
                      ;; This used to use interpreter-mode-alist, but that is
                      ;; no longer appropriate now that uses regexps.
                      ;; Maybe there could be a separate variable that lists
                      ;; the shells, used here and to construct i-mode-alist.
                      ;; But the following is probably good enough:
                      (append (mapcar (lambda (e) (symbol-name (car e)))
                                      sh-ancestor-alist)
                              '("csh" "rc" "sh"))
                      nil nil nil nil sh-shell-file)
		     (eq executable-query 'function)
		     (not current-prefix-arg)))
  (if (string-match "\\.exe\\'" shell)
      (setq shell (substring shell 0 (match-beginning 0))))
  (setq sh-shell (sh-canonicalize-shell shell))
  (if insert-flag
      (setq sh-shell-file
	    (executable-set-magic shell (sh-feature sh-shell-arg)
				  no-query-flag insert-flag)))
  (setq mode-line-process (format "[%s]" sh-shell))
  (setq-local sh-shell-variables nil)
  (setq-local sh-shell-variables-initialized nil)
  (setq-local imenu-generic-expression
	      (sh-feature sh-imenu-generic-expression))
  (let ((tem (sh-feature sh-mode-syntax-table-input)))
    (when tem
      (setq-local sh-mode-syntax-table
		  (apply 'sh-mode-syntax-table tem))
      (set-syntax-table sh-mode-syntax-table)))
  (dolist (var (sh-feature sh-variables))
    (sh-remember-variable var))
  (if (setq-local sh-indent-supported-here
		  (sh-feature sh-indent-supported))
      (progn
	(message "Setting up indent for shell type %s" sh-shell)
        (let ((mksym (lambda (name)
                       (intern (format "sh-smie-%s-%s"
                                       sh-indent-supported-here name)))))
	  (add-function :around (local 'smie--hanging-eolp-function)
			(lambda (orig)
			  (if (looking-at "[ \t]*\\\\\n")
			      (goto-char (match-end 0))
			    (funcall orig))))
          (add-hook 'smie-indent-functions #'sh-smie--indent-continuation nil t)
          (smie-setup (symbol-value (funcall mksym "grammar"))
                      (funcall mksym "rules")
                      :forward-token  (funcall mksym "forward-token")
                      :backward-token (funcall mksym "backward-token")))
	(if sh-make-vars-local
	    (sh-make-vars-local))
	(message "Indentation setup for shell type %s" sh-shell))
    (message "No indentation for this shell type.")
    (setq-local indent-line-function #'sh-basic-indent-line))
  (when font-lock-mode
    (setq font-lock-set-defaults nil)
    (font-lock-set-defaults)
    (font-lock-flush))
  (setq sh-shell-process nil)
  (run-hooks 'sh-set-shell-hook))


(defun sh-feature (alist &optional function)
  "Index ALIST by the current shell.
If ALIST isn't a list where every element is a cons, it is returned as is.
Else indexing follows an inheritance logic which works in two ways:

  - Fall back on successive ancestors (see `sh-ancestor-alist') as long as
    the alist contains no value for the current shell.
    The ultimate default is always `sh'.

  - If the value thus looked up is a list starting with `sh-append',
    we call the function `sh-append' with the rest of the list as
    arguments, and use the value.  However, the next element of the
    list is not used as-is; instead, we look it up recursively
    in ALIST to allow the function called to define the value for
    one shell to be derived from another shell.
    The value thus determined is physically replaced into the alist.

If FUNCTION is non-nil, it is called with one argument,
the value thus obtained, and the result is used instead."
  (or (if (consp alist)
	  ;; Check for something that isn't a valid alist.
	  (let ((l alist))
	    (while (and l (consp (car l)))
	      (setq l (cdr l)))
	    (if l alist)))

      (let ((orig-sh-shell sh-shell))
	(let ((sh-shell sh-shell)
	      elt val)
	  (while (and sh-shell
		      (not (setq elt (assq sh-shell alist))))
	    (setq sh-shell (cdr (assq sh-shell sh-ancestor-alist))))
	  ;; If the shell is not known, treat it as sh.
	  (unless elt
	    (setq elt (assq 'sh alist)))
	  (setq val (cdr elt))
	  (if (and (consp val)
		   (memq (car val) '(sh-append sh-modify)))
	      (setq val
		    (apply (car val)
			   ;; Refer to the value for a different shell,
			   ;; as a kind of inheritance.
			   (let ((sh-shell (car (cdr val))))
			     (sh-feature alist))
			   (cddr val))))
	  (if function
	      (setq sh-shell orig-sh-shell
		    val (funcall function val)))
	  val))))



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


(defun sh-basic-indent-line ()
  "Indent a line for Sh mode (shell script mode).
Indent as far as preceding non-empty line, then by steps of `sh-basic-offset'.
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
      (indent-to (if (or (eq this-command 'newline-and-indent)
                         (and electric-indent-mode (eq this-command 'newline)))
		     previous
		   (if (< (current-column)
			  (setq current (progn (back-to-indentation)
					       (current-column))))
		       (if (eolp) previous 0)
		     (delete-region (point)
				    (progn (beginning-of-line) (point)))
		     (if (eolp)
			 (max previous (* (1+ (/ current sh-basic-offset))
					  sh-basic-offset))
		       (* (1+ (/ current sh-basic-offset)) sh-basic-offset))))))
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
      (push (cons var var) sh-shell-variables))
  var)



(defun sh-quoted-p ()
  "Is point preceded by an odd number of backslashes?"
  (eq -1 (% (save-excursion (skip-chars-backward "\\\\")) 2)))

;; Indentation stuff.

(defun sh-make-vars-local ()
  "Make the indentation variables local to this buffer.
Normally they already are local.  This command is provided in case
variable `sh-make-vars-local' has been set to nil.

To revert all these variables to the global values, use
command `sh-reset-indent-vars-to-global-values'."
  (interactive)
  (mapc 'make-local-variable sh-var-list)
  (message "Indentation variables are now local."))

(defun sh-reset-indent-vars-to-global-values ()
  "Reset local indentation variables to the global values.
Then, if variable `sh-make-vars-local' is non-nil, make them local."
  (interactive)
  (mapc 'kill-local-variable sh-var-list)
  (if sh-make-vars-local
      (mapcar 'make-local-variable sh-var-list)))

(defun sh-in-comment-or-string (start)
  "Return non-nil if START is in a comment or string."
  (save-excursion
    (let ((state (syntax-ppss start)))
      (or (nth 3 state) (nth 4 state)))))


(defun sh-var-value (var &optional ignore-error)
  "Return the value of variable VAR, interpreting symbols.
It can also return t or nil.
If an invalid value is found, throw an error unless Optional argument
IGNORE-ERROR is non-nil."
  (let ((val (symbol-value var)))
    (cond
     ((numberp val)
      val)
     ((eq val t)
      val)
     ((null val)
      val)
     ((eq val '+)
      sh-basic-offset)
     ((eq val '-)
      (- sh-basic-offset))
     ((eq val '++)
      (* 2 sh-basic-offset))
     ((eq val '--)
      (* 2 (- sh-basic-offset)))
     ((eq val '*)
      (/ sh-basic-offset 2))
     ((eq val '/)
      (/ (- sh-basic-offset) 2))
     (t
      (funcall (if ignore-error #'message #'error)
               "Don't know how to handle %s's value of %s" var val)
      0))))

(define-obsolete-function-alias 'sh-show-indent
  #'smie-config-show-indent "28.1")

(define-obsolete-function-alias 'sh-set-indent #'smie-config-set-indent "28.1")

(define-obsolete-function-alias 'sh-learn-line-indent
  #'smie-config-set-indent "28.1")

(define-obsolete-function-alias 'sh-learn-buffer-indent
  #'smie-config-guess "28.1")

;; ========================================================================

;; Styles -- a quick and dirty way of saving the indentation settings.

(defvar sh-styles-alist nil
  "A list of all known shell indentation styles.")

(defun sh-name-style (name &optional confirm-overwrite)
  "Name the current indentation settings as a style called NAME.
If this name exists, the command will prompt whether it should be
overwritten if
- - it was called interactively with a prefix argument, or
- - called non-interactively with optional CONFIRM-OVERWRITE non-nil."
  ;; (interactive "sName for this style: ")
  (interactive
   (list
    (read-from-minibuffer "Name for this style? " )
    (not current-prefix-arg)))
  (let ((slist (cons name
		     (mapcar (lambda (var) (cons var (symbol-value var)))
			     sh-var-list)))
	(style (assoc name sh-styles-alist)))
    (if style
	(if (and confirm-overwrite
		 (not (y-or-n-p "This style exists.  Overwrite it? ")))
	    (message "Not changing style %s" name)
	  (message "Updating style %s" name)
	  (setcdr style (cdr slist)))
      (message "Creating new style %s" name)
      (push slist sh-styles-alist))))

(defun sh-load-style (name)
  "Set shell indentation values for this buffer from those in style NAME."
  (interactive (list (completing-read
		      "Which style to use for this buffer? "
		      sh-styles-alist nil t)))
  (let ((sl (assoc name  sh-styles-alist)))
    (if (null sl)
        (error "sh-load-style: Style %s not known" name)
      (dolist (var (cdr sl))
	(set (car var) (cdr var))))))

(defun sh-save-styles-to-buffer (buff)
  "Save all current styles in elisp to buffer BUFF.
This is always added to the end of the buffer."
  (interactive
   (list
    (read-from-minibuffer "Buffer to save styles in? " "*scratch*")))
  (with-current-buffer (get-buffer-create buff)
    (goto-char (point-max))
    (insert "\n")
    (pp `(setq sh-styles-alist ',sh-styles-alist) (current-buffer))))



;; statement syntax-commands for various shells

;; You are welcome to add the syntax or even completely new statements as
;; appropriate for your favorite shell.

(defconst sh-non-closing-paren
  ;; If we leave it rear-sticky, calling `newline' ends up inserting a \n
  ;; that inherits this property, which then confuses the indentation.
  (propertize ")" 'syntax-table sh-st-punc 'rear-nonsticky t))

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
       < < "endsw" \n)
  (es)
  (rc "expression: "
      > "switch( " str " ) {" \n
      > "case " (read-string "pattern: ") \n
      > _ \n
      ( "other pattern, %s: "
	"case " str > \n
	> _ \n)
      "case *" > \n
      > _ \n
      resume:
      ?\} > \n)
  (sh "expression: "
      > "case " str " in" \n
      ( "pattern, %s: "
	> str sh-non-closing-paren \n
	> _ \n
	";;" \n)
      > "*" sh-non-closing-paren \n
      > _ \n
      resume:
      "esac" > \n))

(define-skeleton sh-for
  "Insert a for loop.  See `sh-feature'."
  (csh sh-modify sh
       1 ""
       2 "foreach "
       4 " ( "
       6 " )"
       15 '<
       16 "end")
  (es sh-modify rc
      4 " = ")
  (rc sh-modify sh
      2 "for( "
      6 " ) {"
      15 ?\} )
  (sh "Index variable: "
      > "for " str " in " _ "; do" \n
      > _ | ?$ & (sh-remember-variable str) \n
      "done" > \n))



(define-skeleton sh-indexed-loop
  "Insert an indexed loop from 1 to n.  See `sh-feature'."
  (bash sh-modify posix)
  (csh "Index variable: "
       "@ " str " = 1" \n
       "while( $" str " <= " (read-string "upper limit: ") " )" \n
       > _ ?$ str \n
       "@ " str "++" \n
       < "end" \n)
  (es sh-modify rc
      4 " =")
  (ksh88 "Index variable: "
	 > "integer " str "=0" \n
	 > "while (( ( " str " += 1 ) <= "
	 (read-string "upper limit: ")
	 " )); do" \n
	 > _ ?$ (sh-remember-variable str) > \n
	 "done" > \n)
  (posix "Index variable: "
	 > str "=1" \n
	 "while [ $" str " -le "
	 (read-string "upper limit: ")
	 " ]; do" \n
	 > _ ?$ str \n
	 str ?= (sh-add (sh-remember-variable str) 1) \n
	 "done" > \n)
  (rc "Index variable: "
      > "for( " str " in" " `{awk 'BEGIN { for( i=1; i<="
      (read-string "upper limit: ")
      "; i++ ) print i }'`}) {" \n
      > _ ?$ (sh-remember-variable str) \n
      ?\} > \n)
  (sh "Index variable: "
      > "for " str " in `awk 'BEGIN { for( i=1; i<="
      (read-string "upper limit: ")
      "; i++ ) print i }'`; do" \n
      > _ ?$ (sh-remember-variable str) \n
      "done" > \n))


(defun sh-shell-initialize-variables ()
  "Scan the buffer for variable assignments.
Add these variables to `sh-shell-variables'."
  (message "Scanning buffer `%s' for variable assignments..." (buffer-name))
  (save-excursion
    (goto-char (point-min))
    (setq sh-shell-variables-initialized t)
    (while (search-forward "=" nil t)
      (sh--assignment-collect)))
  (add-hook 'post-self-insert-hook #'sh--assignment-collect nil t)
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
  (let ((vars
	 (with-current-buffer sh-add-buffer
	   (or sh-shell-variables-initialized
	       (sh-shell-initialize-variables))
	   (nconc (mapcar (lambda (var)
                            (substring var 0 (string-search "=" var)))
			  process-environment)
		  sh-shell-variables))))
    (complete-with-action code vars string predicate)))

(defun sh-add (var delta)
  "Insert an addition of VAR and prefix DELTA for Bourne (type) shell."
  (interactive
   (let ((sh-add-buffer (current-buffer)))
     (list (completing-read "Variable: " 'sh-add-completer)
	   (prefix-numeric-value current-prefix-arg))))
  (insert (sh-feature '((bash . "$(( ")
			(ksh88 . "$(( ")
			(posix . "$(( ")
			(rc . "`{expr $")
			(sh . "`expr $")
			(zsh . "$[ ")))
	  (sh-remember-variable var)
	  (if (< delta 0) " - " " + ")
	  (number-to-string (abs delta))
	  (sh-feature '((bash . " ))")
			(ksh88 . " ))")
			(posix . " ))")
			(rc . "}")
			(sh . "`")
			(zsh . " ]")))))



(define-skeleton sh-function
  "Insert a function definition.  See `sh-feature'."
  (bash sh-modify ksh88
	3 "() {")
  (ksh88 "name: "
	 "function " str " {" \n
	 > _ \n
	 < "}" \n)
  (rc sh-modify ksh88
      1 "fn ")
  (sh ()
      "() {" \n
      > _ \n
      < "}" \n))



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
       < "endif" \n)
  (es "condition: "
      > "if { " str " } {" \n
      > _ \n
      ( "other condition, %s: "
	"} { " str " } {" > \n
	> _ \n)
      "} {" > \n
      > _ \n
      resume:
      ?\} > \n)
  (rc "condition: "
      > "if( " str " ) {" \n
      > _ \n
      ( "other condition, %s: "
	"} else if( " str " ) {"  > \n
	> _ \n)
      "} else {" > \n
      > _ \n
      resume:
      ?\} > \n)
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      > "if " str "; then" \n
      > _ \n
      ( "other condition, %s: "
	>  "elif " str "; then" > \n
	> \n)
      "else" > \n
      > \n
      resume:
      "fi" > \n))



(define-skeleton sh-repeat
  "Insert a repeat loop definition.  See `sh-feature'."
  (es nil
      > "forever {" \n
      > _ \n
      ?\} > \n)
  (zsh "factor: "
       > "repeat " str "; do" > \n
       >  \n
       "done" > \n))

;;;(put 'sh-repeat 'menu-enable '(sh-feature sh-repeat))



(define-skeleton sh-select
  "Insert a select statement.  See `sh-feature'."
  (ksh88 "Index variable: "
	 > "select " str " in " _ "; do" \n
	 > ?$ str \n
	 "done" > \n)
  (bash sh-append ksh88))
;;;(put 'sh-select 'menu-enable '(sh-feature sh-select))



(define-skeleton sh-tmp-file
  "Insert code to setup temporary file handling.  See `sh-feature'."
  (bash sh-append ksh88)
  (csh (file-name-nondirectory (buffer-file-name))
       "set tmp = `mktemp -t " str ".XXXXXX`" \n
       "onintr exit" \n _
       (and (goto-char (point-max))
	    (not (bolp))
	    ?\n)
       "exit:\n"
       "rm $tmp* >&" null-device > \n)
  (es (file-name-nondirectory (buffer-file-name))
      > "local( signals = $signals sighup sigint;" \n
      > "tmp = `{ mktemp -t " str ".XXXXXX } ) {" \n
      > "catch @ e {" \n
      > "rm $tmp^* >[2]" null-device \n
      "throw $e" \n
      "} {" > \n
      _ \n
      ?\} > \n
      ?\} > \n)
  (ksh88 sh-modify sh
	 7 "EXIT")
  (rc (file-name-nondirectory (buffer-file-name))
      > "tmp = `{ mktemp -t " str ".XXXXXX }" \n
      "fn sigexit { rm $tmp^* >[2]" null-device " }" \n)
  (sh (file-name-nondirectory (buffer-file-name))
      > "TMP=`mktemp -t " str ".XXXXXX`" \n
      "trap \"rm $TMP* 2>" null-device "\" " ?0 \n))



(define-skeleton sh-until
  "Insert an until loop.  See `sh-feature'."
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      > "until " str "; do" \n
      > _ \n
      "done" > \n))
;;;(put 'sh-until 'menu-enable '(sh-feature sh-until))



(define-skeleton sh-while
  "Insert a while loop.  See `sh-feature'."
  (csh sh-modify sh
       2 ""
       3 "while( "
       5 " )"
       10 '<
       11 "end")
  (es sh-modify sh
      3 "while { "
      5 " } {"
      10 ?\} )
  (rc sh-modify sh
      3 "while( "
      5 " ) {"
      10 ?\} )
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      > "while " str "; do" \n
      > _ \n
      "done" > \n))



(define-skeleton sh-while-getopts
  "Insert a while getopts loop.  See `sh-feature'.
Prompts for an options string which consists of letters for each recognized
option followed by a colon `:' if the option accepts an argument."
  (bash sh-modify sh
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
       < "end" \n)
  (ksh88 sh-modify sh
	 16 "print"
	 18 "${0##*/}"
	 37 "OPTIND-1")
  (posix sh-modify sh
	 18 "$(basename $0)")
  (sh "optstring: "
      > "while getopts :" str " OPT; do" \n
      > "case $OPT in" \n
      '(setq v1 (append (vconcat str) nil))
      ( (prog1 (if v1 (char-to-string (car v1)))
	  (if (eq (nth 1 v1) ?:)
	      (setq v1 (nthcdr 2 v1)
		    v2 "\"$OPTARG\"")
	    (setq v1 (cdr v1)
		  v2 nil)))
	> str "|+" str sh-non-closing-paren \n
	> _ v2 \n
	> ";;" \n)
      > "*" sh-non-closing-paren \n
      > "echo" " \"usage: " "`basename $0`"
      " [+-" '(setq v1 (point)) str
      '(save-excursion
	 (while (search-backward ":" v1 t)
	   (replace-match " ARG] [+-" t t)))
      (if (eq (preceding-char) ?-) -5)
      (if (and (sequencep v1) (length v1)) "] " "} ")
      "[--] ARGS...\"" \n
      "exit 2"  > \n
      "esac" >
      \n "done"
      > \n
      "shift " (sh-add "OPTIND" -1) \n
      "OPTIND=1" \n))



(put 'sh-assignment 'delete-selection t)
(defun sh-assignment (arg)
  "Remember preceding identifier for future completion and do self-insert."
  (declare (obsolete nil "27.1"))
  (interactive "p")
  (self-insert-command arg)
  (sh--assignment-collect))

(defun sh--assignment-collect ()
  (sh-remember-variable
   (when (eq ?= (char-before))
     (save-excursion
       (if (re-search-forward (sh-feature sh-assignment-regexp)
			      (prog1 (point)
			        (beginning-of-line 1))
			      t)
	   (match-string 1))))))


(defun sh-maybe-here-document (arg)
  "Insert self.  Without prefix, following unquoted `<' inserts here document.
The document is bounded by `sh-here-document-word'."
  (declare (obsolete sh-electric-here-document-mode "24.3"))
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (or arg (sh--maybe-here-document)))

(defun sh--maybe-here-document ()
  (when (and (looking-back "[^<]<<[ E-]" (line-beginning-position))
             (save-excursion
	       (backward-char 2)
               (not
                (or (sh-quoted-p)
                    (sh--inside-noncommand-expression (point)))))
             (not (nth 8 (syntax-ppss))))
    (let ((tabs (if (string-match "\\`-" sh-here-document-word)
                    (make-string (/ (current-indentation) tab-width) ?\t)
                  ""))
          (delim (replace-regexp-in-string "['\"]" ""
                                           sh-here-document-word)))
      ;; If we're at <<-, we don't want to delete the previous char.
      (unless (= (preceding-char) ?-)
        (delete-char -1))
      (insert sh-here-document-word)
      (or (eolp) (looking-at "[ \t]") (insert ?\s))
      (end-of-line 1)
      (while
	  (sh-quoted-p)
	(end-of-line 2))
      (insert ?\n tabs)
      (save-excursion
        (insert ?\n tabs (replace-regexp-in-string
                          "\\`-?[ \t]*" "" delim))))))

(define-minor-mode sh-electric-here-document-mode
  "Make << insert a here document skeleton."
  :lighter nil
  (if sh-electric-here-document-mode
      (add-hook 'post-self-insert-hook #'sh--maybe-here-document nil t)
    (remove-hook 'post-self-insert-hook #'sh--maybe-here-document t)))

;; various other commands

(defun sh-beginning-of-command ()
  ;; FIXME: Redefine using SMIE.
  "Move point to successive beginnings of commands."
  (interactive)
  (if (re-search-backward sh-beginning-of-command nil t)
      (goto-char (match-beginning 2))))

(defun sh-end-of-command ()
  ;; FIXME: Redefine using SMIE.
  "Move point to successive ends of commands."
  (interactive)
  (if (re-search-forward sh-end-of-command nil t)
      (goto-char (match-end 1))))

;; Backslashification.  Stolen from make-mode.el.

(defun sh-backslash-region (from to delete-flag)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.

This function does not modify the last line of the region if the region ends
right at the start of the following line; it does not modify blank lines
at the start of the region.  So you can put the region around an entire
shell command and conveniently use this command."
  (interactive "r\nP")
  (save-excursion
    (goto-char from)
    (let ((column sh-backslash-column)
          (endmark (make-marker)))
      (move-marker endmark to)
      ;; Compute the smallest column number past the ends of all the lines.
      (if sh-backslash-align
	  (progn
	    (if (not delete-flag)
		(while (< (point) to)
		  (end-of-line)
		  (if (= (preceding-char) ?\\)
		      (progn (forward-char -1)
			     (skip-chars-backward " \t")))
		  (setq column (max column (1+ (current-column))))
		  (forward-line 1)))
	    ;; Adjust upward to a tab column, if that doesn't push
	    ;; past the margin.
	    (if (> (% column tab-width) 0)
		(let ((adjusted (* (/ (+ column tab-width -1) tab-width)
				   tab-width)))
		  (if (< adjusted (window-width))
		      (setq column adjusted))))))
      ;; Don't modify blank lines at start of region.
      (goto-char from)
      (while (and (< (point) endmark) (eolp))
        (forward-line 1))
      ;; Add or remove backslashes on all the lines.
      (while (and (< (point) endmark)
                  ;; Don't backslashify the last line
                  ;; if the region ends right at the start of the next line.
                  (save-excursion
                    (forward-line 1)
                    (< (point) endmark)))
        (if (not delete-flag)
            (sh-append-backslash column)
          (sh-delete-backslash))
        (forward-line 1))
      (move-marker endmark nil))))

(defun sh-append-backslash (column)
  (end-of-line)
  ;; Note that "\\\\" is needed to get one backslash.
  (if (= (preceding-char) ?\\)
      (progn (forward-char -1)
             (delete-horizontal-space)
             (indent-to column (if sh-backslash-align nil 1)))
    (indent-to column (if sh-backslash-align nil 1))
    (insert "\\")))

(defun sh-delete-backslash ()
  (end-of-line)
  (or (bolp)
      (progn
 	(forward-char -1)
 	(if (looking-at "\\\\")
 	    (delete-region (1+ (point))
 			   (progn (skip-chars-backward " \t") (point)))))))

(provide 'sh-script)

;;; sh-script.el ends here
