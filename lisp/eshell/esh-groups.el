;;; do not modify this file; it is auto-generated

(defgroup eshell-alias nil
  "Command aliases allow for easy definition of alternate commands."
  :tag "Command aliases"
  :link '(info-link "(eshell)Command aliases")
  :group 'eshell-module)

(defgroup eshell-banner nil
  "This sample module displays a welcome banner at login.
It exists so that others wishing to create their own Eshell extension
modules may have a simple template to begin with."
  :tag "Login banner"
  :link '(info-link "(eshell)Login banner")
  :group 'eshell-module)

(defgroup eshell-basic nil
  "The \"basic\" code provides a set of convenience functions which
are traditionally considered shell builtins.  Since all of the
functionality provided by them is accessible through Lisp, they are
not really builtins at all, but offer a command-oriented way to do the
same thing."
  :tag "Basic shell commands"
  :group 'eshell-module)

(defgroup eshell-cmpl nil
  "This module provides a programmable completion function bound to
the TAB key, which allows for completing command names, file names,
variable names, arguments, etc."
  :tag "Argument completion"
  :group 'eshell-module)

(defgroup eshell-dirs nil
  "Directory navigation involves changing directories, examining the
current directory, maintaining a directory stack, and also keeping
track of a history of the last directory locations the user was in.
Emacs does provide standard Lisp definitions of `pwd' and `cd', but
they lack somewhat in feel from the typical shell equivalents."
  :tag "Directory navigation"
  :group 'eshell-module)

(defgroup eshell-glob nil
  "This module provides extended globbing syntax, similar what is used
by zsh for filename generation."
  :tag "Extended filename globbing"
  :group 'eshell-module)

(defgroup eshell-hist nil
  "This module provides command history management."
  :tag "History list management"
  :group 'eshell-module)

(defgroup eshell-ls nil
  "This module implements the \"ls\" utility fully in Lisp.  If it is
passed any unrecognized command switches, it will revert to the
operating system's version.  This version of \"ls\" uses text
properties to colorize its output based on the setting of
`eshell-ls-use-colors'."
  :tag "Implementation of `ls' in Lisp"
  :group 'eshell-module)

(defgroup eshell-pred nil
  "This module allows for predicates to be applied to globbing
patterns (similar to zsh), in addition to string modifiers which can
be applied either to globbing results, variable references, or just
ordinary strings."
  :tag "Value modifiers and predicates"
  :group 'eshell-module)

(defgroup eshell-prompt nil
  "This module provides command prompts, and navigation between them,
as is common with most shells."
  :tag "Command prompts"
  :group 'eshell-module)

(defgroup eshell-rebind nil
  "This module allows for special keybindings that only take effect
while the point is in a region of input text.  By default, it binds
C-a to move to the beginning of the input text (rather than just the
beginning of the line), and C-p and C-n to move through the input
history, C-u kills the current input text, etc.  It also, if
`eshell-confine-point-to-input' is non-nil, does not allow certain
commands to cause the point to leave the input area, such as
`backward-word', `previous-line', etc.  This module intends to mimic
the behavior of normal shells while the user editing new input text."
  :tag "Rebind keys at input"
  :group 'eshell-module)

(defgroup eshell-script nil
  "This module allows for the execution of files containing Eshell
commands, as a script file."
  :tag "Running script files."
  :group 'eshell-module)

(defgroup eshell-smart nil
  "This module combines the facility of normal, modern shells with
some of the edit/review concepts inherent in the design of Plan 9's
9term.  See the docs for more details.

Most likely you will have to turn this option on and play around with
it to get a real sense of how it works."
  :tag "Smart display of output"
  :link '(info-link "(eshell)Smart display of output")
  :group 'eshell-module)

(defgroup eshell-term nil
  "This module causes visual commands (e.g., 'vi') to be executed by
the `term' package, which comes with Emacs.  This package handles most
of the ANSI control codes, allowing curses-based applications to run
within an Emacs window.  The variable `eshell-visual-commands' defines
which commands are considered visual in nature."
  :tag "Running visual commands"
  :group 'eshell-module)

(defgroup eshell-unix nil
  "This module defines many of the more common UNIX utilities as
aliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If
the user passes arguments which are too complex, or are unrecognized
by the Lisp variant, the external version will be called (if
available).  The only reason not to use them would be because they are
usually much slower.  But in several cases their tight integration
with Eshell makes them more versatile than their traditional cousins
\(such as being able to use `kill' to kill Eshell background processes
by name)."
  :tag "UNIX commands in Lisp"
  :group 'eshell-module)

(defgroup eshell-xtra nil
  "This module defines some extra alias functions which are entirely
optional.  They can be viewed as samples for how to write Eshell alias
functions, or as aliases which make some of Emacs' behavior more
naturally accessible within Emacs."
  :tag "Extra alias functions"
  :group 'eshell-module)

