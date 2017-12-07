;;; python.el --- Python's flying circus support for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2003-2017 Free Software Foundation, Inc.

;; Author: Fabián E. Gallina <fgallina@gnu.org>
;; URL: https://github.com/fgallina/python.el
;; Version: 0.25.2
;; Package-Requires: ((emacs "24.1") (cl-lib "1.0"))
;; Maintainer: emacs-devel@gnu.org
;; Created: Jul 2010
;; Keywords: languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Python files with some fontification and
;; indentation bits extracted from original Dave Love's python.el
;; found in GNU/Emacs.

;; Implements Syntax highlighting, Indentation, Movement, Shell
;; interaction, Shell completion, Shell virtualenv support, Shell
;; package support, Shell syntax highlighting, Pdb tracking, Symbol
;; completion, Skeletons, FFAP, Code Check, Eldoc, Imenu.

;; Syntax highlighting: Fontification of code is provided and supports
;; python's triple quoted strings properly.

;; Indentation: Automatic indentation with indentation cycling is
;; provided, it allows you to navigate different available levels of
;; indentation by hitting <tab> several times.  Also electric-indent-mode
;; is supported such that when inserting a colon the current line is
;; dedented automatically if needed.

;; Movement: `beginning-of-defun' and `end-of-defun' functions are
;; properly implemented.  There are also specialized
;; `forward-sentence' and `backward-sentence' replacements called
;; `python-nav-forward-block', `python-nav-backward-block'
;; respectively which navigate between beginning of blocks of code.
;; Extra functions `python-nav-forward-statement',
;; `python-nav-backward-statement',
;; `python-nav-beginning-of-statement', `python-nav-end-of-statement',
;; `python-nav-beginning-of-block', `python-nav-end-of-block' and
;; `python-nav-if-name-main' are included but no bound to any key.  At
;; last but not least the specialized `python-nav-forward-sexp' allows
;; easy navigation between code blocks.  If you prefer `cc-mode'-like
;; `forward-sexp' movement, setting `forward-sexp-function' to nil is
;; enough, You can do that using the `python-mode-hook':

;; (add-hook 'python-mode-hook
;;           (lambda () (setq forward-sexp-function nil)))

;; Shell interaction: is provided and allows opening Python shells
;; inside Emacs and executing any block of code of your current buffer
;; in that inferior Python process.

;; Besides that only the standard CPython (2.x and 3.x) shell and
;; IPython are officially supported out of the box, the interaction
;; should support any other readline based Python shells as well
;; (e.g. Jython and PyPy have been reported to work).  You can change
;; your default interpreter and commandline arguments by setting the
;; `python-shell-interpreter' and `python-shell-interpreter-args'
;; variables.  This example enables IPython globally:

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i")

;; Using the "console" subcommand to start IPython in server-client
;; mode is known to fail intermittently due a bug on IPython itself
;; (see URL `https://debbugs.gnu.org/cgi/bugreport.cgi?bug=18052#27').
;; There seems to be a race condition in the IPython server (A.K.A
;; kernel) when code is sent while it is still initializing, sometimes
;; causing the shell to get stalled.  With that said, if an IPython
;; kernel is already running, "console --existing" seems to work fine.

;; Running IPython on Windows needs more tweaking.  The way you should
;; set `python-shell-interpreter' and `python-shell-interpreter-args'
;; is as follows (of course you need to modify the paths according to
;; your system):

;; (setq python-shell-interpreter "C:\\Python27\\python.exe"
;;       python-shell-interpreter-args
;;       "-i C:\\Python27\\Scripts\\ipython-script.py")

;; Missing or delayed output used to happen due to differences between
;; Operating Systems' pipe buffering (e.g. CPython 3.3.4 in Windows 7.
;; See URL `https://debbugs.gnu.org/cgi/bugreport.cgi?bug=17304').  To
;; avoid this, the `python-shell-unbuffered' defaults to non-nil and
;; controls whether `python-shell-calculate-process-environment'
;; should set the "PYTHONUNBUFFERED" environment variable on startup:
;; See URL `https://docs.python.org/3/using/cmdline.html#cmdoption-u'.

;; The interaction relies upon having prompts for input (e.g. ">>> "
;; and "... " in standard Python shell) and output (e.g. "Out[1]: " in
;; IPython) detected properly.  Failing that Emacs may hang but, in
;; the case that happens, you can recover with \\[keyboard-quit].  To
;; avoid this issue, a two-step prompt autodetection mechanism is
;; provided: the first step is manual and consists of a collection of
;; regular expressions matching common prompts for Python shells
;; stored in `python-shell-prompt-input-regexps' and
;; `python-shell-prompt-output-regexps', and dir-local friendly vars
;; `python-shell-prompt-regexp', `python-shell-prompt-block-regexp',
;; `python-shell-prompt-output-regexp' which are appended to the
;; former automatically when a shell spawns; the second step is
;; automatic and depends on the `python-shell-prompt-detect' helper
;; function.  See its docstring for details on global variables that
;; modify its behavior.

;; Shell completion: hitting tab will try to complete the current
;; word.  The two built-in mechanisms depend on Python's readline
;; module: the "native" completion is tried first and is activated
;; when `python-shell-completion-native-enable' is non-nil, the
;; current `python-shell-interpreter' is not a member of the
;; `python-shell-completion-native-disabled-interpreters' variable and
;; `python-shell-completion-native-setup' succeeds; the "fallback" or
;; "legacy" mechanism works by executing Python code in the background
;; and enables auto-completion for shells that do not support
;; receiving escape sequences (with some limitations, i.e. completion
;; in blocks does not work).  The code executed for the "fallback"
;; completion can be found in `python-shell-completion-setup-code' and
;; `python-shell-completion-string-code' variables.  Their default
;; values enable completion for both CPython and IPython, and probably
;; any readline based shell (it's known to work with PyPy).  If your
;; Python installation lacks readline (like CPython for Windows),
;; installing pyreadline (URL `http://ipython.org/pyreadline.html')
;; should suffice.  To troubleshoot why you are not getting any
;; completions, you can try the following in your Python shell:

;; >>> import readline, rlcompleter

;; If you see an error, then you need to either install pyreadline or
;; setup custom code that avoids that dependency.

;; Shell virtualenv support: The shell also contains support for
;; virtualenvs and other special environment modifications thanks to
;; `python-shell-process-environment' and `python-shell-exec-path'.
;; These two variables allows you to modify execution paths and
;; environment variables to make easy for you to setup virtualenv rules
;; or behavior modifications when running shells.  Here is an example
;; of how to make shell processes to be run using the /path/to/env/
;; virtualenv:

;; (setq python-shell-process-environment
;;       (list
;;        (format "PATH=%s" (mapconcat
;;                           'identity
;;                           (reverse
;;                            (cons (getenv "PATH")
;;                                  '("/path/to/env/bin/")))
;;                           ":"))
;;        "VIRTUAL_ENV=/path/to/env/"))
;; (python-shell-exec-path . ("/path/to/env/bin/"))

;; Since the above is cumbersome and can be programmatically
;; calculated, the variable `python-shell-virtualenv-root' is
;; provided.  When this variable is set with the path of the
;; virtualenv to use, `process-environment' and `exec-path' get proper
;; values in order to run shells inside the specified virtualenv.  So
;; the following will achieve the same as the previous example:

;; (setq python-shell-virtualenv-root "/path/to/env/")

;; Also the `python-shell-extra-pythonpaths' variable have been
;; introduced as simple way of adding paths to the PYTHONPATH without
;; affecting existing values.

;; Shell package support: you can enable a package in the current
;; shell so that relative imports work properly using the
;; `python-shell-package-enable' command.

;; Shell remote support: remote Python shells are started with the
;; correct environment for files opened remotely through tramp, also
;; respecting dir-local variables provided `enable-remote-dir-locals'
;; is non-nil.  The logic for this is transparently handled by the
;; `python-shell-with-environment' macro.

;; Shell syntax highlighting: when enabled current input in shell is
;; highlighted.  The variable `python-shell-font-lock-enable' controls
;; activation of this feature globally when shells are started.
;; Activation/deactivation can be also controlled on the fly via the
;; `python-shell-font-lock-toggle' command.

;; Pdb tracking: when you execute a block of code that contains some
;; call to pdb (or ipdb) it will prompt the block of code and will
;; follow the execution of pdb marking the current line with an arrow.

;; Symbol completion: you can complete the symbol at point.  It uses
;; the shell completion in background so you should run
;; `python-shell-send-buffer' from time to time to get better results.

;; Skeletons: skeletons are provided for simple inserting of things like class,
;; def, for, import, if, try, and while.  These skeletons are
;; integrated with abbrev.  If you have `abbrev-mode' activated and
;; `python-skeleton-autoinsert' is set to t, then whenever you type
;; the name of any of those defined and hit SPC, they will be
;; automatically expanded.  As an alternative you can use the defined
;; skeleton commands: `python-skeleton-<foo>'.

;; FFAP: You can find the filename for a given module when using ffap
;; out of the box.  This feature needs an inferior python shell
;; running.

;; Code check: Check the current file for errors with `python-check'
;; using the program defined in `python-check-command'.

;; Eldoc: returns documentation for object at point by using the
;; inferior python subprocess to inspect its documentation.  As you
;; might guessed you should run `python-shell-send-buffer' from time
;; to time to get better results too.

;; Imenu: There are two index building functions to be used as
;; `imenu-create-index-function': `python-imenu-create-index' (the
;; default one, builds the alist in form of a tree) and
;; `python-imenu-create-flat-index'.  See also
;; `python-imenu-format-item-label-function',
;; `python-imenu-format-parent-item-label-function',
;; `python-imenu-format-parent-item-jump-label-function' variables for
;; changing the way labels are formatted in the tree version.

;; If you used python-mode.el you may miss auto-indentation when
;; inserting newlines.  To achieve the same behavior you have two
;; options:

;; 1) Enable the minor-mode `electric-indent-mode' (enabled by
;;    default) and use RET.  If this mode is disabled use
;;    `newline-and-indent', bound to C-j.

;; 2) Add the following hook in your .emacs:

;; (add-hook 'python-mode-hook
;;   #'(lambda ()
;;       (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; I'd recommend the first one since you'll get the same behavior for
;; all modes out-of-the-box.

;;; Installation:

;; Add this to your .emacs:

;; (add-to-list 'load-path "/folder/containing/file")
;; (require 'python)

;;; TODO:

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'tramp-sh)

;; Avoid compiler warnings
(defvar view-return-to-alist)
(defvar compilation-error-regexp-alist)
(defvar outline-heading-end-regexp)

(autoload 'comint-mode "comint")
(autoload 'help-function-arglist "help-fns")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.py[iw]?\\'") 'python-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist (cons (purecopy "python[0-9.]*") 'python-mode))

(defgroup python nil
  "Python Language's flying circus support for Emacs."
  :group 'languages
  :version "24.3"
  :link '(emacs-commentary-link "python"))


;;; 24.x Compat


(unless (fboundp 'prog-widen)
  (defun prog-widen ()
    (widen)))

(unless (fboundp 'prog-first-column)
  (defun prog-first-column ()
    0))


;;; Bindings

(defvar python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
    (define-key map [remap backward-sentence] 'python-nav-backward-block)
    (define-key map [remap forward-sentence] 'python-nav-forward-block)
    (define-key map [remap backward-up-list] 'python-nav-backward-up-list)
    (define-key map [remap mark-defun] 'python-mark-defun)
    (define-key map "\C-c\C-j" 'imenu)
    ;; Indent specific
    (define-key map "\177" 'python-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
    (define-key map "\C-c<" 'python-indent-shift-left)
    (define-key map "\C-c>" 'python-indent-shift-right)
    ;; Skeletons
    (define-key map "\C-c\C-tc" 'python-skeleton-class)
    (define-key map "\C-c\C-td" 'python-skeleton-def)
    (define-key map "\C-c\C-tf" 'python-skeleton-for)
    (define-key map "\C-c\C-ti" 'python-skeleton-if)
    (define-key map "\C-c\C-tm" 'python-skeleton-import)
    (define-key map "\C-c\C-tt" 'python-skeleton-try)
    (define-key map "\C-c\C-tw" 'python-skeleton-while)
    ;; Shell interaction
    (define-key map "\C-c\C-p" 'run-python)
    (define-key map "\C-c\C-s" 'python-shell-send-string)
    (define-key map "\C-c\C-r" 'python-shell-send-region)
    (define-key map "\C-\M-x" 'python-shell-send-defun)
    (define-key map "\C-c\C-c" 'python-shell-send-buffer)
    (define-key map "\C-c\C-l" 'python-shell-send-file)
    (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
    ;; Some util commands
    (define-key map "\C-c\C-v" 'python-check)
    (define-key map "\C-c\C-f" 'python-eldoc-at-point)
    (define-key map "\C-c\C-d" 'python-describe-at-point)
    ;; Utilities
    (substitute-key-definition 'complete-symbol 'completion-at-point
                               map global-map)
    (easy-menu-define python-menu map "Python Mode menu"
      `("Python"
        :help "Python-specific Features"
        ["Shift region left" python-indent-shift-left :active mark-active
         :help "Shift region left by a single indentation step"]
        ["Shift region right" python-indent-shift-right :active mark-active
         :help "Shift region right by a single indentation step"]
        "-"
        ["Start of def/class" beginning-of-defun
         :help "Go to start of outermost definition around point"]
        ["End of def/class" end-of-defun
         :help "Go to end of definition around point"]
        ["Mark def/class" mark-defun
         :help "Mark outermost definition around point"]
        ["Jump to def/class" imenu
         :help "Jump to a class or function definition"]
        "--"
        ("Skeletons")
        "---"
        ["Start interpreter" run-python
         :help "Run inferior Python process in a separate buffer"]
        ["Switch to shell" python-shell-switch-to-shell
         :help "Switch to running inferior Python process"]
        ["Eval string" python-shell-send-string
         :help "Eval string in inferior Python session"]
        ["Eval buffer" python-shell-send-buffer
         :help "Eval buffer in inferior Python session"]
        ["Eval region" python-shell-send-region
         :help "Eval region in inferior Python session"]
        ["Eval defun" python-shell-send-defun
         :help "Eval defun in inferior Python session"]
        ["Eval file" python-shell-send-file
         :help "Eval file in inferior Python session"]
        ["Debugger" pdb :help "Run pdb under GUD"]
        "----"
        ["Check file" python-check
         :help "Check file for errors"]
        ["Help on symbol" python-eldoc-at-point
         :help "Get help on symbol at point"]
        ["Complete symbol" completion-at-point
         :help "Complete symbol before point"]))
    map)
  "Keymap for `python-mode'.")


;;; Python specialized rx

(eval-and-compile
  (defconst python-rx-constituents
    `((block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with"
                                       ;; Python 3.5+ PEP492
                                       (and "async" (+ space)
                                            (or "def" "for" "with")))
                                   symbol-end))
      (dedenter            . ,(rx symbol-start
                                   (or "elif" "else" "except" "finally")
                                   symbol-end))
      (block-ender         . ,(rx symbol-start
                                  (or
                                   "break" "continue" "pass" "raise" "return")
                                  symbol-end))
      (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start
                                   (or "def" "class"
                                       ;; Python 3.5+ PEP492
                                       (and "async" (+ space) "def"))
                                   symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''")))))
      (coding-cookie . ,(rx line-start ?# (* space)
                            (or
                             ;; # coding=<encoding name>
                             (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                             ;; # -*- coding: <encoding name> -*-
                             (: "-*-" (* space) "coding:" (* space)
                                (group-n 1 (+ (or word ?-))) (* space) "-*-")
                             ;; # vim: set fileencoding=<encoding name> :
                             (: "vim:" (* space) "set" (+ space)
                                "fileencoding" (* space) ?= (* space)
                                (group-n 1 (+ (or word ?-))) (* space) ":")))))
    "Additional Python specific sexps for `python-rx'")

  (defmacro python-rx (&rest regexps)
    "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
    (let ((rx-constituents (append python-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))


;;; Font-lock and syntax

(eval-and-compile
  (defun python-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (`'comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (`'string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (`'paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))

(defun python-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro python-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      (`comment (and (nth 4 ppss) (nth 8 ppss)))
      (`string (and (nth 3 ppss) (nth 8 ppss)))
      (`paren (nth 1 ppss))
      (_ nil))))

(defun python-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst python-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst python-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eql (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))

(define-obsolete-function-alias
  'python-info-ppss-context #'python-syntax-context "24.3")

(define-obsolete-function-alias
  'python-info-ppss-context-type #'python-syntax-context-type "24.3")

(define-obsolete-function-alias
  'python-info-ppss-comment-or-string-p
  #'python-syntax-comment-or-string-p "24.3")

(defun python-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      (if (python-info-docstring-p state)
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(defvar python-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
          "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
          "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
          "try"
          ;; Python 2:
          "print" "exec"
          ;; Python 3:
          ;; False, None, and True are listed as keywords on the Python 3
          ;; documentation, but since they also qualify as constants they are
          ;; fontified like that in order to keep font-lock consistent between
          ;; Python versions.
          "nonlocal"
          ;; Python 3.5+ PEP492
          (and "async" (+ space) (or "def" "for" "with"))
          "await"
          ;; Extra:
          "self")
         symbol-end)
    ;; functions
    (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    ;; classes
    (,(rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    ;; Constants
    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end) . font-lock-constant-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
    (,(rx symbol-start
          (or
           ;; Python 2 and 3:
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
           "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
           "GeneratorExit" "IOError" "ImportError" "ImportWarning"
           "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
           "LookupError" "MemoryError" "NameError" "NotImplementedError"
           "OSError" "OverflowError" "PendingDeprecationWarning"
           "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
           "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
           "TypeError" "UnboundLocalError" "UnicodeDecodeError"
           "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
           "UnicodeWarning" "UserWarning" "ValueError" "Warning"
           "ZeroDivisionError"
           ;; Python 2:
           "StandardError"
           ;; Python 3:
           "BlockingIOError" "BrokenPipeError" "ChildProcessError"
           "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
           "ConnectionResetError" "FileExistsError" "FileNotFoundError"
           "InterruptedError" "IsADirectoryError" "NotADirectoryError"
           "PermissionError" "ProcessLookupError" "RecursionError"
           "ResourceWarning" "StopAsyncIteration" "TimeoutError"
           ;; OS specific
           "VMSError" "WindowsError"
           )
          symbol-end) . font-lock-type-face)
    ;; Builtins
    (,(rx symbol-start
          (or
           "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
           "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
           "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
           "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
           "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
           "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
           "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
           "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
           "__import__"
           ;; Python 2:
           "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
           "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
           "intern"
           ;; Python 3:
           "ascii" "bytearray" "bytes" "exec"
           ;; Extra:
           "__all__" "__doc__" "__name__" "__package__")
          symbol-end) . font-lock-builtin-face)
    ;; assignments
    ;; support for a = b = c = 5
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_)))
                             (? ?\[ (+ (not (any  ?\]))) ?\]) (* space)
                             assignment-operator))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (or (python-syntax-context 'paren)
                          (equal (char-after (point)) ?=))))
          res))
     (1 font-lock-variable-name-face nil nil))
    ;; support for a, b, c = (1, 2, 3)
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                             (* ?, (* space) (+ (any word ?. ?_)) (* space))
                             ?, (* space) (+ (any word ?. ?_)) (* space)
                             assignment-operator))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (goto-char (match-end 1))
                      (python-syntax-context 'paren)))
          res))
     (1 font-lock-variable-name-face nil nil))))

(defconst python-syntax-propertize-function
  (syntax-propertize-rules
   ((python-rx string-delimiter)
    (0 (ignore (python-syntax-stringify))))))

(defvar python-prettify-symbols-alist
  '(("lambda"  . ?λ)
    ("and" . ?∧)
    ("or" . ?∨))
  "Value for `prettify-symbols-alist' in `python-mode'.")

(define-obsolete-variable-alias 'python--prettify-symbols-alist
  'python-prettify-symbols-alist "26.1")

(defsubst python-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun python-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (python-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defvar python-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Python files.")

(defvar python-dotty-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Python files.
It makes underscores and dots word constituent chars.")


;;; Indentation

(defcustom python-indent-offset 4
  "Default indentation offset for Python."
  :group 'python
  :type 'integer
  :safe 'integerp)

(defcustom python-indent-guess-indent-offset t
  "Non-nil tells Python mode to guess `python-indent-offset' value."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-indent-guess-indent-offset-verbose t
  "Non-nil means to emit a warning when indentation guessing fails."
  :version "25.1"
  :type 'boolean
  :group 'python
  :safe' booleanp)

(defcustom python-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `python-indent-line' call."
  :type '(repeat symbol)
  :group 'python)

(define-obsolete-variable-alias
  'python-indent 'python-indent-offset "24.3")

(define-obsolete-variable-alias
  'python-guess-indent 'python-indent-guess-indent-offset "24.3")

(defvar python-indent-current-level 0
  "Deprecated var available for compatibility.")

(defvar python-indent-levels '(0)
  "Deprecated var available for compatibility.")

(make-obsolete-variable
 'python-indent-current-level
 "The indentation API changed to avoid global state.
The function `python-indent-calculate-levels' does not use it
anymore.  If you were defadvising it and or depended on this
variable for indentation customizations, refactor your code to
work on `python-indent-calculate-indentation' instead."
 "24.5")

(make-obsolete-variable
 'python-indent-levels
 "The indentation API changed to avoid global state.
The function `python-indent-calculate-levels' does not use it
anymore.  If you were defadvising it and or depended on this
variable for indentation customizations, refactor your code to
work on `python-indent-calculate-indentation' instead."
 "24.5")

(defun python-indent-guess-indent-offset ()
  "Guess and set `python-indent-offset' for the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (prog-widen)
      (goto-char (point-min))
      (let ((block-end))
        (while (and (not block-end)
                    (re-search-forward
                     (python-rx line-start block-start) nil t))
          (when (and
                 (not (python-syntax-context-type))
                 (progn
                   (goto-char (line-end-position))
                   (python-util-forward-comment -1)
                   (if (equal (char-before) ?:)
                       t
                     (forward-line 1)
                     (when (python-info-block-continuation-line-p)
                       (while (and (python-info-continuation-line-p)
                                   (not (eobp)))
                         (forward-line 1))
                       (python-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         t)))))
            (setq block-end (point-marker))))
        (let ((indentation
               (when block-end
                 (goto-char block-end)
                 (python-util-forward-comment)
                 (current-indentation))))
          (if (and indentation (not (zerop indentation)))
              (set (make-local-variable 'python-indent-offset) indentation)
            (when python-indent-guess-indent-offset-verbose
              (message "Can't guess python-indent-offset, using defaults: %s"
                       python-indent-offset))))))))

(defun python-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
  (save-restriction
    (prog-widen)
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (python-syntax-context 'string ppss)))
          (when start
            (cons (if (python-info-docstring-p)
                      :inside-docstring
                    :inside-string) start))))
       ;; Inside a paren.
       ((let* ((start (python-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (python-util-forward-comment)
                          (line-number-at-pos))))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (python-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (python-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (python-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line starts from a opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (python-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (cons :inside-paren
                      (save-excursion
                        (goto-char (1+ start))
                        (skip-syntax-forward "(" 1)
                        (skip-syntax-forward " ")
                        (point))))))))
       ;; After backslash.
       ((let ((start (when (not (python-syntax-comment-or-string-p ppss))
                       (python-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (python-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (python-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (python-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (python-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (python-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (python-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (python-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (python-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         (python-nav-beginning-of-block)))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (python-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (if (bobp)
              (cons :no-indent 0)
            (python-nav-beginning-of-statement)
            (cons
             (cond ((python-info-current-line-comment-p)
                    :after-comment)
                   ((save-excursion
                      (goto-char (line-end-position))
                      (python-util-forward-comment -1)
                      (python-nav-beginning-of-statement)
                      (looking-at (python-rx block-ender)))
                    :after-block-end)
                   (t :after-line))
             (point)))))))))

(defun python-indent--calculate-indentation ()
  "Internal implementation of `python-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
  (save-restriction
    (prog-widen)
    (save-excursion
      (pcase (python-indent-context)
        (`(:no-indent . ,_) (prog-first-column)) ; usually 0
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash
                :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(:inside-docstring . ,start)
         (let* ((line-indentation (current-indentation))
                (base-indent (progn
                               (goto-char start)
                               (current-indentation))))
           (max line-indentation base-indent)))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :after-backslash-assignment-continuation
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) python-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) python-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (python-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               (prog-first-column) ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         ;; Add two indentation levels to make the suite stand out.
         (goto-char start)
         (+ (current-indentation) (* python-indent-offset 2)))))))

(defun python-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (nconc (number-sequence (prog-first-column) (1- indentation)
                            python-indent-offset)
           (list indentation))))

(defun python-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun python-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (python-indent--calculate-indentation))
         (levels (python-indent--calculate-levels indentation)))
    (if previous
        (python-indent--previous-level levels (current-indentation))
      (if levels
          (apply #'max levels)
        (prog-first-column)))))

(defun python-indent-line (&optional previous)
  "Internal implementation of `python-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (python-indent-calculate-indentation previous))
      (python-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun python-indent-calculate-levels ()
  "Return possible indentation levels."
  (python-indent--calculate-levels
   (python-indent--calculate-indentation)))

(defun python-indent-line-function ()
  "`indent-line-function' for Python mode.
When the variable `last-command' is equal to one of the symbols
inside `python-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (python-indent-line
   (and (memq this-command python-indent-trigger-commands)
        (eq last-command this-command))))

(defun python-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (python-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (python-indent-line t)
      t))

(defun python-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (python-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'python-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun python-indent-region (start end)
  "Indent a Python region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (python-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (python-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (python-info-current-line-empty-p)))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes or a docstring.
                   (or (not (python-syntax-context 'string))
                       (eq
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (python-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|"))
                       (python-info-docstring-p))
                   ;; Skip if current line is a block start, a
                   ;; dedenter or block ender.
                   (save-excursion
                     (back-to-indentation)
                     (not (looking-at
                           (python-rx
                            (or block-start dedenter block-ender))))))
              (python-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun python-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `python-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun python-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `python-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  python-indent-offset))
    (indent-rigidly start end count)))

(defun python-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event)
             (not (python-syntax-context 'string))
             (save-excursion
               (beginning-of-line)
               (not (python-syntax-context 'string (syntax-ppss)))))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (python-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (python-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (python-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (python-info-dedenter-statement-p))
            (current-pos (point)))
        (when dedenter-pos
          (save-excursion
            (goto-char dedenter-pos)
            (python-indent-line)
            (unless (= (line-number-at-pos dedenter-pos)
                       (line-number-at-pos current-pos))
              ;; Reindent region if this is a multiline statement
              (python-indent-region dedenter-pos current-pos)))))))))


;;; Mark

(defun python-mark-defun (&optional allow-extend)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.

Interactively (or with ALLOW-EXTEND non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next defun after the ones already marked."
  (interactive "p")
  (when (python-info-looking-at-beginning-of-defun)
    (end-of-line 1))
  (mark-defun allow-extend))


;;; Navigation

(defvar python-nav-beginning-of-defun-regexp
  (python-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defun python-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `python-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (beg-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and
                         (not (python-info-looking-at-beginning-of-defun))
                         (python-nav-backward-block)))
                 (or (and (python-info-looking-at-beginning-of-defun)
                          (+ (current-indentation) python-indent-offset))
                     0))))
         (found
          (progn
            (when (and (< arg 0)
                       (python-info-looking-at-beginning-of-defun))
              (end-of-line 1))
            (while (and (funcall re-search-fn
                                 python-nav-beginning-of-defun-regexp nil t)
                        (or (python-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) beg-indentation)))))
            (and (python-info-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) t)
      (and (goto-char pos) nil))))

(defun python-nav-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled with care depending on current
point position.  Return non-nil if point is moved to
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (python-nav--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun python-nav-end-of-defun ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (python-info-looking-at-beginning-of-defun)
              (python-nav-beginning-of-defun 1)
              (python-nav-beginning-of-defun -1))
      (setq beg-defun-indent (current-indentation))
      (while (progn
               (python-nav-end-of-statement)
               (python-util-forward-comment 1)
               (and (> (current-indentation) beg-defun-indent)
                    (not (eobp)))))
      (python-util-forward-comment -1)
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> beg-pos (point)) (goto-char beg-pos)))))

(defun python-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`python-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'python-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))

(defun python-nav--forward-defun (arg)
  "Internal implementation of python-nav-{backward,forward}-defun.
Uses ARG to define which function to call, and how many times
repeat it."
  (let ((found))
    (while (and (> arg 0)
                (setq found
                      (python-nav--syntactically
                       (lambda ()
                         (re-search-forward
                          python-nav-beginning-of-defun-regexp nil t))
                       '>)))
      (setq arg (1- arg)))
    (while (and (< arg 0)
                (setq found
                      (python-nav--syntactically
                       (lambda ()
                         (re-search-backward
                          python-nav-beginning-of-defun-regexp nil t))
                       '<)))
      (setq arg (1+ arg)))
    found))

(defun python-nav-backward-defun (&optional arg)
  "Navigate to closer defun backward ARG times.
Unlikely `python-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (python-nav--forward-defun (- (or arg 1))))

(defun python-nav-forward-defun (&optional arg)
  "Navigate to closer defun forward ARG times.
Unlikely `python-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (python-nav--forward-defun (or arg 1)))

(defun python-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (back-to-indentation)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (python-syntax-context 'paren ppss)
           (python-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (python-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (python-info-line-ends-backslash-p))
           (forward-line -1)
           (python-nav-beginning-of-statement))))
  (point-marker))

(defun python-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos (last-string-end 0))
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (python-syntax-context 'string))
                       ;; The assertion can only fail if syntax table
                       ;; text properties and the `syntax-ppss' cache
                       ;; are somehow out of whack.  This has been
                       ;; observed when using `syntax-ppss' during
                       ;; narrowing.
                       (cl-assert (> string-start last-string-end)
                                  :show-args
                                  "Overlapping strings detected")
                       (goto-char string-start)
                       (if (python-syntax-context 'paren)
                           ;; Ended up inside a paren, roll again.
                           (python-nav-end-of-statement t)
                         ;; This is not inside a paren, move to the
                         ;; end of this string.
                         (goto-char (+ (point)
                                       (python-syntax-count-quotes
                                        (char-after (point)) (point))))
                         (setq last-string-end
                               (or (re-search-forward
                                    (rx (syntax string-delimiter)) nil t)
                                   (goto-char (point-max))))))
                      ((python-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (python-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))

(defun python-nav-backward-statement (&optional arg)
  "Move backward to previous statement.
With ARG, repeat.  See `python-nav-forward-statement'."
  (interactive "^p")
  (or arg (setq arg 1))
  (python-nav-forward-statement (- arg)))

(defun python-nav-forward-statement (&optional arg)
  "Move forward to next statement.
With ARG, repeat.  With negative argument, move ARG times
backward to previous statement."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (python-nav-end-of-statement)
    (python-util-forward-comment)
    (python-nav-beginning-of-statement)
    (setq arg (1- arg)))
  (while (< arg 0)
    (python-nav-beginning-of-statement)
    (python-util-forward-comment -1)
    (python-nav-beginning-of-statement)
    (setq arg (1+ arg))))

(defun python-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    (if (progn
          (python-nav-beginning-of-statement)
          (looking-at (python-rx block-start)))
        (point-marker)
      ;; Go to first line beginning a statement
      (while (and (not (bobp))
                  (or (and (python-nav-beginning-of-statement) nil)
                      (python-info-current-line-comment-p)
                      (python-info-current-line-empty-p)))
        (forward-line -1))
      (let ((block-matching-indent
             (- (current-indentation) python-indent-offset)))
        (while
            (and (python-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (python-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun python-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (python-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (python-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (python-nav-end-of-statement) t))
                      (python-info-current-line-comment-p)
                      (python-info-current-line-empty-p))))
      (python-util-forward-comment -1)
      (point-marker))))

(defun python-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `python-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (python-nav-forward-block (- arg)))

(defun python-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (python-rx line-start (* whitespace) block-start))
        (starting-pos (point)))
    (while (> arg 0)
      (python-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (python-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (python-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (python-syntax-context-type)))
      (setq arg (1+ arg)))
    (python-nav-beginning-of-statement)
    (if (not (looking-at (python-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun python-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun python-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at a opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (python-rx close-paren) (python-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (python-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (python-syntax-context 'paren)))))))

(defun python-nav--forward-sexp (&optional dir safe skip-parens-p)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (python-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((and (not skip-parens-p)
             (or (eq context-type 'paren)
                 (if forward-p
                     (eq (syntax-class (syntax-after (point)))
                         (car (string-to-syntax "(")))
                   (eq (syntax-class (syntax-after (1- (point))))
                       (car (string-to-syntax ")"))))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (python-nav--lisp-forward-sexp-safe dir)
          (python-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `python-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((python-info-beginning-of-block-p) 'block-start)
                 ((python-info-end-of-block-p) 'block-end)
                 ((python-info-beginning-of-statement-p) 'statement-start)
                 ((python-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (python-nav--lisp-forward-sexp-safe dir)
                    (python-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((python-info-beginning-of-block-p) 'block-start)
                   ((python-info-end-of-block-p) 'block-end)
                   ((python-info-beginning-of-statement-p) 'statement-start)
                   ((python-info-end-of-statement-p) 'statement-end)
                   ((python-info-statement-starts-block-p) 'starts-block)
                   ((python-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (python-info-current-line-empty-p))
                     (python-util-forward-comment dir)
                     (python-nav--forward-sexp dir safe skip-parens-p))
                    ((eq context 'block-start)
                     (python-nav-end-of-block))
                    ((eq context 'statement-start)
                     (python-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (python-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (python-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (python-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (python-info-current-line-empty-p))
                   (python-util-forward-comment dir)
                   (python-nav--forward-sexp dir safe skip-parens-p))
                  ((eq context 'block-end)
                   (python-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (python-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (python-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (python-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (python-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun python-nav-forward-sexp (&optional arg safe skip-parens-p)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Do not follow parens on interactive calls.  This hack to detect
  ;; if the function was called interactively copes with the way
  ;; `forward-sexp' works by calling `forward-sexp-function', losing
  ;; interactive detection by checking `current-prefix-arg'.  The
  ;; reason to make this distinction is that lisp functions like
  ;; `blink-matching-open' get confused causing issues like the one in
  ;; Bug#16191.  With this approach the user gets a symmetric behavior
  ;; when working interactively while called functions expecting
  ;; paren-based sexp motion work just fine.
  (or
   skip-parens-p
   (setq skip-parens-p
         (memq real-this-command
               (list
                #'forward-sexp #'backward-sexp
                #'python-nav-forward-sexp #'python-nav-backward-sexp
                #'python-nav-forward-sexp-safe #'python-nav-backward-sexp))))
  (while (> arg 0)
    (python-nav--forward-sexp 1 safe skip-parens-p)
    (setq arg (1- arg)))
  (while (< arg 0)
    (python-nav--forward-sexp -1 safe skip-parens-p)
    (setq arg (1+ arg))))

(defun python-nav-backward-sexp (&optional arg safe skip-parens-p)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  (python-nav-forward-sexp (- arg) safe skip-parens-p))

(defun python-nav-forward-sexp-safe (&optional arg skip-parens-p)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  With optional argument SKIP-PARENS-P force
sexp motion to ignore parenthesized expressions when looking at
them in either direction (forced to t in interactive calls)."
  (interactive "^p")
  (python-nav-forward-sexp arg t skip-parens-p))

(defun python-nav-backward-sexp-safe (&optional arg skip-parens-p)
  "Move backward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  With optional argument SKIP-PARENS-P force sexp
motion to ignore parenthesized expressions when looking at them in
either direction (forced to t in interactive calls)."
  (interactive "^p")
  (python-nav-backward-sexp arg t skip-parens-p))

(defun python-nav--up-list (&optional dir)
  "Internal implementation of `python-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`python-nav-up-list' calls."
  (let ((context (python-syntax-context-type))
        (forward-p (> dir 0)))
    (cond
     ((memq context '(string comment)))
     ((eq context 'paren)
      (let ((forward-sexp-function))
        (up-list dir)))
     ((and forward-p (python-info-end-of-block-p))
      (let ((parent-end-pos
             (save-excursion
               (let ((indentation (and
                                   (python-nav-beginning-of-block)
                                   (current-indentation))))
                 (while (and indentation
                             (> indentation 0)
                             (>= (current-indentation) indentation)
                             (python-nav-backward-block)))
                 (python-nav-end-of-block)))))
        (and (> (or parent-end-pos (point)) (point))
             (goto-char parent-end-pos))))
     (forward-p (python-nav-end-of-block))
     ((and (not forward-p)
           (> (current-indentation) 0)
           (python-info-beginning-of-block-p))
      (let ((prev-block-pos
             (save-excursion
               (let ((indentation (current-indentation)))
                 (while (and (python-nav-backward-block)
                             (>= (current-indentation) indentation))))
               (point))))
        (and (> (point) prev-block-pos)
             (goto-char prev-block-pos))))
     ((not forward-p) (python-nav-beginning-of-block)))))

(defun python-nav-up-list (&optional arg)
  "Move forward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (python-nav--up-list 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (python-nav--up-list -1)
    (setq arg (1+ arg))))

(defun python-nav-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (python-nav-up-list (- arg)))

(defun python-nav-if-name-main ()
  "Move point at the beginning the __main__ block.
When \"if __name__ == \\='__main__\\=':\" is found returns its
position, else returns nil."
  (interactive)
  (let ((point (point))
        (found (catch 'found
                 (goto-char (point-min))
                 (while (re-search-forward
                         (python-rx line-start
                                    "if" (+ space)
                                    "__name__" (+ space)
                                    "==" (+ space)
                                    (group-n 1 (or ?\" ?\'))
                                    "__main__" (backref 1) (* space) ":")
                         nil t)
                   (when (not (python-syntax-context-type))
                     (beginning-of-line)
                     (throw 'found t))))))
    (if found
        (point)
      (ignore (goto-char point)))))


;;; Shell integration

(defcustom python-shell-buffer-name "Python"
  "Default buffer name for Python interpreter."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-interpreter "python"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python)

(defcustom python-shell-internal-buffer-name "Python Internal"
  "Default buffer name for the Internal Python interpreter."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-interpreter-args "-i"
  "Default arguments for the Python interpreter."
  :type 'string
  :group 'python)

(defcustom python-shell-interpreter-interactive-arg "-i"
  "Interpreter argument to force it to run interactively."
  :type 'string
  :version "24.4")

(defcustom python-shell-prompt-detect-enabled t
  "Non-nil enables autodetection of interpreter prompts."
  :type 'boolean
  :safe 'booleanp
  :version "24.4")

(defcustom python-shell-prompt-detect-failure-warning t
  "Non-nil enables warnings when detection of prompts fail."
  :type 'boolean
  :safe 'booleanp
  :version "24.4")

(defcustom python-shell-prompt-input-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :version "24.4")

(defcustom python-shell-prompt-output-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts."
  :type '(repeat string)
  :version "24.4")

(defcustom python-shell-prompt-regexp ">>> "
  "Regular expression matching top level input prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom python-shell-prompt-block-regexp "\\.\\.\\. "
  "Regular expression matching block input prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom python-shell-prompt-output-regexp ""
  "Regular expression matching output prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular expression matching pdb input prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(define-obsolete-variable-alias
  'python-shell-enable-font-lock 'python-shell-font-lock-enable "25.1")

(defcustom python-shell-font-lock-enable t
  "Should syntax highlighting be enabled in the Python shell buffer?
Restart the Python shell after changing this variable for it to take effect."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-shell-unbuffered t
  "Should shell output be unbuffered?.
When non-nil, this may prevent delayed and missing output in the
Python shell.  See commentary for details."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-shell-process-environment nil
  "List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
When this variable is non-nil, values are exported into the
process environment before starting it.  Any variables already
present in the current environment are superseded by variables
set here."
  :type '(repeat string)
  :group 'python)

(defcustom python-shell-extra-pythonpaths nil
  "List of extra pythonpaths for Python shell.
When this variable is non-nil, values added at the beginning of
the PYTHONPATH before starting processes.  Any values present
here that already exists in PYTHONPATH are moved to the beginning
of the list so that they are prioritized when looking for
modules."
  :type '(repeat string)
  :group 'python)

(defcustom python-shell-exec-path nil
  "List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
  :group 'python)

(defcustom python-shell-remote-exec-path nil
  "List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
`python-shell-exec-path' will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Python shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
  :group 'python)

(defcustom python-shell-virtualenv-root nil
  "Path to virtualenv root.
This variable, when set to a string, makes the environment to be
modified such that shells are started within the specified
virtualenv."
  :type '(choice (const nil) string)
  :group 'python)

(define-obsolete-variable-alias
  'python-shell-virtualenv-path 'python-shell-virtualenv-root "25.1")

(defcustom python-shell-setup-codes nil
  "List of code run by `python-shell-send-setup-codes'."
  :type '(repeat symbol)
  :group 'python)

(defcustom python-shell-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python."
  :type '(alist string)
  :group 'python)

(defmacro python-shell--add-to-path-with-priority (pathvar paths)
  "Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun python-shell-calculate-pythonpath ()
  "Calculate the PYTHONPATH using `python-shell-extra-pythonpaths'."
  (let ((pythonpath
         (split-string
          (or (getenv "PYTHONPATH") "") path-separator 'omit)))
    (python-shell--add-to-path-with-priority
     pythonpath python-shell-extra-pythonpaths)
    (mapconcat 'identity pythonpath path-separator)))

(defun python-shell-calculate-process-environment ()
  "Calculate `process-environment' or `tramp-remote-process-environment'.
Prepends `python-shell-process-environment', sets extra
pythonpaths from `python-shell-extra-pythonpaths' and sets a few
virtualenv related vars.  If `default-directory' points to a
remote host, the returned value is intended for
`tramp-remote-process-environment'."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment))
         (virtualenv (when python-shell-virtualenv-root
                       (directory-file-name python-shell-virtualenv-root))))
    (dolist (env python-shell-process-environment)
      (pcase-let ((`(,key ,value) (split-string env "=")))
        (setenv key value)))
    (when python-shell-unbuffered
      (setenv "PYTHONUNBUFFERED" "1"))
    (when python-shell-extra-pythonpaths
      (setenv "PYTHONPATH" (python-shell-calculate-pythonpath)))
    (if (not virtualenv)
        process-environment
      (setenv "PYTHONHOME" nil)
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defun python-shell-calculate-exec-path ()
  "Calculate `exec-path'.
Prepends `python-shell-exec-path' and adds the binary directory
for virtualenv if `python-shell-virtualenv-root' is set - this
will use the python interpreter from inside the virtualenv when
starting the shell.  If `default-directory' points to a remote host,
the returned value appends `python-shell-remote-exec-path' instead
of `exec-path'."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       python-shell-remote-exec-path
                     exec-path)))

        ;; Windows and POSIX systems use different venv directory structures
        (virtualenv-bin-dir (if (eq system-type 'windows-nt) "Scripts" "bin")))
    (python-shell--add-to-path-with-priority
     new-path python-shell-exec-path)
    (if (not python-shell-virtualenv-root)
        new-path
      (python-shell--add-to-path-with-priority
       new-path
       (list (expand-file-name virtualenv-bin-dir python-shell-virtualenv-root)))
      new-path)))

(defun python-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      (python-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))

(defun python-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from `tramp-open-connection-setup-interactive-shell'.
  (let ((env (append (when (fboundp #'tramp-get-remote-locale)
                       ;; Emacs<24.4 compat.
                       (list (tramp-get-remote-locale vec)))
		     (copy-sequence env)))
        (tramp-end-of-heredoc
         (if (boundp 'tramp-end-of-heredoc)
             tramp-end-of-heredoc
           (md5 tramp-end-of-output)))
	unset vars item)
    (while env
      (setq item (split-string (car env) "=" 'omit))
      (setcdr item (mapconcat 'identity (cdr item) "="))
      (if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
	  (push (format "%s %s" (car item) (cdr item)) vars)
	(push (car item) unset))
      (setq env (cdr env)))
    (when vars
      (tramp-send-command
       vec
       (format "while read var val; do export $var=$val; done <<'%s'\n%s\n%s"
	       tramp-end-of-heredoc
	       (mapconcat 'identity vars "\n")
	       tramp-end-of-heredoc)
       t))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " ")) t))))

(defmacro python-shell-with-environment (&rest body)
  "Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`python-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (python-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (python-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (python-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (python-shell-tramp-refresh-remote-path
            ,vec (python-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (python-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))

(defvar python-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior python shell.
Do not set this variable directly, instead use
`python-shell-prompt-set-calculated-regexps'.")

(defvar python-shell--block-prompt nil
  "Input block prompt for inferior python shell.
Do not set this variable directly, instead use
`python-shell-prompt-set-calculated-regexps'.")

(defvar python-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior python shell.
Do not set this variable directly, instead use
`python-shell-set-prompt-regexp'.")

(defun python-shell-prompt-detect ()
  "Detect prompts for the current `python-shell-interpreter'.
When prompts can be retrieved successfully from the
`python-shell-interpreter' run with
`python-shell-interpreter-interactive-arg', returns a list of
three elements, where the first two are input prompts and the
last one is an output prompt.  When no prompts can be detected
and `python-shell-prompt-detect-failure-warning' is non-nil,
shows a warning with instructions to avoid hangs and returns nil.
When `python-shell-prompt-detect-enabled' is nil avoids any
detection and just returns nil."
  (when python-shell-prompt-detect-enabled
    (python-shell-with-environment
      (let* ((code (concat
                    "import sys\n"
                    "ps = [getattr(sys, 'ps%s' % i, '') for i in range(1,4)]\n"
                    ;; JSON is built manually for compatibility
                    "ps_json = '\\n[\"%s\", \"%s\", \"%s\"]\\n' % tuple(ps)\n"
                    "print (ps_json)\n"
                    "sys.exit(0)\n"))
             (interpreter python-shell-interpreter)
             (interpreter-arg python-shell-interpreter-interactive-arg)
             (output
              (with-temp-buffer
                ;; TODO: improve error handling by using
                ;; `condition-case' and displaying the error message to
                ;; the user in the no-prompts warning.
                (ignore-errors
                  (let ((code-file
                         ;; Python 2.x on Windows does not handle
                         ;; carriage returns in unbuffered mode.
                         (let ((inhibit-eol-conversion (getenv "PYTHONUNBUFFERED")))
                           (python-shell--save-temp-file code))))
                    ;; Use `process-file' as it is remote-host friendly.
                    (process-file
                     interpreter
                     code-file
                     '(t nil)
                     nil
                     interpreter-arg)
                    ;; Try to cleanup
                    (delete-file code-file)))
                (buffer-string)))
             (prompts
              (catch 'prompts
                (dolist (line (split-string output "\n" t))
                  (let ((res
                         ;; Check if current line is a valid JSON array
                         (and (string= (substring line 0 2) "[\"")
                              (ignore-errors
                                ;; Return prompts as a list, not vector
                                (append (json-read-from-string line) nil)))))
                    ;; The list must contain 3 strings, where the first
                    ;; is the input prompt, the second is the block
                    ;; prompt and the last one is the output prompt.  The
                    ;; input prompt is the only one that can't be empty.
                    (when (and (= (length res) 3)
                               (cl-every #'stringp res)
                               (not (string= (car res) "")))
                      (throw 'prompts res))))
                nil)))
        (when (and (not prompts)
                   python-shell-prompt-detect-failure-warning)
          (lwarn
           '(python python-shell-prompt-regexp)
           :warning
           (concat
            "Python shell prompts cannot be detected.\n"
            "If your emacs session hangs when starting python shells\n"
            "recover with `keyboard-quit' and then try fixing the\n"
            "interactive flag for your interpreter by adjusting the\n"
            "`python-shell-interpreter-interactive-arg' or add regexps\n"
            "matching shell prompts in the directory-local friendly vars:\n"
            "  + `python-shell-prompt-regexp'\n"
            "  + `python-shell-prompt-block-regexp'\n"
            "  + `python-shell-prompt-output-regexp'\n"
            "Or alternatively in:\n"
            "  + `python-shell-prompt-input-regexps'\n"
            "  + `python-shell-prompt-output-regexps'")))
        prompts))))

(defun python-shell-prompt-validate-regexps ()
  "Validate all user provided regexps for prompts.
Signals `user-error' if any of these vars contain invalid
regexps: `python-shell-prompt-regexp',
`python-shell-prompt-block-regexp',
`python-shell-prompt-pdb-regexp',
`python-shell-prompt-output-regexp',
`python-shell-prompt-input-regexps',
`python-shell-prompt-output-regexps'."
  (dolist (symbol (list 'python-shell-prompt-input-regexps
                        'python-shell-prompt-output-regexps
                        'python-shell-prompt-regexp
                        'python-shell-prompt-block-regexp
                        'python-shell-prompt-pdb-regexp
                        'python-shell-prompt-output-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (python-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in `%s'"
                    regexp symbol)))))

(defun python-shell-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.
Build and set the values for `python-shell-input-prompt-regexp'
and `python-shell-output-prompt-regexp' using the values from
`python-shell-prompt-regexp', `python-shell-prompt-block-regexp',
`python-shell-prompt-pdb-regexp',
`python-shell-prompt-output-regexp',
`python-shell-prompt-input-regexps',
`python-shell-prompt-output-regexps' and detected prompts from
`python-shell-prompt-detect'."
  (when (not (and python-shell--prompt-calculated-input-regexp
                  python-shell--prompt-calculated-output-regexp))
    (let* ((detected-prompts (python-shell-prompt-detect))
           (input-prompts nil)
           (output-prompts nil)
           (build-regexp
            (lambda (prompts)
              (concat "^\\("
                      (mapconcat #'identity
                                 (sort prompts
                                       (lambda (a b)
                                         (let ((length-a (length a))
                                               (length-b (length b)))
                                           (if (= length-a length-b)
                                               (string< a b)
                                             (> (length a) (length b))))))
                                 "\\|")
                      "\\)"))))
      ;; Validate ALL regexps
      (python-shell-prompt-validate-regexps)
      ;; Collect all user defined input prompts
      (dolist (prompt (append python-shell-prompt-input-regexps
                              (list python-shell-prompt-regexp
                                    python-shell-prompt-block-regexp
                                    python-shell-prompt-pdb-regexp)))
        (cl-pushnew prompt input-prompts :test #'string=))
      ;; Collect all user defined output prompts
      (dolist (prompt (cons python-shell-prompt-output-regexp
                            python-shell-prompt-output-regexps))
        (cl-pushnew prompt output-prompts :test #'string=))
      ;; Collect detected prompts if any
      (when detected-prompts
        (dolist (prompt (butlast detected-prompts))
          (setq prompt (regexp-quote prompt))
          (cl-pushnew prompt input-prompts :test #'string=))
        (setq python-shell--block-prompt (nth 1 detected-prompts))
        (cl-pushnew (regexp-quote
                     (car (last detected-prompts)))
                    output-prompts :test #'string=))
      ;; Set input and output prompt regexps from collected prompts
      (setq python-shell--prompt-calculated-input-regexp
            (funcall build-regexp input-prompts)
            python-shell--prompt-calculated-output-regexp
            (funcall build-regexp output-prompts)))))

(defun python-shell-get-process-name (dedicated)
  "Calculate the appropriate process name for inferior Python process.
If DEDICATED is t returns a string with the form
`python-shell-buffer-name'[`buffer-name'] else returns the value
of `python-shell-buffer-name'."
  (if dedicated
      (format "%s[%s]" python-shell-buffer-name (buffer-name))
    python-shell-buffer-name))

(defun python-shell-internal-get-process-name ()
  "Calculate the appropriate process name for Internal Python process.
The name is calculated from `python-shell-global-buffer-name' and
the `buffer-name'."
  (format "%s[%s]" python-shell-internal-buffer-name (buffer-name)))

(defun python-shell-calculate-command ()
  "Calculate the string used to execute the inferior Python process."
  (format "%s %s"
          ;; `python-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          (combine-and-quote-strings (list python-shell-interpreter))
          python-shell-interpreter-args))

(define-obsolete-function-alias
  'python-shell-parse-command
  #'python-shell-calculate-command "25.1")

(defvar python-shell--package-depth 10)

(defun python-shell-package-enable (directory package)
  "Add DIRECTORY parent to $PYTHONPATH and enable PACKAGE."
  (interactive
   (let* ((dir (expand-file-name
                (read-directory-name
                 "Package root: "
                 (file-name-directory
                  (or (buffer-file-name) default-directory)))))
          (name (completing-read
                 "Package: "
                 (python-util-list-packages
                  dir python-shell--package-depth))))
     (list dir name)))
  (python-shell-send-string
   (format
    (concat
     "import os.path;import sys;"
     "sys.path.append(os.path.dirname(os.path.dirname('''%s''')));"
     "__package__ = '''%s''';"
     "import %s")
    directory package package)
   (python-shell-get-process)))

(defun python-shell-accept-process-output (process &optional timeout regexp)
  "Accept PROCESS output with TIMEOUT until REGEXP is found.
Optional argument TIMEOUT is the timeout argument to
`accept-process-output' calls.  Optional argument REGEXP
overrides the regexp to match the end of output, defaults to
`comint-prompt-regexp.'.  Returns non-nil when output was
properly captured.

This utility is useful in situations where the output may be
received in chunks, since `accept-process-output' gives no
guarantees they will be grabbed in a single call.  An example use
case for this would be the CPython shell start-up, where the
banner and the initial prompt are received separately."
  (let ((regexp (or regexp comint-prompt-regexp)))
    (catch 'found
      (while t
        (when (not (accept-process-output process timeout))
          (throw 'found nil))
        (when (looking-back
               regexp (car (python-util-comint-last-prompt)))
          (throw 'found t))))))

(defun python-shell-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT is ends with input prompt."
  (string-match
   ;; XXX: It seems on macOS an extra carriage return is attached
   ;; at the end of output, this handles that too.
   (concat
    "\r?\n?"
    ;; Remove initial caret from calculated regexp
    (replace-regexp-in-string
     (rx string-start ?^) ""
     python-shell--prompt-calculated-input-regexp)
    (rx eos))
   output))

(define-obsolete-function-alias
  'python-comint-output-filter-function
  'ansi-color-filter-apply
  "25.1")

(defun python-comint-postoutput-scroll-to-bottom (output)
  "Faster version of `comint-postoutput-scroll-to-bottom'.
Avoids `recenter' calls until OUTPUT is completely sent."
  (when (and (not (string= "" output))
             (python-shell-comint-end-of-output-p
              (ansi-color-filter-apply output)))
    (comint-postoutput-scroll-to-bottom output))
  output)

(defvar python-shell--parent-buffer nil)

(defmacro python-shell-with-shell-buffer (&rest body)
  "Execute the forms in BODY with the shell buffer temporarily current.
Signals an error if no shell buffer is available for current buffer."
  (declare (indent 0) (debug t))
  (let ((shell-process (make-symbol "shell-process")))
    `(let ((,shell-process (python-shell-get-process-or-error)))
       (with-current-buffer (process-buffer ,shell-process)
         ,@body))))

(defvar python-shell--font-lock-buffer nil)

(defun python-shell-font-lock-get-or-create-buffer ()
  "Get or create a font-lock buffer for current inferior process."
  (python-shell-with-shell-buffer
    (if python-shell--font-lock-buffer
        python-shell--font-lock-buffer
      (let ((process-name
             (process-name (get-buffer-process (current-buffer)))))
        (generate-new-buffer
         (format " *%s-font-lock*" process-name))))))

(defun python-shell-font-lock-kill-buffer ()
  "Kill the font-lock buffer safely."
  (when (and python-shell--font-lock-buffer
             (buffer-live-p python-shell--font-lock-buffer))
    (kill-buffer python-shell--font-lock-buffer)
    (when (derived-mode-p 'inferior-python-mode)
      (setq python-shell--font-lock-buffer nil))))

(defmacro python-shell-font-lock-with-font-lock-buffer (&rest body)
  "Execute the forms in BODY in the font-lock buffer.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'."
  (declare (indent 0) (debug t))
  `(python-shell-with-shell-buffer
     (save-current-buffer
       (when (not (and python-shell--font-lock-buffer
                       (get-buffer python-shell--font-lock-buffer)))
         (setq python-shell--font-lock-buffer
               (python-shell-font-lock-get-or-create-buffer)))
       (set-buffer python-shell--font-lock-buffer)
       (when (not font-lock-mode)
         (font-lock-mode 1))
       (set (make-local-variable 'delay-mode-hooks) t)
       (let ((python-indent-guess-indent-offset nil))
         (when (not (derived-mode-p 'python-mode))
           (python-mode))
         ,@body))))

(defun python-shell-font-lock-cleanup-buffer ()
  "Cleanup the font-lock buffer.
Provided as a command because this might be handy if something
goes wrong and syntax highlighting in the shell gets messed up."
  (interactive)
  (python-shell-with-shell-buffer
    (python-shell-font-lock-with-font-lock-buffer
      (erase-buffer))))

(defun python-shell-font-lock-comint-output-filter-function (output)
  "Clean up the font-lock buffer after any OUTPUT."
  (if (and (not (string= "" output))
           ;; Is end of output and is not just a prompt.
           (not (member
                 (python-shell-comint-end-of-output-p
                  (ansi-color-filter-apply output))
                 '(nil 0))))
      ;; If output is other than an input prompt then "real" output has
      ;; been received and the font-lock buffer must be cleaned up.
      (python-shell-font-lock-cleanup-buffer)
    ;; Otherwise just add a newline.
    (python-shell-font-lock-with-font-lock-buffer
      (goto-char (point-max))
      (newline)))
  output)

(defun python-shell-font-lock-post-command-hook ()
  "Fontifies current line in shell buffer."
  (let ((prompt-end (cdr (python-util-comint-last-prompt))))
    (when (and prompt-end (> (point) prompt-end)
               (process-live-p (get-buffer-process (current-buffer))))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (font-lock-buffer-pos nil)
             (replacement
              (python-shell-font-lock-with-font-lock-buffer
                (delete-region (line-beginning-position)
                               (point-max))
                (setq font-lock-buffer-pos (point))
                (insert input)
                ;; Ensure buffer is fontified, keeping it
                ;; compatible with Emacs < 24.4.
                (if (fboundp 'font-lock-ensure)
                    (funcall 'font-lock-ensure)
                  (font-lock-default-fontify-buffer))
                (buffer-substring font-lock-buffer-pos
                                  (point-max))))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (not (= i replacement-length))
          (let* ((plist (text-properties-at i replacement))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (plist (let ((face (plist-get plist 'face)))
                          (if (not face)
                              plist
                            ;; Replace FACE text properties with
                            ;; FONT-LOCK-FACE so input is fontified.
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face)))))
            (set-text-properties
             (+ start-pos i) (+ start-pos next-change) plist)
            (setq i next-change)))))))

(defun python-shell-font-lock-turn-on (&optional msg)
  "Turn on shell font-lock.
With argument MSG show activation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (python-shell-font-lock-kill-buffer)
    (set (make-local-variable 'python-shell--font-lock-buffer) nil)
    (add-hook 'post-command-hook
              #'python-shell-font-lock-post-command-hook nil 'local)
    (add-hook 'kill-buffer-hook
              #'python-shell-font-lock-kill-buffer nil 'local)
    (add-hook 'comint-output-filter-functions
              #'python-shell-font-lock-comint-output-filter-function
              'append 'local)
    (when msg
      (message "Shell font-lock is enabled"))))

(defun python-shell-font-lock-turn-off (&optional msg)
  "Turn off shell font-lock.
With argument MSG show deactivation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (python-shell-font-lock-kill-buffer)
    (when (python-util-comint-last-prompt)
      ;; Cleanup current fontification
      (remove-text-properties
       (cdr (python-util-comint-last-prompt))
       (line-end-position)
       '(face nil font-lock-face nil)))
    (set (make-local-variable 'python-shell--font-lock-buffer) nil)
    (remove-hook 'post-command-hook
                 #'python-shell-font-lock-post-command-hook 'local)
    (remove-hook 'kill-buffer-hook
                 #'python-shell-font-lock-kill-buffer 'local)
    (remove-hook 'comint-output-filter-functions
                 #'python-shell-font-lock-comint-output-filter-function
                 'local)
    (when msg
      (message "Shell font-lock is disabled"))))

(defun python-shell-font-lock-toggle (&optional msg)
  "Toggle font-lock for shell.
With argument MSG show activation/deactivation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (set (make-local-variable 'python-shell-font-lock-enable)
         (not python-shell-font-lock-enable))
    (if python-shell-font-lock-enable
        (python-shell-font-lock-turn-on msg)
      (python-shell-font-lock-turn-off msg))
    python-shell-font-lock-enable))

(defvar python-shell--first-prompt-received-output-buffer nil)
(defvar python-shell--first-prompt-received nil)

(defcustom python-shell-first-prompt-hook nil
  "Hook run upon first (non-pdb) shell prompt detection.
This is the place for shell setup functions that need to wait for
output.  Since the first prompt is ensured, this helps the
current process to not hang waiting for output by safeguarding
interactive actions can be performed.  This is useful to safely
attach setup code for long-running processes that eventually
provide a shell."
  :version "25.1"
  :type 'hook
  :group 'python)

(defun python-shell-comint-watch-for-first-prompt-output-filter (output)
  "Run `python-shell-first-prompt-hook' when first prompt is found in OUTPUT."
  (when (not python-shell--first-prompt-received)
    (set (make-local-variable 'python-shell--first-prompt-received-output-buffer)
         (concat python-shell--first-prompt-received-output-buffer
                 (ansi-color-filter-apply output)))
    (when (python-shell-comint-end-of-output-p
           python-shell--first-prompt-received-output-buffer)
      (if (string-match-p
           (concat python-shell-prompt-pdb-regexp (rx eos))
           (or python-shell--first-prompt-received-output-buffer ""))
          ;; Skip pdb prompts and reset the buffer.
          (setq python-shell--first-prompt-received-output-buffer nil)
        (set (make-local-variable 'python-shell--first-prompt-received) t)
        (setq python-shell--first-prompt-received-output-buffer nil)
        (with-current-buffer (current-buffer)
          (let ((inhibit-quit nil))
            (run-hooks 'python-shell-first-prompt-hook))))))
  output)

;; Used to hold user interactive overrides to
;; `python-shell-interpreter' and `python-shell-interpreter-args' that
;; will be made buffer-local by `inferior-python-mode':
(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for Python inferior process.
Runs a Python interpreter as a subprocess of Emacs, with Python
I/O through an Emacs buffer.  Variables `python-shell-interpreter'
and `python-shell-interpreter-args' control which Python
interpreter is run.  Variables
`python-shell-prompt-regexp',
`python-shell-prompt-output-regexp',
`python-shell-prompt-block-regexp',
`python-shell-font-lock-enable',
`python-shell-completion-setup-code',
`python-shell-completion-string-code',
`python-eldoc-setup-code', `python-eldoc-string-code',
`python-ffap-setup-code' and `python-ffap-string-code' can
customize this mode for different Python interpreters.

This mode resets `comint-output-filter-functions' locally, so you
may want to re-add custom functions to it using the
`inferior-python-mode-hook'.

You can also add additional setup code to be run at
initialization of the interpreter via `python-shell-setup-codes'
variable.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (when python-shell--parent-buffer
    (python-util-clone-local-variables python-shell--parent-buffer))
  (set (make-local-variable 'indent-tabs-mode) nil)
  ;; Users can interactively override default values for
  ;; `python-shell-interpreter' and `python-shell-interpreter-args'
  ;; when calling `run-python'.  This ensures values let-bound in
  ;; `python-shell-make-comint' are locally set if needed.
  (set (make-local-variable 'python-shell-interpreter)
       (or python-shell--interpreter python-shell-interpreter))
  (set (make-local-variable 'python-shell-interpreter-args)
       (or python-shell--interpreter-args python-shell-interpreter-args))
  (set (make-local-variable 'python-shell--prompt-calculated-input-regexp) nil)
  (set (make-local-variable 'python-shell--block-prompt) nil)
  (set (make-local-variable 'python-shell--prompt-calculated-output-regexp) nil)
  (python-shell-prompt-set-calculated-regexps)
  (setq comint-prompt-regexp python-shell--prompt-calculated-input-regexp)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         python-shell-comint-watch-for-first-prompt-output-filter
         python-pdbtrack-comint-output-filter-function
         python-comint-postoutput-scroll-to-bottom
         comint-watch-for-password-prompt))
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-shell-compilation-regexp-alist)
  (add-hook 'completion-at-point-functions
            #'python-shell-completion-at-point nil 'local)
  (define-key inferior-python-mode-map "\t"
    'python-shell-completion-complete-or-indent)
  (make-local-variable 'python-pdbtrack-buffers-to-kill)
  (make-local-variable 'python-pdbtrack-tracked-buffer)
  (make-local-variable 'python-shell-internal-last-output)
  (when python-shell-font-lock-enable
    (python-shell-font-lock-turn-on))
  (compilation-shell-minor-mode 1))

(defun python-shell-make-comint (cmd proc-name &optional show internal)
  "Create a Python shell comint buffer.
CMD is the Python command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `inferior-python-mode' is activated.  When
optional argument SHOW is non-nil the buffer is shown.  When
optional argument INTERNAL is non-nil this process is run on a
buffer with a name that starts with a space, following the Emacs
convention for temporary/internal buffers, and also makes sure
the user is not queried for confirmation when the process is
killed."
  (save-excursion
    (python-shell-with-environment
      (let* ((proc-buffer-name
              (format (if (not internal) "*%s*" " *%s*") proc-name)))
        (when (not (comint-check-proc proc-buffer-name))
          (let* ((cmdlist (split-string-and-unquote cmd))
                 (interpreter (car cmdlist))
                 (args (cdr cmdlist))
                 (buffer (apply #'make-comint-in-buffer proc-name proc-buffer-name
                                interpreter nil args))
                 (python-shell--parent-buffer (current-buffer))
                 (process (get-buffer-process buffer))
                 ;; Users can override the interpreter and args
                 ;; interactively when calling `run-python', let-binding
                 ;; these allows having the new right values in all
                 ;; setup code that is done in `inferior-python-mode',
                 ;; which is important, especially for prompt detection.
                 (python-shell--interpreter interpreter)
                 (python-shell--interpreter-args
                  (mapconcat #'identity args " ")))
            (with-current-buffer buffer
              (inferior-python-mode))
            (when show (display-buffer buffer))
            (and internal (set-process-query-on-exit-flag process nil))))
        proc-buffer-name))))

;;;###autoload
(defun run-python (&optional cmd dedicated show)
  "Run an inferior Python process.

Argument CMD defaults to `python-shell-calculate-command' return
value.  When called interactively with `prefix-arg', it allows
the user to edit such value and choose whether the interpreter
should be DEDICATED for the current buffer.  When numeric prefix
arg is other than 0 or 4 do not SHOW.

For a given buffer and same values of DEDICATED, if a process is
already running for it, it will do nothing.  This means that if
the current buffer is using a global process, the user is still
able to switch it to use a dedicated one.

Runs the hook `inferior-python-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Python: " (python-shell-calculate-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (python-shell-calculate-command) nil t)))
  (get-buffer-process
   (python-shell-make-comint
    (or cmd (python-shell-calculate-command))
    (python-shell-get-process-name dedicated) show)))

(defun run-python-internal ()
  "Run an inferior Internal Python process.
Input and output via buffer named after
`python-shell-internal-buffer-name' and what
`python-shell-internal-get-process-name' returns.

This new kind of shell is intended to be used for generic
communication related to defined configurations; the main
difference with global or dedicated shells is that these ones are
attached to a configuration, not a buffer.  This means that can
be used for example to retrieve the sys.path and other stuff,
without messing with user shells.  Note that
`python-shell-font-lock-enable' and `inferior-python-mode-hook'
are set to nil for these shells, so setup codes are not sent at
startup."
  (let ((python-shell-font-lock-enable nil)
        (inferior-python-mode-hook nil))
    (get-buffer-process
     (python-shell-make-comint
      (python-shell-calculate-command)
      (python-shell-internal-get-process-name) nil t))))

(defun python-shell-get-buffer ()
  "Return inferior Python buffer for current buffer.
If current buffer is in `inferior-python-mode', return it."
  (if (derived-mode-p 'inferior-python-mode)
      (current-buffer)
    (let* ((dedicated-proc-name (python-shell-get-process-name t))
           (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
           (global-proc-name  (python-shell-get-process-name nil))
           (global-proc-buffer-name (format "*%s*" global-proc-name))
           (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
           (global-running (comint-check-proc global-proc-buffer-name)))
      ;; Always prefer dedicated
      (or (and dedicated-running dedicated-proc-buffer-name)
          (and global-running global-proc-buffer-name)))))

(defun python-shell-get-process ()
  "Return inferior Python process for current buffer."
  (get-buffer-process (python-shell-get-buffer)))

(defun python-shell-get-process-or-error (&optional interactivep)
  "Return inferior Python process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (python-shell-get-process)
      (if interactivep
          (user-error
           "Start a Python process first with `M-x run-python' or `%s'."
           ;; Get the binding.
           (key-description
            (where-is-internal
             #'run-python overriding-local-map t)))
        (error
         "No inferior Python process running."))))

(defun python-shell-get-or-create-process (&optional cmd dedicated show)
  "Get or create an inferior Python process for current buffer and return it.
Arguments CMD, DEDICATED and SHOW are those of `run-python' and
are used to start the shell.  If those arguments are not
provided, `run-python' is called interactively and the user will
be asked for their values."
  (let ((shell-process (python-shell-get-process)))
    (when (not shell-process)
      (if (not cmd)
          ;; XXX: Refactor code such that calling `run-python'
          ;; interactively is not needed anymore.
          (call-interactively 'run-python)
        (run-python cmd dedicated show)))
    (or shell-process (python-shell-get-process))))

(make-obsolete
 #'python-shell-get-or-create-process
 "Instead call `python-shell-get-process' and create one if returns nil."
 "25.1")

(defvar python-shell-internal-buffer nil
  "Current internal shell buffer for the current buffer.
This is really not necessary at all for the code to work but it's
there for compatibility with CEDET.")

(defvar python-shell-internal-last-output nil
  "Last output captured by the internal shell.
This is really not necessary at all for the code to work but it's
there for compatibility with CEDET.")

(defun python-shell-internal-get-or-create-process ()
  "Get or create an inferior Internal Python process."
  (let ((proc-name (python-shell-internal-get-process-name)))
    (if (process-live-p proc-name)
        (get-process proc-name)
      (run-python-internal))))

(define-obsolete-function-alias
  'python-proc 'python-shell-internal-get-or-create-process "24.3")

(define-obsolete-variable-alias
  'python-buffer 'python-shell-internal-buffer "24.3")

(define-obsolete-variable-alias
  'python-preoutput-result 'python-shell-internal-last-output "24.3")

(defun python-shell--save-temp-file (string)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write (python-info-encoding)))
    (with-temp-file temp-file-name
      (insert string)
      (delete-trailing-whitespace))
    temp-file-name))

(defun python-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (python-shell-get-process-or-error msg))))
    (if (string-match ".\n+." string)   ;Multiline.
        (let* ((temp-file-name (python-shell--save-temp-file string))
               (file-name (or (buffer-file-name) temp-file-name)))
          (python-shell-send-file file-name process temp-file-name t))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

(defvar python-shell-output-filter-in-progress nil)
(defvar python-shell-output-filter-buffer nil)

(defun python-shell-output-filter (string)
  "Filter used in `python-shell-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`python-shell-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   python-shell-output-filter-buffer
   (concat python-shell-output-filter-buffer string))
  (when (python-shell-comint-end-of-output-p
         python-shell-output-filter-buffer)
    ;; Output ends when `python-shell-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq python-shell-output-filter-in-progress nil
          python-shell-output-filter-buffer
          (substring python-shell-output-filter-buffer
                     0 (match-beginning 0)))
    (when (string-match
           python-shell--prompt-calculated-output-regexp
           python-shell-output-filter-buffer)
      ;; Some shells, like IPython might append a prompt before the
      ;; output, clean that.
      (setq python-shell-output-filter-buffer
            (substring python-shell-output-filter-buffer (match-end 0)))))
  "")

(defun python-shell-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let ((process (or process (python-shell-get-process-or-error)))
        (comint-preoutput-filter-functions
         '(python-shell-output-filter))
        (python-shell-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (python-shell-send-string string process)
       (while python-shell-output-filter-in-progress
         ;; `python-shell-output-filter' takes care of setting
         ;; `python-shell-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output process))
       (prog1
           python-shell-output-filter-buffer
         (setq python-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer process)
       (comint-interrupt-subjob)))))

(defun python-shell-internal-send-string (string)
  "Send STRING to the Internal Python interpreter.
Returns the output.  See `python-shell-send-string-no-output'."
  ;; XXX Remove `python-shell-internal-last-output' once CEDET is
  ;; updated to support this new mode.
  (setq python-shell-internal-last-output
        (python-shell-send-string-no-output
         ;; Makes this function compatible with the old
         ;; python-send-receive. (At least for CEDET).
         (replace-regexp-in-string "_emacs_out +" "" string)
         (python-shell-internal-get-or-create-process))))

(define-obsolete-function-alias
  'python-send-receive 'python-shell-internal-send-string "24.3")

(define-obsolete-function-alias
  'python-send-string 'python-shell-internal-send-string "24.3")

(defun python-shell-buffer-substring (start end &optional nomain)
  "Send buffer substring from START to END formatted for shell.
This is a wrapper over `buffer-substring' that takes care of
different transformations for the code sent to be evaluated in
the python shell:
  1. When optional argument NOMAIN is non-nil everything under an
     \"if __name__ == \\='__main__\\='\" block will be removed.
  2. When a subregion of the buffer is sent, it takes care of
     appending extra empty lines so tracebacks are correct.
  3. When the region sent is a substring of the current buffer, a
     coding cookie is added.
  4. Wraps indented regions under an \"if True:\" block so the
     interpreter evaluates them correctly."
  (let* ((start (save-excursion
                  ;; Normalize start to the line beginning position.
                  (goto-char start)
                  (line-beginning-position)))
         (substring (buffer-substring-no-properties start end))
         (starts-at-point-min-p (save-restriction
                                  (widen)
                                  (= (point-min) start)))
         (encoding (python-info-encoding))
         (toplevel-p (zerop (save-excursion
                              (goto-char start)
                              (python-util-forward-comment 1)
                              (current-indentation))))
         (fillstr (when (not starts-at-point-min-p)
                    (concat
                     (format "# -*- coding: %s -*-\n" encoding)
                     (make-string
                      ;; Subtract 2 because of the coding cookie.
                      (- (line-number-at-pos start) 2) ?\n)))))
    (with-temp-buffer
      (python-mode)
      (when fillstr
        (insert fillstr))
      (insert substring)
      (goto-char (point-min))
      (when (not toplevel-p)
        (insert "if True:")
        (delete-region (point) (line-end-position)))
      (when nomain
        (let* ((if-name-main-start-end
                (and nomain
                     (save-excursion
                       (when (python-nav-if-name-main)
                         (cons (point)
                               (progn (python-nav-forward-sexp-safe)
                                      ;; Include ending newline
                                      (forward-line 1)
                                      (point)))))))
               ;; Oh destructuring bind, how I miss you.
               (if-name-main-start (car if-name-main-start-end))
               (if-name-main-end (cdr if-name-main-start-end))
               (fillstr (make-string
                         (- (line-number-at-pos if-name-main-end)
                            (line-number-at-pos if-name-main-start)) ?\n)))
          (when if-name-main-start-end
            (goto-char if-name-main-start)
            (delete-region if-name-main-start if-name-main-end)
            (insert fillstr))))
      ;; Ensure there's only one coding cookie in the generated string.
      (goto-char (point-min))
      (when (looking-at-p (python-rx coding-cookie))
        (forward-line 1)
        (when (looking-at-p (python-rx coding-cookie))
          (delete-region
           (line-beginning-position) (line-end-position))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun python-shell-send-region (start end &optional send-main msg)
  "Send the region delimited by START and END to inferior Python process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument.  When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (let* ((string (python-shell-buffer-substring start end (not send-main)))
         (process (python-shell-get-process-or-error msg))
         (original-string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    (message "Sent: %s..." (match-string 1 original-string))
    (python-shell-send-string string process)))

(defun python-shell-send-buffer (&optional send-main msg)
  "Send the entire buffer to inferior Python process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument.  When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive (list current-prefix-arg t))
  (save-restriction
    (widen)
    (python-shell-send-region (point-min) (point-max) send-main msg)))

(defun python-shell-send-defun (&optional arg msg)
  "Send the current defun to inferior Python process.
When argument ARG is non-nil do not include decorators.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list current-prefix-arg t))
  (save-excursion
    (python-shell-send-region
     (progn
       (end-of-line 1)
       (while (and (or (python-nav-beginning-of-defun)
                       (beginning-of-line 1))
                   (> (current-indentation) 0)))
       (when (not arg)
         (while (and (forward-line -1)
                     (looking-at (python-rx decorator))))
         (forward-line 1))
       (point-marker))
     (progn
       (or (python-nav-end-of-defun)
           (end-of-line 1))
       (point-marker))
     nil  ;; noop
     msg)))

(defun python-shell-send-file (file-name &optional process temp-file-name
                                         delete msg)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; temp-file-name
    nil                                 ; delete
    t))                                 ; msg
  (let* ((process (or process (python-shell-get-process-or-error msg)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (python-info-encoding)))
         (file-name (expand-file-name (file-local-name file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (file-local-name temp-file-name)))))
    (python-shell-send-string
     (format
      (concat
       "import codecs, os;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "exec(compile(__code, '''%s''', 'exec'));")
      (or temp-file-name file-name) encoding encoding file-name)
     process)))

(defun python-shell-switch-to-shell (&optional msg)
  "Switch to inferior Python process buffer.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive "p")
  (pop-to-buffer
   (process-buffer (python-shell-get-process-or-error msg)) nil t))

(defun python-shell-send-setup-code ()
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`python-shell-setup-codes' list."
  (when python-shell-setup-codes
    (let ((process (python-shell-get-process))
          (code (concat
                 (mapconcat
                  (lambda (elt)
                    (cond ((stringp elt) elt)
                          ((symbolp elt) (symbol-value elt))
                          (t "")))
                  python-shell-setup-codes
                  "\n\nprint ('python.el: sent setup code')"))))
      (python-shell-send-string code process)
      (python-shell-accept-process-output process))))

(add-hook 'python-shell-first-prompt-hook
          #'python-shell-send-setup-code)


;;; Shell completion

(defcustom python-shell-completion-setup-code
  "
def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        elif is_ipython and '__IP' in builtins:
            completions = __IP.complete(text)
        elif is_ipython and 'get_ipython' in builtins:
            completions = get_ipython().Completer.all_completions(text)
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = readline.get_completer()
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
  :group 'python)

(defcustom python-shell-completion-string-code
  "';'.join(__PYTHON_EL_get_completions('''%s'''))"
  "Python code used to get a string of completions separated by semicolons.
The string passed to the function is the current python name or
the full statement in the case of imports."
  :type 'string
  :group 'python)

(define-obsolete-variable-alias
  'python-shell-completion-module-string-code
  'python-shell-completion-string-code
  "24.4"
  "Completion string code must also autocomplete modules.")

(define-obsolete-variable-alias
  'python-shell-completion-pdb-string-code
  'python-shell-completion-string-code
  "25.1"
  "Completion string code must work for (i)pdb.")

(defcustom python-shell-completion-native-disabled-interpreters
  ;; PyPy's readline cannot handle some escape sequences yet.  Native
  ;; completion was found to be non-functional for IPython (see
  ;; Bug#25067).  Native completion doesn't work on w32 (Bug#28580).
  (if (eq system-type 'windows-nt) '("")
    '("pypy" "ipython"))
  "List of disabled interpreters.
When a match is found, native completion is disabled."
  :version "25.1"
  :type '(repeat string))

(defcustom python-shell-completion-native-enable t
  "Enable readline based native completion."
  :version "25.1"
  :type 'boolean)

(defcustom python-shell-completion-native-output-timeout 5.0
  "Time in seconds to wait for completion output before giving up."
  :version "25.1"
  :type 'float)

(defcustom python-shell-completion-native-try-output-timeout 1.0
  "Time in seconds to wait for *trying* native completion output."
  :version "25.1"
  :type 'float)

(defvar python-shell-completion-native-redirect-buffer
  " *Python completions redirect*"
  "Buffer to be used to redirect output of readline commands.")

(defun python-shell-completion-native-interpreter-disabled-p ()
  "Return non-nil if interpreter has native completion disabled."
  (when python-shell-completion-native-disabled-interpreters
    (string-match
     (regexp-opt python-shell-completion-native-disabled-interpreters)
     (file-name-nondirectory python-shell-interpreter))))

(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (let ((python-shell-completion-native-enable t)
        (python-shell-completion-native-output-timeout
         python-shell-completion-native-try-output-timeout))
    (python-shell-completion-native-get-completions
     (get-buffer-process (current-buffer))
     nil "_")))

(defun python-shell-completion-native-setup ()
  "Try to setup native completion, return non-nil on success."
  (let ((process (python-shell-get-process)))
    (with-current-buffer (process-buffer process)
      (python-shell-send-string "
def __PYTHON_EL_native_completion_setup():
    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__

        builtins = dir(__builtin__)
        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)

        class __PYTHON_EL_Completer:
            '''Completer wrapper that prints candidates to stdout.

            It wraps an existing completer function and changes its behavior so
            that the user input is unchanged and real candidates are printed to
            stdout.

            Returned candidates are '0__dummy_completion__' and
            '1__dummy_completion__' in that order ('0__dummy_completion__' is
            returned repeatedly until all possible candidates are consumed).

            The real candidates are printed to stdout so that they can be
            easily retrieved through comint output redirect trickery.
            '''

            PYTHON_EL_WRAPPED = True

            def __init__(self, completer):
                self.completer = completer
                self.last_completion = None
                self.print_mode = True

            def __call__(self, text, state):
                if state == 0:
                    # Set the first dummy completion.
                    self.last_completion = None
                    completion = '0__dummy_completion__'
                else:
                    completion = self.completer(text, state - 1)

                if not completion:
                    if self.last_completion != '1__dummy_completion__':
                        # When no more completions are available, returning a
                        # dummy with non-sharing prefix allow ensuring output
                        # while preventing changes to current input.
                        # Coincidentally it's also the end of output.
                        completion = '1__dummy_completion__'
                elif completion.endswith('('):
                    # Remove parens on callables as it breaks completion on
                    # arguments (e.g. str(Ari<tab>)).
                    completion = completion[:-1]
                self.last_completion = completion

                if completion in (
                        '0__dummy_completion__', '1__dummy_completion__'):
                    return completion
                elif completion:
                    # For every non-dummy completion, return a repeated dummy
                    # one and print the real candidate so it can be retrieved
                    # by comint output filters.
                    if self.print_mode:
                        print (completion)
                        return '0__dummy_completion__'
                    else:
                        return completion
                else:
                    return completion

        completer = readline.get_completer()

        if not completer:
            # Used as last resort to avoid breaking customizations.
            import rlcompleter
            completer = readline.get_completer()

        if completer and not getattr(completer, 'PYTHON_EL_WRAPPED', False):
            # Wrap the existing completer function only once.
            new_completer = __PYTHON_EL_Completer(completer)
            if not is_ipython:
                readline.set_completer(new_completer)
            else:
                # Try both initializations to cope with all IPython versions.
                # This works fine for IPython 3.x but not for earlier:
                readline.set_completer(new_completer)
                # IPython<3 hacks readline such that `readline.set_completer`
                # won't work.  This workaround injects the new completer
                # function into the existing instance directly:
                instance = getattr(completer, 'im_self', completer.__self__)
                instance.rlcomplete = new_completer

        if readline.__doc__ and 'libedit' in readline.__doc__:
            raise Exception('''libedit based readline is known not to work,
      see etc/PROBLEMS under \"In Inferior Python mode, input is echoed\".''')
            readline.parse_and_bind('bind ^I rl_complete')
        else:
            readline.parse_and_bind('tab: complete')
            # Require just one tab to send output.
            readline.parse_and_bind('set show-all-if-ambiguous on')

        print ('python.el: native completion setup loaded')
    except:
        import sys
        print ('python.el: native completion setup failed, %s: %s'
               % sys.exc_info()[:2])

__PYTHON_EL_native_completion_setup()" process)
      (when (and
             (python-shell-accept-process-output
              process python-shell-completion-native-try-output-timeout)
             (save-excursion
               (re-search-backward
                (regexp-quote "python.el: native completion setup loaded") nil t 1)))
        (python-shell-completion-native-try)))))

(defun python-shell-completion-native-turn-off (&optional msg)
  "Turn off shell native completions.
With argument MSG show deactivation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (set (make-local-variable 'python-shell-completion-native-enable) nil)
    (when msg
      (message "Shell native completion is disabled, using fallback"))))

(defun python-shell-completion-native-turn-on (&optional msg)
  "Turn on shell native completions.
With argument MSG show deactivation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (set (make-local-variable 'python-shell-completion-native-enable) t)
    (python-shell-completion-native-turn-on-maybe msg)))

(defun python-shell-completion-native-turn-on-maybe (&optional msg)
  "Turn on native completions if enabled and available.
With argument MSG show activation/deactivation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (when python-shell-completion-native-enable
      (cond
       ((python-shell-completion-native-interpreter-disabled-p)
        (python-shell-completion-native-turn-off msg))
       ((python-shell-completion-native-setup)
        (when msg
          (message "Shell native completion is enabled.")))
       (t (lwarn
           '(python python-shell-completion-native-turn-on-maybe)
           :warning
           (concat
            "Your `python-shell-interpreter' doesn't seem to "
            "support readline, yet `python-shell-completion-native-enable' "
            (format "was t and %S is not part of the "
                    (file-name-nondirectory python-shell-interpreter))
            "`python-shell-completion-native-disabled-interpreters' "
            "list.  Native completions have been disabled locally. "))
          (python-shell-completion-native-turn-off msg))))))

(defun python-shell-completion-native-turn-on-maybe-with-msg ()
  "Like `python-shell-completion-native-turn-on-maybe' but force messages."
  (python-shell-completion-native-turn-on-maybe t))

(add-hook 'python-shell-first-prompt-hook
          #'python-shell-completion-native-turn-on-maybe-with-msg)

(defun python-shell-completion-native-toggle (&optional msg)
  "Toggle shell native completion.
With argument MSG show activation/deactivation message."
  (interactive "p")
  (python-shell-with-shell-buffer
    (if python-shell-completion-native-enable
        (python-shell-completion-native-turn-off msg)
      (python-shell-completion-native-turn-on msg))
    python-shell-completion-native-enable))

(defun python-shell-completion-native-get-completions (process import input)
  "Get completions using native readline for PROCESS.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (with-current-buffer (process-buffer process)
    (let* ((input (or import input))
           (original-filter-fn (process-filter process))
           (redirect-buffer (get-buffer-create
                             python-shell-completion-native-redirect-buffer))
           (trigger "\t")
           (new-input (concat input trigger))
           (input-length
            (save-excursion
              (+ (- (point-max) (comint-bol)) (length new-input))))
           (delete-line-command (make-string input-length ?\b))
           (input-to-send (concat new-input delete-line-command)))
      ;; Ensure restoring the process filter, even if the user quits
      ;; or there's some other error.
      (unwind-protect
          (with-current-buffer redirect-buffer
            ;; Cleanup the redirect buffer
            (erase-buffer)
            ;; Mimic `comint-redirect-send-command', unfortunately it
            ;; can't be used here because it expects a newline in the
            ;; command and that's exactly what we are trying to avoid.
            (let ((comint-redirect-echo-input nil)
                  (comint-redirect-completed nil)
                  (comint-redirect-perform-sanity-check nil)
                  (comint-redirect-insert-matching-regexp t)
                  (comint-redirect-finished-regexp
                   "1__dummy_completion__[[:space:]]*\n")
                  (comint-redirect-output-buffer redirect-buffer))
              ;; Compatibility with Emacs 24.x.  Comint changed and
              ;; now `comint-redirect-filter' gets 3 args.  This
              ;; checks which version of `comint-redirect-filter' is
              ;; in use based on its args and uses `apply-partially'
              ;; to make it up for the 3 args case.
              (if (= (length
                      (help-function-arglist 'comint-redirect-filter)) 3)
                  (set-process-filter
                   process (apply-partially
                            #'comint-redirect-filter original-filter-fn))
                (set-process-filter process #'comint-redirect-filter))
              (process-send-string process input-to-send)
              ;; Grab output until our dummy completion used as
              ;; output end marker is found.
              (when (python-shell-accept-process-output
                     process python-shell-completion-native-output-timeout
                     comint-redirect-finished-regexp)
                (re-search-backward "0__dummy_completion__" nil t)
                (cl-remove-duplicates
                 (split-string
                  (buffer-substring-no-properties
                   (line-beginning-position) (point-min))
                  "[ \f\t\n\r\v()]+" t)
                 :test #'string=))))
        (set-process-filter process original-filter-fn)))))

(defun python-shell-completion-get-completions (process import input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (setq input (or import input))
  (with-current-buffer (process-buffer process)
    (let ((completions
           (python-util-strip-string
            (python-shell-send-string-no-output
             (format
              (concat python-shell-completion-setup-code
                      "\nprint (" python-shell-completion-string-code ")")
              input) process))))
      (when (> (length completions) 2)
        (split-string completions
                      "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun python-shell-completion-at-point (&optional process)
  "Function for `completion-at-point-functions' in `inferior-python-mode'.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  (setq process (or process (get-buffer-process (current-buffer))))
  (let* ((line-start (if (derived-mode-p 'inferior-python-mode)
                         ;; Working on a shell buffer: use prompt end.
                         (cdr (python-util-comint-last-prompt))
                       (line-beginning-position)))
         (import-statement
          (when (string-match-p
                 (rx (* space) word-start (or "from" "import") word-end space)
                 (buffer-substring-no-properties line-start (point)))
            (buffer-substring-no-properties line-start (point))))
         (start
          (save-excursion
            (if (not (re-search-backward
                      (python-rx
                       (or whitespace open-paren close-paren string-delimiter))
                      line-start
                      t 1))
                line-start
              (forward-char (length (match-string-no-properties 0)))
              (point))))
         (end (point))
         (prompt-boundaries
          (with-current-buffer (process-buffer process)
            (python-util-comint-last-prompt)))
         (prompt
          (with-current-buffer (process-buffer process)
            (when prompt-boundaries
              (buffer-substring-no-properties
               (car prompt-boundaries) (cdr prompt-boundaries)))))
         (completion-fn
          (with-current-buffer (process-buffer process)
            (cond ((or (null prompt)
                       (< (point) (cdr prompt-boundaries)))
                   #'ignore)
                  ((or (not python-shell-completion-native-enable)
                       ;; Even if native completion is enabled, for
                       ;; pdb interaction always use the fallback
                       ;; mechanism since the completer is changed.
                       ;; Also, since pdb interaction is single-line
                       ;; based, this is enough.
                       (string-match-p python-shell-prompt-pdb-regexp prompt))
                   (if (or (equal python-shell--block-prompt prompt)
                           (string-match-p
                            python-shell-prompt-block-regexp prompt))
                       ;; The non-native completion mechanism sends
                       ;; newlines to the interpreter, so we can't use
                       ;; it during a multiline statement (Bug#28051).
                       #'ignore
                     #'python-shell-completion-get-completions))
                  (t #'python-shell-completion-native-get-completions)))))
    (list start end
          (completion-table-dynamic
           (apply-partially
            completion-fn
            process import-statement)))))

(define-obsolete-function-alias
  'python-shell-completion-complete-at-point
  'python-shell-completion-at-point
  "25.1")

(defun python-shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace, indent.
If not try to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point)))
      (indent-for-tab-command)
    (completion-at-point)))


;;; PDB Track integration

(defcustom python-pdbtrack-activate t
  "Non-nil makes Python shell enable pdbtracking."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-pdbtrack-stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :group 'python
  :safe 'stringp)

(defvar python-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`python-pdbtrack-set-tracked-buffer' instead.")

(defvar python-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defun python-pdbtrack-set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the `python-pdbtrack-tracked-buffer' variable.
Returns the tracked buffer."
  (let* ((file-name-prospect (concat (file-remote-p default-directory)
                              file-name))
         (file-buffer (get-file-buffer file-name-prospect)))
    (if file-buffer
        (setq python-pdbtrack-tracked-buffer file-buffer)
      (cond
       ((file-exists-p file-name-prospect)
        (setq file-buffer (find-file-noselect file-name-prospect)))
       ((and (not (equal file-name file-name-prospect))
             (file-exists-p file-name))
        ;; Fallback to a locally available copy of the file.
        (setq file-buffer (find-file-noselect file-name-prospect))))
      (when (not (member file-buffer python-pdbtrack-buffers-to-kill))
        (add-to-list 'python-pdbtrack-buffers-to-kill file-buffer)))
    file-buffer))

(defun python-pdbtrack-comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and python-pdbtrack-activate (not (string= output "")))
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (line-number)
           (file-name
            (with-temp-buffer
              (insert full-output)
              ;; When the debugger encounters a pdb.set_trace()
              ;; command, it prints a single stack frame.  Sometimes
              ;; it prints a bit of extra information about the
              ;; arguments of the present function.  When ipdb
              ;; encounters an exception, it prints the _entire_ stack
              ;; trace.  To handle all of these cases, we want to find
              ;; the _last_ stack frame printed in the most recent
              ;; batch of output, then jump to the corresponding
              ;; file/line number.
              (goto-char (point-max))
              (when (re-search-backward python-pdbtrack-stacktrace-info-regexp nil t)
                (setq line-number (string-to-number
                                   (match-string-no-properties 2)))
                (match-string-no-properties 1)))))
      (if (and file-name line-number)
          (let* ((tracked-buffer
                  (python-pdbtrack-set-tracked-buffer file-name))
                 (shell-buffer (current-buffer))
                 (tracked-buffer-window (get-buffer-window tracked-buffer))
                 (tracked-buffer-line-pos))
            (with-current-buffer tracked-buffer
              (set (make-local-variable 'overlay-arrow-string) "=>")
              (set (make-local-variable 'overlay-arrow-position) (make-marker))
              (setq tracked-buffer-line-pos (progn
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (point-marker)))
              (when tracked-buffer-window
                (set-window-point
                 tracked-buffer-window tracked-buffer-line-pos))
              (set-marker overlay-arrow-position tracked-buffer-line-pos))
            (pop-to-buffer tracked-buffer)
            (switch-to-buffer-other-window shell-buffer))
        (when python-pdbtrack-tracked-buffer
          (with-current-buffer python-pdbtrack-tracked-buffer
            (set-marker overlay-arrow-position nil))
          (mapc #'(lambda (buffer)
                    (ignore-errors (kill-buffer buffer)))
                python-pdbtrack-buffers-to-kill)
          (setq python-pdbtrack-tracked-buffer nil
                python-pdbtrack-buffers-to-kill nil)))))
  output)


;;; Symbol completion

(defun python-completion-at-point ()
  "Function for `completion-at-point-functions' in `python-mode'.
For this to work as best as possible you should call
`python-shell-send-buffer' from time to time so context in
inferior Python process is updated properly."
  (let ((process (python-shell-get-process)))
    (when process
      (python-shell-completion-at-point process))))

(define-obsolete-function-alias
  'python-completion-complete-at-point
  'python-completion-at-point
  "25.1")


;;; Fill paragraph

(defcustom python-fill-comment-function 'python-fill-comment
  "Function to fill comments.
This is the function used by `python-fill-paragraph' to
fill comments."
  :type 'symbol
  :group 'python)

(defcustom python-fill-string-function 'python-fill-string
  "Function to fill strings.
This is the function used by `python-fill-paragraph' to
fill strings."
  :type 'symbol
  :group 'python)

(defcustom python-fill-decorator-function 'python-fill-decorator
  "Function to fill decorators.
This is the function used by `python-fill-paragraph' to
fill decorators."
  :type 'symbol
  :group 'python)

(defcustom python-fill-paren-function 'python-fill-paren
  "Function to fill parens.
This is the function used by `python-fill-paragraph' to
fill parens."
  :type 'symbol
  :group 'python)

(defcustom python-fill-docstring-style 'pep-257
  "Style used to fill docstrings.
This affects `python-fill-string' behavior with regards to
triple quotes positioning.

Possible values are `django', `onetwo', `pep-257', `pep-257-nn',
`symmetric', and nil.  A value of nil won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

`django':

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`onetwo':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257-nn':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`symmetric':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice
          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257 with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :group 'python
  :safe (lambda (val)
          (memq val '(django onetwo pep-257 pep-257-nn symmetric nil))))

(defun python-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified."
  (interactive "P")
  (save-excursion
    (cond
     ;; Comments
     ((python-syntax-context 'comment)
      (funcall python-fill-comment-function justify))
     ;; Strings/Docstrings
     ((save-excursion (or (python-syntax-context 'string)
                          (equal (string-to-syntax "|")
                                 (syntax-after (point)))))
      (funcall python-fill-string-function justify))
     ;; Decorators
     ((equal (char-after (save-excursion
                           (python-nav-beginning-of-statement))) ?@)
      (funcall python-fill-decorator-function justify))
     ;; Parens
     ((or (python-syntax-context 'paren)
          (looking-at (python-rx open-paren))
          (save-excursion
            (skip-syntax-forward "^(" (line-end-position))
            (looking-at (python-rx open-paren))))
      (funcall python-fill-paren-function justify))
     (t t))))

(defun python-fill-comment (&optional justify)
  "Comment fill function for `python-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (fill-comment-paragraph justify))

(defun python-fill-string (&optional justify)
  "String fill function for `python-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (let* ((str-start-pos
          (set-marker
           (make-marker)
           (or (python-syntax-context 'string)
               (and (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (point)))))
         (num-quotes (python-syntax-count-quotes
                      (char-after str-start-pos) str-start-pos))
         (str-end-pos
          (save-excursion
            (goto-char (+ str-start-pos num-quotes))
            (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                (goto-char (point-max)))
            (point-marker)))
         (multi-line-p
          ;; Docstring styles may vary for oneliners and multi-liners.
          (> (count-matches "\n" str-start-pos str-end-pos) 0))
         (delimiters-style
          (pcase python-fill-docstring-style
            ;; delimiters-style is a cons cell with the form
            ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
            ;; is NIL means to not add any newlines for start or end
            ;; of docstring.  See `python-fill-docstring-style' for a
            ;; graphic idea of each style.
            (`django (cons 1 1))
            (`onetwo (and multi-line-p (cons 1 2)))
            (`pep-257 (and multi-line-p (cons nil 2)))
            (`pep-257-nn (and multi-line-p (cons nil 1)))
            (`symmetric (and multi-line-p (cons 1 1)))))
         (fill-paragraph-function))
    (save-restriction
      (narrow-to-region str-start-pos str-end-pos)
      (fill-paragraph justify))
    (save-excursion
      (when (and (python-info-docstring-p) python-fill-docstring-style)
        ;; Add the number of newlines indicated by the selected style
        ;; at the start of the docstring.
        (goto-char (+ str-start-pos num-quotes))
        (delete-region (point) (progn
                                 (skip-syntax-forward "> ")
                                 (point)))
        (and (car delimiters-style)
             (or (newline (car delimiters-style)) t)
             ;; Indent only if a newline is added.
             (indent-according-to-mode))
        ;; Add the number of newlines indicated by the selected style
        ;; at the end of the docstring.
        (goto-char (if (not (= str-end-pos (point-max)))
                       (- str-end-pos num-quotes)
                     str-end-pos))
        (delete-region (point) (progn
                                 (skip-syntax-backward "> ")
                                 (point)))
        (and (cdr delimiters-style)
             ;; Add newlines only if string ends.
             (not (= str-end-pos (point-max)))
             (or (newline (cdr delimiters-style)) t)
             ;; Again indent only if a newline is added.
             (indent-according-to-mode))))) t)

(defun python-fill-decorator (&optional _justify)
  "Decorator fill function for `python-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  t)

(defun python-fill-paren (&optional justify)
  "Paren fill function for `python-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (python-syntax-context 'paren)
                          (goto-char (1- (point))))
                        (line-beginning-position))
                      (progn
                        (when (not (python-syntax-context 'paren))
                          (end-of-line)
                          (when (not (python-syntax-context 'paren))
                            (skip-syntax-backward "^)")))
                        (while (and (python-syntax-context 'paren)
                                    (not (eobp)))
                          (goto-char (1+ (point))))
                        (point)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (python-indent-line)
      (goto-char (line-end-position))))
  t)


;;; Skeletons

(defcustom python-skeleton-autoinsert nil
  "Non-nil means template skeletons will be automagically inserted.
This happens when pressing \"if<SPACE>\", for example, to prompt for
the if condition."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(define-obsolete-variable-alias
  'python-use-skeletons 'python-skeleton-autoinsert "24.3")

(defvar python-skeleton-available '()
  "Internal list of available skeletons.")

(define-abbrev-table 'python-mode-skeleton-abbrev-table ()
  "Abbrev table for Python mode skeletons."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*"
  ;; Only expand in code.
  :enable-function (lambda ()
                     (and
                      (not (python-syntax-comment-or-string-p))
                      python-skeleton-autoinsert)))

(defmacro python-skeleton-define (name doc &rest skel)
  "Define a `python-mode' skeleton using NAME DOC and SKEL.
The skeleton will be bound to python-skeleton-NAME and will
be added to `python-mode-skeleton-abbrev-table'."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "python-skeleton-" name))))
    `(progn
       (define-abbrev python-mode-skeleton-abbrev-table
         ,name "" ',function-name :system t)
       (setq python-skeleton-available
             (cons ',function-name python-skeleton-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert %s statement." name))
         ,@skel))))

(define-abbrev-table 'python-mode-abbrev-table ()
  "Abbrev table for Python mode."
  :parents (list python-mode-skeleton-abbrev-table))

(defmacro python-define-auxiliary-skeleton (name &optional doc &rest skel)
  "Define a `python-mode' auxiliary skeleton using NAME DOC and SKEL.
The skeleton will be bound to python-skeleton-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "python-skeleton--" name)))
         (msg (funcall (if (fboundp 'format-message) #'format-message #'format)
                       "Add `%s' clause? " name)))
    (when (not skel)
      (setq skel
            `(< ,(format "%s:" name) \n \n
                > _ \n)))
    `(define-skeleton ,function-name
       ,(or doc
            (format "Auxiliary skeleton for %s statement." name))
       nil
       (unless (y-or-n-p ,msg)
         (signal 'quit t))
       ,@skel)))

(python-define-auxiliary-skeleton else)

(python-define-auxiliary-skeleton except)

(python-define-auxiliary-skeleton finally)

(python-skeleton-define if nil
  "Condition: "
  "if " str ":" \n
  _ \n
  ("other condition, %s: "
   <
   "elif " str ":" \n
   > _ \n nil)
  '(python-skeleton--else) | ^)

(python-skeleton-define while nil
  "Condition: "
  "while " str ":" \n
  > _ \n
  '(python-skeleton--else) | ^)

(python-skeleton-define for nil
  "Iteration spec: "
  "for " str ":" \n
  > _ \n
  '(python-skeleton--else) | ^)

(python-skeleton-define import nil
  "Import from module: "
  "from " str & " " | -5
  "import "
  ("Identifier: " str ", ") -2 \n _)

(python-skeleton-define try nil
  nil
  "try:" \n
  > _ \n
  ("Exception, %s: "
   <
   "except " str ":" \n
   > _ \n nil)
  resume:
  '(python-skeleton--except)
  '(python-skeleton--else)
  '(python-skeleton--finally) | ^)

(python-skeleton-define def nil
  "Function name: "
  "def " str "(" ("Parameter, %s: "
                  (unless (equal ?\( (char-before)) ", ")
                  str) "):" \n
                  "\"\"\"" - "\"\"\"" \n
                  > _ \n)

(python-skeleton-define class nil
  "Class name: "
  "class " str "(" ("Inheritance, %s: "
                    (unless (equal ?\( (char-before)) ", ")
                    str)
  & ")" | -1
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)

(defun python-skeleton-add-menu-items ()
  "Add menu items to Python->Skeletons menu."
  (let ((skeletons (sort python-skeleton-available 'string<)))
    (dolist (skeleton skeletons)
      (easy-menu-add-item
       nil '("Python" "Skeletons")
       `[,(format
           "Insert %s" (nth 2 (split-string (symbol-name skeleton) "-")))
         ,skeleton t]))))

;;; FFAP

(defcustom python-ffap-setup-code
  "
def __FFAP_get_module_path(objstr):
    try:
        import inspect
        import os.path
        # NameError exceptions are delayed until this point.
        obj = eval(objstr)
        module = inspect.getmodule(obj)
        filename = module.__file__
        ext = os.path.splitext(filename)[1]
        if ext in ('.pyc', '.pyo'):
            # Point to the source file.
            filename = filename[:-1]
        if os.path.exists(filename):
            return filename
        return ''
    except:
        return ''"
  "Python code to get a module path."
  :type 'string
  :group 'python)

(defcustom python-ffap-string-code
  "__FFAP_get_module_path('''%s''')"
  "Python code used to get a string with the path of a module."
  :type 'string
  :group 'python)

(defun python-ffap-module-path (module)
  "Function for `ffap-alist' to return path for MODULE."
  (let ((process (or
                  (and (derived-mode-p 'inferior-python-mode)
                       (get-buffer-process (current-buffer)))
                  (python-shell-get-process))))
    (if (not process)
        nil
      (let ((module-file
             (python-shell-send-string-no-output
              (concat
               python-ffap-setup-code
               "\nprint (" (format python-ffap-string-code module) ")")
              process)))
        (unless (zerop (length module-file))
          (python-util-strip-string module-file))))))

(defvar ffap-alist)

(eval-after-load "ffap"
  '(progn
     (push '(python-mode . python-ffap-module-path) ffap-alist)
     (push '(inferior-python-mode . python-ffap-module-path) ffap-alist)))


;;; Code check

(defcustom python-check-command
  (or (executable-find "pyflakes")
      (executable-find "epylint")
      "install pyflakes, pylint or something else")
  "Command used to check a Python file."
  :type 'string
  :group 'python)

(defcustom python-check-buffer-name
  "*Python check: %s*"
  "Buffer name used for check commands."
  :type 'string
  :group 'python)

(defvar python-check-custom-command nil
  "Internal use.")
;; XXX: Avoid `defvar-local' for compat with Emacs<24.3
(make-variable-buffer-local 'python-check-custom-command)

(defun python-check (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default."
  (interactive
   (list (read-string "Check command: "
                      (or python-check-custom-command
                          (concat python-check-command " "
                                  (shell-quote-argument
                                   (or
                                    (let ((name (buffer-file-name)))
                                      (and name
                                           (file-name-nondirectory name)))
                                    "")))))))
  (setq python-check-custom-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (python-shell-with-environment
    (compilation-start command nil
                       (lambda (_modename)
                         (format python-check-buffer-name command)))))


;;; Eldoc

(defcustom python-eldoc-setup-code
  "def __PYDOC_get_help(obj):
    try:
        import inspect
        try:
            str_type = basestring
            argspec_function = inspect.getargspec
        except NameError:
            str_type = str
            argspec_function = inspect.getfullargspec
        if isinstance(obj, str_type):
            obj = eval(obj, globals())
        doc = inspect.getdoc(obj)
        if not doc and callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = inspect.formatargspec(*argspec_function(target))
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args
                )
        else:
            doc = doc.splitlines()[0]
    except:
        doc = ''
    return doc"
  "Python code to setup documentation retrieval."
  :type 'string
  :group 'python)

(defcustom python-eldoc-string-code
  "__PYDOC_get_help('''%s''')"
  "Python code used to get a string with the documentation of an object."
  :type 'string
  :group 'python)

(defun python-eldoc--get-symbol-at-point ()
  "Get the current symbol for eldoc.
Returns the current symbol handling point within arguments."
  (save-excursion
    (let ((start (python-syntax-context 'paren)))
      (when start
        (goto-char start))
      (when (or start
                (eobp)
                (memq (char-syntax (char-after)) '(?\ ?-)))
        ;; Try to adjust to closest symbol if not in one.
        (python-util-forward-comment -1)))
    (python-info-current-symbol t)))

(defun python-eldoc--get-doc-at-point (&optional force-input force-process)
  "Internal implementation to get documentation at point.
If not FORCE-INPUT is passed then what `python-eldoc--get-symbol-at-point'
returns will be used.  If not FORCE-PROCESS is passed what
`python-shell-get-process' returns is used."
  (let ((process (or force-process (python-shell-get-process))))
    (when process
      (let* ((input (or force-input
                        (python-eldoc--get-symbol-at-point)))
             (docstring
              (when input
                ;; Prevent resizing the echo area when iPython is
                ;; enabled.  Bug#18794.
                (python-util-strip-string
                 (python-shell-send-string-no-output
                  (concat
                   python-eldoc-setup-code
                   "\nprint(" (format python-eldoc-string-code input) ")")
                  process)))))
        (unless (zerop (length docstring))
          docstring)))))

(defvar-local python-eldoc-get-doc t
  "Non-nil means eldoc should fetch the documentation
  automatically. Set to nil by `python-eldoc-function' if
  `python-eldoc-function-timeout-permanent' is non-nil and
  `python-eldoc-function' times out.")

(defcustom python-eldoc-function-timeout 1
  "Timeout for `python-eldoc-function' in seconds."
  :group 'python
  :type 'integer
  :version "25.1")

(defcustom python-eldoc-function-timeout-permanent t
  "Non-nil means that when `python-eldoc-function' times out
`python-eldoc-get-doc' will be set to nil"
  :group 'python
  :type 'boolean
  :version "25.1")

(defun python-eldoc-function ()
  "`eldoc-documentation-function' for Python.
For this to work as best as possible you should call
`python-shell-send-buffer' from time to time so context in
inferior Python process is updated properly.

If `python-eldoc-function-timeout' seconds elapse before this
function returns then if
`python-eldoc-function-timeout-permanent' is non-nil
`python-eldoc-get-doc' will be set to nil and eldoc will no
longer return the documentation at the point automatically.

Set `python-eldoc-get-doc' to t to reenable eldoc documentation
fetching"
  (when python-eldoc-get-doc
    (with-timeout (python-eldoc-function-timeout
                   (if python-eldoc-function-timeout-permanent
                       (progn
                         (message "Eldoc echo-area display muted in this buffer, see `python-eldoc-function'")
                         (setq python-eldoc-get-doc nil))
                     (message "`python-eldoc-function' timed out, see `python-eldoc-function-timeout'")))
      (python-eldoc--get-doc-at-point))))

(defun python-eldoc-at-point (symbol)
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol."
  (interactive
   (let ((symbol (python-eldoc--get-symbol-at-point))
         (enable-recursive-minibuffers t))
     (list (read-string (if symbol
                            (format "Describe symbol (default %s): " symbol)
                          "Describe symbol: ")
                        nil nil symbol))))
  (message (python-eldoc--get-doc-at-point symbol)))

(defun python-describe-at-point (symbol process)
  (interactive (list (python-info-current-symbol)
                     (python-shell-get-process)))
  (comint-send-string process (concat "help('" symbol "')\n")))


;;; Hideshow

(defun python-hideshow-forward-sexp-function (arg)
  "Python specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
  arg  ; Shut up, byte compiler.
  (python-nav-end-of-defun)
  (unless (python-info-current-line-empty-p)
    (backward-char)))


;;; Imenu

(defvar python-imenu-format-item-label-function
  'python-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar python-imenu-format-parent-item-label-function
  'python-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar python-imenu-format-parent-item-jump-label-function
  'python-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun python-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun python-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (python-imenu-format-item-label type name)))

(defun python-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))

(defun python-imenu--get-defun-type-name ()
  "Return defun type and name at current position."
  (when (looking-at python-nav-beginning-of-defun-regexp)
    (let ((split (split-string (match-string-no-properties 0))))
      (if (= (length split) 2)
          split
        (list (concat (car split) " " (cadr split))
              (car (last split)))))))

(defun python-imenu--put-parent (type name pos tree)
  "Add the parent with TYPE, NAME and POS to TREE."
  (let ((label
         (funcall python-imenu-format-item-label-function type name))
        (jump-label
         (funcall python-imenu-format-parent-item-jump-label-function type name)))
    (if (not tree)
        (cons label pos)
      (cons label (cons (cons jump-label pos) tree)))))

(defun python-imenu--build-tree (&optional min-indent prev-indent tree)
  "Recursively build the tree of nested definitions of a node.
Arguments MIN-INDENT, PREV-INDENT and TREE are internal and should
not be passed explicitly unless you know what you are doing."
  (setq min-indent (or min-indent 0)
        prev-indent (or prev-indent python-indent-offset))
  (let* ((pos (python-nav-backward-defun))
         (defun-type-name (and pos (python-imenu--get-defun-type-name)))
         (type (car defun-type-name))
         (name (cadr defun-type-name))
         (label (when name
                  (funcall python-imenu-format-item-label-function type name)))
         (indent (current-indentation))
         (children-indent-limit (+ python-indent-offset min-indent)))
    (cond ((not pos)
           ;; Nothing found, probably near to bobp.
           nil)
          ((<= indent min-indent)
           ;; The current indentation points that this is a parent
           ;; node, add it to the tree and stop recursing.
           (python-imenu--put-parent type name pos tree))
          (t
           (python-imenu--build-tree
            min-indent
            indent
            (if (<= indent children-indent-limit)
                ;; This lies within the children indent offset range,
                ;; so it's a normal child of its parent (i.e., not
                ;; a child of a child).
                (cons (cons label pos) tree)
              ;; Oh no, a child of a child?!  Fear not, we
              ;; know how to roll.  We recursively parse these by
              ;; swapping prev-indent and min-indent plus adding this
              ;; newly found item to a fresh subtree.  This works, I
              ;; promise.
              (cons
               (python-imenu--build-tree
                prev-indent indent (list (cons label pos)))
               tree)))))))

(defun python-imenu-create-index ()
  "Return tree Imenu alist for the current Python buffer.
Change `python-imenu-format-item-label-function',
`python-imenu-format-parent-item-label-function',
`python-imenu-format-parent-item-jump-label-function' to
customize how labels are formatted."
  (goto-char (point-max))
  (let ((index)
        (tree))
    (while (setq tree (python-imenu--build-tree))
      (setq index (cons tree index)))
    index))

(defun python-imenu-create-flat-index (&optional alist prefix)
  "Return flat outline of the current Python buffer for Imenu.
Optional argument ALIST is the tree to be flattened; when nil
`python-imenu-build-index' is used with
`python-imenu-format-parent-item-jump-label-function'
`python-imenu-format-parent-item-label-function'
`python-imenu-format-item-label-function' set to
  (lambda (type name) name)
Optional argument PREFIX is used in recursive calls and should
not be passed explicitly.

Converts this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\"
      (\"decorator\" . 173)
      (\"wrap\"
       (\"wrap\" . 353)
       (\"wrapped_f\" . 393))))

To this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\" . 173)
     (\"decorator.wrap\" . 353)
     (\"decorator.wrapped_f\" . 393))"
  ;; Inspired by imenu--flatten-index-alist removed in revno 21853.
  (apply
   'nconc
   (mapcar
    (lambda (item)
      (let ((name (if prefix
                      (concat prefix "." (car item))
                    (car item)))
            (pos (cdr item)))
        (cond ((or (numberp pos) (markerp pos))
               (list (cons name pos)))
              ((listp pos)
               (cons
                (cons name (cdar pos))
                (python-imenu-create-flat-index (cddr item) name))))))
    (or alist
        (let* ((fn (lambda (_type name) name))
               (python-imenu-format-item-label-function fn)
              (python-imenu-format-parent-item-label-function fn)
              (python-imenu-format-parent-item-jump-label-function fn))
          (python-imenu-create-index))))))


;;; Misc helpers

(defun python-info-current-defun (&optional include-type)
  "Return name of surrounding function with Python compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function can be used as the value of `add-log-current-defun-function'
since it returns nil if point is not inside a defun."
  (save-restriction
    (prog-widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          (while (python-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (python-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              python-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     python-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (python-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t))))
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat 'identity names ".")))))))

(defun python-info-current-symbol (&optional replace-self)
  "Return current symbol using dotty syntax.
With optional argument REPLACE-SELF convert \"self\" to current
parent defun name."
  (let ((name
         (and (not (python-syntax-comment-or-string-p))
              (with-syntax-table python-dotty-syntax-table
                (let ((sym (symbol-at-point)))
                  (and sym
                       (substring-no-properties (symbol-name sym))))))))
    (when name
      (if (not replace-self)
          name
        (let ((current-defun (python-info-current-defun)))
          (if (not current-defun)
              name
            (replace-regexp-in-string
             (python-rx line-start word-start "self" word-end ?.)
             (concat
              (mapconcat 'identity
                         (butlast (split-string current-defun "\\."))
                         ".") ".")
             name)))))))

(defun python-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (python-nav-beginning-of-statement)
    (looking-at (python-rx block-start))))

(defun python-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (python-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (python-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun python-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (python-nav-beginning-of-statement)
               (point))))

(defun python-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (python-nav-end-of-statement)
               (point))))

(defun python-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (python-info-beginning-of-statement-p)
       (python-info-statement-starts-block-p)))

(defun python-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (python-info-end-of-statement-p)
       (python-info-statement-ends-block-p)))

(define-obsolete-function-alias
  'python-info-closing-block
  'python-info-dedenter-opening-block-position "24.4")

(defun python-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid python file."
  (let ((positions (python-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun python-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid python file."
  (save-excursion
    (let ((dedenter-pos (python-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((cur-line (line-beginning-position))
               (pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (python-nav--syntactically
                    (lambda ()
                      (re-search-backward (python-rx block-start) nil t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation (apply #'min collected-indentations)))
                           ;; There must be no line with indentation
                           ;; smaller than `indentation' (except for
                           ;; blank lines) between the found opening
                           ;; block and the current line, otherwise it
                           ;; is not an opening block.
                           (save-excursion
                             (forward-line)
                             (let ((no-back-indent t))
                               (save-match-data
                                 (while (and (< (point) cur-line)
                                             (setq no-back-indent
                                                   (or (> (current-indentation) indentation)
                                                       (python-info-current-line-empty-p))))
                                   (forward-line)))
                               no-back-indent)))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(define-obsolete-function-alias
  'python-info-closing-block-message
  'python-info-dedenter-opening-block-message "24.4")

(defun python-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (python-info-dedenter-opening-block-position)))
    (when point
      (save-restriction
        (prog-widen)
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position))))))))

(defun python-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (python-nav-beginning-of-statement)
    (when (and (not (python-syntax-context-type))
               (looking-at (python-rx dedenter)))
      (point))))

(defun python-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
    (save-restriction
      (prog-widen)
      (when line-number
        (python-util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (python-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker)))))

(defun python-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line start.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
    (save-restriction
      (prog-widen)
      (when line-number
        (python-util-goto-line line-number))
      (when (python-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (python-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker)))))

(defun python-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
    (save-restriction
      (prog-widen)
      (let* ((context-type (progn
                             (back-to-indentation)
                             (python-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (python-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (python-util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (python-util-forward-comment)
               (python-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (python-util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (python-info-line-ends-backslash-p))
                 (point-marker))))))))

(defun python-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (python-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (python-rx block-start))
        (point-marker)))))

(defun python-info-assignment-statement-p (&optional current-line-only)
  "Check if current line is an assignment.
With argument CURRENT-LINE-ONLY is non-nil, don't follow any
continuations, just check the if current line is an assignment."
  (save-excursion
    (let ((found nil))
      (if current-line-only
          (back-to-indentation)
        (python-nav-beginning-of-statement))
      (while (and
              (re-search-forward (python-rx not-simple-operator
                                            assignment-operator
                                            (group not-simple-operator))
                                 (line-end-position) t)
              (not found))
        (save-excursion
          ;; The assignment operator should not be inside a string.
          (backward-char (length (match-string-no-properties 1)))
          (setq found (not (python-syntax-context-type)))))
      (when found
        (skip-syntax-forward " ")
        (point-marker)))))

;; TODO: rename to clarify this is only for the first continuation
;; line or remove it and move its body to `python-indent-context'.
(defun python-info-assignment-continuation-line-p ()
  "Check if current line is the first continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (python-info-continuation-line-p)
      (forward-line -1)
      (python-info-assignment-statement-p t))))

(defun python-info-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (python-syntax-context-type (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at python-nav-beginning-of-defun-regexp))))

(defun python-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun python-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (python-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))

(defun python-info-docstring-p (&optional syntax-ppss)
  "Return non-nil if point is in a docstring.
When optional argument SYNTAX-PPSS is given, use that instead of
point's current `syntax-ppss'."
  ;;; https://www.python.org/dev/peps/pep-0257/#what-is-a-docstring
  (save-excursion
    (when (and syntax-ppss (python-syntax-context 'string syntax-ppss))
      (goto-char (nth 8 syntax-ppss)))
    (python-nav-beginning-of-statement)
    (let ((counter 1)
          (indentation (current-indentation))
          (backward-sexp-point)
          (re (concat "[uU]?[rR]?"
                      (python-rx string-delimiter))))
      (when (and
             (not (python-info-assignment-statement-p))
             (looking-at-p re)
             ;; Allow up to two consecutive docstrings only.
             (>=
              2
              (let (last-backward-sexp-point)
                (while (save-excursion
                         (python-nav-backward-sexp)
                         (setq backward-sexp-point (point))
                         (and (= indentation (current-indentation))
                              ;; Make sure we're always moving point.
                              ;; If we get stuck in the same position
                              ;; on consecutive loop iterations,
                              ;; bail out.
                              (prog1 (not (eql last-backward-sexp-point
                                               backward-sexp-point))
                                (setq last-backward-sexp-point
                                      backward-sexp-point))
                              (looking-at-p
                               (concat "[uU]?[rR]?"
                                       (python-rx string-delimiter)))))
                  ;; Previous sexp was a string, restore point.
                  (goto-char backward-sexp-point)
                  (cl-incf counter))
                counter)))
        (python-util-forward-comment -1)
        (python-nav-beginning-of-statement)
        (cond ((bobp))
              ((python-info-assignment-statement-p) t)
              ((python-info-looking-at-beginning-of-defun))
              (t nil))))))

(defun python-info-encoding-from-cookie ()
  "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
  (let ((first-two-lines
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (forward-line 2)
             (buffer-substring-no-properties
              (point)
              (point-min))))))
    (when (string-match (python-rx coding-cookie) first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(defun python-info-encoding ()
  "Return encoding for file.
Try `python-info-encoding-from-cookie', if none is found then
default to utf-8."
  ;; If no encoding is defined, then it's safe to use UTF-8: Python 2
  ;; uses ASCII as default while Python 3 uses UTF-8.  This means that
  ;; in the worst case scenario python.el will make things work for
  ;; Python 2 files with unicode data and no encoding defined.
  (or (python-info-encoding-from-cookie)
      'utf-8))


;;; Utility functions

(defun python-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

;; Stolen from org-mode
(defun python-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^python-\"."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
          (string-match (or regexp "^python-")
                        (symbol-name (car pair)))
          (set (make-local-variable (car pair))
               (cdr pair))))
   (buffer-local-variables from-buffer)))

(defvar comint-last-prompt-overlay)     ; Shut up, byte compiler.

(defun python-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

(defun python-util-forward-comment (&optional direction)
  "Python mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (python-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun python-util-list-directories (directory &optional predicate max-depth)
  "List DIRECTORY subdirs, filtered by PREDICATE and limited by MAX-DEPTH.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files.  When optional argument MAX-DEPTH is non-nil, stop
searching when depth is reached, else don't limit."
  (let* ((dir (expand-file-name directory))
         (dir-length (length dir))
         (predicate (or predicate #'identity))
         (to-scan (list dir))
         (tally nil))
    (while to-scan
      (let ((current-dir (car to-scan)))
        (when (funcall predicate current-dir)
          (setq tally (cons current-dir tally)))
        (setq to-scan (append (cdr to-scan)
                              (python-util-list-files
                               current-dir #'file-directory-p)
                              nil))
        (when (and max-depth
                   (<= max-depth
                       (length (split-string
                                (substring current-dir dir-length)
                                "/\\|\\\\" t))))
          (setq to-scan nil))))
    (nreverse tally)))

(defun python-util-list-files (dir &optional predicate)
  "List files in DIR, filtering with PREDICATE.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files."
  (let ((dir-name (file-name-as-directory dir)))
    (apply #'nconc
           (mapcar (lambda (file-name)
                     (let ((full-file-name (expand-file-name file-name dir-name)))
                       (when (and
                              (not (member file-name '("." "..")))
                              (funcall (or predicate #'identity) full-file-name))
                         (list full-file-name))))
                   (directory-files dir-name)))))

(defun python-util-list-packages (dir &optional max-depth)
  "List packages in DIR, limited by MAX-DEPTH.
When optional argument MAX-DEPTH is non-nil, stop searching when
depth is reached, else don't limit."
  (let* ((dir (expand-file-name dir))
         (parent-dir (file-name-directory
                      (directory-file-name
                       (file-name-directory
                        (file-name-as-directory dir)))))
         (subpath-length (length parent-dir)))
    (mapcar
     (lambda (file-name)
       (replace-regexp-in-string
        (rx (or ?\\ ?/)) "." (substring file-name subpath-length)))
     (python-util-list-directories
      (directory-file-name dir)
      (lambda (dir)
        (file-exists-p (expand-file-name "__init__.py" dir)))
      max-depth))))

(defun python-util-popn (lst n)
  "Return LST first N elements.
N should be an integer, when negative its opposite is used.
When N is bigger than the length of LST, the list is
returned as is."
  (let* ((n (min (abs n)))
         (len (length lst))
         (acc))
    (if (> n len)
        lst
      (while (< 0 n)
        (setq acc (cons (car lst) acc)
              lst (cdr lst)
              n (1- n)))
      (reverse acc))))

(defun python-util-strip-string (string)
  "Strip STRING whitespace and newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any whitespace ?\r ?\n)))
           (: (* (any whitespace ?\r ?\n)) string-end)))
   ""
   string))

(defun python-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))


;;; Flymake integration

(defgroup python-flymake nil
  "Integration between Python and Flymake."
  :group 'python
  :link '(custom-group-link :tag "Flymake" flymake)
  :version "26.1")

(defcustom python-flymake-command '("pyflakes")
  "The external tool that will be used to perform the syntax check.
This is a non empty list of strings, the checker tool possibly followed by
required arguments.  Once launched it will receive the Python source to be
checked as its standard input.
To use `flake8' you would set this to (\"flake8\" \"-\")."
  :group 'python-flymake
  :type '(repeat string))

;; The default regexp accomodates for older pyflakes, which did not
;; report the column number, and at the same time it's compatible with
;; flake8 output, although it may be redefined to explicitly match the
;; TYPE
(defcustom python-flymake-command-output-pattern
  (list
   "^\\(?:<?stdin>?\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):\\)? \\(?3:.*\\)$"
   1 2 nil 3)
  "Specify how to parse the output of `python-flymake-command'.
The value has the form (REGEXP LINE COLUMN TYPE MESSAGE): if
REGEXP matches, the LINE'th subexpression gives the line number,
the COLUMN'th subexpression gives the column number on that line,
the TYPE'th subexpression gives the type of the message and the
MESSAGE'th gives the message text itself.

If COLUMN or TYPE are nil or that index didn't match, that
information is not present on the matched line and a default will
be used."
  :group 'python-flymake
  :type '(list regexp
               (integer :tag "Line's index")
               (choice
                (const :tag "No column" nil)
                (integer :tag "Column's index"))
               (choice
                (const :tag "No type" nil)
                (integer :tag "Type's index"))
               (integer :tag "Message's index")))

(defcustom python-flymake-msg-alist
  '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning))
  "Alist used to associate messages to their types.
Each element should be a cons-cell (REGEXP . TYPE), where TYPE must be
one defined in the variable `flymake-diagnostic-types-alist'.
For example, when using `flake8' a possible configuration could be:

  ((\"\\(^redefinition\\|.*unused.*\\|used$\\)\" . :warning)
   (\"^E999\" . :error)
   (\"^[EW][0-9]+\" . :note))

By default messages are considered errors."
  :group 'python-flymake
  :type `(alist :key-type (regexp)
                :value-type (symbol)))

(defvar-local python--flymake-proc nil)

(defun python--flymake-parse-output (source proc report-fn)
  "Collect diagnostics parsing checker tool's output line by line."
  (let ((rx (nth 0 python-flymake-command-output-pattern))
        (lineidx (nth 1 python-flymake-command-output-pattern))
        (colidx (nth 2 python-flymake-command-output-pattern))
        (typeidx (nth 3 python-flymake-command-output-pattern))
        (msgidx (nth 4 python-flymake-command-output-pattern)))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (cl-loop
       while (search-forward-regexp rx nil t)
       for msg = (match-string msgidx)
       for (beg . end) = (flymake-diag-region
                          source
                          (string-to-number
                           (match-string lineidx))
                          (and colidx
                               (match-string colidx)
                               (string-to-number
                                (match-string colidx))))
       for type = (or (and typeidx
                           (match-string typeidx)
                           (assoc-default
                            (match-string typeidx)
                            python-flymake-msg-alist
                            #'string-match))
                      (assoc-default msg
                                     python-flymake-msg-alist
                                     #'string-match)
                      :error)
       collect (flymake-make-diagnostic
                source beg end type msg)
       into diags
       finally (funcall report-fn diags)))))

(defun python-flymake (report-fn &rest _args)
  "Flymake backend for Python.
This backend uses `python-flymake-command' (which see) to launch a process
that is passed the current buffer's content via stdin.
REPORT-FN is Flymake's callback function."
  (unless (executable-find (car python-flymake-command))
    (error "Cannot find a suitable checker"))

  (when (process-live-p python--flymake-proc)
    (kill-process python--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq python--flymake-proc
            (make-process
             :name "python-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *python-flymake*")
             :command python-flymake-command
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc python--flymake-proc))
                       (python--flymake-parse-output source proc report-fn))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region python--flymake-proc (point-min) (point-max))
      (process-send-eof python--flymake-proc))))


(defun python-electric-pair-string-delimiter ()
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

(defvar electric-indent-inhibit)

;;;###autoload
(define-derived-mode python-mode prog-mode "Python"
  "Major mode for editing Python files.

\\{python-mode-map}"
  (set (make-local-variable 'tab-width) 8)
  (set (make-local-variable 'indent-tabs-mode) nil)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (set (make-local-variable 'forward-sexp-function)
       'python-nav-forward-sexp)

  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function
          . python-font-lock-syntactic-face-function)))

  (set (make-local-variable 'syntax-propertize-function)
       python-syntax-propertize-function)

  (set (make-local-variable 'indent-line-function)
       #'python-indent-line-function)
  (set (make-local-variable 'indent-region-function) #'python-indent-region)
  ;; Because indentation is not redundant, we cannot safely reindent code.
  (set (make-local-variable 'electric-indent-inhibit) t)
  (set (make-local-variable 'electric-indent-chars)
       (cons ?: electric-indent-chars))

  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'python-electric-pair-string-delimiter 'append t)

  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function)
       #'python-fill-paragraph)

  (set (make-local-variable 'beginning-of-defun-function)
       #'python-nav-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'python-nav-end-of-defun)

  (add-hook 'completion-at-point-functions
            #'python-completion-at-point nil 'local)

  (add-hook 'post-self-insert-hook
            #'python-indent-post-self-insert-function 'append 'local)

  (set (make-local-variable 'imenu-create-index-function)
       #'python-imenu-create-index)

  (set (make-local-variable 'add-log-current-defun-function)
       #'python-info-current-defun)

  (add-hook 'which-func-functions #'python-info-current-defun nil t)

  (set (make-local-variable 'skeleton-further-elements)
       '((abbrev-mode nil)
         (< '(backward-delete-char-untabify (min python-indent-offset
                                                 (current-column))))
         (^ '(- (1+ (current-indentation))))))

  (if (null eldoc-documentation-function)
      ;; Emacs<25
      (set (make-local-variable 'eldoc-documentation-function)
           #'python-eldoc-function)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'python-eldoc-function))

  (add-to-list
   'hs-special-modes-alist
   `(python-mode
     "\\s-*\\_<\\(?:def\\|class\\)\\_>"
     ;; Use the empty string as end regexp so it doesn't default to
     ;; "\\s)".  This way parens at end of defun are properly hidden.
     ""
     "#"
     python-hideshow-forward-sexp-function
     nil))

  (set (make-local-variable 'outline-regexp)
       (python-rx (* space) block-start))
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
  (set (make-local-variable 'outline-level)
       #'(lambda ()
           "`outline-level' function for Python mode."
           (1+ (/ (current-indentation) python-indent-offset))))

  (set (make-local-variable 'prettify-symbols-alist)
       python--prettify-symbols-alist)

  (python-skeleton-add-menu-items)

  (make-local-variable 'python-shell-internal-buffer)

  (when python-indent-guess-indent-offset
    (python-indent-guess-indent-offset))

  (add-hook 'flymake-diagnostic-functions #'python-flymake nil t))


(provide 'python)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; python.el ends here
