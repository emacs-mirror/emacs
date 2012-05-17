;;; python.el --- Python's flying circus support for Emacs

;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.

;; Author: Fabián E. Gallina <fabian@anue.biz>
;; URL: https://github.com/fgallina/python.el
;; Version: 0.23.1
;; Maintainer: FSF
;; Created: Jul 2010
;; Keywords: languages

;; This file is NOT part of GNU Emacs.

;; python.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; python.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with python.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Python files with some fontification and
;; indentation bits extracted from original Dave Love's python.el
;; found in GNU/Emacs.

;; While it probably has less features than Dave Love's python.el and
;; PSF's python-mode.el it provides the main stuff you'll need while
;; keeping it simple :)

;; Implements Syntax highlighting, Indentation, Movement, Shell
;; interaction, Shell completion, Shell virtualenv support, Pdb
;; tracking, Symbol completion, Skeletons, FFAP, Code Check, Eldoc,
;; imenu.

;; Syntax highlighting: Fontification of code is provided and supports
;; python's triple quoted strings properly.

;; Indentation: Automatic indentation with indentation cycling is
;; provided, it allows you to navigate different available levels of
;; indentation by hitting <tab> several times.  Also when inserting a
;; colon the `python-indent-electric-colon' command is invoked and
;; causes the current line to be dedented automatically if needed.

;; Movement: `beginning-of-defun' and `end-of-defun' functions are
;; properly implemented.  There are also specialized
;; `forward-sentence' and `backward-sentence' replacements
;; (`python-nav-forward-sentence', `python-nav-backward-sentence'
;; respectively).  Extra functions `python-nav-sentence-start' and
;; `python-nav-sentence-end' are included to move to the beginning and
;; to the end of a setence while taking care of multiline definitions.
;; `python-nav-jump-to-defun' is provided and allows jumping to a
;; function or class definition quickly in the current buffer.

;; Shell interaction: is provided and allows you to execute easily any
;; block of code of your current buffer in an inferior Python process.

;; Shell completion: hitting tab will try to complete the current
;; word.  Shell completion is implemented in a manner that if you
;; change the `python-shell-interpreter' to any other (for example
;; IPython) it should be easy to integrate another way to calculate
;; completions.  You just need to specify your custom
;; `python-shell-completion-setup-code' and
;; `python-shell-completion-string-code'.

;; Here is a complete example of the settings you would use for
;; iPython 0.11:

;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; For iPython 0.10 everything would be the same except for
;; `python-shell-completion-string-code' and
;; `python-shell-completion-module-string-code':

;; (setq python-shell-completion-string-code
;;       "';'.join(__IP.complete('''%s'''))\n"
;;       python-shell-completion-module-string-code "")

;; Unfortunately running iPython on Windows needs some more tweaking.
;; The way you must set `python-shell-interpreter' and
;; `python-shell-interpreter-args' is as follows:

;; (setq
;;  python-shell-interpreter "C:\\Python27\\python.exe"
;;  python-shell-interpreter-args
;;  "-i C:\\Python27\\Scripts\\ipython-script.py")

;; That will spawn the iPython process correctly (Of course you need
;; to modify the paths according to your system).

;; Please note that the default completion system depends on the
;; readline module, so if you are using some Operating System that
;; bundles Python without it (like Windows) just install the
;; pyreadline from http://ipython.scipy.org/moin/PyReadline/Intro and
;; you should be good to go.

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

;; Since the above is cumbersome and can be programatically
;; calculated, the variable `python-shell-virtualenv-path' is
;; provided.  When this variable is set with the path of the
;; virtualenv to use, `process-environment' and `exec-path' get proper
;; values in order to run shells inside the specified virtualenv.  So
;; the following will achieve the same as the previous example:

;; (setq python-shell-virtualenv-path "/path/to/env/")

;; Also the `python-shell-extra-pythonpaths' variable have been
;; introduced as simple way of adding paths to the PYTHONPATH without
;; affecting existing values.

;; Pdb tracking: when you execute a block of code that contains some
;; call to pdb (or ipdb) it will prompt the block of code and will
;; follow the execution of pdb marking the current line with an arrow.

;; Symbol completion: you can complete the symbol at point.  It uses
;; the shell completion in background so you should run
;; `python-shell-send-buffer' from time to time to get better results.

;; Skeletons: 6 skeletons are provided for simple inserting of class,
;; def, for, if, try and while.  These skeletons are integrated with
;; dabbrev.  If you have `dabbrev-mode' activated and
;; `python-skeleton-autoinsert' is set to t, then whenever you type
;; the name of any of those defined and hit SPC, they will be
;; automatically expanded.

;; FFAP: You can find the filename for a given module when using ffap
;; out of the box.  This feature needs an inferior python shell
;; running.

;; Code check: Check the current file for errors with `python-check'
;; using the program defined in `python-check-command'.

;; Eldoc: returns documentation for object at point by using the
;; inferior python subprocess to inspect its documentation.  As you
;; might guessed you should run `python-shell-send-buffer' from time
;; to time to get better results too.

;; imenu: This mode supports imenu.  It builds a plain or tree menu
;; depending on the value of `python-imenu-make-tree'.  Also you can
;; customize if menu items should include its type using
;; `python-imenu-include-defun-type'.

;; If you used python-mode.el you probably will miss auto-indentation
;; when inserting newlines.  To achieve the same behavior you have
;; two options:

;; 1) Use GNU/Emacs' standard binding for `newline-and-indent': C-j.

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
(require 'comint)

(eval-when-compile
  (require 'cl)
  ;; Avoid compiler warnings
  (defvar view-return-to-alist)
  (defvar compilation-error-regexp-alist)
  (defvar outline-heading-end-regexp))

(autoload 'comint-mode "comint")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist (cons (purecopy "python") 'python-mode))

(defgroup python nil
  "Python Language's flying circus support for Emacs."
  :group 'languages
  :version "23.2"
  :link '(emacs-commentary-link "python"))


;;; Bindings

(defvar python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
    (substitute-key-definition 'backward-sentence
                               'python-nav-backward-sentence
                               map global-map)
    (substitute-key-definition 'forward-sentence
                               'python-nav-forward-sentence
                               map global-map)
    (define-key map "\C-c\C-j" 'python-nav-jump-to-defun)
    ;; Indent specific
    (define-key map "\177" 'python-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
    (define-key map "\C-c<" 'python-indent-shift-left)
    (define-key map "\C-c>" 'python-indent-shift-right)
    (define-key map ":" 'python-indent-electric-colon)
    ;; Skeletons
    (define-key map "\C-c\C-tc" 'python-skeleton-class)
    (define-key map "\C-c\C-td" 'python-skeleton-def)
    (define-key map "\C-c\C-tf" 'python-skeleton-for)
    (define-key map "\C-c\C-ti" 'python-skeleton-if)
    (define-key map "\C-c\C-tt" 'python-skeleton-try)
    (define-key map "\C-c\C-tw" 'python-skeleton-while)
    ;; Shell interaction
    (define-key map "\C-c\C-s" 'python-shell-send-string)
    (define-key map "\C-c\C-r" 'python-shell-send-region)
    (define-key map "\C-\M-x" 'python-shell-send-defun)
    (define-key map "\C-c\C-c" 'python-shell-send-buffer)
    (define-key map "\C-c\C-l" 'python-shell-send-file)
    (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
    ;; Some util commands
    (define-key map "\C-c\C-v" 'python-check)
    (define-key map "\C-c\C-f" 'python-eldoc-at-point)
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
        ["Jump to def/class" python-nav-jump-to-defun
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

(eval-when-compile
  (defconst python-rx-constituents
    (list
     `(block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with")
                                   symbol-end))
     `(decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
     `(defun                . ,(rx symbol-start (or "def" "class") symbol-end))
     `(symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
     `(open-paren           . ,(rx (or "{" "[" "(")))
     `(close-paren          . ,(rx (or "}" "]" ")")))
     `(simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
     `(not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
     `(operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
     `(assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|="))))
    "Additional Python specific sexps for `python-rx'"))

(defmacro python-rx (&rest regexps)
  "Python mode specialized rx macro.
This variant of `rx' supports common python named REGEXPS."
  (let ((rx-constituents (append python-rx-constituents rx-constituents)))
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (rx-to-string `(and ,@regexps) t))
          (t
           (rx-to-string (car regexps) t)))))


;;; Font-lock and syntax
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
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
           "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError"
           "ImportError" "ImportWarning" "IndexError" "KeyError"
           "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
           "NotImplementedError" "OSError" "OverflowError"
           "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
           "RuntimeWarning" "StopIteration" "SyntaxError" "SyntaxWarning"
           "SystemError" "SystemExit" "TypeError" "UnboundLocalError"
           "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
           "UnicodeTranslateError" "UnicodeWarning" "UserWarning" "VMSError"
           "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
           ;; Python 2:
           "StandardError"
           ;; Python 3:
           "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
           "TabError")
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
    ;; asignations
    ;; support for a = b = c = 5
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_)))
                             (? ?\[ (+ (not (any  ?\]))) ?\]) (* space)
                             assignment-operator)))
          (when (re-search-forward re limit t)
            (while (and (python-info-ppss-context 'paren)
                        (re-search-forward re limit t)))
            (if (and (not (python-info-ppss-context 'paren))
                     (not (equal (char-after (point-marker)) ?=)))
                t
              (set-match-data nil)))))
     (1 font-lock-variable-name-face nil nil))
    ;; support for a, b, c = (1, 2, 3)
    (,(lambda (limit)
        (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                             (* ?, (* space) (+ (any word ?. ?_)) (* space))
                             ?, (* space) (+ (any word ?. ?_)) (* space)
                             assignment-operator)))
          (when (and (re-search-forward re limit t)
                     (goto-char (nth 3 (match-data))))
            (while (and (python-info-ppss-context 'paren)
                        (re-search-forward re limit t))
              (goto-char (nth 3 (match-data))))
            (if (not (python-info-ppss-context 'paren))
                t
              (set-match-data nil)))))
     (1 font-lock-variable-name-face nil nil))))

(defconst python-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:\\([RUru]\\)[Rr]?\\|^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?:\\('\\)'\\('\\)\\|\\(?2:\"\\)\"\\(?3:\"\\)\\)")
     (3 (python-quote-syntax)))))

(defun python-quote-syntax ()
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.)  We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (let ((syntax (save-match-data (syntax-ppss))))
      (cond
       ((eq t (nth 3 syntax))           ; after unclosed fence
        ;; Consider property for the last char if in a fenced string.
        (goto-char (nth 8 syntax))  ; fence position
        (skip-chars-forward "uUrR") ; skip any prefix
        ;; Is it a matching sequence?
        (if (eq (char-after) (char-after (match-beginning 2)))
            (put-text-property (match-beginning 3) (match-end 3)
                               'syntax-table (string-to-syntax "|"))))
       ((match-end 1)
        ;; Consider property for initial char, accounting for prefixes.
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "|")))
       (t
        ;; Consider property for initial char, accounting for prefixes.
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "|"))))
      )))

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

(defvar python-indent-current-level 0
  "Current indentation level `python-indent-line-function' is using.")

(defvar python-indent-levels '(0)
  "Levels of indentation available for `python-indent-line-function'.")

(defvar python-indent-dedenters '("else" "elif" "except" "finally")
  "List of words that should be dedented.
These make `python-indent-calculate-indentation' subtract the value of
`python-indent-offset'.")

(defun python-indent-guess-indent-offset ()
  "Guess and set `python-indent-offset' for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((block-end))
        (while (and (not block-end)
                    (re-search-forward
                     (python-rx line-start block-start) nil t))
          (when (and
                 (not (python-info-ppss-context-type))
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
          (if indentation
              (setq python-indent-offset indentation)
            (message "Can't guess python-indent-offset, using defaults: %s"
                     python-indent-offset)))))))

(defun python-indent-context ()
  "Get information on indentation context.
Context information is returned with a cons with the form:
    \(STATUS . START)

Where status can be any of the following symbols:
 * inside-paren: If point in between (), {} or []
 * inside-string: If point is inside a string
 * after-backslash: Previous line ends in a backslash
 * after-beginning-of-block: Point is after beginning of block
 * after-line: Point is after normal line
 * no-indent: Point is at beginning of buffer or other special case
START is the buffer position where the sexp starts."
  (save-restriction
    (widen)
    (let ((ppss (save-excursion (beginning-of-line) (syntax-ppss)))
          (start))
      (cons
       (cond
        ;; Beginning of buffer
        ((save-excursion
           (goto-char (line-beginning-position))
           (bobp))
         'no-indent)
        ;; Inside a paren
        ((setq start (python-info-ppss-context 'paren ppss))
         'inside-paren)
        ;; Inside string
        ((setq start (python-info-ppss-context 'string ppss))
         'inside-string)
        ;; After backslash
        ((setq start (when (not (or (python-info-ppss-context 'string ppss)
                                    (python-info-ppss-context 'comment ppss)))
                       (let ((line-beg-pos (line-beginning-position)))
                         (when (python-info-line-ends-backslash-p
                                (1- line-beg-pos))
                           (- line-beg-pos 2)))))
         'after-backslash)
        ;; After beginning of block
        ((setq start (save-excursion
                       (when (progn
                               (back-to-indentation)
                               (python-util-forward-comment -1)
                               (equal (char-before) ?:))
                         ;; Move to the first block start that's not in within
                         ;; a string, comment or paren and that's not a
                         ;; continuation line.
                         (while (and (re-search-backward
                                      (python-rx block-start) nil t)
                                     (or
                                      (python-info-ppss-context 'string)
                                      (python-info-ppss-context 'comment)
                                      (python-info-ppss-context 'paren)
                                      (python-info-continuation-line-p))))
                         (when (looking-at (python-rx block-start))
                           (point-marker)))))
         'after-beginning-of-block)
        ;; After normal line
        ((setq start (save-excursion
                       (back-to-indentation)
                       (python-util-forward-comment -1)
                       (python-nav-sentence-start)
                       (point-marker)))
         'after-line)
        ;; Do not indent
        (t 'no-indent))
       start))))

(defun python-indent-calculate-indentation ()
  "Calculate correct indentation offset for the current line."
  (let* ((indentation-context (python-indent-context))
         (context-status (car indentation-context))
         (context-start (cdr indentation-context)))
    (save-restriction
      (widen)
      (save-excursion
        (case context-status
          ('no-indent 0)
          ;; When point is after beginning of block just add one level
          ;; of indentation relative to the context-start
          ('after-beginning-of-block
           (goto-char context-start)
           (+ (current-indentation) python-indent-offset))
          ;; When after a simple line just use previous line
          ;; indentation, in the case current line starts with a
          ;; `python-indent-dedenters' de-indent one level.
          ('after-line
           (-
            (save-excursion
              (goto-char context-start)
              (current-indentation))
            (if (progn
                  (back-to-indentation)
                  (looking-at (regexp-opt python-indent-dedenters)))
                python-indent-offset
              0)))
          ;; When inside of a string, do nothing. just use the current
          ;; indentation.  XXX: perhaps it would be a good idea to
          ;; invoke standard text indentation here
          ('inside-string
           (goto-char context-start)
           (current-indentation))
          ;; After backslash we have several posibilities
          ('after-backslash
           (cond
            ;; Check if current line is a dot continuation.  For this
            ;; the current line must start with a dot and previous
            ;; line must contain a dot too.
            ((save-excursion
               (back-to-indentation)
               (when (looking-at "\\.")
                 ;; If after moving one line back point is inside a paren it
                 ;; needs to move back until it's not anymore
                 (while (prog2
                            (forward-line -1)
                            (and (not (bobp))
                                 (python-info-ppss-context 'paren))))
                 (goto-char (line-end-position))
                 (while (and (re-search-backward
                              "\\." (line-beginning-position) t)
                             (or (python-info-ppss-context 'comment)
                                 (python-info-ppss-context 'string)
                                 (python-info-ppss-context 'paren))))
                 (if (and (looking-at "\\.")
                          (not (or (python-info-ppss-context 'comment)
                                   (python-info-ppss-context 'string)
                                   (python-info-ppss-context 'paren))))
                     ;; The indentation is the same column of the
                     ;; first matching dot that's not inside a
                     ;; comment, a string or a paren
                     (current-column)
                   ;; No dot found on previous line, just add another
                   ;; indentation level.
                   (+ (current-indentation) python-indent-offset)))))
            ;; Check if prev line is a block continuation
            ((let ((block-continuation-start
                    (python-info-block-continuation-line-p)))
               (when block-continuation-start
                 ;; If block-continuation-start is set jump to that
                 ;; marker and use first column after the block start
                 ;; as indentation value.
                 (goto-char block-continuation-start)
                 (re-search-forward
                  (python-rx block-start (* space))
                  (line-end-position) t)
                 (current-column))))
            ;; Check if current line is an assignment continuation
            ((let ((assignment-continuation-start
                    (python-info-assignment-continuation-line-p)))
               (when assignment-continuation-start
                 ;; If assignment-continuation is set jump to that
                 ;; marker and use first column after the assignment
                 ;; operator as indentation value.
                 (goto-char assignment-continuation-start)
                 (current-column))))
            (t
             (forward-line -1)
             (goto-char (python-info-beginning-of-backlash))
             (if (save-excursion
                   (and
                    (forward-line -1)
                    (goto-char
                     (or (python-info-beginning-of-backlash) (point)))
                    (python-info-line-ends-backslash-p)))
                 ;; The two previous lines ended in a backslash so we must
                 ;; respect previous line indentation.
                 (current-indentation)
               ;; What happens here is that we are dealing with the second
               ;; line of a backslash continuation, in that case we just going
               ;; to add one indentation level.
               (+ (current-indentation) python-indent-offset)))))
          ;; When inside a paren there's a need to handle nesting
          ;; correctly
          ('inside-paren
           (cond
            ;; If current line closes the outtermost open paren use the
            ;; current indentation of the context-start line.
            ((save-excursion
               (skip-syntax-forward "\s" (line-end-position))
               (when (and (looking-at (regexp-opt '(")" "]" "}")))
                          (progn
                            (forward-char 1)
                            (not (python-info-ppss-context 'paren))))
                 (goto-char context-start)
                 (current-indentation))))
            ;; If open paren is contained on a line by itself add another
            ;; indentation level, else look for the first word after the
            ;; opening paren and use it's column position as indentation
            ;; level.
            ((let* ((content-starts-in-newline)
                    (indent
                     (save-excursion
                       (if (setq content-starts-in-newline
                                 (progn
                                   (goto-char context-start)
                                   (forward-char)
                                   (save-restriction
                                     (narrow-to-region
                                      (line-beginning-position)
                                      (line-end-position))
                                     (python-util-forward-comment))
                                   (looking-at "$")))
                           (+ (current-indentation) python-indent-offset)
                         (current-column)))))
               ;; Adjustments
               (cond
                ;; If current line closes a nested open paren de-indent one
                ;; level.
                ((progn
                   (back-to-indentation)
                   (looking-at (regexp-opt '(")" "]" "}"))))
                 (- indent python-indent-offset))
                ;; If the line of the opening paren that wraps the current
                ;; line starts a block add another level of indentation to
                ;; follow new pep8 recommendation. See: http://ur1.ca/5rojx
                ((save-excursion
                   (when (and content-starts-in-newline
                              (progn
                                (goto-char context-start)
                                (back-to-indentation)
                                (looking-at (python-rx block-start))))
                     (+ indent python-indent-offset))))
                (t indent)))))))))))

(defun python-indent-calculate-levels ()
  "Calculate `python-indent-levels' and reset `python-indent-current-level'."
  (let* ((indentation (python-indent-calculate-indentation))
         (remainder (% indentation python-indent-offset))
         (steps (/ (- indentation remainder) python-indent-offset)))
    (setq python-indent-levels (list 0))
    (dotimes (step steps)
      (push (* python-indent-offset (1+ step)) python-indent-levels))
    (when (not (eq 0 remainder))
      (push (+ (* python-indent-offset steps) remainder) python-indent-levels))
    (setq python-indent-levels (nreverse python-indent-levels))
    (setq python-indent-current-level (1- (length python-indent-levels)))))

(defun python-indent-toggle-levels ()
  "Toggle `python-indent-current-level' over `python-indent-levels'."
  (setq python-indent-current-level (1- python-indent-current-level))
  (when (< python-indent-current-level 0)
    (setq python-indent-current-level (1- (length python-indent-levels)))))

(defun python-indent-line (&optional force-toggle)
  "Internal implementation of `python-indent-line-function'.
Uses the offset calculated in
`python-indent-calculate-indentation' and available levels
indicated by the variable `python-indent-levels' to set the
current indentation.

When the variable `last-command' is equal to
`indent-for-tab-command' or FORCE-TOGGLE is non-nil it cycles
levels indicated in the variable `python-indent-levels' by
setting the current level in the variable
`python-indent-current-level'.

When the variable `last-command' is not equal to
`indent-for-tab-command' and FORCE-TOGGLE is nil it calculates
possible indentation levels and saves it in the variable
`python-indent-levels'.  Afterwards it sets the variable
`python-indent-current-level' correctly so offset is equal
to (`nth' `python-indent-current-level' `python-indent-levels')"
  (if (or (and (eq this-command 'indent-for-tab-command)
               (eq last-command this-command))
          force-toggle)
      (if (not (equal python-indent-levels '(0)))
          (python-indent-toggle-levels)
        (python-indent-calculate-levels))
    (python-indent-calculate-levels))
  (beginning-of-line)
  (delete-horizontal-space)
  (indent-to (nth python-indent-current-level python-indent-levels))
  (save-restriction
    (widen)
    (let ((closing-block-point (python-info-closing-block)))
      (when closing-block-point
        (message "Closes %s" (buffer-substring
                              closing-block-point
                              (save-excursion
                                (goto-char closing-block-point)
                                (line-end-position))))))))

(defun python-indent-line-function ()
  "`indent-line-function' for Python mode.
See `python-indent-line' for details."
  (python-indent-line))

(defun python-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (or (python-info-ppss-context 'string)
                      (python-info-ppss-context 'comment)))
             (<= (point-marker) (save-excursion
                                  (back-to-indentation)
                                  (point-marker)))
             (> (current-column) 0))
    (python-indent-line t)
    t))

(defun python-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is  not in between the indentation."
  (interactive "*p")
  (when (not (python-indent-dedent-line))
    (backward-delete-char-untabify arg)))
(put 'python-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun python-indent-region (start end)
  "Indent a python region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (let (word)
              (forward-line -1)
              (back-to-indentation)
              (setq word (current-word))
              (forward-line 1)
              (when word
                (beginning-of-line)
                (delete-horizontal-space)
                (indent-to (python-indent-calculate-indentation)))))
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
              (error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun python-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `python-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (if count
        (setq count (prefix-numeric-value count))
      (setq count python-indent-offset))
    (indent-rigidly start end count)))

(defun python-indent-electric-colon (arg)
  "Insert a colon and maybe de-indent the current line.
With numeric ARG, just insert that many colons.  With
\\[universal-argument], just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (when (and (not arg)
             (eolp)
             (not (equal ?: (char-after (- (point-marker) 2))))
             (not (or (python-info-ppss-context 'string)
                      (python-info-ppss-context 'comment))))
    (let ((indentation (current-indentation))
          (calculated-indentation (python-indent-calculate-indentation)))
      (when (> indentation calculated-indentation)
        (save-excursion
          (indent-line-to calculated-indentation)
          (when (not (python-info-closing-block))
            (indent-line-to indentation)))))))
(put 'python-indent-electric-colon 'delete-selection t)


;;; Navigation

(defvar python-nav-beginning-of-defun-regexp
  (python-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defun python-nav-beginning-of-defun (&optional nodecorators)
  "Move point to `beginning-of-defun'.
When NODECORATORS is non-nil decorators are not included.  This
is the main part of`python-beginning-of-defun-function'
implementation.  Return non-nil if point is moved to the
`beginning-of-defun'."
  (let ((indent-pos (save-excursion
                      (back-to-indentation)
                      (point-marker)))
        (found)
        (include-decorators
         (lambda ()
           (when (not nodecorators)
             (when (save-excursion
                     (forward-line -1)
                     (looking-at (python-rx decorator)))
               (while (and (not (bobp))
                           (forward-line -1)
                           (looking-at (python-rx decorator))))
               (when (not (bobp)) (forward-line 1)))))))
    (if (and (> (point) indent-pos)
             (save-excursion
               (goto-char (line-beginning-position))
               (looking-at python-nav-beginning-of-defun-regexp)))
        (progn
          (goto-char (line-beginning-position))
          (funcall include-decorators)
          (setq found t))
      (goto-char (line-beginning-position))
      (when (re-search-backward python-nav-beginning-of-defun-regexp nil t)
        (setq found t))
      (goto-char (or (python-info-ppss-context 'string) (point)))
      (funcall include-decorators))
    found))

(defun python-beginning-of-defun-function (&optional arg nodecorators)
  "Move point to the beginning of def or class.
With positive ARG move that number of functions forward.  With
negative do the same but backwards.  When NODECORATORS is non-nil
decorators are not included.  Return non-nil if point is moved to the
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (if (> arg 0)
      (dotimes (i arg (python-nav-beginning-of-defun nodecorators)))
    (let ((found))
      (dotimes (i (- arg) found)
        (python-end-of-defun-function)
        (python-util-forward-comment)
        (goto-char (line-end-position))
        (when (not (eobp))
          (setq found
                (python-nav-beginning-of-defun nodecorators)))))))

(defun python-end-of-defun-function ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (decorator-regexp "[[:space:]]*@"))
    (when (looking-at decorator-regexp)
      (while (and (not (eobp))
                  (forward-line 1)
                  (looking-at decorator-regexp))))
    (when (not (looking-at python-nav-beginning-of-defun-regexp))
      (python-beginning-of-defun-function))
    (setq beg-defun-indent (current-indentation))
    (forward-line 1)
    (while (and (forward-line 1)
                (not (eobp))
                (or (not (current-word))
                    ;; This checks if the indentation is less than the base
                    ;; one and if the line is not a comment
                    (or (> (current-indentation) beg-defun-indent)
                        (equal
                         (char-after
                          (+ (point) (current-indentation))) ?#)))))
    (python-util-forward-comment)
    (goto-char (line-beginning-position))))

(defun python-nav-sentence-start ()
  "Move to start of current sentence."
  (interactive "^")
  (while (and (not (back-to-indentation))
              (not (bobp))
              (when (or
                     (save-excursion
                       (forward-line -1)
                       (python-info-line-ends-backslash-p))
                     (python-info-ppss-context 'string)
                     (python-info-ppss-context 'paren))
                (forward-line -1)))))

(defun python-nav-sentence-end ()
  "Move to end of current sentence."
  (interactive "^")
  (while (and (goto-char (line-end-position))
              (not (eobp))
              (when (or
                     (python-info-line-ends-backslash-p)
                     (python-info-ppss-context 'string)
                     (python-info-ppss-context 'paren))
                (forward-line 1)))))

(defun python-nav-backward-sentence (&optional arg)
  "Move backward to start of sentence.  With ARG, do it arg times.
See `python-nav-forward-sentence' for more information."
  (interactive "^p")
  (or arg (setq arg 1))
  (python-nav-forward-sentence (- arg)))

(defun python-nav-forward-sentence (&optional arg)
  "Move forward to next end of sentence.  With ARG, repeat.
With negative argument, move backward repeatedly to start of sentence."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (python-util-forward-comment)
    (python-nav-sentence-end)
    (forward-line 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (python-nav-sentence-end)
    (python-util-forward-comment -1)
    (python-nav-sentence-start)
    (forward-line -1)
    (setq arg (1+ arg))))

(defun python-nav-list-defun-positions (&optional include-type)
  "Make an Alist of defun names and point markers for current buffer.
When optional argument INCLUDE-TYPE is non-nil the type is
included the defun name."
  (let ((defs))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward python-nav-beginning-of-defun-regexp nil t)
          (when (and (not (python-info-ppss-context 'string))
                     (not (python-info-ppss-context 'comment))
                     (not (python-info-ppss-context 'parent)))
            (add-to-list
             'defs (cons
                    (python-info-current-defun include-type)
                    (point-marker)))))
        defs))))

(defun python-nav-read-defun ()
  "Read a defun name of current buffer and return its point marker.
A cons cell with the form (DEFUN-NAME . POINT-MARKER) is returned
when defun is completed, else nil."
  (let ((defs (python-nav-list-defun-positions)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table (mapcar 'car defs)))
      (let ((stringdef
             (read-from-minibuffer
              "Jump to definition: " nil
              minibuffer-local-must-match-map)))
        (when (not (string= stringdef ""))
          (assoc-string stringdef defs))))))

(defun python-nav-jump-to-defun (def)
  "Jump to the definition of DEF in current file."
  (interactive
   (list (python-nav-read-defun)))
  (when (not (called-interactively-p 'interactive))
    (setq def (assoc-string def (python-nav-list-defun-positions))))
  (let ((def-marker (cdr def)))
    (when (markerp def-marker)
      (goto-char (marker-position def-marker))
      (back-to-indentation))))


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

(defcustom python-shell-prompt-regexp ">>> "
  "Regular Expression matching top\-level input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-prompt-block-regexp "[.][.][.] "
  "Regular Expression matching block input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-prompt-output-regexp ""
  "Regular Expression matching output prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular Expression matching pdb input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-send-setup-max-wait 5
  "Seconds to wait for process output before code setup.
If output is received before the especified time then control is
returned in that moment and not after waiting."
  :type 'integer
  :group 'python
  :safe 'integerp)

(defcustom python-shell-process-environment nil
  "List of environment variables for Python shell.
This variable follows the same rules as `process-environment'
since it merges with it before the process creation routines are
called.  When this variable is nil, the Python shell is run with
the default `process-environment'."
  :type '(repeat string)
  :group 'python
  :safe 'listp)

(defcustom python-shell-extra-pythonpaths nil
  "List of extra pythonpaths for Python shell.
The values of this variable are added to the existing value of
PYTHONPATH in the `process-environment' variable."
  :type '(repeat string)
  :group 'python
  :safe 'listp)

(defcustom python-shell-exec-path nil
  "List of path to search for binaries.
This variable follows the same rules as `exec-path' since it
merges with it before the process creation routines are called.
When this variable is nil, the Python shell is run with the
default `exec-path'."
  :type '(repeat string)
  :group 'python
  :safe 'listp)

(defcustom python-shell-virtualenv-path nil
  "Path to virtualenv root.
This variable, when set to a string, makes the values stored in
`python-shell-process-environment' and `python-shell-exec-path'
to be modified properly so shells are started with the specified
virtualenv."
  :type 'string
  :group 'python
  :safe 'stringp)

(defcustom python-shell-setup-codes '(python-shell-completion-setup-code
                                      python-ffap-setup-code
                                      python-eldoc-setup-code)
  "List of code run by `python-shell-send-setup-codes'."
  :type '(repeat symbol)
  :group 'python
  :safe 'listp)

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

(defun python-shell-get-process-name (dedicated)
  "Calculate the appropriate process name for inferior Python process.
If DEDICATED is t and the variable `buffer-file-name' is non-nil
returns a string with the form
`python-shell-buffer-name'[variable `buffer-file-name'] else
returns the value of `python-shell-buffer-name'.  After
calculating the process name adds the buffer name for the process
in the `same-window-buffer-names' list."
  (let ((process-name
         (if (and dedicated
                  buffer-file-name)
             (format "%s[%s]" python-shell-buffer-name buffer-file-name)
           (format "%s" python-shell-buffer-name))))
    (add-to-list 'same-window-buffer-names (purecopy
                                            (format "*%s*" process-name)))
    process-name))

(defun python-shell-internal-get-process-name ()
  "Calculate the appropriate process name for Internal Python process.
The name is calculated from `python-shell-global-buffer-name' and
a hash of all relevant global shell settings in order to ensure
uniqueness for different types of configurations."
  (format "%s [%s]"
          python-shell-internal-buffer-name
          (md5
           (concat
            (python-shell-parse-command)
            python-shell-prompt-regexp
            python-shell-prompt-block-regexp
            python-shell-prompt-output-regexp
            (mapconcat #'symbol-value python-shell-setup-codes "")
            (mapconcat #'identity python-shell-process-environment "")
            (mapconcat #'identity python-shell-extra-pythonpaths "")
            (mapconcat #'identity python-shell-exec-path "")
            (or python-shell-virtualenv-path "")
            (mapconcat #'identity python-shell-exec-path "")))))

(defun python-shell-parse-command ()
  "Calculate the string used to execute the inferior Python process."
  (format "%s %s" python-shell-interpreter python-shell-interpreter-args))

(defun python-shell-calculate-process-environment ()
  "Calculate process environment given `python-shell-virtualenv-path'."
  (let ((process-environment (append
                              python-shell-process-environment
                              process-environment nil))
        (virtualenv (if python-shell-virtualenv-path
                        (directory-file-name python-shell-virtualenv-path)
                      nil)))
    (when python-shell-extra-pythonpaths
      (setenv "PYTHONPATH"
              (format "%s%s%s"
                      (mapconcat 'identity
                                 python-shell-extra-pythonpaths
                                 path-separator)
                      path-separator
                      (or (getenv "PYTHONPATH") ""))))
    (if (not virtualenv)
        process-environment
      (setenv "PYTHONHOME" nil)
      (setenv "PATH" (format "%s/bin%s%s"
                             virtualenv path-separator
                             (or (getenv "PATH") "")))
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defun python-shell-calculate-exec-path ()
  "Calculate exec path given `python-shell-virtualenv-path'."
  (let ((path (append python-shell-exec-path
                      exec-path nil)))
    (if (not python-shell-virtualenv-path)
        path
      (cons (format "%s/bin"
                    (directory-file-name python-shell-virtualenv-path))
            path))))

(defun python-comint-output-filter-function (output)
  "Hook run after content is put into comint buffer.
OUTPUT is a string with the contents of the buffer."
  (ansi-color-filter-apply output))

(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for Python inferior process.
Runs a Python interpreter as a subprocess of Emacs, with Python
I/O through an Emacs buffer.  Variables
`python-shell-interpreter' and `python-shell-interpreter-args'
controls which Python interpreter is run.  Variables
`python-shell-prompt-regexp',
`python-shell-prompt-output-regexp',
`python-shell-prompt-block-regexp',
`python-shell-completion-setup-code',
`python-shell-completion-string-code',
`python-shell-completion-module-string-code',
`python-eldoc-setup-code', `python-eldoc-string-code',
`python-ffap-setup-code' and `python-ffap-string-code' can
customize this mode for different Python interpreters.

You can also add additional setup code to be run at
initialization of the interpreter via `python-shell-setup-codes'
variable.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (set-syntax-table python-mode-syntax-table)
  (setq mode-line-process '(":%s"))
  (setq comint-prompt-regexp (format "^\\(?:%s\\|%s\\|%s\\)"
                                     python-shell-prompt-regexp
                                     python-shell-prompt-block-regexp
                                     python-shell-prompt-pdb-regexp))
  (make-local-variable 'comint-output-filter-functions)
  (add-hook 'comint-output-filter-functions
            'python-comint-output-filter-function)
  (add-hook 'comint-output-filter-functions
            'python-pdbtrack-comint-output-filter-function)
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-shell-compilation-regexp-alist)
  (define-key inferior-python-mode-map [remap complete-symbol]
    'completion-at-point)
  (add-hook 'completion-at-point-functions
            'python-shell-completion-complete-at-point nil 'local)
  (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
               'python-shell-completion-complete-at-point)
  (define-key inferior-python-mode-map (kbd "<tab>")
    'python-shell-completion-complete-or-indent)
  (compilation-shell-minor-mode 1))

(defun python-shell-make-comint (cmd proc-name &optional pop)
  "Create a python shell comint buffer.
CMD is the python command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `inferior-python-mode' is activated.  If POP is
non-nil the buffer is shown."
  (save-excursion
    (let* ((proc-buffer-name (format "*%s*" proc-name))
           (process-environment (python-shell-calculate-process-environment))
           (exec-path (python-shell-calculate-exec-path)))
      (when (not (comint-check-proc proc-buffer-name))
        (let* ((cmdlist (split-string-and-unquote cmd))
               (buffer (apply 'make-comint proc-name (car cmdlist) nil
                              (cdr cmdlist)))
               (current-buffer (current-buffer)))
          (with-current-buffer buffer
            (inferior-python-mode)
            (python-util-clone-local-variables current-buffer))))
      (when pop
        (pop-to-buffer proc-buffer-name))
      proc-buffer-name)))

(defun run-python (dedicated cmd)
  "Run an inferior Python process.
Input and output via buffer named after
`python-shell-buffer-name'.  If there is a process already
running in that buffer, just switch to it.
With argument, allows you to define DEDICATED, so a dedicated
process for the current buffer is open, and define CMD so you can
edit the command used to call the interpreter (default is value
of `python-shell-interpreter' and arguments defined in
`python-shell-interpreter-args').  Runs the hook
`inferior-python-mode-hook' (after the `comint-mode-hook' is
run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (y-or-n-p "Make dedicated process? ")
        (read-string "Run Python: " (python-shell-parse-command)))
     (list nil (python-shell-parse-command))))
  (python-shell-make-comint cmd (python-shell-get-process-name dedicated) t)
  dedicated)

(defun run-python-internal ()
  "Run an inferior Internal Python process.
Input and output via buffer named after
`python-shell-internal-buffer-name' and what
`python-shell-internal-get-process-name' returns.  This new kind
of shell is intended to be used for generic communication related
to defined configurations.  The main difference with global or
dedicated shells is that these ones are attached to a
configuration, not a buffer.  This means that can be used for
example to retrieve the sys.path and other stuff, without messing
with user shells.  Runs the hook
`inferior-python-mode-hook' (after the `comint-mode-hook' is
run).  \(Type \\[describe-mode] in the process buffer for a list
of commands.)"
  (interactive)
  (set-process-query-on-exit-flag
   (get-buffer-process
    (python-shell-make-comint
     (python-shell-parse-command)
     (python-shell-internal-get-process-name))) nil))

(defun python-shell-get-process ()
  "Get inferior Python process for current buffer and return it."
  (let* ((dedicated-proc-name (python-shell-get-process-name t))
         (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
         (global-proc-name  (python-shell-get-process-name nil))
         (global-proc-buffer-name (format "*%s*" global-proc-name))
         (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
         (global-running (comint-check-proc global-proc-buffer-name)))
    ;; Always prefer dedicated
    (get-buffer-process (or (and dedicated-running dedicated-proc-buffer-name)
                            (and global-running global-proc-buffer-name)))))

(defun python-shell-get-or-create-process ()
  "Get or create an inferior Python process for current buffer and return it."
  (let* ((old-buffer (current-buffer))
         (dedicated-proc-name (python-shell-get-process-name t))
         (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
         (global-proc-name  (python-shell-get-process-name nil))
         (global-proc-buffer-name (format "*%s*" global-proc-name))
         (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
         (global-running (comint-check-proc global-proc-buffer-name))
         (current-prefix-arg 4))
    (when (and (not dedicated-running) (not global-running))
      (if (call-interactively 'run-python)
          (setq dedicated-running t)
        (setq global-running t)))
    ;; Always prefer dedicated
    (switch-to-buffer old-buffer)
    (get-buffer-process (if dedicated-running
                            dedicated-proc-buffer-name
                          global-proc-buffer-name))))

(defvar python-shell-internal-buffer nil
  "Current internal shell buffer for the current buffer.
This is really not necessary at all for the code to work but it's
there for compatibility with CEDET.")
(make-variable-buffer-local 'python-shell-internal-buffer)

(defun python-shell-internal-get-or-create-process ()
  "Get or create an inferior Internal Python process."
  (let* ((proc-name (python-shell-internal-get-process-name))
         (proc-buffer-name (format "*%s*" proc-name)))
    (run-python-internal)
    (setq python-shell-internal-buffer proc-buffer-name)
    (get-buffer-process proc-buffer-name)))

(define-obsolete-function-alias
  'python-proc 'python-shell-internal-get-or-create-process "23.3")

(define-obsolete-variable-alias
  'python-buffer 'python-shell-internal-buffer "23.3")

(defun python-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When MSG is non-nil messages the first line of STRING."
  (interactive "sPython command: ")
  (let ((process (or process (python-shell-get-or-create-process)))
        (lines (split-string string "\n" t)))
    (when msg
      (message (format "Sent: %s..." (nth 0 lines))))
    (if (> (length lines) 1)
        (let* ((temp-file-name (make-temp-file "py"))
               (file-name (or (buffer-file-name) temp-file-name)))
          (with-temp-file temp-file-name
            (insert string)
            (delete-trailing-whitespace))
          (python-shell-send-file file-name process temp-file-name))
      (comint-send-string process string)
      (when (or (not (string-match "\n$" string))
                (string-match "\n[ \t].*\n?$" string))
        (comint-send-string process "\n")))))

(defun python-shell-send-string-no-output (string &optional process msg)
  "Send STRING to PROCESS and inhibit output.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* ((output-buffer)
         (process (or process (python-shell-get-or-create-process)))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output-buffer (concat output-buffer string))
                      "")))))
    (python-shell-send-string string process msg)
    (accept-process-output process)
    (replace-regexp-in-string
     (if (> (length python-shell-prompt-output-regexp) 0)
         (format "\n*%s$\\|^%s\\|\n$"
                 python-shell-prompt-regexp
                 (or python-shell-prompt-output-regexp ""))
       (format "\n*$\\|^%s\\|\n$"
               python-shell-prompt-regexp))
     "" output-buffer)))

(defun python-shell-internal-send-string (string)
  "Send STRING to the Internal Python interpreter.
Returns the output.  See `python-shell-send-string-no-output'."
  (python-shell-send-string-no-output
   ;; Makes this function compatible with the old
   ;; python-send-receive. (At least for CEDET).
   (replace-regexp-in-string "_emacs_out +" "" string)
   (python-shell-internal-get-or-create-process) nil))

(define-obsolete-function-alias
  'python-send-receive 'python-shell-internal-send-string "23.3")

(define-obsolete-function-alias
  'python-send-string 'python-shell-internal-send-string "23.3")

(defun python-shell-send-region (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (let ((deactivate-mark nil))
    (python-shell-send-string (buffer-substring start end) nil t)))

(defun python-shell-send-buffer ()
  "Send the entire buffer to inferior Python process."
  (interactive)
  (save-restriction
    (widen)
    (python-shell-send-region (point-min) (point-max))))

(defun python-shell-send-defun (arg)
  "Send the current defun to inferior Python process.
When argument ARG is non-nil sends the innermost defun."
  (interactive "P")
  (save-excursion
    (python-shell-send-region
     (progn
       (or (python-beginning-of-defun-function)
           (progn (beginning-of-line) (point-marker))))
     (progn
       (or (python-end-of-defun-function)
           (progn (end-of-line) (point-marker)))))))

(defun python-shell-send-file (file-name &optional process temp-file-name)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME."
  (interactive "fFile to send: ")
  (let* ((process (or process (python-shell-get-or-create-process)))
         (temp-file-name (when temp-file-name
                           (expand-file-name temp-file-name)))
         (file-name (or (expand-file-name file-name) temp-file-name)))
    (when (not file-name)
      (error "If FILE-NAME is nil then TEMP-FILE-NAME must be non-nil"))
    (python-shell-send-string
     (format
      (concat "__pyfile = open('''%s''');"
              "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
              "__pyfile.close()")
      (or temp-file-name file-name) file-name)
     process)))

(defun python-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer (process-buffer (python-shell-get-or-create-process)) t))

(defun python-shell-send-setup-code ()
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`python-shell-setup-codes' list."
  (let ((msg "Sent %s")
        (process (get-buffer-process (current-buffer))))
    (accept-process-output process python-shell-send-setup-max-wait)
    (dolist (code python-shell-setup-codes)
      (when code
        (message (format msg code))
        (python-shell-send-string
         (symbol-value code) process)))))

(add-hook 'inferior-python-mode-hook
          #'python-shell-send-setup-code)


;;; Shell completion

(defcustom python-shell-completion-setup-code
  "try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
  :group 'python)

(defcustom python-shell-completion-string-code
  "';'.join(__COMPLETER_all_completions('''%s'''))\n"
  "Python code used to get a string of completions separated by semicolons."
  :type 'string
  :group 'python)

(defcustom python-shell-completion-module-string-code ""
  "Python code used to get completions separated by semicolons for imports.

For IPython v0.11, add the following line to
`python-shell-completion-setup-code':

from IPython.core.completerlib import module_completion

and use the following as the value of this variable:

';'.join(module_completion('''%s'''))\n"
  :type 'string
  :group 'python)

(defcustom python-shell-completion-pdb-string-code
  "';'.join(globals().keys() + locals().keys())"
  "Python code used to get completions separated by semicolons for [i]pdb."
  :type 'string
  :group 'python)

(defun python-shell-completion--get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (with-current-buffer (process-buffer process)
    (let ((completions (python-shell-send-string-no-output
                        (format completion-code input) process)))
      (when (> (length completions) 2)
        (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun python-shell-completion--do-completion-at-point (process)
  "Do completion at point for PROCESS."
  (with-syntax-table python-dotty-syntax-table
    (let* ((beg
            (save-excursion
              (let* ((paren-depth (car (syntax-ppss)))
                     (syntax-string "w_")
                     (syntax-list (string-to-syntax syntax-string)))
                ;; Stop scanning for the beginning of the completion subject
                ;; after the char before point matches a delimiter
                (while (member (car (syntax-after (1- (point)))) syntax-list)
                  (skip-syntax-backward syntax-string)
                  (when (or (equal (char-before) ?\))
                            (equal (char-before) ?\"))
                    (forward-char -1))
                  (while (or
                          ;; honor initial paren depth
                          (> (car (syntax-ppss)) paren-depth)
                          (python-info-ppss-context 'string))
                    (forward-char -1))))
              (point)))
           (end (point))
           (line (buffer-substring-no-properties (point-at-bol) end))
           (input (buffer-substring-no-properties beg end))
           ;; Get the last prompt for the inferior process buffer. This is
           ;; used for the completion code selection heuristic.
           (prompt
            (with-current-buffer (process-buffer process)
              (buffer-substring-no-properties
               (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay))))
           (completion-code
            ;; Check wether a prompt matches a pdb string, an import statement
            ;; or just the standard prompt and use the correct
            ;; python-shell-completion-*-code string
            (cond ((and (> (length python-shell-completion-pdb-string-code) 0)
                        (string-match
                         (concat "^" python-shell-prompt-pdb-regexp) prompt))
                   python-shell-completion-pdb-string-code)
                  ((and (>
                         (length python-shell-completion-module-string-code) 0)
                        (string-match
                         (concat "^" python-shell-prompt-regexp) prompt)
                        (string-match "^\\(from\\|import\\)[ \t]" line))
                   python-shell-completion-module-string-code)
                  ((string-match
                    (concat "^" python-shell-prompt-regexp) prompt)
                   python-shell-completion-string-code)
                  (t nil)))
           (completions
            (and completion-code (> (length input) 0)
                 (python-shell-completion--get-completions
                  input process completion-code))))
      (list beg end completions))))

(defun python-shell-completion-complete-at-point ()
  "Perform completion at point in inferior Python process."
  (interactive)
  (and comint-last-prompt-overlay
       (> (point-marker) (overlay-end comint-last-prompt-overlay))
       (python-shell-completion--do-completion-at-point
        (get-buffer-process (current-buffer)))))

(defun python-shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace indent.  If not try
to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point-marker)))
      (indent-for-tab-command)
    (comint-dynamic-complete)))


;;; PDB Track integration

(defcustom python-pdbtrack-activate t
  "Non-nil makes python shell enable pdbtracking."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-pdbtrack-stacktrace-info-regexp
  "^> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular Expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :group 'python
  :safe 'stringp)

(defvar python-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`python-pdbtrack-set-tracked-buffer' instead.")
(make-variable-buffer-local 'python-pdbtrack-tracked-buffer)

(defvar python-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")
(make-variable-buffer-local 'python-pdbtrack-buffers-to-kill)

(defun python-pdbtrack-set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the `python-pdbtrack-tracked-buffer' variable.
Returns the tracked buffer."
  (let ((file-buffer (get-file-buffer file-name)))
    (if file-buffer
        (setq python-pdbtrack-tracked-buffer file-buffer)
      (setq file-buffer (find-file-noselect file-name))
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
              (goto-char (point-min))
              ;; OK, this sucked but now it became a cool hack. The
              ;; stacktrace information normally is on the first line
              ;; but in some cases (like when doing a step-in) it is
              ;; on the second.
              (when (or (looking-at python-pdbtrack-stacktrace-info-regexp)
                        (and
                         (forward-line)
                         (looking-at python-pdbtrack-stacktrace-info-regexp)))
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

(defun python-completion-complete-at-point ()
  "Complete current symbol at point.
For this to work the best as possible you should call
`python-shell-send-buffer' from time to time so context in
inferior python process is updated properly."
  (interactive)
  (let ((process (python-shell-get-process)))
    (if (not process)
        (error "Completion needs an inferior Python process running.")
      (python-shell-completion--do-completion-at-point process))))

(add-to-list 'debug-ignored-errors
             "^Completion needs an inferior Python process running.")


;;; Fill paragraph

(defcustom python-fill-comment-function 'python-fill-comment
  "Function to fill comments.
This is the function used by `python-fill-paragraph-function' to
fill comments."
  :type 'symbol
  :group 'python
  :safe 'symbolp)

(defcustom python-fill-string-function 'python-fill-string
  "Function to fill strings.
This is the function used by `python-fill-paragraph-function' to
fill strings."
  :type 'symbol
  :group 'python
  :safe 'symbolp)

(defcustom python-fill-decorator-function 'python-fill-decorator
  "Function to fill decorators.
This is the function used by `python-fill-paragraph-function' to
fill decorators."
  :type 'symbol
  :group 'python
  :safe 'symbolp)

(defcustom python-fill-paren-function 'python-fill-paren
  "Function to fill parens.
This is the function used by `python-fill-paragraph-function' to
fill parens."
  :type 'symbol
  :group 'python
  :safe 'symbolp)

(defun python-fill-paragraph-function (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified."
  (interactive "P")
  (save-excursion
    (back-to-indentation)
    (cond
     ;; Comments
     ((funcall python-fill-comment-function justify))
     ;; Strings/Docstrings
     ((save-excursion (skip-chars-forward "\"'uUrR")
                      (python-info-ppss-context 'string))
      (funcall python-fill-string-function justify))
     ;; Decorators
     ((equal (char-after (save-excursion
                           (back-to-indentation)
                           (point-marker))) ?@)
      (funcall python-fill-decorator-function justify))
     ;; Parens
     ((or (python-info-ppss-context 'paren)
          (looking-at (python-rx open-paren))
          (save-excursion
            (skip-syntax-forward "^(" (line-end-position))
            (looking-at (python-rx open-paren))))
      (funcall python-fill-paren-function justify))
     (t t))))

(defun python-fill-comment (&optional justify)
  "Comment fill function for `python-fill-paragraph-function'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (fill-comment-paragraph justify))

(defun python-fill-string (&optional justify)
  "String fill function for `python-fill-paragraph-function'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (let ((marker (point-marker))
        (string-start-marker
         (progn
           (skip-chars-forward "\"'uUrR")
           (goto-char (python-info-ppss-context 'string))
           (skip-chars-forward "\"'uUrR")
           (point-marker)))
        (reg-start (line-beginning-position))
        (string-end-marker
         (progn
           (while (python-info-ppss-context 'string)
             (goto-char (1+ (point-marker))))
           (skip-chars-backward "\"'")
           (point-marker)))
        (reg-end (line-end-position))
        (fill-paragraph-function))
    (save-restriction
      (narrow-to-region reg-start reg-end)
      (save-excursion
        (goto-char string-start-marker)
        (delete-region (point-marker) (progn
                                        (skip-syntax-forward "> ")
                                        (point-marker)))
        (goto-char string-end-marker)
        (delete-region (point-marker) (progn
                                        (skip-syntax-backward "> ")
                                        (point-marker)))
        (save-excursion
          (goto-char marker)
          (fill-paragraph justify))
        ;; If there is a newline in the docstring lets put triple
        ;; quote in it's own line to follow pep 8
        (when (save-excursion
                (re-search-backward "\n" string-start-marker t))
          (newline)
          (newline-and-indent))
        (fill-paragraph justify)))) t)

(defun python-fill-decorator (&optional justify)
  "Decorator fill function for `python-fill-paragraph-function'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  t)

(defun python-fill-paren (&optional justify)
  "Paren fill function for `python-fill-paragraph-function'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (python-info-ppss-context 'paren)
                          (goto-char (1- (point-marker))))
                        (point-marker)
                        (line-beginning-position))
                      (progn
                        (when (not (python-info-ppss-context 'paren))
                          (end-of-line)
                          (when (not (python-info-ppss-context 'paren))
                            (skip-syntax-backward "^)")))
                        (while (python-info-ppss-context 'paren)
                          (goto-char (1+ (point-marker))))
                        (point-marker)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (python-indent-line)
      (goto-char (line-end-position)))) t)


;;; Skeletons

(defcustom python-skeleton-autoinsert nil
  "Non-nil means template skeletons will be automagically inserted.
This happens when pressing \"if<SPACE>\", for example, to prompt for
the if condition."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defvar python-skeleton-available '()
  "Internal list of available skeletons.")

(define-abbrev-table 'python-mode-abbrev-table ()
  "Abbrev table for Python mode."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*"
  ;; Only expand in code.
  :enable-function (lambda ()
                     (and
                      (not (or (python-info-ppss-context 'string)
                               (python-info-ppss-context 'comment)))
                      python-skeleton-autoinsert)))

(defmacro python-skeleton-define (name doc &rest skel)
  "Define a `python-mode' skeleton using NAME DOC and SKEL.
The skeleton will be bound to python-skeleton-NAME and will
be added to `python-mode-abbrev-table'."
  (let* ((name (symbol-name name))
         (function-name (intern (concat "python-skeleton-" name))))
    `(progn
       (define-abbrev python-mode-abbrev-table ,name "" ',function-name)
       (setq python-skeleton-available
             (cons ',function-name python-skeleton-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert %s statement." name))
         ,@skel))))
(put 'python-skeleton-define 'lisp-indent-function 2)

(defmacro python-define-auxiliary-skeleton (name doc &optional &rest skel)
  "Define a `python-mode' auxiliary skeleton using NAME DOC and SKEL.
The skeleton will be bound to python-skeleton-NAME."
  (let* ((name (symbol-name name))
         (function-name (intern (concat "python-skeleton--" name)))
         (msg (format
               "Add '%s' clause? " name)))
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
(put 'python-define-auxiliary-skeleton 'lisp-indent-function 2)

(python-define-auxiliary-skeleton else nil)

(python-define-auxiliary-skeleton except nil)

(python-define-auxiliary-skeleton finally nil)

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
  "def " str " ("  ("Parameter, %s: "
                    (unless (equal ?\( (char-before)) ", ")
                    str) "):" \n
                    "\"\"\"" - "\"\"\"" \n
                    > _ \n)

(python-skeleton-define class nil
  "Class name: "
  "class " str " (" ("Inheritance, %s: "
                     (unless (equal ?\( (char-before)) ", ")
                     str)
  & ")" | -2
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)

(defun python-skeleton-add-menu-items ()
  "Add menu items to Python->Skeletons menu."
  (let ((skeletons (sort python-skeleton-available 'string<))
        (items))
    (dolist (skeleton skeletons)
      (easy-menu-add-item
       nil '("Python" "Skeletons")
       `[,(format
           "Insert %s" (caddr (split-string (symbol-name skeleton) "-")))
         ,skeleton t]))))

;;; FFAP

(defcustom python-ffap-setup-code
  "def __FFAP_get_module_path(module):
    try:
        import os
        path = __import__(module).__file__
        if path[-4:] == '.pyc' and os.path.exists(path[0:-1]):
            path = path[:-1]
        return path
    except:
        return ''"
  "Python code to get a module path."
  :type 'string
  :group 'python)

(defcustom python-ffap-string-code
  "__FFAP_get_module_path('''%s''')\n"
  "Python code used to get a string with the path of a module."
  :type 'string
  :group 'python)

(defun python-ffap-module-path (module)
  "Function for `ffap-alist' to return path for MODULE."
  (let ((process (or
                  (and (eq major-mode 'inferior-python-mode)
                       (get-buffer-process (current-buffer)))
                  (python-shell-get-process))))
    (if (not process)
        nil
      (let ((module-file
             (python-shell-send-string-no-output
              (format python-ffap-string-code module) process)))
        (when module-file
          (substring-no-properties module-file 1 -1))))))

(eval-after-load "ffap"
  '(progn
     (push '(python-mode . python-ffap-module-path) ffap-alist)
     (push '(inferior-python-mode . python-ffap-module-path) ffap-alist)))


;;; Code check

(defcustom python-check-command
  "pyflakes"
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

(defun python-check (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.  See
`python-check-command' for the default."
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
  (let ((process-environment (python-shell-calculate-process-environment))
        (exec-path (python-shell-calculate-exec-path)))
    (compilation-start command nil
                       (lambda (mode-name)
                         (format python-check-buffer-name command)))))


;;; Eldoc

(defcustom python-eldoc-setup-code
  "def __PYDOC_get_help(obj):
    try:
        import inspect
        if hasattr(obj, 'startswith'):
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
                args = inspect.formatargspec(
                    *inspect.getargspec(target)
                )
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args
                )
        else:
            doc = doc.splitlines()[0]
    except:
        doc = ''
    try:
        exec('print doc')
    except SyntaxError:
        print(doc)"
  "Python code to setup documentation retrieval."
  :type 'string
  :group 'python)

(defcustom python-eldoc-string-code
  "__PYDOC_get_help('''%s''')\n"
  "Python code used to get a string with the documentation of an object."
  :type 'string
  :group 'python)

(defun python-eldoc--get-doc-at-point (&optional force-input force-process)
  "Internal implementation to get documentation at point.
If not FORCE-INPUT is passed then what `current-word' returns
will be used.  If not FORCE-PROCESS is passed what
`python-shell-get-process' returns is used."
  (let ((process (or force-process (python-shell-get-process))))
    (if (not process)
        "Eldoc needs an inferior Python process running."
      (let* ((current-defun (python-info-current-defun))
             (input (or force-input
                        (with-syntax-table python-dotty-syntax-table
                          (if (not current-defun)
                              (current-word)
                            (concat current-defun "." (current-word))))))
             (ppss (syntax-ppss))
             (help (when (and
                          input
                          (not (string= input (concat current-defun ".")))
                          (not (or (python-info-ppss-context 'string ppss)
                                   (python-info-ppss-context 'comment ppss))))
                     (when (string-match
                            (concat
                             (regexp-quote (concat current-defun "."))
                             "self\\.") input)
                       (with-temp-buffer
                         (insert input)
                         (goto-char (point-min))
                         (forward-word)
                         (forward-char)
                         (delete-region
                          (point-marker) (search-forward "self."))
                         (setq input (buffer-substring
                                      (point-min) (point-max)))))
                     (python-shell-send-string-no-output
                      (format python-eldoc-string-code input) process))))
        (with-current-buffer (process-buffer process)
          (when comint-last-prompt-overlay
            (delete-region comint-last-input-end
                           (overlay-start comint-last-prompt-overlay))))
        (when (and help
                   (not (string= help "\n")))
          help)))))

(defun python-eldoc-function ()
  "`eldoc-documentation-function' for Python.
For this to work the best as possible you should call
`python-shell-send-buffer' from time to time so context in
inferior python process is updated properly."
  (python-eldoc--get-doc-at-point))

(defun python-eldoc-at-point (symbol)
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol."
  (interactive
   (let ((symbol (with-syntax-table python-dotty-syntax-table
                   (current-word)))
         (enable-recursive-minibuffers t))
     (list (read-string (if symbol
                            (format "Describe symbol (default %s): " symbol)
                          "Describe symbol: ")
                        nil nil symbol))))
  (let ((process (python-shell-get-process)))
    (if (not process)
        (message "Eldoc needs an inferior Python process running.")
      (message (python-eldoc--get-doc-at-point symbol process)))))


;;; Imenu

(defcustom python-imenu-include-defun-type t
  "Non-nil make imenu items to include its type."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-imenu-make-tree t
  "Non-nil make imenu to build a tree menu.
Set to nil for speed."
  :type 'boolean
  :group 'python
  :safe 'booleanp)

(defcustom python-imenu-subtree-root-label "<Jump to %s>"
  "Label displayed to navigate to root from a subtree.
It can contain a \"%s\" which will be replaced with the root name."
  :type 'string
  :group 'python
  :safe 'stringp)

(defvar python-imenu-index-alist nil
  "Calculated index tree for imenu.")

(defun python-imenu-tree-assoc (keylist tree)
  "Using KEYLIST traverse TREE."
  (if keylist
      (python-imenu-tree-assoc (cdr keylist)
                               (ignore-errors (assoc (car keylist) tree)))
    tree))

(defun python-imenu-make-element-tree (element-list full-element plain-index)
  "Make a tree from plain alist of module names.
ELEMENT-LIST is the defun name splitted by \".\" and FULL-ELEMENT
is the same thing, the difference is that FULL-ELEMENT remains
untouched in all recursive calls.
Argument PLAIN-INDEX is the calculated plain index used to build the tree."
  (when (not (python-imenu-tree-assoc full-element python-imenu-index-alist))
    (when element-list
      (let* ((subelement-point (cdr (assoc
                                     (mapconcat #'identity full-element ".")
                                     plain-index)))
             (subelement-name (car element-list))
             (subelement-position (python-util-position
                                   subelement-name full-element))
             (subelement-path (when subelement-position
                                (butlast
                                 full-element
                                 (- (length full-element)
                                    subelement-position)))))
        (let ((path-ref (python-imenu-tree-assoc subelement-path
                                                 python-imenu-index-alist)))
          (if (not path-ref)
              (push (cons subelement-name subelement-point)
                    python-imenu-index-alist)
            (when (not (listp (cdr path-ref)))
              ;; Modifiy root cdr to be a list
              (setcdr path-ref
                      (list (cons (format python-imenu-subtree-root-label
                                          (car path-ref))
                                  (cdr (assoc
                                        (mapconcat #'identity
                                                   subelement-path ".")
                                        plain-index))))))
            (when (not (assoc subelement-name path-ref))
              (push (cons subelement-name subelement-point) (cdr path-ref))))))
      (python-imenu-make-element-tree (cdr element-list)
                                      full-element plain-index))))

(defun python-imenu-make-tree (index)
  "Build the imenu alist tree from plain INDEX.

The idea of this function is that given the alist:

 '((\"Test\" . 100)
   (\"Test.__init__\" . 200)
   (\"Test.some_method\" . 300)
   (\"Test.some_method.another\" . 400)
   (\"Test.something_else\" . 500)
   (\"test\" . 600)
   (\"test.reprint\" . 700)
   (\"test.reprint\" . 800))

This tree gets built:

 '((\"Test\" . ((\"jump to...\" . 100)
                (\"__init__\" . 200)
                (\"some_method\" . ((\"jump to...\" . 300)
                                    (\"another\" . 400)))
                (\"something_else\" . 500)))
   (\"test\" . ((\"jump to...\" . 600)
                (\"reprint\" . 700)
                (\"reprint\" . 800))))

Internally it uses `python-imenu-make-element-tree' to create all
branches for each element."
  (setq python-imenu-index-alist nil)
  (mapc (lambda (element)
          (python-imenu-make-element-tree element element index))
        (mapcar (lambda (element)
                  (split-string (car element) "\\." t)) index))
  python-imenu-index-alist)

(defun python-imenu-create-index ()
  "`imenu-create-index-function' for Python."
  (let ((index
         (python-nav-list-defun-positions python-imenu-include-defun-type)))
    (if python-imenu-make-tree
        (python-imenu-make-tree index)
      index)))


;;; Misc helpers

(defun python-info-current-defun (&optional include-type)
  "Return name of surrounding function with Python compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function is compatible to be used as
`add-log-current-defun-function' since it returns nil if point is
not inside a defun."
  (let ((names '())
        (min-indent)
        (first-run t))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (line-end-position))
        (python-util-forward-comment -1)
        (setq min-indent (current-indentation))
        (while (python-beginning-of-defun-function 1 t)
          (when (or (< (current-indentation) min-indent)
                    first-run)
            (setq first-run nil)
            (setq min-indent (current-indentation))
            (looking-at python-nav-beginning-of-defun-regexp)
            (setq names (cons
                         (if (not include-type)
                             (match-string-no-properties 1)
                           (mapconcat 'identity
                                      (split-string
                                       (match-string-no-properties 0)) " "))
                         names))))))
    (when names
      (mapconcat (lambda (string) string) names "."))))

(defun python-info-closing-block ()
  "Return the point of the block the current line closes."
  (let ((closing-word (save-excursion
                        (back-to-indentation)
                        (current-word)))
        (indentation (current-indentation)))
    (when (member closing-word python-indent-dedenters)
      (save-excursion
        (forward-line -1)
        (while (and (> (current-indentation) indentation)
                    (not (bobp))
                    (not (back-to-indentation))
                    (forward-line -1)))
        (back-to-indentation)
        (cond
         ((not (equal indentation (current-indentation))) nil)
         ((string= closing-word "elif")
          (when (member (current-word) '("if" "elif"))
            (point-marker)))
         ((string= closing-word "else")
          (when (member (current-word) '("if" "elif" "except" "for" "while"))
            (point-marker)))
         ((string= closing-word "except")
          (when (member (current-word) '("try"))
            (point-marker)))
         ((string= closing-word "finally")
          (when (member (current-word) '("except" "else"))
            (point-marker))))))))

(defun python-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
    (save-restriction
      (widen)
      (when line-number
        (goto-char line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (python-info-ppss-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker)))))

(defun python-info-beginning-of-backlash (&optional line-number)
  "Return the point where the backlashed line starts."
  (save-excursion
    (save-restriction
      (widen)
      (when line-number
        (goto-char line-number))
      (when (python-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (python-info-ppss-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker)))))

(defun python-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((context-type (progn
                             (back-to-indentation)
                             (python-info-ppss-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (python-info-ppss-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (when (equal (python-info-ppss-context-type) 'paren)
                 (python-util-forward-comment -1)
                 (python-util-forward-comment -1)
                 (point-marker)))
              ((or (equal context-type 'comment)
                   (equal context-type 'string))
               ;; move forward an roll again
               (goto-char context-start)
               (python-util-forward-comment)
               (python-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way we are
               ;; dealing with a continuation line is that previous line
               ;; contains a backslash, and this can only be the previous line
               ;; from current
               (back-to-indentation)
               (python-util-forward-comment -1)
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

(defun python-info-assignment-continuation-line-p ()
  "Check if current line is a continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (python-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (and (not (looking-at (python-rx block-start)))
                 (and (re-search-forward (python-rx not-simple-operator
                                                    assignment-operator
                                                    not-simple-operator)
                                         (line-end-position) t)
                      (not (or (python-info-ppss-context 'string)
                               (python-info-ppss-context 'paren)
                               (python-info-ppss-context 'comment)))))
        (skip-syntax-forward "\s")
        (point-marker)))))

(defun python-info-ppss-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be 'comment, 'string or 'paren.  It returns the start
character address of the specified TYPE."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (case type
      ('comment
       (and (nth 4 ppss)
            (nth 8 ppss)))
      ('string
       (nth 8 ppss))
      ('paren
       (nth 1 ppss))
      (t nil))))

(defun python-info-ppss-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be 'comment, 'string or 'paren."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((and (nth 4 ppss)
           (nth 8 ppss))
      'comment)
     ((nth 8 ppss)
      'string)
     ((nth 1 ppss)
      'paren)
     (t nil))))


;;; Utility functions

(defun python-util-position (item seq)
  "Find the first occurrence of ITEM in SEQ.
Return the index of the matching item, or nil if not found."
  (let ((member-result (member item seq)))
    (when member-result
      (- (length seq) (length member-result)))))

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

(defun python-util-forward-comment (&optional direction)
  "Python mode specific version of `forward-comment'."
  (let ((comment-start (python-info-ppss-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))


;;;###autoload
(define-derived-mode python-mode fundamental-mode "Python"
  "Major mode for editing Python files.

\\{python-mode-map}
Entry to this mode calls the value of `python-mode-hook'
if that value is non-nil."
  (set (make-local-variable 'tab-width) 8)
  (set (make-local-variable 'indent-tabs-mode) nil)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-keywords . python-font-lock-syntactic-keywords)))

  (set (make-local-variable 'indent-line-function)
       #'python-indent-line-function)
  (set (make-local-variable 'indent-region-function) #'python-indent-region)

  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function)
       'python-fill-paragraph-function)

  (set (make-local-variable 'beginning-of-defun-function)
       #'python-beginning-of-defun-function)
  (set (make-local-variable 'end-of-defun-function)
       #'python-end-of-defun-function)

  (add-hook 'completion-at-point-functions
            'python-completion-complete-at-point nil 'local)

  (setq imenu-create-index-function #'python-imenu-create-index)

  (set (make-local-variable 'add-log-current-defun-function)
       #'python-info-current-defun)

  (set (make-local-variable 'skeleton-further-elements)
       '((abbrev-mode nil)
         (< '(backward-delete-char-untabify (min python-indent-offset
                                                 (current-column))))
         (^ '(- (1+ (current-indentation))))))

  (set (make-local-variable 'eldoc-documentation-function)
       #'python-eldoc-function)

  (add-to-list 'hs-special-modes-alist
               `(python-mode "^\\s-*\\(?:def\\|class\\)\\>" nil "#"
                             ,(lambda (arg)
                                (python-end-of-defun-function)) nil))

  (set (make-local-variable 'mode-require-final-newline) t)

  (set (make-local-variable 'outline-regexp)
       (python-rx (* space) block-start))
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\n")
  (set (make-local-variable 'outline-level)
       #'(lambda ()
           "`outline-level' function for Python mode."
           (1+ (/ (current-indentation) python-indent-offset))))

  (python-skeleton-add-menu-items)

  (when python-indent-guess-indent-offset
    (python-indent-guess-indent-offset)))


(provide 'python)
;;; python.el ends here
