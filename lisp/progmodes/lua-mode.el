;;; lua-mode.el --- a major-mode for editing Lua scripts  -*- lexical-binding: t -*-

;; Author: 2011-2013 immerrr <immerrr+lua@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;;
;; URL:         https://immerrr.github.io/lua-mode
;; Version:     20221027
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Keywords: languages, processes, tools

;; This field is expanded to commit SHA and commit date during the
;; archive creation.
;; Revision: $Format:%h (%cD)$
;;

;;; Commentary:

;; lua-mode provides support for editing Lua, including automatic
;; indentation, syntactical font-locking, running interactive shell,
;; Flymake checks with luacheck, interacting with `hs-minor-mode' and
;; online documentation lookup.

;; The following variables are available for customization (see more via
;; `M-x customize-group lua`):

;; - Var `lua-indent-level':
;;   indentation offset in spaces
;; - Var `lua-indent-string-contents':
;;   set to `t` if you like to have contents of multiline strings to be
;;   indented like comments
;; - Var `lua-indent-nested-block-content-align':
;;   set to `nil' to stop aligning the content of nested blocks with the
;;   open parenthesis
;; - Var `lua-indent-close-paren-align':
;;   set to `t' to align close parenthesis with the open parenthesis,
;;   rather than with the beginning of the line
;; - Var `lua-mode-hook':
;;   list of functions to execute when lua-mode is initialized
;; - Var `lua-documentation-url':
;;   base URL for documentation lookup
;; - Var `lua-documentation-function': function used to
;;   show documentation (`eww` is a viable alternative for Emacs 25)

;; These are variables/commands that operate on the Lua process:

;; - Var `lua-default-application':
;;   command to start the Lua process (REPL)
;; - Var `lua-default-command-switches':
;;   arguments to pass to the Lua process on startup (make sure `-i` is there
;;   if you expect working with Lua shell interactively)
;; - Cmd `lua-start-process': start new REPL process, usually happens automatically
;; - Cmd `lua-kill-process': kill current REPL process

;; These are variables/commands for interaction with the Lua process:

;; - Cmd `lua-show-process-buffer': switch to REPL buffer
;; - Cmd `lua-hide-process-buffer': hide window showing REPL buffer
;; - Var `lua-always-show': show REPL buffer after sending something
;; - Cmd `lua-send-buffer': send whole buffer
;; - Cmd `lua-send-current-line': send current line
;; - Cmd `lua-send-defun': send current top-level function
;; - Cmd `lua-send-region': send active region
;; - Cmd `lua-restart-with-whole-file': restart REPL and send whole buffer

;; To enable on-the-fly linting, make sure you have the luacheck
;; program installed (available from luarocks) and activate
;; `flymake-mode'.

;; See "M-x apropos-command ^lua-" for a list of commands.
;; See "M-x customize-group lua" for a list of customizable variables.


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'comint)
(require 'newcomment)
(require 'rx)


;; rx-wrappers for Lua

(eval-when-compile
  ;; Silence compilation warning about `compilation-error-regexp-alist' defined
  ;; in compile.el.
  (require 'compile))

(eval-and-compile
  (if (fboundp #'rx-let)
      (progn
        ;; Emacs 27+ way of customizing rx
        (defvar lua--rx-bindings)

        (setq
         lua--rx-bindings
         '((symbol (&rest x) (seq symbol-start (or x) symbol-end))
           (ws (* (any " \t")))
           (ws+ (+ (any " \t")))

           (lua-name (symbol (seq (+ (any alpha "_")) (* (any alnum "_")))))
           (lua-funcname (seq lua-name (* ws "." ws lua-name)
                              (opt ws ":" ws lua-name)))
           (lua-funcheader
            ;; Outer (seq ...) is here to shy-group the definition
            (seq (or (seq (symbol "function") ws (group-n 1 lua-funcname))
                     (seq (group-n 1 lua-funcname) ws "=" ws
                          (symbol "function")))))
           (lua-number
            (seq (or (seq (+ digit) (opt ".") (* digit))
                     (seq (* digit) (opt ".") (+ digit)))
                 (opt (regexp "[eE][+-]?[0-9]+"))))
           (lua-assignment-op (seq "=" (or buffer-end (not (any "=")))))
           (lua-operator (or "+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">=" "<"
                             ">" "=" ";" ":" "," "." ".." "..."))
           (lua-keyword-operator (symbol "and" "not" "or"))
           (lua-keyword
            (symbol "break" "do" "else" "elseif" "end"  "for" "function"
                    "goto" "if" "in" "local" "repeat" "return"
                    "then" "until" "while"))
           (lua-up-to-9-variables
            (seq (group-n 1 lua-name) ws
                 (? "," ws (group-n 2 lua-name) ws
                    (? "," ws (group-n 3 lua-name) ws
                       (? "," ws (group-n 4 lua-name) ws
                          (? "," ws (group-n 5 lua-name) ws
                             (? "," ws (group-n 6 lua-name) ws
                                (? "," ws (group-n 7 lua-name) ws
                                   (? "," ws (group-n 8 lua-name) ws
                                      (? "," ws (group-n 9 lua-name) ws))))))))))))

        (defmacro lua-rx (&rest regexps)
          (eval `(rx-let ,lua--rx-bindings
                   (rx ,@regexps))))

        (defun lua-rx-to-string (form &optional no-group)
          (rx-let-eval lua--rx-bindings
            (rx-to-string form no-group))))
    (progn
      ;; Pre-Emacs 27 way of customizing rx
      (defvar lua-rx-constituents)
      (defvar rx-parent)

      (defun lua-rx-to-string (form &optional no-group)
        "Lua-specific replacement for `rx-to-string'.

See `rx-to-string' documentation for more information FORM and
NO-GROUP arguments."
        (let ((rx-constituents lua-rx-constituents))
          (rx-to-string form no-group)))

      (defmacro lua-rx (&rest regexps)
        "Lua-specific replacement for `rx'.

See `rx' documentation for more information about REGEXPS param."
        (cond ((null regexps)
               (error "No regexp"))
              ((cdr regexps)
               (lua-rx-to-string `(and ,@regexps) t))
              (t
               (lua-rx-to-string (car regexps) t))))

      (defun lua--new-rx-form (form)
        "Add FORM definition to `lua-rx' macro.

FORM is a cons (NAME . DEFN), see more in `rx-constituents' doc.
This function enables specifying new definitions using old ones:
if DEFN is a list that starts with `:rx' symbol its second
element is itself expanded with `lua-rx-to-string'. "
        (let ((form-definition (cdr form)))
          (when (and (listp form-definition) (eq ':rx (car form-definition)))
            (setcdr form (lua-rx-to-string (cadr form-definition) 'nogroup)))
          (push form lua-rx-constituents)))

      (defun lua--rx-symbol (form)
        ;; form is a list (symbol XXX ...)
        ;; Skip initial 'symbol
        (setq form (cdr form))
        ;; If there's only one element, take it from the list, otherwise wrap the
        ;; whole list into `(or XXX ...)' form.
        (setq form (if (eq 1 (length form))
                       (car form)
                     (append '(or) form)))
        (and (fboundp 'rx-form) ; Silence Emacs 27's byte-compiler.
             (rx-form `(seq symbol-start ,form symbol-end) rx-parent)))

      (setq lua-rx-constituents (copy-sequence rx-constituents))

      (mapc 'lua--new-rx-form
            `((symbol lua--rx-symbol 1 nil)
              (ws . "[ \t]*") (ws+ . "[ \t]+")
              (lua-name :rx (symbol (regexp "[[:alpha:]_]+[[:alnum:]_]*")))
              (lua-funcname
               :rx (seq lua-name (* ws "." ws lua-name)
                        (opt ws ":" ws lua-name)))
              (lua-funcheader
               ;; Outer (seq ...) is here to shy-group the definition
               :rx (seq (or (seq (symbol "function") ws (group-n 1 lua-funcname))
                            (seq (group-n 1 lua-funcname) ws "=" ws
                                 (symbol "function")))))
              (lua-number
               :rx (seq (or (seq (+ digit) (opt ".") (* digit))
                            (seq (* digit) (opt ".") (+ digit)))
                        (opt (regexp "[eE][+-]?[0-9]+"))))
              (lua-assignment-op
               :rx (seq "=" (or buffer-end (not (any "=")))))
              (lua-operator
               :rx (or "+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">=" "<"
                       ">" "=" ";" ":" "," "." ".." "..."))
              (lua-keyword-operator
               :rx (symbol "and" "not" "or"))
              (lua-keyword
               :rx (symbol "break" "do" "else" "elseif" "end"  "for" "function"
                           "goto" "if" "in" "local" "repeat" "return"
                           "then" "until" "while"))
              (lua-up-to-9-variables
               :rx (seq (group-n 1 lua-name) ws
                        (? "," ws (group-n 2 lua-name) ws
                           (? "," ws (group-n 3 lua-name) ws
                              (? "," ws (group-n 4 lua-name) ws
                                 (? "," ws (group-n 5 lua-name) ws
                                    (? "," ws (group-n 6 lua-name) ws
                                       (? "," ws (group-n 7 lua-name) ws
                                          (? "," ws (group-n 8 lua-name) ws
                                             (? "," ws (group-n 9 lua-name) ws)))))))))))))))


;; Local variables
(defgroup lua nil
  "Major mode for editing Lua code."
  :prefix "lua-"
  :group 'languages)

(defcustom lua-indent-level 3
  "Amount by which Lua subexpressions are indented."
  :type 'integer
  :group 'lua
  :safe #'integerp)

(defcustom lua-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'lua)

(defcustom lua-comment-start-skip "---*[ \t]*"
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'lua)

(defcustom lua-default-application "lua"
  "Default application to run in Lua process.

Can be a string, where it denotes a command to be executed to
start Lua process, or a (HOST . PORT) cons, that can be used to
connect to Lua process running remotely."
  :type '(choice (string)
                 (cons string integer))
  :group 'lua)

(defcustom lua-default-command-switches (list "-i")
  "Command switches for `lua-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'lua)
(make-variable-buffer-local 'lua-default-command-switches)

(defcustom lua-always-show t
  "*Non-nil means display lua-process-buffer after sending a command."
  :type 'boolean
  :group 'lua)

(defcustom lua-documentation-function 'browse-url
  "Function used to fetch the Lua reference manual."
  :type `(radio (function-item browse-url)
                ,@(when (fboundp 'eww) '((function-item eww)))
                ,@(when (fboundp 'w3m-browse-url) '((function-item w3m-browse-url)))
                (function :tag "Other function"))
  :group 'lua)

(defcustom lua-documentation-url
  (or (and (file-readable-p "/usr/share/doc/lua/manual.html")
           "file:///usr/share/doc/lua/manual.html")
      "http://www.lua.org/manual/5.1/manual.html")
  "URL pointing to the Lua reference manual."
  :type 'string
  :group 'lua)


(defvar lua-process nil
  "The active Lua process")

(defvar lua-process-buffer nil
  "Buffer used for communication with the Lua process.")

(defun lua--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  (cl-assert (eq prefix-key-sym 'lua-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'lua-prefix-key-update-bindings)
      (lua-prefix-key-update-bindings)))

(defcustom lua-prefix-key "\C-c"
  "Prefix for all lua-mode commands."
  :type 'string
  :group 'lua
  :set 'lua--customize-set-prefix-key
  :get (lambda (sym)
         (let ((val (eval sym)))
           (if val (single-key-description (eval sym)) ""))))

(defvar lua-mode-menu (make-sparse-keymap "Lua")
  "Keymap for lua-mode's menu.")

(defvar lua-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . lua-send-buffer)
              ("C-f" . lua-search-documentation)))
      result-map))
  "Keymap that is used to define keys accessible by `lua-prefix-key'.

If the latter is nil, the keymap translates into `lua-mode-map' verbatim.")

(defvar lua--electric-indent-chars
  (mapcar #'string-to-char '("}" "]" ")")))


(defvar lua-mode-map
  (let ((result-map (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)
      (mapc (lambda (electric-char)
              (define-key result-map
                (read-kbd-macro
                 (char-to-string electric-char))
                #'lua-electric-match))
            lua--electric-indent-chars))
    (define-key result-map [menu-bar lua-mode] (cons "Lua" lua-mode-menu))
    (define-key result-map [remap backward-up-list] 'lua-backward-up-list)

    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if lua-prefix-key
        (define-key result-map (vector lua-prefix-key) lua-prefix-mode-map)
      (set-keymap-parent result-map lua-prefix-mode-map))
    result-map)
  "Keymap used in lua-mode buffers.")

(defvar lua-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'lua-electric-flag)

(defcustom lua-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Lua program's prompt."
  :type  'regexp
  :group 'lua)

(defcustom lua-traceback-line-re
  ;; This regexp skips prompt and meaningless "stdin:N:" prefix when looking
  ;; for actual file-line locations.
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'lua)

(defvar lua--repl-buffer-p nil
  "Buffer-local flag saying if this is a Lua REPL buffer.")
(make-variable-buffer-local 'lua--repl-buffer-p)

(defun lua--compilation-find-file (fn marker filename directory &rest formats)
  "Return Lua REPL buffer when looking for \"stdin\" file in it."
  (if (and
       lua--repl-buffer-p
       (string-equal filename "stdin")
       ;; NOTE: this doesn't traverse `compilation-search-path' when
       ;; looking for filename.
       (not (file-exists-p (expand-file-name
                            filename
                            (when directory (expand-file-name directory))))))
      (current-buffer)
    (apply fn marker filename directory formats)))

(advice-add 'compilation-find-file :around #'lua--compilation-find-file)

(defun lua--compilation-goto-locus (fn msg mk end-mk)
  "When message points to Lua REPL buffer, go to the message itself.
Usually, stdin:XX line number points to nowhere."
  (let ((errmsg-buf (marker-buffer msg))
        (error-buf (marker-buffer mk)))
    (if (and (with-current-buffer errmsg-buf lua--repl-buffer-p)
             (eq error-buf errmsg-buf))
        (progn
          (compilation-set-window (display-buffer (marker-buffer msg)) msg)
          (goto-char msg))
      (funcall fn msg mk end-mk))))

(advice-add 'compilation-goto-locus :around #'lua--compilation-goto-locus)

(defcustom lua-indent-string-contents nil
  "If non-nil, contents of multiline string will be indented.
Otherwise leading amount of whitespace on each line is preserved."
  :group 'lua
  :type 'boolean
  :safe #'booleanp)

(defcustom lua-indent-nested-block-content-align t
  "If non-nil, the contents of nested blocks are indented to
align with the column of the opening parenthesis, rather than
just forward by `lua-indent-level'."
  :group 'lua
  :type 'boolean
  :safe #'booleanp)

(defcustom lua-indent-close-paren-align t
  "If non-nil, close parenthesis are aligned with their open
parenthesis.  If nil, close parenthesis are aligned to the
beginning of the line."
  :group 'lua
  :type 'boolean
  :safe #'booleanp)

(defcustom lua-jump-on-traceback t
  "*Jump to innermost traceback location in *lua* buffer.  When this
variable is non-nil and a traceback occurs when running Lua code in a
process, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'lua)

(defcustom lua-mode-hook nil
  "Hooks called when Lua mode fires up."
  :type 'hook
  :group 'lua)

(defvar lua-region-start (make-marker)
  "Start of special region for Lua communication.")

(defvar lua-region-end (make-marker)
  "End of special region for Lua communication.")

(defvar lua-emacs-menu
  '(["Restart With Whole File" lua-restart-with-whole-file t]
    ["Kill Process" lua-kill-process t]
    ["Hide Process Buffer" lua-hide-process-buffer t]
    ["Show Process Buffer" lua-show-process-buffer t]
    ["Beginning Of Proc" lua-beginning-of-proc t]
    ["End Of Proc" lua-end-of-proc t]
    ["Set Lua-Region Start" lua-set-lua-region-start t]
    ["Set Lua-Region End" lua-set-lua-region-end t]
    ["Send Lua-Region" lua-send-lua-region t]
    ["Send Current Line" lua-send-current-line t]
    ["Send Region" lua-send-region t]
    ["Send Proc" lua-send-proc t]
    ["Send Buffer" lua-send-buffer t]
    ["Search Documentation" lua-search-documentation t])
  "Emacs menu for Lua mode.")

;; the whole defconst is inside eval-when-compile, because it's later referenced
;; inside another eval-and-compile block
(eval-and-compile
  (defconst
    lua--builtins
    (let*
        ((modules
          '("_G" "_VERSION" "assert" "collectgarbage" "dofile" "error" "getfenv"
            "getmetatable" "ipairs" "load" "loadfile" "loadstring" "module"
            "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen" "rawset"
            "require" "select" "setfenv" "setmetatable" "tonumber" "tostring"
            "type" "unpack" "xpcall" "self"
            ("bit32" . ("arshift" "band" "bnot" "bor" "btest" "bxor" "extract"
                        "lrotate" "lshift" "replace" "rrotate" "rshift"))
            ("coroutine" . ("create" "isyieldable" "resume" "running" "status"
                            "wrap" "yield"))
            ("debug" . ("debug" "getfenv" "gethook" "getinfo" "getlocal"
                        "getmetatable" "getregistry" "getupvalue" "getuservalue"
                        "setfenv" "sethook" "setlocal" "setmetatable"
                        "setupvalue" "setuservalue" "traceback" "upvalueid"
                        "upvaluejoin"))
            ("io" . ("close" "flush" "input" "lines" "open" "output" "popen"
                     "read" "stderr" "stdin" "stdout" "tmpfile" "type" "write"))
            ("math" . ("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cosh"
                       "deg" "exp" "floor" "fmod" "frexp" "huge" "ldexp" "log"
                       "log10" "max" "maxinteger" "min" "mininteger" "modf" "pi"
                       "pow" "rad" "random" "randomseed" "sin" "sinh" "sqrt"
                       "tan" "tanh" "tointeger" "type" "ult"))
            ("os" . ("clock" "date" "difftime" "execute" "exit" "getenv"
                     "remove"  "rename" "setlocale" "time" "tmpname"))
            ("package" . ("config" "cpath" "loaded" "loaders" "loadlib" "path"
                          "preload" "searchers" "searchpath" "seeall"))
            ("string" . ("byte" "char" "dump" "find" "format" "gmatch" "gsub"
                         "len" "lower" "match" "pack" "packsize" "rep" "reverse"
                         "sub" "unpack" "upper"))
            ("table" . ("concat" "insert" "maxn" "move" "pack" "remove" "sort"
                        "unpack"))
            ("utf8" . ("char" "charpattern" "codepoint" "codes" "len"
                       "offset")))))

      (cl-labels
          ((module-name-re (x)
                           (concat "\\(?1:\\_<"
                                   (if (listp x) (car x) x)
                                   "\\_>\\)"))
           (module-members-re (x) (if (listp x)
                                      (concat "\\(?:[ \t]*\\.[ \t]*"
                                              "\\_<\\(?2:"
                                              (regexp-opt (cdr x))
                                              "\\)\\_>\\)?")
                                    "")))

        (concat
         ;; common prefix:
         ;; - beginning-of-line
         ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
         ;; - or concatenation operator ".."
         "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
         ;; optional whitespace
         "[ \t]*"
         "\\(?:"
         ;; any of modules/functions
         (mapconcat (lambda (x) (concat (module-name-re x)
                                        (module-members-re x)))
                    modules
                    "\\|")
         "\\)"))))

  "A regexp that matches Lua builtin functions & variables.

This is a compilation of 5.1, 5.2 and 5.3 builtins taken from the
index of respective Lua reference manuals.")


(defvar lua-font-lock-keywords
  `(;; highlight the hash-bang line "#!/foo/bar/lua" as comment
    ("^#!.*$" . font-lock-comment-face)

    ;; Builtin constants
    (,(lua-rx (symbol "true" "false" "nil"))
     . font-lock-constant-face)

    ;; Keywords
    (,(lua-rx (or lua-keyword lua-keyword-operator))
     . font-lock-keyword-face)

    ;; Labels used by the "goto" statement
    ;; Highlights the following syntax:  ::label::
    (,(lua-rx "::" ws lua-name ws "::")
     . font-lock-constant-face)

    ;; Highlights the name of the label in the "goto" statement like
    ;; "goto label"
    (,(lua-rx (symbol (seq "goto" ws+ (group-n 1 lua-name))))
     (1 font-lock-constant-face))

    ;; Highlight Lua builtin functions and variables
    (,lua--builtins
     (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))

    (,(lua-rx (symbol "for") ws+ lua-up-to-9-variables)
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face nil noerror)
     (3 font-lock-variable-name-face nil noerror)
     (4 font-lock-variable-name-face nil noerror)
     (5 font-lock-variable-name-face nil noerror)
     (6 font-lock-variable-name-face nil noerror)
     (7 font-lock-variable-name-face nil noerror)
     (8 font-lock-variable-name-face nil noerror)
     (9 font-lock-variable-name-face nil noerror))

    (,(lua-rx (symbol "function") (? ws+ lua-funcname) ws "(" ws lua-up-to-9-variables)
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face nil noerror)
     (3 font-lock-variable-name-face nil noerror)
     (4 font-lock-variable-name-face nil noerror)
     (5 font-lock-variable-name-face nil noerror)
     (6 font-lock-variable-name-face nil noerror)
     (7 font-lock-variable-name-face nil noerror)
     (8 font-lock-variable-name-face nil noerror)
     (9 font-lock-variable-name-face nil noerror))

    (,(lua-rx lua-funcheader)
     (1 font-lock-function-name-face))

    ;; local x, y, z
    ;; local x = .....
    ;;
    ;; NOTE: this is intentionally below funcheader matcher, so that in
    ;;
    ;; local foo = function() ...
    ;;
    ;; "foo" is fontified as function-name-face, and variable-name-face is not applied.
    (,(lua-rx (symbol "local") ws+ lua-up-to-9-variables)
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face nil noerror)
     (3 font-lock-variable-name-face nil noerror)
     (4 font-lock-variable-name-face nil noerror)
     (5 font-lock-variable-name-face nil noerror)
     (6 font-lock-variable-name-face nil noerror)
     (7 font-lock-variable-name-face nil noerror)
     (8 font-lock-variable-name-face nil noerror)
     (9 font-lock-variable-name-face nil noerror))

    (,(lua-rx (or (group-n 1
                           "@" (symbol "author" "copyright" "field" "release"
                                       "return" "see" "usage" "description"))
                  (seq (group-n 1 "@" (symbol "param" "class" "name")) ws+
                       (group-n 2 lua-name))))
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t noerror)))

  "Default expressions to highlight in Lua mode.")

(defvar lua-imenu-generic-expression
  `(("Requires" ,(lua-rx (or bol ";") ws (opt (seq (symbol "local") ws)) (group-n 1 lua-name) ws "=" ws (symbol "require")) 1)
    (nil ,(lua-rx (or bol ";") ws (opt (seq (symbol "local") ws)) lua-funcheader) 1))
  "Imenu generic expression for lua-mode.  See `imenu-generic-expression'.")

(defvar lua-sexp-alist '(("then" . "end")
                         ("function" . "end")
                         ("do" . "end")
                         ("repeat" . "until")))

(defvar lua-mode-abbrev-table nil
  "Abbreviation table used in lua-mode buffers.")

(define-abbrev-table 'lua-mode-abbrev-table
  '(("end"    "end"    lua-indent-line :system t)
    ("else"   "else"   lua-indent-line :system t)
    ("elseif" "elseif" lua-indent-line :system t)))

(defvar lua-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; main comment syntax: begins with "--", ends with "\n"
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?\n ">")

    ;; main string syntax: bounded by ' or "
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")

    ;; single-character binary operators: punctuation
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")

    (syntax-table))
  "`lua-mode' syntax table.")

;;;###autoload
(define-derived-mode lua-mode prog-mode "Lua"
  "Major mode for editing Lua code."
  :abbrev-table lua-mode-abbrev-table
  :syntax-table lua-mode-syntax-table
  :group 'lua
  (setq-local font-lock-defaults '(lua-font-lock-keywords ;; keywords
                                   nil                    ;; keywords-only
                                   nil                    ;; case-fold
                                   nil                    ;; syntax-alist
                                   nil                    ;; syntax-begin
                                   ))

  (setq-local syntax-propertize-function
              'lua--propertize-multiline-bounds)

  (setq-local parse-sexp-lookup-properties   t)
  (setq-local indent-line-function           'lua-indent-line)
  (setq-local beginning-of-defun-function    'lua-beginning-of-proc)
  (setq-local end-of-defun-function          'lua-end-of-proc)
  (setq-local comment-start                  lua-comment-start)
  (setq-local comment-start-skip             lua-comment-start-skip)
  (setq-local comment-use-syntax             t)
  (setq-local fill-paragraph-function        #'lua--fill-paragraph)
  (with-no-warnings
    (setq-local comment-use-global-state     t))
  (setq-local imenu-generic-expression       lua-imenu-generic-expression)
  (when (boundp 'electric-indent-chars)
    ;; If electric-indent-chars is not defined, electric indentation is done
    ;; via `lua-mode-map'.
    (setq-local electric-indent-chars
                (append electric-indent-chars lua--electric-indent-chars)))
  (add-hook 'flymake-diagnostic-functions #'lua-flymake nil t)

  ;; setup menu bar entry (XEmacs style)
  (if (and (featurep 'menubar)
           (boundp 'current-menubar)
           (fboundp 'set-buffer-menubar)
           (fboundp 'add-menu)
           (not (assoc "Lua" current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-menu nil "Lua" lua-emacs-menu)))
  ;; Append Lua menu to popup menu for Emacs.
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
            (cons (concat mode-name " Mode Commands") lua-emacs-menu)))

  ;; hideshow setup
  (unless (assq 'lua-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 `(lua-mode
                   ,(regexp-opt (mapcar 'car lua-sexp-alist) 'words) ;start
                   ,(regexp-opt (mapcar 'cdr lua-sexp-alist) 'words) ;end
                   nil lua-forward-sexp))))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defun lua-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (let (blink-paren-function)
    (self-insert-command (prefix-numeric-value arg)))
  (if lua-electric-flag
      (lua-indent-line))
  (blink-matching-open))

;; private functions

(defun lua--fill-paragraph (&optional justify region)
  ;; Implementation of forward-paragraph for filling.
  ;;
  ;; This function works around a corner case in the following situations:
  ;;
  ;;     <>
  ;;     -- some very long comment ....
  ;;     some_code_right_after_the_comment
  ;;
  ;; If point is at the beginning of the comment line, fill paragraph code
  ;; would have gone for comment-based filling and done the right thing, but it
  ;; does not find a comment at the beginning of the empty line before the
  ;; comment and falls back to text-based filling ignoring comment-start and
  ;; spilling the comment into the code.
  (save-excursion
    (while (and (not (eobp))
                (progn (move-to-left-margin)
                       (looking-at paragraph-separate)))
      (forward-line 1))
    (let ((fill-paragraph-handle-comment t))
      (fill-paragraph justify region))))


(defun lua-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq lua-prefix-mode-map (keymap-parent lua-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent lua-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc lua-prefix-mode-map lua-mode-map))
          (delq old-cons lua-mode-map)))

    (if (null lua-prefix-key)
        (set-keymap-parent lua-mode-map lua-prefix-mode-map)
      (define-key lua-mode-map (vector lua-prefix-key) lua-prefix-mode-map))))

(defun lua-set-prefix-key (new-key-str)
  "Changes `lua-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (lua--customize-set-prefix-key 'lua-prefix-key new-key-str)
  (message "Prefix key set to %S"  (single-key-description lua-prefix-key))
  (lua-prefix-key-update-bindings))

(defun lua-string-p (&optional pos)
  "Returns true if the point is in a string."
  (save-excursion (elt (syntax-ppss pos) 3)))

(defun lua--containing-double-hyphen-start-pos ()
  "Return position of the beginning comment delimiter (--).

Emacs syntax framework does not consider comment delimiters as
part of the comment itself, but for this package it is useful to
consider point as inside comment when it is between the two hyphens"
  (and (eql (char-before) ?-)
       (eql (char-after) ?-)
       (1- (point))))

(defun lua-comment-start-pos (&optional parsing-state)
  "Return position of comment containing current point.

If point is not inside a comment, return nil."
  (unless parsing-state (setq parsing-state (syntax-ppss)))
  (and
   ;; Not a string
   (not (nth 3 parsing-state))
   ;; Syntax-based comment
   (or (and (nth 4 parsing-state) (nth 8 parsing-state))
       (lua--containing-double-hyphen-start-pos))))

(defun lua-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (lua-comment-start-pos parse-result)))))

(defun lua-comment-or-string-start-pos (&optional pos)
  "Returns start position of string or comment which contains point.

If point is not inside string or comment, return nil."
  (save-excursion
    (when pos (goto-char pos))
    (or (elt (syntax-ppss pos) 8)
        (lua--containing-double-hyphen-start-pos))))

;; They're propertized as follows:
;; 1. generic-comment
;; 2. generic-string
;; 3. equals signs
(defconst lua-ml-begin-regexp
  "\\(?:\\(?1:-\\)-\\[\\|\\(?2:\\[\\)\\)\\(?3:=*\\)\\[")


(defun lua-try-match-multiline-end (end)
  "Try to match close-bracket for multiline literal around point.

Basically, detect form of close bracket from syntactic
information provided at point and re-search-forward to it."
  (let ((comment-or-string-start-pos (lua-comment-or-string-start-pos)))
    ;; Is there a literal around point?
    (and comment-or-string-start-pos
         ;; It is, check if the literal is a multiline open-bracket
         (save-excursion
           (goto-char comment-or-string-start-pos)
           (looking-at lua-ml-begin-regexp))

         ;; Yes it is, look for it matching close-bracket.  Close-bracket's
         ;; match group is determined by match-group of open-bracket.
         (re-search-forward
          (format "]%s\\(?%s:]\\)"
                  (match-string-no-properties 3)
                  (if (match-beginning 1) 1 2))
          end 'noerror))))


(defun lua-try-match-multiline-begin (limit)
  "Try to match multiline open-brackets.

Find next opening long bracket outside of any string/comment.
If none can be found before reaching LIMIT, return nil."

  (let (last-search-matched)
    (while
        ;; This loop will iterate skipping all multiline-begin tokens that are
        ;; inside strings or comments ending either at EOL or at valid token.
        (and (setq last-search-matched
                   (re-search-forward lua-ml-begin-regexp limit 'noerror))
             ;; Ensure --[[ is not inside a comment or string.
             ;;
             ;; This includes "---[[" sequence, in which "--" at the beginning
             ;; creates a single-line comment, and thus "-[[" is no longer a
             ;; multi-line opener.
             ;;
             ;; XXX: need to ensure syntax-ppss beyond (match-beginning 0) is
             ;; not calculated, or otherwise we'll need to flush the cache.
             (lua-comment-or-string-start-pos (match-beginning 0))))

    last-search-matched))

(defun lua-match-multiline-literal-bounds (limit)
  ;; First, close any multiline literal spanning from previous block. This will
  ;; move the point accordingly so as to avoid double traversal.
  (or (lua-try-match-multiline-end limit)
      (lua-try-match-multiline-begin limit)))

(defun lua--propertize-multiline-bounds (start end)
  "Put text properties on beginnings and ends of multiline literals.

Intended to be used as a `syntax-propertize-function'."
  (save-excursion
    (goto-char start)
    (while (lua-match-multiline-literal-bounds end)
      (when (match-beginning 1)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "!")))
      (when (match-beginning 2)
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "|"))))))


(defun lua-indent-line ()
  "Indent current line for Lua mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (lua-comment-or-string-p)
        (setq indent (lua-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (lua-calculate-indentation))))

    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    indent))

(defun lua-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (lua-string-p) (not lua-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)

    ;; At this point, we know that we're inside comment, so make sure
    ;; close-bracket is unindented like a block that starts after
    ;; left-shifter.
    (let ((left-shifter-p (looking-at "\\s *\\(?:--\\)?\\]\\(?1:=*\\)\\]")))
      (save-excursion
        (goto-char (lua-comment-or-string-start-pos))
        (+ (current-indentation)
           (if (and left-shifter-p
                    (looking-at (format "--\\[%s\\["
                                        (match-string-no-properties 1))))
               0
             lua-indent-level))))))


(defun lua--signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  ;; XXX: backport from cl-extras for Emacs24
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

(defun lua--ensure-point-within-limit (limit backward)
  "Return non-nil if point is within LIMIT going forward.

With BACKWARD non-nil, return non-nil if point is within LIMIT
going backward.

If point is beyond limit, move it onto limit."
  (if (= (lua--signum (- (point) limit))
         (if backward 1 -1))
      t
    (goto-char limit)
    nil))


(defun lua--escape-from-string (&optional backward)
  "Move point outside of string if it is inside one.

By default, point is placed after the string, with BACKWARD it is
placed before the string."
  (interactive)
  (let ((parse-state (syntax-ppss)))
    (when (nth 3 parse-state)
      (if backward
          (goto-char (nth 8 parse-state))
        (parse-partial-sexp (point) (line-end-position) nil nil (syntax-ppss) 'syntax-table))
      t)))


(defun lua-find-regexp (direction regexp &optional limit)
  "Searches for a regular expression in the direction specified.

Direction is one of \\='forward and \\='backward.

Matches in comments and strings are ignored. If the regexp is
found, returns point position, nil otherwise."
  (let ((search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (cl-loop
     always (or (null limit)
                (lua--ensure-point-within-limit limit (not (eq direction 'forward))))
     always (funcall search-func regexp limit 'noerror)
     for match-beg = (match-beginning 0)
     for match-end = (match-end 0)
     while (or (lua-comment-or-string-p match-beg)
               (lua-comment-or-string-p match-end))
     do (let ((parse-state (syntax-ppss)))
          (cond
           ;; Inside a string
           ((nth 3 parse-state)
            (lua--escape-from-string (not (eq direction 'forward))))
           ;; Inside a comment
           ((nth 4 parse-state)
            (goto-char (nth 8 parse-state))
            (when (eq direction 'forward)
              (forward-comment 1)))))
     finally return (point))))


(defconst lua-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "repeat" "then"
                   "else" "elseif" "end" "until") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst lua-block-token-alist
  '(("do"       "\\_<end\\_>"   "\\_<for\\|while\\_>"                       middle-or-open)
    ("function" "\\_<end\\_>"   nil                                       open)
    ("repeat"   "\\_<until\\_>" nil                                       open)
    ("then"     "\\_<\\(e\\(lse\\(if\\)?\\|nd\\)\\)\\_>" "\\_<\\(else\\)?if\\_>" middle)
    ("{"        "}"           nil                                       open)
    ("["        "]"           nil                                       open)
    ("("        ")"           nil                                       open)
    ("if"       "\\_<then\\_>"  nil                                       open)
    ("for"      "\\_<do\\_>"    nil                                       open)
    ("while"    "\\_<do\\_>"    nil                                       open)
    ("else"     "\\_<end\\_>"   "\\_<then\\_>"                              middle)
    ("elseif"   "\\_<then\\_>"  "\\_<then\\_>"                              middle)
    ("end"      nil           "\\_<\\(do\\|function\\|then\\|else\\)\\_>" close)
    ("until"    nil           "\\_<repeat\\_>"                            close)
    ("}"        nil           "{"                                       close)
    ("]"        nil           "\\["                                     close)
    (")"        nil           "("                                       close))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possible tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possible tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defconst lua-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   (regexp-opt '("do" "function" "repeat" "then" "if" "else" "elseif" "for" "while") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun lua-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from lua-block-token-alist"
  (assoc token lua-block-token-alist))

(defun lua-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (nth 2 token-info))
   (t nil)))

(defun lua-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
  (nth 3 token-info))

(defun lua-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (lua-find-regexp 'backward lua-block-regexp))

(defun lua-find-matching-token-word (token &optional direction)
  "Find matching open- or close-token for TOKEN in DIRECTION.
Point has to be exactly at the beginning of TOKEN, e.g. with | being point

  {{ }|}  -- (lua-find-matching-token-word \"}\" \\='backward) will return
          -- the first {
  {{ |}}  -- (lua-find-matching-token-word \"}\" \\='backward) will find
          -- the second {.

DIRECTION has to be either \\='forward or \\='backward."
  (let* ((token-info (lua-get-block-token-info token))
         (match-type (lua-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (lua-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (lua-find-regexp search-direction lua-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (lua-get-token-type
                             (lua-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (goto-char found-pos)
                          (lua-find-matching-token-word found-token
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (lua-get-block-token-info token))
              (setq match (lua-get-token-match-re token-info search-direction))
              (setq match-type (lua-get-token-type token-info))))))
      maybe-found-pos)))

(defun lua-goto-matching-block-token (&optional parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point.  Returns the point position of the first character of
the matching token if successful, nil otherwise.

Optional PARSE-START is a position to which the point should be moved first.
DIRECTION has to be \\='forward or \\='backward (\\='forward by default)."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at lua-indentation-modifier-regexp)
        (let ((position (lua-find-matching-token-word (match-string 0)
                                                      direction)))
          (and position
               (goto-char position))))))

(defun lua-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa.

If optional NOREPORT is non-nil, it won't flag an error if there
is no block open/close open."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (when (and (eq (char-syntax (following-char)) ?w)
             (not (looking-at "\\_<")))
    (re-search-backward "\\_<" nil t))
  (let ((position (lua-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun lua-skip-ws-and-comments-backward (&optional limit)
  "Move point back skipping all whitespace and comments.

If LIMIT is given, stop at it or before.

Return non-nil if moved point."
  (interactive)
  (unless (lua-string-p)
    (let ((start-pos (point))
          (comment-start-pos (lua-comment-start-pos)))
      (setq limit (min (point) (or limit (point-min))))
      (when comment-start-pos
        (goto-char (max limit comment-start-pos)))
      (when (< limit (point)) (forward-comment (- limit (point))))
      (when (< (point) limit) (goto-char limit))
      (when (/= start-pos (point))
        (point)))))

(defun lua-skip-ws-and-comments-forward (&optional limit)
  "Move point forward skipping all whitespace and comments.

If LIMIT is given, stop at it or before.

Return non-nil if moved point."
  (interactive)
  (unless (lua-string-p)
    (let ((start-pos (point))
          (comment-start-pos (lua-comment-start-pos)))
      (setq limit (max (point) (or limit (point-max))))
      ;; Escape from current comment. It is necessary to use "while" because
      ;; luadoc parameters have non-comment face, and parse-partial-sexp with
      ;; 'syntax-table flag will stop on them.
      (when comment-start-pos
        (goto-char comment-start-pos)
        (forward-comment 1))
      (when (< (point) limit) (forward-comment (- limit (point))))
      (when (< limit (point)) (goto-char limit))
      (when (/= start-pos (point))
        (point)))))


(defun lua-forward-line-skip-blanks (&optional back)
  "Move 1 line forward/backward and skip all insignificant ws/comment lines.

Moves point 1 line forward (or backward) skipping lines that contain
no Lua code besides comments.  The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (let ((start-pos (point)))
    (if back
        (progn
          (beginning-of-line)
          (lua-skip-ws-and-comments-backward))
      (forward-line)
      (lua-skip-ws-and-comments-forward))
    (beginning-of-line)
    (when (> (count-lines start-pos (point)) 0)
      (point))))

(eval-when-compile
  (defconst lua-operator-class
    "-+*/^.=<>~:&|"))

(defconst lua-cont-eol-regexp
  (eval-when-compile
    (concat
     "\\(?:\\(?1:\\_<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
                   "local" "function" "if" "until" "elseif" "return")
                 t)
     "\\_>\\)\\|"
     "\\(?:^\\|[^" lua-operator-class "]\\)\\(?2:"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~" ",")
                 t)
     "\\)\\)"
     "\\s *\\="))
  "Regexp that matches the ending of a line that needs continuation.

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. `for' without `do' or `if' without `then') followed by
an optional whitespace till the end of the line.")

(defconst lua-cont-bol-regexp
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(?:\\(?1:\\_<"
     (regexp-opt '("and" "or" "not" "in") t)
     "\\_>\\)\\|\\(?2:"
     (regexp-opt '("," "+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\)\\(?:$\\|[^" lua-operator-class "]\\)"
     "\\)"))
  "Regexp that matches a line that continues previous one.

This regexp means, starting from point there is an optional whitespace followed
by Lua binary operator.  Lua is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement.")

(defun lua-last-token-continues-p ()
  "Return non-nil if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        return-value)
    (save-excursion
      (end-of-line)
      (lua-skip-ws-and-comments-backward line-begin)
      (setq return-value (and (re-search-backward lua-cont-eol-regexp line-begin t)
                              (or (match-beginning 1)
                                  (match-beginning 2))))
      (if (and return-value
               (string-equal (match-string-no-properties 0) "return"))
          ;; "return" keyword is ambiguous and depends on next token
          (unless (save-excursion
                    (goto-char (match-end 0))
                    (forward-comment (point-max))
                    (and
                     ;; Not continuing: at end of file
                     (not (eobp))
                     (or
                      ;; "function" keyword: it is a continuation, e.g.
                      ;;
                      ;;    return
                      ;;       function() return 123 end
                      ;;
                      (looking-at (lua-rx (symbol "function")))
                      ;; Looking at semicolon or any other keyword: not continuation
                      (not (looking-at (lua-rx (or ";" lua-keyword)))))))
            (setq return-value nil)))
      return-value)))


(defun lua-first-token-continues-p ()
  "Return non-nil if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (lua-skip-ws-and-comments-forward line-end)
      ;; if first character of the line is inside string, it's a continuation
      ;; if strings aren't supposed to be indented, `lua-calculate-indentation' won't even let
      ;; the control inside this function
      (and
       (re-search-forward lua-cont-bol-regexp line-end t)
       (or (match-beginning 1)
           (match-beginning 2))))))


(defun lua--backward-up-list-noerror ()
  "Safe version of lua-backward-up-list that does not signal an error."
  (condition-case nil
      (lua-backward-up-list)
    (scan-error nil)))


(defun lua-backward-up-list ()
  "Goto starter/opener of the block that contains point."
  (interactive)
  (let ((start-pos (point))
        end-pos)
    (or
     ;; Return parent block opener token if it exists.
     (cl-loop
      ;; Search indentation modifier backward, return nil on failure.
      always (lua-find-regexp 'backward lua-indentation-modifier-regexp)
      ;; Fetch info about the found token
      for token = (match-string-no-properties 0)
      for token-info = (lua-get-block-token-info token)
      for token-type = (lua-get-token-type token-info)
      ;; If the token is a close token, continue to skip its opener. If not
      ;; close, stop and return found token.
      while (eq token-type 'close)
      ;; Find matching opener to skip it and continue from beginning.
      ;;
      ;; Return nil on failure.
      always (let ((position (lua-find-matching-token-word token 'backward)))
               (and position (goto-char position)))
      finally return token-info)
     (progn
       (setq end-pos (point))
       (goto-char start-pos)
       (signal 'scan-error
               (list "Block open token not found"
                     ;; If start-pos == end-pos, the "obstacle" is current
                     (if (eql start-pos end-pos) start-pos (match-beginning 0))
                     (if (eql start-pos end-pos) start-pos (match-end 0))))))))

(defun lua--continuation-breaking-line-p ()
  "Return non-nil if looking at token(-s) that forbid continued line."
  (save-excursion
    (lua-skip-ws-and-comments-forward (line-end-position))
    (looking-at (lua-rx (or (symbol "do" "while" "repeat" "until"
                                    "if" "then" "elseif" "else"
                                    "for" "local")
                            lua-funcheader)))))


(defun lua-is-continuing-statement-p-1 ()
  "Return non-nil if current lined continues a statement.

More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

* the expression is not enclosed by a parentheses/braces/brackets"
  (let (prev-line continuation-pos parent-block-opener)
    (save-excursion (setq prev-line (lua-forward-line-skip-blanks 'back)))
    (and prev-line
         (not (lua--continuation-breaking-line-p))
         (save-excursion
           (or
            ;; Binary operator or keyword that implies continuation.
            (and (setq continuation-pos
                       (or (lua-first-token-continues-p)
                           (save-excursion (and (goto-char prev-line)
                                                ;; check last token of previous nonblank line
                                                (lua-last-token-continues-p)))))
                 (not
                  ;; Operators/keywords does not create continuation inside some blocks:
                  (and
                   (setq parent-block-opener (car-safe (lua--backward-up-list-noerror)))
                   (or
                    ;; - inside parens/brackets
                    (member parent-block-opener '("(" "["))
                    ;; - inside braces if it is a comma
                    (and (eq (char-after continuation-pos) ?,)
                         (equal parent-block-opener "{")))))
                 continuation-pos))))))


(defun lua-is-continuing-statement-p (&optional parse-start)
  "Returns non-nil if the line at PARSE-START should be indented as continuation line.

This true is when the line :

* is continuing a statement itself

* starts with a 1+ block-closer tokens, an top-most block opener is on a continuation line
"
  (save-excursion
    (if parse-start (goto-char parse-start))

    ;; If line starts with a series of closer tokens, whether or not the line
    ;; is a continuation line is decided by the opener line, e.g.
    ;;
    ;; x = foo +
    ;;    long_function_name(
    ;;       long_parameter_1,
    ;;       long_parameter_2,
    ;;       long_parameter_3,
    ;;    ) + long_function_name2({
    ;;       long_parameter_1,
    ;;       long_parameter_2,
    ;;       long_parameter_3,
    ;;    })
    ;;
    ;; Final line, "})" is a continuation line, but it is decided by the
    ;; opener line, ") + long_function_name2({", which in its turn is decided
    ;; by the "long_function_name(" line, which is a continuation line
    ;; because the line before it ends with a binary operator.
    (cl-loop
     ;; Go to opener line
     while (and (lua--goto-line-beginning-rightmost-closer)
                (lua--backward-up-list-noerror))
     ;; If opener line is continuing, repeat. If opener line is not
     ;; continuing, return nil.
     always (lua-is-continuing-statement-p-1)
     ;; We get here if there was no opener to go to: check current line.
     finally return (lua-is-continuing-statement-p-1))))

(defun lua-make-indentation-info-pair (found-token found-pos)
  "Create a pair from FOUND-TOKEN and FOUND-POS for indentation calculation.

This is a helper function to lua-calculate-indentation-info.
Don't use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) lua-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative lua-indent-level))

   ;; block openers
   ((and lua-indent-nested-block-content-align
         (member found-token (list "{" "(" "[")))
    (save-excursion
      (let ((found-bol (line-beginning-position)))
        (forward-comment (point-max))
        ;; If the next token is on this line and it's not a block opener,
        ;; the next line should align to that token.
        (if (and (zerop (count-lines found-bol (line-beginning-position)))
                 (not (looking-at lua-indentation-modifier-regexp)))
            (cons 'absolute (current-column))
          (cons 'relative lua-indent-level)))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; lua-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let* ((line-beginning (line-beginning-position))
             (same-line (and (lua-goto-matching-block-token found-pos 'backward)
                             (<= line-beginning (point)))))
        (if same-line
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
    (save-excursion
      (let* ((line-beginning (line-beginning-position))
             (same-line (and (lua-goto-matching-block-token found-pos 'backward)
                             (<= line-beginning (point)))))
        (if same-line
            (cons 'replace-matching (cons 'relative lua-indent-level))
          (cons 'relative lua-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let* ((line-beginning (line-beginning-position))
             (same-line (and (lua-goto-matching-block-token found-pos 'backward)
                             (<= line-beginning (point))))
             (opener-pos (point))
             opener-continuation-offset)
        (if same-line
            (cons 'remove-matching 0)
          (back-to-indentation)
          (setq opener-continuation-offset
                (if (lua-is-continuing-statement-p-1) lua-indent-level 0))

          ;; Accumulate indentation up to opener, including indentation. If
          ;; there were no other indentation modifiers until said opener,
          ;; ensure there is no continuation after the closer.
          `(multiple . ((absolute . ,(- (current-indentation) opener-continuation-offset))
                        ,@(when (/= opener-continuation-offset 0)
                            (list (cons 'continued-line opener-continuation-offset)))
                        ,@(delete nil (list (lua-calculate-indentation-info-1 nil opener-pos)))
                        (cancel-continued-line . nil)))))))

   ((member found-token '("do" "then"))
    `(multiple . ((cancel-continued-line . nil) (relative . ,lua-indent-level))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        lua-indent-level
                      ;; end of a block matched
                      (- lua-indent-level))))))

(defun  lua-add-indentation-info-pair (pair info-list)
  "Add the given indentation info PAIR to the list of indentation INFO-LIST.
This function has special case handling for two tokens: remove-matching,
and replace-matching.  These two tokens are cleanup tokens that remove or
alter the effect of a previously recorded indentation info.

When a remove-matching token is encountered, the last recorded info, i.e.
the car of the list is removed.  This is used to roll-back an indentation of a
block opening statement when it is closed.

When a replace-matching token is seen, the last recorded info is removed,
and the cdr of the replace-matching info is added in its place.  This is used
when a middle-of the block (the only case is `else') is seen on the same line
the block is opened."
  (cond
   ( (eq 'multiple (car pair))
     (let ((info-pair-elts (cdr pair)))
       (while info-pair-elts
         (setq info-list (lua-add-indentation-info-pair (car info-pair-elts) info-list)
               info-pair-elts (cdr info-pair-elts)))
       info-list))
   ( (eq 'cancel-continued-line (car pair))
     (if (eq (caar info-list) 'continued-line)
         (cdr info-list)
       info-list))
   ( (eq 'remove-matching (car pair))
     ;; Remove head of list
     (cdr info-list))
   ( (eq 'replace-matching (car pair))
     ;; remove head of list, and add the cdr of pair instead
     (cons (cdr pair) (cdr info-list)))
   ( (listp (cdr-safe pair))
     (nconc pair info-list))
   ( t
     ;; Just add the pair
     (cons pair info-list))))

(defun lua-calculate-indentation-info-1 (indentation-info bound)
  "Helper function for `lua-calculate-indentation-info'.

Return list of indentation modifiers from point to BOUND."
  (while (lua-find-regexp 'forward lua-indentation-modifier-regexp
                          bound)
    (let ((found-token (match-string 0))
          (found-pos (match-beginning 0)))
      (setq indentation-info
            (lua-add-indentation-info-pair
             (lua-make-indentation-info-pair found-token found-pos)
             indentation-info))))
  indentation-info)


(defun lua-calculate-indentation-info (&optional parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let (indentation-info cont-stmt-pos)
    (while (setq cont-stmt-pos (lua-is-continuing-statement-p))
      (lua-forward-line-skip-blanks 'back)
      (when (< cont-stmt-pos (point))
        (goto-char cont-stmt-pos)))

    ;; calculate indentation modifiers for the line itself
    (setq indentation-info (list (cons 'absolute (current-indentation))))

    (back-to-indentation)
    (setq indentation-info
          (lua-calculate-indentation-info-1
           indentation-info (min parse-end (line-end-position))))

    ;; and do the following for each continuation line before PARSE-END
    (while (and (eql (forward-line 1) 0)
                (<= (point) parse-end))

      ;; handle continuation lines:
      (if (lua-is-continuing-statement-p)
          ;; if it's the first continuation line, add one level
          (unless (eq (car (car indentation-info)) 'continued-line)
            (push (cons 'continued-line lua-indent-level) indentation-info))

        ;; if it's the first non-continued line, subtract one level
        (when (eq (car (car indentation-info)) 'continued-line)
          (push (cons 'stop-continued-line (- lua-indent-level)) indentation-info)))

      ;; add modifiers found in this continuation line
      (setq indentation-info
            (lua-calculate-indentation-info-1
             indentation-info (min parse-end (line-end-position)))))

    indentation-info))


(defun lua-accumulate-indentation-info (reversed-indentation-info)
  "Accumulates the indentation information previously calculated by
lua-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let (indentation-info
        (type 'relative)
        (accu 0))
    ;; Aggregate all neighbouring relative offsets, reversing the INFO list.
    (cl-dolist (elt reversed-indentation-info)
      (if (and (eq (car elt) 'relative)
               (eq (caar indentation-info) 'relative))
          (setcdr (car indentation-info) (+ (cdar indentation-info) (cdr elt)))
        (push elt indentation-info)))

    ;; Aggregate indentation info, taking 'absolute modifiers into account.
    (mapc (lambda (x)
            (let ((new-val (cdr x)))
              (if (eq 'absolute (car x))
                  (progn (setq type 'absolute
                               accu new-val))
                (setq accu (+ accu new-val)))))
          indentation-info)

    (cons type accu)))

(defun lua-calculate-indentation-block-modifier (&optional parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add lua-indent-level once each, and endings
of blocks subtract lua-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (let (indentation-info)
    (save-excursion
      ;; First go back to the line that starts it all
      ;; lua-calculate-indentation-info will scan through the whole thing
      (let ((case-fold-search nil))
        (setq indentation-info
              (lua-accumulate-indentation-info
               (lua-calculate-indentation-info parse-end)))))

    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))


(eval-when-compile
  (defconst lua--function-name-rx
    '(seq symbol-start
          (+ (any alnum "_"))
          (* "." (+ (any alnum "_")))
          (? ":" (+ (any alnum "_")))
          symbol-end)
    "Lua function name regexp in `rx'-SEXP format."))


(defconst lua--left-shifter-regexp
  (eval-when-compile
    (rx
     ;; This regexp should answer the following questions:
     ;; 1. is there a left shifter regexp on that line?
     ;; 2. where does block-open token of that left shifter reside?
     (or (seq (group-n 1 symbol-start "local" (+ blank)) "function" symbol-end)

         (seq (group-n 1 (eval lua--function-name-rx) (* blank)) (any "{("))
         (seq (group-n 1 (or
                          ;; assignment statement prefix
                          (seq (* nonl) (not (any "<=>~")) "=" (* blank))
                          ;; return statement prefix
                          (seq word-start "return" word-end (* blank))))
              ;; right hand side
              (or "{"
                  "function"
                  "("
                  (seq (group-n 1 (eval lua--function-name-rx) (* blank))
                       (any "({")))))))

  "Regular expression that matches left-shifter expression.

Left-shifter expression is defined as follows.  If a block
follows a left-shifter expression, its contents & block-close
token should be indented relative to left-shifter expression
indentation rather then to block-open token.

For example:
   -- `local a = ' is a left-shifter expression
   -- `function' is a block-open token
   local a = function()
      -- block contents is indented relative to left-shifter
      foobarbaz()
   -- block-end token is unindented to left-shifter indentation
   end

The following left-shifter expressions are currently handled:
1. local function definition with function block, begin-end
2. function call with arguments block, () or {}
3. assignment/return statement with
   - table constructor block, {}
   - function call arguments block, () or {} block
   - function expression a.k.a. lambda, begin-end block.")


(defun lua-point-is-after-left-shifter-p ()
  "Check if point is right after a left-shifter expression.

See `lua--left-shifter-regexp' for description & example of
left-shifter expression. "
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (and
       (/= (point) old-point)
       (looking-at lua--left-shifter-regexp)
       (= old-point (match-end 1))))))

(defun lua--goto-line-beginning-rightmost-closer (&optional parse-start)
  (let (case-fold-search pos line-end-pos return-val)
    (save-excursion
      (if parse-start (goto-char parse-start))
      (setq line-end-pos (line-end-position))
      (back-to-indentation)
      (unless (lua-comment-or-string-p)
        (cl-loop while (and (<= (point) line-end-pos)
                            (looking-at lua-indentation-modifier-regexp))
                 for token-info = (lua-get-block-token-info (match-string 0))
                 for token-type = (lua-get-token-type token-info)
                 while (not (eq token-type 'open))
                 do (progn
                      (setq pos (match-beginning 0)
                            return-val token-info)
                      (goto-char (match-end 0))
                      (forward-comment (line-end-position))))))
    (when pos
      (progn
        (goto-char pos)
        return-val))))


(defun lua-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.

If there's a sequence of block-close tokens starting at the
beginning of the line, calculate indentation according to the
line containing block-open token for the last block-close token
in the sequence.

If not, return nil."
  (let (case-fold-search rightmost-closer-info opener-info opener-pos)
    (save-excursion
      (when (and (setq rightmost-closer-info (lua--goto-line-beginning-rightmost-closer parse-start))
                 (setq opener-info (lua--backward-up-list-noerror))
                 ;; Ensure opener matches closer.
                 (string-match (lua-get-token-match-re rightmost-closer-info 'backward)
                               (car opener-info)))

        ;; Special case: "middle" tokens like for/do, while/do, if/then,
        ;; elseif/then: corresponding "end" or corresponding "else" must be
        ;; unindented to the beginning of the statement, which is not
        ;; necessarily the same as beginning of string that contains "do", e.g.
        ;;
        ;; while (
        ;;    foo and
        ;;    bar) do
        ;;    hello_world()
        ;; end
        (setq opener-pos (point))
        (when (/= (- opener-pos (line-beginning-position)) (current-indentation))
          (unless (or
                   (and (string-equal (car opener-info) "do")
                        (member (car (lua--backward-up-list-noerror)) '("while" "for")))
                   (and (string-equal (car opener-info) "then")
                        (member (car (lua--backward-up-list-noerror)) '("if" "elseif"))))
            (goto-char opener-pos)))

        ;; (let (cont-stmt-pos)
        ;;   (while (setq cont-stmt-pos (lua-is-continuing-statement-p))
        ;;     (goto-char cont-stmt-pos)))
        ;; Exception cases: when the start of the line is an assignment,
        ;; go to the start of the assignment instead of the matching item
        (if (and lua-indent-close-paren-align
                 (member (car opener-info) '("{" "(" "["))
                 (not (lua-point-is-after-left-shifter-p)))
            (current-column)
          (current-indentation))))))


(defun lua-calculate-indentation ()
  "Return appropriate indentation for current line as Lua code."
  (save-excursion
    (let ((cur-line-begin-pos (line-beginning-position)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (lua-calculate-indentation-override)

       (when (lua-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let* ((modifier
                 (lua-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))

       ;; 4. if there's no previous line, indentation is 0
       0))))

(defvar lua--beginning-of-defun-re
  (lua-rx-to-string '(: bol (? (symbol "local") ws+) lua-funcheader))
  "Lua top level (matches only at the beginning of line) function header regex.")


(defun lua-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a Lua proc (or similar).

With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.

Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg (setq arg 1))

  (while (and (> arg 0)
              (re-search-backward lua--beginning-of-defun-re nil t))
    (setq arg (1- arg)))

  (while (and (< arg 0)
              (re-search-forward lua--beginning-of-defun-re nil t))
    (beginning-of-line)
    (setq arg (1+ arg)))

  (zerop arg))

(defun lua-end-of-proc (&optional arg)
  "Move forward to next end of Lua proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defvar lua-process-init-code
  (mapconcat
   'identity
   '("local loadstring = loadstring or load"
     "function luamode_loadstring(str, displayname, lineoffset)"
     "  if lineoffset > 1 then"
     "    str = string.rep('\\n', lineoffset - 1) .. str"
     "  end"
     ""
     "  local x, e = loadstring(str, '@'..displayname)"
     "  if e then"
     "    error(e)"
     "  end"
     "  return x()"
     "end")
   " "))

(defun lua-make-lua-string (str)
  "Convert string to Lua literal."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "[\"'\\\t\\\n]" nil t)
        (cond
         ((string= (match-string 0) "\n")
          (replace-match "\\\\n"))
         ((string= (match-string 0) "\t")
          (replace-match "\\\\t"))
         (t
          (replace-match "\\\\\\&" t))))
      (concat "'" (buffer-string) "'"))))

;;;###autoload
(defalias 'run-lua #'lua-start-process)

;;;###autoload
(defun lua-start-process (&optional name program startfile &rest switches)
  "Start a Lua process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `lua-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (setq name (or name (if (consp lua-default-application)
                          (car lua-default-application)
                        lua-default-application)))
  (setq program (or program lua-default-application))
  ;; don't re-initialize if there already is a lua process
  (unless (comint-check-proc (format "*%s*" name))
    (setq lua-process-buffer (apply #'make-comint name program startfile
                                    (or switches lua-default-command-switches)))
    (setq lua-process (get-buffer-process lua-process-buffer))
    (set-process-query-on-exit-flag lua-process nil)
    (with-current-buffer lua-process-buffer
      ;; enable error highlighting in stack traces
      (require 'compile)
      (setq lua--repl-buffer-p t)
      (make-local-variable 'compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist
            (cons (list lua-traceback-line-re 1 2)
                  compilation-error-regexp-alist))
      (compilation-shell-minor-mode 1)
      (setq-local comint-prompt-regexp lua-prompt-regexp)

      ;; Don't send initialization code until seeing the prompt to ensure that
      ;; the interpreter is ready.
      (while (not (lua-prompt-line))
        (accept-process-output (get-buffer-process (current-buffer)))
        (goto-char (point-max)))
      (lua-send-string lua-process-init-code)))

  ;; when called interactively, switch to process buffer
  (if (called-interactively-p 'any)
      (switch-to-buffer lua-process-buffer)))

(defun lua-get-create-process ()
  "Return active Lua process creating one if necessary."
  (lua-start-process)
  lua-process)

(defun lua-kill-process ()
  "Kill Lua process and its buffer."
  (interactive)
  (when (buffer-live-p lua-process-buffer)
    (kill-buffer lua-process-buffer)
    (setq lua-process-buffer nil)))

(defun lua-set-lua-region-start (&optional arg)
  "Set start of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-start (or arg (point))))

(defun lua-set-lua-region-end (&optional arg)
  "Set end of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-end (or arg (point))))

(defun lua-send-string (str)
  "Send STR plus a newline to the Lua process.

If `lua-process' is nil or dead, start a new process first."
  (unless (string-equal (substring str -1) "\n")
    (setq str (concat str "\n")))
  (process-send-string (lua-get-create-process) str))

(defun lua-send-current-line ()
  "Send current line to the Lua process, found in `lua-process'.
If `lua-process' is nil or dead, start a new process first."
  (interactive)
  (lua-send-region (line-beginning-position) (line-end-position)))

(defun lua-send-defun (pos)
  "Send the function definition around point to the Lua process."
  (interactive "d")
  (save-excursion
    (let ((start (if (save-match-data (looking-at "^function[ \t]"))
                     ;; point already at the start of "function".
                     ;; We need to handle this case explicitly since
                     ;; lua-beginning-of-proc will move to the
                     ;; beginning of the _previous_ function.
                     (point)
                   ;; point is not at the beginning of function, move
                   ;; there and bind start to that position
                   (lua-beginning-of-proc)
                   (point)))
          (end (progn (lua-end-of-proc) (point))))

      ;; make sure point is in a function definition before sending to
      ;; the process
      (if (and (>= pos start) (< pos end))
          (lua-send-region start end)
        (error "Not on a function definition")))))

(defun lua-maybe-skip-shebang-line (start)
  "Skip shebang (#!/path/to/interpreter/) line at beginning of buffer.

Return a position that is after Lua-recognized shebang line (1st
character in file must be ?#) if START is at its beginning.
Otherwise, return START."
  (save-restriction
    (widen)
    (if (and (eq start (point-min))
             (eq (char-after start) ?#))
        (save-excursion
          (goto-char start)
          (forward-line)
          (point))
      start)))

(defun lua-send-region (start end)
  (interactive "r")
  (setq start (lua-maybe-skip-shebang-line start))
  (let* ((lineno (line-number-at-pos start))
         (lua-file (or (buffer-file-name) (buffer-name)))
         (region-str (buffer-substring-no-properties start end))
         (command
          ;; Print empty line before executing the code so that the first line
          ;; of output doesn't end up on the same line as current prompt.
          (format "print(''); luamode_loadstring(%s, %s, %s);\n"
                  (lua-make-lua-string region-str)
                  (lua-make-lua-string lua-file)
                  lineno)))
    (lua-send-string command)
    (when lua-always-show (lua-show-process-buffer))))

(defun lua-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun lua-send-lua-region ()
  "Send preset Lua region to Lua process."
  (interactive)
  (unless (and lua-region-start lua-region-end)
    (error "lua-region not set"))
  (lua-send-region lua-region-start lua-region-end))

(defalias 'lua-send-proc 'lua-send-defun)

(defun lua-send-buffer ()
  "Send whole buffer to Lua process."
  (interactive)
  (lua-send-region (point-min) (point-max)))

(defun lua-restart-with-whole-file ()
  "Restart Lua process and send whole file as input."
  (interactive)
  (lua-kill-process)
  (lua-send-buffer))

(defun lua-show-process-buffer ()
  "Make sure `lua-process-buffer' is being displayed.
Create a Lua process if one doesn't already exist."
  (interactive)
  (display-buffer (process-buffer (lua-get-create-process))))


(defun lua-hide-process-buffer ()
  "Delete all windows that display `lua-process-buffer'."
  (interactive)
  (when (buffer-live-p lua-process-buffer)
    (delete-windows-on lua-process-buffer)))

(defun lua--funcname-char-p (c)
  "Check if character C is part of a function name.
Return nil if C is nil. See `lua-funcname-at-point'."
  (and c (string-match-p "\\`[A-Za-z_.]\\'" (string c))))

(defun lua-funcname-at-point ()
  "Get current Name { '.' Name } sequence."
  (when (or (lua--funcname-char-p (char-before))
            (lua--funcname-char-p (char-after)))
    (save-excursion
      (save-match-data
        (re-search-backward "\\`\\|[^A-Za-z_.]")
        ;; NOTE: `point' will be either at the start of the buffer or on a
        ;; non-symbol character.
        (re-search-forward "\\([A-Za-z_]+\\(?:\\.[A-Za-z_]+\\)*\\)")
        (match-string-no-properties 1)))))

(defun lua-search-documentation ()
  "Search Lua documentation for the word at the point."
  (interactive)
  (let ((url (concat lua-documentation-url "#pdf-" (lua-funcname-at-point))))
    (funcall lua-documentation-function url)))

(defun lua-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq lua-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not lua-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" lua-electric-flag))

(defun lua-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  ;; negative offsets not supported
  (cl-assert (or (not count) (>= count 0)))
  (save-match-data
    (let ((count (or count 1))
          (block-start (mapcar 'car lua-sexp-alist)))
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (lua-find-matching-token-word keyword 'forward))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))

;; Flymake integration

(defcustom lua-luacheck-program "luacheck"
  "Name of the luacheck executable."
  :type 'string
  :group 'lua)

(defvar-local lua--flymake-process nil)

(defun lua-flymake (report-fn &rest _args)
  "Flymake backend using the luacheck program.
Takes a Flymake callback REPORT-FN as argument, as expected of a
member of `flymake-diagnostic-functions'."
  (when (process-live-p lua--flymake-process)
    (kill-process lua--flymake-process))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq lua--flymake-process
            (make-process
             :name "luacheck" :noquery t :connection-type 'pipe
             :buffer (generate-new-buffer " *flymake-luacheck*")
             :command `(,lua-luacheck-program
                        "--codes" "--ranges" "--formatter" "plain" "-")
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (if (with-current-buffer source
                           (eq proc lua--flymake-process))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (cl-loop
                            while (search-forward-regexp
                                   "^\\([^:]*\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\): \\(.*\\)$"
                                   nil t)
                            for line = (string-to-number (match-string 2))
                            for col1 = (string-to-number (match-string 3))
                            for col2 = (1+ (string-to-number (match-string 4)))
                            for msg = (match-string 5)
                            for type = (if (string-match-p "\\`(E" msg) :error :warning)
                            collect (flymake-make-diagnostic source
                                                             (cons line col1)
                                                             (cons line col2)
                                                             type
                                                             msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region lua--flymake-process (point-min) (point-max))
      (process-send-eof lua--flymake-process))))

;; menu bar

(define-key lua-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  lua-restart-with-whole-file))
(define-key lua-mode-menu [kill-process]
  '("Kill Process" . lua-kill-process))

(define-key lua-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . lua-hide-process-buffer))
(define-key lua-mode-menu [show-process-buffer]
  '("Show Process Buffer" . lua-show-process-buffer))

(define-key lua-mode-menu [end-of-proc]
  '("End Of Proc" . lua-end-of-proc))
(define-key lua-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . lua-beginning-of-proc))

(define-key lua-mode-menu [send-lua-region]
  '("Send Lua-Region" . lua-send-lua-region))
(define-key lua-mode-menu [set-lua-region-end]
  '("Set Lua-Region End" . lua-set-lua-region-end))
(define-key lua-mode-menu [set-lua-region-start]
  '("Set Lua-Region Start" . lua-set-lua-region-start))

(define-key lua-mode-menu [send-current-line]
  '("Send Current Line" . lua-send-current-line))
(define-key lua-mode-menu [send-region]
  '("Send Region" . lua-send-region))
(define-key lua-mode-menu [send-proc]
  '("Send Proc" . lua-send-proc))
(define-key lua-mode-menu [send-buffer]
  '("Send Buffer" . lua-send-buffer))
(define-key lua-mode-menu [search-documentation]
  '("Search Documentation" . lua-search-documentation))


(provide 'lua-mode)

;;; lua-mode.el ends here
