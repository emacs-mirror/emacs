;;; eglot.el --- The Emacs Client for LSP servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Free Software Foundation, Inc.

;; Version: 1.16
;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.23") (flymake "1.2.1") (project "0.9.8") (xref "1.6.2") (eldoc "1.14.0") (seq "2.23") (external-completion "0.1"))

;; This is a GNU ELPA :core package.  Avoid adding functionality
;; that is not available in the version of Emacs recorded above or any
;; of the package dependencies.

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

;; Eglot ("Emacs Polyglot") is an Emacs LSP client that stays out of
;; your way.
;;
;; Typing M-x eglot in some source file is often enough to get you
;; started, if the language server you're looking to use is installed
;; in your system.  Please refer to the manual, available from
;; https://joaotavora.github.io/eglot/ or from M-x info for more usage
;; instructions.
;;
;; If you wish to contribute changes to Eglot, please do read the user
;; manual first.  Additionally, take the following in consideration:

;; * Eglot's main job is to hook up the information that language
;;   servers offer via LSP to Emacs's UI facilities: Xref for
;;   definition-chasing, Flymake for diagnostics, Eldoc for at-point
;;   documentation, etc.  Eglot's job is generally *not* to provide
;;   such a UI itself, though a small number of simple
;;   counter-examples do exist, e.g. in the `eglot-rename' command or
;;   the `eglot-inlay-hints-mode' minor mode.  When a new UI is
;;   evidently needed, consider adding a new package to Emacs, or
;;   extending an existing one.
;;
;; * Eglot was designed to function with just the UI facilities found
;;   in the latest Emacs core, as long as those facilities are also
;;   available as GNU ELPA :core packages.  Historically, a number of
;;   :core packages were added or reworked in Emacs to make this
;;   possible.  This principle should be upheld when adding new LSP
;;   features or tweaking existing ones.  Design any new facilities in
;;   a way that they could work in the absence of LSP or using some
;;   different protocol, then make sure Eglot can link up LSP
;;   information to it.

;; * There are few Eglot configuration variables.  This principle
;;   should also be upheld.  If Eglot had these variables, it could be
;;   duplicating configuration found elsewhere, bloating itself up,
;;   and making it generally hard to integrate with the ever growing
;;   set of LSP features and Emacs packages.  For instance, this is
;;   why one finds a single variable
;;   `eglot-ignored-server-capabilities' instead of a number of
;;   capability-specific flags, or why customizing the display of
;;   LSP-provided documentation is done via ElDoc's variables, not
;;   Eglot's.
;;
;; * Linking up LSP information to other libraries is generally done
;;   in the `eglot--managed-mode' minor mode function, by
;;   buffer-locally setting the other library's variables to
;;   Eglot-specific versions.  When deciding what to set the variable
;;   to, the general idea is to choose a good default for beginners
;;   that doesn't clash with Emacs's defaults.  The settings are only
;;   in place during Eglot's LSP-enriched tenure over a project.  Even
;;   so, some of those decisions will invariably aggravate a minority
;;   of Emacs power users, but these users can use `eglot-stay-out-of'
;;   and `eglot-managed-mode-hook' to adjust things to their
;;   preferences.
;;
;; * On occasion, to enable new features, Eglot can have soft
;;   dependencies on popular libraries that are not in Emacs core.
;;   "Soft" means that the dependency doesn't impair any other use of
;;   Eglot beyond that feature.  Such is the case of the snippet
;;   functionality, via the Yasnippet package, Markdown formatting of
;;   at-point documentation via the markdown-mode package, and nicer
;;   looking completions when the Company package is used.

;;; Code:

(require 'imenu)
(require 'cl-lib)

(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile) ; for some faces
(require 'warnings)
(eval-when-compile
  (require 'subr-x))
(require 'filenotify)
(require 'ert)
(require 'text-property-search nil t)
(require 'diff-mode)
(require 'diff)

;; These dependencies are also GNU ELPA core packages.  Because of
;; bug#62576, since there is a risk that M-x package-install, despite
;; having installed them, didn't correctly re-load them over the
;; built-in versions.
(eval-and-compile
  (load "project")
  (load "eldoc")
  (load "seq")
  (load "flymake")
  (load "xref")
  (load "jsonrpc")
  (load "external-completion"))

;; forward-declare, but don't require (Emacs 28 doesn't seem to care)
(defvar markdown-fontify-code-blocks-natively)
(defvar company-backends)
(defvar company-tooltip-align-annotations)
(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-ssh-controlmaster-options)


;;; Obsolete aliases
;;;
(make-obsolete-variable 'eglot--managed-mode-hook
                        'eglot-managed-mode-hook "1.6")
(define-obsolete-variable-alias 'eglot-confirm-server-initiated-edits
  'eglot-confirm-server-edits "1.16")
(make-obsolete-variable 'eglot-events-buffer-size
  'eglot-events-buffer-config "1.16")
(define-obsolete-function-alias 'eglot--uri-to-path 'eglot-uri-to-path "1.16")
(define-obsolete-function-alias 'eglot--path-to-uri 'eglot-path-to-uri "1.16")
(define-obsolete-function-alias 'eglot--range-region 'eglot-range-region "1.16")
(define-obsolete-function-alias 'eglot--server-capable 'eglot-server-capable "1.16")
(define-obsolete-function-alias 'eglot--server-capable-or-lose 'eglot-server-capable-or-lose "1.16")
(define-obsolete-function-alias
  'eglot-lsp-abiding-column 'eglot-utf-16-linepos "1.12")
(define-obsolete-function-alias
  'eglot-current-column 'eglot-utf-32-linepos "1.12")
(define-obsolete-variable-alias
  'eglot-current-column-function 'eglot-current-linepos-function "1.12")
(define-obsolete-function-alias
  'eglot-move-to-current-column 'eglot-move-to-utf-32-linepos "1.12")
(define-obsolete-function-alias
  'eglot-move-to-lsp-abiding-column 'eglot-move-to-utf-16-linepos "1.12")
(define-obsolete-variable-alias
'eglot-move-to-column-function 'eglot-move-to-linepos-function "1.12")
(define-obsolete-variable-alias 'eglot-ignored-server-capabilites
  'eglot-ignored-server-capabilities "1.8")
;;;###autoload
(define-obsolete-function-alias 'eglot-update 'eglot-upgrade-eglot "29.1")


;;; User tweakable stuff
(defgroup eglot nil
  "Interaction with Language Server Protocol servers."
  :prefix "eglot-"
  :group 'applications)

(defun eglot-alternatives (alternatives)
  "Compute server-choosing function for `eglot-server-programs'.
Each element of ALTERNATIVES is a string PROGRAM or a list of
strings (PROGRAM ARGS...) where program names an LSP server
program to start with ARGS.  Returns a function of one argument.
When invoked, that function will return a list (ABSPATH ARGS),
where ABSPATH is the absolute path of the PROGRAM that was
chosen (interactively or automatically)."
  (lambda (&optional interactive)
    ;; JT@2021-06-13: This function is way more complicated than it
    ;; could be because it accounts for the fact that
    ;; `eglot--executable-find' may take much longer to execute on
    ;; remote files.
    (let* ((listified (cl-loop for a in alternatives
                               collect (if (listp a) a (list a))))
           (err (lambda ()
                  (error "None of '%s' are valid executables"
                         (mapconcat #'car listified ", ")))))
      (cond (interactive
             (let* ((augmented (mapcar (lambda (a)
                                         (let ((found (eglot--executable-find
                                                       (car a) t)))
                                           (and found
                                                (cons (car a) (cons found (cdr a))))))
                                       listified))
                    (available (remove nil augmented)))
               (cond ((cdr available)
                      (cdr (assoc
                            (completing-read
                             "[eglot] More than one server executable available: "
                             (mapcar #'car available)
                             nil t nil nil (car (car available)))
                            available #'equal)))
                     ((cdr (car available)))
                     (t
                      ;; Don't error when used interactively, let the
                      ;; Eglot prompt the user for alternative (github#719)
                      nil))))
            (t
             (cl-loop for (p . args) in listified
                      for probe = (eglot--executable-find p t)
                      when probe return (cons probe args)
                      finally (funcall err)))))))

(defvar eglot-server-programs `(((rust-ts-mode rust-mode) . ("rust-analyzer"))
                                ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                                (vimrc-mode . ("vim-language-server" "--stdio"))
                                ((python-mode python-ts-mode)
                                 . ,(eglot-alternatives
                                     '("pylsp" "pyls" ("pyright-langserver" "--stdio") "jedi-language-server" "ruff-lsp")))
                                ((js-json-mode json-mode json-ts-mode)
                                 . ,(eglot-alternatives '(("vscode-json-language-server" "--stdio")
                                                          ("vscode-json-languageserver" "--stdio")
                                                          ("json-languageserver" "--stdio"))))
                                (((js-mode :language-id "javascript")
                                  (js-ts-mode :language-id "javascript")
                                  (tsx-ts-mode :language-id "typescriptreact")
                                  (typescript-ts-mode :language-id "typescript")
                                  (typescript-mode :language-id "typescript"))
                                 . ("typescript-language-server" "--stdio"))
                                ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
                                ((php-mode phps-mode)
                                 . ,(eglot-alternatives
                                     '(("phpactor" "language-server")
                                       ("php" "vendor/felixfbecker/language-server/bin/php-language-server.php"))))
                                ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                                 . ,(eglot-alternatives
                                     '("clangd" "ccls")))
                                (((caml-mode :language-id "ocaml")
                                  (tuareg-mode :language-id "ocaml") reason-mode)
                                 . ("ocamllsp"))
                                ((ruby-mode ruby-ts-mode)
                                 . ("solargraph" "socket" "--port" :autoport))
                                (haskell-mode
                                 . ("haskell-language-server-wrapper" "--lsp"))
                                (elm-mode . ("elm-language-server"))
                                (mint-mode . ("mint" "ls"))
                                (kotlin-mode . ("kotlin-language-server"))
                                ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                                 . ("gopls"))
                                ((R-mode ess-r-mode) . ("R" "--slave" "-e"
                                                        "languageserver::run()"))
                                ((java-mode java-ts-mode) . ("jdtls"))
                                ((dart-mode dart-ts-mode)
                                 . ("dart" "language-server"
                                    "--client-id" "emacs.eglot-dart"))
                                ((elixir-mode elixir-ts-mode heex-ts-mode)
                                 . ,(if (and (fboundp 'w32-shell-dos-semantics)
                                             (w32-shell-dos-semantics))
                                        '("language_server.bat")
                                      (eglot-alternatives
                                       '("language_server.sh" "start_lexical.sh"))))
                                (ada-mode . ("ada_language_server"))
                                (scala-mode . ,(eglot-alternatives
                                                '("metals" "metals-emacs")))
                                (racket-mode . ("racket" "-l" "racket-langserver"))
                                ((tex-mode context-mode texinfo-mode bibtex-mode)
                                 . ,(eglot-alternatives '("digestif" "texlab")))
                                (erlang-mode . ("erlang_ls" "--transport" "stdio"))
                                ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
                                (nix-mode . ,(eglot-alternatives '("nil" "rnix-lsp" "nixd")))
                                (nickel-mode . ("nls"))
                                (gdscript-mode . ("localhost" 6008))
                                ((fortran-mode f90-mode) . ("fortls"))
                                (futhark-mode . ("futhark" "lsp"))
                                ((lua-mode lua-ts-mode) . ,(eglot-alternatives
                                                            '("lua-language-server" "lua-lsp")))
                                (zig-mode . ("zls"))
                                ((css-mode css-ts-mode)
                                 . ,(eglot-alternatives '(("vscode-css-language-server" "--stdio")
                                                          ("css-languageserver" "--stdio"))))
                                (html-mode . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio") ("html-languageserver" "--stdio"))))
                                ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))
                                ((clojure-mode clojurescript-mode clojurec-mode clojure-ts-mode)
                                 . ("clojure-lsp"))
                                ((csharp-mode csharp-ts-mode)
                                 . ,(eglot-alternatives
                                     '(("omnisharp" "-lsp")
                                       ("csharp-ls"))))
                                (purescript-mode . ("purescript-language-server" "--stdio"))
                                ((perl-mode cperl-mode) . ("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"))
                                (markdown-mode
                                 . ,(eglot-alternatives
                                     '(("marksman" "server")
                                       ("vscode-markdown-language-server" "--stdio"))))
                                (graphviz-dot-mode . ("dot-language-server" "--stdio"))
                                (terraform-mode . ("terraform-ls" "serve"))
                                ((uiua-ts-mode uiua-mode) . ("uiua" "lsp")))
  "How the command `eglot' guesses the server to start.
An association list of (MAJOR-MODE . CONTACT) pairs.  MAJOR-MODE
identifies the buffers that are to be managed by a specific
language server.  The associated CONTACT specifies how to connect
to a server for those buffers.

MAJOR-MODE can be:

* In the most common case, a symbol such as `c-mode';

* A list (MAJOR-MODE-SYMBOL :LANGUAGE-ID ID) where
  MAJOR-MODE-SYMBOL is the aforementioned symbol and ID is a
  string identifying the language to the server;

* A list combining the previous two alternatives, meaning
  multiple major modes will be associated with a single server
  program.  This association is such that the same resulting
  server process will manage buffers of different major modes.

CONTACT can be:

* In the most common case, a list of strings (PROGRAM [ARGS...]).
  PROGRAM is called with ARGS and is expected to serve LSP requests
  over the standard input/output channels.

* A list (PROGRAM [ARGS...] :initializationOptions OPTIONS),
  whereupon PROGRAM is called with ARGS as in the first option,
  and the LSP \"initializationOptions\" JSON object is
  constructed from OPTIONS.  If OPTIONS is a unary function, it
  is called with the server instance and should return a JSON
  object.

* A list (HOST PORT [TCP-ARGS...]) where HOST is a string and
  PORT is a positive integer for connecting to a server via TCP.
  Remaining ARGS are passed to `open-network-stream' for
  upgrading the connection with encryption or other capabilities.

* A list (PROGRAM [ARGS...] :autoport [MOREARGS...]), whereupon a
  combination of previous options is used.  First, an attempt is
  made to find an available server port, then PROGRAM is launched
  with ARGS; the `:autoport' keyword substituted for that number;
  and MOREARGS.  Eglot then attempts to establish a TCP
  connection to that port number on the localhost.

* A cons (CLASS-NAME . INITARGS) where CLASS-NAME is a symbol
  designating a subclass of `eglot-lsp-server', for representing
  experimental LSP servers.  INITARGS is a keyword-value plist
  used to initialize the object of CLASS-NAME, or a plain list
  interpreted as the previous descriptions of CONTACT.  In the
  latter case that plain list is used to produce a plist with a
  suitable :PROCESS initarg to CLASS-NAME.  The class
  `eglot-lsp-server' descends from `jsonrpc-process-connection',
  which you should see for the semantics of the mandatory
  :PROCESS argument.

* A function of a single argument producing any of the above
  values for CONTACT.  The argument's value is non-nil if the
  connection was requested interactively (e.g. from the `eglot'
  command), and nil if it wasn't (e.g. from `eglot-ensure').  If
  the call is interactive, the function can ask the user for
  hints on finding the required programs, etc.  Otherwise, it
  should not ask the user for any input, and return nil or signal
  an error if it can't produce a valid CONTACT.  The helper
  function `eglot-alternatives' (which see) can be used to
  produce a function that offers more than one server for a given
  MAJOR-MODE.")

(defface eglot-highlight-symbol-face
  '((t (:inherit bold)))
  "Face used to highlight the symbol at point.")

(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in Eglot's mode line.")

(defface eglot-diagnostic-tag-unnecessary-face
  '((t (:inherit shadow)))
  "Face used to render unused or unnecessary code.")

(defface eglot-diagnostic-tag-deprecated-face
  '((t . (:inherit shadow :strike-through t)))
  "Face used to render deprecated or obsolete code.")

(defcustom eglot-autoreconnect 3
  "Control ability to reconnect automatically to the LSP server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (const :tag "Reconnect automatically" t)
                 (const :tag "Never reconnect" nil)
                 (integer :tag "Number of seconds")))

(defcustom eglot-connect-timeout 30
  "Number of seconds before timing out LSP connection attempts.
If nil, never time out."
  :type '(choice (number :tag "Number of seconds")
                 (const  :tag "Never time out" nil)))

(defcustom eglot-sync-connect 3
  "Control blocking of LSP connection attempts.
If t, block for `eglot-connect-timeout' seconds.  A positive
integer number means block for that many seconds, and then wait
for the connection in the background.  nil has the same meaning
as 0, i.e. don't block at all."
  :type '(choice (const :tag "Block for `eglot-connect-timeout' seconds" t)
                 (const :tag "Never block" nil)
                 (integer :tag "Number of seconds to block")))

(defcustom eglot-autoshutdown nil
  "If non-nil, shut down server after killing last managed buffer."
  :type 'boolean)

(defcustom eglot-send-changes-idle-time 0.5
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :type 'number)

(defcustom eglot-events-buffer-config
  (list :size (or (bound-and-true-p eglot-events-buffer-size) 2000000)
        :format 'full)
  "Configure the Eglot events buffer.

Value is a plist accepting the keys `:size', which controls the
size in characters of the buffer (0 disables, nil means
infinite), and `:format', which controls the shape of each log
entry (`full' includes the original JSON, `lisp' uses
pretty-printed Lisp).

For changes on this variable to take effect, you need to restart
the LSP connection.  That can be done by `eglot-reconnect'."
  :type '(plist :key-type (symbol :tag "Keyword")
                :options (((const :tag "Size" :size)
                           (choice
                            (const :tag "No limit" nil)
                            (integer :tag "Number of characters")))
                          ((const :tag "Format" :format)
                           (choice
                            (const :tag "Full with original JSON" full)
                            (const :tag "Shortened" short)
                            (const :tag "Pretty-printed lisp" lisp))))))

(defcustom eglot-confirm-server-edits '((eglot-rename . nil)
                                        (t . maybe-summary))
  "Control if changes proposed by LSP should be confirmed with user.

If this variable's value is the symbol `diff', a diff buffer is
pops up, allowing the user to apply each change individually.  If
the symbol `summary' or any other non-nil value, the user is
prompted in the minibuffer with aa short summary of changes.  The
symbols `maybe-diff' and `maybe-summary' mean that the
confirmation is offered to the user only if the changes target
files visited in buffers.  Finally, a nil value means all changes
are applied directly without any confirmation.

If this variable's value can also be an alist ((COMMAND . ACTION)
...) where COMMAND is a symbol designating a command, such as
`eglot-rename', `eglot-code-actions',
`eglot-code-action-quickfix', etc.  ACTION is one of the symbols
described above.  The value `t' for COMMAND is accepted and its
ACTION is the default value for commands not in the alist."
  :type (let ((basic-choices
               '((const :tag "Use diff" diff)
                 (const :tag "Summarize and prompt" summary)
                 (const :tag "Maybe use diff" maybe-diff)
                 (const :tag "Maybe summarize and prompt" maybe-summary)
                 (const :tag "Don't confirm" nil))))
          `(choice ,@basic-choices
                   (alist :tag "Per-command alist"
                          :key-type (choice (function :tag "Command")
                                            (const :tag "Default" t))
                          :value-type (choice . ,basic-choices)))))

(defcustom eglot-extend-to-xref nil
  "If non-nil, activate Eglot in cross-referenced non-project files."
  :type 'boolean)

(defcustom eglot-prefer-plaintext nil
  "If non-nil, always request plaintext responses to hover requests."
  :type 'boolean)

(defcustom eglot-menu-string "eglot"
  "String displayed in mode line when Eglot is active."
  :type 'string)

(defcustom eglot-report-progress t
  "If non-nil, show progress of long running LSP server work.
If set to `messages', use *Messages* buffer, else use Eglot's
mode line indicator."
  :type '(choice (const :tag "Don't show progress" nil)
                 (const :tag "Show progress in *Messages*" messages)
                 (const :tag "Show progress in Eglot's mode line indicator" t))
  :version "1.10")

(defcustom eglot-ignored-server-capabilities (list)
  "LSP server capabilities that Eglot could use, but won't.
You could add, for instance, the symbol
`:documentHighlightProvider' to prevent automatic highlighting
under cursor."
  :type '(set
          :tag "Tick the ones you're not interested in"
          (const :tag "Documentation on hover" :hoverProvider)
          (const :tag "Code completion" :completionProvider)
          (const :tag "Function signature help" :signatureHelpProvider)
          (const :tag "Go to definition" :definitionProvider)
          (const :tag "Go to type definition" :typeDefinitionProvider)
          (const :tag "Go to implementation" :implementationProvider)
          (const :tag "Go to declaration" :declarationProvider)
          (const :tag "Find references" :referencesProvider)
          (const :tag "Highlight symbols automatically" :documentHighlightProvider)
          (const :tag "List symbols in buffer" :documentSymbolProvider)
          (const :tag "List symbols in workspace" :workspaceSymbolProvider)
          (const :tag "Execute code actions" :codeActionProvider)
          (const :tag "Code lens" :codeLensProvider)
          (const :tag "Format buffer" :documentFormattingProvider)
          (const :tag "Format portion of buffer" :documentRangeFormattingProvider)
          (const :tag "On-type formatting" :documentOnTypeFormattingProvider)
          (const :tag "Rename symbol" :renameProvider)
          (const :tag "Highlight links in document" :documentLinkProvider)
          (const :tag "Decorate color references" :colorProvider)
          (const :tag "Fold regions of buffer" :foldingRangeProvider)
          (const :tag "Execute custom commands" :executeCommandProvider)
          (const :tag "Inlay hints" :inlayHintProvider)))

(defvar eglot-withhold-process-id nil
  "If non-nil, Eglot will not send the Emacs process id to the language server.
This can be useful when using docker to run a language server.")


;;; Constants
;;;
(defconst eglot--version
  (eval-when-compile
    (when byte-compile-current-file
      (require 'lisp-mnt)
      (lm-version byte-compile-current-file)))
  "The version as a string of this version of Eglot.
It is nil if Eglot is not byte-complied.")

(defconst eglot--symbol-kind-names
  `((1 . "File") (2 . "Module")
    (3 . "Namespace") (4 . "Package") (5 . "Class")
    (6 . "Method") (7 . "Property") (8 . "Field")
    (9 . "Constructor") (10 . "Enum") (11 . "Interface")
    (12 . "Function") (13 . "Variable") (14 . "Constant")
    (15 . "String") (16 . "Number") (17 . "Boolean")
    (18 . "Array") (19 . "Object") (20 . "Key")
    (21 . "Null") (22 . "EnumMember") (23 . "Struct")
    (24 . "Event") (25 . "Operator") (26 . "TypeParameter")))

(defconst eglot--kind-names
  `((1 . "Text") (2 . "Method") (3 . "Function") (4 . "Constructor")
    (5 . "Field") (6 . "Variable") (7 . "Class") (8 . "Interface")
    (9 . "Module") (10 . "Property") (11 . "Unit") (12 . "Value")
    (13 . "Enum") (14 . "Keyword") (15 . "Snippet") (16 . "Color")
    (17 . "File") (18 . "Reference") (19 . "Folder") (20 . "EnumMember")
    (21 . "Constant") (22 . "Struct") (23 . "Event") (24 . "Operator")
    (25 . "TypeParameter")))

(defconst eglot--tag-faces
  `((1 . eglot-diagnostic-tag-unnecessary-face)
    (2 . eglot-diagnostic-tag-deprecated-face)))

(defvaralias 'eglot-{} 'eglot--{})

(defconst eglot--{} (make-hash-table :size 1) "The empty JSON object.")

(defun eglot--executable-find (command &optional remote)
  "Like Emacs 27's `executable-find', ignore REMOTE on Emacs 26."
  (if (>= emacs-major-version 27) (executable-find command remote)
    (executable-find command)))

(defun eglot--accepted-formats ()
  (if (and (not eglot-prefer-plaintext) (fboundp 'gfm-view-mode))
      ["markdown" "plaintext"] ["plaintext"]))

(defconst eglot--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil) ;; see github#639
    vec)
  "Like `url-path-allows-chars' but more restrictive.")


;;; Message verification helpers
;;;
(eval-and-compile
  (defvar eglot--lsp-interface-alist
    `(
      (CodeAction (:title) (:kind :diagnostics :edit :command :isPreferred :data))
      (ConfigurationItem () (:scopeUri :section))
      (Command ((:title . string) (:command . string)) (:arguments))
      (CompletionItem (:label)
                      (:kind :detail :documentation :deprecated :preselect
                             :sortText :filterText :insertText :insertTextFormat
                             :textEdit :additionalTextEdits :commitCharacters
                             :command :data :tags))
      (Diagnostic (:range :message) (:severity :code :source :relatedInformation :codeDescription :tags))
      (DocumentHighlight (:range) (:kind))
      (FileSystemWatcher (:globPattern) (:kind))
      (Hover (:contents) (:range))
      (InitializeResult (:capabilities) (:serverInfo))
      (Location (:uri :range))
      (LocationLink (:targetUri :targetRange :targetSelectionRange) (:originSelectionRange))
      (LogMessageParams (:type :message))
      (MarkupContent (:kind :value))
      (ParameterInformation (:label) (:documentation))
      (Position (:line :character))
      (Range (:start :end))
      (Registration (:id :method) (:registerOptions))
      (ResponseError (:code :message) (:data))
      (ShowMessageParams (:type :message))
      (ShowMessageRequestParams (:type :message) (:actions))
      (SignatureHelp (:signatures) (:activeSignature :activeParameter))
      (SignatureInformation (:label) (:documentation :parameters :activeParameter))
      (SymbolInformation (:name :kind :location)
                         (:deprecated :containerName))
      (DocumentSymbol (:name :range :selectionRange :kind)
                      (:detail :deprecated :children))
      (TextDocumentEdit (:textDocument :edits) ())
      (TextEdit (:range :newText))
      (VersionedTextDocumentIdentifier (:uri :version) ())
      (WorkDoneProgress (:kind) (:title :message :percentage :cancellable))
      (WorkspaceEdit () (:changes :documentChanges))
      (WorkspaceSymbol (:name :kind) (:containerName :location :data))
      (InlayHint (:position :label) (:kind :textEdits :tooltip :paddingLeft
                                           :paddingRight :data))
      (InlayHintLabelPart (:value) (:tooltip :location :command)))
    "Alist (INTERFACE-NAME . INTERFACE) of known external LSP interfaces.

INTERFACE-NAME is a symbol designated by the spec as
\"interface\".  INTERFACE is a list (REQUIRED OPTIONAL) where
REQUIRED and OPTIONAL are lists of KEYWORD designating field
names that must be, or may be, respectively, present in a message
adhering to that interface.  KEY can be a keyword or a cons (SYM
TYPE), where type is used by `cl-typep' to check types at
runtime.

Here's what an element of this alist might look like:

    (Command ((:title . string) (:command . string)) (:arguments))"))

(eval-and-compile
  (defvar eglot-strict-mode
    '(;; Uncomment next lines for fun and debugging
      ;; disallow-non-standard-keys
      ;; enforce-required-keys
      ;; enforce-optional-keys
      no-unknown-interfaces)
    "How strictly to check LSP interfaces at compile- and run-time.

Value is a list of symbols (if the list is empty, no checks are
performed).

If the symbol `disallow-non-standard-keys' is present, an error
is raised if any extraneous fields are sent by the server.  At
compile-time, a warning is raised if a destructuring spec
includes such a field.

If the symbol `enforce-required-keys' is present, an error is
raised if any required fields are missing from the message sent
from the server.  At compile-time, a warning is raised if a
destructuring spec doesn't use such a field.

If the symbol `enforce-optional-keys' is present, nothing special
happens at run-time.  At compile-time, a warning is raised if a
destructuring spec doesn't use all optional fields.

If the symbol `disallow-unknown-methods' is present, Eglot warns
on unknown notifications and errors on unknown requests.

If the symbol `no-unknown-interfaces' is present, Eglot warns at
compile time if an undeclared LSP interface is used."))

(cl-defun eglot--check-object (interface-name
                               object
                               &optional
                               (enforce-required t)
                               (disallow-non-standard t)
                               (check-types t))
  "Check that OBJECT conforms to INTERFACE.  Error otherwise."
  (cl-destructuring-bind
      (&key types required-keys optional-keys &allow-other-keys)
      (eglot--interface interface-name)
    (when-let ((missing (and enforce-required
                             (cl-set-difference required-keys
                                                (eglot--plist-keys object)))))
      (eglot--error "A `%s' must have %s" interface-name missing))
    (when-let ((excess (and disallow-non-standard
                            (cl-set-difference
                             (eglot--plist-keys object)
                             (append required-keys optional-keys)))))
      (eglot--error "A `%s' mustn't have %s" interface-name excess))
    (when check-types
      (cl-loop
       for (k v) on object by #'cddr
       for type = (or (cdr (assoc k types)) t) ;; FIXME: enforce nil type?
       unless (cl-typep v type)
       do (eglot--error "A `%s' must have a %s as %s, but has %s"
                        interface-name)))
    t))

(eval-and-compile
  (defun eglot--keywordize-vars (vars)
    (mapcar (lambda (var) (intern (format ":%s" var))) vars))

  (defun eglot--ensure-type (k) (if (consp k) k (cons k t)))

  (defun eglot--interface (interface-name)
    (let* ((interface (assoc interface-name eglot--lsp-interface-alist))
           (required (mapcar #'eglot--ensure-type (car (cdr interface))))
           (optional (mapcar #'eglot--ensure-type (cadr (cdr interface)))))
      (list :types (append required optional)
            :required-keys (mapcar #'car required)
            :optional-keys (mapcar #'car optional))))

  (defun eglot--check-dspec (interface-name dspec)
    "Check destructuring spec DSPEC against INTERFACE-NAME."
    (cl-destructuring-bind (&key required-keys optional-keys &allow-other-keys)
        (eglot--interface interface-name)
      (cond ((or required-keys optional-keys)
             (let ((too-many
                    (and
                     (memq 'disallow-non-standard-keys eglot-strict-mode)
                     (cl-set-difference
                      (eglot--keywordize-vars dspec)
                      (append required-keys optional-keys))))
                   (ignored-required
                    (and
                     (memq 'enforce-required-keys eglot-strict-mode)
                     (cl-set-difference
                      required-keys (eglot--keywordize-vars dspec))))
                   (missing-out
                    (and
                     (memq 'enforce-optional-keys eglot-strict-mode)
                     (cl-set-difference
                      optional-keys (eglot--keywordize-vars dspec)))))
               (when too-many (byte-compile-warn
                               "Destructuring for %s has extraneous %s"
                               interface-name too-many))
               (when ignored-required (byte-compile-warn
                                       "Destructuring for %s ignores required %s"
                                       interface-name ignored-required))
               (when missing-out (byte-compile-warn
                                  "Destructuring for %s is missing out on %s"
                                  interface-name missing-out))))
            ((memq 'no-unknown-interfaces eglot-strict-mode)
             (byte-compile-warn "Unknown LSP interface %s" interface-name))))))

(cl-defmacro eglot--dbind (vars object &body body)
  "Destructure OBJECT, binding VARS in BODY.
VARS is ([(INTERFACE)] SYMS...)
Honor `eglot-strict-mode'."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((interface-name (if (consp (car vars))
                            (car (pop vars))))
        (object-once (make-symbol "object-once"))
        (fn-once (make-symbol "fn-once")))
    (cond (interface-name
           (eglot--check-dspec interface-name vars)
           `(let ((,object-once ,object))
              (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                (eglot--check-object ',interface-name ,object-once
                                     (memq 'enforce-required-keys eglot-strict-mode)
                                     (memq 'disallow-non-standard-keys eglot-strict-mode)
                                     (memq 'check-types eglot-strict-mode))
                ,@body)))
          (t
           `(let ((,object-once ,object)
                  (,fn-once (lambda (,@vars) ,@body)))
              (if (memq 'disallow-non-standard-keys eglot-strict-mode)
                  (cl-destructuring-bind (&key ,@vars) ,object-once
                    (funcall ,fn-once ,@vars))
                (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                  (funcall ,fn-once ,@vars))))))))

(cl-defmacro eglot--lambda (cl-lambda-list &body body)
  "Function of args CL-LAMBDA-LIST for processing INTERFACE objects.
Honor `eglot-strict-mode'."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (cl-block nil (eglot--dbind ,cl-lambda-list ,e ,@body)))))

(cl-defmacro eglot--dcase (obj &rest clauses)
  "Like `pcase', but for the LSP object OBJ.
CLAUSES is a list (DESTRUCTURE FORMS...) where DESTRUCTURE is
treated as in `eglot--dbind'."
  (declare (indent 1) (debug (sexp &rest (sexp &rest form))))
  (let ((obj-once (make-symbol "obj-once")))
    `(let ((,obj-once ,obj))
       (cond
        ,@(cl-loop
           for (vars . body) in clauses
           for vars-as-keywords = (eglot--keywordize-vars vars)
           for interface-name = (if (consp (car vars))
                                    (car (pop vars)))
           for condition =
           (cond (interface-name
                  (eglot--check-dspec interface-name vars)
                  ;; In this mode, in runtime, we assume
                  ;; `eglot-strict-mode' is partially on, otherwise we
                  ;; can't disambiguate between certain types.
                  `(ignore-errors
                     (eglot--check-object
                      ',interface-name ,obj-once
                      t
                      (memq 'disallow-non-standard-keys eglot-strict-mode)
                      t)))
                 (t
                  ;; In this interface-less mode we don't check
                  ;; `eglot-strict-mode' at all: just check that the object
                  ;; has all the keys the user wants to destructure.
                  `(null (cl-set-difference
                          ',vars-as-keywords
                          (eglot--plist-keys ,obj-once)))))
           collect `(,condition
                     (cl-destructuring-bind (&key ,@vars &allow-other-keys)
                         ,obj-once
                       ,@body)))
        (t
         (eglot--error "%S didn't match any of %S"
                       ,obj-once
                       ',(mapcar #'car clauses)))))))

(cl-defmacro eglot--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro eglot--when-buffer-window (buf &body body)
  "Check BUF showing somewhere, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf))
       ;;notice the exception when testing with `ert'
       (when (or (get-buffer-window ,b) (ert-running-test))
         (with-current-buffer ,b ,@body)))))

(cl-defmacro eglot--widening (&rest body)
  "Save excursion and restriction.  Widen.  Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))


;;; Public Elisp API
;;;
(cl-defgeneric eglot-handle-request (server method &rest params)
  "Handle SERVER's METHOD request with PARAMS.")

(cl-defgeneric eglot-handle-notification (server method &rest params)
  "Handle SERVER's METHOD notification with PARAMS.")

(cl-defgeneric eglot-execute-command (_ _ _)
  (declare (obsolete eglot-execute "30.1"))
  (:method
   (server command arguments)
   (eglot--request server :workspace/executeCommand
                   `(:command ,(format "%s" command) :arguments ,arguments))))

(cl-defgeneric eglot-execute (server action)
  "Ask SERVER to execute ACTION.
ACTION is an LSP object of either `CodeAction' or `Command' type."
  (:method
   (server action) "Default implementation."
   (eglot--dcase action
     (((Command)) (eglot--request server :workspace/executeCommand action))
     (((CodeAction) edit command data)
      (if (and (null edit) (null command) data
               (eglot-server-capable :codeActionProvider :resolveProvider))
          (eglot-execute server (eglot--request server :codeAction/resolve action))
        (when edit (eglot--apply-workspace-edit edit this-command))
        (when command (eglot--request server :workspace/executeCommand command)))))))

(cl-defgeneric eglot-initialization-options (server)
  "JSON object to send under `initializationOptions'."
  (:method (s)
   (let ((probe (plist-get (eglot--saved-initargs s) :initializationOptions)))
     (cond ((functionp probe) (funcall probe s))
           (probe)
           (t eglot--{})))))

(cl-defgeneric eglot-register-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (eglot--warn "Server tried to register unsupported capability `%s'"
                method)))

(cl-defgeneric eglot-unregister-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (eglot--warn "Server tried to unregister unsupported capability `%s'"
                method)))

(cl-defgeneric eglot-client-capabilities (server)
  "What the Eglot LSP client supports for SERVER."
  (:method (s)
           (list
            :workspace (list
                        :applyEdit t
                        :executeCommand `(:dynamicRegistration :json-false)
                        :workspaceEdit `(:documentChanges t)
                        :didChangeWatchedFiles
                        `(:dynamicRegistration
                          ,(if (eglot--trampish-p s) :json-false t))
                        :symbol `(:dynamicRegistration :json-false)
                        :configuration t
                        :workspaceFolders t)
            :textDocument
            (list
             :synchronization (list
                               :dynamicRegistration :json-false
                               :willSave t :willSaveWaitUntil t :didSave t)
             :completion      (list :dynamicRegistration :json-false
                                    :completionItem
                                    `(:snippetSupport
                                      ,(if (and
                                            (not (eglot--stay-out-of-p 'yasnippet))
                                            (eglot--snippet-expansion-fn))
                                           t
                                         :json-false)
                                      :deprecatedSupport t
                                      :resolveSupport (:properties
                                                       ["documentation"
                                                        "details"
                                                        "additionalTextEdits"])
                                      :tagSupport (:valueSet [1]))
                                    :contextSupport t)
             :hover              (list :dynamicRegistration :json-false
                                       :contentFormat (eglot--accepted-formats))
             :signatureHelp      (list :dynamicRegistration :json-false
                                       :signatureInformation
                                       `(:parameterInformation
                                         (:labelOffsetSupport t)
                                         :documentationFormat ,(eglot--accepted-formats)
                                         :activeParameterSupport t))
             :references         `(:dynamicRegistration :json-false)
             :definition         (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :declaration        (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :implementation     (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :typeDefinition     (list :dynamicRegistration :json-false
                                       :linkSupport t)
             :documentSymbol     (list
                                  :dynamicRegistration :json-false
                                  :hierarchicalDocumentSymbolSupport t
                                  :symbolKind `(:valueSet
                                                [,@(mapcar
                                                    #'car eglot--symbol-kind-names)]))
             :documentHighlight  `(:dynamicRegistration :json-false)
             :codeAction         (list
                                  :dynamicRegistration :json-false
                                  :resolveSupport `(:properties ["edit" "command"])
                                  :dataSupport t
                                  :codeActionLiteralSupport
                                  '(:codeActionKind
                                    (:valueSet
                                     ["quickfix"
                                      "refactor" "refactor.extract"
                                      "refactor.inline" "refactor.rewrite"
                                      "source" "source.organizeImports"]))
                                  :isPreferredSupport t)
             :formatting         `(:dynamicRegistration :json-false)
             :rangeFormatting    `(:dynamicRegistration :json-false)
             :rename             `(:dynamicRegistration :json-false)
             :inlayHint          `(:dynamicRegistration :json-false)
             :publishDiagnostics (list :relatedInformation :json-false
                                       ;; TODO: We can support :codeDescription after
                                       ;; adding an appropriate UI to
                                       ;; Flymake.
                                       :codeDescriptionSupport :json-false
                                       :tagSupport
                                       `(:valueSet
                                         [,@(mapcar
                                             #'car eglot--tag-faces)])))
            :window `(:showDocument (:support t)
                      :workDoneProgress t)
            :general (list :positionEncodings ["utf-32" "utf-8" "utf-16"])
            :experimental eglot--{})))

(cl-defgeneric eglot-workspace-folders (server)
  "Return workspaceFolders for SERVER."
  (let ((project (eglot--project server)))
    (vconcat
     (mapcar (lambda (dir)
               (list :uri (eglot-path-to-uri dir)
                     :name (abbreviate-file-name dir)))
             `(,(project-root project) ,@(project-external-roots project))))))

(defclass eglot-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor eglot--project-nickname
    :reader eglot-project-nickname)
   (languages
    :initform nil
    :documentation "Alist ((MODE . LANGUAGE-ID-STRING)...) of managed languages."
    :accessor eglot--languages)
   (capabilities
    :initform nil
    :documentation "JSON object containing server capabilities."
    :accessor eglot--capabilities)
   (server-info
    :initform nil
    :documentation "JSON object containing server info."
    :accessor eglot--server-info)
   (shutdown-requested
    :initform nil
    :documentation "Flag set when server is shutting down."
    :accessor eglot--shutdown-requested)
   (project
    :initform nil
    :documentation "Project associated with server."
    :accessor eglot--project)
   (progress-reporters
    :initform (make-hash-table :test #'equal) :accessor eglot--progress-reporters
    :documentation "Maps LSP progress tokens to progress reporters.")
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor eglot--inhibit-autoreconnect)
   (file-watches
    :documentation "Map (DIR -> (WATCH ID1 ID2...)) for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor eglot--file-watches)
   (managed-buffers
    :initform nil
    :documentation "List of buffers managed by server."
    :accessor eglot--managed-buffers)
   (saved-initargs
    :documentation "Saved initargs for reconnection purposes."
    :accessor eglot--saved-initargs))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")

(declare-function w32-long-file-name "w32proc.c" (fn))
(defun eglot-uri-to-path (uri)
  "Convert URI to file path, helped by `eglot--current-server'."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let* ((server (eglot-current-server))
         (remote-prefix (and server (eglot--trampish-p server)))
         (url (url-generic-parse-url uri)))
    ;; Only parse file:// URIs, leave other URI untouched as
    ;; `file-name-handler-alist' should know how to handle them
    ;; (bug#58790).
    (if (string= "file" (url-type url))
        (let* ((retval (url-unhex-string (url-filename url)))
               ;; Remove the leading "/" for local MS Windows-style paths.
               (normalized (if (and (not remote-prefix)
                                    (eq system-type 'windows-nt)
                                    (cl-plusp (length retval)))
                               (w32-long-file-name (substring retval 1))
                             retval)))
          (concat remote-prefix normalized))
      uri)))

(defun eglot-path-to-uri (path)
  "Convert PATH, a file name, to LSP URI string and return it."
  (let ((truepath (file-truename path)))
    (if (and (url-type (url-generic-parse-url path))
             ;; It might be MS Windows path which includes a drive
             ;; letter that looks like a URL scheme (bug#59338)
             (not (and (eq system-type 'windows-nt)
                       (file-name-absolute-p truepath))))
        ;; Path is already a URI, so forward it to the LSP server
        ;; untouched.  The server should be able to handle it, since
        ;; it provided this URI to clients in the first place.
        path
      (concat "file://"
              ;; Add a leading "/" for local MS Windows-style paths.
              (if (and (eq system-type 'windows-nt)
                       (not (file-remote-p truepath)))
                  "/")
              (url-hexify-string
               ;; Again watch out for trampy paths.
               (directory-file-name (file-local-name truepath))
               eglot--uri-path-allowed-chars)))))

(defun eglot-range-region (range &optional markers)
  "Return a cons (BEG . END) of positions representing LSP RANGE.
If optional MARKERS, make markers instead."
  (let* ((st (plist-get range :start))
         (beg (eglot--lsp-position-to-point st markers))
         (end (eglot--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

(defun eglot-server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (unless (cl-some (lambda (feat)
                     (memq feat eglot-ignored-server-capabilities))
                   feats)
    (cl-loop for caps = (eglot--capabilities (eglot--current-server-or-lose))
             then (cadr probe)
             for (feat . more) on feats
             for probe = (plist-member caps feat)
             if (not probe) do (cl-return nil)
             if (eq (cadr probe) :json-false) do (cl-return nil)
             if (not (listp (cadr probe))) do (cl-return (if more nil (cadr probe)))
             finally (cl-return (or (cadr probe) t)))))

(defun eglot-server-capable-or-lose (&rest feats)
  "Like `eglot-server-capable', but maybe error out."
  (let ((retval (apply #'eglot-server-capable feats)))
    (unless retval
      (eglot--error "Unsupported or ignored LSP capability `%s'"
                    (mapconcat #'symbol-name feats " ")))
    retval))


;;; Process/server management
(defun eglot--major-modes (s) "Major modes server S is responsible for."
  (mapcar #'car (eglot--languages s)))

(defun eglot--language-ids (s) "LSP Language ID strings for server S's modes."
  (mapcar #'cdr (eglot--languages s)))

(cl-defmethod initialize-instance :before ((_server eglot-lsp-server) &optional args)
  (cl-remf args :initializationOptions))

(defvar eglot--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defun eglot-shutdown (server &optional _interactive timeout preserve-buffers)
  "Politely ask SERVER to quit.
Interactively, read SERVER from the minibuffer unless there is
only one and it's managing the current buffer.

Forcefully quit it if it doesn't respond within TIMEOUT seconds.
TIMEOUT defaults to 1.5 seconds.  Don't leave this function with
the server still running.

If PRESERVE-BUFFERS is non-nil (interactively, when called with a
prefix argument), do not kill events and output buffers of
SERVER."
  (interactive (list (eglot--read-server "Shutdown which server"
                                         (eglot-current-server))
                     t nil current-prefix-arg))
  (eglot--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (progn
        (setf (eglot--shutdown-requested server) t)
        (eglot--request server :shutdown nil :timeout (or timeout 1.5))
        (jsonrpc-notify server :exit nil))
    ;; Now ask jsonrpc.el to shut down the server.
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers (kill-buffer (jsonrpc-events-buffer server)))))

(defun eglot-shutdown-all (&optional preserve-buffers)
  "Politely ask all language servers to quit, in order.
PRESERVE-BUFFERS as in `eglot-shutdown', which see."
  (interactive (list current-prefix-arg))
  (cl-loop for ss being the hash-values of eglot--servers-by-project
           do (with-demoted-errors "[eglot] shutdown all: %s"
                (cl-loop for s in ss do (eglot-shutdown s nil nil preserve-buffers)))))

(defvar eglot--servers-by-xrefed-file (make-hash-table :test 'equal))

(defun eglot--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `eglot--managed-mode' where appropriate.
  (dolist (buffer (eglot--managed-buffers server))
    (let (;; Avoid duplicate shutdowns (github#389)
          (eglot-autoshutdown nil))
      (eglot--when-live-buffer buffer (eglot--managed-mode-off))))
  ;; Kill any expensive watches
  (maphash (lambda (_dir watch-and-ids)
             (file-notify-rm-watch (car watch-and-ids)))
           (eglot--file-watches server))
  ;; Sever the project/server relationship for `server'
  (setf (gethash (eglot--project server) eglot--servers-by-project)
        (delq server
              (gethash (eglot--project server) eglot--servers-by-project)))
  (maphash (lambda (f s)
             (when (eq s server) (remhash f eglot--servers-by-xrefed-file)))
           eglot--servers-by-xrefed-file)
  (cond ((eglot--shutdown-requested server)
         t)
        ((not (eglot--inhibit-autoreconnect server))
         (eglot--warn "Reconnecting after unexpected server exit.")
         (eglot-reconnect server))
        ((timerp (eglot--inhibit-autoreconnect server))
         (eglot--warn "Not auto-reconnecting, last one didn't last long."))))

(defun eglot--all-major-modes ()
  "Return all known major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defvar eglot-command-history nil
  "History of CONTACT arguments to `eglot'.")

(defun eglot--lookup-mode (mode)
  "Lookup `eglot-server-programs' for MODE.
Return (LANGUAGES . CONTACT-PROXY).

MANAGED-MODES is a list with MODE as its first element.
Subsequent elements are other major modes also potentially
managed by the server that is to manage MODE.

LANGUAGE-IDS is a list of the same length as MANAGED-MODES.  Each
elem is derived from the corresponding mode name, if not
specified in `eglot-server-programs' (which see).

CONTACT-PROXY is the value of the corresponding
`eglot-server-programs' entry."
  (cl-flet ((languages (main-mode-sym specs)
              (let* ((res
                      (mapcar (jsonrpc-lambda (sym &key language-id &allow-other-keys)
                                (cons sym
                                      (or language-id
                                          (or (get sym 'eglot-language-id)
                                              (replace-regexp-in-string
                                               "\\(?:-ts\\)?-mode$" ""
                                               (symbol-name sym))))))
                              specs))
                     (head (cl-find main-mode-sym res :key #'car)))
                (cons head (delq head res)))))
    (cl-loop
     for (modes . contact) in eglot-server-programs
     for specs = (mapcar #'eglot--ensure-list
                         (if (or (symbolp modes) (keywordp (cadr modes)))
                             (list modes) modes))
     thereis (cl-some (lambda (spec)
                        (cl-destructuring-bind (sym &key &allow-other-keys) spec
                          (and (provided-mode-derived-p mode sym)
                               (cons (languages sym specs) contact))))
                      specs))))

(defun eglot--guess-contact (&optional interactive)
  "Helper for `eglot'.
Return (MANAGED-MODES PROJECT CLASS CONTACT LANG-IDS).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((guessed-mode (if buffer-file-name major-mode))
         (guessed-mode-name (and guessed-mode (symbol-name guessed-mode)))
         (main-mode
          (cond
           ((and interactive
                 (or (>= (prefix-numeric-value current-prefix-arg) 16)
                     (not guessed-mode)))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              guessed-mode-name nil guessed-mode-name nil)))
           ((not guessed-mode)
            (eglot--error "Can't guess mode to manage for `%s'" (current-buffer)))
           (t guessed-mode)))
         (languages-and-contact (eglot--lookup-mode main-mode))
         (managed-modes (mapcar #'car (car languages-and-contact)))
         (language-ids (mapcar #'cdr (car languages-and-contact)))
         (guess (cdr languages-and-contact))
         (guess (if (functionp guess)
                    (funcall guess interactive)
                  guess))
         (class (or (and (consp guess) (symbolp (car guess))
                         (prog1 (unless current-prefix-arg (car guess))
                           (setq guess (cdr guess))))
                    'eglot-lsp-server))
         (program (and (listp guess)
                       (stringp (car guess))
                       ;; A second element might be the port of a (host, port)
                       ;; pair, but in that case it is not a string.
                       (or (null (cdr guess)) (stringp (cadr guess)))
                       (car guess)))
         (base-prompt
          (and interactive
               "Enter program to execute (or <host>:<port>): "))
         (full-program-invocation
          (and program
               (cl-every #'stringp guess)
               (combine-and-quote-strings guess)))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[eglot] Couldn't guess LSP server for `%s'\n%s"
                              main-mode base-prompt))
                     ((and program
                           (not (file-name-absolute-p program))
                           (not (eglot--executable-find program t)))
                      (if full-program-invocation
                          (concat (format "[eglot] I guess you want to run `%s'"
                                          full-program-invocation)
                                  (format ", but I can't find `%s' in PATH!"
                                          program)
                                  "\n" base-prompt)
                        (eglot--error
                         (concat "`%s' not found in PATH, but can't form"
                                 " an interactive prompt for help you fix"
                                 " this.")
                         program guess))))))
         (input (and prompt (read-shell-command prompt
                                                full-program-invocation
                                                'eglot-command-history)))
         (contact
          (if input
              (if (string-match
                   "^[\s\t]*\\(.*\\):\\([[:digit:]]+\\)[\s\t]*$" input)
                  ;; <host>:<port> special case (bug#67682)
                  (list (match-string 1 input)
                        (string-to-number (match-string 2 input)))
                (split-string-and-unquote input))
            guess)))
    (list managed-modes (eglot--current-project) class contact language-ids)))

(defvar eglot-lsp-context nil
  "Dynamically non-nil when searching for projects in LSP context.")

(defun eglot--current-project ()
  "Return a project object for Eglot's LSP purposes.
This relies on `project-current' and thus on
`project-find-functions'.  Functions in the latter
variable (which see) can query the value `eglot-lsp-context' to
decide whether a given directory is a project containing a
suitable root directory for a given LSP server's purposes."
  (let ((eglot-lsp-context t))
    (or (project-current)
        `(transient . ,(expand-file-name default-directory)))))

;;;###autoload
(defun eglot (managed-major-modes project class contact language-ids
                                 &optional _interactive)
  "Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.

This starts a Language Server Protocol (LSP) server suitable for
the buffers of PROJECT whose `major-mode' is among
MANAGED-MAJOR-MODES.  CLASS is the class of the LSP server to
start and CONTACT specifies how to connect to the server.

Interactively, the command attempts to guess MANAGED-MAJOR-MODES,
CLASS, CONTACT, and LANGUAGE-IDS from `eglot-server-programs',
according to the current buffer's `major-mode'.  PROJECT is
guessed from `project-find-functions'.  The search for active
projects in this context binds `eglot-lsp-context' (which see).

If it can't guess, it prompts the user for the mode and the
server.  With a single \\[universal-argument] prefix arg, it
always prompts for COMMAND.  With two \\[universal-argument], it
also always prompts for MANAGED-MAJOR-MODE.

The LSP server of CLASS is started (or contacted) via CONTACT.
If this operation is successful, current *and future* file
buffers of MANAGED-MAJOR-MODE inside PROJECT become \"managed\"
by the LSP server, meaning the information about their contents is
exchanged periodically with the server to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', and `completion-at-point', among others.

PROJECT is a project object as returned by `project-current'.

CLASS is a subclass of `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

LANGUAGE-IDS is a list of language ID string to send to the
server for each element in MANAGED-MAJOR-MODES.

INTERACTIVE is ignored and provided for backward compatibility."
  (interactive
   (let ((current-server (eglot-current-server)))
     (unless (or (null current-server)
                 (y-or-n-p "\
[eglot] Shut down current connection before attempting new one?"))
       (user-error "[eglot] Connection attempt aborted by user."))
     (prog1 (append (eglot--guess-contact t) '(t))
       (when current-server (ignore-errors (eglot-shutdown current-server))))))
  (eglot--connect (eglot--ensure-list managed-major-modes)
                  project class contact
                  (eglot--ensure-list language-ids)))

(defun eglot-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-server-or-lose) t))
  (when (jsonrpc-running-p server)
    (ignore-errors (eglot-shutdown server interactive nil 'preserve-buffers)))
  (eglot--connect (eglot--major-modes server)
                  (eglot--project server)
                  (eieio-object-class-name server)
                  (eglot--saved-initargs server)
                  (eglot--language-ids server))
  (eglot--message "Reconnected!"))

(defvar eglot--managed-mode) ; forward decl

;;;###autoload
(defun eglot-ensure ()
  "Start Eglot session for current buffer if there isn't one.

Only use this function (in major mode hooks, etc) if you are
confident that Eglot can be started safely and efficiently for
*every* buffer visited where these hooks may execute.

Since it is difficult to establish this confidence fully, it's
often wise to use the interactive command `eglot' instead.  This
command only needs to be invoked once per project, as all other
files of a given major mode visited within the same project will
automatically become managed with no further user intervention
needed."
  (let ((buffer (current-buffer)))
    (cl-labels
        ((maybe-connect
           ()
           (eglot--when-live-buffer buffer
             (remove-hook 'post-command-hook #'maybe-connect t)
             (unless eglot--managed-mode
               (condition-case-unless-debug oops
                   (apply #'eglot--connect (eglot--guess-contact))
                 (error (eglot--warn (error-message-string oops))))))))
      (when buffer-file-name
        (add-hook 'post-command-hook #'maybe-connect 'append t)))))

(defun eglot-events-buffer (server)
  "Display events buffer for SERVER.
Use current server's or first available Eglot events buffer."
  (interactive (list (eglot-current-server)))
  (let ((buffer (if server (jsonrpc-events-buffer server)
                  (cl-find "\\*EGLOT.*events\\*"
                           (buffer-list)
                           :key #'buffer-name :test #'string-match))))
    (if buffer (display-buffer buffer)
      (eglot--error "Can't find an Eglot events buffer!"))))

(defun eglot-stderr-buffer (server)
  "Display stderr buffer for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (display-buffer (jsonrpc-stderr-buffer server)))

(defun eglot-forget-pending-continuations (server)
  "Forget pending requests for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (jsonrpc-forget-pending-continuations server))

(defvar eglot-connect-hook
  '(eglot-signal-didChangeConfiguration)
  "Hook run after connecting in `eglot--connect'.")

(defvar eglot-server-initialized-hook
  '()
  "Hook run after a `eglot-lsp-server' instance is created.

That is before a connection was established.  Use
`eglot-connect-hook' to hook into when a connection was
successfully established and the server on the other side has
received the initializing configuration.

Each function is passed the server as an argument")

(defun eglot--cmd (contact)
  "Helper for `eglot--connect'."
  (if (file-remote-p default-directory)
      ;; TODO: this seems like a bug, although it’s everywhere. For
      ;; some reason, for remote connections only, over a pipe, we
      ;; need to turn off line buffering on the tty.
      ;;
      ;; Not only does this seem like there should be a better way,
      ;; but it almost certainly doesn’t work on non-unix systems.
      (list shell-file-name "-c"
            (string-join (cons "stty raw > /dev/null;"
                               (mapcar #'shell-quote-argument contact))
                         " "))
    contact))

(defvar-local eglot--cached-server nil
  "A cached reference to the current Eglot server.")

(defun eglot--connect (managed-modes project class contact language-ids)
  "Connect to MANAGED-MODES, LANGUAGE-IDS, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (project-root project))
         (nickname (project-name project))
         (readable-name (format "EGLOT (%s/%s)" nickname managed-modes))
         server-info
         (contact (if (functionp contact) (funcall contact) contact))
         (initargs
          (cond ((keywordp (car contact)) contact)
                ((integerp (cadr contact))
                 (setq server-info (list (format "%s:%s" (car contact)
                                                 (cadr contact))))
                 `(:process ,(lambda ()
                               (apply #'open-network-stream
                                      readable-name nil
                                      (car contact) (cadr contact)
                                      (cddr contact)))))
                ((and (stringp (car contact))
                      (cl-find-if (lambda (x)
                                    (or (eq x :autoport)
                                        (eq (car-safe x) :autoport)))
                                  contact))
                 (setq server-info (list "<inferior process>"))
                 `(:process ,(jsonrpc-autoport-bootstrap
                                             readable-name
                                             contact
                                             :connect-args '(:noquery t))))
                ((stringp (car contact))
                 (let* ((probe (cl-position-if #'keywordp contact))
                        (more-initargs (and probe (cl-subseq contact probe)))
                        (contact (cl-subseq contact 0 probe)))
                   `(:process
                     ,(lambda ()
                        (let ((default-directory default-directory)
                              ;; bug#61350: Tramp turns on a feature
                              ;; by default that can't (yet) handle
                              ;; very much data so we turn it off
                              ;; unconditionally -- just for our
                              ;; process.
                              (tramp-use-ssh-controlmaster-options 'suppress)
                              (tramp-ssh-controlmaster-options
                               "-o ControlMaster=no -o ControlPath=none"))
                          (make-process
                           :name readable-name
                           :command (setq server-info (eglot--cmd contact))
                           :connection-type 'pipe
                           :coding 'utf-8-emacs-unix
                           :noquery t
                           :stderr (get-buffer-create
                                    (format "*%s stderr*" readable-name))
                           :file-handler t)))
                     ,@more-initargs)))))
         (spread (lambda (fn) (lambda (server method params)
                                (let ((eglot--cached-server server))
                                  (apply fn server method (append params nil))))))
         (server
          (apply
           #'make-instance class
           :name readable-name
           :events-buffer-config eglot-events-buffer-config
           :notification-dispatcher (funcall spread #'eglot-handle-notification)
           :request-dispatcher (funcall spread #'eglot-handle-request)
           :on-shutdown #'eglot--on-shutdown
           initargs))
         (canceled nil)
         (tag (make-symbol "connected-catch-tag")))
    (when server-info
      (jsonrpc--debug server "Running language server: %s"
                      (string-join server-info " ")))
    (setf (eglot--saved-initargs server) initargs)
    (setf (eglot--project server) project)
    (setf (eglot--project-nickname server) nickname)
    (setf (eglot--languages server)
          (cl-loop for m in managed-modes for l in language-ids
                   collect (cons m l)))
    (run-hook-with-args 'eglot-server-initialized-hook server)
    ;; Now start the handshake.  To honor `eglot-sync-connect'
    ;; maybe-sync-maybe-async semantics we use `jsonrpc-async-request'
    ;; and mimic most of `jsonrpc-request'.
    (unwind-protect
        (condition-case _quit
            (let ((retval
                   (catch tag
                     (jsonrpc-async-request
                      server
                      :initialize
                      (list :processId
                            (unless (or eglot-withhold-process-id
                                        (file-remote-p default-directory)
                                        (eq (jsonrpc-process-type server)
                                            'network))
                              (emacs-pid))
                            :clientInfo
                            `(:name "Eglot" ,@(when eglot--version
                                                `(:version ,eglot--version)))
                            ;; Maybe turn trampy `/ssh:foo@bar:/path/to/baz.py'
                            ;; into `/path/to/baz.py', so LSP groks it.
                            :rootPath (file-local-name
                                       (expand-file-name default-directory))
                            :rootUri (eglot-path-to-uri default-directory)
                            :initializationOptions (eglot-initialization-options
                                                    server)
                            :capabilities (eglot-client-capabilities server)
                            :workspaceFolders (eglot-workspace-folders server))
                      :success-fn
                      (eglot--lambda ((InitializeResult) capabilities serverInfo)
                        (unless canceled
                          (push server
                                (gethash project eglot--servers-by-project))
                          (setf (eglot--capabilities server) capabilities)
                          (setf (eglot--server-info server) serverInfo)
                          (jsonrpc-notify server :initialized eglot--{})
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              ;; No need to pass SERVER as an argument: it has
                              ;; been registered in `eglot--servers-by-project',
                              ;; so that it can be found (and cached) from
                              ;; `eglot--maybe-activate-editing-mode' in any
                              ;; managed buffer.
                              (eglot--maybe-activate-editing-mode)))
                          (setf (eglot--inhibit-autoreconnect server)
                                (cond
                                 ((booleanp eglot-autoreconnect)
                                  (not eglot-autoreconnect))
                                 ((cl-plusp eglot-autoreconnect)
                                  (run-with-timer
                                   eglot-autoreconnect nil
                                   (lambda ()
                                     (setf (eglot--inhibit-autoreconnect server)
                                           (null eglot-autoreconnect)))))))
                          (run-hook-with-args 'eglot-connect-hook server)
                          (eglot--message
                           "Connected! Server `%s' now managing `%s' buffers \
in project `%s'."
                           (or (plist-get serverInfo :name)
                               (jsonrpc-name server))
                           managed-modes
                           (eglot-project-nickname server))
                          (when tag (throw tag t))))
                      :timeout eglot-connect-timeout
                      :error-fn (eglot--lambda ((ResponseError) code message)
                                  (unless canceled
                                    (jsonrpc-shutdown server)
                                    (let ((msg (format "%s: %s" code message)))
                                      (if tag (throw tag `(error . ,msg))
                                        (eglot--error msg)))))
                      :timeout-fn (lambda ()
                                    (unless canceled
                                      (jsonrpc-shutdown server)
                                      (let ((msg (format "Timed out after %s seconds"
                                                         eglot-connect-timeout)))
                                        (if tag (throw tag `(error . ,msg))
                                          (eglot--error msg))))))
                     (cond ((numberp eglot-sync-connect)
                            (accept-process-output nil eglot-sync-connect))
                           (eglot-sync-connect
                            (while t (accept-process-output
                                      nil eglot-connect-timeout)))))))
              (pcase retval
                (`(error . ,msg) (eglot--error msg))
                (`nil (eglot--message "Waiting in background for server `%s'"
                                      (jsonrpc-name server))
                      nil)
                (_ server)))
          (quit (jsonrpc-shutdown server) (setq canceled 'quit)))
      (setq tag nil))))


;;; Helpers (move these to API?)
;;;
(defun eglot--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[eglot] %s" (apply #'format format args)))

(defun eglot--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[eglot] %s" (apply #'format format args)))

(defun eglot--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'eglot--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'eglot (apply #'format format args) :warning)))

(defalias 'eglot--bol
  (if (fboundp 'pos-bol) #'pos-bol
    (lambda (&optional n) (let ((inhibit-field-text-motion t))
                            (line-beginning-position n))))
  "Return position of first character in current line.")

(cl-defun eglot--request (server method params &key
                                 immediate
                                 timeout cancel-on-input
                                 cancel-on-input-retval)
  "Like `jsonrpc-request', but for Eglot LSP requests.
Unless IMMEDIATE, send pending changes before making request."
  (unless immediate (eglot--signal-textDocument/didChange))
  (jsonrpc-request server method params
                   :timeout timeout
                   :cancel-on-input cancel-on-input
                   :cancel-on-input-retval cancel-on-input-retval))


;;; Encoding fever
;;;
(defvar eglot-current-linepos-function #'eglot-utf-16-linepos
  "Function calculating position relative to line beginning.

It is a function of no arguments considering the text from line
beginning up to current point.  The return value is the number of
UTF code units needed to encode that text from the LSP server's
perspective.  This may be a number of octets, 16-bit words or
Unicode code points, depending on whether the LSP server's
`positionEncoding' capability is UTF-8, UTF-16 or UTF-32,
respectively.  Position of point should remain unaltered if that
return value is fed through the corresponding inverse function
`eglot-move-to-linepos-function' (which see).")

(defun eglot-utf-8-linepos ()
  "Calculate number of UTF-8 bytes from line beginning."
  (length (encode-coding-region (eglot--bol) (point) 'utf-8-unix t)))

(defun eglot-utf-16-linepos (&optional lbp)
  "Calculate number of UTF-16 code units from position given by LBP.
LBP defaults to `eglot--bol'."
  (/ (- (length (encode-coding-region (or lbp (eglot--bol))
                                      ;; Fix github#860
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun eglot-utf-32-linepos ()
  "Calculate number of Unicode codepoints from line beginning."
  (- (point) (eglot--bol)))

(defun eglot--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (eglot--widening
   ;; LSP line is zero-origin; emacs is one-origin.
   (list :line (1- (line-number-at-pos pos t))
         :character (progn (when pos (goto-char pos))
                           (funcall eglot-current-linepos-function)))))

(defvar eglot-move-to-linepos-function #'eglot-move-to-utf-16-linepos
  "Function to move to a position within a line reported by the LSP server.

Per the LSP spec, character offsets in LSP Position objects count
UTF-16 code units, not actual code points.  So when LSP says
position 3 of a line containing just \"aXbc\", where X is a funny
looking character in the UTF-16 \"supplementary plane\", it
actually means `b', not `c'.  The default value
`eglot-move-to-utf-16-linepos' accounts for this.

This variable can also be set to `eglot-move-to-utf-8-linepos' or
`eglot-move-to-utf-32-linepos' for servers not closely following
the spec.  Also, since LSP 3.17 server and client may agree on an
encoding and Eglot will set this variable automatically.")

(defun eglot-move-to-utf-8-linepos (n)
  "Move to line's Nth byte as computed by LSP's UTF-8 criterion."
  (let* ((bol (eglot--bol))
         (goal-byte (+ (position-bytes bol) n))
         (eol (line-end-position)))
    (goto-char bol)
    (while (and (< (position-bytes (point)) goal-byte) (< (point) eol))
      ;; raw bytes take 2 bytes in the buffer
      (when (>= (char-after) #x3fff80) (setq goal-byte (1+ goal-byte)))
      (forward-char 1))))

(defun eglot-move-to-utf-16-linepos (n)
  "Move to line's Nth code unit as computed by LSP's UTF-16 criterion."
  (let* ((bol (eglot--bol))
         (goal-char (+ bol n))
         (eol (line-end-position)))
    (goto-char bol)
    (while (and (< (point) goal-char) (< (point) eol))
      ;; code points in the "supplementary place" use two code units
      (when (<= #x010000 (char-after) #x10ffff) (setq goal-char (1- goal-char)))
      (forward-char 1))))

(defun eglot-move-to-utf-32-linepos (n)
  "Move to line's Nth codepoint as computed by LSP's UTF-32 criterion."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP characters in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (eglot--bol) n) (line-end-position))))

(defun eglot--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get pos-plist :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((col (plist-get pos-plist :character)))
          (unless (wholenump col)
            (eglot--warn
             "Caution: LSP server sent invalid character position %s. Using 0 instead."
             col)
            (setq col 0))
          (funcall eglot-move-to-linepos-function col)))
      (if marker (copy-marker (point-marker)) (point)))))


;;; More helpers
(defun eglot--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (fboundp 'yas-minor-mode)
       (lambda (&rest args)
         (with-no-warnings
           (unless (bound-and-true-p yas-minor-mode) (yas-minor-mode 1))
           (apply #'yas-expand-snippet args)))))

(defun eglot--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list markup 'gfm-view-mode)
                 (list (plist-get markup :value)
                       (pcase (plist-get markup :kind)
                         ("markdown" 'gfm-view-mode)
                         ("plaintext" 'text-mode)
                         (_ major-mode))))))
    (with-temp-buffer
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert string)
      (let ((inhibit-message t)
            (message-log-max nil)
            match)
        (ignore-errors (delay-mode-hooks (funcall mode)))
        (font-lock-ensure)
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          (when (fboundp 'text-property-search-forward) ;; FIXME: use compat
            (while (setq match (text-property-search-forward 'invisible))
              (delete-region (prop-match-beginning match)
                             (prop-match-end match)))))
        (string-trim (buffer-string))))))

(defun eglot--read-server (prompt &optional dont-if-just-the-one)
  "Read a running Eglot server from minibuffer using PROMPT.
If DONT-IF-JUST-THE-ONE and there's only one server, don't prompt
and just return it.  PROMPT shouldn't end with a question mark."
  (let ((servers (cl-loop for servers
                          being hash-values of eglot--servers-by-project
                          append servers))
        (name (lambda (srv)
                (format "%s %s" (eglot-project-nickname srv)
                        (eglot--major-modes srv)))))
    (cond ((null servers)
           (eglot--error "No servers!"))
          ((or (cdr servers) (not dont-if-just-the-one))
           (let* ((default (when-let ((current (eglot-current-server)))
                             (funcall name current)))
                  (read (completing-read
                         (if default
                             (format "%s (default %s)? " prompt default)
                           (concat prompt "? "))
                         (mapcar name servers)
                         nil t
                         nil nil
                         default)))
             (cl-find read servers :key name :test #'equal)))
          (t (car servers)))))

(defun eglot--trampish-p (server)
  "Tell if SERVER's project root is `file-remote-p'."
  (file-remote-p (project-root (eglot--project server))))

(defun eglot--plist-keys (plist) "Get keys of a plist."
  (cl-loop for (k _v) on plist by #'cddr collect k))

(defalias 'eglot--ensure-list
  (if (fboundp 'ensure-list) #'ensure-list
    (lambda (x) (if (listp x) x (list x)))))


;;; Minor modes
;;;
(defvar eglot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap display-local-help] #'eldoc-doc-buffer)
    map))

(defvar-local eglot--current-flymake-report-fn nil
  "Current flymake report function for this buffer.")

(defvar-local eglot--saved-bindings nil
  "Bindings saved by `eglot--setq-saving'.")

(defvar eglot-stay-out-of '()
  "List of Emacs things that Eglot should try to stay of.
Each element is a string, a symbol, or a regexp which is matched
against a variable's name.  Examples include the string
\"company\" or the symbol `xref'.

Before Eglot starts \"managing\" a particular buffer, it
opinionatedly sets some peripheral Emacs facilities, such as
Flymake, Xref and Company.  These overriding settings help ensure
consistent Eglot behavior and only stay in place until
\"managing\" stops (usually via `eglot-shutdown'), whereupon the
previous settings are restored.

However, if you wish for Eglot to stay out of a particular Emacs
facility that you'd like to keep control of add an element to
this list and Eglot will refrain from setting it.

For example, to keep your Company customization, add the symbol
`company' to this variable.")

(defun eglot--stay-out-of-p (symbol)
  "Tell if Eglot should stay out of SYMBOL."
  (cl-find (symbol-name symbol) eglot-stay-out-of
           :test (lambda (s thing)
                   (let ((re (if (symbolp thing) (symbol-name thing) thing)))
                     (string-match re s)))))

(defmacro eglot--setq-saving (symbol binding)
  `(unless (or (not (boundp ',symbol)) (eglot--stay-out-of-p ',symbol))
     (push (cons ',symbol (symbol-value ',symbol)) eglot--saved-bindings)
     (setq-local ,symbol ,binding)))

(defun eglot-managed-p ()
  "Tell if current buffer is managed by Eglot."
  eglot--managed-mode)

(defvar eglot-managed-mode-hook nil
  "A hook run by Eglot after it started/stopped managing a buffer.
Use `eglot-managed-p' to determine if current buffer is managed.")

(define-minor-mode eglot--managed-mode
  "Mode for source buffers managed by some Eglot project."
  :init-value nil :lighter nil :keymap eglot-mode-map
  (cond
   (eglot--managed-mode
    (pcase (plist-get (eglot--capabilities (eglot-current-server))
                      :positionEncoding)
      ("utf-32"
       (eglot--setq-saving eglot-current-linepos-function #'eglot-utf-32-linepos)
       (eglot--setq-saving eglot-move-to-linepos-function #'eglot-move-to-utf-32-linepos))
      ("utf-8"
       (eglot--setq-saving eglot-current-linepos-function #'eglot-utf-8-linepos)
       (eglot--setq-saving eglot-move-to-linepos-function #'eglot-move-to-utf-8-linepos)))
    (add-hook 'after-change-functions 'eglot--after-change nil t)
    (add-hook 'before-change-functions 'eglot--before-change nil t)
    (add-hook 'kill-buffer-hook #'eglot--managed-mode-off nil t)
    ;; Prepend "didClose" to the hook after the "nonoff", so it will run first
    (add-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'before-revert-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'after-revert-hook 'eglot--after-revert-hook nil t)
    (add-hook 'before-save-hook 'eglot--signal-textDocument/willSave nil t)
    (add-hook 'after-save-hook 'eglot--signal-textDocument/didSave nil t)
    (unless (eglot--stay-out-of-p 'xref)
      (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (add-hook 'completion-in-region-mode-hook #'eglot--capf-session-flush nil t)
    (add-hook 'company-after-completion-hook #'eglot--capf-session-flush nil t)
    (add-hook 'change-major-mode-hook #'eglot--managed-mode-off nil t)
    (add-hook 'post-self-insert-hook 'eglot--post-self-insert-hook nil t)
    (add-hook 'pre-command-hook 'eglot--pre-command-hook nil t)
    (eglot--setq-saving xref-prompt-for-identifier nil)
    (eglot--setq-saving flymake-diagnostic-functions '(eglot-flymake-backend))
    (eglot--setq-saving company-backends '(company-capf))
    (eglot--setq-saving company-tooltip-align-annotations t)
    (eglot--setq-saving eldoc-documentation-strategy
                        #'eldoc-documentation-compose)
    (unless (eglot--stay-out-of-p 'imenu)
      (add-function :before-until (local 'imenu-create-index-function)
                    #'eglot-imenu))
    (unless (eglot--stay-out-of-p 'flymake) (flymake-mode 1))
    (unless (eglot--stay-out-of-p 'eldoc)
      (add-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function
                nil t)
      (add-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function
                nil t)
      (eldoc-mode 1))
    (cl-pushnew (current-buffer) (eglot--managed-buffers (eglot-current-server))))
   (t
    (remove-hook 'after-change-functions 'eglot--after-change t)
    (remove-hook 'before-change-functions 'eglot--before-change t)
    (remove-hook 'kill-buffer-hook #'eglot--managed-mode-off t)
    (remove-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-revert-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'after-revert-hook 'eglot--after-revert-hook t)
    (remove-hook 'before-save-hook 'eglot--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook 'eglot--signal-textDocument/didSave t)
    (remove-hook 'xref-backend-functions 'eglot-xref-backend t)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (remove-hook 'completion-in-region-mode-hook #'eglot--capf-session-flush t)
    (remove-hook 'company-after-completion-hook #'eglot--capf-session-flush t)
    (remove-hook 'change-major-mode-hook #'eglot--managed-mode-off t)
    (remove-hook 'post-self-insert-hook 'eglot--post-self-insert-hook t)
    (remove-hook 'pre-command-hook 'eglot--pre-command-hook t)
    (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
    (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function t)
    (cl-loop for (var . saved-binding) in eglot--saved-bindings
             do (set (make-local-variable var) saved-binding))
    (remove-function (local 'imenu-create-index-function) #'eglot-imenu)
    (when eglot--current-flymake-report-fn
      (eglot--report-to-flymake nil)
      (setq eglot--current-flymake-report-fn nil))
    (let ((server eglot--cached-server))
      (setq eglot--cached-server nil)
      (when server
        (setf (eglot--managed-buffers server)
              (delq (current-buffer) (eglot--managed-buffers server)))
        (when (and eglot-autoshutdown
                   (null (eglot--managed-buffers server)))
          (eglot-shutdown server)))))))

(defun eglot--managed-mode-off ()
  "Turn off `eglot--managed-mode' unconditionally."
  (remove-overlays nil nil 'eglot--overlay t)
  (eglot-inlay-hints-mode -1)
  (eglot--managed-mode -1))

(defun eglot-current-server ()
  "Return logical Eglot server for current buffer, nil if none."
  (setq eglot--cached-server
        (or eglot--cached-server
            (and (not (eq major-mode 'fundamental-mode)) ; gh#1330
                 (or
                  (cl-find-if #'eglot--languageId
                              (gethash (eglot--current-project)
                                       eglot--servers-by-project))
                  (and eglot-extend-to-xref
                       buffer-file-name
                       (gethash (expand-file-name buffer-file-name)
                                eglot--servers-by-xrefed-file)))))))

(defun eglot--current-server-or-lose ()
  "Return current logical Eglot server connection or error."
  (or (eglot-current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(defvar-local eglot--diagnostics nil
  "Flymake diagnostics for this buffer.")

(defvar revert-buffer-preserve-modes)
(defun eglot--after-revert-hook ()
  "Eglot's `after-revert-hook'."
  (when revert-buffer-preserve-modes (eglot--signal-textDocument/didOpen)))

(defun eglot--maybe-activate-editing-mode ()
  "Maybe activate `eglot--managed-mode'.

If it is activated, also signal textDocument/didOpen."
  (unless eglot--managed-mode
    ;; Called when `revert-buffer-in-progress-p' is t but
    ;; `revert-buffer-preserve-modes' is nil.
    (when (and buffer-file-name (eglot-current-server))
      (setq eglot--diagnostics nil)
      (eglot--managed-mode)
      (eglot--signal-textDocument/didOpen)
      ;; Run user hook after 'textDocument/didOpen' so server knows
      ;; about the buffer.
      (eglot-inlay-hints-mode 1)
      (run-hooks 'eglot-managed-mode-hook))))

(add-hook 'after-change-major-mode-hook 'eglot--maybe-activate-editing-mode)

(defun eglot-clear-status (server)
  "Clear the last JSONRPC error for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (setf (jsonrpc-last-error server) nil))


;;; Mode-line, menu and other sugar
;;;
(defvar eglot--mode-line-format `(:eval (eglot--mode-line-format)))

(put 'eglot--mode-line-format 'risky-local-variable t)

(defun eglot--mouse-call (what &optional update-mode-line)
  "Make an interactive lambda for calling WHAT with the mouse."
  (lambda (event)
    (interactive "e")
    (let ((start (event-start event))) (with-selected-window (posn-window start)
                                         (save-excursion
                                           (goto-char (or (posn-point start)
                                                          (point)))
                                           (call-interactively what)
                                           (when update-mode-line
                                             (force-mode-line-update t)))))))

(defun eglot-manual () "Read Eglot's manual."
       (declare (obsolete info "1.10"))
       (interactive) (info "(eglot)"))

;;;###autoload
(defun eglot-upgrade-eglot (&rest _) "Update Eglot to latest version."
  (interactive)
  (with-no-warnings
    (require 'package)
    (unless package-archive-contents (package-refresh-contents))
    (when-let ((existing (cadr (assoc 'eglot package-alist))))
      (package-delete existing t))
    (package-install (cadr (assoc 'eglot package-archive-contents)))))

(easy-menu-define eglot-menu nil "Eglot"
  `("Eglot"
    ;; Commands for getting information and customization.
    ["Customize Eglot" (lambda () (interactive) (customize-group "eglot"))]
    "--"
    ;; xref like commands.
    ["Find definitions" xref-find-definitions
     :help "Find definitions of identifier at point"
     :active (eglot-server-capable :definitionProvider)]
    ["Find references" xref-find-references
     :help "Find references to identifier at point"
     :active (eglot-server-capable :referencesProvider)]
    ["Find symbols in workspace (apropos)" xref-find-apropos
     :help "Find symbols matching a query"
     :active (eglot-server-capable :workspaceSymbolProvider)]
    ["Find declaration" eglot-find-declaration
     :help "Find declaration for identifier at point"
     :active (eglot-server-capable :declarationProvider)]
    ["Find implementation" eglot-find-implementation
     :help "Find implementation for identifier at point"
     :active (eglot-server-capable :implementationProvider)]
    ["Find type definition" eglot-find-typeDefinition
     :help "Find type definition for identifier at point"
     :active (eglot-server-capable :typeDefinitionProvider)]
    "--"
    ;; LSP-related commands (mostly Eglot's own commands).
    ["Rename symbol" eglot-rename
     :active (eglot-server-capable :renameProvider)]
    ["Format buffer" eglot-format-buffer
     :active (eglot-server-capable :documentFormattingProvider)]
    ["Format active region" eglot-format
     :active (and (region-active-p)
                  (eglot-server-capable :documentRangeFormattingProvider))]
    ["Show Flymake diagnostics for buffer" flymake-show-buffer-diagnostics]
    ["Show Flymake diagnostics for project" flymake-show-project-diagnostics]
    ["Show Eldoc documentation at point" eldoc-doc-buffer]
    "--"
    ["All possible code actions" eglot-code-actions
     :active (eglot-server-capable :codeActionProvider)]
    ["Organize imports" eglot-code-action-organize-imports
     :visible (eglot-server-capable :codeActionProvider)]
    ["Extract" eglot-code-action-extract
     :visible (eglot-server-capable :codeActionProvider)]
    ["Inline" eglot-code-action-inline
     :visible (eglot-server-capable :codeActionProvider)]
    ["Rewrite" eglot-code-action-rewrite
     :visible (eglot-server-capable :codeActionProvider)]
    ["Quickfix" eglot-code-action-quickfix
     :visible (eglot-server-capable :codeActionProvider)]))

(easy-menu-define eglot-server-menu nil "Monitor server communication"
  '("Debugging the server communication"
    ["Reconnect to server" eglot-reconnect]
    ["Quit server" eglot-shutdown]
    "--"
    ["LSP events buffer" eglot-events-buffer]
    ["Server stderr buffer" eglot-stderr-buffer]
    ["Customize event buffer size"
     (lambda ()
       (interactive)
       (customize-variable 'eglot-events-buffer-size))]))

(defun eglot--mode-line-props (thing face defs &optional prepend)
  "Helper for function `eglot--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (eglot--mouse-call def t))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize ,thing
                                         face ,face
                                         keymap ,map help-echo ,(concat prepend blurb)
                                         mouse-face mode-line-highlight))))

(defun eglot--mode-line-format ()
  "Compose Eglot's mode-line."
  (let* ((server (eglot-current-server))
         (nick (and server (eglot-project-nickname server)))
         (pending (and server (jsonrpc-continuation-count server)))
         (last-error (and server (jsonrpc-last-error server))))
    (append
     `(,(propertize
         eglot-menu-string
         'face 'eglot-mode-line
         'mouse-face 'mode-line-highlight
         'help-echo "Eglot: Emacs LSP client\nmouse-1: Display minor mode menu"
         'keymap (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line down-mouse-1] eglot-menu)
                   map)))
     (when nick
       `(":"
         ,(propertize
           nick
           'face 'eglot-mode-line
           'mouse-face 'mode-line-highlight
           'help-echo (format "Project '%s'\nmouse-1: LSP server control menu" nick)
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line down-mouse-1] eglot-server-menu)
                     map))
         ,@(when last-error
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 eglot-clear-status  "Clear this status"))
                     (format "An error occurred: %s\n" (plist-get last-error
                                                                  :message)))))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-3 eglot-forget-pending-continuations
                                "Forget pending continuations"))
                     "Number of outgoing, \
still unanswered LSP requests to the server\n")))
         ,@(cl-loop for pr hash-values of (eglot--progress-reporters server)
                    when (eq (car pr)  'eglot--mode-line-reporter)
                    append `("/" ,(eglot--mode-line-props
                                   (format "%s%%%%" (or (nth 4 pr) "?"))
                                   'eglot-mode-line
                                   nil
                                   (format "(%s) %s %s" (nth 1 pr)
                                           (nth 2 pr) (nth 3 pr))))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))


;;; Flymake customization
;;;
(put 'eglot-note 'flymake-category 'flymake-note)
(put 'eglot-warning 'flymake-category 'flymake-warning)
(put 'eglot-error 'flymake-category 'flymake-error)

(defalias 'eglot--make-diag 'flymake-make-diagnostic)
(defalias 'eglot--diag-data 'flymake-diagnostic-data)

(defvar eglot-diagnostics-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'eglot-code-actions-at-mouse)
    map)
  "Keymap active in Eglot-backed Flymake diagnostic overlays.")

(cl-loop for i from 1
         for type in '(eglot-note eglot-warning eglot-error)
         do (put type 'flymake-overlay-control
                 `((mouse-face . highlight)
                   (priority . ,(+ 50 i))
                   (keymap . ,eglot-diagnostics-map))))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(cl-defmethod eglot-handle-notification
  (_server method &key &allow-other-keys)
  "Handle unknown notification."
  (unless (or (string-prefix-p "$" (format "%s" method))
              (not (memq 'disallow-unknown-methods eglot-strict-mode)))
    (eglot--warn "Server sent unknown notification method `%s'" method)))

(cl-defmethod eglot-handle-request
  (_server method &key &allow-other-keys)
  "Handle unknown request."
  (when (memq 'disallow-unknown-methods eglot-strict-mode)
    (jsonrpc-error "Unknown request method `%s'" method)))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql window/showMessage)) &key type message)
  "Handle notification window/showMessage."
  (eglot--message (propertize "Server reports (type=%s): %s"
                              'face (if (<= type 1) 'error))
                  type message))

(cl-defmethod eglot-handle-request
  (_server (_method (eql window/showMessageRequest))
           &key type message actions &allow-other-keys)
  "Handle server request window/showMessageRequest."
  (let* ((actions (append actions nil)) ;; gh#627
         (label (completing-read
                 (concat
                  (format (propertize "[eglot] Server reports (type=%s): %s"
                                      'face (if (or (not type) (<= type 1)) 'error))
                          type message)
                  "\nChoose an option: ")
                 (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
                     '("OK"))
                 nil t (plist-get (elt actions 0) :title))))
    (if label `(:title ,label) :null)))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql window/logMessage)) &key _type _message)
  "Handle notification window/logMessage.") ;; noop, use events buffer

(cl-defmethod eglot-handle-notification
  (_server (_method (eql telemetry/event)) &rest _any)
  "Handle notification telemetry/event.") ;; noop, use events buffer

(defalias 'eglot--reporter-update
  (if (> emacs-major-version 26) #'progress-reporter-update
    (lambda (a b &optional _c) (progress-reporter-update a b))))

(cl-defmethod eglot-handle-notification
  (server (_method (eql $/progress)) &key token value)
  "Handle $/progress notification identified by TOKEN from SERVER."
  (when eglot-report-progress
    (cl-flet ((fmt (&rest args) (mapconcat #'identity args " "))
              (mkpr (title)
                (if (eq eglot-report-progress 'messages)
                    (make-progress-reporter
                     (format "[eglot] %s %s: %s"
                             (eglot-project-nickname server) token title))
                  (list 'eglot--mode-line-reporter token title)))
              (upd (pcnt msg &optional
                         (pr (gethash token (eglot--progress-reporters server))))
                (cond
                  ((eq (car pr) 'eglot--mode-line-reporter)
                   (setcdr (cddr pr) (list msg pcnt))
                   (force-mode-line-update t))
                  (pr (eglot--reporter-update pr pcnt msg)))))
      (eglot--dbind ((WorkDoneProgress) kind title percentage message) value
        (pcase kind
          ("begin"
           (upd percentage (fmt title message)
                (puthash token (mkpr title)
                         (eglot--progress-reporters server))))
          ("report" (upd percentage message))
          ("end" (upd (or percentage 100) message)
           (run-at-time 2 nil
                        (lambda ()
                          (remhash token (eglot--progress-reporters server))))))))))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics
           &allow-other-keys) ; FIXME: doesn't respect `eglot-strict-mode'
  "Handle notification publishDiagnostics."
  (cl-flet ((eglot--diag-type (sev)
              (cond ((null sev) 'eglot-error)
                    ((<= sev 1) 'eglot-error)
                    ((= sev 2)  'eglot-warning)
                    (t          'eglot-note)))
            (mess (source code message)
              (concat source (and code (format " [%s]" code)) ": " message)))
    (if-let* ((path (expand-file-name (eglot-uri-to-path uri)))
              (buffer (find-buffer-visiting path)))
        (with-current-buffer buffer
          (cl-loop
           initially
           (setq flymake-list-only-diagnostics
                 (assoc-delete-all path flymake-list-only-diagnostics))
           for diag-spec across diagnostics
           collect (eglot--dbind ((Diagnostic) range code message severity source tags)
                       diag-spec
                     (setq message (mess source code message))
                     (pcase-let
                         ((`(,beg . ,end) (eglot-range-region range)))
                       ;; Fallback to `flymake-diag-region' if server
                       ;; botched the range
                       (when (= beg end)
                         (if-let* ((st (plist-get range :start))
                                   (diag-region
                                    (flymake-diag-region
                                     (current-buffer) (1+ (plist-get st :line))
                                     (plist-get st :character))))
                             (setq beg (car diag-region) end (cdr diag-region))
                           (eglot--widening
                            (goto-char (point-min))
                            (setq beg
                                  (eglot--bol
                                   (1+ (plist-get (plist-get range :start) :line))))
                            (setq end
                                  (line-end-position
                                   (1+ (plist-get (plist-get range :end) :line)))))))
                       (eglot--make-diag
                        (current-buffer) beg end
                        (eglot--diag-type severity)
                        message `((eglot-lsp-diag . ,diag-spec))
                        (when-let ((faces
                                    (cl-loop for tag across tags
                                             when (alist-get tag eglot--tag-faces)
                                             collect it)))
                          `((face . ,faces))))))
           into diags
           finally (cond ((and
                           ;; only add to current report if Flymake
                           ;; starts on idle-timer (github#958)
                           (not (null flymake-no-changes-timeout))
                           eglot--current-flymake-report-fn)
                          (eglot--report-to-flymake diags))
                         (t
                          (setq eglot--diagnostics diags)))))
      (cl-loop
       for diag-spec across diagnostics
       collect (eglot--dbind ((Diagnostic) code range message severity source) diag-spec
                 (setq message (mess source code message))
                 (let* ((start (plist-get range :start))
                        (line (1+ (plist-get start :line)))
                        (char (1+ (plist-get start :character))))
                   (eglot--make-diag
                    path (cons line char) nil (eglot--diag-type severity) message)))
       into diags
       finally
       (setq flymake-list-only-diagnostics
             (assoc-delete-all path flymake-list-only-diagnostics))
       (push (cons path diags) flymake-list-only-diagnostics)))))

(cl-defun eglot--register-unregister (server things how)
  "Helper for `registerCapability'.
THINGS are either registrations or unregisterations (sic)."
  (cl-loop
   for thing in (cl-coerce things 'list)
   do (eglot--dbind ((Registration) id method registerOptions) thing
        (apply (cl-ecase how
                 (register 'eglot-register-capability)
                 (unregister 'eglot-unregister-capability))
               server (intern method) id registerOptions))))

(cl-defmethod eglot-handle-request
  (server (_method (eql client/registerCapability)) &key registrations)
  "Handle server request client/registerCapability."
  (eglot--register-unregister server registrations 'register))

(cl-defmethod eglot-handle-request
  (server (_method (eql client/unregisterCapability))
          &key unregisterations) ;; XXX: "unregisterations" (sic)
  "Handle server request client/unregisterCapability."
  (eglot--register-unregister server unregisterations 'unregister))

(cl-defmethod eglot-handle-request
  (_server (_method (eql workspace/applyEdit)) &key _label edit)
  "Handle server request workspace/applyEdit."
  (eglot--apply-workspace-edit edit last-command)
  `(:applied t))

(cl-defmethod eglot-handle-request
  (server (_method (eql workspace/workspaceFolders)))
  "Handle server request workspace/workspaceFolders."
  (eglot-workspace-folders server))

(cl-defmethod eglot-handle-request
  (_server (_method (eql window/showDocument)) &key
           uri external takeFocus selection)
  "Handle request window/showDocument."
  (let ((success t)
        (filename))
    (cond
     ((eq external t) (browse-url uri))
     ((file-readable-p (setq filename (eglot-uri-to-path uri)))
      ;; Use run-with-timer to avoid nested client requests like the
      ;; "synchronous imenu" floated in bug#62116 presumably caused by
      ;; which-func-mode.
      (run-with-timer
       0 nil
       (lambda ()
         (with-current-buffer (find-file-noselect filename)
           (cond (takeFocus
                  (pop-to-buffer (current-buffer))
                  (select-frame-set-input-focus (selected-frame)))
                 ((display-buffer (current-buffer))))
           (when selection
             (pcase-let ((`(,beg . ,end) (eglot-range-region selection)))
               ;; FIXME: it is very naughty to use someone else's `--'
               ;; function, but `xref--goto-char' happens to have
               ;; exactly the semantics we want vis-a-vis widening.
               (xref--goto-char beg)
               (pulse-momentary-highlight-region beg end 'highlight)))))))
     (t (setq success :json-false)))
    `(:success ,success)))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  `(:uri ,(eglot-path-to-uri (or buffer-file-name
                                  (ignore-errors
                                    (buffer-file-name
                                     (buffer-base-buffer)))))))

(defvar-local eglot--versioned-identifier 0)

(defun eglot--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (eglot--TextDocumentIdentifier)
          `(:version ,eglot--versioned-identifier)))

(cl-defun eglot--languageId (&optional (server (eglot--current-server-or-lose)))
  "Compute LSP \\='languageId\\=' string for current buffer.
Doubles as an predicate telling if SERVER can manage current
buffer."
  (cl-loop for (mode . languageid) in
           (eglot--languages server)
           when (provided-mode-derived-p major-mode mode)
           return languageid))

(defun eglot--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (eglot--VersionedTextDocumentIdentifier)
   (list :languageId (eglot--languageId)
         :text
         (eglot--widening
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun eglot--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (list :textDocument (eglot--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position)))

(defvar-local eglot--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defun eglot--post-self-insert-hook ()
  "Set `eglot--last-inserted-char', maybe call on-type-formatting."
  (setq eglot--last-inserted-char last-command-event)
  (let ((ot-provider (eglot-server-capable :documentOnTypeFormattingProvider)))
    (when (and ot-provider
               (ignore-errors ; github#906, some LS's send empty strings
                 (or (eq eglot--last-inserted-char
                         (seq-first (plist-get ot-provider :firstTriggerCharacter)))
                     (cl-find eglot--last-inserted-char
                              (plist-get ot-provider :moreTriggerCharacter)
                              :key #'seq-first))))
      (eglot-format (point) nil eglot--last-inserted-char))))

(defvar eglot--workspace-symbols-cache (make-hash-table :test #'equal)
  "Cache of `workspace/Symbol' results  used by `xref-find-definitions'.")

(defun eglot--pre-command-hook ()
  "Reset some temporary variables."
  (clrhash eglot--workspace-symbols-cache)
  (setq eglot--last-inserted-char nil))

(defun eglot--CompletionParams ()
  (append
   (eglot--TextDocumentPositionParams)
   `(:context
     ,(if-let (trigger (and (characterp eglot--last-inserted-char)
                            (cl-find eglot--last-inserted-char
                                     (eglot-server-capable :completionProvider
                                                            :triggerCharacters)
                                     :key (lambda (str) (aref str 0))
                                     :test #'char-equal)))
          `(:triggerKind 2 :triggerCharacter ,trigger) `(:triggerKind 1)))))

(defvar-local eglot--recent-changes nil
  "Recent buffer changes as collected by `eglot--before-change'.")

(cl-defmethod jsonrpc-connection-ready-p ((_server eglot-lsp-server) _what)
  "Tell if SERVER is ready for WHAT in current buffer."
  (and (cl-call-next-method) (not eglot--recent-changes)))

(defvar-local eglot--change-idle-timer nil "Idle timer for didChange signals.")

(defun eglot--before-change (beg end)
  "Hook onto `before-change-functions' with BEG and END."
  (when (listp eglot--recent-changes)
    ;; Records BEG and END, crucially convert them into LSP
    ;; (line/char) positions before that information is lost (because
    ;; the after-change thingy doesn't know if newlines were
    ;; deleted/added).  Also record markers of BEG and END
    ;; (github#259)
    (push `(,(eglot--pos-to-lsp-position beg)
            ,(eglot--pos-to-lsp-position end)
            (,beg . ,(copy-marker beg nil))
            (,end . ,(copy-marker end t)))
          eglot--recent-changes)))

(defvar eglot--document-changed-hook '(eglot--signal-textDocument/didChange)
  "Internal hook for doing things when the document changes.")

(defun eglot--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (pcase (car-safe eglot--recent-changes)
    (`(,lsp-beg ,lsp-end
                (,b-beg . ,b-beg-marker)
                (,b-end . ,b-end-marker))
     ;; github#259 and github#367: with `capitalize-word' & friends,
     ;; `before-change-functions' records the whole word's `b-beg' and
     ;; `b-end'.  Similarly, when `fill-paragraph' coalesces two
     ;; lines, `b-beg' and `b-end' mark end of first line and end of
     ;; second line, resp.  In both situations, `beg' and `end'
     ;; received here seemingly contradict that: they will differ by 1
     ;; and encompass the capitalized character or, in the coalescing
     ;; case, the replacement of the newline with a space.  We keep
     ;; both markers and positions to detect and correct this.  In
     ;; this specific case, we ignore `beg', `len' and
     ;; `pre-change-len' and send richer information about the region
     ;; from the markers.  I've also experimented with doing this
     ;; unconditionally but it seems to break when newlines are added.
     (if (and (= b-end b-end-marker) (= b-beg b-beg-marker)
              (or (/= beg b-beg) (/= end b-end)))
         (setcar eglot--recent-changes
                 `(,lsp-beg ,lsp-end ,(- b-end-marker b-beg-marker)
                            ,(buffer-substring-no-properties b-beg-marker
                                                             b-end-marker)))
       (setcar eglot--recent-changes
               `(,lsp-beg ,lsp-end ,pre-change-length
                          ,(buffer-substring-no-properties beg end)))))
    (_ (setf eglot--recent-changes :emacs-messup)))
  (when eglot--change-idle-timer (cancel-timer eglot--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq eglot--change-idle-timer
          (run-with-idle-timer
           eglot-send-changes-idle-time
           nil (lambda () (eglot--when-live-buffer buf
                            (when eglot--managed-mode
                              (run-hooks 'eglot--document-changed-hook)
                              (setq eglot--change-idle-timer nil))))))))

(defvar-local eglot-workspace-configuration ()
  "Configure LSP servers specifically for a given project.

This variable's value should be a plist (SECTION VALUE ...).
SECTION is a keyword naming a parameter section relevant to a
particular server.  VALUE is a plist or a primitive type
converted to JSON also understood by that server.

Instead of a plist, an alist ((SECTION . VALUE) ...) can be used
instead, but this variant is less reliable and not recommended.

This variable should be set as a directory-local variable.  See
info node `(emacs)Directory Variables' for various ways to do that.

Here's an example value that establishes two sections relevant to
the Pylsp and Gopls LSP servers:

  (:pylsp (:plugins (:jedi_completion (:include_params t
                                       :fuzzy t)
                     :pylint (:enabled :json-false)))
   :gopls (:usePlaceholders t))

The value of this variable can also be a unary function of a
single argument, which will be a connected `eglot-lsp-server'
instance.  The function runs with `default-directory' set to the
root of the current project.  It should return an object of the
format described above.")

;;;###autoload
(put 'eglot-workspace-configuration 'safe-local-variable 'listp)

(defun eglot-show-workspace-configuration (&optional server)
  "Dump `eglot-workspace-configuration' as JSON for debugging."
  (interactive (list (eglot--read-server "Show workspace configuration for" t)))
  (let ((conf (eglot--workspace-configuration-plist server)))
    (with-current-buffer (get-buffer-create "*EGLOT workspace configuration*")
      (erase-buffer)
      (insert (jsonrpc--json-encode conf))
      (with-no-warnings
        (require 'json)
        (when (require 'json-mode nil t) (json-mode))
        (json-pretty-print-buffer))
      (pop-to-buffer (current-buffer)))))

(defun eglot--workspace-configuration-plist (server &optional path)
  "Returns SERVER's workspace configuration as a plist.
If PATH consider that file's `file-name-directory' to get the
local value of the `eglot-workspace-configuration' variable, else
use the root of SERVER's `eglot--project'."
  (let ((val (with-temp-buffer
               (setq default-directory
                     ;; See github#1281
                     (if path (if (file-directory-p path)
                                  (file-name-as-directory path)
                                (file-name-directory path))
                       (project-root (eglot--project server))))
               ;; Set the major mode to be the first of the managed
               ;; modes.  This is the one the user started eglot in.
               (setq major-mode (car (eglot--major-modes server)))
               (hack-dir-local-variables-non-file-buffer)
               (if (functionp eglot-workspace-configuration)
                   (funcall eglot-workspace-configuration server)
                 eglot-workspace-configuration))))
    (or (and (consp (car val))
             (cl-loop for (section . v) in val
                      collect (if (keywordp section) section
                                (intern (format ":%s" section)))
                      collect v))
        val)))

(defun eglot-signal-didChangeConfiguration (server)
  "Send a `:workspace/didChangeConfiguration' signal to SERVER.
When called interactively, use the currently active server"
  (interactive (list (eglot--current-server-or-lose)))
  (jsonrpc-notify
   server :workspace/didChangeConfiguration
   (list
    :settings
    (or (eglot--workspace-configuration-plist server)
        eglot--{}))))

(cl-defmethod eglot-handle-request
  (server (_method (eql workspace/configuration)) &key items)
  "Handle server request workspace/configuration."
  (apply #'vector
         (mapcar
          (eglot--lambda ((ConfigurationItem) scopeUri section)
            (cl-loop
             with scope-uri-path = (and scopeUri (eglot-uri-to-path scopeUri))
             for (wsection o)
             on (eglot--workspace-configuration-plist server scope-uri-path)
             by #'cddr
             when (string=
                   (if (keywordp wsection)
                       (substring (symbol-name wsection) 1)
                     wsection)
                   section)
             return o))
          items)))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when eglot--recent-changes
    (let* ((server (eglot--current-server-or-lose))
           (sync-capability (eglot-server-capable :textDocumentSync))
           (sync-kind (if (numberp sync-capability) sync-capability
                        (plist-get sync-capability :change)))
           (full-sync-p (or (eq sync-kind 1)
                            (eq :emacs-messup eglot--recent-changes))))
      (jsonrpc-notify
       server :textDocument/didChange
       (list
        :textDocument (eglot--VersionedTextDocumentIdentifier)
        :contentChanges
        (if full-sync-p
            (vector `(:text ,(eglot--widening
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
          (cl-loop for (beg end len text) in (reverse eglot--recent-changes)
                   ;; github#259: `capitalize-word' and commands based
                   ;; on `casify_region' will cause multiple duplicate
                   ;; empty entries in `eglot--before-change' calls
                   ;; without an `eglot--after-change' reciprocal.
                   ;; Weed them out here.
                   when (numberp len)
                   vconcat `[,(list :range `(:start ,beg :end ,end)
                                    :rangeLength len :text text)]))))
      (setq eglot--recent-changes nil)
      (jsonrpc--call-deferred server))))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq eglot--recent-changes nil eglot--versioned-identifier 0)
  (jsonrpc-notify
   (eglot--current-server-or-lose)
   :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[eglot] error sending textDocument/didClose: %s"
    (jsonrpc-notify
     (eglot--current-server-or-lose)
     :textDocument/didClose `(:textDocument ,(eglot--TextDocumentIdentifier)))))

(defun eglot--signal-textDocument/willSave ()
  "Maybe send textDocument/willSave to server."
  (let ((server (eglot--current-server-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (when (eglot-server-capable :textDocumentSync :willSave)
      (jsonrpc-notify server :textDocument/willSave params))
    (when (eglot-server-capable :textDocumentSync :willSaveWaitUntil)
      (ignore-errors
        (eglot--apply-text-edits
         (eglot--request server :textDocument/willSaveWaitUntil params
                         :timeout 0.5))))))

(defun eglot--signal-textDocument/didSave ()
  "Maybe send textDocument/didSave to server."
  (eglot--signal-textDocument/didChange)
  (when (eglot-server-capable :textDocumentSync :save)
    (jsonrpc-notify
     (eglot--current-server-or-lose)
     :textDocument/didSave
     (list
      ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
      :text (buffer-substring-no-properties (point-min) (point-max))
      :textDocument (eglot--TextDocumentIdentifier)))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "A Flymake backend for Eglot.
Calls REPORT-FN (or arranges for it to be called) when the server
publishes diagnostics.  Between calls to this function, REPORT-FN
may be called multiple times (respecting the protocol of
`flymake-diagnostic-functions')."
  (cond (eglot--managed-mode
         (setq eglot--current-flymake-report-fn report-fn)
         (eglot--report-to-flymake eglot--diagnostics))
        (t
         (funcall report-fn nil))))

(defun eglot--report-to-flymake (diags)
  "Internal helper for `eglot-flymake-backend'."
  (save-restriction
    (widen)
    (funcall eglot--current-flymake-report-fn diags
             ;; If the buffer hasn't changed since last
             ;; call to the report function, flymake won't
             ;; delete old diagnostics.  Using :region
             ;; keyword forces flymake to delete
             ;; them (github#159).
             :region (cons (point-min) (point-max))))
  (setq eglot--diagnostics diags))

(defun eglot-xref-backend () "Eglot xref backend." 'eglot)

(defvar eglot--temp-location-buffers (make-hash-table :test #'equal)
  "Helper variable for `eglot--collecting-xrefs'.")

(defvar eglot-xref-lessp-function #'ignore
  "Compare two `xref-item' objects for sorting.")

(cl-defmacro eglot--collecting-xrefs ((collector) &rest body)
  "Sort and handle xrefs collected with COLLECTOR in BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((collected (cl-gensym "collected")))
    `(unwind-protect
         (let (,collected)
           (cl-flet ((,collector (xref) (push xref ,collected)))
             ,@body)
           (setq ,collected (nreverse ,collected))
           (sort ,collected eglot-xref-lessp-function))
       (maphash (lambda (_uri buf) (kill-buffer buf)) eglot--temp-location-buffers)
       (clrhash eglot--temp-location-buffers))))

(defun eglot--xref-make-match (name uri range)
  "Like `xref-make-match' but with LSP's NAME, URI and RANGE.
Try to visit the target file for a richer summary line."
  (pcase-let*
      ((file (eglot-uri-to-path uri))
       (visiting (or (find-buffer-visiting file)
                     (gethash uri eglot--temp-location-buffers)))
       (collect (lambda ()
                  (eglot--widening
                   (pcase-let* ((`(,beg . ,end) (eglot-range-region range))
                                (bol (progn (goto-char beg) (eglot--bol)))
                                (substring (buffer-substring bol (line-end-position)))
                                (hi-beg (- beg bol))
                                (hi-end (- (min (line-end-position) end) bol)))
                     (add-face-text-property hi-beg hi-end 'xref-match
                                             t substring)
                     (list substring (line-number-at-pos (point) t)
                           (eglot-utf-32-linepos) (- end beg))))))
       (`(,summary ,line ,column ,length)
        (cond
         (visiting (with-current-buffer visiting (funcall collect)))
         ((file-readable-p file) (with-current-buffer
                                     (puthash uri (generate-new-buffer " *temp*")
                                              eglot--temp-location-buffers)
                                   (insert-file-contents file)
                                   (funcall collect)))
         (t ;; fall back to the "dumb strategy"
          (let* ((start (cl-getf range :start))
                 (line (1+ (cl-getf start :line)))
                 (start-pos (cl-getf start :character))
                 (end-pos (cl-getf (cl-getf range :end) :character)))
            (list name line start-pos (- end-pos start-pos)))))))
    (setf (gethash (expand-file-name file) eglot--servers-by-xrefed-file)
          (eglot--current-server-or-lose))
    (xref-make-match summary (xref-make-file-location file line column) length)))

(defun eglot--workspace-symbols (pat &optional buffer)
  "Ask for :workspace/symbol on PAT, return list of formatted strings.
If BUFFER, switch to it before."
  (with-current-buffer (or buffer (current-buffer))
    (eglot-server-capable-or-lose :workspaceSymbolProvider)
    (mapcar
     (lambda (wss)
       (eglot--dbind ((WorkspaceSymbol) name containerName kind) wss
         (propertize
          (format "%s%s %s"
                  (if (zerop (length containerName)) ""
                    (concat (propertize containerName 'face 'shadow) " "))
                  name
                  (propertize (alist-get kind eglot--symbol-kind-names "Unknown")
                              'face 'shadow))
          'eglot--lsp-workspaceSymbol wss)))
     (eglot--request (eglot--current-server-or-lose) :workspace/symbol
                     `(:query ,pat)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
  "Yet another tricky connection between LSP and Elisp completion semantics."
  (let ((buf (current-buffer)) (cache eglot--workspace-symbols-cache))
    (cl-labels ((refresh (pat) (eglot--workspace-symbols pat buf))
                (lookup-1 (pat) ;; check cache, else refresh
                  (let ((probe (gethash pat cache :missing)))
                    (if (eq probe :missing) (puthash pat (refresh pat) cache)
                      probe)))
                (lookup (pat _point)
                  (let ((res (lookup-1 pat))
                        (def (and (string= pat "") (gethash :default cache))))
                    (append def res nil)))
                (score (c)
                  (cl-getf (get-text-property
                            0 'eglot--lsp-workspaceSymbol c)
                           :score 0)))
      (external-completion-table
       'eglot-indirection-joy
       #'lookup
       `((cycle-sort-function
          . ,(lambda (completions)
               (cl-sort completions #'> :key #'score))))))))

(defun eglot--recover-workspace-symbol-meta (string)
  "Search `eglot--workspace-symbols-cache' for rich entry of STRING."
  (catch 'found
    (maphash (lambda (_k v)
               (while (consp v)
                 ;; Like mess? Ask minibuffer.el about improper lists.
                 (when (equal (car v) string) (throw 'found (car v)))
                 (setq v (cdr v))))
             eglot--workspace-symbols-cache)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
  (let ((attempt
         (and (xref--prompt-p this-command)
              (puthash :default
                       (ignore-errors
                         (eglot--workspace-symbols (symbol-name (symbol-at-point))))
                       eglot--workspace-symbols-cache))))
    (if attempt (car attempt) "LSP identifier at point")))

(defvar eglot--lsp-xref-refs nil
  "`xref' objects for overriding `xref-backend-references''s.")

(cl-defun eglot--lsp-xrefs-for-method (method &key extra-params capability)
  "Make `xref''s for METHOD, EXTRA-PARAMS, check CAPABILITY."
  (eglot-server-capable-or-lose
   (or capability
       (intern
        (format ":%sProvider"
                (cadr (split-string (symbol-name method)
                                    "/"))))))
  (let ((response
         (eglot--request
          (eglot--current-server-or-lose)
          method (append (eglot--TextDocumentPositionParams) extra-params))))
    (eglot--collecting-xrefs (collect)
      (mapc
       (lambda (loc-or-loc-link)
         (let ((sym-name (symbol-name (symbol-at-point))))
           (eglot--dcase loc-or-loc-link
             (((LocationLink) targetUri targetSelectionRange)
              (collect (eglot--xref-make-match sym-name
                                               targetUri targetSelectionRange)))
             (((Location) uri range)
              (collect (eglot--xref-make-match sym-name
                                               uri range))))))
       (if (vectorp response) response (and response (list response)))))))

(cl-defun eglot--lsp-xref-helper (method &key extra-params capability)
  "Helper for `eglot-find-declaration' & friends."
  (let ((eglot--lsp-xref-refs (eglot--lsp-xrefs-for-method
                               method
                               :extra-params extra-params
                               :capability capability)))
    (if eglot--lsp-xref-refs
        (xref-find-references "LSP identifier at point.")
      (eglot--message "%s returned no references" method))))

(defun eglot-find-declaration ()
  "Find declaration for SYM, the identifier at point."
  (interactive)
  (eglot--lsp-xref-helper :textDocument/declaration))

(defun eglot-find-implementation ()
  "Find implementation for SYM, the identifier at point."
  (interactive)
  (eglot--lsp-xref-helper :textDocument/implementation))

(defun eglot-find-typeDefinition ()
  "Find type definition for SYM, the identifier at point."
  (interactive)
  (eglot--lsp-xref-helper :textDocument/typeDefinition))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot)) id)
  (let ((probe (eglot--recover-workspace-symbol-meta id)))
    (if probe
        (eglot--dbind ((WorkspaceSymbol) name location)
            (get-text-property 0 'eglot--lsp-workspaceSymbol probe)
          (eglot--dbind ((Location) uri range) location
            (list (eglot--xref-make-match name uri range))))
      (eglot--lsp-xrefs-for-method :textDocument/definition))))

(cl-defmethod xref-backend-references ((_backend (eql eglot)) _identifier)
  (or
   eglot--lsp-xref-refs
   (eglot--lsp-xrefs-for-method
    :textDocument/references :extra-params `(:context (:includeDeclaration t)))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot-server-capable :workspaceSymbolProvider)
    (eglot--collecting-xrefs (collect)
      (mapc
       (eglot--lambda ((SymbolInformation) name location)
         (eglot--dbind ((Location) uri range) location
           (collect (eglot--xref-make-match name uri range))))
       (eglot--request (eglot--current-server-or-lose)
                       :workspace/symbol
                       `(:query ,pattern))))))

(defun eglot-format-buffer ()
  "Format contents of current buffer."
  (interactive)
  (eglot-format nil nil))

(defun eglot-format (&optional beg end on-type-format)
  "Format region BEG END.
If either BEG or END is nil, format entire buffer.
Interactively, format active region, or entire buffer if region
is not active.

If non-nil, ON-TYPE-FORMAT is a character just inserted at BEG
for which LSP on-type-formatting should be requested."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (pcase-let ((`(,method ,cap ,args)
               (cond
                ((and beg on-type-format)
                 `(:textDocument/onTypeFormatting
                   :documentOnTypeFormattingProvider
                   ,`(:position ,(eglot--pos-to-lsp-position beg)
                                :ch ,(string on-type-format))))
                ((and beg end)
                 `(:textDocument/rangeFormatting
                   :documentRangeFormattingProvider
                   (:range ,(list :start (eglot--pos-to-lsp-position beg)
                                  :end (eglot--pos-to-lsp-position end)))))
                (t
                 '(:textDocument/formatting :documentFormattingProvider nil)))))
    (eglot-server-capable-or-lose cap)
    (eglot--apply-text-edits
     (eglot--request
      (eglot--current-server-or-lose)
      method
      (cl-list*
       :textDocument (eglot--TextDocumentIdentifier)
       :options (list :tabSize tab-width
                      :insertSpaces (if indent-tabs-mode :json-false t)
                      :insertFinalNewline (if require-final-newline t :json-false)
                      :trimFinalNewlines (if delete-trailing-lines t :json-false))
       args))
     nil
     on-type-format)))

(defvar eglot-cache-session-completions t
  "If non-nil Eglot caches data during completion sessions.")

(defvar eglot--capf-session :none "A cache used by `eglot-completion-at-point'.")

(defun eglot--capf-session-flush (&optional _) (setq eglot--capf-session :none))

(defun eglot--dumb-flex (pat comp ignorecase)
  "Return destructively fontified COMP iff PAT matches it."
  (cl-loop with lcomp = (length comp)
           with case-fold-search = ignorecase
           initially (remove-list-of-text-properties 0 lcomp '(face) comp)
           for x across pat
           for i = (cl-loop for j from (if i (1+ i) 0) below lcomp
                            when (char-equal x (aref comp j)) return j)
           unless i do (cl-return nil)
           ;; FIXME: could do much better here and coalesce intervals
           do (add-face-text-property i (1+ i) 'completions-common-part
                                      nil comp)
           finally (cl-return comp)))

(defun eglot--dumb-allc (pat table pred _point) (funcall table pat pred t))

(add-to-list 'completion-category-defaults '(eglot-capf (styles eglot--dumb-flex)))
(add-to-list 'completion-styles-alist '(eglot--dumb-flex ignore eglot--dumb-allc))

(defun eglot-completion-at-point ()
  "Eglot's `completion-at-point' function."
  ;; Commit logs for this function help understand what's going on.
  (when-let (completion-capability (eglot-server-capable :completionProvider))
    (let* ((server (eglot--current-server-or-lose))
           (bounds (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point))))
           (bounds-string (buffer-substring (car bounds) (cdr bounds)))
           (sort-completions
            (lambda (completions)
              (cl-sort completions
                       #'string-lessp
                       :key (lambda (c)
                              (plist-get
                               (get-text-property 0 'eglot--lsp-item c)
                               :sortText)))))
           (metadata `(metadata (category . eglot-capf)
                                (display-sort-function . ,sort-completions)))
           (local-cache :none)
           (orig-pos (point))
           (resolved (make-hash-table))
           (proxies
            (lambda ()
              (if (listp local-cache) local-cache
                (let* ((resp (eglot--request server
                                             :textDocument/completion
                                             (eglot--CompletionParams)
                                             :cancel-on-input t))
                       (items (append
                               (if (vectorp resp) resp (plist-get resp :items))
                               nil))
                       (cachep (and (listp resp) items
                                    eglot-cache-session-completions
                                    (eq (plist-get resp :isIncomplete) :json-false)))
                       (retval
                        (mapcar
                         (jsonrpc-lambda
                             (&rest item &key label insertText insertTextFormat
                                    textEdit &allow-other-keys)
                           (let ((proxy
                                  ;; Snippet or textEdit, it's safe to
                                  ;; display/insert the label since
                                  ;; it'll be adjusted.  If no usable
                                  ;; insertText at all, label is best,
                                  ;; too.
                                  (cond ((or (eql insertTextFormat 2)
                                             textEdit
                                             (null insertText)
                                             (string-empty-p insertText))
                                         (string-trim-left label))
                                        (t insertText))))
                             (unless (zerop (length proxy))
                               (put-text-property 0 1 'eglot--lsp-item item proxy))
                             proxy))
                         items)))
                  ;; (trace-values "Requested" (length proxies) cachep bounds)
                  (setq eglot--capf-session
                        (if cachep (list bounds retval resolved orig-pos) :none))
                  (setq local-cache retval)))))
           (resolve-maybe
            ;; Maybe completion/resolve JSON object `lsp-comp' into
            ;; another JSON object, if at all possible.  Otherwise,
            ;; just return lsp-comp.
            (lambda (lsp-comp)
              (or (gethash lsp-comp resolved)
                  (setf (gethash lsp-comp resolved)
                        (if (and (eglot-server-capable :completionProvider
                                                        :resolveProvider)
                                 (plist-get lsp-comp :data))
                            (eglot--request server :completionItem/resolve
                                            lsp-comp :cancel-on-input t)
                          lsp-comp))))))
      (when (and (consp eglot--capf-session)
                 (= (car bounds) (car (nth 0 eglot--capf-session)))
                 (>= (cdr bounds) (cdr (nth 0 eglot--capf-session))))
        (setq local-cache (nth 1 eglot--capf-session)
              resolved (nth 2 eglot--capf-session)
              orig-pos (nth 3 eglot--capf-session))
        ;; (trace-values "Recalling cache" (length local-cache) bounds orig-pos)
        )
      (list
       (car bounds)
       (cdr bounds)
       (lambda (pattern pred action)
         (cond
          ((eq action 'metadata) metadata)               ; metadata
          ((eq action 'lambda)                           ; test-completion
           (test-completion pattern (funcall proxies)))
          ((eq (car-safe action) 'boundaries) nil)       ; boundaries
          ((null action)                                 ; try-completion
           (try-completion pattern (funcall proxies)))
          ((eq action t)                                 ; all-completions
           (let ((comps (funcall proxies)))
             (dolist (c comps) (eglot--dumb-flex pattern c t))
             (all-completions
              ""
              comps
              (lambda (proxy)
                (let* ((item (get-text-property 0 'eglot--lsp-item proxy))
                       (filterText (plist-get item :filterText)))
                  (and (or (null pred) (funcall pred proxy))
                       (eglot--dumb-flex
                        pattern (or filterText proxy) completion-ignore-case)))))))))
       :annotation-function
       (lambda (proxy)
         (eglot--dbind ((CompletionItem) detail kind)
             (get-text-property 0 'eglot--lsp-item proxy)
           (let* ((detail (and (stringp detail)
                               (not (string= detail ""))
                               detail))
                  (annotation
                   (or detail
                       (cdr (assoc kind eglot--kind-names)))))
             (when annotation
               (concat " "
                       (propertize annotation
                                   'face 'font-lock-function-name-face))))))
       :company-kind
       ;; Associate each lsp-item with a lsp-kind symbol.
       (lambda (proxy)
         (when-let* ((lsp-item (get-text-property 0 'eglot--lsp-item proxy))
                     (kind (alist-get (plist-get lsp-item :kind)
                                      eglot--kind-names)))
           (pcase kind
             ("EnumMember" 'enum-member)
             ("TypeParameter" 'type-parameter)
             (_ (intern (downcase kind))))))
       :company-deprecated
       (lambda (proxy)
         (when-let ((lsp-item (get-text-property 0 'eglot--lsp-item proxy)))
           (or (seq-contains-p (plist-get lsp-item :tags)
                               1)
               (eq t (plist-get lsp-item :deprecated)))))
       :company-docsig
       ;; FIXME: autoImportText is specific to the pyright language server
       (lambda (proxy)
         (when-let* ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy))
                     (data (plist-get (funcall resolve-maybe lsp-comp) :data))
                     (import-text (plist-get data :autoImportText)))
           import-text))
       :company-doc-buffer
       (lambda (proxy)
         (let* ((documentation
                 (let ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy)))
                   (plist-get (funcall resolve-maybe lsp-comp) :documentation)))
                (formatted (and documentation
                                (eglot--format-markup documentation))))
           (when formatted
             (with-current-buffer (get-buffer-create " *eglot doc*")
               (erase-buffer)
               (insert formatted)
               (current-buffer)))))
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (goto-char (car bounds))
         (when (listp completion-capability)
           (looking-back
            (regexp-opt
             (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
            (eglot--bol))))
       :exit-function
       (lambda (proxy status)
         (eglot--capf-session-flush)
         (when (memq status '(finished exact))
           ;; To assist in using this whole `completion-at-point'
           ;; function inside `completion-in-region', ensure the exit
           ;; function runs in the buffer where the completion was
           ;; triggered from.  This should probably be in Emacs itself.
           ;; (github#505)
           (with-current-buffer (if (minibufferp)
                                    (window-buffer (minibuffer-selected-window))
                                  (current-buffer))
             (eglot--dbind ((CompletionItem) insertTextFormat
                            insertText textEdit additionalTextEdits label)
                 (funcall
                  resolve-maybe
                  (or (get-text-property 0 'eglot--lsp-item proxy)
                      ;; When selecting from the *Completions*
                      ;; buffer, `proxy' won't have any properties.
                      ;; A lookup should fix that (github#148)
                      (get-text-property
                       0 'eglot--lsp-item
                       (cl-find proxy (funcall proxies) :test #'string=))))
               (let ((snippet-fn (and (eql insertTextFormat 2)
                                      (eglot--snippet-expansion-fn))))
                 (cond (textEdit
                        ;; Revert buffer back to state when the edit
                        ;; was obtained from server. If a `proxy'
                        ;; "bar" was obtained from a buffer with
                        ;; "foo.b", the LSP edit applies to that
                        ;; state, _not_ the current "foo.bar".
                        (delete-region orig-pos (point))
                        (insert (substring bounds-string (- orig-pos (car bounds))))
                        (eglot--dbind ((TextEdit) range newText) textEdit
                          (pcase-let ((`(,beg . ,end)
                                       (eglot-range-region range)))
                            (delete-region beg end)
                            (goto-char beg)
                            (funcall (or snippet-fn #'insert) newText))))
                       (snippet-fn
                        ;; A snippet should be inserted, but using plain
                        ;; `insertText'.  This requires us to delete the
                        ;; whole completion, since `insertText' is the full
                        ;; completion's text.
                        (delete-region (- (point) (length proxy)) (point))
                        (funcall snippet-fn (or insertText label))))
                 (when (cl-plusp (length additionalTextEdits))
                   (eglot--apply-text-edits additionalTextEdits)))
               (eglot--signal-textDocument/didChange)))))))))

(defun eglot--hover-info (contents &optional _range)
  (mapconcat #'eglot--format-markup
             (if (vectorp contents) contents (list contents)) "\n"))

(defun eglot--sig-info (sig &optional sig-active briefp)
  (eglot--dbind ((SignatureInformation)
                 ((:label siglabel))
                 ((:documentation sigdoc)) parameters activeParameter)
      sig
    (with-temp-buffer
      (insert siglabel)
      ;; Add documentation, indented so we can distinguish multiple signatures
      (when-let (doc (and (not briefp) sigdoc (eglot--format-markup sigdoc)))
        (goto-char (point-max))
        (insert "\n" (replace-regexp-in-string "^" "  " doc)))
      ;; Try to highlight function name only
      (let (first-parlabel)
        (cond ((and (cl-plusp (length parameters))
                    (vectorp (setq first-parlabel
                                   (plist-get (aref parameters 0) :label))))
               (save-excursion
                (goto-char (elt first-parlabel 0))
                (skip-syntax-backward "^w")
                (add-face-text-property (point-min) (point)
                                        'font-lock-function-name-face)))
              ((save-excursion
                 (goto-char (point-min))
                 (looking-at "\\([^(]*\\)([^)]*)"))
               (add-face-text-property (match-beginning 1) (match-end 1)
                                       'font-lock-function-name-face))))
      ;; Now to the parameters
      (cl-loop
       with active-param = (or sig-active activeParameter)
       for i from 0 for parameter across parameters do
       (eglot--dbind ((ParameterInformation)
                      ((:label parlabel))
                      ((:documentation pardoc)))
           parameter
         ;; ...perhaps highlight it in the formals list
         (when (eq i active-param)
           (save-excursion
             (goto-char (point-min))
             (pcase-let
                 ((`(,beg ,end)
                   (if (stringp parlabel)
                       (let ((case-fold-search nil))
                         (and (search-forward parlabel (line-end-position) t)
                              (list (match-beginning 0) (match-end 0))))
                     (mapcar #'1+ (append parlabel nil)))))
               (if (and beg end)
                   (add-face-text-property
                    beg end
                    'eldoc-highlight-function-argument)))))
         ;; ...and/or maybe add its doc on a line by its own.
         (let (fpardoc)
           (when (and pardoc (not briefp)
                      (not (string-empty-p
                            (setq fpardoc (eglot--format-markup pardoc)))))
             (insert "\n  "
                     (propertize
                      (if (stringp parlabel) parlabel
                        (apply #'substring siglabel (mapcar #'1+ parlabel)))
                      'face (and (eq i active-param) 'eldoc-highlight-function-argument))
                     ": " fpardoc)))))
      (buffer-string))))

(defun eglot-signature-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for signatures."
  (when (eglot-server-capable :signatureHelpProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/signatureHelp (eglot--TextDocumentPositionParams)
       :success-fn
       (eglot--lambda ((SignatureHelp)
                       signatures activeSignature (activeParameter 0))
         (eglot--when-buffer-window buf
           (let ((active-sig (and (cl-plusp (length signatures))
                                  (aref signatures (or activeSignature 0)))))
             (if (not active-sig) (funcall cb nil)
               (funcall
                cb (mapconcat (lambda (s)
                                (eglot--sig-info s (and (eq s active-sig)
                                                        activeParameter)
                                                 nil))
                              signatures "\n")
                :echo (eglot--sig-info active-sig activeParameter t))))))
       :deferred :textDocument/signatureHelp))
    t))

(defun eglot-hover-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for hover."
  (when (eglot-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn (eglot--lambda ((Hover) contents range)
                     (eglot--when-buffer-window buf
                       (let ((info (unless (seq-empty-p contents)
                                     (eglot--hover-info contents range))))
                         (funcall cb info
                                  :echo (and info (string-match "\n" info))))))
       :deferred :textDocument/hover))
    (eglot--highlight-piggyback cb)
    t))

(defvar eglot--highlights nil "Overlays for textDocument/documentHighlight.")

(defun eglot--highlight-piggyback (_cb)
  "Request and handle `:textDocument/documentHighlight'."
  ;; FIXME: Obviously, this is just piggy backing on eldoc's calls for
  ;; convenience, as shown by the fact that we just ignore cb.
  (let ((buf (current-buffer)))
    (when (eglot-server-capable :documentHighlightProvider)
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/documentHighlight (eglot--TextDocumentPositionParams)
       :success-fn
       (lambda (highlights)
         (mapc #'delete-overlay eglot--highlights)
         (setq eglot--highlights
               (eglot--when-buffer-window buf
                 (mapcar
                  (eglot--lambda ((DocumentHighlight) range)
                    (pcase-let ((`(,beg . ,end)
                                 (eglot-range-region range)))
                      (let ((ov (make-overlay beg end)))
                        (overlay-put ov 'face 'eglot-highlight-symbol-face)
                        (overlay-put ov 'modification-hooks
                                     `(,(lambda (o &rest _) (delete-overlay o))))
                        ov)))
                  highlights))))
       :deferred :textDocument/documentHighlight)
      nil)))

(defun eglot--imenu-SymbolInformation (res)
  "Compute `imenu--index-alist' for RES vector of SymbolInformation."
  (mapcar
   (pcase-lambda (`(,kind . ,objs))
     (cons
      (alist-get kind eglot--symbol-kind-names "Unknown")
      (mapcan
       (pcase-lambda (`(,container . ,objs))
         (let ((elems (mapcar
                       (eglot--lambda ((SymbolInformation) kind name location)
                         (let ((reg (eglot-range-region
                                     (plist-get location :range)))
                               (kind (alist-get kind eglot--symbol-kind-names)))
                           (cons (propertize name
                                             'breadcrumb-region reg
                                             'breadcrumb-kind kind)
                                 (car reg))))
                       objs)))
           (if container (list (cons container elems)) elems)))
       (seq-group-by
        (eglot--lambda ((SymbolInformation) containerName) containerName) objs))))
   (seq-group-by (eglot--lambda ((SymbolInformation) kind) kind) res)))

(defun eglot--imenu-DocumentSymbol (res)
  "Compute `imenu--index-alist' for RES vector of DocumentSymbol."
  (cl-labels ((dfs (&key name children range kind &allow-other-keys)
                (let* ((reg (eglot-range-region range))
                       (kind (alist-get kind eglot--symbol-kind-names))
                       (name (propertize name
                                         'breadcrumb-region reg
                                         'breadcrumb-kind kind)))
                  (if (seq-empty-p children)
                      (cons name (car reg))
                    (cons name
                            (mapcar (lambda (c) (apply #'dfs c)) children))))))
    (mapcar (lambda (s) (apply #'dfs s)) res)))

(cl-defun eglot-imenu ()
  "Eglot's `imenu-create-index-function'.
Returns a list as described in docstring of `imenu--index-alist'."
  (unless (eglot-server-capable :documentSymbolProvider)
    (cl-return-from eglot-imenu))
  (let* ((res (eglot--request (eglot--current-server-or-lose)
                              :textDocument/documentSymbol
                              `(:textDocument
                                ,(eglot--TextDocumentIdentifier))
                              :cancel-on-input non-essential))
         (head (and (cl-plusp (length res)) (elt res 0))))
    (when head
      (eglot--dcase head
        (((SymbolInformation)) (eglot--imenu-SymbolInformation res))
        (((DocumentSymbol)) (eglot--imenu-DocumentSymbol res))))))

(cl-defun eglot--apply-text-edits (edits &optional version silent)
  "Apply EDITS for current buffer if at VERSION, or if it's nil.
If SILENT, don't echo progress in mode-line."
  (unless edits (cl-return-from eglot--apply-text-edits))
  (unless (or (not version) (equal version eglot--versioned-identifier))
    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version eglot--versioned-identifier))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (unless silent
                       (make-progress-reporter
                        (format "[eglot] applying %s edits to `%s'..."
                                howmany (current-buffer))
                        0 howmany)))
           (done 0))
      (mapc (pcase-lambda (`(,newText ,beg . ,end))
              (let ((source (current-buffer)))
                (with-temp-buffer
                  (insert newText)
                  (let ((temp (current-buffer)))
                    (with-current-buffer source
                      (save-excursion
                        (save-restriction
                          (narrow-to-region beg end)
                          (replace-buffer-contents temp)))
                      (when reporter
                        (eglot--reporter-update reporter (cl-incf done))))))))
            (mapcar (eglot--lambda ((TextEdit) range newText)
                      (cons newText (eglot-range-region range 'markers)))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (when reporter
        (progress-reporter-done reporter)))))

(defun eglot--confirm-server-edits (origin _prepared)
  "Helper for `eglot--apply-workspace-edit.
ORIGIN is a symbol designating a command.  Reads the
`eglot-confirm-server-edits' user option and returns a symbol
like `diff', `summary' or nil."
  (let (v)
    (cond ((symbolp eglot-confirm-server-edits) eglot-confirm-server-edits)
          ((setq v (assoc origin eglot-confirm-server-edits)) (cdr v))
          ((setq v (assoc t eglot-confirm-server-edits)) (cdr v)))))

(defun eglot--propose-changes-as-diff (prepared)
  "Helper for `eglot--apply-workspace-edit'.
Goal is to popup a `diff-mode' buffer containing all the changes
of PREPARED, ready to apply with C-c C-a.  PREPARED is a
list ((FILENAME EDITS VERSION)...)."
  (with-current-buffer (get-buffer-create "*EGLOT proposed server changes*")
    (buffer-disable-undo (current-buffer))
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (diff-mode)
      (erase-buffer)
      (pcase-dolist (`(,path ,edits ,_) prepared)
        (with-temp-buffer
          (let* ((diff (current-buffer))
                 (existing-buf (find-buffer-visiting path))
                 (existing-buf-label (prin1-to-string existing-buf)))
            (with-temp-buffer
              (if existing-buf
                  (insert-buffer-substring existing-buf)
                (insert-file-contents path))
              (eglot--apply-text-edits edits nil t)
              (diff-no-select (or existing-buf path) (current-buffer) nil t diff)
              (when existing-buf
                ;; Here we have to pretend the label of the unsaved
                ;; buffer is the actual file, just so that we can
                ;; diff-apply without troubles.  If there's a better
                ;; way, it probably involves changes to `diff.el'.
                (with-current-buffer diff
                  (goto-char (point-min))
                  (while (search-forward existing-buf-label nil t)
                    (replace-match (buffer-file-name existing-buf))))))
            (with-current-buffer target
              (insert-buffer-substring diff))))))
    (setq-local buffer-read-only t)
    (buffer-enable-undo (current-buffer))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (font-lock-ensure)))

(defun eglot--apply-workspace-edit (wedit origin)
  "Apply (or offer to apply) the workspace edit WEDIT.
ORIGIN is a symbol designating the command that originated this
edit proposed by the server."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (eglot-uri-to-path uri) edits version)))
                   documentChanges)))
      (unless (and changes documentChanges)
        ;; We don't want double edits, and some servers send both
        ;; changes and documentChanges.  This unless ensures that we
        ;; prefer documentChanges over changes.
        (cl-loop for (uri edits) on changes by #'cddr
                 do (push (list (eglot-uri-to-path uri) edits) prepared)))
      (cl-flet ((notevery-visited-p ()
                  (cl-notevery #'find-buffer-visiting
                               (mapcar #'car prepared)))
                (accept-p ()
                  (y-or-n-p
                   (format "[eglot] Server wants to edit:\n%sProceed? "
                           (cl-loop
                            for (f eds _) in prepared
                            concat (format
                                    "  %s (%d change%s)\n"
                                    f (length eds)
                                    (if (> (length eds) 1) "s" ""))))))
                (apply ()
                  (cl-loop for edit in prepared
                   for (path edits version) = edit
                   do (with-current-buffer (find-file-noselect path)
                        (eglot--apply-text-edits edits version))
                   finally (eldoc) (eglot--message "Edit successful!"))))
        (let ((decision (eglot--confirm-server-edits origin prepared)))
          (cond
           ((or (eq decision 'diff)
                (and (eq decision 'maybe-diff) (notevery-visited-p)))
            (eglot--propose-changes-as-diff prepared))
           ((or (memq decision '(t summary))
                (and (eq decision 'maybe-summary) (notevery-visited-p)))
            (when (accept-p) (apply)))
           (t
            (apply))))))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (eglot-server-capable-or-lose :renameProvider)
  (eglot--apply-workspace-edit
   (eglot--request (eglot--current-server-or-lose)
                   :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                          :newName ,newname))
   this-command))

(defun eglot--code-action-bounds ()
  "Calculate appropriate bounds depending on region and point."
  (let (diags)
    (cond ((use-region-p) `(,(region-beginning) ,(region-end)))
          ((setq diags (flymake-diagnostics (point)))
           (cl-loop for d in diags
                    minimizing (flymake-diagnostic-beg d) into beg
                    maximizing (flymake-diagnostic-end d) into end
                    finally (cl-return (list beg end))))
          (t
           (let ((boftap (bounds-of-thing-at-point 'sexp)))
             (list (car boftap) (cdr boftap)))))))

(defun eglot-code-actions (beg &optional end action-kind interactive)
  "Find LSP code actions of type ACTION-KIND between BEG and END.
Interactively, offer to execute them.
If ACTION-KIND is nil, consider all kinds of actions.
Interactively, default BEG and END to region's bounds else BEG is
point and END is nil, which results in a request for code actions
at point.  With prefix argument, prompt for ACTION-KIND."
  (interactive
   `(,@(eglot--code-action-bounds)
     ,(and current-prefix-arg
           (completing-read "[eglot] Action kind: "
                            '("quickfix" "refactor.extract" "refactor.inline"
                              "refactor.rewrite" "source.organizeImports")))
     t))
  (eglot-server-capable-or-lose :codeActionProvider)
  (let* ((server (eglot--current-server-or-lose))
         (actions
          (eglot--request
           server
           :textDocument/codeAction
           (list :textDocument (eglot--TextDocumentIdentifier)
                 :range (list :start (eglot--pos-to-lsp-position beg)
                              :end (eglot--pos-to-lsp-position end))
                 :context
                 `(:diagnostics
                   [,@(cl-loop for diag in (flymake-diagnostics beg end)
                               when (cdr (assoc 'eglot-lsp-diag
                                                (eglot--diag-data diag)))
                               collect it)]
                   ,@(when action-kind `(:only [,action-kind]))))))
         ;; Redo filtering, in case the `:only' didn't go through.
         (actions (cl-loop for a across actions
                           when (or (not action-kind)
                                    (equal action-kind (plist-get a :kind)))
                           collect a)))
    (if interactive
        (eglot--read-execute-code-action actions server action-kind)
      actions)))

(defalias 'eglot-code-actions-at-mouse (eglot--mouse-call 'eglot-code-actions)
  "Like `eglot-code-actions', but intended for mouse events.")

(defun eglot--read-execute-code-action (actions server &optional action-kind)
  "Helper for interactive calls to `eglot-code-actions'."
  (let* ((menu-items
          (or (cl-loop for a in actions
                       collect (cons (plist-get a :title) a))
              (apply #'eglot--error
                     (if action-kind `("No \"%s\" code actions here" ,action-kind)
                       `("No code actions here")))))
         (preferred-action (cl-find-if
                            (lambda (menu-item)
                              (plist-get (cdr menu-item) :isPreferred))
                            menu-items))
         (default-action (car (or preferred-action (car menu-items))))
         (chosen (if (and action-kind (null (cadr menu-items)))
                     (cdr (car menu-items))
                   (if (listp last-nonmenu-event)
                       (x-popup-menu last-nonmenu-event `("Eglot code actions:"
                                                          ("dummy" ,@menu-items)))
                     (cdr (assoc (completing-read
                                  (format "[eglot] Pick an action (default %s): "
                                          default-action)
                                  menu-items nil t nil nil default-action)
                                 menu-items))))))
    (eglot-execute server chosen)))

(defmacro eglot--code-action (name kind)
  "Define NAME to execute KIND code action."
  `(defun ,name (beg &optional end)
     ,(format "Execute `%s' code actions between BEG and END." kind)
     (interactive (eglot--code-action-bounds))
     (eglot-code-actions beg end ,kind t)))

(eglot--code-action eglot-code-action-organize-imports "source.organizeImports")
(eglot--code-action eglot-code-action-extract "refactor.extract")
(eglot--code-action eglot-code-action-inline "refactor.inline")
(eglot--code-action eglot-code-action-rewrite "refactor.rewrite")
(eglot--code-action eglot-code-action-quickfix "quickfix")


;;; Dynamic registration
;;;
(cl-defmethod eglot-register-capability
  (server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles."
  (eglot-unregister-capability server method id)
  (let* (success
         (globs (mapcar
                 (eglot--lambda ((FileSystemWatcher) globPattern kind)
                   (cons (eglot--glob-compile globPattern t t)
                         ;; the default "7" means bitwise OR of
                         ;; WatchKind.Create (1), WatchKind.Change
                         ;; (2), WatchKind.Delete (4)
                         (or kind 7)))
                 watchers))
         (dirs-to-watch
          (delete-dups (mapcar #'file-name-directory
                               (project-files
                                (eglot--project server))))))
    (cl-labels
        ((handle-event (event)
           (pcase-let* ((`(,desc ,action ,file ,file1) event)
                        (action-type (cl-case action
                                       (created 1) (changed 2) (deleted 3)))
                        (action-bit (when action-type
                                      (ash 1 (1- action-type)))))
             (cond
              ((and (memq action '(created changed deleted))
                    (cl-loop for (glob . kind-bitmask) in globs
                             thereis (and (> (logand kind-bitmask action-bit) 0)
                                          (funcall glob file))))
               (jsonrpc-notify
                server :workspace/didChangeWatchedFiles
                `(:changes ,(vector `(:uri ,(eglot-path-to-uri file)
                                           :type ,action-type))))
               (when (and (eq action 'created)
                          (file-directory-p file))
                 (watch-dir file)))
              ((eq action 'renamed)
               (handle-event `(,desc 'deleted ,file))
               (handle-event `(,desc 'created ,file1))))))
         (watch-dir (dir)
           (when-let ((probe
                       (and (file-readable-p dir)
                            (or (gethash dir (eglot--file-watches server))
                                (puthash dir (list (file-notify-add-watch
                                                    dir '(change) #'handle-event))
                                         (eglot--file-watches server))))))
             (push id (cdr probe)))))
      (unwind-protect
          (progn
            (mapc #'watch-dir dirs-to-watch)
            (setq
             success
             `(:message ,(format "OK, watching %s directories in %s watchers"
                                 (length dirs-to-watch) (length watchers)))))
        (unless success
          (eglot-unregister-capability server method id))))))

(cl-defmethod eglot-unregister-capability
  (server (_method (eql workspace/didChangeWatchedFiles)) id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles."
  (maphash (lambda (dir watch-and-ids)
             (setcdr watch-and-ids (delete id (cdr watch-and-ids)))
             (when (null (cdr watch-and-ids))
               (file-notify-rm-watch (car watch-and-ids))
               (remhash dir (eglot--file-watches server))))
           (eglot--file-watches server))
  (list t "OK"))


;;; Glob heroics
;;;
(defun eglot--glob-parse (glob)
  "Compute list of (STATE-SYM EMITTER-FN PATTERN)."
  (with-temp-buffer
    (save-excursion (insert glob))
    (cl-loop
     with grammar = '((:**      "\\*\\*/?"              eglot--glob-emit-**)
                      (:*       "\\*"                   eglot--glob-emit-*)
                      (:?       "\\?"                   eglot--glob-emit-?)
                      (:{}      "{[^][*{}]+}"           eglot--glob-emit-{})
                      (:range   "\\[\\^?[^][/,*{}]+\\]" eglot--glob-emit-range)
                      (:literal "[^][,*?{}]+"           eglot--glob-emit-self))
     until (eobp)
     collect (cl-loop
              for (_token regexp emitter) in grammar
              thereis (and (re-search-forward (concat "\\=" regexp) nil t)
                           (list (cl-gensym "state-") emitter (match-string 0)))
              finally (error "Glob '%s' invalid at %s" (buffer-string) (point))))))

(defun eglot--glob-compile (glob &optional byte-compile noerror)
  "Convert GLOB into Elisp function.  Maybe BYTE-COMPILE it.
If NOERROR, return predicate, else erroring function."
  (let* ((states (eglot--glob-parse glob))
         (body `(with-current-buffer (get-buffer-create " *eglot-glob-matcher*")
                  (erase-buffer)
                  (save-excursion (insert string))
                  (cl-labels ,(cl-loop for (this that) on states
                                       for (self emit text) = this
                                       for next = (or (car that) 'eobp)
                                       collect (funcall emit text self next))
                    (or (,(caar states))
                        (error "Glob done but more unmatched text: '%s'"
                               (buffer-substring (point) (point-max)))))))
         (form `(lambda (string) ,(if noerror `(ignore-errors ,body) body))))
    (if byte-compile (byte-compile form) form)))

(defun eglot--glob-emit-self (text self next)
  `(,self () (re-search-forward ,(concat "\\=" (regexp-quote text))) (,next)))

(defun eglot--glob-emit-** (_ self next)
  `(,self () (or (ignore-errors (save-excursion (,next)))
                 (and (re-search-forward "\\=/?[^/]+/?") (,self)))))

(defun eglot--glob-emit-* (_ self next)
  `(,self () (re-search-forward "\\=[^/]")
          (or (ignore-errors (save-excursion (,next))) (,self))))

(defun eglot--glob-emit-? (_ self next)
  `(,self () (re-search-forward "\\=[^/]") (,next)))

(defun eglot--glob-emit-{} (arg self next)
  (let ((alternatives (split-string (substring arg 1 (1- (length arg))) ",")))
    `(,self ()
            (or (re-search-forward ,(concat "\\=" (regexp-opt alternatives)) nil t)
                (error "Failed matching any of %s" ',alternatives))
            (,next))))

(defun eglot--glob-emit-range (arg self next)
  (when (eq ?! (aref arg 1)) (aset arg 1 ?^))
  `(,self () (re-search-forward ,(concat "\\=" arg)) (,next)))


;;; List connections mode

(define-derived-mode eglot-list-connections-mode  tabulated-list-mode
  "" "Eglot mode for listing server connections
\\{eglot-list-connections-mode-map}"
  (setq-local tabulated-list-format
              `[("Language server" 16) ("Project name" 16) ("Modes handled" 16)])
  (tabulated-list-init-header))

(defun eglot-list-connections ()
  "List currently active Eglot connections."
  (interactive)
  (with-current-buffer
      (get-buffer-create "*EGLOT connections*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eglot-list-connections-mode)
      (setq-local tabulated-list-entries
                  (mapcar
                   (lambda (server)
                     (list server
                           `[,(or (plist-get (eglot--server-info server) :name)
                                  (jsonrpc-name server))
                             ,(eglot-project-nickname server)
                             ,(mapconcat #'symbol-name
                                         (eglot--major-modes server)
                                         ", ")]))
                   (cl-reduce #'append
                              (hash-table-values eglot--servers-by-project))))
      (revert-buffer)
      (pop-to-buffer (current-buffer)))))


;;; Inlay hints
(defface eglot-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays.")

(defface eglot-type-hint-face '((t (:inherit eglot-inlay-hint-face)))
  "Face used for type inlay hint overlays.")

(defface eglot-parameter-hint-face '((t (:inherit eglot-inlay-hint-face)))
  "Face used for parameter inlay hint overlays.")

(defvar-local eglot--outstanding-inlay-hints-region (cons nil nil)
  "Jit-lock-calculated (FROM . TO) region with potentially outdated hints")

(defvar-local eglot--outstanding-inlay-hints-last-region nil)

(defvar-local eglot--outstanding-inlay-regions-timer nil
  "Helper timer for `eglot--update-hints'")

(defun eglot--update-hints (from to)
  "Jit-lock function for Eglot inlay hints."
  (cl-symbol-macrolet ((region eglot--outstanding-inlay-hints-region)
                       (last-region eglot--outstanding-inlay-hints-last-region)
                       (timer eglot--outstanding-inlay-regions-timer))
    (setcar region (min (or (car region) (point-max)) from))
    (setcdr region (max (or (cdr region) (point-min)) to))
    ;; HACK: We're relying on knowledge of jit-lock internals here.  The
    ;; condition comparing `jit-lock-context-unfontify-pos' to
    ;; `point-max' is a heuristic for telling whether this call to
    ;; `jit-lock-functions' happens after `jit-lock-context-timer' has
    ;; just run.  Only after this delay should we start the smoothing
    ;; timer that will eventually call `eglot--update-hints-1' with the
    ;; coalesced region.  I wish we didn't need the timer, but sometimes
    ;; a lot of "non-contextual" calls come in all at once and do verify
    ;; the condition.  Notice it is a 0 second timer though, so we're
    ;; not introducing any more delay over jit-lock's timers.
    (when (= jit-lock-context-unfontify-pos (point-max))
      (if timer (cancel-timer timer))
      (let ((buf (current-buffer)))
        (setq timer (run-at-time
                     0 nil
                     (lambda ()
                       (eglot--when-live-buffer buf
                         ;; HACK: In some pathological situations
                         ;; (Emacs's own coding.c, for example),
                         ;; jit-lock is calling `eglot--update-hints'
                         ;; repeatedly with same sequence of
                         ;; arguments, which leads to
                         ;; `eglot--update-hints-1' being called with
                         ;; the same region repeatedly.  This happens
                         ;; even if the hint-painting code does
                         ;; nothing else other than widen, narrow,
                         ;; move point then restore these things.
                         ;; Possible Emacs bug, but this fixes it.
                         (unless (equal last-region region)
                           (eglot--update-hints-1 (max (car region) (point-min))
                                                  (min (cdr region) (point-max)))
                           (setq last-region region))
                         (setq region (cons nil nil)
                               timer nil)))))))))

(defun eglot--update-hints-1 (from to)
  "Do most work for `eglot--update-hints', including LSP request."
  (let* ((buf (current-buffer))
         (paint-hint
          (eglot--lambda ((InlayHint) position kind label paddingLeft paddingRight)
            (goto-char (eglot--lsp-position-to-point position))
            (when (or (> (point) to) (< (point) from)) (cl-return))
            (let* ((left-pad (and paddingLeft
                                  (not (eq paddingLeft :json-false))
                                  (not (memq (char-before) '(32 9))) " "))
                   (right-pad (and paddingRight
                                   (not (eq paddingRight :json-false))
                                   (not (memq (char-after) '(32 9))) " "))
                   (peg-after-p (eql kind 1)))
              (cl-labels
                  ((make-ov ()
                     (if peg-after-p
                         (make-overlay (point) (1+ (point)) nil t)
                       (make-overlay (1- (point)) (point) nil nil nil)))
                   (do-it (label lpad rpad i n)
                     (let* ((firstp (zerop i))
                            (tweak-cursor-p (and firstp peg-after-p))
                            (ov (make-ov))
                            (text (concat lpad label rpad)))
                       (when tweak-cursor-p (put-text-property 0 1 'cursor 1 text))
                       (overlay-put ov (if peg-after-p 'before-string 'after-string)
                                    (propertize
                                     text
                                     'face (pcase kind
                                             (1 'eglot-type-hint-face)
                                             (2 'eglot-parameter-hint-face)
                                             (_ 'eglot-inlay-hint-face))))
                       (overlay-put ov 'priority (if peg-after-p i (- n i)))
                       (overlay-put ov 'eglot--inlay-hint t)
                       (overlay-put ov 'evaporate t)
                       (overlay-put ov 'eglot--overlay t))))
                (if (stringp label) (do-it label left-pad right-pad 0 1)
                  (cl-loop
                   for i from 0 for ldetail across label
                   do (eglot--dbind ((InlayHintLabelPart) value) ldetail
                        (do-it value
                               (and (zerop i) left-pad)
                               (and (= i (1- (length label))) right-pad)
                               i (length label))))))))))
    (jsonrpc-async-request
     (eglot--current-server-or-lose)
     :textDocument/inlayHint
     (list :textDocument (eglot--TextDocumentIdentifier)
           :range (list :start (eglot--pos-to-lsp-position from)
                        :end (eglot--pos-to-lsp-position to)))
     :success-fn (lambda (hints)
                   (eglot--when-live-buffer buf
                     (eglot--widening
                      ;; Overlays ending right at FROM with an
                      ;; `after-string' property logically belong to
                      ;; the (FROM TO) region.  Likewise, such
                      ;; overlays ending at TO don't logically belong
                      ;; to it.
                      (dolist (o (overlays-in (1- from) to))
                        (when (and (overlay-get o 'eglot--inlay-hint)
                                   (cond ((eq (overlay-end o) from)
                                          (overlay-get o 'after-string))
                                         ((eq (overlay-end o) to)
                                          (overlay-get o 'before-string))
                                         (t)))
                          (delete-overlay o)))
                      (mapc paint-hint hints))))
     :deferred 'eglot--update-hints-1)))

(define-minor-mode eglot-inlay-hints-mode
  "Minor mode for annotating buffers with LSP server's inlay hints."
  :global nil
  (cond (eglot-inlay-hints-mode
         (if (eglot-server-capable :inlayHintProvider)
             (jit-lock-register #'eglot--update-hints 'contextual)
           (eglot-inlay-hints-mode -1)))
        (t
         (jit-lock-unregister #'eglot--update-hints)
         (remove-overlays nil nil 'eglot--inlay-hint t))))


;;; Hacks
;;;
;; Emacs bug#56407, the optimal solution is in desktop.el, but that's
;; harder. For now, use `with-eval-after-load'. See also github#1183.
(with-eval-after-load 'desktop
  (add-to-list 'desktop-minor-mode-handlers '(eglot--managed-mode . ignore))
  (add-to-list 'desktop-minor-mode-handlers '(eglot-inlay-hints-mode . ignore)))


;;; Misc
;;;
;;;###autoload
(progn
  (put 'eglot--debbugs-or-github-bug-uri 'bug-reference-url-format t)
  (defun eglot--debbugs-or-github-bug-uri ()
    (format (if (string= (match-string 2) "github")
                "https://github.com/joaotavora/eglot/issues/%s"
              "https://debbugs.gnu.org/%s")
            (match-string 3))))

(provide 'eglot)


;; Local Variables:
;; bug-reference-bug-regexp: "\\(\\(github\\|bug\\)#\\([0-9]+\\)\\)"
;; bug-reference-url-format: eglot--debbugs-or-github-bug-uri
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; eglot.el ends here
