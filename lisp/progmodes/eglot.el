;;; eglot.el --- Client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Version: 1.8
;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (jsonrpc "1.0.14") (flymake "1.2.1") (project "0.3.0") (xref "1.0.1") (eldoc "1.11.0") (seq "2.23"))

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
;; Typing M-x eglot should be enough to get you started, but here's a
;; little info (see the accompanying README.md or the URL for more).
;;
;; M-x eglot starts a server via a shell command guessed from
;; `eglot-server-programs', using the current major mode (for whatever
;; language you're programming in) as a hint.  If it can't guess, it
;; prompts you in the minibuffer for these things.  Actually, the
;; server does not need to be running locally: you can connect to a
;; running server via TCP by entering a <host:port> syntax.
;;
;; If the connection is successful, you should see an `eglot'
;; indicator pop up in your mode-line.  More importantly, this means
;; that current *and future* file buffers of that major mode *inside
;; your current project* automatically become \"managed\" by the LSP
;; server.  In other words, information about their content is
;; exchanged periodically to provide enhanced code analysis using
;; `xref-find-definitions', `flymake-mode', `eldoc-mode',
;; `completion-at-point', among others.
;;
;; To "unmanage" these buffers, shutdown the server with
;; M-x eglot-shutdown.
;;
;; To start an eglot session automatically when a foo-mode buffer is
;; visited, you can put this in your init file:
;;
;;   (add-hook 'foo-mode-hook 'eglot-ensure)

;;; Code:

(require 'json)
(require 'imenu)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile) ; for some faces
(require 'warnings)
(require 'flymake)
(require 'xref)
(eval-when-compile
  (require 'subr-x))
(require 'jsonrpc)
(require 'filenotify)
(require 'ert)
(require 'array)

;; ElDoc is preloaded in Emacs, so `require'-ing won't guarantee we are
;; using the latest version from GNU Elpa when we load eglot.el.  Use an
;; heuristic to see if we need to `load' it in Emacs < 28.
(if (and (< emacs-major-version 28)
         (not (boundp 'eldoc-documentation-strategy)))
    (load "eldoc")
  (require 'eldoc))

;; Similar issue as above for Emacs 26.3 and seq.el.
(if (< emacs-major-version 27)
    (load "seq")
  (require 'seq))

;; forward-declare, but don't require (Emacs 28 doesn't seem to care)
(defvar markdown-fontify-code-blocks-natively)
(defvar company-backends)
(defvar company-tooltip-align-annotations)



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
                             "[eglot] More than one server executable available:"
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

(defvar eglot-server-programs `((rust-mode . ,(eglot-alternatives '("rust-analyzer" "rls")))
                                (cmake-mode . ("cmake-language-server"))
                                (vimrc-mode . ("vim-language-server" "--stdio"))
                                (python-mode
                                 . ,(eglot-alternatives
                                     '("pylsp" "pyls" ("pyright-langserver" "--stdio"))))
                                ((js-mode typescript-mode)
                                 . ("typescript-language-server" "--stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                ((php-mode phps-mode)
                                 . ("php" "vendor/felixfbecker/\
language-server/bin/php-language-server.php"))
                                ((c++-mode c-mode) . ,(eglot-alternatives
                                                       '("clangd" "ccls")))
                                (((caml-mode :language-id "ocaml")
                                  (tuareg-mode :language-id "ocaml") reason-mode)
                                 . ("ocamllsp"))
                                (ruby-mode
                                 . ("solargraph" "socket" "--port" :autoport))
                                (haskell-mode
                                 . ("haskell-language-server-wrapper" "--lsp"))
                                (elm-mode . ("elm-language-server"))
                                (mint-mode . ("mint" "ls"))
                                (kotlin-mode . ("kotlin-language-server"))
                                (go-mode . ("gopls"))
                                ((R-mode ess-r-mode) . ("R" "--slave" "-e"
                                                        "languageserver::run()"))
                                (java-mode . eglot--eclipse-jdt-contact)
                                (dart-mode . ("dart_language_server"))
                                (elixir-mode . ("language_server.sh"))
                                (ada-mode . ("ada_language_server"))
                                (scala-mode . ("metals-emacs"))
                                (racket-mode . ("racket" "-l" "racket-langserver"))
                                ((tex-mode context-mode texinfo-mode bibtex-mode)
                                 . ("digestif"))
                                (erlang-mode . ("erlang_ls" "--transport" "stdio"))
                                (yaml-mode . ("yaml-language-server" "--stdio"))
                                (nix-mode . ("rnix-lsp"))
                                (gdscript-mode . ("localhost" 6008))
                                ((fortran-mode f90-mode) . ("fortls"))
                                (lua-mode . ("lua-lsp"))
                                (zig-mode . ("zls"))
                                (css-mode . ,(eglot-alternatives '(("vscode-css-language-server" "--stdio") ("css-languageserver" "--stdio"))))
                                (html-mode . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio") ("html-languageserver" "--stdio"))))
                                (json-mode . ,(eglot-alternatives '(("vscode-json-language-server" "--stdio") ("json-languageserver" "--stdio"))))
                                (dockerfile-mode . ("docker-langserver" "--stdio"))
                                (clojure-mode . ("clojure-lsp")))
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
  program.

CONTACT can be:

* In the most common case, a list of strings (PROGRAM [ARGS...]).
  PROGRAM is called with ARGS and is expected to serve LSP requests
  over the standard input/output channels.

* A list (HOST PORT [TCP-ARGS...]) where HOST is a string and
  PORT is a positive integer for connecting to a server via TCP.
  Remaining ARGS are passed to `open-network-stream' for
  upgrading the connection with encryption or other capabilities.

* A list (PROGRAM [ARGS...] :autoport [MOREARGS...]), whereupon a
  combination of the two previous options is used.  First, an
  attempt is made to find an available server port, then PROGRAM
  is launched with ARGS; the `:autoport' keyword substituted for
  that number; and MOREARGS.  Eglot then attempts to establish a
  TCP connection to that port number on the localhost.

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
  an error if it can't produce a valid CONTACT.")

(defface eglot-highlight-symbol-face
  '((t (:inherit bold)))
  "Face used to highlight the symbol at point.")

(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in EGLOT's mode line.")

(defface eglot-diagnostic-tag-unnecessary-face
  '((t . (:weight ultra-light)))
  "Face used to render unused or unnecessary code.")

(defface eglot-diagnostic-tag-deprecated-face
  '((t . (:strike-through t)))
  "Face used to render deprecated or obsolete code.")

(defcustom eglot-autoreconnect 3
  "Control ability to reconnect automatically to the LSP server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))

(defcustom eglot-connect-timeout 30
  "Number of seconds before timing out LSP connection attempts.
If nil, never time out."
  :type 'number)

(defcustom eglot-sync-connect 3
  "Control blocking of LSP connection attempts.
If t, block for `eglot-connect-timeout' seconds.  A positive
integer number means block for that many seconds, and then wait
for the connection in the background.  nil has the same meaning
as 0, i.e. don't block at all."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))

(defcustom eglot-autoshutdown nil
  "If non-nil, shut down server after killing last managed buffer."
  :type 'boolean)

(defcustom eglot-send-changes-idle-time 0.5
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :type 'number)

(defcustom eglot-events-buffer-size 2000000
  "Control the size of the Eglot events buffer.
If a number, don't let the buffer grow larger than that many
characters.  If 0, don't use an event's buffer at all.  If nil,
let the buffer grow forever."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Number of characters")))

(defcustom eglot-confirm-server-initiated-edits 'confirm
  "Non-nil if server-initiated edits should be confirmed with user."
  :type '(choice (const :tag "Don't show confirmation prompt" nil)
                 (symbol :tag "Show confirmation prompt" 'confirm)))

(defcustom eglot-extend-to-xref nil
  "If non-nil, activate Eglot in cross-referenced non-project files."
  :type 'boolean)

(defvar eglot-withhold-process-id nil
  "If non-nil, Eglot will not send the Emacs process id to the language server.
This can be useful when using docker to run a language server.")

;; Customizable via `completion-category-overrides'.
(when (assoc 'flex completion-styles-alist)
  (add-to-list 'completion-category-defaults '(eglot (styles flex basic))))


;;; Constants
;;;
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

(defconst eglot--{} (make-hash-table) "The empty JSON object.")

(defun eglot--executable-find (command &optional remote)
  "Like Emacs 27's `executable-find', ignore REMOTE on Emacs 26."
  (if (>= emacs-major-version 27) (executable-find command remote)
    (executable-find command)))


;;; Message verification helpers
;;;
(eval-and-compile
  (defvar eglot--lsp-interface-alist
    `(
      (CodeAction (:title) (:kind :diagnostics :edit :command :isPreferred))
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
                      ;; `:containerName' isn't really allowed , but
                      ;; it simplifies the impl of `eglot-imenu'.
                      (:detail :deprecated :children :containerName))
      (TextDocumentEdit (:textDocument :edits) ())
      (TextEdit (:range :newText))
      (VersionedTextDocumentIdentifier (:uri :version) ())
      (WorkspaceEdit () (:changes :documentChanges))
      )
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
  (defvar eglot-strict-mode (if load-file-name '()
                              '(disallow-non-standard-keys
                                ;; Uncomment these two for fun at
                                ;; compile-time or with flymake-mode.
                                ;;
                                ;; enforce-required-keys
                                ;; enforce-optional-keys
                                ))
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
on unknown notifications and errors on unknown requests."))

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
                        interface-name )))
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
            (t
             (byte-compile-warn "Unknown LSP interface %s" interface-name))))))

(cl-defmacro eglot--dbind (vars object &body body)
  "Destructure OBJECT, binding VARS in BODY.
VARS is ([(INTERFACE)] SYMS...)
Honour `eglot-strict-mode'."
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
Honour `eglot-strict-mode'."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (eglot--dbind ,cl-lambda-list ,e ,@body))))

(cl-defmacro eglot--dcase (obj &rest clauses)
  "Like `pcase', but for the LSP object OBJ.
CLAUSES is a list (DESTRUCTURE FORMS...) where DESTRUCTURE is
treated as in `eglot-dbind'."
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


;;; API (WORK-IN-PROGRESS!)
;;;
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

(cl-defgeneric eglot-handle-request (server method &rest params)
  "Handle SERVER's METHOD request with PARAMS.")

(cl-defgeneric eglot-handle-notification (server method &rest params)
  "Handle SERVER's METHOD notification with PARAMS.")

(cl-defgeneric eglot-execute-command (server command arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(cl-defgeneric eglot-initialization-options (server)
  "JSON object to send under `initializationOptions'."
  (:method (_s) eglot--{})) ; blank default

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
  "What the EGLOT LSP client supports for SERVER."
  (:method (_s)
           (list
            :workspace (list
                        :applyEdit t
                        :executeCommand `(:dynamicRegistration :json-false)
                        :workspaceEdit `(:documentChanges :json-false)
                        :didChangeWatchedFiles `(:dynamicRegistration t)
                        :symbol `(:dynamicRegistration :json-false)
                        :configuration t)
            :textDocument
            (list
             :synchronization (list
                               :dynamicRegistration :json-false
                               :willSave t :willSaveWaitUntil t :didSave t)
             :completion      (list :dynamicRegistration :json-false
                                    :completionItem
                                    `(:snippetSupport
                                      ,(if (eglot--snippet-expansion-fn)
                                           t
                                         :json-false)
                                      :deprecatedSupport t
                                      :tagSupport (:valueSet [1]))
                                    :contextSupport t)
             :hover              (list :dynamicRegistration :json-false
                                       :contentFormat
                                       (if (fboundp 'gfm-view-mode)
                                           ["markdown" "plaintext"]
                                         ["plaintext"]))
             :signatureHelp      (list :dynamicRegistration :json-false
                                       :signatureInformation
                                       `(:parameterInformation
                                         (:labelOffsetSupport t)
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
             :publishDiagnostics (list :relatedInformation :json-false
                                       ;; TODO: We can support :codeDescription after
                                       ;; adding an appropriate UI to
                                       ;; Flymake.
                                       :codeDescriptionSupport :json-false
                                       :tagSupport
                                       `(:valueSet
                                         [,@(mapcar
                                             #'car eglot--tag-faces)])))
            :experimental eglot--{})))

(defclass eglot-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor eglot--project-nickname
    :reader eglot-project-nickname)
   (major-mode
    :documentation "Major mode symbol."
    :accessor eglot--major-mode)
   (language-id
    :documentation "Language ID string for the mode."
    :accessor eglot--language-id)
   (capabilities
    :documentation "JSON object containing server capabilities."
    :accessor eglot--capabilities)
   (server-info
    :documentation "JSON object containing server info."
    :accessor eglot--server-info)
   (shutdown-requested
    :documentation "Flag set when server is shutting down."
    :accessor eglot--shutdown-requested)
   (project
    :documentation "Project associated with server."
    :accessor eglot--project)
   (spinner
    :documentation "List (ID DOING-WHAT DONE-P) representing server progress."
    :initform `(nil nil t) :accessor eglot--spinner)
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor eglot--inhibit-autoreconnect)
   (file-watches
    :documentation "Map ID to list of WATCHES for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor eglot--file-watches)
   (managed-buffers
    :documentation "List of buffers managed by server."
    :accessor eglot--managed-buffers)
   (saved-initargs
    :documentation "Saved initargs for reconnection purposes."
    :accessor eglot--saved-initargs)
   (inferior-process
    :documentation "Server subprocess started automatically."
    :accessor eglot--inferior-process))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")


;;; Process management
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
        (jsonrpc-request server :shutdown nil :timeout (or timeout 1.5))
        (jsonrpc-notify server :exit nil))
    ;; Now ask jsonrpc.el to shut down the server.
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers (kill-buffer (jsonrpc-events-buffer server)))))

(defun eglot-shutdown-all (&optional preserve-buffers)
  "Politely ask all language servers to quit, in order.
PRESERVE-BUFFERS as in `eglot-shutdown', which see."
  (interactive (list current-prefix-arg))
  (cl-loop for ss being the hash-values of eglot--servers-by-project
           do (cl-loop for s in ss do (eglot-shutdown s nil preserve-buffers))))

(defun eglot--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `eglot--managed-mode' where appropriate.
  (dolist (buffer (eglot--managed-buffers server))
    (let (;; Avoid duplicate shutdowns (github#389)
          (eglot-autoshutdown nil))
      (eglot--when-live-buffer buffer (eglot--managed-mode-off))))
  ;; Kill any expensive watches
  (maphash (lambda (_id watches)
             (mapcar #'file-notify-rm-watch watches))
           (eglot--file-watches server))
  ;; Kill any autostarted inferior processes
  (when-let (proc (eglot--inferior-process server))
    (delete-process proc))
  ;; Sever the project/server relationship for `server'
  (setf (gethash (eglot--project server) eglot--servers-by-project)
        (delq server
              (gethash (eglot--project server) eglot--servers-by-project)))
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

(defvar eglot--command-history nil
  "History of CONTACT arguments to `eglot'.")

(defun eglot--lookup-mode (mode)
  "Lookup `eglot-server-programs' for MODE.
Return (LANGUAGE-ID . CONTACT-PROXY).  If not specified,
LANGUAGE-ID is determined from MODE."
  (cl-loop
   for (modes . contact) in eglot-server-programs
   thereis (cl-some
            (lambda (spec)
              (cl-destructuring-bind (probe &key language-id &allow-other-keys)
                  (if (consp spec) spec (list spec))
                (and (provided-mode-derived-p mode probe)
                     (cons
                      (or language-id
                          (or (get mode 'eglot-language-id)
                              (get spec 'eglot-language-id)
                              (string-remove-suffix "-mode" (symbol-name mode))))
                      contact))))
            (if (or (symbolp modes) (keywordp (cadr modes)))
                (list modes) modes))))

(defun eglot--guess-contact (&optional interactive)
  "Helper for `eglot'.
Return (MANAGED-MODE PROJECT CLASS CONTACT LANG-ID).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((guessed-mode (if buffer-file-name major-mode))
         (managed-mode
          (cond
           ((and interactive
                 (or (>= (prefix-numeric-value current-prefix-arg) 16)
                     (not guessed-mode)))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              (symbol-name guessed-mode) nil (symbol-name guessed-mode) nil)))
           ((not guessed-mode)
            (eglot--error "Can't guess mode to manage for `%s'" (current-buffer)))
           (t guessed-mode)))
         (lang-id-and-guess (eglot--lookup-mode guessed-mode))
         (language-id (car lang-id-and-guess))
         (guess (cdr lang-id-and-guess))
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
         (program-guess
          (and program
               (combine-and-quote-strings (cl-subst ":autoport:"
                                                    :autoport guess))))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[eglot] Sorry, couldn't guess for `%s'!\n%s"
                              managed-mode base-prompt))
                     ((and program
                           (not (file-name-absolute-p program))
                           (not (eglot--executable-find program t)))
                      (concat (format "[eglot] I guess you want to run `%s'"
                                      program-guess)
                              (format ", but I can't find `%s' in PATH!" program)
                              "\n" base-prompt)))))
         (contact
          (or (and prompt
                   (let ((s (read-shell-command
                             prompt
                             program-guess
                             'eglot-command-history)))
                     (if (string-match "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                       (string-trim s))
                         (list (match-string 1 s)
                               (string-to-number (match-string 2 s)))
                       (cl-subst
                        :autoport ":autoport:" (split-string-and-unquote s)
                        :test #'equal))))
              guess
              (eglot--error "Couldn't guess for `%s'!" managed-mode))))
    (list managed-mode (eglot--current-project) class contact language-id)))

(defvar eglot-lsp-context)
(put 'eglot-lsp-context 'variable-documentation
     "Dynamically non-nil when searching for projects in LSP context.")

(defvar eglot--servers-by-xrefed-file
  (make-hash-table :test 'equal :weakness 'value))

(defun eglot--current-project ()
  "Return a project object for Eglot's LSP purposes.
This relies on `project-current' and thus on
`project-find-functions'.  Functions in the latter
variable (which see) can query the value `eglot-lsp-context' to
decide whether a given directory is a project containing a
suitable root directory for a given LSP server's purposes."
  (let ((eglot-lsp-context t))
    (or (project-current) `(transient . ,default-directory))))

;;;###autoload
(defun eglot (managed-major-mode project class contact language-id
                                 &optional interactive)
  "Manage a project with a Language Server Protocol (LSP) server.

The LSP server of CLASS is started (or contacted) via CONTACT.
If this operation is successful, current *and future* file
buffers of MANAGED-MAJOR-MODE inside PROJECT become \"managed\"
by the LSP server, meaning information about their contents is
exchanged periodically to provide enhanced code-analysis via
`xref-find-definitions', `flymake-mode', `eldoc-mode',
`completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CLASS and CONTACT from
`eglot-server-programs' and PROJECT from
`project-find-functions'.  The search for active projects in this
context binds `eglot-lsp-context' (which see).

If it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.

PROJECT is a project object as returned by `project-current'.

CLASS is a subclass of `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

LANGUAGE-ID is the language ID string to send to the server for
MANAGED-MAJOR-MODE, which matters to a minority of servers.

INTERACTIVE is t if called interactively."
  (interactive (append (eglot--guess-contact t) '(t)))
  (let* ((current-server (eglot-current-server))
         (live-p (and current-server (jsonrpc-running-p current-server))))
    (if (and live-p
             interactive
             (y-or-n-p "[eglot] Live process found, reconnect instead? "))
        (eglot-reconnect current-server interactive)
      (when live-p (ignore-errors (eglot-shutdown current-server)))
      (eglot--connect managed-major-mode project class contact language-id))))

(defun eglot-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-server-or-lose) t))
  (when (jsonrpc-running-p server)
    (ignore-errors (eglot-shutdown server interactive nil 'preserve-buffers)))
  (eglot--connect (eglot--major-mode server)
                  (eglot--project server)
                  (eieio-object-class-name server)
                  (eglot--saved-initargs server)
                  (eglot--language-id server))
  (eglot--message "Reconnected!"))

(defvar eglot--managed-mode) ; forward decl

;;;###autoload
(defun eglot-ensure ()
  "Start Eglot session for current buffer if there isn't one."
  (let ((buffer (current-buffer)))
    (cl-labels
        ((maybe-connect
          ()
          (remove-hook 'post-command-hook #'maybe-connect nil)
          (eglot--when-live-buffer buffer
            (unless eglot--managed-mode
              (apply #'eglot--connect (eglot--guess-contact))))))
      (when buffer-file-name
        (add-hook 'post-command-hook #'maybe-connect 'append nil)))))

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
      (list "sh" "-c"
            (string-join (cons "stty raw > /dev/null;"
                               (mapcar #'shell-quote-argument contact))
             " "))
    contact))

(defvar-local eglot--cached-server nil
  "A cached reference to the current EGLOT server.")

(defun eglot--connect (managed-major-mode project class contact language-id)
  "Connect to MANAGED-MAJOR-MODE, LANGUAGE-ID, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (project-root project))
         (nickname (file-name-base (directory-file-name default-directory)))
         (readable-name (format "EGLOT (%s/%s)" nickname managed-major-mode))
         autostart-inferior-process
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
                ((and (stringp (car contact)) (memq :autoport contact))
                 (setq server-info (list "<inferior process>"))
                 `(:process ,(lambda ()
                               (pcase-let ((`(,connection . ,inferior)
                                            (eglot--inferior-bootstrap
                                             readable-name
                                             contact)))
                                 (setq autostart-inferior-process inferior)
                                 connection))))
                ((stringp (car contact))
                 `(:process
                   ,(lambda ()
                      (let ((default-directory default-directory))
                        (make-process
                         :name readable-name
                         :command (setq server-info (eglot--cmd contact))
                         :connection-type 'pipe
                         :coding 'utf-8-emacs-unix
                         :noquery t
                         :stderr (get-buffer-create
                                  (format "*%s stderr*" readable-name))
                         :file-handler t)))))))
         (spread (lambda (fn) (lambda (server method params)
                                (let ((eglot--cached-server server))
                                 (apply fn server method (append params nil))))))
         (server
          (apply
           #'make-instance class
           :name readable-name
           :events-buffer-scrollback-size eglot-events-buffer-size
           :notification-dispatcher (funcall spread #'eglot-handle-notification)
           :request-dispatcher (funcall spread #'eglot-handle-request)
           :on-shutdown #'eglot--on-shutdown
           initargs))
         (cancelled nil)
         (tag (make-symbol "connected-catch-tag")))
    (when server-info
      (jsonrpc--debug server "Running language server: %s"
                      (string-join server-info " ")))
    (setf (eglot--saved-initargs server) initargs)
    (setf (eglot--project server) project)
    (setf (eglot--project-nickname server) nickname)
    (setf (eglot--major-mode server) managed-major-mode)
    (setf (eglot--language-id server) language-id)
    (setf (eglot--inferior-process server) autostart-inferior-process)
    (run-hook-with-args 'eglot-server-initialized-hook server)
    ;; Now start the handshake.  To honour `eglot-sync-connect'
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
                            ;; Maybe turn trampy `/ssh:foo@bar:/path/to/baz.py'
                            ;; into `/path/to/baz.py', so LSP groks it.
                            :rootPath (file-local-name
                                       (expand-file-name default-directory))
                            :rootUri (eglot--path-to-uri default-directory)
                            :initializationOptions (eglot-initialization-options
                                                    server)
                            :capabilities (eglot-client-capabilities server))
                      :success-fn
                      (eglot--lambda ((InitializeResult) capabilities serverInfo)
                        (unless cancelled
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
                          (let ((default-directory (project-root project))
                                (major-mode managed-major-mode))
                            (hack-dir-local-variables-non-file-buffer)
                            (run-hook-with-args 'eglot-connect-hook server))
                          (eglot--message
                           "Connected! Server `%s' now managing `%s' buffers \
in project `%s'."
                           (or (plist-get serverInfo :name)
                               (jsonrpc-name server))
                           managed-major-mode
                           (eglot-project-nickname server))
                          (when tag (throw tag t))))
                      :timeout eglot-connect-timeout
                      :error-fn (eglot--lambda ((ResponseError) code message)
                                  (unless cancelled
                                    (jsonrpc-shutdown server)
                                    (let ((msg (format "%s: %s" code message)))
                                      (if tag (throw tag `(error . ,msg))
                                        (eglot--error msg)))))
                      :timeout-fn (lambda ()
                                    (unless cancelled
                                      (jsonrpc-shutdown server)
                                      (let ((msg (format "Timed out after %s seconds"
                                                         eglot-connect-timeout)))
                                        (if tag (throw tag `(error . ,msg))
                                          (eglot--error msg))))))
                     (cond ((numberp eglot-sync-connect)
                            (accept-process-output nil eglot-sync-connect))
                           (eglot-sync-connect
                            (while t (accept-process-output nil 30)))))))
              (pcase retval
                (`(error . ,msg) (eglot--error msg))
                (`nil (eglot--message "Waiting in background for server `%s'"
                                      (jsonrpc-name server))
                      nil)
                (_ server)))
          (quit (jsonrpc-shutdown server) (setq cancelled 'quit)))
      (setq tag nil))))

(defun eglot--inferior-bootstrap (name contact &optional connect-args)
  "Use CONTACT to start a server, then connect to it.
Return a cons of two process objects (CONNECTION . INFERIOR).
Name both based on NAME.
CONNECT-ARGS are passed as additional arguments to
`open-network-stream'."
  (let* ((port-probe (make-network-process :name "eglot-port-probe-dummy"
                                           :server t
                                           :host "localhost"
                                           :service 0))
         (port-number (unwind-protect
                          (process-contact port-probe :service)
                        (delete-process port-probe)))
         inferior connection)
    (unwind-protect
        (progn
          (setq inferior
                (make-process
                 :name (format "autostart-inferior-%s" name)
                 :stderr (format "*%s stderr*" name)
                 :noquery t
                 :command (cl-subst
                           (format "%s" port-number) :autoport contact)))
          (setq connection
                (cl-loop
                 repeat 10 for i from 1
                 do (accept-process-output nil 0.5)
                 while (process-live-p inferior)
                 do (eglot--message
                     "Trying to connect to localhost and port %s (attempt %s)"
                     port-number i)
                 thereis (ignore-errors
                           (apply #'open-network-stream
                                  (format "autoconnect-%s" name)
                                  nil
                                  "localhost" port-number connect-args))))
          (cons connection inferior))
      (cond ((and (process-live-p connection)
                  (process-live-p inferior))
             (eglot--message "Done, connected to %s!" port-number))
            (t
             (when inferior (delete-process inferior))
             (when connection (delete-process connection))
             (eglot--error "Could not start and connect to server%s"
                           (if inferior
                               (format " started with %s"
                                       (process-command inferior))
                             "!")))))))


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

(defun eglot-current-column () (- (point) (point-at-bol)))

(defvar eglot-current-column-function #'eglot-lsp-abiding-column
  "Function to calculate the current column.

This is the inverse operation of
`eglot-move-to-column-function' (which see).  It is a function of
no arguments returning a column number.  For buffers managed by
fully LSP-compliant servers, this should be set to
`eglot-lsp-abiding-column' (the default), and
`eglot-current-column' for all others.")

(defun eglot-lsp-abiding-column (&optional lbp)
  "Calculate current COLUMN as defined by the LSP spec.
LBP defaults to `line-beginning-position'."
  (/ (- (length (encode-coding-region (or lbp (line-beginning-position))
                                      (point) 'utf-16 t))
        2)
     2))

(defun eglot--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (eglot--widening
   (list :line (1- (line-number-at-pos pos t)) ; F!@&#$CKING OFF-BY-ONE
         :character (progn (when pos (goto-char pos))
                           (funcall eglot-current-column-function)))))

(defvar eglot-move-to-column-function #'eglot-move-to-lsp-abiding-column
  "Function to move to a column reported by the LSP server.

According to the standard, LSP column/character offsets are based
on a count of UTF-16 code units, not actual visual columns.  So
when LSP says position 3 of a line containing just \"aXbc\",
where X is a multi-byte character, it actually means `b', not
`c'. However, many servers don't follow the spec this closely.

For buffers managed by fully LSP-compliant servers, this should
be set to `eglot-move-to-lsp-abiding-column' (the default), and
`eglot-move-to-column' for all others.")

(defun eglot-move-to-column (column)
  "Move to COLUMN without closely following the LSP spec."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP columns in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (line-beginning-position) column)
                  (line-end-position))))

(defun eglot-move-to-lsp-abiding-column (column)
  "Move to COLUMN abiding by the LSP spec."
  (save-restriction
    (cl-loop
     with lbp = (line-beginning-position)
     initially
     (narrow-to-region lbp (line-end-position))
     (move-to-column column)
     for diff = (- column
                   (eglot-lsp-abiding-column lbp))
     until (zerop diff)
     do (condition-case eob-err
            (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))
          (end-of-buffer (cl-return eob-err))))))

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
        (let ((tab-width 1)
              (col (plist-get pos-plist :character)))
          (unless (wholenump col)
            (eglot--warn
             "Caution: LSP server sent invalid character position %s. Using 0 instead."
             col)
            (setq col 0))
          (funcall eglot-move-to-column-function col)))
      (if marker (copy-marker (point-marker)) (point)))))

(defconst eglot--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil) ;; see github#639
    vec)
  "Like `url-path-allows-chars' but more restrictive.")

(defun eglot--path-to-uri (path)
  "URIfy PATH."
  (let ((truepath (file-truename path)))
    (concat "file://"
            ;; Add a leading "/" for local MS Windows-style paths.
            (if (and (eq system-type 'windows-nt)
                     (not (file-remote-p truepath)))
                "/")
            (url-hexify-string
             ;; Again watch out for trampy paths.
             (directory-file-name (file-local-name truepath))
             eglot--uri-path-allowed-chars))))

(defun eglot--uri-to-path (uri)
  "Convert URI to file path, helped by `eglot--current-server'."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let* ((server (eglot-current-server))
         (remote-prefix (and server
                             (file-remote-p
                              (project-root (eglot--project server)))))
         (retval (url-filename (url-generic-parse-url (url-unhex-string uri))))
         ;; Remove the leading "/" for local MS Windows-style paths.
         (normalized (if (and (not remote-prefix)
                              (eq system-type 'windows-nt)
                              (cl-plusp (length retval)))
                         (substring retval 1)
                       retval)))
    (concat remote-prefix normalized)))

(defun eglot--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'yas-expand-snippet))

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
	    (message-log-max nil))
        (ignore-errors (delay-mode-hooks (funcall mode))))
      (font-lock-ensure)
      (string-trim (filter-buffer-substring (point-min) (point-max))))))

(define-obsolete-variable-alias 'eglot-ignored-server-capabilites
  'eglot-ignored-server-capabilities "1.8")

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
          (const :tag "Go to declaration" :implementationProvider)
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
          (const :tag "Execute custom commands" :executeCommandProvider)))

(defun eglot--server-capable (&rest feats)
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

(defun eglot--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((st (plist-get range :start))
         (beg (eglot--lsp-position-to-point st markers))
         (end (eglot--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

(defun eglot--read-server (prompt &optional dont-if-just-the-one)
  "Read a running Eglot server from minibuffer using PROMPT.
If DONT-IF-JUST-THE-ONE and there's only one server, don't prompt
and just return it.  PROMPT shouldn't end with a question mark."
  (let ((servers (cl-loop for servers
                          being hash-values of eglot--servers-by-project
                          append servers))
        (name (lambda (srv)
                (format "%s/%s" (eglot-project-nickname srv)
                        (eglot--major-mode srv)))))
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
consistent Eglot behaviour and only stay in place until
\"managing\" stops (usually via `eglot-shutdown'), whereupon the
previous settings are restored.

However, if you wish for Eglot to stay out of a particular Emacs
facility that you'd like to keep control of add an element to
this list and Eglot will refrain from setting it.

For example, to keep your Company customization use

(add-to-list 'eglot-stay-out-of 'company)")

(defun eglot--stay-out-of-p (symbol)
  "Tell if EGLOT should stay of of SYMBOL."
  (cl-find (symbol-name symbol) eglot-stay-out-of
           :test (lambda (s thing)
                   (let ((re (if (symbolp thing) (symbol-name thing) thing)))
                     (string-match re s)))))

(defmacro eglot--setq-saving (symbol binding)
  `(unless (or (not (boundp ',symbol)) (eglot--stay-out-of-p ',symbol))
     (push (cons ',symbol (symbol-value ',symbol)) eglot--saved-bindings)
     (setq-local ,symbol ,binding)))

(defun eglot-managed-p ()
  "Tell if current buffer is managed by EGLOT."
  eglot--managed-mode)

(defvar eglot-managed-mode-hook nil
  "A hook run by EGLOT after it started/stopped managing a buffer.
Use `eglot-managed-p' to determine if current buffer is managed.")

(define-minor-mode eglot--managed-mode
  "Mode for source buffers managed by some EGLOT project."
  :init-value nil :lighter nil :keymap eglot-mode-map
  (cond
   (eglot--managed-mode
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
    (add-hook 'change-major-mode-hook #'eglot--managed-mode-off nil t)
    (add-hook 'post-self-insert-hook 'eglot--post-self-insert-hook nil t)
    (add-hook 'pre-command-hook 'eglot--pre-command-hook nil t)
    (eglot--setq-saving eldoc-documentation-functions
                        '(eglot-signature-eldoc-function
                          eglot-hover-eldoc-function))
    (eglot--setq-saving eldoc-documentation-strategy
                        #'eldoc-documentation-enthusiast)
    (eglot--setq-saving xref-prompt-for-identifier nil)
    (eglot--setq-saving flymake-diagnostic-functions '(eglot-flymake-backend))
    (eglot--setq-saving company-backends '(company-capf))
    (eglot--setq-saving company-tooltip-align-annotations t)
    (unless (eglot--stay-out-of-p 'imenu)
      (add-function :before-until (local 'imenu-create-index-function)
                    #'eglot-imenu))
    (unless (eglot--stay-out-of-p 'flymake) (flymake-mode 1))
    (unless (eglot--stay-out-of-p 'eldoc) (eldoc-mode 1))
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
    (remove-hook 'change-major-mode-hook #'eglot--managed-mode-off t)
    (remove-hook 'post-self-insert-hook 'eglot--post-self-insert-hook t)
    (remove-hook 'pre-command-hook 'eglot--pre-command-hook t)
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
          (eglot-shutdown server))))))
  ;; Note: the public hook runs before the internal eglot--managed-mode-hook.
  (run-hooks 'eglot-managed-mode-hook))

(defun eglot--managed-mode-off ()
  "Turn off `eglot--managed-mode' unconditionally."
  (eglot--managed-mode -1))

(defun eglot-current-server ()
  "Return logical EGLOT server for current buffer, nil if none."
  (setq eglot--cached-server
        (or eglot--cached-server
            (cl-find major-mode
                     (gethash (eglot--current-project) eglot--servers-by-project)
                     :key #'eglot--major-mode)
            (and eglot-extend-to-xref
                 buffer-file-name
                 (gethash (expand-file-name buffer-file-name)
                          eglot--servers-by-xrefed-file)))))

(defun eglot--current-server-or-lose ()
  "Return current logical EGLOT server connection or error."
  (or (eglot-current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(defvar-local eglot--unreported-diagnostics nil
  "Unreported Flymake diagnostics for this buffer.")

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
      (setq eglot--unreported-diagnostics `(:just-opened . nil))
      (eglot--managed-mode)
      (eglot--signal-textDocument/didOpen))))

(add-hook 'find-file-hook 'eglot--maybe-activate-editing-mode)
(add-hook 'after-change-major-mode-hook 'eglot--maybe-activate-editing-mode)

(defun eglot-clear-status (server)
  "Clear the last JSONRPC error for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (setf (jsonrpc-last-error server) nil))


;;; Mode-line, menu and other sugar
;;;
(defvar eglot--mode-line-format `(:eval (eglot--mode-line-format)))

(put 'eglot--mode-line-format 'risky-local-variable t)

(defun eglot--mouse-call (what)
  "Make an interactive lambda for calling WHAT from mode-line."
  (lambda (event)
    (interactive "e")
    (let ((start (event-start event))) (with-selected-window (posn-window start)
                                         (save-excursion
                                           (goto-char (or (posn-point start)
                                                          (point)))
                                           (call-interactively what)
                                           (force-mode-line-update t))))))

(defun eglot--mode-line-props (thing face defs &optional prepend)
  "Helper for function `eglot--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (eglot--mouse-call def))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize ,thing
                                         face ,face
                                         keymap ,map help-echo ,(concat prepend blurb)
                                         mouse-face mode-line-highlight))))

(defun eglot--mode-line-format ()
  "Compose the EGLOT's mode-line."
  (pcase-let* ((server (eglot-current-server))
               (nick (and server (eglot-project-nickname server)))
               (pending (and server (hash-table-count
                                     (jsonrpc--request-continuations server))))
               (`(,_id ,doing ,done-p ,_detail) (and server (eglot--spinner server)))
               (last-error (and server (jsonrpc-last-error server))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line nil))
     (when nick
       `(":" ,(eglot--mode-line-props
               nick 'eglot-mode-line
               '((C-mouse-1 eglot-stderr-buffer "go to stderr buffer")
                 (mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when last-error
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 eglot-clear-status  "clear this status"))
                     (format "An error occurred: %s\n" (plist-get last-error
                                                                 :message)))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props doing
                                            'compilation-mode-line-run '())))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-3 eglot-forget-pending-continuations
                                "forget pending continuations"))
                     "Number of outgoing, \
still unanswered LSP requests to the server"))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))

(put 'eglot-note 'flymake-category 'flymake-note)
(put 'eglot-warning 'flymake-category 'flymake-warning)
(put 'eglot-error 'flymake-category 'flymake-error)

(defalias 'eglot--make-diag 'flymake-make-diagnostic)
(defalias 'eglot--diag-data 'flymake-diagnostic-data)

(cl-loop for i from 1
         for type in '(eglot-note eglot-warning eglot-error )
         do (put type 'flymake-overlay-control
                 `((mouse-face . highlight)
                   (priority . ,(+ 50 i))
                   (keymap . ,(let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1]
                                  (eglot--mouse-call 'eglot-code-actions))
                                map)))))


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

(cl-defmethod eglot-execute-command
  (server command arguments)
  "Execute COMMAND on SERVER with `:workspace/executeCommand'.
COMMAND is a symbol naming the command."
  (jsonrpc-request server :workspace/executeCommand
                   `(:command ,(format "%s" command) :arguments ,arguments)))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql window/showMessage)) &key type message)
  "Handle notification window/showMessage."
  (eglot--message (propertize "Server reports (type=%s): %s"
                              'face (if (<= type 1) 'error))
                  type message))

(cl-defmethod eglot-handle-request
  (_server (_method (eql window/showMessageRequest)) &key type message actions)
  "Handle server request window/showMessageRequest."
  (let* ((actions (append actions nil)) ;; gh#627
         (label (completing-read
                 (concat
                  (format (propertize "[eglot] Server reports (type=%s): %s"
                                      'face (if (<= type 1) 'error))
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

(cl-defmethod eglot-handle-notification
  (_server (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics
           &allow-other-keys) ; FIXME: doesn't respect `eglot-strict-mode'
  "Handle notification publishDiagnostics."
  (cl-flet ((eglot--diag-type (sev)
              (cond ((null sev) 'eglot-error)
                    ((<= sev 1) 'eglot-error)
                    ((= sev 2)  'eglot-warning)
                    (t          'eglot-note))))
    (if-let ((buffer (find-buffer-visiting (eglot--uri-to-path uri))))
        (with-current-buffer buffer
          (cl-loop
           for diag-spec across diagnostics
           collect (eglot--dbind ((Diagnostic) range message severity source tags)
                       diag-spec
                     (setq message (concat source ": " message))
                     (pcase-let
                         ((`(,beg . ,end) (eglot--range-region range)))
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
                                  (point-at-bol
                                   (1+ (plist-get (plist-get range :start) :line))))
                            (setq end
                                  (point-at-eol
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
           finally (cond (eglot--current-flymake-report-fn
                          (eglot--report-to-flymake diags))
                         (t
                          (setq eglot--unreported-diagnostics (cons t diags))))))
      (cl-loop
       with path = (expand-file-name (eglot--uri-to-path uri))
       for diag-spec across diagnostics
       collect (eglot--dbind ((Diagnostic) range message severity source) diag-spec
                 (setq message (concat source ": " message))
                 (let* ((start (plist-get range :start))
                        (line (1+ (plist-get start :line)))
                        (char (1+ (plist-get start :character))))
                   (eglot--make-diag
                    path (cons line char) nil (eglot--diag-type severity) message)))
       into diags
       finally
       (setq flymake-list-only-diagnostics
             (assoc-delete-all path flymake-list-only-diagnostics #'string=))
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
  (eglot--apply-workspace-edit edit eglot-confirm-server-initiated-edits))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  `(:uri ,(eglot--path-to-uri (or buffer-file-name
                                  (ignore-errors
                                    (buffer-file-name
                                     (buffer-base-buffer)))))))

(defvar-local eglot--versioned-identifier 0)

(defun eglot--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (eglot--TextDocumentIdentifier)
          `(:version ,eglot--versioned-identifier)))

(defun eglot--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (eglot--VersionedTextDocumentIdentifier)
   (list :languageId
	 (eglot--language-id (eglot--current-server-or-lose))
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
  "Set `eglot--last-inserted-char'."
  (setq eglot--last-inserted-char last-input-event))

(defun eglot--pre-command-hook ()
  "Reset `eglot--last-inserted-char'."
  (setq eglot--last-inserted-char nil))

(defun eglot--CompletionParams ()
  (append
   (eglot--TextDocumentPositionParams)
   `(:context
     ,(if-let (trigger (and (characterp eglot--last-inserted-char)
                            (cl-find eglot--last-inserted-char
                                     (eglot--server-capable :completionProvider
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

(defun eglot--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (pcase (and (listp eglot--recent-changes)
              (car eglot--recent-changes))
    (`(,lsp-beg ,lsp-end
                (,b-beg . ,b-beg-marker)
                (,b-end . ,b-end-marker))
     ;; github#259 and github#367: With `capitalize-word' or somesuch,
     ;; `before-change-functions' always records the whole word's
     ;; `b-beg' and `b-end'.  Similarly, when coalescing two lines
     ;; into one, `fill-paragraph' they mark the end of the first line
     ;; up to the end of the second line.  In both situations, args
     ;; received here contradict that information: `beg' and `end'
     ;; will differ by 1 and will likely only encompass the letter
     ;; that was capitalized or, in the sentence-joining situation,
     ;; the replacement of the newline with a space.  That's we keep
     ;; markers _and_ positions so we're able to detect and correct
     ;; this.  We ignore `beg', `len' and `pre-change-len' and send
     ;; "fuller" information about the region from the markers.  I've
     ;; also experimented with doing this unconditionally but it seems
     ;; to break when newlines are added.
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
                              (eglot--signal-textDocument/didChange)
                              (setq eglot--change-idle-timer nil))))))))

;; HACK! Launching a deferred sync request with outstanding changes is a
;; bad idea, since that might lead to the request never having a
;; chance to run, because `jsonrpc-connection-ready-p'.
(advice-add #'jsonrpc-request :before
            (cl-function (lambda (_proc _method _params &key
                                        deferred &allow-other-keys)
                           (when (and eglot--managed-mode deferred)
                             (eglot--signal-textDocument/didChange))))
            '((name . eglot--signal-textDocument/didChange)))

(defvar-local eglot-workspace-configuration ()
  "Alist of (SECTION . VALUE) entries configuring the LSP server.
SECTION should be a keyword or a string, value can be anything
that can be converted to JSON.")

;;;###autoload
(put 'eglot-workspace-configuration 'safe-local-variable 'listp)

(defun eglot-signal-didChangeConfiguration (server)
  "Send a `:workspace/didChangeConfiguration' signal to SERVER.
When called interactively, use the currently active server"
  (interactive (list (eglot--current-server-or-lose)))
  (jsonrpc-notify
   server :workspace/didChangeConfiguration
   (list
    :settings
    (cl-loop for (section . v) in eglot-workspace-configuration
             collect (if (keywordp section)
                         section
                       (intern (format ":%s" section)))
             collect v))))

(cl-defmethod eglot-handle-request
  (server (_method (eql workspace/configuration)) &key items)
  "Handle server request workspace/configuration."
  (apply #'vector
         (mapcar
          (eglot--lambda ((ConfigurationItem) scopeUri section)
            (with-temp-buffer
              (let* ((uri-path (eglot--uri-to-path scopeUri))
                     (default-directory
                       (if (and (not (string-empty-p uri-path))
                                (file-directory-p uri-path))
                           (file-name-as-directory uri-path)
                         (project-root (eglot--project server)))))
                (setq-local major-mode (eglot--major-mode server))
                (hack-dir-local-variables-non-file-buffer)
                (alist-get section eglot-workspace-configuration
                           nil nil
                           (lambda (wsection section)
                             (string=
                              (if (keywordp wsection)
                                  (substring (symbol-name wsection) 1)
                                wsection)
                              section))))))
          items)))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when eglot--recent-changes
    (let* ((server (eglot--current-server-or-lose))
           (sync-capability (eglot--server-capable :textDocumentSync))
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
      (setf (eglot--spinner server) (list nil :textDocument/didChange t))
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
  "Send textDocument/willSave to server."
  (let ((server (eglot--current-server-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (jsonrpc-notify server :textDocument/willSave params)
    (when (eglot--server-capable :textDocumentSync :willSaveWaitUntil)
      (ignore-errors
        (eglot--apply-text-edits
         (jsonrpc-request server :textDocument/willSaveWaitUntil params
                          :timeout 0.5))))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (eglot--signal-textDocument/didChange)
  (jsonrpc-notify
   (eglot--current-server-or-lose)
   :textDocument/didSave
   (list
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (eglot--TextDocumentIdentifier))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "A Flymake backend for Eglot.
Calls REPORT-FN (or arranges for it to be called) when the server
publishes diagnostics.  Between calls to this function, REPORT-FN
may be called multiple times (respecting the protocol of
`flymake-backend-functions')."
  (cond (eglot--managed-mode
         (setq eglot--current-flymake-report-fn report-fn)
         ;; Report anything unreported
         (when eglot--unreported-diagnostics
           (eglot--report-to-flymake (cdr eglot--unreported-diagnostics))))
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
  (setq eglot--unreported-diagnostics nil))

(defun eglot-xref-backend () "EGLOT xref backend." 'eglot)

(defvar eglot--temp-location-buffers (make-hash-table :test #'equal)
  "Helper variable for `eglot--handling-xrefs'.")

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
      ((file (eglot--uri-to-path uri))
       (visiting (or (find-buffer-visiting file)
                     (gethash uri eglot--temp-location-buffers)))
       (collect (lambda ()
                  (eglot--widening
                   (pcase-let* ((`(,beg . ,end) (eglot--range-region range))
                                (bol (progn (goto-char beg) (point-at-bol)))
                                (substring (buffer-substring bol (point-at-eol)))
                                (hi-beg (- beg bol))
                                (hi-end (- (min (point-at-eol) end) bol)))
                     (add-face-text-property hi-beg hi-end 'xref-match
                                             t substring)
                     (list substring (1+ (current-line)) (eglot-current-column)
                           (- end beg))))))
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

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
  (eglot--error "Cannot (yet) provide reliable completion table for LSP symbols"))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
  ;; JT@19/10/09: This is a totally dummy identifier that isn't even
  ;; passed to LSP.  The reason for this particular wording is to
  ;; construct a readable message "No references for LSP identifier at
  ;; point.".   See https://github.com/joaotavora/eglot/issues/314
  "LSP identifier at point.")

(defvar eglot--lsp-xref-refs nil
  "`xref' objects for overriding `xref-backend-references''s.")

(cl-defun eglot--lsp-xrefs-for-method (method &key extra-params capability)
  "Make `xref''s for METHOD, EXTRA-PARAMS, check CAPABILITY."
  (unless (eglot--server-capable
           (or capability
               (intern
                (format ":%sProvider"
                        (cadr (split-string (symbol-name method)
                                            "/"))))))
    (eglot--error "Sorry, this server doesn't do %s" method))
  (let ((response
         (jsonrpc-request
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

(cl-defun eglot--lsp-xref-helper (method &key extra-params capability )
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

(cl-defmethod xref-backend-definitions ((_backend (eql eglot)) _identifier)
  (eglot--lsp-xrefs-for-method :textDocument/definition))

(cl-defmethod xref-backend-references ((_backend (eql eglot)) _identifier)
  (or
   eglot--lsp-xref-refs
   (eglot--lsp-xrefs-for-method
    :textDocument/references :extra-params `(:context (:includeDeclaration t)))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (eglot--collecting-xrefs (collect)
      (mapc
       (eglot--lambda ((SymbolInformation) name location)
         (eglot--dbind ((Location) uri range) location
           (collect (eglot--xref-make-match name uri range))))
       (jsonrpc-request (eglot--current-server-or-lose)
                        :workspace/symbol
                        `(:query ,pattern))))))

(defun eglot-format-buffer ()
  "Format contents of current buffer."
  (interactive)
  (eglot-format nil nil))

(defun eglot-format (&optional beg end)
  "Format region BEG END.
If either BEG or END is nil, format entire buffer.
Interactively, format active region, or entire buffer if region
is not active."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (pcase-let ((`(,method ,cap ,args)
               (cond
                ((and beg end)
                 `(:textDocument/rangeFormatting
                   :documentRangeFormattingProvider
                   (:range ,(list :start (eglot--pos-to-lsp-position beg)
                                  :end (eglot--pos-to-lsp-position end)))))
                (t
                 '(:textDocument/formatting :documentFormattingProvider nil)))))
    (unless (eglot--server-capable cap)
      (eglot--error "Server can't format!"))
    (eglot--apply-text-edits
     (jsonrpc-request
      (eglot--current-server-or-lose)
      method
      (cl-list*
       :textDocument (eglot--TextDocumentIdentifier)
       :options (list :tabSize tab-width
                      :insertSpaces (if indent-tabs-mode :json-false t))
       args)
      :deferred method))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  ;; Commit logs for this function help understand what's going on.
  (when-let (completion-capability (eglot--server-capable :completionProvider))
    (let* ((server (eglot--current-server-or-lose))
           (sort-completions
            (lambda (completions)
              (cl-sort completions
                       #'string-lessp
                       :key (lambda (c)
                              (or (plist-get
                                   (get-text-property 0 'eglot--lsp-item c)
                                   :sortText)
                                  "")))))
           (metadata `(metadata (category . eglot)
                                (display-sort-function . ,sort-completions)))
           resp items (cached-proxies :none)
           (proxies
            (lambda ()
              (if (listp cached-proxies) cached-proxies
                (setq resp
                      (jsonrpc-request server
                                       :textDocument/completion
                                       (eglot--CompletionParams)
                                       :deferred :textDocument/completion
                                       :cancel-on-input t))
                (setq items (append
                             (if (vectorp resp) resp (plist-get resp :items))
                             nil))
                (setq cached-proxies
                      (mapcar
                       (jsonrpc-lambda
                           (&rest item &key label insertText insertTextFormat
                                  &allow-other-keys)
                         (let ((proxy
                                (cond ((and (eql insertTextFormat 2)
                                            (eglot--snippet-expansion-fn))
                                       (string-trim-left label))
                                      ((and insertText
                                            (not (string-empty-p insertText)))
                                       insertText)
                                      (t
                                       (string-trim-left label)))))
                           (unless (zerop (length proxy))
                             (put-text-property 0 1 'eglot--lsp-item item proxy))
                           proxy))
                       items)))))
           (resolved (make-hash-table))
           (resolve-maybe
            ;; Maybe completion/resolve JSON object `lsp-comp' into
            ;; another JSON object, if at all possible.  Otherwise,
            ;; just return lsp-comp.
            (lambda (lsp-comp)
              (or (gethash lsp-comp resolved)
                  (setf (gethash lsp-comp resolved)
                        (if (and (eglot--server-capable :completionProvider
                                                        :resolveProvider)
                                 (plist-get lsp-comp :data))
                            (jsonrpc-request server :completionItem/resolve
                                             lsp-comp :cancel-on-input t)
                          lsp-comp)))))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (lambda (probe pred action)
         (cond
          ((eq action 'metadata) metadata)               ; metadata
          ((eq action 'lambda)                           ; test-completion
           (test-completion probe (funcall proxies)))
          ((eq (car-safe action) 'boundaries) nil)       ; boundaries
          ((null action)                                 ; try-completion
           (try-completion probe (funcall proxies)))
          ((eq action t)                                 ; all-completions
           (all-completions
            ""
            (funcall proxies)
            (lambda (proxy)
              (let* ((item (get-text-property 0 'eglot--lsp-item proxy))
                     (filterText (plist-get item :filterText)))
                (and (or (null pred) (funcall pred proxy))
                     (string-prefix-p
                      probe (or filterText proxy) completion-ignore-case))))))))
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
           (intern (downcase kind))))
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
         (when (car bounds) (goto-char (car bounds)))
         (when (listp completion-capability)
           (looking-back
            (regexp-opt
             (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
            (line-beginning-position))))
       :exit-function
       (lambda (proxy status)
         (when (eq status 'finished)
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
                        ;; Undo (yes, undo) the newly inserted completion.
                        ;; If before completion the buffer was "foo.b" and
                        ;; now is "foo.bar", `proxy' will be "bar".  We
                        ;; want to delete only "ar" (`proxy' minus the
                        ;; symbol whose bounds we've calculated before)
                        ;; (github#160).
                        (delete-region (+ (- (point) (length proxy))
                                          (if bounds
                                              (- (cdr bounds) (car bounds))
                                            0))
                                       (point))
                        (eglot--dbind ((TextEdit) range newText) textEdit
                          (pcase-let ((`(,beg . ,end)
                                       (eglot--range-region range)))
                            (delete-region beg end)
                            (goto-char beg)
                            (funcall (or snippet-fn #'insert) newText)))
                        (when (cl-plusp (length additionalTextEdits))
                          (eglot--apply-text-edits additionalTextEdits)))
                       (snippet-fn
                        ;; A snippet should be inserted, but using plain
                        ;; `insertText'.  This requires us to delete the
                        ;; whole completion, since `insertText' is the full
                        ;; completion's text.
                        (delete-region (- (point) (length proxy)) (point))
                        (funcall snippet-fn (or insertText label)))))
               (eglot--signal-textDocument/didChange)
               (eldoc)))))))))

(defun eglot--hover-info (contents &optional range)
  (let ((heading (and range (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                              (concat (buffer-substring beg end)  ": "))))
        (body (mapconcat #'eglot--format-markup
                         (if (vectorp contents) contents (list contents)) "\n")))
    (when (or heading (cl-plusp (length body))) (concat heading body))))

(defun eglot--sig-info (sigs active-sig sig-help-active-param)
  (cl-loop
   for (sig . moresigs) on (append sigs nil) for i from 0
   concat
   (eglot--dbind ((SignatureInformation) label documentation parameters activeParameter) sig
     (with-temp-buffer
       (save-excursion (insert label))
       (let ((active-param (or activeParameter sig-help-active-param))
             params-start params-end)
         ;; Ad-hoc attempt to parse label as <name>(<params>)
         (when (looking-at "\\([^(]+\\)(\\([^)]+\\))")
           (setq params-start (match-beginning 2) params-end (match-end 2))
           (add-face-text-property (match-beginning 1) (match-end 1)
                                   'font-lock-function-name-face))
         (when (eql i active-sig)
           ;; Decide whether to add one-line-summary to signature line
           (when (and (stringp documentation)
                      (string-match "[[:space:]]*\\([^.\r\n]+[.]?\\)"
                                    documentation))
             (setq documentation (match-string 1 documentation))
             (unless (string-prefix-p (string-trim documentation) label)
               (goto-char (point-max))
               (insert ": " (eglot--format-markup documentation))))
           ;; Decide what to do with the active parameter...
           (when (and (eql i active-sig) active-param
                      (< -1 active-param (length parameters)))
             (eglot--dbind ((ParameterInformation) label documentation)
                 (aref parameters active-param)
               ;; ...perhaps highlight it in the formals list
               (when params-start
                 (goto-char params-start)
                 (pcase-let
                     ((`(,beg ,end)
                       (if (stringp label)
                           (let ((case-fold-search nil))
                             (and (re-search-forward
                                   (concat "\\<" (regexp-quote label) "\\>")
                                   params-end t)
                                  (list (match-beginning 0) (match-end 0))))
                         (mapcar #'1+ (append label nil)))))
                   (if (and beg end)
                       (add-face-text-property
                        beg end
                        'eldoc-highlight-function-argument))))
               ;; ...and/or maybe add its doc on a line by its own.
               (when documentation
                 (goto-char (point-max))
                 (insert "\n"
                         (propertize
                          (if (stringp label)
                              label
                            (apply #'buffer-substring (mapcar #'1+ label)))
                          'face 'eldoc-highlight-function-argument)
                         ": " (eglot--format-markup documentation))))))
         (buffer-string))))
   when moresigs concat "\n"))

(defun eglot-signature-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for signatures."
  (when (eglot--server-capable :signatureHelpProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/signatureHelp (eglot--TextDocumentPositionParams)
       :success-fn
       (eglot--lambda ((SignatureHelp)
                       signatures activeSignature activeParameter)
         (eglot--when-buffer-window buf
           (funcall cb
                    (unless (seq-empty-p signatures)
                      (eglot--sig-info signatures
                                       activeSignature
                                       activeParameter)))))
       :deferred :textDocument/signatureHelp))
    t))

(defun eglot-hover-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for hover."
  (when (eglot--server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn (eglot--lambda ((Hover) contents range)
                     (eglot--when-buffer-window buf
                       (let ((info (unless (seq-empty-p contents)
                                     (eglot--hover-info contents range))))
                         (funcall cb info :buffer t))))
       :deferred :textDocument/hover))
    (eglot--highlight-piggyback cb)
    t))

(defvar eglot--highlights nil "Overlays for textDocument/documentHighlight.")

(defun eglot--highlight-piggyback (_cb)
  "Request and handle `:textDocument/documentHighlight'."
  ;; FIXME: Obviously, this is just piggy backing on eldoc's calls for
  ;; convenience, as shown by the fact that we just ignore cb.
  (let ((buf (current-buffer)))
    (when (eglot--server-capable :documentHighlightProvider)
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
                                 (eglot--range-region range)))
                      (let ((ov (make-overlay beg end)))
                        (overlay-put ov 'face 'eglot-highlight-symbol-face)
                        (overlay-put ov 'modification-hooks
                                     `(,(lambda (o &rest _) (delete-overlay o))))
                        ov)))
                  highlights))))
       :deferred :textDocument/documentHighlight)
      nil)))

(defun eglot-imenu ()
  "EGLOT's `imenu-create-index-function'."
  (cl-labels
      ((visit (_name one-obj-array)
              (imenu-default-goto-function
               nil (car (eglot--range-region
                         (eglot--dcase (aref one-obj-array 0)
                           (((SymbolInformation) location)
                            (plist-get location :range))
                           (((DocumentSymbol) selectionRange)
                            selectionRange))))))
       (unfurl (obj)
               (eglot--dcase obj
                 (((SymbolInformation)) (list obj))
                 (((DocumentSymbol) name children)
                  (cons obj
                        (mapcar
                         (lambda (c)
                           (plist-put
                            c :containerName
                            (let ((existing (plist-get c :containerName)))
                              (if existing (format "%s::%s" name existing)
                                name))))
                         (mapcan #'unfurl children)))))))
    (mapcar
     (pcase-lambda (`(,kind . ,objs))
       (cons
        (alist-get kind eglot--symbol-kind-names "Unknown")
        (mapcan (pcase-lambda (`(,container . ,objs))
                  (let ((elems (mapcar (lambda (obj)
                                         (list (plist-get obj :name)
                                               `[,obj] ;; trick
                                               #'visit))
                                       objs)))
                    (if container (list (cons container elems)) elems)))
                (seq-group-by
                 (lambda (e) (plist-get e :containerName)) objs))))
     (seq-group-by
      (lambda (obj) (plist-get obj :kind))
      (mapcan #'unfurl
              (jsonrpc-request (eglot--current-server-or-lose)
                               :textDocument/documentSymbol
                               `(:textDocument
                                 ,(eglot--TextDocumentIdentifier))
                               :cancel-on-input non-essential))))))

(defun eglot--apply-text-edits (edits &optional version)
  "Apply EDITS for current buffer if at VERSION, or if it's nil."
  (unless (or (not version) (equal version eglot--versioned-identifier))
    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version eglot--versioned-identifier))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (make-progress-reporter
                      (format "[eglot] applying %s edits to `%s'..."
                              howmany (current-buffer))
                      0 howmany))
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

                          ;; On emacs versions < 26.2,
                          ;; `replace-buffer-contents' is buggy - it calls
                          ;; change functions with invalid arguments - so we
                          ;; manually call the change functions here.
                          ;;
                          ;; See emacs bugs #32237, #32278:
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
                          (let ((inhibit-modification-hooks t)
                                (length (- end beg))
                                (beg (marker-position beg))
                                (end (marker-position end)))
                            (run-hook-with-args 'before-change-functions
                                                beg end)
                            (replace-buffer-contents temp)
                            (run-hook-with-args 'after-change-functions
                                                beg (+ beg (length newText))
                                                length))))
                      (progress-reporter-update reporter (cl-incf done)))))))
            (mapcar (eglot--lambda ((TextEdit) range newText)
                      (cons newText (eglot--range-region range 'markers)))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (progress-reporter-done reporter))))

(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (eglot--uri-to-path uri) edits version)))
                   documentChanges)))
      (cl-loop for (uri edits) on changes by #'cddr
               do (push (list (eglot--uri-to-path uri) edits) prepared))
      (if (or confirm
              (cl-notevery #'find-buffer-visiting
                           (mapcar #'car prepared)))
          (unless (y-or-n-p
                   (format "[eglot] Server wants to edit:\n  %s\n Proceed? "
                           (mapconcat #'identity (mapcar #'car prepared) "\n  ")))
            (eglot--error "User cancelled server edit")))
      (cl-loop for edit in prepared
               for (path edits version) = edit
               do (with-current-buffer (find-file-noselect path)
                    (eglot--apply-text-edits edits version))
               finally (eldoc) (eglot--message "Edit successful!")))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (jsonrpc-request (eglot--current-server-or-lose)
                    :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                           :newName ,newname))
   current-prefix-arg))

(defun eglot--region-bounds () "Region bounds if active, else point and nil."
  (if (use-region-p) `(,(region-beginning) ,(region-end)) `(,(point) nil)))

(defun eglot-code-actions (beg &optional end action-kind)
  "Offer to execute actions of ACTION-KIND between BEG and END.
If ACTION-KIND is nil, consider all kinds of actions.
Interactively, default BEG and END to region's bounds else BEG is
point and END is nil, which results in a request for code actions
at point.  With prefix argument, prompt for ACTION-KIND."
  (interactive
   `(,@(eglot--region-bounds)
     ,(and current-prefix-arg
           (completing-read "[eglot] Action kind: "
                            '("quickfix" "refactor.extract" "refactor.inline"
                              "refactor.rewrite" "source.organizeImports")))))
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions
          (jsonrpc-request
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
                   ,@(when action-kind `(:only [,action-kind]))))
           :deferred t))
         (menu-items
          (or (cl-loop for action across actions
                       ;; Do filtering ourselves, in case the `:only'
                       ;; didn't go through.
                       when (or (not action-kind)
                                (equal action-kind (plist-get action :kind)))
                       collect (cons (plist-get action :title) action))
              (apply #'eglot--error
                     (if action-kind `("No \"%s\" code actions here" ,action-kind)
                       `("No code actions here")))))
         (preferred-action (cl-find-if
                            (lambda (menu-item)
                              (plist-get (cdr menu-item) :isPreferred))
                            menu-items))
         (default-action (car (or preferred-action (car menu-items))))
         (action (if (and action-kind (null (cadr menu-items)))
                     (cdr (car menu-items))
                   (if (listp last-nonmenu-event)
                       (x-popup-menu last-nonmenu-event `("Eglot code actions:"
                                                          ("dummy" ,@menu-items)))
                     (cdr (assoc (completing-read
                                  (format "[eglot] Pick an action (default %s): "
                                          default-action)
                                  menu-items nil t nil nil default-action)
                                 menu-items))))))
    (eglot--dcase action
      (((Command) command arguments)
       (eglot-execute-command server (intern command) arguments))
      (((CodeAction) edit command)
       (when edit (eglot--apply-workspace-edit edit))
       (when command
         (eglot--dbind ((Command) command arguments) command
           (eglot-execute-command server (intern command) arguments)))))))

(defmacro eglot--code-action (name kind)
  "Define NAME to execute KIND code action."
  `(defun ,name (beg &optional end)
     ,(format "Execute '%s' code actions between BEG and END." kind)
     (interactive (eglot--region-bounds))
     (eglot-code-actions beg end ,kind)))

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
                 (eglot--lambda ((FileSystemWatcher) globPattern)
                   (eglot--glob-compile globPattern t t))
                 watchers))
         (dirs-to-watch
          (delete-dups (mapcar #'file-name-directory
                               (project-files
                                (eglot--project server))))))
    (cl-labels
        ((handle-event
          (event)
          (pcase-let ((`(,desc ,action ,file ,file1) event))
            (cond
             ((and (memq action '(created changed deleted))
                   (cl-find file globs :test (lambda (f g) (funcall g f))))
              (jsonrpc-notify
               server :workspace/didChangeWatchedFiles
               `(:changes ,(vector `(:uri ,(eglot--path-to-uri file)
                                          :type ,(cl-case action
                                                   (created 1)
                                                   (changed 2)
                                                   (deleted 3)))))))
             ((eq action 'renamed)
              (handle-event `(,desc 'deleted ,file))
              (handle-event `(,desc 'created ,file1)))))))
      (unwind-protect
          (progn
            (dolist (dir dirs-to-watch)
              (push (file-notify-add-watch dir '(change) #'handle-event)
                    (gethash id (eglot--file-watches server))))
            (setq
             success
             `(:message ,(format "OK, watching %s directories in %s watchers"
                                 (length dirs-to-watch) (length watchers)))))
        (unless success
          (eglot-unregister-capability server method id))))))

(cl-defmethod eglot-unregister-capability
  (server (_method (eql workspace/didChangeWatchedFiles)) id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles."
  (mapc #'file-notify-rm-watch (gethash id (eglot--file-watches server)))
  (remhash id (eglot--file-watches server))
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
            (or ,@(cl-loop for alt in alternatives
                           collect `(re-search-forward ,(concat "\\=" alt) nil t))
                (error "Failed matching any of %s" ',alternatives))
            (,next))))

(defun eglot--glob-emit-range (arg self next)
  (when (eq ?! (aref arg 1)) (aset arg 1 ?^))
  `(,self () (re-search-forward ,(concat "\\=" arg)) (,next)))


;;; eclipse-jdt-specific
;;;
(defclass eglot-eclipse-jdt (eglot-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
  "Passes through required jdt initialization options."
  `(:workspaceFolders
    [,@(cl-delete-duplicates
        (mapcar #'eglot--path-to-uri
                (let* ((root (project-root (eglot--project server))))
                  (cons root
                        (mapcar
                         #'file-name-directory
                         (append
                          (file-expand-wildcards (concat root "*/pom.xml"))
                          (file-expand-wildcards (concat root "*/build.gradle"))
                          (file-expand-wildcards (concat root "*/.project")))))))
        :test #'string=)]
    ,@(if-let ((home (or (getenv "JAVA_HOME")
                         (ignore-errors
                           (expand-file-name
                            ".."
                            (file-name-directory
                             (file-chase-links (executable-find "javac"))))))))
          `(:settings (:java (:home ,home)))
        (ignore (eglot--warn "JAVA_HOME env var not set")))))

(defun eglot--eclipse-jdt-contact (interactive)
  "Return a contact for connecting to eclipse.jdt.ls server, as a cons cell.
If INTERACTIVE, prompt user for details."
  (cl-labels
      ((is-the-jar
        (path)
        (and (string-match-p
              "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"
              (file-name-nondirectory path))
             (file-exists-p path))))
    (let* ((classpath (or (getenv "CLASSPATH") path-separator))
           (cp-jar (cl-find-if #'is-the-jar (split-string classpath path-separator)))
           (jar cp-jar)
           (dir
            (cond
             (jar (file-name-as-directory
                   (expand-file-name ".." (file-name-directory jar))))
             (interactive
              (expand-file-name
               (read-directory-name
                (concat "Path to eclipse.jdt.ls directory (could not"
                        " find it in CLASSPATH): ")
                nil nil t)))
             (t (error "Could not find eclipse.jdt.ls jar in CLASSPATH"))))
           (repodir
            (concat dir
                    "org.eclipse.jdt.ls.product/target/repository/"))
           (repodir (if (file-directory-p repodir) repodir dir))
           (config
            (concat
             repodir
             (cond
              ((string= system-type "darwin") "config_mac")
              ((string= system-type "windows-nt") "config_win")
              (t "config_linux"))))
           (workspace
            (expand-file-name (md5 (project-root (eglot--current-project)))
                              (locate-user-emacs-file
                               "eglot-eclipse-jdt-cache"))))
      (unless jar
        (setq jar
              (cl-find-if #'is-the-jar
                          (directory-files (concat repodir "plugins") t))))
      (unless (and jar (file-exists-p jar) (file-directory-p config))
        (error "Could not find required eclipse.jdt.ls files (build required?)"))
      (when (and interactive (not cp-jar)
                 (y-or-n-p (concat "Add path to the server program "
                                   "to CLASSPATH environment variable?")))
        (setenv "CLASSPATH" (concat (getenv "CLASSPATH") path-separator jar)))
      (unless (file-directory-p workspace)
        (make-directory workspace t))
      (cons 'eglot-eclipse-jdt
            (list (executable-find "java")
                  "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                  "-Dosgi.bundles.defaultStartLevel=4"
                  "-Declipse.product=org.eclipse.jdt.ls.core.product"
                  "-jar" jar
                  "-configuration" config
                  "-data" workspace)))))

(cl-defmethod eglot-execute-command
  ((_server eglot-eclipse-jdt) (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))


;;; Obsolete
;;;

(make-obsolete-variable 'eglot--managed-mode-hook
                        'eglot-managed-mode-hook "1.6")

(if (< emacs-major-version 27)
    (defun eglot--plist-keys (plist)
      (cl-loop for (k _v) on plist by #'cddr collect k))
  ;; Make into an obsolete alias once we drop support for Emacs 26.
  (defalias 'eglot--plist-keys #'map-keys))

(provide 'eglot)

;; Local Variables:
;; bug-reference-bug-regexp: "\\(github#\\([0-9]+\\)\\)"
;; bug-reference-url-format: "https://github.com/joaotavora/eglot/issues/%s"
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; eglot.el ends here
