;;; eglot.el --- Client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Version: 0.7
;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simply M-x eglot should be enough to get you started, but here's a
;; little info (see the accompanying README.md or the URL for more).
;;
;; M-x eglot starts a server via a shell-command guessed from
;; `eglot-server-programs', using the current major-mode (for whatever
;; language you're programming in) as a hint.  If it can't guess, it
;; prompts you in the mini-buffer for these things.  Actually, the
;; server needen't be locally started: you can connect to a running
;; server via TCP by entering a <host:port> syntax.
;;
;; Anyway, if the connection is successful, you should see an `eglot'
;; indicator pop up in your mode-line.  More importantly, this means
;; current *and future* file buffers of that major mode *inside your
;; current project* automatically become \"managed\" by the LSP
;; server, i.e.  information about their contents is exchanged
;; periodically to provide enhanced code analysis via
;; `xref-find-definitions', `flymake-mode', `eldoc-mode',
;; `completion-at-point', among others.
;;
;; To "unmanage" these buffers, shutdown the server with M-x
;; eglot-shutdown.
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile) ; for some faces
(require 'warnings)
(require 'flymake)
(require 'xref)
(require 'subr-x)
(require 'jsonrpc)
(require 'filenotify)
(require 'ert)


;;; User tweakable stuff
(defgroup eglot nil
  "Interaction with Language Server Protocol servers"
  :prefix "eglot-"
  :group 'applications)

(defvar eglot-server-programs '((rust-mode . (eglot-rls "rls"))
                                (python-mode . ("pyls"))
                                (js-mode . ("javascript-typescript-stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                (c++-mode . (eglot-cquery "cquery"))
                                (c-mode . (eglot-cquery "cquery"))
                                (php-mode . ("php" "vendor/felixfbecker/\
language-server/bin/php-language-server.php")))
  "How the command `eglot' guesses the server to start.
An association list of (MAJOR-MODE . SPEC) pair.  MAJOR-MODE is a
mode symbol.  SPEC is

* In the most common case, a list of strings (PROGRAM [ARGS...]).
PROGRAM is called with ARGS and is expected to serve LSP requests
over the standard input/output channels.

* A list (HOST PORT [ARGS...]) where HOST is a string and PORT is a
positive integer number for connecting to a server via TCP.
Remaining ARGS are passed to `open-network-stream' for upgrading
the connection with encryption, etc...

* A function of no arguments returning a connected process.

* A cons (CLASS-NAME . SPEC) where CLASS-NAME is a symbol
designating a subclass of `eglot-lsp-server', for
representing experimental LSP servers.  In this case SPEC is
interpreted as described above this point.")

(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in EGLOT's mode line.")

(defcustom eglot-autoreconnect 3
  "Control ability to reconnect automatically to the LSP server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))


;;; API (WORK-IN-PROGRESS!)
;;;
(cl-defmacro eglot--with-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro eglot--lambda (cl-lambda-list &body body)
  "Make a unary function of ARG, a plist-like JSON object.
CL-LAMBDA-LIST destructures ARGS before running BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (gensym "eglot--lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(cl-defmacro eglot--widening (&rest body)
  "Save excursion and restriction. Widen. Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))

(cl-defgeneric eglot-handle-request (server method id &rest params)
  "Handle SERVER's METHOD request with ID and PARAMS.")

(cl-defgeneric eglot-handle-notification (server method id &rest params)
  "Handle SERVER's METHOD notification with PARAMS.")

(cl-defgeneric eglot-initialization-options (server)
  "JSON object to send under `initializationOptions'"
  (:method (_s) nil)) ; blank default

(cl-defgeneric eglot-client-capabilities (server)
  "What the EGLOT LSP client supports for SERVER."
  (:method (_s)
           (list
            :workspace (list
                        :applyEdit t
                        :workspaceEdit `(:documentChanges :json-false)
                        :didChangeWatchesFiles `(:dynamicRegistration t)
                        :symbol `(:dynamicRegistration :json-false))
            :textDocument
            (list
             :synchronization (list
                               :dynamicRegistration :json-false
                               :willSave t :willSaveWaitUntil t :didSave t)
             :completion         `(:dynamicRegistration :json-false)
             :hover              `(:dynamicRegistration :json-false)
             :signatureHelp      `(:dynamicRegistration :json-false)
             :references         `(:dynamicRegistration :json-false)
             :definition         `(:dynamicRegistration :json-false)
             :documentSymbol     `(:dynamicRegistration :json-false)
             :documentHighlight  `(:dynamicRegistration :json-false)
             :rename             `(:dynamicRegistration :json-false)
             :publishDiagnostics `(:relatedInformation :json-false))
            :experimental (list))))

(defclass eglot-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :initarg :project-nickname :accessor eglot--project-nickname)
   (major-mode
    :documentation "Major mode symbol."
    :initarg :major-mode :accessor eglot--major-mode)
   (capabilities
    :documentation "JSON object containing server capabilities."
    :accessor eglot--capabilities)
   (shutdown-requested
    :documentation "Flag set when server is shutting down."
    :accessor eglot--shutdown-requested)
   (project
    :documentation "Project associated with server."
    :initarg :project :accessor eglot--project)
   (spinner
    :documentation "List (ID DOING-WHAT DONE-P) representing server progress."
    :initform `(nil nil t) :accessor eglot--spinner)
   (inhibit-autoreconnect
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :initarg :inhibit-autoreconnect :accessor eglot--inhibit-autoreconnect)
   (file-watches
    :documentation "Map ID to list of WATCHES for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor eglot--file-watches)
   (managed-buffers
    :documentation "List of buffers managed by server."
    :initarg :managed-buffers :accessor eglot--managed-buffers))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")


;;; Process management
(defvar eglot--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

;; HACK: Do something to fix this in the jsonrpc API or here, but in
;; the meantime concentrate the hack here.
(defalias 'eglot--process 'jsonrpc--process
  "An abuse of `jsonrpc--process', a jsonrpc.el internal.")

(defun eglot-shutdown (server &optional _interactive)
  "Politely ask SERVER to quit.
Forcefully quit it if it doesn't respond.  Don't leave this
function with the server still running."
  (interactive (list (jsonrpc-current-connection-or-lose) t))
  (eglot--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (progn
        (setf (eglot--shutdown-requested server) t)
        (jsonrpc-request server :shutdown nil :timeout 3)
        ;; this one is supposed to always fail, hence ignore-errors
        (ignore-errors (jsonrpc-request server :exit nil :timeout 1)))
    ;; Turn off `eglot--managed-mode' where appropriate.
    (dolist (buffer (eglot--managed-buffers server))
      (with-current-buffer buffer (eglot--managed-mode-onoff server -1)))
    (while (progn (accept-process-output nil 0.1)
                  (not (eq (eglot--shutdown-requested server) :sentinel-done)))
      (eglot--warn "Sentinel for %s still hasn't run, brutally deleting it!"
                   (eglot--process server))
      (delete-process (eglot--process server)))))

(defun eglot--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `eglot--managed-mode' where appropriate.
  (dolist (buffer (eglot--managed-buffers server))
    (with-current-buffer buffer (eglot--managed-mode-onoff server -1)))
  ;; Kill any expensive watches
  (maphash (lambda (_id watches)
             (mapcar #'file-notify-rm-watch watches))
           (eglot--file-watches server))
  ;; Sever the project/server relationship for `server'
  (setf (gethash (eglot--project server) eglot--servers-by-project)
        (delq server
              (gethash (eglot--project server) eglot--servers-by-project)))
  (cond ((eglot--shutdown-requested server)
         (setf (eglot--shutdown-requested server) :sentinel-done))
        ((not (eglot--inhibit-autoreconnect server))
         (eglot--warn "Reconnecting after unexpected server exit.")
         (eglot-reconnect server))
        ((timerp (eglot--inhibit-autoreconnect server))
         (eglot--warn "Not auto-reconnecting, last one didn't last long."))))

(defun eglot--all-major-modes ()
  "Return all know major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defvar eglot--command-history nil
  "History of CONTACT arguments to `eglot'.")

(defun eglot--interactive ()
  "Helper for `eglot'."
  (let* ((guessed-mode (if buffer-file-name major-mode))
         (managed-mode
          (cond
           ((or (>= (prefix-numeric-value current-prefix-arg) 16)
                (not guessed-mode))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              (symbol-name guessed-mode) nil (symbol-name guessed-mode) nil)))
           (t guessed-mode)))
         (project (or (project-current) `(transient . ,default-directory)))
         (guess (cdr (assoc managed-mode eglot-server-programs)))
         (class (if (and (consp guess) (symbolp (car guess)))
                    (prog1 (car guess) (setq guess (cdr guess)))
                  'eglot-lsp-server))
         (program (and (listp guess) (stringp (car guess)) (car guess)))
         (base-prompt "[eglot] Enter program to execute (or <host>:<port>): ")
         (prompt
          (cond (current-prefix-arg base-prompt)
                ((null guess)
                 (format "[eglot] Sorry, couldn't guess for `%s'\n%s!"
                         managed-mode base-prompt))
                ((and program (not (executable-find program)))
                 (concat (format "[eglot] I guess you want to run `%s'"
                                 (combine-and-quote-strings guess))
                         (format ", but I can't find `%s' in PATH!" program)
                         "\n" base-prompt))))
         (contact
          (if prompt
              (let ((s (read-shell-command
                        prompt
                        (if program (combine-and-quote-strings guess))
                        'eglot-command-history)))
                (if (string-match "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                  (string-trim s))
                    (list (match-string 1 s) (string-to-number (match-string 2 s)))
                  (split-string-and-unquote s)))
            guess)))
    (list managed-mode project (cons class contact) t)))

;;;###autoload
(defun eglot (managed-major-mode project contact &optional interactive)
  "Manage a project with a Language Server Protocol (LSP) server.

The LSP server is started (or contacted) via CONTACT.  If this
operation is successful, current *and future* file buffers of
MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', `completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CONTACT from `eglot-server-programs' and
PROJECT from `project-current'.  If it can't guess, the user is
prompted.  With a single \\[universal-argument] prefix arg, it
always prompt for COMMAND.  With two \\[universal-argument]
prefix args, also prompts for MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

CONTACT specifies how to contact the server. It can be:

* a list of strings (COMMAND [ARGS...]) specifying how
to start a server subprocess to connect to.

* A list with a string as the first element and an integer number
as the second list is interpreted as (HOST PORT [PARAMETERS...])
and connects to an existing server via TCP, with the remaining
PARAMETERS being given as `open-network-stream's optional
arguments.

* A list (CLASS-SYM CONTACT...) where CLASS-SYM names the
subclass of `eglot-server' used to create the server object. The
remaining arguments are processed as described in the previous
paragraphs.

* A function of arguments returning arguments compatible with the
previous description.

INTERACTIVE is t if called interactively."
  (interactive (eglot--interactive))
  (let* ((nickname (file-name-base (directory-file-name
                                    (car (project-roots project)))))
         (current-server (jsonrpc-current-connection))
         (live-p (and current-server
                      (process-live-p (eglot--process current-server)))))
    (if (and live-p
             interactive
             (y-or-n-p "[eglot] Live process found, reconnect instead? "))
        (eglot-reconnect current-server interactive)
      (when live-p (ignore-errors (eglot-shutdown current-server)))
      (let ((server (eglot--connect project
                                    managed-major-mode
                                    (format "%s/%s" nickname managed-major-mode)
                                    nickname
                                    contact)))
        (eglot--message "Connected! Process `%s' now \
managing `%s' buffers in project `%s'."
                        (jsonrpc-name server) managed-major-mode
                        nickname)
        server))))

(defun eglot-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (jsonrpc-current-connection-or-lose) t))
  (when (process-live-p (eglot--process server))
    (ignore-errors (eglot-shutdown server interactive)))
  (eglot--connect (eglot--project server)
                  (eglot--major-mode server)
                  (jsonrpc-name server)
                  (eglot--project-nickname server)
                  (jsonrpc-contact server))
  (eglot--message "Reconnected!"))

(defalias 'eglot-events-buffer 'jsonrpc-events-buffer)

(defvar eglot-connect-hook nil "Hook run after connecting in `eglot--connect'.")

(defun eglot--dispatch (server method id params)
  "Dispatcher passed to `jsonrpc-connect'.
Calls a function on SERVER, METHOD ID and PARAMS."
  (let ((method (intern (format ":%s" method))))
    (if id
        (apply #'eglot-handle-request server id method params)
      (apply #'eglot-handle-notification server method params)
      (force-mode-line-update t))))

(defun eglot--connect (project managed-major-mode name nickname contact)
  "Connect to PROJECT, MANAGED-MAJOR-MODE, NAME.
And NICKNAME and CONTACT."
  (let* ((contact (if (functionp contact) (funcall contact) contact))
         (server
          (jsonrpc-connect name contact #'eglot--dispatch #'eglot--on-shutdown))
         success)
    (setf (eglot--project server) project)
    (setf (eglot--project-nickname server) nickname)
    (setf (eglot--major-mode server) managed-major-mode)
    (push server (gethash project eglot--servers-by-project))
    (run-hook-with-args 'eglot-connect-hook server)
    (unwind-protect
        (cl-destructuring-bind (&key capabilities)
            (jsonrpc-request
             server
             :initialize
             (list :processId (unless (eq (process-type
                                           (eglot--process server))
                                          'network)
                                (emacs-pid))
                   :rootPath  (expand-file-name
                               (car (project-roots project)))
                   :rootUri  (eglot--path-to-uri
                              (car (project-roots project)))
                   :initializationOptions (eglot-initialization-options server)
                   :capabilities (eglot-client-capabilities server)))
          (setf (eglot--capabilities server) capabilities)
          (setf (jsonrpc-status server) nil)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (eglot--maybe-activate-editing-mode server)))
          (jsonrpc-notify server :initialized `(:__dummy__ t))
          (setf (eglot--inhibit-autoreconnect server)
                (cond
                 ((booleanp eglot-autoreconnect) (not eglot-autoreconnect))
                 ((cl-plusp eglot-autoreconnect)
                  (run-with-timer eglot-autoreconnect nil
                                  (lambda ()
                                    (setf (eglot--inhibit-autoreconnect server)
                                          (null eglot-autoreconnect)))))))
          (setq success server))
      (unless (or success (not (process-live-p (eglot--process server))))
        (eglot-shutdown server)))))


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

(defun eglot--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (save-excursion
    (list :line (1- (line-number-at-pos pos t)) ; F!@&#$CKING OFF-BY-ONE
          :character (- (goto-char (or pos (point)))
                        (line-beginning-position)))))

(defun eglot--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion (goto-char (point-min))
                  (forward-line (plist-get pos-plist :line))
                  (forward-char (min (plist-get pos-plist :character)
                                     (- (line-end-position)
                                        (line-beginning-position))))
                  (if marker (copy-marker (point-marker)) (point))))

(defun eglot--path-to-uri (path)
  "URIfy PATH."
  (url-hexify-string
   (concat "file://" (if (eq system-type 'windows-nt) "/") (file-truename path))
   url-path-allowed-chars))

(defun eglot--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-filename (url-generic-parse-url (url-unhex-string uri)))))
    (if (eq system-type 'windows-nt) (substring retval 1) retval)))

(defconst eglot--kind-names
  `((1 . "Text") (2 . "Method") (3 . "Function") (4 . "Constructor")
    (5 . "Field") (6 . "Variable") (7 . "Class") (8 . "Interface")
    (9 . "Module") (10 . "Property") (11 . "Unit") (12 . "Value")
    (13 . "Enum") (14 . "Keyword") (15 . "Snippet") (16 . "Color")
    (17 . "File") (18 . "Reference")))

(defun eglot--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list (string-trim markup)
                                          (intern "markdown-mode"))
                 (list (plist-get markup :value)
                       (intern (concat (plist-get markup :language) "-mode" ))))))
    (with-temp-buffer
      (ignore-errors (funcall mode))
      (insert string) (font-lock-ensure) (buffer-string))))

(defun eglot--server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (cl-loop for caps = (eglot--capabilities (jsonrpc-current-connection-or-lose))
           then (cadr probe)
           for feat in feats
           for probe = (plist-member caps feat)
           if (not probe) do (cl-return nil)
           if (eq (cadr probe) t) do (cl-return t)
           if (eq (cadr probe) :json-false) do (cl-return nil)
           finally (cl-return (or probe t))))

(defun eglot--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((st (plist-get range :start))
         (beg (eglot--lsp-position-to-point st markers))
         (end (eglot--lsp-position-to-point (plist-get range :end) markers)))
    ;; Fallback to `flymake-diag-region' if server botched the range
    (if (/= beg end) (cons beg end) (flymake-diag-region
                                     (current-buffer) (plist-get st :line)
                                     (1- (plist-get st :character))))))


;;; Minor modes
;;;
(defvar eglot-mode-map (make-sparse-keymap))

(defvar-local eglot--current-flymake-report-fn nil
  "Current flymake report function for this buffer")

(define-minor-mode eglot--managed-mode
  "Mode for source buffers managed by some EGLOT project."
  nil nil eglot-mode-map
  (cond
   (eglot--managed-mode
    (add-hook 'jsonrpc-find-connection-functions 'eglot--find-current-server nil t)
    (add-hook 'after-change-functions 'eglot--after-change nil t)
    (add-hook 'before-change-functions 'eglot--before-change nil t)
    (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
    (add-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'before-revert-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'before-save-hook 'eglot--signal-textDocument/willSave nil t)
    (add-hook 'after-save-hook 'eglot--signal-textDocument/didSave nil t)
    (add-hook 'xref-backend-functions 'eglot-xref-backend nil t)
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'eglot-eldoc-function)
    (add-function :around (local imenu-create-index-function) #'eglot-imenu))
   (t
    (remove-hook 'jsonrpc-find-connection-functions 'eglot--find-current-server t)
    (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)
    (remove-hook 'after-change-functions 'eglot--after-change t)
    (remove-hook 'before-change-functions 'eglot--before-change t)
    (remove-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-revert-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-save-hook 'eglot--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook 'eglot--signal-textDocument/didSave t)
    (remove-hook 'xref-backend-functions 'eglot-xref-backend t)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (remove-function (local 'eldoc-documentation-function)
                     #'eglot-eldoc-function)
    (remove-function (local imenu-create-index-function) #'eglot-imenu)
    (setq eglot--current-flymake-report-fn nil))))

(defun eglot--managed-mode-onoff (server arg)
  "Proxy for function `eglot--managed-mode' with ARG and SERVER."
  (eglot--managed-mode arg)
  (let ((buf (current-buffer)))
    (if eglot--managed-mode
        (cl-pushnew buf (eglot--managed-buffers server))
      (setf (eglot--managed-buffers server)
            (delq buf (eglot--managed-buffers server))))))

(add-hook 'eglot--managed-mode-hook 'flymake-mode)
(add-hook 'eglot--managed-mode-hook 'eldoc-mode)

(defun eglot--find-current-server ()
  "Find the current logical EGLOT server."
  (let* ((probe (or (project-current) `(transient . ,default-directory))))
    (cl-find major-mode (gethash probe eglot--servers-by-project)
             :key #'eglot--major-mode)))

(defun eglot--maybe-activate-editing-mode (&optional server)
  "Maybe activate mode function `eglot--managed-mode'.
If SERVER is supplied, do it only if BUFFER is managed by it.  In
that case, also signal textDocument/didOpen."
  ;; Called even when revert-buffer-in-progress-p
  (let* ((cur (and buffer-file-name (eglot--find-current-server)))
         (server (or (and (null server) cur) (and server (eq server cur) cur))))
    (when server
      (eglot--managed-mode-onoff server 1)
      (eglot--signal-textDocument/didOpen))))

(add-hook 'find-file-hook 'eglot--maybe-activate-editing-mode)


;;; Mode-line, menu and other sugar
;;;
(defvar eglot--mode-line-format `(:eval (eglot--mode-line-format)))

(put 'eglot--mode-line-format 'risky-local-variable t)

(defun eglot--mode-line-call (what)
  "Make an interactive lambda for calling WHAT from mode-line."
  (lambda (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (call-interactively what)
      (force-mode-line-update t))))

(defun eglot--mode-line-props (thing face defs &optional prepend)
  "Helper for function `eglot--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (eglot--mode-line-call def))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize ,thing
                                         face ,face
                                         keymap ,map help-echo ,(concat prepend blurb)
                                         mouse-face mode-line-highlight))))

(defun eglot--mode-line-format ()
  "Compose the EGLOT's mode-line."
  (pcase-let* ((server (jsonrpc-current-connection))
               (nick (and server (eglot--project-nickname server)))
               (pending (and server (hash-table-count
                                     (jsonrpc--request-continuations server))))
               (`(,_id ,doing ,done-p ,detail) (and server (eglot--spinner server)))
               (`(,status ,serious-p) (and server (jsonrpc-status server))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line nil))
     (when nick
       `(":" ,(eglot--mode-line-props
               nick 'eglot-mode-line
               '((C-mouse-1 jsonrpc-stderr-buffer "go to stderr buffer")
                 (mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when serious-p
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 jsonrpc-clear-status  "clear this status"))
                     (format "An error occured: %s\n" status))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props
                     (format "%s%s" doing
                             (if detail (format ":%s" detail) ""))
                     'compilation-mode-line-run '())))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d oustanding requests" pending) 'warning
                     '((mouse-3 jsonrpc-forget-pending-continuations
                                "fahgettaboudit"))))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(cl-defmethod eglot-handle-notification
  (_server (_method (eql :window/showMessage)) &key type message)
  "Handle notification window/showMessage"
  (eglot--message (propertize "Server reports (type=%s): %s"
                              'face (if (<= type 1) 'error))
                  type message))

(cl-defmethod eglot-handle-request
  (server id (_method (eql :window/showMessageRequest)) &key type message actions)
  "Handle server request window/showMessageRequest"
  (let (reply)
    (unwind-protect
        (setq reply
              (completing-read
               (concat
                (format (propertize "[eglot] Server reports (type=%s): %s"
                                    'face (if (<= type 1) 'error))
                        type message)
                "\nChoose an option: ")
               (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
                   '("OK"))
               nil t (plist-get (elt actions 0) :title)))
      (if reply
          (jsonrpc-reply server id :result `(:title ,reply))
        (jsonrpc-reply server id
                       :error `(:code -32800 :message "User cancelled"))))))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql :window/logMessage)) &key _type _message)
  "Handle notification window/logMessage") ;; noop, use events buffer

(cl-defmethod eglot-handle-notification
  (_server (_method (eql :telemetry/event)) &rest _any)
  "Handle notification telemetry/event") ;; noop, use events buffer

(defvar-local eglot--unreported-diagnostics nil
  "Unreported diagnostics for this buffer.")

(cl-defmethod eglot-handle-notification
  (server (_method (eql :textDocument/publishDiagnostics)) &key uri diagnostics)
  "Handle notification publishDiagnostics"
  (if-let ((buffer (find-buffer-visiting (eglot--uri-to-path uri))))
      (with-current-buffer buffer
        (cl-loop
         for diag-spec across diagnostics
         collect (cl-destructuring-bind (&key range severity _group
                                              _code source message)
                     diag-spec
                   (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                     (flymake-make-diagnostic (current-buffer)
                                              beg end
                                              (cond ((<= severity 1) :error)
                                                    ((= severity 2)  :warning)
                                                    (t               :note))
                                              (concat source ": " message))))
         into diags
         finally (cond (eglot--current-flymake-report-fn
                        (funcall eglot--current-flymake-report-fn diags)
                        (setq eglot--unreported-diagnostics nil))
                       (t
                        (setq eglot--unreported-diagnostics (cons t diags))))))
    (jsonrpc--debug server "Diagnostics received for unvisited %s" uri)))

(cl-defun eglot--register-unregister (server jsonrpc-id things how)
  "Helper for `registerCapability'.
THINGS are either registrations or unregisterations."
  (dolist (thing (cl-coerce things 'list))
    (cl-destructuring-bind (&key id method registerOptions) thing
      (let (retval)
        (unwind-protect
            (setq retval (apply (intern (format "eglot--%s-%s" how method))
                                server :id id registerOptions))
          (unless (eq t (car retval))
            (cl-return-from eglot--register-unregister
              (jsonrpc-reply
               server jsonrpc-id
               :error `(:code -32601 :message ,(or (cadr retval) "sorry")))))))))
  (jsonrpc-reply server jsonrpc-id :result `(:message "OK")))

(cl-defmethod eglot-handle-request
  (server id (_method (eql :client/registerCapability)) &key registrations)
  "Handle server request client/registerCapability"
  (eglot--register-unregister server id registrations 'register))

(cl-defmethod eglot-handle-request
  (server id (_method (eql :client/unregisterCapability))
          &key unregisterations) ;; XXX: "unregisterations" (sic)
  "Handle server request client/unregisterCapability"
  (eglot--register-unregister server id unregisterations 'unregister))

(cl-defmethod eglot-handle-request
  (server id (_method (eql :workspace/applyEdit)) &key _label edit)
  "Handle server request workspace/applyEdit"
  (condition-case err
      (progn (eglot--apply-workspace-edit edit 'confirm)
             (jsonrpc-reply server id :result `(:applied )))
    (error (jsonrpc-reply server id
                          :result `(:applied :json-false)
                          :error `(:code -32001 :message (format "%s" ,err))))))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  `(:uri ,(eglot--path-to-uri buffer-file-name)))

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
         (if (string-match "\\(.*\\)-mode" (symbol-name major-mode))
             (match-string 1 (symbol-name major-mode))
           "unknown")
         :text
         (eglot--widening
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun eglot--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (list :textDocument (eglot--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position)))

(defvar-local eglot--recent-changes nil
  "Recent buffer changes as collected by `eglot--before-change'.")

(cl-defmethod jsonrpc-connection-ready-p ((_server eglot-lsp-server) _what)
  "Tell if SERVER is ready for WHAT in current buffer."
  (and (cl-call-next-method) (not eglot--recent-changes)))

(defvar-local eglot--change-idle-timer nil "Idle timer for didChange signals.")

(defun eglot--before-change (start end)
  "Hook onto `before-change-functions'.
Records START and END, crucially convert them into
LSP (line/char) positions before that information is
lost (because the after-change thingy doesn't know if newlines
were deleted/added)"
  (when (listp eglot--recent-changes)
    (push `(,(eglot--pos-to-lsp-position start)
            ,(eglot--pos-to-lsp-position end))
          eglot--recent-changes)))

(defun eglot--after-change (start end pre-change-length)
  "Hook onto `after-change-functions'.
Records START, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (if (and (listp eglot--recent-changes)
           (null (cddr (car eglot--recent-changes))))
      (setf (cddr (car eglot--recent-changes))
            `(,pre-change-length ,(buffer-substring-no-properties start end)))
    (setf eglot--recent-changes :emacs-messup))
  (when eglot--change-idle-timer (cancel-timer eglot--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq eglot--change-idle-timer
          (run-with-idle-timer
           0.5 nil (lambda () (eglot--with-live-buffer buf
                                (when eglot--managed-mode
                                  (eglot--signal-textDocument/didChange)
                                  (setq eglot--change-idle-timer nil))))))))

;; HACK! Launching a deferred sync request with outstanding changes is a
;; bad idea, since that might lead to the request never having a
;; chance to run, because `jsonrpc-connection-ready-p'.
(advice-add #'jsonrpc-request :before
            (cl-function (lambda (_proc _method _params &key deferred _timeout)
                           (when (and eglot--managed-mode deferred)
                             (eglot--signal-textDocument/didChange))))
            '((name . eglot--signal-textDocument/didChange)))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when eglot--recent-changes
    (let* ((server (jsonrpc-current-connection-or-lose))
           (sync-kind (eglot--server-capable :textDocumentSync))
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
                   vconcat `[,(list :range `(:start ,beg :end ,end)
                                    :rangeLength len :text text)]))))
      (setq eglot--recent-changes nil)
      (setf (eglot--spinner server) (list nil :textDocument/didChange t))
      (jsonrpc--call-deferred server))))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq eglot--recent-changes nil eglot--versioned-identifier 0)
  (jsonrpc-notify
   (jsonrpc-current-connection-or-lose)
   :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (jsonrpc-notify
   (jsonrpc-current-connection-or-lose)
   :textDocument/didClose `(:textDocument ,(eglot--TextDocumentIdentifier))))

(defun eglot--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (let ((server (jsonrpc-current-connection-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (jsonrpc-notify server :textDocument/willSave params)
    (when (eglot--server-capable :textDocumentSync :willSaveWaitUntil)
      (ignore-errors
        (eglot--apply-text-edits
         (jsonrpc-request server :textDocument/willSaveWaituntil params
                          :timeout 0.5))))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (jsonrpc-notify
   (jsonrpc-current-connection-or-lose)
   :textDocument/didSave
   (list
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (eglot--TextDocumentIdentifier))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "An EGLOT Flymake backend.
Calls REPORT-FN maybe if server publishes diagnostics in time."
  (setq eglot--current-flymake-report-fn report-fn)
  ;; Report anything unreported
  (when eglot--unreported-diagnostics
    (funcall report-fn (cdr eglot--unreported-diagnostics))
    (setq eglot--unreported-diagnostics nil)))

(defun eglot-xref-backend ()
  "EGLOT xref backend."
  (when (eglot--server-capable :definitionProvider) 'eglot))

(defvar eglot--xref-known-symbols nil)

(defun eglot--xref-reset-known-symbols (&rest _dummy)
  "Reset `eglot--xref-reset-known-symbols'.
DUMMY is ignored"
  (setq eglot--xref-known-symbols nil))

(advice-add 'xref-find-definitions :after #'eglot--xref-reset-known-symbols)
(advice-add 'xref-find-references :after #'eglot--xref-reset-known-symbols)

(defun eglot--xref-make (name uri position)
  "Like `xref-make' but with LSP's NAME, URI and POSITION."
  (cl-destructuring-bind (&key line character) position
    (xref-make name (xref-make-file-location
                     (eglot--uri-to-path uri)
                     ;; F!@(#*&#$)CKING OFF-BY-ONE again
                     (1+ line) character))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
  (when (eglot--server-capable :documentSymbolProvider)
    (let ((server (jsonrpc-current-connection-or-lose))
          (text-id (eglot--TextDocumentIdentifier)))
      (completion-table-with-cache
       (lambda (string)
         (setq eglot--xref-known-symbols
               (mapcar
                (jsonrpc-lambda
                    (&key name kind location containerName)
                  (propertize name
                              :textDocumentPositionParams
                              (list :textDocument text-id
                                    :position (plist-get
                                               (plist-get location :range)
                                               :start))
                              :locations (list location)
                              :kind kind
                              :containerName containerName))
                (jsonrpc-request server
                                 :textDocument/documentSymbol
                                 `(:textDocument ,text-id))))
         (all-completions string eglot--xref-known-symbols))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
  (when-let ((symatpt (symbol-at-point)))
    (propertize (symbol-name symatpt)
                :textDocumentPositionParams
                (eglot--TextDocumentPositionParams))))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot)) identifier)
  (let* ((rich-identifier
          (car (member identifier eglot--xref-known-symbols)))
         (location-or-locations
          (if rich-identifier
              (get-text-property 0 :locations rich-identifier)
            (jsonrpc-request (jsonrpc-current-connection-or-lose)
                             :textDocument/definition
                             (get-text-property
                              0 :textDocumentPositionParams identifier)))))
    (mapcar (jsonrpc-lambda (&key uri range)
              (eglot--xref-make identifier uri (plist-get range :start)))
            location-or-locations)))

(cl-defmethod xref-backend-references ((_backend (eql eglot)) identifier)
  (unless (eglot--server-capable :referencesProvider)
    (cl-return-from xref-backend-references nil))
  (let ((params
         (or (get-text-property 0 :textDocumentPositionParams identifier)
             (let ((rich (car (member identifier eglot--xref-known-symbols))))
               (and rich (get-text-property 0 :textDocumentPositionParams rich))))))
    (unless params
      (eglot--error "Don' know where %s is in the workspace!" identifier))
    (mapcar
     (jsonrpc-lambda (&key uri range)
       (eglot--xref-make identifier uri (plist-get range :start)))
     (jsonrpc-request (jsonrpc-current-connection-or-lose)
                      :textDocument/references
                      (append
                       params
                       (list :context
                             (list :includeDeclaration t)))))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (mapcar
     (jsonrpc-lambda (&key name location &allow-other-keys)
       (cl-destructuring-bind (&key uri range) location
         (eglot--xref-make name uri (plist-get range :start))))
     (jsonrpc-request (jsonrpc-current-connection-or-lose)
                      :workspace/symbol
                      `(:query ,pattern)))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (server (jsonrpc-current-connection-or-lose)))
    (when (eglot--server-capable :completionProvider)
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (completion-table-with-cache
        (lambda (_ignored)
          (let* ((resp (jsonrpc-request server
                                        :textDocument/completion
                                        (eglot--TextDocumentPositionParams)
                                        :deferred :textDocument/completion))
                 (items (if (vectorp resp) resp (plist-get resp :items))))
            (mapcar
             (jsonrpc-lambda (&rest all &key label insertText &allow-other-keys)
               (let ((insert (or insertText label)))
                 (add-text-properties 0 1 all insert) insert))
             items))))
       :annotation-function
       (lambda (obj)
         (cl-destructuring-bind (&key detail documentation kind &allow-other-keys)
             (text-properties-at 0 obj)
           (concat " " (propertize
                        (or (and documentation
                                 (replace-regexp-in-string "\n.*" "" documentation))
                            detail (cdr (assoc kind eglot--kind-names)))
                        'face 'font-lock-function-name-face))))
       :display-sort-function
       (lambda (items)
         (sort items (lambda (a b)
                       (string-lessp
                        (or (get-text-property 0 :sortText a) "")
                        (or (get-text-property 0 :sortText b) "")))))
       :company-doc-buffer
       (lambda (obj)
         (let ((documentation
                (or (get-text-property 0 :documentation obj)
                    (and (eglot--server-capable :completionProvider
                                                :resolveProvider)
                         (plist-get (jsonrpc-request server :completionItem/resolve
                                                     (text-properties-at 0 obj))
                                    :documentation)))))
           (when documentation
             (with-current-buffer (get-buffer-create " *eglot doc*")
               (insert (eglot--format-markup documentation))
               (current-buffer)))))
       :exit-function (lambda (_string _status)
                        (eglot--signal-textDocument/didChange)
                        (eglot-eldoc-function))))))

(defvar eglot--highlights nil "Overlays for textDocument/documentHighlight.")

(defun eglot--hover-info (contents &optional range)
  (let ((heading (and range (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                              (concat (buffer-substring beg end)  ": "))))
        (body (mapconcat #'eglot--format-markup
                         (append (cond ((vectorp contents) contents)
                                       ((stringp contents) (list contents)))) "\n")))
    (when (or heading (cl-plusp (length body))) (concat heading body))))

(defun eglot--sig-info (sigs active-sig active-param)
  (cl-loop
   for (sig . moresigs) on (append sigs nil) for i from 0
   concat (cl-destructuring-bind (&key label _documentation parameters) sig
            (let (active-doc)
              (concat
               (propertize (replace-regexp-in-string "(.*$" "(" label)
                           'face 'font-lock-function-name-face)
               (cl-loop
                for (param . moreparams) on (append parameters nil) for j from 0
                concat (cl-destructuring-bind (&key label documentation) param
                         (when (and (eql j active-param) (eql i active-sig))
                           (setq label (propertize
                                        label
                                        'face 'eldoc-highlight-function-argument))
                           (when documentation
                             (setq active-doc (concat label ": " documentation))))
                         label)
                if moreparams concat ", " else concat ")")
               (when active-doc (concat "\n" active-doc)))))
   when moresigs concat "\n"))

(defun eglot-help-at-point ()
  "Request \"hover\" information for the thing at point."
  (interactive)
  (cl-destructuring-bind (&key contents range)
      (jsonrpc-request (jsonrpc-current-connection-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (let ((blurb (eglot--hover-info contents range)))
      (with-help-window "*eglot help*"
        (with-current-buffer standard-output (insert blurb))))))

(defun eglot-eldoc-function ()
  "EGLOT's `eldoc-documentation-function' function.
If SKIP-SIGNATURE, don't try to send textDocument/signatureHelp."
  (let* ((buffer (current-buffer))
         (server (jsonrpc-current-connection-or-lose))
         (position-params (eglot--TextDocumentPositionParams))
         sig-showing)
    (cl-macrolet ((when-buffer-window
                   (&body body) ; notice the exception when testing with `ert'
                   `(when (or (get-buffer-window buffer) (ert-running-test))
                      (with-current-buffer buffer ,@body))))
      (when (eglot--server-capable :signatureHelpProvider)
        (jsonrpc-async-request
         server :textDocument/signatureHelp position-params
         :success-fn
         (jsonrpc-lambda (&key signatures activeSignature
                               activeParameter)
           (when-buffer-window
            (when (cl-plusp (length signatures))
              (setq sig-showing t)
              (eldoc-message (eglot--sig-info signatures
                                              activeSignature
                                              activeParameter)))))
         :deferred :textDocument/signatureHelp))
      (when (eglot--server-capable :hoverProvider)
        (jsonrpc-async-request
         server :textDocument/hover position-params
         :success-fn (jsonrpc-lambda (&key contents range)
                       (unless sig-showing
                         (when-buffer-window
                          (when-let (info (eglot--hover-info contents range))
                            (eldoc-message info)))))
         :deferred :textDocument/hover))
      (when (eglot--server-capable :documentHighlightProvider)
        (jsonrpc-async-request
         server :textDocument/documentHighlight position-params
         :success-fn
         (lambda (highlights)
           (mapc #'delete-overlay eglot--highlights)
           (setq eglot--highlights
                 (when-buffer-window
                  (mapcar
                   (jsonrpc-lambda (&key range _kind _role)
                     (pcase-let ((`(,beg ,end)
                                  (eglot--range-region range)))
                       (let ((ov (make-overlay beg end)))
                         (overlay-put ov 'face 'highlight)
                         (overlay-put ov 'evaporate t)
                         ov)))
                   highlights))))
         :deferred :textDocument/documentHighlight))))
  nil)

(defun eglot-imenu (oldfun)
  "EGLOT's `imenu-create-index-function' overriding OLDFUN."
  (if (eglot--server-capable :documentSymbolProvider)
      (let ((entries
             (mapcar
              (jsonrpc-lambda
                  (&key name kind location _containerName)
                (cons (propertize name :kind (cdr (assoc kind eglot--kind-names)))
                      (eglot--lsp-position-to-point
                       (plist-get (plist-get location :range) :start))))
              (jsonrpc-request (jsonrpc-current-connection-or-lose)
                               :textDocument/documentSymbol
                               `(:textDocument ,(eglot--TextDocumentIdentifier))))))
        (append
         (seq-group-by (lambda (e) (get-text-property 0 :kind (car e)))
                       entries)
         entries))
    (funcall oldfun)))

(defun eglot--apply-text-edits (edits &optional version)
  "Apply EDITS for current buffer if at VERSION, or if it's nil."
  (unless (or (not version) (equal version eglot--versioned-identifier))
    (eglot--error "Edits on `%s' require version %d, you have %d"
                  (current-buffer) version eglot--versioned-identifier))
  (eglot--widening
   (mapc (pcase-lambda (`(,newText ,beg . ,end))
           (goto-char beg) (delete-region beg end) (insert newText))
         (mapcar (jsonrpc-lambda (&key range newText)
                   (cons newText (eglot--range-region range 'markers)))
                 edits)))
  (eglot--message "%s: Performed %s edits" (current-buffer) (length edits)))

(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (cl-destructuring-bind (&key changes documentChanges) wedit
    (let ((prepared
           (mapcar (jsonrpc-lambda (&key textDocument edits)
                     (cl-destructuring-bind (&key uri version) textDocument
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
      (unwind-protect
          (let (edit) (while (setq edit (car prepared))
                        (cl-destructuring-bind (path edits &optional version) edit
                          (with-current-buffer (find-file-noselect path)
                            (eglot--apply-text-edits edits version))
                          (pop prepared))))
        (if prepared (eglot--warn "Caution: edits of files %s failed."
                                  (mapcar #'car prepared))
          (eglot--message "Edit successful!"))))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (jsonrpc-request (jsonrpc-current-connection-or-lose)
                    :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                           :newName ,newname))
   current-prefix-arg))


;;; Dynamic registration
;;;
(cl-defun eglot--register-workspace/didChangeWatchedFiles (server &key id watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles"
  (eglot--unregister-workspace/didChangeWatchedFiles server :id id)
  (let* (success
         (globs (mapcar (lambda (w) (plist-get w :globPattern)) watchers)))
    (cl-labels
        ((handle-event
          (event)
          (cl-destructuring-bind (desc action file &optional file1) event
            (cond
             ((and (memq action '(created changed deleted))
                   (cl-find file globs
                            :test (lambda (f glob)
                                    (string-match (wildcard-to-regexp
                                                   (expand-file-name glob))
                                                  f))))
              (jsonrpc-notify
               server :workspace/didChangeWatchedFiles
               `(:changes ,(vector `(:uri ,(eglot--path-to-uri file)
                                          :type ,(cl-case action
                                                   (created 1)
                                                   (changed 2)
                                                   (deleted 3)))))))
             ((eq action 'renamed)
              (handle-event desc 'deleted file)
              (handle-event desc 'created file1))))))
      (unwind-protect
          (progn (dolist (dir (delete-dups (mapcar #'file-name-directory globs)))
                   (push (file-notify-add-watch dir '(change) #'handle-event)
                         (gethash id (eglot--file-watches server))))
                 (setq success `(t "OK")))
        (unless success
          (eglot--unregister-workspace/didChangeWatchedFiles server :id id))))))

(cl-defun eglot--unregister-workspace/didChangeWatchedFiles (server &key id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles"
  (mapc #'file-notify-rm-watch (gethash id (eglot--file-watches server)))
  (remhash id (eglot--file-watches server))
  (list t "OK"))


;;; Rust-specific
;;;
(defclass eglot-rls (eglot-lsp-server) () :documentation "Rustlang's RLS.")

(cl-defmethod jsonrpc-connection-ready-p ((server eglot-rls) what)
  "Except for :completion, RLS isn't ready until Indexing done."
  (and (cl-call-next-method)
       (or ;; RLS normally ready for this, even if building.
        (eq :textDocument/completion what)
        (pcase-let ((`(,_id ,what ,done ,_detail) (eglot--spinner server)))
          (and (equal "Indexing" what) done)))))

(cl-defmethod eglot-handle-notification
  ((server eglot-rls) (_method (eql :window/progress))
   &key id done title message &allow-other-keys)
  "Handle notification window/progress"
  (setf (eglot--spinner server) (list id title done message)))


;;; cquery-specific
;;;
(defclass eglot-cquery (eglot-lsp-server) ()
  :documentation "cquery's C/C++ langserver.")

(cl-defmethod eglot-initialization-options ((server eglot-cquery))
  "Passes through required cquery initialization options"
  (let* ((root (car (project-roots (eglot--project server))))
         (cache (expand-file-name ".cquery_cached_index/" root)))
    (vector :cacheDirectory (file-name-as-directory cache)
            :progressReportFrequencyMs -1)))

(cl-defmethod eglot-handle-notification
  ((_server eglot-cquery) (_method (eql :$cquery/progress))
   &rest counts &key _activeThreads &allow-other-keys)
  "No-op for noisy $cquery/progress extension")

(cl-defmethod eglot-handle-notification
  ((_server eglot-cquery) (_method (eql :$cquery/setInactiveRegions))
   &key _uri _inactiveRegions &allow-other-keys)
  "No-op for unsupported $cquery/setInactiveRegions extension")

(cl-defmethod eglot-handle-notification
  ((_server eglot-cquery) (_method (eql :$cquery/publishSemanticHighlighting))
   &key _uri _symbols &allow-other-keys)
  "No-op for unsupported $cquery/publishSemanticHighlighting extension")

(provide 'eglot)
;;; eglot.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
