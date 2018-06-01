;;; eglot.el --- Client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Version: 0.8
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

(defcustom eglot-request-timeout 10
  "How many seconds to wait for a reply from the server."
  :type :integer)

(defcustom eglot-autoreconnect 3
  "Control EGLOT's ability to reconnect automatically.
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

(cl-defgeneric eglot-server-ready-p (server what) ;; API
  "Tell if SERVER is ready for WHAT in current buffer.
If it isn't, a deferrable `eglot--async-request' *will* be
deferred to the future.")

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
                        :executeCommand `(:dynamicRegistration :json-false)
                        :codeAction `(:dynamicRegistration :json-false)
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


;;; Process management
(defvar eglot--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defclass eglot-lsp-server ()
  ((process
    :documentation "Wrapped process object."
    :initarg :process :accessor eglot--process)
   (name
    :documentation "Readable name used for naming processes, buffers, etc..."
    :initarg :name :accessor eglot--name)
   (project-nickname
    :documentation "Short nickname for the associated project."
    :initarg :project-nickname :accessor eglot--project-nickname)
   (major-mode
    :documentation "Major mode symbol."
    :initarg :major-mode :accessor eglot--major-mode)
   (pending-continuations
    :documentation "Map request ID's to (SUCCESS-FN ERROR-FN TIMEOUT-FN) triads."
    :initform (make-hash-table) :accessor eglot--pending-continuations)
   (events-buffer
    :documentation "Buffer holding a log of server-related events."
    :accessor eglot--events-buffer)
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
   (status
    :documentation "List (STATUS SERIOUS-P) representing server problems/status."
    :initform `(:unknown nil) :accessor eglot--status)
   (inhibit-autoreconnect
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :initarg :inhibit-autoreconnect :accessor eglot--inhibit-autoreconnect)
   (contact
    :documentation "How server was started and how it can be re-started."
    :initarg :contact :accessor eglot--contact)
   (deferred-actions
     :documentation "Map (DEFERRED BUF) to (FN TIMER ID).  FN is a saved\
DEFERRED request from BUF, to be sent not later than TIMER as ID."
     :initform (make-hash-table :test #'equal) :accessor eglot--deferred-actions)
   (file-watches
    :documentation "Map ID to list of WATCHES for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor eglot--file-watches)
   (managed-buffers
    :documentation "List of buffers managed by server."
    :initarg :managed-buffers :accessor eglot--managed-buffers))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")

(cl-defmethod cl-print-object ((obj eglot-lsp-server) stream)
  (princ (format "#<%s: %s>" (eieio-object-class obj) (eglot--name obj)) stream))

(defun eglot--current-server ()
  "The current logical EGLOT process."
  (let* ((probe (or (project-current) `(transient . ,default-directory))))
    (cl-find major-mode (gethash probe eglot--servers-by-project)
             :key #'eglot--major-mode)))

(defun eglot--current-server-or-lose ()
  "Return the current EGLOT process or error."
  (or (eglot--current-server) (eglot--error "No current EGLOT process")))

(defun eglot--make-process (name contact)
  "Make a process object from CONTACT.
NAME is used to name the the started process or connection.
CONTACT is in `eglot'.  Returns a process object."
  (let* ((stdout (format "*%s stdout*" name)) stderr
         (proc (cond
                ((processp contact) contact)
                ((integerp (cadr contact))
                 (apply #'open-network-stream name stdout contact))
                (t (make-process
                    :name name :command contact :buffer stdout
                    :coding 'no-conversion :connection-type 'pipe
                    :stderr (setq stderr (format "*%s stderr*" name)))))))
    (process-put proc 'eglot-stderr stderr)
    (set-process-buffer proc (get-buffer-create stdout))
    (set-marker (process-mark proc) (with-current-buffer stdout (point-min)))
    (set-process-filter proc #'eglot--process-filter)
    (set-process-sentinel proc #'eglot--process-sentinel)
    (with-current-buffer stdout
      (let ((inhibit-read-only t)) (erase-buffer) (read-only-mode t)))
    proc))

(defun eglot--all-major-modes ()
  "Return all know major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defvar eglot-connect-hook nil "Hook run after connecting in `eglot--connect'.")

(defun eglot--connect (project managed-major-mode contact server-class)
  "Connect for PROJECT, MANAGED-MAJOR-MODE and CONTACT.
INTERACTIVE is t if inside interactive call.  Return an object of
class SERVER-CLASS."
  (let* ((nickname (file-name-base (directory-file-name
                                    (car (project-roots project)))))
         (name (format "EGLOT (%s/%s)" nickname managed-major-mode))
         (proc (eglot--make-process
                name (if (functionp contact) (funcall contact) contact)))
         server connect-success)
    (setq server
          (make-instance
           (or server-class 'eglot-lsp-server)
           :process proc :major-mode managed-major-mode
           :project project :contact contact
           :name name :project-nickname nickname
           :inhibit-autoreconnect
           (cond
            ((booleanp eglot-autoreconnect) (not eglot-autoreconnect))
            ((cl-plusp eglot-autoreconnect)
             (run-with-timer eglot-autoreconnect nil
                             (lambda ()
                               (setf (eglot--inhibit-autoreconnect server)
                                     (null eglot-autoreconnect))))))))
    (push server (gethash project eglot--servers-by-project))
    (process-put proc 'eglot-server server)
    (unwind-protect
        (cl-destructuring-bind (&key capabilities)
            (eglot--request
             server
             :initialize
             (list
              :processId (unless (eq (process-type proc) 'network) (emacs-pid))
              :capabilities (eglot-client-capabilities server)
              :rootPath  (expand-file-name (car (project-roots project)))
              :rootUri  (eglot--path-to-uri (car (project-roots project)))
              :initializationOptions (eglot-initialization-options server)))
          (setf (eglot--capabilities server) capabilities)
          (setf (eglot--status server) nil)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (eglot--maybe-activate-editing-mode server)))
          (eglot--notify server :initialized `(:__dummy__ t))
          (run-hook-with-args 'eglot-connect-hook server)
          (setq connect-success server))
      (unless (or connect-success
                  (not (process-live-p proc)))
        (eglot-shutdown server)))))

(defvar eglot--command-history nil
  "History of COMMAND arguments to `eglot'.")

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
         (class (and (consp guess) (symbolp (car guess))
                     (prog1 (car guess) (setq guess (cdr guess)))))
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
    (list managed-mode project contact class t)))

;;;###autoload
(defun eglot (managed-major-mode project command server-class
                                 &optional interactive)
  "Manage a project with a Language Server Protocol (LSP) server.

The LSP server is started (or contacted) via COMMAND.  If this
operation is successful, current *and future* file buffers of
MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', `completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, COMMAND from `eglot-server-programs' and
PROJECT from `project-current'.  If it can't guess, the user is
prompted.  With a single \\[universal-argument] prefix arg, it
always prompt for COMMAND.  With two \\[universal-argument]
prefix args, also prompts for MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

COMMAND is a list of strings, an executable program and
optionally its arguments.  If the first and only string in the
list is of the form \"<host>:<port>\" it is taken as an
indication to connect to a server instead of starting one.  This
is also know as the server's \"contact\".

SERVER-CLASS is a symbol naming a class that must inherit from
`eglot-server', or nil to use the default server class.

INTERACTIVE is t if called interactively."
  (interactive (eglot--interactive))
  (let ((current-server (eglot--current-server)))
    (if (and current-server
             (process-live-p (eglot--process current-server))
             interactive
             (y-or-n-p "[eglot] Live process found, reconnect instead? "))
        (eglot-reconnect current-server interactive)
      (when (and current-server
                 (process-live-p (eglot--process current-server)))
        (ignore-errors (eglot-shutdown current-server)))
      (let ((server (eglot--connect project
                                    managed-major-mode
                                    command
                                    server-class)))
        (eglot--message "Connected! Server `%s' now \
managing `%s' buffers in project `%s'."
                        (eglot--name server) managed-major-mode
                        (eglot--project-nickname server))
        server))))

(defun eglot-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-server-or-lose) t))
  (when (process-live-p (eglot--process server))
    (ignore-errors (eglot-shutdown server interactive)))
  (eglot--connect (eglot--project server)
                  (eglot--major-mode server)
                  (eglot--contact server)
                  (eieio-object-class server))
  (eglot--message "Reconnected!"))

(defun eglot--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (let ((server (process-get proc 'eglot-server)))
    (eglot--debug server "Process state changed: %s" change)
    (when (not (process-live-p proc))
      (with-current-buffer (eglot-events-buffer server)
        (let ((inhibit-read-only t))
          (insert "\n----------b---y---e---b---y---e----------\n")))
      ;; Cancel outstanding timers and file system watches
      (maphash (lambda (_id triplet)
                 (cl-destructuring-bind (_success _error timeout) triplet
                   (cancel-timer timeout)))
               (eglot--pending-continuations server))
      (maphash (lambda (_id watches)
                 (mapcar #'file-notify-rm-watch watches))
               (eglot--file-watches server))
      (unwind-protect
          ;; Call all outstanding error handlers
          (maphash (lambda (_id triplet)
                     (cl-destructuring-bind (_success error _timeout) triplet
                       (funcall error `(:code -1 :message "Server died"))))
                   (eglot--pending-continuations server))
        ;; Turn off `eglot--managed-mode' where appropriate.
        (dolist (buffer (eglot--managed-buffers server))
          (with-current-buffer buffer (eglot--managed-mode-onoff server -1)))
        ;; Forget about the process-project relationship
        (setf (gethash (eglot--project server) eglot--servers-by-project)
              (delq server
                    (gethash (eglot--project server) eglot--servers-by-project)))
        ;; Say last words
        (eglot--message "%s exited with status %s" (eglot--name server)
                        (process-exit-status
                         (eglot--process server)))
        (delete-process proc)
        ;; Consider autoreconnecting
        (cond ((eglot--shutdown-requested server)
               (setf (eglot--shutdown-requested server) :sentinel-done))
              ((not (eglot--inhibit-autoreconnect server))
               (eglot--warn "Reconnecting after unexpected server exit")
               (eglot-reconnect server))
              ((timerp (eglot--inhibit-autoreconnect server))
               (eglot--warn "Not auto-reconnecting, last on didn't last long.")))))))

(defun eglot--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (eglot--with-live-buffer (process-buffer proc)
    (let ((expected-bytes (process-get proc 'eglot-expected-bytes))
          (inhibit-read-only t) done)
      ;; Insert the text, advancing the process marker.
      ;;
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      ;; Loop (more than one message might have arrived)
      ;;
      (unwind-protect
          (while (not done)
            (cond ((not expected-bytes)
                   ;; Starting a new message
                   ;;
                   (setq expected-bytes
                         (and (search-forward-regexp
                               "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                               (+ (point) 100)
                               t)
                              (string-to-number (match-string 1))))
                   (unless expected-bytes
                     (setq done :waiting-for-new-message)))
                  (t
                   ;; Attempt to complete a message body
                   ;;
                   (let ((available-bytes (- (position-bytes (process-mark proc))
                                             (position-bytes (point)))))
                     (cond
                      ((>= available-bytes
                           expected-bytes)
                       (let* ((message-end (byte-to-position
                                            (+ (position-bytes (point))
                                               expected-bytes))))
                         (unwind-protect
                             (save-restriction
                               (narrow-to-region (point) message-end)
                               (let* ((json-object-type 'plist)
                                      (json-message (json-read)))
                                 ;; Process content in another buffer,
                                 ;; shielding buffer from tamper
                                 ;;
                                 (with-temp-buffer
                                   (eglot--server-receive
                                    (process-get proc 'eglot-server)
                                    json-message))))
                           (goto-char message-end)
                           (delete-region (point-min) (point))
                           (setq expected-bytes nil))))
                      (t
                       ;; Message is still incomplete
                       ;;
                       (setq done :waiting-for-more-bytes-in-this-message)))))))
        ;; Saved parsing state for next visit to this filter
        ;;
        (process-put proc 'eglot-expected-bytes expected-bytes)))))

(defun eglot-events-buffer (server &optional interactive)
  "Display events buffer for current LSP SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-server-or-lose) t))
  (let* ((probe (eglot--events-buffer server))
         (buffer (or (and (buffer-live-p probe) probe)
                     (let ((buffer (get-buffer-create
                                    (format "*%s events*"
                                            (eglot--name server)))))
                       (with-current-buffer buffer
                         (buffer-disable-undo)
                         (read-only-mode t)
                         (setf (eglot--events-buffer server) buffer))
                       buffer))))
    (when interactive (display-buffer buffer))
    buffer))

(defun eglot-stderr-buffer (server)
  "Pop to stderr of SERVER, if it exists, else error."
  (interactive (list (eglot--current-server-or-lose)))
  (if-let ((b (process-get (eglot--process server) 'eglot-stderr)))
      (pop-to-buffer b) (user-error "[eglot] No stderr buffer!")))

(defun eglot--log-event (server message &optional type)
  "Log an eglot-related event.
SERVER is the current server.  MESSAGE is a JSON-like plist.
TYPE is a symbol saying if this is a client or server
originated."
  (with-current-buffer (eglot-events-buffer server)
    (cl-destructuring-bind (&key method id error &allow-other-keys) message
      (let* ((inhibit-read-only t)
             (subtype (cond ((and method id)       'request)
                            (method                'notification)
                            (id                    'reply)
                            (t                     'message)))
             (type
              (format "%s-%s" (or type :internal) subtype)))
        (goto-char (point-max))
        (let ((msg (format "%s%s%s:\n%s\n"
                           type
                           (if id (format " (id:%s)" id) "")
                           (if error " ERROR" "")
                           (pp-to-string message))))
          (when error
            (setq msg (propertize msg 'face 'error)))
          (insert-before-markers msg))))))

(defun eglot--server-receive (server message)
  "Process MESSAGE from SERVER."
  (cl-destructuring-bind (&key method id params error result _jsonrpc) message
    (let* ((continuations (and id
                               (not method)
                               (gethash id (eglot--pending-continuations server)))))
      (eglot--log-event server message 'server)
      (when error (setf (eglot--status server) `(,error t)))
      (unless (or (null method) (keywordp method))
        (setq method (intern (format ":%s" method))))
      (cond
       (method
        (condition-case-unless-debug _err
            (if id
                (apply #'eglot-handle-request server id method params)
              (apply #'eglot-handle-notification server method params))
          (cl-no-applicable-method
           (if id
               (eglot--reply
                server id :error `(:code -32601 :message "Method unimplemented"))
             (eglot--debug
              server '(:error `(:message "Notification unimplemented")))))))
       (continuations
        (cancel-timer (cl-third continuations))
        (remhash id (eglot--pending-continuations server))
        (if error
            (funcall (cl-second continuations) error)
          (funcall (cl-first continuations) result)))
       (id
        (eglot--warn "Ooops no continuation for id %s" id)))
      (eglot--call-deferred server)
      (force-mode-line-update t))))

(defun eglot--send (server message)
  "Send MESSAGE to SERVER (ID is optional)."
  (let ((json (json-encode message)))
    (process-send-string (eglot--process server)
                         (format "Content-Length: %d\r\n\r\n%s"
                                 (string-bytes json) json))
    (eglot--log-event server message 'client)))

(defun eglot-forget-pending-continuations (server)
  "Stop waiting for responses from the current LSP SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (clrhash (eglot--pending-continuations server)))

(defun eglot-clear-status (server)
  "Clear most recent error message from SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (setf (eglot--status server) nil)
  (force-mode-line-update t))

(defun eglot--call-deferred (server)
  "Call SERVER's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (eglot--deferred-actions server))))
    (eglot--debug server `(:maybe-run-deferred ,(mapcar #'caddr actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defvar-local eglot--next-request-id 0 "ID for next `eglot--async-request'.")

(cl-defun eglot--async-request (server
                                method
                                params
                                &rest args
                                &key success-fn error-fn timeout-fn
                                (timeout eglot-request-timeout)
                                (deferred nil))
  "Make a request to SERVER expecting a reply later on.
SUCCESS-FN and ERROR-FN are passed `:result' and `:error'
objects, respectively.  Wait TIMEOUT seconds for response or call
nullary TIMEOUT-FN.  If DEFERRED, maybe defer request to the
future, or to never at all, in case a new request with identical
DEFERRED and for the same buffer overrides it (however, if that
happens, the original timer keeps counting). Return (ID TIMER)."
  (pcase-let* ( (buf (current-buffer))
                (`(,_ ,timer ,old-id)
                 (and deferred (gethash (list deferred buf)
                                        (eglot--deferred-actions server))))
                (id (or old-id (cl-incf eglot--next-request-id)))
                (make-timer
                 (lambda ( )
                   (run-with-timer
                    timeout nil
                    (lambda ()
                      (remhash id (eglot--pending-continuations server))
                      (if timeout-fn (funcall timeout-fn)
                        (eglot--debug
                         server `(:timed-out ,method :id ,id :params ,params))))))))
    (when deferred
      (if (eglot-server-ready-p server deferred)
          ;; Server is ready, we jump below and send it immediately.
          (remhash (list deferred buf) (eglot--deferred-actions server))
        ;; Otherwise, save in `eglot--deferred-actions' and exit non-locally
        (unless old-id
          ;; Also, if it's the first deferring for this id, inform the log
          (eglot--debug server `(:deferring ,method :id ,id :params ,params)))
        (puthash (list deferred buf)
                 (list (lambda () (eglot--with-live-buffer buf
                                    (apply #'eglot--async-request server
                                           method params args)))
                       (or timer (funcall make-timer)) id)
                 (eglot--deferred-actions server))
        (cl-return-from eglot--async-request nil)))
    ;; Really send the request
    (eglot--send server `(:jsonrpc "2.0" :id ,id :method ,method :params ,params))
    (puthash id (list
                 (or success-fn
                     (eglot--lambda (&rest _ignored)
                       (eglot--debug
                        server `(:message "success ignored" :id ,id))))
                 (or error-fn
                     (eglot--lambda (&key code message &allow-other-keys)
                       (setf (eglot--status server) `(,message t))
                       server `(:message "error ignored, status set"
                                         :id ,id :error ,code)))
                 (or timer (funcall make-timer)))
             (eglot--pending-continuations server))
    (list id timer)))

(defun eglot--request (server method params &optional deferred)
  "Like `eglot--async-request' for SERVER, METHOD and PARAMS, but synchronous.
Meaning only return locally if successful, otherwise exit non-locally.
DEFERRED is passed to `eglot--async-request', which see."
  ;; HACK: A deferred sync request with outstanding changes is a bad
  ;; idea, since that might lead to the request never having a chance
  ;; to run, because idle timers don't run in `accept-process-output'.
  (when deferred (eglot--signal-textDocument/didChange))
  (let* ((done (make-symbol "eglot-catch")) id-and-timer
         (res
          (unwind-protect
              (catch done
                (setq
                 id-and-timer
                 (eglot--async-request
                  server method params
                  :success-fn (lambda (result) (throw done `(done ,result)))
                  :timeout-fn (lambda () (throw done '(error "Timed out")))
                  :error-fn (eglot--lambda (&key code message _data)
                              (throw done `(error
                                            ,(format "Ooops: %s: %s" code message))))
                  :deferred deferred))
                (while t (accept-process-output nil 30)))
            (pcase-let ((`(,id ,timer) id-and-timer))
              (when id (remhash id (eglot--pending-continuations server)))
              (when timer (cancel-timer timer))))))
    (when (eq 'error (car res)) (eglot--error (cadr res)))
    (cadr res)))

(cl-defun eglot--notify (server method params)
  "Notify SERVER of something, don't expect a reply.e"
  (eglot--send server `(:jsonrpc  "2.0" :method ,method :params ,params)))

(cl-defun eglot--reply (server id &key result error)
  "Reply to PROCESS's request ID with MESSAGE."
  (eglot--send
   server `(:jsonrpc  "2.0" :id ,id
                      ,@(when result `(:result ,result))
                      ,@(when error `(:error ,error)))))


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

(defun eglot--debug (server format &rest args)
  "Debug message for SERVER with FORMAT and ARGS."
  (eglot--log-event
   server (if (stringp format)`(:message ,(format format args)) format)))

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
  (cl-loop for caps = (eglot--capabilities (eglot--current-server-or-lose))
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

(defun eglot--maybe-activate-editing-mode (&optional server)
  "Maybe activate mode function `eglot--managed-mode'.
If SERVER is supplied, do it only if BUFFER is managed by it.  In
that case, also signal textDocument/didOpen."
  ;; Called even when revert-buffer-in-progress-p
  (let* ((cur (and buffer-file-name (eglot--current-server)))
         (server (or (and (null server) cur) (and server (eq server cur) cur))))
    (when server
      (eglot--managed-mode-onoff server 1)
      (eglot--signal-textDocument/didOpen))))

(add-hook 'find-file-hook 'eglot--maybe-activate-editing-mode)


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
                                           (call-interactively what))))))

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
  (pcase-let* ((server (eglot--current-server))
               (name (and
                      server
                      (eglot--project-nickname server)))
               (pending (and server (hash-table-count
                                     (eglot--pending-continuations server))))
               (`(,_id ,doing ,done-p ,detail) (and server (eglot--spinner server)))
               (`(,status ,serious-p) (and server (eglot--status server))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line nil))
     (when name
       `(":" ,(eglot--mode-line-props
               name 'eglot-mode-line
               '((C-mouse-1 eglot-stderr-buffer "go to stderr buffer")
                 (mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when serious-p
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 eglot-clear-status  "clear this status"))
                     (format "An error occured: %s\n" status))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props
                     (format "%s%s" doing
                             (if detail (format ":%s" detail) ""))
                     'compilation-mode-line-run '())))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-3 eglot-forget-pending-continuations
                                "forget these continuations"))
                     (format "%d pending requests\n" pending)))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))


;; FIXME: A horrible hack of Flymake's insufficient API that must go
;; into Emacs master, or better, 26.2
(cl-defstruct (eglot--diag (:include flymake--diag)
                           (:constructor eglot--make-diag
                                         (buffer beg end type text props)))
  props)

(advice-add 'flymake--highlight-line :after
            (lambda (diag)
              (when (cl-typep diag 'eglot--diag)
                (let ((ov (cl-find diag
                                   (overlays-at (flymake-diagnostic-beg diag))
                                   :key (lambda (ov)
                                          (overlay-get ov 'flymake-diagnostic)))))
                  (cl-loop for (key . value) in (eglot--diag-props diag)
                           do (overlay-put ov key value)))))
            '((name . eglot-hacking-in-some-per-diag-overlay-properties)))


(defun eglot--overlay-diag-props ()
  `((mouse-face . highlight)
    (help-echo . (lambda (window _ov pos)
                   (with-selected-window window
                     (mapconcat
                      #'flymake-diagnostic-text
                      (flymake-diagnostics pos)
                      "\n"))))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (define-key map [mouse-1]
                   (eglot--mouse-call 'eglot-code-actions))
                 map))))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(defun eglot-shutdown (server &optional _interactive)
  "Politely ask SERVER to quit.
Forcefully quit it if it doesn't respond.  Don't leave this
function with the server still running."
  (interactive (list (eglot--current-server-or-lose) t))
  (eglot--message "Asking %s politely to terminate" (eglot--name server))
  (unwind-protect
      (let ((eglot-request-timeout 3))
        (setf (eglot--shutdown-requested server) t)
        (eglot--request server :shutdown nil)
        ;; this one is supposed to always fail, hence ignore-errors
        (ignore-errors (eglot--request server :exit nil)))
    ;; Turn off `eglot--managed-mode' where appropriate.
    (dolist (buffer (eglot--managed-buffers server))
      (with-current-buffer buffer (eglot--managed-mode-onoff server -1)))
    (while (progn (accept-process-output nil 0.1)
                  (not (eq (eglot--shutdown-requested server) :sentinel-done)))
      (eglot--warn "Sentinel for %s still hasn't run, brutally deleting it!"
                   (eglot--process server))
      (delete-process (eglot--process server)))))

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
          (eglot--reply server id :result `(:title ,reply))
        (eglot--reply server id
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
         collect (cl-destructuring-bind (&key range ((:severity sev)) _group
                                              _code source message)
                     diag-spec
                   (setq message (concat source ": " message))
                   (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                     (eglot--make-diag (current-buffer) beg end
                                       (cond ((<= sev 1) ':error)
                                             ((= sev 2)  ':warning)
                                             (t          ':note))
                                       message (cons
                                                `(eglot-lsp-diag . ,diag-spec)
                                                (eglot--overlay-diag-props)))))
         into diags
         finally (cond (eglot--current-flymake-report-fn
                        (funcall eglot--current-flymake-report-fn diags)
                        (setq eglot--unreported-diagnostics nil))
                       (t
                        (setq eglot--unreported-diagnostics (cons t diags))))))
    (eglot--debug server "Diagnostics received for unvisited %s" uri)))

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
              (eglot--reply
               server jsonrpc-id
               :error `(:code -32601 :message ,(or (cadr retval) "sorry")))))))))
  (eglot--reply server jsonrpc-id :result `(:message "OK")))

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
             (eglot--reply server id :result `(:applied )))
    (error (eglot--reply server id
                         :result `(:applied :json-false)
                         :error `(:code -32001 :message ,(format "%s" err))))))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  (list :uri (eglot--path-to-uri buffer-file-name)))

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

(defmethod eglot-server-ready-p (_s _what)
  "Normally ready if no outstanding changes." (not eglot--recent-changes))

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

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when eglot--recent-changes
    (let* ((server (eglot--current-server-or-lose))
           (sync-kind (eglot--server-capable :textDocumentSync))
           (full-sync-p (or (eq sync-kind 1)
                            (eq :emacs-messup eglot--recent-changes))))
      (eglot--notify
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
      (eglot--call-deferred server))))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq eglot--recent-changes nil eglot--versioned-identifier 0)
  (eglot--notify
   (eglot--current-server-or-lose)
   :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (eglot--notify
   (eglot--current-server-or-lose)
   :textDocument/didClose `(:textDocument ,(eglot--TextDocumentIdentifier))))

(defun eglot--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (let ((server (eglot--current-server-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (eglot--notify server :textDocument/willSave params)
    (ignore-errors
      (let ((eglot-request-timeout 0.5))
        (when (plist-get :willSaveWaitUntil
                         (eglot--server-capable :textDocumentSync))
          (eglot--apply-text-edits
           (eglot--request server :textDocument/willSaveWaituntil params)))))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (eglot--notify
   (eglot--current-server-or-lose)
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
    (let ((server (eglot--current-server-or-lose))
          (text-id (eglot--TextDocumentIdentifier)))
      (completion-table-with-cache
       (lambda (string)
         (setq eglot--xref-known-symbols
               (mapcar
                (eglot--lambda (&key name kind location containerName)
                  (propertize name
                              :textDocumentPositionParams
                              (list :textDocument text-id
                                    :position (plist-get
                                               (plist-get location :range)
                                               :start))
                              :locations (list location)
                              :kind kind
                              :containerName containerName))
                (eglot--request
                 server :textDocument/documentSymbol `(:textDocument ,text-id))))
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
            (eglot--request (eglot--current-server-or-lose)
                            :textDocument/definition
                            (get-text-property
                             0 :textDocumentPositionParams identifier)))))
    (mapcar (eglot--lambda (&key uri range)
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
    (mapcar (eglot--lambda (&key uri range)
              (eglot--xref-make identifier uri (plist-get range :start)))
            (eglot--request (eglot--current-server-or-lose)
                            :textDocument/references
                            (append
                             params
                             `(:context (:includeDeclaration t)))))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (mapcar (eglot--lambda (&key name location &allow-other-keys)
              (cl-destructuring-bind (&key uri range) location
                (eglot--xref-make name uri (plist-get range :start))))
            (eglot--request (eglot--current-server-or-lose)
                            :workspace/symbol
                            (list :query pattern)))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (server (eglot--current-server-or-lose)))
    (when (eglot--server-capable :completionProvider)
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (completion-table-with-cache
        (lambda (_ignored)
          (let* ((resp (eglot--request server
                                       :textDocument/completion
                                       (eglot--TextDocumentPositionParams)
                                       :textDocument/completion))
                 (items (if (vectorp resp) resp (plist-get resp :items))))
            (mapcar
             (eglot--lambda (&rest all &key label insertText &allow-other-keys)
               (let ((insert (or insertText label)))
                 (add-text-properties 0 1 all insert)
                 (put-text-property 0 1 'eglot--lsp-completion all insert)
                 insert))
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
         (let* ((documentation
                 (or (get-text-property 0 :documentation obj)
                     (and (eglot--server-capable :completionProvider
                                                 :resolveProvider)
                          (plist-get
                           (eglot--request server :completionItem/resolve
                                           (get-text-property
                                            0 'eglot--lsp-completion obj))
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
  (concat (and range (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                       (concat (buffer-substring beg end)  ": ")))
          (mapconcat #'eglot--format-markup
                     (append (cond ((vectorp contents) contents)
                                   (contents (list contents)))) "\n")))

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
      (eglot--request (eglot--current-server-or-lose) :textDocument/hover
                      (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (let ((blurb (eglot--hover-info contents range)))
      (with-help-window "*eglot help*"
        (with-current-buffer standard-output (insert blurb))))))

(defun eglot-eldoc-function ()
  "EGLOT's `eldoc-documentation-function' function.
If SKIP-SIGNATURE, don't try to send textDocument/signatureHelp."
  (let* ((buffer (current-buffer))
         (server (eglot--current-server-or-lose))
         (position-params (eglot--TextDocumentPositionParams))
         sig-showing)
    (cl-macrolet ((when-buffer-window
                   (&body body)
                   `(when (or (get-buffer-window buffer) (ert-running-test))
                      (with-current-buffer buffer ,@body))))
      (when (eglot--server-capable :signatureHelpProvider)
        (eglot--async-request
         server :textDocument/signatureHelp position-params
         :success-fn (eglot--lambda (&key signatures activeSignature
                                          activeParameter)
                       (when-buffer-window
                        (when (cl-plusp (length signatures))
                          (setq sig-showing t)
                          (eldoc-message (eglot--sig-info signatures
                                                          activeSignature
                                                          activeParameter)))))
         :deferred :textDocument/signatureHelp))
      (when (eglot--server-capable :hoverProvider)
        (eglot--async-request
         server :textDocument/hover position-params
         :success-fn (eglot--lambda (&key contents range)
                       (unless sig-showing
                         (when-buffer-window
                          (eldoc-message (eglot--hover-info contents range)))))
         :deferred :textDocument/hover))
      (when (eglot--server-capable :documentHighlightProvider)
        (eglot--async-request
         server :textDocument/documentHighlight position-params
         :success-fn (lambda (highlights)
                       (mapc #'delete-overlay eglot--highlights)
                       (setq eglot--highlights
                             (when-buffer-window
                              (mapcar (eglot--lambda (&key range _kind _role)
                                        (pcase-let ((`(,beg . ,end)
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
              (eglot--lambda (&key name kind location _containerName)
                (cons (propertize name :kind (cdr (assoc kind eglot--kind-names)))
                      (eglot--lsp-position-to-point
                       (plist-get (plist-get location :range) :start))))
              (eglot--request (eglot--current-server-or-lose)
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
         (mapcar (eglot--lambda (&key range newText)
                   (cons newText (eglot--range-region range 'markers)))
                 edits)))
  (eglot--message "%s: Performed %s edits" (current-buffer) (length edits)))

(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (cl-destructuring-bind (&key changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda (&key textDocument edits)
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
          (eglot-eldoc-function)
          (eglot--message "Edit successful!"))))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (eglot--request (eglot--current-server-or-lose)
                   :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                          :newName ,newname))
   current-prefix-arg))


(defun eglot-code-actions (&optional beg end)
  "Get and offer to execute code actions between BEG and END."
  (interactive
   (let (diags)
     (cond ((region-active-p) (list (region-beginning) (region-end)))
           ((setq diags (flymake-diagnostics (point)))
            (list (cl-reduce #'min (mapcar #'flymake-diagnostic-beg diags))
                  (cl-reduce #'max (mapcar #'flymake-diagnostic-end diags))))
           (t (list (point-min) (point-max))))))
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions (eglot--request
                   server
                   :textDocument/codeAction
                   (list :textDocument (eglot--TextDocumentIdentifier)
                         :range (list :start (eglot--pos-to-lsp-position beg)
                                      :end (eglot--pos-to-lsp-position end))
                         :context
                         `(:diagnostics
                           [,@(mapcar (lambda (diag)
                                        (cdr (assoc 'eglot-lsp-diag
                                                    (eglot--diag-props diag))))
                                      (cl-remove-if-not
                                       (lambda (diag) (cl-typep diag 'eglot--diag))
                                       (flymake-diagnostics beg end)))]))))
         (menu-items (mapcar (eglot--lambda (&key title command arguments)
                               `(,title . (:command ,command :arguments ,arguments)))
                             actions))
         (menu (and menu-items `("Eglot code actions:" ("dummy" ,@menu-items))))
         (command-and-args
          (and menu
               (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (let ((never-mind (gensym)) retval)
                   (setcdr (cadr menu)
                           (cons `("never mind..." . ,never-mind) (cdadr menu)))
                   (if (eq (setq retval (tmm-prompt menu)) never-mind)
                       (keyboard-quit)
                     retval))))))
    (if command-and-args
        (eglot--request server :workspace/executeCommand command-and-args)
      (eglot--message "No code actions here"))))



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
              (eglot--notify
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

(cl-defmethod eglot-server-ready-p ((server eglot-rls) what)
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
    (list :cacheDirectory (file-name-as-directory cache)
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
