;;; eglot.el --- Client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Version: 0.3
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


;;; User tweakable stuff
(defgroup eglot nil
  "Interaction with Language Server Protocol servers"
  :prefix "eglot-"
  :group 'applications)

(defvar eglot-server-programs '((rust-mode . ("rls"))
                                (python-mode . ("pyls"))
                                (js-mode . ("javascript-typescript-stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                (php-mode . ("php" "vendor/felixfbecker/\
language-server/bin/php-language-server.php")))
  "Alist mapping major modes to server executables.")

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


;;; Process management
(defvar eglot--processes-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defun eglot--current-process ()
  "The current logical EGLOT process."
  (let* ((probe (or (project-current) `(transient . ,default-directory))))
    (cl-find major-mode (gethash probe eglot--processes-by-project)
             :key #'eglot--major-mode)))

(defun eglot--current-process-or-lose ()
  "Return the current EGLOT process or error."
  (or (eglot--current-process) (eglot--error "No current EGLOT process")))

(defmacro eglot--define-process-var
    (var-sym initval &optional doc)
  "Define VAR-SYM as a generalized process-local variable.
INITVAL is the default value.  DOC is the documentation."
  (declare (indent 2))
  `(progn
     (put ',var-sym 'function-documentation ,doc)
     (defun ,var-sym (proc)
       (let* ((plist (process-plist proc))
              (probe (plist-member plist ',var-sym)))
         (if probe
             (cadr probe)
           (let ((def ,initval))
             (process-put proc ',var-sym def)
             def))))
     (gv-define-setter ,var-sym (to-store process)
       `(let ((once ,to-store)) (process-put ,process ',',var-sym once) once))))

(eglot--define-process-var eglot--short-name nil
  "A short name for the process")

(eglot--define-process-var eglot--major-mode nil
  "The major-mode this server is managing.")

(eglot--define-process-var eglot--expected-bytes nil
  "How many bytes declared by server")

(eglot--define-process-var eglot--pending-continuations (make-hash-table)
  "A hash table of request ID to continuation lambdas")

(eglot--define-process-var eglot--events-buffer nil
  "A buffer pretty-printing the EGLOT RPC events")

(eglot--define-process-var eglot--capabilities :unreported
  "Holds list of capabilities that server reported")

(eglot--define-process-var eglot--moribund nil
  "Non-nil if server is about to exit")

(eglot--define-process-var eglot--project nil
  "The project the server belongs to.")

(eglot--define-process-var eglot--spinner `(nil nil t)
  "\"Spinner\" used by some servers.
A list (ID WHAT DONE-P).")

(eglot--define-process-var eglot--status `(:unknown nil)
  "Status as declared by the server.
A list (WHAT SERIOUS-P).")

(eglot--define-process-var eglot--inhibit-autoreconnect eglot-autoreconnect
  "If non-nil, don't autoreconnect on unexpected quit.")

(eglot--define-process-var eglot--contact nil
  "Method used to contact a server.")

(eglot--define-process-var eglot--deferred-actions
    (make-hash-table :test #'equal)
  "Actions deferred to when server is thought to be ready.")

(eglot--define-process-var eglot--file-watches (make-hash-table :test #'equal)
  "File system watches for the didChangeWatchedfiles thingy.")

(eglot--define-process-var eglot--managed-buffers nil
  "Buffers managed by the server.")

(defun eglot--make-process (name managed-major-mode contact)
  "Make a process from CONTACT.
NAME is used to name the the started process or connection.
MANAGED-MAJOR-MODE is a symbol naming a major mode.
CONTACT is in `eglot'.  Returns a process object."
  (let* ((readable-name (format "EGLOT server (%s/%s)" name managed-major-mode))
         (buffer (get-buffer-create (format "*%s stdout*" readable-name)))
         (proc (cond
                ((processp contact) contact)
                ((integerp (cadr contact))
                 (apply #'open-network-stream readable-name buffer contact))
                (t (make-process
                    :name readable-name
                    :command contact
                    :coding 'no-conversion
                    :connection-type 'pipe
                    :stderr (get-buffer-create (format "*%s stderr*" name)))))))
    (set-process-buffer proc buffer)
    (set-marker (process-mark proc) (with-current-buffer buffer (point-min)))
    (set-process-filter proc #'eglot--process-filter)
    (set-process-sentinel proc #'eglot--process-sentinel)
    proc))

(defmacro eglot--obj (&rest what)
  "Make WHAT a suitable argument for `json-encode'."
  (declare (debug (&rest form)))
  ;; FIXME: maybe later actually do something, for now this just fixes
  ;; the indenting of literal plists.
  `(list ,@what))

(defun eglot--project-short-name (project)
  "Give PROJECT a short name."
  (file-name-base (directory-file-name (car (project-roots project)))))

(defun eglot--all-major-modes ()
  "Return all know major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defun eglot--client-capabilities ()
  "What the EGLOT LSP client supports."
  (eglot--obj
   :workspace    (eglot--obj
                  :applyEdit t
                  :workspaceEdit `(:documentChanges :json-false)
                  :didChangeWatchesFiles `(:dynamicRegistration t)
                  :symbol `(:dynamicRegistration :json-false))
   :textDocument (eglot--obj
                  :synchronization (eglot--obj
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
   :experimental (eglot--obj)))

(defvar eglot-connect-hook nil "Hook run after connecting in `eglot--connect'.")

(defun eglot--connect (project managed-major-mode short-name contact _interactive)
  "Connect for PROJECT, MANAGED-MAJOR-MODE, SHORT-NAME and CONTACT.
INTERACTIVE is t if inside interactive call."
  (let* ((proc (eglot--make-process
                short-name managed-major-mode (if (functionp contact)
                                                  (funcall contact) contact)))
         (buffer (process-buffer proc)))
    (setf (eglot--contact proc) contact
          (eglot--project proc) project
          (eglot--major-mode proc) managed-major-mode)
    (with-current-buffer buffer
      (let ((inhibit-read-only t) success)
        (setf (eglot--inhibit-autoreconnect proc)
              (cond
               ((booleanp eglot-autoreconnect) (not eglot-autoreconnect))
               ((cl-plusp eglot-autoreconnect)
                (run-with-timer eglot-autoreconnect nil
                                (lambda ()
                                  (setf (eglot--inhibit-autoreconnect proc)
                                        (null eglot-autoreconnect)))))))
        (setf (eglot--short-name proc) short-name)
        (push proc (gethash project eglot--processes-by-project))
        (run-hook-with-args 'eglot-connect-hook proc)
        (erase-buffer)
        (read-only-mode t)
        (unwind-protect
            (cl-destructuring-bind (&key capabilities)
                (eglot--request
                 proc
                 :initialize
                 (eglot--obj :processId (unless (eq (process-type proc)
                                                    'network)
                                          (emacs-pid))
                             :capabilities(eglot--client-capabilities)
                             :rootPath  (expand-file-name
                                         (car (project-roots project)))
                             :rootUri  (eglot--path-to-uri
                                        (car (project-roots project)))
                             :initializationOptions  []))
              (setf (eglot--capabilities proc) capabilities)
              (setf (eglot--status proc) nil)
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (eglot--maybe-activate-editing-mode proc)))
              (eglot--notify proc :initialized (eglot--obj :__dummy__ t))
              (setq success proc))
          (unless (or success (not (process-live-p proc)) (eglot--moribund proc))
            (eglot-shutdown proc)))))))

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
         (guessed (cdr (assoc managed-mode eglot-server-programs)))
         (program (and (listp guessed) (stringp (car guessed)) (car guessed)))
         (base-prompt "[eglot] Enter program to execute (or <host>:<port>): ")
         (prompt
          (cond (current-prefix-arg base-prompt)
                ((null guessed)
                 (format "[eglot] Sorry, couldn't guess for `%s'\n%s!"
                         managed-mode base-prompt))
                ((and program (not (executable-find program)))
                 (concat (format "[eglot] I guess you want to run `%s'"
                                 (combine-and-quote-strings guessed))
                         (format ", but I can't find `%s' in PATH!" program)
                         "\n" base-prompt))))
         (contact
          (if prompt
              (let ((s (read-shell-command
                        prompt
                        (if program (combine-and-quote-strings guessed))
                        'eglot-command-history)))
                (if (string-match "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                  (string-trim s))
                    (list (match-string 1 s) (string-to-number (match-string 2 s)))
                  (split-string-and-unquote s)))
            guessed)))
    (list managed-mode project contact t)))

;;;###autoload
(defun eglot (managed-major-mode project command &optional interactive)
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

MANAGED-MAJOR-MODE is an Emacs major mode.

INTERACTIVE is t if called interactively."
  (interactive (eglot--interactive))
  (let* ((short-name (eglot--project-short-name project)))
    (let ((current-process (eglot--current-process)))
      (if (and (process-live-p current-process)
               interactive
               (y-or-n-p "[eglot] Live process found, reconnect instead? "))
          (eglot-reconnect current-process interactive)
        (when (process-live-p current-process)
          (eglot-shutdown current-process))
        (let ((proc (eglot--connect project
                                    managed-major-mode
                                    short-name
                                    command
                                    interactive)))
          (eglot--message "Connected! Process `%s' now \
managing `%s' buffers in project `%s'."
                          proc managed-major-mode short-name)
          proc)))))

(defun eglot-reconnect (process &optional interactive)
  "Reconnect to PROCESS.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-process-or-lose) t))
  (when (process-live-p process)
    (eglot-shutdown process interactive))
  (eglot--connect (eglot--project process)
                  (eglot--major-mode process)
                  (eglot--short-name process)
                  (eglot--contact process)
                  interactive)
  (eglot--message "Reconnected!"))

(defun eglot--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (eglot--log-event proc `(:message "Process state changed" :change ,change))
  (when (not (process-live-p proc))
    (with-current-buffer (eglot-events-buffer proc)
      (let ((inhibit-read-only t))
        (insert "\n----------b---y---e---b---y---e----------\n")))
    ;; Cancel outstanding timers and file system watches
    (maphash (lambda (_id triplet)
               (cl-destructuring-bind (_success _error timeout) triplet
                 (cancel-timer timeout)))
             (eglot--pending-continuations proc))
    (maphash (lambda (_id watches)
               (mapcar #'file-notify-rm-watch watches))
             (eglot--file-watches proc))
    (unwind-protect
        ;; Call all outstanding error handlers
        (maphash (lambda (_id triplet)
                   (cl-destructuring-bind (_success error _timeout) triplet
                     (funcall error `(:code -1 :message "Server died"))))
                 (eglot--pending-continuations proc))
      ;; Turn off `eglot--managed-mode' where appropriate.
      (dolist (buffer (eglot--managed-buffers proc))
        (with-current-buffer buffer (eglot--managed-mode-onoff proc -1)))
      ;; Forget about the process-project relationship
      (setf (gethash (eglot--project proc) eglot--processes-by-project)
            (delq proc
                  (gethash (eglot--project proc) eglot--processes-by-project)))
      ;; Say last words
      (eglot--message "%s exited with status %s" proc (process-exit-status proc))
      (delete-process proc)
      ;; Consider autoreconnecting
      (cond ((eglot--moribund proc))
            ((not (eglot--inhibit-autoreconnect proc))
             (eglot--warn "Reconnecting after unexpected server exit")
             (eglot-reconnect proc))
            ((timerp (eglot--inhibit-autoreconnect proc))
             (eglot--warn "Not auto-reconnecting, last on didn't last long."))))))

(defun eglot--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (expected-bytes (eglot--expected-bytes proc)))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        ;; Loop (more than one message might have arrived)
        ;;
        (unwind-protect
            (let (done)
              (while (not done)
                (cond
                 ((not expected-bytes)
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
                                  (eglot--process-receive proc json-message))))
                          (goto-char message-end)
                          (delete-region (point-min) (point))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter
          ;;
          (setf (eglot--expected-bytes proc) expected-bytes))))))

(defun eglot-events-buffer (process &optional interactive)
  "Display events buffer for current LSP connection PROCESS.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-process-or-lose) t))
  (let* ((probe (eglot--events-buffer process))
         (buffer (or (and (buffer-live-p probe)
                          probe)
                     (let ((buffer (get-buffer-create
                                    (format "*%s events*"
                                            (process-name process)))))
                       (with-current-buffer buffer
                         (buffer-disable-undo)
                         (read-only-mode t)
                         (setf (eglot--events-buffer process) buffer))
                       buffer))))
    (when interactive (display-buffer buffer))
    buffer))

(defun eglot--log-event (proc message &optional type)
  "Log an eglot-related event.
PROC is the current process.  MESSAGE is a JSON-like plist.  TYPE
is a symbol saying if this is a client or server originated."
  (with-current-buffer (eglot-events-buffer proc)
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

(defun eglot--process-receive (proc message)
  "Process MESSAGE from PROC."
  (cl-destructuring-bind (&key method id params error result _jsonrpc) message
    (let* ((continuations (and id
                               (not method)
                               (gethash id (eglot--pending-continuations proc)))))
      (eglot--log-event proc message 'server)
      (when error (setf (eglot--status proc) `(,error t)))
      (cond (method
             ;; a server notification or a server request
             (let* ((handler-sym (intern (concat "eglot--server-" method))))
               (if (functionp handler-sym)
                   ;; FIXME: will fail if params is array instead of  not an object
                   (apply handler-sym proc (append params (if id `(:id ,id))))
                 (eglot--warn "No implementation of method %s yet" method)
                 (when id
                   (eglot--reply
                    proc id
                    :error `(:code -32601 :message "Method unimplemented"))))))
            (continuations
             (cancel-timer (cl-third continuations))
             (remhash id (eglot--pending-continuations proc))
             (if error
                 (funcall (cl-second continuations) error)
               (funcall (cl-first continuations) result)))
            (id
             (eglot--warn "Ooops no continuation for id %s" id)))
      (eglot--call-deferred proc)
      (force-mode-line-update t))))

(defun eglot--process-send (proc message)
  "Send MESSAGE to PROC (ID is optional)."
  (let ((json (json-encode message)))
    (process-send-string proc (format "Content-Length: %d\r\n\r\n%s"
                                      (string-bytes json)
                                      json))
    (eglot--log-event proc message 'client)))

(defvar eglot--next-request-id 0 "ID for next request.")

(defun eglot--next-request-id ()
  "Compute the next id for a client request."
  (setq eglot--next-request-id (1+ eglot--next-request-id)))

(defun eglot-forget-pending-continuations (process)
  "Stop waiting for responses from the current LSP PROCESS."
  (interactive (list (eglot--current-process-or-lose)))
  (clrhash (eglot--pending-continuations process)))

(defun eglot-clear-status (process)
  "Clear most recent error message from PROCESS."
  (interactive (list (eglot--current-process-or-lose)))
  (setf (eglot--status process) nil)
  (force-mode-line-update t))

(defun eglot--call-deferred (proc)
  "Call PROC's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (eglot--deferred-actions proc))))
    (eglot--log-event proc `(:running-deferred ,(length actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defvar eglot--ready-predicates '(eglot--server-ready-p)
  "Special hook of predicates controlling deferred actions.
If one of these returns nil, a deferrable `eglot--async-request'
will be deferred.  Each predicate is passed the symbol for the
request request and a process object.")

(defun eglot--server-ready-p (_what _proc)
  "Tell if server of PROC ready for processing deferred WHAT."
  (not (eglot--outstanding-edits-p)))

(cl-defmacro eglot--lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (gensym "eglot--lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(cl-defun eglot--async-request (proc
                                method
                                params
                                &rest args
                                &key success-fn error-fn timeout-fn
                                (timeout eglot-request-timeout)
                                (deferred nil))
  "Make a request to PROCESS, expecting a reply later on.
SUCCESS-FN and ERROR-FN are passed `:result' and `:error'
objects, respectively.  Wait TIMEOUT seconds for response or call
nullary TIMEOUT-FN.  If DEFERRED, maybe defer request to the
future, or to never at all, in case a new request with identical
DEFERRED and for the same buffer overrides it (however, if that
happens, the original timeout keeps counting). Return a list (ID
TIMER)."
  (let* ((id (eglot--next-request-id))
         (timer nil)
         (make-timer
          (lambda ( )
            (or timer
                (run-with-timer
                 timeout nil
                 (lambda ()
                   (remhash id (eglot--pending-continuations proc))
                   (funcall (or timeout-fn
                                (lambda ()
                                  (eglot--log-event
                                   proc `(:timed-out ,method :id ,id
                                                     :params ,params)))))))))))
    (when deferred
      (let* ((buf (current-buffer))
             (existing (gethash (list deferred buf) (eglot--deferred-actions proc))))
        (when existing (setq existing (cadr existing)))
        (if (run-hook-with-args-until-failure 'eglot--ready-predicates
                                              deferred proc)
            (remhash (list deferred buf) (eglot--deferred-actions proc))
          (eglot--log-event proc `(:deferring ,method :id ,id :params ,params))
          (let* ((buf (current-buffer)) (point (point))
                 (later (lambda ()
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (save-excursion (goto-char point)
                                              (apply #'eglot--async-request proc
                                                     method params args)))))))
            (puthash (list deferred buf) (list later (setq timer (funcall make-timer)))
                     (eglot--deferred-actions proc))
            (cl-return-from eglot--async-request nil)))))
    ;; Really run it
    ;;
    (eglot--process-send proc (eglot--obj :jsonrpc "2.0"
                                          :id id
                                          :method method
                                          :params params))
    (puthash id
             (list (or success-fn
                       (eglot--lambda (&rest _ignored)
                         (eglot--log-event
                          proc (eglot--obj :message "success ignored" :id id))))
                   (or error-fn
                       (eglot--lambda (&key code message &allow-other-keys)
                         (setf (eglot--status proc) `(,message t))
                         proc (eglot--obj :message "error ignored, status set"
                                          :id id :error code)))
                   (setq timer (funcall make-timer)))
             (eglot--pending-continuations proc))
    (list id timer)))

(defun eglot--request (proc method params &optional deferred)
  "Like `eglot--async-request' for PROC, METHOD and PARAMS, but synchronous.
Meaning only return locally if successful, otherwise exit non-locally.
DEFERRED is passed to `eglot--async-request', which see."
  ;; Launching a deferred sync request with outstanding changes is a
  ;; bad idea, since that might lead to the request never having a
  ;; chance to run, because `eglot--ready-predicates'.
  (when deferred (eglot--signal-textDocument/didChange))
  (let* ((done (make-symbol "eglot-catch")) id-and-timer
         (res
          (unwind-protect
              (catch done
                (setq
                 id-and-timer
                 (eglot--async-request
                  proc method params
                  :success-fn (lambda (result) (throw done `(done ,result)))
                  :timeout-fn (lambda () (throw done '(error "Timed out")))
                  :error-fn (eglot--lambda (&key code message _data)
                              (throw done `(error
                                            ,(format "Ooops: %s: %s" code message))))
                  :deferred deferred))
                (while t (accept-process-output nil 30)))
            (pcase-let ((`(,id ,timer) id-and-timer))
              (when id (remhash id (eglot--pending-continuations proc)))
              (when timer (cancel-timer timer))))))
    (when (eq 'error (car res)) (eglot--error (cadr res)))
    (cadr res)))

(cl-defun eglot--notify (process method params)
  "Notify PROCESS of something, don't expect a reply.e"
  (eglot--process-send process (eglot--obj :jsonrpc  "2.0"
                                           :method method
                                           :params params)))

(cl-defun eglot--reply (process id &key result error)
  "Reply to PROCESS's request ID with MESSAGE."
  (eglot--process-send
   process `(:jsonrpc  "2.0" :id ,id
                       ,@(when result `(:result ,result))
                       ,@(when error `(:error ,error)))))


;;; Helpers
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
    (display-warning 'eglot
                     (apply #'format format args)
                     :warning)))

(defun eglot--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (save-excursion
    (eglot--obj :line
                ;; F!@(#*&#$)CKING OFF-BY-ONE
                (1- (line-number-at-pos pos t))
                :character
                (- (goto-char (or pos (point)))
                   (line-beginning-position)))))

(defun eglot--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion (goto-char (point-min))
                  (forward-line (plist-get pos-plist :line))
                  (forward-char
                   (min (plist-get pos-plist :character)
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
  (cond ((stringp markup)
         (with-temp-buffer
           (ignore-errors (funcall (intern "markdown-mode"))) ;escape bytecomp
           (font-lock-ensure)
           (insert markup)
           (string-trim (buffer-string))))
        (t
         (with-temp-buffer
           (ignore-errors (funcall (intern (concat
                                            (plist-get markup :language)
                                            "-mode" ))))
           (insert (plist-get markup :value))
           (font-lock-ensure)
           (buffer-string)))))

(defun eglot--server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (cl-loop for caps = (eglot--capabilities (eglot--current-process-or-lose))
           then (cadr probe)
           for feat in feats
           for probe = (plist-member caps feat)
           if (not probe) do (cl-return nil)
           if (eq (cadr probe) t) do (cl-return t)
           if (eq (cadr probe) :json-false) do (cl-return nil)
           finally (cl-return (or probe t))))

(defun eglot--range-region (range &optional markers)
  "Return region (BEG END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (list (eglot--lsp-position-to-point (plist-get range :start) markers)
        (eglot--lsp-position-to-point (plist-get range :end) markers)))


;;; Minor modes
;;;
(defvar eglot-mode-map (make-sparse-keymap))

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
    (remove-function (local imenu-create-index-function) #'eglot-imenu))))

(defun eglot--managed-mode-onoff (proc arg)
  "Proxy for function `eglot--managed-mode' with ARG and PROC."
  (eglot--managed-mode arg)
  (let ((buf (current-buffer)))
    (if eglot--managed-mode
        (cl-pushnew buf (eglot--managed-buffers proc))
      (setf (eglot--managed-buffers proc)
            (delq buf (eglot--managed-buffers proc))))))

(add-hook 'eglot--managed-mode-hook 'flymake-mode)
(add-hook 'eglot--managed-mode-hook 'eldoc-mode)


(defvar-local eglot--current-flymake-report-fn nil
  "Current flymake report function for this buffer")

(defun eglot--maybe-activate-editing-mode (&optional proc)
  "Maybe activate mode function `eglot--managed-mode'.
If PROC is supplied, do it only if BUFFER is managed by it.  In
that case, also signal textDocument/didOpen."
  ;; Called even when revert-buffer-in-progress-p
  (let* ((cur (and buffer-file-name (eglot--current-process)))
         (proc (or (and (null proc) cur)
                   (and proc (eq proc cur) cur))))
    (when proc
      (eglot--managed-mode-onoff proc 1)
      (eglot--signal-textDocument/didOpen)
      (flymake-start)
      (funcall (or eglot--current-flymake-report-fn #'ignore) nil))))

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
      (call-interactively what))))

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
  (pcase-let* ((proc (eglot--current-process))
               (name (and (process-live-p proc) (eglot--short-name proc)))
               (pending (and proc (hash-table-count
                                   (eglot--pending-continuations proc))))
               (`(,_id ,doing ,done-p ,detail) (and proc (eglot--spinner proc)))
               (`(,status ,serious-p) (and proc (eglot--status proc))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line nil))
     (when name
       `(":" ,(eglot--mode-line-props
               name 'eglot-mode-line
               '((mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when serious-p
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-1 eglot-events-buffer "go to events buffer")
                       (mouse-3 eglot-clear-status  "clear this status"))
                     (format "An error occured: %s\n" status))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props
                     (format "%s%s" doing
                             (if detail (format ":%s" detail) ""))
                     'compilation-mode-line-run
                     '((mouse-1 eglot-events-buffer "go to events buffer")))))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-1 eglot-events-buffer "go to events buffer")
                       (mouse-3 eglot-clear-status  "clear this status"))
                     (format "%d pending requests\n" pending)))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(defun eglot-shutdown (proc &optional _interactive)
  "Politely ask the server PROC to quit.
Forcefully quit it if it doesn't respond.  Don't leave this
function with the server still running."
  (interactive (list (eglot--current-process-or-lose) t))
  (eglot--message "Asking %s politely to terminate" proc)
  (unwind-protect
      (let ((eglot-request-timeout 3))
        (setf (eglot--moribund proc) t)
        (eglot--request proc :shutdown nil)
        ;; this one is supposed to always fail, hence ignore-errors
        (ignore-errors (eglot--request proc :exit nil)))
    ;; Turn off `eglot--managed-mode' where appropriate.
    (dolist (buffer (eglot--managed-buffers proc))
      (with-current-buffer buffer (eglot--managed-mode-onoff proc -1)))
    (when (process-live-p proc)
      (eglot--warn "Brutally deleting non-compliant existing process %s" proc)
      (delete-process proc))))

(cl-defun eglot--server-window/showMessage (_process &key type message)
  "Handle notification window/showMessage"
  (eglot--message (propertize "Server reports (type=%s): %s"
                              'face (if (<= type 1) 'error))
                  type message))

(cl-defun eglot--server-window/showMessageRequest
    (process &key id type message actions)
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
          (eglot--reply process id :result (eglot--obj :title reply))
        (eglot--reply process id
                      :error (eglot--obj :code -32800
                                         :message "User cancelled"))))))

(cl-defun eglot--server-window/logMessage (_proc &key _type _message)
  "Handle notification window/logMessage") ;; noop, use events buffer

(cl-defun eglot--server-telemetry/event (_proc &rest _any)
  "Handle notification telemetry/event") ;; noop, use events buffer

(defvar-local eglot--unreported-diagnostics nil
  "Unreported diagnostics for this buffer.")

(cl-defun eglot--server-textDocument/publishDiagnostics
    (_process &key uri diagnostics)
  "Handle notification publishDiagnostics"
  (if-let ((buffer (find-buffer-visiting (eglot--uri-to-path uri))))
      (with-current-buffer buffer
        (cl-loop
         for diag-spec across diagnostics
         collect (cl-destructuring-bind (&key range severity _group
                                              _code source message)
                     diag-spec
                   (pcase-let ((`(,beg ,end) (eglot--range-region range)))
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
                        (setq eglot--unreported-diagnostics diags)))))
    (eglot--warn "Diagnostics received for unvisited %s" uri)))

(cl-defun eglot--register-unregister (proc jsonrpc-id things how)
  "Helper for `eglot--server-client/registerCapability'.
THINGS are either registrations or unregisterations."
  (dolist (thing (cl-coerce things 'list))
    (cl-destructuring-bind (&key id method registerOptions) thing
      (let (retval)
        (unwind-protect
            (setq retval (apply (intern (format "eglot--%s-%s" how method))
                                proc :id id registerOptions))
          (unless (eq t (car retval))
            (cl-return-from eglot--register-unregister
              (eglot--reply
               proc jsonrpc-id
               :error `(:code -32601 :message ,(or (cadr retval) "sorry")))))))))
  (eglot--reply proc jsonrpc-id :result (eglot--obj :message "OK")))

(cl-defun eglot--server-client/registerCapability
    (proc &key id registrations)
  "Handle server request client/registerCapability"
  (eglot--register-unregister proc id registrations 'register))

(cl-defun eglot--server-client/unregisterCapability
    (proc &key id unregisterations) ;; XXX: Yeah, typo and all.. See spec...
  "Handle server request client/unregisterCapability"
  (eglot--register-unregister proc id unregisterations 'unregister))

(cl-defun eglot--server-workspace/applyEdit
    (proc &key id _label edit)
  "Handle server request workspace/applyEdit"
  (condition-case err
      (progn
        (eglot--apply-workspace-edit edit 'confirm)
        (eglot--reply proc id :result `(:applied )))
    (error
     (eglot--reply proc id
                   :result `(:applied :json-false)
                   :error
                   (eglot--obj :code -32001
                               :message (format "%s" err))))))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  (eglot--obj :uri (eglot--path-to-uri buffer-file-name)))

(defvar-local eglot--versioned-identifier 0)

(defun eglot--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (eglot--TextDocumentIdentifier)
          (eglot--obj :version eglot--versioned-identifier)))

(defun eglot--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (eglot--VersionedTextDocumentIdentifier)
   (eglot--obj :languageId
               (if (string-match "\\(.*\\)-mode" (symbol-name major-mode))
                   (match-string 1 (symbol-name major-mode))
                 "unknown")
               :text
               (save-restriction
                 (widen)
                 (buffer-substring-no-properties (point-min) (point-max))))))

(defun eglot--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (eglot--obj :textDocument (eglot--TextDocumentIdentifier)
              :position (eglot--pos-to-lsp-position)))

(defvar-local eglot--recent-changes nil
  "Recent buffer changes as collected by `eglot--before-change'.")

(defun eglot--outstanding-edits-p ()
  "Non-nil if there are outstanding edits."
  (cl-plusp (+ (length (car eglot--recent-changes))
               (length (cdr eglot--recent-changes)))))

(defun eglot--before-change (start end)
  "Hook onto `before-change-functions'.
Records START and END, crucially convert them into
LSP (line/char) positions before that information is
lost (because the after-change thingy doesn't know if newlines
were deleted/added)"
  (setf (car eglot--recent-changes)
        (vconcat (car eglot--recent-changes)
                 `[(,(eglot--pos-to-lsp-position start)
                    ,(eglot--pos-to-lsp-position end))])))

(defun eglot--after-change (start end pre-change-length)
  "Hook onto `after-change-functions'.
Records START, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (setf (cdr eglot--recent-changes)
        (vconcat (cdr eglot--recent-changes)
                 `[(,pre-change-length
                    ,(buffer-substring-no-properties start end))])))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when (eglot--outstanding-edits-p)
    (let* ((proc (eglot--current-process-or-lose))
           (sync-kind (eglot--server-capable :textDocumentSync))
           (emacs-messup (/= (length (car eglot--recent-changes))
                             (length (cdr eglot--recent-changes))))
           (full-sync-p (or (eq sync-kind 1) emacs-messup)))
      (when emacs-messup
        (eglot--warn "`eglot--recent-changes' messup: %s" eglot--recent-changes))
      (save-restriction
        (widen)
        (eglot--notify
         proc :textDocument/didChange
         (eglot--obj
          :textDocument
          (eglot--VersionedTextDocumentIdentifier)
          :contentChanges
          (if full-sync-p (vector
                           (eglot--obj
                            :text (buffer-substring-no-properties (point-min)
                                                                  (point-max))))
            (cl-loop for (start-pos end-pos) across (car eglot--recent-changes)
                     for (len after-text) across (cdr eglot--recent-changes)
                     vconcat `[,(eglot--obj :range (eglot--obj :start start-pos
                                                               :end end-pos)
                                            :rangeLength len
                                            :text after-text)])))))
      (setq eglot--recent-changes (cons [] []))
      (setf (eglot--spinner proc) (list nil :textDocument/didChange t))
      (eglot--call-deferred proc))))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq eglot--recent-changes (cons [] []))
  (eglot--notify
   (eglot--current-process-or-lose)
   :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (eglot--notify
   (eglot--current-process-or-lose)
   :textDocument/didClose `(:textDocument ,(eglot--TextDocumentIdentifier))))

(defun eglot--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (let ((proc (eglot--current-process-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (eglot--notify proc :textDocument/willSave params)
    (ignore-errors
      (let ((eglot-request-timeout 0.5))
        (when (plist-get :willSaveWaitUntil
                         (eglot--server-capable :textDocumentSync))
          (eglot--apply-text-edits
           (eglot--request proc :textDocument/willSaveWaituntil params)))))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (eglot--notify
   (eglot--current-process-or-lose)
   :textDocument/didSave
   (eglot--obj
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (eglot--TextDocumentIdentifier))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "An EGLOT Flymake backend.
Calls REPORT-FN maybe if server publishes diagnostics in time."
  (setq eglot--current-flymake-report-fn report-fn)
  ;; Report anything unreported
  (when eglot--unreported-diagnostics
    (funcall report-fn eglot--unreported-diagnostics)
    (setq eglot--unreported-diagnostics nil))
  ;; Signal a didChange that might eventually bring new diagnotics
  (eglot--signal-textDocument/didChange))

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
    (let ((proc (eglot--current-process-or-lose))
          (text-id (eglot--TextDocumentIdentifier)))
      (completion-table-with-cache
       (lambda (string)
         (setq eglot--xref-known-symbols
               (mapcar
                (eglot--lambda (&key name kind location containerName)
                  (propertize name
                              :textDocumentPositionParams
                              (eglot--obj :textDocument text-id
                                          :position (plist-get
                                                     (plist-get location :range)
                                                     :start))
                              :locations (list location)
                              :kind kind
                              :containerName containerName))
                (eglot--request proc
                                :textDocument/documentSymbol
                                (eglot--obj
                                 :textDocument text-id))))
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
            (eglot--request (eglot--current-process-or-lose)
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
            (eglot--request (eglot--current-process-or-lose)
                            :textDocument/references
                            (append
                             params
                             (eglot--obj :context
                                         (eglot--obj :includeDeclaration t)))))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (mapcar (eglot--lambda (&key name location &allow-other-keys)
              (cl-destructuring-bind (&key uri range) location
                (eglot--xref-make name uri (plist-get range :start))))
            (eglot--request (eglot--current-process-or-lose)
                            :workspace/symbol
                            (eglot--obj :query pattern)))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (proc (eglot--current-process-or-lose)))
    (when (eglot--server-capable :completionProvider)
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (completion-table-with-cache
        (lambda (_ignored)
          (let* ((resp (eglot--request proc
                                       :textDocument/completion
                                       (eglot--TextDocumentPositionParams)
                                       :textDocument/completion))
                 (items (if (vectorp resp) resp (plist-get resp :items))))
            (mapcar
             (eglot--lambda (&rest all &key label insertText &allow-other-keys)
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
                         (plist-get (eglot--request proc :completionItem/resolve
                                                    (text-properties-at 0 obj))
                                    :documentation)))))
           (when documentation
             (with-current-buffer (get-buffer-create " *eglot doc*")
               (erase-buffer)
               (ignore-errors (funcall (intern "markdown-mode")))
               (font-lock-ensure)
               (insert documentation)
               (current-buffer)))))
       :exit-function (lambda (_string _status)
                        (eglot--signal-textDocument/didChange)
                        (eglot-eldoc-function))))))

(defvar eglot--highlights nil "Overlays for textDocument/documentHighlight.")

(defun eglot--hover-info (contents &optional range)
  (concat (and range
               (pcase-let ((`(,beg ,end) (eglot--range-region range)))
                 (concat (buffer-substring beg end)  ": ")))
          (mapconcat #'eglot--format-markup
                     (append
                      (cond ((vectorp contents)
                             contents)
                            (contents
                             (list contents)))) "\n")))

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
      (eglot--request (eglot--current-process-or-lose) :textDocument/hover
                      (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (with-help-window "*eglot help*"
      (with-current-buffer standard-output
        (insert (eglot--hover-info contents range))))))

(defun eglot-eldoc-function ()
  "EGLOT's `eldoc-documentation-function' function.
If SKIP-SIGNATURE, don't try to send textDocument/signatureHelp."
  (let* ((buffer (current-buffer))
         (proc (eglot--current-process-or-lose))
         (position-params (eglot--TextDocumentPositionParams))
         sig-showing)
    (cl-macrolet ((when-buffer-window
                   (&body body) `(when (get-buffer-window buffer)
                                   (with-current-buffer buffer ,@body))))
      (when (eglot--server-capable :signatureHelpProvider)
        (eglot--async-request
         proc :textDocument/signatureHelp position-params
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
         proc :textDocument/hover position-params
         :success-fn (eglot--lambda (&key contents range)
                       (unless sig-showing
                         (when-buffer-window
                          (eldoc-message (eglot--hover-info contents range)))))
         :deferred :textDocument/hover))
      (when (eglot--server-capable :documentHighlightProvider)
        (eglot--async-request
         proc :textDocument/documentHighlight position-params
         :success-fn (lambda (highlights)
                       (mapc #'delete-overlay eglot--highlights)
                       (setq eglot--highlights
                             (when-buffer-window
                              (mapcar (eglot--lambda (&key range _kind)
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
              (eglot--lambda (&key name kind location _containerName)
                (cons (propertize name :kind (cdr (assoc kind eglot--kind-names)))
                      (eglot--lsp-position-to-point
                       (plist-get (plist-get location :range) :start))))
              (eglot--request (eglot--current-process-or-lose)
                              :textDocument/documentSymbol
                              (eglot--obj
                               :textDocument (eglot--TextDocumentIdentifier))))))
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
  (save-restriction
    (widen)
    (save-excursion
      (mapc (eglot--lambda (newText beg end)
              (goto-char beg) (delete-region beg end) (insert newText))
            (mapcar (eglot--lambda (&key range newText)
                      (cons newText (eglot--range-region range 'markers)))
                    edits))))
  (eglot--message "%s: Performed %s edits" (current-buffer) (length edits)))

(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (let (prepared)
    (cl-destructuring-bind (&key changes documentChanges)
        wedit
      (cl-loop
       for change on documentChanges
       do (push (cl-destructuring-bind (&key textDocument edits) change
                  (cl-destructuring-bind (&key uri version) textDocument
                    (list (eglot--uri-to-path uri) edits version)))
                prepared))
      (cl-loop for (uri edits) on changes by #'cddr
               do (push (list (eglot--uri-to-path uri) edits) prepared)))
    (if (or confirm
            (cl-notevery #'find-buffer-visiting
                         (mapcar #'car prepared)))
        (unless (y-or-n-p
                 (format "[eglot] Server requests to edit %s files.\n  %s\n\
Proceed? "
                         (length prepared)
                         (mapconcat #'identity
                                    (mapcar #'car prepared)
                                    "\n  ")))
          (eglot--error "User cancelled server edit")))
    (unwind-protect
        (let (edit)
          (while (setq edit (car prepared))
            (cl-destructuring-bind (path edits &optional version) edit
              (with-current-buffer (find-file-noselect path)
                (eglot--apply-text-edits edits version))
              (pop prepared))))
      (if prepared
          (eglot--warn "Caution: edits of files %s failed."
                       (mapcar #'car prepared))
        (eglot--message "Edit successful!")))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (eglot--request (eglot--current-process-or-lose)
                   :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                          ,@(eglot--obj :newName newname)))
   current-prefix-arg))


;;; Dynamic registration
;;;
(cl-defun eglot--register-workspace/didChangeWatchedFiles (proc &key id watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles"
  (eglot--unregister-workspace/didChangeWatchedFiles proc :id id)
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
               proc :workspace/didChangeWatchedFiles
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
                         (gethash id (eglot--file-watches proc))))
                 (setq success `(t "OK")))
        (unless success
          (eglot--unregister-workspace/didChangeWatchedFiles proc :id id))))))

(cl-defun eglot--unregister-workspace/didChangeWatchedFiles (proc &key id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles"
  (mapc #'file-notify-rm-watch (gethash id (eglot--file-watches proc)))
  (remhash id (eglot--file-watches proc))
  (list t "OK"))


;;; Rust-specific
;;;
(defun eglot--rls-probably-ready-for-p (what proc)
  "Guess if the RLS running in PROC is ready for WHAT."
  (or (eq what :textDocument/completion) ; RLS normally ready for this
                                        ; one, even if building ;
      (pcase-let ((`(,_id ,what ,done ,_detail) (eglot--spinner proc)))
        (and (equal "Indexing" what) done))))

;;;###autoload
(progn
  (add-hook 'rust-mode-hook 'eglot--setup-rls-idiosyncrasies)
  (defun eglot--setup-rls-idiosyncrasies ()
    "Prepare `eglot' to deal with RLS's special treatment."
    (add-hook 'eglot--ready-predicates 'eglot--rls-probably-ready-for-p t t)))

(cl-defun eglot--server-window/progress
    (process &key id done title message &allow-other-keys)
  "Handle notification window/progress"
  (setf (eglot--spinner process) (list id title done message))
  (when (and (equal "Indexing" title) done)
    (dolist (buffer (eglot--managed-buffers process))
      (with-current-buffer buffer
        (funcall (or eglot--current-flymake-report-fn #'ignore)
                 eglot--unreported-diagnostics)))))

(provide 'eglot)
;;; eglot.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
