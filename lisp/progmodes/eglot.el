;;; eglot.el --- A client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  João Távora

;; Author: João Távora
;; Keywords: extensions

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


;;; User tweakable stuff
(defgroup eglot nil
  "Interaction with Language Server Protocol servers"
  :prefix "eglot-"
  :group 'applications)

(defvar eglot-executables '((rust-mode . ("rls"))
                            (python-mode . ("pyls"))
                            (js-mode . ("javascript-typescript-stdio")))
  "Alist mapping major modes to server executables.")

(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in EGLOT's mode line.")

(defcustom eglot-request-timeout 10
  "How many seconds to way for a reply from the server."
  :type :integer)


;;; Process management
(defvar eglot--processes-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defun eglot--current-process ()
  "The current logical EGLOT process."
  (let* ((cur (project-current))
         (processes (and cur (gethash cur eglot--processes-by-project))))
    (cl-find major-mode processes :key #'eglot--major-mode)))

(defun eglot--current-process-or-lose ()
  "Return the current EGLOT process or error."
  (or (eglot--current-process)
      (eglot--error "No current EGLOT process%s"
                    (if (project-current) "" " (Also no current project)"))))

(defmacro eglot--define-process-var
    (var-sym initval &optional doc mode-line-update-p)
  "Define VAR-SYM as a generalized process-local variable.
INITVAL is the default value.  DOC is the documentation.
MODE-LINE-UPDATE-P says to also force a mode line update
after setting it."
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
     (gv-define-setter ,var-sym (to-store &optional process)
       (let* ((prop ',var-sym))
         ,(let ((form '(let ((proc (or ,process (eglot--current-process-or-lose))))
                         (process-put proc ',prop ,to-store))))
            (if mode-line-update-p
                `(backquote (prog1 ,form (force-mode-line-update t)))
              `(backquote ,form)))))))

(eglot--define-process-var eglot--short-name nil
  "A short name for the process" t)

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
A list (ID WHAT DONE-P)." t)

(eglot--define-process-var eglot--status `(:unknown nil)
  "Status as declared by the server.
A list (WHAT SERIOUS-P)." t)

(eglot--define-process-var eglot--contact nil
  "Method used to contact a server.
Either a list of strings (a shell command and arguments), or a
list of a single string of the form <host>:<port>")

(defun eglot--make-process (name managed-major-mode contact)
  "Make a process from CONTACT.
NAME is a name to give the inferior process or connection.
MANAGED-MAJOR-MODE is a symbol naming a major mode.
CONTACT is as `eglot--contact'.  Returns a process object."
  (let* ((readable-name (format "EGLOT server (%s/%s)" name managed-major-mode))
         (buffer (get-buffer-create
                  (format "*%s inferior*" readable-name)))
         singleton
         (proc
          (if (and (setq singleton (and (null (cdr contact)) (car contact)))
                   (string-match "^[\s\t]*\\(.*\\):\\([[:digit:]]+\\)[\s\t]*$"
                                 singleton))
              (open-network-stream readable-name
                                   buffer
                                   (match-string 1 singleton)
                                   (string-to-number
                                    (match-string 2 singleton)))
            (make-process :name readable-name
                          :buffer buffer
                          :command contact
                          :connection-type 'pipe
                          :stderr (get-buffer-create (format "*%s stderr*"
                                                             name))))))
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
                  :symbol `(:dynamicRegistration :json-false))
   :textDocument (eglot--obj
                  :synchronization (eglot--obj
                                    :dynamicRegistration :json-false
                                    :willSave t
                                    :willSaveWaitUntil :json-false
                                    :didSave t)
                  :completion `(:dynamicRegistration :json-false)
                  :hover      `(:dynamicRegistration :json-false)
                  :references `(:dynamicRegistration :json-false)
                  :definition `(:dynamicRegistration :json-false)
                  :publishDiagnostics `(:relatedInformation :json-false))
   :experimental (eglot--obj)))

(defun eglot--connect (project managed-major-mode
                               short-name contact &optional success-fn)
  "Connect for PROJECT, MANAGED-MAJOR-MODE, SHORT-NAME and CONTACT.
SUCCESS-FN with no args if all goes well."
  (let* ((proc (eglot--make-process short-name managed-major-mode contact))
         (buffer (process-buffer proc)))
    (setf (eglot--contact proc) contact
          (eglot--project proc) project
          (eglot--major-mode proc) managed-major-mode)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setf (eglot--short-name proc) short-name)
        (push proc
              (gethash (project-current)
                       eglot--processes-by-project))
        (erase-buffer)
        (read-only-mode t)
        (with-current-buffer (eglot-events-buffer proc)
          (let ((inhibit-read-only t))
            (insert
             (format "\n-----------------------------------\n"))))
        (eglot--request
         proc
         :initialize
         (eglot--obj :processId (unless (eq (process-type proc)
                                            'network)
                                  (emacs-pid))
                     :rootUri  (eglot--path-to-uri
                                (car (project-roots (project-current))))
                     :initializationOptions  []
                     :capabilities (eglot--client-capabilities))
         :success-fn
         (cl-function
          (lambda (&key capabilities)
            (setf (eglot--capabilities proc) capabilities)
            (setf (eglot--status proc) nil)
            (dolist (buffer (buffer-list))
              (with-current-buffer buffer
                (eglot--maybe-activate-editing-mode proc)))
            (when success-fn (funcall success-fn proc))
            (eglot--notify proc :initialized (eglot--obj :__dummy__ t)))))))))

(defvar eglot--command-history nil
  "History of COMMAND arguments to `eglot'.")

(defun eglot--interactive ()
  "Helper for `eglot'."
  (let* ((managed-major-mode
          (cond
           ((or current-prefix-arg
                (not buffer-file-name))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              (symbol-name major-mode) nil
              (symbol-name major-mode) nil)))
           (t major-mode)))
         (guessed-command
          (cdr (assoc managed-major-mode eglot-executables))))
    (list
     managed-major-mode
     (let ((prompt
            (cond (current-prefix-arg
                   "[eglot] Enter program to execute (or <host>:<port>): ")
                  ((null guessed-command)
                   (format "[eglot] Sorry, couldn't guess for `%s'!\n\
Enter program to execute (or <host>:<port>): "
                           managed-major-mode)))))
       (if prompt
           (split-string-and-unquote
            (read-shell-command prompt
                                (if (listp guessed-command)
                                    (combine-and-quote-strings guessed-command))
                                'eglot-command-history))
         guessed-command))
     t)))

(defun eglot (managed-major-mode command &optional interactive)
  "Start a Language Server Protocol server.
Server is started with COMMAND and manages buffers of
MANAGED-MAJOR-MODE for the current project.

COMMAND is a list of strings, an executable program and
optionally its arguments.  If the first and only string in the
list is of the form \"<host>:<port>\" it is taken as an
indication to connect to a server instead of starting one.  This
is also know as the server's \"contact\".

MANAGED-MAJOR-MODE is an Emacs major mode.

With a prefix arg, prompt for MANAGED-MAJOR-MODE and COMMAND,
else guess them from current context and `eglot-executables'.

INTERACTIVE is t if called interactively."
  (interactive (eglot--interactive))
  (let* ((project (project-current))
         (short-name (eglot--project-short-name project)))
    (unless project (eglot--error "Cannot work without a current project!"))
    (unless command (eglot--error "Don't know how to start EGLOT for %s buffers"
                                  major-mode))
    (let ((current-process (eglot--current-process)))
      (if (and (process-live-p current-process)
               interactive
               (y-or-n-p "[eglot] Live process found, reconnect instead? "))
          (eglot-reconnect current-process interactive)
        (when (process-live-p current-process)
          (eglot-shutdown current-process 'sync))
        (eglot--connect project
                        managed-major-mode
                        short-name
                        command
                        (lambda (proc)
                          (eglot--message "Connected! Process `%s' now \
managing `%s' buffers in project `%s'."
                                          proc
                                          managed-major-mode
                                          short-name)))))))

(defun eglot-reconnect (process &optional interactive)
  "Reconnect to PROCESS.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-process-or-lose) t))
  (when (process-live-p process)
    (eglot-shutdown process 'sync interactive))
  (eglot--connect (eglot--project process)
                  (eglot--major-mode process)
                  (eglot--short-name process)
                  (eglot--contact process)
                  (lambda (_proc) (eglot--message "Reconnected!"))))

(defvar eglot--inhibit-auto-reconnect nil
  "If non-nil, don't autoreconnect on unexpected quit.")

(defun eglot--process-sentinel (process change)
  "Called with PROCESS undergoes CHANGE."
  (eglot--debug "(sentinel) Process state changed to %s" change)
  (when (not (process-live-p process))
    ;; Remember to cancel all timers
    ;;
    (maphash (lambda (id triplet)
               (cl-destructuring-bind (_success _error timeout) triplet
                 (eglot--message
                  "(sentinel) Cancelling timer for continuation %s" id)
                 (cancel-timer timeout)))
             (eglot--pending-continuations process))
    ;; Turn off `eglot--managed-mode' where appropriate.
    ;;
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eglot--buffer-managed-p process)
          (eglot--managed-mode -1))))
    ;; Forget about the process-project relationship
    ;;
    (setf (gethash (eglot--project process) eglot--processes-by-project)
          (delq process
                (gethash (eglot--project process) eglot--processes-by-project)))
    (cond ((eglot--moribund process)
           (eglot--message "(sentinel) Moribund process exited with status %s"
                           (process-exit-status process)))
          ((null eglot--inhibit-auto-reconnect)
           (eglot--warn
            "(sentinel) Reconnecting after process unexpectedly changed to `%s'."
            change)
           (condition-case-unless-debug err
               (eglot-reconnect process)
             (error (eglot--warn "Auto-reconnect failed: %s " err) ))
           (setq eglot--inhibit-auto-reconnect
                 (run-with-timer
                  3 nil
                  (lambda ()
                    (setq eglot--inhibit-auto-reconnect nil)))))
          (t
           (eglot--warn
            "(sentinel) Not auto-reconnecting, last one didn't last long."
            change)))
    (force-mode-line-update t)
    (delete-process process)))

(defun eglot--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (expected-bytes (eglot--expected-bytes proc))
            (done (make-symbol "eglot--process-filter-done-tag")))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        ;; Loop (more than one message might have arrived)
        ;;
        (unwind-protect
            (catch done
              (while t
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
                         (throw done :waiting-for-new-message)))
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
                           (throw done :waiting-for-more-bytes-in-this-message))))))))
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

(defun eglot--log-event (proc message type)
  "Log an eglot-related event.
PROC is the current process.  MESSAGE is a JSON-like plist.  TYPE
is a symbol saying if this is a client or server originated."
  (with-current-buffer (eglot-events-buffer proc)
    (let* ((inhibit-read-only t)
           (id (plist-get message :id))
           (error (plist-get message :error))
           (method (plist-get message :method))
           (subtype (cond ((and method id)       'request)
                          (method                'notification)
                          (id                    'reply)
                          ;; pyls keeps on sending these
                          (t                     'unexpected-thingy)))
           (type
            (format "%s-%s" type subtype)))
      (goto-char (point-max))
      (let ((msg (format "%s%s%s:\n%s\n"
                         type
                         (if id (format " (id:%s)" id) "")
                         (if error " ERROR" "")
                         (pp-to-string message))))
        (when error
          (setq msg (propertize msg 'face 'error)))
        (insert msg)))))

(defun eglot--process-receive (proc message)
  "Process MESSAGE from PROC."
  (let* ((id (plist-get message :id))
         (method (plist-get message :method))
         (err (plist-get message :error))
         (continuations (and id
                             (not method)
                             (gethash id (eglot--pending-continuations proc)))))
    (eglot--log-event proc message 'server)
    (when err (setf (eglot--status proc) '("error" t)))
    (cond (method
           ;; a server notification or a server request
           (let* ((handler-sym (intern (concat "eglot--server-"
                                               method))))
             (if (functionp handler-sym)
                 (apply handler-sym proc (append
                                          (plist-get message :params)
                                          (if id `(:id ,id))))
               (eglot--warn "No implementation of method %s yet"
                            method)
               (when id
                 (eglot--reply
                  proc id
                  :error (eglot--obj :code -32601
                                     :message "Method unimplemented"))))))
          (continuations
           (cancel-timer (cl-third continuations))
           (remhash id (eglot--pending-continuations proc))
           (if err
               (apply (cl-second continuations) err)
             (let ((res (plist-get message :result)))
               (if (listp res)
                   (apply (cl-first continuations) res)
                 (funcall (cl-first continuations) res)))))
          (id
           (eglot--warn "Ooops no continuation for id %s" id)))))

(defvar eglot--expect-carriage-return nil)

(defun eglot--process-send (proc message)
  "Send MESSAGE to PROC (ID is optional)."
  (let ((json (json-encode message)))
    (process-send-string proc (format "Content-Length: %d\r\n\r\n%s"
                                      (string-bytes json)
                                      json))
    (eglot--log-event proc message 'client)))

(defvar eglot--next-request-id 0)

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
  (setf (eglot--status process) nil))

(cl-defun eglot--request (process
                          method
                          params
                          &key success-fn error-fn timeout-fn (async-p t))
  "Make a request to PROCESS, expecting a reply.
Return the ID of this request, unless ASYNC-P is nil, in which
case never returns locally."
  (let* ((id (eglot--next-request-id))
         (timeout-fn (or timeout-fn
                         (lambda ()
                           (eglot--warn
                            "(request) Tired of waiting for reply to %s" id))))
         (error-fn (or error-fn
                       (cl-function
                        (lambda (&key code message &allow-other-keys)
                          (setf (eglot--status process) '("error" t))
                          (eglot--warn
                           "(request) Request id=%s errored with code=%s: %s"
                           id code message)))))
         (success-fn (or success-fn
                         (cl-function
                          (lambda (&rest result-body)
                            (eglot--debug
                             "(request) Request id=%s replied to with result=%s"
                             id result-body)))))
         (catch-tag (cl-gensym (format "eglot--tag-%d-" id))))
    (eglot--process-send process
                         (eglot--obj :jsonrpc "2.0"
                                     :id id
                                     :method method
                                     :params params))
    (catch catch-tag
      (let ((timeout-timer
             (run-with-timer
              eglot-request-timeout nil
              (if async-p
                  (lambda ()
                    (remhash id (eglot--pending-continuations process))
                    (funcall timeout-fn))
                (lambda ()
                  (remhash id (eglot--pending-continuations process))
                  (throw catch-tag (funcall timeout-fn)))))))
        (puthash id
                 (list (if async-p
                           success-fn
                         (lambda (&rest args)
                           (throw catch-tag (apply success-fn args))))
                       (if async-p
                           error-fn
                         (lambda (&rest args)
                           (throw catch-tag (apply error-fn args))))
                       timeout-timer)
                 (eglot--pending-continuations process))
        (unless async-p
          (unwind-protect
              (while t
                (unless (process-live-p process)
                  (cond ((eglot--moribund process)
                         (throw catch-tag (delete-process process)))
                        (t
                         (eglot--error
                          "(request) Proc %s died unexpectedly during request with code %s"
                          process
                          (process-exit-status process)))))
                (accept-process-output nil 0.01))
            (when (memq timeout-timer timer-list)
              (eglot--message
               "(request) Last-change cancelling timer for continuation %s" id)
              (cancel-timer timeout-timer))))))
    ;; Finally, return the id.
    id))

(cl-defmacro eglot--lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  `(cl-function (lambda ,cl-lambda-list ,@body)))

(defun eglot--sync-request (proc method params)
  "Like `eglot--request' for PROC, METHOD and PARAMS, but synchronous.
Meaning only return locally if successful, otherwise exit non-locally."
  (let* ((timeout-error-sym (cl-gensym))
         (catch-tag (make-symbol "eglot--sync-request-catch-tag"))
         (retval
          (catch catch-tag
            (eglot--request proc method params
                            :success-fn (lambda (&rest args)
                                          (throw catch-tag (if (vectorp (car args))
                                                               (car args)
                                                             args)))
                            :error-fn (eglot--lambda
                                          (&key code message &allow-other-keys)
                                        (eglot--error "Oops: %s: %s" code message))
                            :timeout-fn (lambda ()
                                          (throw catch-tag timeout-error-sym))
                            :async-p nil))))
    ;; FIXME: There's maybe an emacs bug here. Because timeout-fn runs
    ;; in a timer, the better and obvious choice of throwing the erro
    ;; in the lambda is not quitting the `accept-process-output'
    ;; infinite loop up there. So use this contorted strategy with
    ;; `cl-gensym'.
    (if (eq retval timeout-error-sym)
        (eglot--error "Tired of waiting for reply to sync request")
      retval)))

(cl-defun eglot--notify (process method params)
  "Notify PROCESS of something, don't expect a reply.e"
  (eglot--process-send process (eglot--obj :jsonrpc  "2.0"
                                           :method method
                                           :params params)))

(cl-defun eglot--reply (process id &key result error)
  "Reply to PROCESS's request ID with MESSAGE."
  (eglot--process-send process (eglot--obj :jsonrpc  "2.0"
                                           :id id
                                           :result result
                                           :error error)))


;;; Helpers
;;;
(defun eglot--debug (format &rest args)
  "Debug message FORMAT with ARGS."
  (display-warning 'eglot
                   (apply #'format format args)
                   :debug))

(defun eglot--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error (apply #'format format args)))

(defun eglot--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message (concat "[eglot] " (apply #'format format args))))

(defun eglot--log (format &rest args)
  "Log out with FORMAT with ARGS."
  (message (concat "[eglot-log] " (apply #'format format args))))

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

(defun eglot--mapply (fun seq)
  "Apply FUN to every element of SEQ."
  (mapcar (lambda (e) (apply fun e)) seq))

(defun eglot--path-to-uri (path)
  "Urify PATH."
  (url-hexify-string (concat "file://" (file-truename path))
                     url-path-allowed-chars))

(defun eglot--uri-to-path (uri)
  "Convert URI to a file path."
  (url-filename (url-generic-parse-url (url-unhex-string uri))))

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
           (ignore-errors (funcall (intern "markdown-mode"))) ;escape bytecompiler
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

(defun eglot--server-capable (feat)
  "Determine if current server is capable of FEAT."
  (plist-get (eglot--capabilities (eglot--current-process-or-lose)) feat))


;;; Minor modes
;;;
(defvar eglot-mode-map (make-sparse-keymap))

(define-minor-mode eglot--managed-mode
  "Mode for source buffers managed by some EGLOT project."
  nil nil eglot-mode-map
  (cond
   (eglot--managed-mode
    (eglot-mode 1)
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
    (flymake-mode 1)
    (eldoc-mode 1))
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
                     #'eglot-eldoc-function))))

(define-minor-mode eglot-mode
  "Minor mode for all buffers managed by EGLOT in some way."  nil
  nil eglot-mode-map)

(defun eglot--buffer-managed-p (&optional proc)
  "Tell if current buffer is managed by PROC."
  (and buffer-file-name (let ((cur (eglot--current-process)))
                          (or (and (null proc) cur)
                              (and proc (eq proc cur))))))

(defun eglot--maybe-activate-editing-mode (&optional proc)
  "Maybe activate mode function `eglot--managed-mode'.
If PROC is supplied, do it only if BUFFER is managed by it.  In
that case, also signal textDocument/didOpen."
  ;; Called even when revert-buffer-in-progress-p
  (when (eglot--buffer-managed-p proc)
    (eglot--managed-mode 1)
    (eglot--signal-textDocument/didOpen)
    (flymake-start)))

(add-hook 'find-file-hook 'eglot--maybe-activate-editing-mode)


;;; Mode-line, menu and other sugar
;;;
(defvar eglot-menu)

(easy-menu-define eglot-menu eglot-mode-map "EGLOT" `("EGLOT" ))

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
               (`(,_id ,doing ,done-p) (and proc (eglot--spinner proc)))
               (`(,status ,serious-p) (and proc (eglot--status proc))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line
                                '((down-mouse-1 eglot-menu "pop up EGLOT menu"))))
     (when name
       `(":" ,(eglot--mode-line-props
               name 'eglot-mode-line
               '((mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when serious-p
             `("/" ,(eglot--mode-line-props
                     status 'compilation-mode-line-fail
                     '((mouse-1 eglot-events-buffer "go to events buffer")
                       (mouse-3 eglot-clear-status  "clear this status")))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props
                     doing 'compilation-mode-line-run
                     '((mouse-1 eglot-events-buffer "go to events buffer")))))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-1 eglot-events-buffer "go to events buffer")
                       (mouse-3 eglot-clear-status  "clear this status"))))))))))

(add-to-list 'mode-line-misc-info `(eglot-mode (" [" eglot--mode-line-format "] ")))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(defun eglot-shutdown (process &optional sync interactive)
  "Politely ask the server PROCESS to quit.
Forcefully quit it if it doesn't respond.
If SYNC, don't leave this function with the server still
running.  INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-process-or-lose) t t))
  (when interactive
    (eglot--message "(eglot-shutdown) Asking %s politely to terminate"
                    process))
  (let ((brutal (lambda ()
                  (eglot--warn "Brutally deleting existing process %s"
                               process)
                  (setf (eglot--moribund process) t)
                  (delete-process process))))
    (eglot--request
     process :shutdown nil
     :success-fn (lambda (&rest _anything)
                   (when interactive
                     (eglot--message "Now asking %s politely to exit" process))
                   (setf (eglot--moribund process) t)
                   (eglot--request process
                                   :exit
                                   nil
                                   :success-fn brutal
                                   :async-p (not sync)
                                   :error-fn brutal
                                   :timeout-fn brutal))
     :error-fn brutal
     :async-p (not sync)
     :timeout-fn brutal)))

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
               (mapcar (lambda (obj) (plist-get obj :title)) actions)
               nil
               t
               (plist-get (elt actions 0) :title)))
      (if reply
          (eglot--reply process id :result (eglot--obj :title reply))
        (eglot--reply process id
                      :error (eglot--obj :code -32800
                                         :message "User cancelled"))))))

(cl-defun eglot--server-window/logMessage (_process &key type message)
  "Handle notification window/logMessage"
  (eglot--log (propertize "Server reports (type=%s): %s"
                          'face (if (<= type 1) 'error))
              type message))

(cl-defun eglot--server-telemetry/event (_process &rest any)
  "Handle notification telemetry/event"
  (eglot--log "Server telemetry: %s" any))

(defvar-local eglot--current-flymake-report-fn nil
  "Current flymake report function for this buffer")

(defvar-local eglot--unreported-diagnostics nil
  "Unreported diagnostics for this buffer.")

(cl-defun eglot--server-textDocument/publishDiagnostics
    (_process &key uri diagnostics)
  "Handle notification publishDiagnostics"
  (let* ((obj (url-generic-parse-url uri))
	 (filename (car (url-path-and-query obj)))
         (buffer (find-buffer-visiting filename)))
    (cond
     (buffer
      (with-current-buffer buffer
        (cl-flet ((pos-at (pos-plist)
                          (save-excursion (goto-char (point-min))
                                          (forward-line (plist-get pos-plist :line))
                                          (forward-char
                                           (min (plist-get pos-plist :character)
                                                (- (line-end-position)
                                                   (line-beginning-position))))
                                          (point))))
          (cl-loop for diag-spec across diagnostics
                   collect (cl-destructuring-bind (&key range severity _group
                                                        _code source message)
                               diag-spec
                             (cl-destructuring-bind (&key start end)
                                 range
                               (let* ((begin-pos (pos-at start))
                                      (end-pos (pos-at end)))
                                 (flymake-make-diagnostic
                                  (current-buffer)
                                  begin-pos end-pos
                                  (cond ((<= severity 1) :error)
                                        ((= severity 2)  :warning)
                                        (t               :note))
                                  (concat source ": " message)))))
                   into diags
                   finally
                   (if eglot--current-flymake-report-fn
                       (funcall eglot--current-flymake-report-fn
                                diags)
                     (setq eglot--unreported-diagnostics
                           diags))))))
     (t
      (eglot--message "OK so %s isn't visited" filename)))))

(cl-defun eglot--server-client/registerCapability
    (proc &key id registrations)
  "Handle notification client/registerCapability"
  (let ((jsonrpc-id id)
        (done (make-symbol "done")))
    (catch done
      (mapc
       (lambda (reg)
         (apply
          (cl-function
           (lambda (&key id method registerOptions)
             (pcase-let*
                 ((handler-sym (intern (concat "eglot--register-"
                                               method)))
                  (`(,ok ,message)
                   (and (functionp handler-sym)
                        (apply handler-sym proc :id id registerOptions))))
               (unless ok
                 (throw done
                        (eglot--reply proc jsonrpc-id
                                      :error (eglot--obj
                                              :code -32601
                                              :message (or message "sorry :-("))))))))
          reg))
       registrations)
      (eglot--reply proc id :result (eglot--obj :message "OK")))))

(defvar eglot--recent-before-changes nil
  "List of recent changes as collected by `eglot--before-change'.")
(defvar eglot--recent-after-changes nil
  "List of recent changes as collected by `eglot--after-change'.")

(defvar-local eglot--versioned-identifier 0)

(defun eglot--current-buffer-TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  (eglot--obj :uri (eglot--path-to-uri buffer-file-name)))

(defun eglot--current-buffer-VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (eglot--current-buffer-TextDocumentIdentifier)
          (eglot--obj :version eglot--versioned-identifier)))

(defun eglot--current-buffer-TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (eglot--current-buffer-VersionedTextDocumentIdentifier)
   (eglot--obj :languageId
               (if (string-match "\\(.*\\)-mode" (symbol-name major-mode))
                   (match-string 1 (symbol-name major-mode))
                 "unknown")
               :text
               (save-restriction
                 (widen)
                 (buffer-substring-no-properties (point-min) (point-max))))))

(defun eglot--before-change (start end)
  "Hook onto `before-change-functions'.
Records START and END, crucially convert them into
LSP (line/char) positions before that information is
lost (because the after-change thingy doesn't know if newlines
were deleted/added)"
  (push (list (eglot--pos-to-lsp-position start)
              (eglot--pos-to-lsp-position end))
        eglot--recent-before-changes))

(defun eglot--after-change (start end pre-change-length)
  "Hook onto `after-change-functions'.
Records START, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (push (list start end pre-change-length
              (buffer-substring-no-properties start end))
        eglot--recent-after-changes))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (unwind-protect
      (when (or eglot--recent-before-changes
                eglot--recent-after-changes)
        (let* ((proc (eglot--current-process-or-lose))
               (sync-kind (plist-get (eglot--capabilities proc)
                                     :textDocumentSync))
               (emacs-messup
                (/= (length eglot--recent-before-changes)
                    (length eglot--recent-after-changes)))
               (full-sync-p (or (eq sync-kind 1) emacs-messup)))
          (when emacs-messup
            (unless (eq sync-kind 1)
              (eglot--warn "Using full sync because before: %s and after: %s"
                           eglot--recent-before-changes
                           eglot--recent-after-changes)))
          (save-restriction
            (widen)
            (unless (or (not sync-kind)
                        (eq sync-kind 0))
              (eglot--notify
               proc
               :textDocument/didChange
               (eglot--obj
                :textDocument
                (eglot--current-buffer-VersionedTextDocumentIdentifier)
                :contentChanges
                (if full-sync-p
                    (vector
                     (eglot--obj
                      :text (buffer-substring-no-properties (point-min)
                                                            (point-max))))
                  (apply
                   #'vector
                   (mapcar
                    (pcase-lambda (`(,before-start-position
                                     ,before-end-position
                                     ,_after-start
                                     ,_after-end
                                     ,len
                                     ,after-text))
                      (eglot--obj :range (eglot--obj :start before-start-position
                                                     :end before-end-position)
                                  :rangeLength len
                                  :text after-text))
                    (reverse (cl-mapcar 'append
                                        eglot--recent-before-changes
                                        eglot--recent-after-changes)))))))))))
    (setq eglot--recent-before-changes nil
          eglot--recent-after-changes nil)))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (eglot--notify (eglot--current-process-or-lose)
                 :textDocument/didOpen
                 (eglot--obj :textDocument
                             (eglot--current-buffer-TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (eglot--notify (eglot--current-process-or-lose)
                 :textDocument/didClose
                 (eglot--obj :textDocument
                             (eglot--current-buffer-TextDocumentIdentifier))))

(defun eglot--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (eglot--notify
   (eglot--current-process-or-lose)
   :textDocument/willSave
   (eglot--obj
    :reason 1 ; Manual, emacs laughs in the face of auto-save muahahahaha
    :textDocument (eglot--current-buffer-TextDocumentIdentifier))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (eglot--notify
   (eglot--current-process-or-lose)
   :textDocument/didSave
   (eglot--obj
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (eglot--current-buffer-TextDocumentIdentifier))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "An EGLOT Flymake backend.
Calls REPORT-FN maybe if server publishes diagnostics in time."
  ;; Maybe call immediately if anything unreported (this will clear
  ;; any pending diags)
  (when eglot--unreported-diagnostics
    (funcall report-fn eglot--unreported-diagnostics)
    (setq eglot--unreported-diagnostics nil))
  ;; Setup so maybe it's called later, too.
  (setq eglot--current-flymake-report-fn report-fn)
  ;; Take this opportunity to signal a didChange that might eventually
  ;; make the server report new diagnostics.
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
  (xref-make name (xref-make-file-location
                   (eglot--uri-to-path uri)
                   ;; F!@(#*&#$)CKING OFF-BY-ONE again
                   (1+ (plist-get position :line))
                   (plist-get position :character))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
  (when (eglot--server-capable :documentSymbolProvider)
    (let ((proc (eglot--current-process-or-lose))
          (text-id (eglot--current-buffer-TextDocumentIdentifier)))
      (completion-table-with-cache
       (lambda (string)
         (setq eglot--xref-known-symbols
               (eglot--mapply
                (eglot--lambda (&key name kind location containerName)
                  (propertize name
                              :position (plist-get
                                         (plist-get location :range)
                                         :start)
                              :locations (list location)
                              :textDocument text-id
                              :kind kind
                              :containerName containerName))
                (eglot--sync-request proc
                                     :textDocument/documentSymbol
                                     (eglot--obj
                                      :textDocument text-id))))
         (all-completions string eglot--xref-known-symbols))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
  (let ((symatpt (symbol-at-point)))
    (when symatpt
      (propertize (symbol-name symatpt)
                  :textDocument (eglot--current-buffer-TextDocumentIdentifier)
                  :position (eglot--pos-to-lsp-position)))))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot)) identifier)
  (let* ((rich-identifier
          (car (member identifier eglot--xref-known-symbols)))
         (location-or-locations
          (if rich-identifier
              (get-text-property 0 :locations rich-identifier)
            (eglot--sync-request (eglot--current-process-or-lose)
                                 :textDocument/definition
                                 (eglot--obj
                                  :textDocument
                                  (get-text-property 0 :textDocument identifier)
                                  :position
                                  (get-text-property 0 :position identifier))))))
    (eglot--mapply
     (eglot--lambda (&key uri range)
       (eglot--xref-make identifier uri (plist-get range :start)))
     location-or-locations)))

(cl-defmethod xref-backend-references ((_backend (eql eglot)) identifier)
  (unless (eglot--server-capable :referencesProvider) (cl-return nil))
  (let* ((identifier (if (get-text-property 0 :position identifier)
                         identifier
                       (car (member identifier eglot--xref-known-symbols))))
         (position
          (and identifier (get-text-property 0 :position identifier)))
         (textDocument
          (and identifier (get-text-property 0 :textDocument identifier))))
    (unless (and position textDocument)
      (eglot--error "Don't know where %s is in the workspace" identifier))
    (eglot--mapply
     (eglot--lambda (&key uri range)
       (eglot--xref-make identifier uri (plist-get range :start)))
     (eglot--sync-request (eglot--current-process-or-lose)
                          :textDocument/references
                          (eglot--obj
                           :textDocument
                           textDocument
                           :position
                           position
                           :context (eglot--obj :includeDeclaration t))))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (eglot--mapply
     (eglot--lambda (&key name location &allow-other-keys)
       (let ((range (plist-get location :range))
             (uri (plist-get location :uri)))
         (eglot--xref-make name uri (plist-get range :start))))
     (eglot--sync-request (eglot--current-process-or-lose)
                          :workspace/symbol
                          (eglot--obj :query pattern)))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  (let ((bounds (bounds-of-thing-at-point 'sexp))
        (proc (eglot--current-process-or-lose)))
    (when (eglot--server-capable :completionProvider)
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (completion-table-dynamic
        (lambda (_ignored)
          (let* ((resp (eglot--sync-request
                        proc
                        :textDocument/completion
                        (eglot--obj
                         :textDocument (eglot--current-buffer-TextDocumentIdentifier)
                         :position (eglot--pos-to-lsp-position))))
                 (items (if (vectorp resp) resp (plist-get resp :items))))
            (eglot--mapply
             (eglot--lambda (&key insertText label kind detail
                                  documentation sortText)
               (propertize insertText
                           :label label :kind kind :detail detail
                           :documentation documentation :sortText sortText))
             items))))
       :annotation-function
       (lambda (what) (let ((detail (get-text-property 0 :detail what))
                            (kind (get-text-property 0 :kind what)))
                        (format "%s%s"
                                detail
                                (if kind
                                    (format " (%s)" (cdr (assoc kind eglot--kind-names)))
                                  ""))))
       :display-sort-function
       (lambda (items) (sort items (lambda (a b)
                                     (string-lessp
                                      (get-text-property 0 :sortText a)
                                      (get-text-property 0 :sortText b)))))))))

(defun eglot-eldoc-function ()
  "EGLOT's `eldoc-documentation-function' function."
  (when (eglot--server-capable :hoverProvider)
    (eglot--request (eglot--current-process-or-lose)
                    :textDocument/hover
                    (eglot--obj
                     :textDocument (eglot--current-buffer-TextDocumentIdentifier)
                     :position (eglot--pos-to-lsp-position))
                    :success-fn (eglot--lambda (&key contents _range)
                                  (eldoc-message
                                   (mapconcat #'eglot--format-markup
                                              (if (vectorp contents)
                                                  contents
                                                (list contents))
                                              "\n")))))
  nil)


;;; Dynamic registration
;;;
(cl-defun eglot--register-workspace/didChangeWatchedFiles
    (_proc &key _id _watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles"
  ;; TODO: file-notify-add-watch and
  ;; file-notify-rm-watch can probably handle this
  (list nil "Sorry, can't do this yet"))


;;; Rust-specific
;;;
(cl-defun eglot--server-window/progress
    (process &key id done title &allow-other-keys)
  "Handle notification window/progress"
  (setf (eglot--spinner process) (list id title done)))

(provide 'eglot)
;;; eglot.el ends here
