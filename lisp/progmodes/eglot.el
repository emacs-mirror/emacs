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

(defgroup eglot nil
  "Interaction with Language Server Protocol servers"
  :prefix "eglot-"
  :group 'applications)

(defvar eglot-executables '((rust-mode . ("rls")))
  "Alist mapping major modes to server executables")

(defvar eglot--processes-by-project (make-hash-table :test #'equal))

(defun eglot--current-process ()
  "The current logical EGLOT process"
  (let ((cur (project-current)))
    (and cur
         (gethash cur eglot--processes-by-project))))

(defun eglot--current-process-or-lose ()
  (or (eglot--current-process)
      (eglot--error "No current EGLOT process%s"
                    (if (project-current) ""
                      " (Also no current project)"))))

(defmacro eglot--define-process-var (var-sym initval &optional doc)
  (declare (indent 2))
  `(progn
     (put ',var-sym 'function-documentation ,doc)
     (defun ,var-sym (&optional process)
       (let* ((proc (or process (eglot--current-process-or-lose)))
              (probe (process-get proc ',var-sym)))
         (or probe
             (let ((def ,initval))
               (process-put proc ',var-sym def)
               def))))
     (gv-define-setter ,var-sym (to-store &optional process)
       (let ((prop ',var-sym))
         `(let ((proc (or ,process (eglot--current-process-or-lose))))
            (process-put proc ',prop ,to-store))))))

(eglot--define-process-var eglot--message-mark nil
  "Point where next unread message starts")

(eglot--define-process-var eglot--short-name nil
  "A short name")

(eglot--define-process-var eglot--expected-bytes nil
  "How many bytes declared by server")

(eglot--define-process-var eglot--pending-continuations (make-hash-table)
  "A hash table of request ID to continuation lambdas")

(eglot--define-process-var eglot--events-buffer nil
  "A buffer pretty-printing the EGLOT RPC events")

(cl-defmacro eglot--request (process
                              method
                              params
                              success-fn
                              &key
                              error-fn
                              timeout-fn
                              (async-p t))
  (append `(eglot--call-with-request
            ,process
            ,async-p
            ,method
            ,params
            (cl-function ,success-fn))
          (and error-fn
               `((cl-function ,error-fn)))
          (and timeout-fn
               `((cl-function ,timeout-fn)))))

(defun eglot--command (&optional errorp)
  (let ((probe (cdr (assoc major-mode eglot-executables))))
    (unless (or (not errorp)
                probe)
      (eglot--error "Don't know how to start EGLOT for %s buffers"
                    major-mode))
    probe))

(defun eglot-new-process (&optional interactive)
  "Starts a new EGLOT process and initializes it"
  (interactive (list t))
  (let ((project (project-current))
        (command (eglot--command 'errorp)))
    (unless project (eglot--error "Cannot work without a current project!"))
    (let ((current-process (eglot--current-process)))
      (when (and current-process
                 (process-live-p current-process))
        (eglot-quit-server current-process 'sync)))
    (let* ((short-name (file-name-base
                        (directory-file-name
                         (car (project-roots (project-current))))))
           (good-name
            (format "EGLOT server (%s)" short-name)))
      (with-current-buffer (get-buffer-create
                            (format "*%s inferior*" good-name))
        (let* ((proc
                (make-process :name good-name
                              :buffer (current-buffer)
                              :command command
                              :connection-type 'pipe
                              :filter 'eglot--process-filter
                              :sentinel 'eglot--process-sentinel
                              :stderr (get-buffer-create (format "*%s stderr*"
                                                                 good-name))))
               (inhibit-read-only t))
          (setf (eglot--short-name proc) short-name)
          (puthash (project-current) proc eglot--processes-by-project)
          (erase-buffer)
          (let ((marker (point-marker)))
            (set-marker-insertion-type marker nil)
            (setf (eglot--message-mark proc) marker))
          (read-only-mode t)
          (with-current-buffer (eglot-events-buffer proc)
            (let ((inhibit-read-only t))
              (insert
               (format "\n-----------------------------------\n"))))
          (eglot--protocol-initialize proc interactive))))))


(defun eglot--process-sentinel (process change)
  (with-current-buffer (process-buffer process)
    (eglot--debug "Process state changed to %s" change)
    (when (not (process-live-p process))
      ;; Remember to cancel all timers
      ;;
      (maphash (lambda (id v) 
                 (cl-destructuring-bind (_success _error timeout) v
                   (eglot--message "Cancelling timer for continuation %s" id)
                   (cancel-timer timeout)))
               (eglot--pending-continuations process))
      (cond ((process-get process 'eglot--moribund)
             (eglot--message "Process exited with status %s"
                             (process-exit-status process)))
            (t
             (eglot--warn "Process unexpectedly changed to %s" change))))))

(defun eglot--process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t)
            (pre-insertion-mark (copy-marker (process-mark proc)))
            (expected-bytes (eglot--expected-bytes proc))
            (message-mark (eglot--message-mark proc)))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))

        ;; check for new message header
        ;;
        (save-excursion
          (goto-char pre-insertion-mark)
          (let* ((match (search-forward-regexp
                         "\\(?:.*: .*\r\n\\)*Content-Length: \\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                         (+ (point) 100)
                         t))
                 (new-expected-bytes (and match
                                          (string-to-number (match-string 1)))))
            (when new-expected-bytes
              (when expected-bytes
                (eglot--warn
                 (concat "Unexpectedly starting new message but %s bytes"
                         "reportedly remaining from previous one")
                 expected-bytes))
              (set-marker message-mark (point))
              (setf (eglot--expected-bytes proc) new-expected-bytes)
              (setq expected-bytes new-expected-bytes))))

        ;; check for message body
        ;;
        (let ((available-bytes (- (position-bytes (process-mark proc))
                                  (position-bytes message-mark))))
          (cond ((not expected-bytes)
                 (eglot--warn
                  "Skipping %s bytes of unexpected garbage from process %s"
                  available-bytes
                  proc)
                 (set-marker message-mark (process-mark proc)))
                ((>= available-bytes
                     expected-bytes)
                 (let* ((message-end (byte-to-position
                                      (+ (position-bytes message-mark)
                                         expected-bytes))))
                   (save-excursion
                     (save-restriction
                       (goto-char message-mark)
                       (narrow-to-region message-mark
                                         message-end)
                                              (eglot--process-receive proc (let ((json-object-type 'plist))
                                                       (json-read)))))
                   (set-marker message-mark message-end)
                   (setf (eglot--expected-bytes proc) nil)))
                (t
                 ;; just adding some stuff to the end that doesn't yet
                 ;; complete the message
                 )))))))

(defun eglot-events-buffer (process &optional interactive)
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
                         (setf (eglot--events-buffer process)
                               buffer))
                       buffer))))
    (when interactive
      (display-buffer buffer))
    buffer))

(defun eglot--log-event (proc type message)
  (with-current-buffer (eglot-events-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "%s: \n%s\n" type (pp-to-string message))))))

(defun eglot--process-receive (proc message)
  (let ((inhibit-read-only t))
    (insert (format "Server said:\n%s\n" message)))
  (eglot--log-event proc 'server message)
  ;; Maybe this is a responsee
  ;;
  (let* ((response-id (plist-get message :id))
         (err (plist-get message :error))
         (continuations (and response-id
                             (gethash response-id (eglot--pending-continuations)))))
    (cond ((and response-id
                (not continuations))
           (eglot--warn "Ooops no continuation for id %s" response-id))
          (continuations
           (cancel-timer (third continuations))
           (remhash response-id
                    (eglot--pending-continuations))
           (cond (err
                  (apply (second continuations) err))
                 (t
                  (apply (first continuations) (plist-get message :result)))))
          (t
           (let* ((method (plist-get message :method))
                  (handler-sym (intern (concat "eglot--"
                                              method))))
             (if (functionp handler-sym)
                 (apply handler-sym proc (plist-get message :params))
               (eglot--debug "No implemetation for notification %s yet"
                         method)))))))

(defvar eglot--expect-carriage-return nil)

(defun eglot--process-send (proc message)
  (let* ((json (json-encode message))
         (to-send (format "Content-Length: %d\r\n\r\n%s"
                          (string-bytes json)
                          json)))
    (process-send-string proc to-send)
    (eglot--log-event proc 'client message)))

(defvar eglot--next-request-id 0)

(defun eglot--next-request-id ()
  (setq eglot--next-request-id (1+ eglot--next-request-id)))

(defun eglot-forget-pending-continuations (process)
  (interactive (eglot--current-process-or-lose))
  (clrhash (eglot--pending-continuations process)))

(defun eglot--call-with-request (process
                                  async-p
                                  method
                                  params
                                  success-fn
                                  &optional error-fn timeout-fn)
  (let* ((id (eglot--next-request-id))
         (timeout-fn (or timeout-fn
                         (lambda ()
                           (eglot--warn "Tired of waiting for reply to %s" id)
                           (remhash id (eglot--pending-continuations process)))))
         (error-fn (or error-fn
                       (cl-function
                        (lambda (&key code message)
                          (eglot--warn "Request id=%s errored with code=%s: %s"
                                        id code message)))))
         (catch-tag (cl-gensym (format "eglot--tag-%d-" id))))
    (eglot--process-send process
                          `(:jsonrpc  "2.0"
                                      :id  ,id
                                      :method  ,method
                                      :params  ,params))
    (catch catch-tag
      (let ((timeout-timer
             (run-with-timer 5 nil
                             (if async-p
                                 timeout-fn
                               (lambda ()
                                 (throw catch-tag (apply timeout-fn)))))))
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
                  (eglot--error "Process %s died unexpectedly" process))
                (accept-process-output nil 0.01))
            (cancel-timer timeout-timer)))))))


;;; Requests
;;; 

(defun eglot--protocol-initialize (process interactive)
  (eglot--request
   process
   :initialize
   `(:processId  ,(emacs-pid)
                 :rootPath  ,(concat "" ;; FIXME RLS doesn't like "file://"
                                     "file://"
                                     (expand-file-name (car (project-roots
                                                             (project-current)))))
                 :initializationOptions  []
                 :capabilities (:workspace (:executeCommand (:dynamicRegistration t))
                                           :textDocument (:synchronization (:didSave t))))
   (lambda (&key capabilities)
     (cl-destructuring-bind
         (&rest all
                &key
                ;; capabilities reported by server
                _textDocumentSync
                _hoverProvider
                _completionProvider
                _definitionProvider
                _referencesProvider
                _documentHighlightProvider
                _documentSymbolProvider
                _workspaceSymbolProvider
                _codeActionProvider
                _documentFormattingProvider
                _documentRangeFormattingProvider
                _renameProvider
                _executeCommandProvider
                )
         capabilities
       (when interactive
         (eglot--message
          "So yeah I got lots (%d) of capabilities"
          (length all)))))))

(defun eglot-quit-server (process &optional sync)
  (interactive (list (eglot--current-process-or-lose)))
  (eglot--message "Asking server to terminate")
  (eglot--request
      process
      :shutdown
      nil
      (lambda (&rest _anything)
        (eglot--message "Now asking server to exit")
        (process-put process 'eglot--moribund t)
        (eglot--process-send process
                              `(:jsonrpc  "2.0"
                                          :method  :exit)))
      :async-p (not sync)
      :timeout-fn (lambda ()
                    (eglot--warn "Brutally deleting existing process %s"
                                  process)
                    (process-put process 'eglot--moribund t)
                    (delete-process process))))


;;; Notifications
;;;
(cl-defun eglot--textDocument/publishDiagnostics
    (_process &key uri diagnostics)
  "Handle notification publishDiagnostics"
  (eglot--message "So yeah I got %s for %s"
                  diagnostics uri))


;;; Helpers
;;;
(defun
    eglot--debug (format &rest args)
  (display-warning 'eglot
     (apply #'format format args)
     :debug))

(defun eglot--error (format &rest args)
  (error (apply #'format format args)))

(defun eglot--message (format &rest args)
  (message (concat "[eglot] " (apply #'format format args))))

(defun eglot--warn (format &rest args)
  (display-warning 'eglot
     (apply #'format format args)
     :warning))



;;; Mode line
;;;


(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in EGLOT's mode line."
  :group 'eglot)

(define-minor-mode eglot-mode
  "Minor mode for buffers where EGLOT is possible")

(defvar eglot-menu)

(defvar eglot-mode-map (make-sparse-keymap))

(easy-menu-define eglot-menu eglot-mode-map "SLY"
  `("EGLOT" ))

(defvar eglot--mode-line-format
  `(:eval (eglot--mode-line-format)))

(put 'eglot--mode-line-format 'risky-local-variable t)

(defun eglot--mode-line-format ()
  (let* ((proc (eglot--current-process))
         (name (and proc
                    (process-live-p proc)
                    (eglot--short-name proc)))
         (pending (and proc
                       (hash-table-count
                        (eglot--pending-continuations proc))))
         (format-number (lambda (n) (cond ((and n (not (zerop n)))
                                           (format "%d" n))
                                          (n "-")
                                          (t "*")))))
    (append
     `((:propertize "eglot"
                    face eglot-mode-line
                    keymap ,(let ((map (make-sparse-keymap)))
                              (define-key map [mode-line down-mouse-1]
                                eglot-menu)
                              map)
                    mouse-face mode-line-highlight
                    help-echo "mouse-1: pop-up EGLOT menu"
                    ))
     (if name
         `(" "
           (:propertize
            ,name
            face eglot-mode-line
            keymap ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1] 'eglot-events-buffer)
                      (define-key map [mode-line mouse-2] 'eglot-quit-server)
                      (define-key map [mode-line mouse-3] 'eglot-new-process)
                      map)
            mouse-face mode-line-highlight
            help-echo ,(concat "mouse-1: events buffer\n"
                               "mouse-2: quit server\n"
                               "mouse-3: new process"))
           "/"
           (:propertize
            ,(funcall format-number pending)
            help-echo ,(if name
                           (format
                            "%s pending events outgoing\n%s"
                            pending
                            (concat "mouse-1: go to events buffer"
                                    "mouse-3: forget pending continuations"))
                         "No current connection")
            mouse-face mode-line-highlight
            face ,(cond ((and pending (cl-plusp pending))
                         'warning)
                        (t
                         'eglot-mode-line))
            keymap ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1]
                        'eglot-events-buffer)
                      (define-key map [mode-line mouse-3]
                        'eglot-forget-pending-continuations)
                      map)))))))

(add-to-list 'mode-line-misc-info
             `(t
               (" [" eglot--mode-line-format "] ")))

(provide 'eglot)
;;; eglot.el ends here
