(uiop:define-package :lem-copilot/client
  (:use :cl
        :lem-copilot/utils
        :lem-copilot/logger)
  (:export :run-client
           :client-process
           :connect
           :request
           :request-async
           :notify
           :initialize
           :initialized
           :set-editor-info
           :sign-in-initiate
           :sign-in-confirm
           :check-status
           :text-document/did-open
           :text-document/did-close
           :text-document/did-change
           :text-document/did-focus
           :get-completions
           :notify-shown
           :notify-accepted
           :notify-rejected
           :get-completions-cycling
           :+trigger-kind.invoked+
           :+trigger-kind.trigger-character+
           :text-document/inline-completion
           :text-document/did-show-completion
           :$/cancel-request))
(in-package :lem-copilot/client)

(defparameter *logging-output* t)
(defparameter *logging-request* nil)
(defparameter *display-copilot-warning* t)

(defstruct client
  jsonrpc
  process
  stream)

(defun run-client (&key process)
  (let ((stream (lem-lsp-mode/async-process-stream:make-input-stream
                 process
                 :logger (when *logging-output* 'logger)))
        (client (jsonrpc:make-client)))
    (make-client :jsonrpc client
                 :process process
                 :stream stream)))

(defun request-log (type method params)
  (when *logging-request*
    (do-log (format nil "~A ~A ~A~%" type method (pretty-json params)))))

(defun display-copilot-warning ()
  (when *display-copilot-warning*
    (lem:display-popup-message
     (format nil
             "~{~A~^~%~}"
             '("Copilot has issued a warning."
               "If it does not work properly, please execute `M-x copilot-restart`."
               ""
               "To view the copilot log, execute `M-x test/copilot-log`."))
     :style '(:gravity :top)
     :timeout 10)))

(defun logger (output)
  (when (search "MaxListenersExceededWarning: Possible EventEmitter memory leak detected. 11 change listeners added to [EventEmitter]." output)
    (display-copilot-warning))
  (do-log output))

(defun connect (client)
  (jsonrpc/client:client-connect-using-class
   (client-jsonrpc client)
   'lem-lsp-mode/lem-stdio-transport:lem-stdio-transport
   :process (client-process client)
   :stream (client-stream client)))

(defun request (client method params)
  (request-log :request method params)
  (jsonrpc:call (client-jsonrpc client) method params))

(defun request-async (client method params &key callback error-callback)
  (request-log :request-async method params)
  (jsonrpc:call-async (client-jsonrpc client)
                      method
                      params
                      callback
                      error-callback))

(defun notify (client method params)
  (request-log :notify method params)
  (jsonrpc:notify (client-jsonrpc client) method params))

(defun initialize (client &key callback)
  (request-async client
                 "initialize"
                 (hash "capabilities"
                       (hash "workspace"
                             (hash "workspaceFolders" t
                                   "editorConfiguration" (hash "enableAutoCompletions" t))))
                 :callback callback))

(defun initialized (client)
  (notify client "initialized" (hash)))

(defun set-editor-info (client &key callback)
  (request-async client
                 "setEditorInfo"
                 (hash "editorInfo"
                       (hash "name" "Lem"
                             "version" (lem:get-version-string))
                       "editorPluginInfo" (hash "name" "lem-copilot"
                                                "version" "0.0"))
                 :callback callback))

(defun sign-in-initiate (client)
  (request client
           "signInInitiate"
           (hash)))

(defun sign-in-confirm (client user-code &key callback)
  (request-async client
                 "signInConfirm"
                 (hash "userCode" user-code)
                 :callback callback
                 :error-callback #'default-error-callback))

(defun check-status (client)
  (request client
           "checkStatus"
           (hash)))

(defun text-document/did-open (client &key uri language-id version text)
  (notify client
          "textDocument/didOpen"
          (hash "textDocument" (hash "uri" uri
                                     "languageId" language-id
                                     "version" version
                                     "text" text))))

(defun text-document/did-close (client &key uri)
  (notify client
          "textDocument/didClose"
          (hash "textDocument" (hash "uri" uri))))

(defun text-document/did-change (client &key uri version content-changes)
  (notify client
          "textDocument/didChange"
          (hash "textDocument" (hash "uri" uri
                                     "version" version)
                "contentChanges" content-changes)))

(defun text-document/did-focus (client &key uri)
  (notify client
          "textDocument/didFocus"
          (hash "textDocument" (hash "uri" uri))))

(defun get-completions (client &key doc callback)
  (request-async client
                 "getCompletions"
                 (hash "doc" doc)
                 :callback callback
                 :error-callback #'default-error-callback))

(defun notify-shown (client uuid)
  (request-async client "notifyShown" (hash "uuid" uuid)))

(defun notify-accepted (client uuid)
  (request-async client
                 "notifyAccepted"
                 (hash "uuid" uuid)
                 :error-callback #'default-error-callback))

(defun notify-rejected (client uuid)
  (request-async client
                 "notifyRejected"
                 (hash "uuids" (vector uuid))
                 :error-callback #'default-error-callback))

(defun get-completions-cycling (client &key doc callback error-callback)
  (request-async client
                 "getCompletionsCycling"
                 (hash "doc" doc)
                 :callback callback
                 :error-callback error-callback))

(defparameter +trigger-kind.invoked+ 1)
(defparameter +trigger-kind.trigger-character+ 1)

(defun text-document/inline-completion
    (client &key callback
                error-callback
                uri
                position
                insert-spaces
                tab-size
                (trigger-kind +trigger-kind.trigger-character+))
  (request-async client
                 "textDocument/inlineCompletion"
                 (hash "textDocument" (hash "uri" uri)
                       "position" position
                       "formattingOptions" (hash "insertSpaces" insert-spaces
                                                 "tabSize" tab-size)
                       "context" (hash "triggerKind" trigger-kind))
                 :callback callback
                 :error-callback (when error-callback (lambda (&rest args)
                                                        (apply error-callback args)))))

(defun text-document/did-show-completion (client item)
  (notify client
	  "textDocument/didShowCompletion"
	  (hash "item" item)))

(defun $/cancel-request (client id)
  (notify client
	  "$/cancelRequest"
	  (hash "id" id)))

(defun default-error-callback (&rest args)
  (lem:send-event (lambda () (lem:message "~A" args))))
