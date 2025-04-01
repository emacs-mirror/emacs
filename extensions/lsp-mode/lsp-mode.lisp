(defpackage :lem-lsp-mode/lsp-mode
  (:nicknames :lem-lsp-mode)
  (:use :cl
        :lem
        :alexandria
        :lem-lsp-base/type
        :lem-lsp-base/converter
        :lem-lsp-base/yason-utils
        :lem-lsp-base/utils
        :lem-lsp-mode/spec
        :lem/common/utils)
  (:shadow :execute-command)
  (:import-from :lem-language-client/request)
  (:import-from :lem/context-menu)
  (:local-nicknames (:client :lem-lsp-mode/client))
  (:local-nicknames (:request :lem-language-client/request))
  (:local-nicknames (:completion :lem/completion-mode))
  (:local-nicknames (:context-menu :lem/context-menu))
  (:local-nicknames (:spinner :lem/loading-spinner))
  (:local-nicknames (:language-mode :lem/language-mode))
  (:export :*inhibit-highlight-diagnotics*
           :get-buffer-from-text-document-identifier
           :spec-initialization-options
           :register-lsp-method
           :define-language-spec))
(in-package :lem-lsp-mode/lsp-mode)

;;;
(define-condition not-found-program (editor-error)
  ((name :initarg :name
         :initform (required-argument :name)
         :reader not-found-program-name)
   (spec :initarg :spec
         :initform (required-argument :spec)
         :reader not-found-program-spec))
  (:report (lambda (c s)
             (with-slots (name spec) c
               (format s (gen-install-help-message name spec))))))

(defun gen-install-help-message (program spec)
  (with-output-to-string (out)
    (format out "\"~A\" is not installed." program)
    (when (spec-install-command spec)
      (format out
              "~&You can install it with the following command.~2% $ ~A"
              (spec-install-command spec)))
    (when (spec-readme-url spec)
      (format out "~&~%See follow for the readme URL~2% ~A ~%" (spec-readme-url spec)))))

(defun check-exist-program (program spec)
  (unless (exist-program-p program)
    (error 'not-found-program :name program :spec spec)))

;;;
(defun server-process-buffer-name (spec)
  (format nil "*Lsp <~A>*" (spec-language-id spec)))

(defmethod run-server-using-mode ((mode (eql :tcp)) spec)
  (flet ((output-callback (string)
           (let* ((buffer (make-buffer (server-process-buffer-name spec)))
                  (point (buffer-point buffer)))
             (buffer-end point)
             (insert-string point string))))
    (let* ((port (or (spec-port spec) (lem/common/socket:random-available-port)))
           (process (when-let (command (get-spec-command spec port))
                      (check-exist-program (first command) spec)
                      (lem-process:run-process command :output-callback #'output-callback))))
      (make-instance 'client:tcp-client :process process :port port))))

(defmethod run-server-using-mode ((mode (eql :stdio)) spec)
  (let ((command (get-spec-command spec)))
    (check-exist-program (first command) spec)
    (let ((process (async-process:create-process command :nonblock nil)))
      (make-instance 'client:stdio-client :process process))))

(defmethod run-server (spec)
  (run-server-using-mode (spec-connection-mode spec) spec))

;;;
(defmacro with-jsonrpc-error (() &body body)
  (with-unique-names (c)
    `(handler-case (progn ,@body)
       (jsonrpc/errors:jsonrpc-callback-error (,c)
         (editor-error "~A" ,c)))))

(defun jsonrpc-editor-error (message code)
  (editor-error "JSONRPC-CALLBACK-ERROR: ~A (Code=~A)" message code))

(defun async-request (client request params &key then)
  (request:request-async client
                         request
                         params
                         (lambda (response)
                           (send-event (lambda () (funcall then response))))
                         (lambda (message code)
                           (send-event (lambda () (jsonrpc-editor-error message code))))))

(defun display-message (text &key style source-window)
  (when text
    (show-message text
                  :style style
                  :timeout nil
                  :source-window source-window)))

(defun make-temporary-unwrap-buffer ()
  (let ((buffer (make-buffer nil :temporary t :enable-undo-p nil)))
    (setf (variable-value 'lem:line-wrap :buffer buffer) nil)
    buffer))

;;;
(defun buffer-language-spec (buffer)
  (get-language-spec (language-mode:buffer-language-mode buffer)))

(defun buffer-language-id (buffer)
  (when-let (spec (buffer-language-spec buffer))
    (spec-language-id spec)))

(defun buffer-version (buffer)
  (buffer-modified-tick buffer))

(defun buffer-uri (buffer)
  ;; TODO: lem-language-server::buffer-uri
  (if (buffer-filename buffer)
      (pathname-to-uri (buffer-filename buffer))
      (format nil "buffer://~A" (buffer-name buffer))))

(defun compute-root-uri (spec buffer)
  (pathname-to-uri
   (language-mode:find-root-directory (buffer-directory buffer)
                                      (spec-root-uri-patterns spec))))

;;;
(defclass workspace ()
  ((root-uri
    :initarg :root-uri
    :initform nil
    :accessor workspace-root-uri)
   (client
    :initarg :client
    :initform nil
    :accessor workspace-client)
   (spec
    :initarg :spec
    :initform nil
    :accessor workspace-spec)
   (server-capabilities
    :initarg :server-capabilities
    :initform nil
    :accessor workspace-server-capabilities)
   (server-info
    :initarg :server-info
    :initform nil
    :accessor workspace-server-info)
   (trigger-characters
    :initarg :trigger-characters
    :initform (make-hash-table)
    :accessor workspace-trigger-characters)
   (plist
    :initarg :plist
    :initform nil
    :accessor workspace-plist)))

(defun make-workspace (&key spec client buffer)
  (make-instance 'workspace
                 :spec spec
                 :client client
                 :root-uri (compute-root-uri spec buffer)))

(defun workspace-value (workspace key &optional default)
  (getf (workspace-plist workspace) key default))

(defun (setf workspace-value) (value workspace key &optional default)
  (declare (ignore default))
  (setf (getf (workspace-plist workspace) key) value))

(defun workspace-language-id (workspace)
  (spec-language-id (workspace-spec workspace)))

(defun get-workspace-from-point (point)
  (buffer-workspace (point-buffer point)))

(defun dispose-workspace (workspace)
  (client:dispose (workspace-client workspace)))

(defun set-trigger-characters (workspace)
  (dolist (character (get-completion-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          #'completion-with-trigger-character))
  (dolist (character (get-signature-help-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          #'lsp-signature-help-with-trigger-character)))

;;;
(defvar *workspace-list-per-language-id* (make-hash-table :test 'equal))

(defstruct workspace-list
  current-workspace
  workspaces)

(defun add-workspace (workspace)
  (if-let (workspace-list (gethash (workspace-language-id workspace)
                                   *workspace-list-per-language-id*))
    (progn
      (setf (workspace-list-current-workspace workspace-list)
            workspace)
      (push workspace (workspace-list-workspaces workspace-list)))
    (setf (gethash (workspace-language-id workspace)
                   *workspace-list-per-language-id*)
          (make-workspace-list :current-workspace workspace
                               :workspaces (list workspace)))))

(defun change-workspace (workspace)
  (let ((workspace-list (gethash (workspace-language-id workspace)
                                 *workspace-list-per-language-id*)))
    (assert workspace-list)
    (setf (workspace-list-current-workspace workspace-list) workspace)))

(defun find-workspace (language-id &key (errorp t))
  (if-let (workspace-list (gethash language-id *workspace-list-per-language-id*))
    (workspace-list-current-workspace workspace-list)
    (when errorp
      (error "The ~A workspace is not found." language-id))))

(defun buffer-workspace (buffer &optional (errorp t))
  (find-workspace (buffer-language-id buffer) :errorp errorp))

(defun all-workspaces ()
  (loop :for workspace-list :being :each :hash-value :in *workspace-list-per-language-id*
        :append (workspace-list-workspaces workspace-list)))

(defun dispose-all-workspaces ()
  (dolist (workspace (all-workspaces))
    (dispose-workspace workspace)))

;;;
(defvar *lsp-mode-keymap* (make-keymap))

(define-key *lsp-mode-keymap* "C-c h" 'lsp-hover)

(define-minor-mode lsp-mode
    (:name "LSP"
     :keymap *lsp-mode-keymap*
     :enable-hook 'enable-hook)
  (setf (variable-value 'language-mode:completion-spec)
        (lem/completion-mode:make-completion-spec 'text-document/completion :async t))
  (setf (variable-value 'language-mode:find-definitions-function)
        #'lsp-find-definitions)
  (setf (variable-value 'language-mode:find-references-function)
        #'lsp-find-references)
  (setf (buffer-value (current-buffer) 'revert-buffer-function)
        #'lsp-revert-buffer))

(defun enable-hook ()
  (let ((buffer (current-buffer)))
    (unless (buffer-temporary-p buffer)
      (handler-case
          (progn
            (add-hook *exit-editor-hook* 'dispose-all-workspaces)
            (ensure-lsp-buffer buffer
                               :then (lambda ()
                                       (text-document/did-open buffer)
                                       (enable-document-highlight-idle-timer))))
        (editor-error (c)
          (show-message (princ-to-string c)))))))

(defun reopen-buffer (buffer)
  (text-document/did-close buffer)
  (text-document/did-open buffer))

(define-command lsp-sync-buffer () ()
  (reopen-buffer (current-buffer)))

(defun lsp-revert-buffer (buffer)
  (remove-hook (variable-value 'before-change-functions :buffer buffer) 'handle-change-buffer)
  (unwind-protect (progn
                    (sync-buffer-with-file-content buffer)
                    (reopen-buffer buffer))
    (add-hook (variable-value 'before-change-functions :buffer buffer) 'handle-change-buffer)))

(defun convert-to-characters (string-characters)
  (map 'list
       (lambda (string) (char string 0))
       string-characters))

(defun get-completion-trigger-characters (workspace)
  (convert-to-characters
   (handler-case
       (lsp:completion-options-trigger-characters
        (lsp:server-capabilities-completion-provider
         (workspace-server-capabilities workspace)))
     (unbound-slot ()
       nil))))

(defun get-signature-help-trigger-characters (workspace)
  (convert-to-characters
   (handler-case
       (lsp:signature-help-options-trigger-characters
        (lsp:server-capabilities-signature-help-provider
         (workspace-server-capabilities workspace)))
     (unbound-slot ()
       nil))))

(defun self-insert-hook (c)
  (when-let* ((workspace (buffer-workspace (current-buffer)))
              (command (gethash c (workspace-trigger-characters workspace))))
    (funcall command c)))

(defun buffer-change-event-to-content-change-event (point arg)
  (labels ((inserting-content-change-event (string)
             (let ((position (point-to-lsp-position point)))
               (make-lsp-map :range (make-instance 'lsp:range
                                                   :start position
                                                   :end position)
                             :range-length 0
                             :text string)))
           (deleting-content-change-event (count)
             (with-point ((end point))
               (character-offset end count)
               (make-lsp-map :range (points-to-lsp-range
                                     point
                                     end)
                             :range-length (count-characters point end)
                             :text ""))))
    (etypecase arg
      (character
       (inserting-content-change-event (string arg)))
      (string
       (inserting-content-change-event arg))
      (integer
       (deleting-content-change-event arg)))))

(defun handle-change-buffer (point arg)
  (let ((buffer (point-buffer point))
        (change-event (buffer-change-event-to-content-change-event point arg)))
    (text-document/did-change buffer (make-lsp-array change-event))))

(defun add-buffer-hooks (buffer)
  (add-hook (variable-value 'kill-buffer-hook :buffer buffer) 'text-document/did-close)
  (add-hook (variable-value 'after-save-hook :buffer buffer) 'text-document/did-save)
  (add-hook (variable-value 'before-change-functions :buffer buffer) 'handle-change-buffer)
  (add-hook (variable-value 'self-insert-after-hook :buffer buffer) 'self-insert-hook))

(defun register-lsp-method (workspace method function)
  (jsonrpc:expose (lem-language-client/client:client-connection (workspace-client workspace))
                  method
                  function))

(defun initialize-workspace (workspace continuation)
  (register-lsp-method workspace
                       "textDocument/publishDiagnostics"
                       'text-document/publish-diagnostics)
  (register-lsp-method workspace
                       "window/showMessage"
                       'window/show-message)
  (register-lsp-method workspace
                       "window/logMessage"
                       'window/log-message)
  (initialize workspace
              (lambda ()
                (initialized workspace)
                (funcall continuation workspace))))

(defun connect (client continuation)
  (bt2:make-thread
   (lambda ()
     (loop :with condition := nil
           :repeat 20
           :do (handler-case (with-yason-bindings ()
                               (lem-language-client/client:jsonrpc-connect client))
                 (:no-error (&rest values)
                   (declare (ignore values))
                   (send-event continuation)
                   (return))
                 (error (c)
                   (setq condition c)
                   (sleep 0.5)))
           :finally (send-event
                     (lambda ()
                       (editor-error
                        "Could not establish a connection with the Language Server (condition: ~A)"
                        condition)))))))

(defgeneric initialized-workspace (mode workspace)
  (:method (mode workspace)))

(defun connect-and-initialize (workspace buffer continuation)
  (let ((spinner (spinner:start-loading-spinner
                  :modeline
                  :loading-message "initializing"
                  :buffer buffer)))
    (connect (workspace-client workspace)
             (lambda ()
               (initialize-workspace
                workspace
                (lambda (workspace)
                  (add-workspace workspace)
                  (set-trigger-characters workspace)
                  (add-buffer-hooks buffer)
                  (when continuation (funcall continuation))
                  (let ((mode (ensure-mode-object
                               (spec-mode
                                (workspace-spec workspace)))))
                    (initialized-workspace mode workspace))
                  (spinner:stop-loading-spinner spinner)
                  (redraw-display)))))))

(defun ensure-lsp-buffer (buffer &key ((:then continuation)))
  (let ((spec (buffer-language-spec buffer)))
    (if-let ((workspace (find-workspace (spec-language-id spec) :errorp nil)))
      (progn
        (add-buffer-hooks buffer)
        (when continuation (funcall continuation)))
      (let ((client (run-server spec)))
        (connect-and-initialize (make-workspace :spec spec
                                                :client client
                                                :buffer buffer)
                                buffer
                                continuation)))))

(defun check-connection ()
  (assert (buffer-language-spec (current-buffer))))

(defun buffer-to-text-document-item (buffer)
  (make-instance 'lsp:text-document-item
                 :uri (buffer-uri buffer)
                 :language-id (buffer-language-id buffer)
                 :version (buffer-version buffer)
                 :text (buffer-text buffer)))

(defun make-text-document-identifier (buffer)
  (make-instance 'lsp:text-document-identifier
                 :uri (buffer-uri buffer)))

(defun make-text-document-position-arguments (point)
  (list :text-document (make-text-document-identifier (point-buffer point))
        :position (point-to-lsp-position point)))

(defun make-text-document-position-params (point)
  (apply #'make-instance
         'lsp:text-document-position-params
         (make-text-document-position-arguments point)))

(defun find-buffer-from-uri (uri)
  (find uri (buffer-list) :key #'buffer-uri :test #'equal))

(defun get-buffer-from-text-document-identifier (text-document-identifier)
  (let ((uri (lsp:text-document-identifier-uri text-document-identifier)))
    (find-buffer-from-uri uri)))

(defun apply-text-edits (buffer text-edits)
  (flet ((replace-points ()
           (let ((points '()))
             (with-point ((start (buffer-point buffer) :left-inserting)
                          (end (buffer-point buffer) :left-inserting))
               (do-sequence (text-edit text-edits)
                 (let ((range (lsp:text-edit-range text-edit))
                       (new-text (lsp:text-edit-new-text text-edit)))
                   (move-to-lsp-position start (lsp:range-start range))
                   (move-to-lsp-position end (lsp:range-end range))
                   (push (list (copy-point start)
                               (copy-point end)
                               new-text)
                         points))))
             (nreverse points))))
    (let ((points (replace-points)))
      (unwind-protect
           (loop :for (start end text) :in points
                 :do (delete-between-points start end)
                     (insert-string start text))
        (loop :for (start end) :in points
              :do (delete-point start)
                  (delete-point end))))))

(defgeneric apply-document-change (document-change))

(defmethod apply-document-change ((document-change lsp:text-document-edit))
  (let* ((buffer
           (get-buffer-from-text-document-identifier
            (lsp:text-document-edit-text-document document-change))))
    (apply-text-edits buffer (lsp:text-document-edit-edits document-change))))

(defmethod apply-document-change ((document-change lsp:create-file))
  (error "createFile is not yet supported"))

(defmethod apply-document-change ((document-change lsp:rename-file))
  (error "renameFile is not yet supported"))

(defmethod apply-document-change ((document-change lsp:delete-file))
  (error "deleteFile is not yet supported"))

(defun apply-change (uri text-edits)
  (let ((buffer (find-buffer-from-uri uri)))
    (apply-text-edits buffer text-edits)))

(defun apply-workspace-edit (workspace-edit)
  (labels ((apply-document-changes (document-changes)
             (do-sequence (document-change document-changes)
               (apply-document-change document-change)))
           (apply-changes (changes)
             (maphash #'apply-change changes)))
    (if-let ((document-changes (handler-case
                                   (lsp:workspace-edit-document-changes workspace-edit)
                                 (unbound-slot () nil))))
      (apply-document-changes document-changes)
      (when-let ((changes (handler-case (lsp:workspace-edit-changes workspace-edit)
                            (unbound-slot () nil))))
        (apply-changes changes)))))

;;; General Messages

(defgeneric spec-initialization-options (spec)
  (:method (spec) nil))

(defparameter *client-capabilities-text*
  (load-time-value
   (uiop:read-file-string
    (asdf:system-relative-pathname :lem-lsp-mode
                                   "client-capabilities.json"))))

(defun client-capabilities ()
  (convert-from-json
   (parse-json *client-capabilities-text*)
   'lsp:client-capabilities))

(defun initialize (workspace continuation)
  (async-request
   (workspace-client workspace)
   (make-instance 'lsp:initialize)
   (apply #'make-instance
          'lsp:initialize-params
          :process-id (get-pid)
          :client-info (make-lsp-map :name "lem" #|:version "0.0.0"|#)
          :root-uri (workspace-root-uri workspace)
          :capabilities (client-capabilities)
          :trace "off"
          :workspace-folders +null+
          (when-let ((value (spec-initialization-options (workspace-spec workspace))))
            (list :initialization-options value)))
   :then (lambda (initialize-result)
           (setf (workspace-server-capabilities workspace)
                 (lsp:initialize-result-capabilities initialize-result))
           (handler-case (lsp:initialize-result-server-info initialize-result)
             (unbound-slot () nil)
             (:no-error (server-info)
               (setf (workspace-server-info workspace)
                     server-info)))
           (funcall continuation))))

(defun initialized (workspace)
  (request:request (workspace-client workspace)
                   (make-instance 'lsp:initialized)
                   (make-instance 'lsp:initialized-params)))

;;; Window

;; TODO
;; - window/showMessageRequest
;; - window/workDoneProgress/create
;; - window/workDoenProgress/cancel

(defun window/show-message (params)
  (request::do-request-log "window/showMessage" params :from :server)
  (let* ((params (convert-from-json params 'lsp:show-message-params))
         (text (format nil "~A: ~A"
                       (switch ((lsp:show-message-params-type params) :test #'=)
                         (lsp:message-type-error
                          "Error")
                         (lsp:message-type-warning
                          "Warning")
                         (lsp:message-type-info
                          "Info")
                         (lsp:message-type-log
                          "Log"))
                       (lsp:show-message-params-message params))))
    (send-event (lambda ()
                  (display-popup-message text
                                         :style '(:gravity :top)
                                         :timeout 3)))))

(defun log-message (text)
  (let ((buffer (make-buffer "*lsp output*")))
    (with-point ((point (buffer-point buffer) :left-inserting))
      (buffer-end point)
      (unless (start-line-p point)
        (insert-character point #\newline))
      (insert-string point text))
    (when (get-buffer-windows buffer)
      (redraw-display))))

(defun window/log-message (params)
  (request::do-request-log "window/logMessage" params :from :server)
  (let* ((params (convert-from-json params 'lsp:log-message-params))
         (text (lsp:log-message-params-message params)))
    (send-event (lambda ()
                  (log-message text)))))

;;; Text Synchronization

(defun text-document/did-open (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'lsp:text-document/did-open)
   (make-instance 'lsp:did-open-text-document-params
                  :text-document (buffer-to-text-document-item buffer))))

(defun text-document/did-change (buffer content-changes)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance
    'lsp:text-document/did-change)
   (make-instance 'lsp:did-change-text-document-params
                  :text-document (make-instance 'lsp:versioned-text-document-identifier
                                                :version (buffer-version buffer)
                                                :uri (buffer-uri buffer))
                  :content-changes content-changes)))

(defun provide-did-save-text-document-p (workspace)
  (let ((sync (lsp:server-capabilities-text-document-sync
               (workspace-server-capabilities workspace))))
    (etypecase sync
      (number
       (member sync
               (list lsp:text-document-sync-kind-full
                     lsp:text-document-sync-kind-incremental)))
      (lsp:text-document-sync-options
       (handler-case (lsp:text-document-sync-options-save sync)
         (unbound-slot ()
           nil))))))

(defun text-document/did-save (buffer)
  (when (provide-did-save-text-document-p (buffer-workspace buffer))
    (request:request
     (workspace-client (buffer-workspace buffer))
     (make-instance 'lsp:text-document/did-save)
     (make-instance 'lsp:did-save-text-document-params
                    :text-document (make-text-document-identifier buffer)
                    :text (buffer-text buffer)))))

(defun text-document/did-close (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'lsp:text-document/did-close)
   (make-instance 'lsp:did-close-text-document-params
                  :text-document (make-text-document-identifier buffer))))

;;; publishDiagnostics

;; TODO
;; - tagSupport
;; - versionSupport

(define-attribute diagnostic-error-attribute
  (t :foreground :base08 :underline t))

(define-attribute diagnostic-warning-attribute
  (t :foreground :base09 :underline t))

(define-attribute diagnostic-information-attribute
  (t :foreground :base04 :underline t))

(define-attribute diagnostic-hint-attribute
  (t :foreground :base0A :underline t))

(defun diagnostic-severity-attribute (diagnostic-severity)
  (switch (diagnostic-severity :test #'=)
    (lsp:diagnostic-severity-error
     'diagnostic-error-attribute)
    (lsp:diagnostic-severity-warning
     'diagnostic-warning-attribute)
    (lsp:diagnostic-severity-information
     'diagnostic-information-attribute)
    (lsp:diagnostic-severity-hint
     'diagnostic-hint-attribute)))

(defstruct diagnostic
  buffer
  position
  message)

(defun buffer-diagnostic-overlays (buffer)
  (buffer-value buffer 'diagnostic-overlays))

(defun (setf buffer-diagnostic-overlays) (overlays buffer)
  (setf (buffer-value buffer 'diagnostic-overlays) overlays))

(defun clear-diagnostic-overlays (buffer)
  (mapc #'delete-overlay (buffer-diagnostic-overlays buffer))
  (setf (buffer-diagnostic-overlays buffer) '()))

(defun buffer-diagnostic-idle-timer (buffer)
  (buffer-value buffer 'diagnostic-idle-timer))

(defun (setf buffer-diagnostic-idle-timer) (idle-timer buffer)
  (setf (buffer-value buffer 'diagnostic-idle-timer) idle-timer))

(defun overlay-diagnostic (overlay)
  (overlay-get overlay 'diagnostic))

(defun buffer-diagnostics (buffer)
  (mapcar #'overlay-diagnostic (buffer-diagnostic-overlays buffer)))

(defun reset-buffer-diagnostic (buffer)
  (clear-diagnostic-overlays buffer)
  (when-let (timer (buffer-diagnostic-idle-timer buffer))
    (stop-timer timer)
    (setf (buffer-diagnostic-idle-timer buffer) nil)))

(defun point-to-xref-position (point)
  (language-mode::make-xref-position :line-number (line-number-at-point point)
                                     :charpos (point-charpos point)))

(defun highlight-diagnostic (buffer diagnostic)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (let ((range (lsp:diagnostic-range diagnostic)))
      (move-to-lsp-position start (lsp:range-start range))
      (move-to-lsp-position end (lsp:range-end range))
      (when (point= start end)
        ;; XXX: gopls用
        ;; func main() {
        ;;     fmt.
        ;; というコードでrange.start, range.endが行末を
        ;; 差していてハイライトされないので一文字ずらしておく
        (if (end-line-p end)
            (character-offset start -1)
            (character-offset end 1)))
      (let ((overlay (make-overlay start end
                                   (handler-case (lsp:diagnostic-severity diagnostic)
                                     (unbound-slot ()
                                       'diagnostic-error-attribute)
                                     (:no-error (severity)
                                       (diagnostic-severity-attribute severity)))
                                   :end-point-kind :right-inserting)))
        (overlay-put overlay
                     'diagnostic
                     (make-diagnostic :buffer buffer
                                      :position (point-to-xref-position start)
                                      :message (lsp:diagnostic-message diagnostic)))
        (push overlay (buffer-diagnostic-overlays buffer))))))

(defun highlight-diagnostics (params)
  (when-let ((buffer (find-buffer-from-uri (lsp:publish-diagnostics-params-uri params))))
    (reset-buffer-diagnostic buffer)
    (do-sequence (diagnostic (lsp:publish-diagnostics-params-diagnostics params))
      (highlight-diagnostic buffer diagnostic))
    (setf (buffer-diagnostic-idle-timer buffer)
          (start-timer (make-idle-timer 'popup-diagnostic :name "lsp-diagnostic")
                       200
                       :repeat t))))

(defun popup-diagnostic ()
  (dolist (overlay (buffer-diagnostic-overlays (current-buffer)))
    (when (point<= (overlay-start overlay)
                   (current-point)
                   (overlay-end overlay))
      (unless (mode-active-p (current-buffer) 'lem/completion-mode:completion-mode)
        (display-message (diagnostic-message (overlay-diagnostic overlay))))
      (return))))

(defun text-document/publish-diagnostics (params)
  (request::do-request-log "textDocument/publishDiagnostics" params :from :server)
  (let ((params (convert-from-json params 'lsp:publish-diagnostics-params)))
    (send-event (lambda ()
                  (highlight-diagnostics params)))))

(define-command lsp-document-diagnostics () ()
  (when-let ((diagnostics (buffer-diagnostics (current-buffer))))
    (lem/peek-source:with-collecting-sources (collector)
      (dolist (diagnostic diagnostics)
        (lem/peek-source:with-appending-source
            (point :move-function (let ((diagnostic diagnostic))
                                    (lambda ()
                                      (language-mode:move-to-xref-location-position
                                       (buffer-point (diagnostic-buffer diagnostic))
                                       (diagnostic-position diagnostic)))))
          (insert-string point (buffer-filename (diagnostic-buffer diagnostic))
                         :attribute 'lem/peek-source:filename-attribute)
          (insert-string point ":")
          (insert-string point
                         (princ-to-string (language-mode::xref-position-line-number
                                           (diagnostic-position diagnostic)))
                         :attribute 'lem/peek-source:position-attribute)
          (insert-string point ":")
          (insert-string point
                         (princ-to-string (language-mode::xref-position-charpos
                                           (diagnostic-position diagnostic)))
                         :attribute 'lem/peek-source:position-attribute)
          (insert-string point ":")
          (insert-string point (diagnostic-message diagnostic)))))))

;;; hover

;; TODO
;; - workDoneProgress
;; - partialResult
;; - hoverClientCapabilitiesのcontentFormatを設定する
;; - hoverのrangeを使って範囲に背景色をつける
;; - serverでサポートしているかのチェックをする

(defun contents-to-string (contents)
  (flet ((marked-string-to-string (marked-string)
           (if (stringp marked-string)
               marked-string
               (or (get-map marked-string "value")
                   ""))))
    (cond
      ;; MarkedString
      ((typep contents 'lsp:marked-string)
       (marked-string-to-string contents))
      ;; MarkedString[]
      ((lsp-array-p contents)
       (with-output-to-string (out)
         (do-sequence (content contents)
           (write-string (marked-string-to-string content)
                         out))))
      ;; MarkupContent
      ((typep contents 'lsp:markup-content)
       (lsp:markup-content-value contents))
      (t
       ""))))

(defun contents-to-markdown-buffer (contents)
  (let ((string (contents-to-string contents)))
    (unless (emptyp (string-trim '(#\space #\newline) string))
      (lem/markdown-buffer:markdown-buffer string))))

(defun provide-hover-p (workspace)
  (handler-case (lsp:server-capabilities-hover-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/hover (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-hover-p workspace)
      (let ((result
              (request:request
               (workspace-client workspace)
               (make-instance 'lsp:text-document/hover)
               (apply #'make-instance
                      'lsp:hover-params
                      (make-text-document-position-arguments point)))))
        (unless (lsp-null-p result)
          (contents-to-markdown-buffer (lsp:hover-contents result)))))))

(define-command lsp-hover () ()
  (check-connection)
  (when-let ((result (text-document/hover (current-point))))
    (display-message result)))

;;; completion

;; TODO
;; - serverでサポートしているかのチェックをする
;; - workDoneProgress
;; - partialResult
;; - completionParams.context, どのように補完が起動されたかの情報を含める
;; - completionItemの使っていない要素が多分にある
;; - completionResolve

(defclass completion-item (completion:completion-item)
  ((sort-text
    :initarg :sort-text
    :reader completion-item-sort-text)))

(defun convert-to-range (point range)
  (let ((range-start (lsp:range-start range))
        (range-end (lsp:range-end range)))
    (with-point ((start point)
                 (end point))
      (move-to-lsp-position start range-start)
      (move-to-lsp-position end range-end)
      (list start end))))

(defun convert-completion-items (point items)
  (labels ((sort-items (items)
             (sort items #'string< :key #'completion-item-sort-text))
           (label-and-points (item)
             (let ((text-edit
                     (handler-case (lsp:completion-item-text-edit item)
                       (unbound-slot () nil))))
               (if text-edit
                   (cons (lsp:text-edit-new-text text-edit)
                         (convert-to-range point (lsp:text-edit-range text-edit)))
                   (list (lsp:completion-item-label item) nil nil))))
           (make-completion-item (item)
             (destructuring-bind (label start end)
                 (label-and-points item)
               (declare (ignore end))
               (make-instance
                'completion-item
                :start start
                ;; 補完候補を表示した後に文字を入力し, 候補選択をするとendがずれるので使えない
                ;; :end end
                :label label
                :detail (handler-case (lsp:completion-item-detail item)
                          (unbound-slot () ""))
                :sort-text (handler-case (lsp:completion-item-sort-text item)
                             (unbound-slot ()
                               (lsp:completion-item-label item)))
                :focus-action (when-let* ((documentation
                                           (handler-case (lsp:completion-item-documentation item)
                                             (unbound-slot () nil)))
                                          (result
                                           (contents-to-markdown-buffer documentation)))
                                (lambda (context)
                                  (display-message
                                   result
                                   :style `(:gravity :vertically-adjacent-window
                                            :offset-y -1
                                            :offset-x 1)
                                   :source-window (lem/popup-menu::popup-menu-window
                                                   (lem/completion-mode::context-popup-menu
                                                    context)))))))))
    (sort-items
     (map 'list
          #'make-completion-item
          items))))

(defun convert-completion-list (point completion-list)
  (convert-completion-items point (lsp:completion-list-items completion-list)))

(defun convert-completion-response (point value)
  (cond ((typep value 'lsp:completion-list)
         (convert-completion-list point value))
        ((lsp-array-p value)
         (convert-completion-items point value))
        (t
         nil)))

(defun provide-completion-p (workspace)
  (handler-case (lsp:server-capabilities-completion-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/completion (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-completion-p workspace)
      (async-request
       (workspace-client workspace)
       (make-instance 'lsp:text-document/completion)
       (apply #'make-instance
              'lsp:completion-params
              (make-text-document-position-arguments point))
       :then (lambda (response)
               (funcall then (convert-completion-response point response)))))))

(defun completion-with-trigger-character (c)
  (declare (ignore c))
  (check-connection)
  (language-mode::complete-symbol))

;;; signatureHelp

(define-attribute signature-help-active-parameter-attribute
  (t :background :base0D :underline t))

(defun provide-signature-help-p (workspace)
  (handler-case (lsp:server-capabilities-signature-help-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun insert-markdown (point markdown-text)
  (insert-buffer point (lem/markdown-buffer:markdown-buffer markdown-text)))

(defun insert-markup-content (point markup-content)
  (switch ((lsp:markup-content-kind markup-content) :test #'equal)
    ("markdown"
     (insert-markdown point (lsp:markup-content-value markup-content)))
    ("plaintext"
     (insert-string point (lsp:markup-content-value markup-content)))
    (otherwise
     (insert-string point (lsp:markup-content-value markup-content)))))

(defun insert-documentation (point documentation)
  (insert-character point #\newline)
  (etypecase documentation
    (lsp:markup-content
     (insert-markup-content point documentation))
    (string
     (insert-string point documentation))))

(defun highlight-signature-active-parameter (point parameters active-parameter)
  (with-point ((point point))
    (buffer-start point)
    (do-sequence ((parameter index) parameters)
      (let ((label (lsp:parameter-information-label parameter)))
        ;; TODO: labelの型が[number, number]の場合に対応する
        (when (stringp label)
          (search-forward point label)
          (when (= active-parameter index)
            (with-point ((start point))
              (character-offset start (- (length label)))
              (put-text-property start
                                 point
                                 :attribute 'signature-help-active-parameter-attribute)
              (return-from highlight-signature-active-parameter))))))))

(defun highlight-signature (point signature active-parameter)
  (let ((parameters
          (handler-case (lsp:signature-information-parameters signature)
            (unbound-slot () nil)))
        (active-parameter
          (handler-case (lsp:signature-information-active-parameter signature)
            (unbound-slot () active-parameter))))
    (when (and (plusp (length parameters))
               (< active-parameter (length parameters)))
      (highlight-signature-active-parameter point
                                            parameters
                                            active-parameter))))

(defun make-signature-help-buffer (signature-help)
  (let ((buffer (make-temporary-unwrap-buffer))
        (active-parameter
          (handler-case (lsp:signature-help-active-parameter signature-help)
            (unbound-slot () 0)))
        (active-signature
          (handler-case (lsp:signature-help-active-signature signature-help)
            (unbound-slot () nil)))
        (signatures (lsp:signature-help-signatures signature-help)))
    (do-sequence ((signature index) signatures)
      (let ((point (buffer-point buffer)))
        (when (plusp index) (insert-character point #\newline))
        (insert-string point (lsp:signature-information-label signature))
        (when (or (eql index active-signature)
                  (length= 1 signatures))
          (highlight-signature point signature active-parameter))
        (insert-character point #\newline)
        (handler-case (lsp:signature-information-documentation signature)
          (unbound-slot () nil)
          (:no-error (documentation)
            (insert-documentation point documentation)))))
    (buffer-start (buffer-point buffer))
    buffer))

(defun display-signature-help (signature-help)
  (let ((buffer (make-signature-help-buffer signature-help)))
    (display-message buffer)))

(defun text-document/signature-help (point &optional signature-help-context)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-signature-help-p workspace)
      (async-request (workspace-client workspace)
                     (make-instance 'lsp:text-document/signature-help)
                     (apply #'make-instance
                            'lsp:signature-help-params
                            (append (when signature-help-context
                                      `(:context ,signature-help-context))
                                    (make-text-document-position-arguments point)))
                     :then (lambda (result)
                             (unless (lsp-null-p result)
                               (display-signature-help result)
                               (redraw-display)))))))

(defun lsp-signature-help-with-trigger-character (character)
  (text-document/signature-help
   (current-point)
   (make-instance 'lsp:signature-help-context
                  :trigger-kind lsp:signature-help-trigger-kind-trigger-character
                  :trigger-character (string character)
                  :is-retrigger +false+
                  #|:active-signature-help|#)))

(define-command lsp-signature-help () ()
  (check-connection)
  (text-document/signature-help (current-point)
                                (make-instance 'lsp:signature-help-context
                                               :trigger-kind lsp:signature-help-trigger-kind-invoked
                                               :is-retrigger +false+)))

;;; declaration

(defun provide-declaration-p (workspace)
  (handler-case (lsp:server-capabilities-declaration-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/declaration (point)
  (declare (ignore point))
  ;; TODO: goplsが対応していなかったので後回し
  nil)

;;; definition

(defun provide-definition-p (workspace)
  (handler-case (lsp:server-capabilities-definition-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun definition-location-to-content (file location)
  (when-let* ((buffer (find-file-buffer file))
              (point (buffer-point buffer))
              (range (lsp:location-range location)))
    (with-point ((start point)
                 (end point))
      (move-to-lsp-position start (lsp:range-start range))
      (move-to-lsp-position end (lsp:range-end range))
      (line-start start)
      (line-end end)
      (points-to-string start end))))

(defgeneric convert-location (location)
  (:method ((location lsp:location))
    ;; TODO: end-positionも使い、定義位置への移動後のハイライトをstart/endの範囲にする
    (let* ((start-position (lsp:range-start (lsp:location-range location)))
           (end-position (lsp:range-end (lsp:location-range location)))
           (uri (lsp:location-uri location))
           (file (uri-to-pathname uri)))
      (declare (ignore end-position))
      (when (uiop:file-exists-p file)
        (language-mode:make-xref-location
         :filespec file
         :position (language-mode::make-position
                    (1+ (lsp:position-line start-position))
                    (lsp:position-character start-position))
         :content (definition-location-to-content file location)))))
  (:method ((location lsp:location-link))
    (error "locationLink is unsupported")))

(defun convert-definition-response (value)
  (remove nil
          (cond ((typep value 'lsp:location)
                 (list (convert-location value)))
                ((lsp-array-p value)
                 ;; TODO: location-link
                 (map 'list #'convert-location value))
                (t
                 nil))))

(defun text-document/definition (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-definition-p workspace)
      (async-request
       (workspace-client workspace)
       (make-instance 'lsp:text-document/definition)
       (apply #'make-instance
              'lsp:definition-params
              (make-text-document-position-arguments point))
       :then (lambda (response)
               (funcall then (convert-definition-response response))
               (redraw-display))))))

(defun lsp-find-definitions (point)
  (check-connection)
  (text-document/definition point #'language-mode:display-xref-locations))

;;; type definition

(defun provide-type-definition-p (workspace)
  (handler-case (lsp:server-capabilities-type-definition-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-type-definition-response (value)
  (convert-definition-response value))

(defun text-document/type-definition (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-type-definition-p workspace)
      (async-request (workspace-client workspace)
                     (make-instance 'lsp:text-document/type-definition)
                     (apply #'make-instance
                            'lsp:type-definition-params
                            (make-text-document-position-arguments point))
                     :then (lambda (response)
                             (funcall then (convert-type-definition-response response)))))))

(define-command lsp-type-definition () ()
  (check-connection)
  (text-document/type-definition (current-point) #'language-mode:display-xref-locations))

;;; implementation

(defun provide-implementation-p (workspace)
  (handler-case (lsp:server-capabilities-implementation-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-implementation-response (value)
  (convert-definition-response value))

(defun text-document/implementation (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-implementation-p workspace)
      (async-request (workspace-client workspace)
                     (make-instance 'lsp:text-document/implementation)
                     (apply #'make-instance
                            'lsp:type-definition-params
                            (make-text-document-position-arguments point))
                     :then (lambda (response)
                             (funcall then (convert-implementation-response response)))))))

(define-command lsp-implementation () ()
  (check-connection)
  (text-document/implementation (current-point)
                                #'language-mode:display-xref-locations))

;;; references

(defun provide-references-p (workspace)
  (handler-case (lsp:server-capabilities-references-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun xref-location-to-content (location)
  (when-let*
      ((buffer (find-file-buffer (language-mode:xref-location-filespec location) :temporary t))
       (point (buffer-point buffer)))
    (language-mode::move-to-location-position
     point
     (language-mode:xref-location-position location))
    (string-trim '(#\space #\tab) (line-string point))))

(defun convert-references-response (value)
  (language-mode:make-xref-references
   :type nil
   :locations (mapcar (lambda (location)
                        (language-mode:make-xref-location
                         :filespec (language-mode:xref-location-filespec location)
                         :position (language-mode:xref-location-position location)
                         :content (xref-location-to-content location)))
                      (convert-definition-response value))))

(defun text-document/references (point then &optional include-declaration)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-references-p workspace)
      (async-request
       (workspace-client workspace)
       (make-instance 'lsp:text-document/references)
       (apply #'make-instance
              'lsp:reference-params
              :context (make-instance 'lsp:reference-context
                                      :include-declaration include-declaration)
              (make-text-document-position-arguments point))
       :then (lambda (response)
               (funcall then (convert-references-response response))
               (redraw-display))))))

(defun lsp-find-references (point)
  (check-connection)
  (text-document/references point
                            #'language-mode:display-xref-references))

;;; document highlights

(define-attribute document-highlight-text-attribute
  (t :background :base02))

(defun provide-document-highlight-p (workspace)
  (handler-case (lsp:server-capabilities-document-highlight-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defstruct document-highlight-context
  (overlays '())
  (last-modified-tick 0))

(defvar *document-highlight-context* (make-document-highlight-context))

(defun document-highlight-overlays ()
  (document-highlight-context-overlays *document-highlight-context*))

(defun (setf document-highlight-overlays) (value)
  (setf (document-highlight-context-overlays *document-highlight-context*)
        value))

(defun cursor-in-document-highlight-p ()
  (dolist (ov (document-highlight-overlays))
    (unless (eq (current-buffer) (overlay-buffer ov))
      (return nil))
    (when (point<= (overlay-start ov) (current-point) (overlay-end ov))
      (return t))))

(defun clear-document-highlight-overlays ()
  (mapc #'delete-overlay (document-highlight-overlays))
  (setf (document-highlight-overlays) '())
  (setf (document-highlight-context-last-modified-tick *document-highlight-context*)
        (buffer-modified-tick (current-buffer))))

(defun clear-document-highlight-overlays-if-required ()
  (when (or (not (cursor-in-document-highlight-p))
            (not (= (document-highlight-context-last-modified-tick *document-highlight-context*)
                    (buffer-modified-tick (current-buffer))))
            (mode-active-p (current-buffer) 'lem/isearch:isearch-mode))
    (clear-document-highlight-overlays)
    t))

(defun display-document-highlights (buffer document-highlights)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (do-sequence (document-highlight document-highlights)
      (let ((range (lsp:document-highlight-range document-highlight)))
        (move-to-lsp-position start (lsp:range-start range))
        (move-to-lsp-position end (lsp:range-end range))
        (push (make-overlay start end 'document-highlight-text-attribute)
              (document-highlight-overlays))))))

(defun text-document/document-highlight (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-document-highlight-p workspace)
      (unless (cursor-in-document-highlight-p)
        (let ((counter (command-loop-counter)))
          (async-request
           (workspace-client workspace)
           (make-instance 'lsp:text-document/document-highlight)
           (apply #'make-instance
                  'lsp:document-highlight-params
                  (make-text-document-position-arguments point))
           :then (lambda (value)
                   (unless (lsp-null-p value)
                     (when (= counter (command-loop-counter))
                       (display-document-highlights (point-buffer point)
                                                    value)
                       (redraw-display))))))))))

(defun document-highlight-calls-timer ()
  (when (mode-active-p (current-buffer) 'lsp-mode)
    (when (buffer-workspace (current-buffer) nil)
      (text-document/document-highlight (current-point)))))

(define-command lsp-document-highlight () ()
  (when (mode-active-p (current-buffer) 'lsp-mode)
    (check-connection)
    (text-document/document-highlight (current-point))))

(defvar *document-highlight-idle-timer* nil)

(defun enable-document-highlight-idle-timer ()
  (unless *document-highlight-idle-timer*
    (setf *document-highlight-idle-timer*
          (start-timer (make-idle-timer #'document-highlight-calls-timer
                                        :name "lsp-document-highlight")
                       200
                       :repeat t))))

(defmethod execute :after ((mode lsp-mode) command argument)
  (clear-document-highlight-overlays-if-required))

;;; document symbols

;; TODO
;; - position順でソートする

(define-attribute symbol-kind-file-attribute
  (t :foreground "snow1"))

(define-attribute symbol-kind-module-attribute
  (t :foreground "firebrick"))

(define-attribute symbol-kind-namespace-attribute
  (t :foreground "dark orchid"))

(define-attribute symbol-kind-package-attribute
  (t :foreground "green"))

(define-attribute symbol-kind-class-attribute
  (t :foreground "bisque2"))

(define-attribute symbol-kind-method-attribute
  (t :foreground "MediumPurple2"))

(define-attribute symbol-kind-property-attribute
  (t :foreground "MistyRose4"))

(define-attribute symbol-kind-field-attribute
  (t :foreground "azure3"))

(define-attribute symbol-kind-constructor-attribute
  (t :foreground "LightSkyBlue3"))

(define-attribute symbol-kind-enum-attribute
  (t :foreground "LightCyan4"))

(define-attribute symbol-kind-interface-attribute
  (t :foreground "gray78"))

(define-attribute symbol-kind-function-attribute
  (t :foreground "LightSkyBlue"))

(define-attribute symbol-kind-variable-attribute
  (t :foreground "LightGoldenrod"))

(define-attribute symbol-kind-constant-attribute
  (t :foreground "yellow2"))

(define-attribute symbol-kind-string-attribute
  (t :foreground "green"))

(define-attribute symbol-kind-number-attribute
  (t :foreground "yellow"))

(define-attribute symbol-kind-boolean-attribute
  (t :foreground "honeydew3"))

(define-attribute symbol-kind-array-attribute
  (t :foreground "red"))

(define-attribute symbol-kind-object-attribute
  (t :foreground "PeachPuff4"))

(define-attribute symbol-kind-key-attribute
  (t :foreground "lime green"))

(define-attribute symbol-kind-null-attribute
  (t :foreground "gray"))

(define-attribute symbol-kind-enum-membe-attribute
  (t :foreground "PaleTurquoise4"))

(define-attribute symbol-kind-struct-attribute
  (t :foreground "turquoise4"))

(define-attribute symbol-kind-event-attribute
  (t :foreground "aquamarine1"))

(define-attribute symbol-kind-operator-attribute
  (t :foreground "SeaGreen3"))

(define-attribute symbol-kind-type-attribute
  (t :foreground "moccasin"))

(defun preview-symbol-kind-colors ()
  (let* ((buffer (make-buffer "symbol-kind-colors"))
         (point (buffer-point buffer)))
    (dolist (attribute
             (list 'symbol-kind-file-attribute
                   'symbol-kind-module-attribute
                   'symbol-kind-namespace-attribute
                   'symbol-kind-package-attribute
                   'symbol-kind-class-attribute
                   'symbol-kind-method-attribute
                   'symbol-kind-property-attribute
                   'symbol-kind-field-attribute
                   'symbol-kind-constructor-attribute
                   'symbol-kind-enum-attribute
                   'symbol-kind-interface-attribute
                   'symbol-kind-function-attribute
                   'symbol-kind-variable-attribute
                   'symbol-kind-constant-attribute
                   'symbol-kind-string-attribute
                   'symbol-kind-number-attribute
                   'symbol-kind-boolean-attribute
                   'symbol-kind-array-attribute
                   'symbol-kind-object-attribute
                   'symbol-kind-key-attribute
                   'symbol-kind-null-attribute
                   'symbol-kind-enum-membe-attribute
                   'symbol-kind-struct-attribute
                   'symbol-kind-event-attribute
                   'symbol-kind-operator-attribute
                   'symbol-kind-type-attribute))
      (insert-string point (string-downcase attribute) :attribute attribute)
      (insert-character point #\newline))))

(defun provide-document-symbol-p (workspace)
  (handler-case (lsp:server-capabilities-document-symbol-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun symbol-kind-to-string-and-attribute (symbol-kind)
  (switch (symbol-kind :test #'=)
    (lsp:symbol-kind-file
     (values "File" 'symbol-kind-file-attribute))
    (lsp:symbol-kind-module
     (values "Module" 'symbol-kind-module-attribute))
    (lsp:symbol-kind-namespace
     (values "Namespace" 'symbol-kind-namespace-attribute))
    (lsp:symbol-kind-package
     (values "Package" 'symbol-kind-package-attribute))
    (lsp:symbol-kind-class
     (values "Class" 'symbol-kind-class-attribute))
    (lsp:symbol-kind-method
     (values "Method" 'symbol-kind-method-attribute))
    (lsp:symbol-kind-property
     (values "Property" 'symbol-kind-property-attribute))
    (lsp:symbol-kind-field
     (values "Field" 'symbol-kind-field-attribute))
    (lsp:symbol-kind-constructor
     (values "Constructor" 'symbol-kind-constructor-attribute))
    (lsp:symbol-kind-enum
     (values "Enum" 'symbol-kind-enum-attribute))
    (lsp:symbol-kind-interface
     (values "Interface" 'symbol-kind-interface-attribute))
    (lsp:symbol-kind-function
     (values "Function" 'symbol-kind-function-attribute))
    (lsp:symbol-kind-variable
     (values "Variable" 'symbol-kind-variable-attribute))
    (lsp:symbol-kind-constant
     (values "Constant" 'symbol-kind-constant-attribute))
    (lsp:symbol-kind-string
     (values "String" 'symbol-kind-string-attribute))
    (lsp:symbol-kind-number
     (values "Number" 'symbol-kind-number-attribute))
    (lsp:symbol-kind-boolean
     (values "Boolean" 'symbol-kind-boolean-attribute))
    (lsp:symbol-kind-array
     (values "Array" 'symbol-kind-array-attribute))
    (lsp:symbol-kind-object
     (values "Object" 'symbol-kind-object-attribute))
    (lsp:symbol-kind-key
     (values "Key" 'symbol-kind-key-attribute))
    (lsp:symbol-kind-null
     (values "Null" 'symbol-kind-null-attribute))
    (lsp:symbol-kind-enum-member
     (values "EnumMember" 'symbol-kind-enum-member-attribute))
    (lsp:symbol-kind-struct
     (values "Struct" 'symbol-kind-struct-attribute))
    (lsp:symbol-kind-event
     (values "Event" 'symbol-kind-event-attribute))
    (lsp:symbol-kind-operator
     (values "Operator" 'symbol-kind-operator-attribute))
    (lsp:symbol-kind-type-parameter
     (values "TypeParameter" 'symbol-kind-type-attribute))))

(define-attribute document-symbol-detail-attribute
  (t :foreground :base04))

(defun append-document-symbol-item (buffer document-symbol nest-level)
  (let ((selection-range (lsp:document-symbol-selection-range document-symbol))
        (range (lsp:document-symbol-range document-symbol)))
    (declare (ignore range)) ; TODO: rangeをリージョンのハイライトに使う
    (lem/peek-source:with-appending-source
        (point :move-function (lambda ()
                                (let ((point (buffer-point buffer)))
                                  (move-to-lsp-position point (lsp:range-start selection-range)))))
      (multiple-value-bind (kind-name attribute)
          (symbol-kind-to-string-and-attribute (lsp:document-symbol-kind document-symbol))
        (insert-string point (make-string (* 2 nest-level) :initial-element #\space))
        (insert-string point (format nil "[~A]" kind-name) :attribute attribute)
        (insert-character point #\space)
        (insert-string point (lsp:document-symbol-name document-symbol))
        (insert-string point " ")
        (when-let (detail (handler-case (lsp:document-symbol-detail document-symbol)
                            (unbound-slot () nil)))
          (insert-string point detail :attribute 'document-symbol-detail-attribute)))))
  (do-sequence
      (document-symbol
       (handler-case (lsp:document-symbol-children document-symbol)
         (unbound-slot () nil)))
    (append-document-symbol-item buffer document-symbol (1+ nest-level))))

(defun display-document-symbol-response (buffer value)
  (lem/peek-source:with-collecting-sources (collector)
    (do-sequence (item value)
      (append-document-symbol-item buffer item 0))))

(defun text-document/document-symbol (buffer)
  (when-let ((workspace (buffer-workspace buffer)))
    (when (provide-document-symbol-p workspace)
      (request:request
       (workspace-client workspace)
       (make-instance 'lsp:text-document/document-symbol)
       (make-instance
        'lsp:document-symbol-params
        :text-document (make-text-document-identifier buffer))))))

(define-command lsp-document-symbol () ()
  (check-connection)
  (display-document-symbol-response
   (current-buffer)
   (text-document/document-symbol (current-buffer))))

;;; code action
;; TODO
;; - codeAction.diagnostics
;; - codeAction.isPreferred

(defun provide-code-action-p (workspace)
  (handler-case (lsp:server-capabilities-code-action-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun execute-command (workspace command)
  ;; TODO
  ;; レスポンスを見てなんらかの処理をする必要がある
  ;; この機能はgoplsで使われる事が今のところないので動作テストをできていない
  (request:request
   (workspace-client workspace)
   (make-instance 'lsp:workspace/execute-command)
   (make-instance 'lsp:execute-command-params
                  :command (lsp:command-command command)
                  :arguments (lsp:command-arguments command))))

(defun execute-code-action (workspace code-action)
  (handler-case (lsp:code-action-edit code-action)
    (unbound-slot () nil)
    (:no-error (workspace-edit)
      (apply-workspace-edit workspace-edit)))
  (handler-case (lsp:code-action-command code-action)
    (unbound-slot () nil)
    (:no-error (command)
      (execute-command workspace command))))

(defun convert-code-actions (code-actions workspace)
  (let ((items '()))
    (do-sequence (command-or-code-action code-actions)
      (etypecase command-or-code-action
        (lsp:code-action
         (let ((code-action command-or-code-action))
           (push (make-instance 'context-menu:item
                                :label (lsp:code-action-title code-action)
                                :callback (lambda (window)
                                            (declare (ignore window))
                                            (execute-code-action workspace code-action)))
                 items)))
        (lsp:command
         (let ((command command-or-code-action))
           (push (make-instance 'context-menu:item
                                :label (lsp:command-title command)
                                :callback (lambda (window)
                                            (declare (ignore window))
                                            (execute-command workspace command)))
                 items)))))
    (nreverse items)))

(defun text-document/code-action (point)
  (flet ((point-to-line-range (point)
           (with-point ((start point)
                        (end point))
             (line-start start)
             (line-end end)
             (points-to-lsp-range start end))))
    (when-let ((workspace (get-workspace-from-point point)))
      (when (provide-code-action-p workspace)
        (request:request
         (workspace-client workspace)
         (make-instance 'lsp:text-document/code-action)
         (make-instance
          'lsp:code-action-params
          :text-document (make-text-document-identifier (point-buffer point))
          :range (point-to-line-range point)
          :context (make-instance 'lsp:code-action-context
                                  :diagnostics (make-lsp-array))))))))

(define-command lsp-code-action () ()
  (check-connection)
  (let ((response (text-document/code-action (current-point)))
        (workspace (buffer-workspace (current-buffer))))
    (cond ((typep response 'lsp:command)
           (execute-command workspace response))
          ((and (lsp-array-p response)
                (not (length= response 0)))
           (context-menu:display-context-menu
            (convert-code-actions response
                                  workspace)))
          (t
           (message "No suggestions from code action")))))

(defun find-organize-imports (code-actions)
  (do-sequence (code-action code-actions)
    (when (equal "source.organizeImports" (lsp:code-action-kind code-action))
      (return-from find-organize-imports code-action))))

(defun organize-imports (buffer)
  (let ((response (text-document/code-action (buffer-point buffer)))
        (workspace (buffer-workspace buffer)))
    (unless (lsp-null-p response)
      (let ((code-action (find-organize-imports response)))
        (unless (lsp-null-p code-action)
          (execute-code-action workspace code-action))))))

(define-command lsp-organize-imports () ()
  (organize-imports (current-buffer)))

;;; formatting

(defun provide-formatting-p (workspace)
  (handler-case (lsp:server-capabilities-document-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun make-formatting-options (buffer)
  (make-instance
   'lsp:formatting-options
   :tab-size (or (variable-value 'tab-width :buffer buffer) +default-tab-size+)
   :insert-spaces (not (variable-value 'indent-tabs-mode :buffer buffer))
   :trim-trailing-whitespace t
   :insert-final-newline t
   :trim-final-newlines t))

(defun text-document/formatting (buffer)
  (when-let ((workspace (buffer-workspace buffer)))
    (when (provide-formatting-p workspace)
      (apply-text-edits
       buffer
       (request:request
        (workspace-client workspace)
        (make-instance 'lsp:text-document/formatting)
        (make-instance
         'lsp:document-formatting-params
         :text-document (make-text-document-identifier buffer)
         :options (make-formatting-options buffer)))))))

(define-command lsp-document-format () ()
  (check-connection)
  (text-document/formatting (current-buffer)))

;;; range formatting

;; WARNING: goplsでサポートされていないので動作未確認

(defun provide-range-formatting-p (workspace)
  (handler-case (lsp:server-capabilities-document-range-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/range-formatting (start end)
  (when (point< end start) (rotatef start end))
  (let ((buffer (point-buffer start)))
    (when-let ((workspace (buffer-workspace buffer)))
      (when (provide-range-formatting-p workspace)
        (apply-text-edits
         buffer
         (request:request
          (workspace-client workspace)
          (make-instance 'lsp:text-document/range-formatting)
          (make-instance
           'lsp:document-range-formatting-params
           :text-document (make-text-document-identifier buffer)
           :range (points-to-lsp-range start end)
           :options (make-formatting-options buffer))))))))

(define-command lsp-document-range-format (start end) (:region)
  (check-connection)
  (text-document/range-formatting start end))

;;; onTypeFormatting

;; TODO
;; - バッファの初期化時にtext-document/on-type-formattingを呼び出すフックを追加する

(defun provide-on-type-formatting-p (workspace)
  (handler-case (lsp:server-capabilities-document-on-type-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/on-type-formatting (point typed-character)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-on-type-formatting-p workspace)
      (when-let ((response
                  (with-jsonrpc-error ()
                    (request:request
                     (workspace-client workspace)
                     (make-instance 'lsp:text-document-client-capabilities-on-type-formatting)
                     (apply #'make-instance
                            'lsp:document-on-type-formatting-params
                            :ch typed-character
                            :options (make-formatting-options (point-buffer point))
                            (make-text-document-position-arguments point))))))
        (apply-text-edits (point-buffer point) response)))))

;;; rename

;; TODO
;; - prepareSupport

(defun provide-rename-p (workspace)
  (handler-case (lsp:server-capabilities-rename-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/rename (point new-name)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-rename-p workspace)
      (when-let ((response
                  (with-jsonrpc-error ()
                    (request:request
                     (workspace-client workspace)
                     (make-instance 'lsp:text-document/rename)
                     (apply #'make-instance
                            'lsp:rename-params
                            :new-name new-name
                            (make-text-document-position-arguments point))))))
        (apply-workspace-edit response)))))

(define-command lsp-rename (new-name) ((:string "New name: "))
  (check-connection)
  (text-document/rename (current-point) new-name))

;;;
(define-command lsp-restart-server () ()
  (when-let (workspace (buffer-workspace (current-buffer) nil))
    (dispose-workspace workspace))
  ;; TODO:
  ;; 現在のバッファを開き直すだけでは不十分
  ;; buffer-listを全て見る必要がある
  (ensure-lsp-buffer (current-buffer)))

;;;
(defun enable-lsp-mode ()
  (lsp-mode t))

(defmacro define-language-spec ((spec-name major-mode) &body initargs)
  `(progn
     ,(when (mode-hook-variable major-mode)
        `(add-hook ,(mode-hook-variable major-mode) 'enable-lsp-mode))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,spec-name (lem-lsp-mode/spec::spec) ()
         (:default-initargs ,@initargs
          :mode ',major-mode)))
     (register-language-spec ',major-mode (make-instance ',spec-name))))

#|
(define-language-spec (js-spec lem-js-mode:js-mode)
  :language-id "javascript"
  :root-uri-patterns '("package.json" "tsconfig.json")
  :command '("typescript-language-server" "--stdio")
  :install-command "npm install -g typescript-language-server typescript"
  :readme-url "https://github.com/typescript-language-server/typescript-language-server"
  :connection-mode :stdio)

(define-language-spec (rust-spec lem-rust-mode:rust-mode)
  :language-id "rust"
  :root-uri-patterns '("Cargo.toml")
  :command '("rls")
  :readme-url "https://github.com/rust-lang/rls"
  :connection-mode :stdio)

(define-language-spec (sql-spec lem-sql-mode:sql-mode)
  :language-id "sql"
  :root-uri-patterns '()
  :command '("sql-language-server" "up" "--method" "stdio")
  :readme-url "https://github.com/joe-re/sql-language-server"
  :connection-mode :stdio)

(defun find-dart-bin-path ()
  (multiple-value-bind (output error-output status)
      (uiop:run-program '("which" "dart")
                        :output :string
                        :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop status)
        (namestring
         (uiop:pathname-directory-pathname
          (string-right-trim '(#\newline) output)))
        nil)))

(defun find-dart-language-server ()
  (let ((program-name "analysis_server.dart.snapshot"))
    (when-let (path (find-dart-bin-path))
      (let ((result
              (string-right-trim
               '(#\newline)
               (uiop:run-program (list "find" path "-name" program-name)
                                 :output :string))))
        (when (search program-name result)
          result)))))

(define-language-spec (dart-spec lem-dart-mode:dart-mode)
  :language-id "dart"
  :root-uri-patterns '("pubspec.yaml")
  :connection-mode :stdio)

(defmethod spec-command ((spec dart-spec))
  (if-let ((lsp-path (find-dart-language-server)))
    (list "dart" lsp-path "--lsp")
    (editor-error "dart language server not found")))

(defmethod spec-initialization-options ((spec dart-spec))
  (make-lsp-map "onlyAnalyzeProjectsWithOpenFiles" +true+
                "suggestFromUnimportedLibraries" +true+))
|#

#|
Language Features
- [X] completion
- [ ] completion resolve
- [X] hover
- [X] signatureHelp
- [ ] declaration
- [X] definition
- [X] typeDefinition
- [X] implementation
- [X] references
- [X] documentHighlight
- [X] documentSymbol
- [X] codeAction
- [ ] codeLens
- [ ] codeLens resolve
- [ ] documentLink
- [ ] documentLink resolve
- [ ] documentColor
- [ ] colorPresentation
- [X] formatting
- [X] rangeFormatting
- [X] onTypeFormatting
- [X] rename
- [ ] prepareRename
- [ ] foldingRange
- [ ] selectionRange

TODO
- partialResult
- workDoneProgress
|#
