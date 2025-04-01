(in-package :lem-language-server)

(define-request (initialize-request "initialize") (params lsp:initialize-params)
  (setf (server-client-capabilities (current-server)) params)
  (convert-to-json
   (make-instance
    'lsp:initialize-result
    :capabilities (make-instance
                   'lsp:server-capabilities
                   ;; :position-encoding
                   :text-document-sync (make-instance
                                        'lsp:text-document-sync-options
                                        :open-close t
                                        :change lsp:text-document-sync-kind-incremental
                                        :will-save nil
                                        :will-save-wait-until nil
                                        :save t)
                   ;; :notebook-document-sync
                   :completion-provider (make-instance
                                         'lsp:completion-options
                                         :trigger-characters (vector ":")
                                         :resolve-provider t
                                         :completion-item (make-lsp-map "labelDetailsSupport" t))
                   :hover-provider (make-instance 'lsp:hover-options :work-done-progress nil)
                   :signature-help-provider (make-instance 'lsp:signature-help-options
                                                           :trigger-characters (vector " ")
                                                           :retrigger-characters (vector))
                   ;; :declaration-provider
                   :definition-provider (make-instance 'lsp:definition-options
                                                       :work-done-progress nil)
                   ;; :type-definition-provider
                   ;; :implementation-provider
                   :references-provider (make-instance 'lsp:reference-options)
                   :document-highlight-provider (make-instance 'lsp:document-highlight-options)
                   :document-symbol-provider (make-instance 'lsp:document-symbol-options
                                                            ;; :label ; ** TODO **
                                                            )
                   ;; :code-action-provider
                   ;; :code-lens-provider
                   ;; :document-link-provider
                   ;; :color-provider
                   :document-formatting-provider (make-instance 'lsp:document-formatting-options)
                   :document-range-formatting-provider (make-instance
                                                        'lsp:document-range-formatting-options)
                   :document-on-type-formatting-provider (make-instance
                                                          'lsp:document-on-type-formatting-options
                                                          :first-trigger-character "("
                                                          :more-trigger-character #(")"))
                   ;; :rename-provider
                   ;; :folding-range-provider
                   :execute-command-provider (make-instance 'lsp:execute-command-options
                                                            :commands (coerce (command-names)
                                                                              'vector))
                   ;; :selection-range-provider
                   ;; :linked-editing-range-provider
                   ;; :call-hierarchy-provider
                   ;; :semantic-tokens-provider
                   ;; :moniker-provider
                   ;; :type-hierarchy-provider
                   ;; :inline-value-provider
                   ;; :inlay-hint-provider
                   ;; :diagnostic-provider
                   ;; :workspace-symbol-provider
                   ;; :workspace
                   :experimental nil)
    :server-info (make-lsp-map "name" (language-server-name)
                               "version" (language-server-version)
                               "swankPort" (swank-port (current-server))
                               "microsPort" (micros-port (current-server))))))

(define-request (initialized-request "initialized") (params lsp:initialized-params)
  (declare (ignore params))
  (values))

#+TODO
(define-request (client-register-capability-request "client/registerCapability")
    (params lsp:registration-params)
  )

#+TODO
(define-request (client-unregister-capability-request "client/unregisterCapability")
    (params lsp:unregistration-params)
  )

#+TODO
(define-request (set-trace-request "$/setTrace")
    (params lsp:set-trace-params)
  )

#+TODO
(define-request (log-trace-request "$/logTrace")
    (params lsp:log-trace-params)
  )

(define-request (shutdown-request "shutdown") ()
  (setf (server-shutdown-request-received-p (current-server)) t)
  nil)

(define-request (exit-request "exit") ()
  (exit-server (current-server)
               (if (server-shutdown-request-received-p (current-server))
                   0
                   1))
  (values))
