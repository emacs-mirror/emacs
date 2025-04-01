;;; Code generated based on '/home/user/tmp/language-server-protocol/_specifications/lsp/3.17/metaModel/metaModel.json'; DO NOT EDIT.

(common-lisp:defpackage :lem-lsp-base/protocol-3-17
  (:nicknames :lsp)
  (:use)
  (:export :/progress
           :/cancel-request
           :/log-trace
           :/set-trace
           :text-document/publish-diagnostics
           :workspace/did-change-watched-files
           :text-document/will-save
           :text-document/did-save
           :text-document/did-close
           :text-document/did-change
           :text-document/did-open
           :telemetry/event
           :window/log-message
           :window/show-message
           :workspace/did-change-configuration
           :exit
           :initialized
           :notebook-document/did-close
           :notebook-document/did-save
           :notebook-document/did-change
           :notebook-document/did-open
           :workspace/did-delete-files
           :workspace/did-rename-files
           :workspace/did-create-files
           :window/work-done-progress/cancel
           :workspace/did-change-workspace-folders
           :workspace/apply-edit
           :workspace/execute-command
           :text-document/prepare-rename
           :text-document/rename
           :text-document/on-type-formatting
           :text-document/ranges-formatting
           :text-document/range-formatting
           :text-document/formatting
           :document-link/resolve
           :text-document/document-link
           :workspace/code-lens/refresh
           :code-lens/resolve
           :text-document/code-lens
           :workspace-symbol/resolve
           :workspace/symbol
           :code-action/resolve
           :text-document/code-action
           :text-document/document-symbol
           :text-document/document-highlight
           :text-document/references
           :text-document/definition
           :text-document/signature-help
           :text-document/hover
           :completion-item/resolve
           :text-document/completion
           :text-document/will-save-wait-until
           :window/show-message-request
           :shutdown
           :initialize
           :client/unregister-capability
           :client/register-capability
           :text-document/inline-completion
           :workspace/diagnostic/refresh
           :workspace/diagnostic
           :text-document/diagnostic
           :workspace/inlay-hint/refresh
           :inlay-hint/resolve
           :text-document/inlay-hint
           :workspace/inline-value/refresh
           :text-document/inline-value
           :type-hierarchy/subtypes
           :type-hierarchy/supertypes
           :text-document/prepare-type-hierarchy
           :text-document/moniker
           :workspace/will-delete-files
           :workspace/will-rename-files
           :workspace/will-create-files
           :text-document/linked-editing-range
           :window/show-document
           :workspace/semantic-tokens/refresh
           :text-document/semantic-tokens/range
           :text-document/semantic-tokens/full/delta
           :text-document/semantic-tokens/full
           :call-hierarchy/outgoing-calls
           :call-hierarchy/incoming-calls
           :text-document/prepare-call-hierarchy
           :window/work-done-progress/create
           :text-document/selection-range
           :text-document/declaration
           :workspace/folding-range/refresh
           :text-document/folding-range
           :text-document/color-presentation
           :text-document/document-color
           :workspace/configuration
           :workspace/workspace-folders
           :text-document/type-definition
           :text-document/implementation
           :notebook-document-filter
           :text-document-filter
           :lsp-object
           :document-filter
           :marked-string
           :text-document-content-change-event
           :workspace-document-diagnostic-report
           :change-annotation-identifier
           :progress-token
           :prepare-rename-result
           :document-diagnostic-report
           :declaration-link
           :lsp-any
           :lsp-array
           :definition-link
           :allowed-tags
           :parser
           :markdown-client-capabilities
           :markdown-client-capabilities-allowed-tags
           :markdown-client-capabilities-version
           :markdown-client-capabilities-parser
           :engine
           :regular-expressions-client-capabilities
           :regular-expressions-client-capabilities-version
           :regular-expressions-client-capabilities-engine
           :support
           :show-document-client-capabilities
           :show-document-client-capabilities-support
           :show-message-request-client-capabilities
           :show-message-request-client-capabilities-message-action-item
           :execution-summary-support
           :notebook-document-sync-client-capabilities
           :notebook-document-sync-client-capabilities-execution-summary-support
           :notebook-document-sync-client-capabilities-dynamic-registration
           :inline-completion-client-capabilities
           :inline-completion-client-capabilities-dynamic-registration
           :related-document-support
           :diagnostic-client-capabilities
           :diagnostic-client-capabilities-related-document-support
           :diagnostic-client-capabilities-dynamic-registration
           :inlay-hint-client-capabilities
           :inlay-hint-client-capabilities-resolve-support
           :inlay-hint-client-capabilities-dynamic-registration
           :inline-value-client-capabilities
           :inline-value-client-capabilities-dynamic-registration
           :type-hierarchy-client-capabilities
           :type-hierarchy-client-capabilities-dynamic-registration
           :moniker-client-capabilities
           :moniker-client-capabilities-dynamic-registration
           :linked-editing-range-client-capabilities
           :linked-editing-range-client-capabilities-dynamic-registration
           :augments-syntax-tokens
           :server-cancel-support
           :multiline-token-support
           :overlapping-token-support
           :formats
           :requests
           :semantic-tokens-client-capabilities
           :semantic-tokens-client-capabilities-augments-syntax-tokens
           :semantic-tokens-client-capabilities-server-cancel-support
           :semantic-tokens-client-capabilities-multiline-token-support
           :semantic-tokens-client-capabilities-overlapping-token-support
           :semantic-tokens-client-capabilities-formats
           :semantic-tokens-client-capabilities-token-modifiers
           :semantic-tokens-client-capabilities-token-types
           :semantic-tokens-client-capabilities-requests
           :semantic-tokens-client-capabilities-dynamic-registration
           :call-hierarchy-client-capabilities
           :call-hierarchy-client-capabilities-dynamic-registration
           :code-description-support
           :version-support
           :publish-diagnostics-client-capabilities
           :publish-diagnostics-client-capabilities-data-support
           :publish-diagnostics-client-capabilities-code-description-support
           :publish-diagnostics-client-capabilities-version-support
           :publish-diagnostics-client-capabilities-tag-support
           :publish-diagnostics-client-capabilities-related-information
           :selection-range-client-capabilities
           :selection-range-client-capabilities-dynamic-registration
           :line-folding-only
           :range-limit
           :folding-range-client-capabilities
           :folding-range-client-capabilities-folding-range
           :folding-range-client-capabilities-folding-range-kind
           :folding-range-client-capabilities-line-folding-only
           :folding-range-client-capabilities-range-limit
           :folding-range-client-capabilities-dynamic-registration
           :prepare-support
           :rename-client-capabilities
           :rename-client-capabilities-honors-change-annotations
           :rename-client-capabilities-prepare-support-default-behavior
           :rename-client-capabilities-prepare-support
           :rename-client-capabilities-dynamic-registration
           :document-on-type-formatting-client-capabilities
           :document-on-type-formatting-client-capabilities-dynamic-registration
           :document-range-formatting-client-capabilities
           :document-range-formatting-client-capabilities-ranges-support
           :document-range-formatting-client-capabilities-dynamic-registration
           :document-formatting-client-capabilities
           :document-formatting-client-capabilities-dynamic-registration
           :document-color-client-capabilities
           :document-color-client-capabilities-dynamic-registration
           :tooltip-support
           :document-link-client-capabilities
           :document-link-client-capabilities-tooltip-support
           :document-link-client-capabilities-dynamic-registration
           :code-lens-client-capabilities
           :code-lens-client-capabilities-dynamic-registration
           :honors-change-annotations
           :data-support
           :disabled-support
           :is-preferred-support
           :code-action-literal-support
           :code-action-client-capabilities
           :code-action-client-capabilities-honors-change-annotations
           :code-action-client-capabilities-resolve-support
           :code-action-client-capabilities-data-support
           :code-action-client-capabilities-disabled-support
           :code-action-client-capabilities-is-preferred-support
           :code-action-client-capabilities-code-action-literal-support
           :code-action-client-capabilities-dynamic-registration
           :label-support
           :hierarchical-document-symbol-support
           :document-symbol-client-capabilities
           :document-symbol-client-capabilities-label-support
           :document-symbol-client-capabilities-tag-support
           :document-symbol-client-capabilities-hierarchical-document-symbol-support
           :document-symbol-client-capabilities-symbol-kind
           :document-symbol-client-capabilities-dynamic-registration
           :document-highlight-client-capabilities
           :document-highlight-client-capabilities-dynamic-registration
           :reference-client-capabilities
           :reference-client-capabilities-dynamic-registration
           :implementation-client-capabilities
           :implementation-client-capabilities-link-support
           :implementation-client-capabilities-dynamic-registration
           :type-definition-client-capabilities
           :type-definition-client-capabilities-link-support
           :type-definition-client-capabilities-dynamic-registration
           :definition-client-capabilities
           :definition-client-capabilities-link-support
           :definition-client-capabilities-dynamic-registration
           :link-support
           :declaration-client-capabilities
           :declaration-client-capabilities-link-support
           :declaration-client-capabilities-dynamic-registration
           :signature-help-client-capabilities
           :signature-help-client-capabilities-context-support
           :signature-help-client-capabilities-signature-information
           :signature-help-client-capabilities-dynamic-registration
           :content-format
           :hover-client-capabilities
           :hover-client-capabilities-content-format
           :hover-client-capabilities-dynamic-registration
           :context-support
           :completion-client-capabilities
           :completion-client-capabilities-completion-list
           :completion-client-capabilities-context-support
           :completion-client-capabilities-insert-text-mode
           :completion-client-capabilities-completion-item-kind
           :completion-client-capabilities-completion-item
           :completion-client-capabilities-dynamic-registration
           :did-save
           :text-document-sync-client-capabilities
           :text-document-sync-client-capabilities-did-save
           :text-document-sync-client-capabilities-will-save-wait-until
           :text-document-sync-client-capabilities-will-save
           :text-document-sync-client-capabilities-dynamic-registration
           :folding-range-workspace-client-capabilities
           :folding-range-workspace-client-capabilities-refresh-support
           :diagnostic-workspace-client-capabilities
           :diagnostic-workspace-client-capabilities-refresh-support
           :inlay-hint-workspace-client-capabilities
           :inlay-hint-workspace-client-capabilities-refresh-support
           :inline-value-workspace-client-capabilities
           :inline-value-workspace-client-capabilities-refresh-support
           :file-operation-client-capabilities
           :file-operation-client-capabilities-will-delete
           :file-operation-client-capabilities-did-delete
           :file-operation-client-capabilities-will-rename
           :file-operation-client-capabilities-did-rename
           :file-operation-client-capabilities-will-create
           :file-operation-client-capabilities-did-create
           :file-operation-client-capabilities-dynamic-registration
           :code-lens-workspace-client-capabilities
           :code-lens-workspace-client-capabilities-refresh-support
           :refresh-support
           :semantic-tokens-workspace-client-capabilities
           :semantic-tokens-workspace-client-capabilities-refresh-support
           :execute-command-client-capabilities
           :execute-command-client-capabilities-dynamic-registration
           :resolve-support
           :tag-support
           :workspace-symbol-client-capabilities
           :workspace-symbol-client-capabilities-resolve-support
           :workspace-symbol-client-capabilities-tag-support
           :workspace-symbol-client-capabilities-symbol-kind
           :workspace-symbol-client-capabilities-dynamic-registration
           :relative-pattern-support
           :did-change-watched-files-client-capabilities
           :did-change-watched-files-client-capabilities-relative-pattern-support
           :did-change-watched-files-client-capabilities-dynamic-registration
           :dynamic-registration
           :did-change-configuration-client-capabilities
           :did-change-configuration-client-capabilities-dynamic-registration
           :change-annotation-support
           :normalizes-line-endings
           :failure-handling
           :resource-operations
           :workspace-edit-client-capabilities
           :workspace-edit-client-capabilities-change-annotation-support
           :workspace-edit-client-capabilities-normalizes-line-endings
           :workspace-edit-client-capabilities-failure-handling
           :workspace-edit-client-capabilities-resource-operations
           :workspace-edit-client-capabilities-document-changes
           :base-uri
           :relative-pattern
           :relative-pattern-pattern
           :relative-pattern-base-uri
           :position-encodings
           :markdown
           :regular-expressions
           :stale-request-support
           :general-client-capabilities
           :general-client-capabilities-position-encodings
           :general-client-capabilities-markdown
           :general-client-capabilities-regular-expressions
           :general-client-capabilities-stale-request-support
           :show-document
           :show-message
           :window-client-capabilities
           :window-client-capabilities-show-document
           :window-client-capabilities-show-message
           :window-client-capabilities-work-done-progress
           :notebook-document-client-capabilities
           :notebook-document-client-capabilities-synchronization
           :inline-completion
           :type-hierarchy
           :linked-editing-range
           :call-hierarchy
           :publish-diagnostics
           :rename
           :on-type-formatting
           :range-formatting
           :formatting
           :references
           :implementation
           :type-definition
           :definition
           :declaration
           :completion
           :synchronization
           :text-document-client-capabilities
           :text-document-client-capabilities-inline-completion
           :text-document-client-capabilities-diagnostic
           :text-document-client-capabilities-inlay-hint
           :text-document-client-capabilities-inline-value
           :text-document-client-capabilities-type-hierarchy
           :text-document-client-capabilities-moniker
           :text-document-client-capabilities-linked-editing-range
           :text-document-client-capabilities-semantic-tokens
           :text-document-client-capabilities-call-hierarchy
           :text-document-client-capabilities-publish-diagnostics
           :text-document-client-capabilities-selection-range
           :text-document-client-capabilities-folding-range
           :text-document-client-capabilities-rename
           :text-document-client-capabilities-on-type-formatting
           :text-document-client-capabilities-range-formatting
           :text-document-client-capabilities-formatting
           :text-document-client-capabilities-color-provider
           :text-document-client-capabilities-document-link
           :text-document-client-capabilities-code-lens
           :text-document-client-capabilities-code-action
           :text-document-client-capabilities-document-symbol
           :text-document-client-capabilities-document-highlight
           :text-document-client-capabilities-references
           :text-document-client-capabilities-implementation
           :text-document-client-capabilities-type-definition
           :text-document-client-capabilities-definition
           :text-document-client-capabilities-declaration
           :text-document-client-capabilities-signature-help
           :text-document-client-capabilities-hover
           :text-document-client-capabilities-completion
           :text-document-client-capabilities-synchronization
           :inline-value
           :file-operations
           :configuration
           :execute-command
           :symbol
           :did-change-watched-files
           :did-change-configuration
           :apply-edit
           :workspace-client-capabilities
           :workspace-client-capabilities-folding-range
           :workspace-client-capabilities-diagnostics
           :workspace-client-capabilities-inlay-hint
           :workspace-client-capabilities-inline-value
           :workspace-client-capabilities-file-operations
           :workspace-client-capabilities-code-lens
           :workspace-client-capabilities-semantic-tokens
           :workspace-client-capabilities-configuration
           :workspace-client-capabilities-workspace-folders
           :workspace-client-capabilities-execute-command
           :workspace-client-capabilities-symbol
           :workspace-client-capabilities-did-change-watched-files
           :workspace-client-capabilities-did-change-configuration
           :workspace-client-capabilities-workspace-edit
           :workspace-client-capabilities-apply-edit
           :execution-order
           :execution-summary-success
           :execution-summary-execution-order
           :ignore-case
           :file-operation-pattern-options-ignore-case
           :language
           :notebook
           :notebook-cell-text-document-filter
           :notebook-cell-text-document-filter-language
           :notebook-cell-text-document-filter-notebook
           :parameter-information
           :parameter-information-documentation
           :parameter-information-label
           :diagnostic-related-information-message
           :diagnostic-related-information-location
           :href
           :code-description-href
           :will-delete
           :did-delete
           :will-rename
           :did-rename
           :will-create
           :did-create
           :file-operation-options
           :file-operation-options-will-delete
           :file-operation-options-did-delete
           :file-operation-options-will-rename
           :file-operation-options-did-rename
           :file-operation-options-will-create
           :file-operation-options-did-create
           :change-notifications
           :supported
           :workspace-folders-server-capabilities
           :workspace-folders-server-capabilities-change-notifications
           :workspace-folders-server-capabilities-supported
           :notebook-document-sync-registration-options
           :notebook-selector
           :notebook-document-sync-options
           :notebook-document-sync-options-save
           :notebook-document-sync-options-notebook-selector
           :save
           :will-save-wait-until
           :will-save
           :open-close
           :text-document-sync-options
           :text-document-sync-options-save
           :text-document-sync-options-will-save-wait-until
           :text-document-sync-options-will-save
           :text-document-sync-options-change
           :text-document-sync-options-open-close
           :general
           :window
           :client-capabilities
           :client-capabilities-experimental
           :client-capabilities-general
           :client-capabilities-window
           :client-capabilities-notebook-document
           :client-capabilities-text-document
           :client-capabilities-workspace
           :selected-completion-info-text
           :selected-completion-info-range
           :notebook-cell-array-change
           :notebook-cell-array-change-cells
           :notebook-cell-array-change-delete-count
           :notebook-cell-array-change-start
           :execution-summary
           :document
           :notebook-cell
           :notebook-cell-execution-summary
           :notebook-cell-metadata
           :notebook-cell-document
           :workspace-unchanged-document-diagnostic-report
           :workspace-unchanged-document-diagnostic-report-version
           :workspace-unchanged-document-diagnostic-report-uri
           :workspace-full-document-diagnostic-report
           :workspace-full-document-diagnostic-report-version
           :workspace-full-document-diagnostic-report-uri
           :matches
           :glob
           :file-operation-pattern
           :file-operation-pattern-options
           :file-operation-pattern-matches
           :file-operation-pattern-glob
           :ignore-if-not-exists
           :recursive
           :delete-file-options-ignore-if-not-exists
           :delete-file-options-recursive
           :rename-file-options-ignore-if-exists
           :rename-file-options-overwrite
           :ignore-if-exists
           :overwrite
           :create-file-options-ignore-if-exists
           :create-file-options-overwrite
           :resource-operation
           :resource-operation-annotation-id
           :annotation-id
           :annotated-text-edit
           :annotated-text-edit-annotation-id
           :optional-versioned-text-document-identifier
           :optional-versioned-text-document-identifier-version
           :token-modifiers
           :token-types
           :semantic-tokens-legend
           :semantic-tokens-legend-token-modifiers
           :semantic-tokens-legend-token-types
           :commands
           :execute-command-options
           :execute-command-options-commands
           :prepare-provider
           :rename-options
           :rename-options-prepare-provider
           :more-trigger-character
           :first-trigger-character
           :document-on-type-formatting-options
           :document-on-type-formatting-options-more-trigger-character
           :document-on-type-formatting-options-first-trigger-character
           :ranges-support
           :document-range-formatting-options
           :document-range-formatting-options-ranges-support
           :document-formatting-options
           :trim-final-newlines
           :insert-final-newline
           :trim-trailing-whitespace
           :insert-spaces
           :tab-size
           :formatting-options
           :formatting-options-trim-final-newlines
           :formatting-options-insert-final-newline
           :formatting-options-trim-trailing-whitespace
           :formatting-options-insert-spaces
           :formatting-options-tab-size
           :document-link-options
           :document-link-options-resolve-provider
           :code-lens-options
           :code-lens-options-resolve-provider
           :workspace-symbol-options
           :workspace-symbol-options-resolve-provider
           :code-action-kinds
           :code-action-options
           :code-action-options-resolve-provider
           :code-action-options-code-action-kinds
           :only
           :code-action-context
           :code-action-context-trigger-kind
           :code-action-context-only
           :code-action-context-diagnostics
           :document-symbol-options
           :document-symbol-options-label
           :container-name
           :base-symbol-information
           :base-symbol-information-container-name
           :base-symbol-information-tags
           :base-symbol-information-kind
           :base-symbol-information-name
           :document-highlight-options
           :reference-options
           :include-declaration
           :reference-context
           :reference-context-include-declaration
           :definition-options
           :retrigger-characters
           :signature-help-options
           :signature-help-options-retrigger-characters
           :signature-help-options-trigger-characters
           :parameters
           :signature-information
           :signature-information-active-parameter
           :signature-information-parameters
           :signature-information-documentation
           :signature-information-label
           :active-signature-help
           :is-retrigger
           :signature-help-context
           :signature-help-context-active-signature-help
           :signature-help-context-is-retrigger
           :signature-help-context-trigger-character
           :signature-help-context-trigger-kind
           :hover-options
           :all-commit-characters
           :trigger-characters
           :completion-options
           :completion-options-completion-item
           :completion-options-resolve-provider
           :completion-options-all-commit-characters
           :completion-options-trigger-characters
           :replace
           :insert
           :insert-replace-edit
           :insert-replace-edit-replace
           :insert-replace-edit-insert
           :insert-replace-edit-new-text
           :completion-item-label-details-description
           :completion-item-label-details-detail
           :trigger-character
           :completion-context
           :completion-context-trigger-character
           :completion-context-trigger-kind
           :related-information
           :source
           :code-description
           :code
           :severity
           :diagnostic
           :diagnostic-data
           :diagnostic-related-information
           :diagnostic-tags
           :diagnostic-message
           :diagnostic-source
           :diagnostic-code-description
           :diagnostic-code
           :diagnostic-range
           :glob-pattern
           :file-system-watcher
           :file-system-watcher-kind
           :file-system-watcher-glob-pattern
           :file-event
           :file-event-type
           :file-event-uri
           :include-text
           :save-options
           :save-options-include-text
           :versioned-text-document-identifier
           :versioned-text-document-identifier-version
           :experimental
           :workspace
           :inline-completion-provider
           :diagnostic-provider
           :inlay-hint-provider
           :inline-value-provider
           :type-hierarchy-provider
           :moniker-provider
           :semantic-tokens-provider
           :linked-editing-range-provider
           :call-hierarchy-provider
           :execute-command-provider
           :selection-range-provider
           :folding-range-provider
           :rename-provider
           :document-on-type-formatting-provider
           :document-range-formatting-provider
           :document-formatting-provider
           :workspace-symbol-provider
           :color-provider
           :document-link-provider
           :code-lens-provider
           :code-action-provider
           :document-symbol-provider
           :document-highlight-provider
           :references-provider
           :implementation-provider
           :type-definition-provider
           :definition-provider
           :declaration-provider
           :signature-help-provider
           :hover-provider
           :completion-provider
           :notebook-document-sync
           :text-document-sync
           :position-encoding
           :server-capabilities
           :server-capabilities-experimental
           :server-capabilities-workspace
           :server-capabilities-inline-completion-provider
           :server-capabilities-diagnostic-provider
           :server-capabilities-inlay-hint-provider
           :server-capabilities-inline-value-provider
           :server-capabilities-type-hierarchy-provider
           :server-capabilities-moniker-provider
           :server-capabilities-semantic-tokens-provider
           :server-capabilities-linked-editing-range-provider
           :server-capabilities-call-hierarchy-provider
           :server-capabilities-execute-command-provider
           :server-capabilities-selection-range-provider
           :server-capabilities-folding-range-provider
           :server-capabilities-rename-provider
           :server-capabilities-document-on-type-formatting-provider
           :server-capabilities-document-range-formatting-provider
           :server-capabilities-document-formatting-provider
           :server-capabilities-workspace-symbol-provider
           :server-capabilities-color-provider
           :server-capabilities-document-link-provider
           :server-capabilities-code-lens-provider
           :server-capabilities-code-action-provider
           :server-capabilities-document-symbol-provider
           :server-capabilities-document-highlight-provider
           :server-capabilities-references-provider
           :server-capabilities-implementation-provider
           :server-capabilities-type-definition-provider
           :server-capabilities-definition-provider
           :server-capabilities-declaration-provider
           :server-capabilities-signature-help-provider
           :server-capabilities-hover-provider
           :server-capabilities-completion-provider
           :server-capabilities-notebook-document-sync
           :server-capabilities-text-document-sync
           :server-capabilities-position-encoding
           :workspace-folders
           :workspace-folders-initialize-params
           :workspace-folders-initialize-params-workspace-folders
           :trace
           :initialization-options
           :root-uri
           :root-path
           :locale
           :client-info
           :process-id
           :_initialize-params
           :_initialize-params-trace
           :_initialize-params-initialization-options
           :_initialize-params-capabilities
           :_initialize-params-root-uri
           :_initialize-params-root-path
           :_initialize-params-locale
           :_initialize-params-client-info
           :_initialize-params-process-id
           :unregistration
           :unregistration-method
           :unregistration-id
           :register-options
           :method
           :registration
           :registration-register-options
           :registration-method
           :registration-id
           :inline-completion-options
           :string-value
           :string-value-value
           :string-value-kind
           :selected-completion-info
           :trigger-kind
           :inline-completion-context
           :inline-completion-context-selected-completion-info
           :inline-completion-context-trigger-kind
           :notebook-document-identifier
           :notebook-document-identifier-uri
           :notebook-document-change-event
           :notebook-document-change-event-cells
           :notebook-document-change-event-metadata
           :versioned-notebook-document-identifier
           :versioned-notebook-document-identifier-uri
           :versioned-notebook-document-identifier-version
           :language-id
           :text-document-item
           :text-document-item-text
           :text-document-item-version
           :text-document-item-language-id
           :text-document-item-uri
           :cells
           :metadata
           :notebook-type
           :notebook-document-cells
           :notebook-document-metadata
           :notebook-document-version
           :notebook-document-notebook-type
           :notebook-document-uri
           :previous-result-id-value
           :previous-result-id-uri
           :workspace-diagnostics
           :inter-file-dependencies
           :diagnostic-options
           :diagnostic-options-workspace-diagnostics
           :diagnostic-options-inter-file-dependencies
           :diagnostic-options-identifier
           :unchanged-document-diagnostic-report
           :unchanged-document-diagnostic-report-result-id
           :unchanged-document-diagnostic-report-kind
           :full-document-diagnostic-report
           :full-document-diagnostic-report-items
           :full-document-diagnostic-report-result-id
           :full-document-diagnostic-report-kind
           :related-unchanged-document-diagnostic-report
           :related-unchanged-document-diagnostic-report-related-documents
           :related-full-document-diagnostic-report
           :related-full-document-diagnostic-report-related-documents
           :resolve-provider
           :inlay-hint-options
           :inlay-hint-options-resolve-provider
           :markup-content
           :markup-content-value
           :markup-content-kind
           :inlay-hint-label-part
           :inlay-hint-label-part-command
           :inlay-hint-label-part-location
           :inlay-hint-label-part-tooltip
           :inlay-hint-label-part-value
           :inline-value-options
           :expression
           :inline-value-evaluatable-expression
           :inline-value-evaluatable-expression-expression
           :inline-value-evaluatable-expression-range
           :case-sensitive-lookup
           :variable-name
           :inline-value-variable-lookup
           :inline-value-variable-lookup-case-sensitive-lookup
           :inline-value-variable-lookup-variable-name
           :inline-value-variable-lookup-range
           :inline-value-text
           :inline-value-text-text
           :inline-value-text-range
           :stopped-location
           :frame-id
           :inline-value-context
           :inline-value-context-stopped-location
           :inline-value-context-frame-id
           :type-hierarchy-options
           :moniker-options
           :file-delete
           :file-delete-uri
           :file-rename
           :file-rename-new-uri
           :file-rename-old-uri
           :pattern
           :file-operation-filter
           :file-operation-filter-pattern
           :file-operation-filter-scheme
           :description
           :needs-confirmation
           :change-annotation
           :change-annotation-description
           :change-annotation-needs-confirmation
           :change-annotation-label
           :delete-file
           :delete-file-options
           :delete-file-uri
           :delete-file-kind
           :new-uri
           :old-uri
           :rename-file
           :rename-file-options
           :rename-file-new-uri
           :rename-file-old-uri
           :rename-file-kind
           :create-file
           :create-file-options
           :create-file-uri
           :create-file-kind
           :text-document-edit
           :text-document-edit-edits
           :text-document-edit-text-document
           :file-create
           :file-create-uri
           :linked-editing-range-options
           :delete-count
           :semantic-tokens-edit
           :semantic-tokens-edit-data
           :semantic-tokens-edit-delete-count
           :semantic-tokens-edit-start
           :full
           :legend
           :semantic-tokens-options
           :semantic-tokens-options-full
           :semantic-tokens-options-range
           :semantic-tokens-options-legend
           :call-hierarchy-options
           :selection-range-options
           :character
           :line
           :position-character
           :position-line
           :declaration-options
           :folding-range-options
           :document-color-options
           :alpha
           :blue
           :green
           :red
           :color-alpha
           :color-blue
           :color-green
           :color-red
           :text-document-identifier
           :text-document-identifier-uri
           :scope-uri
           :configuration-item
           :configuration-item-section
           :configuration-item-scope-uri
           :removed
           :added
           :workspace-folders-change-event
           :workspace-folders-change-event-removed
           :workspace-folders-change-event-added
           :type-definition-options
           :static-registration-options
           :static-registration-options-id
           :implementation-options
           :end
           :start
           :range-end
           :range-start
           :target-selection-range
           :target-range
           :target-uri
           :origin-selection-range
           :location-link
           :location-link-target-selection-range
           :location-link-target-range
           :location-link-target-uri
           :location-link-origin-selection-range
           :partial-result-token
           :partial-result-params
           :partial-result-params-partial-result-token
           :work-done-token
           :work-done-progress-params
           :work-done-progress-params-work-done-token
           :text-document-position-params
           :text-document-position-params-position
           :text-document-position-params-text-document
           :progress-params
           :progress-params-value
           :progress-params-token
           :id
           :cancel-params
           :cancel-params-id
           :verbose
           :log-trace-params
           :log-trace-params-verbose
           :log-trace-params-message
           :value
           :set-trace-params
           :set-trace-params-value
           :work-done-progress-end
           :work-done-progress-end-message
           :work-done-progress-end-kind
           :work-done-progress-report
           :work-done-progress-report-percentage
           :work-done-progress-report-message
           :work-done-progress-report-cancellable
           :work-done-progress-report-kind
           :percentage
           :cancellable
           :work-done-progress-begin
           :work-done-progress-begin-percentage
           :work-done-progress-begin-message
           :work-done-progress-begin-cancellable
           :work-done-progress-begin-title
           :work-done-progress-begin-kind
           :failed-change
           :failure-reason
           :applied
           :apply-workspace-edit-result
           :apply-workspace-edit-result-failed-change
           :apply-workspace-edit-result-failure-reason
           :apply-workspace-edit-result-applied
           :apply-workspace-edit-params
           :apply-workspace-edit-params-edit
           :apply-workspace-edit-params-label
           :execute-command-registration-options
           :execute-command-params
           :execute-command-params-arguments
           :execute-command-params-command
           :prepare-rename-params
           :rename-registration-options
           :new-name
           :rename-params
           :rename-params-new-name
           :rename-params-position
           :rename-params-text-document
           :document-on-type-formatting-registration-options
           :ch
           :document-on-type-formatting-params
           :document-on-type-formatting-params-options
           :document-on-type-formatting-params-ch
           :document-on-type-formatting-params-position
           :document-on-type-formatting-params-text-document
           :document-ranges-formatting-params
           :document-ranges-formatting-params-options
           :document-ranges-formatting-params-ranges
           :document-ranges-formatting-params-text-document
           :document-range-formatting-registration-options
           :document-range-formatting-params
           :document-range-formatting-params-options
           :document-range-formatting-params-range
           :document-range-formatting-params-text-document
           :document-formatting-registration-options
           :options
           :document-formatting-params
           :document-formatting-params-options
           :document-formatting-params-text-document
           :document-link-registration-options
           :target
           :document-link
           :document-link-data
           :document-link-tooltip
           :document-link-target
           :document-link-range
           :document-link-params
           :document-link-params-text-document
           :code-lens-registration-options
           :code-lens
           :code-lens-data
           :code-lens-command
           :code-lens-range
           :code-lens-params
           :code-lens-params-text-document
           :workspace-symbol-registration-options
           :workspace-symbol
           :workspace-symbol-data
           :workspace-symbol-location
           :query
           :workspace-symbol-params
           :workspace-symbol-params-query
           :code-action-registration-options
           :edit
           :disabled
           :is-preferred
           :code-action
           :code-action-data
           :code-action-command
           :code-action-edit
           :code-action-disabled
           :code-action-is-preferred
           :code-action-diagnostics
           :code-action-title
           :arguments
           :command-arguments
           :command-command
           :command-title
           :code-action-params
           :code-action-params-context
           :code-action-params-range
           :code-action-params-text-document
           :document-symbol-registration-options
           :children
           :document-symbol
           :document-symbol-children
           :document-symbol-selection-range
           :document-symbol-range
           :document-symbol-deprecated
           :document-symbol-tags
           :document-symbol-kind
           :document-symbol-detail
           :document-symbol-name
           :symbol-information
           :symbol-information-location
           :symbol-information-deprecated
           :document-symbol-params
           :document-symbol-params-text-document
           :document-highlight-registration-options
           :document-highlight
           :document-highlight-range
           :document-highlight-params
           :reference-registration-options
           :reference-params
           :reference-params-context
           :definition-registration-options
           :definition-params
           :signature-help-registration-options
           :active-parameter
           :active-signature
           :signatures
           :signature-help
           :signature-help-active-parameter
           :signature-help-active-signature
           :signature-help-signatures
           :signature-help-params
           :signature-help-params-context
           :hover-registration-options
           :contents
           :hover
           :hover-range
           :hover-contents
           :hover-params
           :completion-registration-options
           :item-defaults
           :is-incomplete
           :completion-list
           :completion-list-items
           :completion-list-item-defaults
           :completion-list-is-incomplete
           :commit-characters
           :text-edit-text
           :sort-text
           :preselect
           :deprecated
           :documentation
           :label-details
           :completion-item
           :completion-item-data
           :completion-item-command
           :completion-item-commit-characters
           :completion-item-additional-text-edits
           :completion-item-text-edit-text
           :completion-item-text-edit
           :completion-item-insert-text-mode
           :completion-item-insert-text-format
           :completion-item-insert-text
           :completion-item-filter-text
           :completion-item-sort-text
           :completion-item-preselect
           :completion-item-deprecated
           :completion-item-documentation
           :completion-item-detail
           :completion-item-tags
           :completion-item-label-details
           :completion-item-label
           :completion-params
           :completion-params-context
           :diagnostics
           :version
           :publish-diagnostics-params
           :publish-diagnostics-params-diagnostics
           :publish-diagnostics-params-version
           :publish-diagnostics-params-uri
           :watchers
           :did-change-watched-files-registration-options
           :did-change-watched-files-registration-options-watchers
           :did-change-watched-files-params
           :did-change-watched-files-params-changes
           :new-text
           :text-edit-new-text
           :text-edit-range
           :reason
           :will-save-text-document-params
           :will-save-text-document-params-reason
           :will-save-text-document-params-text-document
           :text-document-save-registration-options
           :text
           :did-save-text-document-params
           :did-save-text-document-params-text
           :did-save-text-document-params-text-document
           :did-close-text-document-params
           :did-close-text-document-params-text-document
           :sync-kind
           :text-document-change-registration-options
           :text-document-change-registration-options-sync-kind
           :content-changes
           :did-change-text-document-params
           :did-change-text-document-params-content-changes
           :did-change-text-document-params-text-document
           :did-open-text-document-params
           :did-open-text-document-params-text-document
           :log-message-params
           :log-message-params-message
           :log-message-params-type
           :title
           :message-action-item
           :message-action-item-title
           :actions
           :show-message-request-params
           :show-message-request-params-actions
           :show-message-request-params-message
           :show-message-request-params-type
           :message
           :type
           :show-message-params
           :show-message-params-message
           :show-message-params-type
           :section
           :did-change-configuration-registration-options
           :did-change-configuration-registration-options-section
           :settings
           :did-change-configuration-params
           :did-change-configuration-params-settings
           :initialized-params
           :retry
           :initialize-error
           :initialize-error-retry
           :server-info
           :capabilities
           :initialize-result
           :initialize-result-server-info
           :initialize-result-capabilities
           :initialize-params
           :unregisterations
           :unregistration-params
           :unregistration-params-unregisterations
           :registrations
           :registration-params
           :registration-params-registrations
           :inline-completion-registration-options
           :command
           :filter-text
           :insert-text
           :inline-completion-item
           :inline-completion-item-command
           :inline-completion-item-range
           :inline-completion-item-filter-text
           :inline-completion-item-insert-text
           :inline-completion-list
           :inline-completion-list-items
           :inline-completion-params
           :inline-completion-params-context
           :did-close-notebook-document-params
           :did-close-notebook-document-params-cell-text-documents
           :did-close-notebook-document-params-notebook-document
           :did-save-notebook-document-params
           :did-save-notebook-document-params-notebook-document
           :change
           :did-change-notebook-document-params
           :did-change-notebook-document-params-change
           :did-change-notebook-document-params-notebook-document
           :cell-text-documents
           :notebook-document
           :did-open-notebook-document-params
           :did-open-notebook-document-params-cell-text-documents
           :did-open-notebook-document-params-notebook-document
           :workspace-diagnostic-report-partial-result
           :workspace-diagnostic-report-partial-result-items
           :workspace-diagnostic-report
           :workspace-diagnostic-report-items
           :previous-result-ids
           :workspace-diagnostic-params
           :workspace-diagnostic-params-previous-result-ids
           :workspace-diagnostic-params-identifier
           :diagnostic-registration-options
           :retrigger-request
           :diagnostic-server-cancellation-data
           :diagnostic-server-cancellation-data-retrigger-request
           :related-documents
           :document-diagnostic-report-partial-result
           :document-diagnostic-report-partial-result-related-documents
           :document-diagnostic-params
           :document-diagnostic-params-previous-result-id
           :document-diagnostic-params-identifier
           :document-diagnostic-params-text-document
           :inlay-hint-registration-options
           :padding-right
           :padding-left
           :tooltip
           :text-edits
           :position
           :inlay-hint
           :inlay-hint-data
           :inlay-hint-padding-right
           :inlay-hint-padding-left
           :inlay-hint-tooltip
           :inlay-hint-text-edits
           :inlay-hint-label
           :inlay-hint-position
           :inlay-hint-params
           :inlay-hint-params-range
           :inlay-hint-params-text-document
           :inline-value-registration-options
           :context
           :inline-value-params
           :inline-value-params-context
           :inline-value-params-range
           :inline-value-params-text-document
           :type-hierarchy-subtypes-params
           :type-hierarchy-subtypes-params-item
           :type-hierarchy-supertypes-params
           :type-hierarchy-supertypes-params-item
           :type-hierarchy-registration-options
           :type-hierarchy-item
           :type-hierarchy-item-data
           :type-hierarchy-item-selection-range
           :type-hierarchy-item-range
           :type-hierarchy-item-uri
           :type-hierarchy-item-detail
           :type-hierarchy-item-tags
           :type-hierarchy-item-kind
           :type-hierarchy-item-name
           :type-hierarchy-prepare-params
           :moniker-registration-options
           :unique
           :identifier
           :scheme
           :moniker
           :moniker-unique
           :moniker-identifier
           :moniker-scheme
           :moniker-params
           :delete-files-params
           :delete-files-params-files
           :rename-files-params
           :rename-files-params-files
           :filters
           :file-operation-registration-options
           :file-operation-registration-options-filters
           :change-annotations
           :document-changes
           :changes
           :workspace-edit
           :workspace-edit-change-annotations
           :workspace-edit-document-changes
           :workspace-edit-changes
           :files
           :create-files-params
           :create-files-params-files
           :linked-editing-range-registration-options
           :word-pattern
           :ranges
           :linked-editing-ranges
           :linked-editing-ranges-word-pattern
           :linked-editing-ranges-ranges
           :linked-editing-range-params
           :success
           :show-document-result
           :show-document-result-success
           :selection
           :take-focus
           :external
           :show-document-params
           :show-document-params-selection
           :show-document-params-take-focus
           :show-document-params-external
           :show-document-params-uri
           :semantic-tokens-range-params
           :semantic-tokens-range-params-range
           :semantic-tokens-range-params-text-document
           :semantic-tokens-delta-partial-result
           :semantic-tokens-delta-partial-result-edits
           :edits
           :semantic-tokens-delta
           :semantic-tokens-delta-edits
           :semantic-tokens-delta-result-id
           :previous-result-id
           :semantic-tokens-delta-params
           :semantic-tokens-delta-params-previous-result-id
           :semantic-tokens-delta-params-text-document
           :semantic-tokens-registration-options
           :semantic-tokens-partial-result
           :semantic-tokens-partial-result-data
           :result-id
           :semantic-tokens
           :semantic-tokens-data
           :semantic-tokens-result-id
           :semantic-tokens-params
           :semantic-tokens-params-text-document
           :to
           :call-hierarchy-outgoing-call
           :call-hierarchy-outgoing-call-from-ranges
           :call-hierarchy-outgoing-call-to
           :call-hierarchy-outgoing-calls-params
           :call-hierarchy-outgoing-calls-params-item
           :from-ranges
           :from
           :call-hierarchy-incoming-call
           :call-hierarchy-incoming-call-from-ranges
           :call-hierarchy-incoming-call-from
           :item
           :call-hierarchy-incoming-calls-params
           :call-hierarchy-incoming-calls-params-item
           :call-hierarchy-registration-options
           :data
           :detail
           :tags
           :call-hierarchy-item
           :call-hierarchy-item-data
           :call-hierarchy-item-selection-range
           :call-hierarchy-item-range
           :call-hierarchy-item-uri
           :call-hierarchy-item-detail
           :call-hierarchy-item-tags
           :call-hierarchy-item-kind
           :call-hierarchy-item-name
           :call-hierarchy-prepare-params
           :work-done-progress-cancel-params
           :work-done-progress-cancel-params-token
           :token
           :work-done-progress-create-params
           :work-done-progress-create-params-token
           :selection-range-registration-options
           :parent
           :selection-range
           :selection-range-parent
           :selection-range-range
           :positions
           :selection-range-params
           :selection-range-params-positions
           :selection-range-params-text-document
           :declaration-registration-options
           :declaration-params
           :folding-range-registration-options
           :collapsed-text
           :kind
           :end-character
           :end-line
           :start-character
           :start-line
           :folding-range
           :folding-range-collapsed-text
           :folding-range-end-character
           :folding-range-end-line
           :folding-range-start-character
           :folding-range-start-line
           :folding-range-params
           :folding-range-params-text-document
           :document-selector
           :text-document-registration-options
           :text-document-registration-options-document-selector
           :work-done-progress
           :work-done-progress-options
           :work-done-progress-options-work-done-progress
           :additional-text-edits
           :text-edit
           :label
           :color-presentation
           :color-presentation-additional-text-edits
           :color-presentation-text-edit
           :color-presentation-label
           :color-presentation-params
           :color-presentation-params-range
           :color-presentation-params-color
           :color-presentation-params-text-document
           :document-color-registration-options
           :color
           :color-information
           :color-information-color
           :color-information-range
           :text-document
           :document-color-params
           :document-color-params-text-document
           :items
           :configuration-params
           :configuration-params-items
           :event
           :did-change-workspace-folders-params
           :did-change-workspace-folders-params-event
           :name
           :workspace-folder
           :workspace-folder-name
           :workspace-folder-uri
           :type-definition-registration-options
           :type-definition-params
           :implementation-registration-options
           :range
           :uri
           :location
           :location-range
           :location-uri
           :implementation-params
           :token-format-relative
           :token-format
           :prepare-support-default-behavior-identifier
           :prepare-support-default-behavior
           :failure-handling-kind-undo
           :failure-handling-kind-text-only-transactional
           :failure-handling-kind-transactional
           :failure-handling-kind-abort
           :failure-handling-kind
           :resource-operation-kind-delete
           :resource-operation-kind-rename
           :resource-operation-kind-create
           :resource-operation-kind
           :notebook-cell-kind-code
           :notebook-cell-kind-markup
           :notebook-cell-kind
           :file-operation-pattern-kind-folder
           :file-operation-pattern-kind-file
           :file-operation-pattern-kind
           :code-action-trigger-kind-automatic
           :code-action-trigger-kind-invoked
           :code-action-trigger-kind
           :signature-help-trigger-kind-content-change
           :signature-help-trigger-kind-trigger-character
           :signature-help-trigger-kind-invoked
           :signature-help-trigger-kind
           :completion-trigger-kind-trigger-for-incomplete-completions
           :completion-trigger-kind-trigger-character
           :completion-trigger-kind-invoked
           :completion-trigger-kind
           :diagnostic-tag-deprecated
           :diagnostic-tag-unnecessary
           :diagnostic-tag
           :diagnostic-severity-hint
           :diagnostic-severity-information
           :diagnostic-severity-warning
           :diagnostic-severity-error
           :diagnostic-severity
           :watch-kind-delete
           :watch-kind-change
           :watch-kind-create
           :watch-kind
           :file-change-type-deleted
           :file-change-type-changed
           :file-change-type-created
           :file-change-type
           :position-encoding-kind-utf32
           :position-encoding-kind-utf16
           :position-encoding-kind-utf8
           :position-encoding-kind
           :inline-completion-trigger-kind-automatic
           :inline-completion-trigger-kind-invoked
           :inline-completion-trigger-kind
           :markup-kind-markdown
           :markup-kind-plain-text
           :markup-kind
           :trace-values-verbose
           :trace-values-messages
           :trace-values-off
           :trace-values
           :code-action-kind-source-fix-all
           :code-action-kind-source-organize-imports
           :code-action-kind-source
           :code-action-kind-refactor-rewrite
           :code-action-kind-refactor-inline
           :code-action-kind-refactor-extract
           :code-action-kind-refactor
           :code-action-kind-quick-fix
           :code-action-kind-empty
           :code-action-kind
           :document-highlight-kind-write
           :document-highlight-kind-read
           :document-highlight-kind-text
           :document-highlight-kind
           :insert-text-mode-adjust-indentation
           :insert-text-mode-as-is
           :insert-text-mode
           :insert-text-format-snippet
           :insert-text-format-plain-text
           :insert-text-format
           :completion-item-tag-deprecated
           :completion-item-tag
           :completion-item-kind-type-parameter
           :completion-item-kind-operator
           :completion-item-kind-event
           :completion-item-kind-struct
           :completion-item-kind-constant
           :completion-item-kind-enum-member
           :completion-item-kind-folder
           :completion-item-kind-reference
           :completion-item-kind-file
           :completion-item-kind-color
           :completion-item-kind-snippet
           :completion-item-kind-keyword
           :completion-item-kind-enum
           :completion-item-kind-value
           :completion-item-kind-unit
           :completion-item-kind-property
           :completion-item-kind-module
           :completion-item-kind-interface
           :completion-item-kind-class
           :completion-item-kind-variable
           :completion-item-kind-field
           :completion-item-kind-constructor
           :completion-item-kind-function
           :completion-item-kind-method
           :completion-item-kind-text
           :completion-item-kind
           :text-document-save-reason-focus-out
           :text-document-save-reason-after-delay
           :text-document-save-reason-manual
           :text-document-save-reason
           :text-document-sync-kind-incremental
           :text-document-sync-kind-full
           :text-document-sync-kind-none
           :text-document-sync-kind
           :message-type-debug
           :message-type-log
           :message-type-info
           :message-type-warning
           :message-type-error
           :message-type
           :inlay-hint-kind-parameter
           :inlay-hint-kind-type
           :inlay-hint-kind
           :moniker-kind-local
           :moniker-kind-export
           :moniker-kind-import
           :moniker-kind
           :uniqueness-level-global
           :uniqueness-level-scheme
           :uniqueness-level-group
           :uniqueness-level-project
           :uniqueness-level-document
           :uniqueness-level
           :symbol-tag-deprecated
           :symbol-tag
           :symbol-kind-type-parameter
           :symbol-kind-operator
           :symbol-kind-event
           :symbol-kind-struct
           :symbol-kind-enum-member
           :symbol-kind-null
           :symbol-kind-key
           :symbol-kind-object
           :symbol-kind-array
           :symbol-kind-boolean
           :symbol-kind-number
           :symbol-kind-string
           :symbol-kind-constant
           :symbol-kind-variable
           :symbol-kind-function
           :symbol-kind-interface
           :symbol-kind-enum
           :symbol-kind-constructor
           :symbol-kind-field
           :symbol-kind-property
           :symbol-kind-method
           :symbol-kind-class
           :symbol-kind-package
           :symbol-kind-namespace
           :symbol-kind-module
           :symbol-kind-file
           :symbol-kind
           :folding-range-kind-region
           :folding-range-kind-imports
           :folding-range-kind-comment
           :folding-range-kind
           :lsp-error-codes-request-cancelled
           :lsp-error-codes-content-modified
           :lsp-error-codes-server-cancelled
           :lsp-error-codes-request-failed
           :lsp-error-codes
           :error-codes-unknown-error-code
           :error-codes-server-not-initialized
           :error-codes-internal-error
           :error-codes-invalid-params
           :error-codes-method-not-found
           :error-codes-invalid-request
           :error-codes-parse-error
           :error-codes
           :document-diagnostic-report-kind-unchanged
           :document-diagnostic-report-kind-full
           :document-diagnostic-report-kind
           :semantic-token-modifiers-default-library
           :semantic-token-modifiers-documentation
           :semantic-token-modifiers-modification
           :semantic-token-modifiers-async
           :semantic-token-modifiers-abstract
           :semantic-token-modifiers-deprecated
           :semantic-token-modifiers-static
           :semantic-token-modifiers-readonly
           :semantic-token-modifiers-definition
           :semantic-token-modifiers-declaration
           :semantic-token-modifiers
           :semantic-token-types-decorator
           :semantic-token-types-operator
           :semantic-token-types-regexp
           :semantic-token-types-number
           :semantic-token-types-string
           :semantic-token-types-comment
           :semantic-token-types-modifier
           :semantic-token-types-keyword
           :semantic-token-types-macro
           :semantic-token-types-method
           :semantic-token-types-function
           :semantic-token-types-event
           :semantic-token-types-enum-member
           :semantic-token-types-property
           :semantic-token-types-variable
           :semantic-token-types-parameter
           :semantic-token-types-type-parameter
           :semantic-token-types-struct
           :semantic-token-types-interface
           :semantic-token-types-enum
           :semantic-token-types-class
           :semantic-token-types-type
           :semantic-token-types-namespace
           :semantic-token-types
           :*version*))
(common-lisp:in-package :lem-lsp-base/protocol-3-17)

(lem-lsp-base/type:define-enum semantic-token-types
    ((namespace "namespace")
     (type "type" :documentation
      "Represents a generic type. Acts as a fallback for types which can't be mapped to
a specific type like class or enum.")
     (class "class") (enum "enum") (interface "interface") (struct "struct")
     (type-parameter "typeParameter") (parameter "parameter") (variable "variable")
     (property "property") (enum-member "enumMember") (event "event") (function "function")
     (method "method") (macro "macro") (keyword "keyword") (modifier "modifier")
     (comment "comment") (string "string") (number "number") (regexp "regexp")
     (operator "operator") (decorator "decorator" :documentation "@since 3.17.0" :since "3.17.0"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "A set of predefined token types. This set is not fixed
an clients can specify additional token types via the
corresponding client capabilities.

@since 3.16.0"
  :since
  "3.16.0")

(lem-lsp-base/type:define-enum semantic-token-modifiers
    ((declaration "declaration") (definition "definition") (readonly "readonly") (static "static")
     (deprecated "deprecated") (abstract "abstract") (async "async") (modification "modification")
     (documentation "documentation") (default-library "defaultLibrary"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "A set of predefined token modifiers. This set is not fixed
an clients can specify additional token types via the
corresponding client capabilities.

@since 3.16.0"
  :since
  "3.16.0")

(lem-lsp-base/type:define-enum document-diagnostic-report-kind
    ((full "full" :documentation "A diagnostic report with a full
set of problems.")
     (unchanged "unchanged" :documentation "A report indicating that the last
returned report is still accurate."))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "The document diagnostic report kinds.

@since 3.17.0"
  :since
  "3.17.0")

(lem-lsp-base/type:define-enum error-codes
    ((parse-error -32700) (invalid-request -32600) (method-not-found -32601)
     (invalid-params -32602) (internal-error -32603)
     (server-not-initialized -32002 :documentation
      "Error code indicating that a server received a notification or
request before the server has received the `initialize` request.")
     (unknown-error-code -32001))
  (:type lem-lsp-base/type:lsp-integer)
  :since
  "Predefined error codes.")

(lem-lsp-base/type:define-enum lsp-error-codes
    ((request-failed -32803 :documentation
      "A request failed but it was syntactically correct, e.g the
method name was known and the parameters were valid. The error
message should contain human readable information about why
the request failed.

@since 3.17.0"
      :since "3.17.0")
     (server-cancelled -32802 :documentation
      "The server cancelled the request. This error code should
only be used for requests that explicitly support being
server cancellable.

@since 3.17.0"
      :since "3.17.0")
     (content-modified -32801 :documentation "The server detected that the content of a document got
modified outside normal conditions. A server should
NOT send this error code if it detects a content change
in it unprocessed messages. The result even computed
on an older state might still be useful for the client.

If a client decides that a result is not of any use anymore
the client should cancel the request.")
     (request-cancelled -32800 :documentation
      "The client has canceled a request and a server has detected
the cancel."))
  (:type lem-lsp-base/type:lsp-integer))

(lem-lsp-base/type:define-enum folding-range-kind
    ((comment "comment" :documentation "Folding range for a comment")
     (imports "imports" :documentation "Folding range for an import or include")
     (region "region" :documentation "Folding range for a region (e.g. `#region`)"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "A set of predefined range kinds.")

(lem-lsp-base/type:define-enum symbol-kind
    ((file 1) (module 2) (namespace 3) (package 4) (class 5) (method 6) (property 7) (field 8)
     (constructor 9) (enum 10) (interface 11) (function 12) (variable 13) (constant 14) (string 15)
     (number 16) (boolean 17) (array 18) (object 19) (key 20) (null 21) (enum-member 22)
     (struct 23) (event 24) (operator 25) (type-parameter 26))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "A symbol kind.")

(lem-lsp-base/type:define-enum symbol-tag
    ((deprecated 1 :documentation "Render a symbol as obsolete, usually using a strike-out."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Symbol tags are extra annotations that tweak the rendering of a symbol.

@since 3.16"
  :since
  "3.16")

(lem-lsp-base/type:define-enum uniqueness-level
    ((document "document" :documentation "The moniker is only unique inside a document")
     (project "project" :documentation
      "The moniker is unique inside a project for which a dump got created")
     (group "group" :documentation
      "The moniker is unique inside the group to which a project belongs")
     (scheme "scheme" :documentation "The moniker is unique inside the moniker scheme.")
     (global "global" :documentation "The moniker is globally unique"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "Moniker uniqueness level to define scope of the moniker.

@since 3.16.0"
  :since
  "3.16.0")

(lem-lsp-base/type:define-enum moniker-kind
    ((import "import" :documentation
      "The moniker represent a symbol that is imported into a project")
     (export "export" :documentation
      "The moniker represents a symbol that is exported from a project")
     (local "local" :documentation
      "The moniker represents a symbol that is local to a project (e.g. a local
variable of a function, a class not visible outside the project, ...)"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "The moniker kind.

@since 3.16.0"
  :since
  "3.16.0")

(lem-lsp-base/type:define-enum inlay-hint-kind
    ((type 1 :documentation "An inlay hint that for a type annotation.")
     (parameter 2 :documentation "An inlay hint that is for a parameter."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Inlay hint kinds.

@since 3.17.0"
  :since
  "3.17.0")

(lem-lsp-base/type:define-enum message-type
    ((error 1 :documentation "An error message.") (warning 2 :documentation "A warning message.")
     (info 3 :documentation "An information message.") (log 4 :documentation "A log message.")
     (debug 5 :documentation "A debug message.

@since 3.18.0"
      :since "3.18.0"))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "The message type")

(lem-lsp-base/type:define-enum text-document-sync-kind
    ((none 0 :documentation "Documents should not be synced at all.")
     (full 1 :documentation "Documents are synced by always sending the full content
of the document.")
     (incremental 2 :documentation "Documents are synced by sending the full content on open.
After that only incremental updates to the document are
send."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Defines how the host (editor) should sync
document changes to the language server.")

(lem-lsp-base/type:define-enum text-document-save-reason
    ((manual 1 :documentation
      "Manually triggered, e.g. by the user pressing save, by starting debugging,
or by an API call.")
     (after-delay 2 :documentation "Automatic after a delay.")
     (focus-out 3 :documentation "When the editor lost focus."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Represents reasons why a text document is saved.")

(lem-lsp-base/type:define-enum completion-item-kind
    ((text 1) (method 2) (function 3) (constructor 4) (field 5) (variable 6) (class 7)
     (interface 8) (module 9) (property 10) (unit 11) (value 12) (enum 13) (keyword 14)
     (snippet 15) (color 16) (file 17) (reference 18) (folder 19) (enum-member 20) (constant 21)
     (struct 22) (event 23) (operator 24) (type-parameter 25))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "The kind of a completion entry.")

(lem-lsp-base/type:define-enum completion-item-tag
    ((deprecated 1 :documentation "Render a completion as obsolete, usually using a strike-out."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Completion item tags are extra annotations that tweak the rendering of a completion
item.

@since 3.15.0"
  :since
  "3.15.0")

(lem-lsp-base/type:define-enum insert-text-format
    ((plain-text 1 :documentation "The primary text to be inserted is treated as a plain string.")
     (snippet 2 :documentation "The primary text to be inserted is treated as a snippet.

A snippet can define tab stops and placeholders with `$1`, `$2`
and `${3:foo}`. `$0` defines the final tab stop, it defaults to
the end of the snippet. Placeholders with equal identifiers are linked,
that is typing in one will update others too.

See also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax"))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Defines whether the insert text in a completion item should be interpreted as
plain text or a snippet.")

(lem-lsp-base/type:define-enum insert-text-mode
    ((as-is 1 :documentation "The insertion or replace strings is taken as it is. If the
value is multi line the lines below the cursor will be
inserted using the indentation defined in the string value.
The client will not apply any kind of adjustments to the
string.")
     (adjust-indentation 2 :documentation
      "The editor adjusts leading whitespace of new lines so that
they match the indentation up to the cursor of the line for
which the item is accepted.

Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
multi line completion item is indented using 2 tabs and all
following lines inserted will be indented using 2 tabs as well."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "How whitespace and indentation is handled during completion
item insertion.

@since 3.16.0"
  :since
  "3.16.0")

(lem-lsp-base/type:define-enum document-highlight-kind
    ((text 1 :documentation "A textual occurrence.")
     (read 2 :documentation "Read-access of a symbol, like reading a variable.")
     (write 3 :documentation "Write-access of a symbol, like writing to a variable."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "A document highlight kind.")

(lem-lsp-base/type:define-enum code-action-kind
    ((empty "" :documentation "Empty kind.")
     (quick-fix "quickfix" :documentation "Base kind for quickfix actions: 'quickfix'")
     (refactor "refactor" :documentation "Base kind for refactoring actions: 'refactor'")
     (refactor-extract "refactor.extract" :documentation
      "Base kind for refactoring extraction actions: 'refactor.extract'

Example extract actions:

- Extract method
- Extract function
- Extract variable
- Extract interface from class
- ...")
     (refactor-inline "refactor.inline" :documentation
      "Base kind for refactoring inline actions: 'refactor.inline'

Example inline actions:

- Inline function
- Inline variable
- Inline constant
- ...")
     (refactor-rewrite "refactor.rewrite" :documentation
      "Base kind for refactoring rewrite actions: 'refactor.rewrite'

Example rewrite actions:

- Convert JavaScript function to class
- Add or remove parameter
- Encapsulate field
- Make method static
- Move method to base class
- ...")
     (source "source" :documentation "Base kind for source actions: `source`

Source code actions apply to the entire file.")
     (source-organize-imports "source.organizeImports" :documentation
      "Base kind for an organize imports source action: `source.organizeImports`")
     (source-fix-all "source.fixAll" :documentation
      "Base kind for auto-fix source actions: `source.fixAll`.

Fix all actions automatically fix errors that have a clear fix that do not require user input.
They should not suppress errors or perform unsafe fixes such as generating new types or classes.

@since 3.15.0"
      :since "3.15.0"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "A set of predefined code action kinds")

(lem-lsp-base/type:define-enum trace-values
    ((off "off" :documentation "Turn tracing off.")
     (messages "messages" :documentation "Trace messages only.")
     (verbose "verbose" :documentation "Verbose message tracing."))
  (:type lem-lsp-base/type:lsp-string))

(lem-lsp-base/type:define-enum markup-kind
    ((plain-text "plaintext" :documentation "Plain text is supported as a content format")
     (markdown "markdown" :documentation "Markdown is supported as a content format"))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "Describes the content type that a client supports in various
result literals like `Hover`, `ParameterInfo` or `CompletionItem`.

Please note that `MarkupKinds` must not start with a `$`. This kinds
are reserved for internal usage.")

(lem-lsp-base/type:define-enum inline-completion-trigger-kind
    ((invoked 0 :documentation "Completion was triggered explicitly by a user gesture.")
     (automatic 1 :documentation "Completion was triggered automatically while editing."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "Describes how an {@link InlineCompletionItemProvider inline completion provider} was triggered.

@since 3.18.0
@proposed"
  :since
  common-lisp:t
  :since
  "3.18.0")

(lem-lsp-base/type:define-enum position-encoding-kind
    ((utf8 "utf-8" :documentation "Character offsets count UTF-8 code units (e.g. bytes).")
     (utf16 "utf-16" :documentation "Character offsets count UTF-16 code units.

This is the default and must always be supported
by servers")
     (utf32 "utf-32" :documentation "Character offsets count UTF-32 code units.

Implementation note: these are the same as Unicode codepoints,
so this `PositionEncodingKind` may also be used for an
encoding-agnostic representation of character offsets."))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "A set of predefined position encoding kinds.

@since 3.17.0"
  :since
  "3.17.0")

(lem-lsp-base/type:define-enum file-change-type
    ((created 1 :documentation "The file got created.")
     (changed 2 :documentation "The file got changed.")
     (deleted 3 :documentation "The file got deleted."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "The file event type")

(lem-lsp-base/type:define-enum watch-kind
    ((create 1 :documentation "Interested in create events.")
     (change 2 :documentation "Interested in change events")
     (delete 4 :documentation "Interested in delete events"))
  (:type lem-lsp-base/type:lsp-uinteger))

(lem-lsp-base/type:define-enum diagnostic-severity
    ((error 1 :documentation "Reports an error.") (warning 2 :documentation "Reports a warning.")
     (information 3 :documentation "Reports an information.")
     (hint 4 :documentation "Reports a hint."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "The diagnostic's severity.")

(lem-lsp-base/type:define-enum diagnostic-tag
    ((unnecessary 1 :documentation "Unused or unnecessary code.

Clients are allowed to render diagnostics with this tag faded out instead of having
an error squiggle.")
     (deprecated 2 :documentation "Deprecated or obsolete code.

Clients are allowed to rendered diagnostics with this tag strike through."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "The diagnostic tags.

@since 3.15.0"
  :since
  "3.15.0")

(lem-lsp-base/type:define-enum completion-trigger-kind
    ((invoked 1 :documentation "Completion was triggered by typing an identifier (24x7 code
complete), manual invocation (e.g Ctrl+Space) or via API.")
     (trigger-character 2 :documentation
      "Completion was triggered by a trigger character specified by
the `triggerCharacters` properties of the `CompletionRegistrationOptions`.")
     (trigger-for-incomplete-completions 3 :documentation
      "Completion was re-triggered as current completion list is incomplete"))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "How a completion was triggered")

(lem-lsp-base/type:define-enum signature-help-trigger-kind
    ((invoked 1 :documentation "Signature help was invoked manually by the user or by a command.")
     (trigger-character 2 :documentation "Signature help was triggered by a trigger character.")
     (content-change 3 :documentation
      "Signature help was triggered by the cursor moving or by the document content changing."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "How a signature help was triggered.

@since 3.15.0"
  :since
  "3.15.0")

(lem-lsp-base/type:define-enum code-action-trigger-kind
    ((invoked 1 :documentation
      "Code actions were explicitly requested by the user or by an extension.")
     (automatic 2 :documentation "Code actions were requested automatically.

This typically happens when current selection in a file changes, but can
also be triggered when file content changes."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "The reason why code actions were requested.

@since 3.17.0"
  :since
  "3.17.0")

(lem-lsp-base/type:define-enum file-operation-pattern-kind
    ((file "file" :documentation "The pattern matches a file only.")
     (folder "folder" :documentation "The pattern matches a folder only."))
  (:type lem-lsp-base/type:lsp-string)
  :since
  "A pattern kind describing if a glob pattern matches a file a folder or
both.

@since 3.16.0"
  :since
  "3.16.0")

(lem-lsp-base/type:define-enum notebook-cell-kind
    ((markup 1 :documentation "A markup-cell is formatted source that is used for display.")
     (code 2 :documentation "A code-cell is source code."))
  (:type lem-lsp-base/type:lsp-uinteger)
  :since
  "A notebook cell kind.

@since 3.17.0"
  :since
  "3.17.0")

(lem-lsp-base/type:define-enum resource-operation-kind
    ((create "create" :documentation "Supports creating new files and folders.")
     (rename "rename" :documentation "Supports renaming existing files and folders.")
     (delete "delete" :documentation "Supports deleting existing files and folders."))
  (:type lem-lsp-base/type:lsp-string))

(lem-lsp-base/type:define-enum failure-handling-kind
    ((abort "abort" :documentation
      "Applying the workspace change is simply aborted if one of the changes provided
fails. All operations executed before the failing operation stay executed.")
     (transactional "transactional" :documentation
      "All operations are executed transactional. That means they either all
succeed or no changes at all are applied to the workspace.")
     (text-only-transactional "textOnlyTransactional" :documentation
      "If the workspace edit contains only textual file changes they are executed transactional.
If resource changes (create, rename or delete file) are part of the change the failure
handling strategy is abort.")
     (undo "undo" :documentation
      "The client tries to undo the operations already executed. But there is no
guarantee that this is succeeding."))
  (:type lem-lsp-base/type:lsp-string))

(lem-lsp-base/type:define-enum prepare-support-default-behavior
    ((identifier 1 :documentation "The client's default behavior is to select the identifier
according the to language's syntax rule."))
  (:type lem-lsp-base/type:lsp-uinteger))

(lem-lsp-base/type:define-enum token-format
    ((relative "relative"))
  (:type lem-lsp-base/type:lsp-string))

(lem-lsp-base/type:define-class implementation-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class location
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor location-uri)
   (range :type range :initarg :range :accessor location-range))
  (:documentation "Represents a location inside a resource, such as a line
inside a text file."))

(lem-lsp-base/type:define-class implementation-registration-options
    (text-document-registration-options implementation-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class type-definition-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class type-definition-registration-options
    (text-document-registration-options type-definition-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class workspace-folder
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-uri :initarg :uri :accessor workspace-folder-uri :documentation
    "The associated URI for this workspace folder.")
   (name :type lem-lsp-base/type:lsp-string :initarg :name :accessor workspace-folder-name
    :documentation "The name of the workspace folder. Used to refer to this
workspace folder in the user interface."))
  (:documentation "A workspace folder inside a client."))

(lem-lsp-base/type:define-class did-change-workspace-folders-params
    common-lisp:nil
  ((event :type workspace-folders-change-event :initarg :event :accessor
    did-change-workspace-folders-params-event :documentation
    "The actual workspace folder change event."))
  (:documentation "The parameters of a `workspace/didChangeWorkspaceFolders` notification."))

(lem-lsp-base/type:define-class configuration-params
    common-lisp:nil
  ((items :type (lem-lsp-base/type:lsp-array configuration-item) :initarg :items :accessor
    configuration-params-items))
  (:documentation "The parameters of a configuration request."))

(lem-lsp-base/type:define-class document-color-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-color-params-text-document :documentation "The text document."))
  (:documentation "Parameters for a {@link DocumentColorRequest}."))

(lem-lsp-base/type:define-class color-information
    common-lisp:nil
  ((range :type range :initarg :range :accessor color-information-range :documentation
    "The range in the document where this color appears.")
   (color :type color :initarg :color :accessor color-information-color :documentation
    "The actual color value for this color range."))
  (:documentation "Represents a color range from a document."))

(lem-lsp-base/type:define-class document-color-registration-options
    (text-document-registration-options document-color-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class color-presentation-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    color-presentation-params-text-document :documentation "The text document.")
   (color :type color :initarg :color :accessor color-presentation-params-color :documentation
    "The color to request presentations for.")
   (range :type range :initarg :range :accessor color-presentation-params-range :documentation
    "The range where the color would be inserted. Serves as a context."))
  (:documentation "Parameters for a {@link ColorPresentationRequest}."))

(lem-lsp-base/type:define-class color-presentation
    common-lisp:nil
  ((label :type lem-lsp-base/type:lsp-string :initarg :label :accessor color-presentation-label
    :documentation "The label of this color presentation. It will be shown on the color
picker header. By default this is also the text that is inserted when selecting
this color presentation.")
   (text-edit :type text-edit :initarg :text-edit :accessor color-presentation-text-edit :optional
    common-lisp:t :documentation
    "An {@link TextEdit edit} which is applied to a document when selecting
this presentation for the color.  When `falsy` the {@link ColorPresentation.label label}
is used.")
   (additional-text-edits :type (lem-lsp-base/type:lsp-array text-edit) :initarg
    :additional-text-edits :accessor color-presentation-additional-text-edits :optional
    common-lisp:t :documentation
    "An optional array of additional {@link TextEdit text edits} that are applied when
selecting this color presentation. Edits must not overlap with the main {@link ColorPresentation.textEdit edit} nor with themselves.")))

(lem-lsp-base/type:define-class work-done-progress-options
    common-lisp:nil
  ((work-done-progress :type lem-lsp-base/type:lsp-boolean :initarg :work-done-progress :accessor
    work-done-progress-options-work-done-progress :optional common-lisp:t)))

(lem-lsp-base/type:define-class text-document-registration-options
    common-lisp:nil
  ((document-selector :type (common-lisp:or document-selector lem-lsp-base/type:lsp-null) :initarg
    :document-selector :accessor text-document-registration-options-document-selector
    :documentation "A document selector to identify the scope of the registration. If set to null
the document selector provided on the client side will be used."))
  (:documentation "General text document registration options."))

(lem-lsp-base/type:define-class folding-range-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    folding-range-params-text-document :documentation "The text document."))
  (:documentation "Parameters for a {@link FoldingRangeRequest}."))

(lem-lsp-base/type:define-class folding-range
    common-lisp:nil
  ((start-line :type lem-lsp-base/type:lsp-uinteger :initarg :start-line :accessor
    folding-range-start-line :documentation
    "The zero-based start line of the range to fold. The folded area starts after the line's last character.
To be valid, the end must be zero or larger and smaller than the number of lines in the document.")
   (start-character :type lem-lsp-base/type:lsp-uinteger :initarg :start-character :accessor
    folding-range-start-character :optional common-lisp:t :documentation
    "The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.")
   (end-line :type lem-lsp-base/type:lsp-uinteger :initarg :end-line :accessor
    folding-range-end-line :documentation
    "The zero-based end line of the range to fold. The folded area ends with the line's last character.
To be valid, the end must be zero or larger and smaller than the number of lines in the document.")
   (end-character :type lem-lsp-base/type:lsp-uinteger :initarg :end-character :accessor
    folding-range-end-character :optional common-lisp:t :documentation
    "The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.")
   (kind :type folding-range-kind :initarg :kind :accessor folding-range-kind :optional
    common-lisp:t :documentation
    "Describes the kind of the folding range such as `comment' or 'region'. The kind
is used to categorize folding ranges and used by commands like 'Fold all comments'.
See {@link FoldingRangeKind} for an enumeration of standardized kinds.")
   (collapsed-text :type lem-lsp-base/type:lsp-string :initarg :collapsed-text :accessor
    folding-range-collapsed-text :optional common-lisp:t :since "3.17.0" :documentation
    "The text that the client should show when the specified range is
collapsed. If not defined or not supported by the client, a default
will be chosen by the client.

@since 3.17.0"))
  (:documentation
   "Represents a folding range. To be valid, start and end line must be bigger than zero and smaller
than the number of lines in the document. Clients are free to ignore invalid ranges."))

(lem-lsp-base/type:define-class folding-range-registration-options
    (text-document-registration-options folding-range-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class declaration-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class declaration-registration-options
    (declaration-options text-document-registration-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class selection-range-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    selection-range-params-text-document :documentation "The text document.")
   (positions :type (lem-lsp-base/type:lsp-array position) :initarg :positions :accessor
    selection-range-params-positions :documentation "The positions inside the text document."))
  (:documentation "A parameter literal used in selection range requests."))

(lem-lsp-base/type:define-class selection-range
    common-lisp:nil
  ((range :type range :initarg :range :accessor selection-range-range :documentation
    "The {@link Range range} of this selection range.")
   (parent :type selection-range :initarg :parent :accessor selection-range-parent :optional
    common-lisp:t :documentation
    "The parent selection range containing this range. Therefore `parent.range` must contain `this.range`."))
  (:documentation "A selection range represents a part of a selection hierarchy. A selection range
may have a parent selection range that contains it."))

(lem-lsp-base/type:define-class selection-range-registration-options
    (selection-range-options text-document-registration-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class work-done-progress-create-params
    common-lisp:nil
  ((token :type progress-token :initarg :token :accessor work-done-progress-create-params-token
    :documentation "The token to be used to report progress.")))

(lem-lsp-base/type:define-class work-done-progress-cancel-params
    common-lisp:nil
  ((token :type progress-token :initarg :token :accessor work-done-progress-cancel-params-token
    :documentation "The token to be used to report progress.")))

(lem-lsp-base/type:define-class call-hierarchy-prepare-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "The parameter of a `textDocument/prepareCallHierarchy` request.

@since 3.16.0"))

(lem-lsp-base/type:define-class call-hierarchy-item
    common-lisp:nil
  ((name :type lem-lsp-base/type:lsp-string :initarg :name :accessor call-hierarchy-item-name
    :documentation "The name of this item.")
   (kind :type symbol-kind :initarg :kind :accessor call-hierarchy-item-kind :documentation
    "The kind of this item.")
   (tags :type (lem-lsp-base/type:lsp-array symbol-tag) :initarg :tags :accessor
    call-hierarchy-item-tags :optional common-lisp:t :documentation "Tags for this item.")
   (detail :type lem-lsp-base/type:lsp-string :initarg :detail :accessor call-hierarchy-item-detail
    :optional common-lisp:t :documentation
    "More detail for this item, e.g. the signature of a function.")
   (uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor call-hierarchy-item-uri
    :documentation "The resource identifier of this item.")
   (range :type range :initarg :range :accessor call-hierarchy-item-range :documentation
    "The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code.")
   (selection-range :type range :initarg :selection-range :accessor
    call-hierarchy-item-selection-range :documentation
    "The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.
Must be contained by the {@link CallHierarchyItem.range `range`}.")
   (data :type lsp-any :initarg :data :accessor call-hierarchy-item-data :optional common-lisp:t
    :documentation "A data entry field that is preserved between a call hierarchy prepare and
incoming calls or outgoing calls requests."))
  (:since "3.16.0")
  (:documentation "Represents programming constructs like functions or constructors in the context
of call hierarchy.

@since 3.16.0"))

(lem-lsp-base/type:define-class call-hierarchy-registration-options
    (text-document-registration-options call-hierarchy-options static-registration-options)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "Call hierarchy options used during static or dynamic registration.

@since 3.16.0"))

(lem-lsp-base/type:define-class call-hierarchy-incoming-calls-params
    (work-done-progress-params partial-result-params)
  ((item :type call-hierarchy-item :initarg :item :accessor
    call-hierarchy-incoming-calls-params-item))
  (:since "3.16.0")
  (:documentation "The parameter of a `callHierarchy/incomingCalls` request.

@since 3.16.0"))

(lem-lsp-base/type:define-class call-hierarchy-incoming-call
    common-lisp:nil
  ((from :type call-hierarchy-item :initarg :from :accessor call-hierarchy-incoming-call-from
    :documentation "The item that makes the call.")
   (from-ranges :type (lem-lsp-base/type:lsp-array range) :initarg :from-ranges :accessor
    call-hierarchy-incoming-call-from-ranges :documentation
    "The ranges at which the calls appear. This is relative to the caller
denoted by {@link CallHierarchyIncomingCall.from `this.from`}."))
  (:since "3.16.0")
  (:documentation "Represents an incoming call, e.g. a caller of a method or constructor.

@since 3.16.0"))

(lem-lsp-base/type:define-class call-hierarchy-outgoing-calls-params
    (work-done-progress-params partial-result-params)
  ((item :type call-hierarchy-item :initarg :item :accessor
    call-hierarchy-outgoing-calls-params-item))
  (:since "3.16.0")
  (:documentation "The parameter of a `callHierarchy/outgoingCalls` request.

@since 3.16.0"))

(lem-lsp-base/type:define-class call-hierarchy-outgoing-call
    common-lisp:nil
  ((to :type call-hierarchy-item :initarg :to :accessor call-hierarchy-outgoing-call-to
    :documentation "The item that is called.")
   (from-ranges :type (lem-lsp-base/type:lsp-array range) :initarg :from-ranges :accessor
    call-hierarchy-outgoing-call-from-ranges :documentation
    "The range at which this item is called. This is the range relative to the caller, e.g the item
passed to {@link CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls `provideCallHierarchyOutgoingCalls`}
and not {@link CallHierarchyOutgoingCall.to `this.to`}."))
  (:since "3.16.0")
  (:documentation
   "Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.

@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    semantic-tokens-params-text-document :documentation "The text document."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens
    common-lisp:nil
  ((result-id :type lem-lsp-base/type:lsp-string :initarg :result-id :accessor
    semantic-tokens-result-id :optional common-lisp:t :documentation
    "An optional result id. If provided and clients support delta updating
the client will include the result id in the next semantic token request.
A server can then instead of computing all semantic tokens again simply
send a delta.")
   (data :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-uinteger) :initarg :data
    :accessor semantic-tokens-data :documentation "The actual tokens."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-partial-result
    common-lisp:nil
  ((data :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-uinteger) :initarg :data
    :accessor semantic-tokens-partial-result-data))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-registration-options
    (text-document-registration-options semantic-tokens-options static-registration-options)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-delta-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    semantic-tokens-delta-params-text-document :documentation "The text document.")
   (previous-result-id :type lem-lsp-base/type:lsp-string :initarg :previous-result-id :accessor
    semantic-tokens-delta-params-previous-result-id :documentation
    "The result id of a previous response. The result Id can either point to a full response
or a delta response depending on what was received last."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-delta
    common-lisp:nil
  ((result-id :type lem-lsp-base/type:lsp-string :initarg :result-id :accessor
    semantic-tokens-delta-result-id :optional common-lisp:t)
   (edits :type (lem-lsp-base/type:lsp-array semantic-tokens-edit) :initarg :edits :accessor
    semantic-tokens-delta-edits :documentation
    "The semantic token edits to transform a previous result into a new result."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-delta-partial-result
    common-lisp:nil
  ((edits :type (lem-lsp-base/type:lsp-array semantic-tokens-edit) :initarg :edits :accessor
    semantic-tokens-delta-partial-result-edits))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-range-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    semantic-tokens-range-params-text-document :documentation "The text document.")
   (range :type range :initarg :range :accessor semantic-tokens-range-params-range :documentation
    "The range the semantic tokens are requested for."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class show-document-params
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-uri :initarg :uri :accessor show-document-params-uri
    :documentation "The uri to show.")
   (external :type lem-lsp-base/type:lsp-boolean :initarg :external :accessor
    show-document-params-external :optional common-lisp:t :documentation
    "Indicates to show the resource in an external program.
To show, for example, `https://code.visualstudio.com/`
in the default WEB browser set `external` to `true`.")
   (take-focus :type lem-lsp-base/type:lsp-boolean :initarg :take-focus :accessor
    show-document-params-take-focus :optional common-lisp:t :documentation
    "An optional property to indicate whether the editor
showing the document should take focus or not.
Clients might ignore this property if an external
program is started.")
   (selection :type range :initarg :selection :accessor show-document-params-selection :optional
    common-lisp:t :documentation "An optional selection range if the document is a text
document. Clients might ignore the property if an
external program is started or the file is not a text
file."))
  (:since "3.16.0")
  (:documentation "Params to show a resource in the UI.

@since 3.16.0"))

(lem-lsp-base/type:define-class show-document-result
    common-lisp:nil
  ((success :type lem-lsp-base/type:lsp-boolean :initarg :success :accessor
    show-document-result-success :documentation "A boolean indicating if the show was successful."))
  (:since "3.16.0")
  (:documentation "The result of a showDocument request.

@since 3.16.0"))

(lem-lsp-base/type:define-class linked-editing-range-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class linked-editing-ranges
    common-lisp:nil
  ((ranges :type (lem-lsp-base/type:lsp-array range) :initarg :ranges :accessor
    linked-editing-ranges-ranges :documentation
    "A list of ranges that can be edited together. The ranges must have
identical length and contain identical text content. The ranges cannot overlap.")
   (word-pattern :type lem-lsp-base/type:lsp-string :initarg :word-pattern :accessor
    linked-editing-ranges-word-pattern :optional common-lisp:t :documentation
    "An optional word pattern (regular expression) that describes valid contents for
the given ranges. If no pattern is provided, the client configuration's word
pattern will be used."))
  (:since "3.16.0")
  (:documentation "The result of a linked editing range request.

@since 3.16.0"))

(lem-lsp-base/type:define-class linked-editing-range-registration-options
    (text-document-registration-options linked-editing-range-options static-registration-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class create-files-params
    common-lisp:nil
  ((files :type (lem-lsp-base/type:lsp-array file-create) :initarg :files :accessor
    create-files-params-files :documentation
    "An array of all files/folders created in this operation."))
  (:since "3.16.0")
  (:documentation "The parameters sent in notifications/requests for user-initiated creation of
files.

@since 3.16.0"))

(lem-lsp-base/type:define-class workspace-edit
    common-lisp:nil
  ((changes :type
    (lem-lsp-base/type:lsp-map lem-lsp-base/type:lsp-document-uri
     (lem-lsp-base/type:lsp-array text-edit))
    :initarg :changes :accessor workspace-edit-changes :optional common-lisp:t :documentation
    "Holds changes to existing resources.")
   (document-changes :type
    (lem-lsp-base/type:lsp-array
     (common-lisp:or text-document-edit create-file rename-file delete-file))
    :initarg :document-changes :accessor workspace-edit-document-changes :optional common-lisp:t
    :documentation
    "Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
are either an array of `TextDocumentEdit`s to express changes to n different text documents
where each text document edit addresses a specific version of a text document. Or it can contain
above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.

Whether a client supports versioned document edits is expressed via
`workspace.workspaceEdit.documentChanges` client capability.

If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
only plain `TextEdit`s using the `changes` property are supported.")
   (change-annotations :type
    (lem-lsp-base/type:lsp-map change-annotation-identifier change-annotation) :initarg
    :change-annotations :accessor workspace-edit-change-annotations :optional common-lisp:t :since
    "3.16.0" :documentation
    "A map of change annotations that can be referenced in `AnnotatedTextEdit`s or create, rename and
delete file / folder operations.

Whether clients honor this property depends on the client capability `workspace.changeAnnotationSupport`.

@since 3.16.0"))
  (:documentation
   "A workspace edit represents changes to many resources managed in the workspace. The edit
should either provide `changes` or `documentChanges`. If documentChanges are present
they are preferred over `changes` if the client can handle versioned document edits.

Since version 3.13.0 a workspace edit can contain resource operations as well. If resource
operations are present clients need to execute the operations in the order in which they
are provided. So a workspace edit for example can consist of the following two changes:
(1) a create file a.txt and (2) a text document edit which insert text into file a.txt.

An invalid sequence (e.g. (1) delete file a.txt and (2) insert text into file a.txt) will
cause failure of the operation. How the client recovers from the failure is described by
the client capability: `workspace.workspaceEdit.failureHandling`"))

(lem-lsp-base/type:define-class file-operation-registration-options
    common-lisp:nil
  ((filters :type (lem-lsp-base/type:lsp-array file-operation-filter) :initarg :filters :accessor
    file-operation-registration-options-filters :documentation "The actual filters."))
  (:since "3.16.0")
  (:documentation "The options to register for file operations.

@since 3.16.0"))

(lem-lsp-base/type:define-class rename-files-params
    common-lisp:nil
  ((files :type (lem-lsp-base/type:lsp-array file-rename) :initarg :files :accessor
    rename-files-params-files :documentation
    "An array of all files/folders renamed in this operation. When a folder is renamed, only
the folder will be included, and not its children."))
  (:since "3.16.0")
  (:documentation "The parameters sent in notifications/requests for user-initiated renames of
files.

@since 3.16.0"))

(lem-lsp-base/type:define-class delete-files-params
    common-lisp:nil
  ((files :type (lem-lsp-base/type:lsp-array file-delete) :initarg :files :accessor
    delete-files-params-files :documentation
    "An array of all files/folders deleted in this operation."))
  (:since "3.16.0")
  (:documentation "The parameters sent in notifications/requests for user-initiated deletes of
files.

@since 3.16.0"))

(lem-lsp-base/type:define-class moniker-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class moniker
    common-lisp:nil
  ((scheme :type lem-lsp-base/type:lsp-string :initarg :scheme :accessor moniker-scheme
    :documentation "The scheme of the moniker. For example tsc or .Net")
   (identifier :type lem-lsp-base/type:lsp-string :initarg :identifier :accessor moniker-identifier
    :documentation "The identifier of the moniker. The value is opaque in LSIF however
schema owners are allowed to define the structure if they want.")
   (unique :type uniqueness-level :initarg :unique :accessor moniker-unique :documentation
    "The scope in which the moniker is unique")
   (kind :type moniker-kind :initarg :kind :accessor moniker-kind :optional common-lisp:t
    :documentation "The moniker kind if known."))
  (:since "3.16.0")
  (:documentation "Moniker definition to match LSIF 0.5 moniker definition.

@since 3.16.0"))

(lem-lsp-base/type:define-class moniker-registration-options
    (text-document-registration-options moniker-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class type-hierarchy-prepare-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "The parameter of a `textDocument/prepareTypeHierarchy` request.

@since 3.17.0"))

(lem-lsp-base/type:define-class type-hierarchy-item
    common-lisp:nil
  ((name :type lem-lsp-base/type:lsp-string :initarg :name :accessor type-hierarchy-item-name
    :documentation "The name of this item.")
   (kind :type symbol-kind :initarg :kind :accessor type-hierarchy-item-kind :documentation
    "The kind of this item.")
   (tags :type (lem-lsp-base/type:lsp-array symbol-tag) :initarg :tags :accessor
    type-hierarchy-item-tags :optional common-lisp:t :documentation "Tags for this item.")
   (detail :type lem-lsp-base/type:lsp-string :initarg :detail :accessor type-hierarchy-item-detail
    :optional common-lisp:t :documentation
    "More detail for this item, e.g. the signature of a function.")
   (uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor type-hierarchy-item-uri
    :documentation "The resource identifier of this item.")
   (range :type range :initarg :range :accessor type-hierarchy-item-range :documentation
    "The range enclosing this symbol not including leading/trailing whitespace
but everything else, e.g. comments and code.")
   (selection-range :type range :initarg :selection-range :accessor
    type-hierarchy-item-selection-range :documentation
    "The range that should be selected and revealed when this symbol is being
picked, e.g. the name of a function. Must be contained by the
{@link TypeHierarchyItem.range `range`}.")
   (data :type lsp-any :initarg :data :accessor type-hierarchy-item-data :optional common-lisp:t
    :documentation "A data entry field that is preserved between a type hierarchy prepare and
supertypes or subtypes requests. It could also be used to identify the
type hierarchy in the server, helping improve the performance on
resolving supertypes and subtypes."))
  (:since "3.17.0")
  (:documentation "@since 3.17.0"))

(lem-lsp-base/type:define-class type-hierarchy-registration-options
    (text-document-registration-options type-hierarchy-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Type hierarchy options used during static or dynamic registration.

@since 3.17.0"))

(lem-lsp-base/type:define-class type-hierarchy-supertypes-params
    (work-done-progress-params partial-result-params)
  ((item :type type-hierarchy-item :initarg :item :accessor type-hierarchy-supertypes-params-item))
  (:since "3.17.0")
  (:documentation "The parameter of a `typeHierarchy/supertypes` request.

@since 3.17.0"))

(lem-lsp-base/type:define-class type-hierarchy-subtypes-params
    (work-done-progress-params partial-result-params)
  ((item :type type-hierarchy-item :initarg :item :accessor type-hierarchy-subtypes-params-item))
  (:since "3.17.0")
  (:documentation "The parameter of a `typeHierarchy/subtypes` request.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-params
    (work-done-progress-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    inline-value-params-text-document :documentation "The text document.")
   (range :type range :initarg :range :accessor inline-value-params-range :documentation
    "The document range for which inline values should be computed.")
   (context :type inline-value-context :initarg :context :accessor inline-value-params-context
    :documentation "Additional information about the context in which inline values were
requested."))
  (:since "3.17.0")
  (:documentation "A parameter literal used in inline value requests.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-registration-options
    (inline-value-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Inline value options used during static or dynamic registration.

@since 3.17.0"))

(lem-lsp-base/type:define-class inlay-hint-params
    (work-done-progress-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    inlay-hint-params-text-document :documentation "The text document.")
   (range :type range :initarg :range :accessor inlay-hint-params-range :documentation
    "The document range for which inlay hints should be computed."))
  (:since "3.17.0")
  (:documentation "A parameter literal used in inlay hint requests.

@since 3.17.0"))

(lem-lsp-base/type:define-class inlay-hint
    common-lisp:nil
  ((position :type position :initarg :position :accessor inlay-hint-position :documentation
    "The position of this hint.

If multiple hints have the same position, they will be shown in the order
they appear in the response.")
   (label :type
    (common-lisp:or lem-lsp-base/type:lsp-string
                    (lem-lsp-base/type:lsp-array inlay-hint-label-part))
    :initarg :label :accessor inlay-hint-label :documentation
    "The label of this hint. A human readable string or an array of
InlayHintLabelPart label parts.

*Note* that neither the string nor the label part can be empty.")
   (kind :type inlay-hint-kind :initarg :kind :accessor inlay-hint-kind :optional common-lisp:t
    :documentation "The kind of this hint. Can be omitted in which case the client
should fall back to a reasonable default.")
   (text-edits :type (lem-lsp-base/type:lsp-array text-edit) :initarg :text-edits :accessor
    inlay-hint-text-edits :optional common-lisp:t :documentation
    "Optional text edits that are performed when accepting this inlay hint.

*Note* that edits are expected to change the document so that the inlay
hint (or its nearest variant) is now part of the document and the inlay
hint itself is now obsolete.")
   (tooltip :type (common-lisp:or lem-lsp-base/type:lsp-string markup-content) :initarg :tooltip
    :accessor inlay-hint-tooltip :optional common-lisp:t :documentation
    "The tooltip text when you hover over this item.")
   (padding-left :type lem-lsp-base/type:lsp-boolean :initarg :padding-left :accessor
    inlay-hint-padding-left :optional common-lisp:t :documentation "Render padding before the hint.

Note: Padding should use the editor's background color, not the
background color of the hint itself. That means padding can be used
to visually align/separate an inlay hint.")
   (padding-right :type lem-lsp-base/type:lsp-boolean :initarg :padding-right :accessor
    inlay-hint-padding-right :optional common-lisp:t :documentation "Render padding after the hint.

Note: Padding should use the editor's background color, not the
background color of the hint itself. That means padding can be used
to visually align/separate an inlay hint.")
   (data :type lsp-any :initarg :data :accessor inlay-hint-data :optional common-lisp:t
    :documentation "A data entry field that is preserved on an inlay hint between
a `textDocument/inlayHint` and a `inlayHint/resolve` request."))
  (:since "3.17.0")
  (:documentation "Inlay hint information.

@since 3.17.0"))

(lem-lsp-base/type:define-class inlay-hint-registration-options
    (inlay-hint-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Inlay hint options used during static or dynamic registration.

@since 3.17.0"))

(lem-lsp-base/type:define-class document-diagnostic-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-diagnostic-params-text-document :documentation "The text document.")
   (identifier :type lem-lsp-base/type:lsp-string :initarg :identifier :accessor
    document-diagnostic-params-identifier :optional common-lisp:t :documentation
    "The additional identifier  provided during registration.")
   (previous-result-id :type lem-lsp-base/type:lsp-string :initarg :previous-result-id :accessor
    document-diagnostic-params-previous-result-id :optional common-lisp:t :documentation
    "The result id of a previous response if provided."))
  (:since "3.17.0")
  (:documentation "Parameters of the document diagnostic request.

@since 3.17.0"))

(lem-lsp-base/type:define-class document-diagnostic-report-partial-result
    common-lisp:nil
  ((related-documents :type
    (lem-lsp-base/type:lsp-map lem-lsp-base/type:lsp-document-uri
     (common-lisp:or full-document-diagnostic-report unchanged-document-diagnostic-report))
    :initarg :related-documents :accessor
    document-diagnostic-report-partial-result-related-documents))
  (:since "3.17.0")
  (:documentation "A partial result for a document diagnostic report.

@since 3.17.0"))

(lem-lsp-base/type:define-class diagnostic-server-cancellation-data
    common-lisp:nil
  ((retrigger-request :type lem-lsp-base/type:lsp-boolean :initarg :retrigger-request :accessor
    diagnostic-server-cancellation-data-retrigger-request))
  (:since "3.17.0")
  (:documentation "Cancellation data returned from a diagnostic request.

@since 3.17.0"))

(lem-lsp-base/type:define-class diagnostic-registration-options
    (text-document-registration-options diagnostic-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Diagnostic registration options.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-diagnostic-params
    (work-done-progress-params partial-result-params)
  ((identifier :type lem-lsp-base/type:lsp-string :initarg :identifier :accessor
    workspace-diagnostic-params-identifier :optional common-lisp:t :documentation
    "The additional identifier provided during registration.")
   (previous-result-ids :type (lem-lsp-base/type:lsp-array previous-result-id) :initarg
    :previous-result-ids :accessor workspace-diagnostic-params-previous-result-ids :documentation
    "The currently known diagnostic reports with their
previous result ids."))
  (:since "3.17.0")
  (:documentation "Parameters of the workspace diagnostic request.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-diagnostic-report
    common-lisp:nil
  ((items :type (lem-lsp-base/type:lsp-array workspace-document-diagnostic-report) :initarg :items
    :accessor workspace-diagnostic-report-items))
  (:since "3.17.0")
  (:documentation "A workspace diagnostic report.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-diagnostic-report-partial-result
    common-lisp:nil
  ((items :type (lem-lsp-base/type:lsp-array workspace-document-diagnostic-report) :initarg :items
    :accessor workspace-diagnostic-report-partial-result-items))
  (:since "3.17.0")
  (:documentation "A partial result for a workspace diagnostic report.

@since 3.17.0"))

(lem-lsp-base/type:define-class did-open-notebook-document-params
    common-lisp:nil
  ((notebook-document :type notebook-document :initarg :notebook-document :accessor
    did-open-notebook-document-params-notebook-document :documentation
    "The notebook document that got opened.")
   (cell-text-documents :type (lem-lsp-base/type:lsp-array text-document-item) :initarg
    :cell-text-documents :accessor did-open-notebook-document-params-cell-text-documents
    :documentation "The text documents that represent the content
of a notebook cell."))
  (:since "3.17.0")
  (:documentation "The params sent in an open notebook document notification.

@since 3.17.0"))

(lem-lsp-base/type:define-class did-change-notebook-document-params
    common-lisp:nil
  ((notebook-document :type versioned-notebook-document-identifier :initarg :notebook-document
    :accessor did-change-notebook-document-params-notebook-document :documentation
    "The notebook document that did change. The version number points
to the version after all provided changes have been applied. If
only the text document content of a cell changes the notebook version
doesn't necessarily have to change.")
   (change :type notebook-document-change-event :initarg :change :accessor
    did-change-notebook-document-params-change :documentation
    "The actual changes to the notebook document.

The changes describe single state changes to the notebook document.
So if there are two changes c1 (at array index 0) and c2 (at array
index 1) for a notebook in state S then c1 moves the notebook from
S to S' and c2 from S' to S''. So c1 is computed on the state S and
c2 is computed on the state S'.

To mirror the content of a notebook using change events use the following approach:
- start with the same initial content
- apply the 'notebookDocument/didChange' notifications in the order you receive them.
- apply the `NotebookChangeEvent`s in a single notification in the order
  you receive them."))
  (:since "3.17.0")
  (:documentation "The params sent in a change notebook document notification.

@since 3.17.0"))

(lem-lsp-base/type:define-class did-save-notebook-document-params
    common-lisp:nil
  ((notebook-document :type notebook-document-identifier :initarg :notebook-document :accessor
    did-save-notebook-document-params-notebook-document :documentation
    "The notebook document that got saved."))
  (:since "3.17.0")
  (:documentation "The params sent in a save notebook document notification.

@since 3.17.0"))

(lem-lsp-base/type:define-class did-close-notebook-document-params
    common-lisp:nil
  ((notebook-document :type notebook-document-identifier :initarg :notebook-document :accessor
    did-close-notebook-document-params-notebook-document :documentation
    "The notebook document that got closed.")
   (cell-text-documents :type (lem-lsp-base/type:lsp-array text-document-identifier) :initarg
    :cell-text-documents :accessor did-close-notebook-document-params-cell-text-documents
    :documentation "The text documents that represent the content
of a notebook cell that got closed."))
  (:since "3.17.0")
  (:documentation "The params sent in a close notebook document notification.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-completion-params
    (text-document-position-params work-done-progress-params)
  ((context :type inline-completion-context :initarg :context :accessor
    inline-completion-params-context :documentation
    "Additional information about the context in which inline completions were
requested."))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "A parameter literal used in inline completion requests.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class inline-completion-list
    common-lisp:nil
  ((items :type (lem-lsp-base/type:lsp-array inline-completion-item) :initarg :items :accessor
    inline-completion-list-items :documentation "The inline completion items"))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation
   "Represents a collection of {@link InlineCompletionItem inline completion items} to be presented in the editor.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class inline-completion-item
    common-lisp:nil
  ((insert-text :type (common-lisp:or lem-lsp-base/type:lsp-string string-value) :initarg
    :insert-text :accessor inline-completion-item-insert-text :documentation
    "The text to replace the range with. Must be set.")
   (filter-text :type lem-lsp-base/type:lsp-string :initarg :filter-text :accessor
    inline-completion-item-filter-text :optional common-lisp:t :documentation
    "A text that is used to decide if this inline completion should be shown. When `falsy` the {@link InlineCompletionItem.insertText} is used.")
   (range :type range :initarg :range :accessor inline-completion-item-range :optional
    common-lisp:t :documentation "The range to replace. Must begin and end on the same line.")
   (command :type command :initarg :command :accessor inline-completion-item-command :optional
    common-lisp:t :documentation
    "An optional {@link Command} that is executed *after* inserting this completion."))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation
   "An inline completion item represents a text snippet that is proposed inline to complete text that is being typed.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class inline-completion-registration-options
    (inline-completion-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "Inline completion options used during static or dynamic registration.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class registration-params
    common-lisp:nil
  ((registrations :type (lem-lsp-base/type:lsp-array registration) :initarg :registrations
    :accessor registration-params-registrations)))

(lem-lsp-base/type:define-class unregistration-params
    common-lisp:nil
  ((unregisterations :type (lem-lsp-base/type:lsp-array unregistration) :initarg :unregisterations
    :accessor unregistration-params-unregisterations)))

(lem-lsp-base/type:define-class initialize-params
    (_initialize-params workspace-folders-initialize-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class initialize-result
    common-lisp:nil
  ((capabilities :type server-capabilities :initarg :capabilities :accessor
    initialize-result-capabilities :documentation "The capabilities the language server provides.")
   (server-info :type
    (lem-lsp-base/type:lsp-interface
     ((name :type lem-lsp-base/type:lsp-string :documentation
       "The name of the server as defined by the server.")
      (version :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
       "The server's version as defined by the server.")))
    :initarg :server-info :accessor initialize-result-server-info :optional common-lisp:t :since
    "3.15.0" :documentation "Information about the server.

@since 3.15.0"))
  (:documentation "The result returned from an initialize request."))

(lem-lsp-base/type:define-class initialize-error
    common-lisp:nil
  ((retry :type lem-lsp-base/type:lsp-boolean :initarg :retry :accessor initialize-error-retry
    :documentation "Indicates whether the client execute the following retry logic:
(1) show the message provided by the ResponseError to the user
(2) user selects retry or cancel
(3) if user selected retry the initialize method is sent again."))
  (:documentation "The data type of the ResponseError if the
initialize request fails."))

(lem-lsp-base/type:define-class initialized-params
    common-lisp:nil
  common-lisp:nil)

(lem-lsp-base/type:define-class did-change-configuration-params
    common-lisp:nil
  ((settings :type lsp-any :initarg :settings :accessor did-change-configuration-params-settings
    :documentation "The actual changed settings"))
  (:documentation "The parameters of a change configuration notification."))

(lem-lsp-base/type:define-class did-change-configuration-registration-options
    common-lisp:nil
  ((section :type
    (common-lisp:or lem-lsp-base/type:lsp-string
                    (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string))
    :initarg :section :accessor did-change-configuration-registration-options-section :optional
    common-lisp:t)))

(lem-lsp-base/type:define-class show-message-params
    common-lisp:nil
  ((type :type message-type :initarg :type :accessor show-message-params-type :documentation
    "The message type. See {@link MessageType}")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    show-message-params-message :documentation "The actual message."))
  (:documentation "The parameters of a notification message."))

(lem-lsp-base/type:define-class show-message-request-params
    common-lisp:nil
  ((type :type message-type :initarg :type :accessor show-message-request-params-type
    :documentation "The message type. See {@link MessageType}")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    show-message-request-params-message :documentation "The actual message.")
   (actions :type (lem-lsp-base/type:lsp-array message-action-item) :initarg :actions :accessor
    show-message-request-params-actions :optional common-lisp:t :documentation
    "The message action items to present.")))

(lem-lsp-base/type:define-class message-action-item
    common-lisp:nil
  ((title :type lem-lsp-base/type:lsp-string :initarg :title :accessor message-action-item-title
    :documentation "A short title like 'Retry', 'Open Log' etc.")))

(lem-lsp-base/type:define-class log-message-params
    common-lisp:nil
  ((type :type message-type :initarg :type :accessor log-message-params-type :documentation
    "The message type. See {@link MessageType}")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    log-message-params-message :documentation "The actual message."))
  (:documentation "The log message parameters."))

(lem-lsp-base/type:define-class did-open-text-document-params
    common-lisp:nil
  ((text-document :type text-document-item :initarg :text-document :accessor
    did-open-text-document-params-text-document :documentation "The document that was opened."))
  (:documentation "The parameters sent in an open text document notification"))

(lem-lsp-base/type:define-class did-change-text-document-params
    common-lisp:nil
  ((text-document :type versioned-text-document-identifier :initarg :text-document :accessor
    did-change-text-document-params-text-document :documentation
    "The document that did change. The version number points
to the version after all provided content changes have
been applied.")
   (content-changes :type (lem-lsp-base/type:lsp-array text-document-content-change-event) :initarg
    :content-changes :accessor did-change-text-document-params-content-changes :documentation
    "The actual content changes. The content changes describe single state changes
to the document. So if there are two content changes c1 (at array index 0) and
c2 (at array index 1) for a document in state S then c1 moves the document from
S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
on the state S'.

To mirror the content of a document using change events use the following approach:
- start with the same initial content
- apply the 'textDocument/didChange' notifications in the order you receive them.
- apply the `TextDocumentContentChangeEvent`s in a single notification in the order
  you receive them."))
  (:documentation "The change text document notification's parameters."))

(lem-lsp-base/type:define-class text-document-change-registration-options
    (text-document-registration-options)
  ((sync-kind :type text-document-sync-kind :initarg :sync-kind :accessor
    text-document-change-registration-options-sync-kind :documentation
    "How documents are synced to the server."))
  (:documentation "Describe options to be used when registered for text document change events."))

(lem-lsp-base/type:define-class did-close-text-document-params
    common-lisp:nil
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    did-close-text-document-params-text-document :documentation "The document that was closed."))
  (:documentation "The parameters sent in a close text document notification"))

(lem-lsp-base/type:define-class did-save-text-document-params
    common-lisp:nil
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    did-save-text-document-params-text-document :documentation "The document that was saved.")
   (text :type lem-lsp-base/type:lsp-string :initarg :text :accessor
    did-save-text-document-params-text :optional common-lisp:t :documentation
    "Optional the content when saved. Depends on the includeText value
when the save notification was requested."))
  (:documentation "The parameters sent in a save text document notification"))

(lem-lsp-base/type:define-class text-document-save-registration-options
    (text-document-registration-options save-options)
  common-lisp:nil
  (:documentation "Save registration options."))

(lem-lsp-base/type:define-class will-save-text-document-params
    common-lisp:nil
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    will-save-text-document-params-text-document :documentation "The document that will be saved.")
   (reason :type text-document-save-reason :initarg :reason :accessor
    will-save-text-document-params-reason :documentation "The 'TextDocumentSaveReason'."))
  (:documentation "The parameters sent in a will save text document notification."))

(lem-lsp-base/type:define-class text-edit
    common-lisp:nil
  ((range :type range :initarg :range :accessor text-edit-range :documentation
    "The range of the text document to be manipulated. To insert
text into a document create a range where start === end.")
   (new-text :type lem-lsp-base/type:lsp-string :initarg :new-text :accessor text-edit-new-text
    :documentation "The string to be inserted. For delete operations use an
empty string."))
  (:documentation "A text edit applicable to a text document."))

(lem-lsp-base/type:define-class did-change-watched-files-params
    common-lisp:nil
  ((changes :type (lem-lsp-base/type:lsp-array file-event) :initarg :changes :accessor
    did-change-watched-files-params-changes :documentation "The actual file events."))
  (:documentation "The watched files change notification's parameters."))

(lem-lsp-base/type:define-class did-change-watched-files-registration-options
    common-lisp:nil
  ((watchers :type (lem-lsp-base/type:lsp-array file-system-watcher) :initarg :watchers :accessor
    did-change-watched-files-registration-options-watchers :documentation
    "The watchers to register."))
  (:documentation "Describe options to be used when registered for text document change events."))

(lem-lsp-base/type:define-class publish-diagnostics-params
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor
    publish-diagnostics-params-uri :documentation
    "The URI for which diagnostic information is reported.")
   (version :type lem-lsp-base/type:lsp-integer :initarg :version :accessor
    publish-diagnostics-params-version :optional common-lisp:t :since "3.15.0" :documentation
    "Optional the version number of the document the diagnostics are published for.

@since 3.15.0")
   (diagnostics :type (lem-lsp-base/type:lsp-array diagnostic) :initarg :diagnostics :accessor
    publish-diagnostics-params-diagnostics :documentation
    "An array of diagnostic information items."))
  (:documentation "The publish diagnostic notification's parameters."))

(lem-lsp-base/type:define-class completion-params
    (text-document-position-params work-done-progress-params partial-result-params)
  ((context :type completion-context :initarg :context :accessor completion-params-context
    :optional common-lisp:t :documentation
    "The completion context. This is only available it the client specifies
to send this using the client capability `textDocument.completion.contextSupport === true`"))
  (:documentation "Completion parameters"))

(lem-lsp-base/type:define-class completion-item
    common-lisp:nil
  ((label :type lem-lsp-base/type:lsp-string :initarg :label :accessor completion-item-label
    :documentation "The label of this completion item.

The label property is also by default the text that
is inserted when selecting this completion.

If label details are provided the label itself should
be an unqualified name of the completion item.")
   (label-details :type completion-item-label-details :initarg :label-details :accessor
    completion-item-label-details :optional common-lisp:t :since "3.17.0" :documentation
    "Additional details for the label

@since 3.17.0")
   (kind :type completion-item-kind :initarg :kind :accessor completion-item-kind :optional
    common-lisp:t :documentation "The kind of this completion item. Based of the kind
an icon is chosen by the editor.")
   (tags :type (lem-lsp-base/type:lsp-array completion-item-tag) :initarg :tags :accessor
    completion-item-tags :optional common-lisp:t :since "3.15.0" :documentation
    "Tags for this completion item.

@since 3.15.0")
   (detail :type lem-lsp-base/type:lsp-string :initarg :detail :accessor completion-item-detail
    :optional common-lisp:t :documentation "A human-readable string with additional information
about this item, like type or symbol information.")
   (documentation :type (common-lisp:or lem-lsp-base/type:lsp-string markup-content) :initarg
    :documentation :accessor completion-item-documentation :optional common-lisp:t :documentation
    "A human-readable string that represents a doc-comment.")
   (deprecated :type lem-lsp-base/type:lsp-boolean :initarg :deprecated :accessor
    completion-item-deprecated :optional common-lisp:t :deprecated "Use `tags` instead."
    :documentation "Indicates if this item is deprecated.
@deprecated Use `tags` instead.")
   (preselect :type lem-lsp-base/type:lsp-boolean :initarg :preselect :accessor
    completion-item-preselect :optional common-lisp:t :documentation "Select this item when showing.

*Note* that only one completion item can be selected and that the
tool / client decides which item that is. The rule is that the *first*
item of those that match best is selected.")
   (sort-text :type lem-lsp-base/type:lsp-string :initarg :sort-text :accessor
    completion-item-sort-text :optional common-lisp:t :documentation
    "A string that should be used when comparing this item
with other items. When `falsy` the {@link CompletionItem.label label}
is used.")
   (filter-text :type lem-lsp-base/type:lsp-string :initarg :filter-text :accessor
    completion-item-filter-text :optional common-lisp:t :documentation
    "A string that should be used when filtering a set of
completion items. When `falsy` the {@link CompletionItem.label label}
is used.")
   (insert-text :type lem-lsp-base/type:lsp-string :initarg :insert-text :accessor
    completion-item-insert-text :optional common-lisp:t :documentation
    "A string that should be inserted into a document when selecting
this completion. When `falsy` the {@link CompletionItem.label label}
is used.

The `insertText` is subject to interpretation by the client side.
Some tools might not take the string literally. For example
VS Code when code complete is requested in this example
`con<cursor position>` and a completion item with an `insertText` of
`console` is provided it will only insert `sole`. Therefore it is
recommended to use `textEdit` instead since it avoids additional client
side interpretation.")
   (insert-text-format :type insert-text-format :initarg :insert-text-format :accessor
    completion-item-insert-text-format :optional common-lisp:t :documentation
    "The format of the insert text. The format applies to both the
`insertText` property and the `newText` property of a provided
`textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.

Please note that the insertTextFormat doesn't apply to
`additionalTextEdits`.")
   (insert-text-mode :type insert-text-mode :initarg :insert-text-mode :accessor
    completion-item-insert-text-mode :optional common-lisp:t :since "3.16.0" :documentation
    "How whitespace and indentation is handled during completion
item insertion. If not provided the clients default value depends on
the `textDocument.completion.insertTextMode` client capability.

@since 3.16.0")
   (text-edit :type (common-lisp:or text-edit insert-replace-edit) :initarg :text-edit :accessor
    completion-item-text-edit :optional common-lisp:t :since
    "3.16.0 additional type `InsertReplaceEdit`" :documentation
    "An {@link TextEdit edit} which is applied to a document when selecting
this completion. When an edit is provided the value of
{@link CompletionItem.insertText insertText} is ignored.

Most editors support two different operations when accepting a completion
item. One is to insert a completion text and the other is to replace an
existing text with a completion text. Since this can usually not be
predetermined by a server it can report both ranges. Clients need to
signal support for `InsertReplaceEdits` via the
`textDocument.completion.insertReplaceSupport` client capability
property.

*Note 1:* The text edit's range as well as both ranges from an insert
replace edit must be a [single line] and they must contain the position
at which completion has been requested.
*Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
must be a prefix of the edit's replace range, that means it must be
contained and starting at the same position.

@since 3.16.0 additional type `InsertReplaceEdit`")
   (text-edit-text :type lem-lsp-base/type:lsp-string :initarg :text-edit-text :accessor
    completion-item-text-edit-text :optional common-lisp:t :since "3.17.0" :documentation
    "The edit text used if the completion item is part of a CompletionList and
CompletionList defines an item default for the text edit range.

Clients will only honor this property if they opt into completion list
item defaults using the capability `completionList.itemDefaults`.

If not provided and a list's default range is provided the label
property is used as a text.

@since 3.17.0")
   (additional-text-edits :type (lem-lsp-base/type:lsp-array text-edit) :initarg
    :additional-text-edits :accessor completion-item-additional-text-edits :optional common-lisp:t
    :documentation
    "An optional array of additional {@link TextEdit text edits} that are applied when
selecting this completion. Edits must not overlap (including the same insert position)
with the main {@link CompletionItem.textEdit edit} nor with themselves.

Additional text edits should be used to change text unrelated to the current cursor position
(for example adding an import statement at the top of the file if the completion item will
insert an unqualified type).")
   (commit-characters :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :commit-characters :accessor completion-item-commit-characters :optional common-lisp:t
    :documentation
    "An optional set of characters that when pressed while this completion is active will accept it first and
then type that character. *Note* that all commit characters should have `length=1` and that superfluous
characters will be ignored.")
   (command :type command :initarg :command :accessor completion-item-command :optional
    common-lisp:t :documentation
    "An optional {@link Command command} that is executed *after* inserting this completion. *Note* that
additional modifications to the current document should be described with the
{@link CompletionItem.additionalTextEdits additionalTextEdits}-property.")
   (data :type lsp-any :initarg :data :accessor completion-item-data :optional common-lisp:t
    :documentation "A data entry field that is preserved on a completion item between a
{@link CompletionRequest} and a {@link CompletionResolveRequest}."))
  (:documentation "A completion item represents a text snippet that is
proposed to complete text that is being typed."))

(lem-lsp-base/type:define-class completion-list
    common-lisp:nil
  ((is-incomplete :type lem-lsp-base/type:lsp-boolean :initarg :is-incomplete :accessor
    completion-list-is-incomplete :documentation
    "This list it not complete. Further typing results in recomputing this list.

Recomputed lists have all their items replaced (not appended) in the
incomplete completion sessions.")
   (item-defaults :type
    (lem-lsp-base/type:lsp-interface
     ((commit-characters :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :optional
       common-lisp:t :since "3.17.0" :documentation "A default commit character set.

@since 3.17.0")
      (edit-range :type
       (common-lisp:or range
                       (lem-lsp-base/type:lsp-interface
                        ((insert :type range) (replace :type range))))
       :optional common-lisp:t :since "3.17.0" :documentation "A default edit range.

@since 3.17.0")
      (insert-text-format :type insert-text-format :optional common-lisp:t :since "3.17.0"
       :documentation "A default insert text format.

@since 3.17.0")
      (insert-text-mode :type insert-text-mode :optional common-lisp:t :since "3.17.0"
       :documentation "A default insert text mode.

@since 3.17.0")
      (data :type lsp-any :optional common-lisp:t :since "3.17.0" :documentation
       "A default data value.

@since 3.17.0")))
    :initarg :item-defaults :accessor completion-list-item-defaults :optional common-lisp:t :since
    "3.17.0" :documentation "In many cases the items of an actual completion result share the same
value for properties like `commitCharacters` or the range of a text
edit. A completion list can therefore define item defaults which will
be used if a completion item itself doesn't specify the value.

If a completion list specifies a default value and a completion item
also specifies a corresponding value the one from the item is used.

Servers are only allowed to return default values if the client
signals support for this via the `completionList.itemDefaults`
capability.

@since 3.17.0")
   (items :type (lem-lsp-base/type:lsp-array completion-item) :initarg :items :accessor
    completion-list-items :documentation "The completion items."))
  (:documentation
   "Represents a collection of {@link CompletionItem completion items} to be presented
in the editor."))

(lem-lsp-base/type:define-class completion-registration-options
    (text-document-registration-options completion-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link CompletionRequest}."))

(lem-lsp-base/type:define-class hover-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:documentation "Parameters for a {@link HoverRequest}."))

(lem-lsp-base/type:define-class hover
    common-lisp:nil
  ((contents :type
    (common-lisp:or markup-content marked-string (lem-lsp-base/type:lsp-array marked-string))
    :initarg :contents :accessor hover-contents :documentation "The hover's content")
   (range :type range :initarg :range :accessor hover-range :optional common-lisp:t :documentation
    "An optional range inside the text document that is used to
visualize the hover, e.g. by changing the background color."))
  (:documentation "The result of a hover request."))

(lem-lsp-base/type:define-class hover-registration-options
    (text-document-registration-options hover-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link HoverRequest}."))

(lem-lsp-base/type:define-class signature-help-params
    (text-document-position-params work-done-progress-params)
  ((context :type signature-help-context :initarg :context :accessor signature-help-params-context
    :optional common-lisp:t :since "3.15.0" :documentation
    "The signature help context. This is only available if the client specifies
to send this using the client capability `textDocument.signatureHelp.contextSupport === true`

@since 3.15.0"))
  (:documentation "Parameters for a {@link SignatureHelpRequest}."))

(lem-lsp-base/type:define-class signature-help
    common-lisp:nil
  ((signatures :type (lem-lsp-base/type:lsp-array signature-information) :initarg :signatures
    :accessor signature-help-signatures :documentation "One or more signatures.")
   (active-signature :type lem-lsp-base/type:lsp-uinteger :initarg :active-signature :accessor
    signature-help-active-signature :optional common-lisp:t :documentation
    "The active signature. If omitted or the value lies outside the
range of `signatures` the value defaults to zero or is ignored if
the `SignatureHelp` has no signatures.

Whenever possible implementors should make an active decision about
the active signature and shouldn't rely on a default value.

In future version of the protocol this property might become
mandatory to better express this.")
   (active-parameter :type lem-lsp-base/type:lsp-uinteger :initarg :active-parameter :accessor
    signature-help-active-parameter :optional common-lisp:t :documentation
    "The active parameter of the active signature. If omitted or the value
lies outside the range of `signatures[activeSignature].parameters`
defaults to 0 if the active signature has parameters. If
the active signature has no parameters it is ignored.
In future version of the protocol this property might become
mandatory to better express the active parameter if the
active signature does have any."))
  (:documentation "Signature help represents the signature of something
callable. There can be multiple signature but only one
active and only one active parameter."))

(lem-lsp-base/type:define-class signature-help-registration-options
    (text-document-registration-options signature-help-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link SignatureHelpRequest}."))

(lem-lsp-base/type:define-class definition-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:documentation "Parameters for a {@link DefinitionRequest}."))

(lem-lsp-base/type:define-class definition-registration-options
    (text-document-registration-options definition-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DefinitionRequest}."))

(lem-lsp-base/type:define-class reference-params
    (text-document-position-params work-done-progress-params partial-result-params)
  ((context :type reference-context :initarg :context :accessor reference-params-context))
  (:documentation "Parameters for a {@link ReferencesRequest}."))

(lem-lsp-base/type:define-class reference-registration-options
    (text-document-registration-options reference-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link ReferencesRequest}."))

(lem-lsp-base/type:define-class document-highlight-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:documentation "Parameters for a {@link DocumentHighlightRequest}."))

(lem-lsp-base/type:define-class document-highlight
    common-lisp:nil
  ((range :type range :initarg :range :accessor document-highlight-range :documentation
    "The range this highlight applies to.")
   (kind :type document-highlight-kind :initarg :kind :accessor document-highlight-kind :optional
    common-lisp:t :documentation
    "The highlight kind, default is {@link DocumentHighlightKind.Text text}."))
  (:documentation "A document highlight is a range inside a text document which deserves
special attention. Usually a document highlight is visualized by changing
the background color of its range."))

(lem-lsp-base/type:define-class document-highlight-registration-options
    (text-document-registration-options document-highlight-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentHighlightRequest}."))

(lem-lsp-base/type:define-class document-symbol-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-symbol-params-text-document :documentation "The text document."))
  (:documentation "Parameters for a {@link DocumentSymbolRequest}."))

(lem-lsp-base/type:define-class symbol-information
    (base-symbol-information)
  ((deprecated :type lem-lsp-base/type:lsp-boolean :initarg :deprecated :accessor
    symbol-information-deprecated :optional common-lisp:t :deprecated "Use tags instead"
    :documentation "Indicates if this symbol is deprecated.

@deprecated Use tags instead")
   (location :type location :initarg :location :accessor symbol-information-location :documentation
    "The location of this symbol. The location's range is used by a tool
to reveal the location in the editor. If the symbol is selected in the
tool the range's start information is used to position the cursor. So
the range usually spans more than the actual symbol's name and does
normally include things like visibility modifiers.

The range doesn't have to denote a node range in the sense of an abstract
syntax tree. It can therefore not be used to re-construct a hierarchy of
the symbols."))
  (:documentation "Represents information about programming constructs like variables, classes,
interfaces etc."))

(lem-lsp-base/type:define-class document-symbol
    common-lisp:nil
  ((name :type lem-lsp-base/type:lsp-string :initarg :name :accessor document-symbol-name
    :documentation
    "The name of this symbol. Will be displayed in the user interface and therefore must not be
an empty string or a string only consisting of white spaces.")
   (detail :type lem-lsp-base/type:lsp-string :initarg :detail :accessor document-symbol-detail
    :optional common-lisp:t :documentation
    "More detail for this symbol, e.g the signature of a function.")
   (kind :type symbol-kind :initarg :kind :accessor document-symbol-kind :documentation
    "The kind of this symbol.")
   (tags :type (lem-lsp-base/type:lsp-array symbol-tag) :initarg :tags :accessor
    document-symbol-tags :optional common-lisp:t :since "3.16.0" :documentation
    "Tags for this document symbol.

@since 3.16.0")
   (deprecated :type lem-lsp-base/type:lsp-boolean :initarg :deprecated :accessor
    document-symbol-deprecated :optional common-lisp:t :deprecated "Use tags instead"
    :documentation "Indicates if this symbol is deprecated.

@deprecated Use tags instead")
   (range :type range :initarg :range :accessor document-symbol-range :documentation
    "The range enclosing this symbol not including leading/trailing whitespace but everything else
like comments. This information is typically used to determine if the clients cursor is
inside the symbol to reveal in the symbol in the UI.")
   (selection-range :type range :initarg :selection-range :accessor document-symbol-selection-range
    :documentation
    "The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
Must be contained by the `range`.")
   (children :type (lem-lsp-base/type:lsp-array document-symbol) :initarg :children :accessor
    document-symbol-children :optional common-lisp:t :documentation
    "Children of this symbol, e.g. properties of a class."))
  (:documentation "Represents programming constructs like variables, classes, interfaces etc.
that appear in a document. Document symbols can be hierarchical and they
have two ranges: one that encloses its definition and one that points to
its most interesting range, e.g. the range of an identifier."))

(lem-lsp-base/type:define-class document-symbol-registration-options
    (text-document-registration-options document-symbol-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentSymbolRequest}."))

(lem-lsp-base/type:define-class code-action-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    code-action-params-text-document :documentation
    "The document in which the command was invoked.")
   (range :type range :initarg :range :accessor code-action-params-range :documentation
    "The range for which the command was invoked.")
   (context :type code-action-context :initarg :context :accessor code-action-params-context
    :documentation "Context carrying additional information."))
  (:documentation "The parameters of a {@link CodeActionRequest}."))

(lem-lsp-base/type:define-class command
    common-lisp:nil
  ((title :type lem-lsp-base/type:lsp-string :initarg :title :accessor command-title :documentation
    "Title of the command, like `save`.")
   (command :type lem-lsp-base/type:lsp-string :initarg :command :accessor command-command
    :documentation "The identifier of the actual command handler.")
   (arguments :type (lem-lsp-base/type:lsp-array lsp-any) :initarg :arguments :accessor
    command-arguments :optional common-lisp:t :documentation
    "Arguments that the command handler should be
invoked with."))
  (:documentation "Represents a reference to a command. Provides a title which
will be used to represent a command in the UI and, optionally,
an array of arguments which will be passed to the command handler
function when invoked."))

(lem-lsp-base/type:define-class code-action
    common-lisp:nil
  ((title :type lem-lsp-base/type:lsp-string :initarg :title :accessor code-action-title
    :documentation "A short, human-readable, title for this code action.")
   (kind :type code-action-kind :initarg :kind :accessor code-action-kind :optional common-lisp:t
    :documentation "The kind of the code action.

Used to filter code actions.")
   (diagnostics :type (lem-lsp-base/type:lsp-array diagnostic) :initarg :diagnostics :accessor
    code-action-diagnostics :optional common-lisp:t :documentation
    "The diagnostics that this code action resolves.")
   (is-preferred :type lem-lsp-base/type:lsp-boolean :initarg :is-preferred :accessor
    code-action-is-preferred :optional common-lisp:t :since "3.15.0" :documentation
    "Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
by keybindings.

A quick fix should be marked preferred if it properly addresses the underlying error.
A refactoring should be marked preferred if it is the most reasonable choice of actions to take.

@since 3.15.0")
   (disabled :type
    (lem-lsp-base/type:lsp-interface
     ((reason :type lem-lsp-base/type:lsp-string :documentation
       "Human readable description of why the code action is currently disabled.

This is displayed in the code actions UI.")))
    :initarg :disabled :accessor code-action-disabled :optional common-lisp:t :since "3.16.0"
    :documentation "Marks that the code action cannot currently be applied.

Clients should follow the following guidelines regarding disabled code actions:

  - Disabled code actions are not shown in automatic [lightbulbs](https://code.visualstudio.com/docs/editor/editingevolved#_code-action)
    code action menus.

  - Disabled actions are shown as faded out in the code action menu when the user requests a more specific type
    of code action, such as refactorings.

  - If the user has a [keybinding](https://code.visualstudio.com/docs/editor/refactoring#_keybindings-for-code-actions)
    that auto applies a code action and only disabled code actions are returned, the client should show the user an
    error message with `reason` in the editor.

@since 3.16.0")
   (edit :type workspace-edit :initarg :edit :accessor code-action-edit :optional common-lisp:t
    :documentation "The workspace edit this code action performs.")
   (command :type command :initarg :command :accessor code-action-command :optional common-lisp:t
    :documentation "A command this code action executes. If a code action
provides an edit and a command, first the edit is
executed and then the command.")
   (data :type lsp-any :initarg :data :accessor code-action-data :optional common-lisp:t :since
    "3.16.0" :documentation "A data entry field that is preserved on a code action between
a `textDocument/codeAction` and a `codeAction/resolve` request.

@since 3.16.0"))
  (:documentation
   "A code action represents a change that can be performed in code, e.g. to fix a problem or
to refactor code.

A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed."))

(lem-lsp-base/type:define-class code-action-registration-options
    (text-document-registration-options code-action-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link CodeActionRequest}."))

(lem-lsp-base/type:define-class workspace-symbol-params
    (work-done-progress-params partial-result-params)
  ((query :type lem-lsp-base/type:lsp-string :initarg :query :accessor
    workspace-symbol-params-query :documentation
    "A query string to filter symbols by. Clients may send an empty
string here to request all symbols."))
  (:documentation "The parameters of a {@link WorkspaceSymbolRequest}."))

(lem-lsp-base/type:define-class workspace-symbol
    (base-symbol-information)
  ((location :type
    (common-lisp:or location
                    (lem-lsp-base/type:lsp-interface
                     ((uri :type lem-lsp-base/type:lsp-document-uri))))
    :initarg :location :accessor workspace-symbol-location :documentation
    "The location of the symbol. Whether a server is allowed to
return a location without a range depends on the client
capability `workspace.symbol.resolveSupport`.

See SymbolInformation#location for more details.")
   (data :type lsp-any :initarg :data :accessor workspace-symbol-data :optional common-lisp:t
    :documentation "A data entry field that is preserved on a workspace symbol between a
workspace symbol request and a workspace symbol resolve request."))
  (:since "3.17.0")
  (:documentation "A special workspace symbol that supports locations without a range.

See also SymbolInformation.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-symbol-registration-options
    (workspace-symbol-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link WorkspaceSymbolRequest}."))

(lem-lsp-base/type:define-class code-lens-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    code-lens-params-text-document :documentation "The document to request code lens for."))
  (:documentation "The parameters of a {@link CodeLensRequest}."))

(lem-lsp-base/type:define-class code-lens
    common-lisp:nil
  ((range :type range :initarg :range :accessor code-lens-range :documentation
    "The range in which this code lens is valid. Should only span a single line.")
   (command :type command :initarg :command :accessor code-lens-command :optional common-lisp:t
    :documentation "The command this code lens represents.")
   (data :type lsp-any :initarg :data :accessor code-lens-data :optional common-lisp:t
    :documentation "A data entry field that is preserved on a code lens item between
a {@link CodeLensRequest} and a {@link CodeLensResolveRequest}"))
  (:documentation "A code lens represents a {@link Command command} that should be shown along with
source text, like the number of references, a way to run tests, etc.

A code lens is _unresolved_ when no command is associated to it. For performance
reasons the creation of a code lens and resolving should be done in two stages."))

(lem-lsp-base/type:define-class code-lens-registration-options
    (text-document-registration-options code-lens-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link CodeLensRequest}."))

(lem-lsp-base/type:define-class document-link-params
    (work-done-progress-params partial-result-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-link-params-text-document :documentation
    "The document to provide document links for."))
  (:documentation "The parameters of a {@link DocumentLinkRequest}."))

(lem-lsp-base/type:define-class document-link
    common-lisp:nil
  ((range :type range :initarg :range :accessor document-link-range :documentation
    "The range this link applies to.")
   (target :type lem-lsp-base/type:lsp-uri :initarg :target :accessor document-link-target
    :optional common-lisp:t :documentation
    "The uri this link points to. If missing a resolve request is sent later.")
   (tooltip :type lem-lsp-base/type:lsp-string :initarg :tooltip :accessor document-link-tooltip
    :optional common-lisp:t :since "3.15.0" :documentation
    "The tooltip text when you hover over this link.

If a tooltip is provided, is will be displayed in a string that includes instructions on how to
trigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,
user settings, and localization.

@since 3.15.0")
   (data :type lsp-any :initarg :data :accessor document-link-data :optional common-lisp:t
    :documentation "A data entry field that is preserved on a document link between a
DocumentLinkRequest and a DocumentLinkResolveRequest."))
  (:documentation
   "A document link is a range in a text document that links to an internal or external resource, like another
text document or a web site."))

(lem-lsp-base/type:define-class document-link-registration-options
    (text-document-registration-options document-link-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentLinkRequest}."))

(lem-lsp-base/type:define-class document-formatting-params
    (work-done-progress-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-formatting-params-text-document :documentation "The document to format.")
   (options :type formatting-options :initarg :options :accessor document-formatting-params-options
    :documentation "The format options."))
  (:documentation "The parameters of a {@link DocumentFormattingRequest}."))

(lem-lsp-base/type:define-class document-formatting-registration-options
    (text-document-registration-options document-formatting-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentFormattingRequest}."))

(lem-lsp-base/type:define-class document-range-formatting-params
    (work-done-progress-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-range-formatting-params-text-document :documentation "The document to format.")
   (range :type range :initarg :range :accessor document-range-formatting-params-range
    :documentation "The range to format")
   (options :type formatting-options :initarg :options :accessor
    document-range-formatting-params-options :documentation "The format options"))
  (:documentation "The parameters of a {@link DocumentRangeFormattingRequest}."))

(lem-lsp-base/type:define-class document-range-formatting-registration-options
    (text-document-registration-options document-range-formatting-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentRangeFormattingRequest}."))

(lem-lsp-base/type:define-class document-ranges-formatting-params
    (work-done-progress-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-ranges-formatting-params-text-document :documentation "The document to format.")
   (ranges :type (lem-lsp-base/type:lsp-array range) :initarg :ranges :accessor
    document-ranges-formatting-params-ranges :documentation "The ranges to format")
   (options :type formatting-options :initarg :options :accessor
    document-ranges-formatting-params-options :documentation "The format options"))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "The parameters of a {@link DocumentRangesFormattingRequest}.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class document-on-type-formatting-params
    common-lisp:nil
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    document-on-type-formatting-params-text-document :documentation "The document to format.")
   (position :type position :initarg :position :accessor
    document-on-type-formatting-params-position :documentation
    "The position around which the on type formatting should happen.
This is not necessarily the exact position where the character denoted
by the property `ch` got typed.")
   (ch :type lem-lsp-base/type:lsp-string :initarg :ch :accessor
    document-on-type-formatting-params-ch :documentation
    "The character that has been typed that triggered the formatting
on type request. That is not necessarily the last character that
got inserted into the document since the client could auto insert
characters as well (e.g. like automatic brace completion).")
   (options :type formatting-options :initarg :options :accessor
    document-on-type-formatting-params-options :documentation "The formatting options."))
  (:documentation "The parameters of a {@link DocumentOnTypeFormattingRequest}."))

(lem-lsp-base/type:define-class document-on-type-formatting-registration-options
    (text-document-registration-options document-on-type-formatting-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentOnTypeFormattingRequest}."))

(lem-lsp-base/type:define-class rename-params
    (work-done-progress-params)
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    rename-params-text-document :documentation "The document to rename.")
   (position :type position :initarg :position :accessor rename-params-position :documentation
    "The position at which this request was sent.")
   (new-name :type lem-lsp-base/type:lsp-string :initarg :new-name :accessor rename-params-new-name
    :documentation "The new name of the symbol. If the given name is not valid the
request must return a {@link ResponseError} with an
appropriate message set."))
  (:documentation "The parameters of a {@link RenameRequest}."))

(lem-lsp-base/type:define-class rename-registration-options
    (text-document-registration-options rename-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link RenameRequest}."))

(lem-lsp-base/type:define-class prepare-rename-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil)

(lem-lsp-base/type:define-class execute-command-params
    (work-done-progress-params)
  ((command :type lem-lsp-base/type:lsp-string :initarg :command :accessor
    execute-command-params-command :documentation "The identifier of the actual command handler.")
   (arguments :type (lem-lsp-base/type:lsp-array lsp-any) :initarg :arguments :accessor
    execute-command-params-arguments :optional common-lisp:t :documentation
    "Arguments that the command should be invoked with."))
  (:documentation "The parameters of a {@link ExecuteCommandRequest}."))

(lem-lsp-base/type:define-class execute-command-registration-options
    (execute-command-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link ExecuteCommandRequest}."))

(lem-lsp-base/type:define-class apply-workspace-edit-params
    common-lisp:nil
  ((label :type lem-lsp-base/type:lsp-string :initarg :label :accessor
    apply-workspace-edit-params-label :optional common-lisp:t :documentation
    "An optional label of the workspace edit. This label is
presented in the user interface for example on an undo
stack to undo the workspace edit.")
   (edit :type workspace-edit :initarg :edit :accessor apply-workspace-edit-params-edit
    :documentation "The edits to apply."))
  (:documentation "The parameters passed via an apply workspace edit request."))

(lem-lsp-base/type:define-class apply-workspace-edit-result
    common-lisp:nil
  ((applied :type lem-lsp-base/type:lsp-boolean :initarg :applied :accessor
    apply-workspace-edit-result-applied :documentation
    "Indicates whether the edit was applied or not.")
   (failure-reason :type lem-lsp-base/type:lsp-string :initarg :failure-reason :accessor
    apply-workspace-edit-result-failure-reason :optional common-lisp:t :documentation
    "An optional textual description for why the edit was not applied.
This may be used by the server for diagnostic logging or to provide
a suitable error for a request that triggered the edit.")
   (failed-change :type lem-lsp-base/type:lsp-uinteger :initarg :failed-change :accessor
    apply-workspace-edit-result-failed-change :optional common-lisp:t :documentation
    "Depending on the client's failure handling strategy `failedChange` might
contain the index of the change that failed. This property is only available
if the client signals a `failureHandlingStrategy` in its client capabilities."))
  (:since "3.17 renamed from ApplyWorkspaceEditResponse")
  (:documentation "The result returned from the apply workspace edit request.

@since 3.17 renamed from ApplyWorkspaceEditResponse"))

(lem-lsp-base/type:define-class work-done-progress-begin
    common-lisp:nil
  ((kind :type (lem-lsp-base/type:lsp-string "begin") :initarg :kind :accessor
    work-done-progress-begin-kind)
   (title :type lem-lsp-base/type:lsp-string :initarg :title :accessor
    work-done-progress-begin-title :documentation
    "Mandatory title of the progress operation. Used to briefly inform about
the kind of operation being performed.

Examples: \"Indexing\" or \"Linking dependencies\".")
   (cancellable :type lem-lsp-base/type:lsp-boolean :initarg :cancellable :accessor
    work-done-progress-begin-cancellable :optional common-lisp:t :documentation
    "Controls if a cancel button should show to allow the user to cancel the
long running operation. Clients that don't support cancellation are allowed
to ignore the setting.")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    work-done-progress-begin-message :optional common-lisp:t :documentation
    "Optional, more detailed associated progress message. Contains
complementary information to the `title`.

Examples: \"3/25 files\", \"project/src/module2\", \"node_modules/some_dep\".
If unset, the previous progress message (if any) is still valid.")
   (percentage :type lem-lsp-base/type:lsp-uinteger :initarg :percentage :accessor
    work-done-progress-begin-percentage :optional common-lisp:t :documentation
    "Optional progress percentage to display (value 100 is considered 100%).
If not provided infinite progress is assumed and clients are allowed
to ignore the `percentage` value in subsequent in report notifications.

The value should be steadily rising. Clients are free to ignore values
that are not following this rule. The value range is [0, 100].")))

(lem-lsp-base/type:define-class work-done-progress-report
    common-lisp:nil
  ((kind :type (lem-lsp-base/type:lsp-string "report") :initarg :kind :accessor
    work-done-progress-report-kind)
   (cancellable :type lem-lsp-base/type:lsp-boolean :initarg :cancellable :accessor
    work-done-progress-report-cancellable :optional common-lisp:t :documentation
    "Controls enablement state of a cancel button.

Clients that don't support cancellation or don't support controlling the button's
enablement state are allowed to ignore the property.")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    work-done-progress-report-message :optional common-lisp:t :documentation
    "Optional, more detailed associated progress message. Contains
complementary information to the `title`.

Examples: \"3/25 files\", \"project/src/module2\", \"node_modules/some_dep\".
If unset, the previous progress message (if any) is still valid.")
   (percentage :type lem-lsp-base/type:lsp-uinteger :initarg :percentage :accessor
    work-done-progress-report-percentage :optional common-lisp:t :documentation
    "Optional progress percentage to display (value 100 is considered 100%).
If not provided infinite progress is assumed and clients are allowed
to ignore the `percentage` value in subsequent in report notifications.

The value should be steadily rising. Clients are free to ignore values
that are not following this rule. The value range is [0, 100]")))

(lem-lsp-base/type:define-class work-done-progress-end
    common-lisp:nil
  ((kind :type (lem-lsp-base/type:lsp-string "end") :initarg :kind :accessor
    work-done-progress-end-kind)
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    work-done-progress-end-message :optional common-lisp:t :documentation
    "Optional, a final message indicating to for example indicate the outcome
of the operation.")))

(lem-lsp-base/type:define-class set-trace-params
    common-lisp:nil
  ((value :type trace-values :initarg :value :accessor set-trace-params-value)))

(lem-lsp-base/type:define-class log-trace-params
    common-lisp:nil
  ((message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    log-trace-params-message)
   (verbose :type lem-lsp-base/type:lsp-string :initarg :verbose :accessor log-trace-params-verbose
    :optional common-lisp:t)))

(lem-lsp-base/type:define-class cancel-params
    common-lisp:nil
  ((id :type (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-string) :initarg
    :id :accessor cancel-params-id :documentation "The request id to cancel.")))

(lem-lsp-base/type:define-class progress-params
    common-lisp:nil
  ((token :type progress-token :initarg :token :accessor progress-params-token :documentation
    "The progress token provided by the client or server.")
   (value :type lsp-any :initarg :value :accessor progress-params-value :documentation
    "The progress data.")))

(lem-lsp-base/type:define-class text-document-position-params
    common-lisp:nil
  ((text-document :type text-document-identifier :initarg :text-document :accessor
    text-document-position-params-text-document :documentation "The text document.")
   (position :type position :initarg :position :accessor text-document-position-params-position
    :documentation "The position inside the text document."))
  (:documentation
   "A parameter literal used in requests to pass a text document and a position inside that
document."))

(lem-lsp-base/type:define-class work-done-progress-params
    common-lisp:nil
  ((work-done-token :type progress-token :initarg :work-done-token :accessor
    work-done-progress-params-work-done-token :optional common-lisp:t :documentation
    "An optional token that a server can use to report work done progress.")))

(lem-lsp-base/type:define-class partial-result-params
    common-lisp:nil
  ((partial-result-token :type progress-token :initarg :partial-result-token :accessor
    partial-result-params-partial-result-token :optional common-lisp:t :documentation
    "An optional token that a server can use to report partial results (e.g. streaming) to
the client.")))

(lem-lsp-base/type:define-class location-link
    common-lisp:nil
  ((origin-selection-range :type range :initarg :origin-selection-range :accessor
    location-link-origin-selection-range :optional common-lisp:t :documentation
    "Span of the origin of this link.

Used as the underlined span for mouse interaction. Defaults to the word range at
the definition position.")
   (target-uri :type lem-lsp-base/type:lsp-document-uri :initarg :target-uri :accessor
    location-link-target-uri :documentation "The target resource identifier of this link.")
   (target-range :type range :initarg :target-range :accessor location-link-target-range
    :documentation
    "The full target range of this link. If the target for example is a symbol then target range is the
range enclosing this symbol not including leading/trailing whitespace but everything else
like comments. This information is typically used to highlight the range in the editor.")
   (target-selection-range :type range :initarg :target-selection-range :accessor
    location-link-target-selection-range :documentation
    "The range that should be selected and revealed when this link is being followed, e.g the name of a function.
Must be contained by the `targetRange`. See also `DocumentSymbol#range`"))
  (:documentation
   "Represents the connection of two locations. Provides additional metadata over normal {@link Location locations},
including an origin range."))

(lem-lsp-base/type:define-class range
    common-lisp:nil
  ((start :type position :initarg :start :accessor range-start :documentation
    "The range's start position.")
   (end :type position :initarg :end :accessor range-end :documentation
    "The range's end position."))
  (:documentation "A range in a text document expressed as (zero-based) start and end positions.

If you want to specify a range that contains a line including the line ending
character(s) then use an end position denoting the start of the next line.
For example:
```ts
{
    start: { line: 5, character: 23 }
    end : { line 6, character : 0 }
}
```"))

(lem-lsp-base/type:define-class implementation-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class static-registration-options
    common-lisp:nil
  ((id :type lem-lsp-base/type:lsp-string :initarg :id :accessor static-registration-options-id
    :optional common-lisp:t :documentation
    "The id used to register the request. The id can be used to deregister
the request again. See also Registration#id."))
  (:documentation "Static registration options to be returned in the initialize
request."))

(lem-lsp-base/type:define-class type-definition-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class workspace-folders-change-event
    common-lisp:nil
  ((added :type (lem-lsp-base/type:lsp-array workspace-folder) :initarg :added :accessor
    workspace-folders-change-event-added :documentation "The array of added workspace folders")
   (removed :type (lem-lsp-base/type:lsp-array workspace-folder) :initarg :removed :accessor
    workspace-folders-change-event-removed :documentation
    "The array of the removed workspace folders"))
  (:documentation "The workspace folder change event."))

(lem-lsp-base/type:define-class configuration-item
    common-lisp:nil
  ((scope-uri :type lem-lsp-base/type:lsp-uri :initarg :scope-uri :accessor
    configuration-item-scope-uri :optional common-lisp:t :documentation
    "The scope to get the configuration section for.")
   (section :type lem-lsp-base/type:lsp-string :initarg :section :accessor
    configuration-item-section :optional common-lisp:t :documentation
    "The configuration section asked for.")))

(lem-lsp-base/type:define-class text-document-identifier
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor
    text-document-identifier-uri :documentation "The text document's uri."))
  (:documentation "A literal to identify a text document in the client."))

(lem-lsp-base/type:define-class color
    common-lisp:nil
  ((red :type lem-lsp-base/type:lsp-decimal :initarg :red :accessor color-red :documentation
    "The red component of this color in the range [0-1].")
   (green :type lem-lsp-base/type:lsp-decimal :initarg :green :accessor color-green :documentation
    "The green component of this color in the range [0-1].")
   (blue :type lem-lsp-base/type:lsp-decimal :initarg :blue :accessor color-blue :documentation
    "The blue component of this color in the range [0-1].")
   (alpha :type lem-lsp-base/type:lsp-decimal :initarg :alpha :accessor color-alpha :documentation
    "The alpha component of this color in the range [0-1]."))
  (:documentation "Represents a color in RGBA space."))

(lem-lsp-base/type:define-class document-color-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class folding-range-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class declaration-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class position
    common-lisp:nil
  ((line :type lem-lsp-base/type:lsp-uinteger :initarg :line :accessor position-line :documentation
    "Line position in a document (zero-based).

If a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.
If a line number is negative, it defaults to 0.")
   (character :type lem-lsp-base/type:lsp-uinteger :initarg :character :accessor position-character
    :documentation "Character offset on a line in a document (zero-based).

The meaning of this offset is determined by the negotiated
`PositionEncodingKind`.

If the character value is greater than the line length it defaults back to the
line length."))
  (:since "3.17.0 - support for negotiated position encoding.")
  (:documentation "Position in a text document expressed as zero-based line and character
offset. Prior to 3.17 the offsets were always based on a UTF-16 string
representation. So a string of the form `a𐐀b` the character offset of the
character `a` is 0, the character offset of `𐐀` is 1 and the character
offset of b is 3 since `𐐀` is represented using two code units in UTF-16.
Since 3.17 clients and servers can agree on a different string encoding
representation (e.g. UTF-8). The client announces it's supported encoding
via the client capability [`general.positionEncodings`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#clientCapabilities).
The value is an array of position encodings the client supports, with
decreasing preference (e.g. the encoding at index `0` is the most preferred
one). To stay backwards compatible the only mandatory encoding is UTF-16
represented via the string `utf-16`. The server can pick one of the
encodings offered by the client and signals that encoding back to the
client via the initialize result's property
[`capabilities.positionEncoding`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#serverCapabilities). If the string value
`utf-16` is missing from the client's capability `general.positionEncodings`
servers can safely assume that the client supports UTF-16. If the server
omits the position encoding in its initialize result the encoding defaults
to the string value `utf-16`. Implementation considerations: since the
conversion from one encoding into another requires the content of the
file / line the conversion is best done where the file is read which is
usually on the server side.

Positions are line end character agnostic. So you can not specify a position
that denotes `\\r|\\n` or `\\n|` where `|` represents the character offset.

@since 3.17.0 - support for negotiated position encoding."))

(lem-lsp-base/type:define-class selection-range-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class call-hierarchy-options
    (work-done-progress-options)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "Call hierarchy options used during static registration.

@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-options
    (work-done-progress-options)
  ((legend :type semantic-tokens-legend :initarg :legend :accessor semantic-tokens-options-legend
    :documentation "The legend used by the server")
   (range :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean
                    (lem-lsp-base/type:lsp-interface common-lisp:nil))
    :initarg :range :accessor semantic-tokens-options-range :optional common-lisp:t :documentation
    "Server supports providing semantic tokens for a specific range
of a document.")
   (full :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean
                    (lem-lsp-base/type:lsp-interface
                     ((delta :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t
                       :documentation "The server supports deltas for full documents."))))
    :initarg :full :accessor semantic-tokens-options-full :optional common-lisp:t :documentation
    "Server supports providing semantic tokens for a full document."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-edit
    common-lisp:nil
  ((start :type lem-lsp-base/type:lsp-uinteger :initarg :start :accessor semantic-tokens-edit-start
    :documentation "The start offset of the edit.")
   (delete-count :type lem-lsp-base/type:lsp-uinteger :initarg :delete-count :accessor
    semantic-tokens-edit-delete-count :documentation "The count of elements to remove.")
   (data :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-uinteger) :initarg :data
    :accessor semantic-tokens-edit-data :optional common-lisp:t :documentation
    "The elements to insert."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class linked-editing-range-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class file-create
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-string :initarg :uri :accessor file-create-uri :documentation
    "A file:// URI for the location of the file/folder being created."))
  (:since "3.16.0")
  (:documentation "Represents information on a file/folder create.

@since 3.16.0"))

(lem-lsp-base/type:define-class text-document-edit
    common-lisp:nil
  ((text-document :type optional-versioned-text-document-identifier :initarg :text-document
    :accessor text-document-edit-text-document :documentation "The text document to change.")
   (edits :type (lem-lsp-base/type:lsp-array (common-lisp:or text-edit annotated-text-edit))
    :initarg :edits :accessor text-document-edit-edits :since
    "3.16.0 - support for AnnotatedTextEdit. This is guarded using a
client capability."
    :documentation "The edits to be applied.

@since 3.16.0 - support for AnnotatedTextEdit. This is guarded using a
client capability."))
  (:documentation
   "Describes textual changes on a text document. A TextDocumentEdit describes all changes
on a document version Si and after they are applied move the document to version Si+1.
So the creator of a TextDocumentEdit doesn't need to sort the array of edits or do any
kind of ordering. However the edits must be non overlapping."))

(lem-lsp-base/type:define-class create-file
    (resource-operation)
  ((kind :type (lem-lsp-base/type:lsp-string "create") :initarg :kind :accessor create-file-kind
    :documentation "A create")
   (uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor create-file-uri
    :documentation "The resource to create.")
   (options :type create-file-options :initarg :options :accessor create-file-options :optional
    common-lisp:t :documentation "Additional options"))
  (:documentation "Create file operation."))

(lem-lsp-base/type:define-class rename-file
    (resource-operation)
  ((kind :type (lem-lsp-base/type:lsp-string "rename") :initarg :kind :accessor rename-file-kind
    :documentation "A rename")
   (old-uri :type lem-lsp-base/type:lsp-document-uri :initarg :old-uri :accessor
    rename-file-old-uri :documentation "The old (existing) location.")
   (new-uri :type lem-lsp-base/type:lsp-document-uri :initarg :new-uri :accessor
    rename-file-new-uri :documentation "The new location.")
   (options :type rename-file-options :initarg :options :accessor rename-file-options :optional
    common-lisp:t :documentation "Rename options."))
  (:documentation "Rename file operation"))

(lem-lsp-base/type:define-class delete-file
    (resource-operation)
  ((kind :type (lem-lsp-base/type:lsp-string "delete") :initarg :kind :accessor delete-file-kind
    :documentation "A delete")
   (uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor delete-file-uri
    :documentation "The file to delete.")
   (options :type delete-file-options :initarg :options :accessor delete-file-options :optional
    common-lisp:t :documentation "Delete options."))
  (:documentation "Delete file operation"))

(lem-lsp-base/type:define-class change-annotation
    common-lisp:nil
  ((label :type lem-lsp-base/type:lsp-string :initarg :label :accessor change-annotation-label
    :documentation "A human-readable string describing the actual change. The string
is rendered prominent in the user interface.")
   (needs-confirmation :type lem-lsp-base/type:lsp-boolean :initarg :needs-confirmation :accessor
    change-annotation-needs-confirmation :optional common-lisp:t :documentation
    "A flag which indicates that user confirmation is needed
before applying the change.")
   (description :type lem-lsp-base/type:lsp-string :initarg :description :accessor
    change-annotation-description :optional common-lisp:t :documentation
    "A human-readable string which is rendered less prominent in
the user interface."))
  (:since "3.16.0")
  (:documentation "Additional information that describes document changes.

@since 3.16.0"))

(lem-lsp-base/type:define-class file-operation-filter
    common-lisp:nil
  ((scheme :type lem-lsp-base/type:lsp-string :initarg :scheme :accessor
    file-operation-filter-scheme :optional common-lisp:t :documentation
    "A Uri scheme like `file` or `untitled`.")
   (pattern :type file-operation-pattern :initarg :pattern :accessor file-operation-filter-pattern
    :documentation "The actual file operation pattern."))
  (:since "3.16.0")
  (:documentation "A filter to describe in which file operation requests or notifications
the server is interested in receiving.

@since 3.16.0"))

(lem-lsp-base/type:define-class file-rename
    common-lisp:nil
  ((old-uri :type lem-lsp-base/type:lsp-string :initarg :old-uri :accessor file-rename-old-uri
    :documentation "A file:// URI for the original location of the file/folder being renamed.")
   (new-uri :type lem-lsp-base/type:lsp-string :initarg :new-uri :accessor file-rename-new-uri
    :documentation "A file:// URI for the new location of the file/folder being renamed."))
  (:since "3.16.0")
  (:documentation "Represents information on a file/folder rename.

@since 3.16.0"))

(lem-lsp-base/type:define-class file-delete
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-string :initarg :uri :accessor file-delete-uri :documentation
    "A file:// URI for the location of the file/folder being deleted."))
  (:since "3.16.0")
  (:documentation "Represents information on a file/folder delete.

@since 3.16.0"))

(lem-lsp-base/type:define-class moniker-options
    (work-done-progress-options)
  common-lisp:nil)

(lem-lsp-base/type:define-class type-hierarchy-options
    (work-done-progress-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Type hierarchy options used during static registration.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-context
    common-lisp:nil
  ((frame-id :type lem-lsp-base/type:lsp-integer :initarg :frame-id :accessor
    inline-value-context-frame-id :documentation
    "The stack frame (as a DAP Id) where the execution has stopped.")
   (stopped-location :type range :initarg :stopped-location :accessor
    inline-value-context-stopped-location :documentation
    "The document range where execution has stopped.
Typically the end position of the range denotes the line where the inline values are shown."))
  (:since "3.17.0")
  (:documentation "@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-text
    common-lisp:nil
  ((range :type range :initarg :range :accessor inline-value-text-range :documentation
    "The document range for which the inline value applies.")
   (text :type lem-lsp-base/type:lsp-string :initarg :text :accessor inline-value-text-text
    :documentation "The text of the inline value."))
  (:since "3.17.0")
  (:documentation "Provide inline value as text.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-variable-lookup
    common-lisp:nil
  ((range :type range :initarg :range :accessor inline-value-variable-lookup-range :documentation
    "The document range for which the inline value applies.
The range is used to extract the variable name from the underlying document.")
   (variable-name :type lem-lsp-base/type:lsp-string :initarg :variable-name :accessor
    inline-value-variable-lookup-variable-name :optional common-lisp:t :documentation
    "If specified the name of the variable to look up.")
   (case-sensitive-lookup :type lem-lsp-base/type:lsp-boolean :initarg :case-sensitive-lookup
    :accessor inline-value-variable-lookup-case-sensitive-lookup :documentation
    "How to perform the lookup."))
  (:since "3.17.0")
  (:documentation "Provide inline value through a variable lookup.
If only a range is specified, the variable name will be extracted from the underlying document.
An optional variable name can be used to override the extracted name.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-evaluatable-expression
    common-lisp:nil
  ((range :type range :initarg :range :accessor inline-value-evaluatable-expression-range
    :documentation "The document range for which the inline value applies.
The range is used to extract the evaluatable expression from the underlying document.")
   (expression :type lem-lsp-base/type:lsp-string :initarg :expression :accessor
    inline-value-evaluatable-expression-expression :optional common-lisp:t :documentation
    "If specified the expression overrides the extracted expression."))
  (:since "3.17.0")
  (:documentation "Provide an inline value through an expression evaluation.
If only a range is specified, the expression will be extracted from the underlying document.
An optional expression can be used to override the extracted expression.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-options
    (work-done-progress-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Inline value options used during static registration.

@since 3.17.0"))

(lem-lsp-base/type:define-class inlay-hint-label-part
    common-lisp:nil
  ((value :type lem-lsp-base/type:lsp-string :initarg :value :accessor inlay-hint-label-part-value
    :documentation "The value of this label part.")
   (tooltip :type (common-lisp:or lem-lsp-base/type:lsp-string markup-content) :initarg :tooltip
    :accessor inlay-hint-label-part-tooltip :optional common-lisp:t :documentation
    "The tooltip text when you hover over this label part. Depending on
the client capability `inlayHint.resolveSupport` clients might resolve
this property late using the resolve request.")
   (location :type location :initarg :location :accessor inlay-hint-label-part-location :optional
    common-lisp:t :documentation "An optional source code location that represents this
label part.

The editor will use this location for the hover and for code navigation
features: This part will become a clickable link that resolves to the
definition of the symbol at the given location (not necessarily the
location itself), it shows the hover that shows at the given location,
and it shows a context menu with further code navigation commands.

Depending on the client capability `inlayHint.resolveSupport` clients
might resolve this property late using the resolve request.")
   (command :type command :initarg :command :accessor inlay-hint-label-part-command :optional
    common-lisp:t :documentation "An optional command for this label part.

Depending on the client capability `inlayHint.resolveSupport` clients
might resolve this property late using the resolve request."))
  (:since "3.17.0")
  (:documentation "An inlay hint label part allows for interactive and composite labels
of inlay hints.

@since 3.17.0"))

(lem-lsp-base/type:define-class markup-content
    common-lisp:nil
  ((kind :type markup-kind :initarg :kind :accessor markup-content-kind :documentation
    "The type of the Markup")
   (value :type lem-lsp-base/type:lsp-string :initarg :value :accessor markup-content-value
    :documentation "The content itself"))
  (:documentation
   "A `MarkupContent` literal represents a string value which content is interpreted base on its
kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.

If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

Here is an example how such a string can be constructed using JavaScript / TypeScript:
```ts
let markdown: MarkdownContent = {
 kind: MarkupKind.Markdown,
 value: [
   '# Header',
   'Some text',
   '```typescript',
   'someCode();',
   '```'
 ].join('\\n')
};
```

*Please Note* that clients might sanitize the return markdown. A client could decide to
remove HTML from the markdown to avoid script execution."))

(lem-lsp-base/type:define-class inlay-hint-options
    (work-done-progress-options)
  ((resolve-provider :type lem-lsp-base/type:lsp-boolean :initarg :resolve-provider :accessor
    inlay-hint-options-resolve-provider :optional common-lisp:t :documentation
    "The server provides support to resolve additional
information for an inlay hint item."))
  (:since "3.17.0")
  (:documentation "Inlay hint options used during static registration.

@since 3.17.0"))

(lem-lsp-base/type:define-class related-full-document-diagnostic-report
    (full-document-diagnostic-report)
  ((related-documents :type
    (lem-lsp-base/type:lsp-map lem-lsp-base/type:lsp-document-uri
     (common-lisp:or full-document-diagnostic-report unchanged-document-diagnostic-report))
    :initarg :related-documents :accessor related-full-document-diagnostic-report-related-documents
    :optional common-lisp:t :since "3.17.0" :documentation
    "Diagnostics of related documents. This information is useful
in programming languages where code in a file A can generate
diagnostics in a file B which A depends on. An example of
such a language is C/C++ where marco definitions in a file
a.cpp and result in errors in a header file b.hpp.

@since 3.17.0"))
  (:since "3.17.0")
  (:documentation "A full diagnostic report with a set of related documents.

@since 3.17.0"))

(lem-lsp-base/type:define-class related-unchanged-document-diagnostic-report
    (unchanged-document-diagnostic-report)
  ((related-documents :type
    (lem-lsp-base/type:lsp-map lem-lsp-base/type:lsp-document-uri
     (common-lisp:or full-document-diagnostic-report unchanged-document-diagnostic-report))
    :initarg :related-documents :accessor
    related-unchanged-document-diagnostic-report-related-documents :optional common-lisp:t :since
    "3.17.0" :documentation "Diagnostics of related documents. This information is useful
in programming languages where code in a file A can generate
diagnostics in a file B which A depends on. An example of
such a language is C/C++ where marco definitions in a file
a.cpp and result in errors in a header file b.hpp.

@since 3.17.0"))
  (:since "3.17.0")
  (:documentation "An unchanged diagnostic report with a set of related documents.

@since 3.17.0"))

(lem-lsp-base/type:define-class full-document-diagnostic-report
    common-lisp:nil
  ((kind :type (lem-lsp-base/type:lsp-string "full") :initarg :kind :accessor
    full-document-diagnostic-report-kind :documentation "A full document diagnostic report.")
   (result-id :type lem-lsp-base/type:lsp-string :initarg :result-id :accessor
    full-document-diagnostic-report-result-id :optional common-lisp:t :documentation
    "An optional result id. If provided it will
be sent on the next diagnostic request for the
same document.")
   (items :type (lem-lsp-base/type:lsp-array diagnostic) :initarg :items :accessor
    full-document-diagnostic-report-items :documentation "The actual items."))
  (:since "3.17.0")
  (:documentation "A diagnostic report with a full set of problems.

@since 3.17.0"))

(lem-lsp-base/type:define-class unchanged-document-diagnostic-report
    common-lisp:nil
  ((kind :type (lem-lsp-base/type:lsp-string "unchanged") :initarg :kind :accessor
    unchanged-document-diagnostic-report-kind :documentation
    "A document diagnostic report indicating
no changes to the last result. A server can
only return `unchanged` if result ids are
provided.")
   (result-id :type lem-lsp-base/type:lsp-string :initarg :result-id :accessor
    unchanged-document-diagnostic-report-result-id :documentation
    "A result id which will be sent on the next
diagnostic request for the same document."))
  (:since "3.17.0")
  (:documentation "A diagnostic report indicating that the last returned
report is still accurate.

@since 3.17.0"))

(lem-lsp-base/type:define-class diagnostic-options
    (work-done-progress-options)
  ((identifier :type lem-lsp-base/type:lsp-string :initarg :identifier :accessor
    diagnostic-options-identifier :optional common-lisp:t :documentation
    "An optional identifier under which the diagnostics are
managed by the client.")
   (inter-file-dependencies :type lem-lsp-base/type:lsp-boolean :initarg :inter-file-dependencies
    :accessor diagnostic-options-inter-file-dependencies :documentation
    "Whether the language has inter file dependencies meaning that
editing code in one file can result in a different diagnostic
set in another file. Inter file dependencies are common for
most programming languages and typically uncommon for linters.")
   (workspace-diagnostics :type lem-lsp-base/type:lsp-boolean :initarg :workspace-diagnostics
    :accessor diagnostic-options-workspace-diagnostics :documentation
    "The server provides support for workspace diagnostics as well."))
  (:since "3.17.0")
  (:documentation "Diagnostic options.

@since 3.17.0"))

(lem-lsp-base/type:define-class previous-result-id
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor previous-result-id-uri
    :documentation "The URI for which the client knowns a
result id.")
   (value :type lem-lsp-base/type:lsp-string :initarg :value :accessor previous-result-id-value
    :documentation "The value of the previous result id."))
  (:since "3.17.0")
  (:documentation "A previous result id in a workspace pull request.

@since 3.17.0"))

(lem-lsp-base/type:define-class notebook-document
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-uri :initarg :uri :accessor notebook-document-uri
    :documentation "The notebook document's uri.")
   (notebook-type :type lem-lsp-base/type:lsp-string :initarg :notebook-type :accessor
    notebook-document-notebook-type :documentation "The type of the notebook.")
   (version :type lem-lsp-base/type:lsp-integer :initarg :version :accessor
    notebook-document-version :documentation
    "The version number of this document (it will increase after each
change, including undo/redo).")
   (metadata :type lsp-object :initarg :metadata :accessor notebook-document-metadata :optional
    common-lisp:t :documentation "Additional metadata stored with the notebook
document.

Note: should always be an object literal (e.g. LSPObject)")
   (cells :type (lem-lsp-base/type:lsp-array notebook-cell) :initarg :cells :accessor
    notebook-document-cells :documentation "The cells of a notebook."))
  (:since "3.17.0")
  (:documentation "A notebook document.

@since 3.17.0"))

(lem-lsp-base/type:define-class text-document-item
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor text-document-item-uri
    :documentation "The text document's uri.")
   (language-id :type lem-lsp-base/type:lsp-string :initarg :language-id :accessor
    text-document-item-language-id :documentation "The text document's language identifier.")
   (version :type lem-lsp-base/type:lsp-integer :initarg :version :accessor
    text-document-item-version :documentation
    "The version number of this document (it will increase after each
change, including undo/redo).")
   (text :type lem-lsp-base/type:lsp-string :initarg :text :accessor text-document-item-text
    :documentation "The content of the opened text document."))
  (:documentation "An item to transfer a text document from the client to the
server."))

(lem-lsp-base/type:define-class versioned-notebook-document-identifier
    common-lisp:nil
  ((version :type lem-lsp-base/type:lsp-integer :initarg :version :accessor
    versioned-notebook-document-identifier-version :documentation
    "The version number of this notebook document.")
   (uri :type lem-lsp-base/type:lsp-uri :initarg :uri :accessor
    versioned-notebook-document-identifier-uri :documentation "The notebook document's uri."))
  (:since "3.17.0")
  (:documentation "A versioned notebook document identifier.

@since 3.17.0"))

(lem-lsp-base/type:define-class notebook-document-change-event
    common-lisp:nil
  ((metadata :type lsp-object :initarg :metadata :accessor notebook-document-change-event-metadata
    :optional common-lisp:t :documentation "The changed meta data if any.

Note: should always be an object literal (e.g. LSPObject)")
   (cells :type
    (lem-lsp-base/type:lsp-interface
     ((structure :type
       (lem-lsp-base/type:lsp-interface
        ((array :type notebook-cell-array-change :documentation "The change to the cell array.")
         (did-open :type (lem-lsp-base/type:lsp-array text-document-item) :optional common-lisp:t
          :documentation "Additional opened cell text documents.")
         (did-close :type (lem-lsp-base/type:lsp-array text-document-identifier) :optional
          common-lisp:t :documentation "Additional closed cell text documents.")))
       :optional common-lisp:t :documentation "Changes to the cell structure to add or
remove cells.")
      (data :type (lem-lsp-base/type:lsp-array notebook-cell) :optional common-lisp:t
       :documentation "Changes to notebook cells properties like its
kind, execution summary or metadata.")
      (text-content :type
       (lem-lsp-base/type:lsp-array
        (lem-lsp-base/type:lsp-interface
         ((document :type versioned-text-document-identifier)
          (changes :type (lem-lsp-base/type:lsp-array text-document-content-change-event)))))
       :optional common-lisp:t :documentation "Changes to the text content of notebook cells.")))
    :initarg :cells :accessor notebook-document-change-event-cells :optional common-lisp:t
    :documentation "Changes to cells"))
  (:since "3.17.0")
  (:documentation "A change event for a notebook document.

@since 3.17.0"))

(lem-lsp-base/type:define-class notebook-document-identifier
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-uri :initarg :uri :accessor notebook-document-identifier-uri
    :documentation "The notebook document's uri."))
  (:since "3.17.0")
  (:documentation "A literal to identify a notebook document in the client.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-completion-context
    common-lisp:nil
  ((trigger-kind :type inline-completion-trigger-kind :initarg :trigger-kind :accessor
    inline-completion-context-trigger-kind :documentation
    "Describes how the inline completion was triggered.")
   (selected-completion-info :type selected-completion-info :initarg :selected-completion-info
    :accessor inline-completion-context-selected-completion-info :optional common-lisp:t
    :documentation
    "Provides information about the currently selected item in the autocomplete widget if it is visible."))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation
   "Provides information about the context in which an inline completion was requested.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class string-value
    common-lisp:nil
  ((kind :type (lem-lsp-base/type:lsp-string "snippet") :initarg :kind :accessor string-value-kind
    :documentation "The kind of string value.")
   (value :type lem-lsp-base/type:lsp-string :initarg :value :accessor string-value-value
    :documentation "The snippet string."))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "A string value used as a snippet is a template which allows to insert text
and to control the editor cursor when insertion happens.

A snippet can define tab stops and placeholders with `$1`, `$2`
and `${3:foo}`. `$0` defines the final tab stop, it defaults to
the end of the snippet. Variables are defined with `$name` and
`${name:default value}`.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class inline-completion-options
    (work-done-progress-options)
  common-lisp:nil
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "Inline completion options used during static registration.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class registration
    common-lisp:nil
  ((id :type lem-lsp-base/type:lsp-string :initarg :id :accessor registration-id :documentation
    "The id used to register the request. The id can be used to deregister
the request again.")
   (method :type lem-lsp-base/type:lsp-string :initarg :method :accessor registration-method
    :documentation "The method / capability to register for.")
   (register-options :type lsp-any :initarg :register-options :accessor
    registration-register-options :optional common-lisp:t :documentation
    "Options necessary for the registration."))
  (:documentation "General parameters to register for a notification or to register a provider."))

(lem-lsp-base/type:define-class unregistration
    common-lisp:nil
  ((id :type lem-lsp-base/type:lsp-string :initarg :id :accessor unregistration-id :documentation
    "The id used to unregister the request or notification. Usually an id
provided during the register request.")
   (method :type lem-lsp-base/type:lsp-string :initarg :method :accessor unregistration-method
    :documentation "The method to unregister for."))
  (:documentation "General parameters to unregister a request or notification."))

(lem-lsp-base/type:define-class _initialize-params
    (work-done-progress-params)
  ((process-id :type (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-null)
    :initarg :process-id :accessor _initialize-params-process-id :documentation
    "The process Id of the parent process that started
the server.

Is `null` if the process has not been started by another process.
If the parent process is not alive then the server should exit.")
   (client-info :type
    (lem-lsp-base/type:lsp-interface
     ((name :type lem-lsp-base/type:lsp-string :documentation
       "The name of the client as defined by the client.")
      (version :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
       "The client's version as defined by the client.")))
    :initarg :client-info :accessor _initialize-params-client-info :optional common-lisp:t :since
    "3.15.0" :documentation "Information about the client

@since 3.15.0")
   (locale :type lem-lsp-base/type:lsp-string :initarg :locale :accessor _initialize-params-locale
    :optional common-lisp:t :since "3.16.0" :documentation
    "The locale the client is currently showing the user interface
in. This must not necessarily be the locale of the operating
system.

Uses IETF language tags as the value's syntax
(See https://en.wikipedia.org/wiki/IETF_language_tag)

@since 3.16.0")
   (root-path :type (common-lisp:or lem-lsp-base/type:lsp-string lem-lsp-base/type:lsp-null)
    :initarg :root-path :accessor _initialize-params-root-path :optional common-lisp:t :deprecated
    "in favour of rootUri." :documentation "The rootPath of the workspace. Is null
if no folder is open.

@deprecated in favour of rootUri.")
   (root-uri :type (common-lisp:or lem-lsp-base/type:lsp-document-uri lem-lsp-base/type:lsp-null)
    :initarg :root-uri :accessor _initialize-params-root-uri :deprecated
    "in favour of workspaceFolders." :documentation "The rootUri of the workspace. Is null if no
folder is open. If both `rootPath` and `rootUri` are set
`rootUri` wins.

@deprecated in favour of workspaceFolders.")
   (capabilities :type client-capabilities :initarg :capabilities :accessor
    _initialize-params-capabilities :documentation
    "The capabilities provided by the client (editor or tool)")
   (initialization-options :type lsp-any :initarg :initialization-options :accessor
    _initialize-params-initialization-options :optional common-lisp:t :documentation
    "User provided initialization options.")
   (trace :type trace-values :initarg :trace :accessor _initialize-params-trace :optional
    common-lisp:t :documentation
    "The initial trace setting. If omitted trace is disabled ('off')."))
  (:documentation "The initialize parameters"))

(lem-lsp-base/type:define-class workspace-folders-initialize-params
    common-lisp:nil
  ((workspace-folders :type
    (common-lisp:or (lem-lsp-base/type:lsp-array workspace-folder) lem-lsp-base/type:lsp-null)
    :initarg :workspace-folders :accessor workspace-folders-initialize-params-workspace-folders
    :optional common-lisp:t :since "3.6.0" :documentation
    "The workspace folders configured in the client when the server starts.

This property is only available if the client supports workspace folders.
It can be `null` if the client supports workspace folders but none are
configured.

@since 3.6.0")))

(lem-lsp-base/type:define-class server-capabilities
    common-lisp:nil
  ((position-encoding :type position-encoding-kind :initarg :position-encoding :accessor
    server-capabilities-position-encoding :optional common-lisp:t :since "3.17.0" :documentation
    "The position encoding the server picked from the encodings offered
by the client via the client capability `general.positionEncodings`.

If the client didn't provide any position encodings the only valid
value that a server can return is 'utf-16'.

If omitted it defaults to 'utf-16'.

@since 3.17.0")
   (text-document-sync :type (common-lisp:or text-document-sync-options text-document-sync-kind)
    :initarg :text-document-sync :accessor server-capabilities-text-document-sync :optional
    common-lisp:t :documentation
    "Defines how text documents are synced. Is either a detailed structure
defining each notification or for backwards compatibility the
TextDocumentSyncKind number.")
   (notebook-document-sync :type
    (common-lisp:or notebook-document-sync-options notebook-document-sync-registration-options)
    :initarg :notebook-document-sync :accessor server-capabilities-notebook-document-sync :optional
    common-lisp:t :since "3.17.0" :documentation "Defines how notebook documents are synced.

@since 3.17.0")
   (completion-provider :type completion-options :initarg :completion-provider :accessor
    server-capabilities-completion-provider :optional common-lisp:t :documentation
    "The server provides completion support.")
   (hover-provider :type (common-lisp:or lem-lsp-base/type:lsp-boolean hover-options) :initarg
    :hover-provider :accessor server-capabilities-hover-provider :optional common-lisp:t
    :documentation "The server provides hover support.")
   (signature-help-provider :type signature-help-options :initarg :signature-help-provider
    :accessor server-capabilities-signature-help-provider :optional common-lisp:t :documentation
    "The server provides signature help support.")
   (declaration-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean declaration-options
                    declaration-registration-options)
    :initarg :declaration-provider :accessor server-capabilities-declaration-provider :optional
    common-lisp:t :documentation "The server provides Goto Declaration support.")
   (definition-provider :type (common-lisp:or lem-lsp-base/type:lsp-boolean definition-options)
    :initarg :definition-provider :accessor server-capabilities-definition-provider :optional
    common-lisp:t :documentation "The server provides goto definition support.")
   (type-definition-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean type-definition-options
                    type-definition-registration-options)
    :initarg :type-definition-provider :accessor server-capabilities-type-definition-provider
    :optional common-lisp:t :documentation "The server provides Goto Type Definition support.")
   (implementation-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean implementation-options
                    implementation-registration-options)
    :initarg :implementation-provider :accessor server-capabilities-implementation-provider
    :optional common-lisp:t :documentation "The server provides Goto Implementation support.")
   (references-provider :type (common-lisp:or lem-lsp-base/type:lsp-boolean reference-options)
    :initarg :references-provider :accessor server-capabilities-references-provider :optional
    common-lisp:t :documentation "The server provides find references support.")
   (document-highlight-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean document-highlight-options) :initarg
    :document-highlight-provider :accessor server-capabilities-document-highlight-provider
    :optional common-lisp:t :documentation "The server provides document highlight support.")
   (document-symbol-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean document-symbol-options) :initarg
    :document-symbol-provider :accessor server-capabilities-document-symbol-provider :optional
    common-lisp:t :documentation "The server provides document symbol support.")
   (code-action-provider :type (common-lisp:or lem-lsp-base/type:lsp-boolean code-action-options)
    :initarg :code-action-provider :accessor server-capabilities-code-action-provider :optional
    common-lisp:t :documentation "The server provides code actions. CodeActionOptions may only be
specified if the client states that it supports
`codeActionLiteralSupport` in its initial `initialize` request.")
   (code-lens-provider :type code-lens-options :initarg :code-lens-provider :accessor
    server-capabilities-code-lens-provider :optional common-lisp:t :documentation
    "The server provides code lens.")
   (document-link-provider :type document-link-options :initarg :document-link-provider :accessor
    server-capabilities-document-link-provider :optional common-lisp:t :documentation
    "The server provides document link support.")
   (color-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean document-color-options
                    document-color-registration-options)
    :initarg :color-provider :accessor server-capabilities-color-provider :optional common-lisp:t
    :documentation "The server provides color provider support.")
   (workspace-symbol-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean workspace-symbol-options) :initarg
    :workspace-symbol-provider :accessor server-capabilities-workspace-symbol-provider :optional
    common-lisp:t :documentation "The server provides workspace symbol support.")
   (document-formatting-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean document-formatting-options) :initarg
    :document-formatting-provider :accessor server-capabilities-document-formatting-provider
    :optional common-lisp:t :documentation "The server provides document formatting.")
   (document-range-formatting-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean document-range-formatting-options) :initarg
    :document-range-formatting-provider :accessor
    server-capabilities-document-range-formatting-provider :optional common-lisp:t :documentation
    "The server provides document range formatting.")
   (document-on-type-formatting-provider :type document-on-type-formatting-options :initarg
    :document-on-type-formatting-provider :accessor
    server-capabilities-document-on-type-formatting-provider :optional common-lisp:t :documentation
    "The server provides document formatting on typing.")
   (rename-provider :type (common-lisp:or lem-lsp-base/type:lsp-boolean rename-options) :initarg
    :rename-provider :accessor server-capabilities-rename-provider :optional common-lisp:t
    :documentation "The server provides rename support. RenameOptions may only be
specified if the client states that it supports
`prepareSupport` in its initial `initialize` request.")
   (folding-range-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean folding-range-options
                    folding-range-registration-options)
    :initarg :folding-range-provider :accessor server-capabilities-folding-range-provider :optional
    common-lisp:t :documentation "The server provides folding provider support.")
   (selection-range-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean selection-range-options
                    selection-range-registration-options)
    :initarg :selection-range-provider :accessor server-capabilities-selection-range-provider
    :optional common-lisp:t :documentation "The server provides selection range support.")
   (execute-command-provider :type execute-command-options :initarg :execute-command-provider
    :accessor server-capabilities-execute-command-provider :optional common-lisp:t :documentation
    "The server provides execute command support.")
   (call-hierarchy-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean call-hierarchy-options
                    call-hierarchy-registration-options)
    :initarg :call-hierarchy-provider :accessor server-capabilities-call-hierarchy-provider
    :optional common-lisp:t :since "3.16.0" :documentation
    "The server provides call hierarchy support.

@since 3.16.0")
   (linked-editing-range-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean linked-editing-range-options
                    linked-editing-range-registration-options)
    :initarg :linked-editing-range-provider :accessor
    server-capabilities-linked-editing-range-provider :optional common-lisp:t :since "3.16.0"
    :documentation "The server provides linked editing range support.

@since 3.16.0")
   (semantic-tokens-provider :type
    (common-lisp:or semantic-tokens-options semantic-tokens-registration-options) :initarg
    :semantic-tokens-provider :accessor server-capabilities-semantic-tokens-provider :optional
    common-lisp:t :since "3.16.0" :documentation "The server provides semantic tokens support.

@since 3.16.0")
   (moniker-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean moniker-options moniker-registration-options)
    :initarg :moniker-provider :accessor server-capabilities-moniker-provider :optional
    common-lisp:t :since "3.16.0" :documentation "The server provides moniker support.

@since 3.16.0")
   (type-hierarchy-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean type-hierarchy-options
                    type-hierarchy-registration-options)
    :initarg :type-hierarchy-provider :accessor server-capabilities-type-hierarchy-provider
    :optional common-lisp:t :since "3.17.0" :documentation
    "The server provides type hierarchy support.

@since 3.17.0")
   (inline-value-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean inline-value-options
                    inline-value-registration-options)
    :initarg :inline-value-provider :accessor server-capabilities-inline-value-provider :optional
    common-lisp:t :since "3.17.0" :documentation "The server provides inline values.

@since 3.17.0")
   (inlay-hint-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean inlay-hint-options
                    inlay-hint-registration-options)
    :initarg :inlay-hint-provider :accessor server-capabilities-inlay-hint-provider :optional
    common-lisp:t :since "3.17.0" :documentation "The server provides inlay hints.

@since 3.17.0")
   (diagnostic-provider :type (common-lisp:or diagnostic-options diagnostic-registration-options)
    :initarg :diagnostic-provider :accessor server-capabilities-diagnostic-provider :optional
    common-lisp:t :since "3.17.0" :documentation "The server has support for pull model diagnostics.

@since 3.17.0")
   (inline-completion-provider :type
    (common-lisp:or lem-lsp-base/type:lsp-boolean inline-completion-options) :initarg
    :inline-completion-provider :accessor server-capabilities-inline-completion-provider :optional
    common-lisp:t :proposed common-lisp:t :since "3.18.0" :documentation
    "Inline completion options used during static registration.

@since 3.18.0
@proposed")
   (workspace :type
    (lem-lsp-base/type:lsp-interface
     ((workspace-folders :type workspace-folders-server-capabilities :optional common-lisp:t :since
       "3.6.0" :documentation "The server supports workspace folder.

@since 3.6.0")
      (file-operations :type file-operation-options :optional common-lisp:t :since "3.16.0"
       :documentation "The server is interested in notifications/requests for operations on files.

@since 3.16.0")))
    :initarg :workspace :accessor server-capabilities-workspace :optional common-lisp:t
    :documentation "Workspace specific server capabilities.")
   (experimental :type lsp-any :initarg :experimental :accessor server-capabilities-experimental
    :optional common-lisp:t :documentation "Experimental server capabilities."))
  (:documentation "Defines the capabilities provided by a language
server."))

(lem-lsp-base/type:define-class versioned-text-document-identifier
    (text-document-identifier)
  ((version :type lem-lsp-base/type:lsp-integer :initarg :version :accessor
    versioned-text-document-identifier-version :documentation
    "The version number of this document."))
  (:documentation "A text document identifier to denote a specific version of a text document."))

(lem-lsp-base/type:define-class save-options
    common-lisp:nil
  ((include-text :type lem-lsp-base/type:lsp-boolean :initarg :include-text :accessor
    save-options-include-text :optional common-lisp:t :documentation
    "The client is supposed to include the content on save."))
  (:documentation "Save options."))

(lem-lsp-base/type:define-class file-event
    common-lisp:nil
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor file-event-uri
    :documentation "The file's uri.")
   (type :type file-change-type :initarg :type :accessor file-event-type :documentation
    "The change type."))
  (:documentation "An event describing a file change."))

(lem-lsp-base/type:define-class file-system-watcher
    common-lisp:nil
  ((glob-pattern :type glob-pattern :initarg :glob-pattern :accessor
    file-system-watcher-glob-pattern :since "3.17.0 support for relative patterns." :documentation
    "The glob pattern to watch. See {@link GlobPattern glob pattern} for more detail.

@since 3.17.0 support for relative patterns.")
   (kind :type watch-kind :initarg :kind :accessor file-system-watcher-kind :optional common-lisp:t
    :documentation "The kind of events of interest. If omitted it defaults
to WatchKind.Create | WatchKind.Change | WatchKind.Delete
which is 7.")))

(lem-lsp-base/type:define-class diagnostic
    common-lisp:nil
  ((range :type range :initarg :range :accessor diagnostic-range :documentation
    "The range at which the message applies")
   (severity :type diagnostic-severity :initarg :severity :accessor diagnostic-severity :optional
    common-lisp:t :documentation
    "The diagnostic's severity. Can be omitted. If omitted it is up to the
client to interpret diagnostics as error, warning, info or hint.")
   (code :type (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-string) :initarg
    :code :accessor diagnostic-code :optional common-lisp:t :documentation
    "The diagnostic's code, which usually appear in the user interface.")
   (code-description :type code-description :initarg :code-description :accessor
    diagnostic-code-description :optional common-lisp:t :since "3.16.0" :documentation
    "An optional property to describe the error code.
Requires the code field (above) to be present/not null.

@since 3.16.0")
   (source :type lem-lsp-base/type:lsp-string :initarg :source :accessor diagnostic-source
    :optional common-lisp:t :documentation "A human-readable string describing the source of this
diagnostic, e.g. 'typescript' or 'super lint'. It usually
appears in the user interface.")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor diagnostic-message
    :documentation "The diagnostic's message. It usually appears in the user interface")
   (tags :type (lem-lsp-base/type:lsp-array diagnostic-tag) :initarg :tags :accessor
    diagnostic-tags :optional common-lisp:t :since "3.15.0" :documentation
    "Additional metadata about the diagnostic.

@since 3.15.0")
   (related-information :type (lem-lsp-base/type:lsp-array diagnostic-related-information) :initarg
    :related-information :accessor diagnostic-related-information :optional common-lisp:t
    :documentation "An array of related diagnostic information, e.g. when symbol-names within
a scope collide all definitions can be marked via this property.")
   (data :type lsp-any :initarg :data :accessor diagnostic-data :optional common-lisp:t :since
    "3.16.0" :documentation
    "A data entry field that is preserved between a `textDocument/publishDiagnostics`
notification and `textDocument/codeAction` request.

@since 3.16.0"))
  (:documentation "Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
are only valid in the scope of a resource."))

(lem-lsp-base/type:define-class completion-context
    common-lisp:nil
  ((trigger-kind :type completion-trigger-kind :initarg :trigger-kind :accessor
    completion-context-trigger-kind :documentation "How the completion was triggered.")
   (trigger-character :type lem-lsp-base/type:lsp-string :initarg :trigger-character :accessor
    completion-context-trigger-character :optional common-lisp:t :documentation
    "The trigger character (a single character) that has trigger code complete.
Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`"))
  (:documentation
   "Contains additional information about the context in which a completion request is triggered."))

(lem-lsp-base/type:define-class completion-item-label-details
    common-lisp:nil
  ((detail :type lem-lsp-base/type:lsp-string :initarg :detail :accessor
    completion-item-label-details-detail :optional common-lisp:t :documentation
    "An optional string which is rendered less prominently directly after {@link CompletionItem.label label},
without any spacing. Should be used for function signatures and type annotations.")
   (description :type lem-lsp-base/type:lsp-string :initarg :description :accessor
    completion-item-label-details-description :optional common-lisp:t :documentation
    "An optional string which is rendered less prominently after {@link CompletionItem.detail}. Should be used
for fully qualified names and file paths."))
  (:since "3.17.0")
  (:documentation "Additional details for a completion item label.

@since 3.17.0"))

(lem-lsp-base/type:define-class insert-replace-edit
    common-lisp:nil
  ((new-text :type lem-lsp-base/type:lsp-string :initarg :new-text :accessor
    insert-replace-edit-new-text :documentation "The string to be inserted.")
   (insert :type range :initarg :insert :accessor insert-replace-edit-insert :documentation
    "The range if the insert is requested")
   (replace :type range :initarg :replace :accessor insert-replace-edit-replace :documentation
    "The range if the replace is requested."))
  (:since "3.16.0")
  (:documentation "A special text edit to provide an insert and a replace operation.

@since 3.16.0"))

(lem-lsp-base/type:define-class completion-options
    (work-done-progress-options)
  ((trigger-characters :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :trigger-characters :accessor completion-options-trigger-characters :optional common-lisp:t
    :documentation
    "Most tools trigger completion request automatically without explicitly requesting
it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
starts to type an identifier. For example if the user types `c` in a JavaScript file
code complete will automatically pop up present `console` besides others as a
completion item. Characters that make up identifiers don't need to be listed here.

If code complete should automatically be trigger on characters not being valid inside
an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.")
   (all-commit-characters :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :all-commit-characters :accessor completion-options-all-commit-characters :optional
    common-lisp:t :since "3.2.0" :documentation
    "The list of all possible characters that commit a completion. This field can be used
if clients don't support individual commit characters per completion item. See
`ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`

If a server provides both `allCommitCharacters` and commit characters on an individual
completion item the ones on the completion item win.

@since 3.2.0")
   (resolve-provider :type lem-lsp-base/type:lsp-boolean :initarg :resolve-provider :accessor
    completion-options-resolve-provider :optional common-lisp:t :documentation
    "The server provides support to resolve additional
information for a completion item.")
   (completion-item :type
    (lem-lsp-base/type:lsp-interface
     ((label-details-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :since
       "3.17.0" :documentation "The server has support for completion item label
details (see also `CompletionItemLabelDetails`) when
receiving a completion item in a resolve call.

@since 3.17.0")))
    :initarg :completion-item :accessor completion-options-completion-item :optional common-lisp:t
    :since "3.17.0" :documentation "The server supports the following `CompletionItem` specific
capabilities.

@since 3.17.0"))
  (:documentation "Completion options."))

(lem-lsp-base/type:define-class hover-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Hover options."))

(lem-lsp-base/type:define-class signature-help-context
    common-lisp:nil
  ((trigger-kind :type signature-help-trigger-kind :initarg :trigger-kind :accessor
    signature-help-context-trigger-kind :documentation
    "Action that caused signature help to be triggered.")
   (trigger-character :type lem-lsp-base/type:lsp-string :initarg :trigger-character :accessor
    signature-help-context-trigger-character :optional common-lisp:t :documentation
    "Character that caused signature help to be triggered.

This is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`")
   (is-retrigger :type lem-lsp-base/type:lsp-boolean :initarg :is-retrigger :accessor
    signature-help-context-is-retrigger :documentation
    "`true` if signature help was already showing when it was triggered.

Retriggers occurs when the signature help is already active and can be caused by actions such as
typing a trigger character, a cursor move, or document content changes.")
   (active-signature-help :type signature-help :initarg :active-signature-help :accessor
    signature-help-context-active-signature-help :optional common-lisp:t :documentation
    "The currently active `SignatureHelp`.

The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on
the user navigating through available signatures."))
  (:since "3.15.0")
  (:documentation
   "Additional information about the context in which a signature help request was triggered.

@since 3.15.0"))

(lem-lsp-base/type:define-class signature-information
    common-lisp:nil
  ((label :type lem-lsp-base/type:lsp-string :initarg :label :accessor signature-information-label
    :documentation "The label of this signature. Will be shown in
the UI.")
   (documentation :type (common-lisp:or lem-lsp-base/type:lsp-string markup-content) :initarg
    :documentation :accessor signature-information-documentation :optional common-lisp:t
    :documentation "The human-readable doc-comment of this signature. Will be shown
in the UI but can be omitted.")
   (parameters :type (lem-lsp-base/type:lsp-array parameter-information) :initarg :parameters
    :accessor signature-information-parameters :optional common-lisp:t :documentation
    "The parameters of this signature.")
   (active-parameter :type lem-lsp-base/type:lsp-uinteger :initarg :active-parameter :accessor
    signature-information-active-parameter :optional common-lisp:t :since "3.16.0" :documentation
    "The index of the active parameter.

If provided, this is used in place of `SignatureHelp.activeParameter`.

@since 3.16.0"))
  (:documentation "Represents the signature of something callable. A signature
can have a label, like a function-name, a doc-comment, and
a set of parameters."))

(lem-lsp-base/type:define-class signature-help-options
    (work-done-progress-options)
  ((trigger-characters :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :trigger-characters :accessor signature-help-options-trigger-characters :optional common-lisp:t
    :documentation "List of characters that trigger signature help automatically.")
   (retrigger-characters :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :retrigger-characters :accessor signature-help-options-retrigger-characters :optional
    common-lisp:t :since "3.15.0" :documentation "List of characters that re-trigger signature help.

These trigger characters are only active when signature help is already showing. All trigger characters
are also counted as re-trigger characters.

@since 3.15.0"))
  (:documentation "Server Capabilities for a {@link SignatureHelpRequest}."))

(lem-lsp-base/type:define-class definition-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Server Capabilities for a {@link DefinitionRequest}."))

(lem-lsp-base/type:define-class reference-context
    common-lisp:nil
  ((include-declaration :type lem-lsp-base/type:lsp-boolean :initarg :include-declaration :accessor
    reference-context-include-declaration :documentation
    "Include the declaration of the current symbol."))
  (:documentation "Value-object that contains additional information when
requesting references."))

(lem-lsp-base/type:define-class reference-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Reference options."))

(lem-lsp-base/type:define-class document-highlight-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Provider options for a {@link DocumentHighlightRequest}."))

(lem-lsp-base/type:define-class base-symbol-information
    common-lisp:nil
  ((name :type lem-lsp-base/type:lsp-string :initarg :name :accessor base-symbol-information-name
    :documentation "The name of this symbol.")
   (kind :type symbol-kind :initarg :kind :accessor base-symbol-information-kind :documentation
    "The kind of this symbol.")
   (tags :type (lem-lsp-base/type:lsp-array symbol-tag) :initarg :tags :accessor
    base-symbol-information-tags :optional common-lisp:t :since "3.16.0" :documentation
    "Tags for this symbol.

@since 3.16.0")
   (container-name :type lem-lsp-base/type:lsp-string :initarg :container-name :accessor
    base-symbol-information-container-name :optional common-lisp:t :documentation
    "The name of the symbol containing this symbol. This information is for
user interface purposes (e.g. to render a qualifier in the user interface
if necessary). It can't be used to re-infer a hierarchy for the document
symbols."))
  (:documentation "A base for all symbol information."))

(lem-lsp-base/type:define-class document-symbol-options
    (work-done-progress-options)
  ((label :type lem-lsp-base/type:lsp-string :initarg :label :accessor
    document-symbol-options-label :optional common-lisp:t :since "3.16.0" :documentation
    "A human-readable string that is shown when multiple outlines trees
are shown for the same document.

@since 3.16.0"))
  (:documentation "Provider options for a {@link DocumentSymbolRequest}."))

(lem-lsp-base/type:define-class code-action-context
    common-lisp:nil
  ((diagnostics :type (lem-lsp-base/type:lsp-array diagnostic) :initarg :diagnostics :accessor
    code-action-context-diagnostics :documentation
    "An array of diagnostics known on the client side overlapping the range provided to the
`textDocument/codeAction` request. They are provided so that the server knows which
errors are currently presented to the user for the given range. There is no guarantee
that these accurately reflect the error state of the resource. The primary parameter
to compute code actions is the provided range.")
   (only :type (lem-lsp-base/type:lsp-array code-action-kind) :initarg :only :accessor
    code-action-context-only :optional common-lisp:t :documentation
    "Requested kind of actions to return.

Actions not of this kind are filtered out by the client before being shown. So servers
can omit computing them.")
   (trigger-kind :type code-action-trigger-kind :initarg :trigger-kind :accessor
    code-action-context-trigger-kind :optional common-lisp:t :since "3.17.0" :documentation
    "The reason why code actions were requested.

@since 3.17.0"))
  (:documentation "Contains additional diagnostic information about the context in which
a {@link CodeActionProvider.provideCodeActions code action} is run."))

(lem-lsp-base/type:define-class code-action-options
    (work-done-progress-options)
  ((code-action-kinds :type (lem-lsp-base/type:lsp-array code-action-kind) :initarg
    :code-action-kinds :accessor code-action-options-code-action-kinds :optional common-lisp:t
    :documentation "CodeActionKinds that this server may return.

The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
may list out every specific kind they provide.")
   (resolve-provider :type lem-lsp-base/type:lsp-boolean :initarg :resolve-provider :accessor
    code-action-options-resolve-provider :optional common-lisp:t :since "3.16.0" :documentation
    "The server provides support to resolve additional
information for a code action.

@since 3.16.0"))
  (:documentation "Provider options for a {@link CodeActionRequest}."))

(lem-lsp-base/type:define-class workspace-symbol-options
    (work-done-progress-options)
  ((resolve-provider :type lem-lsp-base/type:lsp-boolean :initarg :resolve-provider :accessor
    workspace-symbol-options-resolve-provider :optional common-lisp:t :since "3.17.0"
    :documentation "The server provides support to resolve additional
information for a workspace symbol.

@since 3.17.0"))
  (:documentation "Server capabilities for a {@link WorkspaceSymbolRequest}."))

(lem-lsp-base/type:define-class code-lens-options
    (work-done-progress-options)
  ((resolve-provider :type lem-lsp-base/type:lsp-boolean :initarg :resolve-provider :accessor
    code-lens-options-resolve-provider :optional common-lisp:t :documentation
    "Code lens has a resolve provider as well."))
  (:documentation "Code Lens provider options of a {@link CodeLensRequest}."))

(lem-lsp-base/type:define-class document-link-options
    (work-done-progress-options)
  ((resolve-provider :type lem-lsp-base/type:lsp-boolean :initarg :resolve-provider :accessor
    document-link-options-resolve-provider :optional common-lisp:t :documentation
    "Document links have a resolve provider as well."))
  (:documentation "Provider options for a {@link DocumentLinkRequest}."))

(lem-lsp-base/type:define-class formatting-options
    common-lisp:nil
  ((tab-size :type lem-lsp-base/type:lsp-uinteger :initarg :tab-size :accessor
    formatting-options-tab-size :documentation "Size of a tab in spaces.")
   (insert-spaces :type lem-lsp-base/type:lsp-boolean :initarg :insert-spaces :accessor
    formatting-options-insert-spaces :documentation "Prefer spaces over tabs.")
   (trim-trailing-whitespace :type lem-lsp-base/type:lsp-boolean :initarg :trim-trailing-whitespace
    :accessor formatting-options-trim-trailing-whitespace :optional common-lisp:t :since "3.15.0"
    :documentation "Trim trailing whitespace on a line.

@since 3.15.0")
   (insert-final-newline :type lem-lsp-base/type:lsp-boolean :initarg :insert-final-newline
    :accessor formatting-options-insert-final-newline :optional common-lisp:t :since "3.15.0"
    :documentation "Insert a newline character at the end of the file if one does not exist.

@since 3.15.0")
   (trim-final-newlines :type lem-lsp-base/type:lsp-boolean :initarg :trim-final-newlines :accessor
    formatting-options-trim-final-newlines :optional common-lisp:t :since "3.15.0" :documentation
    "Trim all newlines after the final newline at the end of the file.

@since 3.15.0"))
  (:documentation "Value-object describing what options formatting should use."))

(lem-lsp-base/type:define-class document-formatting-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Provider options for a {@link DocumentFormattingRequest}."))

(lem-lsp-base/type:define-class document-range-formatting-options
    (work-done-progress-options)
  ((ranges-support :type lem-lsp-base/type:lsp-boolean :initarg :ranges-support :accessor
    document-range-formatting-options-ranges-support :optional common-lisp:t :proposed
    common-lisp:t :since "3.18.0" :documentation
    "Whether the server supports formatting multiple ranges at once.

@since 3.18.0
@proposed"))
  (:documentation "Provider options for a {@link DocumentRangeFormattingRequest}."))

(lem-lsp-base/type:define-class document-on-type-formatting-options
    common-lisp:nil
  ((first-trigger-character :type lem-lsp-base/type:lsp-string :initarg :first-trigger-character
    :accessor document-on-type-formatting-options-first-trigger-character :documentation
    "A character on which formatting should be triggered, like `{`.")
   (more-trigger-character :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string)
    :initarg :more-trigger-character :accessor
    document-on-type-formatting-options-more-trigger-character :optional common-lisp:t
    :documentation "More trigger characters."))
  (:documentation "Provider options for a {@link DocumentOnTypeFormattingRequest}."))

(lem-lsp-base/type:define-class rename-options
    (work-done-progress-options)
  ((prepare-provider :type lem-lsp-base/type:lsp-boolean :initarg :prepare-provider :accessor
    rename-options-prepare-provider :optional common-lisp:t :since "version 3.12.0" :documentation
    "Renames should be checked and tested before being executed.

@since version 3.12.0"))
  (:documentation "Provider options for a {@link RenameRequest}."))

(lem-lsp-base/type:define-class execute-command-options
    (work-done-progress-options)
  ((commands :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg :commands
    :accessor execute-command-options-commands :documentation
    "The commands to be executed on the server"))
  (:documentation "The server capabilities of a {@link ExecuteCommandRequest}."))

(lem-lsp-base/type:define-class semantic-tokens-legend
    common-lisp:nil
  ((token-types :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :token-types :accessor semantic-tokens-legend-token-types :documentation
    "The token types a server uses.")
   (token-modifiers :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :token-modifiers :accessor semantic-tokens-legend-token-modifiers :documentation
    "The token modifiers a server uses."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class optional-versioned-text-document-identifier
    (text-document-identifier)
  ((version :type (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-null)
    :initarg :version :accessor optional-versioned-text-document-identifier-version :documentation
    "The version number of this document. If a versioned text document identifier
is sent from the server to the client and the file is not open in the editor
(the server has not received an open notification before) the server can send
`null` to indicate that the version is unknown and the content on disk is the
truth (as specified with document content ownership)."))
  (:documentation
   "A text document identifier to optionally denote a specific version of a text document."))

(lem-lsp-base/type:define-class annotated-text-edit
    (text-edit)
  ((annotation-id :type change-annotation-identifier :initarg :annotation-id :accessor
    annotated-text-edit-annotation-id :documentation
    "The actual identifier of the change annotation"))
  (:since "3.16.0.")
  (:documentation "A special text edit with an additional change annotation.

@since 3.16.0."))

(lem-lsp-base/type:define-class resource-operation
    common-lisp:nil
  ((kind :type lem-lsp-base/type:lsp-string :initarg :kind :accessor resource-operation-kind
    :documentation "The resource operation kind.")
   (annotation-id :type change-annotation-identifier :initarg :annotation-id :accessor
    resource-operation-annotation-id :optional common-lisp:t :since "3.16.0" :documentation
    "An optional annotation identifier describing the operation.

@since 3.16.0"))
  (:documentation "A generic resource operation."))

(lem-lsp-base/type:define-class create-file-options
    common-lisp:nil
  ((overwrite :type lem-lsp-base/type:lsp-boolean :initarg :overwrite :accessor
    create-file-options-overwrite :optional common-lisp:t :documentation
    "Overwrite existing file. Overwrite wins over `ignoreIfExists`")
   (ignore-if-exists :type lem-lsp-base/type:lsp-boolean :initarg :ignore-if-exists :accessor
    create-file-options-ignore-if-exists :optional common-lisp:t :documentation
    "Ignore if exists."))
  (:documentation "Options to create a file."))

(lem-lsp-base/type:define-class rename-file-options
    common-lisp:nil
  ((overwrite :type lem-lsp-base/type:lsp-boolean :initarg :overwrite :accessor
    rename-file-options-overwrite :optional common-lisp:t :documentation
    "Overwrite target if existing. Overwrite wins over `ignoreIfExists`")
   (ignore-if-exists :type lem-lsp-base/type:lsp-boolean :initarg :ignore-if-exists :accessor
    rename-file-options-ignore-if-exists :optional common-lisp:t :documentation
    "Ignores if target exists."))
  (:documentation "Rename file options"))

(lem-lsp-base/type:define-class delete-file-options
    common-lisp:nil
  ((recursive :type lem-lsp-base/type:lsp-boolean :initarg :recursive :accessor
    delete-file-options-recursive :optional common-lisp:t :documentation
    "Delete the content recursively if a folder is denoted.")
   (ignore-if-not-exists :type lem-lsp-base/type:lsp-boolean :initarg :ignore-if-not-exists
    :accessor delete-file-options-ignore-if-not-exists :optional common-lisp:t :documentation
    "Ignore the operation if the file doesn't exist."))
  (:documentation "Delete file options"))

(lem-lsp-base/type:define-class file-operation-pattern
    common-lisp:nil
  ((glob :type lem-lsp-base/type:lsp-string :initarg :glob :accessor file-operation-pattern-glob
    :documentation "The glob pattern to match. Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)")
   (matches :type file-operation-pattern-kind :initarg :matches :accessor
    file-operation-pattern-matches :optional common-lisp:t :documentation
    "Whether to match files or folders with this pattern.

Matches both if undefined.")
   (options :type file-operation-pattern-options :initarg :options :accessor
    file-operation-pattern-options :optional common-lisp:t :documentation
    "Additional options used during matching."))
  (:since "3.16.0")
  (:documentation "A pattern to describe in which file operation requests or notifications
the server is interested in receiving.

@since 3.16.0"))

(lem-lsp-base/type:define-class workspace-full-document-diagnostic-report
    (full-document-diagnostic-report)
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor
    workspace-full-document-diagnostic-report-uri :documentation
    "The URI for which diagnostic information is reported.")
   (version :type (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-null)
    :initarg :version :accessor workspace-full-document-diagnostic-report-version :documentation
    "The version number for which the diagnostics are reported.
If the document is not marked as open `null` can be provided."))
  (:since "3.17.0")
  (:documentation "A full document diagnostic report for a workspace diagnostic result.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-unchanged-document-diagnostic-report
    (unchanged-document-diagnostic-report)
  ((uri :type lem-lsp-base/type:lsp-document-uri :initarg :uri :accessor
    workspace-unchanged-document-diagnostic-report-uri :documentation
    "The URI for which diagnostic information is reported.")
   (version :type (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-null)
    :initarg :version :accessor workspace-unchanged-document-diagnostic-report-version
    :documentation "The version number for which the diagnostics are reported.
If the document is not marked as open `null` can be provided."))
  (:since "3.17.0")
  (:documentation "An unchanged document diagnostic report for a workspace diagnostic result.

@since 3.17.0"))

(lem-lsp-base/type:define-class notebook-cell
    common-lisp:nil
  ((kind :type notebook-cell-kind :initarg :kind :accessor notebook-cell-kind :documentation
    "The cell's kind")
   (document :type lem-lsp-base/type:lsp-document-uri :initarg :document :accessor
    notebook-cell-document :documentation "The URI of the cell's text document
content.")
   (metadata :type lsp-object :initarg :metadata :accessor notebook-cell-metadata :optional
    common-lisp:t :documentation "Additional metadata stored with the cell.

Note: should always be an object literal (e.g. LSPObject)")
   (execution-summary :type execution-summary :initarg :execution-summary :accessor
    notebook-cell-execution-summary :optional common-lisp:t :documentation
    "Additional execution summary information
if supported by the client."))
  (:since "3.17.0")
  (:documentation "A notebook cell.

A cell's document URI must be unique across ALL notebook
cells and can therefore be used to uniquely identify a
notebook cell or the cell's text document.

@since 3.17.0"))

(lem-lsp-base/type:define-class notebook-cell-array-change
    common-lisp:nil
  ((start :type lem-lsp-base/type:lsp-uinteger :initarg :start :accessor
    notebook-cell-array-change-start :documentation "The start oftest of the cell that changed.")
   (delete-count :type lem-lsp-base/type:lsp-uinteger :initarg :delete-count :accessor
    notebook-cell-array-change-delete-count :documentation "The deleted cells")
   (cells :type (lem-lsp-base/type:lsp-array notebook-cell) :initarg :cells :accessor
    notebook-cell-array-change-cells :optional common-lisp:t :documentation
    "The new cells, if any"))
  (:since "3.17.0")
  (:documentation "A change describing how to move a `NotebookCell`
array from state S to S'.

@since 3.17.0"))

(lem-lsp-base/type:define-class selected-completion-info
    common-lisp:nil
  ((range :type range :initarg :range :accessor selected-completion-info-range :documentation
    "The range that will be replaced if this completion item is accepted.")
   (text :type lem-lsp-base/type:lsp-string :initarg :text :accessor selected-completion-info-text
    :documentation "The text the range will be replaced with if this completion is accepted."))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "Describes the currently selected completion item.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class client-capabilities
    common-lisp:nil
  ((workspace :type workspace-client-capabilities :initarg :workspace :accessor
    client-capabilities-workspace :optional common-lisp:t :documentation
    "Workspace specific client capabilities.")
   (text-document :type text-document-client-capabilities :initarg :text-document :accessor
    client-capabilities-text-document :optional common-lisp:t :documentation
    "Text document specific client capabilities.")
   (notebook-document :type notebook-document-client-capabilities :initarg :notebook-document
    :accessor client-capabilities-notebook-document :optional common-lisp:t :since "3.17.0"
    :documentation "Capabilities specific to the notebook document support.

@since 3.17.0")
   (window :type window-client-capabilities :initarg :window :accessor client-capabilities-window
    :optional common-lisp:t :documentation "Window specific client capabilities.")
   (general :type general-client-capabilities :initarg :general :accessor
    client-capabilities-general :optional common-lisp:t :since "3.16.0" :documentation
    "General client capabilities.

@since 3.16.0")
   (experimental :type lsp-any :initarg :experimental :accessor client-capabilities-experimental
    :optional common-lisp:t :documentation "Experimental client capabilities."))
  (:documentation "Defines the capabilities provided by the client."))

(lem-lsp-base/type:define-class text-document-sync-options
    common-lisp:nil
  ((open-close :type lem-lsp-base/type:lsp-boolean :initarg :open-close :accessor
    text-document-sync-options-open-close :optional common-lisp:t :documentation
    "Open and close notifications are sent to the server. If omitted open close notification should not
be sent.")
   (change :type text-document-sync-kind :initarg :change :accessor
    text-document-sync-options-change :optional common-lisp:t :documentation
    "Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.")
   (will-save :type lem-lsp-base/type:lsp-boolean :initarg :will-save :accessor
    text-document-sync-options-will-save :optional common-lisp:t :documentation
    "If present will save notifications are sent to the server. If omitted the notification should not be
sent.")
   (will-save-wait-until :type lem-lsp-base/type:lsp-boolean :initarg :will-save-wait-until
    :accessor text-document-sync-options-will-save-wait-until :optional common-lisp:t
    :documentation
    "If present will save wait until requests are sent to the server. If omitted the request should not be
sent.")
   (save :type (common-lisp:or lem-lsp-base/type:lsp-boolean save-options) :initarg :save :accessor
    text-document-sync-options-save :optional common-lisp:t :documentation
    "If present save notifications are sent to the server. If omitted the notification should not be
sent.")))

(lem-lsp-base/type:define-class notebook-document-sync-options
    common-lisp:nil
  ((notebook-selector :type
    (lem-lsp-base/type:lsp-array
     (common-lisp:or
      (lem-lsp-base/type:lsp-interface
       ((notebook :type (common-lisp:or lem-lsp-base/type:lsp-string notebook-document-filter)
         :documentation "The notebook to be synced If a string
value is provided it matches against the
notebook type. '*' matches every notebook.")
        (cells :type
         (lem-lsp-base/type:lsp-array
          (lem-lsp-base/type:lsp-interface ((language :type lem-lsp-base/type:lsp-string))))
         :optional common-lisp:t :documentation
         "The cells of the matching notebook to be synced.")))
      (lem-lsp-base/type:lsp-interface
       ((notebook :type (common-lisp:or lem-lsp-base/type:lsp-string notebook-document-filter)
         :optional common-lisp:t :documentation "The notebook to be synced If a string
value is provided it matches against the
notebook type. '*' matches every notebook.")
        (cells :type
         (lem-lsp-base/type:lsp-array
          (lem-lsp-base/type:lsp-interface ((language :type lem-lsp-base/type:lsp-string))))
         :documentation "The cells of the matching notebook to be synced.")))))
    :initarg :notebook-selector :accessor notebook-document-sync-options-notebook-selector
    :documentation "The notebooks to be synced")
   (save :type lem-lsp-base/type:lsp-boolean :initarg :save :accessor
    notebook-document-sync-options-save :optional common-lisp:t :documentation
    "Whether save notification should be forwarded to
the server. Will only be honored if mode === `notebook`."))
  (:since "3.17.0")
  (:documentation "Options specific to a notebook plus its cells
to be synced to the server.

If a selector provides a notebook document
filter but no cell selector all cells of a
matching notebook document will be synced.

If a selector provides no notebook document
filter but only a cell selector all notebook
document that contain at least one matching
cell will be synced.

@since 3.17.0"))

(lem-lsp-base/type:define-class notebook-document-sync-registration-options
    (notebook-document-sync-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Registration options specific to a notebook.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-folders-server-capabilities
    common-lisp:nil
  ((supported :type lem-lsp-base/type:lsp-boolean :initarg :supported :accessor
    workspace-folders-server-capabilities-supported :optional common-lisp:t :documentation
    "The server has support for workspace folders")
   (change-notifications :type
    (common-lisp:or lem-lsp-base/type:lsp-string lem-lsp-base/type:lsp-boolean) :initarg
    :change-notifications :accessor workspace-folders-server-capabilities-change-notifications
    :optional common-lisp:t :documentation "Whether the server wants to receive workspace folder
change notifications.

If a string is provided the string is treated as an ID
under which the notification is registered on the client
side. The ID can be used to unregister for these events
using the `client/unregisterCapability` request.")))

(lem-lsp-base/type:define-class file-operation-options
    common-lisp:nil
  ((did-create :type file-operation-registration-options :initarg :did-create :accessor
    file-operation-options-did-create :optional common-lisp:t :documentation
    "The server is interested in receiving didCreateFiles notifications.")
   (will-create :type file-operation-registration-options :initarg :will-create :accessor
    file-operation-options-will-create :optional common-lisp:t :documentation
    "The server is interested in receiving willCreateFiles requests.")
   (did-rename :type file-operation-registration-options :initarg :did-rename :accessor
    file-operation-options-did-rename :optional common-lisp:t :documentation
    "The server is interested in receiving didRenameFiles notifications.")
   (will-rename :type file-operation-registration-options :initarg :will-rename :accessor
    file-operation-options-will-rename :optional common-lisp:t :documentation
    "The server is interested in receiving willRenameFiles requests.")
   (did-delete :type file-operation-registration-options :initarg :did-delete :accessor
    file-operation-options-did-delete :optional common-lisp:t :documentation
    "The server is interested in receiving didDeleteFiles file notifications.")
   (will-delete :type file-operation-registration-options :initarg :will-delete :accessor
    file-operation-options-will-delete :optional common-lisp:t :documentation
    "The server is interested in receiving willDeleteFiles file requests."))
  (:since "3.16.0")
  (:documentation "Options for notifications/requests for user operations on files.

@since 3.16.0"))

(lem-lsp-base/type:define-class code-description
    common-lisp:nil
  ((href :type lem-lsp-base/type:lsp-uri :initarg :href :accessor code-description-href
    :documentation "An URI to open with more information about the diagnostic error."))
  (:since "3.16.0")
  (:documentation "Structure to capture a description for an error code.

@since 3.16.0"))

(lem-lsp-base/type:define-class diagnostic-related-information
    common-lisp:nil
  ((location :type location :initarg :location :accessor diagnostic-related-information-location
    :documentation "The location of this related diagnostic information.")
   (message :type lem-lsp-base/type:lsp-string :initarg :message :accessor
    diagnostic-related-information-message :documentation
    "The message of this related diagnostic information."))
  (:documentation
   "Represents a related message and source code location for a diagnostic. This should be
used to point to code locations that cause or related to a diagnostics, e.g when duplicating
a symbol in a scope."))

(lem-lsp-base/type:define-class parameter-information
    common-lisp:nil
  ((label :type
    (common-lisp:or lem-lsp-base/type:lsp-string
                    (lem-lsp-base/type:lsp-tuple lem-lsp-base/type:lsp-uinteger
                     lem-lsp-base/type:lsp-uinteger))
    :initarg :label :accessor parameter-information-label :documentation
    "The label of this parameter information.

Either a string or an inclusive start and exclusive end offsets within its containing
signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
string representation as `Position` and `Range` does.

*Note*: a label of type string should be a substring of its containing signature label.
Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.")
   (documentation :type (common-lisp:or lem-lsp-base/type:lsp-string markup-content) :initarg
    :documentation :accessor parameter-information-documentation :optional common-lisp:t
    :documentation "The human-readable doc-comment of this parameter. Will be shown
in the UI but can be omitted."))
  (:documentation "Represents a parameter of a callable-signature. A parameter can
have a label and a doc-comment."))

(lem-lsp-base/type:define-class notebook-cell-text-document-filter
    common-lisp:nil
  ((notebook :type (common-lisp:or lem-lsp-base/type:lsp-string notebook-document-filter) :initarg
    :notebook :accessor notebook-cell-text-document-filter-notebook :documentation
    "A filter that matches against the notebook
containing the notebook cell. If a string
value is provided it matches against the
notebook type. '*' matches every notebook.")
   (language :type lem-lsp-base/type:lsp-string :initarg :language :accessor
    notebook-cell-text-document-filter-language :optional common-lisp:t :documentation
    "A language id like `python`.

Will be matched against the language id of the
notebook cell document. '*' matches every language."))
  (:since "3.17.0")
  (:documentation "A notebook cell text document filter denotes a cell text
document by different properties.

@since 3.17.0"))

(lem-lsp-base/type:define-class file-operation-pattern-options
    common-lisp:nil
  ((ignore-case :type lem-lsp-base/type:lsp-boolean :initarg :ignore-case :accessor
    file-operation-pattern-options-ignore-case :optional common-lisp:t :documentation
    "The pattern should be matched ignoring casing."))
  (:since "3.16.0")
  (:documentation "Matching options for the file operation pattern.

@since 3.16.0"))

(lem-lsp-base/type:define-class execution-summary
    common-lisp:nil
  ((execution-order :type lem-lsp-base/type:lsp-uinteger :initarg :execution-order :accessor
    execution-summary-execution-order :documentation "A strict monotonically increasing value
indicating the execution order of a cell
inside a notebook.")
   (success :type lem-lsp-base/type:lsp-boolean :initarg :success :accessor
    execution-summary-success :optional common-lisp:t :documentation
    "Whether the execution was successful or
not if known by the client.")))

(lem-lsp-base/type:define-class workspace-client-capabilities
    common-lisp:nil
  ((apply-edit :type lem-lsp-base/type:lsp-boolean :initarg :apply-edit :accessor
    workspace-client-capabilities-apply-edit :optional common-lisp:t :documentation
    "The client supports applying batch edits
to the workspace by supporting the request
'workspace/applyEdit'")
   (workspace-edit :type workspace-edit-client-capabilities :initarg :workspace-edit :accessor
    workspace-client-capabilities-workspace-edit :optional common-lisp:t :documentation
    "Capabilities specific to `WorkspaceEdit`s.")
   (did-change-configuration :type did-change-configuration-client-capabilities :initarg
    :did-change-configuration :accessor workspace-client-capabilities-did-change-configuration
    :optional common-lisp:t :documentation
    "Capabilities specific to the `workspace/didChangeConfiguration` notification.")
   (did-change-watched-files :type did-change-watched-files-client-capabilities :initarg
    :did-change-watched-files :accessor workspace-client-capabilities-did-change-watched-files
    :optional common-lisp:t :documentation
    "Capabilities specific to the `workspace/didChangeWatchedFiles` notification.")
   (symbol :type workspace-symbol-client-capabilities :initarg :symbol :accessor
    workspace-client-capabilities-symbol :optional common-lisp:t :documentation
    "Capabilities specific to the `workspace/symbol` request.")
   (execute-command :type execute-command-client-capabilities :initarg :execute-command :accessor
    workspace-client-capabilities-execute-command :optional common-lisp:t :documentation
    "Capabilities specific to the `workspace/executeCommand` request.")
   (workspace-folders :type lem-lsp-base/type:lsp-boolean :initarg :workspace-folders :accessor
    workspace-client-capabilities-workspace-folders :optional common-lisp:t :since "3.6.0"
    :documentation "The client has support for workspace folders.

@since 3.6.0")
   (configuration :type lem-lsp-base/type:lsp-boolean :initarg :configuration :accessor
    workspace-client-capabilities-configuration :optional common-lisp:t :since "3.6.0"
    :documentation "The client supports `workspace/configuration` requests.

@since 3.6.0")
   (semantic-tokens :type semantic-tokens-workspace-client-capabilities :initarg :semantic-tokens
    :accessor workspace-client-capabilities-semantic-tokens :optional common-lisp:t :since
    "3.16.0." :documentation "Capabilities specific to the semantic token requests scoped to the
workspace.

@since 3.16.0.")
   (code-lens :type code-lens-workspace-client-capabilities :initarg :code-lens :accessor
    workspace-client-capabilities-code-lens :optional common-lisp:t :since "3.16.0." :documentation
    "Capabilities specific to the code lens requests scoped to the
workspace.

@since 3.16.0.")
   (file-operations :type file-operation-client-capabilities :initarg :file-operations :accessor
    workspace-client-capabilities-file-operations :optional common-lisp:t :documentation
    "The client has support for file notifications/requests for user operations on files.

Since 3.16.0")
   (inline-value :type inline-value-workspace-client-capabilities :initarg :inline-value :accessor
    workspace-client-capabilities-inline-value :optional common-lisp:t :since "3.17.0."
    :documentation "Capabilities specific to the inline values requests scoped to the
workspace.

@since 3.17.0.")
   (inlay-hint :type inlay-hint-workspace-client-capabilities :initarg :inlay-hint :accessor
    workspace-client-capabilities-inlay-hint :optional common-lisp:t :since "3.17.0."
    :documentation "Capabilities specific to the inlay hint requests scoped to the
workspace.

@since 3.17.0.")
   (diagnostics :type diagnostic-workspace-client-capabilities :initarg :diagnostics :accessor
    workspace-client-capabilities-diagnostics :optional common-lisp:t :since "3.17.0."
    :documentation "Capabilities specific to the diagnostic requests scoped to the
workspace.

@since 3.17.0.")
   (folding-range :type folding-range-workspace-client-capabilities :initarg :folding-range
    :accessor workspace-client-capabilities-folding-range :optional common-lisp:t :proposed
    common-lisp:t :since "3.18.0" :documentation
    "Capabilities specific to the folding range requests scoped to the workspace.

@since 3.18.0
@proposed"))
  (:documentation "Workspace specific client capabilities."))

(lem-lsp-base/type:define-class text-document-client-capabilities
    common-lisp:nil
  ((synchronization :type text-document-sync-client-capabilities :initarg :synchronization
    :accessor text-document-client-capabilities-synchronization :optional common-lisp:t
    :documentation "Defines which synchronization capabilities the client supports.")
   (completion :type completion-client-capabilities :initarg :completion :accessor
    text-document-client-capabilities-completion :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/completion` request.")
   (hover :type hover-client-capabilities :initarg :hover :accessor
    text-document-client-capabilities-hover :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/hover` request.")
   (signature-help :type signature-help-client-capabilities :initarg :signature-help :accessor
    text-document-client-capabilities-signature-help :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/signatureHelp` request.")
   (declaration :type declaration-client-capabilities :initarg :declaration :accessor
    text-document-client-capabilities-declaration :optional common-lisp:t :since "3.14.0"
    :documentation "Capabilities specific to the `textDocument/declaration` request.

@since 3.14.0")
   (definition :type definition-client-capabilities :initarg :definition :accessor
    text-document-client-capabilities-definition :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/definition` request.")
   (type-definition :type type-definition-client-capabilities :initarg :type-definition :accessor
    text-document-client-capabilities-type-definition :optional common-lisp:t :since "3.6.0"
    :documentation "Capabilities specific to the `textDocument/typeDefinition` request.

@since 3.6.0")
   (implementation :type implementation-client-capabilities :initarg :implementation :accessor
    text-document-client-capabilities-implementation :optional common-lisp:t :since "3.6.0"
    :documentation "Capabilities specific to the `textDocument/implementation` request.

@since 3.6.0")
   (references :type reference-client-capabilities :initarg :references :accessor
    text-document-client-capabilities-references :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/references` request.")
   (document-highlight :type document-highlight-client-capabilities :initarg :document-highlight
    :accessor text-document-client-capabilities-document-highlight :optional common-lisp:t
    :documentation "Capabilities specific to the `textDocument/documentHighlight` request.")
   (document-symbol :type document-symbol-client-capabilities :initarg :document-symbol :accessor
    text-document-client-capabilities-document-symbol :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/documentSymbol` request.")
   (code-action :type code-action-client-capabilities :initarg :code-action :accessor
    text-document-client-capabilities-code-action :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/codeAction` request.")
   (code-lens :type code-lens-client-capabilities :initarg :code-lens :accessor
    text-document-client-capabilities-code-lens :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/codeLens` request.")
   (document-link :type document-link-client-capabilities :initarg :document-link :accessor
    text-document-client-capabilities-document-link :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/documentLink` request.")
   (color-provider :type document-color-client-capabilities :initarg :color-provider :accessor
    text-document-client-capabilities-color-provider :optional common-lisp:t :since "3.6.0"
    :documentation "Capabilities specific to the `textDocument/documentColor` and the
`textDocument/colorPresentation` request.

@since 3.6.0")
   (formatting :type document-formatting-client-capabilities :initarg :formatting :accessor
    text-document-client-capabilities-formatting :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/formatting` request.")
   (range-formatting :type document-range-formatting-client-capabilities :initarg :range-formatting
    :accessor text-document-client-capabilities-range-formatting :optional common-lisp:t
    :documentation "Capabilities specific to the `textDocument/rangeFormatting` request.")
   (on-type-formatting :type document-on-type-formatting-client-capabilities :initarg
    :on-type-formatting :accessor text-document-client-capabilities-on-type-formatting :optional
    common-lisp:t :documentation
    "Capabilities specific to the `textDocument/onTypeFormatting` request.")
   (rename :type rename-client-capabilities :initarg :rename :accessor
    text-document-client-capabilities-rename :optional common-lisp:t :documentation
    "Capabilities specific to the `textDocument/rename` request.")
   (folding-range :type folding-range-client-capabilities :initarg :folding-range :accessor
    text-document-client-capabilities-folding-range :optional common-lisp:t :since "3.10.0"
    :documentation "Capabilities specific to the `textDocument/foldingRange` request.

@since 3.10.0")
   (selection-range :type selection-range-client-capabilities :initarg :selection-range :accessor
    text-document-client-capabilities-selection-range :optional common-lisp:t :since "3.15.0"
    :documentation "Capabilities specific to the `textDocument/selectionRange` request.

@since 3.15.0")
   (publish-diagnostics :type publish-diagnostics-client-capabilities :initarg :publish-diagnostics
    :accessor text-document-client-capabilities-publish-diagnostics :optional common-lisp:t
    :documentation "Capabilities specific to the `textDocument/publishDiagnostics` notification.")
   (call-hierarchy :type call-hierarchy-client-capabilities :initarg :call-hierarchy :accessor
    text-document-client-capabilities-call-hierarchy :optional common-lisp:t :since "3.16.0"
    :documentation "Capabilities specific to the various call hierarchy requests.

@since 3.16.0")
   (semantic-tokens :type semantic-tokens-client-capabilities :initarg :semantic-tokens :accessor
    text-document-client-capabilities-semantic-tokens :optional common-lisp:t :since "3.16.0"
    :documentation "Capabilities specific to the various semantic token request.

@since 3.16.0")
   (linked-editing-range :type linked-editing-range-client-capabilities :initarg
    :linked-editing-range :accessor text-document-client-capabilities-linked-editing-range
    :optional common-lisp:t :since "3.16.0" :documentation
    "Capabilities specific to the `textDocument/linkedEditingRange` request.

@since 3.16.0")
   (moniker :type moniker-client-capabilities :initarg :moniker :accessor
    text-document-client-capabilities-moniker :optional common-lisp:t :since "3.16.0"
    :documentation "Client capabilities specific to the `textDocument/moniker` request.

@since 3.16.0")
   (type-hierarchy :type type-hierarchy-client-capabilities :initarg :type-hierarchy :accessor
    text-document-client-capabilities-type-hierarchy :optional common-lisp:t :since "3.17.0"
    :documentation "Capabilities specific to the various type hierarchy requests.

@since 3.17.0")
   (inline-value :type inline-value-client-capabilities :initarg :inline-value :accessor
    text-document-client-capabilities-inline-value :optional common-lisp:t :since "3.17.0"
    :documentation "Capabilities specific to the `textDocument/inlineValue` request.

@since 3.17.0")
   (inlay-hint :type inlay-hint-client-capabilities :initarg :inlay-hint :accessor
    text-document-client-capabilities-inlay-hint :optional common-lisp:t :since "3.17.0"
    :documentation "Capabilities specific to the `textDocument/inlayHint` request.

@since 3.17.0")
   (diagnostic :type diagnostic-client-capabilities :initarg :diagnostic :accessor
    text-document-client-capabilities-diagnostic :optional common-lisp:t :since "3.17.0"
    :documentation "Capabilities specific to the diagnostic pull model.

@since 3.17.0")
   (inline-completion :type inline-completion-client-capabilities :initarg :inline-completion
    :accessor text-document-client-capabilities-inline-completion :optional common-lisp:t :proposed
    common-lisp:t :since "3.18.0" :documentation
    "Client capabilities specific to inline completions.

@since 3.18.0
@proposed"))
  (:documentation "Text document specific client capabilities."))

(lem-lsp-base/type:define-class notebook-document-client-capabilities
    common-lisp:nil
  ((synchronization :type notebook-document-sync-client-capabilities :initarg :synchronization
    :accessor notebook-document-client-capabilities-synchronization :since "3.17.0" :documentation
    "Capabilities specific to notebook document synchronization

@since 3.17.0"))
  (:since "3.17.0")
  (:documentation "Capabilities specific to the notebook document support.

@since 3.17.0"))

(lem-lsp-base/type:define-class window-client-capabilities
    common-lisp:nil
  ((work-done-progress :type lem-lsp-base/type:lsp-boolean :initarg :work-done-progress :accessor
    window-client-capabilities-work-done-progress :optional common-lisp:t :since "3.15.0"
    :documentation "It indicates whether the client supports server initiated
progress using the `window/workDoneProgress/create` request.

The capability also controls Whether client supports handling
of progress notifications. If set servers are allowed to report a
`workDoneProgress` property in the request specific server
capabilities.

@since 3.15.0")
   (show-message :type show-message-request-client-capabilities :initarg :show-message :accessor
    window-client-capabilities-show-message :optional common-lisp:t :since "3.16.0" :documentation
    "Capabilities specific to the showMessage request.

@since 3.16.0")
   (show-document :type show-document-client-capabilities :initarg :show-document :accessor
    window-client-capabilities-show-document :optional common-lisp:t :since "3.16.0" :documentation
    "Capabilities specific to the showDocument request.

@since 3.16.0")))

(lem-lsp-base/type:define-class general-client-capabilities
    common-lisp:nil
  ((stale-request-support :type
    (lem-lsp-base/type:lsp-interface
     ((cancel :type lem-lsp-base/type:lsp-boolean :documentation
       "The client will actively cancel the request.")
      (retry-on-content-modified :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string)
       :documentation "The list of requests for which the client
will retry the request if it receives a
response with error code `ContentModified`")))
    :initarg :stale-request-support :accessor general-client-capabilities-stale-request-support
    :optional common-lisp:t :since "3.17.0" :documentation
    "Client capability that signals how the client
handles stale requests (e.g. a request
for which the client will not process the response
anymore since the information is outdated).

@since 3.17.0")
   (regular-expressions :type regular-expressions-client-capabilities :initarg :regular-expressions
    :accessor general-client-capabilities-regular-expressions :optional common-lisp:t :since
    "3.16.0" :documentation "Client capabilities specific to regular expressions.

@since 3.16.0")
   (markdown :type markdown-client-capabilities :initarg :markdown :accessor
    general-client-capabilities-markdown :optional common-lisp:t :since "3.16.0" :documentation
    "Client capabilities specific to the client's markdown parser.

@since 3.16.0")
   (position-encodings :type (lem-lsp-base/type:lsp-array position-encoding-kind) :initarg
    :position-encodings :accessor general-client-capabilities-position-encodings :optional
    common-lisp:t :since "3.17.0" :documentation
    "The position encodings supported by the client. Client and server
have to agree on the same position encoding to ensure that offsets
(e.g. character position in a line) are interpreted the same on both
sides.

To keep the protocol backwards compatible the following applies: if
the value 'utf-16' is missing from the array of position encodings
servers can assume that the client supports UTF-16. UTF-16 is
therefore a mandatory encoding.

If omitted it defaults to ['utf-16'].

Implementation considerations: since the conversion from one encoding
into another requires the content of the file / line the conversion
is best done where the file is read which is usually on the server
side.

@since 3.17.0"))
  (:since "3.16.0")
  (:documentation "General client capabilities.

@since 3.16.0"))

(lem-lsp-base/type:define-class relative-pattern
    common-lisp:nil
  ((base-uri :type (common-lisp:or workspace-folder lem-lsp-base/type:lsp-uri) :initarg :base-uri
    :accessor relative-pattern-base-uri :documentation
    "A workspace folder or a base URI to which this pattern will be matched
against relatively.")
   (pattern :type pattern :initarg :pattern :accessor relative-pattern-pattern :documentation
    "The actual glob pattern;"))
  (:since "3.17.0")
  (:documentation "A relative pattern is a helper to construct glob patterns that are matched
relatively to a base URI. The common value for a `baseUri` is a workspace
folder root, but it can be another absolute URI as well.

@since 3.17.0"))

(lem-lsp-base/type:define-class workspace-edit-client-capabilities
    common-lisp:nil
  ((document-changes :type lem-lsp-base/type:lsp-boolean :initarg :document-changes :accessor
    workspace-edit-client-capabilities-document-changes :optional common-lisp:t :documentation
    "The client supports versioned document changes in `WorkspaceEdit`s")
   (resource-operations :type (lem-lsp-base/type:lsp-array resource-operation-kind) :initarg
    :resource-operations :accessor workspace-edit-client-capabilities-resource-operations :optional
    common-lisp:t :since "3.13.0" :documentation
    "The resource operations the client supports. Clients should at least
support 'create', 'rename' and 'delete' files and folders.

@since 3.13.0")
   (failure-handling :type failure-handling-kind :initarg :failure-handling :accessor
    workspace-edit-client-capabilities-failure-handling :optional common-lisp:t :since "3.13.0"
    :documentation "The failure handling strategy of a client if applying the workspace edit
fails.

@since 3.13.0")
   (normalizes-line-endings :type lem-lsp-base/type:lsp-boolean :initarg :normalizes-line-endings
    :accessor workspace-edit-client-capabilities-normalizes-line-endings :optional common-lisp:t
    :since "3.16.0" :documentation
    "Whether the client normalizes line endings to the client specific
setting.
If set to `true` the client will normalize line ending characters
in a workspace edit to the client-specified new line
character.

@since 3.16.0")
   (change-annotation-support :type
    (lem-lsp-base/type:lsp-interface
     ((groups-on-label :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :documentation
       "Whether the client groups edits with equal labels into tree nodes,
for instance all edits labelled with \"Changes in Strings\" would
be a tree node.")))
    :initarg :change-annotation-support :accessor
    workspace-edit-client-capabilities-change-annotation-support :optional common-lisp:t :since
    "3.16.0" :documentation
    "Whether the client in general supports change annotations on text edits,
create file, rename file and delete file changes.

@since 3.16.0")))

(lem-lsp-base/type:define-class did-change-configuration-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor did-change-configuration-client-capabilities-dynamic-registration :optional
    common-lisp:t :documentation
    "Did change configuration notification supports dynamic registration.")))

(lem-lsp-base/type:define-class did-change-watched-files-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor did-change-watched-files-client-capabilities-dynamic-registration :optional
    common-lisp:t :documentation
    "Did change watched files notification supports dynamic registration. Please note
that the current protocol doesn't support static configuration for file changes
from the server side.")
   (relative-pattern-support :type lem-lsp-base/type:lsp-boolean :initarg :relative-pattern-support
    :accessor did-change-watched-files-client-capabilities-relative-pattern-support :optional
    common-lisp:t :since "3.17.0" :documentation
    "Whether the client has support for {@link  RelativePattern relative pattern}
or not.

@since 3.17.0")))

(lem-lsp-base/type:define-class workspace-symbol-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor workspace-symbol-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Symbol request supports dynamic registration.")
   (symbol-kind :type
    (lem-lsp-base/type:lsp-interface
     ((value-set :type (lem-lsp-base/type:lsp-array symbol-kind) :optional common-lisp:t
       :documentation "The symbol kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.

If this property is not present the client only supports
the symbol kinds from `File` to `Array` as defined in
the initial version of the protocol.")))
    :initarg :symbol-kind :accessor workspace-symbol-client-capabilities-symbol-kind :optional
    common-lisp:t :documentation
    "Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.")
   (tag-support :type
    (lem-lsp-base/type:lsp-interface
     ((value-set :type (lem-lsp-base/type:lsp-array symbol-tag) :documentation
       "The tags supported by the client.")))
    :initarg :tag-support :accessor workspace-symbol-client-capabilities-tag-support :optional
    common-lisp:t :since "3.16.0" :documentation "The client supports tags on `SymbolInformation`.
Clients supporting tags have to handle unknown tags gracefully.

@since 3.16.0")
   (resolve-support :type
    (lem-lsp-base/type:lsp-interface
     ((properties :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :documentation
       "The properties that a client can resolve lazily. Usually
`location.range`")))
    :initarg :resolve-support :accessor workspace-symbol-client-capabilities-resolve-support
    :optional common-lisp:t :since "3.17.0" :documentation
    "The client support partial workspace symbols. The client will send the
request `workspaceSymbol/resolve` to the server to resolve additional
properties.

@since 3.17.0"))
  (:documentation "Client capabilities for a {@link WorkspaceSymbolRequest}."))

(lem-lsp-base/type:define-class execute-command-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor execute-command-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Execute command supports dynamic registration."))
  (:documentation "The client capabilities of a {@link ExecuteCommandRequest}."))

(lem-lsp-base/type:define-class semantic-tokens-workspace-client-capabilities
    common-lisp:nil
  ((refresh-support :type lem-lsp-base/type:lsp-boolean :initarg :refresh-support :accessor
    semantic-tokens-workspace-client-capabilities-refresh-support :optional common-lisp:t
    :documentation "Whether the client implementation supports a refresh request sent from
the server to the client.

Note that this event is global and will force the client to refresh all
semantic tokens currently shown. It should be used with absolute care
and is useful for situation where a server for example detects a project
wide change that requires such a calculation."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class code-lens-workspace-client-capabilities
    common-lisp:nil
  ((refresh-support :type lem-lsp-base/type:lsp-boolean :initarg :refresh-support :accessor
    code-lens-workspace-client-capabilities-refresh-support :optional common-lisp:t :documentation
    "Whether the client implementation supports a refresh request sent from the
server to the client.

Note that this event is global and will force the client to refresh all
code lenses currently shown. It should be used with absolute care and is
useful for situation where a server for example detect a project wide
change that requires such a calculation."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class file-operation-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor file-operation-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation
    "Whether the client supports dynamic registration for file requests/notifications.")
   (did-create :type lem-lsp-base/type:lsp-boolean :initarg :did-create :accessor
    file-operation-client-capabilities-did-create :optional common-lisp:t :documentation
    "The client has support for sending didCreateFiles notifications.")
   (will-create :type lem-lsp-base/type:lsp-boolean :initarg :will-create :accessor
    file-operation-client-capabilities-will-create :optional common-lisp:t :documentation
    "The client has support for sending willCreateFiles requests.")
   (did-rename :type lem-lsp-base/type:lsp-boolean :initarg :did-rename :accessor
    file-operation-client-capabilities-did-rename :optional common-lisp:t :documentation
    "The client has support for sending didRenameFiles notifications.")
   (will-rename :type lem-lsp-base/type:lsp-boolean :initarg :will-rename :accessor
    file-operation-client-capabilities-will-rename :optional common-lisp:t :documentation
    "The client has support for sending willRenameFiles requests.")
   (did-delete :type lem-lsp-base/type:lsp-boolean :initarg :did-delete :accessor
    file-operation-client-capabilities-did-delete :optional common-lisp:t :documentation
    "The client has support for sending didDeleteFiles notifications.")
   (will-delete :type lem-lsp-base/type:lsp-boolean :initarg :will-delete :accessor
    file-operation-client-capabilities-will-delete :optional common-lisp:t :documentation
    "The client has support for sending willDeleteFiles requests."))
  (:since "3.16.0")
  (:documentation "Capabilities relating to events from file operations by the user in the client.

These events do not come from the file system, they come from user operations
like renaming a file in the UI.

@since 3.16.0"))

(lem-lsp-base/type:define-class inline-value-workspace-client-capabilities
    common-lisp:nil
  ((refresh-support :type lem-lsp-base/type:lsp-boolean :initarg :refresh-support :accessor
    inline-value-workspace-client-capabilities-refresh-support :optional common-lisp:t
    :documentation "Whether the client implementation supports a refresh request sent from the
server to the client.

Note that this event is global and will force the client to refresh all
inline values currently shown. It should be used with absolute care and is
useful for situation where a server for example detects a project wide
change that requires such a calculation."))
  (:since "3.17.0")
  (:documentation "Client workspace capabilities specific to inline values.

@since 3.17.0"))

(lem-lsp-base/type:define-class inlay-hint-workspace-client-capabilities
    common-lisp:nil
  ((refresh-support :type lem-lsp-base/type:lsp-boolean :initarg :refresh-support :accessor
    inlay-hint-workspace-client-capabilities-refresh-support :optional common-lisp:t :documentation
    "Whether the client implementation supports a refresh request sent from
the server to the client.

Note that this event is global and will force the client to refresh all
inlay hints currently shown. It should be used with absolute care and
is useful for situation where a server for example detects a project wide
change that requires such a calculation."))
  (:since "3.17.0")
  (:documentation "Client workspace capabilities specific to inlay hints.

@since 3.17.0"))

(lem-lsp-base/type:define-class diagnostic-workspace-client-capabilities
    common-lisp:nil
  ((refresh-support :type lem-lsp-base/type:lsp-boolean :initarg :refresh-support :accessor
    diagnostic-workspace-client-capabilities-refresh-support :optional common-lisp:t :documentation
    "Whether the client implementation supports a refresh request sent from
the server to the client.

Note that this event is global and will force the client to refresh all
pulled diagnostics currently shown. It should be used with absolute care and
is useful for situation where a server for example detects a project wide
change that requires such a calculation."))
  (:since "3.17.0")
  (:documentation "Workspace client capabilities specific to diagnostic pull requests.

@since 3.17.0"))

(lem-lsp-base/type:define-class folding-range-workspace-client-capabilities
    common-lisp:nil
  ((refresh-support :type lem-lsp-base/type:lsp-boolean :initarg :refresh-support :accessor
    folding-range-workspace-client-capabilities-refresh-support :optional common-lisp:t :proposed
    common-lisp:t :since "3.18.0" :documentation
    "Whether the client implementation supports a refresh request sent from the
server to the client.

Note that this event is global and will force the client to refresh all
folding ranges currently shown. It should be used with absolute care and is
useful for situation where a server for example detects a project wide
change that requires such a calculation.

@since 3.18.0
@proposed"))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "Client workspace capabilities specific to folding ranges

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class text-document-sync-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor text-document-sync-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether text document synchronization supports dynamic registration.")
   (will-save :type lem-lsp-base/type:lsp-boolean :initarg :will-save :accessor
    text-document-sync-client-capabilities-will-save :optional common-lisp:t :documentation
    "The client supports sending will save notifications.")
   (will-save-wait-until :type lem-lsp-base/type:lsp-boolean :initarg :will-save-wait-until
    :accessor text-document-sync-client-capabilities-will-save-wait-until :optional common-lisp:t
    :documentation "The client supports sending a will save request and
waits for a response providing text edits which will
be applied to the document before it is saved.")
   (did-save :type lem-lsp-base/type:lsp-boolean :initarg :did-save :accessor
    text-document-sync-client-capabilities-did-save :optional common-lisp:t :documentation
    "The client supports did save notifications.")))

(lem-lsp-base/type:define-class completion-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor completion-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether completion supports dynamic registration.")
   (completion-item :type
    (lem-lsp-base/type:lsp-interface
     ((snippet-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :documentation
       "Client supports snippets as insert text.

A snippet can define tab stops and placeholders with `$1`, `$2`
and `${3:foo}`. `$0` defines the final tab stop, it defaults to
the end of the snippet. Placeholders with equal identifiers are linked,
that is typing in one will update others too.")
      (commit-characters-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t
       :documentation "Client supports commit characters on a completion item.")
      (documentation-format :type (lem-lsp-base/type:lsp-array markup-kind) :optional common-lisp:t
       :documentation "Client supports the following content formats for the documentation
property. The order describes the preferred format of the client.")
      (deprecated-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t
       :documentation "Client supports the deprecated property on a completion item.")
      (preselect-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :documentation
       "Client supports the preselect property on a completion item.")
      (tag-support :type
       (lem-lsp-base/type:lsp-interface
        ((value-set :type (lem-lsp-base/type:lsp-array completion-item-tag) :documentation
          "The tags supported by the client.")))
       :optional common-lisp:t :since "3.15.0" :documentation
       "Client supports the tag property on a completion item. Clients supporting
tags have to handle unknown tags gracefully. Clients especially need to
preserve unknown tags when sending a completion item back to the server in
a resolve call.

@since 3.15.0")
      (insert-replace-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :since
       "3.16.0" :documentation
       "Client support insert replace edit to control different behavior if a
completion item is inserted in the text or should replace text.

@since 3.16.0")
      (resolve-support :type
       (lem-lsp-base/type:lsp-interface
        ((properties :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string)
          :documentation "The properties that a client can resolve lazily.")))
       :optional common-lisp:t :since "3.16.0" :documentation
       "Indicates which properties a client can resolve lazily on a completion
item. Before version 3.16.0 only the predefined properties `documentation`
and `details` could be resolved lazily.

@since 3.16.0")
      (insert-text-mode-support :type
       (lem-lsp-base/type:lsp-interface
        ((value-set :type (lem-lsp-base/type:lsp-array insert-text-mode))))
       :optional common-lisp:t :since "3.16.0" :documentation
       "The client supports the `insertTextMode` property on
a completion item to override the whitespace handling mode
as defined by the client (see `insertTextMode`).

@since 3.16.0")
      (label-details-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :since
       "3.17.0" :documentation "The client has support for completion item label
details (see also `CompletionItemLabelDetails`).

@since 3.17.0")))
    :initarg :completion-item :accessor completion-client-capabilities-completion-item :optional
    common-lisp:t :documentation "The client supports the following `CompletionItem` specific
capabilities.")
   (completion-item-kind :type
                         (lem-lsp-base/type:lsp-interface
                          ((value-set :type (lem-lsp-base/type:lsp-array completion-item-kind)
                            :optional common-lisp:t :documentation
                            "The completion item kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.

If this property is not present the client only supports
the completion items kinds from `Text` to `Reference` as defined in
the initial version of the protocol.")))
                         :initarg :completion-item-kind :accessor
                         completion-client-capabilities-completion-item-kind :optional
                         common-lisp:t)
   (insert-text-mode :type insert-text-mode :initarg :insert-text-mode :accessor
    completion-client-capabilities-insert-text-mode :optional common-lisp:t :since "3.17.0"
    :documentation "Defines how the client handles whitespace and indentation
when accepting a completion item that uses multi line
text in either `insertText` or `textEdit`.

@since 3.17.0")
   (context-support :type lem-lsp-base/type:lsp-boolean :initarg :context-support :accessor
    completion-client-capabilities-context-support :optional common-lisp:t :documentation
    "The client supports to send additional context information for a
`textDocument/completion` request.")
   (completion-list :type
    (lem-lsp-base/type:lsp-interface
     ((item-defaults :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :optional
       common-lisp:t :since "3.17.0" :documentation
       "The client supports the following itemDefaults on
a completion list.

The value lists the supported property names of the
`CompletionList.itemDefaults` object. If omitted
no properties are supported.

@since 3.17.0")))
    :initarg :completion-list :accessor completion-client-capabilities-completion-list :optional
    common-lisp:t :since "3.17.0" :documentation
    "The client supports the following `CompletionList` specific
capabilities.

@since 3.17.0"))
  (:documentation "Completion client capabilities"))

(lem-lsp-base/type:define-class hover-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor hover-client-capabilities-dynamic-registration :optional common-lisp:t :documentation
    "Whether hover supports dynamic registration.")
   (content-format :type (lem-lsp-base/type:lsp-array markup-kind) :initarg :content-format
    :accessor hover-client-capabilities-content-format :optional common-lisp:t :documentation
    "Client supports the following content formats for the content
property. The order describes the preferred format of the client.")))

(lem-lsp-base/type:define-class signature-help-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor signature-help-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether signature help supports dynamic registration.")
   (signature-information :type
    (lem-lsp-base/type:lsp-interface
     ((documentation-format :type (lem-lsp-base/type:lsp-array markup-kind) :optional common-lisp:t
       :documentation "Client supports the following content formats for the documentation
property. The order describes the preferred format of the client.")
      (parameter-information :type
       (lem-lsp-base/type:lsp-interface
        ((label-offset-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :since
          "3.14.0" :documentation "The client supports processing label offsets instead of a
simple label string.

@since 3.14.0")))
       :optional common-lisp:t :documentation
       "Client capabilities specific to parameter information.")
      (active-parameter-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :since
       "3.16.0" :documentation
       "The client supports the `activeParameter` property on `SignatureInformation`
literal.

@since 3.16.0")))
    :initarg :signature-information :accessor
    signature-help-client-capabilities-signature-information :optional common-lisp:t :documentation
    "The client supports the following `SignatureInformation`
specific properties.")
   (context-support :type lem-lsp-base/type:lsp-boolean :initarg :context-support :accessor
    signature-help-client-capabilities-context-support :optional common-lisp:t :since "3.15.0"
    :documentation "The client supports to send additional context information for a
`textDocument/signatureHelp` request. A client that opts into
contextSupport will also support the `retriggerCharacters` on
`SignatureHelpOptions`.

@since 3.15.0"))
  (:documentation "Client Capabilities for a {@link SignatureHelpRequest}."))

(lem-lsp-base/type:define-class declaration-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor declaration-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether declaration supports dynamic registration. If this is set to `true`
the client supports the new `DeclarationRegistrationOptions` return value
for the corresponding server capability as well.")
   (link-support :type lem-lsp-base/type:lsp-boolean :initarg :link-support :accessor
    declaration-client-capabilities-link-support :optional common-lisp:t :documentation
    "The client supports additional metadata in the form of declaration links."))
  (:since "3.14.0")
  (:documentation "@since 3.14.0"))

(lem-lsp-base/type:define-class definition-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor definition-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether definition supports dynamic registration.")
   (link-support :type lem-lsp-base/type:lsp-boolean :initarg :link-support :accessor
    definition-client-capabilities-link-support :optional common-lisp:t :since "3.14.0"
    :documentation "The client supports additional metadata in the form of definition links.

@since 3.14.0"))
  (:documentation "Client Capabilities for a {@link DefinitionRequest}."))

(lem-lsp-base/type:define-class type-definition-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor type-definition-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `TypeDefinitionRegistrationOptions` return value
for the corresponding server capability as well.")
   (link-support :type lem-lsp-base/type:lsp-boolean :initarg :link-support :accessor
    type-definition-client-capabilities-link-support :optional common-lisp:t :documentation
    "The client supports additional metadata in the form of definition links.

Since 3.14.0"))
  (:documentation "Since 3.6.0"))

(lem-lsp-base/type:define-class implementation-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor implementation-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `ImplementationRegistrationOptions` return value
for the corresponding server capability as well.")
   (link-support :type lem-lsp-base/type:lsp-boolean :initarg :link-support :accessor
    implementation-client-capabilities-link-support :optional common-lisp:t :since "3.14.0"
    :documentation "The client supports additional metadata in the form of definition links.

@since 3.14.0"))
  (:since "3.6.0")
  (:documentation "@since 3.6.0"))

(lem-lsp-base/type:define-class reference-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor reference-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether references supports dynamic registration."))
  (:documentation "Client Capabilities for a {@link ReferencesRequest}."))

(lem-lsp-base/type:define-class document-highlight-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-highlight-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether document highlight supports dynamic registration."))
  (:documentation "Client Capabilities for a {@link DocumentHighlightRequest}."))

(lem-lsp-base/type:define-class document-symbol-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-symbol-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether document symbol supports dynamic registration.")
   (symbol-kind :type
    (lem-lsp-base/type:lsp-interface
     ((value-set :type (lem-lsp-base/type:lsp-array symbol-kind) :optional common-lisp:t
       :documentation "The symbol kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.

If this property is not present the client only supports
the symbol kinds from `File` to `Array` as defined in
the initial version of the protocol.")))
    :initarg :symbol-kind :accessor document-symbol-client-capabilities-symbol-kind :optional
    common-lisp:t :documentation "Specific capabilities for the `SymbolKind` in the
`textDocument/documentSymbol` request.")
   (hierarchical-document-symbol-support :type lem-lsp-base/type:lsp-boolean :initarg
    :hierarchical-document-symbol-support :accessor
    document-symbol-client-capabilities-hierarchical-document-symbol-support :optional
    common-lisp:t :documentation "The client supports hierarchical document symbols.")
   (tag-support :type
    (lem-lsp-base/type:lsp-interface
     ((value-set :type (lem-lsp-base/type:lsp-array symbol-tag) :documentation
       "The tags supported by the client.")))
    :initarg :tag-support :accessor document-symbol-client-capabilities-tag-support :optional
    common-lisp:t :since "3.16.0" :documentation
    "The client supports tags on `SymbolInformation`. Tags are supported on
`DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
Clients supporting tags have to handle unknown tags gracefully.

@since 3.16.0")
   (label-support :type lem-lsp-base/type:lsp-boolean :initarg :label-support :accessor
    document-symbol-client-capabilities-label-support :optional common-lisp:t :since "3.16.0"
    :documentation "The client supports an additional label presented in the UI when
registering a document symbol provider.

@since 3.16.0"))
  (:documentation "Client Capabilities for a {@link DocumentSymbolRequest}."))

(lem-lsp-base/type:define-class code-action-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor code-action-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether code action supports dynamic registration.")
   (code-action-literal-support :type
    (lem-lsp-base/type:lsp-interface
     ((code-action-kind :type
                        (lem-lsp-base/type:lsp-interface
                         ((value-set :type (lem-lsp-base/type:lsp-array code-action-kind)
                           :documentation
                           "The code action kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.")))
                        :documentation "The code action kind is support with the following value
set.")))
    :initarg :code-action-literal-support :accessor
    code-action-client-capabilities-code-action-literal-support :optional common-lisp:t :since
    "3.8.0" :documentation "The client support code action literals of type `CodeAction` as a valid
response of the `textDocument/codeAction` request. If the property is not
set the request can only return `Command` literals.

@since 3.8.0")
   (is-preferred-support :type lem-lsp-base/type:lsp-boolean :initarg :is-preferred-support
    :accessor code-action-client-capabilities-is-preferred-support :optional common-lisp:t :since
    "3.15.0" :documentation "Whether code action supports the `isPreferred` property.

@since 3.15.0")
   (disabled-support :type lem-lsp-base/type:lsp-boolean :initarg :disabled-support :accessor
    code-action-client-capabilities-disabled-support :optional common-lisp:t :since "3.16.0"
    :documentation "Whether code action supports the `disabled` property.

@since 3.16.0")
   (data-support :type lem-lsp-base/type:lsp-boolean :initarg :data-support :accessor
    code-action-client-capabilities-data-support :optional common-lisp:t :since "3.16.0"
    :documentation "Whether code action supports the `data` property which is
preserved between a `textDocument/codeAction` and a
`codeAction/resolve` request.

@since 3.16.0")
   (resolve-support :type
    (lem-lsp-base/type:lsp-interface
     ((properties :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :documentation
       "The properties that a client can resolve lazily.")))
    :initarg :resolve-support :accessor code-action-client-capabilities-resolve-support :optional
    common-lisp:t :since "3.16.0" :documentation
    "Whether the client supports resolving additional code action
properties via a separate `codeAction/resolve` request.

@since 3.16.0")
   (honors-change-annotations :type lem-lsp-base/type:lsp-boolean :initarg
    :honors-change-annotations :accessor code-action-client-capabilities-honors-change-annotations
    :optional common-lisp:t :since "3.16.0" :documentation
    "Whether the client honors the change annotations in
text edits and resource operations returned via the
`CodeAction#edit` property by for example presenting
the workspace edit in the user interface and asking
for confirmation.

@since 3.16.0"))
  (:documentation "The Client Capabilities of a {@link CodeActionRequest}."))

(lem-lsp-base/type:define-class code-lens-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor code-lens-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether code lens supports dynamic registration."))
  (:documentation "The client capabilities  of a {@link CodeLensRequest}."))

(lem-lsp-base/type:define-class document-link-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-link-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether document link supports dynamic registration.")
   (tooltip-support :type lem-lsp-base/type:lsp-boolean :initarg :tooltip-support :accessor
    document-link-client-capabilities-tooltip-support :optional common-lisp:t :since "3.15.0"
    :documentation "Whether the client supports the `tooltip` property on `DocumentLink`.

@since 3.15.0"))
  (:documentation "The client capabilities of a {@link DocumentLinkRequest}."))

(lem-lsp-base/type:define-class document-color-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-color-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `DocumentColorRegistrationOptions` return value
for the corresponding server capability as well.")))

(lem-lsp-base/type:define-class document-formatting-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-formatting-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether formatting supports dynamic registration."))
  (:documentation "Client capabilities of a {@link DocumentFormattingRequest}."))

(lem-lsp-base/type:define-class document-range-formatting-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-range-formatting-client-capabilities-dynamic-registration :optional
    common-lisp:t :documentation "Whether range formatting supports dynamic registration.")
   (ranges-support :type lem-lsp-base/type:lsp-boolean :initarg :ranges-support :accessor
    document-range-formatting-client-capabilities-ranges-support :optional common-lisp:t :proposed
    common-lisp:t :since "3.18.0" :documentation
    "Whether the client supports formatting multiple ranges at once.

@since 3.18.0
@proposed"))
  (:documentation "Client capabilities of a {@link DocumentRangeFormattingRequest}."))

(lem-lsp-base/type:define-class document-on-type-formatting-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor document-on-type-formatting-client-capabilities-dynamic-registration :optional
    common-lisp:t :documentation "Whether on type formatting supports dynamic registration."))
  (:documentation "Client capabilities of a {@link DocumentOnTypeFormattingRequest}."))

(lem-lsp-base/type:define-class rename-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor rename-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether rename supports dynamic registration.")
   (prepare-support :type lem-lsp-base/type:lsp-boolean :initarg :prepare-support :accessor
    rename-client-capabilities-prepare-support :optional common-lisp:t :since "3.12.0"
    :documentation "Client supports testing for validity of rename operations
before execution.

@since 3.12.0")
   (prepare-support-default-behavior :type prepare-support-default-behavior :initarg
    :prepare-support-default-behavior :accessor
    rename-client-capabilities-prepare-support-default-behavior :optional common-lisp:t :since
    "3.16.0" :documentation "Client supports the default behavior result.

The value indicates the default behavior used by the
client.

@since 3.16.0")
   (honors-change-annotations :type lem-lsp-base/type:lsp-boolean :initarg
    :honors-change-annotations :accessor rename-client-capabilities-honors-change-annotations
    :optional common-lisp:t :since "3.16.0" :documentation
    "Whether the client honors the change annotations in
text edits and resource operations returned via the
rename request's workspace edit by for example presenting
the workspace edit in the user interface and asking
for confirmation.

@since 3.16.0")))

(lem-lsp-base/type:define-class folding-range-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor folding-range-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration for folding range
providers. If this is set to `true` the client supports the new
`FoldingRangeRegistrationOptions` return value for the corresponding
server capability as well.")
   (range-limit :type lem-lsp-base/type:lsp-uinteger :initarg :range-limit :accessor
    folding-range-client-capabilities-range-limit :optional common-lisp:t :documentation
    "The maximum number of folding ranges that the client prefers to receive
per document. The value serves as a hint, servers are free to follow the
limit.")
   (line-folding-only :type lem-lsp-base/type:lsp-boolean :initarg :line-folding-only :accessor
    folding-range-client-capabilities-line-folding-only :optional common-lisp:t :documentation
    "If set, the client signals that it only supports folding complete lines.
If set, client will ignore specified `startCharacter` and `endCharacter`
properties in a FoldingRange.")
   (folding-range-kind :type
                       (lem-lsp-base/type:lsp-interface
                        ((value-set :type (lem-lsp-base/type:lsp-array folding-range-kind)
                          :optional common-lisp:t :documentation
                          "The folding range kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.")))
                       :initarg :folding-range-kind :accessor
                       folding-range-client-capabilities-folding-range-kind :optional common-lisp:t
                       :since "3.17.0" :documentation "Specific options for the folding range kind.

@since 3.17.0")
   (folding-range :type
    (lem-lsp-base/type:lsp-interface
     ((collapsed-text :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t :since "3.17.0"
       :documentation "If set, the client signals that it supports setting collapsedText on
folding ranges to display custom labels instead of the default text.

@since 3.17.0")))
    :initarg :folding-range :accessor folding-range-client-capabilities-folding-range :optional
    common-lisp:t :since "3.17.0" :documentation "Specific options for the folding range.

@since 3.17.0")))

(lem-lsp-base/type:define-class selection-range-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor selection-range-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation
    "Whether implementation supports dynamic registration for selection range providers. If this is set to `true`
the client supports the new `SelectionRangeRegistrationOptions` return value for the corresponding server
capability as well.")))

(lem-lsp-base/type:define-class publish-diagnostics-client-capabilities
    common-lisp:nil
  ((related-information :type lem-lsp-base/type:lsp-boolean :initarg :related-information :accessor
    publish-diagnostics-client-capabilities-related-information :optional common-lisp:t
    :documentation "Whether the clients accepts diagnostics with related information.")
   (tag-support :type
    (lem-lsp-base/type:lsp-interface
     ((value-set :type (lem-lsp-base/type:lsp-array diagnostic-tag) :documentation
       "The tags supported by the client.")))
    :initarg :tag-support :accessor publish-diagnostics-client-capabilities-tag-support :optional
    common-lisp:t :since "3.15.0" :documentation
    "Client supports the tag property to provide meta data about a diagnostic.
Clients supporting tags have to handle unknown tags gracefully.

@since 3.15.0")
   (version-support :type lem-lsp-base/type:lsp-boolean :initarg :version-support :accessor
    publish-diagnostics-client-capabilities-version-support :optional common-lisp:t :since "3.15.0"
    :documentation "Whether the client interprets the version property of the
`textDocument/publishDiagnostics` notification's parameter.

@since 3.15.0")
   (code-description-support :type lem-lsp-base/type:lsp-boolean :initarg :code-description-support
    :accessor publish-diagnostics-client-capabilities-code-description-support :optional
    common-lisp:t :since "3.16.0" :documentation "Client supports a codeDescription property

@since 3.16.0")
   (data-support :type lem-lsp-base/type:lsp-boolean :initarg :data-support :accessor
    publish-diagnostics-client-capabilities-data-support :optional common-lisp:t :since "3.16.0"
    :documentation "Whether code action supports the `data` property which is
preserved between a `textDocument/publishDiagnostics` and
`textDocument/codeAction` request.

@since 3.16.0"))
  (:documentation "The publish diagnostic client capabilities."))

(lem-lsp-base/type:define-class call-hierarchy-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor call-hierarchy-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class semantic-tokens-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor semantic-tokens-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well.")
   (requests :type
    (lem-lsp-base/type:lsp-interface
     ((range :type
       (common-lisp:or lem-lsp-base/type:lsp-boolean
                       (lem-lsp-base/type:lsp-interface common-lisp:nil))
       :optional common-lisp:t :documentation
       "The client will send the `textDocument/semanticTokens/range` request if
the server provides a corresponding handler.")
      (full :type
       (common-lisp:or lem-lsp-base/type:lsp-boolean
                       (lem-lsp-base/type:lsp-interface
                        ((delta :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t
                          :documentation
                          "The client will send the `textDocument/semanticTokens/full/delta` request if
the server provides a corresponding handler."))))
       :optional common-lisp:t :documentation
       "The client will send the `textDocument/semanticTokens/full` request if
the server provides a corresponding handler.")))
    :initarg :requests :accessor semantic-tokens-client-capabilities-requests :documentation
    "Which requests the client supports and might send to the server
depending on the server's capability. Please note that clients might not
show semantic tokens or degrade some of the user experience if a range
or full request is advertised by the client but not provided by the
server. If for example the client capability `requests.full` and
`request.range` are both set to true but the server only provides a
range provider the client might not render a minimap correctly or might
even decide to not show any semantic tokens at all.")
   (token-types :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :token-types :accessor semantic-tokens-client-capabilities-token-types :documentation
    "The token types that the client supports.")
   (token-modifiers :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :token-modifiers :accessor semantic-tokens-client-capabilities-token-modifiers :documentation
    "The token modifiers that the client supports.")
   (formats :type (lem-lsp-base/type:lsp-array token-format) :initarg :formats :accessor
    semantic-tokens-client-capabilities-formats :documentation
    "The token formats the clients supports.")
   (overlapping-token-support :type lem-lsp-base/type:lsp-boolean :initarg
    :overlapping-token-support :accessor
    semantic-tokens-client-capabilities-overlapping-token-support :optional common-lisp:t
    :documentation "Whether the client supports tokens that can overlap each other.")
   (multiline-token-support :type lem-lsp-base/type:lsp-boolean :initarg :multiline-token-support
    :accessor semantic-tokens-client-capabilities-multiline-token-support :optional common-lisp:t
    :documentation "Whether the client supports tokens that can span multiple lines.")
   (server-cancel-support :type lem-lsp-base/type:lsp-boolean :initarg :server-cancel-support
    :accessor semantic-tokens-client-capabilities-server-cancel-support :optional common-lisp:t
    :since "3.17.0" :documentation "Whether the client allows the server to actively cancel a
semantic token request, e.g. supports returning
LSPErrorCodes.ServerCancelled. If a server does the client
needs to retrigger the request.

@since 3.17.0")
   (augments-syntax-tokens :type lem-lsp-base/type:lsp-boolean :initarg :augments-syntax-tokens
    :accessor semantic-tokens-client-capabilities-augments-syntax-tokens :optional common-lisp:t
    :since "3.17.0" :documentation "Whether the client uses semantic tokens to augment existing
syntax tokens. If set to `true` client side created syntax
tokens and semantic tokens are both used for colorization. If
set to `false` the client only uses the returned semantic tokens
for colorization.

If the value is `undefined` then the client behavior is not
specified.

@since 3.17.0"))
  (:since "3.16.0")
  (:documentation "@since 3.16.0"))

(lem-lsp-base/type:define-class linked-editing-range-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor linked-editing-range-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well."))
  (:since "3.16.0")
  (:documentation "Client capabilities for the linked editing range request.

@since 3.16.0"))

(lem-lsp-base/type:define-class moniker-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor moniker-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether moniker supports dynamic registration. If this is set to `true`
the client supports the new `MonikerRegistrationOptions` return value
for the corresponding server capability as well."))
  (:since "3.16.0")
  (:documentation "Client capabilities specific to the moniker request.

@since 3.16.0"))

(lem-lsp-base/type:define-class type-hierarchy-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor type-hierarchy-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well."))
  (:since "3.17.0")
  (:documentation "@since 3.17.0"))

(lem-lsp-base/type:define-class inline-value-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor inline-value-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation
    "Whether implementation supports dynamic registration for inline value providers."))
  (:since "3.17.0")
  (:documentation "Client capabilities specific to inline values.

@since 3.17.0"))

(lem-lsp-base/type:define-class inlay-hint-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor inlay-hint-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether inlay hints support dynamic registration.")
   (resolve-support :type
    (lem-lsp-base/type:lsp-interface
     ((properties :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :documentation
       "The properties that a client can resolve lazily.")))
    :initarg :resolve-support :accessor inlay-hint-client-capabilities-resolve-support :optional
    common-lisp:t :documentation "Indicates which properties a client can resolve lazily on an inlay
hint."))
  (:since "3.17.0")
  (:documentation "Inlay hint client capabilities.

@since 3.17.0"))

(lem-lsp-base/type:define-class diagnostic-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor diagnostic-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well.")
   (related-document-support :type lem-lsp-base/type:lsp-boolean :initarg :related-document-support
    :accessor diagnostic-client-capabilities-related-document-support :optional common-lisp:t
    :documentation "Whether the clients supports related documents for document diagnostic pulls."))
  (:since "3.17.0")
  (:documentation "Client capabilities specific to diagnostic pull requests.

@since 3.17.0"))

(lem-lsp-base/type:define-class inline-completion-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor inline-completion-client-capabilities-dynamic-registration :optional common-lisp:t
    :documentation
    "Whether implementation supports dynamic registration for inline completion providers."))
  (:proposed common-lisp:t)
  (:since "3.18.0")
  (:documentation "Client capabilities specific to inline completions.

@since 3.18.0
@proposed"))

(lem-lsp-base/type:define-class notebook-document-sync-client-capabilities
    common-lisp:nil
  ((dynamic-registration :type lem-lsp-base/type:lsp-boolean :initarg :dynamic-registration
    :accessor notebook-document-sync-client-capabilities-dynamic-registration :optional
    common-lisp:t :documentation "Whether implementation supports dynamic registration. If this is
set to `true` the client supports the new
`(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well.")
   (execution-summary-support :type lem-lsp-base/type:lsp-boolean :initarg
    :execution-summary-support :accessor
    notebook-document-sync-client-capabilities-execution-summary-support :optional common-lisp:t
    :documentation "The client supports sending execution summary data per cell."))
  (:since "3.17.0")
  (:documentation "Notebook specific client capabilities.

@since 3.17.0"))

(lem-lsp-base/type:define-class show-message-request-client-capabilities
    common-lisp:nil
  ((message-action-item :type
    (lem-lsp-base/type:lsp-interface
     ((additional-properties-support :type lem-lsp-base/type:lsp-boolean :optional common-lisp:t
       :documentation "Whether the client supports additional attributes which
are preserved and send back to the server in the
request's response.")))
    :initarg :message-action-item :accessor
    show-message-request-client-capabilities-message-action-item :optional common-lisp:t
    :documentation "Capabilities specific to the `MessageActionItem` type."))
  (:documentation "Show message request client capabilities"))

(lem-lsp-base/type:define-class show-document-client-capabilities
    common-lisp:nil
  ((support :type lem-lsp-base/type:lsp-boolean :initarg :support :accessor
    show-document-client-capabilities-support :documentation
    "The client has support for the showDocument
request."))
  (:since "3.16.0")
  (:documentation "Client capabilities for the showDocument request.

@since 3.16.0"))

(lem-lsp-base/type:define-class regular-expressions-client-capabilities
    common-lisp:nil
  ((engine :type lem-lsp-base/type:lsp-string :initarg :engine :accessor
    regular-expressions-client-capabilities-engine :documentation "The engine's name.")
   (version :type lem-lsp-base/type:lsp-string :initarg :version :accessor
    regular-expressions-client-capabilities-version :optional common-lisp:t :documentation
    "The engine's version."))
  (:since "3.16.0")
  (:documentation "Client capabilities specific to regular expressions.

@since 3.16.0"))

(lem-lsp-base/type:define-class markdown-client-capabilities
    common-lisp:nil
  ((parser :type lem-lsp-base/type:lsp-string :initarg :parser :accessor
    markdown-client-capabilities-parser :documentation "The name of the parser.")
   (version :type lem-lsp-base/type:lsp-string :initarg :version :accessor
    markdown-client-capabilities-version :optional common-lisp:t :documentation
    "The version of the parser.")
   (allowed-tags :type (lem-lsp-base/type:lsp-array lem-lsp-base/type:lsp-string) :initarg
    :allowed-tags :accessor markdown-client-capabilities-allowed-tags :optional common-lisp:t
    :since "3.17.0" :documentation "A list of HTML tags that the client allows / supports in
Markdown.

@since 3.17.0"))
  (:since "3.16.0")
  (:documentation "Client capabilities specific to the used markdown parser.

@since 3.16.0"))

(lem-lsp-base/type:define-type-alias definition
    (common-lisp:or location (lem-lsp-base/type:lsp-array location))
  (:documentation "The definition of a symbol represented as one or many {@link Location locations}.
For most programming languages there is only one location at which a symbol is
defined.

Servers should prefer returning `DefinitionLink` over `Definition` if supported
by the client."))

(lem-lsp-base/type:define-type-alias definition-link
    location-link
  (:documentation "Information about where a symbol is defined.

Provides additional metadata over normal {@link Location location} definitions, including the range of
the defining symbol"))

(lem-lsp-base/type:define-type-alias lsp-array
    (lem-lsp-base/type:lsp-array lsp-any)
  (:documentation "LSP arrays.
@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias lsp-any
    (common-lisp:or lsp-object lsp-array lem-lsp-base/type:lsp-string lem-lsp-base/type:lsp-integer
                    lem-lsp-base/type:lsp-uinteger lem-lsp-base/type:lsp-decimal
                    lem-lsp-base/type:lsp-boolean lem-lsp-base/type:lsp-null)
  (:documentation "The LSP any type.
Please note that strictly speaking a property with the value `undefined`
can't be converted into JSON preserving the property name. However for
convenience it is allowed and assumed that all these properties are
optional as well.
@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias declaration
    (common-lisp:or location (lem-lsp-base/type:lsp-array location))
  (:documentation
   "The declaration of a symbol representation as one or many {@link Location locations}."))

(lem-lsp-base/type:define-type-alias declaration-link
    location-link
  (:documentation "Information about where a symbol is declared.

Provides additional metadata over normal {@link Location location} declarations, including the range of
the declaring symbol.

Servers should prefer returning `DeclarationLink` over `Declaration` if supported
by the client."))

(lem-lsp-base/type:define-type-alias inline-value
    (common-lisp:or inline-value-text inline-value-variable-lookup
                    inline-value-evaluatable-expression)
  (:documentation "Inline value information can be provided by different means:
- directly as a text value (class InlineValueText).
- as a name to use for a variable lookup (class InlineValueVariableLookup)
- as an evaluatable expression (class InlineValueEvaluatableExpression)
The InlineValue types combines all inline value types into one type.

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias document-diagnostic-report
    (common-lisp:or related-full-document-diagnostic-report
                    related-unchanged-document-diagnostic-report)
  (:documentation "The result of a document diagnostic pull request. A report can
either be a full report containing all diagnostics for the
requested document or an unchanged report indicating that nothing
has changed in terms of diagnostics in comparison to the last
pull request.

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias prepare-rename-result
    (common-lisp:or range
                    (lem-lsp-base/type:lsp-interface
                     ((range :type range) (placeholder :type lem-lsp-base/type:lsp-string)))
                    (lem-lsp-base/type:lsp-interface
                     ((default-behavior :type lem-lsp-base/type:lsp-boolean)))))

(lem-lsp-base/type:define-type-alias document-selector
    (lem-lsp-base/type:lsp-array document-filter)
  (:documentation "A document selector is the combination of one or many document filters.

@sample `let sel:DocumentSelector = [{ language: 'typescript' }, { language: 'json', pattern: '**∕tsconfig.json' }]`;

The use of a string as a document filter is deprecated @since 3.16.0.")
  (:since "3.16.0."))

(lem-lsp-base/type:define-type-alias progress-token
    (common-lisp:or lem-lsp-base/type:lsp-integer lem-lsp-base/type:lsp-string))

(lem-lsp-base/type:define-type-alias change-annotation-identifier
    lem-lsp-base/type:lsp-string
  (:documentation "An identifier to refer to a change annotation stored with a workspace edit."))

(lem-lsp-base/type:define-type-alias workspace-document-diagnostic-report
    (common-lisp:or workspace-full-document-diagnostic-report
                    workspace-unchanged-document-diagnostic-report)
  (:documentation "A workspace diagnostic document report.

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias text-document-content-change-event
    (common-lisp:or
     (lem-lsp-base/type:lsp-interface
      ((range :type range :documentation "The range of the document that changed.")
       (range-length :type lem-lsp-base/type:lsp-uinteger :optional common-lisp:t :documentation
        "The optional length of the range that got replaced.

@deprecated use range instead.")
       (text :type lem-lsp-base/type:lsp-string :documentation
        "The new text for the provided range.")))
     (lem-lsp-base/type:lsp-interface
      ((text :type lem-lsp-base/type:lsp-string :documentation
        "The new text of the whole document."))))
  (:documentation "An event describing a change to a text document. If only a text is provided
it is considered to be the full content of the document."))

(lem-lsp-base/type:define-type-alias marked-string
    (common-lisp:or lem-lsp-base/type:lsp-string
                    (lem-lsp-base/type:lsp-interface
                     ((language :type lem-lsp-base/type:lsp-string)
                      (value :type lem-lsp-base/type:lsp-string))))
  (:deprecated "use MarkupContent instead.")
  (:documentation
   "MarkedString can be used to render human readable text. It is either a markdown string
or a code-block that provides a language and a code snippet. The language identifier
is semantically equal to the optional language identifier in fenced code blocks in GitHub
issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

The pair of a language and a value is an equivalent to markdown:
```${language}
${value}
```

Note that markdown strings will be sanitized - that means html will be escaped.
@deprecated use MarkupContent instead."))

(lem-lsp-base/type:define-type-alias document-filter
    (common-lisp:or text-document-filter notebook-cell-text-document-filter)
  (:documentation "A document filter describes a top level text document or
a notebook cell document.

@since 3.17.0 - proposed support for NotebookCellTextDocumentFilter.")
  (:since "3.17.0 - proposed support for NotebookCellTextDocumentFilter."))

(lem-lsp-base/type:define-type-alias lsp-object
    (lem-lsp-base/type:lsp-map string lsp-any)
  (:documentation "LSP object definition.
@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias glob-pattern
    (common-lisp:or pattern relative-pattern)
  (:documentation "The glob pattern. Either a string pattern or a relative pattern.

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias text-document-filter
    (common-lisp:or
     (lem-lsp-base/type:lsp-interface
      ((language :type lem-lsp-base/type:lsp-string :documentation
        "A language id, like `typescript`.")
       (scheme :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples.")))
     (lem-lsp-base/type:lsp-interface
      ((language :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A language id, like `typescript`.")
       (scheme :type lem-lsp-base/type:lsp-string :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples.")))
     (lem-lsp-base/type:lsp-interface
      ((language :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A language id, like `typescript`.")
       (scheme :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-lsp-base/type:lsp-string :documentation
        "A glob pattern, like **​/*.{ts,js}. See TextDocumentFilter for examples."))))
  (:documentation "A document filter denotes a document by different properties like
the {@link TextDocument.languageId language}, the {@link Uri.scheme scheme} of
its resource, or a glob-pattern that is applied to the {@link TextDocument.fileName path}.

Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)

@sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`
@sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias notebook-document-filter
    (common-lisp:or
     (lem-lsp-base/type:lsp-interface
      ((notebook-type :type lem-lsp-base/type:lsp-string :documentation
        "The type of the enclosing notebook.")
       (scheme :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A glob pattern.")))
     (lem-lsp-base/type:lsp-interface
      ((notebook-type :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "The type of the enclosing notebook.")
       (scheme :type lem-lsp-base/type:lsp-string :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A glob pattern.")))
     (lem-lsp-base/type:lsp-interface
      ((notebook-type :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "The type of the enclosing notebook.")
       (scheme :type lem-lsp-base/type:lsp-string :optional common-lisp:t :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-lsp-base/type:lsp-string :documentation "A glob pattern."))))
  (:documentation "A notebook document filter denotes a notebook document by
different properties. The properties will be match
against the notebook's URI (same as with documents)

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-type-alias pattern
    lem-lsp-base/type:lsp-string
  (:documentation
   "The glob pattern to watch relative to the base path. Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)

@since 3.17.0")
  (:since "3.17.0"))

(lem-lsp-base/type:define-request-message text-document/implementation
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the implementation locations of a symbol at a given text
document position. The request's parameter is of type {@link TextDocumentPositionParams}
the response is of type {@link Definition} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/implementation"
  :params
  'implementation-params
  :partial-result
  '(common-lisp:or (lem-lsp-base/type:lsp-array location)
                   (lem-lsp-base/type:lsp-array definition-link))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'implementation-registration-options
  :result
  '(common-lisp:or definition (lem-lsp-base/type:lsp-array definition-link)
                   lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/type-definition
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the type definition locations of a symbol at a given text
document position. The request's parameter is of type {@link TextDocumentPositionParams}
the response is of type {@link Definition} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/typeDefinition"
  :params
  'type-definition-params
  :partial-result
  '(common-lisp:or (lem-lsp-base/type:lsp-array location)
                   (lem-lsp-base/type:lsp-array definition-link))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'type-definition-registration-options
  :result
  '(common-lisp:or definition (lem-lsp-base/type:lsp-array definition-link)
                   lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message workspace/workspace-folders
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/workspaceFolders"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array workspace-folder) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message workspace/configuration
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The 'workspace/configuration' request is sent from the server to the client to fetch a certain
configuration setting.

This pull model replaces the old push model where the client signaled configuration change via an
event. If the server still needs to react to configuration changes (since the server caches the
result of `workspace/configuration` requests) the server should register for an empty configuration
change event and empty the cache if such an event is received."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/configuration"
  :params
  'configuration-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(lem-lsp-base/type:lsp-array lsp-any)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/document-color
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to list all color symbols found in a given text document. The request's
parameter is of type {@link DocumentColorParams} the
response is of type {@link ColorInformation ColorInformation[]} or a Thenable
that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/documentColor"
  :params
  'document-color-params
  :partial-result
  '(lem-lsp-base/type:lsp-array color-information)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-color-registration-options
  :result
  '(lem-lsp-base/type:lsp-array color-information)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/color-presentation
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to list all presentation for a color. The request's
parameter is of type {@link ColorPresentationParams} the
response is of type {@link ColorInformation ColorInformation[]} or a Thenable
that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/colorPresentation"
  :params
  'color-presentation-params
  :partial-result
  '(lem-lsp-base/type:lsp-array color-presentation)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  '(common-lisp:and work-done-progress-options text-document-registration-options)
  :result
  '(lem-lsp-base/type:lsp-array color-presentation)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/folding-range
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide folding ranges in a document. The request's
parameter is of type {@link FoldingRangeParams}, the
response is of type {@link FoldingRangeList} or a Thenable
that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/foldingRange"
  :params
  'folding-range-params
  :partial-result
  '(lem-lsp-base/type:lsp-array folding-range)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'folding-range-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array folding-range) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message workspace/folding-range/refresh
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.18.0
@proposed"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/foldingRange/refresh"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:t
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  "3.18.0")

(lem-lsp-base/type:define-request-message text-document/declaration
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the type definition locations of a symbol at a given text
document position. The request's parameter is of type {@link TextDocumentPositionParams}
the response is of type {@link Declaration} or a typed array of {@link DeclarationLink}
or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/declaration"
  :params
  'declaration-params
  :partial-result
  '(common-lisp:or (lem-lsp-base/type:lsp-array location)
                   (lem-lsp-base/type:lsp-array declaration-link))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'declaration-registration-options
  :result
  '(common-lisp:or declaration (lem-lsp-base/type:lsp-array declaration-link)
                   lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/selection-range
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide selection ranges in a document. The request's
parameter is of type {@link SelectionRangeParams}, the
response is of type {@link SelectionRange SelectionRange[]} or a Thenable
that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/selectionRange"
  :params
  'selection-range-params
  :partial-result
  '(lem-lsp-base/type:lsp-array selection-range)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'selection-range-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array selection-range) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message window/work-done-progress/create
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The `window/workDoneProgress/create` request is sent from the server to the client to initiate progress
reporting from the server."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "window/workDoneProgress/create"
  :params
  'work-done-progress-create-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/prepare-call-hierarchy
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to result a `CallHierarchyItem` in a document at a given position.
Can be used as an input to an incoming or outgoing call hierarchy.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/prepareCallHierarchy"
  :params
  'call-hierarchy-prepare-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'call-hierarchy-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array call-hierarchy-item) lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message call-hierarchy/incoming-calls
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the incoming calls for a given `CallHierarchyItem`.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "callHierarchy/incomingCalls"
  :params
  'call-hierarchy-incoming-calls-params
  :partial-result
  '(lem-lsp-base/type:lsp-array call-hierarchy-incoming-call)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array call-hierarchy-incoming-call)
                   lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message call-hierarchy/outgoing-calls
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the outgoing calls for a given `CallHierarchyItem`.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "callHierarchy/outgoingCalls"
  :params
  'call-hierarchy-outgoing-calls-params
  :partial-result
  '(lem-lsp-base/type:lsp-array call-hierarchy-outgoing-call)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array call-hierarchy-outgoing-call)
                   lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message text-document/semantic-tokens/full
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/semanticTokens/full"
  :params
  'semantic-tokens-params
  :partial-result
  'semantic-tokens-partial-result
  :proposed
  common-lisp:nil
  :registration-method
  "textDocument/semanticTokens"
  :registration-options
  'semantic-tokens-registration-options
  :result
  '(common-lisp:or semantic-tokens lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message text-document/semantic-tokens/full/delta
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/semanticTokens/full/delta"
  :params
  'semantic-tokens-delta-params
  :partial-result
  '(common-lisp:or semantic-tokens-partial-result semantic-tokens-delta-partial-result)
  :proposed
  common-lisp:nil
  :registration-method
  "textDocument/semanticTokens"
  :registration-options
  'semantic-tokens-registration-options
  :result
  '(common-lisp:or semantic-tokens semantic-tokens-delta lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message text-document/semantic-tokens/range
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/semanticTokens/range"
  :params
  'semantic-tokens-range-params
  :partial-result
  'semantic-tokens-partial-result
  :proposed
  common-lisp:nil
  :registration-method
  "textDocument/semanticTokens"
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or semantic-tokens lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message workspace/semantic-tokens/refresh
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/semanticTokens/refresh"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message window/show-document
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to show a document. This request might open an
external program depending on the value of the URI to open.
For example a request to open `https://code.visualstudio.com/`
will very likely open the URI in a WEB browser.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "window/showDocument"
  :params
  'show-document-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'show-document-result
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message text-document/linked-editing-range
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide ranges that can be edited together.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/linkedEditingRange"
  :params
  'linked-editing-range-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'linked-editing-range-registration-options
  :result
  '(common-lisp:or linked-editing-ranges lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message workspace/will-create-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The will create files request is sent from the client to the server before files are actually
created as long as the creation is triggered from within the client.

The request can return a `WorkspaceEdit` which will be applied to workspace before the
files are created. Hence the `WorkspaceEdit` can not manipulate the content of the file
to be created.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "workspace/willCreateFiles"
  :params
  'create-files-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'file-operation-registration-options
  :result
  '(common-lisp:or workspace-edit lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message workspace/will-rename-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The will rename files request is sent from the client to the server before files are actually
renamed as long as the rename is triggered from within the client.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "workspace/willRenameFiles"
  :params
  'rename-files-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'file-operation-registration-options
  :result
  '(common-lisp:or workspace-edit lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message workspace/will-delete-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The did delete files notification is sent from the client to the server when
files were deleted from within the client.

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "workspace/willDeleteFiles"
  :params
  'delete-files-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'file-operation-registration-options
  :result
  '(common-lisp:or workspace-edit lem-lsp-base/type:lsp-null)
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message text-document/moniker
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to get the moniker of a symbol at a given text document position.
The request parameter is of type {@link TextDocumentPositionParams}.
The response is of type {@link Moniker Moniker[]} or `null`."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/moniker"
  :params
  'moniker-params
  :partial-result
  '(lem-lsp-base/type:lsp-array moniker)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'moniker-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array moniker) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/prepare-type-hierarchy
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to result a `TypeHierarchyItem` in a document at a given position.
Can be used as an input to a subtypes or supertypes type hierarchy.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/prepareTypeHierarchy"
  :params
  'type-hierarchy-prepare-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'type-hierarchy-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array type-hierarchy-item) lem-lsp-base/type:lsp-null)
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message type-hierarchy/supertypes
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the supertypes for a given `TypeHierarchyItem`.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "typeHierarchy/supertypes"
  :params
  'type-hierarchy-supertypes-params
  :partial-result
  '(lem-lsp-base/type:lsp-array type-hierarchy-item)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array type-hierarchy-item) lem-lsp-base/type:lsp-null)
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message type-hierarchy/subtypes
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the subtypes for a given `TypeHierarchyItem`.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "typeHierarchy/subtypes"
  :params
  'type-hierarchy-subtypes-params
  :partial-result
  '(lem-lsp-base/type:lsp-array type-hierarchy-item)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array type-hierarchy-item) lem-lsp-base/type:lsp-null)
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message text-document/inline-value
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide inline values in a document. The request's parameter is of
type {@link InlineValueParams}, the response is of type
{@link InlineValue InlineValue[]} or a Thenable that resolves to such.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/inlineValue"
  :params
  'inline-value-params
  :partial-result
  '(lem-lsp-base/type:lsp-array inline-value)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'inline-value-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array inline-value) lem-lsp-base/type:lsp-null)
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message workspace/inline-value/refresh
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/inlineValue/refresh"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message text-document/inlay-hint
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide inlay hints in a document. The request's parameter is of
type {@link InlayHintsParams}, the response is of type
{@link InlayHint InlayHint[]} or a Thenable that resolves to such.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/inlayHint"
  :params
  'inlay-hint-params
  :partial-result
  '(lem-lsp-base/type:lsp-array inlay-hint)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'inlay-hint-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array inlay-hint) lem-lsp-base/type:lsp-null)
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message inlay-hint/resolve
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve additional properties for an inlay hint.
The request's parameter is of type {@link InlayHint}, the response is
of type {@link InlayHint} or a Thenable that resolves to such.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "inlayHint/resolve"
  :params
  'inlay-hint
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'inlay-hint
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message workspace/inlay-hint/refresh
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/inlayHint/refresh"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message text-document/diagnostic
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The document diagnostic request definition.

@since 3.17.0"
  :error-data
  'diagnostic-server-cancellation-data
  :message-direction
  "clientToServer"
  :method
  "textDocument/diagnostic"
  :params
  'document-diagnostic-params
  :partial-result
  'document-diagnostic-report-partial-result
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'diagnostic-registration-options
  :result
  'document-diagnostic-report
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message workspace/diagnostic
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The workspace diagnostic request definition.

@since 3.17.0"
  :error-data
  'diagnostic-server-cancellation-data
  :message-direction
  "clientToServer"
  :method
  "workspace/diagnostic"
  :params
  'workspace-diagnostic-params
  :partial-result
  'workspace-diagnostic-report-partial-result
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'workspace-diagnostic-report
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message workspace/diagnostic/refresh
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The diagnostic refresh request definition.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/diagnostic/refresh"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message text-document/inline-completion
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide inline completions in a document. The request's parameter is of
type {@link InlineCompletionParams}, the response is of type
{@link InlineCompletion InlineCompletion[]} or a Thenable that resolves to such.

@since 3.18.0
@proposed"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/inlineCompletion"
  :params
  'inline-completion-params
  :partial-result
  '(lem-lsp-base/type:lsp-array inline-completion-item)
  :proposed
  common-lisp:t
  :registration-method
  common-lisp:nil
  :registration-options
  'inline-completion-registration-options
  :result
  '(common-lisp:or inline-completion-list (lem-lsp-base/type:lsp-array inline-completion-item)
                   lem-lsp-base/type:lsp-null)
  :since
  "3.18.0")

(lem-lsp-base/type:define-request-message client/register-capability
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The `client/registerCapability` request is sent from the server to the client to register a new capability
handler on the client side."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "client/registerCapability"
  :params
  'registration-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message client/unregister-capability
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability
handler on the client side."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "client/unregisterCapability"
  :params
  'unregistration-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message initialize
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The initialize request is sent from the client to the server.
It is sent once as the request after starting up the server.
The requests parameter is of type {@link InitializeParams}
the response if of type {@link InitializeResult} of a Thenable that
resolves to such."
  :error-data
  'initialize-error
  :message-direction
  "clientToServer"
  :method
  "initialize"
  :params
  'initialize-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'initialize-result
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message shutdown
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A shutdown request is sent from the client to the server.
It is sent once when the client decides to shutdown the
server. The only notification that is sent after a shutdown request
is the exit event."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "shutdown"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message window/show-message-request
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The show message request is sent from the server to the client to show a message
and a set of options actions to the user."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "window/showMessageRequest"
  :params
  'show-message-request-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or message-action-item lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/will-save-wait-until
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A document will save request is sent from the client to the server before
the document is actually saved. The request can return an array of TextEdits
which will be applied to the text document before it is saved. Please note that
clients might drop results if computing the text edits took too long or if a
server constantly fails on this request. This is done to keep the save fast and
reliable."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/willSaveWaitUntil"
  :params
  'will-save-text-document-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'text-document-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array text-edit) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/completion
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Request to request completion at a given text document position. The request's
parameter is of type {@link TextDocumentPosition} the response
is of type {@link CompletionItem CompletionItem[]} or {@link CompletionList}
or a Thenable that resolves to such.

The request can delay the computation of the {@link CompletionItem.detail `detail`}
and {@link CompletionItem.documentation `documentation`} properties to the `completionItem/resolve`
request. However, properties that are needed for the initial sorting and filtering, like `sortText`,
`filterText`, `insertText`, and `textEdit`, must not be changed during resolve."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/completion"
  :params
  'completion-params
  :partial-result
  '(lem-lsp-base/type:lsp-array completion-item)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'completion-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array completion-item) completion-list
                   lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message completion-item/resolve
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Request to resolve additional information for a given completion item.The request's
parameter is of type {@link CompletionItem} the response
is of type {@link CompletionItem} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "completionItem/resolve"
  :params
  'completion-item
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'completion-item
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/hover
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Request to request hover information at a given text document position. The request's
parameter is of type {@link TextDocumentPosition} the response is of
type {@link Hover} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/hover"
  :params
  'hover-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'hover-registration-options
  :result
  '(common-lisp:or hover lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/signature-help
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  common-lisp:nil
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/signatureHelp"
  :params
  'signature-help-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'signature-help-registration-options
  :result
  '(common-lisp:or signature-help lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/definition
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the definition location of a symbol at a given text
document position. The request's parameter is of type {@link TextDocumentPosition}
the response is of either type {@link Definition} or a typed array of
{@link DefinitionLink} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/definition"
  :params
  'definition-params
  :partial-result
  '(common-lisp:or (lem-lsp-base/type:lsp-array location)
                   (lem-lsp-base/type:lsp-array definition-link))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'definition-registration-options
  :result
  '(common-lisp:or definition (lem-lsp-base/type:lsp-array definition-link)
                   lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/references
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve project-wide references for the symbol denoted
by the given text document position. The request's parameter is of
type {@link ReferenceParams} the response is of type
{@link Location Location[]} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/references"
  :params
  'reference-params
  :partial-result
  '(lem-lsp-base/type:lsp-array location)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'reference-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array location) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/document-highlight
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Request to resolve a {@link DocumentHighlight} for a given
text document position. The request's parameter is of type {@link TextDocumentPosition}
the request response is an array of type {@link DocumentHighlight}
or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/documentHighlight"
  :params
  'document-highlight-params
  :partial-result
  '(lem-lsp-base/type:lsp-array document-highlight)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-highlight-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array document-highlight) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/document-symbol
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to list all symbols found in a given text document. The request's
parameter is of type {@link TextDocumentIdentifier} the
response is of type {@link SymbolInformation SymbolInformation[]} or a Thenable
that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/documentSymbol"
  :params
  'document-symbol-params
  :partial-result
  '(common-lisp:or (lem-lsp-base/type:lsp-array symbol-information)
                   (lem-lsp-base/type:lsp-array document-symbol))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-symbol-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array symbol-information)
                   (lem-lsp-base/type:lsp-array document-symbol) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/code-action
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide commands for the given text document and range."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/codeAction"
  :params
  'code-action-params
  :partial-result
  '(lem-lsp-base/type:lsp-array (common-lisp:or command code-action))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'code-action-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array (common-lisp:or command code-action))
                   lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message code-action/resolve
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Request to resolve additional information for a given code action.The request's
parameter is of type {@link CodeAction} the response
is of type {@link CodeAction} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "codeAction/resolve"
  :params
  'code-action
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'code-action
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message workspace/symbol
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to list project-wide symbols matching the query string given
by the {@link WorkspaceSymbolParams}. The response is
of type {@link SymbolInformation SymbolInformation[]} or a Thenable that
resolves to such.

@since 3.17.0 - support for WorkspaceSymbol in the returned data. Clients
 need to advertise support for WorkspaceSymbols via the client capability
 `workspace.symbol.resolveSupport`.
"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "workspace/symbol"
  :params
  'workspace-symbol-params
  :partial-result
  '(common-lisp:or (lem-lsp-base/type:lsp-array symbol-information)
                   (lem-lsp-base/type:lsp-array workspace-symbol))
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'workspace-symbol-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array symbol-information)
                   (lem-lsp-base/type:lsp-array workspace-symbol) lem-lsp-base/type:lsp-null)
  :since
  "3.17.0 - support for WorkspaceSymbol in the returned data. Clients
need to advertise support for WorkspaceSymbols via the client capability
`workspace.symbol.resolveSupport`.")

(lem-lsp-base/type:define-request-message workspace-symbol/resolve
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve the range inside the workspace
symbol's location.

@since 3.17.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "workspaceSymbol/resolve"
  :params
  'workspace-symbol
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'workspace-symbol
  :since
  "3.17.0")

(lem-lsp-base/type:define-request-message text-document/code-lens
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide code lens for the given text document."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/codeLens"
  :params
  'code-lens-params
  :partial-result
  '(lem-lsp-base/type:lsp-array code-lens)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'code-lens-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array code-lens) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message code-lens/resolve
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to resolve a command for a given code lens."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "codeLens/resolve"
  :params
  'code-lens
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'code-lens
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message workspace/code-lens/refresh
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to refresh all code actions

@since 3.16.0"
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/codeLens/refresh"
  :params
  'common-lisp:nil
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'lem-lsp-base/type:lsp-null
  :since
  "3.16.0")

(lem-lsp-base/type:define-request-message text-document/document-link
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to provide document links"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/documentLink"
  :params
  'document-link-params
  :partial-result
  '(lem-lsp-base/type:lsp-array document-link)
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-link-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array document-link) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message document-link/resolve
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Request to resolve additional information for a given document link. The request's
parameter is of type {@link DocumentLink} the response
is of type {@link DocumentLink} or a Thenable that resolves to such."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "documentLink/resolve"
  :params
  'document-link
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'document-link
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/formatting
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to format a whole document."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/formatting"
  :params
  'document-formatting-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-formatting-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array text-edit) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/range-formatting
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to format a range in a document."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/rangeFormatting"
  :params
  'document-range-formatting-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-range-formatting-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array text-edit) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/ranges-formatting
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to format ranges in a document.

@since 3.18.0
@proposed"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/rangesFormatting"
  :params
  'document-ranges-formatting-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:t
  :registration-method
  common-lisp:nil
  :registration-options
  'document-range-formatting-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array text-edit) lem-lsp-base/type:lsp-null)
  :since
  "3.18.0")

(lem-lsp-base/type:define-request-message text-document/on-type-formatting
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to format a document on type."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/onTypeFormatting"
  :params
  'document-on-type-formatting-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'document-on-type-formatting-registration-options
  :result
  '(common-lisp:or (lem-lsp-base/type:lsp-array text-edit) lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/rename
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to rename a symbol."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/rename"
  :params
  'rename-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'rename-registration-options
  :result
  '(common-lisp:or workspace-edit lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message text-document/prepare-rename
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request to test and perform the setup necessary for a rename.

@since 3.16 - support for default behavior"
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "textDocument/prepareRename"
  :params
  'prepare-rename-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  '(common-lisp:or prepare-rename-result lem-lsp-base/type:lsp-null)
  :since
  "3.16 - support for default behavior")

(lem-lsp-base/type:define-request-message workspace/execute-command
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request send from the client to the server to execute a command. The request might return
a workspace edit which the client will apply to the workspace."
  :error-data
  'common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "workspace/executeCommand"
  :params
  'execute-command-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'execute-command-registration-options
  :result
  '(common-lisp:or lsp-any lem-lsp-base/type:lsp-null)
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-request-message workspace/apply-edit
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A request sent from the server to the client to modified certain resources."
  :error-data
  'common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "workspace/applyEdit"
  :params
  'apply-workspace-edit-params
  :partial-result
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :result
  'apply-workspace-edit-result
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message workspace/did-change-workspace-folders
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace
folder configuration changes."
  :message-direction
  "clientToServer"
  :method
  "workspace/didChangeWorkspaceFolders"
  :params
  'did-change-workspace-folders-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message window/work-done-progress/cancel
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The `window/workDoneProgress/cancel` notification is sent from  the client to the server to cancel a progress
initiated on the server side."
  :message-direction
  "clientToServer"
  :method
  "window/workDoneProgress/cancel"
  :params
  'work-done-progress-cancel-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message workspace/did-create-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The did create files notification is sent from the client to the server when
files were created from within the client.

@since 3.16.0"
  :message-direction
  "clientToServer"
  :method
  "workspace/didCreateFiles"
  :params
  'create-files-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'file-operation-registration-options
  :since
  "3.16.0")

(lem-lsp-base/type:define-notification-message workspace/did-rename-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The did rename files notification is sent from the client to the server when
files were renamed from within the client.

@since 3.16.0"
  :message-direction
  "clientToServer"
  :method
  "workspace/didRenameFiles"
  :params
  'rename-files-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'file-operation-registration-options
  :since
  "3.16.0")

(lem-lsp-base/type:define-notification-message workspace/did-delete-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The will delete files request is sent from the client to the server before files are actually
deleted as long as the deletion is triggered from within the client.

@since 3.16.0"
  :message-direction
  "clientToServer"
  :method
  "workspace/didDeleteFiles"
  :params
  'delete-files-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'file-operation-registration-options
  :since
  "3.16.0")

(lem-lsp-base/type:define-notification-message notebook-document/did-open
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A notification sent when a notebook opens.

@since 3.17.0"
  :message-direction
  "clientToServer"
  :method
  "notebookDocument/didOpen"
  :params
  'did-open-notebook-document-params
  :proposed
  common-lisp:nil
  :registration-method
  "notebookDocument/sync"
  :registration-options
  'common-lisp:nil
  :since
  "3.17.0")

(lem-lsp-base/type:define-notification-message notebook-document/did-change
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "notebookDocument/didChange"
  :params
  'did-change-notebook-document-params
  :proposed
  common-lisp:nil
  :registration-method
  "notebookDocument/sync"
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message notebook-document/did-save
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A notification sent when a notebook document is saved.

@since 3.17.0"
  :message-direction
  "clientToServer"
  :method
  "notebookDocument/didSave"
  :params
  'did-save-notebook-document-params
  :proposed
  common-lisp:nil
  :registration-method
  "notebookDocument/sync"
  :registration-options
  'common-lisp:nil
  :since
  "3.17.0")

(lem-lsp-base/type:define-notification-message notebook-document/did-close
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A notification sent when a notebook closes.

@since 3.17.0"
  :message-direction
  "clientToServer"
  :method
  "notebookDocument/didClose"
  :params
  'did-close-notebook-document-params
  :proposed
  common-lisp:nil
  :registration-method
  "notebookDocument/sync"
  :registration-options
  'common-lisp:nil
  :since
  "3.17.0")

(lem-lsp-base/type:define-notification-message initialized
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The initialized notification is sent from the client to the
server after the client is fully initialized and the server
is allowed to send requests from the server to the client."
  :message-direction
  "clientToServer"
  :method
  "initialized"
  :params
  'initialized-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message exit
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The exit event is sent from the client to the server to
ask the server to exit its process."
  :message-direction
  "clientToServer"
  :method
  "exit"
  :params
  'common-lisp:nil
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message workspace/did-change-configuration
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The configuration change notification is sent from the client to the server
when the client's configuration has changed. The notification contains
the changed configuration as defined by the language client."
  :message-direction
  "clientToServer"
  :method
  "workspace/didChangeConfiguration"
  :params
  'did-change-configuration-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'did-change-configuration-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message window/show-message
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The show message notification is sent from a server to a client to ask
the client to display a particular message in the user interface."
  :message-direction
  "serverToClient"
  :method
  "window/showMessage"
  :params
  'show-message-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message window/log-message
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The log message notification is sent from the server to the client to ask
the client to log a particular message."
  :message-direction
  "serverToClient"
  :method
  "window/logMessage"
  :params
  'log-message-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message telemetry/event
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The telemetry event notification is sent from the server to the client to ask
the client to log telemetry data."
  :message-direction
  "serverToClient"
  :method
  "telemetry/event"
  :params
  'lsp-any
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message text-document/did-open
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The document open notification is sent from the client to the server to signal
newly opened text documents. The document's truth is now managed by the client
and the server must not try to read the document's truth using the document's
uri. Open in this sense means it is managed by the client. It doesn't necessarily
mean that its content is presented in an editor. An open notification must not
be sent more than once without a corresponding close notification send before.
This means open and close notification must be balanced and the max open count
is one."
  :message-direction
  "clientToServer"
  :method
  "textDocument/didOpen"
  :params
  'did-open-text-document-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'text-document-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message text-document/did-change
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The document change notification is sent from the client to the server to signal
changes to a text document."
  :message-direction
  "clientToServer"
  :method
  "textDocument/didChange"
  :params
  'did-change-text-document-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'text-document-change-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message text-document/did-close
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The document close notification is sent from the client to the server when
the document got closed in the client. The document's truth now exists where
the document's uri points to (e.g. if the document's uri is a file uri the
truth now exists on disk). As with the open notification the close notification
is about managing the document's content. Receiving a close notification
doesn't mean that the document was open in an editor before. A close
notification requires a previous open notification to be sent."
  :message-direction
  "clientToServer"
  :method
  "textDocument/didClose"
  :params
  'did-close-text-document-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'text-document-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message text-document/did-save
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The document save notification is sent from the client to the server when
the document got saved in the client."
  :message-direction
  "clientToServer"
  :method
  "textDocument/didSave"
  :params
  'did-save-text-document-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'text-document-save-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message text-document/will-save
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "A document will save notification is sent from the client to the server before
the document is actually saved."
  :message-direction
  "clientToServer"
  :method
  "textDocument/willSave"
  :params
  'will-save-text-document-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'text-document-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message workspace/did-change-watched-files
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "The watched files notification is sent from the client to the server when
the client detects changes to file watched by the language client."
  :message-direction
  "clientToServer"
  :method
  "workspace/didChangeWatchedFiles"
  :params
  'did-change-watched-files-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'did-change-watched-files-registration-options
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message text-document/publish-diagnostics
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  "Diagnostics notification are sent from the server to the client to signal
results of validation runs."
  :message-direction
  "serverToClient"
  :method
  "textDocument/publishDiagnostics"
  :params
  'publish-diagnostics-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message /set-trace
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  common-lisp:nil
  :message-direction
  "clientToServer"
  :method
  "$/setTrace"
  :params
  'set-trace-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message /log-trace
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  common-lisp:nil
  :message-direction
  "serverToClient"
  :method
  "$/logTrace"
  :params
  'log-trace-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message /cancel-request
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  common-lisp:nil
  :message-direction
  "both"
  :method
  "$/cancelRequest"
  :params
  'cancel-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)

(lem-lsp-base/type:define-notification-message /progress
    common-lisp:nil
  :deprecated
  common-lisp:nil
  :documentation
  common-lisp:nil
  :message-direction
  "both"
  :method
  "$/progress"
  :params
  'progress-params
  :proposed
  common-lisp:nil
  :registration-method
  common-lisp:nil
  :registration-options
  'common-lisp:nil
  :since
  common-lisp:nil)