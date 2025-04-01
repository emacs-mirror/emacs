(in-package :lem-language-server)

(defun make-buffer (uri version)
  (lem:make-buffer (format nil "*lsp ~A ~A*" uri version)
                   :temporary t
                   :syntax-table lem-lisp-syntax:*syntax-table*
                   :enable-undo-p nil))

(define-request (text-document-did-open-request "textDocument/didOpen")
    (params lsp:did-open-text-document-params)
  (with-accessors ((item lsp:did-open-text-document-params-text-document))
      params
    (with-accessors ((uri lsp:text-document-item-uri)
                     (language-id lsp:text-document-item-language-id)
                     (version lsp:text-document-item-version)
                     (text lsp:text-document-item-text))
        item
      (let ((buffer (make-buffer uri version)))
        (lem:insert-string (lem:buffer-point buffer) text)
        (register-text-document :uri uri
                                :language-id language-id
                                :version version
                                :buffer buffer))
      (values))))

(define-request (text-document-did-change-request "textDocument/didChange")
    (params lsp:did-change-text-document-params)
  (with-accessors ((text-document-identifier lsp:did-change-text-document-params-text-document)
                   (content-changes lsp:did-change-text-document-params-content-changes))
      params
    (let ((text-document (find-text-document text-document-identifier)))
      (lem/common/utils:do-sequence (content-change content-changes)
        (edit-text-document text-document content-change))))
  (values))

(define-request (text-document-will-save-request "textDocument/willSave")
    (params lsp:will-save-text-document-params)
  (declare (ignore params))
  (values))

(define-request (text-document-will-save-wait-until-request "textDocument/willSaveWaitUntil")
    (params lsp:will-save-text-document-params)
  (declare (ignore params))
  (values))

(define-request (text-document-did-save-request "textDocument/didSave")
    (params lsp:did-save-text-document-params)
  (let ((uri (lsp:text-document-identifier-uri
              (lsp:did-save-text-document-params-text-document params))))
    (declare (ignore uri)))
  (values))

(define-request (text-document-did-close-request "textDocument/didClose")
    (params lsp:did-close-text-document-params)
  (with-accessors ((text-document-identifier lsp:did-close-text-document-params-text-document))
      params
    (let ((text-document (find-text-document text-document-identifier)))
      (close-text-document text-document)))
  (values))
