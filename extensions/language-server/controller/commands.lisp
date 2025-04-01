(in-package :lem-language-server)

(define-lsp-command eval-previous-form-command "cl-lsp.eval-last-expression" (arguments)
  (let ((text-document-position-params
          (convert-from-json (elt arguments 0)
                             'lsp:text-document-position-params)))
    (eval-last-expression text-document-position-params))
  :null)

(define-lsp-command eval-range-command "cl-lsp.eval-range" (arguments)
  (let* ((text-document-identifier (convert-from-json (elt arguments 0) 'lsp:text-document-identifier))
         (range (convert-from-json (elt arguments 1) 'lsp:range))
         (text-document (find-text-document text-document-identifier))
         (buffer (text-document-buffer text-document)))
    (lem:with-point ((start (lem:buffer-point buffer))
                     (end (lem:buffer-point buffer)))
      (move-to-lsp-position start (lsp:range-start range))
      (move-to-lsp-position end (lsp:range-end range))
      ;; TODO
      ;; (eval-range start end)
      ))
  :null)

(define-lsp-command interrupt-eval-command "cl-lsp.interrupt" (arguments)
  (let ((id (elt arguments 0)))
    (interrupt-eval id))
  :null)
