(in-package :lem-language-server)

(defun in-package-line-p (line)
  (ppcre:register-groups-bind (package-name)
      ("^\\s*\\(\\s*(?:cl:|common-lisp:)?in-package (?:#?:|')?([^\)\\s]*)\\s*\\)"
       (string-downcase line))
    (string-upcase package-name)))

(defun buffer-package (buffer)
  (lem:with-point ((point (lem:buffer-start-point buffer)))
    (loop :when (in-package-line-p (lem:line-string point))
          :return :it
          :while (lem:line-offset point 1))))

(defun scan-current-package (point &optional (default "COMMON-LISP-USER"))
  (lem:with-point ((p point))
    (loop
      (let ((package-name (in-package-line-p (lem:line-string p))))
        (when package-name
          (return package-name)))
      (unless (lem:line-offset p -1)
        (return default)))))

(defun symbol-points-to-lsp-range (point)
  (lem:with-point ((start point)
                   (end point))
    (lem:skip-symbol-backward start)
    (lem:skip-symbol-forward end)
    (points-to-lsp-range start end)))

(defun definitions-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (remote-eval-sync *server*
                      `(micros:find-definitions-for-emacs ,symbol-string)
                      package-name)))

(defun move-to-location-position (point location-position)
  (destructuring-ecase location-position
    ((:position position)
     (lem:move-to-bytes point (1+ position)))
    ((:offset start offset)
     (lem:move-to-position point start)
     (lem:character-offset point offset))
    ((:line line-number &optional column)
     (lem:move-to-line point line-number)
     (if column
         (lem:line-offset point 0 column)
         (lem:back-to-indentation point)))
    ((:function-name name)
     (lem:buffer-start point)
     (lem:search-forward-regexp point
                                (ppcre:create-scanner
                                 `(:sequence
                                   "(def"
                                   (:greedy-repetition 1 nil (:char-class :word-char-class #\-))
                                   (:greedy-repetition 1 nil :whitespace-char-class)
                                   (:greedy-repetition 0 nil #\()
                                   ,name
                                   (:char-class :whitespace-char-class #\( #\)))
                                 :case-insensitive-mode t))
     (lem:form-offset point -1))
    ((:eof)
     (lem:buffer-end point))
    ;; maybe unused
    ((:method name specializers &rest qualifiers)
     (declare (ignore name specializers qualifiers)))
    ((:source-path source-path start-position)
     (declare (ignore source-path start-position)))))

(defun resolve-location-buffer (location-buffer)
  (destructuring-ecase location-buffer
    ((:file filename)
     (when (probe-file filename)
       (lem:find-file-buffer filename
                             :syntax-table lem-lisp-syntax:*syntax-table*
                             :temporary t)))
    ((:buffer-and-file buffer filename)
     (declare (ignore buffer))
     (lem:find-file-buffer filename
                           :syntax-table lem-lisp-syntax:*syntax-table*
                           :temporary t))
    ;; maybe unused
    ((:buffer buffer-name)
     (declare (ignore buffer-name)))
    ((:source-form string)
     (declare (ignore string)))
    ((:zip file entry)
     (declare (ignore file entry)))))

(defun resolve-location (location)
  (destructuring-ecase location
    ((:location location-buffer position hints)
     (declare (ignore hints))
     (let ((buffer (resolve-location-buffer location-buffer)))
       (cond ((null buffer)
              (log:error "unresolve location" location)
              nil)
             (t
              (lem:with-point ((point (lem:buffer-point buffer)))
                (when (move-to-location-position point position)
                  point))))))
    ((:error message)
     ;; TODO: send message to client
     (log:debug message)
     nil)))

(defun collect-points-from-definitions (definitions)
  (loop :for (dspec location) :in definitions
        :when (resolve-location location)
        :collect :it))

(define-request (go-to-definition-request "textDocument/definition") (params lsp:definition-params)
  (let* ((point (text-document-position-params-to-point params))
         (definitions (definitions-at-point point))
         (definition-points (collect-points-from-definitions definitions)))
    (when-let ((point (lem-lisp-syntax:search-local-definition
                       point
                       (lem:symbol-string-at-point point))))
      (push point definition-points))
    (convert-to-json (map 'vector #'point-to-lsp-location definition-points))))

(defun find-references-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (remote-eval-sync *server*
                      `(micros:xrefs '(:calls :macroexpands :binds
                                       :references :sets :specializes)
                                     ,symbol-string)
                      package-name)))

(define-request (find-references-request "textDocument/references") (params lsp:reference-params)
  (let* ((point (text-document-position-params-to-point params))
         (result (find-references-at-point point)))
    (convert-to-json
     (map 'vector
          #'point-to-lsp-location
          (loop :for (type . definitions) :in result
                :append (collect-points-from-definitions definitions))))))

(defun hover-symbol (symbol-string package-name)
  (remote-eval-sync *server*
                    `(micros/lsp-api:hover-symbol ,symbol-string)
                    package-name))

(defun hover-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (hover-symbol symbol-string package-name)))

(define-request (hover-request "textDocument/hover") (params lsp:hover-params)
  (let* ((point (text-document-position-params-to-point params))
         (text (or (hover-at-point point) "")))
    (convert-to-json (make-instance 'lsp:hover
                                    :contents text
                                    :range (symbol-points-to-lsp-range point)))))

(define-request (document-highlight-request "textDocument/documentHighlight")
    (params lsp:document-highlight-params)
  (let ((point (text-document-position-params-to-point params)))
    (unless (lem:in-string-or-comment-p point)
      (let ((symbol-string (lem:symbol-string-at-point point))
            (buffer (lem:point-buffer point)))
        (when symbol-string
          (lem:with-point ((point (lem:buffer-point buffer)))
            (lem:buffer-start point)
            (convert-to-json
             (coerce (loop :while (lem:search-forward-symbol point symbol-string)
                           :unless (lem:in-string-or-comment-p point)
                           :collect (lem:with-point ((start point))
                                      (lem:search-backward-symbol start symbol-string)
                                      (make-instance 'lsp:document-highlight
                                                     :kind lsp:document-highlight-kind-text
                                                     :range (points-to-lsp-range start point))))
                     'vector))))))))

(defun autodoc (point)
  (let* ((raw-form (lem-lisp-syntax:parse-for-autodoc point))
         (result (remote-eval-sync *server*
                                   `(micros::autodoc-function ',raw-form)
                                   (scan-current-package point))))
    (destructuring-bind (doc function-name) result
      (unless (eq doc :not-available)
        (values doc function-name)))))

(defun make-markdown-documentation (documentation)
  (make-instance 'lsp:markup-content
                 :kind lsp:markup-kind-markdown
                 :value documentation))

(defun symbol-matcher (string)
  (lambda (x)
    (and (symbolp x)
         (string-equal string x))))

(defun signature-help-at-point (point)
  (multiple-value-bind (doc function-name) (autodoc point)
    (when doc
      (let* ((form (read-from-string doc))
             (start (position-if (symbol-matcher "===>") form))
             (end (position-if (symbol-matcher "<===") form))
             (documentation (make-markdown-documentation
                             (hover-symbol function-name
                                           (buffer-package (lem:point-buffer point)))))
             (*print-case* :downcase))
        (make-instance
         'lsp:signature-help
         :signatures (vector
                      (if (or (null start) (null end))
                          (apply #'make-instance
                                 'lsp:signature-information
                                 :label (prin1-to-string form)
                                 (when documentation
                                   (list :documentation documentation)))
                          (let* ((form (append (subseq form 0 start)
                                               (list (elt form (1+ start)))
                                               (subseq form (1+ end)))))
                            (apply #'make-instance
                                   'lsp:signature-information
                                   :label (prin1-to-string form)
                                   :parameters (map 'vector
                                                    (lambda (parameter)
                                                      (make-instance 'lsp:parameter-information
                                                                     :label (prin1-to-string parameter)))
                                                    (rest form))
                                   :active-parameter (1- start)
                                   (when documentation
                                     (list :documentation documentation)))))))))))

(define-request (signature-help-request "textDocument/signatureHelp")
    (params lsp:signature-help-params)
  (let ((point (text-document-position-params-to-point params)))
    (if-let (signature-help (signature-help-at-point point))
      (convert-to-json signature-help)
      :null)))

(defun make-text-edit (point string)
  (let ((range (symbol-points-to-lsp-range point)))
    (make-instance 'lsp:text-edit
                   :new-text string
                   :range range)))

(define-request (completion-request "textDocument/completion") (params lsp:completion-params)
  (let* ((point (text-document-position-params-to-point params))
         (symbol-string (lem:symbol-string-at-point point))
         (package-name (scan-current-package point)))
    (if (null symbol-string)
        :null
        (convert-to-json
         (map 'vector
              (lambda (completed-item)
                (destructuring-bind (label classification signature documentation sort-text)
                    completed-item
                  (declare (ignore classification))
                  (make-instance 'lsp:completion-item
                                 :label label
                                 :label-details (make-instance 'lsp:completion-item-label-details
                                                               ;; :description "" ; TODO: set value
                                                               :detail (if signature
                                                                           (format nil " ~A" signature)
                                                                           ""))
                                 :detail (ppcre:regex-replace-all "\\s+" (or signature "") " ")
                                 :text-edit (make-text-edit point label)
                                 :documentation (make-instance 'lsp:markup-content
                                                               :kind lsp:markup-kind-markdown
                                                               :value documentation)
                                 :sort-text sort-text)))
              (remote-eval-sync *server*
                                `(micros/lsp-api:completions ,symbol-string
                                                             ,package-name)
                                package-name))))))

(define-request (completion-item-resolve-request "completionItem/resolve")
    (params lsp:completion-item)
  (convert-to-json params))

(defstruct symbol-definition
  symbol-spec
  range
  line-string)

(defun collect-definition-symbols-in-buffer (buffer default-package-name)
  (lem:with-point ((point (lem:buffer-point buffer)))
    (lem:buffer-start point)
    (loop :while (lem:search-forward-regexp point "^\\s*\\(def")
          :when (cond ((lem:in-comment-p point)
                       (lem:maybe-beginning-of-comment point)
                       (lem:skip-space-and-comment-forward point)
                       nil)
                      ((lem:in-string-p point)
                       (lem:maybe-beginning-of-string point)
                       (lem:form-offset point 1)
                       nil)
                      (t
                       (when (and (lem:form-offset point 1)
                                  (lem:skip-space-and-comment-forward point)
                                  (or (lem:syntax-symbol-char-p (lem:character-at point))
                                      (forward-down-list point)))
                         (let ((symbol-string (lem:symbol-string-at-point point)))
                           (multiple-value-bind (symbol-name package-name internalp)
                               (micros::tokenize-symbol-thoroughly symbol-string)
                             (declare (ignore internalp))
                             (when symbol-name
                               (cond ((equal package-name "") nil) ; keyword
                                     ((equal package-name "#") nil) ; uninternal symbol
                                     (t
                                      (lem:with-point ((start point)
                                                       (end point))
                                        (lem:skip-symbol-forward start)
                                        (lem:skip-symbol-forward end)
                                        (let ((package (or package-name default-package-name)))
                                          (make-symbol-definition
                                           :symbol-spec (micros/lsp-api:make-symbol-spec
                                                         :name symbol-name
                                                         :package package)
                                           :range (points-to-lsp-range start end)
                                           :line-string (lem:line-string point))))))))))))
          :collect :it)))

(defun document-symbol (buffer)
  ;; in-packageがファイル先頭に一つだけあることを前提にしている
  (let* ((package-name (buffer-package buffer))
         (symbol-definitions (collect-definition-symbols-in-buffer buffer package-name)))
    (convert-to-json
     (map 'vector
          (lambda (symbol-information symbol-definition)
            (let ((range (symbol-definition-range symbol-definition))
                  (line-string (symbol-definition-line-string symbol-definition))
                  (name (micros/lsp-api::symbol-information-name symbol-information))
                  ;; (detail (micros/lsp-api::symbol-information-detail symbol-information))
                  (kind (micros/lsp-api::symbol-information-kind symbol-information)))
              ;; symbolに複数の束縛があることを考慮していない(variableとfunctionなど)
              (make-instance 'lsp:document-symbol
                             :name name
                             :detail line-string ;(or detail "")
                             :kind (case kind
                                     (:variable
                                      lsp:symbol-kind-variable)
                                     (:function
                                      lsp:symbol-kind-function)
                                     (:class
                                      lsp:symbol-kind-class)
                                     (:package
                                      lsp:symbol-kind-package)
                                     (otherwise
                                      lsp:symbol-kind-function))
                             :range range
                             :selection-range range)))
          (remote-eval-sync *server*
                            `(micros/lsp-api:symbol-informations
                              ',(mapcar #'symbol-definition-symbol-spec symbol-definitions))
                            package-name)
          symbol-definitions))))

(define-request (document-symbol-request "textDocument/documentSymbol")
    (params lsp:document-symbol-params)
  (let* ((text-document-identifier (lsp:document-symbol-params-text-document params))
         (text-document (find-text-document text-document-identifier)))
    (document-symbol (text-document-buffer text-document))))

(defun make-indenting-text-edit (new-text line-number start-character end-character)
  (make-instance 'lsp:text-edit
                 :new-text new-text
                 :range (make-instance 'lsp:range
                                       :start (make-instance 'lsp:position
                                                             :line line-number
                                                             :character start-character)
                                       :end (make-instance 'lsp:position
                                                           :line line-number
                                                           :character end-character))))

(defun insert-indent-space (point old-column new-column options)
  (check-type options lsp:formatting-options)
  (lem:line-start point)
  (lem:delete-character point old-column)
  (let ((line-number (point-lsp-line-number point)))
    (cond ((lsp:formatting-options-insert-spaces options)
           (lem:insert-character point #\space new-column)
           (unless (= old-column new-column)
             (make-indenting-text-edit (make-string new-column :initial-element #\space)
                                       line-number
                                       0
                                       old-column)))
          (t
           (let ((tab-size (lsp:formatting-options-tab-size options)))
             (multiple-value-bind (num-tabs num-spaces) (floor new-column tab-size)
               (lem:insert-character point #\tab num-tabs)
               (lem:insert-character point #\space num-spaces)
               (make-indenting-text-edit
                (concatenate 'string
                             (make-string num-tabs :initial-element #\tab)
                             (make-string num-spaces :initial-element #\space))
                line-number
                0
                old-column)))))))

(defun delete-trailing-line (point)
  (lem:with-point ((start point :left-inserting)
                   (end point :left-inserting))
    (lem:skip-whitespace-backward (lem:line-end start))
    (lem:line-end end)
    (unless (lem:point= start end)
      (let ((range (points-to-lsp-range start end)))
        (lem:delete-between-points start end)
        (make-instance 'lsp:text-edit
                       :new-text ""
                       :range range)))))

(defun edit-indent-line (point old-column new-column options)
  (let ((text-edits '()))
    (when-let (text-edit (insert-indent-space point old-column new-column options))
      (push text-edit text-edits))
    (when (handler-case (lsp:formatting-options-trim-trailing-whitespace options)
            (unbound-slot () nil))
      (when-let ((text-edit (delete-trailing-line point)))
        (push text-edit text-edits)))
    text-edits))

(defun indent-line (point options)
  (check-type options lsp:formatting-options)
  (unless (lem:blank-line-p point)
    (let ((old-column (lem:point-charpos (lem:back-to-indentation point)))
          (new-column (lem-lisp-syntax:calc-indent (lem:copy-point point :temporary))))
      (when new-column
        (edit-indent-line point old-column new-column options)))))

(defun indent-lines (start end options)
  (let ((text-edits '()))
    (lem:apply-region-lines start
                            end
                            (lambda (point)
                              (setf text-edits
                                    (nconc (indent-line point options)
                                           text-edits))))
    text-edits))

(defun trim-and-insert-final-newlines-edit (buffer options)
  (let ((text-edits '()))
    (lem:with-point ((start (lem:buffer-point buffer))
                     (end (lem:buffer-point buffer)))
      (when (handler-case (lsp:formatting-options-trim-final-newlines options)
              (unbound-slot () nil))
        (lem:buffer-end start)
        (lem:buffer-end end)
        (lem:skip-whitespace-backward start)
        (let ((text-edit (make-instance 'lsp:text-edit
                                        :new-text ""
                                        :range (points-to-lsp-range start end))))
          (lem:delete-between-points start end)
          (push text-edit text-edits)))
      (when (handler-case (lsp:formatting-options-insert-final-newline options)
              (unbound-slot () nil))
        (lem:buffer-end start)
        (lem:buffer-end end)
        (lem:skip-whitespace-backward start)
        (when (and (lem:point= start end)
                   ;; is buffer empty?
                   (not (lem:start-buffer-p start)))
          (let ((text-edit (make-instance 'lsp:text-edit
                                          :new-text (string #\newline)
                                          :range (points-to-lsp-range start end))))
            (lem:delete-between-points start end)
            (push text-edit text-edits)))))
    text-edits))

(defun call-with-indent-text-document (text-document function)
  (let* ((buffer (text-document-buffer text-document))
         (old-buffer-text (lem:buffer-text buffer)))
    (lem:buffer-enable-undo buffer)
    (let ((text-edits (funcall function buffer)))
      (lem:buffer-undo (lem:buffer-point buffer))
      (lem:buffer-disable-undo buffer)
      (assert (string= old-buffer-text (lem:buffer-text buffer)))
      (coerce (nreverse text-edits) 'vector))))

(defun indent-text-document (text-document options)
  (call-with-indent-text-document
   text-document
   (lambda (buffer)
     (let ((text-edits
             (indent-lines (lem:buffer-start-point buffer)
                           (lem:buffer-end-point buffer)
                           options)))
       (nconc (trim-and-insert-final-newlines-edit buffer options)
              text-edits)
       text-edits))))

(define-request (document-formatting-request "textDocument/formatting")
    (params lsp:document-formatting-params)
  (let ((options (lsp:document-formatting-params-options params))
        (text-document-identifier (lsp:document-formatting-params-text-document params)))
    (if-let (text-document (find-text-document text-document-identifier))
      (convert-to-json (indent-text-document text-document options))
      :null)))

(defun indent-range-text-document (text-document range options)
  (call-with-indent-text-document
   text-document
   (lambda (buffer)
     (lem:with-point ((start (lem:buffer-point buffer))
                      (end (lem:buffer-point buffer)))
       (move-to-lsp-position start (lsp:range-start range))
       (move-to-lsp-position end (lsp:range-end range))
       (indent-lines start end options)))))

(define-request (document-range-formatting-request "textDocument/rangeFormatting")
    (params lsp:document-range-formatting-params)
  (let ((options (lsp:document-range-formatting-params-options params))
        (range (lsp:document-range-formatting-params-range params))
        (text-document-identifier (lsp:document-range-formatting-params-text-document params)))
    (if-let (text-document (find-text-document text-document-identifier))
      (convert-to-json (indent-range-text-document text-document range options))
      :null)))

(defun on-type-formatting (text-document position trigger-character options)
  (declare (ignore trigger-character))
  (call-with-indent-text-document
   text-document
   (lambda (buffer)
     (lem:with-point ((point (lem:buffer-point buffer)))
       (move-to-lsp-position point position)
       (indent-line point options)))))

(define-request (on-type-formatting-request "textDocument/onTypeFormatting")
    (params lsp:document-on-type-formatting-params)
  (let ((options (lsp:document-on-type-formatting-params-options params))
        (text-document-identifier (lsp:document-on-type-formatting-params-text-document params))
        (position (lsp:document-on-type-formatting-params-position params))
        (trigger-character (lsp:document-on-type-formatting-params-ch params)))
    (if-let (text-document (find-text-document text-document-identifier))
      (on-type-formatting text-document position trigger-character options)
      :null)))
