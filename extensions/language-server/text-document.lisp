(in-package :lem-language-server)

(defun buffer-text-document (buffer)
  (lem:buffer-value buffer 'text-document))

(defun (setf buffer-text-document) (text-document buffer)
  (setf (lem:buffer-value buffer 'text-document) text-document))

(defvar *text-document-table* (make-hash-table :test 'equal))

(defclass text-document ()
  ((uri :initarg :uri :accessor text-document-uri)
   (language-id :initarg :language-id :accessor text-document-language-id)
   (version :initarg :version :accessor text-document-version)
   (buffer :initarg :buffer :accessor text-document-buffer)))

(defmethod print-object ((object text-document) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "uri:~S version:~A" (text-document-uri object) (text-document-version object))))

(defun register-text-document (&rest initargs &key uri language-id version buffer)
  (declare (ignore uri language-id version))
  (let ((text-document (apply #'make-instance 'text-document initargs)))
    (setf (buffer-text-document buffer) text-document)
    (setf (gethash (text-document-uri text-document) *text-document-table*)
          text-document)))

(defun find-text-document (text-document-identifier)
  (check-type text-document-identifier lsp:text-document-identifier)
  (gethash (lsp:text-document-identifier-uri text-document-identifier)
           *text-document-table*))

(defun close-text-document (text-document)
  (check-type text-document text-document)
  (lem:delete-buffer (text-document-buffer text-document))
  (remhash (text-document-uri text-document)
           *text-document-table*))

(defun edit-text-document (text-document content-change)
  (check-type text-document text-document)
  (check-type content-change lsp:text-document-content-change-event)
  (let* ((buffer (text-document-buffer text-document))
         (point (lem:buffer-point buffer))
         (text (gethash "text" content-change))
         (range (gethash "range" content-change)))
    (cond ((null range)
           (lem:erase-buffer buffer)
           (lem:insert-string point text))
          (t
           (let ((start-position (lsp:range-start range))
                 (end-position (lsp:range-end range)))
             (lem:with-point ((start point)
                              (end point))
               (move-to-lsp-position start start-position)
               (move-to-lsp-position end end-position)
               (lem:delete-between-points start end)
               (lem:insert-string start text)))))))

(defun text-document-position-params-to-point (params)
  (check-type params lsp:text-document-position-params)
  (let ((text-document-identifier (lsp:text-document-position-params-text-document params))
        (position (lsp:text-document-position-params-position params)))
    (let* ((text-document (find-text-document text-document-identifier))
           (buffer (text-document-buffer text-document)))
      (lem:with-point ((point (lem:buffer-point buffer)))
        (move-to-lsp-position point position)
        point))))

(defun buffer-uri (buffer)
  (let ((text-document (buffer-text-document buffer)))
    (cond (text-document
           (text-document-uri text-document))
          (t
           (assert (lem:buffer-temporary-p buffer))
           ;; Temporary buffer are not associated with text-document, so return URI based on filename
           (pathname-to-uri (lem:buffer-filename buffer))))))

(defun point-to-lsp-location (point)
  (let ((uri (buffer-uri (lem:point-buffer point))))
    (lem:with-point ((end point))
      (lem:form-offset end 1)
      (make-instance 'lsp:location
                     :uri uri
                     :range (points-to-lsp-range point end)))))
