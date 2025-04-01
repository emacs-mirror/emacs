(defpackage :lem-lsp-mode/lem-stdio-transport
  (:use :cl
        :jsonrpc/transport/interface)
  (:import-from :jsonrpc/connection
                :connection)
  (:import-from :jsonrpc/request-response
                :parse-message)
  (:import-from :lem-lsp-mode/async-process-stream
                :make-input-stream)
  (:export :lem-stdio-transport))
(in-package :lem-lsp-mode/lem-stdio-transport)

(defclass lem-stdio-transport (transport)
  ((process :initarg :process
            :reader lem-stdio-transport-process)
   (stream :initarg :stream
           :initform nil
           :accessor lem-stdio-transport-stream)))

(defmethod initialize-instance ((instance lem-stdio-transport) &rest initargs)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (unless (lem-stdio-transport-stream instance)
      (setf (lem-stdio-transport-stream instance)
            (make-input-stream (lem-stdio-transport-process instance))))
    instance))

(defmethod start-client ((transport lem-stdio-transport))
  (let ((connection (make-instance 'connection
                                   :request-callback (transport-message-callback transport))))
    (setf (transport-connection transport) connection)
    (setf (transport-threads transport)
          (list
           (bt2:make-thread
            (lambda ()
              (run-processing-loop transport connection))
            :name "lem-lsp-mode/lem-stdio-transport processing")
           (bt2:make-thread
            (lambda ()
              (run-reading-loop transport connection))
            :name "lem-lsp-mode/lem-stdio-transport reading")))
    connection))

(defmethod send-message-using-transport ((transport lem-stdio-transport) connection message)
  (let* ((json (with-output-to-string (s)
                 (yason:encode message s)))
         (body (format nil
                       "Content-Length: ~A~C~C~:*~:*~C~C~A"
                       (babel:string-size-in-octets json)
                       #\Return
                       #\Newline
                       json)))
    (async-process:process-send-input
     (lem-stdio-transport-process transport)
     body)))

(defmethod receive-message-using-transport ((transport lem-stdio-transport) connection)
  (let* ((stream (lem-stdio-transport-stream transport))
         (headers (handler-case (read-headers stream)
                    (error ()
                      ;; プロセスを終了したときにread-headersでエラーが出るのでここでハンドリングする
                      (return-from receive-message-using-transport nil))))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (jsonrpc:parse-message stream))))

(defun read-headers (stream)
  ;; copied from jsonrpc/transport/stdio::read-headers
  (let ((headers (make-hash-table :test 'equal)))
    (loop for line = (read-line stream)
          until (equal (string-trim '(#\Return #\Newline) line) "")
          do (let* ((colon-pos (position #\: line))
                    (field (string-downcase (subseq line 0 colon-pos)))
                    (value (string-trim '(#\Return #\Space #\Tab) (subseq line (1+ colon-pos)))))
               (setf (gethash field headers) value)))
    headers))
