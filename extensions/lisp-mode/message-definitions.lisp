(in-package :lem-lisp-mode/internal)

(define-message (:write-string string &optional target thread)
  (declare (ignore target))
  (write-string-to-repl string)
  (when thread
    (with-broadcast-connections (connection)
      (send-message connection `(:write-done ,thread)))))

(define-message (:write-object string id type)
  (write-object-to-repl string id type))

(define-message (:read-string thread tag)
  (repl-read-string thread tag))

(define-message (:read-aborted thread tag)
  (repl-abort-read thread tag))

(define-message (:new-package name prompt-string)
  (new-package name prompt-string))

(define-message (:return value id)
  (with-broadcast-connections (connection)
    (finish-evaluated connection value id)))

(define-message (:read-from-minibuffer thread tag prompt initial-value)
  (read-from-minibuffer thread tag prompt initial-value))

(define-message (:y-or-n-p thread tag question)
  (dispatch-message `(:emacs-return ,thread ,tag ,(prompt-for-y-or-n-p question))))

(define-message (:emacs-return-string thread tag string)
  (with-broadcast-connections (connection)
    (send-message-string
     connection
     (format nil "(:emacs-return-string ~A ~A ~S)"
             thread
             tag
             string))))

(define-message (:new-features features)
  (with-broadcast-connections (connection)
    (setf (connection-features connection)
          features)))

(define-message (:indentation-update info)
  (indentation-update info))

(define-message (:eval-no-wait form)
  (eval (read-from-string form)))

(define-message (:eval thread tag form-string)
  (let ((result (handler-case (eval (read-from-string form-string))
                  (error (c)
                    `(:error ,(type-of c) ,(princ-to-string c)))
                  (:no-error (&rest values)
                    `(:ok ,(first values))))))
    (dispatch-message `(:emacs-return ,thread ,tag ,result))))

(define-message (:emacs-return thread tag value)
  (with-broadcast-connections (connection)
    (send-message-string
     connection
     (format nil "(:emacs-return ~A ~A ~S)" thread tag value))))

(define-message (:debug-condition thread message)
  (assert thread)
  (display-message "~A" message))

(define-message (:ping thread tag)
  (with-broadcast-connections (connection)
    (send-message-string
     connection
     (format nil "(:emacs-pong ~A ~A)" thread tag))))
