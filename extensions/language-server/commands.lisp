(in-package :lem-language-server)

(defvar *command-table* (make-hash-table :test 'equal))

(defun command-names ()
  (alexandria:hash-table-keys *command-table*))

(defun get-command (name)
  (gethash name *command-table*))

(defgeneric call-command (command arguments))

(defclass command () ())

(defun execute-command (name arguments)
  (let ((command (get-command name)))
    (assert command) ; TODO: error responseを返す
    (call-command command arguments)))

(defmacro define-lsp-command (class-name command-name (arguments) &body body)
  (let ((command (gensym)))
    `(progn
       (defclass ,class-name (command) ())
       (defmethod call-command ((,command ,class-name) ,arguments) ,@body)
       (setf (gethash ,command-name *command-table*)
             (make-instance ',class-name)))))
