(defpackage :lem/common/command
  (:use :cl)
  (:export :primary-command
           :command-name
           :command-source-location
           :command-equal
           :register-command-class
           :get-command
           :ensure-command
           :make-command-table
           :add-command
           :remove-command
           :all-command-names
           :all-commands
           :find-command
           :exist-command-p)
  #+sbcl
  (:lock t))
(in-package :lem/common/command)

(defclass primary-command ()
  ((name :initarg :name
         :reader command-name)
   (source-location :initarg :source-location
                    :initform nil
                    :reader command-source-location)))

(defun command-equal (command1 command2)
  (eq (command-name command1)
      (command-name command2)))

(defun register-command-class (symbol class-name)
  (setf (get symbol 'command-class) class-name))

(defun get-command-class (symbol)
  (get symbol 'command-class))

(defun get-command (symbol)
  (alexandria:when-let (class (get-command-class symbol))
    (make-instance class)))

(defun ensure-command (command)
  (check-type command (or primary-command symbol))
  (if (typep command 'primary-command)
      command
      (get-command command)))

;;
(defvar *command-table*)

(defstruct command-table
  (table (make-hash-table :test 'equal)))

(defun add-command (name command &optional (command-table *command-table*))
  (check-type name string)
  (check-type command primary-command)
  (setf (gethash name (command-table-table command-table)) command))

(defun remove-command (name &optional (command-table *command-table*))
  (remhash name (command-table-table command-table)))

(defun all-command-names (&optional (command-table *command-table*))
  (alexandria:hash-table-keys (command-table-table command-table)))

(defun all-commands (&optional (command-table *command-table*))
  (alexandria:hash-table-values (command-table-table command-table)))

(defun find-command (command-name &optional (command-table *command-table*))
  (gethash command-name (command-table-table command-table)))

(defun exist-command-p (command-name &optional (command-table *command-table*))
  (not (null (find-command command-name command-table))))

(unless (boundp '*command-table*)
  (setf *command-table* (make-command-table)))
