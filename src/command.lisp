(in-package :lem-core)

(defvar *pre-command-hook* '()
  "Normal hook run before each command is executed.")
(defvar *post-command-hook* '()
  "Normal hook run after each command is executed.")

(defvar *this-command*
  "The command now being executed.")

(defvar *this-command-keys* '()
  "List containing the key sequence that invoked the present command.")

(defvar *universal-argument* nil
  "The raw prefix argument for the current command.")

(defun this-command ()
  "Return the command now being executed."
  *this-command*)

(defun this-command-keys ()
  "Return the list of key sequence that invoked the present command."
  (reverse *this-command-keys*))

(defun universal-argument-of-this-command ()
  "Return the universal argument of this command now being executed."
  *universal-argument*)

(defgeneric execute (mode command argument))

(defun call-command (this-command universal-argument)
  "Call first argument as the command, passing remaining arguments to it."
  (let ((*this-command* (ensure-command this-command))
        (*universal-argument* universal-argument))
    (unless *this-command*
      (editor-error "~A: command not found" this-command))
    (run-hooks *pre-command-hook*)
    (flet ((post-command ()
             (buffer-undo-boundary)
             (run-hooks *post-command-hook*)))
      (prog1 (handler-bind ((editor-condition (lambda (e)
                                                (declare (ignore e))
                                                (post-command))))
               (execute (get-active-modes-class-instance (current-buffer))
                        *this-command*
                        universal-argument))
        (post-command)))))
