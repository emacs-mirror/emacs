(defpackage :lem-vi-mode/ex-core
  (:use :cl)
  (:import-from :lem-vi-mode/visual
                :visual-p
                :visual-range)
  (:export :*point*
           :syntax-error
           :search-forward
           :search-backward
           :goto-line
           :current-line
           :last-line
           :marker
           :offset-line
           :goto-current-point
           :range
           :all-lines
           :call-ex-command
           :define-ex-command
           :*command*))
(in-package :lem-vi-mode/ex-core)

(defvar *point*)
(defvar *command-table* '())

(defun syntax-error ()
  (lem:editor-error "syntax error"))

(defun search-forward (pattern)
  (lem:search-forward-regexp *point* pattern))

(defun search-backward (pattern)
  (lem:search-backward-regexp *point* pattern))

(defun goto-line (line-number)
  (let* ((nlines (lem:buffer-nlines (lem:point-buffer *point*)))
         (line-number (cond ((< line-number 1) 1)
                            ((< nlines line-number) nlines)
                            (t line-number))))
    (lem:move-point (lem:current-point)
                    (lem:move-to-line (lem:copy-point *point* :temporary)
                                      line-number))))

(defun current-line ()
  *point*)

(defun last-line ()
  (lem:buffer-end *point*))

(defun marker (char)
  (ecase char
    (#\<
     (unless (visual-p)
       (lem:editor-error "Mark not set"))
     (first (visual-range)))
    (#\>
     (unless (visual-p)
       (lem:editor-error "Mark not set"))
     (second (visual-range)))))

(defun offset-line (offset)
  (declare (ignore offset)))

(defun goto-current-point (range)
  (declare (ignore range)))

(defun range (&rest range)
  range)

(defun all-lines ()
  (let ((buffer (lem:point-buffer *point*)))
    (list (lem:copy-point (lem:buffer-start-point buffer) :temporary)
          (lem:copy-point (lem:buffer-end-point buffer) :temporary))))

(defvar *command* nil)
(defun call-ex-command (range *command* argument)
  (let ((function (find-ex-command *command*)))
    (unless function
      (lem:editor-error "unknown command: ~A" *command*))
    (funcall function range argument)))

(defun find-ex-command (command)
  (loop :for (regex function) :in *command-table*
        :do (when (cl-ppcre:scan regex command)
              (return function))))

(defmacro define-ex-command (regex (range argument) &body body)
  `(push (list ,regex (lambda (,range ,argument) ,@body))
         *command-table*))
