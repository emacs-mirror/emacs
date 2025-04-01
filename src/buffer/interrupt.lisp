(defpackage :lem/buffer/interrupt
  (:use :cl)
  (:export :without-interrupts
           :interrupt))
(in-package :lem/buffer/interrupt)

(defvar *interrupts-enabled* t)
(defvar *interrupted* nil)

(defmacro %without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts
    #+ccl ccl:without-interrupts
    #-(or sbcl ccl) progn
    ,@body))

(defmacro without-interrupts (&body body)
  (let ((prev-enabled (gensym)))
    `(let ((,prev-enabled *interrupts-enabled*)
           (*interrupts-enabled* nil))
       (prog1 (progn ,@body)
         (when (and *interrupted* ,prev-enabled)
           (%without-interrupts
             (setf *interrupted* nil)
             (error 'lem/buffer/errors:editor-interrupt)))))))

;; 別のスレッドから(bt2:interrupt-thread thread #'interrupt)で使う関数
(defun interrupt (&optional force)
  (cond
    (force
     (error 'lem/buffer/errors:editor-interrupt))
    (*interrupts-enabled*
     (error 'lem/buffer/errors:editor-interrupt))
    (t
     (setf *interrupted* t))))
