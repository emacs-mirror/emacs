(defpackage :lem/common/killring
  (:use :cl :lem/common/ring)
  (:export :make-killring
           :make-nop-killring
           :copy-killring
           :push-killring-item
           :peek-killring-item
           :rotate-killring
           :rotate-killring-undo
           :with-killring-context)
  #+sbcl
  (:lock t))
(in-package :lem/common/killring)

(defvar *appending* nil)
(defvar *before-inserting* nil)
(defvar *options* nil)

(defstruct item string options)

(defun append-item (item1 item2)
  (make-item :string (concatenate 'string (item-string item1) (item-string item2))
             :options (append (item-options item1) (item-options item2))))

(defgeneric push-killring-item (killring string &key))
(defgeneric peek-killring-item (killring n))
(defgeneric rotate-killring (killring))
(defgeneric rotate-killring-undo (killring))

(defclass killring ()
  ((ring :initarg :ring :reader killring-ring)
   (offset :initform 0
           :accessor killring-offset)))

(defun make-killring (size)
  (make-instance 'killring :ring (make-ring size)))

(defun copy-killring (killring)
  (make-instance 'killring :ring (copy-ring (killring-ring killring))))

(defmethod push-killring-item ((killring killring)
                               string
                               &key (options *options*))
  (setf (killring-offset killring) 0)
  (let ((item (make-item :string string :options (alexandria:ensure-list options)))
        (ring (killring-ring killring)))
    (cond ((not *appending*)
           (ring-push ring item))
          ((ring-empty-p ring)
           (ring-push ring item))
          (t
           (let ((existing-item (ring-ref ring 0)))
             (setf (ring-ref ring 0)
                   (if *before-inserting*
                       (append-item item existing-item)
                       (append-item existing-item item)))))))
  killring)

(defmethod peek-killring-item ((killring killring) n)
  (unless (ring-empty-p (killring-ring killring))
    (let ((item (ring-ref (killring-ring killring)
                          (mod (+ n (killring-offset killring))
                               (ring-length (killring-ring killring))))))
      (values (item-string item)
              (item-options item)))))

(defmethod rotate-killring ((killring killring))
  (unless (ring-empty-p (killring-ring killring))
    (incf (killring-offset killring))))

(defmethod rotate-killring-undo ((killring killring))
  (unless (ring-empty-p (killring-ring killring))
    (decf (killring-offset killring))))

(defun call-with-killring-context (function *appending* *before-inserting* *options*)
  (funcall function))

(defmacro with-killring-context ((&key appending
                                       (before-inserting '*before-inserting*)
                                       (options '*options*))
                                 &body body)
  `(call-with-killring-context (lambda () ,@body)
                               ,appending
                               ,before-inserting
                               ,options))

(defclass nop-killring () ())

(defmethod push-killring-item ((killring nop-killring) string &key &allow-other-keys)
  killring)

(defmethod peek-killring-item ((killring nop-killring) n)
  nil)

(defmethod rotate-killring ((killring nop-killring))
  nil)

(defmethod rotate-killring-undo ((killring nop-killring))
  nil)

(defun make-nop-killring ()
  (make-instance 'nop-killring))
