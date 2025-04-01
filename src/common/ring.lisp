(defpackage :lem/common/ring
  (:use :cl)
  (:export :invalid-index-error
           :ring
           :make-ring
           :copy-ring
           :ring-empty-p
           :ring-length
           :ring-push
           :ring-ref)
  #+sbcl
  (:lock t))
(in-package :lem/common/ring)

(define-condition invalid-index-error (program-error)
  ((index :initarg :index)
   (size :initarg :size))
  (:report (lambda (c s)
             (with-slots (index size) c
               (format s "The index ~D is too large for a ring of size ~D." index size)))))

(defclass ring ()
  ((data :initarg :data
         :reader ring-data)
   (front :initform 0
          :accessor ring-front)
   (rear :initform 0
         :accessor ring-rear)
   (empty :initform t
          :writer set-ring-empty
          :reader ring-empty-p)))

(defmethod print-object ((object ring) stream)
  (print-unreadable-object (object stream :type t)
    (format stream
            "data: ~A front: ~D rear: ~D"
            (ring-data object)
            (ring-front object)
            (ring-rear object))))

(defun make-ring (size)
  (make-instance 'ring :data (make-array size)))

(defun copy-ring (ring)
  (let ((new-ring (make-instance 'ring :data (copy-seq (ring-data ring)))))
    (with-slots (front rear empty) new-ring
      (setf front (ring-front ring)
            rear (ring-rear ring)
            empty (ring-empty-p ring)))
    new-ring))

(defmethod ring-length ((ring ring))
  (cond ((ring-empty-p ring)
         0)
        ((< (ring-front ring) (ring-rear ring))
         (- (ring-rear ring) (ring-front ring)))
        (t
         (+ (- (ring-rear ring) (ring-front ring))
            (length (ring-data ring))))))

(defmethod ring-size ((ring ring))
  (length (ring-data ring)))

(defmethod ring-push ((ring ring) value)
  (setf (aref (ring-data ring) (ring-rear ring))
        value)
  (when (and (not (ring-empty-p ring))
             (= (ring-rear ring)
                (ring-front ring)))
    (setf (ring-front ring)
          (mod (1+ (ring-front ring))
               (ring-size ring))))
  (setf (ring-rear ring)
        (mod (1+ (ring-rear ring))
             (ring-size ring)))
  (set-ring-empty nil ring)
  ring)

(defmethod ring-ref ((ring ring) n)
  (unless (<= 0 n (1- (ring-length ring)))
    (error 'invalid-index-error :index n :size (ring-length ring)))
  (aref (ring-data ring)
        (mod (- (ring-rear ring) n 1)
             (ring-size ring))))

(defmethod (setf ring-ref) (value (ring ring) n)
  (unless (<= 0 n (1- (ring-length ring)))
    (error 'invalid-index-error :index n :size (ring-length ring)))
  (setf (aref (ring-data ring)
              (mod (- (ring-rear ring) n 1)
                   (ring-size ring)))
        value))
