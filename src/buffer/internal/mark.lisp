(in-package :lem/buffer/internal)

(defclass mark ()
  ((point :initform nil
          :type (or null point)
          :accessor mark-point)
   (active :initform nil
           :type boolean
           :accessor mark-active-p)))

(defmethod mark-cancel ((mark mark))
  (setf (mark-active-p mark) nil))

(defmethod mark-set-point ((mark mark) point)
  (setf (mark-active-p mark) t)
  (if (mark-point mark)
      (move-point (mark-point mark) point)
      (setf (mark-point mark)
            (copy-point point :right-inserting)))
  point)
