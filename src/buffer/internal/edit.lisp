(in-package :lem/buffer/internal)

(deftype edit-kind ()
  '(member :insert-string :delete-string))

(defstruct (edit (:constructor make-edit (kind position string)))
  (kind (alexandria:required-argument :kind)
        :type edit-kind
        :read-only t)
  (position (alexandria:required-argument :position)
            :type (integer 1 *))
  (string (alexandria:required-argument :string)
          :type string
          :read-only t))

(defun apply-edit (point edit)
  (ecase (edit-kind edit)
    ((:insert-string)
     (move-to-position point (edit-position edit))
     (with-point ((p point))
       (insert-string/point point (edit-string edit))
       (move-point point p)))
    ((:delete-string)
     (move-to-position point (edit-position edit))
     (delete-char/point point (length (edit-string edit))))))

(defun apply-inverse-edit (point edit)
  (ecase (edit-kind edit)
    ((:insert-string)
     (apply-edit point
                 (make-edit :delete-string
                            (edit-position edit)
                            (edit-string edit))))
    ((:delete-string)
     (apply-edit point
                 (make-edit :insert-string
                            (edit-position edit)
                            (edit-string edit))))))

(defun compute-edit-offset (dest src)
  (ecase (edit-kind src)
    ((:insert-string)
     (when (<= (edit-position src) (edit-position dest))
       (incf (edit-position dest) (length (edit-string src)))))
    ((:delete-string)
     (when (< (edit-position src)
              (edit-position dest))
       (setf (edit-position dest)
             (max 1 (- (edit-position dest) (length (edit-string src)))))
       (when (< (edit-position dest) (edit-position src))
         (setf (edit-position dest) (edit-position src)))))))
