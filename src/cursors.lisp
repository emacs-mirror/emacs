(in-package :lem-core)

(defclass cursor (point)
  ((saved-column :initform nil
                 :accessor cursor-saved-column)
   (yank-start :initform nil
               :accessor cursor-yank-start)
   (yank-end :initform nil
             :accessor cursor-yank-end)))

(defmethod make-buffer-point (point)
  (let ((cursor (make-instance 'cursor)))
    (copy-point-using-class cursor point :left-inserting)))

(defmethod change-yank-start (cursor point)
  (when (cursor-yank-start cursor)
    (delete-point (cursor-yank-start cursor)))
  (setf (cursor-yank-start cursor) point))

(defmethod change-yank-end (cursor point)
  (when (cursor-yank-end cursor)
    (delete-point (cursor-yank-end cursor)))
  (setf (cursor-yank-end cursor) point))

(defclass fake-cursor (cursor)
  ((killring :initarg :killring
             :reader fake-cursor-killring)
   (mark :initarg :mark
         :initform (make-instance 'mark)
         :reader fake-cursor-mark)))

(defmethod cursor-mark ((cursor cursor))
  (buffer-mark-object (point-buffer cursor)))

(defmethod cursor-mark ((cursor fake-cursor))
  (fake-cursor-mark cursor))

(defmethod set-cursor-mark ((cursor cursor) point)
  (set-current-mark point))

(defmethod set-cursor-mark ((cursor fake-cursor) point)
  (mark-set-point (fake-cursor-mark cursor) point))

(defmethod cursor-region-beginning ((cursor cursor))
  (point-min cursor (mark-point (cursor-mark cursor))))

(defmethod cursor-region-end ((cursor cursor))
  (point-max cursor (mark-point (cursor-mark cursor))))

(defmethod cursor-killring ((cursor cursor))
  (current-killring))

(defmethod cursor-killring ((cursor fake-cursor))
  (fake-cursor-killring cursor))

(defun buffer-fake-cursors (buffer)
  (buffer-value buffer 'fake-cursors))

(defun (setf buffer-fake-cursors) (value buffer)
  (setf (buffer-value buffer 'fake-cursors) value))

(defun make-fake-cursor (point)
  (let* ((killring (copy-killring (current-killring)))
         (fake-cursor (make-instance 'fake-cursor :killring killring)))
    (copy-point-using-class fake-cursor point :left-inserting)
    (push fake-cursor (buffer-fake-cursors (point-buffer point)))
    fake-cursor))

(defun delete-fake-cursor (point)
  (let ((buffer (point-buffer point)))
    (assert (not (eq point (buffer-point buffer))))
    ;; (assert (find point (buffer-fake-cursors buffer)))
    (alexandria:deletef (buffer-fake-cursors buffer) point)
    (delete-point point)
    (alexandria:when-let ((yank-start (cursor-yank-start point)))
      (delete-point yank-start))
    (alexandria:when-let ((yank-end (cursor-yank-end point)))
      (delete-point yank-end))
    (values)))

(defun clear-cursors (buffer)
  (mapc #'delete-fake-cursor (buffer-fake-cursors buffer))
  (values))

(defun buffer-cursors (buffer)
  (sort (copy-list (cons (buffer-point buffer)
                         (buffer-fake-cursors buffer)))
        #'point<))

(defun merge-cursor-killrings (buffer)
  (with-output-to-string (out)
    (loop :for (cursor . not-lastp) :on (buffer-cursors buffer)
          :do (let ((killring (cursor-killring cursor)))
                (multiple-value-bind (string options)
                    (peek-killring-item killring 0)
                  (declare (ignore options))
                  ;; TODO: consider options
                  (when string
                    (if not-lastp
                        (write-line string out)
                        (write-string string out))))))))
