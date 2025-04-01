(in-package :lem/buffer/internal)

(defparameter *undo-modes* '(:edit :undo :redo))
(defvar *undo-mode* :edit)

(defvar *inhibit-undo* nil)

(defun inhibit-undo-p ()
  *inhibit-undo*)

(defmacro with-inhibit-undo (() &body body)
  `(let ((*inhibit-undo* t))
     ,@body))

(defun buffer-enable-undo-p (&optional (buffer (current-buffer)))
  "Returns T if undo is enabled for `buffer`, otherwise returns NIL."
  (and (buffer-%enable-undo-p buffer) (not *inhibit-undo*)))

(defun buffer-enable-undo (buffer)
  "Enables undo for `buffer`."
  (setf (buffer-%enable-undo-p buffer) t)
  nil)

(defun buffer-disable-undo (buffer)
  "Disables undo for `buffer` and remove all undo information."
  (setf (buffer-%enable-undo-p buffer) nil)
  (setf (buffer-edit-history buffer) (make-array 0 :adjustable t :fill-pointer 0))
  (setf (buffer-redo-stack buffer) nil)
  nil)

(defun buffer-enable-undo-boundary-p (&optional (buffer (current-buffer)))
  (buffer-%enable-undo-boundary-p buffer))

(defun buffer-enable-undo-boundary (buffer)
  (setf (buffer-%enable-undo-boundary-p buffer) t)
  nil)

(defun buffer-disable-undo-boundary (buffer)
  (setf (buffer-%enable-undo-boundary-p buffer) nil)
  nil)

(defun buffer-undo-boundary (&optional (buffer (current-buffer)))
  (when (buffer-enable-undo-boundary-p)
    (unless (eq :separator (last-edit-history buffer))
      (vector-push-extend :separator (buffer-edit-history buffer)))))

(defun buffer-modify (buffer)
  (ecase *undo-mode*
    ((:edit :redo)
     (incf (buffer-%modified-p buffer)))
    ((:undo)
     (decf (buffer-%modified-p buffer))))
  (buffer-mark-cancel buffer))

(defun push-undo-stack (buffer elt)
  (vector-push-extend elt (buffer-edit-history buffer)))

(defun push-redo-stack (buffer elt)
  (push elt (buffer-redo-stack buffer)))

(defun push-undo (buffer edit)
  (when (buffer-enable-undo-p buffer)
    (ecase *undo-mode*
      (:edit
       (push-undo-stack buffer edit)
       (setf (buffer-redo-stack buffer) nil))
      (:redo
       (push-undo-stack buffer edit))
      (:undo
       (push-redo-stack buffer edit)))))

(defun buffer-undo-1 (point)
  (let* ((buffer (point-buffer point))
         (edit-history (buffer-edit-history buffer))
         (elt (and (< 0 (length edit-history)) (vector-pop edit-history))))
    (when elt
      (let ((*undo-mode* :undo))
        (unless (eq elt :separator)
          (apply-inverse-edit point elt))))))

(defun buffer-undo (point)
  (let ((buffer (point-buffer point)))
    (push :separator (buffer-redo-stack buffer))
    (when (eq :separator (last-edit-history buffer))
      (vector-pop (buffer-edit-history buffer)))
    (let ((result0 nil))
      (loop :for result := (buffer-undo-1 point)
            :while result
            :do (setf result0 result))
      (unless result0
        (assert (eq :separator (car (buffer-redo-stack buffer))))
        (pop (buffer-redo-stack buffer)))
      result0)))

(defun buffer-redo-1 (point)
  (let* ((buffer (point-buffer point))
         (elt (pop (buffer-redo-stack buffer))))
    (when elt
      (let ((*undo-mode* :redo))
        (unless (eq elt :separator)
          (apply-inverse-edit point elt))))))

(defun buffer-redo (point)
  (let ((buffer (point-buffer point)))
    (vector-push-extend :separator (buffer-edit-history buffer))
    (let ((result0 nil))
      (loop :for result := (buffer-redo-1 point)
            :while result
            :do (setf result0 result))
      (unless result0
        (assert (eq :separator
                    (last-edit-history buffer)))
        (vector-pop (buffer-edit-history buffer)))
      result0)))

(defun recompute-undo-position-offset (buffer edit)
  (loop :for edit-1 :across (buffer-edit-history buffer)
        :do (unless (eq edit-1 :separator)
              (compute-edit-offset edit-1 edit)))
  (loop :for edit-1 :in (buffer-redo-stack buffer)
        :do (unless (eq edit-1 :separator)
              (compute-edit-offset edit-1 edit))))
