(in-package :lem-process)

(defclass %process-stream ()
  ((process
    :initform (error ":process missing")
    :initarg :process
    :reader stream-process)))


;;; input
(defclass process-input-stream (%process-stream trivial-gray-streams:fundamental-character-input-stream)
  ((unread-char
    :initform nil
    :accessor stream-unread-character)
   (pool-string
    :initform ""
    :accessor stream-pool-string)
   (string-position
    :initform 0
    :accessor stream-string-position)))

(defmethod trivial-gray-streams::close ((stream process-input-stream) &key abort)
  (declare (ignore abort)))

(defun stream-process-buffer-stream (stream)
  (process-buffer-stream (stream-process stream)))

(defun end-of-string-p (stream)
  (<= (length (stream-pool-string stream))
      (stream-string-position stream)))

(defun update-pool-string-if-required (stream)
  (when (end-of-string-p stream)
    (setf (stream-pool-string stream)
          (get-output-stream-string (stream-process-buffer-stream stream)))
    (setf (stream-string-position stream) 0)))

(defun stream-next-char (stream &optional spend)
  (alexandria:when-let (character (stream-unread-character stream))
    (when spend (setf (stream-unread-character stream) nil))
    (return-from stream-next-char character))
  (update-pool-string-if-required stream)
  (if (end-of-string-p stream)
      :eof
      (prog1 (char (stream-pool-string stream)
                   (stream-string-position stream))
        (when spend
          (incf (stream-string-position stream))))))

(defmethod trivial-gray-streams:stream-read-char ((stream process-input-stream))
  (stream-next-char stream t))

(defmethod trivial-gray-streams:stream-unread-char ((stream process-input-stream) character)
  (setf (stream-unread-character stream) character)
  nil)

#+(or)
(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream process-input-stream))
  )

(defmethod trivial-gray-streams:stream-peek-char ((stream process-input-stream))
  (stream-next-char stream nil))

(defmethod trivial-gray-streams:stream-listen ((stream process-input-stream))
  (update-pool-string-if-required stream)
  (not (end-of-string-p stream)))

#+(or)
(defmethod trivial-gray-streams:stream-read-line ((stream process-input-stream))
  )

(defmethod trivial-gray-streams:stream-clear-input ((stream process-input-stream))
  nil)


;;; output
(defclass process-output-stream (%process-stream trivial-gray-streams:fundamental-character-output-stream)
  ((column
    :initform 0
    :accessor stream-column)))

(defmethod trivial-gray-streams:stream-write-char ((stream process-output-stream) character)
  (process-send-input (stream-process stream) (string character))
  (if (char= character #\newline)
      (setf (stream-column stream) 0)
      (setf (stream-column stream)
            (char-width character (stream-column stream)))))

(defmethod trivial-gray-streams:stream-line-column ((stream process-output-stream))
  (stream-column stream))

(defmethod trivial-gray-streams:stream-start-line-p ((stream process-output-stream))
  (zerop (stream-column stream)))

#+(or)
(defmethod trivial-gray-streams:stream-write-string ((stream process-output-stream) string &optional start end)
  )

#+(or)
(defmethod trivial-gray-streams:stream-terpri ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-fresh-line ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-finish-output ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-force-output ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-clear-output ((stream process-output-stream))
  )

#+(or)
(defmethod trivial-gray-streams:stream-advance-to-column ((stream process-output-stream) column)
  )


(defclass process-io-stream (process-input-stream process-output-stream)
  ())

(defun make-process-stream (process)
  (make-instance 'process-io-stream :process process))
