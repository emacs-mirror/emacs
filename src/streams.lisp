(in-package :lem-core)

(defclass buffer-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((unread-char
    :initform nil
    :accessor buffer-input-stream-unread-char)
   (point
    :initarg :point
    :accessor buffer-stream-point)
   (eof-p
    :initform nil
    :accessor buffer-input-stream-eof-p)))

(defun make-buffer-input-stream (&optional (point (current-point)))
  (make-instance 'buffer-input-stream
                 :point (copy-point point :left-inserting)))

(defmethod trivial-gray-streams::close ((stream buffer-input-stream) &key abort)
  (declare (ignore abort))
  (delete-point (buffer-stream-point stream))
  t)

;;; TODO: unread-charがあるときの処理が間違ってるかも
(defmethod trivial-gray-streams:stream-read-char ((stream buffer-input-stream))
  (let ((character (buffer-input-stream-unread-char stream)))
    (prog1 (cond (character
                  (setf (buffer-input-stream-unread-char stream) nil)
                  character)
                 ((buffer-input-stream-eof-p stream)
                  :eof)
                 (t
                  (character-at (buffer-stream-point stream))))
      (unless (character-offset (buffer-stream-point stream) 1)
        (setf (buffer-input-stream-eof-p stream) t)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream buffer-input-stream) character)
  (setf (buffer-input-stream-unread-char stream) character)
  (character-offset (buffer-stream-point stream) -1)
  nil)

#+nil
(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream buffer-input-stream))
  )

(defmethod trivial-gray-streams:stream-peek-char ((stream buffer-input-stream))
  (character-at (buffer-stream-point stream)))

(defmethod trivial-gray-streams:stream-listen ((stream buffer-input-stream))
  (end-buffer-p (buffer-stream-point stream)))

(defmethod trivial-gray-streams:stream-read-line ((stream buffer-input-stream))
  (let ((start (copy-point (buffer-stream-point stream) :temporary)))
    (let ((string (points-to-string
                   start
                   (line-end (buffer-stream-point stream)))))
      (values string
              (not (line-offset (buffer-stream-point stream) 1))))))

(defmethod trivial-gray-streams:stream-clear-input ((stream buffer-input-stream))
  nil)

(defclass buffer-output-stream (trivial-gray-streams:fundamental-output-stream)
  ((interactive-update-p
    :initarg :interactive-update-p
    :accessor buffer-output-stream-interactive-update-p)
   (point
    :initarg :point
    :accessor buffer-stream-point)))

(defun make-buffer-output-stream (&optional (point (current-point))
                                            interactive-update-p)
  (make-instance 'buffer-output-stream
                 :point (copy-point point :left-inserting)
                 :interactive-update-p interactive-update-p))

(defmethod trivial-gray-streams::close ((stream buffer-output-stream) &key abort)
  (declare (ignore abort))
  (delete-point (buffer-stream-point stream))
  t)

(defmethod stream-element-type ((stream buffer-output-stream))
  'character)

(defmethod trivial-gray-streams:stream-line-column ((stream buffer-output-stream))
  (point-charpos (buffer-stream-point stream)))

(defun buffer-output-stream-refresh (stream)
  (when (buffer-output-stream-interactive-update-p stream)
    (let ((buffer (point-buffer (buffer-stream-point stream)))
          (point (buffer-stream-point stream)))
      (pop-to-buffer buffer)
      (dolist (window (get-buffer-windows buffer))
        (move-point (buffer-point (window-buffer window)) point))
      (redraw-display)))
  nil)

(defmethod trivial-gray-streams:stream-fresh-line ((stream buffer-output-stream))
  (unless (zerop (point-charpos (buffer-stream-point stream)))
    (trivial-gray-streams:stream-terpri stream)))

(defmethod trivial-gray-streams:stream-write-byte ((stream buffer-output-stream) byte)
  (trivial-gray-streams:stream-write-char stream (code-char byte)))

(defmethod trivial-gray-streams:stream-write-char ((stream buffer-output-stream) char)
  (prog1 char
    (insert-character (buffer-stream-point stream) char 1)))

(defun %write-string-to-buffer-stream (stream string start end &key)
  (insert-string (buffer-stream-point stream)
                 (subseq string start end))
  string)

(defun %write-octets-to-buffer-stream (stream octets start end &key)
  (let ((octets (subseq octets start end)))
    (loop :for c :across octets
          :do (trivial-gray-streams:stream-write-byte stream c))
    octets))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream buffer-output-stream)
     sequence start end &key)
  (etypecase sequence
    (string
     (%write-string-to-buffer-stream stream sequence start end))
    ((array (unsigned-byte 8) (*))
     (%write-octets-to-buffer-stream stream sequence start end))))

(defmethod trivial-gray-streams:stream-write-string
    ((stream buffer-output-stream)
     (string string)
     &optional (start 0) end)
  (%write-string-to-buffer-stream stream string start end))

(defmethod trivial-gray-streams:stream-terpri ((stream buffer-output-stream))
  (insert-character (buffer-stream-point stream) #\newline 1)
  (buffer-output-stream-refresh stream)
  #\newline)

(defmethod trivial-gray-streams:stream-finish-output ((stream buffer-output-stream))
  (buffer-output-stream-refresh stream))

(defmethod trivial-gray-streams:stream-force-output ((stream buffer-output-stream))
  (buffer-output-stream-refresh stream))

#+nil
(defmethod trivial-gray-streams:clear-output ((stream buffer-output-stream))
  )


(defclass editor-input-stream (trivial-gray-streams:fundamental-input-stream)
  ((queue
    :initform nil
    :initarg :queue
    :accessor editor-input-stream-queue)))

(defun make-editor-input-stream ()
  (make-instance 'editor-input-stream :queue nil))

(defmethod trivial-gray-streams:stream-read-char ((stream editor-input-stream))
  (let ((c (pop (editor-input-stream-queue stream))))
    (cond ((null c)
           (let ((string
                   (handler-case (values (prompt-for-string "") t)
                     (editor-abort ()
                       (setf (editor-input-stream-queue stream) nil)
                       (return-from trivial-gray-streams:stream-read-char :eof)))))
             (setf (editor-input-stream-queue stream)
                   (nconc (editor-input-stream-queue stream)
                          (coerce string 'list)
                          (list #\newline))))
           (trivial-gray-streams:stream-read-char stream))
          ((eql c #\eot)
           :eof)
          (c))))

(defmethod trivial-gray-streams:stream-unread-char ((stream editor-input-stream) char)
  (push char (editor-input-stream-queue stream))
  nil)

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream editor-input-stream))
  (trivial-gray-streams:stream-read-char stream))

(defmethod trivial-gray-streams:stream-peek-char ((stream editor-input-stream))
  (let ((c (trivial-gray-streams:stream-read-char stream)))
    (prog1 c
      (trivial-gray-streams:stream-unread-char stream c))))

(defmethod trivial-gray-streams:stream-listen ((stream editor-input-stream))
  (let ((c (trivial-gray-streams:stream-read-char-no-hang stream)))
    (prog1 c
      (trivial-gray-streams:stream-unread-char stream c))))

(defmethod trivial-gray-streams:stream-read-line ((stream editor-input-stream))
  (prompt-for-string ""))

(defmethod trivial-gray-streams:stream-clear-input ((stream editor-input-stream))
  nil)


(defclass editor-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((pool
    :initform (make-string-output-stream))
   (column
    :initform 0)
   (destination
    :initform nil
    :initarg :destination)))

(defun make-editor-output-stream (&optional destination)
  (make-instance 'editor-output-stream
                 :destination destination))

(defun editor-output-stream-flush (stream)
  (with-slots (pool destination) stream
    (let ((string (get-output-stream-string pool)))
      (etypecase destination
        (point
         (insert-string destination string))
        (function
         (funcall destination string))
        (null
         (message "~A" string))))))

(defmethod trivial-gray-streams:stream-write-char ((stream editor-output-stream) character)
  (with-slots (destination pool column) stream
    (unless (eq destination nil)
      (when (char= character #\newline)
        (editor-output-stream-flush stream)))
    (write-char character pool)
    (setf column (char-width character column))
    character))

(defmethod trivial-gray-streams:stream-line-column ((stream editor-output-stream))
  (with-slots (column) stream
    column))

(defmethod trivial-gray-streams:stream-start-line-p ((stream editor-output-stream))
  (with-slots (column) stream
    (zerop column)))

;; (defmethod trivial-gray-streams:stream-write-string ((stream editor-output-stream) string &optional start end)
;;   )

;; (defmethod trivial-gray-streams:stream-terpri ((stream editor-output-stream))
;;   )

;; (defmethod trivial-gray-streams:stream-fresh-line ((stream editor-output-stream))
;;   )

(defmethod trivial-gray-streams:stream-finish-output ((stream editor-output-stream))
  (editor-output-stream-flush stream))

(defmethod trivial-gray-streams:stream-force-output ((stream editor-output-stream))
  (editor-output-stream-flush stream))

;; (defmethod trivial-gray-streams:stream-clear-output ((stream editor-output-stream))
;;   )

;; (defmethod trivial-gray-streams:stream-advance-to-column ((stream editor-output-stream) column)
;;   )


(defclass editor-io-stream (editor-input-stream editor-output-stream)
  ())

(defun make-editor-io-stream (&optional destination)
  (make-instance 'editor-io-stream
                 :destination destination))
