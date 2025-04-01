(defpackage :lem/popup-message
  (:use :cl :lem)
  #+sbcl
  (:lock t))
(in-package :lem/popup-message)

(defclass popup-messenger () ())

(setf lem-core/popup-message-interface:*popup-messenger* (make-instance 'popup-messenger))

(defun make-popup-buffer (text)
  (let ((buffer (make-buffer "*Popup Message*" :temporary t :enable-undo-p nil)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (erase-buffer buffer)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(defmethod lem-core/popup-message-interface:display-popup-message
    ((popup-messenger popup-messenger) buffer-or-string
     &key timeout
          destination-window
          source-window
          style)
  (let ((buffer (etypecase buffer-or-string
                  (string (make-popup-buffer buffer-or-string))
                  (buffer buffer-or-string))))
    (destructuring-bind (width height)
        (lem/popup-window::compute-buffer-size buffer)
      (delete-popup-message destination-window)
      (let ((window (lem/popup-window::make-popup-window
                     :source-window (or source-window (current-window))
                     :buffer buffer
                     :width width
                     :height height
                     ;; NOTE: Block mouse clicks on popup windows to prevent them from gaining focus.
                     ;; If a popup window gains focus, it becomes the only active window in the window-tree,
                     ;; which prevents Lem from being able to close it.
                     ;; Mouse scrolling remains enabled since popup windows are used for displaying text content.
                     :clickable nil
                     :style style)))
        (buffer-start (window-view-point window))
        (window-see window)
        (when timeout
          (check-type timeout number)
          (start-timer (make-timer (lambda ()
                                     (unless (deleted-window-p window)
                                       (delete-window window))))
                       (round (* timeout 1000))))
        window))))

(defmethod lem-core/popup-message-interface:delete-popup-message ((popup-messenger popup-messenger) popup-message)
  (when (and popup-message (not (deleted-window-p popup-message)))
    (delete-window popup-message)))

(defmethod lem:show-message (value &rest args &key timeout (style '(:gravity :follow-cursor)))
  (setf (frame-message-window (current-frame))
        (apply #'display-popup-message
               value
               :destination-window (frame-message-window (current-frame))
               :style style
               :timeout timeout
               args)))

(defmethod lem:clear-message ()
  (delete-popup-message (frame-message-window (current-frame)))
  (setf (frame-message-window (current-frame)) nil))
