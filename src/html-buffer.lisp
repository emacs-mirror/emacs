(in-package :lem-core)

(defclass html-buffer (text-buffer)
  ((html :initarg :html
         :reader html-buffer-html)
   (updated :initform nil
            :reader html-buffer-updated-p)))

(defmethod (setf html-buffer-html) (html (buffer html-buffer))
  (setf (slot-value buffer 'updated) t)
  (setf (slot-value buffer 'html) html))

(defmethod invalidate-html-buffer-updated ((buffer html-buffer))
  (setf (slot-value buffer 'updated) nil))

(defun make-html-buffer (buffer-name html)
  (let ((buffer (make-buffer buffer-name)))
    (change-class buffer 'html-buffer :html html)
    buffer))

(defun js-eval (window code &key (wait nil))
  (lem-if:js-eval (implementation)
                  (window-view window)
                  code
                  :wait wait))
