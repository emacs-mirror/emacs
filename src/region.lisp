(in-package :lem-core)

(defgeneric check-marked-using-global-mode (global-mode buffer))
(defgeneric region-beginning-using-global-mode (global-mode &optional buffer))
(defgeneric region-end-using-global-mode (global-mode &optional buffer))
(defgeneric set-region-point-using-global-mode (global-mode start end))

(defmethod region-beginning-using-global-mode ((global-mode emacs-mode)
                                         &optional (buffer (current-buffer)))
  (region-beginning buffer))

(defmethod region-end-using-global-mode ((global-mode emacs-mode)
                                   &optional (buffer (current-buffer)))
  (region-end buffer))

(defmethod set-region-point-using-global-mode ((global-mode emacs-mode) (start point) (end point))
  (declare (ignore global-mode))
  (cond
    ((buffer-mark-p (current-buffer))
     (move-point start (cursor-region-beginning (current-point)))
     (move-point end (cursor-region-end (current-point))))
    (t
     (line-start start)
     (line-end end))))

(defmethod check-marked-using-global-mode ((global-mode emacs-mode) buffer)
  (unless (buffer-mark buffer)
    (editor-error "Not mark in this buffer")))

(defun check-marked ()
  (check-marked-using-global-mode (current-global-mode) (current-buffer)))
