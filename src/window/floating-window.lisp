(in-package :lem-core)

(defclass floating-window (window)
  ((border
    :initarg :border
    :initform 0
    :reader floating-window-border)
   (border-shape
    :type (member nil :drop-curtain)
    :initarg :border-shape
    :initform nil
    :reader floating-window-border-shape)
   (background-color
    :initarg :background-color
    :initform nil
    :reader floating-window-background-color)
   (focusable
    :initform nil
    :accessor floating-window-focusable-p)))

(defmethod initialize-instance :before ((floating-window floating-window) &rest initargs)
  (declare (ignore initargs))
  (unless (support-floating-window (implementation))
    (error "floating window is not supported"))
  (notify-floating-window-modified (current-frame)))

(defmethod initialize-instance :after ((floating-window floating-window)
                                       &key (frame (current-frame)) &allow-other-keys)
  (add-floating-window frame floating-window))

(defmethod initialize-instance ((floating-window floating-window)
                                &rest initargs
                                &key use-border
                                &allow-other-keys)
  (apply #'call-next-method
         floating-window
         (if use-border
             (list* :border 1 initargs)
             initargs)))

(defun make-floating-window (&key (buffer (alexandria:required-argument :buffer))
                                  (x (alexandria:required-argument :x))
                                  (y (alexandria:required-argument :y))
                                  (width (alexandria:required-argument :width))
                                  (height (alexandria:required-argument :height))
                                  (use-modeline-p nil)
                                  (background-color nil)
                                  (use-border nil))
  (make-instance 'floating-window
                 :buffer buffer
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline-p use-modeline-p
                 :background-color background-color
                 :border (if use-border 1 0)))

(defmethod %delete-window ((window floating-window))
  (when (eq window (current-window))
    (editor-error "Can not delete this window"))
  (notify-floating-window-modified (current-frame))
  (remove-floating-windows (current-frame) window))

(defun floating-window-p (window)
  (typep window 'floating-window))
