(in-package :lem-core)

(defclass header-window (window) ())

(defmethod initialize-instance ((window header-window) &key &allow-other-keys)
  (with-slots (x y width height) window
    (setf x 0)
    (setf y (length (frame-header-windows (current-frame))))
    (setf width (display-width))
    (setf height 1))
  (add-header-window (current-frame) window)
  (notify-header-window-modified (current-frame))
  (call-next-method))

(defmethod %delete-window ((window header-window))
  (remove-header-window (current-frame) window)
  (notify-header-window-modified (current-frame)))
