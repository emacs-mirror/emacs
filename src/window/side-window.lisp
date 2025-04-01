(in-package :lem-core)

(defclass side-window (floating-window) ())

(defun side-window-p (window)
  (typep window 'side-window))

;; leftside
(defclass leftside-window (side-window) ())

(defun make-leftside-window (buffer &key (width 30))
  (cond ((frame-leftside-window (current-frame))
         (with-current-window (frame-leftside-window (current-frame))
           (switch-to-buffer buffer)))
        (t
         (setf (frame-leftside-window (current-frame))
               (make-instance 'leftside-window
                              :buffer buffer
                              :x 0
                              :y (topleft-window-y (current-frame))
                              :width width
                              :height (max-window-height (current-frame))
                              :use-modeline-p nil
                              :background-color nil
                              :border 0))
         (balance-windows))))

(defun delete-leftside-window ()
  (delete-window (frame-leftside-window (current-frame)))
  (setf (frame-leftside-window (current-frame)) nil)
  (balance-windows))

(defun resize-leftside-window (width)
  (let ((window (frame-leftside-window (current-frame))))
    (window-set-size window width (window-height window))
    (balance-windows)))

(defun resize-leftside-window-relative (offset)
  (let* ((window (frame-leftside-window (current-frame)))
         (new-width (+ (window-width window) offset)))
    (when (< 2 new-width)
      (window-set-size window
                       new-width
                       (window-height window))
      (balance-windows)
      t)))

;; rightside
(defclass rightside-window (side-window) ())

(defun make-rightside-window (buffer &key (width 30))
  (cond ((frame-rightside-window (current-frame))
         (with-current-window (frame-rightside-window (current-frame))
           (switch-to-buffer buffer))
         (frame-rightside-window (current-frame)))
        (t
         (let ((window
                 (make-instance 'rightside-window
                                :buffer buffer
                                :x (- (display-width) width)
                                :y (topleft-window-y (current-frame))
                                :width width
                                :height (max-window-height (current-frame))
                                :use-modeline-p nil
                                :background-color nil
                                :border 1
                                :border-shape :left-border)))
           (setf (frame-rightside-window (current-frame)) window)
           (balance-windows)
           window))))

(defun delete-rightside-window ()
  (delete-window (frame-rightside-window (current-frame)))
  (setf (frame-rightside-window (current-frame)) nil)
  (balance-windows))

(defun resize-rightside-window (window)
  (check-type window rightside-window)
  (window-set-size window
                   (window-width window)
                   (max-window-height (current-frame)))
  (window-set-pos window
                  (- (display-width) (window-width window))
                  (topleft-window-y (current-frame))))

(defun resize-rightside-window-relative (offset)
  (log:info offset)
  (let* ((window (frame-rightside-window (current-frame)))
         (new-x (+ (window-x window) offset))
         (new-width (- (window-width window) offset)))
    (when (< 2 new-width)
      (window-set-pos window
                      new-x
                      (window-y window))
      (window-set-size window
                       new-width
                       (window-height window))
      (balance-windows)
      t)))
