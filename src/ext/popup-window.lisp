(defpackage :lem/popup-window
  (:use :cl :lem)
  #+sbcl
  (:lock t))
(in-package :lem/popup-window)

(defconstant +border-size+ 1)
(defconstant +min-width+   3)
(defconstant +min-height+  1)

(defvar *extra-right-margin* 0)
(defvar *extra-width-margin* 0)

(defgeneric adjust-for-redrawing (gravity popup-window)
  (:method (gravity popup-window)))

(defgeneric compute-popup-window-rectangle (gravity &key source-window width height border-size))

(defclass gravity ()
  ((offset-x :initarg :offset-x :accessor gravity-offset-x :initform 0)
   (offset-y :initarg :offset-y :accessor gravity-offset-y :initform 0)))
(defclass gravity-center (gravity) ())
(defclass gravity-top-display (gravity) ())
(defclass gravity-bottom-display (gravity) ())
(defclass gravity-top (gravity) ())
(defclass gravity-topright (gravity) ())
(defclass gravity-cursor (gravity) ())
(defclass gravity-follow-cursor (gravity-cursor) ())
(defclass gravity-mouse-cursor (gravity) ())
(defclass gravity-vertically-adjacent-window (gravity) ())
(defclass gravity-vertically-adjacent-window-dynamic (gravity) ())
(defclass gravity-horizontally-adjacent-window (gravity) ())
(defclass gravity-horizontally-above-window (gravity) ())

(defclass popup-window (floating-window)
  ((gravity
    :initarg :gravity
    :reader popup-window-gravity)
   (source-window
    :initarg :source-window
    :reader popup-window-source-window)
   (base-width
    :initarg :base-width
    :reader popup-window-base-width)
   (base-height
    :initarg :base-height
    :reader popup-window-base-height)
   (style
    :initarg :style
    :reader popup-window-style)))

(defmethod lem:window-parent ((window popup-window))
  (popup-window-source-window window))

(defun ensure-gravity (gravity)
  (if (typep gravity 'gravity)
      gravity
      (ecase gravity
        (:center (make-instance 'gravity-center))
        (:top-display (make-instance 'gravity-top-display))
        (:bottom-display (make-instance 'gravity-bottom-display))
        (:top (make-instance 'gravity-top))
        (:topright (make-instance 'gravity-topright))
        (:cursor (make-instance 'gravity-cursor))
        (:follow-cursor (make-instance 'gravity-follow-cursor))
        (:mouse-cursor (make-instance 'gravity-mouse-cursor))
        (:vertically-adjacent-window (make-instance 'gravity-vertically-adjacent-window))
        (:vertically-adjacent-window-dynamic (make-instance 'gravity-vertically-adjacent-window-dynamic))
        (:horizontally-adjacent-window (make-instance 'gravity-horizontally-adjacent-window))
        (:horizontally-above-window (make-instance 'gravity-horizontally-above-window)))))

(defmethod adjust-for-redrawing ((gravity gravity-follow-cursor) popup-window)
  (destructuring-bind (x y width height)
      (compute-popup-window-rectangle (popup-window-gravity popup-window)
                                      :source-window (popup-window-source-window popup-window)
                                      :width (popup-window-base-width popup-window)
                                      :height (popup-window-base-height popup-window)
                                      :border-size (floating-window-border popup-window))
    (lem-core::window-set-size popup-window width height)
    (lem-core::window-set-pos popup-window
                              (+ x (floating-window-border popup-window))
                              (+ y (floating-window-border popup-window)))))

(defmethod compute-popup-window-rectangle :around ((gravity gravity) &key &allow-other-keys)
  (destructuring-bind (x y width height)
      (call-next-method)
    (list (+ x (gravity-offset-x gravity))
          (+ y (gravity-offset-y gravity))
          (max width 1)
          (max height 1))))

(defmethod compute-popup-window-rectangle ((gravity gravity-center)
                                           &key width height &allow-other-keys)
  (let ((x (- (floor (display-width) 2)
              (floor width 2)))
        (y (- (floor (display-height) 2)
              (floor height 2))))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-cursor)
                                           &key source-window width height border-size)
  (let* ((border-size (or border-size 0))
         (b2 (* border-size 2))
         (disp-w (max (- (display-width)  b2 *extra-right-margin*)
                      +min-width+))
         (disp-h (max (- (display-height) b2)
                      +min-height+))
         (width  (max (+ width *extra-width-margin*)
                      +min-width+))
         (height (max height
                      +min-height+))
         (x (+ (window-x source-window)
               (window-cursor-x source-window)))
         (y (max (min (+ (window-y source-window)
                         (window-cursor-y source-window)
                         border-size)
                      (1- (display-height)))
                 0))
         (w width)
         (h height))
    ;; calc y and h
    (when (> (+ y height) disp-h)
      (decf h (- (+ y height) disp-h)))
    (when (< h (min height (floor disp-h 3)))
      (setf h height)
      (decf y (+ height b2 1)))
    (when (< y 0)
      (decf h (- y))
      (setf y 0))
    (when (<= h 0) ; for safety
      (setf y 0)
      (setf h (min height disp-h)))
    ;; calc x and w
    (when (> (+ x width) disp-w)
      (decf x (- (+ x width) disp-w)))
    (when (< x 0)  ; for safety
      (setf x 0)
      (setf w (min width disp-w)))
    (list x y w h)))

(defmethod compute-popup-window-rectangle ((gravity gravity-mouse-cursor) &key width height
                                                                          &allow-other-keys)
  (multiple-value-bind (x y)
      (lem-if:get-mouse-position (lem:implementation))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-top-display)
                                           &key width height
                                           &allow-other-keys)
  (let* ((x (- (floor (display-width) 2)
               (floor width 2)))
         (y 1))
    (list x y (1- width) height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-bottom-display)
                                           &key width height
                                           &allow-other-keys)
  (let* ((x (- (floor (display-width) 2)
               (floor width 2)))
         (y (- (display-height) height)))
    (list x y (1- width) height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-top) &key source-window width height
                                                                 &allow-other-keys)
  (let* ((x (- (floor (display-width) 2)
               (floor width 2)))
         (y (+ (window-y source-window) 1)))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-topright)
                                           &key source-window width height border-size
                                           &allow-other-keys)
  (let* ((b2 (* (or border-size 0) 2))
         (win-x (window-x source-window))
         (win-y (window-y source-window))
         (win-w (max (- (window-width  source-window) b2 2)
                     +min-width+))
         (win-h (max (- (window-height source-window) b2)
                     +min-height+))
         (width  (max (+ width *extra-width-margin*)
                      +min-width+))
         (height (max height
                      +min-height+))
         (x (+ win-x (- win-w width)))
         (y (+ win-y 1))
         (w width)
         (h height))
    ;; calc y and h
    (when (> (+ y height) (+ win-y win-h))
      (decf h (- (+ y height) (+ win-y win-h))))
    (when (<= h 0)    ; for safety
      (setf y win-y)
      (setf h (min height win-h)))
    ;; calc x and w
    (when (< x win-x) ; for safety
      (setf x win-x)
      (setf w (min width win-w)))
    (list x y w h)))

(defmethod compute-popup-window-rectangle ((gravity gravity-vertically-adjacent-window)
                                           &key source-window width height #+(or)border-size
                                           &allow-other-keys)
  (let ((x (+ (window-x source-window)
              (window-width source-window)))
        (y (window-y source-window)))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-vertically-adjacent-window-dynamic)
                                           &key source-window width height #+(or)border-size
                                           &allow-other-keys)
  (let ((x (+ (window-x source-window) (window-width source-window)))
        (y (window-y source-window)))
    (when (>= (+ x width) (display-width))
      (setf (gravity-offset-x gravity) (- (gravity-offset-x gravity))
            x (max 0 (- (window-x source-window) width 1))))
    (when (>= (+ y height) (display-height))
      (setf (gravity-offset-y gravity) 0
            y (max 0 (- (display-height) height 2))))
    (list x y width height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-horizontally-adjacent-window)
                                           &key source-window width height border-size
                                           &allow-other-keys)
  (let ((x (- (window-x source-window) border-size))
        (y (+ (window-y source-window)
              (window-height source-window)
              border-size)))
    ;; workaround: cases that extend beyond the screen
    (when (< (display-height) (+ y height))
      (setf height (- (display-height) y 1)))
    (list x
          y
          (max width (window-width source-window))
          height)))

(defmethod compute-popup-window-rectangle ((gravity gravity-horizontally-above-window)
                                           &key source-window width height border-size
                                           &allow-other-keys)
  (let ((x (- (window-x source-window) border-size))
        (y (- (window-y source-window)
              height
              border-size)))
    ;; workaround: cases that extend beyond the screen
    (when (< (display-height) (+ y height))
      (setf height (- (display-height) y 1)))
    (list x
          y
          (max width (window-width source-window))
          height)))

(defun compute-buffer-width (buffer)
  (with-point ((point (buffer-start-point buffer)))
    (loop :maximize (point-column (line-end point))
          :while (line-offset point 1))))

(defun compute-buffer-height (buffer)
  (buffer-nlines buffer))

(defun compute-buffer-size (buffer)
  (list (compute-buffer-width buffer)
        (compute-buffer-height buffer)))

(defmethod window-redraw ((popup-window popup-window) force)
  (adjust-for-redrawing (popup-window-gravity popup-window) popup-window)
  (call-next-method))

(defstruct style
  (gravity :cursor)
  (use-border t)
  (background-color nil)
  (offset-x 0)
  (offset-y 0)
  (cursor-invisible nil)
  shape)

(defun merge-style (style &key (gravity nil gravity-p)
                               (use-border nil use-border-p)
                               (background-color nil background-color-p)
                               (cursor-invisible nil cursor-invisible-p)
                               (shape nil shape-p))
  (make-style :gravity (if gravity-p
                           gravity
                           (style-gravity style))
              :use-border (if use-border-p
                              use-border
                              (style-use-border style))
              :background-color (if background-color-p
                                    background-color
                                    (style-background-color style))
              :offset-x (style-offset-x style)
              :offset-y (style-offset-y style)
              :cursor-invisible (if cursor-invisible-p
                                    cursor-invisible
                                    (style-cursor-invisible style))
              :shape (if shape-p
                         shape
                         (style-shape style))))

(defun ensure-style (style)
  (cond ((null style)
         (make-style))
        ((style-p style)
         style)
        (t
         (apply #'make-style style))))

(defun make-popup-window (&key (source-window (alexandria:required-argument :source-window))
                               (buffer (alexandria:required-argument :buffer))
                               (width (alexandria:required-argument :width))
                               (height (alexandria:required-argument :height))
                               (clickable t)
                               style)
  (let* ((style (ensure-style style))
         (border-size (if (style-use-border style) +border-size+ 0))
         (gravity (ensure-gravity (style-gravity style))))
    (setf (gravity-offset-x gravity) (style-offset-x style)
          (gravity-offset-y gravity) (style-offset-y style))
    (destructuring-bind (x y w h)
        (compute-popup-window-rectangle gravity
                                        :source-window source-window
                                        :width width
                                        :height height
                                        :border-size border-size)
      (make-instance 'popup-window
                     :buffer buffer
                     :x (+ x border-size)
                     :y (+ y border-size)
                     :width  w
                     :height h
                     :use-modeline-p nil
                     :gravity gravity
                     :source-window source-window
                     :base-width  width
                     :base-height height
                     :border border-size
                     :border-shape (style-shape style)
                     :background-color (style-background-color style)
                     :cursor-invisible (style-cursor-invisible style)
                     :clickable clickable
                     :style style))))

(defun update-popup-window (&key (source-window (alexandria:required-argument :source-window))
                                 (width (alexandria:required-argument :width))
                                 (height (alexandria:required-argument :height))
                                 (destination-window
                                  (alexandria:required-argument :destination-window)))
  (let* ((style (popup-window-style destination-window))
         (border-size (if (style-use-border style) +border-size+ 0))
         (gravity (ensure-gravity (style-gravity style))))
    (setf (gravity-offset-x gravity) (style-offset-x style)
          (gravity-offset-y gravity) (style-offset-y style))
    (destructuring-bind (x y w h)
        (compute-popup-window-rectangle gravity
                                        :source-window source-window
                                        :width width
                                        :height height
                                        :border-size border-size)
      ;; XXX: workaround for context-menu
      (unless (typep gravity 'gravity-cursor)
        (lem-core::window-set-pos destination-window
                                  (+ x border-size)
                                  (+ y border-size)))
      (lem-core::window-set-size destination-window w h)
      destination-window)))
