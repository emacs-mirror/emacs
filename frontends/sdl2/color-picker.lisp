(defpackage :lem-sdl2/color-picker
  (:use :cl)
  (:export :make-color-picker-buffer))
(in-package :lem-sdl2/color-picker)

;;; widget

(defgeneric render (widget))
(defgeneric handle-mouse-button-down (widget x y button))
(defgeneric handle-mouse-button-up (widget))

(defclass widget ()
  ((parent :initform nil
           :initarg :parent
           :reader widget-parent)
   (x :initarg :x
      :initform (alexandria:required-argument :x)
      :reader widget-x)
   (y :initarg :y
      :initform (alexandria:required-argument :y)
      :reader widget-y)
   (width :initarg :width
          :initform (alexandria:required-argument :width)
          :reader widget-width)
   (height :initarg :height
           :initform (alexandria:required-argument :height)
           :reader widget-height)
   (texture :accessor widget-texture)
   (modified :initform t
             :reader widget-modified-p
             :writer set-widget-modified)))

(defmethod initialize-instance ((instance widget) &rest initargs)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (sdl2:in-main-thread ()
      (setf (widget-texture instance)
            ;; TODO: sdl2:destroy-texture
            (sdl2:create-texture (lem-sdl2:current-renderer)
                                 sdl2-ffi:+sdl-pixelformat-rgba8888+
                                 sdl2-ffi:+sdl-textureaccess-target+
                                 (widget-width instance)
                                 (widget-height instance))))
    instance))

(defmethod modify ((widget widget))
  (when (widget-parent widget)
    (modify (widget-parent widget)))
  (set-widget-modified t widget))

(defmethod render :around ((widget widget))
  (cond ((widget-modified-p widget)
         (set-widget-modified nil widget)
         (let ((previous-target (sdl2-ffi.functions:sdl-get-render-target (lem-sdl2:current-renderer))))
           (sdl2:set-render-target (lem-sdl2:current-renderer) (widget-texture widget))
           (unwind-protect (call-next-method)
             (sdl2:set-render-target (lem-sdl2:current-renderer) previous-target)
             (sdl2:with-rects ((rect (widget-x widget)
                                     (widget-y widget)
                                     (widget-width widget)
                                     (widget-height widget)))
               (sdl2:render-copy (lem-sdl2:current-renderer)
                                 (widget-texture widget)
                                 :dest-rect rect)))))
        (t
         (sdl2:with-rects ((rect (widget-x widget)
                                 (widget-y widget)
                                 (widget-width widget)
                                 (widget-height widget)))
           (sdl2:render-copy (lem-sdl2:current-renderer)
                             (widget-texture widget)
                             :dest-rect rect)))))

(defmethod handle-mouse-button-down :around ((widget widget) x y button)
  (when (and (<= (widget-x widget) x (+ (widget-x widget) (widget-width widget)))
             (<= (widget-y widget) y (+ (widget-y widget) (widget-height widget))))
    (call-next-method widget
                      (- x (widget-x widget))
                      (- y (widget-y widget))
                      button)))

(defmethod handle-mouse-button-down (widget x y button)
  (values))

(defmethod handle-mouse-button-up (widget)
  (values))

(defclass vertical-color-slider (widget)
  ((position :initform 0
             :accessor vertical-color-slider-position)
   (selected-color :initform (lem:make-color 255 0 0)
                   :accessor vertical-color-slider-selected-color)
   (slide-callback :initarg :slide-callback
                   :initform nil
                   :reader vertical-color-slider-slide-callback)))

(defmethod render ((widget vertical-color-slider))
  (let ((color (render-vertical-slider :x 0
                                       :y 0
                                       :width (widget-width widget)
                                       :height (widget-height widget)
                                       :slider-position (vertical-color-slider-position widget))))
    (setf (vertical-color-slider-selected-color widget) color)))

(defmethod handle-mouse-button-down ((widget vertical-color-slider) x y button)
  (when (eql button :button-1)
    (modify widget)
    (setf (vertical-color-slider-position widget) y)
    (when (vertical-color-slider-slide-callback widget)
      (funcall (vertical-color-slider-slide-callback widget) widget))))

(defun compute-slider-current-colors (color i segmented num)
  (let ((red (lem:color-red color))
        (green (lem:color-green color))
        (blue (lem:color-blue color)))
    (setf blue (if (<= i segmented) (+ blue num) blue))
    (setf red (if (and (>= i segmented) (< i (* 2 segmented))) (- red num) red))
    (setf green (if (and (>= i (* 2 segmented)) (< i (* 3 segmented))) (+ green num) green))

    (setf blue (if (and (>= i (* 3 segmented)) (< i (* 4 segmented))) (- blue num) blue))
    (setf red (if (and (>= i (* 4 segmented)) (< i (* 5 segmented))) (+ red num) red))
    (setf green (if (>= i (* 5 segmented)) (- green num) green))

    (setf red (if (> red 255.0) 255 red))
    (setf green (if (> green 255.0) 255 green))
    (setf blue (if (> blue 255.0) 255 blue))

    (setf red (if (< red 0) 0 red))
    (setf green (if (< green 0) 0 green))
    (setf blue (if (< blue 0) 0 blue))

    (lem:make-color (round red)
                    (round green)
                    (round blue))))

(defun render-vertical-slider (&key x y width height slider-position)
  (let* ((color (lem:make-color 255 0 0))
         (segmented (/ height 6))
         (num (/ 255 (/ height 6)))
         (selected-color color))
    (loop :for i :from 0 :below height
          :do (setf color (compute-slider-current-colors color i segmented num))
              (when (= i slider-position)
                (setf selected-color color))
              (sdl2:with-rects ((rect x (+ y i) width 1))
                (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display) color)
                (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect)))

    (sdl2:with-rects ((rect x (1- (+ slider-position y)) width 3))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 0 0 0))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))
    (sdl2:with-rects ((rect x (+ slider-position y) width 1))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 255 255 255))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))

    selected-color))

;;; color-square

(defclass color-square (widget)
  ((base-color :initform (lem:make-color 255 0 0)
               :accessor color-square-base-color)
   (cursor-x :initform 0
             :accessor color-square-cursor-x)
   (cursor-y :initform 0
             :accessor color-square-cursor-y)
   (selected-color :initform nil
                   :accessor color-square-selected-color)
   (texture :initform nil
            :accessor color-square-texture)))

(defmethod render ((widget color-square))
  (let ((selected-color
          (render-square :red (lem:color-red (color-square-base-color widget))
                         :green (lem:color-green (color-square-base-color widget))
                         :blue (lem:color-blue (color-square-base-color widget))
                         :x 0
                         :y 0
                         :size (widget-width widget)
                         :cursor-x (color-square-cursor-x widget)
                         :cursor-y (color-square-cursor-y widget))))
    (setf (color-square-selected-color widget) selected-color)))

(defmethod handle-mouse-button-down ((widget color-square) x y button)
  (when (eql button :button-1)
    (modify widget)
    (setf (color-square-cursor-x widget) x
          (color-square-cursor-y widget) y)))

(defun render-square (&key red green blue x y size cursor-x cursor-y)
  (let* ((xdifference-red (- 255.0 red))
         (xdifference-green (- 255.0 green))
         (xdifference-blue (- 255.0 blue))
         (xred-delta (if (= xdifference-red 0) 0 (/ xdifference-red size)))
         (xgreen-delta (if (= xdifference-green 0) 0 (/ xdifference-green size)))
         (xblue-delta (if (= xdifference-blue 0) 0 (/ xdifference-blue size)))
         (ydifference-red 255.0)
         (ydifference-green 255.0)
         (ydifference-blue 255.0)
         (yred-delta (/ ydifference-red size))
         (ygreen-delta (/ ydifference-green size))
         (yblue-delta (/ ydifference-blue size))
         (current-red 255.0)
         (current-green 255.0)
         (current-blue 255.0)
         selected-color)
    (dotimes (current-y size)
      (dotimes (current-x size)
        (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                           (lem:make-color (round current-red)
                                                           (round current-green)
                                                           (round current-blue)))
        (sdl2:render-draw-point (lem-sdl2:current-renderer) (+ current-x x) (+ current-y y))
        (decf current-red xred-delta)
        (decf current-green xgreen-delta)
        (decf current-blue xblue-delta)
        (setf current-red (if (> current-red 255.0) 255 current-red))
        (setf current-green (if (> current-green 255.0) 255 current-green))
        (setf current-blue (if (> current-blue 255.0) 255 current-blue))
        (setf current-red (if (< current-red 0) 0 current-red))
        (setf current-green (if (< current-green 0) 0 current-green))
        (setf current-blue (if (< current-blue 0) 0 current-blue))
        (when (and (= current-x cursor-x) (= current-y cursor-y))
          (setf selected-color
                (lem:make-color (round current-red)
                                (round current-green)
                                (round current-blue)))))
      (setf current-red (- 255.0 (* yred-delta (1+ current-y))))
      (setf current-green (- 255.0 (* ygreen-delta (1+ current-y))))
      (setf current-blue (- 255.0 (* yblue-delta (1+ current-y)))))

    (sdl2:with-rects ((rect x (+ +square-y+ (1- cursor-y)) size 3))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 0 0 0))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))
    (sdl2:with-rects ((rect x (+ +square-y+ cursor-y) size 1))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 255 255 255))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))

    (sdl2:with-rects ((rect (1- cursor-x) +square-y+ 3 size))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 0 0 0))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))
    (sdl2:with-rects ((rect cursor-x +square-y+ 1 size))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (lem:make-color 255 255 255))
      (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect))

    selected-color))

;; color-picker

(defparameter +square-x+ 0)
(defparameter +square-y+ 0)
(defparameter +square-size+ 500)
(defparameter +slider-x+ 510)
(defparameter +slider-y+ 0)
(defparameter +slider-width+ 50)
(defparameter +slider-height+ 500)

(defclass color-picker (widget)
  ((slider :accessor color-picker-slider)
   (square :accessor color-picker-square)
   (callback :initarg :callback :reader color-picker-callback))
  (:default-initargs
   :x 0
   :y 0
   :width (+ +square-size+ 10 +slider-width+)
   :height +square-size+))

(defmethod initialize-instance ((instance color-picker) &rest initargs)
  (declare (ignore initargs))
  (let* ((instance (call-next-method))
         (color-square (make-instance 'color-square
                                      :parent instance
                                      :x (+ (widget-x instance) +square-x+)
                                      :y (+ (widget-y instance) +square-y+)
                                      :width +square-size+
                                      :height +square-size+)))
    (setf (color-picker-slider instance)
          (make-instance 'vertical-color-slider
                         :parent instance
                         :x (+ (widget-x instance) +slider-x+)
                         :y (+ (widget-y instance) +slider-y+)
                         :width +slider-width+
                         :height +slider-height+
                         :slide-callback (lambda (slider-widget)
                                           (modify color-square)
                                           (setf (color-square-base-color color-square)
                                                 (vertical-color-slider-selected-color slider-widget)))))
    (setf (color-picker-square instance)
          color-square)
    instance))

(defmethod render ((widget color-picker))
  (render (color-picker-slider widget))
  (render (color-picker-square widget)))

(defmethod handle-mouse-button-down ((widget color-picker) x y button)
  (handle-mouse-button-down (color-picker-slider widget) x y button)
  (handle-mouse-button-down (color-picker-square widget) x y button))

(defmethod handle-mouse-button-up ((widget color-picker))
  (when (color-picker-callback widget)
    (funcall (color-picker-callback widget)
             (color-square-selected-color (color-picker-square widget)))))

;;; color-picker-buffer

(defclass color-picker-buffer (lem:text-buffer)
  ((color-picker :initarg :color-picker
                 :accessor color-picker-buffer-color-picker)))

(defmethod lem-sdl2:render (texture window (buffer color-picker-buffer))
  (sdl2:set-render-target (lem-sdl2:current-renderer) texture)
  (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                     (lem:make-color 100 100 100))
  (sdl2:render-fill-rect (lem-sdl2:current-renderer) nil)
  (render (color-picker-buffer-color-picker buffer)))

(defun %handle-mouse-button-down (mouse-event window buffer)
  (multiple-value-bind (x y)
      (lem-core::get-relative-mouse-coordinates-pixels mouse-event window)
    (handle-mouse-button-down (color-picker-buffer-color-picker buffer)
                              x
                              y
                              (lem-core::mouse-event-button mouse-event))))

(defmethod lem:handle-mouse-button-down ((buffer color-picker-buffer) mouse-event &key window)
  (%handle-mouse-button-down mouse-event window buffer))

(defmethod lem:handle-mouse-button-up ((buffer color-picker-buffer) mouse-event &key window)
  (declare (ignore window))
  (handle-mouse-button-up (color-picker-buffer-color-picker buffer)))

(defmethod lem:handle-mouse-hover ((buffer color-picker-buffer) mouse-event &key window)
  (when (eql :button-1 (lem-core::mouse-event-button mouse-event))
    (%handle-mouse-button-down mouse-event window buffer)))

(lem:define-minor-mode color-picker-mode
    (:name "Color Picker"
     :keymap *color-picker-keymap*))

(lem:define-key *color-picker-keymap* "q" 'color-picker-quit)

(lem:define-command color-picker-quit () ()
  (assert (lem:mode-active-p (lem:window-buffer (lem:current-window)) 'color-picker-mode))
  (lem:quit-window (lem:current-window) :kill-buffer t))

(defun make-color-picker-buffer (buffer-name &key callback)
  (let ((buffer (lem:make-buffer buffer-name)))
    (change-class buffer 'color-picker-buffer
                  :color-picker (make-instance 'color-picker :x 0 :y 0 :callback callback))
    (lem:change-buffer-mode buffer 'color-picker-mode)
    buffer))

(defmethod lem-color-preview:invoke-color-picker ((frontend lem-sdl2/sdl2:sdl2) callback)
  (let ((buffer (make-color-picker-buffer "*Color Picker*" :callback callback)))
    (setf (lem:current-window)
          (lem:pop-to-buffer buffer))))

(lem:define-command test-color-picker () ()
  (lem:pop-to-buffer (make-color-picker-buffer "*test*")))
