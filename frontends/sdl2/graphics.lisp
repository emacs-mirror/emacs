(defpackage :lem-sdl2/graphics
  (:use :cl)
  (:export :delete-drawable
           :clear-drawables
           :draw-line
           :draw-rectangle
           :draw-point
           :draw-points
           :draw-string
           :load-image
           :delete-image
           :draw-image
           :image-width
           :image-height))
(in-package :lem-sdl2/graphics)

(defclass drawable ()
  ((target :initarg :target
           :reader drawable-target)
   (draw-function :initarg :draw-function
                  :reader drawable-draw-function)
   (targets :initform '()
            :accessor drawable-targets)))

(defun buffer-drawables (buffer)
  (lem:buffer-value buffer 'drawables))

(defun (setf buffer-drawables) (drawables buffer)
  (setf (lem:buffer-value buffer 'drawables) drawables))

(defun window-drawables (window)
  (lem:window-parameter window 'drawables))

(defun (setf window-drawables) (drawables window)
  (setf (lem:window-parameter window 'drawables) drawables))

(defmethod drawables ((target lem:window))
  (window-drawables target))

(defmethod drawables ((target lem:buffer))
  (buffer-drawables target))

(defmethod (setf drawables) (drawables (target lem:window))
  (setf (window-drawables target) drawables))

(defmethod (setf drawables) (drawables (target lem:buffer))
  (setf (buffer-drawables target) drawables))

(defmethod add-drawable ((target lem:window) drawable)
  (push target (drawable-targets drawable))
  (push drawable (window-drawables target)))

(defmethod add-drawable ((target lem:buffer) drawable)
  (push target (drawable-targets drawable))
  (push drawable (buffer-drawables target)))

(defun delete-drawable (drawable)
  (dolist (target (drawable-targets drawable))
    (alexandria:deletef (drawables target) drawable)))

(defun clear-drawables (target)
  (mapc #'delete-drawable (drawables target))
  (values))

(defmethod lem-sdl2:render (texture window buffer)
  (dolist (drawable (window-drawables window))
    (funcall (drawable-draw-function drawable)))
  (dolist (drawable (buffer-drawables buffer))
    (funcall (drawable-draw-function drawable))))

(defun call-with-drawable (target draw-function)
  (let ((drawable
          (make-instance 'drawable
                         :target target
                         :draw-function draw-function)))
    (add-drawable target drawable)
    drawable))

(defmacro with-drawable ((target) &body body)
  `(call-with-drawable ,target (lambda () ,@body)))

(defun draw-line (target x1 y1 x2 y2 &key color)
  (with-drawable (target)
    (lem-sdl2/display::set-render-color lem-sdl2/display::*display* color)
    (sdl2:render-draw-line (lem-sdl2:current-renderer)
                           x1
                           y1
                           x2
                           y2)))

(defun draw-rectangle (target x y width height &key filled color)
  (with-drawable (target)
    (lem-sdl2/display::set-render-color lem-sdl2/display::*display* color)
    (sdl2:with-rects ((rect x y width height))
      (if filled
          (sdl2:render-fill-rect (lem-sdl2:current-renderer) rect)
          (sdl2:render-draw-rect (lem-sdl2:current-renderer) rect)))))

(defun draw-point (target x y &key color)
  (with-drawable (target)
    (lem-sdl2/display::set-render-color lem-sdl2/display::*display* color)
    (sdl2:render-draw-point (lem-sdl2:current-renderer) x y)))

(defun convert-to-points (x-y-seq)
  (let ((num-points (length x-y-seq)))
    (plus-c:c-let ((c-points sdl2-ffi:sdl-point :count num-points))
      (etypecase x-y-seq
        (vector
         (loop :for i :from 0
               :for (x . y) :across x-y-seq
               :do (let ((dest-point (c-points i)))
                     (sdl2::c-point (dest-point)
                       (setf (dest-point :x) x
                             (dest-point :y) y))))))
      (values (c-points plus-c:&)
              num-points))))

(defun draw-points (target x-y-seq &key color)
  (multiple-value-bind (points num-points)
      (convert-to-points x-y-seq)
    (with-drawable (target)
      (lem-sdl2/display::set-render-color lem-sdl2/display::*display* color)
      (sdl2:render-draw-points (lem-sdl2:current-renderer)
                               points
                               num-points))))

(defun draw-string (target string x y
                    &key (font (lem-sdl2/font:font-latin-normal-font
                                (lem-sdl2/display::display-font lem-sdl2/display::*display*)))
                         (color (alexandria:required-argument :color)))
  (let* ((surface (sdl2-ttf:render-utf8-blended font
                                                string
                                                (lem:color-red color)
                                                (lem:color-green color)
                                                (lem:color-blue color)
                                                0)))
    (with-drawable (target)
      (let ((texture (sdl2:create-texture-from-surface (lem-sdl2:current-renderer) surface)))
        (sdl2:with-rects ((dest-rect x
                                     y
                                     (sdl2:surface-width surface)
                                     (sdl2:surface-height surface)))
          (sdl2:render-copy (lem-sdl2:current-renderer) texture :dest-rect dest-rect))
        (sdl2:destroy-texture texture)))))

(defclass image ()
  ((surface :initarg :surface
            :reader image-surface)
   (width :initarg :width
          :reader image-width)
   (height :initarg :height
           :reader image-height)))

(defun load-image (pathname)
  (let ((image (sdl2-image:load-image pathname)))
    (make-instance 'image
                   :width (sdl2:surface-width image)
                   :height (sdl2:surface-height image)
                   :surface image)))

(defun delete-image (image)
  (declare (ignore image))
  (values))

(defun draw-image (target image
                   &key (x 0)
                        (y 0)
                        (width (image-width image))
                        (height (image-height image))
                        clip-rect)
  (with-drawable (target)
    (sdl2:with-rects ((dest-rect x y width height))
      (let ((source-rect
              (when clip-rect
                (destructuring-bind (x y w h) clip-rect
                  (sdl2:make-rect x y w h))))
            (texture (sdl2:create-texture-from-surface (lem-sdl2:current-renderer) (image-surface image))))
        (unwind-protect
             (sdl2:render-copy (lem-sdl2:current-renderer)
                               texture
                               :source-rect source-rect
                               :dest-rect dest-rect)
          (when source-rect
            (sdl2:free-rect source-rect))
          (sdl2:destroy-texture texture))))))
