(defpackage :lem-sdl2/image-buffer
  (:use
   :cl
   :lem
   :lem-sdl2
   :lem-sdl2/graphics)
  (:export :image-fit-to-height
           :image-fit-to-screen
           :image-fit-to-width
           :image-zoom-help
           :image-zoom-in
           :image-zoom-out
           :image-zoom-reset))
(in-package :lem-sdl2/image-buffer)

(defclass image-buffer (text-buffer) ())

(defun buffer-image (buffer)
  (buffer-value buffer 'image))

(defun (setf buffer-image) (image buffer)
  (setf (buffer-value buffer 'image) image))

(defun buffer-scaling (buffer)
  (buffer-value buffer 'scaling))

(defun (setf buffer-scaling) (scaling buffer)
  (setf (buffer-value buffer 'scaling) scaling))

(define-major-mode image-viewer-mode ()
    (:name "Image Viewer"
     :keymap *image-viewer-keymap*)
  (modeline-add-status-list 'image-information (current-buffer))
  (setf (lem:buffer-read-only-p (current-buffer)) t))

(defun image-information (window)
  (let ((image (buffer-image (window-buffer window))))
    (format nil "  ~Dx~D (x~,2F)"
            (image-width image)
            (image-height image)
            (buffer-scaling (window-buffer window)))))

;; Zoom.
(define-key *image-viewer-keymap* "C-+" 'image-zoom-in)
(define-key *image-viewer-keymap* "+" 'image-zoom-in)
(define-key *image-viewer-keymap* "C--" 'image-zoom-out)
(define-key *image-viewer-keymap* "-" 'image-zoom-out)
(define-key *image-viewer-keymap* "C-0" 'image-zoom-reset)
(define-key *image-viewer-keymap* "0" 'image-zoom-reset)
(define-key *image-viewer-keymap* "?" 'image-zoom-help)
(define-key *image-viewer-keymap* "C-h" 'image-zoom-help)
;; Fit to width, height, screen.
(define-key *image-viewer-keymap* "f" 'image-fit-to-screen)
(define-key *image-viewer-keymap* "w" 'image-fit-to-width)
(define-key *image-viewer-keymap* "h" 'image-fit-to-height)

(defmethod render :before (texture window (buffer image-buffer))
  (sdl2:set-render-target (current-renderer) texture)
  (lem-sdl2/display::set-render-color lem-sdl2/display::*display* (lem-sdl2/display::display-background-color lem-sdl2/display::*display*))
  (sdl2:with-rects ((rect 0
                          0
                          (* (lem-sdl2::char-width)
                             (window-width window))
                          (* (lem-sdl2::char-height)
                             (1- (window-height window)))))
    (sdl2:render-fill-rect (current-renderer) rect)))

(defun scale-buffer-image (buffer scale-offset)
  (clear-drawables buffer)
  (let ((image (buffer-image buffer)))
    (incf (buffer-scaling buffer) scale-offset)
    (draw-image buffer
                image
                :x 0
                :y 0
                :width (round (* (image-width image) (buffer-scaling buffer)))
                :height (round (* (image-height image) (buffer-scaling buffer))))))

(defun reset-buffer-scale (buffer)
  (clear-drawables buffer)
  (setf (buffer-scaling buffer) 1)
  (let ((image (buffer-image buffer)))
    (draw-image buffer
                image
                :x 0
                :y 0
                :width (image-width image)
                :height (image-height image))))

(defun fit-to-width (buffer)
  (clear-drawables buffer)
  (let* ((image (buffer-image buffer))
         (image-width (image-width image))
         (display-width (* (lem-sdl2::char-width)
                           (window-width (current-window)))))
    (let* ((ratio (/ image-width display-width))
           (percent (- 100 (/ 100.0 ratio))))
      (setf (buffer-scaling buffer) 1)
      (scale-buffer-image buffer (* -1 (/ percent 100))))))

(defun fit-to-height (buffer)
  (clear-drawables buffer)
  (let* ((image (buffer-image buffer))
         (image-height (image-height image))
         (display-height (* (lem-sdl2::char-height)
                            (1-
                             (window-height (current-window))))))
    (let* ((ratio (/ image-height display-height))
           (percent (- 100 (/ 100.0 ratio))))
      (setf (buffer-scaling buffer) 1)
      (scale-buffer-image buffer (* -1 (/ percent 100))))))

(defun fit-to-screen (buffer)
  "Change the image dimensions so it fits in the screen."
  (let* ((image (buffer-image buffer))
         (width (image-width image))
         (height (image-height image))
         (display-width (* (lem-sdl2::char-width)
                           (window-width (current-window))))
         (display-height (* (lem-sdl2::char-height)
                            (1-
                             (window-height (current-window)))))
         (ratio-image (/ height width))
         (ratio-display (/ display-height display-width)))

    (if (>= ratio-image ratio-display)
        (fit-to-height buffer)
        (fit-to-width buffer))))

(define-command image-zoom-in () ()
  (scale-buffer-image (current-buffer) 0.1))

(define-command image-zoom-out () ()
  (scale-buffer-image (current-buffer) -0.1))

(define-command image-zoom-reset () ()
  "Set the image to its original size."
  (reset-buffer-scale (current-buffer)))

(define-command image-fit-to-width () ()
  "Make the image as large as the display width."
  (fit-to-width (current-buffer)))

(define-command image-fit-to-height () ()
  "Make the image as big as the display height."
  (fit-to-height (current-buffer)))

(define-command image-fit-to-screen () ()
  "Enlarge or shrink the image to fit the display."
  (fit-to-screen (current-buffer)))

(defclass sdl2-find-file-executor (lem:find-file-executor) ())

(defmethod lem:execute-find-file ((executor sdl2-find-file-executor) mode pathname)
  (cond ((member (pathname-type pathname)
                 '("png" "jpg" "jpeg" "bmp" "gif")
                 :test #'equal)
         (open-image-buffer pathname))
        (t
         (call-next-method))))

(defun open-image-buffer (pathname)
  (let ((image (load-image pathname))
        (buffer (lem:make-buffer (file-namestring pathname)
                                 :directory (expand-file-name
                                             (namestring (uiop:pathname-directory-pathname pathname))))))
    (change-class buffer 'image-buffer)
    (setf (buffer-image buffer) image)
    (setf (buffer-scaling buffer) 1)
    (draw-image buffer image :x 0 :y 0)
    (fit-to-screen buffer)
    (change-buffer-mode buffer 'image-viewer-mode)
    buffer))

(setf lem:*find-file-executor* (make-instance 'sdl2-find-file-executor))

(define-command image-zoom-help () ()
  (with-pop-up-typeout-window (s (make-buffer "*image-zoom-help*") :erase t)
    (format s "Open an image file in Lem and use these keys to zoom in and out:~&")
    (format s "Zoom in: + or C - + (M-x image-zoom-in)~&")
    (format s "Zoom out: - or C - - (M-x image-zoom-out)~&")
    (format s "Reset: 0 or C - 0 (M-x image-zoom-reset)~&")
    (format s "~%")
    (format s "Fit the image to the screen:~&")
    (format s "Fit to screen: f (M-x image-fit-to-screen)~&")
    (format s "Fit to width: w (M-x image-fit-to-width)~&")
    (format s "Fit to height: h (M-x image-fit-to-height)~&")))
