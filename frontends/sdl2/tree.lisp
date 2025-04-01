(defpackage :lem-sdl2/tree
  (:use :cl :lem))
(in-package :lem-sdl2/tree)

(defconstant +scroll-unit+ 30)

(defclass tree-view-buffer (text-buffer)
  ((drawables :initarg :drawables
              :accessor tree-view-buffer-drawables)
   (scroll-x :initform 0
             :accessor tree-view-buffer-scroll-x)
   (scroll-y :initform 0
             :accessor tree-view-buffer-scroll-y)
   (margin-x :initarg :margin-x
             :initform 100
             :accessor tree-view-buffer-margin-x)
   (margin-y :initarg :margin-y
             :initform 50
             :accessor tree-view-buffer-margin-y)
   (width :accessor tree-view-buffer-width)
   (height :accessor tree-view-buffer-height)
   (active-node :initform nil
                :accessor tree-view-buffer-active-node)))

(defmethod tree-view-display-end ((buffer tree-view-buffer))
  (+ (tree-view-buffer-scroll-y buffer)
     (lem-sdl2/display::display-height lem-sdl2/display::*display*)))

(defmethod tree-view-scroll-vertically ((buffer tree-view-buffer) window n)
  (incf (tree-view-buffer-scroll-y buffer) n)
  (let* ((height (* (1- (window-height window))
                    (lem-if:get-char-height (implementation))))
         (last-y (max 0 (- (tree-view-buffer-height buffer) height))))
    (cond ((< last-y
              (tree-view-buffer-scroll-y buffer))
           (setf (tree-view-buffer-scroll-y buffer)
                 last-y))
          ((< (tree-view-buffer-scroll-y buffer) 0)
           (setf (tree-view-buffer-scroll-y buffer) 0)))))

(defmethod tree-view-scroll-horizontally ((buffer tree-view-buffer) window n)
  (incf (tree-view-buffer-scroll-x buffer) n)
  (cond ((< (tree-view-buffer-scroll-x buffer) 0)
         (setf (tree-view-buffer-scroll-x buffer) 0))))

(defmethod tree-view-scroll-horizontally-first ((buffer tree-view-buffer))
  (setf (tree-view-buffer-scroll-x buffer) 0))

(defclass node ()
  ((name :initarg :name
         :reader node-name)
   (value :initarg :value
          :reader node-value)
   (children :initarg :children
             :initform '()
             :reader node-children)
   (x :initform 0
      :accessor node-x)
   (y :initform 0
      :accessor node-y)
   (last-y :initform 0
           :accessor node-last-y)
   (click-callback :initarg :click-callback
                   :reader node-click-callback)))

(defmethod print-object ((node node) stream)
  (loop :repeat (* 2 (or *print-level* 0)) :do (write-char #\space stream))
  (print-unreadable-object (node stream :type t)
    (let ((*print-level* (1+ (or *print-level* 0))))
      (format stream "~S x:~D y:~D" (node-value node) (node-x node) (node-y node))
      (when(node-children node)
        (format stream "~%~{~S~^~%~}" (node-children node))))))

(defun compute-position-with-rightward-extending (node)
  (labels ((recursive (node current-y depth)
             (setf (node-x node) depth)
             (cond ((null (node-children node))
                    (setf (node-y node) current-y)
                    (setf (node-last-y node) current-y)
                    (node-y node))
                   (t
                    (loop :for child :in (node-children node)
                          :for i := current-y :then (1+ child-y)
                          :for child-y := (recursive child i (1+ depth))
                          :sum (node-y child) :into sum-y
                          :finally (setf (node-y node)
                                         (/ sum-y (length (node-children node)))))
                    (setf (node-last-y node)
                          (node-last-y (car (last (node-children node)))))))))
    (recursive node 0 0)))

(defclass text-node ()
  ((surface :initarg :surface
            :reader text-node-surface)
   (x :initarg :x
      :reader text-node-x)
   (y :initarg :y
      :reader text-node-y)
   (width :initarg :width
          :reader text-node-width)
   (height :initarg :height
           :reader text-node-height)
   (node :initarg :node
         :reader text-node-node)))

(defclass line-edge ()
  ((x0 :initarg :x0
       :reader line-edge-x0)
   (y0 :initarg :y0
       :reader line-edge-y0)
   (x1 :initarg :x1
       :reader line-edge-x1)
   (y1 :initarg :y1
       :reader line-edge-y1)
   (color :initarg :color
          :reader line-edge-color)))

(defun compute-width (objects)
  (loop :for object :in objects
        :when (typep object 'text-node)
        :maximize (+ (text-node-x object)
                     (text-node-width object))))

(defun compute-height (objects)
  (loop :for object :in objects
        :when (typep object 'text-node)
        :maximize (+ (text-node-y object)
                     (text-node-height object))))

(defun load-font ()
  (sdl2-ttf:open-font
   (lem-sdl2/resource:get-resource-pathname
    "resources/fonts/NotoSansMono-Regular.ttf")
   (lem-sdl2/font:font-config-size
    (lem-sdl2/display:display-font-config
     (lem-sdl2/display:current-display)))))

(defun draw (buffer node)
  (let ((drawables '())
        (font (load-font)))
    (labels ((recursive (node current-x)
               (let* ((y (round (* (tree-view-buffer-margin-y buffer) (node-y node))))
                      (surface (sdl2-ttf:render-utf8-blended font
                                                             (princ-to-string (node-name node))
                                                             255
                                                             255
                                                             255
                                                             0))
                      (node-width (sdl2:surface-width surface))
                      (node-height (sdl2:surface-height surface)))
                 (push (make-instance 'text-node
                                      :surface surface
                                      :x current-x
                                      :y y
                                      :width node-width
                                      :height node-height
                                      :node node)
                       drawables)
                 (dolist (child (node-children node))
                   (multiple-value-bind (child-x child-y child-width child-height)
                       (recursive child (+ (tree-view-buffer-margin-x buffer)
                                           (+ current-x node-width)))
                     (declare (ignore child-width))
                     (push (make-instance 'line-edge
                                          :color (lem:make-color 255 255 255)
                                          :x0 (+ current-x (sdl2:surface-width surface))
                                          :y0 (+ y (round (sdl2:surface-height surface) 2))
                                          :x1 child-x
                                          :y1 (+ child-y (round child-height 2)))
                           drawables)))
                 (values current-x
                         y
                         (sdl2:surface-width surface)
                         (sdl2:surface-height surface)))))
      (recursive node 0)
      (setf (tree-view-buffer-drawables buffer) drawables)
      (setf (tree-view-buffer-width buffer) (compute-width drawables))
      (setf (tree-view-buffer-height buffer) (compute-height drawables))
      (values))))

(defmethod render ((text-node text-node) buffer)
  (when (<= (tree-view-buffer-scroll-y buffer)
            (text-node-y text-node)
            (+ (text-node-y text-node)
               (text-node-height text-node))
            (tree-view-display-end buffer))
    (let ((texture
            (sdl2:create-texture-from-surface
             (lem-sdl2:current-renderer)
             (text-node-surface text-node))))
      (sdl2:with-rects ((dest-rect (- (text-node-x text-node)
                                      (tree-view-buffer-scroll-x buffer))
                                   (- (text-node-y text-node)
                                      (tree-view-buffer-scroll-y buffer))
                                   (text-node-width text-node)
                                   (text-node-height text-node)))
        (sdl2:render-copy (lem-sdl2:current-renderer) texture :dest-rect dest-rect)
        (when (eq text-node (tree-view-buffer-active-node buffer))
          (sdl2:set-render-draw-color (lem-sdl2:current-renderer) 255 255 255 0)
          (sdl2:render-draw-rect (lem-sdl2:current-renderer) dest-rect)))
      (sdl2:destroy-texture texture))))

(defmethod render ((line-edge line-edge) buffer)
  (let ((y0 (min (line-edge-y0 line-edge)
                 (line-edge-y1 line-edge)))
        (y1 (max (line-edge-y0 line-edge)
                 (line-edge-y1 line-edge)))
        (start-x (tree-view-buffer-scroll-x buffer))
        (start-y (tree-view-buffer-scroll-y buffer))
        (end (tree-view-display-end buffer)))
    (unless (or (< y1 start-y) (< end y0))
      (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                         (line-edge-color line-edge))
      (sdl2:render-draw-line (lem-sdl2:current-renderer)
                             (- (line-edge-x0 line-edge)
                                start-x)
                             (- (line-edge-y0 line-edge)
                                start-y)
                             (- (line-edge-x1 line-edge)
                                start-x)
                             (- (line-edge-y1 line-edge)
                                start-y)))))

(defun render-all (buffer)
  (loop :for drawable :in (tree-view-buffer-drawables buffer)
        :do (render drawable buffer)))

(defun get-node-at-coordinates (buffer x y)
  (let ((scroll-y (tree-view-buffer-scroll-y buffer)))
    (dolist (drawable (tree-view-buffer-drawables buffer))
      (when (typep drawable 'text-node)
        (let ((node-y (- (text-node-y drawable) scroll-y)))
          (when (and (typep drawable 'text-node)
                     (<= (text-node-x drawable)
                         x
                         (+ (text-node-x drawable)
                            (text-node-width drawable)))
                     (<= node-y
                         y
                         (+ node-y
                            (text-node-height drawable))))
            (return drawable)))))))

(define-major-mode tree-view-mode ()
    (:name "Tree View"
     :keymap *tree-view-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *tree-view-keymap* 'forward-char 'tree-view-scroll-right)
(define-key *tree-view-keymap* 'backward-char 'tree-view-scroll-left)
(define-key *tree-view-keymap* 'move-to-beginning-of-line 'tree-view-scroll-horizontally-start)
(define-key *tree-view-keymap* 'next-line 'tree-view-scroll-down)
(define-key *tree-view-keymap* 'previous-line 'tree-view-scroll-up)
(define-key *tree-view-keymap* 'next-page 'tree-view-scroll-pagedown)
(define-key *tree-view-keymap* 'previous-page 'tree-view-scroll-pageup)
(define-key *tree-view-keymap* 'move-to-end-of-buffer 'tree-view-scroll-bottom)
(define-key *tree-view-keymap* 'move-to-beginning-of-buffer 'tree-view-scroll-top)

(define-command tree-view-scroll-right (n) (:universal)
  (tree-view-scroll-horizontally (current-buffer)
                                 (current-window)
                                 (* n +scroll-unit+)))

(define-command tree-view-scroll-left (n) (:universal)
  (tree-view-scroll-horizontally (current-buffer)
                                 (current-window)
                                 (* (- n) +scroll-unit+)))

(define-command tree-view-scroll-horizontally-start () ()
  (tree-view-scroll-horizontally-first (current-buffer)))

(define-command tree-view-scroll-down (n) (:universal)
  (tree-view-scroll-vertically (current-buffer)
                               (current-window)
                               (* n +scroll-unit+)))

(define-command tree-view-scroll-up (n) (:universal)
  (tree-view-scroll-vertically (current-buffer)
                               (current-window)
                               (* (- n) +scroll-unit+)))

(defun scroll-page-unit ()
  (floor (lem-sdl2/display:display-height (lem-sdl2/display:current-display)) 1.1))

(define-command tree-view-scroll-pagedown () ()
  (tree-view-scroll-vertically (current-buffer) (current-window) (scroll-page-unit)))

(define-command tree-view-scroll-pageup () ()
  (tree-view-scroll-vertically (current-buffer) (current-window) (- (scroll-page-unit))))

(define-command tree-view-scroll-bottom () ()
  (tree-view-scroll-vertically (current-buffer)
                               (current-window)
                               (tree-view-buffer-height (current-buffer))))

(define-command tree-view-scroll-top () ()
  (tree-view-scroll-vertically (current-buffer)
                               (current-window)
                               (- (tree-view-buffer-height (current-buffer)))))

(defmethod lem-sdl2:render (texture window (buffer tree-view-buffer))
  (sdl2:set-render-target (lem-sdl2:current-renderer) texture)
  (lem-sdl2/display:set-render-color (lem-sdl2/display:current-display)
                                     (lem-sdl2/display:display-background-color
                                      (lem-sdl2/display:current-display)))
  (sdl2:render-fill-rect (lem-sdl2:current-renderer) nil)
  (render-all buffer))

(defmethod execute ((mode tree-view-mode) (command scroll-down) argument)
  (tree-view-scroll-vertically (current-buffer)
                               (current-window)
                               (* argument +scroll-unit+)))

(defmethod lem:handle-mouse-button-down ((buffer tree-view-buffer) mouse-event &key window)
  (multiple-value-bind (x y)
      (lem-core::get-relative-mouse-coordinates-pixels mouse-event window)
    (let ((node (get-node-at-coordinates buffer x y)))
      (cond ((null node)
             (call-next-method))
            ((eql :button-1 (lem-core::mouse-event-button mouse-event))
             (funcall (node-click-callback (text-node-node node))
                      (text-node-node node)))))))

(defmethod lem:handle-mouse-hover ((buffer tree-view-buffer) mouse-event &key window)
  (multiple-value-bind (x y)
      (lem-core::get-relative-mouse-coordinates-pixels mouse-event window)
    (let ((node (get-node-at-coordinates buffer x y)))
      (setf (tree-view-buffer-active-node buffer) node))))

(defun make-tree-view-buffer (buffer-name)
  (let ((buffer (make-buffer buffer-name)))
    (change-class buffer 'tree-view-buffer)
    (change-buffer-mode buffer 'tree-view-mode)
    buffer))

(defun draw-tree (buffer-name node)
  (compute-position-with-rightward-extending node)
  (let ((buffer (make-tree-view-buffer buffer-name)))
    (draw buffer node)
    buffer))

;;;
(defun find-tree-view-window (buffer-name)
  (alexandria:when-let* ((buffer (get-buffer buffer-name))
                         (window (first (get-buffer-windows buffer))))
    window))

(defun make-class-tree (tree buffer-name)
  (make-instance 'node
                 :name (first tree)
                 :value (first tree)
                 :click-callback (lambda (node)
                                   (alexandria:when-let
                                       (window (find-tree-view-window buffer-name))
                                     (setf (current-window) window)
                                     (lem-lisp-mode:lisp-inspect
                                      (format nil
                                              "(micros:find-class-from-string ~S)"
                                              (node-value node))
                                      :self-evaluation nil
                                      :focus t)))
                 :children (mapcar (lambda (node)
                                     (make-class-tree node buffer-name))
                                   (rest tree))))

(defmethod lem-lisp-mode/class-browser:display-class-inheritance-tree (buffer-name class-name)
  (let ((tree (lem-lisp-mode:lisp-eval-from-string
               (format nil
                       "(micros:compute-class-inheritance-tree ~S ~S)"
                       class-name
                       (lem-lisp-mode:current-package)))))
    (unless tree
      (editor-error "There is no class named ~:@(~A~)" class-name))
    (pop-to-buffer (draw-tree buffer-name (make-class-tree tree buffer-name)))))
