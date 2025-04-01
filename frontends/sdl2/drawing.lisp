(defpackage :lem-sdl2/drawing
  (:use :cl
        :lem-core/display)
  (:local-nicknames (:display :lem-sdl2/display)
                    (:view :lem-sdl2/view)))
(in-package :lem-sdl2/drawing)

(defgeneric get-surface (drawing-object display))

(defmethod get-surface :around (drawing-object display)
  (or (text-object-surface drawing-object)
      (setf (text-object-surface drawing-object)
            (call-next-method))))

(defun make-text-surface (display string attribute type)
  (cffi:with-foreign-string (c-string string)
    (let ((foreground (lem-core:attribute-foreground-with-reverse attribute)))
      (when (lem:cursor-attribute-p attribute)
        (when (eq :box (display:display-cursor-type display))
          (setf foreground (lem-if:get-background-color (lem:implementation)))))
      (sdl2-ttf:render-utf8-blended
       (or (lem-core:attribute-font attribute)
           (display:get-display-font display
                                     :type type
                                     :bold (and attribute (lem:attribute-bold attribute))))
       c-string
       (lem:color-red foreground)
       (lem:color-green foreground)
       (lem:color-blue foreground)
       0))))

(defun make-text-surface-with-cache (display string attribute type)
  (or (lem-sdl2/text-surface-cache:get-text-surface-cache string attribute type)
      (let ((surface (make-text-surface display string attribute type)))
        (lem-sdl2/text-surface-cache:register-text-surface-cache string attribute type surface)
        surface)))

(defmethod get-surface ((drawing-object text-object) display)
  (let ((string (text-object-string drawing-object))
        (attribute (text-object-attribute drawing-object))
        (type (text-object-type drawing-object)))
    (make-text-surface-with-cache display string attribute type)))

(defmethod get-surface ((drawing-object icon-object) display)
  (let* ((string (text-object-string drawing-object))
         (attribute (text-object-attribute drawing-object))
         (font (lem-sdl2/icon-font:icon-font
                (char (text-object-string drawing-object) 0)
                (lem-sdl2/font:font-config-size
                 (display:display-font-config display))))
         (foreground (lem-core:attribute-foreground-with-reverse attribute)))
    (cffi:with-foreign-string (c-string string)
      (sdl2-ttf:render-utf8-blended font
                                    c-string
                                    (lem:color-red foreground)
                                    (lem:color-green foreground)
                                    (lem:color-blue foreground)
                                    0))))

(defmethod get-surface ((drawing-object folder-object) display)
  (sdl2-image:load-image
   (lem-sdl2/resource:get-resource-pathname
    "resources/open-folder.png")))

(defgeneric object-width (drawing-object display))

(defmethod object-width ((drawing-object void-object) display)
  0)

(defmethod object-width ((drawing-object text-object) display)
  (sdl2:surface-width (get-surface drawing-object display)))

(defmethod object-width ((drawing-object control-character-object) display)
  (* 2 (display:display-char-width display)))

(defmethod object-width ((drawing-object icon-object) display)
  (sdl2:surface-width (get-surface drawing-object display)))

(defmethod object-width ((drawing-object folder-object) display)
  (* 2 (display:display-char-width display)))

(defmethod object-width ((drawing-object emoji-object) display)
  (* (display:display-char-width display) 2 (length (text-object-string drawing-object))))

(defmethod object-width ((drawing-object eol-cursor-object) display)
  0)

(defmethod object-width ((drawing-object extend-to-eol-object) display)
  0)

(defmethod object-width ((drawing-object line-end-object) display)
  (sdl2:surface-width (get-surface drawing-object display)))

(defmethod object-width ((drawing-object image-object) display)
  (or (image-object-width drawing-object)
      (sdl2:surface-width (image-object-image drawing-object))))


(defgeneric object-height (drawing-object display))

(defmethod object-height ((drawing-object void-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object text-object) display)
  (sdl2:surface-height (get-surface drawing-object display)))

(defmethod object-height ((drawing-object icon-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object control-character-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object folder-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object emoji-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object eol-cursor-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object extend-to-eol-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object line-end-object) display)
  (display:display-char-height display))

(defmethod object-height ((drawing-object image-object) display)
  (or (image-object-height drawing-object)
      (sdl2:surface-height (image-object-image drawing-object))))

(defmethod lem-if:object-width ((implementation lem-sdl2/sdl2:sdl2) drawing-object)
  (display:with-display (display)
    (object-width drawing-object display)))

(defmethod lem-if:object-height ((implementation lem-sdl2/sdl2:sdl2) drawing-object)
  (display:with-display (display)
    (object-height drawing-object display)))

(defmethod draw-object ((drawing-object void-object) x bottom-y display view)
  0)

(defun draw-rect (display x y width height color)
  (sdl2:with-rects ((rect x y width height))
    (display:set-render-color display color)
    (sdl2:render-fill-rect (display:display-renderer display) rect)))

(defun draw-cursor (display x y surface-width surface-height background)
  (ecase (display:display-cursor-type display)
    (:box
     (draw-rect display x y surface-width surface-height background))
    (:bar
     (draw-rect display x y 1 surface-height background))
    (:underline
     (draw-rect display x (+ y surface-height -1) surface-width 1 background))))

(defmethod draw-object ((drawing-object text-object) x bottom-y display view)
  (let* ((surface-width (object-width drawing-object display))
         (surface-height (object-height drawing-object display))
         (attribute (text-object-attribute drawing-object))
         (background (lem-core:attribute-background-with-reverse attribute))
         (texture (sdl2:create-texture-from-surface
                   (display:display-renderer display)
                   (get-surface drawing-object display)))
         (y (- bottom-y surface-height)))
    (cond ((and attribute (lem-core:cursor-attribute-p attribute))
           (lem-sdl2/view:set-cursor-position view x y)
           (draw-cursor display x y surface-width surface-height background))
          (t
           (draw-rect display x y surface-width surface-height background)))
    (lem-sdl2/utils:render-texture (display:display-renderer display)
                                   texture
                                   x
                                   y
                                   surface-width
                                   surface-height)
    (sdl2:destroy-texture texture)
    (when (and attribute
               (lem:attribute-underline attribute))
      (display:render-line display
                           x
                           (1- (+ y surface-height))
                           (+ x surface-width)
                           (1- (+ y surface-height))
                           :color (let ((underline (lem:attribute-underline attribute)))
                                    (if (eq underline t)
                                        (lem-core:attribute-foreground-color attribute)
                                        (or (lem:parse-color underline)
                                            (lem-core:attribute-foreground-color attribute))))))
    surface-width))

(defmethod draw-object ((drawing-object eol-cursor-object) x bottom-y display view)
  (display:set-render-color display (eol-cursor-object-color drawing-object))
  (let ((y (- bottom-y (object-height drawing-object display))))
    (lem-sdl2/view:set-cursor-position view x y)
    (draw-cursor display
                 x
                 y
                 (display:display-char-width display)
                 (object-height drawing-object display)
                 (eol-cursor-object-color drawing-object)))
  (object-width drawing-object display))

(defmethod draw-object ((drawing-object extend-to-eol-object) x bottom-y display view)
  (display:set-render-color display (extend-to-eol-object-color drawing-object))
  (sdl2:with-rects ((rect x
                          (- bottom-y (display:display-char-height display))
                          (- (lem-if:view-width (lem-core:implementation) view) x)
                          (display:display-char-height display)))
    (sdl2:render-fill-rect (display:display-renderer display) rect))
  (object-width drawing-object display))

(defmethod draw-object ((drawing-object line-end-object) x bottom-y display view)
  (call-next-method drawing-object
                    (+ x
                       (* (line-end-object-offset drawing-object)
                          (display:display-char-width display)))
                    bottom-y
                    display
                    view))

(defmethod draw-object ((drawing-object image-object) x bottom-y display view)
  (let* ((surface-width (object-width drawing-object display))
         (surface-height (object-height drawing-object display))
         (texture (sdl2:create-texture-from-surface (display:display-renderer display)
                                                    (image-object-image drawing-object)))
         (y (- bottom-y surface-height)))
    (lem-sdl2/utils:render-texture (display:display-renderer display)
                                   texture
                                   x
                                   y
                                   surface-width
                                   surface-height)
    (sdl2:destroy-texture texture)
    surface-width))

(defun redraw-physical-line (display view x y objects height)
  (let ((display-width (round (* (display:display-window-width display)
                                 (first (display:display-scale display))))))
    (loop :with current-x := x
          :for object :in objects
          :do (if (and (typep object 'text-object)
                       (< display-width
                          (+ current-x (object-width object display))))
                  (loop :for c :across (text-object-string object)
                        :do (let ((object (lem-core::make-letter-object c (text-object-attribute object))))
                              (incf current-x (draw-object object current-x (+ y height) display view)))
                        :while (< current-x display-width))
                  (incf current-x (draw-object object current-x (+ y height) display view))))))

(defun redraw-physical-line-from-behind (display view objects)
  (loop :with current-x := (lem-if:view-width (lem-core:implementation) view)
        :and y := (lem-if:view-height (lem-core:implementation) view)
        :for object :in objects
        :do (decf current-x (object-width object display))
            (draw-object object current-x y display view)))

(defun fill-to-end-of-line (display view x y height &optional default-attribute)
  (sdl2:with-rects ((rect x y (- (lem-if:view-width (lem-core:implementation) view) x) height))
    (display:set-render-color display
                              (lem-core:attribute-background-color default-attribute))
    (sdl2:render-fill-rect (display:display-renderer display) rect)))

(defmethod lem-if:render-line ((implementation lem-sdl2/sdl2:sdl2) view x y objects height)
  (display:with-display (display)
    (fill-to-end-of-line display view x y height)
    (redraw-physical-line display view x y objects height)))

(defmethod lem-if:render-line-on-modeline ((implementation lem-sdl2/sdl2:sdl2)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  (display:with-display (display)
    (fill-to-end-of-line display
                         view
                         0
                         (- (lem-if:view-height (lem-core:implementation) view) height)
                         height
                         default-attribute)
    (redraw-physical-line display
                          view
                          0
                          (- (lem-if:view-height (lem-core:implementation) view)
                             (display:display-char-height display))
                          left-objects
                          height)
    (redraw-physical-line-from-behind display view right-objects)))

(defmethod lem-if:clear-to-end-of-window ((implementation lem-sdl2/sdl2:sdl2) view y)
  (display:with-display (display)
    (display:set-render-color display
                              (display:display-background-color display))
    (sdl2:with-rects ((rect 0
                            y
                            (lem-if:view-width implementation view)
                            (- (lem-if:view-height implementation view) y)))
      (sdl2:render-fill-rect (display:display-renderer display) rect))))

(defmethod lem-core:redraw-buffer :before ((implementation lem-sdl2/sdl2:sdl2) buffer window force)
  (display:with-display (display)
    (sdl2:set-render-target (display:display-renderer display)
                            (view:view-texture (lem:window-view window)))))

(defmethod lem-core:redraw-buffer :around ((implementation lem-sdl2/sdl2:sdl2) buffer window force)
  (sdl2:in-main-thread ()
    (call-next-method)))
