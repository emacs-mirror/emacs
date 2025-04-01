(defpackage :lem-ncurses/render
  (:use :cl
        :lem-core/display)
  (:export :render-line
           :render-line-on-modeline
           :clear-to-end-of-window))
(in-package :lem-ncurses/render)

(defun print-string (scrwin x y string attribute)
  (let ((attr (lem-ncurses/attribute:attribute-to-bits attribute)))
    (charms/ll:wattron scrwin attr)
    (charms/ll:mvwaddstr scrwin y x string)
    (charms/ll:wattroff scrwin attr)))

(defgeneric draw-object (object x y view scrwin))

(defmethod draw-object ((object void-object) x y view scrwin)
  (values))

(defmethod draw-object ((object text-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (when (and attribute (lem:cursor-attribute-p attribute))
      (lem-ncurses/view:set-last-print-cursor view x y)
      (lem:set-attribute-foreground
       attribute
       (lem:color-to-hex-string (lem-if:get-background-color (lem:implementation)))))
    (print-string scrwin x y string attribute)))

(defmethod draw-object ((object eol-cursor-object) x y view scrwin)
  (when (eol-cursor-object-true-cursor-p object)
    (lem-ncurses/view:set-last-print-cursor view x y))
  (print-string
   scrwin
   x
   y
   " "
   (lem:make-attribute :foreground (lem:color-to-hex-string (eol-cursor-object-color object)))))

(defmethod draw-object ((object extend-to-eol-object) x y view scrwin)
  (let ((width (lem-if:view-width (lem:implementation) view)))
    (when (< x width)
      (print-string
       scrwin
       x
       y
       (make-string (- width x) :initial-element #\space)
       (lem:make-attribute :background
                           (lem:color-to-hex-string (extend-to-eol-object-color object)))))))

(defmethod draw-object ((object line-end-object) x y view scrwin)
  (let ((string (text-object-string object))
        (attribute (text-object-attribute object)))
    (print-string
     scrwin
     (+ x (line-end-object-offset object))
     y
     string
     attribute)))

(defmethod draw-object ((object image-object) x y view scrwin)
  (values))

(defun render-line-from-behind (view y objects scrwin)
  (loop :with current-x := (lem-if:view-width (lem:implementation) view)
        :for object :in objects
        :do (decf current-x (lem-ncurses/drawing-object:object-width object))
            (draw-object object current-x y view scrwin)))

(defun clear-line (view x y)
  (charms/ll:wmove (lem-ncurses/view:view-scrwin view) y x)
  (charms/ll:wclrtoeol (lem-ncurses/view:view-scrwin view)))

(defun %render-line (view x y objects scrwin)
  (loop :for object :in objects
        :do (draw-object object x y view scrwin)
            (incf x (lem-ncurses/drawing-object:object-width object))))

(defun render-line (view x y objects)
  (clear-line view x y)
  (%render-line view x y objects (lem-ncurses/view:view-scrwin view)))

(defun render-line-on-modeline (view
                                left-objects
                                right-objects
                                default-attribute)
  (print-string (lem-ncurses/view:view-modeline-scrwin view)
                0
                0
                (make-string (lem-ncurses/view:view-width view)
                             :initial-element #\space)
                default-attribute)
  (%render-line view 0 0 left-objects (lem-ncurses/view:view-modeline-scrwin view))
  (render-line-from-behind view
                           0
                           right-objects
                           (lem-ncurses/view:view-modeline-scrwin view)))

(defun clear-to-end-of-window (view y)
  (let ((win (lem-ncurses/view:view-scrwin view)))
    (when (< y (lem-ncurses/view:view-height view))
      (charms/ll:wmove win y 0)
      (charms/ll:wclrtobot win))))
