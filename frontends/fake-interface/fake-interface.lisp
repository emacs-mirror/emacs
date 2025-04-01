(defpackage :lem-fake-interface
  (:use :cl :lem)
  (:export :fake-interface
           :with-fake-interface
           :get-displayed-text))
(in-package :lem-fake-interface)

(defclass fake-interface (lem:implementation)
  ((foreground
    :initform nil
    :accessor fake-interface-foreground)
   (background
    :initform nil
    :accessor fake-interface-background)
   (display-width
    :initform 80
    :reader fake-interface-display-width)
   (display-height
    :initform 24
    :reader fake-interface-display-height))
  (:default-initargs
   :name :fake
   :redraw-after-modifying-floating-window t))

(defstruct view
  x
  y
  width
  height
  modeline
  lines)

(defun get-displayed-text (&optional (window (current-window)))
  (let ((lines (view-lines (screen-view (window-screen window)))))
    (with-output-to-string (out)
      (loop :for line :across lines
            :for line-string := (string-right-trim (string (code-char 0)) line)
            :do (fresh-line out)
                (write-string line-string out)))))

(defmethod lem-if:invoke ((implementation fake-interface) function)
  (funcall function))

(defmethod lem-if:get-background-color ((implementation fake-interface))
  (make-color 0 0 0))

(defmethod lem-if:update-foreground ((implementation fake-interface) color-name)
  (setf (fake-interface-foreground implementation) color-name))

(defmethod lem-if:update-background ((implementation fake-interface) color-name)
  (setf (fake-interface-background implementation) color-name))

(defmethod lem-if:display-width ((implementation fake-interface))
  (fake-interface-display-width implementation))

(defmethod lem-if:display-height ((implementation fake-interface))
  (fake-interface-display-height implementation))

(defmethod lem-if:make-view ((implementation fake-interface) window x y width height use-modeline)
  (make-view
   :x x
   :y y
   :width width
   :height height
   :modeline use-modeline
   :lines (let ((lines (make-array height)))
            (dotimes (i height)
              (setf (aref lines i)
                    (make-string width :initial-element (code-char 0))))
            lines)))

(defmethod lem-if:delete-view ((implementation fake-interface) view)
  nil)

(defmethod lem-if:clear ((implementation fake-interface) view)
  nil)

(defmethod lem-if:set-view-size ((implementation fake-interface) view width height)
  (setf (view-width view) width
        (view-height view) height))

(defmethod lem-if:set-view-pos ((implementation fake-interface) view x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod lem-if:update-display ((implementation fake-interface)))

(defmethod lem-if:view-width ((implementation fake-interface) view)
  (view-width view))

(defmethod lem-if:view-height ((implementation fake-interface) view)
  (view-height view))

(defmethod lem-if:object-width ((implementation fake-interface) object)
  1)

(defmethod lem-if:object-height ((implementation fake-interface) object)
  1)

(defmethod lem-if:render-line ((implementation fake-interface) view x y objects height)
  nil)

(defmethod lem-if:clear-to-end-of-window ((implementation fake-interface) view y)
  nil)

(defmethod lem-if:get-char-width ((implementation fake-interface))
  1)

(defmethod lem-if:get-char-height ((implementation fake-interface))
  1)

(defmethod lem-if:render-line-on-modeline ((implementation fake-interface)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  nil)

(defmacro with-fake-interface (() &body body)
  `(with-implementation (make-instance 'fake-interface)
     (setup-first-frame)
     ,@body))
