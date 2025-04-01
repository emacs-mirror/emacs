(defpackage :lem-ncurses/drawing-object
  (:use :cl
        :lem-core/display)
  (:export :object-width
           :object-height))
(in-package :lem-ncurses/drawing-object)

(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object void-object))
  0)

(defmethod object-width ((drawing-object text-object))
  (lem-core:string-width (text-object-string drawing-object)))

(defmethod object-width ((drawing-object eol-cursor-object))
  0)

(defmethod object-width ((drawing-object extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object line-end-object))
  (lem-core:string-width (text-object-string drawing-object)))

(defmethod object-width ((drawing-object image-object))
  0)

(defmethod object-height (drawing-object)
  1)
