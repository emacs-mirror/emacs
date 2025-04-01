(defpackage :lem-sdl2/sdl2
  (:use :cl)
  (:export :sdl2))
(in-package :lem-sdl2/sdl2)

(defclass sdl2 (lem:implementation)
  ()
  (:default-initargs
   :name :sdl2
   :redraw-after-modifying-floating-window nil))

(setf (lem-core:variable-value 'lem-core:highlight-line :global) t)

(pushnew :lem-sdl2 *features*)
