(defpackage :lem-sdl2/utils
  (:use :cl)
  (:export :create-texture
           :render-texture))
(in-package :lem-sdl2/utils)

(defun create-texture (renderer width height)
  (sdl2:create-texture renderer
                       sdl2:+pixelformat-rgba8888+
                       sdl2-ffi:+sdl-textureaccess-target+
                       width
                       height))

(defun render-texture (renderer texture x y width height)
  (sdl2:with-rects ((dest-rect x y width height))
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect nil
                         :dest-rect dest-rect
                         :flip (list :none))))
