(uiop:define-package :lem
  (:use :cl)
  (:use-reexport :lem-core)
  (:use-reexport :lem-core/commands/multiple-cursors)
  (:use-reexport :lem-core/commands/move)
  (:use-reexport :lem-core/commands/edit)
  (:use-reexport :lem-core/commands/mark)
  (:use-reexport :lem-core/commands/word)
  (:use-reexport :lem-core/commands/s-expression)
  (:use-reexport :lem-core/commands/file)
  (:use-reexport :lem-core/commands/window)
  (:use-reexport :lem-core/commands/buffer)
  (:use-reexport :lem-core/commands/process)
  (:use-reexport :lem-core/commands/help)
  (:use-reexport :lem-core/commands/font)
  (:use-reexport :lem-core/commands/other)
  (:use-reexport :lem-core/commands/frame))
#+sbcl
(sb-ext:lock-package :lem)

(defpackage :lem-user
  (:use :cl :lem))
