(uiop:define-package :lem-scheme-syntax
  (:use :cl)
  (:use-reexport
   :lem-scheme-syntax.data
   :lem-scheme-syntax.indent
   :lem-scheme-syntax.syntax-table
   :lem-scheme-syntax.misc
   :lem-scheme-syntax.parse))
