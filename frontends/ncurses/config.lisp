(defpackage :lem-ncurses/config
  (:use :cl
        :lem)
  (:export :escape-delay))
(in-package :lem-ncurses/config)

;; escape key delay setting
(define-editor-variable escape-delay 100)
