(defpackage :lem-ncurses/style
  (:use :cl
        :lem)
  (:export :border-upleft
           :border-upright
           :border-downleft
           :border-downright
           :border-up
           :border-right
           :border-down
           :border-left
           :border-vertical-and-right
           :border-vertical-and-left
           :border-attribute))
(in-package :lem-ncurses/style)

(define-attribute space-border-color
  (:light :foreground "gray" :reverse t)
  (:dark :foreground "#666666" :reverse t))

(define-attribute ruled-border-color
  )

(defstruct style
  border-shapes
  border-attribute
  window-separator)

(defstruct (border-shapes (:type list))
  upleft
  up
  upright
  right
  downright
  down
  downleft
  left
  vertical-and-right
  vertical-and-left)

(defparameter *single-border-style*
  (make-style :border-shapes '("┌" "─" "┐" "│" "┘" "─" "└" "│" "├" "┤")
              :border-attribute 'ruled-border-color
              :window-separator "║"))

(defparameter *single-border-roundly-style*
  (make-style :border-shapes '("╭" "─" "╮" "│" "╯" "─" "╰" "│" "├" "┤")
              :border-attribute 'ruled-border-color
              :window-separator "║"))

(defparameter *double-border-style*
  (make-style :border-shapes '("╔" "═" "╗" "║" "╝" "═" "╚" "║" "╠" "╣")
              :border-attribute 'ruled-border-color
              :window-separator "║"))

(defparameter *space-border-style*
  (make-style :border-shapes (loop :repeat 8 :collect " ")
              :border-attribute 'space-border-color
              :window-separator " "))

(defparameter *style* *single-border-roundly-style*)

(defun border-upleft () (border-shapes-upleft (style-border-shapes *style*)))
(defun border-upright () (border-shapes-upright (style-border-shapes *style*)))
(defun border-downleft () (border-shapes-downleft (style-border-shapes *style*)))
(defun border-downright () (border-shapes-downright (style-border-shapes *style*)))
(defun border-up () (border-shapes-up (style-border-shapes *style*)))
(defun border-right () (border-shapes-right (style-border-shapes *style*)))
(defun border-down () (border-shapes-down (style-border-shapes *style*)))
(defun border-left () (border-shapes-left (style-border-shapes *style*)))
(defun border-vertical-and-right () (border-shapes-vertical-and-right (style-border-shapes *style*)))
(defun border-vertical-and-left () (border-shapes-vertical-and-left (style-border-shapes *style*)))
(defun border-attribute () (style-border-attribute *style*))
