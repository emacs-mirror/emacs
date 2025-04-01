(defpackage :lem-markdown-mode/internal
  (:use :cl)
  (:export :on-save
           :on-kill
           :on-change
           :preview
           :on-save-default
           :on-kill-default
           :on-change-default
           :preview-default))
(in-package :lem-markdown-mode/internal)

(defparameter *view-type* :external-browser)

(defgeneric on-save (buffer view-type))
(defgeneric on-kill (buffer view-type))
(defgeneric on-change (buffer view-type))
(defgeneric preview (buffer view-type))

(defun on-save-default (buffer)
  (on-save buffer *view-type*))

(defun on-kill-default (buffer)
  (on-kill buffer *view-type*))

(defun on-change-default (buffer)
  (on-change buffer *view-type*))

(defun preview-default (buffer)
  (preview buffer *view-type*))
