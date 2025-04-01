(defpackage :lem-lsp-base/utils
  (:use :cl)
  (:import-from :quri)
  (:export :pathname-to-uri
           :uri-to-pathname
           :point-lsp-line-number
           :point-to-lsp-position
           :points-to-lsp-range
           :move-to-lsp-position
           :destructuring-lsp-range))
(in-package :lem-lsp-base/utils)

(defun pathname-to-uri (pathname)
  (format nil "file://~A" (namestring pathname)))

(defun uri-to-pathname (uri)
  (pathname (quri:uri-path (quri:uri uri))))

(defun point-lsp-line-number (point)
  (1- (lem:line-number-at-point point)))

(defun point-to-lsp-position (point)
  (make-instance 'lsp:position
                 :line (point-lsp-line-number point)
                 :character (lem:point-charpos point)))

(defun points-to-lsp-range (start end)
  (make-instance 'lsp:range
                 :start (point-to-lsp-position start)
                 :end (point-to-lsp-position end)))

(defun move-to-lsp-position (point position)
  (check-type point lem:point)
  (check-type position lsp:position)
  (let ((line (lsp:position-line position))
        (character (lsp:position-character position)))
    (lem:move-to-line point (1+ line))
    (lem:character-offset (lem:line-start point) character)
    point))

(defun destructuring-lsp-range (start end range)
  (move-to-lsp-position start (lsp:range-start range))
  (move-to-lsp-position end (lsp:range-end range))
  (values))
