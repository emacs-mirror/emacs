(defpackage :lem-sdl2/text-surface-cache
  (:use :cl)
  (:export :clear-text-surface-cache
           :get-text-surface-cache
           :register-text-surface-cache))
(in-package :lem-sdl2/text-surface-cache)

(defvar *text-surface-cache* (make-hash-table :test 'equal))

(defstruct cache-entry
  type
  attribute
  surface)

(defun clear-text-surface-cache ()
  (clrhash *text-surface-cache*))

(defun get-text-surface-cache (string attribute type)
  (dolist (entry (gethash string *text-surface-cache*))
    (when (and (lem-core:attribute-equal attribute (cache-entry-attribute entry))
               (eq type (cache-entry-type entry)))
      (return (cache-entry-surface entry)))))

(defun register-text-surface-cache (string attribute type surface)
  (push (make-cache-entry
         :type type
         :attribute attribute
         :surface surface)
        (gethash string *text-surface-cache*)))
