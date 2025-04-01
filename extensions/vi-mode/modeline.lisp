(defpackage :lem-vi-mode/modeline
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :state-name
                :state-modeline-color
                :*default-cursor-color*
                :*enable-hook*
                :*disable-hook*
                :vi-current-window)
  (:import-from :lem-interface)
  (:export :state-modeline-white
           :state-modeline-yellow
           :state-modeline-green
           :state-modeline-aqua
           :state-modeline-orange
           :change-element-by-state))
(in-package :lem-vi-mode/modeline)

(defvar *modeline-element*)

(define-attribute state-modeline-white
  (t :foreground "white" :reverse t))

(define-attribute state-modeline-yellow
  (t :foreground "yellow" :reverse t))

(define-attribute state-modeline-green
  (t :foreground "#003300" :reverse t))

(define-attribute state-modeline-aqua
  (t :foreground "#33CCFF" :reverse t))

(define-attribute state-modeline-orange
  (t :foreground "orange" :reverse t))

(defstruct (vi-modeline-element (:conc-name element-))
  name
  attribute)

(defmethod lem:convert-modeline-element ((element vi-modeline-element) window)
  (if (and (element-name element)
           (eq (vi-current-window) window))
      (values (format nil " ~A " (element-name element))
              (element-attribute element))
      (values "" 'state-modeline-white)))

(defun initialize-vi-modeline ()
  (setf *modeline-element* (make-vi-modeline-element))
  (pushnew *modeline-element* (lem:variable-value 'lem:modeline-format :global)))

(defun finalize-vi-modeline ()
  (setf (lem:variable-value 'lem:modeline-format :global)
        (remove-if #'vi-modeline-element-p
                   (lem:variable-value 'lem:modeline-format :global)))
  (lem:modeline-remove-status-list *modeline-element*)
  (set-attribute 'cursor :background *default-cursor-color*)
  (lem-if:update-cursor-shape (lem:implementation) :box))

(defun change-element-by-state (state)
  (setf (element-name *modeline-element*) (state-name state)
        (element-attribute *modeline-element*) (state-modeline-color state)))

(add-hook *enable-hook* 'initialize-vi-modeline 10)
(add-hook *disable-hook* 'finalize-vi-modeline)
