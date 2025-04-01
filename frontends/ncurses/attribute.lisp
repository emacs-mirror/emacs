(defpackage :lem-ncurses/attribute
  (:use :cl :lem)
  (:export :attribute-to-bits))
(in-package :lem-ncurses/attribute)

(defun underline-color (attribute)
  (cond ((eq t (attribute-underline attribute))
         nil)
        ((and (attribute-underline attribute)
              (parse-color (attribute-underline attribute)))
         (attribute-underline attribute))
        (t
         nil)))

(defun compute-attribute-value (attribute cursorp)
  (let* ((underline-color (underline-color attribute))
         (foreground (or underline-color (attribute-foreground attribute)))
         (background (or (attribute-background attribute)
                         lem-if:*background-color-of-drawing-window*))
         (bits (logior (lem-ncurses/term:get-color-pair foreground background)
                       0
                       (if (attribute-bold attribute)
                           charms/ll:a_bold
                           0)
                       (if (attribute-underline attribute)
                           charms/ll:a_underline
                           0)
                       (if (or cursorp (attribute-reverse attribute))
                           charms/ll:a_reverse
                           0))))
    bits))

(defun attribute-to-bits (attribute-or-name)
  (let* ((attribute (ensure-attribute attribute-or-name nil))
         (cursorp (or (eq attribute-or-name 'cursor)
                      (and attribute (lem-core:attribute-value attribute :cursor)))))
    (when (and lem-if:*background-color-of-drawing-window* (null attribute))
      (setf attribute (make-attribute :background lem-if:*background-color-of-drawing-window*)))
    (if (null attribute)
        0
        (cond ((get-attribute-cache
                attribute
                :background lem-if:*background-color-of-drawing-window*))
              (t
               (let ((bits (compute-attribute-value attribute cursorp)))
                 (setf (get-attribute-cache
                        attribute
                        :background lem-if:*background-color-of-drawing-window*)
                       bits)
                 bits))))))
