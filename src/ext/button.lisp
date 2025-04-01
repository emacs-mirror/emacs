(defpackage :lem/button
  (:use :cl :lem)
  (:export :with-context
           :button
           :button-start
           :button-end
           :button-get
           :insert-button
           :apply-button-between-points
           :button-action
           :forward-button
           :backward-button
           :button-at)
  #+sbcl
  (:lock t))
(in-package :lem/button)

(defstruct button
  start
  end
  plist
  callback)

(defun button-get (button key)
  (getf (button-plist button) key))

(defun (setf button-get) (value button key)
  (setf (getf (button-plist button) key) value))

(defstruct context
  window
  point)

(defvar *context* nil)

(defun call-with-context (function)
  (if *context*
      (with-current-window (context-window *context*)
        (save-excursion
          (setf (current-buffer) (point-buffer (context-point *context*)))
          (move-point (current-point) (context-point *context*))
          (funcall function)))
      (funcall function)))

(defmacro with-context (() &body body)
  `(call-with-context (lambda () ,@body)))

(defun with-callback (callback)
  (lambda (window point)
    (let ((*context* (make-context :window window :point point)))
      (funcall callback))))

(defun insert-button (point text &optional callback &rest plist)
  (let ((button-tag (getf plist :button-tag))
        (without-mouse-hover-highlight (getf plist :without-mouse-hover-highlight)))
    (flet ((f ()
             (apply #'insert-string
                    point text
                    'button (make-button :plist plist :callback callback)
                    'action callback
                    :click-callback (with-callback callback)
                    :attribute (getf plist :attribute)
                    (if button-tag
                        `(,button-tag t)
                        '()))))
      (if without-mouse-hover-highlight
          (f)
          (with-point ((start point :right-inserting)
                       (end point :left-inserting))
            (prog1 (f)
              (lem-core::set-clickable start
                                       end
                                       (with-callback callback))))))))

(defun apply-button-between-points (start end callback)
  (lem-core::set-clickable start
                           end
                           (with-callback callback))
  (put-text-property start end 'button (make-button :callback callback)))

(defun button-action (button)
  (funcall (button-callback button)))

(defun forward-button (point &optional limit)
  (let ((button (button-at point)))
    (cond (button
           (move-point point (button-end button))
           (if (text-property-at point 'button)
               point
               (next-single-property-change point 'button limit)))
          (t
           (next-single-property-change point 'button limit)))))

(defun backward-button (point &optional limit)
  (let ((button (button-at point)))
    (when button
      (move-point point (button-start button))))
  (previous-single-property-change point 'button limit))

(defun move-to-button-start (point)
  (let ((following (text-property-at point 'button -1))
        (preceding (text-property-at point 'button 0)))
    (cond ((and preceding
                (eq following preceding))
           (previous-single-property-change point 'button))
          (preceding
           point))))

(defun move-to-button-end (point)
  (when (text-property-at point 'button)
    (next-single-property-change point 'button)))

(defun button-at (point)
  (with-point ((point point))
    (let ((button (or (text-property-at point 'button)
                      (when (character-offset point -1)
                        (text-property-at point 'button)))))
      (when button
        (with-point ((start point)
                     (end point))
          (setf (button-start button)
                (move-to-button-start start))
          (setf (button-end button)
                (or (move-to-button-end end) (buffer-end point)))
          button)))))
