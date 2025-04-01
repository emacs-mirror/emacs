(defpackage :lem/hover
  (:use :cl :lem)
  (:export :show-hover))
(in-package :lem/hover)

(defvar *hover-window* nil)

(defun clear-hover-window ()
  (delete-popup-message *hover-window*)
  (setf *hover-window* nil))

(defun clear-hover-window-if-unnecessary ()
  (when (and (not (eq (current-window) *hover-window*))
             (or (typep (this-command) 'movable-advice)
                 (typep (this-command) 'editable-advice)
                 (typep (this-command) 'keyboard-quit)
                 (typep (this-command) 'escape)))
    (clear-hover-window)))

(add-hook *pre-command-hook* 'clear-hover-window-if-unnecessary)

(defun show-hover (buffer-or-string)
  (when *hover-window*
    (clear-hover-window))
  (setf *hover-window*
        (display-popup-message buffer-or-string
                               :timeout nil
                               :style '(:gravity :cursor)))
  (setf (floating-window-focusable-p *hover-window*) t)
  (values))
