(defpackage :lem/show-paren
  (:use :cl
        :alexandria
        :lem)
  (:export :enable
           :showparen-attribute
           :forward-matching-paren
           :backward-matching-paren)
  #+sbcl
  (:lock t))
(in-package :lem/show-paren)

(defvar *brackets-overlays* '())

(define-attribute showparen-attribute
  (t :background "darkcyan" :foreground "white"))

(define-editor-variable forward-matching-paren 'forward-matching-paren-default)
(define-editor-variable backward-matching-paren 'backward-matching-paren-default)
(define-editor-variable enable t)

(defun forward-matching-paren-default (window point)
  (when (syntax-open-paren-char-p (character-at point))
    (with-point ((limit (window-view-point window)))
      (unless (line-offset limit (window-height window))
        (buffer-end limit))
      (when-let ((goal-point (scan-lists (copy-point point :temporary) 1 0 t limit)))
        (character-offset goal-point -1)))))

(defun backward-matching-paren-default (window point &optional (offset -1))
  (when (syntax-closed-paren-char-p (character-at point offset))
    (scan-lists (character-offset (copy-point point :temporary) (1+ offset))
                -1 0 t (window-view-point window))))

(defun show-paren-at-point (window current-point &optional (offset -1))
  (let ((highlight-points '()))
    (or (when-let ((point (funcall (variable-value 'backward-matching-paren) window current-point offset)))
          (push (copy-point point :temporary) highlight-points)
          (when-let ((point (funcall (variable-value 'forward-matching-paren) window point)))
            (push (copy-point point :temporary) highlight-points)))
        (when-let ((point (funcall (variable-value 'forward-matching-paren) window current-point)))
          (push (copy-point current-point :temporary) highlight-points)
          (push (copy-point point :temporary) highlight-points)))
    highlight-points))

(defun mouse-hover-highlight ()
  (multiple-value-bind (x y)
      (lem-if:get-mouse-position (implementation))
    (multiple-value-bind (window relative-x relative-y)
        (focus-window-position (current-frame) x y)
      (when window
        (let ((point (get-point-from-window-with-coordinates window relative-x relative-y)))
          (show-paren-at-point window point 0))))))

(defun update-show-paren ()
  (when (variable-value 'enable)
    (mapc #'delete-overlay *brackets-overlays*)
    (setq *brackets-overlays* nil)
    (let ((highlight-points (show-paren-at-point (current-window) (current-point))))
      (nconcf highlight-points (mouse-hover-highlight))
      (dolist (point highlight-points)
        (push (make-overlay point
                            (character-offset (copy-point point :temporary) 1)
                            'showparen-attribute)
              *brackets-overlays*)))))

(defvar *show-paren-timer* nil)

(define-command toggle-show-paren () ()
  (let ((enabled (not *show-paren-timer*)))
    (when (interactive-p)
      (message "show paren ~:[dis~;en~]abled." enabled))
    (cond (enabled
           (when *show-paren-timer*
             (stop-timer *show-paren-timer*))
           (setf *show-paren-timer*
                 (start-timer (make-idle-timer 'update-show-paren :name "show paren timer")
                              1
                              :repeat t))
           t)
          (t
           (when *show-paren-timer*
             (stop-timer *show-paren-timer*))
           (mapc #'delete-overlay *brackets-overlays*)
           (setq *brackets-overlays* nil)
           (setf *show-paren-timer* nil)))))

(unless *show-paren-timer*
  (toggle-show-paren))

(add-hook (variable-value 'mouse-button-down-functions :global) 'update-show-paren)
