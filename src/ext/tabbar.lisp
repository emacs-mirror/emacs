(defpackage :lem/tabbar
  (:use :cl :lem :lem/button)
  (:export :tabbar-active-tab-attribute
           :tabbar-attribute
           :tabbar-background-attribute
           :tabbar)
  #+sbcl
  (:lock t))
(in-package :lem/tabbar)

(define-attribute tabbar-active-tab-attribute
  (t :foreground "black" :background "dark gray"))

(define-attribute tabbar-attribute
  (t :foreground "white" :background "gray"))

(define-attribute tabbar-background-attribute
  (t :underline t))

(defclass tabbar-window (header-window)
  ((buffer
    :initarg :buffer
    :accessor tabbar-buffer)
   (prev-buffer-list
    :initform '()
    :accessor tabbar-prev-buffer-list)
   (prev-current-buffer
    :initform nil
    :accessor tabbar-prev-current-buffer)
   (prev-display-width
    :initform 0
    :accessor tabbar-prev-display-width)))

(defvar *tabbar* nil)

(defun tabbar-init ()
  (let ((buffer (make-buffer "*tabbar*" :enable-undo-p nil :temporary t)))
    (when (display-light-p)
      (set-attribute-foreground 'tabbar-active-tab-attribute (foreground-color))
      (set-attribute-foreground 'tabbar-attribute (foreground-color)))
    (when (display-dark-p)
      (set-attribute-foreground 'tabbar-active-tab-attribute (foreground-color))
      (set-attribute-background 'tabbar-active-tab-attribute "light gray")
      (set-attribute-foreground 'tabbar-attribute (foreground-color)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (setf *tabbar* (make-instance 'tabbar-window :buffer buffer))))

(defun tabbar-require-update ()
  (block exit
    (unless (eq (current-buffer) (tabbar-prev-current-buffer *tabbar*))
      (return-from exit t))
    (unless (eq (buffer-list) (tabbar-prev-buffer-list *tabbar*))
      (return-from exit t))
    (unless (= (display-width) (tabbar-prev-display-width *tabbar*))
      (return-from exit t))
    nil))

(defmethod window-redraw ((window tabbar-window) force)
  (when (or force (tabbar-require-update))
    (let* ((buffer (tabbar-buffer *tabbar*))
           (p (buffer-point buffer))
           (charpos (point-charpos p)))
      (erase-buffer buffer)
      (dolist (buffer (buffer-list))
        (let ((focusp (eq buffer (current-buffer))))
          (let ((start-pos (point-charpos p)))
            (insert-button p
                           (let ((name (buffer-name buffer)))
                             (if (< 20 (length name))
                                 (format nil " ~A... " (subseq name 0 17))
                                 (format nil " ~A " name)))
                           (let ((buffer buffer))
                             (lambda () (switch-to-buffer buffer nil)))
                           :attribute (if focusp
                                          'tabbar-active-tab-attribute
                                          'tabbar-attribute))
            (when focusp
              (let ((end-pos (point-charpos p)))
                (unless (<= start-pos charpos (1- end-pos))
                  (setf charpos start-pos)))))))
      (let ((n (- (display-width) (point-column p))))
        (when (> n 0)
          (insert-string p (make-string n :initial-element #\space)
                         :attribute 'tabbar-background-attribute)))
      (line-offset p 0 charpos))
    (setf (tabbar-prev-buffer-list *tabbar*) (buffer-list))
    (setf (tabbar-prev-current-buffer *tabbar*) (current-buffer))
    (setf (tabbar-prev-display-width *tabbar*) (display-width))
    (call-next-method)))

(defun tabbar-clear-cache ()
  (setf (tabbar-buffer *tabbar*) nil)
  (setf (tabbar-prev-buffer-list *tabbar*) '())
  (setf (tabbar-prev-current-buffer *tabbar*) nil)
  (setf (tabbar-prev-display-width *tabbar*) 0))

(defun tabbar-off ()
  (when (and (variable-value 'tabbar :global)
             *tabbar*)
    (tabbar-clear-cache)
    (delete-window *tabbar*)
    (setf *tabbar* nil)))

(defun tabbar-on ()
  (unless (variable-value 'tabbar :global)
    (tabbar-init)))

(define-editor-variable tabbar nil ""
  (lambda (value)
    (if value
        (tabbar-on)
        (tabbar-off))))

(define-command toggle-tabbar () ()
  (setf (variable-value 'tabbar :global)
        (not (variable-value 'tabbar :global))))

;(define-key *global-keymap* "Shift-PageDown" 'tabbar-next)
;(define-key *global-keymap* "Shift-PageUp" 'tabbar-prev)

(define-command tabbar-next (n) (:universal)
  (let ((p (buffer-point (tabbar-buffer *tabbar*))))
    (dotimes (_ n)
      (forward-button p))
    (let ((button (button-at p)))
      (when button
        (move-point p (button-end button))
        (character-offset p -1)
        (button-action button)))))

(define-command tabbar-prev (n) (:universal)
  (let ((p (buffer-point (tabbar-buffer *tabbar*))))
    (dotimes (_ n)
      (backward-button p))
    (let ((button (button-at p)))
      (when button
        (button-action button)))))

(add-hook *after-init-hook*
          (lambda ()
            (when (variable-value 'tabbar :global)
              (tabbar-init))))
