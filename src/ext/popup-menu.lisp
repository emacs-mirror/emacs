(defpackage :lem/popup-menu
  (:use :cl :lem)
  (:export :write-header
           :get-focus-item
           :apply-print-spec)
  #+sbcl
  (:lock t))
(in-package :lem/popup-menu)

(defclass popup-menu ()
  ((buffer
    :initarg :buffer
    :accessor popup-menu-buffer)
   (window
    :initarg :window
    :accessor popup-menu-window)
   (focus-overlay
    :initarg :focus-overlay
    :accessor popup-menu-focus-overlay)
   (action-callback
    :initarg :action-callback
    :accessor popup-menu-action-callback)
   (focus-attribute
    :initarg :focus-attribute
    :accessor popup-menu-focus-attribute)))

(define-attribute popup-menu-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-popup-menu-attribute)

(defun focus-point (popup-menu)
  (buffer-point (popup-menu-buffer popup-menu)))

(defun make-focus-overlay (point focus-attribute)
  (make-line-overlay point focus-attribute))

(defun update-focus-overlay (popup-menu point)
  (delete-overlay (popup-menu-focus-overlay popup-menu))
  (setf (popup-menu-focus-overlay popup-menu)
        (make-focus-overlay point (popup-menu-focus-attribute popup-menu))))

(defgeneric write-header (print-spec point)
  (:method (print-spec point)))

(defgeneric apply-print-spec (print-spec point item)
  (:method ((print-spec function) point item)
    (let ((string (funcall print-spec item)))
      (insert-string point string))))

(defun insert-items (point items print-spec)
  (with-point ((start point :right-inserting))
    (loop :for (item . continue-p) :on items
          :do (move-point start point)
              (apply-print-spec print-spec point item)
              (line-end point)
              (put-text-property start point :item item)
              (when continue-p
                (insert-character point #\newline)))
    (buffer-start point)))

(defun get-focus-item (popup-menu)
  (alexandria:when-let (p (focus-point popup-menu))
    (text-property-at (line-start p) :item)))

(defun make-menu-buffer ()
  (make-buffer "*popup menu*" :enable-undo-p nil :temporary t))

(defun buffer-start-line (buffer)
  (buffer-value buffer 'start-line))

(defun (setf buffer-start-line) (line buffer)
  (setf (buffer-value buffer 'start-line) line))

(defun setup-menu-buffer (buffer items print-spec focus-attribute &optional last-line)
  (clear-overlays buffer)
  (erase-buffer buffer)
  (setf (variable-value 'line-wrap :buffer buffer) nil)
  (let ((point (buffer-point buffer)))
    (write-header print-spec point)
    (let ((header-exists (not (start-line-p point)))
          (start-line 1))
      (when header-exists
        (insert-character point #\newline)
        (setf start-line (line-number-at-point point)))
      (setf (buffer-start-line buffer) start-line)
      (insert-items point items print-spec)
      (buffer-start point)
      (when header-exists
        (move-to-line point start-line))
      (when last-line (move-to-line point last-line))
      (let ((focus-overlay (make-focus-overlay point focus-attribute))
            (width (lem/popup-window::compute-buffer-width buffer)))
        (values width
                focus-overlay
                (+ (1- start-line)
                   (length items)))))))

(defparameter *style* '(:use-border t :offset-y 0))

(defmethod lem-if:display-popup-menu (implementation items
                                      &key action-callback
                                           print-spec
                                           (style *style*)
                                           (max-display-items 20))
  (let ((style (lem/popup-window::ensure-style style))
        (focus-attribute (ensure-attribute 'popup-menu-attribute))
        (non-focus-attribute (ensure-attribute 'non-focus-popup-menu-attribute))
        (buffer (make-menu-buffer)))
    (multiple-value-bind (menu-width focus-overlay height)
        (setup-menu-buffer buffer
                           items
                           print-spec
                           focus-attribute)
      (let ((window (lem/popup-window::make-popup-window
                     :source-window (current-window)
                     :buffer buffer
                     :width menu-width
                     :height (min max-display-items height)
                     :style (lem/popup-window::merge-style
                             style
                             :background-color (or (lem/popup-window::style-background-color style)
                                                   (attribute-background
                                                    non-focus-attribute))
                             :cursor-invisible t))))
        (make-instance 'popup-menu
                       :buffer buffer
                       :window window
                       :focus-overlay focus-overlay
                       :action-callback action-callback
                       :focus-attribute focus-attribute)))))

(defmethod lem-if:popup-menu-update (implementation popup-menu items &key print-spec (max-display-items 20) keep-focus)
  (when popup-menu
    (let ((last-line (line-number-at-point (buffer-point (popup-menu-buffer popup-menu)))))
      (multiple-value-bind (menu-width focus-overlay height)
          (setup-menu-buffer (popup-menu-buffer popup-menu)
                             items
                             print-spec
                             (popup-menu-focus-attribute popup-menu)
                             (if keep-focus last-line))
        (setf (popup-menu-focus-overlay popup-menu) focus-overlay)
        (let ((source-window (current-window)))
          (when (eq source-window
                    (frame-prompt-window (current-frame)))
            ;; prompt-window内でcompletion-windowを出している場合,
            ;; completion-windowの位置を決める前にprompt-windowの調整を先にしておかないとずれるため,
            ;; ここで更新する
            (lem-core::update-floating-prompt-window (current-frame)))
          (lem/popup-window::update-popup-window :source-window source-window
                                                 :width menu-width
                                                 :height (min max-display-items height)
                                                 :destination-window (popup-menu-window popup-menu)))))))

(defmethod lem-if:popup-menu-quit (implementation popup-menu)
  (delete-window (popup-menu-window popup-menu))
  (delete-buffer (popup-menu-buffer popup-menu)))

(defun header-point-p (point)
  (< (line-number-at-point point)
     (buffer-start-line (point-buffer point))))

(defun move-focus (popup-menu function)
  (alexandria:when-let (point (focus-point popup-menu))
    (funcall function point)
    (line-start point)
    (window-see (popup-menu-window popup-menu))
    (let ((buffer (point-buffer point)))
      (when (header-point-p point)
        (move-to-line point (buffer-start-line buffer))))
    (update-focus-overlay popup-menu point)))

(defmethod lem-if:popup-menu-down (implementation popup-menu)
  (move-focus
   popup-menu
   (lambda (point)
     (unless (line-offset point 1)
       (buffer-start point)))))

(defmethod lem-if:popup-menu-up (implementation popup-menu)
  (move-focus
   popup-menu
   (lambda (point)
     (unless (line-offset point -1)
       (buffer-end point))
     (when (header-point-p point)
       (buffer-end point)))))

(defmethod lem-if:popup-menu-first (implementation popup-menu)
  (move-focus
   popup-menu
   (lambda (point)
     (buffer-start point))))

(defmethod lem-if:popup-menu-last (implementation popup-menu)
  (move-focus
   popup-menu
   (lambda (point)
     (buffer-end point))))

(defmethod lem-if:popup-menu-select (implementation popup-menu)
  (alexandria:when-let ((f (popup-menu-action-callback popup-menu))
                        (item (get-focus-item popup-menu)))
    (funcall f item)))
