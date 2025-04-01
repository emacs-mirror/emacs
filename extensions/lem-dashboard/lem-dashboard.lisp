(defpackage :lem-dashboard
  (:use :cl :lem)
  (:export :open-dashboard
           :*dashboard-mode-keymap*
           :*dashboard-enable*
           :dashboard-move-to-recent-projects
           :dashboard-move-to-recent-files
           :set-dashboard
           :set-default-dashboard
           :create-centered-string)
  (:local-nicknames (:button :lem/button)
                    (:history :lem/common/history)
                    (:project :lem-core/commands/project)
                    (:file :lem-core/commands/file)))

(in-package :lem-dashboard)

(defvar *dashboard-buffer-name* "*dashboard*")
(defvar *dashboard-enable* t)
(defvar *dashboard-mode-keymap* (make-keymap :name '*dashboard-mode-keymap* :parent *global-keymap*))
(defvar *dashboard-layout* nil
  "List of dashboard-item instances; will be drawn in order.")

(defun create-centered-string (str)
  "Creates a 'centered' string by padding with space to fill the screen width halfway."
  (let* ((padding (max 0 (floor (- (window-width (current-window)) (length str)) 2)))
         (spaces (make-string padding :initial-element #\Space)))
    (concatenate 'string spaces str)))

(defclass dashboard-item ()
  ((item-attribute 
    :initarg :item-attribute 
    :accessor item-attribute 
    :initform 'document-text-attribute
    :documentation "Attribute to use when drawing this item.")
   (top-margin
    :initarg :top-margin 
    :accessor top-margin
    :initform 0
    :documentation "The amount of vertical space (lines) to apply before the item.")
   (bottom-margin
    :initarg :bottom-margin 
    :accessor bottom-margin
    :initform 1
    :documentation "The amount of vertical space (lines) to apply after the item.")
   (action 
    :initarg :action 
    :accessor action 
    :initform nil
    :documentation "Function to execute when <return> is pressed over this item."))
  (:documentation "Base class for all dashboard items."))

(defgeneric draw-dashboard-item (item point)
  (:documentation "Called to draw the dashboard item.")
  (:method :before ((item dashboard-item) point)
    (dotimes (i (top-margin item))
      (insert-character point #\Newline)))
  (:method :after ((item dashboard-item) point)
    (dotimes (i (bottom-margin item))
      (insert-character point #\Newline))))

(define-command dashboard-open-selected-item () ()
  "Execute action on selected dashboard item."
  (let* ((point (current-point))
         (item (text-property-at point :dashboard-item)))
    (when (and item (action item))
      (funcall (action item)))))

(defmethod draw-dashboard-item :around ((item dashboard-item) point)
  "Inserts a :dashboard-item text property in between the starting and ending POINT, useful for tracking."
  (let ((start (copy-point point :temporary)))
    (call-next-method)
    (let ((end (copy-point point :temporary)))
      (put-text-property start end :dashboard-item item)
      (delete-point start)
      (delete-point end))))

(define-major-mode dashboard-mode ()
    (:name "Dashboard"
     :keymap *dashboard-mode-keymap*))

(defun create-dashboard-buffer ()
  "Creates the dashboard buffer."
  (or (get-buffer *dashboard-buffer-name*)
      (make-buffer *dashboard-buffer-name*
                   :enable-undo-p nil
                   :read-only-p t)))

(defun redraw-dashboard ()
  "Redraws the dashboard, clearing and redrawing all content while attempting to maintain point position."
  (let* ((buffer (create-dashboard-buffer))
         (old-line (line-number-at-point (buffer-point buffer)))
         (old-column (point-column (buffer-point buffer))))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (let ((point (buffer-point buffer)))
        (dolist (item *dashboard-layout*)
          (draw-dashboard-item item point)))
      (change-buffer-mode buffer 'dashboard-mode)
      (move-to-line (buffer-point buffer) old-line)
      (move-to-column (buffer-point buffer) old-column))))

(define-command open-dashboard () ()
  "Opens the dashboard if it doesn't exist, or switches to it if it does."
  (when *dashboard-enable*
    (if (get-buffer *dashboard-buffer-name*)
        (switch-to-buffer (get-buffer *dashboard-buffer-name*))
        (progn
          (redraw-dashboard)
          (switch-to-buffer (get-buffer *dashboard-buffer-name*))))))

(defun set-dashboard (dashboard-items)
  "Sets the new dashboard layout to DASHBOARD-ITEMS list and applies new keymap."
  (when dashboard-items
    (setf *dashboard-layout* dashboard-items)
    (when (get-buffer *dashboard-buffer-name*)
      (redraw-dashboard))))

(defun handle-resize (window)
  "Handle resizing; in this case, redraw the dashboard to keep it centered."
  (when (string= (buffer-name (window-buffer window)) *dashboard-buffer-name*)
    (redraw-dashboard)))

(define-key *dashboard-mode-keymap* "n" 'next-line)
(define-key *dashboard-mode-keymap* "p" 'previous-line)
(define-key *dashboard-mode-keymap* "j" 'next-line)
(define-key *dashboard-mode-keymap* "k" 'previous-line)
(define-key *dashboard-mode-keymap* "Return" 'dashboard-open-selected-item)

(setf lem:*splash-function* #'open-dashboard)
(add-hook *window-size-change-functions* 'handle-resize)
