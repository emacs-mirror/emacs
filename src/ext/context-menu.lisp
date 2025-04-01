(defpackage :lem/context-menu
  (:use :cl
        :lem
        :lem/multi-column-list)
  (:export :context-menu
           :item
           :make-item
           :display-context-menu)
  #+sbcl
  (:lock t))
(in-package :lem/context-menu)

(defclass item (multi-column-list-item)
  ((label :initarg :label
          :reader item-label)
   (description :initarg :description
                :initform nil
                :reader item-description)
   (callback :initarg :callback
             :reader item-callback)))

(defun make-item (&key label description callback)
  (make-instance 'item
                 :label label
                 :description description
                 :callback callback))

(defun item-equal (item1 item2)
  (equal (item-label item1)
         (item-label item2)))

(defgeneric activate (context-menu)
  (:documentation "This generic-function is called just before the context menu is displayed.
It is intended for use in dynamically computing menu items."))

(defclass context-menu (multi-column-list)
  ((compute-items-function :initarg :compute-items-function
                           :initform nil
                           :reader context-menu-compute-items-function)
   (last-selected-item :initform nil
                       :accessor context-menu-last-selected-item))
  (:default-initargs :columns '()))

(defmethod activate ((context-menu context-menu))
  (when (context-menu-compute-items-function context-menu)
    (setf (lem/multi-column-list::multi-column-list-search-string context-menu) "")
    (setf (lem/multi-column-list::multi-column-list-items context-menu)
          (funcall (context-menu-compute-items-function context-menu)))))

(defmethod select-item ((component context-menu) item)
  (setf (context-menu-last-selected-item component) item)
  (let ((window (window-parent (lem/multi-column-list::multi-column-list-window component))))
    (quit component)
    (funcall (item-callback item) window)))

(defmethod map-columns ((component context-menu) (item item))
  (list (item-label item) (or (item-description item) "")))

(defun display-context-menu (items)
  (display (make-instance 'context-menu :items items)
           :style '(:gravity :cursor)))

(defun last-selected-index (context-menu)
  (or (alexandria:when-let (item (context-menu-last-selected-item context-menu))
        (position item (lem/multi-column-list::multi-column-list-items context-menu)
                  :test #'item-equal))
      0))

(defmethod lem-if:display-context-menu (implementation context-menu style)
  (activate context-menu)
  (when (lem/multi-column-list::multi-column-list-items context-menu)
    (display context-menu
             :style style
             :index (last-selected-index context-menu))))

(define-command test-context-menu () ()
  (display-context-menu (list (make-item :label "foo"
                                         :callback (lambda (window)
                                                     (declare (ignore window))
                                                     (message "select foo")))
                              (make-item :label "bar"
                                         :callback (lambda (window)
                                                     (declare (ignore window))
                                                     (message "select bar")))
                              (make-item :label "baz"
                                         :callback (lambda (window)
                                                     (declare (ignore window))
                                                     (message "select baz"))))))
