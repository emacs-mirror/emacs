(defpackage :lem/filer
  (:use :cl :lem))
(in-package :lem/filer)

(define-attribute triangle-attribute
  (t :bold t :foreground :base0D))

(define-major-mode filer-mode ()
    (:name "Filer"
     :keymap *filer-mode-keymap*)
  (setf (variable-value 'line-wrap :buffer (current-buffer)) nil)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *global-keymap* "C-x d" 'filer)
(define-key *filer-mode-keymap* "Return" 'filer-select)

(defclass item ()
  ((pathname :initarg :pathname
             :reader item-pathname)))

(defclass file-item (item) ())

(defclass directory-item (item)
  ((open :initarg :open
         :initform nil
         :accessor directory-item-open-p)
   (children :initarg :children
             :initform nil
             :accessor directory-item-children)))

(defgeneric item-content (item)
  (:method ((item file-item))
    (enough-namestring (item-pathname item)
                       (uiop:pathname-directory-pathname (item-pathname item))))
  (:method ((item directory-item))
    (enough-namestring (item-pathname item)
                       (uiop:pathname-parent-directory-pathname (item-pathname item)))))

(defun create-item (pathname &key open)
  (if (uiop:directory-pathname-p pathname)
      (create-directory-item pathname :open open)
      (create-file-item pathname)))

(defun create-file-item (pathname)
  (make-instance 'file-item :pathname pathname))

(defun create-directory-children (pathname)
  (loop :for pathname :in (list-directory pathname)
        :collect (create-item pathname :open nil)))

(defun create-directory-item (pathname &key open)
  (if open
      (make-instance 'directory-item
                     :pathname pathname
                     :open t
                     :children (create-directory-children pathname))
      (make-instance 'directory-item
                     :pathname pathname
                     :open nil
                     :children '())))

(defmethod open-item ((item file-item) buffer)
  (when (typep (current-window) 'lem-core:side-window)
    (next-window))
  (find-file (item-pathname item)))

(defmethod open-item ((item directory-item) buffer)
  (cond ((directory-item-open-p item)
         (setf (directory-item-open-p item) nil)
         (setf (directory-item-children item) '()))
        (t
         (setf (directory-item-open-p item) t)
         (setf (directory-item-children item)
               (create-directory-children (item-pathname item)))))
  (render buffer (root-item buffer)))

(defun root-item (buffer)
  (buffer-value buffer 'root-item))

(defun (setf root-item) (item buffer)
  (setf (buffer-value buffer 'root-item) item))

(defun select (point)
  (let ((buffer (point-buffer point))
        (item (text-property-at point :item)))
    (when item
      (open-item item buffer))))

(defun insert-item (point item)
  (with-point ((start point))
    (back-to-indentation start)
    (lem/directory-mode/internal::insert-icon point (item-pathname item))
    (insert-string point
                   (item-content item)
                   :attribute (lem/directory-mode/internal::get-file-attribute (item-pathname item)))
    (put-text-property start point :item item)
    (lem-core::set-clickable start
                             point
                             (lambda (window point)
                               (declare (ignore window))
                               (select point)))))

(defmethod render-item :before (point item depth)
  (insert-string point (make-string (* depth 2) :initial-element #\space)))

(defmethod render-item (point (item file-item) depth)
  (insert-string point "  ")
  (insert-item point item))

(defmethod render-item (point (item directory-item) depth)
  (insert-string point
                 (uiop:strcat 
                  (if (directory-item-open-p item)
                      (icon-string "down-pointing-triangle")
                      (icon-string "right-pointing-triangle"))
                  " ")
                 :attribute 'triangle-attribute)
  (insert-item point item)
  (dolist (item (directory-item-children item))
    (insert-character point #\newline)
    (render-item point item (1+ depth))))

(defun call-with-fix-scroll (function)
  (let ((n (if (lem-core::frame-leftside-window (current-frame))
               (1- (line-number-at-point 
                    (window-view-point
                     (lem-core::frame-leftside-window (current-frame)))))
               0)))
    (funcall function)
    (when (lem-core::frame-leftside-window (current-frame))
      (scroll-down n (lem-core::frame-leftside-window (current-frame))))))

(defmacro with-fix-scroll (() &body body)
  `(call-with-fix-scroll (lambda () ,@body)))

(defun render (buffer item)
  (with-buffer-read-only buffer nil
    (with-fix-scroll ()
      (let ((line (line-number-at-point (buffer-point buffer))))
        (setf (root-item buffer) item)
        (erase-buffer buffer)
        (render-item (buffer-point buffer) item 0)
        (move-to-line (buffer-point buffer) line)
        (back-to-indentation (buffer-point buffer))))))

(defun make-filer-buffer (directory)
  (let* ((directory (probe-file directory))
         (buffer (make-buffer "*Filer*" :temporary t)))
    (change-buffer-mode buffer 'filer-mode)
    (render buffer (create-directory-item directory :open t))
    (setf (not-switchable-buffer-p buffer) t)
    buffer))

(defun filer-active-p ()
  (and (lem-core::frame-leftside-window (current-frame))
       (eq 'filer-mode
           (buffer-major-mode 
            (window-buffer
             (lem-core::frame-leftside-window
              (current-frame)))))))

(defun deactive-filer ()
  (when (eq (current-window) (lem-core::frame-leftside-window (current-frame)))
    (next-window))
  (delete-leftside-window))

(define-command filer () ()
  "Open the filer tree view at the project root."
  (if (filer-active-p)
      (deactive-filer)
      (let ((directory (lem-core/commands/project:find-root (buffer-directory))))
        (make-leftside-window (make-filer-buffer directory)))))

(define-command filer-directory () ()
  "Open the filer tree view at this directory."
  (if (filer-active-p)
      (deactive-filer)
      (let ((directory (buffer-directory)))
        (make-leftside-window (make-filer-buffer directory)))))

(define-command filer-at-directory () ()
  "Prompt for a directory and open the filer tree view at this directory."
  (let ((directory (prompt-for-directory "Directory: "
                                         :directory (buffer-directory)
                                         :gravity :cursor
                                         :use-border t)))
    (when (filer-active-p)
      (deactive-filer)
    (make-leftside-window (make-filer-buffer directory)))))

(define-command filer-select () ()
  (select (back-to-indentation (current-point))))
