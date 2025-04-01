(defpackage :lem-server/view
  (:use :cl)
  (:export :view
           :make-view
           :view-window
           :view-id
           :view-x
           :view-y
           :view-width
           :view-height
           :view-use-modeline
           :view-kind
           :move-view
           :resize-view))
(in-package :lem-server/view)

(defvar *view-id-counter* 0)

(deftype border-shape ()
  '(member nil :drop-curtain :left-border))

(defstruct (view (:constructor %make-view))
  (id (incf *view-id-counter*))
  window
  x
  y
  width
  height
  use-modeline
  kind ; "tile" / "floating"
  border
  (border-shape nil :type border-shape))

(defun make-view (&rest args &key window x y width height use-modeline kind border border-shape)
  (declare (ignore window x y width height use-modeline kind border border-shape))
  (apply #'%make-view args))

(defun move-view (view x y)
  (setf (view-x view) x
        (view-y view) y))

(defun resize-view (view width height)
  (setf (view-width view) width
        (view-height view) height)
  (values))

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view))
      (yason:encode-object-element "x" (view-x view))
      (yason:encode-object-element "y" (view-y view))
      (yason:encode-object-element "width" (view-width view))
      (yason:encode-object-element "height" (view-height view))
      (yason:encode-object-element "use_modeline" (view-use-modeline view))
      (yason:encode-object-element "kind" (view-kind view))
      (yason:encode-object-element "type"
                                   (let ((buffer (lem:window-buffer (view-window view))))
                                     (if (typep buffer 'lem:html-buffer)
                                         "html"
                                         "editor")))
      (yason:encode-object-element "content"
                                   (let ((buffer (lem:window-buffer (view-window view))))
                                     (when (typep buffer 'lem:html-buffer)
                                       (lem:html-buffer-html buffer))))
      (yason:encode-object-element "border" (view-border view))
      (yason:encode-object-element "border_shape" (if (view-border-shape view)
                                                      (string-downcase (view-border-shape view)))))))

