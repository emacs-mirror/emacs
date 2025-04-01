(in-package :lem-core)

(defmacro with-current-window (window &body body)
  (let ((gprev-window (gensym "PREV-WINDOW"))
        (gwindow (gensym "WINDOW")))
    `(let ((,gprev-window (current-window))
           (,gwindow ,window))
       (setf (current-window) ,gwindow)
       (unwind-protect (progn ,@body)
         (unless (deleted-window-p ,gprev-window)
           (setf (current-window) ,gprev-window))))))

(defmacro with-pop-up-typeout-window ((stream-var buffer &key erase (read-only t)) &body body)
  `(pop-up-typeout-window ,buffer
                          :function (lambda (,stream-var) ,@body)
                          :erase ,erase
                          :read-only ,read-only))

(defmacro with-debug-output ((filename) &body body)
  `(with-open-file (out ,filename
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
     (let ((*terminal-io* out)
           (*standard-output* out)
           (*standard-input* out)
           (*error-output* out)
           (*query-io* out)
           (*debug-io* out)
           (*trace-output* out))
       ,@body)))

#+sbcl
(defmacro with-profile (&body body)
  (let ((names '("LEM" "LEM-CORE" "LEM-INTERFACE")))
    `(progn
       (sb-profile:profile ,@names)
       ,@body
       (with-debug-output ("PROFILE")
         (sb-profile:report))
       (sb-profile:unprofile ,@names))))

(defmacro with-editor-stream (() &body body)
  (alexandria:with-gensyms (stream)
    `(let* ((,stream (make-editor-io-stream))
            (*standard-output* ,stream)
            (*standard-input* ,stream)
            (*error-output* ,stream)
            (*query-io* ,stream)
            (*debug-io* ,stream)
            (*trace-output* ,stream)
            (*terminal-io* ,stream))
       ,@body)))

(defmacro define-buffer-accessor (name)
  (alexandria:with-unique-names (buffer value)
    `(progn
       (defun ,name (,buffer)
         (buffer-value ,buffer ',name))
       (defun (setf ,name) (,value ,buffer)
         (setf (buffer-value ,buffer ',name) ,value)))))

(defmacro define-overlay-accessors (name &key clear-function add-function)
  (alexandria:with-unique-names (buffer overlay)
    `(progn
       (define-buffer-accessor ,name)
       ,(when clear-function
          `(defun ,clear-function (,buffer)
             (map () #'delete-overlay (,name ,buffer))
             (setf (,name ,buffer) '())))
       ,(when add-function
          `(defun ,add-function (,buffer ,overlay)
             (push ,overlay (,name ,buffer)))))))
