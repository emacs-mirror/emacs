(in-package :lem/buffer/internal)

(defparameter +primordial-buffer-name+ "*tmp*")

(defclass buffer ()
  ((name
    :initarg :name
    :accessor buffer-%name)
   (temporary
    :initarg :temporary
    :reader buffer-temporary-p
    :type boolean)
   (variables
    :initform (make-hash-table :test 'equal)
    :accessor buffer-variables)
   ;; only used in text buffer
   (%filename
    :initform nil
    :accessor buffer-%filename)
   (%directory
    :initarg :%directory
    :initform nil
    :accessor buffer-%directory
    :documentation "The buffer's directory. See: `buffer-directory'.")
   (%modified-p
    :initform 0
    :reader buffer-modified-tick
    :accessor buffer-%modified-p
    :type integer)
   (%enable-undo-p
    :initarg :%enable-undo-p
    :accessor buffer-%enable-undo-p
    :type boolean)
   (%enable-undo-boundary-p
    :initarg :%enable-undo-boundary-p
    :initform t
    :accessor buffer-%enable-undo-boundary-p)
   (read-only-p
    :initarg :read-only-p
    :accessor buffer-read-only-p
    :type boolean)
   (syntax-table
    :initform (fundamental-syntax-table)
    :initarg :syntax-table
    :accessor buffer-syntax-table
    :type syntax-table)
   (major-mode
    :initform 'lem/buffer/fundamental-mode:fundamental-mode
    :initarg :major-mode
    :accessor buffer-major-mode)
   (minor-modes
    :initform nil
    :accessor buffer-minor-modes
    :type list)
   (start-point
    :initform nil
    :writer set-buffer-start-point
    :reader buffer-start-point)
   (end-point
    :writer set-buffer-end-point
    :reader buffer-end-point)
   (%mark
    :type mark
    :initform (make-instance 'mark)
    :reader buffer-mark-object)
   (point
    :initform nil
    :reader buffer-point
    :writer set-buffer-point)
   (keep-binfo
    :initform nil
    :accessor %buffer-keep-binfo)
   (points
    :initform nil
    :accessor buffer-points)
   (nlines
    :initform 1
    :accessor buffer-nlines
    :type (integer 1 *))
   (edit-history
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor buffer-edit-history)
   (redo-stack
    :initform '()
    :accessor buffer-redo-stack)
   (encoding
    :initform nil
    :accessor buffer-encoding)
   (last-write-date
    :initform nil
    :accessor buffer-last-write-date)))

(defclass text-buffer (buffer)
  ())

(defmethod buffer-mark ((buffer buffer))
  (mark-point (buffer-mark-object buffer)))

(defmethod buffer-mark-p ((buffer buffer))
  (mark-active-p (buffer-mark-object buffer)))

;; workaround for windows
#+win32
(defmethod initialize-instance :after ((buffer buffer) &rest initargs)
  "set default buffer encoding to utf-8"
  (setf (buffer-encoding buffer) (encoding :utf-8 :lf)))

(setf (documentation 'buffer-point 'function) "Returns the current `point` of `buffer`.")
(setf (documentation 'buffer-mark 'function)
      "Returns the `point` of the current mark in the `buffer`")
(setf (documentation 'buffer-start-point 'function)
      "Returns the `point` at the start of the `buffer`.")
(setf (documentation 'buffer-end-point 'function)
      "Returns the `point` at the end of the `buffer`.")

(defvar *current-buffer*)

(defun primordial-buffer ()
  (make-buffer +primordial-buffer-name+))

(defun current-buffer ()
  "Return the current buffer."
  (unless (boundp '*current-buffer*)
    (setf *current-buffer*
          (primordial-buffer)))
  *current-buffer*)

(defun (setf current-buffer) (buffer)
  "Change the current buffer."
  (check-type buffer buffer)
  (setf *current-buffer* buffer))

(defun last-edit-history (buffer)
  (when (< 0 (fill-pointer (buffer-edit-history buffer)))
    (aref (buffer-edit-history buffer)
          (1- (fill-pointer (buffer-edit-history buffer))))))

(defun make-buffer-start-point (point)
  (copy-point point :right-inserting))

(defun make-buffer-end-point (point)
  (copy-point point :left-inserting))

(defgeneric make-buffer-point (point)
  (:method (point)
    (copy-point point :left-inserting)))

(defun make-buffer (name &key temporary read-only-p (enable-undo-p t) directory
                              (syntax-table (fundamental-syntax-table)))
  "If the buffer of name `name` exists in Lem's buffer list, return it, otherwise create it.
`read-only-p` sets this buffer read-only.
`enable-undo-p` enables undo.
`syntax-table` specifies the syntax table for the buffer.
If `temporary` is non-NIL, it creates a buffer and doesn't add it in the buffer list.
Options that can be specified by arguments are ignored if `temporary` is NIL and the buffer already exists.
"
  (unless temporary
    (uiop:if-let ((buffer (get-buffer name)))
      (return-from make-buffer buffer)))
  (let ((buffer (make-instance 'text-buffer
                               :name name
                               :read-only-p read-only-p
                               :%directory directory
                               :%enable-undo-p enable-undo-p
                               :temporary temporary
                               :syntax-table syntax-table)))
    (let* ((temp-point (make-point buffer 1 (line:make-empty-line) 0 :kind :temporary))
           (start-point (make-buffer-start-point temp-point))
           (end-point (make-buffer-end-point temp-point))
           (point (make-buffer-point temp-point)))
      (set-buffer-start-point start-point buffer)
      (set-buffer-end-point end-point buffer)
      (set-buffer-point point buffer))
    (unless temporary (add-buffer buffer))
    buffer))

(defun bufferp (x)
  "Return T if 'x' is a buffer, NIL otherwise."
  (typep x 'buffer))

(defun buffer-modified-p (&optional (buffer (current-buffer)))
  "Return T if 'buffer' has been modified, NIL otherwise."
  (/= 0 (buffer-%modified-p buffer)))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :identity t :type t)
    (format stream "~A ~A"
            (buffer-name buffer)
            (buffer-filename buffer))))

(defun %buffer-clear-keep-binfo (buffer)
  (when (%buffer-keep-binfo buffer)
    (destructuring-bind (view-point point)
        (%buffer-keep-binfo buffer)
      (delete-point view-point)
      (delete-point point))))

(defun buffer-free (buffer)
  (%buffer-clear-keep-binfo buffer)
  (delete-point (buffer-point buffer))
  (set-buffer-point nil buffer))

(defun deleted-buffer-p (buffer)
  (null (buffer-point buffer)))

(defun buffer-name (&optional (buffer (current-buffer)))
  "Return the name of 'buffer'. Defaults to the current buffer."
  (buffer-%name buffer))

(defun buffer-filename (&optional (buffer (current-buffer)))
  "Return the filename (string) of 'buffer'. Defaults to the current buffer."
  (alexandria:when-let (filename (buffer-%filename buffer))
    (namestring filename)))

(defun (setf buffer-filename) (filename &optional (buffer (current-buffer)))
  (setf (buffer-directory buffer) (directory-namestring filename))
  (setf (buffer-%filename buffer) filename))

(defun buffer-directory (&optional (buffer (current-buffer)))
  "Return the directory (string) of 'buffer' or the current working directory. Defaults to the current buffer."
  (or (buffer-%directory buffer)
      (namestring (uiop:getcwd))))

(defun (setf buffer-directory) (directory &optional (buffer (current-buffer)))
  (let ((result (uiop:directory-exists-p directory)))
    (unless result
      (error 'directory-does-not-exist :directory directory))
    (setf (buffer-%directory buffer)
          (namestring result))))

(defun buffer-unmark (buffer)
  "Unflag the change flag of 'buffer'."
  (setf (buffer-%modified-p buffer) 0))

(defun buffer-mark-cancel (buffer)
  (mark-cancel (buffer-mark-object buffer)))

(defun buffer-attributes (buffer)
  (concatenate 'string
               " "
               (cond ((buffer-read-only-p buffer)
                      "% ")
                     ((buffer-modified-p buffer)
                      "* ")
                     (t
                      "  "))))

(defun check-read-only-buffer (buffer)
  (when (buffer-read-only-p buffer)
    (error 'read-only-error)))

(defun buffer-rename (buffer name)
  "Rename 'buffer' to 'name'."
  (check-type buffer buffer)
  (check-type name string)
  (when (get-buffer name)
    (editor-error "Buffer name `~A' is in use" name))
  (setf (buffer-%name buffer) name))

(defun ensure-buffer (where)
  (if (pointp where)
      (point-buffer where)
      (progn
        (check-type where buffer)
        where)))

(defun buffer-value (buffer name &optional default)
  "Return the value bound to the buffer variable 'name' in 'buffer'.

  The type of 'buffer' is 'buffer' or 'point'.
  If the variable is not set, 'default' is returned."
  (setf buffer (ensure-buffer buffer))
  (multiple-value-bind (value foundp)
      (gethash name (buffer-variables buffer))
    (if foundp value default)))

(defun (setf buffer-value) (value buffer variable &optional default)
  "Bind 'value' to the buffer variable 'variable' of 'buffer'.
  The type of 'buffer' is 'buffer' or 'point'."
  (declare (ignore default))
  (setf buffer (ensure-buffer buffer))
  (setf (gethash variable (buffer-variables buffer)) value))

(defun buffer-unbound (buffer variable)
  "Remove the binding of the buffer variable 'variable' in 'buffer'."
  (remhash variable (buffer-variables buffer)))

(defun clear-buffer-variables (&key (buffer (current-buffer)))
  "Clear all buffer variables bound to 'buffer'. Defaults to the current buffer."
  (clrhash (buffer-variables buffer)))

(defun call-with-buffer-point (buffer point function)
  (let ((original-point (buffer-point buffer)))
    (set-buffer-point point buffer)
    (unwind-protect (funcall function)
      (set-buffer-point original-point buffer))))

(defmacro with-buffer-point ((buffer point) &body body)
  `(call-with-buffer-point ,buffer ,point (lambda () ,@body)))

(defmacro with-current-buffer (buffer-or-name &body body)
  `(let ((*current-buffer* (get-buffer ,buffer-or-name)))
     ,@body))

(defmacro with-buffer-read-only (buffer flag &body body)
  (let ((gbuffer (gensym "BUFFER"))
        (gtmp (gensym "GTMP")))
    `(let* ((,gbuffer ,buffer)
            (,gtmp (buffer-read-only-p ,gbuffer)))
       (setf (buffer-read-only-p ,gbuffer) ,flag)
       (unwind-protect (progn ,@body)
         (setf (buffer-read-only-p ,gbuffer) ,gtmp)))))


;;;
(defun buffer-list ()
  "Return a list of buffers."
  (lem/buffer/buffer-list-manager::buffer-list-manager-buffers (buffer-list-manager)))

(defun set-buffer-list (buffer-list)
  (setf (lem/buffer/buffer-list-manager::buffer-list-manager-buffers (buffer-list-manager))
        buffer-list))

(defun add-buffer (buffer)
  (check-type buffer buffer)
  (assert (not (get-buffer (buffer-name buffer))))
  (set-buffer-list (cons buffer (buffer-list))))

(defun any-modified-buffer-p ()
  (some (lambda (buffer)
          (and (buffer-filename buffer)
               (buffer-modified-p buffer)))
        (buffer-list)))

(defun modified-buffers ()
  (remove-if (lambda (buffer)
               (not (and (buffer-filename buffer)
                         (buffer-modified-p buffer))))
             (buffer-list)))

(defun get-buffer (buffer-or-name)
  "If 'buffer-or-name' is a buffer, it is returned.
  If it is a string, it returns the buffer with that name."
  (check-type buffer-or-name (or buffer string))
  (if (bufferp buffer-or-name)
      buffer-or-name
      (find-if (lambda (buffer)
                 (string= buffer-or-name
                          (buffer-name buffer)))
               (buffer-list))))

(defun unique-buffer-name (name)
  (check-type name string)
  (if (null (get-buffer name))
      name
      (loop :for n :from 1
            :for sub-name := (format nil "~A<~D>" name n)
            :do (unless (get-buffer sub-name)
                  (return sub-name)))))

(defmethod delete-buffer-using-manager ((manager buffer-list-manager) buffer)
  (buffer-free buffer)
  (set-buffer-list (delete buffer (buffer-list))))

(defun delete-buffer (buffer)
  "Remove 'buffer' from the list of buffers."
  (check-type buffer buffer)
  (delete-buffer-using-manager (buffer-list-manager) buffer))

(defun get-next-buffer (buffer)
  "Return the next buffer of 'buffer' in the buffer list."
  (check-type buffer buffer)
  (let ((rest (member buffer (buffer-list))))
    (cadr rest)))

(defun get-previous-buffer (buffer)
  "Return the buffer before 'buffer' in the buffer list."
  (check-type buffer buffer)
  (loop :for prev := nil :then curr
        :for curr :in (buffer-list)
        :do (when (eq buffer curr)
              (return prev))))

(defun unbury-buffer (buffer)
  (check-type buffer buffer)
  (unless (buffer-temporary-p buffer)
    (set-buffer-list
     (cons buffer
           (delete buffer (buffer-list)))))
  buffer)

(defun bury-buffer (buffer)
  "Move 'buffer' to the end of the buffer list and return the beginning of the buffer list."
  (check-type buffer buffer)
  (unless (buffer-temporary-p buffer)
    (set-buffer-list
     (nconc (delete buffer (buffer-list))
            (list buffer))))
  (car (buffer-list)))

(defun get-file-buffer (filename)
  "Return the buffer corresponding to 'filename' or NIL if not found."
  (check-type filename (or string pathname))
  (dolist (buffer (buffer-list))
    (when (uiop:pathname-equal filename (buffer-filename buffer))
      (return buffer))))

(defun clear-buffer-edit-history (buffer)
  (setf (buffer-edit-history buffer)
        (make-array 0 :adjustable t :fill-pointer 0)))
