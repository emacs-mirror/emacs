(uiop:define-package :lem/directory-mode/internal
  (:use :cl
        :lem)
  (:import-from :lem/directory-mode/file
                :symbolic-link-p
                :pathname-directory-last-name)
  (:import-from :lem/directory-mode/attributes
                :current-directory-attribute
                :file-size-attribute
                :file-date-attribute
                :file-attribute
                :directory-attribute
                :link-attribute)
  (:import-from :lem/directory-mode/mode
                :directory-mode)
  (:export :sort-method
           :*default-sort-method*
           :update-buffer
           :directory-buffer
           :open-selected-file
           :set-mark
           :get-mark
           :filter-marks
           :get-name
           :marked-lines
           :marked-files
           :get-pathname
           :current-file
           :selected-files
           :update-all-buffers
           :update-highlight-line-overlay))
(in-package :lem/directory-mode/internal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/internal))

(deftype sort-method ()
  '(member :pathname :mtime :size))

(declaim (type (sort-method) *default-sort-method*))
(defvar *default-sort-method* :pathname
  "Default method to sort files when opening a directory.

  A keyword, one of :pathname (sort by file name), :mtime (last modification time) and :size.")

(defun move-to-start-line (point)
  (move-to-line point 3))

(defun insert-spaces-with-property (point pathname name)
  (insert-string point
                 "  "
                 'pathname pathname
                 'name name))

(defun get-line-property (p key)
  (with-point ((p p))
    (text-property-at (line-start p) key)))

(defun get-pathname (point)
  (get-line-property point 'pathname))

(defun get-name (point)
  (get-line-property point 'name))

(defun get-mark (p)
  (with-point ((p p))
    (eql #\* (character-at (line-start p) 1))))

(defun set-mark (p mark)
  (with-buffer-read-only (point-buffer p) nil
    (let ((*inhibit-read-only* t))
      (with-point ((p p))
        (let ((pathname (get-line-property p 'pathname)))
          (when (and pathname (not (uiop:pathname-equal
                                    pathname
                                    (uiop:pathname-parent-directory-pathname
                                     (buffer-directory (point-buffer p))))))
            (character-offset (line-start p) 1)
            (delete-character p 1)
            (insert-character p (if mark #\* #\space))))))))

(defun iter-marks (p function)
  (with-point ((p p))
    (move-to-start-line p)
    (loop
      (funcall function p)
      (unless (line-offset p 1) (return)))))

(defun marked-lines (p)
  (with-point ((p p))
    (let ((points '()))
      (iter-marks p
                  (lambda (p)
                    (when (get-mark p)
                      (push (copy-point p :temporary) points))))
      (nreverse points))))

(defun marked-files (p)
  (mapcar 'get-pathname (marked-lines p)))

(defun filter-marks (p function)
  (iter-marks p
              (lambda (p)
                (set-mark p (funcall function p)))))

(defun current-file (p)
  (alexandria:when-let (pathname (get-pathname p))
    pathname))

(defun selected-files (p)
  (or (marked-files p)
      (uiop:ensure-list (current-file p))))

(defun process-current-line-pathname (function)
  (alexandria:when-let (pathname (get-pathname (current-point)))
    (funcall function pathname)))

(defun open-selected-file (&key read-only next-window)
  (if read-only
      (process-current-line-pathname (if next-window 'read-file-next-window 'read-file))
      (process-current-line-pathname (if next-window 'find-file-next-window 'find-file))))

(defstruct item
  directory
  pathname
  content)

(defun item-name (item)
  (or (item-content item)
      (namestring (enough-namestring (item-pathname item)
                                     (item-directory item)))))

(defun human-readable-file-size (size)
  (loop :for sign :in '(#\Y #\Z #\E #\P #\T #\G #\M #\k)
        :for val := (expt 1024 8) :then (/ val 1024)
        :do (when (>= size val)
              (return (format nil "~D~C" (1+ (floor size val)) sign)))
        :finally (return (princ-to-string size))))

(defun insert-file-size (point item)
  (let ((pathname (item-pathname item)))
    (let ((file-size (handler-case (file-size pathname)
                       (error ()
                         (return-from insert-file-size)))))

      (insert-string point
                     (format nil " ~5@A "
                             (if file-size (human-readable-file-size file-size) ""))
                     :attribute 'file-size-attribute))))

(defun insert-file-write-date (point item)
  (let ((pathname (item-pathname item)))
    (multiple-value-bind (second minute hour day month year week)
        (let ((date (file-write-date pathname)))
          (if date
              (decode-universal-time date)
              (values 0 0 0 0 0 0 nil)))
      (insert-string point
                     (format nil "~4,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D ~A "
                             year month day hour minute second
                             (if week (aref #("Mon" "Tue" "Wed" "Thr" "Fri" "Sat" "Sun") week)
                                 "   "))
                     :attribute 'file-date-attribute))))

(defun insert-icon (point pathname)
  (cond ((uiop:directory-pathname-p pathname)
         (insert-string point (icon-string "folder")))
        ((let ((string (icon-string-by-ext (pathname-type pathname))))
           (when string
             (insert-string point string)
             t)))
        (t
         (let ((string (icon-string-by-ext "txt")))
           (when string
             (insert-string point string)
             t)))))

(defun get-file-attribute (pathname)
  (cond ((symbolic-link-p pathname)
         'link-attribute)
        ((uiop:directory-pathname-p pathname)
         'directory-attribute)
        (t
         'file-attribute)))

(defun insert-file-name (point item)
  (let ((name (item-name item))
        (pathname (item-pathname item)))
    (unless (string= name "..")
      (insert-icon point name))
    (insert-string point
                   name
                   :attribute (get-file-attribute pathname)
                   :file pathname)
    (when (symbolic-link-p pathname)
      (insert-string point (format nil " -> ~A" (probe-file pathname))))))

(defparameter *file-entry-inserters*
  (list #'insert-file-size
        #'insert-file-write-date
        #'insert-file-name))

(defun insert-item (point item)
  (dolist (inserter *file-entry-inserters*)
    (funcall inserter point item)))

(defun insert-pathname (point item)
  (let ((pathname (item-pathname item)))
    (with-point ((start point))
      (insert-spaces-with-property point pathname (item-name item))
      (insert-item point item)
      (back-to-indentation start)
      (lem/button:apply-button-between-points
       start
       point
       (lambda ()
         (lem/button:with-context ()
           (open-selected-file :read-only nil :next-window nil))))
      (insert-character point #\newline)
      (put-text-property start point :read-only t))))

(defun insert-directories-and-files (point
                                     directory
                                     &key (sort-method *default-sort-method*)
                                          (without-parent-directory t))
  (unless without-parent-directory
    (alexandria:when-let (pathname (probe-file (merge-pathnames "../" directory)))
      (insert-pathname point (make-item :directory directory :pathname pathname :content ".."))))
  (dolist (pathname (list-directory directory :sort-method sort-method))
    (insert-pathname point (make-item :directory directory :pathname pathname))))

(defun update-buffer (buffer &key (sort-method *default-sort-method*))
  "Update this directory buffer content."
  (with-buffer-read-only buffer nil
    (let ((*inhibit-read-only* t))
      (let* ((directory (buffer-directory buffer))
             (p (buffer-point buffer))
             (line-number (line-number-at-point p)))
        (erase-buffer buffer)
        (buffer-start p)
        (insert-string p (format nil "~A~2%" directory) :attribute 'current-directory-attribute)
        (insert-directories-and-files p directory
                                      :sort-method sort-method
                                      :without-parent-directory nil)
        (move-to-line p line-number)))))

(defun create-directory-buffer (name filename)
  (let ((buffer (make-buffer name :enable-undo-p nil :read-only-p t)))
    (change-buffer-mode buffer 'directory-mode)
    (setf (buffer-directory buffer) filename)
    (update-buffer buffer)
    (move-to-start-line (buffer-point buffer))
    buffer))

(defun directory-buffer (filename)
  (setf filename (uiop:directory-exists-p
                  (expand-file-name (namestring filename)
                                    (buffer-directory))))
  (let* ((name (pathname-directory-last-name filename))
         (buffer (get-buffer name)))
    (cond ((and buffer (not (uiop:pathname-equal filename (buffer-directory buffer))))
           (create-directory-buffer (unique-buffer-name name) filename))
          ((null buffer)
           (create-directory-buffer name filename))
          (t
           buffer))))

(defun update-all-buffers ()
  (dolist (buffer (buffer-list))
    (when (eq 'directory-mode (buffer-major-mode buffer))
      (update-buffer buffer))))

(defun remove-line-overlay-when-buffer-change (point arg)
  (declare (ignore arg))
  (alexandria:when-let (ov (buffer-value (point-buffer point) 'line-overlay))
    (setf (buffer-value (point-buffer point) 'line-overlay) nil)
    (delete-overlay ov)))

(defun update-highlight-line-overlay (point)
  (with-point ((start point)
               (end point))
    (back-to-indentation start)
    (line-end end)
    (let ((ov (buffer-value point 'line-overlay)))
      (cond (ov
             (move-point (overlay-start ov) start)
             (move-point (overlay-end ov) end))
            (t
             (add-hook (variable-value 'before-change-functions :buffer (point-buffer point))
                       'remove-line-overlay-when-buffer-change)
             (setf ov (make-overlay start end 'region))
             (setf (buffer-value point 'line-overlay) ov))))))
