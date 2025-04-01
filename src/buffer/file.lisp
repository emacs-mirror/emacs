(in-package :lem/buffer/file)

(defvar *find-file-hook* '())

(define-editor-variable before-save-hook '())
(define-editor-variable after-save-hook '())

(defvar *external-format-function* nil)
(defvar *find-directory-function* nil)
(defvar *default-external-format* :detect-encoding)

(define-condition encoding-read-error (editor-error)
  ((condition :initarg :condition)
   (filename :initarg :filename
             :reader encoding-read-error-filename))
  (:report (lambda (c s)
             (with-slots (filename) c
               (format s "Couldn't read this file: ~A" filename)))))

(defun %encoding-read (encoding point stream stream-filename)
  (let ((end-of-line (encoding-end-of-line encoding)))
    (loop
      (multiple-value-bind (str eof-p)
          (handler-bind ((error (lambda (e)
                                  (error 'encoding-read-error
                                         :condition e
                                         :filename stream-filename))))
            (read-line stream nil))
        (cond
          (eof-p
           (when str
             (insert-string point str))
           (return))
          (t
           (let ((end nil))
             #+sbcl
             (when (and (eq end-of-line :crlf)
                        (< 0 (length str)))
               (setf end (1- (length str))))
             (insert-string point
                            (if end
                                (subseq str 0 end)
                                str))
             (insert-character point #\newline))))))))

(defun insert-file-contents (point filename
                             &key (external-format *default-external-format*)
                                  (end-of-line :auto))
  (when (eql external-format :detect-encoding)
    (if *external-format-function*
        (multiple-value-setq (external-format end-of-line)
          (funcall *external-format-function* filename))
        (setf external-format :utf-8)))
  (let* ((encoding (encoding external-format end-of-line))
         (use-internal-p (typep encoding 'internal-encoding)))
    (with-point ((point point :left-inserting))
      (with-open-virtual-file (stream filename
                                      :element-type (unless use-internal-p
                                                      '(unsigned-byte 8))
                                      :external-format (and use-internal-p external-format)
                                      :direction :input)
        (if use-internal-p
            (%encoding-read encoding point stream filename)
            (encoding-read encoding
                           stream
                           (encoding-read-detect-eol
                            (lambda (c)
                              (when c (insert-character point (code-char c)))))))))
    encoding))

(defun find-file-buffer (filename &key temporary (enable-undo-p t) (syntax-table nil syntax-table-p))
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (setf filename (expand-file-name filename))
  (unless (uiop:directory-exists-p (directory-namestring filename))
    (error 'directory-does-not-exist :directory (directory-namestring filename)))
  (alexandria:when-let (it (probe-file filename)) (setf filename (namestring it)))
  (cond ((uiop:directory-pathname-p filename)
         (if *find-directory-function*
             (funcall *find-directory-function* filename)
             (editor-error "~A is a directory" filename)))
        ((and (not temporary)
              (find filename (buffer-list) :key #'buffer-filename :test #'equal)))
        (t
         (let* ((name (file-namestring filename))
                (buffer (make-buffer (if temporary
                                         name
                                         (if (get-buffer name)
                                             (unique-buffer-name name)
                                             name))
                                     :enable-undo-p nil
                                     :temporary temporary)))
           (setf (buffer-filename buffer) filename)
           (when (probe-file filename)
             (let ((*inhibit-modification-hooks* t))
               (let ((encoding
                       (handler-bind ((encoding-read-error
                                        (lambda (e)
                                          (declare (ignore e))
                                          (delete-buffer buffer))))
                         (insert-file-contents (buffer-start-point buffer) filename))))
                 (setf (buffer-encoding buffer) encoding)))
             (buffer-unmark buffer))
           (buffer-start (buffer-point buffer))
           (when enable-undo-p (buffer-enable-undo buffer))
           (when syntax-table-p (setf (buffer-syntax-table buffer) syntax-table))
           (update-changed-disk-date buffer)
           (run-hooks *find-file-hook* buffer)
           (values buffer t)))))

(defun write-to-file-without-write-hook (buffer filename)
  (write-region-to-file (buffer-start-point buffer)
                        (buffer-end-point buffer) filename))

(defun run-before-save-hooks (buffer)
  (run-hooks (make-per-buffer-hook :var 'before-save-hook :buffer buffer)
             buffer))

(defun run-after-save-hooks (buffer)
  (run-hooks (make-per-buffer-hook :var 'after-save-hook :buffer buffer)
             buffer))

(defun call-with-write-hook (buffer function)
  (run-before-save-hooks buffer)
  (funcall function)
  (update-changed-disk-date buffer)
  (run-after-save-hooks buffer))

(defmacro with-write-hook (buffer &body body)
  `(call-with-write-hook ,buffer (lambda () ,@body)))

(defun write-to-file (buffer filename)
  (with-write-hook buffer
    (write-to-file-without-write-hook buffer filename)))

(defun %write-region-to-file (end-of-line out)
  (lambda (string eof-p)
    (princ string out)
    (unless eof-p
      #+sbcl
      (case end-of-line
        ((:crlf)
         (princ #\return out)
         (princ #\newline out))
        ((:lf)
         (princ #\newline out))
        ((:cr)
         (princ #\return out)))
      #-sbcl
      (princ #\newline out))))

(defun %%write-region-to-file (encoding out)
  (let ((f (encoding-write encoding out))
        (end-of-line (encoding-end-of-line encoding)))
    (lambda (string eof-p)
      (loop :for c :across string
            :do (funcall f c))
      (unless eof-p
        (ecase end-of-line
          ((:crlf)
           (funcall f #\return)
           (funcall f #\newline))
          ((:lf :auto)
           (funcall f #\newline))
          ((:cr)
           (funcall f #\return)))))))

(defun write-region-to-file (start end filename)
  (let* ((buffer (point-buffer start))
         (encoding (buffer-encoding buffer))
         (use-internal (or (typep encoding 'internal-encoding) (null encoding)))
         (check (encoding-check encoding)))
    (when check
      (map-region start end check)) ;; throw condition?
    (with-open-virtual-file (out filename
                                 :element-type (unless use-internal '(unsigned-byte 8))
                                 :external-format (if (and use-internal encoding)
                                                      (encoding-external-format encoding))
                                 :direction :output)
      (map-region start end
                  (if use-internal
                      (%write-region-to-file (if encoding
                                                 (encoding-end-of-line encoding)
                                                 :lf)
                                             out)
                      (%%write-region-to-file encoding out))))))

(defun file-write-date* (buffer)
  (if (probe-file (buffer-filename buffer))
      (file-write-date (buffer-filename buffer))))

(defun update-changed-disk-date (buffer)
  (setf (buffer-last-write-date buffer)
        (file-write-date* buffer)))

(defun changed-disk-p (buffer)
  (and (buffer-filename buffer)
       (probe-file (buffer-filename buffer))
       (not (eql (buffer-last-write-date buffer)
                 (file-write-date* buffer)))))
