(uiop:define-package :lem-encodings/gb2312
  (:use :cl :lem/buffer))
(in-package :lem-encodings/gb2312)

(defun e+ (c) (+ #xe000 c))

(let* ((path (asdf:system-relative-pathname :lem-encodings "gb2312.table"))
       (data (lem-encodings/table:read-table path)))
  (defvar *from* (loop with result = (make-hash-table)
                       for line in data
                       do (setf (gethash (first line) result) (second line))
                       finally (return result)))
  (defvar *to* (loop with result = (make-hash-table)
                     for line in data
                     do (setf (gethash (second line) result) (or (third line) (first line)))
                     finally (return result))))

(defclass gb2312 (encoding) ())

(register-encoding 'gb2312)

(defmethod encoding-read ((encoding gb2312) input output-char)
  (let (cr)
    (labels ((commit (c)
               (setf cr (funcall output-char c cr encoding)))
             (ecommit (c)
               (commit (e+ c))))
      (loop
        :with 1st := 0
        :with buffer-size := 8192
        :with buffer := (make-array (list buffer-size) :element-type '(unsigned-byte 8))
        :for end := (read-sequence buffer input)
        :until (zerop end)
        :do (loop :for i :from 0 :below end
                  :for c := (aref buffer i)
                  :do (cond ((= 1st 0)
                             #1=(cond ((<= #x00 c #x7f) ;;ascii
                                       (commit (gethash c *from*)))
                                      ((or (<= #x81 c #xa9)
                                           (<= #xb0 c #xf7)))
                                      (setf 1st c)
                                      (t (ecommit c)))) ;; 1stbyte reading
                            (t ;2ndbyte reading
                             (cond ((<= #xa1 c #xfe)
                                    (let ((g (gethash (+ (* 256 1st) c) *from*)))
                                      (cond (g (commit g))
                                            (t (ecommit 1st)
                                               (ecommit c)))
                                      (setf 1st 0)))
                                   (t (ecommit 1st)
                                      (setf 1st 0)
                                      #1#)))))
            (when (< end buffer-size)
              (return))
        :finally (commit nil)))))

(defmethod encoding-write ((encoding gb2312) out)
  (lambda (c)
    (when c
      (let ((find (gethash (char-code c) *to*)))
        (cond ((>= find (* 256 256))
               (write-byte (ash find -16) out)
               (write-byte (logand (ash find -8) #xff) out)
               (write-byte (logand find #xff) out))
              ((>= find 256)
               (write-byte (ash find -8) out)
               (write-byte (logand find #xff) out))
              (t (write-byte find out)))))))

(defmethod encoding-check ((encoding gb2312))
  (lambda (string eof-p)
    (unless eof-p
      (loop :for c :across string
            :unless (gethash (char-code c) *to*)
            :do (error "~A is not acceptable for ~S" c encoding)))))
