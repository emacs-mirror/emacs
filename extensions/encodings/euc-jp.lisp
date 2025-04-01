(uiop:define-package :lem-encodings/euc-jp
  (:use :cl :lem/buffer))
(in-package :lem-encodings/euc-jp)

(defun e+ (c) (+ #xe000 c))

(let* ((path (asdf:system-relative-pathname :lem-encodings "euc-jp.table"))
       (data (lem-encodings/table:read-table path)))
  (defvar *from* (loop with result = (make-hash-table)
                       for line in data
                       do (setf (gethash (first line) result) (second line))
                       finally (return result)))
  (defvar *to* (loop with result = (make-hash-table)
                     for line in data
                     do (setf (gethash (second line) result) (or (third line) (first line)))
                     finally (return result))))

(defclass euc-jp (encoding) ())

(register-encoding 'euc-jp)

(defmethod encoding-read ((encoding euc-jp) input output-char)
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
                  :do (cond ((listp 1st) ;; 3rdbyte reading
                             (cond ((<= #xa1 c #xfe)
                                    (let ((g (gethash (+ #x8f0000 (* 256 (second 1st)) c) *from*)))
                                      (cond (g (commit g))
                                            (t (ecommit (first 1st))
                                               (ecommit (second 1st))
                                               (ecommit c)))
                                      (setf 1st 0)))
                                   (t (ecommit (first 1st))
                                      (ecommit (second 1st))
                                      (setf 1st 0)
                                      #1=(cond ((<= #x00 c #x7f) ;;ascii
                                                (commit (gethash c *from*)))
                                               ((or (= c #x8e)
                                                    (= c #x8f)
                                                    (<= #xa1 c #xfe))
                                                (setf 1st c))
                                               (t (ecommit c))))))
                            ((= 1st 0) #1#) ;; 1stbyte reading
                            (t ;2ndbyte reading
                             (cond ((and (= 1st #x8f) (<= #xa1 c #xfe))
                                    (setf 1st (list 1st c)))
                                   ((<= #xa1 c #xfe)
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

(defmethod encoding-write ((encoding euc-jp) out)
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

(defmethod encoding-check ((encoding euc-jp))
  (lambda (string eof-p)
    (unless eof-p
      (loop :for c :across string
            :unless (gethash (char-code c) *to*)
            :do (error "~A is not acceptable for ~S" c encoding)))))
