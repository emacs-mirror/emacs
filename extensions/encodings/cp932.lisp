(uiop:define-package :lem-encodings/cp932
  (:use :cl :lem/buffer)
  (:import-from :lem-encodings/table))
(in-package :lem-encodings/cp932)

(defun e+ (c) (+ #xe000 c))

(let* ((path (asdf:system-relative-pathname :lem-encodings "cp932.table"))
       (data (lem-encodings/table:read-table path)))
  (defvar *from* (loop with result = (make-hash-table)
                       for line in data
                       do (setf (gethash (first line) result) (second line))
                       finally (return result)))
  (defvar *to* (loop with result = (make-hash-table)
                     for line in data
                     do (setf (gethash (second line) result) (or (third line) (first line)))
                     finally (return result))))

(defclass cp932 (encoding) ())

(register-encoding 'cp932)

(defmethod encoding-read ((encoding cp932) input output-char)
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
                  :do (cond ((= 1st 0) ;; 1stbyte reading
                             #1=(cond ((or (<= #x00 c #x7f) ;;ascii
                                           (<= #xa1 c #xdf));;kana
                                       (commit (gethash c *from*)))
                                      ((or (= c #x80)
                                           (= c #xa0)
                                           (<= #xf0 c #xff))
                                       (ecommit c))
                                      (t (setf 1st c))))
                            (t
                             (let ((g (gethash (+ (* 256 1st) c) *from*)))
                               (cond (g (setf 1st 0)
                                        (commit g))
                                     (t (ecommit 1st)
                                        (setf 1st 0)
                                        #1#))))))
            (when (< end buffer-size)
              (return))
        :finally (commit nil)))))

(defmethod encoding-write ((encoding cp932) out)
  (lambda (c)
    (when c
      (let ((find (gethash (char-code c) *to*)))
        (cond ((> find 255)
               (write-byte (ash find -8) out)
               (write-byte (logand find #xff) out))
              (t (write-byte find out)))))))

(defmethod encoding-check ((encoding cp932))
  (lambda (string eof-p)
    (unless eof-p
      (loop :for c :across string
            :unless (gethash (char-code c) *to*)
            :do (error "~A is not acceptable for ~S" c encoding)))))
