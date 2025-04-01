(uiop:define-package :lem-encodings/iso-8859-1
  (:use :cl :lem/buffer))
(in-package :lem-encodings/iso-8859-1)

(defclass iso-8859-1 (encoding) ())

(register-encoding 'iso-8859-1)

(defmethod encoding-read ((encoding iso-8859-1) input output-char)
  (let (cr)
    (labels ((commit (c)
               (setf cr (funcall output-char c cr encoding))))
      (loop
        :with buffer-size := 8192
        :with buffer := (make-array (list buffer-size) :element-type '(unsigned-byte 8))
        :for end := (read-sequence buffer input)
        :until (zerop end)
        :do (loop :for i :from 0 :below end
                  :do (commit (aref buffer i)))
            (when (< end buffer-size)
              (return))
        :finally (commit nil)))))

(defmethod encoding-write ((encoding iso-8859-1) out)
  (lambda (c)
    (when c
      (write-byte (char-code c) out))))

(defmethod encoding-check ((encoding iso-8859-1))
  (lambda (string eof-p)
    (unless eof-p
      (loop :for c :across string
            :unless (char<= #.(code-char    0)
                            c
                            #.(code-char #xff))
            :do (error "~A is not acceptable for ~S" c encoding)))))
