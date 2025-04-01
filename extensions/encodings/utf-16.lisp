(uiop:define-package :lem-encodings/utf-16
  (:use :lem/buffer :cl))
(in-package :lem-encodings/utf-16)

(defclass utf-16 (encoding) ())
(defclass utf-16be (encoding) ())
(defclass utf-16le (encoding) ())

(register-encoding 'utf-16)
(register-encoding 'utf-16be)
(register-encoding 'utf-16le)

;; Shadow/unshadow invalid character into Private Use Area (U+E000–U+F8FF)
(defun e+ (c) (+ #xe000 c))
(defun e- (c) (- c #xe000))
(defun e-range (c)
  (<= #xe000 c #xe0ff))

(defun decode-char (msb lsb)
  (+ (ash msb 8) lsb))

(defun decode (input endian cr encoding output-char)
  (let ((cr cr))
    (labels ((commit (c)
               (setf cr (funcall output-char c cr encoding)))
             (ecommit (c)
               (commit (cond ((<= #x00 c #x7f) c)
                             ((<= #x80 c #xff) (e+ c))))))  ; Why latin-1 supplement are treated as error?
      (loop
        :with count := 0
        :with prev-byte := 0
        :with high-surrogate := 0
        :with buffer-size := 8192
        :with buffer := (make-array buffer-size :element-type '(unsigned-byte 8))
        :for end := (read-sequence buffer input)
        :until (zerop end)
        :do (loop
              :for i :from 0 :below end
              :for byte := (aref buffer i)
              :do (if (prog1 (evenp count)
                        (incf count))
                      (setf prev-byte byte)
                      (let ((ch (ecase endian
                                  (:big-endian (decode-char prev-byte byte))
                                  (:little-endian (decode-char byte prev-byte)))))
                        (cond ((<= #xd800 ch #xdbff)
                               (setf high-surrogate ch))
                              ((and (zerop high-surrogate)
                                    (<= #xdc00 ch #xdfff))
                               (ecommit ch))
                              ((and (not (zerop high-surrogate))
                                    (<= #xdc00 ch #xdfff))
                               (progn
                                 (commit (+ #x10000 (* (- high-surrogate #xd800) #x400) (- ch #xdc00)))
                                 (setf high-surrogate 0)))
                              (t (commit ch))))))))))


;; Decode bytes from '(unsigned-byte 8) stream `input` to buffer or anything else
;; by the function `output-char`
(defmethod encoding-read ((encoding utf-16be) input output-char)
  (decode input :big-endian nil encoding output-char))

(defmethod encoding-read ((encoding utf-16le) input output-char)
  (decode input :little-endian nil encoding output-char))

(defmethod encoding-read ((encoding utf-16) input output-char)
  (let ((first-byte (read-byte input))
        (second-byte (read-byte input)))
    (cond ((and (= first-byte #xfe) (= second-byte #xff))
           (decode input :big-endian nil encoding output-char))
          ((and (= first-byte #xff) (= second-byte #xfe))
           (decode input :little-endian nil encoding output-char))
          (t
           (let ((cr (funcall output-char (decode-char first-byte second-byte) nil encoding)))
             (decode input :big-endian cr encoding output-char))))))

(defun write-cp (cp endian out)
  (ecase endian
    (:big-endian
     (progn
       (write-byte (ash (logand cp #xff00) -8) out)
       (write-byte (logand cp #x00ff) out)))
    (:little-endian
     (progn
       (write-byte (logand cp #x00ff) out)
       (write-byte (ash (logand cp #xff00) -8) out)))))

(defun encode (endian out)
  (lambda (ch)
    (when ch
      (let ((cp (char-code ch)))
        (cond ((e-range cp) (write-cp (e- cp) endian out))
              ((<= #x10000 cp)
               (multiple-value-bind (q r)
                   (truncate (- cp #x10000) #x400)
                 (write-cp (+ q #xd800) endian out)
                 (write-cp (+ r #xdc00) endian out)))
              (t (write-cp cp endian out)))))))

;; Return a function which encodes a character and writes the result into stream `out`
(defmethod encoding-write ((external-format utf-16be) out)
  (declare (ignore external-format))
  (encode :big-endian out))

(defmethod encoding-write ((external-format utf-16le) out)
  (declare (ignore external-format))
  (encode :little-endian out))

(defmethod encoding-write ((external-format utf-16) out)
  (declare (ignore external-format))
  (write-byte #xfe out)(write-byte #xff out)  ; BOM for big endian
  (encode :big-endian out))
