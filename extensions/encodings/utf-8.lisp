(uiop:define-package :lem-encodings/utf-8
  (:use :cl :lem/buffer))
(in-package :lem-encodings/utf-8)

(defclass utf-8 (encoding)
  ((bom
    :initform nil
    :accessor encoding-bom)))

(register-encoding 'utf-8)

(defun e+ (c) (+ #xe000 c))
(defun e- (c) (- c #xe000))
(defun e-range (c)
  (<=  #xe000 c #xe0ff))

(defmethod encoding-read ((encoding utf-8) input output-char)
  (let (cr not-first-byte)
    (labels ((commit (c)
               (setf cr (funcall output-char c cr encoding)))
             (ecommit (c)
               (commit (cond ((<= #x00 c #x7f) c)
                             ((<= #x80 c #xff) (e+ c))))))
      (loop
        :with state := 0
        :with count := 0
        :with read := 0
        :with buffer-size := 8192
        :with buffer := (make-array (list buffer-size) :element-type '(unsigned-byte 8))
        :for end := (read-sequence buffer input)
        :until (zerop end)
        :do (loop :for i :from 0 :below end
                  :for c := (aref buffer i)
                  :do (if (= 0 count)
                          #1=(multiple-value-setq (count state) ;; take first byte
                               (cond ((<= #x00 c #x7f) (commit c) (values 0 0))
                                     ((<= #x80 c #xbf) (ecommit c) (values 0 0))
                                     ((<= #xc0 c #xdf) (values 1 (logand #x1f c)))
                                     ((<= #xe0 c #xef) (values 2 (logand #x0f c)))
                                     ((<= #xf0 c #xf7) (values 3 (logand #x07 c)))
                                     ((<= #xf8 c #xff) (ecommit c) (values 0 0)))) ;; error exceed 21bit
                          (cond ((or (<= #x00 c #x7f)  ;;error input
                                     (<= #xc0 c #xff))
                                 #2=(loop :with result ;;revert partial read.
                                          :for i :from 1 :to read
                                          :do (push (+ #x80 (logand #x3f state)) result)
                                              (setf state (ash state -6))
                                          :finally (ecommit (+ (logand (ash #x3f (- count))
                                                                       state)
                                                               (logxor #xff (ash #x7f (- count)))))
                                                   (mapc #'ecommit result))
                                 (setf state 0 count 0 read 0)
                                 #1#)
                                ((<= #x80 c #xbf)
                                 (setf state (+ (ash state 6) (logand c #x7f))
                                       read (1+ read))
                                 (when (= read count)
                                   (if (or (case count ;; range check
                                             (1 (< state #x80))
                                             (2 (< state #x800))
                                             (3 (< state #x10000)))
                                           (< char-code-limit state)
                                           (e-range state)) ;; should be escaped.
                                       #2#
                                       (if not-first-byte
                                           (commit state)
                                           (progn
                                             (setf not-first-byte t)
                                             (if (or (= state #xfeff)
                                                     (= state #xfffe))
                                                 (setf (encoding-bom encoding) state)
                                                 (commit state)))))
                                   (setf state 0 count 0 read 0))))))
            (when (< end buffer-size)
              (return))
        :finally (commit nil))))) ;; signal eof

(defmethod encoding-write ((encoding utf-8) out)
  (flet ((utf8-write-char (c)
           (when c
             (let ((p (char-code c)))
               (cond ((<=    #x00 p   #x7f) (write-byte p out))
                     ((e-range p) (write-byte (e- p) out))
                     ((<=    #x80 p  #x7ff)
                      (write-byte (+ #xc0 (ash p -6)) out)
                      (write-byte (+ #x80 (logand p #x3f)) out))
                     ((<=   #x800 p #xffff)
                      (write-byte (+ #xe0 (ash p -12)) out)
                      (write-byte (+ #x80 (logand (ash p -6) #x3f)) out)
                      (write-byte (+ #x80 (logand p #x3f)) out))
                     ((<= #x10000 p #x1fffff)
                      (write-byte (+ #xf0 (ash p -18)) out)
                      (write-byte (+ #x80 (logand (ash p -12) #x3f)) out)
                      (write-byte (+ #x80 (logand (ash p -6) #x3f)) out)
                      (write-byte (+ #x80 (logand p #x3f)) out)))))))
    (let ((bom (encoding-bom encoding)))
      (when bom
        (case bom
          (#xfffe (utf8-write-char bom))
          (t (utf8-write-char #xfeff)))))
    #'utf8-write-char))
