(in-package :lem/buffer/encodings)

(defclass encoding ()
  ((end-of-line
    :initarg :end-of-line
    :accessor encoding-end-of-line)))

(defclass internal-encoding (encoding)
  ((external-format
    :initarg :external-format
    :reader encoding-external-format)))

(defvar *encoding-collections* (make-hash-table :test 'equal))

(defun encodings ()
  (let (encodings)
    (maphash (lambda (x y)
               (declare (ignore y))
               (push (string-downcase x) encodings))
             *encoding-collections*)
    encodings))

(defun register-encoding (symbol)
  (assert (symbolp symbol))
  (let ((o (make-instance symbol)))
    (assert (typep o 'encoding))
    (setf (gethash (string symbol) *encoding-collections*) symbol))
  symbol)

(defun unregister-encoding (symbol)
  (remhash (string symbol) *encoding-collections*))

(defun encoding (symbol end-of-line)
  (let ((symbol (gethash (string symbol) *encoding-collections* symbol)))
    (assert (symbolp symbol))
    (if (keywordp symbol)
        (make-instance 'internal-encoding :end-of-line end-of-line :external-format symbol)
        (make-instance symbol :end-of-line end-of-line))))

(defun e+ (c) (+ #xe000 c))

(defun encoding-read-detect-eol (f)
  (lambda (c cr encoding)
    (case (encoding-end-of-line encoding)
      (:cr (funcall f (case c
                        (#.(char-code #\cr) #.(char-code #\lf))
                        (#.(char-code #\lf) (e+ (char-code #\lf)))
                        (t c))))
      (:crlf (case c
               (#.(char-code #\cr)
                  (if cr
                      (funcall f (e+ (char-code #\cr)))
                      (setf cr t)))
               (#.(char-code #\lf)
                  (if cr
                      (progn (funcall f (char-code #\lf))
                             (setf cr nil))
                      (funcall f (e+ (char-code #\lf)))))
               (t
                (when cr
                  (funcall f (e+ (char-code #\cr)))
                  (setf cr nil))
                (funcall f c))))
      (:auto (if cr
                 (case c
                   (#.(char-code #\lf)
                      (setf (encoding-end-of-line encoding) :crlf
                            cr nil)
                      (funcall f c))
                   (t (setf (encoding-end-of-line encoding) :cr
                            cr nil)
                      (funcall f #.(char-code #\lf))
                      (funcall f
                               (if (= c #.(char-code #\cr))
                                   #\lf
                                   c))))
                 (case c
                   (#.(char-code #\lf)
                      (setf (encoding-end-of-line encoding) :lf)
                      (funcall f c))
                   (#.(char-code #\cr)
                      (setf cr t))
                   (t (funcall f c)))))
      ((:lf t) (funcall f c)))
    cr))

(defgeneric encoding-read (encoding input output-char)
  (:documentation "read binary stream until eof calling output-char which takes (char cr-state encoding) and return new cr-state"))
(defgeneric encoding-write (encoding out)
  (:documentation "return function which takes character and write to `out` binary stream.EOF is informed via passing NIL to the function."))
(defgeneric encoding-check (encoding)
  (:documentation "return function which takes (string eofp) to check buffer valid to write into a file."))
(defmethod encoding-check (encoding) nil)
