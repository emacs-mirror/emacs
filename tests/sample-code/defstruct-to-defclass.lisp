;;; input

(defstruct foo
  slot-a
  slot-b
  slot-c)

(defstruct foo
  ;; comment1
  a
  ;; comment2
  b
  c ;comment3
  d)

(defstruct foo
  (a 12)
  (b)
  (c (let ((x 0))
       (f x)))
  (d 100 :type integer)
  (e nil :type (or nil
                   string))
  (f (progn
       (foo))
     :type symbol)
  (g nil :read-only t)
  (h nil :read-only nil)
  (i nil :read-only (complex expression))
  (j 1
     :type integer
     :read-only t)
  (k 2
     :read-only t
     :type integer))

(defstruct (foo (:conc-name xxx-))
  a
  (b 100 :read-only t)
  (c 200 :type integer)
  (d 300 :read-only t :type float))

;;; output

(defclass foo ()
  ((slot-a
    :initarg :slot-a
    :initform nil
    :accessor foo-slot-a)
   (slot-b
    :initarg :slot-b
    :initform nil
    :accessor foo-slot-b)
   (slot-c
    :initarg :slot-c
    :initform nil
    :accessor foo-slot-c)))

(defclass foo ()
  (;; comment1
   (a
    :initarg :a
    :initform nil
    :accessor foo-a)
   ;; comment2
   (b
    :initarg :b
    :initform nil
    :accessor foo-b)
   (c ;comment3
    :initarg :c
    :initform nil
    :accessor foo-c)
   (d
    :initarg :d
    :initform nil
    :accessor foo-d)))

(defclass foo ()
  ((a
    :initarg :a
    :initform 12
    :accessor foo-a)
   (b
    :initarg :b
    :initform nil
    :accessor foo-b)
   (c
    :initarg :c
    :initform (let ((x 0))
                (f x))
    :accessor foo-c)
   (d
    :initarg :d
    :initform 100
    :accessor foo-d
    :type integer)
   (e
    :initarg :e
    :initform nil
    :accessor foo-e
    :type (or nil
              string))
   (f
    :initarg :f
    :initform (progn
                (foo))
    :accessor foo-f
    :type symbol)
   (g
    :initarg :g
    :initform nil
    :reader foo-g)
   (h
    :initarg :h
    :initform nil
    :accessor foo-h)
   (i
    :initarg :i
    :initform nil
    :reader foo-i)
   (j
    :initarg :j
    :initform 1
    :reader foo-j
    :type integer)
   (k
    :initarg :k
    :initform 2
    :reader foo-k
    :type integer)))

(defclass foo ()
  ((a
    :initarg :a
    :initform nil
    :accessor xxx-a)
   (b
    :initarg :b
    :initform 100
    :reader xxx-b)
   (c
    :initarg :c
    :initform 200
    :accessor xxx-c
    :type integer)
   (d
    :initarg :d
    :initform 300
    :reader xxx-d
    :type float)))
