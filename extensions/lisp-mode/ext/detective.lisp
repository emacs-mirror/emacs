(defpackage :lem-lisp-mode/detective
  (:use :cl :lem)
  (:export :capture-reference))

(in-package :lem-lisp-mode/detective)

(defun %default-capture (class position)
  (let* ((line (str:split #\Space (line-string position)))
         (name (remove #\) (second line))))
    (make-instance class
                   :reference-name name
                   :reference-point position)))

(defmethod capture-reference ((position lem:point) (class (eql :function-reference)))
  (let* ((line (str:split #\Space (line-string position)))
         (pname (second line))
         (name (or (and (str:starts-with-p "(setf" pname)
                        (str:concat pname " " (third line)))
                   pname)))
    (make-instance 'lem/detective:function-reference
                   :reference-name name
                   :reference-point position)))

(defmethod capture-reference ((position lem:point) (class (eql :class-reference)))
  (%default-capture 'lem/detective:class-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :variable-reference)))
  (%default-capture 'lem/detective:variable-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :package-reference)))
  (%default-capture 'lem/detective:package-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :macro-reference)))
  (%default-capture 'lem/detective:macro-reference position))

(defmethod capture-reference ((position lem:point) (class (eql :misc-reference)))
  (ppcre:register-groups-bind (type name)
      ("^\\s*\\(\\s*(\\w+)\\s+(.*)\\s*" (line-string position))
    (make-instance 'lem/detective:misc-reference
                   :misc-custom-type type
                   :reference-name (string-right-trim '(#\space #\tab) name)
                   :reference-point position)))
