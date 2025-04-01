(uiop:define-package :lem-lisp-mode/reader
  (:use :cl)
  (:export :read-from-string*))
(in-package :lem-lisp-mode/reader)

(defun read-atom (in)
  (let ((token
          (coerce (loop :for c := (peek-char nil in nil)
                        :until (or (null c) (member c '(#\( #\) #\space #\newline #\tab)))
                        :collect c
                        :do (read-char in))
                  'string)))
    (handler-case (values (read-from-string token) nil)
      (error ()
        (ppcre:register-groups-bind (prefix name) ("(.*?)::?(.*)" token)
          (values (intern (string-upcase (string-left-trim ":" name))
                          :keyword)
                  (when prefix
                    (read-from-string prefix))))))))

(defun read-list (in)
  (read-char in)
  (loop :until (eql (peek-char t in) #\))
        :collect (read-ahead in)
        :finally (read-char in)))

(defun read-sharp (in)
  (read-char in)
  (case (peek-char nil in)
    ((#\()
     (let ((list (read-list in)))
       (make-array (length list) :initial-contents list)))
    ((#\\)
     (read-char in)
     (read-char in))
    (otherwise
     (unread-char #\# in))))

(defun read-ahead (in)
  (let ((c (peek-char t in)))
    (case c
      ((#\()
       (read-list in))
      ((#\")
       (read in))
      ((#\#)
       (read-sharp in))
      (otherwise
       (read-atom in)))))

(defun read-from-string* (string)
  (with-input-from-string (in string)
    (read-ahead in)))
