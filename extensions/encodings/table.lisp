(uiop:define-package :lem-encodings/table
  (:use :cl)
  (:export :read-table))
(in-package :lem-encodings/table)

(defun decode-code (code)
  (assert (equal (subseq code 0 2) "0x"))
    (parse-integer (subseq code 2) :radix 16))

(defun decode-unicode (unicode)
  (assert (equal (subseq unicode 0 2) "U+"))
  (parse-integer (subseq unicode 2) :radix 16))

(defun decode-line (line)
  "line are space separated. each line are consisted of
code unicode character unicode->code(optional if the iconv result is different from code)"
  (destructuring-bind (code unicode character &optional unicode->code)
      (uiop:split-string line :separator '(#\ ))
    (declare (ignore character))
    (list (decode-code code)
          (decode-unicode unicode)
          (when unicode->code
            (decode-code unicode->code)))))

(defun read-table (file)
  "read utf-8 encoded file"
  (loop for line in (uiop:read-file-lines file)
        collect (decode-line line)))
