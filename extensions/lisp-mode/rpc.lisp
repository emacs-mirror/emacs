(uiop:define-package :lem-lisp-mode/rpc
  (:use :cl)
  (:import-from :lem-lisp-mode/errors
                :disconnected)
  (:export :write-message-to-stream
           :read-message-from-stream))
(in-package :lem-lisp-mode/rpc)

;;; Encoding and decoding messages

(defun encode-integer (integer)
  "Encode an integer to a 0-padded 16-bit hexadecimal string."
  (babel:string-to-octets (format nil "~6,'0,X" integer)))

(defun decode-integer (string)
  "Decode a string representing a 0-padded 16-bit hex string to an integer."
  (parse-integer string :radix 16))

;; Writing and reading messages to/from streams

(defun write-message-to-stream (stream message)
  "Write a string to a stream, prefixing it with length information for Swank."
  (let* ((octets (babel:string-to-octets message))
         (length-octets (encode-integer (length octets)))
         (msg (make-array (+ (length length-octets)
                             (length octets))
                          :element-type '(unsigned-byte 8))))
    (replace msg length-octets)
    (replace msg octets :start1 (length length-octets))
    (write-sequence msg stream)))

(defun read-message-from-stream (stream)
  "Read a string from a string.

Parses length information to determine how many characters to read."
  (let ((length-buffer (make-array 6 :element-type '(unsigned-byte 8))))
    (when (/= 6 (read-sequence length-buffer stream))
      (error 'disconnected))
    (let* ((length (decode-integer (babel:octets-to-string length-buffer)))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (babel:octets-to-string buffer))))
