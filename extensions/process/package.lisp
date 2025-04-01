(defpackage :lem-process
  (:use :cl :lem)
  (:export :run-process
           :get-process-output-string
           :delete-process
           :process-alive-p
           :make-process-stream
           :process-send-input)
  #+sbcl
  (:lock t))
