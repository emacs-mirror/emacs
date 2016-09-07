;;; example.el --- Do nothing as an example

;; Copyright (c) 2016 Free Software Foundation, Inc.

;; Version: 1.0

;;; Code:
;;;###autoload
(defun example-hello-world ()
  (interactive)
  (message "hello world"))
