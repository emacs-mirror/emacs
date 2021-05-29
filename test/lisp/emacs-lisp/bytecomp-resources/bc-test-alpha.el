;;; -*- lexical-binding: t -*-

(require 'bc-test-beta)

(defun bc-test-alpha-f (x)
  (let ((y nil))
    (list y (bc-test-beta-f x))))

(provide 'bc-test-alpha)
