(defpackage :lem/tests/interp
  (:use :cl :rove))
(in-package :lem/tests/interp)

(defvar *result*)

(lem:define-command $test-command-flag () ()
  (let ((value (lem:continue-flag :test-command-flag)))
    (alexandria:nconcf *result* (list value))))

(defun execute-testing-command (n command-name &optional argument)
  (lem:do-command-loop ()
    (lem:call-command command-name argument)
    (unless (plusp (decf n)) (return))))

(deftest Execute-the-same-command-consecutively
  (lem-fake-interface:with-fake-interface ()
    (lem:with-current-buffers ()
      (let ((*result* '()))
        (execute-testing-command 3 '$test-command-flag)
        (ok (equal '(nil t t) *result*))))))
