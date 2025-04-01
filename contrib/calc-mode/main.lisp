(uiop/package:define-package :lem-calc-mode/main (:use :cl :lem))
(in-package :lem-calc-mode/main)
;;;don't edit above

(define-major-mode calc-mode ()
    (:name "Calc"
     :keymap *calc-mode-keymap*)
  (buffer-end (current-point))
  (insert-character (current-point) #\newline)
  (insert-string (current-point) "$ "))

(define-key *calc-mode-keymap* "Return" 'calc-eval-line)

(defun calc-buffer ()
  (let ((name "*calc*"))
    (unless (get-buffer name)
      (make-buffer name))
    (get-buffer name)))

(defun calc-string (string)
  (unless (find-package :xyzzy-calc/calc)
    #-ros.init(lem:maybe-load-systems "xyzzy-calc"
                                      :error-on-failure-p t)
    #+ros.init(lem:maybe-load-systems "snmsts//xyzzy-calc"
                                      :error-on-failure-p t)
    (assert (find-package :xyzzy-calc/calc)))
  (with-output-to-string (o)
    (format o "~%")
    (uiop:symbol-call :xyzzy-calc/calc :calc-print
                      (uiop:symbol-call :xyzzy-calc/calc :calc-eval
                                        (uiop:symbol-call :xyzzy-calc/calc :calc-read-from-string string))
                      o)))

(define-command calc () ()
  (switch-to-buffer (calc-buffer))
  (calc-mode))

(define-command calc-eval-line () ()
  (with-point ((start (line-start (current-point)))
               (end (line-end (current-point))))
    (let* ((str (points-to-string start end))
           (pos (position #\$ str))
           (str (subseq str (1+ pos))))
      (insert-string (current-point)
                     (calc-string str))))
  (insert-character (current-point) #\newline)
  (insert-string (current-point) "$ "))
