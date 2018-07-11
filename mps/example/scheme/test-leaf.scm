;;; test-leaf.scm -- test leaf objects
;;; $Id$
;;; This test case creates many leaf objects (strings and integers).

(load "test-common.scm")

(define (triangle n) (if (eqv? n 0) 0 (+ n (triangle (- n 1)))))
(check '(triangle 10000) 50005000)
(check '(length (range 1000)) 1000)
(check '(let ((f (lambda (n) (make-string n #\x))))
          (string-length (apply string-append (map f (range 100)))))
       (triangle 100))
(check '(sum (map (lambda (n) (sum (range n))) (range 800))) 85653600)

(write-string "All tests pass.")
(newline)
