;;; test-common.scm -- common definitions for the Scheme tests
;;; $Id$

(load "r4rs.scm")

(define (check exp result)
  (let ((actually (eval exp)))
    (gc)                   ; frequent collection often turns up errors
    (if (not (equal? actually result))
        (begin
          (write-string "test: ") (write exp) (newline)
          (write-string "expect: ") (write result) (newline)
          (write-string "got: ") (write actually) (newline)
          (error "failed!")))))

;; Return (f (f (f ... (f a) ... ))) with n invocations of f.
(define (church n f a) (if (eqv? n 0) a (church (- n 1) f (f a))))
(define (all l) (if (null? l) #t (if (car l) (all (cdr l)) #f)))
(define (range n) (if (eqv? n 0) '() (append (range (- n 1)) (list n))))
(define (reduce f l a) (if (null? l) a (f (car l) (reduce f (cdr l) a))))
(define (sum l) (reduce + l 0))
