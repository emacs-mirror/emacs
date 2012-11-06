;;; test-common.scm -- common definitions for the Scheme tests

(define (check exp result)
  (let ((actually (eval exp)))
    (if (not (equal? actually result))
        (begin
          (write-string "test: ") (write exp) (newline)
          (write-string "expect: ") (write result) (newline)
          (write-string "got: ") (write actually) (newline)
          (error "failed!")))))

;; Return (f (f (f ... (f a) ... ))) with n invocations of f.
(define (church n f a)
  (if (eqv? n 0)
      a
      (church (- n 1) f (f a))))

(define (map f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))
(define (all l) (if (null? l) #t (if (car l) (all (cdr l)) #f)))
(define (range n) (if (eqv? n 0) '() (append (range (- n 1)) (list n))))
(define (for-each f l) (if (null? l) #f (begin (f (car l)) (for-each f (cdr l)))))
(define (reduce f l a) (if (null? l) a (f (car l) (reduce f (cdr l) a))))
(define (sum l) (reduce + l 0))
