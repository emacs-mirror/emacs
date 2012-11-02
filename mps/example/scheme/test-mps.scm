;;; test-mps.scm -- tests for the MPS toy Scheme interpreter

(define (check exp result)
  (write-string "test: ") (write exp) (newline)
  (write-string "expect: ") (write result) (newline)
  (define actually (eval exp))
  (write-string "got: ") (write actually) (newline)
  (if (not (equal? actually result))
    (error exp)))

;; Return (f (f (f ... (f a) ... ))) with n invocations of f.
(define (church n f a)
  (if (eqv? n 0)
      a
      (church (- n 1) f (f a))))

;; Call (f n) ... (f 2) (f 1).
(define (repeat n f)
  (if (eqv? n 0)
      '()
      (begin (f n)
             (repeat (- n 1) f))))

;; Test recursion.
(check '(church 1000 (lambda (a) (+ 1 a)) 0) 1000)

;; Create strings of various lengths.
(check '(church 50 (lambda (s) (string-append s "x")) "") (make-string 50 #\x))

(define (map f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))
(define (all l) (if (null? l) #t (if (car l) (all (cdr l)) #f)))
(define (range n) (if (eqv? n 0) '() (append (range (- n 1)) (list n))))

(check '(map (lambda (x) (+ 1 x)) '(1 2 3)) '(2 3 4))
(check '(range 5) '(1 2 3 4 5))
(check '(map (lambda (x) (+ 1 x)) (range 10)) (cdr (range 11)))

;; Hashtables
(define (ht-test ht key)
  (let* ((f (lambda (n) (equal? (hashtable-ref ht (key n) #f) n)))
         (g (lambda (n) (hashtable-set! ht (key n) n)))
         (n 100))
    (repeat n g)
    (all (map f (range n)))))

(define (stringify n) (make-string n #\b))
(check '(ht-test (make-hashtable string-hash string=?) stringify) #t)
(define (symbolize n) (string->symbol (make-string n #\a)))
(check '(ht-test (make-eq-hashtable) symbolize) #t)
(check '(ht-test (make-hashtable eq-hash eq?) symbolize) #t)
(define (identity n) n)
(check '(ht-test (make-eqv-hashtable) identity) #t)
(check '(ht-test (make-hashtable eqv-hash eqv?) identity) #t)

(write-string "All tests pass.")
(newline)
