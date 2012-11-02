;;; test-mps.scm -- tests for the MPS toy Scheme interpreter

(load "test-common.scm")

;; Test recursion.
(check '(church 1000 (lambda (a) (+ 1 a)) 0) 1000)

;; Create strings of various lengths.
(check '(church 50 (lambda (s) (string-append s "x")) "") (make-string 50 #\x))

;; Test map, range.
(check '(map (lambda (x) (+ 1 x)) '(1 2 3)) '(2 3 4))
(check '(range 5) '(1 2 3 4 5))
(check '(map (lambda (x) (+ 1 x)) (range 10)) (cdr (range 11)))

;; Hashtables
(define (ht-test ht key)
  (let* ((f (lambda (n) (equal? (hashtable-ref ht (key n) #f) n)))
         (g (lambda (n) (hashtable-set! ht (key n) n)))
         (r (range 100)))
    (for-each g r)
    (all (map f r))))

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
