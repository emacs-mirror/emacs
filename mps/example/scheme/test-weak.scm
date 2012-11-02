;;; test-weak.scm -- weak hashtable tests for the MPS toy Scheme interpreter

(load "test-common.scm")

;; First, check that all the hash tables behave as expected.

(define (ht-test ht-fun hash-fun cmp-fun key)
  (let* ((ht (ht-fun hash-fun cmp-fun))
         (f (lambda (n) (equal? (hashtable-ref ht (key n) #f) n)))
         (g (lambda (n) (hashtable-set! ht (key n) n)))
         (r (range 25)))
    (for-each g r)
    (all (map f r))))

(define (stringify n) (make-string n #\b))
(check '(ht-test make-hashtable string-hash string=? stringify) #t)
(check '(ht-test make-weak-key-hashtable string-hash string=? stringify) #t)
(check '(ht-test make-weak-value-hashtable string-hash string=? stringify) #t)
(check '(ht-test make-doubly-weak-hashtable string-hash string=? stringify) #t)
(define (symbolize n) (string->symbol (make-string n #\a)))
(check '(ht-test make-hashtable eq-hash eq? symbolize) #t)
(define (identity n) n)
(check '(ht-test make-hashtable eqv-hash eqv? identity) #t)


;; The MPS doesn't actually guarantee promptness of splatting. But we
;; have to test it somehow!

(write-string "All tests pass.")
(newline)
