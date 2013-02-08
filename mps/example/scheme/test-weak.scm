;;; test-weak.scm -- weak hashtable tests for the MPS toy Scheme interpreter

(load "test-common.scm")

(define (populate ht kvs)
  (let ((f (lambda (kv) (hashtable-set! ht (car kv) (cdr kv)))))
    (for-each f kvs)))

;; The MPS doesn't actually guarantee promptness of splatting. But we
;; have to test it somehow!

(define (ht-test ht-fun hash cmp f1 f2 kvs)
  (let* ((ht (ht-fun hash cmp))
         (f (lambda (kv) (equal? (hashtable-ref ht (car kv) #f) (cdr kv)))))
    (populate ht kvs)
    (list (begin (gc) (all (map f kvs)))
          (begin (for-each f1 kvs) (gc) (hashtable-size ht))
          (begin (for-each f2 kvs) (gc) (hashtable-size ht)))))

(define (dk kv) (set-car! kv #f))
(define (dv kv) (set-cdr! kv #f))

(check '(ht-test make-hashtable string-hash string=? dk dv
                 '(("one" . 1) ("two" . 2) ("three" . 3)))
       '(#t 3 3))

(check '(ht-test make-weak-key-hashtable eq-hash eq? dk dv
                 '((ONE . 1) (TWO . 2) (THREE . 3)))
       '(#t 0 0))

(check '(ht-test make-weak-key-hashtable eqv-hash eqv? dv dk
                 '((1 . 1) (2 . 2) (3 . 3)))
       '(#t 3 0))

(check '(ht-test make-weak-value-hashtable string-hash string=? dk dv
                 '(("one" . 1) ("two" . 2) ("three" . 3)))
       '(#t 3 0))

(check '(ht-test make-weak-value-hashtable string-hash string=? dv dk
                 '(("one" . 1) ("two" . 2) ("three" . 3)))
       '(#t 0 0))

(check '(ht-test make-doubly-weak-hashtable eq-hash eq? dk dv
                 '(("one" . 1) ("two" . 2) ("three" . 3)))
       '(#t 0 0))

(check '(ht-test make-doubly-weak-hashtable eqv-hash eqv? dv dk
                 '((#\a . 1) (#\b . 2) (#\c . 3)))
       '(#t 0 0))

(write-string "All tests pass.")
(newline)
