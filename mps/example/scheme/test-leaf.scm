;;; test-leaf.scm -- test leaf objects
;;; 
;;; This test case creates many leaf objects (strings and integers).

(define (check exp result)
  (write-string "test: ") (write exp) (newline)
  (write-string "expect: ") (write result) (newline)
  (define actually (eval exp))
  (write-string "got: ") (write actually) (newline)
  (if (not (equal? actually result))
    (error exp)))

(define (triangle n) (if (eqv? n 0) 0 (+ n (triangle (- n 1)))))
(check '(triangle 10000) 50005000)

(define (range n) (if (eqv? n 0) '() (append (range (- n 1)) (list n))))
(check '(length (range 1000)) 1000)

(define (map f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))
(check '(let ((f (lambda (n) (make-string n #\x))))
          (string-length (apply string-append (map f (range 100)))))
       (triangle 100))

(define (sum l) (if (null? l) 0 (+ (car l) (sum (cdr l)))))
(check '(sum (map (lambda (n) (sum (range n))) (range 400))) 10746800)

(write-string "All tests pass.")
(newline)
