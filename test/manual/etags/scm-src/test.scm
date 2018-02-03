(define hello "Hello, Emacs!")

(set! hello "Hello, world!")

(define (hello-world)
  (display hello)
  (newline))

;; Bug 5624
(define (there-is-a-=-in-the-middle!) #t)

(define =starts-with-equals! #t)

(define (((((curry-test a) b) c) d) e)
  (list a b c d e))

(define-syntax test-begin
  (syntax-rules ()
    ((test-begin exp ...)
     ((lambda () exp ...)))))
