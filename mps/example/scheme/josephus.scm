;;; josephus.scm -- A small benchmark for Scheme
;;; $Id$
;;; Adapted from <http://blog.dhananjaynene.com/2008/07/performance-comparison-c-java-python-ruby-jython-jruby-groovy/>

(define (make-person count)
    (define person (make-vector 3))
    (vector-set! person 0 count)
    person)

(define (person-shout person shout deadif)
    (if (< shout deadif)
        (+ shout 1)
        (begin
            (vector-set! (vector-ref person 2) 1 (vector-ref person 1))
            (vector-set! (vector-ref person 1) 2 (vector-ref person 2))
            1)))

(define (make-chain size)
    (define chain (make-vector 1 #f))
    (define last #f)
    (define (loop i)
        (if (< i size)
            (begin
                (define current (make-person i))
                (if (not (vector-ref chain 0)) (vector-set! chain 0 current))
                (if last
                    (begin
                        (vector-set! last 1 current)
                        (vector-set! current 2 last)))
                (set! last current)
                (loop (+ i 1)))))
    (loop 0)
    (vector-set! (vector-ref chain 0) 2 last)
    (vector-set! last 1 (vector-ref chain 0))
    chain)

(define (chain-kill chain nth)
    (define current (vector-ref chain 0))
    (define shout 1)
    (define (loop)
        (if (not (eq? (vector-ref current 1) current))
            (begin
                (set! shout (person-shout current shout nth))
                (set! current (vector-ref current 1))
                (loop))))
    (loop)
    (vector-set! chain 0 current)
    current)

(define (inner-loop i)
    (if (< i 100)
        (begin
            (define chain (make-chain 40))
            (chain-kill chain 3)
            (inner-loop (+ i 1)))))

(define (loop i)
    (if (< i 100)
        (begin
            (inner-loop 0)
            (write i)
            (loop (+ i 1)))))
(loop 0)
