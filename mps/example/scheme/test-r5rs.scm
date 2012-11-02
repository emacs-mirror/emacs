;;; test-r5rs.scm -- tests from R5RS
;;;
;;; This file contains test code derived directly from R5RS. It
;;; ensures that all the functions correctly evaluate the examples in
;;; R5RS.
;;;
;;; Tests that rely on features that are unimplemented in the toy
;;; Scheme interpreter distributed with the MPS are marked "UNIMPL".
;;;
;;; DOCUMENT HISTORY
;;;
;;; 2004-07-27  RB  Added document history and Perforce Id keyword.
;;; 2011-06-11  RB  Updated for Ruse/SC.
;;; 2012-11-01  GDR Updated for toy Scheme in MPS kit.

;; UNIMPL: (define test-environment (env-push (the-environment)))

(define (check exp result)
  (write-string "test: ") (write exp) (newline)
  (write-string "expect: ") (write result) (newline)
  (define actually (eval exp ; UNIMPL: test-environment
                         ))
  (write-string "got: ") (write actually) (newline)
  (if (not (equal? actually result))
    (error exp)))

;;; let, let*, letrec

(check '(let ((x 2) (y 3)) (* x y)) '6)
(check '(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) '35)
(check '(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))) '70)
(check '(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (even? 88)) '#t)

;;; eqv?

(check '(eqv? 'a 'a) '#t)
(check '(eqv? 'a 'b) '#f)
(check '(eqv? 2 2) '#t)
(check '(eqv? '() '()) '#t)
(check '(eqv? 100000000 100000000) '#t)
(check '(eqv? (cons 1 2) (cons 1 2)) '#f)
(check '(eqv? (lambda () 1) (lambda () 2)) '#f)
(check '(eqv? #f 'nil) '#f)
(check '(let ((p (lambda (x) x))) (eqv? p p)) '#t)

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(check '(let ((g (gen-counter))) (eqv? g g)) '#t)
(check '(eqv? (gen-counter) (gen-counter)) '#f)
(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(check '(let ((g (gen-loser))) (eqv? g g)) '#t)

(check '(letrec ((f (lambda () (if (eqv? f g) 'f 'both))) (g (lambda () (if (eqv? f g) 'g 'both)))) (eqv? f g)) '#f)

(check '(let ((x '(a))) (eqv? x x)) '#t)


;;; eq?

(check '(eq? 'a 'a) '#t)
(check '(eq? (list 'a) (list 'a)) '#f)
(check '(eq? '() '()) '#t)
(check '(eq? car car) '#t)
(check '(let ((x '(a))) (eq? x x)) '#t)
(check '(let ((x '#())) (eq? x x)) '#t)
(check '(let ((p (lambda (x) x))) (eq? p p)) '#t)


;;; equal?

(check '(equal? 'a 'a) '#t)
(check '(equal? '(a) '(a)) '#t)
(check '(equal? '(a (b) c) '(a (b) c)) '#t)
(check '(equal? "abc" "abc") '#t)
(check '(equal? 2 2) '#t)
(check '(equal? (make-vector 5 'a) (make-vector 5 'a)) '#t)


;;; not?

(check '(not #t) '#f)
(check '(not 3) '#f)
(check '(not (list 3)) '#f)
(check '(not #f) '#t)
(check '(not '()) '#f)
(check '(not (list)) '#f)
(check '(not 'nil) '#f)


;;; boolean?

(check '(boolean? #f) '#t)
(check '(boolean? 0) '#f)
(check '(boolean? '()) '#f)


;;; Lists

(check ''(a b c . d) '(a . (b . (c . d))))
(define x (list 'a 'b 'c))
(define y x)
(check 'y '(a b c))
(check '(list? y) '#t)
(set-cdr! x 4)
(check 'x '(a . 4))
(check '(eqv? x y) '#t)
(check 'y '(a . 4))
(check '(list? y) '#f)
(set-cdr! x x)
;; UNIMPL: (check '(list? x) '#f)


;;; pair?

(check '(pair? '(a . b)) '#t)
(check '(pair? '(a b c)) '#t)
(check '(pair? '()) '#f)
(check '(pair? '#(a b)) '#f)

;;; cons

(check '(cons 'a '()) '(a))
(check '(cons '(a) '(b c d)) '((a) b c d))
(check '(cons "a" '(b c)) '("a" b c))
(check '(cons 'a 3) '(a . 3))
(check '(cons '(a b) 'c) '((a b) . c))

;;; car

(check '(car '(a b c)) 'a)
(check '(car '((a) b c d)) '(a))
(check '(car '(1 . 2)) '1)

;;; cdr

(check '(cdr '((a) b c d)) '(b c d))
(check '(cdr '(1 . 2)) '2)

;;; list?

(check '(list? '(a b c)) '#t)
(check '(list? '()) '#t)
(check '(list? '(a . b)) '#f)
;; UNIMPL: (check '(let ((x (list 'a))) (set-cdr! x x) (list? x)) '#f)

;;; list

(check '(list 'a (+ 3 4) 'c) '(a 7 c))
(check '(list) '())

;;; length

(check '(length '(a b c)) '3)
(check '(length '(a (b) (c d e))) '3)
(check '(length '()) '0)

;;; append

(check '(append '(x) '(y)) '(x y))
(check '(append '(a) '(b c d)) '(a b c d))
(check '(append '(a (b)) '((c))) '(a (b) (c)))
(check '(append '(a b) '(c . d)) '(a b c . d))
(check '(append '() 'a) 'a)

;;; reverse

(check '(reverse '(a b c)) '(c b a))
(check '(reverse '(a (b c) d (e (f)))) '((e (f)) d (b c) a))

;;; list-tail

(check '(list-tail '(a b c d) 2) '(c d))

;;; list-ref

(check '(list-ref '(a b c d) 2) 'c)
;; UNIMPL: (check '(list-ref '(a b c d) (inexact->exact (round 1.8))) 'c)

;;; memq, memv, member

;; UNIMPL: (check '(memq 'a '(a b c)) '(a b c))
;; UNIMPL: (check '(memq 'b '(a b c)) '(b c))
;; UNIMPL: (check '(memq 'a '(b c d)) #f)
;; UNIMPL: (check '(memq (list 'a) '(b (a) c)) #f)
;; UNIMPL: (check '(member (list 'a) '(b (a) c)) '((a) c))
;; UNIMPL: (check '(memv 101 '(100 101 102)) '(101 102))

;;; assq, assv, assoc

;; UNIMPL: (define e '((a 1) (b 2) (c 3)))
;; UNIMPL: (check '(assq 'a e) '(a 1))
;; UNIMPL: (check '(assq 'b e) '(b 2))
;; UNIMPL: (check '(assq 'd e) #f)
;; UNIMPL: (check '(assq (list 'a) '(((a)) ((b)) ((c)))) #f)
;; UNIMPL: (check '(assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)))
;; UNIMPL: (check '(assv 5 '((2 3) (5 7) (11 13))) '(5 7))

;;; symbol?

(check '(symbol? 'foo) '#t)
(check '(symbol? (car '(a b))) '#t)
(check '(symbol? "bar") '#f)
(check '(symbol? 'nil) '#t)
(check '(symbol? '()) '#f)
(check '(symbol? #f) '#f)

;;; symbol->string

(check '(symbol->string 'flying-fish) '"flying-fish")
(check '(symbol->string 'Martin) '"martin")
(check '(symbol->string (string->symbol "Malvina")) '"Malvina")

;;; string->symbol

(check '(eq? 'mISSISSIppi 'mississippi) '#t)
(check '(eq? 'bitBlt (string->symbol "bitBlt")) '#f)
(check '(eq? 'JollyWog (string->symbol (symbol->string 'JollyWog))) '#t)
(check '(string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D."))) '#t)

;;; vector

(check '(vector 'a 'b 'c) '#(a b c))

;;; vector-ref

(check '(vector-ref '#(1 1 2 3 5 8 13 21) 5) '8)

;(check '(vector-ref '#(1 1 2 3 5 8 13 21) (let ((i (round (* 2 (acos -1))))) (if (inexact? i) (inexact->exact i) i))) '13)

;;; vector-set!

(check '(let ((vec (vector 0 '(2 2 2 2) "Anna"))) (vector-set! vec 1 '("Sue" "Sue")) vec) '#(0 ("Sue" "Sue") "Anna"))

;;; vector->list, list->vector

(check '(vector->list '#(dah dah didah)) '(dah dah didah))
(check '(list->vector '(dididit dah)) '#(dididit dah))

;;; procedure?

(check '(procedure? car) '#t)
(check '(procedure? 'car) '#f)
(check '(procedure? (lambda (x) (* x x))) '#t)
(check '(procedure? '(lambda (x) (* x x))) '#f)
;; UNIMPL: (check '(call-with-current-continuation procedure?) '#t)

;;; apply

;; UNIMPL: (check '(apply + (list 3 4)) '7)
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
;; UNIMPL: (check '((compose sqrt *) 12 75) '30)

;;; map

;; UNIMPL: (check '(map cadr '((a b) (d e) (g h))) '(b e h))
;; UNIMPL: (check '(map (lambda (n) (expt n n)) '(1 2 3 4 5)) '(1 4 27 256 3125))
;; UNIMPL: (check '(map + '(1 2 3) '(4 5 6)) '(5 7 9))

;;; for-each

;; UNIMPL: (check '(let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4)) v) '#(0 1 4 9 16))

;;; call/cc

;; UNIMPL: (check '(call-with-current-continuation (lambda (exit) (for-each (lambda (x) (if (negative? x) (exit x))) '(54 0 37 -3 245 19)) #t)) '-3)

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (define (r obj)
          (cond ((null? obj) 0)
                ((pair? obj)
                  (+ (r (cdr obj)) 1))
                (else (return #f))))
        (r obj)))))

;; UNIMPL: (check '(list-length '(1 2 3 4)) '4)

;; UNIMPL: (check '(list-length '(a b . c)) '#f)

;;; values, call-with-values

;; UNIMPL: (check '(call-with-values (lambda () (values 4 5)) (lambda (a b) b)) '5)
;; UNIMPL: (check '(call-with-values * -) '-1)

;;; delay, force

(define (stream-from n) (delay (cons n (stream-from (+ n 1)))))
(define s0 (stream-from 0))
(define (head stream) (car (force stream)))
(define (tail stream) (cdr (force stream)))
(check '(head (tail (tail s0))) '2)

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
(check '(force p) '6)
;; UNIMPL: (check '(begin (set! x 10) (force p)) '6)

;;; quasiquote

(check '`(list ,(+ 1 2) 4) '(list 3 4))
;; UNIMPL: (check '(let ((name 'a)) `(list ,name ',name)) '(list a (quote a)))
;; UNIMPL: (check '`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) '(a 3 4 5 6 b))
;; UNIMPL: (check '`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))
;; UNIMPL: (check '`#(10 5 ,(abs -4) ,@(map abs '(16 -9)) 8) '#(10 5 4 16 9 8))
;; UNIMPL: (check '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
;; UNIMPL: (check '(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)) '(a `(b ,x ,'y d) e))

;;; do

;; UNIMPL: (check '(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i)) '#(0 1 2 3 4))
;; UNIMPL: (check '(let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum))) '25)

;;; named let

;; UNIMPL: (check '(let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '())) (cond ((null? numbers) (list nonneg neg)) ((>= (car numbers) 0) (loop (cdr numbers) (cons (car numbers) nonneg) neg)) ((< (car numbers) 0) (loop (cdr numbers) nonneg (cons (car numbers) neg))))) '((6 1 3) (-5 -2)))

(write-string "All tests pass.")
(newline)
