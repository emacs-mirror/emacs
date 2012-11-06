;;; r4rs.scm -- essential procedures from R4RS

;; (caar pair)
;; (cadr pair)
;; ...
;; (cdddar pair)
;; (cddddr pair)
;; These procedures are compositions of car and cdr. Arbitrary
;; compositions, up to four deep, are provided. There are twenty-eight
;; of these procedures in all.
;; See R4RS 6.3.

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))


;; (memq obj list)
;; (memv obj list)
;; (member obj list)
;; These procedures return the first sublist of list whose car is obj,
;; where the sublists of list are the non-empty lists returned by
;; (list-tail list k) for k less than the length of list. If obj does
;; not occur in list, then #f (not the empty list) is returned. Memq
;; uses eq? to compare obj with the elements of list, while memv uses
;; eqv? and member uses equal?.
;; See R4RS 6.3.

(define (memq obj list)
  (cond ((null? list) #f)
        ((eq? obj (car list)) list)
        (else (memq obj (cdr list)))))

(define (memv obj list)
  (cond ((null? list) #f)
        ((eqv? obj (car list)) list)
        (else (memv obj (cdr list)))))

(define (member obj list)
  (cond ((null? list) #f)
        ((equal? obj (car list)) list)
        (else (member obj (cdr list)))))


;; (assq obj alist)
;; (assv obj alist)
;; (assoc obj alist)
;; Alist (for "association list") must be a list of pairs. These
;; procedures find the first pair in alist whose car field is obj, and
;; returns that pair. If no pair in alist has obj as its car, then #f
;; (not the empty list) is returned. Assq uses eq? to compare obj with
;; the car fields of the pairs in alist, while assv uses eqv? and
;; assoc uses equal?.
;; See R4RS 6.3.

(define (assq obj list)
  (cond ((null? list) #f)
        ((eq? obj (caar list)) (car list))
        (else (assq obj (cdr list)))))

(define (assv obj list)
  (cond ((null? list) #f)
        ((eqv? obj (caar list)) (car list))
        (else (assv obj (cdr list)))))

(define (assoc obj list)
  (cond ((null? list) #f)
        ((equal? obj (caar list)) (car list))
        (else (assoc obj (cdr list)))))


;; (<= x1 x2 x3 ...)
;; (>= x1 x2 x3 ...)
;; These procedures return #t if their arguments are (respectively):
;; monotonically nondecreasing, or monotonically nonincreasing.
;; These predicates are required to be transitive.
;; See R4RS 6.5.5.

(define (no-fold op list)
  (cond ((null? list) #t)
        ((null? (cdr list)) #t)
        ((op (car list) (cadr list)) #f)
        (else (no-fold op (cdr list)))))

(define (<= . rest) (no-fold > rest))
(define (>= . rest) (no-fold < rest))


;; (odd? n)
;; (even? n)
;; These numerical predicates test a number for a particular property,
;; returning #t or #f.
;; See R4RS 6.5.5.

(define (odd? n) (eqv? (remainder n 2) 1))
(define (even? n) (eqv? (remainder n 2) 0))


;; (max x1 x2 ...)
;; (min x1 x2 ...)
;; These procedures return the maximum or minimum of their arguments.
;; See R4RS 6.5.5.

(define (extremum op x list)
  (if (null? list) x
      (extremum op (if (op x (car list)) x (car list)) (cdr list))))

(define (max x1 . rest) (extremum > x1 rest))
(define (min x1 . rest) (extremum < x1 rest))


;; (abs x)
;; Abs returns the magnitude of its argument.
;; See R4RS 6.5.5.

(define (abs x) (if (< x 0) (- x) x))


;; (quotient n1 n2)
;; (remainder n1 n2)
;; These procedures implement number-theoretic (integer) division: For
;; positive integers n1 and n2, if n3 and n4 are integers such that
;; n1=n2n3+n4 and 0<= n4<n2, then
;;
;; (quotient n1 n2)                       ==>  n3
;; (remainder n1 n2)                      ==>  n4
;; 
;; For integers n1 and n2 with n2 not equal to 0,
;; 
;; (= n1 (+ (* n2 (quotient n1 n2))
;;               (remainder n1 n2)))
;;                                        ==>  #t
;; 
;; provided all numbers involved in that computation are exact.
;; See R4RS 6.5.5.

(define quotient /)
(define (remainder n1 n2) (- n1 (* n2 (quotient n1 n2))))


;; (number->string number)
;; (number->string number radix)
;; Radix must be an exact integer, either 2, 8, 10, or 16. If omitted,
;; radix defaults to 10. The procedure number->string takes a number
;; and a radix and returns as a string an external representation of
;; the given number in the given radix.
;; See R4RS 6.5.6.

(define (number->string . args)
  (letrec ((number (car args))
           (radix (if (null? (cdr args)) 10 (cadr args)))
           (digits "0123456789ABCDEF")
           (n->s (lambda (n list) 
                   (if (zero? n) list
                       (n->s (quotient n radix)
                             (cons (string-ref digits (remainder n radix))
                                   list))))))
    (cond ((or (< radix 2) (> radix 16))
           (error "radix must be in the range 2-16"))
          ((negative? number) 
           (string-append "-" (number->string (abs number) radix)))
          ((zero? number) "0")
          (else (list->string (n->s number '()))))))
