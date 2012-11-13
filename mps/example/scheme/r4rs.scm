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


;; (= x1 x2 x3 ...)
;; (<= x1 x2 x3 ...)
;; (>= x1 x2 x3 ...)
;; These procedures return #t if their arguments are (respectively):
;; equal, monotonically nondecreasing, or monotonically nonincreasing.
;; These predicates are required to be transitive.
;; See R4RS 6.5.5.

(define (no-fold op list)
  (cond ((null? list) #t)
        ((null? (cdr list)) #t)
        ((op (car list) (cadr list)) #f)
        (else (no-fold op (cdr list)))))

(define (= . rest) (and (apply <= rest) (apply >= rest)))
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


;; (string->number string)
;; (string->number string radix)
;; Returns a number of the maximally precise representation expressed
;; by the given string. Radix must be an exact integer, either 2, 8,
;; 10, or 16. If radix is not supplied, then the default radix is 10.
;; If string is not a syntactically valid notation for a number, then
;; string->number returns #f.

(define (string->number . args)
  (letrec ((string (car args))
           (length (string-length string))
           (radix (if (null? (cdr args)) 10 (cadr args)))
           (c->d (lambda (c)
                   (let ((i (char->integer c)))
                     (cond ((char-numeric? c) (- i (char->integer #\0)))
                           ((char-upper-case? c) (- i -10 (char->integer #\A)))
                           ((char-lower-case? c) (- i -10 (char->integer #\a)))
                           (else #f)))))
           (s->n (lambda (i a)
                   (if (>= i length) a
                       (let ((d (c->d (string-ref string i))))
                         (cond ((eq? d #f) #f)
                               ((>= d radix) #f)
                               (else (s->n (+ i 1) (+ (* a radix) d)))))))))
    (s->n 0 0)))


;; (char=? char1 char2)
;; (char<? char1 char2)
;; (char>? char1 char2)
;; (char<=? char1 char2)
;; (char>=? char1 char2)
;; These procedures impose a total ordering on the set of characters.
;; See R4RS 6.6.

(define (char=? c1 c2) (eqv? (char->integer c1) (char->integer c2)))
(define (char<? c1 c2) (< (char->integer c1) (char->integer c2)))
(define (char>? c1 c2) (> (char->integer c1) (char->integer c2)))
(define (char<=? c1 c2) (<= (char->integer c1) (char->integer c2)))
(define (char>=? c1 c2) (>= (char->integer c1) (char->integer c2)))


;; (char-ci=? char1 char2)
;; (char-ci<? char1 char2)
;; (char-ci>? char1 char2)
;; (char-ci<=? char1 char2)
;; (char-ci>=? char1 char2)
;; These procedures are similar to char=? et cetera, but they treat
;; upper case and lower case letters as the same. For example,
;; `(char-ci=? #\A #\a)' returns #t.
;; See R4RS 6.6.

(define (char-ci=? c1 c2) (char=? (char-upcase c1) (char-upcase c2)))
(define (char-ci<? c1 c2) (char<? (char-upcase c1) (char-upcase c2)))
(define (char-ci>? c1 c2) (char>? (char-upcase c1) (char-upcase c2)))
(define (char-ci<=? c1 c2) (char<=? (char-upcase c1) (char-upcase c2)))
(define (char-ci>=? c1 c2) (char>=? (char-upcase c1) (char-upcase c2)))


;; (char-alphabetic? char)
;; (char-numeric? char)
;; (char-whitespace? char)
;; (char-upper-case? letter)
;; (char-lower-case? letter)
;; These procedures return #t if their arguments are alphabetic,
;; numeric, whitespace, upper case, or lower case characters,
;; respectively, otherwise they return #f. The following remarks,
;; which are specific to the ASCII character set, are intended only as
;; a guide: The alphabetic characters are the 52 upper and lower case
;; letters. The numeric characters are the ten decimal digits. The
;; whitespace characters are space, tab, line feed, form feed, and
;; carriage return.

(define (char-alphabetic? c) (or (char-upper-case? c) (char-lower-case? c)))
(define (char-numeric? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (char-whitespace? c) (memv (char->integer c) '(8 10 12 13 32)))
(define (char-upper-case? c) (and (char>=? c #\A) (char<=? c #\Z)))
(define (char-lower-case? c) (and (char>=? c #\a) (char<=? c #\z)))


;; (char-upcase char)
;; (char-downcase char)
;; These procedures return a character char2 such that `(char-ci=?
;; char char2)'. In addition, if char is alphabetic, then the result
;; of char-upcase is upper case and the result of char-downcase is
;; lower case.

(define (char-upcase c)
  (if (char-lower-case? c)
      (integer->char (- (+ (char->integer c) (char->integer #\A))
                        (char->integer #\a)))
      c))

(define (char-downcase c)
  (if (char-upper-case? c)
      (integer->char (- (+ (char->integer c) (char->integer #\a))
                        (char->integer #\A)))
      c))


;; (string-ci=? string1 string2)
;; Returns #t if the two strings are the same length and contain the
;; same characters in the same positions, otherwise returns #f.
;; String-ci=? treats upper and lower case letters as though they were
;; the same character.
;; See R4RS 6.7.

(define (string-cmp op1 op2 s1 s2 e1 e2)
  (letrec ((l1 (string-length s1))
           (l2 (string-length s2))
           (sc (lambda (i)
                 (cond ((and (>= i l1) (>= i l2)) #t)
                       ((>= i l1) e1)
                       ((>= i l2) e2)
                       ((op1 (string-ref s1 i) (string-ref s2 i)) #t)
                       ((not (op2 (string-ref s1 i) (string-ref s2 i))) #f)
                       (else (sc (+ 1 i)))))))
    (sc 0)))

(define (string-ci=? s1 s2) (string-cmp (lambda _ #f) char-ci=? s1 s2 #f #f))


;; (string<? string1 string2)
;; (string>? string1 string2)
;; (string<=? string1 string2)
;; (string>=? string1 string2)
;; (string-ci<? string1 string2)
;; (string-ci>? string1 string2)
;; (string-ci<=? string1 string2)
;; (string-ci>=? string1 string2)
;; These procedures are the lexicographic extensions to strings of the
;; corresponding orderings on characters. For example, string<? is the
;; lexicographic ordering on strings induced by the ordering char<? on
;; characters. If two strings differ in length but are the same up to
;; the length of the shorter string, the shorter string is considered
;; to be lexicographically less than the longer string.
;; See R4RS 6.7.

(define (string<? s1 s2) (not (string>=? s1 s2)))
(define (string>? s1 s2) (not (string<=? s1 s2)))
(define (string<=? s1 s2) (string-cmp char<? char<=? s1 s2 #t #f))
(define (string>=? s1 s2) (string-cmp char>? char>=? s1 s2 #f #t))
(define (string-ci<? s1 s2) (not (string-ci>=? s1 s2)))
(define (string-ci>? s1 s2) (not (string-ci<=? s1 s2)))
(define (string-ci<=? s1 s2) (string-cmp char-ci<? char-ci<=? s1 s2 #t #f))
(define (string-ci>=? s1 s2) (string-cmp char-ci>? char-ci>=? s1 s2 #f #t))


;; (map proc list1 list2 ...)
;; The lists must be lists, and proc must be a procedure taking as
;; many arguments as there are lists. If more than one list is given,
;; then they must all be the same length. Map applies proc
;; element-wise to the elements of the lists and returns a list of the
;; results, in order from left to right. The dynamic order in which
;; proc is applied to the elements of the lists is unspecified.
;; See R4RS 6.9.

(define (map proc . args)
  (letrec ((map1 (lambda (f l) (if (null? l) '() 
                                   (cons (f (car l)) (map1 f (cdr l))))))
           (map2 (lambda (l) (if (null? (car l)) '()
                                 (cons (apply proc (map1 car l))
                                       (map2 (map1 cdr l)))))))
    (map2 args)))


;; (for-each proc list1 list2 ...)
;; The arguments to for-each are like the arguments to map, but
;; for-each calls proc for its side effects rather than for its
;; values. Unlike map, for-each is guaranteed to call proc on the
;; elements of the lists in order from the first element to the last,
;; and the value returned by for-each is unspecified.
;; See R4RS 6.9.

(define (for-each proc . args)
  (letrec ((map1 (lambda (f l) (if (null? l) '() 
                                   (cons (f (car l)) (map1 f (cdr l))))))
           (map2 (lambda (l) (if (null? (car l)) #f
                                 (begin (apply proc (map1 car l))
                                        (map2 (map1 cdr l)))))))
    (map2 args)))
