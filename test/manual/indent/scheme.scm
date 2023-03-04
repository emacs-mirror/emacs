;; Testing sexp-comments

(define a #;(hello) there)

(define a #;1 there)

(define a #;"asdf" there)

(define a ;; #;(hello
  there)

(define a #;(hello
  there) 2)

(define a #;(hello
     #;(world))
        and)
  there) 2)

(define a #;(hello
     #;"asdf" (world
        and)
  there) 2)
